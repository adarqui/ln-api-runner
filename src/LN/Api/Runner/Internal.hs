--
-- This will test the LN api
-- Right now i'm keeping everything in one file, because i'm not sure how I want to structure things yet.
--

{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Api.Runner.Internal where



import           Control.Exception
import           Control.Monad              (void)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (runEitherT)
import qualified Control.Monad.Trans.Either as Either
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.RWS
import           Data.ByteString            (ByteString)
import qualified Data.Map                   as M
import           Data.Monoid                ((<>))
import           Data.String.Conversions
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Arbitrary
import           Haskell.Api.Helpers
import           LN.Api
import           LN.T
import           Test.QuickCheck
import           Test.QuickCheck.Utf8


type RunnerM = RWST RunnerReader RunnerWriter RunnerState IO



data RunnerReader = RunnerReader {
  rApiOpts :: ApiOptions
}

defaultRunnerReader :: RunnerReader
defaultRunnerReader = RunnerReader {
  rApiOpts = defaultApiOpts
}



defaultApiOpts :: ApiOptions
defaultApiOpts = ApiOptions {
  apiUrl         = "http://dev.adarq.org",
  apiPrefix      = "api",
  apiKey         = Nothing,
  apiKeyHeader   = Just "z-authorization",
  apiWreqOptions = defaultWreqOptions,
  apiDebug       = True
}



type RunnerWriter = ()




data RunnerState = RunnerState {
  orgs  :: M.Map Text OrganizationPackResponse,
  users :: M.Map Text UserSanitizedPackResponse,
  keys  :: M.Map Text ApiResponse
} deriving (Show)



defaultRunnerState = RunnerState {
  orgs = M.empty,
  users = M.empty,
  keys = M.empty
}



-- cs = convertString



oneOf = oneof



genUppercaseChar :: Gen Char
genUppercaseChar = elements ['A'..'Z']

genUppercaseString :: Gen String
genUppercaseString = listOf genUppercaseChar



genLowercaseChar :: Gen Char
genLowercaseChar = elements ['a'..'z']

genLowercaseString :: Gen String
genLowercaseString = listOf genLowercaseChar



genAlphaChar :: Gen Char
genAlphaChar = oneOf [genUppercaseChar, genLowercaseChar]

genAlphaString :: Gen String
genAlphaString = listOf genAlphaChar



genDigitChar :: Gen Char
genDigitChar = elements ['0','1','2','3','4','5','6','7','8','9']

genDigitString :: Gen String
genDigitString = listOf genDigitChar



genAlphaNumChar :: Gen Char
genAlphaNumChar = oneOf [genAlphaChar, genDigitChar]

genAlphaNumString :: Gen String
genAlphaNumString = listOf genAlphaNumChar



genSpaceChar :: Gen Char
genSpaceChar = elements [' ']

genSpaceString :: Gen String
genSpaceString = listOf genSpaceChar


genPunctChar :: Gen Char
genPunctChar = elements "!@#$%^&*()_-+=~`{[}]|\\:;\"'<,>.?/"



genAsciiChar :: Gen Char
genAsciiChar = oneOf [genUppercaseChar, genLowercaseChar, genDigitChar, genSpaceChar, genPunctChar]

genAsciiString :: Gen String
genAsciiString = listOf genAsciiChar




genUsername :: Gen String
genUsername = listOf $ oneOf [genUppercaseChar, genLowercaseChar, genDigitChar]



genDisplayNick :: Gen String
genDisplayNick = listOf $ oneOf [genAlphaNumChar, genSpaceChar]




-- genEmail :: IO String
-- genEmail = do
--   user <- generate $ genUsername
--   pure (user <> "@adarq.org")



-- teste = forAll genUppercaseString $ \str -> str == str




rd
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either ApiError a)
  -> RWST RunnerReader w s m (Either ApiError a)
rd actions = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKey = Just "1" }



rd'
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either ApiError a)
  -> RWST RunnerReader w s m (Either SomeException (Either ApiError a))
rd' actions = do
  opts <- asks rApiOpts
  liftIO $ try (runWith actions $ opts { apiKey = Just "1" })



rw
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either ApiError a)
  -> ByteString
  -> RWST RunnerReader w s m (Either ApiError a)
rw actions s = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKey = Just s }



rw'
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either ApiError a)
  -> ByteString
  -> RWST RunnerReader w s m (Either SomeException (Either ApiError a))
rw' actions s = do
  opts <- asks rApiOpts
  liftIO $ try $ runWith actions $ opts { apiKey = Just s }



left :: forall a (f :: * -> *) b.  Applicative f => a -> f (Either a b)
left  = pure . Left



right :: forall a (f :: * -> *) a1.  Applicative f => a -> f (Either a1 a)
right = pure . Right



leftT :: forall e (m :: * -> *) a.  Monad m => e -> Either.EitherT e m a
leftT = Either.left



rightT :: forall a e (m :: * -> *).  Monad m => a -> Either.EitherT e m a
rightT = Either.right



isT :: forall b (m :: * -> *) e.  Monad m => m (Either e b) -> Either.EitherT e m b
isT go = do
  x <- lift go
  case x of
    Left err -> leftT err
    Right v  -> rightT v



isNotT :: forall b (m :: * -> *) e.  Monad m => m (Either e b) -> Either.EitherT b m e
isNotT go = do
  x <- lift go
  case x of
    Left err -> rightT err
    Right v  -> leftT v



launchRunner :: IO ()
launchRunner = do
  putStrLn "Launching"
  runnerRWST go
  putStrLn "Done"
  where
  go = do
    testInvalidCreateUsers
--    createUsers
--    createOrganizations



cleanupRunner :: IO ()
cleanupRunner = do
  putStrLn "Cleanup"
  runnerRWST go
  where
  go = do
    removeUsers
    removeOrganizations



-- runnerRWST :: forall w a. RWST RunnerReader w RunnerState IO a -> IO ()
runnerRWST :: forall a. RunnerM a -> IO ()
runnerRWST go = do
  void $ evalRWST go defaultRunnerReader defaultRunnerState
  pure ()



createUsers :: RunnerM ()
createUsers = do
  user1 <- buildUser
  user2 <- buildUser
  e_user1 <- rd (postUser' user1)
  e_user2 <- rd (postUser' user2)
  case (e_user1, e_user2) of
    (Right user1', Right user2') -> liftIO $ print "success"
    _                      -> liftIO $ print "failure"



buildUser :: RunnerM UserRequest
buildUser = do
  display_nick <- liftIO $ generate genDisplayNick
  let nick     =  filter (/= ' ') display_nick
  pure $ UserRequest {
    userRequestDisplayNick = cs display_nick,
    userRequestName        = cs display_nick,
    userRequestEmail       = cs $ nick <> "@adarq.org",
    userRequestPlugin      = "ln-api-runner",
    userRequestIdent       = cs nick,
    userRequestAcceptTOS   = Nothing
  }



removeUsers :: RunnerM ()
removeUsers = pure ()



-- | Tests invalid user creation
-- User creation via this api call can only happen in "GOD MODE" anyway..
-- User creation via the api is used by ln-api-runner & ln-smf-migrate
--
testInvalidCreateUsers :: RunnerM (Either () ())
testInvalidCreateUsers = do
  lr <- runEitherT $ do
    user <- lift $ buildUser
    isNotT $ rd' (postUser' $ user { userRequestDisplayNick = "" })
    isNotT $ rd' (postUser' $ user { userRequestName = "" })
    isNotT $ rd' (postUser' $ user { userRequestEmail = "" })
    isNotT $ rd' (postUser' $ user { userRequestIdent = "" })
    pure ()

  case lr of
    Left _ -> left ()
    Right _ -> right ()








createOrganizations :: RunnerM ()
createOrganizations = do
--  org_a <- createOrganization "1240177678" $ OrganizationRequest "orga" (Just "org a") "Org A" "TestLand" "runner.org.a@adarq.org" Membership_Join [] Nothing Public 0
--  org_b <- createOrganization "1240177678" $ OrganizationRequest "orgb" (Just "org b") "Org B" "TestLand" "runner.org.b@adarq.org" Membership_InviteOnly  [] Nothing Public 0
--  org_c <- createOrganization "1240177678" $ OrganizationRequest "orgc" (Just "org c") "Org C" "TestLand" "runner.org.c@adarq.org" Membership_RequestInvite [] Nothing Public 0
--  modify (\st->st{ orgs = M.fromList [("orga", org_a), ("orgb", org_b), ("orgc", org_c)] })
  pure ()



createOrganization :: OrganizationRequest -> RunnerM ()
createOrganization org_req = do
  e_result <- rd (postOrganization' org_req)
  case e_result of
    (Left err)           -> liftIO $ print err
    (Right org_response) -> do
      pure ()



removeOrganizations :: RunnerM ()
removeOrganizations = do
  pure ()