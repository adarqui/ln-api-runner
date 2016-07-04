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
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import qualified Control.Monad.Trans.Either as Either
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.RWS
import           Data.ByteString            (ByteString)
import           Data.Either                (Either (..), isLeft)
import qualified Data.Map                   as M
import           Data.Monoid                ((<>))
import           Data.String.Conversions
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO as TIO
import           Data.Text.Arbitrary
import           Haskell.Api.Helpers
import           LN.Api
import           LN.Generate
import           LN.Sanitize
import           LN.T
import           LN.T.Error                 (ApplicationError (..),
                                             ValidationError (..),
                                             ValidationErrorCode (..))
import           LN.Validate
import           Test.QuickCheck
import           Test.QuickCheck.Utf8
import Rainbow



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
  apiKeyHeader   = Just "x-api-authorization",
  apiWreqOptions = defaultWreqOptions,
  apiDebug       = True
}



type RunnerWriter = ()




data RunnerState = RunnerState {
  orgs  :: M.Map Text OrganizationPackResponse,
  users :: M.Map Text UserSanitizedPackResponse,
  keys  :: M.Map Text ApiResponse
}



defaultRunnerState = RunnerState {
  orgs = M.empty,
  users = M.empty,
  keys = M.empty
}



rd
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd actions = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKey = Just "poop" }



-- rd'
--   :: (Monoid w, MonadIO m)
--   => ReaderT ApiOptions IO (Either (ApiError b) a)
--   -> RWST RunnerReader w s m (Either SomeException (Either (ApiError b) a))
-- rd' actions = do
--   opts <- asks rApiOpts
--   liftIO $ try (runWith actions $ opts { apiKey = Just "1" })



rw
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either (ApiError b) a)
  -> ByteString
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rw actions s = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKey = Just s }



-- rw'
--   :: (Monoid w, MonadIO m)
--   => ReaderT ApiOptions IO (Either (ApiError b) a)
--   -> ByteString
--   -> RWST RunnerReader w s m (Either SomeException (Either (ApiError b) a))
-- rw' actions s = do
--   opts <- asks rApiOpts
--   liftIO $ try $ runWith actions $ opts { apiKey = Just s }



left :: forall a (f :: * -> *) b. Applicative f => a -> f (Either a b)
left  = pure . Left



right :: forall a (f :: * -> *) a1. Applicative f => a -> f (Either a1 a)
right = pure . Right



leftT :: forall e (m :: * -> *) a. Monad m => e -> Either.EitherT e m a
leftT = Either.left



rightT :: forall a e (m :: * -> *). Monad m => a -> Either.EitherT e m a
rightT = Either.right



mustPassT :: forall b (m :: * -> *) e. Monad m => m (Either e b) -> Either.EitherT e m b
mustPassT go = do
  x <- lift go
  case x of
    Left err -> leftT err
    Right v  -> rightT v



mustFailT :: forall b (m :: * -> *) e. Monad m => m (Either e b) -> Either.EitherT b m e
mustFailT go = do
  x <- lift go
  case x of
    Left err -> rightT err
    Right v  -> leftT v



assertFailT :: forall b (m :: * -> *) e. (Eq e, Monad m) => e -> m (Either e b) -> Either.EitherT b m e
assertFailT criteria go = do
  x <- lift go
  case x of
    Left err -> do
      if err /= criteria
        then leftT undefined
        else rightT err
    Right v  -> leftT v



-- | An assertion for Failure.
-- `go` must fail with Left _, in order for this test to Pass
--
assertFail_ValidateT
  :: (Monad m, MonadIO m)
  => Text
  -> ValidationError
  -> m (Either (ApiError ApplicationError) e)
  -> EitherT e m ValidationError
assertFail_ValidateT message criteria go = do
  x <- lift go
  case x of
    Left (ServerError _ (Error_Validation error_validation)) -> do
      if error_validation /= criteria
        then do
          liftIO $ printFail message
          liftIO $ printActualFailure (T.pack $ show error_validation)
          leftT undefined
        else (liftIO $ printPass message) *> rightT error_validation
    Left err -> do
      liftIO $ printFail message
      liftIO $ printActualFailure (T.pack $ show err)
      leftT undefined
    Right v  -> do
      liftIO $ printFail message
      leftT v



testPassFailT message act = do
  lr <- act
  if isLeft lr
    then liftIO $ printFail message
    else liftIO $ printPass message
  pure lr



printFail message = do
  putChunk $ chunk ("Fail: " :: Text) & fore red & bold
  TIO.putStrLn message



printActualFailure message = do
  putChunk $ chunk ("Actual: " :: Text) & fore red & bold
  putChunk $ chunk message & fore cyan



printPass message = do
  putChunk $ chunk ("Pass: " :: Text) & fore green & bold
  TIO.putStrLn message



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
  user1 <- liftIO buildValidUser
  user2 <- liftIO buildValidUser
  e_user1 <- rd (postUser' user1)
  e_user2 <- rd (postUser' user2)
  case (e_user1, e_user2) of
    (Right user1', Right user2') -> liftIO $ print "success"
    _                            -> liftIO $ print "failure"



removeUsers :: RunnerM ()
removeUsers = pure ()



-- | Tests invalid user creation
-- User creation via this api call can only happen in "GOD MODE" anyway..
-- User creation via the api is used by ln-api-runner & ln-smf-migrate
--
testInvalidCreateUsers :: RunnerM (Either () ())
testInvalidCreateUsers = do
  lr <- runEitherT $ do
    user <- liftIO buildValidUser

    void $ assertFail_ValidateT "Empty display_name = error" (Validate Validate_CannotBeEmpty $ Just "display_name") $
      rd (postUser' $ user { userRequestDisplayName = "" })

    void $ assertFail_ValidateT "Empty full_name = error" (Validate Validate_CannotBeEmpty $ Just "full_name") $
      rd (postUser' $ user { userRequestFullName = "" })

    void $ assertFail_ValidateT "Empty email = error" (Validate Validate_CannotBeEmpty $ Just "email") $
      rd (postUser' $ user { userRequestEmail = "" })

    void $ assertFail_ValidateT "Empty plugin = error" (Validate Validate_CannotBeEmpty $ Just "plugin") $
      rd (postUser' $ user { userRequestPlugin = "" })

    void $ assertFail_ValidateT "Empty ident = error" (Validate Validate_CannotBeEmpty $ Just "ident") $
      rd (postUser' $ user { userRequestIdent = "" })

    void $ assertFail_ValidateT "display_name > maxDisplayName = error" (Validate Validate_TooLong $ Just "display_name") $
      rd (postUser' $ user { userRequestDisplayName = T.replicate 33 "A" })

    pure ()

  case lr of
    Left _  -> left ()
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
    (Left err)           -> liftIO $ print "err"
    (Right org_response) -> do
      pure ()



removeOrganizations :: RunnerM ()
removeOrganizations = do
  pure ()
