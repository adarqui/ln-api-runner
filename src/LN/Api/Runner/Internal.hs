--
-- This will test the LN api
-- Right now i'm keeping everything in one file, because i'm not sure how I want to structure things yet.
--

{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module LN.Api.Runner.Internal where



import Data.Int (Int64)
import System.Exit (exitFailure)
import           Control.Break              (break, loop)
import           Control.Concurrent         (threadDelay)
import           Control.Exception
import           Control.Monad              (void)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import qualified Control.Monad.Trans.Either as Either
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader (asks)
import           Control.Monad.Trans.RWS    (RWST, asks, evalRWST, get, modify,
                                             put)
import           Control.Monad.Trans.State  (StateT, evalStateT, runStateT)
import qualified Control.Monad.Trans.State  as State (get, modify, put)
import           Data.ByteString            (ByteString)
import           Data.Either                (Either (..), isLeft, isRight)
import qualified Data.Map                   as M
import           Data.Monoid                ((<>))
import           Data.Rehtie
import           Data.String.Conversions
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Arbitrary
import qualified Data.Text.IO               as TIO
import           Haskell.Api.Helpers
import           LN.Api
import           LN.Generate
import           LN.Sanitize
import           LN.T
import           LN.T.Error                 (ApplicationError (..),
                                             ValidationError (..),
                                             ValidationErrorCode (..))
import           LN.Validate
import           Prelude                    hiding (break)
import           Rainbow
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



superKey :: ByteString
superKey = "pooppooppooppooppooppooppooppooppooppooppooppooppooppooppooppooppooppoop"



rd_Super
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_Super = rd_Api superKey



rd_AsApiKey
  :: (Monoid w, MonadIO m)
  => ByteString
  -> ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_AsApiKey = rd_Api



rd_Api
  :: (Monoid w, MonadIO m)
  => ByteString
  -> ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_Api api_key actions = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKey = Just api_key }



rd_AsUserId
  :: (Monoid w, MonadIO m)
  => Int64
  -> ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_AsUserId user_id actions = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKeyHeader = Just "x-as-user",  apiKey = Just (superKey <> (cs $ show user_id)) }



rd_Guest
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_Guest actions = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts



rw
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either (ApiError b) a)
  -> ByteString
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rw actions s = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKey = Just s }



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



assertT
  :: (Monad m, MonadIO m)
  => Text
  -> (Either e a -> Bool)
  -> m (Either e a)
  -> EitherT e m a
assertT message test go = do
  lr <- lift go
  if test lr
    then do
      (liftIO $ printPass message) *> rightT ()
    else do
      liftIO $ printFail message
  case lr of
    Left l  -> leftT l
    Right r -> rightT r



-- | Retry `retries` times until a success
--
assertRetryT
  :: (Monad m, MonadIO m)
  => Int
  -> Text
  -> (Either e a -> Bool)
  -> m (Either e a)
  -> EitherT e m a
assertRetryT retries message test go = do

  lr <- lift $ runEitherT $ do
    assertT message test go
  case lr of
    Left err -> do
      if retries == 0
        then liftIO (printActualFailure "Maximum retries attempted.") *> leftT err
        else assertRetryT (retries-1) message test go
    Right v  -> rightT v


  --
  -- lr <- flip evalStateT 0 $ loop $ do
  --   n <- lift (State.modify (+1) *> State.get)
  --   lr <- lift $ assertT message test go
  --   if (isLeft lr || n == retries)
  --     then break lr
  --     else break lr

  -- case lr of
  --   Left err -> leftT err
  --   Right v  -> rightT v



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
          liftIO $ printActualFailure (show error_validation)
          leftT undefined
        else (liftIO $ printPass message) *> rightT error_validation
    Left err -> do
      liftIO $ printFail message
      liftIO $ printActualFailure (show err)
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



printFail :: forall a. ConvertibleStrings a Text => a -> IO ()
printFail message = do
  putChunk $ chunk ("Fail: " :: Text) & fore red & bold
  TIO.putStrLn (cs message)



printActualFailure :: String -> IO ()
printActualFailure message = do
  putChunk $ chunk ("ActualFailure: " :: Text) & fore red & bold
  putChunkLn $ chunk message & fore cyan



printFatal :: String -> IO ()
printFatal message = do
  putChunk $ chunk ("Fatal: " :: Text) & fore red & bold
  putChunkLn $ chunk message & fore red & bold
  exitFailure



printPass :: forall a. ConvertibleStrings a Text => a -> IO ()
printPass message = do
  putChunk $ chunk ("Pass: " :: Text) & fore green & bold
  TIO.putStrLn (cs message)



printInfo :: String -> IO ()
printInfo message = do
  putChunk $ chunk ("Info: " :: Text) & fore white & bold
  putStrLn message



launchRunner :: IO ()
launchRunner = do
  printInfo "Launching API Runner"
  runnerRWST go
  printInfo "Done"
  where
  go = do
    testCreateUser >>= either (const $ liftIO (printFatal "testCreateUser must not fail.")) (const $ pure ())
    testCreateInvaidUsers
    testCreateInvalidOrganizations
    pure ()
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
  e_user1 <- rd_Super (postUser' user1)
  e_user2 <- rd_Super (postUser' user2)
  case (e_user1, e_user2) of
    (Right user1', Right user2') -> liftIO $ print "success"
    _                            -> liftIO $ print "failure"



removeUsers :: RunnerM ()
removeUsers = pure ()



testCreateUser :: RunnerM (Either () ())
testCreateUser = do
  lr <- runEitherT $ do
    user_request <- liftIO buildValidUser
    user@UserResponse{..} <- assertT "A valid user is created" isRight $
      rd_Super (postUser' user_request)
    void $ assertRetryT 5 "After a user is created, a profile is subsequently created" isRight $
      rd_Super (getUserProfiles_ByUserId' userResponseId)
    void $ assertRetryT 5 "After a user is created, an api entry is subsequently created" isRight $
      rd_AsUserId userResponseId getApis'
    pure ()

  either (const $ left ()) (const $ right ()) lr



-- | Tests invalid user creation
-- User creation via this api call can only happen in "GOD MODE" anyway..
-- User creation via the api is used by ln-api-runner & ln-smf-migrate
--
testCreateInvaidUsers :: RunnerM (Either () ())
testCreateInvaidUsers = do
  lr <- runEitherT $ do
    user <- liftIO buildValidUser

    void $ assertFail_ValidateT "Empty display_name = error" (Validate Validate_CannotBeEmpty $ Just "display_name") $
      rd_Super (postUser' $ user { userRequestDisplayName = "" })

    void $ assertFail_ValidateT "Empty full_name = error" (Validate Validate_CannotBeEmpty $ Just "full_name") $
      rd_Super (postUser' $ user { userRequestFullName = "" })

    void $ assertFail_ValidateT "Empty email = error" (Validate Validate_CannotBeEmpty $ Just "email") $
      rd_Super (postUser' $ user { userRequestEmail = "" })

    void $ assertFail_ValidateT "Empty plugin = error" (Validate Validate_CannotBeEmpty $ Just "plugin") $
      rd_Super (postUser' $ user { userRequestPlugin = "" })

    void $ assertFail_ValidateT "Empty ident = error" (Validate Validate_CannotBeEmpty $ Just "ident") $
      rd_Super (postUser' $ user { userRequestIdent = "" })

    void $ assertFail_ValidateT "display_name > maxDisplayName = error" (Validate Validate_TooLong $ Just "display_name") $
      rd_Super (postUser' $ user { userRequestDisplayName = T.replicate 33 "A" })

    pure ()

  either (const $ left ()) (const $ right ()) lr



testCreateInvalidOrganizations :: RunnerM (Either () ())
testCreateInvalidOrganizations = do
  runEitherT $ do
    user <- liftIO buildValidUser
    pure ()
--    api  <- liftIO $ rd (postApi' $ ApiRequest (Just "comment) 0)





createOrganizations :: RunnerM ()
createOrganizations = do
--  org_a <- createOrganization "1240177678" $ OrganizationRequest "orga" (Just "org a") "Org A" "TestLand" "runner.org.a@adarq.org" Membership_Join [] Nothing Public 0
--  org_b <- createOrganization "1240177678" $ OrganizationRequest "orgb" (Just "org b") "Org B" "TestLand" "runner.org.b@adarq.org" Membership_InviteOnly  [] Nothing Public 0
--  org_c <- createOrganization "1240177678" $ OrganizationRequest "orgc" (Just "org c") "Org C" "TestLand" "runner.org.c@adarq.org" Membership_RequestInvite [] Nothing Public 0
--  modify (\st->st{ orgs = M.fromList [("orga", org_a), ("orgb", org_b), ("orgc", org_c)] })
  pure ()



createOrganization :: OrganizationRequest -> RunnerM ()
createOrganization org_req = do
  e_result <- rd_Super (postOrganization' org_req)
  case e_result of
    (Left err)           -> liftIO $ print "err"
    (Right org_response) -> do
      pure ()



removeOrganizations :: RunnerM ()
removeOrganizations = do
  pure ()
