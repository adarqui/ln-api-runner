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



import           Control.Break              (break, loop)
import           Control.Concurrent         (forkIO, threadDelay)
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
import           Data.Int                   (Int64)
import           Data.List                  (find)
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
import           System.Exit                (exitFailure)
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



rd_AsUser
  :: (Monoid w, MonadIO m, Show a, Show b)
  => UserResponse
  -> ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_AsUser UserResponse{..} = rd_AsUserId userResponseId



rd_AsUserId
  :: (Monoid w, MonadIO m, Show b, Show a)
  => Int64
  -> ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_AsUserId user_id actions = do
  opts <- asks rApiOpts
  v <- liftIO $ runWith actions $ opts { apiKeyHeader = Just "x-as-user",  apiKey = Just (superKey <> (cs $ show user_id)) }
--  liftIO $ print v
  pure v



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



mustPassT' :: forall b (m :: * -> *) e. Monad m => m (Either e b) -> Either.EitherT () m ()
mustPassT' go = do
  x <- lift go
  case x of
    Left err -> leftT ()
    Right v  -> rightT ()



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



assertTrueT
  :: (Monad m, MonadIO m)
  => Text
  -> m Bool
  -> EitherT Bool m Bool
assertTrueT message go = assertBoolT message True go



assertFalseT
  :: (Monad m, MonadIO m)
  => Text
  -> m Bool
  -> EitherT Bool m Bool
assertFalseT message go = assertBoolT message False go



assertBoolT
  :: (Monad m, MonadIO m)
  => Text
  -> Bool
  -> m Bool
  -> EitherT Bool m Bool
assertBoolT message b go = do
  result <- lift go
  if result == b
    then (liftIO $ printPass message) *> rightT True
    else (liftIO $ printFail message) *> leftT False



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
    Left l  -> if test lr then rightT undefined else leftT l
    Right r -> if test lr then rightT r else leftT undefined



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



-- | TODO CLEANUP : HACKING IT UP
--
assertFail_ServerErrorT
  :: (Monad m, MonadIO m)
  => Text
  -> ApplicationError
  -> m (Either (ApiError ApplicationError) e)
  -> EitherT e m ApplicationError
assertFail_ServerErrorT message criteria go = do
  x <- lift go
  case x of
    Left (ServerError _ application_error) -> do
      if application_error /= criteria
        then do
          liftIO $ printFail message
          liftIO $ printActualFailure (show application_error)
          leftT undefined
        else (liftIO $ printPass message) *> rightT application_error
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



printSection :: String -> IO ()
printSection message = do
  putChunkLn $ chunk ("- " <> message) & fore blue & bold



-- | DONT USE THIS, WILL BREAK YOUR TERMINAL
--
-- NEED CENTRALIZED LOGGING FUNCTION TO HANDLE TERMINAL PROPERLY
--
launchRunners :: Int -> IO ()
launchRunners n = do
  forM_ [1..n] $ const $ forkIO launchRunner
  forever $ getLine



launchRunner :: IO ()
launchRunner = do
  printInfo "Launching API Runner"
  runnerRWST go
  printInfo "Done"
  where
  go = do
    testCreateUser >>= either (const $ liftIO (printFatal "testCreateUser must not fail.")) pure
    testCreateInvaidUsers

    testCreateOrganization >>= either (const $ liftIO (printFatal "testCreateOrganization must not fail.")) pure
    testCreateInvalidOrganizations

    forM_ [1..5] $ const $ do
      testOrganizations >>= either (const $ liftIO (printFatal "testOrganizations must not fail.")) pure

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

  liftIO $ printSection "Testing user creation"

  lr <- runEitherT $ do
    user_request <- liftIO buildValidUser
    user@UserResponse{..} <- _assertT "A valid user is created" isRight $
      rd_Super (postUser' user_request)
    void $ _assertTrueT "User is active" $ pure (userResponseActive == True)
    void $ _assertRetryT 5 "After a user is created, a profile is subsequently created" isRight $
      rd_Super (getUserProfiles_ByUserId' userResponseId)
    void $ _assertRetryT 5 "After a user is created, an api entry is subsequently created" isRight $
      rd_AsUserId userResponseId getApis'
    pure ()

  either (const $ left ()) (const $ right ()) lr



-- | Tests invalid user creation
-- User creation via this api call can only happen in "GOD MODE" anyway..
-- User creation via the api is used by ln-api-runner & ln-smf-migrate
--
testCreateInvaidUsers :: RunnerM (Either () ())
testCreateInvaidUsers = do

  liftIO $ printSection "Creating invalid users"

  lr <- runEitherT $ do
    user <- liftIO buildValidUser

    void $ _assertFail_ValidateT "Empty display_name = error" (Validate Validate_CannotBeEmpty $ Just "display_name") $
      rd_Super (postUser' $ user { userRequestDisplayName = "" })

    void $ _assertFail_ValidateT "Empty full_name = error" (Validate Validate_CannotBeEmpty $ Just "full_name") $
      rd_Super (postUser' $ user { userRequestFullName = "" })

    void $ _assertFail_ValidateT "Empty email = error" (Validate Validate_CannotBeEmpty $ Just "email") $
      rd_Super (postUser' $ user { userRequestEmail = "" })

    void $ _assertFail_ValidateT "Empty plugin = error" (Validate Validate_CannotBeEmpty $ Just "plugin") $
      rd_Super (postUser' $ user { userRequestPlugin = "" })

    void $ _assertFail_ValidateT "Empty ident = error" (Validate Validate_CannotBeEmpty $ Just "ident") $
      rd_Super (postUser' $ user { userRequestIdent = "" })

    void $ _assertFail_ValidateT "display_name > maxDisplayName = error" (Validate Validate_TooLong $ Just "display_name") $
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



testCreateOrganization :: RunnerM (Either () ())
testCreateOrganization = do

  liftIO $ printSection "Testing organization creation"

  lr <- runEitherT $ do
    owner_req <- liftIO buildValidUser
    org_req   <- liftIO buildValidOrganization
    owner                        <- _assertT "An owner is created" isRight $ rd_Super (postUser' owner_req)
    org@OrganizationResponse{..} <- _assertT "An organization is created" isRight $ rd_AsUser owner (postOrganization' org_req)
    void $ runEitherT $ _assertTrueT "Created organization is owned by owner" $ pure (organizationResponseUserId == (userResponseId owner))
    void $ _assertTrueT "Organization is active" $ pure (organizationResponseActive == True)

    _mustPassT $ testOrganizationsMembershipOwner org owner
    pure ()

  either (const $ left ()) (const $ right ()) lr



testOrganizations :: RunnerM (Either () ())
testOrganizations = do

  liftIO $ printSection "Testing Organizations"

  lr <- runEitherT $ do
    owner_req <- liftIO buildValidUser
    user_req  <- liftIO buildValidUser
    org_req   <- liftIO buildValidOrganization
    owner                        <- _assertT "An owner is created" isRight $ rd_Super (postUser' owner_req)
    user                         <- _assertT "A user is created" isRight $ rd_Super (postUser' user_req)
    org@OrganizationResponse{..} <- _assertT "An organization is created" isRight $ rd_AsUser owner (postOrganization' org_req)
    pure ()

  either (const $ left ()) (const $ right ()) lr



testOrganizationsMembershipOwner :: OrganizationResponse -> UserResponse -> RunnerM (Either () ())
testOrganizationsMembershipOwner org@OrganizationResponse{..} owner@UserResponse{..} = do

  liftIO $ printSection "Testing Organization Membership for an Owner"

  runEitherT $ do
    teams <- _assertT "Teams exist" isRight $ rd_AsUser owner (getTeams_ByOrganizationId' organizationResponseId)
    let team_responses = teamResponses teams
    void $ _assertTrueT "Only 2 teams exist" $ pure (length team_responses == 2)
    void $ _assertTrueT "Team_Owners exists" $ pure (elem Team_Owners $ map teamResponseSystem team_responses)
    void $ _assertTrueT "Team_Members exists" $ pure (elem Team_Members $ map teamResponseSystem team_responses)

    forM_ team_responses $ \team -> _mustPassT $ testOrganizationsMembership_OfTeam team owner

    forM_ team_responses $ \TeamResponse{..} -> do
      void $ _assertT "Cannot delete teams" isLeft $ rd_AsUser owner (deleteTeam' teamResponseId)

    forM_ team_responses $ \TeamResponse{..} -> do
      team_members <- _assertT "Team has members" isRight $ rd_AsUser owner (getTeamMembers_ByTeamId' teamResponseId)
      let team_member_responses = teamMemberResponses team_members
      _assertTrueT "Owner is a member of this team" $ pure $ (find (\TeamMemberResponse{..} -> teamMemberResponseUserId == userResponseId) team_member_responses) /= Nothing

      let my_memberships = filter (\TeamMemberResponse{..} -> teamMemberResponseUserId == userResponseId) team_member_responses
      forM_ my_memberships $ \TeamMemberResponse{..} -> do
        _assertT "Owner cannot delete themselves from a team" isLeft $ rd_AsUser owner (deleteTeamMember' teamMemberResponseId)

    pure ()

--  either (const $ left ()) (const $ right()) lr



testOrganizationsMembership_OfTeam :: TeamResponse -> UserResponse -> RunnerM (Either () ())
testOrganizationsMembership_OfTeam team@TeamResponse{..} user@UserResponse{..} = do

  liftIO $ printSection "Testing membership of an organization"

  runEitherT $ do
--    team_members <- _assertT "TeamMembers exists" isRight $ rd_AsUser user (getTeamMembers_ByTeamId' teamResponseId)
--    let team_member_responses = teamMemberResponses team_members
    pure ()




--
-- JANK LIFE: getting rid of 'e' and forcing it to () .. was having too much problems lining up types
--
--
_assertTrueT
  :: (Monad m, MonadIO m)
  => Text
  -> m Bool
  -> EitherT () m Bool
_assertTrueT message go = _assertBoolT message True go



_assertFalseT
  :: (Monad m, MonadIO m)
  => Text
  -> m Bool
  -> EitherT () m Bool
_assertFalseT message go = _assertBoolT message False go



_assertBoolT
  :: (Monad m, MonadIO m)
  => Text
  -> Bool
  -> m Bool
  -> EitherT () m Bool
_assertBoolT message b go = do
  result <- lift go
  if result == b
    then (liftIO $ printPass message) *> _rightT True
    else (liftIO $ printFail message) *> _leftT ()



_assertT
  :: (Monad m, MonadIO m)
  => Text
  -> (Either e a -> Bool)
  -> m (Either e a)
  -> EitherT () m a
_assertT message test go = do
  lr <- lift go
  if test lr
    then do
      (liftIO $ printPass message) *> _rightT ()
    else do
      liftIO $ printFail message
  case lr of
    Left l  -> if test lr then _rightT undefined else _leftT ()
    Right r -> if test lr then _rightT r else _leftT ()



-- | Retry `retries` times until a success
--
_assertRetryT
  :: (Monad m, MonadIO m)
  => Int
  -> Text
  -> (Either e a -> Bool)
  -> m (Either e a)
  -> EitherT () m a
_assertRetryT retries message test go = do

  lr <- lift $ runEitherT $ do
    _assertT message test go
  case lr of
    Left err -> do
      if retries == 0
        then liftIO (printActualFailure "Maximum retries attempted.") *> _leftT ()
        else _assertRetryT (retries-1) message test go
    Right v  -> _rightT v



-- | An assertion for Failure.
-- `go` must fail with Left _, in order for this test to Pass
--
_assertFail_ValidateT
  :: (Monad m, MonadIO m)
  => Text
  -> ValidationError
  -> m (Either (ApiError ApplicationError) e)
  -> EitherT () m ()
_assertFail_ValidateT message criteria go = do
  x <- lift go
  case x of
    Left (ServerError _ (Error_Validation error_validation)) -> do
      if error_validation /= criteria
        then do
          liftIO $ printFail message
          liftIO $ printActualFailure (show error_validation)
          _leftT ()
        else (liftIO $ printPass message) *> _rightT ()
    Left err -> do
      liftIO $ printFail message
      liftIO $ printActualFailure (show err)
      _leftT ()
    Right v  -> do
      liftIO $ printFail message
      _leftT ()



-- | TODO CLEANUP : HACKING IT UP
--
_assertFail_ServerErrorT
  :: (Monad m, MonadIO m)
  => Text
  -> ApplicationError
  -> m (Either (ApiError ApplicationError) e)
  -> EitherT () m ()
_assertFail_ServerErrorT message criteria go = do
  x <- lift go
  case x of
    Left (ServerError _ application_error) -> do
      if application_error /= criteria
        then do
          liftIO $ printFail message
          liftIO $ printActualFailure (show application_error)
          _leftT ()
        else (liftIO $ printPass message) *> _rightT ()
    Left err -> do
      liftIO $ printFail message
      liftIO $ printActualFailure (show err)
      _leftT ()
    Right v  -> do
      liftIO $ printFail message
      _leftT ()



_mustPassT :: forall b (m :: * -> *) e. Monad m => m (Either e b) -> Either.EitherT () m b
_mustPassT go = do
  x <- lift go
  case x of
    Left err -> _leftT ()
    Right v  -> _rightT v



_leftT :: forall e (m :: * -> *) a. Monad m => e -> Either.EitherT () m a
_leftT _ = Either.left ()



_rightT :: forall a e (m :: * -> *). Monad m => a -> Either.EitherT () m a
_rightT = Either.right
