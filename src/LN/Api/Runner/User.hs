{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Api.Runner.User (
  createUsers,
  removeUsers,
  testCreateUser,
  testCreateInvalidUsers
) where


import           Control.Monad              (forever, void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Either                (Either (..), isLeft, isRight)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (replicate)
import           LN.Api
import           LN.Api.Runner.Api
import           LN.Api.Runner.Assert
import           LN.Api.Runner.Control
import           LN.Api.Runner.Print
import           LN.Generate
import           LN.T.Error                 (ApplicationError (..),
                                             ValidationError (..),
                                             ValidationErrorCode (..))
import           LN.T.User.Request          (UserRequest (..))
import           LN.T.User.Response         (UserResponse (..))



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
testCreateInvalidUsers :: RunnerM (Either () ())
testCreateInvalidUsers = do

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
