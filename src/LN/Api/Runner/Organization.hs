{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module LN.Api.Runner.Organization where



import LN.Api.Runner.Control
import LN.Api.Runner.Api
import LN.Api.Runner.Print
import LN.T.Team.Response (TeamResponse(..), TeamResponses(..))
import LN.T.Team (SystemTeam(..))
import LN.T.TeamMember.Response (TeamMemberResponse(..), TeamMemberResponses(..))
import LN.T.User.Response (UserResponse(..))
import LN.T.Organization.Response (OrganizationResponse(..))
import LN.T.Organization.Request (OrganizationRequest(..))
import Data.List (find)
import LN.Api.Runner.Assert
-- import           Control.Break              (break, loop)
-- import           Control.Concurrent         (forkIO, threadDelay)
-- import           Control.Exception
import           Control.Monad              (void, forM_)
-- import           Control.Monad
-- import           Control.Monad.Except
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
-- import qualified Control.Monad.Trans.Either as Either
-- import           Control.Monad.Trans.Reader (ReaderT)
-- import qualified Control.Monad.Trans.Reader as Reader (asks)
import           Control.Monad.Trans.RWS    (RWST, asks, evalRWST, get, modify,
                                             put)
-- import           Control.Monad.Trans.State  (StateT, evalStateT, runStateT)
-- import qualified Control.Monad.Trans.State  as State (get, modify, put)
-- import           Data.ByteString            (ByteString)
import           Data.Either                (Either (..), isLeft, isRight)
-- import           Data.Int                   (Int64)
-- import           Data.List                  (find)
-- import qualified Data.Map                   as M
-- import           Data.Monoid                ((<>))
-- import           Data.Rehtie
-- import           Data.String.Conversions
-- import           Data.Text                  (Text)
-- import qualified Data.Text                  as T
-- import           Data.Text.Arbitrary
-- import qualified Data.Text.IO               as TIO
-- import           Haskell.Api.Helpers
import           LN.Api
import           LN.Generate
--import           LN.Sanitize
-- import           LN.T
-- import           LN.T.Error                 (ApplicationError (..),
--                                              ValidationError (..),
--                                              ValidationErrorCode (..))
-- import           LN.Validate
-- import           Prelude                    hiding (break)
-- import           Rainbow
-- import           System.Exit                (exitFailure)
-- import           Test.QuickCheck
-- import           Test.QuickCheck.Utf8



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
