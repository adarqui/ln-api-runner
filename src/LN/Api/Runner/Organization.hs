{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Api.Runner.Organization (
  testCreateOrganization,
  testCreateInvalidOrganizations,
  testOrganizations,
  createOrganization,
  createOrganizations,
  removeOrganizations
) where



import           Control.Monad              (forM_, void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (runEitherT)
import           Data.Either                (isLeft, isRight)
import           Data.List                  (find)
import           LN.Api
import           LN.Api.Runner.Api
import           LN.Api.Runner.Assert
import           LN.Api.Runner.Control
import           LN.Api.Runner.Print
import           LN.Generate
import           LN.T.Organization.Request  (OrganizationRequest (..))
import           LN.T.Organization.Response (OrganizationResponse (..))
import           LN.T.Team                  (SystemTeam (..))
import           LN.T.Team.Response         (TeamResponse (..),
                                             TeamResponses (..))
import           LN.T.TeamMember.Response   (TeamMemberResponse (..),
                                             TeamMemberResponses (..))
import           LN.T.User.Response         (UserResponse (..))



testCreateInvalidOrganizations :: RunnerM (Either () ())
testCreateInvalidOrganizations = do
  runEitherT $ do
    _ <- liftIO buildValidUser
    pure ()



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
    (Left _)  -> liftIO $ putStrLn "err"
    (Right _) -> do
      pure ()



removeOrganizations :: RunnerM ()
removeOrganizations = do
  pure ()



testCreateOrganization :: RunnerM (Either () ())
testCreateOrganization = do

  printSection "Testing organization creation"

  lr <- runEitherT $ do
    owner_req <- liftIO buildValidUser
    org_req   <- liftIO buildValidOrganization
    owner                        <- assertT "An owner is created" isRight $ rd_Super (postUser' owner_req)
    org@OrganizationResponse{..} <- assertT "An organization is created" isRight $ rd_AsUser owner (postOrganization' org_req)
    void $ assertTrueT "Created organization is owned by owner" $ pure (organizationResponseUserId == (userResponseId owner))
    void $ assertTrueT "Organization is active" $ pure (organizationResponseActive == True)

    mustPassT $ testOrganizationsMembershipOwner org owner
    pure ()

  either (const $ left ()) (const $ right ()) lr



testOrganizations :: RunnerM (Either () ())
testOrganizations = do

  printSection "Testing Organizations"

  lr <- runEitherT $ do
    owner_req <- liftIO buildValidUser
    user_req  <- liftIO buildValidUser
    org_req   <- liftIO buildValidOrganization
    owner                    <- assertT "An owner is created" isRight $ rd_Super (postUser' owner_req)
    _                        <- assertT "A user is created" isRight $ rd_Super (postUser' user_req)
    OrganizationResponse{..} <- assertT "An organization is created" isRight $ rd_AsUser owner (postOrganization' org_req)
    pure ()

  either (const $ left ()) (const $ right ()) lr



testOrganizationsMembershipOwner :: OrganizationResponse -> UserResponse -> RunnerM (Either () ())
testOrganizationsMembershipOwner OrganizationResponse{..} owner@UserResponse{..} = do

  printSection "Testing Organization Membership for an Owner"

  runEitherT $ do
    teams <- assertT "Teams exist" isRight $ rd_AsUser owner (getTeams_ByOrganizationId' organizationResponseId)
    let team_responses = teamResponses teams
    void $ assertTrueT "Only 2 teams exist" $ pure (length team_responses == 2)
    void $ assertTrueT "Team_Owners exists" $ pure (elem Team_Owners $ map teamResponseSystem team_responses)
    void $ assertTrueT "Team_Members exists" $ pure (elem Team_Members $ map teamResponseSystem team_responses)

    forM_ team_responses $ \team -> mustPassT $ testOrganizationsMembership_OfTeam team owner

    forM_ team_responses $ \TeamResponse{..} -> do
      void $ assertT "Cannot delete teams" isLeft $ rd_AsUser owner (deleteTeam' teamResponseId)

    forM_ team_responses $ \TeamResponse{..} -> do
      team_members <- assertT "Team has members" isRight $ rd_AsUser owner (getTeamMembers_ByTeamId' teamResponseId)
      let team_member_responses = teamMemberResponses team_members
      void $ assertTrueT "Owner is a member of this team" $ pure $ (find (\TeamMemberResponse{..} -> teamMemberResponseUserId == userResponseId) team_member_responses) /= Nothing

      let my_memberships = filter (\TeamMemberResponse{..} -> teamMemberResponseUserId == userResponseId) team_member_responses
      forM_ my_memberships $ \TeamMemberResponse{..} -> do
        assertT "Owner cannot delete themselves from a team" isLeft $ rd_AsUser owner (deleteTeamMember' teamMemberResponseId)

    pure ()

--  either (const $ left ()) (const $ right()) lr



testOrganizationsMembership_OfTeam :: TeamResponse -> UserResponse -> RunnerM (Either () ())
testOrganizationsMembership_OfTeam TeamResponse{..} UserResponse{..} = do

  printSection "Testing membership of an organization"

  runEitherT $ do
--    team_members <- assertT "TeamMembers exists" isRight $ rd_AsUser user (getTeamMembers_ByTeamId' teamResponseId)
--    let team_member_responses = teamMemberResponses team_members
    pure ()
