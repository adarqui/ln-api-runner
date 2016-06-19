{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Api.Tester.Internal (
  launchTester,
  cleanupTester
) where



import Control.Monad.Except
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Trans.RWS
import qualified Data.Map                as M
import           Haskell.Api.Helpers
import           LN.Api
import           LN.T



data TesterReader = TesterReader {
  rInt :: Int
}



type TesterWriter = ()



type TesterRWST = RWST TesterReader TesterWriter TesterState IO



rd actions = runWith actions apiOpts { apiKey = Just "1" }
rw actions s = runWith actions $ apiOpts { apiKey = Just s }



apiOpts = ApiOptions {
  apiUrl = "https://leuro.adarq.org",
  apiPrefix = "api",
  apiKey = Nothing,
  apiKeyHeader = Just "z-authorization",
  apiWreqOptions = defaultWreqOptions,
  apiDebug = True
}



data TesterState = TesterState {
  orgs  :: M.Map String OrganizationPackResponse,
  users :: M.Map String UserSanitizedPackResponse,
  keys  :: M.Map String ApiResponse
} deriving (Show)



defaultTesterState = TesterState {
  orgs = M.empty,
  users = M.empty,
  keys = M.empty
}



launchTester :: IO ()
launchTester = do
  putStrLn "Launching"
  testerRWST go
  where
  go = do
    createOrganizations



cleanupTester :: IO ()
cleanupTester = do
  putStrLn "Cleanup"
  testerRWST go
  where
  go = do
    removeOrganizations



-- testerRWST :: forall w a. RWST TesterReader w TesterState IO a -> IO ()
testerRWST :: forall a. TesterRWST a -> IO ()
testerRWST go = do
  void $ evalRWST go (TesterReader 0) defaultTesterState
  return ()



createOrganizations :: TesterRWST ()
createOrganizations = do
--  org_a <- createOrganization "1240177678" $ OrganizationRequest "orga" (Just "org a") "Org A" "TestLand" "tester.org.a@adarq.org" Membership_Join [] Nothing Public 0
--  org_b <- createOrganization "1240177678" $ OrganizationRequest "orgb" (Just "org b") "Org B" "TestLand" "tester.org.b@adarq.org" Membership_InviteOnly  [] Nothing Public 0
--  org_c <- createOrganization "1240177678" $ OrganizationRequest "orgc" (Just "org c") "Org C" "TestLand" "tester.org.c@adarq.org" Membership_RequestInvite [] Nothing Public 0
--  modify (\st->st{ orgs = M.fromList [("orga", org_a), ("orgb", org_b), ("orgc", org_c)] })
  return ()



createOrganization :: String -> OrganizationRequest -> TesterRWST ()
createOrganization ts org_req = do
  e_result <- liftIO $ rd (postOrganization [UnixTimestamp $ read ts] $ org_req)
  case e_result of
    (Left err)           -> liftIO $ print err
    (Right org_response) -> do
      return ()



removeOrganizations :: TesterRWST ()
removeOrganizations = do
  return ()
