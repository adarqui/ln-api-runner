module LN.Api.Runner.Launch (
  launchRunners,
  launchRunner,
  cleanupRunner
) where



import           Control.Monad              (forM_, void)
import           LN.Api.Runner.Control
import           LN.Api.Runner.Organization
import           LN.Api.Runner.Print
import           LN.Api.Runner.User



-- | Can't multi-thread this because of the terminal operations/output
--
launchRunners :: Int -> IO ()
launchRunners n = do
  forM_ [1..n] $ const launchRunner



launchRunner :: IO ()
launchRunner = do
  runnerM go
  where
  go = do
    printInfo "Launching API Runner"
    enterM

    testCreateUser >>= either (const $ printFatal "testCreateUser must not fail.") pure
    void $ testCreateInvalidUsers

    testCreateOrganization >>= either (const $ printFatal "testCreateOrganization must not fail.") pure
    void $ testCreateInvalidOrganizations

    forM_ [1 .. 5 :: Int] $ const $ do
      testOrganizations >>= either (const $ printFatal "testOrganizations must not fail.") pure

    leaveM
    printStats
    pure ()



cleanupRunner :: IO ()
cleanupRunner = do
  runnerM go
  where
  go = do
    printInfo "Cleanup"
    enterM
    removeUsers
    removeOrganizations
    leaveM
