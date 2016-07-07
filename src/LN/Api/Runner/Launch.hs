module LN.Api.Runner.Launch (
  launchRunners,
  launchRunner,
  cleanupRunner
) where



import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forM_, forever, void)
import           Control.Monad.IO.Class     (liftIO)
import           LN.Api.Runner.Control
import           LN.Api.Runner.Organization
import           LN.Api.Runner.Print
import           LN.Api.Runner.User



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
  runnerM go
  printInfo "Done"
  where
  go = do
    testCreateUser >>= either (const $ liftIO (printFatal "testCreateUser must not fail.")) pure
    testCreateInvalidUsers

    testCreateOrganization >>= either (const $ liftIO (printFatal "testCreateOrganization must not fail.")) pure
    testCreateInvalidOrganizations

    forM_ [1..5] $ const $ do
      testOrganizations >>= either (const $ liftIO (printFatal "testOrganizations must not fail.")) pure

    pure ()



cleanupRunner :: IO ()
cleanupRunner = do
  putStrLn "Cleanup"
  runnerM go
  where
  go = do
    removeUsers
    removeOrganizations
