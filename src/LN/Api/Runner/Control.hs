{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module LN.Api.Runner.Control (
  RunnerM,
  RunnerReader (..),
  defaultRunnerReader,
  RunnerWriter,
  defaultRunnerWriter,
  RunnerState (..),
  defaultRunnerState,
  defaultApiOpts,
  runnerM,
  left,
  right,
  leftT,
  rightT,
  enterM,
  leaveM
) where



import           Control.Monad              (void)
import           Control.Monad.IO.Class     ()
import           Control.Monad.State.Lazy   (get, put)
import           Control.Monad.StateStack   (StateStackT, restore,
                                             runStateStackT, save)
import           Control.Monad.Trans        (lift)
import qualified Control.Monad.Trans.Either as Either
import           Control.Monad.Trans.RWS    (RWST, runRWST)
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import           Haskell.Api.Helpers        (ApiOptions (..),
                                             defaultWreqOptions)
import           LN.T.Api.Response          (ApiResponse)
import           LN.T.Pack.Organization     (OrganizationPackResponse)
import           LN.T.Pack.Sanitized.User   (UserSanitizedPackResponse)



type RunnerM = RWST RunnerReader RunnerWriter RunnerState (StateStackT StackState IO)



data StackState = StackState {
  level :: Int
} deriving (Show, Eq)

defaultStackState :: StackState
defaultStackState = StackState {
  level = 0
}



data RunnerReader = RunnerReader {
  rApiOpts :: ApiOptions
}

defaultRunnerReader :: RunnerReader
defaultRunnerReader = RunnerReader {
  rApiOpts = defaultApiOpts
}



type RunnerWriter = ()

defaultRunnerWriter :: RunnerWriter
defaultRunnerWriter = ()



data RunnerState = RunnerState {
  orgs  :: M.Map Text OrganizationPackResponse,
  users :: M.Map Text UserSanitizedPackResponse,
  keys  :: M.Map Text ApiResponse
}

defaultRunnerState :: RunnerState
defaultRunnerState = RunnerState {
  orgs = M.empty,
  users = M.empty,
  keys = M.empty
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



runnerM :: forall a. RunnerM a -> IO ()
runnerM go = do
  void $ runStateStackT (runRWST go defaultRunnerReader defaultRunnerState) defaultStackState
  pure ()



left :: forall a (f :: * -> *) b. Applicative f => a -> f (Either a b)
left  = pure . Left



right :: forall a (f :: * -> *) a1. Applicative f => a -> f (Either a1 a)
right = pure . Right




leftT :: forall (m :: * -> *) a e. Monad m => e -> Either.EitherT () m a
leftT _ = Either.left ()



rightT :: forall a (m :: * -> *). Monad m => a -> Either.EitherT () m a
rightT = Either.right



enterM :: RunnerM ()
enterM = do
  StackState{..} <- lift $ get
  lift $ put $ StackState (level+1)
  lift save



leaveM :: RunnerM ()
leaveM = do
  StackState{..} <- lift get
  lift $ put $ StackState (level-1)
  lift restore
