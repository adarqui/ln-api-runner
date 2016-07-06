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

  runnerM
) where



-- import           Control.Break              (break, loop)
-- import           Control.Concurrent         (forkIO, threadDelay)
-- import           Control.Exception
import           Control.Monad              (void)
-- import           Control.Monad.Except
import           Control.Monad.IO.Class     ()
-- import           Control.Monad.Trans.Either (EitherT, runEitherT)
-- import qualified Control.Monad.Trans.Either as Either
-- import           Control.Monad.Trans.Reader (ReaderT)
-- import qualified Control.Monad.Trans.Reader as Reader (asks)
import           Control.Monad.Trans.RWS    (RWST, asks, evalRWST, get, modify,
                                              put)
-- import           Control.Monad.Trans.State  (StateT, evalStateT, runStateT)
-- import qualified Control.Monad.Trans.State  as State (get, modify, put)
-- import           Data.ByteString            (ByteString)
-- import           Data.Either                (Either (..), isLeft, isRight)
-- import           Data.Int                   (Int64)
-- import           Data.List                  (find)
import qualified Data.Map                   as M
-- import           Data.Monoid                ((<>))
-- import           Data.Rehtie
-- import           Data.String.Conversions
import           Data.Text                  (Text)
-- import qualified Data.Text                  as T
-- import           Data.Text.Arbitrary
-- import qualified Data.Text.IO               as TIO
import           Haskell.Api.Helpers (ApiOptions(..), defaultWreqOptions)
-- import           LN.Api
-- import           LN.Generate
-- import           LN.Sanitize
import           LN.T.Api.Response (ApiResponse)
import LN.T.Pack.Sanitized.User (UserSanitizedPackResponse)
import LN.T.Pack.Organization (OrganizationPackResponse)
-- import           LN.T.Error                 (ApplicationError (..),
--                                              ValidationError (..),
--                                              ValidationErrorCode (..))
-- import           LN.Validate
-- import           Prelude                    hiding (break)
-- import           Rainbow
-- import           System.Exit                (exitFailure)
-- import           Test.QuickCheck
-- import           Test.QuickCheck.Utf8



type RunnerM = RWST RunnerReader RunnerWriter RunnerState IO



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
  void $ evalRWST go defaultRunnerReader defaultRunnerState
  pure ()
