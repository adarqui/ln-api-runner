{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module LN.Api.Runner.Assert where



import LN.Api.Runner.Print
import LN.T.Error (ApplicationError(..), ValidationError(..), ValidationErrorCode(..))

-- import           Control.Break              (break, loop)
-- import           Control.Concurrent         (forkIO, threadDelay)
-- import           Control.Exception
-- import           Control.Monad              (void)
-- import           Control.Monad
-- import           Control.Monad.Except
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import qualified Control.Monad.Trans.Either as Either
-- import           Control.Monad.Trans.Reader (ReaderT)
-- import qualified Control.Monad.Trans.Reader as Reader (asks)
-- import           Control.Monad.Trans.RWS    (RWST, asks, evalRWST, get, modify,
--                                              put)
-- import           Control.Monad.Trans.State  (StateT, evalStateT, runStateT)
-- import qualified Control.Monad.Trans.State  as State (get, modify, put)
-- import           Data.ByteString            (ByteString)
-- import           Data.Either                (Either (..), isLeft, isRight)
-- import           Data.Int                   (Int64)
-- import           Data.List                  (find)
-- import qualified Data.Map                   as M
-- import           Data.Monoid                ((<>))
-- import           Data.Rehtie
-- import           Data.String.Conversions
import           Data.Text                  (Text)
-- import qualified Data.Text                  as T
-- import           Data.Text.Arbitrary
-- import qualified Data.Text.IO               as TIO
import           Haskell.Api.Helpers (ApiError(..))
-- import           LN.Api
-- import           LN.Generate
-- import           LN.Sanitize
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
