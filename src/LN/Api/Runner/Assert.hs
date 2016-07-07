{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module LN.Api.Runner.Assert (
  assertTrueT,
  assertFalseT,
  assertBoolT,
  assertT,
  assertRetryT,
  assertFail_ValidateT,
  assertFail_ServerErrorT,
  mustPassT
) where



import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import qualified Control.Monad.Trans.Either as Either
import           Data.Text                  (Text)
import           Haskell.Api.Helpers        (ApiError (..))
import           LN.Api.Runner.Control
import           LN.Api.Runner.Print
import           LN.T.Error                 (ApplicationError (..),
                                             ValidationError (..))



assertTrueT
  :: (Monad m, MonadIO m)
  => Text
  -> m Bool
  -> EitherT () m Bool
assertTrueT message go = assertBoolT message True go



assertFalseT
  :: (Monad m, MonadIO m)
  => Text
  -> m Bool
  -> EitherT () m Bool
assertFalseT message go = assertBoolT message False go



assertBoolT
  :: (Monad m, MonadIO m)
  => Text
  -> Bool
  -> m Bool
  -> EitherT () m Bool
assertBoolT message b go = do
  result <- lift go
  if result == b
    then (liftIO $ printPass message) *> rightT True
    else (liftIO $ printFail message) *> leftT ()



assertT
  :: (Monad m, MonadIO m)
  => Text
  -> (Either e a -> Bool)
  -> m (Either e a)
  -> EitherT () m a
assertT message test go = do
  lr <- lift go
  if test lr
    then do
      (liftIO $ printPass message) *> rightT ()
    else do
      liftIO $ printFail message
  case lr of
    Left _  -> if test lr then rightT undefined else leftT ()
    Right r -> if test lr then rightT r else leftT ()



-- | Retry `retries` times until a success
--
assertRetryT
  :: (Monad m, MonadIO m)
  => Int
  -> Text
  -> (Either e a -> Bool)
  -> m (Either e a)
  -> EitherT () m a
assertRetryT retries message test go = do

  lr <- lift $ runEitherT $ do
    assertT message test go
  case lr of
    Left _  -> do
      if retries == 0
        then liftIO (printActualFailure "Maximum retries attempted.") *> leftT ()
        else assertRetryT (retries-1) message test go
    Right v -> rightT v



-- | An assertion for Failure.
-- `go` must fail with Left _, in order for this test to Pass
--
assertFail_ValidateT
  :: (Monad m, MonadIO m)
  => Text
  -> ValidationError
  -> m (Either (ApiError ApplicationError) e)
  -> EitherT () m ()
assertFail_ValidateT message criteria go = do
  x <- lift go
  case x of
    Left (ServerError _ (Error_Validation error_validation)) -> do
      if error_validation /= criteria
        then do
          liftIO $ printFail message
          liftIO $ printActualFailure (show error_validation)
          leftT ()
        else (liftIO $ printPass message) *> rightT ()
    Left err -> do
      liftIO $ printFail message
      liftIO $ printActualFailure (show err)
      leftT ()
    Right _  -> do
      liftIO $ printFail message
      leftT ()



-- | TODO CLEANUP : HACKING IT UP
--
assertFail_ServerErrorT
  :: (Monad m, MonadIO m)
  => Text
  -> ApplicationError
  -> m (Either (ApiError ApplicationError) e)
  -> EitherT () m ()
assertFail_ServerErrorT message criteria go = do
  x <- lift go
  case x of
    Left (ServerError _ application_error) -> do
      if application_error /= criteria
        then do
          liftIO $ printFail message
          liftIO $ printActualFailure (show application_error)
          leftT ()
        else (liftIO $ printPass message) *> rightT ()
    Left err -> do
      liftIO $ printFail message
      liftIO $ printActualFailure (show err)
      leftT ()
    Right _  -> do
      liftIO $ printFail message
      leftT ()



mustPassT :: forall b (m :: * -> *) e. Monad m => m (Either e b) -> Either.EitherT () m b
mustPassT go = do
  x <- lift go
  case x of
    Left _  -> leftT ()
    Right v -> rightT v
