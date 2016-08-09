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



import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import qualified Control.Monad.Trans.Either as Either
import           Data.Text                  (Text)
import           Haskell.Api.Helpers.Shared (ApiError (..))
import           LN.Api.Runner.Control
import           LN.Api.Runner.Print
import           LN.T.Error                 (ApplicationError (..),
                                             ValidationError (..))



assertTrueT
  :: Text
  -> RunnerM Bool
  -> EitherT () RunnerM Bool
assertTrueT message go = assertBoolT message True go



assertFalseT
  :: Text
  -> RunnerM Bool
  -> EitherT () RunnerM Bool
assertFalseT message go = assertBoolT message False go



assertBoolT
  :: Text
  -> Bool
  -> RunnerM Bool
  -> EitherT () RunnerM Bool
assertBoolT message b go = do
  result <- lift go
  if result == b
    then (printPassT message) *> rightT True
    else (printFailT message) *> leftT ()



assertT
  :: Text
  -> (Either e a -> Bool)
  -> RunnerM (Either e a)
  -> EitherT () RunnerM a
assertT message test go = do
  lr <- lift go
  if test lr
    then do
      (printPassT message) *> rightT ()
    else do
      printFailT message
  case lr of
    Left _  -> if test lr then rightT undefined else leftT ()
    Right r -> if test lr then rightT r else leftT ()



-- | Retry `retries` times until a success
--
assertRetryT
  :: Int
  -> Text
  -> (Either e a -> Bool)
  -> RunnerM (Either e a)
  -> EitherT () RunnerM a
assertRetryT retries message test go = do

  lr <- lift $ runEitherT $ do
    assertT message test go
  case lr of
    Left _  -> do
      if retries == 0
        then (printActualFailureT "Maximum retries attempted.") *> leftT ()
        else assertRetryT (retries-1) message test go
    Right v -> rightT v



-- | An assertion for Failure.
-- `go` must fail with Left _, in order for this test to Pass
--
assertFail_ValidateT
  :: Text
  -> ValidationError
  -> RunnerM (Either (ApiError ApplicationError) e)
  -> EitherT () RunnerM ()
assertFail_ValidateT message criteria go = do
  x <- lift go
  case x of
    Left (ServerError _ (Error_Validation error_validation)) -> do
      if error_validation /= criteria
        then do
          printFailT message
          printActualFailureT (show error_validation)
          leftT ()
        else (printPassT message) *> rightT ()
    Left err -> do
      printFailT message
      printActualFailureT (show err)
      leftT ()
    Right _  -> do
      printFailT message
      leftT ()



-- | TODO CLEANUP : HACKING IT UP
--
assertFail_ServerErrorT
  :: Text
  -> ApplicationError
  -> RunnerM (Either (ApiError ApplicationError) e)
  -> EitherT () RunnerM ()
assertFail_ServerErrorT message criteria go = do
  x <- lift go
  case x of
    Left (ServerError _ application_error) -> do
      if application_error /= criteria
        then do
          printFailT message
          printActualFailureT (show application_error)
          leftT ()
        else (printPassT message) *> rightT ()
    Left err -> do
      printFailT message
      printActualFailureT (show err)
      leftT ()
    Right _  -> do
      printFailT message
      leftT ()



mustPassT :: forall b e. RunnerM (Either e b) -> Either.EitherT () RunnerM b
mustPassT go = do
  x <- lift go
  case x of
    Left _  -> leftT ()
    Right v -> rightT v
