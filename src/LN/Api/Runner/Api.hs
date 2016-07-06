{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module LN.Api.Runner.Api where


import LN.Api.Runner.Control

import Control.Monad.IO.Class (MonadIO, liftIO)
-- import           Control.Break              (break, loop)
-- import           Control.Concurrent         (forkIO, threadDelay)
-- import           Control.Exception
-- import           Control.Monad              (void)
-- import           Control.Monad
-- import           Control.Monad.Except
-- import           Control.Monad.IO.Class     (liftIO)
-- import           Control.Monad.Trans.Either (EitherT, runEitherT)
-- import qualified Control.Monad.Trans.Either as Either
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader (asks)
import           Control.Monad.Trans.RWS    (RWST, asks, evalRWST, get, modify,
                                              put)
-- import           Control.Monad.Trans.State  (StateT, evalStateT, runStateT)
-- import qualified Control.Monad.Trans.State  as State (get, modify, put)
import           Data.ByteString            (ByteString)
-- import           Data.Either                (Either (..), isLeft, isRight)
import           Data.Int                   (Int64)
-- import           Data.List                  (find)
-- import qualified Data.Map                   as M
import           Data.Monoid                ((<>))
-- import           Data.Rehtie
import           Data.String.Conversions (cs)
-- import           Data.Text                  (Text)
-- import qualified Data.Text                  as T
-- import           Data.Text.Arbitrary
-- import qualified Data.Text.IO               as TIO
import           Haskell.Api.Helpers (ApiOptions(..), ApiError(..), runWith)
import LN.T (UserResponse(..))
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



superKey :: ByteString
superKey = "pooppooppooppooppooppooppooppooppooppooppooppooppooppooppooppooppooppoop"



rd_Super
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_Super = rd_Api superKey



rd_AsApiKey
  :: (Monoid w, MonadIO m)
  => ByteString
  -> ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_AsApiKey = rd_Api



rd_Api
  :: (Monoid w, MonadIO m)
  => ByteString
  -> ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_Api api_key actions = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKey = Just api_key }



rd_AsUser
  :: (Monoid w, MonadIO m, Show a, Show b)
  => UserResponse
  -> ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_AsUser UserResponse{..} = rd_AsUserId userResponseId



rd_AsUserId
  :: (Monoid w, MonadIO m, Show b, Show a)
  => Int64
  -> ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_AsUserId user_id actions = do
  opts <- asks rApiOpts
  v <- liftIO $ runWith actions $ opts { apiKeyHeader = Just "x-as-user",  apiKey = Just (superKey <> (cs $ show user_id)) }
--  liftIO $ print v
  pure v



rd_Guest
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_Guest actions = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts



rw
  :: (Monoid w, MonadIO m)
  => ReaderT ApiOptions IO (Either (ApiError b) a)
  -> ByteString
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rw actions s = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKey = Just s }



left :: forall a (f :: * -> *) b. Applicative f => a -> f (Either a b)
left  = pure . Left



right :: forall a (f :: * -> *) a1. Applicative f => a -> f (Either a1 a)
right = pure . Right
