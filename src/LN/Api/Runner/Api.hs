{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Api.Runner.Api (
  rd_Super,
  rd_AsUser,
  rd_AsUserId,
  rd_AsApiKey,
  rd_Guest,
  rw,
  left,
  right
) where



import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.RWS    (RWST, asks)
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Monoid                ((<>))
import           Data.String.Conversions    (cs)
import           Haskell.Api.Helpers        (SpecificApiOptions)
import           Haskell.Api.Helpers.Shared (ApiError (..), ApiOptions (..),
                                             runWith)
import           LN.Api.Runner.Control
import           LN.T                       (UserResponse (..))



superKey :: ByteString
superKey = "pooppooppooppooppooppooppooppooppooppooppooppooppooppooppooppooppooppoop"



rd_Super
  :: (Monoid w, MonadIO m)
  => ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_Super = rd_Api superKey



rd_AsApiKey
  :: (Monoid w, MonadIO m)
  => ByteString
  -> ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_AsApiKey = rd_Api



rd_Api
  :: (Monoid w, MonadIO m)
  => ByteString
  -> ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_Api api_key actions = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKey = Just api_key }



rd_AsUser
  :: (Monoid w, MonadIO m, Show a, Show b)
  => UserResponse
  -> ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_AsUser UserResponse{..} = rd_AsUserId userResponseId



rd_AsUserId
  :: (Monoid w, MonadIO m, Show b, Show a)
  => Int64
  -> ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_AsUserId user_id actions = do
  opts <- asks rApiOpts
  v <- liftIO $ runWith actions $ opts { apiKeyHeader = Just "x-as-user",  apiKey = Just (superKey <> (cs $ show user_id)) }
--  liftIO $ print v
  pure v



rd_Guest
  :: (Monoid w, MonadIO m)
  => ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError b) a)
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rd_Guest actions = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts



rw
  :: (Monoid w, MonadIO m)
  => ReaderT (ApiOptions SpecificApiOptions) IO (Either (ApiError b) a)
  -> ByteString
  -> RWST RunnerReader w s m (Either (ApiError b) a)
rw actions s = do
  opts <- asks rApiOpts
  liftIO $ runWith actions $ opts { apiKey = Just s }
