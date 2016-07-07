module LN.Api.Runner.Internal (
  io
) where



import           Control.Monad.IO.Class (MonadIO, liftIO)



io :: MonadIO m => IO a -> m a
io = liftIO
