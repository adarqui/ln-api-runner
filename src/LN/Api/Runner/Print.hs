{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}

module LN.Api.Runner.Print  (
  printFailT,
  printFail,
  printActualFailureT,
  printActualFailure,
  printFatal,
  printPassT,
  printPass,
  printWarningT,
  printWarning,
  printInfoT,
  printInfo,
  printSection,
  printStats,
  indent
) where



import           Control.Monad.State.Lazy (get, gets)
import           Control.Monad.Trans      (MonadTrans, lift)
import           Data.Monoid              ((<>))
import           Data.String.Conversions  (ConvertibleStrings, cs)
import           Data.Text                (Text)
import qualified Data.Text.IO             as TIO (putStrLn)
import           LN.Api.Runner.Control
import           LN.Api.Runner.Internal
import           Rainbow                  (blue, blue, bold, chunk, cyan, fore,
                                           green, putChunk, putChunkLn, red,
                                           white, yellow, (&))
import           System.Exit              (exitFailure)




printFailT :: forall (t :: (* -> *) -> * -> *) a. (MonadTrans t, ConvertibleStrings a Text) => a -> t RunnerM ()
printFailT s = lift $ printFail s



printFail :: forall a. ConvertibleStrings a Text => a -> RunnerM ()
printFail message = do
  indent
  incFail
  io $ putChunk $ chunk ("Fail: " :: Text) & fore red & bold
  io $ TIO.putStrLn (cs message)



printActualFailureT :: forall (t :: (* -> *) -> * -> *). MonadTrans t => String -> t RunnerM ()
printActualFailureT s = lift $ printActualFailure s



printActualFailure :: String -> RunnerM ()
printActualFailure message = do
  indent
  io $ putChunk $ chunk ("ActualFailure: " :: Text) & fore red & bold
  io $ putChunkLn $ chunk message & fore cyan



printFatal :: String -> RunnerM ()
printFatal message = do
  indent
  incFatal
  io $ putChunk $ chunk ("Fatal: " :: Text) & fore red & bold
  io $ putChunkLn $ chunk message & fore red & bold
  io $ exitFailure



printPassT :: forall (t :: (* -> *) -> * -> *) a. (MonadTrans t, ConvertibleStrings a Text) => a -> t RunnerM ()
printPassT s = lift $ printPass s



printPass :: forall a. ConvertibleStrings a Text => a -> RunnerM ()
printPass message = do
  indent
  incPass
  io $ putChunk $ chunk ("Pass: " :: Text) & fore green & bold
  io $ TIO.putStrLn (cs message)



printWarningT :: forall (t :: (* -> *) -> * -> *). MonadTrans t => String -> t RunnerM ()
printWarningT s = lift $ printWarning s



printWarning :: String -> RunnerM ()
printWarning message = do
  indent
  incWarning
  io $ putChunk $ chunk ("Warning: " :: Text) & fore yellow & bold
  io $ putStrLn message



printInfoT :: forall (t :: (* -> *) -> * -> *). MonadTrans t => String -> t RunnerM ()
printInfoT s = lift $ printInfo s



printInfo :: String -> RunnerM ()
printInfo message = do
  indent
  incSection
  io $ putChunk $ chunk ("Info: " :: Text) & fore white & bold
  io $ putStrLn message



printSection :: String -> RunnerM ()
printSection message = do
  indent
  io $ putChunkLn $ chunk ("- " <> message) & fore blue & bold



printStats :: RunnerM ()
printStats = do
  RunnerState{..} <- get
  io $ putChunkLn $ chunk ("Test sections: " <> show statSection) & fore blue & bold
  io $ putChunkLn $ chunk ("Passing tests: " <> show statPass)    & fore green & bold
  io $ putChunkLn $ chunk ("Warnings: " <> show statWarning)      & fore yellow & bold
  io $ putChunkLn $ chunk ("Failed tests: " <> show statFail)     & fore red & bold
  io $ putChunkLn $ chunk ("Fatal tests: " <> show statFatal)     & fore red & bold



indent :: RunnerM ()
indent = do
  level <- lift $ gets level
  io $ putStr $ replicate level ' '
