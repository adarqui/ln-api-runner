{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}

module LN.Api.Runner.Print  (
  printFailT,
  printFail,
  printActualFailureT,
  printActualFailure,
  printFatal,
  printPassT,
  printPass,
  printInfoT,
  printInfo,
  printSection,
) where



import           Control.Monad.Trans     (MonadTrans, lift)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (ConvertibleStrings, cs)
import           Data.Text               (Text)
import qualified Data.Text.IO            as TIO (putStrLn)
import           LN.Api.Runner.Control
import           LN.Api.Runner.Internal
import           LN.T.Internal.JSON      ()
import           Rainbow                 (blue, blue, bold, chunk, cyan, fore,
                                          green, putChunk, putChunkLn, red,
                                          white, (&))
import           System.Exit             (exitFailure)




printFailT :: forall (t :: (* -> *) -> * -> *) a. (MonadTrans t, ConvertibleStrings a Text) => a -> t RunnerM ()
printFailT s = lift $ printFail s



printFail :: forall a. ConvertibleStrings a Text => a -> RunnerM ()
printFail message = do
  io $ putChunk $ chunk ("Fail: " :: Text) & fore red & bold
  io $ TIO.putStrLn (cs message)



printActualFailureT :: forall (t :: (* -> *) -> * -> *). MonadTrans t => String -> t RunnerM ()
printActualFailureT s = lift $ printActualFailure s



printActualFailure :: String -> RunnerM ()
printActualFailure message = do
  io $ putChunk $ chunk ("ActualFailure: " :: Text) & fore red & bold
  io $ putChunkLn $ chunk message & fore cyan



printFatal :: String -> RunnerM ()
printFatal message = do
  io $ putChunk $ chunk ("Fatal: " :: Text) & fore red & bold
  io $ putChunkLn $ chunk message & fore red & bold
  io $ exitFailure



printPassT :: forall (t :: (* -> *) -> * -> *) a. (MonadTrans t, ConvertibleStrings a Text) => a -> t RunnerM ()
printPassT s = lift $ printPass s



printPass :: forall a. ConvertibleStrings a Text => a -> RunnerM ()
printPass message = do
  io $ putChunk $ chunk ("Pass: " :: Text) & fore green & bold
  io $ TIO.putStrLn (cs message)



printInfoT :: forall (t :: (* -> *) -> * -> *). MonadTrans t => String -> t RunnerM ()
printInfoT s = lift $ printInfo s



printInfo :: String -> RunnerM ()
printInfo message = do
  io $ putChunk $ chunk ("Info: " :: Text) & fore white & bold
  io $ putStrLn message



printSection :: String -> RunnerM ()
printSection message = do
  io $ putChunkLn $ chunk ("- " <> message) & fore blue & bold
