{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module LN.Api.Runner.Print  (
  printFail,
  printActualFailure,
  printFatal,
  printPass,
  printInfo,
  printSection,
) where



import           Data.Monoid             ((<>))
import           Data.String.Conversions (ConvertibleStrings, cs)
import           Data.Text               (Text)
import qualified Data.Text.IO            as TIO (putStrLn)
import           LN.T.Internal.JSON      ()
import           Rainbow                 (blue, blue, bold, chunk, cyan, fore,
                                          green, putChunk, putChunkLn, red,
                                          white, (&))
import           System.Exit             (exitFailure)



printFail :: forall a. ConvertibleStrings a Text => a -> IO ()
printFail message = do
  putChunk $ chunk ("Fail: " :: Text) & fore red & bold
  TIO.putStrLn (cs message)



printActualFailure :: String -> IO ()
printActualFailure message = do
  putChunk $ chunk ("ActualFailure: " :: Text) & fore red & bold
  putChunkLn $ chunk message & fore cyan



printFatal :: String -> IO ()
printFatal message = do
  putChunk $ chunk ("Fatal: " :: Text) & fore red & bold
  putChunkLn $ chunk message & fore red & bold
  exitFailure



printPass :: forall a. ConvertibleStrings a Text => a -> IO ()
printPass message = do
  putChunk $ chunk ("Pass: " :: Text) & fore green & bold
  TIO.putStrLn (cs message)



printInfo :: String -> IO ()
printInfo message = do
  putChunk $ chunk ("Info: " :: Text) & fore white & bold
  putStrLn message



printSection :: String -> IO ()
printSection message = do
  putChunkLn $ chunk ("- " <> message) & fore blue & bold
