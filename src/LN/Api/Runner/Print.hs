{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module LN.Api.Runner.Print where



import           Control.Break              (break, loop)
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Exception
import           Control.Monad              (void)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import qualified Control.Monad.Trans.Either as Either
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader (asks)
import           Control.Monad.Trans.RWS    (RWST, asks, evalRWST, get, modify,
                                             put)
import           Control.Monad.Trans.State  (StateT, evalStateT, runStateT)
import qualified Control.Monad.Trans.State  as State (get, modify, put)
import           Data.ByteString            (ByteString)
import           Data.Either                (Either (..), isLeft, isRight)
import           Data.Int                   (Int64)
import           Data.List                  (find)
import qualified Data.Map                   as M
import           Data.Monoid                ((<>))
import           Data.Rehtie
import           Data.String.Conversions
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Arbitrary
import qualified Data.Text.IO               as TIO
import           Haskell.Api.Helpers
import           LN.Api
import           LN.Generate
import           LN.Sanitize
import           LN.T
import           LN.T.Error                 (ApplicationError (..),
                                             ValidationError (..),
                                             ValidationErrorCode (..))
import           LN.Validate
import           Prelude                    hiding (break)
import           Rainbow
import           System.Exit                (exitFailure)
import           Test.QuickCheck
import           Test.QuickCheck.Utf8



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
