{-# LANGUAGE OverloadedStrings #-}
module Main where

import Eval (evalPipe)
import Parser (parsePipe)
import Types (EditorParseError, MonadStack)
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import qualified Pipes.Prelude as P
import Pipes (runEffect, (>->), Consumer, Producer, Effect)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  s <- Prelude.getLine
  let n = read s
  eitherVal <- runExceptT $ runEffect $ pipeline n
  handleEither eitherVal

-- Main flow of the program
pipeline  :: Int -> Effect MonadStack ()
pipeline n = producer >-> P.take n >-> parsePipe >-> evalPipe >-> consumer

-- Creates a producer that reads each line from IO
producer  :: Producer B.ByteString MonadStack ()
producer   = P.repeatM (liftIO B.getLine)

-- Outputs each line
consumer  :: Consumer (B.ByteString) MonadStack ()
consumer   =  P.mapM_ $ \text -> (liftIO $ BC.putStrLn text)

handleEither :: Either EditorParseError a -> IO ()
handleEither (Left e) = Prelude.putStrLn $ "Error: " ++ show e
handleEither (Right value) = return ()
