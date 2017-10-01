{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Text.Parsec (ParseError)
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import qualified Pipes.Prelude as P
import Pipes (runEffect, (>->), Consumer, Producer, Pipe, Effect)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  s <- Prelude.getLine
  let n = read s
  eitherVal <- runExceptT $ runEffect $ pipeline n
  handleEither eitherVal
  where pipeline  :: Int -> Effect ExecMonadStack ()
        pipeline n = producer >-> P.take n >-> parsePipe >-> evalPipe >-> consumer

        producer  :: Producer B.ByteString ExecMonadStack ()
        producer   = P.repeatM (liftIO B.getLine)

        consumer  :: Consumer B.ByteString ExecMonadStack ()
        consumer   =  P.mapM_ $ \text -> (liftIO $ BC.putStrLn text)

        handleEither :: Either ParseError () -> IO ()
        handleEither (Left e) = Prelude.putStrLn $ "Error: " ++ show e
        handleEither (Right value) = return value

-- main :: IO ()
-- main = defaultMain [
--   bgroup "editor" [ bench "parse"
      
                  -- ]
                  --  ]
