{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Text.Parsec (ParseError)
import Data.Text (Text)
import Data.Text.IO as T
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

        producer  :: Producer Text ExecMonadStack ()
        producer   = P.repeatM (liftIO T.getLine)

        consumer  :: Consumer Text ExecMonadStack ()
        consumer   =  P.mapM_ $ \text -> (liftIO $ T.putStrLn text)

        handleEither :: Either ParseError () -> IO ()
        handleEither (Left e) = Prelude.putStrLn $ "Error: " ++ show e
        handleEither (Right value) = return value

-- main :: IO ()
-- main = defaultMain [
--   bgroup "editor" [ bench "parse"
      
                  -- ]
                  --  ]
