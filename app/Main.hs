{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Text.Parsec (ParseError)
import Data.Text (unpack)
import Pipes.Lift (evalStateP)
import qualified Pipes.Prelude as P
import Pipes (runEffect, (>->))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Vector (replicateM)

main :: IO ()
main = do
  n <- getLine
  stringCommands <- replicateM (read n) getLine
  let producer = evalStateP [""] (createText stringCommands)
  let consumerFunction text = liftIO $ putStrLn $ unpack text
  eitherVal <- runExceptT $ runEffect (producer >-> P.mapM_ consumerFunction)
  case eitherVal of
    Left error -> putStrLn $  "Error: " ++ show error
    Right value -> return value


