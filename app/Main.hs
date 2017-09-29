{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (replicateM)
import Lib
import Text.Parsec (ParseError)
import Data.Text (unpack)
import Pipes.Lift (evalStateP)
import qualified Pipes.Prelude as P
import Pipes (runEffect, (>->))
import Control.Monad.Except (runExceptT)

main :: IO ()
main = do
  n <- getLine
  stringCommands <- replicateM (read n) getLine
  let producer = evalStateP [""] (createText stringCommands)
  eitherVal <- runExceptT $ runEffect (producer >-> P.print)
  case eitherVal of
    Left error -> putStrLn $  "Error: " ++ show error
    Right value -> return value


