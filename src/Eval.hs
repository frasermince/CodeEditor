{-# LANGUAGE OverloadedStrings #-}
module Eval
    ( evalPipe
    , evalPipeWithState
    ) where
import Control.Monad (forever)
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Control.Monad.State.Strict (StateT, get, modify)
import Pipes (Pipe, yield, await)
import Pipes.Lift (evalStateP)
import Types (Command(..))
import Data.Sequence as S

evalPipe :: Monad m => Pipe Command ByteString m ()
evalPipe = evalStateP [""] evalPipeWithState

evalPipeWithState :: Monad m => Pipe Command ByteString (StateT [(S.Seq Char)] m) ()
evalPipeWithState = forever $ do command <- await
                                 applyCommand command
  where applyCommand :: Monad m => Command -> Pipe Command ByteString (StateT [(S.Seq Char)] m) ()
        applyCommand (Append str) = modify $ \(x : xs) -> (x S.>< str) : x : xs
        applyCommand (Delete num) = modify $ \(x : xs) -> S.take ((S.length x) - num) x : x : xs
        applyCommand Undo         = modify $ \(x : xs) -> xs
        applyCommand (Print num)  = do x : xs <- get
                                       yield $ BC.singleton $ S.index x (num - 1)

