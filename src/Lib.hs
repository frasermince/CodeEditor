{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( parsePipe
    , evalPipe
    , evalPipeWithState
    , parseCommand
    , ExecMonadStack
    , Command(..)
    ) where
import Control.Monad (forever)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Text (Text, index, append, dropEnd, singleton, pack, unpack)
import Text.Parsec ((<|>), parse, ParseError, string, many, many1, digit, eof, oneOf)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (anyChar)
import Control.Applicative ((<$>), (<$))
import Control.Monad (void)
import Control.Monad.State.Strict (StateT, get, put, runStateT, modify)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Pipes (Pipe, yield, await)
import Pipes.Lift (evalStateP)

type ExecMonadStack = ExceptT ParseError IO

data Command = Append Text | Delete Int | Print Int | Undo
               deriving (Eq)
instance Show Command where
  show (Append t) = "Append: " ++ (unpack t)
  show (Delete i) = "Delete: " ++ show i
  show (Print i) = "Print: " ++ show i
  show Undo = "Undo"

parsePipe :: Monad m => Pipe Text Command (ExceptT ParseError m) ()
parsePipe = forever $ do
  command <- await
  c <- either throwError return (parseCommand command)
  yield c

evalPipe :: Monad m => Pipe Command Text m ()
evalPipe = evalStateP [""] evalPipeWithState

evalPipeWithState :: Monad m => Pipe Command Text (StateT [Text] m) ()
evalPipeWithState = forever $ do command <- await
                                 applyCommand command
  where applyCommand :: Monad m => Command -> Pipe Command Text (StateT [Text] m) ()
        applyCommand (Append str) = modify $ \(x : xs) -> append x str : x : xs
        applyCommand (Delete num) = modify $ \(x : xs) -> dropEnd num x : x : xs
        applyCommand Undo         = modify $ \(x : xs) -> xs
        applyCommand (Print num)  = do x : xs <- get
                                       yield $ singleton $ index x (num - 1)

parseCommand :: Text -> Either ParseError Command
parseCommand string = parseWithWhitespace parser string

parseWithWhitespace :: Parser a -> Text -> Either ParseError a
parseWithWhitespace p = parseWithEof (whitespace >> p)

parseWithEof :: Parser a -> Text -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parser = appendParser <|> deleteParser <|> printParser <|> undoParser

appendParser :: Parser Command
appendParser = (Append . pack) <$> (string "1 " *> many anyChar)

deleteParser :: Parser Command
deleteParser = (Delete . read) <$> (string "2 " *> many1 digit)

printParser :: Parser Command
printParser = (Print . read) <$> (string "3 " *> many1 digit)

undoParser :: Parser Command
undoParser = Undo <$ string "4"

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"
