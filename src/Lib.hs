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
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Text.Parsec ((<|>), parse, ParseError, string, many, many1, digit, eof, oneOf)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (anyChar)
import Control.Applicative ((<$>), (<$))
import Control.Monad (void)
import Control.Monad.State.Strict (StateT, get, put, runStateT, modify)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Pipes (Pipe, yield, await)
import Pipes.Lift (evalStateP)

type ExecMonadStack = ExceptT ParseError IO

data Command = Append B.ByteString | Delete Int | Print Int | Undo
               deriving (Eq)
instance Show Command where
  show (Append t) = "Append: " ++ show t
  show (Delete i) = "Delete: " ++ show i
  show (Print i) = "Print: " ++ show i
  show Undo = "Undo"

parsePipe :: Monad m => Pipe B.ByteString Command (ExceptT ParseError m) ()
parsePipe = forever $ do
  command <- await
  c <- either throwError return (parseCommand command)
  yield c

evalPipe :: Monad m => Pipe Command B.ByteString m ()
evalPipe = evalStateP [""] evalPipeWithState

evalPipeWithState :: Monad m => Pipe Command B.ByteString (StateT [B.ByteString] m) ()
evalPipeWithState = forever $ do command <- await
                                 applyCommand command
  where applyCommand :: Monad m => Command -> Pipe Command B.ByteString (StateT [B.ByteString] m) ()
        applyCommand (Append str) = modify $ \(x : xs) -> B.append x str : x : xs
        applyCommand (Delete num) = modify $ \(x : xs) -> B.take ((B.length x) - num) x : x : xs
        applyCommand Undo         = modify $ \(x : xs) -> xs
        applyCommand (Print num)  = do x : xs <- get
                                       yield $ B.singleton $ B.index x (num - 1)

parseCommand :: B.ByteString -> Either ParseError Command
parseCommand string = parseWithWhitespace parser string

parseWithWhitespace :: Parser a -> B.ByteString -> Either ParseError a
parseWithWhitespace p = parseWithEof (whitespace >> p)

parseWithEof :: Parser a -> B.ByteString -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parser = appendParser <|> deleteParser <|> printParser <|> undoParser

appendParser :: Parser Command
appendParser = (Append . BC.pack) <$> (string "1 " *> many anyChar)

deleteParser :: Parser Command
deleteParser = (Delete . read) <$> (string "2 " *> many1 digit)

printParser :: Parser Command
printParser = (Print . read) <$> (string "3 " *> many1 digit)

undoParser :: Parser Command
undoParser = Undo <$ string "4"

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"
