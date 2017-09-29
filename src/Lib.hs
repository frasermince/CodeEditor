{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( applyCommand
    , createText
    , parseCommand
    , Editor
    , MonadStack
    , Command(..)
    ) where
import Control.Monad.Identity (Identity, runIdentity)
import Data.Text (Text, index, append, dropEnd, singleton, pack, unpack)
import Text.Parsec ((<|>), parse, ParseError, string, many, many1, digit, eof, oneOf)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar)
import Control.Applicative ((<$>), (<$))
import Control.Monad (void)
import Control.Monad.State.Strict (StateT, get, put, runStateT, modify)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Pipes (Producer, for, each, yield)
import Data.Vector (Vector)

type MonadStack m = StateT [Text] m
type ExecMonadStack = MonadStack (ExceptT ParseError IO)
type Editor = Producer Text ExecMonadStack ()

data Command = Append Text | Delete Int | Print Int | Undo
               deriving (Eq)
instance Show Command where
  show (Append t) = "Append: " ++ (unpack t)
  show (Delete i) = "Delete: " ++ show i
  show (Print i) = "Print: " ++ show i
  show Undo = "Undo"

createText :: Vector String -> Producer Text ExecMonadStack ()
createText commands = foldCommands
  where foldCommands :: Producer Text ExecMonadStack ()
        foldCommands = for (each commands) foldString

        foldString :: String -> Producer Text ExecMonadStack ()
        foldString command = do
          c <- either throwError return (parseCommand command)
          applyCommand c

applyCommand :: Monad m => Command -> Producer Text (StateT [Text] m) ()
applyCommand (Append str) = modify $ \(x : xs) -> append x str : x : xs
applyCommand (Delete num) = modify $ \(x : xs) -> dropEnd num x : x : xs
applyCommand Undo         = modify $ \(x : xs) -> xs
applyCommand (Print num)  = do x : xs <- get
                               yield $ singleton $ index x (num - 1)

parseCommand :: String -> Either ParseError Command
parseCommand string = parseWithWhitespace parser string

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof (whitespace >> p)

parseWithEof :: Parser a -> String -> Either ParseError a
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
