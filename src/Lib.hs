{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( applyCommand
    , createText
    , parseCommand
    , Editor
    , Command(..)
    ) where
import Control.Monad.Identity (Identity, runIdentity)
import Data.Text (Text, index, append, dropEnd, singleton, pack, unpack)
import Text.Parsec ((<|>), parse, ParseError, string, many, many1, digit)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar)
import Control.Applicative ((<$>), (<$))
import Control.Monad.State.Strict (StateT, get, put, runStateT, modify)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Pipes (Producer, for, each, yield)

-- runEditor e = runExceptT $ runStateT e [""]
-- runEditorForState :: Editor -> [Text]
-- runEditorForState e = state
--   where (_, state) = case runEditor e of
--                        (Left _) -> (return (), [pack ""])
--                        (Right tuple) -> tuple

-- runEditorForValue :: Editor -> IO ()
-- runEditorForValue e = value
--   where (value, _) = case runEditor e of
--                        Left error -> (putStrLn $ "Error: " ++ show error, [])
--                        Right tuple -> tuple

-- type Editor = ExceptT ParseError (StateT [Text] Identity) (IO ())
type Editor = Producer Text (StateT [Text] (ExceptT ParseError IO)) ()

data Command = Append Text | Delete Int | Print Int | Undo
               deriving (Eq)
instance Show Command where
  show (Append t) = "Append: " ++ (unpack t)
  show (Delete i) = "Delete: " ++ show i
  show (Print i) = "Print: " ++ show i
  show Undo = "Undo"

createText :: [String] -> Editor
createText commands = foldCommands
  where foldCommands :: Editor
        foldCommands = for (each commands) foldString

        foldString :: String -> Editor
        foldString command = do
          c <- either throwError return (parseCommand command)
          applyCommand c

applyCommand :: Command -> Editor
applyCommand (Append str) = modify $ \(x : xs) -> append x str : x : xs
applyCommand (Delete num) = modify $ \(x : xs) -> (dropEnd num x) : x : xs
applyCommand Undo         = modify $ \(x : xs) -> xs
applyCommand (Print num)  = do x : xs <- get
                               yield $ singleton $ index x (num - 1)

parseCommand :: String -> Either ParseError Command
parseCommand string = parse parser "" string

parser = appendParser <|> deleteParser <|> printParser <|> undoParser

appendParser :: Parser Command
appendParser = (Append . pack) <$> (string "1 " *> many anyChar)

deleteParser :: Parser Command
deleteParser = (Delete . read) <$> (string "2 " *> many1 digit)

printParser :: Parser Command
printParser = (Print . read) <$> (string "3 " *> many1 digit)

undoParser :: Parser Command
undoParser = Undo <$ string "4"
