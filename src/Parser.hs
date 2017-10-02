{-# LANGUAGE OverloadedStrings #-}
-- |Module for parsing input.
module Parser
  (parsePipe, parseCommand) where
import Pipes (Pipe, yield, await)
import Types (Command(..), EditorParseError)
import Data.ByteString as B
import Control.Applicative ((<$>), (<$))
import Text.Megaparsec ((<|>), parse, many, some, eof, oneOf, Parsec, anyChar, string, digitChar)
import Text.Megaparsec.ByteString (Parser)
import Control.Monad.Except (ExceptT, throwError)
import Data.Sequence as S
import Control.Monad (void)
import Control.Monad (forever)

-- |Pipe used to change the ByteString Input into a Command.
parsePipe :: Monad m => Pipe ByteString Command (ExceptT EditorParseError m) ()
parsePipe = forever $ do
  command <- await
  c <- either throwError return (parseCommand command)
  yield c

-- |Function used to parse the ByteString input.
parseCommand :: B.ByteString -> Either EditorParseError Command
parseCommand string = parseWithWhitespace parser string

parseWithWhitespace :: Parser a -> B.ByteString -> Either EditorParseError a
parseWithWhitespace p = parseWithEof (whitespace >> p)

parseWithEof :: Parser a -> B.ByteString -> Either EditorParseError a
parseWithEof p = parse (p <* eof) ""

parser = appendParser <|> deleteParser <|> printParser <|> undoParser

appendParser :: Parser Command
appendParser = (Append . S.fromList) <$> (string "1 " *> many anyChar)

deleteParser :: Parser Command
deleteParser = (Delete . read) <$> (string "2 " *> some digitChar)

printParser :: Parser Command
printParser = (Print . read) <$> (string "3 " *> some digitChar)

undoParser :: Parser Command
undoParser = Undo <$ string "4"

whitespace :: Parser ()
whitespace = void $ many $ oneOf [' ', '\n', '\t']
