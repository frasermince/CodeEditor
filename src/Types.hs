module Types
  (EditorParseError, MonadStack, Command(..)) where
import Text.Megaparsec (ParseError)
import Text.Megaparsec.Prim (Token)
import Data.ByteString as B
import Control.Monad.Except (ExceptT)
import Data.Sequence as S
import Text.Megaparsec.Error (Dec)

type EditorParseError = ParseError (Token B.ByteString) Dec
type MonadStack = ExceptT EditorParseError IO

data Command = Append (S.Seq Char) | Delete Int | Print Int | Undo
               deriving (Eq)

-- This is mostly just used for testing
instance Show Command where
  show (Append t) = "Append: " ++ show t
  show (Delete i) = "Delete: " ++ show i
  show (Print i) = "Print: " ++ show i
  show Undo = "Undo"

