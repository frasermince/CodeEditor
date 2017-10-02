{-# LANGUAGE OverloadedStrings #-}
module LibSpec (spec) where
import Test.Hspec
import Eval (evalPipeWithState)
import Parser (parseCommand)
import Types (Command(..))
import Data.ByteString (ByteString)
import Pipes (Producer, for, each, yield, (>->))
import Pipes.Prelude (toListM)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State.Strict (runStateT, StateT)
import Data.Either (isLeft)
import Data.Sequence (Seq)

type TestMonadStack = StateT [Seq Char] Identity

-- Unwrap the monad inputint a starting state of [""]
unwrap :: TestMonadStack a -> (a, [Seq Char])
unwrap stack = runIdentity $ runStateT stack [""]

getState = head . snd . unwrap
getValue = head . fst . unwrap

-- A producer for inputing test values
testProducer :: [Command] -> Producer Command TestMonadStack ()
testProducer list = for (each list) yield

-- Changes a pipe into a list of values
buildListM :: [Command] -> TestMonadStack [ByteString]
buildListM commands = toListM $ ((testProducer commands) >-> evalPipeWithState)

spec :: Spec
spec = do
  describe "applyCommand Append" $ do
    describe "with Append" $
      it "appends the string to the input" $ do
        let commands = [Append "Hello World"]
        (getState $ buildListM commands) `shouldBe` "Hello World"
    describe "with Delete" $
      it "deletes the last N characters where N is 3" $ do
        let commands = [Append "Helloooo", Delete 3]
        (getState $ buildListM commands) `shouldBe` "Hello"
    describe "with Print" $
      it "Adds an element to the print array with the correct character" $ do
        let commands = [Append "Hi There", Print 4]
        (getValue $ buildListM commands) `shouldBe` "T"
    describe "with Undo" $
      it "Undoes the last command" $ do
        let commands = [Append "", Append "abc", Append "a", Undo]
        (getState $ buildListM commands) `shouldBe` "abc"

  describe "parseCommand" $ do
    it "parses an append" $
      (parseCommand "1 abc") `shouldBe` (Right $ Append "abc")
    it "parses a delete" $
      (parseCommand "2 3") `shouldBe` (Right $ Delete 3)
    it "parses a print" $
      (parseCommand "3 4") `shouldBe` (Right $ Print 4)
    it "parses an undo" $
      (parseCommand "4") `shouldBe` (Right $ Undo)
    it "parses anything else" $
      isLeft (parseCommand "5") `shouldBe` True
