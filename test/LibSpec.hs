{-# LANGUAGE OverloadedStrings #-}
module LibSpec (spec) where
import Test.Hspec
import Control.Monad.State.Lazy (get, put)
import Data.Text (Text)
import Lib
import Pipes (runEffect)
import Pipes.Prelude (toListM)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State.Strict (runStateT)
import Data.Either (isLeft)

type TestMonadStack = MonadStack Identity
editor :: [Text] -> TestMonadStack ()
editor text = put text
unwrap :: TestMonadStack a -> (a, [Text])
unwrap stack = runIdentity $ runStateT stack [""]

getState = head . snd . unwrap
getValue = head . fst . unwrap

spec :: Spec
spec = do
  describe "applyCommand Append" $ do
    describe "with Append" $
      it "appends the string to the input" $ do
        let applied = toListM $ applyCommand $ Append "Hello World"
        getState ((editor [""]) >> applied) `shouldBe` "Hello World"
    describe "with Delete" $
      it "deletes the last N characters where N is 3" $ do
        let applied = toListM $ applyCommand $ Delete 3
        getState (editor ["Helloooo"] >> applied) `shouldBe` "Hello"
    describe "with Print" $
      it "Adds an element to the print array with the correct character" $ do
        let applied = toListM $ applyCommand $ Print 4
        getValue (editor ["Hi There"] >> applied) `shouldBe` "T"
    describe "with Undo" $
      it "Undoes the last command" $ do
        let previousCommand = (editor ["", "abc", "a"])
        let applied = toListM $ applyCommand Undo
        getState (previousCommand >> applied) `shouldBe` "abc"

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
