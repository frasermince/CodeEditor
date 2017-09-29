{-# LANGUAGE OverloadedStrings #-}
module LibSpec (spec) where
import Test.Hspec
import Control.Monad.State.Lazy (get, put)
import Data.Text (Text)
import Lib

currentText :: Editor -> Text
currentText editor = head $ runExceptT $ editor << get
previousEditor :: Text -> Editor
previousEditor text = put [text]
-- lastPrint = last . printStack

spec :: Spec
spec = do
  describe "applyCommand Append" $ do
    describe "with Append" $
      it "appends the string to the input" $
        currentText ((previousEditor "") >> applyCommand (Append "Hello World")) `shouldBe` "Hello World"
    describe "with Delete" $
      it "deletes the last N characters where N is 3" $
        currentText (previousEditor "Helloooo" >> applyCommand  (Delete 3)) `shouldBe` "Hello"
    -- describe "with Print" $
    --   it "Adds an element to the print array with the correct character" $
    --     lastPrint (applyCommand (createResponse "Hi There") (Print 4)) `shouldBe` "T"
    describe "with Undo" $
      it "Undoes the last command" $ do
        let previousCommand = (previousEditor "") >> (applyCommand $ Append "abc") >> (applyCommand $ Delete 2)
        currentText (previousCommand >> applyCommand (Undo)) `shouldBe` "abc"

  describe "parseCommand" $ do
    it "parses an append" $
      (parseCommand "1 abc") `shouldBe` (Right $ Append "abc")
    it "parses a delete" $
      (parseCommand "2 3") `shouldBe` (Right $ Delete 3)
    it "parses a print" $
      (parseCommand "3 4") `shouldBe` (Right $ Print 4)
    it "parses an undo" $
      (parseCommand "4") `shouldBe` (Right $ Undo)
