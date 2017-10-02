{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where
import Test.Hspec
import Parser (parseCommand)
import Types (Command(..))
import Data.Either (isLeft)

spec :: Spec
spec = do
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
