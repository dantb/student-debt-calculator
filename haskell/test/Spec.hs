{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Maybe
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.QuickCheck

import Lib (app)
import Range

main :: IO ()
main = hspec (spec >> range)

spec :: Spec
spec = with (return app) $ do
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
            get "/users" `shouldRespondWith` users


range :: Spec
range = 
    describe "Range.hs should" $ do
        prop "parse valid ranges" $ do
            prop_validRanges

prop_validRanges :: Property
prop_validRanges = forAll genValidRanges (\(sheet, start, end) -> parseRange sheet start end `shouldSatisfy` isJust)

genValidRanges :: Gen (String, Range.Cell, Range.Cell)
genValidRanges = do
  startCol <- elements ['A'..'Z']
  startRow <- arbitrary :: Gen (Positive Int)
  endCol <- elements ['A'..'Z']
  endRow <- arbitrary :: Gen (Positive Int)
  sheet <- arbitrary :: Gen String
  return (sheet, fromJust $ parseCell (getPositive startRow) (show startCol), fromJust $ parseCell (getPositive endRow) (show endCol))
  
--   if isJust $ parseRange sheet (fromJust $ parseCell startRow startCol) (fromJust $ parseCell endRow endCol)
