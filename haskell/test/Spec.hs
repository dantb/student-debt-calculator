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
        prop "parse single cell range" $ do
            forAll genSingleCell (\(sheet, cell) -> singleCellRange sheet cell `shouldBe` (fromJust $ parseRange sheet cell cell))

prop_validRanges :: Property
prop_validRanges = forAll genValidRanges (\(sheet, start, end) -> parseRange sheet start end `shouldSatisfy` isJust)

genValidRanges :: Gen (String, Range.Cell, Range.Cell)
genValidRanges = do
  startCol <- elements ['A'..'Z']
  startRow <- arbitrary :: Gen (Positive Int)
  let startCell = fromJust $ parseCell (getPositive startRow) [startCol]
  incrementColBy <- arbitrary :: Gen (Positive Int)
  incrementRowBy <- arbitrary :: Gen (Positive Int)
  let endCol = incrementCol incrementColBy startCol
  let endRow = incrementRow incrementRowBy startRow
  let endCell = fromJust $ parseCell (getPositive endRow) [endCol]
  sheet <- arbitrary :: Gen String
  return (sheet, startCell, endCell)

genSingleCell :: Gen (String, Range.Cell)
genSingleCell = do
  col <- elements ['A'..'Z']
  row <- arbitrary :: Gen (Positive Int)
  let cell = fromJust $ parseCell (getPositive row) [col]
  return (sheet, cell)

-- incrementCol :: Int -> Char -> Char
-- incrementCol _ 'Z' = 'Z'
-- incrementCol i c = chr (ord c + i)

--   if isJust $ parseRange sheet (fromJust $ parseCell startRow startCol) (fromJust $ parseCell endRow endCol)
