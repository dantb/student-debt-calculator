module Range 
  (parseRow
  , parseCol
  , parseCol'
  , parseCell
  , parseCell'
  , parseRange
  , singleCellRange
  , incrementCol
  , incrementRow
  , Cell) where

import Data.Char
import Data.Either.Combinators

import Errors

newtype Col = Col String deriving (Eq, Show)
newtype Row = Row Int deriving (Eq, Ord, Show)

instance Ord Col where
  compare (Col a) (Col b) = case compare (length a) (length b) of
    LT -> LT
    EQ -> compare a b
    GT -> GT
  a >= b = case compare a b of
    LT -> False
    EQ -> True
    GT -> True

validCols :: String
validCols = ['A'..'Z']

incrementCol :: Int -> Col -> Col
incrementCol _ (Col "Z") = Col 'Z'
incrementCol i c = Col $ chr (ord c + i)

incrementRow :: Int -> Row -> Row
incrementRow i (Row row) = Row (row + i)

parseCol :: String -> Maybe Col
parseCol [] = Nothing
parseCol [x] = if x `elem` validCols then Just $ Col [x] else Nothing
parseCol (x:xs) = parseCol [x] *> parseCol xs *> Just (Col (x:xs))

parseCol' :: String -> Either String Col
parseCol' [] = Left "Empty string is not a valid column"
parseCol' [x] = if x `elem` validCols then Right $ Col [x] else Left $ "Invalid character for column: " ++ show x
parseCol' (x:xs) = parseCol' [x] *> parseCol' xs *> Right (Col (x:xs))

parseRow :: Int -> Maybe Row
parseRow i
  | i <= 0    = Nothing
  | otherwise = Just $ Row i

data Cell = Cell { row :: Row, col :: Col } deriving Show

parseCell :: Int -> String -> Maybe Cell
parseCell r c = do
  row <- parseRow r
  col <- parseCol c
  return $ Cell row col

parseCell' :: Int -> String -> Either String Cell
parseCell' r c = do
  row <- maybeToRight ("Invalid Row: " ++ show r) (parseRow r)
  col <- maybeToRight ("Invalid Col: " ++ show c) (parseCol c)
  return $ Cell row col

data Range = Range { sheet :: String, start :: Cell, end :: Cell } deriving Show

parseRange :: String -> Cell -> Cell -> Maybe Range
parseRange sheet start end = if (row start > row end) || (col start > col end) then Nothing else Just (Range sheet start end)

singleCellRange :: String -> Cell -> Range
singleCellRange s c = Range s c c
