module Sudoku.Problem
  ( extractProblemsFromString
  , convertToUnits
  , isAllFilled
  , fillBox
  , SudokuBox(..)
  , SudokuProblem
  , SudokuUnit
  ) where

import           Data.Char
import           Data.Foldable                  ( toList )
import           Data.List.Index
import qualified Data.Sequence                 as Sequence
import           System.IO

data SudokuBox = SudokuBox
  { num         :: Int
  , realIndex   :: Int
  , rowIndex    :: Int
  , columnIndex :: Int
  , blockIndex  :: Int
  }
type SudokuProblem = [SudokuBox]
type SudokuUnit = [Int]

extractProblemsFromString :: [String] -> [SudokuProblem]
extractProblemsFromString lines = map (stringToProblem) lines

stringToProblem :: String -> SudokuProblem
stringToProblem row
  | length row == 81 = map numToSudoku (indexed $ map zenNumToInt row)
  | -- 81マスの数独として読み込めるもののみ
    otherwise        = []

zenNumToInt :: Char -> Int
zenNumToInt c | c == '　' = 0
              | -- 全角はゼロに変換
                and [ord c >= ord '０', ord c <= ord '９'] = ord (c) - ord ('０') -- 数字に変換

numToSudoku :: (Int, Int) -> SudokuBox
numToSudoku (realIndex, num) | and [num >= 0, num <= 9] = SudokuBox
  num
  realIndex
  rowIndex
  columnIndex
  blockIndex
 where
  rowIndex    = truncate $ fromIntegral realIndex / 9 -- 9マスごとの行のインデックス
  columnIndex = realIndex `mod` 9 -- 9マスごとの列のインデックス
  blockIndex  = (truncate (fromIntegral rowIndex / 3)) * 3 + truncate
    (fromIntegral columnIndex / 3) -- 9マスごとのブロックのインデックス

convertToUnits :: SudokuProblem -> (SudokuBox -> Int) -> [SudokuUnit]
convertToUnits sp f = [ toUnit i | i <- [0 .. 8] ]
 where
  tuples = map (\x -> (f x, num x)) sp
  toUnit i = [ snd x | x <- tuples, fst x == i ]

isAllFilled :: SudokuProblem -> Bool
isAllFilled sp = all (\x -> num x /= 0) sp

fillBox :: Int -> SudokuBox -> SudokuProblem -> SudokuProblem
fillBox i sb sp = toList $ Sequence.update i sb (Sequence.fromList sp)
