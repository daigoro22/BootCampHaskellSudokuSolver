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

data SudokuBox = SudokuBox -- 数独の1マスを表すデータ型
  { num         :: Int -- マスの数値
  , realIndex   :: Int -- リスト上のインデックス
  , rowIndex    :: Int -- 行としてのインデックス
  , columnIndex :: Int -- 列としてのインデックス
  , blockIndex  :: Int -- ブロックとしてのインデックス
  }
type SudokuProblem = [SudokuBox] -- 数独問題
type SudokuUnit = [Int] -- 数独の行, 列, ブロックを表す

-- 1行1問の形式で読み込んだ数独問題を SudokuProblem のリストとして返す関数
extractProblemsFromString :: [String] -> [SudokuProblem]
extractProblemsFromString lines = map (stringToProblem) lines

-- String で表した数独問題を SudokuProblem に変換する関数
stringToProblem :: String -> SudokuProblem
stringToProblem row
  | length row == 81 = map numToSudoku (indexed $ map zenNumToInt row)
  | -- 81マスの数独として読み込めるもののみ、マスの文字列 -> 数値（Int） の変換とマスの数字（Int） ->SudokuBoxへの変換を行う 
    otherwise        = []

-- 全角数字を Int 型に変換する関数
zenNumToInt :: Char -> Int
zenNumToInt c | c == '　' = 0
              | -- 全角はゼロに変換
                and [ord c >= ord '０', ord c <= ord '９'] = ord (c) - ord ('０') -- 数字に変換

-- インデックスとマスの数値のタプルを SudokuBox に変換する関数
numToSudoku :: (Int, Int) -> SudokuBox
numToSudoku (realIndex, num) | and [num >= 0, num <= 9] = SudokuBox -- 数値が 0~9 の範囲の場合のみ
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

-- SudokuProblem を、SudokuBox のあるフィールドの値ごとにまとめた形に変換
convertToUnits :: SudokuProblem -> (SudokuBox -> Int) -> [SudokuUnit]
convertToUnits sp f = [ toUnit i | i <- [0 .. 8] ]
 where
  tuples = map (\x -> (f x, num x)) sp -- SudokuProblem を (SudokuBox の指定の要素,マスの数値)のタプルに変換
  toUnit i = [ snd x | x <- tuples, fst x == i ] -- マスの数値を SudokuBox のあるフィールドの値ごとにまとめる関数

-- 全てのマスが埋められているかどうか判定する関数
isAllFilled :: SudokuProblem -> Bool
isAllFilled sp = all (\x -> num x /= 0) sp

-- 指定した1マスを埋めた状態の SudokuProblem を返す関数
fillBox :: Int -> SudokuBox -> SudokuProblem -> SudokuProblem
fillBox i sb sp = toList $ Sequence.update i sb (Sequence.fromList sp)
