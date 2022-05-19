module Sudoku.Solver
  ( exhausiveSearch
--  , getAllCandidates
  , getCandidates
  ) where

import           Data.Char
import           Data.Foldable                  ( toList )
import           Data.List.Index
import qualified Data.Set                      as Set
import           Sudoku.Problem
import           System.IO

data ValidationResult = Valid | InValid | NotFilled deriving (Eq, Show,Enum)
type Candidates = Set.Set Int

-- 数独問題を全探索する関数
exhausiveSearch :: SudokuProblem -> SudokuBox -> SudokuProblem
exhausiveSearch sp sb
  | realIndex sb == 80 = fillSp
  $ if candidates /= [] then head candidates else num sb
  | -- 探索完了条件
    num sb /= 0 = exhausiveSearch sp nextBox
  | -- 決定済みのマス目の場合、次のマスを探索
    candidates == [] = []
  | -- 探索打ち切り条件
    otherwise = concat [ exhausiveSearch (fillSp n) nextBox | n <- candidates ] -- 探索が打ち切られた場合の返り値は空のリストなので、そのまま結合してしまう
 where
  nextBox    = sp !! ((realIndex sb) + 1) -- 次に探索するマス
  candidates = toList $ getCandidates sp sb -- マスに入力できる数字の候補
  fillSp n = fillBox (realIndex sb) sb { num = n } sp -- 注目しているマスに数字を埋めた状態の SudokuProblem を返す関数

-- マスに入力できる数字の候補を求める関数
getCandidates :: SudokuProblem -> SudokuBox -> Candidates
getCandidates sp sb
  | num sb /= 0 = Set.empty
  | -- すでに埋められているマスの場合、候補は空
    otherwise   = Set.difference (Set.fromList [0 .. 9]) allFilled -- マスに使える候補（[0..9]の数字の集合から使えない数字を除外して求める）
 where
  units     = convertToUnits sp -- 行, 列, ブロックに変換前の部分適用
  rowFilled = units rowIndex !! (rowIndex sb) -- 注目しているマスの行要素
  colFilled = units columnIndex !! (columnIndex sb) -- 注目しているマスの列要素
  blkFilled = units blockIndex !! (blockIndex sb) -- 注目しているマスのブロック要素
  allFilled = Set.fromList (rowFilled ++ colFilled ++ blkFilled) -- 使えない（行, 列, ブロックですでに埋められている）要素の一覧
