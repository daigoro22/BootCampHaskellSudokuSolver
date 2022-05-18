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

exhausiveSearch :: SudokuProblem -> SudokuBox -> SudokuProblem
exhausiveSearch sp sb
  | realIndex sb == 80 = fillSp
  $ if candidates /= [] then head candidates else num sb
  | -- 探索完了条件
    num sb /= 0 = exhausiveSearch sp nextBox
  | -- 決定済みのマス目の場合、次のマスを探索
    candidates == [] = []
  | -- 探索打ち切り条件
    otherwise = concat [ exhausiveSearch (fillSp n) nextBox | n <- candidates ] -- 探索した条件の中から、打ち切られていないものを採用 
 where
  nextBox    = sp !! ((realIndex sb) + 1)
  candidates = toList $ getCandidates sp sb
  fillSp n = fillSudokuProblem (realIndex sb) sb { num = n } sp

getCandidates :: SudokuProblem -> SudokuBox -> Candidates
getCandidates sp sb
  | num sb /= 0 = Set.empty
  | otherwise   = Set.difference (Set.fromList [0 .. 9]) allFilled
 where
  rowFilled = convertToUnits sp rowIndex !! (rowIndex sb)
  colFilled = convertToUnits sp columnIndex !! (columnIndex sb)
  blkFilled = convertToUnits sp blockIndex !! (blockIndex sb)
  allFilled = Set.fromList (rowFilled ++ colFilled ++ blkFilled)
