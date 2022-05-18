module Sudoku.Solver
  ( exhausiveSearch
--  , getAllCandidates
  , getCandidates
  ) where

import           Data.Char
import           Data.List.Index
import qualified Data.Set                      as Set
import           Sudoku.Problem
import           System.IO

data ValidationResult = Valid | InValid | NotFilled deriving (Eq, Show,Enum)
type Candidates = Set.Set Int

-- exhausiveSearch::SudokuProblem->SudokuAnswer
exhausiveSearch :: SudokuProblem -> [SudokuUnit]
exhausiveSearch sp = convertToUnits sp blockIndex

getCandidates :: SudokuProblem -> SudokuBox -> Candidates
getCandidates sp sb
  | num sb /= 0 = Set.empty
  | otherwise   = Set.difference (Set.fromList [0 .. 9]) allFilled
 where
  rowFilled = convertToUnits sp rowIndex !! (rowIndex sb)
  colFilled = convertToUnits sp columnIndex !! (columnIndex sb)
  blkFilled = convertToUnits sp blockIndex !! (blockIndex sb)
  allFilled = Set.fromList (rowFilled ++ colFilled ++ blkFilled)
