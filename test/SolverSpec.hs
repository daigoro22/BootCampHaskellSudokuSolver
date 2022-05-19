module SolverSpec
  ( spec
  ) where

import           Control.Exception              ( bracket
                                                , evaluate
                                                )
import           Sudoku.Problem
import           Sudoku.Solver
import           System.IO
import           Test.Hspec
import           Test.QuickCheck

data ValidationResult = Valid | InValid | NotFilled deriving (Eq, Show,Enum)

fileOpen :: (Handle -> IO ()) -> IO ()
fileOpen action = do
  bracket (openFile "resources/problem/sudoku_test.txt" ReadMode) hClose action

spec :: Spec
spec = do
  around fileOpen $ do
    describe "validateSolvedProblem" $ do
      it "validates solved sudoku problems" $ \handle -> do
        text <- hGetContents handle
        (allValid results text) `shouldBe` True
 where
  problems t = extractProblemsFromString $ lines $ t
  solved t = map (\p -> exhausiveSearch p $ head p) $ problems t
  results t = map (validateAll) $ solved t
  allValid r t = (all (== Valid) $ r t)

validateUnit :: SudokuUnit -> ValidationResult
validateUnit su | elem 0 su    = NotFilled
                | sum su == 45 = Valid
                | otherwise    = InValid

validateAll :: SudokuProblem -> ValidationResult
validateAll sp | elem InValid results   = InValid
               | elem NotFilled results = NotFilled
               | otherwise              = Valid
 where
  units   = convertToUnits sp -- 行, 列, ブロックに変換前の部分適用
  results = map validateUnit $ rows ++ cols ++ blks
  rows    = units rowIndex
  cols    = units columnIndex
  blks    = units blockIndex
