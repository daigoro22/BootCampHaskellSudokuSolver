module Lib
  ( someFunc
  ) where

import           Control.Exception
import           Numeric
import           Sudoku.Problem
import           Sudoku.Solver
import           System.IO

someFunc :: IO ()
someFunc = do
  bracket
    (openFile "resources/problem/sudoku.txt" ReadMode)
    hClose
    (\h -> do
      text <- hGetContents h
      mapM_ showProblemAndAnswer $ extractProblemsFromString $ lines $ text
    )

showProblemAndAnswer :: SudokuProblem -> IO ()
-- 問題
showProblemAndAnswer sp = do
  mapM_
      (\x -> do
        print $ show x
      )
    $ (convertToUnits sp rowIndex)
  putStrLn ""
-- 答え
  mapM_
      (\x -> do
        print $ show x
      )
    $ (convertToUnits (exhausiveSearch sp (head sp)) rowIndex)
  putStrLn "---------------------"
