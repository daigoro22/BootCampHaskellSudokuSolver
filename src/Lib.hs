module Lib
  ( someFunc
  ) where

import           Numeric
import           Sudoku.Problem
import           Sudoku.Solver
import           System.IO

someFunc :: IO ()
someFunc = do
  handle <- openFile "resources/sudoku.txt" ReadMode
  text   <- hGetContents handle
  let problems = extractProblemsFromString $ lines text

  mapM_ showProblemAndAnswer problems

  hClose handle

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
