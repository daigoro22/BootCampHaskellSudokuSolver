module Lib
    ( someFunc
    ) where

import Sudoku.Problem
import System.IO
import Numeric

someFunc :: IO ()
someFunc = do 
    handle <- openFile "resources/sudoku.txt" ReadMode
    text <- hGetContents handle
    let problems = extractProblemsFromString $ lines text

    mapM_ (\p -> do{print $ blockIndex p}) $ head problems

    hClose handle
