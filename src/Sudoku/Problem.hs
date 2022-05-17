module Sudoku.Problem
    (
        extractProblemsFromString
    ) where

import System.IO
import Data.Char

extractProblemsFromString::[String] -> [[Int]]
extractProblemsFromString lines = map(convertRow) lines

convertRow::String -> [Int]
convertRow row
    | length row == 81 = map zenNumToInt row -- 81マスの数独として読み込めるもののみ
    | otherwise = []

zenNumToInt::Char -> Int
zenNumToInt c
    | c == '　' = 0 -- 全角はゼロに変換
    | and [ord c >= ord '０', ord c <= ord '９'] = ord(c) - ord('０') -- 数字に変換