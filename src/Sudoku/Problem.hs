module Sudoku.Problem
    (
        extractProblemsFromString,
        -- SudokuBox,
        SudokuBox(..),
        SudokuProblem
    ) where

import System.IO
import Data.Char
import Data.List.Index

data SudokuBox = SudokuBox {num::Int, realIndex::Int, rowIndex::Int, columnIndex::Int, blockIndex::Int}
type SudokuProblem = [SudokuBox]

extractProblemsFromString::[String] -> [SudokuProblem]
extractProblemsFromString lines = map(convertRow) lines

convertRow::String -> SudokuProblem
convertRow row
    | length row == 81 = map numToSudoku (indexed $ map zenNumToInt row) -- 81マスの数独として読み込めるもののみ
    | otherwise = []

zenNumToInt::Char -> Int
zenNumToInt c
    | c == '　' = 0 -- 全角はゼロに変換
    | and [ord c >= ord '０', ord c <= ord '９'] = ord(c) - ord('０') -- 数字に変換

numToSudoku::(Int ,Int) -> SudokuBox
numToSudoku (realIndex,num)
    | and [num>=0,num<=9] = SudokuBox num realIndex rowIndex columnIndex blockIndex
    where
        rowIndex = truncate $ fromIntegral realIndex / 9 -- 9マスごとの列のインデックス
        columnIndex = realIndex `mod` 9 -- 9マスごとの行のインデックス
        blockIndex = (truncate (fromIntegral rowIndex/3)) * 3 + truncate (fromIntegral columnIndex/3) -- 9マスごとのブロックのインデックス