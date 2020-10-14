{-# LANGUAGE QuasiQuotes                      #-}

module Main where

import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import System.Console.ANSI
import Control.Monad (when, forM_)
import Data.List (intercalate)
import Text.RE.TDFA.String

import Lib

rows :: [RowData]
rows = [
    Row 1 "one" "smth-one" 101,
    Row 2 "two" "smth-two" 102,
    Row 3 "tree" "smth-tree" 103,

    Row 4 "four" "smth-four" 104,
    Row 5 "five" "smth-five" 105,
    Row 6 "six" "smth-six" 106,

    Row 7 "seven" "smth-seven" 107,
    Row 8 "eight" "smth-eight" 108,
    Row 9 "nine" "smth-nine" 109
  ]

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  mainLoop 0 3

loop :: IO ()
loop = do
  putStr ">"
  line <- getLine
  case line of
    "q" -> do
      putStrLn "Bye"
      return ()
    "p" -> do
      forM_ rows (putStrLn . show)
      loop
    _ -> do
      let
        m :: String
        (_, m, _) = line =~ [re|p ([0-9]+)|] :: (String, String, String)
        numstr = drop 2 m
        num :: Int
        num = read numstr
        nRows = take num rows
      forM_ nRows (putStrLn . show)
      loop

mainLoop :: Int -> Int -> IO ()
mainLoop rowOffset rowsPerPage = do
  printTable rowOffset rowsPerPage
  key <- getKey
  when (key /= "\ESC") $ do
    case key of
      "\ESC[A" -> do -- up
        let
          rowsCount = length rows
          newRowsPerPage = if rowsPerPage >= rowsCount then rowsCount else rowsPerPage + 1
        mainLoop rowOffset newRowsPerPage
      "\ESC[B" -> do -- down
        let
          newRowsPerPage = if rowsPerPage <= 1 then 1 else rowsPerPage - 1
        mainLoop rowOffset newRowsPerPage
      "\ESC[C" -> do -- right
        let
          rowsCount = length rows
          offset = if rowOffset >= rowsCount - 1 then rowsCount - 1 else rowOffset + 1
        mainLoop offset rowsPerPage
      "\ESC[D" -> do -- left
        let offset = if rowOffset <= 0 then 0 else rowOffset - 1
        mainLoop offset rowsPerPage
      --"\n" -> do -- enter
      "q" -> return ()
      _ -> return ()

printTable :: Int -> Int -> IO ()
printTable rowOffset rowsPerPage = do
  saveCursor
  clearScreen
  let
    rowsToPrint = take rowsPerPage (drop rowOffset rows)
    columnWidth = 10
    columnCount = 4
    topStr         = "┌" ++ (intercalate "┬" (replicate columnCount (replicate columnWidth '─'))) ++ "┐"
    rowBoxStr      = "│" ++ (intercalate "│" (replicate columnCount (replicate columnWidth ' '))) ++ "│"
    betweenRowsStr = "├" ++ (intercalate "┼" (replicate columnCount (replicate columnWidth '─'))) ++ "┤"
    bottomStr      = "└" ++ (intercalate "┴" (replicate columnCount (replicate columnWidth '─'))) ++ "┘"
    x0 = 0
    y0 = 0
    printRowBox rowIndex = do
      if rowIndex == 0
        then do
          setCursorPosition (y0+1) x0
          putStr rowBoxStr
        else do
          setCursorPosition (y0+rowIndex*2) x0
          putStr betweenRowsStr
          setCursorPosition (y0+rowIndex*2+1) x0
          putStr rowBoxStr
    printRows = do
      forM_ (zip rowsToPrint [1..]) $ \(row, rowIndex) -> do
        let
          a = show . getA $ row
          b = getB row
          c = getC row
          d = show . getD $ row
        printRowValues (a:b:c:d:[]) rowIndex
    printRowValues (a : b : c : d : _) rowIndex = do
      let
        yPos = y0+1+rowIndex*2
        x1 = x0+1
        x2 = x0+1+columnWidth+1
        x3 = x0+1+(columnWidth+1)*2
        x4 = x0+1+(columnWidth+1)*3
      setCursorPosition yPos x1
      putStr a
      setCursorPosition yPos x2
      putStr b
      setCursorPosition yPos x3
      putStr c
      setCursorPosition yPos x4
      putStr d
    printHeader = printRowValues ("a":"b":"c":"d":[]) 0
  setCursorPosition y0 x0
  putStr topStr
  forM_ [0 .. (length rowsToPrint)] printRowBox
  setCursorPosition (y0+((length rowsToPrint) + 1)*2) x0
  putStr bottomStr
  printHeader
  printRows
  restoreCursor
  --setCursorPosition (y0+(length rowsToPrint)*2+3) x0
  --forM_ rowsToPrint (putStrLn . show)

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
