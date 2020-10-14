module Lib
    ( someFunc
      , RowData(..)
    ) where

data RowData = Row { getA :: Int, getB :: String, getC :: String, getD :: Int }
  deriving Show

someFunc :: IO ()
someFunc = putStrLn "someFunc"
