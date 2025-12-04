module Main where

import Data.Char (digitToInt)

main :: IO ()
main = do
  banks <- map parse . lines <$> readFile "input"
  print $ solve 2 banks
  print $ solve 12 banks

parse :: String -> [Int]
parse = map digitToInt

joltage :: Int -> [Int] -> [Int]
joltage 0 _ = []
joltage n xs =
  let i = length xs - n
      m = maximum $ take (i + 1) xs
      (_:rest) = dropWhile (/= m) xs
  in m : joltage (n - 1) rest

solve :: Int -> [[Int]] -> Int
solve n = sum . map (read . concatMap show . joltage n)
