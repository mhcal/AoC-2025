module Main where

import Data.List.Split (splitOn, chunksOf)

main :: IO ()
main = do
  ranges <- parse <$> readFile "input"
  print $ solve invalid1 ranges
  print $ solve invalid2 ranges

parse :: String -> [(Int, Int)]
parse = map (f . break (== '-')) . splitOn ","
  where f (a, b) = (read a, read $ drop 1 b)

invalid1 :: Int -> Bool
invalid1 n =
  let s = show n
      (a, b) = splitAt (length s `div` 2) s
  in a == b

invalid2 :: Int -> Bool
invalid2 n =
  let s = show n
      repeating l@(x:_) = all (== x) l
  in any repeating [chunksOf len s | len <- [1..(length s `div` 2)]]

solve :: (Int -> Bool) -> [(Int, Int)] -> Int
solve p = sum . map (\(a, b) -> sum . filter p $ [a..b]) 
