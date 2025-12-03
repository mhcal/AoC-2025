module Main where

type State = (Int, Int)

main :: IO ()
main = do
  rots <- map parse . lines <$> readFile "input"
  print $ solve1 rots
  print $ solve2 rots

parse :: String -> Int
parse ('L':rots) = negate $ read rots
parse ('R':rots) = read rots
parse _ = 0

step :: State -> Int -> State
step (acc, pos) rot =
  let pos' = pos + rot
      cs | rot >= 0 = (pos' `div` 100) - (pos `div` 100)
         | otherwise = ((pos - 1) `div` 100) - ((pos' - 1) `div` 100)
  in (acc + cs, pos' `mod` 100)

solve1 :: [Int] -> Int
solve1 = length . filter ((== 0) . snd) . scanl step (0, 50)

solve2 :: [Int] -> Int
solve2 = fst . foldl step (0, 50)
