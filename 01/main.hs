-- for the first part of the challenge
countIncreases :: (Ord a) => [a] -> Int
countIncreases [_] = 0
countIncreases (x : y : rest)
  | y > x = 1 + countIncreases (y : rest)
  | otherwise = countIncreases (y : rest)

-- using a three-measurement window, for the second part
countIncreases' :: (Num a, Ord a) => [a] -> Int
countIncreases' [_, _, _] = 0
countIncreases' (a : b : c : d : rest)
  | a + b + c < b + c + d = 1 + countIncreases' (b : c : d : rest)
  | otherwise = countIncreases' (b : c : d : rest)

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input" :: IO [Int]
  print $ countIncreases input
  print $ countIncreases' input
