import Data.List.Split (splitOn)

count :: (Eq a) => a -> [a] -> Integer
count x = foldr (\y d -> if x == y then d + 1 else d) 0

breedfish :: [Integer] -> [Integer]
breedfish (zero : one : two : three : four : five : six : seven : eight : _) =
  [one, two, three, four, five, six, zero + seven, eight, zero]

main :: IO ()
main = do
  input <- map read . splitOn "," <$> readFile "input" :: IO [Integer]
  let input' = [count x input | x <- [0 .. 8]]
  print . sum $ iterate breedfish input' !! 80
  print . sum $ iterate breedfish input' !! 256
