import Data.List (sort)
import Data.List.Split (splitOn)

findMedian :: [Int] -> Int
findMedian ls = sort ls !! (length ls `div` 2)

findMean :: (Fractional a) => [Int] -> a
findMean ls = (fromIntegral . sum) ls / (fromIntegral . length) ls

nthTriangleNumber :: Int -> Int
nthTriangleNumber n = n * (n + 1) `div` 2

main :: IO ()
main = do
  input <- map read . splitOn "," <$> readFile "input" :: IO [Int]
  -- part 1
  let median = findMedian input
  print $ sum [abs (x - median) | x <- input]
  -- part 2
  let mean = truncate (findMean input) -- round down
  print $ sum [nthTriangleNumber $ abs (x - mean) | x <- input]
