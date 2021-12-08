import Data.List (sort, (\\))
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, (!))

-- every string must be sorted
makeMap :: [String] -> Map String Int
makeMap ns = do
  let n1 = head $ filter (\x -> length x == 2) ns
      n4 = head $ filter (\x -> length x == 4) ns
      n7 = head $ filter (\x -> length x == 3) ns
      n8 = head $ filter (\x -> length x == 7) ns
      n3 = head $ filter (\x -> length (x \\ n1) == 3) ns
      n2 = head $ filter (\x -> length (n4 \\ x) == 2 && x /= n7 && x /= n1) ns
      n5 = head $ filter (\x -> length (n8 \\ x) == 2 && x /= n2 && x /= n3) ns
      n0 = head $ filter (\x -> length (x \\ n5) == 2 && x /= n2 && x /= n8 && x /= n3) ns
      n6 = head $ filter (\x -> length (n1 \\ x) == 1 && x /= n2 && x /= n5 && x /= n3) ns
      n9 = head $ filter (\x -> length (n0 \\ x) == 1 && x /= n6) ns
   in fromList $ zip [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] [0 .. 9]

solveLine :: ([String], [String]) -> Int
solveLine (input, output) = do
  let input' = map sort input
  let output' = map sort output
  let m = makeMap input'
  foldl (\s n -> n + 10 * s) 0 $ map (m !) output'

main :: IO ()
main = do
  input <- map ((\(a : b : _) -> (a, b)) . map words . splitOn " | ") . lines <$> readFile "input" :: IO [([String], [String])]
  print . sum $ map (length . filter (\x -> length x `elem` [2, 3, 4, 7]) . snd) input
  print . sum $ map solveLine input
