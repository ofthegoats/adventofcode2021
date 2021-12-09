import Data.Char (digitToInt)
import Data.List (sort)

buildInput :: FilePath -> IO [[Int]]
buildInput filename = map (map digitToInt) . lines <$> readFile filename :: IO [[Int]]

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

isLeastNeighbour :: [[Int]] -> (Int, Int) -> Bool
isLeastNeighbour ls (i, j) =
  let current = ls !! i !! j
      neighbours =
        [ ls !! i' !! j'
          | i' <- [i - 1, i, i + 1],
            j' <- [j - 1, j, j + 1],
            i' >= 0 && i' < length ls,
            j' >= 0 && j' < length (head ls),
            i' /= i || j' /= j,
            i' /= i - 1 || j' /= j - 1, -- do not consider diagonals
            i' /= i - 1 || j' /= j + 1,
            i' /= i + 1 || j' /= j - 1,
            i' /= i + 1 || j' /= j + 1
        ]
   in all (> current) neighbours

lowPoints :: [[Int]] -> [(Int, Int)]
lowPoints input =
  [ (i, j)
    | i <- [0 .. length input - 1],
      j <- [0 .. length (head input) - 1],
      isLeastNeighbour input (i, j)
  ]

buildBasin :: [(Int, Int)] -> [[Int]] -> [(Int, Int)]
buildBasin basin lake = do
  let newPoints =
        [ (i', j')
          | (i, j) <- basin,
            i' <- [i - 1, i, i + 1],
            j' <- [j - 1, j, j + 1],
            i' >= 0 && i' < length lake,
            j' >= 0 && j' < length (head lake),
            (i', j') `notElem` basin,
            i' /= i - 1 || j' /= j - 1, -- do not consider diagonals
            i' /= i - 1 || j' /= j + 1,
            i' /= i + 1 || j' /= j - 1,
            i' /= i + 1 || j' /= j + 1,
            lake !! i !! j <= lake !! i' !! j' && lake !! i' !! j' /= 9
        ]
  if null newPoints
    then basin
    else buildBasin (basin ++ removeDuplicates newPoints) lake

main :: IO ()
main = do
  input <- buildInput "input"
  let lp = lowPoints input
  print . sum $ map ((+ 1) . (\(i, j) -> input !! i !! j)) lp
  print . product . take 3 . reverse . sort $ map (\x -> length (buildBasin [x] input)) lp
