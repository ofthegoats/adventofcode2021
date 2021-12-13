import Data.List (transpose)
import Data.List.Split (splitOn)
import qualified Data.Set as S

type Grid = S.Set (Int, Int)

getSquare :: FilePath -> IO Grid
getSquare file = S.fromList . map ((\(a : b : _) -> (read a, read b)) . splitOn ",") . lines <$> readFile file

getInstructions :: FilePath -> IO [(Char, Int)]
getInstructions file = map (((\(a : b : _) -> (head a, read b)) . splitOn "=") . drop 11) . lines <$> readFile file

performFold :: (Char, Int) -> Grid -> Grid
performFold ('x', n) = S.foldr (\(x, y) s -> S.insert (if x <= n then (x, y) else (n - (x - n), y)) s) S.empty
performFold ('y', n) = S.foldr (\(x, y) s -> S.insert (if y <= n then (x, y) else (x, n - (y - n))) s) S.empty

prettyGrid :: Grid -> String
prettyGrid grid =
  unlines
    [ [if (x, y) `S.member` grid then '#' else ' ' | x <- [0 .. w]]
      | y <- [0 .. h]
    ]
  where
    (w, h) = (maximum $ S.map fst grid, maximum $ S.map snd grid)

main :: IO ()
main = do
  positions <- getSquare "input_square"
  instructions <- getInstructions "input_instructions"
  print . length $ performFold (head instructions) positions
  let grid' = foldl (flip performFold) positions instructions
  putStrLn $ prettyGrid grid'
