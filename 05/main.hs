{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type Point = (Int, Int)

type Line = (Point, Point)

parseLine :: String -> Line
parseLine s =
  (\(a : b : _) -> (a, b)) $
    fmap
      (\x -> ((\(a : b : _) -> (a, b)) . fmap read . splitOn ",") x :: Point)
      (splitOn " -> " s)

lineToPoints :: Line -> [Point]
lineToPoints ((sx, sy), (ex, ey))
  | sx == ex = [(sx, y) | y <- [min sy ey .. max sy ey]]
  | sy == ey = [(x, sy) | x <- [min sx ex .. max sx ex]]
  | otherwise -- for the first part: []
    =
    let m = (sy - ey) `div` (sx - ex)
     in if m > 0
          then [(x, (x - min sx ex) * m + min sy ey) | x <- [min sx ex .. max sx ex]]
          else [(x, (x - min sx ex) * m + max sy ey) | x <- [min sx ex .. max sx ex]]

main :: IO ()
main = do
  input <- map parseLine . lines <$> readFile "input" :: IO [Line]
  let points = foldr (\line list -> list ++ lineToPoints line) [] input
  let mpoints = M.fromListWith (+) $ fmap (,1) points :: M.Map Point Int
  print $ M.foldr (\x c -> if x > 1 then c + 1 else c) 0 mpoints
