import Data.Char (digitToInt)
import qualified Data.Map as M

type Position = (Int, Int)

data Octopus = NotFlashed Int | Flashed Int deriving (Show, Eq)

getInput :: FilePath -> IO (M.Map Position Octopus)
getInput file =
  M.fromList
    . concat
    . (\l -> zipWith (\i l' -> zipWith (\j n -> ((i, j), NotFlashed n)) [0 .. length l' - 1] l') [0 .. length l - 1] l)
    . map (map digitToInt)
    . lines
    <$> readFile file

count :: (Eq a) => a -> [a] -> Int
count x = foldr (\y c -> if x == y then c + 1 else c) 0

handleFlashes :: M.Map Position Octopus -> M.Map Position Octopus
handleFlashes octos =
  let toFlash =
        M.foldrWithKey
          ( \k o l -> case o of
              Flashed _ -> l
              NotFlashed n -> if n > 9 then k : l else l
          )
          []
          octos
      octos' =
        M.map
          ( \o -> case o of
              Flashed _ -> o
              NotFlashed n -> if n > 9 then Flashed n else o
          )
          octos
   in if null toFlash
        then octos'
        else
          let neighbours =
                [ (i', j')
                  | (i, j) <- toFlash,
                    i' <- [i - 1 .. i + 1],
                    j' <- [j - 1 .. j + 1]
                ]
              octos'' =
                M.mapWithKey
                  ( \k o ->
                      if k `elem` neighbours
                        then case o of
                          Flashed _ -> o
                          NotFlashed n -> NotFlashed $ n + count k neighbours
                        else o
                  )
                  octos'
           in handleFlashes octos''

reset :: M.Map Position Octopus -> (Int, M.Map Position Octopus)
reset octos =
  let count =
        M.foldr
          ( \o c -> case o of
              Flashed _ -> 1 + c
              NotFlashed _ -> c
          )
          0
          octos
   in ( count,
        M.map
          ( \o -> case o of
              Flashed n -> NotFlashed 0
              NotFlashed _ -> o
          )
          octos
      )

step :: (Int, Int, M.Map Position Octopus) -> (Int, Int, M.Map Position Octopus)
step (s, n, os) = ((\(m, os') -> (s + 1, n + m, os')) . reset . handleFlashes . M.map (\(NotFlashed n) -> NotFlashed $ n + 1)) os

main :: IO ()
main = do
  input <- getInput "input"
  print . (\(_, a, _) -> a) . last . take 100 . tail . iterate step $ (0, 0, input)
  print . (\(a, _, _) -> a) . head . filter (\(_, _, m) -> all (== NotFlashed 0) (M.elems m)) . iterate step $ (0, 0, input)
