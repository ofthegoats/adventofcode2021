{-# LANGUAGE LambdaCase #-}

import Data.List (transpose)
import Data.List.Split (splitOn)
import System.IO (IOMode (ReadMode), hGetContents, hGetLine, openFile)

data BingoSquare = UnMarked Int | Marked Int deriving (Show)

type BingoBoard = [[BingoSquare]]

calculateScore :: BingoBoard -> (Bool, Int)
calculateScore board =
  let winner =
        any
          ( all
              ( \case
                  Marked _ -> True
                  UnMarked _ -> False
              )
          )
          board
          || any
            ( all
                ( \case
                    Marked _ -> True
                    UnMarked _ -> False
                )
            )
            (transpose board)
   in if not winner
        then (False, 0)
        else
          ( True,
            sum
              ( map
                  ( foldr
                      ( \x s -> case x of
                          UnMarked n -> s + n
                          Marked n -> s
                      )
                      0
                  )
                  board
              )
          )

boardScoreOrder :: [Int] -> [BingoBoard] -> [Int]
boardScoreOrder (n : ns) boards =
  let boards' =
        map
          ( map
              ( map
                  ( \case
                      Marked x -> Marked x
                      UnMarked x -> if x == n then Marked x else UnMarked x
                  )
              )
          )
          boards
      scores = map calculateScore boards'
      winners = filter fst scores
   in if not (null winners)
        then n * (snd . head) winners : boardScoreOrder ns (filter (not . fst . calculateScore) boards')
        else boardScoreOrder ns boards'
boardScoreOrder _ _ = []

main :: IO ()
main = do
  h <- openFile "input" ReadMode
  numbers <- map read . splitOn "," <$> hGetLine h :: IO [Int]
  _ <- hGetLine h -- empty line in input file
  boards <- map (map (map (UnMarked . read) . words) . lines) . splitOn "\n\n" <$> hGetContents h :: IO [BingoBoard]
  print . head $ boardScoreOrder numbers boards
  print . last $ boardScoreOrder numbers boards
