import Data.List (sort)

data LineScore = Incomplete Int | Corrupt Int

lineScore :: String -> String -> LineScore
lineScore [] stack = Incomplete $ foldl (\c x -> (5 * c) + completionPoint x) 0 stack
  where
    completionPoint ')' = 1
    completionPoint ']' = 2
    completionPoint '}' = 3
    completionPoint '>' = 4
lineScore (i : input_rest) []
  | i == '(' = lineScore input_rest [')']
  | i == '[' = lineScore input_rest [']']
  | i == '{' = lineScore input_rest ['}']
  | i == '<' = lineScore input_rest ['>']
  | otherwise = undefined
lineScore (i : input_rest) stack@(expect_c : expect_rest)
  | i == expect_c = lineScore input_rest expect_rest
  | i == ')' = Corrupt 3
  | i == ']' = Corrupt 57
  | i == '}' = Corrupt 1197
  | i == '>' = Corrupt 25137
  | i == '(' = lineScore input_rest $ ')' : stack
  | i == '[' = lineScore input_rest $ ']' : stack
  | i == '{' = lineScore input_rest $ '}' : stack
  | i == '<' = lineScore input_rest $ '>' : stack

main :: IO ()
main = do
  input <- getInput "input"
  print . sum $
    map
      ( \l -> case lineScore l [] of
          Corrupt s -> s
          Incomplete _ -> 0
      )
      input
  print . (\xs -> xs !! (length xs `div` 2)) . dropWhile (== 0) . sort $
    map
      ( \l -> case lineScore l [] of
          Corrupt _ -> 0
          Incomplete s -> s
      )
      input

getInput :: FilePath -> IO [String]
getInput file = lines <$> readFile file
