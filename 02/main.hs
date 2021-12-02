type Command = (String, Int) -- command, amount

type Position = (Int, Int) -- horiz, depth

type Submarine = (Int, Int, Int) -- horiz, depth, aim

-- foldl impl that forces evaluation for efficiency
foldl' :: (t -> a -> t) -> t -> [a] -> t
foldl' f z [] = z
foldl' f z (x : xs) = let z' = f z x in seq z' $ foldl' f z' xs

-- unsafe parse, assumes correct input
parse :: String -> Command
parse input = let [direction, amount] = words input in (direction, read amount :: Int)

-- movement as defined in the first challenge
move :: Position -> Command -> Position
move (horiz, depth) (dir, s) = case dir of
  "forward" -> (horiz + s, depth)
  "up" -> (horiz, depth - s)
  "down" -> (horiz, depth + s)

-- movement as defined in the second challenge
move' :: Submarine -> Command -> Submarine
move' (horiz, depth, aim) (dir, x) = case dir of
  "forward" -> (horiz + x, depth + (x * aim), aim)
  "up" -> (horiz, depth, aim - x)
  "down" -> (horiz, depth, aim + x)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input" :: IO [Command]
  let (finalH, finalD) = foldl' move (0, 0) input
  print $ finalH * finalD
  let (finalH', finalD', _) = foldl' move' (0, 0, 0) input
  print $ finalH' * finalD'
