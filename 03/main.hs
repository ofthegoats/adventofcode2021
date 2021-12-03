import Data.List (transpose)

binToDen :: String -> Int
binToDen = foldl (\d x -> if x == '1' then (2 * d) + 1 else 2 * d) 0

sumBits :: String -> Int
sumBits [] = 0
sumBits ('1' : xs) = 1 + sumBits xs
sumBits ('0' : xs) = sumBits xs

mostCommon :: [String] -> String
mostCommon input =
  let tlines = transpose input
      nlines = length input
      sumlines = map sumBits tlines
   in map (\x -> if x >= nlines `div` 2 then '1' else '0') sumlines

invert :: String -> String
invert = fmap (\x -> if x == '1' then '0' else '1')

o2RatingFilter, co2RatingFilter :: [String] -> String -> Int -> String
o2RatingFilter [] _ _ = undefined -- error case
o2RatingFilter [x] _ _ = x -- one remains
o2RatingFilter rem com pos = o2RatingFilter rem' (mostCommon rem') (pos + 1)
  where
    rem' = filter (\x -> (x !! pos) == (com !! pos)) rem -- keep only common
co2RatingFilter [] _ _ = undefined -- error case
co2RatingFilter [x] _ _ = x -- one remains
co2RatingFilter rem com pos = co2RatingFilter rem' (mostCommon rem') (pos + 1)
  where
    rem' = filter (\x -> (x !! pos) /= (com !! pos)) rem -- remove all common

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let gamma = mostCommon input
  let epsilon = invert gamma
  putStrLn $ "γ = " ++ (show . binToDen) gamma
  putStrLn $ "ε = " ++ (show . binToDen) epsilon
  print $ binToDen gamma * binToDen epsilon
  putStrLn $ "O2 = " ++ (show . binToDen . o2RatingFilter input gamma) 0
  putStrLn $ "CO2 = " ++ (show . binToDen . co2RatingFilter input epsilon) 0
  print $ binToDen (o2RatingFilter input gamma 0) * binToDen (co2RatingFilter input gamma 0)
