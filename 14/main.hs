import Data.List.Split (splitOn)
import qualified Data.Map as M

getStart :: FilePath -> IO String
getStart file = init <$> readFile file -- remove newline

getRules :: FilePath -> IO (M.Map String String)
getRules file = M.fromList . map ((\(a : b : _) -> (a, b)) . splitOn " -> ") . lines <$> readFile file

step :: (M.Map String Int, M.Map Char Int) -> M.Map String String -> (M.Map String Int, M.Map Char Int)
step (pairs, chars) rules =
  ( M.foldrWithKey
      ( \(a : b : _) c m -> case M.lookup [a, b] rules of
          Nothing -> m
          Just new ->
            let m' = M.alter (`decreaseCountBy` c) [a, b] m
                m'' = M.alter (`increaseCountBy` c) (a : new) m'
                m''' = M.alter (`increaseCountBy` c) (new ++ [b]) m''
             in m'''
      )
      pairs
      pairs,
    M.foldrWithKey
      ( \k c m -> case M.lookup k rules of
          Nothing -> m
          Just n -> M.alter (`increaseCountBy` c) (head n) m
      )
      chars
      pairs
  )

increaseCountBy, decreaseCountBy :: Maybe Int -> Int -> Maybe Int
increaseCountBy Nothing c = Just c
increaseCountBy (Just a) c = Just $ a + c
decreaseCountBy Nothing c = Nothing
decreaseCountBy (Just a) c
  | a - c <= 0 = Nothing
  | otherwise = Just $ a - c

makeMaps :: String -> (M.Map String Int, M.Map Char Int)
makeMaps str =
  ( foldr (M.alter (`increaseCountBy` 1)) M.empty (pairify str),
    foldr (M.alter (`increaseCountBy` 1)) M.empty str
  )

pairify :: String -> [String]
pairify [] = []
pairify [x] = []
pairify (a : b : rest) = [a, b] : pairify (b : rest)

main :: IO ()
main = do
  start <- getStart "input_start"
  rules <- getRules "input_rules"
  let (startPairs, startChars) = makeMaps start
  let part1 = M.elems . snd . last . take 11 . iterate (`step` rules) $ (startPairs, startChars)
  print $ maximum part1 - minimum part1
  let part2 = M.elems . snd . last . take 41 . iterate (`step` rules) $ (startPairs, startChars)
  print $ maximum part2 - minimum part2
