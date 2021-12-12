import Data.Char (isUpper)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

type Vertex = String

type Graph = M.Map Vertex [Vertex]

getInput :: FilePath -> IO Graph
getInput file =
  foldr
    ( \(a : b : _) m ->
        let m' = M.insertWith (++) a [b] m
            m'' = M.insertWith (++) b [a] m'
         in m''
    )
    M.empty
    . map (splitOn "-")
    . lines
    <$> readFile file

noDoubles :: (Ord a) => [a] -> Bool
noDoubles xs = length xs == S.size (S.fromList xs)

-- DFS for part 1
countPaths :: Vertex -> [Vertex] -> Graph -> Int
countPaths "end" _ _ = 1
countPaths current visited graph =
  foldr
    ( \v c ->
        if v `notElem` visited
          then c + countPaths v visited' graph
          else c
    )
    0
    connected
  where
    visited' = if any isUpper current then visited else current : visited
    connected = fromMaybe undefined (M.lookup current graph)

-- DFS for part 2
countPaths' :: Vertex -> [Vertex] -> Graph -> Int
countPaths' "end" _ _ = 1
countPaths' current visited graph =
  foldr
    ( \v c ->
        if v `notElem` visited || noDoubles visited' && v /= "start"
          then c + countPaths' v visited' graph
          else c
    )
    0
    connected
  where
    visited' = if any isUpper current then visited else current : visited
    connected = fromMaybe undefined (M.lookup current graph)

main :: IO ()
main = do
  input <- getInput "input"
  print $ countPaths "start" [] input
  print $ countPaths' "start" [] input
