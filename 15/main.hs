import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Graph = M.Map (Int, Int) Int

getInput :: FilePath -> IO Graph
getInput file =
  M.fromList
    . concat
    . (\ls -> zipWith (\x l -> zipWith (\y n -> ((x, y), n)) [0 .. length l - 1] l) [0 .. length ls - 1] ls)
    . map (map digitToInt)
    . lines
    <$> readFile file

extendGraph :: Graph -> Graph
extendGraph = M.foldrWithKey (\(i, j) w g' -> foldr (\(a, b) g'' -> M.insert (i + 100 * a, j + 100 * b) (w + (a + b) `mod` 10) g'') g' [(a, b) | a <- [0 .. 5], b <- [0 .. 5]]) M.empty

maxPoint :: Graph -> (Int, Int)
maxPoint g = last $ M.keys g

dijkstra ::
  Graph -> -- The graph being traversed
  (Int, Int) -> -- The current node
  (Int, Int) -> -- The target node
  Graph -> -- Visited nodes + weights
  Graph -> -- Unvisited nodes, waiting in BFS
  Int -- Final weight
dijkstra g curr@(i, j) target done todo =
  let (mi, mj) = maxPoint g
      currWeight = fromMaybe undefined $ M.lookup curr todo
      done' = M.insert curr currWeight done
      todo' = M.delete curr todo
      neighbours =
        [ (n, currWeight + fromMaybe undefined (M.lookup n g))
          | n@(i', j') <- [(i, j - 1), (i, j + 1), (i - 1, j), (i + 1, j)],
            i' >= 0 && i' <= mi,
            j' >= 0 && j' <= mj,
            n `M.notMember` done'
        ]
   in if null neighbours && M.null todo'
        then fromMaybe undefined (M.lookup target done')
        else
          let todo'' = foldr (\(n, w) m -> M.insertWith (\w' oldw -> if w' < oldw then w' else oldw) n w m) todo' neighbours
              (next, _) = foldr (\(a, b) (c, d) -> if b < d then (a, b) else (c, d)) (head $ M.assocs todo'') (tail $ M.assocs todo'')
           in dijkstra g next target done' todo''

main :: IO ()
main = do
  g <- getInput "input"
  print $ dijkstra g (0, 0) (maxPoint g) M.empty $ M.fromList [((0, 0), 0)]

-- let g' = extendGraph g
-- print $ dijkstra g' (0, 0) (maxPoint g') M.empty $ M.fromList [((0, 0), 0)]
