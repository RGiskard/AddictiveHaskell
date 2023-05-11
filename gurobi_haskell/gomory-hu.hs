--ghc -o gomory-hu gomory-hu.hs
import Data.List (find, delete)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type Edge = (Int, Int, Int)
type Graph = ([Int], [Edge]) -- (Vertices, Arestas)

gomoryHu :: Graph -> [(Int, Int, Int)]
gomoryHu ([v], _) = []
gomoryHu g = let (a, b) = minCut g
                 t1 = gomoryHu a
                 t2 = gomoryHu b
                 r = findRoot a t1
                 edge = (fromJust $ findEdge b r, fromJust $ findEdge a r, snd $ fromJust $ findEdge a b)
             in t1 ++ [edge] ++ t2

minCut :: Graph -> (Graph, Graph)
minCut ([v], _) = error "minCut: empty graph"
minCut g = let (v1, v2) = pickTwo (fst g)
               (a, b) = minCut (contract g v1 v2)
           in if Set.member v1 a then (a, b) else (b, a)

contract :: Graph -> Int -> Int -> Graph
contract ([v], _) _ _ = error "contract: empty graph"
contract (v:e, es) v1 v2 = let v' = length v:e
                               (e1, e2) = splitEdges (delete v1 $ delete v2 v:e) v1 v2
                               newEdges = zip (repeat v') $ e1 ++ e2
                               newVertices = v':delete v2 (delete v1 v)
                           in (newVertices, newEdges)
  where splitEdges es' x y = ([(a, b) | (a, b, w) <- es', a == x || b == x], [(a, b) | (a, b, w) <- es', a == y || b == y])

findRoot :: Graph -> [(Int, Int, Int)] -> Graph
findRoot g [] = g
findRoot g ((x, y, _):es) = let (a, b) = contract g x y
                                t = findRoot a es
                            in if Set.member y $ fst t then findRoot b es else t

findEdge :: Graph -> Graph -> Maybe Edge
findEdge ([v1], _) ([v2], _) = Nothing
findEdge (v1:e1, _) (v2:e2, _) = case [(x, y, w) | (x, y, w) <- e2, Set.member x $ Set.fromList v1, Set.member y $ Set.fromList v1] of
                                [] -> findEdge (v1, e1) (v2, e2)
                                (x, y, w):_ -> Just (x, y, w)

pickTwo :: [a] -> (a, a)
pickTwo xs = let (a:b:_) = xs in (a, b)

