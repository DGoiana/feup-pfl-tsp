-- import qualified Data.List
-- import qualified Data.Array
-- import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String

type Path = [City]

type Distance = Int

type RoadMap = [(City, City, Distance)]

{- Complexity: O(n) -}
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

{- Complexity: O(n) -}
citiesRec :: RoadMap -> [City]
citiesRec [] = []
citiesRec ((c1, c2, _dist) : xs) = [c1, c2] ++ cities xs

{- Complexity: O(n^2) -}
cities :: RoadMap -> [City]
cities roadMap = removeDuplicates (citiesRec roadMap)

{- Complexity: O(n) -}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] c1 c2 = False
areAdjacent ((x1, x2, _dist) : xs) c1 c2
  | x1 == c1 && x2 == c2 = True
  | x1 == c2 && x2 == c1 = True
  | otherwise = areAdjacent xs c1 c2

{- Complexity: O(n) -}
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] c1 c2 = Nothing
distance ((x1, x2, d) : xs) c1 c2
  | x1 == c1 && x2 == c2 = Just d
  | x1 == c2 && x2 == c1 = Just d
  | otherwise = distance xs c1 c2

{- Complexity: O(1) -}
{- Ignore suggestions, cannot import Data.Maybe -}
intDistance:: RoadMap -> City -> City -> Distance
intDistance rm c1 c2 = case distance rm c1 c2 of
  Nothing -> 100000
  Just d -> d

{- Complexity: O(n) -}
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent [] _ = []
adjacent ((c1, c2, dist) : xs) c
  | c1 == c = (c2, dist) : adjacent xs c
  | c2 == c = (c1, dist) : adjacent xs c
  | otherwise = adjacent xs c

{- Complexity: O(n^2) -}
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance r (c1 : c2 : cs)
  | areAdjacent r c1 c2 = do
      d1 <- distance r c1 c2
      d2 <- pathDistance r (c2 : cs)
      return (d1 + d2)
  | otherwise = Nothing

{- Complexity: O(n^3) -}
rome :: RoadMap -> [City]
rome rm = [c | (c, num) <- tuples, num == maximum (map snd tuples)]
  where
    tuples = [(c, length (adjacent rm c)) | c <- cities rm]

{- Complexity: O(max(V+E,V^2)) -}
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = length (dfs_ rm [] [head (map (\(x, _, _) -> x) rm)]) == length (cities rm)

{- Complexity: O(V+E) -}
dfs_ :: RoadMap -> [City] -> [City] -> [City]
dfs_ rm visited [] = visited
dfs_ rm visited (x : xs)
  | x `elem` visited = dfs_ rm visited xs
  | otherwise = dfs_ rm (x : visited) (map fst (adjacent rm x) ++ xs)

{- Complexity: O(max(V+E,V^2)) -}
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start end = findShortestPaths roadMap (bfs [[start]] [])
  where
    bfs :: [Path] -> [Path] -> [Path] -- first are the paths to explore, second is the result
    bfs [] result = result -- no more paths to explore, just return the result
    bfs (p : ps) result
      | last p == end = bfs ps (p : result) -- if p is the end city, append it to the result and finish with base case
      | otherwise = bfs (ps ++ newPaths) result -- otherwise, call bfs with the remaining paths and new ones
      where
        lastCity = last p
        newPaths = [p ++ [c] | (c, _) <- adjacent roadMap lastCity, c `notElem` p] -- find the adjacent nodes of the last city in the tree and add them to the current path
    findShortestPaths :: RoadMap -> [Path] -> [Path]
    findShortestPaths r paths = [p | (p, d) <- validPaths, Just d == minDistance] -- just filter all the valid paths to be the ones with minimal distance
      where
        distances = map (pathDistance r) paths -- find the distance of all paths
        validPaths = [(p, d) | (p, Just d) <- zip paths distances] -- filter out all the paths that do not have a valid distance, e.g "Nothing"
        minDistance = minimum distances -- find the minimum distance

{- SET -}

type Set = Int

emptySet :: Set
emptySet = 0

isEmpty :: Set -> Bool
isEmpty n = n==0

maxSet :: Set
maxSet = truncate (logBase 2 (fromIntegral (maxBound::Int))) - 1

fullSet :: Set -> Set
fullSet n | (n >= 0) && (n<=maxSet) = 2^(n+1)-2
          | otherwise = -1

addSet :: Int -> Set -> Set
addSet i s = d' * e + m
        where (d,m) = divMod s e
              e = 2^i
              d' = if odd d then d else d+1

delSet:: Int -> Set -> Set
delSet i s = d' * e + m
        where (d,m) = divMod s e
              e = 2^i
              d' = if odd d then d-1 else d

set2List :: Set -> [Int]
set2List s = s21 s 0
    where s21 0 _ =               []
          s21 n i | odd n =        i : s21 (n `div` 2) (i+1)
                 | otherwise =    s21 (n `div` 2) (i+1)

{- TABLE -}
newtype Table a b = Tbl [(b,a)] deriving (Show)

newTable :: [(b, a)] -> Table a b
newTable = Tbl
findTable :: Eq t => Table a t -> t -> a
findTable (Tbl []) i = error "findTable: item not found in table"
findTable (Tbl ((j,v):r)) i
  | i==j = v
  | otherwise = findTable (Tbl r) i

updTable :: Eq a1 => (a1, a2) -> Table a2 a1 -> Table a2 a1
updTable e (Tbl []) = Tbl [e]
updTable e'@(i,_) (Tbl (e@(j,_):r))
  | i==j          = Tbl (e':r)
  | otherwise       = Tbl (e:r')
  where Tbl r'      = updTable e' (Tbl r)

{- TSP Auxiliary Functions - Based on [R99] -}

range :: ((Int,Set),(Int,Set)) -> [(Int, Set)]
range ((startCity,startSet),(endCity,endSet)) = [(iCity,iSet) | iCity <-[startCity..endCity], iSet <- [startSet..endSet]]

dynamic :: (Table entry TspCoord -> TspCoord -> entry) -> (TspCoord,TspCoord) -> Table entry TspCoord
dynamic compute bnds = t
  where t = newTable (map ( \coord -> ( coord , compute t coord) ) (range bnds) )

type TspCoord = (Int, Set)
type TspEntry = (Int,[Int])

compTsp :: RoadMap -> Int -> Table TspEntry TspCoord -> TspCoord -> TspEntry
compTsp rm n a (i,k)
  | isEmpty k = (intDistance rm (show (i-1)) (show (n-1)),[i,n])
  | otherwise = minimum [ addFst (findTable a (j,delSet j k)) (intDistance rm (show (i-1)) (show (j-1))) | j <- set2List k]
  where addFst (c,p) w = (w+c,i:p)

bndsTsp :: Int -> ((Int,Set),(Int,Set))
bndsTsp n = ((1,emptySet),(n,fullSet n))

tsp:: RoadMap -> TspEntry
tsp rm = findTable t (n,fullSet (n-1))
  where n = length (cities rm)
        t = dynamic (compTsp rm n) (bndsTsp n)

{- TSP -}

{- Complexity: O(n^2 * 2^n) -}
travelSales :: RoadMap -> Path
travelSales rm
  | isStronglyConnected rm = map (\x -> show (x-1)) (snd (tsp rm))
  | otherwise = []

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 4), ("2", "3", 2)]
