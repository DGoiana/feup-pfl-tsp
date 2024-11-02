import qualified Data.Maybe
-- import qualified Data.List
-- import qualified Data.Array
-- import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String

type Path = [City]

type Distance = Int

type RoadMap = [(City, City, Distance)]

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

citiesRec :: RoadMap -> [City]
citiesRec [] = []
citiesRec ((c1, c2, _dist) : xs) = [c1, c2] ++ cities xs

cities :: RoadMap -> [City]
cities roadMap = removeDuplicates (citiesRec roadMap)

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] c1 c2 = False
areAdjacent ((x1, x2, _dist) : xs) c1 c2
  | x1 == c1 && x2 == c2 = True
  | x1 == c2 && x2 == c1 = True
  | otherwise = areAdjacent xs c1 c2

distance :: RoadMap -> City -> City -> Maybe Distance
distance [] c1 c2 = Nothing
distance ((x1, x2, d) : xs) c1 c2
  | x1 == c1 && x2 == c2 = Just d
  | x1 == c2 && x2 == c1 = Just d
  | otherwise = distance xs c1 c2

intDistance:: RoadMap -> City -> City -> Distance
intDistance rm c1 c2
  | Data.Maybe.isNothing d =  1000000
  | otherwise = Data.Maybe.fromJust d
  where d = distance rm c1 c2

adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent [] _ = []
adjacent ((c1, c2, dist) : xs) c
  | c1 == c = (c2, dist) : adjacent xs c
  | c2 == c = (c1, dist) : adjacent xs c
  | otherwise = adjacent xs c

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance r (c1 : c2 : cs)
  | areAdjacent r c1 c2 = do
      d1 <- distance r c1 c2
      d2 <- pathDistance r (c2 : cs)
      return (d1 + d2)
  | otherwise = Nothing

{- rome :: RoadMap -> [City], returns the names of the cities with the
highest number of roads connecting to them (i.e. the vertices with the
highest degree). -}
{- rome :: RoadMap -> [City] -}

rome :: RoadMap -> [City]
rome rm = [c | (c, num) <- tuples, num == maximum (map snd tuples)]
  where
    tuples = [(c, length (adjacent rm c)) | c <- cities rm]

{- isStronglyConnected :: RoadMap -> Bool, returns a boolean indicat-
ing whether all the cities in the graph are connected in the roadmap (i.e.,
if every city is reachable from every other city) -}
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = length (dfs_ rm [] [head (map (\(x, _, _) -> x) rm)]) == length (cities rm)

dfs_ :: RoadMap -> [City] -> [City] -> [City]
dfs_ rm visited [] = visited
dfs_ rm visited (x : xs)
  | x `elem` visited = dfs_ rm visited xs
  | otherwise = dfs_ rm (x : visited) (map fst (adjacent rm x) ++ xs)

{- shortestPath :: RoadMap -> City -> City -> [Path], computes all
shortest paths [RL99, BG20] connecting the two cities given as input.
Note that there may be more than one path with the same total distance.
If there are no paths between the input cities, then return an empty list.
Note that the (only) shortest path between a city c and itself is [c]. -}
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

{- travelSales :: RoadMap -> Path, given a roadmap, returns a solution
of the Traveling Salesman Problem (TSP). In this problem, a traveling
salesperson has to visit each city exactly once and come back to the start-
ing town. The problem is to find the shortest route, that is, the route
whose total distance is minimum. This problem has a known solution
using dynamic programming [RL99]. Any optimal TSP path will be ac-
cepted and the function only needs to return one of them, so the starting
city (which is also the ending city) is left to be chosen by each group. Note
that the roadmap might not be a complete graph (i.e. a graph where all
vertices are connected to all other vertices). If the graph does not have a
TSP path, then return an empty list -}

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

{- TSP Auxiliary Functions -}

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
