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

{-
Description: Determines the cities in a RoadMap.
Arguments:
  - roadMap: roadMap where cities will be derived 
Time Complexity: O(n) 
Return: List of cities in roadmap.
-}
cities :: RoadMap -> [City]
cities roadMap = removeDuplicates (citiesRec roadMap)

{-
Description: Determines if two cities are adjacent.
Arguments:
  - roadMap    : a roadMap 
  - firstCity  : one of the cities
  - secondCity : another city
Time Complexity: O(n) 
Return: If the cities are adjacent.
-}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] c1 c2 = False
areAdjacent ((x1, x2, _dist) : xs) c1 c2
  | x1 == c1 && x2 == c2 = True
  | x1 == c2 && x2 == c1 = True
  | otherwise = areAdjacent xs c1 c2

{-
Description: Determines the distance between two cities.
Arguments:
  - roadMap    : a roadMap
  - firstCity  : one of the cities
  - secondCity : another city
Time Complexity: O(n)
Return: Nothing if there are no edge and the Just distance if there is.
-}
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] c1 c2 = Nothing
distance ((x1, x2, d) : xs) c1 c2
  | x1 == c1 && x2 == c2 = Just d
  | x1 == c2 && x2 == c1 = Just d
  | otherwise = distance xs c1 c2

{-
Description: Determines the integer value of the distance between two cities.
Arguments:
  - roadMap    : a roadMap
  - firstCity  : one of the cities
  - secondCity : another city
Time Complexity: O(1)
Return: 100000 if there are no edge and the distance if there is.
-}
intDistance:: RoadMap -> City -> City -> Distance
intDistance rm c1 c2 = case distance rm c1 c2 of
  Nothing -> 100000
  Just d -> d

{-
Description: Determines a list of all the adjacent cities to a given city.
Arguments:
  - roadMap    : a roadMap 
  - city       : one of the cities
Time Complexity: O(n)
Return: List of the cities that are adjacent to city.
-}
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent [] _ = []
adjacent ((c1, c2, dist) : xs) c
  | c1 == c = (c2, dist) : adjacent xs c
  | c2 == c = (c1, dist) : adjacent xs c
  | otherwise = adjacent xs c

{-
Description: Determines the sum of distances in a path.
Arguments:
  - roadMap    : a roadMap 
  - path       : a path
Time Complexity: O(n^2)
Return: Nothing if there is no distance and the sum of distances if there is.
-}
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance r (c1 : c2 : cs)
  | areAdjacent r c1 c2 = do
      d1 <- distance r c1 c2
      d2 <- pathDistance r (c2 : cs)
      return (d1 + d2)
  | otherwise = Nothing

{-
Description: Determines the sum of distances in a path.
Arguments:
  - roadMap    : a roadMap
  - path       : a path
Time Complexity: O(n^2)
Return: Nothing if there is no distance and the sum of distances if there is.
-}
rome :: RoadMap -> [City]
rome rm = [c | (c, num) <- tuples, num == maximum (map snd tuples)]
  where
    tuples = [(c, length (adjacent rm c)) | c <- cities rm]

{-
Description: Determines if all cities are connected.
Arguments:
  - roadMap    : a roadMap
Time Complexity: O(V+E)
Return: If all of the cities in the roadMap are connected.
-}
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = length (dfs_ rm [] [head (map (\(x, _, _) -> x) rm)]) == length (cities rm)

dfs_ :: RoadMap -> [City] -> [City] -> [City]
dfs_ rm visited [] = visited
dfs_ rm visited (x : xs)
  | x `elem` visited = dfs_ rm visited xs
  | otherwise = dfs_ rm (x : visited) (map fst (adjacent rm x) ++ xs)

{-
Description: Determines the shortest path between two cities.
Arguments:
  - roadMap    : a roadMap
  - start      : start city
  - end        : end city
Time Complexity: O(V+E)
Return: The shortest path between the two cities.
-}
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

{- Auxiliary Functions - Based on [R99] -}
rangeTSP :: ((Int,Set),(Int,Set)) -> [(Int, Set)]
rangeTSP ((startCity,startSet),(endCity,endSet)) = [(iCity,iSet) | iCity <-[startCity..endCity], iSet <- [startSet..endSet]]

-- Not used
rangeASP :: ((Int,Int,Int),(Int,Int,Int)) -> [(Int, Int, Int)]
rangeASP ((startI,startJ,startK),(endI,endJ,endK)) = [(iI,iJ,iK) | iI <- [startI..endI],iJ <- [startJ..endJ], iK <- [startK..endK]]

dynamicTSP :: (Table entry TspCoord -> TspCoord -> entry) -> (TspCoord,TspCoord) -> Table entry TspCoord
dynamicTSP compute bnds = t
  where t = newTable (map ( \coord -> ( coord , compute t coord) ) (rangeTSP bnds) )

-- Not used
dynamicASP :: (Table entry AspCoord -> AspCoord -> entry) -> (AspCoord,AspCoord) -> Table entry AspCoord
dynamicASP compute bnds = t
  where t = newTable (map ( \coord -> ( coord , compute t coord) ) (rangeASP bnds) )

{- TSP Auxiliary Functions - Based on [R99] -}

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
        t = dynamicTSP (compTsp rm n) (bndsTsp n)

{- Shortest Path Auxiliary Functions - Based on [R99] -}

-- Not used
type AspCoord = (Int,Int,Int)
type AspEntry = (Int,[Int])

-- Not used
compAsp :: RoadMap -> Table AspEntry AspCoord -> AspCoord -> AspEntry
compAsp rm c (i,j,k)
  | k == 0 = (intDistance rm (show (i-1)) (show (j-1)), if i==j then [i] else [i,j])
  | otherwise = let (v1,p)   = findTable c (i,j,k-1)
                    (a,p1)   = findTable c (i,k,k-1)
                    (b,_:p2) = findTable c (k,j,k-1)
                    v2 = a+b
                in if v1 < v2 then (v1,p)  else (v2,p1++p2)

-- Not used
bndsAsp :: Int -> ((Int,Int,Int),(Int,Int,Int))
bndsAsp n = ((1,1,0),(n,n,n))

-- Not used
asp :: RoadMap -> Int -> Int -> [City]
asp rm i j = map (\x -> show (x-1)) (snd (findTable t (i,j,n)))
  where n = length (cities rm)
        t = dynamicASP (compAsp rm) (bndsAsp n)

{- TSP -}
{-
Description: Determines the shortest path for trasversing all of the cities in a roadMap (starting in the last city).
Arguments:
  - roadMap    : a roadMap
  - start      : start city
  - end        : end city
Time Complexity: O(n^2 * 2^n)
Return: The path of all the cities, starting on the last, where the cost is minimized.
-}
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
