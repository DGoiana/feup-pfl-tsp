-- import qualified Data.List
-- import qualified Data.Array
-- import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String

type Path = [City]

type Distance = Int

type RoadMap = [(City, City, Distance)]

cities :: RoadMap -> [City]
cities [] = []
cities ((c1, c2, _dist) : xs) = [c1, c2] ++ cities xs

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

rome :: RoadMap -> [City]
rome [] = []
rome ((c1, c2, c) : rs)
  | 
  |

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 4), ("2", "3", 2)]
