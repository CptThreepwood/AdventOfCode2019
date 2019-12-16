import Data.List
import Data.Function
import Data.List.Split

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq)
data Segment = Segment {start :: Point, end :: Point} deriving (Show, Eq)

move :: [Char] -> Point -> Point
move instruction cur
  | dir == 'U' = Point x (y + read mag)
  | dir == 'D' = Point x (y - read mag)
  | dir == 'R' = Point (x + read mag) y
  | dir == 'L' = Point (x - read mag) y
  | otherwise = error $ "Unknown Direction: " ++ [dir]
  where Point x y = cur
        (d, mag) = splitAt 1 instruction
        dir = head d

nextSegment :: Segment -> [Char] -> Segment
nextSegment current instruction = 
    let init = end current
    in Segment init (move instruction init)

segments :: [[Char]] -> [Segment]
segments instructions = 
    let start = [Segment (Point 0 0) (Point 0 0)]
    in init $ foldl (\acc x -> (nextSegment (head acc) x):acc) start instructions

boundingBox :: Segment -> (Point, Point)
boundingBox seg = 
    let segX = [x $ start seg, x $ end seg]
        segY = [y $ start seg, y $ end seg]
    in ((Point (minimum segX) (minimum segY)), (Point (maximum segX) (maximum segY)))

crosses :: Segment -> Segment -> Bool
crosses a b = 
    let boxA = boundingBox a
        boxB = boundingBox b
        boundsX = (x $ fst boxA) <= (x $ fst boxB) && (x $ fst boxB) <= (x $ snd boxA)
        boundsY = (y $ fst boxB) <= (y $ fst boxA) && (y $ fst boxA) <= (y $ snd boxB)
    in boundsX && boundsY

crossingSegments :: ([Segment], [Segment]) -> [(Segment, Segment)]
crossingSegments (a, b) = [(x, y) | x <- a, y <- b, (crosses x y) || (crosses y x)]

crossingPoint :: (Segment, Segment) -> Point
crossingPoint (a, b)
    | x (start a) == x (end a) = Point (x $ start a) (y $ start b)
    | y (start a) == y (end a) = Point (x $ start b) (y $ start a)

inSegment :: Point -> Segment -> Bool
inSegment pt seg = 
    let box = boundingBox seg
        boundX = (x $ fst box) <= x pt && x pt <= (x $ snd box)
        boundY = (y $ fst box) <= y pt && y pt <= (y $ snd box)
    in  boundX && boundY

-- Distance Metrics
-- Manhattan Distance to Origin
manhattan :: Point -> Int
manhattan point = 
    let Point x y = point
    in abs x + abs y

-- Length of Path
segmentLength :: Segment -> Int
segmentLength segment = 
    let s = start segment
        e = end segment
    in abs((x s) - (x e)) + abs((y s) - (y e))

pathLength :: [Segment] -> Int
pathLength segments = sum $ map segmentLength segments

path2Point :: Point -> [Segment] -> Int
path2Point p segs = 
    let path = (takeWhile (\x -> not . inSegment p $ x) segs)
        endpoint = if length path > 0 then (end . last $ path) else Point 0 0
        extended = (Segment endpoint p):path
    in pathLength extended

-- Generics
-- Read Instructions to Segments
input2Path :: [Char] -> [[Char]]
input2Path input = splitOn "," input

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y, _] = (x, y)
tuplify2 [x, y] = (x, y)
tuplify2 [x] = error "Insufficient Paths"
tuplify2 [] = error "Insufficient Paths"

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f = uncurry ((,) `on` f)

parseInput :: [Char] -> ([Segment], [Segment])
parseInput input = mapPair (reverse . segments . input2Path) (tuplify2 . lines $ input)

-- Find all crossing points for segments
getCrossings :: ([Segment], [Segment]) -> [Point]
getCrossings paths = filter (\p -> p /= Point 0 0)
                    . map crossingPoint
                    . crossingSegments
                    $ paths

-- Find closest point under some metric
closest :: (Point -> Int) -> [Point] -> Point
closest distance crossings = minimumBy (compare `on` distance) crossings

-- First Answer
manhattan2Closest :: [Char] -> Int
manhattan2Closest input = manhattan
                        . closest manhattan
                        . getCrossings
                        . parseInput
                        $ input

distance2Closest :: [Char] -> Int
distance2Closest input = 
    let paths = parseInput input
    in minimum [ path2Point cross (fst paths)  + path2Point cross (snd paths) | cross <- getCrossings paths ]

-- Run Code
main = do
    input <- readFile "input_3.txt"

    -- Tests 1
    print $ manhattan2Closest "R8,U5,L5,D3\nU7,R6,D4,L4" == 6
    print $ manhattan2Closest "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" == 159
    print $ manhattan2Closest "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" == 135
    -- Answer 1
    print $ manhattan2Closest input

    -- Tests 2
    print $ distance2Closest "R8,U5,L5,D3\nU7,R6,D4,L4" == 30
    print $ distance2Closest "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" == 610
    print $ distance2Closest "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" == 410
    -- Answer 2
    print $ distance2Closest input
