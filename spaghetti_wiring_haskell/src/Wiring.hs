module Wiring
    ( Point (..)
    , Path (..)
    , MapSize (..)
    , Solution (..)
    , Direction (..)
    , pathsBetweenPoints
    , wireup
    , WireMap (..)
    , cleanMap
    , toMap
    , toString
    ) where

import           Data.List (intercalate)

newtype Point = Point (Int, Int)
                deriving (Show, Eq)

newtype Path = Path [Point]
               deriving (Show, Eq)

newtype MapSize = MapSize (Int, Int)
                  deriving (Show, Eq)

newtype Solution = Solution [Path]
                   deriving (Show, Eq)

data Direction = DirUp
               | DirRight
               | DirDown
               | DirLeft
               deriving (Show, Eq)

width :: MapSize -> Int
width (MapSize (w, _)) = w

height :: MapSize -> Int
height (MapSize (_, h)) = h

movePoint :: Direction -> Point -> Point
movePoint DirUp (Point (x, y))    = Point (x, y-1)
movePoint DirRight (Point (x, y)) = Point (x+1, y)
movePoint DirDown (Point (x, y))  = Point (x, y+1)
movePoint DirLeft (Point (x, y))  = Point (x-1, y)

distance :: Point -> Point -> Int
distance (Point (x1, y1)) (Point (x2, y2)) = abs (x1-x2) + abs (y1-y2)

inBound :: Point -> MapSize -> Bool
inBound (Point (x, y)) size = x >= 0 &&
                              x < width size &&
                              y >= 0 &&
                              y < height size

pathsBetweenPoints :: MapSize -> (Point, Point) -> [Point] -> [Point] -> [Path]
pathsBetweenPoints size (from, to) ignoredPoints walkedPoints
  | from == to = [Path [to]]
  | otherwise = concatMap walk $ dirs from to
  where walk dir = map (\(Path points) -> Path (from:points)) $ walkThrough $ movePoint dir from
        walkThrough point = if not (point `inBound` size) ||
                               point == from ||
                               (point /= to && point `elem` ignoredPoints) ||
                               (point /= to && any (\p -> distance p point <= 1 ) walkedPoints)
                              then []
                              else pathsBetweenPoints size (point, to) ignoredPoints (from:walkedPoints)
        dirs (Point (x1, y1)) (Point (x2, y2))
          | dx >= 0 && dy >= 0 && adx >= ady = [DirRight, DirDown, DirUp, DirLeft]
          | dx >= 0 && dy >= 0 && ady > adx = [DirDown, DirRight, DirLeft, DirUp]
          | dx >= 0 && dy <= 0 && adx >= ady = [DirRight, DirUp, DirDown, DirLeft]
          | dx >= 0 && dy <= 0 && ady > adx = [DirUp, DirRight, DirLeft, DirDown]
          | dx <= 0 && dy >= 0 && adx >= ady = [DirLeft, DirDown, DirUp, DirRight]
          | dx <= 0 && dy >= 0 && ady > adx = [DirDown, DirLeft, DirRight, DirUp]
          | dx <= 0 && dy <= 0 && adx >= ady = [DirLeft, DirUp, DirDown, DirRight]
          | dx <= 0 && dy <= 0 && ady > adx = [DirUp, DirLeft, DirRight, DirDown]
          where dx = x2 - x1
                dy = y2 - y1
                adx = abs dx
                ady = abs dy

wireup' :: MapSize -> [(Point, Point)] -> [Point] -> [Solution]
wireup' size [(pntA, pntB)] ignore = map (\p -> Solution [p]) $ pathsBetweenPoints size (pntA, pntB) ignore []
wireup' size ((from, to):pnts) ignore
  = do Solution derivSolution <- wireup' size pnts (from:to:ignore)
       curPath <- pathsBetweenPoints size (from, to) (allPoints derivSolution ++ ignore) []
       return (Solution (curPath:derivSolution))
    where
      allPoints = concatMap (\(Path pnts) -> pnts)

wireup :: MapSize -> [(Point, Point)] -> Maybe Solution
wireup size pnts = listToMaybe $ wireup' size pnts []
  where listToMaybe []    = Nothing
        listToMaybe (s:_) = Just s

newtype WireMap = WireMap [[Int]]
                  deriving (Show, Eq)

cleanMap :: MapSize -> WireMap
cleanMap size
  = WireMap $ map (const $ map (const 0) [0..width size - 1]) [0..height size - 1]

setListElement :: Int -> [Int] -> Int -> [Int]
setListElement _ [] _         = []
setListElement val (_:xs) 0   = val:xs
setListElement val (x:xs) ind = x : setListElement val xs (ind-1)

setMapCell :: Int -> WireMap -> Point -> WireMap
setMapCell val (WireMap cells) (Point (col, row))
  = WireMap $ zipWith setIfIndex [0..] cells
    where setIfIndex ind list
            | ind == row = setListElement val list col
            | otherwise = list

toMap :: MapSize -> Solution -> WireMap
toMap size (Solution paths)
  = foldl markPathOnMap (cleanMap size) $ zip [1..] paths
  where markPathOnMap wmap (i, Path pnts)
          = foldl (setMapCell i) wmap pnts

intToLetter :: Int -> Char
intToLetter 0 = '.'
intToLetter i = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" !! (i-1)

toString :: WireMap -> String
toString (WireMap cells) = intercalate "\n" $ map (map intToLetter) cells
