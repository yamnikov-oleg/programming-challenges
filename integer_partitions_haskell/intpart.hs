import Data.List
import Test.HUnit

-- |Finds all partitions of `n` of length `l`, whose first element is `i`.
partsOfLen' :: Integer -> Integer -> Integer -> [[Integer]]
partsOfLen' n l i = ([i]++) `map` partsOfLen (n-i) (l-1)

-- |Finds all partitions of `n` of length `l`.
partsOfLen :: Integer -> Integer -> [[Integer]]
partsOfLen n 1 = [[n]]
partsOfLen n l = foldl (++) [] $ map (partsOfLen' n l) [1..quot n l]

assertPartsOfLen res n l =
  assertEqual ("for (partsOfLen "++(show n)++" "++(show l)++")") res (partsOfLen n l)

testPartsOfLen =
  test [ assertPartsOfLen [[5]] 5 1,
         assertPartsOfLen [[1, 4], [2, 3]] 5 2,
         assertPartsOfLen [[1, 1, 3], [1, 2, 2]] 5 3,
         assertPartsOfLen [[1, 1, 1, 2]] 5 4,
         assertPartsOfLen [[1, 1, 1, 1, 1]] 5 5 ]

-- |Finds all partitions of `n`.
partitions :: Integer -> [[Integer]]
partitions n = foldl (++) [] $ map (partsOfLen n) [1..n]

assertPartitions res n =
  assertEqual ("for (partitions "++(show n)++")") res (partitions n)

testPartitions =
  test [ assertPartitions [[5], [1, 4], [2, 3], [1, 1, 3], [1, 2, 2], [1, 1, 1, 2], [1, 1, 1, 1, 1]] 5 ]

nodups' :: Eq a => Maybe a -> [a] -> [a]
nodups' _ [] = []
nodups' Nothing (x:xs) = x : nodups' (Just x) xs
nodups' (Just lst) (x:xs)
  | lst == x = nodups' (Just lst) xs
  | otherwise = x : nodups' (Just x) xs

-- |Removes consequent duplicates from the list.
nodups :: Eq a => [a] -> [a]
nodups l = nodups' Nothing l

-- |Returns products of all the partitions of a number.
partProds :: Integer -> [Integer]
partProds n = nodups $ sort $ map (foldl (*) 1) (partitions n)

assertPartProds res n = assertEqual ("for (partProds "++(show n)++")") res (partProds n)
testPartProds = test [ assertPartProds [1,2,3,4,5,6] 5,
                       assertPartProds [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 16, 18] 8 ]

-- |Returns range between maximum and minimum elements of a list.
range :: (Num a, Ord a) => [a] -> a
range xs = maximum xs - minimum xs

-- |Returns average value of a list.
avg :: (Real a, Fractional b) => [a] -> b
avg xs = realToFrac (sum xs) / genericLength xs

-- |Returns median value of a list.
median :: (Real a, Ord a, Fractional b) => [a] -> b
median xs
  | odd len = realToFrac $ sorted !! mid
  | even len = (/2) $ realToFrac $ sorted !! mid + sorted !! (mid - 1)
  where len = length xs
        mid = quot len 2
        sorted = sort xs

data ListStats a b =
  ListStats { prange :: a
            , pavg :: b
            , pmed :: b }

instance (Show a, Show b) => Show (ListStats a b) where
  show s = "Range: " ++ (show $ prange s) ++
           "; Average: " ++ (show $ pavg s) ++
           "; Median: " ++ (show $ pmed s) ++ "."

-- |Computes list statistics.
stats :: (Real a, Ord a, Fractional b) => [a] -> ListStats a b
stats l = ListStats { prange = range l, pavg = avg l, pmed = median l }
