import Data.Char

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (a:as) = (f a) : (myMap f as)

myFoldl :: (acc -> b -> acc) -> acc -> [b] -> acc
myFoldl _ acc [] = acc
myFoldl f acc (b:bs) = myFoldl f (f acc b) bs

myFoldr :: (x -> acc -> acc) -> acc -> [x] -> acc
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

foldlUsingFoldr :: (a -> b -> a) -> a -> [b] -> a
foldlUsingFoldr _ acc [] = acc
-- how to do this

myIdentity :: [a] -> [a]
--myIdentity xs = map id xs
myIdentity xs = myFoldr (:) [] xs

myAppend :: [a] -> [a] -> [a]
myAppend xs ys = myFoldr (:) ys xs

asInt_fold :: String -> Int
asInt_fold cs = myFoldl f 0 cs
  where f acc c = (acc * 10 + (digitToInt c))

myFoldingConcat :: [[a]] -> [a]
myFoldingConcat ls = myFoldr f [] ls
  where f l acc = l ++ acc

myFoldingTakeWhile :: (a -> Bool) -> [a] -> [a]
myFoldingTakeWhile test ls = myFoldr f [] ls
  where f l acc | test l    = l:acc
                | otherwise = []

--strugs
--myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
--myGroupBy comp ls = myFoldl f [] ls
--  where f [] l         = [[l]]
--        f l ((a:as):bs) | comp l a  = (l:a:as):bs
--                        | otherwise = [l]:(a:as):bs

anyFold :: (a -> Bool) -> [a] -> Bool
anyFold test ls = myFoldr f False ls
  where f l acc = (test l) || acc

cycleFold :: [a] -> [a]
cycleFold ls = ls ++ (cycleFold ls)
