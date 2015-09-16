listLen :: [a] -> Int
listLen []     = 0
listLen (_:xs) = 1 + (listLen xs)

--listMean :: [Int] -> Float

listPal :: [a] -> [a]
listPal [] = []
listPal (x:xs) = [x] ++ (listPal xs) ++ [x]

--isPal :: [Eq a] -> Bool
isPal xs | (length xs <= 1) = True
isPal (x:xs) = (x == (last xs)) && (isPal (init xs))

--sortByLen :: [[a]] -> [[a]]

intersperse :: a -> [[a]] -> [a]
intersperse n l@(x:xs)
  | null l = []
  | null xs = x
  | otherwise = x ++ [n] ++ (intersperse n xs)


