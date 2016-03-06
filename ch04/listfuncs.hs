myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myNull :: [a] -> Bool
myNull [] = True
myNull _  = False

myHead :: [a] -> a
myHead []    = error "empty list"
myHead (x:_) = x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

myTail :: [a] -> [a]
myTail []     = error "empty list"
myTail (_:xs) = xs

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

myLast :: [a] -> a
myLast []     = error "empty list"
myLast (x:xs)
  | null xs   = x
  | otherwise = myLast xs

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast (x:xs)
  | null xs   = Just x
  | otherwise = safeLast xs

myInit :: [a] -> [a]
myInit []     = error "empty list"
myInit (x:[]) = []
myInit (x:xs) = x : (myInit xs)

safeInit :: [a] -> Maybe [a]
safeInit []     = Nothing
safeInit (x:[]) = Just []
safeInit (x:xs) = Just (x : (myInit xs))

myAppend :: [a] -> [a] -> [a]
myAppend [] ys     = ys
myAppend (x:xs) ys = x : (myAppend xs ys)

myConcat :: [[a]] -> [a]
myConcat []     = []
myConcat (x:xs) = myAppend x (myConcat xs)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myAppend (myReverse xs) [x]

myAnd :: [Bool] -> Bool
myAnd []     = error "empty list"
myAnd (b:bs) = b && case bs of
                      [] -> b
                      otherwise -> (myAnd bs)

myOr :: [Bool] -> Bool
myOr []     = error "empty list"
myOr (b:bs) = b || case bs of
                      [] -> b
                      otherwise -> (myOr bs)

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f (x:xs) = (f x) && (myAll f xs)

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = (f x) || (myAny f xs)

myTake :: Int -> [a] -> [a]
myTake n l@(x:xs)
  | n <= 0    = []
  | null l    = []
  | otherwise = (x:(myTake (n - 1) xs))

myDrop :: Int -> [a] -> [a]
myDrop n l@(_:xs)
  | n <= 0    = l
  | null l    = []
  | otherwise = (myDrop (n - 1) xs)

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n l@(x:xs)
  | null xs   = ([], [])
  | n <= 0    = ([], l)
  | otherwise = (x:pre, suf)
    where (pre, suf) = mySplitAt (n-1) xs

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ []     = []
myTakeWhile f (x:xs)
  | f x       = x : (myTakeWhile f xs)
  | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ []     = []
myDropWhile f l@(x:xs)
  | f x       = (myDropWhile f xs)
  | otherwise = l

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan f l = (myTakeWhile f l, myDropWhile f l)

myBreak :: (a -> Bool) -> [a] -> ([a], [a])
myBreak f l = (myTakeWhile g l, myDropWhile g l)
  where g y = not (f y)

myElem :: (Eq a) => a -> [a] -> Bool
myElem x l = myAny isX l
  where isX i = x == i

myNotElem :: (Eq a) => a -> [a] -> Bool
myNotElem x l = myAll isntX l
  where isntX i = x /= i

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x       = x : (myFilter f xs)
  | otherwise = (myFilter f xs)

foldingFilter :: (a -> Bool) -> [a] -> [a]
foldingFilter f xs = foldr step [] xs
  where step a acc | f a       = a : acc
                   | otherwise = acc

myIsPrefixOf :: (Eq a) => [a] -> [a] -> Bool
myIsPrefixOf [] _          = True
myIsPrefixOf _ []          = False
myIsPrefixOf (x:xs) (y:ys) = (x == y) && (myIsPrefixOf xs ys)

myIsInfixOf :: (Eq a) => [a] -> [a] -> Bool
myIsInfixOf [] _ = True
myIsInfixOf _ [] = False
myIsInfixOf i@(x:xs) (y:ys) = ((x == y) && (myIsPrefixOf xs ys)) || (myIsInfixOf i ys)

myIsSuffixOf :: (Eq a) => [a] -> [a] -> Bool
myIsSuffixOf [] _ = True
myIsSuffixOf _ [] = False
myIsSuffixOf i o  = ((last i) == (last o)) && (myIsSuffixOf (init i) (init o))

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _                = []
myZip _ []                = []
myZip lx@(x:xs) ly@(y:ys) = (x, y) : (myZip xs ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f lx@(x:xs) ly@(y:ys) = (f x y) : (myZipWith f xs ys)

myLines :: [Char] -> [[Char]]
myLines [] = []
myLines cs
  | null suf  = [pre]
  | otherwise = pre : (myLines (myTail suf))
  where (pre, suf)  = break isNewline cs
        isNewline c = c == '\n'

myUnLines :: [[Char]] -> [Char]
myUnLines []     = []
myUnLines (x:xs) = x ++ "\n" ++ (myUnLines xs)
