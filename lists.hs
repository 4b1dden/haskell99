module Lists (encode) where 

main :: IO ()
main = return ()

--- PROBLEM 1
myLast :: [a] -> a
myLast [] = error "No empty lists"
myLast [x] = x
myLast (x:xs) = myLast xs


--- PROBLEM 2
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [x] = error "too few elements"
myButLast [x, _] = x
myButLast (x:xs) = myButLast xs

--- PROBLEM 3
elementAt :: [a] -> Int -> a 
elementAt ls pos = ls !! (pos - 1) 

elementAt' :: [a] -> Int -> a
elementAt' (x:_) 1 = x
elementAt' [] _ = error "Index out of bounds"
elementAt' (x:xs) i  
    | i < 1 = error "Index out of bounds"
    | otherwise = elementAt' xs (i-1) 

--- PROBLEM 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = sum . map (\_->1)

--- PROBLEM 5
myReverse :: [a] -> [a]
myReverse [] = [] 
myReverse (x:xs) = myReverse xs ++ [x]

--- PROBLEM 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome ls = myReverse ls == ls

--- PROBLEM 7
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a] 
myFlatten (Elem a) = [a]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
myFlatten (List []) = [] 

--- PROBLEM 8
compress :: (Eq a) => [a] -> [a]
compress ls = foldr (\x y -> if x == (head y) then y else x:y) [last ls] ls

--- PROBLEM 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

--- PROBLEM 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode ls = map (\x -> (length x, head x)) (pack ls)

encodePointFree :: (Eq a) => [a] -> [(Int, a)]
encodePointFree = map (\x -> (length x, head x)) . pack

--- PROBLEM 11
data EncodedData a = Multiple Int a | Single a 
    deriving Show 

encodeModified :: (Eq a) => [a] -> [EncodedData a]
encodeModified = map (\(cnt, elem) -> if cnt == 1 then Single elem else Multiple cnt elem) . encode 

--- PROBLEM 12
decodeModified :: [EncodedData a] -> [a]
decodeModified = foldl (\acc curr -> 
    case curr of 
      Single x -> acc ++ [x];
      Multiple n elem -> acc ++ replicate n elem
    ) []

--- PROBLEM 13
encodeDirect :: (Eq a) => [a] -> [EncodedData a]
encodeDirect [] = []
encodeDirect (x:xs)  
    | count == 1   = (Single x) : (encodeDirect xs)
    | otherwise    = (Multiple count x) : (encodeDirect rest)
    where 
        (match, rest) = span (==x) xs
        count = 1 + length match

--- PROBLEM 14
duplicate :: [a] -> [a]
duplicate = concatMap (\x -> [x,x])

--- PROBLEM 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

--- PROBLEM 16
dropEvery :: [a] -> Int -> [a]
dropEvery ls n = dropEveryHelper ls n n 
    where dropEveryHelper [] _ _ = []
          dropEveryHelper (x:xs) n 1 = dropEveryHelper xs n n
          dropEveryHelper (x:xs) n m = x : dropEveryHelper xs n (m-1)
        
--- PROBLEM 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split ls n = (first accState, second accState)
    where accState = foldl (\acc x -> if third acc > 0 
                                         then (first acc ++ [x], second acc, (third acc) - 1)
                                         else (first acc, second acc ++ [x], third acc)
                           ) ([], [], n) ls 
first :: (a, b, c) -> a
first (x, _, _) = x
second :: (a, b, c) -> b
second (_, y, _) = y
third :: (a, b, c) -> c
third (_, _, z) = z

--- PROBLEM 18
slice :: [a] -> Int -> Int -> [a]
slice ls i j = take j $ drop i ls

--- PROBLEM 19
rotate :: [a] -> Int -> [a]
rotate ls n = take (length ls) $ drop n $ cycle ls 

--- PROBLEM 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n ls = (ls !! n, take (n - 1) ls ++ drop n ls)

















