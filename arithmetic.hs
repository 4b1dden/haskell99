import Lists

main :: IO ()
main = return ()

--- PROBLEM 31
isPrime :: Integral a => a -> Bool
isPrime n | n < 4 = n > 1
isPrime n = null [x | x <- [2..highest], n `mod` x == 0]
    where highest = floor $ sqrt $ fromIntegral n


--- PROBLEM 32
myGcd :: Int -> Int -> Int
myGcd 0 b = b
myGcd a b = myGcd (b `mod` a) a 


--- PROBLEM 33
areCoprime :: Int -> Int -> Bool
areCoprime a b = myGcd a b == 1


--- PROBLEM 34
totient :: Int -> Int
totient 1 = 1
totient m = length [x | x <- [1..m], areCoprime x m]

--- PROBLEM 35
primeFactors :: Int -> [Int]
primeFactors n = 
    case factors of
      [] -> [n]
      _ -> factors ++ primeFactors (n `div` (head factors))
    where
        factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2..n - 1] 

--- PROBLEM 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = map swap $ encode $ primeFactors n
  where swap (a,b) = (b,a)

--- PROBLEM 37
phi :: Int -> Int
phi m = product [(p - 1) * p ^ (m - 1) | (p,m) <- primeFactorsMult m]


--- PROBLEM 38
primesR :: Integral a => a -> a -> [a] 
primesR low hi = filter isPrime [low..hi] 


--- PROBLEM 39
goldbach :: Int -> (Int, Int)
goldbach n = head [(x, y) | x <- primeCandidates, y <- primeCandidates, x + y == n]
  where primeCandidates = primesR 2 n

--- PROBLEM 40
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList low hi = map (\x -> goldbach x) [x | x <- [low..hi], even x]




















