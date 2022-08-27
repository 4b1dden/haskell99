import Data.List
import Control.Monad (replicateM)

main :: IO ()
main = return ()

--- PROBLEM 46
and' :: Bool -> Bool -> Bool
and' a b = a && b

or' :: Bool -> Bool -> Bool
or' a b = a || b

nand' :: Bool -> Bool -> Bool
nand' a b = not (and' a b)

nor' :: Bool -> Bool -> Bool
nor' a b = not (or' a b)

xor' :: Bool -> Bool -> Bool
xor' a b = not (a == b)

impl :: Bool -> Bool -> Bool
impl a b = or' (not a) b

equ' :: Bool -> Bool -> Bool
equ' a b = a == b

type BinaryTableRow = (Bool, Bool, Bool)
type BinaryTable = [BinaryTableRow]

table :: (Bool -> Bool -> Bool) -> BinaryTable
table predFn = [(a, b, predFn a b) | a <- [True, False], b <- [True, False]]

makeTableRow :: BinaryTableRow -> String
-- probably cant extend to n-ary tuple, we'd need to use list instead
makeTableRow (a, b, c) = show a ++ " " ++ show b ++ " " ++ show c

printTable :: BinaryTable -> IO ()
printTable binaryTable = putStrLn $ intercalate "\n" (map makeTableRow binaryTable) 


--- PROBLEM 47
--- what... i mean ok ill just copypasta
infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'` -- was 7, changing it to 3 got me the same results as in the original question :(

--- PROBLEM 48
tableNary :: Int -> ([Bool] -> Bool) -> [[Bool]]
tableNary n predFn = [values ++ [predFn values] | values <- makeValues n]
    where makeValues n = replicateM n [True, False]

makeNAryTableRow :: [Bool] -> String 
-- try writing intercalate with foldl
makeNAryTableRow values = 
    foldl' (\acc x -> if (null acc) 
        then acc ++ show x
        else acc ++ " " ++ show x
    ) "" values 

makeNAryTableStr :: [[Bool]] -> String
makeNAryTableStr table = intercalate "\n" (map makeNAryTableRow table)

printNAryTableStr :: [[Bool]] -> IO ()
printNAryTableStr table = putStrLn $ makeNAryTableStr table


--- PROBLEM 49
gray :: Int -> [String]
gray 0 = [""]
gray n = ['0' : prev | prev <- previousSeq] ++ ['1' : prev | prev <- reverse previousSeq]
    where previousSeq = gray (n - 1) 














