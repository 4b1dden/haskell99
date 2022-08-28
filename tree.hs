main :: IO ()
main = return ()

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf x = Branch x Empty Empty

-- PROBLEM 54A
-- lol


-- PROBLEM 55
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [leaf 'x']
cbalTree n =
    if n `mod` 2 == 1 then
        [Branch 'x' leftSubtree rightSubtree |
            leftSubtree <- cbalTree ((n - 1) `div` 2),
            rightSubtree <- cbalTree ((n - 1) `div` 2)
        ]
    else 
        concat [ 
            [Branch 'x' leftSubtree rightSubtree, Branch 'x' rightSubtree leftSubtree] | 
                leftSubtree <- cbalTree (n `div` 2),
                rightSubtree <- cbalTree  ((n - 1) `div` 2)
        ]


--- PROBLEM 56
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True 
mirror (Branch _ l r) (Branch _ j k) = mirror l k && mirror r j 
mirror _ _ = False 

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

--- PROBLEM 57
appendToTree :: Ord a => a -> Tree a -> Tree a
appendToTree n Empty = Branch n Empty Empty
appendToTree n o@(Branch d l r) =
    case compare n d of 
      LT -> Branch d (appendToTree n l) r
      GT -> Branch d l (appendToTree n r)
      EQ -> o -- we can ignore dupes

construct :: Ord a => [a] -> Tree a
construct ls = foldl (flip appendToTree) Empty ls

