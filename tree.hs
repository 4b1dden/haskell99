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

            


