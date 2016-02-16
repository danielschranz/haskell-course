-- ex 1

-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
--   | even x = (x - 2) * fun1 xs
--   | otherwise = fun1 xs

fun1 :: [Integer] -> Integer
fun1 = foldr (*) 1 . map (pred . pred) . filter even

-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n | even n = n + fun2 (n `div` 2)
--           | otherwise = fun2 (3 * n + 1)

fun2 :: Integer -> Integer
fun2 = foldr1 (+) . filter even . takeWhile (/=1) . iterate (\x -> if even x then div x 2 else 3 * x + 1)


--ex 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

insertTree :: Tree a -> a -> Tree a
insertTree Leaf x = Node 0 Leaf x Leaf
insertTree (Node 0 Leaf e Leaf) x = Node 1 (insertTree Leaf x) e Leaf
insertTree (Node 1 left e Leaf) x = Node 1 left e (insertTree Leaf x)
insertTree (Node n left@(Node ln _ _ _) e right@(Node rn _ _ _)) x
  = (Node (succ (max nrn nln)) newleft e newright)
  where newright@(Node nrn _ _ _) = if ln > rn then (insertTree right x) else right
        newleft@(Node nln _ _ _) = if (ln > rn) then left else (insertTree left x)

foldTree :: [a] -> Tree a
foldTree = foldl insertTree Leaf


--ex 3

---------------- more folds
xor :: [Bool] -> Bool
xor = foldr (\e acc -> if e then not acc else acc) False
--xor = foldr (\e acc -> not acc) False . filter id -- if using filter is ok
--xor = odd . length . filter id -- ignore using fold

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x:acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x y -> f y x) base (reverse xs)


-- ex 4

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys ]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> x*2+1) $ filter (\x -> not (elem x sieve)) [1..n]
  where sieve = map (\(i,j) -> i+j+2*i*j) . filter (\(i,j) -> i <= j || (i+j+2*i*j) <= n ) $ cartProd [1..n] [1..n]
