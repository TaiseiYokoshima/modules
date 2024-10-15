-- inorder [] = True
-- inorder ( x:y:xs ) = x < y && inorder (y:xs)
-- inorder [x] = True
--
-- insert :: Ord a => a -> [a] -> [a]
-- insert x [] = [x]
-- insert x [y] = if x > y then [y, x] else [x, y]
-- insert x (first:rest)  
--   | x <= first = x:first:rest
--   | otherwise = first:insert x rest
--
--
-- sort :: Ord a => [a] -> [a]
-- sort [] = []
-- sort (first:rest) = insert first (sort rest)
--
--
-- comp3 :: (input -> output1) -> (output1 -> output2) -> (output2 -> final) -> input -> final
-- comp3 f g z value =  z (g (f value))


data MyBool = T | F deriving Show

myand T T = T
myand x y = F

myor F F = F
myor _ _ = T

mynot F = T
mynot _ = F


data RPS = R | P | S deriving Show
beats R S = True
beats R _ = False

beats P R = True
beats P _ = False

beats S P = True
beats S _ = False


data Nat = Zero | Successor Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Successor m) n = Successor (add m n)


