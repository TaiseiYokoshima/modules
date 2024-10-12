inorder [] = True
inorder ( x:y:xs ) = x < y && inorder (y:xs)
inorder [x] = True

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x [y] = if x > y then [y, x] else [x, y]
insert x (first:rest)  
  | x <= first = x:first:rest
  | otherwise = first:insert x rest

sort_inner :: Ord a => [a] -> [a] -> [a]
sort_inner [] (first:rest) = (first:rest)
sort_inner (first:rest) (second:all) = sort_inner (rest) (insert first (second:all))

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (first:rest) = sort_inner rest [first]


optional_func :: Either (Int, [Int]) ([Int], Int) -> Integer
optional_func g [] = 1
optional_func [] g = 1
















