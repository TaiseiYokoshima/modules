inorder [] = True
inorder ( x:y:xs ) = x < y && inorder (y:xs)
inorder [x] = True

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x [y] = if x > y then [y, x] else [x, y]
insert x (first:rest)  
  | x <= first = x:first:rest
  | otherwise = first:insert x rest


sort :: Ord a => [a] -> [a] -> [a]
sort [] [] = []
sort [] [x] = [x]
sort [] (x:rest) = x:rest
sort [x] [] = [x]
sort [x] [y] = insert x [y]
sort [y] (x:xs) = insert y (x:xs)
sort [x, y] [] = if x > y then [y, x] else [x, y]
sort [x, y] (first:rest) = insert y (insert x (first:rest))
sort (first:second:rest) [] = sort rest (insert second (insert first [])) 
sort (first:second:rest) [x] = sort rest (insert second (insert first [x])) 
sort (first:second:rest) (x:sorted) = sort rest (insert second (insert first (x:sorted))) 


sort_outer :: (Ord a) => [a] -> [a]
sort_outer [] = sort [] []
sort_outer [x] = sort [x] []
sort_outer (x:xs) = sort (x:xs) []

