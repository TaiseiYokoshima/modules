square x = x ^ 2
twice x y = x (x y)

fibonacci x 
  | x > 1 = ( (fibonacci (x - 1)) + (fibonacci (x - 2))) 
  | otherwise = 1


main :: IO ()
main = print (fibonacci 3)



