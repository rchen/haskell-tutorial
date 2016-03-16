-- 1.a
power1:: Int -> Int -> Int
power1 n 0 = 1
power1 n k = product([n] ++ [power1 n (k-1)])
--- 1.b
power2:: Int -> Int -> Int
power2 n 0 = 1
power2 n k | even(k) =  (n ^ 2) ^ (k `div` 2)
           | odd(k) = n * power2 n (k-1)
-- 2.a
myButLast :: [a] -> a
myButLast l = last(init l)

---2.b
rev2 :: [a] -> [a]
rev2 [x,y] = [y,x]
rev2 s@(x:xs) = [x] ++ xs
-- 3.a
tailUpto :: Int -> Int -> [Int] -> [Int]
tailUpto m n l | m > n = l
               | otherwise = tailUpto m (n-1) ([n] ++ l)
upto m n = tailUpto m n []


main = do
-- 1.a
print $ power1 3 9
-- 1.b
print $ power2 3 9
-- 2.a
print $ myButLast[1,2,3,4,5,6,7]
print $ myButLast ['a'..'z']
-- 2.b
print $ rev2 [1, 2]
print $ rev2 [1, 2, 3]
-- 3.a
print $ tailUpto 3 8 [1,2]
print $ tailUpto 8 3 [1]
