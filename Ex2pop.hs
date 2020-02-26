------------------------------------------------------------
--Data Structures. ETSI Informatica. UMA
--
--Degree: Grado en Ingenieria Informatica
--Student: Klimek, Krzysztof
--Date 6 | 11 | 2017
--
-- Exercises 2. I solved the following exercises:
-- 1,2,3,4,5,6,7a,..,8,9abcd,..,10,..,13abc,..,14,
--
------------------------------------------------------------

import Test.QuickCheck
import Data.List(nub)


--1
--allDifferent :: Eq a => [a] -> Bool
--allDifferent list = case list of
--    []      -> True
--    (x:xs)  -> x `notElem` xs && allDifferent xs
allDifferent :: Eq a => [a] -> Bool
allDifferent  [] = True
allDifferent  (x:xs) = x `notElem` xs && allDifferent xs



--2a
replicante' :: Int -> a -> [a]
replicante' n x = [ z | z <- [x], y <- [1..n]]

--2b
p_replicante' n x = n >= 0 && n <= 1000 ==>
                            length (filter (==x) xs) == n
                            && length (filter (/=x) xs) == 0
                              where xs = replicante' n x

--3
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

divisors' :: Int -> [Int]
divisors' n
    | n>0 = [x | x <- [-n..n], x /= 0 && n `mod` x ==0]
    | n<0 = [x | x <- [n..(-n)], x /= 0 && n `mod` x ==0]

--4a
gcdiv :: Int -> Int -> Int
gcdiv x y = maximum [z | z <- [1..y], x `mod` z == 0 && y `mod` z == 0]

--4b
p1_gcdiv x y z = x>0 && y>0 && z>0 ==> gcdiv (x*z) (z*y) == z * gcdiv x y

--4c
lcmul :: Int -> Int -> Int
lcmul x y = (x*y) `div` (gcdiv x y)

--5a
isPrime :: Int -> Bool
isPrime n
    | divisors n == [1,n] = True
    | otherwise = False

--5b
primesUpto :: Int -> [Int]
primesUpto n = [x | x <- [2..n], isPrime x]

--5c
primesUpto' :: Int -> [Int]
primesUpto' n = filter isPrime [1..n]

--5d
p1_primes x = primesUpto x == primesUpto' x

--6a
take' :: Int -> [a] -> [a]
take' n xs = [ x | (p,x) <- zip [0..(n-1)] xs]
--without last guard ??

--6b
drop' :: Int -> [a] -> [a]
drop' n xs = [ x | (p,x) <- zip [1..(length xs)] xs, p>n]

--6c
p1_concatenation n xs = n>0 ==> take' n xs ++ drop' n xs == xs

--7a
concat' :: [[a]] -> [a]
concat' xss = foldr (++) [] xss

--7b
--concat'' :: [[a]] -> [a]
--concat'' xss = [xs | ]

--8
unknown :: (Ord a) => [a] -> Bool
unknown xs = and [x<=y | (x,y) <- zip xs (tail xs)]

--9a
insert :: Ord a => a -> [a] -> [a]
--insert :: Int -> [Int] -> [Int]
insert n xs = (takeWhile (<n) xs) ++ (n : dropWhile (<n) xs)

--9b
p1_insert x xs = unknown xs ==> unknown (insert x xs)

--9d
isort :: Ord a => [a] -> [a]
isort xs = foldr insert [] xs

--10a
geometric :: Num a => a -> a  -> [a]
geometric x y = iterate (*y) x

--10b
p1_geometric x r = x>0 && r>0 ==>
                      and [div z y == r | (y,z) <- zip xs (tail xs)]
                          where xs = take 100 (geometric x r)

--10c
multipiesOf :: Num a => a -> [a]
multipiesOf x = iterate (+x) 0

--10d
powersOf :: Num a => a -> [a]
powersOf x = iterate (*x) 1

--13a
nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' (filter (/=x) xs)

--13b
p_nub' xs = nub xs == nub' xs

--13c
p_necessary xs = allDifferent (nub' xs)

--13d


--14
bin :: Int -> [String]
bin 0 = [""]
bin n
  | n > 0 = ['0':x | x <-xs] ++ ['1':x | x <- xs]
  where
    xs = bin (n-1)


--end
