------------------------------------------------------------
--Data Structures. ETSI Informatica. UMA
--
--Degree: Grado en Ingenieria Informatica
--Student: Klimek, Krzysztof
--Date 23 | 10 | 2017
--
-- Exercises 1. I solved the following exercises:
-- 1,2,3,..,5,6,7,8,9,..,11,..,15,16,17
--
------------------------------------------------------------

import Test.QuickCheck
import Test.QuickCheck

--1a
isTriple :: Integer -> Integer -> Integer -> Bool
isTriple x y z = if x*x + y*y == z*z then True else False

--1b
triple :: Integer -> Integer -> (Integer, Integer, Integer)
triple x y = (x*x-y*y, 2*x*y, x*x+y*y)

--1c,d
p_triples x y = x>0 && y>0 && x>y ==> isTriple l1 l2 h
  where
    (l1, l2, h) = triple x y

--2
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

--3a
sort2 :: Ord a => (a,a) -> (a,a)
sort2  (x,y)
    | x < y = (x,y)
    | x > y = (y,x)
    | x==y = (x, y)

--3b
p1_sort2 x y = sorted (sort2 (x,y))
  where sorted (x,y) = x <= y

p2_sort2 x y = sameElements (x,y) (sort2 (x,y))
  where
    sameElements (x,y) (x',y') = (x==x' && y==y') || (x==y' && y==x')

--3c
sort3 :: Ord a => (a,a,a) -> (a,a,a)
sort3 (x,y,z)
    | x == y && y==z = (x,y,z)

    | x>=y && y>=z = (x,y,z)
    | x>=y && z>=y && x>=z = (x,z,y)

    | y>=z && z>=x = (y,z,x)
    | y>=z && x>=z && y>=x= (y,x,z)

    | z>=x && x>=y = (z,x,y)
    | z>=x && y>=x && z>=y = (z,y,x)

--3d

p1_sort3 x y z = sorted2 (sort3 (x,y,z))
  where
    sorted2 (x,y,z) = x<=y && y<=z

p2_sort3 x y z = sameElements2 (x,y,z) (sort3 (x,y,z))
  where sameElements2 (x,y,z) (x',y',z') = (x==x' && y==y' && z==z') || (x==x' && y==z' && z==y') || (x==z' && y==y' && z==x') || (x==y' && y==x' && z==z')

--4a
max2 :: (Ord a) => a -> a -> a
max2 x y
    | x>y = x
    | otherwise = y

--4b
p1_max2 x y = (max2 x y) == x || (max2 x y) == y
p2_max2 x y = (max2 x y) >= x || (max2 x y) >= y
--p3_max2 x y = x>y && x==y ==> (max2 x y) == x
--p3_max2 x y = greaterX (max2 x y) == x
  --where
    --greaterX z = x >=y
--p4_max2 x y = greaterY (max2 x y) == y
  --where
    --greaterY z = y >=x

--5
between :: Ord a => a -> (a,a) -> Bool
between x (a,b)
    | a<x && x<b = True
    | otherwise = False

--6
equals3 :: Eq a => (a,a,a) -> Bool
equals3 (x,y,z)
    | x==y && y==z = True
    | otherwise = False

--7a
type Hour = Integer
type Minute = Integer
type Second = Integer
decompose :: Second -> (Hour, Minute, Second)
decompose x = (hours, minutes, seconds)
  where
    hours = x `div` 3600
    minutes = (x - hours*3600) `div` 60
    seconds = x `mod` 60

--7b
p_decompose x = x>=0 ==> h*3600 + m*60 + s == x && between m (0,59)
                          && between s (0,59)
  where (h,m,s) = decompose x

--8a
oneEuro :: Double
oneEuro = 166.386
pasetasToEuros :: Double -> Double
pasetasToEuros x = x/oneEuro

--8b
eurosToPasetas :: Double -> Double
eurosToPasetas x = x*oneEuro

--8c
p_inverse x = eurosToPasetas (pasetasToEuros x) == x

--9
infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
  where epsilon = 1/1000


--10a
roots :: Double -> Double -> Double -> (Double, Double)
roots a b c = (((-b) - sqrt(b*b - 4*a*c))/2*a,((-b) + sqrt(b*b - 4*a*c))/2*a)

--10b
p1_roots a b c = isRoot r1 && isRoot r2
  where
    (r1,r2) = roots a b c
    isRoot r = a*r^2 + b*r + c ~=0

--p2_roots a b c = ????? && ????? ==> isRoot r1 && isRoot r2
  --where
    --(r1,r2) = roots a b c
    --isRoot r = a*r^2 + b*r +c ~=0

--11
isMultiple :: (Eq a, Integral a) =>a -> a -> Bool
isMultiple x y
    | x `mod` y == 0 = True
    | otherwise = False

--14a
--power :: Integer -> Integer -> Integer
--power x y = if y<2 then x else x*power x (y-1)
power :: Integer -> Integer -> Integer
power b n
    | n >=0 = power' b n
    | otherwise = error "negative component"
    where
      power' b 0 = 1
      power' b n = b* power' b (n-1)

--14b
power2 :: Integer -> Integer -> Integer
power2 b n
  |n >= 0 = power' b n
  |otherwise = error "negative component"
  where
    power' b 0 = 1
    power' b n
      |even n = p*p
      |otherwise = b *power' b (n-1)
      where
        p= power' b (div n 2)



--power1 :: Integer -> Integer -> Integer
--power1 b n
--    | n>0 = b *power1 b (n-1)
--    | otherwise = error Wrong numbers

--power' :: Integer -> Integer -> Integer
--power' x y =
  --if y `mod` 2 == 0
    --then if y<2 then x else (x^(y/2)^2
    --else
      --if y<2 then x else x*power' x (y-1)

--15
factorial :: Integer -> Integer
factorial x = if x<2 then 1 else x*factorial(x-1)

--16a
divides :: Integer -> Integer -> Bool
divides x y = if y `mod` x == 0 then True else False

--16b
p1_divides x y = y/=0 && y `divides` x ==> div x y * y == x

--16c
p2_divides x y z = z/=0 && z `divides` x && z `divides` y ==> mod (x+y) z == 0

--17
median :: Ord a => (a,a,a,a,a) -> a
median (x,y,z,t,u)
    | x>z = median (z,y,x,t,u)
    | y>z = median (x,z,y,t,u)
    | t<z = median (x,y,t,z,u)
    | u<z = median (x,y,u,t,z)
    | otherwise = z

--end
