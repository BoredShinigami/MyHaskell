-- Returns the factors of n
factors		:: Int -> [Int]
factors n	= [x | x <- [1..n], mod n x == 0]

-- Checks if value is a prime
prime 		:: Int -> Bool
prime n 	= factors n == [1,n]

-- Lists all primes from 1 to a given value
primes		:: Int -> [Int]
primes n 	= [x | x <- [2..n], prime x]

-- Returns all Pythagorean Triples up to a given value
pyths		:: Int -> [(Int,Int,Int)]
pyths x 	= [(a,b,c) | a <- [1..x],b <- [1..x],c <- [1..x], (a^2)+(b^2)==(c^2)]

-- Returns all Perfect numbers up to a given number
perfects 	:: Int -> [Int]
perfects x 	= [a|a <- [1..x], (sum(factors a))-a == a]

-- Finds Scalar Product of elements of 2 lists
scaleprod 	:: [Int] -> [Int] -> Int
scaleprod xs ys	= sum [ a | a <- zipWith (*) xs ys]

-- Calculate the Factorial of a given number
fac 0 = 1
fac n = n * fac(n-1)

-- Quicksort
qsort		:: Ord a => [a] -> [a]
qsort 	[] 	= []
qsort (x:xs) 	= qsort smaller ++ [x] ++ qsort larger
	where 	smaller = [ a | a <- xs, a <= x]
		larger 	= [ b | b <- xs, b > x]
		
qsort'		:: [Int] -> [Int]
qsort' 	[] 	= []
qsort' (x:xs) 	= qsort' smaller ++ [x] ++ qsort' larger
	where 	smaller = [ a | a <- xs, a <= x]
		larger 	= [ b | b <- xs, b > x]
		
-- Verify if all Bools in a list are true
and'		:: [Bool] -> Bool
and'	[True]	= True
and'	[False] = False
and'	[]	= True
and'	(x:xs)	= x && and' xs

-- Concatenate a list of lists
concat'		:: [[a]] -> [a]
concat'	[]	= []
concat'	(x:xs)	= x ++ concat' xs

-- Repeat an item n times
dup		:: Int -> a -> [a]
dup n x
	| n <= 0 = [] 
	| otherwise = x : dup (n-1) x
	
-- Select nth entry in a list
sel		:: [a] -> Int -> a
sel x n		= last ( take n x)

-- Check if x is an element of a list
elem'		:: Eq a => a-> [a] -> Bool
elem' a []	= False
elem' a (x:xs)
	| a==x	= True
	| otherwise = elem' a xs
	
-- Mergesort
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
	| x <= y = x : merge xs (y:ys)
	| otherwise = y : merge (x:xs) ys
	
