-- Problem 1
-- Find the last element of a list

myLast :: [a] -> a
myLast (x:xs) 
    | length xs == 0 = x
    | otherwise      = myLast xs

-- Problem 2
-- Find the last but one element of a list.

myButLast :: [a] -> a
myButLast (x:xs)
    | length xs == 1 = x
    | otherwise      = myButLast xs

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt (x:xs) n
    | n > length (x:xs) = error "Number is bigger than list"
    | n == 1    = x
    | otherwise = elementAt xs (n-1) 

-- Problem 4
-- Find the number of elements of a list

myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5
-- Reverse a list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x). 

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = False
isPalindrome xs
    | xs == reverse xs = True
    | otherwise        = False

-- Problem 7
{-- 
(**) Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a `flat' 
list by replacing each list with its elements (recursively).
--}

-- Problem 8
{-- 
(**) Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be 
replaced with a single copy of the element. 
The order of the elements should not be changed. 
--}

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) 
    | x `elem` xs = compress xs
    | otherwise = [x] ++ compress xs


-- Problem 9
{--
(**) Pack consecutive duplicates of list elements into 
sublists. If a list contains repeated elements they should
be placed in separate sublists. 
--}

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- Problem 10
{--
(*) Run-length encoding of a list. Use the result of problem P09
to implement the so-called run-length encoding data compression 
method. Consecutive duplicates of elements are encoded as 
lists (N E) where N is the number of duplicates of the element E. 
--}

