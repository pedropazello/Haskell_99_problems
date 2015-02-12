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

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

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

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = [(length x, head x) | x <- pack xs]

-- Problem 11
{--
(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an 
element has no duplicates it is simply copied into the 
result list. Only elements with duplicates are transferred 
as (N E) lists. 
--}

data MultipleList a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [MultipleList a]
encodeModified = map transform . encode
        where transform (1,a) = Single a
              transform (n,a) = Multiple n a 

-- Problem 12
{--
**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem 11. 
Construct its uncompressed version. 
--}

decodeModified :: Eq a => [MultipleList a] -> [a]
decodeModified [] = []
decodeModified (Multiple n c:xs) = replicate n c ++ decodeModified xs
decodeModified (Single a:xs)     = a : decodeModified xs

-- Problem 13
{--
(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression 
method directly. I.e. don't explicitly create the sublists containing 
the duplicates, as in problem 9, but only count them. 
As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
--}

-- not done yet

-- Problem 14
{--
(*) Duplicate the elements of a list
--}

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = replicate 2 x ++ dupli xs

-- Problem 15
{--
(**) Replicate the elements of a list a given number of times. 
--}

repli :: [a] -> Int -> [a]
repli [] _     = []
repli (x:xs) n = replicate n x ++ repli xs n

-- Problem 16
{-- 
(**) Drop every N'th element from a list. 
--}

dropEvery :: Eq a => [a] -> Int -> [a]
dropEvery [] _     = []
dropEvery (_:xs) 1 = xs 
dropEvery (x:xs) n = [x] ++ dropEvery xs (n-1) 

-- Problem 17
{--
(*) Split a list into two parts; the length of the first part is given.
--}

split :: [a] -> Int -> ([a],[a])
split xs n = (take n xs, drop n xs)

-- Problem 18
{--
(**) Extract a slice from a list.
Given two indices, i and k, the slice is the list containing the 
elements between the i'th and k'th element of the original 
list (both limits included). Start counting the elements with 1. 
--}
