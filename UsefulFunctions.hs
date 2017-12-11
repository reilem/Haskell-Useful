
-- ### LIST SPECIFICS ###

-- Replicate any `a` an int number of times.
replicate :: int -> a -> [a]

-- Apply binary operator to starting element and list, reduces left to right
foldl :: (b -> a -> b) -> b -> [a] -> b
-- Apply binary operator to starting element and list, reduces right to left
foldr :: (a -> b -> b) -> b -> [a] -> b

-- Maps a list to another list given a mapping
map :: (a -> b) -> [a] -> [b]

-- Filters a list
filter :: (a -> Bool) -> [a] -> [a]

-- Concats two lists
(++) :: [a] -> [a] -> [a]

-- Concats n lists
concat :: [[a]] -> a

-- Other (operations that go without saying)
head :: [a] -> a
tail :: [a] -> [a]
last :: [a] -> [a]
reverse :: [a] -> [a]


-- ### STRING SPECIFICS ###

-- Breaks up string into words seperated by space
words :: String -> [String]

-- Join words with seperating space
unwords :: [String] -> String

-- Breaks up string into lines
lines :: String -> [String]

-- Joins lines with newline
unlines :: [String] -> String


-- ### FUNCTION SPECIFIC ###

-- Function composition
(.) :: (b -> c) -> (a -> b) -> a -> c

-- ### TUPLE SPECIFICS ###

-- Finds value of b corresponding to given a
lookup :: Eq a => a -> [(a,b)] -> Maybe b

-- Get first element in tuple
fst :: (a,b) -> a

-- Get second element in tuple
snd :: (a,b) -> b

-- Swaps first and second
swap :: (a,b) -> (b,a)


-- #### IO SPECIFICS ####

-- Writes string to std. output device, returns void IO
putStr :: String -> IO ()

-- Adds newline character, returns void IO
putStrLn :: String -> IO ()

-- Takes any showable and prints to std. output device, returns void IO
print :: Show a => a -> IO ()

-- Read line from std. input device, returns IO containing string
getLine :: IO String

-- Takes any showable and converts to String
show :: Show a => a -> String

-- Takes String and convert to any readbale value contained in it
read :: Read a => String -> a


-- ### MONAD SPECIFIC ###

-- (If-without-else) Takes bool condition and performs given monadic if bool succeeds
when :: Monad m => Bool -> m () -> m()

-- Extract the value from a Maybe Monad
fromJust :: Maybe a -> a

-- ### MISC ###

-- Returns unicode value of character
ord :: Char -> Int
-- Returns character of unicode value
chr :: Int -> Char

-- b will be executed before being as paramter for a
($) :: (a -> b) -> a -> b
