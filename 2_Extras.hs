import Data.Char
import Data.List
import Data.Maybe

-- Caeser Cipher

let2int :: Char -> Int
let2int c = (ord c) - (ord 'a')

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift _ ' ' = ' '
shift f c =
  let v = (let2int c) + f in
    if v < 26 then
      int2let v
    else
      int2let (v - 26)

encode :: Int -> String -> String
encode f ""     = ""
encode f (x:xs) = (shift f x):(encode f xs)

-- Frequency tables

table :: [Float]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
  6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

percent :: Int -> Int -> Float
percent a b = 100 * ((fromIntegral a) / (fromIntegral b))

freqs :: String -> [Float]
freqs s =
  let l = length s in
    map (\ a -> percent a l) [x | i <- [0..25], let x = count i s]
  where
    count :: Int -> String -> Int
    count c s = foldl (\ a b -> a + (isChar b c)) 0 s

    isChar :: Char -> Int -> Int
    isChar c i =
      let v = let2int c in
      if v == i then 1
      else 0

chisqr :: [Float] -> [Float] -> Float
chisqr o e = foldl (+) 0 $ zipWith (\ a b -> ((a - b)**2)/b) o e

rotate :: Int -> [a] -> [a]
rotate n list =  (drop n list) ++ (take n list)

crack :: String -> String
crack s =
  let t = freqs s in
  let csqList = [chisqr (rotate n t) table | n <- [0..25]] in
  let ind = fromJust $ elemIndex (minimum csqList) csqList in
  encode (-ind) s

-- List comprehension sans the sugar

lc1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lc1 f p as = map f $ filter p as

lc2 :: (a -> b -> c) -> [a] -> (a -> [b]) -> (b -> Bool) -> [c]
lc2 f as bf p = concat $ map (\ a -> (map (\ b -> f a b) $ filter p (bf a))) as

lc3 :: (Int -> Int -> Int -> a) -> Int -> [a]
lc3 f n =
  concat $ map (\ a ->
    concat $ map (\ b ->
        map (\ c ->
          f a b c
        ) $ filter (\ c -> c*c == a*a + b*b) (mList b n)
    ) (mList a n)
  ) $ filter even (mList 1 n)
  where
    mList :: Int -> Int -> [Int]
    mList from to =
      if from >= to then [from]
      else from:(mList (from+1) to)
