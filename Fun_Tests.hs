splitString :: Char -> String -> [String]
splitString c s = split c s "" []
  where
    split :: Char -> String -> String -> [String] -> [String]
    split c' []     sub acc
      | sub == ""           = reverse acc
      | otherwise           = reverse (sub:acc)
    split c' (c:cs) ""  acc = split c' cs [c]        acc
    split c' (c:cs) sub acc
      | c' == c             = split c' cs ""         (sub:acc)
      | otherwise           = split c' cs (sub++[c]) acc
