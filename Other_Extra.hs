pack :: Eq a => [a] -> [[a]]
pack list = packup list [] []
  where
    packup :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
    packup []     sub acc =   acc ++ [sub]
    packup (a:as) []  acc =   packup as [a] acc
    packup (a:as) sub acc =
      if a == (head sub) then packup as (a:sub) acc
      else                    packup as [a] (acc++[sub])
