module Utils
  ( join,
    split,
  )
where

-- functions that have been useful in multiple days
-- `imported` by symlinking into each day's src if
-- needed

-- join joins lists, e.g. strings, together
join :: [a] -> [[a]] -> [a]
join _ [] = []
join _ [x] = x
join sep (x : xs) = x ++ sep ++ join sep xs

-- split seperates a list into multiple lists on the sep given
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split sep (x : xs)
  | x == sep = split sep xs
  | otherwise = takeWhile (/= sep) (x : xs) : split sep (dropWhile (/= sep) (x : xs))