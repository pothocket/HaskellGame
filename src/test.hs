snowball :: (a -> a) -> [a] -> [a]
snowball f xs = xs ++ snowball f (map f xs)
