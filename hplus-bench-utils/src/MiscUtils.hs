-- | The 'splitLast' function takes as input a delimeter
-- symbol and a list. It then splits all elements in the
-- list by the last occurrence of the delimeter. The two
-- halves is returned as a tuple. If the delimeter does
-- not occurr, the element is left unchanged.
splitLast :: Eq a => a -> [a] -> Either [a] ([a],[a])
splitLast c' = foldr go (Left [])
    where
        go c (Right (f,b)) = Right (c:f,b)
        go c (Left s) | c' == c = Right ([],s)
                      | otherwise = Left (c:s)

-- | The 'merge' function takes in two lists of the same type.
-- It concatenates the two lists.
-- TODO: replate usage w/ 'concat'
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys
