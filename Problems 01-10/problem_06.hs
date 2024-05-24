isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome = (==) <$> foldl (flip (:)) [] <*> id
