data ListItem a = Single a | Multiple Int a deriving (Read)

fromListItem :: ListItem a -> [a]
fromListItem (Single x) = [x]
fromListItem (Multiple n x) = replicate n x

decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap fromListItem
