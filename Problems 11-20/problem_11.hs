data ListItem a = Single a | Multiple Int a deriving (Show)

getListItem :: (Int, a) -> ListItem a
getListItem (1, x) = Single x
getListItem (n, x) = Multiple n x

encode :: (Eq a) => [a] -> [(Int, a)]
encode =
  foldr
    ( \x acc -> case acc of
        [] -> [(1, x)]
        (hd@(hd_n, hd_a) : tl) ->
          if x == hd_a
            then (hd_n + 1, hd_a) : tl
            else (1, x) : hd : tl
    )
    []

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map getListItem <$> encode
