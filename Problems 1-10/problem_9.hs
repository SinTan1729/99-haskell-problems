pack :: (Eq a) => [a] -> [[a]]
pack =
  foldr
    ( \x acc -> case acc of
        [] -> [[x]]
        (hd : tl) ->
          if x == head hd
            then (x : hd) : tl
            else [x] : hd : tl
    )
    []
