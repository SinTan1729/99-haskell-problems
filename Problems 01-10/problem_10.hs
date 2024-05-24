encode :: (Eq a, Num n) => [a] -> [(n, a)]
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
