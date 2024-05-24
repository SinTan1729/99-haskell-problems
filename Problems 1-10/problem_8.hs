compress :: (Eq a) => [a] -> [a]
compress =
  foldr
    ( \x acc ->
        if acc /= []
          && x == head acc
          then acc
          else x : acc
    )
    []
