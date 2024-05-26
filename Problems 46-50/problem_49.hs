gray :: Int -> [[Char]]
gray n
  | n == 1 = ["0", "1"]
  | n > 1 =
      concatMap
        ( \(i, l) ->
            if odd i
              then [(l ++)] <*> ["0", "1"]
              else [(l ++)] <*> ["1", "0"]
        )
        $ zip [1 ..]
        $ gray (n - 1)
  | otherwise = error "Not defined for n<1"
