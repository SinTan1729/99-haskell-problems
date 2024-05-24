elementAt :: (Num n, Eq n) => [a] -> n -> a
elementAt [] _ = error "Invalid index"
elementAt (hd : _) 1 = hd
elementAt (_ : tl) k = elementAt tl (k - 1)
