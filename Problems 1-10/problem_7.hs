data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten x = case x of
  Elem e -> [e]
  List l -> concatMap flatten l
