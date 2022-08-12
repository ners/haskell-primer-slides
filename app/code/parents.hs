data Person = Person {name :: String, father :: Maybe Person, mother :: Maybe Person}

parents :: Person -> [Person]
parents p = [x | Just x <- [father p, mother p]]

grandparents :: Person -> [Person]
grandparents p = concat $ parents <$> parents p

maternalGrandfather :: Person -> Maybe Person
maternalGrandfather p = mother p >>= father
