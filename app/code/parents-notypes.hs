data Person = Person {name :: String, father :: Maybe Person, mother :: Maybe Person}


parents p = [x | Just x <- [father p, mother p]]


grandparents p = concat $ parents <$> parents p


maternalGrandfather p = mother p >>= father
