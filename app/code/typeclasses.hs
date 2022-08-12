class Animal a where
    name :: a -> String
    age :: a -> Int
    say :: a -> String -> String

data Cat = Cat
    { catName :: String
    , catAge :: Int
    , catIsHungry :: Bool
    }

instance Animal Cat where
    name = catName
    age = catAge
    say c str = name c <> " meows: " <> str

data Dog = Dog
    { dogName :: String
    , dogAge :: Int
    , dogIsGoodBoy :: Bool
    }

instance Animal Dog where
    name = dogName
    age = dogAge
    say c str = name c <> " barks: " <> str
