module Location where

data Location
  = Somewhere
  | At {
    locationFile     :: String,
    locationPosition :: (Int, Int)
  }

instance Show Location where
  show (At f (l, c))
    = "`" ++ f ++ "' " ++ show l ++ ":" ++ show c

data Located a = Located {
    location    :: Location,
    fromLocated :: a
  } deriving (Show)

instance Functor Located where
  fmap f (Located at x) = Located at $ f x
