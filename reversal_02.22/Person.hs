--Module with Person data type and automatic instances of Read and Show
module Person where
	data Person = Person {name :: String, age :: Int, weight :: Double} deriving (Eq, Show, Read)