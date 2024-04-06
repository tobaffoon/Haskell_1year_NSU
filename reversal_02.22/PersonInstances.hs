{-# LANGUAGE QuasiQuotes #-}
--Module with Person data type and non-automatic instances of Read and Show
module PersonInstances where
	import Text.Regex.PCRE.Heavy
	data Person = Person {name :: String, age :: Int, weight :: Double} deriving Eq
	
	instance Show Person where
		show (Person name age weight) = "name = \"" ++ name ++ "\"\n" 
										++"age = " ++ (show age) ++ "\n"
										++"weight = " ++ (show weight)	
		
	instance Read Person where
		readsPrec _ text = [(newPerson, "")] where 
			f .> g = g . f
				
			string_search :: Regex
			string_search = [re|name = \"(.+?)\"|]
			
			int_search :: Regex
			int_search = [re|age = ([0-9]+)|]
			
			double_search :: Regex
			double_search = [re|weight = ([0-9]+\.[0-9]+)|]
			
			newName :: String
			newName = (scan string_search .> head .> snd .> head) text
			
			newAge :: Int
			newAge = (scan int_search .> head .> snd .> head .> read) text
			
			newWeight :: Double
			newWeight = (scan double_search .> head .> snd .> head .> read) text
			
			newPerson :: Person
			newPerson = Person {name = newName, age = newAge, weight = newWeight}