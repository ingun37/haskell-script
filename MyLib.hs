module MyLib (onlyMatch) where

-- matchRegexPR gmatchRegexPR
onlyMatch :: Functor m => m ((String, (String, String)), [(Int, String)]) -> m String
onlyMatch = fmap (\((x,_),_) -> x)