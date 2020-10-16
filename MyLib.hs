module MyLib (onlyMatch, onlyGroups, onlyGroup) where

-- matchRegexPR gmatchRegexPR
onlyMatch :: Functor m => m ((String, (String, String)), [(Int, String)]) -> m String
onlyMatch = fmap (fst . fst)

onlyGroups :: Functor m => m ((String, (String, String)), [(Int, String)]) -> m [String]
onlyGroups = fmap ((map snd) . snd)

onlyGroup :: Functor m => Int -> m ((String, (String, String)), [(Int, String)]) -> m String
onlyGroup groupIdx = fmap (!! groupIdx) . onlyGroups