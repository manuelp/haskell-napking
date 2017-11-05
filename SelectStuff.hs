module SelectStuff where

data Entry a = Entry
  { id :: UUID
  , description :: String
  , value :: a
  }

newtype UUID =
  UUID String    

-- Custom
class Selectable a where
  fetchLike :: String -> [a]
 
-- Generic
toEntry :: a -> Entry a
toEntry = undefined

getSelected :: [Entry a] -> UUID -> Maybe a
getSelected = undefined


