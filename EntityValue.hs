module EntityValue where

type Entity i v = Entity i v

-- CRUD!
class Repository i v where
  create :: v -> IO i
  read :: i -> IO (Maybe (Entity i v))
  update :: Entity i v -> IO ()
  delete :: i -> IO ()

--
-- Example
--
data PersonID = UUID
data Person = Person
  { personName :: String
  , personAge :: Int
  , height :: Int
  }

instance Repository PersonID Person where
  create :: Person -> IO PersonID
  create p = undefined
  --
  update :: Entity PersonID Person -> IO ()
  update = undefined
  --
  delete :: PersonID -> IO ()
  delete = undefined
  -- 
  read :: PersonID -> IO (Maybe (Entity PersonID Person))
  read id = undefined

-- Things are more complicated with linked entities. We're
-- reimplementing JPA.
