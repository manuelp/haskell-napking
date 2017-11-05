module ConfunionPatch where

-- https://wiki.haskell.org/Heterogenous_collections
-- https://hackage.haskell.org/package/hedn

newtype ParamName = ParamName String
data Param a = Param
    { name :: ParamName
    , value :: a
    }
data Config =
    forall a. Config [Param a]

data Patch a
    = Add (Param a)
    | Delete ParamName

change :: Param a -> [Patch a]
change param = [Delete (name param), Add param]

apply :: forall a . Param a -> Config a -> Config a

project :: forall a . Stream -> Config
project (Stream []) = Config []
project (Stream (p:ps)) = undefined 

reverse :: Patch a -> Patch a
reverse (Add param) = Delete . name $ param
reverse (Delete name) = undefined
