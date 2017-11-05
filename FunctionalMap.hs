module FunctionalMap where

import qualified Data.Map as Map
import Data.Char

data FunctionalMap a b =
  FunctionalMap (Map.Map String (a -> b))

applyFunction :: FunctionalMap a b -> String -> a -> Maybe b
applyFunction (FunctionalMap m) name i = fmap (\f -> f i) $ (Map.lookup name m)

-- Example
stringUtils =
  FunctionalMap $
  Map.fromList [("uppercase", map toUpper), ("lowercase", map toLower)]

e1 = applyFunction stringUtils "uppercase" "prova"
-- e1 = Just "PROVA"
e2 = applyFunction stringUtils "emphasize" "prova"
-- e2 = Nothing
e3 = fromMaybe "LOL" e1
-- e3 = "PROVA"
e4 = fromMaybe "LOL" e2
-- e4 = "LOL"
