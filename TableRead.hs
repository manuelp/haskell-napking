module TableRead where

import Data.Maybe (fromJust, isJust)

data Row =
  Row [Cell]
data Cell =
  Cell CellName
       Object
newtype CellName =
  CellName String

foo :: [(ResultSet -> Maybe Cell)] -> ResultSet -> Row
foo readers rs = map fromJust . filter isJust $ apply [rs] readers
