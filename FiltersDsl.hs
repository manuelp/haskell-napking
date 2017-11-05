-- http://composition.al/blog/2013/06/23/write-an-interpreter/
-- https://blog.cppcabrera.com/posts/56-writing-a-search-dsl-1.html
-- http://stackoverflow.com/questions/16970431/implementing-a-language-interpreter-in-haskell
-- https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf

module FiltersDsl where

import Data.List as DL

-- For convenience and clarity
type FilterName = String

-- Our current filter values
data FilterValue
  = StringFilter FilterName
                 String
  | BooleanFilter FilterName
                  Bool
  | IntFilter FilterName
              Int
  | ArrayFilter FilterName
                [String]
    -- This is opinable...
    -- |
    --   DateFilter { name :: FilterName
    --              , before :: Maybe Date
    --              , after :: Maybe Date
    --              , on :: Maybe Date}
  deriving (Show)

{-
A language is made of expressions. It's defined by scalar expressions
(values) and combinators ones (composition). It forms a tree.
-}
data Filter
    = Filter FilterValue
    | And Filter
          Filter
    | Or Filter
         Filter
    deriving (Show)

{-
The interpretation of this language yelds some result. The type of the
result depends on the _context_ (SQL queries, etc).

interpret :: Filter -> a
-}

--
-- EXAMPLE context: simple strings
--
newtype SampleLanguage =
  SampleLanguage String

instance Show SampleLanguage where
  show (SampleLanguage x) = x

interpret :: Filter -> SampleLanguage
interpret (Filter (StringFilter n v)) = SampleLanguage $ n ++ "=" ++ v
interpret (Filter (BooleanFilter n v)) = SampleLanguage $ n ++ "=" ++ show v
interpret (Filter (IntFilter n v)) = SampleLanguage $ n ++ "=" ++ show v
interpret (Filter (ArrayFilter n vs)) =
  let commified = concat (DL.intersperse "," vs)
  in SampleLanguage $ n ++ " in [" ++ commified ++ "]"
interpret (And x y) =
  SampleLanguage $
  "(" ++ (show . interpret $ x) ++ " and " ++ (show . interpret $ y) ++ ")"
interpret (Or x y) =
  SampleLanguage $
  "(" ++ (show . interpret $ x) ++ " or " ++ (show . interpret $ y) ++ ")"

a = Filter $ StringFilter "a" "12"
b = Filter $ BooleanFilter "b" True
c = Filter $ IntFilter "c" 5
d = Filter $ ArrayFilter "d" ["hello", "filters", "world"]
e = And a b
f = Or b c
g = And (Or a c) (And b d)
