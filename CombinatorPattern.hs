-- https://www.sitepoint.com/combinator-pattern-with-java-8/
module CombinatorPattern where

import Data.Semigroup
import Data.List

data User = User
  { name :: String
  , email :: String
  } deriving (Eq, Show)

data UserValidation =
  UserValidation (User -> Either String User)

-- Semigroup has the wrong logic for Either: picks the first Right
-- value that it finds:
--
-- instance Semigroup (Either a b) where
--  Left _ <> b = b
--  a      <> _ = a
--  stimes = stimesIdempotent
--
-- Interesting is that (->) also forms a Semigroup:
--
-- instance Semigroup b => Semigroup (a -> b) where
--  f <> g = \a -> f a <> g a
--  stimes n f e = stimes n (f e)
instance Semigroup UserValidation where
  (UserValidation v1) <> (UserValidation v2) =
    UserValidation (\u -> v1 u <> v2 u)

applyValidation :: UserValidation -> User -> Either String User
applyValidation (UserValidation v) = v

-- :i (->)
-- applicative? functor? monad? monoid? semigroup?

manuel = User "Manuel Paccagnella" "manuel.paccagnella@gmail.com"
lisa = User "Lisa Masala" "lisa.masala@sardinia.it"
tony = User "Tony Stark" "tony.stark"
giovanni = User "Giovanni Paolo Giuseppe Del Granaio Di Malatesta" "gpgdgdm@gmail.com"

validEmailAddress =
    UserValidation
        (\u@(User _ e) ->
              case (isInfixOf "@" e) of
                  True -> Right u
                  False -> Left "Invalid email address")

twoWords =
    UserValidation
        (\u@(User n _) ->
              case (length . words $ n) of
                  2 -> Right u
                  _ -> Left "Invalid number of words")

--
-- Really, the general pattern here is this:
--
applyV :: Semigroup b => (a -> b) -> (a -> b) -> a -> b
applyV f g = \x -> (f x) <> (g x)

foo :: User -> Either String User
foo u@(User _ e) =
    case (isInfixOf "@" e) of
        True -> Right u
        False -> Left "Invalid email address"

bar :: User -> Either String User
bar u@(User n _) =
    case (length . words $ n) of
        2 -> Right u
        _ -> Left "Invalid number of words"

-- applyAll :: Semigroup b => [(a -> b)] -> (a -> b)
-- applyAll fs = \x -> ?
