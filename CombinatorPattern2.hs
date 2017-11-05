-- https://www.sitepoint.com/combinator-pattern-with-java-8/
--
-- Really, is just about *composition*. Composing simple things to
-- form more complex things; or, for validations, composing things to
-- form the same thing (semigroup/monoid).
module CombinatorPattern2 where

import Data.Semigroup
import Data.List
import qualified Data.List.NonEmpty as NE

-- We define the validation result as a separated type instead of the
-- usual Either since we need an ad-hoc instance of the Semigroup type
-- class (see below).
data ValidationResult
    = Success
    | Failure String
    deriving (Eq,Show)

-- Either is an instance of Semigroup, but the combinator
-- short-circuits on the *successful* value. In this case, we want the
-- opposite behaviour: short-circuiting on the *failure* one.
instance Semigroup ValidationResult where
  Success <> r = r
  l@(Failure s) <> _ = l

-- Combining two predicates that returns a `ValidationResult` just
-- means applying both of them and combining the results using the
-- Semigroup binary operation.
andP :: (a -> ValidationResult) -> (a -> ValidationResult) -> (a -> ValidationResult)
andP f g = \x -> (f x) <> (g x)

-- We can also combine and arbitrary number of predicates (stopping at the first failure).
andAllP :: NE.NonEmpty (a -> ValidationResult) -> (a -> ValidationResult)
andAllP ps = \x -> sconcat $ NE.map (\v -> v x) ps

-- Combining an arbitrary number of predicates and *accumulating the
-- failures, instead, is a different story. In this case we don't want
-- to short-circuit at the first Failure, but gather all of
-- them. Where? How? Here another Semigroup is very useful :P allP ::
-- [(a -> ValidationResult)] -> ?

--
-- Examples
--
data User = User
    { name :: String
    , email :: String
    } deriving (Eq,Show)

manuel = User "Manuel Paccagnella" "manuel.paccagnella@gmail.com"
lisa = User "Lisa Masala" "lisa.masala@sardinia.it"
tony = User "Tony Stark" "tony.stark"
giovanni = User "Giovanni Paolo Giuseppe Del Granaio Di Malatesta" "gpgdgdm@gmail.com"

validEmailAddress =
    \u@(User _ e) ->
         case (isInfixOf "@" e) of
             True -> Success
             False -> Failure "Invalid email address"

twoWords =
    \u@(User n _) ->
         case (length . words $ n) of
             2 -> Success
             _ -> Failure "Invalid number of words"

e1 = validEmailAddress manuel
-- e1 = Success
e2 = validEmailAddress tony
-- e2 = Failure "Invalid email address"
e3 = andP validEmailAddress twoWords $ manuel
-- e3 = Success
e4 = andP validEmailAddress twoWords $ tony
-- e4 = Failure "Invalid email address"
e5 = andP validEmailAddress twoWords $ giovanni
-- e5 = Failure "Invalid number of words"
e6 = andAllP (NE.fromList [validEmailAddress, twoWords]) $ tony
-- e6 = Failure "Invalid email address"
