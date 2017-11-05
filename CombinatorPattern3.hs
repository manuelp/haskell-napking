-- https://www.sitepoint.com/combinator-pattern-with-java-8/
--
-- Really, is just about *composition*. Composing simple things to
-- form more complex things; or, for validations, composing things to
-- form the same thing (semigroup/monoid).
module CombinatorPattern3 where

import Data.Semigroup
import Data.List
import qualified Data.List.NonEmpty as NE

-- We define the validation result as a separated type instead of the
-- usual Either since we need an ad-hoc instance of the Semigroup type
-- class (see below).
--
-- It's parametric on the error type `e`.
data ValidationResult e
    = Success
    | Failure e
    deriving (Eq,Show)

-- The `ValidationResult` forms a semigroup, in this case *all*
-- validations are applied and the failures accumulated. The
-- accumilation made possible by the fact that there is a type
-- constraint on the failure values: they must form a semigroup too.
instance Semigroup e => Semigroup (ValidationResult e) where
  Success <> r = r
  l@(Failure x) <> Success = l
  (Failure x) <> (Failure y) = Failure (x <> y)

-- If we don't want to accumulate the errors but stop at and report the first failure we need to define a different combinator.
andShortCircuit :: ValidationResult e -> ValidationResult e -> ValidationResult e
andShortCircuit Success r = r
andShortCircuit l@(Failure x) _ = l

-- We could also define two new type classes, or a single type class
-- with two combinations, for the two types of composition.

-- Combining two predicates that returns a `ValidationResult` just
-- means applying both of them and combining the results using the
-- Semigroup binary operation.
--
-- Using the primitives defined and using the semigroup combinator has
-- the nice property that we can represent failures both as strings
-- (that can be combined with string concatenation) and as list of
-- values (strings, error values, etc).
andP :: Semigroup e => (a -> ValidationResult e) -> (a -> ValidationResult e) -> (a -> ValidationResult e)
andP f g = \x -> (f x) <> (g x)

-- We can also combine and arbitrary number of predicates (stopping at the first failure).
andAllP :: Semigroup e => NE.NonEmpty (a -> ValidationResult e) -> (a -> ValidationResult e)
andAllP ps = \x -> sconcat $ NE.map (\v -> v x) ps

-- We can also convert an arbitrary error value to a NonEmpty list to
-- accumulate the results in another NonEmpty list (this way the can
-- choose if and how to combine the failures externally: SRP!).
nelError :: Semigroup e => ValidationResult e -> ValidationResult (NE.NonEmpty e)
nelError Success = Success
nelError (Failure x) = Failure (x NE.:| [])

-- Using that function, we can convert a predicate to a
-- ValidationResult with the possible error value in a NonEmpty list.
toNelError :: Semigroup e => (a -> ValidationResult e) -> (a -> ValidationResult (NE.NonEmpty e))
toNelError v = nelError . v

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
lamia = User "Lamia Selena Augustus" "lamia.augustus"

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

-- Using strings as error values, we get automatic accumulation of
-- failures via string concatenation.
e7 = andAllP (NE.fromList [validEmailAddress, twoWords]) $ lamia
-- e7 = Failure ["Invalid email address","Invalid number of words"]

-- But we can also accumulate all errors in an NonEmpty list
e8 =
    let predicates = map toNelError [validEmailAddress, twoWords]
    in andAllP (NE.fromList predicates) $ lamia
-- e8 = Failure ("Invalid email address" :| ["Invalid number of words"])

-- But what is the right and most general data type to do validation?
-- What is the relationship with a more general [error handling
-- approach](http://fsharpforfunandprofit.com/rop)? How to compose
-- data types with functions that compose them in various ways? For
-- example: AND (short-circuiting), accumulate failures, accumulate
-- both successes and failures, etc.
--
-- Error handling generally is concerned with stopping at and
-- reporting the first error, but here we have an additional
-- requirement: we want to go on and accumulate _all_ errors. This is
-- an additional type of *composition*.

-- See also: https://hackage.haskell.org/package/validation
-- For, perhaps, the last step of the ladder :)
