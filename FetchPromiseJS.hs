-- https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise
module FetchPromiseJS where

--
-- PROMISE
--
data Promise s f =
    Promise (() -> s)
            (() -> f)

-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then
promiseThen :: Promise s f -> Maybe (s -> ()) -> Maybe (f -> ())

--
-- FETCH
--
type URL = String
type Object = [(String, String)]
type Config = Object

data Response = Response
    { ok :: Bool
    , status :: Integer
    , statusText :: String
    }

fetch :: URL -> Config -> Promise Response ()
json :: Promise Response f -> Promise Object f
text :: Promise Response f -> Promise String f

