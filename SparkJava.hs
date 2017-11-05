module SparkJava where

type Route = (Request, Response) -> Response

retrieveUserFromSession :: Route -> Either Response (Route, User)
retrieveUserFromSession = undefined

buildCommand :: (Route, User) -> Either Response (Route, Command a)
buildCommand = undefined

executeCommand :: (Route, Command a) -> Response
executeCommand = undefined

data Command a
  = PlainCommand (Validation Error (IO ()))
  | CommandWithResult (Validation Error (IO a))

type Validation e a = Either e a

-- Not true, but we just care about function signatures and
-- composition.
type Request = String;
type Response = String;
type Error = String
type User = String;
