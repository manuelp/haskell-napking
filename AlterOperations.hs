module AlterOperations where

type UserId = String
type AlterOperationParams = [(String, String)]

-- Validation and execution are separated phases, and only the
-- execution one is an actual side-effect.
data AlterOperation = AlterOperation
    { validate :: AlterOperationParams -> UserId -> [AlterOperationError]
    , execute :: AlterOperationParams -> UserId -> IO ()
    }

-- The actual execution of an operation performs first the validation
-- phase, then if there isn't any error it's actually executed.
applyOperation :: AlterOperation
               -> AlterOperationParams
               -> UserId
               -> IO [AlterOperationError]
applyOperation op params user =
    let errors = validate op $ params user
    in case null errors of
           False -> return errors
           True -> execute op $ params user

data AlterOperationError = AlterOperationError
    { opErrorType :: String
    , opErrorMessage :: String
    }

-- This formulation is enough for operations where the only feedback
-- we're interested in is:
--
-- - Validation: OK or here are the reasons why the operation cannot be performed.
-- - Execution: OK or here is the runtime exception that I'm throwing in your face.
--
-- This is all well and good, but if we need some more feedback for a
-- successful execution this is not enough. A typical example is the
-- execution of the same kind of (fallible) operation multiple times:
-- we want to try to perform all of them, and return a success even in
-- the presence of some errors. In the latter case, we would like to
-- communicate something like "OK, I've successfully performed 34 of
-- the 45 operations".

-- We can easily modify out `AlterOperation` data type to add this
-- feedback:
data AlterOperation = AlterOperation
    { validate :: AlterOperationParams -> UserId -> [AlterOperationError]
    , execute :: AlterOperationParams -> UserId -> IO String
    }

-- But then every operation has to supply its own success
-- message. This could be acceptable, and for simplicity we're going
-- to assume this. Otherwise we could change the return type of
-- `execute` from `IO String` to `IO (Maybe String)` and have a
-- default value in the application function.

-- With a minor modification of the operation application function, we
-- can communicate either the errors or the execution phase feedback
-- to the outside world.
applyOperation
    :: AlterOperation
    -> AlterOperationParams
    -> UserId
    -> Either [AlterOperationError] (IO String)
applyOperation op params user =
    let errors = validate op $ params user
    in case null errors of
           False -> Left errors
           True -> Right (execute op $ params user)
