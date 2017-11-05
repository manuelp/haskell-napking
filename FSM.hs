module FSM where

data State
    = InProgress
    | Approved
    | Rejected
    | Deleted
    deriving (Eq,Show)

data Event
    = Create
    | Approve
    | Reject
    | Delete
    deriving (Eq,Show)

transition :: Maybe State -> Event -> Maybe State
transition Nothing Create = Just InProgress
transition (Just InProgress) Approve = Just Approved
transition (Just InProgress) Reject = Just Rejected
transition (Just _) Delete = Just Deleted
transition s _ = Nothing

project :: Maybe State -> [Event] -> Maybe State
project start = foldl transition start

projectAll :: [Event] -> Maybe State
projectAll = project Nothing
