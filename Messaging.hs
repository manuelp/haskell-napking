module WardaMessenger where

-- http://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html
import Data.Text
-- https://two-wrongs.com/haskell-time-library-tutorial
import Data.Time
-- https://github.com/Gabriel439/Haskell-Pipes-Library
import Pipes

--
-- Types
--
newtype User = User String
newtype Group = Group String
data ContentType = PlainText | Html
data Validity = ClosedInterval UTCTime UTCTime
              | OpenInterval UTCTime
  
data Message = Message Validity ContentType Text

data MessageRecipient = ToEveryone
                      | ToUser User
                      | ToGroup Group

--
-- Functions
--
send :: User -> MessageRecipient -> Message -> IO ()
send u ToEveryone m = ...
send u (ToUser ru) m = ...
send u (ToGroup rg) m = ...

-- TODO Pull vs push?
class MessageBoard a where
  send :: User -> MessageRecipient -> Message -> IO ()
  -- Pull-based API
  byUser :: User -> IO [Message]
  byGroup :: Group -> IO [Message]
  -- Push-based (reactive) API
  -- TODO pipes-based streams exposed

-- Use cases:
--
-- * (x) Announces
-- * ( ) Chat? See/unseen? Different use-case!
