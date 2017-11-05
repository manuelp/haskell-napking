-- http://pchiusano.github.io/2016-10-13/view-inspired.html
-- http://robotlolita.me/2013/04/27/the-hikikomoris-guide-to-javascript.html
module TaskManagement where

data User
  = Public
  | User String

data Card = Card
    { card.dueDate :: Date
    , card.archived :: Bool
    , card.description :: String
    , card.dependencies :: [Card]
    , card.labels :: [String]
    , card.assignees :: [User]
    -- TODO Proper RSBAC/ACL
    , card.readableBy :: [User]
    , card.writableBy :: [User]
    }

class CardView a where
  query :: Set Card -> [Card]
