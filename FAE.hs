-- https://fate-srd.com/fate-accelerated/example-characters
module FAE where

data FAESheet = FAESheet
  { sheetName :: Name
  , sheetConcept :: HighConcept
  , sheetTrouble :: Trouble
  , sheetAspects :: [Aspect]
  , sheetApproaches :: Approaches
  , sheetStunts :: [Stunt]
  , sheetRefresh :: Refresh
  }

newtype Name =
  Name String
newtype HighConcept =
  HighConcept String
  deriving (Show)
newtype Trouble =
  Trouble String
  deriving (Show)
newtype Aspect =
  Aspect String
  deriving (Show)

data Approaches = Approaches
  { approachCareful :: ScaleValue
  , approachClever :: ScaleValue
  , approachFlashy :: ScaleValue
  , approachForceful :: ScaleValue
  , approachQuick :: ScaleValue
  , approachSneaky :: ScaleValue
  } deriving (Show)

newtype Stunt =
  Stunt String
  deriving (Show)

newtype Refresh =
  Refresh Int
  deriving (Eq, Show)

data ScaleValue
  = Legendary
  | Epic
  | Fantastic
  | Superb
  | Great
  | Good
  | Fair
  | Average
  | Mediocre
  | Poor
  | Terrible
  deriving (Show, Eq)

scaleFateDice :: ScaleValue -> Int
scaleFateDice Legendary = 8
scaleFateDice Epic = 7
scaleFateDice Fantastic = 6
scaleFateDice Superb = 5
scaleFateDice Great = 4
scaleFateDice Good = 3
scaleFateDice Fair = 2
scaleFateDice Average = 1
scaleFateDice Mediocre = 0
scaleFateDice Poor = -1
scaleFateDice Terrible = -2
