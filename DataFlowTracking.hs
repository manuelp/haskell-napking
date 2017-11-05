module DataFlowTracking where

import Data.Time.Clock.POSIX
import Data.UUID

type Json = String
class Jsonable a where
    toJson :: a -> Json
    fromJson :: Json -> a

-- Existential data constructor
-- See: https://wiki.haskell.org/Heterogenous_collections
data TrackableVal = forall a. Jsonable a => TrackableVal a

type CorrelationId = UUID
type UserId = String
type Timestamp = POSIXTime
data FlowId = FlowId CorrelationId UserId Timestamp
data DataFlow = DataFlow FlowId [TrackableVal]

appendValue :: DataFlow -> TrackableVal -> DataFlow
appendValue id v = appendValues id [v]

appendValues :: DataFlow -> [TrackableVal] -> DataFlow
appendValues (DataFlow id xs) vs = DataFlow id (vs ++ xs)

-- ----------
-- Command
-- ----------

class DataFlowWriteModel a where
    saveFlow :: DataFlow -> IO ()

-- ----------
-- Query
-- ----------

data FlowFilter = CorrelationFilter CorrelationId
                | UserFilter UserId
                | TimeFilter Timestamp (Maybe Timestamp)
                | ValueFilter Integer (Json -> Boolean)

class DataFlowReadModel a where
    findFlows :: [FlowFilter] -> [DataFlow]
