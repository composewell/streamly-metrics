module Streamly.Metrics.Channel
    (
    )
where

import Streamly.KeyValue.Type
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Array.Foreign (Array)
import Streamly.Internal.Data.Time.Units (AbsTime)
import Data.Word (Word64)

-------------------------------------------------------------------------------
-- Event processing
-------------------------------------------------------------------------------

data Channel k v = Channel

send :: Channel k v -> KeyValue k v -> m ()
send = undefined

-- | Log a string.
log :: String -> m String
log = undefined

