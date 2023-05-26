{-# LANGUAGE CPP #-}
module EventParser
    (
      parseLogHeader
    , parseDataHeader
    , parseEvents
    , Counter (..)
    , Location (..)
    , Event (..)
    )
where

-- import Debug.Trace
import Data.Char (ord, chr)
import Data.IntMap (IntMap)
import Data.Word (Word8, Word32, Word64)
import Streamly.Data.Array (Array)
import Streamly.Data.Parser (Parser)
import Streamly.Data.ParserK (ParserK)
import Streamly.Data.Stream (Stream)
import Streamly.Data.StreamK (StreamK)
import Streamly.Internal.Serialize.FromBytes (int16be, word16be, word32be, word64be)

import qualified Data.IntMap as Map
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
-- import qualified Streamly.Internal.Data.Fold.Container as Fold (toContainer)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.StreamK as StreamK
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.ParserK as ParserK

{-
#define EVENT_HEADER_BEGIN    0x68647262
#define EVENT_HEADER_END      0x68647265
#define EVENT_DATA_BEGIN      0x64617462
#define EVENT_DATA_END        0xffff
#define EVENT_HET_BEGIN       0x68657462
#define EVENT_HET_END         0x68657465
#define EVENT_ET_BEGIN        0x65746200
#define EVENT_ET_END          0x65746500
-}

w8 :: String -> [Word8]
w8 = map (fromIntegral . ord)

---------------------
-- Event Type
---------------------

hetBegin :: [Word8]
hetBegin = w8 "hetb"

hetEnd :: [Word8]
hetEnd = w8 "hete"

eventTypeBegin :: [Word8]
eventTypeBegin = w8 "etb\0"

eventTypeEnd :: [Word8]
eventTypeEnd = w8 "ete\0"

--  *       EVENT_ET_BEGIN
--  *       Word16         -- unique identifier for this event
--  *       Int16          -- >=0  size of the event in bytes (minus the header)
--  *                      -- -1   variable size
--  *       Word32         -- length of the next field in bytes
--  *       Word8*         -- string describing the event
--  *       Word32         -- length of the next field in bytes
--  *       Word8*         -- extra info (for future extensions)
--  *       EVENT_ET_END
-- (event Id, event size)
eventType :: ParserK Word8 IO (Int, Int)
eventType = do
    _ <- ParserK.fromParser (Parser.listEq eventTypeBegin)
    eventId <- ParserK.fromParser word16be
    -- ParserK.fromEffect $ print $ "Event id =" ++ show eventId
    eventSize <- ParserK.fromParser int16be
    -- ParserK.fromEffect $ print $ "Event size =" ++ show eventSize
    descLen <- ParserK.fromParser word32be
    _desc <- ParserK.fromParser (Parser.takeEQ (fromIntegral descLen) Fold.toList)
    -- ParserK.fromEffect $ putStrLn (map (chr . fromIntegral) desc)
    infoLen <- ParserK.fromParser word32be
    -- ParserK.fromEffect $ print $ "info len =" ++ show infoLen
    ParserK.fromParser (Parser.takeEQ (fromIntegral infoLen) Fold.drain)
    _ <- ParserK.fromParser (Parser.listEq eventTypeEnd)
    return (fromIntegral eventId, fromIntegral eventSize)

---------------------
-- Header
---------------------

headerBegin :: [Word8]
headerBegin = w8 "hdrb"

headerEnd :: [Word8]
headerEnd = w8 "hdre"

{-
{-# INLINE kvToMap #-}
kvToMap :: (Monad m) => Fold m a b -> Fold m (Int, a) (IntMap b)
kvToMap = Fold.toContainer fst . Fold.lmap snd
-}

parseEventTypes ::
       Map.IntMap Int
    -> StreamK IO (Array Word8)
    -> IO (Map.IntMap Int, StreamK IO (Array Word8))
parseEventTypes kv stream = do
    -- putStrLn "parsing eventType"
    (r, rest) <- StreamK.parseBreakChunks eventType stream
    case r of
        -- XXX When the parser fails the original stream should not change, we
        -- should be able to use "rest" here.
        Left _ -> return (kv, stream)
        Right (eventId, eventSize) -> do
            let kv1 = Map.insert eventId  eventSize kv
            parseEventTypes kv1 rest

headerPre :: ParserK Word8 IO ()
headerPre = do
    _ <- ParserK.fromParser (Parser.listEq headerBegin)
    _ <- ParserK.fromParser (Parser.listEq hetBegin)
    return ()

headerPost :: ParserK Word8 IO ()
headerPost = do
    _ <- ParserK.fromParser (Parser.listEq hetEnd)
    _ <- ParserK.fromParser (Parser.listEq headerEnd)
    return ()

{-
header :: ParserK Word8 IO (Map.IntMap Int)
header = do
    ParserK.fromParser (Parser.listEq headerBegin)
    ParserK.fromParser (Parser.listEq hetBegin)
    -- r <- ParserK.fromParser (Parser.many eventType (kvToMap Fold.one))
    r <- parseEventTypes
    ParserK.fromParser (Parser.listEq hetEnd)
    ParserK.fromParser (Parser.listEq headerEnd)
    return r
-}

-- XXX We can create a Parser monad based on parseBreak to compose parsers in a
-- chain, we can also create an Alternative instance for that.
parseLogHeader ::
    StreamK IO (Array Word8) -> IO (Map.IntMap Int, StreamK IO (Array Word8))
parseLogHeader stream = do
    (res, rest) <- StreamK.parseBreakChunks headerPre stream
    case res of
        Left err -> fail $ show err
        Right () -> do
            (kv, rest1) <- parseEventTypes Map.empty rest
            (res1, rest2) <- StreamK.parseBreakChunks headerPost rest1
            case res1 of
                Left err -> putStrLn "header end not found" >> (fail $ show err)
                Right () -> return (kv, rest2)

---------------------
-- Parse events
---------------------

dataBegin :: [Word8]
dataBegin = w8 "datb"

-- dataEnd :: [Word8]
-- dataEnd = [0xff, 0xff]

parseDataHeader :: StreamK IO (Array Word8) -> IO (StreamK IO (Array Word8))
parseDataHeader stream = do
    let p = ParserK.fromParser (Parser.listEq dataBegin)
    (res, rest) <- StreamK.parseBreakChunks p stream
    case res of
        Left err -> fail $ show err
        Right _ -> return rest

#define EVENT_RUN_THREAD           1 /* (thread)               */
#define EVENT_STOP_THREAD          2 /* (thread, status, blockinfo) */
#define EVENT_USER_MSG            19 /* (message ...)          */

#define EVENT_PRE_THREAD_CLOCK           200
#define EVENT_POST_THREAD_CLOCK          201
#define EVENT_PRE_THREAD_PAGE_FAULTS     202
#define EVENT_POST_THREAD_PAGE_FAULTS    203
#define EVENT_PRE_THREAD_CTX_SWITCHES    204
#define EVENT_POST_THREAD_CTX_SWITCHES   205
#define EVENT_PRE_THREAD_ALLOCATED       206
#define EVENT_POST_THREAD_ALLOCATED      207
#define EVENT_PRE_HW_CACHE_L1I           208
#define EVENT_POST_HW_CACHE_L1I          209
#define EVENT_PRE_HW_CACHE_L1I_MISS      210
#define EVENT_POST_HW_CACHE_L1I_MISS     211
#define EVENT_PRE_HW_CACHE_L1D           212
#define EVENT_POST_HW_CACHE_L1D          213
#define EVENT_PRE_HW_CACHE_L1D_MISS      214
#define EVENT_POST_HW_CACHE_L1D_MISS     215
#define EVENT_PRE_HW_CACHE_MISSES        216
#define EVENT_POST_HW_CACHE_MISSES       217
#define EVENT_PRE_HW_INSTRUCTIONS        218
#define EVENT_POST_HW_INSTRUCTIONS       219
#define EVENT_PRE_HW_BRANCH_MISSES       220
#define EVENT_POST_HW_BRANCH_MISSES      221
#define EVENT_PRE_THREAD_CPU_MIGRATIONS  222
#define EVENT_POST_THREAD_CPU_MIGRATIONS 223

-- XXX We attach a user event to a thread by looking at the previous thread
-- start event. But when there are multiple capabilities this may not be
-- possible? We need to use the thread-id on the same capability as the user
-- event. Or we can emit the tid in the user event. How does ghc-events-analyze
-- handle this? or the user event can log the thread-id as part of the tag.

data Counter =
      ThreadCPUTime
    | ThreadCtxVoluntary
    | ThreadPageFaultMinor
    | ThreadAllocated
    | L1iCacheHit
    | L1iCacheMiss
    | L1dCacheHit
    | L1dCacheMiss
    | ThreadCPUMigrations
    | BranchMisses
    | Instructions
    | LastLevelCacheMisses

    deriving (Show, Eq, Ord)

-- data Location = Enter | Exit | Resume | Suspend deriving Show
data Location = Resume | Suspend | Exit deriving Show

-- Event tid window counter start/stop value
data Event = Event Word32 String Counter Location Word64 deriving Show

-- XXX We can ensure that the required size of data is compacted into the next
-- array and then use a more efficient single array parser which only passes
-- around pos instead of the array.
--
-- XXX We should use a parseManyK

--  * Event :
--  *       Word16         -- event_type
--  *       Word64         -- time (nanosecs)
--  *       [Word16]       -- length of the rest (for variable-sized events only)
--  *       ... extra event-specific info ...
{-# INLINE event #-}
event :: IntMap Int -> Parser Word8 IO (Maybe Event)
event kv = do
    eventId <- word16be
    ts <- word64be
    -- XXX This does not get executed if we error out
    -- trace ("event = " ++ show eventId ++ " ts = " ++ show ts) (return ())
    case eventId of
        {-
        EVENT_RUN_THREAD -> do
            tid <- word32be
            return $ StartThreadCPUTimeWall tid ts
        EVENT_STOP_THREAD -> do
            tid <- word32be
            -- XXX Stop thread has two more fields
            return $ StopThreadCPUTimeWall tid ts
        -}
        EVENT_USER_MSG -> do
            len <- word16be
            tid <- word32be
            ctrType <- word16be
            msg <- Parser.takeEQ
                        (fromIntegral (len - 6))
                        (Fold.lmap (chr . fromIntegral) Fold.toList)
            let (loc, rest) = span (/= ':') msg
                tag = drop 1 rest
            -- Parser.fromEffect $ putStrLn $ "tid = " ++ show tid1 ++ " loc = " ++ loc ++ " tag = " ++ tag
            let counterId =
                    case ctrType of
                        EVENT_PRE_THREAD_CLOCK -> ThreadCPUTime
                        EVENT_PRE_THREAD_ALLOCATED -> ThreadAllocated
                        EVENT_PRE_HW_CACHE_L1I -> L1iCacheHit
                        EVENT_PRE_HW_CACHE_L1I_MISS -> L1iCacheMiss
                        EVENT_PRE_HW_CACHE_L1D -> L1dCacheHit
                        EVENT_PRE_HW_CACHE_L1D_MISS -> L1dCacheMiss
                        EVENT_PRE_THREAD_PAGE_FAULTS -> ThreadPageFaultMinor
                        EVENT_PRE_THREAD_CTX_SWITCHES -> ThreadCtxVoluntary
                        EVENT_PRE_HW_CACHE_MISSES -> LastLevelCacheMisses
                        EVENT_PRE_HW_INSTRUCTIONS -> Instructions
                        EVENT_PRE_HW_BRANCH_MISSES -> BranchMisses
                        EVENT_PRE_THREAD_CPU_MIGRATIONS -> ThreadCPUMigrations
                        
                        _ -> error "Invalid event in user window"
            case loc of
                "START" -> do
                    let ev = Event tid tag counterId Resume ts
                    -- trace ("Parsed: = " ++ show ev) (return ())
                    return $ Just ev
                "END" -> do
                    let ev = Event tid tag counterId Suspend ts
                    -- trace ("Parsed: = " ++ show ev) (return ())
                    return $ Just ev
                _ -> error $ "Invalid window location tag: " ++ loc
        EVENT_PRE_THREAD_CLOCK -> do
            tid <- word32be
            -- Parser.fromEffect $ putStr $ "event = " ++ show eventId ++ " ts = " ++ show ts
            -- Parser.fromEffect $ putStrLn $ " tid = " ++ show tid
            -- trace ("event = " ++ show eventId ++ " ts = " ++ show ts) (return ())
            return $ Just $ Event tid "" ThreadCPUTime Resume ts
        EVENT_POST_THREAD_CLOCK -> do
            tid <- word32be
            -- Parser.fromEffect $ putStr $ "event = " ++ show eventId ++ " ts = " ++ show ts
            -- Parser.fromEffect $ putStrLn $ " tid = " ++ show tid
            -- trace ("event = " ++ show eventId ++ " ts = " ++ show ts) (return ())
            return $ Just $ Event tid "" ThreadCPUTime Suspend ts
        EVENT_PRE_THREAD_CTX_SWITCHES -> do
            tid <- word32be
            return $ Just $ Event tid "" ThreadCtxVoluntary Resume ts
        EVENT_POST_THREAD_CTX_SWITCHES -> do
            tid <- word32be
            return $ Just $ Event tid "" ThreadCtxVoluntary Suspend ts
        EVENT_PRE_THREAD_PAGE_FAULTS -> do
            tid <- word32be
            return $ Just $ Event tid "" ThreadPageFaultMinor Resume ts
        EVENT_POST_THREAD_PAGE_FAULTS -> do
            tid <- word32be
            return $ Just $ Event tid "" ThreadPageFaultMinor Suspend ts
        EVENT_PRE_THREAD_ALLOCATED -> do
            tid <- word32be
            -- trace ("tid = " ++ show tid ++ " event = " ++ show eventId ++ " ts = " ++ show ts) (return ())
            return $ Just $ Event tid "" ThreadAllocated Resume ts
        EVENT_POST_THREAD_ALLOCATED -> do
            tid <- word32be
            -- trace ("tid = " ++ show tid ++ " event = " ++ show eventId ++ " ts = " ++ show ts) (return ())
            return $ Just $ Event tid "" ThreadAllocated Suspend ts
        EVENT_PRE_HW_CACHE_L1I -> do
            tid <- word32be
            return $ Just $ Event tid "" L1iCacheHit Resume ts
        EVENT_POST_HW_CACHE_L1I -> do
            tid <- word32be
            return $ Just $ Event tid "" L1iCacheHit Suspend ts
        EVENT_PRE_HW_CACHE_L1D -> do
            tid <- word32be
            return $ Just $ Event tid "" L1dCacheHit Resume ts
        EVENT_POST_HW_CACHE_L1D -> do
            tid <- word32be
            return $ Just $ Event tid "" L1dCacheHit Suspend ts
        EVENT_PRE_HW_CACHE_L1I_MISS -> do
            tid <- word32be
            return $ Just $ Event tid "" L1iCacheMiss Resume ts
        EVENT_POST_HW_CACHE_L1I_MISS -> do
            tid <- word32be
            return $ Just $ Event tid "" L1iCacheMiss Suspend ts
        EVENT_PRE_HW_CACHE_L1D_MISS -> do
            tid <- word32be
            return $ Just $ Event tid "" L1dCacheMiss Resume ts
        EVENT_POST_HW_CACHE_L1D_MISS -> do
            tid <- word32be
            return $ Just $ Event tid "" L1dCacheMiss Suspend ts
        EVENT_PRE_HW_CACHE_MISSES -> do
            tid <- word32be
            return $ Just $ Event tid "" LastLevelCacheMisses Resume ts
        EVENT_POST_HW_CACHE_MISSES -> do
            tid <- word32be
            return $ Just $ Event tid "" LastLevelCacheMisses Suspend ts
        EVENT_PRE_HW_INSTRUCTIONS -> do
            tid <- word32be
            return $ Just $ Event tid "" Instructions Resume ts
        EVENT_POST_HW_INSTRUCTIONS -> do
            tid <- word32be
            return $ Just $ Event tid "" Instructions Suspend ts
        EVENT_PRE_HW_BRANCH_MISSES -> do
            tid <- word32be
            return $ Just $ Event tid "" BranchMisses Resume ts
        EVENT_POST_HW_BRANCH_MISSES -> do
            tid <- word32be
            return $ Just $ Event tid "" BranchMisses Suspend ts
        EVENT_PRE_THREAD_CPU_MIGRATIONS -> do
            tid <- word32be
            return $ Just $ Event tid "" ThreadCPUMigrations Resume ts
        EVENT_POST_THREAD_CPU_MIGRATIONS -> do
            tid <- word32be
            return $ Just $ Event tid "" ThreadCPUMigrations Suspend ts
        _ -> do
            -- Parser.fromEffect $ putStrLn ""
            let r = Map.lookup (fromIntegral eventId) kv
            len <-
                case r of
                    Just x ->
                        if x == -1
                        then fmap fromIntegral word16be
                        else return x
                    Nothing -> error $ "Event not in the header: " ++ show eventId
            _ <- Parser.takeEQ len Fold.drain
            return Nothing

{-# INLINE parseEvents #-}
parseEvents :: IntMap Int -> StreamK IO (Array Word8) -> Stream IO Event
parseEvents kv =
      Stream.catMaybes
    . Stream.catRights
    . Stream.parseMany (event kv)
    . Stream.unfoldMany Array.reader
    . StreamK.toStream
