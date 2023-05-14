{-# LANGUAGE CPP #-}
module EventParser
    (
      parseLogHeader
    , parseDataHeader
    , parseEvents
    , Event (..)
    )
where

import Data.Char (ord)
import Data.IntMap (IntMap)
import Data.Word (Word8, Word16, Word32, Word64)
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

#define EVENT_PRE_RUN_THREAD           200
#define EVENT_POST_RUN_THREAD          201
#define EVENT_THREAD_PAGE_FAULTS       202
#define EVENT_THREAD_CTX_SWITCHES      203
#define EVENT_THREAD_IO_BLOCKS         204
#define EVENT_PRE_RUN_THREAD_RU        205
#define EVENT_POST_RUN_THREAD_RU       206
#define EVENT_PRE_RUN_THREAD_USER      207
#define EVENT_PRE_RUN_THREAD_SYSTEM    208
#define EVENT_POST_RUN_THREAD_USER     209
#define EVENT_POST_RUN_THREAD_SYSTEM   210

data Event =
  -- RTS thread start/stop events
    StartThreadCPUTime Word32 Word64 -- tid, cputime
  | StopThreadCPUTime Word32 Word64

  -- User defined window events
  | StartWindowCPUTime Word32 String Word64 -- tid, windowtag, cputime
  | StopWindowCPUTime Word32 String Word64

  -- Other events
  | Unknown Word64 Word16 -- timestamp, size
    {-
  | PreRunThreadUser
  | PreRunThreadSystem

  | PostRunThreadUser
  | PostRunThreadSystem

  | ThreadPageFaults
  | ThreadCtxSwitches
  | ThreadIOBlocks
    -}
  deriving Show

isKnown :: Event -> Bool
isKnown ev =
    case ev of
        Unknown _ _ -> False
        _ -> True

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
event :: IntMap Int -> Parser Word8 IO Event
event kv = do
    eventId <- word16be
    ts <- word64be
    -- Parser.fromEffect $ putStr $ "event = " ++ show eventId ++ " ts = " ++ show ts
    case eventId of
        EVENT_PRE_RUN_THREAD -> do
            tid <- word32be
            -- Parser.fromEffect $ putStr $ "event = " ++ show eventId ++ " ts = " ++ show ts
            -- Parser.fromEffect $ putStrLn $ " tid = " ++ show tid
            return $ StartThreadCPUTime tid ts
        EVENT_POST_RUN_THREAD -> do
            tid <- word32be
            -- Parser.fromEffect $ putStr $ "event = " ++ show eventId ++ " ts = " ++ show ts
            -- Parser.fromEffect $ putStrLn $ " tid = " ++ show tid
            return $ StopThreadCPUTime tid ts
        _ -> do
            -- Parser.fromEffect $ putStrLn ""
            let r = Map.lookup (fromIntegral eventId) kv
            len <-
                case r of
                    Just x ->
                        if x == -1
                        then fmap fromIntegral word16be
                        else return x
                    Nothing -> error "Event not in the header"
            _ <- Parser.takeEQ len Fold.drain
            return $ Unknown ts eventId

{-# INLINE parseEvents #-}
parseEvents :: IntMap Int -> StreamK IO (Array Word8) -> Stream IO Event
parseEvents kv =
      Stream.filter isKnown
    . Stream.catRights
    . Stream.parseMany (event kv)
    . Stream.unfoldMany Array.reader
    . StreamK.toStream
