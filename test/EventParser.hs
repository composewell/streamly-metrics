{-# LANGUAGE CPP #-}
module EventParser
    (
      main
    )
where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import Data.Either (fromRight)
import Data.Word
import Streamly.Internal.Serialize.FromBytes
import Streamly.Data.Parser

import qualified Data.IntMap as Map
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Parser as PD (takeEQ)
import qualified Streamly.FileSystem.File as FP
import qualified Streamly.Internal.Data.Stream as Stream (parseBreak)

#define EVENT_HEADER_BEGIN    0x68647262
#define EVENT_HEADER_END      0x68647265
#define EVENT_DATA_BEGIN      0x64617462
#define EVENT_DATA_END        0xffff
#define EVENT_HET_BEGIN       0x68657462
#define EVENT_HET_END         0x68657465
#define EVENT_ET_BEGIN        0x65746200
#define EVENT_ET_END          0x65746500

parseF :: IORef (Map.IntMap Word16) -> FilePath -> IO ()
parseF kvRef path = do
    (res, nxt) <- Stream.parseBreak word32be $ FP.read path
    case res of
        Left err -> fail $ show err
        Right x -> do
           -- print x >> print (x == EVENT_HEADER_BEGIN)
            when (x /= EVENT_HEADER_BEGIN ) $ fail "Event header begin not found."
            (res2, nxt2) <- Stream.parseBreak word32be nxt
            case res2 of
                Left err -> fail $ show err
                Right x -> do
                    --print x >> print (x == EVENT_HET_BEGIN)
                    when (x /= EVENT_HET_BEGIN ) $ fail "Event Type list, begin not found."
                    (res2, nxt2) <- Stream.parseBreak word32be nxt
                    ehe <- getEventTypes nxt2
                    (res3, eds) <- Stream.parseBreak word32be ehe
                    case res3 of
                        Right EVENT_HEADER_END -> do
                            print "EVENT_HEADER_END" >> print (x == EVENT_HEADER_END)
                            (res, nxt) <- Stream.parseBreak word32be eds
                            case res of
                                Left err -> fail $ show err
                                Right EVENT_DATA_BEGIN -> do
                                    print "EVENT_DATA_BEGIN" >> getEventData nxt
                        Right _ -> fail "Event header end not found."

                    return ()
            return ()
    where

    getEventData st = do
        (ev_num, eds1) <- Stream.parseBreak word16be st
        (res4, eds2) <- Stream.parseBreak word64be eds1
        skl <-
            case ev_num of
                Right num -> do
                    mp <- readIORef kvRef
                    return $ Map.lookup (fromIntegral num) mp
        (res5, eds3) <-
            case skl of
                Just l -> do
                    skip l eds2
                Nothing -> fail "Event type unknown."
        print $ "Event num = " ++ show ev_num
        print $ "evTime = " ++ show res4
        getEventData eds3

    getEventDatas st = do
        (res, nxt) <- Stream.parseBreak word32be st
        case res of
            Left err -> fail $ show err
            Right EVENT_DATA_BEGIN -> do
                print "EVENT_DATA_BEGIN"
                (res3, eds1) <- Stream.parseBreak word16be nxt
                (res4, eds2) <- Stream.parseBreak word64be eds1
                (res5, eds3) <- skip 14 eds2
                (res6, eds4) <- Stream.parseBreak word16be eds3
                (res7, eds5) <- Stream.parseBreak word64be eds4
                (res8, eds6) <- Stream.parseBreak word32be eds5
                print res3 >> print res4 >> print res6 >> print res7 >> print res8
                return ()

            Right EVENT_DATA_END -> print "EVENT_DATA_END"
    getEventTypes st = do
            (res, nxt) <- Stream.parseBreak word32be st
            case res of
                Left err -> fail $ show err
                Right EVENT_ET_BEGIN -> getEventType nxt >>= getEventTypes
                Right EVENT_HET_END -> return (nxt)
    skip len str = Stream.parseBreak (PD.takeEQ (fromIntegral len) Fold.drain) str
    getEventType st = do
        (evtnum, nxt) <- Stream.parseBreak word16be st
        let e_num = fromIntegral $ fromRight 9999 evtnum
        print $ "Event Num =" ++ show e_num
        (evtsize, nxt2) <- Stream.parseBreak word16be nxt
        let e_size = fromIntegral $ case evtsize of
                        Left err -> 0
                        Right sz ->
                            if (sz == 0xffff)
                            then 0
                            else sz
        print $ "Event Size =" ++ show e_size

        modifyIORef kvRef (Map.insert e_num  e_size)

        (res3, nxt3) <- Stream.parseBreak word32be nxt2
        case res3 of
            Left err -> fail $ show err
            Right len -> do
                (res4, nxt4) <- skip len nxt3
                (res5, nxt5) <- Stream.parseBreak word32be nxt4
                (res6, nxt6) <- skip (fromRight 0 res5) nxt5
                (res7, nxt7) <- Stream.parseBreak word32be nxt6
                case res7 of
                    Left err -> fail $ show err
                    Right x -> when (x /= EVENT_ET_END ) $
                                    fail "Event Type end marker not found."
                return nxt7

main :: IO ()
main = do
    kvRef <- newIORef Map.empty
    parseF kvRef "EventParser.eventlog"
    print "Done Event"
