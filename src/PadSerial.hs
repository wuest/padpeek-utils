module PadSerial
    (
      -- * The @PadReader@ type - handles reading from pad serial and tracking
      -- state
      PadReader
    , open
    , rawPacket

      -- * The @ControllerState@ type - describes the state of the pad observed
      -- by PadReader
    , ControllerState
    , controllerState
    , buttonActive
    
      -- * The @ControllerMap@ type - describes Label -> Bitmask relationships
    , ControllerMap
    ) where

import Control.Concurrent
import qualified Data.Bits as Bits
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as PTime
import qualified Data.Map.Strict as Map
import Prelude

import System.Hardware.Serialport

type ControllerState = Map.Map String Bool
type ControllerMap = Map.Map String Int
type PadPacket = (Time.UTCTime, String, String)
type TimedChar = (Time.UTCTime, Char)
type PadReader = (Chan TimedChar, SerialPort, ControllerMap)

packetSize :: Int
packetSize = 8

emptyPacket :: PadPacket
emptyPacket = (PTime.posixSecondsToUTCTime 0, "", "")

emptyState :: ControllerState
emptyState = Map.fromList []

buttonActive :: ControllerState -> String -> Bool
buttonActive c s = buttonActive' $ Map.lookup s c

buttonActive' :: Maybe Bool -> Bool
buttonActive' Nothing = False
buttonActive' (Just False) = False
buttonActive' (Just True) = True

stringToInt :: String -> Int
stringToInt state = foldl ((Bits..|.) . flip Bits.shift 8) 0 $ fmap Char.ord state

controllerState :: PadReader -> IO (Time.UTCTime, ControllerState)
controllerState reader = do
    let controller = mapFor reader
    (time, state) <- padState reader
    return (time, controllerState' controller state)

controllerState' :: ControllerMap -> String -> ControllerState
controllerState' controller state = do
    let s = stringToInt state
        f k v = Map.insert k ((Bits..&.) v s /= 0)
    Map.foldrWithKey f emptyState controller

rawPacket :: PadReader -> IO (Time.UTCTime, (Int, (Int, Int)))
rawPacket reader = do
    (time, packet) <- padState reader
    let controllerID = stringToInt $ take 2 packet
        state = stringToInt $ drop 2 packet
        high = flip Bits.shift (-8) $ (Bits..&.) state 0xFF00
        low = (Bits..&.) state 0xFF
    return (time, (controllerID, (high, low)))

validPacket :: PadPacket -> Bool
validPacket (_, sync, msg) = sync == "SYNC" && length msg == 4

padState :: PadReader -> IO (Time.UTCTime, String)
padState (chan, _, _) = padState' chan emptyPacket

padState' :: Chan TimedChar -> PadPacket -> IO (Time.UTCTime, String)
padState' chan (time, sync, msg) =
    if validPacket (time, sync, msg)
        then return (time, msg)
        else (do
            (time', byte) <- readChan chan
            let full = sync ++ msg ++ [byte]
                part = drop (length full - 8) full
            padState' chan (time', take 4 part, drop 4 part))

readSerial :: PadReader -> IO ()
readSerial reader = do
    packet <- flip recv packetSize $ serialFor reader
    time <- Time.getCurrentTime
    let chan = chanFor reader
    writeList2Chan chan $ map ((,) time) $ ByteString.unpack packet
    readSerial reader

open :: ControllerMap -> String -> IO PadReader
open controllerMap serialPath = do
    chan <- newChan
    serial <- openSerial serialPath defaultSerialSettings
    let reader = (chan, serial, controllerMap)
    _ <- forkIO $ readSerial reader
    return reader

chanFor :: PadReader -> Chan TimedChar
chanFor (c, _, _) = c

serialFor :: PadReader -> SerialPort
serialFor (_, s, _) = s

mapFor :: PadReader -> ControllerMap
mapFor (_, _, m) = m
