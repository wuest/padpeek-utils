import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.Bits as Bits
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as PTime
import qualified System.Environment as Env
import System.Hardware.Serialport
import System.IO

type PadPacket = (Time.UTCTime, String, String)
type ControllerMap = Map.Map String Int
type ControllerState = Map.Map String Bool
type ControllerDisplay = [[String]]
type TimedChar = (Time.UTCTime, Char)

pressedButtonString = " (\ESC[31;41m+\ESC[0m)"
unpressedButtonString = " ( )"

packetSize = 8
emptyPacket = (PTime.posixSecondsToUTCTime 0, "", "") :: PadPacket

emptySnesState = Map.fromList [] :: ControllerState
snesController = Map.fromList [("A", 0x1), ("B", 0x100), ("X", 0x2), ("Y", 0x200)
                              ,("^", 0x1000), ("v", 0x2000), ("<", 0x4000), (">", 0x8000)
                              ,("L", 0x4), ("R", 0x8), ("Select", 0x400), ("Start", 0x800)
                              ] :: ControllerMap
snesDisplay = [["A", "B", "X", "Y"]
              ,["^", "v", "<", ">"]
              ,["L", "R", "Select", "Start"]
              ]

ifPressed :: Maybe Bool -> String
ifPressed Nothing = unpressedButtonString
ifPressed (Just False) = unpressedButtonString
ifPressed (Just True) = pressedButtonString

buildDisplayString :: ControllerDisplay -> ControllerState -> String
buildDisplayString cd cs =
    unlines $ fmap (unwords .
        fmap (\k -> (k ++) $ ifPressed $ Map.lookup k cs)) cd

printPad :: Time.NominalDiffTime -> (Time.UTCTime, String) -> IO ()
printPad delayMilliseconds (packetTime, state) = do
    currentTime <- Time.getCurrentTime
    let timeDiff = Time.diffUTCTime currentTime packetTime
    threadDelay $ floor . (* 1000000) . toRational $ delayMilliseconds - timeDiff
    putStr $ (++) "\ESC[1;1H" $ buildDisplayString snesDisplay $ activeMap snesController state

-- SNES only presently
activeMap :: ControllerMap -> String -> ControllerState
activeMap controller state = do
    let s = foldl ((Bits..|.) . flip Bits.shift 8) 0 $ fmap Char.ord state
        f k v = Map.insert k ((Bits..&.) v s /= 0)
    Map.foldrWithKey f emptySnesState snesController

validPacket :: PadPacket -> Bool
validPacket (_, sync, msg) = sync == "SYNC" && length msg == 4

padState :: Chan TimedChar -> PadPacket -> IO (Time.UTCTime, String)
padState chan (time, sync, msg) =
    if validPacket (time, sync, msg)
        then return (time, msg)
        else (do
            (time, byte) <- readChan chan
            let full = sync ++ msg ++ [byte]
                part = drop (length full - 8) full
            padState chan (time, take 4 full, drop 4 full))

displayPad :: Chan TimedChar -> Time.NominalDiffTime -> IO ()
displayPad chan delayMilliseconds = do
    printPad delayMilliseconds =<< padState chan emptyPacket
    displayPad chan delayMilliseconds

readSerial :: Chan TimedChar -> SerialPort -> IO ()
readSerial chan serial = do
    packet <- recv serial packetSize
    time <- Time.getCurrentTime
    writeList2Chan chan $ map ((,) time) $ ByteString.unpack packet
    readSerial chan serial

parseArgs :: IO (String, Int)
parseArgs = do
    args <- Env.getArgs
    let serialPath = head args
        delayMilliseconds = read (args!!1) :: Int
    return (serialPath, delayMilliseconds)

main :: IO ()
main = do
    chan <- newChan
    (serialPath, delayMilliseconds) <- parseArgs
    serial <- openSerial serialPath defaultSerialSettings
    forkIO $ readSerial chan serial
    putStr "\ESC[?25l\ESC[2J"
    displayPad chan ((realToFrac delayMilliseconds :: Time.NominalDiffTime) / 1000)
