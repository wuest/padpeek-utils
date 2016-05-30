import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.Bits as Bits
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified System.Environment as Env
import System.Hardware.Serialport
import System.IO

type PadPacket = (String, String)
type ControllerMap = Map.Map String Int
type ControllerState = Map.Map String Bool
type ControllerDisplay = [[String]]

pressedButtonString = " (\ESC[31;41m+\ESC[0m)"
unpressedButtonString = " ( )"

packetSize = 8
emptyPacket = ("", "") :: PadPacket

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

printPad :: String -> IO ()
printPad state =
    putStr . (++) "\ESC[1;1H" . buildDisplayString snesDisplay $ activeMap snesController state

-- SNES only presently
activeMap :: ControllerMap -> String -> ControllerState
activeMap controller state = do
    let s = foldl ((Bits..|.) . flip Bits.shift 8) 0 $ fmap Char.ord state
        f k v = Map.insert k ((Bits..&.) v s /= 0)
    Map.foldrWithKey f emptySnesState snesController

validPacket :: PadPacket -> Bool
validPacket (sync, msg) = sync == "SYNC" && length msg == 4

padState :: Chan Char -> PadPacket -> IO String
padState chan (sync, msg) =
    if validPacket (sync, msg)
        then return msg
        else (do
            byte <- readChan chan
            let full = sync ++ msg ++ [byte]
                part = drop (length full - 8) full
            padState chan (take 4 full, drop 4 full))

displayPad :: Chan Char -> IO ()
displayPad chan = do
    printPad =<< padState chan emptyPacket
    displayPad chan

readSerial :: Chan Char -> SerialPort -> IO ()
readSerial chan serial = do
    packet <- recv serial packetSize
    writeList2Chan chan (ByteString.unpack packet)
    readSerial chan serial

serialPort :: IO String
serialPort = do
    args <- Env.getArgs
    return $ head args

main :: IO ()
main = do
    chan <- newChan
    s <- flip openSerial defaultSerialSettings =<< serialPort
    forkIO $ readSerial chan s
    putStr "\ESC[2J"
    displayPad chan
