import qualified Data.Map.Strict as Map
import qualified Data.Time as Time
import Prelude
import qualified System.Environment as Env

import PadSerial as Pad

debugController :: ControllerMap
debugController = Map.fromList []

printPad :: Time.NominalDiffTime -> (Time.UTCTime, (Int, (Int, Int))) -> IO ()
printPad delaySeconds (packetTime, (controllerID, (high, low))) = do
    currentTime <- Time.getCurrentTime
    let timeDiff = Time.diffUTCTime currentTime packetTime
    putStr "\ESC[1;1H\ESC[2J"
    putStr "Controller ID: "
    print controllerID
    putStr "Controller state: "
    print [high, low]
    putStr "Packet time: "
    print packetTime
    putStr "Delay: "
    print delaySeconds
    putStr "Delay delta: "
    print timeDiff

displayPad :: Pad.PadReader -> Time.NominalDiffTime -> IO ()
displayPad reader delayMilliseconds = do
    printPad delayMilliseconds =<< Pad.rawPacket reader
    displayPad reader delayMilliseconds

parseArgs :: IO (String, Int)
parseArgs = do
    args <- Env.getArgs
    let serialPath = head args
        delayMilliseconds = read (args!!1) :: Int
    return (serialPath, delayMilliseconds)

main :: IO ()
main = do
    (serialPath, delayMilliseconds) <- parseArgs
    pad <- Pad.open debugController serialPath
    putStr "\ESC[2J"
    displayPad pad ((realToFrac delayMilliseconds :: Time.NominalDiffTime) / 1000)
