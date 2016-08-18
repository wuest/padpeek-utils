import Control.Concurrent
import qualified Data.Map.Strict as Map
import qualified Data.Time as Time
import qualified System.Environment as Env

import PadSerial as Pad

type ControllerDisplay = [[String]]

pressedButton :: String -> String
pressedButton = ("(\ESC[37;41m" ++) . (++ "\ESC[0m)")

unpressedButton :: String -> String
unpressedButton (' ':xs) = ' ':xs
unpressedButton x   = "(" ++ x ++ ")"

snesController :: ControllerMap
snesController = Map.fromList [("A", 0x1), ("B", 0x100), ("X", 0x2), ("Y", 0x200)
                              ,("^", 0x1000), ("v", 0x2000), ("<", 0x4000), (">", 0x8000)
                              ,("L", 0x4), ("R", 0x8), ("s", 0x400), ("S", 0x800)
                              ]

snesDisplay :: [[String]]
snesDisplay = [["  ", "^", "  ", "L", "R", "  ", "X"]
              ,["<", " ", ">", "   ", "   ", "Y", " ", "A"]
              ,["  ", "v", "  ", "s", "S", "  ", "B"]
              ]

buttonDisplay :: Bool -> (String -> String)
buttonDisplay True  = pressedButton
buttonDisplay False = unpressedButton

buildDisplayString :: ControllerDisplay -> ControllerState -> String
buildDisplayString cd cs =
    unlines $ fmap (unwords .
        fmap (\k -> (buttonDisplay $ Pad.buttonActive cs k) k)) cd

printPad :: Time.NominalDiffTime -> (Time.UTCTime, Pad.ControllerState) -> IO ()
printPad delayMilliseconds (packetTime, state) = do
    currentTime <- Time.getCurrentTime
    let timeDiff = Time.diffUTCTime currentTime packetTime
    threadDelay $ floor . (* 1000000) . toRational $ delayMilliseconds - timeDiff
    putStr $ (++) "\ESC[1;1H" $ buildDisplayString snesDisplay state

displayPad :: Pad.PadReader -> Time.NominalDiffTime -> IO ()
displayPad reader delayMilliseconds = do
    printPad delayMilliseconds =<< Pad.controllerState reader
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
    pad <- Pad.open snesController serialPath
    putStr "\ESC[?25l\ESC[2J"
    displayPad pad ((realToFrac delayMilliseconds :: Time.NominalDiffTime) / 1000)
