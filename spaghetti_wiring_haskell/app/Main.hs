module Main where

import           Control.Monad      (mapM)
import           System.Environment (getArgs)
import           System.IO          (IOMode (..), hClose, hGetContents,
                                     hPutStrLn, openFile)
import           Text.Read          (readEither)
import           Wiring

parseArgs :: [String] -> Either String (String, String)
parseArgs [] = Left "Required arguments: <input file path> <output file path>"
parseArgs [_] = Left "Required arguments: <input file path> <output file path>"
parseArgs [inp, outp] = Right (inp, outp)
parseArgs _ = Left "Required arguments: <input file path> <output file path>"

parseInputContents :: String -> Either String (MapSize, [(Point, Point)])
parseInputContents content =
  case lines content of
    []  -> Left "Too few lines in the input file"
    [_] -> Left "Too few lines in the input file"
    (sizeLn:pntsLns) -> do
      [width, height] <- readInts 2 sizeLn
      -- points lines with no trailing newline
      let pntsLnsNoTnl = if null $ last pntsLns then init pntsLns else pntsLns
      pntComps <- mapM (readInts 4) pntsLnsNoTnl
      let mapSize = MapSize (width, height)
      let pnts = map (\[x1, y1, x2, y2] -> (Point (x1, y1), Point (x2, y2))) pntComps
      return (mapSize, pnts)
    where
      readInts n line =
        let ws = words line in
        if length ws /= n
          then Left ("Too few integer on line '" ++ line ++ "'")
          else mapM readEither ws

runCmd :: String -> String -> IO ()
runCmd inPath outPath = do
  inFile <- openFile inPath ReadMode
  outFile <- openFile outPath WriteMode
  inContents <- hGetContents inFile
  case parseInputContents inContents of
    Left err           -> putStrLn err
    Right (size, pnts) -> do
      putStr "Map size: "
      print size
      putStr "Points: "
      print pnts
      case wireup size pnts of
        Just solution -> hPutStrLn outFile $ toString $ toMap size solution
        Nothing       -> putStrLn "No solution was found"
  hClose inFile
  hClose outFile

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left errText      -> putStrLn errText
    Right (inp, outp) -> runCmd inp outp
