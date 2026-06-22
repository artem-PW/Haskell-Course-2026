module Main (main) where

import Types (Score (..), Event (..))
import Parser (parseScore)
import Engine (renderScore, totalDuration)
import Pretty (prettyScore)
import Midi (writeMidi)

sampleScore :: String
sampleScore = unlines
  [ "-- A short demo score"
  , "song \"AlgoBeat\" tempo 120 {"
  , "  instrument piano {"
  , "    repeat 2 {"
  , "      C4 q  E4 q  G4 q  C5 q"
  , "    }"
  , "    r h"
  , "    transpose +3 {"
  , "      C4 q  E4 q  G4 q"
  , "    }"
  , "  }"
  , "  instrument bass {"
  , "    C2 w  G2 w  C2 w"
  , "  }"
  , "}"
  ]

main :: IO ()
main = do
  putStrLn "=== MusicGenLang ==="
  putStrLn ""

  putStrLn "-- Input score:"
  putStrLn sampleScore

  case parseScore sampleScore of
    Left err -> putStrLn ("ERROR: " ++ err)
    Right s  -> do
      putStrLn "-- Pretty-printed (canonical):"
      putStrLn (prettyScore s)
      putStrLn ""

      let events = renderScore s
      putStrLn ("-- Rendered " ++ show (length events) ++ " events:")
      mapM_ printEvent events
      putStrLn ""

      putStrLn ("-- Total duration: " ++ show (totalDuration s) ++ " seconds")
      putStrLn ""

      let midiFile = "output.mid"
      writeMidi midiFile (tempo s) events
      putStrLn ("-- MIDI file written to: " ++ midiFile)

printEvent :: Event -> IO ()
printEvent (Event t p d v) =
  putStrLn $ "  t=" ++ showF t
          ++ "s  pitch=" ++ show p
          ++ "  dur=" ++ showF d
          ++ "s  vel=" ++ show v

showF :: Double -> String
showF x = show (fromIntegral (round (x * 1000) :: Int) / 1000.0 :: Double)
