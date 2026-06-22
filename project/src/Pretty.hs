module Pretty
  ( prettyScore
  , prettyPart
  , prettyItem
  , prettyPitch
  , prettyDuration
  ) where

import Types

prettyScore :: Score -> String
prettyScore (Score t bpm ps) =
  "song " ++ show t ++ " tempo " ++ show bpm ++ " { "
  ++ unwords (map prettyPart ps)
  ++ "}"

prettyPart :: Part -> String
prettyPart (Part inst items) =
  "instrument " ++ inst ++ " { "
  ++ unwords (map prettyItem items)
  ++ "} "

prettyItem :: Item -> String
prettyItem (Note p d) = prettyPitch p ++ " " ++ prettyDuration d
prettyItem (Rest d) = "r " ++ prettyDuration d
prettyItem (Repeat n items) =
  "repeat " ++ show n ++ " { " ++ unwords (map prettyItem items) ++ " }"
prettyItem (Group items) =
  "group { " ++ unwords (map prettyItem items) ++ " }"
prettyItem (Transpose k items) =
  "transpose " ++ showSigned k ++ " { "
  ++ unwords (map prettyItem items) ++ " }"

prettyPitch :: Pitch -> String
prettyPitch (Pitch l acc o) = [l] ++ accStr ++ show o
  where
    accStr = case acc of
      1 -> "#"
      (-1) -> "b"
      _ -> ""

prettyDuration :: Duration -> String
prettyDuration Whole = "w"
prettyDuration Half = "h"
prettyDuration Quarter = "q"
prettyDuration Eighth = "e"
prettyDuration Sixteenth = "s"

showSigned :: Int -> String
showSigned n
  | n >= 0 = "+" ++ show n
  | otherwise = show n
