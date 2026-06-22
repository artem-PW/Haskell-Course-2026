module Engine
  ( renderScore
  , renderPart
  , renderItems
  , itemDuration
  , itemsDuration
  , totalDuration
  , transposePitch
  , transposeItem
  , scaleTempo
  ) where

import Types

defaultVelocity :: Int
defaultVelocity = 100

renderScore :: Score -> [Event]
renderScore s = concatMap (renderPart (tempo s)) (parts s)

renderPart :: Int -> Part -> [Event]
renderPart bpm p = fst (renderItems bpm 0.0 0 (body p))

renderItems :: Int -> Double -> Int -> [Item] -> ([Event], Double)
renderItems _   start _  [] = ([], start)
renderItems bpm start tr (i : is) =
  let (events1, mid) = renderItem bpm start tr i
      (events2, end) = renderItems bpm mid tr is
  in (events1 ++ events2, end)

renderItem :: Int -> Double -> Int -> Item -> ([Event], Double)
renderItem bpm start tr (Note p d) =
  let dur = beatsToSeconds bpm (durationToBeats d)
      midi = pitchToMidi p + tr
      ev = Event start midi dur defaultVelocity
  in ([ev], start + dur)

renderItem bpm start _tr (Rest d) =
  let dur = beatsToSeconds bpm (durationToBeats d)
  in ([], start + dur)

renderItem bpm start tr (Repeat n items) =
  foldl step ([], start) [1 .. n]
  where
    step (accEvs, t) _ =
      let (newEvs, t') = renderItems bpm t tr items
      in (accEvs ++ newEvs, t')

renderItem bpm start tr (Group items) =
  renderItems bpm start tr items

renderItem bpm start tr (Transpose k items) =
  renderItems bpm start (tr + k) items

itemDuration :: Int -> Item -> Double
itemDuration bpm (Note _ d) = beatsToSeconds bpm (durationToBeats d)
itemDuration bpm (Rest d) = beatsToSeconds bpm (durationToBeats d)
itemDuration bpm (Repeat n items) = fromIntegral n * itemsDuration bpm items
itemDuration bpm (Group items) = itemsDuration bpm items
itemDuration bpm (Transpose _ items) = itemsDuration bpm items

itemsDuration :: Int -> [Item] -> Double
itemsDuration bpm = sum . map (itemDuration bpm)

totalDuration :: Score -> Double
totalDuration s
  | null (parts s) = 0.0
  | otherwise = maximum (map partDur (parts s))
  where
    partDur p = itemsDuration (tempo s) (body p)

transposePitch :: Int -> Pitch -> Pitch
transposePitch k p = midiToPitch (pitchToMidi p + k)

transposeItem :: Int -> Item -> Item
transposeItem k (Note p d) = Note (transposePitch k p) d
transposeItem _ (Rest d) = Rest d
transposeItem k (Repeat n items) = Repeat n (map (transposeItem k) items)
transposeItem k (Group items) = Group (map (transposeItem k) items)
transposeItem k (Transpose j items) = Transpose j (map (transposeItem k) items)

scaleTempo :: Int -> Score -> Score
scaleTempo newBpm s = s { tempo = newBpm }
