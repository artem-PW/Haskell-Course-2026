{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Types
import Parser   (parseScore, runParser, pitch, duration, noteItem,
                 restItem, scoreItem)
import Engine   (renderScore, renderItems, itemDuration,
                 itemsDuration, totalDuration)
import Pretty   (prettyScore, prettyItem)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck


instance Arbitrary Pitch where
  arbitrary = Pitch
    <$> elements "CDEFGAB"
    <*> elements [-1, 0, 1]
    <*> choose (0, 8)
  shrink (Pitch l acc o) =
       [ Pitch l acc o' | o' <- shrink o, o' >= 0 ]
    ++ [ Pitch l 0 o | acc /= 0 ]

instance Arbitrary Duration where
  arbitrary = elements allDurations

instance Arbitrary Item where
  arbitrary = sized gen
    where
      gen 0 = oneof [Note <$> arbitrary <*> arbitrary
                     , Rest <$> arbitrary]
      gen n = frequency
        [ (3, Note <$> arbitrary <*> arbitrary)
        , (2, Rest <$> arbitrary)
        , (1, Repeat <$> choose (1, 4) <*> subItems)
        , (1, Group <$> subItems)
        , (1, Transpose <$> choose (-12, 12) <*> subItems)
        ]
        where subItems = resize (n `div` 2) (listOf1 (gen (n `div` 2)))

  shrink (Note p d)         = [Note p' d | p' <- shrink p]
                           ++ [Note p d' | d' <- shrink d]
  shrink (Rest d)           = [Rest d'   | d' <- shrink d]
  shrink (Repeat n items)   = items
                           ++ [Repeat n' items  | n' <- shrink n, n' >= 1]
                           ++ [Repeat n items'  | items' <- shrinkList shrink items
                                                , not (null items')]
  shrink (Group items)      = items
                           ++ [Group items'     | items' <- shrinkList shrink items
                                                , not (null items')]
  shrink (Transpose k items) = items
                            ++ [Transpose k' items | k' <- shrink k]
                            ++ [Transpose k items' | items' <- shrinkList shrink items
                                                   , not (null items')]

instance Arbitrary Part where
  arbitrary = Part
    <$> elements ["piano", "bass", "drums", "guitar", "violin"]
    <*> listOf1 (resize 3 arbitrary)
  shrink (Part inst items) =
    [Part inst items' | items' <- shrinkList shrink items, not (null items')]

instance Arbitrary Score where
  arbitrary = Score
    <$> elements ["Song", "Demo", "Test", "Beat"]
    <*> choose (40, 240)
    <*> listOf1 (resize 3 arbitrary)
  shrink (Score t bpm ps) =
       [Score t bpm' ps | bpm' <- shrink bpm, bpm' > 0]
    ++ [Score t bpm ps' | ps' <- shrinkList shrink ps, not (null ps')]


(~=) :: Double -> Double -> Bool
a ~= b = abs (a - b) < 1e-6

infix 4 ~=

unitTests :: Spec
unitTests = describe "Unit tests" $ do

  describe "pitchToMidi" $ do
    it "C4 = 60 (middle C)" $
      pitchToMidi (Pitch 'C' 0 4) `shouldBe` 60

    it "A4 = 69 (concert A)" $
      pitchToMidi (Pitch 'A' 0 4) `shouldBe` 69

    it "C#4 = 61" $
      pitchToMidi (Pitch 'C' 1 4) `shouldBe` 61

    it "Db4 = 61 (enharmonic with C#4)" $
      pitchToMidi (Pitch 'D' (-1) 4) `shouldBe` 61

    it "C0 = 12" $
      pitchToMidi (Pitch 'C' 0 0) `shouldBe` 12

  describe "durationToBeats" $ do
    it "whole = 4 beats" $
      durationToBeats Whole `shouldBe` 4.0

    it "quarter = 1 beat" $
      durationToBeats Quarter `shouldBe` 1.0

    it "sixteenth = 0.25 beats" $
      durationToBeats Sixteenth `shouldBe` 0.25

  describe "beatsToSeconds" $ do
    it "1 beat at 120 BPM = 0.5 s" $
      beatsToSeconds 120 1.0 `shouldBe` 0.5

    it "4 beats at 60 BPM = 4.0 s" $
      beatsToSeconds 60 4.0 `shouldBe` 4.0

    it "1 quarter at 120 BPM = 0.5 s" $
      beatsToSeconds 120 (durationToBeats Quarter) `shouldBe` 0.5

  describe "itemDuration" $ do
    it "Note Quarter at 120 BPM = 0.5 s" $
      itemDuration 120 (Note (Pitch 'C' 0 4) Quarter) ~= 0.5
        `shouldBe` True

    it "Rest Half at 120 BPM = 1.0 s" $
      itemDuration 120 (Rest Half) ~= 1.0
        `shouldBe` True

    it "Repeat 3 of Quarter at 120 BPM = 1.5 s" $
      let item' = Repeat 3 [Note (Pitch 'C' 0 4) Quarter]
      in itemDuration 120 item' ~= 1.5
        `shouldBe` True

  describe "Parser: pitch" $ do
    it "parses C4" $
      runParser pitch "C4" `shouldBe` [(Pitch 'C' 0 4, "")]

    it "parses D#5" $
      runParser pitch "D#5" `shouldBe` [(Pitch 'D' 1 5, "")]

    it "parses Eb3" $
      runParser pitch "Eb3" `shouldBe` [(Pitch 'E' (-1) 3, "")]

    it "fails on X4" $
      runParser pitch "X4" `shouldBe` []

  describe "Parser: duration" $ do
    it "parses q as Quarter" $
      runParser duration "q" `shouldBe` [(Quarter, "")]

    it "parses w as Whole" $
      runParser duration "w" `shouldBe` [(Whole, "")]

  describe "Parser: noteItem" $ do
    it "parses C4 q" $
      runParser noteItem "C4 q" `shouldBe`
        [(Note (Pitch 'C' 0 4) Quarter, "")]

    it "parses F#3 e" $
      runParser noteItem "F#3 e" `shouldBe`
        [(Note (Pitch 'F' 1 3) Eighth, "")]

  describe "Parser: restItem" $ do
    it "parses r h" $
      runParser restItem "r h" `shouldBe` [(Rest Half, "")]

  describe "Parser: repeat" $ do
    it "parses repeat block" $
      case runParser scoreItem "repeat 2 { C4 q }" of
        [(Repeat 2 [Note (Pitch 'C' 0 4) Quarter], "")] -> pure ()
        other -> expectationFailure (show other)

  describe "Parser: transpose" $ do
    it "parses transpose +3 block" $
      case runParser scoreItem "transpose +3 { C4 q }" of
        [(Transpose 3 [Note (Pitch 'C' 0 4) Quarter], "")] -> pure ()
        other -> expectationFailure (show other)

    it "parses transpose -2 block" $
      case runParser scoreItem "transpose -2 { D4 h }" of
        [(Transpose (-2) [Note (Pitch 'D' 0 4) Half], "")] -> pure ()
        other -> expectationFailure (show other)

  describe "Parser: comments" $ do
    it "ignores line comments" $
      case parseScore (unlines
        [ "-- this is a comment"
        , "song \"Test\" tempo 120 {"
        , "  instrument piano {"
        , "    C4 q  -- inline comment"
        , "  }"
        , "}"
        ]) of
        Right (Score "Test" 120 _) -> pure ()
        other -> expectationFailure (show other)

  describe "Parser: errors" $ do
    it "returns Left on invalid input" $
      parseScore "not a score" `shouldSatisfy` isLeft
    it "returns Left on empty input" $
      parseScore "" `shouldSatisfy` isLeft

  where
    isLeft (Left _) = True
    isLeft _        = False


endToEndTests :: Spec
endToEndTests = describe "End-to-end tests" $ do

  it "single note: C4 quarter at 120 BPM -> one event at t=0, dur=0.5" $ do
    let input = "song \"T\" tempo 120 { instrument piano { C4 q } }"
    case parseScore input of
      Left err -> expectationFailure err
      Right s  -> do
        let events = renderScore s
        length events `shouldBe` 1
        let ev = head events
        eventTime ev     ~= 0.0  `shouldBe` True
        eventPitch ev          `shouldBe` 60
        eventDuration ev ~= 0.5  `shouldBe` True
        eventVelocity ev       `shouldBe` 100

  it "two notes: C4 q E4 q at 120 BPM -> events at t=0.0 and t=0.5" $ do
    let input = "song \"T\" tempo 120 { instrument piano { C4 q E4 q } }"
    case parseScore input of
      Left err -> expectationFailure err
      Right s  -> do
        let events = renderScore s
        length events `shouldBe` 2
        eventTime (events !! 0) ~= 0.0 `shouldBe` True
        eventTime (events !! 1) ~= 0.5 `shouldBe` True
        eventPitch (events !! 0) `shouldBe` 60  
        eventPitch (events !! 1) `shouldBe` 64  

  it "rest between notes advances time" $ do
    let input = "song \"T\" tempo 120 { instrument piano { C4 q r q E4 q } }"
    case parseScore input of
      Left err -> expectationFailure err
      Right s  -> do
        let events = renderScore s
        length events `shouldBe` 2  
        eventTime (events !! 0) ~= 0.0 `shouldBe` True
        eventTime (events !! 1) ~= 1.0 `shouldBe` True  

  it "repeat 3 produces 3 copies" $ do
    let input = "song \"T\" tempo 120 { instrument piano { repeat 3 { C4 q } } }"
    case parseScore input of
      Left err -> expectationFailure err
      Right s  -> do
        let events = renderScore s
        length events `shouldBe` 3
        eventTime (events !! 0) ~= 0.0 `shouldBe` True
        eventTime (events !! 1) ~= 0.5 `shouldBe` True
        eventTime (events !! 2) ~= 1.0 `shouldBe` True

  it "transpose shifts pitch by semitones" $ do
    let input = "song \"T\" tempo 120 { instrument p { transpose +3 { C4 q } } }"
    case parseScore input of
      Left err -> expectationFailure err
      Right s  -> do
        let events = renderScore s
        length events `shouldBe` 1
        eventPitch (head events) `shouldBe` 63

  it "two parallel parts: both start at t=0" $ do
    let input = unlines
          [ "song \"T\" tempo 120 {"
          , "  instrument piano { C4 q E4 q }"
          , "  instrument bass  { C2 h }"
          , "}"
          ]
    case parseScore input of
      Left err -> expectationFailure err
      Right s  -> do
        let events = renderScore s
        length events `shouldBe` 3  
        eventPitch (events !! 0) `shouldBe` 60  
        eventPitch (events !! 1) `shouldBe` 64  
        eventPitch (events !! 2) `shouldBe` 36  
        eventTime  (events !! 2) ~= 0.0 `shouldBe` True

  it "total duration is max over parts, not sum" $ do
    let input = unlines
          [ "song \"T\" tempo 60 {"
          , "  instrument piano { C4 q }"            
          , "  instrument bass  { C2 w }"            
          , "}"
          ]
    case parseScore input of
      Left err -> expectationFailure err
      Right s  ->
        totalDuration s ~= 4.0 `shouldBe` True  

  it "nested repeat and group" $ do
    let input = unlines
          [ "song \"T\" tempo 120 {"
          , "  instrument piano {"
          , "    repeat 2 {"
          , "      group { C4 q E4 q }"
          , "    }"
          , "  }"
          , "}"
          ]
    case parseScore input of
      Left err -> expectationFailure err
      Right s  -> do
        let events = renderScore s
        length events `shouldBe` 4
        eventTime (events !! 0) ~= 0.0 `shouldBe` True
        eventTime (events !! 1) ~= 0.5 `shouldBe` True
        eventTime (events !! 2) ~= 1.0 `shouldBe` True
        eventTime (events !! 3) ~= 1.5 `shouldBe` True

  it "full example score from the spec" $ do
    let input = unlines
          [ "song \"AlgoBeat\" tempo 120 {"
          , "  instrument piano {"
          , "    repeat 4 {"
          , "      C4 q  E4 q  G4 q  C5 q"
          , "    }"
          , "  }"
          , "}"
          ]
    case parseScore input of
      Left err -> expectationFailure err
      Right s  -> do
        let events = renderScore s
        length events `shouldBe` 16
        eventPitch (head events)       `shouldBe` 60
        eventTime  (head events) ~= 0.0 `shouldBe` True
        eventPitch (last events)       `shouldBe` 72
        eventTime  (last events) ~= 7.5 `shouldBe` True



propertyTests :: Spec
propertyTests = describe "Property-based tests" $ do


  prop "prettyItem round-trip: parseItem (prettyItem x) == x" $
    \item' -> let pp = prettyItem item'
              in case runParser scoreItem pp of
                   [(parsed, "")] -> parsed === item'
                   other          -> counterexample
                     ("pretty = " ++ show pp ++ ", parse = " ++ show other)
                     False

  prop "prettyScore round-trip: parseScore (prettyScore s) == Right s" $
    \s -> parseScore (prettyScore s) === Right s


  prop "duration conservation: renderItems endTime = sum of item durations" $
    \(Positive bpm) items ->
      let bpm'         = bpm `mod` 300 + 30    
          (_, endTime) = renderItems bpm' 0.0 0 items
          expected     = itemsDuration bpm' items
      in counterexample
           ("endTime=" ++ show endTime
            ++ " expected=" ++ show expected)
           (endTime ~= expected)


  prop "transpose shifts every event pitch by exactly k" $
    \k items ->
      let k'       = (k :: Int) `mod` 24 - 12  
          bpm      = 120
          original = fst (renderItems bpm 0.0 0 items)
          shifted  = fst (renderItems bpm 0.0 k' items)
      in length original === length shifted
         .&&.
         conjoin (zipWith (\o s ->
           eventPitch s === eventPitch o + k') original shifted)


  prop "doubling BPM halves start times and durations" $
    \items ->
      let bpm1     = 60
          bpm2     = 120  
          events1  = fst (renderItems bpm1 0.0 0 items)
          events2  = fst (renderItems bpm2 0.0 0 items)
      in length events1 === length events2
         .&&.
         conjoin (zipWith (\e1 e2 ->
           counterexample "start time" (eventTime e2 ~= eventTime e1 / 2)
           .&&.
           counterexample "duration" (eventDuration e2 ~= eventDuration e1 / 2))
           events1 events2)


  prop "Repeat n items produces n x (notes in items) events" $
    \(Positive n) items ->
      let n'      = n `mod` 8 + 1
          bpm     = 120
          single  = fst (renderItems bpm 0.0 0 items)
          repeated = fst (renderItems bpm 0.0 0 [Repeat n' items])
      in length repeated === n' * length single


  prop "rendering preserves note pitches in order" $
    \items ->
      let bpm     = 120
          events  = fst (renderItems bpm 0.0 0 items)
          expected = collectPitches 0 items
      in map eventPitch events === expected

  where
    collectPitches :: Int -> [Item] -> [Int]
    collectPitches _  []       = []
    collectPitches tr (i : is) = collectFromItem tr i ++ collectPitches tr is

    collectFromItem :: Int -> Item -> [Int]
    collectFromItem tr (Note p _)         = [pitchToMidi p + tr]
    collectFromItem _  (Rest _)           = []
    collectFromItem tr (Repeat n items)   =
      concat (replicate n (collectPitches tr items))
    collectFromItem tr (Group items)      = collectPitches tr items
    collectFromItem tr (Transpose k items) = collectPitches (tr + k) items


main :: IO ()
main = hspec $ do
  unitTests
  endToEndTests
  propertyTests
