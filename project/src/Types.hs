module Types
  ( Score (..)
  , Part (..)
  , Item (..)
  , Pitch (..)
  , Duration (..)
  , Event (..)
  , pitchToMidi
  , midiToPitch
  , durationToBeats
  , beatsToSeconds
  , allDurations
  ) where

data Score = Score
  { title :: String
  , tempo :: Int
  , parts :: [Part]
  } deriving (Show, Eq)

data Part = Part
  { instrument :: String
  , body       :: [Item]
  } deriving (Show, Eq)

data Item
  = Note      Pitch Duration
  | Rest      Duration
  | Repeat    Int [Item]
  | Group     [Item]
  | Transpose Int [Item]
  deriving (Show, Eq)

data Pitch = Pitch
  { letter     :: Char
  , accidental :: Int
  , octave     :: Int
  } deriving (Show, Eq)

data Duration
  = Whole
  | Half
  | Quarter
  | Eighth
  | Sixteenth
  deriving (Show, Eq, Ord, Enum, Bounded)

allDurations :: [Duration]
allDurations = [Whole, Half, Quarter, Eighth, Sixteenth]

data Event = Event
  { eventTime     :: Double
  , eventPitch    :: Int
  , eventDuration :: Double
  , eventVelocity :: Int
  } deriving (Show)

instance Eq Event where
  e1 == e2 =
    nearEq (eventTime e1) (eventTime e2)
    && eventPitch e1 == eventPitch e2
    && nearEq (eventDuration e1) (eventDuration e2)
    && eventVelocity e1 == eventVelocity e2
    where
      nearEq a b = abs (a - b) < 1e-9

pitchToMidi :: Pitch -> Int
pitchToMidi (Pitch l acc oct) = (oct + 1) * 12 + noteOffset l + acc
  where
    noteOffset 'C' = 0
    noteOffset 'D' = 2
    noteOffset 'E' = 4
    noteOffset 'F' = 5
    noteOffset 'G' = 7
    noteOffset 'A' = 9
    noteOffset 'B' = 11
    noteOffset _   = 0

midiToPitch :: Int -> Pitch
midiToPitch midi =
  let (oct, note) = (midi `div` 12 - 1, midi `mod` 12)
      table = [ ('C', 0), ('C', 1), ('D', 0), ('D', 1), ('E', 0)
              , ('F', 0), ('F', 1), ('G', 0), ('G', 1), ('A', 0)
              , ('A', 1), ('B', 0) ]
      (l, acc) = table !! note
  in Pitch l acc oct

durationToBeats :: Duration -> Double
durationToBeats Whole = 4.0
durationToBeats Half = 2.0
durationToBeats Quarter = 1.0
durationToBeats Eighth = 0.5
durationToBeats Sixteenth = 0.25

beatsToSeconds :: Int -> Double -> Double
beatsToSeconds bpm beats = beats * (60.0 / fromIntegral bpm)
