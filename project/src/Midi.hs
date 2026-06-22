module Midi
  ( exportMidi
  , writeMidi
  ) where

import Types (Event (..))

import qualified Data.ByteString as BS
import Data.Bits ((.&.), shiftR)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word8)

ticksPerQuarter :: Int
ticksPerQuarter = 480

midiChannel :: Word8
midiChannel = 0

exportMidi :: Int -> [Event] -> BS.ByteString
exportMidi bpm events =
  let trackData = buildTrack bpm events
      header = midiHeader
      track = midiTrack trackData
  in BS.concat [header, track]

writeMidi :: FilePath -> Int -> [Event] -> IO ()
writeMidi path bpm events = BS.writeFile path (exportMidi bpm events)

midiHeader :: BS.ByteString
midiHeader = BS.pack $
     [0x4D, 0x54, 0x68, 0x64]
  ++ int32 6
  ++ int16 0
  ++ int16 1
  ++ int16 ticksPerQuarter

midiTrack :: BS.ByteString -> BS.ByteString
midiTrack trackBytes = BS.pack
     ([0x4D, 0x54, 0x72, 0x6B]
  ++ int32 (BS.length trackBytes))
  `BS.append` trackBytes

data MidiEvent = MidiEvent
  { meTick  :: Int
  , meBytes :: [Word8]
  }

buildTrack :: Int -> [Event] -> BS.ByteString
buildTrack bpm events =
  let usPerQuarter = 60000000 `div` bpm
      tempoEvent = MidiEvent 0 (tempoMeta usPerQuarter)
      noteEvents = concatMap (eventToMidi bpm) events
      sorted = sortBy (comparing meTick) (tempoEvent : noteEvents)
      deltas = toDelta 0 sorted
      endTrack = vlq 0 ++ [0xFF, 0x2F, 0x00]
  in BS.pack (deltas ++ endTrack)

eventToMidi :: Int -> Event -> [MidiEvent]
eventToMidi bpm ev =
  let onTick = secondsToTicks bpm (eventTime ev)
      offTick = secondsToTicks bpm (eventTime ev + eventDuration ev)
      p = clamp 0 127 (eventPitch ev)
      v = clamp 0 127 (eventVelocity ev)
      noteOn  = MidiEvent onTick  [0x90 + midiChannel, fromIntegral p, fromIntegral v]
      noteOff = MidiEvent offTick [0x80 + midiChannel, fromIntegral p, 0]
  in [noteOn, noteOff]

toDelta :: Int -> [MidiEvent] -> [Word8]
toDelta _ []       = []
toDelta prev (e : es) =
  let dt = meTick e - prev
  in vlq dt ++ meBytes e ++ toDelta (meTick e) es

tempoMeta :: Int -> [Word8]
tempoMeta us = [0xFF, 0x51, 0x03]
  ++ [ fromIntegral ((us `shiftR` 16) .&. 0xFF)
     , fromIntegral ((us `shiftR`  8) .&. 0xFF)
     , fromIntegral ( us              .&. 0xFF) ]

secondsToTicks :: Int -> Double -> Int
secondsToTicks bpm secs =
  round (secs * fromIntegral bpm * fromIntegral ticksPerQuarter / 60.0)

vlq :: Int -> [Word8]
vlq n
  | n < 0 = vlq 0
  | n < 0x80  = [fromIntegral n]
  | otherwise = go n []
  where
    go 0 acc = acc
    go v [] = go (v `shiftR` 7) [fromIntegral (v .&. 0x7F)]
    go v acc = go (v `shiftR` 7) (fromIntegral (v .&. 0x7F + 0x80) : acc)

int16 :: Int -> [Word8]
int16 n =
  [ fromIntegral ((n `shiftR` 8) .&. 0xFF)
  , fromIntegral ( n             .&. 0xFF) ]

int32 :: Int -> [Word8]
int32 n =
  [ fromIntegral ((n `shiftR` 24) .&. 0xFF)
  , fromIntegral ((n `shiftR` 16) .&. 0xFF)
  , fromIntegral ((n `shiftR`  8) .&. 0xFF)
  , fromIntegral ( n              .&. 0xFF) ]

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)
