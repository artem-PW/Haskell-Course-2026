# MusicGenLang

A small DSL for algorithmic music composition.
Written in Haskell as a project for the Functional Programming course.

The system takes a text score as input, parses it into an AST,
renders it into a flat list of timed events, and optionally exports a MIDI file.

## How to build

```
stack build
```

## How to run tests

```
stack test
```

45 tests total: 30 unit tests, 9 end-to-end tests, 6 QuickCheck property-based tests.

## How to run the demo

```
stack run
```

Parses a sample score, prints all timed events, and writes `output.mid`.

## How to use interactively

```
stack repl
```

Then try:

```haskell
parseScore "song \"X\" tempo 120 { instrument piano { C4 q E4 q G4 q } }"

renderScore <$> parseScore "song \"X\" tempo 120 { instrument piano { repeat 2 { C4 q E4 q } } }"

totalDuration <$> parseScore "song \"X\" tempo 120 { instrument piano { C4 w } }"
```

Exit with `:q`.

## Syntax

```
song "Title" tempo BPM {
  instrument name {
    C4 q          -- note: pitch + duration
    r q           -- rest
    repeat 3 { }  -- repeat a block N times
    group { }     -- group items together
    transpose +5 { }  -- shift pitches by N semitones
    -- this is a comment
  }
}
```

Pitches: `C D E F G A B` with optional `#` (sharp) or `b` (flat), then octave number.
Durations: `w` (whole), `h` (half), `q` (quarter), `e` (eighth), `s` (sixteenth).

## Project structure

```
src/Types.hs    -- core data types (Score, Part, Item, Pitch, Duration, Event)
src/Parser.hs   -- monadic parser (StateT String [])
src/Engine.hs   -- interpreter: Score -> [Event]
src/Pretty.hs   -- pretty-printer (for round-trip tests)
src/Midi.hs     -- MIDI file export (stretch goal)
app/Main.hs     -- demo application
test/Spec.hs    -- HSpec + QuickCheck test suite
```
