# Advent of Code 2025

trying haskell

## Running Solutions

Build and run a specific day:

```bash
cabal run day-01
```

Or build all:

```bash
cabal build all
```

## Project Structure

```
day-01/Main.hs    # Solution for day 1
day-02/Main.hs    # Solution for day 2 (etc.)
inputs/day-01.txt # Puzzle input for day 1
```

## Adding a New Day

1. Create a new directory: `mkdir day-02`
2. Add `Main.hs` in that directory
3. Add the executable to `advent-of-code-2025.cabal`
4. Put your input in `inputs/day-02.txt`
