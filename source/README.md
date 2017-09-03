# CODING AND LAYOUT STANDARDS

- No anonymous name importation;  either import qualified or import an explicit list.
- No 'do'.
- No [partial functions](https://wiki.haskell.org/Avoiding_partial_functions):  mainly replace 'head' with 'uncons'.
- Single tab indentation.
- Clean pass of [hlint](https://github.com/ndmitchell/hlint).
- Clean pass of [weeder](https://github.com/ndmitchell/weeder).
- Prefer CPS to pattern-matching.
- Filenames ending in -Data.hs are persistent TH tables and minimal supporting code.
- Do not use extra spaces to align related items vertically.

# HADDOCK

## Gentoo
> $ cabal haddock --executables --builddir=../site/documentation
Currently, Gentoo is the only platform that supports Haddock on the author's systems.
