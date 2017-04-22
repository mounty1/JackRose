# CODING AND LAYOUT STANDARDS

- No 'do'.
- No [partial functions](https://wiki.haskell.org/Avoiding_partial_functions):  mainly replace 'head' with 'uncons'.
- Single tab indentation.
- Clean pass of hlint.
- Prefer CPS to pattern-matching.
- Filenames ending in -Data.hs are persistent TH tables and minimal supporting code.

# HADDOCK

## Gentoo
> $ cabal haddock --executables --builddir=../site/documentation
