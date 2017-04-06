# CODING AND LAYOUT STANDARDS

- No 'do'.
- No partial functions:  mainly replace head with uncons
- Single tab indentation.
- Clean pass of hlint.
- Prefer CPS to pattern-matching.
- Filenames ending in -Data.hs are persistent TH tables and minimal supporting code.
- Filenames ending in -Spec.hs are in-memory representations derived or obtained therefrom.

# HADDOCK

## Gentoo
> $ cabal haddock --executables --builddir=../site/documentation
