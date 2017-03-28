# CODING AND LAYOUT STANDARDS

- No 'do'.
- single tab indentation.
- prefer CPS to pattern-matching.
- Filenames ending in -Data.hs are persistent TH tables and minimal supporting code.
- Filenames ending in -Spec.hs are in-memory representations derived or obtained therefrom.

# HADDOCK

## Gentoo
> $ cabal haddock --executables --builddir=../site/documentation
