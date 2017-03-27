# CODING AND LAYOUT STANDARDS

- No 'do'.
- single tab indentation.
- prefer CPS to pattern-matching.
- Filenames ending in -Data.hs are persistent TH tables and minimal supporting code.
- Filenames ending in -Spec.hs are in-memory representations derived or obtained therefrom.

# PER-PLATFORM BUILD NOTES

These are for the author's systems and may require tweaking for other situations.

## CentOS 7 + stack
stack build --verbosity warn --allow-different-user --install-ghc --haddock && (cd ../sandbox;../source/.stack-work/install/x86_64-linux/lts-7.0/8.0.1/bin/JackRose -c ./jackrose.conf )

## NixOS + Nix
cabal2nix . >default.nix && nix-build JackRose.nix && (cd ../sandbox;/nix/store/wnpz8zwvmk96ad07190caqwgv2mhbqjs-JackRose-0.8/bin/JackRose -c jackrose.conf )

## Gentoo + cabal
cabal install && (cd ../sandbox/;../source/.cabal-sandbox/bin/JackRose -c jackrose.conf)
