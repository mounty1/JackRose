# ARCHITECTURE

This is random jottings which eventually will be change requests.

- Suppression of logging by level.

- Duplication of primary key?

- Split:
  - normal operation/running: simple HTML + browser.
  - administration: will need something more sophisticated such as client side
    Javascript, most likely via one of the Haskell-to-JS projects.

- As a site option, it will be necessary to allow creation of new users without
  an existing user .cfg and possibly selection from a list of data sources.

- Notation for drilling into indirect data.

- Question deduplication.  This must consider both note data and views, since
  for example two question data could be distinct;  e.g., "copy" as a verb and
  as a noun;  provided that they are distinguished by colour or annotation,
  they are distinct.

- User configuration <include> scheme including filename stacking to detect recursion.

- One-to-many cards.

- http://www.nifdi.org/resources/free-downloads/suggested-reading/white-papers-by-zig/900-student-program-alignment-and-teaching-to-mastery-by-siegfried-engelmann/file

- Debug/info. Mode checks attributes: [optional missing] [optional present] [unknown-present].

- Security Model. Configurable, hierarchical write rights to data.

- Partial card ordering, to impose learning of basic material before more derived or advanced
  material is presented, as [Anki](https://ankiweb.net/shared/info/699486759).
  http://sfondilias.com/publications/calculus1987.pdf

- Security:  whitelist of tags with whitelist of attributes to pass.  Greylist of tags to elide.  Get the
  whitelist [here][https://github.com/peerlibrary/peerlibrary/issues/558)

- Security:  https://github.com/pillarjs/understanding-csrf
https://www.owasp.org/index.php/Cross-Site_Request_Forgery_%28CSRF%29_Prevention_Cheat_Sheet

- Security:  HPKP http://www.theregister.co.uk/2016/03/24/see_a_pin_and_pick_it_up_for_the_sake_of_security/

# CODING AND LAYOUT STANDARDS

- No 'do'.
- single tab indentation.
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
