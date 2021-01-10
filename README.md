Spaced Repetition Software as a web service
===========================================

Project status:  **alpha**.  You can run it if you are prepared to do some work.
In particular, you'll need a running Postgres server and will have to run some SQL statements.

Implemented using the Yesod framework.

[Building and running instructions](test/README.md).
[Introductory page](http://www.landcroft.com/jackrose).
[Haddock](http://www.landcroft.com/jackrose/documentation/doc/html/JackRose/JackRose/index.html).

As this is a learning exercise for the author:
- It is likely that best practice is not followed in some areas.
- Code will be rewritten in progressively higher-level constructions.
- Patches and comments are welcome, provided they are explained.
- The Yesod scaffolding system is not of interest.

Now that the project has become feasibly useable:
- I will _try_ to ensure that any database migration is automatic.
- As soon as CI is complete, I'll only push to github when the tests pass.
- I'll keep Haddock up to date.

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
> stack exec -- haddock --html --optghc=-fno-warn-tabs source/*.hs --hyperlinked-source --odir=site/documentation/
