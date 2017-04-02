Continuous Integration (CI) Testing
===========================================

This is a WIP.  Incomplete.  Don't use it yet.

Integration / release testing is based on [Hydra](https://nixos.org/hydra) running on [NixOS](https://nixos.org).

To run the test-suite without user interaction, you must creat a Postgres database called *jr_ci*
and configure it for passwordless access.  These instructions are for:

* A Postgres server on host _services_ that is configured to require passwords.
* A Postgres account _jackrose_ with password _test_.

To be really sure, we ought to repeat the testing with a SQLite back-end but this would require
different setting-up and we don't do it, currently.

# One-time setup database

In _psql_:

    create database jr_ci owner = jackrose encoding = 'UTF8' template template0;

Supply password so that CI is not interactive.

    $ echo services:'*':jr_ci:jackrose:test >$HOME/.pgpass

# Prepare JackRoses own data

Load the data and apply time offset.  Obviously, the base data were produced at some instant,
but time never stops, and we want to run the test as though the data were current.  One way
would be for JackRose to take a 'time offset' parameter but that would make testing different
from normal usage so we adjust all time-stamps to make the base data as-of the present.
Postgres doesn't allow for global constants so the timeshift function is defined but this
will produce a slightly different result each time.  Since the data set is small, this doesn't
matter in practice.  Investigate psql constants (_-v_ command-line option).

    $ psql -h services -U jackrose -d jr_ci -f dump.text
    $ psql -h services -U jackrose -d jr_ci -f timeshift.text

# Build the software

    $ cabal2nix ../source >default.nix && nix-build JackRose.nix
