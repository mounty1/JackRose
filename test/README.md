BUILDING, RUNNING AND TESTING JACKROSE
===========================================

The phases required are:

- Downloading and installing the building software;  mainly ghc and some
   platform-specific tools.
- Downloading and building JackRose itself.
- Running JackRose.
- Connecting to JackRose from a web browser.
- One-time database server set-up to run integration tests.
- Running the integration tests.

**Please send suggestions and errata to {the project name} at landcroft dot com.**
**Please also email if you get through all this lot and get JackRose running.**

# The building software

## On Centos 7

Install [Stack](https://docs.haskellstack.org/en/stable/README/)

## NixOS + Nix

Install packages `pkgs.ghc` `pkgs.git` `pkgs.ssmtp` `pkgs.gnumake` `pkgs.cabal2nix`.
If you want to run Postgres locally, install `pkgs.postgresql` as well.

## On Gentoo

    stack build

## Ubuntu, Mess Windoze etc.

Although I've not tried it, I assume the Haskell platform at
https://www.haskell.org/platform/ is the way to go here.


# Downloading and building JackRose itself

Download the software:

    $ git clone https://github.com/mounty1/JackRose.git
    $ cd JackRose

Modify test/jackrose.conf to taste.

You will need an interactive tool (psql or sqlitebrowser) for your chosen back-end.
For Postgres, the dataStore value must be of the form _dbi:Pg:arg=value[;...]_
For SQLite, it is just a file name.
If you choose Postgres, you must create a matching database, user and password.
Then proceed according to your platform, below.

## PER-PLATFORM BUILD NOTES

These are for the author's systems and the paths at least _will_ require tweaking for other situations.

### CentOS 7 + stack
    $ cd source ; stack build --verbosity warn --allow-different-user --install-ghc --haddock && (cd ../test;../source/.stack-work/install/x86_64-linux/lts-7.0/8.0.1/bin/JackRose -c ./jackrose.conf)

### NixOS + Nix
    $ cd test ; cabal2nix ../source >default.nix && nix-build JackRose.nix && (cd ../test;/nix/store/wnpz8zwvmk96ad07190caqwgv2mhbqjs-JackRose-0.8/bin/JackRose -c jackrose.conf)

### Gentoo + cabal
    $ cd source ; cabal sandbox init && cabal install && (cd ../test/;../source/.cabal-sandbox/bin/JackRose -c jackrose.conf)

## Running JackRose

The steps above get JackRose running.
The first time you do this you will see a series of messages about creating and migrating tables.

Connect to JackRose via a browser (e.g., http://192.168.1.113:3001).  A login screen should appear.
Create a new account.  If your email is configured correctly, you should receive a confirmation email
with a link to click.  If it's not, the link appears in the log output so copy and paste it into your browser.
If you can't for any reason, see below.

Stop JackRose (^C)

It is worth pointing-out that JackRose programmatically accesses two classes of data:
1.  One or more _data sources_, containing the data which you wish to memorise, and which you must make known to JackRose.
Generally these data are read-only to JackRose.  Most SRSs require that learning data be entered or imported into
their own format.  JackRose only requires to be informed of those data, which (currently) can
be in Postgres tables or views.
2.  JackRoses own housekeeping data, which it created in the _migration_ step above.

First set up some data to memorise.  This example uses the numbers one to ten in various European languages.
Set up a table in your choice of **Postgres** database:

    CREATE TABLE numbers (
        value integer NOT NULL PRIMARY KEY,
        english character varying NOT NULL,
        french character varying NOT NULL,
        german character varying NOT NULL,
        spanish character varying NOT NULL,
        greek character varying NOT NULL
    );

    INSERT INTO numbers (value, english, french, german, spanish, greek) VALUES (1, 'one', 'un', 'eins', 'uno', 'εἷς');
    INSERT INTO numbers (value, english, french, german, spanish, greek) VALUES (2, 'two', 'deux', 'zwei', 'dos', 'δύο');
    INSERT INTO numbers (value, english, french, german, spanish, greek) VALUES (3, 'three', 'trois', 'drei', 'tres', 'τρεῖς');
    INSERT INTO numbers (value, english, french, german, spanish, greek) VALUES (4, 'four', 'quatre', 'vier', 'cuatro', 'τέττᾰρες');
    INSERT INTO numbers (value, english, french, german, spanish, greek) VALUES (5, 'five', 'cinq', 'funf', 'cinco', 'πέντε');
    INSERT INTO numbers (value, english, french, german, spanish, greek) VALUES (6, 'six', 'six', 'sechs', 'seis', 'ἕξ');
    INSERT INTO numbers (value, english, french, german, spanish, greek) VALUES (7, 'seven', 'sept', 'sieben', 'siete', 'ἑπτᾰ́');
    INSERT INTO numbers (value, english, french, german, spanish, greek) VALUES (8, 'eight', 'huit', 'acht', 'ocho', 'ὀκτώ');
    INSERT INTO numbers (value, english, french, german, spanish, greek) VALUES (9, 'nine', 'neuf', 'neun', 'nueve', 'ἐννέᾰ');
    INSERT INTO numbers (value, english, french, german, spanish, greek) VALUES (10, 'ten', 'dix', 'zehn', 'die', 'δέκα');

This table can be on the same Postgres server as you use below for JackRoses own data, or a different one, but it must be
accessible to the running JackRose server and it must (for the time being) be Postgres.

This example is all from the point of view of an English speaker who wishes to learn non-English languages,
but you can change the _views_ below to fix that.

Now it is necessary to tell JackRose about this table and how we want to memorise the information in it.
When JackRose is a complete product there will be a nice friendly interface to configure all this but for
the time being you must tell it manually by populating its configuration tables.

Run your interactive database tool as user _jackrose_, connected to the server you configured as
the _dataStore_ in _jackrose.conf_ above.  You can see yourself in the _users_ table.
If you were unable to click on the confirmation link for any reason, you can fix this now by setting the _verified_ field:

    update "User" set verified=True;

Since this is a fresh installation, there should be only one row (you) in the _User_ table.

Next, tell JackRose about the 'data source';  i.e., the table containing data you wish to memorise.

    INSERT INTO data_source (id, accessor_write, accessor_read, name, source_serial, resynced) VALUES (1, 0, 0, 'numbers', 'P:services:::numbers:mounty:', '2017-03-28 06:08:58.628821+00');

The source_serial value is a colon-separated list of:
* P for Postgres
* server host name
* optional port number
* optional database name
* table name -- this must match the _numbers_ table you created above.
* user name under which to perform access
* optional password

Then set up how we want to memorise the data.  We don't want to memorise all combinations.
For example, in a school context, some pupils will be learning only French, some French and
German etc.  We insert a _view_ for each, which is somewhat analogous to a _card_ in Anki parlance.

    INSERT INTO view (id, name, data_source_id, obverse, reverse, style_c_s_s) VALUES (1, 'econosphere', 1, 'What is the German for <field name="english"/>', '<frontSide/><hr/><field name="german"/>', 'font-family: Code2000;
    font-size:24pt;
    background-color: #00ff80;
    text-align: center;');
    INSERT INTO view (id, name, data_source_id, obverse, reverse, style_c_s_s) VALUES (2, 'engtogreek', 1, 'What is the Greek for <field name="english"/>', '<frontSide/><hr/><field name="greek"/>', 'font-family: Code2000;
    font-size:24pt;
    text-align: center;
    background-color: #00ff80;');
    
That last column is obviously some CSS to apply to the questions and answers.   Put it all on one line if you prefer.
Omit or change the _font-family_ value if you don't have the Code2000 font.
The obverse and reverse columns are strict XHTML.

Now specify for yourself which 'views' you want to see.
If you don't want to learn the (Attic) Greek numbers for example, omit the second row.

    INSERT INTO user_deck_end (view_id, "user", parent, throttle, shuffle) VALUES (1, 1, NULL, NULL, NULL);
    INSERT INTO user_deck_end (view_id, "user", parent, throttle, shuffle) VALUES (2, 1, NULL, NULL, NULL);

With this setup we are not using the German and Spanish columns but you can set up views for them if you wish.

Quit the interactive tool and start the JackRose server again.  This time you will see it print the rows from the _numbers_
table as it adds them to its internal data.  Currently JackRose only scans the table for additions and deletions when you
restart it but eventually there will be other means for telling it to resynchronise.  It only does this once;  if you restart JackRose
again, it won't print the rows again.

Now start the browser.  It goes without saying that the browser does not have to be on the same machine as
that on which the JackRose server is running.  The browser must understand the CSS _flex_ attribute.  Qupzilla and Firefox
do.  Chrome and Chromium _should_.  Connect to the JackRose server.  In development here the URL is
http://192.168.1.113:3001 and you must of course specify the port number, if it's other than the default 80.

You should then be able to log in and immediately begin memorisation.  JackRose will want to give you all twenty items, one
after the other so until the daily _throttle_ is implemented, you must just stop (logout) when you've had enough.  After that,
the process is just as with any other SRS:  log in, do your repetitions, log out.  Score: 0 - clueless;  6 - eventually;  7 - normal
and 9 - quick-fire.

One difference that might surprise is that JackRose computes intervals in seconds, not days.  If you do your repetitions at
one time each day, this will not affect you.  It does mean that if you log in again a second time on the same day,
you will likely see some repetitions.  If you don't like this, don't log in for the second time.

## Read-only access to Postgres tables

[This is how](http://jamie.curle.io/posts/creating-a-read-only-user-in-postgres/).

# Continuous Integration (CI) Testing

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
    $ chmod 0400 $HOME/.pgpass

# Prepare JackRoses own data

Load the data and apply time offset.  Obviously, the base data were produced at some instant,
but time never stops, and we want to run the test as though the data were current.  One way
would be for JackRose to take a 'time offset' parameter but that would make testing different
from normal usage so we adjust all time-stamps to make the base-data as-of the present.
Postgres doesn't allow for global constants so the timeshift function is defined but this
will produce a slightly different result each time.  Since the data set is small, this doesn't
matter in practice.  Investigate psql constants (_-v_ command-line option).

    $ psql -h services -U jackrose -d jr_ci -q -f setup.sql

# Build the software

    $ cabal2nix ../source >default.nix && nix-build JackRose.nix
