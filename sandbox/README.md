# DON'T USE THIS YET -- UPLOADED JUST TO CHECK MARK-UP ONLINE.  PLEASE BE PATIENT.   WE GO LIVE SOON!

# RUNNING JACKROSE

The phases required are:

- Downloading and installing the building software;  mainly ghc and some
   platform-specific tools.
- Downloading and building JackRose itself.
- Running JackRose.
- Connecting to JackRose from a web browser.

These notes are from memory and are most likely incomplete;  please send
suggestions and errata to {the project name} at landcroft dot com.

# The building software

## On Centos 7

Install [Stack](https://docs.haskellstack.org/en/stable/README/)

## NixOS + Nix

Install packages `pkgs.ghc` `pkgs.git` `pkgs.gnumake` `pkgs.cabal2nix`.
If you want to run Postgres locally, install `pkgs.postgresql` as well.

## On Gentoo

> $ emerge ghc cabal

## Ubuntu, Mess Windoze etc.

Although I've not tried it, I assume the Haskell platform at
https://www.haskell.org/platform/ is the way to go here.


# Downloading and building JackRose itself

Download the software:
    $ git clone https://github.com/mounty1/JackRose.git
    $ cd JackRose

Modify sandbox/jackrose.conf to task.

If you choose Postgres for JackRoses own data store (the dataStore value) you must
create a suitable user with password in your Postgres instance.
_You will need an interactive tool (psql or sqlitebrowser) for your chosen back-end_.
For Postgres, the dataStore value must be of the form _dbi:Pg:arg=value[;...]_
For SQLite, it is just a file name.

    $ cd source

Then proceed according to your platform, below.

## PER-PLATFORM BUILD NOTES

These are for the author's systems and the paths at least _will_ require tweaking for other situations.

### CentOS 7 + stack
    $ stack build --verbosity warn --allow-different-user --install-ghc --haddock && (cd ../sandbox;../source/.stack-work/install/x86_64-linux/lts-7.0/8.0.1/bin/JackRose -c ./jackrose.conf)

### NixOS + Nix
    $ cabal2nix . >default.nix && nix-build JackRose.nix && (cd ../sandbox;/nix/store/wnpz8zwvmk96ad07190caqwgv2mhbqjs-JackRose-0.8/bin/JackRose -c jackrose.conf)

### Gentoo + cabal
    $ cabal sandbox init && cabal install && (cd ../sandbox/;../source/.cabal-sandbox/bin/JackRose -c jackrose.conf)

## Running JackRose

The steps above get JackRose running.
The first time you do this you will see a series of messages about creating and migrating tables.
As JackRose is still alpha, it is necessary to insert some data manually.

* Connect to JackRose via a browser (e.g., http://192.168.1.113:3001).  A login screen should appear.
Create a new account.  If your email is configured correctly, you should receive a confirmation email
with a link to click.  If it's not, the link appears in the log output so copy and paste it into your browser.
If you can't for any reason, see below.

* Stop JackRose (^C)

It is worth pointing-out that JackRose programmatically accesses two classes of data:
1.  One or more _data sources_, containing the data which you wish to memorise, and which you must make known to JackRose.
Generally these data are read-only to JackRose.  Most SRSs require that learning data be entered or imported into
their own format.  JackRose only requires to be informed of those data, which (currently) can
be in Postgres tables or views.
2.  JackRoses own housekeeping data, which you created in the _migration_ step above, and which you are now viewing,
    in database _jackrose_.

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

This table can be on the same Postgres server (or a different one) as you use below for JackRoses own data but it must be
accessible to the running JackRose server and it must (for the time being) be Postgres.

This example is all from the point of view of an English speaker who wishes to learn non-English languages.

Now it is necessary to tell JackRose about this table and how we want to memorise the information in it.
When JackRose is a complete product there will be a nice friendly interface to configure all this but for
the time being you must populate its tables manually.

Run your interactive database tool as user _jackrose_, connected to the Postgres server you configured as the _dataStore_
in _jackrose.conf_ above.  You can see yourself in the _users_ table.
If you were unable click on the confirmation link for any reason, you can fix this now by setting the _verified_ field:

    update "User" set verified=True;

Since this is a fresh installation, there should be only one row (you) in the _User_ table.

First tell JackRose about the 'data source';  i.e., the table containing data you wish to memorise.
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
German etc.  We insert a view for each, which is somewhat analogous to a _card_ in Anki parlance.
    INSERT INTO view (id, name, data_source_id, obverse, reverse, style_c_s_s) VALUES (1, 'econosphere', 1, 'What is the German for <field name="english"/>', '<frontSide/><hr/><field name="german"/>', 'font-family: Code2000;
    font-size:24pt;
    background-color: #00ff80;
    text-align: center;');
    INSERT INTO view (id, name, data_source_id, obverse, reverse, style_c_s_s) VALUES (2, 'engtogreek', 1, 'What is the Greek for <field name="english"/>', '<frontSide/><hr/><field name="greek"/>', 'font-family: Code2000;
    font-size:24pt;
    text-align: center;
    background-color: #00ff80;');
That last column is obviously some CSS to apply to the questions and answers.   You can put it all on one line if you prefer.
Omit or change the _font-family_ value if you don't have the Code2000 font.
The obverse and reverse columns are strict XHTML.

Now specify for yourself which 'views' you want to see.
If you don't want to learn the (Attic) Greek numbers for example, omit the second row.
    INSERT INTO user_deck_end (view_id, "user", parent, throttle, shuffle) VALUES (1, 1, NULL, NULL, NULL);
    INSERT INTO user_deck_end (view_id, "user", parent, throttle, shuffle) VALUES (2, 1, NULL, NULL, NULL);
With this setup we are not using the German and Spanish columns but you can set up a view for them if you wish.

Quit the interactive tool and start the JackRose server again.  This time you will see it print the rows from the _numbers_
table as it adds them to its internal data.  Currently JackRose only scans the table for additions and deletions when you
restart it but eventually there will be other means for telling it to resynchronise.  It only does this once;  if you restart JackRose
again, it won't print the rows again.

Finally, start the browser.  It goes without saying that the browser does not have to be on the same machine as
that on which the JackRose server is running.  The browser must understand the CSS _flex_ attribute.  Qupzilla and Firefox
do.  Chrome and Chromium _should_.  Connect to the JackRose server.  In development here the URL is
http://192.168.1.113:3001 and you must of course specify the port number, if it's other than the default 80.

You should then be able to log in and immediately begin memorisation.  JackRose will want to give you all twenty items, one
after the other so until the daily _throttle_ is implemented, you must just stop (logout) when you've had enough.  After that,
the process is just as with any other SRS:  log in, do your repetitions, log out.

One difference that might surprise is that JackRose computes intervals in seconds, not days.  If you do your repetitions at
one time each day, this will not affect you.  It does mean that if you log in again a second time, you might see some repetitions.
If you don't like this, don't log in for the second time.

## Read-only access to Postgres tables

Note for file.  See [this](http://jamie.curle.io/posts/creating-a-read-only-user-in-postgres/).
