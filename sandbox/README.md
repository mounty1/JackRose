# RUNNING JACKROSE

The phases required are:

- Downloading and installing the building software;  mainly ghc and some
   platform-specific tools.
- Downloading and building JackRose itself.
- Running JackRose.
- Connecting to JackRose from a web browser.


# THE DETAIL

These notes are from memory and are most likely incomplete;  please send
suggestions and errata to {the project name} at landcroft dot com.


## The building software

### On Centos 7

Use Halcyon:  https://halcyon.sh/

Halcyon is great;  it Just Works and the developer Mietek is very helpful and
friendly.

    # git clone https://github.com/mietek/halcyon.git
    ...
    # cd JackRose/source
    # halcyon install

### On Gentoo

emerge ghc cabal


### Ubuntu, Mess Windoze etc.

Although I've not tried it, I assume the Haskell platform at
https://www.haskell.org/platform/ is the way to go here.


## Downloading and building JackRose itself

    $ git clone https://github.com/mounty1/JackRose.git
    $ cd JackRose/source
    $ cabal update
    $ cabal install --builddir=gentoo

Then cabal install the missing/required packages.
The idea of that builddir option is that if you are building from the same tree
on Centos 7 then Halcyon uses the default dist/ directory which should be
entirely separate from the build on other platforms.


## Running JackRose

    $ cd JackRose/sandbox
    Modify jackrose.conf to taste.
    $ mv default.cfg {yourloginname}.cfg

When you connect to JackRose, you will have to set up an account.
Currently, the only way of configuring your account is to have a server-side
configuration file of the same name as your login.
You are doing this step before you have logged-in;  i.e., before you have
actually created the account.

Modify {yourloginname}.cfg to taste.

Finally, run JackRose:

    Centos7# cd JackRose/sandbox ; /app/bin/JackRose -c ./jackrose.conf
    Gentoo$  cd JackRose/sandbox ; ../source/gentoo/dist/JackRose/JackRose -c ./jackrose.conf


## Connecting to JackRose from a web browser

In the simple case, open your browser and navigate to http://localhost:3000

Then create your account, using the same login as the configuration XML you
just modified.  You should then see the question item as set up in your
configuration XML.  So far, that's all it does.