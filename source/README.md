# ARCHITECTURE

- Split:
  - normal operation/running: simple HTML + browser.
  - administration: will need something more sophisticated such as client side
    Javascript, most likely via one of the Haskell-to-JS projects.

- Hierarchy of .cfg files:  personal, group, site;  maybe generalised multiple
  levels, specified in the .conf

- As a site option, it will be necessary to allow creation of new users without
  an existing user .cfg and possibly selection from a list of data sources.

- Writeback of .cfg files during administrative change;  requires:
  - passthrough of XML without entity substitution etc.
  - configuration items (views, datasources etc.) must track their .cfg so
    that the correct .cfg can be rewritten.

- Testing framework -- query/response pairs.

- Notation for drilling into indirect data.

- Question deduplication.  This must consider both note data and views, since
  for example two question data could be distinct;  e.g., "copy" as a verb and
  as a noun;  provided that they are distinguished by colour or annotation,
  they are distinct.

- User configuration <include> scheme including filename stacking to detect recursion.

- Server configuration specifies:
    userdir=.
    usertemplate=default.cfg
  Copy defaultuser on creation of a new account;  or maybe <include> it.
  On modification, copy the <include>d file.

- One-to-many cards.

- Debug/info. Mode checks attributes: [optional missing] [optional present] [unknown-present]


# CODING AND LAYOUT STANDARDS

- No 'do'.
- single tab indentation.
