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

- http://www.nifdi.org/resources/free-downloads/suggested-reading/white-papers-by-zig/900-student-program-alignment-and-teaching-to-mastery-by-siegfried-engelmann/file

- layered or leveled cards:  http://sfondilias.com/publications/calculus1987.pdf

- Debug/info. Mode checks attributes: [optional missing] [optional present] [unknown-present].

- Security Model. Configurable, hierarchical write rights to data.

- Partial card ordering, to impose learning of basic material before more derived or advanced
  material is presented, as [Anki](https://ankiweb.net/shared/info/699486759).

- Security:  whitelist of tags with whitelist of attributes to pass.  Greylist of tags to elide.  Get the
  whitelist [here][https://github.com/peerlibrary/peerlibrary/issues/558)

- Security:  https://github.com/pillarjs/understanding-csrf
https://www.owasp.org/index.php/Cross-Site_Request_Forgery_%28CSRF%29_Prevention_Cheat_Sheet

- Security:  HPKP http://www.theregister.co.uk/2016/03/24/see_a_pin_and_pick_it_up_for_the_sake_of_security/

# CODING AND LAYOUT STANDARDS

- No 'do'.
- single tab indentation.
