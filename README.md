Codenames Board Generator
=========================

Generates [Codenames][codenames] and [Codenames Duet][codenames-duet]
boards.

For Codenames proper, the generated board should be self-explanatory;
the colored bars at the top and bottom show which side needs to go
first.

For Codenames Duet, the idea is to generate the board for player 1,
then tell the seed to player 2 who can then enter the same seed on her
device. When a new seed is entered, the page automatically shifts to
the other player's view, so there should be no spoilers displayed.


Technology
----------

* Implemented in [Idris][idris]
* [`idris-js`][idris-js] for interfacing with the DOM
* `contrib` for `ST` as used by `idris-js` 

[Live demo][demo]
=================

[codenames]:      https://boardgamegeek.com/boardgame/178900/codenames
[codenames-duet]: https://boardgamegeek.com/boardgame/224037/codenames-duet
[idris]:          http://idris-lang.org/
[idris-js]:       https://github.com/rbarreiro/idrisjs
[demo]:           https://unsafePerform.IO/projects/codenames/
