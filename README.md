# dominion-api

An API for searching [Dominion](https://en.wikipedia.org/wiki/Dominion_(card_game)) cards, hopefully to be of use to those developing any applications based on the game.

I hope it will be useful but the main motivation for this is, after a year or so of immersing myself in Haskell, to try to actually build a "real" application with it. The main technologies used are:
- the [stack](https://docs.haskellstack.org/en/stable/README/) tool to build the project and manage dependencies in a sand-boxed environment
- the incredible [servant](https://haskell-servant.readthedocs.io/en/stable/index.html) package which provides a type-level (and therefore totally type-safe) DSL for building APIs
- [Persistent](https://www.yesodweb.com/book/persistent) to handle the database


## Todo List

At the time of writing this is at a very early stage - the basic API functionality is working but there is a lot to do before this will be ready to deploy to a production environment. The following is what I expect to be an ever-changing (and hopefully generally decreasing!) list of things still to do:

- Add a "comesWith" field (or some other name) which shows, for Kingdom cards, any other cards that come into play with them, and conversely for non-Kingdom cards, which Kingdom cards can bring them into play. Will also be useful for split piles.
- Add defaults to some fields: for example make costInPotions and costInDebt automatically false if they are not supplied but costInCoins is
- Change the database backend from SQLite to Postgres
- Add authentication to the POST/PUT/DELETE routes to add, update and delete cards (once done, the card database will not need updating except if more cards are released - this is not a huge task and I am happy to manage it myself, and I would like to do it by POSTing to the API but obviously need to protect it from vandalism when this is live)
- Add documentation, probably using [Servant.Docs](https://haskell-servant.readthedocs.io/en/stable/tutorial/Docs.html)
- make the query parameters more userfriendly (allow kebab-case instead of CamelCase, change eg CanSometimes to just "sometimes")
