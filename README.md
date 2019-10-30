# dominion-api

An API for searching [Dominion](https://en.wikipedia.org/wiki/Dominion_(card_game)) cards, hopefully to be of use to those developing any applications based on the game.

I hope it will be useful but the main motivation for this is, after a year or so of immersing myself in Haskell, to try to actually build a "real" application with it. The main technologies used are:
- the [stack](https://docs.haskellstack.org/en/stable/README/) tool to build the project and manage dependencies in a sand-boxed environment
- the incredible [servant](https://haskell-servant.readthedocs.io/en/stable/index.html) package which provides a type-level (and therefore totally type-safe) DSL for building APIs
- [Persistent](https://www.yesodweb.com/book/persistent) to handle the database


## Todo List

At the time of writing this is at a very early stage - the basic API functionality is working but there is a lot to do before this will be ready to deploy to a production environment. The following is what I expect to be an ever-changing (and hopefully generally decreasing!) list of things still to do:


- change to use SQL Joins (probably with Esqueleto), so it doesn't take O(n) time to respond with a list of cards
[I had thought the performance loss from not doing so would be minimal, but in fact it's huge - even with only 20-30 cards in the database any operation that selects more than a few takes a completely unacceptable length of time to complete]
- Add documentation, probably using [Servant.Docs](https://haskell-servant.readthedocs.io/en/stable/tutorial/Docs.html)
- Add tests!
- make any error messages (eg. requests for a card name that doesn't exist) return them as JSON via an "error" key or similar
