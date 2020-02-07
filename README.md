# dominion-api

An API for searching [Dominion](https://en.wikipedia.org/wiki/Dominion_(card_game)) cards, hopefully to be of use to those developing any applications based on the game.

I hope it will be useful but the main motivation for this is, after a year or so of immersing myself in Haskell, to try to actually build a "real" application with it. The main technologies used are:
- the [stack](https://docs.haskellstack.org/en/stable/README/) tool to build the project and manage dependencies in a sand-boxed environment
- the incredible [servant](https://haskell-servant.readthedocs.io/en/stable/index.html) package which provides a type-level (and therefore totally type-safe) DSL for building APIs
- [Persistent](https://www.yesodweb.com/book/persistent) and [Esqueleto](https://www.stackage.org/package/esqueleto) to handle the database

As of now (7/2/20), the API is mostly finished - I'm slowly adding tests and want to tidy things up a bit (both the code, and the API schema). Doing the tests first so that I have some confidence that changes haven't broken it!
