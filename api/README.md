API
---

Currently, the API has two calls - 

 * GET /open - returns an array of JSON objects corresponding to open locations, with name, location, openFor, and openTill fields. 
 * GET /open/{timestamp} - returns a list of open locations much like /open, except for a specific time instead of the current time. 

I am planning on adding these calls eventually - 

 * GET /hours/{location}/{timestamp} - returns an array of the hours on this day for this location
 * GET /locations - returns a list of locations we have information for

Timestamp
---------

The timestamp mentioned in the API calls are structured like this - YYMMDDHHMM

In particular, I use the Haskell [time](https://hackage.haskell.org/package/time) library's parser with the format string "%y%m%d%H%M"

Get Developin'
--------------

If for some contrived reason you want to help develop whatsopen, here are the instructions to do it. 

To set up the haskell api development environment, you have to use cabal >= 1.18 for sandboxing. I'm sure cabal-dev would work too, but I'm not using that. 

    cabal sandbox init
    cabal install --dependencies-only
    cabal build
