API
---

Currently, the API has one call - 

 * GET /open - returns an array of JSON objects corresponding to open locations, with name, location, openFor, and openTill fields. 

I am planning on adding these calls eventually - 

 * GET /open/{timestamp} - returns a list of open locations much like /open, except for a specific time instead of the current time. 
 * GET /hours/{location}/{timestamp} - returns an array of the hours on this day for this location
 * GET /locations - returns a list of locations we have information for

Get Developin'
--------------

If for some contrived reason you want to help develop whatsopen, here are the instructions to do it. 

To set up the haskell api development environment, you have to use cabal >= 1.18 for sandboxing. I'm sure cabal-dev would work too, but I'm not using that. 

    cabal sandbox init
    cabal install --dependencies-only
    cabal build
