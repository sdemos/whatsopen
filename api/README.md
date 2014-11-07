API
---

The API is currently planned to have these calls - 

 * GET /open/{epoch timestamp} - returns json array of locations that are open with some other information, like time it closes, how much time until then, and some other stuff I guess, like the name probably. Maybe the location string. 
 * GET /hours/{location}/{epoch timestamp} - I don't know the structure of this one yet. It will return the whole location object, with the hours. I don't know if this is going to be a day per call or a week, probably one of those two, maybe both. I also don't know how I am going to encode the location. 
 * GET /locations - returns json array of all of the names of the locations. Might be a list of objects with encoded names or an object with id, pretty name, location, and any other relevant information. 

Get Developin'
--------------

To set up the haskell api development environment, you have to use cabal >= 1.18 for sandboxing. I'm sure cabal-dev would work too, but I'm not using that. 

    cabal sandbox init
    cabal install --dependencies-only
    cabal build
