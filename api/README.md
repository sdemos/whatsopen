API
---

Currently, the API has two calls - 

 * GET / - returns an array of JSON objects corresponding to open locations,
   with name, location, openFor, and openTill fields. 
 * GET /open/{timestamp} - returns a list of open locations much like /, except for
   a specific time instead of the current time. 

I am planning on adding these calls eventually - 

 * GET /hours/{location} - returns the hours of a particular location. This
   should probably only include the general hours, rather than any special
   holiday hours associated with the location.
 * GET /hours/{location}/{timestamp} - returns an array of the hours on this
   day for this location
 * GET /stores - returns a list of locations we have information for
 * GET /stores/{location} - returns information about a particular store. I'm
   not sure yet of the scope of information that should be included in this.
   I think given the /hours/{location} call this should just include general
   information about the location.
 * A whole host of POST, PUT, and DELETE calls to go along with an admin page
   so I can stop inputing hours by hand into my db.

Timestamp
---------

The timestamp mentioned in the API calls are structured like this - YYMMDDHHMM

In particular, I use the Haskell
[time](https://hackage.haskell.org/package/time) library's parser with the
format string "%y%m%d%H%M"

Get Developin'
--------------

If for some contrived reason you want to help develop whatsopen, here are the
instructions to do it. 

I use stack for building and running the project. Run

    stack install
    PORT=8000 whatsopenapi

and you will get whatopen running on port 8000.
