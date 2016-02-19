WhatsOpen (whatsopen.csh.rit.edu)
=================================

As a college student, I don't plan my days in advance. I do whatever I feel
like, whenever I feel like. If I think to myself "I want to shove
semi-nutritious grey matter into my mouth hole," I don't want to wade through
garbage information on the ugly and non-functional dining services webpage just
to find out what places are open, and I certainly don't want to figure out what
time it is or, God forbid, do math to figure out how much time I have left to
get there. 

What I want is a place that tells me what is open now, and for how long they
will stay open. And that's it. I don't care about things that are already
closed. I refuse to navigate farther than the homepage for something so simple.
This is made for me. Use it if you want. 

Structure --------- After several (incomplete) iterations of whatsopen, in
multiple languages, I finally settled on making a RESTful API in Haskell using
[Servant](https://github.com/haskell-servant/servant), using the
[Warp](https://hackage.haskell.org/package/warp) web server and the
[Shakespeare](https://hackage.haskell.org/package/shakespeare) templating
engine. The frontend is written in [Twitter
Bootstrap](http://getbootstrap.com/) using Ben Centra's
[bootstrap-csh](https://github.com/bencentra/bootstrap-csh) for the CSH colors. 

The separate backend means that all of the calculation and datastorage can be
separated from the frontend, that way if someone else wants to invest in their
future laziness like I did, they can get json based information on whatsopen
right now, and probably some other stuff, and use it in their own thing. 

All hours are stored in the database by having generic hours for each week, and
then having a table of exceptions, like holiday hours. 

API
---

Currently, the API has two calls - 

 * GET / - returns an array of JSON objects corresponding to open locations,
   with name, location, openFor, and openTill fields. 
 * GET /open/{timestamp} - returns a list of open locations much like /, except for
   a specific time instead of the current time. 
 * GET /stores - returns a list of locations we have information for

I am planning on adding these calls eventually - 

 * GET /hours/{location} - returns the hours of a particular location. This
   should probably only include the general hours, rather than any special
   holiday hours associated with the location.
 * GET /hours/{location}/{timestamp} - returns an array of the hours on this
   day for this location
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
