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
