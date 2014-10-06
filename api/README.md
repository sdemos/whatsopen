WhatsOpen (whatsopen.csh.rit.edu)
=================================

As a college student, I don't plan my days in advance. I do whatever I feel like, whenever I feel like. If I think to myself "I want to shove semi-nutritious grey matter into my mouth hole," I don't want to wade through garbage information on the ugly and non-functional dining services webpage just to find out what places are open, and I certainly don't want to figure out what time it is or, God forbid, do math to figure out how much time I have left to get there. 

What I want is a place that tells me what is open now, and for how long they will stay open. And that's it. I don't care about things that are already closed. I refuse to navigate farther than the homepage for something so simple. This is made for me. Use it if you want. 

Structure
---------
After several (incomplete) iterations of whatsopen, in multiple languages, I finally settled on making a RESTful API in Haskell using the Snap library, and writing a frontend in some non-determinate Javascript library and probably some kind of grid-based frontend css thing (is there any other kind?). 

The separate backend means that all of the calculation and datastorage can be separated from the frontend, that way if someone else wants to invest in their future laziness like I did, they can get json based information on whatsopen right now, and probably some other stuff. 

API
---

The API is currently planned to have these calls - 

 * GET /open/{epoch timestamp} - returns json array of locations that are open with some other information, like time it closes, how much time until then, and some other stuff I guess, like the name probably. Maybe the location string. 
 * GET /hours/{location}/{epoch timestamp} - I don't know the structure of this one yet. It will return the whole location object, with the hours. I don't know if this is going to be a day per call or a week, probably one of those two, maybe both. I also don't know how I am going to encode the location. 
 * GET /locations - returns json array of all of the names of the locations. Might be a list of objects with encoded names or an object with id, pretty name, location, and any other relevant information. 
