sgp4s
===============

SGP4 algorithm to predict orbits with TLE written in Scala. The Deep Space algorithm, which contains lunar-solar perturbations, is not implemented.

Most of the code is based in the work done by Vallado, documented in [SpaceTrackReport #3](https://celestrak.com/NORAD/documentation/). To verify the results, the cpp [public code](http://celestrak.com/publications/AIAA/2006-6753/) has been taken. Also, I have looked into the TLE processing done in the [Orekit](http://www.orekit.org) project.

This implementation is an exercise of having cleaner code written in Scala. That means, the physical models and transformations should appear clearer in the code. 

The numerical library Spire is used. In particular, the user can work with other numerical types that can offer more precision than Double. That can be of interest near conflictive points in the theory (low inclinations, and 63 deg) at the expense of longer running time. 

For the future, it should be easier to introduce alternative implementations for the calculation of the perturbations (like those suggested by [Martin Lara](http://arxiv.org/pdf/1407.8076.pdf)). 


License
=======

Apache v. 2.0.

