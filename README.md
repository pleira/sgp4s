sgp4s
===============

SGP4 algorithm to predict orbits with TLE written in Scala. The Deep Space algorithm, which contains lunar-solar perturbations, is not implemented.

Most of the code is based in the work done by Vallado, documented in [SpaceTrackReport #3](https://celestrak.com/NORAD/documentation/). To verify the results, the cpp [public code](http://celestrak.com/publications/AIAA/2006-6753/) has been taken. Results match to 1E-9. Also, I have looked into the TLE processing done in the [Orekit](http://www.orekit.org) project.


This implementation is an exercise for having cleaner code written in Scala. That means, the physical models and transformations should appear clearer in the code. 

The numerical library Spire is used. In particular, the user can work with other numerical types that can offer more precision than Double. That can be of interest at the expense of longer running time. 

For the future, it should be easier to introduce alternative implementations for the calculation of the perturbations (like those suggested by [Martin Lara](http://arxiv.org/pdf/1407.8076.pdf)). 


Copyright and License
====================

This software is Copyright by Pablo Pita Leira, 2015. Licensed under Apache v. 2.0.

The software contains parts adapted from an SGP 4 s/w implementation in the public domain from Vallado and by the SGP4 Orekit implementation, specially at the beginning of this project. Software taken from Orekit has copyright by CS Systèmes d'Information, licensed under Apache v. 2.0.


