sgp4s
===============

SGP4 algorithm to predict orbits with TLE written in Scala. The Deep Space algorithm, which contains lunar-solar perturbations, is not implemented.

There are many SGP4 implementations out there. This one just is an exercise of having clean code. That means, the modelled physical processes should be much clearer in the code.

A good part of the code is based in the work done by the [Orekit](http://www.orekit.org) project.

The numerical library Spire is used. In particular, the user can work with other numerical types that can offer more precision than Double.  

License
=======

Apache v. 2.0.

