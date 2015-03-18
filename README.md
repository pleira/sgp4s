sgp4s
===============

SGP4 algorithm to predict orbits with TLE written in Scala.

There are many SGP4 implementations out there. This one just is an exercise of having clean code. That means, the modelled physical processes should be much clearer in the code.

The numerical library Spire is used. In particular, the user can work with other numerical types that can offer more precision than Double.  

todo: introduce units and scala-geo to have more type safety.

