package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import predict4s.tle.TEME.SGPElems

/**
 *  Kepler's equation
 *  https://en.wikipedia.org/wiki/Kepler%27s_equation
 *  See the section Numerical approximation of inverse problem
 *  Orekit seems to solve this a little bit differently to Vallado
 */
trait NewtonRaphsonKeplerSolver {
  
    def solveEccentricAnomaly[F: Field: NRoot : Order: Trig](nodep: F, axnl: F, aynl: F, xl : F) : (F, F, F)  = {

         /* --------------------- solve kepler's equation  M = E - e sin E     --------------- */
     // Nodep (or M) is the mean anomaly, E is the eccentric anomaly, and e is the eccentricity.

     var ktr : Int = 1
     val u    = Field[F].mod(xl - nodep, (2*pi).as[F])
     var eo1  = u
     var tem5 : F = 9999.9.as[F]     //   sgp4fix for kepler iteration
     var ecosE : F = 0.as[F]
     var esinE : F = 0.as[F]
     
     //   the following iteration needs better limits on corrections
     while (( abs(tem5) >= 1e-12.as[F]) && (ktr <= 10) )
       {
         val sineo1 = sin(eo1)
         val coseo1 = cos(eo1)
         ecosE = axnl * coseo1 + aynl * sineo1
         esinE = axnl * sineo1 - aynl * coseo1

         val fdot   = 1 - ecosE
         val f = (u + esinE - eo1)
         tem5   = f / fdot  // delta value
         if(abs(tem5) >= 0.95.as[F])
             tem5 = if (tem5 > 0.as[F]) 0.95.as[F]  else -0.95.as[F] 
         eo1    = eo1 + tem5
         ktr = ktr + 1
       }

     (eo1,ecosE,esinE)
  }

}

object NewtonRaphsonKeplerSolver extends NewtonRaphsonKeplerSolver