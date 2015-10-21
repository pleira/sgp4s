package predict4s.tle

//import predict4s.KeplerCoord
import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }


/** The SGP-4 theory is applied for all orbits with periods of T <= 225 min. It
 * performs a propagation in time of doubly averaged elements according to their
 * secular rates of change due to the zonal harmonics J2 and J4 of the Earth potential,
 * and due to drag perturbations in an atmosphere with a power-law altitude profile of air density. 
 * The propagated, doubly averaged elements at epoch are subsequently
 * converted into singly averaged elements, by overlaying long-periodic
 * perturbations due to J3, before a final conversion step to osculating elements by superimposition
 * of first-order, short-period perturbation amplitudes due to J2. 
 * (from Space Debris, by H. Klinkrad, pag 216).
 */
case class SGP4[F : Field : NRoot : Order : Trig](val state0: SGP4State[F]) {
     
    def propagate(t: F)(implicit wgs : WGSConstants[F]) : SGP4State[F] = {
      val (_, el, am) = SecularEffects.propagate(t)(state0.tif)
      val (nodep, axnl, aynl, xl) = SGP4LongPeriodicEffects.calculateSGP4LongPeriodicEffects(state0.tif, el, am)
      val (eo1,ecosE,esinE) = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(nodep, axnl, aynl, xl)
      val nm    = el.n0
      val xincp = el.i0
      val cosip = cos(xincp)
      val sinip = sin(xincp)
      // here, should be something returned before in other coordinates 
      val posVel = ShortPeriodPeriodicPerturbations.calcPositionVelocity(state0.tif, nm, xincp, cosip, sinip, am, nodep, axnl, aynl, xl, eo1)
      SGP4State(t, el, state0.tif, posVel)
    }
}

case class SGP4State[F : Field : NRoot : Order : Trig](t: F, elem: TEME.SGPElems[F], tif : SGP4TimeIndependentFunctions[F], posVel: TEME.CartesianElems[F])

object SGP4 {

  // TODO See if Delaunays variables can be introduced 
  // this should use the state monad
  def apply[F : Field : NRoot : Order : Trig](tle: TLE)(implicit wgs : WGSConstants[F]) : SGP4[F] = {
    
    val elem = TEME.sgpElems[F](tle)
    val tif  = SGP4TimeIndependentFunctions(elem)
      // Propagate for time 0 minutes to get all initialized. 
    val t = 0.as[F] 
    val (_, el, am) = SecularEffects.propagate(t)(tif)

    val (nodep, axnl, aynl, xl) = SGP4LongPeriodicEffects.calculateSGP4LongPeriodicEffects(tif, el, am)
    
    val (eo1,ecosE,esinE) = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(nodep, axnl, aynl, xl)
    val nm    = el.n0
    val xincp = el.i0
    val cosip = cos(xincp)
    val sinip = sin(xincp)
    // here, should be something returned before in other coordinates 
    val posVel = ShortPeriodPeriodicPerturbations.calcPositionVelocity(tif, nm, xincp, cosip, sinip, am, nodep, axnl, aynl, xl, eo1)
    val state0 = SGP4State(t, elem, tif, posVel)
    SGP4(state0)
  }
}

