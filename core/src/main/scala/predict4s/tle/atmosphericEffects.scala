package predict4s.tle
import spire.algebra._
import spire.math._
import spire.implicits._

/** 
 *  Initialization for Secular Effects of Atmospheric Drag
 *  
 */  
trait AtmosphericDragSecularEffects { 
 
 /** 
 *  Atmospheric drag modeling is based on a power-law density function given by
 *     ρ = ρ0(q0 - s)^4  / (r - s)^4
 * where r is the radial distance of the satellite from the center of the Earth with q0 and s being
 * altitude parameters of the power law density function.  
 * In all equations that follow, the parameters q0 and s should be in units of Earth radii.
 */
    
  /** t is the duration in minutes from the epoch , then the SGP4 Time Independent Functions */
  def propagate[F: Field: NRoot : Order: Trig](t: F)(ti: SGP4TimeIndependentFunctions[F])
         : (SGP4TimeIndependentFunctions[F], TEME.OrbitalElements[F]) = {
    import ti.ini._
    import ti._
    // FIXME objective is to do this: oe0 + change(t)(ti)
    val ωdf = ω0 + ωdot*t
    val Ωdf = Ω0 + Ωdot*t
    val Mdf = M0 + Ṁ*t
    // a0 is a0dp, then e0, i0 are unchanged, and the other are perturbed
    (ti, TEME.OrbitalElements(a0, e0, i0, ωdf, Ωdf, Mdf))  
  }
   
  /** t is the duration in minutes from the epoch, then the SGP4 Time Independent Functions */
  def perturb[F: Field: NRoot : Order : Trig](t: F)(ti: SGP4TimeIndependentFunctions[F]): TEME.OrbitalElements[F] = {
    import ti._
    // fixme : scalac does not handle initialising val (ωdot, Ωdot, Ṁ)   
    TEME.OrbitalElements(0.as[F], 0.as[F], 0.as[F], ωdot*t, Ωdot*t, mdot*t)   
  }
  
}