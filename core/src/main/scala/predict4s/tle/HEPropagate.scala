package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._

trait EarthAtmosphereDragEffects extends EarthHarmonicsAndDragSecularEffects {
  
  
  def dragEffects[F: Field: NRoot : Order : Trig](ti : SGP4TimeIndependentFunctions[F])(Mdf: F)(t: F) = {
   import ti.wgs.{aE,K2}
   import ti.ini._
   import ti._
    val (δω, δM) =  
   // FIXME AND NOTE THAT IT IS a0dp although appears a0, and the same for n0
//   if (perigee > 220 && !isDeepSpace)
     if (perige > 220) {
       (bStar*C3*cos(ω0)*t, 
           -2*q0ms_ξ__to4 * bStar * aE * ((1 + η*cos(Mdf))**3 - (1 + η*cos(M0))**3) / (e0*η))  
     } else {
       (0.as[F], 0.as[F])
     }
   val δΩ = - 21*n0*K2*θ*C1*t*t / (2*a0sq*β0sq)
  (δω, δΩ, δM)

  }
  
  /** t is the duration in minutes from the epoch , then the perturbation, then the constants */
 override def propagate[F: Field: NRoot : Order : Trig](t: F)(ti: SGP4TimeIndependentFunctions[F])
       : (SGP4TimeIndependentFunctions[F], TEME.OrbitalElements[F]) = {
   import ti.wgs.{aE,K2}
   import ti.{perige,a0}
   import ti.ini._
   
   val (_, hoe) = super.propagate(t)(ti)
   // I am using Kepler elements to hold the updated mean/true anomaly of the harmonic orbital effects
   val Mdf = hoe.trueAnomaly
   val ωdf = hoe.ω
   val Ωdf = hoe.Ω
   val (δω, δΩ, δM) = dragEffects(ti)(Mdf)(t)
   val ω = ωdf - δω - δM
   val Ω = Ωdf + δΩ
   val M = Mdf + δω + δM
   
   (ti, TEME.OrbitalElements(a0, e0, i0, ω, Ω, M))
 }

}