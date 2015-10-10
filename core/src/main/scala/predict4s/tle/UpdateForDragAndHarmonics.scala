package predict4s.tle
import spire.algebra._
import spire.math._
import spire.implicits._
/**
 * Propagation: the Predictions of satellite motion are performed using the constants computed in the initialization.
 * 
 * Secular Update for Earth Zonal Gravity and Partial Atmospheric Drag Effects.
 * The angles M , ω and Ω are first updated to include the effects of the Earth zonal harmonics and atmospheric drag effects.
 * 
 */
//trait DragAndHarmonicsPropagator {
//  
//// import f.f.f.tlec._
//// import f.f.f.tle._
//// import f._
//// import f.f._
//// import f.f.f._
//// //import f.f.f.f._
//// import f.f.f.perigee 
//// import f.f.C3
//// 
// // [F: Field: NRoot : Order : Trig](val f : EarthHarmonicsSecularEffects[F]) 
// /** t is the duration in minutes from the epoch */
// def propagate[F: Field: NRoot : Order : Trig](hsef : EarthHarmonicsSecularEffects)(t: F) : TEME.OrbitalElements[F] = {
//   import hsef.a.f.tle._
//   import self.a.f.tlec._
//   val Mdf = M0 + n0*t + Ṁ*t
//   val ωdf = ω0 + ωdot*t
//   val Ωdf = Ω0 + Ωdot*t
//   // Fixme
////   if (perigee > 220 && !isDeepSpace)
//   val (δω, δM) =  
//   if (perigee > 220) {
//     (bStar*C3*cos(ω0)*t, 
//         -2*q0ms_ξ__to4 * bStar * aE * ((1 + η*cos(Mdf))**3 - (1 + η*cos(M0))**3) / (e0*η))  
//   } else {
//     (0.as[F], 0.as[F])
//   }
//   //KC
// }
//
//// where ( t  t o ) is time since epoch in minutes. It should be noted that when epoch perigee height
//// is less than 220 kilometers or for “deep space” satellites, the terms  , and   are dropped.
//}