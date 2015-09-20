package predict4s.tle
import spire.algebra._
import spire.math._
import spire.implicits._

/** 
 *  Initialization for Secular Effects of Atmospheric Drag
 *  
 */  
class AtmosphericDragSecularEffects[F: Field: NRoot : Trig](f : SGP4TimeIndependentFunctions[F]) { 
  
 import f.tlec._
 import f.tle._
 import f._
 /** 
  *  Atmospheric drag modeling is based on a power-law density function given by
 *     ρ = ρ0(q0 - s)^4  / (r - s)^4
 * where r is the radial distance of the satellite from the center of the Earth with q0 and s being
 * altitude parameters of the power law density function.  
 * In all equations that follow, the parameters q0 and s should be in units of Earth radii.
 */
  
  // all quantities on the right hand side of equations are understood to be double prime mean elements.
  // that is, follow the Brower convention, and therefore, the names use n0 and a0 but
  // they refer to the double prime quantities
  val n0 = n0dp
  val a0 = a0dp
  
  // The parameter q0 is a constant equal to 120 km plus one Earth radius 
  val q0 = 1 + 120/aE 


  val s : F = calculateS(f.tle, f.tlec)
  
  val ξ = 1 / (a0 - s)
  val ξsq = ξ*ξ
  val ξto3 = ξsq * ξ
  val ξto4 = ξsq * ξsq
  val ξto5 = ξto4 * ξ
  
  val β0 = (1 - e0sq).sqrt
  val β0sq = 1 - e0sq
  val η = a0 * e0 * ξ
  val ηsq = η * η
  val ηto3 = ηsq*η
  val ηto4 = ηsq*ηsq
  
  // q0 minus s ξ  all to 4 
  val q0ms_ξ__to4 = ((q0 - s)*ξ)**4
  val C2 = q0ms_ξ__to4 * n0 * (1 - ηsq)**3.5 * 
    (a0 * (1 + 3*ηsq/2 + 4*e0*η  + e0*η*ηsq) + 
        3*K2*ξ/(2*(1-ηsq)) * (3*θsq - 1)/2 * (8 + 24*ηsq + 3*ηto4)) 
  
  val C1 = bStar * C2
  
  val sini0 = sin(i0)
  val C3 = q0ms_ξ__to4 * ξ * A30 * n0 * aE * sini0 / (K2 * e0)
  
  // TODO: check using poly
  // val aterm1 = poly"1 - 2η + 3/2η^2 - 1/2η^3"
  val aterm = 3*(1-3*θsq)*(1 + 3*ηsq/2 - 2*e0*η - e0*ηto3/2) 
  + 3*(1-3*θsq)*(2*ηsq - e0*η - e0*ηto3)*cos(2*ω0)/4
  val C4 = 2*n0*q0ms_ξ__to4*a0*β0sq*((1 - ηsq) pow -3.5)*
    ((2*η*(1+e0*η) + (e0 + ηto3)/2) - 2*K2*ξ*aterm/(a0*(1-ηsq)))
    
  val C5 = 2*q0ms_ξ__to4*a0*β0sq*((1 - ηsq) pow -3.5)*(1 + 11*η*(η+e0)/4 + e0*ηto3)
  
  
  // TODO: check using poly
  val D2 = 4*a0*C1*C1*ξ
  val D3 = D2*(17*a0+s)*C1*ξ/3
  val D4 = D2*D2*ξ*(221*a0+31*s)/24
  
  //val coef1  = coef / (psisq pow 3.5)
  
  def calculateS[F: Field: NRoot : Trig](tle : InitialTleValues[F], tlec : StandardReferenceConstants[F]) : F = { 
 /* the parameter s is determined based of epoch perigee
 * height above a spherical Earth. If perigee height is greater than or equal 156 km, the value of s is
 * fixed to be 78 km plus one Earth radius. For altitudes greater than or equal to 98 km but less
 * than 156 km, s is defined to be perigee height minus 78 km plus one Earth radius. 
 * For altitudes below 98 km, s is 20 km plus one Earth radius.
 */    
 1.as[F]
  }

// case class DCoefs[F](val C1: F) {
//   val D2 = 
// }
}