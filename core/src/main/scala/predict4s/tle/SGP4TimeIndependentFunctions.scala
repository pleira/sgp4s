package predict4s.tle


import spire.algebra._
import spire.math._
import spire.implicits._


/**
 * Contains the TimeIndependentFunctions from SGP4
 * all equations below are taken from SPACETRACK Report #3 by Hoots and Roehrich
 *  
 * (EXAMPLE http://aero.tamu.edu/sites/default/files/faculty/alfriend/S6.1%20Hoots.pdf)
 */
class SGP4TimeIndependentFunctions[F: Field: NRoot : Trig](val tle : InitialTleValues[F], val tlec : StandardReferenceConstants[F])   {
  
  
  import tlec._
  import tle._

  val ONE_THIRD = (1.0/3.0).as[F]
  val TWO_THIRD = (2.0/3.0).as[F]
  val NEQR = 1.as[F]
  val CK4 = J4
//CK4 = .62098875E-6 //  J4
  val EARTH_RADIUS = aE
  val S = 1.01222928.as[F]
  val QOMS2T = 1.88027916E-9.as[F]
  val MINUTES_PER_DAY = 1440.as[F]
  
  // Intermediate values used by the propagator models
  // recovery of the Brouwer mean motion from the Kozai mean motion by the equations
  val θ = cos(i0)
  def cosi0 = θ
  
//  def r1 = cosi0
  val θsq = θ * θ
  def cosi0sq  = θsq
  def theta2 = cosi0sq
  val e0sq = e0 * e0 
  
  // rad per min
  //val xno = meanMotion * 3 * pi / 21600
  
  // Hoots a1
  val a1 = (KE / n0) fpow (TWO_THIRD)
  // 3 theta2 minus 1
  val x3thm1 = 3 * theta2 - 1
  val beta02 = 1 - e0sq
  val beta0 = sqrt(beta02)
  val tval = 3 * K2 * x3thm1 / (beta0 * beta02) / 2
  
  // Hoots δ1
  val δ1 = tval/(a1 * a1)  
  def delta1 = δ1 
  
  // Hoots a2
  val a2 = a1 * (1 - δ1 * (ONE_THIRD + δ1 * (1 + 134 * δ1 / 81)))
  def a0 = a2
  
  // Hoots δ0
  val δ0 = tval/(a2 * a2)
  def delta0 = δ0
  
  // Hoots original mean motion n0'' , double prime
  val n0dp = n0 /(1 + δ0) 
  def xn0dp = n0dp
 
  // Hoots original a0'' , semimajor axis a0 double prime
  val a0dp = (KE / n0dp) fpow (TWO_THIRD)       
 
  def originalMeanMotionAndSemimajorAxis() = (n0dp, a0dp)
  def perige = (a0dp *(1-e0) - 1) * aE 

  // From this point on, the mean motion n0" and the semimajor axis a0"
  // follow the Brouwer convention.
    
}
