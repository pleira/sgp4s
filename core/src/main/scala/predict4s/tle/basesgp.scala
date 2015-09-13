package predict4s.tle 

import spire.algebra._
import spire.math._
import spire.implicits._


/**
 * Contains the common bits across the TLE propagation algorithms SGP4 and SGP8
 */
class BaseSGP[F: Field: NRoot : Order : Trig](tle : TLE[F], tlec : TLEConstants[F])   {
  
  
  import tlec._
  import tle._
  
  def epoch = 1000 * year + refepoch

  // Intermediate values used by the propagator models
  val cosi0 = cos(i)       
  def r1 = cosi0
  val theta2  = cosi0 * cosi0
  val e0sq = e * e 
  val xno = meanMotion * 3 * pi / 2160 

  val a1 = (KE / xno) fpow (TWO_THIRD)
  val x3thm1 = 3 * theta2 - 1
  val beta02 = 1 - e0sq
  val beta0 = sqrt(beta02)
  val tval = CK2 * 1.5 * x3thm1 / (beta0 * beta02)
  val delta1 = tval/(a1 * a1)
  
  // original semimajor axis
  val a0 = a1 * (1 - delta1 * (ONE_THIRD + delta1 * (1 + 134 * delta1 / 81)))

  val delta0 = tval/(a0 * a0)
  // the original mean motion
  val xn0dp = xno /(delta0 + 1)   

  val incl = i                   // already in Radians 
  val nodeo = raan               // already Radians
  val omegao = pa                // argper // already Radians 
  val xmo = meanAnomaly          // already in Radians
//  def nddot6 : F
//  def xndt2o = drag * 2 * π / MINUTES_PER_DAY / MINUTES_PER_DAY
  
  
   val a0dp   = a0/(1 - delta0)       
   val perige = (a0dp*(1-e) - NEQR) * EARTH_RADIUS 
    
   // Values of s and qms2t :    
   val (s4, q0ms24) : (F, F) = 
     if (perige < 156) {
	     //  For perigee below 156 km, the values of s and QOMS2T are changed :
	     val s4t : F = if (perige <= 98) 20.as[F] else perige - 78
	     val temp_val = (120 - s4t) * NEQR / EARTH_RADIUS
       // new value for (s, q0ms2T) 
       (s4t / EARTH_RADIUS + NEQR, temp_val pow 4)  
     } else {
       // unmodified
       (S, QOMS2T)  
     }
    
   val pinv   = 1 / (a0dp * beta02)
   val pinvsq = pinv * pinv
   val tsi    = 1 / (a0dp - s4)
   val eta    = a0dp*e*tsi
   val etasq  = eta*eta
   val eeta   = e*eta

   val psisq  = abs(1-etasq) // abs because pow 3.5 needs positive value
   val coef   = q0ms24 * (tsi pow 4)
   val coef1  = coef / (psisq pow 3.5)

    // C2 and C1 coefficients computation :
    val c2    = coef1 * xn0dp * (a0dp * (1 + 1.5*etasq + eeta * (4 + etasq)) + 
                    0.75*CK2 * tsi/psisq * x3thm1 *(8 + 3*etasq * (8 + etasq)))
    val c1    = bStar * c2
    val sini0 = sin(i)    
    // val cosi0 = cos(i)  
//    val a3ovk2 = -XJ3 / CK2
//    val c3 = coef * tsi * a3ovk2 * xn0dp * sini0 / e
   
    val x1mth2 = 1-theta2

    // C4 coefficient computation :
    val c4    = 2 * xn0dp * coef1 * a0dp * beta02 * (eta * (2 + etasq / 2) + e * 
                (0.5 + 2*etasq) - 2 * CK2 * tsi/(a0dp*psisq) * 
                (-3*x3thm1 * (1 - 2*eeta + etasq * (1.5 - eeta / 2)) + 
                   0.75*x1mth2 * (2 * etasq - eeta * (1+etasq)) * cos(2*pa)))

//    val	c5 = 2.0 * coef1 * a0dp * beta02 *
//				(1.0 + 2.75 * (etasq + eeta) + eeta * etasq)
				
    val theta4 = theta2 * theta2
    val temp1  = 3 * CK2 * pinvsq * xn0dp
    val temp2  = temp1 * CK2 * pinvsq
    val temp3  = 1.25 * CK4 * pinvsq * pinvsq * xn0dp

    // atmospheric and gravitation coefs :(Mdf and OMEGAdf)
    val xmdot  = xn0dp + temp1 * beta0 * x3thm1 / 2 + 0.0625*temp2 * beta0 * (13 - 78*theta2 + 137*theta4)
    val x1m5th = 1 - 5*theta2
    val omgdot = -temp1 * x1m5th / 2 + 0.0625*temp2 * (7 - 114*theta2 + 395*theta4) + temp3*(3 - 36*theta2 + 49*theta4)
    val xhdot1 = -temp1 * cosi0
    val xnodot = xhdot1 + (temp2 * (4 - 19*theta2) / 2 + 2*temp3 * (3 - 7*theta2)) * cosi0
    val xnodcf = 3.5 * beta02 * xhdot1 * c1
    val t2cof  = 3 * c1 / 2
    
  // would this method  be better in a typeclass for TLE?
  def isDeepSpacePeriod : Boolean = (0.15625.as[F] / (2*pi)) < (1.as[F] / (xn0dp * MINUTES_PER_DAY))  
  def isDeepSpace : Boolean = isDeepSpacePeriod
    
}



