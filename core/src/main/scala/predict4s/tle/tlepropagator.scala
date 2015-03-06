package predict4s.tle 

import spire.algebra._
import spire.math._
import spire.implicits._
//import spire.math.Fractional
//import spire.algebra.Order
//import spire.algebra.Trig
//import Predef.{any2stringadd => _, _}

//trait TLEPropagation[R] {
//  def propagate[T <: {def toMinutes: Long}](duration: T) : KeplerCoord[R]
//}

/**
 * Contains the common bits across the TLE propagation
 */
abstract class TLEPropagator[F: Fractional: Order : Trig](tle : TLE[F])  { 
   import tle._
   // FIXME
   import tle.tlec._
   
   val a0dp = a0/(Fractional[F].one - delta0)       
   val perige = (a0dp*(1-e) - NORMALIZED_EQUATORIAL_RADIUS) * EARTH_RADIUS 
    
   // Values of s and qms2t :    
   val (s4, q0ms24) : (F, F)= 
     if(perige < 156) {
	   //  For perigee below 156 km, the values of s and QOMS2T are changed :
	   val s4t = if (perige <= 98) Fractional[F].fromInt(20) else perige - Fractional[F].fromInt(78)
	   val temp_val = (120.0 - s4t) * NORMALIZED_EQUATORIAL_RADIUS / EARTH_RADIUS
	   val temp_val_squared = temp_val * temp_val
       (s4t / EARTH_RADIUS + NORMALIZED_EQUATORIAL_RADIUS,
       temp_val_squared * temp_val_squared)  // new value for q0ms2T and s
     } else {
        (S,  // unmodified value for s
         QOMS2T) // unmodified value for q0ms2T 
     }
    
   val pinv =  1 / (a0dp * beta02)
   val pinvsq = pinv * pinv
   val tsi = 1.0 / (a0dp - s4)
   val eta = a0dp*e*tsi
   val etasq = eta*eta
   val eeta = e*eta

   val psisq = abs(1-etasq) // abs because pow 3.5 needs positive value
   val coef = q0ms24 * (tsi * tsi * tsi * tsi)
   val coef1 = coef / (psisq pow 3.5)

    // C2 and C1 coefficients computation :
    val c2 = coef1 * xn0dp * (a0dp * (1 + 1.5*etasq + eeta * (4 + etasq)) + 
        0.75*CK2 * tsi/psisq * x3thm1 *(8 + 3*etasq * (8 + etasq)))
    val c1 = bStar * c2
    val sini0 = sin(i)    
    val cosi0 = cos(i)  
//    val a3ovk2 = -XJ3 / CK2
//    val c3 = coef * tsi * a3ovk2 * xn0dp * sini0 / e
   
    val x1mth2 = 1-theta2

    // C4 coefficient computation :
    val c4 = 2 * xn0dp * coef1 * a0dp * beta02 * (eta * (2 + 0.5*etasq) + e * 
        (0.5 + 2*etasq) - 2 * CK2 * tsi/(a0dp*psisq) * (-3*x3thm1 * 
            (1 - 2*eeta + etasq * (1.5 - 0.5*eeta)) + 0.75*x1mth2 * 
              (2 * etasq - eeta * (1+etasq)) * cos(2*pa)))

//    val	c5 = 2.0 * coef1 * a0dp * beta02 *
//				(1.0 + 2.75 * (etasq + eeta) + eeta * etasq)
				
    val theta4 = theta2 * theta2
    val temp1 = 3 * CK2 * pinvsq * xn0dp
    val temp2 = temp1 * CK2 * pinvsq
    val temp3 = 1.25 * CK4 * pinvsq * pinvsq * xn0dp

    // atmospheric and gravitation coefs :(Mdf and OMEGAdf)
    val xmdot = xn0dp + 0.5*temp1 * beta0 * x3thm1 + 0.0625*temp2 * beta0 * 
                 (13 - 78*theta2 + 137*theta4)
    val x1m5th = 1 - 5*theta2
    val omgdot = -0.5*temp1 * x1m5th + 0.0625*temp2 * (7 - 114*theta2 + 395*theta4) + 
                temp3*(3 - 36*theta2 + 49*theta4)
    val xhdot1 = -temp1 * cosi0
    val xnodot = xhdot1 + (0.5*temp2 * (4 - 19*theta2) + 2*temp3 * (3 - 7*theta2)) * cosi0
    val xnodcf = 3.5 * beta02 * xhdot1 * c1
    val t2cof = 1.5 * c1
}



