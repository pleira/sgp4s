package predict4s.tle

import predict4s.KeplerCoord
//import spire.math.Fractional
//import spire.algebra.Order
import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }

class SGP4[F : Fractional: Trig](tle: TLE[F]) extends TLEPropagator[F](tle) {

  import tle._
  // FIXME
  import tle.tlec._
   
  def sgp4Simple : Boolean = perige < Fractional[F].fromInt(220)

  // For perigee less than 220 kilometers, the equations are truncated to
  // linear variation in sqrt a and quadratic variation in mean anomaly.
  // Also, the c3 term, the delta omega term, and the delta m term are dropped.
  val (d2, d3, d4, t3cof, t4cof, t5cof, omgcof, xmcof, sinM0, delM0) = 
    if (sgp4Simple) (Fractional[F].zero,Fractional[F].zero,Fractional[F].zero,Fractional[F].zero,Fractional[F].zero,Fractional[F].zero, Fractional[F].zero, Fractional[F].zero,Fractional[F].zero,Fractional[F].zero)
    else {
      val c1sq : F = c1 * c1
      val _delM0_ : F= 1.0 + eta * cos(meanAnomaly)
      val _delM0 : F= _delM0_ * _delM0_ * _delM0_
      val _d2 : F = 4 * a0dp * tsi * c1sq
      val temp : F = _d2 * tsi * c1 / 3.0
      val _d3 : F = (17 * a0dp + s4) * temp
      val _d4 : F = 0.5 * temp * a0dp * tsi * (221 * a0dp + 31 * s4) * c1
      val _t3cof : F = _d2 + 2 * c1sq
      val _t4cof : F = 0.25 * (3 * _d3 + c1 * (12 * _d2 + 10 * c1sq))
      val _t5cof : F = 0.2 * (3 * _d4 + 12 * c1 * _d3 + 6 * _d2 * _d2 + 15 * c1sq * (2 * _d2 + c1sq))
      val _sinM0 : F = sin(meanAnomaly)
      val (_xmcof, _omgcof) = 
        if (e < Fractional[F].fromDouble(1e-4)) (Fractional[F].zero, Fractional[F].zero)
        else  {
          val c3 = coef * tsi * A3OVK2 * xn0dp *
                 NORMALIZED_EQUATORIAL_RADIUS * sini0 / e
          (- TWO_THIRD * coef * tle.bStar *
                NORMALIZED_EQUATORIAL_RADIUS / eeta, 
           tle.bStar * c3 * cos(pa))
        }
      (_d2, _d3, _d4, _t3cof, _t4cof, _t5cof, _omgcof, _xmcof, _sinM0, _delM0)
    } 
         
  val c5 = 2 * coef1 * a0dp * beta02 * (1 + 2.75 * (etasq + eeta) + eeta * etasq)
  // initialized
        
  //val temp = Array[Double](9)
 

  /*override*/ def propagate[T <: { def toMinutes: Long}](duration: T) : KeplerCoord[F] = {
    import scala.language.reflectiveCalls
    val tSince = duration.toMinutes
    // Update for secular gravity and atmospheric drag.
    val xmdf0 =  xmdot * tSince
    val xmdf = meanAnomaly + xmdf0
    val omgadf = pa + omgdot * tSince
    val xn0ddf = raan + xnodot * tSince
    val tsq = tSince * tSince
    val xnode = xn0ddf + xnodcf * tsq
           
    val ctempa = 1 - c1 * tSince
    val ctempe = bStar * c4 * tSince
    val ctempl = t2cof * tsq
    val (omega, xmp, tempa, tempe, templ) = 
      if (!sgp4Simple) { 
        val delomg = omgcof * tSince
        val delmt = 1 + eta * cos(xmdf)
        val delm = xmcof * ((delmt pow 3) - delM0)
        val _temp = delomg + delm
        val _xmp = xmdf + _temp
        val tcube =  tsq * tSince
        val tfour = tSince * tcube
        (omgadf - _temp,
         _xmp,
         ctempa - d2 * tsq - d3 * tcube - d4 * tfour,
         ctempe + bStar * c5 * (sin(_xmp) - sinM0), 
         ctempl + t3cof *tcube + tfour * (t4cof + tSince * t5cof))
      } else 
        (omgadf, xmdf, ctempa, ctempe, ctempl) 

    val a = a0dp * tempa * tempa
    val e_new = e - tempe
    val beta = sqrt(1 - e_new * e_new)
    val xn = KE / (a pow 1.5)
    val xl = xmp + omega + xnode + xn0dp * templ

    KeplerCoord[F](a, e_new, i, omega, xnode, xl)
  }

}
