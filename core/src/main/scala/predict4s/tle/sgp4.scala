package predict4s.tle

import predict4s._
import scala.concurrent.duration._
//import scala.math._
import spire.math._
import spire.implicits._
// import spire.algebra._
//import spire.optional.unicode.π
import scala.{ specialized => spec }

class SGP4(tle: TLE) extends TLEPropagator(tle) {

  import TLEConstants._
  import tle._

  def sgp4Simple : Boolean = perige < 220

  // For perigee less than 220 kilometers, the equations are truncated to
  // linear variation in sqrt a and quadratic variation in mean anomaly.
  // Also, the c3 term, the delta omega term, and the delta m term are dropped.
  val (d2, d3, d4, t3cof, t4cof, t5cof, omgcof, xmcof, sinM0, delM0) = 
    if (sgp4Simple) (0.0,0.0,0.0,0.0,0.0,0.0, 0.0, 0.0,0.0,0.0)
    else {
      val c1sq = c1 * c1
      val _delM0_ = 1.0 + eta * cos(meanAnomaly)
      val _delM0 = _delM0_ * _delM0_ * _delM0_
      val _d2 = 4 * a0dp * tsi * c1sq
      val temp = _d2 * tsi * c1 / 3.0
      val _d3 = (17 * a0dp + s4) * temp
      val _d4 = 0.5 * temp * a0dp * tsi * (221 * a0dp + 31 * s4) * c1
      val _t3cof = _d2 + 2 * c1sq
      val _t4cof = 0.25 * (3 * _d3 + c1 * (12 * _d2 + 10 * c1sq))
      val _t5cof = 0.2 * (3 * _d4 + 12 * c1 * _d3 + 6 * _d2 * _d2 + 15 * c1sq * (2 * _d2 + c1sq))
      val _sinM0 = sin(meanAnomaly)
      val (_xmcof, _omgcof) = 
        if (e < 1e-4) (0.0, 0.0)
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
        
  val temp = Array[Double](9)
 

  override def propagate[T <: { def toMinutes: Long}](duration: T) = {
    import scala.language.reflectiveCalls
    val tSince = duration.toMinutes
    // Update for secular gravity and atmospheric drag.
    val xmdf = meanAnomaly + xmdot * tSince
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
        val delm = xmcof * ((pow(delmt , 3)) - delM0)
        val _temp = delomg + delm
        val _xmp = xmdf + _temp
        val tcube = tsq * tSince
        val tfour = tSince * tcube
        (omgadf - _temp,
         _xmp,
         ctempa - d2 * tsq - d3 * tcube - d4 * tfour,
         ctempe + bStar * c5 * (sin(_xmp) - sinM0), 
         ctempl + t3cof * tcube + tfour * (t4cof + tSince * t5cof))
      } else 
        (omgadf, xmdf, ctempa, ctempe, ctempl) 

    val a = a0dp * tempa * tempa
    val e_new = e - tempe
    val beta = math.sqrt(1.0 - e_new * e_new)
    val xn = KE / pow(a, 1.5)
    val xl = xmp + omega + xnode + xn0dp * templ

    KeplerCoord(xnode, a, e_new, i, omega, xl)
  }

}
