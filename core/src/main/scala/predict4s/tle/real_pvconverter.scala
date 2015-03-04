package predict4s.tle
import predict4s._
//import predict4s.OrbitPropagator._
import predict4s.tle.TLEConstants._
import spire.math._
import spire.algebra.Order
//import spire.algebra.Order._

import spire.implicits._
// import spire.algebra._
//import spire.optional.unicode.π
import predict4s.KeplerCoord

object RealPVConverter {
  
  def apply(p : KeplerCoord[Real]) = coord(p)
		  	  
  // TODO: away with this method by having degrees and radians
  private def normalizeAngle(a : Real, center: Real) : Real = 
     a - 2*pi * floor((a + pi - center) / (2* pi))

  private def coord(kc: KeplerCoord[Real]): (Vector[Real], Vector[Real])  = {
    import kc._
    // implicit def RealRadians: Radians[Real] = radians(_ * spire.math.Pi / 180, _ * 180 / scala.math.Pi)
    val sini0 = sin(i)
    val cosi0 = cos(i)
 
    val axn = e * cos(ω)
    var temp = Real.one / (a * (Real.one - e * e))
    val xlcof = 0.125 * A3OVK2 * sini0 * (3.0 + 5.0 * cosi0) / (Real.one + cosi0)
    val aycof = 0.25 * A3OVK2 * sini0
    val xll = temp * xlcof * axn
    val aynl = temp * aycof
    val xlt = xl + xll
    val ayn = e * sin(ω) + aynl
    val elsq = axn * axn + ayn * ayn
    val capu = normalizeAngle(xlt - raan, pi)
    var epw = capu
    var ecosE = Real.zero
    var esinE = Real.zero
    var sinEPW = Real.zero
    var cosEPW = Real.zero
    val cosi0Sq = cosi0 * cosi0
    val x3thm1 = 3.0 * cosi0Sq - Real.one
    val x1mth2 = Real.one - cosi0Sq
    val x7thm1 = 7.0 * cosi0Sq - Real.one
    if (e > Real(1 - 1e-6)) {
      throw new Predict4sException("TOO_LARGE_ECCENTRICITY_FOR_PROPAGATION_MODEL")
    }
    val newtonRaphsonEpsilon = Real(1e-12)
    for (j <- 0 until 10) {
      var doSecondOrderNewtonRaphson = true
      sinEPW = sin(epw)
      cosEPW = cos(epw)
      ecosE = axn * cosEPW + ayn * sinEPW
      esinE = axn * sinEPW - ayn * cosEPW
      val f = capu - epw + esinE
      if (abs(f) < newtonRaphsonEpsilon) {
        //break
      }
      val fdot = Real.one - ecosE
      var delta_epw = f / fdot
      if (j == 0) {
        val maxNewtonRaphson = 1.25 * abs(e)
        doSecondOrderNewtonRaphson = false
        if (delta_epw > maxNewtonRaphson) {
          delta_epw = maxNewtonRaphson
        } else if (delta_epw < -maxNewtonRaphson) {
          delta_epw = -maxNewtonRaphson
        } else {
          doSecondOrderNewtonRaphson = true
        }
      }
      if (doSecondOrderNewtonRaphson) {
        delta_epw = f / (fdot + 0.5 * esinE * delta_epw)
      }
      epw += delta_epw
    }
    temp = Real.one - elsq
    val pl = a * temp
    val r = a * (Real.one - ecosE)
    var temp2 = a / r
    val betal = sqrt(temp)
    temp = esinE / (Real.one + betal)
    val cosu = temp2 * (cosEPW - axn + ayn * temp)
    val sinu = temp2 * (sinEPW - ayn - axn * temp)
    val u = atan2(sinu, cosu)
    val sin2u = Real.two * sinu * cosu
    val cos2u = Real.two * cosu * cosu - Real.one
    val temp1 = TLEConstants.CK2 / pl
    temp2 = temp1 / pl
    val rk = r * (Real.one- 1.5 * temp2 * betal * x3thm1) + 0.5 * temp1 * x1mth2 * cos2u
    val uk = u - 0.25 * temp2 * x7thm1 * sin2u
    val xnodek = raan + 1.5 * temp2 * cosi0 * sin2u
    val xinck = i + 1.5 * temp2 * cosi0 * sini0 * cos2u
    val sinuk = sin(uk)
    val cosuk = cos(uk)
    val sinik = sin(xinck)
    val cosik = cos(xinck)
    val sinnok = sin(xnodek)
    val cosnok = cos(xnodek)
    val xmx = -sinnok * cosik
    val xmy = cosnok * cosik
    val ux = xmx * sinuk + cosnok * cosuk
    val uy = xmy * sinuk + sinnok * cosuk
    val uz = sinik * sinuk
    val cr = rk * EARTH_RADIUS
    val pos = Vector(cr * ux, cr * uy, cr * uz)
    val rdot = KE * sqrt(a) * esinE / r
    val rfdot = KE * sqrt(pl) / r
    val xn = KE / (a * sqrt(a))
    val rdotk = rdot - xn * temp1 * x1mth2 * sin2u
    val rfdotk = rfdot + xn * temp1 * (x1mth2 * cos2u + 1.5 * x3thm1)
    val vx = xmx * cosuk - cosnok * sinuk
    val vy = xmy * cosuk - sinnok * sinuk
    val vz = sinik * cosuk
    val cv = EARTH_RADIUS / 60.0
    val vel = Vector(cv * (rdotk * ux + rfdotk * vx), cv * (rdotk * uy + rfdotk * vy), cv * (rdotk * uz + rfdotk * vz))
    (pos, vel)    
  }

}
