package predict4s.tle
import spire.algebra.Trig
import spire.math._
import spire.implicits._
import predict4s.KeplerCoord

class PVConverter[F: Fractional: Trig]() {
  //  FIXME use implicits
  val tlec : TLEConstants[F] = new TLEConstants[F]()
  import tlec._
  
  // TODO: away with this method by having degrees and radians
  private def normalizeAngle(a : F, center: F) : F = 
     a - 2*pi * floor((a + pi - center) / (2* pi))

  def coord(kc: KeplerCoord[F]): (Vector[F], Vector[F])  = {
    import kc._
    val sini0 = sin(i)
    val cosi0 = cos(i)
    val axn = e * cos(ω)
    val temp : F = 1 / (a * (1 - e * e))
    val xlcof = 0.125 * A3OVK2 * sini0 * (3 + 5 * cosi0) / (1 + cosi0)
    val aycof = A3OVK2 * sini0 / 4
    val xll = temp * xlcof * axn
    val aynl = temp * aycof
    val xlt = xl + xll
    val ayn = e * sin(ω) + aynl
    val elsq = axn * axn + ayn * ayn
    val capu = normalizeAngle(xlt - raan, Fractional[F].fromReal(pi))
    var epw = capu
    var ecosE : F = 0
    var esinE : F = 0
    var sinEPW : F = 0
    var cosEPW : F = 0
    val cosi0Sq = cosi0 * cosi0
    val x3thm1 = 3 * cosi0Sq - 1
    val x1mth2 = 1 - cosi0Sq
    val x7thm1 = 7 * cosi0Sq - 1
    val limit : F = Fractional[F].fromDouble(1 - 1e-6)
    if (e > limit) {
      throw new predict4s.Predict4sException("TOO_LARGE_ECCENTRICITY_FOR_PROPAGATION_MODEL")
    }
    val newtonRaphsonEpsilon = Fractional[F].fromDouble(1e-12)
    var idx = 0
    while (idx < 10) {
      var doSecondOrderNewtonRaphson = true
      sinEPW = sin(epw)
      cosEPW = cos(epw)
      ecosE = axn * cosEPW + ayn * sinEPW
      esinE = axn * sinEPW - ayn * cosEPW
      val f = capu - epw + esinE
//      if (abs(f) < newtonRaphsonEpsilon) {
//        //break
//      }
      val fdot = 1 - ecosE
      var delta_epw = f / fdot
      if (idx == 0) {
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
        delta_epw = f / (fdot + esinE * delta_epw / 2)
      }
      epw += delta_epw
      idx += 1
    }
    val pl = a * (1 - elsq)
    val r = a * (1 - ecosE)
    val betal = sqrt(1 - elsq)
    val temp3 = esinE / (1 + betal)
    val cosu =  (cosEPW - axn + ayn * temp3)
    val sinu =  (sinEPW - ayn - axn * temp3)
    val u = atan2(sinu, cosu)
    val aconst = 2 * a * a  * cosu / r / r
    val sin2u = aconst * sinu
    val cos2u = aconst * cosu - 1
    val temp1 = CK2 / pl
    val temp2 = temp1 / pl
    val rk = r * (1 - 3 * temp2 * betal * x3thm1 / 2) + temp1 * x1mth2 * cos2u / 2
    val uk = u - temp2 * x7thm1 * sin2u / 4
    val xnodek = raan + 3 * temp2 * cosi0 * sin2u / 2
    val xinck = i + 3 * temp2 * cosi0 * sini0 * cos2u / 2
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
    val rdot = KE * sqrt(a) * esinE / r
    val rfdot = KE * sqrt(pl) / r
    val xn = KE / (a * sqrt(a))
    val rdotk = rdot - xn * temp1 * x1mth2 * sin2u
    val rfdotk = rfdot + xn * temp1 * (x1mth2 * cos2u + 3 * x3thm1 / 2)
    val pos = (EARTH_RADIUS * rk) *: Vector(ux, uy, uz)
    val v_u = rdotk *: Vector(ux, uy, uz)
    val v_v = rfdotk *: Vector(xmx * cosuk - cosnok * sinuk, xmy * cosuk - sinnok * sinuk,  sinik * cosuk)
    val vel = (EARTH_RADIUS / 60) *: (v_v + v_u) 
    (pos, vel)
  }
  
}

//object PVConverter {
//  def apply[F: Fractional: Trig](p : KeplerCoord[F]) : (Vector[F], Vector[F]) = new PVConverter[F]().coord(p)
//}

