package predict4s.tle
import spire.algebra._
import spire.math._
import spire.implicits._

case class TLEConstants[F: Fractional: Trig]() {
  
  val ONE_THIRD = Fractional[F].fromRational(Rational(1,3))
  val TWO_THIRD = Fractional[F].fromRational(Rational(2,3))
  val EARTH_RADIUS = Fractional[F].fromDouble(6378.135)  // km
  val MINUTES_PER_DAY = Fractional[F].fromInt(1440)
  val NORMALIZED_EQUATORIAL_RADIUS : F = 1
  val NEQR : F = NORMALIZED_EQUATORIAL_RADIUS
  
  // Potential perturbation coefficients
  val KE     = Fractional[F].fromDouble(0.0743669161331734132) // mu = 3.986008e+14;
  val J3     = Fractional[F].fromDouble(-2.53881e-6)
  val J2     = Fractional[F].fromDouble(1.082616e-3)
  val J4     = Fractional[F].fromDouble(-1.65597e-6)
  val CK2    = J2 * NEQR * NEQR / 2
  val CK4    = -0.375 * J4 * NEQR * NEQR * NEQR * NEQR
  val S      = NEQR * (1 + 78 / EARTH_RADIUS)
  val QOMS2T : F = Fractional[F].fromDouble(1.880279159015270643865e-9)
  val A3OVK2 : F = -J3 / CK2 * NEQR * NEQR * NEQR

  /** Earth gravity coefficient in m<sup>3</sup>/s<sup>2</sup>. */
  val MU = KE * KE * ((EARTH_RADIUS * 1000) pow 3) / (60 * 60)
  
}

object TLEConstants {
  implicit object TLEConstantsD  extends TLEConstants[Double]
  implicit object TLEConstantsR  extends TLEConstants[Real]
  implicit object TLEConstantsBD extends TLEConstants[BigDecimal]
}

