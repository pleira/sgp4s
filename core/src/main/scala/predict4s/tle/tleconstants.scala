package predict4s.tle
import spire.algebra._
import spire.math._
import spire.implicits._

class TLEConstants[F: Fractional: Trig]() {
  
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
  val CK4    = -0.375 * J4 * (NEQR pow 4)
  val S      = NEQR * (1 + 78 / EARTH_RADIUS)
  val QOMS2T : F = Fractional[F].fromDouble(1.880279159015270643865e-9)
  val A3OVK2 : F = -J3 / CK2 * (NEQR pow 3)

  /** Earth gravity coefficient in m<sup>3</sup>/s<sup>2</sup>. */
  val MU = KE * KE * ((EARTH_RADIUS * 1000) pow 3) / (60 * 60)
  
}

// Variable name Definition Value as given in Hoots document
//CK2 = 5.413080E-4 // 1/2 J2aE
//CK4 = .62098875E-6 //  J4
//E6A = 10E-6  // 6 1.0 E-6
//QOMS2T = 1.88027916E-9
//S = 1.01222928
//TOTHRD =  .66666667 // 2/3
//XJ3 = - 0.253881E-5 // J3
//XKE = 0.743669161E-1
//XKMPER = 6378.135
//XMNPDA = 1440.0
//AE = 1.0 // distance units/Earth radii
//DE2RA = 0.174532925E-1 // radians/degree
//PI = 3.14159265 // π
//PIO2 = 1.57079633 // π/2 
//TWOPI =  6.2831853 // 2π
//X3PIO2 = 4.71238898  // 3π/2 

//object TLEConstants {
//  val TLEConstantsD = new TLEConstants[Double]
//  val TLEConstantsR = new TLEConstants[Real]
//  val TLEConstantsBD = new TLEConstants[BigDecimal]
//  
//  import reflect.runtime.universe._
//  def getTLEConstants[T](implicit tag: TypeTag[T]) : TLEConstants[T] = 
//    typeOf[T] match {
//      case t if (t <:< typeOf[Double])      => TLEConstantsD 
//      case t if (t <:< typeOf[Real])        => TLEConstantsR
//      case t if (t <:< typeOf[BigDecimal])  => TLEConstantsBD
//      // case t if (t <:< typeOf[Fractional[_]])  => TLEConstants[t]()
//      case _ => throw new IllegalArgumentException()
//  }
//}

