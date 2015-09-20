package predict4s.tle
import spire.algebra._
import spire.math._
import spire.implicits._

trait StandardReferenceConstants[F] {

  /** sqrt(Grativational Constant * Earths's Mass) */
  def MU: F
  
  /** Equatorial radius of the Earth in km */ 
  def aE: F
  def EARTH_RADIUS = aE
  
  /**  sqrt (Grativational Constant * Earths's Mass)  in units (Earths Radii)** 1.5 / minute */
  def KE: F
  
  // spherical harmonics 
  
  /** J2 spherical harmonic value */
  def J2: F
  /** J3 spherical harmonic value */
  def J3: F
  /** J3 spherical harmonic value */
  def J4: F  

  def CK2: F  // = 5.413080E-4.as[F] // 1/2 J2aE
  def K2: F = CK2
  
  def A3OVK2 : F // [F: Field: Signed] : F = - J3 / CK2

  // def K2: F   // units of (Earth radii) ]
}

class WGS72OldConstants[F: Field]() extends StandardReferenceConstants[F] {
  val MU     =   398600.8.as[F]
  val aE     =   6378.135.as[F]
  val KE     =   0.0743669161.as[F] 
  val J2     =   0.001082616.as[F]
  val J3     =  -0.00000253881.as[F]
  val J4     =  -0.00000165597.as[F]
  
  val CK2: F = 5.413080E-4.as[F] // 1/2 J2aE
  val A3OVK2 : F = - J3 / CK2
}

class WGS72Constants[F: Field: NRoot]() extends StandardReferenceConstants[F] {
  val MU     =   398600.79964.as[F] 
  val aE     =   6378.135.as[F]
  val KE     =   60 / (aE**3 / MU).sqrt
  val J2     =   0.001082616.as[F]
  val J3     =  -0.00000253881.as[F]
  val J4     =  -0.00000165597.as[F]
  
  val CK2: F = 5.413080E-4.as[F] // 1/2 J2aE
  val A3OVK2 : F = - J3 / CK2
  
}

class WGS84Constants[F: Field: NRoot]() extends StandardReferenceConstants[F] {
  val MU     =   398600.5.as[F]            
  val aE     =   6378.137.as[F]
  val KE     =   60 / (aE**3 / MU).sqrt
  val J2     =   0.00108262998905.as[F]
  val J3     =  -0.00000253215306.as[F]
  val J4     =  -0.00000161098761.as[F]
  
  val CK2: F = 5.413080E-4.as[F] // 1/2 J2aE  
  val A3OVK2 : F = - J3 / CK2
}

object WGS72Constants {
  
  implicit val tleDoubleConstants = new WGS72Constants[Double]()  
  implicit val tleRealConstants = new WGS72Constants[Real]()
  
}

object WGS84Constants {
  
  implicit val tleDoubleConstants = new WGS84Constants[Double]()  
  implicit val tleRealConstants = new WGS84Constants[Real]()
  
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
//      // case t if (t <:< typeOf[Field[_]])  => TLEConstants[t]()
//      case _ => throw new IllegalArgumentException()
//  }
//}


/*
          // -- wgs-72 low precision str#3 constants --
           case wgs72old:
           mu     = 398600.79964;        // in km3 / s2
           radiusearthkm = 6378.135;     // km
           xke    = 0.0743669161;
           tumin  = 1.0 / xke;
           j2     =   0.001082616;
           j3     =  -0.00000253881;
           j4     =  -0.00000165597;
           j3oj2  =  j3 / j2;
         break;
           // ------------ wgs-72 constants ------------
           case wgs72:
           mu     = 398600.8;            // in km3 / s2
           radiusearthkm = 6378.135;     // km
           xke    = 60.0 / sqrt(radiusearthkm*radiusearthkm*radiusearthkm/mu);
           tumin  = 1.0 / xke;
           j2     =   0.001082616;
           j3     =  -0.00000253881;
           j4     =  -0.00000165597;
           j3oj2  =  j3 / j2;
          case wgs84:
           // ------------ wgs-84 constants ------------
           mu     = 398600.5;            // in km3 / s2
           radiusearthkm = 6378.137;     // km
           xke    = 60.0 / sqrt(radiusearthkm*radiusearthkm*radiusearthkm/mu);
           tumin  = 1.0 / xke;
           j2     =   0.00108262998905;
           j3     =  -0.00000253215306;
           j4     =  -0.00000161098761;
           j3oj2  =  j3 / j2;
*/
