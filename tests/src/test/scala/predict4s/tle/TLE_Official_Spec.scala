package predict4s.tle
import org.scalatest._
import org.scalautils.TolerantNumerics
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import spire.math.Real
import predict4s.ReferenceSystem

class Official_TLE_Spec extends FunSuite
  with BeforeAndAfterAll
  with ShouldMatchers {
    object ddd extends spire.std.DoubleInstances
    import ddd.DoubleAlgebra

  // SGP4 precissions
//  needed for 0 min  
//  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.005)
//  needed for 360 min  
//  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.007)
//  needed for 1080 min  
//  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)
//  needed for 1440 min  
//    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.012)
 
  val expectedSGP4 : Map[Int, TEME.PosVel[Double]] = 
    Map(0 -> TEME.PosVel[Double](Vector(2328.97048951,   -5995.22076416,    1719.97067261),
              Vector(  2.91207230,     -0.98341546,     -7.09081703)),
     360 ->  TEME.PosVel[Double](Vector(2456.10705566,   -6071.93853760,    1222.89727783),
          Vector(  2.67938992,     -0.44829041,     -7.22879231)), 
     720 ->  TEME.PosVel[Double](Vector(2567.56195068,   -6112.50384522,     713.96397400),
         Vector(  2.44024599,      0.09810869,     -7.31995916)), 
     1080 ->  TEME.PosVel[Double](Vector(2663.09078980,   -6115.48229980,     196.39640427),
         Vector(  2.19611958,      0.65241995,     -7.36282432)),
     1440 ->  TEME.PosVel[Double](Vector(2742.55133057,   -6079.67144775,    -326.38095856),
         Vector(  1.94850229,      1.21106251,     -7.35619372)))


  val tle_11 = "1 88888U          80275.98708465  .00073094  13844-3  66816-4 0     9"
  val tle_12 = "2 88888  72.8435 115.9689 0086731  52.6988 110.5714 16.05824518   103"
  val tle_21 = "1 11801U          80230.29629788  .01431103  00000-0  14311-1      13"
  val tle_22 = "2 11801  46.7916 230.4354 7318036  47.4722  10.4117  2.28537848    13"
 
  def check(result: TEME.PosVel[Double], expected: TEME.PosVel[Double])
     (implicit ev: org.scalautils.Equality[Double]) {
//   If can we have tolerance for container comparisons, use this other version
//    result.p should === (expected.p)
//    result.v should === (expected.v)
    (result.p zip expected.p)  foreach {x => assert(x._1 === x._2, "position")}
    (result.v zip expected.v)  foreach {x => assert(x._1 === x._2, "velocity")}
  }
    
  test("Oficial SGP4 prediction") {
    // instead of import spire.implicits._ for doubles I can do
    val tle : TLE = TLE(tle_11, tle_12)
    val vtle = InitialTleValues(tle)
    val prop = new SGP4(vtle, WGS72Constants.tleDoubleConstants)
    assert(!prop.isDeepSpace)
    val pvconv = new PVConverter[Double](WGS72Constants.tleDoubleConstants)
    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.012)
    expectedSGP4.keys foreach {min => 
      val kc = prop.propagate(min)
      val posVel : TEME.PosVel[Double] = pvconv.coord(kc)
      check( posVel, expectedSGP4(min)) 
    }
  }

  //    Note: using reals, the test passes but takes longer
//  test("Oficial SGP4 Real prediction") {
//    val tle : TLE[Real] = TLE(tle_11, tle_12)
//    val prop = new SGP4[Real](tle, TLEConstants.tleRealConstants)
//    val pvconv = new PVConverter[Real]()
//    // assert(!tle.isDeepSpace)
//    // we will do the comparison with doubles
//    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.012)
//    expectedSGP4.keys foreach {min => 
//      val kc = prop.propagate(Duration(min, TimeUnit.MINUTES))
//      val (pos,vel) : (Vector[Real],Vector[Real]) = pvconv.coord(kc)
//      val dpos = pos map (_.toDouble)
//      val dvel = vel map (_.toDouble)
//      check( (dpos, dvel), expectedSGP4(min)) 
//    }
//  }
    
}