package predict4s.tle
import org.scalatest._
import org.scalautils.TolerantNumerics
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

class TLE_PropagatorSpec extends FunSuite
  with BeforeAndAfterAll
  with ShouldMatchers {
   
  // setup a TLE for a GPS satellite
  final val line1 = "1 37753U 11036A   12090.13205652 -.00000006  00000-0  00000+0 0  2272"
  final val line2 = "2 37753  55.0032 176.5796 0004733  13.2285 346.8266  2.00565440  5153"
//
//  final val tle = TLE(line1, line2)
// 
//  // the period of the GPS satellite
//  val period = 717.97 * 60.0
//
//  test("TLE corresponds to deep space") {
//    assert(tle !== null)
//    assert(tle.isDeepSpacePeriod)
//  }
//  
//  final val tle_deep = 
//    TLE("1 26609U 00072B   09105.66069202 -.00000356  00000-0  10000-3 0  2169",
//	    "2 26609 009.1977 023.4368 7962000 194.9139 106.0662 01.25584647 38840") 
//        
//
//  test("TLE_DEEP corresponds to deep space") {
//    assert(tle_deep !== null)
//    assert(tle_deep.isDeepSpacePeriod)
//  }
//    
//  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(1e-10)

  // import spire.implicits._
  // instead of import spire.implicits._ for doubles I can do
  object ddd extends spire.std.DoubleInstances
  import ddd.DoubleAlgebra

  final val tle_leo : TLE[Double] = 
    TLE("1 28375U 04025K   09105.66391970  .00000003  00000-0  13761-4 0  3643",
	    "2 28375 098.0551 118.9086 0084159 315.8041 043.6444 14.40638450251959")
			
//  test("TLE_LEO corresponds to LEO space") {
//    assert(tle_leo !== null)
//    assert(!tle_leo.isDeepSpacePeriod)
//  }
	      
  test("TLE_LEO Slave Mode") {
    val duration   = Duration(2341.4889653027058, TimeUnit.MINUTES)
    val finalState = new SGP4[Double](tle_leo, TLEConstants.tleDoubleConstants).propagate(duration)
    
    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(1E-12)

    assert(finalState.a === 1.118136309343834, " a ")
    assert(finalState.e === 0.008415889590488213, " e ") // 0.008415889602669875?
    assert(finalState.i === 1.7113843433722917, " i ") // 1.7113843433722922	?
    assert(finalState.ω === 5.425763790324254, " perigee ")
    assert(finalState.Ω === 2.1020704722413246, " raan ") // 2.07534657893693?
    assert(finalState.ν === 155.44411314880793, " true anomaly ")  // 155.47483697687704?
  }
  
}