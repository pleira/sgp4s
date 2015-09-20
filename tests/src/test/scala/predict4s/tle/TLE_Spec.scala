package predict4s.tle
import org.scalatest._
import org.scalautils.TolerantNumerics
import predict4s.Predict4sException

class Step1_Spec extends FunSuite
  with BeforeAndAfterAll
  with ShouldMatchers {
  
  test("TLE Format must be correctly parsed") {
        val line1 = "1 27421U 02021A   02124.48976499 -.00021470  00000-0 -89879-2 0    20"
        val line2 = "2 27421  98.7490 199.5121 0001333 133.9522 226.1918 14.26113993    62"

        assert(TLE.isFormatOK(line1, line2))

        // instead of import spire.implicits._ for doubles I can do
        object ddd extends spire.std.DoubleInstances
        import ddd.DoubleAlgebra

        val tle : TLE = TLE(line1, line2)
        
        implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(1e-10)
       
        assert(27421 == tle.satelliteNumber)
        assert(2002 == tle.launchYear)
        assert(21 == tle.launchNumber)
        assert("A" == tle.launchPiece)
        assert(-0.0089879 == tle.dragCoeficient.toDouble)
        assert(0 == tle.ephemerisType)
        assert(98.749 === tle.inclination.toDouble)
        assert(199.5121 === tle.rightAscension.toDouble)
        assert(0.0001333 === tle.eccentricity.toDouble)
        assert(133.9522 === tle.argumentOfPeriapsis.toDouble)
        assert(226.1918 === tle.meanAnomaly.toDouble)
        assert(14.26113993 === tle.meanMotion.toDouble)
        assert(tle.revolutionNumberAtEpoch == 6)
        assert(tle.elementNumber == 2)
  }
  
//  test("Symmetry") {
//    checkSymmetry("1 27421U 02021A   02124.48976499 -.00021470  00000-0 -89879-2 0    20", "2 27421  98.7490 199.5121 0001333 133.9522 226.1918 14.26113993    62")
//    checkSymmetry("1 31928U 98067BA  08269.84884916  .00114257  17652-4  13615-3 0  4412", "2 31928  51.6257 175.4142 0001703  41.9031 318.2112 16.08175249 68368")
//  }

//  private def checkSymmetry(line1: String, line2: String) {
//    import spire.implicits._
//    val tleRef : TLE[Double] = TLE(line1, line2)
//    val tle : TLE[Double] = TLE(tleRef.satelliteNumber, tleRef.classification, tleRef.launchYear, tleRef.launchNumber, tleRef.launchPiece, tleRef.ephemerisType, tleRef.elementNumber, tleRef.year, tleRef.refepoch, tleRef.meanMotion, tleRef.meanMotionFirstDerivative, tleRef.meanMotionSecondDerivative, tleRef.e, tleRef.i, tleRef.pa, tleRef.raan, tleRef.meanAnomaly, tleRef.revolutionNumberAtEpoch, tleRef.bStar)
//    (line1, line2)
//  }

//  test("Bug74") {
//    checkSymmetry("1 00001U 00001A   12026.45833333 2.94600864  39565-9  16165-7 1    12", "2 00001 627.0796 454.4522 0000000 624.9662   0.4817  0.00000000    12")
//  }
//
//  test("Bug77") {
//    checkSymmetry("1 05555U 71086J   12026.96078249 -.00000004  00001-9  01234-9 0  9082", "2 05555  74.0161 228.9750 0075476 328.9888  30.6709 12.26882470804545")
//  }

//  test("Different SatNumbers") {
//    intercept[Predict4sException] {
//    TLE("1 27421U 02021A   02124.48976499 -.00021470  00000-0 -89879-2 0    20", "2 27422  98.7490 199.5121 0001333 133.9522 226.1918 14.26113993    62")
//    }
//  }

  test("Checksum OK") {
    TLE.isFormatOK("1 27421U 02021A   02124.48976499 -.00021470  00000-0 -89879-2 0    20", "2 27421  98.7490 199.5121 0001333 133.9522 226.1918 14.26113993    62")
  }

  test("Wrong Checksum1") {
    assert(!TLE.isFormatOK("1 27421U 02021A   02124.48976499 -.00021470  00000-0 -89879-2 0    21", "2 27421  98.7490 199.5121 0001333 133.9522 226.1918 14.26113993    62"))
  }

  test("Wrong Checksum2") {
    assert(!TLE.isFormatOK("1 27421U 02021A   02124.48976499 -.00021470  00000-0 -89879-2 0    20", "2 27421  98.7490 199.5121 0001333 133.9522 226.1918 14.26113993    61"))
  }
  
}