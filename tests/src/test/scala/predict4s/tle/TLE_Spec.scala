package predict4s.tle
import org.scalatest._

class Step1_Spec extends FunSuite {
  
  test("TLE Format must be correctly parsed") {
        val line1 = "1 27421U 02021A   02124.48976499 -.00021470  00000-0 -89879-2 0    20"
        val line2 = "2 27421  98.7490 199.5121 0001333 133.9522 226.1918 14.26113993    62"

        assert(TLE.isFormatOK(line1, line2))

        val tle : TLE = TLE(line1, line2)
        
        assert(27421 == tle.satelliteNumber)
        assert(2002 == tle.launchYear)
        assert(21 == tle.launchNumber)
        assert("A" == tle.launchPiece)
        assert(-0.0089879 == tle.atmosphericDragCoeficient.toDouble)
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