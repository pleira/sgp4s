/**
 * See Dr. T.S. Kelso comments on TLE at 
 * http://www.celestrak.com/columns/v04n03/
 */
package predict4s.tle {

import java.util.regex.Pattern
import TLEConstants._
import spire.math._
import spire.implicits._

import predict4s.KeplerCoord
import predict4s.Predict4sException

trait TLE {
  // identification
  def satelliteNumber: Int
  def classification: Char // U for unclassified
  def launchYear: Int
  def launchNumber: Int 
  def launchPiece: String  
  def ephemerisType:Int 
  def elementNumber: Int 
  // orbital parameters
  def year: Int 
  def refepoch : Double 
  def meanMotion: Double                  // rad/s
  def meanMotionFirstDerivative: Double   // rad/s^2
  def meanMotionSecondDerivative: Double  // rad/s^3
  def e: Double                           // eccentricity
  def i: Double                           // inclination
  def pa: Double                          // argument of perigee (rad)
  def raan: Double                        // right ascension of ascending node (rad)
  def meanAnomaly: Double                 // rad
  def revolutionNumberAtEpoch: Int      
  def bStar: Double                       // ballistic coefficient
  
  def meanMotion0: Double = (meanMotion / pi) * 43200.0
  def epoch = 1000.0 * year + refepoch
  def incl : Double = i                   // already in Radians 
  def nodeo : Double = raan               // already Radians
  def omegao : Double = pa                // argper // already Radians 
  def xmo : Double = meanAnomaly          // already in Radians
//  def nddot6 : Double
//  def xndt2o = drag * 2 * π / MINUTES_PER_DAY / MINUTES_PER_DAY
  
  // Intermediate values used by the propagator models
  def cosi0 = cos(i)       
  def r1 = cosi0
  def theta2  = cosi0 * cosi0
  def e0sq = e * e 
  def xno = meanMotion * 60
  
  def a1 = pow (KE / xno,  TWO_THIRD)
  def x3thm1 = 3 * theta2 - 1.0
  def beta02 = 1 - e0sq
  def beta0 = sqrt(beta02)
  def tval = CK2 * 1.5 * x3thm1 / (beta0 * beta02)
  def delta1 = tval/(a1 * a1)
  
  // original semimajor axis
  def a0 = a1 * (1 - delta1 * (ONE_THIRD + delta1 * (1.0 + 134.0/81.0 * delta1)))
  
  def delta0 = tval/(a0 * a0)
  def xn0dp = xno /(delta0 + 1)   // the original mean motion
   
  // Select a deep-space/near-earth ephemeris 
  def isDeepSpacePeriod : Boolean = (2*pi / (xn0dp * MINUTES_PER_DAY)) >= (1.0 / 6.4)
  def isDeepSpace : Boolean = isDeepSpacePeriod
  
  // def drag : Double 
  def KC = KeplerCoord[Double](raan, a0, e, i, pa, xn0dp)
}

case class TLE0(
  // identification
  satelliteNumber: Int,  
  classification: Char, 
  launchYear: Int, 
  launchNumber: Int,  
  launchPiece: String, 
  ephemerisType:Int,  
  elementNumber: Int, 

  // orbital parameters
  year: Int, 
  refepoch : Double, 
  meanMotion: Double, 
  meanMotionFirstDerivative: Double, 
  meanMotionSecondDerivative: Double,   
  e: Double, 
  i: Double, 
  pa: Double,   
  raan: Double,   
  meanAnomaly: Double, 
  revolutionNumberAtEpoch: Int,  
  bStar: Double
) extends TLE


object TLE {
    import java.util.regex.Pattern;
    import spire.math.pi

    /** Pattern for line 1. */
    val LINE_1_PATTERN : Pattern =
        Pattern.compile("1 [ 0-9]{5}U [ 0-9]{5}[ A-Z]{3} [ 0-9]{5}[.][ 0-9]{8} [ +-][.][ 0-9]{8} " +
                        "[ +-][ 0-9]{5}[+-][ 0-9] [ +-][ 0-9]{5}[+-][ 0-9] [ 0-9] [ 0-9]{4}[ 0-9]")

    /** Pattern for line 2. */
    val LINE_2_PATTERN : Pattern =
        Pattern.compile("2 [ 0-9]{5} [ 0-9]{3}[.][ 0-9]{4} [ 0-9]{3}[.][ 0-9]{4} [ 0-9]{7} " +
                        "[ 0-9]{3}[.][ 0-9]{4} [ 0-9]{3}[.][ 0-9]{4} [ 0-9]{2}[.][ 0-9]{13}[ 0-9]")

    
    def apply(line1 : String, line2: String ) : TLE = {

      val satNum  = line1.parseInteger(2, 5)
      val satNum2 = line2.parseInteger(2, 5)
      if (satNum != satNum2) throw new Predict4sException("different satnum") 

      // identification
      TLE0(
        satelliteNumber = satNum,
        classification  = line1.charAt(7),
        launchYear      = line1.parseYear(9),
        launchNumber    = line1.parseInteger(11, 3),
        launchPiece     = line1.substring(14, 17).trim,
        ephemerisType   = line1.parseInteger(62, 1),
        elementNumber   = line1.parseInteger(64, 4),
        year            = line1.parseInteger(18, 2),
        refepoch        = line1.parseDouble(20, 12),
        // mean motion development
        // converted from rev/day, 2 * rev/day^2 and 6 * rev/day^3 to rad/s, rad/s^2 and rad/s^3
        meanMotion                 = line2.parseDouble(52, 11) * pi / 43200.0,
//        drag                       = line1.parseDouble(33, 10),
        meanMotionFirstDerivative  = line1.parseDouble(33, 10) * pi / 1.86624e9,
//        nddot6                     = ((line1.substring(44, 45) + '.' +
//                                     line1.substring(45, 50) + 'e' +
//                                     line1.substring(50, 52)).replace(' ', '0')).toDouble,
        meanMotionSecondDerivative = ((line1.substring(44, 45) + '.' +
                                     line1.substring(45, 50) + 'e' +
                                     line1.substring(50, 52)).replace(' ', '0')).toDouble *
                                     pi / 5.3747712e13,
        e = ("." + line2.substring(26, 33).replace(' ', '0')).toDouble,
        i = line2.parseDouble(8, 8).toRadians,
        pa           = line2.parseDouble(34, 8).toRadians,
        raan         = line2.substring(17, 25).replace(' ', '0').toDouble.toRadians,
        meanAnomaly  = line2.parseDouble(43, 8).toRadians,

        revolutionNumberAtEpoch = line2.parseInteger(63, 5),
        bStar = ((line1.substring(53, 54) + '.' +
                  line1.substring(54, 59) + 'e' +
                  line1.substring(59, 61)).replace(' ', '0')).toDouble
        )
    }
 

    def isFormatOK(line1: String, line2: String) : Boolean = {
        if (line1 == null || line1.length() != 69 ||
            line2 == null || line2.length() != 69 ||   
            !LINE_1_PATTERN.matcher(line1).matches() ||
            !LINE_2_PATTERN.matcher(line2).matches()) 
           return false

        // check sums
        val checksum1 = checksum(line1)
        if (line1.substring(68).toInt != (checksum1 % 10)) 
          return false
        
        val checksum2 = checksum(line2)
        if (line2.substring(68).toInt != (checksum2 % 10)) 
          return false
        
        true
    }

    def checksum(line: String): Int = {
        var sum: Int = 0
        for (j <- 0 until 68) {
         val c = line(j)
         if (Character.isDigit(c)) {
              sum += Character.digit(c, 10)
            } else if (c == '-') {
              sum += 1
            }
        }
        sum % 10
    }
  
	implicit class TLEString(line: String) {
	  
	  def parseInteger(start: Int, length: Int) : Int = 
	    line.substring(start, start + length).replace(' ', '0').toInt
	
	  def parseDouble(start: Int, length: Int) : Double = 
	    line.substring(start, start + length).toDouble
		  
	  def parseYear(start: Int) : Int = {
	    val year = 2000 + line.parseInteger(start, 2)
	    if (year > 2056) (year - 100) else year
	  }
	}
}

} // end package
