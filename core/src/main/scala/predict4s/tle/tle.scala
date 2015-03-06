/**
 * See Dr. T.S. Kelso comments on TLE at 
 * http://www.celestrak.com/columns/v04n03/
 */
package predict4s.tle {

import spire.algebra._
import spire.math._
import spire.implicits._

case class TLE[F: Fractional: Trig] (
  // identification
  val satelliteNumber: Int,  
  val classification: Char, 
  val launchYear: Int, 
  val launchNumber: Int,  
  val launchPiece: String, 
  val ephemerisType:Int,  
  val elementNumber: Int, 

  // orbital parameters
  val year: Int, 
  val refepoch : F, 
  val meanMotion: F, 
  val meanMotionFirstDerivative: F, 
  val meanMotionSecondDerivative: F,   
  val e: F, 
  val i: F, 
  val pa: F,   
  val raan: F,   
  val meanAnomaly: F, 
  val revolutionNumberAtEpoch: Int,  
  val bStar: F
)  {
//  FIXME
  val tlec = new TLEConstants[F]();
  import tlec._
  
  def meanMotion0: F = (meanMotion / pi) * 43200
  def epoch : F = 1000 * year + refepoch

  // Intermediate values used by the propagator models
  def cosi0 = cos(i)       
  def r1 = cosi0
  def theta2  = cosi0 * cosi0
  def e0sq = e * e 
  def xno = meanMotion * 60

  def a1 = (KE / xno) fpow (TWO_THIRD)
  def x3thm1 = 3 * theta2 - 1
  def beta02 = 1 - e0sq
  def beta0 = sqrt(beta02)
  def tval = CK2 * 1.5 * x3thm1 / (beta0 * beta02)
  def delta1 = tval/(a1 * a1)
  
  // original semimajor axis
  def a0 = a1 * (1 - delta1 * (ONE_THIRD + delta1 * (1 + 134.0/81.0 * delta1)))
//  
  def delta0 = tval/(a0 * a0)
  def xn0dp = xno /(delta0 + 1)   // the original mean motion
   
  // Select a deep-space/near-earth ephemeris 
  def isDeepSpacePeriod : Boolean = {
    val a = Fractional[F].fromDouble(0.15625 / (2.0*pi))
    val b = Fractional[F].one / (xn0dp * MINUTES_PER_DAY)
    b >= a // (1.0 / 6.4)
  }
  def isDeepSpace : Boolean = isDeepSpacePeriod
  
//  // def drag : F 
//  def KC = KeplerCoord[F](a0, e, i, pa, raan, xn0dp)
  def incl : F = i                   // already in Radians 
  def nodeo : F = raan               // already Radians
  def omegao : F = pa                // argper // already Radians 
  def xmo : F = meanAnomaly          // already in Radians
//  def nddot6 : F
//  def xndt2o = drag * 2 * π / MINUTES_PER_DAY / MINUTES_PER_DAY
  
}


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

    
    def apply(line1 : String, line2: String ) : TLE[Double] = {

      val satNum  = parseInteger(line1, 2, 5)
      val satNum2 = parseInteger(line2, 2, 5)
      if (satNum != satNum2) throw new predict4s.Predict4sException("different satnum") 

      // identification
      TLE[Double](
        satelliteNumber = satNum,
        classification  = line1.charAt(7),
        launchYear      = parseYear(line1, 9),
        launchNumber    = parseInteger(line1, 11, 3),
        launchPiece     = line1.substring(14, 17).trim,
        ephemerisType   = parseInteger(line1, 62, 1),
        elementNumber   = parseInteger(line1, 64, 4),
        year            = parseInteger(line1, 18, 2),
        refepoch        = parseDouble(line1, 20, 12),
        // mean motion development
        // converted from rev/day, 2 * rev/day^2 and 6 * rev/day^3 to rad/s, rad/s^2 and rad/s^3
        meanMotion                 = parseDouble(line2, 52, 11) * pi / 43200.0,
//        drag                       = line1.parseDouble(33, 10),
        meanMotionFirstDerivative  = parseDouble(line1, 33, 10) * pi / 1.86624e9,
//        nddot6                     = ((line1.substring(44, 45) + '.' +
//                                     line1.substring(45, 50) + 'e' +
//                                     line1.substring(50, 52)).replace(' ', '0')).toDouble,
        meanMotionSecondDerivative = ((line1.substring(44, 45) + '.' +
                                     line1.substring(45, 50) + 'e' +
                                     line1.substring(50, 52)).replace(' ', '0')).toDouble *
                                     pi / 5.3747712e13,
        e = ("." + line2.substring(26, 33).replace(' ', '0')).toDouble,
        i = parseDouble(line2, 8, 8).toRadians,
        pa           = parseDouble(line2, 34, 8).toRadians,
        raan         = line2.substring(17, 25).replace(' ', '0').toDouble.toRadians,
        meanAnomaly  = parseDouble(line2, 43, 8).toRadians,

        revolutionNumberAtEpoch = parseInteger(line2, 63, 5),
        bStar = ((line1.substring(53, 54) + '.' +
                  line1.substring(54, 59) + 'e' +
                  line1.substring(59, 61)).replace(' ', '0')).toDouble
        )
    }
 
    
    def buildReal(line1 : String, line2: String ) : TLE[Real] = {

      val satNum  = parseInteger(line1, 2, 5)
      val satNum2 = parseInteger(line2, 2, 5)
      if (satNum != satNum2) throw new predict4s.Predict4sException("different satnum") 

      // identification
      TLE[Real](
        satelliteNumber = satNum,
        classification  = line1.charAt(7),
        launchYear      = parseYear(line1, 9),
        launchNumber    = parseInteger(line1, 11, 3),
        launchPiece     = line1.substring(14, 17).trim,
        ephemerisType   = parseInteger(line1, 62, 1),
        elementNumber   = parseInteger(line1, 64, 4),
        year            = parseInteger(line1, 18, 2),
        refepoch        = Real(parseDouble(line1, 20, 12)),
        // mean motion development
        // converted from rev/day, 2 * rev/day^2 and 6 * rev/day^3 to rad/s, rad/s^2 and rad/s^3
        meanMotion                 = Real(parseDouble(line2, 52, 11) * pi / 43200.0),
//        drag                       = line1.parseDouble(33, 10),
        meanMotionFirstDerivative  = Real(parseDouble(line1, 33, 10) * pi / 1.86624e9),
//        nddot6                     = ((line1.substring(44, 45) + '.' +
//                                     line1.substring(45, 50) + 'e' +
//                                     line1.substring(50, 52)).replace(' ', '0')).toDouble,
        meanMotionSecondDerivative = Real((line1.substring(44, 45) + '.' +
                                     line1.substring(45, 50) + 'e' +
                                     line1.substring(50, 52)).replace(' ', '0').toDouble) *
                                     pi / 5.3747712e13,
        e = Real(("." + line2.substring(26, 33).replace(' ', '0')).toDouble),
        i = Real(parseDouble(line2, 8, 8).toRadians),
        pa           = Real(parseDouble(line2, 34, 8).toRadians),
        raan         = Real(line2.substring(17, 25).replace(' ', '0').toDouble.toRadians),
        meanAnomaly  = Real(parseDouble(line2, 43, 8).toRadians),

        revolutionNumberAtEpoch = parseInteger(line2, 63, 5),
        bStar = Real(((line1.substring(53, 54) + '.' +
                  line1.substring(54, 59) + 'e' +
                  line1.substring(59, 61)).replace(' ', '0')).toDouble)
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

    def parseInteger(line : String, start: Int, length: Int) : Int = {
      line.substring(start, start + length).replace(' ', '0').toInt
    }
  
    def parseDouble(line : String, start: Int, length: Int) : Double = {
      line.substring(start, start + length).toDouble
    }
    
    def parseYear(line : String, start: Int) : Int = {
      val year = 2000 + parseInteger(line, start, 2)
      if (year > 2056) (year - 100) else year
    }

//	implicit class TLEString(line: String) {
//	  
//	  def parseInteger(start: Int, length: Int) : Int = 
//	    line.substring(start, start + length).replace(' ', '0').toInt
//	
//	  def parseDouble(start: Int, length: Int) : Double = 
//	    line.substring(start, start + length).toDouble
//		  
//	  def parseYear(start: Int) : Int = {
//	    val year = 2000 + line.parseInteger(start, 2)
//	    if (year > 2056) (year - 100) else year
//	  }
//	}
}

} // end package
