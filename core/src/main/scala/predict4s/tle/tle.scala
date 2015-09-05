/**
 * See Dr. T.S. Kelso comments on TLE at 
 * http://www.celestrak.com/columns/v04n03/
 */
package predict4s.tle {

import spire.math._
import spire.implicits._

trait TLE[F] {
  def satelliteNumber: Int
  def classification: Char
  def launchYear: Int
  def launchNumber: Int
  def launchPiece: String
  def ephemerisType: Int
  def elementNumber: Int
  def year: Int
  def refepoch: F
  // These 6 elements correspond to classical Kepler Coordinates, with meanMotion instead of semimajor axis argument
  def meanMotion: F
  def e: F
  def i: F
  def pa: F
  def raan: F
  def meanAnomaly: F
  // ---
  def meanMotionFirstDerivative: F
  def meanMotionSecondDerivative: F
  def revolutionNumberAtEpoch: Int
  def bStar: F
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

  def apply[F: Fractional](line1 : String, line2: String ) : TLE[F] = new TLE[F] {
      def satelliteNumber = parseInteger(line1, 2, 5)
      def classification  = line1.charAt(7)
      def launchYear      = parseYear(line1, 9)
      def launchNumber    = parseInteger(line1, 11, 3)
      def launchPiece     = line1.substring(14, 17).trim
      def ephemerisType   = parseInteger(line1, 62, 1)
      def elementNumber   = parseInteger(line1, 64, 4)
      def year            = parseInteger(line1, 18, 2)
      val refepoch        = Fractional[F].fromDouble(parseDouble(line1, 20, 12))
      // mean motion development
      // converted from rev/day, 2 * rev/day^2 and 6 * rev/day^3 to rad/s, rad/s^2 and rad/s^3
      val meanMotion                 = Fractional[F].fromDouble(parseDouble(line2, 52, 11) * pi / 43200.0)
//        drag                       = line1.parseDouble(33, 10),
      val meanMotionFirstDerivative  = Fractional[F].fromDouble(parseDouble(line1, 33, 10) * pi / 1.86624e9)
//        nddot6                     = ((line1.substring(44, 45) + '.' +
//                                     line1.substring(45, 50) + 'e' +
//                                     line1.substring(50, 52)).replace(' ', '0')).toDouble,
      val meanMotionSecondDerivative = Fractional[F].fromDouble(((line1.substring(44, 45) + '.' +
                                   line1.substring(45, 50) + 'e' +
                                   line1.substring(50, 52)).replace(' ', '0').toDouble) *
                                   pi / 5.3747712e13)
      val e = Fractional[F].fromDouble(("." + line2.substring(26, 33).replace(' ', '0')).toDouble)
      val i = Fractional[F].fromDouble(parseDouble(line2, 8, 8).toRadians)
      val pa           = Fractional[F].fromDouble(parseDouble(line2, 34, 8).toRadians)
      val raan         = Fractional[F].fromDouble(line2.substring(17, 25).replace(' ', '0').toDouble.toRadians)
      val meanAnomaly  = Fractional[F].fromDouble(parseDouble(line2, 43, 8).toRadians)

      val revolutionNumberAtEpoch = parseInteger(line2, 63, 5)
      val bStar = Fractional[F].fromDouble(((line1.substring(53, 54) + '.' +
                line1.substring(54, 59) + 'e' +
                line1.substring(59, 61)).replace(' ', '0')).toDouble)
  }
  
  def isFormatOK(line1: String, line2: String) : Boolean = {
      if (line1 == null || line1.length() != 69 ||
          line2 == null || line2.length() != 69 ||  
          !LINE_1_PATTERN.matcher(line1).matches() ||
          !LINE_2_PATTERN.matcher(line2).matches()) 
         return false

      val satNum  = parseInteger(line1, 2, 5)
      val satNum2 = parseInteger(line2, 2, 5)
      if (satNum != satNum2) return false

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

}


} // end package
