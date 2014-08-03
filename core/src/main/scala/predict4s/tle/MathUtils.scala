package predict4s.tle
import scala.math._

object MathUtils {

    /**
     * Normalize an angle in a 2&pi; wide interval around a center value.
     */
  def normalizeAngle(a : Double, center: Double) =
     a - 2*Pi * floor((a + Pi - center) / (2* Pi))
  
}