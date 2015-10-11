package predict4s
import spire.algebra.{Ring, Field}


package object tle {
  
  //type FNOT = Field with NRoot with Order with Trig
  
  // FIXME: to remove with a Spire version > 10.2
  implicit class IntAs(val n:Int)  {
    def as[A](implicit ev:Ring[A]) = ev.fromInt(n)
  }

  implicit class DoubleAs(val n:Double) {
    def as[A](implicit ev:Field[A]) = ev.fromDouble(n)
  }

  // true equator, mean equinox (TEME) (Herrick, 1971:325, 338, 341)
  val TEME = ReferenceSystem("TEME")
  
  def TWO_THIRD[F: Field] = (2.0/3.0).as[F]
  
}