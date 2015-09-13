package predict4s
import spire.algebra.{Ring, Field}

package object tle {
  
  // FIXME: to remove with a Spire version > 10.2
  
  implicit class IntAs(val n:Int)  {
    def as[A](implicit ev:Ring[A]) = ev.fromInt(n)
  }

  implicit class DoubleAs(val n:Double) {
    def as[A](implicit ev:Field[A]) = ev.fromDouble(n)
  }

}