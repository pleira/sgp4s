package predict4s

import scala.concurrent.duration._

// TODO: have radians
case class KeplerCoord[F <: Double](
  val raan : F,   //  RAAN (rad)
  val a : F,      //  semi major axis
  val e : F,      //  eccentricity
  val i : F,      //  inclination
  val omega : F,  //  perigee argument
  val xl : F      //  direction of vernal equinox, L from SPTRCK #3 
) {
  def ω = omega 
  override def toString = s"a: $a, e: $e, i: $i, raan: $raan, omega: $ω, vernal equinox: $xl"
}

object OrbitPropagator {
  type Vector3D = Vector[Double]
  type Position = Vector3D
  type Velocity = Vector3D
}

case class Predict4sException(msg: String) extends Exception 

