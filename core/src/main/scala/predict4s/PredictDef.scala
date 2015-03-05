package predict4s

import spire.math.Fractional
import spire.algebra.Order
import spire.implicits._

// TODO: have radians
case class KeplerCoord[F : Fractional : Order](
  val a : F,      //  semi major axis
  val e : F,      //  eccentricity
  val i : F,      //  inclination
  val ω : F,      //  perigee argument
  val Ω : F,      //  RAAN (rad)
  val xl : F      //  direction of vernal equinox, L from SPTRCK #3 
) {
  
  /** A more verbose name for a */
  def semiMajorAxis = a

  /** A more verbose name for e */
  def eccentricity = e

  /** A more verbose name for i */
  def inclination = i

  /** A more verbose name for ω */
  def omega = ω 
  def argumentOfPeriapsis = ω

  /** A more verbose name for Ω */
  def raan = Ω 
  def rightAscension = Ω

  def isCircular = e == 0

  def isElliptical = e < 1 && e >= 0

  def isParabolic = e == 1

  def isHyperbolical = e > 1
  
  override def toString = s"a: $a, e: $e, i: $i, raan: $raan, omega: $ω, vernal equinox: $xl"
}

object OrbitPropagator {
  type Vector3D = Vector[Double]
  type Position = Vector3D
  type Velocity = Vector3D
}

case class Predict4sException(msg: String) extends Exception 

