package predict4s

import spire.math.Fractional
import spire.algebra.Trig
import spire.implicits._

/**
 * Holds classical Kepler Coordinate elements:
 *
 * <pre>
 * Elements: { a,   e,    i,     &omega;,     &Omega;,     &nu;  }
 * Units:    {[m], [-], [rad], [rad], [rad], [rad]}
 * </pre>
 *
 * - Semi-major axis a: This is one half of the major axis, and thus runs from the center, through a focus, and to the
 * edge of the ellipse.
 * - Eccentricity e: May be interpreted as a measure of how much this shape deviates from a circle.
 * - Inclination i: This element tells you what the angle is between the ecliptic and the orbit. The inclination
 * ranges from 0 to &pi; rad.
 * - Argument of periapsis, &omega;: This is the angle between the orbit's periapsis (the point of closest approach to
 * the central point) and the orbit's ascending node (the point where the body crosses the plane of reference
 * from South to North). The angle is measured in the orbital plane and in the direction of motion.
 * - Right ascension of the ascending node, &Omega;: It is the angle from a reference direction, called the origin of
 * longitude, to the direction of the ascending node, measured in a reference plane.
 *
 * @param a Semi-major axis, a [m].
 * @param e Eccentricity, e [-].
 * @param i Inclination, i [rad].
 * @param ω Argument of periapsis, &omega; [rad].
 * @param Ω Right ascension of the ascending node, &Omega; [rad]
 * @param ν true anomaly [rad]
 * 
 */
case class KeplerCoord[F : Fractional: Trig](
  val a : F,      
  val e : F,      
  val i : F,      
  val ω : F,      
  val Ω : F,      
  val ν : F        
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
  
  /** A more verbose notation for ν */
  def xl = ν
  def trueAnomaly = ν
    
  def isCircular = e == 0

  def isElliptical = e < 1 && e >= 0

  def isParabolic = e == 1

  def isHyperbolical = e > 1
  
  override def toString = s"a: $a, e: $e, i: $i, raan: $Ω, ω: $ω, true anomaly: $xl"
}

case class Predict4sException(msg: String) extends Exception 

