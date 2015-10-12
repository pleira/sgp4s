package predict4s
import spire.algebra.{Trig, NRoot}
import spire.algebra.Field
import spire.math.pi
import spire.implicits._


trait ReferenceSystem {

  trait KeplerianElements[F] {
    def semiMajorAxis : F 
    def eccentricity : F
    def inclination : F
    def argumentOfPeriapsis : F
    def rightAscension : F
    def trueAnomaly : F
  }
  
  // TODO relate to spire's VectorSpaces 
  case class OrbitalElements[F](val a : F, val e : F, val i : F, val ω : F, val Ω : F, val ν : F) extends KeplerianElements[F] {
    def semiMajorAxis = a
    def eccentricity = e
    def inclination = i
    def argumentOfPeriapsis = ω
    def rightAscension = Ω
    def trueAnomaly = ν
    override def toString = s"a: $a, e: $e, i: $i, raan: $Ω, ω: $ω, true anomaly: $trueAnomaly"
  }
  case class PosVel[F](val p : IndexedSeq[F], v : IndexedSeq[F])
}

// can we do without exceptions, just scala.util.Try?
case class Predict4sException(msg: String) extends Exception 

