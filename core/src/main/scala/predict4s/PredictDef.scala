package predict4s

case class ReferenceSystem(val name: String) {

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

case class SGPElements[F](
    n0 : F, // mean motion 
    e0 : F, // eccentricity
    i0 : F, // inclination
    ω0 : F, // argument Of perigee
    Ω0 : F, // right ascension ascending node
    M0 : F, // mean anomaly
    bStar : F, // atmospheric Drag Coeficient
    epoch : F) 
{
  
}
  
}

// can we do without exceptions, just scala.util.Try?
case class Predict4sException(msg: String) extends Exception 

