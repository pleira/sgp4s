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
  
  case class KeplerCoord[F](val a : F, val e : F, val i : F, val ω : F, val Ω : F, val ν : F) extends KeplerianElements[F] {
    def semiMajorAxis = a
    def eccentricity = e
    def inclination = i
    def argumentOfPeriapsis = ω
    def rightAscension = Ω
    def trueAnomaly = ν
    override def toString = s"a: $a, e: $e, i: $i, raan: $Ω, ω: $ω, true anomaly: $trueAnomaly"
  }
  // remove Vector restriction when tests can traverse the collection
  case class PosVel[F](val p : Vector[F], v : Vector[F])
  // case class PosVel[F](val p : GenTraversable[F], v : GenTraversable[F])   
}

// can we do without exceptions?
case class Predict4sException(msg: String) extends Exception 

