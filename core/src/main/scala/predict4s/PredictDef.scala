package predict4s
import spire.algebra.{Trig, NRoot}
import spire.algebra.Field
import spire.math._
import spire.implicits._


trait ReferenceSystem {

  case class ClassicalElems[F](
        a : F, // semimajor axis 
        e : F, // eccentricity
        i : F, // inclination
        ω : F, // argument of perigee
        Ω : F, // right ascension ascending node
        M : F) // mean anomaly
  
  case class OrbitalElements[F](val a : F, val e : F, val i : F, val ω : F, val Ω : F, val ν : F) {
    def semiMajorAxis = a
    def eccentricity = e
    def inclination = i
    def argumentOfPeriapsis = ω
    def rightAscension = Ω
    def trueAnomaly = ν
    override def toString = s"a: $a, e: $e, i: $i, raan: $Ω, ω: $ω, true anomaly: $trueAnomaly"
  }
      
  case class SGPElems[F](
        n : F, // mean motion 
        e : F, // eccentricity
        i : F, // inclination
        ω : F, // argument Of perigee
        Ω : F, // right ascension ascending node
        M : F, // mean anomaly
        bStar : F, // atmospheric Drag Coeficient
        epoch : F) // epoch time in days from jan 0, 1950. 0 hr 
  {
    def n0 = n  
    def e0 = e 
    def i0 = i 
    def ω0 = ω 
    def Ω0 = Ω
    def M0 = M 
  }
  
         
  case class CartesianElems[F](x: F, y: F, z: F, vx: F, vy: F, vz: F) {
    def pos = Vector[F](x,y,z)
    def vel = Vector[F](vx,vy,vz)
  }
    
  case class DelaunayElems[F](
        ℓ : F, L : F,  // mean anomaly and its conjugate momentum L = √µ a (the Delaunay action),
        g : F, G : F,  // the argument of the perigee g = ω and its conjugate momentum G = L√(1 − e*e)
                       // (the total angular momentum), where e is the orbital eccentricity, 
        h : F, H : F   // the argument of the node h and its conjugate momentum H = G cosI (the polar component of the angular momentum).       
  )
  
  
  def coord2UnitCartesian[F: Field: Trig](xinc: F, su: F, xnode: F) = {
  
      /* --------------------- orientation vectors ------------------- */
      val     sinsu =  sin(su)
      val     cossu =  cos(su)
      val     snod  =  sin(xnode)
      val     cnod  =  cos(xnode)
      val     sini  =  sin(xinc)
      val     cosi  =  cos(xinc)
      val     xmx   = -snod * cosi
      val     xmy   =  cnod * cosi
      val     ux    =  xmx * sinsu + cnod * cossu
      val     uy    =  xmy * sinsu + snod * cossu
      val     uz    =  sini * sinsu
      val     vx    =  xmx * cossu - cnod * sinsu
      val     vy    =  xmy * cossu - snod * sinsu
      val     vz    =  sini * cossu
  
      // return unit vectors position and velocity
      CartesianElems(ux,uy,uz,vx,vy,vz)
    }  
}

// can we do without exceptions, just scala.util.Try?
case class Predict4sException(msg: String) extends Exception 

