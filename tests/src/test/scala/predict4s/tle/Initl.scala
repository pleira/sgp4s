package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._

trait Initl {
  def satn: Int
  def ecco : Double
  def epoch : Double
  def inclo : Double
  
  def    no  : Double   
 //   outputs : 
  def  ainv  : Double     ; def    ao  : Double  ; def con41 : Double  ; def con42  : Double  ; def cosio  : Double  ; def cosio2  : Double    
  def eccsq  : Double  ; def omeosq : Double     ; def  posq  : Double ; def    rp  : Double  ; def rteosq : Double  ; 
  def sinio  : Double  ; def  gsto  : Double     ; 
}


trait  Sgp4Init {
def satn: Int; def      yr  : Int      ; def   bstar  : Double; def    ecco  : Double; def   epoch  : Double; def   argpo  : Double; 
def inclo  : Double     ; def      mo  : Double   ; 
// in and out variables 
def    no  : Double     ; 
//    outputs  :
def   isimp  : Int      ; def  method  : Char     ; def   aycof  : Double     ; 
def con41  : Double     ; def    cc1   : Double     ; def     cc4  : Double    ; 
def   cc5  : Double     ; def      d2  : Double     ; def      d3  : Double     ; def      d4  : Double     ; def   delmo  : Double; 
def   eta  : Double     ; def  argpdot : Double     ; def   omgcof : Double; 
def sinmao : Double     ; def   t2cof  : Double     ; def   t3cof  : Double    ; 
def t4cof  : Double     ; def   t5cof  : Double     ; def    gsto  : Double    ; def  x1mth2  : Double ; def  x7thm1  : Double ; def   xlcof  : Double ; 
def xmcof  : Double     ; def    mdot  : Double     ; def   nodecf : Double    ; def   nodedt : Double  ; 
//   in and outputs from deep space satellites :
def     t  : Double     ; def   nodeo  : Double     ; 
}

// Todo: given SGP4TimeIndependentFunctions[Double] build Initl and Sgp4Init
// it can be via type classes or by making 
trait Sgp4Vars extends Initl  with Sgp4Init  


object Sgp4Vars {
  
  
  implicit val wgs = WGS72Constants.tleDoubleConstants

  def f(tle: TLE) : TEME.SGPElements[Double] = TEME.SGPElements(tle)
  
  def g(el: TEME.SGPElements[Double]) : SGP4TimeIndependentFunctions[Double] = SGP4TimeIndependentFunctions(el)

  def apply(tle: TLE) : Sgp4Vars = {
    import tle._ 
    
    val tif : SGP4TimeIndependentFunctions[Double] = g(f(tle))
    import tif._
  import wgs._
  import i0f._
  import e0f._
  import bmmf._
  import sf._
  import coeff._
  import ilf._
  import ocf._
  
    new Sgp4Vars {
      def satn = satelliteNumber
      def ecco = e0
      def epoch : Double = tif.ini.epoch 
      def inclo : Double = i0
    
      //def    no  : Double  = n0 // or n0k
   //   outputs : 
      def method  : Char  = if (isDeepSpace) 'd' else 'n'
    
      def  ainv  : Double    = 1 / a0 
      def    ao  : Double    = a0 
      def con41  : Double    = x3thm1   // FIXME for d
      def con42  : Double    = tif.i0f.con42
      def cosio  : Double    = θ
      def cosio2 : Double    = θsq
      def eccsq  : Double    = e0sq 
      def omeosq : Double    = β0sq
      def  posq  : Double    = tif.sf.posq
      def    rp  : Double    = tif.bmmf.rp
      def rteosq : Double    = β0
      def sinio  : Double    = tif.i0f.sinio
      def  gsto  : Double    = tif.ocf.gsto    
      
      // ---
      def      yr  : Int    = tle.epochyear
      def   bstar  : Double = tif.ini.bStar
      def   argpo  : Double = tif.ini.ω0
      def      mo  : Double = tif.ini.M0
      def   isimp  : Int    = if ((omeosq >= 0.0) || (no >= 0.0)) { 
        var isimp : Int = if (tif.bmmf.isImpacting) 1 else 0
        isimp = if (isDeepSpace) 1 else 0 
        isimp
      } else 0
      def   aycof  : Double = tif.ocf.aycof // FIXME for d
      def    cc1   : Double =  C1   
      def     cc4  : Double =  C4 
      def     cc5  : Double =  C5   
      def      d2  : Double =  D2   
      def      d3  : Double =  D3     
      def      d4  : Double =  D4 
      def   delmo  : Double =  delM0
      def     eta  : Double =  η
      def  argpdot : Double =  ωdot 
      def   omgcof : Double =  ωcof 
      def   sinmao : Double =  sinM0.toDouble
      def   t2cof  : Double = tif.ilf.t2cof
      def   t3cof  : Double = tif.ilf.t3cof
      def   t4cof  : Double = tif.ilf.t4cof
      def   t5cof  : Double = tif.ilf.t5cof
      def  x1mth2  : Double = tif.i0f.x1mth2 // FIXME for d
      def  x7thm1  : Double = tif.ocf.x7thm1 // FIXME for d
      def   xlcof  : Double = tif.ocf.xlcof // FIXME for d
      def   xmcof  : Double = Mcof
//      def temp1 : Double = 3 * J2 / posq * no / 2  
//      def temp2 : Double = J2/ posq *temp1 / 2
      //def    mdot  : Double = no + 0.5 * temp1 * rteosq * con41 + 0.0625 * temp2 * rteosq * (13.0 - 78.0 * cosio2 + 137.0 * cosio2*cosio2) // tif.ocf.mdot
      // def    mdot  : Double = no + temp1 * rteosq * con41 / 2 + temp2 * rteosq * (13 - 78 * cosio2 + 137 * cosio2*cosio2) / 16 // 
      def    mdot  : Double = tif.ocf.mdot
      def   nodecf : Double = Ωcof
      def   nodedt : Double = omegadot     
      def        t          = 0.0
      def   nodeo  : Double = tif.ini.Ω0
      def      no  : Double = tif.bmmf.n0
    }
  }
  
}