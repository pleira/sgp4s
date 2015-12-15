package predict4s.tle.vallado

import predict4s.tle.Sgp4Near
import predict4s.tle.TEME
import predict4s.tle.Sgp4Result
import predict4s.tle.Sgp4Near
import predict4s.tle.TLE

case class Sgp4ValladoResult(
    sgp: SGP4Vallado[Double], 
    statett: (TEME.CartesianElems[Double], TEME.CartesianElems[Double], SGP4Vallado[Double]#SpecialPolarNodal, 
        SGP4Vallado[Double]#ShortPeriodPolarNodalContext, SGP4Vallado[Double]#LongPeriodPolarNodalContext, SGP4Vallado[Double]#EccentricAnomalyState), tle: TLE, t: Double) 
        extends Sgp4Result[Double] {
  val posVel = statett._1 
  val r = posVel.pos
  val v = posVel.vel
    
  val secElemt = statett._5
    
  import sgp.elem0
  import sgp.geoPot._
  import sgp.isImpacting
  import sgp.{secularTerms,laneCoefs}
  import statett._
  val error = 0
  val x = r(0); 
  val y = r(1) ; 
  val z = r(2); 
  val xdot = v(0) ; 
  val ydot = v(1) ; 
  val zdot = v(2) ;
  def satn = tle.satelliteNumber
  def ecco  : Double = elem0.e
  def epoch : Double = elem0.epoch
  def inclo : Double = elem0.I
  
 //   outputs : 
  // def method  : Char  = 'n' // FIXME if (isDeepSpace) 'd' else 'n'
  
  def  ainv  : Double    = 1 / elem0.a
  def    ao  : Double    = elem0.a
  def con41  : Double    = sgp.ctx0.x3thm1   // FIXME for d
  def con42  : Double    = sgp.ctx0.con42
  def cosio  : Double    = sgp.ctx0.c // θ
  def cosio2 : Double    = sgp.ctx0.`c²` // θsq
  def eccsq  : Double    = sgp.ctx0.`e²` // e0sq 
  def omeosq : Double    = 1 - sgp.ctx0.`e²` //  β0sq
  def  posq  : Double    = sgp.ctx0.`p²` // posq
  def    rp  : Double    = sgp.rp
  def rteosq : Double    = math.sqrt(1 - sgp.ctx0.`e²`) // β0
  def sinio  : Double    = sgp.ctx0.s
  def  gsto  : Double    = sgp.gsto    
  
  // ---
  def      yr  : Int    = tle.epochyear
  def   bstar  : Double = elem0.bStar
  def   argpo  : Double = elem0.ω
  def      mo  : Double = elem0.M
  def   isimp  : Int    = if ((omeosq >= 0.0) || (no >= 0.0))
                                  if (isImpacting) 1 else 0
                                // FIXME: should be if (isImpacting || isDeepSpace) 1 else 0
                          else 0
  def   aycof  : Double = secularTerms._2.aycof // FIXME for d
  def    cc1   : Double =  C1   
  def     cc4  : Double =  C4 
  def     cc5  : Double =  C5   
  def      d2  : Double =  D2
  def      d3  : Double =  D3     
  def      d4  : Double =  D4 
  def   delmo  : Double =  secularTerms._2.delM0
  def     eta  : Double =  sgp.gctx.η
  def  argpdot : Double =  secularTerms._1.ωdot 
  def   omgcof : Double =  secularTerms._2.ωcof 
  def   sinmao : Double = math.sin(elem0.M)
  def   t2cof  : Double = laneCoefs.t2cof
  def   t3cof  : Double = laneCoefs.t3cof
  def   t4cof  : Double = laneCoefs.t4cof
  def   t5cof  : Double = laneCoefs.t5cof
  def  x1mth2  : Double = sgp.ctx0.x1mth2 // FIXME for d
  def  x7thm1  : Double = sgp.ctx0.x7thm1 // FIXME for d
  def   xlcof  : Double = secularTerms._2.xlcof // FIXME for d
  def   xmcof  : Double = secularTerms._2.Mcof
//      def temp1 : Double = 3 * J2 / posq * no / 2  
//      def temp2 : Double = J2/ posq *temp1 / 2
  //def    mdot  : Double = no + 0.5 * temp1 * rteosq * con41 + 0.0625 * temp2 * rteosq * (13.0 - 78.0 * cosio2 + 137.0 * cosio2*cosio2) // tif.ocf.mdot
  // def    mdot  : Double = no + temp1 * rteosq * con41 / 2 + temp2 * rteosq * (13 - 78 * cosio2 + 137 * cosio2*cosio2) / 16 // 
  def    mdot  : Double = secularTerms._1.Mdot
  def   nodecf : Double = secularTerms._2.Ωcof
  def   nodedt : Double = secularTerms._1.Ωdot     
  def   nodeo  : Double = elem0.Ω
  def      no  : Double = elem0.n
  // FIXME
  def atime : Double = t
    
}