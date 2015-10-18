package predict4s.tle

import spire.algebra._
import spire.implicits._
import spire.math._

trait ShortPeriodPeriodicPerturbations {
  
  
  // should be here be Delauney's elements?
  
  def calcUnitVectorsAndCoefs[F: Field: NRoot : Order: Trig](tind: SGP4TimeIndependentFunctions[F], nm: F, xincp: F, cosip: F, sinip: F, am: F, nodep: F, axnl: F, aynl: F, xl : F, eo1: F)
  (implicit wgs : WGSConstants[F])  = {
    
    
    import tind._
  import i0f._
  //import sf._
  import wgs._
  import ocf._
  
     /* ------------- short period preliminary quantities ----------- */
     // these can come from the kepler solver 
     val coseo1 = cos(eo1)
     val sineo1 = sin(eo1)
     val ecose = axnl*coseo1 + aynl*sineo1
     val esine = axnl*sineo1 - aynl*coseo1
     
     
     val el2   = axnl*axnl + aynl*aynl
     val pl    = am*(1 - el2)
     if (pl < 0.as[F]) throw new Exception("pl: " + pl)

     val    rl     = am * (1 - ecose)
     val    rdotl  = sqrt(am) * esine/rl
     val    rvdotl = sqrt(pl) / rl
     val    betal  = sqrt(1 - el2)
     var    temp   = esine / (1 + betal)
     val    sinu   = am / rl * (sineo1 - aynl - axnl * temp)
     val    cosu   = am / rl * (coseo1 - axnl + aynl * temp)
     var    su     = atan2(sinu, cosu)
     val    sin2u  = (cosu + cosu) * sinu
     val    cos2u  = 1 - 2.0 * sinu * sinu
            temp   = 1/ pl
     val    temp1  = 0.5 * J2 * temp
     val    temp2  = temp1 * temp

         /* -------------- update for short period periodics ------------ */
//         if (satrec.method == 'd')
//           {
//             cosisq                 = cosip * cosip
//             satrec.con41  = 3.0*cosisq - 1.0
//             satrec.x1mth2 = 1- cosisq
//             satrec.x7thm1 = 7.0*cosisq - 1.0
//           }
     val    mrt   = rl * (1 - 1.5 * temp2 * betal * con41) +
                 0.5 * temp1 * x1mth2 * cos2u
            su    = su - 0.25 * temp2 * x7thm1 * sin2u
     val    xnode = nodep + 1.5 * temp2 * cosip * sin2u
     val    xinc  = xincp + 1.5 * temp2 * cosip * sinip * cos2u
     val    mvt   = rdotl - nm * temp1 * x1mth2 * sin2u / KE
     val    rvdot = rvdotl + nm * temp1 * (x1mth2 * cos2u + 1.5 * con41) / KE
         // here
     
    val (upos, uvel) = calcUnitVectors(xinc, su, xnode)
  
    // return position and velocity (in km and km/sec)
    convertUnitVectors(upos, uvel, mrt, mvt, rvdot)
  }

 
  def calcUnitVectors[F: Field: NRoot : Order: Trig](xinc: F, su: F, xnode: F) = {

      /* --------------------- orientation vectors ------------------- */
    // TODO: explain transformation
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
    (Vector(ux,uy,uz), Vector(vx,vy,vz))
  }


   /* --------- position and velocity (in km and km/sec) ---------- */
  def convertUnitVectors[F: Field: NRoot : Order: Trig](pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F)
  (implicit wgs : WGSConstants[F]) : (Vector[F], Vector[F]) = {
      import wgs._      
     ( (aE*mrt) *: pos,  vkmpersec *: (mvt *: pos + rvdot *: vel))
  }

}

