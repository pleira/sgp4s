package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import predict4s.tle.TEME.SGPElements

trait SGP4LongPeriodicEffects {
  
  def calculateSGP4LongPeriodicEffects[F: Field: NRoot : Order: Trig](tif: SGP4TimeIndependentFunctions[F], el: TEME.SGPElements[F], am: F) = {
       /* ----------------- compute extra mean quantities ------------- */
    import el._
    val sinim = sin(i0) //  sin(inclm);
    val cosim = cos(i0)

//     ep     = em;
//     xincp  = inclm;
//     argpp  = argpm;
//     nodep  = nodem;
//     mp     = mm;
//     sinip  = sinim;
//     cosip  = cosim;
    
    // p for periodics
    // inputs are the mean elements
     val ep     = e0 
     val xincp  = i0 
     val argpp  = ω0 
     val nodep  = Ω0 
     val mp     = M0 
     val sinip  = sinim 
     val cosip  = cosim 
     
     val axnl = ep * cos(argpp)
     val temp = 1 / (am * (1 - ep * ep))
     val aynl = ep * sin(argpp) + temp * tif.ocf.aycof
     val xl   = mp + argpp + nodep + temp * tif.ocf.xlcof * axnl

     // Are these variables in relation with Delauney's? 
     (nodep, axnl, aynl, xl)
  }
}

object SGP4LongPeriodicEffects extends SGP4LongPeriodicEffects