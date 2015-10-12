package predict4s.tle

import org.scalautils.TolerantNumerics
import collection.immutable.HashMap
import spire.math._
import spire.implicits.{eqOps => _, _}


class InitlTestSpec extends TLE_Base {
  
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(1E-8)
  
  implicit val wgs = WGS72Constants.tleDoubleConstants
  
  val resultsIni = HashMap (
   5 ->    (0.185966700,0.598092919,5.790416027,6.086385471,0.337309313,0.000028098,0.047229443),
   4632 -> (0.145050600,0.200063601,3.623303527,4.766670465,2.512139659,0.000100000,0.005246110),
   6251 -> (0.003003500,1.013301512,2.428744337,0.943219561,3.860413487,0.000128080,0.067910210)
  )

  val resultsTimeIndep = HashMap (
      5 ->    (0.826410932,0.047206302,1.353899821, 1.708450473,1.102119539, 1.048865088,0.0,0.000000526,0.000016465,0.0,0.0,0.0),
      4632 -> (0.980053940,0.005245869,5.857445274,32.881124410,5.007819322, 1.881517177,0.0,0.0,0.0,0.0,0.0,0.0),
      6251 -> (0.529062002,0.067918037,1.062338933, 1.128543648,1.059148199,-0.160280193,0.000000003,0.000005200,0.000650194,0.0,0.0,0.0)
   )
   
  val resultsSecularEffects = HashMap (
      5 ->    (0.000054293, -0.000037171, 0.047229443),
      4632 -> (0.000000493, -0.000000254, 0.005246110),
      6251 -> (0.000019407, -0.000051677, 0.067910210)
   )
 
   /*
   satn               5            yr            ecco     0.185966700 epoch 18441.784950620 inclo     0.598092919
    in/out : 
    no     0.047206302
    outputs : 
method               n  ainv     0.738607085    ao     1.353899821 con41     1.048865088 con42    -2.414775147 cosio     0.826410932
cosio2     0.682955029
 einx??    0.034583614 eccsq     0.034583614 omeosq    0.965416386  posq     1.708450473    rp     1.102119539 rteosq    0.982556048
 sinio     0.563067465  gsto     3.469172342
                                     ------------------after sgp4   :---------------
    inputs :        method   110 aycof     0.000660216 bstar     0.000028098 con41     1.048865088   cc1     0.000000000
   cc4     0.000000526   cc5     0.000016465    d2     0.000000000    d3     0.000000000    d4     0.000000000 delmo     4.873084659
  ecco     0.185966700   eta     0.736909543 argpo     5.790416027argpdot    0.000054293 omgcof    0.000000000 sinmao    0.330949230
     t     0.000000000 t2cof     0.000000000 t3cof     0.000000000 t4cof     0.000000000 t5cof     0.000000000 x1mth2    0.317044971
 x7thm1    3.780685205 inclo     0.598092919    mo     0.337309313  mdot     0.047229443   xno     0.047206302 nodeo     6.086385471
nodedt    -0.000037171 xlcof     0.001289058 xmcof    -0.000000000 nodecf   -0.000000000
    outputs : 
 error               0     x  7022.465292664     y -1400.082967554     z     0.039951554  xdot     1.893841015  ydot     6.405893759  zdot     4.534807250
    
 isimp               
 
   satn            4632            yr            ecco     0.145050600 epoch 19754.910709590 inclo     0.200063601
    in/out : 
    no     0.005245869p
    outputs : 
method               n  ainv     0.170722893    ao     5.857445274 con41     1.881517177 con42    -3.802528628 cosio     0.980053940
cosio2     0.960505726
 einx??    0.021039677 eccsq     0.021039677 omeosq    0.978960323  posq    32.881124410    rp     5.007819322 rteosq    0.989424238
 sinio     0.198731664  gsto     1.716027084
                                      ------------------after sgp4   :---------------
    inputs : 
 isimp               method             100 aycof     0.000232797 bstar     0.000100000 con41     1.881743507   cc1     0.000000000
   cc4     0.000000000   cc5     0.000000000    d2     0.000000000    d3     0.000000000    d4     0.000000000 delmo     0.632187620
  ecco     0.145050600   eta     0.175353576 argpo     3.623303527argpdot    0.000000493 omgcof   -0.000000000 sinmao    0.588702675
     t     0.000000000 t2cof     0.000000000 t3cof     0.000000000 t4cof     0.000000000 t5cof     0.000000000 x1mth2    0.039418831
 x7thm1    5.724068183 inclo     0.200063601    mo     2.512139659  mdot     0.005246110   xno     0.005245869 nodeo     4.766670465
nodedt    -0.000000254 xlcof     0.000464424 xmcof    -0.000000000 nodecf   -0.000000000
    outputs : 
 error               0     x  2334.114500848     y -41920.440353490     z    -0.038674374  xdot     2.826321032  ydot    -0.065091664  zdot     0.570936053
 
                                       ------------------after initl  :---------------
    inputs : 
  satn            6251            yr            ecco     0.003003500 epoch 20630.824120140 inclo     1.013301512
    in/out : 
    no     0.067918037
    outputs : 
method               n  ainv     0.941319167    ao     1.062338933 con41    -0.160280193 con42    -0.399533012 cosio     0.529062002
cosio2     0.279906602
 einx??    0.000009021 eccsq     0.000009021 omeosq    0.999990979  posq     1.128543648    rp     1.059148199 rteosq    0.999995489
 sinio     0.848583171  gsto     3.673754968
                                     ------------------after sgp4   :---------------
    inputs : 
 isimp                method 110 aycof     0.000994993 bstar     0.000128080 con41    -0.160280193   cc1     0.000000003
   cc4     0.000005200   cc5     0.000650194    d2     0.000000000    d3     0.000000000    d4     0.000000000 delmo     0.863016906
  ecco     0.003003500   eta     0.063675056 argpo     2.428744337argpdot    0.000019407 omgcof   -0.000000052 sinmao   -0.658497710
     t     0.000000000 t2cof     0.000000004 t3cof     0.000000000 t4cof     0.000000000 t5cof     0.000000000 x1mth2    0.720093398
 x7thm1    0.959346217 inclo     1.013301512    mo     3.860413487  mdot     0.067910210   xno     0.067918037 nodeo     0.943219561
nodedt    -0.000051677 xlcof     0.001836762 xmcof    -0.000133147 nodecf   -0.000000000
    outputs : 
 error               0     x  3988.310226994     y  5498.966572352     z     0.900558787  xdot    -3.290032738  ydot     2.357652820  zdot     6.496623475
 */
  test("structure all") ({
    assert(tles.size == 32 && rmap.size == 32)
    
    def checkIni(satelliteNumber : Int, ini: TEME.SGPElements[Double]) : Unit = {
      val r = resultsIni.get(satelliteNumber).get
      import ini._
      assert(e0 === r._1)  //ecco
      assert(i0 === r._2)  //inclo
      assert(ω0 === r._3)  //argpo
      assert(Ω0 === r._4)  //nodeo
      assert(M0 === r._5)  //mo      
      assert(bStar === r._6) //bstar
 //     assert(radpm0 === r._7) ?
    }
    
    def checkTimeIndepCoeficients(satelliteNumber : Int, tIndep: SGP4TimeIndepFunctions[Double]) : Unit = {
      val r = resultsTimeIndep.get(satelliteNumber).get
      import tIndep._
  //      import wgs._

   
      assert(θ === r._1) // cosio
      assert(n0 === r._2) // n0 but after calculation of n0dp 
      assert(a0 === r._3) // a0 but after calculation of a0dp
      assert(posq === r._4) // 
      assert(rp === r._5) // radius of perige
      assert(x3thm1 === r._6) // con41 
      assert(C1 === r._7) 
      assert(C4 === r._8)
      assert(C5 === r._9)
      assert(D2 === r._10)
      assert(D3 === r._11)
      assert(D4 === r._12)
    }
    
    def checkTimeIndepSecularEffects(satelliteNumber : Int, tIndep: SGP4TimeIndepFunctions[Double]) : Unit = {
      val r = resultsSecularEffects.get(satelliteNumber).get
      import tIndep._
      
      assert(ωdot === r._1)  //argpdot
      assert(Ωdot === r._2)  //nodedot
      assert(mdot === r._3)  //mdot

    }


    var j = 0
    for (j <- 0 to 2) {
      val tle = tles(j)
      val ini = TEME.SGPElements[Double](tle)
      val tIndep = SGP4TimeIndepFunctions[Double](ini)
      checkIni(tle.satelliteNumber, ini)
      checkTimeIndepCoeficients(tle.satelliteNumber, tIndep)   
      checkTimeIndepSecularEffects(tle.satelliteNumber, tIndep)   
    }

  })
	      
  test("00005U") {  
//    val mmtopv5 = map.get(5)
//    val (px, py, pz, vx, vy, vz) = mmtopv5.get(0)
//    assert(px == 7022.46529057)
//    assert(py == -1400.08296714)
//    assert(pz == 0.03995155)
//    assert(vx == 1.893841014)
//    assert(vy == 6.405893757)
//    assert(vz == 4.534807249)
    
    val tle = tles(0)
    assert(tle.satelliteNumber == 5)
    val state0 = SGP4TimeIndepFunctions[Double](TEME.SGPElements[Double](tle))
    // assert(state0.a0 === 1.353899821)
    // assert(state0.a0dp === )
    
//    assert(state0.ini.epoch === 18441.784950620)
//    assert(state0.ini.ω0 === 5.790416027)  // argpo
//    assert(state0.ini.Ω0 === 6.086385471)  // node0
//    assert(state0.ini.M0 === 0.337309313)
    //assert(state0.ωdot === 0.0)  //2.886976017128261E8
    //assert(state0.Ωdot === 0.0)  //-6.10648416934914E7 
    
    // time independent outputs
//    assert(state0.θ === 0.826410932)
    
    // to check
//    assert(state0.a1 === 1.3534574827552335) 
//    assert(state0.tval === 5.726338842986412) 
//    assert(state0.δ1 === 3.125991608605109)
//    assert(state0.a2 === -81.67813506826018) 
//    assert(state0.δ0 === 8.583519866830798E-4)
//    
//    assert(state0.n0dp === 0.047188940719762276)
//    // ?assert(state0.a2 === 1.3534574827552335)
//    assert(state0.x3thm1 === 1.048865088)  // con41
//    assert(state0.β0sq === 0.965416386)  // omeosq
//    assert(state0.D3 === 2.5836394056164486E70)
//    assert(state0.D2 === 2.7051121169261756E46) 
    //?assert(state0.q0 === 1.018814277214264)
    //assert(state0.δ1 === 0.0) // 3.125991608605109
    
//    assert(state0.ini. === )

    // assert(state0.ini.epoch === 18441.784950620) here, it uses days 
  }
  
//  test("28375U") {
//    val duration   = 2341.4889653027058 // in minutes
//    val initialState = new SGP4TimeIndepFunctions[Double](InitialTleValues(tle), WGS72Constants.tleDoubleConstants)
//    val ti = new SGP4TimeIndepFunctions[Double](initialState)
//    val finalState = initialState.propagate(duration)
//   
//    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(1E-9)
//
//    assert(finalState.a === 1.118136309343834, " a ")
//    assert(finalState.e === 0.008415889590488213, " e ") // 0.008415889602669875?
//    assert(finalState.i === 1.7113843433722917, " i ") // 1.7113843433722922	?
//    assert(finalState.ω === 5.425745815821618, " perigee ")
//    assert(finalState.Ω === 2.102076054068816, " raan ") // 2.07534657893693?
//    assert(finalState.ν === 155.4748369659863, " true anomaly ")  // 155.47483697687704?
//  }
  
}