package predict4s.tle

import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSuite

class TLE_Base extends FunSuite  with BeforeAndAfterAll {
   
  // for implicits
  object ddd extends spire.std.DoubleInstances
  
//  final val tle : TLE = 
//        TLE("1 00005U 58002B   00179.78495062  .00000023  00000-0  28098-4 0  4753",
//            "2 00005  34.2682 348.7242 1859667 331.7664  19.3264 10.82419157413667"); 

  val tles = TLE.parseFile("/SGP4-VER.TLE")
  assert(tles.length > 0)
  val rmap = tleResults("/results_all_wgs72.out")
  
  def tleResults(path : String) = {
    val results = io.Source.fromInputStream(getClass.getResourceAsStream(path))
    //assert(input.length() > 0)
    val iter1 = results.getLines()
    type MapMinToPosVel =  scala.collection.mutable.Map[Int, (Double,Double,Double,Double,Double,Double)]
    type Sat = Int
    var map = scala.collection.mutable.Map[Sat, MapMinToPosVel]()
    //var sat : Sat = 0
    var mmtopv : MapMinToPosVel = null // scala.collection.mutable.Map[Int, (Double,Double,Double,Double,Double,Double)]
    while (iter1.hasNext) {
      val line = iter1.next()
        val tokens = line.split("\\s+")    
      if (!line.startsWith(" ")) {
        val sat = tokens(0).toInt
        val tmp = scala.collection.mutable.Map[Int, (Double,Double,Double,Double,Double,Double)]()
        map.put(sat, tmp)
        mmtopv = tmp
      } else {
        // min, (px,py,pz, vx, vy, vz)
        try {
          val min = tokens(1).toDouble.toInt
          mmtopv.put(min, 
             (tokens(2).toDouble,tokens(3).toDouble,tokens(4).toDouble,tokens(5).toDouble,tokens(6).toDouble, tokens(7).toDouble))
        } catch {
          case  _  =>  println(line)
        }
      }
    }
    results.close()
    map
  }

  
}