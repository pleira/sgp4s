package predict4s.tle {

import spire.algebra.{Trig, NRoot}
import spire.algebra.Field
import spire.math.pi
import spire.implicits._

case class InitialTleValues[F: Field: Trig](tle: TLE) {
  val e0 = tle.eccentricity.toDouble.as[F]
  val i = tle.inclination.toDouble.toRadians.toRadians.as[F]
  val pa = tle.argumentOfPeriapsis.toDouble.toRadians.as[F]
  val raan = tle.rightAscension.toDouble.toRadians.as[F]
  val meanAnomaly =  tle.meanAnomaly.toDouble.toRadians.as[F]
  def meanMotion = tle.meanMotion.toDouble.as[F]
  val refepoch = tle.epoch.toDouble.as[F]
  val year = tle.year
  val bStar = tle.atmosphericDragCoeficient.toDouble.as[F]

  // in days
  val epoch = (1000 * tle.year + tle.epoch.toDouble).as[F]

  // follow Hoots notation here
  // Unicode
  val i0 = i
  //val n0 = meanMotion
  val t0 = epoch
  val ω0 = pa
  val Ω0 = raan
  val M0 = meanAnomaly
  
  def revPerDay2RadPerMin(rpd: F) : F = 2 * pi * rpd / 1440 
   
  val n0 = revPerDay2RadPerMin(meanMotion)
  val xno = n0
}

}
