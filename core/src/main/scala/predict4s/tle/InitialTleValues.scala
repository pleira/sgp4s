package predict4s.tle {

import spire.algebra.Trig
import spire.algebra.Field

case class InitialTleValues[F](tle: TLE)(implicit ev: Trig[F], f: Field[F]) {
  val e0 = tle.eccentricity.toDouble.as[F]
  val i = ev.toRadians(tle.inclination.toDouble.as[F])
  val pa =  ev.toRadians(tle.argumentOfPeriapsis.toDouble.as[F])
  val raan = ev.toRadians(tle.rightAscension.toDouble.as[F])
  val meanAnomaly =  ev.toRadians(tle.meanAnomaly.toDouble.as[F])
  val meanMotion = tle.meanMotion.toDouble.as[F]
  val refepoch = tle.epoch.toDouble.as[F]
  val year = tle.year
  val bStar = tle.atmosphericDragCoeficient.toDouble.as[F]
  
  // in days 
  val epoch = (1000 * tle.year + tle.epoch.toDouble).as[F]

}

}