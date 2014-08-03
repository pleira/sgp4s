package predict4s.tle

object TLEConstants {

  val ONE_THIRD = 1.0 / 3.0
  val TWO_THIRD = 2.0 / 3.0

  /** Earth radius in km. */
  val EARTH_RADIUS = 6378.135

  /** Equatorial radius rescaled (1.0). */
  val NORMALIZED_EQUATORIAL_RADIUS = 1.0

  /** Time units per julian day. */
  val MINUTES_PER_DAY = 1440.0

  // Potential perturbation coefficients
  val KE    =  0.0743669161331734132 // mu = 3.986008e+14;
  val J3    = -2.53881e-6
  val J2    =  1.082616e-3
  val J4    = -1.65597e-6
  val CK2   =  0.5 * J2 * NORMALIZED_EQUATORIAL_RADIUS * NORMALIZED_EQUATORIAL_RADIUS
  val CK4   = -0.375 * J4 * NORMALIZED_EQUATORIAL_RADIUS * NORMALIZED_EQUATORIAL_RADIUS *
                    NORMALIZED_EQUATORIAL_RADIUS * NORMALIZED_EQUATORIAL_RADIUS
  val S      = NORMALIZED_EQUATORIAL_RADIUS * (1.0 + 78.0 / EARTH_RADIUS)
  val QOMS2T = 1.880279159015270643865e-9
  val A3OVK2 = -J3 / CK2 * NORMALIZED_EQUATORIAL_RADIUS * NORMALIZED_EQUATORIAL_RADIUS *
                    NORMALIZED_EQUATORIAL_RADIUS

  /** Earth gravity coefficient in m<sup>3</sup>/s<sup>2</sup>. */
  val MU = KE * KE * EARTH_RADIUS * EARTH_RADIUS * EARTH_RADIUS * (1000 * 1000 * 1000) / (60 * 60)
    
}
