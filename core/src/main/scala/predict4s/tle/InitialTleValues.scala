package predict4s.tle {

import spire.algebra.{Trig, NRoot}
import spire.algebra.Field
import spire.math.pi
import spire.implicits._



object SGPElements {
 
  def apply[F: Field: NRoot: Trig](tle: TLE) :  TEME.SGPElements[F] = { 
    val e0 = tle.eccentricity.toDouble.as[F]
    val i0 = tle.inclination.toDouble.toRadians.as[F]
    val pa = tle.argumentOfPeriapsis.toDouble.toRadians.as[F]
    val raan = tle.rightAscension.toDouble.toRadians.as[F]
    val meanAnomaly =  tle.meanAnomaly.toDouble.toRadians.as[F]
    def meanMotion = tle.meanMotion.toDouble.as[F]
    val refepoch = tle.epoch.toDouble.as[F]
    val year = tle.year
    val bStar = tle.atmosphericDragCoeficient.toDouble.as[F]
  
    // in days
    val epoch = tle.epoch
    val ω0 = pa
    val Ω0 = raan
    val M0 = meanAnomaly    
    val radpm0 = revPerDay2RadPerMin(meanMotion)

    TEME.SGPElements[F](radpm0,e0,i0,pa,raan,M0,bStar,refepoch)
  }

  def revPerDay2RadPerMin[F: Field: NRoot: Trig](rpd: F) : F = 2 * pi * rpd / 1440 

  // FIXME: calculate the correct epoch date
}

}
/* -----------------------------------------------------------------------------
*
*                           procedure days2mdhms
*
*  this procedure converts the day of the year, days, to the equivalent month
*    day, hour, minute and second.
*
*  algorithm     : set up array for the number of days per month
*                  find leap year - use 1900 because 2000 is a leap year
*                  loop through a temp value while the value is < the days
*                  perform int conversions to the correct day and month
*                  convert remainder into h m s using type conversions
*
*  author        : david vallado                  719-573-2600    1 mar 2001
*
*  inputs          description                    range / units
*    year        - year                           1900 .. 2100
*    days        - julian day of the year         0.0  .. 366.0
*
*  outputs       :
*    mon         - month                          1 .. 12
*    day         - day                            1 .. 28,29,30,31
*    hr          - hour                           0 .. 23
*    min         - minute                         0 .. 59
*    sec         - second                         0.0 .. 59.999
*
*  locals        :
*    dayofyr     - day of year
*    temp        - temporary extended values
*    inttemp     - temporary int value
*    i           - index
*    lmonth[12]  - int array containing the number of days per month
*
*  coupling      :
*    none.
* --------------------------------------------------------------------------- 

void    days2mdhms
        (
          int year, double days,
          int& mon, int& day, int& hr, int& minute, double& sec
        )
   {
     int i, inttemp, dayofyr;
     double    temp;
     int lmonth[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

     dayofyr = (int)floor(days);
     /* ----------------- find month and day of month ---------------- */
     if ( (year % 4) == 0 )
       lmonth[1] = 29;

     i = 1;
     inttemp = 0;
     while ((dayofyr > inttemp + lmonth[i-1]) && (i < 12))
     {
       inttemp = inttemp + lmonth[i-1];
       i++;
     }
     mon = i;
     day = dayofyr - inttemp;

     /* ----------------- find hours minutes and seconds ------------- */
     temp = (days - dayofyr) * 24.0;
     hr   = (int)floor(temp);
     temp = (temp - hr) * 60.0;
     minute  = (int)floor(temp);
     sec  = (temp - minute) * 60.0;
   }  // end days2mdhms

/ -----------------------------------------------------------------------------
*
*                           procedure jday
*
*  this procedure finds the julian date given the year, month, day, and time.
*    the julian date is defined by each elapsed day since noon, jan 1, 4713 bc.
*
*  algorithm     : calculate the answer in one step for efficiency
*
*  author        : david vallado                  719-573-2600    1 mar 2001
*
*  inputs          description                    range / units
*    year        - year                           1900 .. 2100
*    mon         - month                          1 .. 12
*    day         - day                            1 .. 28,29,30,31
*    hr          - universal time hour            0 .. 23
*    min         - universal time min             0 .. 59
*    sec         - universal time sec             0.0 .. 59.999
*
*  outputs       :
*    jd          - julian date                    days from 4713 bc
*
*  locals        :
*    none.
*
*  coupling      :
*    none.
*
*  references    :
*    vallado       2007, 189, alg 14, ex 3-14
*
* --------------------------------------------------------------------------- 

void    jday
        (
          int year, int mon, int day, int hr, int minute, double sec,
          double& jd
        )
   {
     jd = 367.0 * year -
          floor((7 * (year + floor((mon + 9) / 12.0))) * 0.25) +
          floor( 275 * mon / 9.0 ) +
          day + 1721013.5 +
          ((sec / 60.0 + minute) / 60.0 + hr) / 24.0;  // ut in days
          // - 0.5*sgn(100.0*year + mon - 190002.5) + 0.5;
   }  // end jday

*/