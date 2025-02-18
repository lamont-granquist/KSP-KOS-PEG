@LAZYGLOBAL OFF.
@CLOBBERBUILTINS OFF.

function oe2rv {
  parameter
    mu,     // gravitational constant
    sma,    // semi-major axis (m)
    ecc,    // eccentricity
    inc,    // inclination (deg)
    lan,    // longitude of ascending node (deg)
    argp,   // argument of the periapsis (deg)
    tanom,  // true anomaly (deg)
    outpos, // output position vector
    outvel. // output velocity vector

  local l is sma * (1 - ecc * ecc).       // semi-latus rectum
  local rm is l / (1 + ecc * cos(tanom)). // magnitude of the position
  local arglat is argp + tanom.           // argument of the latitude

  local sarglat is sin(arglat).
  local carglat is cos(arglat).

  local c1 is sqrt(mu / l).
  local c2 is ecc * cos(argp) + carglat.
  local c3 is ecc * sin(argp) + sarglat.

  local sinc is sin(inc).
  local cinc is cos(inc).

  local slan is sin(lan).
  local clan is cos(lan).

  set outpos:x to rm * (clan * carglat - slan * cinc * sarglat).
  set outpos:y to rm * sinc * sarglat.
  set outpos:z to rm * (slan * carglat + cinc * sarglat * clan).

  set outvel:x to -c1 * (clan * c3 + slan * cinc * c2).
  set outvel:y to c1 * c2 * sinc.
  set outvel:z to -c1 * (slan * c3 - clan * cinc * c2).
}
