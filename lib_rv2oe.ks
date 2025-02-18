@LAZYGLOBAL OFF.
@CLOBBERBUILTINS OFF.

run once lib_util.

// r, v in BCI non-rotating left-handed coordinates
function rv2oe {
  parameter mu.
  parameter rv.
  parameter vv.

  // convert to right handed.
  set rv to swizzle(rv).
  set vv to swizzle(vv).

  set ret to lexicon().

  local rmag is rv:mag.                             // Magnitude of position vector
  local vmag is vv:mag.                             // Magnitude of velocity vector
  local rhat is rv:normalized.                      // Unit vector in the direction of r
  local hv is vcrs(rv, vv).                         // Specific angular momentum vector
  local hhat is hv:normalized.                      // Unit vector of angular momentum
  local eccvec is vcrs(vv / mu, hv) - rhat.         // Eccentricity vector
  ret:add("sma", 1.0 / (2.0 / rmag - vmag^2 / mu)). // Semi-major axis
  local l is hv:mag / mu.                           // Semi-latus rectum

  // Parameters for frame transformation
  local d is 1.0 + hhat:z.
  local pc is 0.
  local qc is 0.
  if (d <> 0) {
    set pc to hhat:x / d.
    set qc to -hhat:y / d.
  }
  local const1 is 1.0 / (1.0 + pc^2 + qc^2).

  local fhat is V(
    const1 * (1.0 - pc^2 + qc^2),
    const1 * 2.0 * pc * qc,
    -const1 * 2.0 * pc
  ).

  local ghat is V(
    const1 * 2.0 * pc * qc,
    const1 * (1.0 + pc^2 - qc^2),
    const1 * 2.0 * qc
  ).

  local eps to 2.22e-16.

  // Calculate Keplerian elements
  local h is vdot(eccvec, ghat).
  local xk is vdot(eccvec, fhat).
  local x1 is vdot(rv, fhat).
  local y1 is vdot(rv, ghat).
  local xlambdot is arctan2(y1, x1).               // True longitude
  ret:add("ecc", sqrt(h^2 + xk^2)).                // Eccentricity
  ret:add("inc", 2.0 * arctan(sqrt(pc^2 + qc^2))). // Inclination
  local lan is arctan2(pc, qc).                    // Longitude of ascending node
  if ret["inc"] < eps { set lan to 0.0. }
  local argp is arctan2(h, xk) - lan.              // Longitude of ascending node
  if ret["ecc"] < eps { set argp to 0.0. }
  local nu is xlambdot - lan - argp.               // True anomaly

  // Normalize angles to [0, 360)
  ret:add("lan", mod360(lan)).
  ret:add("argp", mod360(argp)).
  ret:add("nu", mod360(nu)).

  return ret.
}
