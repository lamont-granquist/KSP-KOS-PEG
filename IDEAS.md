The gravity turn code is not good at precision plane control.  It needs to be able to
do most of the work of plane matching before the coast, so that PEG doesn't have to
do any plane control at apoapsis.  Just trying to follow `peg_heading()` is pretty
bad in stock.  It works better with single-burn-to-orbit in RSS.

That means that it needs to burnout in the requested plane, with a velocity in the
requested plane.  That requires something very much like PEG, and exceeds the hacky
bit of code I used in MJ's stock `HeadingForLaunchInclination()` years ago (which only
targets an inclination and not a specific plane).

So, this would modify the control law to a gravity turn plus yaw steering rate, which
would need to be analytically integrated (likely in some form of quadrature).  The
turning rate calculation of lambdaDot would need to be updated.  Then rp could be
projected onto the plane and vmiss could be calculated.

There might be some halfway point to abuse PEG during gravity turn to integrate the
gravity turn with no yaw rate (or with the yaw rate from the last cycle) then run
PEG with the rdval+vdval+gamma of the prediction along with the desired iy plane and
then kerbal up some conversion from lambdaDot to yaw rate.

