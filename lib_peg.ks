@LAZYGLOBAL OFF.
@CLOBBERBUILTINS OFF.

//
// references:
//
// - Brand1973: https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19740004402.pdf
// - Langston1976: https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19760024151.pdf
// - Mchenry1979: https://ntrs.nasa.gov/search.jsp?R=19790048206 [not available on internet]
// - Jaggers1977: https://arc.aiaa.org/doi/abs/10.2514/6.1977-1051 [not available on internet]
// - Delporte1992: https://arc.aiaa.org/doi/10.2514/6.1992-1145
// - Song2015: https://doi.org/10.1016/j.asr.2014.09.025
// - Yéboles2016: https://oa.upm.es/39534/
// - Porten2018: https://ntrs.nasa.gov/api/citations/20180002035/downloads/20180002035.pdf (NASA SLS PEG enhancements)
//
// additionally:
//
// - Jaggers1974: https://ntrs.nasa.gov/search.jsp?R=19740024190 (RTLS)
// - Jaggers1976: https://ntrs.nasa.gov/search.jsp?R=19760020204 (Alternative Thrust Integrals)
// - Springmann2004: https://core.ac.uk/download/4400510.pdf (Thesis: PEG Lunar Descent)
// - Goodman2005: https://ntrs.nasa.gov/api/citations/20070018243/downloads/20070018243.pdf (Rendezvous)
// - Goodman2009: https://ntrs.nasa.gov/api/citations/20240003182/downloads/Introduction%20To%20Space%20Shuttle%20Rendezvous.pdf (Ascent+Rendezvous)
// - Rea2009: https://core.ac.uk/download/10645877.pdf (Thesis: PEG Lunar Descent)
// - Goodman2011: https://ntrs.nasa.gov/api/citations/20110023479/downloads/20110023479.pdf (Rendezvous/Lambert/Clohessy-Wiltshire)
// - Goddman2011-2: https://ntrs.nasa.gov/api/citations/20110023477/downloads/20110023477.pdf (references to tons of JSC reports, time for some FOIA requests?)
// - Hull2011: https://dl.acm.org/doi/10.1007/s10957-011-9884-5 (Lunar Ascent with PEG-like Algorithm)
// - Kim2012: https://www.icas.org/icas_archive/ICAS2012/PAPERS/611.PDF (PEG for ramjet missiles)
// - Scarritt2015: https://core.ac.uk/download/pdf/42719321.pdf (NASA Orion on-orbit PEG modifications)
// - Fill2018: https://ntrs.nasa.gov/api/citations/20180001863/downloads/20180001863.pdf (NASA Orion on-orbit PEG w/good LTVC info)
// - Fill2018-2: https://ntrs.nasa.gov/api/citations/20180001864/downloads/20180001864.pdf (NASA Orion PEG presentation)
// - Ito2021: https://arc.aiaa.org/doi/10.2514/1.G005577 (PEG modifications for Lunar Descent w/switching function)
// - Wang2022: https://ascelibrary.org/doi/10.1061/%28ASCE%29AS.1943-5525.0001383 (RK4 integrator for PEG descent)
// - Zhang2022: https://doi.org/10.1088/1742-6596/2235/1/012017 (another PEG-ish landing algorithm)
// - Lee2023: https://ieeexplore.ieee.org/document/10178305 (PEG modified for antiballistic missile interceptors)
// - Ma2025: https://www.mdpi.com/2226-4310/12/1/61 (gauss quadrature thrust integrals and enhancement for convergence range)
//
// wish someone would e-mail me these:
//
// - Fill1980: PEG prediction corrections. Shuttle Memo No. 10E-80-11, C.S. Draper Laboratory
// - Fill1989: Introduction to bi-linear tangent steering for shuttle ascent and aborts.
//             Shuttle Memo No. SHUTTLE-89-022, C.S. Draper Laboratory
//

//
// peg_targettype options:
//
//   1: standard PEG ascent 5-orbital conditions with rdval, vdval, gamma, iy.
//   2. PEG ascent 4-orbital conditions with rdval, vdval, gamma, inc.  However, iy must be initialized to a decent guess with the correct inc.
//
// peg_thrustcalc options:
//
//   1. PEG with higher order terms (e.g. Langston1975/McHenry1979).
//   2. PEG without higher order terms. [ simpler, less accurate, eliminates biasing ]
//   3. UPFG style integrals (Brand1973 or kOS PEGAS2). [ more nonlinear, possibly more accurate, seems less stable ]
//

run once lib_util.

// PEG Ascent target conditions
global peg_rdval is 0.     // mode 1,2
global peg_vdval is 0.     // mode 1,2
global peg_gamma is 0.     // mode 1,2
global peg_iy is V(0,0,0). // mode 1,2
global peg_inc is 0.       // mode 2

// Global Options
global peg_targettype is 2.
global peg_thrustcalc is 1.

// Public helpers for alternative PEG target conditions
function peg_set_inc_lan {
    parameter inc.
    parameter lan.

    set peg_iy to V(-sin(lan)*sin(inc), cos(lan)*sin(inc), -cos(inc)).
    set peg_inc to inc.
}

function peg_set_inc_lan_at_r {
    parameter inc.
    parameter rot is 0.

    local rv is obt:position - obt:body:position.
    local vinc is horizontal_velocity_for_inclination(inc, rv, 1.0).

    local hhat is vcrs(rv, vinc):normalized.
    set hhat to rotxy(hhat, rot).

    set peg_iy to swizzle(toinertial(hhat)).
    set peg_inc to inc.
}

function peg_set_sma_ecc_attR {
    parameter sma.
    parameter ecc.
    parameter attR.

    local mu is body:mu.

    local hmag is sqrt(mu * sma * (1 - ecc * ecc)).
    local vmag is sqrt(mu * (2 / attR - 1 / sma)).
    local gamma is safe_arccos(hmag / (attR * vmag)).

    set peg_rdval to attR.
    set peg_vdval to vmag.
    set peg_gamma to gamma.
}

function peg_set_peR_apR_attR {
    parameter peR.
    parameter apR.
    parameter attR.

    if attR < peR { set attR to peR. }
    if apR > peR and attR > apR { set attR to apR. }
    if apR > 0 and apR < peR { set apR to peR. }

    local sma is (peR + apR) / 2.
    local ecc is (apR - peR) / (apR + peR).

    peg_set_sma_ecc_attR(sma, ecc, attR).
}

function peg_set_peA_apA_attA {
    parameter peA.
    parameter apA.
    parameter attA.

    local rad is body:radius.

    peg_set_peR_apR_attR(peA+rad, apA+rad, attA+rad).
}

// output variables
global peg_lambda is V(0,0,0).      // primer vector.
global peg_lambdaDot is V(0,0,0).   // primer rate vector.
global peg_tlambda is missiontime.  // burn centroid time.
global peg_i_F is V(0,0,0).         // lock to this before burns.
global peg_vgo is V(0,0,0).         // FIXME: figure out how to update this

// status
global peg_converged is false.
global peg_terminalGuidance is false.

// peg variables exposed to the update trigger
local lambda is V(0,0,0).
local lambdaDot is V(0,0,0).
local tlambda is 0.
local i_F is V(0,0,0).
local tguid is 0.
local tgo is 0.
local vgo is 0.
local converged is false.
local failed is false.

// private trigger variables to ensure we're reading off guidance only while peg isn't running.
local pegresults is false.
local runpeg is true.

// persistent trigger to update results and kick off next cycle.
when pegresults then {
    set pegresults to false.

    // read results off of last peg call.
    set peg_lambda to lambda.
    set peg_lambdaDot to lambdaDot.
    set peg_tlambda to tlambda.
    set peg_i_F to i_F.
    local peg_tguid is tguid.
    local peg_tgo_at_tguid is tgo.

    // global locks: remember to call these as peg_uf(), peg_tgo(), peg_heading() with parens
    lock peg_uf to frominertial(swizzle(peg_lambda + peg_lambdaDot * (missiontime - peg_tlambda))):normalized.          // lock to this during burns.
    lock peg_tgo to peg_tguid + peg_tgo_at_tguid - missiontime.
    lock peg_heading to mod360(arctan2(vdot(peg_uf(), -vcrs(ship:north:vector, ship:up:vector)), vdot(peg_uf(), ship:north:vector))).

    set peg_converged to converged.
    set runpeg to true.

    return true.
}

function peg_checkargs {
    if peg_targettype < 1 or peg_targettype > 2 {
        this_is_an_error().
    }
    if peg_thrustcalc < 1 or peg_thrustcalc > 3 {
        this_is_an_error().
    }
}

// FIXME: break up this function so people can write their own peg loop.
function peg {
    peg_checkargs().

    // these variables are necessary for sensed velocity calcs
    local vprev is V(0,0,0).
    local vgrav is V(0,0,0).

    // these variables recycle through the loop into calculating rgo
    local rgrav is V(0,0,0).
    local rd is V(0,0,0).
    local rbias is V(0,0,0).
    local tgoprev is 1.

    // vp and rp guess is required for gravity integrals
    local rp is V(0,0,0).
    local vp is V(0,0,0).

    function peg_init {
        local o is ship:orbit.
        local rv to swizzle(toinertial(o:position - o:body:position)).
        local vv to swizzle(toinertial(o:velocity:orbit)).
        set rp to rv.
        set vp to -vv.
        set rd to rv.
        set vprev to -vv.
        set tgo to 1.
        set vgo to -vv.
    }

    peg_init().

    // loop initialization.
    set runpeg to true.
    set pegresults to false.

    until false {
        wait until runpeg.
        set runpeg to false.

        if failed { peg_init(). }

        // update guidance input variables from "sensors".
        local o is ship:orbit.
        local rv to swizzle(toinertial(o:position - o:body:position)).
        local vv to swizzle(toinertial(o:velocity:orbit)).
        local mu to o:body:mu.
        set tguid to missiontime.

        // stage analysis -- this is very KSP specific
        local stages is list().

        // FIXME: need to update for current thrust+isp to support failures
        for stg in peg_stages {
            if ship:mass > stg:endmass
            {
                local vex is constant:g0 * stg:isp.
                local mdot is stg:thrust / vex.
                local startmass is stg:startmass.
                if stg:startmass >= ship:mass {
                    set startmass to ship:mass.
                }
                local bt is ( startmass - stg:endmass ) / mdot.
                local tau is ( startmass ) / mdot.
                local vgos is -vex * ln(1 - bt/tau).
                stages:add(
                    lexicon("vex", vex, "bt", bt, "tau", tau, "vgo", vgos)
                ).
            }
        }

        // update of vgo with "sensed velocity change" [UPDATE]
        // FIXME: add "modified initial guess"
        set vgo to vgo - (vv - vprev).

        if vgo:mag = 0 {
            set vgo to vv:normalized.
        }

        // FIXME: needs better initialization
        // FIXME: do something about clipping lambdadot for short burns
        // FIXME: support coast-to-burn at fixed burn centroid?
        // FIXME: use kepler propagator for coasts?

        local iter is 0.

        until false {
            /////////////////////////////////////
            // calculate tgo from stages [TGO] //
            /////////////////////////////////////

            local vgo1 is 0.
            set tgo to 0.
            for stage in stages {
                local vgoleft is vgo:mag - vgo1.
                if vgoleft > 0 {
                    if stage:vgo < vgoleft {
                        set tgo to tgo + stage:bt.
                        set vgo1 to vgo1 + stage:vgo.
                    } else {
                        local bt is stage:tau*(1.0-constant:e^(-vgoleft/stage:vex)).
                        set tgo to tgo + bt.
                        set vgo1 to vgo:mag.
                    }
                }
            }

            if converged and tgo < 5 {
                return.
            }

            if converged and tgo < 40 { set peg_terminalGuidance to true. }
            if throttle = 0 { set peg_terminalGuidance to false. }

            //////////////////////////////////////////////////
            // stage times and thrust integrals [TGO+INTEG] //
            //////////////////////////////////////////////////

            // FIXME: support constant accel phases.
            // FIXME: support X secs of min/lower Throttle to reduce dispersions.

            // kind of follows Langston(1976)

            local Jt is 0.
            local Lt is 0.
            local St is 0.
            local Qt is 0. // kOS has a global named "Q"
            local Pt is 0.
            local Ht is 0.

            local tgob is 0.

            for stg in stages {
                // FIXME: let the last stage go up to stg:tau and overburn the rocket to converge
                local tgoa is tgob + stg:bt.

                if tgoa > tgo {
                    set tgoa to tgo.
                }

                local bt is tgoa - tgob.

                if bt <= 0 { break. }

                local Li is - stg:vex * ln(1 - bt/stg:tau).
                local Si is - Li * (stg:tau - bt) + stg:vex * bt.
                local Ji is - Si + Li * tgoa.
                local Qi is Si * (stg:tau + tgob) - 0.5 * stg:vex * bt * bt.
                local Pi is Qi * (stg:tau + tgob) - 0.5 * stg:vex * bt * bt * (tgob + bt/3.0).

                local Hi is Ji * tgoa - Qi.
                local Si is Si + Lt * bt.
                local Qi is Qi + Jt * bt.
                local Pi is Pi + Ht * bt.

                set Lt to Lt + Li.
                set St to St + Si.
                set Ht to Ht + Hi.
                set Jt to Jt + Ji.
                set Qt to Qt + Qi.
                set Pt to Pt + Pi.

                set tgob to tgoa.
            }

            local K is 0.
            if Lt <> 0 {
                set K to Jt / Lt.
            }
            local Qp is Qt - St * K.

            set lambda to vgo:normalized.

            ///////////////////////////////////////////////////
            // turning rate calculation of lambdaDot [TURNR] //
            ///////////////////////////////////////////////////

            // follows Jaggers 1977

            set rgrav to tgo * tgo / ( tgoprev * tgoprev ) * rgrav.
            set vgrav to tgo / tgoprev * vgrav. // FIXME: Song(2015)

            local rgo is rd - ( rv + vv * tgo + rgrav ) + rbias.

            if not peg_terminalGuidance {
                if peg_targettype = 2 { // from Jaggers1977
                    local ip is vcrs(lambda, peg_iy):normalized.
                    local A1 is (rgo - vdot(lambda, rgo) * lambda) * ip.
                    set rgo to St * lambda + A1 * ip.
                } else {
                    set rgo to rgo + ( St - vdot(lambda, rgo) ) * lambda.  // enforce orthogonality (converges to rgo = rgo)
                }
                set lambdaDot to ( rgo - St * lambda ) / Qp.           // turn rate
            }

            local ldm is lambdaDot:mag.

            // large angle clamp for the thrust calculation in the predictor

            local phiMax is 45.0 * constant:degtorad.
            if peg_thrustcalc = 3 { set phiMax to 90.0 * constant:degtorad. }

            if ldm > phiMax / K {
                set ldm to phiMax / K.
                set lambdaDot to lambdaDot:normalized * ldm.
                set rgo to St * lambda + Qp * lambdaDot.
            }

            set tlambda to tguid + K.

            set i_F to (lambda - K * lambdaDot):normalized.

            /////////////////////////////////////////////////////
            // calculate rthrust/vthrust and rbias [PREDICTOR] //
            /////////////////////////////////////////////////////

            local vthrust is 0.
            local rthrust is 0.

            if peg_thrustcalc = 1 {
                set vthrust to lambda * ( Lt - ldm * ldm * ( Ht - Jt * K ) / 2.0 ).
                set rthrust to lambda * ( St - ldm * ldm * ( Pt - 2.0 * Qt * K + St * K * K ) / 2.0 ) + Qp * lambdaDot.
            } else if peg_thrustcalc = 2 {
                set vthrust to Lt * lambda.
                set rthrust to St * lambda + Qp * lambdaDot.
            } else if peg_thrustcalc = 3 {
                local phi is K * ldm.
                local phidot is - phi * Lt / Jt.
                set vthrust to ( Lt - Lt * phi * phi / 2.0 - Jt * phi * phidot - Ht * phidot * phidot / 2.0 ) * lambda.
                set rthrust to ( St - St * phi * phi / 2.0 - Qt * phi * phidot - Pt * phidot * phidot / 2.0 ) * lambda - ( St * phi + Qt * phidot ) * lambdaDot:normalized.
            }

            set rbias to rgo - rthrust.

            // rgrav/vgrav calculation from Delporte and Sauvient(1992) [NB: several typos].

            local graviter = 0.

            set rp to rv + vv*tgo + rthrust + rgrav.
            set vp to vv + vthrust + vgrav.

            until false {

                local rm is rv:mag.
                local rpm is rp:mag.

                // FIXME: include J2 in g0/gf for Principia
                local g0 is -mu*rv/rm^3.
                local gf is -mu*rp/rpm^3.
                local g0dot is -mu*vv/rm^3 + 3*mu/rm^5*vdot(rv, vv)*rv.
                local gfdot is -mu*vp/rpm^3 + 3*mu/rpm^5*vdot(rp, vp)*rp.

                set vgrav to tgo*(gf + g0)/2.0 + tgo^2*(g0dot - gfdot)/12.0.
                set rgrav to tgo^2*(3.0*gf + 7.0*g0)/20.0 + tgo^3*(3.0*g0dot - 2.0*gfdot)/60.0.

                local rp1 is rp.
                local vp1 is vp.

                set rp to rv + vv*tgo + rthrust + rgrav.
                set vp to vv + vthrust + vgrav.

                print((rp - rp1):mag).

                if (rp - rp1):mag < 0.001*rgo:mag { break. }

                set graviter to graviter + 1.

                if graviter > 20 {
                    set failed to true.
                    break.
                }
            }

            //////////////////////////////////////////////////
            // Update rd and vgo for next cycle [CORRECTOR] //
            //////////////////////////////////////////////////

            // FIXME: add releasing plane control
            // FIXME: add Lambert targetting
            set rd to peg_rdval * (rp - vdot(rp, peg_iy) * peg_iy):normalized.

            if peg_terminalGuidance {
                set rd to rp.
            }

            local ix is rd:normalized.
            local iz is vcrs(ix, peg_iy).
            local vd to peg_vdval * (sin(peg_gamma)*ix + cos(peg_gamma)*iz).

            if peg_targettype = 2 { // Jaggers1977
                local n is V(0,0,1).
                local SE to -0.5*(vdot(n,peg_iy) + cos(peg_inc))*vdot(n,iz)/(1-vdot(n,ix)^2). // converges to zero
                set peg_iy to (peg_iy*sqrt(1 - SE^2) + SE*iz):normalized.
            }

            local vmiss is vd - vp.

            set vgo to vgo + 0.5 * vmiss. // 0.5 here stops chattering (NASA SLS fixes to PEG)

            set converged to vmiss:mag < 0.01 * vgo:mag.

            set tgoprev to tgo. // update rgrav for next loop

            if converged { break. }

            set iter to iter + 1.

            if iter > 20 {
                set failed to true.
                break.
            }
        }

        if failed {
            print("FAILED").
        } else {
            print("CONVERGED").
        }
        set vprev to vv. // update vgo guess for next guidance cycle

        if not failed {
            set pegresults to true.
        } else {
            set runpeg to true.
        }
    }
}
