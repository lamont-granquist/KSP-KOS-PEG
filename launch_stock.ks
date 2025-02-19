@LAZYGLOBAL OFF.
@CLOBBERBUILTINS OFF.

run once lib_oe2rv.
run once lib_rv2oe.
run once lib_util.
run once lib_peg.

local tacc is 15.       // time of end of vertical rise, higher is less aggressive.
local tpo is tacc + 10. // time to end of initiation.
local tc is 3.          // time constant for decay from initiation to gravity turn.
local dtheta is 2.0.    // angular deflection to initiate at tpo, higher is more aggressive.

local tgt_per is 100000.

// PEG target conditions
set peg_targettype to 2.
peg_set_peA_apA_attA(tgt_per, 200000, 0).
peg_set_inc_lan_at_r(30).

// This is the kOS MyFirstRocket craft with an additional Adv Inline Stabilizer on the upper stage for more RW control.
//
// mass is t, thrust is kN, isp is sec, stages are reverse order from ksp (bottom up if you're looking at MJ)
// ISP numbers for RO engines should come from only MJ's deltaV analyzer and not the PAW or KER
// (MJ gets nonISP fuel correct).
local stageone to lexicon("startmass", 13.972, "endmass", 5.972, "thrust", 215, "isp", 320).
local stagetwo to lexicon("startmass", 3.12, "endmass", 1.120, "thrust", 60, "isp", 345).
global peg_stages to list(stageone, stagetwo).

clearscreen.

function enable_autostaging {
    when maxthrust = 0 then {
        print "staging".
        stage.
        return true.
    }
}

function set_start_launch_trigger {
    when peg_converged then {
        set_switch_to_vertical_rise_trigger().
    }
}

function set_switch_to_vertical_rise_trigger {
    print "vertical rise".

    lock throttle to 1.0.

    enable_autostaging().

    lock steering to ship:facing.

    when ship:altitude > 120 then { // FIXME: altitude of launchpad fix
        lock steering to heading(peg_heading(),89.99999).
    }

    set_switch_to_pitchover_trigger().
}

function set_switch_to_pitchover_trigger {
    when missiontime > tacc then {
        print "initiating pitchover".
        local t is missiontime.

        lock fpa to 90 - vang(ship:up:vector, ship:velocity:surface).
        lock pitch to fpa - (t - tacc) / (tpo - tacc) * dtheta.
        lock steering to heading(peg_heading(), pitch).

        set_switch_to_gravity_turn_trigger().
    }
}

function set_switch_to_gravity_turn_trigger {
    when missiontime > tpo then {
        print "gravity turn".
        local t is missiontime.

        lock pitch to fpa - dtheta * constant:e^(-(t-tpo)/tc).

        set kuniverse:timewarp:mode to "PHYSICS".
        set kuniverse:timewarp:rate to 4.

        set_switch_to_coasting_trigger().
    }
}

function set_switch_to_coasting_trigger {
    when ship:apoapsis > tgt_per then {
        print "coasting to insertion".

        lock throttle to 0.
        lock steering to ship:velocity:surface.

        // for some reason my vessel gets the wobblies at high physwarp on coast initiation.
        set kuniverse:timewarp:rate to 0.

        local resumewarp to missiontime + 3.

        when missiontime > resumewarp then {
            set kuniverse:timewarp:rate to 4.
        }

        when ship:altitude > ship:obt:body:atm:height then {
            lock steering to frominertial(swizzle(peg_i_F)). // FIXME: peg_i_F should be in rotating alice-world
        }

        when ship:orbit:eta:apoapsis < 0.5*peg_tgo() + 10 then {
            set kuniverse:timewarp:rate to 0.
        }

        set_switch_to_insertion_trigger().
    }
}

function set_switch_to_insertion_trigger {
    when ship:orbit:eta:apoapsis < 0.5*peg_tgo() then {
        print "PEG insertion burn".

        lock steering to peg_uf().
        lock throttle to 1.

        // FIXME: need to have better cutoff based on reaching angular momentum target
        when peg_tgo() <= 0.02 then {
            lock throttle to 0.
        }
    }
}

set_start_launch_trigger().

when peg_converged then {
    //print(peg_tgo()).
    //print(peg_uf()).
    //print(peg_heading()).

    return true.
}

peg().

wait until false.
