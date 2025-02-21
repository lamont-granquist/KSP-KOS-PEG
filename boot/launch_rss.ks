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

local tgt_per is 145000.

// PEG target conditions
set peg_targettype to 2.
peg_set_peA_apA_attA(tgt_per, 200000, 0).
peg_set_inc_lan_at_r(30).

// This is a Titan-II GLV clone.
//
// mass is t, thrust is kN, isp is sec, stages are reverse order from ksp (bottom up if you're looking at MJ)
// ISP numbers for RO engines should come from only MJ's deltaV analyzer and not the PAW or KER
// (MJ gets nonISP fuel correct).
local stageone to lexicon("startmass", 155.488, "endmass", 39.294, "thrust", 2338.58, "isp", 301.82).
local stagetwo to lexicon("startmass", 31.883, "endmass", 5.774, "thrust", 456.1, "isp", 315).
global peg_stages to list(stageone, stagetwo).

clearscreen.

print "Waiting for ship to unpack.".
wait until ship:unpacked.

local startgui is gui(400).
local button is startgui:addbutton("launch").

set button:onclick to start_launch@.

startgui:show().

local ready is false.
wait until ready.

function start_launch {
    startgui:hide().
    set ready to true.
}

function enable_autostaging {
    when maxthrust = 0 then {
        print "staging".
        stage.
        return true.
    }
}

// FIXME: stage launch clamps at TWR > 1.0
// FIXME: hotstaging

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

        //set kuniverse:timewarp:mode to "PHYSICS".
        //set kuniverse:timewarp:rate to 4.

        set_high_Q_trigger().
    }
}

function set_high_Q_trigger {
    when ship:dynamicpressure > 0.10 then {
        set_peg_guidance_trigger().
    }
}

function set_peg_guidance_trigger {
    when ship:dynamicpressure < 0.05 then {
        print "PEG guidance".

        lock steering to peg_uf().

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
