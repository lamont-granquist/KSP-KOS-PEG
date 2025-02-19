@LAZYGLOBAL OFF.
@CLOBBERBUILTINS OFF.

function rotxy {
    parameter vec.
    parameter ang.

    local sang to sin(ang).
    local cang to cos(ang).

    return V(
        vec:x * cang - vec:z * sang,
        vec:y,
        vec:x * sang + vec:z * cang
    ).
}

function toinertial {
    parameter vec.

    local ang to vang(SOLARPRIMEVECTOR, V(1, 0, 0)).

    return rotxy(vec, ang).
}

function frominertial {
    parameter vec.

    local ang to vang(SOLARPRIMEVECTOR, V(1, 0, 0)).

    return rotxy(vec, -ang).
}

function swizzle {
    parameter vec.

    return V(
        vec:x,
        vec:z,
        vec:y
    ).
}

// clamps to [0,360)
function mod360 {
    parameter x.

    local x is mod(x, 360).

    if x >= 0 {
        return x.
    }
    return 360 + x.
}

// clamps to (-180,180]
function mod180 {
    parameter x.

    local x is mod360(x).
    if x <= 180 { return x. }
    return x - 360.
}

function clamp {
    parameter x.
    parameter xmin.
    parameter xmax.

    if x < xmin { return xmin. }.
    if x > xmax { return xmax. }.
    return x.
}

function safe_arcsin {
    parameter x.
    return arcsin(clamp(x, -1.0, 1.0)).
}

function safe_arccos {
    parameter x.
    return arccos(clamp(x, -1.0, 1.0)).
}

function angle_for_inclination {
    parameter inc. // inclination in degrees.
    parameter lat. // latitude of position.

    local cosAngle is cos(inc) / cos(lat).
    if (abs(cosAngle) > 1.0)
    {
        // for impossible inclinations return due east or west
        if abs(mod180(inc)) < 90 {
            return 0.
        } else {
            return 180.
        }
    }
    local angle is safe_arccos(cosAngle).

    // negative inclinations are conventionally south-going
    if inc < 0 { local angle is -angle. }.

    return mod360(90 - angle).
}

function heading_for_inclination {
    parameter inc. // inclination in degrees.
    parameter rv.  // position in left-handed frame.

    local lat is safe_arcsin(rv:x / r:mag).

    return angle_for_inclination(inc, lat).
}

function horizontal_velocity_for_inclination {
    parameter inc.  // inclination in degrees.
    parameter rv.   // position in left-handed frame.
    parameter vmag. // magnitude of constructed vector.

    local lat is safe_arcsin(rv:y / rv:mag).
    local lng is arctan2(rv:z, rv:x). // not real lng

    local angle is angle_for_inclination(inc, lat).

    local eval is sin(angle).
    local nval is cos(angle).

    local slat is sin(lat).
    local slng is sin(lng).
    local clat is cos(lat).
    local clng is cos(lng).

    local vec is V(
        -slng * eval + (-slat * clng) * nval,
        clat * nval,
        clng * eval + (-slat * slng) * nval
    ).

    return vec * vmag.
}
