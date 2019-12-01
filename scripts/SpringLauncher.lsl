    /*
                       Fourmilab Spring Launcher

        Designed and implemented in September 2019 by John Walker

        This program is licensed under a Creative Commons
        Attribution-ShareAlike 4.0 International License.
            http://creativecommons.org/licenses/by-sa/4.0/
        Please see the License section in the "Fourmilab Spring
        Launcher User Guide" notecard included in the object for
        details.

    */

    integer commandChannel = 1949; // Command channel in chat
    integer commandH;           // Handle for command channel
    key whoDat = NULL_KEY;      // Avatar who sent command
    integer restrictAccess = 2; // Access restriction: 0 none, 1 group, 2 owner

    integer sit = FALSE;        // Is user sitting on control seat ?

    float X_THRUST = 20;        // Thrust along X axis
    float Z_THRUST = 15;        // Thrust along Z axis

    float Z_impulse = 25;       // Impulse when avatar sits
    vector traj = <0, 0, 1>;    // Trajectory of launch
    float bail = 12;            // Time before we bail out avatar
    float epause = 0.5;         // Pause between sit/launch and unsit/descent
    integer tstate = 0;         // Current timer event state
    integer flight = FALSE;     // Flight mode (take control)
    integer flying = FALSE;     // Has user taken control ?

    string helpFileName = "Fourmilab Spring Launcher User Guide"; // Help notecard name

    float xMotor;               // Current thrust setting for X motor
    float zMotor;               // Current thrust setting for Z motor

    vector posOrig;             // Position before launch
    rotation rotOrig;           // Rotation before launch

    key agent;                  // UUID of agent sitting on control seat
    key owner;                  // UUID of owner of ship

    /*  The following sets the position where the avatar sits
        on the control seat linked prim and the position and
        aim angle of the camera with respect to the avatar's
        location.  */

    vector SIT_POS = <0.25, 0.0, 0.65>;     // Avatar sit position on platform
    vector CAM_OFFSET = <-2.5, 0.0, 1.25>;  // Offset of camera lens from sit position
    vector CAM_ANG = <0.5, -1.0, -1>;       // Camera look-at point relative to CAM_OFFSET

    //  Indices of linked components

    integer lSpringE;           // Extended spring
    integer lSpringC;           // Compressed spring

    /*  Find a linked prim from its name.  Avoids having to slavishly
        link prims in order in complex builds to reference them later
        by link number.  You should only call this once, in state_entry(),
        and then save the link numbers in global variables.  Returns the
        prim number or -1 if no such prim was found.  Caution: if there
        are more than one prim with the given name, the first will be
        returned without warning of the duplication.  */

    integer findLinkNumber(string pname) {
        integer i = llGetLinkNumber() != 0;
        integer n = llGetNumberOfPrims() + i;

        for (; i < n; i++) {
            if (llGetLinkName(i) == pname) {
                return i;
            }
        }
        return -1;
    }

    //  tawk  --  Send a message to the interacting user in chat

    tawk(string msg) {
        if (whoDat == NULL_KEY) {
            //  No known sender.  Say in nearby chat.
            llSay(PUBLIC_CHANNEL, msg);
        } else {
            llRegionSayTo(whoDat, PUBLIC_CHANNEL, msg);
        }
    }

    //  checkAccess  --  Check if user has permission to send commands

    integer checkAccess(key id) {
        return (restrictAccess == 0) ||
               ((restrictAccess == 1) && llSameGroup(id)) ||
               (id == llGetOwner());
    }

    //  processCommand  --  Process a command

    processCommand(key id, string message) {

        if (!checkAccess(id)) {
            llRegionSayTo(id, PUBLIC_CHANNEL,
                "You do not have permission to control this object.");
            return;
        }

        string lmessage = llToLower(llStringTrim(message, STRING_TRIM));
        list args = llParseString2List(lmessage, [" "], []);    // Command and arguments
        string command = llList2String(args, 0);    // The command

        whoDat = id;                    // Direct chat output to sender of command

        //  Access who                  Restrict chat command access to public/group/owner

        if (command == "access") {
            string who = llList2String(args, 1);

            if (who == "public") {
                restrictAccess = 0;
            } else if (who == "group") {
                restrictAccess = 1;
            } else if (who == "owner") {
                restrictAccess = 2;
            } else {
                tawk("Unknown access restriction \"" + who +
                    "\".  Valid: public, group, owner.\n");
            }

        /*  Channel n                   Change command channel.  Note that
                                        the channel change is lost on a
                                        script reset.  */

        } else if (command == "channel") {
            integer newch = (integer) llList2String(args, 1);
            if ((newch < 2)) {
                tawk("Invalid channel " + (string) newch + ".");
            } else {
                llListenRemove(commandH);
                commandChannel = newch;
                commandH = llListen(commandChannel, "", NULL_KEY, "");
                tawk("Listening on /" + (string) commandChannel);
            }

        //  Help                        Give help information

        } else if (command == "help") {
            llGiveInventory(id, helpFileName);      // Give requester the User Guide notecard

        //  Restart                     Perform a hard restart (reset script)

        } else if (command == "restart") {
            llResetScript();            // Note that all global variables are re-initialised

        //  Set                         Set simulation parameter

        } else if (command == "set") {
            string param = llList2String(args, 1);
            string svalue = llList2String(args, 2);
            float value = (float) svalue;

            if (param == "bail") {              // bail: time before bail-out in seconds
                bail = value;

            } else if (param == "flight") {     // flight: take control with navigation key
                if (svalue == "on") {
                    flight = TRUE;
                } else if (svalue == "off") {
                    flight = FALSE;
                } else {
                    tawk("Invalid flight mode \"" + svalue + "\".  Must be on or off.");
                }

            } else if (param == "impulse") {    // impulse: thrust in +Z axis
                Z_impulse = value;

            } else if (param == "pause") {      // pause: delay for sit/unsit action, seconds
                epause = value;

            } else if (llGetSubString(param, 0, 3) == "traj") { // trajectory: specify launch vector
                traj = < value, (float) llList2String(args, 3), (float) llList2String(args, 4) >;

            } else {
                tawk("Unknown variable \"" + param +
                    "\".  Valid: bail, flight, impulse, pause, trajectory.");
            }

        //  Stat                        Print current status

        } else if (llGetSubString(command, 0, 3) == "stat") {
            tawk("Original Pos: " + (string) posOrig + " Rot: " + (string) rotOrig);
            tawk("Impulse: " + (string) Z_impulse + "  Trajectory: " + (string) traj +
                 " Bail: " + (string) bail + "  Pause: " + (string) epause);
            string sflight = "off";
            if (flight) {
                sflight = "on";
            }
            tawk("Flight: " + sflight);

/*
        //  Test n                      Run built-in test n

        } else if (command == "test") {
            integer n = (integer) llList2String(args, 1);
            if (n == 1) {
            } else if (n == 2) {
            } else if (n == 3) {
            } else {
            }
*/
        } else {
            tawk("Huh?  \"" + message + "\" undefined.  Chat /" +
                (string) commandChannel + " help for the User Guide.");
        }
    }

    //  vehicleInit  --  Initialise vehicle modes

    vehicleInit() {

        //  Define vehicle properties

        llSetVehicleType(VEHICLE_TYPE_AIRPLANE);
        llSetVehicleVectorParam(VEHICLE_LINEAR_FRICTION_TIMESCALE, <200, 20, 20>);

        //  Uniform angular friction

        llSetVehicleFloatParam(VEHICLE_ANGULAR_FRICTION_TIMESCALE, 2);

        //  Linear motor parameters (for front/back motion)

        llSetVehicleVectorParam(VEHICLE_LINEAR_MOTOR_DIRECTION, <0, 0, 0>);
        llSetVehicleFloatParam(VEHICLE_LINEAR_MOTOR_TIMESCALE, 0);
        llSetVehicleFloatParam(VEHICLE_LINEAR_MOTOR_DECAY_TIMESCALE, 1);

        //  Angular motor parameters (for turning)

        llSetVehicleVectorParam(VEHICLE_ANGULAR_MOTOR_DIRECTION, <0, 0, 0>);
        llSetVehicleFloatParam(VEHICLE_ANGULAR_MOTOR_TIMESCALE, 0);
        llSetVehicleFloatParam(VEHICLE_ANGULAR_MOTOR_DECAY_TIMESCALE, 0.4);

        //  Hovering parameters

        llSetVehicleFloatParam(VEHICLE_HOVER_HEIGHT, 2);
        llSetVehicleFloatParam(VEHICLE_HOVER_EFFICIENCY, 0);
        llSetVehicleFloatParam(VEHICLE_HOVER_TIMESCALE, 10000);
        llSetVehicleFloatParam(VEHICLE_BUOYANCY, 1.0);      // Maintain existing altitude

        //  Disable linear deflection

        llSetVehicleFloatParam(VEHICLE_LINEAR_DEFLECTION_EFFICIENCY, 0);
        llSetVehicleFloatParam(VEHICLE_LINEAR_DEFLECTION_TIMESCALE, 5);

        //  Disable angular deflection

        llSetVehicleFloatParam(VEHICLE_ANGULAR_DEFLECTION_EFFICIENCY, 0);
        llSetVehicleFloatParam(VEHICLE_ANGULAR_DEFLECTION_TIMESCALE, 5);

        //  Disable vertical attractor
        llSetVehicleFloatParam( VEHICLE_VERTICAL_ATTRACTION_EFFICIENCY, 1 );
        llSetVehicleFloatParam( VEHICLE_VERTICAL_ATTRACTION_TIMESCALE, 1 );

        /*  We steer by banking, as on a motorcycle or in an
            aircraft with co-ordinated turns.  The following
            parameters specify how banking behaves, and can be
            interpreted as how responsive the vehicle is to the
            the left and right arrow keys whilst moving.  */

        llSetVehicleFloatParam( VEHICLE_BANKING_EFFICIENCY, 1);
        llSetVehicleFloatParam( VEHICLE_BANKING_MIX, 0.5);
        llSetVehicleFloatParam( VEHICLE_BANKING_TIMESCALE, 0.01);

        /*  We rotate our local frame with respect to the
            global co-ordinate system as follows.  */

        llSetVehicleRotationParam(VEHICLE_REFERENCE_FRAME, <0, 0, 0, 1>);

        //  Remove these flags
        llRemoveVehicleFlags(VEHICLE_FLAG_NO_DEFLECTION_UP |
                             VEHICLE_FLAG_HOVER_WATER_ONLY |
                             VEHICLE_FLAG_LIMIT_ROLL_ONLY |
                             VEHICLE_FLAG_HOVER_TERRAIN_ONLY |
                             VEHICLE_FLAG_HOVER_GLOBAL_HEIGHT |
                             VEHICLE_FLAG_HOVER_UP_ONLY |
                             VEHICLE_FLAG_LIMIT_MOTOR_UP);

        //  A vehicle must have physics
        llSetStatus(STATUS_PHYSICS, TRUE);
    }

    //  vehicleTerm  --  Terminate vehicle mode

    vehicleTerm() {
        //  We aren't a vehicle any more
        llSetVehicleType(VEHICLE_TYPE_NONE);
        llSetStatus(STATUS_PHYSICS, FALSE);
    }

    //  springCompression  --  Set spring visibility to compressed or expanded

    springCompression(integer compressed) {
        llSetLinkPrimitiveParamsFast(lSpringC,
            [ PRIM_COLOR, ALL_SIDES, <1, 1, 1>, compressed ]);
        llSetLinkPrimitiveParamsFast(lSpringE,
            [ PRIM_COLOR, ALL_SIDES, <1, 1, 1>, !compressed ]);
    }

    default {
        state_entry() {

            //  Find link numbers for components

            lSpringC = findLinkNumber("Spring: compressed");
            lSpringE = findLinkNumber("Spring: extended");

            //  Initialise sit position

            llSetSitText("Meep Meep");         // Set text for Sit On menu item
            llSitTarget(SIT_POS, ZERO_ROTATION);
            llSetCameraEyeOffset(CAM_OFFSET);
            llSetCameraAtOffset(CAM_ANG);

            //  Set initial spring visibility

            springCompression(TRUE);            // Show spring as compressed

            //  Set initial modes

            flight = flying = FALSE;

            //  Start listening on the command chat channel
            commandH = llListen(commandChannel, "", NULL_KEY, "");
        }

        //  When we're instantiated, set physics off and save our owner

        on_rez(integer num) {
            llSetStatus(STATUS_PHYSICS, FALSE);
            owner = llGetOwner();
        }

        /*  The listen event handler processes messages from
            our chat control channel.  */

        listen(integer channel, string name, key id, string message) {
            processCommand(id, message);
        }

        /*  The changed event handler detects when an avatar
            sits on the control seat or stands up and departs.
            Note: if you need to link or unlink prims from the
            composite object, you *must* add code to this event
            to just return without doing anything.  Otherwise
            the link change will be interpreted as a sit/stand
            event and cause all kinds of mayhem.  */

        changed(integer change) {
            agent = llAvatarOnSitTarget();
            if (change & CHANGED_LINK) {
                if ((agent == NULL_KEY) && sit) {

                    //  Avatar has stood up, departing

                    springCompression(TRUE);
                    vehicleTerm();
                    llStopAnimation("sit_ground");
                    llReleaseControls();
                    sit = FALSE;
                    flying = FALSE;

                    //  Zap back to starting position
                    llSetRegionPos(posOrig);
                    llSetLinkPrimitiveParamsFast(LINK_ROOT, [ PRIM_ROTATION, rotOrig ]);
                } else if ((agent != NULL_KEY) && (!sit)) {

                    //  Avatar has sat on the control seat

                    agent = llAvatarOnSitTarget();
                    sit = TRUE;
                    llRequestPermissions(agent,
                        PERMISSION_TAKE_CONTROLS | PERMISSION_TRIGGER_ANIMATION);

                    //  Remember starting position and rotation of platform
                    posOrig = llList2Vector(llGetLinkPrimitiveParams(LINK_ROOT,
                         [ PRIM_POSITION]), 0);
                    rotOrig = llList2Rot(llGetLinkPrimitiveParams(LINK_ROOT,
                         [ PRIM_ROTATION]), 0);


                    springCompression(FALSE);       // Show the expanded spring
                    vehicleInit();
                    tstate = 1;                     // Initialise the state machine
                    llSetTimerEvent(epause);
                }
            }
        }

        //  If we have permission, capture controls and run animation

        run_time_permissions(integer perm) {
            if (perm & (PERMISSION_TAKE_CONTROLS)) {
                llTakeControls(CONTROL_UP |
                               CONTROL_DOWN |
                               CONTROL_FWD |
                               CONTROL_BACK |
                               CONTROL_RIGHT |
                               CONTROL_LEFT |
                               CONTROL_ROT_RIGHT |
                               CONTROL_ROT_LEFT, TRUE, FALSE);
            }
            if (perm & PERMISSION_TRIGGER_ANIMATION) {
                llStartAnimation("sit_ground");
                llStopAnimation("sit");
            }
        }

        /*  The control event receives messages when one of the flight
            control keys we've captured is pressed.  It adjusts the
            thrust on the X and Z axis linear motors.  */

        control(key id, integer level, integer edge) {
            vector angular_motor;

            /*  "What's with this tstate business?", you ask.
                Well, you see, when you first capture controls,
                one or more bogus control events may filter in,
                surprising you, even though the user hasn't
                pressed any of the keys you captured.  We ignore
                control inputs during the epause between the sit
                and launch so these inputs don't cause trouble,
                particularly in flight mode.  */

            if (tstate == 1) {
                return;
            }

            //  Forward and backward motion keys

            if ((level & CONTROL_FWD) || (level & CONTROL_BACK)) {
                if (edge & CONTROL_FWD) xMotor = X_THRUST;
                if (edge & CONTROL_BACK) xMotor = -X_THRUST;
            } else {
                xMotor = 0;
             }

            //  Upward and downward motion keys

            if ((level & CONTROL_UP) || (level & CONTROL_DOWN)) {
                if (level & CONTROL_UP) {
                    zMotor = Z_THRUST;
                }
                if (level & CONTROL_DOWN) {
                    zMotor = -Z_THRUST;
                }
            } else {
                zMotor = 0;
            }

            llSetVehicleVectorParam(VEHICLE_LINEAR_MOTOR_DIRECTION,
                <xMotor, 0, zMotor>);

            //  Left and right turn keys

            if (level & CONTROL_RIGHT) {
                angular_motor.x = TWO_PI;
                angular_motor.y /= 8;
            }

            if (level & CONTROL_LEFT) {
                angular_motor.x = -TWO_PI;
                angular_motor.y /= 8;
            }

            if (level & CONTROL_ROT_RIGHT) {
                angular_motor.x = TWO_PI;
                angular_motor.y /= 8;
            }

            if (level & CONTROL_ROT_LEFT) {
                angular_motor.x = -TWO_PI;
                angular_motor.y /= 8;
            }

            llSetVehicleVectorParam(VEHICLE_ANGULAR_MOTOR_DIRECTION,
                angular_motor);

            /*  If flight mode is set and we aren't already in
                flying mode, set flying and cancel the bail
                timer if it's running.  In flight mode, once the
                user presses a navigation key, bail out is
                suppressed and it's up to the user to stand up
                when bored of flying.  */

            if (flight && (!flying) &&
                (level & (CONTROL_FWD | CONTROL_BACK |
                          CONTROL_UP | CONTROL_DOWN |
                          CONTROL_RIGHT | CONTROL_LEFT |
                          CONTROL_ROT_RIGHT | CONTROL_ROT_LEFT))) {
                flying = TRUE;
                if (bail != 0) {
                    tstate = 0;
                    llSetTimerEvent(0);
                }
                springCompression(TRUE);
            }
        }

        /*  The timer event operates a simple state machine which
            sequences operations to avoid race conditions.  There
            are four states:

                    0   Idle, waiting for avatar to sit
                    1   Avatar just sat, send launch impulse
                    2   Sit timer expired, play bail-out sound
                    3   Throw avatar off platform and un-sit, return to 0
        */

        timer() {

            //  State 1: New avatar seated; apply launch impulse

            if (tstate == 1) {
                tstate = 2;
                llPlaySound("Launch", 1);
                if (Z_impulse > 0) {
                    llSetVehicleVectorParam(VEHICLE_LINEAR_MOTOR_DIRECTION, llVecNorm(traj) * Z_impulse);
                }
                llSetTimerEvent(bail);    //  Set timer to eject avatar
                if (bail == 0) {
                    tstate = 0;
                }

            //  State 2: Bail-out timer expired; play bail-out sound

            } else if (tstate == 2) {
                llPlaySound("Punch", 1);
                tstate = 3;
                llSetTimerEvent(epause);

            //  State 3: Shift avatar off platform an un-seat, return to state 0

            } else if (tstate == 3) {
                tstate = 0;
                if (agent != NULL_KEY) {
                    //  Ensure agent not on platform
                    llSetLinkPrimitiveParamsFast(llGetNumberOfPrims(),
                        [ PRIM_POS_LOCAL,
                          llList2Vector(llGetLinkPrimitiveParams(llGetNumberOfPrims(),
                             [ PRIM_POS_LOCAL]), 0) + <1, 0, 0> ]);
                    //  Now unsit agent
                    llUnSit(agent);
                    //  Cancel timer
                    llSetTimerEvent(0);
                }
            }
        }
    }
