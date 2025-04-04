'   _____  _             ___  ___              _      _
'  |_   _|| |            |  \/  |             | |    (_)
'    | |  | |__    ___   | .  . |  __ _   ___ | |__   _  _ __    ___
'    | |  | '_ \  / _ \  | |\/| | / _` | / __|| '_ \ | || '_ \  / _ \ 
'    | |  | | | ||  __/  | |  | || (_| || (__ | | | || || | | ||  __/
'    \_/  |_| |_| \___|  \_|  |_/ \__,_| \___||_| |_||_||_| |_| \___|
'
'
'______        _      _                  __   ______  _         _             _
'| ___ \      (_)    | |                / _|  | ___ \(_)       | |           | |
'| |_/ / _ __  _   __| |  ___     ___  | |_   | |_/ / _  _ __  | |__    ___  | |_
'| ___ \| '__|| | / _` | / _ \   / _ \ |  _|  |  __/ | || '_ \ | '_ \  / _ \ | __|
'| |_/ /| |   | || (_| ||  __/  | (_) || |    | |    | || | | || |_) || (_) || |_
'\____/ |_|   |_| \__,_| \___|   \___/ |_|    \_|    |_||_| |_||_.__/  \___/  \__|
'
'
'  _    _  _  _  _  _                           __   _____  _____  __
' | |  | |(_)| || |(_)                         /  | |  _  ||  _  |/  |
' | |  | | _ | || | _   __ _  _ __ ___   ___   `| | | |_| || |_| |`| |
' | |/\| || || || || | / _` || '_ ` _ \ / __|   | | \____ |\____ | | |
' \  /\  /| || || || || (_| || | | | | |\__ \  _| |_.___/ /.___/ /_| |_
'  \/  \/ |_||_||_||_| \__,_||_| |_| |_||___/  \___/\____/ \____/ \___/


'The Machine (Bride of Pinbot) Williams 1991 for VP10 (Requires VP10.5 final release or greater to play)

'***The very talented BOP development team.***
'Original VP10 beta by "Unclewilly and completed by wrd1972"
'Additional scripting assistance by "cyberpez", "Rothbauerw".
'Clear ramps, wire ramp primitives, pop-bumper caps and flasher domes, shadows and flipper prims by "Flupper"
'Additional 3D work by "Dark" and "Cyberpez"
'Space shuttle toy and sidewall graphics by "Cyberpez"
'Face rotation scripting by "KieferSkunk/Dorsola & "Rothbauerw""
'High poly helmet and re-texture by "Dark"
'Original bride helmet, plastics and playfield redraw by "Jc144"
'Desktop view, scoring dials aand background by "32Assassin"
'Full funtion ball trough by "cyberpez"
'Lighting by wrd1972
'Table physics by "wrd1972"
'Modulated GI lighting by "nFozzy"
'LED color GI lighting by "cyberpez"
'DOF and controller by "Arngrim"
'***Many...many thanks to you all for the tremenedous efforts and hard work for making this awesome table a reality. I just cant thank you all enough.***

'NOTE: On rare occasions and based on the performance of the PC being used, the BOP face rotation can become un-syncronized.
'When this happens, the incorrect face will be shown at table start-up. To fix this, delete the "BOP_L*.nv" file located in the NVRAM folder, inside your VpinMAME directory.

Option Explicit
Randomize
Dim OptReset


''***************************************************************************************************************************************************************
' _____     _     _        ___________ _   _                   _   _
'|_   _|   | |   | |      |  _  | ___ \ | (_)                 | | | |
'  | | __ _| |__ | | ___  | | | | |_/ / |_ _  ___  _ __  ___  | |_| | ___ _ __ ___
'  | |/ _` | '_ \| |/ _ \ | | | |  __/| __| |/ _ \| '_ \/ __| |  _  |/ _ \ '__/ _ \ 
'  | | (_| | |_) | |  __/ \ \_/ / |   | |_| | (_) | | | \__ \ | | | |  __/ | |  __/
'  \_/\__,_|_.__/|_|\___|  \___/\_|    \__|_|\___/|_| |_|___/ \_| |_/\___|_|  \___|
'

'MUSIC SNIPPET MOD
'Music Snippet On = 0
'Music Snippet Off = 1
Musicsnippet = 0


'SIDEWALLS MOD
'Black Wood = 0
'Starfield =1
'BOP Blades = 2
'Random = 3
Sidewalls = 0


'SIDE RAILS
'   Show Side Rails = 0
'   Hide Side Rails = 1
'Change the value below to set option
Rails = 0


'CHEATER POST MOD
'Hide Cheater Post = 0
'Show Cheater Post = 1
cheaterpost = 0


'GI COLOR MOD
'White LED = 0
'Blue LED = 1
'Pink LED = 2
'Blue/Pink = 3
'Random = 4
GIColorMod = 0


'HELMET LIGHTS COLOR MOD
'White lights = 0
'Blue lights = 1
'BOP 2.0 Lights = 2
'ColorChanging = 3
'Random = 4
HLcolor = 0


'SKILL SHOT RAMP COLOR MOD
'Skill Shot Ramp Normal = 0
'Skill Shot Ramp Colored = 1
'Random = 2
SSRampColorMod = 0


'RUBBERS MOD
'Black Rubbers = 0
'White Rubbers = 1
'Random = 2
RubberColor = 0


'Flippers Rubbers Mod
'Black Rubbers = 0
'Red Rubbers = 1
'Blue Rubbers = 2
FlipperRubbers = 0


'SHUTTLE TOY MOD
'Hide Shuttle =0
'Show Shuttle =1
ShipMod = 0


'Hottie Face Mod
'Normal = 0
'Ashley = 1
'Brooke = 2
'Harley = 3
'Kelly = 4
'Lisa = 5
'Meagan = 6
'Shelly = 7
'Rachel = 8
'Valerie = 9
'Random = 10
HottieMod = 0


'BALL SHADOW MOD
'	No Ball Shadow = 0
'	Add Ball Shadow = 1
Ballshadow = 0


'PLAYFIELD SHADOW INTENSITY (adds additional visual depth)
'Usable range is 0 (lighter) - 100 (darker)
shadowopacity = 80


' ********************************************************************************
' Sound Options
'

Const VolDiv = 500    ' Smaller value - louder sound.

Const VolBump   = 2    ' Bumpers multiplier.
Const VolRol    = 1    ' Rollovers volume multiplier.
Const VolRub    = 3    ' Rubbers volume multiplier.
Const VolGates  = 1    ' Gates volume multiplier.
Const VolTarg   = 1    ' Targets multiplier.
Const VolSpin   = 1.5  ' Spinners volume.
Const VolFlip   = 1    ' Flipper volume.
Const VolCol    = 3    ' Ball Collition volume.

'

'***************************************************************************************************************************************************************

'RomName
cGameName = "bop_l8"

Const BallSize = 25  'Ball radius
Const ballmass = 1  'Ball mass
Const UseVPMModSol = True


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim cGameName, ShipMod, cheaterpost, sidewalls, Musicsnippet, Prevgameover


Dim DesktopMode: DesktopMode = Table.ShowDT

If DesktopMode = False Then 'Show Desktop components
Rails=1
Else
Rails=0
End if


LoadVPM "01560000", "WPC.VBS", 3.26

SetLocale(1033)

'Variables
Dim bsSaucer, bsLEye, bsREye, bsSS, plungerIM, mFace, xx, bump1, bump2, bump3
Dim mechHead, headAngle, prevHeadAngle, currentFace
Dim MaxBalls

MaxBalls=3
Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 1
Const HandleMech = 0

'Standard Sounds
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = ""
Const SCoin = "CoinIn"



'Table Init
Sub Table_Init
    vpmInit Me
    With Controller
        .GameName =  cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "The Machine BOP, Williams 1991" & vbNewLine & "by wrd1972"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
		 if .Version >= "03020000" Then
			.HandleMechanics = -1   ' -1: Reset internal head position, 
		 Else
			.HandleMechanics = 0
		 end if 
        .Hidden = 1
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With


'	ChangeBats(ChooseBats)

    'Nudging
    vpmNudge.TiltSwitch=14
    vpmNudge.Sensitivity=1
    vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot,UpperSlingShot)


    ' Impulse Plunger
    Const IMPowerSetting = 35 'Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP SSLaunch, IMPowerSetting, IMTime
        .Random 0.3
        .switch 31
        .InitExitSnd SoundFX("Shooter",DOFContactors), SoundFX("shooter",DOFContactors)
        .CreateEvents "plungerIM"
    End With



      Set mechHead = New cvpmMyMech
      With mechHead
'         .MType = vpmMechOneDirSol + vpmMechCircle + vpmMechLinear + vpmMechFast
         .MType = vpmMechOneDirSol + vpmMechCircle + vpmMechLinear + vpmMechSlow
         .Sol1 = 28
         .Sol2 = 27
'         .Length = 60 * 10 * 6	 ' 10 seconds to cycle through all faces.
         .Length = 60 * 10	 ' 10 seconds to cycle through all faces.
         .Steps = 360 * 4    ' 4 half wheel rotations for one full head rotation
	     .InitialPos = 0
'		 .AddSw 67, 0, 150
'		 .AddSw 67, 210, 510
'         .AddSw 67, 570, 870
'		 .AddSw 67, 1290, 1440
         .Callback = GetRef("HeadMechCallback")
         .Start
      End With
    '**Main Timer init
    PinMAMETimer.Enabled = 1

     PrevGameOver = 0

    SetOptions

    CheckMaxBalls 'Allow balls to be created at table start up

	InitLampsNF

	controller.Switch(67)=1

Face_eyes.collidable = False
Face_flat.collidable = False


End Sub

Sub Table_Paused:Controller.Pause = 1:End Sub
Sub Table_unPaused:Controller.Pause = 0:End Sub
Sub Table_Exit:Controller.Stop:End Sub


'*****Keys
Sub Table_KeyDown(ByVal keycode)
     If keycode = plungerkey then plunger.PullBack:PlaySoundAt "plungerpull", plunger
     'If Keycode = RightFlipperKey then SolRFlipper 1
     'If Keycode = LeftFlipperKey then SolLFlipper 1
     If keycode = LeftTiltKey Then LeftNudge 80, 1, 20
     If keycode = RightTiltKey Then RightNudge 280, 1, 20
     If keycode = CenterTiltKey Then CenterNudge 0, 1, 25

    '************************   Start Ball Control 1/3
        if keycode = 46 then                ' C Key
            If contball = 1 Then
                contball = 0
            Else
                contball = 1
            End If
        End If
        if keycode = 48 then                'B Key
            If bcboost = 1 Then
                bcboost = bcboostmulti
            Else
                bcboost = 1
            End If
        End If
        if keycode = 203 then bcleft = 1        ' Left Arrow
        if keycode = 200 then bcup = 1          ' Up Arrow
        if keycode = 208 then bcdown = 1        ' Down Arrow
        if keycode = 205 then bcright = 1       ' Right Arrow
'    '************************   End Ball Control 1/3
    If vpmKeyDown(keycode) Then Exit Sub
End Sub


Sub Table_KeyUp(ByVal keycode)

     If keycode = plungerkey then plunger.Fire:PlaySoundAt "Plunger2", plunger
     'If Keycode = RightFlipperKey then SolRFlipper 0
     'If Keycode = LeftFlipperKey then SolLFlipper 0

    '************************   Start Ball Control 2/3
    if keycode = 203 then bcleft = 0        ' Left Arrow
    if keycode = 200 then bcup = 0          ' Up Arrow
    if keycode = 208 then bcdown = 0        ' Down Arrow
    if keycode = 205 then bcright = 0       ' Right Arrow
    '************************   End Ball Control 2/3
     If vpmKeyUp(keycode) Then Exit Sub
End sub



'************************   Start Ball Control 3/3
Sub StartControl_Hit()
    Set ControlBall = ActiveBall
    contballinplay = true
End Sub

Sub StopControl_Hit()
    contballinplay = false
End Sub

Dim bcup, bcdown, bcleft, bcright, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti

bcboost = 1     'Do Not Change - default setting
bcvel = 4       'Controls the speed of the ball movement
bcyveloffset = -0.01    'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3    'Boost multiplier to ball veloctiy (toggled with the B key)

Sub BallControl_Timer()
    If Contball and ContBallInPlay then
        If bcright = 1 Then
            ControlBall.velx = bcvel*bcboost
        ElseIf bcleft = 1 Then
            ControlBall.velx = - bcvel*bcboost
        Else
            ControlBall.velx=0
        End If

        If bcup = 1 Then
            ControlBall.vely = -bcvel*bcboost
        ElseIf bcdown = 1 Then
            ControlBall.vely = bcvel*bcboost
        Else
            ControlBall.vely= bcyveloffset
        End If
    End If
End Sub
'************************   End Ball Control 3/3

 '***Solenoids***
        SolCallback(1) = "kisort"
        SolCallback(2) = "KickBallToLane"
        SolCallback(3) = "SolKickout"
        SolCallback(4) = "vpmSolGate Gate3,0,"
        SolCallback(5) = "Solss"
        SolCallback(6) = "solBallLockPost"
        SolCallback(7) = "vpmSolSound SoundFX(""fx_Knocker"",DOFKnocker),"
        SolCallback(8) = "KickMouth"
        SolCallback(15) = "KickLeftEye"
        SolCallback(16) = "KickRightEye"
		SolModCallBack(17) = "ModLampz.SetModLamp 17, " 'Billions
		SolModCallBack(18) = "ModLampz.SetModLamp 18, " 'Left ramp
		SolModCallBack(19) = "ModLampz.SetModLamp 19, " 'jackpot
		SolModCallBack(20) = "ModLampz.SetModLamp 20, " 'SkillShot
		SolModCallBack(21) = "ModLampz.SetModLamp 21, "	'Left Helmet Dome
		SolModCallBack(22) = "ModLampz.SetModLamp 22, "	 'Right Helmet Dome
		SolModCallBack(23) = "ModLampz.SetModLamp 23, "	'Jets Enter Dome
		SolModCallBack(24) = "ModLampz.SetModLamp 24, "	 'Left Loop Dome



'Sub SetLamp(aNr, aOn)
'	Lampz.state(aNr) = abs(aOn)
'End Sub

Sub SolKickout(enabled)
    If Enabled then
        sw46k.kick 22.5,70   '70 = power
        sw46.enabled= 0
        vpmtimer.addtimer 600, "sw46.enabled= 1'"
        PlaysoundAt SoundFx("s",DOFContactors), sw46k
    End If
End Sub

'Sub sw46_Hit()
'    Playsound "fx_vuk_enter"
'End Sub

Sub sw46k_Hit()
    PlaysoundAt "fx_kicker_catch", sw46k
    Controller.Switch(46) = 1
End Sub

Sub sw46k_UnHit()
    Controller.Switch(46) = 0
End Sub


Sub SolMotor(Enabled)
    Light1.State = enabled
End Sub

Sub SolRelay(Enabled)
    Light2.State = enabled
End Sub


 '**************
 ' Solenoid Subs
 '**************


Sub SolSS(enabled)
If Enabled then
plungerIM.AutoFire:SSKick.transZ = 30:EMPos = 30:SSLaunch1.Enabled = True
end if
End Sub

Sub solBallLockPost(enabled)
    If Enabled then BL.IsDropped=1:BLP.transY = -50:BL.TimerEnabled = 1
    PlaySound SoundFX("Solenoid2",DOFContactors)
End Sub

Sub BL_Timer()
        BL.IsDropped=0:BLP.transY = 0:BL.TimerEnabled = 0
End Sub


Dim EMPos
EMPos = 0
Sub SSLaunch1_timer()
	EmPos = EmPos - 1
	SSKick.transZ = EmPos
	If EmPos < 0 then SSLaunch1.Enabled = 0
End Sub


'***********************************************
'**************
'ConstantUpdates
'**************

Sub GameTimer_Timer()
    UpdateGatesSpinners
End Sub

Dim Pi, GateSpeed
Pi = Round(4*Atn(1),6)
GateSpeed = 0.5

'SPinner Brake
sub SpinnerBrake_Hit
	dim amount: amount = .5
	ActiveBall.velx = ActiveBall.velx * amount
	ActiveBall.vely = ActiveBall.vely * amount
end sub

Dim Gate2Open,Gate2Angle:Gate2Open=0:Gate2Angle=0
Sub Gate2_Hit():Gate2Open=1:Gate2Angle=0:End Sub

Dim Gate4Open,Gate4Angle:Gate4Open=0:Gate4Angle=0
Sub Gate4_Hit():Gate4Open=1:Gate4Angle=0:End Sub

Dim Gate5Open,Gate5Angle:Gate5Open=0:Gate5Angle=0
Sub Gate5_Hit():Gate5Open=1:Gate5Angle=0:End Sub

Dim Gate6Open,Gate6Angle:Gate6Open=0:Gate6Angle=0
Sub Gate6_Hit():Gate6Open=1:Gate6Angle=0:End Sub

Dim Gate8Open,Gate8Angle:Gate8Open=0:Gate8Angle=0
Sub Gate8_Hit():Gate8Open=1:Gate8Angle=0:End Sub



Sub UpdateGatesSpinners
    SpinnerT1.RotX = -(sw51.currentangle)

    pSpinnerRod.TransX = sin( (sw51.CurrentAngle+180) * (2*PI/360)) * 5
    pSpinnerRod.TransY = sin( (sw51.CurrentAngle- 90) * (2*PI/360))

    If Gate3.currentangle > 70 Then
        Gate3P.RotZ = -90
    Else
        Gate3P.RotZ = -(Gate3.currentangle+40)
    End If

	Gate0P.Rotx = 90 - Gate0.currentangle
	Gate1P.Rotx = 90 - Gate1.currentangle
	Gate2P.Rotz = 90 - Gate2.currentangle +270
	Gate4P.Rotx = 90 - Gate4.currentangle
	Gate5P.Rotz = 110 + Gate5.currentangle -75
	Gate6P.RotX = 90 - Gate6.currentangle
	Gate8P.Rotz = 90 + Gate8.currentangle + 270





    pFaceDiverter.RotX = div.currentangle / 2
End Sub


'***********************************************
'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"


Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFx("fx_flipperup",DOFFlippers), LeftFlipper, VolFlip
         LeftFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFx("fx_flipperdown",DOFFlippers), LeftFlipper, VolFlip
         LeftFlipper.RotateToStart
     End If
 End Sub

Sub SolRFlipper(Enabled)
    If enabled then
         PlaySoundAtVol SoundFx("fx_flipperup",DOFFlippers), RightFlipper, VolFlip
         RightFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFx("fx_flipperdown",DOFFlippers), RightFlipper, VolFlip
         RightFlipper.RotateToStart
     End If
 End Sub


'************************************************************************************************************

 '-----------------
' Head/Face rotation script
' cvpmMech-based script by RothbauerW, DJRobX
'-----------------
'
' MOTOR ASSEMBLY (notes from BoP manual)
'
' Motor has a single peg on it at the outside of a disk, which intersects
' with a plus-shaped guide on the head box.  The peg pushes into a slot on
' the guide and rotates the box 90 degrees - during that time, the disk has
' also rotated 90 degrees.  The remaining 270 degrees, the wheel just free-
' spins.  This type of motion causes the face to rotate slowest when the pin
' is entering or exiting the slot (0/90 degrees), and fastest when the peg is
' halfway through the rotation (45 degrees).  A cosine function will simulate
' this speed curve quite nicely.
'
' The mech handler will do 4 full 360-degree rotations to simulate the drive
' wheel through an entire head-box rotation.  The callback will then determine
' the head-box's current position by simulating the peg.
' ---
'
' FACE SWITCH ASSEMBLY:(notes from BoP manual)
'
' Switch 67 is the head position switch.  The switch should be closed when in
' an indentation on the head box bottom plate, and open at all other times.
' Each indent is a concave surface to allow the switch to travel smoothly, so
' we can assume that the switch is closed when at least halfway into the maximum
' depth of an indent.  There are indents on only three sides (same sides as
' Faces 1, 2 and 4) - the last side (Face 3) doesn't have an indent, so the
' switch stays open on this side.  When looking at the head top-down (on playfield),
' this switch is on the right side, which means Face 4 is facing upward when the
' switch is stuck open.  The size of the indents and an assumption about their
' concavity leads me to believe the switch should be closed for about 10 degrees
' either side of center.

headAngle = 0
prevHeadAngle = 0
currentFace = 1
Dim AngleMult

Function headIsNear(target)
    headIsNear = (headAngle >= target - 10) AND (headAngle <= target + 10)
End Function

Sub HeadMechCallback(aNewPos, aSpeed, aLastPos)

	Controller.HandleMechanics = 0
    headAngle = Fix(aNewPos / 360) * 90        ' Get integer position for current face.
   Dim wheelPos : wheelPos = aNewPos - (headAngle * 4)   ' What position is the wheel in?
   ' Wheel position > 270 = head is 90 degrees further along than original base calc.
   If (wheelPos >= 270) Then
      headAngle = headAngle + 90
    ElseIf (wheelPos >= 180) Then
      ' Calculate how far along into rotation the head is.
     ' Since our goal is to get slow movement at both ends of the arc, we need a
     ' Cosine over 180 degrees (Pi radians), so this formula takes care of that.
     ' Also, Cosine varies between -1 and 1, so add 1 to the value (range 0-2) and
     ' cut it in half to get correct partial angle with correct accel curve.
     Dim wheelRads : wheelRads = ((wheelPos - 180) * 2 + 180) * 0.0174532925
      headAngle = headAngle + 45 * (1 + Cos(wheelRads))
    End If
 
    ' No more to do if the head hasn't changed position.
   If headAngle = prevHeadAngle Then
        Exit Sub
    End If
 
    ' Head has moved.
   prevHeadAngle = headAngle

'	if aNewPos <= 360 Then
'		HeadAngle =  getAngle(aNewPos)
'	elseif aNewPos <= 720 Then
'		HeadAngle =  getAngle(aNewPos - 360) + 90
'	elseif aNewPos <= 1080 Then
'		HeadAngle =  getAngle(aNewPos - 720) + 180
'	elseif aNewPos <= 1440 Then
'		HeadAngle =  getAngle(aNewPos - 1080) + 270
'	end if
'
'	debug.print "Head: " & aNewPos &" " & Controller.Switch(67)&" " & HeadAngle


	' Position primitives
	Face.ObjRotY = headAngle
	Face1.ObjRotY = headAngle
	FaceGuides.ObjRotY = headAngle
	pFaceDiverter.ObjRotY = headAngle
	pFaceDiverterPegs.ObjRotY = headAngle


	' Determine which face is up, if any
	'Face1
	If headIsNear(0) OR headIsNear(360) Then
		currentFace = 1
		If HottieModType = 1 Then
		Else
			'FaceGuides.Visible = true
		End If
		DOF 102, DOFOff
	'Face2
    ElseIf headIsNear(90) Then
		currentFace = 2
		If HottieModType = 1 Then
		Else
			FaceGuides.Visible = true
		End If

		DOF 102, DOFoff
	'Face3
     ElseIf headIsNear(180) Then
		currentFace = 3 
		If HottieModType = 1 Then
		Else
			FaceGuides.Visible = true
		End If
		DOF 102, DOFOff
	'Face4
	ElseIf headIsNear(270) Then
		currentFace = 4 
		If HottieModType = 1 Then
		Else
			FaceGuides.Visible = True
		End If
		DOF 102, DOFOff
    Else
		currentFace = 0
		DOF 102, DOFOn
    End If

    ' Head box position switch
	Controller.Switch(67) = (currentFace > 0 AND currentFace < 4) ' Faces 1, 2 and 3 close the switch.


    F1Guide.IsDropped = NOT (currentFace = 1)
    F1Guide2.IsDropped = NOT (currentFace = 1)
	sw65x.Enabled = (currentFace = 1)

    ' Face 2 parts
	sw63x.Enabled = (currentFace = 2)
    sw64x.Enabled = (currentFace = 2)
    div.Enabled = (currentFace = 2)
    sw63div.Enabled = (currentFace = 2)
    sw64div.Enabled = (currentFace = 2)

	Face_Eyes.Collidable = (currentFace = 2)
	Face_Eyes.visible = (currentFace = 2)

	Select Case currentFace
		Case 1:Face_Mouth.Collidable = True:Face_Eyes.Collidable = False:Face_Flat.Collidable = False:Face_Mouth.visible = False:Face_Eyes.visible = False:Face_Flat.visible = False:ColFaceGuideL3.Collidable = False 
		Case 2:Face_Mouth.Collidable = False:Face_Eyes.Collidable = True:Face_Flat.Collidable = False:Face_Mouth.visible = False:Face_Eyes.visible = False:Face_Flat.visible = False:ColFaceGuideL3.Collidable = True
		Case 3:Face_Mouth.Collidable = False:Face_Eyes.Collidable = False:Face_Flat.Collidable = True:Face_Mouth.visible = False:Face_Eyes.visible = False:Face_Flat.visible = False :ColFaceGuideL3.Collidable = False
		Case 4:Face_Mouth.Collidable = False:Face_Eyes.Collidable = False:Face_Flat.Collidable = True:Face_Mouth.visible = False:Face_Eyes.visible = False:Face_Flat.visible = False :ColFaceGuideL3.Collidable = False
	End Select
End Sub


Function getAngle(step)
	Dim radAngle
	radAngle = Round(4*Atn(1),6) * step / 180 / 2
	getAngle = 45 - 45 * Cos(radAngle)
End Function


'-----------------
' End Head/Face rotation script
'-----------------


'MouthAndEyeKicks
Dim BallLeftEye, BallRightEye, BallMouth

 'Kickers, poppers

   Sub sw63div_hit():div.RotateToStart:End Sub
   Sub sw64div_Hit():div.RotateToEnd:End Sub


	Sub sw63x_Hit()
		Controller.Switch(63) = 1

		Set BallLeftEye = ActiveBall
	End Sub

	Sub KickLeftEye(Enabled)
		Controller.Switch(63) = 0
    PlaySound SoundFX("Solenoid",DOFContactors)
		On Error Resume Next
		BallLeftEye.velz = 10:BallLeftEye.vely = 5:
		On Error Goto 0
	    sw63x.timerenabled = 1
	End Sub

Dim SW63xStep
Sub SW63x_Timer()
	Select Case SW63xStep
		Case 0:	TWKicker2.transy = 7
		Case 1: TWKicker2.transy = 14
		Case 2: TWKicker2.transy = 21
		Case 3: TWKicker2.transy = 14
		Case 4:	TWKicker2.transy = 0
		Case 5:
	End Select
	SW63xStep = SW63xStep + 1
End Sub


   Sub sw64x_Hit():
		Controller.Switch(64) = 1:

		Set BallRightEye = ActiveBall
	End Sub

	Sub KickRightEye(Enabled)
		Controller.Switch(64) = 0
    PlaySound SoundFX("Solenoid",DOFContactors)
		On Error Resume Next
		BallRightEye.velz = 10:BallRightEye.vely = 5:
         'vpmtimer.addtimer 100, "BallRightEye.velz = 10:BallRightEye.vely = 4'"
		On Error Goto 0
	    sw64x.timerenabled = 1
	End Sub


Dim SW64xStep
Sub SW64x_Timer()
	Select Case SW64xStep
		Case 0:	TWKicker3.transy = 7
		Case 1: TWKicker3.transy = 14
		Case 2: TWKicker3.transy = 21
		Case 3: TWKicker3.transy = 14
		Case 4:	TWKicker3.transy = 0
		Case 5:
	End Select
	SW64xStep = SW64xStep + 1
End Sub


	Sub sw65x_Hit()
		Controller.Switch(65) = 1
		Set BallMouth = ActiveBall
		'sw65x.timerenabled = 1
	End Sub


	Sub KickMouth(Enabled)
		Controller.Switch(65) = 0
    PlaySound SoundFX("Solenoid",DOFContactors)
		on error resume next
		BallMouth.velz = 10:BallMouth.vely = 5:
		on error goto 0
	    sw65x.timerenabled = 1
	End Sub


Dim SW65xStep
Sub SW65x_Timer()
	Select Case SW65xStep
		Case 0:	TWKicker1.transy = 7
		Case 1: TWKicker1.transy = 14
		Case 2: TWKicker1.transy = 21
		Case 3: TWKicker1.transy = 14
		Case 4:	TWKicker1.transy = 0
		Case 5:
	End Select
	SW65xStep = SW65xStep + 1
End Sub
'*******************************************************************************************************************************



'Copied from 2.1 release. (4th face - heartbeats, Hey there - MyGod shes alive) works correctly. These are missing from current table on 4th face.

' '-----------------
'' Head/Face rotation script
'' cvpmMech-based script by KieferSkunk/Dorsola, May 2015
''-----------------
''
'' MOTOR ASSEMBLY (notes from BoP manual)
''
'' Motor has a single peg on it at the outside of a disk, which intersects
'' with a plus-shaped guide on the head box.  The peg pushes into a slot on
'' the guide and rotates the box 90 degrees - during that time, the disk has
'' also rotated 90 degrees.  The remaining 270 degrees, the wheel just free-
'' spins.  This type of motion causes the face to rotate slowest when the pin
'' is entering or exiting the slot (0/90 degrees), and fastest when the peg is
'' halfway through the rotation (45 degrees).  A cosine function will simulate
'' this speed curve quite nicely.
''
'' The mech handler will do 4 full 360-degree rotations to simulate the drive
'' wheel through an entire head-box rotation.  The callback will then determine
'' the head-box's current position by simulating the peg.
'' ---
''
'' FACE SWITCH ASSEMBLY:(notes from BoP manual)
''
'' Switch 67 is the head position switch.  The switch should be closed when in
'' an indentation on the head box bottom plate, and open at all other times.
'' Each indent is a concave surface to allow the switch to travel smoothly, so
'' we can assume that the switch is closed when at least halfway into the maximum
'' depth of an indent.  There are indents on only three sides (same sides as
'' Faces 1, 2 and 4) - the last side (Face 3) doesn't have an indent, so the
'' switch stays open on this side.  When looking at the head top-down (on playfield),
'' this switch is on the right side, which means Face 4 is facing upward when the
'' switch is stuck open.  The size of the indents and an assumption about their
'' concavity leads me to believe the switch should be closed for about 10 degrees
'' either side of center.
' 
'headAngle = 0
'prevHeadAngle = 0
'currentFace = 1
' 
'Function headIsNear(target)
'    headIsNear = (headAngle >= target - 10) AND (headAngle <= target + 10)
'End Function
' 
'Sub HeadMechCallback(aNewPos, aSpeed, aLastPos)
'    headAngle = Fix(aNewPos / 360) * 90        ' Get integer position for current face.
'   Dim wheelPos : wheelPos = aNewPos - (headAngle * 4)   ' What position is the wheel in?
'   ' Wheel position > 270 = head is 90 degrees further along than original base calc.
'   If (wheelPos >= 270) Then
'      headAngle = headAngle + 90
'    ElseIf (wheelPos >= 180) Then
'      ' Calculate how far along into rotation the head is.
'     ' Since our goal is to get slow movement at both ends of the arc, we need a
'     ' Cosine over 180 degrees (Pi radians), so this formula takes care of that.
'     ' Also, Cosine varies between -1 and 1, so add 1 to the value (range 0-2) and
'     ' cut it in half to get correct partial angle with correct accel curve.
'     Dim wheelRads : wheelRads = ((wheelPos - 180) * 2 + 180) * 0.0174532925
'      headAngle = headAngle + 45 * (1 + Cos(wheelRads))
'    End If
' 
'    ' No more to do if the head hasn't changed position.
'   If headAngle = prevHeadAngle Then
'        Exit Sub
'    End If
' 
'    ' Head has moved.
'   prevHeadAngle = headAngle
' 
'    ' Position primitives
'    Face.ObjRotY = headAngle
'    Face1.ObjRotY = headAngle
'    FaceGuides.ObjRotY = headAngle
'    pFaceDiverter.ObjRotY = headAngle
'    pFaceDiverterPegs.ObjRotY = headAngle
'
' 
'  ' Determine which face is up, if any
'   If headIsNear(0) OR headIsNear(360) Then
'      currentFace = 1
'
'	If HottieModType = 1 Then
'
'	Else
'		'FaceGuides.Visible = true
'	End If
'
'      DOF 102, DOFOff
'    ElseIf headIsNear(90) Then
'      currentFace = 2
'	If HottieModType = 1 Then
'
'	Else
'		FaceGuides.Visible = true
'	End If
'
'      DOF 102, DOFoff
'    ElseIf headIsNear(180) Then
'      currentFace = 3
'	If HottieModType = 1 Then
'
'	Else
'		FaceGuides.Visible = true
'	End If
'
'      DOF 102, DOFOff
'    ElseIf headIsNear(270) Then
'      currentFace = 4
'	If HottieModType = 1 Then
'
'	Else
'		FaceGuides.Visible = True
'	End If
'
'      DOF 102, DOFOff
'    Else
'      currentFace = 0
'      DOF 102, DOFOn
'    End If
' 
'    ' Head box position switch
'   Controller.Switch(67) = (currentFace > 0 AND currentFace < 4) ' Faces 1, 2 and 3 close the switch.
' 
'    ' Face 1 parts
'   sw65.Enabled = (currentFace = 1)
'    F1Guide.IsDropped = NOT (currentFace = 1)
'    F1Guide2.IsDropped = NOT (currentFace = 1)
' 
'    ' Face 2 parts
'   sw63.Enabled = (currentFace = 2)
'    sw64.Enabled = (currentFace = 2)
'    div.Enabled = (currentFace = 2)
'    sw63div.Enabled = (currentFace = 2)
'    sw64div.Enabled = (currentFace = 2)
'End Sub
' 
'
' 
''-----------------
'' End Head/Face rotation script
''-----------------
' 
' 'Kickers, poppers
'  Sub sw63_Hit():bsLEye.AddBall 0: End Sub
'   Sub sw64_Hit():bsREye.AddBall 0: End Sub
' 
'   Sub sw63div_hit():div.RotateToStart:End Sub
'   Sub sw64div_Hit():div.RotateToEnd:End Sub
' 
'   Sub sw65_Hit():bsMouth.AddBall 0:End Sub
'   Sub SSLaunch_Hit():activeball.mass = BallMass:bsSS.AddBall 0:End Sub

'***********************************************************************************************************************************


   'Bumpers
     Sub Bumper1_Hit:vpmTimer.PulseSw 53:PlaySoundAtVol SoundFx("fx_bumper1",DOFContactors), Bumper1, VolBump:End Sub 'bump1 = 1:Me.TimerEnabled = 1

       Sub Bumper1_Timer()  '53
          Select Case bump1
               Case 1:BR1.z = 0:bump1 = 2
               Case 2:BR1.z = -10:bump1 = 3
               Case 3:BR1.z = -20:bump1 = 4
               Case 4:BR1.z = -20:bump1 = 5
               Case 5:BR1.z = -10:bump1 = 6
               Case 6:BR1.z = 0:bump1 = 7
               Case 7:BR1.z = 10:Me.TimerEnabled = 0
           End Select
       End Sub

      Sub Bumper2_Hit:vpmTimer.PulseSw 55:PlaySoundAtVol SoundFx("fx_bumper2",DOFContactors), Bumper2, VolBump:End Sub 'bump2 = 1:Me.TimerEnabled = 1

       Sub Bumper2_Timer()   '55
          Select Case bump2
               Case 1:BR2.z = 0:bump2 = 2
               Case 2:BR2.z = -10:bump2 = 3
               Case 3:BR2.z = -20:bump2 = 4
               Case 4:BR2.z = -20:bump2 = 5
               Case 5:BR2.z = -10:bump2 = 6
               Case 6:BR2.z = 0:bump2 = 7
               Case 7:BR2.z = 10:Me.TimerEnabled = 0
           End Select
       End Sub

      Sub Bumper3_Hit:vpmTimer.PulseSw 54:PlaySoundAtVol SoundFx("fx_bumper3",DOFContactors), Bumper3, VolBump:End Sub 'bump3 = 1:Me.TimerEnabled = 1

       Sub Bumper3_Timer()  '54
          Select Case bump3
               Case 1:BR3.z = 0:bump3 = 2
               Case 2:BR3.z = -10:bump3 = 3
               Case 3:BR3.z = -20:bump3 = 4
               Case 4:BR3.z = -20:bump3 = 5
               Case 5:BR3.z = -10:bump3 = 6
               Case 6:BR3.z = 0:bump3 = 7
               Case 7:BR3.z = 10:Me.TimerEnabled = 0
           End Select
       End Sub



 'FlipperLanes and Plunger
   Sub sw15_Hit:Controller.Switch(15) = 1:PlaySoundAtVol "rollover", sw15, VolRol:End Sub
   Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub
   Sub sw16_Hit:Controller.Switch(16) = 1:PlaySoundAtVol "rollover", sw16, VolRol:End Sub
   Sub sw16_UnHit:Controller.Switch(16) = 0:End Sub
   Sub sw17_Hit:Controller.Switch(17) = 1:PlaySoundAtVol "rollover", sw17, VolRol:End Sub
   Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub
   Sub sw18_Hit:Controller.Switch(18) = 1:PlaySoundAtVol "rollover", sw18, VolRol:End Sub
   Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub
   Sub sw52_Hit:Controller.Switch(52) = 1:activeball.mass = 1:PlaySoundAtVol "rollover", sw15, VolRol:Stopsound "intro":End Sub
   Sub sw52_UnHit:Controller.Switch(52) = 0:End Sub

   'SS Lane
  Sub sw31_Hit:Controller.Switch(31) = 1:PlaySoundAtVol "rollover", sw31, VolRol:End Sub
   Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub
   Sub sw32_Hit:Controller.Switch(32) = 1:PlaySoundAtVol "rollover", sw32, VolRol:End Sub
   Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub
   Sub sw33_Hit:Controller.Switch(33) = 1:PlaySoundAtVol "rollover", sw33, VolRol:End Sub
   Sub sw33_UnHit:Controller.Switch(33) = 0:End Sub
   Sub sw34_Hit:Controller.Switch(34) = 1:PlaySoundAtVol "rollover", sw34, VolRol:End Sub
   Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub
   Sub sw35_Hit:Controller.Switch(35) = 1:PlaySoundAtVol "rollover", sw35, VolRol:End Sub

   Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub
   'Under Right Ramp
   Sub sw44_Hit:Controller.Switch(44) = 1:PlaySoundAtVol "rollover", sw44, VolRol:End Sub
   Sub sw44_UnHit:Controller.Switch(44) = 0:End Sub
   Sub sw45_Hit:Controller.Switch(45) = 1:PlaySoundAtVol "rollover", sw45, VolRol:End Sub
   Sub sw45_UnHit:Controller.Switch(45) = 0:End Sub

   'Left Loop
  Sub sw43_Hit:Controller.Switch(43) = 1:PlaySoundAtVol "rollover", sw43, VolRol:End Sub
   Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub

   'Gate Switches
  Sub sw47_Hit:vpmTimer.PulseSw 47:End Sub

   'Ball lock Switches
    'LockInit
  LockWall.IsDropped = 1
   Sub sw72_Hit:Controller.Switch(72) = 1:::PlaySoundAtVol "rollover",sw72, VolRol:LockWall.IsDropped = 0:Switch72dir=-2:sw72.TimerEnabled = True:End Sub
   Sub sw72_UnHit:Controller.Switch(72) = 0:Switch72dir=4:sw72.TimerEnabled = True::LockWall.IsDropped = 1:PlaySoundAt "rail_low_slower",sw72:End Sub
   Sub sw71_Hit:Controller.Switch(71) = 1::PlaySoundAtVol "rollover",sw71,VolRol:Switch71dir=-2:sw71.TimerEnabled = True:End Sub
   Sub sw71_UnHit:Controller.Switch(71) = 0:Switch71dir=4:sw71.TimerEnabled = True:End Sub

Sub sw41_Hit:vpmTimer.PulseSw(41):sw41.TimerEnabled = True:End Sub
Sub sw73_Hit:vpmTimer.PulseSw(73):sw73.TimerEnabled = True:End Sub
Sub sw74_Hit:vpmTimer.PulseSw(74):sw74.TimerEnabled = True:PlaySoundAtVol "fx_Gate",sw74,VolGates:End Sub
Sub sw75_Hit:vpmTimer.PulseSw(75):sw75.TimerEnabled = True:End Sub
Sub sw76_Hit:vpmTimer.PulseSw(76):sw76.TimerEnabled = True:PlaySoundAtVol "fx_Gate",sw76,VolGates:End Sub
Sub sw77_Hit:vpmTimer.PulseSw(77):sw77.TimerEnabled = True:PlaySoundAtVol "fx_Gate",sw77,VolGates:End Sub

Const Switch41min = 0
Const Switch41max = -20
Dim Switch41dir

    'switch 41 animation
Switch41dir = -2

Sub sw41_timer()
 pRampSwitch1B.RotY = pRampSwitch1B.RotY + Switch41dir
    If pRampSwitch1B.RotY >= Switch41min Then
        sw41.timerenabled = False
        pRampSwitch1B.RotY = Switch41min
        Switch41dir = -2
    End If
    If pRampSwitch1B.RotY <= Switch41max Then
        Switch41dir = 4
    End If
End Sub

    'switch 71 animation
Const Switch71min = 0
Const Switch71max = -20
Dim Switch71dir
Switch71dir = -2

Sub sw71_timer()
 pRampSwitch7B.RotY = pRampSwitch7B.RotY + Switch71dir
    If Switch71dir = 4 Then
        If pRampSwitch7B.RotY >= Switch71min Then
            sw71.timerenabled = False
            pRampSwitch7B.RotY = Switch71min
        End If
    End If
    If Switch71dir = -2 Then
        If pRampSwitch7B.RotY <= Switch71max Then
            sw71.timerenabled = False
            pRampSwitch7B.RotY = Switch71max
        End If
    End If
End Sub

    'switch 72 animation
Const Switch72min = 0
Const Switch72max = -20
Dim Switch72dir
Switch72dir = -2

Sub sw72_timer()
 pRampSwitch8B.RotY = pRampSwitch8B.RotY + Switch72dir
    If pRampSwitch8B.RotY >= Switch72min Then
        sw72.timerenabled = False
        pRampSwitch8B.RotY = Switch72min
    End If
    If pRampSwitch8B.RotY <= Switch72max Then
        sw72.timerenabled = False
        pRampSwitch8B.RotY = Switch72max
    End If
End Sub

    'switch 73 animation
Const Switch73min = 0
Const Switch73max = -20
Dim Switch73dir
Switch73dir = -2

Sub sw73_timer()
 pRampSwitch6B.RotX = pRampSwitch6B.RotX + Switch73dir
    If pRampSwitch6B.RotX >= Switch73min Then
        sw73.timerenabled = False
        pRampSwitch6B.RotX = Switch73min
        Switch73dir = -2
    End If
    If pRampSwitch6B.RotX <= Switch73max Then
        Switch73dir = 4
    End If
End Sub

    'switch 74 animation
Const Switch74min = 0
Const Switch74max = -20
Dim Switch74dir
Switch74dir = -2

Sub sw74_timer()
 pRampSwitch3B.RotX = pRampSwitch3B.RotX + Switch74dir
    If pRampSwitch3B.RotX >= Switch74min Then
        sw74.timerenabled = False
        pRampSwitch3B.RotX = Switch74min
        Switch74dir = -2
    End If
    If pRampSwitch3B.RotX <= Switch74max Then
        Switch74dir = 4
    End If
End Sub

    'switch 75 animation
Const Switch75min = 0
Const Switch75max = -20
Dim Switch75dir
Switch75dir = -2

Sub sw75_timer()
 pRampSwitch2B.RotY = pRampSwitch2B.RotY + Switch75dir
    If pRampSwitch2B.RotY >= Switch75min Then
        sw75.timerenabled = False
        pRampSwitch2B.RotY = Switch75min
        Switch75dir = -2
    End If
    If pRampSwitch2B.RotY <= Switch75max Then
        Switch75dir = 4
    End If
End Sub


    'switch 76 animation
Const Switch76min = 0
Const Switch76max = -20
Dim Switch76dir
Switch76dir = -2

Sub sw76_timer()
 pRampSwitch5B.RotX = pRampSwitch5B.RotX + Switch76dir
    If pRampSwitch5B.RotX >= Switch76min Then
        sw76.timerenabled = False
        pRampSwitch5B.RotX = Switch76min
        Switch76dir = -2
    End If
    If pRampSwitch5B.RotX <= Switch76max Then
        Switch76dir = 4
    End If
End Sub


    'switch 77 animation
Const Switch77min = 0
Const Switch77max = -20
Dim Switch77dir
Switch77dir = -2

Sub sw77_timer()
 pRampSwitch4B.Rotx = pRampSwitch4B.Rotx + Switch77dir
    If pRampSwitch4B.Rotx >= Switch77min Then
        sw77.timerenabled = False
        pRampSwitch4B.Rotx = Switch77min
        Switch77dir = -2
    End If
    If pRampSwitch4B.Rotx <= Switch77max Then
        Switch77dir = 4
    End If
End Sub


'***Spot targets***

Sub DoubleTarget1_hit
vpmTimer.PulseSw 36:pSW36.TransY = -4:Target36Step = 1:sw36.TimerEnabled = 1:PlaySoundAtVol SoundFx("target",DOFTargets),misc, VolTarg
vpmTimer.PulseSw 37:pSW37.TransY = -4:Target37Step = 1:sw37.TimerEnabled = 1:PlaySoundAtVol SoundFx("target",DOFTargets),misc, VolTarg
End Sub



DIM target28step
Sub sw28_Hit:vpmTimer.PulseSw 28:pSW28.TransY = -3:Target28Step = 1:Me.TimerEnabled = 1:PlaySoundAtVol SoundFx("target",DOFTargets),misc, VolTarg:End Sub			
Sub sw28_timer()
	Select Case Target28Step

		Case 1:pSW28.TransY = -1
		Case 2:pSW28.TransY = 2
        Case 3:pSW28.TransY = 0:Me.TimerEnabled = 0
     End Select
	Target28Step = Target28Step + 1
End Sub	


DIM target36step
Sub sw36_Hit:vpmTimer.PulseSw 36:pSW36.TransY = -3:Target36Step = 1:Me.TimerEnabled = 1:PlaySoundAtVol SoundFx("target",DOFTargets),misc, VolTarg:End Sub			
Sub sw36_timer()
	Select Case Target36Step

		Case 1:pSW36.TransY = -1
		Case 2:pSW36.TransY = 2
        Case 3:pSW36.TransY = 0:Me.TimerEnabled = 0
     End Select
	Target36Step = Target36Step + 1
End Sub	


DIM target37step
Sub sw37_Hit:vpmTimer.PulseSw 37:pSW37.TransY = -3:Target37Step = 1:Me.TimerEnabled = 1:PlaySoundAtVol SoundFx("target",DOFTargets),misc, VolTarg:End Sub			
Sub sw37_timer()
	Select Case Target37Step

		Case 1:pSW37.TransY = -1
		Case 2:pSW37.TransY = 2
        Case 3:pSW37.TransY = 0:Me.TimerEnabled = 0
     End Select
	Target37Step = Target37Step + 1
End Sub	



'*****************
'Animated rubbers
'*****************
Sub wall69_Hit:vpmTimer.PulseSw 100:rubber25.visible = 0::rubber25a.visible = 1:wall69.timerenabled = 1:End Sub
Sub wall69_timer:rubber25.visible = 1::rubber25a.visible = 0: wall69.timerenabled= 0:End Sub

Sub wall75_Hit:vpmTimer.PulseSw 100:rubber24.visible = 0::rubber24a.visible = 1:wall75.timerenabled = 1:End Sub
Sub wall75_timer:rubber24.visible = 1::rubber24a.visible = 0: wall75.timerenabled= 0:End Sub

Sub wall77_Hit:vpmTimer.PulseSw 100:rubber23.visible = 0::rubber23a.visible = 1:wall77.timerenabled = 1:End Sub
Sub wall77_timer:rubber23.visible = 1::rubber23a.visible = 0: wall77.timerenabled= 0:End Sub

Sub wall79_Hit:vpmTimer.PulseSw 100:rubber22.visible = 0::rubber22a.visible = 1:wall79.timerenabled = 1:End Sub
Sub wall79_timer:rubber22.visible = 1::rubber22a.visible = 0: wall79.timerenabled= 0:End Sub

Sub wall80_Hit:vpmTimer.PulseSw 100:rubber21.visible = 0::rubber21a.visible = 1:wall80.timerenabled = 1:End Sub
Sub wall80_timer:rubber21.visible = 1::rubber21a.visible = 0: wall80.timerenabled= 0:End Sub

Sub wall81_Hit:vpmTimer.PulseSw 100:rubber20.visible = 0::rubber20a.visible = 1:wall81.timerenabled = 1:End Sub
Sub wall81_timer:rubber20.visible = 1::rubber20a.visible = 0: wall81.timerenabled= 0:End Sub

Sub wall82_Hit:vpmTimer.PulseSw 100:rubber3.visible = 0::rubber3a.visible = 1:wall82.timerenabled = 1:End Sub
Sub wall82_timer:rubber3.visible = 1::rubber3a.visible = 0: wall82.timerenabled= 0:End Sub

Sub wall83_Hit:vpmTimer.PulseSw 100:rubber14.visible = 0::rubber14a.visible = 1:wall83.timerenabled = 1:End Sub
Sub wall83_timer:rubber14.visible = 1::rubber14a.visible = 0: wall83.timerenabled= 0:End Sub

Sub wall84_Hit:vpmTimer.PulseSw 100:rubber19.visible = 0::rubber19a.visible = 1:wall84.timerenabled = 1:End Sub
Sub wall84_timer:rubber19.visible = 1::rubber19a.visible = 0: wall84.timerenabled= 0:End Sub

Sub LockWall_Hit():PlaysoundAt "collide0",sw72:End Sub

'Spinner
Sub sw51_Spin:vpmTimer.PulseSw 51:PlaySoundAtVol "spinner",sw51,VolSpin:End Sub


'Ball Drop
Sub RRail_Hit()
    StopRollingSound
    dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    if finalspeed > 2 AND finalspeed < 4 Then
        PlaySoundAt "rail_low_slower", RRail
    elseif finalspeed >= 4 Then
        PlaySoundAt "rail", RRail
    End if
End Sub

Sub LRail_Hit()
    StopRollingSound
    dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    if finalspeed > 2 AND finalspeed < 4 Then
        PlaySoundAt "rail_low_slower", LRail
    elseif finalspeed >= 4 Then
        PlaySoundAt "rail", LRail
    End if
End Sub

Sub RubbersPosts_Hit(idx)
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 20 then
        PlaySound "fx_rubber", 0, VolRub * Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End if
    If finalspeed >= 6 AND finalspeed <= 20 then
        RandomSoundRubber()
    End If
End Sub

Sub RubbersRingsSleeves_Hit(idx)
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 20 then
        PlaySound "fx_rubber", 0, VolRub * Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End if
    If finalspeed >= 6 AND finalspeed <= 20 then
        RandomSoundRubber()
    End If
End Sub

Sub RubbersWalls_Hit(idx)
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 20 then
        PlaySound "fx_rubber", 0, VolRub * Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End if
    If finalspeed >= 6 AND finalspeed <= 20 then
        RandomSoundRubber()
    End If
End Sub

Sub Skillrubbers_Hit(idx)
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 20 then
        PlaySound "fx_rubber", 0, VolRub * Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End if
    If finalspeed >= 6 AND finalspeed <= 20 then
        RandomSoundRubber()
    End If
End Sub

Sub Primitive16_Hit(idx)
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 20 then
        PlaySound "fx_rubber", 0, VolRub * Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End if
    If finalspeed >= 6 AND finalspeed <= 20 then
        RandomSoundRubber()
    End If
End Sub
'
Sub Primitive62_Hit(idx)
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 20 then
        PlaySound "fx_rubber", 0, VolRub * Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End if
    If finalspeed >= 6 AND finalspeed <= 20 then
        RandomSoundRubber()
    End If
End Sub

'Sub Primitive16_Hit:Playsound "fx_rubber_hit_2":End Sub
'Sub Primitive62_Hit:Playsound "fx_rubber_hit_2":End Sub


Sub RandomSoundRubber()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "fx_rubberhit1", 0, VolRub * Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 2:PlaySound "fx_rubberhit2", 0, VolRub * Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 3:PlaySound "fx_rubberhit3", 0, VolRub * Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "fx_fliphit_1", 0, VolFlip * Vol(ActiveBall)*10, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "fx_fliphit_2", 0, VolFlip * Vol(ActiveBall)*10, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "fx_fliphit_3", 0, VolFlip * Vol(ActiveBall)*10, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub




'**************
'***Lighting***
'**************
Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
Dim ModLampz : Set ModLampz = New DynamicLamps

Sub LampTimer_Timer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update2	'update (Both fading and object updates)
	ModLampz.Update2
End Sub


'Material swap arrays.

'GI Color Mod  Arrays
Dim MaterialWhiteArray: MaterialWhiteArray = Array("BulbGIOff", "BulbGIOff","BulbGIOff","BulbGIOn")
Dim MaterialBlueArray: MaterialBlueArray = Array("BulbBlueOff", "BulbBlueOff","BulbBlueOff","BulbBlueOn")
Dim MaterialPurpleArray: MaterialPurpleArray = Array("BulbPurpleOff", "BulbPurpleOff","BulbPurpleOff","BulbPurpleOn")
Dim MaterialEyesArray: MaterialEyesArray = Array("BulbEyesOff", "BulbEyesOff","BulbEyesOff","BulbGIOn")


'Colored Bulb Mod  Arrays
Dim MaterialRedArray: MaterialRedArray = Array("BulbRedOff", "BulbRedOff","BulbRedOff","BulbRedOn")
Dim MaterialLTBlueArray: MaterialLTBlueArray = Array("BulbLTBlueOff", "BulbLTBlueOff","BulbLTBlueOff","BulbLTBlueOn")
Dim MaterialFrostedLTBlueArray: MaterialFrostedLTBlueArray = Array("BulbFrostedLTBlueOff", "BulbFrostedLTBlueOff","BulbFrostedLTBlueOff","BulbFrostedLTBlueOn")
Dim MaterialClearBumperArray: MaterialClearBumperArray = Array("BumperClearOff", "BumperClearOff","BumperClearOff","BumperClearOn")
Dim MaterialBlueBumperArray: MaterialBlueBumperArray = Array("BumperBlueOff", "BumperBlueOff","BumperBlueOff","BumperBlueOn")
Dim MaterialRedBumperArray: MaterialRedBumperArray = Array("BumperRedOff", "BumperRedOff","BumperRedOff","BumperRedOn")
Dim DomeRedArray: DomeRedArray = Array("DomeRedOff", "DomeRedOff", "DomeRedOff", "DomeRedOn")





'***Material Swap***
'Fade material for colored Bulb prims
Sub FadeMaterialColoredBulb(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 4)
		Case 1:pri.Material = group(0) 'Off
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
        Case 4:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * .03 'Intensity Adjustment 
End Sub


'Fade material for colored bulb Filiment prims
Sub FadeMaterialColoredFilament(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 4)
		Case 1:pri.Material = group(0) 'Off
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
        Case 4:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * 20  'Intensity Adjustment 
End Sub


'Fade material for GI bulb prims
Sub FadeMaterialGIBulb(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 4)
		Case 1:pri.Material = group(0) 'Off
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
        Case 4:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * .03  'Intensity Adjustment 
End Sub


'Fade material for GI Filiment prims
Sub FadeMaterialGIFiliment(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 4)
		Case 1:pri.Material = group(0) 'Off
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
        Case 4:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * 5  'Intensity Adjustment
End Sub


'Fade material for EYES bulb prims
Sub FadeMaterialEyesBulb(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 4)
		Case 1:pri.Material = group(0) 'Off
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
        Case 4:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * .03  'Intensity Adjustment 
End Sub


'Fade material for EYES FROSTED bulb prims
Sub FadeMaterialEyesFrostedBulb(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 4)
		Case 1:pri.Material = group(0) 'Off
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
        Case 4:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * 5  'Intensity Adjustment 
End Sub


'Fade material for EYES Filiment prims
Sub FadeMaterialEyesFiliment(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 4)
		Case 1:pri.Material = group(0) 'Off
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
        Case 4:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * 4  'Intensity Adjustment
End Sub


'Fade material for MOUTH bulb prims
Sub FadeMaterialMouthBulb(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 4)
		Case 1:pri.Material = group(0) 'Off
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
        Case 4:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * .05  'Intensity Adjustment 
End Sub


'Fade material for MOUTH Filiment prims
Sub FadeMaterialMouthFiliment(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 4)
		Case 1:pri.Material = group(0) 'Off
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
        Case 4:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * 2  'Intensity Adjustment
End Sub


'Fade material for HELMET bulb prims
Sub FadeMaterialHelmetBulb(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 4)
		Case 1:pri.Material = group(0) 'Off
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
        Case 4:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * .03  'Intensity Adjustment 
End Sub


'Fade material for HELMET Filiment prims
Sub FadeMaterialHelmetFiliment(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 4)
		Case 1:pri.Material = group(0) 'Off
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
        Case 4:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * 7  'Intensity Adjustment
End Sub


'DOME Primitive fading script
Sub FadeDome(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 4)
		Case 1:pri.material = group(0) 'Off
		Case 2:pri.material = group(1) 'Fading...
		Case 3:pri.material = group(2) 'Fading...
        Case 4:pri.material = group(3) 'Full
    End Select
pri.blenddisablelighting = aLvl * 100  'Intensity Adjustment for prim bulb
End Sub

'***End Material swap scripting***




Sub InitLampsNF()
	lampz.filter = "TestFunction"
	Modlampz.filter = "TestFunction"
	dim x
	for x = 0 to 140	'Fading Speeds
		Lampz.FadeSpeedUp(x) = 1/80
		Lampz.FadeSpeedDown(x) = 1/120
	Next
	for x = 5 to 28
		ModLampz.FadeSpeedUp(x) = 1/64
		ModLampz.FadeSpeedDown(x) = 1/64
	Next


	'Lamp Assignments
	Lampz.MassAssign(11)= l11
	Lampz.MassAssign(11)= l11a
	Lampz.Callback(11) = "PlayMusic"
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign(13)= l13
	Lampz.MassAssign(14)= l14
	Lampz.MassAssign(12)= l12a
	Lampz.MassAssign(13)= l13a
	Lampz.MassAssign(14)= l14a
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(15)= l15a
	Lampz.MassAssign(16)= l16
	Lampz.MassAssign(16)= l16a
	Lampz.MassAssign(17)= l17
	Lampz.MassAssign(17)= l17a
	Lampz.MassAssign(18)= l18
	Lampz.MassAssign(18)= l18a
	Lampz.MassAssign(21)= l21
	Lampz.MassAssign(21)= l21a
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(22)= l22a
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign(23)= l23a
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(24)= l24a
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(25)= l25a
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= l26a
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign(27)= l27a
	Lampz.MassAssign(28)= l28
	Lampz.MassAssign(28)= l28a
	Lampz.MassAssign(31)= l31
	Lampz.MassAssign(31)= l31a
	Lampz.MassAssign(32)= l32
	Lampz.MassAssign(32)= l32a
	Lampz.MassAssign(33)= l33
	Lampz.MassAssign(33)= l33a
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(34)= l34a
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(35)= l35a
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(36)= l36a

	Lampz.MassAssign(37)= l37
	Lampz.MassAssign(37)= l37a
	Lampz.MassAssign(37)= l37b
	Lampz.MassAssign(37)= l37a
	If HLColorType = 0 then
		Lampz.Callback(37) =         "FadeMaterialHelmetBulb pBulb37, MaterialWhiteArray, "
		Lampz.Callback(37) = "FadeMaterialHelmetFiliment pFiliment37, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(37) =         "FadeMaterialHelmetBulb pBulb37, MaterialLTBlueArray, "
		Lampz.Callback(37) = "FadeMaterialHelmetFiliment pFiliment37, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(37) =         "FadeMaterialEyesFrostedBulb pBulb37, MaterialFrostedLTBlueArray, "
		Lampz.Callback(37) = "FadeMaterialHelmetFiliment pFiliment37, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(37) =         "FadeMaterialHelmetBulb pBulb37, MaterialWhiteArray, "
		Lampz.Callback(37) = "FadeMaterialHelmetFiliment pFiliment37, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(38)= l38a
	Lampz.MassAssign(38)= l38b
	Lampz.MassAssign(38)= l38c
	Lampz.MassAssign(38)= l38d
	Lampz.MassAssign(38)= l38e
	Lampz.MassAssign(38)= l38f
	Lampz.MassAssign(38)= l38g
	Lampz.MassAssign(38)= l38h
	Lampz.MassAssign(38)= l38i
	Lampz.MassAssign(38)= l38j
	Lampz.MassAssign(38)= l38k
	Lampz.MassAssign(38)= l38l
	Lampz.MassAssign(38)= l38m
	Lampz.MassAssign(38)= l38n
	Lampz.MassAssign(38)= l38


	Lampz.MassAssign(41)= L41
	Lampz.MassAssign(41)= l41c
	Lampz.Callback(41) =         "FadeMaterialColoredBulb pBulb41, MaterialWhiteArray, "
	Lampz.Callback(41) = "FadeMaterialColoredFilament pFilament41, MaterialWhiteArray, "


	Lampz.MassAssign(42)= L42
	Lampz.MassAssign(42)= l42c
	Lampz.Callback(42) =         "FadeMaterialColoredBulb pBulb42, MaterialWhiteArray, "
	Lampz.Callback(42) = "FadeMaterialColoredFilament pFilament42, MaterialWhiteArray, "


	Lampz.MassAssign(43)= L43
	Lampz.MassAssign(43)= l43c
	Lampz.Callback(43) =         "FadeMaterialColoredBulb pBulb43, MaterialWhiteArray, "
	Lampz.Callback(43) = "FadeMaterialColoredFilament pFilament43, MaterialWhiteArray, "


	Lampz.MassAssign(44)= L44
	Lampz.MassAssign(44)= l44c
	Lampz.Callback(44) =         "FadeMaterialColoredBulb pBulb44, MaterialWhiteArray, "
	Lampz.Callback(44) = "FadeMaterialColoredFilament pFilament44, MaterialWhiteArray, "


	Lampz.MassAssign(45)= L45
	Lampz.MassAssign(45)= l45c
	Lampz.Callback(45) =         "FadeMaterialColoredBulb pBulb45, MaterialWhiteArray, "
	Lampz.Callback(45) = "FadeMaterialColoredFilament pFilament45, MaterialWhiteArray, "


	Lampz.MassAssign(46)= l46
	Lampz.Callback(46) =         "FadeMaterialEyesBulb pBulb46, MaterialEyesArray, "
	Lampz.Callback(46) = "FadeMaterialEyesFiliment pFiliment46, MaterialEyesArray, "

	Lampz.MassAssign(47)= l47
	Lampz.Callback(47) =         "FadeMaterialEyesBulb pBulb47, MaterialEyesArray, "
	Lampz.Callback(47) = "FadeMaterialEyesFiliment pFiliment47, MaterialEyesArray, "

	Lampz.MassAssign(48)= l48
	Lampz.MassAssign(48)= l48a
	Lampz.Callback(48) =         "FadeMaterialMouthBulb pBulb48, MaterialRedArray, "
	Lampz.Callback(48) = "FadeMaterialMouthFiliment pFiliment48, MaterialRedArray, "

	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= l51a
	Lampz.MassAssign(52)= l52
	Lampz.MassAssign(52)= l52a
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= l53a
	Lampz.MassAssign(54)= l54
	Lampz.MassAssign(54)= l54a
	Lampz.MassAssign(55)= l55
	Lampz.MassAssign(55)= l55a
	Lampz.MassAssign(55)= l55b
	Lampz.MassAssign(55)= l55c
	Lampz.MassAssign(56)= l56
	Lampz.MassAssign(56)= l56a
	Lampz.MassAssign(57)= l57
	Lampz.MassAssign(57)= l57a
	Lampz.MassAssign(58)= l58
	Lampz.MassAssign(58)= l58a
	Lampz.MassAssign(61)= l61
	Lampz.MassAssign(61)= l61a
	Lampz.MassAssign(62)= l62
	Lampz.MassAssign(62)= l62a
	Lampz.MassAssign(63)= l63
	Lampz.MassAssign(63)= l63a
	Lampz.MassAssign(64)= F64a
	Lampz.MassAssign(64)= F64b
	Lampz.MassAssign(86)= f86
	Lampz.MassAssign(87)= f87
	Lampz.MassAssign(88)= f88
	Lampz.MassAssign(65)= l65
	Lampz.MassAssign(65)= l65a
	Lampz.MassAssign(66)= l66
	Lampz.MassAssign(66)= l66a
	Lampz.MassAssign(67)= l67
	Lampz.MassAssign(67)= l67a
	Lampz.MassAssign(68)= l68
	Lampz.MassAssign(68)= l68a

'***Helmet Lights***

	Lampz.MassAssign(91)= l91
	If HLColorType = 0 then
		Lampz.Callback(91) =         "FadeMaterialHelmetBulb pBulb91, MaterialWhiteArray, "
		Lampz.Callback(91) = "FadeMaterialHelmetFiliment pFiliment91, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(91) =         "FadeMaterialHelmetBulb pBulb91, MaterialLTBlueArray, "
		Lampz.Callback(91) = "FadeMaterialHelmetFiliment pFiliment91, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(91) =         "FadeMaterialEyesFrostedBulb pBulb91, MaterialFrostedLTBlueArray, "
		Lampz.Callback(91) = "FadeMaterialHelmetFiliment pFiliment91, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(91) =         "FadeMaterialHelmetBulb pBulb91, MaterialWhiteArray, "
		Lampz.Callback(91) = "FadeMaterialHelmetFiliment pFiliment91, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(92)= l92
	If HLColorType = 0 then
		Lampz.Callback(92) =         "FadeMaterialHelmetBulb pBulb92, MaterialWhiteArray, "
		Lampz.Callback(92) = "FadeMaterialHelmetFiliment pFiliment92, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(92) =         "FadeMaterialHelmetBulb pBulb92, MaterialLTBlueArray, "
		Lampz.Callback(92) = "FadeMaterialHelmetFiliment pFiliment92, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(92) =         "FadeMaterialEyesFrostedBulb pBulb92, MaterialFrostedLTBlueArray, "
		Lampz.Callback(92) = "FadeMaterialHelmetFiliment pFiliment92, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(92) =         "FadeMaterialHelmetBulb pBulb92, MaterialWhiteArray, "
		Lampz.Callback(92) = "FadeMaterialHelmetFiliment pFiliment92, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(93)= l93
	If HLColorType = 0 then
		Lampz.Callback(93) =         "FadeMaterialHelmetBulb pBulb93, MaterialWhiteArray, "
		Lampz.Callback(93) = "FadeMaterialHelmetFiliment pFiliment93, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(93) =         "FadeMaterialHelmetBulb pBulb93, MaterialLTBlueArray, "
		Lampz.Callback(93) = "FadeMaterialHelmetFiliment pFiliment93, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(93) =         "FadeMaterialEyesFrostedBulb pBulb93, MaterialFrostedLTBlueArray, "
		Lampz.Callback(93) = "FadeMaterialHelmetFiliment pFiliment93, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(93) =         "FadeMaterialHelmetBulb pBulb93, MaterialWhiteArray, "
		Lampz.Callback(93) = "FadeMaterialHelmetFiliment pFiliment93, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(94)= l94
	If HLColorType = 0 then
		Lampz.Callback(94) =         "FadeMaterialHelmetBulb pBulb94, MaterialWhiteArray, "
		Lampz.Callback(94) = "FadeMaterialHelmetFiliment pFiliment94, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(94) =         "FadeMaterialHelmetBulb pBulb94, MaterialLTBlueArray, "
		Lampz.Callback(94) = "FadeMaterialHelmetFiliment pFiliment94, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(94) =         "FadeMaterialEyesFrostedBulb pBulb94, MaterialFrostedLTBlueArray, "
		Lampz.Callback(94) = "FadeMaterialHelmetFiliment pFiliment94, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(94) =         "FadeMaterialHelmetBulb pBulb94, MaterialWhiteArray, "
		Lampz.Callback(94) = "FadeMaterialHelmetFiliment pFiliment94, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(95)= l95
	If HLColorType = 0 then
		Lampz.Callback(95) =         "FadeMaterialHelmetBulb pBulb95, MaterialWhiteArray, "
		Lampz.Callback(95) = "FadeMaterialHelmetFiliment pFiliment95, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(95) =         "FadeMaterialHelmetBulb pBulb95, MaterialLTBlueArray, "
		Lampz.Callback(95) = "FadeMaterialHelmetFiliment pFiliment95, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(95) =         "FadeMaterialEyesFrostedBulb pBulb95, MaterialFrostedLTBlueArray, "
		Lampz.Callback(95) = "FadeMaterialHelmetFiliment pFiliment95, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType =3 then
		'Lampz.Callback(95) =         "FadeMaterialHelmetBulb pBulb95, MaterialWhiteArray, "
		Lampz.Callback(95) = "FadeMaterialHelmetFiliment pFiliment95, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(96)= l96
	If HLColorType = 0 then
		Lampz.Callback(96) =         "FadeMaterialHelmetBulb pBulb96, MaterialWhiteArray, "
		Lampz.Callback(96) = "FadeMaterialHelmetFiliment pFiliment96, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(96) =         "FadeMaterialHelmetBulb pBulb96, MaterialLTBlueArray, "
		Lampz.Callback(96) = "FadeMaterialHelmetFiliment pFiliment96, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(96) =         "FadeMaterialEyesFrostedBulb pBulb96, MaterialFrostedLTBlueArray, "
		Lampz.Callback(96) = "FadeMaterialHelmetFiliment pFiliment96, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(96) =         "FadeMaterialHelmetBulb pBulb96, MaterialWhiteArray, "
		Lampz.Callback(96) = "FadeMaterialHelmetFiliment pFiliment96, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(97)= l97
	If HLColorType = 0 then
		Lampz.Callback(97) =         "FadeMaterialHelmetBulb pBulb97, MaterialWhiteArray, "
		Lampz.Callback(97) = "FadeMaterialHelmetFiliment pFiliment97, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(97) =         "FadeMaterialHelmetBulb pBulb97, MaterialLTBlueArray, "
		Lampz.Callback(97) = "FadeMaterialHelmetFiliment pFiliment97, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(97) =         "FadeMaterialEyesFrostedBulb pBulb97, MaterialFrostedLTBlueArray, "
		Lampz.Callback(97) = "FadeMaterialHelmetFiliment pFiliment97, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(97) =         "FadeMaterialHelmetBulb pBulb97, MaterialWhiteArray, "
		Lampz.Callback(97) = "FadeMaterialHelmetFiliment pFiliment97, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(98)= l98
	If HLColorType = 0 then
		Lampz.Callback(98) =         "FadeMaterialHelmetBulb pBulb98, MaterialWhiteArray, "
		Lampz.Callback(98) = "FadeMaterialHelmetFiliment pFiliment98, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(98) =         "FadeMaterialHelmetBulb pBulb98, MaterialLTBlueArray, "
		Lampz.Callback(98) = "FadeMaterialHelmetFiliment pFiliment98, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(98) =         "FadeMaterialEyesFrostedBulb pBulb98, MaterialFrostedLTBlueArray, "
		Lampz.Callback(98) = "FadeMaterialHelmetFiliment pFiliment98, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(98) =         "FadeMaterialHelmetBulb pBulb98, MaterialWhiteArray, "
		Lampz.Callback(98) = "FadeMaterialHelmetFiliment pFiliment98, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(101)= l101
	If HLColorType = 0 then
		Lampz.Callback(101) =         "FadeMaterialHelmetBulb pBulb101, MaterialWhiteArray, "
		Lampz.Callback(101) = "FadeMaterialHelmetFiliment pFiliment101, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(101) =         "FadeMaterialHelmetBulb pBulb101, MaterialLTBlueArray, "
		Lampz.Callback(101) = "FadeMaterialHelmetFiliment pFiliment101, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(101) =         "FadeMaterialEyesFrostedBulb pBulb101, MaterialFrostedLTBlueArray, "
		Lampz.Callback(101) = "FadeMaterialHelmetFiliment pFiliment101, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(101) =         "FadeMaterialHelmetBulb pBulb101, MaterialWhiteArray, "
		Lampz.Callback(101) = "FadeMaterialHelmetFiliment pFiliment101, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(102)= l102
	If HLColorType = 0 then
		Lampz.Callback(102) =         "FadeMaterialHelmetBulb pBulb102, MaterialWhiteArray, "
		Lampz.Callback(102) = "FadeMaterialHelmetFiliment pFiliment102, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(102) =         "FadeMaterialHelmetBulb pBulb102, MaterialLTBlueArray, "
		Lampz.Callback(102) = "FadeMaterialHelmetFiliment pFiliment102, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(102) =         "FadeMaterialEyesFrostedBulb pBulb102, MaterialFrostedLTBlueArray, "
		Lampz.Callback(102) = "FadeMaterialHelmetFiliment pFiliment102, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(102) =         "FadeMaterialHelmetBulb pBulb102, MaterialWhiteArray, "
		Lampz.Callback(102) = "FadeMaterialHelmetFiliment pFiliment102, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(103)= l103
	If HLColorType = 0 then
		Lampz.Callback(103) =         "FadeMaterialHelmetBulb pBulb103, MaterialWhiteArray, "
		Lampz.Callback(103) = "FadeMaterialHelmetFiliment pFiliment103, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(103) =         "FadeMaterialHelmetBulb pBulb103, MaterialLTBlueArray, "
		Lampz.Callback(103) = "FadeMaterialHelmetFiliment pFiliment103, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(103) =         "FadeMaterialEyesFrostedBulb pBulb103, MaterialFrostedLTBlueArray, "
		Lampz.Callback(103) = "FadeMaterialHelmetFiliment pFiliment103, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(103) =         "FadeMaterialHelmetBulb pBulb103, MaterialWhiteArray, "
		Lampz.Callback(103) = "FadeMaterialHelmetFiliment pFiliment103, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(104)= l104
	If HLColor = 0 then
		Lampz.Callback(104) =         "FadeMaterialHelmetBulb pBulb104, MaterialWhiteArray, "
		Lampz.Callback(104) = "FadeMaterialHelmetFiliment pFiliment104, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(104) =         "FadeMaterialHelmetBulb pBulb104, MaterialLTBlueArray, "
		Lampz.Callback(104) = "FadeMaterialHelmetFiliment pFiliment104, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(104) =         "FadeMaterialEyesFrostedBulb pBulb104, MaterialFrostedLTBlueArray, "
		Lampz.Callback(104) = "FadeMaterialHelmetFiliment pFiliment104, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(104) =         "FadeMaterialHelmetBulb pBulb104, MaterialWhiteArray, "
		Lampz.Callback(104) = "FadeMaterialHelmetFiliment pFiliment104, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(105)= l105
	If HLColorType = 0 then
		Lampz.Callback(105) =         "FadeMaterialHelmetBulb pBulb105, MaterialWhiteArray, "
		Lampz.Callback(105) = "FadeMaterialHelmetFiliment pFiliment105, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(105) =         "FadeMaterialHelmetBulb pBulb105, MaterialLTBlueArray, "
		Lampz.Callback(105) = "FadeMaterialHelmetFiliment pFiliment105, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(105) =         "FadeMaterialEyesFrostedBulb pBulb105, MaterialFrostedLTBlueArray, "
		Lampz.Callback(105) = "FadeMaterialHelmetFiliment pFiliment105, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(105) =         "FadeMaterialHelmetBulb pBulb105, MaterialWhiteArray, "
		Lampz.Callback(105) = "FadeMaterialHelmetFiliment pFiliment105, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(106)= l106
	If HLColorType = 0 then
		Lampz.Callback(106) =         "FadeMaterialHelmetBulb pBulb106, MaterialWhiteArray, "
		Lampz.Callback(106) = "FadeMaterialHelmetFiliment pFiliment106, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(106) =         "FadeMaterialHelmetBulb pBulb106, MaterialLTBlueArray, "
		Lampz.Callback(106) = "FadeMaterialHelmetFiliment pFiliment106, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(106) =         "FadeMaterialEyesFrostedBulb pBulb106, MaterialFrostedLTBlueArray, "
		Lampz.Callback(106) = "FadeMaterialHelmetFiliment pFiliment106, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(106) =         "FadeMaterialHelmetBulb pBulb106, MaterialWhiteArray, "
		Lampz.Callback(106) = "FadeMaterialHelmetFiliment pFiliment106, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(107)= l107
	If HLColorType = 0 then
		Lampz.Callback(107) =         "FadeMaterialHelmetBulb pBulb107, MaterialWhiteArray, "
		Lampz.Callback(107) = "FadeMaterialHelmetFiliment pFiliment107, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(107) =         "FadeMaterialHelmetBulb pBulb107, MaterialLTBlueArray, "
		Lampz.Callback(107) = "FadeMaterialHelmetFiliment pFiliment107, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(107) =         "FadeMaterialEyesFrostedBulb pBulb107, MaterialFrostedLTBlueArray, "
		Lampz.Callback(107) = "FadeMaterialHelmetFiliment pFiliment107, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(107) =         "FadeMaterialHelmetBulb pBulb107, MaterialWhiteArray, "
		Lampz.Callback(107) = "FadeMaterialHelmetFiliment pFiliment107, MaterialWhiteArray, "
	End if

	Lampz.MassAssign(108)= l108
	If HLColorType = 0 then
		Lampz.Callback(108) =         "FadeMaterialHelmetBulb pBulb108, MaterialWhiteArray, "
		Lampz.Callback(108) = "FadeMaterialHelmetFiliment pFiliment108, MaterialWhiteArray, "
	End if
	If HLColorType = 1 then
		Lampz.Callback(108) =         "FadeMaterialHelmetBulb pBulb108, MaterialLTBlueArray, "
		Lampz.Callback(108) = "FadeMaterialHelmetFiliment pFiliment108, MaterialLTBlueArray, "
	End if
	If HLColorType = 2 then
		Lampz.Callback(108) =         "FadeMaterialEyesFrostedBulb pBulb108, MaterialFrostedLTBlueArray, "
		Lampz.Callback(108) = "FadeMaterialHelmetFiliment pFiliment108, MaterialFrostedLTBlueArray, "
	End if
	If HLColorType = 3 then
		'Lampz.Callback(108) =         "FadeMaterialHelmetBulb pBulb108, MaterialWhiteArray, "
		Lampz.Callback(108) = "FadeMaterialHelmetFiliment pFiliment108, MaterialWhiteArray, "
	End if



	'Flashers
	ModLampz.MassAssign(17)= f17   'Billions
	ModLampz.MassAssign(17)= f17a  'Billions
	ModLampz.MassAssign(18)= f18   'Left ramp
	ModLampz.MassAssign(19)= f19   'jackpot
	ModLampz.MassAssign(19)= f19a  'jackpot
	ModLampz.MassAssign(20)= L120  'SkillShot
  	MODLampz.Callback(21) = "FadeDome pDome21On, DomeRedArray,"
	ModLampz.MassAssign(21)= UpperLeftBloom
	ModLampz.MassAssign(21)= UpperLeftBloom1
	ModLampz.MassAssign(21)= UpperLeftBloom2
	ModLampz.MassAssign(21)= UpperLeftWallGlare
	ModLampz.MassAssign(21)= Dome21Bloom

	MODLampz.Callback(22) = "FadeDome pDome22On, DomeRedArray,"
	ModLampz.MassAssign(22)= UpperRightBloom
	ModLampz.MassAssign(22)= UpperRightBloom1
	ModLampz.MassAssign(22)= UpperRightBloom2
	ModLampz.MassAssign(22)= UpperRightWallGlare
	ModLampz.MassAssign(22)= Dome22Bloom

	MODLampz.Callback(23) = "FadeDome pDome23On, DomeRedArray,"
	MODLampz.Callback(23) = "FadeDome pBulb23On, DomeRedArray,"
	ModLampz.MassAssign(23)= LowerRightBloom
	ModLampz.MassAssign(23)= LowerRightBloom1
	ModLampz.MassAssign(23)= LowerRightBloom2
	ModLampz.MassAssign(23)= LowerRightWallGlare
	ModLampz.MassAssign(23)= F23SB1
	ModLampz.MassAssign(23)= F23SB2
	ModLampz.MassAssign(23)= Dome23Bloom

	ModLampz.Callback(24) = "FadeDome pDome24On, DomeRedArray,"
	ModLampz.Callback(24) = "FadeDome pBulb24On, DomeRedArray,"
	ModLampz.MassAssign(24)= LowerLeftBloom
	ModLampz.MassAssign(24)= LowerLeftBloom1
	ModLampz.MassAssign(24)= LowerLeftBloom2
	ModLampz.MassAssign(24)= LowerLeftWallGlare
	ModLampz.MassAssign(24)= F24SB1
	ModLampz.MassAssign(24)= F24SB2
	ModLampz.MassAssign(24)= LowerLeftGlare
	ModLampz.MassAssign(24)= Dome24Bloom


'***Shuttle Mod***
	If ShipMod = 1 then
		ModLampz.MassAssign(24)= F124c
		ModLampz.MassAssign(24)= F124d
		ModLampz.MassAssign(24)= F124e
		ModLampz.MassAssign(24)= F124f
		ModLampz.MassAssign(24)= F124g
		ModLampz.MassAssign(24)= F124h
	End If


'***GI Assignments***

'***GI Rear***
    ModLampz.MassAssign(2)= ColToArray(GI_Rear)
	ModLampz.Callback(2) = "GIUpdates"
	ModLampz.state(2) = 1	'Start GI on

	If GIColorModType = 0 then
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb10, MaterialWhiteArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment10, MaterialWhiteArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb11, MaterialWhiteArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment11, MaterialWhiteArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb12, MaterialWhiteArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment12, MaterialWhiteArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb13, MaterialWhiteArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment13, MaterialWhiteArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb14, MaterialWhiteArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment14, MaterialWhiteArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb15, MaterialWhiteArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment15, MaterialWhiteArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBumperBulb1, MaterialRedArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pBumperFilament1, MaterialRedArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBumperBulb2, MaterialRedArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pBumperFilament2, MaterialRedArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBumperBulb3, MaterialRedArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pBumperFilament3, MaterialRedArray, "
	End If

	If GIColorModType = 1 then
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb10, MaterialBlueArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment10, MaterialBlueArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb11, MaterialBlueArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment11, MaterialBlueArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb12, MaterialBlueArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment12, MaterialBlueArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb13, MaterialBlueArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment13, MaterialBlueArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb14, MaterialBlueArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment14, MaterialBlueArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb15, MaterialBlueArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment15, MaterialBlueArray, "
	End If


	If GIColorModType = 2 then
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb10, MaterialPurpleArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment10, MaterialPurpleArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb11, MaterialPurpleArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment11, MaterialPurpleArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb12, MaterialPurpleArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment12, MaterialPurpleArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb13, MaterialPurpleArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment13, MaterialPurpleArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb14, MaterialPurpleArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment14, MaterialPurpleArray, "
		ModLampz.Callback(2) =        "FadeMaterialGIBulb pBulb15, MaterialPurpleArray, "
		ModLampz.Callback(2) = "FadeMaterialGIFiliment pFiliment15, MaterialPurpleArray, "
	End If


'***GI Front***
	ModLampz.MassAssign(4)= ColToArray(GI_Front)
	ModLampz.Callback(4) = "GIUpdates"
	ModLampz.state(4) = 1	'Start GI on
	ModLampz.Init	'Starts states on and initializes flasher images so they don't stutter on first appearance
	Lampz.Init


	If GIColorModType = 0 then
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb1, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment1, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb2, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment2, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb3, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment3, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb4, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment4, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb5, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment5, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb6, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment6, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb7, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment7, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb8, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment8, MaterialWhiteArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb9, MaterialWhiteArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment9, MaterialWhiteArray, "



	End If

	If GIColorModType = 1 then
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb1, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment1, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb2, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment2, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb3, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment3, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb4, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment4, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb5, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment5, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb6, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment6, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb7, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment7, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb8, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment8, MaterialBlueArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb9, MaterialBlueArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment9, MaterialBlueArray, "

	End If

	If GIColorModType = 2 then
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb1, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment1, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb2, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment2, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb3, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment3, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb4, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment4, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb5, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment5, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb6, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment6, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb7, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment7, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb8, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment8, MaterialPurpleArray, "
		ModLampz.Callback(4) =        "FadeMaterialGIBulb pBulb9, MaterialPurpleArray, "
		ModLampz.Callback(4) = "FadeMaterialGIFiliment pFiliment9, MaterialPurpleArray, "

	End If
End Sub

'***End GI***


function FlashLevelToIndex(Input, MaxSize)
	'FlashLevelToIndex = cInt(Input * (MaxSize-1)+.5)+1
     FlashLevelToIndex = cInt(MaxSize * Input)
end function


Sub FadeDisableLighting1(aObject, ByVal aLvl)
	if Lampz.UseFunction then aLvl = lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	aObject.BlendDisableLighting = aLvl * 0.2
End Sub


Sub FadeMaterialP(itemp, group, ByVal aLvl)	'cp's script
	Select Case aLvl
		case 0 : itemp.Material = group(1)
		case 1 : itemp.Material = group(0)
	end select
End Sub


Function TestFunction(ByVal aLvl)
	TestFunction = aLvl^1.6
End Function

Sub PlayMusic(aLvl)
    'Intro Snippet
	If aLvl > 0 and PrevGameOver = 0 Then
		If MusicSnippet = 0 Then
			PlaySound "intro"
			PrevGameOver = 1
		End If
	else
'		PrevGameOver = 0
	End If
End Sub


Sub MouthLight(aObj, ByVal aLvl)
	If CurrentFace = 2 Then
		'if Lampz.UseFunction then aLvl = LampFilter(aLvl)
		aLvl = Lampz.FilterOut(aLvl)
		aObj.IntensityScale = aLvl
	Else
		aObj.Intensityscale = 0
	End If
End Sub


'GI callbacks

Dim GIoffMult : GIoffMult = 1.625 'adjust how bright the inserts get when the GI is off
Dim GIoffMultFlashers : GIoffMultFlashers = 1.3	'adjust how bright the Flashers get when the GI is off

Sub GIupdates(aLvl)
	'2 and 4 are the major PF gi circuits, averaging them together...
	dim giAvg
'	if Lampz.UseFunction then 	'Callbacks don't get this filter automatically
'		giAvg = (LampFilter(ModLampz.Lvl(2)) + LampFilter(ModLampz.Lvl(4)) )/2
'	Else
'		giAvg = (ModLampz.Lvl(2) + ModLampz.Lvl(4) )/2
'	end if
	giAvg = (Lampz.FilterOut(ModLampz.Lvl(2)) + Lampz.FilterOut(ModLampz.Lvl(4)) )/2

	'Lut Fading
	dim LutName, LutCount, GoLut
	LutName = "LutCont_"
	LutCount = 14
	GoLut = cInt(LutCount * giAvg	)'+1	'+1 if no 0 with these luts
	GoLut = LutName & GoLut + 13
	if Table.ColorGradeImage <> GoLut then Table.ColorGradeImage = GoLut ': 	tb.text = golut

	'Brighten inserts when GI is Low
	dim GIscale
	GiScale = (GIoffMult-1) * (ABS(giAvg-1 )  ) + 1	'invert
	dim x : for x = 0 to 140
		lampz.Modulate(x) = GiScale
	Next

	'Brighten Flashers when GI is low
	GiScale = (GIoffMultFlashers-1) * (ABS(giAvg-1 )  ) + 1	'invert
	for x = 5 to 28
		modlampz.modulate(x) = GiScale
	Next


End Sub

Sub GIon() : dim x : for x = 0 to 4 : modlampz.state(x) = 1 : next : end Sub 'debug
Sub GIoff() : dim x : for x = 0 to 4 : modlampz.state(x) = 0 : next : end Sub 'debug


'Helper functions

Function Luminance(aRgb)	'input: array, output: value between 0 and 255
	Luminance = (argb(0)*0.3 + argb(1)*0.59 + argb(2)*0.11)
End Function


Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function


'*******************************
'Intermediate Solenoid Procedures (Setlamp, etc)
'********************************
'Solenoid pipeline looks like this:
'Pinmame Controller -> UseSolenoids -> Solcallback -> intermediate subs (here) -> ModLampz dynamiclamps object -> object updates / more callbacks

'GI
'Pinmame Controller -> core.vbs PinMameTimer Loop -> GIcallback2 ->  ModLampz dynamiclamps object -> object updates / more callbacks
'(Can't even disable core.vbs's GI handling unless you deliberately set GIcallback & GIcallback2 to Empty)

'Lamps, for reference:
'Pinmame Controller -> LampTimer -> Lampz Fading Object -> Object Updates / callbacks

Set GICallback2 = GetRef("SetGI")

'    GI lights controlled by Strings
' 01 Backglass			'Case 0
' 02 Helmet				'Case 1
' 03 Rear Playfield		'Case 2
' 04 Backglass			'Case 3
' 05 Front Playfield	'Case 4

Sub SetGI(aNr, aValue)
	ModLampz.SetGI aNr, aValue 'Redundant. Could reassign GI indexes here
End Sub



'End lamps

'*************************************
'          Nudge System
' JP's based on Noah's nudgetest table
'*************************************

Dim LeftNudgeEffect, RightNudgeEffect, NudgeEffect

Sub LeftNudge(angle, strength, delay)
    vpmNudge.DoNudge angle, (strength * (delay-LeftNudgeEffect) / delay) + RightNudgeEffect / delay
    LeftNudgeEffect = delay
    RightNudgeEffect = 0
    RightNudgeTimer.Enabled = 0
    LeftNudgeTimer.Interval = delay
    LeftNudgeTimer.Enabled = 1
End Sub

Sub RightNudge(angle, strength, delay)
    vpmNudge.DoNudge angle, (strength * (delay-RightNudgeEffect) / delay) + LeftNudgeEffect / delay
    RightNudgeEffect = delay
    LeftNudgeEffect = 0
    LeftNudgeTimer.Enabled = 0
    RightNudgeTimer.Interval = delay
    RightNudgeTimer.Enabled = 1
End Sub

Sub CenterNudge(angle, strength, delay)
    vpmNudge.DoNudge angle, strength * (delay-NudgeEffect) / delay
    NudgeEffect = delay
    NudgeTimer.Interval = delay
    NudgeTimer.Enabled = 1
End Sub

Sub LeftNudgeTimer_Timer()
    LeftNudgeEffect = LeftNudgeEffect-1
    If LeftNudgeEffect = 0 then LeftNudgeTimer.Enabled = 0
End Sub

Sub RightNudgeTimer_Timer()
    RightNudgeEffect = RightNudgeEffect-1
    If RightNudgeEffect = 0 then RightNudgeTimer.Enabled = 0
End Sub

Sub NudgeTimer_Timer()
    NudgeEffect = NudgeEffect-1
    If NudgeEffect = 0 then NudgeTimer.Enabled = 0
End Sub

' ******************************************************************************************************************************************
'**********Sling Shot Animations
'****************
Dim Ustep

Sub UpperSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot_r",DOFContactors), slingu
    USling.Visible = 0
    USling1.Visible = 1
    slingu.TransZ = -28
    UStep = 0
    UpperSlingShot.TimerEnabled = 1
    vpmTimer.PulseSw 56
    'gi1.State = 0:Gi2.State = 0
End Sub

Sub UpperSlingShot_Timer
    Select Case UStep
        Case 1:USLing1.Visible = 0:USLing2.Visible = 1:slingu.transZ = -10
        Case 2:USLing2.Visible = 0:USLing.Visible = 1:slingu.transZ = 0:UpperSlingShot.TimerEnabled = 0:'gi1.State = 1:Gi2.State = 1
   End Select
    UStep = UStep + 1
End Sub

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot_l",DOFContactors),sling2:vpmTimer.PulseSw 57
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -28
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    vpmTimer.PulseSw 57
    'gi1.State = 0:Gi2.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 2:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:'gi1.State = 1:Gi2.State = 1
   End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot_r",DOFContactors),sling1:vpmTimer.PulseSw 58
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -28
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    vpmTimer.PulseSw 58
    'gi1.State = 0:Gi2.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 2:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:'gi1.State = 1:Gi2.State = 1
   End Select
    RStep = RStep + 1
End Sub

'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
Dim Digits(32)
 Digits(0)=Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
 Digits(1)=Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
 Digits(2)=Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
 Digits(3)=Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
 Digits(4)=Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
 Digits(5)=Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
 Digits(6)=Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
 Digits(7)=Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
 Digits(8)=Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
 Digits(9)=Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
 Digits(10)=Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
 Digits(11)=Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
 Digits(12)=Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
 Digits(13)=Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
 Digits(14)=Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
 Digits(15)=Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)
 Digits(16)=Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
 Digits(17)=Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
 Digits(18)=Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
 Digits(19)=Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
 Digits(20)=Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
 Digits(21)=Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
 Digits(22)=Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
 Digits(23)=Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
 Digits(24)=Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
 Digits(25)=Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
 Digits(26)=Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
 Digits(27)=Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
 Digits(28)=Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
 Digits(29)=Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
 Digits(30)=Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
 Digits(31)=Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)

 Sub DisplayTimer_Timer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
        If DesktopMode = True Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
            if (num < 32) then
              For Each obj In Digits(num)
                   If chg And 1 Then obj.State=stat And 1
                   chg=chg\2 : stat=stat\2
                  Next
            Else
                   end if
        Next
       end if
    End If
 End Sub
'**********************************************************************************************************
'**********************************************************************************************************


' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX
' PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "Table" is the name of the table
  Dim tmp
  tmp = tableobj.y * 2 / Table.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "Table" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / Table.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioFade(ball) ' Can this be together with the above function ?
  Dim tmp
  tmp = ball.y * 2 / Table.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function



'*****************************************
'    JP's VP10 Rolling Sounds
'*****************************************

Dim OnWireRamp
Const tnob = 7 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub CollisionTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_RollingWood" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

       ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 Then
			rolling(b) = True


                ' ***Ball on WOOD playfield***
            if BOT(b).z < 30 Then 
BOT(b).bulbintensityscale = 1
                      StopSound("fx_RollingPlastic" & b):StopSound("fx_RollingMetal" & b):PlaySound("fx_RollingWood" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )

            Else 
				' ***Ball on METAL ramp*** - Requires Start/End triggers
                If OnWireRamp = 1 Then 
                        StopSound("fx_RollingPlastic" & b):StopSound("fx_RollingWood" & b):PlaySound("fx_RollingMetal" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) ) 

				' ***Ball on PLASTIC ramp***
                Else 
                        StopSound("fx_RollingWood" & b):StopSound("fx_RollingMetal" & b):PlaySound("fx_RollingPlastic" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) ) 
                End If
          if BOT(b).x > 300 and BOT(b).x < 450 and BOT(b).z > 103 Then BOT(b).bulbintensityscale = 0.1 :End If
End If
        Else
            If rolling(b) = True Then
                StopSound("fx_RollingWood" & b)
                StopSound("fx_RollingPlastic" & b)
                StopSound("fx_RollingMetal" & b)
                rolling(b) = False
            End If
        End If
    Next

End Sub


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub



Sub Gates_Hit (idx)
    PlaySound "gate", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub BallHitSound(dummy):PlaySound "BallDrop_1":End Sub

'-------------------------------------

Sub SkillRampStart_hit()
'If ActiveBall.VelY > 5 Then Playsound "fx_Rolling_Metal", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0


If OnWireRamp = 1 Then
	OnWireRamp = 0
'	StopSound("fx_Rolling_Metal")
Else
	OnWireRamp = 1
End If

End Sub


'
Sub SkillRampEnd_hit()
	OnWireRamp = 0
'	StopSound "fx_Rolling_Metal"
End Sub


Sub LwireStart_Hit()
	OnWireRamp = 1
End Sub



'--------------------------------
Sub RWireEnd_Hit()
     vpmTimer.AddTimer 150 :PlaySound "Balldrop_" & Int(Rnd*3)+1
 End Sub

Sub plungeballdrop_Hit()
If ActiveBall.VelY > 0 Then PlaysoundAt "wirerampdrop2", ActiveBall
End Sub

Sub Trigger1_Hit():PlaySoundAt "xramp1", Trigger1:End Sub
Sub Trigger2_Hit():PlaySoundAt "xramp1", Trigger2:End Sub
Sub Trigger3_Hit():PlaySoundAt "xramp1", Trigger3:End Sub
Sub Trigger4_Hit():PlaySoundAt "xramp1", Trigger4:End Sub
Sub Trigger5_Hit():PlaySoundAt "xramp1", Trigger5:End Sub
Sub Trigger6_Hit():PlaySoundAt "xramp1", Trigger6 End Sub
Sub Trigger7_Hit():PlaySoundAt "xramp1", Trigger7:End Sub
Sub Trigger8_Hit():PlaySoundAt "xramp1", Trigger8:End Sub
Sub Trigger9_Hit():PlaySoundAt "xramp1", Trigger9:End Sub
Sub Trigger10_Hit():PlaySoundAt "xramp1", Trigger10:End Sub
Sub Trigger5_Hit():PlaySoundAt "xramp2", Trigger10:End Sub
Sub Trigger8_Hit():PlaySoundAt "xramp2", Trigger10:End Sub
Sub Trigger11_Hit():PlaySoundAt "xramp2", Trigger10:End Sub
Sub Trigger13_Hit():PlaySoundAt "xramp2", Trigger10:End Sub
Sub Trigger14_Hit():PlaySoundAt "xramp2", Trigger10:End Sub
Sub Trigger15_Hit():PlaySoundAt "xramp2", Trigger10:End Sub



Sub RubberslowerPFlargepost_Hit(idx)
    dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 3 then
        PlaySound "fx_rubber2", 0, 2*Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End if
    If finalspeed >= 6 AND finalspeed <= 20 then
        RandomSoundRubber()
    End If
End Sub

Sub RubberlowerPFsmallpost_Hit(idx)
    dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 3 then
        PlaySound "rubber", 0, 2*Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End if
    If finalspeed >= 6 AND finalspeed <= 16 then
        RandomSoundRubber()
    End If
End Sub



Sub ShuttleRampStart_Hit:PlaySound "plasticrollinglowhighpass", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall): End Sub
Sub ShuttleRampEnd_UnHit(): StopSound "plasticrollinglowhighpass" :End Sub
Sub ShuttleRampEnd1_Hit(): StopSound "plasticrollinglowhighpass" : End Sub
Sub ShuttleRampEnd2_Hit(): StopSound "plasticrollinglowhighpass" : End Sub

Sub HeartRampStart_Hit ():PlaySound "plasticrollinglowhighpass", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):End Sub
'Sub HeartRampStart_Hit ():PlaySound "plasticrollinglowhighpass", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0: ActiveBall.vely = Activeball.vely*1.25:End Sub
Sub HeartRampEnd_UnHit(): StopSound "plasticrollinglowhighpass" :End Sub

Sub HeartRampStart1_Hit:PlaySound "plasticrollinglowhighpass", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall): End Sub
Sub HeartRampEnd1_UnHit(): StopSound "plasticrollinglowhighpass" :End Sub


'MISC. Sound effects

'Sub Subway_Hit(): playsound"fx_Rolling_Metal" : End Sub
Sub BL_hit():PlaySound "wirerampdrop":End Sub
Sub right_gate_Hit():if gate3.currentangle < 90 then:PlaySound "fx_gate2":end if:End Sub
Sub Metals_Hit(idx):PlaySound "fx_metalhit2", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub Gates_Hit (idx): PlaySound "fx_gate", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall): End Sub

Sub pSaucerFloor1_hit : playsound "rampbump5" : End Sub
Sub pSaucerFloor2_hit : playsound "rampbump5" : End Sub
Sub pSaucerFloor3_hit : playsound "rampbump5" : End Sub


'Ramps Bumps sounds
Dim NextOrbitHit:NextOrbitHit = 0 


'Platic Ramp Bumps
Sub PlasticRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 2, Pitch(ActiveBall)*2
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .005 + (Rnd * .2)
	end if 
End Sub


''Metal Ramp Bumps
'Sub RampFlaps_Hit(idx)
'	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
'		RandomBump 3, 20000 'Increased pitch to simulate metal wall
'		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
'		' Lowering these numbers allow more closely-spaced clunks.
'		NextOrbitHit = Timer + .2 + (Rnd * .2)
'	end if 
'End Sub


'' Requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub


'' Requires metalguidebump1 to 2 in Sound Manager
'Sub RandomBump2(voladj, freq)
'	dim BumpSnd:BumpSnd= "metalguidebump" & CStr(Int(Rnd*2)+1)
'		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
'End Sub


'***Random Ball Drop Sounds***


Sub BallDrop1_Hit
    PlaySound "Balldrop_" & Int(Rnd*3)+1
End Sub

Sub BallDrop2_Hit
    PlaySound "Balldrop_" & Int(Rnd*3)+1
End Sub

Sub BallDrop3_Hit
    PlaySound "Balldrop_" & Int(Rnd*3)+1
	OnWireRamp = 0
End Sub



''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''  Ball Trough system ''''''''''''''''''''''''''
'''''''''''''''''''''by cyberpez''''''''''''''''''''''''''''''''
''''''''''''''''based off of EalaDubhSidhe's''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Dim BallCount

Sub CheckMaxBalls()
    BallCount = MaxBalls
    TroughWall1.isDropped = true
    TroughWall2.isDropped = true
End Sub

Sub CreatBalls_timer()
    If BallCount > 0 then
        drain.CreateSizedBallWithMass BallSize, BallMass
        Drain.kick 70,25
        BallCount = BallCount - 1
    End If

    If BallCount = 0 Then
        CreatBalls.enabled = false

    End If
End Sub

Dim DRSstep

Sub DelayRollingStart_timer()
    Select Case DRSstep
        Case 5: RollingSoundTimer.enabled = true
    End Select
    DRSstep = DRSstep + 1
End Sub

Sub ballrelease_hit()
'   Kicker1active = 1
    Controller.Switch(25)=1
    TroughWall1.isDropped = false

End Sub

Sub sw26_Hit()
    Controller.Switch(26)=1
    TroughWall2.isDropped = false
End Sub

Sub sw26_unHit()
    Controller.Switch(26)=0
    TroughWall2.isDropped = true
End Sub

Sub sw27_Hit()
    Controller.Switch(27)=1
End Sub

Sub sw27_unHit()
    Controller.Switch(27)=0
End Sub

Sub KickBallToLane(Enabled)
    PlaySound SoundFX("BallRelease",DOFContactors)
    ballrelease.Kick 60,12
    TroughWall1.isDropped = true
    Controller.Switch(25)=0
End Sub


sub kisort(enabled)
    Drain.Kick 70,25
    controller.switch(38) = false
end sub


Sub Drain_hit()
    PlaySoundAt "drain", Drain
    controller.switch(38) = true
End Sub


'***OPTIONS***

Dim TableOptions, TableName, Rubbercolor,xxrubbercolor, RubberColorType, GIColorMod, GIColorModType, xxSSRampColor, SSRampColorMod, SSRampColorModType, HLColorType, HottieMod, HottieModType, FlipperRubbersType, FlipperRubbers
Dim DivPOS, SideWallType, HLcolor, rails

Sub SetOptions()

'ShipMod
    If ShipMod = 1 Then
        pShipToy.Visible = True
        F124c.visible = true
        F124d.visible = true
        F124e.visible = true
        F124f.visible = true
        F124g.visible = true
        F124h.visible = true
    Else
        pShipToy.Visible = False
        F124c.visible = False
        F124d.visible = False
        F124e.visible = False
        F124f.visible = False
        F124g.visible = False
        F124h.visible = False
    End if



'Cheater Post
    If cheaterpost = 1 then
        rubber38.collidable = True
        cpost.visible = True
        Primitive69.visible = True
    Else
        rubber38.collidable = false
        cpost.visible = false
        Primitive69.visible = false
    End If



' Sidewall switching
    If sidewalls = 3 then
        SideWallType = Int(Rnd*3)
    Else
        SideWallType = sidewalls
    End If


    select case SideWallType
        case 2: pSidewall.image = "sidewalls_texture":pSidewall.visible = true
        case 1: pSidewall.image = "sidewalls_texture2":pSidewall.visible = true
        case 0: pSidewall.image = "sidewalls_texture3":pSidewall.visible = true
    end select

'Random Diverter POS at startup
    DivPOS = Int(Rnd*2)+1
    If DivPOS = 1 Then
        div.RotateToStart
    Else
        div.RotateToEnd
    End If

'Helmet Lamp Color
Dim BlueFull, Blue, BlueI, BlueFlasher, YellowFull, Yellow, YellowI, YellowFlasher, xxHLColorL, xxHLColorF, xxHLRefl, BulbBlueOff, BulbGIOff, xxHLBulb, BulbFrostedLTBlueOff, CCHLI, SSBulb, xxSSBulb

YellowFull = rgb(255,255,255)
Yellow = rgb(255,255,128)
YellowFlasher = rgb(255,255,230)
YellowI = 3000

BlueFull = rgb(50,50,255)
Blue = rgb(50,50,255)
BlueFlasher = rgb(150,150,255)
BlueI = 3000

CCHLI = 50000

If HLColor = 4 Then
	HLColorType = Int(Rnd*4)
Else
	HLColorType = HLColor
End If


If HLColorType = 0 Then
for each xxHLColorL in HelmetLights
xxHLColorL.Color=Yellow
xxHLColorL.ColorFull=YellowFull
xxHLColorL.Intensity = yellowi
next
for each xxHLColorF in HelmetFlashers
xxHLColorF.Color=YellowFlasher
next
for each xxHLBulb in HelmetBulbs
xxHLBulb.material="BulbGIOff"
next
CCHL.enabled = False
End If

If HLColorType = 1 Then
for each xxHLColorL in HelmetLights
xxHLColorL.Color=Blue
xxHLColorL.ColorFull=BlueFull
xxHLColorL.Intensity = BlueI
next
for each xxHLColorF in HelmetFlashers
xxHLColorF.Color=BlueFlasher
next
for each xxHLBulb in HelmetBulbs
xxHLBulb.material="BulbLTBlueOff"
next
CCHL.enabled = False
End If



If HLColorType = 2 Then
for each xxHLColorL in HelmetLights
xxHLColorL.Color=Blue
xxHLColorL.ColorFull=BlueFull
xxHLColorL.Intensity = BlueI
next
for each xxHLColorF in HelmetFlashers
xxHLColorF.Color=BlueFlasher
next
for each xxHLBulb in HelmetBulbs
xxHLBulb.material="BulbFrostedLTBlueOff"
next
CCHL.enabled = False
End If

If HLColorType = 3 Then
for each xxHLColorL in HelmetLights
xxHLColorL.Intensity = CCHLI 
next
for each xxHLBulb in HelmetBulbs
xxHLBulb.material="BulbGIOff"
next
CCHL.enabled = true
End If


If Rails =1 Then
		Leftrail.visible = 0
		Rightrail.visible = 0
Else
		Leftrail.visible = 1
		Rightrail.visible = 1
End if


If RubberColor = 2 Then
	RubberColorType = Int(Rnd*2)
Else
	RubberColorType = RubberColor
End If


If RubberColorType = 0 Then
	for each xxRubberColor in RubbersCol
		xxRubberColor.Material = "RubberBlack"
		next
End If


If RubberColorType = 1 Then
	for each xxRubberColor in RubbersCol
		xxRubberColor.Material = "RubberWhite"
		next
End If



If GIColorMod = 4 Then
	GIColorModType = Int(Rnd*4)
Else
	GIColorModType = GIColorMod
End If


If GIColorModType = 0 then
	for each xxGIColor in GIColorPlasticsA
		xxGIColor.Color=WhitePlastics
		xxGIColor.ColorFull=WhitePlasticsFull
		xxGIColor.Intensity = WhitePlasticsI
		next
	for each xxGIColor in GIColorPlasticsB
		xxGIColor.Color=WhitePlastics
		xxGIColor.ColorFull=WhitePlasticsFull
		xxGIColor.Intensity = WhitePlasticsI
		next
	for each xxGIColor in GIColorMainA
		xxGIColor.Color=WhiteMain
		xxGIColor.ColorFull=WhiteMainFull
		xxGIColor.Intensity = WhiteMainI
		next
	for each xxGIColor in GIColorMainB
		xxGIColor.Color=WhiteMain
		xxGIColor.ColorFull=WhiteMainFull
		xxGIColor.Intensity = WhiteMainI
		next
	for each xxGIColor in GIColorBulbsA
		xxGIColor.Color=WhiteBulbs
		xxGIColor.ColorFull=WhiteBulbsFull
		xxGIColor.Intensity = WhiteBulbsI
		next
	for each xxGIColor in GIColorBulbsB
		xxGIColor.Color=WhiteBulbs
		xxGIColor.ColorFull=WhiteBulbsFull
		xxGIColor.Intensity = WhiteBulbsI
		next
	for each xxGIColor in GIColorOverhead
		xxGIColor.Color=WhiteOverhead
		xxGIColor.ColorFull=WhiteOverheadFull
		xxGIColor.Intensity = WhiteOverheadI
		next
End If


If GIColorModType = 1 then
	for each xxGIColor in GIColorPlasticsA
		xxGIColor.Color=BluePlastics
		xxGIColor.ColorFull=BluePlasticsFull
		xxGIColor.Intensity = BluePlasticsI
		next
	for each xxGIColor in GIColorPlasticsB
		xxGIColor.Color=BluePlastics
		xxGIColor.ColorFull=BluePlasticsFull
		xxGIColor.Intensity = BluePlasticsI
		next
	for each xxGIColor in GIColorMainA
		xxGIColor.Color=BlueMain
		xxGIColor.ColorFull=BlueMainFull
		xxGIColor.Intensity = BlueMainI
		next
	for each xxGIColor in GIColorMainB
		xxGIColor.Color=BlueMain
		xxGIColor.ColorFull=BlueMainFull
		xxGIColor.Intensity = BlueMainI
		next

	for each xxGIColor in GIColorBulbsA
		xxGIColor.Color=BlueBulbs
		xxGIColor.ColorFull=BlueBulbsFull
		xxGIColor.Intensity = BlueBulbsI
		next
	for each xxGIColor in GIColorBulbsB
		xxGIColor.Color=BlueBulbs
		xxGIColor.ColorFull=BlueBulbsFull
		xxGIColor.Intensity = BlueBulbsI
		next
	for each xxGIColor in GIColorOverhead
		xxGIColor.Color=BlueOverhead
		xxGIColor.ColorFull=BlueOverheadFull
		xxGIColor.Intensity = BlueOverheadI
		next
End If

If GIColorModType = 2 then
	for each xxGIColor in GIColorPlasticsA
		xxGIColor.Color=PinkPlastics
		xxGIColor.ColorFull=PinkPlasticsFull
		xxGIColor.Intensity = PinkPlasticsI
		next
	for each xxGIColor in GIColorPlasticsB
		xxGIColor.Color=PinkPlastics
		xxGIColor.ColorFull=PinkPlasticsFull
		xxGIColor.Intensity = PinkPlasticsI
		next
	for each xxGIColor in GIColorMainA
		xxGIColor.Color=PinkMain
		xxGIColor.ColorFull=PinkMainFull
		xxGIColor.Intensity = PinkMainI
		next
	for each xxGIColor in GIColorMainB
		xxGIColor.Color=PinkMain
		xxGIColor.ColorFull=PinkMainFull
		xxGIColor.Intensity = PinkMainI
		next

	for each xxGIColor in GIColorBulbsA
		xxGIColor.Color=PinkBulbs
		xxGIColor.ColorFull=PinkBulbsFull
		xxGIColor.Intensity = PinkBulbsI
		next
	for each xxGIColor in GIColorBulbsB
		xxGIColor.Color=PinkBulbs
		xxGIColor.ColorFull=PinkBulbsFull
		xxGIColor.Intensity = PinkBulbsI
		next
	for each xxGIColor in GIColorBulbsB
		xxGIColor.Color=PinkBulbs
		xxGIColor.ColorFull=PinkBulbsFull
		xxGIColor.Intensity = PinkBulbsI
		next
	for each xxGIColor in GIColorOverhead
		xxGIColor.Color=PinkBulbs
		xxGIColor.ColorFull=PinkOverheadFull
		xxGIColor.Intensity = PinkOverheadI
		next
End If

If GIColorModType = 3 then
	for each xxGIColor in GIColorPlasticsA
		xxGIColor.Color=BlueMain
		xxGIColor.ColorFull=BlueMainFull
		xxGIColor.Intensity = BlueMainI
		next
	for each xxGIColor in GIColorMainA
		xxGIColor.Color=BlueMain
		xxGIColor.ColorFull=BlueMainFull
		xxGIColor.Intensity = BlueMainI
		next
	for each xxGIColor in GIColorBulbsA
		xxGIColor.Color=BlueMain
		xxGIColor.ColorFull=BlueMainFull
		xxGIColor.Intensity = BlueMainI
		next
	for each xxGIColor in GIColorOverhead
		xxGIColor.Color=BlueMain
		xxGIColor.ColorFull=BlueOverheadFull
		xxGIColor.Intensity = BlueOverheadI
		next

	for each xxGIColor in GIColorPlasticsB
		xxGIColor.Color=PinkMain
		xxGIColor.ColorFull=PinkMainFull
		xxGIColor.Intensity = PinkMainI
		next
	for each xxGIColor in GIColormainB
		xxGIColor.Color=PinkMain
		xxGIColor.ColorFull=PinkMainFull
		xxGIColor.Intensity = PinkMainI
		next
	for each xxGIColor in GIColorBulbsB
		xxGIColor.Color=PinkMain
		xxGIColor.ColorFull=PinkMainFull
		xxGIColor.Intensity = PinkMainI
		next
End If


If SSRampColorMod = 2 Then
	SSRampColorModType = Int(Rnd*2)
Else
	SSRampColorModType = SSRampColorMod
End If

If SSRampColorModType = 0 Then
	for each xxSSrampColor in SSLampsWhite
		xxSSRampColor.Color = SSwhite
		xxSSRampColor.ColorFull = SSWhiteFull
		xxSSRampColor.Intensity = SSWhiteI
next
End if

If SSRampColorModType = 1 Then
	for each xxSSrampColor in SSLampsBlue
		xxSSRampColor.Color = SSBLue
		xxSSRampColor.ColorFull = SSBlueFull
		xxSSRampColor.Intensity = SSBlueI
		'xxSSRampColor.TransmissionScale  = .01
		next
        for each xxSSBulb in SSBulbs
		xxSSBulb.material="BulbGIOff"
	next


End If

If SSRampColorModType = 1 Then
	for each xxSSrampColor in SSLampsYellow
		xxSSRampColor.Color = SSYellow
		xxSSRampColor.ColorFull = SSYellowFull
		xxSSRampColor.Intensity = SSYellowI
		'xxSSRampColor.TransmissionScale  = .01
	next
End if

If SSRampColorModType = 1 Then
	for each xxSSrampColor in SSLampsOrange
		xxSSRampColor.Color = SSOrange
		xxSSRampColor.ColorFull = SSOrangeFull
		xxSSRampColor.Intensity = SSOrangeI
		'xxSSRampColor.TransmissionScale  = .01
	next
End if

If SSRampColorModType = 1 Then
	for each xxSSrampColor in SSLampsRed
		xxSSRampColor.Color = SSRed
		xxSSRampColor.ColorFull = SSRedFull
		xxSSRampColor.Intensity = SSRedI
		'xxSSRampColor.TransmissionScale  = .01
	next
End if


If SSRampColorModType = 1 Then
	for each xxSSrampColor in SSLampsWhite
		xxSSRampColor.Color = SSWhite
		xxSSRampColor.ColorFull = SSWhiteFull
		xxSSRampColor.Intensity = SSWhiteI
		'xxSSRampColor.TransmissionScale  = .01
	next
End if



'<<<Hottie Mod>>>
If HottieMod = 10 Then
	HottieModType = Int(Rnd*10)
Else
	HottieModType = HottieMod
End If

If HottieModType = 0 Then
	Face.image = "BOPHead"
End If

If HottieModType = 1 Then
	Face.image = "Ashley"
End If

If HottieModType = 2 Then
	Face.image = "Brooke"
End If

If HottieModType = 3 Then
	Face.image = "Kelly"
End If

If HottieModType = 4 Then
	Face.image = "Brunette"
End If

If HottieModType = 5 Then
	Face.image = "Meagan"
End If

If HottieModType = 6 Then
	Face.image = "Shelly"
End If

If HottieModType = 7 Then
	Face.image = "Rachel"
End If

If HottieModType = 8 Then
	Face.image = "Valerie"
End If

If HottieModType = 9 Then
	Face.image = "Harley"
End If

End Sub


'Flipper Rubbers Color Mod
If FlipperRubbers = 3 Then
	FlipperRubbers = Int(Rnd*3)
Else
	FlipperRubbersType = FlipperRubbers
End If

If FlipperRubbersType = 0 Then
			'batleft.visible = 1 : batright.visible = 1 : LeftFlipper.visible = 0 : RightFlipper.visible = 0
			'batleftshadow.visible = 1 : batrightshadow.visible = 1 : GraphicsTimer.enabled = True
			batleft.image = "flipper_white_black" : batright.image = "flipper_white_black"
End If

If FlipperRubbersType = 1 Then
			'batleft.visible = 1 : batright.visible = 1 : LeftFlipper.visible = 0 : RightFlipper.visible = 0
			'batleftshadow.visible = 1 : batrightshadow.visible = 1 : GraphicsTimer.enabled = True
			batleft.image = "flipper_white_red" : batright.image = "flipper_white_red"
End If

If FlipperRubbersType = 2 Then
			'batleft.visible = 1 : batright.visible = 1 : LeftFlipper.visible = 0 : RightFlipper.visible = 0
			'batleftshadow.visible = 1 : batrightshadow.visible = 1 : GraphicsTimer.enabled = True
			batleft.image = "flipper_white_blue" : batright.image = "flipper_white_blue"
End If


Sub GraphicsTimer_Timer()
		'If FlipperRubbers > 0 Then
		' *** move primitive bats ***
		batleft.objrotz = LeftFlipper.CurrentAngle + 1
		batleftshadow.objrotz = batleft.objrotz
		batright.objrotz = RightFlipper.CurrentAngle - 1
		batrightshadow.objrotz  = batright.objrotz
	'End If
End Sub




'Example Usage (Needs to be piggybacked off of a sub that handles the fading, such as nModFlash)
'            Lamp Number     Object      Image Prefix    Number of images (in this case 0 is full off, 13 is full on)
'ModFlashObjm 420,           F20P,       "DomeYellow_",  13

Sub ModFlashObjm(nr, object, imgseq, steps)    'Primitive texture image sequence    'Modified for SolModCallbacks
   dim x, x2, fadex
    Select Case FadingLevel(nr)
        Case 3, 4, 5, 6 'off
           FadeX = ScaleLights(FlashLevel(nr),0 )    'mod for solmodcallbacks
           for x = 0 to steps-1
                if FadeX <= ((x/steps) + ((1/steps)/2 )) then
                    x2 = x
'                    tb3.text = "on " & x & vbnewline & ((x/steps) + ((1/steps)/2 )) & " =? " & x2
                   exit for
                end if
            next
            if    FadeX >= 1-(1/steps) then x2 = steps ': tb3.text = "fullon"
       '    if    FlashLevel(nr) <= (1/steps) then x2 = 0 : tb3.text = "fulloff"
'            tb.text = "flashlevel: " & FlashLevel(nr) & vbnewline & "stepper: " & x2 & vbnewline & "flasherimg: " & x2
'            tb.text = FlashLevel(nr) & vbnewline & imgseq & x2
           object.image = imgseq & x2
    End Select
End Sub


Dim xxGIColor

Dim WhiteMain, WhiteMainFull, WhiteMainI, WhitePlastics, WhitePlasticsFull, WhitePlasticsI, WhiteBumper, WhiteBumperFull, WhiteBumperI, WhiteBulbs, WhiteBulbsFull, WhiteBulbsI,   WhiteOverhead, WhiteOverheadFull, WhiteOverheadI
WhiteMainFull = rgb(255,255,180)
WhiteMain = rgb(255,255,180)
WhiteMainI = 2
WhitePlasticsFull = rgb(255,255,180)
WhitePlastics = rgb(255,255,180)
WhitePlasticsI = 20
WhiteBulbsFull = rgb(255,255,180)
WhiteBulbs = rgb(255,255,180)
WhiteBulbsI = 300
WhiteOverheadFull = rgb(255,255,250)
WhiteOverhead = rgb(255,255,250)
WhiteOverheadI = .05

Dim BlueMain, BlueMainFull, BlueMainI, BluePlastics, BluePlasticsFull, BluePlasticsI, BlueBulbs, BlueBulbsFull, BlueBulbsI,  BlueOverhead, BlueOverheadFull, BlueOverheadI
BlueMainFull = rgb(10,10,255)
BlueMain = rgb(10,10,255)
BlueMainI = 2
BluePlasticsFull = rgb(10,10,255)
BluePlastics = rgb(10,10,255)
BluePlasticsI =16
BlueBulbsFull = rgb(10,10,255)
BlueBulbs = rgb(10,10,255)
BlueBulbsI = 2000
BlueOverheadFull = rgb(10,10,255)
BlueOverhead = rgb(10,10,255)
BlueOverheadI = 1

Dim PinkMain, PinkMainFull, PinkMainI, PinkPlastics, PinkPlasticsFull, PinkPlasticsI, PinkBumper, PinkBumperFull, PinkBumperI, PinkBulbs, PinkBulbsFull, PinkBulbsI,  PinkOverhead, PinkOverheadFull, PinkOverheadI
PinkMainFull = rgb(255,0,255)
PinkMain = rgb(255,0,255)
PinkMainI = 1
PinkPlasticsFull = rgb(255,0,255)
PinkPlastics = rgb(255,0,255)
PinkPlasticsI = 11
PinkBulbsFull = rgb(255,0,255)
PinkBulbs = rgb(255,0,255)
PinkBulbsI = 2000
PinkOverheadFull = rgb(255,0,255)
PinkOverhead = rgb(255,0,255)
PinkOverheadI = 2

'Dim RedBumper, RedBumperFull, RedBumperI
'RedBumperFull = rgb(255,0,0)
'RedBumper = rgb(255,0,0)
'RedBumperI = 20

Dim BlueBumper, BlueBumperFull, BlueBumperI
BlueBumperFull = rgb(10,10,255)
BlueBumper = rgb(10,10,255)
BlueBumperI = 60

Dim PurpleBumper, PurpleBumperFull, PurpleBumperI
PurpleBumperFull = rgb(150,0,255)
PurpleBumper = rgb(150,0,255)
PurpleBumperI = 60


'<<<Skill Shot Lamp RGB>>>
Dim SSBlueFull, SSBlue, SSBlueI
SSBlueFull = rgb(25,25,255)
SSBlue = rgb(25,25,255)
SSBlueI = 10000

Dim SSYellowFull, SSYellow, SSYellowI
SSYellowFull = rgb(255,255,0)
SSYellow = rgb(255,255,0)
SSYellowI = 10000

Dim SSOrangeFull, SSOrange, SSOrangeI
SSOrangeFull = rgb(255,168,0)
SSOrange = rgb(255,168,0)
SSOrangeI = 10000

Dim SSRedFull, SSRed, SSRedI
SSRedFull = rgb(255,25,25)
SSRed = rgb(255,25,25)
SSRedI = 10000

Dim SSWhiteFull, SSWhite, SSWhiteI
SSWhiteFull = rgb(225,255,220)
SSWhite = rgb(255,255,220)
SSWhiteI = 1000



' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
'(_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_)

'''''''''''Color Changing Helmet Lights




'Dim R, G, B
Dim CCGI2Step, xxGiCC
Dim Red1RGB, Green1RGB, Blue1RGB, Red2RGB, Green2RGB, Blue2RGB, Red3RGB, Green3RGB, Blue3RGB

Red1RGB = 255
Green1RGB = 0
Blue1RGB = 0

Red2RGB = 0
Green2RGB = 255
Blue2RGB = 0

Red3RGB = 0
Green3RGB = 0
Blue3RGB = 255


Sub CCHL_timer ()



	If Red1RGB < 0 then Red1RGB = 0 end If
	If Red2RGB < 0 then Red2RGB = 0 end If
	If Red3RGB < 0 then Red3RGB = 0 end If
	If Green1RGB < 0 then Green1RGB = 0 End If
	If Green2RGB < 0 then Green2RGB = 0 End If
	If Green3RGB < 0 then Green3RGB = 0 End If
	If Blue1RGB < 0 then Blue1RGB = 0 End If
	If Blue2RGB < 0 then Blue2RGB = 0 End If
	If Blue3RGB < 0 then Blue3RGB = 0 End If
	If Red1RGB > 255 then Red1RGB = 255 End If
	If Red2RGB > 255 then Red2RGB = 255 End If
	If Red3RGB > 255 then Red3RGB = 255 End If
	If Green1RGB > 255 then Green1RGB = 255 End If
	If Green2RGB > 255 then Green2RGB = 255 End If
	If Green3RGB > 255 then Green3RGB = 255 End If
	If Blue1RGB > 255 then Blue1RGB = 255 End If
	If Blue2RGB > 255 then Blue2RGB = 255 End If
	If Blue3RGB > 255 then Blue3RGB = 255 End If
	
	If CCGI2Step > 0 and CCGI2Step < 255 Then
		Green1RGB = Green1RGB + 1
		Red3RGB = Red3RGB + 1
		Blue2RGB = Blue2RGB + 1
	End If
		If CCGI2Step > 255 and CCGI2Step < 510 Then
		Red1RGB = Red1RGB - 1
		Blue3RGB = Blue3RGB - 1
		Green2RGB = Green2RGB - 1

	End If
	If CCGI2Step > 510 and CCGI2Step < 765 Then
		Blue1RGB = Blue1RGB + 1
		Green3RGB = Green3RGB + 1
		Red2RGB = Red2RGB + 1

	End If
	If CCGI2Step > 765 and CCGI2Step < 1020 Then
		Green1RGB = Green1RGB - 1
		Red3RGB = Red3RGB - 1
		Blue2RGB = Blue2RGB - 1

	End If
	If CCGI2Step > 1020 and CCGI2Step < 1275 Then
		Red1RGB = Red1RGB + 1
		Blue3RGB = Blue3RGB + 1
		Green2RGB = Green2RGB + 1

	End If
	If CCGI2Step > 1275 and CCGI2Step < 1530 Then
		Blue1RGB = Blue1RGB - 1
		Green3RGB = Green3RGB - 1
		Red2RGB = Red2RGB - 1
	End If

If CCGI2Step = 1530 then CCGI2Step = 0 End If


CCGI2Step = CCGI2Step + 1


	for each xxGiCC in HelmetLights
			xxGiCC.Color = rgb(Red1RGB,Green1RGB,Blue1RGB)
			xxGiCC.ColorFull = rgb(Red1RGB,Green1RGB,Blue1RGB)
			next


End Sub



''''''''''''' End Color Changing Helmet Lights

'====================
'Class jungle nf
'=============

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks

Class LampFader
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Public UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)
	Private Mult(140)
	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		'debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next

		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class




'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be publicly accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
'Version 0.13a - fixed DynamicLamps hopefully
' Note: if using multiple 'DynamicLamps' objects, change the 'name' variable to avoid conflicts with callbacks

Class DynamicLamps 'Lamps that fade up and down. GI and Flasher handling
	Public Loaded(50), FadeSpeedDown(50), FadeSpeedUp(50)
	Private Lock(50), SolModValue(50)
	Private UseCallback(50), cCallback(50)
	Public Lvl(50)
	Public Obj(50)
	Private UseFunction, cFilter
	private Mult(50)
	Public Name

	Public FrameTime
	Private InitFrame

	Private Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(Obj)
			FadeSpeedup(x) = 0.01
			FadeSpeedDown(x) = 0.01
			lvl(x) = 0.0001 : SolModValue(x) = 0
			Lock(x) = True : Loaded(x) = False
			mult(x) = 1
			Name = "DynamicFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function

	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'debugstr = debugstr & x & "=" & tmp(x) & vbnewline
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next

		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property


	Public Property Let State(idx,Value)
		'If Value = SolModValue(idx) Then Exit Property ' Discard redundant updates
		If Value <> SolModValue(idx) Then ' Discard redundant updates
			SolModValue(idx) = Value
			Lock(idx) = False : Loaded(idx) = False
		End If
	End Property
	Public Property Get state(idx) : state = SolModValue(idx) : end Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	'solcallback (solmodcallback) handler
	Sub SetLamp(aIdx, aInput) : state(aIdx) = aInput : End Sub	'0->1 Input
	Sub SetModLamp(aIdx, aInput) : state(aIdx) = aInput/255 : End Sub	'0->255 Input
	Sub SetGI(aIdx, ByVal aInput) : if aInput = 8 then aInput = 7 end if : state(aIdx) = aInput/7 : End Sub	'0->8 WPC GI input

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) ': debugstr = debugstr & tmp(x).name & " state'd" & vbnewline

				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) ': debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline

			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'just call turnonstates for now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all numeric fading. If done fading, Lock(x) = True
		'dim stringer
		dim x : for x = 0 to uBound(Lvl)
			'stringer = "Locked @ " & SolModValue(x)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					'stringer = "Fading Up " & lvl(x) & " + " & FadeSpeedUp(x)
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					'stringer = "Fading Down " & lvl(x) & " - " & FadeSpeedDown(x)
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				End If
			end if
		Next
		'tbF.text = stringer
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(Lvl)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				End If
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx
		for x = 0 to uBound(Lvl)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(abs(Lvl(x))*mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(abs(Lvl(x))*mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)*mult(x)
					End If
				end if
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x)*mult(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					Loaded(x) = True
				end if
			end if
		Next
	End Sub
End Class

'Helper functions
Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String)
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
		AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function





'Helper function
Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
		AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function



Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7)

Sub BallShadowUpdate_timer()
   Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10                       
        If BOT(b).Z > 20 and BOT(b).Z < 200 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 80
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 80
End If
    Next	
End Sub


Dim shadowopacity
Shadow.opacity = shadowopacity


Class cvpmMyMech
	Public Sol1, Sol2, MType, Length, Steps, Acc, Ret, InitialPos
	Private mMechNo, mNextSw, mSw(), mLastPos, mLastSpeed, mCallback

	Private Sub Class_Initialize
		ReDim mSw(10)
		gNextMechNo = gNextMechNo + 1 : mMechNo = gNextMechNo : mNextSw = 0 : mLastPos = 0 : mLastSpeed = 0
		MType = 0 : Length = 0 : Steps = 0 : Acc = 0 : Ret = 0 : InitialPos = -1 : vpmTimer.addResetObj Me
	End Sub

	Public Sub AddSw(aSwNo, aStart, aEnd)
		mSw(mNextSw) = Array(aSwNo, aStart, aEnd, 0)
		mNextSw = mNextSw + 1
	End Sub

	Public Sub AddPulseSwNew(aSwNo, aInterval, aStart, aEnd)
		If Controller.Version >= "01200000" Then
			mSw(mNextSw) = Array(aSwNo, aStart, aEnd, aInterval)
		Else
			mSw(mNextSw) = Array(aSwNo, -aInterval, aEnd - aStart + 1, 0)
		End If
		mNextSw = mNextSw + 1
	End Sub

	Public Sub Start
		Dim sw, ii
		With Controller
			.Mech(1) = Sol1 : .Mech(2) = Sol2 : .Mech(3) = Length
			.Mech(4) = Steps : .Mech(5) = MType : .Mech(6) = Acc : .Mech(7) = Ret
			ii = 10
			For Each sw In mSw
				If IsArray(sw) Then
					.Mech(ii) = sw(0) : .Mech(ii+1) = sw(1)
					.Mech(ii+2) = sw(2) : .Mech(ii+3) = sw(3)
					ii = ii + 10
				End If
			Next
			if InitialPos >= 0 then .Mech(8) = InitialPos
			.Mech(0) = mMechNo
		End With
		If IsObject(mCallback) Then mCallBack 0, 0, 0 : mLastPos = 0 : vpmTimer.EnableUpdate Me, True, True  ' <------- Enhances smoothness
	End Sub

	Public Property Get Position : Position = Controller.GetMech(mMechNo) : End Property
	Public Property Get Speed    : Speed = Controller.GetMech(-mMechNo)   : End Property
	Public Property Let Callback(aCallBack) : Set mCallback = aCallBack : End Property

	Public Sub Update
		Dim currPos, speed
		currPos = Controller.GetMech(mMechNo)
		speed = Controller.GetMech(-mMechNo)
		If currPos < 0 Or (mLastPos = currPos And mLastSpeed = speed) Then Exit Sub
		mCallBack currPos, speed, mLastPos : mLastPos = currPos : mLastSpeed = speed
	End Sub

	Public Sub Reset : Start : End Sub
	' Obsolete
	Public Sub AddPulseSw(aSwNo, aInterval, aLength) : AddSw aSwNo, -aInterval, aLength : End Sub
End Class

