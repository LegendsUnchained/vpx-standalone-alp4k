 '*********************************************************************
 '1994 Williams Demolition Man
 '*********************************************************************
 '*********************************************************************
 'Pinball Machine designed by Dennis Nordman
 '*********************************************************************
 '*********************************************************************
 'recreated for Visual Pinball by Knorr
 '*********************************************************************
 '*********************************************************************
 'Playfield and Plastics Redraw by Kiwi
 '*********************************************************************
 '*********************************************************************
 'I would like to give my sincere thanks to
 'Mfuegemann, Freneticamnesic, Toxie and the VPdevs, Clark Kent and
 'Gigalula for always being so friendly, helpful and motivating while
 'building this table.
 '*********************************************************************


 'V1.3.1
		'fix for ballsearching bug
		'fix for insertmaterial

 'V1.3
		'reworked inserts lights and Playfield
		'baked in shadows and ambient inc.
		'reduced polycount
		'reworked Flasher
		'reworked Ramps
		'fixed some bugs with the claw crane
		'added nfozzy´s FastFlips
		'changed and added some soundfx
		'made for vpx 10.4

 'V1.2
		'new ramps made by flupper (thanks!!!)
		'cleaned up script
		'fewer timers for better performance
		'small changes with the physics
		'claw animation improved by shoopity (thanks for all the explaination!)
		'bug fixes with pballs
		'added missing sounds
		'new enviroment, new lightning


 'V1.1
		'Added controller.vbs
		'Bug Fixes





Option Explicit
 Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
Const UseVPMModSol = 1

 LoadVPM "01560000", "WPC.VBS", 3.36
 
 '********************
 'Standard definitions
 '********************


 '***ROM***ROM***ROM***ROM***
 Const cGameName = "dm_h6b"  'original ROM - dm_lx4
'***************************

 Const UseSolenoids = 1
 Const UseLamps = 0
 Const SSolenoidOn = "SolOn"
 Const SSolenoidOff = "SolOff" 
 Const SFlipperOn = "FlipperUp"
 Const SFlipperOff = "FlipperDown"
 Const SCoin = "Coin5"
 Set GiCallback2 = GetRef("UpdateGI")
' Set LampCallback = GetRef("UpdateMultipleLamps")
 Set MotorCallback = GetRef("RealTimeUpdates")
 BSize = 26
 BMass = 1.7

 Dim bsTrough, retinascan, bsTopPopper, BottomPopper, bsEject, Oldsmobileball, RedCar, GMUltraliteball, WhiteCar, clawmech, elevatormech, BallinClaw
 Dim ClawSpeed:ClawSpeed = 67
 Dim FastFlips


 '************
 ' Table init.
 '************
 
 Sub Table1_Init
     vpmInit Me
     With Controller
        .GameName = cGameName
          If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
         .Games(cGameName).Settings.Value("rol") = 0
         .HandleKeyboard = 0
         .ShowTitle = 0
         .ShowDMDOnly = 1
         .ShowFrame = 0
         .HandleMechanics = 0
         .Hidden = 0
         '.SetDisplayPosition 0, 0, GetPlayerHWnd 'uncomment this line if you don't see the DMD
          On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
         .Switch(22) = 1 'close coin door
         .Switch(24) = 1 'and keep it close
			PinMAMETimer.Interval = PinMAMEInterval
			PinMAMETimer.Enabled = true
			vpmNudge.TiltSwitch = 14
			vpmNudge.Sensitivity = 2
     End With


 '******
 'Trough
 '******
     Set bsTrough = New cvpmBallStack
     With bsTrough
         .InitSw 0, 31, 32, 33, 34, 35, 0, 0
         .InitKick BallRelease, 90, 10
		 .InitExitSnd SoundFX("BallRelease",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
         .Balls = 5
'         .IsTrough = 1
     End With

 '***********
 'Retina Scan
 '***********

Set retinascan = New cvpmCaptiveBall
	With retinascan
		.InitCaptive RetinaTrigger, RetinaWall, RetinaKicker, 345
		.ForceTrans = .9
		.MinForce = 2
		.CreateEvents "retinascan"
		.Start
	End With

 '***************
 'Claw & Elevator
 '***************

set clawmech = new cvpmmech
 with clawmech
  .mtype =vpmmechtwodirsol + vpmmechstopend + vpmmechlinear
  .sol1  =19
  .sol2  =20
  .length=Clawspeed
  .steps = 147
  .addsw 25,0,2
  .addsw 26,140,147
  .callback= getRef("UpdateClaw")
  .start
 end with

Set elevatormech = new cvpmMech
	with elevatormech
		.mtype = vpmMechOneSol + vpmMechReverse + vpmMechLinear
		.Sol1 = 18
		.Length = 15
		.steps = 70
		.addsw 67,0,2
		.addsw 74,65,70
		.Callback = getRef("UpdateElevator")
		.Start
	End with


 '***********
 'Top Popper
 '***********

Set bsTopPopper = New cvpmBallStack
	with bsTopPopper
	.InitSaucer sw73, 73, 182, 18
	.InitExitSnd SoundFX("EjectKick",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
	.KickForceVar = 1
	.KickAngleVar = 0.5
	End With


 '*************
 'Bottom Popper
 '*************

Set BottomPopper = New cvpmBallStack
	With BottomPopper
		.InitSw 0, 76, 0, 0, 0, 0, 0, 0
		.InitKick sw76, 58, 73
		.KickZ = 60
		.InitExitSnd SoundFX("BottomPopper",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
	End With


 '*****
 'Eject
 '*****

Set bsEject = New cvpmBallStack
	With bsEject
	.InitSaucer sw66, 66, 180, 2
	.InitExitSnd SoundFX("EjectKick",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
	.KickForceVar = 1
	.KickAngleVar = 0.5
End With

 '************
 'CarCapative
 '************

Set RedCar = New cvpmCaptiveBall
	With RedCar
		.InitCaptive OldsmobileTrigger, OldsmobileWall, OldsmobileKicker, 345
		.ForceTrans = .9
		.MinForce = 3.5
'		.RestSwitch = 71
		.CreateEvents "RedCar"
		.Start
	End With


DiverterOn.isdropped = 1
DiverterOff.isdropped = 0


Set FastFlips = new cFastFlips
	with FastFlips
	.CallBackL = "SolLflipper"	'Point these to flipper subs
	.CallBackR = "SolRflipper"	'...
'	.CallBackUL = "SolULflipper"'...(upper flippers, if needed)
'	.CallBackUR = "SolURflipper"'...
	.TiltObjects = True 'Calls vpmnudge.solgameon automatically
'	.InitDelay "FastFlips", 100
'	.DebugOn = False		'Call FastFlips.DebugOn True or False in debugger to enable/disable.
	End with

	vpmNudge.TiltSwitch=1
	vpmNudge.Sensitivity=3
	vpmNudge.Sensitivity=3
	vpmNudge.TiltObj=Array(Leftjetbumper,rightjetbumper,LeftSlingshot,RightSlingshot)
	BallinClaw = False



 '************
 'FS Setting
 '************


 If table1.ShowDT = False then
	LeftSideRail.WidthTop = 0
	LeftSideRail.WidthBottom = 0
	RightSideRail.WidthTop = 0
	RightSideRail.WidthBottom = 0
	Korpus.Size_Y = 1.7
'	Korpus.visible = False
 End if
End Sub


 '******
 'Cars Animation
 '******

'Oldsmobile

	Set Oldsmobileball = CapKicker1.CreateBall
	Oldsmobileball.Visible = False
	CapKicker1.Kick 0,0
	CapKicker1.Enabled = false
	Controller.Switch(71) = 1

Sub OldsmobileTimer_Timer()
	Oldsmobile.x = Oldsmobileball.x:Oldsmobile.y = Oldsmobileball.y
End Sub

'GM Ultralite

	Set GMUltraliteball = CapKicker2.CreateBall
	GMUltraliteball.Visible = False
	CapKicker2.Kick 0,0
	CapKicker2.Enabled = false
	Controller.Switch(72) = 1

Sub GMUltraliteTimer_Timer()
	GMUltralite.x = GMUltraliteball.x:GMUltralite.y = GMUltraliteball.y
End Sub



Sub sw72_Hit:Controller.Switch(72) = 0:End Sub		'GMUltralite Restswitch
Sub sw72_UnHit:Controller.Switch(72) = 1: End Sub

Sub sw71_Hit:Controller.Switch(71) = 0:End Sub		'Oldsmobil Restswitch
Sub sw71_UnHit:Controller.Switch(71) = 1: End Sub

 '*********
 'Solenoids
 '*********
 
SolCallback(1) = "SolRelease"
SolCallback(2) = "BottomPopper.SolOut"
SolCallback(3) = "AutoPlunge"
SolCallback(4) = "bsTopPopper.SolOut"
SolCallback(15) = "DiverterRight"
SolCallback(7) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallBack(14) = "bsEject.SolOut"
SolCallBack(19) = "SolClawMotorLeft"
SolCallback(33) = "ClawMagnetOn"

SolCallBack(31) = "FastFlips.TiltSol"


SolModCallback(17) = "Flash117"			'ClawFlasher
SolModCallback(21) = "Flash121"			'JetsFlasher
SolModCallback(22) = "Flash122"			'SideRampFlasher
SolModCallback(23) = "Flash123"			'LeftRampUpFlasher
SolModCallback(24) = "Flash124"			'LeftRampLwrFlasher
SolModCallback(25) = "Flash125"			'CarChaseCntrFlasher
SolModCallback(26) = "Flash126"			'CarChaseLwrFlasher
SolModCallback(27) = "Flash127"			'RightRampFlasher
SolModCallback(28) = "Flash128"			'EjectFlasher


SolModCallback(51) = "Flash137"			'CarChaseUprFlasher
SolModCallback(52) = "Flash138"			'LowerReboundFlasher
SolModCallback(53) = "Flash139"			'EyeballFlasher
SolModCallback(54) = "Flash140"			'CenterRampFlasher
SolModCallback(55) = "Flash141"			'Elevator 2 Flasher
SolModCallback(56) = "Flash142"			'Elevator 1 Flasher
SolModCallback(57) = "Flash143"			'DiverterFlasher
SolModCallback(58) = "Flash144"			'Rt.RampUpFlasher

'************
' BallRelease
'************


 Sub SolRelease(Enabled)
     If Enabled And bsTrough.Balls > 0 Then
         vpmTimer.PulseSw 36
         bsTrough.ExitSol_On
     End If
 End Sub


'*****************************************
' Claw and Elevator Animation + Ballupdate
'*****************************************


Sub UpdateClaw(aNewPos,aSpeed,aLastPos)
	ClawTimer.Enabled = 1
End Sub

Sub SolClawMotorLeft(Enabled)
	If enabled then
		PlaySound SoundFX("Motor",DofGear)
	Else
		StopSound SoundFX("Motor",DofGear)
	End if
End Sub



Sub ClawTimer_Timer()
If clawmech.position-107 > Claw.Roty Then
Claw.RotY = Claw.RotY + (8/ClawSpeed)
If Claw.RotY > clawmech.position-107 Then me.enabled = 0
End If
If clawmech.position-107 < Claw.Roty Then
Claw.RotY = Claw.RotY - (8/ClawSpeed)
If Claw.RotY < clawmech.position-107 Then me.enabled = 0
End If
BallP1.RotY = Claw.RotY
End Sub

Sub UpdateElevator(aNewPos,aSpeed,aLastPos)
	PlaySound SoundFX("ElevatorMotor",DOFGear)
	Elevator.TransY = elevatormech.Position
	BallP.TransY = elevatormech.Position
End Sub

Sub ElevatorKicker_Hit()
	me.DestroyBall
	BallP.Visible = True
	ClawKicker1.Enabled = True
	ClawKicker2.Enabled = True
	ClawKicker3.Enabled = True
	ClawKicker4.Enabled = True
	ClawKicker5.Enabled = True
	controller.Switch(74) = True
	BallinClaw = True
End Sub


Sub ClawMagnetOn(Enabled)
	If Enabled And BallinClaw = True Then
		vpmTimer.AddTimer 700, "BallP.Visible = False'"
		vpmTimer.AddTImer 700, "BallP1.Visible = True'"
	Else
		ClawOff
	End if
End Sub

Sub ClawOff()
	BallP1.Visible = False
	If BallinClaw = True then
		if Claw.RotY >= 11 Then ClawKicker5.CreateSizedBall(25.5): ClawKicker5.Kick 185, 1:BallinClaw = False
		if Claw.RotY <= 10.99 And Claw.RotY >= -9.99 Then ClawKicker4.CreateSizedBall(25.5): ClawKicker4.Kick 180, 1:BallinClaw = False
		if Claw.RotY <= -10 And Claw.RotY >= -28.99 Then ClawKicker3.CreateSizedBall(25.5): ClawKicker3.Kick 161, 1:BallinClaw = False
		if Claw.RotY <= -29 And Claw.RotY >= -52.99 Then ClawKicker2.CreateSizedBall(25.5): ClawKicker2.Kick 143, 1:BallinClaw = False
		if Claw.Roty <= -53 And Claw.RotY >= -108.99 Then ClawKicker1.CreateSizedBall(25.5): ClawKicker1.Enabled = False: ClawKicker1.Kick 0, 1:BallinClaw = False
	End if
End Sub

'*****************
' EyeBallPrimitive
'*****************

Dim EyeBallM


Sub EyeballTrigger_Hit()
	Set EyeballM = ActiveBall
	EyeballTimer.Enabled = True
End Sub


Sub EyeBallTimer_Timer()
	EyeballP.x = Eyeballm.x: EyeballP.y = EyeballM.y
	If EyeBallm.VelY <= -1 Then EyeballP.Roty = EyeballP.Roty +30
	If EyeBallm.VelY >= 1 Then EyeballP.Roty = EyeballP.Roty +30
'	If EyeBallm.VelY = 0 Then EyeballP.Roty = EyeballP.Roty 0
End Sub


'**********
' Vuks
'**********


Dim aBall

Sub sw73_Hit()
    PlaySound "HeadquarterHit2", 0, 1, pan(ActiveBall)
    Set aBall = ActiveBall:Me.TimerEnabled = 1
    bsTopPopper.AddBall Me
End Sub

Sub sw73_Timer
    Do While aBall.Z > 0
        aBall.Z = aBall.Z -5
        Exit Sub
    Loop
    Me.TimerEnabled = 0
End Sub


Sub BottomPopperHole_Hit()
	RandomSoundHole
	BottomPopper.AddBall Me
End Sub

Sub BottomPopperHole1_Hit()
	RandomSoundHole
	BottomPopper.AddBall Me
End Sub

Sub ClawRampKicker_Hit()
	RandomSoundHole
	vpmTimer.PulseSw 81
	BottomPopper.AddBall Me
End Sub

Sub sw66_Hit()
	PlaySound ""
	bsEject.AddBall Me
End Sub

Sub Drain_Hit
	bsTrough.AddBall Me
	PlaySound "drain",0,0.5,0
End Sub


'**********
' Keys
'**********

 Sub table1_KeyDown(ByVal Keycode)
	If KeyCode=MechanicalTilt Then
		vpmTimer.PulseSw vpmNudge.TiltSwitch
		Exit Sub
	End if

	If KeyCode = LeftFlipperKey then FastFlips.FlipL True' :  FastFlips.FlipUL True
	If KeyCode = RightFlipperKey then FastFlips.FlipR True' :  FastFlips.FlipUR True
    If keycode = PlungerKey Then Controller.Switch(11) = 1: Controller.Switch(12) = 1
'    If keycode = LeftMagnaSave Or RightMagnaSave Then Controller.Switch(12) = 1
    If keycode = keyFront Then Controller.Switch(23) = 1
    If vpmKeyDown(keycode) Then Exit Sub
 End Sub
 





 Sub table1_KeyUp(ByVal Keycode)

	If KeyCode = LeftFlipperKey then FastFlips.FlipL False' :  FastFlips.FlipUL False
	If KeyCode = RightFlipperKey then FastFlips.FlipR False' :  FastFlips.FlipUR False


     If vpmKeyUp(keycode) Then Exit Sub
     If keycode = PlungerKey Then Controller.Switch(11) = 0: Controller.Switch(12) = 0
'     If keycode = LeftMagnaSave Or RightMagnaSave Then Controller.Switch(12) = 0
     If keycode = keyFront Then Controller.Switch(23) = 0
 End Sub




 '**************
 ' Subs
 '**************
 
' SolCallback(sLRFlipper) = "SolRFlipper"
' SolCallback(sLLFlipper) = "SolLFlipper"
 
 Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("FlipperUpLeftBoth",DOFContactors):LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
     Else
         PlaySound SoundFX("FlipperDown",DOFContactors):LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
     End If
 End Sub
 
 Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("FlipperUpRight",DOFContactors):RightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("FlipperDown",DOFContactors):RightFlipper.RotateToStart
     End If
 End Sub


 '******
 'Plunger
 '******

Dim AP

Sub AutoPlunge(Enabled)
	if enabled then
		AP = True
		Kicker1.Kick 1,48
		PlaySound SoundFX("Plunger",DOFContactors)
	End if
End Sub

	Sub PlungerPTimer()
	if AP = True and PlungerP.TransZ < 45 then PlungerP.TransZ = PlungerP.TransZ +10
	if AP = False and PlungerP.TransZ > 0 then PlungerP.TransZ = PlungerP.TransZ -10
	if PlungerP.TransZ >= 45 then AP = False
End Sub


 '*********
 ' Switches
 '*********


Sub sw27_Hit:Controller.Switch(27) = 1:sw27wire.RotX = 15:PlaySound "metalhit_thin":End Sub		'shooterlane
Sub sw27_UnHit:Controller.Switch(27) = 0:sw27wire.RotX = 0: End Sub
Sub sw15_Hit:Controller.Switch(15) = 1:sw15wire.RotX = 15:PlaySound "metalhit_thin":End Sub 	'left outlane
Sub sw15_UnHit:Controller.Switch(15) = 0:sw15wire.RotX = 0:End Sub
Sub sw16_Hit:Controller.Switch(16) = 1:sw16wire.RotX = 15:PlaySound "metalhit_thin":End Sub 	'left inlane
Sub sw16_UnHit:Controller.Switch(16) = 0:sw16wire.RotX = 0:End Sub
Sub sw17_Hit:Controller.Switch(17) = 1:sw17wire.RotX = 15:PlaySound "metalhit_thin":End Sub 	'right inlane
Sub sw17_UnHit:Controller.Switch(17) = 0:sw17wire.RotX = 0:End Sub
Sub sw18_Hit:Controller.Switch(18) = 1:sw18wire.RotX = 15:PlaySound "metalhit_thin":End Sub 	'right outlane
Sub sw18_UnHit:Controller.Switch(18) = 0:sw18wire.RotX = 0:End Sub
Sub sw63_Hit:Controller.Switch(63) = 1:sw63wire.RotX = 15:PlaySound "metalhit_thin":End Sub 	'leftrollover
Sub sw63_UnHit:Controller.Switch(63) = 0:sw63wire.RotX = 0:End Sub
Sub sw64_Hit:Controller.Switch(64) = 1:sw64wire.RotX = 15:PlaySound "metalhit_thin":End Sub 	'centerrollover
Sub sw64_UnHit:Controller.Switch(64) = 0:sw64wire.RotX = 0:End Sub
Sub sw65_Hit:Controller.Switch(65) = 1:sw65wire.RotX = 15:PlaySound "metalhit_thin":End Sub 	'rightrollover
Sub sw65_UnHit:Controller.Switch(65) = 0:sw65wire.RotX = 0:End Sub
Sub sw48_Hit:Controller.Switch(48) = 1:sw48wire.RotX = 15:PlaySound "metalhit_thin":End Sub 	'Right freeway
Sub sw48_UnHit:Controller.Switch(48) = 0:sw48wire.RotX = 0:End Sub
Sub sw55_Hit:Controller.Switch(55) = 1:sw55wire.RotX = 15:PlaySound "metalhit_thin":End Sub 	'leftloop
Sub sw55_UnHit:Controller.Switch(55) = 0:sw55wire.RotX = 0:End Sub
Sub sw86_Hit:Controller.Switch(86) = 1:End Sub 		'upperleftflippergate
Sub sw86_UnHit:Controller.Switch(86) = 0:End Sub
Sub sw46_Hit:Controller.Switch(46) = 1:End Sub 		'rightrampenter
Sub sw46_UnHit:Controller.Switch(46) = 0:End Sub
Sub sw47_Hit:Controller.Switch(47) = 1:End Sub 		'rightrampexit
Sub sw47_UnHit:Controller.Switch(47) = 0:End Sub
Sub sw75_Hit:Controller.Switch(75) = 1: End Sub 		'elevatorramp
Sub sw75_UnHit:Controller.Switch(75) = 0:End Sub
Sub sw53_Hit:Controller.Switch(53) = 1:End Sub 		'centerramp
Sub sw53_UnHit:Controller.Switch(53) = 0:End Sub
Sub sw51_Hit:Controller.Switch(51) = 1:End Sub 		'leftrampenter
Sub sw51_UnHit:Controller.Switch(51) = 0:End Sub
Sub sw52_Hit:Controller.Switch(52) = 1:End Sub 		'leftrampexit
Sub sw52_UnHit:Controller.Switch(52) = 0:End Sub
Sub sw61_Hit:Controller.Switch(61) = 1:End Sub 		'siderampenter
Sub sw61_UnHit:Controller.Switch(61) = 0:End Sub
Sub sw62_Hit:Controller.Switch(62) = 1:End Sub 		'siderampexit
Sub sw62_UnHit:Controller.Switch(62) = 0:End Sub


Sub sw82_Hit:Controller.Switch(82) = 1:sw82wire.RotX = 75:PlaySound "metalhit_thin":End Sub		'Claw SuperJets
Sub sw82_UnHit:Controller.Switch(82) = 0:sw82wire.RotX = 90: End Sub

Sub sw83_Hit:Controller.Switch(83) = 1:sw83wire.RotX = 70:PlaySound "metalhit_thin":End Sub		'Claw PrisonBreak
Sub sw83_UnHit:Controller.Switch(83) = 0:sw83wire.RotX = 90: End Sub

Sub sw84_Hit:Controller.Switch(84) = 1:sw84wire.RotX = 75:PlaySound "metalhit_thin":End Sub		'Claw Freeze
Sub sw84_UnHit:Controller.Switch(84) = 0:sw84wire.RotX = 90: End Sub

Sub sw85_Hit:Controller.Switch(85) = 1:sw85wire.RotX = 75:PlaySound "metalhit_thin":End Sub		'Claw ACMAG
Sub sw85_UnHit:Controller.Switch(85) = 0:sw85wire.RotX = 90: End Sub





'cFastFlips by nFozzy
	'Bypasses pinmame callback for faster and more responsive flippers
	'Version 1.0
	
	'Flipper / game-on Solenoid # reference (incomplete):
	'Williams System 11: Sol23 or 24
	'Gottlieb System 3: Sol32
	'Data East (pre-whitestar): Sol23 or 24
	'WPC 90', 92', WPC Security: Sol31
	
	'********************Setup*******************:
	
	'....somewhere outside of any subs....
	'dim FastFlips
	
	'....table init....
	'Set FastFlips = new cFastFlips
	'with FastFlips
	'	.CallBackL = "SolLflipper"	'Point these to flipper subs
	'	.CallBackR = "SolRflipper"	'...
	''	.CallBackUL = "SolULflipper"'...(upper flippers, if needed)
	''	.CallBackUR = "SolURflipper"'...
	''	.TiltObjects = True 'Optional, if True calls vpmnudge.solgameon automatically. IF YOU GET A LINE 1 ERROR, DISABLE THIS! (or setup vpmNudge.TiltObj!)
	''	.InitDelay "FastFlips", 100			'Optional, if > 0 adds some compensation for solenoid jitter (occasional problem on Bram Stoker's Dracula)
	''	.DebugOn = False		'Debug, always-on flippers. Call FastFlips.DebugOn True or False in debugger to enable/disable.
	'end with
	
	'...keydown section... (comment out the upper flippers as needed)
	'If KeyCode = LeftFlipperKey then FastFlips.FlipL True :  FastFlips.FlipUL True
	'If KeyCode = RightFlipperKey then FastFlips.FlipR True :  FastFlips.FlipUR True
	'(Do not use Exit Sub, this script does not handle switch handling at all!)
	
	'...keyUp section...
	'If KeyCode = LeftFlipperKey then FastFlips.FlipL False :  FastFlips.FlipUL False
	'If KeyCode = RightFlipperKey then FastFlips.FlipR False :  FastFlips.FlipUR False
	
	'...Flipper Callbacks....
	'if pinmame flipper callbacks are in use, comment them out. For example 'SolCallback(sLRFlipper)
	'But use these subs (they should be the same ones defined in CallBackL / CallBackR) to handle flipper rotation and sounds!
	
	'...Solenoid...
	'SolCallBack(31) = "FastFlips.TiltSol"
	'//////for a reference of solenoid numbers, see top /////
	
	
	'One last note - Because this script is super simple it will call flipper return a lot.
	'It might be a good idea to add extra conditional logic to your flipper return sounds so they don't play every time the game on solenoid turns off
	'Example:
	'Instead of
			'LeftFlipper.RotateToStart
			'playsound SoundFX("FlipperDown",DOFFlippers), 0, 1, 0.01	'return
	'Add Extra conditional logic:
			'LeftFlipper.RotateToStart
			'if LeftFlipper.CurrentAngle = LeftFlipper.StartAngle then
			'	playsound SoundFX("FlipperDown",DOFFlippers), 0, 1, 0.01	'return
			'end if
	'That's it]
	'*************************************************
	
	Class cFastFlips
		Public TiltObjects, DebugOn
		Private SubL, SubUL, SubR, SubUR, FlippersEnabled, Delay, LagCompensation, Name
		
		Private Sub Class_Initialize()
			Delay = 0 : FlippersEnabled = False : DebugOn = False : LagCompensation = False
		End Sub
		
		'set callbacks
		Public Property Let CallBackL(aInput)  : Set SubL  = GetRef(aInput) : End Property
		Public Property Let CallBackUL(aInput) : Set SubUL = GetRef(aInput) : End Property
		Public Property Let CallBackR(aInput)  : Set SubR  = GetRef(aInput) : End Property
		Public Property Let CallBackUR(aInput) : Set SubUR = GetRef(aInput) : End Property
		Public Sub InitDelay(aName, aDelay) : Name = aName : delay = aDelay : End Sub	'Create Delay
		
		'call callbacks
		Public Sub FlipL(aEnabled)
			if not FlippersEnabled and not DebugOn then Exit Sub
			subL aEnabled
		End Sub
		
		Public Sub FlipR(aEnabled)
			if not FlippersEnabled and not DebugOn then Exit Sub
			subR aEnabled
		End Sub
	
		Public Sub FlipUL(aEnabled)
			if not FlippersEnabled and not DebugOn then Exit Sub
			subUL aEnabled
		End Sub	
	
		Public Sub FlipUR(aEnabled)
			if not FlippersEnabled and not DebugOn then Exit Sub
			subUR aEnabled
		End Sub	
	
		Public Sub TiltSol(aEnabled)	'Handle solenoid / Delay (if delayinit)
			if delay > 0 and not aEnabled then 	'handle delay
				vpmtimer.addtimer Delay, Name & ".FireDelay" & "'"
				LagCompensation = True
			else
				if Delay > 0 then LagCompensation = False
				EnableFlippers(aEnabled)
			end if
		End Sub
		
		Sub FireDelay() : if LagCompensation then EnableFlippers False End If : End Sub
		
		Private Sub EnableFlippers(aEnabled)
			FlippersEnabled = aEnabled
			if TiltObjects then vpmnudge.solgameon aEnabled
			If Not aEnabled then
				subL False
				subR False
				if not IsEmpty(subUL) then subUL False
				if not IsEmpty(subUR) then subUR False
			End If		
		End Sub
		
	End Class


'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)
Dim ccount

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 10 'lamp fading speed
LampTimer.Enabled = 1

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
    UpdateLamps
End Sub



Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0         ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4       ' used to track the fading state
        FlashSpeedUp(x) = 0.5    ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.1 ' slower speed when turning off the flasher
        FlashMax(x) = 1          ' the maximum value when on, usually 1
        FlashMin(x) = 0          ' the minimum value when off, usually 0
        FlashLevel(x) = 0        ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

 Sub UpdateLamps
	'Inserts
	NFadeLm 11,  l11a
	NFadeLm 11,  l11b
	NFadeLm 11,  l11c
	NFadeLm 11,  l11d

	NFadeLm 12,  l12
	NFadeLm 12,  l12b
	NFadeLm 13,  l13
	NFadeLm 13,  l13b
	NFadeLm 14,  l14
	NFadeLm 14,  l14b
	NFadeLm 15,  l15
	NFadeLm 15,  l15b
	NFadeLm 16,  l16
	NFadeLm 16,  l16b
	NFadeLm 17,  l17
	NFadeLm 17,  l17b
	NFadeLm 18,  l18
	NFadeLm 18,  l18b

	NFadeLm 21,  l21
	NFadeLm 21,  l21b
	NFadeLm 22,  l22
	NFadeLm 22,  l22b
	NFadeLm 23,  l23
	NFadeLm 23,  l23b
	NFadeLm 24,  l24
	NFadeLm 24,  l24b
	NFadeLm 25,  l25
	NFadeLm 25,  l25b
	NFadeLm 26,  l26
	NFadeLm 26,  l26b
	NFadeLm 27,  l27
	NFadeLm 27,  l27b
	NFadeLm 28,  l28
	NFadeLm 28,  l28b

	NFadeLm 31,  l31
	NFadeLm 31,  l31b
	NFadeLm 32,  l32
'	NFadeLm 32,  l32b
	NFadeLm 33,  l33
	NFadeLm 33,  l33b
	NFadeLm 34,  l34
	NFadeLm 34,  l34b
	NFadeLm 35,  l35
	NFadeLm 35,  l35b
	NFadeLm 36,  l36
	NFadeLm 36,  l36b
	NFadeObjm 36, targetcars, "target1On", "target1"
	NFadeLm 37,  l37
'	NFadeLm 37,  l37b
	NFadeLm 38,  l38
	NFadeLm 38,  l38b


	NFadeLm 41,  l41
	NFadeLm 41,  l41b
	NFadeLm 42,  l42
	NFadeLm 42,  l42b
 	NFadeLm 43,  l43
 	NFadeLm 43,  l43b
	NFadeLm 44,  l44
	NFadeLm 44,  l44b

	NFadeLm 45,  l45
	NFadeLm 45,  l45b
	NFadeLm 46,  l46
	NFadeLm 46,  l46b
	NFadeLm 47,  l47
	NFadeLm 47,  l47b

	NFadeLm 48,  l48
	NFadeLm 48,  l48b

	NFadeLm 51,  l51
	NFadeLm 51,  l51b
	NFadeLm 52,  l52
	NFadeLm 52,  l52b
	NFadeLm 53,  l53
'	NFadeLm 53,  l53b
	NFadeLm 54,  l54
	NFadeLm 54,  l54b
	NFadeLm 55,  l55
	NFadeLm 55,  l55b
	NFadeLm 56,  l56
	NFadeLm 56,  l56b
	NFadeLm 57,  l57
	NFadeLm 57,  l57b
	NFadeLm 58,  l58
	NFadeLm 58,  l58b

'61-65 cranelights
	Flash 61,	f61
	NFadeLm 61,  l61
	Flash 62,	f62
	NFadeLm 62,  l62
	Flash 63,	f63
	NFadeLm 63,  l63
	Flash 64,	f64
	NFadeLm 64,  l64
	Flash 65, 	f65
	NFadeLm 65,  l65

	NFadeLm 66,  l66
	NFadeLm 66,  l66b
	NFadeLm 67,  l67
	NFadeLm 67,  l67b
	NFadeLm 68,  l68
	NFadeLm 68,  l68b

	Flash 71,	f71
'	NFadeLm 71,  l71b
	Flash 72,	f72
'	NFadeLm 72,  l72b
	Flash 73,	f73
'	NFadeLm 73,  l73b
'	NFadeLm 74,  l74
''	NFadeLm 74,  l74b
'	NFadeLm 75,  l75
''	NFadeLm 75,  l75b
	NFadeLm 76,  l76
'	NFadeLm 76,  l76b
	NFadeLm 77,  l77
'	NFadeLm 77,  l77b
	NFadeLm 78,  l78
	NFadeLm 78,  l78b
	NFadeObjm 78, targetretina, "target2On", "target2"


	NFadeLm 81,  l81
	NFadeLm 81,  l81b
	NFadeLm 82,  l82
	NFadeLm 82,  l82a
	NFadeLm 82,  l82b
	NFadeLm 82,  l82c
	NFadeLm 82,  l83
	NFadeLm 82,  l83a
	NFadeLm 82,  l83b
	NFadeLm 82,  l83c
	NFadeLm 84,  l84
	NFadeLm 84,  l84b
	NFadeLm 85,  l85
	NFadeLm 85,  l85b
End Sub


Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself

Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

Sub NFadeLmb(nr, object) ' used for multiple lights with blinking
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 2
    End Select
End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
	Select Case FadingLevel(nr)
		Case 2:object.image = d:FadingLevel(nr) = 0 'Off
		Case 3:object.image = c:FadingLevel(nr) = 2 'fading...
		Case 4:object.image = b:FadingLevel(nr) = 3 'fading...
		Case 5:object.image = a:FadingLevel(nr) = 1 'ON
 End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:object.image = d
        Case 3:object.image = c
        Case 4:object.image = b
        Case 5:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

' Flasher objects

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(nr)
End Sub


'*******************
'***SolModFlasher***
'*******************


Sub Flash117(enabled)
	f17.Opacity = enabled * 4
	f117.Opacity = enabled * 4
End Sub

Sub Flash121(Enabled)
	l121.Intensity = enabled / 9
	l121a.Intensity = enabled / 9
End Sub

Sub Flash122(Enabled)
	l122.Intensity = enabled / 9
End Sub

Sub Flash123(Enabled) '
	f123.Opacity = enabled * 4
	l123.Intensity = enabled / 9
End Sub

Sub Flash124(Enabled) '
	l124.Intensity = enabled / 9
End Sub

Sub Flash125(Enabled) '
	l125.Intensity = enabled / 9
End Sub

Sub Flash126(Enabled) '
	l126.Intensity = enabled / 9
	l126a.Intensity = enabled / 9
End Sub

Sub Flash127(Enabled)
	l127.Intensity = enabled / 9
End Sub

Sub Flash128(Enabled)
	l128.Intensity = enabled / 9
End Sub

Sub Flash137(Enabled) '
	l137.Intensity = enabled / 9
End Sub


Sub Flash138(Enabled)
	l138.Intensity = enabled / 9
End Sub

Sub Flash139(Enabled)
	l139.Intensity = enabled / 9
End Sub

Sub Flash140(Enabled)
	l140.Intensity = enabled / 9
End Sub

Sub Flash141(enabled)
	f141.Opacity = enabled * 4
	f141a.Opacity = enabled * 4
	f141b.Opacity = enabled * 4
End Sub

Sub Flash142(enabled)
	f142.Opacity = enabled * 4
	f142b.Opacity = enabled * 4
End Sub

Sub Flash143(Enabled)
	f143.Opacity = enabled * 4
	l143.Intensity = enabled / 10
End Sub

Sub Flash144(Enabled)
	f144.Opacity = enabled * 4
	l144.Intensity = enabled / 10
End Sub



'************
'RainbowLight
'************


Dim RGBStep, RGBFactor, Red, Green, Blue

RGBStep = 0
RGBFactor = 1
Red = 255
Green = 0
Blue = 0

Sub RGBTimer_timer 'rainbow light color changing
    Select Case RGBStep
        Case 0 'Green
            Green = Green + RGBFactor
            If Green > 255 then
                Green = 255
                RGBStep = 1
            End If
        Case 1 'Red
            Red = Red - RGBFactor
            If Red < 0 then
                Red = 0
                RGBStep = 2
            End If
        Case 2 'Blue
            Blue = Blue + RGBFactor
            If Blue > 255 then
                Blue = 255
                RGBStep = 3
            End If
        Case 3 'Green
            Green = Green - RGBFactor
            If Green < 0 then
                Green = 0
                RGBStep = 4
            End If
        Case 4 'Red
            Red = Red + RGBFactor
            If Red > 255 then
                Red = 255
                RGBStep = 5
            End If
        Case 5 'Blue
            Blue = Blue - RGBFactor
            If Blue < 0 then
                Blue = 0
                RGBStep = 0
            End If
    End Select
    'Light1.color = RGB(Red\10, Green\10, Blue\10)
    l53.colorfull = RGB(Red, Green, Blue)
    l77.colorfull = RGB(Red, Green, Blue)
    l76.colorfull = RGB(Red, Green, Blue)
    l32.colorfull = RGB(Red, Green, Blue)
    l78.colorfull = RGB(Red, Green, Blue)
    l78b.colorfull = RGB(Red, Green, Blue)
    l37.colorfull = RGB(Red, Green, Blue)
    l1.colorfull = RGB(Red, Green, Blue)
    l1b.colorfull = RGB(Red, Green, Blue)
'    light2.color = RGB(Red, Green, Blue)
'    textbox1.text = Red
'    textbox2.text = Green
'    textbox3.text = Blue
End Sub

 '*********
 ' Ramps
 '*********


	'RampSounds

Dim SoundBall

Sub BallDropSoundCenter()
	PlaySound "BallDrop"
End Sub

Sub BallDropSoundLeft()
	PlaySound "BallDrop", 0, 1, -0.2
End Sub

Sub BallDropSoundRight()
	PlaySound "BallDrop", 0, 1, 0.2
End Sub


Sub WireStartAcmag_Hit()
 Set SoundBall = Activeball 'Ball-assignment
 playsound "WireRamp",-1,0.6,pan(ActiveBall)                    '-1 = looping, Panning = -1 bis 1, je nach Position
End Sub


Sub WireStartLockFreeze_Hit()
 Set SoundBall = Activeball 'Ball-assignment
 playsound "WireRamp",-1,0.6,pan(ActiveBall)                    '-1 = looping, Panning = -1 bis 1, je nach Position
End Sub


Sub WireStartSuperJets_Hit()
 Set SoundBall = Activeball 'Ball-assignment
 playsound "WireRamp",-1,0.6,pan(ActiveBall)                   '-1 = looping, Panning = -1 bis 1, je nach Position
End Sub


Sub WireStartLeftRamp_Hit()
 Set SoundBall = Activeball 'Ball-assignment
 playsound "WireRamp",-1,0.6,pan(ActiveBall)                    '-1 = looping, Panning = -1 bis 1, je nach Position
End Sub


Sub WireStartRightRamp_Hit()
 Set SoundBall = Activeball 'Ball-assignment
 playsound "WireRamp",-1,0.6,pan(ActiveBall)                    '-1 = looping, Panning = -1 bis 1, je nach Position
End Sub


Sub WireStartMiddleRamp_Hit()
 Set SoundBall = Activeball 'Ball-assignment
 playsound "WireRamp",-1,0.6,pan(ActiveBall)                    '-1 = looping, Panning = -1 bis 1, je nach Position
End Sub


Sub WireStartBottomPopper_Hit()
 Set SoundBall = Activeball 'Ball-assignment
 playsound "WireRamp",-1,0.6,pan(ActiveBall)                    '-1 = looping, Panning = -1 bis 1, je nach Position
End Sub


Sub WireStopLeft_Hit(): StopSound "WireRamp":PlaySound "WireRamp_Stop": vpmTimer.AddTimer 200, "BallDropSoundLeft'": End Sub

Sub WireStopRight_Hit(): StopSound "WireRamp":PlaySound "WireRamp_Stop": vpmTimer.AddTimer 200, "BallDropSoundRight'": End Sub

Sub WireStopRetina_Hit(): StopSound "WireRamp":PlaySound "WireRamp_Stop": vpmTimer.AddTimer 200, "BallDropSoundLeft'": End Sub

Sub WireStopSuperJets_Hit(): StopSound "WireRamp":PlaySound "WireRamp_Stop": vpmTimer.AddTimer 200, "BallDropSoundCenter'": End Sub



 '********
 'Diverter
 '********

Sub DiverterRight(Enabled)
	If Enabled then
        DiverterR.rotatetoend
		DiverterOff.isdropped = 1
		DiverterOn.isdropped = 0
		PlaySound SoundFX ("DiverterRight",DOFContactors)
		Else
		DiverterOff.isdropped = 0
		DiverterOn.isdropped = 1
		DiverterR.rotatetostart
		Playsound SoundFX ("DiverterRight",DOFContactors)
	End if
End Sub

 '***************
 ' StandupTargets
 '***************

Sub Standup77_Hit: vpmTimer.pulseSw 77:Standup77p.RotX=Standup77p.RotX +15:Playsound SoundFX("target",DOFContactors):Me.TimerEnabled = 1: End Sub
Sub Standup77_Timer:Standup77p.RotX=Standup77p.RotX -15:Me.TimerEnabled = 0: End Sub

Sub Standup87_Hit: vpmTimer.pulseSw 87:Standup87p.RotX=Standup87p.RotX +15:Playsound SoundFX("target",DOFContactors):Me.TimerEnabled = 1: End Sub
Sub Standup87_Timer:Standup87p.RotX=Standup87p.RotX -15:Me.TimerEnabled = 0: End Sub


Sub Standup78_Hit: vpmTimer.pulseSw 78:Standup78p.RotZ=Standup78p.RotZ +15:Playsound SoundFX("target",DOFContactors):Me.TimerEnabled = 1: End Sub
Sub Standup78_Timer:Standup78p.RotZ=Standup78p.RotZ -15:Me.TimerEnabled = 0: End Sub

Sub Standup38_Hit: vpmTimer.pulseSw 38:Standup38p.RotZ=Standup38p.RotZ +15:Playsound SoundFX("target",DOFContactors):Me.TimerEnabled = 1: End Sub
Sub Standup38_Timer:Standup38p.RotZ=Standup38p.RotZ -15:Me.TimerEnabled = 0: End Sub

Sub Standup57_Hit: vpmTimer.pulseSw 57:Standup57p.RotZ=Standup57p.RotZ +15:Playsound SoundFX("target",DOFContactors):Me.TimerEnabled = 1: End Sub
Sub Standup57_Timer:Standup57p.RotZ=Standup57p.RotZ -15:Me.TimerEnabled = 0: End Sub

Sub Standup58_Hit: vpmTimer.pulseSw 58:Standup58p.RotZ=Standup58p.RotZ +15:Playsound SoundFX("target",DOFContactors):Me.TimerEnabled = 1: End Sub
Sub Standup58_Timer:Standup58p.RotZ=Standup58p.RotZ -15:Me.TimerEnabled = 0: End Sub

Sub Standup56_Hit: vpmTimer.pulseSw 56:Standup56p.RotZ=Standup56p.RotZ +15:Playsound SoundFX("target",DOFContactors):Me.TimerEnabled = 1: End Sub
Sub Standup56_Timer:Standup56p.RotZ=Standup56p.RotZ -15:Me.TimerEnabled = 0: End Sub






 '*********
 ' Bumper
 '*********

Sub leftjetbumper_Hit:vpmTimer.PulseSw 43:Playsound SoundFX ("bumperleft",DOFContactors):Me.TimerEnabled = 1: End Sub
Sub Leftjetbumper_Timer:Me.TimerEnabled = 0:End Sub
Sub rightjetbumper_Hit:vpmTimer.PulseSw 45:Playsound SoundFX ("bumperright",DOFContactors):Me.TimerEnabled = 1: End Sub
Sub rightjetbumper_Timer:Me.TimerEnabled = 0: End Sub



'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, LStep, TStep

Sub RightSlingShot_Slingshot
    PlaySound SoundFX ("right_slingshot",DOFContactors), 0, 1, 0.05, 0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    vpmTimer.PulseSw 42
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub



Sub LeftSlingShot_Slingshot
    PlaySound SoundFX ("left_slingshot",DOFContactors),0,1,-0.05,0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    vpmTimer.PulseSw 41
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub



Sub TopSlingShot_Slingshot
	PlaySound SoundFX ("right_slingshot",DOFContactors),0,1,0,0.05
	TSling.Visible = 0
	TSling1.Visible = 1
	Sling3.TransZ = -20
	TStep = 0
	TopSlingshot.TimerEnabled = 1
	vpmTimer.PulseSw 44
End Sub

Sub TopSlingShot_Timer
	Select Case TStep
		Case 3:TSling1.Visible = 0:TSling2.Visible = 1:sling3.TransZ = -10
		Case 4:TSling2.Visible = 0:TSling.Visible = 1:Sling3.TransZ = 0:TopSlingShot.TimerEnabled = 0
	End Select
	TStep = TStep +1
End Sub

Sub UpperRebound_Slingshot
	vpmTimer.PulseSw 54
End Sub

Sub LowerRebound_Slingshot
	vpmTimer.PulseSw 88
End Sub



 '*********
 'Update GI
 '*********


Dim gistep, xx, obj
   gistep = 1 / 8

Sub UpdateGI(no, step)
	If step > 1 Then 
		DOF 200, DOFOn
	Else
		DOF 200, DOFOff
	End If
    Select Case no

'		'Upper Right String
        Case 1
            For each xx in GIString2:xx.IntensityScale = gistep * step:next

		'Upper Left String
		Case 2
            For each xx in GIString3:xx.IntensityScale = gistep * step:next

		'Lower Right String
		Case 3
            For each xx in GIString4:xx.IntensityScale = gistep * step:next
			Table1.ColorGradeImage = "grade_" & step
			For each xx in GIString4: if xx.IntensityScale = 0 then Table1.ColorGradeImage = "grade_1":End if:next
'		'Lower Left String
		Case 4
            For each xx in GIString5:xx.IntensityScale = gistep * step:next
			Table1.ColorGradeImage = "grade_" & step
			For each xx in GIString5: if xx.IntensityScale = 0 then Table1.ColorGradeImage = "grade_1":End if:next
    End Select
End Sub



' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^1.5 / 100)
End Function

Function Vol2(ball1, ball2) ' Calculates the Volume of the sound based on the speed of two balls
    Vol2 = (Vol(ball1) + Vol(ball2) ) / 2
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
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

Const tnob = 9 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = FALSE
    Next
End Sub

Sub RollingSoundUpdate()
    Dim BOT, b
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = FALSE
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = TRUE
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
        Else
            If rolling(b) = TRUE Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = FALSE
            End If
        End If
    Next
End Sub



'**********************
' Ball Collision Sound
'**********************


Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub



'****************************
'     Realtime Updates
' called by the MotorCallBack
'****************************

Sub RealTimeUpdates
    'flippers
    LeftFlipperP.RotY = LeftFlipper.CurrentAngle
    LeftFlipperP1.RotY = LeftFlipper1.CurrentAngle
    RightFlipperP.RotY = RightFlipper.CurrentAngle
' rolling sound
    RollingSoundUpdate
' Plunger update
    PlungerPTimer
' ramp gate
	upperleftflipperspinnerp.RotX = upperleftflipperspinner.currentangle +90
	rightrampenterspinnerp.RotX = rightrampenterspinner.currentangle +95
	rightrampexitspinnerp.RotX = rightrampexitspinner.currentangle +90
	centerrampspinnerp.RotX = centerrampspinner.currentangle +90
	leftrampenterspinnerp.RotX = leftrampenterspinner.currentangle +90
	leftrampexitspinnerp.RotX = leftrampexitspinner.currentangle +90
	siderampenterspinnerp.RotX = siderampenterspinner.currentangle +95
	siderampexitspinnerp.RotZ = siderampexitspinner.currentangle
	retinagatep.RotX = retinagate.currentangle +90
' diverter
    DiverterP.RotY = DiverterR.CurrentAngle
End Sub




'******************************
' Diverse Collection Hit Sounds
'******************************


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub


Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
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
		Case 1 : PlaySound "flip_hit_1", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub RandomSoundHole()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "Hole1", 0, 0.4, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 2:PlaySound "Hole2", 0, 0.4, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 3:PlaySound "Hole4", 0, 0.4, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End Select
End Sub