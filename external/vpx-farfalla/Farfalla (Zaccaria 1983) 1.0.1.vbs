'*******************************************************************************************************
'
'									 Farfalla Zaccaria 1983 v1.0.1
'								http://www.ipdb.org/machine.cgi?id=824
'
'											Created by Kiwi
'
'*******************************************************************************************************

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'********************************************** OPTIONS ************************************************

'****************************************** Volume Settings **********************************

Const RolVol = 1	'Ball Rolling 
Const MroVol = 1	'Wire Ramps Rolling
Const ProVol = 1	'Plastic Ramps Rolling
Const MhiVol = 1	'Metals Hit
Const RubVol = 1	'Rubbers Collision
Const NudVol = 1	'Nudge
Const PlpVol = 1	'Plunger pull
Const PlfVol = 1	'Plunger fire
Const SliVol = 1	'Slingshots
Const BumVol = 1	'Bumpers
Const SwiVol = 1	'Rollovers
Const TarVol = 1	'Targets
Const TadVol = 1	'Targets drop
Const TreVol = 1	'Targets reset
Const GatVol = 1	'Gates
Const RudVol = 1	'Ramp up and down
Const KieVol = 1	'Kicker eject
Const KidVol = 1	'Kicker Drain
Const FluVol = 1	'Flippers up
Const FldVol = 1	'Flippers down
Const KnoVol = 1	'Knocker

'********************************************** ROMs

Const cGameName = "farfalla"
'Const cGameName = "farffp"			'Free Play
'Const cGameName = "farfalli"		'Italian
'Const cGameName = "farfifp"		'Italian Free Play
'Const cGameName = "farfallg"		'German
'Const cGameName = "farfgfp"		'German Free Play

'************************ Ball: 50 unit is standard ball size ** Mass=(50^3)/125000 ,(BallSize^3)/125000

Const BallSize = 50

Const BallMass = 1.05

'************************ Ball Shadow: 0 hidden , 1 visible

Const BallSHW = 1

'************************ CabRails and rail lights Hidden/Visible in FS mode: 0 hidden , 1 visible

Const RailsVisible = 0

'************************ Color Grading LUT: 1 = Active, any other value = disabled

Const LutEnabled = 1

'************************ General Illumination Mode: 0 GI on in play, 1 GI always on

Const GiMode = 1

'************************ 0 Backglass on in FSS, 1 Backglass on and Backdrop off in DT, 2 Backglass on in FS

Const BackG = 0

'************************************************** DMD ************************************************

Dim DmdHidden, ModePlay
ModePlay = Table1.ShowDT + Table1.ShowFSS - BackG

'********************************************** DT Mode

 If ModePlay = -1 Then

'************************ DMD hidden 1, not hidden 0
	DmdHidden = 1
'************************

da1.Visible=1:da2.Visible=1:da3.Visible=1:da4.Visible=1:da5.Visible=1:da6.Visible=1:da7.Visible=1
db1.Visible=1:db2.Visible=1:db3.Visible=1:db4.Visible=1:db5.Visible=1:db6.Visible=1:db7.Visible=1
dc1.Visible=1:dc2.Visible=1:dc3.Visible=1:dc4.Visible=1:dc5.Visible=1:dc6.Visible=1:dc7.Visible=1
dd1.Visible=1:dd2.Visible=1:dd3.Visible=1:dd4.Visible=1:dd5.Visible=1:dd6.Visible=1:dd7.Visible=1
de1.Visible=1:de2.Visible=1:de3.Visible=1:de4.Visible=1:de5.Visible=1:de6.Visible=1:de7.Visible=1
EMReel6.Visible=1:EMReel7.Visible=1:EMReel13.Visible=1:EMReel17.Visible=1:EMReel27.Visible=1
EMReel31.Visible=1:EMReel45.Visible=1:EMReel56.Visible=1
EMReel72.Visible=1:EMReel76.Visible=1:EMReel77.Visible=1:EMReel78.Visible=1
l60a.Visible=1:l62a.Visible=1:l66a.Visible=1:l67a.Visible=1
End If

'********************************************** FS Mode

 If ModePlay = 0 Then

'************************ DMD hidden 1, not hidden 0
	DmdHidden = 0
'************************

	CabRailSX.Visible=RailsVisible
	CabRailDX.Visible=RailsVisible
	l60a.Visible=0:l62a.Visible=0:l66a.Visible=0:l67a.Visible=0
End If

 If B2SOn = True Then DmdHidden = 1

'********************************************** FSS Mode

 If ModePlay = -2 Then

'************************ DMD hidden 1, not hidden 0
	DmdHidden = 1
'************************

LED1x0.Visible=1:LED2x0.Visible=1:LED3x0.Visible=1:LED4x0.Visible=1:LED5x0.Visible=1:LED6x0.Visible=1:LED7x0.Visible=1
LED8x0.Visible=1:LED9x0.Visible=1:LED10x0.Visible=1:LED11x0.Visible=1:LED12x0.Visible=1:LED13x0.Visible=1:LED14x0.Visible=1
LED1x000.Visible=1:LED1x100.Visible=1:LED1x200.Visible=1:LED1x300.Visible=1:LED1x400.Visible=1:LED1x500.Visible=1:LED1x600.Visible=1
LED2x000.Visible=1:LED2x100.Visible=1:LED2x200.Visible=1:LED2x300.Visible=1:LED2x400.Visible=1:LED2x500.Visible=1:LED2x600.Visible=1
LEDax000.Visible=1:LEDax100.Visible=1:LEDax200.Visible=1:LEDax300.Visible=1:LEDax400.Visible=1:LEDax500.Visible=1:LEDax600.Visible=1
l60a.Visible=0:l62a.Visible=0:l66a.Visible=0:l67a.Visible=0
End If

'******************************************** OPTIONS END **********************************************

LoadVPM "01120100", "zac2.vbs", 3.37

'Set Controller = CreateObject("b2s.server")

Dim bsTrough, rtBank, ctBank, ltBank, ttBank, TopWard

Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = ""
Const SFlipperOn = ""
Const SFlipperOff = ""
Const SCoin = "coin3"

'************
' Table init.
'************

Sub Table1_Init
	With Controller
		.GameName = cGameName
		.SplashInfoLine = "Farfalla Zaccaria 1983" & vbNewLine & "VPX table by Kiwi 1.0.1"
		.HandleMechanics = 0
		.HandleKeyboard = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.ShowTitle = 0
		.Hidden = DmdHidden
'		.Games(cGameName).Settings.Value("dmd_pos_x")=0
'		.Games(cGameName).Settings.Value("dmd_pos_y")=0
'		.Games(cGameName).Settings.Value("dmd_width")=400
'		.Games(cGameName).Settings.Value("dmd_height")=92
'		.Games(cGameName).Settings.Value("rol") = 0
'		.Games(cGameName).Settings.Value("ddraw") = 0
'		.Games(cGameName).Settings.Value("sound") = 1
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
	Controller.SolMask(0) = 0
	vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
	Controller.Run

	' Nudging
	vpmNudge.TiltSwitch = 10
	vpmNudge.Sensitivity = 1
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, Bumper4, LeftSlingShot, RightSlingShot)

	' Trough
	Set bsTrough = New cvpmBallStack
	With bsTrough
		.InitSw 0, 16, 0, 0, 0, 0, 0, 0
		.InitKick BallRelease, 59, 21
'		.InitEntrySnd "Solenoid", "Solenoid"
'		.InitExitSnd SoundFX("popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
		.Balls = 1
	End With

	' Orange Drop targets
	set rtBank = new cvpmdroptarget
	With rtBank
		.InitDrop Array(Array(sw25, sw25a), sw26, Array(sw27, sw27a)), Array(25, 26, 27)
'		.Initsnd "DROPTARG", "DTResetB"
'		.CreateEvents "rtBank"
	End With

	' Blu Drop targets
	set ctBank = new cvpmdroptarget
	With ctBank
		.InitDrop Array(Array(sw28, sw28a), sw29, sw30, Array(sw31, sw31a)), Array(28, 29, 30, 31)
'		.Initsnd "DROPTARG", "DTResetB"
'		.CreateEvents "ctBank"
	End With

	' Red Drop targets
	set ltBank = new cvpmdroptarget
	With ltBank
		.InitDrop Array(sw36, sw37, sw38, sw39), Array(36, 37, 38, 39)
'		.Initsnd "DROPTARG", "DTResetB"
'		.CreateEvents "ltBank"
	End With

	' Yellow Drop targets
	set ttBank = new cvpmdroptarget
	With ttBank
		.InitDrop Array(sw50, sw51, sw52, sw53, sw54, sw55), Array(50, 51, 52, 53, 54, 55)
'		.Initsnd "DROPTARG", "DTResetB"
'		.CreateEvents "ttBank"
	End With

	' Main Timer init
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	' Init
	RampM.Collidable=0
	SetLamp 150, GiMode
	SetLamp 151, GiMode

	TextLUT.Visible = 0
	LoadLut

End Sub

Sub Table1_Exit():Controller.Stop:End Sub
Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub

'**********
' Keys
'**********

Sub Table1_KeyDown(ByVal Keycode)
	If KeyCode = LeftFlipperKey And PinPlay = 1 And TopWard = 0 Then Controller.Switch(17) = 1:LeftFlipper.RotateToEnd
	If KeyCode = RightFlipperKey And PinPlay = 1 And TopWard = 0 Then Controller.Switch(23) = 1:RightFlipper.RotateToEnd
	If KeyCode = LeftFlipperKey And PinPlay = 1 And TopWard = 1 Then LeftFlipper1.RotateToEnd
	If KeyCode = RightFlipperKey And PinPlay = 1 And TopWard = 1 Then RightFlipper1.RotateToEnd
	If keycode = LeftMagnaSave Then bLutActive = True:TextLUT.Visible = 1
	If keycode = RightMagnaSave Then
	If bLutActive And LutEnabled = 1 Then NextLUT: End If
	End If
	If keycode = LeftTiltKey Then Nudge 90, 4:PlaySound SoundFX("fx_nudge",0), 0, NudVol, -0.1, 0.25
	If keycode = RightTiltKey Then Nudge 270, 4:PlaySound SoundFX("fx_nudge",0), 0, NudVol, 0.1, 0.25
	If keycode = CenterTiltKey Then Nudge 0, 5:PlaySound SoundFX("fx_nudge",0), 0, NudVol, 0, 0.25
	If vpmKeyDown(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback:PlaySoundAtVol "fx_plungerpull", Plunger, PlpVol
End Sub

Sub Table1_KeyUp(ByVal Keycode)
	If KeyCode = LeftFlipperKey Then Controller.Switch(17) = 0:LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
	If KeyCode = RightFlipperKey Then Controller.Switch(23) = 0:RightFlipper.RotateToStart:RightFlipper1.RotateToStart
	If keycode = LeftMagnaSave Then bLutActive = False:TextLUT.Visible = 0
	If vpmKeyUp(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySoundAtVol "plunger2", Plunger, PlfVol * (Plunger.Position/25)
End Sub

'*********
'   LUT
'*********

Dim bLutActive, LUTImage, x

Sub LoadLUT
	bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1) MOD 10:UpdateLUT:SaveLUT:End Sub

Sub UpdateLUT
Select Case LutImage
Case 0:Table1.ColorGradeImage = "LUT0"
Case 1:Table1.ColorGradeImage = "LUT1"
Case 2:Table1.ColorGradeImage = "LUT2"
Case 3:Table1.ColorGradeImage = "LUT3"
Case 4:Table1.ColorGradeImage = "LUT4"
Case 5:Table1.ColorGradeImage = "LUT5"
Case 6:Table1.ColorGradeImage = "LUT6"
Case 7:Table1.ColorGradeImage = "LUT7"
Case 8:Table1.ColorGradeImage = "LUT8"
Case 9:Table1.ColorGradeImage = "LUT9"
End Select
TextLUT.Text = Table1.ColorGradeImage
End Sub

'*********
' Switches
'*********

'Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 40:PlaySoundAtVol SoundFX("jet1",DOFContactors), Bumper1, BumVol:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 41:PlaySoundAtVol SoundFX("jet1",DOFContactors), Bumper2, BumVol:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 42:PlaySoundAtVol SoundFX("jet1",DOFContactors), Bumper3, BumVol:End Sub
Sub Bumper4_Hit:vpmTimer.PulseSw 43:PlaySoundAtVol SoundFX("jet1",DOFContactors), Bumper4, BumVol:End Sub

'Slings
Dim LStep, RStep

Sub LeftSlingshot_Slingshot:vpmTimer.PulseSw 20:LeftSling.Visible=1:SxEmKickerT1.TransX=-26:LStep=0:Me.TimerEnabled=1:PlaySoundAtVol SoundFX("slingshot",DOFContactors), SxEmKickerT1, SliVol:End Sub
Sub LeftSlingshot_Timer
	Select Case LStep
		Case 0:LeftSling.Visible = 1
		Case 1: 'pause
		Case 2:LeftSling.Visible = 0 :LeftSling1.Visible = 1:SxEmKickerT1.TransX=-21
		Case 3:LeftSling1.Visible = 0:LeftSling2.Visible = 1:SxEmKickerT1.TransX=-16.5
		Case 4:LeftSling2.Visible = 0:Me.TimerEnabled = 0:SxEmKickerT1.TransX=0
	End Select
	LStep = LStep + 1
End Sub

Sub RightSlingshot_Slingshot:vpmTimer.PulseSw 21:RightSling.Visible=1:DxEmKickerT1.TransX=-26:RStep=0:Me.TimerEnabled=1:PlaySoundAtVol SoundFX("slingshot",DOFContactors), DxEmKickerT1, SliVol:End Sub
Sub RightSlingshot_Timer
	Select Case RStep
		Case 0:RightSling.Visible = 1
		Case 1: 'pause
		Case 2:RightSling.Visible = 0 :RightSling1.Visible = 1:DxEmKickerT1.TransX=-21
		Case 3:RightSling1.Visible = 0:RightSling2.Visible = 1:DxEmKickerT1.TransX=-16.5
		Case 4:RightSling2.Visible = 0:Me.TimerEnabled = 0:DxEmKickerT1.TransX=0
	End Select
	RStep = RStep + 1
End Sub

'Drop Targets
Sub sw25_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw25, TadVol:End Sub
Sub sw25_Dropped:rtBank.hit 1:End Sub
Sub sw26_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw26, TadVol:End Sub
Sub sw26_Dropped:rtBank.hit 2:End Sub
Sub sw27_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw27, TadVol:End Sub
Sub sw27_Dropped:rtBank.hit 3:End Sub

Sub sw28_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw28, TadVol:End Sub
Sub sw28_Dropped:ctBank.hit 1:End Sub
Sub sw29_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw29, TadVol:End Sub
Sub sw29_Dropped:ctBank.hit 2:End Sub
Sub sw30_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw30, TadVol:End Sub
Sub sw30_Dropped:ctBank.hit 3:End Sub
Sub sw31_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw31, TadVol:End Sub
Sub sw31_Dropped:ctBank.hit 4:End Sub

Sub sw36_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw36, TadVol:End Sub
Sub sw36_Dropped:ltBank.hit 1:End Sub
Sub sw37_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw37, TadVol:End Sub
Sub sw37_Dropped:ltBank.hit 2:End Sub
Sub sw38_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw38, TadVol:End Sub
Sub sw38_Dropped:ltBank.hit 3:End Sub
Sub sw39_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw39, TadVol:End Sub
Sub sw39_Dropped:ltBank.hit 4:End Sub

Sub sw50_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw50, TadVol:Me.TimerEnabled=1:End Sub
Sub sw50_Dropped:ttBank.hit 1:End Sub
Sub sw51_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw51, TadVol:Me.TimerEnabled=1:End Sub
Sub sw51_Dropped:ttBank.hit 2:End Sub
Sub sw52_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw52, TadVol:Me.TimerEnabled=1:End Sub
Sub sw52_Dropped:ttBank.hit 3:End Sub
Sub sw53_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw53, TadVol:Me.TimerEnabled=1:End Sub
Sub sw53_Dropped:ttBank.hit 4:End Sub
Sub sw54_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw54, TadVol:Me.TimerEnabled=1:End Sub
Sub sw54_Dropped:ttBank.hit 5:End Sub
Sub sw55_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw55, TadVol:Me.TimerEnabled=1:End Sub
Sub sw55_Dropped:ttBank.hit 6:End Sub

Sub sw50_Timer:sw50Servo.Move 0,5,50:Me.TimerEnabled=0:End Sub
Sub sw51_Timer:sw51Servo.Move 0,5,50:Me.TimerEnabled=0:End Sub
Sub sw52_Timer:sw52Servo.Move 0,5,50:Me.TimerEnabled=0:End Sub
Sub sw53_Timer:sw53Servo.Move 0,5,50:Me.TimerEnabled=0:End Sub
Sub sw54_Timer:sw54Servo.Move 0,5,50:Me.TimerEnabled=0:End Sub
Sub sw55_Timer:sw55Servo.Move 0,5,50:Me.TimerEnabled=0:End Sub

'Stand Up Targets
Sub sw32_Hit:vpmTimer.PulseSw 32:Psw32.TransY=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw32_Timer:Psw32.TransY=0:Me.TimerEnabled=0:End Sub
Sub sw33_Hit:vpmTimer.PulseSw 33:Psw33.TransY=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw33_Timer:Psw33.TransY=0:Me.TimerEnabled=0:End Sub
Sub sw34_Hit:vpmTimer.PulseSw 34:Psw34.TransY=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw34_Timer:Psw34.TransY=0:Me.TimerEnabled=0:End Sub
Sub sw49_Hit:vpmTimer.PulseSw 49:Psw49.TransY=-5:Me.TimerEnabled=1:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw49_Timer:Psw49.TransY=0:Me.TimerEnabled=0:End Sub

'Rollovers
Sub sw18_Hit:  Controller.Switch(18) = 1:PlaySoundAtVol "sensor", sw18, SwiVol:Psw18.Z=-20:End Sub
Sub sw18_UnHit:Controller.Switch(18) = 0:Psw18.Z=0:End Sub
Sub sw19_Hit:  Controller.Switch(19) = 1:PlaySoundAtVol "sensor", sw19, SwiVol:Psw19.Z=-20:End Sub
Sub sw19_UnHit:Controller.Switch(19) = 0:Psw19.Z=0:End Sub
Sub sw22_Hit:  Controller.Switch(22) = 1:PlaySoundAtVol "sensor", sw22, SwiVol:Psw22.Z=-20:End Sub
Sub sw22_UnHit:Controller.Switch(22) = 0:Psw22.Z=0:End Sub
Sub sw24_Hit:  Controller.Switch(24) = 1:PlaySoundAtVol "sensor", sw24, SwiVol:Psw24.Z=-20:End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:Psw24.Z=0:End Sub
Sub sw35_Hit:  Controller.Switch(35) = 1:PlaySoundAtVol "sensor", sw35, SwiVol:Psw35.Z=-20:End Sub
Sub sw35_UnHit:Controller.Switch(35) = 0:Psw35.Z=0:End Sub
Sub sw44_Hit:  Controller.Switch(44) = 1:Rollover0.Z = -5:RolloverX.Z = -5:PlaySoundAtVol "sensor", sw44, SwiVol:End Sub
Sub sw44_UnHit:Controller.Switch(44) = 0:Rollover0.Z = 0:RolloverX.Z = 0:End Sub
Sub sw45_Hit:  Controller.Switch(45) = 1:PlaySoundAtVol "sensor", sw45, SwiVol:Psw45.Z=-20:End Sub
Sub sw45_UnHit:Controller.Switch(45) = 0:Psw45.Z=0:End Sub
Sub sw46_Hit:  Controller.Switch(46) = 1:PlaySoundAtVol "sensor", sw46, SwiVol:Psw46.Z=-20:End Sub
Sub sw46_UnHit:Controller.Switch(46) = 0:Psw46.Z=0:End Sub
Sub sw47_Hit:  Controller.Switch(47) = 1:PlaySoundAtVol "sensor", sw47, SwiVol:Psw47.Z=-20:End Sub
Sub sw47_UnHit:Controller.Switch(47) = 0:Psw47.Z=0:End Sub

'Rubbers
Sub sw56_Hit:vpmTimer.PulseSw 56:PlaySoundAtBallVol "rubber1", RubVol:End Sub

'Kickers
Sub Drain_Hit:bsTrough.AddBall Me:PlaysoundAtVol "drain1a", Drain, KidVol:End Sub

'Gates
Sub Gate1_Hit:PlaySoundAtBallVol "Gate51", GatVol:End Sub
Sub Gate2_Hit:PlaySoundAtBallVol "Gate51", GatVol:End Sub
Sub Gate3_Hit:PlaySoundAtBallVol "Gate51", GatVol:End Sub
Sub sw48_Hit:swt48.Enabled = 1:PlaySoundAtBallVol "Gate51", GatVol:End Sub

Sub swt48_Hit:vpmTimer.PulseSw 48:End Sub
Sub swt48_UnHit:swt48.Enabled = 0:End Sub

'Fx Sounds
Sub swfx1_Hit:PlaySoundAtBallVol "fx_InMetalrolling", MhiVol:End Sub
Sub swfx2_Hit:PlaySoundAtBallVol "SGATE", MhiVol:End Sub

'*********
'Solenoids
'*********
Solcallback(1) = "LeftKickerF"
Solcallback(2) = "RightKickerF"
Solcallback(4) = "KnockerSol"
SolCallback(6) = "rtBankSolDropUp"
SolCallback(7) = "ctBankSolDropUp"
SolCallback(9) = "ltBankSolDropUp"
SolCallback(18) = "ttBankSolDropUp"
SolCallback(21) = "FlipperRelay"
SolCallback(22) = "MovingUpWard"
SolCallback(24) = "SolBallRelease"	'"bsTrough.SolOut"

Sub LeftKickerF(Enabled)
 If Enabled Then
	LeftKF.RotateToEnd
	PlaySoundAtVol SoundFX("flipperup1",DOFFlippers), LeftKF, FluVol
Else
	LeftKF.RotateToStart
	PlaySoundAtVol SoundFX("flipperdown1",DOFFlippers), LeftKF, FldVol
End If
End Sub

Sub RightKickerF(Enabled)
 If Enabled Then
	RightKF.RotateToEnd
	PlaySoundAtVol SoundFX("flipperup1",DOFFlippers), RightKF, FluVol
Else
	RightKF.RotateToStart
	PlaySoundAtVol SoundFX("flipperdown1",DOFFlippers), RightKF, FldVol
End If
End Sub

Sub KnockerSol(Enabled)
 If Enabled Then
	PlaySoundAtVol SoundFX("Knocker",DOFKnocker), sw34, KnoVol
End If
End Sub

Sub FlipperRelay(Enabled)
 If Enabled Then
	TopWard=1
Else
	TopWard=0
	LeftFlipper1.RotateToStart
	RightFlipper1.RotateToStart
		If LeftFlipper1.CurrentAngle < LeftFlipper1.StartAngle - 5 Then
		PlaySoundAtVol SoundFX("flipperdown1",DOFFlippers), LeftFlipper1, FldVol
	End If
		If RightFlipper1.CurrentAngle > RightFlipper1.StartAngle + 5 Then
		PlaySoundAtVol SoundFX("flipperdown1",DOFFlippers), RightFlipper1, FldVol
	End If
End If
End Sub

Sub rtBankSolDropUp(Enabled)
	rtBank.DropSol_On
	PlaySoundAtVol SoundFX("DTResetB",DOFContactors), sw26, TreVol
End Sub

Sub ctBankSolDropUp(Enabled)
	ctBank.DropSol_On
	PlaySoundAtVol SoundFX("DTResetB",DOFContactors), sw29, TreVol
End Sub

Sub ltBankSolDropUp(Enabled)
	ltBank.DropSol_On
	PlaySoundAtVol SoundFX("DTResetB",DOFContactors), sw38, TreVol
End Sub

Sub ttBankSolDropUp(Enabled)
	ttBank.DropSol_On
	PlaySoundAtVol SoundFX("DTResetB",DOFContactors), sw53, TreVol
	sw50Servo.Move 0,5,0:sw51Servo.Move 0,5,0:sw52Servo.Move 0,5,0:sw53Servo.Move 0,5,0:sw54Servo.Move 0,5,0:sw55Servo.Move 0,5,0
End Sub

Sub MovingUpWard(Enabled)
 If Enabled Then
	RampM.Collidable=0
	MovRamp.Move 0,3,12
	PlaySoundAtVol SoundFX("flapopen",DOFContactors), RampaPrim, RudVol
Else
	RampM.Collidable=1
	MovRamp.Move 0,3,0
	PlaySoundAtVol SoundFX("flapclos",DOFContactors), RampaPrim, RudVol
End If
End Sub

Sub SolBallRelease(Enabled)
 If Enabled Then
	bsTrough.ExitSol_On
	PlaySoundAtVol SoundFX("popper",DOFContactors), Drain, KieVol
End If
End Sub

'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
	If Enabled And TopWard=0 Then
		PlaySoundAtVol SoundFX("flipperup1",DOFFlippers), LeftFlipper, FluVol':LeftFlipper.RotateToEnd
	Else
		PlaySoundAtVol SoundFX("flipperdown1",DOFFlippers), LeftFlipper, FldVol':LeftFlipper.RotateToStart
	End If
	If Enabled And TopWard=1 Then
		PlaySoundAtVol SoundFX("flipperup1",DOFFlippers), LeftFlipper1, FluVol':LeftFlipper1.RotateToEnd
	Else
		PlaySoundAtVol SoundFX("flipperdown1",DOFFlippers), LeftFlipper1, FldVol':LeftFlipper1.RotateToStart
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled And TopWard=0 Then
		PlaySoundAtVol SoundFX("flipperup1",DOFFlippers), RightFlipper, FluVol':RightFlipper.RotateToEnd
	Else
		PlaySoundAtVol SoundFX("flipperdown1",DOFFlippers), RightFlipper, FldVol':RightFlipper.RotateToStart
	End If
	If Enabled And TopWard=1 Then
		PlaySoundAtVol SoundFX("flipperup1",DOFFlippers), RightFlipper1, FluVol':RightFlipper1.RotateToEnd
	Else
		PlaySoundAtVol SoundFX("flipperdown1",DOFFlippers), RightFlipper1, FldVol':RightFlipper1.RotateToStart
	End If
End Sub

Sub LeftFlipper_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper1_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper1_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftKF_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightKF_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'Piano Rialzato

Sub HelpT1_Hit:Lift1.Collidable = 1:Lift6.Collidable = 1:MovLift.Move 0,0.8,25:HelpT2.Enabled=1:End Sub	'55

Sub HelpT2_Hit()
 Me.TimerEnabled=1
End Sub

Sub HelpT2_Timer()
 Me.TimerEnabled=0
	Lift6.Collidable=0
'	LiftWireP.RotX=0
	MovLift.Move 0,4,0
	HelpT2.Enabled=0
End Sub

'******************
' RealTime Updates
'******************
Set MotorCallback = GetRef("GameTimer")

Dim PinPlay, LiftAngle, Current20, Last20

Sub GameTimer
    RealTimeUps
	UpdateMultipleLamps
End Sub

Sub TimerMFlipper_Timer()
	FlipperCoperchioSx.RotZ = LeftFlipper.CurrentAngle
	FlipperCoperchioDx.RotZ = RightFlipper.CurrentAngle
	FlipperCoperchioSx1.RotZ = LeftFlipper1.CurrentAngle
	FlipperCoperchioDx1.RotZ = RightFlipper1.CurrentAngle
	LeftKFP.RotZ = LeftKF.CurrentAngle + 180
	RightKFP.RotZ = RightKF.CurrentAngle + 180
End Sub

Sub RealTimeUps
	WireGPR.RotY = ((sw48.CurrentAngle *0.75)+26)
	WireGM1.RotY = Gate1.CurrentAngle
	WireGM2.RotY = Gate2.CurrentAngle
	WireGM3.RotY = Gate3.CurrentAngle
	RampaPrim.RotX = MovRamp.CurrentAngle
	LiftAngle = MovLift.CurrentAngle
	LiftWireP.RotX = LiftAngle
 If LiftAngle >= 5 Then:Lift1.Collidable = 0:Lift2.Collidable = 1:End If
 If LiftAngle >= 10 Then:Lift2.Collidable = 0:Lift3.Collidable = 1:End If
 If LiftAngle >= 15 Then:Lift3.Collidable = 0:Lift4.Collidable = 1:End If
 If LiftAngle >= 20 Then:Lift4.Collidable = 0:Lift5.Collidable = 1:End If
 If LiftAngle >= 25 Then:Lift5.Collidable = 0:End If
 If LiftAngle <= 5 Then:Lift2.Collidable = 0:Lift3.Collidable = 0:Lift4.Collidable = 0:Lift5.Collidable = 0:End If
	sw50Prolunga.Z = 0 - (sw50Servo.CurrentAngle)
	sw51Prolunga.Z = 0 - (sw51Servo.CurrentAngle)
	sw52Prolunga.Z = 0 - (sw52Servo.CurrentAngle)
	sw53Prolunga.Z = 0 - (sw53Servo.CurrentAngle)
	sw54Prolunga.Z = 0 - (sw54Servo.CurrentAngle)
	sw55Prolunga.Z = 0 - (sw55Servo.CurrentAngle)

	Current20 = Controller.Lamp(20)
 If Current20 <> Last20 Then
	If Current20 Then
	vpmNudge.SolGameOn 1
	PinPlay=1
	SetLamp 150, 1
	SetLamp 151, 1
Else
	vpmNudge.SolGameOn 0
	LeftFlipper.RotateToStart
	RightFlipper.RotateToStart
	LeftFlipper1.RotateToStart
	RightFlipper1.RotateToStart
	PinPlay=0
	TopWard=0
	SetLamp 150, GiMode
	SetLamp 151, GiMode
	End If
	Last20 = Current20
End If

End Sub

'*****************UpdateMultipleLamps

Dim Current6, Last6, Current7, Last7, Current13, Last13, Current17, Last17, Current27, Last27, Current31, Last31
Dim Current45, Last45, Current56, Last56, Current72, Last72, Current76, Last76, Current77, Last77, Current78, Last78

Sub UpdateMultipleLamps

	Current6 = Controller.Lamp(6)
 If Current6 <> Last6 Then
	If Current6 Then
	SetLamp 106, 1
Else
	SetLamp 106, 0
End If
	Last6 = Current6
End If

	Current7 = Controller.Lamp(7)
 If Current7 <> Last7 Then
	If Current7 Then
	SetLamp 107, 1
Else
	SetLamp 107, 0
End If
	Last7 = Current7
End If

	Current13 = Controller.Lamp(13)
 If Current13 <> Last13 Then
	If Current13 Then
	SetLamp 113, 1
Else
	SetLamp 113, 0
End If
	Last13 = Current13
End If

	Current17 = Controller.Lamp(17)
 If Current17 <> Last17 Then
	If Current17 Then
	SetLamp 117, 1
Else
	SetLamp 117, 0
End If
	Last17 = Current17
End If

	Current27 = Controller.Lamp(27)
 If Current27 <> Last27 Then
	If Current27 Then
	SetLamp 127, 1
Else
	SetLamp 127, 0
End If
	Last27 = Current27
End If

	Current31 = Controller.Lamp(31)
 If Current31 <> Last31 Then
	If Current31 Then
	SetLamp 131, 1
Else
	SetLamp 131, 0
End If
	Last31 = Current31
End If

	Current45 = Controller.Lamp(45)
 If Current45 <> Last45 Then
	If Current45 Then
	SetLamp 145, 1
Else
	SetLamp 145, 0
End If
	Last45 = Current45
End If

	Current56 = Controller.Lamp(56)
 If Current56 <> Last56 Then
	If Current56 Then
	SetLamp 156, 1
Else
	SetLamp 156, 0
End If
	Last56 = Current56
End If

	Current72 = Controller.Lamp(72)
 If Current72 <> Last72 Then
	If Current72 Then
	SetLamp 172, 1
Else
	SetLamp 172, 0
End If
	Last72 = Current72
End If

	Current76 = Controller.Lamp(76)
 If Current76 <> Last76 Then
	If Current76 Then
	SetLamp 176, 1
Else
	SetLamp 176, 0
End If
	Last76 = Current76
End If

	Current77 = Controller.Lamp(77)
 If Current77 <> Last77 Then
	If Current77 Then
	SetLamp 177, 1
Else
	SetLamp 177, 0
End If
	Last77 = Current77
End If

	Current78 = Controller.Lamp(78)
 If Current78 <> Last78 Then
	If Current78 Then
	SetLamp 178, 1
Else
	SetLamp 178, 0
End If
	Last78 = Current78
End If

'	SetLamp 106, Controller.Lamp(6)
'	SetLamp 107, Controller.Lamp(7)
'	SetLamp 113, Controller.Lamp(13)
'	SetLamp 117, Controller.Lamp(17)
'	SetLamp 127, Controller.Lamp(27)
'	SetLamp 131, Controller.Lamp(31)
'	SetLamp 145, Controller.Lamp(45)
'	SetLamp 156, Controller.Lamp(56)
'	SetLamp 172, Controller.Lamp(72)
'	SetLamp 176, Controller.Lamp(76)
'	SetLamp 177, Controller.Lamp(77)
'	SetLamp 178, Controller.Lamp(78)
End Sub

'**********************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 10 'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If

	If ModePlay = -1 Then UpdateLeds
	If ModePlay = -2 Then UpdateLedsF
    UpdateLamps
End Sub

Sub UpdateLamps
    NFadeL 1, l1
    NFadeL 2, l2
    NFadeL 3, l3
    NFadeL 4, l4
    NFadeL 5, l5
'    NFadeL 6, l6	'Game Over
'    NFadeL 7, l7	'Tilt
    NFadeL 8, l8
    NFadeL 9, l9
    NFadeL 10, l10
    NFadeL 11, l11
    NFadeL 12, l12
    NFadeLm 14, l14
    NFadeL 14, l14a
    NFadeLm 15, l15
    NFadeL 15, l15a
    NFadeL 16, l16
    NFadeL 18, l18
    NFadeLm 19, l19
    NFadeL 19, l19a
    NFadeL 20, l20		'FlipperRelay

    NFadeLm 21, l21
    NFadeLm 21, l21a
    Flash 21, f21
    NFadeLm 22, l22
    NFadeLm 22, l22a
    NFadeL 22, l22b
    NFadeL 23, l23
    NFadeL 24, l24
    NFadeL 25, l25
    NFadeL 26, l26
'    NFadeL 27, l27		'Credits
    NFadeL 28, l28
    NFadeL 29, l29
    NFadeL 30, l30
    NFadeL 32, l32
    NFadeL 33, l33
    NFadeL 34, l34
    NFadeL 35, l35
    NFadeL 36, l36
    NFadeL 38, l38
    NFadeL 39, l39
    NFadeL 40, l40
    NFadeL 41, l41
    NFadeL 42, l42
    NFadeL 43, l43
    NFadeL 44, l44
    NFadeLm 45, l45
    FadeR 45, EMReel45

    NFadeL 46, l46
    NFadeL 47, l47
    NFadeL 48, l48
    NFadeL 49, l49
    NFadeL 50, l50
    NFadeL 51, l51
    NFadeL 52, l52
    NFadeL 53, l53
    NFadeL 54, l54
    NFadeL 55, l55
    NFadeL 57, l57
    NFadeL 58, l58
    NFadeL 59, l59
    NFadeLm 61, l61
    NFadeLm 61, l61a
    Flash 61, f61
    NFadeL 63, l63
    NFadeL 64, l64
    NFadeL 65, l65
    NFadeL 68, l68
    NFadeL 69, l69
    NFadeL 70, l70
    NFadeL 71, l71
    NFadeL 73, l73
    NFadeL 74, l74
    NFadeL 75, l75
    NFadeL 79, l79
    NFadeL 80, l80

' GI Playfield

    NFadeLm 150, Bulb1
    NFadeLm 150, Bulb1b
    NFadeLm 150, Bulb2
    NFadeLm 150, Bulb2b
    NFadeLm 150, Bulb3
    NFadeLm 150, Bulb3b
    NFadeLm 150, Bulb4
    NFadeLm 150, Bulb4b
    NFadeLm 150, Bulb5
    NFadeLm 150, Bulb5b
    NFadeLm 150, Bulb6
    NFadeLm 150, Bulb6b
    NFadeLm 150, Bulb7
    NFadeLm 150, Bulb7b
    NFadeLm 150, Bulb8
    NFadeLm 150, Bulb8b
    NFadeLm 150, Bulb9
    NFadeLm 150, Bulb9b
    NFadeLm 150, Bulb10
    NFadeLm 150, Bulb11
    NFadeLm 150, Bulb11b
    NFadeLm 150, Bulb12
    NFadeLm 150, Bulb12b
    NFadeLm 150, Bulb13
    NFadeLm 150, Bulb13b
    NFadeLm 150, Bulb14
    NFadeLm 150, Bulb14b
    NFadeLm 150, Bulb15
    NFadeLm 150, Bulb15b
    NFadeL 150, Bulb16

' Backdrop

    FadeR 6, EMReel6
    FadeR 7, EMReel7

    FadeR 13, EMReel13

    FadeR 17, EMReel17

    FadeR 27, EMReel27
    FadeR 31, EMReel31

    FadeR 56, EMReel56

    FadeR 72, EMReel72

    FadeR 76, EMReel76
    FadeR 77, EMReel77
    FadeR 78, EMReel78

' Backbox

	Flash 106, f6
	Flash 107, f7

	Flash 113, f13

	Flash 117, f17

	Flash 127, f27

	Flash 131, f31

    Flash 145, f45

	Flash 156, f56

	NFadeLm 60, l60a
	Flash 60, f60
	NFadeLm 62, l62a
	Flash 62, f62
	NFadeLm 66, l66a
	Flash 66, f66
	NFadeLm 67, l67a
	Flash 67, f67

	Flash 172, f72

	Flash 176, f76
	Flash 177, f77
	Flash 178, f78

    Flashm 151, BulbF7
    Flashm 151, BulbF8
    Flashm 151, BulbF9

	Flashm 151, Neon1
	Flashm 151, Neon2
	Flashm 151, Neon3
	Flashm 151, fbb1
	Flashm 151, fbb2
	Flashm 151, fbb3
	Flashm 151, fbb4
	Flashm 151, fbb5
	Flashm 151, fbb6
	Flashm 151, fbb7
	Flashm 151, fbb8
	Flashm 151, fbb9
	Flashm 151, fbb10
	Flashm 151, fbb11
	Flashm 151, fbb12
	Flashm 151, fbb13
	Flashm 151, fbb14
	Flashm 151, fbb15
	Flashm 151, fbb16
	Flashm 151, fbb17
	Flashm 151, fbb18
	Flashm 151, BLuci
	Flash 151, NeonCornice

End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.5   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.07	' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' Lights: old method, using 4 images

Sub FadeL(nr, light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:light.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:light.image = a:light.State = 1:FadingLevel(nr) = 1   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1           'wait
        Case 9:light.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1        'wait
        Case 13:light.image = d:Light.State = 0:FadingLevel(nr) = 0  'Off
    End Select
End Sub

Sub FadeLm(nr, light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:light.image = b
        Case 5:light.image = a
        Case 9:light.image = c
        Case 13:light.image = d
    End Select
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

' Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1            'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
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

' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub FadeR(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.SetValue 0:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.SetValue 2:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1          'wait
        Case 13:object.SetValue 3:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeRm(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1
        Case 5:object.SetValue 0
        Case 9:object.SetValue 2
        Case 13:object.SetValue 3
    End Select
End Sub

'Texts

Sub NFadeT(nr, object, message)
    Select Case FadingLevel(nr)
        Case 4:object.Text = "":FadingLevel(nr) = 0
        Case 5:object.Text = message:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeTm(nr, object, b)
    Select Case FadingLevel(nr)
        Case 4:object.Text = ""
        Case 5:object.Text = message
    End Select
End Sub

' *********************************************************************
' 					Wall, rubber and metal hit sounds
' *********************************************************************

Sub Rubbers_Hit(idx):PlaySoundAtBallVol "rubber1", RubVol:End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / Table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
	Dim AB, BC, CD, DA
	AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
	BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
	CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
	DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)
 
	If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
		InRect = True
	Else
		InRect = False       
	End If
End Function

'*****************************************
'PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, Vol) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, Vol, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVol(soundname, VolMult) ' play a sound at the ball position, like rubbers, targets
    PlaySound soundname, 0, Vol(ActiveBall) * VolMult, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 19 ' total number of balls
Const lob = 0  'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
		aBallShadow(b).Visible = 0
        StopSound("fx_ballrolling" & b)
		StopSound("fx_Rolling_Plastic" & b)
		StopSound("fx_Rolling_Metal" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)

		aBallShadow(b).X = BOT(b).X
		aBallShadow(b).Y = BOT(b).Y
		aBallShadow(b).Height = BOT(b).Z - (BallSize / 2)+1

        If BallVel(BOT(b)) > 1 Then
            rolling(b) = True

'Playfield
			If BOT(b).z < 30 Then
					StopSound("fx_Rolling_Metal" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*RolVol, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
			Else 
'Wire Ramps
				If InRect(BOT(b).x, BOT(b).y, 867,203,927,203,927,1600,867,1600) And BOT(b).z < 130 And BOT(b).z > 55 Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
'Wire lift
			ElseIf InRect(BOT(b).x, BOT(b).y, 340,492,396,509,367,620,303,599) And BOT(b).z < 120 And BOT(b).z > 30 Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )

'Plastic Ramps
			Else
					StopSound("fx_Rolling_Metal" & b):StopSound("fx_ballrolling" & b)
					PlaySound("fx_Rolling_Plastic" & b), -1, Vol(BOT(b) )*3*ProVol, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) ) 
			End If
			End If
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
				StopSound("fx_Rolling_Plastic" & b)
				StopSound("fx_Rolling_Metal" & b)
                rolling(b) = False
            End If
        End If

		' Ball Shadow
		If BallSHW = 1 Then
			aBallShadow(b).Visible = 1
		Else
			aBallShadow(b).Visible = 0
		End If

    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'************************************
'          LEDs Display DT
'************************************

Dim Digits(35)

Set Digits(0) = da1
Set Digits(1) = da2
Set Digits(2) = da3
Set Digits(3) = da4
Set Digits(4) = da5
Set Digits(5) = da6
Set Digits(6) = da7

Set Digits(7) = db1
Set Digits(8) = db2
Set Digits(9) = db3
Set Digits(10) = db4
Set Digits(11) = db5
Set Digits(12) = db6
Set Digits(13) = db7

Set Digits(14) = dc1
Set Digits(15) = dc2
Set Digits(16) = dc3
Set Digits(17) = dc4
Set Digits(18) = dc5
Set Digits(19) = dc6
Set Digits(20) = dc7

Set Digits(21) = dd1
Set Digits(22) = dd2
Set Digits(23) = dd3
Set Digits(24) = dd4
Set Digits(25) = dd5
Set Digits(26) = dd6
Set Digits(27) = dd7

Set Digits(28) = de1
Set Digits(29) = de2
Set Digits(30) = de3
Set Digits(31) = de4
Set Digits(32) = de5
Set Digits(33) = de6
Set Digits(34) = de7

Sub UPdateLEDs
	On Error Resume Next
	Dim ChgLED, ii, jj, chg, stat
	ChgLED = Controller.ChangedLEDs(&H0000003f, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(ChgLED)
			chg = chgLED(ii, 1):stat = chgLED(ii, 2)

			Select Case stat
				Case 0:Digits(chgLED(ii, 0) ).SetValue 0    'empty
				Case 63:Digits(chgLED(ii, 0) ).SetValue 1   '0
				Case 6:Digits(chgLED(ii, 0) ).SetValue 2    '1
				Case 91:Digits(chgLED(ii, 0) ).SetValue 3   '2
				Case 79:Digits(chgLED(ii, 0) ).SetValue 4   '3
				Case 102:Digits(chgLED(ii, 0) ).SetValue 5  '4
				Case 109:Digits(chgLED(ii, 0) ).SetValue 6  '5
				Case 124:Digits(chgLED(ii, 0) ).SetValue 7  '6
				'Case 125:Digits(chgLED(ii, 0) ).SetValue 7  '6
				Case 252:Digits(chgLED(ii, 0) ).SetValue 18  '6,
				Case 7:Digits(chgLED(ii, 0) ).SetValue 8    '7
				Case 127:Digits(chgLED(ii, 0) ).SetValue 9  '8
				Case 103:Digits(chgLED(ii, 0) ).SetValue 10 '9
				'Case 111:Digits(chgLED(ii, 0) ).SetValue 10 '9
				Case 231:Digits(chgLED(ii, 0) ).SetValue 21 '9,
				Case 128:Digits(chgLED(ii, 0) ).SetValue 11  'Comma
				Case 191:Digits(chgLED(ii, 0) ).SetValue 12  '0,
				'Case 832:Digits(chgLED(ii, 0) ).SetValue 2  '1
				'Case 896:Digits(chgLED(ii, 0) ).SetValue 2  '1
				'Case 768:Digits(chgLED(ii, 0) ).SetValue 2  '1
				Case 134:Digits(chgLED(ii, 0) ).SetValue 13  '1,
				Case 219:Digits(chgLED(ii, 0) ).SetValue 14  '2,
				Case 207:Digits(chgLED(ii, 0) ).SetValue 15  '3,
				Case 230:Digits(chgLED(ii, 0) ).SetValue 16  '4,
				Case 237:Digits(chgLED(ii, 0) ).SetValue 17  '5,
				'Case 253:Digits(chgLED(ii, 0) ).SetValue 18  '6,
				Case 135:Digits(chgLED(ii, 0) ).SetValue 19  '7,
				Case 255:Digits(chgLED(ii, 0) ).SetValue 20  '8,
				'Case 239:Digits(chgLED(ii, 0) ).SetValue 21 '9,
			End Select
		Next
	End IF
End Sub

'************************************
'          LEDs Display FSS
'************************************

Dim DigitsF(35)

Set DigitsF(0) = LED1x0
Set DigitsF(1) = LED2x0
Set DigitsF(2) = LED3x0
Set DigitsF(3) = LED4x0
Set DigitsF(4) = LED5x0
Set DigitsF(5) = LED6x0
Set DigitsF(6) = LED7x0

Set DigitsF(7) = LED8x0
Set DigitsF(8) = LED9x0
Set DigitsF(9) = LED10x0
Set DigitsF(10) = LED11x0
Set DigitsF(11) = LED12x0
Set DigitsF(12) = LED13x0
Set DigitsF(13) = LED14x0

Set DigitsF(14) = LED1x000
Set DigitsF(15) = LED1x100
Set DigitsF(16) = LED1x200
Set DigitsF(17) = LED1x300
Set DigitsF(18) = LED1x400
Set DigitsF(19) = LED1x500
Set DigitsF(20) = LED1x600

Set DigitsF(21) = LED2x000
Set DigitsF(22) = LED2x100
Set DigitsF(23) = LED2x200
Set DigitsF(24) = LED2x300
Set DigitsF(25) = LED2x400
Set DigitsF(26) = LED2x500
Set DigitsF(27) = LED2x600

Set DigitsF(28) = LEDax000
Set DigitsF(29) = LEDax100
Set DigitsF(30) = LEDax200
Set DigitsF(31) = LEDax300
Set DigitsF(32) = LEDax400
Set DigitsF(33) = LEDax500
Set DigitsF(34) = LEDax600

Sub UPdateLEDsF
	On Error Resume Next
	Dim ChgLED, ii, jj, chg, stat
	ChgLED = Controller.ChangedLEDs(&H0000003f, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(ChgLED)
			chg = chgLED(ii, 1):stat = chgLED(ii, 2)

			Select Case stat
				Case 0:DigitsF(chgLED(ii, 0) ).ImageA="DigitOff"    'empty
				Case 63:DigitsF(chgLED(ii, 0) ).ImageA="Digit0"   '0
				Case 6:DigitsF(chgLED(ii, 0) ).ImageA="Digit1"    '1
				Case 91:DigitsF(chgLED(ii, 0) ).ImageA="Digit2"   '2
				Case 79:DigitsF(chgLED(ii, 0) ).ImageA="Digit3"   '3
				Case 102:DigitsF(chgLED(ii, 0) ).ImageA="Digit4"  '4
				Case 109:DigitsF(chgLED(ii, 0) ).ImageA="Digit5"  '5
				Case 124:DigitsF(chgLED(ii, 0) ).ImageA="Digit6"  '6
				'Case 125:DigitsF(chgLED(ii, 0) ).ImageA="Digit6"  '6
				Case 252:DigitsF(chgLED(ii, 0) ).ImageA="DigitCom6"  '6,
				Case 7:DigitsF(chgLED(ii, 0) ).ImageA="Digit7"    '7
				Case 127:DigitsF(chgLED(ii, 0) ).ImageA="Digit8"  '8
				Case 103:DigitsF(chgLED(ii, 0) ).ImageA="Digit9" '9
				'Case 111:DigitsF(chgLED(ii, 0) ).ImageA="Digit9" '9
				Case 231:DigitsF(chgLED(ii, 0) ).ImageA="DigitCom9" '9,
				Case 128:DigitsF(chgLED(ii, 0) ).ImageA="DigitComma"  'comma
				Case 191:DigitsF(chgLED(ii, 0) ).ImageA="DigitCom0"  '0,
				'Case 832:DigitsF(chgLED(ii, 0) ).ImageA="Digit1"  '1
				'Case 896:DigitsF(chgLED(ii, 0) ).ImageA="Digit1"  '1
				'Case 768:DigitsF(chgLED(ii, 0) ).ImageA="Digit1"  '1
				Case 134:DigitsF(chgLED(ii, 0) ).ImageA="DigitCom1"  '1,
				Case 219:DigitsF(chgLED(ii, 0) ).ImageA="DigitCom2"  '2,
				Case 207:DigitsF(chgLED(ii, 0) ).ImageA="DigitCom3"  '3,
				Case 230:DigitsF(chgLED(ii, 0) ).ImageA="DigitCom4"  '4,
				Case 237:DigitsF(chgLED(ii, 0) ).ImageA="DigitCom5"  '5,
				'Case 253:DigitsF(chgLED(ii, 0) ).ImageA="DigitCom6"  '6,
				Case 135:DigitsF(chgLED(ii, 0) ).ImageA="DigitCom7"  '7,
				Case 255:DigitsF(chgLED(ii, 0) ).ImageA="DigitCom8"  '8,
				'Case 239:DigitsF(chgLED(ii, 0) ).ImageA="DigitCom9"  '9,
			End Select
		Next
	End IF
End Sub

LED1x0.Y=22
LED2x0.Y=22
LED3x0.Y=22
LED4x0.Y=22
LED5x0.Y=22
LED6x0.Y=22
LED7x0.Y=22

LED8x0.Y=22
LED9x0.Y=22
LED10x0.Y=22
LED11x0.Y=22
LED12x0.Y=22
LED13x0.Y=22
LED14x0.Y=22

LED1x000.Y=22
LED1x100.Y=22
LED1x200.Y=22
LED1x300.Y=22
LED1x400.Y=22
LED1x500.Y=22
LED1x600.Y=22

LED2x000.Y=22
LED2x100.Y=22
LED2x200.Y=22
LED2x300.Y=22
LED2x400.Y=22
LED2x500.Y=22
LED2x600.Y=22

LEDax000.Y=22
LEDax100.Y=22
LEDax200.Y=22
LEDax300.Y=22
LEDax400.Y=22
LEDax500.Y=22
LEDax600.Y=22

Neon1.Y=30:Neon2.Y=30:Neon3.Y=30
fbb1.Y=23:fbb2.Y=23:fbb3.Y=23:fbb4.Y=23:fbb5.Y=23:fbb6.Y=23:fbb7.Y=23:fbb8.Y=23:fbb9.Y=23:fbb10.Y=23
fbb11.Y=23:fbb12.Y=23:fbb13.Y=23:fbb14.Y=23:fbb15.Y=23:fbb16.Y=23:fbb17.Y=23:fbb18.Y=23
BLuci.Y=22.5

f13.Y=23:f17.Y=23:f27.Y=23:f31.Y=23:f72.Y=23:f77.Y=23

f6.Y=58:f7.Y=58:f45.Y=58:f56.Y=58:f60.Y=58:f62.Y=58:f66.Y=58:f67.Y=58:f76.Y=58:f78.Y=58:NeonCornice.Y=58

'16/10 a 16/9
'FSS X Offset da 940 a 955
'FSS X Y Scale da 1,68 a 1,89