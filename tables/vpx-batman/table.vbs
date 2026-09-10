Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const Ballsize = 55
Const BallMass = 1.5

LoadVPM "01120100", "DE.VBS", 3.36

'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 1
Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = "fx_solenoidoff"
Const SCoin = "fx_Coin"

'Solenoids
SolCallback(1)  = "bsTrough.SolIn"									'6-ball lockout								(1L)
SolCallback(2)  = "bsTrough.SolOut"									'ball eject									(2L)
SolCallback(3)  = "bsLScoop.SolOut"									'Left Scoop                          		(3L)
SolCallback(4)  = "SolAutoPlungerIM"								'Autolaunch                             	(4L)
'NOT USED																										(5L)
SolCallback(6)	= "bsRScoop.SolOut"									'Right Scoop		                    	(6L)
'NOT USED																										(7L)
SolCallback(8)  = "Solknocker"										'Knocker							  		(8L)
SolCallback(9)  = "SetLamp 109,"        							'FlashLamp x4 (2 backbox + 2 pf)			(09)
'SolCallback(10) = ""												'L/R Relay 									(10)
SolCallback(11) = "SolGi"											'GI Relay                             		(11)
SolCallback(12) = "SetLamp 112,"                           			'FlashLamp x4 (1 pf + 3 backbox)			(12)
SolCallback(13) = "SetLamp 113,"									'FlashLamp x4 (2 pf + 2 backbox)        	(13)
SolCallBack(14) = "SetLamp 114,"                          			'FlashLamp x4 (1 pf + 3 backbox)			(14)
'SolCallBack(15) = ""												'Ticket Dispenser							(15)
'SolCallBack(16) = ""												'Bar Motor									(16)
'SolCallBack(17) = ""												'Left Bumper								(17)
'SolCallBack(18) = ""												'Center Bumper								(18)
'SolCallBack(19) = ""			 									'Right Bumper								(19)
'SolCallBack(20) = ""												'Left Slingshot								(20)
'SolCallBack(21) = ""												'Right Slingshot							(21)
SolCallback(22) = "SolDiv"                           				'Ramp Diverter                      		(22)
'NOT USED																										(23)
'NOT USED																										(24)
SolCallback(25) = "SetLamp 125,"									'Flashlamp X4 (3 pf + backbox        		(1R)
SolCallback(26) = "SetLamp 126,"									'Flashlamp X4 (1 pf + 2 ramp + 1 backbox)	(2R)
SolCallback(27) = "SetLamp 127,"									'Flashlamp X4 (2 backbox + 2 pf)        	(3R)
SolCallback(28) = "SetLamp 128,"									'Flashlamp X4 (2 backbox + 2 pf) 			(4R)
SolCallback(29) = "SetLamp 129,"									'Flashlamp X4 (4 pf)						(5R)
SolCallback(30) = "SetLamp 130,"									'Flashlamp X4 (3 pf + 1 backbox)			(6R)
SolCallback(31) = "SetLamp 131,"									'Flashlamp X4 (3 pf + 1 backbox)			(7R)
SolCallBack(32) = "SetLamp 132,"     								'Flashlamp X4 (2 bat + 2 backbox)			(8R)

SolCallback(46) = "SolRFlipper"                         			'Right Flipper
SolCallback(48) = "SolLFlipper"                         			'Left Flipper

'************
' Table init.
'************

Const cGameName = "btmn_106"

Dim bsTrough, plungerIM, bsLScoop, bsRScoop, mBar

Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Batman, Data East 1991" & vbNewLine & "VPX table by Javier v1.0"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Hidden = 0
		.Games(cGameName).Settings.Value("sound") = 1
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
	End With
    On Error Goto 0

    ' Nudging
    vpmNudge.TiltSwitch = 1
	vpmNudge.Sensitivity = 2 
    vpmNudge.tiltobj = Array(LeftSlingShot,RightSlingShot,Bumper1B,Bumper2B,Bumper3B)

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	' Trough
     Set bsTrough = new cvpmTrough 
     With bsTrough
		.Size = 3
		.InitSwitches Array (13,12,11)
		.EntrySw = 10
		.InitExit BallRelease, 90, 6
		.InitEntrySounds "fx_drain", SoundFX(SSolenoidOn,DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
		.InitExitSounds  SoundFX(SSolenoidOn,DOFContactors), SoundFX("fx_ballrel",DOFContactors)
		.Balls = 3
		.CreateEvents "bsTrough", Drain
     End With

     ' Scoop Left
	Set bsLScoop = New cvpmSaucer
	With bsLScoop
	    .InitKicker Sw39b, 39,50, 60, 1.56
		.InitSounds "scoopenter", SoundFX(SSolenoidOn,DOFContactors), SoundFX("salidadebola",DOFContactors)
		.CreateEvents "bsLScoop", sw39b
    End With

     ' Scoop Right
	Set bsRScoop = New cvpmTrough
	With bsRScoop
		.Size = 2
		.InitSwitches Array (52,53)
		.InitExit Sw52, 192, 20
		.InitEntrySounds "fx_chapa", SoundFX(SSolenoidOn,DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
		.InitExitSounds  SoundFX(SSolenoidOn,DOFContactors), SoundFX("salidadebola",DOFContactors)
		.Balls = 0
    End With

    ' Impulse Plunger
    Const IMPowerSetting = 55
    Const IMTime = 0.6
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .switch 14
        .InitExitSnd SoundFX("bumper_retro",DOFContactors), SoundFX("fx_target",DOFContactors)
        .CreateEvents "plungerIM"
    End With

	' Bar Motor
     set mBar = new cvpmMech
     with mBar
         .MType = vpmMechOneSol + vpmMechReverse + vpmMechNonLinear 
         .Sol1 = 16
         .Length = 130
         .Steps = 50
		 .addsw 50,0,0
		 .addsw 51,47,49
		 .acc=0
		 .ret=0
         .Callback = GetRef("UpdateBar")
         .Start
     End with

      Controller.Switch(50) = 0 
      Controller.Switch(51) = 1

	  RampDiverter.Isdropped = 1

   If Table1.ShowDT = False then
       Ramp26.visible = 0
       Ramp32.visible = 0
       Ramp33.visible = 0
       Ramp34.visible = 0
       Ramp35.visible = 0
       Ramp36.visible = 0
   End If
End Sub

'******************
'Keys Up and Down
'*****************

Sub Table1_KeyDown(ByVal Keycode)
    If keycode = PlungerKey Then Plunger.Pullback
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal Keycode)
    If keycode = plungerkey then Plunger.Fire 
    If vpmKeyUp(keycode) Then Exit Sub
End Sub

Sub Table1_Paused : Controller.Pause = True : End Sub
Sub Table1_unPaused : Controller.Pause = False : End Sub
Sub Table1_Exit() : Controller.Pause = False : Controller.Stop() : End Sub

'*****************
'Solenoids
'*****************

'AutoPlunger
Sub SolAutoPlungerIM(Enabled)
    If Enabled Then
        PlungerIM.AutoFire
    End If
End Sub

'Knocker
Sub SolKnocker(enabled)
If enabled then Playsound SoundFX("fx_knocker",DOFKnocker)
End Sub

'CavTargets
Dim CPos
 Sub UpdateBar(acurrpos, aspeed, alastpos)
     CPos = acurrpos
     StopSound"motor": PlaySound"motor"
     Sw49a.TransY = 50 - CPos
     Sw49b.TransY = 50 - CPos
     If CPos => 45 Then Sw49.isdropped = 1
     If CPos =< 25 Then Sw49.isdropped = 0     
 End Sub

'Ramp Diverter
Sub SolDiv(enabled)
	If enabled Then
		RampDiverter.Isdropped = 0
        PlaySound "fx_diverter"
 		RampDiverter2.Isdropped = 1 
        Primitive_RampDiverter.RotY = 12
	Else
		RampDiverter.Isdropped = 1 
        StopSound "fx_Ramp"
        PlaySound "fx_diverter"
 		RampDiverter2.Isdropped = 0
        Primitive_RampDiverter.RotY = 0
	End If
End Sub

' Flippers
Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("FlipperUpLeft", DOFFlippers), 0, 1, -0.1, 0.15
        LeftFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_Flipperdown", DOFFlippers), 0, 1, -0.1, 0.15
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("FlipperUpRight", DOFFlippers), 0, 1, 0.1, 0.15
        RightFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_Flipperdown", DOFFlippers), 0, 1, 0.1, 0.15
        RightFlipper.RotateToStart
    End If
End Sub

'*****************
' Switches
'*****************

' Slingshots
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("left_slingshot", DOFContactors), 0, 1, -0.05, 0.05
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 47
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    PlaySound SoundFX("right_slingshot", DOFContactors), 0, 1, 0.05, 0.05
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 48
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' Bumpers
Sub Bumper1B_Hit:vpmTimer.PulseSw 54:PlaySound SoundFX("fx_bumper1",DOFContactors), 0, 1, -0.1, 0.15:End Sub
Sub Bumper2B_Hit:vpmTimer.PulseSw 56:PlaySound SoundFX("fx_bumper3",DOFContactors), 0, 1, 0, 0.15:End Sub
Sub Bumper3B_Hit:vpmTimer.PulseSw 55:PlaySound SoundFX("fx_bumper2",DOFContactors), 0, 1, 0.1, 0.15:End Sub

'Top Lanes
Sub Sw17_Hit():Playsound "fx_sensor":Controller.Switch(17)=1: End Sub
Sub Sw17_UnHit():Controller.Switch(17)=0: End Sub

Sub Sw18_Hit():Playsound "fx_sensor":Controller.Switch(18)=1: End Sub
Sub Sw18_UnHit():Controller.Switch(18)=0: End Sub

Sub Sw19_Hit():Playsound "fx_sensor":Controller.Switch(19)=1: End Sub
Sub Sw19_UnHit():Controller.Switch(19)=0: End Sub


Sub Sw21_Hit():Playsound "fx_sensor":Controller.Switch(21)=1: End Sub
Sub Sw21_UnHit():Controller.Switch(21)=0: End Sub

Sub Sw22_Hit():Playsound "fx_sensor":Controller.Switch(22)=1: End Sub
Sub Sw22_UnHit():Controller.Switch(22)=0: End Sub

Sub Sw23_Hit():Playsound "fx_sensor":Controller.Switch(23)=1: End Sub
Sub Sw23_UnHit():Controller.Switch(23)=0: End Sub

Sub Sw24_Hit():Playsound "fx_sensor":Controller.Switch(24)=1: End Sub
Sub Sw24_UnHit():Controller.Switch(24)=0: End Sub

'Center Ramp
Sub Sw28_Hit():Playsound "fx_sensor":Controller.Switch(28)=1: End Sub
Sub Sw28_UnHit():Controller.Switch(28)=0: End Sub

'Center Ramp Exit
Sub Sw29_Hit()
	If ActiveBall.VelY > 0 Then
		PlaySound"plastic_ramp"
	Else
		StopSound"plastic_ramp"
	End If
   Controller.Switch(29)=1
 End Sub
Sub Sw29_UnHit():Controller.Switch(29)=0: End Sub

' Left Banks Targets
Sub Sw33_Hit:vpmTimer.PulseSw 33 :MoveTarget33 :PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0.15, 0.15:End Sub
Sub Sw34_Hit:vpmTimer.PulseSw 34 :MoveTarget34 :PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0.15, 0.15:End Sub
Sub Sw35_Hit:vpmTimer.PulseSw 35 :MoveTarget35 :PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0.15, 0.15:End Sub

Sub MoveTarget33
	Sw33a.TransZ = 5
	Sw33b.TransZ = 5
	Sw33.Timerenabled = False
	Sw33.Timerenabled = True
End Sub
Sub	Sw33_Timer
	Sw33.Timerenabled = False
	Sw33a.TransZ = 0
	Sw33b.TransZ = 0
End Sub

Sub MoveTarget34
	Sw34a.TransZ = 5
	Sw34b.TransZ = 5
	Sw34.Timerenabled = False
	Sw34.Timerenabled = True
End Sub
Sub	Sw34_Timer
	Sw34.Timerenabled = False
	Sw34a.TransZ = 0
	Sw34b.TransZ = 0
End Sub

Sub MoveTarget35
	Sw35a.TransZ = 5
	Sw35b.TransZ = 5
	Sw35.Timerenabled = False
	Sw35.Timerenabled = True
End Sub
Sub	Sw35_Timer
	Sw35.Timerenabled = False
	Sw35a.TransZ = 0
	Sw35b.TransZ = 0
End Sub

'Joker Eyes and Mouth
Sub Sw36_Hit: PlaySound "kicker_hit": vpmTimer.PulseSw (36) : Sw39.enabled = 0: End Sub
Sub Sw37_Hit: PlaySound "kicker_hit": vpmTimer.PulseSw (37) : Sw39.enabled = 0: End Sub
Sub Sw38_Hit: PlaySound "kicker_hit": vpmTimer.PulseSw (38) : Sw39.enabled = 0: End Sub

'Left Scoop 
Sub Sw39_hit: vpmtimer.addtimer 300, "Sw39.enabled = 0 '" : End Sub
Sub Sw39b_UnHit: vpmtimer.addtimer 300, "Sw39.enabled = 1 '" :End Sub

' Right Banks Targets
Sub Sw41_Hit:vpmTimer.PulseSw 41 :MoveTarget41 :PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0.15, 0.15:End Sub
Sub Sw42_Hit:vpmTimer.PulseSw 42 :MoveTarget42 :PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0.15, 0.15:End Sub
Sub Sw43_Hit:vpmTimer.PulseSw 43 :MoveTarget43 :PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0.15, 0.15:End Sub

Sub MoveTarget41
	Sw41a.TransZ = 5
	Sw41b.TransZ = 5
	Sw41.Timerenabled = False
	Sw41.Timerenabled = True
End Sub
Sub	Sw41_Timer
	Sw41.Timerenabled = False
	Sw41a.TransZ = 0
	Sw41b.TransZ = 0
End Sub

Sub MoveTarget42
	Sw42a.TransZ = 5
	Sw42b.TransZ = 5
	Sw42.Timerenabled = False
	Sw42.Timerenabled = True
End Sub
Sub	Sw42_Timer
	Sw42.Timerenabled = False
	Sw42a.TransZ = 0
	Sw42b.TransZ = 0
End Sub

Sub MoveTarget43
	Sw43a.TransZ = 5
	Sw43b.TransZ = 5
	Sw43.Timerenabled = False
	Sw43.Timerenabled = True
End Sub
Sub	Sw43_Timer
	Sw43.Timerenabled = False
	Sw43a.TransZ = 0
	Sw43b.TransZ = 0
End Sub

'Bat Bar StandUp
Sub Sw49_Hit
    vpmTimer.PulseSw 49
    PlaySound "fx_chapa"
End Sub

Sub Sw52_Hit() : PlaySound "trough": Me.DestroyBall:vpmTimer.Addtimer 1000, "bsRScoop.AddBall" : End Sub

'***************************************************
'	General Illumination
'***************************************************
Dim x

Sub SolGi(enabled)
  If enabled Then
     GiOFF
	Playsound "fx_relay_off"
	Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSatDark"
   Else
     GiON
	Playsound "fx_relay_on"
	Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
 End If
End Sub

Sub GiON
    For each x in aGiLights:x.State = 1:Next
    gi29.IntensityScale=1 : gi30.IntensityScale=1
    Primitive_PlasticRamp.Image = "BatRampMapResized"
Primitive_PlasticRamp.blenddisablelighting = 0.5
Primitive_JokerRamp.blenddisablelighting = 1
End Sub

Sub GiOFF
    For each x in aGiLights:x.State = 0:Next
    gi29.IntensityScale=0 : gi30.IntensityScale=0
    Primitive_PlasticRamp.Image = "BatmanRampMap_resizedOFF"
Primitive_PlasticRamp.blenddisablelighting = 0
Primitive_JokerRamp.blenddisablelighting = 0.3
End Sub

'***************************************************
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
	UpdateLamps
End Sub

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0         ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4       ' used to track the fading state
        FlashSpeedUp(x) = 0.5    ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.35 ' slower speed when turning off the flasher
        FlashMax(x) = 1          ' the maximum value when on, usually 1
        FlashMin(x) = 0          ' the minimum value when off, usually 0
        FlashLevel(x) = 0        ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub UpdateLamps
	nFadeL 1, l1
	nFadeL 2, l2
    nFadeL 3, l3
    nFadeL 4, l4
    nFadeL 5, l5
    nFadeL 6, l6
    nFadeL 7, l7
    nFadeL 8, l8
    nFadeL 9, l9
    nFadeL 10, l10
 
    nFadeL 11, l11
    nFadeL 12, l12
    nFadeL 13, l13
    nFadeLm 14, l14
    NFadeL 14, l14a
    nFadeL 15, l15
    nFadeL 16, l16
    NFadeLm 17, l17
    Flash 17, L17f
    NFadeLm 18, l18
    Flash 18, L18f
    NFadeLm 19, l19
    Flash 19, L19f
    nFadeL 20, l20
 
    nFadeL 21, l21
    nFadeL 22, l22
    NFadeLm 23, l23
    NFadeL 23, l23a
    Flash 23, l23f
    nFadeLm 24, l24
    NFadeL 24, l24a

	nFadeLm 25, FlasherLight25a
	Flashm 25, FlasherL25
	Flash 25, FlasherLight25d

	nFadeLm 26, FlasherLight26a
	Flashm 26, FlasherL26
	Flash 26, FlasherLight26d

	nFadeLm 27, FlasherLight27a
	Flashm 27, FlasherL27
	Flash 27, FlasherLight27d

	nFadeLm 28, FlasherLight28a
	Flashm 28, FlasherL28
	Flash 28, FlasherLight28d

	nFadeLm 29, FlasherLight29a
	Flashm 29, FlasherL29
	Flash 29, FlasherLight29d

    nFadeL 30, l30
 
    nFadeL 31, l31
    nFadeL 32, l32
    nFadeL 33, l33
    nFadeL 34, l34
    nFadeL 35, l35

	nFadeLm 36, l36b
	Flashm 36, l36
	Flash 36, l36Fb

	nFadeLm 37, l37b
	Flash 37, l37

	nFadeLm 38, L38
    nFadeL 38, L38a
	Flashm 38, l38f
'	Flashm 38, FlasherL38
	Flash 38, FlasherL38r

    nFadeL 39, l39
    nFadeL 40, l40
 
    nFadeL 41, l41
    nFadeL 42, l42
    nFadeL 43, l43

	nFadeLm 44, BumperL_Flasher
	nFadeLm 44, BumperL_Flasher_a
	nFadeL 44, BumperL_Flasher1

	nFadeLm 45, BumperB_Flasher
	nFadeLm 45, BumperB_Flasher_a
	nFadeL 45, BumperB_Flasher1

	nFadeLm 46, BumperR_Flasher
	nFadeLm 46, BumperR_Flasher_a
	nFadeL 46, BumperR_Flasher1

    NFadeLm 47, l47
    Flash 47, l47f  
    nFadeL 48, l48

	nFadeLm 49, l49
	Flashm 49, FlasherL49
	Flash 49, l49f

	Flash 55, L55
    NFadeLm 56, l56
    Flash 56, BatCave3
    nFadeL 57, l57
    nFadeL 58, l58
    nFadeL 59, l59
    nFadeL 60, l60
    nFadeL 61, l61
    nFadeL 62, l62
    NFadeLm 63, l63
    Flash 63, BatCave1
    NFadeLm 64, l64 
    Flash 64, BatCave2

	'Flashers
	NFadeObjm 109, Primitive53, "BatCave_bothON", "BatCave_CompleteMap"
	nFadeLm 109, FlasherL9
	nFadeLm 109, FlasherL9b
	Flashm 109, Flasher9
	Flash 109, Flasher9a
	NFadeL 112, Flasher12
'	Flash 113, FlasherL38
	NFadeL 114, Flasher14

	NFadeLm 125, FlasherL1b
	NFadeL 125, Flasher1b

	NFadeObjm 126, Primitive53, "BatCave_bothON", "BatCave_CompleteMap"
	NFadeLm 126, FlasherLight2
	NFadeLm 126, FlasherLight2b
	NFadeL 126, Flasher2b

	NFadeL 127, FlasherLight3
	NFadeL 128, FlasherLight4

	NFadeLm 129, FlasherL1
	NFadeLm 129, FlasherL1a
	NFadeLm 129, FlasherL2
	NFadeLm 129, FlasherL2a
	NFadeLm 129, FlasherL3
	NFadeLm 129, FlasherL3a
	NFadeLm 129, FlasherL4
	NFadeLm 129, FlasherL4a
    NFadeLm 129, FlasherAll
	Flashm 129, FlasherL1c
	Flashm 129, FlasherL2c
    Flashm 129, FlasherL3c
    Flash 129, FlasherL4c

    NFadeLm 130, Flasher6
    NFadeLm 130, Flasher6a
    NFadeLm 130, Flasher6b
    NFadeL 130, Flasher6c

	NFadeLm 131, FlasherLight7a
	NFadeLm 131, FlasherLight7b
	NFadeL 131, FlasherLight7c

	NFadeObjm 132, Primitive_museum, "MuseumMap_on", "MuseumMap_off"
	nFadeLm 132, FlasherLight8
	Flashm 132, Flasher8
	Flash 132, Flasher8a
if FlasherLight8.state = 1 then
 Primitive_museum.BlendDisableLighting = 0.1
else
 Primitive_museum.BlendDisableLighting = 0
end if
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

'Lights, Ramps & Primitives used as 4 step fading lights
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

Sub FadeDisableLighting(nr, a, alvl)
	Select Case FadingLevel(nr)
		Case 4
			a.UserValue = a.UserValue - 0.1
			If a.UserValue < 0 Then 
				a.UserValue = 0
				FadingLevel(nr) = 0
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
		Case 5
			a.UserValue = a.UserValue + 0.50
			If a.UserValue > 1 Then 
				a.UserValue = 1
				FadingLevel(nr) = 1
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
	End Select
End Sub

'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 500)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'****************************************
' Real Time updates
'****************************************

Set MotorCallBack = GetRef("GameTimer")

Sub GameTimer
    RollingUpdate	
    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
 lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
End Sub

'*********** ROLLING SOUND *********************************
Const tnob = 3  ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling:Dim i:For i=0 to (tnob-1):rolling(i) = False:Next:End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch
    BOT = GetBalls
	' stop the sound of deleted balls
	If UBound(BOT)<(tnob -1) Then
		For b = (UBound(BOT) + 1) to (tnob-1)
			rolling(b) = False
			StopSound("fx_ballrolling" & b)
		Next
	End If
	' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 Then
           If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) * 100
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, ballpitch, 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'*********** BALL SHADOW *********************************
Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3)

Sub BallShadow_Timer()
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
        If BOT(b).Z > 20 and BOT(b).Z < 140 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 110
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub

'******************************
'		 Sound FX
'******************************

Sub aMetals_Hit(idx):PlaySound "fx_MetalHit":End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySound "fx_woodhit":End Sub

Sub KickerHelper_Hit:PlaySound "ball_drop":End Sub
Sub UpLaneKickers1_Hit():vpmtimer.addtimer 150, "UpKickers '" End Sub
Sub UpLaneKickers2_Hit():vpmtimer.addtimer 150, "UpKickers '" End Sub
Sub UpLaneKickers3_Hit():vpmtimer.addtimer 150, "UpKickers '" End Sub

Sub Trigger1_Hit
	If ActiveBall.VelY < 0 Then
		PlaySound"fx_ramp"
	Else
		StopSound"plastic_ramp"
	End If
End Sub

Sub Trigger2_Hit():Playsound "plastic_ramp": End Sub
Sub Trigger3_Hit():Playsound "fx_ramp": End Sub

Sub UpKickers()
    Playsound "ball_drop"
End Sub

'Flipper rubber sounds

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.15
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.15
End Sub

' Ball Collision Sound
Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0
End Sub

