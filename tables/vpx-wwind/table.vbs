Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const BallsSize = 50

Const cGameName="whirl_l3",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

LoadVPM "01560000", "S11.VBS", 3.26
Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
Primitive13.visible=1
Else
Ramp16.visible=0
Ramp15.visible=0
Primitive13.visible=0
End if

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
'' Setup solenoid callbacks
SolCallback(1) 			= "bsTrough.SolIn"
SolCallback(2) 			= "bsTrough.SolOut"
SolCallback(3) = "SolRightRampEntryLifter"
SolCallback(4) = "SolLeftLockingKickback"
SolCallback(5) 			= "bsSaucer.SolOut"
SolCallback(6) 			= "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(7) 			= "dtL.SolDropUp" 'Drop Targets
SolCallback(8) 			= "dtT.SolDropUp" 'Drop Targets
SolCallback(11) 		= "SolUpperGI"
SolCallback(13) = "SolRampDiverter"
SolCallback(14) = "trCellar.SolOut"
SolCallback(16) 		= "SolLowerGI"
SolCallback(22) = "SolRightRampEntryDown"
SolCallback(41) 		= "SolSpinWheelsMotor"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
PlaySound SoundFX("fx_Flipperup",DOFContactors):RightFlipper.RotateToEnd:RightFlipper1.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart:RightFlipper1.RotateToStart
     End If
End Sub

' Setup Flasher Callbacks
SolCallback(25) =  "Setlamp 125,"
SolCallback(26) =  "Setlamp 126,"
SolCallback(27) =  "Setlamp 127,"
SolCallback(28) =  "Setlamp 128,"
SolCallback(29) =  "Setlamp 129,"
SolCallback(30) =  "Setlamp 130,"
SolCallback(31) =  "Setlamp 131,"
SolCallback(32) =  "Setlamp 132,"

'backwall Lightning
SolCallback(37) = "Setlamp 137,"
SolCallback(39)	= "Setlamp 139,"
SolCallback(40)	= "Setlamp 140,"

'backwall fan
SolCallback(38)	  		= "SolBackwallFan"

'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************
'Lock ball
Sub SolLeftLockingKickback(enabled)
	if enabled Then
		Kicker3.kick 0,30
		Kicker3.enabled = False
		Kicker2.kick 0,30
		Kicker2.enabled = False
		Kicker1.kick 0,30		
		playsound SoundFX("popper",DOFContactors)
	Else
	end if
End Sub

'Upper PF GI
Sub SolUpperGI(enabled)
	If Enabled Then
		dim xx
		For each xx in UpperGI:xx.State = 0: Next
        PlaySound "fx_relay"
	Else
		For each xx in UpperGI:xx.State = 1: Next
        PlaySound "fx_relay"
	End If
End Sub

'Lower PF GI
Sub SolLowerGI(enabled)
	If Enabled Then
		dim xxxx
		For each xxxx in LowerGI:xxxx.State = 0: Next
        PlaySound "fx_relay"
        Table1.colorgradeimage = "ColorGrade_4"
	Else
		For each xxxx in LowerGI:xxxx.State = 1: Next
        PlaySound "fx_relay"
        Table1.colorgradeimage = "ColorGradeLUT256x16_ConSat"
	End If
End Sub

Sub SolRampDiverter(enabled)
	if enabled Then
		diverter.ObjRotZ = 25
		diverter.collidable = False
		PlaySound "fx_Flipperdown", 0, .67, -0.05, 0.05
	Else
		Diverter.ObjRotZ = 0
		diverter.collidable = True
		PlaySound "fx_Flipperdown", 0, .67, -0.05, 0.05
	end if
End Sub

sub SolRightRampEntryLifter(enabled)
	if enabled Then
		RightRampDown.visible = 0
		RightRampDown.collidable = 0
		RightRampUp.visible = 1
		RightRampUp.collidable = 1
		Controller.Switch(42) = 0
		PlaySound "fx_spinner", 0, .67, -0.05, 0.05
	Else
	end if
end Sub

Sub SolRightRampEntryDown(enabled)
	if enabled Then
		RightRampDown.Visible = 1
		RightRampDown.Collidable = 1
		RightRampUp.Visible = 0
		RightRampup.Collidable = 0
		Controller.Switch(42) = 1
		PlaySound "fx_spinner", 0, .67, -0.05, 0.05
	Else
	end if
End Sub

Sub SolSpinWheelsMotor(enabled)
If enabled Then
	ttMiddleSpinner.MotorOn = True
	ttLeftSpinner.MotorOn = True
	ttRightSpinner.MotorOn = True
	ttMiddleSpinner.Speed = 50
	ttLeftSpinner.Speed = -50
	ttRightSpinner.Speed = -50
	SpinnerStep = 10
	SpinnerMotorOff = False
	SpinnerTimer.Interval = 10
	SpinnerTimer.enabled = True
Else
	SpinnerMotorOff = True
end If
End Sub

'Fan
Sub SolBackwallFan(enabled)
If enabled Then
		playsound SoundFX("Topper_Fan",DOFContactors)
	Else 
		stopsound SoundFX("Topper_Fan",DOFContactors)
	end If
End Sub

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************
Dim bsTrough, bsSaucer, trCellar, ttMiddleSpinner, ttLeftSpinner, ttRightSpinner, dtT, dtL

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
         If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Whirlwind Williams 1990" & vbNewLine & "Created for VPX by Walamab"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 0
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

	PinMAMETimer.Interval	= PinMAMEInterval
	PinMAMETimer.Enabled	= True

	vpmNudge.TiltSwitch		= 1
	vpmNudge.Sensitivity	= 3
	vpmNudge.TiltObj 		= Array(Bumper1,Bumper2,Bumper3,Bumper4,Bumper5,Bumper6,LeftSlingshot,RightSlingshot)

'setup trough
	Set bsTrough = New cvpmBallStack
		bsTrough.InitSw 10, 11, 12, 13, 0, 0, 0, 0
		bsTrough.InitKick BallRelease, 75, 4
		bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsTrough.Balls = 3

'Setup Top Kicker
	Set bsSaucer = New cvpmBallStack
		bsSaucer.InitSaucer sw43, 43, 95, 10
		bsSaucer.InitExitSnd  SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsSaucer.KickForceVar = 3
		bsSaucer.KickAngleVar = 2

' Setup Cellar
Set trCellar = new cvpmTrough
	trCellar.CreateEvents "trCellar", Array(kCellarLeft, kCellarRight)
	trCellar.balls = 0
	trCellar.size = 1
	trCellar.InitEntrySounds "popper_ball", "popper_ball", "popper_ball"
	trCellar.InitExitSounds   SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
	trCellar.addsw 0,20
	trCellar.addsw 1,19
	trCellar.initexit  kCellarLeft, 157,16
	trCellar.StackExitBalls = 0
	trCellar.MaxBallsPerKick = 1
	trCellar.IsTrough = False
	trCellar.Reset

' Setup Turntables
Set ttLeftSpinner = New cvpmTurntable
	ttLeftSpinner.InitTurntable LeftSpinTrigger, 30
	ttLeftSpinner.SpinDown = 10
	ttLeftSpinner.CreateEvents "ttLeftSpinner"
 
 
Set ttMiddleSpinner = New cvpmTurntable
	ttMiddleSpinner.InitTurntable MidSpinTrigger, 40
	ttMiddleSpinner.SpinDown = 10
	ttMiddleSpinner.CreateEvents "ttMiddleSpinner"

Set ttRightSpinner = New cvpmTurntable
	ttRightSpinner.InitTurntable RightSpinTrigger, 30
	ttRightSpinner.SpinDown = 10
	ttRightSpinner.CreateEvents "ttRightSpinner"

'Drop Targets
	Set dtT=New cvpmDropTarget
		dtT.InitDrop sw26,26
		dtT.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

	Set dtL=New cvpmDropTarget
		dtL.InitDrop Array(sw27,sw28,sw29),Array(27,28,29)
		dtL.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

SpinnerStep = 10

Controller.Switch(42) = 1
SolRightRampEntryDown(1)

End Sub


'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback
	If keycode =  RightFlipperKey Then:Controller.Switch(57) = 1:End If
	If keycode = LeftFlipperKey Then:Controller.Switch(58) = 1:End If
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
	If keycode = RightFlipperKey Then:Controller.Switch(57) = 0:End If 
	If keycode = LeftFlipperKey Then:Controller.Switch(58) = 0:End If
End Sub

'**********************************************************************************************************

 ' Drain and Kickers
Sub Drain_Hit:bsTrough.addball me : playsound"drain" : End Sub
Sub sw43_Hit():bsSaucer.addball 0 : playsound"popper_ball" : End Sub

Sub kCellarRight_Hit() : vpmTimer.PulseSw  19 : End Sub

Sub kCellarLeft_Hit() : Controller.Switch(20) = 1 : End Sub
Sub kCellarLeft_Unhit() : Controller.Switch(20) = 0 : End Sub

'Wire triggers
Sub sw15_Hit() : Controller.Switch(15) = 1 : playsound"rollover" : End Sub 
Sub sw15_Unhit() : Controller.Switch(15) = 0 : End Sub
Sub sw16_Hit() : Controller.Switch(16) = 1 : playsound"rollover" : End Sub 
Sub sw16_Unhit() : Controller.Switch(16) = 0 : End Sub
Sub sw17_Hit() : Controller.Switch(17) = 1 : playsound"rollover" : End Sub 
Sub sw17_Unhit() : Controller.Switch(17) = 0 : End Sub
Sub sw18_Hit() : Controller.Switch(18) = 1 : playsound"rollover" : End Sub 
Sub sw18_Unhit() : Controller.Switch(18) = 0 : End Sub
Sub sw36_Hit() : Controller.Switch(36) = 1 : playsound"rollover" : End Sub 
Sub sw36_Unhit() : Controller.Switch(36) = 0 : End Sub
Sub sw37_Hit() : Controller.Switch(37) = 1 : playsound"rollover" : End Sub 
Sub sw37_Unhit() : Controller.Switch(37) = 0 : End Sub
Sub sw38_Hit() : Controller.Switch(38) = 1 : playsound"rollover" : End Sub 
Sub sw38_Unhit() : Controller.Switch(38) = 0 : End Sub
Sub sw39_Hit() : Controller.Switch(39) = 1 : playsound"rollover" : End Sub 
Sub sw39_Unhit() : Controller.Switch(39) = 0 : End Sub
Sub sw59_Hit() : Controller.Switch(59) = 1 : playsound"rollover" : End Sub 
Sub sw59_Unhit() : Controller.Switch(59) = 0 : End Sub

'Stand Up Targets
Sub sw21_Hit() : vpmTimer.PulseSw 21 : End Sub
Sub sw25_Hit() : vpmTimer.PulseSw 25 : End Sub
Sub sw30_Hit() : vpmTimer.PulseSw 30 : End Sub
Sub sw48_Hit() : vpmTimer.PulseSw 48 : End Sub
Sub sw47_Hit() : vpmTimer.PulseSw 47 : End Sub

'Drop Targets
 Sub Sw26_Dropped:dtT.Hit 1 :End Sub 
 
 Sub Sw27_Dropped:dtL.Hit 1 :End Sub  
 Sub Sw28_Dropped:dtL.Hit 2 :End Sub  
 Sub Sw29_Dropped:dtL.Hit 3 :End Sub

'Gate Triggers
Sub sw33_Hit() : vpmTimer.PulseSw 33 : End Sub
Sub sw40_Hit() : vpmtimer.PulseSw 40 : End Sub

'Spinner
Sub sw41_Spin() : vpmTimer.PulseSw 41 : playsound"fx_spinner" : End Sub

'Ramp triggers
Sub sw34_Hit() : vpmTimer.PulseSw 34 : playsound"rollover" : End Sub 
Sub sw35_Hit() : vpmTimer.PulseSw 35 : playsound"rollover" : End Sub 
Sub sw44_Hit() : vpmTimer.PulseSw 44 : playsound"rollover" : End Sub 
Sub sw45_Hit() : vpmTimer.PulseSw 45 : playsound"rollover" : End Sub 

'Bumpers
Sub Bumper1_Hit() : vpmTimer.PulseSw 49 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper2_Hit() : vpmtimer.PulseSw 50 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper3_Hit() : vpmTimer.PulseSw 51 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper4_Hit() : vpmTimer.PulseSw 52 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper5_Hit() : vpmTimer.PulseSw 53 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper6_Hit() : vpmTimer.PulseSw 54 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub

'Scoring Rubbers 
Sub sw60_Hit() : vpmTimer.PulseSw 60 : playsound"flip_hit_3" : End Sub  
Sub sw61_Hit() : vpmTimer.PulseSw 61 : playsound"flip_hit_3" : End Sub 

'Spinning Discs Animation Timer
Dim SpinnerMotorOff, SpinnerStep, ss

Sub SpinnerTimer_Timer()
If Not(SpinnerMotorOff) Then
	LeftWheel.RotY = ss
	MiddleWheel.RotZ = ss
	RightWheel.RotY = ss
	ss = ss + SpinnerStep
Else
	if SpinnerStep < 0 Then
		SpinnerTimer.enabled = False
		ttLeftSpinner.MotorOn = False
		ttleftSpinner.speed = 0
		ttMiddleSpinner.MotorOn = False
		ttMiddleSpinner.Speed = 0
		ttRightSpinner.MotorOn = False
		ttRightSpinner.speed = 0
	Else
	'slow the rate of spin by decreasing rotation step
		SpinnerStep = SpinnerStep - 0.05
		LeftWheel.RotY = ss
		MiddleWheel.RotZ = ss
		RightWheel.RotY = ss
		ss = ss + SpinnerStep
	End If
End If
End Sub

'Lock Ball
Sub Kicker1_Hit() : Kicker2.enabled = True : End Sub
Sub Kicker2_Hit() : Kicker3.enabled = True : End Sub
Sub sw22_Hit() : Controller.Switch(22) = 1 : playsound"rollover" : debug.print "hit lock 1": End Sub 
Sub sw22_Unhit() : Controller.Switch(22) = 0 : debug.print "unhit lock 1": End Sub
Sub sw23_Hit() : Controller.Switch(23) = 1 : playsound"rollover": debug.print "hit lock 2" : End Sub 
Sub sw23_Unhit() : Controller.Switch(23) = 0 : debug.print "unhit lock 2": End Sub
Sub sw24_Hit() : Controller.Switch(24) = 1 : playsound"rollover" : debug.print"hit Lock 3": End Sub 
Sub sw24_Unhit() : Controller.Switch(24) = 0 : debug.print "unhit lock 3" : End Sub

'Generic Ramp Sounds
Sub Trigger1_Hit() : playsound"Ball Drop" : End Sub 
Sub Trigger2_Hit() : playsound"Ball Drop" : End Sub
Sub Trigger3_Hit() : playsound"Ball Drop" : End Sub

Sub Trigger4_Hit() : playsound"Wire Ramp" : End Sub


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
LampTimer.Interval = 5 'lamp fading speed
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


Sub UpdateLamps
	NfadeL 1, l1
	NfadeL 2, l2 'Backglass Upper Jets On
	NfadeL 3, l3 'Backglass  Score 250K
	NfadeL 4, l4 'Backglass  Extra Ball On
	NfadeL 5, l5 'Backglass  2 Bank Targets 100K
	NfadeL 6, l6 'Backglass  500K
	NfadeL 7, l7 'Backglass Lite million
	NfadeL 8, l8 'Backglass Lower Jets On
    NfadeL 9, l9
	NfadeL 10, l10
	NfadeL 11, l11
	NfadeL 12, l12
	NfadeL 13, l13
	NfadeL 14, l14
	NfadeL 15, l15
	NfadeL 16, l16
	NfadeL 17, l17
	NfadeL 18, l18
	NfadeL 19, l19
	NfadeL 20, l20
	NfadeL 21, l21
	NfadeL 22, l22
	NfadeL 23, l23
	NfadeL 24, l24
	NfadeL 25, l25
	NfadeL 26, l26
	NfadeL 27, l27
	NfadeL 28, l28
	NfadeL 29, l29
	NfadeL 30, l30
	NfadeL 31, l31
	NfadeL 32, l32
	NfadeLm 33, l33 'Yellow Bumper
	NfadeL 33, l33a
	NfadeLm 34, l34 'Yellow Bumper
	NfadeL 34, l34a
	NfadeLm 35, l35 'Yellow Bumper
	NfadeL 35, l35a
	NfadeLm 36, l36 'Red Bumper
	NfadeL 36, l36a
	NfadeLm 37, l37 'Red Bumper
	NfadeL 37, l37a
	NfadeLm 38, l38 'Red Bumper
	NfadeLm 38, L38a
	NfadeL 39, l39a
    NfadeL 40, l40a
	NfadeL 41, l41
	NfadeL 42, l42
	NfadeL 43, l43
	NfadeL 44, l44
	NfadeL 45, l45
	NfadeL 46, l46
	NfadeL 47, l47
	NfadeL 48, l48
	NfadeL 49, l49
	NfadeL 50, l50
	NfadeL 51, l51
	NfadeL 52, l52
	NfadeL 53, l53
	NfadeL 54, l54
	NfadeL 55, l55
	NfadeL 56, l56
	NfadeL 57, l57
	NfadeL 58, l58
	NfadeL 59, l59
	NfadeL 60, l60
	NfadeL 61, l61
	NfadeL 62, l62
	NfadeL 63, l63
	NfadeL 64, l64


'Solenoid Controlled Flashers

	NfadeLm 125, f125
	NfadeL 125, f125a

	NfadeL 126, f126

	NfadeLm 127, f127
	NfadeLm 127, f127a
	NfadeL 127, f127b

	NfadeLm 128, f128
	NfadeL 128, f128a

	NfadeLm 129, f129
	NfadeLm 129, f129a
	NfadeL 129, f129b

	NfadeLm 130, f130
	NfadeL 130, f130a

	NfadeLm 131, f131
	NfadeL 131, f131a

	NfadeL 132, f132

NfadeLm 137, Light001
NfadeLm 139, Light002
NfadeLm 140, Light003

End Sub



' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.4   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.2 ' slower speed when turning off the flasher
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

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
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

' Reels

Sub FadeR(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.SetValue 0:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1              'wait
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
        Case 3:object.SetValue 3
    End Select
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

'Hide Reels if DT false
	If DesktopMode = True Then
	dim xxxxxx
		For each xxxxxx in BG:xxxxxx.visible = 1: Next
	Else
		For each xxxxxx in BG:xxxxxx.visible = 0: Next
	End If

'**********************************************************************************************************
'**********************************************************************************************************

'****************************************************************************************************
'****************************************************************************************************
'									Start of VPX Call Backs
'****************************************************************************************************
'****************************************************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 56
    PlaySound SoundFX("right_slingshot",DOFContactors), 0, 1, 0.05, 0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 55
     PlaySound SoundFX("left_slingshot",DOFContactors),0,1,-0.05,0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
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
    Vol = Csng(BallVel(ball) ^2 / 400)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 7 ' total number of balls
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
        StopSound("fx_ballrolling" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

    For b = 0 to UBound(BOT)
      If BallVel(BOT(b) ) > 1 Then
        rolling(b) = True
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/7, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/12, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          rolling(b) = False
        End If
      End If
 ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub BallShadow_Timer()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7)
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


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*6, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*6, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*6, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*6, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*6, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*6, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*6, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*6, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*6, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub


