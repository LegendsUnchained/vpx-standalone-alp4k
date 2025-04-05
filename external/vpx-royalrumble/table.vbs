Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="wwfr_106",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

LoadVPM"01000100","de.vbs",3.38
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
SolCallback(1)="bsTrough.SolOut"
SolCallback(2)="SolBallRelease"
SolCallback(3)="bsShooter.SolOut"
SolCallback(4)="bsRingKick.SolOut"
SolCallback(5)="bsSVUK.SolOut"
SolCallback(6)="bsVUK.SolOut"
SolCallback(7)=		"dtL.SolDropUp" 'Drop Targets
SolCallback(8)=  	"vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(9)=		"dtT.SolDropUp" 'Drop Targets
SolCallback(11)=	"PFGI"
SolCallback(12)=	"SolBackDiverter" 'BackWall Ramp Diverter
SolCallback(13)=	"SolFlipperDiverter" 'Upper PF Left diverter
'SolCallback(16)="ShakeTimer.Enabled="
SolCallback(22)= 	"SolRightDiv"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):RightFlipper.RotateToEnd:RightFlipper1.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart:RightFlipper1.RotateToStart
     End If
End Sub

'FLASHERS
SolCallback(25) = "SetLamp 125," '3x lower PF flash
SolCallback(26) = "SetLamp 126," '3x 4bank dt flash
SolCallback(27) = "SetLamp 127," 'spotlight flash
SolCallback(28) = "SetLamp 128," 'super vuk flash
SolCallback(29) = "SetLamp 129," 'right orbit flash
SolCallback(30) = "SetLamp 130," 'left ramp flash
SolCallback(31) = "SetLamp 131," 'top lanes flash
SolCallback(32) = "SetLamp 132," 'upper pf drop target flash

'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

Sub SolBallRelease(Enabled):If Enabled Then:Controller.Switch(15)=0:BallRelease.Kick 62,8::End If:End Sub

Sub SolFlipperDiverter(enabled)'Upper PF Left diverter
	If Enabled Then
		 PlaySound SoundFX("Popper",DOFContactors):Flipper3.RotateToEnd
		 LeftDiverter.ObjRotZ =-35
	Else
		 PlaySound SoundFX("Popper",DOFContactors):Flipper3.RotateToStart
		 LeftDiverter.ObjRotZ =0
	End If
End Sub

Sub SolBackDiverter(Enabled) 'BackWall Ramp Diverter
	If Enabled Then
		 PlaySound SoundFX("Popper",DOFContactors):Flipper4.RotateToEnd
		 RampDiverter.ObjRotZ =40
	Else
		 PlaySound SoundFX("Popper",DOFContactors):Flipper4.RotateToStart
		 RampDiverter.ObjRotZ =0
	End If
End Sub

Sub solRightDiv(Enabled)
	If Enabled Then
		RightDivT.Enabled = 1
		PlaySound SoundFX("Popper",DOFContactors)
	End If
 End Sub

Dim divstate
divstate = False
Sub RightDivT_Timer()
	If divstate = False then
		If RightDiverter.ObjRotZ <= 45 then 
			RightDiverter.ObjRotZ = RightDiverter.ObjRotZ + 2
		Else
			RightDivT.Enabled = False
			divstate = True
		End If
	Else
		If RightDiverter.ObjRotZ => 0 then 
			RightDiverter.ObjRotZ = RightDiverter.ObjRotZ - 2
		Else
			RightDivT.Enabled = False
			divstate = False
		End If
	End If
End Sub

Sub URFlip_Timer()
	If divstate = false then
		TRDiv1.IsDropped = False
		TRDiv2.IsDropped = True
	Else
		TRDiv1.IsDropped = True
		TRDiv2.IsDropped = False
	End If
End Sub


'Playfield GI
Sub PFGI(Enabled)
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 0: Next
        PlaySound "fx_relay"
Table1.colorgradeimage = "ColorGrade_4"
	Else
		For each xx in GI:xx.State = 1: Next
        PlaySound "fx_relay"
Table1.colorgradeimage = "ColorGradeLUT256x16_ConSat"
	End If
End Sub

'Primitive Flipper Code
Sub FlipperTimer_Timer
	LFLogo.roty = LeftFlipper.currentangle  + 0
	RFLogo.roty = RightFlipper.currentangle + 0
	sw58P.Rotz = sw58.Currentangle
	sw41P.Rotz = sw41.Currentangle
	sw43P.Rotz = sw43.Currentangle
End Sub


'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsShooter, dtL, dtT, bsSVUK, bsVUK, bsRingKick, oTarget, Opt

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "WWF Royal Rumble Data East"&chr(13)&"You Suck"
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

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

	vpmNudge.TiltSwitch=1:
	vpmNudge.Sensitivity=5:
	vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot,Bumper1,Bumper2,Bumper3)

	Set bsTrough=New cvpmBallStack
		bsTrough.InitSw 0,14,13,12,11,10,9,0
		bsTrough.InitKick kicker4,62,8
		bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsTrough.Balls=6
	
	Set bsShooter=New cvpmBallStack
		bsShooter.InitSaucer Shooter,16,0,40
		bsShooter.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsShooter.KickForceVar=3

	Set dtL=New cvpmDropTarget
		dtL.InitDrop Array(sw21,sw22,sw23,sw24),Array(21,22,23,24)
		dtL.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

	Set dtT=New cvpmDropTarget
		dtT.InitDrop Array(sw30,sw31,sw32),Array(30,31,32)
		dtT.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

	Set bsSVUK=New cvpmBallStack
		bsSVUK.InitSw 0,20,0,0,0,0,0,0
		bsSVUK.InitKick sw20a,10,20
		bsSVUK.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

 	Set bsVUK=New cvpmBallStack
		bsVUK.InitSw 0,40,0,0,0,0,0,0
		bsVUK.InitKick sw40,180,7
		bsVUK.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

 	Set bsRingKick=New cvpmBallStack
		bsRingKick.InitSaucer sw28,28,170,7
		bsRingKick.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

		CapKicker.CreateBall
		CapKicker.Kick 140,6

		TRDiv2.IsDropped=1

End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyDownHandler(keycode) Then Exit Sub
 	If Keycode = LeftFlipperKey then
		Controller.Switch(63) = 1
	End If
 	If Keycode = RightFlipperKey then 
		Controller.Switch(64) = 1
	End If
    If keycode = PlungerKey Then  Controller.Switch(62) = 1
    If keycode = RightMagnaSave Then  Controller.Switch(62) = 1
    If keycode = LeftMagnaSave Then  Controller.Switch(62) = 1

End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
 	If Keycode = LeftFlipperKey then
		Controller.Switch(63) = 0
	End If
 	If Keycode = RightFlipperKey then 
		Controller.Switch(64) = 0
	End If
    If keycode = PlungerKey Then  Controller.Switch(62) = 0
    If keycode = RightMagnaSave Then  Controller.Switch(62) = 0
    If keycode = LeftMagnaSave Then  Controller.Switch(62) = 0

End Sub

'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : playsound"drain" : End Sub
Sub BallRelease_Hit:Controller.Switch(15)=1:End Sub
Sub Shooter_Hit:bsShooter.AddBall 0:End Sub 
Sub sw20_Hit:bsSVUK.AddBall Me : playsound "popper_ball": End Sub
Sub sw39_Hit:sw39.DestroyBall:vpmTimer.PulseSwitch 39,200,"AddToVUK" : playsound "popper_ball": End Sub
Sub AddToVUK(swNo):bsVUK.AddBall 0:End Sub
Sub sw40_Hit:bsVUK.AddBall Me : playsound "popper_ball": End Sub

'Drop Targets
 Sub Sw21_Dropped:dtL.Hit 1 :End Sub  
 Sub Sw22_Dropped:dtL.Hit 2 :End Sub  
 Sub Sw23_Dropped:dtL.Hit 3 :End Sub
 Sub Sw24_Dropped:dtL.Hit 4 :End Sub

'Wire Triggers
Sub SW17_Hit:Controller.Switch(17)=1 : playsound"rollover" : End Sub 
Sub SW17_unHit:Controller.Switch(17)=0:End Sub
Sub SW18_Hit:Controller.Switch(18)=1 : playsound"rollover" : End Sub 
Sub SW18_unHit:Controller.Switch(18)=0:End Sub
Sub SW25_Hit:Controller.Switch(25)=1 : playsound"rollover" : End Sub 
Sub SW25_unHit:Controller.Switch(25)=0:End Sub
Sub SW26_Hit:Controller.Switch(26)=1 : playsound"rollover" : End Sub 
Sub SW26_unHit:Controller.Switch(26)=0:End Sub
Sub sw45_Hit:Controller.Switch(45)=1 : playsound"rollover" : End Sub 
Sub sw45_unHit:Controller.Switch(45)=0:End Sub
Sub sw46_Hit:Controller.Switch(46)=1 : playsound"rollover" : End Sub 
Sub sw46_unHit:Controller.Switch(46)=0:End Sub
Sub sw47_Hit:Controller.Switch(47)=1 : playsound"rollover" : End Sub																	'47
Sub sw47_unHit:Controller.Switch(47)=0:End Sub

Sub sw52_Hit:Controller.Switch(52)=1 : playsound"rollover" : End Sub 
Sub sw52_unHit:Controller.Switch(52)=0:End Sub
Sub sw53_Hit:Controller.Switch(53)=1 : playsound"rollover" : End Sub 
Sub sw53_unHit:Controller.Switch(53)=0:End Sub
Sub sw54_Hit:Controller.Switch(54)=1 : playsound"rollover" : End Sub 
Sub sw54_unHit:Controller.Switch(54)=0:End Sub
Sub sw55_Hit:Controller.Switch(55)=1 : playsound"rollover" : End Sub 
Sub sw55_unHit:Controller.Switch(55)=0:End Sub
Sub sw56_Hit:Controller.Switch(56)=1 : playsound"rollover" : End Sub 
Sub sw56_unHit:Controller.Switch(56)=0:End Sub

 'Stand Up Targets
Sub sw33_hit:vpmTimer.pulseSw 33 : End Sub 
Sub sw34_hit:vpmTimer.pulseSw 34 : End Sub 
Sub sw35_hit:vpmTimer.pulseSw 35 : End Sub 
Sub sw36_hit:vpmTimer.pulseSw 36 : End Sub 
Sub sw37_hit:vpmTimer.pulseSw 37 : End Sub 
Sub sw38_hit:vpmTimer.pulseSw 38 : End Sub 

'Gate Triggers
Sub sw41_hit:vpmTimer.pulseSw 41 : End Sub 
Sub sw42_hit:vpmTimer.pulseSw 42 : End Sub
Sub sw43_hit:vpmTimer.pulseSw 43 : End Sub
Sub sw44_hit:vpmTimer.pulseSw 44 : End Sub

'Bumpers
Sub Bumper1_Hit : vpmTimer.PulseSw(49) : playsound SoundFX("fx_bumper3",DOFContactors): End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(50) : playsound SoundFX("fx_bumper3",DOFContactors): End Sub
Sub Bumper3_Hit : vpmTimer.PulseSw(51) : playsound SoundFX("fx_bumper3",DOFContactors): End Sub


'***************************************************
'Mini PlayField
'***************************************************
'Kicker
Sub sw28_Hit:bsRingKick.AddBall 0 : playsound "popper_ball": End Sub

'Drop Targets
 Sub Sw30_Dropped:dtT.Hit 1 :End Sub  
 Sub Sw31_Dropped:dtT.Hit 2 :End Sub  
 Sub Sw32_Dropped:dtT.Hit 3 :End Sub

'Wire Triggers
Sub sw29_Hit:Controller.Switch(29)=1 : playsound"rollover" : End Sub  																	'29
Sub sw29_unHit:Controller.Switch(29)=0:End Sub

'Gate Triggers
Sub sw58_hit:vpmTimer.pulseSw 58 : End Sub

'***************************************************
'***************************************************


'Generic Sounds
Sub Trigger1_Hit: playsound"fx_ballrampdrop" : End Sub 
Sub Trigger2_Hit: playsound"fx_ballrampdrop" : End Sub 
Sub Trigger3_Hit: playsound"fx_ballrampdrop" : End Sub 
Sub Trigger4_Hit: playsound"fx_ballrampdrop" : End Sub 

Sub Trigger5_Hit: playsound"Wire Ramp" : End Sub 
Sub Trigger6_Hit: playsound"Wire Ramp" : End Sub 
Sub Trigger7_Hit: playsound"Wire Ramp" : End Sub 

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


Sub UpdateLamps()
NFadeL 1, L1
NFadeL 2, L2
NFadeL 3, L3
NFadeL 4, L4
NFadeL 5, L5
NFadeL 6, L6
NFadeL 7, L7
NFadeL 8, L8
NFadeL 9, L9
NFadeL 10, L10
NFadeL 11, L11
NFadeL 12, L12
NFadeLm 13, L13
FadeDisableLighting 13, L13P, 3
NFadeL 14, L14
NFadeL 17, L17
NFadeL 18, L18
NFadeL 19, L19
NFadeL 20, L20
NFadeL 21, L21
NFadeL 22, L22
NFadeL 23, L23
NFadeL 24, L24
NFadeL 25, L25
NFadeL 26, L26
NFadeL 27, L27
NFadeLm 28, L28a
NFadeL 28, L28b
NFadeL 29, L29
NFadeL 30, L30
NFadeL 31, L31
NFadeL 32, L32
NFadeL 33, L33
NFadeL 34, L34
NFadeL 35, L35
NFadeL 36, L36
NFadeL 37, L37
NFadeL 38, L38
NFadeL 39, L39
NFadeL 40, L40
NFadeL 41, L41
NFadeL 42, L42
NFadeL 43, L43
NFadeL 44, L44
FadeDisableLighting 45, L45, 3
NFadeL 46, L46
NFadeLm 47, L47
FadeDisableLighting 47, L47P, 3
NFadeL 48, L48
NFadeL 49, L49
NFadeL 50, L50
NFadeL 51, L51
NFadeLm 52, L52
FadeDisableLighting 52, L52P, 5
FadeDisableLighting 53, L53, 3
FadeDisableLighting 54, L54, 3
NFadeL 55, L55
NFadeL 56, L56
FadeDisableLighting 57, L57, 3
FadeDisableLighting 58, L58, 3
NFadeL 60, L60
NFadeL 61, L61 'Bumper 1
NFadeL 62, L62 'Bumper 2 
NFadeL 63, L63 'Bumper 3
NFadeL 64, L64

'Solenoid Controlled Flahers

NFadeLm 125, f125
NFadeLm 125, f125a
NFadeL 125, f125b

NFadeLm 126, f126
NFadeL 126, f126a

NFadeLm 127, f127a
NFadeL 127, f127

NFadeL 128, f128

NFadeLm 129, f129a
NFadeL 129, f129

NFadeLm 130, f130a
NFadeLm 130, f130b
NFadeL 130, f130

NFadeL 131, f131

NFadeLm 132, f132a
NFadeL 132, f132

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

 'Reels
Sub FadeReel(nr, reel)
    Select Case FadingLevel(nr)
        Case 2:FadingLevel(nr) = 0
        Case 3:FadingLevel(nr) = 2
        Case 4:reel.Visible = 0:FadingLevel(nr) = 3
        Case 5:reel.Visible = 1:FadingLevel(nr) = 1
    End Select
End Sub

 'Inverted Reels
Sub FadeIReel(nr, reel)
    Select Case FadingLevel(nr)
        Case 2:FadingLevel(nr) = 0
        Case 3:FadingLevel(nr) = 2
        Case 4:reel.Visible = 1:FadingLevel(nr) = 3
        Case 5:reel.Visible = 0:FadingLevel(nr) = 1
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

'**********************************************************************************************************
'**********************************************************************************************************
'	Start of VPX functions
'**********************************************************************************************************
'**********************************************************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 27
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
	vpmTimer.PulseSw 19
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
    Vol = Csng(BallVel(ball) ^2 / 5000)
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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
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

 '**********************
'Flipper Shadows
'***********************
Sub RealTime_Timer
  lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
BallShadowUpdate
End Sub


Sub BallShadowUpdate()
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
ballShadow(b).opacity = 90
End If
    Next	
End Sub

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
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
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub
