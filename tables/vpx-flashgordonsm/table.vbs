'*************************************
'Flash Gordon (Bally 1981) - IPD No. 874
'VPX by rothbauerw
'VP9 by Sinbad
'************************************
 
Option Explicit
Randomize

Dim VolumeDial, CollectionVolume, ContrastSetting, MusicSnippet, enableBallControl, RampImage, OutLanePosts, BallReflection, PFSpeed, KickerAccuracy
Dim GlowAmountDay, InsertBrightnessDay 
Dim GlowAmountNight, InsertBrightnessNight 

'********************
'Options
'********************

PFSpeed = 0						'0 - Slower, 1 - Faster
OutLanePosts = 1				'0 - Easy, 1 - Medium, 2 - Hard	
KickerAccuracy = 2				'0 - High capture rate, 1 - medium capture rate, 2 - low capture rate (Two Way Kicker)
RampImage = 3 					'0 - shiny ramps, 1 - flat ramps, 2 - brushed steel, 3 - FG Ramps
BallReflection = 1				'0 - Off, 1 - On (simulated reflection since playfield is raised)

MusicSnippet = 1				' 0 - off, 1 - On: Play music snippet on game load and game over 

VolumeDial = 0.5				'Added Sound Volume Dial (ramps, balldrop, kickers, etc)
CollectionVolume = 10 			'Standard Sound Amplifier (targets, gates, rubbers, metals, etc) use 1 for standard setup

' *** Contrast level, possible values are 0 - 7, can be done in game with magnasave keys **
' *** 0: bright, good for desktop view, daytime settings in insert lighting below *********
' *** 1: same as 0, but with nighttime settings in insert lighting below ******************
' *** 2: darker, better contrast, daytime settings in insert lighting below ***************
' *** 3: same as 2, but with nighttime settings in insert lighting below ******************
' *** etc for 4-7; default is 3 ***********************************************************
ContrastSetting = 3

' *** Insert Lighting settings ************************************************************
' *** The settings below together with ContrastSetting determines how the lighting looks **
' *** for all values: 1.0 = default, useful range 0.1 - 5 *********************************
GlowAmountDay = 0.05
InsertBrightnessDay = 0.8 
GlowAmountNight = 0.5
InsertBrightnessNight = 0.6

Const cGameName = "flashgdn"	'Set ROM flashgdn, flashgdv, flashgdf

enableBallControl = 0 	' 1 to enable, 0 to disable

'********************
'End Options
'********************

Dim Ballsize,BallMass
BallSize = 50
BallMass = 1.7

Dim DesktopMode, NightDay:DesktopMode = Table1.ShowDT:NightDay = Table1.NightDay
Dim UseVPMDMD:UseVPMDMD = DesktopMode
Dim Tilted, tt

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01510000", "Bally.VBS", 3.1

'********************
'Standard definitions
'********************
   
Const UseSolenoids = 1
Const UseLamps = 1
Const UseGI = 0
Const UseSync = 0
 
'Standard Sounds
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = ""
Const SCoin = "FG Coin"

'******************************************************
' 					TABLE INIT
'******************************************************

Dim dtInLine, dtSingle, dt3Target, dt4Target, ReflBall, TwoWayMag

 Sub Table1_Init:
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
 		.SplashInfoLine = "Flash Gordon (Bally 1981) Sound Mod v2.0" & vbNewLine & "VPX Table by Rothbauerw" & vbNewLine & "Sound Mod by Xenonph"
		.Games(cGameName).Settings.Value("rol") = 0 'rotated
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Hidden = 1
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
     End With
 
    '************  Main Timer init  ********************

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

    '************  Nudging   **************************

	vpmNudge.TiltSwitch = 7
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, RightSlingShot, LeftSlingShot)
 
    '************  Trough	**************************
	Set ReflBall = Drain.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Controller.Switch(8) = 1

    '************  Droptargets
	Set dtInLine = new cvpmDropTarget
	With dtInline
		.InitDrop Array(dt1, dt2, dt3), Array(25,26,27)
		.Initsnd  SoundFX("droptarget",DOFContactors),  SoundFX("resetdrop",DOFContactors)
	End With
	
	Set dtSingle = new cvpmDropTarget
	With dtSingle
		.InitDrop Array(dt11), Array(3)
		.Initsnd  SoundFX("droptarget",DOFContactors),  SoundFX("resetdrop",DOFContactors)
	End With

	Set dt3Target = new cvpmDropTarget
	With dt3Target
		.InitDrop Array(dt8, dt9, dt10), Array(23,22,21)
		.Initsnd  SoundFX("droptarget",DOFContactors),  SoundFX("resetdrop",DOFContactors)
	End With

	Set dt4Target = new cvpmDropTarget
	With dt4Target
		.InitDrop Array(dt4, dt5, dt6, dt7), Array(17,18,19,20)
		.Initsnd  SoundFX("droptarget",DOFContactors),  SoundFX("resetdrop",DOFContactors)
	End With

	If DesktopMode Then
		'For each xx in GI:xx.y=xx.y+12:Next
	Else
		railleft.visible=0
		railright.visible=0
	End If

	'************  Adjust GI based on NightDay  ******************
	Dim xx

	For each xx in GI:xx.State = 1: Next

	If NightDay <= 75 And NightDay > 50 Then
		'For each xx in GI:xx.Intensity = xx.intensity*1.0:Next
		'For each xx in insertlights:xx.intensity=xx.intensity*0.5:Next
	ElseIf NightDay <= 50 And NightDay > 25 Then
		'For each xx in GI:xx.Intensity = xx.intensity*1.0:Next
		'For each xx in insertlights:xx.intensity=xx.intensity*0.35:Next
	ElseIf NightDay <= 25 And NightDay > 5 Then
		'For each xx in GI:xx.Intensity = xx.intensity*1.0:Next
		'For each xx in insertlights:xx.intensity=xx.intensity*0.25:Next
	ElseIf NightDay <= 5 Then
		'For each xx in GI:xx.Intensity = xx.intensity*1.0:Next
		'For each xx in insertlights:xx.intensity=xx.intensity*0.15:Next
	End If

	'*** option settings ***
	ColorGrade
	SetOptions()
LoadLUT
End Sub

'******************************************************
' 						KEYS
'******************************************************

Dim up, down, left, right, contball

Sub Table1_KeyDown(ByVal keycode)
	If keycode = LeftTiltKey Then Nudge 90, 3:PlaySound SoundFX("nudge_left", 0)
    If keycode = RightTiltKey Then Nudge 270, 3:PlaySound SoundFX("nudge_right", 0)
    If keycode = CenterTiltKey Then Nudge 0, 3:PlaySound SoundFX("nudge_forward", 0)
 	If keycode = Plungerkey then plunger.PullBack:PlaySound "plungerpull", 0, 2*VolumeDial, 0.25, 0.25
	If vpmKeyDown(keycode) Then Exit Sub

	if keycode = 46 then 
		if contball = 1 then contball = 0 else contball = 1
	end if
	if keycode = 203 then left = 1
	if keycode = 200 then up = 1
	if keycode = 208 then down = 1
	if keycode = 205 then right = 1

	If keycode = 3 Then NextLut

	If keycode = RightMagnaSave Then 
		ContrastSetting = ContrastSetting + 1
		If ContrastSetting > 7 Then ContrastSetting = 7 End If
		ColorGrade		
	End If
	If keycode = LeftMagnaSave Then 
		ContrastSetting = ContrastSetting - 1
		If ContrastSetting < 0 Then ContrastSetting = 0 End If
		ColorGrade		
	End If


End Sub

Sub Table1_KeyUp(ByVal keycode)
	If KeyCode = 6 Then
			Dim x
			x = INT(3 * RND(1) )
			Select Case x
			Case 0:StopSound"0FG00":StopSound"0FG32":StopSound"0FG35":PlaySound"0FG00"
			Case 1:StopSound"0FG00":StopSound"0FG32":StopSound"0FG35":PlaySound"0FG32"
			Case 2:StopSound"0FG00":StopSound"0FG32":StopSound"0FG35":PlaySound"0FG35"
			End Select
			end if
	If KeyCode = 4 Then
			Dim y
			y = INT(3 * RND(1) )
			Select Case y
			Case 0:StopSound"0FG00":StopSound"0FG32":StopSound"0FG35":PlaySound"0FG00"
			Case 1:StopSound"0FG00":StopSound"0FG32":StopSound"0FG35":PlaySound"0FG32"
			Case 2:StopSound"0FG00":StopSound"0FG32":StopSound"0FG35":PlaySound"0CB35"
			End Select
			end if
	If KeyCode = 2 Then
		EndMusic:Drain.timerenabled=0:Bumper1.timerenabled=0:Bumper2.timerenabled=0

	End If
	If keycode = plungerkey then plunger.Fire:PlaySound "plunger", 0, 2*VolumeDial,0.25,0.25
         
	If vpmKeyUp(keycode) Then Exit Sub

	if keycode = 203 then left = 0
	if keycode = 200 then up = 0
	if keycode = 208 then down = 0
	if keycode = 205 then right = 0
End Sub

Sub Table1_Paused: Controller.Pause = 1:End Sub
Sub Table1_UnPaused: Controller.Pause = 0:End Sub
Sub Table1_Exit(): Controller.Stop:End Sub

'******************************************************
'					SOLENOIDS
'******************************************************

SolCallback(7) = "SolOuthole"		           		'7 = 1 - OutHole
SolCallback(6) = "vpmSolSound ""knocker"","    		'6 = 2 - Knocker
SolCallback(4) = "TwoWayKicker_Down"           		'4 = 3 - Saucer Kick Down
SolCallback(8) = "TwoWayKicker_Up"					'8 = 4 - Saucer Kick Up
SolCallback(9) = "controller.switch(3)=false:dtSingle.SolDropUp"				'9 = 5 - Single Drop Target Reset
SolCallback(1) = "Dt4Target.SolDropUp"				'1 = 6 - 4 Drop Target Reset
SolCallback(2) = "Dt3Target.SolDropUp"				'2 = 7 - 3 Drop Target Reset
SolCallback(3) = "DtInLine.SolDropUp"				'3 = 8 - In Line Drop Target
'SolCallback(10) =									'10 = 9 - Left Bumper
'SolCallback(11) =									'11 = 10 - Rigth Bumper
SolCallback(12) = "controller.switch(3)=true:dtSingle.SolDropDown"			'12 = 11 - Single Drop Target Pull Down
'SolCallback(13) =									'13 = 12 - Top Bumper
'SolCallback(14) =									'14 = 13 - Left Slingshot
'SolCallback(15) =									'15 = 14 - Right Slingshot
'SolCallback(18) =									'18 = 15 - Coin Lockout Door
'SolCallback(19) = "vpmNudge.SolGameOn" 				'19 = 16 - KI Relay (Flipper enabled)

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolDtInLine(enabled)
	If enabled Then
		dt1.timerinterval=500
		dt1.timerenabled=True
	End if
End Sub

Sub dt1_Timer()
	me.timerenabled=False
	dtInLine.DropSol_On
End Sub

Sub SolDt3Target(enabled)
	If enabled Then
		dt10.timerinterval=500
		dt10.timerenabled=True
	End if
End Sub

Sub dt10_Timer()
	me.timerenabled=False
	dt3Target.DropSol_On
End Sub

Sub SolDt4Target(enabled)
	If enabled Then
		dt4.timerinterval=500
		dt4.timerenabled=True
	End if
End Sub

Sub dt4_Timer()
	me.timerenabled=False
	dt4Target.DropSol_On
End Sub


'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub StopFGSounds()
    StopSound "FG MusicSnippet"
    StopSound "0FG00"
    StopSound "0FG01"
    StopSound "0FG02"
    StopSound "0FG03"
    StopSound "0FG04"
    StopSound "0FG05"
    StopSound "0FG06"
    StopSound "0FG07"
    StopSound "0FG08"
    StopSound "0FG09"
    StopSound "0FG10"
    StopSound "0FG11"
    StopSound "0FG12"
  '  StopSound "0FG13"
    StopSound "0FG14"
    StopSound "0FG15"
    StopSound "0FG16"
    StopSound "0FG17"
    StopSound "0FG18"
    StopSound "0FG19"
    StopSound "0FG20"
    StopSound "0FG22"
    StopSound "0FG23"
    StopSound "0FG24"
    StopSound "0FG25"
    StopSound "0FG26"
    StopSound "0FG27"
    StopSound "0FG28"
    StopSound "0FG29"
    StopSound "0FG30"
    StopSound "0FG31"
    StopSound "0FG32"
    StopSound "0FG33"
    StopSound "0FG34"
    StopSound "0FG35"
 

End Sub

Sub Drain_Hit()
		
        StopFGSounds:TwoWayKicker.timerenabled=0:Drain.timerenabled=0
        Dim x
	PlaySound "fx_drain", 0 , 0.25 * volumedial
	Controller.Switch(8) = 1
x = INT(16 * RND(1) )

If Tilted = True then Exit sub

Select Case x
Case 0:PlaySound"0FG10"
Case 1:PlaySound"0FG12"
Case 2:PlaySound"0FG31"
Case 3:PlaySound"0FG14"
Case 4:PlaySound"0FG16"
Case 5:PlaySound"0FG18"
Case 6:PlaySound"0FG25"
Case 7:PlaySound"0FG20"
Case 8:PlaySound"0FG31"
Case 9:PlaySound"0FG22"
Case 10:PlaySound"0FG23"
Case 11:PlaySound"0FG24"
Case 12:PlaySound"0FG25"
Case 13:PlaySound"0FG26"
Case 14:PlaySound"0FG28"
Case 15:PlaySound"0FG29"


End Select


End Sub

Sub Drain_timer
            StopFGSounds
			Dim x
			x = INT(4 * RND(1) )
			Select Case x
			Case 0:TwoWayKicker.timerinterval=87500:TwoWayKicker.timerenabled=1: If Tilted = False Then PlaySound"0FG01"
			Case 1:TwoWayKicker.timerinterval=40500:TwoWayKicker.timerenabled=1: If Tilted = False Then PlaySound"0FG02"
			Case 2:TwoWayKicker.timerinterval=50500:TwoWayKicker.timerenabled=1: If Tilted = False Then PlaySound"0FG03"
			Case 3:TwoWayKicker.timerinterval=60000:TwoWayKicker.timerenabled=1: If Tilted = False Then PlaySound"0FG04"

			End Select
Drain.timerenabled=0
End Sub

Sub Drain_UnHit()
	Controller.Switch(8) = 0
End Sub


Sub SolOuthole(enabled)
	If enabled Then 
		If Drain.BallCntOver = 0 Then
			PlaySound SoundFX(SSolenoidOn,DOFContactors), 0, 2*VolumeDial
		Else
            PlaySound"0FG00e":Bumper3.timerinterval=10000:Bumper3.timerenabled=1
			Drain.kick 80,20
			PlaySound SoundFX("Ballrelease",DOFContactors), 0, 2*VolumeDial
		End If
	End If
End Sub

  


'******************************************************
'				TWO-WAY KICKER
'******************************************************

Sub TwoWayKicker_Up(enabled)
	If enabled Then
		If Controller.Switch(30) = True then
			Playsound SoundFX("FG SaucerKick",DOFContactors), 0, 2*VolumeDial:Playsound"0FG00a"
		Else
			Playsound SoundFX("solenoid",DOFContactors), 0, 2*VolumeDial
		End If
		Controller.Switch(30) = 0
		TwoWayKicker.Kick 31,25.5,30
		twkicker2.TransY = 20
		vpmTimer.AddTimer 150, "twkicker2.TransY = 0'"
	End If
	TwoWayKicker.Enabled=False
End Sub

Sub TwoWayKicker_Down(enabled)
	If enabled Then
		If Controller.Switch(30) = True then
			Playsound SoundFX("FG SaucerKick",DOFContactors), 0, 2*VolumeDial:Playsound"0FG00a"
		Else
			Playsound SoundFX("solenoid",DOFContactors), 0, 2*VolumeDial
		End If
		Controller.Switch(30) = 0
		TwoWayKicker.Kick 220.5,21
		twkicker1.TransY = 20
		vpmTimer.AddTimer 150, "twkicker1.TransY = 0'"
	End If
	TwoWayKicker.Enabled=False
End Sub

sub TwoWayKicker_hit()
    StopFGSounds
	PlaySound "FG SaucerHit", 0, 2*VolumeDial
	Controller.Switch(30) = 1
            If Tilted = True then TwoWayKicker.timerenabled=0
            If Tilted = True then Exit sub
            Dim x
			x = INT(7 * RND(1) )
			Select Case x
			Case 0:PlaySound"0FG35":TwoWayKicker.timerinterval=2000:TwoWayKicker.timerenabled=1
			Case 1:PlaySound"0FG15":TwoWayKicker.timerinterval=3000:TwoWayKicker.timerenabled=1
			Case 2:PlaySound"0FG17":TwoWayKicker.timerinterval=4000:TwoWayKicker.timerenabled=1
			Case 3:PlaySound"0FG27":TwoWayKicker.timerinterval=3000:TwoWayKicker.timerenabled=1
			Case 4:PlaySound"0FG34":TwoWayKicker.timerinterval=2000:TwoWayKicker.timerenabled=1
			Case 5:PlaySound"0FG32":TwoWayKicker.timerinterval=3000:TwoWayKicker.timerenabled=1
			Case 6:PlaySound"0FG33":TwoWayKicker.timerinterval=2000:TwoWayKicker.timerenabled=1


			End Select
End sub


Sub TwoWayKicker_timer
            If Tilted = True then TwoWayKicker.timerenabled=0
            If Tilted = True then Exit sub
            StopFGSounds
			Dim x
			x = INT(4 * RND(1) )
			Select Case x
			Case 0:PlaySound"0FG01":Drain.timerinterval=87500:Drain.timerenabled=1
			Case 1:PlaySound"0FG02":Drain.timerinterval=40500:Drain.timerenabled=1
			Case 2:PlaySound"0FG03":Drain.timerinterval=50500:Drain.timerenabled=1
			Case 3:PlaySound"0FG04":Drain.timerinterval=60000:Drain.timerenabled=1

			End Select
TwoWayKicker.timerenabled=0
End Sub


'******************************************************
'						FLIPPERS
'******************************************************


Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFContactors), 0, 2*VolumeDial, -0.1, 0.25
		PlaySound "buzz", -1 , .06, -1
        LeftFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown",DOFContactors), 0, 2*VolumeDial, -0.1, 0.25
		StopSound "buzz"
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFContactors), 0, 2*VolumeDial, 0.1, 0.25
		Playsound "buzz1", -1 , .05, 1
        RightFlipper.RotateToEnd
		RightFlipper2.RotateToEnd	
    Else
        PlaySound SoundFX("fx_flipperdown",DOFContactors), 0, 2*VolumeDial, 0.1, 0.25
		StopSound "buzz1"
        RightFlipper.RotateToStart
		RightFlipper2.RotateToStart 
    End If
End Sub

'******************************************************
'				SLINGSHOTS
'******************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySound SoundFX("right_slingshot",DOFContactors), 0, 1, 0.05, 0.05
    vpmTimer.PulseSw 35
    RightSling.Visible = 0
    RightSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1	
    GI5.State = 0:GI6.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:GI5.State = 1:GI6.State = 1
        Case 3:RightSLing1.Visible = 0:RightSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RightSLing2.Visible = 0:RightSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("left_slingshot",DOFContactors),0,1,-0.05,0.05
    vpmTimer.PulseSw 36
    LeftSling.Visible = 0
    LeftSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    GI7.State = 0:GI8.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:GI7.State = 1:GI8.State = 1
        Case 3:LeftSLing1.Visible = 0:LeftSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LeftSLing2.Visible = 0:LeftSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub



'******************************************************
'				Animated Rubbers
'******************************************************

Dim TenPts1, TenPts2, FiftyPts1, FiftyPts2, Ani1, Ani2, Ani3, Ani4, Ani5, Ani6, Ani7, Ani8

Sub Wall10Pts1_Timer
    Select Case TenPts1
        Case 1:Rubber9a.Visible = 0:Rubber9.Visible = 1:me.TimerEnabled = 0
    End Select
    TenPts1 = TenPts1 + 1
End Sub

Sub Wall10Pts2_Timer
    Select Case TenPts2
        Case 1:Rubber2a.Visible = 0:Rubber2.Visible = 1:me.TimerEnabled = 0
    End Select
    TenPts2 = TenPts2 + 1
End Sub

Sub Wall50Pts1_Timer
    Select Case FiftyPts1
        Case 1:Rubber18a.Visible = 0:Rubber18.Visible = 1:me.TimerEnabled = 0
    End Select
    FiftyPts1 = FiftyPts1 + 1
End Sub

Sub Wall50Pts2_Timer
    Select Case FiftyPts2
        Case 1:Rubber8a.Visible = 0:Rubber8.Visible = 1:me.TimerEnabled = 0
    End Select
    FiftyPts2 = FiftyPts2 + 1
End Sub

Sub RubAni1_Hit
    RubberRight.Visible = 0
    RubberRighta.Visible = 1
    Ani1 = 0
    RubAni1.TimerEnabled = 1
End Sub

Sub RubAni1_Timer
    Select Case Ani1
        Case 1:RubberRighta.Visible = 0:RubberRight.Visible = 1:me.TimerEnabled = 0
    End Select
    Ani1 = Ani1 + 1
End Sub

Sub RubAni2_Hit
    RubberRight.Visible = 0
    RubberRightb.Visible = 1
    Ani2 = 0
    RubAni2.TimerEnabled = 1
End Sub

Sub RubAni2_Timer
    Select Case Ani2
        Case 1:RubberRightb.Visible = 0:RubberRight.Visible = 1:me.TimerEnabled = 0
    End Select
    Ani2 = Ani2 + 1
End Sub

Sub RubAni3_Hit
    Rubber6.Visible = 0
    Rubber6a.Visible = 1
    Ani3 = 0
    RubAni3.TimerEnabled = 1
End Sub

Sub RubAni3_Timer
    Select Case Ani3
        Case 1:Rubber6a.Visible = 0:Rubber6.Visible = 1:me.TimerEnabled = 0
    End Select
    Ani3 = Ani3 + 1
End Sub

Sub RubAni4_Hit
    RubberRight1.Visible = 0
    RubberRight1a.Visible = 1
    Ani4 = 0
    RubAni4.TimerEnabled = 1
End Sub

Sub RubAni4_Timer
    Select Case Ani4
        Case 1:RubberRight1a.Visible = 0:RubberRight1.Visible = 1:me.TimerEnabled = 0
    End Select
    Ani4 = Ani4 + 1
End Sub

Sub RubAni5_Hit
    RubberRight1.Visible = 0
    RubberRight1b.Visible = 1
    Ani5 = 0
    RubAni5.TimerEnabled = 1
End Sub

Sub RubAni5_Timer
    Select Case Ani5
        Case 1:RubberRight1b.Visible = 0:RubberRight1.Visible = 1:me.TimerEnabled = 0
    End Select
    Ani5 = Ani5 + 1
End Sub

Sub RubAni6_Hit
    RubberRight2.Visible = 0
    RubberRight2a.Visible = 1
    Ani6 = 0
    RubAni6.TimerEnabled = 1
End Sub

Sub RubAni6_Timer
    Select Case Ani6
        Case 1:RubberRight2a.Visible = 0:RubberRight2.Visible = 1:me.TimerEnabled = 0
    End Select
    Ani6 = Ani6 + 1
End Sub

Sub RubAni7_Hit
    RubberRight2.Visible = 0
    RubberRight2b.Visible = 1
    Ani7 = 0
    RubAni7.TimerEnabled = 1
End Sub

Sub RubAni7_Timer
    Select Case Ani7
        Case 1:RubberRight2b.Visible = 0:RubberRight2.Visible = 1:me.TimerEnabled = 0
    End Select
    Ani7 = Ani7 + 1
End Sub

Sub RubAni8_Hit
    Rubber4.Visible = 0
    Rubber4a.Visible = 1
    Ani8 = 0
    RubAni8.TimerEnabled = 1
End Sub

Sub RubAni8_Timer
    Select Case Ani8
        Case 1:Rubber4a.Visible = 0:Rubber4.Visible = 1:me.TimerEnabled = 0
    End Select
    Ani8 = Ani8 + 1
End Sub

'******************************************************
'				SWITCHES
'******************************************************


' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 39:PlaySound SoundFX("fx_bumper1",DOFContactors):StopSound"0FG00b":StopSound"0FG00c":PlaySound"0FG00c":End Sub

Sub Bumper1_timer
            StopFGSounds
			Dim x
			x = INT(6 * RND(1) )
			Select Case x
			Case 0:PlaySound"0FG05":Bumper2.timerinterval=23000:Bumper2.timerenabled=1
			Case 1:PlaySound"0FG07":Bumper2.timerinterval=11000:Bumper2.timerenabled=1
			Case 2:PlaySound"0FG08":Bumper2.timerinterval=14000:Bumper2.timerenabled=1
			Case 3:PlaySound"0FG10":Bumper2.timerinterval=9000:Bumper2.timerenabled=1
			Case 4:PlaySound"0FG11":Bumper2.timerinterval=9000:Bumper2.timerenabled=1
			Case 5:PlaySound"0FG30":Bumper2.timerinterval=8000:Bumper2.timerenabled=1
			End Select
Bumper1.timerenabled=0
End Sub

Sub Bumper2_Hit:vpmTimer.PulseSw 40:PlaySound SoundFX("fx_bumper2",DOFContactors):StopSound"0FG00b":StopSound"0FG00c":PlaySound"0FG00b":End Sub

Sub Bumper2_timer
            StopFGSounds
			Dim x
			x = INT(4 * RND(1) )
			Select Case x
			Case 0:PlaySound"0FG01":Bumper1.timerinterval=107000:Bumper1.timerenabled=1
			Case 1:PlaySound"0FG02":Bumper1.timerinterval=60500:Bumper1.timerenabled=1
			Case 2:PlaySound"0FG03":Bumper1.timerinterval=70500:Bumper1.timerenabled=1
			Case 3:PlaySound"0FG04":Bumper1.timerinterval=80000:Bumper1.timerenabled=1

			End Select
Bumper2.timerenabled=0
End Sub

Sub Bumper3_Hit:vpmTimer.PulseSw 37:PlaySound SoundFX("fx_bumper3",DOFContactors):StopSound"0FG00b":StopSound"0FG00c":PlaySound"0FG00b":End Sub

Sub Bumper3_timer
            StopFGSounds
			Dim x
			x = INT(4 * RND(1) )
			Select Case x
			Case 0:PlaySound"0FG19":dta.interval=10000:dta.enabled=1
			Case 1:PlaySound"0FG19":dta.interval=10000:dta.enabled=1
			Case 2:PlaySound"0FG19":dta.interval=10000:dta.enabled=1
			Case 3:PlaySound"0FG19":dta.interval=10000:dta.enabled=1

			End Select
Bumper3.timerenabled=0
End Sub
 
' 10 point rebound
Sub Wall10Pts1_hit()
	vpmTimer.PulseSwitch (29), 100, 0
	PlaySound "sensor",0,2*VolumeDial
    Rubber9.Visible = 0
    Rubber9a.Visible = 1
    Tenpts1 = 0
    Wall10Pts1.TimerEnabled = 1
End Sub



Sub Wall10Pts2_hit()
	vpmTimer.PulseSwitch (29), 100, 0
	PlaySound "sensor",0,2*VolumeDial
    Rubber2.Visible = 0
    Rubber2a.Visible = 1
    Tenpts2 = 0
    Wall10Pts2.TimerEnabled = 1
End Sub

' Drop target rebound
Sub Wall50Pts1_hit()
	vpmTimer.PulseSwitch (5), 100, 0 
	PlaySound "sensor",0,2*VolumeDial
    Rubber18.Visible = 0
    Rubber18a.Visible = 1
    Fiftypts1 = 0
    Wall50Pts1.TimerEnabled = 1
End Sub

Sub Wall50Pts2_hit()
	vpmTimer.PulseSwitch (5), 100, 0 
	PlaySound "sensor",0,2*VolumeDial
    Rubber8.Visible = 0
    Rubber8a.Visible = 1
    Fiftypts2 = 0
    Wall50Pts2.TimerEnabled = 1
End Sub

'***********    Drop Targets    *************
Sub dt1_hit():dtInLine.Hit 1:PlaySound"0FG00a":End Sub

Sub dta_timer()
            StopFGSounds
			Dim x
			x = INT(4 * RND(1) )
			Select Case x
			Case 0:PlaySound"0FG31":Bumper3.timerinterval=10000:Bumper3.timerenabled=1
			Case 1:PlaySound"0FG31":Bumper3.timerinterval=10000:Bumper3.timerenabled=1
			Case 2:PlaySound"0FG31":Bumper3.timerinterval=10000:Bumper3.timerenabled=1
			Case 3:PlaySound"0FG31":Bumper3.timerinterval=10000:Bumper3.timerenabled=1

			End Select
dta.enabled=0
End Sub

Sub dt2_hit():dtInLine.Hit 2:PlaySound"0FG00a":End Sub
Sub dt3_hit():dtInLine.Hit 3:PlaySound"0FG00a":End Sub

Sub dt4_hit():dt4Target.Hit 1:End Sub
Sub dt5_hit():dt4Target.Hit 2:End Sub
Sub dt6_hit():dt4Target.Hit 3:End Sub
Sub dt7_hit():dt4Target.Hit 4:End Sub

Sub dt8_hit():dt3Target.Hit 1:End Sub
Sub dt9_hit():dt3Target.Hit 2:End Sub
Sub dt10_hit():dt3Target.Hit 3:End Sub

Sub dt11_hit():dtSingle.Hit 1:PlaySound"0FG21":End Sub


'*******	Targets		******************
Sub UpperSpot_Hit:vpmTimer.PulseSw 24:PlaySound SoundFX("target",DOFContactors):End Sub
Sub Target1_Hit:vpmTimer.PulseSw 15:PlaySound SoundFX("target",DOFContactors):End Sub
Sub Target2_Hit:vpmTimer.PulseSw 12:PlaySound SoundFX("target",DOFContactors):End Sub
Sub Target3_Hit:vpmTimer.PulseSw 28:PlaySound SoundFX("target",DOFContactors):PlaySound"0FG21":End Sub


'*******	Rollover Switches	******************

Sub sw1a_Hit:vpmTimer.PulseSw 1:PlaySound "starsensor",0,2*VolumeDial:End Sub
'Sub sw1a_UnHit:Controller.Switch(1) = 0:End Sub
Sub sw1b_Hit:vpmTimer.PulseSw 1:PlaySound "starsensor",0,2*VolumeDial:End Sub
'Sub sw1b_UnHit:Controller.Switch(1) = 0:End Sub
Sub sw1c_Hit:vpmTimer.PulseSw 1:PlaySound "starsensor",0,2*VolumeDial:End Sub
'Sub sw1c_UnHit:Controller.Switch(1) = 0:End Sub
Sub sw1d_Hit:vpmTimer.PulseSw 1:PlaySound "starsensor",0,2*VolumeDial:End Sub
'Sub sw1d_UnHit:Controller.Switch(1) = 0:End Sub

Sub sw2a_Hit:vpmTimer.PulseSw 2:PlaySound "starsensor",0,2*VolumeDial:End Sub
'Sub sw2a_UnHit:Controller.Switch(2) = 0:End Sub
Sub sw2b_Hit:vpmTimer.PulseSw 2:PlaySound "starsensor",0,2*VolumeDial:End Sub
'Sub sw2b_UnHit:Controller.Switch(2) = 0:End Sub
Sub sw2c_Hit:vpmTimer.PulseSw 2:PlaySound "starsensor",0,2*VolumeDial:End Sub
'Sub sw2c_UnHit:Controller.Switch(2) = 0:End Sub

Sub sw4_Hit:vpmTimer.PulseSw 4:PlaySound "sensor",0,2*VolumeDial
StopFGSounds:TwoWayKicker.timerenabled=0:Drain.timerenabled=0:Bumper1.timerenabled=0:Bumper2.timerenabled=0:Bumper3.timerenabled=0:dt1.timerenabled=0:StopSound"FG MusicSnippet"
              If Tilted = True then Exit sub
              Dim w: w = INT(4 * RND(1) )
              Select Case w
			  Case 0:PlaySound("0FG01"):TwoWayKicker.timerinterval=87000:TwoWayKicker.timerenabled=1
			  Case 1:PlaySound("0FG02"):TwoWayKicker.timerinterval=40500:TwoWayKicker.timerenabled=1
			  Case 2:PlaySound("0FG03"):TwoWayKicker.timerinterval=51500:TwoWayKicker.timerenabled=1
			  Case 3:PlaySound("0FG04"):TwoWayKicker.timerinterval=60500:TwoWayKicker.timerenabled=1

              End Select
End Sub
'Sub sw4_UnHit:End Sub

Sub sw13_Hit:vpmTimer.PulseSw 13:PlaySound "sensor",0,2*VolumeDial:End Sub
'Sub sw13_UnHit:Controller.Switch(13) = 0:End Sub
Sub sw14_Hit:vpmTimer.PulseSw 14:PlaySound "sensor",0,2*VolumeDial:End Sub
'Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub

Sub sw31_Hit:vpmTimer.PulseSw 31:PlaySound "sensor",0,2*VolumeDial:StopFGSounds:PlaySound"0FG00f":End Sub
'Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub
Sub sw32_Hit:vpmTimer.PulseSw 32:PlaySound "sensor",0,2*VolumeDial:StopFGSounds:PlaySound"0FG00f":End Sub
'Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub

'***************	Spinners	******************

Sub spinner1_Spin():vpmTimer.PulseSw (34):PlaySound "fx_spinner",0,2*VolumeDial:End Sub
Sub spinner2_Spin():vpmTimer.PulseSw (33):PlaySound "fx_spinner",0,2*VolumeDial:End Sub


'******************************************************
'				Lights & Flashers
'******************************************************
Lights(1) = Array(L1,L1b)		' LMiniBonus1
Lights(2) = Array(L2,L2b)		' LMiniBonus5	
Lights(3) = Array(L3,L3b)		' LMiniBonus9
Lights(4) = Array(L4,L4b)		' LBonus1
Lights(5) = Array(L5,L5b)		' LBonus5
Lights(6) = Array(L6,L6b)		' LBonus9
Lights(7) = Array(L7,L7b)		' LLower2xBonus
Lights(8) = Array(L8,L8b)		' LUpperTripleDropTargetArrow1
Lights(9) = Array(L9,L9b)		' LLowerDropTragetsOrange
Lights(10) = Array(L10,L10b)	' LLowerRightTargetWhite
'Const LBGShootAgain					= 11
Lights(12) = Array(L12,L12b)	' LCenterBonus10K
'Const LBallInPlay					= 13
Lights(14) = Array (GI33,GI33a)	' LTopBumper
Lights(15) = Array(L15,L15b)	' LRightOutlane
Lights(17) = Array(L17,L17b)	' LMiniBonus2
Lights(18) = Array(L18,L18b)	' LMiniBonus6
Lights(19) = Array(L19,L19b)	' LMiniBonus10
Lights(20) = Array(L20,L20b)	' LBonus2
Lights(21) = Array(L21,L21b)	' LBonus6
Lights(22) = Array(L22,L22b)	' LBonus10
Lights(23) = Array(L23,L23b)	' Lower3xBonus
Lights(24) = Array(L24,L24b)	' LUpperTripleDropTargetArrow2
Lights(25) = Array(L25,L25b)	' LLowerDropTragetsYellow
Lights(26) = Array(L26,L26b)	' LRightFlipperlane
'Const LBGMatch						= 27
Lights(28) = Array(L28,L28b)	' LCenterBonus20K
'Const LBGHighScore					= 29
Lights(30) = Array(L30,L30b)	' LLowerRightExtraBall
Lights(31) = Array(L31,L31b)	' LLeftOutlane
Lights(33) = Array(L33,L33b)	' LMiniBonus3
Lights(34) = Array(L34,L34b)	' LMiniBonus7
Lights(35) = Array(L35,L35b)	' LRightRampArrow
Lights(36) = Array(L36,L36b)	' LBonus3
Lights(37) = Array(L37,L37b)	' LBonus7
Lights(38) = Array(L38,L38b)	' LMiniBonus50K
Lights(39) = Array(L39,L39b)	' LLower4xBonus
Lights(40) = Array(L40,L40b)	' LUpperTripleDropTargetArrow3
Lights(41) = Array(L57,L57b)	' LLowerDropTragetsBlue
Lights(42) = Array(L42,L42b)	' LLeftFlipperlane
Lights(43) = Array(L43,L43b)	' LLowerShootAgain
Lights(44) = Array(L44,L44b)	' LCenterExtraBall
'Lights(45) = 					' LBGGameOver
Lights(46) = Array(L46,L46b)	' LCenterBonus30K
Lights(47) = Array(L47,L47b)	' LRightRampRollover1
Lights(49) = Array(L49,L49b)	' LMiniBonus4
Lights(50) = Array(L50,L50b)	' LMiniBonus8
Lights(51) = Array(L51,L51b)	' LLeftRampArrow
Lights(52) = Array(L52,L52b)	' LBonus4
Lights(53) = Array(L53,L53b)	' LBonus8
Lights(54) = Array(L54,L54b)	' LBonus100K
Lights(55) = Array(L55,L55b)	' LLower5xBonus
Lights(56) = Array(L56,L56b)	' LUpper4xBonus
Lights(57) = Array(L41,L41b)	' LLowerDropTragetsWhite
Lights(58) = Array(L58,L58b)	' LLowerRightTargetOrange
Lights(59) = Array(L59)			' Const LCredit
Lights(60) = Array(L60,L60b)	' LLowerDropTragets5xBonus
'Const LTilt							= 61
Lights(62) = Array(L62,L62b)	' LUpperTargetCollectBonus
Lights(63) = Array(L63,L63b)	' LUpperTargetSpecial
Lights(65) = Array(L65,L65b)	' LRightRampRollover2
'Const LBGFlashLogo1					= 66
'Const LBGFlashLogo4					= 67
Lights(68) = Array(GI13,GI14,GIb14,GIb15)	' LMingFace1
Lights(69) = Array(L69,L69b)	' LRightClockSeconds
Lights(81) = Array(L81,L81b)	' LShooterlaneRollover3
'Const LBGFlashLogo2					= 82
'Const LBGFlashLogo5					= 83
Lights(84) = Array(GI15,GI16)	' LMingFace2
Lights(85) = Array(L85,L85b)	' LLeftClockSeconds	
Lights(97) = Array(L97,L97b)	' LShooterlaneRollover2
'Const LBGFlashLogo3					= 98
'Const LBGFlashLogo6					= 99
Lights(100) = Array(L100a,L100ab,L100b,L100bb)	' LLeftTopLaneRollover
Lights(101) = Array(L101,L101b)	' LSaucer3xArrowRight
Lights(113) = Array(L113,L113b)	' LShooterlaneRollover1	
'Const LBGFlasher					= 116
Lights(117) = Array(L117,L117b)	' LSaucer2xArrowLeft
'Const LLeftBeacon					= 195
'Const LRightBeacon					= 196
Lights(197) = Array(GI32)			' LLeftBumper
Lights(198) = Array(GI31) 			' LRightBumper
'Const LGI							= 199


Sub ChangeGlow(day)
	Dim Light
	If day Then
		For Each Light in GlowLights : Light.IntensityScale = GlowAmountDay: Light.FadeSpeedUp = Light.Intensity * GlowAmountDay / 2: Light.FadeSpeedDown = Light.FadeSpeedUp / 5: Next 'fadespeeddown = fadespeedup / 25
		For Each Light in InsertLights : Light.IntensityScale = InsertBrightnessDay : Light.FadeSpeedUp = Light.Intensity * InsertBrightnessDay / 2 : Light.FadeSpeedDown = Light.FadeSpeedUp / 5 : Next 'fadespeeddown = fadespeedup / 25
	Else
		For Each Light in GlowLights : Light.IntensityScale = GlowAmountNight: Light.FadeSpeedUp = Light.Intensity * GlowAmountNight / 2: Light.FadeSpeedDown = Light.FadeSpeedUp / 5: Next 'fadespeeddown = fadespeedup / 25
		For Each Light in InsertLights : Light.IntensityScale = InsertBrightnessNight : Light.FadeSpeedUp = Light.Intensity * InsertBrightnessNight / 2 : Light.FadeSpeedDown = Light.FadeSpeedUp / 5 : Next 'fadespeeddown = fadespeedup / 25
	End If
End Sub

Sub ColorGrade()
	Dim lutlevel, ContrastLut
	Lutlevel = 0
	If ContrastSetting=0 or ContrastSetting=2 or ContrastSetting=4 or ContrastSetting=6 Then
		ChangeGlow(True)
		ContrastLut = ContrastSetting / 2
	Else
		ChangeGlow(False)
		ContrastLut = ContrastSetting / 2 - 0.5
	End If
	table1.ColorGradeImage = "LUT" & ContrastLut & "_" & lutlevel
End Sub

'******************************************************
'       		RealTime Updates
'******************************************************

'Set MotorCallback = GetRef("GameTimer")

Sub GameTimer_timer()
    UpdateMechs
	RollingSoundUpdate
	BallShadowUpdate
	if enableBallControl then
		BallControl
	End If
End Sub

Dim RolloverArray, PrevGameOver, maxxvel, maxyvel

RolloverArray = Array("FG LightOrange0","FG LightOrange33","FG LightOrange66","FG LightOrange100")

Sub UpdateMechs
	TopFlipperP.RotY=RightFlipper2.currentangle+90
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperR2Sh.RotZ = RightFlipper2.currentangle

	pSpinner1.RotX = spinner1.Currentangle * -1
	pSpinnerRod1.TransX = sin( (spinner1.CurrentAngle+180) * (2*PI/360)) * 5
	pSpinnerRod1.TransY = sin( (spinner1.CurrentAngle- 90) * (2*PI/360)) * 5

	pSpinner2.RotX = spinner2.Currentangle * -1
	pSpinnerRod2.TransX = sin( (spinner2.CurrentAngle+180) * (2*PI/360)) * 5
	pSpinnerRod2.TransY = sin( (spinner2.CurrentAngle- 90) * (2*PI/360)) * 5

	If L81.state = 1 Then
		Chrome1.image = RolloverArray(3)
		Chrome1.DisableLighting = 1
	Else
		Chrome1.image = RolloverArray(0)
		Chrome1.DisableLighting = 0
	End If

	If L97.state = 1 Then
		Chrome2.image = RolloverArray(3)
		Chrome2.DisableLighting = 1
	Else
		Chrome2.image = RolloverArray(0)
		Chrome2.DisableLighting = 0
	End If

	If L113.state = 1 Then
		Chrome3.image = RolloverArray(3)
		Chrome3.DisableLighting = 1
	Else
		Chrome3.image = RolloverArray(0)
		Chrome3.DisableLighting = 0
	End If

	If L100a.state = 1 Then
		Chrome4.image = RolloverArray(3)
		Chrome4.DisableLighting = 1
		Chrome5.image = RolloverArray(3)
		Chrome5.DisableLighting = 1
	Else
		Chrome4.image = RolloverArray(0)
		Chrome4.DisableLighting = 0
		Chrome5.image = RolloverArray(0)
		Chrome5.DisableLighting = 0
	End If

	If GI13.state = 1 Then
		pPlasticsAming.disablelighting = 1
	Else
		pPlasticsAming.disablelighting = 0	
	End If

' *********	Kicker Code
	If InRect(ReflBall.x,ReflBall.y,418,603,450,603,450,635,418,635) Then
		If ABS(ReflBall.velx) < 0.05 and ABS(ReflBall.vely) < 0.05 Then
			TwoWayKicker.enabled = True
		End If
	End If
' *********	End Kicker Code



	If Controller.lamp(45) = true then
		If MusicSnippet = 1 And PrevGameOver = 0 Then	
			StopSound "FG MusicSnippet": PlaySound "FG MusicSnippet":Bumper1.timerinterval=27000:Bumper1.timerenabled=1
			PrevGameOver = 1
		End If
	else 
		PrevGameOver = 0
	End If

'	if InRect(ReflBall.x,ReflBall.y,0,950,850,950,850,1974,0,1974) Then
'		if abs(reflball.velx) > maxxvel Then maxxvel = abs(reflball.velx):debug.print maxxvel & ", " & maxyvel
'		if abs(reflball.vely) > maxyvel then maxyvel = abs(reflball.vely):debug.print maxxvel & ", " & maxyvel
'	end if

End Sub


'*****************************************
'	Ball Shadow
'*****************************************

Dim BallShadow,BR(20)
BallShadow = Array (BallShadow1)

' Format = (x0,y0,x1,y1,x2,y2,x3,y3)
BR(0) = Array(440,33,932,33,942,466,843,466) 'Right Ball Reflection Area
' Format = (x0,y0,x1,y1,x2,y2,x3,y3,ax,ay,bx,by,aangle,bangle,maxx,maxy) where maxx and maxy are max x and y deflection
BR(1) = Array(936,423,861,423,830,305,906,305,925,423,903,307.6,-85,-71.5,5.5,0) 
BR(2) = Array(BR(1)(6),BR(1)(7),BR(1)(4),BR(1)(5),755.7,192.3,840.3,163.2,BR(1)(10),BR(1)(11),824,169.5,BR(1)(13),-50,8,-2) 
BR(3) = Array(BR(2)(6),BR(2)(7),BR(2)(4),BR(2)(5),662.1,128.9,686.8,47,BR(2)(10),BR(2)(11),681,63.75,BR(2)(13),-20,8,-8)
BR(4) = Array(BR(3)(6),BR(3)(7),BR(3)(4),BR(3)(5),644.8,138.7,554.9,60.7,BR(3)(10),BR(3)(11),583.6,86.2,BR(3)(13),50,-10,-10)
BR(5) = Array(BR(4)(6),BR(4)(7),BR(4)(4),BR(4)(5),650.3,149.6,548.1,157.9,BR(4)(10),BR(4)(11),563.7,155.2,BR(4)(13),100,-8,-2)

BR(7) = Array(0,0,406,0,146.6,379.9,0,335) 'Left Ball Reflection Area
BR(8) = Array(141,371.2,11.6,328.7,12.2,200.6,140.8,226.7,27.6,332.3,27.6,226.7,90,90,0,0) 
BR(9) = Array(BR(8)(6),BR(8)(7),BR(8)(4),BR(8)(5),11.4,87.7,170.2,183.5,BR(8)(10),BR(8)(11),58.5,115.2,BR(8)(13),56,-4,-1)
BR(10) = Array(BR(9)(6),BR(9)(7),BR(9)(4),BR(9)(5),254.1,5.1,228.8,150.4,BR(9)(10),BR(9)(11),248.8,34,BR(9)(13),-9,-10,-28)
BR(11) = Array(BR(10)(6),BR(10)(7),BR(10)(4),BR(10)(5),343.7,11.9,276.8,166.5,BR(10)(10),BR(10)(11),322.4,59.4,BR(10)(13),-30,0,0)

Sub BallShadowUpdate()
    Dim BOT, b, shadowZ
    BOT = GetBalls

	' render the shadow for each ball	
	BallShadow1.X = ReflBall.X
	BallShadow1.Y = ReflBall.Y + 10
	BallShadow1.Z = ReflBall.Z - 24.9
	BallShadow2.X = ReflBall.X
	BallShadow2.Y = ReflBall.Y + 10
	BallShadow2.Z = ReflBall.Z - 24.9

	If ReflBall.z < 80 or ReflBall.z > 125 then
		'BallShadow1.visible = True
		'BallShadow2.visible = True
	Else 
		BallShadow1.Y = ReflBall.Y + 15	
		'BallShadow1.visible = False
		'BallShadow2.visible = False
	End If


	If InRect(ReflBall.x,ReflBall.y,BR(0)(0),BR(0)(1),BR(0)(2),BR(0)(3),BR(0)(4),BR(0)(5),BR(0)(6),BR(0)(7)) Then
		If InRect(ReflBall.x,ReflBall.y,BR(1)(0),BR(1)(1),BR(1)(2),BR(1)(3),BR(1)(4),BR(1)(5),BR(1)(6),BR(1)(7)) Then
			UpdateRefl PercentOfPathL(ReflBall.x,ReflBall.y,BR(1)(0),BR(1)(1),BR(1)(2),BR(1)(3),BR(1)(4),BR(1)(5),BR(1)(6),BR(1)(7)), BR(1)(14),BR(1)(15),BR(1)(8),BR(1)(9),BR(1)(12),BR(1)(10),BR(1)(11),BR(1)(13)
		ElseIf InRect(ReflBall.x,ReflBall.y,BR(2)(0),BR(2)(1),BR(2)(2),BR(2)(3),BR(2)(4),BR(2)(5),BR(2)(6),BR(2)(7)) Then
			UpdateRefl PercentOfPathL(ReflBall.x,ReflBall.y,BR(2)(0),BR(2)(1),BR(2)(2),BR(2)(3),BR(2)(4),BR(2)(5),BR(2)(6),BR(2)(7)), BR(2)(14),BR(2)(15),BR(2)(8),BR(2)(9),BR(2)(12),BR(2)(10),BR(2)(11),BR(2)(13)
		ElseIf InRect(ReflBall.x,ReflBall.y,BR(3)(0),BR(3)(1),BR(3)(2),BR(3)(3),BR(3)(4),BR(3)(5),BR(3)(6),BR(3)(7)) Then
			UpdateRefl PercentOfPathL(ReflBall.x,ReflBall.y,BR(3)(0),BR(3)(1),BR(3)(2),BR(3)(3),BR(3)(4),BR(3)(5),BR(3)(6),BR(3)(7)), BR(3)(14),BR(3)(15),BR(3)(8),BR(3)(9),BR(3)(12),BR(3)(10),BR(3)(11),BR(3)(13)
		ElseIf InRect(ReflBall.x,ReflBall.y,BR(4)(0),BR(4)(1),BR(4)(2),BR(4)(3),BR(4)(4),BR(4)(5),BR(4)(6),BR(4)(7)) Then
			UpdateRefl PercentOfPathL(ReflBall.x,ReflBall.y,BR(4)(0),BR(4)(1),BR(4)(2),BR(4)(3),BR(4)(4),BR(4)(5),BR(4)(6),BR(4)(7)), BR(4)(14),BR(4)(15),BR(4)(8),BR(4)(9),BR(4)(12),BR(4)(10),BR(4)(11),BR(4)(13)
		ElseIf InRect(ReflBall.x,ReflBall.y,BR(5)(0),BR(5)(1),BR(5)(2),BR(5)(3),BR(5)(4),BR(5)(5),BR(5)(6),BR(5)(7)) Then
			UpdateRefl PercentOfPathL(ReflBall.x,ReflBall.y,BR(5)(0),BR(5)(1),BR(5)(2),BR(5)(3),BR(5)(4),BR(5)(5),BR(5)(6),BR(5)(7)), BR(5)(14),BR(5)(15),BR(5)(8),BR(5)(9),BR(5)(12),BR(5)(10),BR(5)(11),BR(5)(13)
		Else
			BallRefl.visible = false
		End If
	ElseIf InRect(ReflBall.x,ReflBall.y,BR(7)(0),BR(7)(1),BR(7)(2),BR(7)(3),BR(7)(4),BR(7)(5),BR(7)(6),BR(7)(7)) Then
		If InRect(ReflBall.x,ReflBall.y,BR(8)(0),BR(8)(1),BR(8)(2),BR(8)(3),BR(8)(4),BR(8)(5),BR(8)(6),BR(8)(7)) Then
			UpdateRefl PercentOfPathL(ReflBall.x,ReflBall.y,BR(8)(0),BR(8)(1),BR(8)(2),BR(8)(3),BR(8)(4),BR(8)(5),BR(8)(6),BR(8)(7)), BR(8)(14),BR(8)(15),BR(8)(8),BR(8)(9),BR(8)(12),BR(8)(10),BR(8)(11),BR(8)(13)
		ElseIf InRect(ReflBall.x,ReflBall.y,BR(9)(0),BR(9)(1),BR(9)(2),BR(9)(3),BR(9)(4),BR(9)(5),BR(9)(6),BR(9)(7)) Then
			UpdateRefl PercentOfPathL(ReflBall.x,ReflBall.y,BR(9)(0),BR(9)(1),BR(9)(2),BR(9)(3),BR(9)(4),BR(9)(5),BR(9)(6),BR(9)(7)), BR(9)(14),BR(9)(15),BR(9)(8),BR(9)(9),BR(9)(12),BR(9)(10),BR(9)(11),BR(9)(13)
		ElseIf InRect(ReflBall.x,ReflBall.y,BR(10)(0),BR(10)(1),BR(10)(2),BR(10)(3),BR(10)(4),BR(10)(5),BR(10)(6),BR(10)(7)) Then
			UpdateRefl PercentOfPathL(ReflBall.x,ReflBall.y,BR(10)(0),BR(10)(1),BR(10)(2),BR(10)(3),BR(10)(4),BR(10)(5),BR(10)(6),BR(10)(7)), BR(10)(14),BR(10)(15),BR(10)(8),BR(10)(9),BR(10)(12),BR(10)(10),BR(10)(11),BR(10)(13)
		ElseIf InRect(ReflBall.x,ReflBall.y,BR(11)(0),BR(11)(1),BR(11)(2),BR(11)(3),BR(11)(4),BR(11)(5),BR(11)(6),BR(11)(7)) Then
			UpdateRefl PercentOfPathL(ReflBall.x,ReflBall.y,BR(11)(0),BR(11)(1),BR(11)(2),BR(11)(3),BR(11)(4),BR(11)(5),BR(11)(6),BR(11)(7)), BR(11)(14),BR(11)(15),BR(11)(8),BR(11)(9),BR(11)(12),BR(11)(10),BR(11)(11),BR(11)(13)
		Else
			BallRefl.visible = false
		End If
	Else
		BallRefl.visible = false
	End If

End Sub

Sub UpdateRefl(percent,maxx,maxy,ax,ay,aangle,bx,by,bangle)
	BallRefl.visible = true

	Dim defl, Dist
	If percent < 0.5 Then
		defl = percent/0.5
	elseif percent > 0.49 Then
		defl = ABS(1-percent)/0.5
	End If

	defl = defl ^ (1/1.5)

	BallRefl.Roty = -(aangle - bangle) * percent + aangle
	BallRefl.x = -(ax - bx) * percent + ax + defl * maxx
	BallRefl.y = -(ay - by) * percent + ay + defl * maxy
End Sub

dim prevx, prevy

Sub BallControl()
	
	If Contball then

		If right = 1 Then
			ReflBall.velx = 4
		ElseIf left = 1 Then
			ReflBall.velx = - 4
		Else
			ReflBall.velx=0
		End If

		If up = 1 Then
			ReflBall.vely = -4
		ElseIf down = 1 Then
			ReflBall.vely = 4
		Else
			ReflBall.vely= -0.01
		End If

	End If

End Sub


 '*****************************************************************
 'Functions
 '*****************************************************************

'*** PI returns the value for PI

Function PI()

	PI = 4*Atn(1)

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

Function PercentOfPath(px,py,ax,ay,bx,by)
	PercentOfPath = Distance(px,py,ax,ay)/Distance(ax,ay,bx,by)
End Function

Function PercentOfPathL(px,py,ax1,ay1,bx1,by1,ax2,ay2,bx2,by2) 'Percent distance between two skew lines
	Dim DistanceTo1, DistanceTo2
	DistanceTo1 = DistancePL(px,py,ax1,ay1,bx1,by1)
	DistanceTo2 = DistancePL(px,py,ax2,ay2,bx2,by2)
	PercentOfPathL = DistanceTo1/(DistanceTo1 + DistanceTo2)
End Function

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

'******************************************************
'			Supporting Ball & Sound Functions
'******************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
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

'******************************************************
'      		JP's VP10 Rolling Sounds
'******************************************************

Const tnob = 2 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingSoundUpdate()
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 170 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*2*VolumeDial, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next

End Sub

'******************************************************
' 			JP's VP10 Ball Collision Sound
'******************************************************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, (Csng(velocity) ^2 / 2000)*2*VolumeDial, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'******************************************************
' 				JP's Sound Routines
'******************************************************

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound SoundFX("target",DOFContactors),0,CollectionVolume/10
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*CollectionVolume/5, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate", 0, Vol(ActiveBall)*CollectionVolume/10, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall)*CollectionVolume/10, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*CollectionVolume/5, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*CollectionVolume/5, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*CollectionVolume/5, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub Woods_Hit (idx)
	PlaySound "woodhit", 0, Vol(ActiveBall)*CollectionVolume, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper2_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*CollectionVolume*2, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*CollectionVolume*2, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*CollectionVolume*2, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub


'***********************************************************************************
'****					DIP switch routines (parts by scapino) 					****
'***********************************************************************************

'**************
' Edit Dips
'**************

'         .AddForm 320, 258, "Star Trek - DIP switch settings"
         

Sub EditDips
 	Dim vpmDips: Set vpmDips = New cvpmDips
 	With vpmDips
 		.AddForm 700,400,"Flash Gordon - DIP switches"
 		.AddFrame 0,0,190,"Maximum credits",&H03000000,Array("10 credits",0,"15 credits",&H01000000,"25 credits",&H02000000,"40 credits",&H03000000)'dip 25&26
 		.AddFrame 0,76,190,"Balls per game",&HC0000000,Array ("2 balls",&HC0000000,"3 balls",0,"4 balls",&H80000000,"5 balls",&H40000000)'dip 31&32
 		.AddFrame 0,152,190,"Saucer 10K adjust",&H00000020,Array("10K is off at start of game",0,"10K is on at start of game",&H00000020)'dip 6
 		.AddFrame 0,198,190,"Special limit",&H10000000,Array("1 replay per game",0,"unlimited replays",&H10000000)'dip 29
 		.AddFrame 0,244,190,"Extra ball limit",&H20000000,Array("1 extra ball per game",0,"1 extra ball per ball",&H20000000)'dip 30
 		.AddChk 205,0,180,Array("Match feature",&H08000000)'dip 28
 		.AddChk 205,20,115,Array("Credits displayed",&H04000000)'dip 27
 		.AddChk 205,40,190,Array("Saucer value in memory",&H00000040)'dip 7
 		.AddChk 205,60,190,Array("Saucer 2X, 3X arrow in memory",&H00000080)'dip 8
 		.AddChk 205,80,190,Array("Outlane special in memory",&H00002000)'dip 14
 		.AddChk 205,100,190,Array("Top target special in memory",&H00004000)'dip 15
 		.AddChk 205,120,190,Array("Bonus multiplier in memory",32768)'dip 16
 		.AddChk 205,140,250,Array("Game over attract says 'Emperor Ming awaits'",&H00100000)'dip 21
 		.AddChk 205,160,250,Array("2 Side targets && flipper feed lane memory",&H00200000)'dip 22
 		.AddChk 205,180,190,Array("4 Drop target bank in memory",&H00400000)'dip 23
 		.AddChk 205,200,190,Array("Top 3 target arrows in memory",&H00800000)'dip 24
 		.AddLabel 40,300,350,20,"Set selftest position 16,17,18 and 19 to 03 for the best gameplay."
 		.AddLabel 50,320,300,20,"After hitting OK, press F3 to reset game with new settings."
 		.ViewDips
 	End With
End Sub

 Set vpmShowDips = GetRef("editDips")

 '************************************
 '          LEDs Display
 'Based on Scapino's 7 digit Reel LEDs
 '************************************


 LampTimer.Interval = 35
 LampTimer.Enabled = 1

 Sub LampTimer_Timer() 
     UpdateLeds
     UpdateTextBoxes
 End Sub
 

 Dim Digits(32)
 Dim Patterns(11)
 Dim Patterns2(11)
 
 Patterns(0) = 0     'empty
 Patterns(1) = 63    '0
 Patterns(2) = 6     '1
 Patterns(3) = 91    '2
 Patterns(4) = 79    '3
 Patterns(5) = 102   '4
 Patterns(6) = 109   '5
 Patterns(7) = 125   '6
 Patterns(8) = 7     '7
 Patterns(9) = 127   '8
 Patterns(10) = 111  '9
 
 Patterns2(0) = 128  'empty
 Patterns2(1) = 191  '0
 Patterns2(2) = 134  '1
 Patterns2(3) = 219  '2
 Patterns2(4) = 207  '3
 Patterns2(5) = 230  '4
 Patterns2(6) = 237  '5
 Patterns2(7) = 253  '6
 Patterns2(8) = 135  '7
 Patterns2(9) = 255  '8
 Patterns2(10) = 239 '9
 
 Set Digits(0) = a0
 Set Digits(1) = a1
 Set Digits(2) = a2
 Set Digits(3) = a3
 Set Digits(4) = a4
 Set Digits(5) = a5
 Set Digits(6) = a6
 
 Set Digits(7) = b0
 Set Digits(8) = b1
 Set Digits(9) = b2
 Set Digits(10) = b3
 Set Digits(11) = b4
 Set Digits(12) = b5
 Set Digits(13) = b6
 
 Set Digits(14) = c0
 Set Digits(15) = c1
 Set Digits(16) = c2
 Set Digits(17) = c3
 Set Digits(18) = c4
 Set Digits(19) = c5
 Set Digits(20) = c6
 
 Set Digits(21) = d0
 Set Digits(22) = d1
 Set Digits(23) = d2
 Set Digits(24) = d3
 Set Digits(25) = d4
 Set Digits(26) = d5
 Set Digits(27) = d6
 
 Set Digits(28) = e0
 Set Digits(29) = e1
 Set Digits(30) = e2
 Set Digits(31) = e3
 
 Sub UpdateLeds
     On Error Resume Next
     Dim ChgLED, ii, jj, chg, stat
     ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
     If Not IsEmpty(ChgLED)Then
         For ii = 0 To UBound(ChgLED)
             chg = chgLED(ii, 1):stat = chgLED(ii, 2)
 
             For jj = 0 to 10
                 If stat = Patterns(jj)OR stat = Patterns2(jj)then Digits(chgLED(ii, 0)).SetValue jj
             Next
         Next
     End IF
 End Sub


 Sub UpdateTextBoxes()
	NFadeT 13, l13, "BALL IN PLAY"
	NFadeT 27, l27, "MATCH"
	NFadeT 29, l29, "HIGH SCORE TO DATE"
	NFadeT 11, l11, "SAME PLAYER SHOOTS AGAIN"
	NFadeT 45, l45, "GAME OVER"
	If Tilted = False then NFadeT 61, l61, "TILT" 
 End Sub

 Sub NFadeT(nr, a, b)
     Select Case controller.lamp(nr)
         Case False:a.Text = "" 
         Case True:a.Text = b : If nr = 61 then StopFGSounds: PlaySound "0FG13": Tilted = True: tt = 0 : TiltedTimer.enabled =1:TwoWayKicker.timerenabled=0:Bumper1.timerenabled=0:Bumper2.timerenabled=0:Bumper3.timerenabled=0:dt1.timerenabled=0 
     End Select
End Sub

Sub TiltedTimer_Timer
	tt = tt + 1
	if tt > 2 Then 
		Tilted = False
		TiltedTimer.enabled = 0
		tt = 0
	End If
End Sub


CREDIT.Text = "CREDIT"

dim zz
If Table1.ShowDT = false then
    For each zz in DT:zz.Visible = false: Next    
else
    For each zz in DT:zz.Visible = true: Next 
End If

Sub SetOptions()

	If RampImage = 0 Then
		Ramp1.image = "FG RampB"		
		Ramp7.image = "FG RampB"
		Ramp9.image = "FG RampB"
		Ramp10.image = "FG RampB"
		Ramp1.ImageAlignment = 1		
		Ramp7.ImageAlignment = 1
		Ramp9.ImageAlignment = 1
		Ramp10.ImageAlignment = 1
		Ramp1.material = "Metal Ramp 2"
		Ramp7.material = "Metal Ramp 2"
		Ramp9.material = "Metal Ramp 2"
		Ramp10.material = "Metal Ramp 2"
	Elseif RampImage = 1 Then
		Ramp1.image = "Ramp1"
		Ramp7.image = "Ramp1"
		Ramp9.image = "Ramp1"
		Ramp10.image = "Ramp1"
		Ramp1.ImageAlignment = 1		
		Ramp7.ImageAlignment = 1
		Ramp9.ImageAlignment = 1
		Ramp10.ImageAlignment = 1
		Ramp1.material = "Metal Ramp 2"
		Ramp7.material = "Metal Ramp 2"
		Ramp9.material = "Metal Ramp 2"
		Ramp10.material = "Metal Ramp 2"
	Elseif RampImage = 2 Then
		Ramp1.image = "FG RampB"
		Ramp7.image = "brushed-steel vertical_1"
		Ramp9.image = "brushed-steel vertical_2"
		Ramp10.image = "brushed-steel vertical_1"
		Ramp1.ImageAlignment = 1		
		Ramp7.ImageAlignment = 1
		Ramp9.ImageAlignment = 1
		Ramp10.ImageAlignment = 1
		Ramp1.material = "Metal Ramp 2"
		Ramp7.material = "Metal Ramp"
		Ramp9.material = "Metal Ramp"
		Ramp10.material = "Metal Ramp"
	Else
		Ramp1.image = "Ramp1"
		Ramp7.image = "FG Ramps"
		Ramp9.image = "FG Ramps"
		Ramp10.image = "FG Ramps"
		Ramp1.ImageAlignment = 1		
		Ramp7.ImageAlignment = 0
		Ramp9.ImageAlignment = 0
		Ramp10.ImageAlignment = 0
		Ramp1.material = "Metal Ramp 2"
		Ramp7.material = "Metal Ramp 2"
		Ramp9.material = "Metal Ramp 2"
		Ramp10.material = "Metal Ramp 2"
	End If

	PegPlasticLE.visible = False
	PegPlasticLM.visible = False
	PegPlasticLH.visible = False
	PegPlasticRE.visible = False
	PegPlasticRM.visible = False
	PegPlasticRH.visible = False

	RubberLE.visible = False
	RubberLE.Collidable = False
	RubberLM.visible = False
	RubberLM.Collidable = False
	RubberLH.visible = False
	RubberLH.Collidable = False

	RubberRE.visible = False
	RubberRE.Collidable = False
	RubberRM.visible = False
	RubberRM.Collidable = False
	RubberRH.visible = False
	RubberRH.Collidable = False

	If OutLanePosts = 0 Then
		PegPlasticLE.visible = True
		PegPlasticRE.visible = True
		RubberLE.visible = True
		RubberLE.Collidable = True
		RubberRE.visible = True
		RubberRE.Collidable = True
	ElseIf OutLanePosts = 1 Then
		PegPlasticLM.visible = True
		PegPlasticRM.visible = True
		RubberLM.visible = True
		RubberLM.Collidable = True
		RubberRM.visible = True
		RubberRM.Collidable = True
	Else
		PegPlasticLH.visible = True
		PegPlasticRH.visible = True
		RubberLH.visible = True
		RubberLH.Collidable = True
		RubberRH.visible = True
		RubberRH.Collidable = True
	End If

	If BallReflection = 1 Then
		BallShadow2.visible = True
	Else
		BallShadow2.visible = False
	End If

	Dim xx

	If PFSpeed = 1 Then
		For each xx in PF:xx.PhysicsMaterial = "aPhysics Playfield Fast":Next
	Else
		For each xx in PF:xx.PhysicsMaterial = "aPhysics Playfield":Next
	End If

	If KickerAccuracy = 0 Then
		KickerHole.Size_y = 13.6
		KickerHole.Size_x = 13.6
		KickerHole.Size_z = 13.6
	ElseIf KickerAccuracy = 1 Then
		KickerHole.Size_y = 8
		KickerHole.Size_x = 13
		KickerHole.Size_z = 13
	Else
		KickerHole.Size_y = 5
		KickerHole.Size_x = 12.75
		KickerHole.Size_z = 12.75
	End If

End Sub

'*************
'   JP'S LUT
'*************

Dim LUTImage
Dim xa
Sub LoadLUT
    xa = LoadValue(cGameName, "LUTImage")
    If(xa <> "") Then LUTImage = xa Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 13: UpdateLUT: SaveLUT: End Sub

Sub UpdateLUT
Select Case LutImage
Case 0: Table1.ColorGradeImage = "LUT0"
Case 1: Table1.ColorGradeImage = "LUT1"
Case 2: Table1.ColorGradeImage = "LUT2"
Case 3: Table1.ColorGradeImage = "LUT3"
Case 4: Table1.ColorGradeImage = "LUT4"
Case 5: Table1.ColorGradeImage = "LUT5"
Case 6: Table1.ColorGradeImage = "LUT6"
Case 7: Table1.ColorGradeImage = "LUT7"
Case 8: Table1.ColorGradeImage = "LUT8"
Case 9: Table1.ColorGradeImage = "LUT9"
Case 10: Table1.ColorGradeImage = "LUT10"
Case 11: Table1.ColorGradeImage = "LUT11"
Case 12: Table1.ColorGradeImage = "LUT12"
End Select
End Sub