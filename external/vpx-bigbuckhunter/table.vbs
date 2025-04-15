Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="bbh_170",UseSolenoids=1,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

LoadVPM "01560000", "sam.VBS", 3.10

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
SolCallback(1) = "solTrough"
SolCallback(2) = "solAutofire"
SolCallback(3) = "solBuckRight" 'right or backwards
SolCallback(4) = "solBuckLeft" 'left or forwards
'SolCallback(5) = "solBuckMotor"
SolCallback(7) = "birdorbitpost"
SolCallback(12) = "SolKickBack"
SolCallback(14) = "SolLElk"
SolCallback(23) = "orbitpost"

'Flashers
SolCallback(19) = "Setlamp 119,"
SolCallback(20) = "Setlamp 120,"
SolCallback(21) = "Setlamp 121,"
SolCallback(22) = "Setlamp 122,"
SolCallback(25) = "Setlamp 125,"
SolCallback(26) = "Setlamp 126,"
SolCallback(27) = "Setlamp 127,"
SolCallback(29) = "Setlamp 129,"
SolCallback(31) = "Setlamp 131,"
SolCallback(32) = "Setlamp 132,"

SolCallback(16) = "SolRFlipper"
SolCallback(15) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):RightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart
     End If
End Sub
'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

Sub solTrough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		vpmTimer.PulseSw 22
	End If
 End Sub

Sub solAutofire(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
 End Sub

'**********************************************************************************************************
 'Elk Diverter
'**********************************************************************************************************

Sub SolLElk(Enabled)
	If Enabled Then
	'	 PlaySound SoundFX("fx_Flipperup",DOFContactors):
		 Elkdiverter.RotateToEnd
		 sw85.enabled = 1			
	Else
		 Elkdiverter.RotateToStart
		 sw85.enabled = 0			 
	End If
End Sub

'**********************************************************************************************************
'**********************************************************************************************************

'Stern-Sega GI 
set GICallback = GetRef("UpdateGI")

Sub UpdateGI(no, Enabled)
	If Enabled Then
		dim xx, xxx
		For each xx in GI:xx.State = 1: Next
		For each xxx in GI2:xxx.BlendDisableLighting = 3: Next
        PlaySound "fx_relay"
Table1.colorgradeimage = "ColorGradeLUT256x16_ConSat"
	Else
		For each xx in GI:xx.State = 0: Next
		For each xxx in GI2:xxx.BlendDisableLighting = 0: Next
        PlaySound "fx_relay"
Table1.colorgradeimage = "ColorGrade_4"
	End If
End Sub

'**********************************************************************************************************
 'post
'**********************************************************************************************************

Sub orbitpost(Enabled)
	If Enabled Then
		UpPost.Isdropped=false
		playsound SoundFX("fx_Flipperup",DOFContactors)
	Else
		UpPost.Isdropped=true
	End If
 End Sub

Sub birdorbitpost(Enabled)
	If Enabled Then
		BUpPost.Isdropped=false
		playsound SoundFX("fx_Flipperup",DOFContactors)
	Else
		BUpPost.Isdropped=true
	End If
 End Sub

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************
Dim bsTrough, xx

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Big Buck Hunter (Stern 2009)"
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

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

    vpmNudge.TiltSwitch=-7
    vpmNudge.Sensitivity=2
    vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)

    Set bsTrough = New cvpmBallStack
		bsTrough.InitSw 0, 21, 20, 19, 18, 0, 0, 0
		bsTrough.InitKick BallRelease, 90, 8
		bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsTrough.Balls = 4

	BuckWalls_Init
    BUpPost.IsDropped = 1

 end sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)

	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
	If keycode = RightMagnaSave then vpmTimer.PulseSw 37
	If keycode = LeftMagnaSave then vpmTimer.PulseSw 6 
	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)

	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger":Controller.Switch(11) = 0
	If KeyUpHandler(keycode) Then Exit Sub
End Sub

     ' Impulse Plunger
	Dim PlungerIM
    Const IMPowerSetting = 60
    Const IMTime = 0.6
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .CreateEvents "plungerIM"
    End With

'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : playsound"drain" : End Sub

 'Gate Triggers
Sub sw5_hit:vpmTimer.pulseSw 5 : End Sub 
Sub sw14_hit:vpmTimer.pulseSw 14 : End Sub 

'Wire Triggers
Sub SW6_Hit:Controller.Switch(6)=1 : playsound"rollover" : End Sub 
Sub SW6_unHit:Controller.Switch(6)=0:End Sub
Sub SW7_Hit:Controller.Switch(7)=1 : playsound"rollover" : End Sub 
Sub SW7_unHit:Controller.Switch(7)=0:End Sub
Sub SW8_Hit:Controller.Switch(8)=1 : playsound"rollover" : End Sub 
Sub SW8_unHit:Controller.Switch(8)=0:End Sub
Sub SW9_Hit:Controller.Switch(9)=1 : playsound"rollover" : End Sub 
Sub SW9_unHit:Controller.Switch(9)=0:End Sub
Sub SW13_Hit:Controller.Switch(13)=1 : playsound"rollover" : End Sub 
Sub SW13_unHit:Controller.Switch(13)=0:End Sub
Sub SW23_Hit:Controller.Switch(23)=1 : playsound"rollover" : End Sub 
Sub SW23_unHit:Controller.Switch(23)=0:End Sub
Sub SW24_Hit:Controller.Switch(24)=1 : playsound"rollover" : End Sub 
Sub SW24_unHit:Controller.Switch(24)=0:End Sub
Sub SW25_Hit:Controller.Switch(25)=1 : playsound"rollover" : End Sub 
Sub SW25_unHit:Controller.Switch(25)=0:End Sub
Sub SW28_Hit:Controller.Switch(28)=1 : playsound"rollover" : End Sub 
Sub SW28_unHit:Controller.Switch(28)=0:End Sub
Sub SW29_Hit:Controller.Switch(29)=1 : playsound"rollover" : End Sub 
Sub SW29_unHit:Controller.Switch(29)=0:End Sub
Sub SW33_Hit:Controller.Switch(33)=1 : playsound"rollover" : End Sub 
Sub SW33_unHit:Controller.Switch(33)=0:End Sub

'Hidden trigger
Sub sw43_Hit:Controller.Switch(43) = 1:End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub

 'Stand Up Targets
Sub sw10_hit:vpmTimer.pulseSw 10 : End Sub 
Sub sw35_hit:vpmTimer.pulseSw 35 : End Sub
Sub sw36_hit:vpmTimer.pulseSw 36 : End Sub 
Sub sw38_hit:vpmTimer.pulseSw 38 : End Sub 
Sub sw39_hit:vpmTimer.pulseSw 39 : End Sub
Sub sw40_hit:vpmTimer.pulseSw 40 : End Sub 
Sub sw41_hit:vpmTimer.pulseSw 41 : End Sub 
Sub sw42_hit:vpmTimer.pulseSw 42 : End Sub 

'Spinners
Sub sw44_Spin:vpmTimer.PulseSw 44 : playsound"fx_spinner" : End Sub

'Elk Diverter
Sub sw85_Hit:vpmTimer.PulseSw 85:End Sub

'Bumpers
Sub Bumper1_Hit : vpmTimer.PulseSw(30) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(31) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper3_Hit : vpmTimer.PulseSw(32) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub

'Generic Sounds
Sub Trigger1_Hit: playsound"fx_ballrampdrop" : End Sub
Sub Trigger2_Hit: playsound"fx_ballrampdrop" : End Sub



'***************************************************
'***************************************************
' Ram Kickback
'***************************************************
'***************************************************

Sub SolKickBack(enabled)
	if enabled then
'		Kick is handled by ramb kicker, so ball does not get stuck in kicker
'		ramb.kick 170, 35
'		RamFire = 1
'		rambDisableTimer.Interval = 10
'		rambDisableTimer.Enabled = 1
	End if
End Sub

Const RamXStart = 303
Const RamYStart = 380
Const RamXEnd = 324
Const RamYEnd = 476
Const RamFwdSpeed = .2 '1 is max
Const RamRtnSpeed = .1 '1 is max
Dim   RamFire
Sub rambDisableTimer_Timer
	If RamFire = 2 Then 'Kick the ball back
			sw34.kick 170, 15
			rambDisableTimer.Interval = 10
			RamFire = 1
	End If

	If RamFire = 1 Then 'Move Ram forward
		If Ramlogo.x < RamXEnd Then
			Ramlogo.x = Ramlogo.x + RamFwdSpeed*(RamXEnd-RamXStart)
			Ramlogo.y = Ramlogo.y + RamFwdSpeed*(RamYEnd-RamYStart)
		Else
			RamFire = 0
		End If
	Else	'Move Ram back
		If Ramlogo.x > RamXStart Then
			Ramlogo.x = Ramlogo.x - RamRtnSpeed*(RamXEnd-RamXStart)
			Ramlogo.y = Ramlogo.y - RamRtnSpeed*(RamYEnd-RamYStart)
		Else
			rambDisableTimer.Enabled = 0
			Ramlogo.x = RamXStart
			Ramlogo.y = RamYStart
		End If
	End If
End Sub

Sub sw34_Hit()
	Controller.Switch(34) = 1
	'PlaySound "plunger"
	RamFire = 2
	rambDisableTimer.Interval = 150
	rambDisableTimer.Enabled = 1
End Sub

Sub sw34_Unhit()
	Controller.Switch(34) = 0
End Sub


'***************************************************
'***************************************************
'Buck Movement
'***************************************************
'***************************************************

Dim BuckMotor, BuckDir, BuckPos, sw37State, BuckX(400), BuckY(400)
Dim BuckWallActive, BuckLampCheck, BuckLampCurrent, BuckChanging, BuckLampPossible
Dim BuckLamp16Pos, BuckLamp17Pos, BuckLamp18Pos, BuckLamp19Pos, BuckLamp20Pos, BuckLamp57Pos(3), BuckLamp57Index
Const BuckNumOfWalls = 31
Const BuckLampOffset = 50
Const BuckStepSize = 2
BuckLampAgeoutTimer.Interval = 500
BuckLamp20Pos = 594 - BuckLampOffset
BuckLamp19Pos = 505 - BuckLampOffset
BuckLamp18Pos = 402 - BuckLampOffset
BuckLamp17Pos = 305 - BuckLampOffset
BuckLamp16Pos = 213 - BuckLampOffset
BuckLamp57Pos(0) = BuckLamp16Pos
BuckLamp57Pos(1) = BuckLamp17Pos
BuckLamp57Pos(2) = BuckLamp18Pos

BuckMotor = False
BuckDir = 0
BuckPos = 0
sw37State = 0

' Precalculated to positions
' Start position 672x806
' End position 130x1270
' 400 steps for a smooth animation
Dim ii

For ii = 0 to 400
    BuckX(ii) = 672 - ii * ((672 -130) / 400)
    BuckY(ii) = 802 + ii * ((1270 -806) / 400)
Next

Sub solBuckMotor(Enabled)
    If BuckMotor Then
        BuckMotor = False
    Else
        BuckMotor = True
    End If
End Sub

Sub solBuckLeft(Enabled)
'    BuckDir = 1
'    BuckTimer.Enabled = 1
	'Debug.print Timer & " Ignoring Buck Left"
End Sub

Sub solBuckRight(Enabled)
'    'BuckDir = -1
'    BuckTimer.Enabled = 1
	'Debug.print Timer & " Ignoring Buck Right"
End Sub

Sub BuckTimer_Timer
    BuckPos = BuckPos + BuckDir*BuckStepSize
	If BuckPos > 400 Then BuckPos = 400
    If BuckPos < 0 Then BuckPos = 0

	BuckCheckPosition

    'check for home and stop the timer if reached
    If BuckPos = 0 Then
        Controller.Switch(71) = 1
        Me.Enabled = 0
		'Debug.print Timer & " Buck Home"
    Else
        Controller.Switch(71) = 0
    End If

    'check for left side and stop the timer if reached
    If BuckPos = 400 Then
        Controller.Switch(72) = 1
        'Me.Enabled = 0
		'Debug.print Timer & " Buck End"
		BuckDir = -1 'REVERSING direction instead of stopping
    Else
        Controller.Switch(72) = 0
    End If

    ' opto switch 37 - pulse the opto many times during moving
'    If BuckPos MOD 4 = 0 Then
'        Controller.Switch(37) = 1
'    Else
'        Controller.Switch(37) = 0
'    End If
'
'    ' opto switch 45 - pulse the opto many times during moving
'    If BuckPos MOD 50 = 0 Then
'        Controller.Switch(45) = 0
'    Else
'        Controller.Switch(45) = 1
'    End If

    ' animate the Buck
    Buck.X = BuckX(BuckPos)
    Buck.Y = BuckY(BuckPos)

	' raise Buck walls during animation
	BuckWallActive = int (BuckPos / 400 * (BuckNumOfWalls-1))
	'debug.print buckpos & "," & buckwallactive
	BuckWalls (BuckWallActive).IsDropped = 0 'Raise wall
	If BuckWallActive > 0 Then BuckWalls(BuckWallActive-1).IsDropped = 1 'Lower wall before it
	If BuckWallActive < BuckNumOfWalls-1 Then BuckWalls(BuckWallActive+1).IsDropped = 1 'Lower wall after it
End Sub

Sub BuckCheckPosition 'Will stop Buck if above flashing lamp
	Select Case BuckLampCurrent:
	
		Case 20: 'Stop buck at lamp 20
			If BuckX(BuckPos)> BuckLamp20Pos - BuckStepSize And BuckX(BuckPos) < BuckLamp20Pos + BuckStepSize Then BuckDir = 0': Debug.print Timer & " Stopping at 20"
		Case 19:
			If BuckX(BuckPos)> BuckLamp19Pos - BuckStepSize And BuckX(BuckPos) < BuckLamp19Pos + BuckStepSize Then BuckDir = 0': Debug.print Timer & " Stopping at 19"
		Case 18:
			If BuckX(BuckPos)> BuckLamp18Pos - BuckStepSize And BuckX(BuckPos) < BuckLamp18Pos + BuckStepSize Then BuckDir = 0': Debug.print Timer & " Stopping at 18"
		Case 17:
			If BuckX(BuckPos)> BuckLamp17Pos - BuckStepSize And BuckX(BuckPos) < BuckLamp17Pos + BuckStepSize Then BuckDir = 0': Debug.print Timer & " Stopping at 17"
		Case 16:
			If BuckX(BuckPos)> BuckLamp16Pos - BuckStepSize And BuckX(BuckPos) < BuckLamp16Pos + BuckStepSize Then BuckDir = 0': Debug.print Timer & " Stopping at 16"
		Case 57:
			If BuckX(BuckPos)> BuckLamp57Pos(BuckLamp57Index) - BuckStepSize And BuckX(BuckPos) < BuckLamp57Pos(BuckLamp57Index) + BuckStepSize Then BuckDir = 0': Debug.print Timer & " Stopping at BuckSuperJackpot:"&BuckX(BuckPos)
		Case 0: 'No lamp flashing
			:'Do nothing
	End Select
End Sub

Sub BuckTrackBuckLamps (blamp, benabled) 
	BuckLampCheck = blamp
	if (BuckLampCheck >= 16 and BuckLampCheck <=20) or BuckLampCheck = 57 then
		if benabled = 1 then 'track last Buck light set
			if BuckLampCurrent = BuckLampCheck then 'still flashing, reset ageout timer
				BuckLampAgeoutTimer.Enabled = 0
				BuckLampAgeoutTimer.Enabled = 1
			elseif BuckLampPossible = BuckLampCheck then 'New lamp flashing, start timer and send buck
				'debug.print Timer & " New BuckLamp Lit = " & BuckLampPossible
				BuckLampCurrent = BuckLampPossible
				BuckLampAgeoutTimer.Enabled = 0
				BuckLampAgeoutTimer.Enabled = 1
				BuckDetermineDirToLamp					
			else 'Lamp changed, send buck home
				'debug.print Timer & " BuckLamp Changing " & BuckLampCheck
				BuckLampPossible = BuckLampCheck
				BuckLampCurrent = 0
			end if
		end if
	end if

End Sub

Sub BuckDetermineDirToLamp
	Dim lamploc
	
	Select Case BuckLampCurrent:
		Case 16:
			lamploc = BuckLamp16Pos
		Case 17:
			lamploc = BuckLamp17Pos
		Case 18:
			lamploc = BuckLamp18Pos
		Case 19:
			lamploc = BuckLamp19Pos
		Case 20:
			lamploc = BuckLamp20Pos
		Case 57: 
			lamploc = BuckLamp57Pos(BuckLamp57Index)
			'debug.print "Light 57 - Super Jackpot Mode:" & BuckLamp57Index
	End Select

	'Move Buck to lamp
	If BuckX(BuckPos) > lamploc Then
		BuckDir = 1 'left
		BuckTimer.Enabled = 1
	Elseif BuckX(BuckPos) < lamploc Then 
		BuckDir = -1 'Right
		BuckTimer.Enabled = 1
	Else
		Dir = 0
	End If
End Sub

Sub BuckSuperJackpotMoveBuck
	BuckLamp57Index = (BuckLamp57Index +1) Mod 3
	BuckLampAgeoutTimer.Enabled = 0
	BuckLampAgeoutTimer.Enabled = 1
	BuckDetermineDirToLamp	
End Sub

Sub BuckLampAgeOutTimer_Timer 'If flashing buck light stops flashing resume previous dir
	BuckLampCurrent = 0
	BuckLampPossible = 0
	BuckLampAgeOutTimer.Enabled = 0
	BuckDir = -1
	'debug.print Timer & " BuckLamp ageout. Returning Home"
End Sub


Sub BuckWalls_Init
	For each xx in BuckWalls:xx.IsDropped = 1:next
	BuckWall1.IsDropped = 0
	BuckDir = 1
    BuckTimer.Enabled = 1
	'BuckPos = 0
End Sub

Sub Buck_Hit
    PlaySound "target"
    vpmTimer.PulseSw 1
End Sub

Sub BuckWalls_Hit 'collection 'Doesn't seem to work????
	msgbox "Hit"
    PlaySound "target"
    vpmTimer.PulseSw 1
End Sub

Sub BuckWall1_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall2_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall3_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall4_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall5_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall6_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall7_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall8_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall9_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall10_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall11_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall12_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall13_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall14_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall15_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall16_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall17_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall18_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall19_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall20_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall21_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall22_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall23_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall24_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall25_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall26_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall27_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall28_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall29_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall30_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub
Sub BuckWall31_Hit: BuckWallIsHit: Buck.TransX = -3: Me.TimerEnabled = 1:End Sub

Sub BuckWall1_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall2_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall3_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall4_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall5_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall6_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall7_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall8_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall9_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall10_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall11_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall12_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall13_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall14_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall15_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall16_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall17_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall18_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall19_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall20_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall21_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall22_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall23_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall24_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall25_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall26_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall27_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall28_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall29_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub
Sub BuckWall30_Timer:Buck.TransX = 3:Me.TimerEnabled = 0:End Sub

Sub BuckWallIsHit ()
	PlaySound "target"
	vpmTimer.PulseSw 1
	'Add check if Super Jackpot l57 active
	If BuckLampCurrent = 57 Then BuckSuperJackpotMoveBuck
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

			'Track Buck Lamp Location
			BuckTrackBuckLamps chgLamp(ii,0), chgLamp(ii,1)

        Next
    End If
    UpdateLamps
End Sub


 Sub UpdateLamps()
	NFadeL 3, l3
	NFadeL 5, l5
	NFadeL 6, l6
	NFadeL 7, l7
	NFadeL 8, l8
	NFadeL 9, l9
	NFadeL 10, l10
	NFadeL 11, l11
	NFadeL 14, l14
	NFadeL 15, l15
    NFadeL 16, l16  
    NFadeL 17, l17  
    NFadeL 18, l18  
    NFadeL 19, l19
	NFadeL 20, l20
	NFadeL 21, l21
	NFadeL 22, l22
	NFadeL 23, l23
	NFadeL 24, l24
	NFadeL 25, l25
	NFadeL 26, l26
	FadeDisableLighting 27, L27, 3	
 	FadeDisableLighting 28, L28, 3	
  	FadeDisableLighting 29, L29, 3	
	FadeDisableLighting 30, L30, 3	 
	NFadeL 31, l31
	NFadeL 32, l32
	NFadeL 33, l33
	NFadeL 35, l35
	NFadeL 38, l38
	NFadeL 39, l39
	NFadeL 40, l40
	NFadeL 41, l41
	NFadeL 42, l42
	NFadeL 43, l43
    NFadeL 45, l45  
    NFadeL 46, L46  
    NFadeL 47, L47 
	NFadeL 49, l49
	NFadeL 50, l50
	NFadeL 51, l51
	NFadeL 52, l52
	NFadeL 53, l53
	NFadeL 54, l54
	NFadeL 55, l55
	NFadeL 57, l57
	NFadeL 59, l59
	FadeDisableLighting 60, P60, 3
	NFadeLm 60, l60 'Bumper 1
	FadeDisableLighting 61, P61, 3
	NFadeLm 61, l61 'Bumper 2
	FadeDisableLighting 62, P62, 3
	NFadeLm 62, l62 'Bumper 3
	NFadeL 63, l63
	NFadeL 65, l65
	NFadeL 66, l66
	NFadeL 67, l67
	NFadeL 68, l68
	NFadeL 69, l69
	NFadeL 70, l70

  'Solenoid Controlled Flashers

	NFadeL 119, L119 

	FadeDisableLighting 120, L120, 5
    NFadeLm 120, L120a  

    NFadeL 121, L121 

	FadeDisableLighting 122, L122, 3
	NFadeLm 122, L122a  

	NFadeLm 125, L125a  
	NFadeLm 125, L125b 
	NFadeLm 125, L125c 
	NFadeL 125, L125d

	Flash 126, f126  

	NFadeLm 127, L127a
	NFadeLm 127, L127b
	NFadeLm 127, L127c
	NFadeL 127, L127d

	FadeDisableLighting 129, L129, 3
	NFadeLm 129, L129a

	FadeDisableLighting 131, l131, 3
	NFadeLm 131, L131a

	FadeDisableLighting 132, l132, 3
    NFadeLm 132, L132a
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
	vpmTimer.PulseSw 26
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
    Vol = Csng(BallVel(ball) ^2 / 6000)
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

Const tnob = 5 ' total number of balls
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

'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

	sw14Prim.Rotz = sw14.Currentangle
	sw5Prim.Rotz = sw5.Currentangle
	ElkPrim.roty = Elkdiverter.currentangle  + 0

End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

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
ballShadow(b).opacity = 90
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub



'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc


'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and 
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub
