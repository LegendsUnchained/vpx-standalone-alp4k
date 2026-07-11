'*********************************************************************
'1999 South Park by Sega Pinball
'*********************************************************************
'*********************************************************************
'Mechanics by:	Joe Balcer, Rob Hurtado
'Software by:	Neil Falconer, Orin Day
'*********************************************************************
'*********************************************************************
'recreated for Visual Pinball by Knorr
'*********************************************************************
'*********************************************************************
'I would like to give my sincere thanks to
'Mfuegemann, Freneticamnesic, Toxie and the VPdevs, Clark Kent,Arngrim
'Gigalula, Kiwi and JP for helping me while building this table
'*********************************************************************

'V1.2
'Rebuild KennyKickers
'Rebuild CartmanKickers
'Changed Rolling Sound
'Added RampRollingSounds
'Bug Fixes
'Small Changes With Light

'V1.1
'Fix for "do while" error (thx jp!)

'V1.0
'First Release For VP10.0



Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01560000", "sega.VBS", 3.36

'********************
'Standard definitions
'********************

Const cGameName = "sprk_103"
Const UseSolenoids = 1
Const UseLamps = 1
Const SSolenoidOn = "SolOn"
Const SSolenoidOff = "SolOff"
Const SFlipperOn = ""
Const SFlipperOff = ""
Const SCoin = ""

'************************************
'Switch SlingShotPlastics to Original
'************************************


SlingPlastics = 1			'Change to 1 for "Yes, i hate Timmy and Butters"

Dim TableWidth  : TableWidth  = Table1.Width
Dim TableHeight : TableHeight = Table1.Height

Const Angle = 20
Const ReflipAngle = 20

Dim bsTrough, PlungerIM, TopVuk, SuperVuk, nBall, aBall, SlingPlastics


Set LampCallback = GetRef("UpdateMultipleLamps")
Set MotorCallback = GetRef("RealTimeUpdates")
Set nBall = ckicker.createball
	ckicker.Kick 0, 0
If SlingPlastics = 1 then
	PlasticSlingshotLinks.Image = "PlasticSlingShotStan"
	PlasticSlingShotRechts.Image = "PlasticSlingShotKenny"
End if

'************
' Table init.
'************



Dim VRPreview : VRPreview = False
Dim VRMode : VRMode = False

Dim VR_Obj
If RenderingMode = 2 OR VRPreview Then
	VRMode = True
'    For Each VR_Obj In VRbg  : VR_Obj.Visible = 1 : Next
    For Each VR_Obj In VRCab     : VR_Obj.Visible = 1 : Next
    For Each VR_Obj In VRMega : VR_Obj.Visible = 1 : Next
	LockDownBar.visible=0
	Ramp16.visible=0
	Ramp15.visible=0
Else
    VRMode = False
'    For Each VR_Obj In VRbg  : VR_Obj.Visible = 0 : Next
    For Each VR_Obj In VRCab     : VR_Obj.Visible = 0 : Next
    For Each VR_Obj In VRMega : VR_Obj.Visible = 0 : Next
	if table1.showdt=true then
		LockDownBar.visible=1
		Ramp16.visible=1
		Ramp15.visible=1
	end if
End If

Sub table1_Init
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
'        .Switch(22) = 1 'close coin door
'        .Switch(24) = 1 'and keep it close
        PinMAMETimer.Interval = PinMAMEInterval
        PinMAMETimer.Enabled = true
        vpmNudge.TiltSwitch = swTilt
        vpmNudge.Sensitivity = 2
    End With

    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 14, 13, 12, 11, 10, 0, 0
        .InitKick BallRelease, 80, 5
        .InitExitSnd SoundFX("BallRelease4", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 5
        .IsTrough = 1
    End With


	Set TopVuk = New cvpmBallStack
	With TopVuk
		.InitSaucer sw46, 46, -90, 100
		.InitExitSnd SoundFX("SafeHouseKick", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
		.KickForceVar = 5
		.KickZ = 1.4
        .KickBalls = 1
	End With

    Set SuperVuk = New cvpmBallStack
    With SuperVuk
        .InitSw 0, 45, 0, 0, 0, 0, 0, 0
        .InitKick sw45, -80, 45
        .InitExitSnd SoundFX("kickandwire", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .KickZ = 1.4
    End With

    If table1.ShowDT = False then
        Ramp16.WidthTop = 0
        Ramp16.WidthBottom = 0
        Ramp15.WidthTop = 0
        Ramp15.WidthBottom = 0
        Korpus.Size_Y = 1.7
		Korpus.Z = 20
    End if
	InitZColRotate
End Sub


'******
'Trough
'******
Dim BallCount

Sub SolRelease(Enabled)
    If Enabled Then
'        If bsTrough.Balls = 5 Then vpmTimer.PulseSw 15
        If bsTrough.Balls > 0 Then bsTrough.ExitSol_On
    End If
End Sub

Sub BallRelease_UnHit()
	BallCount = BallCount +1
	End Sub

Sub Drain_Hit
	BallCount = BallCount -1
	RandomSoundDrain Drain
    bsTrough.AddBall Me
    If BallCount <1 then GiOff:GiOffState = True
End Sub

'Autoplunger

Sub AutoLaunch(Enabled)
	If Enabled Then
		Plunger.Autoplunger = True
		Plunger.Fire
		PlaySound SoundFX("AutoPlunger", DOFContactors)
		Else
		Plunger.Autoplunger = False
	End If
 End Sub

'********
'ChefVuk
'********

Sub sw46_Hit()
	PlaySound "SafeHouseHit"
	TopVuk.AddBall Me
End Sub

Sub sw46_UnHit()
	GiOn
End Sub

'*********
'CartmanVuk
'*********

Dim cBall, cBall1, cBall2, cBall3, cBall4, cBall5, cBall6, cBall7

Sub CartmanKickerHole_Hit()
    'RandomSoundCartman
	RandomSoundDelayedBallDropOnPlayfield CartmanKickerHole
    Set cBall = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole1_Hit()
    'RandomSoundCartman
	RandomSoundDelayedBallDropOnPlayfield CartmanKickerHole1
    Set cBall1 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole2_Hit()
    'RandomSoundCartman
	RandomSoundDelayedBallDropOnPlayfield CartmanKickerHole2
    Set cBall2 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole3_Hit()
    'RandomSoundCartman
	RandomSoundDelayedBallDropOnPlayfield CartmanKickerHole3
    Set cBall3 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole4_Hit()
    'RandomSoundCartman
	RandomSoundDelayedBallDropOnPlayfield CartmanKickerHole4
    Set cBall4 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole5_Hit()
    'RandomSoundCartman
	RandomSoundDelayedBallDropOnPlayfield CartmanKickerHole5
    Set cBall5 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole6_Hit()
    'RandomSoundCartman
	RandomSoundDelayedBallDropOnPlayfield CartmanKickerHole6
    Set cBall6 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole7_Hit()
    'RandomSoundCartman
	RandomSoundDelayedBallDropOnPlayfield CartmanKickerHole7
    Set cBall7 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub


Sub SuperVukAddBall()
	SuperVuk.AddBall 1
End Sub


Sub CartmanKickerHole_Timer
    Do While cBall.Z > 0
        cBall.Z = cBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole1_Timer
    Do While cBall1.Z > 0
        cBall1.Z = cBall1.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole2_Timer
    Do While cBall2.Z > 0
        cBall2.Z = cBall2.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole3_Timer
    Do While cBall3.Z > 0
        cBall3.Z = cBall3.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole4_Timer
    Do While cBall4.Z > 0
        cBall4.Z = cBall4.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole5_Timer
    Do While cBall5.Z > 0
        cBall5.Z = cBall5.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole6_Timer
    Do While cBall6.Z > 0
        cBall6.Z = cBall6.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole7_Timer
    Do While cBall7.Z > 0
        cBall7.Z = cBall7.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub sw45_UnHit()
	BallinToilet = 0
End Sub

'*********
'KennyVuk
'*********

Dim KennySolLeft
Dim KennySolRight

Dim kBall, kBall1, kBall2, kBall3, kBall4, kBall5

Sub KennyLeft(Enabled)
	If Enabled then
		UpdateKennyLeft.Enabled = 1
		KennySolLeft = True
		RandomSoundKennyMove
		Else
		UpdateKennyLeft.Enabled = 0
		Kenny.RotZ = 0
		KennyL.RotZ = 0
	End if
End Sub

Sub KennyRight(Enabled)
	If Enabled then
		UpdateKennyRight.Enabled = 1
		KennySolRight = True
		RandomSoundKennyMove
		Else
		UpdateKennyright.Enabled = 0
		Kenny.RotZ = 0
		KennyL.RotZ = 0
	End if
End Sub

Sub KennyDead(Enabled)
	If Enabled then
		KennyFlipper.RotatetoEnd
		PlaySound SoundFX("TrapDoorLow",DOFContactors)
		Else
		KennyFlipper.RotatetoStart
		PlaySound SoundFX("FlipperDown",DOFContactors)
	End if
End Sub

Sub updatekennydead_Timer()
	    Kenny.RotX = KennyFlipper.CurrentAngle
End Sub


'Kenny Animation
'RotX -1 = KennyDead
'RotY +1 = rotate clockwise
'RotZ +1 = KennyLeft
'RotZ -1 = KennyRight

Sub UpdateKennyLeft_Timer()
	If KennySolLeft = True And Kenny.RotZ < 8 then Kenny.RotZ = Kenny.RotZ +1
	If KennysolLeft = False And Kenny.RotZ > 0 then Kenny.RotZ = Kenny.RotZ -1
	If Kenny.RotZ >= 8 Then KennySolLeft = False

	If KennySolLeft = True And KennyL.RotZ < 8 then KennyL.RotZ = KennyL.RotZ +1
	If KennysolLeft = False And KennyL.RotZ > 0 then KennyL.RotZ = KennyL.RotZ -1
	If KennyL.RotZ >= 8 Then KennySolLeft = False
End Sub

Sub UpdateKennyRight_Timer()
	If KennySolRight = True And Kenny.RotZ > -8 then Kenny.RotZ = Kenny.RotZ -1
	If KennySolRight = False And Kenny.RotZ < 0 then Kenny.RotZ = Kenny.RotZ +1
	If Kenny.RotZ <= -8 Then KennySolRight = False

	If KennySolRight = True And KennyL.RotZ > -8 then KennyL.RotZ = KennyL.RotZ -1
	If KennySolRight = False And KennyL.RotZ < 0 then KennyL.RotZ = KennyL.RotZ +1
	If KennyL.RotZ <= -8 Then KennySolRight = False
End Sub




Sub KennyKickerHole_Hit()
    'RandomSoundKenny
	RandomSoundDelayedBallDropOnPlayfield KennyKickerHole
    Set kBall = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 44
    vpmTimer.AddTimer 2000, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub KennyKickerHole_Timer()
    Do While kball.Z > 0
        kBall.Z = kBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub KennyKickerHole1_Hit()
    'RandomSoundKenny
	RandomSoundDelayedBallDropOnPlayfield KennyKickerHole1
    Set kBall1 = ActiveBall:Me.TimerEnabled = 1
	vpmTimer.PulseSw 44
    vpmTimer.AddTimer 2000, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub


Sub KennyKickerHole1_Timer()
    Do While kBall1.Z > 0
        kBall1.Z = kBall1.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub KennyKickerHole2_Hit()
    'RandomSoundKenny
	RandomSoundDelayedBallDropOnPlayfield KennyKickerHole2
    Set kBall2 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 44
    vpmTimer.AddTimer 2000, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub KennyKickerHole2_Timer()
    Do While kBall2.Z > 0
        kBall2.Z = kBall2.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub KennyKickerHole3_Hit()
    'RandomSoundKenny
	RandomSoundDelayedBallDropOnPlayfield KennyKickerHole3
    Set kBall3 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 44
    vpmTimer.AddTimer 2000, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub KennyKickerHole3_Timer()
    Do While kBall3.Z > 0
        kBall3.Z = kBall3.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub


Sub KennyKickerHole4_Hit()
    'RandomSoundKenny
	RandomSoundDelayedBallDropOnPlayfield KennyKickerHole4
    Set kBall4 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 44
    vpmTimer.AddTimer 2000, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub KennyKickerHole4_Timer()
    Do While kBall4.Z > 0
        kBall4.Z = kBall4.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub


Sub KennyKickerHole5_Hit()
	RandomSoundDelayedBallDropOnPlayfield KennyKickerHole5
    'RandomSoundKenny
    Set kBall5 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 44
    vpmTimer.AddTimer 2000, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub KennyKickerHole5_Timer()
    Do While kBall5.Z > 0
        kBall5.Z = kBall5.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub


Sub BallTrigger_Hit()
	ActiveBall.VelZ = +15
End Sub


'*********
'Toilet
'*********
Dim FlasherF7Visible


'Seat
Sub ToiletSeatLid(Enabled)
	If Enabled then
		ToiletFlipper.RotatetoEnd
		ToiletKicker.Enabled = True
		PlaySound SoundFX("FlipperDown",DOFContactors)
	Else
		ToiletFlipper.RotatetoStart
		ToiletKicker.Enabled = False
		PlaySound SoundFX("FlipperDown",DOFContactors)
	End if
End Sub


'MrHankeyUp
Sub MrHankeyup(Enabled)
	If Enabled then
	MrHankeyFlipper.RotatetoEnd
	CisternFlipper.RotatetoEnd
	FlasherF7Visible = True
	PlaySound SoundFX("TrapDoorLow",DOFContactors)
	End if
End Sub

'MrHankey
Sub MrHankeyDown(Enabled)
	If Enabled then
	MrHankeyFlipper.RotatetoStart
	CisternFlipper.RotatetoStart
	FlasherF7Visible = False
	End if
End Sub

Dim BallinToilet

Sub ToiletKicker_Hit
    WireRampOff
	BallinToilet = 1
	sw26spinner.timerenabled = False
    RandomSoundCartman
    Set aBall = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 43
    vpmTimer.AddTimer 2400, "SuperVukAddBall'"
    GiBlinking
	if BallCount < 2 then
		vpmTimer.AddTimer 2400, "GiOff'"
		else
		vpmTimer.AddTimer 2400, "GiOn'"
	End if
End Sub


Sub ToiletKicker_Timer
    Do While aBall.Z > 0
        aBall.Z = aBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
End Sub

'''*****************************************************************************************
'''*freneticamnesic level nudge script, based on rascals nudge bobble with help from gtxjoe*
'''*     add timers and "Nudgebobble(keycode)" to left and right tilt keys to activate     *
'''*****************************************************************************************


dim Shaketime, keycode

ShakeTime = 20


Sub LeftHit_Hit()
	nudgebobble2(keycode)
	nudgebobble3(keycode)
	nudgebobble4(keycode)
End Sub

Sub RightHit_Hit()
	nudgebobble2(keycode)
	nudgebobble3(keycode)
	nudgebobble4(keycode)
End Sub



Dim bgcharctr2a:bgcharctr2a = 3
Dim bgcharctr2b:bgcharctr2b = 3
Dim bgcharctr3a:bgcharctr3a = 3
Dim bgcharctr3b:bgcharctr3b = 3
Dim bgcharctr4a:bgcharctr4a = 3
Dim bgcharctr4b:bgcharctr4b = 3

Dim centerlocation2:centerlocation2 = 0
Dim bgdegree2a:bgdegree2a = 5 'move +/- 8 degrees
Dim bgdegree2b:bgdegree2b = 5 'move +/- 8 degrees
Dim bgdurationctr2a:bgdurationctr2a = 0
Dim bgdurationctr2b:bgdurationctr2b = 0
Dim centerlocation3:centerlocation3 = 0
Dim bgdegree3a:bgdegree3a = 5 'move +/- 8 degrees
Dim bgdegree3b:bgdegree3b = 5 'move +/- 8 degrees
Dim bgdurationctr3a:bgdurationctr3a = 0
Dim bgdurationctr3b:bgdurationctr3b = 0
Dim centerlocation4:centerlocation4 = 0
Dim bgdegree4a:bgdegree4a = 5 'move +/- 8 degrees
Dim bgdegree4b:bgdegree4b = 5 'move +/- 8 degrees
Dim bgdurationctr4a:bgdurationctr4a = 0
Dim bgdurationctr4b:bgdurationctr4b = 0

Sub LevelT2_Timer()
	eric.RotAndTra7 = eric.RotAndTra7 + bgcharctr2a  'change rotation value by bgcharctr
	If eric.RotAndTra7 >= bgdegree2a + centerlocation2 then bgcharctr2a = -1:bgdurationctr2a = bgdurationctr2a + 1   'if level moves past max degrees, change direction and increate durationctr
	If eric.RotAndTra7 <= -bgdegree2a + centerlocation2 then bgcharctr2a = 1  'if level moves past min location, change direction
	eric.ObjRotX = eric.ObjRotX + bgcharctr2b  'change rotation value by bgcharctr
	If eric.ObjRotX >= bgdegree2b + centerlocation2 then bgcharctr2b = -1:bgdurationctr2b = bgdurationctr2b + 1   'if level moves past max degrees, change direction and increate durationctr
	If eric.ObjRotX <= -bgdegree2b + centerlocation2 then bgcharctr2b = 1  'if level moves past min location, change direction

	If bgdurationctr2a = ShakeTime then bgdegree2a = bgdegree2a - 2:bgdurationctr2a = 0 'if level has moved back and forth 4 times, decrease amount of movement by -2 and repeat by resetting durationctr
	If bgdurationctr2b = ShakeTime then bgdegree2b = bgdegree2b - 2:bgdurationctr2b = 0 'if level has moved back and forth 4 times, decrease amount of movement by -2 and repeat by resetting durationctr
	If bgdegree2a <= 0 then LevelT2.Enabled = False:bgdegree2a = 5 'if amount of movement is 0, turn off LevelT timer and reset movement back to max 8 degrees
	If bgdegree2b <= 0 then LevelT2.Enabled = False:bgdegree2b = 5 'if amount of movement is 0, turn off LevelT timer and reset movement back to max 8 degrees
End Sub

Sub LevelT3_Timer()
	'Dim loopctr
	Kyle.RotAndTra7 = Kyle.RotAndTra7 + bgcharctr3a  'change rotation value by bgcharctr
	If Kyle.RotAndTra7 >= bgdegree3a + centerlocation3 then bgcharctr3a = -1:bgdurationctr3a = bgdurationctr3a + 1   'if level moves past max degrees, change direction and increate durationctr
	If Kyle.RotAndTra7 <= -bgdegree3a + centerlocation3 then bgcharctr3a = 1  'if level moves past min location, change direction
	Kyle.ObjRotX = Kyle.ObjRotX + bgcharctr3b  'change rotation value by bgcharctr
	If Kyle.ObjRotX >= bgdegree3b + centerlocation3 then bgcharctr3b = -1:bgdurationctr3b = bgdurationctr3b + 1   'if level moves past max degrees, change direction and increate durationctr
	If Kyle.ObjRotX <= -bgdegree3b + centerlocation3 then bgcharctr3b = 1  'if level moves past min location, change direction

	If bgdurationctr3a = ShakeTime then bgdegree3a = bgdegree3a - 2:bgdurationctr3a = 0 'if level has moved back and forth 4 times, decrease amount of movement by -2 and repeat by resetting durationctr
	If bgdurationctr3b = ShakeTime then bgdegree3b = bgdegree3b - 2:bgdurationctr3b = 0 'if level has moved back and forth 4 times, decrease amount of movement by -2 and repeat by resetting durationctr
	If bgdegree3a <= 0 then LevelT3.Enabled = False:bgdegree3a = 5 'if amount of movement is 0, turn off LevelT timer and reset movement back to max 8 degrees
	If bgdegree3b <= 0 then LevelT3.Enabled = False:bgdegree3b = 5 'if amount of movement is 0, turn off LevelT timer and reset movement back to max 8 degrees
End Sub

Sub LevelT4_Timer()
	'Dim loopctr
	Stan.RotAndTra7 = Stan.RotAndTra7 + bgcharctr4a  'change rotation value by bgcharctr
	If Stan.RotAndTra7 >= bgdegree4a + centerlocation4 then bgcharctr4a = -1:bgdurationctr4a = bgdurationctr4a + 1   'if level moves past max degrees, change direction and increate durationctr
	If Stan.RotAndTra7 <= -bgdegree4a + centerlocation4 then bgcharctr4a = 1  'if level moves past min location, change direction
	Stan.ObjRotX = Stan.ObjRotX + bgcharctr4b  'change rotation value by bgcharctr
	If Stan.ObjRotX >= bgdegree4b + centerlocation4 then bgcharctr4b = -1:bgdurationctr4b = bgdurationctr4b + 1   'if level moves past max degrees, change direction and increate durationctr
	If Stan.ObjRotX <= -bgdegree4b + centerlocation4 then bgcharctr4b = 1  'if level moves past min location, change direction

	If bgdurationctr4a = ShakeTime then bgdegree4a = bgdegree4a - 2:bgdurationctr4a = 0 'if level has moved back and forth 4 times, decrease amount of movement by -2 and repeat by resetting durationctr
	If bgdurationctr4b = ShakeTime then bgdegree4b = bgdegree4b - 2:bgdurationctr4b = 0 'if level has moved back and forth 4 times, decrease amount of movement by -2 and repeat by resetting durationctr
	If bgdegree4a <= 0 then LevelT4.Enabled = False:bgdegree4a = 5 'if amount of movement is 0, turn off LevelT timer and reset movement back to max 8 degrees
	If bgdegree4b <= 0 then LevelT4.Enabled = False:bgdegree4b = 5 'if amount of movement is 0, turn off LevelT timer and reset movement back to max 8 degrees
End Sub


Sub Nudgebobble2(keycode)
	LevelT2.Enabled = True:bgdurationctr2a = 0:bgdegree2a = 7:bgdurationctr2b = 0:bgdegree2b = 7
	'LevelT3.Enabled = True:bgdurationctr3 = 0:bgdegree3 = 7
	'LevelT4.Enabled = True:bgdurationctr4 = 0:bgdegree4 = 7
End Sub

Sub Nudgebobble3(keycode)
	'LevelT2.Enabled = True:bgdurationctr2 = 0:bgdegree2 = 7
	LevelT3.Enabled = True:bgdurationctr3a = 0:bgdegree3a = 7:bgdurationctr3b = 0:bgdegree3b = 7
	'LevelT4.Enabled = True:bgdurationctr4 = 0:bgdegree4 = 7
End Sub

Sub Nudgebobble4(keycode)
	'LevelT2.Enabled = True:bgdurationctr2 = 0:bgdegree2 = 7
	'LevelT3.Enabled = True:bgdurationctr3 = 0:bgdegree3 = 7
	LevelT4.Enabled = True:bgdurationctr4a = 0:bgdegree4a = 7:bgdurationctr4b = 0:bgdegree4b = 7
End Sub

'Sub bobblesome_Timer()  'This looks like a free running timer that 1 out of ten times will start movement 
'	Dim chance
'	chance = Int(10*Rnd+1)
'	If chance = 5 then Nudgebobble(CenterTiltKey)
'End Sub




'*********
'Solenoids
'*********

SolCallback(1) = "SolRelease"
SolCallback(2) = "AutoLaunch"
SolCallback(3) = "SuperVuk.SolOut"
SolCallback(4) = "TopVuk.SolOut"
SolCallback(5) = "ToiletSeatLid"
SolCallback(6) = "MrHankeyUp"
SolCallback(13) = "MrHankeyDown"
SolCallBack(14) = "KennyDead"
SolCallBack(19) = "KennyLeft"
SolCallback(20) = "KennyRight"
'SolCallback(24) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"



'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If KeyCode = MechanicalTilt Then
        vpmTimer.PulseSw vpmNudge.TiltSwitch
        Exit Sub
    End if
	If keycode = PlungerKey Then Plunger.Pullback:SoundPlungerPull:End if
	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress, LF
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress, RF
	If keycode = LeftTiltKey Then Nudge 90, 0.5 : SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 0.5 : SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 0.5 : SoundNudgeCenter
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If


	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = PlungerKey Then Plunger.Fire: SoundPlungerReleaseBall
	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
    If vpmKeyUp(keycode) Then Exit Sub
End Sub


'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(GameOnSolenoid) = "SolGameOn"   ' Whitestar flipper-enable relay (sol 15)

' FlipperActivate fires the bat instantly on keypress instead of waiting for the
' ROM solenoid round-trip. That bypass also skips the ROM's flipper-enable gate, so
' without this the bats would flip while the ROM has flippers powered down (between
' balls, tilt, attract). The GameOn relay (sol 15) is that enable signal: track it
' and only allow the predicted fire when the ROM actually has flippers powered.
Dim FlippersEnabled : FlippersEnabled = False

Sub SolGameOn(Enabled)
	FlippersEnabled = Enabled
	If Not Enabled Then
		' ROM cut flipper power — drop the main bats to match. The instant-fire
		' bypasses the ROM, so a flipper held across a disable would otherwise stay
		' up. Upper/mech flippers stay ROM-driven and are left untouched here.
		LeftFlipper.RotateToStart
		RightFlipper.RotateToStart
	End If
End Sub

' --- Configurable flipper input delay (simulates ROM solenoid latency) -------------
' Diagnostic knob: the instant-fire above removes the ROM round-trip. Set this > 0 to
' add that latency back, in milliseconds, so you can A/B test whether a "weak / late"
' flipper feel is ROM delay or actual flipper strength.
'   0   = instant fire (current optimized behavior)
'   ~30 = roughly one WPC/Whitestar ROM cycle of lag
'   60+ = obviously laggy, for an exaggerated comparison
' Note: the delay is quantized to vpmTimer's resolution, so it's approximate. Applies
' to the bat raise only; release stays instant. Resolution is plenty for A/B feel.
Dim FlipperDelay : FlipperDelay = 0

' Delayed-fire callbacks invoked by vpmTimer. Re-check state at fire time so a tap
' released during the delay (or a mid-delay flipper disable) doesn't raise a stale bat.
Sub FireLeftFlipperDelayed
	If LFPress = 1 And FlippersEnabled Then LF.Fire
End Sub

Sub FireRightFlipperDelayed
	If RFPress = 1 And FlippersEnabled Then RF.Fire
End Sub

'Sub SolLFlipper(Enabled)
'   If Enabled Then
'        PlaySound SoundFX("FlipperUpLeft", DOFContactors):FlipperLeft.RotateToEnd
'    Else
'        PlaySound SoundFX("FlipperDown", DOFContactors):FlipperLeft.RotateToStart:FlipperLeft.return = returnspeed * 0.5
'    End If
'End Sub

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper

        End If	
	Else
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel

	End If
End Sub


'Sub SolRFlipper(Enabled)
'    If Enabled Then
'        PlaySound SoundFX("FlipperUpRightBoth", DOFContactors):FlipperRight.RotateToEnd
'    Else
'        PlaySound SoundFX("FlipperDown", DOFContactors):FlipperRight.RotateToStart:FlipperRight.return = returnspeed * 0.5
'    End If
'End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub


'dim returnspeed, lfstep, rfstep
'returnspeed = FlipperLeft.return
'lfstep = 1
'rfstep = 1

'sub leftflipper_timer()
'	select case lfstep
'		Case 1: leftflipper.return = returnspeed * 0.6 :lfstep = lfstep + 1
'		Case 2: leftflipper.return = returnspeed * 0.7 :lfstep = lfstep + 1
'		Case 3: leftflipper.return = returnspeed * 0.8 :lfstep = lfstep + 1
'		Case 4: leftflipper.return = returnspeed * 0.9 :lfstep = lfstep + 1
'		Case 5: leftflipper.return = returnspeed * 1 :lfstep = lfstep + 1
'		Case 6: leftflipper.timerenabled = 0 : lfstep = 1
'	end select
'end sub

'sub rightflipper_timer()
'	select case rfstep
'		Case 1: rightflipper.return = returnspeed * 0.6 :rfstep = rfstep + 1
'		Case 2: rightflipper.return = returnspeed * 0.7 :rfstep = rfstep + 1
'		Case 3: rightflipper.return = returnspeed * 0.8 :rfstep = rfstep + 1
'		Case 4: rightflipper.return = returnspeed * 0.9 :rfstep = rfstep + 1
'		Case 5: rightflipper.return = returnspeed * 1 :rfstep = rfstep + 1
'		Case 6: rightflipper.timerenabled = 0 : rfstep = 1
'	end select
'end sub


'*********
' Switches
'*********

Sub sw16_Hit:Controller.Switch(16) = 1:sw16wire.RotX = 15:PlaySound "metalhit_thin":End Sub 'shooterlane'
Sub sw16_UnHit:Controller.Switch(16) = 0:sw16wire.RotX = 0:GiOn:End Sub

Sub sw25_Hit:Controller.Switch(25) = 1:End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0: End Sub

Sub sw26_Hit:Controller.Switch(26) = 1: sw26spinner.timerenabled = True:End Sub
Sub sw26_UnHit:Controller.Switch(26) = 0: End Sub

Sub sw26spinner_Timer()
	If (sw26spinner.currentangle > 9 Or  sw26spinner.currentangle < -9) then
	GiBlinking
	Else
	if Ballcount > 0 then GiOn
'	if Ballcount < 1 then GiOff
	End if
End Sub

Sub sw42_Hit: Controller.Switch(42) = 1:ActiveBall.VelZ = 0: End Sub
Sub sw42_UnHit: Controller.Switch(42) = 0: End Sub
'Sub Wall50_Hit: vpmTimer.PulseSw 42: ActiveBall.VelZ = 0:End Sub

Sub sw47_Hit:Controller.Switch(47) = 1:sw47wire.RotX = 15:PlaySound "metalhit_thin":End Sub 'left orbit'
Sub sw47_UnHit:Controller.Switch(47) = 0:sw47wire.RotX = 0:End Sub

Sub sw57_Hit:Controller.Switch(57) = 1:GiOff:sw57wire.RotX = 15:PlaySound "metalhit_thin":End Sub 'left outlane'
Sub sw57_UnHit:Controller.Switch(57) = 0:GiOn::sw57wire.RotX = 0:End Sub

Sub sw58_Hit:Controller.Switch(58) = 1:sw58wire.RotX = 15:sw26spinner.timerenabled = False:PlaySound "metalhit_thin":End Sub 'left inlane'
Sub sw58_UnHit:Controller.Switch(58) = 0:sw58wire.RotX = 0:End Sub

Sub sw60_Hit:Controller.Switch(60) = 1:GiOff:sw60wire.RotX = 15:PlaySound "metalhit_thin":End Sub 'right outlane'
Sub sw60_UnHit:Controller.Switch(60) = 0:GiOn:sw60wire.RotX = 0:End Sub

Sub sw61_Hit:Controller.Switch(61) = 1:sw61wire.RotX = 15:PlaySound "metalhit_thin":End Sub 'right inlane'
Sub sw61_UnHit:Controller.Switch(61) = 0:sw61wire.RotX = 0:End Sub

'*********
' Targets
'*********


Sub T17_Hit:vpmTimer.PulseSw 17: DTGrandpa.RotX = DTGrandpa.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T17_Timer:DTGrandpa.RotX = DTGrandpa.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T18_Hit:vpmTimer.PulseSw 18: DTMsCartman.RotX = DTMsCartman.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T18_Timer:DTMsCartman.RotX = DTMsCartman.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T19_Hit:vpmTimer.PulseSw 19: DTMayor.RotX = DTMayor.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T19_Timer:DTMayor.RotX = DTMayor.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T20_Hit:vpmTimer.PulseSw 20: DTBarbrady.RotX = DTBarbrady.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T20_Timer:DTBarbrady.RotX = DTBarbrady.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T21_Hit:vpmTimer.PulseSw 21: DTHugo.RotX = DTHugo.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T21_Timer:DTHugo.RotX = DTHugo.RotX -5:Me.TimerEnabled = 0:End Sub


Sub T22_Hit:vpmTimer.PulseSw 22: DTJ.RotX = DTJ.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T22_Timer:DTJ.RotX = DTJ.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T23_Hit:vpmTimer.PulseSw 23: DTSirJohn.RotX = DTSirJohn.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T23_Timer:DTSirJohn.RotX = DTSirJohn.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T24_Hit:vpmTimer.PulseSw 24: DTH.RotX = DTH.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T24_Timer:DTH.RotX = DTH.RotX -5:Me.TimerEnabled = 0:End Sub


Sub T34_Hit:vpmTimer.PulseSw 34: DTWendy.RotX = DTWendy.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T34_Timer:DTWendy.RotX = DTWendy.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T35_Hit:vpmTimer.PulseSw 35: DTMrGarrison.RotX = DTMrGarrison.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T35_Timer:DTMrGarrison.RotX = DTMrGarrison.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T36_Hit:vpmTimer.PulseSw 36: DTMrMackey.RotX = DTMrMackey.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T36_Timer:DTMrMackey.RotX = DTMrMackey.RotX -5:Me.TimerEnabled = 0:End Sub


Sub T37_Hit:vpmTimer.PulseSw 37: DTNed.RotX = DTNed.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T37_Timer:DTNed.RotX = DTNed.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T38_Hit:vpmTimer.PulseSw 38: DTMephisto.RotX = DTMephisto.RotX +5:Me.TimerEnabled = 1:End Sub
Sub T38_Timer:DTMephisto.RotX = DTMephisto.RotX -5:Me.TimerEnabled = 0:End Sub


'*********
' Gates
'*********

Sub GateUpdate()
	WireBumpers.RotX = BumperGate.currentangle +90
	WirePlungerLane.RotX = PlungerLaneGate.currentangle +90
	sw25wire.RotX = sw25spinner.currentangle +90
	sw26wire.RotX = sw26spinner.currentangle +90
End Sub


'*********
' Bumper
'*********

Sub LeftBumper_hit:vpmTimer.pulseSw 49:RandomSoundBumperTop LeftBumper:Me.TimerEnabled = 1:End Sub
Sub LeftBumper_Timer:Me.Timerenabled = 0:End Sub

Sub RightBumper_hit:vpmTimer.pulseSw 50:RandomSoundBumperMiddle RightBumper:Me.TimerEnabled = 1:End Sub
Sub RightBumper_Timer:Me.Timerenabled = 0:End Sub

Sub BottomBumper_hit:vpmTimer.pulseSw 51:RandomSoundBumperBottom BottomBumper:Me.TimerEnabled = 1:End Sub
Sub BottomBumper_Timer:Me.Timerenabled = 0:End Sub



'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************


Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect ActiveBall
	RandomSoundSlingshotRight Sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    vpmTimer.PulseSw 62
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
   	LS.VelocityCorrect ActiveBall
	RandomSoundSlingshotLeft Sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    vpmTimer.PulseSw 59
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


'*****************************************
 '  JP's Fading Lamps 3.4 VP9 Fading only
 '      Based on PD's Fading Lights
 ' SetLamp 0 is Off
 ' SetLamp 1 is On
 ' LampState(x) current state
 '****************************************
 
InitLamps

Sub InitLamps()
    Set Lights(1) = l1
    Set Lights(2) = l2
    Set Lights(3) = l3
    Set Lights(4) = l4
    Set Lights(5) = l5
    Set Lights(6) = l6
    Set Lights(7) = l7
    Set Lights(8) = l8
    Set Lights(9) = l9
    Set Lights(10) = l10
    Set Lights(11) = l11
    Set Lights(12) = l12
    Set Lights(13) = l13
    Set Lights(14) = l14
    Set Lights(15) = l15
    Set Lights(16) = l16
    Set Lights(17) = l17
    Set Lights(18) = l18
    Set Lights(19) = l19
    Set Lights(20) = l20
    Set Lights(21) = l21
    Set Lights(22) = l22
    Set Lights(23) = l23
    Set Lights(24) = l24
    Set Lights(25) = l25
    Set Lights(26) = l26
    Set Lights(27) = l27
    Set Lights(28) = l28
    Set Lights(29) = l29
    Set Lights(30) = l30
    Set Lights(33) = l33
    Set Lights(34) = l34
    Set Lights(35) = l35
    Set Lights(36) = l36
    Set Lights(37) = l37
    Set Lights(38) = l38
    Set Lights(39) = l39
    Set Lights(40) = l40
    Set Lights(41) = l41
    Set Lights(42) = l42
    Set Lights(43) = l43
    Set Lights(44) = l44
    Set Lights(45) = l45
    Set Lights(46) = l46
    Set Lights(48) = l48
    Set Lights(50) = l50
    Set Lights(51) = l51
    Set Lights(52) = l52
    Set Lights(53) = l53
    Set Lights(54) = l54
    Set Lights(55) = l55
    Set Lights(56) = l56
    Set Lights(57) = l57
    Set Lights(58) = l58
    Set Lights(59) = l59
    Set Lights(60) = l60
    Set Lights(61) = l61
    Set Lights(62) = l62
    Set Lights(63) = l63
    Set Lights(64) = l64
End Sub
 
  Sub UpdateMultipleLamps()
		If l30.state = 1 then Kenny.Visible = False:KennyL.Visible = True: else Kenny.Visible = True: KennyL.Visible = False
		If l22.state = 1 then BulbCoverKennyRed.visible = False:BulbCoverKennyRedL.visible = True:BulbCoverKennyRedF.visible = True:BulbCoverKennyRedF1.Visible = True: else BulbCoverKennyRed.visible = True: BulbCoverKennyRedL.visible = False:BulbCoverKennyRedF.visible = False: BulbCoverKennyRedF1.Visible = False
		If l23.state = 1 then BulbCoverKennyYellow.visible = False:BulbCoverKennyYellowL.visible = True:BulbCoverKennyYellowF.visible = True: BulbCoverKennyYellowF1.Visible = True: else BulbCoverKennyYellow.visible = True: BulbCoverKennyYellowL.visible = False: BulbCoverKennyYellowF.visible = False:BulbCoverKennyYellowF1.Visible = False
		If l46.state = 1 then bulbgreenleft.visible = False:bulbgreenleftL.visible = True:bulbgreenleftF.Visible = True: Else bulbgreenleft.visible = True:bulbgreenleftL.visible = False:bulbgreenleftF.visible = False
		If l48.state = 1 then bulbgreenright.visible = False:bulbgreenrightL.visible = True:bulbgreenrightf.visible = True: Else bulbgreenright.visible = True:bulbgreenrightL.visible = False:bulbgreenrightF.visible = False
		If l30.state = 1 then l30a.state = 1:FlasherF6.Visible = True: Else FlasherF6.Visible = False:l30a.state = 0
		If l38.state = 1 then BumperLeftLight:BumperFlasher.Visible = 1: Else BumperLeftLightOff:BumperFlasher.Visible = 0
		If l39.state = 1 then BumperRightLight: Else BumperRightLightOff
		If l40.state = 1 then BumperBottomLight: Else BumperBottomLightOff
End Sub

'RainbowLight

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
    l14.colorfull = RGB(Red, Green, Blue)
'    light2.color = RGB(Red, Green, Blue)
'    textbox1.text = Red
'    textbox2.text = Green
'    textbox3.text = Blue
End Sub


'*********************
'Generell Illumination
'*********************

Dim bulb, fbulb, GiOnState, GiOffState
dim flashing
flashing = 0

Sub GIOff()
	GiOffState = True
	for each bulb in GI
	bulb.state = 0
	if flashing then Table1.ColorGradeImage = "-70"
	for each fbulb in GiFlasher
	fbulb.visible = False
	next
	next
End Sub

Sub GIOn()
	GiOnState = True
	for each bulb in GI
	bulb.state = 1
	for each fbulb in GiFlasher
	fbulb.visible = True
	Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
	next
	next
End Sub

Sub GiBlinking()
	GiOnState = False
	GiOffState = False
	FlasherTimer1.Enabled = True
	if flashing then Table1.ColorGradeImage = "-30"
End Sub


'GiFlasherBlinking

Sub FlasherTimer1_Timer()
	for each fbulb in GiFlasher
	fbulb.visible = 0
	for each bulb in Gi
	bulb.state = 0
	FlasherTimer1.Enabled = 0
	If GiOffState = False then FlasherTimer2.Enabled = 1
	next
	next
End Sub

Sub FlasherTimer2_Timer()
	for each fbulb in GiFlasher
	fbulb.visible = 1
	for each bulb in Gi
	bulb.state = 1
	If GiOnState = False then FlasherTimer1.Enabled = 1: Else 
	FlasherTimer2.Enabled = 0
	next
	next
End Sub


'*********************
'BumperRingLights
'*********************

Sub BumperLeftLight()
	for each bulb in RingLightLeftBumper
	bulb.state = 1
	next
End Sub

Sub BumperLeftLightOff()
	for each bulb in RingLightLeftBumper
	bulb.state = 0
	next
End Sub

Sub BumperRightLight()
	for each bulb in RingLightRightBumper
	bulb.state = 1
	next
End Sub

Sub BumperRightLightOff()
	for each bulb in RingLightRightBumper
	bulb.state = 0
	next
End Sub


Sub BumperBottomLight()
	for each bulb in RingLightBottomBumper
	bulb.state = 1
	next
End Sub

Sub BumperBottomLightOff()
	for each bulb in RingLightBottomBumper
	bulb.state = 0
	next
End Sub



'*********
'Flasher
'*********

SolCallback(7) = "FlashPops"
SolCallback(18) = "FlashTopVuk"
SolCallback(25) = "FlashStan"
SolCallback(26) = "FlashChef"
SolCallback(27) = "FlashKenny"
SolCallback(28) = "FlashKyle"
SolCallback(29) = "FlashCartman"
SolCallback(30) = "FlashKennyAndBack"
SolCallback(31) = "FlashMrHankeyToilet"
SolCallback(32) = "FlashSuperVuk"


Sub FlashMrHankeyToilet(Enabled)
	If Enabled Then
	BulbCoverToiletteLeft.Visible = False
	BulbCoverToiletteLeftOn.Visible = True
	BulbCoverToiletteRight.Visible = False
	BulbcoverToiletteRightOn.Visible = True
	F7l.State = 1
	F7r.State = 1
	If FlasherF7Visible = True then FlasherF7.Visible = True
	If FlasherF7Visible = True then FlasherF7a.Visible = True
	Else
	BulbCoverToiletteLeft.Visible = True
	BulbcoverToiletteLeftOn.Visible = False
	BulbCoverToiletteRight.Visible = True
	BulbCoverToiletteRightOn.Visible = False
	F7l.State = 0
	F7r.State = 0
	FlasherF7.Visible = False
	FlasherF7a.Visible = False
	End if
End Sub

Sub FlashStan(Enabled)
	If Enabled Then
	F1.State = 1
	F1a.State = 1
	Else
	F1.State = 0
	F1a.State = 0
	End if
End Sub

Sub FlashChef(Enabled)
	If Enabled Then
	F2.State = 1
	F2a.State = 1
	Else
	F2.State = 0
	F2a.State = 0
	End if
End Sub

Sub FlashKenny(Enabled)
	If Enabled Then
	F3.State = 1
	F3a.State = 1
	Else
	F3.State = 0
	F3a.State = 0
	End if
End Sub

Sub FlashKyle(Enabled)
	If Enabled Then
	F4.State = 1
	F4a.State = 1
	Else
	F4.State = 0
	F4a.State = 0
	End if
End Sub

Sub FlashCartman(Enabled)
	If Enabled Then
	F5.State = 1
	F5a.State = 1
	Else
	F5.State = 0
	F5a.State = 0
	End if
End Sub

Sub FlashSuperVuk(Enabled)
	If Enabled Then
	F8.State = 1
	FlasherCapRed.Image = "dome3_redOn"
	FlasherF8.Visible = True
	if Ballcount > 0 And BallinToilet = 0 then GiBlinking
	Else
	F8.State = 0
	FlasherCapRed.Image = "dome3_red"
	FlasherF8.Visible = False
	If Ballcount > 0 And BallinToilet = 0 then GiOn
	End if
End Sub

Sub FlashPops(Enabled)
	If Enabled Then
	FPa.State = 1
	FPb.State = 1
	FPc.State = 1
	FPd.State = 1
	FlasherCapYellow.Image = "dome3_yellowOn"
	FlasherFPa.Visible = True
	Else
	FPa.State = 0
	FPb.State = 0
	FPc.State = 0
	FPd.State = 0
	FlasherCapYellow.Image = "dome3_yellow"
	FlasherFPa.Visible = False
	End if
End Sub

Sub FlashKennyandBack(Enabled)
	If Enabled Then
	F6b.State = 1
	F6a.Visible = False
	F6a1.Visible = True
	Flasherf6.Visible = True
	FlasherF6a.Visible = True
	Kenny.Visible = False
	KennyL.Visible = True
	Else
	F6b.State = 0
	F6a.Visible = True
	F6a1.Visible = False
	FlasherF6.Visible = False
	FlasherF6a.Visible = False
	Kenny.Visible = True
	KennyL.Visible = False
	End if
End Sub

Sub FlashTopVuk(Enabled)
	If Enabled Then
	F18.State = 1
	FlasherCapChef.Image = "dome3_redOn"
	FlasherF18.Visible = True
	FlasherF18a.Visible = True
	If BallCount > 0 And BallinToilet = 0 then GiBlinking
	Else
	F18.State = 0
	FlasherCapChef.Image = "dome3_red"
	FlasherF18.Visible = False
	FlasherF18a.Visible = False
	If BallCount > 0 And BallinToilet = 0 then GiOn
	End if
End Sub



	


' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

'*****************************************
'    tnob
'*****************************************

Const tnob = 9 ' total number of balls



'*******************
' WireRampDropSounds
'*******************

Dim SoundBall

Sub CartmanWireStart_Hit
	GiOn
End Sub

'Sub CartmanWireEnd_Hit
'	PlaySound "BallDrop"
'End Sub

'Sub Trigger3_Hit()
'	StopSound "plasticrolling"
'	PlaySound "Balldrop"
'End Sub

'Sub Trigger4_Hit()
'	StopSound "plasticrolling"
'	PlaySound "Balldrop"
'End Sub

'Sub Trigger5_Hit()
'	Playsound "Balldrop"
'End Sub

'Sub Trigger6_Hit()
'	Stopsound "plasticrolling"
'End Sub

'Sub RightRampColl_Hit()
'	Set SoundBall = ActiveBall
'	PlaySound "plasticrolling", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'End Sub

'Sub RightRampColl_UnHit()
'	StopSound "plasticrolling"
'End Sub

'Sub LeftRampColl_UnHit()
'	StopSound "plasticrolling"
'End Sub

'Sub LeftRampColl_Hit()
'	Set SoundBall = ActiveBall
'	PlaySound "plasticrolling", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'End Sub

'Sub StopsoundRamp_Hit()
'	StopSound "plasticrolling"
'End Sub
'
'Sub StopsoundRamp1_Hit()
'	StopSound "plasticrolling"
'End Sub


'****************************
'     Realtime Updates
' called by the MotorCallBack
'****************************

Sub RealTimeUpdates
    'flippers
    FlipperLeftP.RotY = LeftFlipper.CurrentAngle
    FlipperRightP.RotY = RightFlipper.CurrentAngle		
    ToiletSeat.RotX = ToiletFlipper.CurrentAngle
    Cistern.RotX = CisternFlipper.CurrentAngle
    MrHankey.TransY = MrHankeyFlipper.CurrentAngle
    ' rolling sound
    'Gates
	GateUpdate
End Sub




'******************************
' Hit Sounds
'******************************



Sub RandomSoundCartman()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "CartmanHole1", 0, 0.6, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 2:PlaySound "CartmanHole2", 0, 0.6, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 3:PlaySound "CartmanHole4", 0, 0.6, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End Select
End Sub

Sub RandomSoundKenny()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "KennyHole1", 0, 0.6, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 2:PlaySound "KennyHole2", 0, 0.6, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 3:PlaySound "KennyHole3", 0, 0.6, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End Select
End Sub

Sub RandomSoundKennyMove()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "KennyMove1", 0, 0.2, 0, 0, 0, 1, 0
        Case 2:PlaySound "KennyMove2", 0, 0.2, 0, 0, 0, 1, 0
        Case 3:PlaySound "KennyMove3", 0, 0.2, 0, 0, 0, 1, 0
    End Select
End Sub




'**************************************************
'        Flipper Collision Subs
'NOTE: COpy and overwrite collision sound from original collision subs over
'RandomSoundFlipper()' below
'**************************************************'

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	RightFlipperCollide parm
End Sub



'******************************************************
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

' Put all the Post and Pin objects in dPosts collection. Make sure dPosts fires hit events.
Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

' This collection contains the bottom sling posts. They are not in the dPosts collection so that the TargetBouncer is not applied to them, but they should still have dampening applied
' If you experience airballs with posts or targets, consider adding them to this collection
Sub NoTargetBouncer_Hit
    RubbersD.dampen ActiveBall
End Sub

' Put all the Sleeve objects in dSleeves collection. Make sure dSleeves fires hit events.
Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	TargetBouncer ActiveBall, 0.7
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports In debugger (In vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports In debugger (In vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		aBall.velz = aBall.velz * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class


'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9 	'Level of bounces. Recommmended value of 0.7-1.0

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.45*defvalue
            Case 6: zMultiplier = 0.5*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
        'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

'******************************************************
'                TRACK ALL BALL VELOCITIES
'                 FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
        public ballvel, ballvelx, ballvely

        Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

        Public Sub Update()        'tracks in-ball-velocity
                dim str, b, AllBalls, highestID : allBalls = getballs

                for each b in allballs
                        if b.id >= HighestID then highestID = b.id
                Next

                if uBound(ballvel) < highestID then redim ballvel(highestID)        'set bounds
                if uBound(ballvelx) < highestID then redim ballvelx(highestID)        'set bounds
                if uBound(ballvely) < highestID then redim ballvely(highestID)        'set bounds

                for each b in allballs
                        ballvel(b.id) = BallSpeed(b)
                        ballvelx(b.id) = b.velx
                        ballvely(b.id) = b.vely
                Next
        End Sub
End Class


'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.
' Create the following new collections:
'	 Metals (all metal objects, metal walls, metal posts, metal wire guides)
'	 Apron (the apron walls and plunger wall)
'	 Walls (all wood or plastic walls)
'	 Rollovers (wire rollover triggers, star triggers, or button triggers)
'	 Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
'	 Gates (plate gates)
'	 GatesWire (wire gates)
'	 Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate
' how to make these updates. But in summary the following needs to be updated:
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
'
' Tutorial videos by Apophis
' Audio : Adding Fleep Part 1					https://youtu.be/rG35JVHxtx4?si=zdN9W4cZWEyXbOz_
' Audio : Adding Fleep Part 2					https://youtu.be/dk110pWMxGo?si=2iGMImXXZ0SFKVCh
' Audio : Adding Fleep Part 3					https://youtu.be/ESXWGJZY_EI?si=6D20E2nUM-xAw7xy


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]
RollingSoundFactor = 1.1 / 5

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.95					  'volume level; range [0, 1]
BumperSoundFactor = 4.25						'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2		  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055 / 5			 'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075 / 5			   'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075 / 5			'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025		   'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025		   'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8	  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075				   'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075 / 3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10	 'volume multiplier; must not be zero
DTSoundLevel = 0.25				 'volume multiplier; must not be zero
RolloverSoundLevel = 0.25		   'volume level; range [0, 1]
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8				   'volume level; range [0, 1]
BallReleaseSoundLevel = 1			   'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2	'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015	 'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025 / 5			 'volume multiplier; must not be zero

'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioFade = CSng(tmp ^ 10)
	Else
		AudioFade = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioPan = CSng(tmp ^ 10)
	Else
		AudioPan = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = CSng(BallVel(ball) ^ 2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(BallVel(ball) ^ 3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^ 2 * 15
End Function

Function RndInt(min, max) ' Sets a random number integer between min and max
	RndInt = Int(Rnd() * (max - min + 1) + min)
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeLeftSoundLevel * VolumeDial, - 0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub

Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall2()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, kickback
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////

Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////

Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd * 11) + 1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd * 7) + 1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperLeft(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Left_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////

Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////

Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////

Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd * 7) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd * 8) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm / 10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm / 10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd * 7) + 1), parm * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd * 4) + 1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub Rubbers_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub

Sub Posts_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd * 10) + 1
		Case 1
			PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 2
			PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 3
			PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 4
			PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 5
			PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 6
			PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 7
			PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 8
			PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 9
			PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 10
			PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd * 9) + 1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()
End Sub

Sub RandomSoundWall()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 5) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 4) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 3) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd * 13) + 1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////

Sub RandomSoundBottomArchBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Bounce_" & Int(Rnd * 2) + 1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////

Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd * 3) + 1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(ActiveBall.id)) < 4 And cor.ballvely(ActiveBall.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd * 3) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd * 7) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft ActiveBall
	Else
		RandomSoundTargetHitWeak()
	End If
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd * 9) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd * 7) + 1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd * 5) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, ActiveBall
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, ActiveBall
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If ActiveBall.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If ActiveBall.velx <  - 8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If ActiveBall.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If ActiveBall.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, ActiveBall
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0
			PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1
			PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)

	FlipperCradleCollision ball1, ball2, velocity

	Dim snd
	Select Case Int(Rnd * 7) + 1
		Case 1
			snd = "Ball_Collide_1"
		Case 2
			snd = "Ball_Collide_2"
		Case 3
			snd = "Ball_Collide_3"
		Case 4
			snd = "Ball_Collide_4"
		Case 5
			snd = "Ball_Collide_5"
		Case 6
			snd = "Ball_Collide_6"
		Case 7
			snd = "Ball_Collide_7"
	End Select
	
	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER   ////////////////////////////

Const RelayFlashSoundLevel = 0.315  'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05	  'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025 * RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025 * RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025 * RelayFlashSoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025 * RelayFlashSoundLevel, obj
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////


'******************************************************
'****  END FLEEP MECHANICAL SOUNDS
'******************************************************

'**********************************
' 	ZMAT: General Math Functions
'**********************************
' These get used throughout the script.

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

Function ArcCos(x)
	If x = 1 Then
		ArcCos = 0/180*PI
	ElseIf x = -1 Then
		ArcCos = 180/180*PI
	Else
		ArcCos = Atn(-x/Sqr(-x * x + 1)) + 2 * Atn(1)
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

' Used for drop targets
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy) 'Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
	Dim AB, BC, CD, DA
	AB = (bx * py) - (by * px) - (ax * py) + (ay * px) + (ax * by) - (ay * bx)
	BC = (cx * py) - (cy * px) - (bx * py) + (by * px) + (bx * cy) - (by * cx)
	CD = (dx * py) - (dy * px) - (cx * py) + (cy * px) + (cx * dy) - (cy * dx)
	DA = (ax * py) - (ay * px) - (dx * py) + (dy * px) + (dx * ay) - (dy * ax)
	
	If (AB <= 0 And BC <= 0 And CD <= 0 And DA <= 0) Or (AB >= 0 And BC >= 0 And CD >= 0 And DA >= 0) Then
		InRect = True
	Else
		InRect = False
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
	Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
	Dim rotxy
	rotxy = RotPoint(ax,ay,angle)
	rax = rotxy(0) + px
	ray = rotxy(1) + py
	rotxy = RotPoint(bx,by,angle)
	rbx = rotxy(0) + px
	rby = rotxy(1) + py
	rotxy = RotPoint(cx,cy,angle)
	rcx = rotxy(0) + px
	rcy = rotxy(1) + py
	rotxy = RotPoint(dx,dy,angle)
	rdx = rotxy(0) + px
	rdy = rotxy(1) + py
	
	InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
	Dim rx, ry
	rx = x * dCos(angle) - y * dSin(angle)
	ry = x * dSin(angle) + y * dCos(angle)
	RotPoint = Array(rx,ry)
End Function


'*******************************************
'  ZOPT: User Options
'*******************************************

Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		' Level of ramp rolling volume. Value between 0 and 1

' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings
Dim dspTriggered : dspTriggered = False
Sub Table1_OptionEvent(ByVal eventId)
    If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If
	Flashing= Table1.Option("Flashing Lights", 0, 1, 1, 1, 0, Array("Reduced", "Normal"))
    ' Sound volumes
    VolumeDial = Table1.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
    BallRollVolume = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.5, 1)
    RampRollVolume = Table1.Option("Ramp Roll Volume", 0, 1, 0.01, 0.3, 1)


    If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub


'******************************************************
'	ZBRL: BALL ROLLING AND DROP SOUNDS
'******************************************************
'
' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b', BOT
	gBOT = GetBalls


	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob - 1
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(gBOT)
		If BallVel(gBOT(b)) > 1 AND gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
		If gBOT(b).VelZ < -1 and gBOT(b).z < 55 and gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard gBOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
	Next
End Sub



' *********************************************************************
' ninuzzu's	BALL SHADOW
' *********************************************************************

Dim BallShadow
BallShadow = Array(BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6,BallShadow7,BallShadow8,BallShadow9)

Sub BallShadowUpdate()
    Dim b
    Dim BOT
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(bot) <(tnob-1) Then
        For b = (UBound(bot) + 1)to(tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(bot) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(bot)
        If bot(b).X <Table1.Width / 2 Then
            BallShadow(b).X = ((bot(b).X)-(Ballsize / 6) + ((bot(b).X -(Table1.Width / 2)) / 7)) + 6
        Else
            BallShadow(b).X = ((bot(b).X) + (Ballsize / 6) + ((bot(b).X -(Table1.Width / 2)) / 7))- 6
        End If
        ballShadow(b).Y = bot(b).Y + 12
        If bot(b).Z> 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************

'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS: Set LS = New SlingshotCorrection
Dim RS: Set RS = New SlingshotCorrection
'Dim TS: Set TS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

	'TS.Object = TopSlingshot
	'TS.EndPoint1 = EndPoint1TS
	'TS.EndPoint2 = EndPoint2TS
	
	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script In-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

' The following sub are needed, however they exist in the ZMAT maths section of the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
'Function RotPoint(x,y,angle)
'	dim rx, ry
'	rx = x*dCos(angle) - y*dSin(angle)
'	ry = x*dSin(angle) + y*dCos(angle)
'	RotPoint = Array(rx,ry)
'End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			'   debug.print " "
		End If
	End Sub
End Class

Dim gBot
Sub GameTimer_Timer()
	'gBOT = GetBalls
	Cor.Update
	RollingUpdate
	BallShadowUpdate
End Sub

'The CorTimer interval should be 10. It's sole purpose is to update the Cor (physics) calculations
'CorTimer.Interval = 10
'Sub CorTimer_Timer(): Cor.Update: End Sub


'--- Add this near the top of your script ---
Function IIf(condition, truePart, falsePart)
    If condition Then
        IIf = truePart
    Else
        IIf = falsePart
    End If
End Function

'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(5,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(5)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
			RampBalls(0, 0) & vbnewline & _
			Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
			Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
			Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
			Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
			Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
			" "
		End If
	next
End Sub

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
Sub WRemoveBall(ID)		'Remove ball
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
	"1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
	"2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
	"3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
	"4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
	"5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
	"6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
	" "
End Sub


Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function


'Ramp triggers


'Ramp triggers

Sub RampStart1_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub RampStart2_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub RampStart3_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub RampStart4_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub RampStart5_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub WireStart1_hit
        WireRampOn False           ' true means plastic ramp here
End Sub

Sub WireStart2_hit
        WireRampOn False           ' true means plastic ramp here
End Sub

Sub WireStart3_hit
        WireRampOn False           ' true means plastic ramp here
End Sub

Sub WireStart4_hit
        WireRampOn False           ' true means plastic ramp here
End Sub

Sub WireStart5_hit
        WireRampOn False           ' true means plastic ramp here
End Sub

Sub Switch2Wire1_hit
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
        WireRampOn False          ' wire
End Sub

Sub Switch2Wire2_hit
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
        WireRampOn False          ' wire
End Sub

Sub Switch2Wire3_hit
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
        WireRampOn False          ' wire
End Sub

Sub Switch2Flat4_hit
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
        WireRampOn FTrue          ' wire
End Sub

Sub Switch2Flat5_hit
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
        WireRampOn True          ' wire
End Sub

Sub trLowerRampStart_Hit()
    If ActiveBall.VelY < 0 Then   ' ball moving away from player
        WireRampOn True           ' true means plastic ramp here
    Else
        WireRampOff               ' stop sound
    End If
End Sub

Sub trUpperRampWireEnd_Hit
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
End Sub

Sub trLowerrampend_Hit 'end of ramp
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
End Sub

Sub trUpperRampWireStart_Hit
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
        WireRampOn False          ' wire
End Sub


Sub RampTrigger1_Hit
	If activeball.vely < 0 Then
		WireRampOn True
	Else
		WireRampOff
	End If
End Sub

Sub RampTrigger2_Hit
	If activeball.vely < 0 Then
		WireRampOn True
	Else
		WireRampOff
	End If
End Sub

Sub RampTrigger3_Hit
    if abs(activeball.AngMomZ) > 70 then activeball.AngMomZ = 50
    activeball.AngMomZ = -abs(activeball.AngMomZ) * 3
    WireRampOff
End Sub

Sub RampTrigger4_Hit
    if abs(activeball.AngMomZ) > 70 then activeball.AngMomZ = 50
    activeball.AngMomZ = abs(activeball.AngMomZ) * 3
    WireRampOff
End Sub

Sub RampTrigger5_Hit
	If activeball.vely < 0 Then
		WireRampOn True
	Else
		WireRampOff
	End If
End Sub

Sub RampTrigger6_Hit
	WireRampOff
End Sub

Sub RandomSoundRampStop(obj)
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtVol "wireramp_stop1", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtVol "wireramp_stop2", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtVol "wireramp_stop3", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub




'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************

'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. and, special scripting
'
' TriggerLF and RF should now be 27 vp units from the flippers. In addition, 3 degrees should be added to the end angle
' when creating these triggers.
'
' RF.ReProcessBalls Activeball and LF.ReProcessBalls Activeball must be added the flipper_collide subs.
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.4            | 0.4                   | 0.375                  | 0.375              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity
'Dim ULF : Set ULF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'
'        x.AddPt "Polarity", 0, 0, 0
'        x.AddPt "Polarity", 1, 0.05, - 2.7
'        x.AddPt "Polarity", 2, 0.16, - 2.7
'        x.AddPt "Polarity", 3, 0.22, - 0
'        x.AddPt "Polarity", 4, 0.25, - 0
'        x.AddPt "Polarity", 5, 0.3, - 1
'        x.AddPt "Polarity", 6, 0.4, - 2
'        x.AddPt "Polarity", 7, 0.5, - 2.7
'        x.AddPt "Polarity", 8, 0.65, - 1.8
'        x.AddPt "Polarity", 9, 0.75, - 0.5
'        x.AddPt "Polarity", 10, 0.81, - 0.5
'        x.AddPt "Polarity", 11, 0.88, 0
'        x.AddPt "Polarity", 12, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
'
'
''*******************************************
'' Mid 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 3.7
'		x.AddPt "Polarity", 2, 0.16, - 3.7
'		x.AddPt "Polarity", 3, 0.22, - 0
'		x.AddPt "Polarity", 4, 0.25, - 0
'		x.AddPt "Polarity", 5, 0.3, - 2
'		x.AddPt "Polarity", 6, 0.4, - 3
'		x.AddPt "Polarity", 7, 0.5, - 3.7
'		x.AddPt "Polarity", 8, 0.65, - 2.3
'		x.AddPt "Polarity", 9, 0.75, - 1.5
'		x.AddPt "Polarity", 10, 0.81, - 1
'		x.AddPt "Polarity", 11, 0.88, 0
'		x.AddPt "Polarity", 12, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
''*******************************************
''  Late 80's early 90's
'
'Sub InitPolarity()
'	dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 5
'		x.AddPt "Polarity", 2, 0.16, - 5
'		x.AddPt "Polarity", 3, 0.22, - 0
'		x.AddPt "Polarity", 4, 0.25, - 0
'		x.AddPt "Polarity", 5, 0.3, - 2
'		x.AddPt "Polarity", 6, 0.4, - 3
'		x.AddPt "Polarity", 7, 0.5, - 4.0
'		x.AddPt "Polarity", 8, 0.7, - 3.5
'		x.AddPt "Polarity", 9, 0.75, - 3.0
'		x.AddPt "Polarity", 10, 0.8, - 2.5
'		x.AddPt "Polarity", 11, 0.85, - 2.0
'		x.AddPt "Polarity", 12, 0.9, - 1.5
'		x.AddPt "Polarity", 13, 0.95, - 1.0
'		x.AddPt "Polarity", 14, 1, - 0.5
'		x.AddPt "Polarity", 15, 1.1, 0
'		x.AddPt "Polarity", 16, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945

'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'	LF.SetObjects "LF", LeftFlipper, TriggerLF
'	RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub

'*******************************************
' Early 90's and after

Sub InitPolarity()
	Dim x, a
	a = Array(LF, RF)
'	a = Array(LF, RF, ULF)
	For Each x In a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, - 5.5
		x.AddPt "Polarity", 2, 0.16, - 5.5
		x.AddPt "Polarity", 3, 0.20, - 0.75
		x.AddPt "Polarity", 4, 0.25, - 1.25
		x.AddPt "Polarity", 5, 0.3, - 1.75
		x.AddPt "Polarity", 6, 0.4, - 3.5
		x.AddPt "Polarity", 7, 0.5, - 5.25
		x.AddPt "Polarity", 8, 0.7, - 4.0
		x.AddPt "Polarity", 9, 0.75, - 3.5
		x.AddPt "Polarity", 10, 0.8, - 3.0
		x.AddPt "Polarity", 11, 0.85, - 2.5
		x.AddPt "Polarity", 12, 0.9, - 2.0
		x.AddPt "Polarity", 13, 0.95, - 1.5
		x.AddPt "Polarity", 14, 1, - 1.0
		x.AddPt "Polarity", 15, 1.05, -0.5
		x.AddPt "Polarity", 16, 1.1, 0
		x.AddPt "Polarity", 17, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.23, 0.85
		x.AddPt "Velocity", 2, 0.27, 1
		x.AddPt "Velocity", 3, 0.3, 1
		x.AddPt "Velocity", 4, 0.35, 1
		x.AddPt "Velocity", 5, 0.6, 1 '0.982
		x.AddPt "Velocity", 6, 0.62, 1.0
		x.AddPt "Velocity", 7, 0.702, 0.968
		x.AddPt "Velocity", 8, 0.95,  0.968
		x.AddPt "Velocity", 9, 1.03,  0.945
		x.AddPt "Velocity", 10, 1.5,  0.945

	Next
	
	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
    'ULF.SetObjects "ULF", LeftFlipper1, TriggerLF1
End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before\
' modified 2024 by rothbauerw
' Added Reprocessballs for flipper collisions (LF.Reprocessballs Activeball and RF.Reprocessballs Activeball must be added to the flipper collide subs
' Improved handling to remove correction for backhand shots when the flipper is raised

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef, FlipStartAngle
	Private Balls(20), balldata(20)
	Private Name
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = new SpoofBall
		Next
	End Sub
	
	Public Sub SetObjects(aName, aFlipper, aTrigger)
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper
		FlipperStart = aFlipper.x
		FlipperEnd = Flipper.Length * Sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * Cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
		
		Dim str
		str = "Sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "Sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)
		
	End Sub
	
	' Legacy: just no op
	Public Property Let EndPoint(aInput)
		
	End Property
	
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
		Select Case aChooseArray
			Case "Polarity"
				ShuffleArrays PolarityIn, PolarityOut, 1
				PolarityIn(aIDX) = aX
				PolarityOut(aIDX) = aY
				ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
				ShuffleArrays VelocityIn, VelocityOut, 1
				VelocityIn(aIDX) = aX
				VelocityOut(aIDX) = aY
				ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
				ShuffleArrays YcoefIn, YcoefOut, 1
				YcoefIn(aIDX) = aX
				YcoefOut(aIDX) = aY
				ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub
	
	Public Sub Fire()
		Flipper.RotateToEnd
		processballs
	End Sub
	
	Public Property Get Pos 'returns % position a ball. For debug stuff.
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				balldata(x).Data = balls(x)
			End If
		Next
		FlipStartAngle = Flipper.currentangle
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

	Public Sub ReProcessBalls(aBall) 'save data of balls in flipper range
		If FlipperOn() Then
			Dim x
			For x = 0 To UBound(balls)
				If Not IsEmpty(balls(x)) Then
					if balls(x).ID = aBall.ID Then
						If isempty(balldata(x).ID) Then
							balldata(x).Data = balls(x)
						End If
					End If
				End If
			Next
		End If
	End Sub

	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef, BalltoFlip, BalltoBase, NoCorrection, checkHit
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY > -8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					BalltoFlip = DistanceFromFlipperAngle(BallData(x).x, BallData(x).y, Flipper, FlipStartAngle)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
				NoCorrection = 1
			Else
				checkHit = 50 + (20 * BallPos) 

				If BalltoFlip > checkHit or (PartialFlipCoef < 0.5 and BallPos > 0.22) Then
					NoCorrection = 1
				Else
					NoCorrection = 0
				End If
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				'If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled and NoCorrection = 0 Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef*VelCoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray)		'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount-1+offset)		'Resize original array
	For x = 0 To aCount-1				'set objects back into original array
		If IsObject(a(x)) Then
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = Sqr(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	'find active line
	Dim ii
	For ii = 1 To UBound(xKeyFrame)
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )		 'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )		'Clamp upper
	
	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS
'******************************************************
' To add the flipper tricks you must
'	 - Include a call to FlipperCradleCollision from within OnBallBallCollision subroutine
'	 - Include a call the CheckLiveCatch from the LeftFlipper_Collide and RightFlipper_Collide subroutines
'	 - Include FlipperActivate and FlipperDeactivate in the Flipper solenoid subs

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	' Flippers rest most of the time; skip the per-ball EOS-nudge work when neither
	' flipper is pressed, and fetch the ball list once for both FlipperNudge calls.
	If LFPress = 0 And RFPress = 0 Then
		Exit Sub
	End If
	Dim fBOT : fBOT = GetBalls
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle, fBOT
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle, fBOT
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2, BOT)
	Dim b

	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub

Dim FCCDamping: FCCDamping = 0.4

Sub FlipperCradleCollision(ball1, ball2, velocity)
	if velocity < 0.7 then exit sub		'filter out gentle collisions
    Dim DoDamping, coef
    DoDamping = false
    'Check left flipper
    If LeftFlipper.currentangle = LFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, LeftFlipper) OR FlipperTrigger(ball2.x, ball2.y, LeftFlipper) Then DoDamping = true
    End If
    'Check right flipper
    If RightFlipper.currentangle = RFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, RightFlipper) OR FlipperTrigger(ball2.x, ball2.y, RightFlipper) Then DoDamping = true
    End If
    If DoDamping Then
		coef = FCCDamping
        ball1.velx = ball1.velx * coef: ball1.vely = ball1.vely * coef: ball1.velz = ball1.velz * coef
        ball2.velx = ball2.velx * coef: ball2.vely = ball2.vely * coef: ball2.velz = ball2.velz * coef
    End If
End Sub
	


'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function DistanceFromFlipperAngle(ballx, bally, Flipper, Angle)
	DistanceFromFlipperAngle = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Angle + 90)) + Flipper.x, Sin(Radians(angle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function



'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, ULFPress, LFCount, RFCount
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
		SOSRampup = 2.5
	Case 1
		SOSRampup = 6
	Case 2
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
'   Const EOSReturn = 0.035  'mid 80's to early 90's
    Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress, FlipperObj)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity
	
	Flipper.eostorque = EOST
	Flipper.eostorqueangle = EOSA

	' Fire the bat immediately on keypress instead of waiting for the ROM
	' solenoid round-trip (removes the perceived flipper input lag).
	' FlipperObj.Fire does Flipper.RotateToEnd + polarity processballs atomically.
	' The ROM still calls SolLFlipper/SolRFlipper a moment later for sound and
	' game logic; that RotateToEnd is then a no-op (bat already raised).
	' Only predict-fire when the ROM actually has flippers powered (GameOn relay),
	' so disabled flippers (between balls, tilt, attract) don't flip. See SolGameOn.
	If FlippersEnabled Then
		If FlipperDelay <= 0 Then
			FlipperObj.Fire                       ' instant (optimized default)
		ElseIf Flipper Is LeftFlipper Then
			vpmTimer.AddTimer FlipperDelay, "FireLeftFlipperDelayed'"
		Else
			vpmTimer.AddTimer FlipperDelay, "FireRightFlipperDelayed'"
		End If
	End If
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
    Dim BOT, b

    FlipperPress = 0
    Flipper.eostorqueangle = EOSA
    Flipper.eostorque = EOST * EOSReturn / FReturn

    ' Drop the bat immediately on key release to match the instant raise above.
    ' The ROM's SolLFlipper/SolRFlipper(False) still fires shortly after for the
    ' flipper-down sound; its RotateToStart is then a no-op.
    Flipper.RotateToStart

    If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
        BOT = GetBalls   ' fresh list of valid balls on the table

        If IsArray(BOT) Then
            For b = 0 To UBound(BOT)
                If IsObject(BOT(b)) Then
                    ' check for cradle near this flipper
                    If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then
                        ' clamp downward speed a bit
                        If BOT(b).VelY >= -0.4 Then BOT(b).VelY = -0.4
                    End If
                End If
            Next
        End If
    End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir, startAng, absCur, absStart, absEnd
	startAng = Flipper.startangle
	absStart = Abs(startAng)
	absCur = Abs(Flipper.currentangle)
	absEnd = Abs(Flipper.endangle)
	Dir = Sgn(startAng) '-1 for Right Flipper

	If absCur > absStart - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf absCur <= absEnd And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf absCur > absEnd + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 5  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
Const BaseDampen = 0.55

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
    Dim Dir, LiveDist
    Dir = Flipper.startangle / Abs(Flipper.startangle)    '-1 for Right Flipper
    Dim LiveCatchBounce   'If live catch is not perfect, it won't freeze ball totally
    Dim CatchTime
    CatchTime = GameTime - FCount
    LiveDist = Abs(Flipper.x - ball.x)

    If CatchTime <= LiveCatch And parm > 3 And LiveDist > LiveDistanceMin And LiveDist < LiveDistanceMax Then
        If CatchTime <= LiveCatch * 0.5 Then   'Perfect catch only when catch time happens in the beginning of the window
            LiveCatchBounce = 0
        Else
            LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)  'Partial catch when catch happens a bit late
        End If
        
        If LiveCatchBounce = 0 And ball.velx * Dir > 0 And LiveDist > 30 Then ball.velx = 0

        If ball.velx * Dir > 0 And LiveDist < 30 Then
            ball.velx = BaseDampen * ball.velx
            ball.vely = BaseDampen * ball.vely
            ball.angmomx = BaseDampen * ball.angmomx
            ball.angmomy = BaseDampen * ball.angmomy
            ball.angmomz = BaseDampen * ball.angmomz
        Elseif LiveDist > 30 Then
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx = 0
            ball.angmomy = 0
            ball.angmomz = 0
        End If
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
    End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************


Const GATE_MIN_VELX = 1 
Const GATE_MIN_VELY = 1 
Const GATE_MIN_SPEED = 6 

Sub GateBackStop1_Hit()
    Dim vx, vy, spd
    vx  = ActiveBall.VelX
    vy  = ActiveBall.VelY
    spd = Sqr(vx * vx + vy * vy)

    If (vx > GATE_MIN_VELX Or vy > GATE_MIN_VELY) And spd > GATE_MIN_SPEED Then
        PlaySoundAtVol "Gate_FastTrigger_1", Ike, 0.6
    End If
End Sub


Dim ZColIndex  'rotates primitives to add variety to bounce angles

Sub InitZColRotate()
    ZColIndex = 0
    ZColRotateTimer.Interval = 3000
    ZColRotateTimer.Enabled = True
End Sub

Sub ZColRotateTimer_Timer()
    Dim obj, i

    i = 0

    For Each obj In zcol
        If i >= ZColIndex And i < ZColIndex + 8 Then
            obj.RotZ = obj.RotZ + 1
        End If
        i = i + 1
    Next

    ZColIndex = ZColIndex + 8
    If ZColIndex >= zcol.Count Then ZColIndex = 0
End Sub




















