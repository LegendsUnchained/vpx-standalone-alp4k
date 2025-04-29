'last saved with VP10.7 rev4406
'*******************************************************
'SEGA PINBALL INC
'STARSHIP TROOPERS 1997
'*******************************************************
'Design by:	Joe Balcer, Joe Kaminkow
'Art by:	Morgan Weistling
'Music by:	Brian Schmidt
'Sound by:	Brian Schmidt
'Software by:	Neil Falconer, Orin Day
'*******************************************************
'*******************************************************
'recreated for Visual Pinball by Knorr and Clark Kent
'*******************************************************
'*******************************************************
'Special Thanks to:
'JPSalas, Arngrim, Vinthar
'Toxie, Fuzzel and the VPX Dev Team
'*******************************************************
'*******************************************************
'V1.1 		fixed left kicker
'			added missing post
'			slowed down orbit
'*******************************************************
'V1.0		First Release
'*******************************************************

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

Const BallMass = 1.5
'Const BallMass = ((BallSize^3)/125000)
Const BallSize = 27
Const UseVPMModSol = 0	'doesnt work

Dim VarHidden, UseVPMColoredDMD
If Table1.ShowDT = true then
    UseVPMColoredDMD = true
    VarHidden = 1
else
    UseVPMColoredDMD = False
    VarHidden = 0
end if

LoadVPM "01560000", "sega.vbs", 3.57

Const UseSolenoids = 15
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0

'' special fastflips
'Const cSingleLFlip = 0
'Const cSingleRFlip = 0

' Standard Sounds
Const SSolenoidOn = "fx_solenoidon"
Const SSolenoidOff = "fx_solenoidoff"
Const SCoin = "fx_Coin"

'**************************
FlipperMassL = leftflipper.mass
FlipperMassR = rightflipper.mass
MassMultiply = 3
'**************************
EosSwitch = 90				'Angle for eos switch active
EosFlipperStrength = 900	'Power Reduction
FlipperStrength = Leftflipper.Strength
FLCoilRampUp = leftflipper.rampup
FRCoilRampUp = rightflipper.rampup
FRSCoilRampUp = rightflipperS.rampup
'***************************


'***ROM***ROM***ROM***ROM***

Const cGameName = "startrp2"
'***************************

Dim bsTrough, mBug, bsSuperVUK, bsLeftVUK, mLeftMagnet, mRightMagnet

Dim FlipperMassL, FlipperMassR, MassMultiply, EosSwitch, EosFlipperStrength, FlipperStrength, FLCoilRampup, FRCoilRampUp,FRSCoilRampUp
'
Set GICallback = GetRef("UpdateGI")


'************
' Table Init 
'************

Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "StarShip Troopers - Sega 1997" & vbNewLine & "VPX by Knorr and Clark Kent"
        .Games(cGameName).Settings.Value("rol") = 1 'set it to 1 to rotate the DMD to the left
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = VarHidden
        '.SetDisplayPosition 0, 0, GetPlayerHWnd 'uncomment this line if you don't see the DMD
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
'        .Switch(22) = 1 'close coin door
'        .Switch(24) = 1 'and keep it close
		' Main Timer init
		PinMAMETimer.Interval = PinMAMEInterval
		PinMAMETimer.Enabled = 1
		RealTime.Enabled = 1
		' Nudging
		vpmNudge.TiltSwitch = 56
		vpmNudge.Sensitivity = 2
		vpmNudge.TiltObj = Array(bumper1, bumper2, bumper3, slingshotleft, slingshotright)
	End With

    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 15, 14, 13, 12, 0, 0, 0
        .InitKick BallRelease, 80, 10
        .InitExitSnd SoundFX("fx_ballrelease", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
        .Balls = 4
        .IsTrough = 1
    End With


    Set bsSuperVUK = New cvpmBallStack
    With bsSuperVUK
        .InitSw 0, 46, 0, 0, 0, 0, 0, 0
        .InitKick sw46, 180, 42
        .InitExitSnd SoundFX("fx_supervuk", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
        .KickZ = 1.15
    End With


    Set bsLeftVUK = New cvpmSaucer
    With bsLeftVUK
        .InitKicker sw45, 45, 136, 30, 30
        .InitExitVariance 2, 1
        .InitSounds "fx_safehousehit", "fx_solenoid", SoundFX("fx_leftvuk",DOFContactors)
        .CreateEvents "bsLeftVUK", sw45
    End With


    Set mLeftMagnet = New cvpmMagnet
    With mLeftMagnet
        .InitMagnet LeftMagnet, 30 
        .GrabCenter = 1
		.Solenoid = 5
        .CreateEvents "mLeftMagnet"
    End With

    Set mRightMagnet = New cvpmMagnet
    With mRightMagnet
        .InitMagnet RightMagnet, 30
        .GrabCenter = 1
		.Solenoid = 6
        .CreateEvents "mRightMagnet"
    End With


    Set mBug = New cvpmMyMech
	With mBug
	' In VPX 10.7+ should use "vpmMechFourStepSol" which happens to be the same as setting both StepSol and TwoDirSol
'		.MType=vpmMechFourStepSol+vpmMechStopEnd+vpmMechLinear+vpmMechFast
		.MType=vpmMechStepSol+vpmMechTwoDirSol+vpmMechStopEnd+vpmMechLinear+vpmMechFast
		.Sol1=17
		.Length=600'150
		.Steps=120 'primitive trans -110 to +10
		.AddSw 34,0,1
		.AddSw 33,119,120
		.Callback=GetRef("UpdateWarriorBug")
		.Start
	End With

 	sw38.IsDropped=1
 	sw38a.IsDropped=1


	for each xx in BWalls:xx.isDropped = 1:Next



'**********************
'***DesktopMode Init***
'**********************

If Table1.ShowDT = False then
		KorpusFs_inside.visible = True
		f125.rotx = -22:f125.height = 230
		f126.rotx = -22:f126.height = 230
		f127.rotx = -22:f127.height = 230
		f128.rotx = -22:f128.height = 230
	Else
		KorpusFS_inside.visible = false
		f125.rotx = -7.45:f125.height = 160
		f126.rotx = -7.45:f126.height = 160
		f127.rotx = -7.45:f127.height = 160
		f128.rotx = -7.45:f128.height = 160
	End if


End Sub


Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit():Controller.Pause = False:Controller.Stop:End Sub



Sub TextboxTimer_Timer()
'	TextBox003.Text = WarriorBugAssembly004.transZ
'	TextBox003.Text = materialamount
'	TextBox001.Text = GIFL1.opacity
'	TextBox003.Text = plunger.position
'	TextBox003.Text = fadestepb

'	MassboxL.text = Leftflipper.Mass
'	MassboxR.text = Rightflipper.Mass
End Sub




'************************************
' Game timer for real time updates
'************************************

Sub RealTime_Timer
    RollingUpdate
    For each BrainBugStuff in BrainBugGroup: BrainBugStuff.TransY = BrainBugFlipper.CurrentAngle:next

    GateP_plungerlane.rotx = Gate_plungerlane.currentangle +25
    GateP_RightRamp.rotx = Gate_rightramp.currentangle +25
    sw25p.rotx = sw25spinner.currentangle
    sw52p.rotx = sw52spinner.currentangle
    sw53p.rotx = sw53spinner.currentangle


    LeftFlipperP.objrotz = LeftFlipper.CurrentAngle
    RightFlipperP.objrotz = RightFlipper.CurrentAngle
    RightFlipperPS.objrotz = RightFlipperS.CurrentAngle
    FlipperLShadow.RotZ = LeftFlipper.CurrentAngle
    FlipperRShadow.RotZ = RightFlipper.CurrentAngle
    FlipperRsShadow.RotZ = RightFlipperS.CurrentAngle

	PlungerP.TransZ = Plunger.position-((plunger.position-4)*3)

'	*****FlipperScript****
    If leftflipper.currentangle < EosSwitch then 
		leftflipper.strength = EosFlipperStrength
		leftflipper.RampUp = 0
	Else
		leftflipper.strength = FlipperStrength
		leftflipper.RampUp = FLCoilRampup
	End if


    If rightflipper.currentangle > -EosSwitch then 
		rightflipper.strength = EosFlipperStrength
		rightflipper.RampUp = 0
	Else
		rightflipper.strength = FlipperStrength
		rightflipper.RampUp = FRCoilRampUp
	End if

    If rightflipperS.currentangle > -EosSwitch then 
		rightflipperS.strength = EosFlipperStrength
		rightflipperS.RampUp = 0
	Else
		rightflipperS.strength = FlipperStrength
		rightflipperS.RampUp = FRSCoilRampUp
	End if

	if fadeb = 0 and fadestepB > 0 then fadestepB = fadestepB -fadespeeddown
	if fadeb = 1 And FadeStepB < 999 then fadestepB = fadestepB +fadespeedup
	MaterialStepB = FadestepB/1000
	UpdateMaterial "FlasherCapsLitBlue",0,1,1,1,materialstepB,materialstepB,materialstepB,RGB(255,255,255),0,0,False,True,0,0,0,0

	if fader = 0 and fadestepR > 0 then fadestepR = fadestepR -fadespeeddown
	if fader = 1 And FadeStepR < 999 then fadestepR = fadestepR +fadespeedup
	MaterialStepR = FadestepR/1000
	UpdateMaterial "FlasherCapsLitRed",0,1,1,1,materialstepR,materialstepR,materialstepR,RGB(255,255,255),0,0,False,True,0,0,0,0

	if fadey = 0 and fadestepY > 0 then fadestepY = fadestepY -fadespeeddown
	if fadey = 1 And FadeStepY < 999 then fadestepY = fadestepY +fadespeedup
	MaterialStepY = FadestepY/1000
	UpdateMaterial "FlasherCapsLitYellow",0,1,1,1,materialstepY,materialstepY,materialstepY,RGB(255,255,255),0,0,False,True,0,0,0,0

	if fadeG = 0 and fadestepG > 0 then fadestepG = fadestepG -fadespeeddown
	if fadeG = 1 And FadeStepG < 999 then fadestepG = fadestepG +fadespeedup
	MaterialStepG = FadestepG/1000
	UpdateMaterial "FlasherCapsLitGreen",0,1,1,1,materialstepG,materialstepG,materialstepG,RGB(255,255,255),0,0,False,True,0,0,0,0
End Sub



'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If KeyCode = MechanicalTilt Then vpmTimer.PulseSw vpmNudge.TiltSwitch: Exit Sub: End if
    If keycode = PlungerKey Then PlaysoundAt "fx_plungerpull", Plunger:Plunger.Pullback
	If vpmKeyDown(keycode) Then Exit Sub
    If keycode = LeftMagnaSave Then bLutActive = True
    If KeyCode = RightMagnaSave Then 
		Controller.Switch(88) = 1
		If bLutActive Then NextLUT:End If
	End if
End Sub


Sub table1_KeyUp(ByVal Keycode)
    If keycode = PlungerKey Then PlaySoundAT "fx_plunger", Plunger:Plunger.Fire
	If vpmKeyUp(keycode) Then Exit Sub
    If keycode = LeftMagnaSave Then bLutActive = False
    If KeyCode = RightMagnaSave Then Controller.Switch(88) = 0
End Sub


'*********
'   LUT
'*********

Dim bLutActive, LUTImage
Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "")Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

'Sub NextLUT:LUTImage = (LUTImage + 1)MOD 9:UpdateLUT:SaveLUT:End Sub
Sub NextLUT:LUTImage = (LUTImage + 1)MOD 5:UpdateLUT:SaveLUT:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0":FlashLut004.visible = 0:FlashLut000.visible = 1:FlashLut000.timerenabled = 1
        Case 1:table1.ColorGradeImage = "LUT1":FlashLut000.visible = 0:FlashLut001.visible = 1:FlashLut001.timerenabled = 1
        Case 2:table1.ColorGradeImage = "LUT2":FlashLut001.visible = 0:FlashLut002.visible = 1:FlashLut002.timerenabled = 1
        Case 3:table1.ColorGradeImage = "LUT3":FlashLut002.visible = 0:FlashLut003.visible = 1:FlashLut003.timerenabled = 1
        Case 4:table1.ColorGradeImage = "LUT4":FlashLut003.visible = 0:FlashLut004.visible = 1:FlashLut004.timerenabled = 1
    End Select
End Sub

Sub FlashLut000_Timer(): me.visible = 0:me.timerenabled = 0:End Sub
Sub FlashLut001_Timer(): me.visible = 0:me.timerenabled = 0:End Sub
Sub FlashLut002_Timer(): me.visible = 0:me.timerenabled = 0:End Sub
Sub FlashLut003_Timer(): me.visible = 0:me.timerenabled = 0:End Sub
Sub FlashLut004_Timer(): me.visible = 0:me.timerenabled = 0:End Sub


''*********
''Solenoids
''*********
'
SolCallback(1) = "SolRelease"
SolCallback(2) = "SolAutolaunch"
'SolCallback(3) = "bsLeftvuk.SolOut"
SolCallback(3) = "SolLeftVuk"
SolCallback(4) = "bsSupervuk.SolOut"

'SolCallback(5) = "mLeftMagnet.MagnetOn="
SolCallback(5) = "SolLeftMagnet"

'SolCallback(6) = "mRightMagnet.MagnetOn="
SolCallback(6) = "SolRightMagnet"

SolCallBack(7) = "SolBrainBug"
SolCallBack(8)= "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"



SolCallBack(23) = "SetLamp 123," 	'flash brainbug
SolCallBack(25) = "Sol25" 	'Flash Red
SolCallBack(26) = "Sol26"	'Flash Yellow
SolCallBack(27) = "Sol27"	'Flash Green
SolCallBack(28) = "Sol28"	'Flash Blue

'**************************************************
''for jp_pacdude routine - not used at the moment
'SolCallBack(25) = "SetLamp 125," 	'Flash Red
'SolCallBack(26) = "SetLamp 126,"	'Flash Yellow
'SolCallBack(27) = "SetLamp 127,"	'Flash Green
'SolCallBack(28) = "SetLamp 128,"	'Flash Blue
'**************************************************

SolCallBack(29) = "SetLamp 129," 'F5 WarBugSled Flasher X4
SolCallBack(30) = "SetLamp 130," 'F6 Left Ramp Flasher X4
SolCallBack(31) = "SetLamp 131," 'F7 Right Ramp Flasher X4
SolCallBack(32) = "SetLamp 132," 'F8 Pop Bumpers Flasher X2


Sub sol25(enabled): If enabled then FlashBlinkingRed:Else FlashoffRed:End if:End Sub
Sub sol26(enabled): If enabled then FlashBlinkingYellow:Else FlashoffYellow:End if:End Sub
Sub sol27(enabled): If enabled then FlashBlinkingGreen:Else FlashoffGreen:End if:End Sub
Sub sol28(enabled): If enabled then FlashBlinkingBlue:Else FlashoffBlue:End if:End Sub


'*************
'BALL RELEASE
'*************
Sub SolRelease(Enabled)
    If Enabled Then
        If bsTrough.Balls > 0 Then bsTrough.ExitSol_On
    End If
End Sub


Sub SolAutoLaunch(Enabled)
	If Enabled Then
		Plunger.Autoplunger = True
		Plunger.Fire
		PlaySound SoundFX("fx_AutoPlunger", DOFContactors)
		Else
		Plunger.Autoplunger = False
	End If
 End Sub


Sub Drain_Hit:bsTrough.AddBall Me:PlaysoundAt "fx_drain", drain:End Sub



'SUPERVUK
Sub sw46_Hit: bsSuperVUK.AddBall Me:End Sub

'Sub SolSuperVuk(Enabled)
'	if enabled and Controller.switch (46) then 
'		bssupervuk.ExitSol_on
'	End if
'End Sub



'leftVUK
dim KB

Sub SolLeftVuk(Enabled)
	if enabled then
		KB = True
		bsleftvuk.exitSol_on
		vuk_switch.RotX = 90
		sw45.TimerEnabled = True
	End if
End Sub

Sub sw45a_Hit(): Vuk_Switch.RotX = 80: End Sub

Sub sw45_Timer()
	if KB = True and vuk_kicker.TransZ < -25 then vuk_kicker.TransZ = vuk_kicker.TransZ +5
	if KB = False and vuk_kicker.TransZ > -50 then vuk_kicker.TransZ = vuk_kicker.TransZ -5
	if vuk_kicker.TransZ >= -25 then KB = False
	if KB = False And vuk_kicker.TransZ = -50 then me.TimerEnabled = False
End Sub

	





'**********
'BRAIN BUG
'**********

Dim BrainBugStuff

Sub SolBrainBug(Enabled)
	if enabled then
		BrainBug.Material = "Toys"
		BrainBugflipper.rotatetoend
		BrainBug.image = "brainbugup_on"
		sw38.isDropped = 0
		sw38a.isDropped = 0
		PlaySoundAT SoundFX("fx_trapdoorhigh",DOFContactors), sw46
		Controller.Switch(37) = True
		Controller.Switch(39) = True
		ShakeBrainBug
		Else
		BrainBugFlipper.rotatetostart
		BrainBug.image = "brainbugup_off"
		sw38.isDropped = 1
		sw38a.isDropped = 1
		PlaySoundAT SoundFX("fx_trapdoorlow",DOFContactors), sw46
		Controller.Switch(37) = False
		Controller.Switch(39) = False
		BrainBug.Material = "Toys_alpha"
	end if
End Sub


Sub sw38_Hit:ShakeBrainBug2:PlaySoundAtBall "fx_brainbughit":Me.TimerEnabled = 1:End Sub
Sub sw38_timer: vpmTimer.PulseSw 38:Me.TimerEnabled = 0:End Sub

'BRAINBUG_SHAKE

Dim BrainBugPos

Sub ShakeBrainBug
    BrainBugPos = 8
    ShakeBrainBugTimer.Enabled = 1
End Sub

Sub ShakeBrainBugTimer_Timer()
    For each BrainBugStuff in BrainBugGroup:BrainBugStuff.RotAndTra4 = BrainBugPos:Next
    If BrainBugPos = 0 Then ShakeBrainBugTimer.Enabled = False:Exit Sub
    If BrainBugPos < 0 Then
        BrainBugPos = ABS(BrainBugPos) - 1
    Else
        BrainBugPos = - BrainBugPos + 1
    End If
End Sub

Sub ShakeBrainBug2
    BrainBugPos = 2
    ShakeBrainBugTimer2.Enabled = 1
End Sub

Sub ShakeBrainBugTimer2_Timer()
    For each BrainBugStuff in BrainBugGroup:BrainBugStuff.RotAndTra2 = BrainBugPos:Next
    If BrainBugPos = 0 Then ShakeBrainBugTimer2.Enabled = False:Exit Sub
    If BrainBugPos < 0 Then
        BrainBugPos = ABS(BrainBugPos) - 1
    Else
        BrainBugPos = - BrainBugPos + 1
    End If
End Sub





'**********************
'Warrior Bug Animation
'**********************

Dim BugWalls, xx
BugWalls = Array (BWall1,BWall2,BWall3,BWall4,BWall5,BWall6,BWall7,BWall8,BWall9,BWall10,BWall11,BWall12,BWall13,BWall14,BWall15,BWall16,BWall17,BWall18,BWall19,BWall20,BWall21,BWall22,BWall23,BWall24)


Sub UpdateWarriorBug(NewPos, aSpeed, LastPos)
	dim Position, LastPosition, WarriorBugStuff
	For each WarriorBugStuff in WarriorBugGroup: WarriorBugStuff.TransZ = NewPos-111:next '-110 -1 difference to starting point

	Position = NewPos * 23 / 120
	LastPosition = LastPos * 23 / 120

	BugWalls(LastPosition).IsDropped=1
	BugWalls(LastPosition).collidable = 0
	BugWalls(Position).IsDropped=0
	BugWalls(Position).collidable = 1
End Sub


Sub BWalls_Hit(idx):vpmTimer.PulseSw 35:ShakeTarget:PlaySoundAtBall "fx_Switch":End Sub

Sub ShakeTarget
    TargetPos = 4	'8
    ShakeTargetTimer.Enabled = 1
End Sub

Dim TargetPos

Sub ShakeTargetTimer_Timer()
    WarriorBugAssembly003.RotAndTra4 = TargetPos
    If TargetPos = 0 Then ShakeTargetTimer.Enabled = False:Exit Sub
    If TargetPos < 0 Then
        TargetPos = ABS(TargetPos) - 1
    Else
        TargetPos = - TargetPos + 1
    End If
End Sub

Sub sw10_Hit:vpmTimer.PulseSw 10: vpmTimer.AddTimer 1800, "SuperVukAddBall'":Me.Enabled = 0:Me.TimerEnabled = 1: End Sub

Sub sw10_Timer: Me.DestroyBall:Me.TimerEnabled = 0:Me.Enabled = 1:End Sub

Sub SuperVukAddBall()
	bsSuperVuk.AddBall 1
End Sub





'********************************************
'DJRobX's Warrior Bug Stepper Supporting Code
'********************************************

Class cvpmMyMech
	Public Sol1, Sol2, MType, Length, Steps, Acc, Ret
	Private mMechNo, mNextSw, mSw(), mLastPos, mLastSpeed, mCallback

	Private Sub Class_Initialize
		ReDim mSw(10)
		gNextMechNo = gNextMechNo + 1 : mMechNo = gNextMechNo : mNextSw = 0 : mLastPos = 0 : mLastSpeed = 0
		MType = 0 : Length = 0 : Steps = 0 : Acc = 0 : Ret = 0 : vpmTimer.addResetObj Me
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
			.Mech(0) = mMechNo
		End With
		If IsObject(mCallback) Then mCallBack 0, 0, 0 : mLastPos = 0 : vpmTimer.EnableUpdate Me, True, True  ' <------- All for this.
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
	
End Class



Sub SolLeftMagnet(Enabled):if enabled then PlaySoundAt "fx_magnet", Leftmagnet:Else:StopSound "fx_magnet":End if:End Sub
Sub SolRightMagnet(Enabled):if enabled then PlaySoundAt "fx_magnet", Rightmagnet:Else:StopSound "fx_magnet":End if:End Sub


'**************
' Flipper Subs
'**************


SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(14) = "SolRFlipperS" ' mini flipper

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperupleft",DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown",DOFFlippers),LeftFlipper
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperupright",DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown",DOFFlippers),RightFlipper
        RightFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipperS(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperuprightS",DOFFlippers), RightFlipperS
        RightFlipperS.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdownS",DOFFlippers),RightFlipperS
        RightFlipperS.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
PlaySound "fx_rubber_flipper", 0, parm/60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub
'
Sub RightFlipper_Collide(parm)
PlaySound "fx_rubber_flipper", 0, parm/60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipperS_Collide(parm)
PlaySound "fx_rubber_flipper", 0, parm/60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub MassTriggerL_Hit(): LeftFlipper.Mass = FlipperMassL * MassMultiply:End Sub
Sub MassTriggerR_Hit(): RightFlipper.Mass = FlipperMassR * MassMultiply:End Sub
Sub MassTriggerRS_Hit(): RightFlipperS.Mass = FlipperMassR * MassMultiply:End Sub
Sub MassTriggerL_UnHit(): vpmtimer.addtimer 10, "LeftFlipper.Mass = FlipperMassL'":End Sub
Sub MassTriggerR_UnHit(): vpmtimer.addtimer 10, "RightFlipper.Mass = FlipperMassR'":End Sub
Sub MassTriggerRS_UnHit(): vpmtimer.addtimer 10, "RightFlipperS.Mass = FlipperMassR'":End Sub


'************
' SlingShots
'************


Dim RStep, Lstep

Sub SlingShotRight_Slingshot
    PlaySoundAT SoundFX("fx_slingshotright", DOFContactors), sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransY = -17
    RStep = 0
    SlingShotRight.TimerEnabled = 1
    vpmTimer.PulseSw 59
End Sub

Sub SlingShotRight_Timer
    Select Case RStep
        Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransY = -5
        Case 2:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransY = 0:SlingShotRight.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub SlingShotLeft_Slingshot
    PlaySoundAT SoundFX("fx_slingshotleft", DOFContactors), sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransY = -12
    LStep = 0
    SlingShotLeft.TimerEnabled = 1
    vpmTimer.PulseSw 62
End Sub

Sub SlingShotLeft_Timer
    Select Case LStep
        Case 1:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransY = -5
        Case 2:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransY = 0:SlingShotLeft.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub




'*********
' Switches
'*********
' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 49:PlaySoundAt SoundFX("fx_bumper1", DOFContactors), Bumper1:End Sub


Sub Bumper2_Hit:vpmTimer.PulseSw 51:PlaySoundAt SoundFX("fx_bumper2", DOFContactors), Bumper2:End Sub


Sub Bumper3_Hit:vpmTimer.PulseSw 50:PlaySoundAt SoundFX("fx_bumper3", DOFContactors), Bumper3:End Sub



'rollover
Sub sw16_Hit:Controller.Switch(16) = 1:sw16p.RotX = 70:PlaySoundAtBall "fx_switch":End Sub
Sub sw16_UnHit:Controller.Switch(16) = 0:sw16p.RotX = 90:End Sub

Sub sw41_Hit:Controller.Switch(41) = 1:sw41p.RotX = 70:PlaySoundAtBall "fx_switch":End Sub
Sub sw41_UnHit:Controller.Switch(41) = 0:sw41p.RotX = 90:End Sub

Sub sw42_Hit:Controller.Switch(42) = 1:sw42p.RotX = 70:PlaySoundAtBall "fx_switch":End Sub
Sub sw42_UnHit:Controller.Switch(42) = 0:sw42p.RotX = 90:End Sub

Sub sw43_Hit:Controller.Switch(43) = 1:sw43p.RotX = 70:PlaySoundAtBall "fx_switch":End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:sw43p.RotX = 90:End Sub

Sub sw57_Hit:Controller.Switch(57) = 1:sw57p.RotX = 70:PlaySoundAtBall "fx_switch":End Sub
Sub sw57_UnHit:Controller.Switch(57) = 0:sw57p.RotX = 90:End Sub

Sub sw58_Hit:Controller.Switch(58) = 1:sw58p.RotX = 70:PlaySoundAtBall "fx_switch":End Sub
Sub sw58_UnHit:Controller.Switch(58) = 0:sw58p.RotX = 90:End Sub

Sub sw61_Hit:Controller.Switch(61) = 1:sw61p.RotX = 70:PlaySoundAtBall "fx_switch":End Sub
Sub sw61_UnHit:Controller.Switch(61) = 0:sw61p.RotX = 90:End Sub

Sub sw60_Hit:Controller.Switch(60) = 1:sw60p.RotX = 70:PlaySoundAtBall "fx_switch":End Sub
Sub sw60_UnHit:Controller.Switch(60) = 0:sw60p.RotX = 90:End Sub

'orbit/optos
Sub sw47_Hit:Controller.Switch(47)= 1:End Sub
Sub sw47_UnHit:Controller.Switch(47) = 0:End Sub

Sub sw48_Hit:Controller.Switch(48)= 1:End Sub
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub


'undertrough

'skillshot
Sub sw9_Hit:Controller.Switch(9)= 1:RandomSoundHole:End Sub
Sub sw9_UnHit:Controller.Switch(9) = 0:End Sub
'bugexit
Sub sw10a_Hit:RandomSoundHole:End Sub
'bumperexit
Sub sw40_Hit:Controller.Switch(40)= 1:PlaySoundAtBall "fx_tedhit":End Sub
Sub sw40_UnHit:Controller.Switch(40) = 0:End Sub


'targets
Sub T17_Hit:vpmTimer.pulseSw 17: T17p.RotX = T17p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T17p:End Sub
Sub T17_Timer:T17p.RotX = T17p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T18_Hit:vpmTimer.pulseSw 18: T18p.RotX = T18p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T18p:End Sub
Sub T18_Timer:T18p.RotX = T18p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T19_Hit:vpmTimer.pulseSw 19: T19p.RotX = T19p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T19p:End Sub
Sub T19_Timer:T19p.RotX = T19p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T20_Hit:vpmTimer.pulseSw 20: T20p.RotX = T20p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T20p:End Sub
Sub T20_Timer:T20p.RotX = T20p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T21_Hit:vpmTimer.pulseSw 21: T21p.RotX = T21p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T21p:End Sub
Sub T21_Timer:T21p.RotX = T21p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T22_Hit:vpmTimer.pulseSw 22: T22p.RotX = T22p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T22p:End Sub
Sub T22_Timer:T22p.RotX = T22p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T23_Hit:vpmTimer.pulseSw 23: T23p.RotX = T23p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T23p:End Sub
Sub T23_Timer:T23p.RotX = T23p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T24_Hit:vpmTimer.pulseSw 24: T24p.RotX = T24p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T24p:End Sub
Sub T24_Timer:T24p.RotX = T24p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T27_Hit:vpmTimer.pulseSw 27: T27p.RotX = T27p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T27p:End Sub
Sub T27_Timer:T27p.RotX = T27p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T28_Hit:vpmTimer.pulseSw 28: T28p.RotX = T28p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T28p:End Sub
Sub T28_Timer:T28p.RotX = T28p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T29_Hit:vpmTimer.pulseSw 29: T29p.RotX = T29p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T29p:End Sub
Sub T29_Timer:T29p.RotX = T29p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T30_Hit:vpmTimer.pulseSw 30: T30p.RotX = T30p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T30p:End Sub
Sub T30_Timer:T30p.RotX = T30p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T31_Hit:vpmTimer.pulseSw 31: T31p.RotX = T31p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T31p:End Sub
Sub T31_Timer:T31p.RotX = T31p.RotX -4:Me.TimerEnabled = 0:End Sub

Sub T32_Hit:vpmTimer.pulseSw 32: T32p.RotX = T32p.RotX +4:Me.TimerEnabled = 1:PlaySoundAT SoundFX ("fx_switch",DOFTargets), T32p:End Sub
Sub T32_Timer:T32p.RotX = T32p.RotX -4:Me.TimerEnabled = 0:End Sub


Sub sw25_Hit:vpmTimer.PulseSW 25:PlaySoundAtBall "fx_gate":End Sub
Sub sw26_Hit:vpmTimer.PulseSW 26:PlaySoundAtBall "fx_switch":PlaySound "fx_wireramp0", 0, 0.7, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):End Sub
Sub sw52_Hit:vpmTimer.PulseSW 52:PlaySoundAtBall "fx_switch":End Sub
Sub sw53_Hit:vpmTimer.PulseSW 53:PlaySoundAtBall "fx_switch":End Sub

Sub BallCatcher001_Hit:If ActiveBall.VelY <= -11 then ActiveBall.VelY = -9:End if:End Sub
Sub BallCatcher002_Hit:If ActiveBall.VelY <= -11 then ActiveBall.VelY = -9:End if:End Sub



'***********
' Update GI
'***********
Dim MaterialAmount, x, StepM

Sub UpdateGi(no, enabled)
	If No = False Then
		Select Case no
			Case 0
				SetLamp 200, enabled
			end select
	End if

		Select Case no
			Case 0
			if enabled then
				for StepM  =  1 to 998 Step +1:next
				for each x in targetgroup: x.image = "targets_on":next
				for each x in imageswap: x.image = Replace(x.image,"_off","_on"):next
			else
				for StepM  =  998 to 1 Step -1:next
				for each x in targetgroup: x.image = "targets_off":next
				for each x in imageswap: x.image = Replace(x.image,"_on","_off"):next
			End if
	End Select
	MaterialAmount = stepm / 1000

	UpdateMaterial "plastics_textured" ,0,0,1,1,1,1,MaterialAmount,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "ramps" ,0,0,1,1,MaterialAmount,MaterialAmount,MaterialAmount,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "plastics_trans" ,0,1,1,1,MaterialAmount,MaterialAmount,MaterialAmount,RGB(255,255,255),0,0,False,True,0,0,0,0

'Wrap/Shininess/use_image/thickness/edge_brightness/edgeopacity/alpha/BaseColor/
End Sub



'*******************************************************************************************************************************************
'*******************************************************************************************************************************************
'Flasher_Strobing
'*******************************************************************************************************************************************
'The command is called from the solenoid with "FlashonRed" or "FlashblinkingRed" or "FlashOffRed" similar to the "states" of a single light
'Flashtimer 1+2 controls the blinking speed of the red flashers Lights
'Flashtimer 3+4 controls the blinking speed of the green flashers Lights
'Flashtimer 5+6 controls the blinking speed of the yellow flashers Lights
'Flashtimer 7+8 controls the blinking speed of the blue flashers Lights
'Fadespeedup and Fadespeeddown only controls the fading of the second primitive with the "lit" texture#
'FadeTimer updates the "lit" material of the flasherdomes
'*******************************************************************************************************************************************
'*******************************************************************************************************************************************

Dim bulb, fbulb, FlasherOnR, FlasherOffR, FlasherOnG, FlasherOffG, FlasherOnY, FlasherOffY, FlasherOnB, FlasherOffB
Dim FadeStepR, FadeStepG, FadeStepY, FadeStepB, materialstepR, MaterialstepG, MaterialStepY, MaterialStepB, fadespeedup, fadespeeddown
Dim FadeR, FadeG, FadeY, FadeB
FadestepR = 0
FadestepG = 0
FadestepY = 0
FadestepB = 0
Fadespeedup = 33
Fadespeeddown = 18		'high is fast, lower is slower



Sub FlashOffRed()
	FlasherOffR = True
	for each bulb in StrobeFlashersLRed
	bulb.state = 0
	for each fbulb in StrobeFlashersFRed
	fbulb.visible = False
	next
	next
End Sub

Sub FlashOffGreen()
	FlasherOffG = True
	for each bulb in StrobeFlashersLGreen
	bulb.state = 0
	for each fbulb in StrobeFlashersFGreen
	fbulb.visible = False
	next
	next
End Sub

Sub FlashOffYellow()
	FlasherOffY = True
	for each bulb in StrobeFlashersLYellow
	bulb.state = 0
	for each fbulb in StrobeFlashersFYellow
	fbulb.visible = False
	next
	next
End Sub

Sub FlashOffBlue()
	FlasherOffB = True
	for each bulb in StrobeFlashersLBlue
	bulb.state = 0
	for each fbulb in StrobeFlashersFBlue
	fbulb.visible = False
	next
	next
End Sub

'Sub FlashOn()
'	FlasherOn = True
'	for each bulb in StrobeFlashersL
'	bulb.state = 1
'	for each fbulb in StrobeFlashersF
'	fbulb.visible = True
'	next
'	next
'End Sub

Sub FlashBlinkingRed()
	FlasherOnR = False
	FlasherOffR = False
	FlasherTimer2.Enabled = True
End Sub

Sub FlashBlinkingGreen()
	FlasherOnG = False
	FlasherOffG = False
	FlasherTimer4.Enabled = True
End Sub


Sub FlashBlinkingYellow()
	FlasherOnY = False
	FlasherOffY = False
	FlasherTimer6.Enabled = True
End Sub


Sub FlashBlinkingBlue()
	FlasherOnB = False
	FlasherOffB = False
	FlasherTimer8.Enabled = True
End Sub



'FlasherStrobing off/on...

Sub FlasherTimer1_Timer()

	FadeR = 0
'	capsred.image = "CapsRed_On"
	for each fbulb in StrobeFlashersFRed
	fbulb.visible = 0
	for each bulb in StrobeFlashersLRed
	bulb.state = 0
	FlasherTimer1.Enabled = 0
	If FlasherOffR = False then FlasherTimer2.Enabled = 1
	next
	next
End Sub

Sub FlasherTimer2_Timer()

	FadeR = 1
'	CapsRed.image = "CapsRed_lit"
	for each fbulb in StrobeFlashersFRed
	fbulb.visible = 1
	for each bulb in StrobeFlashersLRed
	bulb.state = 1
	If FlasherOnR = False then 
	    FlasherTimer1.Enabled = 1
	    Else 
	    FlasherTimer2.Enabled = 0
	end if
	next
	next
End Sub




Sub FlasherTimer3_Timer()

	FadeG = 0
'	capsgreen.image = "CapsGreen_on"
	for each fbulb in StrobeFlashersFGreen
	fbulb.visible = 0
	for each bulb in StrobeFlashersLGreen
	bulb.state = 0
	FlasherTimer3.Enabled = 0
	If FlasherOffG = False then
	    FlasherTimer4.Enabled = 1
	end if
	next
	next
End Sub

Sub FlasherTimer4_Timer()

	FadeG = 1
'	capsgreen.image = "CapsGreen_Lit"
	for each fbulb in StrobeFlashersFGreen
	fbulb.visible = 1
	for each bulb in StrobeFlashersLGreen
	bulb.state = 1
	If FlasherOnG = False then 
	    FlasherTimer3.Enabled = 1
	Else 
	    FlasherTimer4.Enabled = 0
	end if
	next
	next
End Sub




Sub FlasherTimer5_Timer()

	FadeY = 0
'	CapsYellow.image = "CapsYellow_On"
	for each fbulb in StrobeFlashersFYellow
	fbulb.visible = 0
	for each bulb in StrobeFlashersLYellow
	bulb.state = 0
	FlasherTimer5.Enabled = 0
	If FlasherOffY = False then 
	    FlasherTimer6.Enabled = 1
	end if
	next
	next
End Sub

Sub FlasherTimer6_Timer()

	FadeY = 1
'	Capsyellow.image = "CapsYellow_Lit"
	for each fbulb in StrobeFlashersFYellow
	fbulb.visible = 1
	for each bulb in StrobeFlashersLYellow
	bulb.state = 1
	If FlasherOnY = False then 
	    FlasherTimer5.Enabled = 1
	Else 
	FlasherTimer6.Enabled = 0
    end if
	next
	next
End Sub



Sub FlasherTimer7_Timer()

	FadeB = 0
'	CapsBlue.image = "capsblue_on"
	for each fbulb in StrobeFlashersFBlue
	fbulb.visible = 0
	for each bulb in StrobeFlashersLBlue
	bulb.state = 0
	FlasherTimer7.Enabled = 0
	If FlasherOffB = False then 
	    FlasherTimer8.Enabled = 1
	end if
	next
	next
End Sub

Sub FlasherTimer8_Timer()

	FadeB = 1
'	CapsBlue.image = "capsblue_lit"
	for each fbulb in StrobeFlashersFBlue
	fbulb.visible = 1
	for each bulb in StrobeFlashersLBlue
	bulb.state = 1
	If FlasherOnB = False then 
	    FlasherTimer7.Enabled = 1
	Else 
	FlasherTimer8.Enabled = 0
	end if
	next
	next
End Sub


'**********************************************
'*******FadeTimer Moved to the Gametimer*******
'**********************************************

'Sub FadeTimer_Timer()
'	if fadeb = 0 and fadestepB > 0 then fadestepB = fadestepB -fadespeeddown
'	if fadeb = 1 And FadeStepB < 999 then fadestepB = fadestepB +fadespeedup
'	MaterialStepB = FadestepB/1000
'	UpdateMaterial "flashercapslitblue",0,1,1,1,materialstepB,materialstepB,materialstepB,RGB(255,255,255),0,0,False,True,0,0,0,0
'
'	if fader = 0 and fadestepR > 0 then fadestepR = fadestepR -fadespeeddown
'	if fader = 1 And FadeStepR < 999 then fadestepR = fadestepR +fadespeedup
'	MaterialStepR = FadestepR/1000
'	UpdateMaterial "flashercapslitred",0,1,1,1,materialstepR,materialstepR,materialstepR,RGB(255,255,255),0,0,False,True,0,0,0,0
'
'	if fadey = 0 and fadestepY > 0 then fadestepY = fadestepY -fadespeeddown
'	if fadey = 1 And FadeStepY < 999 then fadestepY = fadestepY +fadespeedup
'	MaterialStepY = FadestepY/1000
'	UpdateMaterial "flashercapslityellow",0,1,1,1,materialstepY,materialstepY,materialstepY,RGB(255,255,255),0,0,False,True,0,0,0,0
'
'	if fadeG = 0 and fadestepG > 0 then fadestepG = fadestepG -fadespeeddown
'	if fadeG = 1 And FadeStepG < 999 then fadestepG = fadestepG +fadespeedup
'	MaterialStepG = FadestepG/1000
'	UpdateMaterial "flashercapslitgreen",0,1,1,1,materialstepG,materialstepG,materialstepG,RGB(255,255,255),0,0,False,True,0,0,0,0
'End Sub




'**********************************************************
'     JP's Flasher Fading for VPX and Vpinmame
'       (Based on Pacdude's Fading Light System)
' This is a fast fading for the Flashers in vpinmame tables
'  just 4 steps, like in Pacdude's original script.
' Included the new Modulated flashers & Lights for WPC
'**********************************************************

Dim LampState(200), FadingState(200), FlashLevel(200)

InitLamps() ' turn off the lights and flashers and reset them to the default parameters

' vpinmame Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
			LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingState(chgLamp(ii, 0)) = chgLamp(ii, 1) + 3  'fading step
        Next
    End If
    UpdateLamps
End Sub

Sub UpdateLamps

    lampm 1, l1
    flash 1, lf1
    lampm 2, l2
    flash 2, lf2
    lampm 3, l3
    flash 3, lf3
    lampm 4, l4
    flash 4, lf4
    lampm 5, l5
    flash 5, lf5
    lampm 6, l6
    flash 6, lf6
    lampm 7, l7
    flash 7, lf7
    lampm 8, l8
    flash 8, lf8
    lampm 9, l9
    lampm 9, l9b
    flash 9, lf9
    lampm 10, l10
    lampm 10, l10b
    flash 10, lf10

	lampm 11, l11
	lampm 11, l11b
	flash 11, lf11
    lampm 12, l12
    lampm 12, l12b
    flash 12, lf12
    lampm 13, l13a
    lampm 13, l13b
    flash 13, lf13
    lampm 14, l14a
    lampm 14, l14b
    flash 14, lf14
    lampm 15, l15a
    lampm 15, l15b
    flash 15, lf15
    lampm 16, l16a
    lampm 16, l16b
    flash 16, lf16
    lampm 17, l17
    flash 17, lf17
    lampm 18, l18
    lampm 18, l18b
    flash 18, lf18
    lampm 19, l19
    flash 19, lf19
    lampm 20, l20
    lampm 20, l20b
    flash 20, lf20
    lampm 21, l21
    flash 21, lf21
    lampm 22, l22
    flash 22, lf22
    lampm 23, l23
    flash 23, lf23
    lampm 24, l24
    flash 24, lf24
    lampm 25, l25
    lampm 25, l25b
    flash 25, lf25
    lampm 26, l26
    lampm 26, l26b
    flash 26, lf26
    lampm 27, l27
    lampm 27, l27b
    flash 27, lf27

    lampm 28, l28
    lampm 28, l28b
    lamp 28, l28a
    lampm 29, l29
    lampm 29, l29b
    lamp 29, l29a
    lampm 30, l30
    lampm 30, l30b
    lamp 30, l30a

    lampm 31, l31
    flash 31, lf31
    lampm 32, l32
    lampm 32, l32b
    flash 32, lf32

    lampm 35, l35
    flashm 35, fl35
    flash 35, fl35a
    lampm 36, l36
    flashm 36, fl36
    flash 36, fl36a
    lampm 37, l37
    lampm 37, l37b
    flashm 37, lf37s
    flash 37, lf37
    lampm 38, l38
    lampm 38, l38b
    flashm 38, lf38s
    flash 38, lf38
    lampm 39, l39
    flashm 39, fl39
    flash 39, fl39a


    lampm 40, l40
    lampm 40, l40a
    flashm 40, lf40
    flash 40, lf40a

    lampm 41, l41
    lampm 41, l41b
    flash 41, lf41
    lampm 42, l42
    lampm 42, l42b
    flash 42, lf42
    lampm 43, l43
    flash 43, lf43
    lampm 44, l44
    lampm 44, l44b
    flash 44, lf44
    lampm 45, l45
    lampm 45, l45b
    flash 45, lf45
    lampm 46, l46
    lampm 46, l46b
    flash 46, lf46

    lampm 47, l47
    lampm 47, l47b
    flash 47, fl47
    lampm 48, l48
    lampm 48, l48b
    flash 48, fl48
    flash 49, l49
    flash 50, l50
    flash 51, l51
    flash 52, l52
    flash 53, l53
    flash 54, l54
    flash 55, l55

    flash 57, l57
    flash 58, l58
    flash 59, l59
    flash 60, l60
    flash 61, l61
    flash 62, l62
    flash 63, l63

    flash 65, l65
    flash 66, l66
    flash 67, l67
    flash 68, l68
    flash 69, l69
    flash 70, l70
    flash 71, l71

    flash 73, l73
    flash 74, l74
    flash 75, l75
    flash 76, l76
    flash 77, l77
    flash 78, l78
    flash 79, l79

    lampm 56, l56
    flash 56, lf56

    lampm 64, l64
    flash 64, lf64

    lampm 72, l72
    flash 72, lf72

    lampm 80, l80
    flash 80, lf80

'	BrainBug_Flash
'	flash 123, f123
	lampm 123, l123a
	lampm 123, l123b

'	WarriorBug_Flash
	lampm 129, l129
	lampm 129, l129a
	lampm 129, l129b
	flashm 129, f129s
	flashm 129, lf129
	flash 129, lf129b


'	LT_Ramp_Flash
	lampm 130, l130b
	lampm 130, l130c
	lampm 130, l130d
	lampm 130, l130e
	flashm 130, f130a
	flashm 130, f130b
	flashm 130, f130c
	flash 130, f130d

'	RT_Ramp_Flash
	lampm 131, l131b
	lampm 131, l131c
	lampm 131, l131d
	lampm 131, l131e
	flashm 131, f131a
	flashm 131, f131b
	flashm 131, f131c
	flash 131, f131d

	lampm 132, l132
	flash 132, lf132

'General_Illumination
	lampm 200, Light001
	lampm 200, Light002
	lampm 200, Light003
	lampm 200, Light004
	lampm 200, Light005
	lampm 200, Light006
	lampm 200, Light007
	lampm 200, Light008
	lampm 200, Light009
	lampm 200, Light010
	lampm 200, Light011
	lampm 200, Light012
	lampm 200, Light013
	lampm 200, Light014
	lampm 200, Light015
	lampm 200, Light016
	lampm 200, Light017
	lampm 200, Light018
	lampm 200, Light019
	lampm 200, Light020
	lampm 200, Light021
	lampm 200, Light022
	lampm 200, Light023
	lampm 200, Light024
	lampm 200, Light025
	lampm 200, Light026
	lampm 200, Light027
	lampm 200, Light028
	flashm 200, GIFL1
	flashm 200, lfgi001
	flashm 200, lfgi002
	flashm 200, lfgi003
	flash 200, lfgi004

End Sub

' div lamp subs

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
'    LampTimer.Interval = 25 ' flasher fading speed
    LampTimer.Interval = 10 ' flasher fading speed
    LampTimer.Enabled = 1
    For x = 0 to 200
        LampState(x) = 0
        FadingState(x) = 3 ' used to track the fading state
        FlashLevel(x) = 0
    Next
End Sub

Sub SetLamp(nr, value) ' 0 is off, 1 is on
    FadingState(nr) = abs(value) + 3
End Sub

' Lights: used for VPX standard lights, the fading is handled by VPX itself, they are here to be able to make them work together with the flashers

Sub Lamp(nr, object)
    Select Case FadingState(nr)
        Case 4:object.state = 1:FadingState(nr) = 0
        Case 3:object.state = 0:FadingState(nr) = 0
    End Select
End Sub

Sub Lampm(nr, object) ' used for multiple lights, it doesn't change the fading state
    Select Case FadingState(nr)
        Case 4:object.state = 1
        Case 3:object.state = 0
    End Select
End Sub

' Flashers: 4 is on,3,2,1 fade steps. 0 is off

Sub Flash(nr, object)
    Select Case FadingState(nr)
        Case 4:Object.IntensityScale = 1:FadingState(nr) = 0
        Case 3:Object.IntensityScale = 0.66:FadingState(nr) = 2
        Case 2:Object.IntensityScale = 0.33:FadingState(nr) = 1
        Case 1:Object.IntensityScale = 0:FadingState(nr) = 0
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change the fading state
    Select Case FadingState(nr)
        Case 4:Object.IntensityScale = 1
        Case 3:Object.IntensityScale = 0.66
        Case 2:Object.IntensityScale = 0.33
        Case 1:Object.IntensityScale = 0
    End Select
End Sub

' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub Reel(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1:FadingState(nr) = 0
        Case 3:object.SetValue 2:FadingState(nr) = 2
        Case 2:object.SetValue 3:FadingState(nr) = 1
        Case 1:object.SetValue 0:FadingState(nr) = 0
    End Select
End Sub

Sub Reelm(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1
        Case 3:object.SetValue 2
        Case 2:object.SetValue 3
        Case 1:object.SetValue 0
    End Select
End Sub

Sub NFadeReel(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1:FadingState(nr) = 1
        Case 3:object.SetValue 0:FadingState(nr) = 0
    End Select
End Sub

Sub NFadeReelm(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1
        Case 3:object.SetValue 0
    End Select
End Sub

'Texts

Sub Text(nr, object, message)
    Select Case FadingState(nr)
        Case 4:object.Text = message:FadingState(nr) = 0
        Case 3:object.Text = "":FadingState(nr) = 0
    End Select
End Sub

Sub Textm(nr, object, message)
    Select Case FadingState(nr)
        Case 4:object.Text = message
        Case 3:object.Text = ""
    End Select
End Sub

' Modulated Subs for the WPC tables

Sub SetModLamp(nr, level)
    FlashLevel(nr) = level / 150 'lights & flashers
End Sub

Sub LampMod(nr, object)          ' modulated lights used as flashers
    Object.IntensityScale = FlashLevel(nr)
    Object.State = 1             'in case it was off
End Sub

Sub FlashMod(nr, object)         'sets the flashlevel from the SolModCallback
    Object.IntensityScale = FlashLevel(nr)
End Sub

'Walls and mostly Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingState(nr)
        Case 4:object.image = a:FadingState(nr) = 0 'fading to off...
        Case 3:object.image = b:FadingState(nr) = 2
        Case 2:object.image = c:FadingState(nr) = 1
        Case 1:object.image = d:FadingState(nr) = 0
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingState(nr)
        Case 4:object.image = a
        Case 3:object.image = b
        Case 2:object.image = c
        Case 1:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingState(nr)
        Case 4:object.image = a:FadingState(nr) = 0 'off
        Case 3:object.image = b:FadingState(nr) = 0 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingState(nr)
        Case 4:object.image = a
        Case 3:object.image = b
    End Select
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v3.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 1500)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
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
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.06, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls, 20 balls, from 0 to 19
Const lob = 0     'number of locked balls
Const maxvel = 25 'max ball velocity
'Const maxvel = 47 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b))/4, 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 35 and BOT(b).z > 17 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz), Pan(BOT(b))/4, 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1)/4, 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*******************************
' Diverse Collection Hit Sounds
'*******************************

'RAMPS

Sub RampLFxstart_Hit(): RandomSoundRampL:End Sub

Sub RandomSoundRampL()
	Select Case Int(Rnd * 4) + 1
        Case 1:PlaySound "fx_rampL1", 0, Vol(ActiveBall)*10, Pan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
        Case 2:PlaySound "fx_rampL2", 0, Vol(ActiveBall)*10, Pan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
        Case 3:PlaySound "fx_rampL3", 0, Vol(ActiveBall)*10, Pan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
        Case 4:PlaySound "fx_rampL4", 0, Vol(ActiveBall)*10, Pan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
    End Select
End Sub


Sub RampRFxstart_Hit(): RandomSoundRampR:End Sub

Sub RandomSoundRampR()
	Select Case Int(Rnd * 4) + 1
        Case 1:PlaySound "fx_rampR1", 0, Vol(ActiveBall)*10, Pan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
        Case 2:PlaySound "fx_rampR2", 0, Vol(ActiveBall)*10, Pan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
        Case 3:PlaySound "fx_rampR3", 0, Vol(ActiveBall)*10, Pan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
        Case 4:PlaySound "fx_rampR4", 0, Vol(ActiveBall)*10, Pan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
    End Select
End Sub






Sub aRubber_Hit(idx):PlaySoundAtBall "fx_rubber":End Sub
Sub aPosts_Hit(idx):PlaySoundAtBall "fx_posts":End Sub



Sub aWoods_Hit(idx):PlaySoundAtBall "fx_WoodHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_gate":End Sub

Sub Wall018_Hit():PlaySound "fx_wireramp_stop", 0, 0.7, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):End Sub
Sub StopWireSoundTrigger_Hit():StopSound "fx_wireramp0":End Sub



'*************************************************************
Dim NextOrbitHit:NextOrbitHit = 0
'*************************************************************
'*************************************************************
Sub aMetals_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
	RandomSoundMetal
'	NextOrbitHit = Timer + .5 + (Rnd * .2)
	NextOrbitHit = Timer + .01 + (Rnd * .2)
	end if
End Sub

Sub RandomSoundMetal()
    Select Case Int(Rnd * 4) + 1
        Case 1:PlaySound "fx_metal1", 0, 1, Pan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
        Case 2:PlaySound "fx_metal2", 0, 1, Pan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
        Case 3:PlaySound "fx_metal3", 0, 1, Pan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
        Case 4:PlaySound "fx_metal4", 0, 1, Pan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
    End Select
End Sub



Sub sw46a_Hit:RandomSoundTv:End Sub

Sub RandomSoundTV()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "fx_tv_hit1", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 2:PlaySound "fx_tv_hit2", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 3:PlaySound "fx_tv_hit3", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End Select
End Sub


Sub aMetalWire_Hit(idx)
	if BallVel(ActiveBall) > .1 and Timer > NextOrbitHit then
	RandomSoundMetalWire
'	NextOrbitHit = Timer + .5 + (Rnd * .2)
	NextOrbitHit = Timer + .3 + (Rnd * .2)
	end if
End Sub


Sub RandomSoundMetalWire()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "fx_inlanewire1", 0, Vol(ActiveBall)*10, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 2:PlaySound "fx_inlanewire2", 0, Vol(ActiveBall)*10, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 3:PlaySound "fx_inlanewire3", 0, Vol(ActiveBall)*10, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End Select
End Sub


Sub RandomSoundHole()
    Select Case Int(Rnd * 6) + 1
        Case 1:PlaySound "fx_fallinramp1", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 2:PlaySound "fx_fallinramp2", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 3:PlaySound "fx_fallinramp3", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 4:PlaySound "fx_fallinramp4", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 5:PlaySound "fx_fallinramp5", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 6:PlaySound "fx_fallinramp6", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End Select
End Sub




Sub aApron_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
	RandomSoundApron
	NextOrbitHit = Timer + .5 + (Rnd * .2)
'	NextOrbitHit = Timer + .01 + (Rnd * .2)
	end if
End Sub


Sub RandomSoundApron()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "fx_apronHit1", 0, 0.5, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 2:PlaySound "fx_apronHit2", 0, 0.5, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 3:PlaySound "fx_apronHit3", 0, 0.5, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End Select
End Sub
