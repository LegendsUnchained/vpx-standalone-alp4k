' Transformers The Marcade MOD all art & design,toys,vfx anime by
' Mark Paulik 2021
' Original Table Design: JP's Transformers
' Based on Stern's Transformers (Pro) / IPD No. 5709 / 2011 / 4 Players
' Playfield & plastics redrawn by me.
' Some of the 3D figures from the Original FP table by ROM & Slamt1lt
' VPX - version by JPSalas 2018, version 1.1.0

Option Explicit
Randomize

Const BallSize = 50
Const BallMass = 1.7

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim bsTrough, bsSaucer, bsKickerMegaTron, plungerIM, cbRight, CarBall, x

Const cGameName = "tf_180"

Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 0

Dim VarHidden, UseVPMDMD
If Table1.ShowDT = true then
    UseVPMDMD = True
    VarHidden = 1
    For each x in aReels
        x.Visible = 1
    Next
else
    UseVPMDMD = False
    VarHidden = 0
    For each x in aReels
        x.Visible = 0
    Next
end if

' Use Modulated Flashers
Const UseVPMModSol = False

LoadVPM "01550000", "SAM.VBS", 3.26

' Standard Sounds
Const SSolenoidOn = "fx_solenoidon"
Const SSolenoidOff = "fx_solenoidoff"
Const SCoin = "fx_Coin"

'************
' Table init.
'************

Sub table1_Init
    vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Transformers Marcade Mod" & vbNewLine & "Art Mod by Mark Paulik & Original Table by JPSalas v.1.1"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = VarHidden
        .Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = -7
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 21, 20, 19, 18, 0, 0, 0
        .InitKick BallRelease, 90, 10
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 4
    End With

    'Kickers
    Set bsSaucer = New cvpmBallStack
    bsSaucer.InitSaucer sw3, 3, 170, 10
    bsSaucer.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsSaucer.KickAngleVar = 3

    set bsKickerMegaTron = New cvpmBallStack
    bsKickerMegaTron.InitSw 0, 41, 40, 39, 38, 0, 0, 0
    bsKickerMegaTron.InitKick KickerMegaTron, 165, 38
    bsKickerMegaTron.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsKickerMegaTron.KickAngleVar = 3
    bsKickerMegaTron.Balls = 0

    ' Impulse Plunger - used as the autoplunger
    Const IMPowerSetting = 42 'Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP sw23, IMPowerSetting, IMTime
        .Random 0.3
        .switch 23
        .InitExitSnd SoundFX("fx_plunger", DOFContactors), SoundFX("fx_plunger", DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Captive Ball
    Set cbRight = New cvpmCaptiveBall
    With cbRight
        .InitCaptive CapTrigger1, CapWall1, Array(CapKicker1, CapKicker1a), 0
        .NailedBalls = 1
        .ForceTrans = .99
        .MinForce = 3.5
        .CreateEvents "cbRight"
        .Start
    End With
    CapKicker1.CreateBall
    Set Carball = carkicker.CreateBall
    Carball.Visible = 0
    Carkicker.kick 180, 0

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    RealTime.Enabled = 1

	'Load LUT
	LoadLUT

'Fast Flips
	On Error Resume Next 
	InitVpmFFlipsSAM
	If Err Then MsgBox "You need the latest sam.vbs in order to run this table, available with vp10.5"
	On Error Goto 0
End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = LeftMagnaSave Then bLutActive = True
    If keycode = RightMagnaSave Then 
		If bLutActive Then NextLUT: End If
	End If
    If vpmKeyDown(keycode) Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_PlungerPull", Plunger:Plunger.Pullback
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = LeftMagnaSave Then bLutActive = False
    If vpmKeyUp(keycode) Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_plunger", Plunger:Plunger.Fire
End Sub

'*********
'   LUT
'*********

Dim bLutActive, LUTImage

Sub LoadLUT
	bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 9: UpdateLUT: SaveLUT: End Sub

Sub UpdateLUT
Select Case LutImage
Case 0: table1.ColorGradeImage = "LUT0"
Case 1: table1.ColorGradeImage = "LUT1"
Case 2: table1.ColorGradeImage = "LUT2"
Case 3: table1.ColorGradeImage = "LUT3"
Case 4: table1.ColorGradeImage = "LUT4"
Case 5: table1.ColorGradeImage = "LUT5"
Case 6: table1.ColorGradeImage = "LUT6"
Case 7: table1.ColorGradeImage = "LUT7"
Case 8: table1.ColorGradeImage = "LUT8"
End Select
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 26
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSling4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -20:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 27
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 30:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper1:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 31:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper2:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 32:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper3:End Sub

' Drain & Saucers
Sub Drain_Hit:PlaySoundAt "fx_drain", Drain:bsTrough.AddBall Me:End Sub
Sub sw3_Hit:PlaySoundAt "fx_kicker_enter", sw3:bsSaucer.AddBall 0:End Sub
Sub KickerMegaTron_Hit:PlaySoundAt "fx_kicker_enter", KickerMegaTron:bsKickerMegaTron.AddBall Me:End Sub

' Rollovers
Sub sw24_Hit:Controller.Switch(24) = 1:PlaySoundAt "fx_sensor", sw24:End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub

Sub sw25_Hit:Controller.Switch(25) = 1:PlaySoundAt "fx_sensor", sw25:End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub

Sub sw28_Hit:Controller.Switch(28) = 1:PlaySoundAt "fx_sensor", sw28:End Sub
Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub

Sub sw29_Hit:Controller.Switch(29) = 1:PlaySoundAt "fx_sensor", sw29:End Sub
Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub

Sub sw5_Hit:Controller.Switch(5) = 1:PlaySoundAt "fx_sensor", sw5:End Sub
Sub sw5_UnHit:Controller.Switch(5) = 0:End Sub

Sub sw13_Hit:Controller.Switch(13) = 1:PlaySoundAt "fx_sensor", sw13:End Sub
Sub sw13_UnHit:Controller.Switch(13) = 0:End Sub

Sub sw45_Hit:Controller.Switch(45) = 1:PlaySoundAt "fx_sensor", sw45:End Sub
Sub sw45_UnHit:Controller.Switch(45) = 0:End Sub

Sub sw12_Hit:Controller.Switch(12) = 1:PlaySoundAt "fx_sensor", sw12:End Sub
Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub

Sub sw11_Hit:Controller.Switch(11) = 1:PlaySoundAt "fx_sensor", sw11:End Sub
Sub sw11_UnHit:Controller.Switch(11) = 0:End Sub

Sub sw7_Hit:Controller.Switch(7) = 1:PlaySoundAt "fx_sensor", sw7:End Sub
Sub sw7_UnHit:Controller.Switch(7) = 0:End Sub

Sub sw8_Hit:Controller.Switch(8) = 1:PlaySoundAt "fx_sensor", sw8:End Sub
Sub sw8_UnHit:Controller.Switch(8) = 0:End Sub

Sub sw6_Hit:Controller.Switch(6) = 1:PlaySoundAt "fx_sensor", sw6:End Sub
Sub sw6_UnHit:Controller.Switch(6) = 0:End Sub

Sub sw10_Hit:Controller.Switch(10) = 1:End Sub
Sub sw10_UnHit:Controller.Switch(10) = 0:End Sub

Sub sw14_Hit:Controller.Switch(14) = 1:End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub

Sub sw4_Hit:Controller.Switch(4) = 1:End Sub
Sub sw4_UnHit:Controller.Switch(4) = 0:End Sub

Sub sw35_Hit:Controller.Switch(35) = 1:End Sub
Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub

Sub sw22_Hit:Controller.Switch(4) = 1:End Sub
Sub sw22_UnHit:Controller.Switch(4) = 0:End Sub

' Spinners
Sub sw34_Spin:vpmTimer.PulseSw 34:PlaySoundAt "tf_laserLoop", sw34:End Sub

'Targets
Sub sw37_Hit:vpmTimer.PulseSw 37:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw50_Hit:vpmTimer.PulseSw 50:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw1_Hit:vpmTimer.PulseSw 1:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw51_Hit:vpmTimer.PulseSw 51:PrimeShake:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw2_Hit:vpmTimer.PulseSw 2:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw49_Hit:vpmTimer.PulseSw 49:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw46_Hit:vpmTimer.PulseSw 46:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub

'*********
'Solenoids
'*********

SolCallback(1) = "bsTrough.SolOut"
SolCallback(2) = "Auto_Plunger"
SolCallback(3) = "bsKickerMegaTron.SolOut"
SolCallback(4) = "vpmSolGate LGate,false,"
SolCallback(5) = "vpmSolGate RGate,false,"
'SolCallback(8)  = "" ' PlayfieldShaker
'SolCallback(12)  = "" ' Optimus Prime shake
SolCallback(22) = "bsSaucer.SolOut"
SolCallback(30) = "OptimusPrimeMotor"

'Flashers
If UseVPMModSol Then
SolModCallback(17) = "SetModLamp 117,"
SolModCallback(18) = "SetModLamp 118,"
SolModCallback(19) = "SetModLamp 119,"
SolModCallback(20) = "SetModLamp 120,"
SolModCallback(21) = "SetModLamp 121,"
SolModCallback(23) = "SetModLamp 123,"
SolModCallback(25) = "SetModLamp 125,"
SolModCallback(26) = "SetModLamp 126,"
SolModCallback(27) = "SetModLamp 127,"
SolModCallback(28) = "SetModLamp 128,"
SolModCallback(29) = "SetModLamp 129,"
SolModCallback(31) = "SetModLamp 131,"
SolModCallback(32) = "SetModLamp 132,"
Else
SolCallback(17) = "SetLamp 117,"
SolCallback(18) = "SetLamp 118,"
SolCallback(19) = "SetLamp 119,"
SolCallback(20) = "SetLamp 120,"
SolCallback(21) = "SetLamp 121,"
SolCallback(23) = "SetLamp 123,"
SolCallback(25) = "SetLamp 125,"
SolCallback(26) = "SetLamp 126,"
SolCallback(27) = "SetLamp 127,"
SolCallback(28) = "SetLamp 128,"
SolCallback(29) = "SetLamp 129,"
SolCallback(31) = "SetLamp 131,"
SolCallback(32) = "SetLamp 132,"
End If

Sub Auto_Plunger(Enabled)
    If Enabled Then
        PlungerIM.AutoFire
    End If
End Sub

Dim RampPos, RampDir
RampPos = 0:RampDir = 1
opr1.TimerEnabled = 1 ' move down the ramp

Sub OptimusPrimeMotor(enabled)
    If Enabled Then
        opr1.TimerEnabled = 1
    End If
End Sub

Sub opr1_Timer 'ramp animation
    RampPos = RampPos + RampDir
    opr.Rotx = RampPos
    If RampPos = 0 Then 'ramp is up
        opr1.Isdropped = 0
        opr2.collidable = 1
        Controller.switch(43) = 1
        Controller.switch(44) = 0
        RampDir = 1
        opr1.TimerEnabled = 0
    End If
    If RampPos = 39 Then 'ramp is down
        opr1.Isdropped = 1
        opr2.collidable = 0
        Controller.switch(43) = 0
        Controller.switch(44) = 1
        RampDir = -1
        opr1.TimerEnabled = 0
    End If
End Sub

'*****************************
' Trigger VFX Anime
'*****************************
Dim HPos, HPosEnd

VFXinit
Sub VFXinit
	VFX1Sequence 1, 15
	VFX2Sequence 1, 10
	VFX3Sequence 1, 24
	VFX4Sequence 1, 24
	VFX5Sequence 1, 15
	VFX6Sequence 1, 10
	VFX7Sequence 1, 26
	VFX8Sequence 1, 12
	VFX2Sequence 1, 10
End Sub

' LOGO *****************************



 Sub LOGOtimer_timer() 
    vfx_logo.imageA = "logo"& Hpos
    vfx_logo.imageB = "logo"& Hpos
	If Hpos < HposEnd Then
     HPos = HPos + 1
	Else
		LOGOtimer.enabled = 0
		vfx_logo.Visible = 0
	End If
End Sub 

Sub VFX1Sequence(FrameStart, FrameEnd)
    HPos = FrameStart         
    HPosEnd = FrameEnd
	LOGOtimer.enabled = 1
	vfx_logo.Visible = 1
End Sub

Sub vfx_trigger01_Hit()
		If UBound(GetBalls) = 4 Then
				VFX1Sequence 1, 15
				Playsound "TF_scene"
		End If
End Sub

' OPTIMUS HIT *****************************
 Sub OPTIMUStimer_timer() 
		vfx_explosionA.imageA = "explosionA"& Hpos
		If Hpos < HposEnd Then
				HPos = HPos + 1
	Else
				OPTIMUStimer.enabled = 0
				vfx_explosionA.Visible = 0
	End If
End Sub 

Sub VFX2Sequence(FrameStart, FrameEnd)
    HPos = FrameStart         
    HPosEnd = FrameEnd
	OPTIMUStimer.enabled = 1
	vfx_explosionA.Visible = 1
End Sub

Sub vfx_trigger02_Hit()
		If UBound(GetBalls) = 4 Then
				VFX2Sequence 1, 10
				Playsound "TF_laserHit01"
		End If
End Sub

' ELECTRIC A *****************************
 Sub ELECTRICAtimer_timer() 
		vfx_electricA.imageA = "electricA"& Hpos
		If Hpos < HposEnd Then
				HPos = HPos + 1
		Else
				ELECTRICAtimer.enabled = 0
				vfx_electricA.Visible = 0
		End If
End Sub 

Sub VFX3Sequence(FrameStart, FrameEnd)
		HPos = FrameStart         
		HPosEnd = FrameEnd
		ELECTRICAtimer.enabled = 1
		vfx_electricA.Visible = 1
End Sub

Sub vfx_trigger03_Hit()
		VFX3Sequence 1, 24
		Playsound "TF_electricB"
End Sub

' ELECTRIC B *****************************
 Sub ELECTRICBtimer_timer() 
		vfx_electricB.imageA = "electricA"& Hpos
		If Hpos < HposEnd Then
				HPos = HPos + 1
		Else
				ELECTRICBtimer.enabled = 0
				vfx_electricB.Visible = 0
		End If
End Sub 

Sub VFX4Sequence(FrameStart, FrameEnd)
		HPos = FrameStart         
		HPosEnd = FrameEnd
		ELECTRICBtimer.enabled = 1
		vfx_electricB.Visible = 1
End Sub

Sub vfx_trigger03_Hit()
    VFX4Sequence 1, 24
End Sub

' NRG FLOWER *****************************
 Sub NRGtimer_timer() 
		vfx_nrg.imageA = "energyflower"& Hpos
		If Hpos < HposEnd Then
				HPos = HPos + 1
		Else
				NRGtimer.enabled = 0
				vfx_nrg.Visible = 0
		End If
End Sub 

Sub VFX5Sequence(FrameStart, FrameEnd)
    HPos = FrameStart         
    HPosEnd = FrameEnd
	NRGtimer.enabled = 1
	vfx_nrg.Visible = 1
End Sub

Sub vfx_trigger05_Hit()
		If UBound(GetBalls) = 4 Then
				VFX5Sequence 1, 15
				Playsound "TF_nrgFlower"
		End If
End Sub

' IRONHIDE HIT *****************************
 Sub IRONtimer_timer() 
		vfx_explosionB.imageA = "explosionB"& Hpos
		If Hpos < HposEnd Then
				HPos = HPos + 1
		Else
				IRONtimer.enabled = 0
				vfx_explosionB.Visible = 0
		End If
End Sub 

Sub VFX6Sequence(FrameStart, FrameEnd)
		HPos = FrameStart         
		HPosEnd = FrameEnd
		IRONtimer.enabled = 1
		vfx_explosionB.Visible = 1
End Sub

Sub vfx_trigger06_Hit()
		If UBound(GetBalls) = 4 Then
				VFX6Sequence 1, 10
				Playsound "TF_laserHit02"
		End If
End Sub

' SOUNDWAVE ENERGON ************************
 Sub ENERGONtimer_timer() 
		vfx_energon.imageA = "energon"& Hpos
		If Hpos < HposEnd Then
				HPos = HPos + 1
		Else
				ENERGONtimer.enabled = 0
				vfx_energon.Visible = 0
		End If
End Sub 

Sub VFX7Sequence(FrameStart, FrameEnd)
		HPos = FrameStart         
		HPosEnd = FrameEnd
		ENERGONtimer.enabled = 1
		vfx_energon.Visible = 1
End Sub

Sub vfx_trigger07_Hit()
		If UBound(GetBalls) = 4 Then
		VFX7Sequence 1, 26
		Playsound "TF_energon"
		End If
End Sub

' STARSCREAM HIT ************************
 Sub STARSCREAMtimer_timer() 
		vfx_explosionC.imageA = "explosionC"& Hpos
		If Hpos < HposEnd Then
				HPos = HPos + 1
		Else
				STARSCREAMtimer.enabled = 0
				vfx_explosionC.Visible = 0
		End If
End Sub 

Sub VFX8Sequence(FrameStart, FrameEnd)
		HPos = FrameStart         
		HPosEnd = FrameEnd
		STARSCREAMtimer.enabled = 1
		vfx_explosionC.Visible = 1
End Sub

Sub vfx_trigger08_Hit()
		If UBound(GetBalls) = 4 Then
				VFX8Sequence 1, 12
				Playsound "TF_laserHit02"
		End If
End Sub

' JAZZ ************************
Sub vfx_trigger09_Hit()
				Playsound "TF_jazz2"
End Sub

' GENERATOR ************************
Dim GeneratorPos: GeneratorPos = 1
Sub SolGeneratorMotor(Enabled)
				GENERATORtimer.Enabled = Enabled
End Sub

Sub GENERATORTimer_Timer()
			GeneratorPos = GeneratorPos + 1
			If GeneratorPos = 15 then GeneratorPos = 1
				vfx_generator.ImageA = "generator" & GeneratorPos
				vfx_generator.ImageB = "generator" & GeneratorPos
End Sub

'*****************************
' Optimus Prime hit Animation
'*****************************

Dim ccBall

PrimeInit

Sub PrimeInit
    Set ccBall = hball.createball
    hball.Kick 0, 0
    ccball.Mass = 1.6
End Sub

Sub PrimeShake
    ccball.vely = 10
    sw51.TimerEnabled = True
    vpmtimer.AddTimer 4000, "Stopsw51Timer '"
End Sub

Sub sw51_Timer 'start animation
    Dim y
    y = hball.y - ccball.y
'    OptimusPrime.Rotx = y
End Sub

Sub Stopsw51Timer 'stop animation
'    OptimusPrime.Rotx = 0
    sw51.TimerEnabled = False
End Sub

'*******************
' Flipper Subs v3.0
'*******************

SolCallback(16) = "SolRFlipper"
SolCallback(15) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup",DOFContactors), LeftFlipper
        LeftFlipper.EOSTorque = 0.75:LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown",DOFContactors),LeftFlipper
        LeftFlipper.EOSTorque = 0.1:LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup",DOFContactors), RightFlipper
        RightFlipper.EOSTorque = 0.75:RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown",DOFContactors),RightFlipper
        RightFlipper.EOSTorque = 0.1:RightFlipper.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'**********************************************************
'     JP's Flasher Fading for VPX and Vpinmame v3.0
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
            LampState(chgLamp(ii, 0)) = chgLamp(ii, 1)       'keep the real state in an array
            FadingState(chgLamp(ii, 0)) = chgLamp(ii, 1) + 3 'fading step
        Next
    End If
    ' playfield lights
    'Lamp 1, li1
    'Lamp 2, li2
    Lamp 3, li3
    Lamp 4, li4
    Lamp 5, li5
    Lamp 6, li6
    Lamp 7, li7
    Lamp 8, li8
    Lamp 9, li9
    Lamp 10, li10
    Lamp 11, li11
    Lamp 12, li12
    Lamp 13, li13
    Lamp 14, li14
    Lamp 15, li15
    Lamp 16, li16
    Lamp 17, li17
    Lamp 18, li18
    Lamp 19, li19
    Lamp 20, li20
    Lamp 21, li21
    Lamp 22, li22
    Lamp 23, li23
    Lamp 24, li24
    Lamp 25, li25
    Lamp 26, li26
    Lamp 27, li27
    Lamp 28, li28
    Lamp 29, li29
    Lamp 30, li30
    Lamp 31, li31
    Lamp 32, li32
    Lamp 33, li33
    Lamp 34, li34
    Lamp 35, li35
    Lamp 36, li36
    Lamp 37, li37
    Lamp 38, li38
    Lamp 39, li39
    Lamp 40, li40
    Lamp 41, li41
    Lamp 42, li42
    Lamp 43, li43
    Lamp 44, li44
    Lamp 45, li45
    Lamp 46, li46
    Lamp 47, li47
    Lamp 48, li48
    Lamp 49, li49
    Lamp 50, li50
    Lamp 51, li51
    Lamp 52, li52
    Flash 53, li53
    Lamp 54, li54
    Flash 55, li55
    'Lamp 56, li56
    Flash 57, li57
    Flash 58, li58
    Lamp 59, li59
    Flash 60, li60
    Flash 61, li61
    Flash 62, li62
    'Lamp 63, li63
    'Flashers
If UseVPMModSol Then
    LampMod 117, f17
    FlashMod 117, f17a
    FlashMod 118, f18a
    FlashMod 118, f18
    LampMod 119, f19
    FlashMod 119, f19a
    FlashMod 119, f19b
    LampMod 120, f20
    FlashMod 120, f20a
    FlashMod 121, f21
    LampMod 123, f23b
    FlashMod 123, f23a
    FlashMod 123, f23c
    FlashMod 123, f23
    FlashMod 125, f25
    LampMod 126, f26a
    FlashMod 126, f26
    LampMod 127, f27a
    FlashMod 127, f27
    LampMod 128, f28a
    FlashMod 128, f28
    LampMod 131, f31a
    FlashMod 131, f31
    FlashMod 132, f32
Else
    LampM 117, f17
    Flash 117, f17a
    FlashM 118, f18a
    Flash 118, f18
    LampM 119, f19
    FlashM 119, f19a
    Flash 119, f19b
    LampM 120, f20
    Flash 120, f20a
    Flash 121, f21
    LampM 123, f23b
    FlashM 123, f23a
    FlashM 123, f23c
    Flash 123, f23
    Flash 125, f25
    LampM 126, f26a
    Flash 126, f26
    LampM 127, f27a
    Flash 127, f27
    LampM 128, f28a
    Flash 128, f28
    LampM 131, f31a
    Flash 131, f31
    Flash 132, f32
End If
End Sub

' div lamp subs

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
    LampTimer.Interval = 25 ' flasher fading speed
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

'************************************
' Diverse Collection Hit Sounds v3.0
'************************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_MetalWire":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_LongBands_Hit(idx):PlaySoundAtBall "fx_rubber_longband":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aRubber_Pegs_Hit(idx):PlaySoundAtBall "fx_rubber_peg":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v3.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
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
    PlaySound soundname, 0, 1, Pan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.4, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls, 20 balls, from 0 to 19
Const lob = 0     'number of locked balls
Const maxvel = 42 'max ball velocity
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

        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz)/17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory <1 Then
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
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******************
' RealTime Updates
'******************

Sub RealTime_Timer
    RollingUpdate
    Car.x = carball.x:Car.y = carball.y
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
End Sub

Sub AllSparkTimer_Timer
	AllSpark.Rotz = (AllSpark.Rotz + 1) Mod 360
End Sub

'*************************
' GI - needs new vpinmame
'*************************

Set GICallback = GetRef("GIUpdate")

Sub GIUpdate(no, Enabled)
    For each x in aGiLights
        x.State = ABS(Enabled)
    Next
    For each x in aGiFlashers
        x.Visible = Enabled
    Next
End Sub