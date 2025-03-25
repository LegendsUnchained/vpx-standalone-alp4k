'Banzai Run / IPD No. 175 / May 20, 1988 / 4 Players
'Williams Electronics Games, Incorporated, a subsidiary of WMS Ind., Incorporated (1985-1999) [Trade Name: Williams]
'VPX8 table by jpsalas. This is for desktop and FS. Can be played on 1 screen.
'vpinmame based on table by Lio & Freylis & Destruk

'Aubrel: Changed the code to show the lifter and the ball in table's backwall.
'Aubrel: ShowUpperPF activation updated to switch-on only when the ball reach the top playfield level.
'Aubrel: Fix the magnet code to take the ball only when in the lowest position and small rotation added.
'Aubrel: Lifter switches code fixed, old switches hack removed, lifter's moves are now accurate ! :)


Option Explicit
Randomize

Const BallSize = 50
Const BallMass = 1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'<<<<<<<<<<<<<< ROM OPTIONS >>>>>>>>>>>>>>>>>>>>>>

'Const cGameName = "bnzai_t3" 'l3 rom with fixed target sound
Const cGameName = "bnzai_l3" 'latest williams

'<<<<<<<<<< End of ROM OPTIONS >>>>>>>>>>>>>>>>>>>

Dim bsTrough, cbball, BallCannon, UPFPopper, bsLeftEjectHole, CenterEjectHole, x
Dim plungerIM, lockedball
Dim VarHidden

If Table1.ShowDT = true then
    For each x in aReels
        x.Visible = 1
    Next
    VarHidden = 1
Else
    For each x in aReels
        x.Visible = 0
    Next
    VarHidden = 0
End If

if B2SOn = true then VarHidden = 1

LoadVPM "01210000", "S11.VBS", 3.1

'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = "fx_SolenoidOn"
Const SSolenoidOff = "fx_SolenoidOff"
Const SCoin = "fx_Coin"

'************
' Table init.
'************

Sub table1_Init
    vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Banzai Run - Williams 1988" & vbNewLine & "VPX table by JPSalas 5.5.0"
        .Games(cGameName).Settings.Value("sound") = 1
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 1
        .Games(cGameName).Settings.Value("rol") = 0
        '.SetDisplayPosition 0,0,GetPlayerHWnd 'uncomment if you can't see the dmd
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 1
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 9, 10, 11, 12, 0, 0, 0, 0
        .InitKick BallRelease, 90, 4
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 3
        .IsTrough = 1
    End With

    ' Saucers
    Set BallCannon = New cvpmBallStack
    With BallCannon
        .InitSaucer sw36, 36, 10, 40
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    End With

    Set UPFPopper = New cvpmBallStack
    With UPFPopper
        .InitSaucer sw64, 64, 0, 50
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    End With

    Set bsLeftEjectHole = New cvpmBallStack
    With bsLeftEjectHole
        .Initsw 0, 13, 0, 0, 0, 0, 0, 0
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    End With

    Set CenterEjectHole = New cvpmBallStack
    With CenterEjectHole
        .InitSaucer sw17, 17, 190, 20
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    End With

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 40 ' Plunger Power
    Const IMTime = 0.9        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP sw32, IMPowerSetting, IMTime
        .Random 1.5
        .switch 32
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Captive Ball
    Set cbball = New cvpmCaptiveBall
    With cbball
        .InitCaptive CapTrigger, CapWall, CapKicker, 0
        .ForceTrans = .9
        .MinForce = 3.5
        .CreateEvents "cbball"
        .Start
    End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
    RealTime.Enabled = 1

    ShowUpperPF 0 'hide upper playfield
    lockedball = False

    ' Turn on Gi
    vpmtimer.addtimer 1500, "GiOn '"

    LoadLUT
    LoadRampColor
End Sub

'******************
' RealTime Updates
'******************

Sub RealTime_Timer
    RollingUpdate
End Sub

Sub LeftFlipper_Animate: LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle: End Sub
Sub RightFlipper_Animate: RightFlipperTop.RotZ = RightFlipper.CurrentAngle: End Sub
Sub RightFlipper001_Animate: RightFlipperTop001.RotZ = RightFlipper001.CurrentAngle: End Sub
Sub LeftFlipper001_Animate: LeftFlipperTop001.RotZ = LeftFlipper001.CurrentAngle: End Sub
Sub RightFlipper002_Animate: RightFlipperTop002.RotZ = RightFlipper002.CurrentAngle: End Sub

'*********************************************************
' Real Time Flipper adjustments - by JLouLouLou & JPSalas
'        (to enable flipper tricks) 
'*********************************************************

Dim FlipperPower
Dim FlipperElasticity
Dim SOSTorque, SOSAngle
Dim FullStrokeEOS_Torque, LiveStrokeEOS_Torque
Dim LeftFlipperOn
Dim RightFlipperOn

Dim LLiveCatchTimer
Dim RLiveCatchTimer
Dim LiveCatchSensivity

FlipperPower = 5000
FlipperElasticity = 0.8
FullStrokeEOS_Torque = 0.3 	' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.2	' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

LeftFlipper.EOSTorqueAngle = 10
RightFlipper.EOSTorqueAngle = 10

SOSTorque = 0.1
SOSAngle = 6

LiveCatchSensivity = 10

LLiveCatchTimer = 0
RLiveCatchTimer = 0

LeftFlipper.TimerInterval = 1
LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's tricks timer
'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
	If LeftFlipperOn = 1 Then
		If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
			LeftFlipper.EOSTorque = FullStrokeEOS_Torque
			LLiveCatchTimer = LLiveCatchTimer + 1
			If LLiveCatchTimer < LiveCatchSensivity Then
				LeftFlipper.Elasticity = 0
			Else
				LeftFlipper.Elasticity = FlipperElasticity
				LLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		LeftFlipper.Elasticity = FlipperElasticity
		LeftFlipper.EOSTorque = LiveStrokeEOS_Torque
		LLiveCatchTimer = 0
	End If
	

'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
 	If RightFlipperOn = 1 Then
		If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
			RightFlipper.EOSTorque = FullStrokeEOS_Torque
			RLiveCatchTimer = RLiveCatchTimer + 1
			If RLiveCatchTimer < LiveCatchSensivity Then
				RightFlipper.Elasticity = 0
			Else
				RightFlipper.Elasticity = FlipperElasticity
				RLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		RightFlipper.Elasticity = FlipperElasticity
		RightFlipper.EOSTorque = LiveStrokeEOS_Torque
		RLiveCatchTimer = 0
	End If
End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 8:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If
    If keycode = keyUpperLeft Then ChangeRampColor
    If vpmKeyDown(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundat "fx_PlungerPull", Plunger:Plunger.Pullback
'test
'if keycode = "3" then ShowUpperPF 1
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_plunger", Plunger:Plunger.Fire
'test
'if keycode = "3" then ShowUpperPF 0
End Sub

'************************************
'       LUT - Darkness control
' 10 normal level & 10 warmer levels 
'************************************

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

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 22:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0"
        Case 1:table1.ColorGradeImage = "LUT1"
        Case 2:table1.ColorGradeImage = "LUT2"
        Case 3:table1.ColorGradeImage = "LUT3"
        Case 4:table1.ColorGradeImage = "LUT4"
        Case 5:table1.ColorGradeImage = "LUT5"
        Case 6:table1.ColorGradeImage = "LUT6"
        Case 7:table1.ColorGradeImage = "LUT7"
        Case 8:table1.ColorGradeImage = "LUT8"
        Case 9:table1.ColorGradeImage = "LUT9"
        Case 10:table1.ColorGradeImage = "LUT10"
        Case 11:table1.ColorGradeImage = "LUT Warm 0"
        Case 12:table1.ColorGradeImage = "LUT Warm 1"
        Case 13:table1.ColorGradeImage = "LUT Warm 2"
        Case 14:table1.ColorGradeImage = "LUT Warm 3"
        Case 15:table1.ColorGradeImage = "LUT Warm 4"
        Case 16:table1.ColorGradeImage = "LUT Warm 5"
        Case 17:table1.ColorGradeImage = "LUT Warm 6"
        Case 18:table1.ColorGradeImage = "LUT Warm 7"
        Case 19:table1.ColorGradeImage = "LUT Warm 8"
        Case 20:table1.ColorGradeImage = "LUT Warm 9"
        Case 21:table1.ColorGradeImage = "LUT Warm 10"
    End Select
End Sub

Dim GiIntensity
GiIntensity = 1   'can be used by the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

' New LUT postit
Function GetHSChar(String, Index)
    Dim ThisChar
    Dim FileName
    ThisChar = Mid(String, Index, 1)
    FileName = "PostIt"
    If ThisChar = " " or ThisChar = "" then
        FileName = FileName & "BL"
    ElseIf ThisChar = "<" then
        FileName = FileName & "LT"
    ElseIf ThisChar = "_" then
        FileName = FileName & "SP"
    Else
        FileName = FileName & ThisChar
    End If
    GetHSChar = FileName
End Function

Sub SetLUTLine(String)
    Dim Index
    Dim xFor
    Index = 1
    LUBack.imagea="PostItNote"
    For xFor = 1 to 40
        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
SetLUTLine ""
LUBack.imagea="PostitBL"
End Sub

'*************************
' General Illumination
'*************************
Sub GiOn
    Dim bulb
    PlaySound "fx_gion"
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    Dim bulb
    PlaySound "fx_gioff"
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

Sub GiUPFOn
    Dim bulb
    PlaySound "fx_gion"
    For each bulb in aGiUpperLights
        bulb.State = 1
    Next
End Sub

Sub GiUPFOff
    Dim bulb
    PlaySound "fx_gioff"
    For each bulb in aGiUpperLights
        bulb.State = 0
    Next
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk
    'DOF 101, DOFPulse
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 30
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing004.Visible = 0:LeftSLing003.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing003.Visible = 0:LeftSLing002.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing002.Visible = 0:Lemk.RotX = -20:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk
    'DOF 102, DOFPulse
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 31
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:Remk.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' Scoring rubbers

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 27:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper1:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 28:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper2:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 29:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper3:End Sub

' Drain & holes
Sub Drain_Hit:PlaysoundAt "fx_drain", Drain:bsTrough.AddBall Me:End Sub
Sub sw36_Hit:PlaysoundAt "fx_kicker_enter", sw36:BallCannon.AddBall 0:End Sub
Sub sw17_Hit:PlaysoundAt "fx_kicker_enter", sw17:CenterEjectHole.AddBall 0:End Sub
Sub sw13_Hit:PlaysoundAt "fx_kicker_enter", sw13:bsLeftEjectHole.AddBall 1:End Sub

'upf holes
Dim myball, BallinLock

Sub sw64_Hit
    PlaysoundAt "fx_kicker_enter", sw64
    UPFPopper.AddBall 0
    'make the ball invisible in case the ball will be locked
    'sw64.Destroyball
    'set myball = sw64.CreateSizedBallWithMass(BallSize / 2, BallMass)
    Activeball.Visible = False
    BallinLock = True
End Sub

Sub sw64_Unhit
    If lockedball Then           'keep the ball invisible until it falls down to the lower playfield
        Activeball.Visible = False 'unsure it is invisible
    Else
        Activeball.Visible = True
    End If
    BallinLock = False
End Sub

Sub sw59_Hit
    sw59.DestroyBall
    vpmTimer.PulseSw 59
    vpmTimer.AddTimer 1500, "EndofUPFExit '"
End Sub

Sub sw52_Hit
    sw52.DestroyBall
    vpmTimer.PulseSw 52
    vpmTimer.AddTimer 1500, "EndofUPFExit '"
End Sub

Sub EndofUPFExit
    Set myball = EndOfupf.CreateSizedBallWithMass(BallSize / 2, BallMass)
    If lockedball Then           'keep the ball invisible until it falls down to the lower playfield
        myball.Visible = False  'unsure it is invisible
    Else
        myball.Visible = True
    End If
    EndOfupf.Kick 180, 4
End Sub

Sub UPFdrain_Hit
    ShowUpperPF 0
    UPFdrain.DestroyBall
    PlaySoundAt "fx_plastic", UPFexit
    set myball = UPFexit.CreateSizedBallWithMass(BallSize / 2, BallMass)
    myball.Visible = True
    UPFexit.Kick 270, 8
    lockedball = false 'nor more locked ball at the upper playfield
End Sub

' Rollovers
Sub sw19_Hit 'plunger lane
    Controller.Switch(19) = 1
    PlaySoundAt "fx_sensor", sw19
    If BallinLock Then 'the ball is locked at the top playfield
        ShowUpperPF 0        ' turn off the upper playfield and make the main playfield is visible
        lockedball = True   ' to hide the ball in the upper playfield 
    End If
End Sub
Sub sw19_UnHit:Controller.Switch(19) = 0:End Sub

Sub sw14_Hit:Controller.Switch(14) = 1:PlaySoundAt "fx_sensor", sw14:End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub

Sub sw15_Hit:Controller.Switch(15) = 1:PlaySoundAt "fx_sensor", sw15:End Sub
Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub

Sub sw16_Hit:Controller.Switch(16) = 1:PlaySoundAt "fx_sensor", sw16:End Sub
Sub sw16_UnHit:Controller.Switch(16) = 0:End Sub

Sub sw20_Hit:Controller.Switch(20) = 1:PlaySoundAt "fx_sensor", sw20:End Sub
Sub sw20_UnHit:Controller.Switch(20) = 0:End Sub

Sub sw23_Hit:Controller.Switch(23) = 1:PlaySoundAt "fx_sensor", sw23:End Sub
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub

Sub sw24_Hit:Controller.Switch(24) = 1:PlaySoundAt "fx_sensor", sw24:End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub

Sub sw26_Hit:Controller.Switch(26) = 1:PlaySoundAt "fx_sensor", sw26:End Sub
Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub

Sub sw33_Hit:Controller.Switch(33) = 1:PlaySoundAt "fx_sensor", sw33:End Sub
Sub sw33_UnHit:Controller.Switch(33) = 0:End Sub

Sub sw35_Hit:Controller.Switch(35) = 1:PlaySoundAt "fx_sensor", sw35:End Sub
Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub

Sub sw53_Hit:Controller.Switch(53) = 1:PlaySoundAt "fx_sensor", sw53:End Sub
Sub sw53_UnHit:Controller.Switch(53) = 0:End Sub

Sub sw54_Hit:Controller.Switch(54) = 1:PlaySoundAt "fx_sensor", sw54:End Sub
Sub sw54_UnHit:Controller.Switch(54) = 0:End Sub

'Spinners

Sub sw21_Spin():vpmTimer.PulseSw 21:PlaySoundAt "fx_spinner", sw21:End Sub
Sub sw22_Spin():vpmTimer.PulseSw 22:PlaySoundAt "fx_spinner", sw22:End Sub

'Targets
Sub sw43_Hit:vpmTimer.PulseSw 43:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw44_Hit:vpmTimer.PulseSw 44:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw45_Hit:vpmTimer.PulseSw 45:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw46_Hit:vpmTimer.PulseSw 46:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw18_Hit:vpmTimer.PulseSw 18:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw41_Hit:vpmTimer.PulseSw 41:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw38_Hit:vpmTimer.PulseSw 38:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw39_Hit:vpmTimer.PulseSw 39:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw40_Hit:vpmTimer.PulseSw 40:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw47_Hit:vpmTimer.PulseSw 47:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw37_Hit:vpmTimer.PulseSw 37:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub

Sub sw42_Hit:vpmTimer.PulseSw 42:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw43a_Hit:vpmTimer.PulseSw 43:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw44a_Hit:vpmTimer.PulseSw 44:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw47a_Hit:vpmTimer.PulseSw 47:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw48a_Hit:vpmTimer.PulseSw 48:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw49_Hit:vpmTimer.PulseSw 49:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
'Sub sw55_Hit:vpmTimer.PulseSw 55:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw56_Hit:vpmTimer.PulseSw 56:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw57_Hit:vpmTimer.PulseSw 57:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw58_Hit:vpmTimer.PulseSw 58:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw60_Hit:vpmTimer.PulseSw 60:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw61_Hit:vpmTimer.PulseSw 61:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw62_Hit:vpmTimer.PulseSw 62:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub

'*********
'Solenoids
'*********

SolCallBack(1) = "bsTrough.SolIn"
SolCallBack(2) = "bsTrough.SolOut"
SolCallback(4) = "SolBallCannon"
SolCallback(7) = "vpmSolSound SoundFX(""fx_Knocker"",DOFKnocker),"
SolCallback(8) = "CenterEjectHole.SolOut"
SolCallback(16) = "SolLeftEjectHole"
'SolCallback(18) 'left slingshot
'SolCallback(20) 'right slingshot
SolCallback(14) = "SolKickback"
SolCallback(23) = "vpmNudge.SolGameOn"
SolCallBack(25) = "SetLamp 125,"
SolCallBack(26) = "SetLamp 126,"
SolCallBack(27) = "SetLamp 127,"
SolCallBack(28) = "SetLamp 128,"
SolCallBack(29) = "SetLamp 129,"
SolCallBack(30) = "SetLamp 130,"
SolCallBack(31) = "SetLamp 131,"
SolCallBack(32) = "SetLamp 132,"

' upf
SolCallback(3) = "UPFPopper.SolOut"
SolCallback(5) = "SolFlipperPost"
SolCallback(6) = "SolFreestyleKicker"
SolCallback(9) = "SolUpperFlipperRelay"
SolCallback(10) = "SolLPFGIRelay"
SolCallback(11) = "SolUPFGIRelay"
'SolCallback(12) = "SolenoidSelectRelay"
SolCallback(13) = "SolBallLifterMagnet"
SolCallback(15) = "SolBallLifterMotorRelay"
'SolCallBack(22) = "SolUpperLampRelay"

Dim UpperFlippers
Sub SolUpperFlipperRelay(Enabled)
    UpperFlippers = Enabled
End Sub

Sub SolBallCannon(enabled)
    If enabled Then
        BallCannon.ExitSol_On
        lemk2.RotX = 20
        vpmtimer.addtimer 150, "lemk2.RotX = -20 '"
    End If
End Sub

Sub SolLeftEjectHole(enabled)
    If enabled Then
        If bsLeftEjectHole.balls > 0 Then
            bsLeftEjectHole.ExitSol_On
            PlaySoundAt "fx_kicker", sw13
            sw13.Kick 90, 20
        End If
    End If
End Sub

Sub Solkickback(enabled)
    If enabled then
        PlungerIM.AutoFire
        PlaySoundAt "fx_kicker", sw32
    End If
End Sub

Sub SolFlipperPost(enabled)
    If enabled Then
        Postswitch()
    End If
End Sub

Sub Postswitch()
    If Post.IsDropped = True Then
        Post.IsDropped = False
        Controller.Switch(50) = False
        PlaySoundAt "fx_solenoidoff", leftflipper001
    Else
        Post.IsDropped = True
        Controller.Switch(50) = True
        PlaySoundAt "fx_solenoidon", leftflipper001
    End If
End Sub

Sub SolFreestyleKicker(enabled)
    If enabled then
        Freestylekicker.kick 10, 45
        PlaySoundAt "fx_kicker", freestylekicker
    End If
End Sub

Sub SolLPFGIRelay(enabled)
    If enabled Then
        GiOff
    Else
        GiOn
    End If
End Sub

Sub SolUPFGIRelay(enabled)
    If enabled Then
        GiUPFOff
    Else
        GiUPFOn
    End If
End Sub

'UPF captive ball
Sub Capwall2_Hit
    Dim a
    a = ActiveBall.Vely * 0.3
    PLaySoundAt "fx_collide", Capkicker2
    Capkicker2.kick 10, a
    vpmTimer.PulseSw 55
End Sub

'****************
' Magnet & Motor
'****************
Dim BallInMagnet
'Aubrel: Fix how the lifter moves and takes the ball
Dim MagnetIsEnabled

Sub SolBallLifterMagnet(enabled)
    debug.print "magnet on"
    If enabled and Controller.Switch(13) = True Then 'check if there´s a ball resting in left saucer
		MagnetIsEnabled = True
    Else
        If BallInMagnet Then
            mBall.Visible = 0
            UPFrelease.CreateSizedBallWithMass BallSize / 2, BallMass
            UPFrelease.Kick 0, 0
            BallInMagnet = False
        End If
		MagnetIsEnabled = False
    End If
End Sub

'***************
' Lifter Motor
'***************

Dim LiftPos, LiftDir, LiftPosY, LiftPosX, ShowUP
'Aubrel: Show the lifter in table's backwall too
'LiftPosY = Array(2112, 2102, 2092, 2082, 2072, 2062, 2052, 2042, 2032, 2022, _
LiftPosY = Array(45, 65, 85, 105, 125, 145, 165, 185, 205, 225,              _
    2012, 2002, 1992, 1982, 1972, 1962, 1952, 1942, 1932, 1922,              _
    1912, 1902, 1892, 1882, 1872, 1862, 1852, 1842, 1832, 1822,              _
    1812, 1802, 1792, 1782, 1772, 1762, 1752, 1742, 1732, 1722,              _
    1712, 1702, 1692, 1682, 1672, 1662, 1652, 1642, 1632, 1622,              _
    1612, 1602, 1592, 1582, 1572, 1562, 1552, 1542, 1532, 1522,              _
    1512, 1502, 1492, 1482, 1472, 1462, 1452, 1442, 1432, 1422,              _
    1412, 1402, 1392, 1382, 1372, 1362, 1352, 1342, 1332, 1322)

LiftPosX = Array(150, 150, 150, 150, 150, 150, 150, 150, 150, 150,           _
    150, 150, 150, 150, 150, 150, 150, 150, 150, 150,                        _
    150, 150, 147.5, 145, 142.5, 140, 137.5, 135, 132.5, 130,                _
    127.5, 125, 122.5, 120, 117.5, 115, 112.5, 110, 110, 110,                _
    110, 110, 110, 110, 110, 110, 110, 110, 110, 110,                        _
    110, 110, 110, 110, 110, 110, 110, 110, 110, 110,                        _
    112.5, 115, 117.5, 120, 122.5, 125, 127.5, 130, 132.5, 135,              _
    137.5, 140, 142.5, 145, 147.5, 150, 150, 150, 150, 150)

LiftPos = 50
LiftDir = 1
ShowUP = 0

Sub SolBallLifterMotorRelay(enabled)
    If enabled Then
		LifterTimer.Enabled = True
		PlaySound "fx_bzmotor", -1
    Else
        LifterTimer.Enabled = False
        StopSound "fx_bzmotor"
    End If
End Sub

Sub LifterTimer_Timer() 'moving the actual magnet
	'Aubrel: use accurate switches detections
	If LiftPos = 20 Then
		Controller.Switch(63) = False
		Controller.Switch(51) = True
	ElseIf LiftPos = 70 Then
		Controller.Switch(63) = True
		Controller.Switch(51) = False
	Else
		Controller.Switch(63) = False
		Controller.Switch(51) = False
	End If
	'Aubrel: keep ball in kicker until it's taken  
	If LiftPos = 0 And MagnetIsEnabled And Not BallInMagnet Then
        bsLeftEjectHole.Balls = 0
        sw13.DestroyBall
        Controller.Switch(13) = False
        BallInMagnet = True
        mBall.Visible = 1
	End If
    MagnetP.X = LiftPosX(LiftPos)
	'Aubrel: Show the lift in table's backwall too
	If LiftPos < 10 Then
		MagnetP.ObjRotX = 0
		MagnetP.Y = 80
		MagnetP.Z = LiftPosY(LiftPos)
		MagnetP.Visible = Abs(ShowUP - 1)
	Else
		MagnetP.ObjRotX = 90
		MagnetP.ObjRotZ = (120 - LiftPosX(LiftPos)) / 2
		MagnetP.Y = LiftPosY(LiftPos)
		MagnetP.Visible = ShowUP
	End If
    If BallInMagnet Then
        mBall.X = LiftPosX(LiftPos)
		'Aubrel: Show the ball in table's backwall too
		If LiftPos < 10 Then
			mBall.Y = 100
 			mBall.Z = LiftPosY(LiftPos) - 10
		Else
			If LiftPos = 10 Then mBall.z = 225 : ShowUpperPF 1
			mBall.Y = LiftPosY(LiftPos) + 20
		End If
    End If
    If LiftPos = 0 Then
		LiftDir = - LiftDir
    ElseIf LiftPos = 79 Then
		LiftDir = - LiftDir
	End If
	LiftPos = LiftPos + LiftDir
End Sub

Sub ShowUpperPF(show)
    dim i
    for each i in aUpperPF:i.Visible = show:Next
    for each i in aUpperPFWalls:i.SideVisible = show:Next
    for each i in aGiUpperLights:i.Visible = show:i.State = show:Next 'show the Gi upper
	ShowUP = show
    If show Then
        Capkicker2.CreateSizedBallWithMass BallSize / 2, BallMass
        freestylekicker.CreateSizedBallWithMass BallSize / 2, BallMass
        table1.SlopeMin = 20
        table1.SlopeMax = 20
    Else
        Capkicker2.DestroyBall
        freestylekicker.DestroyBall
        table1.SlopeMin = 5.4
        table1.SlopeMax = 5.4
        LeftFlipper001.RotateToStart
        LeftFlipper002.RotateToStart
        RightFlipper002.RotateToStart
    End If
End Sub

'*******************
'  Flipper Subs
'*******************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), LeftFlipper
        If UpperFlippers Then
            LeftFlipper001.RotateToEnd
            LeftFlipper002.RotateToEnd
        Else
        LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
        End If
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), LeftFlipper
        If UpperFlippers Then
            LeftFlipper001.RotateToStart
            LeftFlipper002.RotateToStart
        Else
        LeftFlipper.RotateToStart
        LeftFlipperOn = 0
        End If
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), RightFlipper
        If UpperFlippers Then
            RightFlipper002.RotateToEnd
        Else
        RightFlipper.RotateToEnd
            RightFlipper001.RotateToEnd
        RightFlipperOn = 1
        End If
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), RightFlipper
        If UpperFlippers Then
            RightFlipper002.RotateToStart
        Else
        RightFlipper.RotateToStart
            RightFlipper001.RotateToStart
        RightFlipperOn = 0
        End If
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper001_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper001_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper002_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper002_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'**********************************************************
'     JP's Lamp Fading for VPX and Vpinmame v4.0
' FadingStep used for all kind of lamps
' FlashLevel used for modulated flashers
' LampState keep the real lamp state in a array
'**********************************************************

Dim LampState(200), FadingStep(200), FlashLevel(200)

InitLamps() ' turn off the lights and flashers and reset them to the default parameters

' vpinmame Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0)) = chgLamp(ii, 1) 'keep the real state in an array
            FadingStep(chgLamp(ii, 0)) = chgLamp(ii, 1)
        Next
    End If
    UpdateLeds
    UpdateLamps
End Sub

Sub UpdateLamps
    ' playfield lights
    Lamp 1, l1
    Lamp 2, l2
    Lamp 3, l3
    'Lamp 4, l4
    Lamp 5, l5
    Lamp 6, l6
    Lamp 7, l7
    Lamp 8, l8
    Lamp 9, l9
    Lamp 10, l10
    Lamp 11, l11
    Lamp 12, l12
    Lamp 13, l13
    Lamp 14, l14
    Lamp 15, l15
    Lamp 16, l16
    Lampm 17, l65
    Lamp 17, l17
    Lamp 18, l18
    Lampm 19, l66
    Lamp 19, l19
    Lampm 20, l68
    Lamp 20, l20
    Lampm 21, l69
    Lamp 21, l21
    Lampm 22, l70
    Lamp 22, l22
    Lampm 23, l23a
    Lampm 23, l71
    Lamp 23, l23
    Lampm 24, l24a
    Lampm 24, l72
    Lamp 24, l24
    Lamp 25, l25
    Lamp 26, l26
    Lamp 27, l27
    Lamp 28, l28
    Lamp 29, l29
    Lamp 30, l30
    Lamp 31, l31
    Lamp 32, l32
    Lamp 33, l33
    Lamp 34, l34
    Lamp 35, l35
    Lamp 36, l36
    Lampm 37, l37001
    Lamp 37, l37
    Lamp 38, l38
    Lamp 39, l39
    Lamp 40, l40
    Lamp 41, l41
    Lamp 42, l42
    Lamp 43, l43
    Lamp 44, l44
    Lamp 45, l45
    Lamp 46, l46
    Lampm 47, l47001
    Lamp 47, l47
    Lampm 48, l48001
    Lamp 48, l48
    Lampm 49, l49001
    Lamp 49, l49
    Lampm 50, l50001
    Lamp 50, l50
    Lamp 51, l51
    Lamp 52, l52
    Lamp 53, l53
    Lamp 54, l54
    Lamp 55, l55
    Lamp 56, l56
    Lamp 57, l57
    Lamp 58, l58
    Lamp 59, l59
    Lamp 60, l60
    Lamp 61, l61
    Lamp 62, l62
    Lamp 63, l63
    Lamp 64, l64

    'flashers
    Lamp 125, f001
    Lamp 126, f002
    Lampm 127, Flash019
    Lampm 127, Flash020
    Lampm 127, Flash001
    Lampm 127, Flash002
    Lamp 127, Flash003
    Lampm 128, F28
    Lampm 128, Flash021
    Lampm 128, Flash022
    Lampm 128, Flash004
    Lampm 128, Flash005
    Lamp 128, Flash006
    Lampm 129, Flash023
    Lampm 129, Flash024
    Lampm 129, Flash007
    Lampm 129, Flash008
    Lamp 129, Flash009
    Lampm 130, Flash025
    Lampm 130, Flash026
    Lampm 130, Flash010
    Lampm 130, Flash011
    Lamp 130, Flash012
    Lampm 131, F31
    Lampm 131, Flash027
    Lampm 131, Flash028
    Lampm 131, Flash013
    Lampm 131, Flash014
    Lamp 131, Flash015
    Lampm 132, Flash029
    Lampm 132, Flash030
    Lampm 132, Flash016
    Lampm 132, Flash017
    Lamp 132, Flash018
End Sub

' div lamp subs

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
    LampTimer.Interval = 20
    LampTimer.Enabled = 1
    For x = 0 to 200
        FadingStep(x) = 0
        FlashLevel(x) = 0
    Next
End Sub

Sub SetLamp(nr, value) ' 0 is off, 1 is on
    FadingStep(nr) = abs(value)
End Sub

' Lights: used for VPX standard lights, the fading is handled by VPX itself, they are here to be able to make them work together with the flashers

Sub Lamp(nr, object)
    Select Case FadingStep(nr)
        Case 1:object.state = 1:FadingStep(nr) = -1
        Case 0:object.state = 0:FadingStep(nr) = -1
    End Select
End Sub

Sub Lampm(nr, object) ' used for multiple lights, it doesn't change the fading state
    Select Case FadingStep(nr)
        Case 1:object.state = 1
        Case 0:object.state = 0
    End Select
End Sub

' Flashers:  0 starts the fading until it is off

Sub Flash(nr, object)
    Dim tmp
    Select Case FadingStep(nr)
        Case 1:Object.IntensityScale = 1:FadingStep(nr) = -1
        Case 0
            tmp = Object.IntensityScale * 0.85 - 0.01
            If tmp > 0 Then
                Object.IntensityScale = tmp
            Else
                Object.IntensityScale = 0
                FadingStep(nr) = -1
            End If
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change the fading state
    Dim tmp
    Select Case FadingStep(nr)
        Case 1:Object.IntensityScale = 1
        Case 0
            tmp = Object.IntensityScale * 0.85 - 0.01
            If tmp > 0 Then
                Object.IntensityScale = tmp
            Else
                Object.IntensityScale = 0
            End If
    End Select
End Sub

' Desktop Objects: Reels & texts

' Reels - 4 steps fading
Sub Reel(nr, object)
    Select Case FadingStep(nr)
        Case 1:object.SetValue 1:FadingStep(nr) = -1
        Case 0:object.SetValue 2:FadingStep(nr) = 2
        Case 2:object.SetValue 3:FadingStep(nr) = 3
        Case 3:object.SetValue 0:FadingStep(nr) = -1
    End Select
End Sub

Sub Reelm(nr, object)
    Select Case FadingStep(nr)
        Case 1:object.SetValue 1
        Case 0:object.SetValue 2
        Case 2:object.SetValue 3
        Case 3:object.SetValue 0
    End Select
End Sub

' Reels non fading
Sub NfReel(nr, object)
    Select Case FadingStep(nr)
        Case 1:object.SetValue 1:FadingStep(nr) = -1
        Case 0:object.SetValue 0:FadingStep(nr) = -1
    End Select
End Sub

Sub NfReelm(nr, object)
    Select Case FadingStep(nr)
        Case 1:object.SetValue 1
        Case 0:object.SetValue 0
    End Select
End Sub

'Texts

Sub Text(nr, object, message)
    Select Case FadingStep(nr)
        Case 1:object.Text = message:FadingStep(nr) = -1
        Case 0:object.Text = "":FadingStep(nr) = -1
    End Select
End Sub

Sub Textm(nr, object, message)
    Select Case FadingStep(nr)
        Case 1:object.Text = message
        Case 0:object.Text = ""
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

'Walls, flashers, ramps and Primitives used as 4 step fading images
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingStep(nr)
        Case 1:object.image = a:FadingStep(nr) = -1
        Case 0:object.image = b:FadingStep(nr) = 2
        Case 2:object.image = c:FadingStep(nr) = 3
        Case 3:object.image = d:FadingStep(nr) = -1
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingStep(nr)
        Case 1:object.image = a
        Case 0:object.image = b
        Case 2:object.image = c
        Case 3:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingStep(nr)
        Case 1:object.image = a:FadingStep(nr) = -1
        Case 0:object.image = b:FadingStep(nr) = -1
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingStep(nr)
        Case 1:object.image = a
        Case 0:object.image = b
    End Select
End Sub

Sub Gi(nr, object)
    Select Case FadingStep(nr)
        Case 1:GiOn:FadingStep(nr) = -1
        Case 0:GiOff:FadingStep(nr) = -1
    End Select
End Sub

'************************************
'          LEDs Display
'************************************

Dim Digits(28)
Digits(0) = Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
Digits(1) = Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
Digits(2) = Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
Digits(3) = Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
Digits(4) = Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
Digits(5) = Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
Digits(6) = Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
Digits(7) = Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
Digits(8) = Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
Digits(9) = Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
Digits(10) = Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
Digits(11) = Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
Digits(12) = Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
Digits(13) = Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
Digits(14) = Array(b00, b02, b05, b06, b04, b01, b03, b07)
Digits(15) = Array(b10, b12, b15, b16, b14, b11, b13)
Digits(16) = Array(b20, b22, b25, b26, b24, b21, b23)
Digits(17) = Array(b30, b32, b35, b36, b34, b31, b33, b37)
Digits(18) = Array(b40, b42, b45, b46, b44, b41, b43)
Digits(19) = Array(b50, b52, b55, b56, b54, b51, b53)
Digits(20) = Array(b60, b62, b65, b66, b64, b61, b63)
Digits(21) = Array(b70, b72, b75, b76, b74, b71, b73, b77)
Digits(22) = Array(b80, b82, b85, b86, b84, b81, b83)
Digits(23) = Array(b90, b92, b95, b96, b94, b91, b93)
Digits(24) = Array(ba0, ba2, ba5, ba6, ba4, ba1, ba3, ba7)
Digits(25) = Array(bb0, bb2, bb5, bb6, bb4, bb1, bb3)
Digits(26) = Array(bc0, bc2, bc5, bc6, bc4, bc1, bc3)
Digits(27) = Array(bd0, bd2, bd5, bd6, bd4, bd1, bd3)

Sub UpdateLeds
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(chgLED)
            num = chgLED(ii, 0):chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For Each obj In Digits(num)
                If chg And 1 Then obj.State = stat And 1
                chg = chg \ 2:stat = stat \ 2
            Next
        Next
    End If
End Sub

'*********************************
' Diverse Collection Hit Sounds
'*********************************

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
Sub aTargets_Hit(idx):PlaySound SoundFX("fx_target", DOFTargets), 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall):End Sub
Sub aRollovers_Hit(idx):PlaySoundAt "fx_sensor", aRollovers(idx):End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v4.0
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
    PlaySound soundname, 0, 1, Pan(tableobj), 0.2, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v4.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls
Const lob = 0     'number of locked balls
Const maxvel = 40 'max ball velocity
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

    ' stop the sound of deleted balls
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
        aBallShadow(b).Height = BOT(b).Z - Ballsize / 2 + 1

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            ElseIf  BOT(b).z > 50 AND BOT(b).z < 190 Then
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 5
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed & spin control
        BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95
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

'*****************************
' Ball 2 Ball Collision Sound
'*****************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'*****************************
'    Change RAMP colors
'*****************************

Dim RampColor

Sub LoadRampColor
    Dim x
    x = LoadValue(cGameName, "RampColor")
    If(x <> "")Then RampColor = x Else RampColor = 2
    UpdateRampColor
End Sub

Sub SaveRampColor
    SaveValue cGameName, "RampColor", RampColor
End Sub

Sub ChangeRampColor
    RampColor = (RampColor + 1)MOD 4
    UpdateRampColor
End Sub

Sub UpdateRampColor
    Dim x
    Select Case RampColor
        Case 0:x = RGB(0, 64, 255) 'blue
        Case 1:x = RGB(96, 96, 96) 'White
        Case 2:x = RGB(0, 128, 32) 'Green
        Case 3:x = RGB(128, 0, 0)  'Red
    End Select
    MaterialColor "Plastic Transp Ramps1", x
    SaveRampColor
End Sub