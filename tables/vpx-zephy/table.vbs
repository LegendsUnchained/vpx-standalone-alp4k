' Zephy / LTD do Brasil / IPD No. 4592 / 1982 / 3 Players
' VPX8 -  by JPSalas 2024, version 5.5.0
' vpm script by mfuegemann

Option Explicit
Randomize

Const BallSize = 50
Const BallMass = 1
Plunger.MechStrength = 150

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01550000", "ltd3.vbs", 3.26

Dim bsTrough, RightDropTargetBank, bsTopHole, bsLeftHole, Flipperactive, x

Const cGameName = "zephy"

Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0   'set it to 1 if the table runs too fast
Const HandleMech = 0
Const vpmhidden = 1 'hide the vpinmame window
Const FreePlay = False

If Table1.ShowDT = true then
    For each x in aReels
        x.Visible = 1
    Next
else
    For each x in aReels
        x.Visible = 0
    Next
end if

' Standard Sounds
Const SSolenoidOn = "fx_Solenoidon"
Const SSolenoidOff = "fx_Solenoidoff"
Const SCoin = "fx_Coin"

'******************
' Realtime Updates
'******************

Sub RealTime_Timer
    GIUpdate
    RollingUpdate
End Sub

' flippers top animations

Sub LeftFlipper_Animate: LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle: End Sub
Sub RightFlipper_Animate: RightFlipperTop.RotZ = RightFlipper.CurrentAngle: End Sub

'************
' Table init.
'************

Sub table1_Init
    vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Zephy - LTD 1982" & vbNewLine & "VPX7 table by JPSalas v.5.5.0"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = vpmhidden
        .Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 1
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, Bumper4, LeftSlingshot, RightSlingshot)

    ' Trough

    Set bsTrough = New cvpmTrough
    With bsTrough
        .Size = 2
        .EntrySw = 57
        .InitSwitches Array(58)
        .InitExit BallRelease, 90, 5
        .InitEntrySounds "fx_drain", "fx_Solenoidon", "fx_Solenoidoff"
        .InitExitSounds SoundFX("fx_Solenoid", DOFContactors), SoundFX("fx_ballrel", DOFContactors)
        .Balls = 1
        .CreateEvents "bsTrough", Drain
    End With
    DrainHole.createball
    DrainHole.kick 160, 2

    'Top Hole
    set bsTopHole = new cvpmSaucer
    bsTopHole.InitKicker TopHole, 25, 210, 10, 0
    bsTopHole.InitExitVariance 5, 2
    bsTopHole.InitSounds SoundFX("fx_kicker_enter", DOFContactors), SoundFX("fx_kicker", DOFContactors), SoundFX("fx_kicker", DOFContactors)

    'Left Hole
    set bsLeftHole = new cvpmSaucer
    bsLeftHole.InitKicker LeftHole, 26, 125, 10, 0
    bsLeftHole.InitExitVariance 5, 2
    bsLeftHole.InitSounds SoundFX("fx_kicker_enter", DOFContactors), SoundFX("fx_kicker", DOFContactors), SoundFX("fx_kicker", DOFContactors)

    ' Drop targets
    set RightDropTargetBank = new cvpmDropTarget
    RightDropTargetBank.InitDrop Array(s33, s34, s35, s36), Array(33, 34, 35, 36)
    RightDropTargetBank.InitSnd SoundFX("fx_droptarget", DOFContactors), SoundFX("fx_resetdrop", DOFContactors)
    RightDropTargetBank.CreateEvents "RightDropTargetBank"

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = True
    Realtime.Enabled = True
    LoadLUT

    ' Turn on Gi
    vpmtimer.addtimer 2000, "GiOn '"
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.stop:End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If FreePlay and keycode = Startgamekey Then vpmtimer.pulsesw -7
    If keycode = LeftFlipperKey AND flipperactive Then SolLFlipper 1
    If keycode = RightFlipperKey AND flipperactive Then SolRFlipper 1
    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If
    If vpmKeyDown(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_PlungerPull", Plunger:Plunger.Pullback
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = LeftFlipperKey AND flipperactive Then SolLFlipper 0
    If keycode = RightFlipperKey AND flipperactive Then SolRFlipper 0
    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PLaySoundAt "fx_plunger", Plunger:Plunger.Fire
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
    vpmTimer.PulseSw 10
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
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 9
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

' Rubbers animations
Dim Rub1, Rub2

Sub s6d_Hit:vpmTimer.PulseSw 6:Rub1 = 1:s6d_Timer:End Sub

Sub s6d_Timer
    Select Case Rub1
        Case 1:r3.Visible = 0:r6.Visible = 1:s6d.TimerEnabled = 1
        Case 2:r6.Visible = 0:r7.Visible = 1
        Case 3:r7.Visible = 0:r3.Visible = 1:s6d.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub s6c_Hit:vpmTimer.PulseSw 6:Rub2 = 1:s6c_Timer:End Sub
Sub s6c_Timer
    Select Case Rub2
        Case 1:r4.Visible = 0:r8.Visible = 1:s6c.TimerEnabled = 1
        Case 2:r8.Visible = 0:r10.Visible = 1
        Case 3:r10.Visible = 0:r4.Visible = 1:s6c.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub

' Scoring rubbers
Sub s6a_Hit:vpmTimer.PulseSw 6:End Sub
Sub s6b_Hit:vpmTimer.PulseSw 6:End Sub
Sub s6e_Hit:vpmTimer.PulseSw 6:End Sub
Sub s6f_Hit:vpmTimer.PulseSw 6:End Sub

' Spinner
Sub s17_Spin:vpmTimer.PulseSw 17:PlaySoundAt "fx_spinner", s17:End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 2:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper1:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 3:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper2:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 4:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper3:End Sub
Sub Bumper4_Hit:vpmTimer.PulseSw 5:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper4:End Sub

' Drain & Saucers
Sub TopHole_Hit:PlaysoundAt "fx_kicker_enter", TopHole:bsTopHole.AddBall Me:TopHole.Timerenabled = True:End Sub
Sub LeftHole_Hit:PlaysoundAt "fx_kicker_enter", LeftHole:bsLeftHole.AddBall Me:End Sub

' Rollovers
Sub s11_Hit:Controller.Switch(11) = 1:PlaySoundAt "fx_sensor", s11:End Sub
Sub s11_UnHit:Controller.Switch(11) = 0:End Sub

Sub s11a_Hit:Controller.Switch(11) = 1:PlaySoundAt "fx_sensor", s11a:End Sub
Sub s11a_UnHit:Controller.Switch(11) = 0:End Sub

Sub s12_Hit:Controller.Switch(12) = 1:PlaySoundAt "fx_sensor", s12:End Sub
Sub s12_UnHit:Controller.Switch(12) = 0:End Sub

Sub s12a_Hit:Controller.Switch(12) = 1:PlaySoundAt "fx_sensor", s12a:End Sub
Sub s12a_UnHit:Controller.Switch(12) = 0:End Sub

Sub s22_Hit:Controller.Switch(22) = 1:PlaySoundAt "fx_sensor", s22:End Sub
Sub s22_UnHit:Controller.Switch(22) = 0:End Sub

Sub s27_Hit:Controller.Switch(27) = 1:PlaySoundAt "fx_sensor", s27:End Sub
Sub s27_UnHit:Controller.Switch(27) = 0:End Sub

' Top Rollovers

Sub s18_Hit:Controller.Switch(18) = 1:PlaySoundAt "fx_sensor", s18:rol1.Duration 1, 600, 0:End Sub
Sub s18_UnHit:Controller.Switch(18) = 0:End Sub

Sub s19_Hit:Controller.Switch(19) = 1:PlaySoundAt "fx_sensor", s19:rol2.Duration 1, 600, 0:End Sub
Sub s19_UnHit:Controller.Switch(19) = 0:End Sub

Sub s20_Hit:Controller.Switch(20) = 1:PlaySoundAt "fx_sensor", s20:rol3.Duration 1, 600, 0:End Sub
Sub s20_UnHit:Controller.Switch(20) = 0:End Sub

Sub s21_Hit:Controller.Switch(21) = 1:PlaySoundAt "fx_sensor", s21:rol4.Duration 1, 600, 0:End Sub
Sub s21_UnHit:Controller.Switch(21) = 0:End Sub

' targets
Sub s13_Hit:vpmTimer.PulseSw 13:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub s14_Hit:vpmTimer.PulseSw 14:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub

'*********
'Solenoids
'*********

SolCallback(1) = "bsTrough.SolIn"
SolCallback(2) = "bsTrough.SolOut"
SolCallback(9) = "Sol9"
SolCallback(10) = "Sol10"
SolCallback(11) = "Sol11"
SolCallback(12) = "Sol12"
SolCallback(13) = "RightDropTargetBank.SolDropUp"
SolCallback(15) = "bsLeftHole.SolOut"
SolCallback(17) = "Sol_GameOn"
SolCallback(14) = "bsTopHole.SolOut"

Sub Sol9(Enabled)
    if enabled Then
        RightDropTargetBank.Hit(1)
    end If
End Sub

Sub Sol10(Enabled)
    if enabled Then
        RightDropTargetBank.Hit(2)
    end If
End Sub

Sub Sol11(Enabled)
    if enabled Then
        RightDropTargetBank.Hit(3)
    end If
End Sub

Sub Sol12(Enabled)
    if enabled Then
        RightDropTargetBank.Hit(4)
    end If
End Sub

Flipperactive = False

Sub Sol_GameOn(enabled)
    Dim obj
    VpmNudge.SolGameOn enabled
    Flipperactive = enabled
    if not Flipperactive Then
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
    end If
    for each obj in aGiLights
        obj.state = abs(Enabled)
    Next
end sub

'--------------------------------
'------  Helper Functions  ------
'--------------------------------

' Rescue stuck ball
Sub TopHole_Timer
    TopHole.Timerenabled = False
    if TopHole.BallCntOver> 0 Then
        bsTopHole.ExitSol_On
    End If
End Sub

'Running backbox and tube Lights
Dim LSpeed, ChkSpeed, RLState, k

LSpeed = 1
ChkSpeed = 1
RLState = False
k = 0

Sub CheckLampSpeed_Timer
    LSpeed = ChkSpeed
    RLTimer.interval = 320 / LSpeed
    ChkSpeed = 1
End Sub

Sub CheckLampSpeed2_Timer
    if controller.Lamp(8)then
        if not RLState Then
            RLState = True
            ChkSpeed = ChkSpeed + 1
        end if
    else
        RLState = False
    End If
End Sub

Sub B2SL(kk)
	If B2SOn Then
		Controller.B2SSetData 100,0
		Controller.B2SSetData 101,0
		Controller.B2SSetData 102,0
		Controller.B2SSetData 103,0
		Controller.B2SSetData 104,0
		Controller.B2SSetData 105,0
		Controller.B2SSetData 106,0
		Controller.B2SSetData 100+kk,1
	End If
End Sub

Sub RLTimer_Timer
    Select Case k
        Case 0:SetLamp 106, 0:SetLamp 100, 1:B2SL(0)
        Case 1:SetLamp 100, 0:SetLamp 101, 1:B2SL(1)
        Case 2:SetLamp 101, 0:SetLamp 102, 1:B2SL(2)
        Case 3:SetLamp 102, 0:SetLamp 103, 1:B2SL(3)
        Case 4:SetLamp 103, 0:SetLamp 104, 1:B2SL(4)
        Case 5:SetLamp 104, 0:SetLamp 105, 1:B2SL(5)
        Case 6:SetLamp 105, 0:SetLamp 106, 1:B2SL(6)
    End Select
    k = (k + 1)MOD 7
End Sub

'*******************
' Flipper Subs Rev3
'*******************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFContactors), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFContactors), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipperOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFContactors), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipperOn = 1
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFContactors), RightFlipper
        RightFlipper.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

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

'*****************
'   Gi Effects
'*****************

Dim OldGiState
OldGiState = -1 'start witht he Gi off

Sub GiON
	PlaySound"fx_gion"
    For each x in aGiLights
        x.State = 1
    Next
End Sub

Sub GiOFF
	PlaySound"fx_gioff"
    For each x in aGiLights
        x.State = 0
    Next
End Sub

Sub GiEffect(enabled)
    If enabled Then
        For each x in aGiLights
            x.Duration 2, 1000, 1
        Next
    End If
End Sub

Sub GIUpdate
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = -1 Then
            GiOff
        Else
            GiOn
        End If
    End If
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
            LampState(chgLamp(ii, 0)) = chgLamp(ii, 1)       'keep the real state in an array
            FadingStep(chgLamp(ii, 0)) = chgLamp(ii, 1)
        Next
    End If
    UpdateLeds
    UpdateLamps
End Sub

Sub UpdateLamps()
    Lamp 1, l1
    Lamp 2, l2
    Lampm 3, l3a
    Lamp 3, l3
    Lamp 4, l4
    'Lamp 5, l5
    Lamp 6, l6
    'Lamp 7, l7
    'Lamp 8, l8
    Lampm 9, l9a
    Lampm 9, l9b
    Lamp 9, l9
    Lampm 10, l10a
    Lampm 10, l10b
    Lamp 10, l10
    Lampm 11, l11a
    Lampm 11, l11b
    Lamp 11, l11
    Lampm 12, l12a
    Lampm 12, l12b
    Lamp 12, l12
    Lamp 13, l13
    Lamp 14, l14
    Lampm 15, l15a
    Lamp 15, l15
    Lampm 16, l16a
    Lamp 16, l16
    Lamp 17, l17
    Lamp 18, l18
    Lamp 19, l19
    Lamp 20, l20
    Lamp 21, l21
    Lamp 22, l22
    Lamp 23, l23
    Lamp 24, l24
    Lamp 25, l25
    Lamp 26, l26
    Lamp 27, l27
    Lamp 28, l28
    Lamp 29, l29
    Lamp 30, l30
    Lamp 31, l31
    Lamp 32, l32
    Lampm 33, l33a
    Lamp 33, l33
    Lamp 34, l34
    Lamp 35, l35
    Lamp 36, l36
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
    Lamp 47, l47
    Lamp 48, l48

    ' backdrop lights
    Lampm 100, rl1
    Lampm 100, rl8
    Lampm 100, rl15
    Lampm 100, rl22
    Lampm 100, diode001
    Flash 100, tl1

    Lampm 101, rl2
    Lampm 101, rl9
    Lampm 101, rl005
    Lampm 101, rl16
    Lampm 101, rl23
    Lampm 101, diode002
    Flash 101, tl2

    Lampm 102, rl3
    Lampm 102, rl006
    Lampm 102, rl004
    Lampm 102, rl10
    Lampm 102, rl17
    Lampm 102, rl24
    Lampm 102, diode003
    Flash 102, tl3

    Lampm 103, rl4
    Lampm 103, rl001
    Lampm 103, rl11
    Lampm 103, rl18
    Lampm 103, rl25
    Lampm 103, diode004
    Flash 103, tl4

    Lampm 104, rl5
    Lampm 104, rl002
    Lampm 104, rl12
    Lampm 104, rl19
    Lampm 104, rl26
    Lampm 104, diode005
    Flash 104, tl5

    Lampm 105, rl6
    Lampm 105, rl003
    Lampm 105, rl13
    Lampm 105, rl20
    Lampm 105, rl27
    Lampm 105, diode006
    Flash 105, tl6

    Lampm 106, rl7
    Lampm 106, rl14
    Lampm 106, rl21
    Lampm 106, rl28
    Lampm 106, diode007
    Flash 106, tl7
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
'     Based on Scapino's LEDs
'************************************

Dim Digits(32)
Dim Patterns(11)

Patterns(0) = 0    'empty
Patterns(1) = 63   '0
Patterns(2) = 6    '1
Patterns(3) = 91   '2
Patterns(4) = 79   '3
Patterns(5) = 102  '4
Patterns(6) = 109  '5
Patterns(7) = 124  '6
Patterns(8) = 7    '7
Patterns(9) = 127  '8
Patterns(10) = 103 '9

'Assign 6-digit output to reels
Set Digits(0) = a0
Set Digits(1) = a1
Set Digits(2) = a2
Set Digits(3) = a3
Set Digits(4) = a4
Set Digits(5) = a5

Set Digits(6) = b0
Set Digits(7) = b1
Set Digits(8) = b2
Set Digits(9) = b3
Set Digits(10) = b4
Set Digits(11) = b5

Set Digits(12) = c0
Set Digits(13) = c1
Set Digits(14) = c2
Set Digits(15) = c3
Set Digits(16) = c4
Set Digits(17) = c5

Set Digits(18) = e0
Set Digits(19) = e1

Sub UpdateLeds
    On Error Resume Next
    Dim ChgLED, ii, jj, chg, stat
    ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(ChgLED)
            chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For jj = 0 to 10
                If stat = Patterns(jj)then Digits(chgLED(ii, 0)).SetValue jj
            Next
        Next
    End IF
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

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
        aBallShadow(b).Height = BOT(b).Z -Ballsize/2

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 3
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
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed & spin control
            BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95
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

'*****************************
' Ball 2 Ball Collision Sound
'*****************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub