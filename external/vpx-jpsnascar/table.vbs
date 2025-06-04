' JP's Grand Prix - Layout and ROM from Stern (uses any rom from Nascar, GP or Dale Jr)
' VPX8 version 5.5.0 by JPSalas
' Used the tables from TAB/destruk as reference for the script (read: copy & paste a lot! :) )
' DOF by arngrim.

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const BallSize = 50
Const BallMass = 1

Dim VarHidden, UseVPMColoredDMD
If Table1.ShowDT = true then
    UseVPMColoredDMD = true
    VarHidden = 1
else
    UseVPMColoredDMD = False
    VarHidden = 0
	TextBox1.Visible = 0
end if

Const UseVPMModSol = False 'it doesn't work very well, as the ball is not ejected to the plunger, but it may work on later vpinmame versions. If so you need to change the 7 lights in the layer "Flasher Lights" to LED type.

LoadVPM "01560000", "SEGA.VBS", 3.26

'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0
Const UseGI = 1
Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = "fx_solenoidoff"
Const SCoin = "fx_Coin"

Dim bsTrough, bsVLock, bsL, bsT, dtR, bsC, bsTE, cbRight, x

'************
' Table init.
'************

' choose the ROM
Const cGameName = "nascar"   'Nascar
'Const cGameName = "gprix"  'Grand Prix
'Const cGameName = "dalejr" 'Dale Jr.

Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Nascar Race" & vbNewLine & "VPX table by JPSalas v5.5.0"
        .Games(cGameName).Settings.Value("sound") = 1: 'ensure the sound is on
        .Games(cGameName).Settings.Value("rol") = 0
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = VarHidden
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = swTilt                                                  'plumb tilt
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(sw49, sw50, sw51, LeftSlingShot, RightSlingShot) 'bumpers & slingshots

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 14, 13, 12, 11, 0, 0, 0
        .InitKick BallRelease, 90, 4
        .InitEntrySnd "fx_Solenoid", "fx_Solenoid"
        .InitExitSnd SoundFX("fx_ballrel",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
        .Balls = 4
    End With

    ' Pit Road Ball Lock
    Set bsVLock = New cvpmVLock
    With bsVLock
        .InitVLock Array(sw32, sw27, sw28), Array(k32, k27, k28), Array(32, 27, 28)
        .ExitDir = 180
        .ExitForce = 0
        .CreateEvents "bsVLock"
    End With

    ' Left-Midway Eject
    Set bsL = New cvpmBallStack
    With bsL
        .InitSaucer sw29, 29, 180, 8
        .InitExitSnd SoundFX("fx_kicker",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
    End With

    ' Truck
    Set bsT = New cvpmBallStack
    With bsT
        .InitSaucer sw23, 23, 90, 20
        .InitExitSnd SoundFX("fx_kicker",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
    End With

    ' Right Drop Targets
    Set dtR = New cvpmDropTarget
    With dtR
        .InitDrop Array(sw17, sw18, sw19), Array(17, 18, 19)
        .InitSnd SoundFX("fx_droptarget",DOFContactors), SoundFX("fx_resetdrop",DOFContactors)
        .CreateEvents "dtR"
    End With

    ' Garage VUK
    Set bsC = New cvpmBallStack
    With bsC
        .InitSw 0, 52, 0, 0, 0, 0, 0, 0
        .InitKick sw52, 187, 12
        .InitExitSnd SoundFX("fx_Popper",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
    End With

    ' Track Exit Popper - Drain
    Set bsTE = New cvpmBallStack
    With bsTE
        .InitSw 0, 30, 0, 0, 0, 0, 0, 0
    End With

    ' Captive Balls
    Set cbRight = New cvpmCaptiveBall
    With cbRight
        .InitCaptive CapTrigger, CapWall, Array(CapKicker1, CapKicker2), 10
        .NailedBalls = 1
        .ForceTrans = .9
        .MinForce = 3.5
        '.CreateEvents "cbRight" 'the events are done later in the script
        .Start
    End With
    CapKicker1.CreateBall

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
    RealTime.Enabled = 1

    ' Init other dropwalls - animations
    AutoPlunger.PullBack
    AutoPlunger.MechStrength = 100
    TurnAround.IsDropped = 1:Controller.Switch(20) = 0
    OrbitPost.IsDropped = 1

	' Remove the cabinet rails if in FS mode
	If Table1.ShowDT = False then
		lrail.Visible = False
		rrail.Visible = False
	End If

	LoadLUT
End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 3:PlaySound SoundFX("fx_nudge",0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 3:PlaySound SoundFX("fx_nudge",0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 4:PlaySound SoundFX("fx_nudge",0), 0, 1, 0, 0.25
    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If
    If vpmKeyDown(keycode) Then Exit Sub
    If keycode = PlungerKey Then PlaySoundat "fx_PlungerPull",Plunger:Plunger.Pullback
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_plunger", Plunger:Plunger.Fire
End Sub

'******************
' RealTime Updates
'******************

Sub RealTime_Timer
    RollingUpdate
End Sub

' flippers top animations

Sub LeftFlipper_Animate:LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle: End Sub
Sub RightFlipper_Animate: RightFlipperTop.RotZ = RightFlipper.CurrentAngle: End Sub

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

'***********
' Solenoids
' the commented solenoids are not in used in this script
'***********

SolCallBack(1) = "SolTrough"
SolCallBack(2) = "vpmSolAutoPlunger AutoPlunger,0,"
SolCallBack(3) = "TruckExit"
SolCallBack(4) = "SolTruck"      'Truck Motor Drive 'in this table the turn around sign
SolCallBack(5) = "SolGarageDown" 'Garage Release - down animation
SolCallBack(6) = "bsC.SolOut"    ' Garage kicker
SolCallBack(7) = "SolTrackExit"  'Track Exit Popper
SolCallBack(8) = "bsL.SolOut"
'SolCallBack(9)="" 'bumper
'SolCallBack(10)="" 'bumper
'SolCallBack(11)="" 'bumper
SolCallBack(12) = "SolResetDroptargets"
SolCallBack(13) = "SolRightDivert" 'Right Ramp Diverter
SolCallBack(14) = "SolGarageUp"    'Garage Raise - up animation
'SolCallBack(17)="vpmSolSound ""lsling""," 			'left slingshot
'SolCallBack(18)="vpmSolSound ""rsling""," 			'right slingshot
SolCallback(20) = "TMag"                           'Upper Accelerator Magnet
SolCallBack(21) = "vpmSolDiverter RDiverter,True," 'Right Track Exit Diverter
SolCallBack(22) = "vpmSolDiverter LDiverter,True," 'Left Track Exit Diverter
SolCallBack(23) = "vpmSolWall OrbitPost, True,"    'Inner Orbit Post
SolCallBack(24) = "vpmSolSound ""fx_knocker"","
SolCallback(25) = "LMag"                           'Lower Accelerator Magnet Left
SolCallback(26) = "BMag"                           'Lower Accelerator Magnet Right
SolCallBack(27) = "SolPitRLeft"                    'Pit Lock Release Left
SolCallBack(28) = "SolPitRRight"                   'Pit Lock Release Right

'SolCallBack(33)="" 'Left UK Post
'SolCallBack(34)="" 'Center UK Post
'SolCallBack(35)="" 'Right UK Post

'Flashers
If UseVPMModSol Then
SolModCallback(19) = "Flasher19"                   'Upper Right Back Panel
SolModCallback(29) = "Flasher29"                   'Midway Sign (Hot Dog) In this tabke the lights of the left car
SolModCallback(30) = "Flasher30"                  'Flash Left x3
SolModCallback(31) = "Flasher31"                   'Flash Right x3
SolModCallback(32) = "Flasher32"                  'Flash Test Car x2
Else
SolCallback(19) = "vpmFlasher f19l,"
SolCallback(29) = "vpmFlasher f29l,"
SolCallback(30) = "vpmFlasher Array(f30a,f30l),"
SolCallback(31) = "vpmFlasher Array(f31a,f31l),"
SolCallback(32) = "vpmFlasher f32a,"
End If

Sub Flasher19(m): m = m /255: f19l.State = m: End Sub
Sub Flasher29(m): m = m /255: f29l.State = m: End Sub
Sub Flasher30(m): m = m /255: f30a.State = m: f30l.State = m: End Sub
Sub Flasher31(m): m = m /255: f31a.State = m: f31l.State = m: End Sub
Sub Flasher32(m): m = m /255: f32a.State = m: End Sub

' Trough
Sub SolTrough(Enabled)
    If Enabled AND bsTrough.Balls > 0 Then
        bsTrough.ExitSol_On
        vpmTimer.PulseSw 15
    End If
End Sub

' Truck ramp kicker
Sub TruckExit(Enabled)
    If Enabled AND bst.Balls > 0 Then
        bsT.ExitSol_On
    End If
End Sub

' Turn around sign
Sub SolTruck(Enabled)
    TurnAround.TimerEnabled = Enabled
End Sub

Sub TurnAround_Timer
    If TurnAround.IsDropped Then
        TurnAround.IsDropped = 0
        TurnAround.TimerInterval = 2000
        Controller.Switch(20) = 1
    Else
        TurnAround.IsDropped = 1
        TurnAround.TimerInterval = 800
        Controller.Switch(20) = 0
    End If
End Sub

Sub TurnAround_Hit:PlaySound SoundFX("fx_target",DOFContactors), 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub

' Garage Release - Down Animation

Dim CarPos, CarDir 'CarDir is both the direction and the speed of the animation
CarPos = 0
CarDir = 0

Sub SolGarageDown(Enabled)
    If Enabled Then
        Controller.Switch(39) = 1
        sw36a.IsDropped = 1
        sw36b.IsDropped = 1
        sw40.IsDropped = 0
        CarDir = -1
        GarageAnimation.Enabled = 1
    End If
End Sub

Sub SolGarageUp(Enabled)
    If Enabled Then
        Controller.Switch(39) = 0
        sw36a.IsDropped = 0
        sw36b.IsDropped = 0
        sw40.IsDropped = 1
        CarDir = 1
        GarageAnimation.Enabled = 1
    End If
End Sub

Sub GarageAnimation_Timer
    TestCar.TransY = CarPos
    CarSupport.TransZ = CarPos
    t36a.TransY = CarPos
    t36b.TransY = CarPos
    CarPos = CarPos + CarDir
    If CarPos > 0 Then
        CarPos = 0
        Me.Enabled = 0
    End If
    If CarPos < -60 Then
        CarPos = -60
        Me.Enabled = 0
    End If
End Sub

' Track Exit - drain

Sub SolTrackExit(Enabled)
    If Enabled AND bsTE.Balls > 0 Then
        bsTE.ExitSol_On
        bsTrough.AddBall 0
    End If
End Sub

Sub SolResetDropTargets(Enabled)
    If Enabled Then
        dtR.SolDropUp Enabled
    End If
End Sub

' Right Ramp Diverter
Sub SolRightDivert(Enabled)
    If Enabled Then
        diverterRamp.RotateToEnd
    Else
        diverterRamp.RotateToStart
    End If
End Sub

'**********
' Magnets
'**********

Dim M1, M2, M3
M1 = 0:M2 = 0:M3 = 0
Dim MyBall1

Sub TMag(Enabled)
    If Enabled Then
        M1 = 1
        Timer2.Enabled = 0
        Timer2.Enabled = 1
    End If
End Sub

Sub LMag(Enabled)
    If Enabled Then
        M2 = 1
        Timer3.Enabled = 0
        Timer3.Enabled = 1
    End If
End Sub

Sub BMag(Enabled)
    If Enabled Then
        M3 = 1
        Timer4.Enabled = 0
        Timer4.Enabled = 1
    End If
End Sub

Sub Timer2_Timer:M1 = 0:Timer2.Enabled = 0:End Sub
Sub Timer3_Timer:M2 = 0:Timer3.Enabled = 0:End Sub
Sub Timer4_Timer:M3 = 0:Timer4.Enabled = 0:End Sub

Sub TopMagnet_unHit
    If M1 = 1 Then
        Set MyBall1 = ActiveBall
        If MyBall1.VelY < 0 Then
            MyBall1.VelY = MyBall1.VelY * 4.5
            If MyBall1.VelY < -95 Then
                MyBall1.VelY = -95
            Else
                If MyBall1.VelY > -75 Then MyBall1.VelY = -75
            End If
        End If
    End If
End Sub

Sub BottomMagnet_unHit
    If M3 = 1 Then
        Set MyBall1 = ActiveBall
        If MyBall1.VelY > 0 Then
            MyBall1.VelY = MyBall1.VelY * 4.5
            If MyBall1.VelY > 95 Then
                MyBall1.VelY = 95
            Else
                If MyBall1.VelY < 45 Then MyBall1.VelY = 45
                If MyBall1.VelX < 45 Then MyBall1.VelX = 45
            End If
        End If
    End If
End Sub

Sub LeftMagnet_unHit
    If M2 = 1 Then
        Set MyBall1 = ActiveBall
        If MyBall1.VelY > 0 Then
            MyBall1.VelY = MyBall1.VelY * 4.5
            If MyBall1.VelY > 95 Then
                MyBall1.VelY = 95
            Else
                If MyBall1.VelY < 65 Then MyBall1.VelY = 65
            End If
        End If
    End If
End Sub

' Pit Lock Release

Sub SolPitRLeft(Enabled)
    If Enabled Then
        LRL.IsDropped = 1:PlaySoundAt SoundFX("fx_Solenoid",DOFContactors), k32
        LRL.TimerEnabled = 1
        bsVLock.SolExit True
    End If
End Sub

Sub LRL_Timer
    LRL.IsDropped = 0:PlaySoundAt SoundFX("fx_Solenoidoff",DOFContactors),k32
    LRL.TimerEnabled = 0
    bsVLock.SolExit False
End Sub

Sub SolPitRRight(Enabled)
    If Enabled Then
        LRR.IsDropped = 1:PlaySoundAt SoundFX("fx_Solenoid",DOFContactors),k32
        LRR.TimerEnabled = 1
        bsVLock.SolExit True
    End If
End Sub

Sub LRR_Timer
    LRR.IsDropped = 0:PlaySoundAt SoundFX("fx_Solenoidoff",DOFContactors), k32
    LRR.TimerEnabled = 0
    bsVLock.SolExit False
End Sub

' Captive Ball Right - done here to add the ball hit sound.
Sub CapTrigger_Hit:cbRight.TrigHit ActiveBall:End Sub
Sub CapTrigger_UnHit:cbRight.TrigHit 0:End Sub
Sub CapWall_Hit:PlaySoundAt "fx_collide",CapKicker1:cbRight.BallHit ActiveBall:End Sub
Sub CapKicker2_Hit:cbRight.BallReturn Me:End Sub

'***************
'  Slingshots
'***************
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    vpmTimer.PulseSw 59
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
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
    vpmTimer.PulseSw 62
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
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

'***************
'   Bumpers
'***************

Sub sw49_Hit:vpmTimer.PulseSw 49:PlaySoundAt SoundFX("fx_bumper", DOFContactors), sw49:End Sub
Sub sw50_Hit:vpmTimer.PulseSw 50:PlaySoundAt SoundFX("fx_bumper", DOFContactors), sw50:End Sub
Sub sw51_Hit:vpmTimer.PulseSw 51:PlaySoundAt SoundFX("fx_bumper", DOFContactors), sw51:End Sub

'*********************
' Switches & Rollovers
'*********************

Sub sw16_Hit:Controller.Switch(16) = 1:PlaySoundAt "fx_sensor",sw16:End Sub
Sub sw16_unHit:Controller.Switch(16) = 0:End Sub

Sub sw21_Hit:Controller.Switch(21) = 1:End Sub
Sub sw21_unHit:Controller.Switch(21) = 0:End Sub

Sub sw24_Hit:vpmTimer.PulseSw 24:PlaySoundAt "fx_gate", sw24:End Sub

Sub sw25_Hit:Controller.Switch(25) = 1:PlaySoundAt "fx_sensor",sw25:End Sub
Sub sw25_unHit:Controller.Switch(25) = 0:End Sub

Sub sw26_Hit:Controller.Switch(26) = 1:PlaySoundAt "fx_sensor",sw26:End Sub
Sub sw26_unHit:Controller.Switch(26) = 0:End Sub

Sub sw57_Hit:Controller.Switch(57) = 1:PlaySoundAt "fx_sensor",sw57:End Sub
Sub sw57_unHit:Controller.Switch(57) = 0:End Sub

Sub sw58_Hit:Controller.Switch(58) = 1:PlaySoundAt "fx_sensor",sw58:End Sub
Sub sw58_unHit:Controller.Switch(58) = 0:End Sub

Sub sw60_Hit:Controller.Switch(60) = 1:PlaySoundAt "fx_sensor",sw60:End Sub
Sub sw60_unHit:Controller.Switch(60) = 0:End Sub

Sub sw61_Hit:Controller.Switch(61) = 1:PlaySoundAt "fx_sensor",sw61:End Sub
Sub sw61_unHit:Controller.Switch(61) = 0:End Sub

'magnets optos
Sub sw34_Hit:vpmTimer.PulseSw 34:End Sub
Sub sw35_Hit:vpmTimer.PulseSw 35:End Sub
Sub sw37_Hit:vpmTimer.PulseSw 37:End Sub
Sub sw47_Hit:vpmTimer.PulseSw 47:End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:End Sub

'****************************
' Drain holes, vuks & saucers
'****************************

Sub Drain_Hit:PlaysoundAt "fx_drain",drain:bsTE.AddBall Me:End Sub
Sub sw23_Hit:PlaySoundAt "fx_kicker_enter", sw23:bsT.AddBall 0:End Sub
Sub sw29_Hit:PlaySoundAt "fx_kicker_enter", sw29:bsL.AddBall 0:End Sub
Sub sw52_Hit:PlaySoundAt "fx_hole_enter",sw52:bsC.AddBall Me:End Sub
Sub sw52t_Hit:CarShake:End Sub
Sub dropsoundtrigger_Hit: PLaySoundAt"fx_hole_enter", dropsoundtrigger: End Sub

'***************
'  Targets
'***************

Sub sw36a_Hit
    vpmTimer.PulseSw 36
    PlaySoundAtBall SoundFX("fx_target", DOFTargets)
    CarShake
End Sub

Sub sw36b_Hit
    vpmTimer.PulseSw 36
    PlaySoundAtBall SoundFX("fx_target", DOFTargets)
    CarShake
End Sub

Sub sw38_Hit
    vpmTimer.PulseSw 38
    PlaySoundAtBall SoundFX("fx_target", DOFTargets)
    t38.x = 825
    t38.y = 1119
    Me.TimerEnabled = 1
End Sub

Sub sw38_Timer
    Me.TimerEnabled = 0
    t38.x = 822
    t38.y = 1123
End Sub

Sub sw40_Hit
    vpmTimer.PulseSw 40
    PlaySoundAtBall SoundFX("fx_target", DOFTargets)
    CarShake
End Sub

Sub sw43_Hit
    vpmTimer.PulseSw 43
    PlaySoundAtBall SoundFX("fx_target", DOFTargets)
    t43.x = 365
    t43.y = 711
    Me.TimerEnabled = 1
End Sub

Sub sw43_Timer
    Me.TimerEnabled = 0
    t43.x = 366
    t43.y = 716
End Sub

Sub sw44_Hit
    vpmTimer.PulseSw 44
    PlaySoundAtBall SoundFX("fx_target", DOFTargets)
    t44.x = 231
    t44.y = 748
    Me.TimerEnabled = 1
End Sub

Sub sw44_Timer
    Me.TimerEnabled = 0
    t44.x = 232
    t44.y = 753
End Sub

Sub sw45_Hit
    vpmTimer.PulseSw 45
    PlaySoundAtBall SoundFX("fx_target", DOFTargets)
    t45.x = 194
    t45.y = 1131
    Me.TimerEnabled = 1
End Sub

Sub sw45_Timer
    Me.TimerEnabled = 0
    t45.x = 195
    t45.y = 1136
End Sub

'************
' Spinners
'************

Sub sw33_Spin:vpmTimer.PulseSw 33:PlaySoundAt "fx_spinner", sw33:End Sub
Sub sw42_Spin:vpmTimer.PulseSw 42:PlaySoundAt "fx_spinner",sw42:End Sub

'************************
' Diverter animation
'************************

Sub SolDiv(Enabled)
    If Enabled Then
        DiverterDir = 1
    Else
        DiverterDir = -1
    End If

    Diverter.Enabled = 0
    If DiverterPos < 1 Then DiverterPos = 1
    If DiverterPos > 5 Then DiverterPos = 5

    Diverter.Enabled = 1
End Sub

Sub Diverter_Timer()
    Select Case DiverterPos
        Case 0:Diverter1.IsDropped = 0:Diverter2.IsDropped = 1:Diverter.Enabled = 0
        Case 1:Diverter2.IsDropped = 0:Diverter1.IsDropped = 1:Diverter3.IsDropped = 1
        Case 2:Diverter3.IsDropped = 0:Diverter2.IsDropped = 1:Diverter4.IsDropped = 1
        Case 3:Diverter4.IsDropped = 0:Diverter3.IsDropped = 1:Diverter5.IsDropped = 1
        Case 4:Diverter5.IsDropped = 0:Diverter4.IsDropped = 1:Diverter6.IsDropped = 1
        Case 5:Diverter6.IsDropped = 0:Diverter5.IsDropped = 1
        Case 6:Diverter.Enabled = 0
    End Select
    DiverterPos = DiverterPos + DiverterDir
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
    UpdateLamps
End Sub

Sub UpdateLamps
    Lamp 1, l1
    Lamp 2, l2
    Lamp 3, l3
    Lamp 4, l4
    Lamp 5, l5
    Lamp 6, l6
    Lampm 7, l7b
    Lamp 7, l7
    Lampm 8, l8a
    Lamp 8, l8b
    Lamp 9, l9
    Lamp 10, l10
    Lamp 11, l11
    Lamp 12, l12
    Lamp 13, l13
    Lamp 14, l14
    Lampm 15, l15b
    Lamp 15, l15
    Lamp 16, l16
    Lamp 17, l17
    Lamp 18, l18
    Lamp 19, l19
    Lamp 20, l20
    Lamp 21, l21
    Lamp 22, l22
    Lampm 23, l23b
    Lamp 23, l23
    Lamp 24, l24
    Lamp 25, l25
    Lamp 26, l26
    Lamp 27, l27
    Lamp 28, l28
    Lamp 29, l29
    Lamp 30, l30
    Lamp 31, l31
    Flashm 32, f32big
    Flash 32, l32
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
    Lamp 49, l49
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
    Lampm 60, l60a
    Lamp 60, l60b
    Lamp 61, l61
    Lamp 62, l62
    Lamp 63, l63
    Lamp 64, l64
    Lamp 65, l65
    Lamp 66, l66
    Lamp 67, l67
    Lamp 68, l68
    Lamp 69, l69
    Lamp 70, l70
    Lamp 71, l71
    Lamp 72, l72
    Lamp 73, l73
    Lamp 74, l74
    Lamp 75, l75
    Lamp 76, l76
    Lamp 77, l77
    Lamp 78, l78
    'Lamp 79, l79 'Tournament Light
    Lamp 80, l80 'Start Button
End Sub

' div lamp subs

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
    LampTimer.Interval = 10
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

'*************************
' GI - needs new vpinmame
'*************************

Set GICallback = GetRef("GIUpdate")

Sub GIUpdate(no, Enabled)
Dim obj
For each obj in aGILights
	obj.State = ABS(Enabled)
Next
End Sub

'****************************
'     Test Car Shake
'Inspired by koadic's code
'   from the CV table
' I know it doesn't look
' like Koadic's code but I
' got the idea from his code :)
'****************************

Dim cBall
Const cMod = .65 'percentage of hit power transfered to the car

CarInit

Sub CarShake
    cball.velx = activeball.velx * cMod
    cball.vely = activeball.vely * cMod
    aCarTimer.enabled = True
    bCarTimer.enabled = True
End Sub

Sub CarInit
    Set cBall = hball.createball
    hball.Kick 0, 0
    cball.Mass = 1.6
End Sub

Sub aCarTimer_Timer             'start animation
    Dim x, y
    x = (hball.x - cball.x) / 4 'reduce the X axis movement
    y = (hball.y - cball.y)
    TestCar.transz = x
    TestCar.transx = - y
    t36a.transz = x
    t36a.transx = - y
    t36b.transz = x
    t36b.transx = - y
    CarSupport.transy = - y
    CarSupport.transx = x
End Sub

Sub bCarTimer_Timer 'stop animation
    TestCar.transz = 0
    TestCar.transx = 0
    t36a.transz = 0
    t36a.transx = 0
    t36b.transz = 0
    t36b.transx = 0
    CarSupport.transy = 0
    CarSupport.transx = 0
    aCarTimer.enabled = False
    bCarTimer.enabled = False
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v4.0
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
Const lob = 3     'number of locked balls
Const maxvel = 45 'max ball velocity
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