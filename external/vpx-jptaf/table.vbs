'The Addams Family / IPD No. 20 / March, 1992 / 4 Players
'Midway Manufacturing Company, a subsidiary of WMS Industries, Incorporated,
'VPX8 by jpsalas 2024, v 5.5.0
'parts of the script from Sliderpoint's table, like the Thing animation.

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim bsTrough, bsSwamp, bsSwampKick, bsChair
Dim mMagnetL, mMagnetT, mMagnetR, BookMech, ThingMech
Dim ThingMagnetOn, ThingBallInBox
Dim PlungerIM, x

'Choose the rom you prefer

Const cGameName = "taf_l7" ' TAF  L-6  (Last Official Release)
'Const cGameName  = "taf_l5"   ' TAF  L-5
'Const cGameName  = "taf_l4"   ' TAF  L-4
'Const cGameName  = "taf_l3"   ' TAF  L-3
'Const cGameName  = "taf_l2"   ' TAF  L-2
'Const cGameName  = "taf_l1"   ' TAF  L-1  (First Production Release)
'Const cGameName  = "taf_l7"   ' TAF  L-7  (Unofficial Newer Release)
'Const cGameName  = "taf_p2"   ' TAF  P-2  (Prototype)
'Const cGameName  = "taf_h4"   ' TAF  H-4  (Home Rom)
'Const cGameName = "tafg_lx3" ' TAFG LX-3 (Production Release)
'Const cGameName = "tafg_h3"  ' TAFG H-3  (Home Rom)

Dim VarHidden, UseVPMColoredDMD
If Table1.ShowDT = true then
    UseVPMColoredDMD = true
    VarHidden = 1
    table1.BloomStrength = 1.3
Else
    UseVPMColoredDMD = False
    VarHidden = 0
    table1.BloomStrength = 0.5
End If

Const UseVPMModSol = True
LoadVPM "01120100", "WPC.VBS", 3.26

'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 1
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
        .SplashInfoLine = "JP's Addams Family - Williams 1992" & vbNewLine & "VPX7 table by JPSalas 5.5.0"
        .Games(cGameName).Settings.Value("sound") = 1
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = VarHidden
        .Games(cGameName).Settings.Value("rol") = 0
        .Games(cGameName).Settings.Value("samples") = 0 ' Make sure samples are disabled in Vpinmame
        '.SetDisplayPosition 0,0,GetPlayerHWnd 'uncomment if you can't see the dmd
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With

    ' Impulse Plunger
    Const IMPowerSetting = 36 ' Plunger Power
    Const IMTime = 0.5        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP SwPlunger, IMPowerSetting, IMTime
        .Random 0.5
        .Switch 27
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper001, Bumper002, Bumper003, Bumper004, Bumper005, LeftSlingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .Initsw 18, 17, 16, 15, 0, 0, 0, 0
        .InitKick BallRelease, 90, 4
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .InitEntrySnd SoundFX("fx_drain", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .CreateEvents "bsTrough", drain
        .Balls = 3
        .IsTrough = True
    End With

    ' Swamp Lock
    Set bsSwamp = New cvpmBallStack
    With bsSwamp
        .Initsw 0, 73, 72, 71, 0, 0, 0, 0
        .CreateEvents "bsSwamp", sw73
    End With

    ' Swamp Kickout
    Set bsSwampKick = New cvpmBallStack
    With bsSwampKick
        .Initsw 0, 74, 0, 0, 0, 0, 0, 0
        .InitKick sw74, 210, 10
        .KickAngleVar = 2
        .KickForceVar = 3
        .InitExitSnd SoundFX("fx_popper", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    End With

    ' Electric Chair
    Set bsChair = New cvpmBallStack
    With bsChair
        .Initsw 0, 43, 0, 0, 0, 0, 0, 0
        .InitKick sw43a, 181, 12
        .InitExitSnd SoundFX("fx_popper", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    End With

    ' The Power Magnet (Left)
    Set mMagnetL = New cvpmMagnet
    With mMagnetL
        .InitMagnet Magnet2, 9
        .GrabCenter = False
        .Solenoid = 16
        .CreateEvents "mMagnetL"
    End With

    ' The Power Magnet (Top)
    Set mMagnetT = New cvpmMagnet
    With mMagnetT
        .InitMagnet Magnet1, 9
        .GrabCenter = False
        .Solenoid = 23
        .CreateEvents "mMagnetT"
    End With

    ' The Power Magnet (Right)
    Set mMagnetR = New cvpmMagnet
    With mMagnetR
        .InitMagnet Magnet3, 9
        .GrabCenter = False
        .Solenoid = 24
        .CreateEvents "mMagnetR"
    End With

    'Bookcase Mech Handler
    Set BookMech = New cvpmMech
    With BookMech
        .Sol1 = 27
        .Length = 100
        .Steps = 90
        .MType = vpmMechOneSol + vpmMechReverse + vpmMechNonLinear
        .AddSw 81, 89, 90 ' Bookcase Open
        .AddSw 82, 0, 1   ' Bookcase Close
        .Callback = GetRef("BookCaseMotor")
        .Start
    End With

    'Thing Mech Handler
    Set ThingMech = New cvpmMech
    With ThingMech
        .Sol1 = 25
        .Length = 175
        .Steps = 60
        .MType = vpmMechOneSol + vpmMechReverse + vpmMechNonLinear
        .AddSw 85, 59, 60 ' Thing Up Opto
        .AddSw 84, 0, 1   ' Thing Down Opto
        .Callback = GetRef("ThingMotor")
        .Start
    End With

    vpmMapLights aLights

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Misc. Initialization
    Controller.Switch(22) = True  ' coin door closed
    Controller.Switch(23) = False ' ticket opto
    Controller.Switch(24) = False ' always closed

    LoadLUT
End Sub

'Solenoid calls
SolCallback(1) = "bsChair.SolOut"                                  'Chair Kickout
SolCallback(2) = "vpmSolSound SoundFX(""fx_Knocker"",DOFKnocker)," 'Thing Knocker
SolCallback(3) = "vpmSolDiverter Diverter, ""fx_diverter"","       'Ramp Diverter
SolCallback(4) = "bsTrough.SolOut"                                 'Ball Release
SolCallBack(5) = "bsTrough.SolIn"                                  'Outhole
SolCallback(6) = "ThingMagnet"                                     'Thing Magnet
SolCallback(7) = "ThingKickout"                                    'Thing Kickout
SolCallback(8) = "bsSwampKick.SolOut"                              'Lockup Kickout
'SolCallback(9)= 															'Upper Left Jet
'SolCallback(10)= 															'Upper Right Jet
'SolCallback(11)=															'Center Left Jet
'SolCallBack(12)=			  												'Center Right Jet
'SolCallback(13)= 															'Lower Jet
'SolCallback(14)= 															'Left Slingshot
'SolCallback(15)= 															'Right Slingshot
'SolCallback(16)=		 													'Left Magnet
'SolCallback(23)=															'Upper Magnet
'SolCallback(24)=															'Right Magnet
'SolCallback(25)=															'Thing Motor
SolCallback(26) = "ThingEject" 'Thing Eject Hole - flasher
'SolCallback(27)=															'Bookcase Motor
SolCallback(28) = "SwampRelease" 'Swamp Release

SolModCallback(17) = "Flasher17"
SolModCallback(18) = "Flasher18"
SolModCallback(19) = "Flasher19"
SolModCallback(20) = "Flasher20"
SolModCallback(21) = "Flasher21"
SolModCallback(22) = "Flasher22"

Sub Flasher17(m): m = m /255: f17a.State = m: f17b.State = m: f17d.State = m: f17e.State = m: End Sub
Sub Flasher18(m): m = m /255: f18a.State = m: f18b.State = m: f18d.State = m: f18e.State = m: End Sub
Sub Flasher19(m): m = m /255: f19a.State = m: f19b.State = m: f19d.State = m: f19e.State = m: End Sub
Sub Flasher20(m): m = m /255: f20.State = m: f20a.State = m: f20b.State = m: End Sub
Sub Flasher21(m): m = m /255: f21.State = m: f21a.State = m: f21b.State = m: f21c.State = m: End Sub
Sub Flasher22(m): m = m /255: f22.State = m: End Sub

' Solenoid calls

Sub SwampRelease(enabled)
    If enabled Then
        If bsSwamp.balls > 0 Then
            bsSwamp.SolOut 1
            bsSwampKick.AddBall 1
        Else
            bsSwamp.SolOut 1
        End If
    End If
End Sub

Sub BookCaseMotor(aNewPos, aSpeed, aLastPos)
    DIM OBJ
    PlaySoundAt SoundFX("fx_Motor2", DOFGear), Bookcase
    Bookcase.rotZ = aNewPos
    'BCPlastic.rotZ = aNewPos 'beetle
    BCScrews.rotZ = aNewPos
    BCRubbers.rotZ = aNewPos
    BCPegs.rotZ = aNewPos
    if aNewPos > 60 then
        for each obj in aBookcaseOpen
            obj.IsDropped = False
        next
        for each obj in aBookcaseClosed
            obj.IsDropped = True
        next
    else
        for each obj in aBookcaseOpen
            obj.IsDropped = True
        next
        for each obj in aBookcaseClosed
            obj.IsDropped = False
        next
    end if
End Sub

Dim Position, HandPosition, Thingball

Sub ThingMotor(aNewPos, aSpeed, aLastPos)
    dim BoxPosition
    PlaySoundAt SoundFX("fx_Motor", DOFGear), Thing
    position = aNewPos * 2 - 90
    if aNewPos < 3 then
        BoxPosition = aNewPos
    elseif aNewPos < 33 then
        BoxPosition = aNewPos * 1.25
    elseif aNewPos <= 34 then
        BoxPosition = aNewPos * 1.2
    elseif aNewPos <= 35 then
        BoxPosition = aNewPos * 1.14
    elseif aNewPos <= 36 then
        BoxPosition = aNewPos * 1.1
    elseif aNewPos <= 37 then
        BoxPosition = aNewPos * 1.055
    elseif aNewPos <= 38 then
        BoxPosition = aNewPos * 1
    elseif aNewPos <= 39 then
        BoxPosition = aNewPos * .965
    elseif aNewPos <= 40 then
        BoxPosition = aNewPos * .925
    elseif aNewPos < 50 then
        BoxPosition = 34
    elseif aNewPos <= 51 then
        BoxPosition = aNewPos * .685
    elseif aNewPos <= 52 then
        BoxPosition = aNewPos * .67
    elseif aNewPos <= 53 then
        BoxPosition = aNewPos * .655
    elseif aNewPos <= 54 then
        BoxPosition = aNewPos * .64
    elseif aNewPos <= 55 then
        BoxPosition = aNewPos * .625
    elseif aNewPos <= 56 then
        BoxPosition = aNewPos * .61
    elseif aNewPos <= 57 then
        BoxPosition = aNewPos * .59
    elseif aNewPos <= 58 then
        BoxPosition = aNewPos * .56
    elseif aNewPos <= 59 then
        BoxPosition = aNewPos * .55
    elseif aNewPos <= 60 then
        BoxPosition = aNewPos * .53
    else
        BoxPosition = 32
    end if

    Thing.RotY = position
    ThingMag.RotY = position
    ThingMagNut.RotY = position
    if ThingBall then
        ThingBallprim.RotY = -1 * (position + 120)
    else
        ThingBallprim.RotY = 0
    end if
    thingBox.Rotx = BoxPosition '-90
End Sub

Sub ThingMagnet(Enabled)
    If Enabled = true and Position > 25 Then
        ThingBall = true
        sw87.destroyball
        Controller.Switch(87) = 0
        PlaysoundAt "fx_metal", sw87
    End If
    If enabled = false and position < -85 and ThingBall = true then
        ThingBall = false
        Controller.Switch(77) = 1 'Thing KickOut
    End If
    If enabled = false and position > 25 then
        ThingBall = false
        sw87.createball
        Controller.switch(87) = 1
    End If
End Sub

Sub ThingKickOut(Enabled)
    If Enabled Then
        PlaySoundAt "fx_kicker", ThingBallPrim
        Controller.switch(77) = 0 'Thing KickOut
        PlaysoundAt "fx_subway", RightFlipper001
        vpmTimer.Pulseswitch 68, 1800, "bsSwamp.Addball"
    End If
End Sub

Sub ThingEject(Enabled) 'ThingSaucer
    If Enabled Then
        sw87.Kick 90, 10
        PlaySoundAt SoundFX("fx_kicker", DOFContactors), sw87
        Controller.Switch(87) = 0
    End If
End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = LeftMagnaSave Then bLutActive = True:SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If
    If vpmKeyDown(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundat "fx_PlungerPull", Plunger:Plunger.Pullback
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = LeftMagnaSave Then bLutActive = False:HideLUT
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_plunger", Plunger:Plunger.Fire
End Sub

'*******************
'  Flipper Subs
'*******************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sURFlipper) = "SolURFlipper"
SolCallback(sULFlipper) = "SolULFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipperOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipperOn = 1
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

Sub SolULFlipper(Enabled)
    If Enabled Then
        LeftFlipper001.RotateToEnd
    Else
        LeftFlipper001.RotateToStart
    End If
End Sub

Sub SolURFlipper(Enabled)
    If Enabled Then
        RightFlipper001.RotateToEnd
    Else
        RightFlipper001.RotateToStart
    End If
End Sub

' flippers top animations

Sub LeftFlipper_Animate:LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle: End Sub
Sub RightFlipper_Animate: RightFlipperTop.RotZ = RightFlipper.CurrentAngle: End Sub
Sub RightFlipper001_Animate: RightFlipperTop001.RotZ = RightFlipper001.CurrentAngle: End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper001_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper001_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
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
FlipperElasticity = 0.85
FullStrokeEOS_Torque = 0.3 ' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.2 ' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

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
    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower:End If

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
    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower:End If

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
    LUBack.imagea = "PostItNote"
    String = CL(String)
    For xFor = 1 to 40
        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
    SetLUTLine ""
    LUBack.imagea = "PostitBL"
End Sub

Function CL(NumString) 'center line
    Dim Temp, TempStr
    If Len(NumString) > 40 Then NumString = Left(NumString, 40)
    Temp = (40 - Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Dim GiIntensity
GiIntensity = 1               'can be used by the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

'*************************
' GI - needs new vpinmame
'*************************

Set GICallback = GetRef("GIUpdate")

Sub GIUpdate(no, Enabled)
    If Enabled Then
        GiOn
    Else
        GiOff
    End If
End Sub

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
    vpmTimer.PulseSw 36
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
    vpmTimer.PulseSw 37
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
Sub Bumper001_Hit:vpmTimer.PulseSw 35:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper001:End Sub
Sub Bumper002_Hit:vpmTimer.PulseSw 33:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper002:End Sub
Sub Bumper003_Hit:vpmTimer.PulseSw 34:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper003:End Sub
Sub Bumper004_Hit:vpmTimer.PulseSw 32:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper004:End Sub
Sub Bumper005_Hit:vpmTimer.PulseSw 31:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper005:End Sub

' Holes
Sub sw43_Hit:PlaysoundAt "fx_hole_enter", sw43:bsChair.AddBall Me:End Sub
Sub sw73a_Hit:bsSwamp.AddBall Me:End Sub
Sub sw87_Hit:PlaysoundAt "fx_kicker_enter", sw87:Controller.Switch(87) = 1:End Sub
Sub Swamp_Hit:PlaysoundAt "fx_hole_enter", swamp:End Sub

'The Vault
Sub sw68_Hit()
    sw68.DestroyBall
    PlaysoundAt "fx_hole_enter", sw68
    PlaysoundAt "fx_subway", RightFlipper001
    vpmTimer.Pulseswitch 68, 1800, "bsSwamp.Addball"
End Sub

' Rollovers & triggers
Sub sw75_Hit:Controller.Switch(75) = 1:PlaySoundAt "fx_sensor", sw75:End Sub
Sub sw75_UnHit:Controller.Switch(75) = 0:End Sub

Sub sw78_Hit:Controller.Switch(78) = 1:PlaySoundAt "fx_sensor", sw78:End Sub
Sub sw78_UnHit:Controller.Switch(78) = 0:End Sub

Sub sw76_Hit:Controller.Switch(76) = 1:PlaySoundAt "fx_sensor", sw76:End Sub
Sub sw76_UnHit:Controller.Switch(76) = 0:End Sub

Sub sw25_Hit:Controller.Switch(25) = 1:PlaySoundAt "fx_sensor", sw25:End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub

Sub sw26_Hit:Controller.Switch(26) = 1:PlaySoundAt "fx_sensor", sw26:End Sub
Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub

Sub sw27_Hit:Controller.Switch(27) = 1:PlaySoundAt "fx_sensor", sw27:End Sub
Sub sw27_UnHit:Controller.Switch(27) = 0:End Sub

Sub sw51_Hit:Controller.Switch(51) = 1:PlaySoundAt "fx_sensor", sw51:End Sub
Sub sw51_UnHit:Controller.Switch(51) = 0:End Sub

Sub sw57_Hit:Controller.Switch(57) = 1:PlaySoundAt "fx_sensor", sw57:End Sub
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub

Sub sw38_Hit:Controller.Switch(38) = 1:PlaySoundAt "fx_sensor", sw38:End Sub
Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub

Sub sw63_Hit:Controller.Switch(63) = 1:PlaySoundAt "fx_sensor", sw63:End Sub
Sub sw63_UnHit:Controller.Switch(63) = 0:End Sub

Sub sw67_Hit:Controller.Switch(67) = 1:PlaySoundAt "fx_sensor", sw67:End Sub
Sub sw67_UnHit:Controller.Switch(67) = 0:End Sub

Sub sw64_Hit:Controller.Switch(64) = 1:PlaySoundAt "fx_sensor", sw64:diode005.duration 2,1000,0:diode006.duration 2,1000,0:End Sub
Sub sw64_UnHit:Controller.Switch(64) = 0:End Sub

Sub sw61_Hit:Controller.Switch(61) = 1:PlaySoundAt "fx_sensor", sw61:End Sub
Sub sw61_UnHit:Controller.Switch(61) = 0:End Sub

Sub sw65_Hit:Controller.Switch(65) = 1:PlaySoundAt "fx_sensor", sw65:End Sub
Sub sw65_UnHit:Controller.Switch(65) = 0:End Sub

Sub sw66_Hit:Controller.Switch(66) = 1:PlaySoundAt "fx_sensor", sw66:End Sub
Sub sw66_UnHit:Controller.Switch(66) = 0:End Sub

Sub sw58_Hit:Controller.Switch(58) = 1:PlaySoundAt "fx_sensor", sw58:End Sub
Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub

Sub sw53_Hit:Controller.Switch(53) = 1:PlaySoundAt "fx_sensor", sw53:End Sub
Sub sw53_UnHit:Controller.Switch(53) = 0:End Sub
Sub sw54_Hit:Controller.Switch(54) = 1:PlaySoundAt "fx_sensor", sw54:End Sub
Sub sw54_UnHit:Controller.Switch(54) = 0:End Sub
Sub sw55_Hit:Controller.Switch(55) = 1:PlaySoundAt "fx_sensor", sw55:End Sub
Sub sw55_UnHit:Controller.Switch(55) = 0:End Sub
Sub sw56_Hit:Controller.Switch(56) = 1:PlaySoundAt "fx_sensor", sw56:End Sub
Sub sw56_UnHit:Controller.Switch(56) = 0:End Sub

'Targets
Sub sw41_Hit:vpmTimer.PulseSw 41:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw42_Hit:vpmTimer.PulseSw 42:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw44a_Hit:vpmTimer.PulseSw 44:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw44b_Hit:vpmTimer.PulseSw 44:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw44c_Hit:vpmTimer.PulseSw 44:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw44d_Hit:vpmTimer.PulseSw 44:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw45_Hit:vpmTimer.PulseSw 45:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw47_Hit:vpmTimer.PulseSw 47:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw62_Hit:vpmTimer.PulseSw 62:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw86_Hit:vpmTimer.PulseSw 86:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub

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

'********************************
'   JP's VPX8 Rolling Sounds
'********************************

Const tnob = 19   'total number of balls
Const lob = 0     'number of locked balls
Const maxvel = 35 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
    RollingTimer.Enabled = 1
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
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