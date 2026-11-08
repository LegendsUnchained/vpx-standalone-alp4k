' Skateboard - Inder 1980
' IPD No. 4479 / 1980 / 4 Players                                                                Reskin for Atari 2600 by Iceman 2022
' VPX by jpsalas, akiles50000 and mfuegemman                                                           Not for Sale for Fans Thanks to JPSalas and more  

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01560000", "inder_skateboard.vbs", 3.2

Dim bsTrough, dtbank1, bsTopHole, Flipperactive, x

Const cGameName = "skatebrd"

' Free play through automatic credit followed by the normal ROM Start input.
Const FreePlay = True
Dim FreePlayStartPending, FreePlayStartHeld


Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0   'set it to 1 if the tables DMD runs too fast
Const HandleMech = 0

Dim vpmhidden
If Table1.ShowDT = true then
	vpmhidden = 1 'hide the vpinmame window
    For each x in aReels
        x.Visible = 1
    Next
else
	vpmhidden = 0
    For each x in aReels
        x.Visible = 0
    Next
end if

If B2SOn Then vpmhidden = 1

' Standard Sounds
Const SSolenoidOn = "fx_SolenoidOn"
Const SSolenoidOff = "fx_SolenoidOff"
Const SCoin = "fx_Coin"

'************************************
' Game timer for real time updates
'************************************

Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates
    RollingUpdate
'	LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
'	RightFlipperTop.RotZ = RightFlipper.CurrentAngle
End Sub

'************
' Table init.
'************

Sub table1_Init
    vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Cherry Coke (TBA 2020)" & vbNewLine & "VPX table by IVANTBA" & "Reskin by Iceman 2022"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 0
        .Games(cGameName).Settings.Value("sound") = 0 '1= rotated display, 0= normal
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 56
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 57, 0, 0, 0, 0, 0, 0
        .InitKick BallRelease, 90, 4
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 1
    End With

    ' Drop targets
    set dtbank1 = new cvpmdroptarget
    dtbank1.InitDrop Array(sw80, sw81, sw82, sw83, sw84), Array(80, 81, 82, 83, 84)
    dtbank1.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtbank1.CreateEvents "dtbank1"

    'Top Hole
    set bsTopHole = new cvpmSaucer
    bsTopHole.InitKicker TopHole, 67, 0, 15, 0
    bsTopHole.InitExitVariance 5, 2
    bsTopHole.InitSounds SoundFX("fx_kicker_enter", DOFContactors), SoundFX("fx_solenoidon", DOFContactors), SoundFX("fx_kicker", DOFContactors)

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Turn on Gi
    vpmtimer.addtimer 2000, "GiOn '"
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.stop:End Sub

'**********
' Music
'**********


'Start the music

DIM music

music = "Atari 2600 Intro engl.mp3"
PlayMusic "Atari 2600 Arcade Remember.mp3" 

Sub Table1_MusicDone()
PlayMusic "Atari 2600 Arcade Remember.mp3"
End Sub 

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If FreePlay And keycode = StartGameKey Then
        If Not FreePlayStartHeld And Not FreePlayStartPending Then
            FreePlayStartPending = True
            Call vpmKeyDown(keyInsertCoin1)
            vpmTimer.AddTimer 100, "FreePlayCoinUp'"
            ' Allow the core coin delay and ROM credit processing before Start.
            vpmTimer.AddTimer 1500, "FreePlayStartDown'"
        End If
        FreePlayStartHeld = True
        Exit Sub
    End If
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then PlaySoundat "fx_PlungerPull", Plunger:Plunger.Pullback
    If KeyCode = LeftFlipperKey AND FlipperActive Then SolLFlipper 1
    If KeyCode = RightFlipperKey AND FlipperActive Then SolRFlipper 1
    If vpmKeyDown(keycode)Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If FreePlay And keycode = StartGameKey Then
        FreePlayStartHeld = False
        Exit Sub
    End If
    If KeyCode = LeftFlipperKey AND FlipperActive Then SolLFlipper 0
    If KeyCode = RightFlipperKey AND FlipperActive Then SolRFlipper 0
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_plunger", Plunger:Plunger.Fire
End Sub

Sub FreePlayCoinUp()
    Call vpmKeyUp(keyInsertCoin1)
End Sub

Sub FreePlayStartDown()
    Call vpmKeyDown(StartGameKey)
    vpmTimer.AddTimer 200, "FreePlayStartUp'"
End Sub

Sub FreePlayStartUp()
    Call vpmKeyUp(StartGameKey)
    FreePlayStartPending = False
End Sub

'*********
' Switches
'*********

' Slings
Dim RStep

Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk
    DOF 102, DOFPulse
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 63
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

' Scoring rubbers
Sub RubberBand001_Hit:vpmTimer.PulseSw 66:End Sub
Sub RubberBand006_Hit:vpmTimer.PulseSw 66:End Sub
Sub RubberBand007_Hit:vpmTimer.PulseSw 66:End Sub
Sub RubberBand008_Hit:vpmTimer.PulseSw 66:End Sub
Sub RubberBand009_Hit:vpmTimer.PulseSw 66:End Sub
Sub RubberBand010_Hit:vpmTimer.PulseSw 66:End Sub
Sub RubberBand011_Hit:vpmTimer.PulseSw 66:End Sub
Sub RubberBand012_Hit:vpmTimer.PulseSw 66:End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 60:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper1:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 61:PlaySoundAt SoundFX("fx_bumper2", DOFContactors), Bumper2:End Sub

' Drain & Holes
Sub Drain_Hit:PlaysoundAt "fx_drain", Drain:bsTrough.AddBall Me:End Sub
Sub TopHole_Hit:bsTopHole.addball 0:End Sub

' Rollovers
Sub sw90_Hit:Controller.Switch(90) = 1:PlaySoundAt "fx_sensor", sw90:End Sub
Sub sw90_UnHit:Controller.Switch(90) = 0:End Sub

Sub sw91_Hit:Controller.Switch(91) = 1:PlaySoundAt "fx_sensor2", sw91:End Sub
Sub sw91_UnHit:Controller.Switch(91) = 0:End Sub

Sub sw92_Hit:Controller.Switch(92) = 1:PlaySoundAt "fx_sensor3", sw92:End Sub
Sub sw92_UnHit:Controller.Switch(92) = 0:End Sub

Sub sw93_Hit:Controller.Switch(93) = 1:PlaySoundAt "fx_sensor4", sw93:End Sub
Sub sw93_UnHit:Controller.Switch(93) = 0:End Sub

Sub sw94_Hit:Controller.Switch(94) = 1:PlaySoundAt "fx_sensor5", sw94:End Sub
Sub sw94_UnHit:Controller.Switch(94) = 0:End Sub

Sub sw75_Hit:Controller.Switch(75) = 1:PlaySoundAt "fx_sensor6", sw75:End Sub
Sub sw75_UnHit:Controller.Switch(75) = 0:End Sub

Sub sw45_Hit:Controller.Switch(45) = 1:PlaySoundAt "fx_sensor7", sw45:End Sub
Sub sw45_UnHit:Controller.Switch(45) = 0:End Sub

Sub sw64_Hit:Controller.Switch(64) = 1:PlaySoundAt "fx_sensor8", sw64:End Sub
Sub sw64_UnHit:Controller.Switch(64) = 0:End Sub

Sub sw74_Hit:Controller.Switch(74) = 1:PlaySoundAt "fx_sensor9", sw74:End Sub
Sub sw74_UnHit:Controller.Switch(74) = 0:End Sub

Sub sw76_Hit:Controller.Switch(76) = 1:PlaySoundAt "fx_sensor10", sw76:End Sub
Sub sw76_UnHit:Controller.Switch(76) = 0:End Sub

Sub sw77_Hit:Controller.Switch(77) = 1:PlaySoundAt "fx_sensor11", sw77:End Sub
Sub sw77_UnHit:Controller.Switch(77) = 0:End Sub

Sub sw97_Hit:Controller.Switch(97) = 1:PlaySoundAt "fx_sensor12", sw77:End Sub
Sub sw97_UnHit:Controller.Switch(97) = 0:End Sub

Sub sw95_Hit:Controller.Switch(95) = 1:End Sub
Sub sw95_UnHit:Controller.Switch(95) = 0:End Sub

' Droptargets - only hit sound
Sub sw80_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), sw80:End Sub
Sub sw81_Hit:PlaySoundAt SoundFX("fx_droptarget2", DOFDropTargets), sw81:End Sub
Sub sw82_Hit:PlaySoundAt SoundFX("fx_droptarget3", DOFDropTargets), sw82:End Sub
Sub sw83_Hit:PlaySoundAt SoundFX("fx_droptarget4", DOFDropTargets), sw83:End Sub
Sub sw84_Hit:PlaySoundAt SoundFX("fx_droptarget5", DOFDropTargets), sw84:End Sub

' Targets
Sub sw70_Hit:vpmTimer.PulseSw 70:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw71_Hit:vpmTimer.PulseSw 71:PlaySoundAtBall SoundFX("fx_target2", DOFDropTargets):End Sub
Sub sw72_Hit:vpmTimer.PulseSw 72:PlaySoundAtBall SoundFX("fx_target3", DOFDropTargets):End Sub
Sub sw73_Hit:vpmTimer.PulseSw 73:PlaySoundAtBall SoundFX("fx_target4", DOFDropTargets):End Sub

' Spinner
Sub sw96_Spin:vpmtimer.pulsesw(96):PlaySoundAt "fx_spinner", sw96:End Sub

'*********
'Solenoids
'*********

SolCallback(8) = "bsTrough.SolOut"
SolCallback(5) = "dtbank1.SolDropUp"
SolCallback(6) = "bsTopHole.SolOut"
SolCallback(9) = "Sol_GameOn"

Flipperactive = False

Sub Sol_GameOn(enabled)
    VpmNudge.SolGameOn enabled
    Flipperactive = enabled
    if not Flipperactive Then
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
    end If
    GiOn
end sub

'**************
' Flipper Subs
'**************

'SolCallback(sLRFlipper) = "SolRFlipper"
'SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'*****************
'   Gi Effects
'*****************

Dim OldGiState
OldGiState = -1 'start witht he Gi off

Sub GiON
    For each x in aGiLights
        x.State = 1
    Next
End Sub

Sub GiOFF
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
            LampState(chgLamp(ii, 0)) = chgLamp(ii, 1)       'keep the real state in an array
            FadingState(chgLamp(ii, 0)) = chgLamp(ii, 1) + 3 'fading step
        Next
    End If
    UpdateLeds
    UpdateLamps
End Sub

Sub UpdateLamps()
    'Lamp 1, li1   ' Ball in Play
                   ' light 2 is always on
    'Reel 3, li3   ' tilt
    'Reel 4, li4   ' game over
    'Reel 5, li5   ' last ball
    'Reel 6, li6   ' highscore/handicap
    'Lamp 7, li7   ' credit
    'Lamp 9, li9   ' extra ball 1
    'Lamp 10, li10 ' extra ball 2
    'Lamp 11, li11 ' extra ball 3
    'Lamp 12, li12 ' extra ball 4
    Lamp 8, li8
    Lamp 13, li13
    Lamp 14, li14
    Lamp 15, li15
    Lampm 16, li16
    Lamp 16, li16a
    Lampm 17, li17
    Lamp 17, li17a
    Lampm 18, li18
    Lamp 18, li18a
    Lampm 19, li19
    Lamp 19, li19a
    Lampm 20, li20
    Lamp 20, li20a
    Lampm 21, li21
    Lamp 21, li21a
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
    Lampm 32, li32
    Lamp 32, li32a
    Lamp 33, li33
    Lamp 34, li34
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
    'Lamp 49, li49
    'Lamp 50, li50
    'Lamp 51, li51
    'Lamp 52, li52
    'Lamp 53, li53
    'Lamp 54, li54
    'Lamp 55, li55
    'Lamp 56, li56
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
        Case 4:object.SetValue 3:FadingState(nr) = 0
        Case 3:object.SetValue 2:FadingState(nr) = 2
        Case 2:object.SetValue 1:FadingState(nr) = 1
        Case 1:object.SetValue 0:FadingState(nr) = 0
    End Select
End Sub

Sub Reelm(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 3
        Case 3:object.SetValue 2
        Case 2:object.SetValue 1
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

'*********************
'LED's based on Eala's
'*********************
'Dim Digits(28)
'Digits(0) = Array(a000, a001, a002, a003, a004, a005, a006, a007, a008)
'Digits(1) = Array(a009, a010, a011, a012, a013, a014, a015, a016, a017)
'Digits(2) = Array(a018, a019, a020, a021, a022, a023, a024, a025, a026)
'Digits(3) = Array(a027, a028, a029, a030, a031, a032, a033, a034, a035)
'Digits(4) = Array(a036, a037, a038, a039, a040, a041, a042, a043, a044)
'Digits(5) = Array(a045, a046, a047, a048, a049, a050, a051, a052, a053)
'Digits(6) = Array(a054, a055, a056, a057, a058, a059, a060, a061, a062)
'Digits(7) = Array(a063, a064, a065, a066, a067, a068, a069, a070, a071)
'Digits(8) = Array(a072, a073, a074, a075, a076, a077, a078, a079, a080)
'Digits(9) = Array(a081, a082, a083, a084, a085, a086, a087, a088, a089)
'Digits(10) = Array(a090, a091, a092, a093, a094, a095, a096, a097, a098)

'Digits(11) = Array(a099, a100, a101, a102, a103, a104, a105, a106, a107)
'Digits(12) = Array(a108, a109, a110, a111, a112, a113, a114, a115, a116)
'Digits(13) = Array(a117, a118, a119, a120, a121, a122, a123, a124, a125)
'Digits(14) = Array(a126, a127, a128, a129, a130, a131, a132, a133, a134)
'Digits(15) = Array(a135, a136, a137, a138, a139, a140, a141, a142, a143)
'Digits(16) = Array(a144, a145, a146, a147, a148, a149, a150, a151, a152)
'Digits(17) = Array(a153, a154, a155, a156, a157, a158, a159, a160, a161)
'Digits(18) = Array(a162, a163, a164, a165, a166, a167, a168, a169, a170)
'Digits(19) = Array(a171, a172, a173, a174, a175, a176, a177, a178, a179)
'Digits(20) = Array(a180, a181, a182, a183, a184, a185, a186, a187, a188)
'Digits(21) = Array(a189, a190, a191, a192, a193, a194, a195, a196, a197)

'Digits(22) = Array(a198, a199, a200, a201, a202, a203, a204, a205, a206)
'Digits(23) = Array(a207, a208, a209, a210, a211, a212, a213, a214, a215)
'Digits(24) = Array(a216, a217, a218, a219, a220, a221, a222, a223, a224)
'Digits(25) = Array(a225, a226, a227, a228, a229, a230, a231, a232, a233)
'Digits(26) = Array(a234, a235, a236, a237, a238, a239, a240, a241, a242)
'Digits(27) = Array(a243, a244, a245, a246, a247, a248, a249, a250, a251)

'********************
'Update LED's display
'********************

Sub UpdateLeds()
    Dim ChgLED, ii, num, chg, stat, obj
    ChgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(chgLED)
            num = chgLED(ii, 0):chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            'For Each obj In Digits(num)
                'If chg And 1 Then obj.State = stat And 1
                chg = chg \ 2:stat = stat \ 2
            Next
        'Next
    End If
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

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
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
    tmp = ball.y * 2 / Table1.height-1
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

'********************************************
'   JP's VP10 Rolling Sounds + Ballshadow
' uses a collection of shadows, aBallShadow
'********************************************

Const tnob = 20 ' total number of balls
Const lob = 0   'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol
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
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

' Code by mfuegemman

'Dip 20 3/5 Balls OK
'Dip 3+4 Max Credits OK
'Dip 23 Show Handycap OK
'Dip 9+10 Handycap OK
'Dip 24 Free Games on Handycap OK
'Dip 11+12+13+14 Replay OK
'Dip 21 Music OK

'-----------------------------
'------ DIP switch menu ------
'-----------------------------
Sub editDips
    Dim vpmDips:Set vpmDips = New cvpmDips
    With vpmDips
        .AddForm 0, 200, "Skateboard DIP switches"
        .AddFrame 0,  0,200,"Balls per game", &H00080000, Array("3 balls", 0, "5 balls", &H00080000) 'dip 20
       	.AddChk   0, 50,200,Array("Extra Ball at 3 completed DT banks",&H00000001)'dip 1
		.AddChk   0, 65,200,Array("Special at 3 completed DT banks",&H00000002)'dip 2
		.AddChk   0, 80,200,Array("Reset DT bank after 3 completions",&H00000010)'dip 8
		.AddChk   0, 95,200,Array("Add Bonus on last Ball",&H00000080)'dip 8
        .AddChk	  0,110,200,Array("Play music", &H00100000)         	                                                                                          'dip 21
        .AddChk   0,125,200,Array("Show handicap", &H00400000)     
        .AddFrame 0,145,200,"Max. credits", &H00000012, Array("5", 0, "10", &H00000004, "15", &H00000008, "20", &H00000012)                                 'dip 3+4
        .AddFrame 0,230,200,"1st replay", &H00003000, Array("650.000", 0, "700.000", &H00001000, "750.000", &H00002000, "800.000", &H00003000)              'dip 13+14
        .AddFrame 0,315,200,"2nd replay", &H00001200, Array("800.000", 0, "850.000", &H00000400, "900.000", &H00000800, "off", &H00001200)                  'dip 11+12
        .AddFrame 0,400,200,"Minimum handicap points", &H00000300, Array("700.000", 0, "750.000", &H00000100, "850.000", &H00000200, "950.000", &H00000300) 'dip 9+10
        .AddFrame 0,485,200,"Free games on handicap", &H00800000, Array("1 credit", 0, "2 credits", &H00800000)                                             'dip 24
                                                                                           
'		.AddFrame 0,50,200,"Coin chute 1 settings",&H00040000,Array("2 coins = 1 credit",0,"1 coin = 1 credit",&H00040000)'dip 17+18+19
        .AddLabel 0, 540, 200, 30, "After hitting OK, press F3 to reset game with new settings."
        .ViewDips
    End With
End Sub
Set vpmShowDips = GetRef("editDips")

Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub