' Faeton - Juegos Populares (Spain) 1985 / IPD No. 3087 / 4 Players
' VPX version by JPSalas, 2018, version 1.0.3
' known issues:
' the hole doesn't always award the 30.000 points as indicated by its light

Option Explicit
Randomize

Const BallSize = 50
Const BallMass = 1.1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim bsTrough, bsSaucer, cbCaptive, dtL, dtR, x

Const cGameName = "faeton"

Const UseSolenoids = 2 'Fastflips enabled
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
    UseVPMDMD = True
    VarHidden = 0
    For each x in aReels
        x.Visible = 0
    Next
    lrail.Visible = 0
    rrail.Visible = 0
end if

if B2SOn = true then VarHidden = 0

LoadVPM "01550000", "juegos.vbs", 3.26

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
        .SplashInfoLine = "Beverly Hills Cop (TBA 2019)" & vbNewLine & "VPX table by IVANTBA"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 0
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 0
        .Games(cGameName).Settings.Value("sound") = 0 '1= rotated display, 0= normal
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 1000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With

PlaySound "Intro",-0

    ' Nudging
    vpmNudge.TiltSwitch = 30
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitNoTrough BallRelease, 25, 90, 7
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    End With

    ' Saucer
    Set bsSaucer = New cvpmBallStack
    bsSaucer.InitSaucer sw9, 9, 60, 20
    bsSaucer.InitExitSnd SoundFX("fx_popper", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)

    ' Captive Ball
    Set cbCaptive = New cvpmCaptiveBall
    cbCaptive.InitCaptive CaptiveTrigger, CaptiveWall, CaptiveKicker, 40
    cbCaptive.Start
    cbCaptive.ForceTrans = 1.8
    cbCaptive.MinForce = 3.5
    cbCaptive.CreateEvents "cbCaptive"

    ' Left Drop targets
    Set dtL = New cvpmDropTarget
    dtL.InitDrop Array(sw4, sw12, sw20), Array(4, 12, 20)
    dtL.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtL.CreateEvents "dtL"

    ' Right Drop targets
    Set dtR = New cvpmDropTarget
    dtR.InitDrop Array(sw5, sw13, sw21), Array(5, 13, 21)
    dtR.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtR.CreateEvents "dtR"

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Init walls
    sw7.IsDropped = 1:sw7a.IsDropped = 0:sw7c.IsDropped = 1
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.stop:End Sub

'**********
' Music
'**********


'Start the music

DIM music

music = "Beverly Hills Cop (Music).mp3"
PlayMusic "Beverly Hills Cop (Music).mp3" 

Sub Table1_MusicDone()
PlayMusic "Beverly Hills Cop (Music).mp3"
End Sub 

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If KeyCode = LeftFlipperKey Then Controller.Switch(84) = 1
    If KeyCode = RightFlipperKey Then Controller.Switch(82) = 1
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then PlaySound "fx_PlungerPull", 0, 1, 0.1, 0.25:Plunger.Pullback
    If vpmKeyDown(keycode)Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If KeyCode = LeftFlipperKey Then Controller.Switch(84) = 0
    If KeyCode = RightFlipperKey Then Controller.Switch(82) = 0
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySound "fx_plunger", 0, 1, 0.1, 0.25:Plunger.Fire
End Sub

'*********
' Switches
'*********

' Slings
Dim RStep

Sub RightSlingShot_Slingshot
    PlaySound SoundFX("fx_slingshot", DOFContactors), 0, 1, 0.05, 0.05
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 16
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
Sub Bumper1_Hit:vpmTimer.PulseSw 24:PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, -0.05:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 32:PlaySound SoundFX("fx_bumper2", DOFContactors), 0, 1, -0.02:End Sub

' Drain & Saucers
Sub Drain_Hit:Playsound "fx_drain":bsTrough.AddBall Me:End Sub
Sub sw9_Hit:PlaySound "fx_hole_enter", 0, 1, -0.05:bsSaucer.AddBall 0:End Sub

' Rollovers
Sub sw14_Hit:Controller.Switch(14) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub

Sub sw10_Hit:Controller.Switch(10) = 1:PlaySound "fx_sensor2", 0, 1, pan(ActiveBall):End Sub
Sub sw10_UnHit:Controller.Switch(10) = 0:End Sub

Sub sw2_Hit:Controller.Switch(2) = 1:PlaySound "fx_sensor6", 0, 1, pan(ActiveBall):End Sub
Sub sw2_UnHit:Controller.Switch(2) = 0:End Sub

Sub sw6_Hit:Controller.Switch(6) = 1:PlaySound "fx_sensor4", 0, 1, pan(ActiveBall):DOF 111, DOFOn:End Sub
Sub sw6_UnHit:Controller.Switch(6) = 0:DOF 111, DOFOff:End Sub

Sub sw22b_Hit:Controller.Switch(22) = 1:PlaySound "fx_sensor5", 0, 1, pan(ActiveBall):DOF 108, DOFOn:End Sub
Sub sw22b_UnHit:Controller.Switch(22) = 0:DOF 108, DOFOff:End Sub

Sub sw22_Hit:Controller.Switch(22) = 1:PlaySound "fx_sensor3", 0, 1, pan(ActiveBall):DOF 109, DOFOn:End Sub
Sub sw22_UnHit:Controller.Switch(22) = 0::DOF 109, DOFOff:End Sub

Sub sw31_Hit:Controller.Switch(31) = 1:PlaySound "fx_sensor7", 0, 1, pan(ActiveBall):End Sub
Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub

Sub sw23a_Hit:Controller.Switch(23) = 1:PlaySound "fx_sensor8", 0, 1, pan(ActiveBall):End Sub
Sub sw23a_UnHit:Controller.Switch(23) = 0:End Sub

Sub sw23b_Hit:Controller.Switch(23) = 1:PlaySound "fx_sensor9", 0, 1, pan(ActiveBall):End Sub
Sub sw23b_UnHit:Controller.Switch(23) = 0:End Sub

Sub sw15_Hit:Controller.Switch(15) = 1:PlaySound "fx_sensor10", 0, 1, pan(ActiveBall):End Sub
Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub

' Spinners
Sub Spinner1_Spin:vpmTimer.PulseSw 6:PlaySound "fx_spinner", 0, 1, 0.05:DOF 112, DOFPulse:End Sub

'Targets
Sub sw7_Hit:vpmTimer.PulseSw 7:sw7.IsDropped = 1:sw7a.IsDropped = 0:sw7c.IsDropped = 1:sw7b.IsDropped = 0:PlaySound SoundFX("fx_target", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw7b_Hit:vpmTimer.PulseSw 7:sw7b.IsDropped = 1:sw7c.IsDropped = 0:sw7.IsDropped = 0:sw7a.IsDropped = 1:PlaySound SoundFX("fx_target2", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw6b_Hit:vpmTimer.PulseSw 6:PlaySound SoundFX("fx_target3", DOFDropTargets), 0, 1, pan(ActiveBall):DOF 104, DOFPulse:End Sub
Sub sw6d_Hit:vpmTimer.PulseSw 6:PlaySound SoundFX("fx_target4", DOFDropTargets), 0, 1, pan(ActiveBall):DOF 105, DOFPulse:End Sub
Sub sw8_Hit:vpmTimer.PulseSw 8:PlaySound SoundFX("fx_target5", DOFDropTargets), 0, 1, pan(ActiveBall):DOF 106, DOFPulse:End Sub
Sub sw8b_Hit:vpmTimer.PulseSw 8:PlaySound SoundFX("fx_target6", DOFDropTargets), 0, 1, pan(ActiveBall):DOF 107, DOFPulse:End Sub
Sub sw18_Hit:vpmTimer.PulseSw 18:PlaySound SoundFX("fx_target7", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub

'Droptargets (only sound effect)

Sub sw4_Hit:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw12_Hit:PlaySound SoundFX("fx_droptarget2", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw20_Hit:PlaySound SoundFX("fx_droptarget3", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw5_Hit:PlaySound SoundFX("fx_droptarget4", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw13_Hit:PlaySound SoundFX("fx_droptarget5", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw21_Hit:PlaySound SoundFX("fx_droptarget6", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub

'Rubbers
Sub sw17a_Hit:vpmTimer.PulseSw 17:DOF 101, DOFPulse:End Sub '17 '10 point rubbers
Sub sw17b_Hit:vpmTimer.PulseSw 17:DOF 102, DOFPulse:End Sub '17
Sub sw17c_Hit:vpmTimer.PulseSw 17:DOF 103, DOFPulse:End Sub '17

' Gi Subs

Sub Gi_Hit:GiON:End Sub

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

'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFFlippers), 0, 1, -0.1, 0.05
        LeftFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFFlippers), 0, 1, -0.1, 0.05
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFFlippers), 0, 1, 0.1, 0.05
        RightFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFFlippers), 0, 1, 0.1, 0.05
        RightFlipper.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.25
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.25
End Sub

'**********************************************************
'     JP's Flasher Fading for VPX and Vpinmame v 1.0
'       (Based on Pacdude's Fading Light System)
' This is a fast fading for the Flashers in vpinmame tables
'**********************************************************

Dim FadingState(200)

InitLamps() ' turn off the lights and flashers and reset them to the default parameters

 'vpinmame Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
            FadingState(chgLamp(ii, 0)) = chgLamp(ii, 1) + 3 'fading step
        Next
    End If
    UpdateLamps
    'UpdateLeds
End Sub

Sub UpdateLamps
    ' playfield lights
    'Lamp 1, li1
    'Lamp 2, li2
    Lamp 3, li3
    'Lamp 4, li4
    'Lamp 5, li5
    'Lamp 6, li6
    'Lamp 7, li7
    'Lamp 8, li8
    'Lampm 9, Light1
    Lamp 9, li9
    'Lampm 10, Light2
    Lamp 10, li10
    'Lampm 11, Light3
    Lamp 11, li11
    'Lampm 12, Light4
    Lamp 12, li12
    'Lampm 13, Light5
    Lamp 13, li13
    'Lampm 14, Light6
    Lamp 14, li14
    'Lampm 15, Light7
    Lamp 15, li15
    'Lampm 16, Light8
    Lamp 16, li16

    Lamp 17, li17
    'Lamp 18, li18
    'Lamp 19, li19
    'Lampm 20, Light9
    Lamp 20, li20
    'Lampm 21, Light10
    Lamp 21, li21
    Lamp 22, li22
    Lampm 23, li23a
    Lamp 23, li23
    'Lamp 24, li24
    Lamp 25, li25
    Lamp 26, li26
    Lamp 27, li27
    Lampm 28, li28a
    Lamp 28, li28
    Lamp 29, li29
    Lamp 30, li30
    'Lamp 31, li31
    'Lamp 32, li32 'game over
    'Lamp 33, li33
    'Lamp 34, li34

    'Lampm 35, li35a 'light 35 to 45 Gi lights
    Lampm 37, li37
    Lampm 37, li37b
    Lampm 37, li37d
    Lampm 38, li38
    Lampm 38, li38b
    'Lampm 39, li39b
    'Lampm 40, li40f
    'Lampm 41, li41b
    'Lampm 42, li42b
    Lamp 42, li42
    'Lampm 43, li43b
    'Lampm 44, li44b
    'Lampm 45, li45b
    Lamp 50, li50
    'Lamp 66, li66 'tilt
    Lamp 81, li81
    'Lamp 82, li82 'lights 82 to 88 maybe backglass decoration lights
    'Lamp 83, li83
    'Lamp 84, li84
    'Lamp 85, li85
    'Lamp 86, li86
    'Lamp 87, li87
    'Lamp 88, li88

    ' Lights turning on Solenoids
    If Controller.Lamp(2)Then 'Imp.Agujero/Hole
        bsSaucer.ExitSol_On
    End If

    If Controller.Lamp(18)Then 'Bancada Dianas Derecho
        dtR.DropSol_On         'reset right target bank
    End If

    If Controller.Lamp(19)Then 'Bancada Dianas Izquierda
        dtL.DropSol_On         'reset left target bank
    End If

    If Controller.Lamp(33)Then 'TACA
        PlaySound SoundFX("fx_knocker",DOFKnocker)
    End If

    If Controller.Lamp(34)Then 'Salida Bolas
        If bsTrough.Balls Then bsTrough.ExitSol_On
    End If
End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    LampTimer.Interval = 50 ' flasher fading speed
    LampTimer.Enabled = 1
    For x = 0 to 200
        FadingState(x) = 3 ' turns off all the lights
    Next
End Sub

Sub SetLamp(nr, value) ' value 0 is off, 1 is on
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

'*********************
'LED's based on Eala's
'*********************
'Dim Digits(32)
'Digits(0) = Array(a00, a02, a05, a06, a04, a01, a03, a07)
'Digits(1) = Array(a10, a12, a15, a16, a14, a11, a13)
'Digits(2) = Array(a20, a22, a25, a26, a24, a21, a23)
'Digits(3) = Array(a30, a32, a35, a36, a34, a31, a33, a37)
'Digits(4) = Array(a40, a42, a45, a46, a44, a41, a43)
'Digits(5) = Array(a50, a52, a55, a56, a54, a51, a53)
'Digits(6) = Array(a60, a62, a65, a66, a64, a61, a63)

'Digits(7) = Array(b00, b02, b05, b06, b04, b01, b03, b07)
'Digits(8) = Array(b10, b12, b15, b16, b14, b11, b13)
'Digits(9) = Array(b20, b22, b25, b26, b24, b21, b23)
'Digits(10) = Array(b30, b32, b35, b36, b34, b31, b33, b37)
'Digits(11) = Array(b40, b42, b45, b46, b44, b41, b43)
'Digits(12) = Array(b50, b52, b55, b56, b54, b51, b53)
'Digits(13) = Array(b60, b62, b65, b66, b64, b61, b63)

'Digits(14) = Array(e00, e02, e05, e06, e04, e01, e03, e07)
'Digits(15) = Array(e10, e12, e15, e16, e14, e11, e13)
'Digits(16) = Array(e20, e22, e25, e26, e24, e21, e23)
'Digits(17) = Array(e30, e32, e35, e36, e34, e31, e33, e37)
'Digits(18) = Array(e40, e42, e45, e46, e44, e41, e43)
'Digits(19) = Array(e50, e52, e55, e56, e54, e51, e53)
'Digits(20) = Array(e60, e62, e65, e66, e64, e61, e63)

'Digits(21) = Array(f00, f02, f05, f06, f04, f01, f03, f07)
'Digits(22) = Array(f10, f12, f15, f16, f14, f11, f13)
'Digits(23) = Array(f20, f22, f25, f26, f24, f21, f23)
'Digits(24) = Array(f30, f32, f35, f36, f34, f31, f33, f37)
'Digits(25) = Array(f40, f42, f45, f46, f44, f41, f43)
'Digits(26) = Array(f50, f52, f55, f56, f54, f51, f53)
'Digits(27) = Array(f60, f62, f65, f66, f64, f61, f63)

'Digits(28) = Array(c00, c02, c05, c06, c04, c01, c03)
'Digits(29) = Array(c10, c12, c15, c16, c14, c11, c13)

'Digits(30) = Array(d00, d02, d05, d06, d04, d01, d03)
'Digits(31) = Array(d10, d12, d15, d16, d14, d11, d13)

'Digits(32) = Array(c20, c22, c25, c26, c24, c21, c23)

'********************
'Update LED's display
'********************

'Sub UpdateLeds()
    'Dim ChgLED, ii, num, chg, stat, obj
    'ChgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    'If Not IsEmpty(ChgLED)Then
        'For ii = 0 To UBound(chgLED)
            'num = chgLED(ii, 0):chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            'For Each obj In Digits(num)
                'If chg And 1 Then obj.State = stat And 1
                'chg = chg \ 2:stat = stat \ 2
            'Next
        'Next
    'End If
'End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySound "fx_MetalHit2", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_rubber_post", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_rubber_pin", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub

Sub REnd1_Hit:PlaySound "fx_ballrampdrop", 0, 1, 0.05, -0.05:End Sub
Sub REnd2_Hit:PlaySound "fx_ballrampdrop", 0, 1, 0.05, 0.05:End Sub
Sub REnd3_Hit:PlaySound "fx_balldrop", 0, 1, 0.05, 0.05:End Sub
Sub REnd4_Hit:PlaySound "fx_ballrampdrop", 0, 1, 0.05, -0.05:End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 4 ' total number of balls +1
Const lob = 0  'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)
        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 15000 'increase the pitch on a ramp or elevated surface
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), Pan(BOT(b)), 0, ballpitch, 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'******************
' RealTime Updates
'******************

Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates
    RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
End Sub

Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub