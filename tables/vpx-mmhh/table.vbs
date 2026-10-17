' Speed Test - Taito 1982 / IPD No. 4589 / 4 Players                Reskin by Iceman for Mickey Maus happy Halloween     Not for Sale  for Fans    
' VPX - version by JPSalas 2018, version 1.0.0                                          Thank you JPSalas and all Creators 

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01550000", "Taito.vbs", 3.26

Dim bsTrough, dtbankLeft, dtbankRight, dtbankCenter, bsLeftSaucer, bsRightSaucer, bsTopSaucer, x

Const cGameName = "stest"

Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 0

'******************************************************************************
' Cabinet options (added for vpx-standalone-alp4k)
'******************************************************************************
Const FreePlay        = 1    ' 1 = Start button adds a credit and starts (no coin needed), 0 = coins required
Const FreePlayStartMs = 300  ' delay between the auto coin and the start press (raise if a press is ignored)

Const ReelGuard       = 1    ' 1 = keep the final scores on the backglass reels after game over
                             ' 0 = original behaviour (Taito end-of-game display scrambles the reels)
Const ReelGuardLockMs = 2500 ' ms after the play relay drops before the reels are locked
                             ' (window for any end-of-ball bonus to finish counting)

Dim VarHidden
If Table1.ShowDT = true then
    VarHidden = 1
    For each x in aReels
        x.Visible = 1
    Next
else
    VarHidden = 0
    For each x in aReels
        x.Visible = 0
    Next
    lrail.Visible = 0
    rrail.Visible = 0
end if

if B2SOn = true then VarHidden = 1

' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = ""
Const SCoin = "fx_Coin"

'************
' Table init.
'************

Sub table1_Init
    vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Ace Ventura - Pet Detective (TBA 2019)" & vbNewLine & "VPX table by IVANTBA" &"Reskin by Iceman 2022"
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

PlaySound "Intro",-0
PlaySound "Music",-1

    ' Nudging
    vpmNudge.TiltSwitch = 30
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(LeftSlingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0,1,0,0,0,0,0,0
        .InitKick BallRelease, 90, 4
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 1
    End With

    ' Saucers
    Set bsLeftSaucer = New cvpmBallStack
    bsLeftSaucer.InitSaucer sw4, 4, 70, 8
    bsLeftSaucer.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsLeftSaucer.KickForceVar = 3

    Set bsRightSaucer = New cvpmBallStack
    bsRightSaucer.InitSaucer sw3, 3, 270, 8
    bsRightSaucer.InitExitSnd SoundFX("fx_kicker2", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsRightSaucer.KickForceVar = 3

    Set bsTopSaucer = New cvpmBallStack
    bsTopSaucer.InitSaucer sw2, 2, 180, 8
    bsTopSaucer.InitExitSnd SoundFX("fx_kicker3", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsTopSaucer.KickForceVar = 3

    ' Drop targets
    set dtbankLeft = new cvpmdroptarget
    dtbankLeft.InitDrop Array(sw12, sw22, sw32), Array(12, 22, 32)
    dtbankLeft.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)

    set dtbankRight = new cvpmdroptarget
    dtbankRight.InitDrop Array(sw42, sw52, sw62), Array(42, 52, 62)
    dtbankRight.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)

    set dtbankCenter = new cvpmdroptarget
    dtbankCenter.InitDrop Array(sw43, sw53, sw63, sw73), Array(43, 53, 63, 73)
    dtbankCenter.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    Controller.Switch(60) = 1
    Controller.Switch(70) = 1

    ' Turn on Gi
    GiOn
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.stop:End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then PlaySound "fx_PlungerPull", 0, 1, 0.1, 0.25:Plunger.Pullback
    If FreePlay = 1 And keycode = StartGameKey Then FreePlayStart:Exit Sub
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If FreePlay = 1 And keycode = StartGameKey Then Exit Sub
    If vpmKeyUp(keycode) Then Exit Sub
    If keycode = PlungerKey Then PlaySound "fx_plunger", 0, 1, 0.1, 0.25:Plunger.Fire
End Sub

' Free play: Taito has no free-play setting, so feed one coin then press Start.
' Pressing Start again during ball 1 adds players exactly like a real coin+start.
' If the credit reel already shows credits, just press Start (no coin), so presses
' the ROM ignores (e.g. after ball 1) don't pile up credits.
Sub FreePlayStart
    Dim cr:cr = 0
    If B2SOn And ReelGuard = 1 Then cr = SegToNum(rgSeg(25)) ' digit 25 = credit display
    If cr > 0 Then
        Controller.Switch(swStartButton) = 1
        vpmTimer.AddTimer 200, "Controller.Switch(swStartButton) = 0'"
    Else
        vpmTimer.PulseSw swCoin1
        vpmTimer.AddTimer FreePlayStartMs, "Controller.Switch(swStartButton) = 1'"
        vpmTimer.AddTimer FreePlayStartMs + 200, "Controller.Switch(swStartButton) = 0'"
    End If
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("fx_slingshot", DOFContactors), 0, 1, -0.05, 0.05
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 13
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
    PlaySound SoundFX("fx_slingshot2", DOFContactors), 0, 1, 0.05, 0.05
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 23
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
' Rubber animations
Dim Rub1, Rub2

Sub sw5_Hit:vpmTimer.PulseSw 5:Rub1 = 1:sw5_Timer:End Sub

Sub sw5_Timer
    Select Case Rub1
        Case 1:r4.Visible = 0:r9.Visible = 1:sw5.TimerEnabled = 1
        Case 2:r9.Visible = 0:r10.Visible = 1
        Case 3:r10.Visible = 0:r4.Visible = 1:sw5.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub sw15_Hit:vpmTimer.PulseSw 15:Rub2 = 1:sw15_Timer:End Sub
Sub sw15_Timer
    Select Case Rub2
        Case 1:r3.Visible = 0:r11.Visible = 1:sw15.TimerEnabled = 1
        Case 2:r11.Visible = 0:r12.Visible = 1
        Case 3:r12.Visible = 0:r3.Visible = 1:sw15.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub

' Drain & Saucers
Sub Drain_Hit:Playsound "fx_drain":bsTrough.AddBall Me:End Sub
Sub sw3_Hit:PlaySound "fx_kicker_enter2", 0, 1, 0.05:bsRightSaucer.AddBall 0:End Sub
Sub sw2_Hit:PlaySound "fx_kicker_enter", 0, 1, 0.05:bsTopSaucer.AddBall 0:End Sub
Sub sw4_Hit:PlaySound "fx_kicker_enter3", 0, 1, -0.05:bsLeftSaucer.AddBall 0:End Sub

' Rollovers
Sub sw44_Hit:Controller.Switch(44) = 1:PlaySound "sw44", 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw44_UnHit:Controller.Switch(44) = 0:End Sub

Sub sw24_Hit:Controller.Switch(24) = 1:PlaySound "sw24", 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub

Sub sw14_Hit:Controller.Switch(14) = 1:PlaySound "sw14", 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub

Sub sw34_Hit:Controller.Switch(34) = 1:PlaySound "sw34", 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub

Sub sw74_Hit:Controller.Switch(74) = 1:PlaySound "sw74", 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):LTleft.State = 1:LTlefta.Visible = 1:End Sub
Sub sw74_UnHit:Controller.Switch(74) = 0:LTleft.State = 0:LTlefta.Visible = 0:End Sub

Sub sw33_Hit:Controller.Switch(33) = 1:PlaySound "sw33", 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):sw33l.Duration 1, 400, 0:End Sub
Sub sw33_UnHit:Controller.Switch(33) = 0:End Sub

Sub sw11_Hit:Controller.Switch(11) = 1:PlaySound "sw11", 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):sw11l.Duration 1, 400, 0:End Sub
Sub sw11_UnHit:Controller.Switch(11) = 0:End Sub

Sub sw72_Hit:Controller.Switch(72) = 1:PlaySound "sw72", 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):LTright.State = 1:End Sub
Sub sw72_UnHit:Controller.Switch(72) = 0:LTright.State = 0: End Sub

' Atalho switch ?
Sub sw31_Hit:Controller.Switch(31) = 1:PlaySound "sw31", 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw31_UnHit:Controller.Switch(31) = 0: End Sub

' Droptargets
Sub sw42_Hit:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw52_Hit:PlaySound SoundFX("fx_droptarget2", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw62_Hit:PlaySound SoundFX("fx_droptarget3", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw12_Hit:PlaySound SoundFX("fx_droptarget4", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw22_Hit:PlaySound SoundFX("fx_droptarget5", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw32_Hit:PlaySound SoundFX("fx_droptarget6", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw43_Hit:PlaySound SoundFX("fx_droptarget7", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw53_Hit:PlaySound SoundFX("fx_droptarget8", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw63_Hit:PlaySound SoundFX("fx_droptarget9", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw73_Hit:PlaySound SoundFX("fx_droptarget10", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub

Sub sw42_Dropped:dtbankRight.Hit 1:End Sub
Sub sw52_Dropped:dtbankRight.Hit 2:End Sub
Sub sw62_Dropped:dtbankRight.Hit 3:End Sub
Sub sw12_Dropped:dtbankLeft.Hit 1:End Sub
Sub sw22_Dropped:dtbankLeft.Hit 2:End Sub
Sub sw32_Dropped:dtbankLeft.Hit 3:End Sub
Sub sw43_Dropped:dtbankCenter.Hit 1:End Sub
Sub sw53_Dropped:dtbankCenter.Hit 2:End Sub
Sub sw63_Dropped:dtbankCenter.Hit 3:End Sub
Sub sw73_Dropped:dtbankCenter.Hit 4:End Sub

'Targets
Sub sw21_Hit:vpmTimer.PulseSw 21:PlaySound SoundFX("fx_target", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub

Sub sw41_Hit:vpmTimer.PulseSw 41:PlaySound SoundFX("fx_target2", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw51_Hit:vpmTimer.PulseSw 51:PlaySound SoundFX("fx_target3", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw61_Hit:vpmTimer.PulseSw 61:PlaySound SoundFX("fx_target4", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw71_Hit:vpmTimer.PulseSw 71:PlaySound SoundFX("fx_target5", DOFDropTargets), 0, 1, pan(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub

'*********
'Solenoids
'*********

SolCallback(1) = "bsTrough.SolOut"
SolCallback(2) = "bsTopSaucer.SolOut"
SolCallback(3) = "bsRightSaucer.SolOut"
SolCallback(4) = "bsLeftSaucer.SolOut"
SolCallback(5) = "dtbankLeft.SolDropUp"
SolCallback(6) = "dtbankRight.SolDropUp"
SolCallback(7) = "dtbankCenter.SolHit 1,"
SolCallback(8) = "dtbankCenter.SolHit 2,"
SolCallback(9) = "dtbankCenter.SolHit 3,"
SolCallback(10) = "dtbankCenter.SolHit 4,"
SolCallback(12)	= "dtbankCenter.SolDropUp"
SolCallback(17) = "SolGi" '17=relay
SolCallback(18) = "SolGameOn" '18 = play relay

Sub SolGameOn(Enabled)
    vpmNudge.SolGameOn Enabled
    ReelGuardGameOn Enabled
End Sub

Sub SolGi(enabled)
    If enabled Then
        GiOff
    Else
        GiOn
    End If
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
        LeftFlipper1.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFFlippers), 0, 1, -0.1, 0.05
        LeftFlipper.RotateToStart
        LeftFlipper1.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFFlippers), 0, 1, 0.1, 0.05
        RightFlipper.RotateToEnd
        RightFlipper1.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFFlippers), 0, 1, 0.1, 0.05
        RightFlipper.RotateToStart
        RightFlipper1.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.25
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.25
End Sub

Sub LeftFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.25
End Sub

Sub RightFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.25
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

'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200), FlashRepeat(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 10 ' lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
    If VarHidden Then
        UpdateLeds
    End If
    UpdateLamps
    GIUpdate
    RollingUpdate
End Sub

Sub UpdateLamps()
    NFadeL 0, l0
    NFadeL 1, l1
    NFadeL 10, l10
    NFadeL 100, l100
    NFadeL 101, l101
    NFadeLm 102, l102
    Flash 102, l102a
    NFadeL 103, l103
    NFadeL 109, l109
    NFadeL 11, l11
    NFadeL 110, L110
    NFadeL 111, L111
    ' NFadeL 112, L112
    NFadeL 113, l113
    NFadeLm 119, l119
    Flashm 119, l119a
    Flash 119, l119b
    NFadeL 12, l12
    NFadeLm 120, l120
    Flash 120, l120a
    NFadeLm 121, l121
    Flash 121, l121a
    NFadeLm 122, l122
    Flash 122, l122a
    NFadeL 123, l123
    NFadeLm 129, l129
    Flash 129, l129a
    NFadeLm 130, l130
    Flash 130, l130a
    NFadeLm 131, l131
    Flash 131, l131a
    NFadeLm 132, l132
    Flash 132, l132a
    NFadeL 133, l133
    NFadeL 143, l143
    NFadeL 153, l153
    NFadeL 2, l2
    NFadeL 20, l20
    NFadeL 21, l21
    NFadeL 22, l22
    NFadeL 30, l30
    NFadeL 31, l31
    NFadeL 32, l32
    NFadeL 40, l40
    NFadeL 41, l41
    NFadeL 42, l42
    NFadeL 50, l50
    NFadeLm 51, l51
    Flash 51, l51a
    NFadeL 52, l52
    NFadeL 60, l60
    NFadeL 61, l61
    NFadeL 62, l62
    NFadeL 70, l70
    NFadeL 71, l71
    NFadeL 72, l72
    NFadeL 79, l79
    NFadeL 80, l80
    NFadeL 81, l81
    NFadeL 82, l82
    NFadeL 83, l83
    NFadeL 89, l89
    NFadeL 90, l90
    NFadeL 91, l91
    NFadeL 92, l92
    NFadeL 93, l93
    NFadeLm 99, l99
    Flash 99, l99a
    'backdrop lights
    'If VarHidden Then
        'NFadeL 139, l139
        'NFadeL 140, l140
        'NFadeL 141, l141
        'NFadeL 142, l142
        'NFadeL 149, l149
        'NFadeL 150, l150
        'NFadeL 151, l151
        'NFadeL 152, l152
    'End If
End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.2   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.1 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
        FlashRepeat(x) = 20     ' how many times the flash repeats
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
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1            'wait
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
            If FlashLevel(nr) <FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr)> FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change anything, it just follows the main flasher
    Select Case FadingLevel(nr)
        Case 4, 5
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub FlashBlink(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) <FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
            If FadingLevel(nr) = 0 AND FlashRepeat(nr) Then 'repeat the flash
                FlashRepeat(nr) = FlashRepeat(nr) -1
                If FlashRepeat(nr) Then FadingLevel(nr) = 5
            End If
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr)> FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
            If FadingLevel(nr) = 1 AND FlashRepeat(nr) Then FadingLevel(nr) = 4
    End Select
End Sub

' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub FadeR(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.SetValue 0:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.SetValue 2:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1          'wait
        Case 13:object.SetValue 3:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeRm(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1
        Case 5:object.SetValue 0
        Case 9:object.SetValue 2
        Case 3:object.SetValue 3
    End Select
End Sub

'Texts

Sub NFadeT(nr, object, message)
    Select Case FadingLevel(nr)
        Case 4:object.Text = "":FadingLevel(nr) = 0
        Case 5:object.Text = message:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeTm(nr, object, message)
    Select Case FadingLevel(nr)
        Case 4:object.Text = ""
        Case 5:object.Text = message
    End Select
End Sub

'************************************
'          LEDs Display
'     Based on Scapino's LEDs
'************************************

Dim Digits(32)
Dim Patterns(11)
Dim Patterns2(11)

Patterns(0) = 0     'empty
Patterns(1) = 63    '0
Patterns(2) = 6     '1
Patterns(3) = 91    '2
Patterns(4) = 79    '3
Patterns(5) = 102   '4
Patterns(6) = 109   '5
Patterns(7) = 125   '6
Patterns(8) = 7     '7
Patterns(9) = 127   '8
Patterns(10) = 111  '9

Patterns2(0) = 128  'empty
Patterns2(1) = 191  '0
Patterns2(2) = 134  '1
Patterns2(3) = 219  '2
Patterns2(4) = 207  '3
Patterns2(5) = 230  '4
Patterns2(6) = 237  '5
Patterns2(7) = 253  '6
Patterns2(8) = 135  '7
Patterns2(9) = 255  '8
Patterns2(10) = 239 '9

'Assign 6-digit output to reels
'Set Digits(0) = a0
'Set Digits(1) = a1
'Set Digits(2) = a2
'Set Digits(3) = a3
'Set Digits(4) = a4
'Set Digits(5) = a5

'Set Digits(6) = b0
'Set Digits(7) = b1
'Set Digits(8) = b2
'Set Digits(9) = b3
'Set Digits(10) = b4
'Set Digits(11) = b5

'Set Digits(12) = c0
'Set Digits(13) = c1
'Set Digits(14) = c2
'Set Digits(15) = c3
'Set Digits(16) = c4
'Set Digits(17) = c5

'Set Digits(18) = d0
'Set Digits(19) = d1
'Set Digits(20) = d2
'Set Digits(21) = d3
'Set Digits(22) = d4
'Set Digits(23) = d5

'Set Digits(24) = e0
'Set Digits(25) = e1

Sub UpdateLeds
    If B2SOn And ReelGuard = 1 Then ReelGuardUpdate:Exit Sub
    On Error Resume Next
    Dim ChgLED, ii, jj, chg, stat
    ChgLED = Controller.ChangedLEDs(0, &H3FFFFFF) ' digits 0-25 (was &HFF, &HFFFF = digits 0-15 only)
    If Not IsEmpty(ChgLED) Then
        For ii = 0 To UBound(ChgLED)
            chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For jj = 0 to 10
                If stat = Patterns(jj) OR stat = Patterns2(jj) then Digits(chgLED(ii, 0) ).SetValue jj
            Next
        Next
    End IF
End Sub

'******************************************************************************
' Reel guard
' At game over the Taito firmware borrows the 4 score displays for a message /
' attract animation. On 7-segment LEDs that reads as text; on this backglass's
' score REELS every non-digit pattern becomes "0", so the scores get scrambled
' and stay wrong. While a game is running the ROM drives the reels as normal.
' After the play relay (sol 18) drops we only accept sane score changes (one
' player at a time, never going down), then lock the reels on the final scores
' and stop reading the displays until the next game starts.
' Note: while locked the credit/ball reels also stay put (no change with FreePlay).
'******************************************************************************
Dim rgSeg(25), rgGood(23), rgText(23), rgState, rgOffTime
' rgState: 0 = game running, 1 = watching after game over, 2 = locked
rgState = 1:rgOffTime = 0
For x = 0 To 25:rgSeg(x) = 0:Next
For x = 0 To 23:rgGood(x) = 0:rgText(x) = -1:Next

Function SegToNum(v) ' -1 = blank, -2 = not a digit
    Select Case (v And 127)
        Case 0:SegToNum = -1
        Case 63:SegToNum = 0
        Case 6:SegToNum = 1
        Case 91:SegToNum = 2
        Case 79:SegToNum = 3
        Case 102:SegToNum = 4
        Case 109:SegToNum = 5
        Case 125, 124:SegToNum = 6
        Case 7:SegToNum = 7
        Case 127:SegToNum = 8
        Case 111, 103:SegToNum = 9
        Case Else:SegToNum = -2
    End Select
End Function

Function PlayerScore(p) ' -1 if the display is not a plain score
    Dim d, n, s, started
    s = 0:started = False
    For d = p * 6 To p * 6 + 5
        n = SegToNum(rgSeg(d))
        If n = -2 Then PlayerScore = -1:Exit Function
        If n = -1 Then
            If started Then PlayerScore = -1:Exit Function ' blank inside a number
            n = 0
        Else
            started = True
        End If
        s = s * 10 + n
    Next
    PlayerScore = s
End Function

Function GoodScore(p)
    Dim d, s:s = 0
    For d = p * 6 To p * 6 + 5:s = s * 10 + rgGood(d):Next
    GoodScore = s
End Function

Sub AcceptPlayer(p)
    Dim d, n
    For d = p * 6 To p * 6 + 5
        n = SegToNum(rgSeg(d)):If n < 0 Then n = 0
        rgGood(d) = n
    Next
End Sub

Sub SetReelDigit(d, v) ' d = ROM digit 0-23, v = 0-9
    On Error Resume Next
    If rgText(d) = v Or rgText(d) = -1 Then Controller.B2SSetScoreDigit d + 1, (v + 1) Mod 10 ' force a repaint
    Controller.B2SSetScoreDigit d + 1, v
    rgText(d) = v
    On Error Goto 0
End Sub

Sub ReelGuardUpdate
    Dim ChgLED, ii, n, d, p, changed(3), nChanged, s, bad
    If rgState = 2 Then Exit Sub ' locked: leave the displays unread so the reels keep the final scores
    If rgState = 1 And GameTime - rgOffTime > ReelGuardLockMs Then rgState = 2:Exit Sub

    ChgLED = Controller.ChangedLEDs(0, &H3FFFFFF) ' digits 0-25: P1-P4 (0-23), ball (24), credits (25)
    If IsEmpty(ChgLED) Then Exit Sub
    n = -1:On Error Resume Next:n = UBound(ChgLED):On Error Goto 0
    If n < 0 Then Exit Sub
    For ii = 0 To n
        d = ChgLED(ii, 0)
        If d >= 0 And d <= 25 Then
            rgSeg(d) = ChgLED(ii, 2)
            If d <= 23 Then changed(d \ 6) = True
        End If
    Next

    nChanged = 0:bad = False
    For p = 0 To 3
        If changed(p) Then
            nChanged = nChanged + 1
            s = PlayerScore(p)
            If s < 0 Then
                bad = True
            ElseIf rgState = 1 And s < GoodScore(p) Then
                bad = True
            End If
        End If
    Next
    If rgState = 1 And nChanged > 1 Then bad = True

    If rgState = 0 Then
        For p = 0 To 3
            If changed(p) And PlayerScore(p) >= 0 Then AcceptPlayer p
        Next
    ElseIf bad Then
        ' end-of-game display started: put the last good scores back and lock
        For d = 0 To 23:SetReelDigit d, rgGood(d):Next
        rgState = 2
    Else
        For p = 0 To 3
            If changed(p) Then AcceptPlayer p
        Next
    End If
End Sub

Sub ReelGuardGameOn(Enabled)
    Dim d, n
    If Not (B2SOn And ReelGuard = 1) Then Exit Sub
    If Enabled Then
        If rgState <> 0 Then
            ' new game / next ball: catch up on everything that changed while locked
            rgState = 0
            ReelGuardUpdate
            For d = 0 To 23
                n = SegToNum(rgSeg(d)):If n < 0 Then n = 0
                rgGood(d) = n
                SetReelDigit d, n
            Next
        End If
    ElseIf rgState = 0 Then
        rgState = 1:rgOffTime = GameTime
    End If
End Sub

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
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

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
        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 15000 'increase the pitch on a ramp or elevated surface
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, ballpitch, 1, 0
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
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub