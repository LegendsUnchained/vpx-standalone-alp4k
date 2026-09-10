' Ali / IPD No. 43 / Stern March, 1980 / 4 Players
' http://www.ipdb.org/machine.cgi?id=43
' VPX table by JPSalas 2017
' Dipswitches from the older table by Luvthatapex, Joe Entropy and Moonchild
' Press F7 for a friendly dipswitch, or F6 to edit manually the dipswitches
' DOF commands by arngrim
 
Option Explicit
Randomize
 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
 
Const BallSize = 50
 
LoadVPM "01120100", "stern.vbs", 3.02
 
Dim VarHidden
If Table1.ShowDT = true then
    VarHidden = 1
else
    VarHidden = 0
    For each x in aReels
        x.Visible = 0
    Next
    lrail.Visible = 0
    rrail.Visible = 0
end if
 
Dim bsTrough, dtTBank, dtLBank, bsHole1, bsHole2, bsHole3, bsRHole
Dim x, i, j, k 'used in loops
 
Const cGameName = "ali"
 
Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0
 
' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = ""
Const SCoin = "fx_coin"
 
'************
' Table init.
'************
 
Sub Table1_Init
    With Controller
        .GameName = cGameName
        .SplashInfoLine = "The Thing, TBA 2019" & vbNewLine & "VPX table by IVANTBA"
        .HandleMechanics = 0
        .HandleKeyboard = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .ShowTitle = 0
        .Hidden = 1
        .Games(cGameName).Settings.Value("sound") = 0
        If Err Then MsgBox Err.Description
    End With
    On Error Goto 0
    Controller.SolMask(0) = 0
    vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
    Controller.Run

PlaySound "Intro",-0

    ' Nudging
    vpmNudge.TiltSwitch = 7
    vpmNudge.Sensitivity = 0.1
    vpmNudge.TiltObj = Array(LBumper, BBumper, RBumper, LeftSlingshot, RightSlingshot)
 
    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        bsTrough.InitNoTrough BallRelease, 33, 115, 3
        .InitExitSnd "fx_ballrel", "fx_Solenoid"
        .Balls = 1
    End With
 
    ' Left Drop targets
    set dtLBank = new cvpmdroptarget
    With dtLBank
        .InitDrop Array(sw22, sw23, sw24), Array(22, 23, 24)
        .Initsnd SoundFX("fx_droptarget",DOFDropTargets), SoundFX("fx_resetdrop",DOFContactors)
    End With
 
    ' Top Drop targets
    set dtTBank = new cvpmdroptarget
    With dtTBank
        .InitDrop Array(sw19, sw20, sw21), Array(19, 20, 21)
        .Initsnd SoundFX("fx_droptarget",DOFDropTargets), SoundFX("fx_resetdrop",DOFContactors)
    End With
 
    ' Top Eject Hole 1
    Set bsHole1 = New cvpmBallStack
    With bsHole1
        .InitSaucer sw30, 30, 180, 10
        .InitExitSnd SoundFX("fx_kicker",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
        .KickForceVar = 3
        .KickAngleVar = 3
    End With
 
    ' Top Eject Hole 2
    Set bsHole2 = New cvpmBallStack
    With bsHole2
        .InitSaucer sw31, 31, 180, 10
        .InitExitSnd SoundFX("fx_kicker",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
        .KickForceVar = 3
        .KickAngleVar = 3
    End With
 
    ' Top Eject Hole 3
    Set bsHole3 = New cvpmBallStack
    With bsHole3
        .InitSaucer sw32, 32, 180, 10
        .InitExitSnd SoundFX("fx_kicker",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
        .KickForceVar = 3
        .KickAngleVar = 3
    End With
 
    ' Right Eject Hole
    Set bsRHole = New cvpmBallStack
    With bsRHole
        .InitSaucer sw38, 38, 180, 10
        .InitExitSnd SoundFX("fx_kicker",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
        .KickForceVar = 3
        .KickAngleVar = 3
    End With
 
    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
 
    ' Init Bumper Rings and targets
    AliKiOff 0:RHoleoff 0
End Sub
 
Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub

'**********
' Music
'**********

'There are two types of music to choose from: 

'OST "The Thing ost music.mp3" 
'METAL "The Thing metal music.mp3"

'Start the music

DIM music

music = "The Thing ost music.mp3"
PlayMusic "The Thing ost music.mp3" 

Sub Table1_MusicDone()
PlayMusic "The Thing ost music.mp3"
End Sub 

'**********
' Keys
'**********
 
Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge",0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge",0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge",0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then PlaySound "fx_PlungerPull", 0, 1, 0.1, 0.25:Plunger.Pullback
    If keycode = 65 then DipSwitchEditor ' F7
    If vpmKeyDown(keycode) Then Exit Sub
End Sub
 
Sub table1_KeyUp(ByVal Keycode)
    If vpmKeyUp(keycode) Then Exit Sub
    If keycode = PlungerKey Then PlaySound "fx_plunger", 0, 1, 0.1, 0.25:Plunger.Fire
End Sub
 
'*********
' Switches
'*********
 
' Slings & Rubbers
' Sub arubber_Hit(idx):vpmTimer.PulseSw 72:PlaySound "rubber":End Sub
 
Dim LStep, RStep
 
Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("fx_slingshot",DOFContactors), 0, 1, -0.05, 0.05
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 16
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
    PlaySound SoundFX("fx_slingshot",DOFContactors), 0, 1, 0.05, 0.05
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 15
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
 
' Bumpers
Sub LBumper_Hit:vpmTimer.PulseSw 13:PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0.15, 0.15:End Sub
Sub RBumper_Hit:vpmTimer.PulseSw 12:PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0.15, 0.15:End Sub
Sub BBumper_Hit:vpmTimer.PulseSw 14:PlaySound SoundFX("ar2_altfire",DOFContactors), 0, 1, 0.15, 0.15:End Sub
 
' Drain & holes
Sub Drain_Hit:Playsound "fx_drain":bsTrough.AddBall Me:End Sub
Sub sw30_Hit:bsHole1.AddBall 0:PlaySound "fx_kicker_enter":End Sub
Sub sw31_Hit:bsHole2.AddBall 0:PlaySound "fx_kicker_enter":End Sub
Sub sw32_Hit:bsHole3.AddBall 0:PlaySound "fx_kicker_enter":End Sub
Sub sw38_Hit:bsRHole.AddBall 0:PlaySound "fx_kicker_enter":End Sub
 
' Rollovers
Sub sw35_Hit:Controller.Switch(35) = 1:PlaySound "walkerdragpain02", 0, 1, pan(ActiveBall):End Sub
Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub
 
Sub sw37_Hit:Controller.Switch(37) = 1:PlaySound "reloadflamerthrower", 0, 1, pan(ActiveBall):CheckGREATEST:End Sub
Sub sw37_UnHit:Controller.Switch(37) = 0:End Sub
 
Sub sw36_Hit:Controller.Switch(36) = 1:PlaySound "fire_gust_05", 0, 1, pan(ActiveBall):CheckGREATEST:End Sub
Sub sw36_UnHit:Controller.Switch(36) = 0:End Sub
 
Sub sw34_Hit:Controller.Switch(34) = 1:PlaySound "walkerdragpain02", 0, 1, pan(ActiveBall):End Sub
Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub
 
Sub sw39_Hit:Controller.Switch(39) = 1:PlaySound "smashlight", 0, 1, pan(ActiveBall):End Sub
Sub sw39_UnHit:Controller.Switch(39) = 0:End Sub
 
Sub sw5_Hit:Controller.Switch(5) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):CheckGREATEST:End Sub
Sub sw5_UnHit:Controller.Switch(5) = 0:End Sub
 
Sub sw11_Hit:Controller.Switch(11) = 1:sw11l.State = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw11_UnHit:Controller.Switch(11) = 0:vpmTimer.AddTimer 250, "sw11l.State=":End Sub
 
Sub sw11a_Hit:Controller.Switch(11) = 1:sw11al.State = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw11a_UnHit:Controller.Switch(11) = 0:vpmTimer.AddTimer 250, "sw11al.State=":End Sub
 
Sub sw9_Hit:Controller.Switch(9) = 1:sw9l.State = 1:PlaySound "flareburn", 0, 1, pan(ActiveBall):End Sub
Sub sw9_UnHit:Controller.Switch(9) = 0:vpmTimer.AddTimer 250, "sw9l.State=":End Sub
 
' Droptargets
Sub sw22_Dropped:dtLBank.hit 1:PlaySound SoundFX("fx_droptarget",DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw23_Dropped:dtLBank.hit 2:PlaySound SoundFX("fx_droptarget",DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw24_Dropped:dtLBank.hit 3:PlaySound SoundFX("fx_droptarget",DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
 
Sub sw19_Dropped:dtTBank.hit 1:PlaySound SoundFX("fx_droptarget",DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw20_Dropped:dtTBank.hit 2:PlaySound SoundFX("fx_droptarget",DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw21_Dropped:dtTBank.hit 3:PlaySound SoundFX("fx_droptarget",DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
 
' Targets
Sub sw25_Hit:vpmTimer.PulseSw 25:PlaySound SoundFX("fx_target",DOFTargets), 0, 1, pan(ActiveBall):CheckGREATEST:End Sub
 
Sub sw26_Hit:vpmTimer.PulseSw 26:PlaySound SoundFX("fx_target",DOFTargets), 0, 1, pan(ActiveBall):CheckGREATEST:End Sub
 
Sub sw27_Hit:vpmTimer.PulseSw 27:PlaySound SoundFX("fx_target",DOFTargets), 0, 1, pan(ActiveBall):CheckGREATEST:End Sub
 
Sub sw28_Hit:vpmTimer.PulseSw 28:PlaySound SoundFX("fx_target",DOFTargets), 0, 1, pan(ActiveBall):CheckGREATEST:End Sub
 
Sub sw29_Hit:vpmTimer.PulseSw 29:PlaySound "fx_target", 0, 1, pan(ActiveBall):CheckGREATEST:End Sub
 
Sub CheckGREATEST
If (l4.State + l51.State +l35.State +l19.State +l3.State +l20.State +l36.State) = 7 Then
    GiEffect
End If
End Sub
 
' Gates
Sub Gate1_Hit():PlaySound "gate":End Sub
 
' Spinner
Sub Spinner1_Spin:vpmTimer.PulseSw 9:PlaySound "fx_spinner", 0, 1, -0.01:End Sub
 
'*********
'Solenoids
'*********
Solcallback(6) = "vpmsolsound SoundFX(""fx_knocker"",DOFKnocker),"
Solcallback(7) = "SolAliKickers"
SolCallback(8) = "dtTbank.SolDropUp"
SolCallback(9) = "dtLBank.SolDropUp"
Solcallback(10) = "SolRHole"
Solcallback(11) = "bsTrough.SolOut"
SolCallback(19) = "vpmNudge.SolGameOn"
 
Sub SolAliKickers(Enabled)
    If Enabled Then
        AliKiOn
        If bsHole1.Balls Then bsHole1.ExitSol_On
        If bsHole2.Balls Then bsHole2.ExitSol_On
        If bsHole3.Balls Then bsHole3.ExitSol_On
        vpmTimer.AddTimer 200, "AliKiOff"
    End If
End Sub
 
Sub AliKiOn():sw30i.IsDropped = 0:sw31i.IsDropped = 0:sw32i.IsDropped = 0:End Sub
Sub AliKiOff(dummy):sw30i.IsDropped = 1:sw31i.IsDropped = 1:sw32i.IsDropped = 1:End Sub
 
Sub SolRHole(Enabled)
    If Enabled Then
        sw38i.IsDropped = 0
        If bsRHole.Balls Then bsRHole.ExitSol_On
        vpmTimer.AddTimer 200, "RHoleOff"
    End If
End Sub
 
Sub RHoleoff(dummy):sw38i.IsDropped = 1:End Sub
 
'********************
'     Flippers
'********************
 
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
 
Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, 1, -0.1, 0.25
        LeftFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, -0.1, 0.25
        LeftFlipper.RotateToStart
    End If
End Sub
 
Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, 1, 0.1, 0.25
        RightFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, 0.1, 0.25
        RightFlipper.RotateToStart
    End If
End Sub
 
Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.25
End Sub
 
Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.25
End Sub
 
'**************
' Extra sounds
'**************
 
'************************************
'          LEDs Display
'************************************
 
Dim Digits(28)
 
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
 
Set Digits(18) = d0
Set Digits(19) = d1
Set Digits(20) = d2
Set Digits(21) = d3
Set Digits(22) = d4
Set Digits(23) = d5
 
Set Digits(24) = e0
Set Digits(25) = e1
Set Digits(26) = e2
Set Digits(27) = e3
 
Sub UpdateLeds
    '    On Error Resume Next
    Dim ChgLED, num, ii, jj, chg, stat
    ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
    If Not IsEmpty(ChgLED) Then
        For ii = 0 To UBound(ChgLED)
            num = chgLED(ii, 0):chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            Select Case stat
                Case 0:Digits(num).SetValue 0    'empty
                Case 63:Digits(num).SetValue 1   '0
                Case 6:Digits(num).SetValue 2    '1
                Case 91:Digits(num).SetValue 3   '2
                Case 79:Digits(num).SetValue 4   '3
                Case 102:Digits(num).SetValue 5  '4
                Case 109:Digits(num).SetValue 6  '5
                Case 124:Digits(num).SetValue 7  '6
                Case 125:Digits(num).SetValue 7  '6
                Case 252:Digits(num).SetValue 7  '6
                Case 7:Digits(num).SetValue 8    '7
                Case 127:Digits(num).SetValue 9  '8
                Case 103:Digits(num).SetValue 10 '9
                Case 111:Digits(num).SetValue 10 '9
                Case 231:Digits(num).SetValue 10 '9
                Case 128:Digits(num).SetValue 0  'empty
                Case 191:Digits(num).SetValue 1  '0
                Case 832:Digits(num).SetValue 2  '1
                Case 896:Digits(num).SetValue 2  '1
                Case 768:Digits(num).SetValue 2  '1
                Case 134:Digits(num).SetValue 2  '1
                Case 219:Digits(num).SetValue 3  '2
                Case 207:Digits(num).SetValue 4  '3
                Case 230:Digits(num).SetValue 5  '4
                Case 237:Digits(num).SetValue 6  '5
                Case 253:Digits(num).SetValue 7  '6
                Case 135:Digits(num).SetValue 8  '7
                Case 255:Digits(num).SetValue 9  '8
                Case 239:Digits(num).SetValue 10 '9
            End Select
        Next
    End IF
End Sub
 
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
 
Sub GiEffect
    For each x in aGiLights
        x.Duration 2, 3000, 1
    Next
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
'*********
 
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
LampTimer.Interval = 10 'lamp fading speed
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
 
    UpdateLeds
    UpdateLamps
    GIUpdate
    RollingUpdate
End Sub
 
Sub UpdateLamps
    NFadeL 1, l1
    NFadeL 2, l2
    NFadeL 3, l3
    NFadeL 4, l4
    NFadeL 5, l5
    NFadeL 6, l6
    NFadeL 7, l7
    NFadeL 8, l8
    '9
    NFadeL 10, l10
    NFadeL 11, l11
    NFadeL 12, l12
    NFadeL 14, l14
    NFadeL 15, l15
    '16
    NFadeL 17, l17
    NFadeL 18, l18
    NFadeL 19, l19
    NFadeL 20, l20
    NFadeL 21, l21
    NFadeL 22, l22
    NFadeL 23, l23
    '24
    '25
    'NFadeL 26, l26
    NFadeL 27, l27
    NFadeL 28, l28
    NFadeL 30, l30
    NFadeL 31, l31
    '32
    NFadeL 33, l33
    NFadeL 34, l34
    NFadeL 35, l35
    NFadeL 36, l36
    NFadeL 37, l37
    NFadeL 38, l38
    NFadeL 39, l39
    '40
    'NFadeL 41, l41
    NFadeL 42, l42
    NFadeL 43, l43
    NFadeL 44, l44
    NFadeL 46, l46
    NFadeL 47, l47
    '48
    NFadeL 49, l49
    NFadeL 50, l50
    NFadeL 51, l51
    '52
    NFadeL 53, l53
    NFadeL 54, l54
    NFadeL 55, l55
    '56
    NFadeL 57, l57
    NFadeL 58, l58
    NFadeL 59, l59
    NFadeL 60, l60
    NFadeL 62, l62
 
' backdrop lights
If VarHidden Then
    NFadeT 13, l13, "Highscore"
    NFadeT 29, l29, "Ball in Play"
    NFadeT 45, l45, "Game Over"
    NFadeT 61, l61, "Tilt"
    NFadeT 63, l63, "Match"
End If
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
 
Sub NFadeTm(nr, object, b)
    Select Case FadingLevel(nr)
        Case 4:object.Text = ""
        Case 5:object.Text = message
    End Select
End Sub
 
'******************************
' Diverse Collection Hit Sounds
'******************************
 
Sub aMetals_Hit(idx):PlaySound "fx_MetalHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_postrubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
 
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
 
'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************
 
Const tnob = 5 ' total number of balls in this table is 4, but always use a higher number here because of the timing
ReDim rolling(tnob)
InitRolling
 
Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub
 
Sub RollingUpdate()
    Dim BOT, b
    BOT = GetBalls
 
    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next
 
    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
 
    ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) )> 1 AND BOT(b).z <30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
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
 
'*******************************
' Dipswitches from the old table
'*******************************
 
Dim saveDips
saveDips = Array(191, 255, 7, 63)
 
Sub DipSwitchEditor()
    dim vpmDips, i, settings(3)
 
    'Save the settings I don't have code for
    for i = 0 to 3
        settings(i) = Controller.dip(i) and saveDips(i)
    next
 
    on error resume next
    set vpmDips = new cvpmDips
    with vpmDips
        .AddForm 315, 250, "ALI DIP Switch Settings"
        .AddFrame 2, 10, 140, "Balls per game", &H00000040,           _
            Array("3", 0, "5", &H00000040)
 
        .AddFrame 160, 10, 140, "GREATEST + ALI scores", &HC0000000,  _
            Array("Jack", 0, "Score", &H40000000,                     _
            "Extra Ball", &H80000000, "Special", &HC0000000)
 
        .AddFrame 2, 90, 140, "Specials per ball", &H00200000,        _
            Array("One", 0, "Unlimited", &H00200000)
 
        .AddFrame 160, 90, 140, "Specials", &H00400000,               _
            Array("Alternate", 0, "All light", &H00400000)
 
        .AddFrame 2, 140, 140, "GREATEST lights Special", &H00800000, _
            Array("2nd time", 0, "1st time", &H00800000)
 
        .AddChk 7, 200, 148, Array("Credit display", &H00080000)
        .AddChk 160, 200, 148, Array("Match", &H00100000)
        .AddLabel 7, 230, 300, 20, "After hitting OK, press F3 to reset game with new settings."
        .ViewDips
    end with
    if Err then DipSwitchDisplayError
    on error goto 0
 
    'Restore non-coded settings
    for i = 0 to 3
        Controller.dip(i) = settings(i) or((255-saveDips(i) ) and Controller.dip(i) )
    next
End Sub
 
Sub DipSwitchDisplayError()
    MsgBox "Can't display dip switch editor." & vbCRLF & vbCRLF &           _
        "Be sure you have wshLtWtForm.ocx loaded and registered" & vbCRLF & _
        "and vbs scripts 3.02 or higher." & vbCRLF & vbCRLF &               _
        "You may want to hit F6 to edit the switches manually."
End Sub

Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub