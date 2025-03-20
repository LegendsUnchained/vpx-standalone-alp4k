' Elvis - Stern 2004
' VPX 1.1.0 by JPSalas 2018

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const BallSize = 50

' Language Roms
Const cGameName = "elvis" 'English

Dim VarHidden, UseVPMColoredDMD
If Table1.ShowDT = true then
    UseVPMColoredDMD = true
    VarHidden = 1
else
    UseVPMColoredDMD = False
    VarHidden = 0
    TextBox1.Visible = 0
    lrail.Visible = 0
    rrail.Visible = 0
end if

LoadVPM "01560000", "SEGA.VBS", 3.26

'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = "fx_Solenoidon"
Const SSolenoidOff = "fx_Solenoidoff"
Const SCoin = "fx_Coin"

Dim bsTrough, mMag, mCenter, dtBankL, bsLock, bsUpper, bsJail, plungerIM, mElvis, x, i

'************
' Table init.
'************

Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = "elvis"
        .Games(cGameName).Settings.Value("sound") = 1 'enable the rom sound
        .SplashInfoLine = "Grease - By Damian" & vbNewLine & "VPX table by JPSalas v.1.1.0"
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .HandleKeyboard = 0
        .Hidden = VarHidden
        .Games(cGameName).Settings.Value("rol") = 0
        .Games(cGameName).Settings.Value("sound")=0
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description

        On Error Goto 0
    End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    vpmNudge.TiltSwitch = 56
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 14, 13, 12, 11, 0, 0, 0
        .InitKick BallRelease, 90, 4
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 4
    End With

    ' Magnet
    Set mMag = New cvpmMagnet
    With mMag
        .InitMagnet Magnet, 60
        .Solenoid = 5
        .GrabCenter = 1
        .CreateEvents "mMag"
    End With

    ' Droptargets
    set dtBankL = new cvpmdroptarget
    With dtBankL
        .initdrop array(sw17, sw18, sw19, sw20, sw21), array(17, 18, 19, 20, 21)
        .initsnd SoundFX("filleetedernier", DOFContactors), SoundFX("fx_resetdrop", DOFContactors)
    End With

    ' Hotel Lock
    Set bsLock = New cvpmBallStack
    With bsLock
        .InitSw 0, 48, 0, 0, 0, 0, 0, 0
        .InitKick HLock, 180, 8
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    End With

    ' Jail Lock
    Set bsJail = new cvpmBallStack
    With bsJail
        .InitSaucer sw34, 34, 186, 18
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("jailouse", DOFContactors)
        .KickAngleVar = 1
        .KickForceVar = 1
    End With

    ' Upper Lock
    Set bsUpper = new cvpmBallStack
    With bsUpper
        .InitSaucer sw32, 32, 90, 10
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .KickAngleVar = 3
        .KickForceVar = 3
    End With

    ' Impulse Plunger
    Const IMPowerSetting = 80 ' Plunger Power
    Const IMTime = 0.8        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .switch 16
        .InitExitSnd SoundFX("fx_autoplunger", DOFContactors), SoundFX("fx_autoplunger", DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Elvis movement
    Set mElvis = New cvpmMech
    With mElvis
        .MType = vpmMechOneSol + vpmMechReverse + vpmMechLinear
        .Sol1 = 25
        .Length = 200
        .Steps = 200
        .AddSw 33, 0, 0
        .Callback = GetRef("UpdateElvis")
        .Start
    End With
    ' Initialize Elvis arms and legs
    ElvisArms 0
    ElvisLegs 0
End Sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_Exit:Controller.Stop:End Sub
Sub table1_Exit():Controller.Games(cGameName).Settings.Value("sound")=1:Controller.Stop:End Sub

'****
'Keys
'****

Sub Table1_KeyDown(ByVal keycode)
	If KeyCode = RightMagnaSave Then NextTrack
    If KeyCode = LeftMagnaSave Then EndMusic
	If keycode = StartGameKey Then PlayMusic "Grease\intro.mp3"
    If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then PlaySound "fx_PlungerPull", 0, 1, 0.1, 0.25:Plunger.Pullback
    If KeyDownHandler(KeyCode) Then Exit Sub
    If keyUpperLeft Then Controller.Switch(55) = 1
    If keycode = KeyRules Then Rules
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If KeyUpHandler(KeyCode) Then Exit Sub
    If keycode = PlungerKey Then PlaySound "fx_plunger", 0, 1, 0.1, 0.25:Plunger.Fire
    If keyUpperLeft Then Controller.Switch(55) = 0
End Sub

'******************************
'*     HiRez00: Music Mod     *
'******************************

Dim musicNum
Sub NextTrack

If musicNum = 0 Then PlayMusic "Grease\01 - Grease.mp3" End If
If musicNum = 1 Then PlayMusic "Grease\02 - Summer Nights.mp3" End If
If musicNum = 2 Then PlayMusic "Grease\03 - Hopelessly Devoted To You.mp3" End If
If musicNum = 3 Then PlayMusic "Grease\04 - You're The One That I Want.mp3" End If
If musicNum = 4 Then PlayMusic "Grease\05 - Sandy.mp3" End If
If musicNum = 5 Then PlayMusic "Grease\06 - Beauty School Drop-Out.mp3" End If
If musicNum = 6 Then PlayMusic "Grease\07 - Look At Me, I'm Sandra Dee.mp3" End If
If musicNum = 7 Then PlayMusic "Grease\08 - Greased Lightning.mp3" End If
If musicNum = 8 Then PlayMusic "Grease\12 - Rock 'n' Roll Is Here To Stay.mp3" End If
If musicNum = 9 Then PlayMusic "Grease\13 - Those Magic Changes.mp3" End If
If musicNum = 10 Then PlayMusic "Grease\11 - Blue Moon.mp3" End If
If musicNum = 11 Then PlayMusic "Grease\14 - Hound Dog.mp3" End If
If musicNum = 12 Then PlayMusic "Grease\15 - Born To Hand Jive.mp3" End If
If musicNum = 13 Then PlayMusic "Grease\20 - There Are Worse Things I Could Do.mp3" End If
If musicNum = 14 Then PlayMusic "Grease\21 - Look At Me, I'm Sandra Dee (Reprise).mp3" End If
If musicNum = 15 Then PlayMusic "Grease\22 - We Go Together.mp3" End If
If musicNum = 16 Then PlayMusic "Grease\23 - Love Is A Many Splendored Thing (Instrumental).mp3" End If

musicNum = (musicNum + 1) mod 17
End Sub

Sub Table1_MusicDone
    NextTrack
End Sub

'*****************************
'*       End Music Mod       *
'*****************************

'**********************
'Elvis movement up/down
'**********************

Sub UpdateElvis(aNewPos, aSpeed, aLastPos)
    pStand.x = 518+aNewPos/3
    pLarm.x = 497+aNewPos/3
    pLegs.x = 516+aNewPos/3
    pRarm.x = 538+aNewPos/3
    pBody.x = 518+aNewPos/3
    pHead.x = 530+aNewPos/3
    pStand.y = 482+aNewPos
    pLarm.y = 486+aNewPos
    pLegs.y = 476+aNewPos
    pRarm.y = 474+aNewPos
    pBody.y = 482+aNewPos
    pHead.y = 535+aNewPos
End Sub

'********************
'     Flippers
'********************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipper1.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipper1.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipper1.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), RightFlipper
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

'*********
'Solenoids
'*********

SolCallBack(1) = "SolTrough"
SolCallBack(2) = "Auto_Plunger"
SolCallBack(3) = "dtBankL.SolDropUp"
SolCallBack(6) = "bsJail.SolOut"
SolCallBack(7) = "bsLock.SolOut"
SolCallBack(8) = "CGate.Open ="
SolCallBack(12) = "bsUpper.SolOut"
SolCallBack(19) = "SolHotelDoor"
SolCallBack(24) = "vpmsolsound SoundFX(""fx_knocker"",DOFKnocker),"

Sub SolTrough(Enabled)
    If Enabled Then
        bsTrough.ExitSol_On
        vpmTimer.PulseSw 15
    End If
End Sub

Sub Auto_Plunger(Enabled)
    If Enabled Then
        PlungerIM.AutoFire
    End If
End Sub

Sub SolHotelDoor(Enabled)
    If Enabled Then
        fDoor.RotatetoEnd
        doorwall.IsDropped = 1
	PlaySoundAt "fx_SolenoidOn", door
    Else
        fDoor.RotatetoStart
        doorwall.IsDropped = 0
	PlaySoundAt "fx_SolenoidOff", door
    End If
End Sub

'*********
' Flashers
'*********
SolCallBack(20) = "SetLamp 100,"
SolCallback(21) = "SetLamp 101,"
SolCallBack(22) = "SetLamp 102,"
SolCallBack(23) = "SetLamp 103,"
SolCallBack(32) = "SetLamp 104,"
SolCallBack(31) = "SetLamp 105,"

'****************
' Elvis animation
'****************

SolCallBack(29) = "ElvisLegs"
SolCallBack(30) = "ElvisArms"

Sub ElvisLegs(Enabled)
    If Enabled Then
        fLegs.RotatetoStart
    Else
        fLegs.RotatetoEnd
    End If
End Sub

Sub ElvisArms(Enabled)
    If Enabled Then
        fArms.RotatetoStart
    Else
        fArms.RotatetoEnd
    End If
End Sub

' ************************************
' Switches, bumpers, lanes and targets
' ************************************

Sub Drain_Hit:PlaySoundAt "fx_drain", Drain:bsTrough.AddBall Me:End Sub
'Sub Drain_Hit:Me.destroyball:End Sub 'debug

Sub sw9_Hit:vpmTimer.PulseSw 9:PlaySound SoundFX("popop", DOFTargets), 0, 1, pan(ActiveBall):End Sub

Sub sw10_Hit:Controller.Switch(10) = 1:End Sub
Sub sw10_unHit:Controller.Switch(10) = 0:End Sub
Sub sw17_Hit:dtBankL.Hit 1:End Sub
Sub sw18_Hit:dtBankL.Hit 2:End Sub
Sub sw19_Hit:dtBankL.Hit 3:End Sub
Sub sw20_Hit:dtBankL.Hit 4:End Sub
Sub sw21_Hit:dtBankL.Hit 5:End Sub
Sub sw22_Hit:vpmTimer.PulseSw 22:PlaySound SoundFX("dring", DOFTargets), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw23_Hit:vpmTimer.PulseSw 23:PlaySound SoundFX("dring", DOFTargets), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw24_Hit:vpmTimer.PulseSw 24:PlaySound SoundFX("dring", DOFTargets), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw25_Spin:PlaySoundAt "fx_spinner", sw25:vpmTimer.PulseSw 25:End Sub
Sub sw26_Hit:Controller.Switch(26) = 1:End Sub
Sub sw26_unHit:Controller.Switch(26) = 0:End Sub
Sub sw27_Hit:Controller.Switch(27) = 1:End Sub
Sub sw27_unHit:Controller.Switch(27) = 0:End Sub
Sub sw28_Hit:Controller.Switch(28) = 1:PlaySound "fx_metalrolling",0,1,pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw28_unHit:Controller.Switch(28) = 0:End Sub
Sub sw30_Hit:Controller.Switch(30) = 1:End Sub
Sub sw30_unHit:Controller.Switch(30) = 0:End Sub
Sub sw31_Hit:Controller.Switch(31) = 1:PlaySound "fx_metalrolling",0,1,pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw31_unHit:Controller.Switch(31) = 0:End Sub
Sub sw32_Hit:playsoundAt "fx_kicker_enter", sw32:bsUpper.AddBall 0:End Sub
Sub sw34_Hit:playsoundAt "fx_kicker_enter", sw34:bsJail.AddBall 0:End Sub
Sub sw36_Hit:vpmTimer.PulseSw 36:PlaySound SoundFX("guitare", DOFTargets), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw37_Hit:vpmTimer.PulseSw 37:PlaySound SoundFX("guitare", DOFTargets), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw38_Hit:vpmTimer.PulseSw 38:PlaySound SoundFX("guitare", DOFTargets), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw39_Hit:vpmTimer.PulseSw 39:PlaySound SoundFX("guitare", DOFTargets), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw40_Hit:vpmTimer.PulseSw 40:PlaySound SoundFX("hou", DOFTargets), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw41_Hit:Controller.Switch(41) = 1:PlaySound "toctoc", 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw41_UnHit:Controller.Switch(41) = 0:End Sub
Sub sw42_Hit:Controller.Switch(42) = 1:PlaySound "toctoc", 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw42_UnHit:Controller.Switch(42) = 0:End Sub
Sub sw43_Hit:Controller.Switch(43) = 1:PlaySound "toctoc", 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub
Sub sw45_Hit:Controller.Switch(45) = 1:End Sub
Sub sw45_unHit:Controller.Switch(45) = 0:End Sub
Sub sw46_Hit:Controller.Switch(46) = 1:End Sub
Sub sw46_unHit:Controller.Switch(46) = 0:End Sub
Sub sw47_Hit:Controller.Switch(47) = 1:End Sub
Sub sw47_unHit:Controller.Switch(47) = 0:End Sub
Sub HLock_Hit:playsoundAt "fx_kicker_enter", HLock:bsLock.AddBall Me:End Sub
Sub sw48_Hit:Controller.Switch(48) = 1:End Sub
Sub sw48_unHit:Controller.Switch(48) = 0:End Sub

'Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 49:PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 50:PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 51:PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub

Sub sw52_Hit:vpmTimer.PulseSw 52:PlaySound SoundFX("fx_target", DOFTargets), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw53_Hit:vpmTimer.PulseSw 53:PlaySound SoundFX("fx_target", DOFTargets), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw57_Hit::Controller.Switch(57) = 1:PlaySound "pulpfiction", 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
Sub sw58_Hit:Controller.Switch(58) = 1:PlaySound "yeye", 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub
Sub sw60_Hit:Controller.Switch(60) = 1:PlaySound "pulpfiction", 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw60_UnHit:Controller.Switch(60) = 0:End Sub
Sub sw61_Hit:Controller.Switch(61) = 1:PlaySound "yeye", 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub sw61_UnHit:Controller.Switch(61) = 0:End Sub

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_Slingshot", DOFContactors), Lemk
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 59
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
    PlaySoundAt SoundFX("fx_Slingshot", DOFContactors), Remk
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 62
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

Sub sw63_Hit:Controller.Switch(63) = 1:End Sub
Sub sw63_UnHit:Controller.Switch(63) = 0:PlaySoundAt "fx_balldrop",door:End Sub

Sub sw64_Hit
    vpmTimer.PulseSw 64
    str = INT(ABS(ActiveBall.Vely) ^3)
    debug.print str
    DogDir = 5 'upwards
    HoundDogTimer.Enabled = 1
    PlaySound SoundFX("rockroll", DOFTargets), 0, 1, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

' Animate Hound Dog
Dim str 'strength of the hit
Dim DogStep, DogDir
DogStep = 0
DogDir = 0

Sub HoundDogTimer_Timer()
    DogStep = DogStep + DogDir
    HoundDog.TransZ = DogStep
    If DogStep> 100 Then DogDir = -5
    If DogStep> str Then DogDir = -5
    If DogStep <5 Then HoundDogTimer.Enabled = 0
End Sub

' Ramp Soundss & Help triggers
Sub REnd1_Hit()
    StopSound "fx_metalrolling"
    PlaySound "fx_ballrampdrop", 0, 1, pan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub REnd2_Hit()
    StopSound "fx_metalrolling"
    PlaySound "fx_ballrampdrop", 0, 1, pan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'**********************************************************
'     JP's Flasher Fading for VPX and Vpinmame
'       (Based on Pacdude's Fading Light System)
' This is a fast fading for the Flashers in vpinmame tables
'  just 4 steps, like in Pacdude's original script.
' Included the new Modulated flashers & Lights for WPC
'**********************************************************

Dim FadingState(200)

InitLamps() ' turn off the lights and flashers and reset them to the default parameters

' vpinmame Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
            FadingState(chgLamp(ii, 0)) = chgLamp(ii, 1) + 3 'fading step
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
    Lampm 57, l57
    Flash 57, l57a
    Lampm 58, l58
    Flash 58, l58a
    Lampm 59, l59
    Flash 59, l59a
    Flash 60, l60
    Flash 61, l61
    Flash 62, l62
    Lamp 63, l63
    Lamp 64, l64
    Flash 65, l65
    Flash 66, l66
    Flash 67, l67
    Flash 68, l68
    Flash 69, l69
    Flashm 70, l70a
    Flash 70, l70
    Flashm 71, l71a
    Flash 71, l71
    Flashm 72, l72a
    Flash 72, l72
    Flash 73, l73
    Flash 74, l74
    Flash 75, l75
    Flash 76, l76
    Flash 77, l77
    Flash 78, l78
    'flashers
    Flashm 100, f20a
    Flashm 100, f20b
    Flashm 100, f20c
    Flash 100, f20
    Flash 101, f21
    Flash 102, f22
    Lampm 103, f23a
    Lamp 103, f23
    Flashm 104, f32a
    Flashm 104, f32b
    Flashm 104, f32c
    Flash 104, f32
    Flashm 105, f31a
    Flash 105, f31
End Sub

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
    LampTimer.Interval = 25 ' flasher fading speed
    LampTimer.Enabled = 1
    For x = 0 to 200
        FadingState(x) = 3 ' used to track the fading state
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

'Texts

Sub Text(nr, object, message)
    Select Case FadingState(nr)
        Case 4:object.Text = message:FadingState(nr) = 0
        Case 3:object.Text = "":FadingState(nr) = 0
    End Select
End Sub

Sub Textm(nr, object, b)
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
        Case 4:object.image = a:FadingState(nr) = 0                   'fading to off...
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
        Case 4:object.image = b:FadingState(nr) = 0 'off
        Case 3:object.image = a:FadingState(nr) = 0 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingState(nr)
        Case 4:object.image = a
        Case 3:object.image = b
    End Select
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

' Ramp Soundss & Help triggers
Sub REnd1_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAt "fx_ballrampdrop", REnd1
End Sub

Sub REnd2_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAt "fx_ballrampdrop", REnd2
End Sub

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
    PlaySound soundname, 0, 1, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

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
    Dim BOT, b, ballpitch, ballvol
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
        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
				ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 20000 'increase the pitch on a ramp
				ballvol = Vol(BOT(b)) *4
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
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
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******************
' RealTime Updates
'******************

Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates
    RollingUpdate
    door.RotX = fdoor.CurrentAngle
    pRarm.objRoty = fArms.CurrentAngle
    pLarm.objRoty = fArms.CurrentAngle
    pLegs.objRotY = fLegs.CurrentAngle
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
        x.Visible = ABS(Enabled)
    Next
End Sub

Sub Break_Hit
	ActiveBall.VelY = 0
	ActiveBall.VelX = 0
End Sub

'******
' Rules
'******
Dim Msg(20)
Sub Rules()
    Msg(0) = "Elvis - Stern 2004" &Chr(10) &Chr(10)
    Msg(1) = ""
    Msg(2) = "OBJECTIVE:Get to Graceland by lighting the following:"
    Msg(3) = "*FEATURED HITS COMPLETED (start all 5 song modes)"
    Msg(4) = "   Hound Dog (Shoot HOUND DOG Target)"
    Msg(5) = "   Blue Suede Shoes (Shoot CENTER LOOP with Upper Right Flipper)"
    Msg(6) = "   Heartbreak Hotel (Shoot balls into HEARTBREAK HOTEL on Upper Playfield)"
    Msg(7) = "   Jailhouse Rock (Shoot balls into the JAILHOUSE EJECT HOLE)"
    Msg(8) = "   All Shook Up (Shoot ALL-SHOOK shots)"
    Msg(9) = "*GIFTS FROM ELVIS COMPLETED ~ Shoot E-L-V-I-S Drop Targets to light GIFT"
    Msg(10) = " FROM ELVIS on the TOP EJECT HOLE"
    Msg(11) = "*TOP TEN COUNTDOWN COMPLETED ~ Shoot lit'music lights's to advance TOP"
    Msg(12) = " 10 COUNTDOWN."
    Msg(13) = "SKILL SHOT: Plunge ball in the WLVS Top Lanes or E-L-V-I-S Drop Targets"
    Msg(14) = "MYSTERY:Ball in the Pop Bumpers will change channels until all 3 TVs match."
    Msg(15) = "EXTRA BALL: Shoot Right Ramp ro light Extra Ball."
    Msg(16) = "TCB: Complete T-C-B to double all scoring."
    Msg(17) = "ENCORE: Spell E-N-C-O-R-E (letters lit in Back Panel) to earn"
    Msg(18) = "an Extra Multiball after the game."
    Msg(19) = ""
    Msg(20) = ""
    For X = 1 To 20
        Msg(0) = Msg(0) + Msg(X) &Chr(13)
    Next

    MsgBox Msg(0), , "         Instructions and Rule Card"
End Sub