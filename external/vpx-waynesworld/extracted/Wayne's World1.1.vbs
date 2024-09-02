' Bally's Radical / IPD No. 1904 / September, 1990 / 4 Players
' This tabls is based on the Radical prototype.
' The prototype playfield included an additional slingshot and a vertical up-kicker not present on production games.
' VPX version by JPSalas, 2018, version 1.1.1
' "SKATE AND DESTROY" table mod  by kenji
' "WAYNE'S WORLD Table mod by Kara www.montetoncab.fr on 1 may 2020
Option Explicit
Randomize

Const BallSize = 50
Const BallMass = 1.4

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0


'START*********************************test nico 05-05-2020 
'init ballsaver
Sub walBallSaver_Init
	walBallSaver001_Init
	walBallSaver002_Init
End Sub
'END*********************************test nico 05-05-2020 

Dim bsTrough, dtBankM, dtBankT, bsLock, bsTP, plungerIM, x
'*******declaratiopn variable
Dim CoinPressed
Dim FlipperPressedL
Dim FlipperPressedR

Const cGameName = "radcl_l1c" 'Latest rom
'Const cGameName = "radcl_g1" 'German
'Const cGameName = "radcl_p3"  'Prototype home

Const UseSolenoids = 2 'Fastflips enabled
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 0
'***************nico18-05-20
'Dim Score
'PlaySound "1Retro",-1
'******apostrophe a retirer si music au lancement table
'LaunchMusic_Unhit
'****************fin nico 18-05-20

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
    lrail.Visible = 0
    rrail.Visible = 0
end if

if B2SOn = true then VarHidden = 1

LoadVPM "01550000", "S11.vbs", 3.26

' Standard Sounds
Const SSolenoidOn = "fx_solenoidon"
Const SSolenoidOff = "fx_solenoidoff"
Const SCoin = "5000"

'************
' Table init.
'************

Sub table1_Init
    vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Radical - Bally 1990" & vbNewLine & "VPX table by jpsalas v1.1.1"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 1
        .Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
.Games(cGameName).Settings.Value("sound")=0
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 1000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With




    ' Nudging
    vpmNudge.TiltSwitch = swTilt
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, Bumper4, LeftSlingshot, RightSlingshot, RightSlingshot1)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 10, 11, 12, 0, 0, 0, 0, 0
        .InitKick BallRelease, 80, 6
        '.InitEntrySnd "fx_drain", "fx_Solenoid"
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 2
    End With

    ' Visible Lock
    Set bsLock = new cvpmVLock
    With bsLock
        .InitVLock Array(sw40, sw39), Array(sw40k, sw39k), Array(40, 39)
        .InitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .ExitDir = 0
        .ExitForce = 21
        .CreateEvents "bsLock"
    End With

    ' Top Eject Hole 'Not available in the latest ROMs, we activate it manually
    Set bsTP = New cvpmBallStack
    With bsTP
    .InitSw 0, 16, 0, 0, 0, 0, 0, 0
    .InitKick sw16, 0, 55
	.KickZ = 1.5
    .InitExitSnd SoundFX("fx_popper", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    End With

    ' Drop Middle targets
    set dtbankM = new cvpmdroptarget
    With dtbankM
        .initdrop array(sw22, sw23, sw24), array(22, 23, 24)
        .initsnd SoundFX("fx_droptarget", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
        .CreateEvents "dtbankM"
    End With

    ' Drop Top targets
    set dtbankT = new cvpmdroptarget
    With dtbankT
        .initdrop array(sw30, sw31, sw32), array(30, 31, 32)
        .initsnd SoundFX("morp", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
        .CreateEvents "dtbankT"
    End With

    ' Impulse Plunger used for kickback
    Const IMPowerSetting = 42 'Plunger Power
    Const IMTime = 0.3        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP sw51, IMPowerSetting, IMTime
        .Random 0.3
        .switch 51
        .InitExitSnd "fx_Solenoid", "fx_Solenoid"
        .CreateEvents "plungerIM"
    End With

    ' Init diverters
    DiverterDown.IsDropped = 1
    RightDiverter1.IsDropped = 1

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Turn on Gi
    vpmTimer.AddTimer 2000, "GiOn '"
End Sub


Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.stop:End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)


'***************************************KIDS MOD


If keycode = AddCreditKey Then
	CoinPressed = True
	if ((CoinPressed = True ) And (FlipperPressedL = True)) then
	BallSaverMoveUp
	end If
    end if


   If keycode = LeftFlipperKey then 
		FlipperPressedL = True
		if ((CoinPressed = True ) And (FlipperPressedL = True)) then
		BallSaverMoveUp
	end If
end if
   If keycode = RightFlipperKey then
       FlipperPressedR = True
       if CoinPressed and FlipperPressedR then
       BallSaverMoveDown
       end if
    end if

 If keycode = StartGameKey then
       LaunchMusic_Unhit
       end if



'*******************fin nico

'ajout 02-05-2020
    If KeyCode = RightMagnaSave Then LaunchMusic_Unhit
'fin ajout 02-05-2020
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then PlaySound "fx_PlungerPull", 0, 1, 0.1, 0.25:Plunger.Pullback

    If vpmKeyDown(keycode)Then Exit Sub
End Sub


'******************************************
'***Ball Saver Handler	KID MODE	    ***
'******************************************

Sub walBallSaver_Init()
	walBallSaver.IsDropped = 1
End Sub

Sub BallSaverMoveUp()
		walBallSaver.IsDropped = False
		walBallSaver001.IsDropped = False
		walBallSaver002.IsDropped = False
        priBallSaver.TransY=0
		PlaySound "up"
End Sub

Sub BallSaverMoveDown()
		walBallSaver.IsDropped = True
		walBallSaver001.IsDropped = True
		walBallSaver002.IsDropped = True
        'etait a -48 mais chapeau visible donc --> -50
		priBallSaver.TransY=-50
		PlaySound "down"
End Sub

'******************************fin NICO

Sub table1_KeyUp(ByVal Keycode)


   If keycode = AddCreditKey then

       CoinPressed = False

    end if

   If keycode = LeftFlipperKey then

       FlipperPressedL = False

   end if


   If keycode = RightFlipperKey then

       FlipperPressedR = False

   end if


'***********************fin18-05-20

    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySound "fx_plunger", 0, 1, 0.1, 0.25:Plunger.Fire
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep, Rstep1

Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("fx_slingshot", DOFContactors), 0, 1, -0.05, 0.05
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 55
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
    PlaySound SoundFX("fx_slingshot", DOFContactors), 0, 1, 0.05, 0.05
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 56
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

Sub RightSlingShot1_Slingshot
    PlaySound SoundFX("fx_slingshot", DOFContactors), 0, 1, 0.05, 0.05
    r6.Visible = 1
    Remk1.RotX = 26
    RStep1 = 0
    vpmTimer.PulseSw 49
    RightSlingShot1.TimerEnabled = 1
End Sub

Sub RightSlingShot1_Timer
    Select Case RStep1
        Case 1:r6.Visible = 0:r4.Visible = 1:Remk1.RotX = 14
        Case 2:r4.Visible = 0:r3.Visible = 1:Remk1.RotX = 2
        Case 3:r3.Visible = 0:Remk1.RotX = -20:RightSlingshot1.TimerEnabled = 0
    End Select
    RStep1 = RStep1 + 1
End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 52:PlaySound SoundFX("aie", DOFContactors), 0, 1, -0.05:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 50:PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, -0.02:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 54:PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, -0.05:End Sub
Sub Bumper4_Hit:vpmTimer.PulseSw 53:PlaySound SoundFX("aie", DOFContactors), 0, 1, -0.02:End Sub

' Drain & Saucers
Sub Drain_Hit:Playsound "fx_drain" :bsTrough.AddBall Me:End Sub
Sub sw16a_Hit:PlaySound "fx_hole_enter", 0, 1, -0.02:bsTP.AddBall Me:vpmTimer.AddTimer 2500, "EjectBall":End Sub
Sub EjectBall(swno):PlaySound "fx_popper", 0, 1, -0.05:bsTP.ExitSol_On:End Sub
Sub RHelp_Hit:ActiveBall.Velx = 10:End Sub 'to ensure the ball always lands on the ramp
sub trigger001_hit: playsound "excel" end sub
' Rollovers
Sub sw51_Hit:Controller.Switch(51) = 1:PlaySound "voiture", 0, 1, pan(ActiveBall):End Sub
Sub sw51_UnHit:Controller.Switch(51) = 0: End Sub

Sub sw46_Hit:Controller.Switch(46) = 1:PlaySound "biba", 0, 1, pan(ActiveBall):End Sub
Sub sw46_UnHit:Controller.Switch(46) = 0:End Sub

Sub sw47_Hit:Controller.Switch(47) = 1:PlaySound "schwing", 0, 1, pan(ActiveBall):End Sub
Sub sw47_UnHit:Controller.Switch(47) = 0:End Sub

Sub sw48_Hit:Controller.Switch(48) = 1:PlaySound "worthy", 0, 1, pan(ActiveBall):End Sub
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub

Sub sw38_Hit:Controller.Switch(38) = 1:PlaySound "lepre", 0, 1, pan(ActiveBall):End Sub
Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub

Sub sw37_Hit:Controller.Switch(37) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw37_UnHit:Controller.Switch(37) = 0:End Sub

Sub sw15_Hit:Controller.Switch(15) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub

Sub sw27_Hit:Controller.Switch(27) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw27_UnHit:Controller.Switch(27) = 0:End Sub

Sub sw14_Hit:Controller.Switch(14) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub

Sub sw18_Hit:Controller.Switch(18) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub

Sub sw19_Hit:Controller.Switch(19) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw19_UnHit:Controller.Switch(19) = 0:End Sub

sub trigger002_hit
dim x
x = int(5 * rnd(1) )
select case x
case 0: playsound "monk"
case 1: playsound "wo2"
case 2: playsound "don"
case 3: playsound "exqu"
case 4: playsound "wo"
end Select
end sub

sub trigger003_hit
dim x
x = int(4 * rnd(1) )
select case x
case 0: playsound "denied"
case 1: playsound "worthy"
case 2: playsound "partytime"
case 3: playsound "babe"
end Select
end sub
dim trigger004
sub trigger004_hit:  playsound "partyon", 0,1, Pan(activeball) end sub 
 
sub trigger005_hit
dim x
x = int(5 * rnd(1) )
select case x
case 0: playsound "2friend"
case 1: playsound "sph"
case 2: playsound "biba"
case 3: playsound "denied"
case 4: playsound "not"
end Select
end sub 


'music start

Dim musicNum : Dim musicEnd
Sub LaunchMusic_Unhit

'If musicNum = 0 Then PlayMusic "WW-blind.mp3" end if
If musicNum = 0 Then PlayMusic "WW 07. Wayne and Garth.mp3" end if
If musicNum = 1 Then PlayMusic "WW 08. Tia Carrere - Ballroom Blitz.mp3" End If
If musicNum = 2 Then PlayMusic "WW Ugly Kid Joe - Everything About You.mp3" End If
If musicNum = 3 Then PlayMusic "WW 01. Queen - Bohemian Rhapsody.mp3" End If
If musicNum = 4 Then PlayMusic "WW 02. Aerosmith - Dude (Looks Like A Lady) (Live Version).mp3" End If
If musicNum = 5 Then PlayMusic "WW 05. Joan Jet And The Blackhearts - I Love Rock & Roll.mp3" End If
If musicNum = 6 Then PlayMusic "WW 05. Red Hot Chili Peppers - Sikamikanico.mp3" End If
If musicNum = 7 Then PlayMusic "WW 09. Golden Earring - Radar Love.mp3" End If
If musicNum = 8 Then PlayMusic "WW 09. Jimi Hendrix - Foxy Lady.mp3" End If
If musicNum = 9 Then PlayMusic "WW 10. Alice Cooper - Feed My Frankenstein.mp3" End If
If musicNum = 10 Then PlayMusic "WW Mrs. Robinson (Remastered).mp3" End If
If musicNum = 11 Then PlayMusic "WW Making Our Dreams Cyndi Grecco.mp3" End If
If musicNum = 12 Then PlayMusic "WW Hey Mickey.mp3" End If
'If musicNum = 13 Then PlayMusic "WW 00.mp3" End If
'If musicNum = 14 Then PlayMusic "WW 00.mp3" End If
'If musicNum = 15 Then PlayMusic "WW 00.mp3" End If
'If musicNum = 16 Then PlayMusic "WW 00.mp3" End If
'If musicNum = 17 Then PlayMusic "WW 00.mp3" End If
'If musicNum = 18 Then PlayMusic "WW 00.mp3" End If
'If musicNum = 19 Then PlayMusic "WW 00.mp3" End If
'If musicNum = 20 Then PlayMusic "WW 00.mp3" End If
'If musicNum = 21 Then PlayMusic "WW 00.mp3" End If
'If musicNum = 22 Then PlayMusic "WW 00.mp3" End If
'If musicNum = 23 Then PlayMusic "WW 00.mp3" End If
'If musicNum = 24 Then PlayMusic "WW 00.mp3" End If
'If musicNum = 25 Then PlayMusic "WW 00.mp3" End If
'musicNum = int(26 * rnd(1) )

musicNum = int(13 * rnd(1) )
End Sub

Sub Table1_MusicDone
    LaunchMusic_Unhit
End Sub




' Spinners
Sub Spinner1_Spin:vpmTimer.PulseSw 29:PlaySound "fx_spinner", 0, 1, 0.05:End Sub
Sub Spinner2_Spin:vpmTimer.PulseSw 17:PlaySound "fx_spinner", 0, 1, -0.05:End Sub

'Targets
Sub sw28_Hit:vpmTimer.PulseSw 28:PlaySound SoundFX("excel", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw43_Hit:vpmTimer.PulseSw 43:PlaySound SoundFX("morp", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw44_Hit:vpmTimer.PulseSw 44:PlaySound SoundFX("partyon", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub

'Droptargets (only sound effect)

Sub sw22_Hit:PlaySound SoundFX("excel", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw23_Hit:PlaySound SoundFX("sph", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw24_Hit:PlaySound SoundFX("don", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw30_Hit:PlaySound SoundFX("monk", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw31_Hit:PlaySound SoundFX("hiway", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw32_Hit:PlaySound SoundFX("not", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub

'*********
'Solenoids
'*********

SolCallback(1) = "bsTrough.SolIn"
SolCallback(2) = "bsTrough.SolOut"
SolCallback(3) = "bsTP.SolOut" 'Not used in the latest ROMs
SolCallback(4) = "dtBankT.SolDropUp"
SolCallback(6) = "dtBankM.SolDropUp"
SolCallback(5) = "VpmSolSound SoundFX(""fx_knocker"",DOFKnocker),"
SolCallback(7) = "SolRampDiv"
SolCallback(8) = "bsLock.SolExit" 'vlock
SolCallback(10) = "SolGI"
SolCallback(13) = "vpmSolToggleWall RightDiverter2,RightDiverter1,""fx_diverter"","
SolCallback(14) = "SolKickBack"
SolCallback(23) = "vpmNudge.SolGameOn"

' flashers
SolCallback(9) = "SetLamp 90,"
SolCallback(16) = "SetLamp 116,"
SolCallback(25) = "SetLamp 125,"
SolCallback(26) = "SetLamp 126,"
SolCallback(27) = "SetLamp 127,"
SolCallback(28) = "SetLamp 128,"
SolCallback(29) = "SetLamp 129,"
SolCallback(30) = "SetLamp 130,"
SolCallback(31) = "SetLamp 131,"
SolCallback(32) = "SetLamp 132,"

'Solenoid Subs

Sub SolRampDiv(Enabled)
    If Enabled Then
        If DiverterDown.IsDropped Then
            Controller.Switch(20) = 1
            DiverterDown.IsDropped = 0
            DiverterUp.IsDropped = 1
        Else
            Controller.Switch(20) = 0
            DiverterDown.IsDropped = 1
            DiverterUp.IsDropped = 0
        End If
    End If
End Sub

Sub SolKickBack(Enabled)
    If Enabled Then
        PlungerIM.AutoFire
    End If
End Sub

Sub SolGi(enabled)
    If enabled Then
        GiOff
    Else
        GiOn
    End If
End Sub

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

'**********************************************************
'     JP's Flasher Fading for VPX and Vpinmame v 1.0
'       (Based on Pacdude's Fading Light System)
' This is a fast fading for the Flashers in vpinmame tables
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
    UpdateLeds
End Sub

Sub UpdateLamps
    ' playfield lights
    Lamp 1, li1
    Lamp 2, li2
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
    Lamp 53, li53
    Lamp 54, li54
    Lamp 55, li55
    Lamp 56, li56
    Lamp 57, li57
    Lamp 58, li58
    Lamp 59, li59
    Lamp 60, li60
    Lamp 61, li61
    Lamp 62, li62
    Lamp 63, li63
    Lamp 64, li64

    'Flashers
    Flash 90, f9
    Lamp 116, f16
    Lampm 125, f25a
    Flash 125, f25
    Lampm 126, f26a
    Flash 126, f26
    Lampm 127, f27a
    Flash 127, f27
    Lampm 128, f28a
    Flash 128, f28
    Lampm 129, f29a
    Flash 129, f29
    Lampm 130, f30a
    Flash 130, f30
    Lampm 131, f31a
    Flash 131, f31
    Lampm 132, f32a
    Lampm 132, f32c
    Flashm 132, f32b
    Flash 132, f32
End Sub

' div lamp subs

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

'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
Dim Digits(32)
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
Digits(14) = Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
Digits(15) = Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)

Digits(16) = Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
Digits(17) = Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
Digits(18) = Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
Digits(19) = Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
Digits(20) = Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
Digits(21) = Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
Digits(22) = Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
Digits(23) = Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
Digits(24) = Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
Digits(25) = Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
Digits(26) = Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
Digits(27) = Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
Digits(28) = Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
Digits(29) = Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
Digits(30) = Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
Digits(31) = Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)

Sub UpdateLeds
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(chgLED)
            num = chgLED(ii, 0):chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            if(num <32)then
                For Each obj In Digits(num)
                    If chg And 1 Then obj.State = stat And 1
                    chg = chg \ 2:stat = stat \ 2
                Next
            Else
            end if
        Next
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
    LeftFlipper1Top.RotZ = LeftFlipper1.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
    RightFlipper1Top.RotZ = RightFlipper1.CurrentAngle
End Sub

'******************
' Rom sound ON wwhen close table
'******************
Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub