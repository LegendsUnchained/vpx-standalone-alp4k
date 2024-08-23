
'********************************************************************************************************
' release date 01/06/2022
'Re-animator, by balutito based on:
' Original Diamond Lady Table by JPsalas / IPD No. 678 / Premier February, 1988 / 4 Players
' Art and Layout mods by HauntFreaks
' RGB LED effects scripted by Ninuzzu
' Drop Target lighting by BorgDog
'*********************************************************************************************************

Option Explicit
Randomize

'optReset = True  'Uncomment to reset to default options in case of error or keep all changes temporary. When done recomment again!

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01210000", "sys80.vbs", 3.1

Dim bsTrough, dtLBank, dtRBank, dtMBank, dtCBank, bsTop, kickbackIM
Dim DesktopMode:DesktopMode=Table1.ShowDT

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
Const cGameName = "diamond"

Sub table1_Init
	Dim xx
    For each xx in aReels:xx.Visible = DesktopMode:Next
    lrail.Visible = DesktopMode
    rrail.Visible = DesktopMode
    With Controller
        .GameName = cGameName
        .Games(cGameName).Settings.Value("sound") = 1   'ensure the sound is on
        .SplashInfoLine = "Harley Quinn, (Gottlieb Diamond Lady retheme)" & vbNewLine & "VPX table by Hauntfreaks"
        .HandleMechanics = 0
        .HandleKeyboard = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .ShowTitle = 0
		.Hidden = 0
		.Games("diamond").Settings.Value("sound") = 0
		.Games("diamond").Settings.Value("samples") = 0

        If Err Then MsgBox Err.Description
		On Error Goto 0
		.SolMask(0) = 0
		vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
		.Run GetPlayerHWnd
     End With

    ' Nudging
    vpmNudge.TiltSwitch = 57
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)
 
    ' Trough & Ball Release
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 66, 0, 46, 0, 0, 0, 0, 0
        .InitKick BallRelease, 80, 6
        .InitExitSnd SoundFX("fx_ballrel",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
        .Balls = 2
    End With
 
    ' Top saucer
    Set bsTop = New cvpmBallStack
    With bsTop
        .InitSaucer sw74, 74, 300, 28
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    End With
 
    ' Left Drop targets
    set dtLBank = new cvpmdroptarget
    With dtLBank
        .InitDrop Array(sw20, sw30, sw40, sw50, sw60), Array(20, 30, 40, 50, 60)
        .initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    End With
 
    ' Middle Drop targets
    set dtMBank = new cvpmdroptarget
    With dtMBank
        .InitDrop Array(sw21, sw31, sw41, sw51), Array(21, 31, 41, 51)
        .initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    End With
 
    ' Right Drop targets
    set dtRBank = new cvpmdroptarget
    With dtRBank
        .InitDrop Array(sw22, sw32, sw42, sw52, sw62), Array(22, 32, 42, 52, 62)
        .initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    End With
 
    ' Center Drop target
    set dtCBank = new cvpmdroptarget
    With dtCBank
        .InitDrop sw23, 23
        .initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    End With
 
    ' Impulse Plunger used as the left kickback
    Set kickbackIM = New cvpmImpulseP
    With kickbackIM
        .InitImpulseP swKickback, 26, 0.4
        .Random 0.6
        '.Switch 43
        .InitExitSnd "fx_popper", "fx_popper"
        .CreateEvents "kickbackIM"
    End With
 
    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
End Sub
 
Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
 
'**********
' Keys
'**********
 
Sub table1_KeyDown(ByVal Keycode)
    If KeyCode = RightFlipperKey Then Controller.Switch(72) = 1
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then PlaySound "fx_PlungerPull", 0, 1, 0.1, 0.25:Plunger.Pullback
    If vpmKeyDown(keycode)Then Exit Sub
    If keycode = KeyRules Then Rules
End Sub
 
Sub table1_KeyUp(ByVal Keycode)
    If KeyCode = RightFlipperKey Then Controller.Switch(72) = 0
    If keycode = PlungerKey Then PlaySound "fx_plunger", 0, 1, 0.1, 0.25:Plunger.Fire
    If vpmKeyUp(keycode)Then Exit Sub
End Sub
 


'**********
' Music Simple
'**********

'Start the music

DIM music

music = "reanimator.mp3"
PlayMusic "reanimator.mp3" 

Sub Table1_MusicDone()
PlayMusic "reanimator.mp3"
End Sub 

'*********
' Switches
'*********
 
' Slings & div switches
Dim LStep, RStep
 
Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("fx_slingshot", DOFContactors), 0, 1, -0.05, 0.05
    DOF 103, DOFPulse
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 33
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
    PlaySound SoundFX("fx_slingshot", DOFContactors), 0, 1, 0.05, 0.05
    DOF 104, DOFPulse
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 33
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
Sub Bumper1_Hit:vpmTimer.PulseSw 61:PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0, 0.1:DOF 105, DOFPulse:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 71:PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0, 0.1:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 61:PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0, 0.1:DOF 107, DOFPulse:End Sub
 
' Drain holes
Sub Drain_Hit:Playsound "fx_drain":bsTrough.AddBall Me:End Sub
 
' Rollovers
Sub sw43_Hit:Controller.Switch(43) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub
 
Sub sw53_Hit:Controller.Switch(53) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw53_UnHit:Controller.Switch(53) = 0:End Sub
 
Sub sw63_Hit:Controller.Switch(63) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw63_UnHit:Controller.Switch(63) = 0:End Sub
 
Sub sw73_Hit:Controller.Switch(73) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw73_UnHit:Controller.Switch(73) = 0:End Sub
 
Sub sw65_Hit:Controller.Switch(65) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw65_UnHit:Controller.Switch(65) = 0:End Sub
 
Sub sw70_Hit:Controller.Switch(70) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw70_UnHit:Controller.Switch(70) = 0:End Sub
 
Sub sw45_Hit:Controller.Switch(45) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw45_UnHit:Controller.Switch(45) = 0:End Sub
 
Sub sw55_Hit:Controller.Switch(55) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw55_UnHit:Controller.Switch(55) = 0:End Sub


Sub Trigger1_Hit:Controller.Switch(70) = 1:PlaySound "fx_ballrampdrop", 0, 1, pan(ActiveBall):End Sub
Sub Trigger1_UnHit:Controller.Switch(70) = 0:End Sub
 
' Ramp Switches
Sub sw44_Hit():Controller.Switch(44) = 1:End Sub
Sub sw44_UnHit:Controller.Switch(44) = 0:End Sub
 
'Spinners
Sub sw64_Spin:vpmTimer.PulseSw 64:PlaySound "fx_spinner", 0, 1, 0.1:End Sub
Sub sw54_Spin:vpmTimer.PulseSw 54:PlaySound "fx_spinner", 0, 1, -0.1:End Sub
 
' Droptargets
Sub sw20_Hit:dtLBank.hit 1:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw30_Hit:dtLBank.hit 2:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw40_Hit:dtLBank.hit 3:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw50_Hit:dtLBank.hit 4:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw60_Hit:dtLBank.hit 5:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw22_Hit:dtRBank.hit 1:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw32_Hit:dtRBank.hit 2:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw42_Hit:dtRBank.hit 3:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw52_Hit:dtRBank.hit 4:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw62_Hit:dtRBank.hit 5:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw21_Hit:dtMBank.hit 1:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw31_Hit:dtMBank.hit 2:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw41_Hit:dtMBank.hit 3:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw51_Hit:dtMBank.hit 4:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw23_Hit:dtCBank.hit 1:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
 
' Targets
Sub sw24_Hit:vpmTimer.PulseSw 24:PlaySound SoundFX("fx_target", DOFTargets), 0, 1, pan(ActiveBall):End Sub
 
Sub sw34_Hit:vpmTimer.PulseSw 34:PlaySound SoundFX("fx_target", DOFTargets), 0, 1, pan(ActiveBall):End Sub
 
Sub sw26_Hit:vpmTimer.PulseSw 26:PlaySound SoundFX("fx_target", DOFTargets), 0, 1, pan(ActiveBall):End Sub
 
Sub sw36_Hit:vpmTimer.PulseSw 36:PlaySound SoundFX("fx_target", DOFTargets), 0, 1, pan(ActiveBall):End Sub
 
Sub sw25_Hit:vpmTimer.PulseSw 25:PlaySound SoundFX("fx_target", DOFTargets), 0, 1, pan(ActiveBall):End Sub
 
Sub sw35_Hit:vpmTimer.PulseSw 35:PlaySound SoundFX("fx_target", DOFTargets), 0, 1, pan(ActiveBall):End Sub
 
' VUK & Holes
 
Sub sw74_Hit:PlaySound "fx_kicker_enter", 0, 1, 0.01:bsTop.AddBall 0:End Sub
 
'********************
'Solenoids & Flashers
'********************

SolCallback(2) = "dtMBank.SolDropUp"
SolCallback(3) = "SetLamp 103," 'Left Spinner Flasher
SolCallback(4) = "SetLamp 104," 'Right Orange Flashers
SolCallback(5) = "dtRBank.SolDropUp"
SolCallback(6) = "dtLBank.SolDropUp"
SolCallback(7) = "SetLamp 107," 'Right Red Flashers
SolCallback(8) = "vpmSolSound SoundFX(""fx_knocker"",DOFKnocker),"
SolCallback(9) = "bsTrough.SolIn"
SolCallback(10) = "dtCBank.SolDropUp"

'**************
' Flipper Subs
'**************
 
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
 
Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFFlippers), 0, 1, -0.1, 0.1
        LeftFlipper1.RotateToEnd
        LeftFlipper2.RotateToEnd
        LeftFlipper3.RotateToEnd
        LeftFlipper4.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFFlippers), 0, 1, -0.1, 0.1
        LeftFlipper1.RotateToStart
        LeftFlipper2.RotateToStart
        LeftFlipper3.RotateToStart
        LeftFlipper4.RotateToStart
    End If
End Sub
 
Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFFlippers), 0, 1, 0.1, 0.1
        RightFlipper1.RotateToEnd
        RightFlipper2.RotateToEnd
        RightFlipper3.RotateToEnd
        RightFlipper4.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFFlippers), 0, 1, 0.1, 0.1
        RightFlipper1.RotateToStart
        RightFlipper2.RotateToStart
        RightFlipper3.RotateToStart
        RightFlipper4.RotateToStart
    End If
End Sub
 
Sub LeftFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.25
End Sub
 
Sub RightFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.25
End Sub
 
'*****************************
'Extra Lamps used as solenoids
'Based on destruk's code
'adapted to the fading lights
'*****************************
 
Dim NewL12, OldL12, NewL2, OldL2, NewL13, OldL13, NewL14, OldL14, NewL15, OldL15, NewL16, OldL16, NewL17, OldL17, NewL18, OldL18
OldL12 = 0:OldL2 = 0:OldL13 = 0:OldL14 = 0:OldL15 = 0:OldL16 = 0:OldL17 = 0:OldL18 = 0
 
Set LampCallback = GetRef("ExtraLamps")
Sub ExtraLamps
    'Aux Relay
    NewL12 = Controller.Lamp(12)
    If NewL12 <> OldL12 Then
        If NewL12 Then
            Aux.Enabled = 1
        Else
            Aux.Enabled = 0
            SetLamp 111, 0:SetLamp 112, 0:SetLamp 113, 0:SetLamp 114, 0:SetLamp 115, 0
        End If
    End If
    OldL12 = NewL12
 
    'Ball Release
    NewL2 = Controller.Lamp(2)
    If NewL2 <> OldL2 Then
        If NewL2 Then bsTrough.ExitSol_On
    End If
    OldL2 = NewL2
 
    'Top Kicker
    NewL13 = Controller.Lamp(13)
    If NewL13 <> OldL13 Then
        If NewL13 Then bsTop.ExitSol_On
    End If
    OldL13 = NewL13
 
    'Kickback
    NewL14 = Controller.Lamp(14)
    If NewL14 <> OldL14 Then
        If NewL14 Then kickbackIM.AutoFire:DOF 108, DOFPulse
    End If
    OldL14 = NewL14
 
    '#1 Drop Target Trip Coil
    NewL15 = Controller.Lamp(15)
    If NewL15 <> OldL15 Then
        If NewL15 Then
            dtLBank.Hit 2
            dtLBank.Hit 3
            dtLBank.Hit 4
        End If
    End If
    OldL15 = NewL15
 
    '#2 Drop Target Trip Coil
    NewL16 = Controller.Lamp(16)
    If NewL16 <> OldL16 Then
        If NewL16 Then
            dtMBank.Hit 2
            dtMBank.Hit 3
        End If
    End If
    OldL16 = NewL16
 
    '#3 Drop Target Trip Coil
    NewL17 = Controller.Lamp(17)
    If NewL17 <> OldL17 Then
        If NewL17 Then
            dtRBank.Hit 2
            dtRBank.Hit 3
            dtRBank.Hit 4
        End If
    End If
    OldL17 = NewL17
 
    '#4 Drop Target Trip Coil
    NewL18 = Controller.Lamp(18)
    If NewL18 <> OldL18 Then
        If NewL18 Then dtCBank.Hit 1
    End If
    OldL18 = NewL18
End Sub
 
Dim AuxCount:AuxCount = 0
Sub Aux_Timer
    Select Case AuxCount
        Case 0:SetLamp 115, 1:SetLamp 111, 0
        Case 1:SetLamp 114, 1:SetLamp 115, 0
        Case 2:SetLamp 113, 1:SetLamp 114, 0
        Case 3:SetLamp 112, 1:SetLamp 113, 0
        Case 4:SetLamp 111, 1:SetLamp 112, 0
    End Select
    AuxCount = AuxCount + 1
    If AuxCount = 5 then AuxCount = 0
End Sub
 
'*****************
'   Gi
'*****************

Sub GIUpdate
    Dim tmp, x
    tmp = Getballs
	If UBound(tmp) = -1 Then
		For each x in aGiLights:x.State = 0:Next
		SetLamp 199,0
		If RGBGI=1 Then RGBTimer.Enabled=0
	Else
		If RGBGI=1 Then RGBTimer.Enabled=1
		For each x in aGiLights:x.State = 1:Next
		SetLamp 199,1

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
LampTimer.Interval = 50 'lamp fading speed
LampTimer.Enabled = 1
 
' Lamp & Flasher Timers
 
Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0)) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0)) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
    If DesktopMode or B2SOn Then UpdateLeds
    UpdateLamps
    GIUpdate
    RollingUpdate
End Sub
 
Sub UpdateLamps
    NFadeLm 3, l3a
    NFadeL 3, l3
    NFadeL 5, l5
    NFadeL 6, l6
    NFadeL 7, l7
    NFadeL 8, l8
    NFadeL 9, l9
    NFadeL 10, l10
    NFadeL 11, l11
    NFadeLm 19, l19b
    NFadeLm 19, l19
    Flash 19, l19a
    NFadeLm 20, l20
    NFadeLm 20, l20a
    Flashm 20, f20a
    Flash 20, l21
    Flash 21, l21
    Flash 22, l22
    Flash 23, l23
    Flash 24, l24
    Flash 25, l25
    Flash 26, l26
    Flash 27, l27
    NFadeL 28, l28
    NFadeL 29, l29
    NFadeL 30, l30
    NFadeLm 31, l31
    NFadeLm 31, l31b
    NFadeLm 31, l31d
    Flashm 31, f31b
    Flash 31, f31d
    NFadeL 32, l32
    NFadeL 33, l33
    NFadeL 34, l34
    NFadeL 35, l35
    NFadeL 36, l36
    NFadeL 37, l37
    NFadeL 38, l38
    NFadeL 39, l39
'NfadeL 40, l40
    NFadeLm 41, l41
    Flash 41, f41a
    NFadeLm 42, l42
    Flash 42, f42
    NFadeLm 43, l43
    Flash 43, f43
    NFadeL 44, l44
    NFadeLm 45, l45
    NFadeL 45, l45b
    NFadeL 46, l46
    NFadeL 47, l47
'NfadeL 48, l48
'NfadeL 49, l49
'NfadeL 50, l50
    NFadeL 51, l51
    Flashm 199, f50
    Flash 199, f50a

	If RGBInserts=1 Then
		RGBSimple 3, l3a
		RGBSimple 3, l3
		RGBSimple 5, l5
		RGBSimple 6, l6
		RGBSimple 7, l7
		RGBSimple 8, l8
		RGBSimple 9, l9
		RGBSimple 10, l10
		RGBSimple 11, l11
		RGBSimple 19, l19b
		RGBSimple 19, l19
		RGBSimple 19, l19a
		RGBSimple 20, l20
		RGBSimple 20, l20a
		RGBSimple 20, f20a
		RGBSimple 28, l28
		RGBSimple 29, l29
		RGBSimple 30, l30
		RGBSimple 31, l31
		RGBSimple 31, l31b
		RGBSimple 31, l31d
		RGBSimple 31, f31b
		RGBSimple 31, f31d
		RGBSimple 32, l32
		RGBSimple 33, l33
		RGBSimple 34, l34
		RGBSimple 35, l35
		RGBSimple 36, l36
		RGBSimple 37, l37
		RGBSimple 38, l38
		RGBSimple 39, l39
		RGBSimple 41, l41
		RGBSimple 41, f41a
		RGBSimple 42, l42
		RGBSimple 42, f42
		RGBSimple 43, l43
		RGBSimple 43, f43
		RGBSimple 44, l44
		RGBSimple 45, l45
		RGBSimple 45, l45b
		RGBSimple 46, l46
		RGBSimple 47, l47
		RGBSimple 51, l51
	End If
    'flashers
    NFadeLm 103, f31
    NFadeLm 103, f3
    Flashm 103, f31a
    Flash 103, f31c
    Flashm 104, f41
    Flashm 104, f4a
    Flash 104, f4b
    Flashm 107, f7
    Flashm 107, f7a
    Flash 107, f7b
    ' Aux lights
    NFadeL 111, Aux1
    NFadeL 112, Aux2
    NFadeL 113, Aux3
    NFadeL 114, Aux4
    NFadeL 115, Aux5

End Sub
Dim RGBColor

Sub RGBSimple (nr,obj)
	If Lampstate(nr)=1 Then
	RGBColor=RndNum(1,3)
		Select Case RGBColor
			Case 1
			If TypeName(obj)="Light" Then obj.colorFull=RGB(255,0,0)
			If TypeName(obj)="Flasher" Then obj.color=RGB(255,0,0)
			Case 2
			If TypeName(obj)="Light" Then obj.colorFull=RGB(0,255,0)
			If TypeName(obj)="Flasher" Then obj.color=RGB(0,255,0)
			Case 3
			If TypeName(obj)="Light" Then obj.colorFull=RGB(0,0,255)
			If TypeName(obj)="Flasher" Then obj.color=RGB(0,0,255)
		End Select
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
    If value <> LampState(nr)Then
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
            FlashLevel(nr) = FlashLevel(nr)- FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr)Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr)Then
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
            FlashLevel(nr) = FlashLevel(nr)- FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr)Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
            If FadingLevel(nr) = 0 AND FlashRepeat(nr)Then 'repeat the flash
                FlashRepeat(nr) = FlashRepeat(nr)-1
                If FlashRepeat(nr)Then FadingLevel(nr) = 5
            End If
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr)Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
            If FadingLevel(nr) = 1 AND FlashRepeat(nr)Then FadingLevel(nr) = 4
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

'*****************
' Leds Display
'*****************
 
Dim Digits(40)
 
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
 
Digits(32) = Array(c00, c05, c0c, c0d, c08, c01, c06, c0f, c02, c03, c04, c07, c0b, c0a, c09, c0e)
Digits(33) = Array(c10, c15, c1c, c1d, c18, c11, c16, c1f, c12, c13, c14, c17, c1b, c1a, c19, c1e)
Digits(34) = Array(c20, c25, c2c, c2d, c28, c21, c26, c2f, c22, c23, c24, c27, c2b, c2a, c29, c2e)
Digits(35) = Array(c30, c35, c3c, c3d, c38, c31, c36, c3f, c32, c33, c34, c37, c3b, c3a, c39, c3e)
Digits(36) = Array(c40, c45, c4c, c4d, c48, c41, c46, c4f, c42, c43, c44, c47, c4b, c4a, c49, c4e)
Digits(37) = Array(c50, c55, c5c, c5d, c58, c51, c56, c5f, c52, c53, c54, c57, c5b, c5a, c59, c5e)
Digits(38) = Array(c60, c65, c6c, c6d, c68, c61, c66, c6f, c62, c63, c64, c67, c6b, c6a, c69, c6e)
Digits(39) = Array(c70, c75, c7c, c7d, c78, c71, c76, c7f, c72, c73, c74, c77, c7b, c7a, c79, c7e)
 
Sub UpdateLeds
    Dim ChgLED, ii, num, chg, stat, obj
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
 
'******************************
' Diverse Collection Hit Sounds
'******************************
 
Sub aMetals_Hit(idx):PlaySound "fx_MetalHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_rubber_post", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_rubber_pin", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
 
' Ramp Soundss
Sub RHelp1_Hit()
    'StopSound "fx_metalrolling"
    PlaySound "fx_ballrampdrop", 0, 1, pan(ActiveBall)
End Sub
 
Sub RHelp2_Hit()
    'StopSound "fx_metalrolling"
    PlaySound "fx_ballrampdrop", 0, 1, pan(ActiveBall)
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
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2)))
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

Function RndNum(min,max)
	RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min AND max
End Function
'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 3 ' total number of balls
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
    If UBound(BOT) = - 1 Then Exit Sub 'there no extra balls on this table
 
    ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
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

'*****************************************
'			FLIPPER SHADOWS
'*****************************************

Sub FlipperTimer_Timer()
	If FlipperType > 0 Then
		' *** move glowbats ***
		GlowBatLightLeft.y = 1535 - 121 + LeftFlipper1.CurrentAngle
		glowbatleft.objrotz = LeftFlipper1.CurrentAngle
		GlowBatLightRight.y =1535 - 121 - RightFlipper1.CurrentAngle
		glowbatright.objrotz = RightFlipper1.CurrentAngle
	Else
		' *** move flipper shadows ***
		FlipperLSh.RotZ = LeftFlipper1.currentangle
		FlipperRSh.RotZ = RightFlipper1.currentangle
	End If

'	****************** drop target shadow lighting
	if sw41.isdropped then
		Lsw41.state=gi16.state
	  Else
		Lsw41.state=0
	end If
	if sw31.isdropped then
		Lsw31.state=gi16.state
	  Else
		Lsw31.state=0
	end if
	if sw20.isdropped Then
		Lsw20.state=gi9.state
	  Else
		Lsw20.state=0
	end If
	if sw40.isdropped Then
		Lsw40.state=gi9.state
	  Else
		Lsw40.state=0	
	end If
	if sw50.isdropped Then
		Lsw50.state=gi9.state
	  Else
		Lsw50.state=0
	end If
	if sw60.isdropped Then
		Lsw60.state=gi9.state
	  else
		Lsw60.state=0
	end if
	if sw62.isdropped Then
		Lsw62.state=gi10.state
	  else
		Lsw62.state=0
	end if
	if sw52.isdropped Then
		Lsw52.state=gi11.state
	  else
		Lsw52.state=0
	end if
	if sw42.isdropped Then
		Lsw42.state=gi11.state
	  else
		Lsw42.state=0
	end if
	if sw32.isdropped Then
		Lsw32.state=gi11.state
	  else
		Lsw32.state=0
	end if
End Sub

'*****************************************
'			BALL SHADOW & GLOWING BALL
'*****************************************
ReDim BallShadow(tnob-1), Glowing(tnob-1)
InitShadowAndGlow

Sub InitShadowAndGlow
	Dim i:For i = 0 to tnob-1
		ExecuteGlobal "Set Glowing("&i&") = Glowball"&(i+1)&" :"
		ExecuteGlobal "Set BallShadow("&i&") = BallShadow"&(i+1)&" :"
	Next
End Sub

Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls
    If UBound(BOT)<(tnob-1) Then						' hide shadow and switch off glowlight of deleted balls
		For b = (UBound(BOT) + 1) to (tnob-1)
		If IsGlowBall Then Glowing(b).state = 0 Else BallShadow(b).visible = 0 End If
        Next
    End If

    If UBound(BOT) = -1 Then Exit Sub		   			 ' exit the Sub if no balls on the table

    For b = 0 to UBound(BOT)
		If IsGlowBall Then								' move glowball light
			If Glowing(b).state = 0 Then
				Glowing(b).state = 1
				Select Case BallType
					Case 2	'green GlowBall
							Glowing(b).color = RGB(100, 255, 100): Glowing(b).colorfull = RGB(100, 255, 100)
					Case 3	'blue GlowBall
							Glowing(b).color = RGB(100, 100, 255): Glowing(b).colorfull = RGB(100, 100, 255)
					Case 4	'orange GlowBall
							Glowing(b).color = RGB(255, 0, 72): Glowing(b).colorfull = RGB(255, 0, 72)
				End Select
			End If
			Glowing(b).BulbHaloHeight = BOT(b).z + 51
			Glowing(b).x = BOT(b).x : Glowing(b).y = BOT(b).y + 15
		Else					    					' render the shadow for each ball
			If BOT(b).X < Table1.Width/2 Then
				BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 13
			Else
				BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 13
			End If
			ballShadow(b).Y = BOT(b).Y + 10
			If BOT(b).Z > 20 Then
				BallShadow(b).visible = 1
			Else
				BallShadow(b).visible = 0
			End If
		End If
    Next
End Sub

'******************************************
' Rules - from Inkochnito instruction cards
'******************************************
 
Dim Msg(20)
Sub Rules()
    Msg(0) = "HOW TO PLAY HARLEY QUINN" &Chr(10)&Chr(10)
    Msg(1) = ""
    Msg(2) = "SPECIALS: A. JOKER SPECIAL LIT BY COMPLETING ROYAL FLUSH."
    Msg(3) = "B. DIAMOND AND RIGHT OUTLANE LIT BY COMPLETING ALL DIAMONDS. "
    Msg(4) = "C. LIT TOP ROLLOVER AT 10X (MULTIPLIER FEATURE)."
    Msg(5) = ""
    Msg(6) = "EXTRA-BALL: COMPLETING ROYAL FLUSH WHEN LIT FOR 'EXTRA-BALL'."
    Msg(7) = ""
    Msg(8) = "DIAMONDS: COMPLETING ALL DIAMONDS SCORES VALUE, LIGHTS 'CAPTURE',"
    Msg(9) = "COLLECTS DIAMOND BONUS (IF ANY), AND DOUBLES ENTIRE"
    Msg(10) = "SCORE IF ALL SPADE ARE 'UP'."
    Msg(11) = ""
    Msg(12) = "MULTI-BALL: COMPLETING ALL DIAMONDS LIGHTS 'CAPTURE'. PLAYFIELD"
    Msg(13) = "SCORES TIMES 'X'."
    Msg(14) = ""
    Msg(15) = "SPADES: COMPLETING ALL SPADES LIGHTS RAMP TO ADVANCE 'JACKPOT'."
    Msg(16) = ""
    Msg(17) = "JACKPOT: MAKING RAMP WHEN FLASHING ADDS LETTER, LAST LETTER COLLECTS"
    Msg(18) = ""
    Msg(19) = "'SAVE' TARGET: MAKING LOWER LEFT AND RIGHT DIAMONDS RAISE TARGET."
    Msg(20) = ""
    Dim x:For X = 1 To 20
        Msg(0) = Msg(0) + Msg(X)&Chr(13)
    Next
    MsgBox Msg(0), , "         Instructions and Rule Card"
End Sub

'******************************************************************************************
'* TABLE OPTIONS **************************************************************************
'******************************************************************************************

'**** New Options Selection through F6 menu ***

Dim RGBInserts, RGBGI, FlipperType, BallType, IsGlowBall

'REGISTRY LOCATIONS ***************************************************************************************************************************************

 Const optOpenAtStart	= &H00001
 Const optRGBInserts	= &H00002
 Const optRGBGI			= &H00004
 Const optFlip			= &H00008
 Const optBall			= &H00040

'MENU INIT *********************************************************************************************************************************************

Dim TableOptions, TableName, optReset
Private vpmShowDips1, vpmDips1

 Sub InitializeOptions
	Set vpmShowDips = GetRef("editDips")
	TableName="HarleyQuinn_VPX"									'Replace with your descriptive table name, it will be used to save settings in VPReg.stg file
	Set vpmShowDips1 = vpmShowDips								'Reassigns vpmShowDips to vpmShowDips1 to allow usage of default dips menu
	Set vpmShowDips = GetRef("TableShowDips")					'Assigns new sub to vmpShowDips
 	TableOptions = LoadValue(TableName,"Options")				'Load saved table options
	Set Controller = CreateObject("VPinMAME.Controller")		'Load vpm controller temporarily so options menu can be loaded if needed
	If TableOptions = "" Or optReset Then						'If no existing options, reset to default through optReset, then open Options menu
		TableOptions = optOpenAtStart + 0*optFlip				'clear any existing settings and set table options to default options
		TableShowOptions
	ElseIf (TableOptions And optOpenAtStart) Then				'If Enable Next Start was selected then
		TableOptions = TableOptions - optOpenAtStart			'clear setting to avoid future executions
		TableShowOptions
	Else
		TableSetOptions
	End If
	Set Controller = Nothing									'Unload vpm controller so selected controller can be loaded
 End Sub

Private Sub TableShowDips
	vpmShowDips1												'Show original Dips menu
	TableShowOptions											'Show new options menu
'	TableShowOptions2											'Add more options menus...
End Sub

 Private Sub editDips											'Dip Switches Menu, added by Inkochnito
	If not IsObject(vpmDips) Then
		Set vpmDips = New cvpmDips
		With vpmDips
			.AddForm 700, 400, "Harley Quinn - DIP switches"
			.AddFrame 2, 4, 190, "Maximum credits", 49152, Array("8 credits", 0, "10 credits", 32768, "15 credits", &H00004000, "20 credits", 49152)                                                                                  'dip 15&16
			.AddFrame 2, 80, 190, "Coin chute 1 and 2 control", &H00002000, Array("seperate", 0, "same", &H00002000)                                                                                                                  'dip 14
			.AddFrame 2, 126, 190, "Playfield special", &H00200000, Array("replay", 0, "extra ball", &H00200000)                                                                                                                      'dip 22
			.AddFrame 2, 172, 190, "High games to date control", &H00000020, Array("no effect", 0, "reset high games 2-5 on power off", &H00000020)                                                                                   'dip 6
			.AddFrame 2, 218, 190, "Auto-percentage control", &H00000080, Array("disabled (normal high score mode)", 0, "enabled", &H00000080)                                                                                        'dip 8
			.AddFrame 2, 264, 190, "Royal flush sequence is", &H40000000, Array("reset royal flush value every ball", 0, "memorize royal flush value every ball", &H40000000)                                                         'dip 31
			.AddFrame 2, 310, 190, "Game playing time control", &H80000000, Array("shorter", 0, "longer", &H80000000)                                                                                                                 'dip 32
			.AddFrame 205, 4, 190, "High game to date awards", &H00C00000, Array("not displayed and no award", 0, "displayed and no award", &H00800000, "displayed and 2 replays", &H00400000, "displayed and 3 replays", &H00C00000) 'dip 23&24
			.AddFrame 205, 80, 190, "Balls per game", &H01000000, Array("5 balls", 0, "3 balls", &H01000000)                                                                                                                          'dip 25
			.AddFrame 205, 126, 190, "Replay limit", &H04000000, Array("no limit", 0, "one per game", &H04000000)                                                                                                                     'dip 27
			.AddFrame 205, 172, 190, "Novelty", &H08000000, Array("normal", 0, "extra ball and replay scores 500K", &H08000000)                                                                                                       'dip 28
			.AddFrame 205, 218, 190, "Game mode", &H10000000, Array("replay", 0, "extra ball", &H10000000)                                                                                                                            'dip 29
			.AddFrame 205, 264, 190, "3rd coin chute credits control", &H20000000, Array("no effect", 0, "add 9", &H20000000)                                                                                                         'dip 30
			.AddChk 205, 316, 180, Array("Match feature", &H02000000)                                                                                                                                                                 'dip 26
			.AddChk 205, 331, 190, Array("Attract sound", &H00000040)                                                                                                                                                                 'dip 7
			.AddLabel 50, 370, 300, 15,"* Requires restart to apply these settings"
		End With
	End If
	vpmDips.ViewDips
End Sub

 Private Sub TableShowOptions									'Table options menu, additional menus can be added as well, just follow similar format and add call to TableShowDips
	If not IsObject(vpmDips1) Then
		Set vpmDips1 = New cvpmDips
		With vpmDips1
			.AddForm 630, 250, "TABLE OPTIONS MENU (Press " & vpmKeyName(keyShowDips) & " to open this menu)"
			.AddFrameExtra 0,0,155,"Enable RGB Inserts",optRGBInserts, Array("No", 0*optRGBInserts, "Yes", 1*optRGBInserts)
			.AddFrameExtra 175,0,155,"Enable RGB GI",optRGBGI, Array("No", 0*optRGBGI, "Yes", 1*optRGBGI)
			.AddFrameExtra 175,50,155,"Flippers Type",3*optFlip, Array("normal", 0*optFlip, "glowbat green", 1*optFlip, "glowbat blue", 2*optFlip, "glowbat pink", 3*optFlip)
			.AddFrameExtra 0,50,155,"Ball Type",7*optBall, Array("Normal", 0*optBall, "Logo", 1*optBall, "glowball green", 2*optBall, "glowball blue", 3*optBall, "glowball pink", 4*optBall)
			.AddChkExtra 0,180,155,Array("Enable Menu Next Start", optOpenAtStart)
			.Addlabel 175,145,125,41,"TIP: To Reset to defaults,open table script and uncomment line 32"
		End With
	End If
	TableOptions=vpmDips1.ViewDipsExtra(TableOptions)
	TableSetOptions
 End Sub

'MENU ENTRYS *********************************************************************************************************************************************

 Sub TableSetOptions		'defines required settings before table is run
	RGBInserts = (TableOptions And optRGBInserts)\optRGBInserts
	RGBGI = (TableOptions And optRGBGI)\optRGBGI
	FlipperType = (TableOptions And 3*optFlip)\optFlip
	BallType = (TableOptions And 7*optBall)\optBall
	SaveValue TableName,"Options",TableOptions
	GetOptions
 End Sub

Sub GetOptions
	Dim xx
	If RGBInserts=0 Then
		For each xx in aRGBLights
			If TypeName(xx)="Light" Then xx.colorfull=RGB(255,255,255)
			If TypeName(xx)="Flasher" Then xx.color=RGB(224,224,224)
		Next
	End If

	If RGBGI=1 Then
		For each xx in aGiLights:xx.color=RGB(0,0,0):xx.colorfull=RGB(255,255,255):Next
	Else
		RGBTimer.Enabled=0
		For each xx in aGiLights:xx.color=RGB(255,252,224):xx.colorfull=RGB(255,197,143):Next
	End If

	Select Case FlipperType
		Case 0:	LeftFlipper1.visible=1:LeftFlipper2.visible=1:LeftFlipper3.visible=1:LeftFlipper4.visible=1
				RightFlipper1.visible=1:RightFlipper2.visible=1:RightFlipper3.visible=1:RightFlipper4.visible=1
				glowbatleft.visible = 0 : glowbatright.visible = 0 : GlowBatLightLeft.visible = 0 : GlowBatLightRight.visible = 0 
				FlipperLSh.visible = 1 : FlipperRSh.visible = 1

		Case 1:	LeftFlipper1.visible=0:LeftFlipper2.visible=0:LeftFlipper3.visible=0:LeftFlipper4.visible=0
				RightFlipper1.visible=0:RightFlipper2.visible=0:RightFlipper3.visible=0:RightFlipper4.visible=0
				glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.visible = 1 : GlowBatLightRight.visible = 1 
				glowbatleft.image = "glowbat green" : glowbatright.image = "glowbat green" 
				GlowBatLightLeft.color = RGB(0,255,0) : GlowBatLightRight.color = RGB(0,255,0)
				GlowBatLightLeft.colorfull = RGB(0,255,0) : GlowBatLightRight.colorfull = RGB(0,255,0)
				FlipperLSh.visible = 0 : FlipperRSh.visible = 0


		Case 2:	LeftFlipper1.visible=0:LeftFlipper2.visible=0:LeftFlipper3.visible=0:LeftFlipper4.visible=0
				RightFlipper1.visible=0:RightFlipper2.visible=0:RightFlipper3.visible=0:RightFlipper4.visible=0
				glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.visible = 1 : GlowBatLightRight.visible = 1 
				glowbatleft.image = "glowbat blue" : glowbatright.image = "glowbat blue" 
				GlowBatLightLeft.color = RGB(0,0,255) : GlowBatLightRight.color = RGB(0,0,255)
				GlowBatLightLeft.colorfull = RGB(0,0,255) : GlowBatLightRight.colorfull = RGB(0,0,255)
				FlipperLSh.visible = 0 : FlipperRSh.visible = 0


		Case 3:	LeftFlipper1.visible=0:LeftFlipper2.visible=0:LeftFlipper3.visible=0:LeftFlipper4.visible=0
				RightFlipper1.visible=0:RightFlipper2.visible=0:RightFlipper3.visible=0:RightFlipper4.visible=0
				glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.visible = 1 : GlowBatLightRight.visible = 1 
				glowbatleft.image = "glowbat pink" : glowbatright.image = "glowbat pink"
				GlowBatLightLeft.color = RGB(255,0,72) : GlowBatLightRight.color = RGB(255,0,72)
				GlowBatLightLeft.colorfull = RGB(255,0,72) : GlowBatLightRight.colorfull = RGB(255,0,72)
				FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	End Select

	Select Case BallType
		Case 0	'normal Ball
			IsGlowBall=False
			Table1.BallImage="ball0"
			Table1.BallFrontDecal="JPBall-Scratches"
			Table1.BallDecalMode=False
			Table1.DefaultBulbIntensityScale = 1
		Case 1	'Logo Ball
			IsGlowBall=False
			Table1.BallImage="black"
			Table1.BallFrontDecal="ball_tex"
			Table1.BallDecalMode=True
			Table1.DefaultBulbIntensityScale = 3
		Case 2	'green GlowBall
			IsGlowBall=True
			Table1.BallImage="glowball green"
			Table1.BallFrontDecal=""
			Table1.BallDecalMode=True
			Table1.DefaultBulbIntensityScale = 0
		Case 3	'blue GlowBall
			IsGlowBall=True
			Table1.BallImage="glowball blue"
			Table1.BallFrontDecal=""
			Table1.BallDecalMode=True
			Table1.DefaultBulbIntensityScale = 0
		Case 4	'pink GlowBall
			IsGlowBall=True
			Table1.BallImage="glowball pink"
			Table1.BallFrontDecal=""
			Table1.BallDecalMode=True
			Table1.DefaultBulbIntensityScale = 0
	End Select

End Sub

Dim  RGBStep, RGBFactor, Red, Green, Blue
RGBStep = 0:RGBFactor = 5
Red = 255:Green = 0:Blue = 0

Sub RGBTimer_timer 'rainbow light color changing
    Select Case RGBStep
        Case 0 'Green
            Green = Green + RGBFactor
            If Green > 255 then
                Green = 255
                RGBStep = 1
            End If
        Case 1 'Red
            Red = Red - RGBFactor
            If Red < 0 then
                Red = 0
                RGBStep = 2
            End If
        Case 2 'Blue
            Blue = Blue + RGBFactor
            If Blue > 255 then
                Blue = 255
                RGBStep = 3
            End If
        Case 3 'Green
            Green = Green - RGBFactor
            If Green < 0 then
                Green = 0
                RGBStep = 4
            End If
        Case 4 'Red
            Red = Red + RGBFactor
            If Red > 255 then
                Red = 255
                RGBStep = 5
            End If
        Case 5 'Blue
            Blue = Blue - RGBFactor
            If Blue < 0 then
                Blue = 0
                RGBStep = 0
            End If
    End Select
	Dim xx:For each xx in aGiLights
		xx.color=RGB(Red\10, Green\10, Blue\10)
		xx.colorfull=RGB(Red, Green, Blue)
	Next
	For each xx in aDTLights
		xx.color=RGB(Red\10, Green\10, Blue\10)
		xx.colorfull=RGB(Red, Green, Blue)
	Next
End Sub
'******************
' Rom sound ON wwhen close table
'******************
Sub Table1_Exit()
    Controller.Pause = False
    Controller.Stop
    Controller.Games("diamond").Settings.Value("sound") = 1
    Controller.Games("diamond").Settings.Value("samples") = 1
End Sub