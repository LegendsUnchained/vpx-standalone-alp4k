' Gamatron / IPD No. 984 / Pinstar December, 1985 / 4 Players
' jpsalas 2021, vpinmame script based on destruk script

Option Explicit
Randomize

'----- FlexDMD Option -----
Dim UseFlexDMD:UseFlexDMD = 1   	' 0 = off, 1 = on. Replaces external dmd if enabled 
Const FlexColour = 4				' 0 = mid blue, 1 = cyan, 2 = red (default), 3 = yellow, 4 = Green, 5 = blue, 6 = white

'----- Rails -----
Dim UseRails:UseRails = 0

'----- Music -----
'If you want BG Music, download BadBatch.mp3 from the VPX file download site and place it in a folder named Music in 
'the same folder as the table.
Dim playBGM: playBGM = 0

Const BallSize = 50
Const BallMass = 1.7

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim bsTrough, bsRTK, bsLTK, dt3, dt5, rolloverplayed, x
rolloverplayed=False

'Select the ROM
Const cGameName = "gamatron" 'Pinstar original ROM, same rules as Flight2000 standard, do not include voices
'Const cGameName = "flight2k" 'Flight2000 original rom
'Const cGameName = "flight2m" 'Updated with extra voices and rules

Dim VarHidden, UseVPMColoredDMD
If Table1.ShowDT = true then
    UseVPMColoredDMD = true
    VarHidden = 1
    For each x in aReels
        x.Visible = 1
    Next
    Else
    UseVPMColoredDMD = False
    VarHidden = 0
    For each x in aReels
        x.Visible = 0
    Next
End If

if B2SOn = true then VarHidden = 1

LoadVPM "01210000", "STERN.VBS", 3.1

'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 0
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

	' initalise the FlexDMD display
    If UseFlexDMD Then FlexDMD_Init

    vpmInit me

	If UseFlexDMD Then UpdateLeds  'get round round strange timing issue if we wait for timer event to call ChangedLEDs
	
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.Games(cGameName).Settings.Value("sound") = 0
        .SplashInfoLine = "Gamatron - Pinstar 1985" & vbNewLine & "VPX table by JPSalas 1.0.0"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = VarHidden
        .Games(cGameName).Settings.Value("rol") = 0
		If UseFlexDMD Then ExternalEnabled = .Games(cGameName).Settings.Value("showpindmd")
		If UseFlexDMD Then .Games(cGameName).Settings.Value("showpindmd") = 0
        '.SetDisplayPosition 0,0,GetPlayerHWnd 'uncomment if you can't see the dmd
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
        If playBGM = 1
		PlayMusic "BadBatchTheme.mp3"
        End If
		vpmTimer.addTimer 270000, "ReplayMusic '"
    End With

' Rails

	If UseRails = 0 Then 
		rrail.visible = False
		lrail.visible = False
		Flasher7.visible = False
		Flasher6.visible = False
		Ramp1.visible = False
		Ramp2.visible = False
		
    End If

    ' Nudging
    vpmNudge.TiltSwitch = 7
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(Bumper1, LeftSlingshot, RightSlingshot, Rightslingshot2)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 33, 34, 35, 0, 0, 0, 0
        .InitKick BallRelease, 90, 4
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 3
        .IsTrough = 1
    End With

    ' Saucer
    Set bsLTK = New cvpmBallStack
    With bsLTK
        .InitSaucer kicker1, 15, 0, 28
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    End With

    Set bsRTK = New cvpmBallStack
    With bsRTK
        .InitSaucer kicker2, 16, 0, 28
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .kickforcevar = 4
    End With

    ' Drop targets
    Set dt3 = New cvpmDropTarget
    With dt3
        .InitDrop Array(T6, T7, T8), Array(25, 26, 27)
        .InitSnd SoundFX("fx_droptarget", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
        .CreateEvents "dt3"
    End With

    Set dt5 = New cvpmDropTarget
    With dt5
        .InitDrop Array(T1, T2, T3, T4, T5), Array(28, 29, 30, 31, 32)
        .InitSnd SoundFX("fx_droptarget", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
        .CreateEvents "dt5"
    End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
    RealTime.Enabled = 1

    ' Turn on Gi
    vpmtimer.addtimer 1500, "GiOn '"

    LoadLUT

End Sub

'******************
' RealTime Updates
'******************

Sub RealTime_Timer
    RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
End Sub

'***********
' GI lights
'***********

Sub GiOn 'enciende las luces GI
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff 'apaga las luces GI
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 8:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = LeftMagnaSave Then bLutActive = True
    If keycode = RightMagnaSave Then
        If bLutActive Then NextLUT:End If
    End If
    If vpmKeyDown(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundat "fx_PlungerPull", Plunger:Plunger.Pullback
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = LeftMagnaSave Then bLutActive = False
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_plunger", Plunger:Plunger.Fire
End Sub

'*********
'   LUT
'*********

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

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 10:UpdateLUT:SaveLUT:End Sub

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
    End Select
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep, RStep2

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk
	PlaySound "efx_chirp1"
    'DOF 101, DOFPulse
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 11
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
	PlaySound "efx_chirp2"
    'DOF 102, DOFPulse
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 12
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

Sub RightSlingShot2_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk
	PlaySound "efx_chirp2"
    'DOF 102, DOFPulse
    Rubber029.Visible = 1
    Remk2.RotX = 26
    RStep2 = 0
    vpmTimer.PulseSw 10
    RightSlingShot2.TimerEnabled = 1
End Sub

Sub RightSlingShot2_Timer
    Select Case RStep2
        Case 1:Rubber029.Visible = 0:Rubber028.Visible = 1:Remk2.RotX = 14
        Case 2:Rubber028.Visible = 0:Rubber027.Visible = 1:Remk2.RotX = 2
        Case 3:Rubber027.Visible = 0:Remk2.RotX = -20:RightSlingShot2.TimerEnabled = 0
    End Select
    RStep2 = RStep2 + 1
End Sub

' Scoring rubbers
Sub rlband003_Hit:vpmTimer.PulseSw 9:End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 13:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper1:PlaySound "efx_laser2":End Sub

' Targets
Sub T1_Hit:PlaySound "efx_hunter-name":End Sub
Sub T2_Hit:PlaySound "efx_wrecker-name":End Sub
Sub T3_Hit:PlaySound "efx_tech-name":End Sub
Sub T4_Hit:PlaySound "efx_echo-name":End Sub
Sub T5_Hit:PlaySound "efx_crosshair-name":End Sub

' Drain & holes
Sub Drain_Hit:PlaysoundAt "fx_drain", Drain:bsTrough.AddBall Me:PlaySound "efx_chat4":End Sub
Sub Kicker1_Hit:bsLTK.addball 0:PlaysoundAt "fx_kicker_enter", kicker1:End Sub
Sub Kicker2_Hit:bsRTK.addball 0:PlaysoundAt "fx_kicker_enter", kicker2:End Sub
Sub kicker5_Hit:PlaysoundAt "fx_kicker_enter", kicker5:Controller.Switch(39) = 1:End Sub

' Rollovers
Sub sw36_Hit:Controller.Switch(36) = 1:PlaySoundAt "fx_sensor", sw36:rollover_bad_callouts:End Sub
Sub sw36_UnHit:Controller.Switch(36) = 0:End Sub

Sub sw24_Hit:Controller.Switch(24) = 1:PlaySoundAt "fx_sensor", sw24:rollover_good_callouts:End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub

Sub sw23_Hit:Controller.Switch(23) = 1:PlaySoundAt "fx_sensor", sw23:rollover_good_callouts:End Sub
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub

Sub sw34_Hit:Controller.Switch(34) = 1:PlaySoundAt "fx_sensor", sw34:rollover_bad_callouts:End Sub
Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub

Sub sw40_Hit:Controller.Switch(40) = 1:PlaySoundAt "fx_sensor", sw40:End Sub
Sub sw40_UnHit:Controller.Switch(40) = 0:End Sub

Sub sw18_Hit:Controller.Switch(18) = 1:PlaySoundAt "fx_sensor", sw18:PlaySound "efx_laser1":End Sub
Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub

Sub sw19_Hit:Controller.Switch(19) = 1:PlaySoundAt "fx_sensor", sw19:PlaySound "efx_laser1":End Sub
Sub sw19_UnHit:Controller.Switch(19) = 0:End Sub

Sub sw20_Hit:Controller.Switch(20) = 1:PlaySoundAt "fx_sensor", sw20:PlaySound "efx_laser1":End Sub
Sub sw20_UnHit:Controller.Switch(20) = 0:End Sub

Sub sw101_Hit:Controller.Switch(101) = 1:rollover_r2_callouts:End Sub
Sub sw101_UnHit:Controller.Switch(101) = 0:PlaySound "efx_xwing":End Sub

'Spinners

Sub spinner1_Spin():vpmTimer.PulseSw 4:PlaySoundAt "fx_spinner", spinner1:End Sub
Sub spinner2_Spin():vpmTimer.PulseSw 5:PlaySoundAt "fx_spinner", spinner2:End Sub

'Targets
Sub sw17_Hit:vpmTimer.PulseSw 17:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw22_Hit:vpmTimer.PulseSw 22:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw21_Hit:vpmTimer.PulseSw 21:PlaySoundAtBall SoundFX("fx_target", DOFTargets):PlaySound "efx_sabre":End Sub
Sub sw38_Hit:vpmTimer.PulseSw 38:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub

'*********'
'Sounds
'*********
Sub ReplayMusic
	PlayMusic "BadBatchTheme.mp3"
	vpmTimer.addTimer 270000, "ReplayMusic '"
End Sub

Sub rollover_bad_callouts()
		Select Case Int(Rnd*7)+1
			Case 1 : PlaySound "efx_who-threw-that"
			Case 2 : PlaySound "efx_not-comforting"
			Case 3 : PlaySound "efx_unfortunate"
			Case 4 : PlaySound "efx_missing-action"
			Case 5 : PlaySound "efx_no-use"
			Case 6 : PlaySound "efx_crash-landing"
			Case 6 : PlaySound "efx_not-ideal"
		End Select
End Sub

Sub rollover_good_callouts()
	If rolloverplayed = False Then
		rolloverplayed = True
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySound "efx_under-control"
			Case 2 : PlaySound "efx_no-systems-compromised"
			Case 3 : PlaySound "efx_not-affecting-life-support"
			Case 4 : PlaySound "efx_were-gonna-die"
		End Select
	vpmTimer.addTimer 3500, "rolloverplayed=false '"
	End If
End Sub

Sub rollover_r2_callouts()
	If rolloverplayed = False Then
		rolloverplayed = True
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySound "efx_r2-1"
			Case 2 : PlaySound "efx_r2-2"
			Case 3 : PlaySound "efx_r2-3"
		End Select
	vpmTimer.addTimer 1000, "rolloverplayed=false '"
	End If
End Sub

Sub CheckBonusLi
	If LampState(63) = 0 Then
		For x = 0 to 200	
			If x=1 OR x=17 or x=33 or x=49 or x=2 or x=18 or x=34 or x=50 or x=3 or x=19 Then
				If OldLampState(x) <> LampState(x) AND LampState(x)=1 Then
					PlaySound "efx_chirp2"
				End If
			End If
			OldLampState(x) = LampState(x)
		Next
	End If

End Sub




'*********
'Solenoids
'*********
SolCallback(6) = "vpmSolSound SoundFX(""fx_Knocker"",DOFKnocker),"
'SolCallback(19) = "vpmNudge.SolGameOn"
SolCallback(10) = "bsTrough.SolOut"
SolCallback(17) = "bsRTK.SolOut"
SolCallback(20) = "bsLTK.SolOut"
SolCallback(7) = "dt5.SolDropUp"
SolCallback(13) = "dt5.SolHit 1,"
SolCallback(14) = "dt5.SolHit 2,"
SolCallback(12) = "dt5.SolHit 3,"
SolCallback(11) = "dt5.SolHit 4,"
SolCallback(8) = "dt5.SolHit 5,"
SolCallback(15) = "dt3.SolDropUp"
SolCallback(9) = "solballaunch"

Sub solballaunch(enabled)
    If Enabled Then
        Controller.Switch(39) = 0
        kicker3.kick 180, 4:kicker4.kick 180, 4:kicker5.kick 180, 4
		kicker_callout()
    End If
End Sub

Sub kicker_callout()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "efx_tapped-in"
		Case 2 : PlaySound "efx_breaking-out"
		Case 3 : PlaySound "efx_hold-position"
	End Select
End Sub

'*******************
' Flipper Subs v3.0
'*******************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.75:LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.2:LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.75:RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.2:RightFlipper.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'**********************************************************
'     JP's Flasher Fading for VPX and Vpinmame v3.0
'       (Based on Pacdude's Fading Light System)
' This is a fast fading for the Flashers in vpinmame tables
'  just 4 steps, like in Pacdude's original script.
' Included the new Modulated flashers & Lights for WPC
'**********************************************************

Dim LampState(200), OldLampState(200), FadingState(200), FlashLevel(200)

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
	CheckBonusLi
End Sub

Sub UpdateLamps()
    If VarHidden Then
        NFReelm 11, li11a
        NFReel 13, li13 'highscore
        NFReel 45, li45
        NFReel 61, li61
        NFReel 63, li63
    End If
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
    Lamp 14, li14
    Lamp 15, li15
    'Lamp 16, li16
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
    Lamp 33, li33
    Lamp 34, li34
    Lamp 35, li35 'turns on at multiball
    Lamp 36, li36
    Lamp 37, li37
    Lamp 38, li38
    Lamp 39, li39
    Lamp 40, li40
    Lamp 41, li41
    Lamp 42, li42
    Lamp 43, li43
    Lamp 44, li44
    Lamp 46, li46
    'Lamp 47, li47
    'Lamp 48, li48
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
    Lamp 62, li62

'Lamp 64, li64
End Sub

' div lamp subs

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
    LampTimer.Interval = 25 ' flasher fading speed
    LampTimer.Enabled = 1
    For x = 0 to 200
        LampState(x) = 0
		OldLampState(x) = 0
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

Sub NFReel(nr, object)
    Select Case FadingState(nr)
        Case 4:Object.Visible = 1:FadingState(nr) = 0
        Case 3:Object.Visible = 0:FadingState(nr) = 0
    End Select
End Sub

Sub NFReelm(nr, object)
    Select Case FadingState(nr)
        Case 4:Object.Visible = 1
        Case 3:Object.Visible = 0
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

'Assign 7-digit output to reels
Set Digits(0) = a0
Set Digits(1) = a1
Set Digits(2) = a2
Set Digits(3) = a3
Set Digits(4) = a4
Set Digits(5) = a5
Set Digits(6) = a6

Set Digits(7) = b0
Set Digits(8) = b1
Set Digits(9) = b2
Set Digits(10) = b3
Set Digits(11) = b4
Set Digits(12) = b5
Set Digits(13) = b6

Set Digits(14) = c0
Set Digits(15) = c1
Set Digits(16) = c2
Set Digits(17) = c3
Set Digits(18) = c4
Set Digits(19) = c5
Set Digits(20) = c6

Set Digits(21) = d0
Set Digits(22) = d1
Set Digits(23) = d2
Set Digits(24) = d3
Set Digits(25) = d4
Set Digits(26) = d5
Set Digits(27) = d6

Set Digits(28) = e0
Set Digits(29) = e1
Set Digits(30) = e2
Set Digits(31) = e3

Sub UpdateLeds
    On Error Resume Next
	If UseFlexDMD then FlexDMD.LockRenderThread
    Dim ChgLED, ii, jj, chg, stat, num
    ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(ChgLED)
            num=chgLED(ii, 0) : chg = chgLED(ii, 1):stat = chgLED(ii, 2)
			If UseFlexDMD then UpdateFlexChar num, stat
            For jj = 0 to 10
                If stat = Patterns(jj)OR stat = Patterns2(jj)then Digits(chgLED(ii, 0)).SetValue jj
            Next
        Next
    End IF
	If UseFlexDMD then
		With FlexDMDScene
			.GetImage("Match").Visible = Controller.Lamp(63)
			.GetImage("Ball").Visible = Not Controller.Lamp(63)
			If LampState(61) = 1 Then 'li61.Visible = True Then '
				.GetImage("GameText").Bitmap = FlexDMD.NewImage("", "VPX.DMD_Tilt").Bitmap
				ActivePlayerScore = 7
			End If
		End With
		FlexDMD.UnlockRenderThread
	End If
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
Sub aTargets_Hit(idx):ActiveBall.VelZ = BallVel(Activeball) * (RND / 2):End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v3.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 1000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
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
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.4, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls, 20 balls, from 0 to 19
Const lob = 0     'number of locked balls
Const maxvel = 34 'max ball velocity 25-50
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

    ' stop the sound of deleted balls and hide the shadow
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
        aBallShadow(b).Height = BOT(b).Z -24

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
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
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
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

'***********************
' Ball Collision Sound
'***********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'***********************
'    Stern Gamatron
'    by Inkochnito
'***********************
Sub editDips
    Dim vpmDips:Set vpmDips = New cvpmDips
    With vpmDips
        .AddForm 700, 400, "Gamatron - DIP switches"
        .AddChk 2, 10, 180, Array("Match feature", &H00100000)                                                                                                    'dip 21
        .AddChk 2, 25, 115, Array("Credits display", &H00080000)                                                                                                  'dip 20
        .AddFrame 2, 45, 190, "Maximum credits", &H00060000, Array("10 credits", 0, "15 credits", &H00020000, "25 credits", &H00040000, "40 credits", &H00060000) 'dip 18&19
        .AddFrame 2, 120, 190, "High game to date", 49152, Array("points", 0, "1 free game", &H00004000, "2 free games", 32768, "3 free games", 49152)            'dip 15&16
        .AddFrame 2, 195, 190, "High score feature", &H00000020, Array("extra ball", 0, "replay", &H00000020)                                                     'dip 6
        .AddFrame 2, 241, 190, "Spot 1 or 2 'F' lites", &H00001000, Array("spot one 'F'", 0, "spot two 'FF'", &H00001000)                                         'dip 13
        .AddFrame 2, 287, 190, "Right spinner lites", &H00200000, Array("start fresh", 0, "retain", &H00200000)                                                   'dip 22
        .AddChk 205, 10, 190, Array("Talking sound", &H00010000)                                                                                                  'dip 17
        .AddChk 205, 25, 190, Array("Background sound", &H00002000)                                                                                               'dip 14
        .AddFrame 205, 45, 190, "Special award", &HC0000000, Array("no award", 0, "100,000 points", &H40000000, "free ball", &H80000000, "free game", &HC0000000) 'dip 31&32
        .AddFrame 205, 120, 190, "Add a ball memory", &H00000090, Array("one ball", 0, "three balls", &H00000010, "five balls", &H00000090)                       'dip 5&8
        .AddFrame 205, 195, 190, "Balls per game", &H00000040, Array("3 balls", 0, "5 balls", &H00000040)                                                         'dip 7
        .AddFrame 205, 241, 190, "Special limit", &H10000000, Array("no limit", 0, "one replay per ball", &H10000000)                                             'dip 29
        .AddFrame 205, 287, 190, "Apollo 1 and 2 lites", &H00800000, Array("are left off at start of the game", 0, "are turned on at start game", &H00800000)     'dip24
        .AddFrame 100, 333, 190, "Bonus multiplier", &H20000000, Array("kept in memory", 0, "reset", &H20000000)                                                  'dip30
        .AddLabel 50, 385, 300, 20, "After hitting OK, press F3 to reset game with new settings."
        .ViewDips
    End With
End Sub
Set vpmShowDips = GetRef("editDips")

Sub Table1_Exit()
	Controller.Pause = False
	Controller.Stop
	SaveLUT
	If UseFlexDMD then
		If IsObject(FlexDMD) Then 
			If Not FlexDMD is Nothing Then 
				FlexDMD.Show = False
				FlexDMD.Run = False
				FlexDMD = NULL
			End if
		End if
		Controller.Games(cGameName).Settings.Value("showpindmd") = ExternalEnabled
	End if
End Sub

'*****************************
' Flex DMD Display - scutters
'*****************************
'**********************************************************
'  2*14 score + game indicators numeric segment to flexdmd display conversion
'**********************************************************
Dim FlexDMD
DIm FlexDMDDict, FlexDMDDictSmall
Dim FlexDMDScene
Dim ExternalEnabled
Dim ActivePlayerScore, LastActivePlayerScore

Sub FlexDMD_Init() 'default/startup values

	' flex dmd variables
	DIm FlexDMDFont
	Dim FlexPath

	' populate the lookup dictionary for mapping display characters
	FlexDictionary_Init
	FlexDictionary_Small_Init

	ActivePlayerScore = 0 '(player 1, player 4  = 3 etc)
	LastActivePlayerScore = 0

	On Error Resume Next

	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
	If Not FlexDMD is Nothing Then
	
		
		FlexDMD.GameName = cGameName
		FlexDMD.TableFile = Table1.Filename & ".vpx"
		FlexDMD.RenderMode = 2
		FlexDMD.Width = 128
		FlexDMD.Height = 32
		FlexDMD.Clear = True
		FlexDMD.Run = True

		FlexDMD.LockRenderThread

		Set FlexDMDScene = FlexDMD.NewGroup("Scene")
		

		With FlexDMDScene
		
			'add back colour
			.AddActor FlexDMD.NewFrame("BackC")
			.GetFrame("BackC").Visible = True
			Select Case FlexColour
			Case 0
				.GetFrame("BackC").FillColor = RGB(255,128,0)
				.GetFrame("BackC").BorderColor = RGB(255,128,0)
			Case 3
				.GetFrame("BackC").FillColor = vbCyan
				.GetFrame("BackC").BorderColor = vbCyan
			Case 4
				.GetFrame("BackC").FillColor = vbGreen
				.GetFrame("BackC").BorderColor = vbGreen
			Case 5
				.GetFrame("BackC").FillColor = vbRed
				.GetFrame("BackC").BorderColor = vbRed
			Case 6
				.GetFrame("BackC").FillColor = vbWhite
				.GetFrame("BackC").BorderColor = vbWhite
			Case 1
				.GetFrame("BackC").FillColor = vbYellow
				.GetFrame("BackC").BorderColor = vbYellow
			Case Else
				.GetFrame("BackC").FillColor = vbBlue
				.GetFrame("BackC").BorderColor = vbBlue
			End Select
			.GetFrame("BackC").Height = 32
			.GetFrame("BackC").Width= 128
			.GetFrame("BackC").Fill= True
			.GetFrame("BackC").Thickness= 1

			'add frame / overlay
			.AddActor FlexDMD.NewImage("Frame","VPX.DMD_Frame")

			'28 score segment display holders
			dim i,j, pos
			pos = 1

			'add small score lines, players 1-4
			For j = 0 to 3
				For i = 0 to 6
					'offsets for commas etc in score digits
					Select Case i
						Case 0
							pos = 8'9
							.AddActor FlexDMD.NewImage("Seg" & cstr(i + (j * 7)), "VPX.DMD_S_Blankc")
							.GetImage("Seg" & cstr(i + (j * 7))).SetAlignedPosition pos, 1 + (j * 6), 0
						Case 1
							pos = 15'17
							.AddActor FlexDMD.NewImage("Seg" & cstr(i + (j * 7)), "VPX.DMD_S_Blank")
							.GetImage("Seg" & cstr(i + (j * 7))).SetAlignedPosition pos, 1 + (j * 6), 0
						Case 2
							pos = 20'23
							.AddActor FlexDMD.NewImage("Seg" & cstr(i + (j * 7)), "VPX.DMD_S_Blank")
							.GetImage("Seg" & cstr(i + (j * 7))).SetAlignedPosition pos, 1 + (j * 6), 0
						Case 3
							pos = 25'29
							.AddActor FlexDMD.NewImage("Seg" & cstr(i + (j * 7)), "VPX.DMD_S_Blankc")
							.GetImage("Seg" & cstr(i + (j * 7))).SetAlignedPosition pos, 1 + (j * 6), 0
						Case 4
							pos = 32'37
							.AddActor FlexDMD.NewImage("Seg" & cstr(i + (j * 7)), "VPX.DMD_S_Blank")
							.GetImage("Seg" & cstr(i + (j * 7))).SetAlignedPosition pos, 1 + (j * 6), 0
						Case 5
							pos = 37'43
							.AddActor FlexDMD.NewImage("Seg" & cstr(i + (j * 7)), "VPX.DMD_S_Blank")
							.GetImage("Seg" & cstr(i + (j * 7))).SetAlignedPosition pos, 1 + (j * 6), 0
						Case 6
							pos = 42'49
							.AddActor FlexDMD.NewImage("Seg" & cstr(i + (j * 7)), "VPX.DMD_S_Blank")
							.GetImage("Seg" & cstr(i + (j * 7))).SetAlignedPosition pos, 1 + (j * 6), 0
					End Select
				Next 'i
			Next 'j

			'add large score display
			For i = 0 to 6
				'offsets for commas etc in score digits
				Select Case i
					Case 0
						pos = 57'61
						.AddActor FlexDMD.NewImage("SegL" & cstr(i), "VPX.DMD_Blankc")
						.GetImage("SegL" & cstr(i)).SetAlignedPosition pos, 3, 0
					Case 1
						pos = 68'72
						.AddActor FlexDMD.NewImage("SegL" & cstr(i), "VPX.DMD_Blank")
						.GetImage("SegL" & cstr(i)).SetAlignedPosition pos, 3, 0
					Case 2
						pos = 76'80
						.AddActor FlexDMD.NewImage("SegL" & cstr(i), "VPX.DMD_Blank")
						.GetImage("SegL" & cstr(i)).SetAlignedPosition pos, 3, 0
					Case 3
						pos = 84'88
						.AddActor FlexDMD.NewImage("SegL" & cstr(i), "VPX.DMD_Blankc")
						.GetImage("SegL" & cstr(i)).SetAlignedPosition pos, 3, 0
					Case 4
						pos = 95'99
						.AddActor FlexDMD.NewImage("SegL" & cstr(i), "VPX.DMD_Blank")
						.GetImage("SegL" & cstr(i)).SetAlignedPosition pos, 3, 0
					Case 5
						pos = 103'107
						.AddActor FlexDMD.NewImage("SegL" & cstr(i), "VPX.DMD_Blank")
						.GetImage("SegL" & cstr(i)).SetAlignedPosition pos, 3, 0
					Case 6
						pos = 111'115
						.AddActor FlexDMD.NewImage("SegL" & cstr(i), "VPX.DMD_Blank")
						.GetImage("SegL" & cstr(i)).SetAlignedPosition pos, 3, 0
				End Select

			Next 'i

			'add text holder
			.AddActor FlexDMD.NewImage("GameText", "VPX.DMD_GameOver")
			.GetImage("GameText").SetAlignedPosition 49,13,0
			
			'ball / match indicators
			.AddActor FlexDMD.NewImage("Match", "VPX.DMD_Match")
			.GetImage("Match").SetAlignedPosition 88,25,0
			.AddActor FlexDMD.NewImage("Ball", "VPX.DMD_Ball")
			.GetImage("Ball").SetAlignedPosition 88,25,0
			.GetImage("Ball").Visible = False
			
			'add credits
			.AddActor FlexDMD.NewImage("Credit1", "VPX.DMD_S_Blank")
			.GetImage("Credit1").SetAlignedPosition 38, 26,0
			.AddActor FlexDMD.NewImage("Credit2", "VPX.DMD_S_Blank")
			.GetImage("Credit2").SetAlignedPosition 43, 26,0
			'add ball/match
			.AddActor FlexDMD.NewImage("Ball1", "VPX.DMD_S_Blank")
			.GetImage("Ball1").SetAlignedPosition 118, 26,0
			.AddActor FlexDMD.NewImage("Ball2", "VPX.DMD_S_Blank")
			.GetImage("Ball2").SetAlignedPosition 123, 26,0

			'add logo
			.AddActor FlexDMD.NewImage("Logo", "VPX.DMD_Logo")
			.GetImage("Logo").Visible = True

		End With
	
		FlexDMD.Stage.AddActor FlexDMDScene
		
		FlexDMD.Show = True
		FlexDMD.UnlockRenderThread

	Else
		
		UseFlexDMD = 0
	
	End If

	On Error Goto 0

End Sub


Sub FlexDictionary_Init

	Set FlexDMDDict = CreateObject("Scripting.Dictionary")

	FlexDMDDict.Add 0, "VPX.DMD_Blank"

	FlexDMDDict.Add 63, "VPX.DMD_0"
	FlexDMDDict.Add 6, "VPX.DMD_1"
	FlexDMDDict.Add 91, "VPX.DMD_2"
	FlexDMDDict.Add 79, "VPX.DMD_3"
	FlexDMDDict.Add 102, "VPX.DMD_4"
	FlexDMDDict.Add 109, "VPX.DMD_5"
	FlexDMDDict.Add 125, "VPX.DMD_6"
	FlexDMDDict.Add 7, "VPX.DMD_7"
	FlexDMDDict.Add 127,"VPX.DMD_8"
	FlexDMDDict.Add 111,"VPX.DMD_9"

	FlexDMDDict.Add 191, "VPX.DMD_0c"
	FlexDMDDict.Add 134, "VPX.DMD_1c"
	FlexDMDDict.Add 219, "VPX.DMD_2c"
	FlexDMDDict.Add 207, "VPX.DMD_3c"
	FlexDMDDict.Add 230, "VPX.DMD_4c"
	FlexDMDDict.Add 237, "VPX.DMD_5c"
	FlexDMDDict.Add 253, "VPX.DMD_6c"
	FlexDMDDict.Add 135, "VPX.DMD_7c"
	FlexDMDDict.Add 255, "VPX.DMD_8c"
	FlexDMDDict.Add 239, "VPX.DMD_9c"

	
End sub

Sub FlexDictionary_Small_Init

	Set FlexDMDDictSmall = CreateObject("Scripting.Dictionary")
	
	FlexDMDDictSmall.Add 0, "VPX.DMD_S_Blank"

	FlexDMDDictSmall.Add 63, "VPX.DMD_S_0"
	FlexDMDDictSmall.Add 6, "VPX.DMD_S_1"
	FlexDMDDictSmall.Add 91, "VPX.DMD_S_2"
	FlexDMDDictSmall.Add 79, "VPX.DMD_S_3"
	FlexDMDDictSmall.Add 102, "VPX.DMD_S_4"
	FlexDMDDictSmall.Add 109, "VPX.DMD_S_5"
	FlexDMDDictSmall.Add 125, "VPX.DMD_S_6"
	FlexDMDDictSmall.Add 7, "VPX.DMD_S_7"
	FlexDMDDictSmall.Add 127,"VPX.DMD_S_8"
	FlexDMDDictSmall.Add 111,"VPX.DMD_S_9"

	FlexDMDDictSmall.Add 191, "VPX.DMD_S_0c"
	FlexDMDDictSmall.Add 134, "VPX.DMD_S_1c"
	FlexDMDDictSmall.Add 219, "VPX.DMD_S_2c"
	FlexDMDDictSmall.Add 207, "VPX.DMD_S_3c"
	FlexDMDDictSmall.Add 230, "VPX.DMD_S_4c"
	FlexDMDDictSmall.Add 237, "VPX.DMD_S_5c"
	FlexDMDDictSmall.Add 253, "VPX.DMD_S_6c"
	FlexDMDDictSmall.Add 135, "VPX.DMD_S_7c"
	FlexDMDDictSmall.Add 255, "VPX.DMD_S_8c"
	FlexDMDDictSmall.Add 239, "VPX.DMD_S_9c"

	
End sub


Sub UpdateFlexChar(id, value)

	With FlexDMDScene

		.GetImage("Logo").Visible = False
		
		if FlexDMDDictSmall.Exists (value) then

			If id < 28 Then
				If Controller.Lamp(45) = False And Controller.Lamp(13) = False Then
					If ActivePlayerScore <> id \ 7 Then
						'clear the large score, active player has changed (not game over) 
						.GetImage("SegL" & ("0")).Bitmap = FlexDMD.NewImage("", "VPX.DMD_Blank" & "c").Bitmap
						.GetImage("SegL" & ("1")).Bitmap = FlexDMD.NewImage("", "VPX.DMD_Blank").Bitmap
						.GetImage("SegL" & ("2")).Bitmap = FlexDMD.NewImage("", "VPX.DMD_Blank").Bitmap
						.GetImage("SegL" & ("3")).Bitmap = FlexDMD.NewImage("", "VPX.DMD_Blank" & "c").Bitmap
						.GetImage("SegL" & ("4")).Bitmap = FlexDMD.NewImage("", "VPX.DMD_Blank").Bitmap
						.GetImage("SegL" & ("5")).Bitmap = FlexDMD.NewImage("", "VPX.DMD_Blank").Bitmap
						.GetImage("SegL" & ("6")).Bitmap = FlexDMD.NewImage("", "VPX.DMD_Blank").Bitmap
						If value <> 0 Then 
							ActivePlayerScore = id \ 7 
							LastActivePlayerScore = ActivePlayerScore
						Else
							ActivePlayerScore = LastActivePlayerScore
						End If
						.GetImage("GameText").Bitmap = FlexDMD.NewImage("", "VPX.DMD_Player" & CStr(ActivePlayerScore + 1)).Bitmap		
					End If
				ElseIf Controller.Lamp(13) = True Then
					.GetImage("GameText").Bitmap = FlexDMD.NewImage("", "VPX.DMD_HighScore").Bitmap
					ActivePlayerScore = 6
				ElseIf Controller.Lamp(45) = True Then
					.GetImage("GameText").Bitmap = FlexDMD.NewImage("", "VPX.DMD_GameOver").Bitmap
					ActivePlayerScore = 5
				End if

				'scores
				select case id 
				case 0, 3, 7, 10, 14, 17, 21, 24 
					If value = 0 Then
						'for positions 0 & 3 os scores if blank / space  we need to add comma space (the only postions commas shown by rom on LED segments)
						.GetImage("Seg" & id).Bitmap = FlexDMD.NewImage("", (FlexDMDDictSmall.Item (value)) & "c").Bitmap
						'update large score (active player scroe flashes, others are static so only one score updating at a time [in game])
						'we know dictionaries match so no need to check lookup value exists again
						.GetImage("SegL" & (id Mod 7)).Bitmap = FlexDMD.NewImage("", (FlexDMDDict.Item (value)) & "c").Bitmap
					Else
						.GetImage("Seg" & id).Bitmap = FlexDMD.NewImage("", FlexDMDDictSmall.Item (value)).Bitmap
						.GetImage("SegL" & (id Mod 7)).Bitmap = FlexDMD.NewImage("", FlexDMDDict.Item (value)).Bitmap
					End If
				Case Else
					.GetImage("Seg" & id).Bitmap = FlexDMD.NewImage("", FlexDMDDictSmall.Item (value)).Bitmap
					.GetImage("SegL" & (id Mod 7)).Bitmap = FlexDMD.NewImage("", FlexDMDDict.Item (value)).Bitmap
				End Select
				
			ElseIf id = 28 Then
				.GetImage("Credit1").Bitmap = FlexDMD.NewImage("", FlexDMDDictSmall.Item (value)).Bitmap
			ElseIf id = 29 Then 
				.GetImage("Credit2").Bitmap = FlexDMD.NewImage("", FlexDMDDictSmall.Item (value)).Bitmap
			ElseIf id = 30 Then
				.GetImage("Ball1").Bitmap = FlexDMD.NewImage("", FlexDMDDictSmall.Item (value)).Bitmap
			ElseIf id = 31 Then
				.GetImage("Ball2").Bitmap = FlexDMD.NewImage("", FlexDMDDictSmall.Item (value)).Bitmap
			End If
		End If
	End With

End Sub