'Iron Maiden by Stern 1981
'VPX table by JPSalas version 1.0.2
'Lights, solenoids & switch numbers and vpinmame parts of the script based on Destruk's script.
'Dip Switches by Inkochnito
'Iron Maiden / IPD No. 1270 / January, 1982 / 4 Players

Option Explicit
Randomize

' Flupper LUT code...
Dim luts, lutpos
' ****** LUT overall contrast & brightness setting **************************************************************************************
luts = array("Fleep Natural Dark 1", "Fleep Natural Dark 2", "Fleep Warm Dark", "Fleep Warm Bright", "Fleep Warm Vivid Soft", "Fleep Warm Vivid Hard", "Skitso Natural and Balanced", "Skitso Natural High Contrast", "3rdaxis Referenced THX Standard", "CalleV Punchy Brightness&Contrast", "HauntFreaks Desaturated", "Tomate Washed Out", "VPW Original 1 to 1", "Bassgeige", "Blacklight", "B&W Comic Book", "Evil Dead 2 Original")

Const EnableMagnasave = 1		' 1 - on; 0 - off; if on then the magnasave button let's you rotate all LUT's
'*********************************************************************************************************************************
' end Flupper LUT code...

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01210000", "STERN.VBS", 3.1

Dim bsTrough, dtL, dtT, dtC, bsSaucer, bsSaucer2, x

Const cGameName = "ironmaid"

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 0

Dim VarHidden
If Table1.ShowDT = true then
    VarHidden = 1
    For each x in aReels
        x.Visible = 1
    Next
    lrail.Visible = 1
    rrail.Visible = 1
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
        .SplashInfoLine = "Evil Dead 2 (TBA 2019)" & vbNewLine & "VPX table by IVANTBA"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 0
        .Games(cGameName).Settings.Value("sound") = 0
        '.SetDisplayPosition 0,0,GetPlayerHWnd 'uncomment if you can't see the dmd
        On Error Resume Next
        'Controller.SolMask(0) = 0
        'vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With

PlaySound "Intro",-0

	Set bsTrough=New cvpmBallstack
	bsTrough.InitSw 5,33,34,35,0,0,0,0
	bsTrough.InitKick BallRelease,90,6
	bsTrough.InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
	bsTrough.Balls=3
	
	Set dtL=New cvpmDropTarget
	dtL.InitDrop Array(sw22,sw23,sw24),Array(22,23,24)
	dtL.InitSnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
	dtL.CreateEvents "dtL"
	
	Set dtT=New cvpmDropTarget
	dtT.InitDrop Array(sw21,sw20,sw19,sw18,sw17),Array(21,20,19,18,17)
	dtT.InitSnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
	dtT.CreateEvents "dtT"
	
	Set dtC=New cvpmDropTarget
	dtC.InitDrop Array(sw29,sw28,sw27,sw26,sw25),Array(29,28,27,26,25)
	dtC.InitSnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
	dtC.CreateEvents "dtC"
	
	Set bsSaucer=New cvpmBallStack
	bsSaucer.InitSaucer Kicker1, 36, 0, 40
	bsSaucer.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)

	Set bsSaucer2=New cvpmBallStack
	bsSaucer2.InitSaucer Kicker2, 37, 0, 40
	bsSaucer2.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)

	vpmNudge.TiltSwitch=7
	vpmNudge.Sensitivity=5
	vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot,RightSlingshot2)

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Turn on Gi
    vpmtimer.addtimer 1500, "GiOn '"

    ' Flupper lut code..
    table1.ColorGradeImage = luts(lutpos)  
    LoadLUT   
    SetLUT  

End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.stop:End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25

    If keycode = RightMagnaSave and EnableMagnasave = 1 then
		ResetRollingTextTimer
		lutpos = lutpos + 1 : If lutpos > ubound(luts) Then lutpos = 0 : end if
        call myChangeLut
		playsound "LutChange"
     End if
	
     If keycode = LeftMagnaSave and EnableMagnasave = 1 then
		ResetRollingTextTimer
		lutpos = lutpos - 1 : If lutpos < 0 Then lutpos = ubound(luts) : end if 
        call myChangeLut
		playsound "LutChange2"
    end if

    If vpmKeyDown(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundat "fx_PlungerPull",Plunger:Plunger.Pullback
    'If keycode = KeyRules Then Rules
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_plunger", Plunger:Plunger.Fire
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep, RStep2

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk
	DOF 101, DOFPulse
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 9
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
    PlaySoundAt SoundFX("fx_slingshot2", DOFContactors), Remk
	DOF 102, DOFPulse
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 19
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

Sub RightSlingShot2_Slingshot
    PlaySoundAt SoundFX("fx_slingshot3", DOFContactors), Remk
	DOF 102, DOFPulse
    r011.Visible = 1
    Remk001.RotX = 26
    RStep2 = 0
    vpmTimer.PulseSw 11
    RightSlingShot2.TimerEnabled = 1
End Sub

Sub RightSlingShot2_Timer
    Select Case RStep2
        Case 1:r011.Visible = 0:r010.Visible = 1:Remk001.RotX = 14
        Case 2:r010.Visible = 0:r009.Visible = 1:Remk001.RotX = 2
        Case 3:r009.Visible = 0:Remk001.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep2 = RStep2 + 1
End Sub

'**********
' Music
'**********

'Start the music

DIM music

music = "Evil Dead 2 Soundtrack.mp3"
PlayMusic "Evil Dead 2 Soundtrack.mp3" 

Sub Table1_MusicDone()
PlayMusic "Evil Dead 2 Soundtrack.mp3"
End Sub 

' Bumpers

' Scoring rubbers

' Drain & holes
Sub Drain_Hit:PlaysoundAt "fx_drain",Drain:bsTrough.AddBall Me:End Sub
Sub kicker1_Hit:PlaysoundAt "fx_kicker_enter", kicker1:bsSaucer.AddBall 0:End Sub
Sub kicker2_Hit:PlaysoundAt "fx_kicker_enter2", kicker2:bsSaucer2.AddBall 0:End Sub

' Rollovers
Sub sw38_Hit:Controller.Switch(38) = 1:PlaySoundAt "fx_sensor",sw38:End Sub
Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub

Sub sw39_Hit:Controller.Switch(39) = 1:PlaySoundAt "fx_sensor2",sw39:End Sub
Sub sw39_UnHit:Controller.Switch(39) = 0:End Sub

Sub sw40_Hit:Controller.Switch(40) = 1:PlaySoundAt "fx_sensor3",sw40:End Sub
Sub sw40_UnHit:Controller.Switch(40) = 0:End Sub

Sub sw38a_Hit:Controller.Switch(38) = 1:PlaySoundAt "fx_sensor4",sw38:End Sub
Sub sw38a_UnHit:Controller.Switch(38) = 0:End Sub

Sub sw39a_Hit:Controller.Switch(39) = 1:PlaySoundAt "fx_sensor5",sw39:End Sub
Sub sw39a_UnHit:Controller.Switch(39) = 0:End Sub

Sub sw40a_Hit:Controller.Switch(40) = 1:PlaySoundAt "fx_sensor6",sw40:End Sub
Sub sw40a_UnHit:Controller.Switch(40) = 0:End Sub


'Spinners

Sub Spinner1_Spin():vpmTimer.PulseSw 4:PlaySoundAt "fx_spinner", Spinner1:End Sub


' Droptargets

Sub sw17_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets),sw17:End Sub
Sub sw18_Hit:PlaySoundAt SoundFX("fx_droptarget2", DOFDropTargets),sw18:End Sub
Sub sw19_Hit:PlaySoundAt SoundFX("fx_droptarget3", DOFDropTargets),sw19:End Sub
Sub sw20_Hit:PlaySoundAt SoundFX("fx_droptarget4", DOFDropTargets),sw20:End Sub
Sub sw21_Hit:PlaySoundAt SoundFX("fx_droptarget5", DOFDropTargets),sw21:End Sub
Sub sw22_Hit:PlaySoundAt SoundFX("fx_droptarget6", DOFDropTargets),sw22:End Sub
Sub sw23_Hit:PlaySoundAt SoundFX("fx_droptarget7", DOFDropTargets),sw23:End Sub
Sub sw24_Hit:PlaySoundAt SoundFX("fx_droptarget8", DOFDropTargets),sw24:End Sub
Sub sw25_Hit:PlaySoundAt SoundFX("fx_droptarget9", DOFDropTargets),sw25:End Sub
Sub sw26_Hit:PlaySoundAt SoundFX("fx_droptarget10", DOFDropTargets),sw26:End Sub
Sub sw27_Hit:PlaySoundAt SoundFX("fx_droptarget11", DOFDropTargets),sw27:End Sub
Sub sw28_Hit:PlaySoundAt SoundFX("fx_droptarget12", DOFDropTargets),sw28:End Sub
Sub sw29_Hit:PlaySoundAt SoundFX("fx_droptarget13", DOFDropTargets),sw29:End Sub

'Targets
Sub sw12_Hit:vpmTimer.PulseSw 12:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw13_Hit:vpmTimer.PulseSw 13:PlaySoundAtBall SoundFX("fx_target2", DOFDropTargets):End Sub
Sub sw14_Hit:vpmTimer.PulseSw 14:PlaySoundAtBall SoundFX("fx_target3", DOFDropTargets):End Sub
Sub sw15_Hit:vpmTimer.PulseSw 15:PlaySoundAtBall SoundFX("fx_target4", DOFDropTargets):End Sub
Sub sw16_Hit:vpmTimer.PulseSw 16:PlaySoundAtBall SoundFX("fx_target5", DOFDropTargets):End Sub
Sub sw30_Hit:vpmTimer.PulseSw 30:PlaySoundAtBall SoundFX("fx_target6", DOFDropTargets):End Sub
Sub sw31_Hit:vpmTimer.PulseSw 31:PlaySoundAtBall SoundFX("fx_target7", DOFDropTargets):End Sub
Sub sw32_Hit:vpmTimer.PulseSw 32:PlaySoundAtBall SoundFX("fx_target8", DOFDropTargets):End Sub

'*********
'Solenoids
'*********
SolCallback(4)="Sol4"
SolCallback(5)="Sol5"
SolCallback(6)="vpmSolSound SoundFX(""fx_Knocker"",DOFKnocker),"
SolCallback(8)="SolOutHole"
SolCallback(9)="dtT.SolDropUp"
SolCallback(10)="dtC.SolDropUp"
SolCallback(11)="dtL.SolDropUp"
SolCallback(12)="SolBallRelease"
SolCallback(17)="SolBallMem"
SolCallback(18)="SolLock"
SolCallback(19)="vpmNudge.SolGameOn"
SolCallback(20)="vpmSolDiverter Diverter3,True,"


Sub Sol4(Enabled) 'bsSaucer
If Enabled Then
	bsSaucer.ExitSol_On
	Remk002F.RotateToEnd
	vpmtimer.Addtimer 200, "Remk002F.RotateToStart '"
End If
End Sub

Sub Sol5(Enabled) 'bsSaucer2
If Enabled Then
	bsSaucer2.ExitSol_On
	Remk003F.RotateToEnd
	vpmtimer.Addtimer 200, "Remk003F.RotateToStart '"
End If
End Sub

Sub SolOutHole(Enabled)
If Enabled Then
	bsTrough.EntrySol_On
End If
End Sub

Sub SolBallRelease(Enabled)
	If Enabled Then
		RightLockTimer.Enabled=0 'used for workaround routines at end of script
		If LockBalls+bsTrough.Balls=3 Then bsTrough.ExitSol_On
		Controller.Switch(8)=0 'used for workaround routines at end of script
	End If
End Sub

'lock
Dim LockBalls
LockBalls=0

Sub lock3_Hit:Controller.Switch(8)=1:LockBalls=LockBalls+1:End Sub
Sub lock3_unHit:Controller.Switch(8)=0:End Sub
Sub lock2_Hit:Controller.Switch(8)=1:End Sub
Sub lock2_unHit:Controller.Switch(8)=0:End Sub
Sub lock1_Hit:Controller.Switch(8)=1:End Sub
Sub lock1_unHit:Controller.Switch(8)=0:End Sub

Sub lockexit_Hit
	LockBalls=LockBalls-1
	If LockBalls=0 Then
		lock1k.Enabled=1
		lock2k.Enabled=0
		lock3k.Enabled=0
	End If
End Sub

Sub SolBallMem(Enabled)
	If Enabled Then
		If LockBalls=3 Then
			RightLockTimer.Enabled=1 'Delay Check
		End If
		Controller.Switch(8)=0
	End If
End Sub

Sub RightLockTimer_Timer
	Me.Enabled=0
		lock1k.Kick 225,2
		lock2k.Kick 225,2
		lock3k.Kick 225,2
End Sub

Sub lock1k_Hit:Me.Enabled=0:lock2k.Enabled=1:End Sub
Sub lock2k_Hit:Me.Enabled=0:lock3k.Enabled=1:End Sub
Sub lock3k_Hit
	Dim LockLights
		LockLights=0
			If Li5.State>0 Then LockLights=LockLights+1
			If Li37.State>0 Then LockLights=LockLights+1
			If Li21.State>0 Then LockLights=LockLights+1
			If Li53.State>0 Then LockLights=LockLights+1
			If Li26.State>0 Then LockLights=LockLights+1
			If Li10.State>0 Then LockLights=LockLights+1
			If Li9.State>0 Then LockLights=LockLights+1
			If Li41.State>0 Then LockLights=LockLights+1
			If Li25.State>0 Then LockLights=LockLights+1
			If Li57.State>0 Then LockLights=LockLights+1
			If LockLights=10 Then
			lock3k.Enabled=0
			lock2k.Enabled=0
			lock1k.Enabled=0
			Else
			lock1k.Enabled=1
			lock2k.Enabled=0
			lock3k.Enabled=0
			End If
Me.Enabled=0
End Sub

' Upperpf diverters

Sub SolLock(Enabled)
	If Enabled Then
		Diverter1.RotateToEnd
		Diverter2.RotateToEnd
	Else
		Diverter1.RotateToStart
		Diverter2.RotateToStart
	End If
End Sub


'**************
' Flipper Subs
'**************

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
    PlaySound "fx_rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*****************
'   Gi Effects
'*****************

Dim OldGiState
OldGiState = -1 'start witht he Gi off

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

'************************************
' Game timer for real time updates
'(some tables may use it's own timer)
'************************************

Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates
    RollingUpdate
	Remk002.Rotx = Remk002F.CurrentAngle
	Remk003.Rotx = Remk003F.CurrentAngle
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
			LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingState(chgLamp(ii, 0)) = chgLamp(ii, 1) + 3  'fading step
        Next
    End If
    UpdateLeds
    UpdateLamps
End Sub


Sub UpdateLamps()
    Lamp 1, li1
    Lamp 2, li2
    Lamp 3, li3
    Lamp 4, li4
	Lampm 5, li29
    Lamp 5, li5
    Lamp 6, li6
    Lamp 7, li7
    Lamp 8, li8
    Lampm 9, li47
    Lamp 9, li9
    Lampm 10, li48
    Lamp 10, li10
    'Textm 11, li11a, "Shoot Again"
    Lamp 11, li11
    Lamp 12, li12
    'Text 13, li13, "High Score to date" '"Ball in Play"
    Lamp 14, li14
    Lamp 15, li15
    Lamp 17, li17
    Lamp 18, li18
    Lamp 19, li19
    Lamp 20, li20
    Lampm 21, li67
    Lamp 21, li21
    Lamp 22, li22
    Lamp 23, li23
    Lamp 24, li24
    Lampm 25, li65
    Lamp 25, li25
    Lampm 26, li66
    Lamp 26, li26
    Lamp 27, li27
    Lamp 28, li28
    Lamp 30, li30
    Lamp 31, li31
    Lamp 33, li33
    Lamp 34, li34
    Lamp 35, li35
    Lamp 36, li36
    Lampm 37, li68
    Lamp 37, li37
    Lamp 38, li38
    Lamp 39, li39
    Lamp 40, li40
    Lampm 41, li64
    Lamp 41, li41
    Lampm 42, li42a
    Lamp 42, li42
    Lamp 43, li43
    Lamp 44, li44
    'Text 45, li45, "Game Over"
    Lamp 46, li46
    Lamp 49, li49
    Lamp 50, li50
    Lamp 51, li51
    Lamp 52, li52
    Lampm 53, li16
    Lamp 53, li53
    Lamp 54, li54
    Lamp 55, li55
    Lamp 56, li56
    Lampm 57, li32
    Lamp 57, li57
    Lamp 58, li58
    Lamp 59, li59
    Lamp 60, li60
    'Text 61, li61, "TILT"
    Lamp 62, li62
    'Text 63, li63, "Match"
End Sub

' div lamp subs

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
    LampTimer.Interval = 25 ' flasher fading speed
    LampTimer.Enabled = 1
    For x = 0 to 200
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

'Modulated lights & Flashers

Sub SetModLamp(nr, level)
	FlashLevel(nr) = level /150 'lights & flashers
End Sub

Sub LightMod(nr, object) ' modulated lights used as flashers
    Object.IntensityScale = FlashLevel(nr)
	Object.State = 1
End Sub

Sub FlashMod(nr, object) 'sets the flashlevel from the SolModCallback
    Object.IntensityScale = FlashLevel(nr)
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
'Set Digits(0) = a0
'Set Digits(1) = a1
'Set Digits(2) = a2
'Set Digits(3) = a3
'Set Digits(4) = a4
'Set Digits(5) = a5
'Set Digits(6) = a6

'Set Digits(7) = b0
'Set Digits(8) = b1
'Set Digits(9) = b2
'Set Digits(10) = b3
'Set Digits(11) = b4
'Set Digits(12) = b5
'Set Digits(13) = b6

'Set Digits(14) = c0
'Set Digits(15) = c1
'Set Digits(16) = c2
'Set Digits(17) = c3
'Set Digits(18) = c4
'Set Digits(19) = c5
'Set Digits(20) = c6

'Set Digits(21) = d0
'Set Digits(22) = d1
'Set Digits(23) = d2
'Set Digits(24) = d3
'Set Digits(25) = d4
'Set Digits(26) = d5
'Set Digits(27) = d6

'Set Digits(28) = e0
'Set Digits(29) = e1
'Set Digits(30) = e2
'Set Digits(31) = e3

Sub UpdateLeds
    On Error Resume Next
    Dim ChgLED, ii, jj, chg, stat
    ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(ChgLED)
            chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For jj = 0 to 10
                If stat = Patterns(jj)OR stat = Patterns2(jj)then Digits(chgLED(ii, 0)).SetValue jj
            Next
        Next
    End IF
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
    PlaySound soundname, 0, 1, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

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

        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) ' * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz)/17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******
' Rules
'******
Dim Msg(12)
Sub Rules()
    Dim X
    Msg(0) = "Cosmic Princess - Stern 1979" &Chr(10) &Chr(10)
    Msg(1) = "1. Pop Bumpers Score 1000."
    Msg(2) = "2. Drop Targets Score 500"
    Msg(3) = "        1st Time Down Lites 2X, 2nd Time Down Lites 3X"
    Msg(4) = "        3rd Time Down Lites 4X, 4th Time Down Lites 5X"
    Msg(5) = "3. Eject Pocket Value Advanced By Drop Target"
    Msg(6) = "4. Stationary Target Score 1000 and Advance Bonus."
    Msg(7) = "5. Spinners Score Indicated Value, SPotting Stationary Targets"
    Msg(8) = "    Increase Spinner Value."
    Msg(9) = "6. Star Rollover Scores 1000 and Increase Value on Both Lanes."
    Msg(10) = "7. Bottom Special Lane Lites After Lighting 4X"
    Msg(11) = "8. Special Score 1 Replay"
    Msg(12) = "9. Tilt Disqualifies Ball in Play Only"
    For X = 1 To 12
        Msg(0) = Msg(0) + Msg(X) &Chr(13)
    Next

    MsgBox Msg(0), , "         Instructions and Rule Card"
End Sub

'Stern Iron Maiden
'added by Inkochnito
Sub editDips
Dim vpmDips : Set vpmDips = New cvpmDips
With vpmDips
.AddForm 700,400,"Iron Maiden - DIP switches"
.AddFrame 2,0,190,"Maximum credits",&H00060000,Array("10 credits",0,"15 credits",&H00020000,"25 credits",&H00040000,"40 credits",&H00060000)'dip 18&19
.AddFrame 2,76,190,"High game to date",49152,Array("points",0,"1 free game",&H00004000,"2 free games",32768,"3 free games",49152)'dip 15&16
.AddFrame 2,152,190,"Special award",&HC0000000,Array("no award",0,"100,000 points",&H40000000,"free ball",&H80000000,"free game",&HC0000000)'dip 31&32
.AddFrame 2,228,190,"Add-a-ball memory",&H00600000,Array("one only",0,"maximum three",&H00200000,"maximum five",&H00600000)'dip 22&23
.AddFrame 205,0,190,"Special limit",&H20000000,Array("1 per ball in play",0,"1 per game",&H20000000)'dip 30
.AddFrame 205,46,190,"High score feature",&H00000020,Array("extra ball",0,"replay",&H00000020)'dip 6
.AddFrame 205,92,190,"Balls per game",&H00000040,Array("3 balls",0,"5 balls",&H00000040)'dip 7
.AddFrame 205,138,190,"Bonus time adjust",&H00002000,Array("30 seconds",0,"50 seconds",&H00002000)'dip 14
.AddChk 205,195,180,Array("Match feature",&H00100000)'dip 21
.AddChk 205,215,180,Array("Credits display",&H00080000)'dip 20
.AddChk 205,235,195,Array("Bonus multiplier in memory",&H10000000)'dip 29
.AddChk 205,255,180,Array("Extra ball awarded",&H00800000)'dip 24
.AddChk 205,275,180,Array("Background sound",&H00000080)'dip 8
.AddLabel 50,310,300,20,"After hitting OK, press F3 to reset game with new settings."
.ViewDips
End With
End Sub
Set vpmShowDips = GetRef("editDips")

Sub Table1_Exit():SaveLUT:Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub

'********************
' LUT Stuff
'********************
'0 = Fleep Natural Dark 1
'1 = Fleep Natural Dark 2
'2 = Fleep Warm Dark
'3 = Fleep Warm Bright
'4 = Fleep Warm Vivid Soft
'5 = Fleep Warm Vivid Hard
'6 = Skitso Natural and Balanced
'7 = Skitso Natural High Contrast
'8 = 3rdaxis Referenced THX Standard
'9 = CalleV Punchy Brightness and Contrast
'10 = HauntFreaks Desaturated
'11 = Tomate Washed Out 
'12 = VPW Original 1 to 1
'13 = Bassgeige
'14 = Blacklight
'15 = B&W Comic Book
'16 = NBA Original

Dim LUTset

Sub SetLUT  'AXS
	Table1.ColorGradeImage = "LUT" & lutpos
end sub 

Sub SaveLUT
	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if

	if lutpos = "" then lutpos = 16 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "EvilDead2LUT.txt",True)
	ScoreFile.WriteLine lutpos
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub

Sub LoadLUT
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		lutpos=17
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "EvilDead2LUT.txt") then
		lutpos=17
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "EvilDead2LUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			lutpos=17
			Exit Sub
		End if
		lutpos = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub


' *************************  script for LUT text display - Flupper1 *********************************
' script related to the script below is in keydown magnasave keys script and luts/lutpos script at the top of the script
' other objects used: layer 7 all Text0?? objects ("A") and the textures "32" to "96"

dim rollingtext
dim textindex : textindex = 1
dim charobj(55), glyph(201)
InitDisplayText

Sub text010_Timer()
	dim tekst
	tekst = Mid(rollingtext, textindex, 34)
	DisplayText -1, tekst
	textindex = textindex + 1
	If textindex > len(rollingtext) - 36 then textindex = 1 : end if
End Sub

Sub myChangeLut
    Table1.ColorGradeImage = "LUT" & lutpos
	DisplayText lutpos, luts(lutpos)
	vpmTimer.AddTimer 2000, "If lutpos = " & lutpos & " then for anr = 10 to 54 : charobj(anr).visible = 0 : next'"
End Sub

Sub ResetRollingTextTimer()
	Dim anr
	text010.TimerEnabled = False : textindex = 1
	For anr = 10 to 43 : charobj(anr).opacity = 4000 : charobj(anr).height = 150 : charobj(anr).visible = 0 : next
End Sub

Sub InitDisplayText
	Dim anr
	For anr = 10 to 54 : set charobj(anr) = eval("text0" & anr) : charobj(anr).visible = 0 : Next
	For anr = 32 to 96 : glyph(anr) = anr : next
	For anr = 0 to 31 : glyph(anr) = 32 : next
	for anr = 97 to 122 : glyph(anr)  = anr - 32 : next
	for anr = 123 to 200 : glyph(anr) = 32 : next
End Sub

Sub DisplayText(nr, luttext)
	dim tekst, anr
	for anr = 10 to 54 : charobj(anr).imageA = 32 : charobj(anr).visible = 1 : next
	If nr > -1 then
		tekst = "lutpos:" & nr
		For anr = 1 to len(tekst) : charobj(43 + anr).imageA = glyph(asc(mid(tekst, anr, 1))) : Next
	End If
	For anr = 1 to len(luttext)
		charobj(9 + anr).imageA = glyph(asc(mid(luttext, anr, 1)))
		If nr = -1 Then
			charobj(9 + anr).y = 1500 + sin(((textindex * 4 + anr)/20)*3.14) * 100
			charobj(9 + anr).height = 150 + cos(((textindex * 4 + anr)/20)*3.14) * 100
		End If
	Next
End Sub