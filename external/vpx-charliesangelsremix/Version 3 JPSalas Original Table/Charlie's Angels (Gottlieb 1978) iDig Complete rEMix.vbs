' Gottlieb's Charlie's Angels / IPD No. 492 / November, 1978 / 4 Players
' VPX - version by JPSalas 2018, version 1.0.2

Option Explicit
Randomize

Const BallSize = 50
Const BallMass = 1.1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01550000", "gts1.vbs", 3.26

Dim bsTrough, dtL, dtR, x

Const cGameName = "charlies"

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 0


'************************************************************************************************************************
'************************************************************************************************************************

 'ADDING YOUR OWN MUSIC (Optional)

 'Place as many songs as you want (MP3 ONLY) in the Charlie70 and CharlieRemix music folders making sure to rename the files 1.mp3 2.mp3 3.mp3 etc...
 'Then come here and change this number to the total number of songs in that folder. All Done!

Const NumMusicTracks = 9		'# of Songs in the Charlie70 Music Folder

Const NumMusicTracks2 = 11		'# of Songs in the CharlieRemix Music Folder

'************************************************************************************************************************

'TABLE MOD OPTIONS

Const ModSelect = 2   		  		'Select 
									'0= Charlie's 70's Cop Show Mod   
									'1= iDig rEMix Vol.1  Music mod
									'2= No Mods (Original Game)

Const ThemeSong = 1			    '0=Off 1=On   Charlie's Angels theme plays when table loads

Const DTSelect = 2				'Select your drop target(5) sounds  [0=Trumpetstab 1=PianoHit 2=Random]

'************************************************************************************************************************
'************************************************************************************************************************

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
        .SplashInfoLine = "Charlie's Angels - Gottlieb 1978" & vbNewLine & "VPX table by JPSalas v.1.0.2"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = VarHidden
        .Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 4
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Leftslingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 66, 0, 0, 0, 0, 0, 0
        .InitKick BallRelease, 90, 4
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 1
    End With

    ' Drop targets
    Set dtL = New cvpmDropTarget
    dtL.InitDrop Array(sw30, sw31, sw32, sw33, sw34), Array(30, 31, 32, 33, 34)
    dtL.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtL.CreateEvents "dtL"

    Set dtR = New cvpmDropTarget
    dtR.InitDrop Array(sw10, sw11, sw12), Array(10, 11, 12)
    dtR.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtR.CreateEvents "dtR"

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
	If ModSelect = 0 And ThemeSong = 1 Then PlayMusic"EM\Charlie\Charlie70\Theme.mp3"
	If ModSelect = 1 And ThemeSong = 1 Then PlayMusic"EM\Charlie\Charlie70\Theme.mp3"
	'If ThemeSong = 0 Then EndMusic 

    ' Turn on Gi
    vpmtimer.addtimer 2000, "GiOn '"
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.stop:End Sub

'****************
'MUSIC MOD 
'****************

Sub MusicOn
If  ModSelect = 0 Then 
	Dim x
    x = INT(NumMusicTracks * Rnd + 1)
      PlayMusic "EM\Charlie\Charlie70\"& x &".mp3" 
End If    
If ModSelect = 1 Then
    Dim y
    y = INT(NumMusicTracks2 * Rnd + 1)
      PlayMusic "EM\Charlie\CharlieRemix\"& y &".mp3" 
End If
End Sub
 
 Sub Table1_MusicDone()
   MusicOn
 End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
	If KeyCode = RightMagnaSave Then MusicOn
    If KeyCode = LeftMagnaSave Then EndMusic
    If KeyCode = StartGameKey Then EndMusic
 	If keycode = StartGameKey And ModSelect = 0 Then PlayMusic "EM\Charlie\Charlie70\00.mp3" 
 	If keycode = StartGameKey And ModSelect = 1 Then PlayMusic "EM\Charlie\Charlie70\00.mp3"

		
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then Plunger.Pullback
	If keycode = PlungerKey And ModSelect = 0 Then PlaySound "sfx_carstart", 0, 1, 0.1, 0.25
	If keycode = PlungerKey And ModSelect = 1 Then PlaySound "fx_plungerpull", 0, 1, 0.1, 0.25
	If keycode = PlungerKey And ModSelect = 2 Then PlaySound "fx_plungerpull", 0, 1, 0.1, 0.25
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If vpmKeyUp(keycode) Then Exit Sub
    If keycode = PlungerKey Then Plunger.Fire
	If keycode = PlungerKey And ModSelect = 0 Then PlaySound "sfx_skidding", 0, 1, 0.1, 0.25
	If keycode = PlungerKey And ModSelect = 1 Then PlaySound "fx_plunger", 0, 1, 0.1, 0.25
	If keycode = PlungerKey And ModSelect = 2 Then PlaySound "fx_plunger", 0, 1, 0.1, 0.25
End Sub

Sub Delay( seconds )
	Dim wshShell, strCmd
	Set wshShell = CreateObject( "WScript.Shell" )
	strCmd = wshShell.ExpandEnvironmentStrings( "%COMSPEC% /C (PING.EXE -n " & ( seconds + 1 ) & " localhost >NUL 2>&1)" )
	wshShell.Run strCmd, 0, 1
	Set wshShell = Nothing
End Sub

'Drain Sub

Sub DrainHit

If ModSelect = 0  Then
	EndMusic
	Dim x
	x = INT(2 * RND(1) )
	Select Case x
	Case 0:PlayMusic "em\charlie\charlie70\dh1.mp3"
	Case 1:PlayMusic "em\charlie\charlie70\dh2.mp3"
End Select
End if
End Sub

Sub DTStop
If ModSelect = 0 Then
							'DT stop sound
	StopSound"mfx_piano"
	StopSound"mfx_mutedtrumpet"
	
End If		
End Sub

Sub gunshots
If ModSelect = 0 Then
		Dim ww
	ww = INT(5 * RND(1) )
	Select Case ww
	Case 0:PlaySound"sfx_goodshot"
	Case 1:PlaySound"sfx_gunshot1"
	Case 2:PlaySound"sfx_gunshot2"
	Case 3:PlaySound"sfx_gunshot4"
	Case 4:PlaySound"sfx_gunshotbongos"
End Select
End If
End Sub

Sub mfx
If ModSelect = 0 Then
		Dim mm
	mm = INT(2 * RND(1) )
	Select Case mm
	Case 0:PlaySound"mfx_mutedtrumpet"
	Case 1:PlaySound"mfx_piano"
	
End Select
End If
End Sub

Sub DTSounds
If ModSelect = 0 Then
	If DTSelect = 0 Then PlaySound"mfx_mutedtrumpet"
	If DTSelect = 1 Then PlaySound"mfx_piano"
	If DTselect = 2 Then mfx
End If
End Sub

Sub kungfu
If ModSelect = 0 Then
		Dim x
	x = INT(10 * RND(1) )
	Select Case x
	Case 0:PlaySound"sfx_karate1"
	Case 1:PlaySound"sfx_karate2"
	Case 2:PlaySound"sfx_karate3"
	Case 3:PlaySound"sfx_karate4"
	Case 4:PlaySound"sfx_kungfumiss1"
	Case 5:PlaySound"sfx_kungfumiss2"
	Case 6:PlaySound"sfx_kungfumiss3"
	Case 7:PlaySound"sfx_kungfumiss4"
	Case 8:PlaySound"sfx_kungfuhit1"
	Case 9:PlaySound"sfx_kungfuhit2"

End Select
End if
End Sub

Sub stopkungfu
If ModSelect = 0 Then

	StopSound"sfx_karate1"
	StopSound"sfx_karate2"
	StopSound"sfx_karate3"
	StopSound"sfx_karate4"
	StopSound"sfx_kungfumiss1"
	StopSound"sfx_kungfumiss2"
	StopSound"sfx_kungfumiss3"
	StopSound"sfx_kungfumiss4"
	StopSound"sfx_kungfuhit1"
	StopSound"sfx_kungfuhit2"
End If
End Sub

Sub angels
If ModSelect = 0 Then
		Dim y
	y = INT(15 * RND(1) )
	Select Case y
	Case  0:PlaySound"dx_blackbelt"
	Case  1:PlaySound"dx_callcharlie"
	Case  2:PlaySound"dx_checkemout"
	Case  3:PlaySound"dx_getdown"
	Case  4:PlaySound"dx_holdit"
	Case  5:PlaySound"dx_myname"
	Case  6:PlaySound"dx_privatedick"
	Case  7:PlaySound"dx_prop"
	Case  8:PlaySound"dx_putitdown"
	Case  9:PlaySound"dx_stiritup"
	Case 10:PlaySound"dx_takeachance"
	Case 11:PlaySound"dx_whatsthegun4"
	Case 12:PlaySound"dx_whereareyou"
	Case 13:PlaySound"dx_wheredowestart"
	Case 14:PlaySound"dx_wegottamove"
End Select
End if
End Sub

Sub stopangels
If ModSelect = 0 Then
	StopSound"dx_blackbelt"
	StopSound"dx_callcharlie"
	StopSound"dx_checkemout"
	StopSound"dx_getdown"
	StopSound"dx_holdit"
	StopSound"dx_myname"
	StopSound"dx_privatedick"
	StopSound"dx_prop"
	StopSound"dx_putitdown"
	StopSound"dx_stiritup"
	StopSound"dx_takeachance"
	StopSound"dx_whatsthegun4"
	StopSound"dx_whereare you"
	StopSound"dx_wheredowestart"
	StopSound"dx_wegottamove"

End If
End Sub

Sub badguys
If ModSelect = 0 Then
		Dim z
	z = INT(6 * RND(1) )
	Select Case z
	Case 0:PlaySound"dx_callthatdriving"
	Case 1:PlaySound"dx_dontneedgun"
	Case 2:PlaySound"dx_nicenez"
	Case 3:PlaySound"dx_oktiger"
	Case 4:PlaySound"dx_whatthedevil"
	Case 5:PlaySound"dx_whatthedevil2"

End Select
End If
End Sub

Sub youredead
If ModSelect = 0 Then
		Dim xx
	xx = INT(9 * RND(1) )
	Select Case xx
	Case 0:PlaySound"dx_cleanshot"
	Case 1:PlaySound"dx_letsget"
	Case 2:PlaySound"dx_noclues"
	Case 3:PlaySound"dx_whatudoing"
	Case 4:PlaySound"dx_youdidwhat"
	Case 5:PlaySound"dx_uhlady"
	Case 6:PlaySound"dx_mailing"
	Case 7:PlaySound"dx_murphys"
	Case 8:PlaySound"dx_reportit"
End Select
End If
End Sub


Sub coppers
If ModSelect = 0 Then
		Dim yy
	yy = INT(4 * RND(1) )
	Select Case yy
	Case 0:PlaySound"dx_gimmethecops"
	Case 1:PlaySound"sfx_static1"
	Case 2:PlaySound"sfx_static2"
	Case 3:PlaySound"sfx_siren"

End Select
End If
End Sub

Sub Ricochets
If ModSelect = 0 Then
Dim zz
	zz = INT(3 * RND(1) )
	Select Case zz
	Case 0:PlaySound"sfx_ricochet"
	Case 1:PlaySound"sfx_ricochet2"
	Case 2:PlaySound"sfx_ricochet3"

End Select
End If
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySound SoundFXDOF("fx_slingshot", 207, DOFPulse, DOFContactors), 0, 1, -0.05, 0.05
	Ricochets
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 54
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
    PlaySound SoundFXDOF("fx_slingshot", 208, DOFPulse, DOFContactors), 0, 1, 0.05, 0.05
	Ricochets
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 54
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

' Rubber & animations
Dim Rub1, Rub2, Rub3, Rub4

Sub sw54a_Hit:vpmTimer.PulseSw 54:Rub1 = 1:sw54a_Timer:End Sub

Sub sw54a_Timer
    Select Case Rub1
        Case 1:r11.Visible = 1:sw54a.TimerEnabled = 1
        Case 2:r11.Visible = 0:r14.Visible = 1
        Case 3:r14.Visible = 0:sw54a.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub sw54b_Hit:vpmTimer.PulseSw 54:Rub2 = 1:sw54b_Timer:PlaySound"fx_slingshot":Ricochets:End Sub
Sub sw54b_Timer
    Select Case Rub2
        Case 1:r15.Visible = 1:sw54b.TimerEnabled = 1
        Case 2:r15.Visible = 0:r16.Visible = 1
        Case 3:r16.Visible = 0:sw54b.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub

Sub sw54c_Hit:vpmTimer.PulseSw 54:Rub3 = 1:sw54c_Timer:PlaySound"fx_slingshot":Ricochets:End Sub
Sub sw54c_Timer
    Select Case Rub3
        Case 1:r17.Visible = 1:sw54c.TimerEnabled = 1
        Case 2:r17.Visible = 0:r18.Visible = 1
        Case 3:r18.Visible = 0:sw54c.TimerEnabled = 0
    End Select
    Rub3 = Rub3 + 1

End Sub

Sub sw54d_Hit:vpmTimer.PulseSw 54:Rub4 = 1:sw54d_Timer:PlaySound"fx_slingshot":Ricochets:End Sub
Sub sw54d_Timer
    Select Case Rub4
        Case 1:r19.Visible = 1:sw54d.TimerEnabled = 1
        Case 2:r19.Visible = 0:r20.Visible = 1
        Case 3:r20.Visible = 0:sw54d.TimerEnabled = 0
    End Select
    Rub4 = Rub4 + 1

End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 50:PlaySound SoundFXDOF("fx_bumper", 206, DOFPulse, DOFContactors), 0, 1, 0:gunshots:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 50:PlaySound SoundFXDOF("fx_bumper2", 205, DOFPulse, DOFContactors), 0, 1, 0:gunshots:End Sub

' Drain & Saucers
 

Sub Drain_Hit

If ModSelect=0  Then Playsound"fx_drain":bsTrough.AddBall Me:DrainHit:End If

If ModSelect=1  Then Playsound"fx_drain":bsTrough.AddBall Me: End If

If ModSelect=2  Then Playsound"fx_drain":bsTrough.AddBall Me: End If

End Sub


' Rollovers
Sub sw14_Hit:Controller.Switch(14) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):DOF 203, DOFOn:youredead:End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:DOF 203, DOFOff:End Sub

Sub sw14a_Hit:Controller.Switch(14) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):DOF 204, DOFOn:youredead:End Sub
Sub sw14a_UnHit:Controller.Switch(14) = 0:DOF 204, DOFOff:End Sub

Sub sw13_Hit:Controller.Switch(13) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):coppers:End Sub
Sub sw13_UnHit:Controller.Switch(13) = 0:End Sub

Sub sw43_Hit:Controller.Switch(43) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):badguys:End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub

Sub sw40_Hit:Controller.Switch(40) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):DOF 201, DOFOn:stopangels:angels:End Sub
Sub sw40_UnHit:Controller.Switch(40) = 0:DOF 201, DOFOff:End Sub

Sub sw40a_Hit:Controller.Switch(40) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):DOF 202, DOFOn:stopangels:angels:End Sub
Sub sw40a_UnHit:Controller.Switch(40) = 0:DOF 202, DOFOff:End Sub

Sub sw41_Hit:Controller.Switch(41) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):stopangels:angels:End Sub
Sub sw41_UnHit:Controller.Switch(41) = 0:End Sub

Sub sw42_Hit:Controller.Switch(42) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):stopangels:angels:End Sub
Sub sw42_UnHit:Controller.Switch(42) = 0:End Sub

Sub sw20_Hit:Controller.Switch(20) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):stopkungfu:kungfu:End Sub
Sub sw20_UnHit:Controller.Switch(20) = 0:End Sub

Sub sw21_Hit:Controller.Switch(21) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):stopkungfu:kungfu:End Sub
Sub sw21_UnHit:Controller.Switch(21) = 0:End Sub

Sub sw22_Hit:Controller.Switch(22) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):stopkungfu:kungfu:End Sub
Sub sw22_UnHit:Controller.Switch(22) = 0:End Sub

Sub sw23_Hit:Controller.Switch(23) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):stopkungfu:kungfu:End Sub
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub

Sub sw24_Hit:Controller.Switch(24) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):stopkungfu:kungfu:End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub

'Targets
Sub sw44_Hit:vpmTimer.PulseSw 44:PlaySound SoundFX("fx_target", DOFDropTargets), 0, 1, pan(ActiveBall)
If ModSelect=0 Then Playsound "sfx_alarm":PlaySound"dx_bullseye":End if:End Sub

' Droptargets (only sound effect)
Sub sw30_Hit:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):DTStop:DTSounds:End Sub
Sub sw31_Hit:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):DTStop:DTSounds:End Sub
Sub sw32_Hit:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):DTStop:DTSounds:End Sub
Sub sw33_Hit:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):DTStop:DTSounds:End Sub
Sub sw34_Hit:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):DTStop:DTSounds:End Sub


Sub sw10_Hit:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):If ModSelect=0 Then PlaySound"mfx_horndip":End If:End Sub
Sub sw11_Hit:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):If ModSelect=0 Then PlaySound"mfx_horndip":End If:End Sub
Sub sw12_Hit:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):If ModSelect=0 Then PlaySound"mfx_horndip":End If:End Sub

'*********
'Solenoids
'*********

SolCallback(1) = "bsTrough.SolOut"
SolCallback(2) = "vpmsolsound SoundFX(""fx_knocker"",DOFKnocker),"
SolCallback(7) = "dtR.SolDropUp"
SolCallback(8) = "dtL.SolDropUp"
SolCallback(17) = "vpmNudge.SolGameOn"

'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFFlippers), 0, 1, -0.1, 0.05
        LeftFlipper2.RotateToEnd:LeftFlipper3.RotateToEnd:LeftFlipper4.RotateToEnd:LeftFlipper5.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFFlippers), 0, 1, -0.1, 0.05
        LeftFlipper2.RotateToStart:LeftFlipper3.RotateToStart:LeftFlipper4.RotateToStart:LeftFlipper5.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFFlippers), 0, 1, 0.1, 0.05
        RightFlipper2.RotateToEnd:RightFlipper3.RotateToEnd:RightFlipper4.RotateToEnd:RightFlipper5.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFFlippers), 0, 1, 0.1, 0.05
        RightFlipper2.RotateToStart:RightFlipper3.RotateToStart:RightFlipper4.RotateToStart:RightFlipper5.RotateToStart
    End If
End Sub

Sub LeftFlipper2_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.25
End Sub

Sub RightFlipper2_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.25
End Sub

'*****************
'   Gi Lights
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
    'backdrop lights
    If VarHidden Then
        NFadeLm 1, li1a                  'Ball in play
        NFadeL 2, li2                    'Tilt
        NFadeL 3, li3                    'High Game to date
        NFadeLm 4, li4a                  'Same Player Shoots Again
        li1.State = ABS(li1a.State - 1)  'game over
        li1b.State = ABS(li1a.State - 1) 'match
    End If

    NFadeL 4, li4
    NFadeL 5, li5
    NFadeL 6, li6
    NFadeL 7, li7
    NFadeL 8, li8
    NFadeL 9, li9
    NFadeL 10, li10
    NFadeL 11, li11
    NFadeL 12, li12
    NFadeL 13, li13
    NFadeL 14, li14
    NFadeL 15, li15
    NFadeL 16, li16
    NFadeL 17, li17
    NFadeL 18, li18
    NFadeL 19, li19
    NFadeL 20, li20
    NFadeL 21, li21
    NFadeL 22, li22
    NFadeL 23, li23
    NFadeL 24, li24
    NFadeL 25, li25
    NFadeL 26, li26
    NFadeL 27, li27
    NFadeL 28, li28
    NFadeL 29, li29
    NFadeL 30, li30
    NFadeL 31, li31
    NFadeL 32, li32
    NFadeL 33, li33
    NFadeL 34, li34
    NFadeL 35, li35
    NFadeL 36, li36
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

'Gottliebs
Patterns(0) = 0     'empty
Patterns(1) = 63    '0
Patterns(2) = 6     '1
Patterns(3) = 91    '2
Patterns(4) = 79    '3
Patterns(5) = 102   '4
Patterns(6) = 109   '5
Patterns(7) = 124   '6
Patterns(8) = 7     '7
Patterns(9) = 127   '8
Patterns(10) = 103  '9

Patterns2(0) = 128  'empty
Patterns2(1) = 191  '0
Patterns2(2) = 768  '134  '1
Patterns2(3) = 219  '2
Patterns2(4) = 207  '3
Patterns2(5) = 230  '4
Patterns2(6) = 237  '5
Patterns2(7) = 253  '6
Patterns2(8) = 135  '7
Patterns2(9) = 255  '8
Patterns2(10) = 239 '9

'Assign 6-digit output to reels
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

Set Digits(26) = f0
Set Digits(27) = f1

Sub UpdateLeds
    On Error Resume Next
    Dim ChgLED, ii, jj, chg, stat
    ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
    If Not IsEmpty(ChgLED) Then
        For ii = 0 To UBound(ChgLED)
            chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For jj = 0 to 10
                If stat = Patterns(jj) OR stat = Patterns2(jj) then Digits(chgLED(ii, 0) ).SetValue jj
            'debug.print stat
            Next
        Next
    End IF
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySound "fx_MetalHit2", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_rubber_post", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_rubber_pin", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
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
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'Gottlieb System 1 games with 3 tones only (or chimes).
'These are: Charlie's Angels, Cleopatra, Close Encounters, Count Down, Dragon, Joker Poker, Pinball Pool, Sinbad, Solar Ride.
'For the other System 1 games use the file called "Gottlieb System 1 with multi-mode sound.txt"
'Added by Inkochnito

Sub editDips
    Dim vpmDips:Set vpmDips = New cvpmDips
    With vpmDips
        .AddForm 700, 400, "System 1 (3 tones) - DIP switches"
        .AddFrame 205, 0, 190, "Maximum credits", &H00030000, Array("5 credits", 0, "8 credits", &H00020000, "10 credits", &H00010000, "15 credits", &H00030000) 'dip 17&18
        .AddFrame 0, 0, 190, "Coin chute control", &H00040000, Array("seperate", 0, "same", &H00040000)                                                          'dip 19
        .AddFrame 0, 46, 190, "Game mode", &H00000400, Array("extra ball", 0, "replay", &H00000400)                                                              'dip 11
        .AddFrame 0, 92, 190, "High game to date awards", &H00200000, Array("no award", 0, "3 replays", &H00200000)                                              'dip 22
        .AddFrame 0, 138, 190, "Balls per game", &H00000100, Array("5 balls", 0, "3 balls", &H00000100)                                                          'dip 9
        .AddFrame 0, 184, 190, "Tilt effect", &H00000800, Array("game over", 0, "ball in play only", &H00000800)                                                 'dip 12
        .AddChk 205, 80, 190, Array("Match feature", &H00000200)                                                                                                 'dip 10
        .AddChk 205, 95, 190, Array("Credits displayed", &H00001000)                                                                                             'dip 13
        .AddChk 205, 110, 190, Array("Play credit button tune", &H00002000)                                                                                      'dip 14
        .AddChk 205, 125, 190, Array("Play tones when scoring", &H00080000)                                                                                      'dip 20
        .AddChk 205, 140, 190, Array("Play coin switch tune", &H00400000)                                                                                        'dip 23
        .AddChk 205, 155, 190, Array("High game to date displayed", &H00100000)                                                                                  'dip 21
        .AddLabel 50, 240, 300, 20, "After hitting OK, press F3 to reset game with new settings."
        .ViewDips
    End With
End Sub
Set vpmShowDips = GetRef("editDips")