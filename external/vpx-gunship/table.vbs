' Lost World - Bally 1977
' http://www.ipdb.org/machine.cgi?id=1476
' Lost World / IPD No. 1476 / February 10, 1977 / 4 Players
' VPX version by JPSalas 2017, version 1.0.1
' Light numbers from the table by Joe Entropy & RipleYYY

Option Explicit
Randomize

'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = True               ' enable Pinup Player functions for this table
cPuPPack = "Gunship"    ' name of the PuP-Pack / PuPVideos folder for this table

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup Player

Dim gameStarted
Dim keyStarted

Sub PuPStart()
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub

Sub pupevent(EventNum)
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

' ******* How to use PUPEvent to trigger / control a PuP-Pack *******

' Usage: pupevent(EventNum)

' EventNum = PuP Exxx trigger from the PuP-Pack

' Example: pupevent 102

' This will trigger E102 from the table's PuP-Pack

' DO NOT use any Exxx triggers already used for DOF (if used) to avoid any possible confusion

'************ PuP-Pack Startup **************

PuPStart() 'Check for PuP - If found, then start Pinup Player / PuP-Pack
Randomize

Dim BallGuardActive

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const BallSize = 50

LoadVPM "01550000", "Bally.vbs", 3.26

Dim bsTrough, bsLSaucer, bsRSaucer, cbR
Dim x

Const cGameName= "lostwrld"

Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0

Dim VarHidden
    VarHidden = 0
    For each x in aReels
        x.Visible = 0
    Next
    lrail.Visible = 0
    rrail.Visible = 0

' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = ""
Const SCoin = "fx_coin"

'************************
'Music Toggle for PupPack
'*************************

'Select True if using the PupPack to mute Music Folder Audio, False if not using Pup
Const PupEnabled = True


'************************
'Glowball
'*************************
Const ChooseBall 			= 7		' *** Ball Settings **********
									' *** 0 = Normal Ball	
									' *** 1 = Purple GlowBall
									' *** 7 = Blue GlowBall																		

									



'************
' Table init.
'************

Sub table1_Init
    vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Lost World - Bally 1977" & vbNewLine & "VPX table by JPSalas v.1.0.1"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Games("lostwrld").Settings.Value("sound") = 0
        .Hidden = VarHidden
        '.Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 7
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 8, 0, 0, 0, 0, 0, 0
        .InitKick BallRelease, 80, 6
        .InitEntrySnd "fx_Solenoid", "fx_Solenoid"
        .InitExitSnd "fx_ballrel", "fx_Solenoid"
        .Balls = 1
    End With

    ' Captive right ball
    Set cbR = New cvpmCaptiveBall
    With cbR
        .InitCaptive cbRtrigger, cbRwall, cbRkicker, 0
        .Start
        .ForceTrans = .99
        .MinForce = 3.8
    '.CreateEvents "cbR"
    End With

    ' Left Saucer
    Set bsLSaucer = New cvpmBallStack
    With bsLSaucer
        .InitSaucer sw23, 23, 145, 20
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .KickAngleVar = 2
        .KickForceVar = 2
    End With

    ' Right Saucer
    Set bsRSaucer = New cvpmBallStack
    With bsRSaucer
        .InitSaucer sw24, 24, 215, 20
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .KickAngleVar = 2
        .KickForceVar = 2
    End With

    

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
	BallGuardActive = False
	popup.HasHitEvent = False
	popup.Collidable = False

	If PupEnabled=False Then MusicOn

	Glowball_Init
    gameStarted = 0
    keyStarted = 0
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
'Sub Table1_Exit:Controller.Stop:End Sub

'*******************
' PopUp Primitive
'*******************

sub startBallGuard()
	if BallGuardActive = True Then
    BallGuardtimer.enabled = true
    lightpost()
	End If
end sub

sub BallGuardtimer_Timer()
    BallGuardtimer.enabled=false
    endBallGuard()
end sub

sub endBallGuard()
    Popupsolenoidpulse.Enabled = 1
    popup.Isdropped = 1
	BallGuardActive = False
end sub

sub lightpost()

    posttimer.enabled=true
    PopupAnimUp 0, 100
    vpmtimer.addtimer 20000, "PopupAnimDown 100, 0 '"
   'flashforms postlight, 20000, 150, 0
   'flashforms BulbPost, 20000, 150, 0
 '   end if
end sub 

Dim HPos, HPosEnd
 Sub Popupsolenoidpulse_timer() 
    PopupP.Z = HPos
	If Hpos < HposEnd Then
     HPos = HPos + 1
	Else
	 Popupsolenoidpulse.enabled = 0
	End If
end Sub 

 Sub Popupsolenoidpulse2_timer() 
    PopupP.Z = HPos
	If Hpos > HposEnd Then
     HPos = HPos - 1
	Else
	 Popupsolenoidpulse2.enabled = 0
	End If
end Sub 

Sub PopupAnimUp(FrameStart, FrameEnd)
    popup.Isdropped = 0
	popup.HasHitEvent = True
	popup.Collidable = True
    HPos = FrameStart         
    HPosEnd = FrameEnd
	Popupsolenoidpulse.enabled = 1
End Sub

Sub PopupAnimDown(FrameStart, FrameEnd)
    popup.Isdropped = 1
	popup.HasHitEvent = False
	popup.Collidable = False
    HPos = FrameStart         
    HPosEnd = FrameEnd
	Popupsolenoidpulse2.enabled = 1
End Sub

'**********************************************************************************************************
'Gunship Tunes
'**********************************************************************************************************

Sub MusicOn
	
    Dim FileSystemObject, folder, r, ct, file, musicPath, myMusicFolder
    myMusicFolder = "Gunship" 
    Set FileSystemObject = CreateObject("Scripting.FileSystemObject")
    musicPath = FileSystemObject.GetAbsolutePathName(".") ' get path to Visual Pinball\table
    musicPath = Left(musicPath, Len(musicPath) - 6) + "music\" 'get path to Visual Pinball\music
    
	Set folder = FileSystemObject.GetFolder(musicPath + myMusicFolder) 'Comment out if using custom path

	'*****************************************************************************************************************
	'NOTE- If you use a non-standard folder structure {IE not using "VisualPinball\Tables" and "VisualPinball\Music"} 
	'please enter in the direct path to music sub-folder on the "Set Folder=" below and comment out "Set Folder=" Line above.
	'*****************************************************************************************************************
	
	'Set custom path below
	'Set folder= FileSystemObject.GetFolder("C:\vPinball\VisualPinball\Music\Evanescne")
	
    Randomize
    r = INT(folder.Files.Count * Rnd + 1)
    ct=1
    For Each file in folder.Files
        if ct = r Then 
            if (LCase(Right(file,4))) = ".mp3" Then ' can only play mp3 files
               PlayMusic Mid(file, Len(musicPath) + 1, 1000) 
            End If 
       End If
   ct = ct + 1
   Next
 End Sub
 
Sub Table1_MusicDone() 
    MusicOn
 End Sub





'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = PlungerKey Then PlaySound "fx_PlungerPull", 0, 1, 0.1, 0.25:Plunger.Pullback
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
	If keycode = RightMagnaSave Then PuPEvent 800
	If keycode = LeftMagnaSave Then PuPEvent 801
	If keycode = StartGameKey Then
        keyStarted = 1
        PuPEvent 802
    End If
	
    If vpmKeyDown(keycode)Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySound "fx_plunger", 0, 1, 0.1, 0.25:Plunger.Fire
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
    vpmTimer.PulseSw 37
    LeftSlingShot.TimerEnabled = 1
    'flash the gun
    flash1.Duration 1, 150, 0
    flash3.Duration 1, 150, 0
    

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
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 36
    RightSlingShot.TimerEnabled = 1
    'flash the gun
    flash2.Duration 1, 150, 0
    flash4.Duration 1, 150, 0
    
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub r2a_Hit:vpmTimer.PulseSw 2:End Sub
Sub r2b_Hit:vpmTimer.PulseSw 2:End Sub
Sub r1a_Hit:vpmTimer.PulseSw 1:End Sub
Sub r1b_Hit:vpmTimer.PulseSw 1:End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 40:PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, -0.05, 0.15 :End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 39:PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, 0, 0.15:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 38:PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, 0.05, 0.15:End Sub

' Drain holes
Sub Drain_Hit
    PlaySound "fx_drain"
    PuPEvent 803
    bsTrough.AddBall Me
End Sub

'Saucer
Sub sw23_Hit:PlaySound "fx_kicker_enter", 0, 1, -0.01:bsLSaucer.AddBall 0:End Sub
Sub sw24_Hit:PlaySound "fx_kicker_enter", 0, 1, 0.01:bsRSaucer.AddBall 0:End Sub

' Rollovers
Sub sw4_Hit:Controller.Switch(4) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw4_UnHit:Controller.Switch(4) = 0:End Sub

Sub sw26_Hit:Controller.Switch(26) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub

Sub sw25_Hit:Controller.Switch(25) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub

Sub sw3_Hit:Controller.Switch(3) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw3_UnHit:Controller.Switch(3) = 0:End Sub

Sub sw32_Hit:Controller.Switch(32) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub

Sub sw31_Hit:Controller.Switch(31) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):End Sub
Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub

Sub sw5b_Hit:Controller.Switch(5) = 1:PlaySound "fx_sensor", 0, 1, pan(ActiveBall):SetLamp 65, 1:End Sub
Sub sw5b_UnHit:Controller.Switch(5) = 0:SetLamp 65, 0:End Sub

'********************
' GIRA FONDO LOGO
'********************

Dim RotAngle5
RotAngle5 = 0

Sub TimerParaFondoLogo_Timer
    RotAngle5 = (RotAngle5+ 1)MOD 360
    FlasherRING.Rotz = RotAngle5
    End Sub




'********************
' GIRA FONDO vp logo
'********************

Dim RotAngle4
RotAngle4 = 0

Sub TimerVueltaLogo_Timer
    RotAngle4 = (RotAngle4+ 1)MOD 360
    FlasherlogoVP.RotX = RotAngle4
    End Sub



' Targets

Sub sw22_Hit:vpmTimer.PulseSw 22:PlaySound SoundFX("fx_target", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub

Sub sw27_Hit:vpmTimer.PulseSw 27:PlaySound SoundFX("fx_target", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub

Sub sw28_Hit:vpmTimer.PulseSw 28:PlaySound SoundFX("fx_target", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub

Sub sw29_Hit:vpmTimer.PulseSw 29:PlaySound SoundFX("fx_target", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub

Sub sw30_Hit:vpmTimer.PulseSw 30:PlaySound SoundFX("fx_target", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub

Sub sw5_Hit:vpmTimer.PulseSw 5:PlaySound SoundFX("fx_target", DOFDropTargets), 0, 1, pan(ActiveBall):BallGuardActive = True:startBallGuard():End Sub

' Captive Ball
Sub cbRtrigger_Hit:cbR.TrigHit ActiveBall:End Sub
Sub cbRtrigger_UnHit:cbR.TrigHit 0:End Sub
Sub cbRwall_Hit:PlaySound "fx_collide", 0, 1, pan(ActiveBall):cbR.BallHit ActiveBall:End Sub
Sub cbRkicker_Hit:cbR.BallReturn Me:End Sub

'Spinner
Sub sw17_Spin:vpmTimer.PulseSw 17:PlaySound "spinner", 0, 1, -0.01:End Sub

'*********
'Solenoids
'*********

SolCallback(6) = "vpmSolSound ""fx_knocker"","
SolCallback(7) = "bsTrough.SolOut"
SolCallBack(8) = "SolLeftHole"
SolCallback(13) = "SolRightHole"
SolCallback(19) = "vpmNudge.SolGameOn"

Sub SolLeftHole(Enabled)
    If Enabled Then
        bsLSaucer.SolOut 1
        sw23a.IsDropped = 0
        sw23a.TimerEnabled = 1
    End If
End Sub

Sub sw23a_Timer:sw23a.IsDropped = 1:Me.TimerEnabled = 0:End Sub

Sub SolRightHole(Enabled)
    If Enabled Then
        bsRSaucer.SolOut 1
        sw24a.IsDropped = 0
        sw24a.TimerEnabled = 1
    End If
End Sub

Sub sw24a_Timer:sw24a.IsDropped = 1:Me.TimerEnabled = 0:End Sub

'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFFlippers), 0, 1, -0.1, 0.25
        LeftFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFFlippers), 0, 1, -0.1, 0.25
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFFlippers), 0, 1, 0.1, 0.25
        RightFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFFlippers), 0, 1, 0.1, 0.25
        RightFlipper.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.25
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.25
End Sub

dim countr
countr = 0
Cartimer.enabled = 1

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
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0)) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0)) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
    UpdateLeds
    UpdateLamps
    GIUpdate
    RollingUpdate

    If Controller.Lamp(45) Then
        If (keyStarted > 0) Then
            If (gameStarted > 0) Then
                pupevent 804
                gameStarted = 0
                keyStarted = 0
            End If
        Else
            gameStarted = 0
        End If
    Else
        gameStarted = 1
    End If
End Sub

Sub UpdateLamps()
    NFadeL 1, l1
    NFadeL 2, l2
    NFadeL 3, l3
    NFadeL 4, l4
    NFadeL 5, l5
    NFadeL 7, l7
    NFadeL 8, l8
    NFadeL 9, l9
    NFadeLm 10, bumper1a
    NFadeLm 10, bumper1b
    NFadeLm 10, bumper2a
    NFadeL 10, bumper2b
    NFadeL 12, l12
    NFadeL 17, l17
    NFadeL 18, l18
    NFadeL 19, l19
    NFadeL 20, l20
    NFadeL 22, l22
    NFadeL 23, l23
    NFadeL 24, l24
    NFadeL 25, l25
    NFadeLm 26, bumper3a
    NFadeL 26, bumper3b
    NFadeL 28, l28
    NFadeL 33, l33
    NFadeL 34, l34
    NFadeL 35, l35
    NFadeL 36, l36
    NFadeL 37, l37
    NFadeL 38, l38
    NFadeL 39, l39
    NFadeL 40, l40
    NFadeL 41, l41
    NFadeL 42, l42
    NFadeL 43, l43
    NFadeL 44, l44
    NFadeL 49, l49
    NFadeL 50, l50
    NFadeL 52, l52
    NFadeL 53, l53
    NFadeL 54, l54
    NFadeL 55, l55
    NFadeL 57, l57
    NFadeL 58, l58
    NFadeL 59, l59
    NFadeL 60, l60
    NFadeL 65, ltop

    ' backdrop lights
    If VarHidden Then
        NFadeT 11, l11, "SAME PLAYER SHOOTS AGAIN"
        NFadeT 13, l13, "BALL IN PLAY"
        NFadeT 14, l14, "1"
        NFadeL 15, l15
        NFadeT 27, l27, "MATCH"
        NFadeT 29, l29, "HI SCORE TO DATE"
        NFadeT 45, l45, "GAME OVER"
        NFadeT 30, l30, "2"
        NFadeL 31, l31
        NFadeT 46, l46, "3"
        NFadeL 47, l47
        NFadeT 61, l61, "TILT"
        NFadeT 62, l62, "4"
        NFadeL 63, l63
    End If
'not used
' 6, 16, 21, 32, 48, 51, 56
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

Sub GiEffect
    For each x in aGiLights
        x.Duration 2, 2000, 1
    Next
End Sub

Sub GIUpdate
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 0 Then 'in this table we increase the value from -1 to 0 since there is a captive ball.
            GiOff
        Else
            GiOn
        End If
    End If
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

Sub aMetals_Hit(idx):PlaySound "fx_MetalHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_postrubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub

'***********************
' Idols follow the ball
'***********************
' they do not move during multiball

Sub aIdolPos_Hit(idx)
    If NOT bMultiBallMode Then
        Idol1.Rotz = -(idx * 2) -32
        Idol2.RotZ = idx * 2 + 32
    End If
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

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 5 ' total number of balls
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
    ' there is one captive ball in this table but we ignore it since we want to play the rolling sound when it moves.
    If UBound(BOT) = -1 Then Exit Sub

    ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)

    'Rotate the Guns
    Idol1.Rotz = -120 + (BOT(b).Y)\15
    Idol2.Rotz = 120 - (BOT(b).Y)\15

        If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0
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

'Bally Lost World
'added by Inkochnito
Sub editDips
    Dim vpmDips:Set vpmDips = New cvpmDips
    With vpmDips
        .AddForm 700, 400, "Lost World - DIP switches"
        .AddChk 2, 10, 180, Array("Match feature", &H00100000)                                                                                                                                                                                                           'dip 21
        .AddChk 205, 10, 115, Array("Credits display", &H00080000)                                                                                                                                                                                                       'dip 20
        .AddFrame 2, 30, 190, "Maximum credits", &H00070000, Array("5 credits", 0, "10 credits", &H00010000, "15 credits", &H00020000, "20 credits", &H00030000, "25 credits", &H00040000, "30 credits", &H00050000, "35 credits", &H00060000, "40 credits", &H00070000) 'dip 17&18&19
        .AddFrame 2, 165, 190, "Sound features", &H80000080, Array("chime effects", &H80000000, "chime and tunes", 0, "noise", &H00000080, "noises and tunes", &H80000080)                                                                                               'dip 8&32
        .AddFrame 2, 246, 190, "High game to date", &H00000060, Array("no award", 0, "1 credit", &H00000020, "2 credits", &H00000040, "3 credits", &H00000060)                                                                                                           'dip 6&7
        .AddFrame 205, 30, 190, "Balls per game", 32768, Array("3 balls", 0, "5 balls", 32768)                                                                                                                                                                           'dip 16
        .AddFrame 205, 76, 190, "Saucer special", &H00200000, Array("one special only", 0, "special lite stays on", &H00200000)                                                                                                                                          'dip 22
        .AddFrame 205, 122, 190, "Saucer 1,000 lite", &H00400000, Array("starts with nothing lit", 0, "starts with 1,000 at saucer", &H00400000)                                                                                                                         'dip 23
        .AddFrame 205, 168, 190, "Outlane special adjust", &H00800000, Array("alternates from side to side", 0, "both lanes lite for special", &H00800000)                                                                                                               'dip 24
        .AddFrame 205, 214, 190, "Flipper feeder lane adjust", &H10000000, Array("alternates from side to side", 0, "both advance saucer value", &H10000000)                                                                                                             'dip 29
        .AddFrame 205, 260, 190, "High score feature", &H00006000, Array("no award", 0, "extra ball", &H00004000, "replay", &H00006000)                                                                                                                                  'dip 14&15
        .AddLabel 50, 340, 300, 20, "After hitting OK, press F3 to reset game with new settings."
        .ViewDips
    End With
End Sub
Set vpmShowDips = GetRef("editDips")


'*********** Glowball Section ***************
Dim GlowBall, CustomBulbIntensity(10)
Dim  GBred(10)
Dim GBgreen(10), GBblue(10)
Dim CustomBallImage(10), CustomBallLogoMode(10), CustomBallDecal(10), CustomBallGlow(10)


' default Ball
CustomBallGlow(0) = 		False
CustomBallImage(0) = 		"JpBall_Chrome_timeShock"
CustomBallLogoMode(0) = 	False
CustomBallDecal(0) = 		"JPBall-Scratches"
CustomBulbIntensity(0) = 	0.01
GBred(0) = 0 : GBgreen(0)	= 0 : GBblue(0) = 0

' Purple GlowBall
CustomBallGlow(1) = 		True
CustomBallImage(1) = 		"glowball purple"
CustomBallLogoMode(1) = 	True
CustomBallDecal(1) = 		""
CustomBulbIntensity(1) = 	0
GBred(1) = 255 : GBgreen(1)	= 0 : GBblue(1) = 255


' green GlowBall
CustomBallGlow(6) = 		True
CustomBallImage(6) = 		"glowball green"
CustomBallLogoMode(6) = 	True
CustomBallDecal(6) = 		""
CustomBulbIntensity(6) = 	0
GBred(6) = 100 : GBgreen(6)	= 255 : GBblue(6) = 100

' blue GlowBall
CustomBallGlow(7) = 		True
CustomBallImage(7) = 		"glowball blue"
CustomBallLogoMode(7) = 	True
CustomBallDecal(7) = 		""
CustomBulbIntensity(7) = 	0
GBred(7) = 50 : GBgreen(7)	= 50 : GBblue(7) = 255
'GBred(7) = 100 : GBgreen(7)	= 100 : GBblue(7) = 255

' red GlowBall
CustomBallGlow(8) = 		True
CustomBallImage(8) = 		"glowball orange"
CustomBallLogoMode(8) = 	True
CustomBallDecal(8) = 		""
CustomBulbIntensity(8) = 	0
GBred(8) = 255 : GBgreen(8)	= 0 : GBblue(8) = 000
'GBred(8) = 255 : GBgreen(8)	= 255 : GBblue(8) = 100  'orange


' *** prepare the variable with references to three lights for glow ball ***
Dim Glowing(10)
Set Glowing(0) = Glowball1 : Set Glowing(1) = Glowball2 : Set Glowing(2) = Glowball3 : Set Glowing(3) = Glowball4


'*** change ball appearance ***

Sub ChangeBall(ballnr)
	Dim BOT, ii, col
	table1.BallDecalMode = CustomBallLogoMode(ballnr)
	table1.BallFrontDecal = CustomBallDecal(ballnr)
	table1.DefaultBulbIntensityScale = CustomBulbIntensity(ballnr)
	table1.BallImage = CustomBallImage(ballnr)
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 3
		col = RGB(GBred(ballnr), GBgreen(ballnr), GBblue(ballnr))
		Glowing(ii).color = col : Glowing(ii).colorfull = col 
	Next
End Sub






' *** Ball Shadow code / Glow Ball code / Primitive Flipper Update ***

Dim BallShadowArray
BallShadowArray = Array (BallShadow1, BallShadow2, BallShadow3)
Const anglecompensate = 15

Sub GraphicsTimer_Timer()
	Dim BOT, b
    BOT = GetBalls

	' switch off glowlight for removed Balls
	IF GlowBall Then
		For b = UBound(BOT) + 1 to 3
			If GlowBall and Glowing(b).state = 1 Then Glowing(b).state = 0 End If
		Next
	End If

    For b = 0 to UBound(BOT)
		' *** move ball shadow for max 3 balls ***
'		If BallShadow and b < 3 Then
'			If BOT(b).X < table1.Width/2 Then
'				BallShadowArray(b).X = ((BOT(b).X) - (50/6) + ((BOT(b).X - (table1.Width/2))/7)) + 10
'			Else
'				BallShadowArray(b).X = ((BOT(b).X) + (50/6) + ((BOT(b).X - (table1.Width/2))/7)) - 10
'			End If
'			BallShadowArray(b).Y = BOT(b).Y + 20 : BallShadowArray(b).Z = 1
'			If BOT(b).Z > 20 Then BallShadowArray(b).visible = 1 Else BallShadowArray(b).visible = 0 End If
'		End If
		' *** move glowball light for max 3 balls ***
		If GlowBall and b < 4 Then
			If Glowing(b).state = 0 Then Glowing(b).state = 1 end if
			Glowing(b).BulbHaloHeight = BOT(b).z + 51
			Glowing(b).x = BOT(b).x : Glowing(b).y = BOT(b).y + anglecompensate
			
			Glowing(b).falloff=200 'GlowBlob Auroa radius
			Glowing(b).intensity=10 'Glowblob intensity
		End If
	Next
End Sub

Sub Glowball_Init
	ChangeBall(ChooseBall)
	If GlowBall Then GraphicsTimer.enabled = True End If
End Sub





Sub table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop : End Sub

Sub Cartimer_Timer
countr = countr + 1 : If Countr > 5 then Countr = 1 : end If
select case countr
case 1 : Car001.z=55:Car002.z=-200:Car003.z=-200:Car004.z=-200:Car005.z=-200
case 2 : Car001.z=-200:Car002.z=55:Car003.z=-200:Car004.z=-200:Car005.z=-200
case 3 : Car001.z=-200:Car002.z=-200:Car003.z=55:Car004.z=-200:Car005.z=-200
case 4 : Car001.z=-200:Car002.z=-200:Car003.z=-200:Car004.z=55:Car005.z=-200
case 5 : Car001.z=-200:Car002.z=-200:Car003.z=-200:Car004.z=-200:Car005.z=55
end Select
End Sub 