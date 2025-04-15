Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim Ballsize,BallMass
Ballsize = 50
BallMass = (Ballsize^3)/125000

Dim DesktopMode:DesktopMode = LastActionHero.ShowDT
Dim UseVPMColoredDMD


Dim CraneLenght, CraneSteps, CraneAnimationSteps
CraneLenght = 2880	'vpmMechFast
CraneSteps = 137
CraneAnimationSteps = 0.4166666666667*CraneLenght 'vpmMechFast


if LastActionHero.ShowDT Then
   vidrio.visible = 1
   leftrail.visible = 1
   rightrail1.visible = 1
   backrail.visible = 1
 Else
   vidrio.visible = 0
   leftrail.visible = 0
   rightrail1.visible = 0
   backrail.visible = 0
End If


UseVPMColoredDMD = DesktopMode

LoadVPM "01000200", "DE.VBS", 3.46

'********************
'Standard definitions
'********************

Const UseSolenoids = 1
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = "fx_Solenoidon"
Const SSolenoidOff = "fx_solenoidoff"
Const SCoin = "fx_Coin"


'Solenoids
SolCallback(1)  = "SolTrough"						'1L - 6 Ball Assy Lockout
SolCallback(2)  = "SolRelease"						'2L - Ball Release
SolCallback(3)  = "SolAutoPlungerIM"				'3L - Ball Launch
SolCallback(4)  = "SolCraneLock"					'4L - Crane Lock
SolCallback(5)  = "bsVuk.SolOut"					'5L - VUK
SolCallback(6)  = "bsRScoop.SolOut"					'6L - Right Scoop
SolCallback(7)  = "SolLockRelease"					'7L - Ball Lock
SolCallback(8)  = "SolKnocker" 						'8L - Knocker
SolCallback(9)  = "SetLamp 99,"						'09 - Flash Crane(x2) + Backbox Inserts(x2)
'SolCallback(10)									'10 - L/R Relay
SolCallback(11) = "SolGI" 							'11 - GI Relay
SolCallback(12) = "SolDiv"							'12 - Diverter
SolCallback(13) = "SoLDrop"							'13 - Drop Target
'SolCallback(14)									'14 - Crane Motor
SolCallback(15) = "bsMLScoop.SolOut"				'15 - Middle Scoop
'SolCallback(16)									'16 - Shaker Motor
'SolCallback(17)									'17 - Left Bumper
'SolCallback(18)									'18 - Center bumper
'SolCallback(19)									'19 - Right bumper
'SolCallback(20)									'20 - Left Slinghshot
'SolCallback(21)									'21 - Right Slingshot
SolCallback(22) = "SolRipperKick"					'22 - Ripper Kickback

'SolCallBack(25)									'1R - Flash Topper Right Police(x4)
SolCallBack(26) = "setlamp 92,"						'2R - Flash Playfield Upper Left(x3) + Backbox Insert
SolCallback(27) = "SetLamp 93,"						'3R - Flash Ramp(x2) + Backbox Insert(x2)
SolCallback(28) = "SetLamp 94,"						'4R - Flash Playfield Upper Right(x2) + Backbox Insert(x2)
SolCallback(29) = "SetLamp 95,"						'5R - Flash Playfield Mid Right(x3) + Backbox Insert
SolCallBack(30) = "setlamp 96," 					'6R - Flash Playfield Low Right(x4)
'SolCallBack(31)									'7R - Flash Topper Left Police(x4)
SolCallBack(32) = "setlamp 97,"     				'8R - Flash Playfield Low Left + Backbox Insert(x2)

SolCallback(37)	= "SolLeftMagnet"					'Left Magnet
SolCallback(38)	= "SolCenterMagnet"					'Center Magnet
SolCallback(39)	= "SolRightMagnet"					'Right Magnet


'************
' Table init.
'************

Const cGameName = "lah_112"

Dim plungerIM, bsTrough, mCrane, bsRScoop, bsVuk, bsMLScoop, dtLDrop, LMAG, RMAG, CMAG

Sub LastActionHero_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Last Action Hero" & vbNewLine & "VPX table by Javier v1.0.0"
        .Games(cGameName).Settings.Value("rol") = 0
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 0
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 1
	vpmNudge.Sensitivity = 2 
    vpmNudge.tiltobj = Array(LeftSlingShot,RightSlingShot,Bumper1b,Bumper2b,Bumper3b)

    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

	'Drain & BallRelease
    Set bsTrough = New cvpmTrough
    With bsTrough
       .Size = 6
       .InitSwitches Array(14, 13, 12, 11, 10, 9)
       .Initexit BallTrough, 90, 5
       .Balls = 6
       .InitExitSounds SoundFX("fx_trough",DOFContactors), SoundFX("fx_trough",DOFContactors)
    End With


    ' Impulse Plunger
    Const IMPowerSetting = 33 'Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .switch 16
        .InitExitSnd SoundFX("fx_plunger",DOFContactors), SoundFX("fx_plunger",DOFContactors)
        .CreateEvents "plungerIM"
    End With


    'CaptiveBalls
    CapKicker1.CreateSizedBallWithMass Ballsize/2,BallMass
    CapKicker1.kick 0,1
    CapKicker1.enabled= 0

    CapKicker2.CreateSizedBallWithMass Ballsize/2,BallMass
    CapKicker2.kick 0,1
    CapKicker2.enabled= 0

     ' Crane
     set mCrane = new cvpmMech
     with mCrane
         .MType = vpmMechOneSol + vpmMechReverse + vpmMechNonlinear + vpmMechFast
         .Sol1 = 14
         .Length = CraneLenght
         .Steps = CraneSteps
		 .addsw 60,0,2
		 .addsw 61,135,138
         .Callback = GetRef("UpdateCrane")
         .Start
     End with

     'Middle Scoop Right
     Set bsRScoop =new cvpmTrough
     With  bsRScoop
        .size = 4
        .initSwitches Array(31)
        .Initexit Sw31, 220, 24
        .InitExitSounds SoundFX("fx_Solenoid",DOFContactors), SoundFX("fx_popper",DOFContactors)
        .InitExitVariance 2, 2
        .MaxBallsPerKick = 1
      End With

    ' Right Vuk
    Set bsVuk = New cvpmBallStack
    With bsVuk
        .InitSaucer sw47, 47, 176, 35
        .KickZ = 1.48
        .InitExitSnd "fx_vukout_LAH", SoundFX("fx_Solenoid",DOFContactors)
    End With

     'Middle Scoop Left
    Set bsMLScoop = New cvpmTrough
    With bsMLScoop
        .size = 4
        .initSwitches Array(57)
        .Initexit sw57, 172, 24
        .InitExitSounds SoundFX("fx_Solenoid",DOFContactors), SoundFX("fx_popper",DOFContactors)
        .InitExitVariance 2, 2
        .MaxBallsPerKick = 1
    End With

  	Set dtLDrop=New cvpmDropTarget
    With dtLDrop
	    .InitDrop Array(sw17,sw18,sw19,sw20,sw21), Array(17,18,19,20,21)
	    .InitSnd SoundFX("fx_droptarget",DOFContactors),SoundFX("fx_resetdrop",DOFContactors)
    End With

    ' Left Magnet
    Set LMAG = New cvpmMagnet
    With LMAG
        .InitMagnet LMagnet, 11
        .CreateEvents "LMAG"
    End With

    ' Right Magnet
    Set RMAG = New cvpmMagnet
    With RMAG
        .InitMagnet RMagnet, 11
        .CreateEvents "RMAG"
    End With

    ' Center Magnet
    Set CMAG = New cvpmMagnet
    With CMAG
        .InitMagnet CMagnet, 11
        .CreateEvents "CMAG"
    End With

SolGi 0

End Sub

'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

End Sub

	
Dim BallShadow
BallShadow = Array(BallShadow001, BallShadow002, BallShadow003, BallShadow004, BallShadow005, BallShadow006, BallShadow007, BallShadow008)
Const shift = 27


'*****************
'Trough,Drain and Release
'*****************

Sub SolTrough(Enabled)
  If enabled Then
   bsTrough.ExitSol_On
   Controller.Switch(15) = 1
  End If
End Sub

Sub SolRelease(Enabled)
  If enabled Then
    BallRelease.kick 90, 8
    Controller.Switch(15) = 0
    Playsound SoundFX("fx_ballrel",DOFContactors)
  End If
End Sub

Sub Drain_Hit():PlaySound "fx_drain": BsTrough.AddBall Me:End Sub

'*****************
'Diverter
'*****************

Sub SolDiv(Enabled)
  If Enabled Then
      DivOpen.IsDropped = 1
      DivClose.IsDropped = 0
      DiverterP.RotY = 15
      PlaySound SoundFX("fx_diverter",DOFContactors)
    Else
      DivClose.IsDropped = 1
      DivOpen.IsDropped = 0
      DiverterP.RotY = 60
      PlaySound SoundFX("fx_diverter",DOFContactors)
  End If
End Sub

'*****************
'LockPost
'*****************
Sub SolLockRelease(enabled)
	If enabled and IgnorePostSol7 = 0 then
        PlaySound SoundFX("fx_diverter",DOFContactors)
		Post.isdropped = true
		LockReleaseTimer.Enabled = True
	End If	
End Sub

Sub LockReleaseTimer_Timer()
      PlaySound SoundFX("fx_diverter",DOFContactors)	
	Post.IsDropped = False
	LockReleaseTimer.Enabled = False
End Sub	

'*****************
'AutoPlunger
'*****************

Sub SolAutoPlungerIM(Enabled)
    If Enabled Then
        PlungerIM.AutoFire
    End If
End Sub

'*****************
'Knocker
'*****************

Sub SolKnocker(enabled)
If enabled then Playsound SoundFX("fx_knocker",DOFKnocker)
End Sub

'*****************
'Ripper Kickback
'*****************

Sub SolRipperKick(enabled)
If enabled then sw48.Kick 185,30
End Sub

'*****************
'Crane Animation
'*****************

Const Pi = 3.1415927
Dim CraneBall

 Sub UpdateCrane(aNewPos,aSpeed,aLastPos)
	StopSound"motor": PlaySound"motor"
	CraneTimer.Enabled = 1
 End Sub

Sub CraneTimer_Timer()
	If mCrane.position > gruap.RotY Then
		gruap.RotY = gruap.RotY +(cranesteps/CraneAnimationSteps)/5.1
		PinP.RotY = gruap.RotY
	End if
	if mCrane.position < gruap.roty Then
		gruap.roty = gruap.roty -(cranesteps/CraneAnimationSteps)/5.1
        PinP.RotY = gruap.RotY
	End if
	If Not IsEmpty(CraneBall) Then
		CraneBall.X=Sin((212-GruaP.RotY)*Pi/180)*80 + GruaP.x
		CraneBall.Y=Cos((32-GruaP.RotY)*Pi/180)*80 + GruaP.y
		CraneBall.Z=237
	End If
	If gruap.roty => 131 Then KickerRamp.Enabled = 1:Ramp_Grua.collidable=1
	If gruap.roty =< 130 Then KickerRamp.Enabled = 0:Ramp_Grua.collidable=0
End Sub

Sub KickerRamp_Hit
	PinP.Z = 222
	Me.Kick 250,10
	PlaySound SoundFX("fx_diverter",DOFContactors)
 End Sub

Sub KickerBolaIn_Hit
    Me.DestroyBall
	Set CraneBall = KickerBolaIn.CreateSizedBallWithMass (Ballsize/2,BallMass)
	PinP.Z = 235
	PlaySound SoundFX("fx_diverter",DOFContactors)
End Sub

Sub SolCraneLock(Enabled)
If NOT Enabled AND Controller.Switch(60) Then
	If Not IsEmpty(CraneBall) Then
		PinP.Z = 222
		PlaySound SoundFX("fx_diverter",DOFContactors)
	    KickerBolaIn.Kick 210,2
		CraneBall = Empty
	End If
End If
End Sub

Sub ExitCrane_UnHit:  PinP.Z = 235 : PlaySound SoundFX("fx_diverter",DOFContactors): End Sub

'*****************
'Drop Target
'*****************
Sub SolDrop(enabled)
	if enabled then
	   dtLDrop.DropSol_On
	end if
End Sub

'*****************
'	Magnets
'*****************

Sub SolCenterMagnet(enabled)
	If enabled Then
		CMAG.MagnetOn = 1
		PlaySound SoundFX("fx_magnet",DOFShaker)
	Else
		CMAG.MagnetOn = 0
	End If
End Sub

Sub SolLeftMagnet(enabled)
	If enabled Then
		LMAG.MagnetOn = 1
		PlaySound SoundFX("fx_magnet",DOFShaker)
	Else
		LMAG.MagnetOn = 0
	End If
End Sub

Sub SolRightMagnet(enabled)
	If enabled Then
		RMAG.MagnetOn = 1
		PlaySound SoundFX("fx_magnet",DOFShaker)
	Else
		RMAG.MagnetOn = 0
	End If
End Sub

'******************
'Keys Up and Down
'*****************

Sub LastActionHero_KeyDown(ByVal Keycode)
    If keycode = plungerkey then controller.switch(8)=1 :controller.switch(62)=1
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub LastActionHero_KeyUp(ByVal Keycode)
    If keycode = plungerkey then controller.switch(8)=0 :controller.switch(62)=0
    If vpmKeyUp(keycode) Then Exit Sub
End Sub


'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)

    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFContactors), 0, 1, -0.1, 0.15
        LeftFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFContactors), 0, 1, -0.1, 0.15
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)

    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFContactors), 0, 1, 0.1, 0.15
        RightFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFContactors), 0, 1, 0.1, 0.15
        RightFlipper.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.15
End Sub

Sub Rightflipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.15
End Sub


' *********
' Switches
' *********

' Slings & div switches

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("fx_slingshot",DOFContactors), 0, 1, -0.05, 0.05
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 36
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
    vpmTimer.PulseSw 37
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
Sub Bumper1b_Hit:vpmTimer.PulseSw 33:PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0.15, 0.15:End Sub

Sub Bumper2b_Hit:vpmTimer.PulseSw 35:PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0.15, 0.15:End Sub

Sub Bumper3b_Hit:vpmTimer.PulseSw 34:PlaySound SoundFX("fx_bumper",DOFContactors), 0, 1, 0.15, 0.15:End Sub



' Targets 
Sub Sw17_Hit:dtLDrop.Hit 1:End Sub
Sub Sw18_Hit:dtLDrop.Hit 2:End Sub
Sub Sw19_Hit:dtLDrop.Hit 3:End Sub
Sub Sw20_Hit:dtLDrop.Hit 4:End Sub
Sub Sw21_Hit:dtLDrop.Hit 5:End Sub


Dim IgnorePostSol7
Sub Sw22_Hit: Controller.Switch(22) = 1:PostDebounceTimer.Enabled = 1:IgnorePostSol7=1:End Sub
Sub Sw22_UnHit: Controller.Switch(22) = 0:End Sub
Sub Sw23_Hit: Controller.Switch(23) = 1:PostDebounceTimer.Enabled = 1:IgnorePostSol7=1:End Sub
Sub Sw23_UnHit: Controller.Switch(23) = 0:End Sub
Sub PostDebounceTimer_Timer
	IgnorePostSol7 = 0
	PostDebounceTimer.Enabled = 0
End Sub

Sub Sw24_Hit():Playsound "fx_sensor":Controller.Switch(24)=1: End Sub
Sub Sw24_UnHit():Controller.Switch(24)=0: End Sub

Sub sw25_Hit:vpmTimer.PulseSw 25:PlaySound SoundFX("fx_target",DOFContactors):sw25p.transY = -10:Me.TimerEnabled = 1:End Sub
Sub sw25_Timer:sw25p.transY = 0:Me.TimerEnabled = 0:End Sub 
Sub sw26_Hit:vpmTimer.PulseSw 26:PlaySound SoundFX("fx_target",DOFContactors):sw26p.transY = -10:Me.TimerEnabled = 1:End Sub
Sub sw26_Timer:sw26p.transY = 0:Me.TimerEnabled = 0:End Sub 
Sub sw27_Hit:vpmTimer.PulseSw 27:PlaySound SoundFX("fx_target",DOFContactors):sw27p.transY = -10:Me.TimerEnabled = 1:End Sub
Sub sw27_Timer:sw27p.transY = 0:Me.TimerEnabled = 0:End Sub 

Sub Sw28_Hit():Playsound "fx_sensor":Controller.Switch(28)=1: End Sub
Sub Sw28_UnHit():Controller.Switch(28)=0: End Sub
Sub Sw29_Hit():Playsound "fx_sensor":Controller.Switch(29)=1: End Sub
Sub Sw29_UnHit():Controller.Switch(29)=0: End Sub
Sub Sw30_Hit():Playsound "fx_sensor":Controller.Switch(30)=1: End Sub
Sub Sw30_UnHit():Controller.Switch(30)=0: End Sub
Sub Sw32_Hit:vpmTimer.PulseSw 32:PlaySound SoundFX("fx_target",DOFContactors):End Sub

Sub Sw38_Hit():Playsound "fx_sensor":Controller.Switch(38)=1: End Sub
Sub Sw38_UnHit():Controller.Switch(38)=0: End Sub

Sub Sw39_Hit():Playsound "fx_sensor":Controller.Switch(39)=1: End Sub
Sub Sw39_UnHit():Controller.Switch(39)=0: End Sub

Sub Sw40_Hit:vpmTimer.PulseSw 40:PlaySound SoundFX("fx_target",DOFContactors):End sub

Sub Sw41_Hit():Playsound "fx_sensor":Controller.Switch(41)=1: End Sub
Sub Sw41_UnHit():Controller.Switch(41)=0: End Sub
Sub Sw42_Hit():Playsound "fx_sensor":Controller.Switch(42)=1: End Sub
Sub Sw42_UnHit():Controller.Switch(42)=0: End Sub
Sub Sw43_Hit():Playsound "fx_sensor":Controller.Switch(43)=1: End Sub
Sub Sw43_UnHit():Controller.Switch(43)=0: End Sub
Sub Sw44_Hit():Playsound "fx_sensor":Controller.Switch(44)=1: End Sub
Sub Sw44_UnHit():Controller.Switch(44)=0: End Sub

Sub Sw45_Hit():Playsound "fx_sensor":Controller.Switch(45)=1 :End Sub
Sub Sw45_UnHit():Controller.Switch(45)=0: End Sub

Sub Sw46_Hit()Controller.Switch(46)=1:PlaySound "Gate" End Sub
Sub Sw46_UnHit()Controller.Switch(46)= 0 End Sub

'Right Up Vuk
Sub Sw47_hit:PlaySound "fx_kicker":bsVuk.AddBall 0:End Sub

Sub Sw48_Hit:vpmTimer.PulseSw 48 : PlaySound "bumper_retro":End Sub

Sub Sw54_Hit:vpmTimer.PulseSw 54:PlaySound "fx_chapa":End Sub

Sub Sw56_Hit:vpmTimer.PulseSw 56:PlaySound SoundFX("fx_target",DOFContactors):End Sub

'Spinner
Sub Sw59_Spin:vpmTimer.PulseSw 59:PlaySound "fx_gate" : End sub

'Holes Animation

Dim cball,cball1,cball2

Sub sw31_Hit
	PlaySound "fx_hole-enter", 0, 1, pan(ActiveBall)
	Set cBall = ActiveBall
	Me.TimerEnabled = 1
    vpmTimer.AddTimer 200, "bsRScoop.AddBall"
    Me.Enabled = 0
End Sub

Sub sw31_Timer
    Do While cBall.Z > 0
        cBall.Z = cBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub sw57_Hit
	PlaySound "fx_hole-enter", 0, 1, pan(ActiveBall)
	Set cBall1 = ActiveBall
	Me.TimerEnabled = 1
    vpmTimer.AddTimer 300, "bsMLScoop.AddBall"
    Me.Enabled = 0
End Sub

Sub sw57_Timer
    Do While cBall1.Z > 0
        cBall1.Z = cBall1.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub sw58_Hit
	PlaySound "fx_hole-enter", 0, 1, pan(ActiveBall)
	Set cBall2 = ActiveBall
	Me.TimerEnabled = 1
    Me.Enabled = 0
	vpmtimer.pulseswitch 58,700,"bsMLScoop.AddBall"
End Sub

Sub sw58_Timer
    Do While cBall2.Z > 0
        cBall2.Z = cBall2.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub


'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 20 'lamp fading speed
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
    UpdateLamps
End Sub

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0         ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4       ' used to track the fading state
        FlashSpeedUp(x) = 0.5    ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.35 ' slower speed when turning off the flasher
        FlashMax(x) = 1          ' the maximum value when on, usually 1
        FlashMin(x) = 0          ' the minimum value when off, usually 0
        FlashLevel(x) = 0        ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub



Sub UpdateLamps
	nFadeL 1, Light1
	nFadeL 2, Light2
	nFadeL 3, Light3
	nFadeL 4, Light4
	nFadeL 5, Light5
    NFadeLm 6, l6
    nFadeLm 6, l6a
    NFadeLm 7, l7
    nFadeLm 7, l7a
    NFadeLm 8, l8
    nFadeLm 8, l8a
	nFadeL 9, Light9
	nFadeL 10, Light10
	nFadeL 11, Light11
	nFadeL 12, Light12
	nFadeL 13, Light13
	nFadeL 14, Light14
	nFadeL 15, Light15
	nFadeL 16, Light16
	nFadeL 17, Light17
	nFadeL 18, Light18
	nFadeL 19, Light19
	nFadeL 20, Light20
	nFadeL 21, Light21
	nFadeL 22, Light22
	Flash 23, Light23
	Flash 23, Light23f
	nFadeL 24, Light24
 	nFadeL 25, Light25
	nFadeL 26, Light26
	nFadeL 27, Light27
	nFadeL 28, Light28
	nFadeL 29, Light29
	nFadeL 30, Light30
	nFadeLm 31, Light31
	NFadeLm 31, Light31b
	nFadeL 32, Light32
 	nFadeL 33, Light33
	nFadeL 34, Light34
	nFadeL 35, Light35
	nFadeL 36, Light36
	nFadeL 37, Light37
	nFadeL 38, Light38
	nFadeL 39, Light39
	NFadeLm 40, Light40
	nFadeLm 40, Light40b
	nFadeL 41, Light41
	nFadeL 42, Light42
	NFadeLm 43, Light43
	nFadeLm 43, Light43b
	nFadeL 44, Light44
	nFadeL 45, Light45
	nFadeL 46, Light46
	nFadeL 47, Light47
	Flash 48, Light48
	nFadeL 49, Light49
	Flash 50, Light50
	Flash 51, Light51
	Flash 52, Light52
	Flash 53, Light53
	Flash 54, Light54
	NFadeL 55, Light55
    Flash 56, Light56
    NFadeLm 57, Light57
	NFadeLm 57, Light57b
	NFadeLm 57, Light57c
	Flash 57, Light57f
    NFadeL 58, Light58
    NFadeL 59, Light59
    NFadeL 60, Light60
    NFadeL 61, Light61
    NFadeL 62, Light62
    NFadeL 63, Light63
'Flashers
	NFadeLm 92, f2
    NFadeLm 92, f2a
    NFadeLm 92, f2b
    NFadeLm 93, f3
    NFadeLm 93, f3a
    NFadeLm 93, f3b
    NFadeLm 93, f3c
	NFadeLm 93, f3e
    Flashm 93, f3d
    Flashm 93, f3r
    Flash 93, f3r1
    NFadeLm 94, f4
    NFadeLm 94, f4a
    NFadeLm 94, f4b
    NFadeLm 94, f4c
    NFadeLm 94, f4e
    Flashm 94, f4r
    Flash 94, f4r1
    NFadeL 95, f5
    NFadeLm 96, f6
    NFadeLm 96, f6a
    NFadeLm 96, f6b
    NFadeLm 96, f6c
    NFadeLm 96, f6d
    NFadeLm 96, f6e
    Flashm 96, f6f
    Flash 96, f6r
	FadeObj 99, GruaP, "CraneTEXTUREMAP_on", "CraneTEXTUREMAP_a", "CraneTEXTUREMAP_b", "CraneTEXTUREMAPFinal"
	NFadeL 97, f7
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
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(nr)
End Sub

'******************
' G. I.
'******************

Dim x

Sub SolGi(enabled)
	If enabled Then
		For each x in aGiLights: x.State = 0: Next
		gi34.intensityscale=0
		Playsound "fx_relay_off"
		'LastActionHero.ColorGradeImage = "ColorGrade_off"
	Else
		For each x in aGiLights: x.State = 1: Next
		gi34.intensityscale=1
		Playsound "fx_relay_on"
		'LastActionHero.ColorGradeImage = "ColorGrade_on"
	End If
End Sub

'******************
' RealTime Updates
'******************

Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates
    RollingSound()
    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
End Sub
'******************************************************
'* JP's VP10 Supporting Ball & Sound Functions ********
'******************************************************

Function RndNum(min,max)
	RndNum = Int(Rnd()*(max-min+1))+min  			' Sets a random number between min and max
End Function

Function Vol(ball) 									' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)   
End Function

Function Pan(ball) 									' Calculates the pan for a ball based on the X position on the table. 
    Dim tmp											' "table1" is the name of the table
    tmp = ball.x * 2 / LastActionHero.width-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) 								' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 								'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'******************************************************
'* JP's VP10 COLLISION & ROLLING SOUNDS ***************
'******************************************************

Const tnob = 8 										' total number of balls

ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingSound() 
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, (Vol(BOT(b) )) , Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 500, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub
'******************************************************
'* SOUND EFFECTS **************************************
'******************************************************

Sub aGates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub cRubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub


Sub RRampFX_Hit:PlaySound "fx_rampL":End Sub
Sub RRampFX1_Hit:PlaySound "fx_rampR":End Sub
Sub LRampFX_Hit:PlaySound "fx_rampL":End Sub

Sub RailEndTrigger2_Hit
stopsound "fx_metalrolling"
vpmTimer.AddTimer 250, "BallHitSound"
End Sub

Sub RailEndTrigger1_Hit
vpmTimer.AddTimer 250, "BallHitSound"
End Sub

Sub BallHitSound(dummy):PlaySound "fx_ballrampdrop":End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub
