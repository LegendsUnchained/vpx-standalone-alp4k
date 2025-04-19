Option Explicit
Randomize





'Switches:
'1 Plumb Tilt
'3 Credit Button
'4 Right Coin
'5 Center Coin
'6 Left Coin
'7 Tilt Slam
'8 Ticket
'10 Outhole
'11 Trough Left
'12 Trough Center
'13 Trough Right
'14 Shooter Lane
'15 Left Flipper
'16 Right Flipper
'17 Left Outlane
'18 Left Return
'19 Right Outlane
'20 Right Return
'21 Left Sling
'22 Right Sling
'23 Wacker Drive
'25 Ape Lane A
'26 Ape Lane P
'27 Ape Lane E
'28 Ape Lane Ramp
'29 Lock VUK
'30 Missile Kicker?
'31 Loop Left
'32 Loop Right
'33 Tower T
'34 Tower o
'35 Tower W
'36 Tower E
'37 Tower R
'38 Lock Bottom
'39 Lock Center
'40 Lock Top
'41 DT Left
'42 DT Center
'43 DT Right
'45 Radar Eject
'46 Bumper Left
'47 Bumper Center
'48 Bumper Right
'49 Cubs Target
'50 Bears Target
'51 Bulls Target
' 
'Lamps:
'1 King K
'2 King I
'3 King N
'4 King G
'5 Kong K
'6 Kong O
'7 Kong N
'8 Kong G
'9 Tower T
'10 Tower O
'11 Tower W
'12 Towet E
'13 Tower R
'14 Cubs
'15 Bears
'16 Bulls
'17 DT Left
'18 DT Center
'19 DT Right
'20 Not Used
'21 Tower 25000
'22 Tower 50000
'23 Tower 100000
'24 Tower Ex.Ball
'25 Lock Red
'26 Lock Yellow
'27 Lock Green
'28 City Bonus
'29 Ape Ramp 25000
'30 Ape Ramp 50000
'31 Ape Ramp 100000
'32 Million Bananas
'33 2x
'34 3x
'35 4x
'36 5x
'37 Go Ape Again
'38 Eject Ex. Ball
'39 Everything Lit
'40 Jungle Bonus
'41 Loop 25000
'42 Loop 50000
'43 Loop 100000
'44 Loop Ex.Ball
'45 S Replay Arrow
'46 Not Used
'47 Tower 3000000
'48 Special Left
'49 Jackpot 1000000
'50 Jackpot 2000000
'51 Jackpot 3000000
'52 Jackpot 4000000
'53 Jackpot 5000000
'54 Jackpot S Replay
'55 Not Used
'56 Special Right
'57 Missile 25000
'58 Missile 50000
'59 Missile 75000
'60 Missile 100000
'61 Missile 1000000
'62 Ape Lane A
'63 Ape Lane P
'64 Ape Lane E
' 
'Solenoids:
'1 King Shooter
'2 Kong Shooter
'3 Ape Ramp BG Skul
'4 BG DNSRS Shooter
'5 BG Tower 25000
'6 BG Tower 50000
'7 BG Tower 100000
'8 BG Tower Ex. Ball
'9 BG Tower Million
'11 Gen Illm
'12 BG Girl Missile
'13 BG Kong Explod??e
'14 Tower Flasher
'15 Skull Island
'16 Missile Launch
'17 Bumper Left
'18 Bumper Center
'19 Bumper Right
'20 Sling Left
'21 Sling Right
'25 Outhole
'26 Shooter Eject
'27 Lock Release
'28 Radar Eject
'29 Ball Lock
'30 Drop Target
'32 Ticket
'45 Flipper RightU
'46 Flipper Right
'47 Flipper LeftU
'48 Flipper Left

Dim FlipperBatMod, SolidStateSticker, KingFlipperBatMod, KingFlipperSticker, InstructionCards, RubberColor

'''''''''''''''''''''''
'''''' Options'''''''''
'''''''''''''''''''''''





'1=white/black  2=yellow/orange   3=black/dark orange
FlipperBatMod = 2


'0=not visible   1=visible
SolidStateSticker = 0


'1=white/black  2=yellow/orange   3=black/dark orange
KingFlipperBatMod = 2


'0=not visible   1=visible
KingFlipperSticker = 1


'1=normal   2=custom
InstructionCards = 2


'1=white  2=black
RubberColor = 1



Const Ballsize = 50
Const BallMass = 1

Const cGameName="kiko_a10"

if table1.showdt = false then Ramp001.visible = 0: Ramp002.visible = 0


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

LoadVPM "01560000", "de.VBS", 3.10

'Variables
Dim xx
Dim Bump1, Bump2, Bump3
Dim bsTrough
Dim bsMissileKicker
Dim bsRadarEject
Dim vLock
Dim bsVuk

Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 1
Const HandleMech = 0 

'Standard Sounds
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = ""
Const SCoin = "Coin_In"


'Table Init
Sub Table1_Init
	vpmInit Me

	With Controller
		.GameName = cGameName
		.SplashInfoLine = "King Kong, Data East 1990"
		.HandleMechanics = 0
		.HandleKeyboard = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.ShowTitle = 0
		.Hidden = 1
	End With
	On Error Resume Next

	Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0

	'Nudging
	vpmNudge.TiltSwitch=1
	vpmNudge.Sensitivity=1
	vpmNudge.TiltObj=Array(Bumper1b,Bumper2b,Bumper3b,LeftSlingshot,RightSlingshot)

	'**Trough
	Set bsTrough = New cvpmBallStack
	With bsTrough
		.InitSw 0, 13, 12, 11, 0, 0, 0, 0
		.InitKick BallRelease, 90, 4
		.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
		.Balls = 3
	End With

	'**Kicker
	Set bsMissileKicker = New cvpmBallStack
	bsMissileKicker.InitSaucer missilekicker, 30, 90, 65
	bsMissileKicker.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("solenoid",DOFContactors)

	'**RadarEject
	Set bsRadarEject = New cvpmBallStack
	bsRadarEject.InitSaucer radareject, 45, 90, 3
	bsRadarEject.InitExitSnd SoundFX("fx_kicker",DOFContactors), SoundFX("Solenoid",DOFContactors)

	'**Main Timer init
	PinMAMETimer.Enabled = 1

	SetOptions

End Sub

dim speed
speed = 42



'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If KeyCode = keyInsertCoin1 Or KeyCode = keyInsertCoin2 or KeyCode = keyInsertCoin3 Then Controller.Switch(30) = 1
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If vpmKeyDown(KeyCode) Then Exit Sub
	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySoundAt"plungerpull",Plunger
	End If


	'* Test Kicker
	If keycode = 37 Then TestKick ' K
	If keycode = 19 Then return_to_test ' R return ball to kicker
	If keycode = 46 Then create_testball ' C create ball ball in test kicker
	If keycode = 205 Then TKickAngle = TKickAngle + 3:fKickDirection.Visible=1:fKickDirection.RotZ=TKickAngle'+90 ' right arrow
	If keycode = 203 Then TKickAngle = TKickAngle - 3:fKickDirection.Visible=1:fKickDirection.RotZ=TKickAngle'+90 'left arrow
	If keycode = 200 Then TKickPower = TKickPower + 2:debug.print "TKickPower: "&TKickPower ' up arrow
	If keycode = 208 Then TKickPower = TKickPower - 2:debug.print "TKickPower: "&TKickPower ' down arrow



End Sub

Sub table1_KeyUp(ByVal Keycode)
    If KeyCode = keyInsertCoin1 Or KeyCode = keyInsertCoin2 or KeyCode = keyInsertCoin3 Then Controller.Switch(30) = 0
    If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySoundAt"plunger",Plunger
	End If

	'* Test Kicker
	If keycode = 205 Then fKickDirection.Visible=0 ' right arrow
	If keycode = 203 Then fKickDirection.Visible=0 'left arrow


End Sub


Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_exit():Controller.Stop:	End sub

'*****************************************
' Dropwall Init
''*****************************************


'''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''  Mods
'''''''''''''''''''''''''''''''''''''''''''''''''''''''

Dim xxRubbers

Sub SetOptions()

	If FlipperBatMod = 1 Then
		batright.image = "Flipperbat WhiteBlack"
		batleft.image = "Flipperbat WhiteBlack"
	End If

	If FlipperBatMod = 2 Then
		batright.image = "Flipperbat OrangeYellow"
		batleft.image = "Flipperbat OrangeYellow"
	End If

	If FlipperBatMod = 3 Then
		batright.image = "Flipperbat DarkOrangeBlack"
		batleft.image = "Flipperbat DarkOrangeBlack"
	End If


	If SolidStateSticker = 0 Then
		pLSS.visible = false
		pRSS.visible = false
	Else
		pLSS.visible = True
		pRSS.visible = True
	End If

	If KingFlipperBatMod = 1 Then
		batright1.image = "Flipperbat WhiteBlack"
	End If

	If KingFlipperBatMod = 2 Then
		batright1.image = "Flipperbat OrangeYellow"
	End If

	If KingFlipperBatMod = 3 Then
		batright1.image = "Flipperbat DarkOrangeBlack"
	End If

	If KingFlipperSticker= 0 Then
		batright2.visible = false
	Else
		batright2.visible = True
	End If

	If InstructionCards = 1 Then
		IC_Right.image = "DE_IC_3ball"
		IC_Left.image = "KK_IC_Left"
	End If

	If InstructionCards = 2 Then
		IC_Right.image = "KK_IC_Right2"
		IC_Left.image = "KK_IC_Left2"
	End If

	If RubberColor = 1 Then
		for each xxRubbers in allRubbers
			xxRubbers.material = "Rubber White"
			next
	End If

	If RubberColor = 2 Then
		for each xxRubbers in allRubbers
			xxRubbers.material = "Rubber Black"
			next
	End If

	If RubberColor = 3 Then
		for each xxRubbers in allRubbers
			xxRubbers.material = "Rubber Yellow"
			next
	End If

End Sub

'******************************************************
'					Test Kicker
'******************************************************

Dim TKickAngle, TKickPower, TKickBall
TKickAngle = 90
TKickPower = 65

Sub testkick()
	missilekicker.kick TKickAngle,TKickPower
End Sub

Sub create_testball():Set TKickBall = missilekicker.CreateBall:End Sub
Sub test1_hit():Set TKickBall=ActiveBall:End Sub
Sub return_to_test():TKickBall.velx=0:TKickBall.vely=0:TKickBall.x=missilekicker.x:TKickBall.y=missilekicker.y-50:missilekicker.timerenabled=0:End Sub



'*******************************
'' Rollovers
''*******************************
Sub sw14_Hit	: Controller.Switch(14) = 1: PlaySoundAt "rollover", sw14: End Sub 'Shooter Lane
Sub sw14_UnHit	: Controller.Switch(14) = 0: End Sub
Sub sw17_Hit:     Controller.Switch(17) = 1: PlaySoundAt "rollover", sw17: End Sub  'Left Outlane
Sub sw17_UnHit:   Controller.Switch(17) = 0: End Sub
Sub sw18_Hit:     Controller.Switch(18) = 1: PlaySoundAt "rollover", sw18: End Sub  'Left Return
Sub sw18_UnHit:   Controller.Switch(18) = 0: End Sub
Sub sw19_Hit:     Controller.Switch(19) = 1: PlaySoundAt "rollover", sw19: End Sub  'Right Outlane
Sub sw19_UnHit:   Controller.Switch(19) = 0: End Sub
Sub sw20_Hit:     Controller.Switch(20) = 1: PlaySoundAt "rollover", sw20: End Sub  'Right Return
Sub sw20_UnHit:   Controller.Switch(20) = 0: End Sub
Sub sw23_Hit:     Controller.Switch(23) = 1: PlaySoundAt "rollover", sw23: End Sub  'Right Return
Sub sw23_UnHit:   Controller.Switch(23) = 0: End Sub
Sub sw25_Hit   : Controller.Switch(25) = 1: PlaySoundAt "rollover", sw25: End Sub 'A
Sub sw25_UnHit : Controller.Switch(25) = 0: End Sub
Sub sw26_Hit   : Controller.Switch(26) = 1: PlaySoundAt "rollover", sw26: End Sub	'P
Sub sw26_UnHit : Controller.Switch(26) = 0: End Sub
Sub sw27_Hit   : Controller.Switch(27) = 1: PlaySoundAt "rollover", sw27: End Sub	'E
Sub sw27_UnHit : Controller.Switch(27) = 0: End Sub
Sub sw31_Hit:   Controller.Switch(31) = 1: PlaySoundAt "rollover", sw31: End Sub  'Loop Left
Sub sw31_UnHit: Controller.Switch(31) = 0: End Sub
Sub sw32_Hit:   Controller.Switch(32) = 1: PlaySoundAt "rollover", sw32: End Sub  'Loop Right
Sub sw32_UnHit: Controller.Switch(32) = 0: End Sub



Sub Gate3_Hit:Controller.Switch(28) = 1::End Sub  		'APE lane ramp (using gate instead of switch)
Sub Gate3_UnHit:Controller.Switch(28) = 0:End Sub

''Targets
Sub sw41_Hit:   PrimStandupTgtHit   41, Sw41, PrimSw41:PlaySoundAt SoundFX("target",DOFTargets),PrimSw41: End Sub 'multi-ball standups
Sub sw41_Timer: PrimStandupTgtMove  41, Sw41, PrimSw41: End Sub
Sub sw42_Hit:   PrimStandupTgtHit   42, Sw42, PrimSw42:PlaySoundAt SoundFX("target",DOFTargets),PrimSw42: End Sub
Sub sw42_Timer: PrimStandupTgtMove  42, Sw42, PrimSw42: End Sub
Sub sw43_Hit:   PrimStandupTgtHit   43, Sw43, PrimSw43:PlaySoundAt SoundFX("target",DOFTargets),PrimSw43: End Sub
Sub sw43_Timer: PrimStandupTgtMove  43, Sw43, PrimSw43: End Sub


''Cubs, Bears, Bulls
Sub sw49_Hit:   PrimStandupTgtHit   49, Sw49, PrimSw49:PlaySoundAt SoundFX("target",DOFTargets),PrimSw49: End Sub
Sub sw49_Timer: PrimStandupTgtMove  49, Sw49, PrimSw49: End Sub
Sub sw50_Hit:   PrimStandupTgtHit   50, Sw50, PrimSw50:PlaySoundAt SoundFX("target",DOFTargets),PrimSw50: End Sub
Sub sw50_Timer: PrimStandupTgtMove  50, Sw50, PrimSw50: End Sub
Sub sw51_Hit:   PrimStandupTgtHit   51, Sw51, PrimSw51:PlaySoundAt SoundFX("target",DOFTargets),PrimSw51: End Sub
Sub sw51_Timer: PrimStandupTgtMove  51, Sw51, PrimSw51: End Sub
'
''Tower
Sub sw33_Hit:   PrimStandupTgtHit   33, Sw33, PrimSw33:PlaySoundAt SoundFX("target",DOFTargets),PrimSw33: End Sub
Sub sw33_Timer: PrimStandupTgtMove  33, Sw33, PrimSw33: End Sub
Sub sw34_Hit:   PrimStandupTgtHit   34, Sw34, PrimSw34:PlaySoundAt SoundFX("target",DOFTargets),PrimSw34: End Sub
Sub sw34_Timer: PrimStandupTgtMove  34, Sw34, PrimSw34: End Sub
Sub sw35_Hit:   PrimStandupTgtHit   35, Sw35, PrimSw35:PlaySoundAt SoundFX("target",DOFTargets),PrimSw35: End Sub
Sub sw35_Timer: PrimStandupTgtMove  35, Sw35, PrimSw35: End Sub
Sub sw36_Hit:   PrimStandupTgtHit   36, Sw36, PrimSw36:PlaySoundAt SoundFX("target",DOFTargets),PrimSw36: End Sub
Sub sw36_Timer: PrimStandupTgtMove  36, Sw36, PrimSw36: End Sub
Sub sw37_Hit:   PrimStandupTgtHit   37, Sw37, PrimSw37:PlaySoundAt SoundFX("target",DOFTargets),PrimSw37: End Sub
Sub sw37_Timer: PrimStandupTgtMove  37, Sw37, PrimSw37: End Sub


Sub Bumper1b_Hit
	vpmTimer.PulseSw 46 'Left
	PlaySoundAtBumperVol SoundFX("fx_bumper2",DOFContactors),Bumper1b,1
'	Playsound SoundFX("fx_bumper2",DOFContactors)
	lbumper1.state = 1
lbumper1a.state = 1
	Me.TimerEnabled = 1
End Sub
Sub Bumper1b_Timer
	lbumper1.state = 0
lbumper1a.state = 0
	Me.TimerEnabled = 0
End Sub

Sub Bumper2b_Hit
	vpmTimer.PulseSw 47 'Center
	PlaySoundAtBumperVol SoundFX("fx_bumper'",DOFContactors),Bumper2b,1
'	Playsound SoundFX("fx_bumper3",DOFContactors)
	lbumper2.state = 1
lbumper2a.state = 1
	Me.TimerEnabled = 1
End Sub
Sub Bumper2b_Timer
	lbumper2.state = 0
lbumper2a.state = 0
	Me.TimerEnabled = 0
End Sub
Sub Bumper3b_Hit
	vpmTimer.PulseSw 48 'Right
	PlaySoundAtBumperVol SoundFX("fx_bumper4",DOFContactors),Bumper3b,1
'	Playsound SoundFX("fx_bumper4",DOFContactors)
	lbumper3.state = 1
lbumper3a.state = 1
	Me.TimerEnabled = 1
End Sub
Sub Bumper3b_Timer
	lbumper3.state = 0
lbumper3a.state = 0
	Me.TimerEnabled = 0
End Sub

 


'Solenoids
SolCallBack(16) = "bsMissileKicker.SolOut"
SolCallBack(25) = "bsTrough.SolIn"
SolCallBack(26) = "bsTrough.SolOut"
SolCallBack(27) = "CRBallLock"
SolCallBack(28) = "bsRadarEject.SolOut"
'SolCallBack(29) = "bsVuk.SolOut"  'VUK VERSION
'SolCallBack(30) = "dtDrop.SolDropUp"  'TARGET BANK NOT USED IN THIS VERSION
SolCallBack(32) = "vpmSolSound SoundFX(""fx_knocker"",DOFKnocker),"    'Need to confirm
SolCallBack(1) = "SetLamp 101," 'King Shooter
SolCallBack(2) = "SetLamp 102," 'King Shooter
SolCallback(3) = "SetLamp 103," 'Ape Ramp BG Skull
SolCallback(4) = "SetLamp 104," 'BG NDSRS Shooter
SolCallback(5) = "SetLamp 105," 'BG Tower 25k
SolCallback(6) = "SetLamp 106," 'BG Tower 50k
SolCallback(7) = "SetLamp 107," 'BG Tower 100k
SolCallback(8) = "SetLamp 108," 'BG Tower Extra Ball
SolCallback(9) = "SetLamp 109," 'BG Tower Million
'SolCallback(10) = NOT USED
SolCallback(11) = "solGI " ' General Illum
SolCallback(12) = "solLamp112" 'BG Girl Missile
SolCallback(13) = "SetLamp 113," 'BG Kong Explode
SolCallback(14) = "SetLamp 114," 'Tower Flasher
SolCallback(15) = "SetLamp 115," 'Skull Island
'SolCallback(22) = NOT USED
'SolCallback(23) = NOT USED
'SolCallback(24) = NOT USED


'Sub SolKnocker(enabled)
'If enabled then Playsound SoundFX("fx_knocker",DOFKnocker)
'End Sub


Sub solGI (enabled)
	if (enabled) Then
		SetLamp 111, 0
        GiOff
	else
		SetLamp 111, 1
        GiOn
	End If
End Sub

Sub solLamp112 (enabled)
	SetLamp 112, enabled
	SetLamp 199, enabled
End Sub

Sub GiOn
    DOF 126, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
upperpf.blenddisablelighting = 0.1
Wall3.blenddisablelighting = 0.1
End Sub

Sub GiOff
    DOF 126, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    Table1.ColorGradeImage = "ColorGrade_4"
upperpf.blenddisablelighting = 0
Wall3.blenddisablelighting = 0
End Sub



'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers),LeftFlipper
        LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers),LeftFlipper
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers),RightFlipper
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers),RightFlipper2
        RightFlipper.RotateToEnd
        RightFlipper2.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers),RightFlipper
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers),RightFlipper2
        RightFlipper.RotateToStart
        RightFlipper2.RotateToStart
    End If
End Sub

Sub UpdateFlipperLogo()
		batleft.objrotz = LeftFlipper.CurrentAngle + 1
		batright.objrotz = RightFlipper.CurrentAngle - 1

		pLSS.Roty = LeftFlipper.Currentangle - 90
		pRSS.Roty = RightFlipper.Currentangle - 90

		batright1.objrotz = RightFlipper2.CurrentAngle - 1 
		batright2.objrotz = RightFlipper2.CurrentAngle - 1 
  lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle

End Sub

Sub GameTimer_Timer
    UpdateFlipperLogo
  '  RollingUpdate
    BallShadowUpdate
End Sub

 'Drains and Kickers
   Sub Drain_Hit():PlaySoundAtVol "fx_drain",Drain,.8:vpmTimer.PulseSw 10:bsTrough.Addball Me:End Sub
   Sub BallRelease_UnHit():End Sub

'Missile Kicker Lock
Sub MissileKicker_Hit():PlaySoundat "fx_kicker-enter",MissileKicker: bsMissileKicker.AddBall 0:End Sub

'Radar Eject Lock
Sub RadarEject_Hit():PlaySoundat "fx_kicker-enter",RadarEject: bsRadarEject.AddBall 0:End Sub

'Cage Tunnel

Sub CageKicker_Hit():PlaySoundat "fx_Drain",CageKicker:cagekicker.kick 0, 5:CageReturnTimer.Enabled = 1:End Sub

'VUK Loc
Sub sw29_Hit():	vpmTimer.PulseSw 29:pRampSwitch1.ObjRotX = -10: Stopsound "fx_ramp_metal" :End Sub
Sub sw29_UnHit():pRampSwitch1.ObjRotX = 0:End Sub
Sub Trigger1_Hit(): Playsound "fx_ramp_metal" :End Sub
Sub Trigger1_UnHit(): vpmtimer.addtimer 200, "FXRamp '" :End Sub
Sub FXRamp
    Stopsound "fx_ramp_metal"
End Sub

'***Slings and rubbers
  ' Slings
 Dim LStep, RStep
 
 Sub LeftSlingShot_Slingshot

'    PlaySound SoundFXDOF("LeftSlingShot", 103, DOFPulse, DOFContactors), 0, 1, -0.05, 0.05
	PlaySoundAt SoundFX("LeftSlingShot",DOFContactors),Lemk
    LeftSling4.Visible = 1
    Lemk.RotX = 26:SlingKongLeft.RotX = 26
    LStep = 0
    LeftSlingshot.TimerEnabled = True
    vpmTimer.PulseSw 21
End Sub

Sub LeftSlingshot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14:SlingKongLeft.RotX = 12
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2:SlingKongLeft.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:SlingKongLeft.RotX = -10:LeftSlingshot.TimerEnabled = 0
    End Select

    LStep = LStep + 1
End Sub

Sub RightSlingshot_Slingshot() 
    'RightSlingNew.SolenoidOn
	PlaySoundAt SoundFX("RightSlingShot",DOFContactors),Remk
'    PlaySound SoundFXDOF("RightSlingShot", 105, DOFPulse, DOFContactors), 0, 1, 0.05, 0.05
    RightSling4.Visible = 1
    Remk.RotX = 26:SlingKongRight.RotX = 26
    RStep = 0
    RightSlingshot.TimerEnabled = True
    vpmTimer.PulseSw 22
End Sub

Sub RightSlingshot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14:SlingKongRight.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2:SlingKongRight.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:SlingKongRight.RotX = -10:RightSlingshot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub 

Sub UpdateDoor
	Primitive7.rotx = (gate5.currentAngle * 35/90)
	p_gate3.rotx = Gate3.CurrentAngle + 120
End Sub

Sub RailTrigger_Hit():  PlaySound "Rail": End Sub
Sub Bullseyes_Hit(IDX):PlaySoundAt SoundFX("target",DOFTargets),Bullseyes:End Sub  
 
'**********************************************************
'     JP's Flasher Fading for VPX and Vpinmame
'       (Based on Pacdude's Fading Light System)
' This is a fast fading for the Flashers in vpinmame tables
'  just 4 steps, like in Pacdude's original script.
' Included the new Modulated flashers & Lights
'**********************************************************

Dim FadingState(200)
Dim TextureArray1: TextureArray1 = Array("Plastic with an image1", "Plastic with an image1 trans")
Dim FilamentArray: FilamentArray = Array("WireDT_off", "WireDT_33", "WireDT_66", "WireDT_on")
Dim ClearBulbArray: ClearBulbArray = Array("BulbGIoff", "BulbGIOn")
Dim YellowDomeArray: YellowDomeArray = Array("dome3_yellowBright", "dome3_yellowBright", "dome3_yellowBright", "dome3_yellow")

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
    UpdateDoor
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
	Lampm 21, l21x
	Lamp 21, l21
	Lampm 22, l22x
	Lamp 22, l22
	Lampm 23, l23x
	Lamp 23, l23
	Lampm 24, l24x
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
	Lampm 48, l48
	Lamp  48, l48_a
	Lampm 56, l56
	Lamp  56, l56_a					
	Lamp 62, l62
	Lamp 63, l63
	Lamp 64, l64

	'Solenoid flasher inserts
	Lampm  101, l101
    Lamp  101, l101a
	Lampm  102, l102
	Lamp  102, l102a
	Lampm  103, l103
    Lamp  103, l103a
	Lampm 112, l112 'BG Girl Missile
	Lamp 112, l112a
	Lampm 113, l113 'BG Kong Explode
	Lampm 115, l115 'BG Kong Explode
    Lampm 115, l115a

	Flash  57,  f57', 255
	Flash  58,  f58', 255
	Flash  59,  f59', 255
	Flash  60,  f60', 255
	Flash  61,  f61', 255		
    Lampm 104, f104c
	Lampm 104, f104b
	Lamp 104, f104a
	Lampm 113, l113b
	Lamp 113, l113a
	FadeDisableLighting 114, Primitive2
	FadeMaterialP 114, Primitive2, TextureArray1	
	Lampm 114, f114b
	Lamp 114, f114a
	FadeDisableLighting 199, pBulbFilament1
	FadePri4m 199, pBulbFilament1, FilamentArray
	FadeMaterialP 199, pBulbFlasher1, ClearBulbArray
	Flash 199, F112X', 255 'set when 112 is set
	Flash 115, f115', 255	
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



''''Additions by CP

Dim aa


Sub FadeDisableLighting(nr, a)
    Select Case FadingState(nr)
        Case 4:a.DisableLighting = 1
        Case 3:a.DisableLighting = 0
        Case 2:a.DisableLighting = .33
        Case 1:a.DisableLighting = .66
    End Select
End Sub


'trxture swap	
dim itemw, itemp, itemp2

Sub FadeMaterialW(nr, itemw, group)
    Select Case FadingState(nr)
        Case 4:itemw.TopMaterial = group(1):itemw.SideMaterial = group(1)
        Case 3:itemw.TopMaterial = group(0):itemw.SideMaterial = group(0)
    End Select
End Sub


Sub FadeMaterialP(nr, itemp, group)
    Select Case FadingState(nr)
        Case 4:itemp.Material = group(1)
        Case 3:itemp.Material = group(0)
    End Select
End Sub


Sub FadeMaterial2P(nr, itemp2, group)
    Select Case FadingState(nr)
        Case 4:itemp2.Material = group(1)
        Case 3:itemp2.Material = group(0)
    End Select
End Sub

Sub FadePri4m(nr, pri, group)
    Select Case FadingState(nr)
		Case 4:pri.image = group(1) 'Off
        Case 3:pri.image = group(3) 'Fading...
        Case 2:pri.image = group(2) 'Fading...
        Case 1:pri.image = group(0) 'ON
    End Select
End Sub

Sub FadePri4(nr, pri, group)
    Select Case FadingState(nr)
        Case 4:pri.image = group(1):FadingLevel(nr) = 0 'Off
        Case 3:pri.image = group(3):FadingLevel(nr) = 2 'Fading...
        Case 2:pri.image = group(2):FadingLevel(nr) = 3 'Fading...
        Case 1:pri.image = group(0):FadingLevel(nr) = 1 'ON
    End Select
End Sub

''''End of additions by CP


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
 
 Sub FadeLCo(nr, a, b) 'fading collection of lights
     Dim obj
     Select Case LampState(nr)
         Case 2:vpmSolToggleObj b, Nothing, 0, 0:LampState(nr) = 0
         Case 3:vpmSolToggleObj b, Nothing, 0, 1:LampState(nr) = 2
         Case 4:vpmSolToggleObj a, Nothing, 0, 0:LampState(nr) = 3
         Case 5:vpmSolToggleObj b, Nothing, 0, 1:LampState(nr) = 6
         Case 6:vpmSolToggleObj a, Nothing, 0, 1:LampState(nr) = 1
     End Select
 End Sub
 
  Sub NFadeLCo(nr, a) 'fading collection of lights
     Dim obj
     Select Case LampState(nr)
         Case 4:vpmSolToggleObj a, Nothing, 0, 0:LampState(nr) = 0
         Case 5:vpmSolToggleObj a, Nothing, 1, 1:LampState(nr) = 1
     End Select
 End Sub
 
 Sub FlashL(nr, a, b) ' simple light flash, not controlled by the rom
     Select Case LampState(nr)
         Case 2:b.state = 0:LampState(nr) = 0
         Case 3:b.state = 1:LampState(nr) = 2
         Case 4:a.state = 0:LampState(nr) = 3
         Case 5:a.state = 1:LampState(nr) = 4
     End Select
 End Sub
 
 Sub MFadeL(nr, a, b, c) 'Light acting as a flash. C is the light number to be restored
     Select Case LampState(nr)
         Case 2:b.state = 0:LampState(nr) = 0
             If LampState(c) = 1 Then SetLamp c, 1
         Case 3:b.state = 1:LampState(nr) = 2
         Case 4:a.state = 0:LampState(nr) = 3
         Case 5:a.state = 1:LampState(nr) = 1
     End Select
 End Sub
 
 Sub NFadeB(nr, a, b, c, d) 'New Bally Bumpers: a and b are the off state, c and d and on state, no fading
     Select Case LampState(nr)
         Case 4:a.IsDropped = 0:b.IsDropped = 0:c.IsDropped = 1:d.IsDropped = 1:LampState(nr) = 0
         Case 5:a.IsDropped = 1:b.IsDropped = 1:c.IsDropped = 0:d.IsDropped = 0:LampState(nr) = 1
     End Select
 End Sub
 
 Sub NFadeBm(nr, a, b, c, d)
     Select Case LampState(nr)
         Case 4:a.IsDropped = 0:b.IsDropped = 0:c.IsDropped = 1:d.IsDropped = 1
         Case 5:a.IsDropped = 1:b.IsDropped = 1:c.IsDropped = 0:d.IsDropped = 0
     End Select
 End Sub
 
 ' only for this table
 
 Sub ThumperW(nr, a, b) 'nr = light number, a = dropwall array 16 walls, b = bumper animation state
     Dim ii
     Select Case LampState(nr)
         Case 2:For Each ii in a:ii.IsDropped = 1:Next:a(b).IsDropped = 0:LampState(nr) = 0      'Off
         Case 3:For Each ii in a:ii.IsDropped = 1:Next:a(4 + b).IsDropped = 0:LampState(nr) = 2  'fading to Off
         Case 4:For Each ii in a:ii.IsDropped = 1:Next:a(8 + b).IsDropped = 0:LampState(nr) = 3  'fading to Off
         Case 5:For Each ii in a:ii.IsDropped = 1:Next:a(12 + b).IsDropped = 0:LampState(nr) = 1 'On
     End Select
 End Sub
 
 Sub ThumperWm(nr, a, b) 'used to update the state with the bumper animation
     Dim ii
     Select Case LampState(nr)
         Case 0:For Each ii in a:ii.IsDropped = 1:Next:a(b).IsDropped = 0
         Case 1:For Each ii in a:ii.IsDropped = 1:Next:a(12 + b).IsDropped = 0
         Case 2:For Each ii in a:ii.IsDropped = 1:Next:a(b).IsDropped = 0
         Case 3:For Each ii in a:ii.IsDropped = 1:Next:a(4 + b).IsDropped = 0
         Case 4:For Each ii in a:ii.IsDropped = 1:Next:a(8 + b).IsDropped = 0
         Case 5:For Each ii in a:ii.IsDropped = 1:Next:a(12 + b).IsDropped = 0
 
     End Select
 End Sub 

'      [ raising ramp animation]
 Dim lkpos	'Animation position 
  lkpos = 0:	'Set Starting positions
 
 sub lkong_anim(enabled)
 if enabled then
 	lkongout.Enabled=1 'Start animation

 else
    lkongin.Enabled=1

    
 end if
 End Sub

  Sub lkongout_Timer()
 	 Select Case lkPos	
 			Case 1:lskong.x=175
 			Case 2:lskong.x=185
 			Case 3:lskong.x=195
 			Case 4:lskong.x=205
 			Case 5:lskong.x=215
            Case 6:lskong.x=225:lkongout.Enabled=0:lkongin.Enabled=1:

 
End Select

 	If lkpos<6 then lkPos=lkpos+1
  End Sub
 
  Sub lkongin_Timer()
 	 Select Case lkPos	


 			Case 1:lskong.x=175:lkongin.Enabled=0
 			Case 2:lskong.x=185
 			Case 3:lskong.x=195
 			Case 4:lskong.x=205
 			Case 5:lskong.x=215
            Case 6:lskong.x=225
 	End Select
 	If lkpos>0 Then lkPos=lkpos-1
  End Sub
'**************************************************************************************************************
 Dim rkpos	'Animation position 
  rkpos = 0:	'Set Starting positions
 
 sub rkong_anim(enabled)
 if enabled then
 	rkongout.Enabled=1 'Start animation

 else
    rkongin.Enabled=1    
 end if
 End Sub

  Sub rkongout_Timer()
 	 Select Case rkPos	
 			Case 1:rskong.x=680
 			Case 2:rskong.x=670
 			Case 3:rskong.x=660
 			Case 4:rskong.x=650
 			Case 5:rskong.x=640
            Case 6:rskong.x=630:rkongout.Enabled=0:rkongin.Enabled=1: 
End Select

 	If rkpos<6 then rkPos=rkpos+1
  End Sub
 
  Sub rkongin_Timer()
 	 Select Case rkPos	


 			Case 1:rskong.x=680:rkongin.Enabled=0
 			Case 2:rskong.x=670
 			Case 3:rskong.x=660
 			Case 4:rskong.x=650
 			Case 5:rskong.x=640
            Case 6:rskong.x=630
 	End Select
 	If rkpos>0 Then rkPos=rkpos-1
  End Sub



'''''''''''''''''''''''''''''
''   Visible Locks - from CP's BTTF table
'''''''''''''''''''''''''''''

Dim CRBLStep, WPStep, WP2Step, PlasticWobbling

Sub sw40_Hit()
	Psw40.rotY = 20
	Controller.Switch(40) = 1
End Sub
Sub sw40_UnHit:Psw40.rotY = 0:Controller.Switch(40) = 0:End Sub

Sub sw39_Hit:Psw39.rotY = 20:Controller.Switch(39) = 1:End Sub
Sub sw39_UnHit:Psw39.rotY = 0:Controller.Switch(39) = 0:End Sub

Sub sw38_Hit()
	Psw38.rotY = 20
	Controller.Switch(38) = 1
End Sub
Sub sw38_UnHit:Psw38.rotY = 0:Controller.Switch(38) = 0:End Sub

Sub CRBallLock(Enabled)
		pCRLock.collidable = false
		pCRLock.RotZ = 50
		CRBallLockTimer.Enabled = true
End Sub

Sub CRBallLockTimer_Timer()
	Select Case CRBLStep
		Case 0:
		Case 1:
		Case 2:
		Case 3:
		Case 4:
		Case 5:pCRLock.collidable = true:pCRLock.RotZ = 0:CRBallLockTimer.Enabled = false:CRBLStep = 0
	End Select
	CRBLStep = CRBLStep + 1

End Sub  

'****************************************************************************
'*****
'***** Primitive Animation subroutines					(gtxjoe v1.1)
'*****
'****************************************************************************
Const WallPrefix 		= "Sw" 'Change this based on your naming convention
Const PrimitivePrefix 	= "PrimSw"'Change this based on your naming convention
Const PrimitiveBumperPrefix = "BumperRing" 'Change this based on your naming convention
Dim primCnt(100), primDir(100), primBmprDir(6)


'****************************************************************************
'***** Primitive Rollover Animation
'USAGE: 	Sub sw18_Hit	: PrimRolloverHit   18, PrimSw18: End Sub 
'USAGE: 	Sub sw18_UnHit	: PrimRolloverUnHit 18, PrimSw18: End Sub
'****************************************************************************
Const RolloverMovementDir = "TransY" 
Const RolloverMovementInit = 0
Const RolloverMovementMax = -14	 

Sub PrimRolloverHit (swnum, primName): PrimRollover swnum, primName, 1: End Sub
Sub PrimRolloverUnHit (swnum, primName): PrimRollover swnum, primName, 0: End Sub

Sub PrimRollover (swnum, primName, isdropped)
	If isdropped = 1 Then
'		PlaySound "rollover"
		Controller.Switch(swnum) = 1
		PrimMove primName, RolloverMovementDir, RolloverMovementMax
	Else
		Controller.Switch(swnum) = 0
		PrimMove primName, RolloverMovementDir, RolloverMovementInit
	End If
End Sub

'=== Generic Primitive Routines
Sub PrimMove (primName, primDir, val)
	Select Case primDir
		Case "TransX":  primName.TransX = val
		Case "TransY":  primName.TransY = val  
		Case "TransZ":  primName.TransZ = val  
	End Select
End Sub
'****************************************************************************
'***** Primitive StarRollover Animation
'USAGE: 	Sub sw17_Hit	: PrimStarRolloverHit   17, Sw17, PrimSw17: End Sub 
'USAGE: 	Sub sw17_UnHit	: PrimStarRolloverUnHit 17, Sw17, PrimSw17: End Sub

' or if delay needed use this
'USAGE: 	Sub sw17_Hit	: PrimStarRolloverHit   17, Sw17, PrimSw17: End Sub 
'USAGE: 	Sub sw17_UnHit	: PrimStarRolloverUnHitWithDelay 17, Sw17, PrimSw17, 200: End Sub
'USAGE:		Sub sw17_Timer	: PrimStarRolloverTimeout Sw17, PrimSw17: End Sub
'****************************************************************************
Const StarRolloverMovementDir = "TransY" 	'Specify direction
Const StarRolloverMovementInit = 0	 		'Specify starting position, usually 0
Const StarRolloverMovementMax = 6	 		'Amount to move when hit

Sub PrimStarRolloverHit (swnum, triggerName, primName): PrimStarRollover swnum, triggerName, primName, 1, 0: End Sub
Sub PrimStarRolloverUnHit (swnum, triggerName, primName): PrimStarRollover swnum, triggerName, primName, 0, 0: End Sub
Sub PrimStarRolloverUnHitWithDelay (swnum, triggerName, primName, delay): PrimStarRollover swnum, triggerName, primName, 0, delay: End Sub
Sub PrimStarRolloverTimeout (triggerName, primName)
	PrimMove primName, StarRolloverMovementDir, StarRolloverMovementInit
	triggerName.TimerEnabled = 0
End Sub

Sub PrimStarRollover (swNum, triggerName, primName, isDropped, upTimerDelay)
	PrimStarRolloverCustom swNum, triggerName, primName, isDropped, StarRolloverMovementDir, StarRolloverMovementInit, StarRolloverMovementMax, upTimerDelay
End Sub

Sub PrimStarRolloverCustom (swNum, triggerName, primName, isDropped, primDirection, startingPos, endingPos, upTimerDelay)
	If isdropped = 1 Then
'		PlaySound "rollover"
		Controller.Switch(swnum) = 1
		PrimMove primName, primDirection, endingPos
	Else
		Controller.Switch(swnum)= 0
		If upTimerDelay = 0 Then
			PrimMove primName, primDirection, startingPos
		Else 'use timer before returning trigger to normal position
			triggerName.TimerInterval = upTimerDelay
			triggerName.TimerEnabled = 1
		End If
	End If
End Sub

Sub PrimStarRolloverCustomTimeout (triggerName, primName, startingPos)
	PrimMove primName, StarRolloverMovementDir, startingPos
	triggerName.TimerEnabled = 0
End Sub

'****************************************************************************
'***** Primitive Standup Target Animation
'****************************************************************************
'USAGE: 	Sub sw1_Hit: 	PrimStandupTgtHit  1, Sw1, PrimSw1: End Sub
'USAGE: 	Sub Sw1_Timer: 	PrimStandupTgtMove 1, Sw1, PrimSw1: End Sub

Const StandupTgtMovementDir = "TransZ" 
Const StandupTgtMovementMax = -6	 

Sub PrimStandupTgtHit (swnum, wallName, primName) 	
'	PlaySound SoundFx("target", DOFTargets)
	vpmTimer.PulseSw swnum	
	primCnt(swnum) = 0 									'Reset count
	wallName.TimerInterval = 20 	'Set timer interval
	wallName.TimerEnabled = 1 	'Enable timer
End Sub

Sub	PrimStandupTgtMove (swnum, wallName, primName)
	Select Case StandupTgtMovementDir
		Case "TransX":
			Select Case primCnt(swnum)
				Case 0: 	primName.TransX = -StandupTgtMovementMax * .5
				Case 1: 	primName.TransX = -StandupTgtMovementMax
				Case 2: 	primName.TransX = -StandupTgtMovementMax * .5
				Case 3: 	primName.TransX = 0
				Case else: 	wallName.TimerEnabled = 0
			End Select
		Case "TransY":
			Select Case primCnt(swnum)
				Case 0: 	primName.TransY = -StandupTgtMovementMax * .5
				Case 1: 	primName.TransY = -StandupTgtMovementMax
				Case 2: 	primName.TransY = -StandupTgtMovementMax * .5
				Case 3: 	primName.TransY = 0
				Case else: 	wallName.TimerEnabled = 0
			End Select
		Case "TransZ":
			Select Case primCnt(swnum)
				Case 0: 	primName.TransZ = -StandupTgtMovementMax * .5
				Case 1: 	primName.TransZ = -StandupTgtMovementMax
				Case 2: 	primName.TransZ = -StandupTgtMovementMax * .5
				Case 3: 	primName.TransZ = 0
				Case else: 	wallName.TimerEnabled = 0
			End Select			
	End Select
	primCnt(swnum) = primCnt(swnum) + 1
End Sub 


'************************************************************************
'***** Primitive Drop Target Animation
'************************************************************************
'USAGE:  Sub Sw13_Hit: PrimDropTgtDown RBank, 3, 13, Sw13: End Sub 
'USAGE:  Sub Sw13_Timer: PrimDropTgtMove 13, Sw13, PrimSw13: End Sub
'USAGE:  Sub solRBankReset (enabled): If enabled Then PrimDropTgtUp RBank, 1, 13, Sw13, 1: PrimDropTgtUp RBank, 2, 12, Sw12, 0: PrimDropTgtUp RBank, 3, 11, Sw11, 0: End If: End Sub

Const DropTgtMovementDir = "TransY" 
Const DropTgtMovementMax = 50	 
	
Sub PrimDropTgtDown (targetbankname, targetbanknum, swnum, wallName)
	PrimDropTgtAnimate swnum, wallName, 0
	targetbankname.Hit targetbanknum

End Sub
Sub PrimDropTgtUp  (targetbankname, targetbanknum, swnum, wallName, resetvpmbank)
	PrimDropTgtAnimate swnum, wallName, 1
	If resetvpmbank = 1 Then targetbankname.SolDropUp True
End Sub
Sub PrimDropTgtSolDown  (targetbankname, targetbanknum, swnum, wallName, resetvpmbank)
	PrimDropTgtAnimate swnum, wallName, 0
	If resetvpmbank = 1 Then targetbankname.SolDropDown True
End Sub

Sub PrimDropTgtMove (swNum, wallName, primName) 'Customize direction as needed
	Select Case DropTgtMovementDir
		Case "TransX":
			If primDir(swNum) = 1 Then 'Up
				Select Case primCnt(swNum)
					Case 0: 	primName.TransX = -DropTgtMovementMax * .75
					Case 1: 	primName.TransX = -DropTgtMovementMax * .25
					Case 2,3,4: primName.TransX = 10
					Case 5: 	primName.TransX = 0
					Case else: 	wallName.TimerEnabled = 0
				End Select
			Else 'Down
				Select Case primCnt(swNum)
					Case 0: primName.TransX = -DropTgtMovementMax * .25
					Case 1: primName.TransX = -DropTgtMovementMax * .5
					Case 2: primName.TransX = -DropTgtMovementMax * .75
					Case 3: primName.TransX = -DropTgtMovementMax
					Case else: wallName.TimerEnabled = 0
				End Select
			End If
		Case "TransY":
			If primDir(swNum) = 1 Then 'Up
				Select Case primCnt(swNum)
					Case 0: 	primName.TransY = -DropTgtMovementMax * .75
					Case 1: 	primName.TransY = -DropTgtMovementMax * .25
					Case 2,3,4: primName.TransY = 10
					Case 5: 	primName.TransY = 0
					Case else: 	wallName.TimerEnabled = 0
				End Select
			Else 'Down
				Select Case primCnt(swNum)
					Case 0: primName.TransY = -DropTgtMovementMax * .25
					Case 1: primName.TransY = -DropTgtMovementMax * .5
					Case 2: primName.TransY = -DropTgtMovementMax * .75
					Case 3: primName.TransY = -DropTgtMovementMax
					Case else: wallName.TimerEnabled = 0
				End Select
			End If
		Case "TransZ":
			If primDir(swNum) = 1 Then 'Up
				Select Case primCnt(swNum)
					Case 0: 	primName.TransZ = -DropTgtMovementMax * .75
					Case 1: 	primName.TransZ = -DropTgtMovementMax * .25
					Case 2,3,4: primName.TransZ = 10
					Case 5: 	primName.TransZ = 0
					Case else: 	wallName.TimerEnabled = 0
				End Select
			Else 'Down
				Select Case primCnt(swNum)
					Case 0: primName.TransZ = -DropTgtMovementMax * .25
					Case 1: primName.TransZ = -DropTgtMovementMax * .5
					Case 2: primName.TransZ = -DropTgtMovementMax * .75
					Case 3: primName.TransZ = -DropTgtMovementMax
					Case else: wallName.TimerEnabled = 0
				End Select
			End If			
	End Select
	primCnt(swnum) = primCnt(swnum) + 1
End Sub

Sub PrimDropTgtAnimate  (swnum, wallName, dir)
	primCnt(swnum) = 0
	primDir(swnum) = dir
	wallName.TimerInterval = 10
	wallName.TimerEnabled = 1			
End Sub


'************************************************************************
'***** Primitive Bumper Animation 
'****************************************************************************
'USAGE:  Sub Bumper1_Hit(): PrimBumperHit 1, 40: End Sub
'USAGE:  Sub Bumper2_Hit(): PrimBumperHit 2, 39: End Sub

Const PrimBumperRingDir = "TransY" 
Const PrimBumperRingMovementMax  = 50
Const PrimBumperRingMovementStep = 10
Const PrimBumperTimeInterval     = 10

Sub PrimBumperHit (ringnum, swnum)
	vpmTimer.PulseSw swnum
	If Bumper1.Disabled = False Then
		PrimBmprDir(ringnum) = 1
		Execute "Bumper" & ringnum & "Timer.Interval = " & PrimBumperTimeInterval
		Execute "Bumper" & ringnum & "Timer.Enabled = 1" 		'Enable timer
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySound Soundfx("bumper_1",DOFContactors)
			Case 2 : PlaySound Soundfx("bumper_2",DOFContactors)
			Case Else : PlaySound Soundfx("bumper_3",DOFContactors)
		End Select
	End If
End Sub

Sub Bumper1Timer_Timer: PrimBumperMove(1): End Sub
Sub Bumper2Timer_Timer: PrimBumperMove(2): End Sub
Sub Bumper3Timer_Timer: PrimBumperMove(3): End Sub
Sub Bumper4Timer_Timer: PrimBumperMove(4): End Sub
Sub Bumper5Timer_Timer: PrimBumperMove(5): End Sub

Sub PrimBumperMove (ringnum)
	Dim tempZ
	Execute "tempZ = " & PrimitiveBumperPrefix & ringnum & "." & PrimBumperRingDir
	If PrimBmprDir(ringnum) = 1 and tempZ > -PrimBumperRingMovementMax then 'Go down
		tempZ = tempZ - PrimBumperRingMovementStep
	ElseIf PrimBmprDir(ringnum) = 0 and tempZ > 0 Then 'Stop timer
		tempZ = 0
		Execute "Bumper" & ringnum & "Timer.Enabled = 0"
	Else 'Go up
		PrimBmprDir(ringnum) = 0
		tempZ = tempZ + PrimBumperRingMovementStep
	End If
	Execute PrimitiveBumperPrefix & ringnum & "." & PrimBumperRingDir & "=" & tempZ
	'debug.print bumperring1.transy & "," & bumperring2.transy
End Sub

''*************************************************************
''Toggle DOF sounds on/off based on cController value
''*************************************************************
dim incup
sub trigger1_hit
	activeball.vely = activeball.vely  * 2
	activeball.velz = activeball.velz * 2
end sub

sub trigger3_hit
	activeball.vely = activeball.vely  * 1.1
	activeball.velz = activeball.velz * 1.1
	debug.print incup & ":1 " & activeball.vely & ":" & activeball.velx
end sub
sub trigger4_hit
	activeball.vely = activeball.vely  * 1.1
	activeball.velz = activeball.velz * 1.1
	debug.print incup & ":2 " & activeball.vely & ":" & activeball.velx
end sub
sub trigger5_hit
	activeball.vely = activeball.vely  * 1.06
	activeball.velz = activeball.velz * 1.06
	debug.print incup & ":3 " & activeball.vely & ":" & activeball.velx
end sub
sub trigger6_hit
	activeball.vely = activeball.vely  * 1.02
	activeball.velz = activeball.velz * 1.02
	debug.print incup & ":4 " & activeball.vely & ":" & activeball.velx
end sub


'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 5000)
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

Const tnob = 8 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
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
      If BallVel(BOT(b) ) > 1 Then
        rolling(b) = True
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          rolling(b) = False
        End If
      End If
 ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*********** BALL SHADOW *********************************
Sub BallShadowUpdate()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7,BallShadow8)
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10                       
        If BOT(b).Z > 20 and BOT(b).Z < 240 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 110
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub

'******************************************************
' 				JP's Sound Routines
'******************************************************

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub aRubbers_Hit(idx)
    PlaySoundAtBallVol "rubber_hit_" & Int(Rnd*3)+1, 1
End Sub

Sub Posts_Hit(idx)
    PlaySoundAtBallVol "rubber_hit_" & Int(Rnd*3)+1, .8
End Sub


Sub LeftFlipper_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 2
End Sub

Sub RightFlipper_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 2
End Sub

Sub RightFlipper2_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 2
End Sub

Sub plastic_Hit(idx)
    PlaySoundAtBallVol "plastichit" & Int(Rnd*1)+1, 1
End Sub



'''''''''''''''''''''''''''other sounds

'''''''Random Metal and plastic sounds

Sub RandomMetalHitSound()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtBallVol "metal_hit1",1
		Case 2 : PlaySoundAtBallVol "metal_hit2",1
		Case 3 : PlaySoundAtBallVol "metal_hit3",1
	End Select
End Sub

Sub RandomPlasticHitSound()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtBallVol "plastic_hit1",1
		Case 2 : PlaySoundAtBallVol "plastic_hit2",1
		Case 3 : PlaySoundAtBallVol "plastic_hit2",1
	End Select
End Sub



'**************************************************************************
'                 Positional Sound Playback Functions by DJRobX
'**************************************************************************

'Set position as table object (Use object or light but NOT wall) and Vol to 1

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Vol)
		PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
		PlaySound sound, 0, VolZ(BOT), AudioPan(BOT), 0, Pitch(BOT), 1, 1, AudioFade(BOT)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
		PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub


'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
		PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

'*****************************
'Random Ramp and Orbit Sounds
'*****************************

Dim NextOrbitHit:NextOrbitHit = 0 

Sub WireRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump3 .1, Pitch(ActiveBall)+5
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .4 + (Rnd * .2)
	end if 
End Sub

Sub PlasticRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .4 and Timer > NextOrbitHit then
		RandomBump 5, Pitch(ActiveBall)
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub


Sub MetalGuideBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump2 2, Pitch(ActiveBall)
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub

Sub MetalWallBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 2, 20000 'Increased pitch to simulate metal wall
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub


' Requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "fx_rampbump" & CStr(Int(Rnd*7)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' Requires metalguidebump1 to 2 in Sound Manager
Sub RandomBump2(voladj, freq)
	dim BumpSnd:BumpSnd= "FX_metalhit" & CStr(Int(Rnd*2)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' Requires WireRampBump1 to 5 in Sound Manage
Sub RandomBump3(voladj, freq)
	dim BumpSnd:BumpSnd= "WireRampBump" & CStr(Int(Rnd*5)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub


' Stop Bump Sounds
Sub BumpSTOP1_Hit ()
dim i:for i=1 to 4:StopSound "WireRampBump" & i:next
NextOrbitHit = Timer + 1
End Sub

Sub BumpSTOP2_Hit ()
dim i:for i=1 to 4:StopSound "PlasticRampBump" & i:next
NextOrbitHit = Timer + 1
End Sub

