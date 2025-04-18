' Indiana Jones: The Pinball Adventure / IPD No. 1267 / August, 1993 / 4 Players
' Thanks to destruk for previous code.
' Thanks to knorr and clark kent for the help and the resources.
' Thanks to flupper for bumper caps, flasher domes models.
' Thanks to RustyCardores for SSF 
' Thanks to VP Dev team for the freaking amazing VPX!

Option Explicit
Randomize

'************************************************************************
'							Table options
'************************************************************************
Const BlimpToy = 0					'Shows a Custom Nazi Blimp Toy
Const PropellerMod = 1				'Animate Bi-Plane Propeller when making left ramp (0= no, 1= yes)
Const InstrCardType = 0				'Instruction Cards Type (0= original, 1= custom, 2 = random)
Const FlipperType = 2				'Flippers Type (0= red rubber, 1= black rubber, 2 = random)
Const TargetDecals = 0				'Shows Custom Target Decals (0= no, 1= yes)
Const SideCabDecals = 1				'Shows Custom Cabinet Decals (0= no, 1= yes)
'************************************************************************
'						End of table options
'************************************************************************

Const BallSize = 52
Const BallMass = 1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD:UseVPMDMD = DesktopMode
Const UseVPMModSol = 1

LoadVPM "02800000", "WPC.VBS", 3.55

'************************************
'******* Standard definitions *******
'************************************
' Rom Name
Const cGameName = "ij_l7"

' Standard Options
Const UseSolenoids = 2
Const UseLamps = 1
Const UseSync = 1
Const HandleMech = 0

' IJ Specific Option
Const cSingleLFlip = 0
Const cSingleRFlip = 0

'keyStagedFlipperL=""
'keyStagedFlipperR=""

' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = ""
Const SCoin = "fx_Coin"

'************************************************************************
'						 Solenoids map
'************************************************************************

'(*) handled by cvpmMech

SolCallback(1)="bsPopper.SolOut" 											'BallPopper
SolCallback(2)="AutoPlunger" 												'BallLaunch
SolCallback(3)="dtTotem.SolDropUp" 											'TotemDropUp
SolCallback(4)="SolBallRelease" 											'BallRelease
SolCallback(5)="dtBank.SolDropUp"											'CenterDropUp
SolCallback(6)="SolIdol"													'IdolRelease
SolCallback(7)="vpmSolSound SoundFX(""fx_Knocker"",DOFKnocker),"			'Knocker
SolCallback(8)="bsLEject.SolOut"											'Left Eject
SolCallback(9)="vpmSolSound SoundFX(""LeftJetNOTUSED"",DOFContactors),"			'Left Bumper
SolCallback(10)="vpmSolSound SoundFX(""RightJetNOTUSED"",DOFContactors),"			'Right Bumper
SolCallback(11)="vpmSolSound SoundFX(""BottomJetNOTUSED"",DOFContactors),"			'Bottom Bumper
SolCallback(12)="vpmSolSound SoundFX(""RightSlingShot"",DOFContactors),"	'Right Sling
SolCallback(13)="vpmSolSound SoundFX(""LeftSlingShot"",DOFContactors),"		'Left Sling	
SolCallback(14)="vpmSolGate LeftGate,SoundFX(""DiverterOn"",DOFContactors),"	'Left ControlGate
SolCallback(15)="vpmSolGate RightGate,SoundFX(""DiverterOn"",DOFContactors),"	'Right ControlGate
SolCallback(16)="dtTotem.SolDropDown"										'TotemDropDown
SolCallback(17)="SetLamp 17,"												'Insert:Eternal Life
SolModCallback(18)="SetModLamp 18,"											'Flasher:Light JackPot
SolCallback(19)="SetLamp 19,"												'Insert:Super Jackpot	
SolModCallback(20)="SetModLamp 20,"												'Flasher:JackPot
SolModCallback(21)="SetModLamp 21,"												'Flasher:Path of Adventure
SolCallback(22)="PoAMoveLeft"												'L_PoA (*)
SolCallback(23)="PoAMoveRight"												'R_PoA (*)
SolModCallback(24)="SetModLamp 24,"											'Flasher:Plane Gun LEDS
SolCallback(25)="SetLamp 25,"												'Insert:Dogfight Hurry up
SolModCallback(26)="SetModLamp 26,"											'Flasher:Right Ramp (x3)
SolModCallback(27)="SetModLamp 27,"											'Flasher:Left Ramp
SolCallback(28)="bsSubway.SolOut"											'SubwayRelease


SolCallback(33)="SolDivPower"												'DivPower
SolCallback(34)="SolDivHold"												'DivHold
SolCallback(35)="SolTopPostPower"											'TopPostPower
SolCallback(36)="SolTopPostHold"											'TopPostHold

SolModCallback(51)="SetModLamp 51,"											'Flasher:Left Side (x2)
SolModCallback(52)="SetModLamp 52,"											'Flasher:Right Side (x2)
SolCallback(53)="SetLamp 53,"												'Insert:Special (x2)
SolCallback(54)="SetLamp 54,"												'Insert:Totem Multi
SolModCallback(55)="SetModLamp 55,"											'Flasher:Jackpot Multi
SolCallback(56)="SolMoveIdol"												'Idol Motor

SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sLRFlipper) = "SolRFlipper"

'************************************************************************
'						 Table Init
'************************************************************************

Dim bsTrough, bsLEject, bsSubway, bsPopper, bsIdol, dtTotem, dtBank, PoAMech

Sub Table1_Init
	vpmInit Me 
	With Controller      
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .Games(cGameName).Settings.Value("rol") = 0
		.SplashInfoLine = "Indiana Jones - The Pinball Adventure (Williams 1993)"
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
		.DIP(0)=&H00	'set dipswitch to USA
		.Switch(22) = 1 'close coin door
		.Switch(24) = 0 'always closed
	End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Nudging
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

	'Trough
    Set bsTrough = New cvpmTrough
    With bsTrough
		.Size = 6
		.InitSwitches Array(86, 85, 84, 83, 82, 81)
		.InitExit BallRelease, 70, 15
		.InitEntrySounds "fx_drain", SoundFX(SSolenoidOn,DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
		.InitExitSounds  SoundFX(SSolenoidOn,DOFContactors), SoundFX("BallRelease",DOFContactors)
		.Balls = 6
    End With

	'Left Eject
    Set bsLEject = new cvpmSaucer
    With bsLEject
        .InitKicker Sw31, 31, 170, 12, 0
        .InitSounds "fx_saucerHit", SoundFX("LeftEject",DOFContactors), SoundFX("LeftEject",DOFContactors)
        .CreateEvents "bsLEject", Sw31
    End With

	'Subway Eject
    Set bsSubway = new cvpmSaucer
    With bsSubway
        .InitKicker sw47, 47, 90, 10, 0
        .InitSounds "", SoundFX("SubWayRelease",DOFContactors), SoundFX("SubWayRelease",DOFContactors)
        .CreateEvents "bsSubway", Sw47
    End With

	'Subway Popper Eject
    Set bsPopper = new cvpmSaucer
    With bsPopper
        .InitKicker Sw44, 44, 0, 32, 1.56
        .InitSounds "", SoundFX("Ball Popper",DOFContactors), SoundFX("Ball Popper",DOFContactors)
        .CreateEvents "bsPopper", Sw44
    End With

	'Idol Eject
    Set bsIdol = New cvpmTrough
    With bsIdol
		.Size = 3
		.InitSwitches Array(0, 0, 0)
		.InitExit IdolExit, 180, 0
		.Balls = 0
		.CreateEvents "bsIdol", IdolEnter
    End With

	'Totem Drop Target
	Set dtTotem = New cvpmDropTarget
	With dtTotem
		.InitDrop sw11, 11
		.Initsnd SoundFX("",DOFDropTargets), SoundFX("TotemDropUp",DOFDropTargets)
	End With

	'3-bank Drop Target
	Set dtBank = New cvpmDropTarget
	With dtBank
		.InitDrop Array(sw115,sw116,sw117), Array(115,116,117)
		.Initsnd SoundFX("",DOFDropTargets), SoundFX("CenterDropBankUp",DOFDropTargets)
'		.Initsnd SoundFX("CenterDropBankDown",DOFDropTargets), SoundFX("CenterDropBankUp",DOFDropTargets)
	End With

	'Path of Adventure
	Set POAMech=New cvpmMech
	With POAMech
	 .MType = vpmMechTwoDirSol + vpmMechStopEnd + vpmMechLinear
	 .Sol1=23
	 .Sol2=22
	 .Length=9
	 .Steps=9
	 .AddSw 124,0,0
	 .AddSw 125,8,8
	 .CallBack=GetRef("UpdatePoA")
	 .ACC=1
	 .RET=1
	 .Start
	End With

	'Init Captive Ball
	CapKicker.CreateSizedBallWithMass Ballsize/2, BallMass:CapKicker.kick 0,0 :CapKicker.enabled=0

	'Other Suff
	InitPoA:InitIdol:InitLamps:InitRolling:InitOptions
	DiverterOn.IsDropped=1
End Sub

'************************************************************************
'							Keys
'************************************************************************

Sub Table1_KeyDown(ByVal Keycode)
	If Keycode= keyFront Then Controller.Switch(12)=1		'buy-in
	If keycode = PlungerKey Then Controller.Switch(34) = 1
	If keycode = LeftTiltKey Then Nudge 90, 2:PlaySound SoundFX("fx_nudge",0)
	If keycode = RightTiltKey Then Nudge 270, 2:PlaySound SoundFX("fx_nudge",0)
	If keycode = CenterTiltKey Then Nudge 0, 4:PlaySound SoundFX("fx_nudge",0)
	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal Keycode)
	If Keycode = keyFront Then Controller.Switch(12)=0		'buy-in
    If keycode = PlungerKey Then Controller.Switch(34) = 0
    If vpmKeyUp(keycode) Then Exit Sub
End Sub

Sub Table1_Paused:Controller.Pause = True:End Sub
Sub Table1_unPaused:Controller.Pause = False:End Sub
Sub Table1_exit():Controller.Pause = False:Controller.Stop:End Sub

'************************************************************************
'						 Solenoids
'************************************************************************

'*********** Flippers
Sub SolLFlipper(Enabled)
    If Enabled AND BSTrough.Balls < 6 Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers),LeftFlipper
        LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers),LeftFlipper
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled AND BSTrough.Balls < 6 Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers),RightFlipper
        RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers),RightFlipper
        RightFlipper.RotateToStart
    End If
End Sub

'*********** AutoPlunger
Sub AutoPlunger(Enabled)
    If Enabled Then
       Plunger.Fire
       PlaySoundAt SoundFX("Autoplunger",DOFContactors),Plunger
	End If
End Sub

'*********** BallRelease
Sub SolBallRelease(Enabled)
    If Enabled Then
        bsTrough.ExitSol_On
        If bsTrough.Balls Then vpmTimer.PulseSw 87
    End If
End Sub

'*********** Diverter
Dim DiverterDir

Sub SolDivPower(Enabled)
	If Enabled Then
		DiverterOff.IsDropped=1
		DiverterOn.IsDropped=0
		DiverterDir = 1
		Diverter.Interval = 5:Diverter.Enabled = 1
		PlaySoundAt SoundFX("DiverterOn",DOFContactors),DiverterP
	End If
End Sub

Sub SolDivHold(Enabled)
	If NOT Enabled AND DiverterDir = 1 Then
		DiverterOff.IsDropped=0
		DiverterOn.IsDropped=1
		DiverterDir = -1
		Diverter.Interval = 5:Diverter.Enabled = 1
		PlaySoundAt SoundFX("DiverterOff",DOFContactors),DiverterP
    End If
End Sub

Sub Diverter_Timer()
DiverterP.RotZ=DiverterP.RotZ+DiverterDir
If DiverterP.RotZ>2 AND DiverterDir=1 Then Me.Enabled=0:DiverterP.RotZ=2
If DiverterP.RotZ<-30 AND DiverterDir=-1 Then Me.Enabled=0:DiverterP.RotZ=-30
End Sub

'*********** Idol Motorized Toy
Dim IdolPos, Ipos, IW

Sub InitIdol
	Controller.Switch(121)=1:Controller.Switch(122)=0:Controller.Switch(123)=0
End Sub

Sub SolMoveIdol(enabled)
	If Enabled Then
		UpdateIdol.enabled = True
		DOF 102, DOFOn
		PlaySoundAt SoundFX("IdolMotor",DOFGear),IdolEnter
	Else
		UpdateIdol.enabled = False
		ResetIdol.enabled = True
	End If
End Sub

Sub UpdateIdol_timer
	ResetIdol.Enabled = False
	IdolPos=(IdolPos+1)Mod 360
	totem.rotz= -IdolPos:totem1.rotz = totem.rotz
	Select Case IdolPos											'91		'92		'93
		Case 0:Controller.Switch(122)=0:IPos=0			'Pos 1	  1      0       0
		Case 60:Controller.Switch(123)=1:IPos=1			'Pos 2	  1      0       1    
		Case 120:Controller.Switch(121)=0:IPos=2		'Pos 3    0      0       1
		Case 180:Controller.Switch(122)=1:IPos=3		'Pos 4    0      1       1
		Case 240:Controller.Switch(123)=0:IPos=4		'Pos 5    0      1       0    
		Case 300:Controller.Switch(121)=1:IPos=5		'Pos 6    1      1       0
	End Select
End Sub

Sub ResetIdol_timer
	If totem.rotz< -60 * Ipos Then totem.rotz = totem.rotz + 1:totem1.rotz = totem.rotz
	If totem.rotz = -60 * Ipos Then Me.Enabled=0:IdolPos=-totem.rotz:StopSound "IdolMotor":DOF 102, DOFOff
End Sub

'*********** Idol Kickout
Sub SolIdol(Enabled)
	If Enabled Then
		IdolStop.IsDropped=1:LockDoor1.Z=-55:LockDoor2.Z=-55:LockDoor3.Z=-55:PlaysoundAt SoundFX("IdolReleaseOn",DOFContactors),IdolExit:bsIdol.ExitSol_On
	Else
		IdolStop.IsDropped=0:LockDoor1.Z=0:LockDoor2.Z=0:LockDoor3.Z=0:PlaysoundAt SoundFX("IdolReleaseOff",DOFContactors),IdolExit
	End If
End Sub

'*********** Path Of Adventure
Dim movePoA,PoAPos,PoADropTrack,MyBall,Ballspeed

Sub InitPoA
	PoADropTrack=0:PoaPos=0:Ballspeed=0:TopPost.Z=0
	Controller.Switch (124) = 0 : Controller.Switch (125) = 0
End Sub

Sub PoaMoveLeft(enabled)
	If enabled then
		movePoA=0
	Else
		ResetPoA.Enabled = 1
	End If
End Sub

Sub PoAMoveRight(enabled)
	If Enabled Then
		movePoA=0
	Else
		ResetPoA.Enabled = 1
	End If
End Sub

Sub ResetPoA_timer
	Dim ii
	movePoA=movePoA+1
	If NOT Controller.Switch (124) AND NOT Controller.Switch (125) AND movePoA > 50 AND NOT PoAPos=0 Then
		PoAPos = 0: Me.Enabled=0
		minipf.roty=PoAPos:minipf1.roty=PoAPos:minipf2.roty=PoAPos:minipf3.roty=PoAPos:minipf4.roty=PoAPos:minipf5.roty=PoAPos:minipf_screws.roty=-PoAPos
		li71.rotY=PoAPos:li72.rotY=PoAPos:li73.rotY=PoAPos:li74.rotY=PoAPos:li75.rotY=PoAPos
		li81.rotY=PoAPos:li82.rotY=PoAPos:li83.rotY=PoAPos:li84.rotY=PoAPos:li85.rotY=PoAPos
		sw65p.roty=PoAPos:sw66p.rotY=PoAPos:sw67p.rotY=PoAPos:sw68p.rotY=PoAPos
		sw75p.roty=PoAPos:sw76p.rotY=PoAPos:sw77p.rotY=PoAPos:sw78p.rotY=PoAPos
		For each ii in GIPOA:ii.rotY=PoAPos:Next
	End If
End Sub

Sub UpdatePOA(oldPos,newPos,aspeed)
	Dim ii
	PoAPos=2*(POAMech.Position-4)
	minipf.roty=PoAPos:minipf1.roty=PoAPos:minipf2.roty=PoAPos:minipf3.roty=PoAPos:minipf4.roty=PoAPos:minipf5.roty=PoAPos:minipf_screws.roty=PoAPos:POASh.transX=PoaPos
	li71.rotY=PoAPos:li72.rotY=PoAPos:li73.rotY=PoAPos:li74.rotY=PoAPos:li75.rotY=PoAPos
	li81.rotY=PoAPos:li82.rotY=PoAPos:li83.rotY=PoAPos:li84.rotY=PoAPos:li85.rotY=PoAPos
	sw65p.roty=PoAPos:sw66p.rotY=PoAPos:sw67p.rotY=PoAPos:sw68p.rotY=PoAPos
	sw75p.roty=PoAPos:sw76p.rotY=PoAPos:sw77p.rotY=PoAPos:sw78p.rotY=PoAPos
	For each ii in GIPOA:ii.rotY=PoAPos:Next
End Sub

Sub EnterPoA_hit:Me.TimerInterval=10:Me.TimerEnabled=1:Playsound "fx_balldrop",0,1,-.2,0,0,1,0,-.8:POABallShadow.visible=1:End Sub
Sub EnterPoA_timer
	If NOT IsEmpty (myball) Then
		myball.velx = myball.velx + 0.5 * Sgn(PoAPos)
		If myball.VelY<0 Then myball.VelY=1
		POABallShadow.X = (myball.X - (Ballsize/6) + ((myball.X - (Table1.Width/2))/7)) + 10
		POABallShadow.Y = myball.Y + 20
		POABallShadow.Z = 156
	End If
End Sub

Sub ExitPoA_hit:myball=empty:EnterPOA.TimerEnabled=0:Me.TimerInterval=100:Me.TimerEnabled=1:End Sub
Sub ExitPoA_timer:Me.TimerEnabled=0:Playsound "fx_ramp_metal",0,1,-.2,0,0,1,0,-.8:POABallShadow.visible=0:POABallShadow.X=182:POABallShadow.Y=130:End Sub

Sub ExitBridge_hit:myball=empty:EnterPOA.TimerEnabled=0:PlaysoundAt "fx_ramp_turn",ExitBridge:End Sub

'*********** Top Post
 Sub SolTopPostPower(Enabled)
	If Enabled Then
		If POADropTrack=1 Then Enabled=0
		DropPoA.IsDropped=1
		sw46.Kick 270,5
		TopPost.Z=-30
		If POADropTrack=0 Then PlaysoundAt SoundFX("TopPostDown",DOFContactors),TopPost
	Else
		DropPoA.IsDropped=0
		TopPost.Z=-30
		If POADropTrack=0 Then DivHelp.TimerInterval=400:DivHelp.TimerEnabled=1
	End If
End Sub

Sub SolTopPostHold(Enabled)
	If Enabled Then 
		POADropTrack=1	
		DivHelp.IsDropped=1
		DropPoA.IsDropped=0
		TopPost.Z=-30:PlaysoundAt SoundFX("TopPostDown",DOFContactors),TopPost
	Else
		DivHelp.TimerInterval=400
		DivHelp.TimerEnabled=1
	End If
End Sub

Sub DivHelp_timer
	Me.TimerEnabled=0
	POADropTrack=0
	DivHelp.IsDropped=0
	TopPost.Z=0:PlaysoundAt SoundFX("TopPostUp",DOFContactors),TopPost
End Sub

Sub sw46_Hit()
	Set myball=ActiveBall
	BallSpeed=myBall.VelX
	If POADropTrack = 0 Then StopSound "fx_ramp_enter3"
	If POADropTrack = 1 Then sw46.Kick 270,ABS(Ballspeed)
	Controller.Switch(46)=1
End Sub

Sub sw46_UnHit():Controller.Switch(46) = 0:End Sub

'************************************************************************
'						Switches
'************************************************************************

Sub Drain_hit
	bsTrough.AddBall Me
	Dim BOT:BOT=GetBalls
	If Ubound(BOT)=0 Then LeftFlipper.RotateToStart:RightFlipper.RotateToStart
End Sub

'*********** Drop Targets
Sub sw11_dropped:dtTotem.Hit 1:End Sub

Sub sw115_dropped:dtBank.Hit 1:End Sub
Sub sw116_dropped:dtBank.Hit 2:End Sub
Sub sw117_dropped:dtBank.Hit 3:End Sub

Sub sw11_hit:PlaysoundAtVol "DropTargetHit",sw11,.8:PlaySoundAtVol "TotemDropDown",sw11,1:End Sub
Sub sw115_hit:PlaysoundAt "DropTargetHit",ActiveBall:PlaysoundAt "CenterDropBankDown",ActiveBall:End Sub
Sub sw116_hit:PlaysoundAt "DropTargetHit",ActiveBall:PlaysoundAt "CenterDropBankDown",ActiveBall:End Sub
Sub sw117_hit:PlaysoundAt "DropTargetHit",ActiveBall:PlaysoundAt "CenterDropBankDown",ActiveBall:End Sub

'*********** Rollovers
Sub Sw15_Hit:Controller.Switch(15)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw15_UnHit:Controller.Switch(15)=0: End Sub
Sub Sw16_Hit:Controller.Switch(16)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw16_UnHit:Controller.Switch(16)=0: End Sub
Sub Sw17_Hit:Controller.Switch(17)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw17_UnHit:Controller.Switch(17)=0: End Sub
Sub Sw18_Hit:Controller.Switch(18)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw18_UnHit:Controller.Switch(18)=0: End Sub

Sub Sw25_Hit:Controller.Switch(25)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw25_UnHit:Controller.Switch(25)=0: End Sub
Sub Sw26_Hit:Controller.Switch(26)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw26_UnHit:Controller.Switch(26)=0: End Sub
Sub Sw27_Hit:Controller.Switch(27)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw27_UnHit:Controller.Switch(27)=0: End Sub
Sub Sw28_Hit:Controller.Switch(28)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw28_UnHit:Controller.Switch(28)=0: End Sub

Sub Sw32_Hit:Controller.Switch(32)=1:Activeball.VelY=1: PlaySoundAtBallVol "fx_sensor",1: End Sub
Sub Sw32_UnHit:Controller.Switch(32)=0: End Sub

Sub Sw54_Hit:Controller.Switch(54)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw54_UnHit:Controller.Switch(54)=0: End Sub
Sub Sw55_Hit:Controller.Switch(55)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw55_UnHit:Controller.Switch(55)=0: End Sub
Sub Sw56_Hit:Controller.Switch(56)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw56_UnHit:Controller.Switch(56)=0: End Sub
Sub Sw57_Hit:Controller.Switch(57)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw57_UnHit:Controller.Switch(57)=0: End Sub
Sub Sw58_Hit:Controller.Switch(58)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw58_UnHit:Controller.Switch(58)=0: End Sub

Sub Sw88_Hit:Controller.Switch(88)=1: PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw88_UnHit:Controller.Switch(88)=0: End Sub

'*********** Slingshots
Dim LStep, RStep

Sub LeftSlingShot_Slingshot()
    LSling1.Visible = 1
    Lemk.TransZ = -20
    LStep = 0
    vpmTimer.PulseSw 33
    Me.TimerInterval = 20:Me.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer()
    Select Case LStep
        Case 1:LSLing1.Visible = 0:LSLing2.Visible = 1:Lemk.TransZ = -10
        Case 2:LSLing2.Visible = 0:Lemk.TransZ = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot()
    RSling1.Visible = 1
    Remk.TransZ = -20
    RStep = 0
    vpmTimer.PulseSw 48
    Me.TimerInterval = 20:Me.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer()
    Select Case RStep
        Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:Remk.TransZ = -10
        Case 2:RSLing2.Visible = 0:Remk.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'*********** Bumpers
Sub Bumper1_Hit():vpmTimer.PulseSw 35:PlaySoundAtBumperVol "LeftJet",Bumper1,2:End Sub
Sub Bumper2_Hit():vpmTimer.PulseSw 36:PlaySoundAtBumperVol "RightJet",Bumper2,2:End Sub
Sub Bumper3_Hit():vpmTimer.PulseSw 37:PlaySoundAtBumperVol "BottomJet",Bumper3,2:End Sub

'*********** Center Standup Target
Sub Sw38_Hit():vpmTimer.PulseSw 38: PlaysoundAt SoundFX("fx_target",DOFTargets),ActiveBall: End Sub

'*********** Left Ramp
Sub Sw41_Hit():vpmtimer.pulseSw 41:End Sub 
Sub Sw118_Hit()
vpmtimer.pulseSw 118:sw118p.Rotz=-50: Me.TimerEnabled=1:PlaysoundAt "fx_sensor",sw118
If PropellerMod=1 Then RotatePropeller
End Sub
Sub Sw118_timer:Me.TimerEnabled=0:sw118p.Rotz=-20:End Sub

'******	 Propeller MOD
Dim stepangle

Sub RotatePropeller()
	PlaysoundAt SoundFXDOF("fx_motor",101,DOFOn,DOFGear),Bumper3
	PropellerMove.Enabled = 0
	PropellerMove.Interval = 10
	PropellerMove.Enabled = 1
	stepAngle=10
End Sub

Sub PropellerMove_Timer()
	Propeller.roty = Propeller.roty + stepAngle
	If Propeller.roty >= 6*360 Then stepAngle = stepAngle - 0.2
	If stepAngle <= 0 Then Me.Enabled = 0 : Propeller.roty = Propeller.roty -6*360 : StopSound "fx_motor" : DOF 101, DOFOff
End Sub

'*********** Right Ramp
Sub Sw42_Hit():vpmtimer.pulseSw 42:End Sub 
Sub Sw74_Hit():vpmtimer.pulseSw 74:sw74p.Rotz=-30:Me.TimerEnabled=1:PlaysoundAt "fx_sensor",sw74: End Sub
Sub Sw74_timer:Me.TimerEnabled=0:sw74p.Rotz=0:End Sub

'*********** Idol Enter
Sub sw43_hit():vpmtimer.pulseSw 43:End Sub

'*********** Subway Enter
Sub Subwayhelp_hit:Me.Kick 30,8:End Sub
Sub sw45_hit():vpmtimer.pulseSw 45:SoundHole45:End Sub
'Sub sw45_hit():vpmtimer.pulseSw 45:RandomSoundHole:End Sub

'*********** Captive Ball Target
Sub Sw64_Hit():vpmTimer.PulseSw 64: PlaysoundAtBall SoundFX("fx_target",DOFTargets): End Sub

'*********** Captive Ball Opto
Sub Sw71_Hit:Controller.Switch(71) = 1:End Sub 
Sub Sw71_UnHit:Controller.Switch(71) = 0:End Sub

'*********** Adventure Targets
Sub Sw51_Hit():vpmTimer.PulseSw 51: PlaySoundAt SoundFX("fx_target",DOFContactors),ActiveBall: End Sub	'(U)
Sub Sw52_Hit():vpmTimer.PulseSw 52: PlaySoundAt SoundFX("fx_target",DOFContactors),ActiveBall: End Sub	'(R)
Sub Sw53_Hit():vpmTimer.PulseSw 53: PlaySoundAt SoundFX("fx_target",DOFContactors),ActiveBall: End Sub	'(E)

Sub Sw61_Hit():vpmTimer.PulseSw 61: PlaySoundAt SoundFX("fx_target",DOFContactors),ActiveBall: End Sub	'(A)
Sub Sw62_Hit():vpmTimer.PulseSw 62: PlaySoundAt SoundFX("fx_target",DOFContactors),ActiveBall: End Sub	'(D)
Sub Sw63_Hit():vpmTimer.PulseSw 63: PlaySoundAt SoundFX("fx_target",DOFContactors),ActiveBall: End Sub	'(V)

'*********** Path of Adventure
Sub Sw65_Hit:vpmTimer.PulseSw 65:sw65p.rotx=-20:Me.TimerEnabled=1:PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw65_timer:Me.TimerEnabled=0:sw65p.rotx=0:End Sub
Sub Sw66_Hit:vpmTimer.PulseSw 66:sw66p.rotx=-20:Me.TimerEnabled=1:PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw66_timer:Me.TimerEnabled=0:sw66p.rotx=0:End Sub
Sub Sw67_Hit:vpmTimer.PulseSw 67:sw67p.rotx=-20:Me.TimerEnabled=1:PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw67_timer:Me.TimerEnabled=0:sw67p.rotx=0:End Sub
Sub Sw68_Hit:vpmTimer.PulseSw 68:sw68p.rotx=-20:Me.TimerEnabled=1:PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw68_timer:Me.TimerEnabled=0:sw68p.rotx=0:End Sub

Sub sw72_hit():vpmtimer.pulseSw 72:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub sw72_timer():Me.TimerEnabled=0:SoundHole72:myball=empty:EnterPOA.TimerEnabled=0:End Sub
Sub sw73_hit():vpmtimer.pulseSw 73:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub sw73_timer():Me.TimerEnabled=0:SoundHole73:myball=empty:EnterPOA.TimerEnabled=0:End Sub

Sub Sw75_Hit:vpmTimer.PulseSw 75:sw75p.rotx=-20:Me.TimerEnabled=1:PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw75_timer:Me.TimerEnabled=0:sw75p.rotx=0:End Sub
Sub Sw76_Hit:vpmTimer.PulseSw 76:sw76p.rotx=-20:Me.TimerEnabled=1:PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw76_timer:Me.TimerEnabled=0:sw76p.rotx=0:End Sub
Sub Sw77_Hit:vpmTimer.PulseSw 77:sw77p.rotx=-20:Me.TimerEnabled=1:PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw77_timer:Me.TimerEnabled=0:sw77p.rotx=0:End Sub
Sub Sw78_Hit:vpmTimer.PulseSw 78:sw78p.rotx=-20:Me.TimerEnabled=1:PlaySoundAt "fx_sensor",ActiveBall: End Sub
Sub Sw78_timer:Me.TimerEnabled=0:sw78p.rotx=0:End Sub

' *********************************************************************
'						Lighting
' *********************************************************************

Dim FadingLevel(200), LampState(200)

Sub InitLamps
	On Error Resume Next
	Dim i
	For i=11 To 88: Execute "Lights(" & i & ")  = Array (Light" & i & ",Light" & i & "a)": Next
	For i=0 to 200:LampState(i) = 0:FadingLevel(i) = 0:Next
	LampTimer.Interval = 10:LampTimer.Enabled = 1
End Sub

Sub LampTimer_Timer()
' Flashers
	FadeLamp 17, FL_ELife	
	FadeModLamp 18, FL_LJackpotA, 2
	FadeModLamp 18, FL_LJackpotB, 2
	FadeLamp 19, FL_SJackpot	
	FadeModLamp 20, FL_Jackpot, 2
	FadeModLamp 21, FL_POA, 2
	FadeModLamp 24, FL_PlaneGunsA, 2
	FadeModLamp 24, FL_PlaneGunsB, 2
	FadeLamp 25, FL_Dogfight	
	FadeModLamp 26, FL_RRamp, 2
	FadeModLamp 26, FL_RRamp2, 2
	FadeModLamp 26, FL_RRamp3, 2
	FadeModLamp 26, FL_RRampa, 2
	FadeModLamp 26, FL_RRampb, 2
	FadeModLamp 26, FL_RRampbDT, 2
	FadeModLamp 26, FL_RRampr, 2
	FadeModLamp 26, FL_RRampr1, 2
	FadeModLamp 26, FL_RRampr2, 2
	FadeModLamp 27, FL_LRamp, 2
	FadeModLamp 27, FL_LRampa, 2
	FadeModLamp 27, FL_LRampb, 2
	FadeModLamp 27, FL_RRampa, 2
	FadeModLamp 27, FL_LRampbDT, 2
	FadeModLamp 27, FL_LRampr, 2
	FadeModLamp 27, FL_LRampr1, 2
	FadeModLamp 27, FL_LRampr2, 2
	FadeModLamp 27, FL_LRampr3, 2
	FadeModLamp 27, FL_LRampr4, 2
	FadeModLamp 27, FL_LRampr5, 2
	FadeModLamp 51, LeftSideFlashA, 2
	FadeModLamp 51, LeftSideFlashB, 2
	FadeModLamp 52, RightSideFlash, 2
	FadeModLamp 52, RightSideFlash1, 2
	FadeModLamp 52, RightSideFlasha, 2
	FadeModLamp 52, RightSideFlashb, 2
	FadeModLamp 52, RightSideFlashr, 2
	FadeModLamp 52, RightSideFlashr1, 2
	FadeModLamp 52, RightSideFlashr2, 2
	FadeModLamp 52, RightSideFlashr3, 2
	FadeLamp 53, FL_SpecialL
	FadeLamp 53, FL_SpecialR		
	FadeLamp 54, FL_TotemMulti	
	FadeModLamp 55, FL_JackpotMultiA, 2
	FadeModLamp 55, FL_JackpotMultiB, 2
'i-n-D-Y inserts burst
	AddBurst 63, l63r
	AddBurst 64, l64r
'Path of Adventure inserts
	SwapImageAndMat 71,Li71,"PoALight_on","PoALight_off","PoAInsertsOn","PoAInsertsOff"
	SwapImageAndMat 72,Li72,"PoALight_on","PoALight_off","PoAInsertsOn","PoAInsertsOff"
	SwapImageAndMat 73,Li73,"PoALight_on","PoALight_off","PoAInsertsOn","PoAInsertsOff"
	SwapImageAndMat 74,Li74,"PoALight_on","PoALight_off","PoAInsertsOn","PoAInsertsOff"
	SwapImageAndMat 75,Li75,"PoAThePit_On","PoAThePit_Off","PoAInsertsOn","PoAInsertsOff"
	SwapImageAndMat 81,Li81,"PoALight_on","PoALight_off","PoAInsertsOn","PoAInsertsOff"
	SwapImageAndMat 82,Li82,"PoALight_on","PoALight_off","PoAInsertsOn","PoAInsertsOff"
	SwapImageAndMat 83,Li83,"PoALight_on","PoALight_off","PoAInsertsOn","PoAInsertsOff"
	SwapImageAndMat 84,Li84,"PoALight_on","PoALight_off","PoAInsertsOn","PoAInsertsOff"
	SwapImageAndMat 85,Li85,"PoAEB_on","PoAEB_off","PoAInsertsOn","PoAInsertsOff"
End Sub

Sub SetLamp(nr, enabled)
        LampState(nr) = enabled
End Sub

Sub SetModLamp(nr, value)
    If value > 0 Then
		LampState(nr) = 1
	Else
		LampState(nr) = 0
	End If
	FadingLevel(nr) = value
End Sub

Sub FadeLamp(nr, object)
	If TypeName(object) = "Light" Then
		Object.State = LampState(nr)
	End If
	If TypeName(object) = "Flasher" Then
		Object.visible = LampState(nr)
	End If
End Sub

Sub FadeModLamp(nr, object, factor)
	Object.IntensityScale = FadingLevel(nr) * factor/255
	If TypeName(object) = "Light" Then
		Object.State = LampState(nr)
	End If
	If TypeName(object) = "Flasher" Then
		Object.visible = LampState(nr)
	End If
End Sub

Sub AddBurst(nr,Object)
	object.visible = Controller.Lamp(nr)
End Sub

Sub SwapImageAndMat(nr,object,imageOn,imageOff,matOn,matOff)
	Select Case ABS(Cint(Controller.Lamp(nr)))
	Case 0 		'off
		object.image= imageOff:object.material= matOff
	Case 1		'on
		object.image= imageOn:object.material= matOn
	End Select
End Sub

'*********** General Illumination
Set GiCallBack2 = GetRef("UpdateGi")

Sub UpdateGi(nr,step)
	Dim ii
	Select Case nr
	Case 0 		'Top Playfield
		If step=0 Then
			DOF 103, DOFOff
			For each ii in GITop:ii.state=0:Next
			For each ii in GITopSides:ii.visible=0:Next
			For each ii in GIBumpers:ii.state=0:Next
			For each ii in GIPOA:ii.visible=0:Next
		Else
			DOF 103, DOFOn
			For each ii in GITop:ii.state=1:Next
			For each ii in GITopSides:ii.visible=1:Next
			For each ii in GIBumpers:ii.state=1:Next
			For each ii in GIPOA:ii.visible=1:Next
		End If
		For each ii in GITopSides:ii.IntensityScale = 0.125 * step:Next
		For each ii in GITop:ii.IntensityScale = 0.125 * step:Next
		For each ii in GIBumpers:ii.IntensityScale = 0.125 * step:Next
		For each ii in GIPOA:ii.IntensityScale = 0.125 * step:Next
	Case 1		'Bottom Playfield
		If step=0 Then
			For each ii in GIBot:ii.state=0:Next
			For each ii in GIBotSides:ii.visible=0:Next
		Else
			For each ii in GIBot:ii.state=1:Next
			For each ii in GIBotSides:ii.visible=1:Next
		End If
		For each ii in GIBot:ii.IntensityScale = 0.125 * step:Next
		For each ii in GIBotSides:ii.IntensityScale = 0.125 * step:Next
		If Step>=7 Then Table1.ColorGradeImage = "ColorGrade_8":Else Table1.ColorGradeImage = "ColorGrade_" & (step+1):End If
	Case 2		'Insert Top
	Case 3		'Insert Bottom
	Case 4		'Return Lane/coin
		If step=0 Then
			LiteHOF_L.state=0:LiteHOF_R.state=0
		Else
			LiteHOF_L.state=1:LiteHOF_R.state=1
		End If
		LiteHOF_L.IntensityScale = 0.125 * step:LiteHOF_R.IntensityScale = 0.125 * step
	End Select
End Sub

' *********************************************************************
'					Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 400)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

function AudioFade(ball)
	Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function RndNum(min,max)
 RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min and max
End Function

' *********************************************************************
' 						Other Sound FX
' *********************************************************************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 50, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub LeftFlipper_Collide(parm)
    RandomSoundFlipper
End Sub

Sub Rightflipper_Collide(parm)
    RandomSoundFlipper
End Sub

Sub RandomSoundFlipper()
	Select Case RndNum(1,3)
		Case 1 : PlaySound "fx_flip_hit_1", 0, 20*Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0,AudioFade(ActiveBall)
		Case 2 : PlaySound "fx_flip_hit_2", 0, 20*Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0,AudioFade(ActiveBall)
		Case 3 : PlaySound "fx_flip_hit_3", 0, 20*Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0,AudioFade(ActiveBall)
	End Select
End Sub

Sub Gates_Hit (idx)
	PlaySoundAtBallVol "fx_gate4",1
End Sub

Sub Posts_Hit(idx)
		 RandomSoundRubber()
End Sub

Sub Rubbers_Hit(idx)
		 RandomSoundRubber()
End Sub

Sub  RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtBallVol "fx_rubber_hit_1",2
		Case 2 : PlaySoundAtBallVol "fx_rubber_hit_2",2
		Case 3 : PlaySoundAtBallVol "fx_rubber_hit_3",2
	End Select
End Sub

Sub SoundHole45()
	PlaySoundAtVol "fx_hole3",sw45,.5
End Sub 

Sub SoundHole72()
	PlaySoundAtVol "fx_hole3",sw72,.2
End Sub 

Sub SoundHole73()
	PlaySoundAtVol "fx_hole3",sw73,.2
End Sub 

' *********************************************************************
' 					Ball Drop & Ramp Sounds
' *********************************************************************

Sub SubwayEnter_Hit:PlaysoundAt "fx_plasticrolling",SubwayEnter:End Sub
Sub SubwayExit_hit:StopSound "fx_plasticrolling":PlaysoundAt "fx_kickerstop",SubwayExit:End Sub

Sub ShooterStart_Hit():StopSound "fx_launchball":If ActiveBall.VelY < 0 Then PlaySoundAt "fx_launchball",ShooterStart:End If:End Sub	'ball is going up
Sub ShooterEnd_Hit:If ActiveBall.Z > 30  Then Me.TimerInterval=100:Me.TimerEnabled=1:End If:End Sub						'ball is flying
Sub ShooterEnd_Timer(): Me.TimerEnabled=0 : PlaySound "fx_balldrop",0,2,.2,0,0,0,1,-.6 : End Sub

Sub LREnter_Hit():If ActiveBall.VelY < 0 Then PlaySoundAtVol "fx_lrenter",LREnter,.2:End If:End Sub			'ball is going up
Sub LREnter_UnHit():If ActiveBall.VelY > 0 Then StopSound "fx_lrenter":End If:End Sub		'ball is going down
Sub LREnter1_Hit():StopSound "fx_lrenter":PlaySoundAtVol "fx_ramp_turn",LREnter1,.2:End Sub
Sub LREnter2_Hit():StopSound "fx_ramp_turn":End Sub
Sub LRExit_Hit():ActiveBall.VelY=1:PlaySoundAt "fx_balldrop",LRExit:End Sub

Sub RREnter_Hit():If ActiveBall.VelY < 0 Then PlaySoundAtVol "fx_ramp_enter1",RREnter,.2:End If:End Sub			'ball is going up
Sub RREnter_UnHit():If ActiveBall.VelY > 0 Then StopSound "fx_ramp_enter1":End If:End Sub		'ball is going down
Sub RREnter1_Hit():PlaySoundAtVol "fx_ramp_enter2",RREnter1,.2:End Sub
Sub RREnter2_Hit():PlaySoundAtVol "fx_ramp_enter2",RREnter2,.2:End Sub
Sub RREnter3_Hit():StopSound "fx_ramp_enter2":End Sub
Sub RRExit_Hit():ActiveBall.VelY=1:PlaysoundAt "fx_balldrop",ActiveBall:End Sub

Sub BREnter_Hit():StopSound "fx_ramp_enter2":PlaySoundAtVol "fx_ramp_enter3",BREnter,.2:End Sub
Sub BRExit_Hit():ActiveBall.VelY=1:PlaysoundAt "fx_balldrop",ActiveBall:End Sub

' *********************************************************************
' 			Left and Right Orbits Hack
' *********************************************************************

Sub LoopHelpL_Unhit():If ActiveBall.VelY > 20 Then ActiveBall.VelY = RndNum(12,14):End If:End Sub
Sub LoopHelpR_Unhit():If ActiveBall.VelY > 20 Then ActiveBall.VelY = RndNum(12,14):End If:End Sub

' *********************************************************************
' 						RealTime Updates
' *********************************************************************
Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates()
	RollingSoundUpdate
	BallShadowUpdate
	MechsUpdate
End Sub

'*********** Rolling Sound *********************************
Const tnob = 7						' total number of balls : 6 (trough) + 1 (Captive Ball)
Const fakeballs = 0					' number of balls created on table start (rolling sound will be skipped)
ReDim rolling(tnob)

Sub InitRolling:Dim i:For i=0 to (tnob-1):rolling(i) = False:Next:End Sub

Sub RollingSoundUpdate()
    Dim BOT, b
    BOT = GetBalls
	' stop the sound of deleted balls
	If UBound(BOT)<(tnob - 1) Then
		For b = (UBound(BOT) + 1) to (tnob-1)
			rolling(b) = False
			StopSound("fx_ballrolling" & b)
		Next
	End If
	' exit the Sub if no balls on the table
    If UBound(BOT) = fakeballs-1 Then Exit Sub
       ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 Then
			rolling(b) = True
			if BOT(b).z < 30 Then ' Ball on playfield
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*2, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
			Else ' Ball on raised ramp
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
				End If
		Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'*********** Ball Shadow *********************************
Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3,Ballshadow4,Ballshadow5,Ballshadow6,Ballshadow7)
Sub BallShadowUpdate()
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

'*********** Gates Primitives Sync *********************************
Sub MechsUpdate()
	TopGateP.RotX = TopGate.currentangle
	BottomGateP.RotX = BottomGate.currentangle + 10
	LeftGateP.RotY = -LeftGate.currentangle
	RightGateP.RotY = -RightGate.currentangle
	LeftFlipperSh.RotZ = LeftFlipper.currentangle
	RightFlipperSh.RotZ = RightFlipper.currentangle
End Sub

' *********************************************************************
'					Table Options 
' *********************************************************************
Dim InstrChoice, FlipperChoice

Sub InitOptions
	Blimp.visible=BlimpToy
	leftrail.visible = DesktopMode:rightrail.visible = DesktopMode
	FL_LRampbDT.visible=DesktopMode:FL_LRampb.visible=NOT DesktopMode:FL_RRampbDT.visible=DesktopMode:FL_RRampb.visible=NOT DesktopMode
	If DesktopMode Then FL_LRampr1.opacity=500 Else FL_LRampr1.opacity=100
	If TargetDecals=0 Then
		sw38.image="target-slim":sw51.image="target-round":sw52.image="target-round":sw53.image="target-round":sw61.image="target-round":sw62.image="target-round":sw63.image="target-round"
	Else
		sw38.image="target_slim":sw51.image="target_U":sw52.image="target_R":sw53.image="target_E":sw61.image="target_A":sw62.image="target_D":sw63.image="target_V"
	End If		
	If SideCabDecals=0 Then
		LeftCab.image = "sidecabL":RightCab.image = "sidecabR"
	Else
		LeftCab.image = "sidecabL_c":RightCab.image = "sidecabR_c"
	End If
	Select Case InstrCardType
		Case 0
			Card1.image= "IJ-Card1" : Card2.image= "IJ-Card2"
		Case 1
			Card1.image= "IJ-Card1c" : Card2.image= "IJ-Card2c"
		Case 2
			Randomize:InstrChoice = RndNum (0,1)
			Select Case InstrChoice
				Case 0
					Card1.image= "IJ-Card1" : Card2.image= "IJ-Card2"
				Case 1
					Card1.image= "IJ-Card1c" : Card2.image= "IJ-Card2c"
			End Select
	End Select
	Select Case FlipperType
		Case 0
			LeftFlipper.RubberMaterial= "Plastic" : RightFlipper.RubberMaterial= "Plastic"
		Case 1
			LeftFlipper.RubberMaterial= "Rubber Black" : RightFlipper.RubberMaterial= "Rubber Black"
		Case 2
			Randomize:FlipperChoice = RndNum (0,1)
			Select Case FlipperChoice
				Case 0
					LeftFlipper.RubberMaterial= "Plastic" : RightFlipper.RubberMaterial= "Plastic"
				Case 1
					LeftFlipper.RubberMaterial= "Rubber Black" : RightFlipper.RubberMaterial= "Rubber Black"
			End Select
	End Select
	Select Case PropellerMod
		Case 0:Propeller.visible=0:Propeller1.visible=1
		Case 1:Propeller.visible=1:Propeller1.visible=0
	End Select
End Sub

'**************************************************************************
'                 Positional Sound Playback Functions by DJRobX
'**************************************************************************

'Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(sound, tableobj)
		PlaySound sound, 1, 1, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'Set all as per ball position & speed.

Sub PlaySoundAtBall(sound)
		PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub


'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Vol)
		PlaySound sound, 1, Vol, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
		PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub


'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
		PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub


Dim NextOrbitHit:NextOrbitHit = 0 

Sub WireRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump3 .3, Pitch(ActiveBall)+5
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub

Sub PlasticRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 2, -20000
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .1 + (Rnd * .2)
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


'' Requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' Requires metalguidebump1 to 2 in Sound Manager
Sub RandomBump2(voladj, freq)
	dim BumpSnd:BumpSnd= "metalguidebump" & CStr(Int(Rnd*2)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' Requires WireRampBump1 to 5 in Sound Manager
Sub RandomBump3(voladj, freq)
	dim BumpSnd:BumpSnd= "WireRampBump" & CStr(Int(Rnd*5)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub



' Stop Bump Sounds
Sub BumpSTOP1_Hit ()
dim i:for i=1 to 4:StopSound "WireRampBump" & i:next
NextOrbitHit = Timer + 1
End Sub

Sub BumpSTOP2_Hit ()
dim i:for i=1 to 4:StopSound "WireRampBump" & i:next
NextOrbitHit = Timer + 1
End Sub

Sub BumpSTOP3_Hit ()
dim i:for i=1 to 4:StopSound "WireRampBump" & i:next
NextOrbitHit = Timer + 1
End Sub