' __          ___    _ _______ _    _ ______ _____  _____ _   _  _____  __          ____      ________  _____ 
' \ \        / / |  | |__   __| |  | |  ____|  __ \|_   _| \ | |/ ____| \ \        / /\ \    / /  ____|/ ____|
'  \ \  /\  / /| |  | |  | |  | |__| | |__  | |__) | | | |  \| | |  __   \ \  /\  / /  \ \  / /| |__  | (___  
'   \ \/  \/ / | |  | |  | |  |  __  |  __| |  _  /  | | | . ` | | |_ |   \ \/  \/ / /\ \ \/ / |  __|  \___ \ 
'    \  /\  /  | |__| |  | |  | |  | | |____| | \ \ _| |_| |\  | |__| |    \  /\  / ____ \  /  | |____ ____) |
'     \/  \/    \____/   |_|  |_|  |_|______|_|  \_\_____|_| \_|\_____|     \/  \/_/    \_\/   |______|_____/ 
' ****************************************************************
'                       VISUAL PINBALL X
' ****************************************************************
SetLocale 1033
Randomize
'***********************************
' // User Settings in F12 menu //   
'***********************************
Dim ModeChangeBallActive
Dim ModeFlipGlowActive
Dim ModeDifficulty
Dim ModeThunderSound
Dim MaxTableVolume: MaxTableVolume = 100
Dim RailChoice: RailChoice = Cabinet

'----- VR Room -----
Dim VRRoomChoice : VRRoomChoice = 3			  ' 1 - Cab Only, 2 - Minimal Room, 3 - MEGA room
Dim VRTest : VRTest = False

Sub Table1_OptionEvent(ByVal eventId)
	' -- pause processes not needed to run --
    If eventId = 1 Then DisableStaticPreRendering = True
	DMDTimer.Enabled = False ' stop FlexDMD timer
'	Controller.Pause = True

	ModeChangeBallActive = Table1.Option("ChangeColorBall", 0, 1, 1, 0, 0, Array("Yes","No"))
	ModeThunderSound = Table1.Option("Sound Thunder BG", 0, 1, 1, 0, 0, Array("Off","On"))
	ModeFlipGlowActive = Table1.Option("FlipGlow Active", 0, 1, 1, 0, 0, Array("Yes","No"))
	SetFlipGlow ModeFlipGlowActive
	ModeDifficulty = Table1.Option("Use Flipper Peg", 0, 1, 1, 1, 0, Array("No","Yes"))
	SetDifficulty ModeDifficulty

	' -- start paused processes --
	DMDTimer.Enabled = True
'	Controller.Pause = False
    If eventId = 3 Then DisableStaticPreRendering = False

    ' VRRoom
	VRRoomChoice = Table1.Option("VR Room", 1, 3, 1, 3, 0, Array("CabOnly", "Minimal", "MEGA"))
	LoadVRRoom


    RailChoice = Table1.Option("Rails Visible", 0, 1, 1, 1, 0, Array("Cabinet", "SideRails (Default)"))
	SetRails RailChoice
	MaxTableVolume = Table1.Option("Music Volume", 1, 7, 1, 1, 0, Array("Level 100%", "Level 95%", "Level 90%", "Level 85%", "Level 80%", "Level 75%", "Level 70%"))
	SetVolMusic MaxTableVolume
    VolumeDial = Table1.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
    BallRollVolume = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.8, 1)
    RampRollVolume = Table1.Option("Ramp Roll Volume", 0, 1, 0.01, 0.8, 1)
    If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
	End Sub

Sub SetVolMusic(Opt)
	Select Case Opt
		Case 0:
			MaxTableVolume = 100
		Case 1:
			MaxTableVolume = 95
		Case 2:
			MaxTableVolume = 90
		Case 3:
			MaxTableVolume = 85
		Case 4:
			MaxTableVolume = 80
		Case 5:
			MaxTableVolume = 75
		Case 6:
			MaxTableVolume = 70
	End Select
	TargetVol = MaxTableVolume
	VolumeTimer.Enabled = True	
End Sub	

Sub SetRails(Opt)
	Select Case Opt
		Case 0:
			Ramp15.Visible = 0
			Ramp16.Visible = 0
			Ramp17.Visible = 0
			PinCab_Blades.visible = 1
			Ramp032.Visible = 0
			Wall024.sidevisible = 0
			Primitive005.Visible = 0
		Case 1:
			If VRRoom = 0 Then
			Ramp15.Visible = 1
			Ramp16.Visible = 1
			Ramp17.Visible = 1
			End If
			PinCab_Blades.visible = 0
			Ramp032.Visible = 1
			Wall024.sidevisible = 1
			Primitive005.Visible = 1
	End Select
End Sub

Sub SetDifficulty(Opt)
	Select Case Opt
		Case 0:
			Primitive008.Visible = 0
			zCol_Rubber_Peg005.Collidable = 0
			Pin004.visible = 0
		Case 1:
			Primitive008.Visible = 1
			zCol_Rubber_Peg005.Collidable = 1
			Pin004.visible = 1
	End Select
End Sub

Sub SetFlipGlow(Opt)
	Select Case Opt
		Case 0:
			ChangePalmBICOL
		Case 1:
			ChangePalmOFF
	End Select
End Sub
'**************************
'DOF 
'**************************
'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 Right Slingshot
'105 Left Slingshot2
'106 Right Flipper2
'107 Bumper Right
'108 Bumper Center
'109 Bumper Left
'110 FX Left Slingshot
'111 FX Right Slingshot
'112 
'113 
'114 
'115 
'116 
'117 AutoPlunger
'118 
'119 
'120 AutoFire
'121 
'122 Knocker
'123 Ball Release
'132 FX Ball Release
'138 FX Bumper1
'139 FX Bumper2
'140 FX Bumper3
'144 
'145 
'146 
'147 
'>300 FX

Randomize

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1
Const SongVolume = 0.5 ' 1 is full volume. Value is from 0 to 1

' Load the core.vbs for supporting Subs and functions

On Error Resume Next
ExecuteGlobal GetTextFile("core.vbs")
If Err Then MsgBox "Can't open core.vbs"
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0


'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow

'----- Others Options -----
Const UltraDMDUpdateTime	= 5000	'UltraDMD update time (msec).  Increase value if you encounter stutter with UltraDMD on
Const UseUltraDMD = 0				'0 = Don't use UltraDMD, 1=Use it
Const UseBGB2S = 0					'0 = Don't use B2S, 1=Use it (0 = Displaying Multiplayer Scores on the PUPDMD)
dim usePuPDMD       : usePuPDMD=True       ' set to false to not use PuPDMD for a DMD (different that BG scoring)

' Define any Constants
Const cGameName = "Wuthering_Waves"
Const TableName = "Wuthering_Waves"
Const typefont = "Ruben"
Const numberfont = "Ruben"
Const zoomfont = "Magic School One"
Const zoombgfont = "Magic School One" ' needs to be an outline of the zoomfont
Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 15 ' in seconds
Const MaxMultiplier = 3  ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 3  ' usually 3 or 5
Const MaxMultiballs = 6  ' max number of balls during multiballs

Const Special1 = 2000000  ' High score to obtain an extra ball/game
Const Special2 = 5000000
Const Special3 = 10000000

' Use FlexDMD if in FS mode

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusMultiplier
Dim PlayfieldMultiplier
Dim bBonusHeld
Dim BallsRemaining(4)
Dim BallinGame(4)
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
Dim Special3Awarded(4)
Dim PlayerScore(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim bAttractMode
Dim mBalls2Eject
Dim bAutoPlunger

' Define Game Control Variables
Dim BallsOnPlayfield
Dim BOT
Const TriggerScriptSize=10
Dim pReset(10)                 ' TriggerScriptSize
Dim pStatement(10)             ' TriggerScriptSize - holds future scripts
Dim FX

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall(4)
Dim FirstShoot
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim MBLock_Sav(4)
Dim MBLockActive(4)
Dim bPersonnageSelect
Dim bMissionSelect
Dim PersonnageActive(4)
Dim EchoCaptureCount(4)
Dim EchoBonusCount(4)
Dim SpinnerCount
Dim SpinnerModeActive
Dim	RampCount
Dim ExperienceCount 'Bonus1
Dim RessourceCount 'Bonus2
Dim PersoSwitchCount 'Bonus3
Dim MissionCount 'Bonus4
Dim ExtraBCount 'no use in this table
Dim SlingCount(4) 'ConcertoEnergy
Dim BattleEchoCount 'Bumper 3 Kill Echo
Dim ModeEchoGold(4) 'MB echo Ready
Dim Mission1_State(4) 'Rectifier - Orbite 6 (Timer)
Dim Mission2_State(4) 'Pistol - 6 Cibles All Light Turn (Timer)
Dim Mission3_State(4) 'Gauntlet - Bumber Hits 20
Dim Mission4_State(4) 'Sword - All Ramp x 2
Dim Mission5_State(4) 'Broadblade - All Light x 2
Dim OrbiteCount 'Mi 1
Dim LightMI2Count 'Mi2
Dim BumperCount 'Mi 3
Dim LightMI4Count 'Mi4
Dim LightMI5Count 'Mi5
Dim AstriteCount(4) 
Dim ForgingTibeCount(4)
Dim LustrousTibeCount(4) 
Dim CoralsCount(4)
Dim KickBackActive(4) 
Dim SkillShotState
Dim MysteryState(4)
Dim bMusicOn
Dim bJustStarted
Dim bJackpot
Dim plungerIM
Dim LastSwitchHit
Dim ComboMcount
Dim ComboLcount
Dim JumpCount
Dim JackpotValue(4)

'****************************************
'		USER OPTIONS
'****************************************

Dim VolumeDial           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume 		' Level of ramp rolling volume. Value between 0 and 1
Dim StagedFlippers : StagedFlippers = 0         ' Staged Flippers. 0 = Disabled, 1 = Enabled

Dim TableWidth: TableWidth = Table1.Width
Dim TableHeight: TableHeight = Table1.Height
'****************************************
' core.vbs variables
Dim cbRight
Dim TopMagnet
dim spinner1
Dim FlasherArray(8) 
Dim IsBlinkingArray(8)

Sub startB2S(aB2S)
'    If B2SOn Then
'        Controller.B2SSetData 1, 0
'        Controller.B2SSetData 2, 0
'        Controller.B2SSetData 3, 0
'        Controller.B2SSetData 4, 0
'        Controller.B2SSetData 5, 0
'        Controller.B2SSetData 6, 0
'        Controller.B2SSetData 7, 0
'        Controller.B2SSetData 8, 0
'        Controller.B2SSetData aB2S, 1
'    End If
End Sub

Dim UltraDMD
Sub LoadUltraDMD
    Set UltraDMD = CreateObject("UltraDMD.DMDObject")
    UltraDMD.Init
	uDMDScoreTimer.Interval = UltraDMDUpdateTime
	uDMDScoreTimer.Enabled = 1
	uDMDScoreUpdate
End Sub

Sub uDMDScoreTimer_Timer
	uDMDScoreUpdate
	pUpdateScores
End Sub

Sub uDMDScoreUpdate
	If UseUltraDMD = 1 Then
		UltraDMD.DisplayScoreboard00 PlayersPlayingGame, CurrentPlayer, PlayerScore(1), PlayerScore(2), PlayerScore(3), PlayerScore(4), "Credits " & Credits, "Ball " & FormatNumber(BallinGame(CurrentPlayer), 0) & "/" & FormatNumber(BallsPerGame, 0)
	End If
End Sub
' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
	LoadEM
'	Reseths
	Dim i
'	Randomize

	' Initialisation des tableaux et des objets
    For i = 1 To 8
        ' S'assurer que vos objets sont bien nommés Flasher001, Flasher002, ... et BlinkTimer001
        Set FlasherArray(i) = Eval("Flasher00" & i) 
        ' Initialisation du tableau de clignotement à False (OFF)
        IsBlinkingArray(i) = False
        
        ' État initial : La lumière ALLUMÉE (Flasher) est invisible
        FlasherArray(i).Visible = 0
    Next
    
    ' Configuration et désactivation du timer global unique
'    GlobalBlinkTimer.TimerInterval = 250
    GlobalBlinkTimer.Enabled = False
    
	'Impulse Plunger as autoplunger
    Const IMPowerSetting = 36 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Loadhs

    'Init main variables
    For i = 1 To MaxPlayers
        PlayerScore(i) = 0
        BonusPoints(i) = 0
   '     BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
		BallinGame(i) = 1
        ExtraBallsAwards(i) = 0
    Next

	' TopMagnet
    Set TopMagnet = New cvpmMagnet
    With TopMagnet
        .InitMagnet Magnet1, 35
        .GrabCenter = True
        .CreateEvents "TopMagnet"
    End With

	' CapKicker
	Set cbRight = New cvpmCaptiveBall
	With cbRight
    .InitCaptive CapTrigger1, CapWall1, Array(CapKicker1, CapKicker1a), 0
    .NailedBalls = 1
	.ForceTrans = 2.1
    .MinForce = 3.5
    .CreateEvents "cbRight"
    .Start
	End With
	CapKicker1.CreateBall

	' Spinner
	Set spinner1 = New cvpmTurntable
	With spinner1
	.InitTurntable spindisc_wheel1, -100
	.SpinDown = 20
	.CreateEvents "spinner1"
	End With
	spinner1.MotorOn = false

	For i = 0 to 4 : bOnTheFirstBall(i) = True : Next
	' FlexDMD
'	Flex_Init
'	ShowScene flexScenes(1), FlexDMD_RenderMode_DMD_RGB, 2
	If usePuPDMD = true Then PUPINIT
	If UseUltraDMD > 0 Then LoadUltraDMD

    ' freeplay or coins
    bFreePlay = True 'we want coins

    'if bFreePlay = false Then DOF 125, DOFOn

	' Turn off the bumper lights
'	FlBumperFadeTarget(1) = 0
'	FlBumperFadeTarget(2) = 0
'	FlBumperFadeTarget(3) = 0

    ' Init main variables and any other flags
	FirstShoot = False
    bAttractMode = False
'    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
	bPersonnageSelect = False
	bMissionSelect = False
    bGameInPlay = False
    bMusicOn = True
    BallsOnPlayfield = 0
	bMultiBallMode = False
	bAutoPlunger = False
	LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bJustStarted = True
    ' set any lights for the attract mode
    GiOff
    StartAttractMode
	pAttractStart
	B2SGIOff
	'EndOfGame()
End Sub

'**********************************
' 	ZMAT: General Math Functions
'**********************************
' These get used throughout the script. 

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

Function ArcCos(x)
	If x = 1 Then
		ArcCos = 0/180*PI
	ElseIf x = -1 Then
		ArcCos = 180/180*PI
	Else
		ArcCos = Atn(-x/Sqr(-x * x + 1)) + 2 * Atn(1)
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

' Used for drop targets
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy) 'Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
	Dim AB, BC, CD, DA
	AB = (bx * py) - (by * px) - (ax * py) + (ay * px) + (ax * by) - (ay * bx)
	BC = (cx * py) - (cy * px) - (bx * py) + (by * px) + (bx * cy) - (by * cx)
	CD = (dx * py) - (dy * px) - (cx * py) + (cy * px) + (cx * dy) - (cy * dx)
	DA = (ax * py) - (ay * px) - (dx * py) + (dy * px) + (dx * ay) - (dy * ax)
	
	If (AB <= 0 And BC <= 0 And CD <= 0 And DA <= 0) Or (AB >= 0 And BC >= 0 And CD >= 0 And DA >= 0) Then
		InRect = True
	Else
		InRect = False
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
	Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
	Dim rotxy
	rotxy = RotPoint(ax,ay,angle)
	rax = rotxy(0) + px
	ray = rotxy(1) + py
	rotxy = RotPoint(bx,by,angle)
	rbx = rotxy(0) + px
	rby = rotxy(1) + py
	rotxy = RotPoint(cx,cy,angle)
	rcx = rotxy(0) + px
	rcy = rotxy(1) + py
	rotxy = RotPoint(dx,dy,angle)
	rdx = rotxy(0) + px
	rdy = rotxy(1) + py
	
	InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
	Dim rx, ry
	rx = x * dCos(angle) - y * dSin(angle)
	ry = x * dSin(angle) + y * dCos(angle)
	RotPoint = Array(rx,ry)
End Function


'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.
' Create the following new collections:
'	 Metals (all metal objects, metal walls, metal posts, metal wire guides)
'	 Apron (the apron walls and plunger wall)
'	 Walls (all wood or plastic walls)
'	 Rollovers (wire rollover triggers, star triggers, or button triggers)
'	 Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
'	 Gates (plate gates)
'	 GatesWire (wire gates)
'	 Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate
' how to make these updates. But in summary the following needs to be updated:
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
'
' Tutorial videos by Apophis
' Audio : Adding Fleep Part 1					https://youtu.be/rG35JVHxtx4?si=zdN9W4cZWEyXbOz_
' Audio : Adding Fleep Part 2					https://youtu.be/dk110pWMxGo?si=2iGMImXXZ0SFKVCh
' Audio : Adding Fleep Part 3					https://youtu.be/ESXWGJZY_EI?si=6D20E2nUM-xAw7xy



'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]
RollingSoundFactor = 1.1 / 5

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.95					  'volume level; range [0, 1]
BumperSoundFactor = 4.25						'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2		  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055 / 5			 'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075 / 5			   'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075 / 5			'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025		   'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025		   'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8	  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075				   'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075 / 3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10	 'volume multiplier; must not be zero
DTSoundLevel = 0.25				 'volume multiplier; must not be zero
RolloverSoundLevel = 0.25		   'volume level; range [0, 1]
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8				   'volume level; range [0, 1]
BallReleaseSoundLevel = 1			   'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2	'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015	 'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025 / 5			 'volume multiplier; must not be zero

'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************
Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioFade = CSng(tmp ^ 10)
	Else
		AudioFade = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioPan = CSng(tmp ^ 10)
	Else
		AudioPan = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function Vol(ball)
    Vol = CSng(BallVel(ball) ^ 2)
End Function

Function BallVel(ball)
    BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2)))
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(BallVel(ball) ^ 3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^ 2 * 15
End Function

Function RndInt(min, max) ' Sets a random number integer between min and max
	RndInt = Int(Rnd() * (max - min + 1) + min)
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeLeftSoundLevel * VolumeDial, - 0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub

Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////

Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////

Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd * 11) + 1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd * 7) + 1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////

Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////

Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////

Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd * 7) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd * 8) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm / 10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm / 10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd * 7) + 1), parm * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd * 4) + 1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub Rubbers_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub



'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd * 10) + 1
		Case 1
			PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 2
			PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 3
			PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 4
			PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 5
			PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 6
			PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 7
			PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 8
			PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 9
			PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 10
			PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd * 9) + 1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()
End Sub

Sub RandomSoundWall()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 5) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 4) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 3) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd * 13) + 1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////

Sub RandomSoundBottomArchBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Bounce_" & Int(Rnd * 2) + 1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////

Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd * 3) + 1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(ActiveBall.id) < 4) And cor.ballvely(ActiveBall.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd * 3) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd * 7) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft ActiveBall
	Else
		RandomSoundTargetHitWeak()
	End If
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd * 9) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd * 7) + 1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd * 5) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, ActiveBall
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, ActiveBall
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If ActiveBall.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If ActiveBall.velx <  - 8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If ActiveBall.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If ActiveBall.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, ActiveBall
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0
			PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1
			PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315  'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05	  'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025 * RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025 * RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025 * RelayFlashSoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025 * RelayFlashSoundLevel, obj
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************


'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(5,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(5)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
			RampBalls(0, 0) & vbnewline & _
			Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
			Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
			Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
			Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
			Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
			" "
		End If
	next
End Sub

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
Sub WRemoveBall(ID)		'Remove ball
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
	"1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
	"2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
	"3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
	"4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
	"5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
	"6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
	" "
End Sub


Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function


'Ramp triggers


'Ramp triggers

Sub RampStart1_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub RampStart2_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub RampStart3_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub RampStart4_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub RampStart5_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub WireStart1_hit
        WireRampOn False           ' true means plastic ramp here
End Sub

Sub WireStart2_hit
        WireRampOn False           ' true means plastic ramp here
End Sub

Sub WireStart3_hit
        WireRampOn False           ' true means plastic ramp here
End Sub

Sub WireStart4_hit
        WireRampOn False           ' true means plastic ramp here
End Sub

Sub WireStart5_hit
        WireRampOn False           ' true means plastic ramp here
End Sub

Sub Switch2Wire1_hit
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
        WireRampOn False          ' wire
End Sub

Sub Switch2Wire2_hit
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
        WireRampOn False          ' wire
End Sub

Sub Switch2Wire3_hit
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
        WireRampOn False          ' wire
End Sub

Sub Switch2Flat1_hit
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
        WireRampOn True          ' wire
End Sub

Sub Switch2Flat2_hit
    WireRampOff
    ' optional "clunk" sound:
    RandomSoundRampStop Me
        WireRampOn True          ' wire
End Sub



Sub RandomSoundRampStop(obj)
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtVol "wireramp_stop1", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtVol "wireramp_stop2", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtVol "wireramp_stop3", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub




'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************



'******************************************************
'  ZANI: Misc Animations
'******************************************************

Sub LeftFlipper_Animate
	dim a: a = LeftFlipper.CurrentAngle
	FlipperLSh.RotZ = a
	LFLogo.RotZ = a
	'Add any left flipper related animations here
End Sub

Sub RightFlipper_Animate
	dim a: a = RightFlipper.CurrentAngle
	FlipperRSh.RotZ = a
	RFLogo.RotZ = a
	'Add any right flipper related animations here
End Sub

Sub RightFlipper001_Animate
	dim a: a = RightFlipper001.CurrentAngle
	FlipperURSh.RotZ = a
	RFLogo001.RotZ = a
	'Add any right flipper related animations here
End Sub



'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    RollingUpdate
    ' add any other real time update subs, like gates or diverters
    FlipperLSh.Rotz = LeftFlipper.CurrentAngle
    FlipperRSh.Rotz = RightFlipper.CurrentAngle
	FlipperURSh.RotZ = RightFlipper001.CurrentAngle
	glowbatleft.objrotz = LeftFlipper.CurrentAngle
	GlowBatLightLeft.y = 1999 - 121.5 + LeftFlipper.CurrentAngle
	LFLogo.RotZ = LeftFlipper.CurrentAngle
	RFlogo.RotZ = RightFlipper.CurrentAngle
	RFlogo001.RotZ = RightFlipper001.CurrentAngle
	glowbatright.objrotz = RightFlipper.CurrentAngle
	GlowBatLightRight.y =1999 - 121.5 - RightFlipper.CurrentAngle
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
'    If keycode = RightMagnaSave Then 'halplay
'        halgame                       'halplay
'        timer067.enabled=1            'halplay
'    End If   
'	If keycode = LeftMagnaSave And halplay=1 Then halgameOFF 'halplay

	If bPersonnageSelect Then
        SelectPersonnage(keycode)
    End If

	If bMissionSelect Then
		SelectMission(keycode)
	End If

    If Keycode = AddCreditKey Then
'		DOF 202, DOFPulse
        Credits = Credits + 1
		If pInAttract = True Then PuPlayer.LabelSet pDMD,"tmpCredits","Crédits "& Credits,1,""
		PlaySound "Coin_In_1"
		'If B2SOn Then Controller.B2SSetscore 5, Credits
        if bFreePlay = False Then
'            DOF 125, DOFOn
            If(Tilted = False) Then
            End If
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        'PlaySoundAt "fx_plungerpull", plunger
		SoundPlungerPull


        'PlaySoundAt "fx_reload", plunger
'		pDMDStartGame
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt
	
'	If keycode = LeftMagnaSave Then
'	If keycode = RightMagnaSave Then

	
	If keycode = LeftFlipperKey Then
		SolLFlipper True	'This would be called by the solenoid callbacks if using a ROM
	End If
	
	If keycode = RightFlipperKey Then
		SolRFlipper True	'This would be called by the solenoid callbacks if using a ROM
		SolURFlipper True
'		if PuPGameRunning Then PuPGameInfo= PuPlayer.GameUpdate("PupMiniGame", 1 , 87 , "")  'w
	End If

        If keycode = StartGameKey Then
'            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then
			If (PlayersPlayingGame < MaxPlayers) AND (bOnTheFirstBall(1) = True) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
						'If B2SOn Then Controller.B2SSetscore 5, Credits
						PlaySound "Fx_Bonus1"
						pDMDStartGame
                        Credits = Credits - 1
						'If B2SOn Then Controller.B2SSetscore 5, Credits
                        If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetForNewGame()
'						UpdateMusicNow
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
'							UpdateMusicNow
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                    End If
                End If
            End If
    End If ' If (GameInPlay)

End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then
        Plunger.Fire
		SoundPlungerReleaseBall
        'PlaySoundAt "fx_plunger", plunger
        If bBallInPlungerLane Then PlaySoundAt "fx_fire", plunger
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific
'	If keycode = LeftMagnaSave Then 
'	If keycode = RightMagnaSave Then 

    If bGameInPLay AND NOT Tilted Then
	pDMDStartGame
		If keycode = LeftFlipperKey Then
			SolLFlipper False   'This would be called by the solenoid callbacks if using a ROM
		End If
	
		If keycode = RightFlipperKey Then
			SolRFlipper False   'This would be called by the solenoid callbacks if using a ROM
			SolURFlipper False
		End If
    End If
End Sub

'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub Table1_Exit
'    Savehs
'    If B2SOn Then Controller.Stop
	If UseUltraDMD > 0 Then UltraDMD.CancelRendering
End Sub

'*******************************************
'	ZFLP: Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled) 'Left flipper solenoid callback
	If Enabled Then
		PlaySoundAt SoundFXDOF("", 101, DOFOn, DOFFlippers), LeftFlipper
		FlipperActivate LeftFlipper, LFPress
		LF.Fire  'leftflipper.rotatetoend
		RotateLaneLightsLeftUp
		
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper
		Else
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
	Else
		PlaySoundAt SoundFXDOF("", 101, DOFOff, DOFFlippers), LeftFlipper
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled) 'Right flipper solenoid callback
	If Enabled Then
		PlaySoundAt SoundFXDOF("", 102, DOFOn, DOFFlippers), RightFlipper
		FlipperActivate RightFlipper, RFPress
		RF.Fire 'rightflipper.rotatetoend
		RotateLaneLightsRightUp
		
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		PlaySoundAt SoundFXDOF("", 102, DOFOff, DOFFlippers), RightFlipper
		FlipperDeActivate RightFlipper, RFPress
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolURFlipper(Enabled)
	If Enabled Then
		PlaySoundAt SoundFXDOF("fx_flipperup", 106, DOFOn, DOFFlippers), RightFlipper
		RightFlipper001.rotatetoend

		If RightFlipper001.currentangle > RightFlipper001.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper001
		Else 
			SoundFlipperUpAttackRight RightFlipper001
			RandomSoundFlipperUpRight RightFlipper001
		End If
	Else
		PlaySoundAt SoundFXDOF("fx_flipperdown", 106, DOFOff, DOFFlippers), RightFlipper
		RightFlipper001.RotateToStart
		If RightFlipper001.currentangle > RightFlipper001.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper001
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	RightFlipperCollide parm
End Sub

Sub RightFlipper001_Collide(parm)
	RightFlipperCollide parm
End Sub

'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. and, special scripting
'
' TriggerLF and RF should now be 27 vp units from the flippers. In addition, 3 degrees should be added to the end angle
' when creating these triggers.
'
' RF.ReProcessBalls Activeball and LF.ReProcessBalls Activeball must be added the flipper_collide subs.
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.4            | 0.4                   | 0.375                  | 0.375              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity

'*******************************************
' Early 90's and after

Sub InitPolarity()
	Dim x, a
	a = Array(LF, RF)
	For Each x In a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, - 5.5
		x.AddPt "Polarity", 2, 0.16, - 5.5
		x.AddPt "Polarity", 3, 0.20, - 0.75
		x.AddPt "Polarity", 4, 0.25, - 1.25
		x.AddPt "Polarity", 5, 0.3, - 1.75
		x.AddPt "Polarity", 6, 0.4, - 3.5
		x.AddPt "Polarity", 7, 0.5, - 5.25
		x.AddPt "Polarity", 8, 0.7, - 4.0
		x.AddPt "Polarity", 9, 0.75, - 3.5
		x.AddPt "Polarity", 10, 0.8, - 3.0
		x.AddPt "Polarity", 11, 0.85, - 2.5
		x.AddPt "Polarity", 12, 0.9, - 2.0
		x.AddPt "Polarity", 13, 0.95, - 1.5
		x.AddPt "Polarity", 14, 1, - 1.0
		x.AddPt "Polarity", 15, 1.05, -0.5
		x.AddPt "Polarity", 16, 1.1, 0
		x.AddPt "Polarity", 17, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.23, 0.85
		x.AddPt "Velocity", 2, 0.27, 1
		x.AddPt "Velocity", 3, 0.3, 1
		x.AddPt "Velocity", 4, 0.35, 1
		x.AddPt "Velocity", 5, 0.6, 1 '0.982
		x.AddPt "Velocity", 6, 0.62, 1.0
		x.AddPt "Velocity", 7, 0.702, 0.968
		x.AddPt "Velocity", 8, 0.95,  0.968
		x.AddPt "Velocity", 9, 1.03,  0.945
		x.AddPt "Velocity", 10, 1.5,  0.945

	Next
	
	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before\
' modified 2024 by rothbauerw
' Added Reprocessballs for flipper collisions (LF.Reprocessballs Activeball and RF.Reprocessballs Activeball must be added to the flipper collide subs
' Improved handling to remove correction for backhand shots when the flipper is raised

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef, FlipStartAngle
	Private Balls(20), balldata(20)
	Private Name
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = new SpoofBall
		Next
	End Sub
	
	Public Sub SetObjects(aName, aFlipper, aTrigger)
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper
		FlipperStart = aFlipper.x
		FlipperEnd = Flipper.Length * Sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * Cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
		
		Dim str
		str = "Sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "Sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)
		
	End Sub
	
	' Legacy: just no op
	Public Property Let EndPoint(aInput)
		
	End Property
	
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
		Select Case aChooseArray
			Case "Polarity"
				ShuffleArrays PolarityIn, PolarityOut, 1
				PolarityIn(aIDX) = aX
				PolarityOut(aIDX) = aY
				ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
				ShuffleArrays VelocityIn, VelocityOut, 1
				VelocityIn(aIDX) = aX
				VelocityOut(aIDX) = aY
				ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
				ShuffleArrays YcoefIn, YcoefOut, 1
				YcoefIn(aIDX) = aX
				YcoefOut(aIDX) = aY
				ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub
	
	Public Sub Fire()
		Flipper.RotateToEnd
		processballs
	End Sub
	
	Public Property Get Pos 'returns % position a ball. For debug stuff.
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				balldata(x).Data = balls(x)
			End If
		Next
		FlipStartAngle = Flipper.currentangle
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

	Public Sub ReProcessBalls(aBall) 'save data of balls in flipper range
		If FlipperOn() Then
			Dim x
			For x = 0 To UBound(balls)
				If Not IsEmpty(balls(x)) Then
					if balls(x).ID = aBall.ID Then
						If isempty(balldata(x).ID) Then
							balldata(x).Data = balls(x)
						End If
					End If
				End If
			Next
		End If
	End Sub

	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef, BalltoFlip, BalltoBase, NoCorrection, checkHit
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY > -8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					BalltoFlip = DistanceFromFlipperAngle(BallData(x).x, BallData(x).y, Flipper, FlipStartAngle)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
				NoCorrection = 1
			Else
				checkHit = 50 + (20 * BallPos) 

				If BalltoFlip > checkHit or (PartialFlipCoef < 0.5 and BallPos > 0.22) Then
					NoCorrection = 1
				Else
					NoCorrection = 0
				End If
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				'If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled and NoCorrection = 0 Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef*VelCoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray)		'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount-1+offset)		'Resize original array
	For x = 0 To aCount-1				'set objects back into original array
		If IsObject(a(x)) Then
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = Sqr(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	'find active line
	Dim ii
	For ii = 1 To UBound(xKeyFrame)
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )		 'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )		'Clamp upper
	
	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS
'******************************************************
' To add the flipper tricks you must
'	 - Include a call to FlipperCradleCollision from within OnBallBallCollision subroutine
'	 - Include a call the CheckLiveCatch from the LeftFlipper_Collide and RightFlipper_Collide subroutines
'	 - Include FlipperActivate and FlipperDeactivate in the Flipper solenoid subs

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	Dim BOT
	BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub


Dim FCCDamping: FCCDamping = 0.4

Sub FlipperCradleCollision(ball1, ball2, velocity)
	if velocity < 0.7 then exit sub		'filter out gentle collisions
    Dim DoDamping, coef
    DoDamping = false
    'Check left flipper
    If LeftFlipper.currentangle = LFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, LeftFlipper) OR FlipperTrigger(ball2.x, ball2.y, LeftFlipper) Then DoDamping = true
    End If
    'Check right flipper
    If RightFlipper.currentangle = RFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, RightFlipper) OR FlipperTrigger(ball2.x, ball2.y, RightFlipper) Then DoDamping = true
    End If
    If DoDamping Then
		coef = FCCDamping
        ball1.velx = ball1.velx * coef: ball1.vely = ball1.vely * coef: ball1.velz = ball1.velz * coef
        ball2.velx = ball2.velx * coef: ball2.vely = ball2.vely * coef: ball2.velz = ball2.velz * coef
    End If
End Sub

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function DistanceFromFlipperAngle(ballx, bally, Flipper, Angle)
	DistanceFromFlipperAngle = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Angle + 90)) + Flipper.x, Sin(Radians(angle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, LFCount, RFCount
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
		SOSRampup = 2.5
	Case 1
		SOSRampup = 6
	Case 2
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
'   Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity
	
	Flipper.eostorque = EOST
	Flipper.eostorqueangle = EOSA
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls
		
		For b = 0 To UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= - 0.4 Then BOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 5  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
Const BaseDampen = 0.55

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
    Dim Dir, LiveDist
    Dir = Flipper.startangle / Abs(Flipper.startangle)    '-1 for Right Flipper
    Dim LiveCatchBounce   'If live catch is not perfect, it won't freeze ball totally
    Dim CatchTime
    CatchTime = GameTime - FCount
    LiveDist = Abs(Flipper.x - ball.x)

    If CatchTime <= LiveCatch And parm > 3 And LiveDist > LiveDistanceMin And LiveDist < LiveDistanceMax Then
        If CatchTime <= LiveCatch * 0.5 Then   'Perfect catch only when catch time happens in the beginning of the window
            LiveCatchBounce = 0
        Else
            LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)  'Partial catch when catch happens a bit late
        End If
        
        If LiveCatchBounce = 0 And ball.velx * Dir > 0 And LiveDist > 30 Then ball.velx = 0

        If ball.velx * Dir > 0 And LiveDist < 30 Then
            ball.velx = BaseDampen * ball.velx
            ball.vely = BaseDampen * ball.vely
            ball.angmomx = BaseDampen * ball.angmomx
            ball.angmomy = BaseDampen * ball.angmomy
            ball.angmomz = BaseDampen * ball.angmomz
        Elseif LiveDist > 30 Then
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx = 0
            ball.angmomy = 0
            ball.angmomz = 0
        End If
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
    End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************
Sub leftInlaneSpeedLimit
	'Wylte's implementation
'    debug.print "Spin in: "& activeball.AngMomZ
'    debug.print "Speed in: "& activeball.vely
	if activeball.vely < 0 then exit sub 							'don't affect upwards movement
    activeball.AngMomZ = -abs(activeball.AngMomZ) * RndNum(3,6)
    If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
    If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)

    if abs(activeball.vely) > 5 then activeball.vely = 0.8 * activeball.vely
    if abs(activeball.vely) > 10 then activeball.vely = 0.8 * activeball.vely
    if abs(activeball.vely) > 15 then activeball.vely = 0.8 * activeball.vely
    if activeball.vely > 16 then activeball.vely = RndNum(14,16)
    if activeball.vely < -16 then activeball.vely = RndNum(-14,-16)
'    debug.print "Spin out: "& activeball.AngMomZ
'    debug.print "Speed out: "& activeball.vely
End Sub


Sub rightInlaneSpeedLimit
	'Wylte's implementation
'    debug.print "Spin in: "& activeball.AngMomZ
'    debug.print "Speed in: "& activeball.vely
	if activeball.vely < 0 then exit sub 							'don't affect upwards movement
    activeball.AngMomZ = abs(activeball.AngMomZ) * RndNum(2,4)
    If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
    If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)

	if abs(activeball.vely) > 5 then activeball.vely = 0.8 * activeball.vely
    if abs(activeball.vely) > 10 then activeball.vely = 0.8 * activeball.vely
    if abs(activeball.vely) > 15 then activeball.vely = 0.8 * activeball.vely
    if activeball.vely > 16 then activeball.vely = RndNum(14,16)
    if activeball.vely < -16 then activeball.vely = RndNum(-14,-16)
'    debug.print "Spin out: "& activeball.AngMomZ
'    debug.print "Speed out: "& activeball.vely
End Sub
'******************************************************
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	TargetBouncer ActiveBall, 0.7
End Sub

Sub zCol_Rubber_leftsling
	RubbersD.dampen Activeball
End Sub

Sub zCol_Rubber_rightsling
	RubbersD.dampen Activeball
End Sub

Sub zCol_Rubber_Peg024
	RubbersD.dampen Activeball
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports In debugger (In vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports In debugger (In vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		aBall.velz = aBall.velz * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
			aBall.velz = aBall.velz * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = GetBalls
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class


' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'

Sub CorTimer_Timer()
	Cor.Update
End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9	 'Level of bounces. Recommmended value of 0.7-1

Sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel, vratio
	If TargetBouncerEnabled = 1 And aball.z < 30 Then
		'   debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
		If aBall.velx = 0 Then vratio = 1 Else vratio = aBall.vely / aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1
				zMultiplier = 0.2 * defvalue
			Case 2
				zMultiplier = 0.25 * defvalue
			Case 3
				zMultiplier = 0.3 * defvalue
			Case 4
				zMultiplier = 0.4 * defvalue
			Case 5
				zMultiplier = 0.45 * defvalue
			Case 6
				zMultiplier = 0.5 * defvalue
		End Select
		aBall.velz = Abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = Sgn(aBall.velx) * Sqr(Abs((vel ^ 2 - aBall.velz ^ 2) / (1 + vratio ^ 2)))
		aBall.vely = aBall.velx * vratio
		'   debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'   debug.print "conservation check: " & BallSpeed(aBall)/vel
	End If
End Sub

'Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer ActiveBall, 1
End Sub

'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS
	
	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script In-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			'   debug.print " "
		End If
	End Sub
End Class


'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
		pDMDmessage1L "!CAREFUL!",1
		pDMDLabelFadePulse "Texte1a", 2500, clRed
		if UseUltraDMD = 1 then UDMD "!CAREFUL!", "", 3000
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        'DMDFlush
		DOF 400, DOFPulse
		pDMDmessage1L "TILT",1
		if UseUltraDMD = 1 then UDMD "TILT", "", 3000
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt> 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
		B2SGIOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
		B2SGIOn
        LightSeqTilt.StopPlay
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        'DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0) Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'********************
' Music as wav sounds
'********************
'
Dim Song, UpdateMusic
Song = ""

'Sub PlaySong(name)
'    If bMusicOn Then
'        If Song <> name Then
'            StopSound Song
'            Song = name
'            PlaySound Song, -1, SongVolume
'        End If
'    End If
'End Sub
'
'Sub StopSong
'    If bMusicOn Then
'        StopSound Song
'        Song = ""
'    End If
'End Sub
'
'Sub ChangeSong
'    If(BallsOnPlayfield = 0)Then
'        PlaySong ""'"Mu_end"
'        Exit Sub
'    End If
'
'    If bAttractMode Then
'        PlaySong ""'"Mu_end"
'        Exit Sub
'    End If
'    If bMultiBallMode Then
'        PlaySong "" '"Mu_MB"
'    Else
'        UpdateMusicNow
'    end if
'End Sub

'if you add more balls to the game use changesong then if bMultiBallMode = true, your multiball song will be played.

'Sub UpdateMusicNow
'    Select Case UpdateMusic
'        Case 0:PlaySong "Mu_1"
'        Case 1:PlaySong "Mu_2"
'        Case 2:PlaySong "Mu_3"
'		Case 3:PlaySong "Mu_1"
'        Case 4:PlaySong "Mu_2"
'        Case 5:PlaySong "Mu_3"
'    End Select
'end sub

'********************
' Play random quotes
'********************

Sub PlayQuote
    Dim tmp
    tmp = INT(RND * 123) + 1
    PlaySound "HIT_" &tmp
End Sub

'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 10 colors: red, orange, amber, yellow...
'******************************************
' in this table this colors are use to keep track of the progress during the acts and battles

'colors
Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white, base, baseLB, baseLH

red = 10
orange = 9
amber = 8
yellow = 7
darkgreen = 6
green = 5
blue = 4
darkblue = 3
purple = 2
white = 1
base = 11
baseLB = 12
baseLH = 13

Sub SetLightColor(n, col, stat)
	Select Case col
		Case red
			n.color = RGB(255, 0, 0)
			n.colorfull = RGB(255, 0, 0)
'			n.color = RGB(18, 0, 0)
'			n.colorfull = RGB(255, 0, 0)
		Case orange
			n.color = RGB(255, 64, 0)
			n.colorfull = RGB(255, 64, 0)
'			n.color = RGB(18, 3, 0)
'			n.colorfull = RGB(255, 64, 0)
		Case amber
			n.color = RGB(193, 49, 0)
			n.colorfull = RGB(255, 153, 0)
		Case yellow
'			n.color = RGB(18, 18, 0)
			n.color = RGB(255, 255, 0)
			n.colorfull = RGB(255, 255, 0)
		Case darkgreen
			n.color = RGB(0, 8, 0)
			n.colorfull = RGB(0, 64, 0)
		Case green
			n.color = RGB(0, 255, 0)
'			n.color = RGB(0, 18, 0)
			n.colorfull = RGB(0, 255, 0)
		Case blue
			n.color = RGB(0, 18, 18)
			n.colorfull = RGB(0, 255, 255)
		Case darkblue
			n.color = RGB(0, 8, 8)
			n.colorfull = RGB(0, 64, 64)
		Case purple
			n.color = RGB(128, 0, 128)
			n.colorfull = RGB(255, 0, 255)
		Case white
			n.color = RGB(255, 252, 224)
			n.colorfull = RGB(193, 91, 0)
'		Case white
'			n.color = RGB(255, 252, 224)
'			n.colorfull = RGB(193, 91, 0)
		Case base
			n.color = RGB(255, 197, 143)
			n.colorfull = RGB(255, 255, 236)
		Case baseLB
			n.color = RGB(0, 0, 160)
			n.colorfull = RGB(0, 0, 160)
		Case baseLH
'			n.color = RGB(34, 100, 255)
'			n.colorfull = RGB(34, 100, 255)
			n.color = RGB(128, 0, 128)
			n.colorfull = RGB(255, 0, 255)
	End Select
	If stat <> -1 Then
		n.State = 0
		n.State = stat
	End If
End Sub
'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************
'B2S GI On and Off support
Sub B2SGIOn
'	If B2SOn Then Controller.B2SSetData 1,1
End Sub
Sub B2SGIOff
'	If B2SOn Then Controller.B2SSetData 1,0
End Sub

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub ChangeGiLampH(col) 'changes the gi color
    Dim bulb
    For each bulb in aGiLampH
        SetLightColor bulb, col, -1
    Next
End Sub

Sub ChangeGiLampB(col) 'changes the gi color
    Dim bulb
    For each bulb in aGiLampB
        SetLightColor bulb, col, -1
    Next
End Sub

Sub ChangeGiLogo(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILightsLogo
        SetLightColor bulb, col, -1
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 1 Then 'we have 2 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aBumperLights
        bulb.State = 1
    Next
	For each bulb in aGiLampB
        bulb.State = 1
    Next
	For each bulb in aGiLampH
        bulb.State = 1
    Next
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to2"
End Sub

Sub GiOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aBumperLights
        bulb.State = 0
    Next
	For each bulb in aGiLampB
        bulb.State = 0
    Next
	For each bulb in aGiLampH
        bulb.State = 0
    Next
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to3"
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 10, 10
        Case 4 'all blink once
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 4, 1
		Case 5 'all blink once
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 3, 1
    End Select
End Sub

Sub GiEffectBumper(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqBumper.Play SeqAlloff
        Case 1 'all blink
            LightSeqBumper.UpdateInterval = 10
            LightSeqBumper.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqBumper.UpdateInterval = 10
            LightSeqBumper.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqBumper.UpdateInterval = 10
            LightSeqBumper.Play SeqBlinking, , 10, 10
        Case 4 'all blink once
            LightSeqBumper.UpdateInterval = 10
            LightSeqBumper.Play SeqBlinking, , 4, 1
		Case 5 'all blink once
            LightSeqBumper.UpdateInterval = 10
            LightSeqBumper.Play SeqBlinking, , 2, 1
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 4 'up 1 time
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 8, 1
        Case 5 'up 2 times
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 8, 2
        Case 6 'down 1 time
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 8, 1
        Case 7 'down 2 times
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 8, 2
    End Select
End Sub

Sub ChangeGILogoNow
	Select Case Int(Rnd * 6) + 1
		Case 1
			ChangeGiLogo red
		Case 2
			ChangeGiLogo green
		Case 3
			ChangeGiLogo purple
		Case 4
			ChangeGiLogo blue
		Case 5
			ChangeGiLogo yellow
		Case 6
			ChangeGiLogo white
	End Select
End Sub
' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function


'********************************************
'   JP's VP10 Rolling Sounds
'********************************************

Const tnob = 11 

Dim gBot

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b', BOT
	gBOT = GetBalls


	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob - 1
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(gBOT)
		If BallVel(gBOT(b)) > 1 AND gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
		If gBOT(b).VelZ < -1 and gBOT(b).z < 55 and gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard gBOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
	Next
End Sub


'**********************
' Ball Collision Sound
'**********************
Sub OnBallBallCollision(ball1, ball2, velocity)
	
	FlipperCradleCollision ball1, ball2, velocity

	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select

	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub SoundOnOrbCollision()
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select
	On error Resume Next 
	PlaySoundAtLevelActiveBall snd, Vol(ActiveBall) * BallWithBallCollisionSoundFactor
	On Error goto 0
End Sub

'Sub OnBallBallCollision(ball1, ball2, velocity)
'    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
'End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

'Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
'Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
'Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
'Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
'Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
'Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
'Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

Sub RHelp1_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAtBall "fx_ballrampdrop"
End Sub

Sub RHelp2_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAtBall"fx_ballrampdrop"
End Sub



' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

    'resets the score display, and turn off attract mode
    StopAttractMode
	ResetEvents
    GiOn
	B2SGIOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
 '   bOnTheFirstBall = True
	'Multiball=false	
    For i = 1 To MaxPlayers
        PlayerScore(i) = 0
        BonusPoints(i) = 0
		'BonusHeldPoints(i) = 0
  '      BonusMultiplier(i) = 1
		bOnTheFirstBall(i) = True
        BallsRemaining(i) = BallsPerGame
		BallinGame(i) = 1
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
    Next
'	If B2SOn Then Controller.B2SSetScorePlayer 1, 0

    ' initialise any other flags
    Tilt = 0

	'reset variables
	bumperHits = 100
'    UpdateMusic = 0
    'UpdateMusic = UpdateMusic + 6
'    UpdateMusicNow

    ' initialise Game variables
    Game_Init()
	
    ' you may wish to start some music, play a sound, do whatever at this point
'StopSong

    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to

' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
	ResetEvents
'	PlaySound "Vo_Start"

    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    AddScore 0

    ' set the current players bonus multiplier back down to 1X
    BonusMultiplier = 1
	PlayfieldMultiplier = 1
    'UpdateBonusXLights
	
	' reset any drop targets, lights, game Mode etc..
	CheckEventPlayer

   'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'Reset any table specific
'	BumperBonus = 0
	JackpotsBonus = 0
'	MulitballBonus = 0
    ResetNewBallVariables
    ResetNewBallLights()
	'Multiball=false	
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    
	LightSeqAttract.StopPlay

	' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
	RandomSoundBallRelease Ballrelease
    'PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
	DOF 123, DOFPulse
	DOF 132, DOFPulse
    BallRelease.Kick 90, 4

	'only this tableDrain / Plunger Functions
	'ChangeBallImage

End Sub


' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
    'and eject the first ball
    CreateMultiballTimer_Timer
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        If BallsOnPlayfield < MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
                CreateMultiballTimer.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            CreateMultiballTimer.Enabled = False
        End If
    End If
End Sub


' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()
	Dim BonusDelayTime
	' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall(CurrentPlayer) = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

	'LightSeqAttract.Play SeqBlinking, , 5, 150

'StopSong

'bonuscheckie

    Dim AwardPoints, TotalBonus, ii
	AwardPoints = 0
    TotalBonus = 0

    'If NOT Tilted Then
	If(Tilted = False) Then
        'Bonus
		Pupevent 820
'		PuPlayer.LabelShowPage pDMD,1,0,""
	
'		PuPlayer.LabelSet pBackglass,"Bonus1", FormatNumber(JackpotsBonus,0),1,"{'mt':2,'color':13264128, 'size': 4 }"
'		AwardPoints = JackpotsBonus * 5000
'       TotalBonus = TotalBonus + AwardPoints

'		PuPlayer.LabelSet pBackglass,"Bonus2", FormatNumber(RaiderBonus,0),1,"{'mt':2,'color':55660, 'size': 4 }"
'		AwardPoints = RaiderBonus * 2000
'       TotalBonus = TotalBonus + AwardPoints

'		PuPlayer.LabelSet pBackglass,"Bonus3", FormatNumber(CroftBonus,0),1,"{'mt':2,'color':3739322, 'size': 4 }"
'		AwardPoints = CroftBonus * 2000
'       TotalBonus = TotalBonus + AwardPoints

'		PuPlayer.LabelSet pBackglass,"Bonus4", FormatNumber(RelicsBonus,0),1,"{'mt':2,'color':25542, 'size': 4 }"
'		AwardPoints = RelicsBonus * 15000
'       TotalBonus = TotalBonus + AwardPoints

'		PuPlayer.LabelSet pBackglass,"Bonus5", FormatNumber(ArtefactsBonus,0),1,"{'mt':2,'color':5602052, 'size': 4 }"
'		AwardPoints = ArtefactsBonus * 10000
        TotalBonus = TotalBonus + AwardPoints
        
'		PuPlayer.LabelSet pBackglass,"BonusTotal", FormatNumber(TotalBonus,0),1,"{'mt':2,'color':16777215, 'size': 4 }"	
        TotalBonus = TotalBonus * BonusMultiplier
       
		SeqBonus
'		vpmtimer.addtimer 2500, "AddScore TotalBonus '"
'		AddScore TotalBonus

		' add a bit of a delay to allow for the bonus points to be shown & added up
		vpmtimer.addtimer 7000, "resetbackglassOFFScore '"
		vpmtimer.addtimer 7000, "EndOfBall2 '"
    Else 'Si hay falta simplemente espera un momento y va directo a la segunta parte después de perder la bola
		BonusDelayTime = 100
		EndOfBall2
    End If
	'vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
'   UpdateMusic = UpdateMusic + 1
'	UpdateMusicNow	
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
'           LiExtra.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        'DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, "vo_extraball"

'		UpdateMusic = UpdateMusic - 1
'		UpdateMusicNow

        ' reset the playfield for the new ball
        ResetForNewPlayerBall()
		
		' set the dropped wall for bonus

		
        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1
		BallinGame(CurrentPlayer) = BallinGame(CurrentPlayer) + 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
			BallinGame(CurrentPlayer) = BallsPerGame
            'debug.print "No More Balls, High Score Entry"

            ' Submit the CurrentPlayers score to the High Score system
            CheckHighScore()
        ' you may wish to play some music at this point

        Else

            ' not the last ball (for that player)
            ' if multiple players are playing then move onto the next one
            EndOfBallComplete()
        End If
    End If
End Sub

' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame> 1) Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer> PlayersPlayingGame) Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode
'		StopSong
		'DMD CL(0, "GAME OVER") "", eNone, 13000, True, ""
		'DMD "", CL(1, "GAME OVER"), "", eNone, eNone, eNone, 13000, False, ""
		PuPEvent 822
		playclear pMusic
        ' set the machine into game over mode
'       vpmtimer.addtimer 4000, "EndOfGame() '"
'		vpmtimer.addtimer 6800, "pDMDGameOver '"
		vpmtimer.addtimer 8000, "EndOfGame() '"
		vpmtimer.addtimer 9500, "pDMDGameOver '"
		
    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        'DMDScoreNow

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            PlaySound "vo_player" &CurrentPlayer
            'DMD "_", CL(1, "PLAYER " &CurrentPlayer), "", eNone, eNone, eNone, 800, True, ""
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    LightSeqAttract.StopPlay
	'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then
		playclear pMusic
    End If

    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0
	SolURFlipper 0	

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
	B2SGIOff
    StartAttractMode
	ResetEvents
' you may wish to light any Game Over Light you may have
End Sub

Function BallsFunc
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
        BallsFunc = BallsPerGame
    Else
        BallsFunc = tmp
    End If
End Function

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
    ' Destroy the ball
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1 
	'If BallsOnPlayfield<2 Then
	'Multiball=false
	'end if
	
    ' pretend to knock the ball into the ball storage mech
	RandomSoundDrain Drain
    'PlaySoundAt "fx_drain", Drain
    'if Tilted then end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then
	AddMultiball 1
	Playsound "Fx_SaveBall"
	DOF 132, DOFPulse
	PuPEvent 843
	pDMDmessage2L "Ball","SAVED",1
	pDMDLabelFadePulse "Texte2a", 2500, clRed
    pDMDLabelFadePulse "Texte2b", 2500, clNavy
	if UseUltraDMD = 1 then UDMD "BALL SAVED", "", 3000:Pupevent 911
	bAutoPlunger = True
            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
    'vpmtimer.addtimer 1250, "CreateNewBall() '"

           ' you may wish to put something on a display or play a sound at this point

            
        Else

			If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
					Mode_Multiball_End
                    bMultiBallMode = False		
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
'					StopSong
'					PlaySong "Mu_1"
'					ChangeSong
					StartMusic
                End If
            End If
            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
'                   if halplay=1 Then 'halplay
'                   kicker025.enabled=1'halplay
'                   end If'halplay
                ' End Mode and timers
				SelectPersoAutoNow
'				StopSong
				DownMusicTmp
'				PlaySoundEND
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub



' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub Trigger1_Hit()
If bAutoPlunger Then
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
        DOF 132, DOFPulse
        PlaySoundAt "fx_fire", Trigger1
        bAutoPlunger = False
    End If	
'StopSong

    bBallInPlungerLane = True
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
'        EnableBallSaver BallSaverTime
		 DOF 310, DOFon
'        Else
'        ' show the message to shoot the ball in case the player has fallen sleep
'        Trigger1.TimerEnabled = 1
    End If
End Sub

Sub Gate001_Hit()
	AddScore 2550
	DOF 310, DOFoff
	FlashForMs F1A003, 1000, 50, 0
	ChangeGILogoNow
'	CheckSkillShot
	If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then 
'        EnableBallSaver BallSaverTime
		If PersonnageActive(CurrentPlayer) = 3 Then EnableBallSaver 25 Else EnableBallSaver 15
		StartKickBack
'		ResetSkillShotTimer.Enabled = True
        Else
        ' show the message to shoot the ball in case the player has fallen sleep
        Trigger1.TimerEnabled = 1
    End If
End Sub
' The ball is released from the plunger

Sub Trigger1_UnHit()
    bBallInPlungerLane = False
    'LightEffect 4
	'ChangeSong
End Sub


Sub Trigger1_Timer
    trigger1.TimerEnabled = 0
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimer.Interval = 1000 * seconds
    BallSaverTimer.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimer_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimer.Enabled = False
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
   LightShootAgain.State = 0
End Sub
Sub BallSaverEnd
	BallSaverTimer.Enabled = False
    bBallSaverActive = False
    LightShootAgain.State = 0
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
Sub AddScore(points)
    If Tilted Then Exit Sub

    ' add the points to the current players score variable
 '   PlayerScore(CurrentPlayer) = PlayerScore(CurrentPlayer) + points
	PlayerScore(CurrentPlayer) = PlayerScore(CurrentPlayer) + (points * PlayfieldMultiplier)
	ExperienceCount = ExperienceCount + 1

    ' you may wish to check to see if the player has gotten an extra ball by a high score
    If PlayerScore(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
        AwardExtraBall
        Special1Awarded(CurrentPlayer) = True
    End If
    If PlayerScore(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
        AwardExtraBall
        Special2Awarded(CurrentPlayer) = True
    End If
    If PlayerScore(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
        AwardExtraBall
        Special3Awarded(CurrentPlayer) = True
    End If
End Sub

Sub AddScore2(points)
	PlayerScore(CurrentPlayer) = PlayerScore(CurrentPlayer) + points
End Sub
' Add bonus to the bonuspoints AND update the score board
Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If(Tilted = False) Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    End if
End Sub

Sub AwardExtraBall()
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
	PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
	if UseUltraDMD = 1 then UDMD "EXTRABALL", "", 3000
	DOF 451, DOFPulse
	PuPEvent 835
'	LiExtra.state = 1 
'    LightShootAgain.State = 1
    LightEffect 2
End Sub


'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

    If TypeName(MyLight) = "Light" Then

        If FinalState = 2 Then
            FinalState = MyLight.State 'Keep the current light state
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then

        Dim steps

        ' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
        If FinalState = 2 Then                      'Keep the current flasher state
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

        ' Start blink timer and create timer subroutine
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub


' ********************************
'   Table info & Attract Mode
' ********************************

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 40, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 40, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqRightOn, 30, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqLeftOn, 30, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe1VertOn, 50, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
'Dim HoleBonus, BumperBonus, ALLRampBonus, RampBonus1, RampBonus2, RampBonus3, MulitballBonus, TargetBonus    
Dim JackpotsBonus, RaiderBonus, CroftBonus, RelicsBonus, ArtefactsBonus

Sub Game_Init() 'called at the start of a new game
	playclear pBackglass
    Dim i, j
	playclear pMusic
	ResetEvents
'	TargetBonus = 0
	'bumperHits = 100
'	BumperBonus = 0
'	MulitballBonus = 0
	'BallInHole = 0
    TurnOffPlayfieldLights()
End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
Dim i
TargetBonus = 0
bBallSaverReady = True
End Sub

Sub ResetNewBallLights()    'turn on or off the needed lights before a new ball is released
	gi1.state = 1
	gi2.state = 1
	gi3.state = 1
	gi4.state = 1
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub should do something like this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************


'****************************************************************
'	ZSLG: Slingshots
'****************************************************************

' RStep and LStep are the variables that increment the animation
Dim RStep, LStep, LStep2

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	PlaySoundAt SoundFXDOF("", 104, DOFPulse, DOFcontactors), Sling1
	RandomSoundSlingshotRight Sling1
    DOF 111, DOFPulse
	Addscore 510
	If PersonnageActive(CurrentPlayer) = 0 Then SlingCount(CurrentPlayer) = SlingCount(CurrentPlayer) + 4 Else SlingCount(CurrentPlayer) = SlingCount(CurrentPlayer) + 2
	CheckLightSlingCount
	CheckMissionDispo
	FlashForMs F1A004, 500, 250, 0
	RSling1.Visible = 1
	Sling1.TransY =  - 20   'Sling Metal Bracket
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	RightSlingShot.TimerInterval = 10
	'   vpmTimer.PulseSw 52	'Slingshot Rom Switch
	RandomSoundSlingshotRight Sling1
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 3
			RSLing1.Visible = 0
			RSLing2.Visible = 1
			Sling1.TransY =  - 10
		Case 4
			RSLing2.Visible = 0
			Sling1.TransY = 0
			RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	PlaySoundAt SoundFXDOF("", 103, DOFPulse, DOFcontactors), Sling2
	RandomSoundSlingshotLeft Sling2
    DOF 110, DOFPulse
	Addscore 510
	If PersonnageActive(CurrentPlayer) = 0 Then SlingCount(CurrentPlayer) = SlingCount(CurrentPlayer) + 4 Else SlingCount(CurrentPlayer) = SlingCount(CurrentPlayer) + 2
	CheckLightSlingCount
	CheckMissionDispo
'	FlashForMs F1A001, 1000, 50, 0
	FlashForMs F1A001, 500, 250, 0
	LSling1.Visible = 1
	Sling2.TransY =  - 20   'Sling Metal Bracket
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval = 10
	'   vpmTimer.PulseSw 51	'Slingshot Rom Switch
	RandomSoundSlingshotLeft Sling2
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3
			LSLing1.Visible = 0
			LSLing2.Visible = 1
			Sling2.TransY =  - 10
		Case 4
			LSLing2.Visible = 0
			Sling2.TransY = 0
			LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

Sub LeftSlingShot2_Slingshot
	LS.VelocityCorrect(ActiveBall)
	Addscore 510
	PlaySoundAt SoundFXDOF("Sling_R", 105, DOFPulse, DOFcontactors), Sling2
	DOF 110, DOFPulse
	If PersonnageActive(CurrentPlayer) = 0 Then SlingCount(CurrentPlayer) = SlingCount(CurrentPlayer) + 4 Else SlingCount(CurrentPlayer) = SlingCount(CurrentPlayer) + 2
	FlashForMs F1A009, 500, 250, 0
	LSling002.Visible = 1
	Sling001.TransY =  - 20   'Sling Metal Bracket
	LStep2 = 0
	LeftSlingShot2.TimerEnabled = 1
	LeftSlingShot2.TimerInterval = 10
	'   vpmTimer.PulseSw 51	'Slingshot Rom Switch
	RandomSoundSlingshotLeft Sling001
End Sub

Sub LeftSlingShot2_Timer
	Select Case LStep2
		Case 3
			LSLing002.Visible = 0
			LSLing001.Visible = 1
			Sling001.TransY =  - 10
		Case 4
			LSLing001.Visible = 0
			Sling001.TransY = 0
			LeftSlingShot2.TimerEnabled = 0
	End Select
	LStep2 = LStep2 + 1
End Sub

Sub TestSlingShot_Slingshot
	TS.VelocityCorrect(ActiveBall)
End Sub

Sub ResetSlingCount
	SlingCount(CurrentPlayer) = 0
	bPersonnageSelect = False
	LightSwitch.State = 0
	LightConcertoE001.State = 0 
	LightConcertoE002.State = 0 
	LightConcertoE003.State = 0 
	LightConcertoE004.State = 0 
	LightConcertoE005.State = 0 
	LightConcertoE006.State = 0
	LightConcertoE007.State = 0
End Sub

Sub CheckLightSlingCount
	If SlingCount(CurrentPlayer) <= 10 Then LightConcertoE001.State = 0 : LightConcertoE002.State = 0 : LightConcertoE003.State = 0 : LightConcertoE004.State = 0 : LightConcertoE005.State = 0 : LightConcertoE006.State = 0 : LightConcertoE007.State = 0 : LightSwitch.State = 0
	If SlingCount(CurrentPlayer) >= 11 and SlingCount(CurrentPlayer) <= 20 Then LightConcertoE001.State = 1 : LightConcertoE002.State = 0 : LightConcertoE003.State = 0 : LightConcertoE004.State = 0 : LightConcertoE005.State = 0 : LightConcertoE006.State = 0 : LightConcertoE007.State = 0 : LightSwitch.State = 0 
	If SlingCount(CurrentPlayer) >= 21 and SlingCount(CurrentPlayer) <= 30 Then LightConcertoE001.State = 1 : LightConcertoE002.State = 1 : LightConcertoE003.State = 0 : LightConcertoE004.State = 0 : LightConcertoE005.State = 0 : LightConcertoE006.State = 0 : LightConcertoE007.State = 0 : LightSwitch.State = 0
	If SlingCount(CurrentPlayer) >= 31 and SlingCount(CurrentPlayer) <= 40 Then LightConcertoE001.State = 1 : LightConcertoE002.State = 1 : LightConcertoE003.State = 1 : LightConcertoE004.State = 0 : LightConcertoE005.State = 0 : LightConcertoE006.State = 0 : LightConcertoE007.State = 0 : LightSwitch.State = 0
	If SlingCount(CurrentPlayer) >= 41 and SlingCount(CurrentPlayer) <= 50 Then LightConcertoE001.State = 1 : LightConcertoE002.State = 1 : LightConcertoE003.State = 1 : LightConcertoE004.State = 1 : LightConcertoE005.State = 0 : LightConcertoE006.State = 0 : LightConcertoE007.State = 0 : LightSwitch.State = 0
	If SlingCount(CurrentPlayer) >= 51 and SlingCount(CurrentPlayer) <= 60 Then LightConcertoE001.State = 1 : LightConcertoE002.State = 1 : LightConcertoE003.State = 1 : LightConcertoE004.State = 1 : LightConcertoE005.State = 1 : LightConcertoE006.State = 0 : LightConcertoE007.State = 0 : LightSwitch.State = 0
	If SlingCount(CurrentPlayer) >= 61 and SlingCount(CurrentPlayer) <= 70 Then LightConcertoE001.State = 1 : LightConcertoE002.State = 1 : LightConcertoE003.State = 1 : LightConcertoE004.State = 1 : LightConcertoE005.State = 1 : LightConcertoE006.State = 1 : LightConcertoE007.State = 0 : LightSwitch.State = 0
	If SlingCount(CurrentPlayer) >= 71 and SlingCount(CurrentPlayer) <= 80 Then LightConcertoE001.State = 1 : LightConcertoE002.State = 1 : LightConcertoE003.State = 1 : LightConcertoE004.State = 1 : LightConcertoE005.State = 1 : LightConcertoE006.State = 1 : LightConcertoE007.State = 1 : LightSwitch.State = 0
	If SlingCount(CurrentPlayer) >= 81 and bPersonnageSelect = False Then LightConcertoE001.State = 1 : LightConcertoE002.State = 1 : LightConcertoE003.State = 1 : LightConcertoE004.State = 1 : LightConcertoE005.State = 1 : LightConcertoE006.State = 1 : LightSwitch.State = 2 : bPersonnageSelect = True : Playsound "CharSelect3" : UpdatePersonnageTMP : StartAutoSelectPerso : DOF 339, DOFPulse
	If SlingCount(CurrentPlayer) >= 81 and bPersonnageSelect = True Then LightConcertoE001.State = 1 : LightConcertoE002.State = 1 : LightConcertoE003.State = 1 : LightConcertoE004.State = 1 : LightConcertoE005.State = 1 : LightConcertoE006.State = 1 : LightSwitch.State = 2 : bPersonnageSelect = True
End Sub

'**************************
'Flupper bumpers
'**************************

Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0 : DNA45 = (NightDay-10)/20 : DNA90 = 0 : DayNightAdjust = 0.4
Else
	DNA30 = (NightDay-10)/30 : DNA45 = (NightDay-10)/45 : DNA90 = (NightDay-10)/90 : DayNightAdjust = NightDay/25
End If

Dim FlBumperFadeActual(6), FlBumperFadeTarget(6), FlBumperColor(6), FlBumperTop(6), FlBumperSmallLight(6), Flbumperbiglight(6)
Dim FlBumperDisk(6), FlBumperBase(6), FlBumperBulb(6), FlBumperscrews(6), FlBumperActive(6), FlBumperHighlight(6)
Dim cnt : For cnt = 1 to 6 : FlBumperActive(cnt) = False : Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight

FlInitBumper 1, "purple"
FlInitBumper 2, "purple"
'FlInitBumper 3, "red"

' ### uncomment the statement below to change the color for all bumpers ###
' Dim ind : For ind = 1 to 5 : FlInitBumper ind, "green" : next

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	' store all objects in an array for use in FlFadeBumper subroutine
	FlBumperFadeActual(nr) = 1 : FlBumperFadeTarget(nr) = 1.1: FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr) : FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr) : Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr) : Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr) : FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr): FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	' set the color for the two VPX lights
	select case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0) : FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0) : FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255) : FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255) : FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8) : FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32) : FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255) : MaterialColor "bumpertopmat" & nr, RGB(16,255,16) 
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "orange"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0) : FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8) : FlBumperBigLight(nr).colorfull = RGB(255,190,8)
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190) : FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255) : FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4) : FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50) : FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255) : FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255) : FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : 
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	end select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
'	UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
'               OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
'               float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 )* DayNightAdjust	

	select case FlBumperColor(nr)

		Case "blue" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(38-24*Z,130 - 98*Z,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20  + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 45 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z^3) / (0.5 + DNA90)

		Case "green"	
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(16 + 16 * sin(Z*3.14),255,16 + 16 * sin(Z*3.14)), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z^3) / (1 + DNA90)
		
		Case "red" 
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 16 - 11*Z + 16 * sin(Z*3.14),0), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z*4,8-Z*8)
		
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 100 - 22*z  + 16 * sin(Z*3.14),Z*32), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,100 + Z*50, 0)

		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 14 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20*Z,255-65*Z) : FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20*Z,255-65*Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z*36,220 - Z*90)

		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 1, RGB(30-27*Z^0.03,30-28*Z^0.01, 255), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z^3
			Flbumperbiglight(nr).intensity = 40 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255-240*(Z^0.1),255 - 240*(Z^0.1),255) : FlBumperSmallLight(nr).colorfull = RGB(255-200*z,255 - 200*Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255-190*Z,235 - z*180,220 + 35*Z)

		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 180 + 40*z, 48* Z), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)

		Case "purple" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(128-118*Z - 32 * sin(Z*3.14), 32-26*Z ,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15  + 200 * Z / (0.5 + DNA30) 
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 50 * Z / (1 + DNA45) 
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128-60*Z,32,255)


	end select
End Sub

Sub BumperTimer_Timer
	dim nr
	For nr = 1 to 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	next
End Sub

'************************** 
'Bumpers 
'************************** 
Dim bumperHits

Sub Bumper1_Hit()
'	CheckBumpers
	If NOT Tilted Then
		RandomSoundBumperTop Bumper1
		ShowDoubleThunder
        PlaySoundAt SoundFXDOF("", 109, DOFPulse, DOFContactors), Bumper1
        DOF 138, DOFPulse
		GiEffectBumper 5
		AddScore 915
		If PersonnageActive(CurrentPlayer) = 6 Then AddScore2 915
		If Mission3_State(CurrentPlayer) = 2 Then BumperCount = BumperCount + 1 : AwardMi3
		FlBumperFadeTarget(1) = 0
		bumper1.timerinterval = 100
		Bumper1.timerenabled = True
	End If
End Sub

Sub Bumper2_Hit()
'	CheckBumpers
	If NOT Tilted Then
		RandomSoundBumperMiddle Bumper2
		ShowDoubleThunder
        PlaySoundAt SoundFXDOF("", 107, DOFPulse, DOFContactors), Bumper2
        DOF 139, DOFPulse
		GiEffectBumper 5
		AddScore 915
		If PersonnageActive(CurrentPlayer) = 6 Then AddScore2 915
		If Mission3_State(CurrentPlayer) = 2 Then BumperCount = BumperCount + 1 : AwardMi3
		FlBumperFadeTarget(2) = 0
		bumper2.timerinterval = 100
		Bumper2.timerenabled = True
	End If
End Sub

Sub Bumper3_Hit()
'	CheckBumpers
	If NOT Tilted Then
 '       PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), Bumper3
		DOF 108, DOFPulse
        DOF 140, DOFPulse
		GiEffect 2
		AddScore 915
		ShakeEcho : Playsound "Sword1" 
		If PersonnageActive(CurrentPlayer) = 1 Then BattleEchoCount = BattleEchoCount + 2 Else BattleEchoCount = BattleEchoCount + 1
		CheckLightBattelEcho
		If BattleEchoCount = 10 Then WinBattleEcho
		FlBumperFadeTarget(3) = 0
		bumper3.timerinterval = 100
		Bumper3.timerenabled = True
	End If
End Sub

Sub CheckLightBattelEcho
	If BattleEchoCount = 1 Then LiBattle001.State=1 : LiBattle002.State=1 : LiBattle003.State=1 : LiBattle004.State=1 : LiBattle005.State=2
	If BattleEchoCount = 2 Then LiBattle001.State=1 : LiBattle002.State=1 : LiBattle003.State=1 : LiBattle004.State=1 : LiBattle005.State=0
	If BattleEchoCount = 3 Then LiBattle001.State=1 : LiBattle002.State=1 : LiBattle003.State=1 : LiBattle004.State=2 : LiBattle005.State=0
	If BattleEchoCount = 4 Then LiBattle001.State=1 : LiBattle002.State=1 : LiBattle003.State=1 : LiBattle004.State=0 : LiBattle005.State=0
	If BattleEchoCount = 5 Then LiBattle001.State=1 : LiBattle002.State=1 : LiBattle003.State=2 : LiBattle004.State=0 : LiBattle005.State=0
	If BattleEchoCount = 6 Then LiBattle001.State=1 : LiBattle002.State=1 : LiBattle003.State=0 : LiBattle004.State=0 : LiBattle005.State=0
	If BattleEchoCount = 7 Then LiBattle001.State=1 : LiBattle002.State=2 : LiBattle003.State=0 : LiBattle004.State=0 : LiBattle005.State=0
	If BattleEchoCount = 8 Then LiBattle001.State=1 : LiBattle002.State=0 : LiBattle003.State=0 : LiBattle004.State=0 : LiBattle005.State=0
	If BattleEchoCount = 9 Then LiBattle001.State=2 : LiBattle002.State=0 : LiBattle003.State=0 : LiBattle004.State=0 : LiBattle005.State=0
End Sub

Sub Bumper1_Timer()
	FlBumperFadeTarget(1) = 1
End Sub

Sub Bumper2_Timer()
	FlBumperFadeTarget(2) = 1
End Sub

Sub Bumper3_Timer()
	FlBumperFadeTarget(3) = 1
End Sub
' Bumper Bonus
' 100000 i bonus after each 100 hits

Sub CheckBumpers()
	If bumperHits <= 0 Then
	BumperBonus = BumperBonus + 1
	DMD "_", CL(1, "BUMPERS BONUS " & BumperBonus), "_", eNone, eBlink, eNone, 500, True, ""
	bumperHits = 100
	End If
End Sub

Sub ResetBumpers()
    bumperHits = 100
End Sub


'' ###################################
'' ###### copy script until here #####
'' ###################################

'*****************
'Gates
'*****************
sub Gate_Hit()
End Sub

'*****************
'Kickers
'*****************
Sub KickBackL_Hit
	If KickBackActive(CurrentPlayer) = True or PersonnageActive(CurrentPlayer) = 7 Then
	KickBackL.Kick -1, 45
	PlaySound "popper"
	PuPEvent 842
	Else
	KickBackL.Kick 90, 1
	End If
End Sub
Sub KickBackR_Hit
	If KickBackActive(CurrentPlayer) = True or PersonnageActive(CurrentPlayer) = 4 Then
	KickBackR.Kick 1, 45
	PlaySound "popper"
	PuPEvent 842
	Else
	KickBackR.Kick -90, 1
	End If
End Sub
Sub StartKickBack
	KickBackActive(CurrentPlayer) = True
	KickBackTimer.Enabled = True
End Sub 
Sub KickBackTimer_Timer()
	KickBackActive(CurrentPlayer) = False
	KickBackTimer.Enabled = False
End Sub

sub kicker001_hit()
'	FlashForMs F1A006, 500, 250, 0
'	FlashForMs F1A007, 500, 250, 0
'	FlashForMs Flasher003, 500, 50, 1
	'PlaySoundAt "fx_kicker_enter", kicker001:
	SoundSaucerLock
	Pupevent 900:Pupevent 901
	Kicker001.timerinterval = 2000
	kicker001.timerenabled = True
end Sub
Sub Kicker001_Timer()
	'PlaySoundAt "fx_kicker", kicker001
	SoundSaucerKick 1, Kicker001
	If MysteryState(CurrentPlayer) >= 5 Then AwardMytery
	kicker001.kick -125, 45, 1.4
	PlaySound ""
	kicker001.timerenabled = False
End Sub

Sub AwardMytery
	AddScore2 3500
	MysteryState(CurrentPlayer) = 0
	LiMystery.state = 0
	Select Case Int(Rnd * 5) + 1
		Case 1 'PF +1
			PlayfieldMultiplier = PlayfieldMultiplier + 1
			Playsound "Fx_UpBonus"
			LightEffect 7
			LightTagetM.state = 0
			LightTagetR.state = 0
			CheckLightBonus
			pDMDmessage2L "PLAYFIELD","Multiplier",1
			Pupevent 902
			if UseUltraDMD = 1 then UDMD "PLAYFIELD X", "", 3000
		Case 2 'Bonus +1
			BonusMultiplier = BonusMultiplier + 1
			Playsound "Fx_UpBonus"
			LightEffect 5
			CheckLightBonus
			ResetLightLane
			pDMDmessage2L "BONUS","Multiplier",1
			if UseUltraDMD = 1 then UDMD "BONUS X", "", 3000
		Case 3 'KickBack 15sec
			StartKickBack
			pDMDmessage2L "KICK BACK","15 SEC",1
			if UseUltraDMD = 1 then UDMD "KICK BACK", "15 SEC", 3000
		Case 4 'Add Ball
			vpmtimer.addtimer 1000, "AddMB2 '"
			pDMDmessage1L "ADD 1 BALL",1
			Pupevent 903
			if UseUltraDMD = 1 then UDMD "ADD", "1 BALL", 3000
		Case 5
			If PersonnageActive(CurrentPlayer) = 3 Then EnableBallSaver 25 Else EnableBallSaver 15
			pDMDmessage2L "BALL SAVE","ACTIVE",1
			if UseUltraDMD = 1 then UDMD "BALL SAVE", "ACTIVE", 3000
	End Select
End Sub 

sub kicker002_hit()
	SoundSaucerLock
	'PlaySoundAt "fx_kicker_enter", kicker002
	If LiForge001.State = 2 And LiForge002.State = 2 Then
	SelectPersoAutoNow
	DOF 126, DOFPulse
	Playsound "ArmorySelect3"
	bMissionSelect = True
	UpdateMissionTMP
	Else
	Kicker002.timerinterval = 1000
	kicker002.timerenabled = True
	End If
end Sub
Sub Kicker002_Timer()
	SoundSaucerKick 1, Kicker002
	'PlaySoundAt "fx_kicker", kicker002
	kicker002.kick 46, 45
	PlaySound ""
	kicker002.timerenabled = False
End Sub

Sub Kicker003_hit() 'MB LOCK
	PlaySoundAt "fx_kicker_enter", kicker003
	MBLockActive(CurrentPlayer) = False
	MBLock_Sav(CurrentPlayer) = MBLock_Sav(CurrentPlayer) + 1
	If MBLock_Sav(CurrentPlayer) = 2 Then Pupevent 823 : DOF 452, DOFPulse
	If MBLock_Sav(CurrentPlayer) = 4 Then Pupevent 823 : DOF 453, DOFPulse
	If MBLock_Sav(CurrentPlayer) >= 6 Then Mode_Multiball_Start
	AddMB1
	Kicker003.Destroyball
	WallLockOFF.Collidable = 1
	WallLockOFF.Visible = 1
	CheckLightMB
End Sub

Sub CheckPupMBperso
	If PersonnageActive(CurrentPlayer) = 0 Then Pupevent 824
	If PersonnageActive(CurrentPlayer) = 1 Then Pupevent 825
	If PersonnageActive(CurrentPlayer) = 2 Then Pupevent 826
	If PersonnageActive(CurrentPlayer) = 3 Then Pupevent 827
	If PersonnageActive(CurrentPlayer) = 4 Then Pupevent 828
	If PersonnageActive(CurrentPlayer) = 5 Then Pupevent 829
	If PersonnageActive(CurrentPlayer) = 6 Then Pupevent 830
	If PersonnageActive(CurrentPlayer) = 7 Then Pupevent 831
End Sub 

Sub Mode_Multiball_Start
	DOF 482, DOFOn
	pDMDmessage1L "MULTIBALL",1
	Pupevent 904 : PlaySound "multiball"
	if UseUltraDMD = 1 then UDMD "START", "MULTIBALL", 3000
	MBLock_Sav(CurrentPlayer) = 6
	StartBonusMB
	CheckPupMBperso
'	PlaySong "Mu_MB"
	StartMusic
	AddScore2 35000
	bMultiBallMode = True
	If PersonnageActive(CurrentPlayer) = 3 Then EnableBallSaver 25 Else EnableBallSaver 15
	vpmtimer.addtimer 3000, "AddMB2 '"
	vpmtimer.addtimer 6000, "AddMB2 '"
	StartKickBack
	CheckLightMB
End Sub

Sub Mode_Multiball_End 
	DOF 482, DOFOff
	bMultiBallMode = False
	MBLock_Sav(CurrentPlayer) = 0
	MBLockActive(CurrentPlayer) = False
	CheckLightMB
	EndBonusMB
	Pupevent 905
End Sub

Sub StartBonusMB
	BonusMBTimer.Enabled = True
	LiJBonus001.state=2
	LiJBonus002.state=2
	LiJBonus003.state=2
End Sub

Sub BonusMBTimer_Timer()
	EndBonusMB
End Sub

Sub EndBonusMB
	LiJBonus001.state=0
	LiJBonus002.state=0
	LiJBonus003.state=0
	BonusMBTimer.Enabled = False
End Sub

Sub Spinner001_Spin
	SoundSpinner Spinner001
    If Tilted Then Exit Sub
	AddScore 350
	If SpinnerModeActive = True Then 
		If PersonnageActive(CurrentPlayer) = 3 Then 
			SpinnerCount = SpinnerCount + 2
		Else
			SpinnerCount = SpinnerCount + 1
		End If
	End If 
	If SpinnerCount >= 30 And MBLock_Sav(CurrentPlayer) = 0 Then MBLock_Sav(CurrentPlayer) = 1 : CheckLightMB : SpinnerCount = 0 : pDMDmessage2L "Lock","Is Lit",1 : PlaySound "lockislit" : pDMDLabelStrobeImage "Magna1", 1000, 0
	If SpinnerCount >= 40 And MBLock_Sav(CurrentPlayer) = 2 Then MBLock_Sav(CurrentPlayer) = 3 : CheckLightMB : SpinnerCount = 0 : pDMDmessage2L "Lock","Is Lit",1 : PlaySound "lockislit" : pDMDLabelStrobeImage "Magna1", 1000, 0
	If SpinnerCount >= 50 And MBLock_Sav(CurrentPlayer) = 4 Then MBLock_Sav(CurrentPlayer) = 5 : CheckLightMB : SpinnerCount = 0
End Sub

Sub CheckLightMB
	If MBLock_Sav(CurrentPlayer) = 0 Then LiLOCK.State = 0 : LiMB.State = 0 : WallLockOFF.Collidable = 1 : WallLockOFF.Visible = 1 : TopMagnet.MagnetOn = False : MBLockActive(CurrentPlayer) = False : SpinnerModeActive = True
	If MBLock_Sav(CurrentPlayer) = 1 Then LiLOCK.State = 2 : LiMB.State = 0 : WallLockOFF.Collidable = 0 : WallLockOFF.Visible = 0 : TopMagnet.MagnetOn = True : MBLockActive(CurrentPlayer) = True : SpinnerModeActive = False
	If MBLock_Sav(CurrentPlayer) = 2 Then LiLOCK.State = 0 : LiMB.State = 0 : WallLockOFF.Collidable = 1 : WallLockOFF.Visible = 1 : TopMagnet.MagnetOn = False : MBLockActive(CurrentPlayer) = False : SpinnerModeActive = True
	If MBLock_Sav(CurrentPlayer) = 3 Then LiLOCK.State = 2 : LiMB.State = 0 : WallLockOFF.Collidable = 0 : WallLockOFF.Visible = 0 : TopMagnet.MagnetOn = True : MBLockActive(CurrentPlayer) = True : SpinnerModeActive = False
	If MBLock_Sav(CurrentPlayer) = 4 Then LiLOCK.State = 0 : LiMB.State = 0 : WallLockOFF.Collidable = 1 : WallLockOFF.Visible = 1 : TopMagnet.MagnetOn = False : MBLockActive(CurrentPlayer) = False : SpinnerModeActive = True
	If MBLock_Sav(CurrentPlayer) = 5 Then LiLOCK.State = 0 : LiMB.State = 2 : WallLockOFF.Collidable = 0 : WallLockOFF.Visible = 0 : TopMagnet.MagnetOn = True : MBLockActive(CurrentPlayer) = True : SpinnerModeActive = False
	If MBLock_Sav(CurrentPlayer) = 6 Then LiLOCK.State = 1 : LiMB.State = 1 : WallLockOFF.Collidable = 1 : WallLockOFF.Visible = 1 : TopMagnet.MagnetOn = False : MBLockActive(CurrentPlayer) = False : SpinnerModeActive = False
End Sub

sub kicker004_hit() 'MB ECHO
	ChangeBall(0)
	EchoCaptureCount(CurrentPlayer) = EchoCaptureCount(CurrentPlayer) +1
	EchoBonusCount(CurrentPlayer) = EchoBonusCount(CurrentPlayer) + 1
	PlaySoundAt "fx_kicker_enter", kicker004
	EchoGoldMooveTimer.enabled=True
	ModeEchoGold(CurrentPlayer) = False
	ResetLightEcho
	CheckPupMBperso
	StartBonusMB
	AddMB2
	vpmtimer.addtimer 3000, "AddMB2 '"
	pDMDmessage2L "ECHO","MULTIBALL",1
	if UseUltraDMD = 1 then UDMD "ECHO", "MULTIBALL", 3000
	If PersonnageActive(CurrentPlayer) = 3 Then EnableBallSaver 25 Else EnableBallSaver 15
	Kicker002.Createball
	Kicker002.timerinterval = 3000
	kicker002.timerenabled = True
	Kicker004.Destroyball
	Kicker004.timerenabled = False
end Sub

'***********
' Target
'***********

Sub Target001_Hit()
'	PlaySoundAtBall SoundFXDOF("", 114, DOFPulse, DOFTargets)
	PlayTargetSound
	AddScore 256
	If LightPerso001.State = 0 Then 
	LightPerso001.State = 2
	Elseif LightPerso001.State = 2 Then
	LightPerso001.State = 1
	DOF 334, DOFPulse
	CheckLightEcho
	End If
End Sub

Sub Target002_Hit()
'	PlaySoundAtBall SoundFXDOF("", 114, DOFPulse, DOFTargets)
	PlayTargetSound
	AddScore 256
	If LightPerso002.State = 0 Then 
	LightPerso002.State = 2
	Elseif LightPerso002.State = 2 Then
	LightPerso002.State = 1
	DOF 335, DOFPulse
	CheckLightEcho
	End If
End Sub
Sub Target003_Hit()
'	PlaySoundAtBall SoundFXDOF("", 114, DOFPulse, DOFTargets)
	PlayTargetSound
	AddScore 256
	If LightPerso003.State = 0 Then 
	LightPerso003.State = 2
	Elseif LightPerso003.State = 2 Then
	LightPerso003.State = 1
	DOF 336, DOFPulse
	CheckLightEcho
	End If
End Sub
Sub Target004_Hit()
'	PlaySoundAtBall SoundFXDOF("", 114, DOFPulse, DOFTargets)
	PlayTargetSound
	AddScore 256
	If LightPerso004.State = 0 Then 
	LightPerso004.State = 2
	Elseif LightPerso004.State = 2 Then
	LightPerso004.State = 1
	DOF 337, DOFPulse
	CheckLightEcho
	End If
End Sub

Sub CheckLightEcho
	If LightPerso001.State = 1 And LightPerso002.State = 1 And LightPerso003.State = 1 And LightPerso004.State = 1 And ModeEchoGold(CurrentPlayer) = False Then
		AddScore 8500
		StartBattleEcho
		DOF 338, DOFPulse
	End If
End Sub

Sub ResetLightEcho
		If EchoCaptureCount(CurrentPlayer) <=1 Then
'		If BallinGame(CurrentPlayer) = 1 Then
			LightPerso001.State=2 
			LightPerso002.State=2 
			LightPerso003.State=2 
			LightPerso004.State=2
		Else
			LightPerso001.State=0 
			LightPerso002.State=0 
			LightPerso003.State=0 
			LightPerso004.State=0
		End If
End Sub

Sub StartBattleEcho
'	Pupevent 906
	If EchoBonusCount(CurrentPlayer) = 0 or EchoBonusCount(CurrentPlayer) = 3 or EchoBonusCount(CurrentPlayer) = 6 or EchoBonusCount(CurrentPlayer) = 9 or EchoBonusCount(CurrentPlayer) >= 12 Then
		PersoCrownLcorp.visible = 1
		PersoCrownLAiles.visible = 1
		PersoCrownLArmy.visible = 1
		PersoCrownLCape.visible = 1
		PersoFeilian.visible = 0
		PersoJueEcho.visible = 0
		Pupevent 940
	Elseif EchoBonusCount(CurrentPlayer) = 1 or EchoBonusCount(CurrentPlayer) = 4 or EchoBonusCount(CurrentPlayer) = 7 or EchoBonusCount(CurrentPlayer) = 10 Then
		PersoCrownLcorp.visible = 0
		PersoCrownLAiles.visible = 0
		PersoCrownLArmy.visible = 0
		PersoCrownLCape.visible = 0
		PersoFeilian.visible = 1
		PersoJueEcho.visible = 0
		Pupevent 941
	Elseif EchoBonusCount(CurrentPlayer) = 2 or EchoBonusCount(CurrentPlayer) = 5 or EchoBonusCount(CurrentPlayer) = 8 or EchoBonusCount(CurrentPlayer) = 11 Then
		PersoCrownLcorp.visible = 0
		PersoCrownLAiles.visible = 0
		PersoCrownLArmy.visible = 0
		PersoCrownLCape.visible = 0
		PersoFeilian.visible = 0
		PersoJueEcho.visible = 1
		Pupevent 942
	End If
	Bumper3.collidable=True
	LiBattle001.State=1
	LiBattle002.State=1
	LiBattle003.State=1
	LiBattle004.State=1
	LiBattle005.State=1
	BattleEchoCount = 0
	StopSpinnerMotor
End Sub

Sub WinBattleEcho
	ModeEchoGold(CurrentPlayer) = True
	AddScore 25000: Playsound "EchoWin1" 
	If PersonnageActive(CurrentPlayer) = 1 or PersonnageActive(CurrentPlayer) = 2 Then AddScore2 50000
	If EchoBonusCount(CurrentPlayer) = 0 or EchoBonusCount(CurrentPlayer) = 3 or EchoBonusCount(CurrentPlayer) = 6 or EchoBonusCount(CurrentPlayer) = 9 or EchoBonusCount(CurrentPlayer) >= 12 Then
		PersoCrownLgold.visible = 1
		PersoFeiliangold.visible = 0
		PersoJuegold.visible = 0
	Elseif EchoBonusCount(CurrentPlayer) = 1 or EchoBonusCount(CurrentPlayer) = 4 or EchoBonusCount(CurrentPlayer) = 7 or EchoBonusCount(CurrentPlayer) = 10 Then
		PersoCrownLgold.visible = 0
		PersoFeiliangold.visible = 1
		PersoJuegold.visible = 0
	Elseif EchoBonusCount(CurrentPlayer) = 2 or EchoBonusCount(CurrentPlayer) = 5 or EchoBonusCount(CurrentPlayer) = 8 or EchoBonusCount(CurrentPlayer) = 11 Then
		PersoCrownLgold.visible = 0
		PersoFeiliangold.visible = 0
		PersoJuegold.visible = 1
	End If
	EchoGoldMooveTimer.enabled=True
	StopBattleEcho
End Sub

Sub StopBattleEcho
	PersoCrownLcorp.visible = 0
	PersoCrownLAiles.visible = 0
	PersoCrownLArmy.visible = 0
	PersoCrownLCape.visible = 0
	PersoFeilian.visible = 0
	PersoJueEcho.visible = 0
	Bumper3.collidable=False
	LiBattle001.State=0
	LiBattle002.State=0
	LiBattle003.State=0
	LiBattle004.State=0
	LiBattle005.State=0
	If ModeEchoGold(CurrentPlayer) = True Then
		LightPerso001.State=1 
		LightPerso002.State=1 
		LightPerso003.State=1 
		LightPerso004.State=1
	Else
		ResetLightEcho
	End If 
End Sub

Sub Target005_Hit()
'	PlaySoundAtBall SoundFXDOF("", 114, DOFPulse, DOFTargets)
	PlayTargetSound
	AddScore 256 
	If LightTagetL.State = 0 Then
		If ModeEchoGold(CurrentPlayer) = False And BattleEchoCount = 0 Then
		StartSpinnerMotor 
		End If
	End If
End Sub
Sub Target006_Hit()
'	PlaySoundAtBall SoundFXDOF("", 114, DOFPulse, DOFTargets)
	AddScore 256
	If LightTagetM.State = 0 Then
		If PersonnageActive(CurrentPlayer) = 6 Then 
			LightTagetM.State = 1 : CheckPlayfieldX
		Else 
		LightTagetM.State = 2
		End If
	Elseif LightTagetM.State = 2 Then
	LightTagetM.State=1
	CheckPlayfieldX
	End If
End Sub
Sub Target007_Hit()
'	PlaySoundAtBall SoundFXDOF("", 114, DOFPulse, DOFTargets)
	PlayTargetSound
	AddScore 256
	If LightTagetR.State = 0 Then
		If PersonnageActive(CurrentPlayer) = 6 Then 
			LightTagetR.State = 1 : CheckPlayfieldX
		Else 
		LightTagetR.State = 2
		End If
	Elseif LightTagetR.State = 2 Then
	LightTagetR.State=1
	CheckPlayfieldX
	End If
End Sub

'*****************
'Triggers
'*****************
Sub Trigger001_Hit()
	AddScore 155
End Sub

Sub Trigger002_Hit() 'Orbite L Astrite
	AddScore 155
	AwardJackpot
	CheckJump
	If LiJBonus001.State = 2 or LiJBonus003.State = 2 Then AddScore2 5000
	If PersonnageActive(CurrentPlayer) = 2 or PersonnageActive(CurrentPlayer) = 6 Then 
		AstriteCount(CurrentPlayer) = AstriteCount(CurrentPlayer) + 2 
		pDMDLabelPulseText2 "AddAstrite","+2",1000,clWhite
	Else 
		AstriteCount(CurrentPlayer) = AstriteCount(CurrentPlayer) + 1
		pDMDLabelPulseText2 "AddAstrite","+1",1000,clWhite
	End If
	RessourceCount = RessourceCount + 1
	If Mission1_State(CurrentPlayer) = 2 Then OrbiteCount = OrbiteCount - 1 : AwardMi1
	If Mission2_State(CurrentPlayer) = 2 And LiMi001.State = 2 Then LightMI2Count = LightMI2Count + 1 : LiMi001.State = 0 : AwardMi2
	If Mission5_State(CurrentPlayer) = 2 And LiMi001.State = 1 Then
		LiMi001.State = 2
	Elseif Mission5_State(CurrentPlayer) = 2 And LiMi001.State = 2 Then 
		LightMI5Count = LightMI5Count + 1
		LiMi001.State = 0
		AwardMi5
	End If	
End Sub

Sub Trigger003_Hit() 'Orbite M MB
	AddScore 155
	DOF 480, DOFPulse
	CheckComboL
	If Mission1_State(CurrentPlayer) = 2 Then OrbiteCount = OrbiteCount - 1 : AwardMi1
	If Mission2_State(CurrentPlayer) = 2 And LiMi003.State = 2 Then LightMI2Count = LightMI2Count + 1 : LiMi003.State = 0 : AwardMi2
	If Mission5_State(CurrentPlayer) = 2 And LiMi003.State = 1 Then
		LiMi003.State = 2
	Elseif Mission5_State(CurrentPlayer) = 2 And LiMi003.State = 2 Then 
		LightMI5Count = LightMI5Count + 1
		LiMi003.State = 0
		AwardMi5
	End If	
End Sub

Sub Trigger004_Hit() 'Spinner
	AddScore 155
	DOF 481, DOFPulse
	If Mission2_State(CurrentPlayer) = 2 And LiMi005.State = 2 Then LightMI2Count = LightMI2Count + 1 : LiMi005.State = 0 : AwardMi2
	If Mission5_State(CurrentPlayer) = 2 And LiMi005.State = 1 Then
		LiMi005.State = 2
	Elseif Mission5_State(CurrentPlayer) = 2 And LiMi005.State = 2 Then 
		LightMI5Count = LightMI5Count + 1
		LiMi005.State = 0
		AwardMi5
	End If	
End Sub

Sub Trigger005_Hit() 'Orbite R Astrite
	AddScore 155
	DOF 481, DOFPulse
	If Mission1_State(CurrentPlayer) = 2 Then OrbiteCount = OrbiteCount - 1 : AwardMi1
	If Mission2_State(CurrentPlayer) = 2 And LiMi007.State = 2 Then LightMI2Count = LightMI2Count + 1 : LiMi007.State = 0 : AwardMi2
	If Mission5_State(CurrentPlayer) = 2 And LiMi007.State = 1 Then
		LiMi007.State = 2
	Elseif Mission5_State(CurrentPlayer) = 2 And LiMi007.State = 2 Then 
		LightMI5Count = LightMI5Count + 1
		LiMi007.State = 0
		AwardMi5
	End If	
End Sub

Sub Trigger007_Hit()
	SoundOnOrbCollision
End Sub

Sub Trigger008_Hit() 'Ramp Forging Tibe
	AddScore 1550
	DOF 440, DOFPulse
	CheckComboM
	If LiJBonus002.State = 2 Then AddScore2 5000
	If PersonnageActive(CurrentPlayer) = 2 Then Addscore2 3100
	If PersonnageActive(CurrentPlayer) = 0 or PersonnageActive(CurrentPlayer) = 1 or PersonnageActive(CurrentPlayer) = 7 Then 
		ForgingTibeCount(CurrentPlayer) = ForgingTibeCount(CurrentPlayer) + 2 
		pDMDLabelPulseText2 "AddForgingTibe","+2",1000,clGold
	Else 
		ForgingTibeCount(CurrentPlayer) = ForgingTibeCount(CurrentPlayer) + 1
		pDMDLabelPulseText2 "AddForgingTibe","+1",1000,clGold
	End If
	RessourceCount = RessourceCount + 1
	If Mission2_State(CurrentPlayer) = 2 And LiMi002.State = 2 Then LightMI2Count = LightMI2Count + 1 : LiMi002.State = 0 : AwardMi2
	If Mission4_State(CurrentPlayer) = 2 And LiMi002.State = 1 Then
		LiMi002.State = 2
	Elseif Mission4_State(CurrentPlayer) = 2 And LiMi002.State = 2 Then 
		LightMI4Count = LightMI4Count + 1
		LiMi002.State = 0
		AwardMi4
	End If	
	If Mission5_State(CurrentPlayer) = 2 And LiMi002.State = 1 Then
		LiMi002.State = 2
	Elseif Mission5_State(CurrentPlayer) = 2 And LiMi002.State = 2 Then 
		LightMI5Count = LightMI5Count + 1
		LiMi002.State = 0
		AwardMi5
	End If	
End Sub

Sub Trigger009_Hit() 'Ramp R Corals
	AddScore 1550
	DOF 441, DOFPulse
	If PersonnageActive(CurrentPlayer) = 2 Then Addscore2 3100
	If PersonnageActive(CurrentPlayer) = 0 or PersonnageActive(CurrentPlayer) = 2 or PersonnageActive(CurrentPlayer) = 3 Then 
		CoralsCount(CurrentPlayer) = CoralsCount(CurrentPlayer) + 2 
		pDMDLabelPulseText2 "AddCorals","+2",1000,clVioletBlue
	Else 
		CoralsCount(CurrentPlayer) = CoralsCount(CurrentPlayer) + 1
		pDMDLabelPulseText2 "AddCorals","+1",1000,clVioletBlue
	End If
	RessourceCount = RessourceCount + 1
	If Mission2_State(CurrentPlayer) = 2 And LiMi006.State = 2 Then LightMI2Count = LightMI2Count + 1 : LiMi006.State = 0 : AwardMi2
	If Mission4_State(CurrentPlayer) = 2 And LiMi006.State = 1 Then
		LiMi006.State = 2
	Elseif Mission4_State(CurrentPlayer) = 2 And LiMi006.State = 2 Then 
		LightMI4Count = LightMI4Count + 1
		LiMi006.State = 0
		AwardMi4
	End If
	If Mission5_State(CurrentPlayer) = 2 And LiMi006.State = 1 Then
		LiMi006.State = 2
	Elseif Mission5_State(CurrentPlayer) = 2 And LiMi006.State = 2 Then 
		LightMI5Count = LightMI5Count + 1
		LiMi006.State = 0
		AwardMi5
	End If	
End Sub

Sub Trigger010_Hit() 'Ramp L Lustrous Tibe
	AddScore 1550
	DOF 441, DOFPulse
	If PersonnageActive(CurrentPlayer) = 2 Then Addscore2 3100
	If PersonnageActive(CurrentPlayer) = 0 or PersonnageActive(CurrentPlayer) = 4 or PersonnageActive(CurrentPlayer) = 5 Then 
		LustrousTibeCount(CurrentPlayer) = LustrousTibeCount(CurrentPlayer) + 2
		pDMDLabelPulseText2 "AddLustrousTibe","+2",1000,clBlue
	Else 
		LustrousTibeCount(CurrentPlayer) = LustrousTibeCount(CurrentPlayer) + 1
		pDMDLabelPulseText2 "AddLustrousTibe","+1",1000,clBlue
	End If
	RessourceCount = RessourceCount + 1
	If Mission2_State(CurrentPlayer) = 2 And LiMi004.State = 2 Then LightMI2Count = LightMI2Count + 1 : LiMi004.State = 0 : AwardMi2
	If Mission4_State(CurrentPlayer) = 2 And LiMi004.State = 1 Then
		LiMi004.State = 2
	Elseif Mission4_State(CurrentPlayer) = 2 And LiMi004.State = 2 Then 
		LightMI4Count = LightMI4Count + 1
		LiMi004.State = 0
		AwardMi4
	End If
	If Mission5_State(CurrentPlayer) = 2 And LiMi004.State = 1 Then
		LiMi004.State = 2
	Elseif Mission5_State(CurrentPlayer) = 2 And LiMi004.State = 2 Then 
		LightMI5Count = LightMI5Count + 1
		LiMi004.State = 0
		AwardMi5
	End If	
End Sub
Sub Trigger011_Hit() 'Ramp Mystery
	AddScore 1550
	DOF 440, DOFPulse
	If PersonnageActive(CurrentPlayer) = 2 Then Addscore2 3100
	If PersonnageActive(CurrentPlayer) = 5 Then MysteryState(CurrentPlayer) = MysteryState(CurrentPlayer) + 2 Else MysteryState(CurrentPlayer) = MysteryState(CurrentPlayer) + 1
	If MysteryState(CurrentPlayer) >= 5 Then LiMystery.State = 2
	If Mission4_State(CurrentPlayer) = 2 And LiMysteryScoop.State = 1 Then
		LiMysteryScoop.State = 2
	Elseif Mission4_State(CurrentPlayer) = 2 And LiMysteryScoop.State = 2 Then 
		LightMI4Count = LightMI4Count + 1
		LiMysteryScoop.State = 0
		AwardMi4
	End If
	If Mission5_State(CurrentPlayer) = 2 And LiMysteryScoop.State = 1 Then
		LiMysteryScoop.State = 2
	Elseif Mission5_State(CurrentPlayer) = 2 And LiMysteryScoop.State = 2 Then 
		LightMI5Count = LightMI5Count + 1
		LiMysteryScoop.State = 0
		AwardMi5
	End If	
End Sub

Sub TriggerDebug_Hit()
	If BallsOnPlayfield = 0 Then TriggerDebug.DestroyBall
End Sub

Sub TriggerTEST_Hit()

	If FirstShoot = True And bPersonnageSelect = True Then 'Debug Selection Pulnger Analogique
		FirstShoot = False
		PersoSwitchCount = PersoSwitchCount + 1
		UpdatePersonnage
		ResetSlingCount
		SelectPersoAutoTimer.Enabled = False
		CountTimerValueTimer.Enabled = False
		puPlayer.LabelSet pDMD,"AutoSelecTime","AutoSelect in : " & FormatNumber(CountTimerValue, 0),0,""
	End If
'	Startmi5
'	pDMDmessage2L "JACKPOT","" & FormatNumber(JackpotValue(CurrentPlayer), 0),1
'	pDMDLabelFadePulse "Texte2a", 2500, clRed
'	pDMDmessage2L "Test 1","!CAREFUL!",1
'	pDMDmessage1L "!CAREFUL!",1
'	pDMDLabelFadePulse "Texte2a", 2500, clRed
'   pDMDLabelFadePulse "Texte2b", 2500, clNavy
'	pDMDLabelPulseText "JackpotValue2","" & FormatNumber(JackpotValue(CurrentPlayer), 0),2000,clBlue
'	pDMDLabelPulseText2 "AddAstrite","+1",1000,clWhite
'	pDMDLabelPulseText2 "AddForgingTibe","+1",1000,clGold
'	pDMDLabelPulseText2 "AddLustrousTibe","+1",1000,clBlue
'	pDMDLabelPulseText2 "AddCorals","+1",1000,clVioletBlue
'	pDMDLabelPulseImageEX "Thunder1",1000,0
'	pDMDLabelWiggleImageEX "Thunder1", 300, 0, -10, 10
'	pDMDLabelStrobeImage "Thunder001", 300, 0
'	pDMDLabelStrobeImage "Thunder002", 300, 0
'	pDMDLabelStrobeImage "Thunder003", 300, 0
'	pDMDLabelStrobeImage "Thunder004", 300, 0
'	pDMDLabelStrobeImage "Thunder005", 300, 0
'	pDMDLabelStrobeImage "Thunder006", 300, 0
'	pDMDLabelStrobeImage "Thunder007", 300, 0
'	pDMDLabelStrobeImage "Thunder008", 300, 0
'	pDMDLabelWiggleImageEX "Cirle1", 300, 0, -10, 10
'	pDMDLabelWiggleImageEX "Magna1", 300, 0, -10, 10

'ok	pDMDLabelPulseImage "Cirle1",800,0
'	pDMDLabelPulseImage "Cirle2",900,0
'	pDMDmessage2L "Mission Rectifier","Go Orbite Light",1

'ok	pDMDmessage2L "Lock","Is Lit",1
'ok	pDMDLabelStrobeImage "Magna1", 1000, 0
'	HighScorelabels
'	pDMDLabelShow "MissSelect1"
'	LiForge001.State = 2
'	LiForge002.State = 2
'	KickBackActive(CurrentPlayer) = True
'	StartMi5
'	Set_LiPerso_State 2, "BLINK"
'	StartBattleEcho
'	StartSpinnerMotor
'	StopSpinnerMotor
'	PersoMooveTimer.enabled=True
'	bPersonnageSelect = True
'	TopMagnet.MagnetOn = True
'	vpmTimer.AddTimer 3000, "TopMagnet.MagnetOn = False '"

'	RocheMinerais.Color = RGB(0,255,0) 'VERT
'	RocheMinerais.Color = RGB(5,5,5) 'OFF
'	CastelRgoldMinerais.Color = RGB(0,255,0) 'VERT
'	CastelRgoldMinerais.Color = RGB(5,5,5)  'OFF
'	RocheMineraisHaut001.Color = RGB(255,255,0) 'JAUNE
'	RocheMineraisBas002.Color = RGB(0,255,0) 'VERT

'	DeparRampML.Image = "T_DungeonFort_D2"
'	PuPlayer.LabelSet pDMD,"HighScore","Enter Your Name :",1,""
'	PuPlayer.LabelSet pDMD,"HighScoreL1","A",1,""
'	PuPlayer.LabelSet pDMD,"HighScoreL2","A",1,""
'	PuPlayer.LabelSet pDMD,"HighScoreL3","A",1,""
'	LightCityON
'	GiEffect 4
'	LightEffect 2
End Sub

Sub Trigger006_Hit() 'StopMagnet
'	If LiLock.State = 2 Then
	If MBLockActive(CurrentPlayer) = True Then Playsound "Fx_Magnet"
	vpmTimer.AddTimer 3000, "TopMagnet.MagnetOn = False '"
'	End If
End Sub

'#################################
'FX SOUND
'#################################

Sub PlaySoundLock
	Select Case Int(Rnd * 4) + 1
		Case 1
			PlaySound "Fx_Lock001"
		Case 2
			PlaySound "Fx_Lock002"
		Case 3
			PlaySound "Fx_Lock003"
		Case 4
			PlaySound "Fx_Lock004"
	End Select
End Sub

Sub PlaySoundEND
	Select Case Int(Rnd * 3) + 1
		Case 1
			PlaySound "Fx_End001"
		Case 2
			PlaySound "Fx_End002"
		Case 3
			PlaySound "Fx_End003"
	End Select
End Sub

Sub PlaySoundThunder
	If ModeThunderSound = 1 Then
	Select Case Int(Rnd * 3) + 1
		Case 1
			PlaySound "Fx_Thunder1"
		Case 2
			PlaySound "Fx_Thunder2"
		Case 3
			PlaySound "Fx_Thunder3"
	End Select
	End If
End Sub


'*************
' Save CurrentPlayer
'*************
Sub ResetEvents
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to2"
	ChangeBall(0)
'	If B2SOn Then Controller.B2SSetscore 6, BallinGame(CurrentPlayer)
	ResetSlingCount
	PersonnageActive(CurrentPlayer) = 0
	UpMusicTmp
	If bGameInPLay = True Then UpdatePersonnageActif
	StopBattleEcho
	ModeEchoGold(CurrentPlayer) = False
	Mode_Multiball_End
	SpinnerCount = 0
	SpinnerModeActive = True
	StopSpinnerMotor
	EchoCaptureCount(CurrentPlayer) = 0
	EchoBonusCount(CurrentPlayer) = 0
	Mission1_State(CurrentPlayer) = 0
	OrbiteCount = 15
	Mission2_State(CurrentPlayer) = 0
	LightMI2Count = 0
	Mission3_State(CurrentPlayer) = 0
	BumperCount = 0
	Mission4_State(CurrentPlayer) = 0
	LightMI4Count = 0
	Mission5_State(CurrentPlayer) = 0
	LightMI5Count = 0
	KickBackActive(CurrentPlayer) = False
	BonusMultiplier = 1
	PlayfieldMultiplier = 1
	AstriteCount(CurrentPlayer) = 0
	ForgingTibeCount(CurrentPlayer) = 0
	LustrousTibeCount(CurrentPlayer) = 0
	CoralsCount(CurrentPlayer) = 0
	CheckLightBonus
	ResetLightLane
	LightTagetM.state = 0
	LightTagetR.state = 0
	ExperienceCount = 0 'Bonus1
	RessourceCount = 0 'Bonus2
	PersoSwitchCount = 0 'Bonus3
	MissionCount = 0 'Bonus4
	MysteryState(CurrentPlayer) = 0
	KickBackActive(CurrentPlayer) = False
	KickBackTimer.Enabled = False
	EndBonusMB
	ComboMcount = 0
	ComboLcount = 0
	ResetJump
	JackpotValue(CurrentPlayer) = 0
	SlingCount(CurrentPlayer) = 0
End Sub
	
Sub CheckEventPlayer
'	If B2SOn Then Controller.B2SSetscore 6, BallinGame(CurrentPlayer)
	CheckLightSlingCount
	UpdatePersonnageActif
	StopBattleEcho
	If ModeEchoGold(CurrentPlayer) = True And EchoGoldMooveON = False Then WinBattleEcho
	If ModeEchoGold(CurrentPlayer) = False And EchoGoldMooveON = True Then EchoGoldMooveTimer.enabled=True : ChangeBall(0)
	StopSpinnerMotor
	BonusMultiplier = 1
	PlayfieldMultiplier = 1
	CheckLightBonus
	ResetLightLane
	LightTagetM.state = 0
	LightTagetR.state = 0
	If MysteryState(CurrentPlayer) >= 5 Then LiMystery.State = 2 Else LiMystery.State = 0
	CheckLightMB
	StopMi1
	StopMi2
	StopMi3
	StopMi4
	StopMi5
	ExperienceCount = 0 'Bonus1
	RessourceCount = 0 'Bonus2
	PersoSwitchCount = 0 'Bonus3
	MissionCount = 0 'Bonus4
	KickBackActive(CurrentPlayer) = False
	KickBackTimer.Enabled = False
	EndBonusMB
	ComboMcount = 0
	ComboLcount = 0
	ResetJump
	CheckJackpotState
	If bOnTheFirstBall(CurrentPlayer) = True Then
		FirstShoot = True
		bPersonnageSelect = True
		Playsound "CharSelect3"
		UpdatePersonnageTMP
		'StartAutoSelectPerso
	Else 
		StartMusic
		UpMusicTmp
	End If
End Sub

'********TRIGGER LANE

Sub TLeftOutlane_Hit() '
	AddScore 1550
	DOF 300, DOFPulse
	If LeftOutlane.State = 0 Then
		If PersonnageActive(CurrentPlayer) = 0 Then 
			LeftOutlane.State = 1 
			DOF 335, DOFPulse
			CheckLightLane
		Else 
			LeftOutlane.State = 2
		End If
	Elseif LeftOutlane.State = 2 Then
	LeftOutlane.State=1
	Pupevent 908
	CheckLightLane
	End If
End Sub

Sub TLeftInlane_Hit() '
	AddScore 1550
	leftInlaneSpeedLimit
	If LeftInlane.State = 0 Then
		If PersonnageActive(CurrentPlayer) = 0 Then 
			LeftInlane.State = 1 
			DOF 335, DOFPulse
			CheckLightLane
		Else 
			LeftInlane.State = 2
		End If
	Elseif LeftInlane.State = 2 Then
	LeftInlane.State=1
	CheckLightLane
	End If
End Sub

Sub TRightInlane_Hit() '
	AddScore 1550
	rightInlaneSpeedLimit
	If RightInlane.State = 0 Then
		If PersonnageActive(CurrentPlayer) = 0 Then 
			RightInlane.State = 1 
			CheckLightLane
		Else 
			RightInlane.State = 2
		End If
	Elseif RightInlane.State = 2 Then
	RightInlane.State=1
	CheckLightLane
	End If
End Sub

Sub TRightOutlane_Hit() 'I
	AddScore 1550
	DOF 309, DOFPulse
	If RightOutlane.State = 0 Then
		If PersonnageActive(CurrentPlayer) = 0 Then 
			RightOutlane.State = 1 
			CheckLightLane
		Else 
			RightOutlane.State = 2
		End If
	Elseif RightOutlane.State = 2 Then
	RightOutlane.State=1
	Pupevent 909
	CheckLightLane
	End If
End Sub

Sub CheckLightLane
	If LeftOutlane.State=1 And LeftInlane.State=1 And RightInlane.State=1 And RightOutlane.State=1 Then
	AddScore 8500
		If BonusMultiplier <4 Then
		BonusMultiplier = BonusMultiplier + 1
		Playsound "Fx_UpBonus"
		LightEffect 5
		CheckLightBonus
		pDMDLabelPulseImage "Cirle1",800,0
		pDMDmessage2L "BONUS","Multiplier",1
		if UseUltraDMD = 1 then UDMD "BONUS X", "", 3000
		End If
	ResetLightLane
	End If
End Sub

Sub ResetLightLane
	LeftOutlane.State=0 
	LeftInlane.State=0 
	RightInlane.State=0 
	RightOutlane.State=0
End Sub

Sub CheckLightBonus
	If BonusMultiplier <= 1 Then LightBM1.state = 0 : LightBM2.state = 0 : LightBM3.state = 0
	If BonusMultiplier = 2 Then LightBM1.state = 1 : LightBM2.state = 0 : LightBM3.state = 0
	If BonusMultiplier = 3 Then LightBM1.state = 0 : LightBM2.state = 1 : LightBM3.state = 0
	If BonusMultiplier >= 4 Then LightBM1.state = 0 : LightBM2.state = 0 : LightBM3.state = 1 : BonusMultiplier = 5
	If PlayfieldMultiplier <= 1 Then LightPFM1.state = 0 : LightPFM2.state = 0 : LightPFM3.state = 0
	If PlayfieldMultiplier = 2 Then LightPFM1.state = 1 : LightPFM2.state = 0 : LightPFM3.state = 0 
	If PlayfieldMultiplier = 3 Then LightPFM1.state = 0 : LightPFM2.state = 1 : LightPFM3.state = 0
	If PlayfieldMultiplier >= 4 Then LightPFM1.state = 0 : LightPFM2.state = 0 : LightPFM3.state = 1 : PlayfieldMultiplier = 5
End Sub

Sub CheckPlayfieldX
	If LightTagetM.state = 1 And LightTagetR.state = 1 Then 
	PlayfieldMultiplier = PlayfieldMultiplier + 1
	Playsound "Fx_UpBonus"
	LightEffect 7
	LightTagetM.state = 0
	LightTagetR.state = 0
	CheckLightBonus
	pDMDLabelPulseImage "Cirle1",800,0
	pDMDmessage2L "PLAYFIELD","Multiplier",1
	if UseUltraDMD = 1 then UDMD "PLAYFIELD X", "", 3000
	End If
End Sub
'******************
' Captive Ball Subs
'******************
Sub CapTrigger1_Hit : cbRight.TrigHit ActiveBall : End Sub
Sub CapTrigger1_UnHit : cbRight.TrigHit 0 : End Sub
Sub CapWall1_Hit : cbRight.BallHit ActiveBall : End Sub
Sub CapKicker1a_Hit : cbRight.BallReturn Me : SoundOnOrbCollision : End Sub
Sub CapKicker1_Hit : SoundOnOrbCollision : End Sub

'***********************
' Glowball Section 
'***********************
Dim GlowBall, CustomBulbIntensity(4)
Dim  GBred(4)
Dim GBgreen(4)
Dim GBblue(4)
Dim CustomBallImage(4), CustomBallLogoMode(4), CustomBallDecal(4), CustomBallGlow(4)
Dim GlowAura,GlowIntensity,ChooseBall

GlowAura=230 'GlowBlob Auroa radius
GlowIntensity=25'Glowblob intensity

' blue GlowBall
CustomBallGlow(1) = 		True
GBred(1) = 1 : GBgreen(1)	= 33 : GBblue(1) = 105
' Vert GlowBall
CustomBallGlow(2) = 		True
GBred(2) = 1 : GBgreen(2)	= 33 : GBblue(2) = 5
' Rouge GlowBall
CustomBallGlow(3) = 		True
GBred(3) = 130 : GBgreen(3)	= 1 : GBblue(3) = 1
' Orange GlowBall
CustomBallGlow(4) = 		True
GBred(4) = 250 : GBgreen(4)	= 130 : GBblue(4) = 1

Dim Glowing(10)
Set Glowing(0) = Glowball1 : Set Glowing(1) = Glowball2 : Set Glowing(2) = Glowball3 : Set Glowing(3) = Glowball4 : Set Glowing(4) = Glowball4

'*** change ball appearance ***

Sub ChangeBall(ballnr)
	If ModeChangeBallActive = 0 Then
	Dim BOT, ii, col
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 4
		col = RGB(GBred(ballnr), GBgreen(ballnr), GBblue(ballnr))
		Glowing(ii).color = col : Glowing(ii).colorfull = col 
	Next
	End If
End Sub

' *** Ball Shadow code / Glow Ball code / Primitive Flipper Update ***

Dim BallShadowArray
BallShadowArray = Array (BallShadow1, BallShadow2, BallShadow3,BallShadow4,BallShadow5)

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
		If GlowBall and b < 4 Then
			If Glowing(b).state = 0 Then Glowing(b).state = 1 end if
			Glowing(b).BulbHaloHeight = BOT(b).z + 32
			Glowing(b).x = BOT(b).x
			Glowing(b).y = BOT(b).Y+10
			Glowing(b).falloff=GlowAura 'GlowBlob Auroa radius
			Glowing(b).intensity=GlowIntensity 'Glowblob intensity
		End If
	Next
End Sub
'************
' Rotation Light Bonus
'************
Sub RotateLaneLightsLeftUp
    Dim TempState
	TempState = RightInlane.State
    RightInlane.State = RightOutlane.State
	RightOuTlane.State = LeftOutlane.State
	LeftOutlane.State = LeftInlane.State
    LeftInlane.State = TempState
End Sub

Sub RotateLaneLightsRightUp
	Dim TempState
    TempState = LeftInlane.State
    LeftInlane.State = LeftOutlane.State
	LeftOutlane.State = RightOutlane.State
	RightOutlane.State = RightInlane.State
	RightInlane.State = TempState
End Sub


Sub UDMD (toptext, bottomtext, utime)
	If UseUltraDMD > 0 Then UltraDMD.DisplayScene00Ex "", toptext, 8, 14, bottomtext, 8,14, 14, utime, 14
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  High Scores
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	' load em up


	Dim hschecker:hschecker = 0

	Sub Loadhs
		Dim x
		x = LoadValue(TableName, "HighScore1")
		If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 4000000 End If

		x = LoadValue(TableName, "HighScore1Name")
		If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "TOM" End If

		x = LoadValue(TableName, "HighScore2")
		If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 2500000 End If

		x = LoadValue(TableName, "HighScore2Name")
		If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "AMA" End If

		x = LoadValue(TableName, "HighScore3")
		If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 1500000 End If

		x = LoadValue(TableName, "HighScore3Name")
		If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "LIA" End If

		x = LoadValue(TableName, "HighScore4")
		If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 1000000 End If

		x = LoadValue(TableName, "HighScore4Name")
		If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "EMI" End If

		x = LoadValue(TableName, "Credits")
		If(x <> "") then Credits = CInt(x) Else Credits = 0 End If

		x = LoadValue(TableName, "TotalGamesPlayed")
		If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If

	'	If hschecker = 0 Then
	'	checkorder
	'	End If
	End Sub

	Dim hs3,hs2,hs1,hs0,hsn3,hsn2,hsn1,hsn0


	Sub checkorder
		hschecker = 1
		hs3 = HighScore(3)
		hs2 = HighScore(2)
		hs1 = HighScore(1)
		hs0 = HighScore(0)
		hsn3 = HighScoreName(3)
		hsn2 = HighScoreName(2)
		hsn1 = HighScoreName(1)
		hsn0 = HighScoreName(0)
		If hs3 > hs0 Then
			HighScore(0) = hs3
			HighScoreName(0) = hsn3	
			HighScore(1) = hs0
			HighScoreName(1) = hsn0	
			HighScore(2) = hs1
			HighScoreName(2) = hsn1	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2

		ElseIf hs3 > hs1 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs3
			HighScoreName(1) = hsn3	
			HighScore(2) = hs1
			HighScoreName(2) = hsn1	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
		ElseIf hs3 > hs2 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs3
			HighScoreName(2) = hsn3	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
		ElseIf hs3 < hs2 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs2
			HighScoreName(2) = hsn2	
			HighScore(3) = hs3
			HighScoreName(3) = hsn3
		End If

		savehs
	End Sub


	Sub Savehs
		SaveValue TableName, "HighScore1", HighScore(0)
		SaveValue TableName, "HighScore1Name", HighScoreName(0)
		SaveValue TableName, "HighScore2", HighScore(1)
		SaveValue TableName, "HighScore2Name", HighScoreName(1)
		SaveValue TableName, "HighScore3", HighScore(2)
		SaveValue TableName, "HighScore3Name", HighScoreName(2)
		SaveValue TableName, "HighScore4", HighScore(3)
		SaveValue TableName, "HighScore4Name", HighScoreName(3)
		SaveValue TableName, "Credits", Credits
	End Sub

Sub Reseths
    HighScoreName(0) = "TOM"
    HighScoreName(1) = "GMN"
    HighScoreName(2) = "DAV"
    HighScoreName(3) = "DDD"
    HighScore(0) = 1000000
    HighScore(1) = 900000
    HighScore(2) = 500000
    HighScore(3) = 150000
    Savehs
End Sub

	Sub Savegp
		SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
		vpmtimer.addtimer 1000, "Loadhs'"
	End Sub


	' Initials

	Dim hsbModeActive:hsbModeActive = False
	Dim hsEnteredName
	Dim hsEnteredDigits(3)
	Dim hsCurrentDigit
	Dim hsValidLetters
	Dim hsCurrentLetter
	Dim hsLetterFlash

	' Check the scores to see if you got one

	Sub CheckHighscore()
	If halplay=0 Then
		Dim tmp
		tmp = PlayerScore(CurrentPlayer)
'		osbtempscore = Score(CurrentPlayer)

		If tmp > HighScore(0)Then 'add 1 credit for beating the highscore
			Credits = Credits + 1
'			DOF 125, DOFOn
		End If

		If tmp > HighScore(3) Then
			PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
			vpmtimer.addtimer 2000, "PlaySound ""vo_contratulationsgreatscore"" '"
			HighScore(3) = tmp
			'enter player's name
			HighScoreEntryInit()
		Else

			EndOfBallComplete()
		End If
	Else
		EndOfBallComplete()
		End If
	End Sub





	Sub HighScoreEntryInit()
		hsbModeActive = True
		PlaySound "vo_enteryourinitials"

		hsEnteredDigits(1) = "A"
		hsEnteredDigits(2) = " "
		hsEnteredDigits(3) = " "

		hsCurrentDigit = 1
		pDMDmessage2L "You Got a","High Score",1
		Pupevent 910
		Playsound "knocker"
		HighScoreDisplayName()
		HighScorelabels	
	End Sub

	' flipper moving around the letters

	Sub EnterHighScoreKey(keycode)
		If keycode = LeftFlipperKey Then
			Playsound "fx_Previous"
		If hsletter = 0 Then
			hsletter = 37
		Else
			hsLetter = hsLetter - 1
		End If
				HighScoreDisplayName()
		End If

		If keycode = RightFlipperKey Then
			Playsound "fx_Next"
				If hsletter = 37 Then
					hsletter = 0
				Else
					hsLetter = hsLetter + 1
				End If
				HighScoreDisplayName()
		End If

		If keycode = StartGameKey or keycode = PlungerKey Then
			PlaySound "success"
				If hsCurrentDigit = 3 Then
					If hsletter = 0 Then
						hsCurrentDigit = hsCurrentDigit -1
					Else
						assignletter
						vpmtimer.addtimer 700, "HighScoreCommitName()'"
						playclear pDMD
					End If
				End If
				If hsCurrentDigit < 3 Then
					If hsletter = 0 Then
						If hsCurrentDigit = 1 Then
						Else
							hsCurrentDigit = hsCurrentDigit -1
						End If
					Else
						assignletter
						hsCurrentDigit = hsCurrentDigit + 1
						HighScoreDisplayName()

					End If
				End If
		End if
	End Sub

	Dim hsletter
	hsletter = 1

	dim hsdigit:hsdigit = 1

	Sub assignletter
		if hscurrentdigit = 1 Then
			hsdigit = 1
		End If
		if hscurrentdigit = 2 Then
			hsdigit = 2
		End If
		if hscurrentdigit = 3 Then
			hsdigit = 3
		End If
		If hsletter = 1 Then 
			hsEnteredDigits(hsdigit) = "A"
		End If
		If hsletter = 2 Then 
			hsEnteredDigits(hsdigit) = "B"
		End If
		If hsletter = 3 Then 
			hsEnteredDigits(hsdigit) = "C"
		End If
		If hsletter = 4 Then 
			hsEnteredDigits(hsdigit) = "D"
		End If
		If hsletter = 5 Then 
			hsEnteredDigits(hsdigit) = "E"
		End If
		If hsletter = 6 Then 
			hsEnteredDigits(hsdigit) = "F"
		End If
		If hsletter = 7 Then 
			hsEnteredDigits(hsdigit) = "G"
		End If
		If hsletter = 8 Then 
			hsEnteredDigits(hsdigit) = "H"
		End If
		If hsletter = 9 Then 
			hsEnteredDigits(hsdigit) = "I"
		End If
		If hsletter = 10 Then 
			hsEnteredDigits(hsdigit) = "J"
		End If
		If hsletter = 11 Then 
			hsEnteredDigits(hsdigit) = "K"
		End If
		If hsletter = 12 Then 
			hsEnteredDigits(hsdigit) = "L"
		End If
		If hsletter = 13 Then 
			hsEnteredDigits(hsdigit) = "M"
		End If
		If hsletter = 14 Then 
			hsEnteredDigits(hsdigit) = "N"
		End If
		If hsletter = 15 Then 
			hsEnteredDigits(hsdigit) = "O"
		End If
		If hsletter = 16 Then 
			hsEnteredDigits(hsdigit) = "P"
		End If
		If hsletter = 17 Then 
			hsEnteredDigits(hsdigit) = "Q"
		End If
		If hsletter = 18 Then 
			hsEnteredDigits(hsdigit) = "R"
		End If
		If hsletter = 19 Then 
			hsEnteredDigits(hsdigit) = "S"
		End If
		If hsletter = 20 Then 
			hsEnteredDigits(hsdigit) = "T"
		End If
		If hsletter = 21 Then 
			hsEnteredDigits(hsdigit) = "U"
		End If
		If hsletter = 22 Then 
			hsEnteredDigits(hsdigit) = "V"
		End If
		If hsletter = 23 Then 
			hsEnteredDigits(hsdigit) = "W"
		End If
		If hsletter = 24 Then 
			hsEnteredDigits(hsdigit) = "X"
		End If
		If hsletter = 25 Then 
			hsEnteredDigits(hsdigit) = "Y"
		End If
		If hsletter = 26 Then 
			hsEnteredDigits(hsdigit) = "Z"
		End If
		If hsletter = 27 Then 
			hsEnteredDigits(hsdigit) = " "
		End If
		If hsletter = 28 Then 
			hsEnteredDigits(hsdigit) = "0"
		End If
		If hsletter = 29 Then 
			hsEnteredDigits(hsdigit) = "1"
		End If
		If hsletter = 30 Then 
			hsEnteredDigits(hsdigit) = "2"
		End If
		If hsletter = 31 Then 
			hsEnteredDigits(hsdigit) = "3"
		End If
		If hsletter = 32 Then 
			hsEnteredDigits(hsdigit) = "4"
		End If
		If hsletter = 33 Then 
			hsEnteredDigits(hsdigit) = "5"
		End If
		If hsletter = 34 Then 
			hsEnteredDigits(hsdigit) = "6"
		End If
		If hsletter = 35 Then 
			hsEnteredDigits(hsdigit) = "7"
		End If
		If hsletter = 36 Then 
			hsEnteredDigits(hsdigit) = "8"
		End If
		If hsletter = 37 Then 
			hsEnteredDigits(hsdigit) = "9"
		End If

	End Sub

	Sub HighScorelabels
		PuPlayer.LabelSet pDMD,"HighScore","Enter Your Name :",1,""
		PuPlayer.LabelSet pDMD,"HighScoreL1","A",1,""
		PuPlayer.LabelSet pDMD,"HighScoreL2","",1,""
		PuPlayer.LabelSet pDMD,"HighScoreL3","",1,""
'		PuPlayer.LabelSet pDMD,"HighScoreL4",PlayerScore(CurrentPlayer),1,""
		hsletter = 1
	End Sub

	Sub HighScoreDisplayName()

		Select case hsLetter
		Case 0
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","<",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","<",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","<",1,""
		Case 1
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","A",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","A",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","A",1,""
		Case 2
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","B",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","B",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","B",1,""
		Case 3
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","C",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","C",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","C",1,""
		Case 4
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","D",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","D",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","D",1,""
		Case 5
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","E",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","E",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","E",1,""
		Case 6
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","F",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","F",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","F",1,""
		Case 7
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","G",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","G",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","G",1,""
		Case 8
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","H",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","H",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","H",1,""
		Case 9
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","I",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","I",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","I",1,""
		Case 10
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","J",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","J",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","J",1,""
		Case 11
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","K",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","K",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","K",1,""
		Case 12
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","L",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","L",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","L",1,""
		Case 13
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","M",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","M",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","M",1,""
		Case 14
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","N",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","N",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","N",1,""
		Case 15
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","O",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","O",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","O",1,""
		Case 16
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","P",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","P",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","P",1,""
		Case 17
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","Q",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","Q",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","Q",1,""
		Case 18
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","R",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","R",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","R",1,""
		Case 19
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","S",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","S",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","S",1,""
		Case 20
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","T",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","T",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","T",1,""
		Case 21
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","U",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","U",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","U",1,""
		Case 22
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","V",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","V",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","V",1,""
		Case 23
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","W",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","W",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","W",1,""
		Case 24
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","X",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","X",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","X",1,""
		Case 25
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","Y",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","Y",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","Y",1,""
		Case 26
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","Z",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","Z",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","Z",1,""
		Case 27
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1"," ",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2"," ",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3"," ",1,""

		Case 28
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","0",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","0",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","0",1,""

		Case 29
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","1",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","1",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","1",1,""

		Case 30
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","2",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","2",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","2",1,""

		Case 31
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","3",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","3",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","3",1,""

		Case 32
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","4",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","4",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","4",1,""

		Case 33
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","5",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","5",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","5",1,""

		Case 34
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","6",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","6",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","6",1,""

		Case 35
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","7",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","7",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","7",1,""

		Case 36
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","8",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","8",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","8",1,""

		Case 37
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","9",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","9",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","9",1,""

		End Select
	End Sub

	' post the high score letters


	Sub HighScoreCommitName()
		playclear pDMD
		PuPlayer.SetLoop 2,0
		PuPlayer.SetLoop 7,0
		hsEnteredName = hsEnteredDigits(1) & hsEnteredDigits(2) & hsEnteredDigits(3)
		HighScoreName(3) = hsEnteredName
		checkorder
		osbtemp = hsEnteredName
		if osbkey="" Then
		Else
		SubmitOSBScore
		end If
		EndOfBallComplete()
		PuPlayer.LabelSet pDMD,"HighScore","",1,""
		PuPlayer.LabelSet pDMD,"HighScoreL1","",1,""
		PuPlayer.LabelSet pDMD,"HighScoreL2"," ",1,""
		PuPlayer.LabelSet pDMD,"HighScoreL3"," ",1,""
'		PuPlayer.LabelSet pDMD,"HighScoreL4"," ",1,""
		hsbModeActive = False
	End Sub

'-------------
'------Music Pup
'-------------
Dim curSong : curSong = ""
Dim MusicDir : MusicDir = "Music" ' Nom du sous-dossier dans ton PupPack (ou "" si à la racine)
Dim VolBGMusic : VolBGMusic = 0.8 ' Volume de 0 à 1 (80%)
Dim bMediaSet(15) ' Tableau pour suivre l'état des canaux

Sub playMusic(fileName)
    If curSong <> fileName Then 
        playclear pMusic
        curSong = fileName
        ' Paramètres : Nom, Dossier, Canal, Cine, Length, Next, Vol, Priority
        playmedia fileName, MusicDir, pMusic, "", -1, "", 1, 1
    End If 
End Sub 

Sub playmedia(name, playlist, chan, cinematic, length, nextitem, audiolevel, priority)
    bMediaSet(chan) = True
    
    ' Calcul du volume (on multiplie par 100 pour PuP)
    Dim finalVol
    finalVol = audiolevel * VolBGMusic * 100
    
    ' La commande magique qui fonctionne sur toutes les versions
    PuPlayer.playlistplayex chan, playlist, name, finalVol, priority
    
    ' On force la boucle (Loop) si c'est de la musique (length = -1)
    If chan = pMusic Then PuPlayer.SetLoop chan, 1
End Sub

Sub playclear(chan)
'		debug.print "play clear'd " & chan

		if chan = pMusic Then
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":0 }"
		End If

		if chan = pBackglass Then
			PuPlayer.playstop pDMD
		End If
End Sub

Sub StartMusic
    ' On récupère l'index du personnage
    Dim charID
    charID = PersonnageActive(CurrentPlayer)
'    If bMultiBallMode = False Then
    ' On appelle la fonction de lecture directe sur le canal pMusic
    StartMusic2 charID
'	Else
'		playMusic "Multiball_Test.mp3"
'	End If
End Sub

Sub StartMusic2(MusicIdx)
    Dim MusicFile
    
    Select Case MusicIdx
        Case 0: MusicFile = "Rover1.mp3"
        Case 1: MusicFile = "Canterella1.mp3"
        Case 2: MusicFile = "Zani1.mp3"
        Case 3: MusicFile = "Augusta1.mp3"
        Case 4: MusicFile = "Jiyan1.mp3"
        Case 5: MusicFile = "Iuno1.mp3"
        Case 6: MusicFile = "Carlotta1.mp3"
        Case 7: MusicFile = "Galbrena1.mp3"
    End Select

    ' On appelle le moteur qu'on vient d'installer
    If HasPuP Then
        playMusic MusicFile
    End If
End Sub

Dim CurVol : CurVol = MaxTableVolume    ' Volume actuel (0-100)
'Dim CurVol : CurVol = 100    ' Volume actuel (0-100)
Dim TargetVol : TargetVol = MaxTableVolume ' Volume vers lequel on veut aller
'Dim TargetVol : TargetVol = 100 ' Volume vers lequel on veut aller
Dim FadeStep : FadeStep = 2    ' Vitesse du fondu (plus c'est haut, plus c'est rapide)

Sub VolumeTimer_Timer()
    If CurVol < TargetVol Then
        CurVol = CurVol + FadeStep
        If CurVol > TargetVol Then CurVol = TargetVol
    ElseIf CurVol > TargetVol Then
        CurVol = CurVol - FadeStep
        If CurVol < TargetVol Then CurVol = TargetVol
    Else
        ' On a atteint la cible, on éteint le timer pour économiser des ressources
        VolumeTimer.Enabled = False
    End If
    
    ' On envoie la mise à jour du volume à PuP
    If HasPuP Then
        PuPlayer.SendMSG "{ ""mt"":301, ""SN"": " & pMusic & ", ""FN"":11, ""VL"":" & Int(CurVol) & " }"
    End If
End Sub

Sub DownMusicTmp
    TargetVol = 20  ' On veut descendre à 20%
    VolumeTimer.Enabled = True
End Sub

Sub UpMusicTmp
	If bGameInPLay = True Then
    ' On remonte le volume à 100% (ou ton VolBGMusic * 100)
'   TargetVol = 100 ' On veut remonter à 100%
	TargetVol = MaxTableVolume ' On veut remonter à 100%
	VolumeTimer.Enabled = True
	End If
End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  ATTRACT MODE
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

	Sub StartAttractMode()
		DOF 323, DOFOn   'DOF MX - Attract Mode ON
		bAttractMode = True
		playclear pMusic
		StartLightSeq
		Set_LiPerso_State 1, "OFF"
		Set_LiPerso_State 2, "OFF"
		Set_LiPerso_State 3, "OFF"
		Set_LiPerso_State 4, "OFF"
		Set_LiPerso_State 5, "OFF"
		Set_LiPerso_State 6, "OFF"
		Set_LiPerso_State 7, "OFF"
		Set_LiPerso_State 8, "OFF"
	End Sub

	Sub StopAttractMode()
		pDMDStartGame
		DOF 323, DOFOff   'DOF MX - Attract Mode Off
		bAttractMode = False
		LightSeqAttract.StopPlay		
	'StopSong
	End Sub

'-----------------------
'---------VR SETTING
'-----------------------

DIM VRThings
DIM VRRoom

Sub LoadVRRoom
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
	for each VRThings in VR_Mega:VRThings.visible = 0:Next
	VR_Fog_far_Fog_0_001.StopAnim
	VR_Fog_near_Fog_0_001.StopAnim

	If RenderingMode = 2 or VRTest Then
		VRRoom = VRRoomChoice
		'disable table objects that should not be visible
		ramp17.visible = 0
		Wall001.sidevisible = 0
		Wall002.sidevisible = 0
		ramp16.visible = 0
		ramp15.visible = 0
	Else
		VRRoom = 0
	End If
	If VRRoom = 1 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next
	End If
	If VRRoom = 3 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 1:Next
		VR_Fog_far_Fog_0_001.PlayAnimEndless(0.05)
		VR_Fog_near_Fog_0_001.PlayAnimEndless(0.05)
	End If
End Sub

'--------------------------------
Sub ChangePalmOFF
	LFLogo.visible=1:RFLogo.visible=1
	FlipperLSh.visible = 1 : FlipperRSh.visible = 1
	glowbatleft.visible = 0 : glowbatright.visible = 0 : GlowBatLightLeft.state = 0 : GlowBatLightRight.state = 0
End Sub
Sub ChangePalmGreen
	If ModeFlipGlowActive = 0 Then
	LFLogo.visible=0:RFLogo.visible=0
	FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.state = 1 : GlowBatLightRight.state = 1 
	glowbatleft.image = "glowbat green" : glowbatright.image = "glowbat green" 
	GlowBatLightLeft.color = RGB(0,255,0) : GlowBatLightLeft.colorfull = RGB(0,255,0)
	GlowBatLightRight.color = RGB(0,255,0): GlowBatLightRight.colorfull = RGB(0,255,0)
	End If
End Sub
Sub ChangePalmRed
	If ModeFlipGlowActive = 0 Then
	LFLogo.visible=0:RFLogo.visible=0
	FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.state = 1 : GlowBatLightRight.state = 1 
	glowbatleft.image = "glowbat Red" : glowbatright.image = "glowbat Red" 
	GlowBatLightLeft.color = RGB(255,0,0) : GlowBatLightLeft.colorfull = RGB(255,0,0)
	GlowBatLightRight.color = RGB(255,0,0): GlowBatLightRight.colorfull = RGB(255,0,0)
	End If
End Sub
Sub ChangePalmBlue
	If ModeFlipGlowActive = 0 Then
	LFLogo.visible=0:RFLogo.visible=0
	FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.state = 1 : GlowBatLightRight.state = 1 
	glowbatleft.image = "glowbat Blue" : glowbatright.image = "glowbat Blue" 
	GlowBatLightLeft.color = RGB(0,128,255) : GlowBatLightLeft.colorfull = RGB(0,128,255)
	GlowBatLightRight.color = RGB(0,128,255): GlowBatLightRight.colorfull = RGB(0,128,255)
	End If
End Sub
Sub ChangePalmPink
	If ModeFlipGlowActive = 0 Then
	LFLogo.visible=0:RFLogo.visible=0
	FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.state = 1 : GlowBatLightRight.state = 1 
	glowbatleft.image = "glowbat Pink" : glowbatright.image = "glowbat Pink" 
	GlowBatLightLeft.color = RGB(255,0,128) : GlowBatLightLeft.colorfull = RGB(255,0,128)
	GlowBatLightRight.color = RGB(255,0,128): GlowBatLightRight.colorfull = RGB(255,0,128)
	End If
End Sub
Sub ChangePalmYel
	If ModeFlipGlowActive = 0 Then
	LFLogo.visible=0:RFLogo.visible=0
	FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.state = 1 : GlowBatLightRight.state = 1 
	glowbatleft.image = "glowbat Yel" : glowbatright.image = "glowbat Yel" 
	GlowBatLightLeft.color = RGB(255,255,0) : GlowBatLightLeft.colorfull = RGB(255,255,0)
	GlowBatLightRight.color = RGB(255,255,0): GlowBatLightRight.colorfull = RGB(255,255,0)
	End If
End Sub
Sub ChangePalmBICOL
	If ModeFlipGlowActive = 0 Then
	LFLogo.visible=0:RFLogo.visible=0
	FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.state = 1 : GlowBatLightRight.state = 1 
	glowbatleft.image = "glowbat Blue" : glowbatright.image = "glowbat Pink" 
	GlowBatLightLeft.color = RGB(0,128,255) : GlowBatLightLeft.colorfull = RGB(0,128,255)
	GlowBatLightRight.color = RGB(255,0,128): GlowBatLightRight.colorfull = RGB(255,0,128)
	End If
End Sub
'*****begin HAL playing against himself************

sub trigger018_hit	
If Activeball.VelY < -5 Then Exit Sub
        Trigger020.Enabled = True
        Trigger023.Enabled = True
LeftFlipper.RotateToEnd
' Flipper Sound Trigger 1
Dim flipSounds1, flipPick1
flipSounds1 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick1 = Int(Rnd * (UBound(flipSounds1) + 1))
PlaySound flipSounds1(flipPick1)
timer065.enabled=1
end Sub

sub timer065_timer
LeftFlipper.RotateToStart
if ballsonplayfield > 1 Then
Timer065.interval=35
end if
if ballsonplayfield = 1 Then
Timer065.interval=100
end if
'PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
timer065.enabled=0
end Sub

sub trigger023_hit	
trigger018.enabled=1
Trigger019.enabled=1
Trigger023.enabled=0	
LeftFlipper.RotateToEnd
' Flipper Sound Trigger 2
Dim flipSounds2, flipPick2
flipSounds2 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick2 = Int(Rnd * (UBound(flipSounds2) + 1))
PlaySound flipSounds2(flipPick2)

timer065.enabled=1
end Sub

sub trigger020_hit		
trigger018.enabled=1
Trigger019.enabled=1
trigger020.enabled=0
LeftFlipper.RotateToEnd
' Flipper Sound Trigger 3
Dim flipSounds3, flipPick3
flipSounds3 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick3 = Int(Rnd * (UBound(flipSounds3) + 1))
PlaySound flipSounds3(flipPick3)

timer065.enabled=1
end Sub

sub trigger024_hit		
LeftFlipper.RotateToEnd
' Flipper Sound Trigger 4
Dim flipSounds4, flipPick4
flipSounds4 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick4 = Int(Rnd * (UBound(flipSounds4) + 1))
PlaySound flipSounds4(flipPick4)

timer065.enabled=1
end Sub

sub trigger019_hit	
If Activeball.VelY < -5 Then Exit Sub
        Trigger022.Enabled = True
        Trigger021.Enabled = True
       RightFlipper.RotateToEnd
RightFlipper001.RotateToEnd
' Flipper Sound Trigger 5
Dim flipSounds5, flipPick5
flipSounds5 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick5 = Int(Rnd * (UBound(flipSounds5) + 1))
PlaySound flipSounds5(flipPick5)

Timer066.enabled=1
end Sub

sub trigger022_hit	
trigger018.Enabled=1
Trigger019.Enabled=1	
trigger022.enabled=0
RightFlipper.RotateToEnd
RightFlipper001.RotateToEnd
' Flipper Sound Trigger 6
Dim flipSounds6, flipPick6
flipSounds6 = Array("flipper_r01", "flipper_r02", "flipper_r03", "flipper_r04", "flipper_r05", "flipper_r06", "flipper_r07", "flipper_r08", "flipper_r09", "flipper_r10", "flipper_r11")
flipPick6 = Int(Rnd * (UBound(flipSounds6) + 1))
PlaySound flipSounds6(flipPick6)

Timer066.enabled=1
end Sub

sub trigger021_hit	
trigger018.enabled=1
Trigger019.enabled=1	
trigger021.enabled=0
RightFlipper.RotateToEnd
RightFlipper001.RotateToEnd
' Flipper Sound Trigger 7
Dim flipSounds7, flipPick7
flipSounds7 = Array("flipper_r01", "flipper_r02", "flipper_r03", "flipper_r04", "flipper_r05", "flipper_r06", "flipper_r07", "flipper_r08", "flipper_r09", "flipper_r10", "flipper_r11")
flipPick7 = Int(Rnd * (UBound(flipSounds7) + 1))
PlaySound flipSounds7(flipPick7)

Timer066.enabled=1
end Sub

sub trigger025_hit		
RightFlipper.RotateToEnd
RightFlipper001.RotateToEnd
Dim flipSounds8, flipPick8
flipSounds8 = Array("flipper_r01", "flipper_r02", "flipper_r03", "flipper_r04", "flipper_r05", "flipper_r06", "flipper_r07", "flipper_r08", "flipper_r09", "flipper_r10", "flipper_r11")
flipPick8 = Int(Rnd * (UBound(flipSounds8) + 1))
PlaySound flipSounds8(flipPick8)
Timer066.enabled=1
end Sub

sub Timer066_timer
RightFlipper.RotateToStart
RightFlipper001.RotateToStart
if ballsonplayfield > 1 Then
timer066.interval=35
end if
if ballsonplayfield = 1 Then
timer066.interval=100
end if
'PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
Timer066.enabled=0
end Sub

sub Kicker025_Hit 'hal kicker for autoplay
Plunger.AutoPlunger = True
kicker025.timerenabled=1
end sub

sub kicker025_timer 'hal kicker for autoplay
kicker025.kick 0, 40
'playsound "fx_kicker"
playsound "Plunger"
plunger.fire
Plunger.AutoPlunger = False
kicker025.enabled=0
kicker025.timerenabled=0
end Sub

Sub Trigger029_Hit()
BumperLactive
End Sub
Sub Trigger030_Hit()
BumperRactive
End Sub

Sub halgame
	If bGameInPLay = False Then
		trigger018.enabled=1
		Trigger019.enabled=1
		Trigger020.enabled=1
		Trigger021.enabled=1
		Trigger022.enabled=1
		Trigger023.enabled=1
		Trigger024.enabled=1
		Trigger025.enabled=1
		Trigger029.enabled=1
		Trigger030.enabled=1
		Trigger026.enabled=1
		Kicker025.enabled=1
		Wall053.collidable = 1
		halplay=1
		ResetForNewGame()	
	End If
End sub

Sub HalgameOFF
	Trigger018.enabled=0
	Trigger019.enabled=0
	Trigger020.enabled=0
	Trigger021.enabled=0
	Trigger022.enabled=0
	Trigger023.enabled=0
	Trigger024.enabled=0
	Trigger025.enabled=0
	Trigger029.enabled=0
	Trigger030.enabled=0
	Trigger026.enabled=0
	kicker025.enabled=0
	kicker025.timerenabled=0
	Wall053.collidable = 0
	Plunger.AutoPlunger = False
	halplay=0
	EndOfGame()
	timer067.enabled=0
End Sub

Sub timer067_timer
halgame
End Sub


Sub Trigger026_Hit  'when hal is playing and a ball doesnt clear the launch lane, this will shoot it back out
if halplay=1 then
    activeball.VelY = -60
    PlaySound "fx_kicker"
end if
End Sub

'*****end HAL playing against himself************

dim halplay
halplay = 0

'*****AnimBallScoop
Dim DropStep2, DropTimer2, vanishBall
DropStep2 = 0


Sub DropBallTimer_Timer()
    On Error Resume Next

    If DropStep2 < 2 Then
        If vanishBall Is Nothing Then
            DropBallTimer.Enabled = False
            Exit Sub
        End If

        vanishBall.Z = vanishBall.Z - 20
        DropStep2 = DropStep2 + 1
    Else
        If Not vanishBall Is Nothing Then vanishBall.Visible = False
        DropBallTimer.Enabled = False
        Set vanishBall = Nothing     ' <-- Clear reference here
    End If
End Sub


Sub StartDropAnimation(ball)
    Set vanishBall = ball
    DropStep2 = 0
    DropBallTimer.Enabled = True
End Sub
'-------- MB MissionCount
Sub AddMB1
	LightEffect 4
	StartKickBack
	If PersonnageActive(CurrentPlayer) = 3 Then EnableBallSaver 25 Else EnableBallSaver 15
	KickerAuto.CreateSizedBallWithMass BallSize / 2, BallMass
	KickerAuto.Kick 10, 1
	DOF 132, DOFPulse 
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
	bAutoPlunger = True
End Sub

Sub AddMB2
	LightEffect 4
'	EnableBallSaver 15
	KickerAuto.CreateSizedBallWithMass BallSize / 2, BallMass
	KickerAuto.Kick 10, 1
	DOF 132, DOFPulse
    BallsOnPlayfield = BallsOnPlayfield + 1 
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
	bAutoPlunger = True
End Sub
'************GEM FORGE
Sub GemForgeRotTimer_Timer()
	MineraisHautForge.ObjRotZ = MineraisHautForge.ObjRotZ +4
	discolight.RotZ = discolight.RotZ +4
End Sub

'----PersonnageSwhith
Dim Personnage, Personnagenr
Personnage = ""
Personnagenr = INT(RND * 1)

Sub UpdatePersonnageTMP
	DownMusicTmp
    Select Case Personnagenr
        Case 0
			PersonnageActive(CurrentPlayer) = 0
'			PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin001.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			pDMDLabelShow "SkinSelect1" 
			pDMDLabelHide "SkinSelect2"
			pDMDLabelHide "SkinSelect3"
			pDMDLabelHide "SkinSelect4"
			pDMDLabelHide "SkinSelect5"
			pDMDLabelHide "SkinSelect6"
			pDMDLabelHide "SkinSelect7"
			pDMDLabelHide "SkinSelect8"
			PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayRover.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Set_LiPerso_State 1, "BLINK"
			Set_LiPerso_State 2, "ON"
			Set_LiPerso_State 3, "ON"
			Set_LiPerso_State 4, "ON"
			Set_LiPerso_State 5, "ON"
			Set_LiPerso_State 6, "ON"
			Set_LiPerso_State 7, "ON"
			Set_LiPerso_State 8, "ON"
			if UseUltraDMD = 1 then UDMD "ROVER", "", 3000	
        Case 1
			PersonnageActive(CurrentPlayer) = 1
'			PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin002.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			pDMDLabelHide "SkinSelect1" 
			pDMDLabelShow "SkinSelect2"
			pDMDLabelHide "SkinSelect3"
			pDMDLabelHide "SkinSelect4"
			pDMDLabelHide "SkinSelect5"
			pDMDLabelHide "SkinSelect6"
			pDMDLabelHide "SkinSelect7"
			pDMDLabelHide "SkinSelect8"
			PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayCantarella.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Set_LiPerso_State 1, "ON"
			Set_LiPerso_State 2, "BLINK"
			Set_LiPerso_State 3, "ON"
			Set_LiPerso_State 4, "ON"
			Set_LiPerso_State 5, "ON"
			Set_LiPerso_State 6, "ON"
			Set_LiPerso_State 7, "ON"
			Set_LiPerso_State 8, "ON"
			if UseUltraDMD = 1 then UDMD "CANTARELLA", "", 3000
        Case 2
			PersonnageActive(CurrentPlayer) = 2
'			PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin003.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			pDMDLabelHide "SkinSelect1" 
			pDMDLabelHide "SkinSelect2"
			pDMDLabelShow "SkinSelect3"
			pDMDLabelHide "SkinSelect4"
			pDMDLabelHide "SkinSelect5"
			pDMDLabelHide "SkinSelect6"
			pDMDLabelHide "SkinSelect7"
			pDMDLabelHide "SkinSelect8"
			PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayZani.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Set_LiPerso_State 1, "ON"
			Set_LiPerso_State 2, "ON"
			Set_LiPerso_State 3, "BLINK"
			Set_LiPerso_State 4, "ON"
			Set_LiPerso_State 5, "ON"
			Set_LiPerso_State 6, "ON"
			Set_LiPerso_State 7, "ON"
			Set_LiPerso_State 8, "ON"
			if UseUltraDMD = 1 then UDMD "ZANI", "", 3000
        Case 3
			PersonnageActive(CurrentPlayer) = 3
'			PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin004.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			pDMDLabelHide "SkinSelect1" 
			pDMDLabelHide "SkinSelect2"
			pDMDLabelHide "SkinSelect3"
			pDMDLabelShow "SkinSelect4"
			pDMDLabelHide "SkinSelect5"
			pDMDLabelHide "SkinSelect6"
			pDMDLabelHide "SkinSelect7"
			pDMDLabelHide "SkinSelect8"
			PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayAugusta.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Set_LiPerso_State 1, "ON"
			Set_LiPerso_State 2, "ON"
			Set_LiPerso_State 3, "ON"
			Set_LiPerso_State 4, "BLINK"
			Set_LiPerso_State 5, "ON"
			Set_LiPerso_State 6, "ON"
			Set_LiPerso_State 7, "ON"
			Set_LiPerso_State 8, "ON"
			if UseUltraDMD = 1 then UDMD "AUGUSTA", "", 3000
        Case 4
			PersonnageActive(CurrentPlayer) = 4
'			PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin005.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			pDMDLabelHide "SkinSelect1" 
			pDMDLabelHide "SkinSelect2"
			pDMDLabelHide "SkinSelect3"
			pDMDLabelHide "SkinSelect4"
			pDMDLabelShow "SkinSelect5"
			pDMDLabelHide "SkinSelect6"
			pDMDLabelHide "SkinSelect7"
			pDMDLabelHide "SkinSelect8"
			PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayJiyan.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Set_LiPerso_State 1, "ON"
			Set_LiPerso_State 2, "ON"
			Set_LiPerso_State 3, "ON"
			Set_LiPerso_State 4, "ON"
			Set_LiPerso_State 5, "BLINK"
			Set_LiPerso_State 6, "ON"
			Set_LiPerso_State 7, "ON"
			Set_LiPerso_State 8, "ON"
			if UseUltraDMD = 1 then UDMD "JIYAN", "", 3000
		Case 5
			PersonnageActive(CurrentPlayer) = 5
'			PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin006.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			pDMDLabelHide "SkinSelect1" 
			pDMDLabelHide "SkinSelect2"
			pDMDLabelHide "SkinSelect3"
			pDMDLabelHide "SkinSelect4"
			pDMDLabelHide "SkinSelect5"
			pDMDLabelShow "SkinSelect6"
			pDMDLabelHide "SkinSelect7"
			pDMDLabelHide "SkinSelect8"
			PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayLuno.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Set_LiPerso_State 1, "ON"
			Set_LiPerso_State 2, "ON"
			Set_LiPerso_State 3, "ON"
			Set_LiPerso_State 4, "ON"
			Set_LiPerso_State 5, "ON"
			Set_LiPerso_State 6, "BLINK"
			Set_LiPerso_State 7, "ON"
			Set_LiPerso_State 8, "ON"
			if UseUltraDMD = 1 then UDMD "IONO", "", 3000
		Case 6
			PersonnageActive(CurrentPlayer) = 6
'			PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin007.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			pDMDLabelHide "SkinSelect1" 
			pDMDLabelHide "SkinSelect2"
			pDMDLabelHide "SkinSelect3"
			pDMDLabelHide "SkinSelect4"
			pDMDLabelHide "SkinSelect5"
			pDMDLabelHide "SkinSelect6"
			pDMDLabelShow "SkinSelect7"
			pDMDLabelHide "SkinSelect8"
			PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayCarlotta.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Set_LiPerso_State 1, "ON"
			Set_LiPerso_State 2, "ON"
			Set_LiPerso_State 3, "ON"
			Set_LiPerso_State 4, "ON"
			Set_LiPerso_State 5, "ON"
			Set_LiPerso_State 6, "ON"
			Set_LiPerso_State 7, "BLINK"
			Set_LiPerso_State 8, "ON"
			if UseUltraDMD = 1 then UDMD "CARLOTTA", "", 3000
		Case 7
			PersonnageActive(CurrentPlayer) = 7
'			PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin008.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			pDMDLabelHide "SkinSelect1" 
			pDMDLabelHide "SkinSelect2"
			pDMDLabelHide "SkinSelect3"
			pDMDLabelHide "SkinSelect4"
			pDMDLabelHide "SkinSelect5"
			pDMDLabelHide "SkinSelect6"
			pDMDLabelHide "SkinSelect7"
			pDMDLabelShow "SkinSelect8"
			PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayGalbrena.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Set_LiPerso_State 1, "ON"
			Set_LiPerso_State 2, "ON"
			Set_LiPerso_State 3, "ON"
			Set_LiPerso_State 4, "ON"
			Set_LiPerso_State 5, "ON"
			Set_LiPerso_State 6, "ON"
			Set_LiPerso_State 7, "ON"
			Set_LiPerso_State 8, "BLINK"
			if UseUltraDMD = 1 then UDMD "GALBRENA", "", 3000
    End Select
End Sub

Sub SelectPersonnage(keycode)
'	If (keycode = StartGameKey or keycode = PlungerKey) And bPersonnageSelect = True Then
	If keycode = PlungerKey And bPersonnageSelect = True Then
		FirstShoot = False
		PersoSwitchCount = PersoSwitchCount + 1
		UpdatePersonnage
		ResetSlingCount
		SelectPersoAutoTimer.Enabled = False
		CountTimerValueTimer.Enabled = False
		puPlayer.LabelSet pDMD,"AutoSelecTime","AutoSelect in : " & FormatNumber(CountTimerValue, 0),0,""
	End If
	If keycode = LeftMagnaSave And bPersonnageSelect = True  Then
        Personnagenr = (Personnagenr - 1)
        If Personnagenr <0 Then Personnagenr = 7
'        UpdatePersonnage
		UpdatePersonnageTMP
		Playsound "Fx_Select"
    End If
	If keycode = RightMagnaSave And bPersonnageSelect = True Then
		Personnagenr = (Personnagenr + 1)MOD 8
'		UpdatePersonnage
'		ResetSlingCount
		UpdatePersonnageTMP
		Playsound "Fx_Select"
	End If
End Sub

Dim CountTimerValue
CountTimerValue = 10

Sub CountTimerValueTimer_Timer()
	CountTimerValue = CountTimerValue - 1
	puPlayer.LabelSet pDMD,"AutoSelecTime","AutoSelect in : " & FormatNumber(CountTimerValue, 0),1,""
End Sub 

Sub StartAutoSelectPerso
	CountTimerValue = 10
	SelectPersoAutoTimer.Enabled = True
	CountTimerValueTimer.Enabled = True
End Sub 

Sub SelectPersoAutoTimer_Timer()
	SelectPersoAutoNow
End Sub

Sub SelectPersoAutoNow
	If bPersonnageSelect = True Then
	PersoSwitchCount = PersoSwitchCount + 1
	Personnagenr = Personnagenr + 1
	UpdatePersonnage
	ResetSlingCount
	End If
	SelectPersoAutoTimer.Enabled = False
	CountTimerValueTimer.Enabled = False
	puPlayer.LabelSet pDMD,"AutoSelecTime","AutoSelect in : " & FormatNumber(CountTimerValue, 0),0,""
End Sub

Sub UpdatePersonnageActif()
	If PersonnageActive(CurrentPlayer) = 0 Then ViewRover
	If PersonnageActive(CurrentPlayer) = 1 Then ViewCantarella
	If PersonnageActive(CurrentPlayer) = 2 Then ViewZani
	If PersonnageActive(CurrentPlayer) = 3 Then ViewAugusta
	If PersonnageActive(CurrentPlayer) = 4 Then ViewJiyan
	If PersonnageActive(CurrentPlayer) = 5 Then ViewLuno
	If PersonnageActive(CurrentPlayer) = 6 Then ViewCarlotta
	If PersonnageActive(CurrentPlayer) = 7 Then ViewGalbrena
End Sub

Sub UpdatePersonnage() '
    Select Case Personnagenr
        Case 0
			PersoMooveTimer.enabled=True
			vpmTimer.AddTimer 180, "ViewRover '"
			PuPEvent 851
        Case 1
			PersoMooveTimer.enabled=True
			vpmTimer.AddTimer 180, "ViewCantarella '"
			PuPEvent 852
        Case 2
			PersoMooveTimer.enabled=True
			vpmTimer.AddTimer 180, "ViewZani '"
			PuPEvent 853
        Case 3
			PersoMooveTimer.enabled=True
			vpmTimer.AddTimer 180, "ViewAugusta '"
			PuPEvent 854
        Case 4
			PersoMooveTimer.enabled=True
			vpmTimer.AddTimer 180, "ViewJiyan '"
			PuPEvent 855
		Case 5
			PersoMooveTimer.enabled=True
			vpmTimer.AddTimer 180, "ViewLuno '"
			PuPEvent 856
        Case 6
			PersoMooveTimer.enabled=True
			vpmTimer.AddTimer 180, "ViewCarlotta '"
			PuPEvent 857
        Case 7
			PersoMooveTimer.enabled=True
			vpmTimer.AddTimer 180, "ViewGalbrena '"
			PuPEvent 858
    End Select
	vpmTimer.AddTimer 4000, "StartMusic '"	
	vpmTimer.AddTimer 4000, "UpMusicTmp '"
End Sub
'************ Moove Perso
Dim PersoMooveON
PersoMooveON = False

Sub PersoMooveTimer_Timer()
If not PersoMooveON Then
	PersoRover.ObjRotZ = PersoRover.ObjRotZ +36
	PersoRover.TransY = PersoRover.TransY +6
	PersoCantarella.ObjRotZ = PersoCantarella.ObjRotZ +36
	PersoCantarella.TransY = PersoCantarella.TransY +6
	PersoZani.ObjRotZ = PersoZani.ObjRotZ +36
	PersoZani.TransY = PersoZani.TransY +6
	PersoAugusta.ObjRotZ = PersoAugusta.ObjRotZ +36
	PersoAugusta.TransY = PersoAugusta.TransY +6
	PersoJiyan.ObjRotZ = PersoJiyan.ObjRotZ +36
	PersoJiyan.TransY = PersoJiyan.TransY +6
	PersoLuno.ObjRotZ = PersoLuno.ObjRotZ +36
	PersoLuno.TransY = PersoLuno.TransY +6
	PersoCarlotta.ObjRotZ = PersoCarlotta.ObjRotZ +36
	PersoCarlotta.TransY = PersoCarlotta.TransY +6
	PersoGalbrena.ObjRotZ = PersoGalbrena.ObjRotZ +36
	PersoGalbrena.TransY = PersoGalbrena.TransY +6
	If PersoRover.ObjRotZ = 180 Then
		PersoMooveTimer.enabled=True
		PersoMooveON=True
	End IF
Else	
	If PersoRover.ObjRotZ = 0 Then
		PersoMooveTimer.enabled=False
		PersoMooveON=False
	Else
	PersoRover.ObjRotZ = PersoRover.ObjRotZ -36
	PersoRover.TransY = PersoRover.TransY -6
	PersoCantarella.ObjRotZ = PersoCantarella.ObjRotZ -36
	PersoCantarella.TransY = PersoCantarella.TransY -6
	PersoZani.ObjRotZ = PersoZani.ObjRotZ -36
	PersoZani.TransY = PersoZani.TransY -6
	PersoAugusta.ObjRotZ = PersoAugusta.ObjRotZ -36
	PersoAugusta.TransY = PersoAugusta.TransY -6
	PersoJiyan.ObjRotZ = PersoJiyan.ObjRotZ -36
	PersoJiyan.TransY = PersoJiyan.TransY -6
	PersoLuno.ObjRotZ = PersoLuno.ObjRotZ -36
	PersoLuno.TransY = PersoLuno.TransY -6
	PersoCarlotta.ObjRotZ = PersoCarlotta.ObjRotZ -36
	PersoCarlotta.TransY = PersoCarlotta.TransY -6
	PersoGalbrena.ObjRotZ = PersoGalbrena.ObjRotZ -36
	PersoGalbrena.TransY = PersoGalbrena.TransY -6
	End IF
End If
End Sub

Sub ViewRover() 'Switch easy
	PersonnageActive(CurrentPlayer) = 0
	pDMDLabelHide "SkinSelect1" 
	pDMDLabelHide "SkinSelect2"
	pDMDLabelHide "SkinSelect3"
	pDMDLabelHide "SkinSelect4"
	pDMDLabelHide "SkinSelect5"
	pDMDLabelHide "SkinSelect6"
	pDMDLabelHide "SkinSelect7"
	pDMDLabelHide "SkinSelect8"
'	PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayRover.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Set_LiPerso_State 1, "OFF"
	Set_LiPerso_State 2, "ON"
	Set_LiPerso_State 3, "ON"
	Set_LiPerso_State 4, "ON"
	Set_LiPerso_State 5, "ON"
	Set_LiPerso_State 6, "ON"
	Set_LiPerso_State 7, "ON"
	Set_LiPerso_State 8, "ON"
	PersoRover.Visible = 1
	PersoCantarella.Visible = 0
	PersoZani.Visible = 0
	PersoAugusta.Visible = 0
	PersoJiyan.Visible = 0
	PersoLuno.Visible = 0
	PersoCarlotta.Visible = 0
	PersoGalbrena.Visible = 0
'	ChangePalmOFF
	ChangePalmBICOL
	ChangeGi White
	ChangeGiLampB baseLB
	ChangeGiLampH baseLH
	CastelRgoldMinerais.Color = RGB(2,2,2) 'OFF
	RocheMinerais.Color = RGB(128,0,65) 'OFF
	RocheMineraisHaut001.Color = RGB(128,0,65) 'OFF
	RocheMineraisBas001.Color = RGB(2,2,2) 'OFF
	RocheMineraisHaut002.Color = RGB(128,0,65) 'OFF
	RocheMineraisBas002.Color = RGB(2,2,2) 'OFF
	MineraisHautForge.Color = RGB(128,0,65) 'OFF
	MineraisRampL.Color = RGB(128,0,65) 'OFF
	DeparRampML.Image = "T_DungeonFort_D"
	CastelRgold.Image = "T_DungeonTown_D"
	ForgeTour002.Image = "T_DungeonTown_D"
End Sub
Sub ViewCantarella()
	pDMDLabelHide "SkinSelect1" 
	pDMDLabelHide "SkinSelect2"
	pDMDLabelHide "SkinSelect3"
	pDMDLabelHide "SkinSelect4"
	pDMDLabelHide "SkinSelect5"
	pDMDLabelHide "SkinSelect6"
	pDMDLabelHide "SkinSelect7"
	pDMDLabelHide "SkinSelect8"
'	PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayCantarella.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PersonnageActive(CurrentPlayer) = 1
	Set_LiPerso_State 1, "ON"
	Set_LiPerso_State 2, "OFF"
	Set_LiPerso_State 3, "ON"
	Set_LiPerso_State 4, "ON"
	Set_LiPerso_State 5, "ON"
	Set_LiPerso_State 6, "ON"
	Set_LiPerso_State 7, "ON"
	Set_LiPerso_State 8, "ON"
	PersoRover.Visible = 0
	PersoCantarella.Visible = 1
	PersoZani.Visible = 0
	PersoAugusta.Visible = 0
	PersoJiyan.Visible = 0
	PersoLuno.Visible = 0
	PersoCarlotta.Visible = 0
	PersoGalbrena.Visible = 0
	ChangePalmBlue
	ChangeGi blue
	ChangeGiLampB baseLB
	ChangeGiLampH baseLH
	CastelRgoldMinerais.Color = RGB(0,128,255) 'Bleu Turquoise
	RocheMinerais.Color = RGB(0,128,255) 'Bleu Turquoise
	RocheMineraisHaut001.Color = RGB(0,128,255) 'Bleu Turquoise
	RocheMineraisBas001.Color = RGB(255,0,128) 'ROSE
	RocheMineraisHaut002.Color = RGB(0,128,255) 'Bleu Turquoise
	RocheMineraisBas002.Color = RGB(255,0,128) 'ROSE
	MineraisHautForge.Color = RGB(0,128,255) 'Bleu Turquoise
	MineraisRampL.Color = RGB(0,128,255) 'Bleu Turquoise
	DeparRampML.Image = "T_DungeonFort_D4"
	CastelRgold.Image = "T_DungeonTown_D4"
	ForgeTour002.Image = "T_DungeonTown_D4"
End Sub
Sub ViewZani()
	pDMDLabelHide "SkinSelect1" 
	pDMDLabelHide "SkinSelect2"
	pDMDLabelHide "SkinSelect3"
	pDMDLabelHide "SkinSelect4"
	pDMDLabelHide "SkinSelect5"
	pDMDLabelHide "SkinSelect6"
	pDMDLabelHide "SkinSelect7"
	pDMDLabelHide "SkinSelect8"
'	PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayZani.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PersonnageActive(CurrentPlayer) = 2
	Set_LiPerso_State 1, "ON"
	Set_LiPerso_State 2, "ON"
	Set_LiPerso_State 3, "OFF"
	Set_LiPerso_State 4, "ON"
	Set_LiPerso_State 5, "ON"
	Set_LiPerso_State 6, "ON"
	Set_LiPerso_State 7, "ON"
	Set_LiPerso_State 8, "ON"
	PersoRover.Visible = 0
	PersoCantarella.Visible = 0
	PersoZani.Visible = 1
	PersoAugusta.Visible = 0
	PersoJiyan.Visible = 0
	PersoLuno.Visible = 0
	PersoCarlotta.Visible = 0
	PersoGalbrena.Visible = 0
	ChangePalmYel
	ChangeGi yellow
	ChangeGiLampB orange
	ChangeGiLampH yellow
	CastelRgoldMinerais.Color = RGB(255,255,0) 'JAUNE
	RocheMinerais.Color = RGB(255,255,0) 'JAUNE
	RocheMineraisHaut001.Color = RGB(255,255,0) 'JAUNE
	RocheMineraisBas001.Color = RGB(255,0,0) 'Rouge
	RocheMineraisHaut002.Color = RGB(255,255,0) 'JAUNE
	RocheMineraisBas002.Color = RGB(255,0,0) 'Rouge
	MineraisHautForge.Color = RGB(255,255,0) 'JAUNE
	MineraisRampL.Color = RGB(255,255,0) 'JAUNE
	DeparRampML.Image = "T_DungeonFort_D3"
	CastelRgold.Image = "T_DungeonTown_D3"
	ForgeTour002.Image = "T_DungeonTown_D3"
End Sub
Sub ViewAugusta()
	pDMDLabelHide "SkinSelect1" 
	pDMDLabelHide "SkinSelect2"
	pDMDLabelHide "SkinSelect3"
	pDMDLabelHide "SkinSelect4"
	pDMDLabelHide "SkinSelect5"
	pDMDLabelHide "SkinSelect6"
	pDMDLabelHide "SkinSelect7"
	pDMDLabelHide "SkinSelect8"
'	PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayAugusta.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PersonnageActive(CurrentPlayer) = 3
	Set_LiPerso_State 1, "ON"
	Set_LiPerso_State 2, "ON"
	Set_LiPerso_State 3, "ON"
	Set_LiPerso_State 4, "OFF"
	Set_LiPerso_State 5, "ON"
	Set_LiPerso_State 6, "ON"
	Set_LiPerso_State 7, "ON"
	Set_LiPerso_State 8, "ON"
	PersoRover.Visible = 0
	PersoCantarella.Visible = 0
	PersoZani.Visible = 0
	PersoAugusta.Visible = 1
	PersoJiyan.Visible = 0
	PersoLuno.Visible = 0
	PersoCarlotta.Visible = 0
	PersoGalbrena.Visible = 0
	ChangePalmRed
	ChangeGi orange
	ChangeGiLampB orange
	ChangeGiLampH red
	CastelRgoldMinerais.Color = RGB(255,128,0) 'ORANGE
	RocheMinerais.Color = RGB(255,128,0) 'ORANGE
	RocheMineraisHaut001.Color = RGB(255,128,0) 'ORANGE
	RocheMineraisBas001.Color = RGB(2,2,2) 'OFF
	RocheMineraisHaut002.Color = RGB(255,128,0) 'ORANGE
	RocheMineraisBas002.Color = RGB(2,2,2) 'OFF
	MineraisHautForge.Color = RGB(255,128,0) 'ORANGE
	MineraisRampL.Color = RGB(255,128,0) 'ORANGE
	DeparRampML.Image = "T_DungeonFort_D3"
	CastelRgold.Image = "T_DungeonTown_D3"
	ForgeTour002.Image = "T_DungeonTown_D3"
End Sub
Sub ViewJiyan()
	pDMDLabelHide "SkinSelect1" 
	pDMDLabelHide "SkinSelect2"
	pDMDLabelHide "SkinSelect3"
	pDMDLabelHide "SkinSelect4"
	pDMDLabelHide "SkinSelect5"
	pDMDLabelHide "SkinSelect6"
	pDMDLabelHide "SkinSelect7"
	pDMDLabelHide "SkinSelect8"
'	PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayJiyan.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PersonnageActive(CurrentPlayer) = 4
	Set_LiPerso_State 1, "ON"
	Set_LiPerso_State 2, "ON"
	Set_LiPerso_State 3, "ON"
	Set_LiPerso_State 4, "ON"
	Set_LiPerso_State 5, "OFF"
	Set_LiPerso_State 6, "ON"
	Set_LiPerso_State 7, "ON"
	Set_LiPerso_State 8, "ON"
	PersoRover.Visible = 0
	PersoCantarella.Visible = 0
	PersoZani.Visible = 0
	PersoAugusta.Visible = 0
	PersoJiyan.Visible = 1
	PersoLuno.Visible = 0
	PersoCarlotta.Visible = 0
	PersoGalbrena.Visible = 0
	ChangePalmGreen
	ChangeGi green
	ChangeGiLampB green
	ChangeGiLampH yellow
	CastelRgoldMinerais.Color = RGB(0,255,0) 'VERT
	RocheMinerais.Color = RGB(0,255,0) 'VERT
	RocheMineraisHaut001.Color = RGB(255,255,0) 'JAUNE
	RocheMineraisBas001.Color = RGB(0,255,0) 'VERT
	RocheMineraisHaut002.Color = RGB(255,255,0) 'JAUNE
	RocheMineraisBas002.Color = RGB(0,255,0) 'VERT
	MineraisHautForge.Color = RGB(0,255,0) 'VERT
	MineraisRampL.Color = RGB(0,255,0) 'VERT
	DeparRampML.Image = "T_DungeonFort_D2"
	CastelRgold.Image = "T_DungeonTown_D2"
	ForgeTour002.Image = "T_DungeonTown_D2"
End Sub
Sub ViewLuno()
	pDMDLabelHide "SkinSelect1" 
	pDMDLabelHide "SkinSelect2"
	pDMDLabelHide "SkinSelect3"
	pDMDLabelHide "SkinSelect4"
	pDMDLabelHide "SkinSelect5"
	pDMDLabelHide "SkinSelect6"
	pDMDLabelHide "SkinSelect7"
	pDMDLabelHide "SkinSelect8"
'	PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayLuno.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PersonnageActive(CurrentPlayer) = 5
	Set_LiPerso_State 1, "ON"
	Set_LiPerso_State 2, "ON"
	Set_LiPerso_State 3, "ON"
	Set_LiPerso_State 4, "ON"
	Set_LiPerso_State 5, "ON"
	Set_LiPerso_State 6, "OFF"
	Set_LiPerso_State 7, "ON"
	Set_LiPerso_State 8, "ON"
	PersoRover.Visible = 0
	PersoCantarella.Visible = 0
	PersoZani.Visible = 0
	PersoAugusta.Visible = 0
	PersoJiyan.Visible = 0
	PersoLuno.Visible = 1
	PersoCarlotta.Visible = 0
	PersoGalbrena.Visible = 0
	ChangePalmYel
	ChangeGi blue
	ChangeGiLampB baseLB
	ChangeGiLampH baseLH
	CastelRgoldMinerais.Color = RGB(255,255,0) 'JAUNE
	RocheMinerais.Color = RGB(255,255,0) 'JAUNE
	RocheMineraisHaut001.Color = RGB(255,255,0) 'JAUNE
	RocheMineraisBas001.Color = RGB(0,128,255) 'Bleu Turquoise
	RocheMineraisHaut002.Color = RGB(255,255,0) 'JAUNE
	RocheMineraisBas002.Color = RGB(0,128,255) 'Bleu Turquoise
	MineraisHautForge.Color = RGB(255,255,0) 'JAUNE
	MineraisRampL.Color = RGB(255,255,0) 'JAUNE
	DeparRampML.Image = "T_DungeonFort_D"
	CastelRgold.Image = "T_DungeonTown_D"
	ForgeTour002.Image = "T_DungeonTown_D"
End Sub
Sub ViewCarlotta()
	pDMDLabelHide "SkinSelect1" 
	pDMDLabelHide "SkinSelect2"
	pDMDLabelHide "SkinSelect3"
	pDMDLabelHide "SkinSelect4"
	pDMDLabelHide "SkinSelect5"
	pDMDLabelHide "SkinSelect6"
	pDMDLabelHide "SkinSelect7"
	pDMDLabelHide "SkinSelect8"
'	PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin007.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayCarlotta.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PersonnageActive(CurrentPlayer) = 6
	Set_LiPerso_State 1, "ON"
	Set_LiPerso_State 2, "ON"
	Set_LiPerso_State 3, "ON"
	Set_LiPerso_State 4, "ON"
	Set_LiPerso_State 5, "ON"
	Set_LiPerso_State 6, "ON"
	Set_LiPerso_State 7, "OFF"
	Set_LiPerso_State 8, "ON"
	PersoRover.Visible = 0
	PersoCantarella.Visible = 0
	PersoZani.Visible = 0
	PersoAugusta.Visible = 0
	PersoJiyan.Visible = 0
	PersoLuno.Visible = 0
	PersoCarlotta.Visible = 1
	PersoGalbrena.Visible = 0
	ChangePalmPink
	ChangeGi purple
	ChangeGiLampB blue
	ChangeGiLampH purple
	CastelRgoldMinerais.Color = RGB(255,0,0) 'Rouge
	RocheMinerais.Color = RGB(255,0,0) 'Rouge
	RocheMineraisHaut001.Color = RGB(255,0,0) 'Rouge
	RocheMineraisBas001.Color = RGB(255,0,128) 'ROSE
	RocheMineraisHaut002.Color = RGB(255,0,0) 'Rouge
	RocheMineraisBas002.Color = RGB(255,0,128) 'ROSE
	MineraisHautForge.Color = RGB(255,0,0) 'Rouge
	MineraisRampL.Color = RGB(255,0,0) 'Rouge
	DeparRampML.Image = "T_DungeonFort_D3"
	CastelRgold.Image = "T_DungeonTown_D3"
	ForgeTour002.Image = "T_DungeonTown_D3"
End Sub
Sub ViewGalbrena()
	pDMDLabelHide "SkinSelect1" 
	pDMDLabelHide "SkinSelect2"
	pDMDLabelHide "SkinSelect3"
	pDMDLabelHide "SkinSelect4"
	pDMDLabelHide "SkinSelect5"
	pDMDLabelHide "SkinSelect6"
	pDMDLabelHide "SkinSelect7"
	pDMDLabelHide "SkinSelect8"
'	PuPlayer.LabelSet pDMD,"SkinSelect","PUPAlphas\\Skin008.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayGalbrena.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PersonnageActive(CurrentPlayer) = 7
	Set_LiPerso_State 1, "ON"
	Set_LiPerso_State 2, "ON"
	Set_LiPerso_State 3, "ON"
	Set_LiPerso_State 4, "ON"
	Set_LiPerso_State 5, "ON"
	Set_LiPerso_State 6, "ON"
	Set_LiPerso_State 7, "ON"
	Set_LiPerso_State 8, "OFF"
	PersoRover.Visible = 0
	PersoCantarella.Visible = 0
	PersoZani.Visible = 0
	PersoAugusta.Visible = 0
	PersoJiyan.Visible = 0
	PersoLuno.Visible = 0
	PersoCarlotta.Visible = 0
	PersoGalbrena.Visible = 1
	ChangePalmBICOL
	ChangeGi darkblue
	ChangeGiLampB baseLB
	ChangeGiLampH baseLH
	CastelRgoldMinerais.Color = RGB(255,0,0) 'Rouge
	RocheMinerais.Color = RGB(255,0,0) 'Rouge
	RocheMineraisHaut001.Color = RGB(255,0,0) 'Rouge
	RocheMineraisBas001.Color = RGB(0,255,0) 'VERT
	RocheMineraisHaut002.Color = RGB(255,0,0) 'Rouge
	RocheMineraisBas002.Color = RGB(0,255,0) 'VERT
	MineraisHautForge.Color = RGB(255,0,0) 'Rouge
	MineraisRampL.Color = RGB(255,0,0) 'Rouge
	DeparRampML.Image = "T_DungeonFort_D4"
	CastelRgold.Image = "T_DungeonTown_D4"
	ForgeTour002.Image = "T_DungeonTown_D4"
End Sub

'------ Spinner
Sub spinning1_timer
	Forge1Plateau.roty = Forge1Plateau.roty - 35
End Sub

Sub StartSpinnerMotor	
	AddScore 10000
	spinner1.MotorOn = true
	spinning1.enabled = True
	LightTagetL.State = 1
	StopspinningTimer.Enabled=True
End Sub

Sub StopSpinnerMotor	
	spinner1.MotorOn = false
	spinning1.enabled = false
	LightTagetL.State = 0
	StopspinningTimer.Enabled=False
End Sub

Sub StopspinningTimer_Timer()
	StopSpinnerMotor
	StopspinningTimer.Enabled=False
End Sub

' SHAKE ECHO
'**********************************
Dim ECHOPos

Sub ShakeECHO
    ECHOPos = 8
    ECHOTimer.Enabled = 1
'	FlashForMs Titre1, 1000, 50, 0
End Sub
Sub ECHOTimer_Timer
    PersoCrownLcorp.TransY = ECHOPos
	PersoCrownLAiles.TransY = ECHOPos
	PersoCrownLArmy.TransY = ECHOPos
	PersoCrownLCape.TransY = ECHOPos
	PersoFeilian.TransY = ECHOPos
	PersoJueEcho.TransY = ECHOPos
    If ECHOPos = 0 Then Me.Enabled = 0:Exit Sub
    If ECHOPos < 0 Then
        ECHOPos = ABS(ECHOPos) - 1
    Else
        ECHOPos = - ECHOPos + 1
    End If
End Sub

Dim EchoGoldMooveON
EchoGoldMooveON = False

Sub EchoGoldMooveTimer_Timer()
If not EchoGoldMooveON Then
	PersoCrownLgold.TransY = PersoCrownLgold.TransY +5
	PersoFeiliangold.TransY = PersoFeiliangold.TransY +5
	PersoJuegold.TransY = PersoJuegold.TransY +5
	Forge1PlatUP.TransY = Forge1PlatUP.TransY +5
	If PersoCrownLgold.TransY = 70 Then
		Wall032.collidable = 1
		Kicker004.enabled=True
		ChangeBall(4)
		Playsound "Vo_EchoAppears"
		EchoGoldMooveTimer.enabled=False
		EchoGoldMooveON=True
	End IF
Else	
	If PersoCrownLgold.TransY = 0 Then
		Wall032.collidable = 0
		PersoCrownLgold.visible = 0
		PersoFeiliangold.visible = 0
		PersoJuegold.visible = 0
		EchoGoldMooveTimer.enabled=False
		Kicker004.enabled=False
		EchoGoldMooveON=False
	Else
	PersoCrownLgold.TransY = PersoCrownLgold.TransY -5
	PersoFeiliangold.TransY = PersoFeiliangold.TransY -5
	PersoJuegold.TransY = PersoJuegold.TransY -5
	Forge1PlatUP.TransY = Forge1PlatUP.TransY -5
	End IF
End If
End Sub

'--------------------- FLASHER PERSO OFF
Sub Set_LiPerso_State(LightIndex, NewState)
    
    ' Vérification que l'index est dans la plage valide
    If LightIndex < 1 Or LightIndex > 8 Then Exit Sub 
    
    ' Référence directe au flasher concerné
    Dim F
    Set F = FlasherArray(LightIndex)

    ' Réinitialiser l'état de clignotement pour cette lumière
    IsBlinkingArray(LightIndex) = False
    
    Select Case UCase(NewState)
        
        Case "OFF"
            F.Visible = 0
            
        Case "ON"
            F.Visible = 1
            
        Case "BLINK"
            F.Visible = 1 ' Assure que l'image ON est visible au début
            IsBlinkingArray(LightIndex) = True
            
    End Select
    
    ' Vérifie s'il faut démarrer ou arrêter le timer global
    UpdateGlobalBlinkTimer
End Sub

Sub UpdateGlobalBlinkTimer()
    Dim i, NeedsBlinking
    NeedsBlinking = False
    
    For i = 1 To 8
        If IsBlinkingArray(i) = True Then
            NeedsBlinking = True
            Exit For ' Sort de la boucle dès qu'un flasher clignotant est trouvé
        End If
    Next
    
    GlobalBlinkTimer.Enabled = NeedsBlinking
End Sub

Sub GlobalBlinkTimer_Timer()
    Dim i
    
    For i = 1 To 8
        ' Inverse l'état de visibilité UNIQUEMENT pour les lumières marquées "clignotantes"
        If IsBlinkingArray(i) = True Then
            FlasherArray(i).Visible = Not FlasherArray(i).Visible
        End If
    Next
End Sub
'Sub BlinkTimer001_Timer()
'    ' Inverse l'état visible du flasher.
'    Flasher001.Visible = Not Flasher001.Visible
'End Sub

'**************************
' Mission Arm 1 TO 5
'**************************
'1 Rectifier - Orbite 6 (Timer)
'2 Pistol - 6 Cibles All Light Turn (Timer)
'3 Gauntlet - Bumber Hits 20
'4 Sword - All Ramp x 2
'5 Broadblade - All Light x 2
Dim CountTimerMI1Value
CountTimerMI1Value = 60

Sub CountTimerMI1ValueTimer_Timer()
	CountTimerMI1Value = CountTimerMI1Value - 1
'	puPlayer.LabelSet pDMD,"AutoSelecTime","AutoSelect in : " & FormatNumber(CountTimerValue, 0),1,""
	puPlayer.LabelSet pDMD,"Miss1EndTime","Mission Failed in : " & FormatNumber(CountTimerMI1Value, 0),1,""
End Sub 

Dim CountTimerMI2Value
CountTimerMI2Value = 90

Sub CountTimerMI2ValueTimer_Timer()
	CountTimerMI2Value = CountTimerMI2Value - 1
'	puPlayer.LabelSet pDMD,"AutoSelecTime","AutoSelect in : " & FormatNumber(CountTimerValue, 0),1,""
	puPlayer.LabelSet pDMD,"Miss2EndTime","Mission Failed in : " & FormatNumber(CountTimerMI2Value, 0),1,""
End Sub 

Sub StartMi1
	CountTimerMI1ValueTimer.Enabled = True
	MissionCount = MissionCount + 1
	Mission1_State(CurrentPlayer) = 2
	LiArm001.State = 2
	MissionArm1Timer.Enabled = True
	If PersonnageActive(CurrentPlayer) = 1 Then OrbiteCount = 3 Else OrbiteCount = 6
	LiMi001.State = 2
	LiMi003.State = 2
	LiMi007.State = 2
	pDMDLabelPulseImage "Cirle2",800,0
	pDMDmessage2L "Mission Rectifier","Go Orbite Light",1
'	pDMDLabelFadePulse "Texte2a", 2500, clRed
    pDMDLabelFadePulse "Texte2b", 2500, clRed
	if UseUltraDMD = 1 then UDMD "MISSION RECTIFIER", "", 3000
End Sub
Sub StopMi1
	If Mission1_State(CurrentPlayer) >= 3 Then 
		LiArm001.State = 1 
	Else 
		LiArm001.State = 0
		Mission1_State(CurrentPlayer) = 0
	End If 
	MissionArm1Timer.Enabled = False
	CountTimerMI1ValueTimer.Enabled = False
	puPlayer.LabelSet pDMD,"Miss1EndTime","Mission Failed in : " & FormatNumber(CountTimerMI1Value, 0),0,""
	OrbiteCount = 6
	LiMi001.State = 0
	LiMi003.State = 0
	LiMi007.State = 0
	PuPlayer.LabelSet pDMD,"MiObjective","",0,""
End Sub
Sub AwardMi1
	If OrbiteCount <= 0 Then
		If PersonnageActive(CurrentPlayer) = 1 Then 
			AddScore2 160000 
			pDMDLabelPulseImage "Cirle2",800,0
			pDMDmessage2L "Weapon Add","Rectifier 160K",1
		'	pDMDLabelFadePulse "Texte2a", 2500, clRed
			pDMDLabelFadePulse "Texte2b", 2500, clRed
			if UseUltraDMD = 1 then UDMD "RECTIFIER 160K", "", 3000
			Pupevent 920
		Else 
			AddScore2 80000
			pDMDLabelPulseImage "Cirle2",800,0
			pDMDmessage2L "Weapon Add","Rectifier 80K",1
		'	pDMDLabelFadePulse "Texte2a", 2500, clRed
			pDMDLabelFadePulse "Texte2b", 2500, clRed
			if UseUltraDMD = 1 then UDMD "RECTIFIER 80K", "", 3000
			Pupevent 921
		End If 
		Mission1_State(CurrentPlayer) = 3
		StopMi1
		vpmtimer.addtimer 4000, "AwardALLmission '"
	End If
End Sub
Sub MissionArm1Timer_Timer()
	LoseMi1
End Sub
Sub LoseMi1
	AddScore2 5000
	pDMDmessage2L "Mission Failed","Rectifier Arm",1
'	pDMDLabelFadePulse "Texte2a", 2500, clRed
	pDMDLabelFadePulse "Texte2b", 2500, clRed
	if UseUltraDMD = 1 then UDMD "MISSION FAILED", "", 3000
	'PUPEVENT
	StopMi1
End Sub

Sub StartMi2
	MissionCount = MissionCount + 1
	Mission2_State(CurrentPlayer) = 2
	LightMI2Count = 0
	LiArm002.State = 2
	MissionArm2Timer.Enabled = True
	ChangeLightMi2Timer.Enabled = True
	CountTimerMI2ValueTimer.Enabled = True
	pDMDLabelPulseImage "Cirle2",800,0
	pDMDmessage2L "Mission Pistol","Shoot Active Light",1
'	pDMDLabelFadePulse "Texte2a", 2500, clRed
	pDMDLabelFadePulse "Texte2b", 2500, clRed
	if UseUltraDMD = 1 then UDMD "MISSION PISTOL", "", 3000
End Sub
Sub StopMi2
	If Mission2_State(CurrentPlayer) >= 3 Then 
		LiArm002.State = 1 
	Else 
		LiArm002.State = 0
		Mission2_State(CurrentPlayer) = 0
	End If 
	MissionArm2Timer.Enabled = False
	CountTimerMI2ValueTimer.Enabled = False
	puPlayer.LabelSet pDMD,"Miss2EndTime","Mission Failed in : " & FormatNumber(CountTimerMI2Value, 0),0,""
	ChangeLightMi2Timer.Enabled = False
	LightMI2Count = 0
	LiMi001.State = 0 
	LiMi002.State = 0 
	LiMi003.State = 0
	LiMi004.State = 0 
	LiMi005.State = 0
	LiMi006.State = 0
	LiMi007.State = 0	
	PuPlayer.LabelSet pDMD,"MiObjective","",0,""
End Sub
Sub AwardMi2
	If LightMI2Count >= 3 And PersonnageActive(CurrentPlayer) = 7 Then
		AddScore2 600000
		Mission2_State(CurrentPlayer) = 3
		pDMDLabelPulseImage "Cirle2",800,0
		pDMDmessage2L "Weapon Add","Pistol 600K",1
'		pDMDLabelFadePulse "Texte2a", 2500, clRed
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "PISTOL 600K", "", 3000
		Pupevent 922
		vpmtimer.addtimer 4000, "AwardALLmission '"
		StopMi2
	Elseif LightMI2Count >= 3 And PersonnageActive(CurrentPlayer) = 6 Then
		AddScore2 300000
		Mission2_State(CurrentPlayer) = 3
		pDMDLabelPulseImage "Cirle2",800,0
		pDMDmessage2L "Weapon Add","Pistol 300K",1
	'	pDMDLabelFadePulse "Texte2a", 2500, clRed
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "PISTOL 300K", "", 3000
		Pupevent 923
		vpmtimer.addtimer 4000, "AwardALLmission '"
		StopMi2
	Elseif LightMI2Count >= 6 Then
		AddScore2 300000
		Mission2_State(CurrentPlayer) = 3
		pDMDLabelPulseImage "Cirle2",800,0
		pDMDmessage2L "Weapon Add","Pistol 300K",1
	'	pDMDLabelFadePulse "Texte2a", 2500, clRed
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "PISTOL 300K", "", 3000
		Pupevent 924
		vpmtimer.addtimer 4000, "AwardALLmission '"
		StopMi2
	End If
End Sub

Sub MissionArm2Timer_Timer()
	LoseMi2
End Sub
Sub LoseMi2
	AddScore2 5000
	pDMDmessage2L "Mission Failed","Pistol Arm",1
'	pDMDLabelFadePulse "Texte2a", 2500, clRed
	pDMDLabelFadePulse "Texte2b", 2500, clRed
	if UseUltraDMD = 1 then UDMD "MISSION FAILED", "", 3000
	'PUPEVENT
	StopMi2
End Sub
Sub ChangeLightMi2Timer_Timer()
	Select Case Int(Rnd * 7) + 1
		Case 1
	LiMi001.State = 2 :	LiMi002.State = 0 :	LiMi003.State = 0 :	LiMi004.State = 0 : LiMi005.State = 0 :	LiMi006.State = 0 :	LiMi007.State = 0
		Case 2
	LiMi001.State = 0 :	LiMi002.State = 2 :	LiMi003.State = 0 :	LiMi004.State = 0 : LiMi005.State = 0 :	LiMi006.State = 0 :	LiMi007.State = 0
		Case 3
	LiMi001.State = 0 :	LiMi002.State = 0 :	LiMi003.State = 2 :	LiMi004.State = 0 : LiMi005.State = 0 :	LiMi006.State = 0 :	LiMi007.State = 0
		Case 4
	LiMi001.State = 0 :	LiMi002.State = 0 :	LiMi003.State = 0 :	LiMi004.State = 2 : LiMi005.State = 0 :	LiMi006.State = 0 :	LiMi007.State = 0
		Case 5
	LiMi001.State = 0 :	LiMi002.State = 0 :	LiMi003.State = 0 :	LiMi004.State = 0 : LiMi005.State = 2 :	LiMi006.State = 0 :	LiMi007.State = 0
		Case 6
	LiMi001.State = 0 :	LiMi002.State = 0 :	LiMi003.State = 0 :	LiMi004.State = 0 : LiMi005.State = 0 :	LiMi006.State = 2 :	LiMi007.State = 0
		Case 7
	LiMi001.State = 0 :	LiMi002.State = 0 :	LiMi003.State = 0 :	LiMi004.State = 0 : LiMi005.State = 0 :	LiMi006.State = 0 :	LiMi007.State = 2
	End Select
End Sub

Sub StartMi3
	MissionCount = MissionCount + 1
	Mission3_State(CurrentPlayer) = 2
	LiArm003.State = 2
	BumperCount = 0
	bumpersmalllight1.State = 2
	bumpersmalllight2.State = 2
	pDMDLabelPulseImage "Cirle2",800,0
	pDMDmessage2L "Mission Gauntlet","Shoot Bumper",1
'	pDMDLabelFadePulse "Texte2a", 2500, clRed
	pDMDLabelFadePulse "Texte2b", 2500, clRed
	if UseUltraDMD = 1 then UDMD "MISSION GAUNTLET", "", 3000
End Sub
Sub StopMi3
	If Mission3_State(CurrentPlayer) >= 3 Then 
		LiArm003.State = 1 
	Else 
		LiArm003.State = 0
		Mission3_State(CurrentPlayer) = 0
	End If 
	BumperCount = 0
	bumpersmalllight1.State = 1
	bumpersmalllight2.State = 1
	PuPlayer.LabelSet pDMD,"MiObjective","",0,""
End Sub
Sub AwardMi3
	If BumperCount >= 10 And (PersonnageActive(CurrentPlayer) = 2 or PersonnageActive(CurrentPlayer) = 5) Then
		AddScore2 160000
		Mission3_State(CurrentPlayer) = 3
		LiArm003.State = 1
		pDMDLabelPulseImage "Cirle2",800,0
		pDMDmessage2L "Weapon Add","Gauntlet 160K",1
	'	pDMDLabelFadePulse "Texte2a", 2500, clRed
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "GAUNTLET 160K", "", 3000
		Pupevent 925
		vpmtimer.addtimer 4000, "AwardALLmission '"
		StopMi3
	Elseif BumperCount >= 20 Then
		AddScore2 80000
		Mission3_State(CurrentPlayer) = 3
		LiArm003.State = 1
		pDMDLabelPulseImage "Cirle2",800,0
		pDMDmessage2L "Weapon Add","Gauntlet 80K",1
	'	pDMDLabelFadePulse "Texte2a", 2500, clRed
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "GAUNTLET 80K", "", 3000
		Pupevent 926
		vpmtimer.addtimer 4000, "AwardALLmission '"
		StopMi3
	End If
End Sub
Sub LoseMi3
	AddScore2 5000
	pDMDmessage2L "Mission Failed","Gauntlet Arm",1
'	pDMDLabelFadePulse "Texte2a", 2500, clRed
	pDMDLabelFadePulse "Texte2b", 2500, clRed
	if UseUltraDMD = 1 then UDMD "MISSION FAILED", "", 3000
	StopMi3
End Sub

Sub StartMi4
	MissionCount = MissionCount + 1
	Mission4_State(CurrentPlayer) = 2
	LightMI4Count = 0
	LiArm004.State = 2
	LiMi002.State = 1
	LiMi004.State = 1
	LiMysteryScoop.State = 1
	LiMi006.State = 1
	pDMDLabelPulseImage "Cirle2",800,0
	pDMDmessage2L "Mission Sword","Shoot Ramps",1
'	pDMDLabelFadePulse "Texte2a", 2500, clRed
	pDMDLabelFadePulse "Texte2b", 2500, clRed
	if UseUltraDMD = 1 then UDMD "MISSION SWORD", "", 3000
End Sub
Sub StopMi4
	If Mission4_State(CurrentPlayer) >= 3 Then 
		LiArm004.State = 1 
	Else 
		LiArm004.State = 0
		Mission4_State(CurrentPlayer) = 0
	End If 
	LightMI4Count = 0
	LiMi002.State = 0
	LiMi004.State = 0
	LiMysteryScoop.State = 0
	LiMi006.State = 0
	PuPlayer.LabelSet pDMD,"MiObjective","",0,""
End Sub
Sub AwardMi4
	If LightMI4Count >= 4 Then
	Mission4_State(CurrentPlayer) = 3
	LiArm004.State = 1
		If PersonnageActive(CurrentPlayer) = 0 Then
			AddScore2 200000
			pDMDLabelPulseImage "Cirle2",800,0
			pDMDmessage2L "Weapon Add","Sword 200K",1
		'	pDMDLabelFadePulse "Texte2a", 2500, clRed
			pDMDLabelFadePulse "Texte2b", 2500, clRed
			if UseUltraDMD = 1 then UDMD "SWORD 200K", "", 3000
		    Pupevent 927
			vpmtimer.addtimer 4000, "AwardALLmission '"
		Else 
			AddScore2 100000
			pDMDLabelPulseImage "Cirle2",800,0
			pDMDmessage2L "Weapon Add","Sword 100K",1
		'	pDMDLabelFadePulse "Texte2a", 2500, clRed
			pDMDLabelFadePulse "Texte2b", 2500, clRed
			if UseUltraDMD = 1 then UDMD "SWORD 100K", "", 3000
			Pupevent 928
			vpmtimer.addtimer 4000, "AwardALLmission '"
		End If
	'PUPEVENT
	StopMi4
	End If
End Sub
Sub LoseMi4
	AddScore2 5000
	Mission4_State(CurrentPlayer) = 0
	pDMDmessage2L "Mission Failed","Sword Arm",1
'	pDMDLabelFadePulse "Texte2a", 2500, clRed
		LabelFadePulse "Texte2b", 2500, clRed
	if UseUltraDMD = 1 then UDMD "MISSION FAILED", "", 3000
	StopMi4
End Sub

Sub StartMi5
	MissionCount = MissionCount + 1
	Mission5_State(CurrentPlayer) = 2
	LightMI5Count = 0
	LiArm005.State = 2
	LiMi001.State = 1
	LiMi002.State = 1
	LiMi003.State = 1
	LiMi004.State = 1
	LiMi005.State = 1
	LiMysteryScoop.State = 1
	LiMi006.State = 1
	LiMi007.State = 1
	pDMDLabelPulseImage "Cirle2",800,0
	pDMDmessage2L "Mission Broadblade","Go Light",1
'	pDMDLabelFadePulse "Texte2a", 2500, clRed
	pDMDLabelFadePulse "Texte2b", 2500, clRed
	if UseUltraDMD = 1 then UDMD "MISSION BROADBLADE", "", 3000
End Sub
Sub StopMi5
	If Mission5_State(CurrentPlayer) >= 3 Then 
		LiArm005.State = 1 
	Else 
		LiArm005.State = 0
		Mission5_State(CurrentPlayer) = 0
	End If 
	LightMI5Count = 0
	LiMi001.State = 0
	LiMi002.State = 0
	LiMi003.State = 0
	LiMi004.State = 0
	LiMi005.State = 0
	LiMysteryScoop.State = 0
	LiMi006.State = 0
	LiMi007.State = 0
	PuPlayer.LabelSet pDMD,"MiObjective","",0,""
End Sub
Sub AwardMi5
	If LightMI5Count >= 4 And PersonnageActive(CurrentPlayer) = 3 Then
		AddScore2 200000
		Mission5_State(CurrentPlayer) = 3
		LiArm005.State = 1
		pDMDLabelPulseImage "Cirle2",800,0
		pDMDmessage2L "Weapon Add","Broadblade 200K",1
	'	pDMDLabelFadePulse "Texte2a", 2500, clRed
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "BROADBLADE 200K", "", 3000
		Pupevent 929
		vpmtimer.addtimer 4000, "AwardALLmission '"
		StopMi5
	Elseif LightMI5Count >= 4 And PersonnageActive(CurrentPlayer) = 4 Then
		AddScore2 400000
		Mission5_State(CurrentPlayer) = 3
		LiArm005.State = 1
		pDMDLabelPulseImage "Cirle2",800,0
		pDMDmessage2L "Weapon Add","Broadblade 400K",1
	'	pDMDLabelFadePulse "Texte2a", 2500, clRed
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "BROADBLADE 400K", "", 3000
		Pupevent 930
		vpmtimer.addtimer 4000, "AwardALLmission '"
		StopMi5
	Elseif LightMI5Count >= 8 Then
		AddScore2 200000
		Mission5_State(CurrentPlayer) = 3
		LiArm005.State = 1
		pDMDLabelPulseImage "Cirle2",800,0
		pDMDmessage2L "Weapon Add","Broadblade 200K",1
	'	pDMDLabelFadePulse "Texte2a", 2500, clRed
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "BROADBLADE 200K", "", 3000
		Pupevent 931
		vpmtimer.addtimer 4000, "AwardALLmission '"
		StopMi5
	End If
End Sub
Sub LoseMi5
	AddScore2 5000
	Mission5_State(CurrentPlayer) = 0
	pDMDmessage2L "Mission Failed","Broadblade Arm",1
'	pDMDLabelFadePulse "Texte2a", 2500, clRed
	pDMDLabelFadePulse "Texte2b", 2500, clRed
	if UseUltraDMD = 1 then UDMD "MISSION FAILED", "", 3000
	StopMi5
End Sub

Sub AwardALLmission
	If Mission1_State(CurrentPlayer) = 3 And Mission2_State(CurrentPlayer) = 3 And Mission3_State(CurrentPlayer) = 3 And Mission4_State(CurrentPlayer) = 3 And Mission5_State(CurrentPlayer) = 3 Then
		AddScore2 1000000
		pDMDLabelPulseImage "Cirle2",800,0
		pDMDmessage2L "All weapons","Super Arsenal 1M",1
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "BROADBLADE 200K", "", 3000
		Mission1_State(CurrentPlayer) = 0
		Mission2_State(CurrentPlayer) = 0
		Mission3_State(CurrentPlayer) = 0
		Mission4_State(CurrentPlayer) = 0
		Mission5_State(CurrentPlayer) = 0
		LiArm001.State = 0
		LiArm002.State = 0
		LiArm003.State = 0
		LiArm004.State = 0
		LiArm005.State = 0
	End If 
End Sub 
'****************
'*** SELECT MISSION
'*****************
Dim Missionnr, MissionDispo
Missionnr = INT(RND * 1)
MissionDispo = False

Sub SelectMission(keycode)
'	If (keycode = StartGameKey or keycode = PlungerKey) And bMissionSelect = True Then
	If keycode = PlungerKey And bMissionSelect = True Then
		If MissionDispo = True Then	
		UpdateMission
		Else
		Playsound "fx_sensor"
		End If
	End If
	If keycode = LeftFlipperKey And bMissionSelect = True Then
		Missionnr = (Missionnr - 1)
		If Missionnr <0 Then Missionnr = 5 'FormatNumber(MissionDispo, 0) '5
		UpdateMissionTMP
		Playsound "Fx_Select"
	End If
	If keycode = RightFlipperKey And bMissionSelect = True Then
			Missionnr = (Missionnr + 1)MOD 6 '(FormatNumber(MissionDispo, 0)+1) '6
		UpdateMissionTMP
		Playsound "Fx_Select"
	End If
End Sub

Sub UpdateMissionTMP
'	CheckMissionDispo
    Select Case Missionnr
        Case 0
			pDMDLabelShow "MissSelect1"
			pDMDLabelHide "MissSelect2"
			pDMDLabelHide "MissSelect3"
			pDMDLabelHide "MissSelect4"
			pDMDLabelHide "MissSelect5"
			pDMDLabelHide "MissSelect6"	
			If Mission1_State(CurrentPlayer) = 1 Then
'			PuPlayer.LabelSet pDMD,"MissSelect","PUPAlphas\\Miss001.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			LiArm001.State = 2
			LiArm002.State = 0
			LiArm003.State = 0
			LiArm004.State = 0
			LiArm005.State = 0
			MissionDispo = True	
			if UseUltraDMD = 1 then UDMD "MISSION 1", "BUY", 3000	
			Else 
'			PuPlayer.LabelSet pDMD,"MissSelect","PUPAlphas\\Miss001b.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			If Mission1_State(CurrentPlayer) = 3 Then
				PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Else
				PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			End If
			LiArm001.State = 0
			LiArm002.State = 0
			LiArm003.State = 0
			LiArm004.State = 0
			LiArm005.State = 0
			MissionDispo = False
			if UseUltraDMD = 1 then UDMD "MISSION 1", "UNVAILABLE", 3000
			End If
        Case 1
			pDMDLabelHide "MissSelect1"
			pDMDLabelShow "MissSelect2"
			pDMDLabelHide "MissSelect3"
			pDMDLabelHide "MissSelect4"
			pDMDLabelHide "MissSelect5"
			pDMDLabelHide "MissSelect6"	
			If Mission2_State(CurrentPlayer) = 1 Then
'			PuPlayer.LabelSet pDMD,"MissSelect","PUPAlphas\\Miss002.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			LiArm001.State = 0
			LiArm002.State = 2
			LiArm003.State = 0
			LiArm004.State = 0
			LiArm005.State = 0
			MissionDispo = True
			if UseUltraDMD = 1 then UDMD "MISSION 2", "BUY", 3000	
			Else 
'			PuPlayer.LabelSet pDMD,"MissSelect","PUPAlphas\\Miss002b.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			If Mission2_State(CurrentPlayer) = 3 Then
				PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Else
				PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			End If
			LiArm001.State = 0
			LiArm002.State = 0
			LiArm003.State = 0
			LiArm004.State = 0
			LiArm005.State = 0
			MissionDispo = False
			if UseUltraDMD = 1 then UDMD "MISSION 2", "UNVAILABLE", 3000	
			End If
        Case 2
			pDMDLabelHide "MissSelect1"
			pDMDLabelHide "MissSelect2"
			pDMDLabelShow "MissSelect3"
			pDMDLabelHide "MissSelect4"
			pDMDLabelHide "MissSelect5"
			pDMDLabelHide "MissSelect6"	
			If Mission3_State(CurrentPlayer) = 1 Then
'			PuPlayer.LabelSet pDMD,"MissSelect","PUPAlphas\\Miss003.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			LiArm001.State = 0
			LiArm002.State = 0
			LiArm003.State = 2
			LiArm004.State = 0
			LiArm005.State = 0
			MissionDispo = True
			if UseUltraDMD = 1 then UDMD "MISSION 3", "", 3000
			Else 
'			PuPlayer.LabelSet pDMD,"MissSelect","PUPAlphas\\Miss003b.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			If Mission3_State(CurrentPlayer) = 3 Then
				PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Else
				PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			End If
			LiArm001.State = 0
			LiArm002.State = 0
			LiArm003.State = 0
			LiArm004.State = 0
			LiArm005.State = 0
			MissionDispo = False	
			if UseUltraDMD = 1 then UDMD "MISSION 3", "UNVAILABLE", 3000	
			End If
        Case 3
			pDMDLabelHide "MissSelect1"
			pDMDLabelHide "MissSelect2"
			pDMDLabelHide "MissSelect3"
			pDMDLabelShow "MissSelect4"
			pDMDLabelHide "MissSelect5"
			pDMDLabelHide "MissSelect6"	
			If Mission4_State(CurrentPlayer) = 1 Then
'			PuPlayer.LabelSet pDMD,"MissSelect","PUPAlphas\\Miss004.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			LiArm001.State = 0
			LiArm002.State = 0
			LiArm003.State = 0
			LiArm004.State = 2
			LiArm005.State = 0
			MissionDispo = True
			if UseUltraDMD = 1 then UDMD "MISSION 4", "", 3000
			Else 
'			PuPlayer.LabelSet pDMD,"MissSelect","PUPAlphas\\Miss004b.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			If Mission4_State(CurrentPlayer) = 3 Then
				PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss005.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Else
				PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			End If
			LiArm001.State = 0
			LiArm002.State = 0
			LiArm003.State = 0
			LiArm004.State = 0
			LiArm005.State = 0
			MissionDispo = False	
			if UseUltraDMD = 1 then UDMD "MISSION 4", "UNVAILABLE", 3000	
			End If
        Case 4
			pDMDLabelHide "MissSelect1"
			pDMDLabelHide "MissSelect2"
			pDMDLabelHide "MissSelect3"
			pDMDLabelHide "MissSelect4"
			pDMDLabelShow "MissSelect5"
			pDMDLabelHide "MissSelect6"	
			If Mission5_State(CurrentPlayer) = 1 Then
'			PuPlayer.LabelSet pDMD,"MissSelect","PUPAlphas\\Miss005.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			LiArm001.State = 0
			LiArm002.State = 0
			LiArm003.State = 0
			LiArm004.State = 0
			LiArm005.State = 2
			MissionDispo = True
			if UseUltraDMD = 1 then UDMD "MISSION 5", "", 3000
			Else 
'			PuPlayer.LabelSet pDMD,"MissSelect","PUPAlphas\\Miss005b.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			If Mission5_State(CurrentPlayer) = 3 Then
				PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Else
				PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			End If
			LiArm001.State = 0
			LiArm002.State = 0
			LiArm003.State = 0
			LiArm004.State = 0
			LiArm005.State = 0
			MissionDispo = False	
			if UseUltraDMD = 1 then UDMD "MISSION 5", "UNVAILABLE", 3000	
			End If
		 Case 5
			pDMDLabelHide "MissSelect1"
			pDMDLabelHide "MissSelect2"
			pDMDLabelHide "MissSelect3"
			pDMDLabelHide "MissSelect4"
			pDMDLabelHide "MissSelect5"
			pDMDLabelShow "MissSelect6"	
'			PuPlayer.LabelSet pDMD,"MissSelect","PUPAlphas\\Miss006.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			LiArm001.State = 0
			LiArm002.State = 0
			LiArm003.State = 0
			LiArm004.State = 0
			LiArm005.State = 0
			MissionDispo = True
			if UseUltraDMD = 1 then UDMD "MISSION LATER", "", 3000
    End Select
End Sub

Sub UpdateMission() '
	bMissionSelect = False
	CheckLiArms
'	PuPlayer.LabelSet pDMD,"MissSelect","PUPAlphas\\Miss005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	pDMDLabelHide "MissSelect1"
	pDMDLabelHide "MissSelect2"
	pDMDLabelHide "MissSelect3"
	pDMDLabelHide "MissSelect4"
	pDMDLabelHide "MissSelect5"
	pDMDLabelHide "MissSelect6"	
	PuPlayer.LabelSet pDMD,"MissSelectNO","PUPAlphas\\Miss_NO.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"MissSelectOK","PUPAlphas\\Miss_OK.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
    Select Case Missionnr
        Case 0
			StopMi2
			StopMi3
			StopMi4
			StopMi5
			StartMi1 'Orbite DIFF 1 80K
			LustrousTibeCount(CurrentPlayer) = LustrousTibeCount(CurrentPlayer) - 8
			ForgingTibeCount(CurrentPlayer) = ForgingTibeCount(CurrentPlayer) - 2
			AstriteCount(CurrentPlayer) = AstriteCount(CurrentPlayer) - 1
			CoralsCount(CurrentPlayer) = CoralsCount(CurrentPlayer) - 2
        Case 1
			StopMi1
			StopMi3
			StopMi4
			StopMi5
			StartMi2 'Cible ALT DIFF 4 300K
			LustrousTibeCount(CurrentPlayer) = LustrousTibeCount(CurrentPlayer) - 4
			ForgingTibeCount(CurrentPlayer) = ForgingTibeCount(CurrentPlayer) - 5
			AstriteCount(CurrentPlayer) = AstriteCount(CurrentPlayer) - 4
			CoralsCount(CurrentPlayer) = CoralsCount(CurrentPlayer) - 4
        Case 2
			StopMi1
			StopMi2
			StopMi4
			StopMi5
			StartMi3 'Bumber Hits DIFF 1 80 K
			LustrousTibeCount(CurrentPlayer) = LustrousTibeCount(CurrentPlayer) - 3
			ForgingTibeCount(CurrentPlayer) = ForgingTibeCount(CurrentPlayer) - 6
			AstriteCount(CurrentPlayer) = AstriteCount(CurrentPlayer) - 5
			CoralsCount(CurrentPlayer) = CoralsCount(CurrentPlayer) - 4
        Case 3
			StopMi1
			StopMi2
			StopMi3
			StopMi5
			StartMi4 'All Ramp x 2 DIFF 2 100K
			LustrousTibeCount(CurrentPlayer) = LustrousTibeCount(CurrentPlayer) - 2
			ForgingTibeCount(CurrentPlayer) = ForgingTibeCount(CurrentPlayer) - 5
			AstriteCount(CurrentPlayer) = AstriteCount(CurrentPlayer) - 3
			CoralsCount(CurrentPlayer) = CoralsCount(CurrentPlayer) - 2
        Case 4
			StopMi1
			StopMi2
			StopMi3
			StopMi4
			StartMi5 'All Light x 2 DIFF 3 200K
			LustrousTibeCount(CurrentPlayer) = LustrousTibeCount(CurrentPlayer) - 5
			ForgingTibeCount(CurrentPlayer) = ForgingTibeCount(CurrentPlayer) - 5
			AstriteCount(CurrentPlayer) = AstriteCount(CurrentPlayer) - 5
			CoralsCount(CurrentPlayer) = CoralsCount(CurrentPlayer) - 5
		Case 5
			AddScore 500
    End Select
	Kicker002.timerinterval = 1000
	kicker002.timerenabled = True
	LiForge001.State = 0
	LiForge002.State = 0
End Sub

Sub CheckLiArms
	If Mission1_State(CurrentPlayer) >= 3 Then LiArm001.State = 1
	If Mission2_State(CurrentPlayer) >= 3 Then LiArm002.State = 1
	If Mission3_State(CurrentPlayer) >= 3 Then LiArm003.State = 1
	If Mission4_State(CurrentPlayer) >= 3 Then LiArm004.State = 1
	If Mission5_State(CurrentPlayer) >= 3 Then LiArm005.State = 1
End Sub

Sub CheckMissionDispo 'Active Forges
	If Mission1_State(CurrentPlayer) = 2 or Mission2_State(CurrentPlayer) = 2 or Mission3_State(CurrentPlayer) = 2 or Mission4_State(CurrentPlayer) = 2 or Mission5_State(CurrentPlayer) = 2 or BallsOnPlayfield >= 2 Then
		LiForge001.State = 0
		LiForge002.State = 0
	Else
		If Mission1_State(CurrentPlayer) = 0 And LustrousTibeCount(CurrentPlayer) >= 8 And ForgingTibeCount(CurrentPlayer) >= 2 And AstriteCount(CurrentPlayer) >= 1 And CoralsCount(CurrentPlayer) >= 2 Then
			Mission1_State(CurrentPlayer) = 1
			LiForge001.State = 2
			LiForge002.State = 2
		End If 
		If Mission2_State(CurrentPlayer) = 0 And LustrousTibeCount(CurrentPlayer) >= 4 And ForgingTibeCount(CurrentPlayer) >= 5 And AstriteCount(CurrentPlayer) >= 4 And CoralsCount(CurrentPlayer) >= 4 Then
			Mission2_State(CurrentPlayer) = 1
			LiForge001.State = 2
			LiForge002.State = 2
		End If
		If Mission3_State(CurrentPlayer) = 0 And LustrousTibeCount(CurrentPlayer) >= 3 And ForgingTibeCount(CurrentPlayer) >= 6 And AstriteCount(CurrentPlayer) >= 5 And CoralsCount(CurrentPlayer) >= 4 Then
			Mission3_State(CurrentPlayer) = 1
			LiForge001.State = 2
			LiForge002.State = 2
		End If
		If Mission4_State(CurrentPlayer) = 0 And LustrousTibeCount(CurrentPlayer) >= 2 And ForgingTibeCount(CurrentPlayer) >= 5 And AstriteCount(CurrentPlayer) >= 3 And CoralsCount(CurrentPlayer) >= 2 Then
			Mission4_State(CurrentPlayer) = 1
			LiForge001.State = 2
			LiForge002.State = 2
		End If
		If Mission5_State(CurrentPlayer) = 0 And LustrousTibeCount(CurrentPlayer) >= 5 And ForgingTibeCount(CurrentPlayer) >= 5 And AstriteCount(CurrentPlayer) >= 5 And CoralsCount(CurrentPlayer) >= 5 Then
			Mission5_State(CurrentPlayer) = 1
			LiForge001.State = 2
			LiForge002.State = 2
		End If
	End If
End Sub

'---------- Combos
Sub ComboMTimer_Timer()
    ResetcomboM
End Sub

Sub ResetcomboM
    ComboMTimer.Enabled = False
    ComboMCount = 0
    LiJCombo002.State = 0
End Sub 

Sub CheckComboM 'Trigger008
    ComboMTimer.Enabled = False 
    ComboMTimer.Interval = 4000 
    ComboMCount = ComboMCount + 1
    
    If ComboMCount = 2 Then 
        LiJCombo002.State = 2 
        ComboMTimer.Enabled = True 
    End If

    If ComboMCount = 3 Then 
        Addscore2 10000 
		pDMDmessage2L "COMBO","10K ",1
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "COMBO", " 10K", 3000
        PupComboPerso
        ResetcomboM
    End If
End Sub

Sub ComboLTimer_Timer()
    ResetcomboL
End Sub

Sub ResetcomboL
    ComboLTimer.Enabled = False
    ComboLCount = 0
    LiJCombo001.State = 0
End Sub 

Sub CheckComboL 'Trigger003
    ComboLTimer.Enabled = False 
    ComboLTimer.Interval = 3000 
    ComboLCount = ComboLCount + 1
    
    If ComboLCount = 2 Then 
        LiJCombo001.State = 2 
        ComboLTimer.Enabled = True 
    End If

    If ComboLCount = 3 Then 
        Addscore2 10000 : PlaySound "combo" 
		pDMDmessage2L "COMBO","10K ",1
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "COMBO", " 10K", 3000
        PupComboPerso
        ResetcomboL
    End If
End Sub

Sub PupComboPerso
	If PersonnageActive(CurrentPlayer) = 0 Then PuPEvent 859
	If PersonnageActive(CurrentPlayer) = 1 Then PuPEvent 860
	If PersonnageActive(CurrentPlayer) = 2 Then PuPEvent 861
	If PersonnageActive(CurrentPlayer) = 3 Then PuPEvent 862
	If PersonnageActive(CurrentPlayer) = 4 Then PuPEvent 863
	If PersonnageActive(CurrentPlayer) = 5 Then PuPEvent 864
	If PersonnageActive(CurrentPlayer) = 6 Then PuPEvent 865
	If PersonnageActive(CurrentPlayer) = 7 Then PuPEvent 866
End Sub

'----------Jump Jackpot and Jackpot
'JumpCount = 0
'JackpotValue(CurrentPlayer) = 0
Sub JumpTimer_Timer()
    ResetJump
End Sub

Sub ResetJump
    JumpTimer.Enabled = False
    JumpCount = 0
	LiUPJ001.State = 0 
    LiUPJ002.State = 0
End Sub 

Sub CheckJump 'Trigger002
	DOF 480, DOFPulse
    JumpTimer.Enabled = False 
    JumpTimer.Interval = 8000 
    JumpCount = JumpCount + 1

    If JumpCount = 2 Then 
        LiUPJ001.State = 2 
        LiUPJ002.State = 2
        JumpTimer.Enabled = True 
    End If

    If JumpCount = 3 Then 
        JackpotValue(CurrentPlayer) = JackpotValue(CurrentPlayer) + 55000
		pDMDLabelPulseText "JackpotValue2","" & FormatNumber(JackpotValue(CurrentPlayer), 0),2000,clBlue
		pDMDmessage2L "JUMP JACKPOT","Value +55K ",1
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "JUMP JACKPOT", " VALUE +55K", 3000
        CheckJackpotState 
        ResetJump
        JumpTimer.Enabled = False
    End If
End Sub

Sub CheckJackpotState
    If JackpotValue(CurrentPlayer) >= 275000 Then 
        LiJackpot001.State = 2
        LiJackpot002.State = 2
    Else
        LiJackpot001.State = 0
        LiJackpot002.State = 0
    End If
End Sub

Sub AwardJackpot
	If JackpotValue(CurrentPlayer) >= 275000 And LiJackpot001.State = 2 And LiJackpot002.State = 2 Then
	AddScore2 JackpotValue(CurrentPlayer)
	pDMDLabelPulseImage "Cirle1",800,0
	pDMDmessage2L "JACKPOT","" & FormatNumber(JackpotValue(CurrentPlayer), 0),1
	pDMDLabelFadePulse "Texte2a", 2500, clRed
	if UseUltraDMD = 1 then UDMD "JACKPOT", "", 3000
	DOF 450, DOFPulse
	JackpotValue(CurrentPlayer) = 0
	ResetJump
	PupJackpotPerso
	LiJackpot001.State = 0 
	LiJackpot002.State = 0
	End If 
End Sub

Sub PupJackpotPerso
	If PersonnageActive(CurrentPlayer) = 0 Then PuPEvent 832
	If PersonnageActive(CurrentPlayer) = 1 Then PuPEvent 833
	If PersonnageActive(CurrentPlayer) = 2 Then PuPEvent 834
	If PersonnageActive(CurrentPlayer) = 3 Then PuPEvent 835
	If PersonnageActive(CurrentPlayer) = 4 Then PuPEvent 836
	If PersonnageActive(CurrentPlayer) = 5 Then PuPEvent 837
	If PersonnageActive(CurrentPlayer) = 6 Then PuPEvent 838
	If PersonnageActive(CurrentPlayer) = 7 Then PuPEvent 839
End Sub

'------------------------------

'********************* START OF PUPDMD FRAMEWORK v3.0 BETA *************************
'******************************************************************************
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********
'******************************************************************************
'
'  Quick Steps:
'      1>  create a folder in PUPVideos with Starter_PuPPack.zip and call the folder "yourgame"
'      2>  above set global variable pGameName="yourgame"
'      3>  copy paste the settings section above to top of table script for user changes.
'      4>  on Table you need to create ONE timer only called pupDMDUpdate and set it to 250 ms enabled on startup.
'      5>  go to your table1_init or table first startup function and call PUPINIT function
'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
'      7>  attractmodenext at bottom is setup for you already,  just go to each case and add/remove as many as you want and setup the messages to show.  
'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
'
'
'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
'       and for future pinball you need to add the follow lines near top
'Need to use BAM and have com idll enabled.
'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
'				if icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
'				Function CreateObject(className)       
'   					Set CreateObject = icom.CreateObject(className)   
'				End Function


'**************************
'   PinUp Player USER Config
'**************************

Dim pGameName       : pGameName="WuWa"  'pupvideos foldername, probably set to cGameName in realworld


Const HasPuP = True   'dont set to false as it will break pup

Const pTopper=0
Const pBackglass=2
Const pDMD=5
Const pMusic=4
Const pCallouts=6
Const pMusic2=7
Const pMusic3=8
Const pPopUP=11
Const pPopUP2=12


'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pTargerLetters=5

dim clRed:   clRed = rgb(249,21,48)
dim clGreen: clGreen = rgb(83,189,36)
dim clBlue:  clBlue = rgb(1,196,252)
dim clWhite: clWhite = rgb(255,255,255)
dim clNavy:  clNavy = rgb(34,32,72)
dim clBlack:  clBlack = rgb(0,0,0)
dim clGold:  clGold = rgb(168,168,0)
dim clPinkDark:  clPinkDark = rgb(128,0,255)
dim clViolet:  clViolet = rgb(128,0,128)
dim clVioletBlue:  clVioletBlue = rgb(128,128,255)
dim clPink:  clPink = rgb(255,128,192)


Dim PuPlayer
dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pInAttract : pInAttract=false   'pAttract mode
Dim pFrameSizeX: pFrameSizeX=1920     'DO NOT CHANGE, this is pupdmd author framesize
Dim pFrameSizeY: pFrameSizeY=1080     'DO NOT CHANGE, this is pupdmd author framesize
Dim pUseFramePos : pUseFramePos=1     'DO NOT CHANGE, this is pupdmd author setting


'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit

Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
PuPlayer.B2SInit "", pGameName
PuPlayer.LabelInit pDMD
pSetPageLayouts
pDMDSetPage(pDMDBlank)   'set blank text overlay page.
pDMDStartUP				 ' firsttime running for like an startup video..
End Sub 'end PUPINIT

Sub pDMDStartUP
'do stuff fancy pants on first run
'pInAttract = True
'pDMDSetPage(pScores)
	pAttractStart
end Sub 'end DMDStartup

Sub pTranslatePos(Byref xpos, byref ypos)  'if using uUseFramePos then all coordinates are based on framesize
   xpos=int(xpos/pFrameSizeX*10000) / 100
   ypos=int(ypos/pFrameSizeY*10000) / 100
end Sub

Sub pTranslateY(Byref ypos)           'if using uUseFramePos then all heights are based on framesize
   ypos=int(ypos/pFrameSizeY*10000) / 100
end Sub

Sub pTranslateX(Byref xpos)           'if using uUseFramePos then all heights are based on framesize
   xpos=int(xpos/pFrameSizeX*10000) / 100
end Sub



'***********************************************************PinUP Player DMD Helper Functions

Sub pDMDLabelSet(labName,LabText)
PuPlayer.LabelSet pDMD,labName,LabText,1,""   
end sub


Sub pDMDLabelHide(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",0,""   
end sub

Sub pDMDLabelShow(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",1,""   
end sub

Sub pDMDLabelVisible(labName, isVis)
PuPlayer.LabelSet pDMD,labName,"`u`",isVis,""   
end sub

Sub pDMDLabelSendToBack(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'zback': 1 }"   
end sub

Sub pDMDLabelSendToFront(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'ztop': 1 }"   
end sub

sub pDMDLabelSetPos(labName, byVal xpos, byVal ypos)
   if pUseFramePos=1 Then pTranslatePos xpos,ypos
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'xpos':"&xpos& ",'ypos':"&ypos&"}"    
end sub

sub pDMDLabelSetSizeImage(labName, byVal lWidth, byVal lHeight)
   if pUseFramePos=1 Then pTranslatePos lWidth,lHeight
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'width':"& lWidth & ",'height':"&lHeight&"}" 
end sub

sub pDMDLabelSetSizeText(labName, byVal fHeight)
   if pUseFramePos=1 Then pTranslateY fHeight
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'size':"&fHeight&"}" 
end sub

sub pDMDLabelSetAutoSize(labName, byVal lWidth, byVal lHeight)
   if pUseFramePos=1 Then pTranslatePos lWidth,lHeight
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'autow':"& lWidth & ",'autoh':"&lHeight&"}" 
end sub

sub PDMDLabelSetAlign(labName,xAlign, YAlign)  '0=left 1=center 2=right,  note you should use center as much as possible because some things like rotate/zoom/etc only look correct with center align!
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'xalign':"& xAlign & ",'yalign':"&yAlign&"}"     
end sub

sub pDMDLabelStopAnis(labName)    'stop any pup animations on label/image (zoom/flash/pulse).  this is not about animated gifs
     PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'stopani':1 }" 
end sub

sub pDMDLabelSetRotateText(labName, fAngle)  ' in tenths.  so 900 is 90 degrees.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'rotate':"&fAngle&"}" 
end sub

sub pDMDLabelSetRotate(labName, fAngle)  ' in tenths.  so 900 is 90 degrees. rotate support for images too.  note images must be aligned center to rotate properly(default)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'rotate':"&fAngle&"}" 
end sub

sub pDMDLabelSetZoom(labName, fFactor)  ' fFactor is 120 for 120% of current height, 80% etc...
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'zoom':"&fFactor&"}" 
end sub

sub pDMDLabelSetColor(labName, lCol)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'color':"&lCol&"}" 
end sub

sub pDMDLabelSetAlpha(labName, lAlpha)  '0-255  255=full, 0=blank
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'alpha':"&lAlpha&"}" 
end sub

sub pDMDLabelSetColorGradient(labName, byVal startCol, byVal EndCol)
dim GS: GS=1
if startCol=EndCol Then GS=0  'turn grad off is same colors.
PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'color':"&startCol&" ,'gradstate':"&GS&" , 'gradcolor':"&endCol&"}" 
end sub

sub pDMDLabelSetColorGradientPercent(labName, byVal startCol, byVal EndCol, byVal StartPercent)
if startCol=EndCol Then StartPercent=0  'turn grad off is same colors.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'color':"&startCol&" ,  'gradstate':"&StartPercent&", 'gradcolor':"&endCol&"}" 
end sub
Sub pDMDLabelSetColorGradient2(labName, myText, ByVal startCol, ByVal EndCol)
    Dim GS: GS = 1
    If startCol = EndCol Then GS = 0  ' Désactive le dégradé si les couleurs sont identiques
    
    If HasPuP Then
        PuPlayer.LabelSet pDMD, labName, myText, 1, "{'mt':2, 'color':" & startCol & ", 'gradstate':" & GS & ", 'gradcolor':" & EndCol & "}" 
    End If
End Sub

sub pDMDLabelSetGrayScale(labName, isGray)  'only on image objects.  will show as grayscale.  1=gray filter on 0=off normal mode
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'grayscale':"&isGray&"}" 
end sub

sub pDMDLabelSetFilter(labName, fMode)  ''fmode 1-5 (invertRGB, invert,grayscale,invertalpha,clear),blur)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'filter':"&fmode&"}" 
end sub

Sub pDMDLabelFlashFilter(LabName,byVal timeSec,fMode)   'timeSec in ms  'fmode 1-5 (invertRGB, invert,grayscale,invertalpha,clear,blur)
    if timeSec<20 Then timeSec=timeSec*1000
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':9,'fq':150,'len':" & (timeSec) & ",'fm':" & fMode & "}"   
end sub



sub pDMDLabelSetShadow(labName,lCol,offsetx,offsety,isVis)  ' shadow of text
dim ST: ST=1 : if isVIS=false Then St=0
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'shadowcolor':"&lCol&",'shadowtype': "&ST&", 'xoffset': "&offsetx&", 'yoffset': "&offsety&"}"
end sub

sub pDMDLabelSetBorder(labName,lCol,offsetx,offsety,isVis)   'outline/border around text.
dim ST: ST=2 : if isVIS=false Then St=0
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'shadowcolor':"&lCol&",'shadowtype': "&ST&", 'xoffset': "&offsetx&", 'yoffset': "&offsety&"}"
end sub



'animations   'pDMDLabelPulseText "pulsetext","jackpot",4000,rgb(100,0,0)

sub pDMDLabelPulseText(LabName,LabValue,mLen,mColor)       'mlen in ms
    PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':4,'hstart':80,'hend':200,'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'aa':0 }"
end Sub
sub pDMDLabelPulseText2(LabName,LabValue,mLen,mColor)       'mlen in ms
    PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelPulseNumber(LabName,LabValue,mLen,mColor,pNumStart,pNumEnd,pNumformat)   'pnumformat 0 no format, 1 with thousands  mLen=ms
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'numstart':"&pNumStart&",'numend' :"&pNumEnd&", 'numformat':"&pNumFormat&",'aa':0 }"    
end Sub

sub pDMDLabelPulseImage(LabName,mLen,isVis)       'mlen in ms isVis is state after animation
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0 }"
end Sub
sub pDMDLabelPulseImage2(LabName,mLen,isVis)       'mlen in ms isVis is state after animation
	PuPlayer.LabelSet pDMD, LabName, "`u`", isVis, "{'mt':1, 'at':2, 'fsstart':10, 'fsend':100, 'yps':0, 'ype':-10, 'len':" & (mLen) & "}" 
'	PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':4,'hstart':10,'hend':120,'len':" & (mLen) & ",'pspeed': 0 }" 
end Sub

sub pDMDLabelPulseTextEX(LabName,LabValue,mLen,mColor,isVis,zStart,zEnd)       'mlen in ms  same subs as above but youspecifiy zoom start and zoom end in % height of original font.
    PuPlayer.LabelSet pDMD,labName,LabValue,isVis,"{'mt':1,'at':4,'hstart':"&zStart&",'hend':"&zEnd&",'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelPulseNumberEX(LabName,LabValue,mLen,mColor,pNumStart,pNumEnd,pNumformat,isVis,zStart,zEnd)   'pnumformat 0 no format, 1 with thousands  mLen=ms
     PuPlayer.LabelSet pDMD,labName,LabValue,isVis,"{'mt':1,'at':4,'hstart':"&zStart&",'hend':"&zEnd&",'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'numstart':"&pNumStart&",'numend' :"&pNumEnd&", 'numformat':"&pNumFormat&",'aa':0}"    
end Sub

sub pDMDLabelPulseImageEX(LabName,mLen,isVis,zStart,zEnd)       'mlen in ms isVis is state after animation
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':4,'hstart':"&zStart&",'hend':"&zEnd&",'len':" & (mLen) & ",'pspeed': 0 }"
end Sub

sub pDMDLabelWiggleText(LabName,LabValue,mLen,mColor)       'mlen in ms  zstart MUST be less than zEND.  -40 to 40 for example
    PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':8,'rstart':-45,'rend':45,'len':" & (mLen) & ",'rspeed': 5,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelWiggleTextEX(LabName,LabValue,mLen,mColor,isVis,zStart,zEnd)       'mlen in ms  zstart MUST be less than zEND.  -40 to 40 for example
    PuPlayer.LabelSet pDMD,labName,LabValue,isVis,"{'mt':1,'at':8,'rstart':"&zStart&",'rend':"&zEnd&",'len':" & (mLen) & ",'rspeed': 5,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelWiggleImage(LabName,mLen,isVis)         'mlen in ms  zstart MUST be less than zEND.  -40 to 40 for example
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':8,'rstart':-45,'rend':45,'len':" & (mLen) & ",'rspeed': 5,'fc':" & 0 & ",'aa':0 }"
end Sub

sub pDMDLabelWiggleImageEX(LabName,mLen,isVis,zStart,zEnd)       'mlen in ms  zstart MUST be less than zEND.  -40 to 40 for example
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':8,'rstart':"&zStart&",'rend':"&zEnd&",'len':" & (mLen) & ",'rspeed': 5,'fc':" & 0 & ",'aa':0 }"
end Sub

'----GEMINI
Sub pDMDLabelStrobeImage(LabName, mLen, isVis)
    ' 'st': vitesse du clignotement
    PuPlayer.LabelSet pDMD, LabName, "`u`", isVis, "{'mt':1, 'at':1, 'st': 10, 'len':" & (mLen) & "}"
End Sub
'-------


sub pDMDPNGAnimate(labName,cSpeed)  'speed is frame timer, 0 = stop animation  100 is 10fps for animated png and gif nextframe timer.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'animate':"&cSpeed&"}" 
end sub

sub pDMDPNGAnimateEx(labName,startFrame,endFrame,LoopMode)  'sets up the apng/gif settings before you call animate.  if you set start/end frame same if will display that frame, set start to -1 to reset settings.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'gifstart':"&startFrame&",'gifend':"&endFrame&",'gifloop':"&loopMode&" }"          'gifstart':3, 'gifend':10, 'gifloop': 1
end sub

sub pDMDPNGShowFrame(labName,fFrame)  'in a animated png/gif, will set it to an individual frame so you could use as an imagelist control
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'gifstart':"&fFrame&",'gifend':"&fFrame&" }"          '
end sub

sub pDMDPNGAnimateOnce(labName,cSpeed)  'will show an animated gif/png and then hide when done, overrides loop to force stop at end.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'animate':"&cSpeed&", 'gifloop': 0 , 'aniendhide':1 }" 
end sub

sub pDMDPNGAnimateReset(labName)  'speed is frame timer, 0 = stop animation  100 is 10fps for animated png and gif nextframe timer, this will show anigif and hide at end no loop
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'animate':0, 'gifloop': 1 , 'aniendhide':0 , 'gifstart':-1}" 
end sub





sub pDMDLabelSetOutShadow(labName, lCol,offsetx,offsety,isOutline,isVis)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'shadowcolor':"&lCol&",'shadowstate': "&isVis&", 'xoffset': "&offsetx&", 'yoffset': "&offsety&", 'outline': "&isOutline&"}"
end sub

sub pDMDLabelMoveHorz(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd)   'pmovestart is -1= left-off 0=current pos 1=right-off    or can use % 
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pMoveStart&",'xpe' :"&pMoveEnd&", 'tt':2,'ad':1 }"    
end Sub

sub pDMDLabelMoveVert(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd)   'pmovestart is -1= left-off 0=current pos 1=right-off   or can use %  
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'yps':"&pMoveStart&",'ype' :"&pMoveEnd&", 'tt':2,'ad':1 }"    
end Sub

sub pDMDLabelMoveTO(LabName,LabValue,mLen,mColor,byVal pStartX,byVal pStartY,byVal pEndX,byVal pEndY)   'pmovestart is -1= left-off 0=current pos 1=right-off
     if pUseFramePos=1 AND (pStartX+pStartY+pEndx+pendY)>4 Then 
                       pTranslatePos pStartX,pStartY
                       pTranslatePos pEndX,pEndY
     end IF 
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pStartX&",'xpe' :"&pEndX& ",'yps':"&pStartY&",'ype' :"&pEndY&", 'tt':2 ,'ad':1}"    
end Sub

sub pDMDLabelMoveHorzFade(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd)   'pmovestart is -1= left-off 0=current pos 1=right-off, or can use %
     PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pMoveStart&",'xpe' :"&pMoveEnd&", 'tt':2 ,'ad':1, 'af':700}"    
end Sub

sub pDMDLabelMoveVertFade(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd)   'pmovestart is -1= left-off 0=current pos 1=right-off  or can use %   
     PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'yps':"&pMoveStart&",'ype' :"&pMoveEnd&", 'tt':2 ,'ad':1, 'af':700}"    
end Sub

sub pDMDLabelMoveTOFade(LabName,LabValue,mLen,mColor,byVal pStartX,byVal pStartY,byVal pEndX,byVal pEndY)   'pmovestart is -1= left-off 0=current pos 1=right-off
     if pUseFramePos=1 AND (pStartX+pStartY+pEndx+pendY)>4 Then 
                       pTranslatePos pStartX,pStartY
                       pTranslatePos pEndX,pEndY
     end IF 
     PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pStartX&",'xpe' :"&pEndX& ",'yps':"&pStartY&",'ype' :"&pEndY&", 'tt':6 ,'ad':1, 'af':700}"    
end Sub





sub pDMDLabelFadeOut(LabName,mLen)   'alpha is 255 max, 0=clear.  
     PuPlayer.LabelSet pDMD,labName,"`u`",0,"{'mt':1,'at':5,'astart':255,'aend':0,'len':" & (mLen) & " }"    
end Sub

sub pDMDLabelFadeIn(LabName,mLen)    'alpha is 255 max, 0=clear. 
     PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':5,'astart':0,'aend':255,'len':" & (mLen) & " }"    
end Sub


sub pDMDLabelFadePulse(LabName,mLen,mColor)   'alpha is 255 max, 0=clear. alpha start/end and pulsespeed of change
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':6,'astart':70,'aend':255,'len':" & (mLen) & ",'pspeed': 40,'fc':" & mColor & "}" 
end Sub

Sub pDMDLabelFlash(LabName,byVal timeSec, mColor)   'timeSec in ms
    if timeSec<20 Then timeSec=timeSec*1000
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec) & ",'fc':" & mColor & "}"   
end sub



sub pDMDScreenFadeOut(LabName,mLen)   'alpha is 255 max, 0=clear.  
     PuPlayer.LabelSet pDMD,labName,"`u`",0,"{'mt':1,'at':7,'astart':255,'aend':0,'len':" & (mLen) & " }"    
end Sub

sub pDMDScreenFadeIn(LabName,mLen)    'alpha is 255 max, 0=clear. 
     PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':7,'astart':0,'aend':255,'len':" & (mLen) & " }"    
end Sub



Sub pDMDScrollBig(LabName,msgText,byVal timeSec,mColor) 'timeSec in MS
if timeSec<20 Then timeSec=timeSec*1000
PuPlayer.LabelSet pDMD,LabName,msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec) & ",'mlen':" & (timeSec*1) & ",'tt':0,'fc':" & mColor & "}"
end sub

Sub pDMDScrollBigV(LabName,msgText,byVal timeSec,mColor) 'timeSec in MS
if timeSec<20 Then timeSec=timeSec*1000
PuPlayer.LabelSet pDMD,LabName,msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec) & ",'mlen':" & (timeSec*0.8) & ",'tt':0,'fc':" & mColor & "}"
end sub


Sub pDMDZoomBig(LabName,msgText,byVal timeSec,mColor,isVis,byVal zStart,byVal zEnd)  'timeSec in MS  zstart/end is % of screen height  notice aa antialias is 0 for big font zooms for performance.  'ns is size by %label height.
if timeSec<20 Then timeSec=timeSec*1000
PuPlayer.LabelSet pDMD,LabName,msgText,isVis,"{'mt':1,'at':3,'hstart':" & (zStart) & ",'hend':" & (zEnd) & ",'len':" & (timeSec) & ",'mlen':" & (timeSec*0.4) & ",'tt':" & 0 & ",'fc':" & mColor & ", 'ns':1, 'aa':0}"
end sub




Sub AudioDuckPuP(MasterPuPID,VolLevel)  
'will temporary volume duck all pups (not masterid) till masterid currently playing video ends.  will auto-return all pups to normal.
'VolLevel is number,  0 to mute 99 for 99%  
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& MasterPuPID& ", ""FN"": 42, ""DV"": "&VolLevel&" }"             
end Sub

Sub AudioDuckPuPAll(MasterPuPID,VolLevel)  
'will temporary volume duck all pups (not masterid) till masterid currently playing video ends.  will auto-return all pups to normal.
'VolLevel is number,  0 to mute 99 for 99%  
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& MasterPuPID& ", ""FN"": 42, ""DV"": "&VolLevel&" , ""ALL"":1 }"             
end Sub




Sub pSetAspectRatio(PuPID, arWidth, arHeight)
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&PuPID& ", ""FN"": 50, ""WIDTH"": "&arWidth&", ""HEIGHT"": "&arHeight&" }"   
end Sub  

Sub pDisableLoopRefresh(PuPID)
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&PuPID& ", ""FN"": 2, ""FF"":0, ""FO"":0 }"   
end Sub  

'set safeloop mode on current playing media.  Good for background videos that refresh often?  { "mt":301, "SN": XX, "FN":41 }
Sub pSafeLoopModeCurrentVideo(PuPID)
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&PuPID& ", ""FN"": 41 }"   
end Sub  

Sub pSetLowQualityPc  'sets fulldmd to run in lower quality mode (slowpc mode)  AAlevel for text is removed and other performance/quality items.  default is always run quality, 
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 5, ""FN"":45, ""SP"":1 }"    'slow pc mode
end Sub 

Sub pDMDLabelDispose(labName)   'not needed unless you want to want to free a heavy resource label from cache/memory.  or temp lables that you created.  performance reasons.
      PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'dispose': 1 }"   
end Sub

Sub pDMDAlwaysPAD  'will pad all text with a space before and after to help with possible text clipping.
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 5, ""FN"":46, ""PA"":1 }"    'slow pc mode
end Sub   


Sub pDMDSetHUD(isVis)   'show hide just the pBackGround object (HUD overlay).      
    pDMDLabelVisible "pBackGround",isVis
end Sub  




Sub pDMDSetPage(pagenum)    
    PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub

Sub pDMDSplashPage(pagenum, cTime)    'cTime is seconds.  3 5,  it will auto return to current page after ctime
    PuPlayer.LabelShowPage pDMD,pagenum,cTime,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub



Sub PDMDSplashPagePlaying(pagenum)  'will hide HUD and show labepage while current media is playing. and then autoreturn.
    PuPlayer.LabelShowPage pDMD,pagenum,500,"hidehudplay"
end Sub    

Sub PDMDSplashPagePlayingHUD(pagenum)  'will show labelpage and auto return to def after current video stopped
    PuPlayer.LabelShowPage pDMD,pagenum,500,"returnplay"
end Sub    


Sub pHideOverlayDuringCurrentPlay() 'will hide pup text labels and HUD till current video stops playing.
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& "5"& ", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
end Sub


Sub pSetVideoPosMS(mPOS)  'set position of video/audio in ms,  must be playing already or will be ignored.  { "mt":301, "SN": XX, "FN":51, "SP": 3431} 
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& "5"& ", ""FN"": 51, ""SP"":"&mPOS&" }"
end Sub

sub pAllVisible(lvis)   '0/1 to show hide pup text overlay and HUD
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& "5"& ",""OT"":"&lvis&", ""FN"": 3 }"             'hideoverlay text force
end Sub


Sub pDMDSetBackFrame(fname)
  PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
end Sub

Sub pDMDBackLoopStart(fPlayList,fname)
  PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
  PuPlayer.SetBackGround pDMD,1
end Sub

Sub pDMDBackLoopStop
  PuPlayer.SetBackGround pDMD,0
  PuPlayer.playstop pDMD
end Sub

'jukebox mode will auto advance to next media in playlist and you can use next/prior sub to manuall advance
'you should really have a specific pupid# display like musictrack that is only used for the playlist
'sub PUPDisplayAsJukebox(pupid) needs to be called/set prior to sending your first media to that pupdisplay.
'pupid=pupdiplay# like pMusic

Sub PUPDisplayAsJukebox(pupid)
PuPlayer.SendMSG("{'mt':301, 'SN': " & pupid & ", 'FN':30, 'PM':1 }")
End Sub

Sub PuPlayListPrior(pupid)
 PuPlayer.SendMSG("{'mt':301, 'SN': " & pupid & ", 'FN':31, 'PM':1 }")
End Sub

Sub PuPlayListNext(pupid)
 PuPlayer.SendMSG("{'mt':301, 'SN': " & pupid & ", 'FN':31, 'PM':2 }")
End Sub

Sub pDMDPause()
 PuPlayer.playpause pDMD
end Sub

Sub pDMDResume()
 PuPlayer.playresume pDMD
end Sub

Sub pDMDStop()
 PuPlayer.playstop pDMD
end Sub

Sub pDMDVolumeDef(cVol)  'sets the default volume of player, doesnt affect current playing media
 PuPlayer.setVolume pdmd,cVol
end Sub

Sub pDMDVolumeCurrent(cVol)  'sets the volume of current media (like to duck audio), doesnt affect default volume for next media.
 PuPlayer.setVolumeCurrent pdmd,cVol
end Sub

Sub pDMDSetLoop(isLoop)     'it will loop the currently playing file 0=cancel looping 1=loop
 PuPlayer.setLoop pDMD,isLoop
end Sub

Sub pDMDBackground(isBack)  'will set the currently playing file as background video and continue to loop and return to it automatically 0=turn off as background.
 PuPlayer.setBackground pDMD,isBack
end Sub


Sub PuPEvent(EventNum)
if hasPUP=false then Exit Sub
PuPlayer.B2SData "E"&EventNum,1  'send event to puppack driver  
End Sub


Sub pupCreateLabel(lName, lValue, lFont, lSize, lColor, xpos, ypos,pagenum, lvis)
PuPlayer.LabelNew pDMD,lName ,lFont,lSize,lColor,0,1,1,1,1,pagenum,lvis
if pUseFramePos=1 Then pTranslatePos xpos,ypos
if pUseFramePos=1 Then pTranslateY lSize
PuPlayer.LabelSet pDMD,lName,lValue,lvis,"{'mt':2,'xpos':"& xpos & ",'ypos':"&ypos&",'fonth':"&lsize&",'v2':1 }"
end Sub

Sub pupCreateLabelImage(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
PuPlayer.LabelNew pDMD,lName ,"",50,RGB(100,100,100),0,1,1,0,1,pagenum,lvis
if pUseFramePos=1 Then pTranslatePos xpos,ypos
if pUseFramePos=1 Then pTranslatePos Iwidth,iHeight
PuPlayer.LabelSet pDMD,lName,lFilename,lvis,"{'mt':2,'width':"&IWidth&",'height':"&Iheight&",'xpos':"&xpos&",'ypos':"&ypos&",'v2':1 }"
end Sub


Sub pSetPageLayouts

DIM dmddef
DIM dmdalt
DIM dmdscr
DIM dmdfixed
DIM dmdtomb
DIM dmdExo
Dim GoldH : GoldH = 55255
Dim GoldB : GoldB = 213

'Using FULL BIG LCD PuPDMD  ************ lcd **************
'
'Sub CreateLabel(lName, lValue, lFont, lSize, lColor, xpos, ypos, alignX, alignY, pagenum, lvis)
'
'Sub CreateLabelImage(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
	
		dmdalt="PKMN Pinball"
		dmdfixed="Instruction"
		dmdscr="Impact"    'main scorefont
		dmddef="Zig"
		dmdtomb="Ruben"
		dmdExo="Exo 2 Extra Bold"

	'Page 1 (default score display)
		'PuPlayer.LabelNew pDMD,"Credits" ,dmddef,20,33023   ,0,2,2,95,0,1,0
       
        pDMDAlwaysPAD		'we pad all text with space before and after for shadow clipping/etc
'1920,1080
		pupCreateLabel "Credits","",      dmdscr, 70,  5112227	,154,130,1,0
		pupCreateLabel "Ball","",         dmdscr, 65,  5112227	,154,190,1,0
		pupCreateLabel "Play1","",        dmdscr, 105, 436985	,160,54,1,0
		pupCreateLabel "CurScore","",     dmdscr, 200, 436985	,980,980,1,0 'ok		
		pupCreateLabel "Astrite","",      dmdscr, 110, 16777215	,1843,562,1,0
		pupCreateLabel "ForgingTibe","",  dmdscr, 110, 16777215	,1843,400,1,0
		pupCreateLabel "LustrousTibe","", dmdscr, 110, 16777215	,1843,238,1,0
		pupCreateLabel "Corals","",       dmdscr, 110, 16777215	,1843,724,1,0
		pupCreateLabel "CScoreP1","",     dmdExo, 60,  32896	,500,135,1,0
		pupCreateLabel "CScoreP2","",     dmdExo, 60,  12870144	,825,135,1,0
		pupCreateLabel "CScoreP3","",     dmdExo, 60,  128		,1150,135,1,0
		pupCreateLabel "CScoreP4","",     dmdExo, 60,  1992703,  1475,135,1,0
		pupCreateLabel "AutoSelecTime","",      dmdscr, 90,  clWhite	,980,870,1,0
		pupCreateLabel "JackpotValue1","",       dmdscr, 45,  clWhite	,1750,35,1,0
		pupCreateLabel "JackpotValue2","",       dmdscr, 55,  clWhite	,1750,80,1,0
		pupCreateLabel "MiObjective","",       dmdscr, 80,  clred	,980,700,1,0
		pupCreateLabel "Mi1State","",       dmdscr, 95,  clred	,980,770,1,0
		pupCreateLabel "Miss1EndTime","",   dmdscr, 70,  clWhite	,980,630,1,0
		pupCreateLabel "Miss2EndTime","",   dmdscr, 70,  clWhite	,980,630,1,0
		pupCreateLabel "Mi2State","",       dmdscr, 95,  clred	,980,770,1,0
		pupCreateLabel "Mi3State","",       dmdscr, 95,  clred	,980,770,1,0
		pupCreateLabel "Mi4State","",       dmdscr, 95,  clred	,980,770,1,0
		pupCreateLabel "Mi5State","",       dmdscr, 95,  clred	,980,770,1,0
		pDMDLabelSetBorder "CurScore",clBlack,2,2,1
		pDMDLabelSetShadow "CurScore",RGB(0,0,0),3,3,1
		pDMDLabelSetColorGradient2 "CurScore", "", GoldH, GoldB
		pDMDLabelSetBorder "Play1",clBlack,2,2,1
		pDMDLabelSetColorGradient2 "Play1", "", GoldH, GoldB
		pDMDLabelSetBorder "Astrite",clBlack,2,2,1
		pDMDLabelSetBorder "ForgingTibe",clBlack,2,2,1
		pDMDLabelSetBorder "LustrousTibe",clBlack,2,2,1
		pDMDLabelSetBorder "Corals",clBlack,2,2,1
		pDMDLabelSetBorder "AutoSelecTime",clBlack,2,2,1
		pDMDLabelSetBorder "MiObjective",clBlack,2,2,1
		pDMDLabelSetBorder "Mi1State",clBlack,2,2,1
		pDMDLabelSetBorder "Mi2State",clBlack,2,2,1
		pDMDLabelSetBorder "Mi3State",clBlack,2,2,1
		pDMDLabelSetBorder "Mi4State",clBlack,2,2,1
		pDMDLabelSetBorder "Mi5State",clBlack,2,2,1
		pupCreateLabel "AddAstrite","",      dmdscr, 100, 16777215	,1760,562,1,0
		pupCreateLabel "AddForgingTibe","",  dmdscr, 100, 16777215	,1760,400,1,0
		pupCreateLabel "AddLustrousTibe","", dmdscr, 100, 16777215	,1760,238,1,0
		pupCreateLabel "AddCorals","",       dmdscr, 100, 16777215	,1760,724,1,0
		pDMDLabelSetBorder "AddAstrite",clBlack,2,2,1
		pDMDLabelSetBorder "AddForgingTibe",clBlack,2,2,1
		pDMDLabelSetBorder "AddLustrousTibe",clBlack,2,2,1
		pDMDLabelSetBorder "AddCorals",clBlack,2,2,1
		pupCreateLabelImage "Thunder001", "PuPAlphas\\Thunder001.png",	1450,200,1000,1000,1,0 'Right
		pupCreateLabelImage "Thunder002", "PuPAlphas\\Thunder002.png",	1500,440,1000,1000,1,0 'Right
		pupCreateLabelImage "Thunder003", "PuPAlphas\\Thunder003.png",	1400,800,1000,1000,1,0 'Right
		pupCreateLabelImage "Thunder004", "PuPAlphas\\Thunder004.png",	300,300,1000,1000,1,0 'left
		pupCreateLabelImage "Thunder005", "PuPAlphas\\Thunder005.png",	400,800,1000,1000,1,0 'left
		pupCreateLabelImage "Thunder006", "PuPAlphas\\Thunder006.png",	1400,700,1000,1000,1,0 'center
		pupCreateLabelImage "Thunder007", "PuPAlphas\\Thunder007.png",	980,600,1000,1000,1,0 'center
		pupCreateLabelImage "Thunder008", "PuPAlphas\\Thunder008.png",	400,800,1000,1000,1,0 'center
		pupCreateLabelImage "Cirle1", "PuPAlphas\\Circle001.png",		980,500,1400,1400,1,0
		pupCreateLabelImage "Cirle2", "PuPAlphas\\Circle002.png",		980,500,1400,1400,1,0
'		pupCreateLabelImage "Magna1", "PuPAlphas\\MagnaLock.png",	980,420,1320,1000,1,0
		pupCreateLabelImage "Magna1", "PuPAlphas\\MagnaLock.png",	980,450,1920,1200,1,0

		'(Text 1 Line)
		pupCreateLabel "Texte1a","",    dmdscr, 270, 16732584, 980,  324, 1, 0
		pDMDLabelSetBorder "Texte1a",clBlack,2,2,1
		'(Text 2 Line)
		pupCreateLabel "Texte2a","",    dmdscr, 200, 16732584, 980,  360, 1, 0
		pupCreateLabel "Texte2b","",    dmdscr, 180, 5112227,  980,  580, 1, 2
		pDMDLabelSetBorder "Texte2a",clBlack,2,2,1
		pDMDLabelSetBorder "Texte2b",clBlack,2,2,1
		'(Text 3 Line)
		pupCreateLabel "Texte3a","",    dmdscr, 190,  5112227,  980,  300, 1, 0
		pupCreateLabel "Texte3b","",    dmdscr, 165,  16732584, 980,  450, 1, 0
		pupCreateLabel "Texte3c","",    dmdscr, 190,  5112227,  980,  600, 1, 0
		
		'EnterHighScore
		pupCreateLabel "HighScore","",   dmdscr, 100,  436985,    922,  756, 1, 1
		pupCreateLabel "HighScoreL1","", dmdscr, 100,  5112227, 1286, 756, 1, 1
		pupCreateLabel "HighScoreL2","", dmdscr, 100,  5112227, 1363, 756, 1, 1
		pupCreateLabel "HighScoreL3","", dmdscr, 100,  5112227,  1440, 756, 1, 1
		pDMDLabelSetBorder "HighScore",clBlack,2,2,1

		'onverlay 	
'		PuPlayer.LabelNew pDMD,"SkinSelect,zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		pupCreateLabelImage "SkinSelect1", "PuPAlphas\\Skin001.png",960,540,1920,1080,1,0
		pupCreateLabelImage "SkinSelect2", "PuPAlphas\\Skin002.png",960,540,1920,1080,1,0
		pupCreateLabelImage "SkinSelect3", "PuPAlphas\\Skin003.png",960,540,1920,1080,1,0
		pupCreateLabelImage "SkinSelect4", "PuPAlphas\\Skin004.png",960,540,1920,1080,1,0
		pupCreateLabelImage "SkinSelect5", "PuPAlphas\\Skin005.png",960,540,1920,1080,1,0
		pupCreateLabelImage "SkinSelect6", "PuPAlphas\\Skin006.png",960,540,1920,1080,1,0
		pupCreateLabelImage "SkinSelect7", "PuPAlphas\\Skin007.png",960,540,1920,1080,1,0
		pupCreateLabelImage "SkinSelect8", "PuPAlphas\\Skin008.png",960,540,1920,1080,1,0
'		PuPlayer.LabelNew pDMD,"MissSelect",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		pupCreateLabelImage "MissSelect1", "PuPAlphas\\Miss001.png",960,540,1920,1080,1,0
		pupCreateLabelImage "MissSelect2", "PuPAlphas\\Miss002.png",960,540,1920,1080,1,0
		pupCreateLabelImage "MissSelect3", "PuPAlphas\\Miss003.png",960,540,1920,1080,1,0
		pupCreateLabelImage "MissSelect4", "PuPAlphas\\Miss004.png",960,540,1920,1080,1,0
		pupCreateLabelImage "MissSelect5", "PuPAlphas\\Miss005.png",960,540,1920,1080,1,0
		pupCreateLabelImage "MissSelect6", "PuPAlphas\\Miss006.png",960,540,1920,1080,1,0	  'No now
		PuPlayer.LabelNew pDMD,"MissSelectNO",zoomfont,		6,16777215 	,0,1,1, 0,0,1,0		'No monney
 		PuPlayer.LabelNew pDMD,"MissSelectOK",zoomfont,		6,16777215 	,0,1,1, 0,0,1,0		'Success
		PuPlayer.LabelNew pDMD,"SkinActive",zoomfont,		6,16777215 	,0,1,1, 0,0,1,0

				'Bonus 
		pupCreateLabel "BonusMultip","", dmdExo, 70,  59624,    1400, 340, 1, 1
		pupCreateLabel "Bonus1","",      dmdExo, 85,  5112227,  768,  216, 1, 1
		pupCreateLabel "Bonus2","",      dmdExo, 85,  5548032,  768,  302, 1, 1
		pupCreateLabel "Bonus3","",      dmdExo, 85,  5112227,  768,  388, 1, 1
		pupCreateLabel "Bonus4","",      dmdExo, 85,  5548032,  768,  475, 1, 1
		pupCreateLabel "BonusTotal","",  dmdExo, 130,  436985,  998,  648, 1, 1
		pDMDLabelSetBorder "BonusMultip",clBlack,2,2,1
		pDMDLabelSetBorder "BonusTotal",clBlack,2,2,1
		pDMDLabelSetBorder "Bonus1",clBlack,2,2,1
		pDMDLabelSetBorder "Bonus2",clBlack,2,2,1
		pDMDLabelSetBorder "Bonus3",clBlack,2,2,1
		pDMDLabelSetBorder "Bonus4",clBlack,2,2,1
		pDMDLabelSetColorGradient2 "Bonus1", "", 8388863, 7929977  'def1
		pDMDLabelSetColorGradient2 "Bonus2", "", GoldH, 5548032
		pDMDLabelSetColorGradient2 "Bonus3", "", 8388863, 7929977
		pDMDLabelSetColorGradient2 "Bonus4", "", GoldH, 5548032
		pDMDLabelSetColorGradient2 "BonusTotal", "", GoldH, GoldB
		pDMDLabelSetColorGradient2  "BonusMultip", "", 65535, 28527

		'Page 3 (default Attract)
		pupCreateLabel "AttractTitle", "", dmdExo, 170, 16732584, 980, 250, 3, 0
		pupCreateLabel "AttractLine1", "", dmdExo, 120, 5112227, 980, 400, 3, 0 
		pupCreateLabel "AttractLine2", "", dmdExo, 120, 5112227, 980, 540, 3, 0
		pupCreateLabel "AttractLine3", "", dmdExo, 120, 5112227, 980, 680, 3, 0
		pupCreateLabel "AttractLine4", "", dmdExo, 120, 436985, 980, 950, 3, 0
        pDMDLabelSetShadow "AttractTitle",RGB(0,0,0),3,3,1
		pDMDLabelSetBorder "AttractTitle",clBlack,2,2,1
        pDMDLabelSetBorder "AttractLine1",clBlack,2,2,1
        pDMDLabelSetBorder "AttractLine2",clBlack,2,2,1
        pDMDLabelSetBorder "AttractLine3",clBlack,2,2,1
		pDMDLabelSetBorder "AttractLine4",clBlack,2,2,1
		pupCreateLabel "TmpCredits","",      dmdscr, 70,  5112227	,154,130,3,0
		pDMDLabelSetColorGradient2  "AttractTitle", "", 16744576, 8388672
		pDMDLabelSetColorGradient2  "AttractLine1", "", 8388863, 3801205
		pDMDLabelSetColorGradient2  "AttractLine2", "", 8388863, 3801205
		pDMDLabelSetColorGradient2  "AttractLine3", "", 8388863, 3801205
		pDMDLabelSetColorGradient2  "AttractLine4", "", 65535, 21930
		pupCreateLabel "TmpCredits","",      dmdscr, 70,  5112227	,154,130,3,0
      
        'sample for image label
        'pupCreateLabelImage "cJewel","dmdmisc\\jewel.png",91.25,80.3,15,30,1,1
end Sub 'page Layouts



Sub pDMDSplashBig(msgText,timeSec, mColor)   'note timesec is seconds( 2, 3..etc) , if timesec>1000 then its ms. (2300, 3200)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & ",'fc':" & mColor & "}"   
end sub

Sub pDMDSplashScore(msgText,timeSec, mColor)   'note timesec is seconds( 2, 3..etc) , if timesec>1000 then its ms. (2300, 3200)
PuPlayer.LabelSet pDMD,"ScoreSplash",msgText,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & ",'fc':" & mColor & "}"   
end sub



'************************ called during gameplay to update Scores ***************************
Dim CurTestScore:CurTestScore=100000
Sub pUpdateScores  'call this ONLY on timer 300ms is good enough
if pDMDCurPage <> pScores then Exit Sub
	puPlayer.LabelSet pDMD,"CurScore","" & FormatNumber(PlayerScore(CurrentPlayer), 0),1,""
	puPlayer.LabelSet pDMD,"Play1","Player  " & FormatNumber(CurrentPlayer, 0),1,""
	puPlayer.LabelSet pDMD,"Ball","Balls " & FormatNumber(BallinGame(CurrentPlayer), 0) & "/" & FormatNumber(BallsPerGame, 0),1,""
	PuPlayer.LabelSet pDMD,"PlayersNumber","Players "& FormatNumber(PlayersPlayingGame, 0) & "/4",1,""
	PuPlayer.LabelSet pDMD,"Credits","Crédits "& Credits,1,""
	PuPlayer.LabelSet pDMD,"Astrite",""& FormatNumber(AstriteCount(CurrentPlayer), 0),1,""
	PuPlayer.LabelSet pDMD,"ForgingTibe",""& FormatNumber(ForgingTibeCount(CurrentPlayer), 0),1,""
	PuPlayer.LabelSet pDMD,"LustrousTibe",""& FormatNumber(LustrousTibeCount(CurrentPlayer), 0),1,""
	PuPlayer.LabelSet pDMD,"Corals",""& FormatNumber(CoralsCount(CurrentPlayer), 0),1,""
	PuPlayer.LabelSet pDMD,"JackpotValue1","JACKPOT Value",1,""
	PuPlayer.LabelSet pDMD,"JackpotValue2",""& FormatNumber(JackpotValue(CurrentPlayer), 0),1,""
'MiObjective
	If Mission1_State(CurrentPlayer) = 2 Then
		PuPlayer.LabelSet pDMD,"MiObjective","Shoot Orbit",1,""
		PuPlayer.LabelSet pDMD,"Mi1State","Rectifier Objective : "& FormatNumber(OrbiteCount, 0) & "/6",1,""
	Else
		PuPlayer.LabelSet pDMD,"Mi1State","Rectifier Objective : "& FormatNumber(OrbiteCount, 0) & "/6",0,""
	End If
	If Mission2_State(CurrentPlayer) = 2 Then
		PuPlayer.LabelSet pDMD,"MiObjective","Hit Moving Targets",1,""
		If PersonnageActive(CurrentPlayer) = 7 or PersonnageActive(CurrentPlayer) = 6 Then
			PuPlayer.LabelSet pDMD,"Mi2State","Pistol Objective : "& FormatNumber(LightMI2Count, 0) & "/3",1,""
		Else 
			PuPlayer.LabelSet pDMD,"Mi2State","Pistol Objective : "& FormatNumber(LightMI2Count, 0) & "/6",1,""
		End If
	Else
		PuPlayer.LabelSet pDMD,"Mi2State","Pistol Objective : "& FormatNumber(LightMI2Count, 0) & "/6",0,""
	End If
	If Mission3_State(CurrentPlayer) = 2 Then
		PuPlayer.LabelSet pDMD,"MiObjective","Hit Bumpers",1,""
		If PersonnageActive(CurrentPlayer) = 2 or PersonnageActive(CurrentPlayer) = 5 Then
			PuPlayer.LabelSet pDMD,"Mi3State","Gauntlet Objective : "& FormatNumber(BumperCount, 0) & "/10",1,""
		Else
			PuPlayer.LabelSet pDMD,"Mi3State","Gauntlet Objective : "& FormatNumber(BumperCount, 0) & "/20",1,""
		End If
	Else
		PuPlayer.LabelSet pDMD,"Mi3State","Gauntlet Objective : "& FormatNumber(BumperCount, 0) & "/20",0,""
	End If
	If Mission4_State(CurrentPlayer) = 2 Then
		PuPlayer.LabelSet pDMD,"MiObjective","Hit All Ramps Twice",1,""
		PuPlayer.LabelSet pDMD,"Mi4State","Sword Objective : "& FormatNumber(LightMI4Count, 0) & "/4",1,""
	Else
		PuPlayer.LabelSet pDMD,"Mi4State","Sword Objective : "& FormatNumber(LightMI4Count, 0) & "/4",0,""
	End If
	If Mission5_State(CurrentPlayer) = 2 Then
		PuPlayer.LabelSet pDMD,"MiObjective","Hit All Lit Shoot Twice",1,""
		If PersonnageActive(CurrentPlayer) = 3 or PersonnageActive(CurrentPlayer) = 4 Then
			PuPlayer.LabelSet pDMD,"Mi5State","Broadblade Objective : "& FormatNumber(LightMI5Count, 0) & "/4",1,""
		Else
			PuPlayer.LabelSet pDMD,"Mi5State","Broadblade Objective : "& FormatNumber(LightMI5Count, 0) & "/8",1,""
		End If
	Else
		PuPlayer.LabelSet pDMD,"Mi5State","Broadblade Objective : "& FormatNumber(LightMI5Count, 0) & "/8",0,""
	End If
'1 Rectifier - Orbite 15 (Timer)
'2 Pistol - 6 Cibles All Light Turn (Timer)
'3 Gauntlet - Bumber Hits 20
'4 Sword - All Ramp x 2
'5 Broadblade - All Light x 2	
' OrbiteCount 'Mi 1
' LightMI2Count 'Mi2
' BumperCount 'Mi 3
' LightMI4Count 'Mi4
' LightMI5Count 'Mi5

If PlayersPlayingGame <= 1 or UseBGB2S = 1 Then 
	PuPlayer.LabelSet pDMD,"CScoreP1","P1 - " & FormatNumber(PlayerScore(1),0),0,""
	PuPlayer.LabelSet pDMD,"CScoreP2","P2 - " & FormatNumber(PlayerScore(2),0),0,""
	PuPlayer.LabelSet pDMD,"CScoreP3","P3 - " & FormatNumber(PlayerScore(3),0),0,""
	PuPlayer.LabelSet pDMD,"CScoreP4","P4 - " & FormatNumber(PlayerScore(4),0),0,""
Else
	If PlayersPlayingGame > 1 Then PuPlayer.LabelSet pDMD,"CScoreP1","P1 - " & FormatNumber(PlayerScore(1),0),1,""
	If PlayersPlayingGame >= 2 Then PuPlayer.LabelSet pDMD,"CScoreP2","P2 - " & FormatNumber(PlayerScore(2),0),1,""
	If PlayersPlayingGame >= 3 Then PuPlayer.LabelSet pDMD,"CScoreP3","P3 - " & FormatNumber(PlayerScore(3),0),1,""
	If PlayersPlayingGame >= 4 Then PuPlayer.LabelSet pDMD,"CScoreP4","P4 - " & FormatNumber(PlayerScore(4),0),1,""
End If
end Sub

PUPINIT   '*****REMINDER **********  you need to call PUPINIT sub in last line of table1_init



'*************************** SAMPLE END OF BALL BONUS SCREEN,  same idea/concept can be used for ATTRACT MODE too **************************************

'sub EndOfBallEnd
'pDMDSetPage 1 'put back to scores
'end Sub
'
'Sub EndOfBallNext
'    Select Case EOBCurIndex
'
'        Case 0
'            pDMDSplashLinesEOB "", FormatNumber(Bonus,0,, ,True) & " x " & BonusMult, 1, clWhite
'            EndOfBallBonus
'            TriggerScript 2200,"EndOfBallNext'"
'
'        Case 1
'            pDMDSplashLinesEOB "", FormatNumber(TotalBonusScore,0,, ,True), 1, clWhite
'            TriggerScript 2200,"EndOfBallNext'"
'
'        Case 2
'            EndOfBallEnd
'
'    End Select
'
'    EOBCurIndex = EOBCurIndex + 1
'End Sub
'
'sub EndOfBallStart
'pDMDSetPage 4  'set to EOB screen
'EOBCurIndex=0
'EndOfBallNext
'end Sub
'
'
'Sub pDMDSplashLinesEOB(msgText,msgText2,timeSec,mColor)
'	PuPlayer.LabelSet pDMD,"EOBLine1",msgText,0,"{'mt':1,'at':1,'fq':250,'len':" & (timeSec*1000) & ",'fc':" & mColor & "}"
'	PuPlayer.LabelSet pDMD,"EOBLine2",msgText2,0,"{'mt':1,'at':1,'fq':250,'len':" & (timeSec*1000) & ",'fc':" & mColor & "}"   
'end Sub
'---------
Sub pDMDmessage1L(msgText,timeSec)
	DmDmessageOFFTimer.Enabled = False
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelSet pDMD,"Texte1a",msgText,vis,pLine1Ani
	DmDmessageOFFTimer.Enabled = True
end Sub
Sub pDMDmessage2L(msgText,msgText2,timeSec)
	DmDmessageOFFTimer.Enabled = False
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelSet pDMD,"Texte2a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Texte2b",msgText2,vis,pLine2Ani
	DmDmessageOFFTimer.Enabled = True
end Sub

Sub pDMDmessage3L(msgText,msgText2,msgText3,timeSec)
	DmDmessageOFFTimer.Enabled = False
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelSet pDMD,"Texte3a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Texte3b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Texte3c",msgText3,vis,pLine3Ani
	DmDmessageOFFTimer.Enabled = True
end Sub

	'need to make a timer and add and remove to it instead of showtext and hide text
Sub DmDmessageOFFTimer_Timer()
	ResetMessage123L
End Sub 
Sub ResetMessage123L
	DmDmessageOFFTimer.Enabled = False
	PuPlayer.LabelSet pDMD, "Texte1a","",0,""
	PuPlayer.LabelSet pDMD, "Texte2a","",0,""
	PuPlayer.LabelSet pDMD, "Texte2b","",0,""
	PuPlayer.LabelSet pDMD, "Texte3a","",0,""
	PuPlayer.LabelSet pDMD, "Texte3b","",0,""
	PuPlayer.LabelSet pDMD, "Texte3c","",0,""
End Sub 
'****************************************************************************************************************************************
' ATTRACT MODE
'****************************************************************************************************************************************
Dim AttractIndex

Sub pAttractStart
    pInAttract = True 
    hsbModeActive = False
    pDMDSetPage 3
    AttractIndex = 0
    AttractNext
End Sub

Sub AttractNext
    If pInAttract = False Then Exit Sub

    Select Case AttractIndex
			' Use fade/pulse effect instead of blinking
		Case 0
			pDMDSplashLinesAttract "", "", "","","", 2
			TriggerScript 500, "AttractNext" 

        Case 1 
            pDMDSplashLinesAttract "", "","","","GAME OVER", 2
'			PuPlayer.LabelSet pDMD,"tmpCredits","Crédits "& Credits,1,""
            TriggerScript 7000, "AttractNext" 
            AttractIndex = 1 
			
        Case 2
           pDMDSplashLinesAttract _
           "HIGH SCORES", _
			"1. " & HighScoreName(0) & " - " & FormatNumber(HighScore(0), 0), _
            "2. " & HighScoreName(1) & " - " & FormatNumber(HighScore(1), 0), _
            "3. " & HighScoreName(2) & " - " & FormatNumber(HighScore(2), 0), _
            "GAME OVER", _
            4000
            TriggerScript 7000, "AttractNext"
		Case 3
			If(Credits> 0) then
				pDMDSplashLinesAttract "", "PRESS","START","","GAME OVER", 2
			Else
				pDMDSplashLinesAttract "", "INSERT","Coins","","GAME OVER", 2
			End If
			PuPEvent 840
            TriggerScript 7000, "AttractNext"

        Case 4
            pDMDSplashLinesAttract "WuWa", "By", "Tombg & Gman","","", 2
            TriggerScript 14000, "AttractNext"
			Playerscore(CurrentPlayer) = 0
            AttractIndex = -1 
    End Select

    AttractIndex = AttractIndex + 1
End Sub


Sub pDMDSplashLinesAttract(msgTitle, msgText1, msgText2, msgText3, msgText4, timeMs)
    PuPlayer.LabelSet pDMD, "AttractTitle", msgTitle, 1, "" 
    PuPlayer.LabelSet pDMD, "AttractLine1", msgText1, 1, ""
    PuPlayer.LabelSet pDMD, "AttractLine2", msgText2, 1, ""
    PuPlayer.LabelSet pDMD, "AttractLine3", msgText3, 1, ""
    PuPlayer.LabelSet pDMD, "AttractLine4", msgText4, 1, ""
End Sub

Sub AttractEnd
    AttractIndex = 0                ' reset index
    pInAttract = False              ' stops AttractNext from doing anything
	PuPEvent 841
	pDMDSplashLinesAttract "", "","","","", 2
    TriggerScript 2000, "ResetHUD"
End Sub

Sub ResetHUD()
    PuPEvent 841
	pDMDSetPage pScores
	If PersonnageActive(CurrentPlayer) = 0 Then PuPlayer.LabelSet pDMD,"SkinActive","PuPOverlays\\OverlayRover.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
End Sub

Sub pDMDGameOver
	playclear pDMD
	pAttractStart
end Sub

Sub pDMDStartGame
'	PersonnageActive(CurrentPlayer) = 0
	AttractEnd
end Sub
'************** Nailbuster TriggerScript Code v1.20
' create a timer in table named exactly pTriggerScript
' set interval on timer to 100ms (not enabled on startup)
' 
'  currently support up to 10 concurrent timers
'
'
'   usage:    TriggerScript <ms>, "vbcode to execute"
'
'   simpletest:  TriggerScript 3500,"MsgBox 1234"   'will show a dialog 1234
'

'Const TriggerScriptSize=10
'Dim pReset(10)                 ' TriggerScriptSize
'Dim pStatement(10)             ' TriggerScriptSize - holds future scripts
'Dim FX

Sub TriggersStopAll()
for fx=0 to TriggerScriptSize
    pReset(FX)=0
    pStatement(FX)=""
next
pTriggerScript.Enabled=False  'YOU MUST HAVE A TIMER NAMED pTriggerScript interval 100 not active on startup.
end Sub

TriggersStopAll



DIM pTriggerCounter:pTriggerCounter=pTriggerScript.interval    'YOU MUST HAVE A TIMER NAMED pTriggerScript interval 100 not active on startup.

Sub pTriggerScript_Timer()
    dim bMoreToRun:bMoreToRun=False
    for fx=0 to TriggerScriptSize  
        if pReset(fx)>0 Then    
            pReset(fx)=pReset(fx)-pTriggerCounter 
            if pReset(fx)<=0 Then
                pReset(fx)=0
                execute(pStatement(fx))
            end if
            bMoreToRun=True            
        End if
    next
    if bMoreToRun = False then pTriggerScript.Enabled=False    ' Disable when we dont need it 
End Sub

Sub TriggerScript(pTimeMS, pScript) ' This is used to Trigger script after the pTriggerScript Timer
    for fx=0 to TriggerScriptSize  
        if pReset(fx)=0 Then
            pReset(fx)=pTimeMS
            pStatement(fx)=pScript
            if pTriggerScript.enabled=false Then pTriggerScript.enabled=true
            Exit Sub
        End If 
    next
end Sub

'--------------------------------
'--- BONUS SEQUENCE
'--------------------------------
Sub resetbackglassOFFScore
	PuPlayer.LabelSet pDMD,"BonusMultip", FormatNumber(BonusMultiplier,0),0,""
	PuPlayer.LabelSet pDMD,"Bonus1", FormatNumber(ExperienceCount,0),0,""
	PuPlayer.LabelSet pDMD,"Bonus2", FormatNumber(RessourceCount,0),0,""
	PuPlayer.LabelSet pDMD,"Bonus3", FormatNumber(PersoSwitchCount,0),0,""
	PuPlayer.LabelSet pDMD,"Bonus4", FormatNumber(MissionCount,0),0,""
	PuPlayer.LabelSet pDMD,"BonusTotal", FormatNumber(TotalBonus,0),0,""
	PuPEvent 821
End Sub

Function BonusScoreTotal()
	BonusScoreTotal = BonusMultiplier * ((ExperienceCount * 50) + (RessourceCount * 2000) + (PersoSwitchCount * 15000) + (MissionCount * 35000) )
End Function 
Sub SeqBonus
	ResetMessage123L
	StopMi1
	StopMi2
	StopMi3
	StopMi4
	StopMi5
	vpmtimer.addtimer 200, "SeqBonus1 '"
	vpmtimer.addtimer 650, "SeqBonus2 '"
	vpmtimer.addtimer 1120, "SeqBonus3 '"
	vpmtimer.addtimer 1570, "SeqBonus4 '"
	vpmtimer.addtimer 2120, "SeqBonus5 '"
End Sub
Sub SeqBonus1
	PuPlayer.LabelSet pDMD,"BonusMultip","BONUS x " & FormatNumber(BonusMultiplier,0),1,""
	PuPlayer.LabelSet pDMD,"Bonus1", "Experience = " & FormatNumber((ExperienceCount * 50),0),1,""
	pDMDLabelPulseNumber "BonusTotal", " " & FormatNumber(BonusScoreTotal,0), 1600, 1992703, 0, BonusScoreTotal, 1
'	PlaySound "Bat"
End Sub
Sub SeqBonus2
	PuPlayer.LabelSet pDMD,"Bonus2", "Resources = " & FormatNumber((RessourceCount * 2000),0),1,""
End Sub
Sub SeqBonus3
	PuPlayer.LabelSet pDMD,"Bonus3", "Resonators = " & FormatNumber((PersoSwitchCount * 15000),0),1,""
End Sub
Sub SeqBonus4
	PuPlayer.LabelSet pDMD,"Bonus4", "Missions = " & FormatNumber((MissionCount * 35000),0),1,""
End Sub
Sub SeqBonus5
'	PuPlayer.LabelSet pDMD,"BonusTotal", "TOTAL BONUS = " & FormatNumber(BonusScoreTotal,0),1,""
'	PlaySound "Bat2"
	PlayerScore(CurrentPlayer) = PlayerScore(CurrentPlayer) + BonusScoreTotal
End Sub
'***********
'*** Anim Bumper Thunder
'***********
Sub ShowDoubleThunder()
    Dim ZoneA, ZoneB, ImgA, ImgB
    
    ' 1. Choisir quelles zones on va allumer (0=L+R, 1=L+C, 2=R+C)
    ZoneA = Int(Rnd * 3)
    
    Select Case ZoneA
        Case 0 ' Gauche + Droite
            ImgA = Int(Rnd * 2) + 4 ' Choisit entre 004 et 005
            ImgB = Int(Rnd * 3) + 1 ' Choisit entre 001 et 003
        Case 1 ' Gauche + Centre
            ImgA = Int(Rnd * 2) + 4 ' Choisit entre 004 et 005
            ImgB = Int(Rnd * 3) + 6 ' Choisit entre 006 et 008
        Case 2 ' Droite + Centre
            ImgA = Int(Rnd * 3) + 1 ' Choisit entre 001 et 003
            ImgB = Int(Rnd * 3) + 6 ' Choisit entre 006 et 008
    End Select

    ' 2. Lancement des deux éclairs (Formatage du nom avec les 00)
    ' On utilise une fonction de fondu ou d'affichage simple
    ShowThunder "Thunder00" & ImgA
    ShowThunder "Thunder00" & ImgB
End Sub

Sub ShowThunder(LabelName)
    ' Ici on utilise votre fonction favorite (ex: Fade ou Once)
    ' On affiche pendant 300ms puis on cache (isVis=0)
	pDMDLabelStrobeImage LabelName, 300, 0
	PlaySoundThunder
End Sub