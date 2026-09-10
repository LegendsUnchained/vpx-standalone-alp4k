'     __________________  __   __  ____  ___   __________________ 
'    / ____/  _/_  __/\ \/ /  / / / / / / / | / /_  __/ ____/ __ \
'   / /    / /  / /    \  /  / /_/ / / / /  |/ / / / / __/ / /_/ /
'  / /____/ /  / /     / /  / __  / /_/ / /|  / / / / /___/ _, _/ 
'  \____/___/ /_/     /_/  /_/ /_/\____/_/ |_/ /_/ /_____/_/ |_|by Tombg
'                                                                 
' ****************************************************************
'                       VISUAL PINBALL X
' ****************************************************************
'***********************************
' // User Settings in F12 menu //   
'***********************************
Dim ModeChangeBallActive
Dim ModeDifficulty
Dim RailChoice: RailChoice = Cabinet

'----- VR Room -----
Dim VRRoomChoice : VRRoomChoice = 2			  ' 1 - Cab Only, 2 - Minimal Room, 3 - MEGA room
Dim VRTest : VRTest = False

Sub Table1_OptionEvent(ByVal eventId)
	' -- pause processes not needed to run --
    If eventId = 1 Then DisableStaticPreRendering = True
	DMDTimer.Enabled = False ' stop FlexDMD timer
	Controller.Pause = True

	ModeChangeBallActive = Table1.Option("ChangeColorBall", 0, 1, 1, 0, 0, Array("Yes","No"))
	ModeDifficulty = Table1.Option("Mode Easy", 0, 1, 1, 0, 0, Array("Yes","No"))
	SetDifficulty ModeDifficulty

	' -- start paused processes --
	DMDTimer.Enabled = True
	Controller.Pause = False
    If eventId = 3 Then DisableStaticPreRendering = False

    ' VRRoom
	VRRoomChoice = Table1.Option("VR Room", 1, 3, 1, 3, 0, Array("CabOnly", "Minimal", "MEGA"))
	LoadVRRoom


    RailChoice = Table1.Option("Rails Visible", 0, 1, 1, 1, 0, Array("Cabinet", "SideRails (Default)"))
	SetRails RailChoice
    If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
	End Sub

	
Sub SetRails(Opt)
	Select Case Opt
		Case 0:
			Ramp15.Visible = 0
			Ramp16.Visible = 0
			PinCab_Blades.visible = 1
		Case 1:
			Ramp15.Visible = 1
			Ramp16.Visible = 1
			PinCab_Blades.visible = 0
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
'**************************
'DOF 
'**************************
'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 Right Slingshot
'105 
'106 
'107 Bumper Right
'108 Bumper Center
'109 Bumper Left
'110 FX Left Slingshot
'111 FX Right Slingshot
'112 Kicker001_Timer
'113 Kicker002_Timer
'114 Kicker003_Timer
'115 Left Flipper - Upper
'116 Right Flipper - Upper
'117 AutoPlunger
'118 Beacon when MiniCooper is running
'119 Shaker when MiniCooper is running
'120 
'121 
'122 Knocker
'123 Ball Release
'124 
'125 
'138 FX Bumper1
'139 FX Bumper2
'140 FX Bumper3
'144 
'145 
'146 
'147 
'>300 FX

'**************************
'   PinUp Player USER Config
'**************************
    dim usePuPDMD       : usePuPDMD=True       ' set to false to not use PuPDMD for a DMD (different that BG scoring)
	Const PuPDMDDriverType = 2  ' 2=FULLDMD (large/16:9 LCD). This table only supports the 16:9 fullsize option
	Const useRealDMDScale = 1    ' 0 or 1 for RealDMD scaling.  Choose which one you prefer.
	dim useDMDVideos    : useDMDVideos=true   ' true or false to use DMD splash videos.
	Dim pGameName       : pGameName="City_Hunter"    'pupvideos foldername, probably set to cGameName in realworld


	dim dmdver : dmdver = PuPDMDDriverType
	if dmdver = 2 Then
	Else
		dmdver = 1 
	end if

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
Const DynamicBallShadowsOn = 0		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow

'----- Others Options -----
Const UltraDMDUpdateTime	= 5000	'UltraDMD update time (msec).  Increase value if you encounter stutter with UltraDMD on
Const UseUltraDMD = 1				'0 = Don't use UltraDMD, 1=Use it
Const UseBGB2S = 0					'0 = Don't use B2S, 1=Use it (0 = Displaying Multiplayer Scores on the PUPDMD)

' Define any Constants
Const cGameName = "City_Hunter"
Const TableName = "City_Hunter"
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

Const Special1 = 50000000  ' High score to obtain an extra ball/game
Const Special2 = 100000000
Const Special3 = 200000000

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

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim MBLock_Sav(4)
Dim	RampCount
Dim MikiCount
Dim MissionCount
Dim RampCATSCount(4)
Dim MiCatsCount
Dim GreenCount(4)
Dim ExtraBCount
Dim SlingCount(4)
Dim BonusGreenCount(4)
Dim YellCount(4)
Dim MIGreenActive(4)
Dim MIYellActive(4)
Dim MBLockActive(4)
Dim KickBackActive(4)
Dim ModeMiniCooperActive
Dim ModeDroneActive
Dim ModeSaekoActive
Dim SkillShotState
Dim SaekoCount
Dim BallNikCount(4)
Dim MysteryState(4)
Dim bMusicOn
Dim bJustStarted
Dim bJackpot
Dim plungerIM
Dim LastSwitchHit

'****************************************
'		USER OPTIONS
'****************************************

Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 1   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.8 		' Level of ramp rolling volume. Value between 0 and 1
Dim StagedFlippers : StagedFlippers = 0         ' Staged Flippers. 0 = Disabled, 1 = Enabled

Dim tablewidth
tablewidth = Table1.width
Dim tableheight
tableheight = Table1.height
'****************************************
' core.vbs variables
Dim cbRight

Sub startB2S(aB2S)
    If B2SOn Then
        Controller.B2SSetData 1, 0
        Controller.B2SSetData 2, 0
        Controller.B2SSetData 3, 0
        Controller.B2SSetData 4, 0
        Controller.B2SSetData 5, 0
        Controller.B2SSetData 6, 0
        Controller.B2SSetData 7, 0
        Controller.B2SSetData 8, 0
        Controller.B2SSetData aB2S, 1
    End If
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
	If usePuPDMD = true Then PUPINIT
	Dim i
	'Randomize

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

	Set cbRight = New cvpmCaptiveBall
	With cbRight
    .InitCaptive CapTrigger1, CapWall1, Array(CapKicker1, CapKicker1a), 0
    .NailedBalls = 1
    .ForceTrans = .4
    .MinForce = 3.5
    .CreateEvents "cbRight"
    .Start
	End With
	CapKicker1.CreateBall

	' FlexDMD
'	Flex_Init
'	ShowScene flexScenes(1), FlexDMD_RenderMode_DMD_RGB, 2
	If UseUltraDMD > 0 Then LoadUltraDMD

    ' freeplay or coins
    bFreePlay = False 'we want coins

    'if bFreePlay = false Then DOF 125, DOFOn

	' Turn off the bumper lights
'	FlBumperFadeTarget(1) = 0
'	FlBumperFadeTarget(2) = 0
'	FlBumperFadeTarget(3) = 0

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bGameInPlay = False
    bMusicOn = True
    BallsOnPlayfield = 0
	bMultiBallMode = False
	'Multiball=false
	bAutoPlunger = False
 '   BallsInLock = 0
 '   BallsInHole = 0
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

'Sub PlaySoundAt(soundname, tableobj)
'	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
'End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Sub PlaySoundAtBall(soundname)
'	PlaySoundAt soundname, ActiveBall
'End Sub

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

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
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

Sub LeftFlipper001_Animate
	dim a: a = LeftFlipper001.CurrentAngle
	FlipperULSh.RotZ = a
	LFLogo001.RotZ = a
	'Add any left flipper related animations here
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
	FlipperULSh.RotZ = LeftFlipper001.CurrentAngle
	FlipperURSh.RotZ = RightFlipper001.CurrentAngle
	LFLogo.RotZ = LeftFlipper.CurrentAngle
	LFLogo001.RotZ = LeftFlipper001.CurrentAngle
	RFlogo.RotZ = RightFlipper.CurrentAngle
	RFlogo001.RotZ = RightFlipper001.CurrentAngle
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If keycode = RightMagnaSave Then 'halplay
        halgame                       'halplay
        timer067.enabled=1            'halplay
    End If   
	If keycode = LeftMagnaSave And halplay=1 Then halgameOFF 'halplay


    If Keycode = AddCreditKey Then
'		DOF 202, DOFPulse
        Credits = Credits + 1
		PlaySound "Coin_In_1"
		pDMDStartGame
		If B2SOn Then Controller.B2SSetscore 5, Credits
        if bFreePlay = False Then
'            DOF 125, DOFOn
            If(Tilted = False) Then
            End If
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
        PlaySoundAt "fx_reload", plunger
		pDMDStartGame
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
'		If ModeDroneActive = True And BallNikCount(CurrentPlayer) >= 1 Then NICKYDroneRotTimer.enabled=True
'	End If
	
	If keycode = LeftFlipperKey Then
		SolLFlipper True	'This would be called by the solenoid callbacks if using a ROM
		SolULFlipper True
		BumperLactive
	End If
	
	If keycode = RightFlipperKey Then
		SolRFlipper True	'This would be called by the solenoid callbacks if using a ROM
		SolURFlipper True
		BumperRactive
'		if PuPGameRunning Then PuPGameInfo= PuPlayer.GameUpdate("PupMiniGame", 1 , 87 , "")  'w
	End If

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
						If B2SOn Then Controller.B2SSetscore 5, Credits
						PlaySound "Fx_Bonus1"
						resetbackglass
                        Credits = Credits - 1
						If B2SOn Then Controller.B2SSetscore 5, Credits
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
						UpdateMusicNow
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
							UpdateMusicNow
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
        PlaySoundAt "fx_plunger", plunger
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
			SolULFlipper False
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
    Savehs
    If B2SOn Then Controller.Stop
	If UseUltraDMD > 0 Then UltraDMD.CancelRendering
End Sub

'*******************************************
'	ZFLP: Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled) 'Left flipper solenoid callback
	If Enabled Then
		PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
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
		PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
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
		PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
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
		PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
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
		PlaySoundAt SoundFXDOF("fx_flipperup", 116, DOFOn, DOFFlippers), RightFlipper
		RightFlipper001.rotatetoend

		If RightFlipper001.currentangle > RightFlipper001.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper001
		Else 
			SoundFlipperUpAttackRight RightFlipper001
			RandomSoundFlipperUpRight RightFlipper001
		End If
	Else
		PlaySoundAt SoundFXDOF("fx_flipperdown", 116, DOFOff, DOFFlippers), RightFlipper
		RightFlipper001.RotateToStart
		If RightFlipper001.currentangle > RightFlipper001.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper001
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolULFlipper(Enabled)
	If Enabled Then
		PlaySoundAt SoundFXDOF("fx_flipperup", 115, DOFOn, DOFFlippers), LeftFlipper
		LeftFlipper001.rotatetoend

		If LeftFlipper001.currentangle > LeftFlipper001.endangle - ReflipAngle Then
			RandomSoundReflipUpRight LeftFlipper001
		Else 
			SoundFlipperUpAttackRight LeftFlipper001
			RandomSoundFlipperUpRight LeftFlipper001
		End If
	Else
		PlaySoundAt SoundFXDOF("fx_flipperdown", 115, DOFOff, DOFFlippers), LeftFlipper
		LeftFlipper001.RotateToStart
		If LeftFlipper001.currentangle > LeftFlipper001.startAngle + 5 Then
			RandomSoundFlipperDownRight LeftFlipper001
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
	ShakeBullets
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
		pupDMDDisplay "Splash4","!CAREFUL!","",3,1,10
		if UseUltraDMD = 1 then UDMD "!CAREFUL!", "", 3000
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        'DMDFlush
		DOF 400, DOFPulse
        pupDMDDisplay "Splash4","TILT","",3,0,20
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

Dim Song, UpdateMusic
Song = ""

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            PlaySound Song, -1, SongVolume
        End If
    End If
End Sub

Sub StopSong
    If bMusicOn Then
        StopSound Song
        Song = ""
    End If
End Sub

Sub ChangeSong
    If(BallsOnPlayfield = 0)Then
        PlaySong ""'"Mu_end"
        Exit Sub
    End If

    If bAttractMode Then
        PlaySong ""'"Mu_end"
        Exit Sub
    End If
    If bMultiBallMode Then
        PlaySong "" '"Mu_MB"
    Else
        UpdateMusicNow
    end if
End Sub

'if you add more balls to the game use changesong then if bMultiBallMode = true, your multiball song will be played.

Sub UpdateMusicNow
    Select Case UpdateMusic
        Case 0:PlaySong "Mu_1"
        Case 1:PlaySong "Mu_2"
        Case 2:PlaySong "Mu_3"
		Case 3:PlaySong "Mu_1"
        Case 4:PlaySong "Mu_2"
        Case 5:PlaySong "Mu_3"
		Case 6:PlaySong "Mu_1"
        Case 7:PlaySong "Mu_2"
        Case 8:PlaySong "Mu_3"
		Case 9:PlaySong "Mu_1"
        Case 10:PlaySong "Mu_2"
        Case 11:PlaySong "Mu_3"
    End Select
end sub

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
			n.color = RGB(34, 100, 255)
			n.colorfull = RGB(34, 100, 255)
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
	If B2SOn Then Controller.B2SSetData 1,1
End Sub
Sub B2SGIOff
	If B2SOn Then Controller.B2SSetData 1,0
End Sub

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
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
    DOF 127, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aBumperLights
        bulb.State = 1
    Next
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to2"
End Sub

Sub GiOff
    DOF 127, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aBumperLights
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

Sub LightCityON
	CityLightENDTimer.Enabled = True
	LiKIT001.state = 2
	LiKIT002.state = 2
	LiKIT003.state = 2
	LiKIT004.state = 2
	LiKIT005.state = 2
	LiKIT006.state = 2
	LiKIT007.state = 2
	LiKIT008.state = 2
	LiKIT009.state = 2
	LiKIT010.state = 2
End Sub
Sub CityLightENDTimer_Timer()
	LiKIT001.state = 0
	LiKIT002.state = 0
	LiKIT003.state = 0
	LiKIT004.state = 0
	LiKIT005.state = 0
	LiKIT006.state = 0
	LiKIT007.state = 0
	LiKIT008.state = 0
	LiKIT009.state = 0
	LiKIT010.state = 0
	CityLightENDTimer.Enabled = False
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
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.06, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function
'********************************************
'   JP's VP10 Rolling Sounds
'********************************************

Const tnob = 11 ' total number of balls
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

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)

        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 10
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
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************
Sub OnBallBallCollision(ball1, ball2, velocity)
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
    bOnTheFirstBall = True
	'Multiball=false	
    For i = 1 To MaxPlayers
        PlayerScore(i) = 0
        BonusPoints(i) = 0
		'BonusHeldPoints(i) = 0
  '      BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
		BallinGame(i) = 1
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
    Next
	If B2SOn Then Controller.B2SSetScorePlayer 1, 0

    ' initialise any other flags
    Tilt = 0

	'reset variables
	bumperHits = 100
    UpdateMusic = 0
    'UpdateMusic = UpdateMusic + 6
    UpdateMusicNow

    ' initialise Game variables
    Game_Init()
	
    ' you may wish to start some music, play a sound, do whatever at this point
StopSong


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
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
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
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

	'LightSeqAttract.Play SeqBlinking, , 5, 150

StopSong

'bonuscheckie

    Dim AwardPoints, TotalBonus, ii
	AwardPoints = 0
    TotalBonus = 0

    'If NOT Tilted Then
	If(Tilted = False) Then
		PuPEvent 811
		PuPEvent 812
		PuPEvent 813
		PuPEvent 814
		PuPEvent 815
		PuPEvent 816
		PuPEvent 820
        'Bonus
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
		vpmtimer.addtimer 6000, "resetbackglassOFFScore '"
		vpmtimer.addtimer 6000, "EndOfBall2 '"
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
    UpdateMusic = UpdateMusic + 1
	UpdateMusicNow	
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
           LiExtra.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        'DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, "vo_extraball"

		UpdateMusic = UpdateMusic - 1
		UpdateMusicNow

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
		StopSong
		'DMD CL(0, "GAME OVER") "", eNone, 13000, True, ""
		'DMD "", CL(1, "GAME OVER"), "", eNone, eNone, eNone, 13000, False, ""
'		PuPEvent 818
        ' set the machine into game over mode
        vpmtimer.addtimer 4000, "EndOfGame() '"
		vpmtimer.addtimer 6800, "pDMDGameOver '"
		
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
        ChangeSong
    End If

    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0
	SolURFlipper 0
	SolULFlipper 0
	

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
    PlaySoundAt "fx_drain", Drain
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
	pupDMDDisplay "Splash4","Ball^SAVED","",3,1,10
	if UseUltraDMD = 1 then UDMD "BALL SAVED", "", 3000
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
					StopSong
					If MIYellActive(CurrentPlayer) = True And MiCatsStartTimer.Enabled = False Then PlaySong "Mu_StartMini"
					If MiCatsStartTimer.Enabled = True Then PlaySong "Mu_CatsStart"
					If MIYellActive(CurrentPlayer) = False And MiCatsStartTimer.Enabled = False Then UpdateMusicNow
'					ChangeSong
                End If
            End If
            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
                   if halplay=1 Then 'halplay
                   kicker025.enabled=1'halplay
                   end If'halplay
                ' End Mode and timers
				StopSong
				PlaySoundEND
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
        DOF 117, DOFPulse
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
	CheckSkillShot
	If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then 
        EnableBallSaver BallSaverTime
'		ResetSkillShotTimer.Enabled = True
        Else
        ' show the message to shoot the ball in case the player has fallen sleep
        Trigger1.TimerEnabled = 1
    End If
'	If MBLockActive(CurrentPlayer) = False Then pupDMDDisplay "Splash4","Hit Ramp Multiball^Unlock KAORI","",4,0,9
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
	DOF 121, DOFPulse
	DOF 451, DOFPulse
	PuPEvent 835
	LiExtra.state = 1 
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
    ChangeSong
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
	PlaySoundAt SoundFXDOF("Sling_L", 104, DOFPulse, DOFcontactors), Sling1
    DOF 111, DOFPulse
	RS.VelocityCorrect(ActiveBall)
	Addscore 510
	SlingCount(CurrentPlayer) = SlingCount(CurrentPlayer) + 1
	CheckLightSlingCount
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
	PlaySoundAt SoundFXDOF("Sling_R", 103, DOFPulse, DOFcontactors), Sling2
    DOF 110, DOFPulse
	LS.VelocityCorrect(ActiveBall)
	Addscore 510
	SlingCount(CurrentPlayer) = SlingCount(CurrentPlayer) + 1
	CheckLightSlingCount
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
	PlaySoundAt SoundFXDOF("Sling_R", 103, DOFPulse, DOFcontactors), Sling2
	DOF 110, DOFPulse
	If ModeDroneActive = True And BallNikCount(CurrentPlayer) = 0 Then EndModeDrone
	SlingCount(CurrentPlayer) = SlingCount(CurrentPlayer) + 1
	CheckLightSlingCount
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

FlInitBumper 1, "red"
FlInitBumper 2, "red"
FlInitBumper 3, "red"

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
        PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper1
        DOF 138, DOFPulse
		GiEffect 4
		AddScore 915
		FlBumperFadeTarget(1) = 0
		bumper1.timerinterval = 100
		Bumper1.timerenabled = True
	End If
End Sub

Sub Bumper2_Hit()
'	CheckBumpers
	If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper2
        DOF 139, DOFPulse
		GiEffect 4
		AddScore 915
		FlBumperFadeTarget(2) = 0
		bumper2.timerinterval = 100
		Bumper2.timerenabled = True
	End If
End Sub

Sub Bumper3_Hit()
'	CheckBumpers
	If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), Bumper3
        DOF 140, DOFPulse
		GiEffect 4
		AddScore 915
		FlBumperFadeTarget(3) = 0
		bumper3.timerinterval = 100
		Bumper3.timerenabled = True
	End If
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

sub kicker001_hit()
	StartMiniCooper
	CheckColorMISS
    DOF 118, DOFOn
    DOF 119, DOFOn
	FlashForMs F1A006, 500, 250, 0
	FlashForMs F1A007, 500, 250, 0
	Kicker001.timerinterval = 3000
	kicker001.timerenabled = True
'	If LiStartDrone.state = 2 Then AwardExtraBall : LiStartDrone.state = 0
end Sub
Sub Kicker001_Timer()
	pupDMDDisplay "Splash4","Move Mini Cooper^< Use Flippers >","",4,0,9
	if UseUltraDMD = 1 then UDMD "Move MiniCooper", "Use Flippers", 3000
	kicker001.kick -125, 45, 1.4
	kicker001.timerenabled = False
    DOF 112, DOFPulse
End Sub

sub kicker002_hit()
	CheckMystery
'	If MIRedActive(CurrentPlayer) = True Then MBLockActive(CurrentPlayer) = True : pupDMDDisplay "Splash4","Lock is Lit^Enter Truck","",3,1,10
	Kicker002.timerinterval = 2000
	kicker002.timerenabled = True
	If ModeDroneActive = False And BallNikCount(CurrentPlayer) <= 4 Then
	BallNikCount(CurrentPlayer) = BallNikCount(CurrentPlayer) + 1
	FlashForMs Flasher009, 2000, 250, 0
		If MysteryState(CurrentPlayer) = False Then PlaySoundDrama
	CheckBallNik
	End If 
	If ModeDroneActive = True And BallNikCount(CurrentPlayer) >= 1 Then NICKYDroneRotTimer.enabled=True
	If LiStartDrone.state = 2 And ModeDroneActive = False Then StartModeDrone
end Sub

Sub CheckMystery
If MysteryState(CurrentPlayer) = True And LiMyst001.State = 2 Then 
	ResetMystery
		Dim ModeChosenMystery
		ModeChosenMystery = False
		Do While ModeChosenMystery = False
			Select Case RndNbr(10)
			Case 1
				If KickBackActive(CurrentPlayer) = False Then 'OK Kick Back active
					LiTargetM001.State = 1
					KickBackActive(CurrentPlayer) = True
					pupDMDDisplay "Splash4","Mystery Award^KickBack Active","",4,1,11
					if UseUltraDMD = 1 then UDMD "Mystery Award", "KickBack Active", 3000
					Playsound ""
					ModeChosenMystery = True
				End If
			Case 2
				If BallNikCount(CurrentPlayer) <= 5 And ModeDroneActive = False Then 'OK 1 ball de gun ajouté
					BallNikCount(CurrentPlayer) = BallNikCount(CurrentPlayer) + 1
					FlashForMs Flasher009, 2000, 250, 0
					CheckBallNik
					PlaySoundDrama
					pupDMDDisplay "Splash4","Mystery Award^1 Bullet Add","",4,1,11
					if UseUltraDMD = 1 then UDMD "Mystery Award", "1 Bullet Add", 3000
					ModeChosenMystery = True
				End If
			Case 3
				If PlayfieldMultiplier <=3 Then 'OK playfield x +1
					PlayfieldMultiplier = PlayfieldMultiplier + 1
					Playsound "Fx_UpBonus"
					DOF 338, DOFPulse
					pupDMDDisplay "Splash4","Mystery Award^Playfield X","",4,1,11
					if UseUltraDMD = 1 then UDMD "Mystery Award", "Playfield X", 3000
					ResetLightLane
					CheckLightBonus
					ModeChosenMystery = True
				End If
			Case 4
				If BonusMultiplier <=3 Then 'OK bonus x +1
					BonusMultiplier = BonusMultiplier + 1
					Playsound "Fx_UpBonus"
					DOF 339, DOFPulse
					pupDMDDisplay "Splash4","Mystery Award^Bonus X","",4,1,11
					if UseUltraDMD = 1 then UDMD "Mystery Award", "Bonus X", 3000
					LiBonus001.state = 0
					LiBonus002.state = 0
					LiBonus003.state = 0
					CheckLightBonus
					ModeChosenMystery = True
				End If
			Case 5
				If BallNikCount(CurrentPlayer) <= 5 And ModeDroneActive = False Then 'Ok Start Mode Drone
					BallNikCount(CurrentPlayer) = 6
					CheckBallNik
					StartModeDrone
					pupDMDDisplay "Splash4","Mystery Award^Drone Attack","",4,1,11
					if UseUltraDMD = 1 then UDMD "Mystery Award", "Drone Attack", 3000
					ModeChosenMystery = True
				End If
			Case 6
				If LightShootAgain.State = 0 Then 'Ok BallSaver 15s
					EnableBallSaver 15
					pupDMDDisplay "Splash4","Mystery Award^Ball Saver Actived","",4,1,11
					if UseUltraDMD = 1 then UDMD "Mystery Award", "BallSaver 15s", 3000
					ModeChosenMystery = True
				End If
			Case 7
				If bMultiBallMode = False Then 'Ok Add 1 Ball
					AddMB2
					pupDMDDisplay "Splash4","Mystery Award^ADD 1 Ball","",4,1,11
					if UseUltraDMD = 1 then UDMD "Mystery Award", "ADD Ball", 3000
					ModeChosenMystery = True
				End If
			Case 8
				If MBLock_Sav(CurrentPlayer) < 4 Then 'A VERIFIER +1 ball Lock
					If MBLock_Sav(CurrentPlayer) <= 1 Then 
						pupDMDDisplay "Splash4","Mystery Award^Ball 1 Lock","",4,1,11
						if UseUltraDMD = 1 then UDMD "Mystery Award", "Ball 1 Lock", 3000
						Playsound ""
						DOF 452, DOFPulse
						PuPEvent 830
						MBLock_Sav(CurrentPlayer) = 2
						LiLOCK001.state=1
						LiLOCK001b.state=1
						CheckKamiMassState
						ModeChosenMystery = True
					Elseif MBLock_Sav(CurrentPlayer) >= 2 Then
						pupDMDDisplay "Splash4","Mystery Award^Ball 2 Lock","",4,1,11
						if UseUltraDMD = 1 then UDMD "Mystery Award", "Ball 2 Lock", 3000
						Playsound ""
						DOF 453, DOFPulse
						MBLock_Sav(CurrentPlayer) = 4
						LiLOCK002.state=1
						LiLOCK002b.state=1
						CheckKamiMassState
						ModeChosenMystery = True
					End If
				End If	
			Case 9
				If  ExtraBallsAwards(CurrentPlayer) = 0 And LiEXTRA.state = 0 And ExtraBCount >=2 Then 'Ok EXTRABALL READY
					ExtraBCount=5 
					LiEXTRA.state=2
					Playsound "Vo_Nick004"
					pupDMDDisplay "Splash4","Mystery Award^ExtraBall LIT","",4,1,11
					if UseUltraDMD = 1 then UDMD "Mystery Award", "Extraball LIT", 3000
					ModeChosenMystery = True
				End If
			Case 10
				If bMultiBallMode = False And BallsOnPlayfield = 1 Then 'Ok MULTIBALL
					AddMB2
					vpmtimer.addtimer 3000, "AddMB2 '"
					pupDMDDisplay "Splash4","Mystery Award^MULTIBALL","",4,1,11
					if UseUltraDMD = 1 then UDMD "Mystery Award", "MULTIBALL", 3000
					ModeChosenMystery = True
				End If
			End Select
		Loop
	Else
	End If
End Sub

Sub Kicker002_Timer()
	kicker002.kick 158, 18
'	PlaySound "KRfxScoop"
	kicker002.timerenabled = False
    DOF 113, DOFPulse
End Sub

Sub kicker003_Hit()
	If MBLock_Sav(CurrentPlayer) >= 1 Then
		MBLock_Sav(CurrentPlayer) = MBLock_Sav(CurrentPlayer) + 1
			If MBLock_Sav(CurrentPlayer) = 2 Then ActiveKimiMass : AddMB1 : PlaySoundLock : PuPEvent 830 : pupDMDDisplay "Splash4","Ball 1^LOCKED","",4,0,11 : ShakeBullets : DOF 452, DOFPulse
			If MBLock_Sav(CurrentPlayer) = 3 Then FlashNikTimer.Enabled = 1 : PlaySoundVoNick : Kicker003.timerinterval = 2000 : kicker003.timerenabled = True
			If MBLock_Sav(CurrentPlayer) = 4 Then ActiveKimiMass : AddMB1 : PlaySoundLock : PuPEvent 830 : pupDMDDisplay "Splash4","Ball 2^LOCKED","",4,0,11 : ShakeBullets : DOF 453, DOFPulse
			If MBLock_Sav(CurrentPlayer) = 5 Then FlashNikTimer.Enabled = 1 : PlaySoundVoNick : Kicker003.timerinterval = 2000 : kicker003.timerenabled = True 
			If MBLock_Sav(CurrentPlayer) = 6 Then ActiveKimiMass : ShakeBullets : PlaySoundLock : Mode_Multiball_Start 
		CheckLightMB
	Else
		Kicker003.timerinterval = 2000
		kicker003.timerenabled = True
	End If
end Sub

Sub DestroyBallK3Timer_Timer()
'	DOF 118, DOFOff
'   DOF 119, DOFOff
	Kicker003.DestroyBall
	kicker003.timerenabled = False
	KickerAuto.CreateSizedBallWithMass BallSize / 2, BallMass
	KickerAuto.Kick 10, 1
	DOF 132, DOFPulse
	DestroyBallK3Timer.Enabled = False
End Sub

Sub AddMB1
	DOF 118, DOFOn
    DOF 119, DOFOn
	ShakerEndTimer.Enabled = True 'Debug DOF Shaker
	StartDropAnimation ActiveBall
	DestroyBallK3Timer.Enabled = True
'   BallsOnPlayfield = 1
	LightEffect 4
'	KickerAuto.CreateSizedBallWithMass BallSize / 2, BallMass
'	KickerAuto.Kick 10, 1
    PlaySoundAt SoundFX("fx_Ballrel", DOFContactors), BallRelease
	bAutoPlunger = True
'	EnableBallSaver 15
	If MBLock_Sav(CurrentPlayer) <= 5 Then vpmtimer.addtimer 6000, "ActiveKimiMass '"
End Sub

Sub AddMB2
	LightEffect 4
	KickerAuto.CreateSizedBallWithMass BallSize / 2, BallMass
	KickerAuto.Kick 10, 1
	DOF 132, DOFPulse
    BallsOnPlayfield = BallsOnPlayfield + 1 
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
	bAutoPlunger = True
End Sub

Sub Kicker003_Timer()
	kicker003.kick 196, 18
'	PlaySound "KRfxScoop"
	kicker003.timerenabled = False
    DOF 114, DOFPulse
End Sub

Sub Kicker004_Hit() 'kickBackLeft
	PuPEvent 838
	MikiCount = MikiCount + 1
	kicker004.kick 0, 32
	Playsound "explosion01":    DOF 114, DOFPulse
	pupDMDDisplay "Splash4","SHIELD^USED","",3,0,9
	if UseUltraDMD = 1 then UDMD "SHIELD USED", "", 3000
	LiTargetM001.State = 0
	KickBackActive(CurrentPlayer) = False
	kicker004.Enabled = False 
	kicker005.Enabled = False 
End Sub

Sub Kicker005_Hit() 'kickBackRight
	PuPEvent 838
	MikiCount = MikiCount + 1
	kicker005.kick 0, 30
	Playsound "explosion01":    DOF 114, DOFPulse
	pupDMDDisplay "Splash4","SHIELD^USED","",3,0,9
	if UseUltraDMD = 1 then UDMD "SHIELD USED", "", 3000
	LiTargetM001.State = 0
	KickBackActive(CurrentPlayer) = False
	kicker004.Enabled = False 
	kicker005.Enabled = False 
End Sub
'***********
' Target
'***********

Sub TargetDrone001_Hit()
	AddScore 256
	PlaySoundTarget
	StartGun
End Sub

Sub Target001_Hit()
	AddScore 256
	If LiTargetR001.State = 0 Then
	LiTargetR001.State = 2
	PlaySoundTarget
	Elseif LiTargetR001.State = 2 Then
	LiTargetR001.State = 1
	PlaySoundTargetsMystery
	CheckLightMystery
	Elseif LiTargetR001.State = 1 Then
	PlaySoundTarget
	End If
End Sub
Sub Target002_Hit()
	AddScore 256
	If LiTargetR002.State = 0 Then
	LiTargetR002.State = 2
	PlaySoundTarget
	Elseif LiTargetR002.State = 2 Then
	LiTargetR002.State = 1
	PlaySoundTargetsMystery
	CheckLightMystery
	Elseif LiTargetR002.State = 1 Then
	PlaySoundTarget
	End If
End Sub
Sub Target003_Hit()
	AddScore 256
	If LiTargetR003.State = 0 Then
	LiTargetR003.State = 2
	PlaySoundTarget
	Elseif LiTargetR003.State = 2 Then
	LiTargetR003.State = 1
	PlaySoundTargetsMystery
	CheckLightMystery
	Elseif LiTargetR003.State = 1 Then
	PlaySoundTarget
	End If
End Sub
Sub CheckLightMystery
	If LiTargetR001.State = 1 And LiTargetR002.State = 1 And LiTargetR003.State = 1 Then
	GiEffect 2
	LiMyst001.State = 2
	MysteryState(CurrentPlayer) = True
	StartSaekoCombo
	End If
End Sub

Sub ResetMystery
	LiTargetR001.State = 2
	LiTargetR002.State = 2
	LiTargetR003.State = 2
	LiMyst001.State = 0
	MysteryState(CurrentPlayer) = False
End Sub

Sub Target004_Hit()
	AddScore 256
	PlaySoundTarget
	If LiTargetM001.State = 0 Then
	LiTargetM001.State = 2
	Elseif LiTargetM001.State = 2 Then
	LiTargetM001.State = 1
	KickBackActive(CurrentPlayer) = True
	GiEffect 2
	kicker004.Enabled = True 
	kicker005.Enabled = True 
	pupDMDDisplay "Splash4","Shield Outlanes^ACTIVED","",4,0,10
	if UseUltraDMD = 1 then UDMD "Shield Outlanes", "ACTIVED", 3000
	End If	
End Sub

Sub Target005_Hit() 'Green
	AddScore 2560
	PlaySound "fx_target"
	If MIGreenActive(CurrentPlayer) = False Then
	GreenCount(CurrentPlayer) = GreenCount(CurrentPlayer) + 1 
	CheckColorMISS
	End If
End Sub
Sub Target006_Hit() 'YELL
	AddScore 2560
	PlaySound "fx_target"
	If MIYellActive(CurrentPlayer) = False Then
	YellCount(CurrentPlayer) = YellCount(CurrentPlayer) + 1 
	CheckColorMISS
	End If
End Sub

Sub CheckColorMISS
	LiRed001.state = 2
	If GreenCount(CurrentPlayer) = 0 Then LiGreen002.state = 2 : LiGreen003.state = 2 : LiGreen004.state = 2
	If GreenCount(CurrentPlayer) = 1 Then LiGreen002.state = 1 : LiGreen003.state = 2 : LiGreen004.state = 2
	If GreenCount(CurrentPlayer) = 2 Then LiGreen002.state = 1 : LiGreen003.state = 1 : LiGreen004.state = 2
	If GreenCount(CurrentPlayer) = 3 And MIGreenActive(CurrentPlayer) = False Then LiGreen001.state = 0 : LiGreen002.state = 1 : LiGreen003.state = 1 : LiGreen004.state = 1 : StartMIGreen
	If GreenCount(CurrentPlayer) = 4 And MIGreenActive(CurrentPlayer) = True Then LiMi005.State = 2 : LiGreen001.state = 0 : LiGreen002.state = 1 : LiGreen003.state = 1 : LiGreen004.state = 1
	If YellCount(CurrentPlayer) = 0 Then LiYell002.state = 2 : LiYell003.state = 2 : LiYell004.state = 2
	If YellCount(CurrentPlayer) = 1 Then LiYell002.state = 1 : LiYell003.state = 2 : LiYell004.state = 2
	If YellCount(CurrentPlayer) = 2 Then LiYell002.state = 1 : LiYell003.state = 1 : LiYell004.state = 2
	If YellCount(CurrentPlayer) = 3 And MIYellActive(CurrentPlayer) = False Then LiYell001.state = 0 : LiYell002.state = 1 : LiYell003.state = 1 : LiYell004.state = 1 : StartMIYell
	If YellCount(CurrentPlayer) = 4 And MIYellActive(CurrentPlayer) = True Then LiMi003.State = 2 : LiYell001.state = 0 : LiYell002.state = 1 : LiYell003.state = 1 : LiYell004.state = 1
End Sub

Sub ResetLightPFhaut
	LiGreen001.state = 2
	LiGreen002.state = 2
	LiGreen003.state = 2
	LiGreen004.state = 2
	LiYell001.state = 2
	LiYell002.state = 2
	LiYell003.state = 2
	LiYell004.state = 2
	GreenCount(CurrentPlayer) = 0
	YellCount(CurrentPlayer) = 0
	MiYellEndTimer.Enabled = False
End Sub

Sub OFFLightPFhaut
	LiRed001.state = 2
'	LiGreen001.state = 0
	LiGreen002.state = 0
	LiGreen003.state = 0
	LiGreen004.state = 0
'	LiYell001.state = 0
	LiYell002.state = 0
	LiYell003.state = 0
	LiYell004.state = 0
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
'*****************
'Triggers
'*****************
Sub Trigger001_Hit()
	AddScore 155
End Sub

Sub Trigger002_Hit()
	FlashForMs F1A003, 1000, 50, 0
	LightEffect 6
	AddScore 1550
	RampCount = RampCount + 1	
	StopMiniCooper
	OFFLightPFhaut
	DOF 118, DOFOff
	DOF 119, DOFOff
	DOF 443, DOFPulse
End Sub

Sub Trigger003_Hit() 'RampMilieu R et L
'	FlashForMs F1A008, 500, 250, 0	
	RampCount = RampCount + 1
	If ModeDroneActive = False And BallNikCount(CurrentPlayer) < 6 Then
	BallNikCount(CurrentPlayer) = BallNikCount(CurrentPlayer) + 1
	DOF 480, DOFPulse
	FlashForMs Flasher009, 2000, 250, 0
	PlaySoundDrama
	CheckBallNik
	End If 
	If ActiveInLaneLeftTimer.Enabled = True Then 'Vient de gauche
		AddScore 955
		FlashForMs F1A007, 500, 250, 0
		If LiKARR003.State = 2 And MIYellActive(CurrentPlayer) = True Then LiKARR003.State = 1 : PlaySound "Fx_Bonus1" : AddScore2 20000 : CheckAwardMIYell
	End If
	If ActiveInLaneRightTimer.Enabled = True Then 'Vient de droite
		AddScore 955
		FlashForMs F1A006, 500, 250, 0
		If LiKARR004.State = 2 And MIYellActive(CurrentPlayer) = True Then LiKARR004.State = 1 : PlaySound "Fx_Bonus1" : AddScore2 20000 : CheckAwardMIYell
	End If
	ActiveInLaneRightTimer.Enabled = False
	ActiveInLaneLeftTimer.Enabled = False
End Sub

Sub Trigger004_Hit()
	AddScore 250
	If SkillShotState = 3 And SkillShotTimer.Enabled = True And LiBonus001.state = 2 Then WinSkillShot
	If LiBonus001.state = 0 Then
	LiBonus001.state = 2
	Elseif LiBonus001.state = 2 Then
	LiBonus001.state = 1
	CheckBonusX
	End If
End Sub
Sub Trigger005_Hit()
	AddScore 250
	If SkillShotState = 3 And SkillShotTimer.Enabled = True And LiBonus002.state = 2 Then WinSkillShot
	If LiBonus002.state = 0 Then
	LiBonus002.state = 2
	Elseif LiBonus002.state = 2 Then
	LiBonus002.state = 1
	CheckBonusX
	End If
End Sub
Sub Trigger006_Hit()
	AddScore 250
	If SkillShotState = 3 And SkillShotTimer.Enabled = True And LiBonus003.state = 2 Then WinSkillShot
	If LiBonus003.state = 0 Then
	LiBonus003.state = 2
	Elseif LiBonus003.state = 2 Then
	LiBonus003.state = 1
	CheckBonusX
	End If
End Sub
Sub CheckBonusX
	If LiBonus001.state = 1 And LiBonus002.state = 1 And LiBonus003.state = 1 Then 
	BonusMultiplier = BonusMultiplier + 1
	Playsound "Fx_UpBonus"
	LightEffect 7
	LiBonus001.state = 0
	LiBonus002.state = 0
	LiBonus003.state = 0
	CheckLightBonus
	Playsound ""
	DOF 339, DOFPulse
	pupDMDDisplay "Splash4","BONUS^MULTIPLIER","",4,0,10
	if UseUltraDMD = 1 then UDMD "BONUS X", "", 3000
	End If
End Sub

Sub Trigger007_Hit() 'Ramp KIMI
	AddScore 4055
	LightCityON
	RampCount = RampCount + 1
	DOF 440, DOFPulse
	If MBLockActive(CurrentPlayer) = False And MBLock_Sav(CurrentPlayer) = 0 Then 
		MBLock_Sav(CurrentPlayer) = 1 
		FlashNikTimer.Enabled = 1
		CheckLightMB 
		ActiveKimiMass 
		MBLockActive(CurrentPlayer) = True
		PlaySoundVoNick
	End If
	ExtraBCount = ExtraBCount + 1
	If MBLockActive(CurrentPlayer) = True Then
		if ExtraBCount=1 then LiEXTRA.state=0 ': pupDMDDisplay "Splash3", "5^More To^EXTRABALL|6538752", "", 5, 1, 6
		if ExtraBCount=2 then LiEXTRA.state=0 : PlaySoundVoMaori : pupDMDDisplay "Splash3", "4^More To^EXTRABALL|6538752", "", 5, 1, 7
		if ExtraBCount=3 then LiEXTRA.state=0 : PlaySoundVoMaori : pupDMDDisplay "Splash3", "3^More To^EXTRABALL|6538752", "", 5, 1, 8
		if ExtraBCount=4 then LiEXTRA.state=0 : PlaySoundVoMaori : pupDMDDisplay "Splash3", "2^More To^EXTRABALL|6538752", "", 5, 1, 9
		if ExtraBCount=5 then LiEXTRA.state=2 : Playsound "Vo_Nick004" : pupDMDDisplay "Splash3", "1^More To^EXTRABALL|6538752", "", 5, 1, 9
		if ExtraBCount>=7 then AddScore 8500 : PlaySoundVoMaori
		if ExtraBCount=6 then 
		LiEXTRA.state=1
		AwardExtraBall
	End If
	End If
End Sub

Sub Trigger008_Hit() 'RampMamouth
	RampCount = RampCount + 1
	DOF 441, DOFPulse
	If MIGreenActive(CurrentPlayer) = False Then
		AddScore 2500
		PlaySoundMamouth
	End If
	If ModeSaekoActive = True Then SaekoCount = SaekoCount + 1 : CheckSaekoCombo
	FlashForMs F1A002, 1000, 50, 0
	If Li10M.state = 2 or Li15M.state = 2 or Li20M.state = 2 or Li100M.state = 2 Then : PlaySound "Fx_Bonus1" : BonusGreenCount(CurrentPlayer) = BonusGreenCount(CurrentPlayer) + 1 : AwardMIGreen
End Sub

Sub Trigger009_Hit() 'RampCATS
	AddScore 4250
	RampCount = RampCount + 1
	DOF 444, DOFPulse
	FlashForMs F1A002, 1000, 50, 0
	If RampCATSCount(CurrentPlayer) <= 3 Then RampCATSCount(CurrentPlayer) = RampCATSCount(CurrentPlayer) + 1 : CheckLightCats
	If LiKARR001.State = 2 And MIYellActive(CurrentPlayer) = True Then LiKARR001.State = 1 : PlaySound "Fx_Bonus1" : AddScore2 20000 : CheckAwardMIYell
	If RampCATSCount(CurrentPlayer) = 1 Then Playsound "Vo_Cats1"
	If RampCATSCount(CurrentPlayer) = 2 Then Playsound "Vo_Cats2"
End Sub

Sub CheckLightCats
	If RampCATSCount(CurrentPlayer) = 0 Then LiMi002.State = 0 : LiCats001.State = 2 : LiCats002.State = 0 : LiCats003.State = 0
	If RampCATSCount(CurrentPlayer) = 1 Then LiMi002.State = 0 : LiCats001.State = 1 : LiCats002.State = 2 : LiCats003.State = 0 
	If RampCATSCount(CurrentPlayer) = 2 Then LiMi002.State = 0 : LiCats001.State = 1 : LiCats002.State = 1 : LiCats003.State = 2
	If RampCATSCount(CurrentPlayer) = 3 Then LiMi002.State = 0 : LiCats001.State = 1 : LiCats002.State = 1 : LiCats003.State = 1 : StartMICats
	If RampCATSCount(CurrentPlayer) = 4 Then LiMi002.State = 2 : LiCats001.State = 2 : LiCats002.State = 2 : LiCats003.State = 2 
End Sub

Sub Trigger010_Hit() 'ActiveLaneLeft
	ActiveLaneLeftTimer.Enabled = True
End Sub

Sub TriggerLaneLeft_Hit()
	FlashForMs F1A008, 500, 250, 0	
	If ActiveLaneLeftTimer.Enabled = True Then
		AddScore 955
		If LiKARR002.State = 2 And MIYellActive(CurrentPlayer) = True Then LiKARR002.State = 1 : PlaySound "Fx_Bonus1" : AddScore2 20000 : CheckAwardMIYell		
	End If
	ActiveLaneLeftTimer.Enabled = False
End Sub

Sub Trigger011_Hit() 'ActiveLaneRight
	ActiveLaneRightTimer.Enabled = True
End Sub

Sub TriggerLaneRight_Hit()
	FlashForMs F1A005, 500, 250, 0
	If ActiveLaneRightTimer.Enabled = True Then
		AddScore 955
		If LiKARR005.State = 2 And MIYellActive(CurrentPlayer) = True Then LiKARR005.State = 1 : PlaySound "Fx_Bonus1" : AddScore2 20000 : CheckAwardMIYell
	End If
	ActiveLaneRightTimer.Enabled = False
End Sub

Sub Trigger012_Hit() 'ActiveRamp InLaneLeft
	ActiveInLaneLeftTimer.Enabled = True
End Sub
Sub Trigger013_Hit() 'ActiverRamp InLaneRight
	ActiveInLaneRightTimer.Enabled = True
End Sub

Sub TriggerDebug_Hit()
'	StopMiniCooper
	If BallsOnPlayfield = 0 Then TriggerDebug.DestroyBall
End Sub

Sub TriggerTEST_Hit()
'	PuPlayer.LabelSet pDMD,"HighScore","Enter Your Name :",1,""
'	PuPlayer.LabelSet pDMD,"HighScoreL1","A",1,""
'	PuPlayer.LabelSet pDMD,"HighScoreL2","A",1,""
'	PuPlayer.LabelSet pDMD,"HighScoreL3","A",1,""
'	Drone3ONTimer.enabled=True
'	Bullet1ONTimer.enabled=True
'	ChangeGi yellow
'	FlashForMs Flasher008, 1500, 250, 0
'	FlashNikTimer.Enabled = 1
'	MajBGXP
	If SkillShotState >= 1 Then SkillShotState = SkillShotState + 1 : CheckSkillShot
'	PuPlayer.LabelShowPage pDMD,1,0,""
'	PuPlayer.LabelSet pDMD,"XP0","PUPAlphas\\XP0.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
'	BumperLactive
'	StartMiniCooper
'	StartSaekoCombo
'	DestroyRamp
'	StartModeDrone
'	FlashForMs FlasherDrone001, 1000, 250, 0
'	NICKYGunONTimer.enabled=True
'	NICKYDroneRotTimer.enabled=True
'	StartModeDrone
	LightCityON
'	GiEffect 4
'	LightEffect 2
'	AddScore 10000000
'	DebugDrone
'	ActiveKimiMass
End Sub

Sub TLeftOutlane_Hit() 'M
	AddScore 1550
	DOF 300, DOFPulse
	If LeftOutlane.State = 0 Then
	LeftOutlane.State = 2
	Elseif LeftOutlane.State = 2 Then
	LeftOutlane.State=1
	DOF 334, DOFPulse
	CheckLightLane
	End If
End Sub

Sub TLeftInlane_Hit() 'I
	AddScore 1550
	If ModeDroneActive = True And BallNikCount(CurrentPlayer) = 0 Then EndModeDrone
	leftInlaneSpeedLimit
	If LeftInlane.State = 0 Then
	LeftInlane.State = 2
	Elseif LeftInlane.State = 2 Then
	LeftInlane.State=1
	DOF 335, DOFPulse
	CheckLightLane
	End If
End Sub
Sub TRightInlane_Hit() 'K
	AddScore 1550
	If ModeDroneActive = True And BallNikCount(CurrentPlayer) = 0 Then EndModeDrone
	rightInlaneSpeedLimit
	If RightInlane.State = 0 Then
	RightInlane.State = 2
	Elseif RightInlane.State = 2 Then
	RightInlane.State=1
	DOF 336, DOFPulse
	CheckLightLane
	End If
End Sub

Sub TRightOutlane_Hit() 'I
	AddScore 1550
	DOF 309, DOFPulse
	If RightOutlane.State = 0 Then
	RightOutlane.State = 2
	Elseif RightOutlane.State = 2 Then
	RightOutlane.State=1
	DOF 337, DOFPulse
	CheckLightLane
	End If
End Sub

Sub CheckLightLane
	If LeftOutlane.State=1 And LeftInlane.State=1 And RightInlane.State=1 And RightOutlane.State=1 Then
	AddScore 8500
	DOF 338, DOFPulse
		If PlayfieldMultiplier <4 Then
		PlayfieldMultiplier = PlayfieldMultiplier + 1
		Playsound "Fx_UpBonus"
		LightEffect 5
		CheckLightBonus
		pupDMDDisplay "Splash4","Playfield^Multiplier","",3,0,10
		if UseUltraDMD = 1 then UDMD "Playfield X", "", 3000
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
'#################################
'FX SOUND
'#################################
Sub PlaySoundCarImpact
	Select Case Int(Rnd * 4) + 1
		Case 1
			PlaySound "Fx_CarImpact001"
		Case 2
			PlaySound "Fx_CarImpact002"
		Case 3
			PlaySound "Fx_CarImpact003"
		Case 4
			PlaySound "Fx_CarImpact004"
	End Select
End Sub

Sub PlaySoundCarMove
	Select Case Int(Rnd * 7) + 1
		Case 1
			PlaySound "Fx_CarMove006"
		Case 2
			PlaySound "Fx_CarMove001"
		Case 3
			PlaySound "Fx_CarMove002"
		Case 4
			PlaySound "Fx_CarMove003"
		Case 5
			PlaySound "Fx_CarMove004"
		Case 6
			PlaySound "Fx_CarMove005"
		Case 7
			PlaySound "Fx_CarMove000"
	End Select
End Sub

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

Sub PlaySoundVoNick
	Select Case Int(Rnd * 3) + 1
		Case 1
			PlaySound "Vo_Nick001"
		Case 2
			PlaySound "Vo_Nick002"
		Case 3
			PlaySound "Vo_Nick003"
	End Select
End Sub

Sub PlaySoundVoMaori
	Select Case Int(Rnd * 3) + 1
		Case 1
			PlaySound "Vo_Maori001"
		Case 2
			PlaySound "Vo_Maori002"
		Case 3
			PlaySound "Vo_Maori003"
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

Sub PlaySoundDrama
	Select Case Int(Rnd * 4) + 1
		Case 1
			PlaySound "Fx_Drama001"
		Case 2
			PlaySound "Fx_Drama002"
		Case 3
			PlaySound "Fx_Drama003"
		Case 4
			PlaySound "Fx_Drama004"
	End Select
End Sub

Sub PlaySoundTargetsMystery
	Select Case Int(Rnd * 3) + 1
		Case 1
			PlaySound "Fx_Shiny001"
		Case 2
			PlaySound "Fx_Shiny002"
		Case 3
			PlaySound "Fx_Shiny003"
	End Select
End Sub

Sub PlaySoundMamouth
	Select Case Int(Rnd * 6) + 1
		Case 1
			PlaySound "Fx_Mamouth001"
		Case 2
			PlaySound "Fx_Mamouth002"
		Case 3
			PlaySound "Fx_Mamouth003"
		Case 4
			PlaySound "Fx_Mamouth004"
		Case 5
			PlaySound "Fx_Mamouth005"
		Case 6
			PlaySound "Fx_Mamouth006"
	End Select
End Sub

Sub PlaySoundTarget
	Select Case Int(Rnd * 4) + 1
		Case 1
			PlaySound "fx_Target001"
		Case 2
			PlaySound "fx_Target002"
		Case 3
			PlaySound "fx_Target003"
		Case 4
			PlaySound "fx_Target004"
	End Select
End Sub
'###########
'MissionSAEKO COMBO
'###########
Sub StartSaekoCombo
	DOF 340, DOFPulse
	MissionCount = MissionCount + 1
	ChangeGi blue
	ModeSaekoActive = True
	PuPEvent 833
	PuPEvent 804
	SaekoCount = 0
	pupDMDDisplay "Splash4","SAEKO COMBO^Mode Actived","",3,0,10
	if UseUltraDMD = 1 then UDMD "SAEKO COMBO", "READY", 3000
	SetLightColor LiSaekoCombo, blue, 2
	LiMi004.State = 2
	MiSaekoTimer.Enabled = True
End Sub

Sub MiSaekoTimer_Timer()
	EndSaekoCombo
	MiSaekoTimer.Enabled = False
End Sub

Sub EndSaekoCombo
	ModeSaekoActive = False
	ChangeGi purple
	PuPEvent 814
	SaekoCount = 0
	SetLightColor LiSaekoCombo, blue, 0
	LiMi004.State = 0
	MiSaekoTimer.Enabled = False
End Sub

Sub CheckSaekoCombo
	If SaekoCount = 0 Then : SetLightColor LiSaekoCombo, green, 2
	If SaekoCount = 1 Then : SetLightColor LiSaekoCombo, green, 2
	If SaekoCount = 2 Then : SetLightColor LiSaekoCombo, blue, 2 : AddScore2 15000 : pupDMDDisplay "Splash4","COMBO SAEKO^15K","",3,1,10
	If SaekoCount = 3 Then : SetLightColor LiSaekoCombo, green, 2 :
	If SaekoCount = 4 Then : SetLightColor LiSaekoCombo, blue, 2 : AddScore2 30000 : pupDMDDisplay "Splash4","COMBO SAEKO^30K","",3,1,10
	If SaekoCount = 5 Then : SetLightColor LiSaekoCombo, green, 2 :
	If SaekoCount = 6 Then 
		Playsound "Fx_Bonus1"
		EndSaekoCombo 
		AddScore2 5000 
		pupDMDDisplay "Splash4","COMBO^SAEKO MULTIBALL","",3,1,10 
		if UseUltraDMD = 1 then UDMD "SAEKO", "MULTIBALL", 3000
		AddMB2 
		vpmtimer.addtimer 3000, "AddMB2 '"
		EnableBallSaver 15
	End If
End Sub

'#################################
'MissionTargets
'#################################
Sub StartMICats
	MissionCount = MissionCount + 1
	ChangeGi white
	LightEffect 4
	PuPEvent 802
	PuPEvent 836
	Playsound "Fx_CatsStart"
	AddScore2 3000
	pupDMDDisplay "Splash4","CATS EYES^Mode Actived","",3,0,10
	if UseUltraDMD = 1 then UDMD "CATS EYES", "Mode Actived", 3000
	If bMultiBallMode = False Then Playsong "Mu_CatsStart"
	RampCATSCount(CurrentPlayer) = 4
	CheckLightCats
	MiCatsCount = 0
	EnableBallSaver 15
	PlaySound "DropTarget_Up"
	TargetCat001.IsDropped = False
	TargetCat002.IsDropped = False
	TargetCat003.IsDropped = False
	LiTargCat001.state = 2
	LiTargCat002.state = 2
	LiTargCat003.state = 2
	MiCatsEndTimer.Enabled = True
	MiCatsStartTimer.Enabled = True
End Sub

Sub MiCatsStartTimer_Timer()
	PlaySound "DropTarget_Up"
	Select Case Int(Rnd * 13) + 1
		Case 1
			TargetCat001.IsDropped = False
			TargetCat002.IsDropped = False
			TargetCat003.IsDropped = False
			LiTargCat001.state = 2
			LiTargCat002.state = 2
			LiTargCat003.state = 2
		Case 2
			TargetCat001.IsDropped = True
			TargetCat002.IsDropped = False
			TargetCat003.IsDropped = False
			LiTargCat001.state = 0
			LiTargCat002.state = 2
			LiTargCat003.state = 2
		Case 3
			TargetCat001.IsDropped = False
			TargetCat002.IsDropped = True
			TargetCat003.IsDropped = False
			LiTargCat001.state = 2
			LiTargCat002.state = 0
			LiTargCat003.state = 2
		Case 4
			TargetCat001.IsDropped = False
			TargetCat002.IsDropped = False
			TargetCat003.IsDropped = True
			LiTargCat001.state = 2
			LiTargCat002.state = 2
			LiTargCat003.state = 0
		Case 5
			TargetCat001.IsDropped = True
			TargetCat002.IsDropped = False
			TargetCat003.IsDropped = True
			LiTargCat001.state = 0
			LiTargCat002.state = 2
			LiTargCat003.state = 0
		Case 6
			TargetCat001.IsDropped = False
			TargetCat002.IsDropped = True
			TargetCat003.IsDropped = True
			LiTargCat001.state = 2
			LiTargCat002.state = 0
			LiTargCat003.state = 0
		Case 7
			TargetCat001.IsDropped = True
			TargetCat002.IsDropped = True
			TargetCat003.IsDropped = False
			LiTargCat001.state = 0
			LiTargCat002.state = 0
			LiTargCat003.state = 2
		Case 8
			TargetCat001.IsDropped = True
			TargetCat002.IsDropped = False
			TargetCat003.IsDropped = False
			LiTargCat001.state = 0
			LiTargCat002.state = 2
			LiTargCat003.state = 2
		Case 9
			TargetCat001.IsDropped = False
			TargetCat002.IsDropped = True
			TargetCat003.IsDropped = False
			LiTargCat001.state = 2
			LiTargCat002.state = 0
			LiTargCat003.state = 2
		Case 10
			TargetCat001.IsDropped = False
			TargetCat002.IsDropped = False
			TargetCat003.IsDropped = True
			LiTargCat001.state = 2
			LiTargCat002.state = 2
			LiTargCat003.state = 0
		Case 11
			TargetCat001.IsDropped = True
			TargetCat002.IsDropped = False
			TargetCat003.IsDropped = True
			LiTargCat001.state = 0
			LiTargCat002.state = 2
			LiTargCat003.state = 0
		Case 12
			TargetCat001.IsDropped = False
			TargetCat002.IsDropped = True
			TargetCat003.IsDropped = True
			LiTargCat001.state = 2
			LiTargCat002.state = 0
			LiTargCat003.state = 0
		Case 13
			TargetCat001.IsDropped = True
			TargetCat002.IsDropped = True
			TargetCat003.IsDropped = False
			LiTargCat001.state = 0
			LiTargCat002.state = 0
			LiTargCat003.state = 2
	End Select
End Sub

Sub MiCatsEndTimer_Timer()
	EndMICats
	AddScore2 10000
	If bMultiBallMode = False Then UpdateMusicNow
	MiCatsEndTimer.Enabled = False
	MiCatsStartTimer.Enabled = False
End Sub

Sub CheckAwardMiCats
	If MiCatsCount = 1 Then AddScore2 60000
	If MiCatsCount = 2 Then AddScore2 80000
	If MiCatsCount = 3 Then AddScore2 150000 : AddMB2 : EnableBallSaver 15
	If MiCatsCount = 4 Then AddScore2 200000
	If MiCatsCount = 5 Then AddScore2 250000
	If MiCatsCount = 6 Then AddScore2 300000 : AddMB2 : EnableBallSaver 15
	If MiCatsCount = 7 Then AddScore2 350000
	If MiCatsCount = 8 Then AddScore2 400000
	If MiCatsCount = 9 Then AddScore2 500000 : AddMB2 : EndMICats
End Sub

Sub EndMICats
	If RampCATSCount(CurrentPlayer) = 4 Then RampCATSCount(CurrentPlayer) = 0
	CheckLightCats
	PuPEvent 812
	ChangeGi purple
	MiCatsCount = 0
	PlaySound "Drop_Target_Down_6"
	TargetCat001.IsDropped = True 
	TargetCat002.IsDropped = True
	TargetCat003.IsDropped = True
	LiTargCat001.state = 0
	LiTargCat002.state = 0
	LiTargCat003.state = 0
	MiCatsStartTimer.Enabled = False
	MiCatsEndTimer.Enabled = False
End Sub

Sub TargetCat001_Hit
	PlaySound "Drop_Target_Down_4"
	MiCatsCount = MiCatsCount + 1
	LiTargCat001.state = 0
	CheckAwardMiCats
End Sub 
Sub TargetCat002_Hit
	PlaySound "Drop_Target_Down_3"
	MiCatsCount = MiCatsCount + 1
	LiTargCat002.state = 0
	CheckAwardMiCats
End Sub 
Sub TargetCat003_Hit
	PlaySound "Drop_Target_Down_4"
	MiCatsCount = MiCatsCount + 1
	LiTargCat003.state = 0
	CheckAwardMiCats
End Sub 

Sub StartMIGreen
	MissionCount = MissionCount + 1
	ChangeGi green
	AddScore2 5000
	LightEffect 2
	PuPEvent 805
	FlashForMs F1A006, 500, 250, 0
	FlashForMs F1A007, 500, 250, 0
	FlashForMs Flasher001, 3000, 200, 0
	pupDMDDisplay "Splash4","UMIBOZU BONUS^Active by Ramp","",3,0,10
	if UseUltraDMD = 1 then UDMD "UMIBOZU", "BONUS READY", 3000
	GreenCount(CurrentPlayer) = 4
	BonusGreenCount(CurrentPlayer) = BonusGreenCount(CurrentPlayer) + 1
	MIGreenActive(CurrentPlayer) = True
	CheckLiGreen
	LiMi005.State = 2
	LiGreen001.state = 0
'	OFFLightPFhaut
End Sub
Sub CheckLiGreen
'	If GreenCount = 4 Then 
		If BonusGreenCount(CurrentPlayer) = 0 Then Li10M.State = 0 : LiMi005.State = 0 : PuPEvent 815
		If BonusGreenCount(CurrentPlayer) = 1 Then Li10M.State = 2 : LiMi005.State = 2
		If BonusGreenCount(CurrentPlayer) = 2 Then Li10M.State = 1 : LiMi005.State = 0 : PuPEvent 815
		If BonusGreenCount(CurrentPlayer) = 3 Then Li10M.State = 1 : Li15M.State = 2 : LiMi005.State = 2
		If BonusGreenCount(CurrentPlayer) = 4 Then Li10M.State = 1 : Li15M.State = 1 : LiMi005.State = 0 : PuPEvent 815
		If BonusGreenCount(CurrentPlayer) = 5 Then Li10M.State = 1 : Li15M.State = 1 : Li20M.State = 2 : LiMi005.State = 2
		If BonusGreenCount(CurrentPlayer) = 6 Then Li10M.State = 1 : Li15M.State = 1 : Li20M.State = 1 : LiMi005.State = 0 : PuPEvent 815
		If BonusGreenCount(CurrentPlayer) = 7 Then Li10M.State = 1 : Li15M.State = 1 : Li20M.State = 1 : Li100M.State = 2 : LiMi005.State = 2
		If BonusGreenCount(CurrentPlayer) >= 8 Then Li10M.State = 1 : Li15M.State = 1 : Li20M.State = 1 : Li100M.State = 1 : LiMi005.State = 0 : PuPEvent 815
'	End If
End Sub
Sub AwardMIGreen 
	If BonusGreenCount(CurrentPlayer) = 2 Then AddScore2 1000000 : GiEffect 4 : pupDMDDisplay "Splash4","UMIBOZU BONUS^1 Million","",3,0,10 : PuPEvent 850 : DOF 450, DOFPulse
	If BonusGreenCount(CurrentPlayer) = 4 Then AddScore2 2000000 : GiEffect 4 : pupDMDDisplay "Splash4","UMIBOZU BONUS^2 Million","",3,0,10 : PuPEvent 851 : DOF 450, DOFPulse
	If BonusGreenCount(CurrentPlayer) = 6 Then AddScore2 5000000 : GiEffect 4 : pupDMDDisplay "Splash4","UMIBOZU BONUS^5 Million","",3,0,10 : PuPEvent 850 : DOF 450, DOFPulse
	If BonusGreenCount(CurrentPlayer) = 8 Then AddScore2 10000000 : GiEffect 4 : pupDMDDisplay "Splash4","UMIBOZU BONUS^10 Million","",3,0,10 : PuPEvent 851 : DOF 450, DOFPulse
	MIGreenActive(CurrentPlayer) = False
	GreenCount(CurrentPlayer) = 0
	CheckLiGreen
	PuPEvent 815
	ChangeGi purple
'	ResetLightPFhaut
	LiMi005.State = 0
	LiGreen001.state = 2
'	LiGreen002.state = 2
'	LiGreen003.state = 2
'	LiGreen004.state = 2
End Sub

Sub StartMIYell
	MissionCount = MissionCount + 1
	ChangeGi yellow
	LightEffect 2
	PuPEvent 803
	If MiCatsStartTimer.Enabled = False And bMultiBallMode = False Then PlaySong "Mu_StartMini"
	FlashForMs F1A006, 500, 250, 0
	FlashForMs F1A007, 500, 250, 0
	FlashForMs Flasher001, 3000, 200, 0
	AddScore2 5000
	YellCount(CurrentPlayer) = 4
	MIYellActive(CurrentPlayer) = True
	pupDMDDisplay "Splash3","Mode MiniCooper^Timer 60s^Browse the places","",4,1,10
	if UseUltraDMD = 1 then UDMD "MINICOOPER", "MODE 60S", 3000
	MiYellEndTimer.Enabled = True
	LiMi003.State = 2
	LiKARR001.State = 2
	LiKARR002.State = 2
	LiKARR003.State = 2
	LiKARR004.State = 2
	LiKARR005.State = 2
	LiYell001.state = 0
'	OFFLightPFhaut
End Sub
Sub CheckAwardMIYell
	If YellCount(CurrentPlayer) = 4 And LiKARR001.State = 1 And LiKARR002.State = 1 And LiKARR003.State = 1 And LiKARR004.State = 1 And LiKARR005.State = 1 Then
		MIYellActive(CurrentPlayer) = False
		PuPEvent 813
		ChangeGi purple
		YellCount(CurrentPlayer) = 0
		pupDMDDisplay "Splash4","Mode MiniCooper^SUCCESSFUL 6 Million ","",3,0,12
		if UseUltraDMD = 1 then UDMD "MINICOOPER", "6 MILLION", 3000
		If MiCatsStartTimer.Enabled = False and bMultiBallMode = False Then UpdateMusicNow
		AddScore2 6000000
'		LiStartDrone.state = 2
		LiMi003.State = 0
		LiKARR001.State = 0
		LiKARR002.State = 0
		LiKARR003.State = 0
		LiKARR004.State = 0
		LiKARR005.State = 0
'		ResetLightPFhaut
		LiYell001.state = 2
'		LiYell002.state = 2
'		LiYell003.state = 2
'		LiYell004.state = 2
		MiYellEndTimer.Enabled = False
	End If
End Sub

Sub MiYellEndTimer_Timer()
	MIYellActive(CurrentPlayer) = False
	PuPEvent 813
	ChangeGi purple
	pupDMDDisplay "Splash4","Mode MiniCooper^FAILED","",3,0,10
	if UseUltraDMD = 1 then UDMD "MINICOOPER", "Mode FAILED", 3000
	If MiCatsStartTimer.Enabled = False and bMultiBallMode = False Then UpdateMusicNow
	YellCount(CurrentPlayer) = 0
	AddScore2 15000
	LiMi003.State = 0
	LiKARR001.State = 0
	LiKARR002.State = 0
	LiKARR003.State = 0
	LiKARR004.State = 0
	LiKARR005.State = 0
	LiYell001.state = 2
'	LiYell002.state = 2
'	LiYell003.state = 2
'	LiYell004.state = 2
	MiYellEndTimer.Enabled = False
End Sub

Sub MiYellEndNow
	MIYellActive(CurrentPlayer) = False
	PuPEvent 813
	ChangeGi purple
	UpdateMusicNow
	YellCount(CurrentPlayer) = 0
	LiMi003.State = 0
	LiKARR001.State = 0
	LiKARR002.State = 0
	LiKARR003.State = 0
	LiKARR004.State = 0
	LiKARR005.State = 0
	LiYell001.state = 2
	MiYellEndTimer.Enabled = False
End Sub

Sub CheckLightMB
	If MBLock_Sav(CurrentPlayer) = 0 Then MBLockActive(CurrentPlayer) = False : LiLOCK001.State = 0 : LiLOCK002.State = 0 : LiLOCK003.State = 0 : LiLOCK001b.State = 0 : LiLOCK002b.State = 0 : LiLOCK003b.State = 0 
	If MBLock_Sav(CurrentPlayer) = 1 Then MBLockActive(CurrentPlayer) = True : LiLOCK001.State = 2 : LiLOCK002.State = 0 : LiLOCK003.State = 0 : LiLOCK001b.State = 2 : LiLOCK002b.State = 0 : LiLOCK003b.State = 0
	If MBLock_Sav(CurrentPlayer) = 2 Then MBLockActive(CurrentPlayer) = False : LiLOCK001.State = 1 : LiLOCK002.State = 0 : LiLOCK003.State = 0 : LiLOCK001b.State = 1 : LiLOCK002b.State = 0 : LiLOCK003b.State = 0
	If MBLock_Sav(CurrentPlayer) = 3 Then MBLockActive(CurrentPlayer) = True : LiLOCK001.State = 1 : LiLOCK002.State = 2 : LiLOCK003.State = 0 : LiLOCK001b.State = 1 : LiLOCK002b.State = 2 : LiLOCK003b.State = 0
	If MBLock_Sav(CurrentPlayer) = 4 Then MBLockActive(CurrentPlayer) = False : LiLOCK001.State = 1 : LiLOCK002.State = 1 : LiLOCK003.State = 0 : LiLOCK001b.State = 1 : LiLOCK002b.State = 1 : LiLOCK003b.State = 0
	If MBLock_Sav(CurrentPlayer) = 5 Then MBLockActive(CurrentPlayer) = True : LiLOCK001.State = 1 : LiLOCK002.State = 1 : LiLOCK003.State = 2 : LiLOCK001b.State = 1 : LiLOCK002b.State = 1 : LiLOCK003b.State = 2
	If MBLock_Sav(CurrentPlayer) >= 6 Then MBLockActive(CurrentPlayer) = False : LiLOCK001.State = 1 : LiLOCK002.State = 1 : LiLOCK003.State = 1 : LiLOCK001b.State = 1 : LiLOCK002b.State = 1 : LiLOCK003b.State = 1
End Sub

Sub Mode_Multiball_Start
	MissionCount = MissionCount + 1
	ChangeGi red
	DOF 482, DOFOn
	pupDMDDisplay "Splash4","MULTIBALL^START","",3,0,14
	if UseUltraDMD = 1 then UDMD "KAORI", "MULTIBALL", 3000
	FlashForMs Flasher001, 3000, 200, 0
	FlashForMs Flasher008, 2000, 250, 0
	MBLock_Sav(CurrentPlayer) = 6
	PlaySong "Mu_MB"
	PuPEvent 806
	PuPEvent 831
	AddScore2 15000
	bMultiBallMode = True
	LiMi006.State = 2
	EnableBallSaver 15
'	EnableBallSaver BallSaverTime
	AddMB1
	vpmtimer.addtimer 3000, "AddMB2 '"
	vpmtimer.addtimer 6000, "AddMB2 '"
'	BallsOnPlayfield = 3
End Sub

Sub Mode_Multiball_End 
	AddScore2 35000
	DOF 482, DOFOff
	bMultiBallMode = False
	ChangeGi purple
	PuPEvent 816
	MBLock_Sav(CurrentPlayer) = 0
	MBLockActive(CurrentPlayer) = False
	CheckLightMB
	LiMi006.State = 0
End Sub

'*************
' ModeRampeBroke
'*************
Sub DestroyRamp
	DOF 118, DOFOn
    DOF 119, DOFOn
	Playsound "Fx_Lazer2"
	MetalRL.visible = False
	GlassRL.visible = False
	GlassRLCASSE.visible = True
	GlassRLCASSE2.visible = True
	SolCasse.visible = True
	GraveCASSE.visible = True
	GraveCASSE2.visible = True
	MetalRLCASSE.visible = True
	MetalRLCASSE2.visible = True
	ShakerEndTimer.Enabled = True
End Sub

Sub ShakerEndTimer_Timer()
	DOF 118, DOFOff
    DOF 119, DOFOff
	ShakerEndTimer.Enabled = False
End Sub

Sub RestorRamp
	MetalRL.visible = True
	GlassRL.visible = True
	GlassRLCASSE.visible = False
	GlassRLCASSE2.visible = False
	SolCasse.visible = False
	GraveCASSE.visible = False
	GraveCASSE2.visible = False
	MetalRLCASSE.visible = False
	MetalRLCASSE2.visible = False
End Sub

'#################################
'Mini Playfield
'#####################
Dim MiniCooperRight
MiniCooperRight = False

Sub MiniCooperTimer_TImer()
	
If not MiniCooperRight Then
	MiniCooper.TransX = MiniCooper.TransX +1
	If MiniCooper.TransX = 0 Then
		MiniCooperTimer.enabled=False
		MiniCooperRight=False		
	End IF
Else	
	If MiniCooper.TransX = -95 Then
		MiniCooperTimer.enabled=False
		MiniCooperRight=True
	Else
		MiniCooper.TransX = MiniCooper.TransX -1
	End IF
End If

End Sub
Sub BumperLactive
	If ModeMiniCooperActive = True And MiniCooperRight = True Then
	PlaySoundCarMove
	MiniCooperTimer.enabled=True
	Bumper001.Collidable = True
	Bumper002.Collidable = False	
	MiniCooperRight = False	
	End If
End Sub

Sub BumperRactive
	If ModeMiniCooperActive = True And MiniCooperRight = False Then
	PlaySoundCarMove
	MiniCooperTimer.enabled=True
	Bumper001.Collidable = False
	Bumper002.Collidable = True
	MiniCooperRight = True
	End If
End Sub

Sub DebugMiniCooper
	MiniCooperTimer.enabled=False
	MiniCooper.TransX = 0
	MiniCooperRight=False
End Sub

Dim RoutePos, FramesRoute
FramesRoute = Array("Route001", "Route002", "Route003", "Route004", "Route005", "Route006", "Route007") 
RoutePos = 0
Sub Routetimer_timer
    Flasher002.ImageA = FramesRoute(RoutePos)
    RoutePos = (RoutePos + 1) MOD 7
End Sub

Sub StartMiniCooper
	PuPEvent 837
	Playsound "fx_car"
	DOF 442, DOFPulse
	LightEffect 7
	ModeMiniCooperActive = True
'	BumperLactive
	Bumper001.Collidable = True
	Bumper002.Collidable = False
	Flasher002.visible = True
	Routetimer.Enabled = True
	Target005.Collidable = False
	Target006.Collidable = False
End Sub
Sub StopMiniCooper
	BumperLactive
	vpmtimer.addtimer 1500, "DebugMiniCooper '" 
	Flasher002.visible = False
	ModeMiniCooperActive = False
	Routetimer.Enabled = False
	Target005.Collidable = False
	Target006.Collidable = False
End Sub

Sub Bumper001_Hit()
	AddScore 1500
	PlaySoundCarImpact
	Target005.Collidable = True
	Target006.Collidable = True
End Sub
Sub Bumper002_Hit()
	AddScore 1500
	PlaySoundCarImpact
	Target005.Collidable = True
	Target006.Collidable = True
End Sub

'*************
' Flasher NiCky
'*************
Dim FlashNikPos, FramesFlashNik
FramesFlashNik = Array("NiKI000", "NiKI001", "NiKI002", "NiKI003", "NiKI004", "NiKI005", "NiKI006", "NiKI007", "NiKI008", "NiKI008", "NiKI007", "NiKI006", "NiKI005", "NiKI004", "NiKI003", "NiKI002", "NiKI001")

'FlashNikTimer.Enabled = 1
Sub FlashNikTimer_timer
	Flasher008.visible = 1
	Flasher008.ImageA = FramesFlashNik(FlashNikPos)
	FlashNikPos = (FlashNikPos + 1) MOD 17
	vpmtimer.addtimer 2080, "StopFlashNik '"
End Sub
Sub StopFlashNik
	Flasher008.visible = 0
	Flasher008.ImageA = "NiKI008"
'	FlashForMs Flasher008, 1500, 250, 0
	FlashNikTimer.Enabled = 0
End Sub
'*************
' Flasher GUN
' Animation Nicky
'*************
Dim CanonPos, FramesCanon
FramesCanon = Array("Canon1", "Canon2", "Canon3", "Canon4", "Canon5", "Canon6", "Canon7", "Canon8", "Canon9")

Sub CanonTimer_timer
	DOF 118, DOFOn
    DOF 119, DOFOn
	ShakerEndTimer.Enabled = True
	Canon001.ImageA = FramesCanon(CanonPos)
	Canon002.ImageA = FramesCanon(CanonPos)
	Canon003.ImageA = FramesCanon(CanonPos)
	CanonPos = (CanonPos + 1) MOD 9
	vpmtimer.addtimer 1500, "StopCanon '"
End Sub
Sub StopCanon
'	DOF 118, DOFOff
'   DOF 119, DOFOff
	Canon001.visible = 0
	Canon002.visible = 0
	Canon003.visible = 0
	CanonTimer.Enabled = 0
End Sub

Dim	Gun1Pos, FramesGun
FramesGun = Array("MF-000", "MF-001", "MF-002", "MF-003", "MF-004", "MF-005", "MF-006", "MF-007", "MF-008", "MF-009", "MF-010", "MF-011", "MF-012", "MF-013", "MF-014", "MF-015", "MF-016", "MF-017", "MF-018", "MF-019", "MF-020", _
"MF-021", "MF-022", "MF-023", "MF-024", "MF-025", "MF-026", "MF-027", "MF-028", "MF-029")

Sub StartGun
	If ModeDroneActive = True Then
		If BallNikCount(CurrentPlayer) >= 1 Then
		BallNikCount(CurrentPlayer) = BallNikCount(CurrentPlayer) - 1
		CheckBallNik
			If DroneStat = 1 Then
			GunFlash001.visible = 1
			Gun1Pos = 0
			GunTimer.Enabled = 1
			PlaySound "fx_Gun"
				If LiDrone001.state=2 Then 
				AddScore2 155000
				PuPEvent 832
				LiDrone001.state=0
				Canon001.visible = 1
				CanonTimer.Enabled = 1
				If Drone1ON=True Then Drone1ONTimer.enabled=True	
				End If
			Elseif DroneStat = 2 Then
			GunFlash002.visible = 1
			Gun1Pos = 0
			GunTimer.Enabled = 1
			PlaySound "fx_Gun"
				If LiDrone002.state=2 Then 
				AddScore2 205000
				PuPEvent 832
				LiDrone002.state=0
				Canon002.visible = 1
				CanonTimer.Enabled = 1
				If Drone2ON=True Then Drone2ONTimer.enabled=True	
				End If
			Elseif DroneStat = 3 Then
			GunFlash003.visible = 1
			Gun1Pos = 0
			GunTimer.Enabled = 1
			PlaySound "fx_Gun"
				If LiDrone003.state=2 Then 
				AddScore2 505000
				PuPEvent 832
				LiDrone003.state=0
				Canon003.visible = 1
				CanonTimer.Enabled = 1 
				If Drone3ON=True Then Drone3ONTimer.enabled=True	
				End If
			End If
		Elseif BallNikCount(CurrentPlayer) = 0 Then
			PlaySound ""
			EndModeDrone
		End If 
	End If
End Sub

Sub GunTimer_timer
	GunFlash001.ImageA = FramesGun(Gun1Pos)
	GunFlash002.ImageA = FramesGun(Gun1Pos)
	GunFlash003.ImageA = FramesGun(Gun1Pos)
	Gun1Pos = (Gun1Pos + 1) MOD 30
	vpmtimer.addtimer 1500, "StopGun '"
End Sub

Sub StopGun
	GunFlash001.visible = 0
	GunFlash002.visible = 0
	GunFlash003.visible = 0
	GunTimer.Enabled = 0
End Sub

'Sub GunSeqENDTimer_Timer()
'	If NICKYGunON=True Then	NICKYGunONTimer.enabled=True
'	GunSeqENDTimer.Enabled = False
'End Sub
'--------------------
Sub DebugDrone
NikBlase2.RotX = 10
NikBrasD.RotX = 10
NikGun.RotX = 10
NikPant.ObjRotZ = -30
NikBust.ObjRotZ = -30 
NikBlase.ObjRotZ = -30 
NikBlase2.ObjRotZ = -30 
NikBrasD.ObjRotZ = -30 
NikBrasG.ObjRotZ = -30 
NikGun.ObjRotZ = -30 
NikHair.ObjRotZ = -30 
NikHead.ObjRotZ = -30 
Drone001.TransX = 60
Drone001.TransY = -156
Drone001.TransZ = -60
Drone002.TransX = 35
Drone002.TransY = -166
Drone003.TransX = -250
Drone003.TransY = -195
Drone003.TransZ = 100
Drone004.TransX = -130
Drone004.TransY = -155
Drone004.TransZ = -290
Drone001.DisableLighting = 0
Drone002.DisableLighting = 0
Drone003.DisableLighting = 0
Drone004.DisableLighting = 0
NICKYGunONTimer.enabled=False
NICKYDroneRotTimer.enabled=False
Drone3ONTimer.enabled=False
Drone3ONTimer.enabled=False
Drone3ONTimer.enabled=False
End Sub

Dim NICKYGunON
NICKYGunON = False

Sub NICKYGunONTimer_TImer()
	
If not NICKYGunON Then
	NikBlase2.RotX = NikBlase2.RotX +1
	NikBrasD.RotX = NikBrasD.RotX +1
	NikGun.RotX = NikGun.RotX +1
	If NikGun.RotX = 90 Then
		NICKYGunONTimer.enabled=False
		NICKYGunON=True		
	End IF
Else	
	If NikGun.RotX = 10 Then
		NICKYGunONTimer.enabled=False
		NICKYGunON=False
	Else
	NikBlase2.RotX = NikBlase2.RotX -1
	NikBrasD.RotX = NikBrasD.RotX -1
	NikGun.RotX = NikGun.RotX -1
	End IF
End If
End Sub
'------------------
Dim DroneStat
DroneStat = 0

Sub NICKYDroneRotTimer_TImer()
If ModeDroneActive = True Then	
If DroneStat = 0 Then 'Drone 1 Ready
	NikPant.ObjRotZ = NikPant.ObjRotZ + 1
	NikBust.ObjRotZ = NikBust.ObjRotZ + 1
	NikBlase.ObjRotZ = NikBlase.ObjRotZ + 1
	NikBlase2.ObjRotZ = NikBlase2.ObjRotZ + 1
	NikBrasD.ObjRotZ = NikBrasD.ObjRotZ + 1
	NikBrasG.ObjRotZ = NikBrasG.ObjRotZ + 1
	NikGun.ObjRotZ = NikGun.ObjRotZ + 1
	NikHair.ObjRotZ = NikHair.ObjRotZ + 1
	NikHead.ObjRotZ = NikHead.ObjRotZ + 1
	If NikGun.ObjRotZ = 5 Then
		NICKYDroneRotTimer.enabled=False
		DroneStat=1		'Drone 1 Ready
	End IF
Elseif DroneStat = 1 Then 
	NikPant.ObjRotZ = NikPant.ObjRotZ - 1
	NikBust.ObjRotZ = NikBust.ObjRotZ - 1
	NikBlase.ObjRotZ = NikBlase.ObjRotZ - 1
	NikBlase2.ObjRotZ = NikBlase2.ObjRotZ - 1
	NikBrasD.ObjRotZ = NikBrasD.ObjRotZ - 1
	NikBrasG.ObjRotZ = NikBrasG.ObjRotZ - 1
	NikGun.ObjRotZ = NikGun.ObjRotZ - 1
	NikHair.ObjRotZ = NikHair.ObjRotZ - 1
	NikHead.ObjRotZ = NikHead.ObjRotZ - 1
	If NikGun.ObjRotZ = -55 Then '-60
		NICKYDroneRotTimer.enabled=False
		DroneStat=2  'Drone 2 Ready
	End IF
Elseif DroneStat = 2 Then
	NikPant.ObjRotZ = NikPant.ObjRotZ - 1
	NikBust.ObjRotZ = NikBust.ObjRotZ - 1
	NikBlase.ObjRotZ = NikBlase.ObjRotZ - 1
	NikBlase2.ObjRotZ = NikBlase2.ObjRotZ - 1
	NikBrasD.ObjRotZ = NikBrasD.ObjRotZ - 1
	NikBrasG.ObjRotZ = NikBrasG.ObjRotZ - 1
	NikGun.ObjRotZ = NikGun.ObjRotZ - 1
	NikHair.ObjRotZ = NikHair.ObjRotZ - 1
	NikHead.ObjRotZ = NikHead.ObjRotZ - 1
	If NikGun.ObjRotZ = -110 Then '-115
		NICKYDroneRotTimer.enabled=False
		DroneStat=3  'Drone 3 Ready
	End IF
Elseif DroneStat = 3 Then
	NikPant.ObjRotZ = NikPant.ObjRotZ + 1
	NikBust.ObjRotZ = NikBust.ObjRotZ + 1
	NikBlase.ObjRotZ = NikBlase.ObjRotZ + 1
	NikBlase2.ObjRotZ = NikBlase2.ObjRotZ + 1
	NikBrasD.ObjRotZ = NikBrasD.ObjRotZ + 1
	NikBrasG.ObjRotZ = NikBrasG.ObjRotZ + 1
	NikGun.ObjRotZ = NikGun.ObjRotZ + 1
	NikHair.ObjRotZ = NikHair.ObjRotZ + 1
	NikHead.ObjRotZ = NikHead.ObjRotZ + 1
	If NikGun.ObjRotZ = 5 Then
		NICKYDroneRotTimer.enabled=False
		DroneStat=1  'Drone 1 Ready
	End IF
End If
Elseif ModeDroneActive = False Then
If DroneStat = 1 Then 'Drone 2 Ready
	NikPant.ObjRotZ = NikPant.ObjRotZ - 1
	NikBust.ObjRotZ = NikBust.ObjRotZ - 1
	NikBlase.ObjRotZ = NikBlase.ObjRotZ - 1
	NikBlase2.ObjRotZ = NikBlase2.ObjRotZ - 1
	NikBrasD.ObjRotZ = NikBrasD.ObjRotZ - 1
	NikBrasG.ObjRotZ = NikBrasG.ObjRotZ - 1
	NikGun.ObjRotZ = NikGun.ObjRotZ - 1
	NikHair.ObjRotZ = NikHair.ObjRotZ - 1
	NikHead.ObjRotZ = NikHead.ObjRotZ - 1
		If NikGun.ObjRotZ = -30 Then
		NICKYDroneRotTimer.enabled=False
		DroneStat=0
	End IF
	Elseif DroneStat = 2 Then 'Drone 3 Ready
	NikPant.ObjRotZ = NikPant.ObjRotZ + 1
	NikBust.ObjRotZ = NikBust.ObjRotZ + 1
	NikBlase.ObjRotZ = NikBlase.ObjRotZ + 1
	NikBlase2.ObjRotZ = NikBlase2.ObjRotZ + 1
	NikBrasD.ObjRotZ = NikBrasD.ObjRotZ + 1
	NikBrasG.ObjRotZ = NikBrasG.ObjRotZ + 1
	NikGun.ObjRotZ = NikGun.ObjRotZ + 1
	NikHair.ObjRotZ = NikHair.ObjRotZ + 1
	NikHead.ObjRotZ = NikHead.ObjRotZ + 1
	If NikGun.ObjRotZ = -30 Then
		NICKYDroneRotTimer.enabled=False
		DroneStat=0
	End IF
	Elseif DroneStat = 3 Then 'Retour Normal
	NikPant.ObjRotZ = NikPant.ObjRotZ + 1
	NikBust.ObjRotZ = NikBust.ObjRotZ + 1
	NikBlase.ObjRotZ = NikBlase.ObjRotZ + 1
	NikBlase2.ObjRotZ = NikBlase2.ObjRotZ + 1
	NikBrasD.ObjRotZ = NikBrasD.ObjRotZ + 1
	NikBrasG.ObjRotZ = NikBrasG.ObjRotZ + 1
	NikGun.ObjRotZ = NikGun.ObjRotZ + 1
	NikHair.ObjRotZ = NikHair.ObjRotZ + 1
	NikHead.ObjRotZ = NikHead.ObjRotZ + 1
	If NikGun.ObjRotZ = -30 Then
		NICKYDroneRotTimer.enabled=False
		DroneStat=0
	End IF
End If
End If
End Sub

Sub EndModeDrone
	ModeDroneActive = False
	PuPEvent 811
	Drone001.DisableLighting = 0
	Drone002.DisableLighting = 0
	Drone003.DisableLighting = 0
	Drone004.DisableLighting = 0
	LiTourn001.State = 0
	LiTourn002.State = 0
	LiTourn003.State = 0
	LiMi001.State = 0
	If BallNikCount(CurrentPlayer) = 6 Then BallNikCount(CurrentPlayer) = BallNikCount(CurrentPlayer) - 1 : CheckBallNik
	LiDrone001.state=0
	LiDrone002.state=0
	LiDrone003.state=0
	NICKYDroneRotTimer.enabled=True
	DroneChangeTimer.enabled=False
	DroneFireTimer.enabled=False	
	If NICKYGunON=True Then NICKYGunONTimer.enabled=True
	If Drone1ON=True Then Drone1ONTimer.enabled=True
	If Drone2ON=True Then Drone2ONTimer.enabled=True
	If Drone3ON=True Then Drone3ONTimer.enabled=True
	vpmtimer.addtimer 4500, "DebugDrone '"
End Sub

Sub StartModeDrone
	MissionCount = MissionCount + 1
	Drone001.DisableLighting = 1
	Drone002.DisableLighting = 1
	Drone003.DisableLighting = 1
	Drone004.DisableLighting = 1
	ModeDroneActive = True
	DOF 341, DOFPulse
	LightEffect 5
	PuPEvent 801
	PuPEvent 834
	PlaySound "Fx_DroneStart"
	LiTourn001.State = 2
	LiTourn002.State = 2
	LiTourn003.State = 2
	LiMi001.State = 2
	LiStartDrone.state = 0
	DroneChangeTimer.enabled=True
	Drone1ONTimer.enabled=True 
	Drone2ONTimer.enabled=True
'	Drone3ONTimer.enabled=True
	DroneFireTimer.enabled=True
	If NICKYGunON=False Then NICKYGunONTimer.enabled=True
	NICKYDroneRotTimer.enabled=True
End Sub
'----------------------
Dim Drone1ON
Dim Drone2ON
Dim Drone3ON
Drone1ON = False
Drone2ON = False
Drone3ON = False

Sub Drone1ONTimer_TImer()
If not Drone1ON Then
	Drone001.TransX = Drone001.TransX -1
	Drone001.TransY = Drone001.TransY +2.6
	Drone001.TransZ = Drone001.TransZ +1
	If Drone001.TransX = 0 Then
		Drone1ONTimer.enabled=False
		Drone1ON=True
		LiDrone001.state=2
	End IF
Else	
	If Drone001.TransX = 60 Then
		Drone1ONTimer.enabled=False
		Drone1ON=False
		LiDrone001.state=0
	Else
	LiDrone001.state=0
	Drone001.TransX = Drone001.TransX +1
	Drone001.TransY = Drone001.TransY -2.6
	Drone001.TransZ = Drone001.TransZ -1
	End IF
End If
End Sub

Sub Drone2ONTimer_TImer()
If not Drone2ON Then
	Drone002.TransX = Drone002.TransX -1
	Drone002.TransY = Drone002.TransY +4.7
	If Drone002.TransX = 0 Then
		Drone2ONTimer.enabled=False
		Drone2ON=True	
		LiDrone002.state=2
	End IF
Else	
	If Drone002.TransX = 35 Then
		Drone2ONTimer.enabled=False
		Drone2ON=False
		LiDrone002.state=0
	Else
	LiDrone002.state=0
	Drone002.TransX = Drone002.TransX +1
	Drone002.TransY = Drone002.TransY -4.7
	End IF
End If
End Sub

'Sub Drone3ONTimer_TImer()
'If not Drone3ON Then
'	Drone003.TransX = Drone003.TransX +0.15
'	Drone003.TransY = Drone003.TransY +1.95
'	Drone003.TransZ = Drone003.TransZ +1
'	If Drone003.TransZ = 0 Then
'		Drone3ONTimer.enabled=False
'		Drone3ON=True
'		LiDrone003.state=2
'	End IF
'Else	
'	If Drone003.TransZ = -100 Then
'		Drone3ONTimer.enabled=False
'		Drone3ON=False
'		LiDrone003.state=0
'	Else
'	LiDrone003.state=0
'	Drone003.TransX = Drone003.TransX -0.15
'	Drone003.TransY = Drone003.TransY -1.95
'	Drone003.TransZ = Drone003.TransZ -1
'	End IF
'End If
'End Sub

Sub Drone3ONTimer_TImer()
If not Drone3ON Then
	Drone004.TransX = Drone004.TransX +4.48
	Drone004.TransY = Drone004.TransY +5.34
	Drone004.TransZ = Drone004.TransZ +10
	If Drone004.TransZ = 0 Then
		Drone3ONTimer.enabled=False
		Drone3ON=True
		LiDrone003.state=2
	End IF
Else	
	If Drone004.TransZ = -290 Then
		Drone3ONTimer.enabled=False
		Drone3ON=False
		LiDrone003.state=0
	Else
	LiDrone003.state=0
	Drone004.TransX = Drone004.TransX -4.48
	Drone004.TransY = Drone004.TransY -5.34
	Drone004.TransZ = Drone004.TransZ -10
	End IF
End If
End Sub

Sub DroneChangeTimer_Timer()
	PlaySound "Fx_DroneFly"
	If Drone1ON=False And Drone2ON=False And Drone3ON=False Then Drone1ONTimer.enabled=True : DroneFireTimer.enabled=True
	If Drone1ON=True And Drone2ON=True And Drone3ON=True Then Drone2ONTimer.enabled=True : Drone3ONTimer.enabled=True : DroneFireTimer.enabled=True
	If Drone1ON=True And Drone2ON=True And Drone3ON=False Then Drone1ONTimer.enabled=True : Drone2ONTimer.enabled=True : Drone3ONTimer.enabled=True : DroneFireTimer.enabled=True
	If Drone1ON=True And Drone2ON=False And Drone3ON=False Then Drone1ONTimer.enabled=True : Drone2ONTimer.enabled=True : DroneFireTimer.enabled=True
	If Drone1ON=False And Drone2ON=True And Drone3ON=False Then Drone2ONTimer.enabled=True : Drone3ONTimer.enabled=True : DroneFireTimer.enabled=True
	If Drone1ON=False And Drone2ON=False And Drone3ON=True Then Drone3ONTimer.enabled=True : Drone1ONTimer.enabled=True : DroneFireTimer.enabled=True
End Sub
Sub DroneFireTimer_Timer()
	If Drone1ON=True Then FlashForMs FlasherDrone001, 1000, 300, 0 : PlaySound "Fx_Lazer"
	If Drone2ON=True Then FlashForMs FlasherDrone002, 1000, 340, 0 : PlaySound "Fx_Lazer"
	If Drone3ON=True Then FlashForMs FlasherDrone003, 1000, 320, 0 : vpmtimer.addtimer 900, "DestroyRamp '"
	DroneFireTimer.enabled=False
End Sub

Sub CheckBallNik
	If BallNikCount(CurrentPlayer) = 0 Then LiBall001.state = 0 : LiBall002.state = 0 : LiBall003.state = 0 : LiBall004.state = 0 : LiBall005.state = 0 : LiBall006.state = 0 
	If BallNikCount(CurrentPlayer) = 1 Then LiBall001.state = 1 : LiBall002.state = 0 : LiBall003.state = 0 : LiBall004.state = 0 : LiBall005.state = 0 : LiBall006.state = 0 
	If BallNikCount(CurrentPlayer) = 2 Then LiBall001.state = 1 : LiBall002.state = 1 : LiBall003.state = 0 : LiBall004.state = 0 : LiBall005.state = 0 : LiBall006.state = 0 
	If BallNikCount(CurrentPlayer) = 3 Then LiBall001.state = 1 : LiBall002.state = 1 : LiBall003.state = 1 : LiBall004.state = 0 : LiBall005.state = 0 : LiBall006.state = 0 
	If BallNikCount(CurrentPlayer) = 4 Then LiBall001.state = 1 : LiBall002.state = 1 : LiBall003.state = 1 : LiBall004.state = 1 : LiBall005.state = 0 : LiBall006.state = 0 
	If BallNikCount(CurrentPlayer) = 5 Then LiBall001.state = 1 : LiBall002.state = 1 : LiBall003.state = 1 : LiBall004.state = 1 : LiBall005.state = 1 : LiBall006.state = 0 
	If BallNikCount(CurrentPlayer) = 6 And ModeDroneActive=False Then LiBall001.state = 1 : LiBall002.state = 1 : LiBall003.state = 1 : LiBall004.state = 1 : LiBall005.state = 1 : LiBall006.state = 1 : LiStartDrone.state = 2
	If BallNikCount(CurrentPlayer) = 6 And ModeDroneActive=True Then LiBall001.state = 1 : LiBall002.state = 1 : LiBall003.state = 1 : LiBall004.state = 1 : LiBall005.state = 1 : LiBall006.state = 1
End Sub 

'*************
' Animation Nicky
'*************
Dim KamiMassON
KamiMassON = False

Sub KamiMassONTimer_TImer()
If not KamiMassON Then
	Mass.ObjRotX = Mass.ObjRotX -5
	Mass.ObjRotY = Mass.ObjRotY -5
	KamiShoes.ObjRotX = KamiShoes.ObjRotX -5
	KamiBras.ObjRotX = KamiBras.ObjRotX -5
	KamiBuste.ObjRotX = KamiBuste.ObjRotX -5
	KamiGant.ObjRotX = KamiGant.ObjRotX -5
	KamiHair.ObjRotX = KamiHair.ObjRotX -5
	KamiHead.ObjRotX = KamiHead.ObjRotX -5
	KamiJambes.ObjRotX = KamiJambes.ObjRotX -5
	KamiRobe.ObjRotX = KamiRobe.ObjRotX -5
	KamiVeste.ObjRotX = KamiVeste.ObjRotX -5
	KamiShoes.ObjRotY = KamiShoes.ObjRotY -5
	KamiBras.ObjRotY = KamiBras.ObjRotY -5
	KamiBuste.ObjRotY = KamiBuste.ObjRotY -5
	KamiGant.ObjRotY = KamiGant.ObjRotY -5
	KamiHair.ObjRotY = KamiHair.ObjRotY -5
	KamiHead.ObjRotY = KamiHead.ObjRotY -5
	KamiJambes.ObjRotY = KamiJambes.ObjRotY -5
	KamiRobe.ObjRotY = KamiRobe.ObjRotY -5
	KamiVeste.ObjRotY = KamiVeste.ObjRotY -5
	If Mass.ObjRotX = 0 Then
		KamiMassONTimer.enabled=False
		KamiMassON=True
		Wall051.Collidable = True
'		LiDrone001.state=2
	End IF
Else	
	If Mass.ObjRotX = 55 Then
		KamiMassONTimer.enabled=False
		KamiMassON=False
'		LiDrone001.state=0
	Else
	Wall051.Collidable = False
	Mass.ObjRotX = Mass.ObjRotX +0.5
	Mass.ObjRotY = Mass.ObjRotY +0.5
	KamiShoes.ObjRotX = KamiShoes.ObjRotX +0.5
	KamiBras.ObjRotX = KamiBras.ObjRotX +0.5
	KamiBuste.ObjRotX = KamiBuste.ObjRotX +0.5
	KamiGant.ObjRotX = KamiGant.ObjRotX +0.5
	KamiHair.ObjRotX = KamiHair.ObjRotX +0.5
	KamiHead.ObjRotX = KamiHead.ObjRotX +0.5
	KamiJambes.ObjRotX = KamiJambes.ObjRotX +0.5
	KamiRobe.ObjRotX = KamiRobe.ObjRotX +0.5
	KamiVeste.ObjRotX = KamiVeste.ObjRotX +0.5
	KamiShoes.ObjRotY = KamiShoes.ObjRotY +0.5
	KamiBras.ObjRotY = KamiBras.ObjRotY +0.5
	KamiBuste.ObjRotY = KamiBuste.ObjRotY +0.5
	KamiGant.ObjRotY = KamiGant.ObjRotY +0.5
	KamiHair.ObjRotY = KamiHair.ObjRotY +0.5
	KamiHead.ObjRotY = KamiHead.ObjRotY +0.5
	KamiJambes.ObjRotY = KamiJambes.ObjRotY +0.5
	KamiRobe.ObjRotY = KamiRobe.ObjRotY +0.5
	KamiVeste.ObjRotY = KamiVeste.ObjRotY +0.5
	End IF
End If
End Sub

Sub ActiveKimiMass
	KamiMassONTimer.enabled=True
End Sub
'*************
' Save CurrentPlayer
'*************
Sub ResetEvents
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to2"
	If B2SOn Then Controller.B2SSetscore 6, BallinGame(CurrentPlayer)
	ChangeBall(0)
	SolURFlipper 0
	SolULFlipper 0
	FlInitBumper 1, "red"
	FlInitBumper 2, "red"
	FlInitBumper 3, "red"
	ChangeGi purple
	ChangeGILogo white
	BonusMultiplier = 1
	PlayfieldMultiplier = 1
	CheckLightBonus
	GreenCount(CurrentPlayer) = 0
	BonusGreenCount(CurrentPlayer) = 0
	YellCount(CurrentPlayer) = 0
	MIGreenActive(CurrentPlayer) = False
	MIYellActive(CurrentPlayer) = False
	MBLockActive(CurrentPlayer) = False
	MBLock_Sav(CurrentPlayer) = 0
	If KamiMassON=False Then ActiveKimiMass
	CheckLightMB 
	ResetLightPFhaut
	KickBackActive(CurrentPlayer) = False
	LiTargetM001.State = 0
	MiniCooperRight = False
	NICKYGunON = False
	ModeMiniCooperActive = False
	OFFLightPFhaut
	EndModeDrone
	ModeDroneActive = False
	BallNikCount(CurrentPlayer) = 0
	CheckBallNik
	RestorRamp
	ResetLightLane
	MysteryState(CurrentPlayer) = False
	EndMICats
	RampCATSCount(CurrentPlayer) = 0
	CheckLightCats
	ExtraBCount = 0
	RampCount = 0
	MikiCount = 0
	MissionCount = 0
	ResetSlingCount
	CheckLightSlingCount
	SkillShotState = 1
	CheckSkillShot
	ResetMystery
	EndSaekoCombo
	ResetPosBullets
	DebugMiniCooper
End Sub
	
Sub CheckEventPlayer
	ChangeGi purple
	ChangeGILogo white
	If B2SOn Then Controller.B2SSetscore 6, BallinGame(CurrentPlayer)
	EndModeDrone
	EndMICats
	MiYellEndNow
	ModeMiniCooperActive = False
	vpmtimer.addtimer 1500, "DebugMiniCooper '"
	OFFLightPFhaut
	BonusMultiplier = 1
	PlayfieldMultiplier = 1
	CheckLightBonus
	ExtraBCount = 0
	RampCount = 0
	MikiCount = 0
	MissionCount = 0
	CheckLightSlingCount
	SkillShotState = 1
	CheckSkillShot
	EndSaekoCombo
	If KickBackActive(CurrentPlayer) = False Then LiTargetM001.State = 0
	If KickBackActive(CurrentPlayer) = True Then LiTargetM001.State = 1
	If TotalGamesPlayed >= 2 Then
		If MysteryState(CurrentPlayer) = False Then	ResetMystery
		If MysteryState(CurrentPlayer) = True Then LiTargetR001.State = 1 : LiTargetR002.State = 1 : LiTargetR003.State = 1 : LiMyst001.State = 2
	CheckBallNik
	CheckLightMB 
	CheckKamiMassState 	
	CheckLiGreen
'	CheckColorMISS
	LiStartDrone.state = 0
	End If
End Sub

Sub CheckKamiMassState 
	If MBLock_Sav(CurrentPlayer) = 6 And KamiMassON=False Then ActiveKimiMass
	If MBLock_Sav(CurrentPlayer) = 5 And KamiMassON=True Then ActiveKimiMass
	If MBLock_Sav(CurrentPlayer) = 4 And KamiMassON=True Then ActiveKimiMass
	If MBLock_Sav(CurrentPlayer) = 3 And KamiMassON=True Then ActiveKimiMass
	If MBLock_Sav(CurrentPlayer) = 2 And KamiMassON=True Then ActiveKimiMass
	If MBLock_Sav(CurrentPlayer) = 1 And KamiMassON=True Then ActiveKimiMass
	If MBLock_Sav(CurrentPlayer) = 0 And KamiMassON=False Then ActiveKimiMass
End Sub

Sub ResetSlingCount
	SlingCount(CurrentPlayer) = 0
	LiLedR001.state = 0 
	LiLedR002.state = 0
	LiLedR003.state = 0 
	LiLedR004.state = 0 
	LiLedR005.state = 0 
	LiLedR006.state = 0 
End Sub

Sub CheckLightSlingCount
	If SlingCount(CurrentPlayer) <= 10 Then LiLedR001.state = 0 : LiLedR002.state = 0 : LiLedR003.state = 0 : LiLedR004.state = 0 : LiLedR005.state = 0 : LiLedR006.state = 0 
	If SlingCount(CurrentPlayer) >= 11 and SlingCount(CurrentPlayer) <= 20 Then LiLedR001.state = 1 : LiLedR002.state = 0 : LiLedR003.state = 0 : LiLedR004.state = 0 : LiLedR005.state = 0 : LiLedR006.state = 0 
	If SlingCount(CurrentPlayer) >= 21 and SlingCount(CurrentPlayer) <= 30 Then LiLedR001.state = 1 : LiLedR002.state = 1 : LiLedR003.state = 0 : LiLedR004.state = 0 : LiLedR005.state = 0 : LiLedR006.state = 0 
	If SlingCount(CurrentPlayer) >= 31 and SlingCount(CurrentPlayer) <= 40 Then LiLedR001.state = 1 : LiLedR002.state = 1 : LiLedR003.state = 1 : LiLedR004.state = 0 : LiLedR005.state = 0 : LiLedR006.state = 0 
	If SlingCount(CurrentPlayer) >= 41 and SlingCount(CurrentPlayer) <= 50 Then LiLedR001.state = 1 : LiLedR002.state = 1 : LiLedR003.state = 1 : LiLedR004.state = 1 : LiLedR005.state = 0 : LiLedR006.state = 0 
	If SlingCount(CurrentPlayer) >= 51 and SlingCount(CurrentPlayer) <= 70 Then LiLedR001.state = 1 : LiLedR002.state = 1 : LiLedR003.state = 1 : LiLedR004.state = 1 : LiLedR005.state = 1 : LiLedR006.state = 0 
	If SlingCount(CurrentPlayer) >= 71 and SlingCount(CurrentPlayer) <= 99 Then LiLedR001.state = 2 : LiLedR002.state = 2 : LiLedR003.state = 2 : LiLedR004.state = 2 : LiLedR005.state = 2 : LiLedR006.state = 2 
	If SlingCount(CurrentPlayer) >= 100 Then PuPEvent 839 : ResetSlingCount : AddMB2
End Sub

Sub CheckSkillShot
	If SkillShotState = 0 Then
		SkillShotState = 0
		SetLightColor LiBonus001, yellow, 2
		SetLightColor LiBonus002, yellow, 2
		SetLightColor LiBonus003, yellow, 2
		SkillShotTimer.Enabled = False
	End If
	If SkillShotState = 1 Then
			Select Case Int(Rnd * 3) + 1
				Case 1
					SetLightColor LiBonus001, green, 2
					SetLightColor LiBonus002, yellow, 0
					SetLightColor LiBonus003, yellow, 0
				Case 2
					SetLightColor LiBonus001, yellow, 0
					SetLightColor LiBonus002, green, 2
					SetLightColor LiBonus003, yellow, 0
				Case 3
					SetLightColor LiBonus001, yellow, 0
					SetLightColor LiBonus002, yellow, 0
					SetLightColor LiBonus003, green, 2
			End Select
	End If
	If SkillShotState = 2 Then SkillShotState = 3
	If SkillShotState = 3 Then SkillShotTimer.Enabled = True
	If SkillShotState >= 4 Then
		SkillShotState = 0
		SetLightColor LiBonus001, yellow, 2
		SetLightColor LiBonus002, yellow, 2
		SetLightColor LiBonus003, yellow, 2
		pupDMDDisplay "Splash4","SkillShot^LOSE","",3,0,10
		if UseUltraDMD = 1 then UDMD "SkillShot", "LOSE", 3000
		SkillShotTimer.Enabled = False
	End If
End Sub

Sub WinSkillShot
	SkillShotState = 0
	AddScore2 35000
	BonusMultiplier = BonusMultiplier + 1
	Playsound "Fx_UpBonus"
	pupDMDDisplay "Splash4","SkillShot^35k & Bonus X","",4,1,11
	if UseUltraDMD = 1 then UDMD "SkillShot", "35k & Bonus X", 3000
	SetLightColor LiBonus001, yellow, 0
	SetLightColor LiBonus002, yellow, 0
	SetLightColor LiBonus003, yellow, 0
	CheckLightBonus
	SkillShotTimer.Enabled = False
End Sub

Sub SkillShotTimer_Timer()
	SkillShotState = 0
	SetLightColor LiBonus001, yellow, 2
	SetLightColor LiBonus002, yellow, 2
	SetLightColor LiBonus003, yellow, 2
	pupDMDDisplay "Splash4","SkillShot^LOSE","",3,0,10
	if UseUltraDMD = 1 then UDMD "SkillShot", "LOSE", 3000
	SkillShotTimer.Enabled = False
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
'   Pinup Active Backglass
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

'**************************
'   PinUp Player Config
'   Change HasPuP = True if using PinUp Player Videos
'**************************

	Dim HasPup:HasPuP = True

	Dim PuPlayer

	Const pTopper=0
	Const pDMD=1
	Const pBackglass=2
	Const pPlayfield=3
	Const pMusic=4
	Const pMusic2=5
	Const pCallouts=6
	Const pBackglass2=7
	Const pTopper2=8
	Const pPopUP=9
	Const pGame=10


	if HasPuP Then
	on error resume next
	Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay") 
	PuPlayer.B2SInit "", pGameName   'use new method to startup pup with dmd via puppack
	on error goto 0
	if not IsObject(PuPlayer) then HasPuP = False
	end If

	PuPlayer.LabelInit pBackglass

Sub resetbackglassOFFScore
	PuPlayer.LabelSet pDMD,"BonusMultip", FormatNumber(BonusMultiplier,0),0,""
	PuPlayer.LabelSet pDMD,"Bonus1", FormatNumber(SlingCount(CurrentPlayer),0),0,""
	PuPlayer.LabelSet pDMD,"Bonus2", FormatNumber(RampCount,0),0,""
	PuPlayer.LabelSet pDMD,"Bonus3", FormatNumber(MikiCount,0),0,""
	PuPlayer.LabelSet pDMD,"Bonus4", FormatNumber(MissionCount,0),0,""
	PuPlayer.LabelSet pDMD,"BonusTotal", FormatNumber(TotalBonus,0),0,""
	PuPEvent 821
End Sub

Function BonusScoreTotal()
	BonusScoreTotal = BonusMultiplier * ((SlingCount(CurrentPlayer) * 50) + (RampCount * 1000) + (MikiCount * 2000) + (MissionCount * 5000) )
End Function 
Sub SeqBonus
	vpmtimer.addtimer 200, "SeqBonus1 '"
	vpmtimer.addtimer 650, "SeqBonus2 '"
	vpmtimer.addtimer 1120, "SeqBonus3 '"
	vpmtimer.addtimer 1570, "SeqBonus4 '"
	vpmtimer.addtimer 2120, "SeqBonus5 '"
End Sub
Sub SeqBonus1
	PuPlayer.LabelSet pDMD,"BonusMultip","BONUS x " & FormatNumber(BonusMultiplier,0),1,""
	PuPlayer.LabelSet pDMD,"Bonus1", "Experience = " & FormatNumber((SlingCount(CurrentPlayer) * 50),0),1,""
'	PlaySound "Bat"
End Sub
Sub SeqBonus2
	PuPlayer.LabelSet pDMD,"Bonus2", "Ramps = " & FormatNumber((RampCount * 1000),0),1,""
End Sub
Sub SeqBonus3
	PuPlayer.LabelSet pDMD,"Bonus3", "Shield Used = " & FormatNumber((MikiCount * 2000),0),1,""
End Sub
Sub SeqBonus4
	PuPlayer.LabelSet pDMD,"Bonus4", "Missions Actived = " & FormatNumber((MissionCount * 5000),0),1,""
End Sub
Sub SeqBonus5
	PuPlayer.LabelSet pDMD,"BonusTotal", "TOTAL BONUS = " & FormatNumber(BonusScoreTotal,0),1,""
'	PlaySound "Bat2"
	PlayerScore(CurrentPlayer) = PlayerScore(CurrentPlayer) + BonusScoreTotal
End Sub

	'called on table load
Sub resetbackglass
'	PuPlayer.LabelSet pBackglass,"titleimg","",1,"{'mt':2,'color':111111, 'width': 0, 'height': 0, 'yalign': 0}"
	PuPlayer.LabelShowPage pBackglass,1,0,""
End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   Pupdmd Settings
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
	'PUPDMD Layout for each Table1
	'Setup Pages.  Note if you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS  "case sensitive exact naming fonts!"
	'*****************************************************************
	dim dmdnote:dmdnote="2-shortnote.mp4"

	Sub pSetPageLayouts

	DIM dmddef
	DIM dmdtomb
	DIM dmdalt
	DIM dmdscr
	DIM dmdfixed

	'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
	'***********************************************************************'
	'<screen#>, in standard wed set this to pDMD ( or 1)
	'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
	'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
	'<size%>, Height as a percent of display height. 20=20% of screen height.
	'<colour>, integer value of windows color.
	'<rotation>, degrees in tenths   (900=90 degrees)
	'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
	'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
	'<xpos>, this should be 0, but if you want to force a position you can set this. it is a % of horizontal width. 20=20% of screen width.
	'<ypos> same as xpos.
	'<PageNum> IMPORTANT this will assign this label to this page or group.
	'<visible> initial state of label. visible=1 show, 0 = off.


	if PuPDMDDriverType=pDMDTypeReal Then 'using RealDMD Mirroring.  **********  128x32 Real Color DMD  
		dmdalt="PKMN Pinball"
		dmdfixed="Instruction"
		dmdscr="Impact"    'main scorefont
		dmddef="Zig"
		dmdtomb="Ruben"

		'Page 1 (default score display)
			PuPlayer.LabelNew pDMD,"Play1"   ,dmddef,15,10013642   ,1,0,2,2,0,1,0
			PuPlayer.LabelNew pDMD,"Ball"    ,dmddef,15,10013642   ,1,2,2,96,0,1,0 '
			PuPlayer.LabelNew pDMD,"CurScore",dmdscr,60,33023   ,0,1,1, 0,0,1,0

		'Page 2 (default Text Splash 1 Big Line)
			PuPlayer.LabelNew pDMD,"Splash",dmdscr,60,16744448,0,1,1,0,0,2,0

		'Page 3 (default Text Splash 2 and 3 Lines)
			 PuPlayer.LabelNew pDMD,"Splash3a",dmddef,25,33023,0,1,0,0,2,3,0
			 PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,25,10013642,0,1,0,0,30,3,0
			 PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,10013642,0,1,0,0,55,3,0

		'Page 4 (2 Line Gameplay DMD)
'			 PuPlayer.LabelNew pDMD,"Splash4a",dmddef,13,2697513,0,1,0,0,30,4,0
'			 PuPlayer.LabelNew pDMD,"Splash4b",dmddef,10,2697513,0,1,2,0,55,4,0
			PuPlayer.LabelNew pDMD,"Splash4a",dmddef,25,8454143,0,1,0,0,10,4,0
			PuPlayer.LabelNew pDMD,"Splash4b",dmddef,25,157,0,1,2,0,80,4,0

		'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
			PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0

		'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
			PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
			PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
			PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0

		'Page 7 (Show High Scores Fixed Fonts)
'			PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,8454143,0,1,0,0,2,7,0
'			PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,33023,0,1,0,0,20,7,0
'			PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,33023,0,1,0,0,50,7,0
			PuPlayer.LabelNew pDMD,"Splash7a",dmddef,25,157,0,1,0,0,5,7,0
			PuPlayer.LabelNew pDMD,"Splash7b",dmddef,20,10013642,0,1,0,0,40,7,0
			PuPlayer.LabelNew pDMD,"Splash7c",dmddef,20,10013642,0,1,0,0,70,7,0

			pDMDStartBackLoop "Videoscenes","MainBack.mp4"
'			pDMDStartBackLoop "DMDSplash","intro1.mp4"
'			dmdnote="1-shortnote.mp4"

	END IF  ' use PuPDMDDriver

	if PuPDMDDriverType=pDMDTypeLCD THEN  'Using 4:1 Standard ratio LCD PuPDMD  ************ lcd **************

		'dmddef="Impact"
		dmdalt="PKMN Pinball"
		dmdfixed="Instruction"
		dmdscr="Impact"    'main scorefont
		dmddef="Zig"
		dmdtomb="Ruben"
		
		'Page 1 (default score display)
		PuPlayer.LabelNew pDMD,"Credits" ,dmdscr,20,16744448   ,1,0,0,3,0,1,0 '
		PuPlayer.LabelNew pDMD,"Play1"   ,dmdscr,25,16744448   ,1,0,2,2,0,1,0
		PuPlayer.LabelNew pDMD,"Ball"    ,dmdscr,25,16744448   ,1,2,2,96,0,1,0 '
		PuPlayer.LabelNew pDMD,"MsgScore",dmdscr,45,33023   ,0,1,0, 0,40,1,0
		PuPlayer.LabelNew pDMD,"CurScore",dmdscr,60,33023   ,0,1,1, 0,0,1,0

		'Page 2 (default Text Splash 1 Big Line)
		PuPlayer.LabelNew pDMD,"Splash",dmdtomb,60,16744448,0,1,1,0,0,2,0
		PuPlayer.LabelNew pBackglass, "InfoGame",dmdtomb,60,159	,0,1,1, 0,0,2,1

		'Page 3 (default Text 3 Lines)
			PuPlayer.LabelNew pDMD,"Splash3a",dmdscr,35,33023	,0,1,0,0,2,3,0
			PuPlayer.LabelNew pDMD,"Splash3b",dmdscr,25,159	,0,1,0,0,30,3,0
			PuPlayer.LabelNew pDMD,"Splash3c",dmdtomb,35,16744448	,0,1,0,0,57,3,0


		'Page 4 (default Text 2 Line)
		PuPlayer.LabelNew pDMD,"Splash4a",dmdtomb,40,8454143,0,1,0,0,0,4,0
		PuPlayer.LabelNew pDMD,"Splash4b",dmdtomb,30,157,0,1,2,0,75,4,0

		'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
			PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,2697513,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,2697513  ,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,2697513  ,0,1,1,0,0,5,0

		'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
			PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,2697513,0,0,0,15,1,6,0
			PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,2697513,0,1,0,60,0,6,0
			PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,2697513,0,1,0,60,50,6,0

		'Page 7 (Show High Scores Fixed Fonts)
			PuPlayer.LabelNew pDMD,"Splash7a",dmdtomb,40,157,0,1,0,0,1,7,0
			PuPlayer.LabelNew pDMD,"Splash7b",dmdscr,34,8454143,0,1,0,0,36,7,0
			PuPlayer.LabelNew pDMD,"Splash7c",dmdscr,30,8454143,0,1,0,0,66,7,0
 
			pDMDStartBackLoop "Videoscenes","MainBack.mp4"
'			dmdnote="1-shortnote.mp4"

	END IF  ' use PuPDMDDriver

	if PuPDMDDriverType=pDMDTypeFULL THEN  'Using FULL BIG LCD PuPDMD  ************ lcd **************

		'dmddef="Impact"
		dmdalt="PKMN Pinball"
		dmdfixed="Instruction"
		dmdscr="Impact"    'main scorefont
		dmddef="Zig"
		dmdtomb="Ruben"

		'Page 1 (default score display)		
			PuPlayer.LabelNew pDMD,"Credits" ,dmdscr,7,5548032   ,0,1,1,34,62,1,0 '
			PuPlayer.LabelNew pDMD,"PlayersNumber" ,dmdscr,7,5112227   ,0,1,1,60,62,1,0 'OK
			PuPlayer.LabelNew pDMD,"Ball"    ,dmdscr,7,12870144   ,0,1,1,85,62,1,0 'OK
			PuPlayer.LabelNew pDMD,"Play1"   ,dmdscr,10,5112227   ,0,1,1,88,77,1,0 'OK
			PuPlayer.LabelNew pDMD,"CurScore",dmdscr,16,32896   ,0,1,1,52,80,1,0 'OK
'			PuPlayer.LabelNew pDMD,"Bonus1"	,dmdscr,		    6,16777215  ,0,1,1,52,30,1,0 'JackpotsBonus
'			PuPlayer.LabelNew pDMD,"Bonus2"	,dmdscr,		    6,16777215  ,0,1,1,52,36,1,0 'RaiderBonus
'			PuPlayer.LabelNew pDMD,"Bonus3",dmdscr,			    6,16777215  ,0,1,1,52,42,1,0 'CroftBonus
'			PuPlayer.LabelNew pDMD,"Bonus4",dmdscr,			    6,16777215  ,0,1,1,52,48,1,0 'RelicsBonus
'			PuPlayer.LabelNew pDMD,"Bonus5",dmdscr,			    6,16777215  ,0,1,1,52,54,1,0 'ArtefactsBonus
'			PuPlayer.LabelNew pDMD,"BonusTotal",dmdscr,			8,16777215  ,0,1,1,78,42,1,0
			PuPlayer.LabelNew pDMD,"CScoreP1",dmdscr,6,32896   ,0,1,1,12,70,1,0
			PuPlayer.LabelNew pDMD,"CScoreP2",dmdscr,6,12870144     ,0,1,1,12,78,1,0
			PuPlayer.LabelNew pDMD,"CScoreP3",dmdscr,6,128    ,0,1,1,12,86,1,0
			PuPlayer.LabelNew pDMD,"CScoreP4",dmdscr,6,1992703   ,0,1,1,12,94,1,0

		'Page 2 (default Text Splash 1 Big Line)
			PuPlayer.LabelNew pDMD,"Splash",dmdscr,20,116732584,0,1,1,52,70,2,0

		'Page 3 (default Text 3 Lines)
			PuPlayer.LabelNew pDMD,"Splash3a",dmdscr,16,16744448	,0,1,0,52,58,3,0 '33023 orange
			PuPlayer.LabelNew pDMD,"Splash3b",dmdscr,14,159	,0,1,0,52,72,3,0 '159 rouge
			PuPlayer.LabelNew pDMD,"Splash3c",dmdscr,16,33023	,0,1,0,52,82,3,0 '16744448 vert


		'Page 4 (default Text 2 Line)
			PuPlayer.LabelNew pDMD,"Splash4a",dmdscr,20,16732584,0,1,0,52,55,4,0 '8454143
			PuPlayer.LabelNew pDMD,"Splash4b",dmdscr,18,5112227,0,1,2,52,94,4,0  '8454143

		'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
			PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,2697513,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,2697513  ,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,2697513  ,0,1,1,0,0,5,0

		'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
			PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,2697513,0,0,0,15,1,6,0
			PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,2697513,0,1,0,60,0,6,0
			PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,2697513,0,1,0,60,50,6,0

		'Page 7 (Show High Scores Fixed Fonts)
			PuPlayer.LabelNew pDMD,"Splash7a",dmdscr,18,5112227,0,1,0,52,55,7,0
			PuPlayer.LabelNew pDMD,"Splash7b",dmdscr,16,32896,0,1,0,52,72,7,0
			PuPlayer.LabelNew pDMD,"Splash7c",dmdscr,14,32896,0,1,0,52,85,7,0

		'Page 8 (HighScore)
		PuPlayer.LabelNew pDMD,"HighScore",dmdscr,			10,33023  ,0,1,1,48,70,1,1
		PuPlayer.LabelNew pDMD,"HighScoreL1",dmdscr,		10,16744448	,0,1,1,67,70,1,1
		PuPlayer.LabelNew pDMD,"HighScoreL2",dmdscr,		10,16744448	,0,1,1,71,70,1,1
		PuPlayer.LabelNew pDMD,"HighScoreL3",dmdscr,		10,16744448	,0,1,1,75,70,1,1
'		PuPlayer.LabelNew pDMD,"HighScoreL4",dmdscr,		16,32896	,0,1,1,52,80,1,1
		
		'onverlay 
		PuPlayer.LabelNew pDMD,"XP0",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"XP1",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"XP2",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"XP3",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"XP4",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"XP5",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"XP6",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0

		'Bonus 
		PuPlayer.LabelNew pDMD,"BonusMultip",dmdscr,		10,59624  ,0,1,1,80,22,1,0
		PuPlayer.LabelNew pDMD,"Bonus1",dmdscr,			    8,5112227  ,0,1,1,40,10,1,0 'ExperienceBonus 
		PuPlayer.LabelNew pDMD,"Bonus2",dmdscr,			    8,5548032  ,0,1,1,40,18,1,0 'Rampes 
		PuPlayer.LabelNew pDMD,"Bonus3",dmdscr,			    8,5112227  ,0,1,1,40,26,1,0 'Miki Kickback 
		PuPlayer.LabelNew pDMD,"Bonus4",dmdscr,			    8,5548032  ,0,1,1,40,34,1,0 'Missions Actived 
		PuPlayer.LabelNew pDMD,"BonusTotal",dmdscr,			12,1992703  ,0,1,1,60,44,1,0

'		pDMDStartBackLoop "Videoscenes","MainBack.mp4"
'		pDMDStartBackLoop "Videoscenes","MStart.mp4"
'		pDMDStartBackLoop "PuPOverlays","Overlay3.png"
'		pDMDStartBackLoop "DMDSplash","intro2.mp4"
'		dmdnote="1-shortnote.mp4"
'		dmdnote="MStart.mp4"

	END IF  ' use PuPDMDDriver

	end Sub 'page Layouts


	'*****************************************************************
	'        PUPDMD Custom SUBS/Events for each Table1
	'     **********    MODIFY THIS SECTION!!!  ***************
	'*****************************************************************
	'

	Sub pDMDStartBall
	end Sub

	Sub pDMDGameOver
	playclear pDMD
	pAttractStart
	end Sub

	Sub pAttractStart
	pDMDSetPage(pDMDBlank)   'set blank text overlay page.
	pCurAttractPos=0
	pInAttract=True 'Startup in AttractMode
	pAttractNext
	end Sub


	Sub pDMDStartUP
	pInAttract=true
	end Sub

	Sub pDMDStartGame
	pInAttract=false
	pDMDSetPage(pScores)   'set blank text overlay page.
	PuPEvent 841
'	PuPEvent 100
	end Sub

	DIM pCurAttractPos: pCurAttractPos=0


	'********************** gets called auto each page next and timed already in DMD_Timer.  make sure you use pupDMDDisplay or it wont advance auto.
	Sub pAttractNext
	pCurAttractPos=pCurAttractPos+1

	  Select Case pCurAttractPos

	  Case 1 pupDMDDisplay "Splash4","Game^Over", "",3, 0,10
			PlaySong "Mu_Intro"
'			PuPEvent 840
	  Case 2 pupDMDDisplay "highscore", "High Score^AAA   2451654^BBB   2342342", "", 5, 0, 10
			PuPEvent 840
      Case 3 If(Credits> 0) then
			pupDMDDisplay "Splash4","Press^Start","",3,1,10
			Else
			pupDMDDisplay "Splash4","INSERT^Coins","",3,1,10
			End If
      Case 4 pupDMDDisplay "Splash3","City Hunter^By^Tombg","",4,0,10
	  Case 5 pupDMDDisplay "Splash4","Magna Right^AI DEMO MODE ","",3,0,10
	  Case 6 If(Credits> 0) then
			pupDMDDisplay "Splash4","Press^Start","",3,1,10
			Else
			pupDMDDisplay "Splash4","INSERT^Coins","",3,1,10
			End If
	  Case Else
		pCurAttractPos=0
		pAttractNext 'reset to beginning
	  end Select

	end Sub

'************************ called during gameplay to update Scores ***************************
Dim CurTestScore:CurTestScore=0
Sub pDMDUpdateScores  'call this ONLY on timer 300ms is good enough
If halplay=0 Then
if pDMDCurPage <> pScores then Exit Sub

puPlayer.LabelSet pDMD,"CurScore","" & FormatNumber(PlayerScore(CurrentPlayer), 0),1,""
puPlayer.LabelSet pDMD,"Play1","Player  " & FormatNumber(CurrentPlayer, 0),1,""
puPlayer.LabelSet pDMD,"Ball","Balls " & FormatNumber(BallinGame(CurrentPlayer), 0) & "/" & FormatNumber(BallsPerGame, 0),1,""
MajBGXP
PuPlayer.LabelSet pDMD,"PlayersNumber","Players "& FormatNumber(PlayersPlayingGame, 0) & "/4",1,""
PuPlayer.LabelSet pDMD,"Credits","Crédits "& Credits,1,""

If PlayersPlayingGame <= 1 or UseBGB2S = 1 Then 
	PuPlayer.LabelSet pDMD,"CScoreP1","P1 - " & FormatNumber(PlayerScore(1),0),0,""
	PuPlayer.LabelSet pDMD,"CScoreP2","P2 - " & FormatNumber(PlayerScore(2),0),0,""
	PuPlayer.LabelSet pDMD,"CScoreP3","P3 - " & FormatNumber(PlayerScore(3),0),0,""
	PuPlayer.LabelSet pDMD,"CScoreP4","P4 - " & FormatNumber(PlayerScore(4),0),0,""
End If 
'If B2SOff Then
If UseBGB2S = 0 Then
	If PlayersPlayingGame > 1 Then PuPlayer.LabelSet pDMD,"CScoreP1","P1 - " & FormatNumber(PlayerScore(1),0),1,""
	If PlayersPlayingGame >= 2 Then PuPlayer.LabelSet pDMD,"CScoreP2","P2 - " & FormatNumber(PlayerScore(2),0),1,""
	If PlayersPlayingGame >= 3 Then PuPlayer.LabelSet pDMD,"CScoreP3","P3 - " & FormatNumber(PlayerScore(3),0),1,""
	If PlayersPlayingGame >= 4 Then PuPlayer.LabelSet pDMD,"CScoreP4","P4 - " & FormatNumber(PlayerScore(4),0),1,""
End If
Elseif halplay=1 Then
	pupDMDDisplay "Splash4","Magna Left^EXIT MODE","",9999,1,10
End If
end Sub

Sub MajBGXP
'	PuPlayer.LabelShowPage pBackglass,1,0,""
	If SlingCount(CurrentPlayer) <= 10 Then
	PuPlayer.LabelSet pDMD,"XP0","PUPAlphas\\XP0.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP1","PUPAlphas\\XP1.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP2","PUPAlphas\\XP2.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP3","PUPAlphas\\XP3.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP4","PUPAlphas\\XP4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP5","PUPAlphas\\XP5.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP6","PUPAlphas\\XP6.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif SlingCount(CurrentPlayer) >= 11 and SlingCount(CurrentPlayer) <= 20 Then
	PuPlayer.LabelSet pDMD,"XP0","PUPAlphas\\XP0.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP1","PUPAlphas\\XP1.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP2","PUPAlphas\\XP2.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP3","PUPAlphas\\XP3.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP4","PUPAlphas\\XP4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP5","PUPAlphas\\XP5.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP6","PUPAlphas\\XP6.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif SlingCount(CurrentPlayer) >= 21 and SlingCount(CurrentPlayer) <= 30 Then
	PuPlayer.LabelSet pDMD,"XP0","PUPAlphas\\XP0.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP1","PUPAlphas\\XP1.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP2","PUPAlphas\\XP2.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP3","PUPAlphas\\XP3.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP4","PUPAlphas\\XP4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP5","PUPAlphas\\XP5.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP6","PUPAlphas\\XP6.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif SlingCount(CurrentPlayer) >= 31 and SlingCount(CurrentPlayer) <= 40 Then
	PuPlayer.LabelSet pDMD,"XP0","PUPAlphas\\XP0.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP1","PUPAlphas\\XP1.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP2","PUPAlphas\\XP2.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP3","PUPAlphas\\XP3.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP4","PUPAlphas\\XP4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP5","PUPAlphas\\XP5.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP6","PUPAlphas\\XP6.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif SlingCount(CurrentPlayer) >= 41 and SlingCount(CurrentPlayer) <= 50 Then
	PuPlayer.LabelSet pDMD,"XP0","PUPAlphas\\XP0.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP1","PUPAlphas\\XP1.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP2","PUPAlphas\\XP2.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP3","PUPAlphas\\XP3.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP4","PUPAlphas\\XP4.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP5","PUPAlphas\\XP5.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP6","PUPAlphas\\XP6.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif SlingCount(CurrentPlayer) >= 51 and SlingCount(CurrentPlayer) <= 70 Then
	PuPlayer.LabelSet pDMD,"XP0","PUPAlphas\\XP0.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP1","PUPAlphas\\XP1.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP2","PUPAlphas\\XP2.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP3","PUPAlphas\\XP3.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP4","PUPAlphas\\XP4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP5","PUPAlphas\\XP5.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP6","PUPAlphas\\XP6.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif SlingCount(CurrentPlayer) >= 71 Then
	PuPlayer.LabelSet pDMD,"XP0","PUPAlphas\\XP0.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP1","PUPAlphas\\XP1.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP2","PUPAlphas\\XP2.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP3","PUPAlphas\\XP3.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP4","PUPAlphas\\XP4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP5","PUPAlphas\\XP5.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"XP6","PUPAlphas\\XP6.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	End If
End Sub
	'********************  pretty much only use pupDMDDisplay all over ************************   
	' Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
	' pEventID = reference if application,  
	' pText = "text to show" separate lines by ^ in same string
	' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
	' also global variable useDMDVideos=true/false if user wishes only TEXT
	' TimeSec how long to display msg in Seconds
	' animation if any 0=none 1=Flasher
	' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345
	'Samples
	'pupDMDDisplay "shoot", "SHOOT AGAIN!", ", 3, 1, 10 
	'pupDMDDisplay "default", "DATA GADGET LIT", "@DataGadgetLit.mp4", 3, 1, 10
	'pupDMDDisplay "shoot", "SHOOT AGAIN!", "@shootagain.mp4", 3, 1, 10   
	'pupDMDDisplay "balllock", "Ball^Locked|16744448", "", 5, 1, 10             '  5 seconds,  1=flash, 10=priority, ball is first line, locked on second and locked has custom color |
	'pupDMDDisplay "balllock","Ball 2^is^Locked", "balllocked2.mp4",3, 1,10     '  3 seconds,  1=flash, play balllocked2.mp4 from dmdsplash folder, 
	'pupDMDDisplay "balllock","Ball^is^Locked", "@balllocked.mp4",3, 1,10       '  3 seconds,  1=flash, play @balllocked.mp4 from dmdsplash folder, because @ text by default is hidden unless useDmDvideos is disabled.


	'pupDMDDisplay "shownum", "3^More To|616744448^GOOOO", "", 5, 1, 10         ' "shownum" is special.  layout is line1=BIG NUMBER and line2,line3 are side two lines.  "4^Ramps^Left"

	'pupDMDDisplay "target", "POTTER^110120", "blank.mp4", 10, 0, 10            ' 'target'...  first string is line,  second is 0=off,1=already on, 2=flash on for each character in line (count must match)

	'pupDMDDisplay "highscore", "High Score^AAA   2451654^BBB   2342342", "", 5, 0, 10            ' highscore is special  line1=text title like highscore, line2, line3 are fixed fonts to show AAA 123,123,123
	'pupDMDDisplay "highscore", "High Score^AAA   2451654|616744448^BBB   2342342", "", 5, 0, 10  ' sames as above but notice how we use a custom color for text |



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
    HighScoreName(1) = "AMA"
    HighScoreName(2) = "CCC"
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
			DOF 121, DOFPulse
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
'		pupDMDDisplay "-","You Got a^High Score",dmdnote,3,0,10
		pupDMDDisplay "-","You Got a^High Score","",3,0,10
		Playsound "knocker"
		HighScoreDisplayName()
		HighScorelabels	
	End Sub

	' flipper moving around the letters

	Sub EnterHighScoreKey(keycode)
		If keycode = LeftFlipperKey Then
			Playsound "fx_Previous"
				If hsletter = 0 Then
					hsletter = 26
				Else
					hsLetter = hsLetter - 1
				End If
				HighScoreDisplayName()
		End If

		If keycode = RightFlipperKey Then
			Playsound "fx_Next"
				If hsletter = 26 Then
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

sub playclear(chan)
		debug.print "play clear'd " & chan

		if chan = pMusic Then
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":0 }"
		End If

		if chan = pBackglass Then
			PuPlayer.playstop pDMD
		End If
	end Sub


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
		ChangeSong
		StartLightSeq
	End Sub

	Sub StopAttractMode()
		pDMDStartGame
		DOF 323, DOFOff   'DOF MX - Attract Mode Off
		bAttractMode = False
		LightSeqAttract.StopPlay		
	'StopSong
	End Sub


'********************* START OF PUPDMD FRAMEWORK v1.0 *************************XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'******************** DO NOT MODIFY STUFF BELOW   THIS LINE!!!! ***************XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'******************************************************************************XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'******************************************************************************XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	'
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

	'Const HasDMD = True   'dont set to false as it will break pup

	'pages
	Const pDMDBlank=0
	Const pScores=1
	Const pBigLine=2
	Const pThreeLines=3
	Const pTwoLines=4
	Const pTargerLetters=5

	'dmdType
	Const pDMDTypeLCD=0
	Const pDMDTypeReal=1
	Const pDMDTypeFULL=2


	dim PUPDMDObject  'for realtime mirroring.
	Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
	Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
	Dim PBackglassCurPage: PBackglassCurPage= 0     'default page is empty.
	Dim pInAttract : pInAttract=false   'pAttract mode


	'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
	Sub PuPInit

	if (PuPDMDDriverType=pDMDTypeReal) and (useRealDMDScale=1) Then 
		   PuPlayer.setScreenEx pDMD,0,0,128,32,0  'if hardware set the dmd to 128,32
	End if

	PuPlayer.LabelInit pDMD


	if PuPDMDDriverType=pDMDTypeReal then
	Set PUPDMDObject = CreateObject("PUPDMDControl.DMD") 
	PUPDMDObject.DMDOpen
	PUPDMDObject.DMDPuPMirror
	PUPDMDObject.DMDPuPTextMirror
	PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
	PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":32, ""FQ"":3 }"   'set no antialias on font render if real
	END IF


	pSetPageLayouts

	pDMDSetPage(pDMDBlank)   'set blank text overlay page.
	pDMDStartUP    ' firsttime running for like an startup video...
	End Sub 'end PUPINIT



	'PinUP Player DMD Helper Functions

	Sub pDMDLabelHide(labName)
	PuPlayer.LabelSet pDMD,labName,"",0,""   
	end sub

	Sub pDMDScrollBig(msgText,timeSec,mColor)
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
	end sub

	Sub pDMDScrollBigV(msgText,timeSec,mColor)
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
	end sub

	Sub pDMDSplashScore(msgText,timeSec,mColor)
	PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
	end Sub

	Sub pDMDSplashScoreScroll(msgText,timeSec,mColor)
	PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
	end Sub

	Sub pDMDZoomBig(msgText,timeSec,mColor)  'new Zoom
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':3,'hstart':5,'hend':80,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
	end sub

	Sub pDMDTargetLettersInfo(msgText,msgInfo, timeSec)  'msgInfo = '0211'  0= layer 1, 1=layer 2, 2=top layer3.
	'this function is when you want to hilite spelled words.  Like B O N U S but have O S hilited as already hit markers... see example.
	PuPlayer.LabelShowPage pDMD,5,timeSec,""  'show page 5
	Dim backText
	Dim middleText
	Dim flashText
	Dim curChar
	Dim i
	Dim offchars:offchars=0
	Dim spaces:spaces=" "  'set this to 1 or more depends on font space width.  only works with certain fonts
							  'if using a fixed font width then set spaces to just one space.

	For i=1 To Len(msgInfo)
		curChar="" & Mid(msgInfo,i,1)
		if curChar="0" Then
				backText=backText & Mid(msgText,i,1)
				middleText=middleText & spaces
				flashText=flashText & spaces          
				offchars=offchars+1
		End If
		if curChar="1" Then
				backText=backText & spaces
				middleText=middleText & Mid(msgText,i,1)
				flashText=flashText & spaces
		End If
		if curChar="2" Then
				backText=backText & spaces
				middleText=middleText & spaces
				flashText=flashText & Mid(msgText,i,1)
		End If   
	Next 

	if offchars=0 Then 'all litup!... flash entire string
	   backText=""
	   middleText=""
	   FlashText=msgText
	end if  

	PuPlayer.LabelSet pDMD,"Back5"  ,backText  ,1,""
	PuPlayer.LabelSet pDMD,"Middle5",middleText,1,""
	PuPlayer.LabelSet pDMD,"Flash5" ,flashText ,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & "}"   
	end Sub


	Sub pDMDSetPage(pagenum)    
		PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
		PDMDCurPage=pagenum
	end Sub

	Sub pBackglassSetPage(pagenum)    
		PuPlayer.LabelShowPage pBackglass,pagenum,0,""   'set page to blank 0 page if want off
		PBackglassCurPage=pagenum
	end Sub

	Sub pHideOverlayText(pDisp)
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& pDisp &", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
	end Sub



	Sub pDMDShowLines3(msgText,msgText2,msgText3,timeSec)
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,3,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash3a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash3b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Splash3c",msgText3,vis,pLine3Ani
	end Sub

	dim msg1
	dim msg2

	Sub pDMDShowLines2(msgText,msgText2,timeSec)
	Dim vis:vis=1
	'msg1=msgText
	'msg2=msgText2
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,4,timeSec,""
	'vpmtimer.addtimer 500, "showtext '"
	'dim endtime:endtime=(timesec*1000)-400
	'vpmtimer.addtimer endtime, "hidetext '"
	PuPlayer.LabelSet pDMD,"Splash4a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash4b",msgText2,vis,pLine2Ani
	end Sub

	'need to make a timer and add and remove to it instead of showtext and hide text

	sub showtext
		PuPlayer.LabelSet pDMD,"Splash4a",msg1,1,0
		PuPlayer.LabelSet pDMD,"Splash4b",msg2,1,0
	end Sub

	sub hidetext
		PuPlayer.LabelSet pDMD,"Splash4a","",1,0
		PuPlayer.LabelSet pDMD,"Splash4b","",1,0
	end Sub

	Sub pDMDShowCounter(msgText,msgText2,msgText3,timeSec)
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,6,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash6a",msgText,vis, pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash6b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Splash6c",msgText3,vis,pLine3Ani
	end Sub


	Sub pDMDShowBig(msgText,timeSec, mColor)
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,vis,pLine1Ani
	end sub


	Sub pDMDShowHS(msgText,msgText2,msgText3,timeSec) 'High Score
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,7,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash7a",msgText,vis,pLine1Ani
'	PuPlayer.LabelSet pDMD,"Splash7b",msgText2,vis,pLine2Ani
'	PuPlayer.LabelSet pDMD,"Splash7c",msgText3,vis,pLine3Ani
	PuPlayer.LabelSet pDMD,"Splash7b","1- " & HighScoreName(0) &"->"& HighScore(0),vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Splash7c","2- " & HighScoreName(1) &"->"& HighScore(1),vis,pLine3Ani
'	PuPlayer.LabelSet pDMD,"Splash7b","1- " & hsn0 &"->"& hs0,vis,pLine2Ani
'	PuPlayer.LabelSet pDMD,"Splash7c","2- " & hsn1 &"->"& hs1,vis,pLine3Ani
	end Sub


	Sub pDMDSetBackFrame(fname)
	  PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
	end Sub

	Sub pDMDStartBackLoop(fPlayList,fname)
	  PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
	  PuPlayer.SetBackGround pDMD,1
	end Sub

	Sub pDMDStopBackLoop
	  PuPlayer.SetBackGround pDMD,0
	  PuPlayer.playstop pDMD
	end Sub


	Dim pNumLines

	'Theme Colors for Text (not used currenlty,  use the |<colornum> in text labels for colouring.
	Dim SpecialInfo
	Dim pLine1Color : pLine1Color=8454143  
	Dim pLine2Color : pLine2Color=8454143
	Dim pLine3Color :  pLine3Color=8454143
	Dim curLine1Color: curLine1Color=pLine1Color  'can change later
	Dim curLine2Color: curLine2Color=pLine2Color  'can change later
	Dim curLine3Color: curLine3Color=pLine3Color  'can change later


	Dim pDMDCurPriority: pDMDCurPriority =-1
	Dim pDMDDefVolume: pDMDDefVolume = 0   'default no audio on pDMD

	Dim pLine1
	Dim pLine2
	Dim pLine3
	Dim pLine1Ani
	Dim pLine2Ani
	Dim pLine3Ani

	Dim PriorityReset:PriorityReset=-1
	DIM pAttractReset:pAttractReset=-1
	DIM pAttractBetween: pAttractBetween=2000 '1 second between calls to next attract page
	DIM pDMDVideoPlaying: pDMDVideoPlaying=false


	'************************ where all the MAGIC goes,  pretty much call this everywhere  ****************************************
	'*************************                see docs for examples                ************************************************
	'****************************************   DONT TOUCH THIS CODE   ************************************************************
	'dim notenow:notenow = dmdnote
	Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
	' pEventID = reference if application,  
	' pText = "text to show" separate lines by ^ in same string
	' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
	' also global variable useDMDVideos=true/false if user wishes only TEXT
	' TimeSec how long to display msg in Seconds
	' animation if any 0=none 1=Flasher
	' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345

	'if dmdnote = notenow Then
	'	VideoName = dmdver &"-shortnote2.mp4"
	'end if
	
	'notenow = VideoName	

	DIM curPos
	if pDMDCurPriority>=pPriority then Exit Sub  'if something is being displayed that we don't want interrupted.  same level will interrupt.
	pDMDCurPriority=pPriority
	if timeSec=0 then timeSec=1 'don't allow page default page by accident


	pLine1=""
	pLine2=""
	pLine3=""
	pLine1Ani=""
	pLine2Ani=""
	pLine3Ani=""


	if pAni=1 Then  'we flashy now aren't we
	pLine1Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	pLine2Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	pLine3Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	end If

	curPos=InStr(pText,"^")   'Lets break apart the string if needed
	if curPos>0 Then 
	   pLine1=Left(pText,curPos-1) 
	   pText=Right(pText,Len(pText) - curPos)
	   
	   curPos=InStr(pText,"^")   'Lets break apart the string
	   if curPOS>0 Then
		  pLine2=Left(pText,curPos-1) 
		  pText=Right(pText,Len(pText) - curPos)

		  curPos=InStr("^",pText)   'Lets break apart the string   
		  if curPos>0 Then
			 pline3=Left(pText,curPos-1) 
		  Else 
			if pText<>"" Then pline3=pText 
		  End if 
	   Else 
		  if pText<>"" Then pLine2=pText
	   End if    
	Else 
	  pLine1=pText  'just one line with no break 
	End if


	'lets see how many lines to Show
	pNumLines=0
	if pLine1<>"" then pNumLines=pNumlines+1
	if pLine2<>"" then pNumLines=pNumlines+1
	if pLine3<>"" then pNumLines=pNumlines+1

	if pDMDVideoPlaying and (VideoName="") Then 
				PuPlayer.playstop pDMD
				pDMDVideoPlaying=False
	End if


	if (VideoName<>"") and (useDMDVideos) Then  'we are showing a splash video instead of the text.
		
		PuPlayer.playlistplayex pDMD,"DMDSplash",VideoName,pDMDDefVolume,pPriority  'should be an attract background (no text is displayed)
		pDMDVideoPlaying=true
	end if 'if showing a splash video with no text


	if StrComp(pEventID,"shownum",1)=0 Then              'check eventIDs
		pDMDShowCounter pLine1,pLine2,pLine3,timeSec
	Elseif StrComp(pEventID,"target",1)=0 Then              'check eventIDs
		pDMDTargetLettersInfo pLine1,pLine2,timeSec
	Elseif StrComp(pEventID,"highscore",1)=0 Then              'check eventIDs
		pDMDShowHS pLine1,pLine2,pline3,timeSec
	Elseif (pNumLines=3) Then                'depends on # of lines which one to use.  pAni=1 will flash.
		pDMDShowLines3 pLine1,pLine2,pLine3,TimeSec
	Elseif (pNumLines=2) Then
		pDMDShowLines2 pLine1,pLine2,TimeSec
	Elseif (pNumLines=1) Then
		pDMDShowBig pLine1,timeSec, curLine1Color
	Else
		pDMDShowBig pLine1,timeSec, curLine1Color
	End if

	PriorityReset=TimeSec*1000
	End Sub 'pupDMDDisplay message

	Sub pupDMDupdate_Timer()
		If B2SOn Then
			For i = 1 to PlayersPlayingGame
				Controller.B2SSetScorePlayer i, PlayerScore(i)
			Next
		End If
		pDMDUpdateScores 
		uDMDScoreUpdate
		
		if PriorityReset>0 Then  'for splashes we need to reset current prioirty on timer
		   PriorityReset=PriorityReset-pupDMDUpdate.interval
		   if PriorityReset<=0 Then 
				pDMDCurPriority=-1            
				if pInAttract then pAttractReset=pAttractBetween ' pAttractNext  call attract next after 1 second HP no attractmode
				pDMDVideoPlaying=false
				End if
		End if

		if pAttractReset>0 Then  'for splashes we need to reset current prioirty on timer
		   pAttractReset=pAttractReset-pupDMDUpdate.interval
		   if pAttractReset<=0 Then 
				pAttractReset=-1            
				if pInAttract then pAttractNext
				End if
		end if 
	End Sub

	Sub PuPEvent(EventNum)
	if hasPUP=false then Exit Sub
	PuPlayer.B2SData "E"&EventNum,1  'send event to puppack driver  
	End Sub

DIM VRThings
DIM VRRoom

Sub LoadVRRoom
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
	for each VRThings in VR_Mega:VRThings.visible = 0:Next
	VR_Billboards_D4_Billboards_D4_.StopAnim
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
		VR_Billboards_D4_Billboards_D4_.PlayAnimEndless(0.4)
		VR_Fog_far_Fog_0_001.PlayAnimEndless(0.05)
		VR_Fog_near_Fog_0_001.PlayAnimEndless(0.05)
	End If
End Sub


'*****begin HAL playing against himself************

sub trigger018_hit	
If Activeball.VelY < -5 Then Exit Sub
        Trigger020.Enabled = True
        Trigger023.Enabled = True
LeftFlipper.RotateToEnd
LeftFlipper001.RotateToEnd
' Flipper Sound Trigger 1
Dim flipSounds1, flipPick1
flipSounds1 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick1 = Int(Rnd * (UBound(flipSounds1) + 1))
PlaySound flipSounds1(flipPick1)
timer065.enabled=1
end Sub

sub timer065_timer
LeftFlipper.RotateToStart
LeftFlipper001.RotateToStart
if ballsonplayfield > 1 Then
Timer065.interval=35
end if
if ballsonplayfield = 1 Then
Timer065.interval=100
end if
PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
timer065.enabled=0
end Sub

sub trigger023_hit	
trigger018.enabled=1
Trigger019.enabled=1
Trigger023.enabled=0	
LeftFlipper.RotateToEnd
LeftFlipper001.RotateToEnd
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
LeftFlipper001.RotateToEnd
' Flipper Sound Trigger 3
Dim flipSounds3, flipPick3
flipSounds3 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick3 = Int(Rnd * (UBound(flipSounds3) + 1))
PlaySound flipSounds3(flipPick3)

timer065.enabled=1
end Sub

sub trigger024_hit		
LeftFlipper.RotateToEnd
LeftFlipper001.RotateToEnd
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
PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
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

'*****MOVE BULLETS

Dim Bullet1ON
Bullet1ON = 0

Sub ResetPosBullets
Bullet1ON = 0
Primitive019.TransX = 0
Primitive019.TransY = 0
Primitive019.RotZ = 30
Primitive054.TransX = 0
Primitive054.TransY = 0
Primitive054.RotZ = 25
Primitive041.TransX = 0
Primitive041.TransY = 0
Primitive041.RotZ = -90
Primitive036.TransX = 0
Primitive036.TransY = 0
Primitive036.RotZ = 70
Bullet1ONTimer.enabled=False
End Sub

Sub ShakeBullets
	Bullet1ONTimer.enabled=True
End Sub

Sub Bullet1ONTimer_TImer()
	If Bullet1ON = 0 Then
	Primitive019.TransX = Primitive019.TransX -6.4
	Primitive019.TransY = Primitive019.TransY +11.36
	Primitive019.RotZ = Primitive019.RotZ -8	
	Primitive036.RotZ = Primitive036.RotZ +2.85
	Primitive036.TransX = Primitive036.TransX +2.85
		If Primitive019.RotZ = -26 Then 'Stat Arrivée 1
			Bullet1ONTimer.enabled=False
			Playsound "fx_collide"
			Bullet1ON=1
		End IF
	Elseif Bullet1ON = 1 Then
	Primitive019.TransY = Primitive019.TransY -5
	Primitive019.RotZ = Primitive019.RotZ -1
	Primitive054.RotZ = Primitive054.RotZ +1
	Primitive054.TransY = Primitive054.TransY -1
	Primitive041.TransX = Primitive041.TransX +1
	Primitive036.TransX = Primitive036.TransX -1.5
		If Primitive019.RotZ = -30 Then 'Stat Arrivée 2 HAUT
			Bullet1ONTimer.enabled=True
			Playsound "fx_collide"
			Bullet1ON=2
		End If
	Elseif Bullet1ON = 2 Then
	Primitive019.TransY = Primitive019.TransY +20
	Primitive019.RotZ = Primitive019.RotZ +4
	Primitive054.RotZ = Primitive054.RotZ -4
	Primitive054.TransY = Primitive054.TransY +4
	Primitive041.TransX = Primitive041.TransX -4
	Primitive036.TransX = Primitive036.TransX +6
		If Primitive019.RotZ = -26 Then 'Stat Arrivée 2 BAS
			Bullet1ONTimer.enabled=False
			Bullet1ON=1
			Playsound "fx_collide"
		End If
	End If 
End Sub

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