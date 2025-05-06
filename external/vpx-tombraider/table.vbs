' ooooooooooooo   .oooooo.   ooo        ooooo oooooooooo.     ooooooooo.         .o.       ooooo oooooooooo.   oooooooooooo ooooooooo.   
' 8'   888   `8  d8P'  `Y8b  `88.       .888' `888'   `Y8b    `888   `Y88.      .888.      `888' `888'   `Y8b  `888'     `8 `888   `Y88. 
'      888      888      888  888b     d'888   888     888     888   .d88'     .8"888.      888   888      888  888          888   .d88' 
'      888      888      888  8 Y88. .P  888   888oooo888'     888ooo88P'     .8' `888.     888   888      888  888oooo8     888ooo88P'  
'      888      888      888  8  `888'   888   888    `88b     888`88b.      .88ooo8888.    888   888      888  888    "     888`88b.    
'      888      `88b    d88'  8    Y     888   888    .88P     888  `88b.   .8'     `888.   888   888     d88'  888       o  888  `88b.  
'     o888o      `Y8bood8P'  o8o        o888o o888bood8P'     o888o  o888o o88o     o8888o o888o o888bood8P'   o888ooooood8 o888o  o888o 
'
' ****************************************************************
'                       VISUAL PINBALL X
' ****************************************************************
'***********************************
' // User Settings in F12 menu //   
'***********************************
Dim ModeChangeBallActive

Sub Table1_OptionEvent(ByVal eventId)
	' -- pause processes not needed to run --
    If eventId = 1 Then DisableStaticPreRendering = True
	DMDTimer.Enabled = False ' stop FlexDMD timer
	Controller.Pause = True

	ModeChangeBallActive = Table1.Option("ChangeColorBall", 0, 1, 1, 0, 0, Array("Yes","No"))

	' -- start paused processes --
	DMDTimer.Enabled = True
	Controller.Pause = False
    If eventId = 3 Then DisableStaticPreRendering = False
	
End Sub
'**************************
'   PinUp Player USER Config
'**************************
    dim usePuPDMD       : usePuPDMD=true       ' set to false to not use PuPDMD for a DMD (different that BG scoring)
	dim PuPDMDDriverType: PuPDMDDriverType=0   ' NO CHANGE THIS OPTION PLZ 0=LCD DMD, 1=RealDMD 2=FULLDMD (large/High LCD)
	dim useRealDMDScale : useRealDMDScale=0    ' 0 or 1 for RealDMD scaling.  Choose which one you prefer.
	dim useDMDVideos    : useDMDVideos=true   ' true or false to use DMD splash videos.
	Dim pGameName       : pGameName="Tomb_Raider"    'pupvideos foldername, probably set to cGameName in realworld


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


' Define any Constants
Const cGameName = "tomb_raider"
Const TableName = "TombRaider"
Const typefont = "Tomb_Raider"
Const numberfont = "Tomb_Raider"
Const zoomfont = "Magic School One"
Const zoombgfont = "Magic School One" ' needs to be an outline of the zoomfont
Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 15 ' in seconds
Const MaxMultiplier = 3  ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 3  ' usually 3 or 5
Const MaxMultiballs = 6  ' max number of balls during multiballs

Const dmdalt="PKMN Pinball"
Const dmdfixed="Instruction"
Const dmdscr="Impact"    'main scorefont
Const dmddef="Zig"
Const dmdtomb="Tomb_Raider"

Const Special1 = 1000000  ' High score to obtain an extra ball/game
Const Special2 = 3000000
Const Special3 = 5000000


' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusMultiplier(4)
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
Dim BallsInLock
Dim BallsInHole
Dim BOT
' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bModeEgyptActive
Dim bModeIncaActive
Dim bModeSouterrainActive
Dim bModeMiniGameActive
Dim bModePbgActive
Dim MBLock_Sav(4)
Dim Gems_Sav(4)
Dim Relics_Sav(4)
Dim Mission_Relic1(4)
Dim Mission_Relic2a(4)
Dim Mission_Relic2b(4)
Dim Mission_Relic3(4)
Dim Mission_Relic4(4)
Dim Mission_Relic5(4)
Dim ModeRelic1Active(4)
Dim ModeRelic2aActive(4)
Dim ModeRelic2bActive(4)
Dim ModeRelic3Active(4)
Dim ModeRelic4Active(4)
Dim ModeRelic5Active(4)
Dim ModeRelicAllActive(4)
Dim ModeSpinnersActive(4)
Dim SpinnerCount
Dim ModeBumpersActive(4)
Dim	BumperCount(4)
Dim ModeMissionActive(4)
Dim ModeSniperShooting(4)
Dim	ExtraBCount
Dim LetterCountC(4)
Dim LetterCountR(4)
Dim LetterCountO(4)
Dim LetterCountF(4)
Dim LetterCountT(4)
Dim SniperShootingCount
Dim RampSoutCount
Dim	RampMCount
Dim	RampLCount
Dim	RampRCount
Dim bMusicOn
Dim bJustStarted
Dim bJackpot
Dim plungerIM
Dim LastSwitchHit
'Dim FlexScenes(100)				 'Array of FlexDMD scenes
Dim VRRoom
Dim FlexOnPlayfield
Dim tablewidth


'****************************************
'		USER OPTIONS
'****************************************

Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		' Level of ramp rolling volume. Value between 0 and 1
Dim StagedFlippers : StagedFlippers = 0         ' Staged Flippers. 0 = Disabled, 1 = Enabled

'****************************************
' core.vbs variables

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
	PUPINIT
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
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
		BallinGame(i) = 1
        ExtraBallsAwards(i) = 0
    Next

	' Adjust DMD Timer
	 pupDMDupdate.Enabled = false
	 pupDMDupdate.Interval = 1000
	 pupDMDupdate.Enabled = True

	' FlexDMD
'	Flex_Init
'	ShowScene flexScenes(1), FlexDMD_RenderMode_DMD_RGB, 2

    ' freeplay or coins
    bFreePlay = True 'we want coins

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
    BallsInLock = 0
    BallsInHole = 0
	LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bJustStarted = True
    ' set any lights for the attract mode
    GiOff
    StartAttractMode
	ResetEvents
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
	'tmp = tableobj.x * 2 / tablewidth - 1
	
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

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = CSng(BallVel(ball) ^ 2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2) ) )
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

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)

	FlipperCradleCollision ball1, ball2, velocity

	Dim snd
	Select Case Int(Rnd * 7) + 1
		Case 1
			snd = "Ball_Collide_1"
		Case 2
			snd = "Ball_Collide_2"
		Case 3
			snd = "Ball_Collide_3"
		Case 4
			snd = "Ball_Collide_4"
		Case 5
			snd = "Ball_Collide_5"
		Case 6
			snd = "Ball_Collide_6"
		Case 7
			snd = "Ball_Collide_7"
	End Select
	
	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
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
	FlipperU2LSh.RotZ = LeftFlipper002.CurrentAngle
	FlipperURSh.RotZ = RightFlipper001.CurrentAngle
	LFLogo.RotZ = LeftFlipper.CurrentAngle
	LFLogo001.RotZ = LeftFlipper001.CurrentAngle
	LFLogo002.RotZ = LeftFlipper002.CurrentAngle
	RFlogo.RotZ = RightFlipper.CurrentAngle
	RFlogo001.RotZ = RightFlipper001.CurrentAngle
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If Keycode = AddCreditKey Then
'		DOF 202, DOFPulse
        Credits = Credits + 1
		PlaySound "Coin_In_1"
		pDMDStartGame
        if bFreePlay = False Then
            DOF 125, DOFOn
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
	
	If keycode = LeftMagnaSave Then ShakeLogLON
	If keycode = RightMagnaSave Then ShakeLogRON
	
	If keycode = LeftFlipperKey And bModeSouterrainActive = False Then
		SolLFlipper True	'This would be called by the solenoid callbacks if using a ROM
'		If bModeCastelActive = True Then
		SolU2LFlipper True
'		End If
'		SolULFlipper False
		if PuPGameRunning Then PuPGameInfo= PuPlayer.GameUpdate("PupMiniGame", 1 , 83 , "")  'w
	Elseif keycode = LeftFlipperKey And bModeSouterrainActive = True Then
'		SolLFlipper False	'This would be called by the solenoid callbacks if using a ROM
		SolULFlipper True
	End If
	
	If keycode = RightFlipperKey And bModeSouterrainActive = False Then
		SolRFlipper True	'This would be called by the solenoid callbacks if using a ROM
'		SolURFlipper False
		if PuPGameRunning Then PuPGameInfo= PuPlayer.GameUpdate("PupMiniGame", 1 , 87 , "")  'w
	Elseif keycode = RightFlipperKey And bModeSouterrainActive = True Then
'		SolRFlipper False	'This would be called by the solenoid callbacks if using a ROM
		SolURFlipper True
	End If

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
					PlaySound "fx_Objet"
					resetbackglass
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
						PlaySound "fx_Objet"
						resetbackglass
                        Credits = Credits - 1
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

'table keys
'If keycode = RightMagnaSave or keycode = LeftMagnasave Then ShowPost 
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
	If keycode = LeftMagnaSave Then ShakeLogLOFF
	If keycode = RightMagnaSave Then ShakeLogROFF

    If bGameInPLay AND NOT Tilted Then
		'pDMDStartGame
		If keycode = LeftFlipperKey Then
			SolLFlipper False   'This would be called by the solenoid callbacks if using a ROM
			SolULFlipper False
			SolU2LFlipper False
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
    Controller.Stop
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
		PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
		RightFlipper001.rotatetoend

		If RightFlipper001.currentangle > RightFlipper001.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper001
		Else 
			SoundFlipperUpAttackRight RightFlipper001
			RandomSoundFlipperUpRight RightFlipper001
		End If
	Else
		PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
		RightFlipper001.RotateToStart
		If RightFlipper001.currentangle > RightFlipper001.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper001
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolULFlipper(Enabled)
	If Enabled Then
		PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
		LeftFlipper001.rotatetoend

		If LeftFlipper001.currentangle > LeftFlipper001.endangle - ReflipAngle Then
			RandomSoundReflipUpRight LeftFlipper001
		Else 
			SoundFlipperUpAttackRight LeftFlipper001
			RandomSoundFlipperUpRight LeftFlipper001
		End If
	Else
		PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
		LeftFlipper001.RotateToStart
		If LeftFlipper001.currentangle > LeftFlipper001.startAngle + 5 Then
			RandomSoundFlipperDownRight LeftFlipper001
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolU2LFlipper(Enabled)
	If Enabled Then
		LeftFlipper002.rotatetoend

		If LeftFlipper002.currentangle > LeftFlipper002.endangle - ReflipAngle Then
			RandomSoundReflipUpRight LeftFlipper002
		Else 
			SoundFlipperUpAttackRight LeftFlipper002
			RandomSoundFlipperUpRight LeftFlipper002
		End If
	Else
		LeftFlipper002.RotateToStart
		If LeftFlipper002.currentangle > LeftFlipper002.startAngle + 5 Then
			RandomSoundFlipperDownRight LeftFlipper002
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
			'If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
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
		'If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & "actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		'If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
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
'Sub RDampen_Timer
'	Cor.Update
'End Sub

Sub CorTimer_Timer()
	Cor.Update
End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************


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
        'DMD "_", CL(1, "CAREFUL!"), "", eNone, eBlinkFast, eNone, 500, True, ""
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        'DMDFlush
		DOF 400, DOFPulse
        pupDMDDisplay "attract","Extraball","@Tilt.mp4",3,0,20
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
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        'DMDFlush
    End If
End Sub

' GI light sequence effects

'Sub GiEffect(n)
'    Select Case n
'        Case 0 'all blink
'            LightSeqGi.UpdateInterval = 8
'            LightSeqGi.Play SeqBlinking, , 5, 50
'        Case 1 'random
'            LightSeqGi.UpdateInterval = 10
'            LightSeqGi.Play SeqRandom, 5, , 1000
'        Case 2 'upon
'            LightSeqGi.UpdateInterval = 4
'            LightSeqGi.Play SeqUpOn, 5, 1
'    End Select
'End Sub
'
'Sub LightEffect(n)
'    Select Case n
'        Case 0 'all blink
'            LightSeqInserts.UpdateInterval = 8
'            LightSeqInserts.Play SeqBlinking, , 5, 50
'        Case 1 'random
'            LightSeqInserts.UpdateInterval = 10
'            LightSeqInserts.Play SeqRandom, 5, , 1000
'        Case 2 'upon
'            LightSeqInserts.UpdateInterval = 4
'            LightSeqInserts.Play SeqUpOn, 10, 1
'        Case 3 ' left-right-left
'            LightSeqInserts.UpdateInterval = 5
'            LightSeqInserts.Play SeqLeftOn, 10, 1
'            LightSeqInserts.UpdateInterval = 5
'            LightSeqInserts.Play SeqRightOn, 10, 1
'    End Select
'End Sub

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
        PlaySong "Mu_MB"
    Else
        UpdateMusicNow
    end if
End Sub

'if you add more balls to the game use changesong then if bMultiBallMode = true, your multiball song will be played.

Sub UpdateMusicNow
    Select Case UpdateMusic
        Case 0:PlaySong "Mu_1"
        Case 1:PlaySong "Mu_1"
        Case 2:PlaySong "Mu_1"
        Case 3:PlaySong "Mu_1"
        Case 4:PlaySong "M_end"
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

'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
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
'	table1.ColorGradeImage = "ColorGradeLUT256x16_1to1"
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
'	table1.ColorGradeImage = "ColorGradeLUT256x16_1to2"
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

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
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
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

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

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
	'Multiball=false	
    For i = 1 To MaxPlayers
        PlayerScore(i) = 0
        BonusPoints(i) = 0
		'BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
		BallinGame(i) = 1
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
    Next

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
PlaySound ""


    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to

' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
	ResetEvents
	PlaySound "Vo_Start"
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
    BonusMultiplier(CurrentPlayer) = 1
    'UpdateBonusXLights
	
' reset any drop targets, lights, game Mode etc..
	CheckLightMission
    
   'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'Reset any table specific
'	BumperBonus = 0
'	HoleBonus = 0
'	ALLRampBonus = 0
'	RampBonus1 = 0
'	RampBonus2 = 0
'	RampBonus3 = 0
	JackpotsBonus = 0
	RaiderBonus = 0
	CroftBonus = 0
	RelicsBonus = 0
	ArtefactsBonus = 0
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

 '   If BallsOnPlayfield> 2 Then
 '       bMultiBallMode = True
 '       bAutoPlunger = True
 '       'ChangeSong
 '   End If

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
	PriorityReset = 0
	pDMDSetPage(pScores) 
	'bonuscheckie

    Dim AwardPoints, TotalBonus, ii
	AwardPoints = 0
    TotalBonus = 0

    'If NOT Tilted Then
	If(Tilted = False) Then
		
        'Bonus
		BandeauBGOFF
		PuPEvent 120
		PuPlayer.LabelShowPage pBackglass,1,0,""
	
'		PuPlayer.LabelSet pBackglass,"Bonus1", FormatNumber(JackpotsBonus,0),1,"{'mt':2,'color':13264128, 'size': 4 }"
		AwardPoints = JackpotsBonus * 5000
       TotalBonus = TotalBonus + AwardPoints

'		PuPlayer.LabelSet pBackglass,"Bonus2", FormatNumber(RaiderBonus,0),1,"{'mt':2,'color':55660, 'size': 4 }"
		AwardPoints = RaiderBonus * 2000
       TotalBonus = TotalBonus + AwardPoints

'		PuPlayer.LabelSet pBackglass,"Bonus3", FormatNumber(CroftBonus,0),1,"{'mt':2,'color':3739322, 'size': 4 }"
		AwardPoints = CroftBonus * 2000
       TotalBonus = TotalBonus + AwardPoints

'		PuPlayer.LabelSet pBackglass,"Bonus4", FormatNumber(RelicsBonus,0),1,"{'mt':2,'color':25542, 'size': 4 }"
		AwardPoints = RelicsBonus * 15000
       TotalBonus = TotalBonus + AwardPoints

'		PuPlayer.LabelSet pBackglass,"Bonus5", FormatNumber(ArtefactsBonus,0),1,"{'mt':2,'color':5602052, 'size': 4 }"
		AwardPoints = ArtefactsBonus * 10000
       TotalBonus = TotalBonus + AwardPoints
        
		PuPlayer.LabelSet pBackglass,"BonusTotal", FormatNumber(TotalBonus,0),1,"{'mt':2,'color':16777215, 'size': 5 }"	
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
       
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
            LightShootAgain.State = 0
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
'		PlaySound "Mu_End"
'		PuPEvent 118
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
	SolU2LFlipper 0
	

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
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
	PuPEvent 101
	DOF 132, DOFPulse
	pupDMDDisplay "Splash4","Ball^SAVED","",3,1,10
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
					PuPEvent 124
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
					If ModeRelic1Active(CurrentPlayer) = True or ModeRelic4Active(CurrentPlayer) = True or ModeRelic5Active(CurrentPlayer) = True Then
					StopSong
					PlaySong "Mu_2"
					Elseif ModeRelic2aActive(CurrentPlayer) = True or ModeRelic2bActive(CurrentPlayer) = True or ModeRelic3Active(CurrentPlayer) = True Then
					StopSong
					PlaySong "Mu_3"
					Else
					ChangeSong
					End If
                End If
            End If
            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then

                ' End Mode and timers
				ResetLightRelic
				RampMCount = 0
				RampLCount = 0
				GunSeqENDTimer.Enabled = 1
				RampRCount = 0
				Wall048.Collidable = 0
				StopSong
				PlaySound "Vo_End"
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
 '       DOF 121, DOFPulse
        PlaySoundAt "fx_fire", Trigger1
        bAutoPlunger = False
    End If	
'StopSong

    bBallInPlungerLane = True
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
'        EnableBallSaver BallSaverTime
		 DOF 310, DOFon
		 LiSkillShot.state = 2
		 LiSKILL.state = 1
'        Else
'        ' show the message to shoot the ball in case the player has fallen sleep
'        Trigger1.TimerEnabled = 1
    End If
End Sub

Sub Gate001_Hit()
	AddScore 25
	DOF 310, DOFoff
	If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then 
        EnableBallSaver BallSaverTime
		ResetSkillShotTimer.Enabled = True
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

Sub ResetSkillShotTimer_Timer()
	LiSkillShot.state = 0
	LiSKILL.state = 0
	ResetSkillShotTimer.Enabled = False
End Sub
Sub ResetSuperSkillShotTimer_Timer()
	LiSuperSkillShot.state = 0
	LiSKILL.state = 0
	ResetSuperSkillShotTimer.Enabled = False
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
Sub AddScore(points)
    If Tilted Then Exit Sub

    ' add the points to the current players score variable
    PlayerScore(CurrentPlayer) = PlayerScore(CurrentPlayer) + points

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
	DOF 121, DOFPulse
	DOF 451, DOFPulse
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
'	ALLRampBonus = 0
'	RampBonus1 = 0
'	RampBonus2 = 0
'	RampBonus3 =0
	JackpotsBonus = 0
	RaiderBonus = 0
	CroftBonus = 0
	RelicsBonus = 0
	ArtefactsBonus = 0
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
    'TurnOffPlayfieldLights
    'li025.State = 1
    'li021.State = 1
    'li022.State = 1
    'li023.State = 1
    'li024.State = 1
	'li033.state = 1
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
Dim RStep, LStep

Sub RightSlingShot_Slingshot
'	PlaySoundAt SoundFXDOF("fx_slingshot", 104, DOFPulse, DOFcontactors), Sling1
	PlaySoundAt SoundFXDOF("Sling_L", 104, DOFPulse, DOFcontactors), Sling1
    DOF 111, DOFPulse
	RS.VelocityCorrect(ActiveBall)
	Addscore 51
	FlashForMs Flasher011, 500, 250, 0
	FlashForMs Light_Folder, 500, 250, 0 
	FlashForMs Light_Folder2, 500, 250, 0 
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
'	PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Sling2
	PlaySoundAt SoundFXDOF("Sling_R", 103, DOFPulse, DOFcontactors), Sling2
    DOF 110, DOFPulse
	LS.VelocityCorrect(ActiveBall)
	Addscore 51
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

FlInitBumper 1, "blacklight"
FlInitBumper 2, "blacklight"
FlInitBumper 3, "blacklight"

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
		AddScore 450
		If ModeBumpersActive(CurrentPlayer) = true Then
		BumperCount(CurrentPlayer) = BumperCount(CurrentPlayer) - 1
		DispXText BumperCount(CurrentPlayer)
'		dmdflush
'		DMD "SHOOT BUMPERS", FormatScore(BumperCount), "Bumper_000", eNone, eNone, eNone, 3000, False, ""
		If BumperCount(CurrentPlayer) = 0 Then
			Mode_Relic1_end
		end If
	End If
		FlBumperFadeTarget(1) = 0
		bumper1.timerinterval = 100
		Bumper1.timerenabled = True
	End If
End Sub

Sub Bumper2_Hit()
'	CheckBumpers
	If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 110, DOFPulse, DOFContactors), Bumper2
        DOF 139, DOFPulse
		GiEffect 4
		AddScore 450
		If ModeBumpersActive(CurrentPlayer) = true Then
		BumperCount(CurrentPlayer) = BumperCount(CurrentPlayer) - 1
		DispXText BumperCount(CurrentPlayer)
'		dmdflush
'		DMD "SHOOT BUMPERS", FormatScore(BumperCount), "Bumper_000", eNone, eNone, eNone, 3000, False, ""
		If BumperCount(CurrentPlayer) = 0 Then
			Mode_Relic1_end
		end If
	End If
		FlBumperFadeTarget(2) = 0
		bumper2.timerinterval = 100
		Bumper2.timerenabled = True
	End If
End Sub

Sub Bumper3_Hit()
'	CheckBumpers
	If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 111, DOFPulse, DOFContactors), Bumper3
        DOF 140, DOFPulse
		GiEffect 4
		AddScore 450
		If ModeBumpersActive(CurrentPlayer) = true Then
		BumperCount(CurrentPlayer) = BumperCount(CurrentPlayer) - 1
		DispXText BumperCount(CurrentPlayer)
'		dmdflush
'		DMD "SHOOT BUMPERS", FormatScore(BumperCount), "Bumper_000", eNone, eNone, eNone, 3000, False, ""
		If BumperCount(CurrentPlayer) = 0 Then
			Mode_Relic1_end
		end If
	End If
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
'ObjLevel(1) = 1 : FlasherFlash9_Timer
End Sub

'*****************
'Kickers
'*****************

Sub Kicker002_Hit()
	AddScore 550
	CheckLightMB
	ResetTargetLara
	kicker002.destroyball
	Kicker002.timerinterval = 2000
	kicker002.timerenabled = True
End Sub

Sub Kicker002_Timer()
	EnableBallSaver 15
	KickerAuto.CreateBall
	KickerAuto.Kick 10, 41
	kicker002.timerenabled = False
End Sub

Sub Kicker003_Hit()
	Playsound "fx_Gate"
	If bMultiBallMode = False And LiLOCK.state = 0 And LiMB.state = 0 Then
	Target000.IsDropped = 1 
	Target001.IsDropped = 1 
	Target002.IsDropped = 1 
	Target003.IsDropped = 1
	CheckLightLock
	Wall038.Collidable = 0
	Wall038.SideVisible = True
	Flasher004.visible = 0
	Flasher005.visible = 0
	End If
	Kicker003.timerinterval = 2000
	kicker003.timerenabled = True
	Light003.state = 2
End Sub

Sub Kicker003_Timer()
	kicker003.kick 0, 50
	kicker003.timerenabled = False
	Light003.state = 0
	Wall032.Collidable = 1
	Wall032.SideVisible = False
	Wall033.Collidable = 1
	Wall033.SideVisible = False
End Sub

Sub Kicker004_Hit()
'	If bModeMiniGameActive = True and BallsOnPlayfield <= 1 Then
'	PuPGameStartMiniGame
	AddScore 1550
	If LiMission.state = 0 and ModeRelic1Active(CurrentPlayer) = False and ModeRelic2aActive(CurrentPlayer) = False and ModeRelic2bActive(CurrentPlayer) = False and ModeRelic3Active(CurrentPlayer) = False and ModeRelic4Active(CurrentPlayer) = False and ModeRelic5Active(CurrentPlayer) = False and ModeRelicALLActive(CurrentPlayer) = False Then
	LightR001.State = 1
	LightR002.State = 1
	LightR003.State = 1
	LightR004.State = 1
	LightR005.State = 1
	LetterCountC(CurrentPlayer) = 1
	LetterCountR(CurrentPlayer) = 1
	LetterCountO(CurrentPlayer) = 1
	LetterCountF(CurrentPlayer) = 1
	LetterCountT(CurrentPlayer) = 1
	CheckTargetCroft
	Kicker004.timerinterval = 3000
	kicker004.timerenabled = True
	light004.state = 2
	Else
	Kicker004.timerinterval = 3000
	kicker004.timerenabled = True
	light004.state = 2
	End If
End Sub

Sub Kicker004_Timer()
	kicker004.kick 180, 2
	light004.state = 0
	kicker004.timerenabled = False
End Sub

Sub Kicker005_Hit()
	If LiMission.state=2 Then
	LightR001.State = 0
	LightR002.State = 0
	LightR003.State = 0
	LightR004.State = 0
	LightR005.State = 0
	LiMission.state=0
	if Mission_Relic1(CurrentPlayer) + Mission_Relic2a(CurrentPlayer) + Mission_Relic2b(CurrentPlayer) + Mission_Relic3(CurrentPlayer) + Mission_Relic4(CurrentPlayer) + Mission_Relic5(CurrentPlayer) = 6 Then
			Mode_RelicAll_Start
			Kicker005.timerinterval = 2000
			kicker005.timerenabled = True
			Light005.state = 2
			Exit Sub
		End If
		Dim ModeChosenMission
		ModeChosenMission = False
		Do While ModeChosenMission = False
			Select Case RndNbr(6)
			Case 1
				If Mission_Relic1(CurrentPlayer) = 0 And LiB001.state = 0 Then
					Mode_Relic1_start ' BUMPERS HITS
					ModeChosenMission = True
				End If
			Case 2
				If Mission_Relic2a(CurrentPlayer) = 0 And LiB002a.state = 0 Then
					Mode_Relic2a_Start ' RAMP L x3
					ModeChosenMission = True
				End If
			Case 3
				If Mission_Relic2b(CurrentPlayer) = 0 And LiB002b.state = 0 Then
					Mode_Relic2b_start ' RAMP R x3
					ModeChosenMission = True
				End If
			Case 4
				If Mission_Relic3(CurrentPlayer) = 0 And LiB003.state = 0 Then
					Mode_Relic3_Start ' ALL RAMPS/LINE x1
					ModeChosenMission = True
				End If
			Case 5
				If Mission_Relic4(CurrentPlayer) = 0 And LiB004.state = 0 Then
					Mode_Relic4_Start ' RAMP L x3 + RAMP R x3 + RAMP EXT x3
					ModeChosenMission = True
				End If
			Case 6 
				If Mission_Relic5(CurrentPlayer) = 0 And LiB005.state = 0 Then
					Mode_Relic5_Start 'Mode Spinner
					ModeChosenMission = True
				End If
			End Select
		Loop
	Else
	End If
	Kicker005.timerinterval = 2000
	kicker005.timerenabled = True
	Light005.state = 2
	AddScore 650
End Sub

Sub Kicker005_Timer()
	kicker005.kick 190, 10
	kicker005.timerenabled = False
	AddLetterCroft
	light005.state = 0
End Sub

Sub Kicker006_Hit()
	AddScore 555
	kicker006.destroyball
	Wall032.Collidable = 1
	Wall032.SideVisible = False
	Wall033.Collidable = 1
	Wall033.SideVisible = False
	If Gems_Sav(CurrentPlayer) < 5 And BallsOnPlayfield <=2 Then
	ChangeBall(2)
	BallSaverEnd
	bModeSouterrainActive = True
	PuPEvent 121
	StopSong
	PlaySong "Mu_4"
	LeftFlipper.RotateToStart
	RightFlipper.RotateToStart
	Flasher001.visible=0
	GiOff
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to2"
	l2.state=1
	l3.state=1
	l4.state=1
	l5.state=1
	l6.state=1
	l7.state=1
	Light012.color = RGB(255, 0, 0) 'red
	Light012.colorfull = RGB(255, 0, 0)
	Light013.color = RGB(255, 0, 0)
	Light013.colorfull = RGB(255, 0, 0)
	Light012.state=1
	Light013.state=1
	Kicker006.timerinterval = 4000
	kicker006.timerenabled = True
	Light006.state = 2
	Light011.state = 2
	Kicker007.CreateSizedBallWithMass BallSize / 2, BallMass
	Elseif Gems_Sav(CurrentPlayer) < 5 And BallsOnPlayfield >=3 Then
	kicker004.CreateSizedBallWithMass BallSize / 2, BallMass
	Kicker004.timerinterval = 3000
	kicker004.timerenabled = True
	Light004.state = 2
	AddScore 5000
	Elseif Gems_Sav(CurrentPlayer) >= 5 Then
	kicker004.CreateSizedBallWithMass BallSize / 2, BallMass
	Kicker004.timerinterval = 4000
	kicker004.timerenabled = True
	Light004.state = 2
	End If
End Sub

Sub Kicker006_Timer()	
	kicker007.kick 45, 5'60
	PuPEvent 104
	kicker006.timerenabled = False
	light006.state = 0
	Light011.state = 0
End Sub
'*****************
'Triggers
'*****************
Sub Trigger001_Hit() 'SklillShot
	If BallsOnPlayfield >=3 Then BallSaverEnd
	If LiSkillShot.state = 2 And ResetSkillShotTimer.Enabled = True Then
	LiSkillShot.state = 0
	LiSuperSkillShot.state = 2
	LiSKILL.state = 1
	pupDMDDisplay "attract","","@SkillShot.mp4",9,0,16
	Playsound "Vo_SkillShot"
	AddScore 25000
	ResetSuperSkillShotTimer.Enabled = True
	ResetSkillShotTimer.Enabled = False
	End If
End Sub

Sub Trigger002_Hit()
	Playsound "fx_slug"
	Ramp014.Visible = True
	Ramp014.collidable = False
End Sub

Sub Trigger003_Hit()
	Ramp014.Visible = False
	Ramp014.collidable = True
	If LiSuperSkillShot.state = 2 And ResetSuperSkillShotTimer.Enabled = True Then
	LiSuperSkillShot.state = 0
	LiSKILL.state = 0
	pupDMDDisplay "attract","","@SskillShot.mp4",8,0,17
	Playsound "Vo_SskillShot"
	AddScore 75000
	AddMBmission
	ResetSuperSkillShotTimer.Enabled = False
	End If
End Sub

Sub Kicker001_Hit()
	StopSong
	PlaySong "Mu_1"
	ChangeBall(0)
	If FlasherGEM1.visible = True or FlasherGEM2.visible = True or FlasherGEM3.visible = True or FlasherGEM4.visible = True or FlasherGEM5.visible = True Then
	PlaySound "Vo_Loose"
	End If 
	Kicker001.destroyball
	bModeSouterrainActive = False
	RampSoutCount = 0
	PuPEvent 122
	Light007.state=0
	Light008.state=0
	FlasherGEM1.visible = 0
	FlasherGEM2.visible = 0
	FlasherGEM3.visible = 0
	FlasherGEM4.visible = 0
	FlasherGEM5.visible = 0
	Light011.state=0
	LeftFlipper001.RotateToStart
	RightFlipper001.RotateToStart
	Flasher001.visible=1
	ChangeLOGO
	GiOn
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to1"
	l2.state=0
	l3.state=0
	l4.state=0
	l5.state=0
	l6.state=0
	l7.state=0
	Light012.color = RGB(255, 0, 0) 'red
	Light012.colorfull = RGB(255, 0, 0)
	Light013.color = RGB(255, 0, 0)
	Light013.colorfull = RGB(255, 0, 0)
	Light012.state=0
	Light013.state=0
	kicker004.CreateSizedBallWithMass BallSize / 2, BallMass
	Kicker004.timerinterval = 4000
	kicker004.timerenabled = True
	Light004.state = 2
End Sub
Sub Trigger006_Hit()
	If FlasherGEM1.visible = True Then
	LiA001.state=1
	Light011.state=0
	PlaySound"fx_Objet"
	PlaySound "Vo_Artefact"
	FlasherGEM1.visible = 0
	Gems_Sav(CurrentPlayer) = 1
	ArtefactsBonus = ArtefactsBonus + 1
	pupDMDDisplay "attract","artefact","@Artefact1.mp4",6,0,16
	AddMBmission
	AddScore 55000
	bModeSouterrainActive = False
	RampSoutCount = 0
	PuPEvent 122
	Elseif FlasherGEM2.visible = True Then
	LiA002.state=1
	Light011.state=0
	PlaySound"fx_Objet"
	PlaySound "Vo_Artefact"
	FlasherGEM2.visible = 0
	Gems_Sav(CurrentPlayer) = 2
	ArtefactsBonus = ArtefactsBonus + 1
	pupDMDDisplay "attract","artefact","@Artefact2.mp4",6,0,16
	AddMBmission
	AddScore 65000
	bModeSouterrainActive = False
	RampSoutCount = 0
	PuPEvent 122
	Elseif FlasherGEM3.visible = True Then
	LiA003.state=1
	Light011.state=0
	PlaySound"fx_Objet"
	PlaySound "Vo_Artefact"
	FlasherGEM3.visible = 0
	Gems_Sav(CurrentPlayer) = 3
	ArtefactsBonus = ArtefactsBonus + 1
	pupDMDDisplay "attract","artefact","@Artefact3.mp4",6,0,16
	AddMBmission
	AddScore 75000
	bModeSouterrainActive = False
	RampSoutCount = 0
	PuPEvent 122
	Elseif FlasherGEM4.visible = True Then
	LiA004.state=1
	Light011.state=0
	PlaySound"fx_Objet"
	PlaySound "Vo_Artefact"
	FlasherGEM4.visible = 0
	Gems_Sav(CurrentPlayer) = 4
	ArtefactsBonus = ArtefactsBonus + 1
	pupDMDDisplay "attract","artefact","@Artefact4.mp4",6,0,16
	AddMBmission
	AddScore 85000
	bModeSouterrainActive = False
	RampSoutCount = 0
	PuPEvent 122
	Elseif FlasherGEM5.visible = True Then
	LiA005.state=1
	Light011.state=0
	PlaySound"fx_Objet"
	PlaySound "Vo_Artefact"
	FlasherGEM5.visible = 0
	Gems_Sav(CurrentPlayer) = 5
	ArtefactsBonus = ArtefactsBonus + 1
	pupDMDDisplay "attract","artefact","@Artefact5.mp4",6,0,16
	AddMBmission
	AddScore 105000
	bModeSouterrainActive = False
	RampSoutCount = 0
	PuPEvent 122
	End If
End Sub

Sub Trigger007_Hit()
	PuPEvent 104
	FlashForMs Light012, 2000, 30, 1
	FlashForMs Light013, 2000, 30, 1
	If RampSoutCount <4 Then RampSoutCount = RampSoutCount + 1
	If RampSoutCount = 0 Then Light007.state=0 : Light008.state=0
	If RampSoutCount = 1 Then Light007.state=1 : Light008.state=0 : AddScore 2550
	If RampSoutCount = 2 Then Light007.state=1 : Light008.state=1 : AddScore 3550 : Light012.color = RGB(255, 64, 0) : Light012.colorfull = RGB(255, 64, 0) : Light013.color = RGB(255, 64, 0) : Light013.colorfull = RGB(255, 64, 0)'orange
	If RampSoutCount = 3 Then Light007.state=2 : Light008.state=1 : AddScore 4550
	If RampSoutCount = 4 Then Light007.state=2 : Light008.state=2 : AddScore 5550 : ChangeBall(1) : Light012.color = RGB(0, 255, 0) : Light012.colorfull = RGB(0, 255, 0) : Light013.color = RGB(0, 255, 0) : Light013.colorfull = RGB(0, 255, 0)'vert
	If RampSoutCount = 5 Then Light007.state=0 : Light008.state=0
End Sub

Sub StartGems
	Light011.state=1
	RampSoutCount = 5
	If Gems_Sav(CurrentPlayer) = 0 Then
	FlasherGEM1.visible = 1
	Light007.state=0
	Light008.state=0
	Elseif Gems_Sav(CurrentPlayer) = 1 Then
	FlasherGEM2.visible = 1
	Light007.state=0
	Light008.state=0
	Elseif Gems_Sav(CurrentPlayer) = 2 Then
	FlasherGEM3.visible = 1
	Light007.state=0
	Light008.state=0
	Elseif Gems_Sav(CurrentPlayer) = 3 Then
	FlasherGEM4.visible = 1
	Light007.state=0
	Light008.state=0
	Elseif Gems_Sav(CurrentPlayer) = 4 Then
	FlasherGEM5.visible = 1
	Light007.state=0
	Light008.state=0
	End If
End Sub

'***********
' Target
'***********
Sub Target000_Hit()
	AddScore 250
	PlaySound "fx_target"
	LiLARA001.state = 1
	DOF 334, DOFPulse
	If Target001.IsDropped = 1 And Target002.IsDropped = 1 And Target003.IsDropped = 1 Then
	CheckLightLock
	DOF 338, DOFPulse
	Wall038.Collidable = 0
	Wall038.SideVisible = True
	Flasher004.visible = 0
	Flasher005.visible = 0
	PlaySound "Vo_Ok"
	End If
End Sub
Sub Target001_Hit()
	AddScore 250
	PlaySound "fx_target"
	LiLARA002.state = 1
	DOF 335, DOFPulse
	If Target000.IsDropped = 1 And Target002.IsDropped = 1 And Target003.IsDropped = 1 Then
	CheckLightLock
	DOF 338, DOFPulse
	Wall038.Collidable = 0
	Wall038.SideVisible = True
	Flasher004.visible = 0
	Flasher005.visible = 0
	PlaySound "Vo_Ok"
	End If
End Sub
Sub Target002_Hit()
	AddScore 250
	PlaySound "fx_target"
	LiLARA003.state = 1
	DOF 336, DOFPulse
	If Target001.IsDropped = 1 And Target000.IsDropped = 1 And Target003.IsDropped = 1 Then
	CheckLightLock
	DOF 338, DOFPulse
	Wall038.Collidable = 0
	Wall038.SideVisible = True
	Flasher004.visible = 0
	Flasher005.visible = 0
	PlaySound "Vo_Ok"
	End If
End Sub
Sub Target003_Hit()
	AddScore 250
	PlaySound "fx_target"
	LiLARA004.state = 1
	DOF 337, DOFPulse
	If Target001.IsDropped = 1 And Target002.IsDropped = 1 And Target000.IsDropped = 1 Then
	CheckLightLock
	DOF 338, DOFPulse
	Wall038.Collidable = 0
	Wall038.SideVisible = True
	Flasher004.visible = 0
	Flasher005.visible = 0
	PlaySound "Vo_Ok"
	End If
End Sub

Sub Target004_Hit()
	AddScore 250
	PlaySound "fx_TDrop"
	If LightR001.State = 2 Then
	LightR001.State = 1
	CroftBonus = CroftBonus + 1
	LetterCountC(CurrentPlayer) = 1
	CheckTargetCroft
'	DmdCroftC
	End If
End Sub
Sub Target005_Hit()
	AddScore 250
	PlaySound "fx_TDrop"
	If LightR002.State = 2 Then
	LightR002.State = 1
	CroftBonus = CroftBonus + 1
	LetterCountR(CurrentPlayer) = 1
	CheckTargetCroft
'	DmdCroftR
	End If
End Sub
Sub Target006_Hit()
	AddScore 250
	PlaySound "fx_TDrop"
	If LightR003.State = 2 Then
	LightR003.State = 1
	CroftBonus = CroftBonus + 1
	LetterCountO(CurrentPlayer) = 1
	CheckTargetCroft
'	DmdCroftO
	End If
End Sub
Sub Target007_Hit()
	AddScore 250
	PlaySound "fx_TDrop"
	If LightR004.State = 2 Then
	LightR004.State = 1
	CroftBonus = CroftBonus + 1
	LetterCountF(CurrentPlayer) = 1
	CheckTargetCroft
'	DmdCroftF
	End If
End Sub
Sub Target008_Hit()
	AddScore 250
	PlaySound "fx_TDrop"
	If LightR005.State = 2 Then
	LightR005.State = 1
	CroftBonus = CroftBonus + 1
	LetterCountT(CurrentPlayer) = 1
	CheckTargetCroft
'	DmdCroftT
	End If
End Sub
Sub ResetTargetCroft
	LightR001.State = 2
	LightR002.State = 2
	LightR003.State = 2
	LightR004.State = 2
	LightR005.State = 2
	LetterCountC(CurrentPlayer) = 0
	LetterCountR(CurrentPlayer) = 0
	LetterCountO(CurrentPlayer) = 0
	LetterCountF(CurrentPlayer) = 0
	LetterCountT(CurrentPlayer) = 0
End Sub
Sub CheckTargetCroft
	If LightR001.State = 1 And LightR002.State = 1 And LightR003.State = 1 And LightR004.State = 1 And LightR005.State = 1 Then
	LiMission.State = 2
	pupDMDDisplay "attract","","@StartMission.mp4",8,0,15
	PlaySound "Vo_StartMiss"
	DOF 401, DOFPulse
	ModeMissionActive(CurrentPlayer) = True
	StopHITpoint
	CroftBonus = 0
'	LightEffect 4
	End if
End Sub

Sub TriggerDebug_Hit()
'	puPlayer.LabelSet pBackglass,"titleimg","PUPAlphas\\FlasherGEM1.png",0,"{'mt':2,'width': 60, 'height': 60, 'yalign': 1}"
'	ChangeBall(0)
	ShakeLaraHeadON
'	StartGun
'	ShakeLaraGunOFF
'	ShakeLogLOFF
'	ShakeLogROFF
	CheckRelic3
	CheckBandeauBG
	DebugLightMissionActive
	If bModeSouterrainActive = False And Flasher001.visible = False Then
	Flasher001.visible=1
	l2.state=0
	l3.state=0
	l4.state=0
	l5.state=0
	l6.state=0
	l7.state=0
	Light012.state=0
	Light013.state=0
	Light012.color = RGB(255, 0, 0) 'red
	Light012.colorfull = RGB(255, 0, 0)
	Light013.color = RGB(255, 0, 0)
	Light013.colorfull = RGB(255, 0, 0)
	GiOn
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to1"
	End If
	If Target000.IsDropped = 1 And Target001.IsDropped = 1 And Target002.IsDropped = 1 And Target003.IsDropped = 1 Then
	CheckLightLock
	Wall038.Collidable = 0
	Wall038.SideVisible = True
	Flasher004.visible = 0
	Flasher005.visible = 0
	End If
End Sub
Sub DebugLightMissionActive
	If ModeRelic1Active(CurrentPlayer) = True Then
	LiB001.state = 2
	End If
	If ModeRelic2aActive(CurrentPlayer) = True Then
	LiB002a.state = 2
	End If
	If ModeRelic2bActive(CurrentPlayer) = True Then
	LiB002b.state = 2
	End If
	If ModeRelic3Active(CurrentPlayer) = True Then
	LiB003.state = 2
	End If
	If ModeRelic4Active(CurrentPlayer) = True Then
	LiB004.state = 2
	End If
	If ModeRelic5Active(CurrentPlayer) = True Then
	LiB005.state = 2
	End If
	If ModeRelicAllActive(CurrentPlayer) = True Then
	LiB001.state = 2
	LiB002a.state = 2
	LiB002b.state = 2
	LiB003.state = 2
	LiB004.state = 2
	LiB005.state = 2
	End If
End Sub

Sub CheckRestartMissionActive
	If ModeRelic1Active(CurrentPlayer) = True Then
	Mode_Relic1_start
	LiB001.state = 2
	End If
	If ModeRelic2aActive(CurrentPlayer) = True Then
	Mode_Relic2a_start
	LiB002a.state = 2
	End If
	If ModeRelic2bActive(CurrentPlayer) = True Then
	Mode_Relic2b_start
	LiB002b.state = 2
	End If
	If ModeRelic3Active(CurrentPlayer) = True Then
	Mode_Relic3_start
	LiB003.state = 2
	End If
	If ModeRelic4Active(CurrentPlayer) = True Then
	ModeRelic4Active(CurrentPlayer) = False
	Mission_Relic4(CurrentPlayer) = 0
	ChangeBall(0)
	LiB004.state = 0
	ResetLightRelic
	LightR001.State = 1
	LightR002.State = 1
	LightR003.State = 1
	LightR004.State = 1
	LightR005.State = 1
	LetterCountC(CurrentPlayer) = 1
	LetterCountR(CurrentPlayer) = 1
	LetterCountO(CurrentPlayer) = 1
	LetterCountF(CurrentPlayer) = 1
	LetterCountT(CurrentPlayer) = 1
	CheckTargetCroft
	End If
	If ModeRelic5Active(CurrentPlayer) = True Then
	Mode_Relic5_start
	LiB005.state = 2
	End If
	If ModeRelicAllActive(CurrentPlayer) = True Then
	ModeRelicAllActive(CurrentPlayer) = False
	Relics_Sav(CurrentPlayer) = 0
	Mission_Relic1(CurrentPlayer) = 0
	Mission_Relic2a(CurrentPlayer) = 0
	Mission_Relic2b(CurrentPlayer) = 0
	Mission_Relic3(CurrentPlayer) = 0
	Mission_Relic4(CurrentPlayer) = 0
	Mission_Relic5(CurrentPlayer) = 0
	ModeRelic1Active(CurrentPlayer) = False
	ModeRelic2aActive(CurrentPlayer) = False
	ModeRelic2bActive(CurrentPlayer) = False
	ModeRelic3Active(CurrentPlayer) = False
	ModeRelic4Active(CurrentPlayer) = False
	ModeRelic5Active(CurrentPlayer) = False
	ResetLightRelic
	LightR001.State = 1
	LightR002.State = 1
	LightR003.State = 1
	LightR004.State = 1
	LightR005.State = 1
	LetterCountC(CurrentPlayer) = 1
	LetterCountR(CurrentPlayer) = 1
	LetterCountO(CurrentPlayer) = 1
	LetterCountF(CurrentPlayer) = 1
	LetterCountT(CurrentPlayer) = 1
	CheckTargetCroft
	End If
End Sub

Dim  ModeRelic1Actived, ModeRelic2aActived, ModeRelic2bActived, ModeRelic3Actived, ModeRelic4Actived, ModeRelic5Actived
ModeRelic1Actived = False : ModeRelic2aActived = False : ModeRelic2bActived = False : ModeRelic3Actived = False : ModeRelic4Actived = False : ModeRelic5Actived = False

Sub CheckBandeauBG
	If ModeRelic1Active(CurrentPlayer) = True Then
		PuPEvent 106
		ModeRelic1Actived = True
	Else
		If ModeRelic1Actived = True Then
			PupEvent 112
			ModeRelic1Actived = False
		End If
	End If
	If ModeRelic2aActive(CurrentPlayer) = True Then
		PuPEvent 107
		ModeRelic2aActived = True
	Else
		If ModeRelic2aActived = True Then
			PupEvent 113
			ModeRelic2aActived = False
		End If
	End If
	If ModeRelic2bActive(CurrentPlayer) = True Then
		PuPEvent 108
		ModeRelic2bActived = True
	Else
		If ModeRelic2bActived = True Then
			PupEvent 114
			ModeRelic2bActived = False
		End If
	End If
	If ModeRelic3Active(CurrentPlayer) = True Then
		PuPEvent 109
		ModeRelic3Actived = True
	Else
		If ModeRelic3Actived = True Then
			PupEvent 115
			ModeRelic3Actived = False
		End If
	End If
	If ModeRelic4Active(CurrentPlayer) = True Then
		PuPEvent 110
		ModeRelic4Actived = True
	Else
		If ModeRelic4Actived = True Then
			PupEvent 116
			ModeRelic4Actived = False
		End If
	End If
	If ModeRelic5Active(CurrentPlayer) = True Then
		PuPEvent 111
		ModeRelic5Actived = True
	Else
		If ModeRelic5Actived = True Then
			PupEvent 117
			ModeRelic5Actived = False
		End If
	End If
End Sub
Sub BandeauBGOFF
	PupEvent 112
	PupEvent 113
	PupEvent 114
	PupEvent 115
	PupEvent 116
	PupEvent 117
End Sub
Sub CheckLightLock
		If LightMB1.state=1 And LightMB2.state=1 And LightMB3.state=0 Then
		liLOCK.State = 0
		liMB.State = 2
		LiLARA001.state = 0
		LiLARA002.state = 0
		LiLARA003.state = 0
		LiLARA004.state = 0
		Else
		liLOCK.State = 2 
		liMB.State = 0
		LiLARA001.state = 0
		LiLARA002.state = 0
		LiLARA003.state = 0
		LiLARA004.state = 0
		End If
End Sub

Sub ResetTargetLara
	LiLARA001.state = 2
	LiLARA002.state = 2
	LiLARA003.state = 2
	LiLARA004.state = 2
	Target000.IsDropped = False
	Target001.IsDropped = False
	Target002.IsDropped = False
	Target003.IsDropped = False
	Wall038.Collidable = 1
	Wall038.SideVisible = False
	If bModeIncaActive = True Then
	Flasher004.visible = 1
	Flasher005.visible = 1
	End If
End Sub

Sub Target011_Hit()
	AddScore 250
	PlaySound "fx_OpenDoor"
	If Wall032.SideVisible = False And Wall033.SideVisible = False Then
	Wall032.Collidable = 0
	Wall032.SideVisible = True
	Wall033.Collidable = 1
	Wall033.SideVisible = False
	Elseif Wall032.SideVisible = True And Wall033.SideVisible = False Then
	Wall032.Collidable = 1
	Wall032.SideVisible = False
	Wall033.Collidable = 0
	Wall033.SideVisible = True
	Elseif Wall032.SideVisible = False And Wall033.SideVisible = True Then
	Wall032.Collidable = 0
	Wall032.SideVisible = True
	Wall033.Collidable = 1
	Wall033.SideVisible = False
	End If
End Sub

Sub Target009_Hit()
	AddScore 250
	PlaySound "fx_target"
	If RampSoutCount >= 4 Then
	Light012.color = RGB(0, 128, 255) 
	Light012.colorfull = RGB(0, 128, 255)
	Light013.color = RGB(0, 128, 255) 
	Light013.colorfull = RGB(0, 128, 255)'BLUE
	ChangeBall(2)
	StartGems
	End If
End Sub
Sub Target010_Hit()
	AddScore 250
	PlaySound "fx_target"
	If RampSoutCount >= 4 Then
	Light012.color = RGB(0, 128, 255) 
	Light012.colorfull = RGB(0, 128, 255)
	Light013.color = RGB(0, 128, 255) 
	Light013.colorfull = RGB(0, 128, 255)'BLUE
	ChangeBall(2)
	StartGems
	End If
End Sub

Sub TriggerTEST_Hit()
	ShakeLaraHeadOFF
'	PuPlayer.LabelShowPage pBackglass,1,0,""
'	PuPlayer.LabelSet pBackglass,"rulecopy1",""& Credits,1,"{'mt':2,'color':15066597, 'size': 3, 'xpos': 8.8, 'xalign': 1, 'ypos': 82, 'yalign': 1}"
'	StartHITpoint
'	ShakeLaraGunON
'	StartGun
'	ShakeLogLON
'	ShakeLogRON
	l001.State = 1
	FlashForMs Light002, 2000, 30, 0 
'	FlashForMs Light_Folder, 500, 250, 0 
'	FlashForMs Light_Folder2, 500, 250, 0 
'	ModeInca3D
'	ModeEgypt3D
'	FlashForMs Flasher010, 2000, 30, 0
'	ChangeBall(1) 'Bleu
'	ChangeBall(2) 'vert
'	ChangeBall(3) 'Rouge
'	ChangeBall(4) 'Jaune
'	BumperGIEffect 3
'	Mission_Relic1(CurrentPlayer) = 1
'	Mission_Relic2a(CurrentPlayer) = 1
'	Mission_Relic2b(CurrentPlayer) = 1
'	Mission_Relic3(CurrentPlayer) = 1
'	Mission_Relic4(CurrentPlayer) = 1
'	Mission_Relic5(CurrentPlayer) = 1
'	CheckLightCroft
'	resetbackglass
'	AddScore 350000
'	Reseths
'OK	puPlayer.LabelSet pBackglass,"titleimg","PUPAlphas\\FlasherGEM1.png",1,"{'mt':2,'width': 60, 'height': 60, 'yalign': 1}"
'	PuPGameStartMiniGame
'	bModeMiniGameActive = True
'OK	PuPlayer.LabelSet pBackglass,"gobletnum","SHOOT AGAIN",1,"{'mt':2}"
'	PuPEvent 101
'	pupDMDDisplay "attract","Extraball","@ExtraBall.mp4",3,0,10
'	pupDMDDisplay "Splash", "MULTIBALL", "", 5, 1, 10
'	Mode_Relic1_start ' BUMPER HITS OK
'	Mode_Relic2a_start ' RAMP L Ok
'	Mode_Relic2b_start ' RAMP R OK
'	Mode_Relic3_start ' ALL RAMPS / LINES OK
'	Mode_Relic4_start ' ALL RAMPS X3 OK
'	Mode_Relic5_start ' SPINNER OK
'	Mode_RelicAll_start
'	ModeEgypt3D
'	LightEffect 1 
'	GiEffect 4
End Sub

Sub Trigger004_Hit()
	DOF 442, DOFPulse
	AddScore 250
	If ModeRelic2bActive (CurrentPlayer) = True And LightM3001.State=2 And LightM3002.State=2 And LightM3003.State=2 Then
	LightM3003.State=1
	Elseif ModeRelic2bActive (CurrentPlayer) = True And LightM3001.State=2 And LightM3002.State=2 And LightM3003.State=1 Then
	LightM3002.State=1
	Elseif ModeRelic2bActive (CurrentPlayer) = True And LightM3001.State=2 And LightM3002.State=1 And LightM3003.State=1 Then
	Mode_Relic2b_end
	End If
	If ModeRelic3Active (CurrentPlayer) = True And LightM3001.State=2 Then
	LightM3001.State=1
	CheckRelic3
	End If
	If ModeRelic4Active (CurrentPlayer) = True And LightM3001.State=2 And LightM3002.State=2 And LightM3003.State=2 Then
	LightM3003.State=1
	Elseif ModeRelic4Active (CurrentPlayer) = True And LightM3001.State=2 And LightM3002.State=2 And LightM3003.State=1 Then
	LightM3002.State=1
	Elseif ModeRelic4Active (CurrentPlayer) = True And LightM3001.State=2 And LightM3002.State=1 And LightM3003.State=1 Then 
	LightM3001.State=1
	CheckRelic4
	End If	
	If ModeRelicAllActive(CurrentPlayer) = True And LightM3001.State=2 And LightM3002.State=2 And LightM3003.State=2 Then 
	LightM3003.State=1
	BumperGIEffect 3
	Elseif ModeRelicAllActive(CurrentPlayer) = True And LightM3001.State=2 And LightM3002.State=2 And LightM3003.State=1 Then 
	LightM3002.State=1
	BumperGIEffect 3
	Elseif ModeRelicAllActive(CurrentPlayer) = True And LightM3001.State=2 And LightM3002.State=1 And LightM3003.State=1 Then 
	LightM3001.State=1
	BumperGIEffect 3
	CheckRelicAll
	End If
End Sub

Sub Trigger008_Hit()
	AddScore 250
	If ModeRelic2aActive (CurrentPlayer) = True And LightM2B001.State=2 And LightM2B002.State=2 And LightM2B003.State=2 Then
	LightM2B003.State=1
	Elseif ModeRelic2aActive (CurrentPlayer) = True And LightM2B001.State=2 And LightM2B002.State=2 And LightM2B003.State=1 Then
	LightM2B002.State=1
	Elseif ModeRelic2aActive (CurrentPlayer) = True And LightM2B001.State=2 And LightM2B002.State=1 And LightM2B003.State=1 Then
	Mode_Relic2a_end
	End If
	If ModeRelic3Active (CurrentPlayer) = True And LightM2B001.State=2 Then
	LightM2B001.State=1
	CheckRelic3
	End If
	If ModeRelic4Active (CurrentPlayer) = True And LightM2B001.State=2 And LightM2B002.State=2 And LightM2B003.State=2 Then
	LightM2B003.State=1
	Elseif ModeRelic4Active (CurrentPlayer) = True And LightM2B001.State=2 And LightM2B002.State=2 And LightM2B003.State=1 Then
	LightM2B002.State=1
	Elseif ModeRelic4Active (CurrentPlayer) = True And LightM2B001.State=2 And LightM2B002.State=1 And LightM2B003.State=1 Then
	LightM2B001.State=1
	CheckRelic4
	End If
	If ModeRelicAllActive(CurrentPlayer) = True And LightM2B001.State=2 And LightM2B002.State=2 And LightM2B003.State=2 Then 
	LightM2B003.State=1
	BumperGIEffect 2
	Elseif ModeRelicAllActive(CurrentPlayer) = True And LightM2B001.State=2 And LightM2B002.State=2 And LightM2B003.State=1 Then 
	LightM2B002.State=1
	BumperGIEffect 2
	Elseif ModeRelicAllActive(CurrentPlayer) = True And LightM2B001.State=2 And LightM2B002.State=1 And LightM2B003.State=1 Then 
	LightM2B001.State=1
	BumperGIEffect 2
	CheckRelicAll
	End If
End Sub

Sub Trigger005_Hit()
	DOF 480, DOFPulse
	ExtraBsensTimer.Enabled=True
End Sub
Sub ExtraBsensTimer_Timer()
	ExtraBsensTimer.Enabled=False
End Sub

Sub Trigger009_Hit()
	AddScore 250
	If ExtraBsensTimer.Enabled=True Then ExtraBCount=ExtraBCount+1
	if ExtraBCount=1 then LiEXTRA.state=0:pupDMDDisplay "Splash3", "5^More To^EXTRABALL", "", 5, 1, 6
	if ExtraBCount=2 then LiEXTRA.state=0:pupDMDDisplay "Splash3", "4^More To^EXTRABALL", "", 5, 1, 7
	if ExtraBCount=3 then LiEXTRA.state=0:pupDMDDisplay "Splash3", "3^More To^EXTRABALL", "", 5, 1, 8
	if ExtraBCount=4 then LiEXTRA.state=0:pupDMDDisplay "Splash3", "2^More To^EXTRABALL", "", 5, 1, 9
	if ExtraBCount=5 then LiEXTRA.state=2:pupDMDDisplay "Splash3", "1^More To^EXTRABALL", "", 5, 1, 9
	if ExtraBCount>=7 then AddScore 2500
	if ExtraBCount=6 then 
	LiEXTRA.state=1
	AwardExtraBall
	pupDMDDisplay "attract","Extraball","@ExtraBall.mp4",5,0,14
	PlaySound "Vo_WinExtraB"
	End If
	If ModeRelic3Active (CurrentPlayer) = True And LightM1001.State=2 Then
	LightM1001.State=1
	CheckRelic3
	End If
	If ModeRelicAllActive(CurrentPlayer) = True And LightM1001.State=2 And LightM1002.State=2 And LightM1003.State=2 Then 
	LightM1003.State=1
	BumperGIEffect 2
	Elseif ModeRelicAllActive(CurrentPlayer) = True And LightM1001.State=2 And LightM1002.State=2 And LightM1003.State=1 Then 
	LightM1002.State=1
	BumperGIEffect 2
	Elseif ModeRelicAllActive(CurrentPlayer) = True And LightM1001.State=2 And LightM1002.State=1 And LightM1003.State=1 Then 
	LightM1001.State=1
	BumperGIEffect 2
	CheckRelicAll
	End If
	If ModeSniperShooting(CurrentPlayer) = True And LightM1001.State = 2 Then
	SniperShootingCount = SniperShootingCount + 1
	CheckHitPoint
		If ExtraBCount <=4 Then Playsound "Vo_Nice"
	End If
End Sub

Sub Trigger010_Hit()
	AddScore 250
	PuPEvent 105
	If ModeRelic3Active (CurrentPlayer) = True And LightM2A001.State=2 Then
	LightM2A001.State=1
	CheckRelic3
	End If
	If ModeRelicAllActive(CurrentPlayer) = True And LightM2A001.State=2 And LightM2A002.State=2 And LightM2A003.State=2 Then 
	LightM2A003.State=1
	BumperGIEffect 1
	Elseif ModeRelicAllActive(CurrentPlayer) = True And LightM2A001.State=2 And LightM2A002.State=2 And LightM2A003.State=1 Then 
	LightM2A002.State=1
	BumperGIEffect 1
	Elseif ModeRelicAllActive(CurrentPlayer) = True And LightM2A001.State=2 And LightM2A002.State=1 And LightM2A003.State=1 Then 
	LightM2A001.State=1
	BumperGIEffect 1
	CheckRelicAll
	End If
	If ModeSniperShooting(CurrentPlayer) = True And LightM2A001.State = 2 Then
	SniperShootingCount = SniperShootingCount + 1
	CheckHitPoint
	End If
End Sub

Sub Trigger011_Hit()
	DOF 440, DOFPulse
	AddScore 650
	If ModeSniperShooting(CurrentPlayer) = False Then Playsound "Vo_rollover"
	pupDMDDisplay "attract","","@Moove2.mp4",8,0,7
	If ModeRelic3Active (CurrentPlayer) = True And LightM4001.State=2 Then
	LightM4001.State=1
	CheckRelic3
	End If
	If ModeRelic4Active (CurrentPlayer) = True And LightM4001.State=2 And LightM4002.State=2 And LightM4003.State=2 Then
	LightM4003.State=1
	Elseif ModeRelic4Active (CurrentPlayer) = True And LightM4001.State=2 And LightM4002.State=2 And LightM4003.State=1 Then
	LightM4002.State=1
	Elseif ModeRelic4Active (CurrentPlayer) = True And LightM4001.State=2 And LightM4002.State=1 And LightM4003.State=1 Then
	LightM4001.State=1
	CheckRelic4
	End If	
	If ModeRelicAllActive(CurrentPlayer) = True And LightM4001.State=2 And LightM4002.State=2 And LightM4003.State=2 Then 
	LightM4003.State=1
	BumperGIEffect 4
	Elseif ModeRelicAllActive(CurrentPlayer) = True And LightM4001.State=2 And LightM4002.State=2 And LightM4003.State=1 Then 
	LightM4002.State=1
	BumperGIEffect 4
	Elseif ModeRelicAllActive(CurrentPlayer) = True And LightM4001.State=2 And LightM4002.State=1 And LightM4003.State=1 Then 
	LightM4001.State=1
	BumperGIEffect 4
	CheckRelicAll
	End If
	If ModeSniperShooting(CurrentPlayer) = True And LightM4001.State = 2 Then
	SniperShootingCount = SniperShootingCount + 1
	CheckHitPoint
	Playsound "Vo_Nice"
	End If
End Sub

Sub Trigger012_Hit()
	DOF 481, DOFPulse
	AddScore 250
	PuPEvent 102
	If BallsOnPlayfield >=3 Then BallSaverEnd
	If ModeRelic3Active (CurrentPlayer) = True And LightM5001.State=2 Then
	LightM5001.State=1
	CheckRelic3
	End If
	If ModeRelicAllActive(CurrentPlayer) = True And LightM5001.State=2 And LightM5002.State=2 And LightM5003.State=2 Then 
	LightM5003.State=1
	BumperGIEffect 2
	Elseif ModeRelicAllActive(CurrentPlayer) = True And LightM5001.State=2 And LightM5002.State=2 And LightM5003.State=1 Then 
	LightM5002.State=1
	BumperGIEffect 2
	Elseif ModeRelicAllActive(CurrentPlayer) = True And LightM5001.State=2 And LightM5002.State=1 And LightM5003.State=1 Then 
	LightM5001.State=1
	BumperGIEffect 2
	CheckRelicAll
	End If
	If ModeSniperShooting(CurrentPlayer) = True And LightM5001.State = 2 Then
	SniperShootingCount = SniperShootingCount + 1
	CheckHitPoint
	Playsound "Vo_Nice"
	End If
End Sub
Sub Trigger013_Hit()
	'AddScore 200
	RampMCount=RampMCount+1
	If bMultiBallMode = True And AddBallBonusMBTimer.enabled = True And RampMCount=2 And BallsOnPlayfield <= 4 Then AddMBmission
	If bMultiBallMode = True And AddBallBonusMBTimer.enabled = True And RampMCount=4 And BallsOnPlayfield <= 4 Then AddMBmission
	If bMultiBallMode = True And AddBallBonusMBTimer.enabled = True And RampMCount=6 And BallsOnPlayfield <= 4 Then AddMBmission
	if RampMCount=1 then pupDMDDisplay "attract","","@Scene1.mp4",8,0,8
	if RampMCount=2 then pupDMDDisplay "Splash3", "3^More To^JACKPOT", "", 5, 1, 7
	if RampMCount=3 then pupDMDDisplay "Splash3", "2^More To^JACKPOT", "", 5, 1, 8
	if RampMCount=4 then pupDMDDisplay "Splash3", "1^More To^JACKPOT", "", 5, 1, 9
	if RampMCount=5 then 
	AddScore 15000
	DOF 128, DOFPulse
	PlaySound "Vo_Jackpot"
	pupDMDDisplay "attract","","@Jackpot.mp4",6,0,11
	JackpotsBonus = JackpotsBonus + 1
	End If
	if RampMCount=6 then pupDMDDisplay "Splash3", "1^More To^SUPER JACKPOT", "", 5, 1, 10
	if RampMCount=7 then 
	DOF 450, DOFPulse
	PlaySound "Vo_SJackpot"
	pupDMDDisplay "attract","","@Jackpot.mp4",6,0,12
	AddScore 35000
	JackpotsBonus = JackpotsBonus + 1
	RampMCount=0
	End If
	If ModeSniperShooting(CurrentPlayer) = True And LightM3001.State = 2 Then
	SniperShootingCount = SniperShootingCount + 1
	CheckHitPoint
		If RampMCount = 6 or RampMCount <=4 Then Playsound "Vo_Nice"
	End If
End Sub

Sub Trigger014_Hit()
	DOF 441, DOFPulse
	RampLCount=RampLCount+1
	If bMultiBallMode = True And AddBallBonusMBTimer.enabled = True And RampLCount=3 And BallsOnPlayfield <= 4 Then AddMBmission
	If bMultiBallMode = True And AddBallBonusMBTimer.enabled = True And RampLCount=8 And BallsOnPlayfield <= 4 Then AddMBmission
	if RampLCount=1 then pupDMDDisplay "attract","","@Scene2.mp4",10,0,8 
	if RampLCount=2 then pupDMDDisplay "Splash3", "3^More To^JACKPOT", "", 5, 1, 7 : ShakeLaraGunON
	if RampLCount=3 then pupDMDDisplay "Splash3", "2^More To^JACKPOT", "", 5, 1, 8 : StartGun : AddScore 5000 : DOF 229, DOFPulse
	if RampLCount=4 then pupDMDDisplay "Splash3", "1^More To^JACKPOT", "", 5, 1, 9
	if RampLCount=5 then 
	AddScore 15000
	DOF 128, DOFPulse
	PlaySound "Vo_Jackpot"
	pupDMDDisplay "attract","","@Jackpot.mp4",6,0,11
	JackpotsBonus = JackpotsBonus + 1
	End If
	if RampLCount=6 then pupDMDDisplay "Splash3", "1^More To^SUPER JACKPOT", "", 5, 1, 10
	if RampLCount=7 then 
	DOF 450, DOFPulse
	PlaySound "Vo_SJackpot"
	pupDMDDisplay "attract","","@Jackpot.mp4",6,0,12
	JackpotsBonus = JackpotsBonus + 1
	AddScore 35000
	RampLCount=0
	End If
	If ModeSniperShooting(CurrentPlayer) = True And LightM2B001.State = 2 Then
	SniperShootingCount = SniperShootingCount + 1
	CheckHitPoint
		If RampLCount = 6 or RampLCount <=4 Then Playsound "Vo_Nice"
	End If
End Sub

Sub Trigger015_Hit()
	AddLetterCroft
End Sub

Sub AddLetterCroft
	If ModeRelic1Active(CurrentPlayer) = False And ModeRelic2aActive(CurrentPlayer) = False And	ModeRelic2bActive(CurrentPlayer) = False And ModeRelic3Active(CurrentPlayer) = False And ModeRelic4Active(CurrentPlayer) = False And ModeRelic5Active(CurrentPlayer) = False And ModeRelicAllActive(CurrentPlayer) = False And ModeMissionActive(CurrentPlayer) = False Then
	Dim ModeChosenLight
		ModeChosenLight = False
		Do While ModeChosenLight = False
			Select Case RndNbr(5)
			Case 1
				If LightR001.state = 2 Then 
				LightR001.state = 1
				LetterCountC(CurrentPlayer) = 1
				ModeChosenLight = True
				End If
			Case 2
				If LightR002.state = 2 Then 
				LightR002.state = 1
				LetterCountR(CurrentPlayer) = 1
				ModeChosenLight = True
				End If
			Case 3
				If LightR003.state = 2 Then 
				LightR003.state = 1
				LetterCountO(CurrentPlayer) = 1
				ModeChosenLight = True
				End If
			Case 4
				If LightR004.state = 2 Then 
				LightR004.state = 1
				LetterCountF(CurrentPlayer) = 1
				ModeChosenLight = True
				End If
			Case 5
				If LightR005.state = 2 Then 
				LightR005.state = 1
				LetterCountT(CurrentPlayer) = 1
				ModeChosenLight = True
				End If
			End Select
		CheckTargetCroft
		Loop
	End If
End Sub

Sub TLeftOutlane_Hit()
	AddScore 250
	If LeftOutlane.State = 0 Then
	LeftOutlane.State = 2
	Elseif LeftOutlane.State = 2 Then
	LeftOutlane.State=1
	Playsound "Vo_Souffle"
	RaiderBonus = RaiderBonus + 1
	CheckLightRaider
	End If
End Sub
Sub TLeftInlane2_Hit()
	AddScore 250
	leftInlaneSpeedLimit
	If LeftInlane2.State = 0 Then
	LeftInlane2.State = 2
	Elseif LeftInlane2.State = 2 Then
	LeftInlane2.State=1
	Playsound "Vo_Souffle"
	RaiderBonus = RaiderBonus + 1
	CheckLightRaider
	End If
End Sub
Sub TLeftInlane_Hit()
	AddScore 250
	leftInlaneSpeedLimit
	If LeftInlane.State = 0 Then
	LeftInlane.State = 2
	Elseif LeftInlane.State = 2 Then
	LeftInlane.State=1
	Playsound "Vo_Souffle"
	RaiderBonus = RaiderBonus + 1
	CheckLightRaider
	End If
End Sub
Sub TRightInlane_Hit()
	AddScore 250
	rightInlaneSpeedLimit
	If RightInlane.State = 0 Then
	RightInlane.State = 2
	Elseif RightInlane.State = 2 Then
	RightInlane.State=1
	Playsound "Vo_Souffle"
	RaiderBonus = RaiderBonus + 1
	CheckLightRaider
	End If
End Sub
Sub TRightInlane2_Hit()
	AddScore 250
	rightInlaneSpeedLimit
	If RightInlane2.State = 0 Then
	RightInlane2.State = 2
	Elseif RightInlane2.State = 2 Then
	RightInlane2.State=1
	Playsound "Vo_Souffle"
	RaiderBonus = RaiderBonus + 1
	CheckLightRaider
	End If
End Sub
Sub TRightOutlane_Hit()
	AddScore 250
	FlashForMs Light002, 2000, 30, 0 
	If RightOutlane.State = 0 Then
	RightOutlane.State = 2
	Elseif RightOutlane.State = 2 Then
	RightOutlane.State=1
	Playsound "Vo_Souffle"
	RaiderBonus = RaiderBonus + 1
	CheckLightRaider
	End If
End Sub
Sub CheckLightRaider
	If LeftOutlane.State=1 And LeftInlane2.State=1 And LeftInlane.State=1 And RightInlane.State=1 And RightInlane2.State=1 And RightOutlane.State=1 Then
	AddScore 8500
	ResetLightRaider
	PuPEvent 103
	pupDMDDisplay "attract","","@Add1Ball.mp4",6,0,14
	AddMBmission
	PlaySound "Vo_Nice"
	End If
End Sub
Sub ResetLightRaider
	LeftOutlane.State=0 
	LeftInlane2.State=0 
	LeftInlane.State=0 
	RightInlane.State=0 
	RightInlane2.State=0
	RightOutlane.State=0
	RaiderBonus = 0
End Sub

Sub CheckRelic3
	If ModeRelic3Active (CurrentPlayer) = True and LightM1001.State=1 and LightM2B001.State=1 and LightM2A001.State=1 and LightM3001.State=1 and LightM4001.State=1 and LightM5001.State=1 Then
	Mode_Relic3_end
	End If
End Sub
Sub CheckRelic4
	If ModeRelic4Active (CurrentPlayer) = True and LightM2B001.State=1 and LightM3001.State=1 and LightM4001.State=1 Then
	Mode_Relic4_end
	End If
End Sub
Sub CheckRelicAll
	If ModeRelicAllActive (CurrentPlayer) = True and LightM1001.State=1 and LightM2A001.State=1 and LightM2B001.State=1 and LightM3001.State=1 and LightM4001.State=1 and LightM5001.State=1 Then
	Mode_RelicAll_end
	End If
End Sub
'***********
' Change 3D
'***********
Sub ModeEgypt3D
	If ModeRelic4Active(CurrentPlayer) = True Then
	Wall024.SideImage = "backwall4"
	Elseif ModeRelic4Active(CurrentPlayer) = False Then
	Wall024.SideImage = "backwall2"
	End If
	bModeEgyptActive = True
	bModeIncaActive = False
	StopSong
	PlaySong "Mu_2"
	Wall001.SideImage = "sidewall2"
	Wall002.SideImage = "sidewall2"
	Primitive034.Visible = 1 'EGYPT
	Primitive035.Visible = 1
	Primitive036.Visible = 1
	Primitive037.Visible = 1
	Primitive038.Visible = 1
	Primitive039.Visible = 0 'INCA
	Primitive040.Visible = 0
	Primitive041.Visible = 0
	Primitive042.Visible = 0
	Primitive043.Visible = 0
	Primitive044.Visible = 0
	Primitive045.Visible = 0
	Primitive046.Visible = 0
	Primitive047.Visible = 0
	Flasher002.visible = 0
	Flasher003.visible = 0
	Flasher004.visible = 0
	Flasher005.visible = 0
	Flasher006.visible = 0
	If LiLOCK.state = 2 or LiMB.state = 2 Then
	Wall038.Collidable = 0
	Wall038.SideVisible = True
	Else 
	Wall038.Collidable = 1
	Wall038.SideVisible = False
	End If
End Sub
Sub ModeInca3D
	If ModeRelic2aActive(CurrentPlayer) = True or ModeRelic2bActive(CurrentPlayer) = True Then
	Wall024.SideImage = "backwall3"
	Elseif ModeRelic2aActive(CurrentPlayer) = False And ModeRelic2bActive(CurrentPlayer) = False Then
	Wall024.SideImage = "backwall"
	End If
	bModeEgyptActive = False
	bModeIncaActive = True
	Wall001.SideImage = "sidewall"
	Wall002.SideImage = "sidewall"
	Primitive034.Visible = 0 'EGYPT
	Primitive035.Visible = 0
	Primitive036.Visible = 0
	Primitive037.Visible = 0
	Primitive038.Visible = 0
	Primitive039.Visible = 1 'INCA
	Primitive040.Visible = 1
	Primitive041.Visible = 1
	Primitive042.Visible = 1
	Primitive043.Visible = 1
	Primitive044.Visible = 1
	Primitive045.Visible = 1
	Primitive046.Visible = 1
	Primitive047.Visible = 1
	Flasher002.visible = 1
	Flasher003.visible = 1
'	Flasher004.visible = 1
'	Flasher005.visible = 1
	Flasher006.visible = 1
	If LiLOCK.state = 2 or LiMB.state = 2 Then
	Wall038.Collidable = 0
	Wall038.SideVisible = True
	Flasher004.visible = 0
	Flasher005.visible = 0
	Else 
	Wall038.Collidable = 1
	Wall038.SideVisible = False
	Flasher004.visible = 1
	Flasher005.visible = 1
	End If
End Sub
'******
'Choix Logo
'******
Sub ChangeLOGO
Dim ModeChosenLOGO
		ModeChosenLOGO = False
		Do While ModeChosenLOGO = False
			Select Case RndNbr(9)
			Case 1
				Flasher001.ImageA = "FlasherLOGO"
				ModeChosenLOGO = True
			Case 2
				Flasher001.ImageA = "FlasherLOGO1"
				ModeChosenLOGO = True
			Case 3
				Flasher001.ImageA = "FlasherLOGO2"
				ModeChosenLOGO = True
			Case 4
				Flasher001.ImageA = "FlasherLOGO3"	
				ModeChosenLOGO = True
			Case 5
				Flasher001.ImageA = "FlasherLOGO4"
				ModeChosenLOGO = True
			Case 6
				Flasher001.ImageA = "FlasherLOGO5"	
				ModeChosenLOGO = True
			Case 7
				Flasher001.ImageA = "FlasherLOGO6"
				ModeChosenLOGO = True
			Case 8
				Flasher001.ImageA = "FlasherLOGO7"
				ModeChosenLOGO = True
			Case 9
				Flasher001.ImageA = "FlasherLOGO8"
				ModeChosenLOGO = True
			End Select
		Loop
End SUB

'*************
' digit count
'*************
Dim TimMul
Sub MissionDigitalCountTimer_Timer()
	If TimMul > 0 And TimMul <= 99 Then
	Flasher010.ImageA = "MissTime"
	Flasher010.visible = 1
	TimMul = TimMul - 1
	DispXText TimMul
	Elseif TimMul > 99 Then
	TimMul = TimMul - 1
	ElseIf TimMul = 0 Then 
	MissionLose
	MissionDigitalCountTimer.Enabled = False
	Xtens.ImageA = "DigitB"
	Xones.ImageA = "DigitB"
	End If
End Sub
Sub DispXText(Mul)
	If Mul > 99 Then
		mul = 99
	End If
'	If Mul = 0 Then MissionDigitalCountTimer.Enabled = False : MissionLose
	If Mul = 0 And ModeRelic1Active(CurrentPlayer) = True Then MissionDigitalCountTimer.Enabled = False : Mode_Relic1_end
	If Mul = 0 And ModeRelic2aActive(CurrentPlayer) = True Then MissionDigitalCountTimer.Enabled = False : MissionLose
	If Mul = 0 And ModeRelic2bActive(CurrentPlayer) = True Then MissionDigitalCountTimer.Enabled = False : MissionLose
	If Mul = 0 And ModeRelic3Active(CurrentPlayer) = True Then MissionDigitalCountTimer.Enabled = False : MissionLose
	If Mul = 0 And ModeRelic4Active(CurrentPlayer) = True Then MissionDigitalCountTimer.Enabled = False : MissionLose
	If Mul = 0 And ModeRelic5Active(CurrentPlayer) = True Then MissionDigitalCountTimer.Enabled = False : Mode_Relic5_end
	If Mul > 0 And Mul <= 99 Then
		Xones.ImageA = "DigitB" & Mul mod 10
		If mul > 9 Then
			Xtens.ImageA = "DigitB" & Int((Mul mod 100)/10)
		Else
			Xtens.ImageA = "DigitB"
		End If
	Else
		Xones.ImageA = "DigitB" & (mul mod 10)
	End If		
End Sub
'*************
' Save CurrentPlayer
'*************
Sub ResetEvents
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to1"
	Flasher010.visible = 0
	CountLogL = 0
	CountLogR = 0
'	MissionLose
'	TimMul = 30
'	DispXText TimMul
'	MissionDigitalCountTimer.Enabled = True
'	MissionLoseTimer.Enabled = True
	ModeInca3D
'	ModeEgypt3D
	ChangeBall(0)
	Gems_Sav(CurrentPlayer) = 0
	Relics_Sav(CurrentPlayer) = 0
	Mission_Relic1(CurrentPlayer) = 0
	Mission_Relic2a(CurrentPlayer) = 0
	Mission_Relic2b(CurrentPlayer) = 0
	Mission_Relic3(CurrentPlayer) = 0
	Mission_Relic4(CurrentPlayer) = 0
	Mission_Relic5(CurrentPlayer) = 0
	ModeRelic1Active(CurrentPlayer) = False
	ModeRelic2aActive(CurrentPlayer) = False
	ModeRelic2bActive(CurrentPlayer) = False
	ModeRelic3Active(CurrentPlayer) = False
	ModeRelic4Active(CurrentPlayer) = False
	ModeRelic5Active(CurrentPlayer) = False
	ModeRelicAllActive(CurrentPlayer) = False
	bModeSouterrainActive = False
'	bModeMiniGameActive = False
	ModeMissionActive(CurrentPlayer) = False
	ModeSniperShooting(CurrentPlayer) = False
	LetterCountC(CurrentPlayer) = 0
	LetterCountR(CurrentPlayer) = 0
	LetterCountO(CurrentPlayer) = 0
	LetterCountF(CurrentPlayer) = 0
	LetterCountT(CurrentPlayer) = 0
	bModePbgActive = False
	SpinnerCount = 50
	ModeBumpersActive(CurrentPlayer) = False
	BumperCount(CurrentPlayer) = 15
	ExtraBCount = 0
	SniperShootingCount = 0
	RampSoutCount = 0
	RampMCount = 0
	RampLCount = 0
	GunSeqENDTimer.Enabled = 1
	RampRCount = 0
	Mode_Multiball_End
	SolURFlipper 0
	SolULFlipper 0
	SolU2LFlipper 0
	FlInitBumper 1, "blacklight"
	FlInitBumper 2, "blacklight"
	FlInitBumper 3, "blacklight"
	LiA001.state=0
	LiA002.state=0
	LiA003.state=0
	LiA004.state=0
	LiA005.state=0
	ResetTargetLara
	ResetTargetCroft
	ResetLightRaider
	ResetLightRelic
	l001.State = 0
End Sub

Sub CheckLightGems
	If Gems_Sav(CurrentPlayer) = 0 Then
	LiA001.state=0
	LiA002.state=0
	LiA003.state=0
	LiA004.state=0
	LiA005.state=0
	Elseif Gems_Sav(CurrentPlayer) = 1 Then
	LiA001.state=1
	LiA002.state=0
	LiA003.state=0
	LiA004.state=0
	LiA005.state=0
	Elseif Gems_Sav(CurrentPlayer) = 2 Then
	LiA001.state=1
	LiA002.state=1
	LiA003.state=0
	LiA004.state=0
	LiA005.state=0
	Elseif Gems_Sav(CurrentPlayer) = 3 Then
	LiA001.state=1
	LiA002.state=1
	LiA003.state=1
	LiA004.state=0
	LiA005.state=0
	Elseif Gems_Sav(CurrentPlayer) = 4 Then
	LiA001.state=1
	LiA002.state=1
	LiA003.state=1
	LiA004.state=1
	LiA005.state=0
	Elseif Gems_Sav(CurrentPlayer) = 5 Then
	LiA001.state=1
	LiA002.state=1
	LiA003.state=1
	LiA004.state=1
	LiA005.state=1
	End If
End Sub
Sub CheckLightRelic
	If Mission_Relic1 (CurrentPlayer) = 1 Then
	LiB001.state=1
	Elseif Mission_Relic1 (CurrentPlayer) = 0 Then
	LiB001.state=0
	End If
	If Mission_Relic2a(CurrentPlayer) = 1 Then
	LiB002a.state=1
	Elseif Mission_Relic2a(CurrentPlayer) = 0 Then
	LiB002a.state=0
	End If
	If Mission_Relic2b(CurrentPlayer) = 1 Then
	LiB002b.state=1
	Elseif Mission_Relic2b(CurrentPlayer) = 0 Then
	LiB002b.state=0
	End If
	If Mission_Relic3(CurrentPlayer) = 1 Then
	LiB003.state=1
	Elseif Mission_Relic3(CurrentPlayer) = 0 Then
	LiB003.state=0
	End If
	If Mission_Relic4(CurrentPlayer) = 1 Then
	LiB004.state=1
	Elseif Mission_Relic4(CurrentPlayer) = 0 Then
	LiB004.state=0
	End If
	If Mission_Relic5(CurrentPlayer) = 1 Then
	LiB005.state=1
	Elseif Mission_Relic5(CurrentPlayer) = 0 Then
	LiB005.state=0
	End If
End Sub

Sub CheckMissionActive
	If ModeMissionActive(CurrentPlayer) = True Then
	LightR001.State = 1 
	LightR002.State = 1 
	LightR003.State = 1 
    LightR004.State = 1 
    LightR005.State = 1 
	LiMission.State = 2
	pupDMDDisplay "attract","","@StartMission.mp4",8,0,15
	PlaySound "Vo_StartMiss"
	DOF 126, DOFPulse
	StopHITpoint
	Elseif ModeMissionActive(CurrentPlayer) = False Then
	CheckLightCroft
	LiMission.State = 0
	End If
End Sub
Sub CheckLightCroft
	If ModeRelic1Active(CurrentPlayer) = False And ModeRelic2aActive(CurrentPlayer) = False And ModeRelic2bActive(CurrentPlayer) = False And ModeRelic3Active(CurrentPlayer) = False And ModeRelic4Active(CurrentPlayer) = False And ModeRelic5Active(CurrentPlayer) = False And ModeRelicAllActive(CurrentPlayer) = False Then
		If LetterCountC(CurrentPlayer) = 1 Then 
		LightR001.State = 1
		Elseif LetterCountC(CurrentPlayer) = 0 Then 
		LightR001.State = 2
		End If
		If LetterCountR(CurrentPlayer) = 1 Then
		LightR002.State = 1
		Elseif LetterCountR(CurrentPlayer) = 0 Then 
		LightR002.State = 2
		End If
		If LetterCountO(CurrentPlayer) = 1 Then 
		LightR003.State = 1
		Elseif LetterCountO(CurrentPlayer) = 0 Then 
		LightR003.State = 2
		End If
		If LetterCountF(CurrentPlayer) = 1 Then 
		LightR004.State = 1
		Elseif LetterCountF(CurrentPlayer) = 0 Then 
		LightR004.State = 2
		End If
		If LetterCountT(CurrentPlayer) = 1 Then 
		LightR005.State = 1
		Elseif LetterCountT(CurrentPlayer) = 0 Then 
		LightR005.State = 2
		End If
	Else
		LightR001.State = 0
		LightR002.State = 0
		LightR003.State = 0
		LightR004.State = 0
		LightR005.State = 0
	End If
End Sub
Sub CheckLightMission
	CheckRestartMissionActive
	CheckMissionActive
	CheckLightGems
	CheckLightRelic
	ResetTargetLara
	ResetLightRaider
	CheckMBLock_Save
	ExtraBCount=0
	SniperShootingCount = 0
	RampSoutCount=0
	LiEXTRA.state=0
	Flasher001.visible=1 'Debug Souterrain
	ShakeLogLOFF
	ShakeLogROFF
	GiOn
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to1"
	l2.state=0
	l3.state=0
	l4.state=0
	l5.state=0
	l6.state=0
	l7.state=0
	Light012.state=0
	Light013.state=0
	Light012.color = RGB(255, 0, 0) 'red
	Light012.colorfull = RGB(255, 0, 0)
	Light013.color = RGB(255, 0, 0)
	Light013.colorfull = RGB(255, 0, 0)
End Sub
'***********
' Moove LOG R/L
'***********
Dim CountLogL
Dim CountLogR
Sub LogLOFFTimer_timer()
LogL.TransZ = LogL.TransZ - 1
if LogL.TransZ <= 1 then LogLOFFTimer.enabled = 0 : Wall051.collidable = 0 : CountLogL = 0
End Sub
Sub LogLONTimer_timer()
LogL.TransZ = LogL.TransZ + 1
if LogL.TransZ >= 70 then LogLONTimer.enabled = 0 : Wall051.collidable = 1 : CountLogL = 1
End Sub

Sub ShakeLogLOFF 'open DOWN
	If LogLONTimer.enabled = 0 Then
	LogLOFFTimer.enabled = 1
	Playsound "fx_Log"
	End If
End Sub
Sub ShakeLogLON 'Close UP
	If LogLOFFTimer.enabled = 0 Then
	LogLONTimer.enabled = 1
	Playsound "fx_Log"
	End If
End Sub

Sub LogROFFTimer_timer()
LogR.TransZ = LogR.TransZ - 1
if LogR.TransZ <= 1 then LogROFFTimer.enabled = 0 : Wall052.collidable = 0 : CountLogR = 0
End Sub
Sub LogRONTimer_timer()
LogR.TransZ = LogR.TransZ + 1
if LogR.TransZ >= 70 then LogRONTimer.enabled = 0 : Wall052.collidable = 1 : CountLogR = 1
End Sub

Sub ShakeLogROFF 'open DOWN
'	Wall044.Collidable = False
	If LogRONTimer.enabled = 0 Then
	LogROFFTimer.enabled = 1
	Playsound "fx_Log"
	End If
End Sub
Sub ShakeLogRON 'Close UP
'	Wall044.Collidable = True
	If LogROFFTimer.enabled = 0 Then
	LogRONTimer.enabled = 1
	Playsound "fx_Log"
	End If
End Sub
Sub TriggerLogL_Hit()
	If CountLogL >= 1 And CountLogL <= 2 Then
		CountLogL = CountLogL + 1 
	Elseif CountLogL >= 3 Then 
		ShakeLogLOFF
	End If
End Sub
Sub TriggerLogR_Hit()
	If CountLogR >= 1 And CountLogR <= 2 Then
		CountLogR = CountLogR + 1 
	Elseif CountLogR >= 3 Then 
		ShakeLogROFF
	End If
End Sub
'***********
' Moove Lara - Animation
'***********
'***********BourneGuns***************

Dim	Gun1Pos, Frames, ShotgunBlast
Frames = Array("MF-000", "MF-001", "MF-002", "MF-003", "MF-004", "MF-005", "MF-006", "MF-007", "MF-008", "MF-009", "MF-010", "MF-011", "MF-012", "MF-013", "MF-014", "MF-015", "MF-016", "MF-017", "MF-018", "MF-019", "MF-020", _
"MF-021", "MF-022", "MF-023", "MF-024", "MF-025", "MF-026", "MF-027", "MF-028", "MF-029")

Sub StartGun
	ShakeLaraHeadOFF
	GunFlash.visible = 1
	Gun1Pos = 0
	GunTimer.Enabled = 1
	PlaySound "fx_Gun"
End Sub

Sub GunTimer_timer
	GunFlash.ImageA = Frames(Gun1Pos)
	Gun1Pos = (Gun1Pos + 1) MOD 30
	vpmtimer.addtimer 1500, "StopGun '"
End Sub

Sub StopGun
	GunFlash.visible = 0
	GunSeqENDTimer.Enabled = 1
	GunTimer.Enabled = 0
End Sub
Sub GunSeqENDTimer_Timer()
	ShakeLaraGunOFF
	GunSeqENDTimer.Enabled = 0
End Sub

Sub LaraGunOFFTimer_timer()
LaraBras.RotY = LaraBras.RotY - 1
LaraBras.ObjRotY = LaraBras.ObjRotY - 1
LaraGun001.RotY = LaraGun001.RotY - 1
LaraGun001.ObjRotY = LaraGun001.ObjRotY - 1
if LaraBras.RotY <= -100 then LaraGunOFFTimer.enabled = 0 : Playsound "fx_pistol" : ShakeLaraHeadOFF
End Sub
Sub LaraGunONTimer_timer()
LaraBras.RotY = LaraBras.RotY + 1
LaraBras.ObjRotY = LaraBras.ObjRotY + 1
LaraGun001.RotY = LaraGun001.RotY + 1
LaraGun001.ObjRotY = LaraGun001.ObjRotY + 1
if LaraBras.RotY >= -55 then LaraGunONTimer.enabled = 0
End Sub

Sub ShakeLaraGunOFF
	If LaraGunONTimer.enabled = 0 Then
	LaraGunONTimer.enabled = 1
	End If
End Sub
Sub ShakeLaraGunON
	If LaraGunOFFTimer.enabled = 0 Then
	LaraGunOFFTimer.enabled = 1
	End If
End Sub
'***********
Sub LaraHeadOFFTimer_timer()
LaraFace.RotY = LaraFace.RotY + 1
LaraHair.RotY = LaraHair.RotY + 1
if LaraFace.RotY >= -25 then LaraHeadOFFTimer.enabled = 0
End Sub
Sub LaraHeadONTimer_timer()
LaraFace.RotY = LaraFace.RotY - 1
LaraHair.RotY = LaraHair.RotY - 1
if LaraFace.RotY <= -70 then LaraHeadONTimer.enabled = 0
End Sub

Sub ShakeLaraHeadOFF
	If LaraHeadONTimer.enabled = 0 Then
	LaraHeadONTimer.enabled = 1
	End If
End Sub
Sub ShakeLaraHeadON
	If LaraHeadOFFTimer.enabled = 0 Then 'And liLOCK.state = 0 And liMB.state = 0 Then
	LaraHeadOFFTimer.enabled = 1
	End If
End Sub
'***********
' Chute d'eau - Animation
'***********
Dim ChutePos, ChuteFrames, Chute2Pos, Chute2Frames
ChuteFrames = Array("PortalR001", "PortalR002", "PortalR003")
Chute2Frames = Array("PortalR", "PortalR", "PortalR2", "PortalR3", "PortalR3", "PortalR3", "PortalR3", "PortalR3", "PortalR3", "PortalR3", "PortalR3", "PortalR3", "PortalR3", "PortalR2", "PortalR")

Sub StartChute
	ChutePos = 0
	Chute2Pos = 0
    ChuteTimer.Enabled = 1
End Sub

Sub ChuteTimer_Timer
    Flasher004.ImageA = ChuteFrames (ChutePos)
	Flasher005.ImageA = ChuteFrames (ChutePos)
	Flasher002.ImageA = Chute2Frames (Chute2Pos)
    ChutePos = (ChutePos + 1) MOD 003
	Chute2Pos = (Chute2Pos + 1) MOD 015
End Sub
'***********
' Turn Aiguille boussole - Animation
'***********
sub timer1_Timer()
Aiguille.rotY=rainspin1.currentangle
end Sub

Sub Rainspin1_Spin
    PlaySoundAt "fx_spinner", rainspin1
    If Tilted Then Exit Sub
    Addscore 175
	If ModeSpinnersActive(CurrentPlayer) = true Then
		SpinnerCount = SpinnerCount - 1
		DispXText SpinnerCount
		If SpinnerCount = 0 Then
			ModeSpinnersActive(CurrentPlayer) = False
			Mode_Relic5_end
		end If
	end If
End Sub
Sub Rainspin2_Spin
    PlaySoundAt "fx_spinner", rainspin2
    If Tilted Then Exit Sub
    Addscore 175
	If ModeSpinnersActive(CurrentPlayer) = true Then
		SpinnerCount = SpinnerCount - 1
		DispXText SpinnerCount
		If SpinnerCount = 0 Then
			ModeSpinnersActive(CurrentPlayer) = False
			Mode_Relic5_end
		end If
	end If
End Sub
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
    TempState = LeftInlane.State
    LeftInlane.State = LeftInlane2.State
    LeftInlane2.State = LeftOutlane.State
    LeftOutlane.State = TempState
End Sub

Sub RotateLaneLightsRightUp
    Dim TempState
	TempState = RightInlane.State
    RightInlane.State = RightInlane2.State
    RightInlane2.State = RightOutlane.State
    RightOutlane.State = TempState
End Sub

'*************
' Multiball
'*************
Sub CheckLightMB
	If LightMB1.state=0 And LightMB2.state=0 And LightMB3.state=0 Then
	lightMB1.state=1
	MBLock_Sav(CurrentPlayer) = 1
	DOF 452, DOFPulse
	PlaySound "Vo_OneBallLock"
	pupDMDDisplay "balllock","Ball^is^Locked", "@balllocked.mp4",3, 1,13
	liLOCK.State = 0
	Elseif LightMB1.state=1 And LightMB2.state=0 And LightMB3.state=0 Then
	LightMB2.state=1
	MBLock_Sav(CurrentPlayer) = 2
	DOF 453, DOFPulse
	PlaySound "Vo_OneBallLock"
	pupDMDDisplay "balllock","Ball^is^Locked", "@balllocked.mp4",3, 1,13
	liLOCK.State = 0
	Elseif LightMB1.state=1 And LightMB2.state=1 And LightMB3.state=0 Then
	lightMB3.state=1
	liMB.State = 0
	Mode_Multiball_Start
	PlaySound "Vo_MB"
	pupDMDDisplay "Splash", "MULTIBALL", "", 5, 1, 15
	Elseif LightMB1.state=1 And LightMB2.state=1 And LightMB3.state=1 Then
	lightMB3.state=0
	liLOCK.State = 0
	End IF
End Sub
Sub CheckMBLock_Save 	
	If MBLock_Sav(CurrentPlayer) = 0 Then
	LightMB1.state=0 
	LightMB2.state=0
	LightMB3.state=0
	LiLOCK.state=0
	LiMB.state=0
	Elseif MBLock_Sav(CurrentPlayer) = 1 Then
	LightMB1.state=1 
	LightMB2.state=0
	LightMB3.state=0
	LiLOCK.state=0
	LiMB.state=0
	Elseif MBLock_Sav(CurrentPlayer) = 2 Then
	LightMB1.state=1 
	LightMB2.state=1
	LightMB3.state=0
	LiLOCK.state=0
	LiMB.state=0
	End If
End Sub

Sub AddBallBonusMBTimer_Timer()
	AddBallBonusMBTimer.enabled = False
End Sub

Sub Mode_Multiball_Start
	DOF 482, DOFOn
	ChangeBall(3) 'Rouge
	AddMBmission2
	AddMBmission
	EnableBallSaver 15
	AddScore 8000
	bMultiBallMode = True
	AddBallBonusMBTimer.enabled = True
	PuPEvent 123
	PlaySong "Mu_MB"
	LightMB1.state=2
	LightMB2.state=2 
	LightMB3.state=2
End Sub
Sub Mode_Multiball_End
	DOF 482, DOFOff
	ChangeBall(0)
	bMultiBallMode = False
	PuPEvent 124
	LightMB1.state=0
	LightMB2.state=0 
	LightMB3.state=0
	MBLock_Sav(CurrentPlayer) = 0
End Sub

Sub AddMBmission
	EnableBallSaver 15
	KickerAuto.CreateBall
	KickerAuto.Kick 10, 41
	PlaySoundAt "fx_fire", Trigger1
	DOF 132, DOFPulse
	BallsOnPlayfield = BallsOnPlayfield +1
End Sub
Sub AddMBmission2 'passage
	Kicker004.CreateSizedBallWithMass BallSize / 2, BallMass / 2
	Kicker004.TimerInterval = 4000
	Kicker004.TimerEnabled = True
	BallsOnPlayfield = BallsOnPlayfield +1
End Sub
'*************
' Missions RELICS
'*************
Sub BumperGIEffect(n)
    Select Case n

	  Case 1 
			Gi1.color = RGB(0, 0, 255) 'Blue
			Gi2.color = RGB(0, 0, 255)
			Gi3.color = RGB(0, 0, 255)
			Gi4.color = RGB(0, 0, 255)
			Gi5.color = RGB(0, 0, 255) 'Blue
			Gi6.color = RGB(0, 0, 255)
			Gi7.color = RGB(0, 0, 255)
			Gi8.color = RGB(0, 0, 255)
			Gi9.color = RGB(0, 0, 255)
			Gi1.colorfull = RGB(0, 0, 255)
			Gi2.colorfull = RGB(0, 0, 255)
			Gi3.colorfull = RGB(0, 0, 255)
			Gi4.colorfull = RGB(0, 0, 255)
			Gi5.colorfull = RGB(0, 0, 255)
			Gi6.colorfull = RGB(0, 0, 255)
			Gi7.colorfull = RGB(0, 0, 255)
			Gi8.colorfull = RGB(0, 0, 255)
			Gi9.colorfull = RGB(0, 0, 255)
			FlInitBumper 1, "blue"
			FlInitBumper 2, "blue"
			FlInitBumper 3, "blue"
	  Case 2 
			Gi1.color = RGB(255, 128, 0) 'ORANGE
			Gi2.color = RGB(255, 128, 0)
			Gi3.color = RGB(255, 128, 0)
			Gi4.color = RGB(255, 128, 0)
			Gi5.color = RGB(255, 128, 0)
			Gi6.color = RGB(255, 128, 0)
			Gi7.color = RGB(255, 128, 0)
			Gi8.color = RGB(255, 128, 0)
			Gi9.color = RGB(255, 128, 0)
			Gi1.colorfull = RGB(255, 128, 0)
			Gi2.colorfull = RGB(255, 128, 0)
			Gi3.colorfull = RGB(255, 128, 0)
			Gi4.colorfull = RGB(255, 128, 0)
			Gi5.colorfull = RGB(255, 128, 0)
			Gi6.colorfull = RGB(255, 128, 0)
			Gi7.colorfull = RGB(255, 128, 0)
			Gi8.colorfull = RGB(255, 128, 0)
			Gi9.colorfull = RGB(255, 128, 0)
			FlInitBumper 1, "blacklight"
			FlInitBumper 2, "blacklight"
			FlInitBumper 3, "blacklight"
      Case 3 
			Gi1.color = RGB(0, 255, 0) 'GREEN
			Gi2.color = RGB(0, 255, 0)
			Gi3.color = RGB(0, 255, 0)
			Gi4.color = RGB(0, 255, 0)
			Gi5.color = RGB(0, 255, 0)
			Gi6.color = RGB(0, 255, 0)
			Gi7.color = RGB(0, 255, 0)
			Gi8.color = RGB(0, 255, 0)
			Gi9.color = RGB(0, 255, 0)
			Gi1.colorfull = RGB(0, 255, 0)
			Gi2.colorfull = RGB(0, 255, 0)
			Gi3.colorfull = RGB(0, 255, 0)
			Gi4.colorfull = RGB(0, 255, 0)
			Gi5.colorfull = RGB(0, 255, 0)
			Gi6.colorfull = RGB(0, 255, 0)
			Gi7.colorfull = RGB(0, 255, 0)
			Gi8.colorfull = RGB(0, 255, 0)
			Gi9.colorfull = RGB(0, 255, 0)
			FlInitBumper 1, "green"
			FlInitBumper 2, "green"
			FlInitBumper 3, "green"
      Case 4 
			Gi1.color = RGB(255, 0, 0) 'RED
			Gi2.color = RGB(255, 0, 0)
			Gi3.color = RGB(255, 0, 0)
			Gi4.color = RGB(255, 0, 0)
			Gi5.color = RGB(255, 0, 0)
			Gi6.color = RGB(255, 0, 0)
			Gi7.color = RGB(255, 0, 0)
			Gi8.color = RGB(255, 0, 0)
			Gi9.color = RGB(255, 0, 0)
			Gi1.colorfull = RGB(255, 0, 0)
			Gi2.colorfull = RGB(255, 0, 0)
			Gi3.colorfull = RGB(255, 0, 0)
			Gi4.colorfull = RGB(255, 0, 0)
			Gi5.colorfull = RGB(255, 0, 0)
			Gi6.colorfull = RGB(255, 0, 0)
			Gi7.colorfull = RGB(255, 0, 0)
			Gi8.colorfull = RGB(255, 0, 0)
			Gi9.colorfull = RGB(255, 0, 0)
			FlInitBumper 1, "red"
			FlInitBumper 2, "red"
			FlInitBumper 3, "red"
	  Case Else	
	  end Select
end Sub

Sub ResetLightRelic
	LightM1001.color = RGB(0, 0, 255) 'Blue
	LightM1002.color = RGB(0, 0, 255) 'Blue
	LightM1003.color = RGB(0, 0, 255) 'Blue
	LightM2A001.color = RGB(0, 0, 255) 'Blue
	LightM2A002.color = RGB(0, 0, 255) 'Blue
	LightM2A003.color = RGB(0, 0, 255) 'Blue
	LightM2B001.color = RGB(0, 0, 255) 'Blue
	LightM2B002.color = RGB(0, 0, 255) 'Blue
	LightM2B003.color = RGB(0, 0, 255) 'Blue
	LightM3001.color = RGB(0, 0, 255) 'Blue
	LightM3002.color = RGB(0, 0, 255) 'Blue
	LightM3003.color = RGB(0, 0, 255) 'Blue
	LightM4001.color = RGB(0, 0, 255) 'Blue
	LightM4002.color = RGB(0, 0, 255) 'Blue
	LightM4003.color = RGB(0, 0, 255) 'Blue
	LightM5001.color = RGB(0, 0, 255) 'Blue
	LightM5002.color = RGB(0, 0, 255) 'Blue
	LightM5003.color = RGB(0, 0, 255) 'Blue
	LightM1001.colorfull = RGB(0, 0, 255) 'Blue
	LightM1002.colorfull = RGB(0, 0, 255) 'Blue
	LightM1003.colorfull = RGB(0, 0, 255) 'Blue
	LightM2A001.colorfull = RGB(0, 0, 255) 'Blue
	LightM2A002.colorfull = RGB(0, 0, 255) 'Blue
	LightM2A003.colorfull = RGB(0, 0, 255) 'Blue
	LightM2B001.colorfull = RGB(0, 0, 255) 'Blue
	LightM2B002.colorfull = RGB(0, 0, 255) 'Blue
	LightM2B003.colorfull = RGB(0, 0, 255) 'Blue
	LightM3001.colorfull = RGB(0, 0, 255) 'Blue
	LightM3002.colorfull = RGB(0, 0, 255) 'Blue
	LightM3003.colorfull = RGB(0, 0, 255) 'Blue
	LightM4001.colorfull = RGB(0, 0, 255) 'Blue
	LightM4002.colorfull = RGB(0, 0, 255) 'Blue
	LightM4003.colorfull = RGB(0, 0, 255) 'Blue
	LightM5001.colorfull = RGB(0, 0, 255) 'Blue
	LightM5002.colorfull = RGB(0, 0, 255) 'Blue
	LightM5003.colorfull = RGB(0, 0, 255) 'Blue
	Gi1.color = RGB(0, 0, 255) 'Blue
	Gi2.color = RGB(0, 0, 255)
	Gi3.color = RGB(0, 0, 255)
	Gi4.color = RGB(0, 0, 255)
	Gi5.color = RGB(0, 0, 255) 'Blue
	Gi6.color = RGB(0, 0, 255)
	Gi7.color = RGB(0, 0, 255)
	Gi8.color = RGB(0, 0, 255)
	Gi9.color = RGB(0, 0, 255)
	Gi1.colorfull = RGB(0, 0, 255)
	Gi2.colorfull = RGB(0, 0, 255)
	Gi3.colorfull = RGB(0, 0, 255)
	Gi4.colorfull = RGB(0, 0, 255)
	Gi5.colorfull = RGB(0, 0, 255)
	Gi6.colorfull = RGB(0, 0, 255)
	Gi7.colorfull = RGB(0, 0, 255)
	Gi8.colorfull = RGB(0, 0, 255)
	Gi9.colorfull = RGB(0, 0, 255)
	l001.color = RGB(255, 128, 0) 'ORANGE
	l001.colorfull = RGB(255, 128, 0)
	bumperbiglight1.state = 1
	bumperbiglight2.state = 1
	bumperbiglight3.state = 1
	FlInitBumper 1, "blacklight"
	FlInitBumper 2, "blacklight"
	FlInitBumper 3, "blacklight"
	LightM1001.state = 0
	LightM1002.state = 0
	LightM1003.state = 0
	LightM2A001.State =0
	LightM2A002.State =0
	LightM2A003.State =0
	LightM2B001.state = 0
	LightM2B002.state = 0
	LightM2B003.state = 0
	LightM3001.state = 0
	LightM3002.state = 0
	LightM3003.state = 0
	LightM4001.state = 0
	LightM4002.state = 0
	LightM4003.state = 0
	LightM5001.State =0
	LightM5002.State =0
	LightM5003.State =0
	MissionDigitalCountTimer.Enabled = False
	Xtens.ImageA = "DigitB"
	Xones.ImageA = "DigitB"
	Flasher010.visible = 0
End Sub

Sub MissionLose
	ResetLightRelic
	MissionDigitalCountTimer.Enabled = False
	Flasher010.visible = 0
'	ModeRelic1Active(CurrentPlayer) = False
	ModeRelic2aActive(CurrentPlayer) = False
	ModeRelic2bactive(CurrentPlayer) = False
	ModeRelic3Active(CurrentPlayer) = False
	ModeRelic4Active(CurrentPlayer) = False
'	ModeRelic5Active(CurrentPlayer) = False
'	ModeBumpersActive(CurrentPlayer) = False
'	BumperCount(CurrentPlayer) = 15
'	ModeSpinnersActive(CurrentPlayer) = True
	ChangeBall(0)
	CheckLightRelic
	ResetTargetCroft
	AddScore 15000
	Playsound "Vo_Loose"
	pupDMDDisplay "Splash4","YOU LOST^RELIC MISSION","",3,1,20
End Sub

Sub Mode_Relic1_start ' BUMPERS HITS
	ModeRelic1Active(CurrentPlayer) = True
	ModeBumpersActive(CurrentPlayer) = True
	DispXText BumperCount(CurrentPlayer)
	Flasher010.ImageA = "MissBumper"
	Flasher010.visible = 1
	ModeMissionActive(CurrentPlayer) = False
	PuPEvent 106
	LightM1001.state = 2
	LightM5001.state = 2
	LightM1001.color = RGB(255, 128, 0) 'ORANGE
	LightM1001.colorfull= RGB(255, 128, 0) 
	LightM5001.color = RGB(255, 128, 0) 'ORANGE
	LightM5001.colorfull= RGB(255, 128, 0) 
	bumperbiglight1.state = 2
	bumperbiglight2.state = 2
	bumperbiglight3.state = 2
	Gi1.color = RGB(255, 128, 0) 'ORANGE
	Gi2.color = RGB(255, 128, 0)
	Gi3.color = RGB(255, 128, 0)
	Gi4.color = RGB(255, 128, 0)
	Gi1.colorfull = RGB(255, 128, 0)
	Gi2.colorfull = RGB(255, 128, 0)
	Gi3.colorfull = RGB(255, 128, 0)
	Gi4.colorfull = RGB(255, 128, 0)
'	BumperCount(CurrentPlayer) = 20
	Wall048.Collidable = 1
	LiB001.state = 2
	FlInitBumper 1, "orange"
	FlInitBumper 2, "orange"
	FlInitBumper 3, "orange"
	ModeEgypt3D
End Sub
Sub Mode_Relic1_end 
'	MissionLoseTimer.Enabled = False
	ModeRelic1Active(CurrentPlayer) = False
	ModeBumpersActive(CurrentPlayer) = False
'	BumperCount(CurrentPlayer) = 15
	Mission_Relic1(CurrentPlayer) = 1
	RelicsBonus = RelicsBonus + 1
	AddScore 80000
	PupEvent 112
	Relics_Sav(CurrentPlayer) = Relics_Sav(CurrentPlayer) + 1
	pupDMDDisplay "attract","Relic","@Relic.mp4",8,0,16
	LightM1001.state = 0
	LightM5001.state = 0
	bumperbiglight1.state = 1
	bumperbiglight2.state = 1
	bumperbiglight3.state = 1
	LiB001.State = 1
	PlaySound"fx_Objet"
	PlaySound "Vo_Relic"
	Wall048.Collidable = 0
	ResetLightRelic
	ResetTargetCroft
	StartHITpoint
End Sub

Sub Mode_Relic2a_start ' RAMP L
	TimMul = 45
	DispXText TimMul
	MissionDigitalCountTimer.Enabled = True
	ModeRelic2aActive(CurrentPlayer) = True
	ModeMissionActive(CurrentPlayer) = False
	ChangeBall(1)
	PuPEvent 107
	LightM2B001.state = 2
	LightM2B002.state = 2
	LightM2B003.state = 2
	l001.color = RGB(0, 0, 255) 'BLEU
	l001.colorfull = RGB(0, 0, 255)
	FlInitBumper 1, "blue"
	FlInitBumper 2, "blue"
	FlInitBumper 3, "blue"
	LiB002a.state = 2
	ModeInca3D
	StopSong
	PlaySong "Mu_3"
End Sub
Sub Mode_Relic2a_end 
	MissionDigitalCountTimer.Enabled = False
	ModeRelic2aActive(CurrentPlayer) = False
	Mission_Relic2a(CurrentPlayer) = 1
	Relics_Sav(CurrentPlayer) = Relics_Sav(CurrentPlayer) + 1
	RelicsBonus = RelicsBonus + 1
	ChangeBall(0)
	AddScore 80000
	PupEvent 113
	pupDMDDisplay "attract","Relic","@Relic.mp4",8,0,16
	LightM2B001.state = 0
	LightM2B002.state = 0
	LightM2B003.state = 0
	LiB002a.State = 1	
	PlaySound"fx_Objet"
	PlaySound "Vo_Relic"
	ResetLightRelic
	ResetTargetCroft
	StartHITpoint
End Sub
Sub Mode_Relic2b_start ' RAMP R
	TimMul = 60
	DispXText TimMul
	MissionDigitalCountTimer.Enabled = True
'	MissionLoseTimer.Enabled = True
	ModeRelic2bActive(CurrentPlayer) = True
	ModeMissionActive(CurrentPlayer) = False
	ChangeBall(1)
	PuPEvent 108
	LightM3001.state = 2
	LightM3002.state = 2
	LightM3003.state = 2
	l001.color = RGB(0, 0, 255) 'BLEU
	l001.colorfull = RGB(0, 0, 255)
	FlInitBumper 1, "blue"
	FlInitBumper 2, "blue"
	FlInitBumper 3, "blue"
	LiB002b.state = 2
	ModeInca3D
	StopSong
	PlaySong "Mu_3"
End Sub
Sub Mode_Relic2b_end 
	MissionDigitalCountTimer.Enabled = False
	ModeRelic2bActive(CurrentPlayer) = False
	Mission_Relic2b(CurrentPlayer) = 1
	Relics_Sav(CurrentPlayer) = Relics_Sav(CurrentPlayer) + 1
	RelicsBonus = RelicsBonus + 1
	ChangeBall(0)
	AddScore 70000
	PupEvent 114
	pupDMDDisplay "attract","Relic","@Relic.mp4",8,0,16
	LightM3001.state = 0
	LightM3002.state = 0
	LightM3003.state = 0
	PlaySound"fx_Objet"
	PlaySound "Vo_Relic"
	LiB002b.State = 1
	ResetLightRelic
	ResetTargetCroft
	StartHITpoint
End Sub

Sub Mode_Relic3_start ' ALL RAMPS/LINE x1
	TimMul = 109
'	DispXText TimMul
	MissionDigitalCountTimer.Enabled = True
	ModeRelic3Active(CurrentPlayer) = True
	ModeMissionActive(CurrentPlayer) = False
	PuPEvent 130
	ChangeBall(2)
	LightM1001.state = 2
	LightM2A001.state = 2
	LightM2B001.state = 2
	LightM3001.state = 2
	LightM4001.state = 2
	LightM5001.state = 2
	LightM1001.color = RGB(0, 255, 0) 'GREEN
	LightM2A001.color = RGB(0, 255, 0)
	LightM2B001.color = RGB(0, 255, 0)
	LightM3001.color = RGB(0, 255, 0)
	LightM4001.color = RGB(0, 255, 0)
	LightM5001.color = RGB(0, 255, 0)
	LightM1001.colorfull = RGB(0, 255, 0)
	LightM2A001.colorfull = RGB(0, 255, 0)
	LightM2B001.colorfull = RGB(0, 255, 0)
	LightM3001.colorfull = RGB(0, 255, 0)
	LightM4001.colorfull = RGB(0, 255, 0)
	LightM5001.colorfull = RGB(0, 255, 0)
	Gi1.color = RGB(0, 255, 0) 'GREEN
	Gi2.color = RGB(0, 255, 0)
	Gi3.color = RGB(0, 255, 0)
	Gi4.color = RGB(0, 255, 0)
	Gi1.colorfull = RGB(0, 255, 0)
	Gi2.colorfull = RGB(0, 255, 0)
	Gi3.colorfull = RGB(0, 255, 0)
	Gi4.colorfull = RGB(0, 255, 0)
	Gi5.color = RGB(0, 255, 0)
	Gi5.colorfull = RGB(0, 255, 0)
	Gi6.color = RGB(0, 255, 0)
	Gi6.colorfull = RGB(0, 255, 0)
	Gi7.color = RGB(0, 255, 0)
	Gi7.colorfull = RGB(0, 255, 0)
	Gi8.color = RGB(0, 255, 0)
	Gi8.colorfull = RGB(0, 255, 0)
	Gi9.color = RGB(0, 255, 0)
	Gi9.colorfull = RGB(0, 255, 0)
	l001.color = RGB(0, 255, 0) 
	l001.colorfull = RGB(0, 255, 0)
	FlInitBumper 1, "green"
	FlInitBumper 2, "green"
	FlInitBumper 3, "green"
	LiB003.state = 2
	ModeInca3D
	StopSong
	PlaySong "Mu_3"
End Sub
Sub Mode_Relic3_end 
	MissionDigitalCountTimer.Enabled = False
	ModeRelic3Active(CurrentPlayer) = False
	Mission_Relic3(CurrentPlayer) = 1
	Relics_Sav(CurrentPlayer) = Relics_Sav(CurrentPlayer) + 1
	RelicsBonus = RelicsBonus + 1
	ChangeBall(0)
	AddScore 70000
	PupEvent 115
	pupDMDDisplay "attract","Relic","@Relic.mp4",8,0,16
	LightM1001.state = 0
	LightM2A001.state = 0
	LightM2B001.state = 0
	LightM3001.state = 0
	LightM4001.state = 0
	LightM5001.state = 0
	PlaySound"fx_Objet"
	PlaySound "Vo_Relic"
	LiB003.State = 1
	ResetLightRelic
	ResetTargetCroft
	StartHITpoint
End Sub

Sub Mode_Relic4_start ' RAMP L x3 + RAMP R x3 + RAMP EXT x3
	TimMul = 300
'	DispXText TimMul
	MissionDigitalCountTimer.Enabled = True
	ModeRelic4Active(CurrentPlayer) = True
	ModeMissionActive(CurrentPlayer) = False
	AddMBmission2
	EnableBallSaver 15
	ChangeBall(3)
	PuPEvent 110
	LightM3001.state = 2
	LightM3002.state = 2
	LightM3003.state = 2
	LightM2B001.state = 2
	LightM2B002.state = 2
	LightM2B003.state = 2
	LightM4001.state = 2
	LightM4002.state = 2
	LightM4003.state = 2
	LightM3001.color = RGB(255, 0, 0) 'RED
	LightM3002.color = RGB(255, 0, 0)
	LightM3003.color = RGB(255, 0, 0)
	LightM2B001.color = RGB(255, 0, 0)
	LightM2B002.color = RGB(255, 0, 0)
	LightM2B003.color = RGB(255, 0, 0)
	LightM4001.color = RGB(255, 0, 0)
	LightM4002.color = RGB(255, 0, 0)
	LightM4003.color = RGB(255, 0, 0)
	LightM3001.colorfull = RGB(255, 0, 0)
	LightM3002.colorfull = RGB(255, 0, 0)
	LightM3003.colorfull = RGB(255, 0, 0)
	LightM2B001.colorfull = RGB(255, 0, 0)
	LightM2B002.colorfull = RGB(255, 0, 0)
	LightM2B003.colorfull = RGB(255, 0, 0)
	LightM4001.colorfull = RGB(255, 0, 0)
	LightM4002.colorfull = RGB(255, 0, 0)
	LightM4003.colorfull = RGB(255, 0, 0)
	Gi1.color = RGB(255, 0, 0) 'RED
	Gi2.color = RGB(255, 0, 0)
	Gi3.color = RGB(255, 0, 0)
	Gi4.color = RGB(255, 0, 0)
	Gi5.color = RGB(255, 0, 0)
	Gi6.color = RGB(255, 0, 0)
	Gi7.color = RGB(255, 0, 0)
	Gi8.color = RGB(255, 0, 0)
	Gi9.color = RGB(255, 0, 0)
	Gi1.colorfull = RGB(255, 0, 0)
	Gi2.colorfull = RGB(255, 0, 0)
	Gi3.colorfull = RGB(255, 0, 0)
	Gi4.colorfull = RGB(255, 0, 0)
	Gi5.colorfull = RGB(255, 0, 0)
	Gi6.colorfull = RGB(255, 0, 0)
	Gi7.colorfull = RGB(255, 0, 0)
	Gi8.colorfull = RGB(255, 0, 0)
	Gi9.colorfull = RGB(255, 0, 0)
	l001.color = RGB(255, 0, 0) 'Red
	l001.colorfull = RGB(255, 0, 0)
	FlInitBumper 1, "red"
	FlInitBumper 2, "red"
	FlInitBumper 3, "red"
	LiB004.state = 2
	ModeEgypt3D
End Sub
Sub Mode_Relic4_end 
	MissionDigitalCountTimer.Enabled = False
	ModeRelic4Active(CurrentPlayer) = False
	Mission_Relic4(CurrentPlayer) = 1
	Relics_Sav(CurrentPlayer) = Relics_Sav(CurrentPlayer) + 1
	RelicsBonus = RelicsBonus + 1
	ChangeBall(0)
	AddScore 100000
	PupEvent 116
	pupDMDDisplay "attract","Relic","@Relic.mp4",8,0,16
	LightM3001.state = 0
	LightM3002.state = 0
	LightM3003.state = 0
	LightM2B001.state = 0
	LightM2B002.state = 0
	LightM2B003.state = 0
	LightM4001.state = 0
	LightM4002.state = 0
	LightM4003.state = 0
	PlaySound"fx_Objet"
	PlaySound "Vo_Relic"
	LiB004.State = 1
	ResetLightRelic
	ResetTargetCroft
	StartHITpoint
End Sub

Sub Mode_Relic5_start 'Spinner
	ModeRelic5Active(CurrentPlayer) = True
	ModeSpinnersActive(CurrentPlayer) = True
	ModeMissionActive(CurrentPlayer) = False
	PuPEvent 111
	Flasher010.ImageA = "MissSpinner"
	Flasher010.visible = 1
	SpinnerCount = 50
	DispXText SpinnerCount
	LightM2A001.State =2
	LightM2A002.State =1
	LightM2A003.State =1
	LightM5001.State =2
	LightM5002.State =1
	LightM5003.State =1
	LightM2A001.color = RGB(255, 255, 0) 'YELL
	LightM2A002.color = RGB(255, 255, 0) 
	LightM2A003.color = RGB(255, 255, 0) 
	LightM2A001.colorfull = RGB(255, 255, 0) 
	LightM2A002.colorfull = RGB(255, 255, 0) 
	LightM2A003.colorfull = RGB(255, 255, 0) 
	LightM5001.color = RGB(255, 255, 0) 
	LightM5002.color = RGB(255, 255, 0) 
	LightM5003.color = RGB(255, 255, 0)  
	LightM5001.colorfull = RGB(255, 255, 0) 
	LightM5002.colorfull = RGB(255, 255, 0) 
	LightM5003.colorfull = RGB(255, 255, 0) 
	Gi1.color = RGB(255, 255, 0) 'YELL
	Gi2.color = RGB(255, 255, 0)
	Gi3.color = RGB(255, 255, 0)
	Gi4.color = RGB(255, 255, 0)
	Gi6.color = RGB(255, 255, 0)
	Gi7.color = RGB(255, 255, 0)
	Gi8.color = RGB(255, 255, 0)
	Gi9.color = RGB(255, 255, 0)
	Gi1.colorfull = RGB(255, 255, 0)
	Gi2.colorfull = RGB(255, 255, 0)
	Gi3.colorfull = RGB(255, 255, 0)
	Gi4.colorfull = RGB(255, 255, 0)
	Gi5.colorfull = RGB(255, 255, 0)
	Gi6.colorfull = RGB(255, 255, 0)
	Gi7.colorfull = RGB(255, 255, 0)
	Gi8.colorfull = RGB(255, 255, 0)
	Gi9.colorfull = RGB(255, 255, 0)
	l001.color = RGB(255, 255, 0) 
	l001.colorfull = RGB(255, 255, 0)
	FlInitBumper 1, "yellow"
	FlInitBumper 2, "yellow"
	FlInitBumper 3, "yellow"
	LiB005.state = 2
	ModeEgypt3D
End Sub
Sub Mode_Relic5_end 
	ModeRelic5Active(CurrentPlayer) = False
	ModeSpinnersActive(CurrentPlayer) = False
	Mission_Relic5(CurrentPlayer) = 1
	RelicsBonus = RelicsBonus + 1
	Relics_Sav(CurrentPlayer) = Relics_Sav(CurrentPlayer) + 1
	AddScore 60000
	PupEvent 117
	pupDMDDisplay "attract","Relic","@Relic.mp4",8,0,16
	LightM2A001.State =0
	LightM2A002.State =0
	LightM2A003.State =0
	LightM5001.State =0
	LightM5002.State =0
	LightM5003.State =0
	LiB005.State = 1
	PlaySound"fx_Objet"
	PlaySound "Vo_Relic"
	ResetLightRelic
	ResetTargetCroft
	StartHITpoint
End Sub

Sub Mode_RelicAll_start '
	ModeRelicAllActive(CurrentPlayer) = True
	ModeMissionActive(CurrentPlayer) = False
	ModeInca3D
	AddMBmission
	AddMBmission2
	PlaySong "Mu_MB"
	Gi1.color = RGB(255, 0, 0) 'RED
	Gi2.color = RGB(255, 0, 0)
	Gi3.color = RGB(255, 0, 0)
	Gi4.color = RGB(255, 0, 0)
	Gi1.colorfull = RGB(255, 0, 0)
	Gi2.colorfull = RGB(255, 0, 0)
	Gi3.colorfull = RGB(255, 0, 0)
	Gi4.colorfull = RGB(255, 0, 0)
	Gi5.color = RGB(255, 0, 0)
	Gi5.colorfull = RGB(255, 0, 0)
	Gi6.color = RGB(255, 0, 0)
	Gi6.colorfull = RGB(255, 0, 0)
	Gi7.color = RGB(255, 0, 0)
	Gi7.colorfull = RGB(255, 0, 0)
	Gi8.color = RGB(255, 0, 0)
	Gi8.colorfull = RGB(255, 0, 0)
	Gi9.color = RGB(255, 0, 0)
	Gi9.colorfull = RGB(255, 0, 0)
	l001.color = RGB(255, 0, 0) 'Red
	l001.colorfull = RGB(255, 0, 0)
	FlInitBumper 1, "red"
	FlInitBumper 2, "red"
	FlInitBumper 3, "red"
	LiB001.state = 2
	LiB002a.state = 2
	LiB002b.state = 2
	LiB003.state = 2
	LiB004.state = 2
	LiB005.state = 2
	LightM1001.state = 2
	LightM1002.state = 2
	LightM1003.state = 2
	LightM1001.color = RGB(255, 128, 0) 'ORANGE
	LightM1001.colorfull= RGB(255, 128, 0) 
	LightM1002.color = RGB(255, 128, 0) 
	LightM1002.colorfull= RGB(255, 128, 0) 
	LightM1003.color = RGB(255, 128, 0) 
	LightM1003.colorfull= RGB(255, 128, 0) 
	LightM2A001.State = 2
	LightM2A002.State = 2
	LightM2A003.State = 2
	LightM2A001.color = RGB(0, 0, 255) 'BLEU
	LightM2A002.color = RGB(0, 0, 255) 
	LightM2A003.color = RGB(0, 0, 255) 
	LightM2A001.colorfull = RGB(0, 0, 255) 
	LightM2A002.colorfull = RGB(0, 0, 255) 
	LightM2A003.colorfull = RGB(0, 0, 255) 
	LightM2B001.state = 2
	LightM2B002.state = 2
	LightM2B003.state = 2
	LightM2B001.color = RGB(0, 0, 255) 'BLEU
	LightM2B002.color = RGB(0, 0, 255) 'BLEU 
	LightM2B003.color = RGB(0, 0, 255) 'BLEU
	LightM2B001.colorfull = RGB(0, 0, 255) 'BLEU
	LightM2B002.colorfull = RGB(0, 0, 255) 'BLEU 
	LightM2B003.colorfull = RGB(0, 0, 255) 'BLEU
	LightM3001.state = 2
	LightM3002.state = 2
	LightM3003.state = 2
	LightM3001.color = RGB(0, 255, 0) 'GREEN
	LightM3001.colorfull= RGB(0, 255, 0) 
	LightM3002.color = RGB(0, 255, 0) 
	LightM3002.colorfull= RGB(0, 255, 0) 
	LightM3003.color = RGB(0, 255, 0)
	LightM3003.colorfull= RGB(0, 255, 0)
	LightM4001.state = 2
	LightM4002.state = 2
	LightM4003.state = 2
	LightM4001.color = RGB(255, 0, 0) 'RED
	LightM4002.color = RGB(255, 0, 0)
	LightM4003.color = RGB(255, 0, 0)
	LightM4001.colorfull = RGB(255, 0, 0) 
	LightM4002.colorfull = RGB(255, 0, 0) 
	LightM4003.colorfull = RGB(255, 0, 0) 
	LightM5001.State = 2
	LightM5002.State = 2
	LightM5003.State = 2
	LightM5001.color = RGB(255, 255, 0) 'YELL
	LightM5002.color = RGB(255, 255, 0) 
	LightM5003.color = RGB(255, 255, 0) 
	LightM5001.colorfull = RGB(255, 255, 0) 
	LightM5002.colorfull = RGB(255, 255, 0) 
	LightM5003.colorfull = RGB(255, 255, 0)
End Sub
Sub Mode_RelicAll_end '
	ModeRelicAllActive(CurrentPlayer) = False
	Relics_Sav(CurrentPlayer) = 0
	Mission_Relic1(CurrentPlayer) = 0
	Mission_Relic2a(CurrentPlayer) = 0
	Mission_Relic2b(CurrentPlayer) = 0
	Mission_Relic3(CurrentPlayer) = 0
	Mission_Relic4(CurrentPlayer) = 0
	Mission_Relic5(CurrentPlayer) = 0
	ModeRelic1Active(CurrentPlayer) = False
	ModeRelic2aActive(CurrentPlayer) = False
	ModeRelic2bActive(CurrentPlayer) = False
	ModeRelic3Active(CurrentPlayer) = False
	ModeRelic4Active(CurrentPlayer) = False
	ModeRelic5Active(CurrentPlayer) = False
	AddScore 450000
	AwardExtraBall
	LiB001.state = 0
	LiB002a.state = 0
	LiB002b.state = 0
	LiB003.state = 0
	LiB004.state = 0
	LiB005.state = 0
	ResetLightRelic
	ResetTargetCroft
	StartHITpoint
End Sub

Sub StartHITpoint
	If ModeMissionActive(CurrentPlayer) = False And ModeRelic1Active(CurrentPlayer) = False And ModeRelic2AActive(CurrentPlayer) = False And ModeRelic2BActive(CurrentPlayer) = False And ModeRelic3Active(CurrentPlayer) = False And ModeRelic4Active(CurrentPlayer) = False And ModeRelic5Active(CurrentPlayer) = False And ModeRelicAllActive(CurrentPlayer) = False Then
	PuPEvent 125
	Flasher010.ImageA = "MissSniper"
	Flasher010.visible = 1
	SniperShootingCount = 0
	DispXText SniperShootingCount
	ModeSniperShooting(CurrentPlayer) = True
	HitPointsTimer.Enabled = True
	LightM1001.state = 1
	LightM2A001.state = 1
	LightM2B001.state = 1
	LightM3001.state = 2
	LightM4001.state = 1
	LightM5001.state = 1
	LightM1001.color = RGB(255, 128, 0) 'Orange
	LightM1001.colorfull= RGB(255, 128, 0) 
	LightM2A001.color = RGB(255, 128, 0) 'Orange
	LightM2A001.colorfull= RGB(255, 128, 0) 
	LightM2B001.color = RGB(255, 128, 0) 'Orange
	LightM2B001.colorfull= RGB(255, 128, 0)
	LightM3001.color = RGB(0, 255, 0) 'Green
	LightM3001.colorfull= RGB(0, 255, 0) 
	LightM4001.color = RGB(255, 128, 0) 'Orange
	LightM4001.colorfull= RGB(255, 128, 0) 
	LightM5001.color = RGB(255, 128, 0) 'Orange
	LightM5001.colorfull= RGB(255, 128, 0) 
	End If
End Sub
Sub CheckHitPoint
	Flasher010.ImageA = "MissSniper"
	Flasher010.visible = 1
	If SniperShootingCount = 1 Then AddScore 4500
	If SniperShootingCount = 2 Then AddScore 8500  
	If SniperShootingCount = 3 Then AddScore 12000 : AddMBmission
	If SniperShootingCount = 4 Then AddScore 15000 
	If SniperShootingCount = 5 Then AddScore 20000 
	If SniperShootingCount = 6 Then AddScore 25000 : AddMBmission
	If SniperShootingCount = 7 Then AddScore 35000  
	If SniperShootingCount = 8 Then AddScore 50000 
	If SniperShootingCount = 9 Then AddScore 50000 : AddMBmission : StopHITpoint
	DispXText SniperShootingCount
End Sub
Sub StopHITpoint
	PuPEvent 126
	ModeSniperShooting(CurrentPlayer) = False
	HitPointsTimer.Enabled = False
	Flasher010.visible = 0
	SniperShootingCount = 0
	ResetLightRelic
End Sub

Sub HitPointsTimer_Timer()
	If LightM1001.state = 2 Then
	LightM1001.state = 1
	LightM1001.color = RGB(255, 128, 0) 'Orange
	LightM1001.colorfull= RGB(255, 128, 0)
	LightM2A001.state = 2
	LightM2A001.color = RGB(0, 255, 0) 'Green
	LightM2A001.colorfull= RGB(0, 255, 0)
	Elseif LightM2A001.state = 2 Then
	LightM2A001.state = 1
	LightM2A001.color = RGB(255, 128, 0) 'Orange
	LightM2A001.colorfull= RGB(255, 128, 0)
	LightM2B001.state = 2
	LightM2B001.color = RGB(0, 255, 0) 'Green
	LightM2B001.colorfull= RGB(0, 255, 0)
	Elseif LightM2B001.state = 2 Then
	LightM2B001.state = 1
	LightM2B001.color = RGB(255, 128, 0) 'Orange
	LightM2B001.colorfull= RGB(255, 128, 0)
	LightM3001.state = 2
	LightM3001.color = RGB(0, 255, 0) 'Green
	LightM3001.colorfull= RGB(0, 255, 0)
	Elseif LightM3001.state = 2 Then
	LightM3001.state = 1
	LightM3001.color = RGB(255, 128, 0) 'Orange
	LightM3001.colorfull= RGB(255, 128, 0)
	LightM4001.state = 2
	LightM4001.color = RGB(0, 255, 0) 'Green
	LightM4001.colorfull= RGB(0, 255, 0)
	Elseif LightM4001.state = 2 Then
	LightM4001.state = 1
	LightM4001.color = RGB(255, 128, 0) 'Orange
	LightM4001.colorfull= RGB(255, 128, 0)
	LightM5001.state = 2
	LightM5001.color = RGB(0, 255, 0) 'Green
	LightM5001.colorfull= RGB(0, 255, 0)
	Elseif LightM5001.state = 2 Then
	LightM5001.state = 1
	LightM5001.color = RGB(255, 128, 0) 'Orange
	LightM5001.colorfull= RGB(255, 128, 0)
	LightM1001.state = 2
	LightM1001.color = RGB(0, 255, 0) 'Green
	LightM1001.colorfull= RGB(0, 255, 0)
	End If
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

	'Setup Pages.  Note if you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS
	'syntax - PuPlayer.LabelNew <screen# or pDMD>,<Labelname>,<fontName>,<size%>,<colour>,<rotation>,<xAlign>,<yAlign>,<xpos>,<ypos>,<PageNum>,<visible>

	'Page 1 (default score display)
	'colors
	'dark - 2697513
	'light - 15066597

	'top

	'bottom

	PuPlayer.LabelNew pBackglass,"Ball",typefont,				2,2697513 	,0,1,1,50,81,1,0
	PuPlayer.LabelNew pBackglass,"curplayer",typefont,			3,2697513	,0,1,1,50,93,1,0
	PuPlayer.LabelNew pBackglass,"curscore",dmdscr,			12.4,33023	,0,1,1,51,87,1,0
	PuPlayer.LabelNew pBackglass,"Play1score",dmdscr,		3,2697513  	,0,0,1,12,64.3,1,0
	PuPlayer.LabelNew pBackglass,"Play2score",dmdscr,		3,2697513  	,0,0,1,30.7,64.3,1,0
	PuPlayer.LabelNew pBackglass,"Play3score",dmdscr,		3,2697513  	,0,0,1,49.5,64.3,1,0
	PuPlayer.LabelNew pBackglass,"Play4score",dmdscr,		3,2697513  	,0,0,1,68.53,64.3,1,0
	PuPlayer.LabelNew pBackglass,"ruletitle",numberfont,		3,15066597  ,0,1,1,50,12,1,0
	PuPlayer.LabelNew pBackglass,"rulecopy1",dmdscr,			2,15066597 	,0,1,1,50,15,1,1
	PuPlayer.LabelNew pBackglass,"rulecopy2",dmdscr,			2,15066597 	,0,1,1,50,15,1,1
	PuPlayer.LabelNew pBackglass,"rulecopy3",dmdscr,			2,15066597 	,0,1,1,50,15,1,1
	PuPlayer.LabelNew pBackglass,"rulecopy4",dmdscr,			2,15066597 	,0,1,1,50,15,1,1


	
	'onverlay
	PuPlayer.LabelNew pBackglass,"titleimg",zoomfont,			6,16777215 	,0,1,1, 0,0,1,0
	PuPlayer.LabelNew pBackglass,"titleimg2",zoomfont,			6,16777215 	,0,1,1, 0,0,1,0
	PuPlayer.LabelNew pBackglass,"titleimg3",zoomfont,			6,16777215 	,0,1,1, 0,0,1,0
	PuPlayer.LabelNew pBackglass,"titleimg4",zoomfont,			6,16777215 	,0,1,1, 0,0,1,0
	PuPlayer.LabelSet pBackglass,"titleimg","",1,"{'mt':2,'width': 0, 'height': 0, 'yalign': 0}"
	PuPlayer.LabelNew pBackglass,"titlebg",zoombgfont,			9,0  		,0,1,1,50,50,1,0
	PuPlayer.LabelNew pBackglass,"title",zoomfont,				9,16777215 	,0,1,1,50,50,1,0
	PuPlayer.LabelNew pBackglass,"titlebg2",zoombgfont,			6,0  		,0,1,1,50,50,1,0
	PuPlayer.LabelNew pBackglass,"title2",zoomfont,				6,16777215 	,0,1,1,50,50,1,0
	PuPlayer.LabelNew pBackglass,"mgscore",numberfont,			9,15066597	,0,1,1,50,47,1,0

	'Bonus
	PuPlayer.LabelNew pBackglass,"BonusTitre",zoomfont,			12,16777215  ,0,1,1,0,0,1,0
	PuPlayer.LabelNew pBackglass,"Bonus1",dmdscr,			    5,16777215  ,0,1,1,51,30.2,1,0 'JackpotsBonus
	PuPlayer.LabelNew pBackglass,"Bonus2",dmdscr,			    5,16777215  ,0,1,1,51,36.2,1,0 'RaiderBonus
	PuPlayer.LabelNew pBackglass,"Bonus3",dmdscr,			    5,16777215  ,0,1,1,51,41.78,1,0 'CroftBonus
	PuPlayer.LabelNew pBackglass,"Bonus4",dmdscr,			    5,16777215  ,0,1,1,51,47.52,1,0 'RelicsBonus
	PuPlayer.LabelNew pBackglass,"Bonus5",dmdscr,			    5,16777215  ,0,1,1,51,54,1,0 'ArtefactsBonus
	PuPlayer.LabelNew pBackglass,"BonusTotal",dmdscr,			5,16777215  ,0,1,1,78,42.2,1,0

	'attract
	PuPlayer.LabelNew pBackglass,"Sout",zoomfont,				12,16777215  ,0,1,1,0,0,1,0
	PuPlayer.LabelNew pBackglass,"Test1",typefont,			    12,16777215  ,0,1,1,16,25,1,0  ',0,1,1,22,26,1,0
	PuPlayer.LabelNew pBackglass,"high1name",typefont,			5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"high1score",numberfont,		5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"high2name",typefont,			5,16777215  ,0,1,1,22,38,1,1
	PuPlayer.LabelNew pBackglass,"high2score",numberfont,		5,16777215  ,0,1,1,36,38,1,1
	PuPlayer.LabelNew pBackglass,"high3name",typefont,			5,16777215  ,0,1,1,22,46,1,1
	PuPlayer.LabelNew pBackglass,"high3score",numberfont,		5,16777215  ,0,1,1,36,46,1,1
	PuPlayer.LabelNew pBackglass,"high4name",typefont,			5,16777215  ,0,1,1,22,54,1,1
	PuPlayer.LabelNew pBackglass,"high4score",numberfont,		5,16777215  ,0,1,1,36,54,1,1
	PuPlayer.LabelNew pBackglass,"HighScore",typefont,			6,16777215	,0,0,1,20,30,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL1",numberfont,		8,16777215	,0,0,1,20,40,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL2",numberfont,		8,16777215	,0,0,1,24,40,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL3",numberfont,		8,16777215	,0,0,1,28,40,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL4",numberfont,		4,16777215	,0,0,1,20,50,1,1
	PuPlayer.LabelNew pBackglass,"CurImage","Arial",			50,391231   ,0,1,1, 0, 0,1,1  'new image type

Sub resetbackglassOFFScore
	PuPlayer.LabelSet pBackglass,"Bonus1", FormatNumber(JackpotsBonus,0),0,"{'mt':2,'color':13264128, 'size': 5 }"
	PuPlayer.LabelSet pBackglass,"Bonus2", FormatNumber(RaiderBonus,0),0,"{'mt':2,'color':55660, 'size': 5 }"
	PuPlayer.LabelSet pBackglass,"Bonus3", FormatNumber(CroftBonus,0),0,"{'mt':2,'color':3739322, 'size': 5 }"
	PuPlayer.LabelSet pBackglass,"Bonus4", FormatNumber(RelicsBonus,0),0,"{'mt':2,'color':25542, 'size': 5 }"
	PuPlayer.LabelSet pBackglass,"Bonus5", FormatNumber(ArtefactsBonus,0),0,"{'mt':2,'color':5602052, 'size': 5 }"
	PuPlayer.LabelSet pBackglass,"BonusTotal", FormatNumber(TotalBonus,0),0,"{'mt':2,'color':16777215, 'size': 5 }"
End Sub

Function BonusScoreTotal()
	BonusScoreTotal = BonusMultiplier(CurrentPlayer) * ((JackpotsBonus * 5000) + (RaiderBonus * 2000) + (CroftBonus * 2000) + (RelicsBonus * 15000) + (ArtefactsBonus *10000) )
End Function

Sub SeqBonus
	vpmtimer.addtimer 200, "SeqBonus1 '"
	vpmtimer.addtimer 650, "SeqBonus2 '"
	vpmtimer.addtimer 1120, "SeqBonus3 '"
	vpmtimer.addtimer 1570, "SeqBonus4 '"
	vpmtimer.addtimer 2120, "SeqBonus5 '"
End Sub
Sub SeqBonus1
	PuPlayer.LabelSet pBackglass,"Bonus1", FormatNumber(JackpotsBonus,0),1,"{'mt':2,'color':13264128, 'size': 5 }"
	PlaySound "Bat"
End Sub
Sub SeqBonus2
	PuPlayer.LabelSet pBackglass,"Bonus2", FormatNumber(RaiderBonus,0),1,"{'mt':2,'color':55660, 'size': 5 }"
End Sub
Sub SeqBonus3
	PuPlayer.LabelSet pBackglass,"Bonus3", FormatNumber(CroftBonus,0),1,"{'mt':2,'color':3739322, 'size': 5 }"
End Sub
Sub SeqBonus4
	PuPlayer.LabelSet pBackglass,"Bonus4", FormatNumber(RelicsBonus,0),1,"{'mt':2,'color':25542, 'size': 5 }"
End Sub
Sub SeqBonus5
	PuPlayer.LabelSet pBackglass,"Bonus5", FormatNumber(ArtefactsBonus,0),1,"{'mt':2,'color':5602052, 'size': 5 }"
	PlaySound "Bat2"
	PlayerScore(CurrentPlayer) = PlayerScore(CurrentPlayer) + BonusScoreTotal
End Sub

	'called on table load
Sub resetbackglass
'	PuPlayer.LabelSet pBackglass,"titleimg","",1,"{'mt':2,'color':111111, 'width': 0, 'height': 0, 'yalign': 0}"
	PuPlayer.LabelShowPage pBackglass,1,0,""
End Sub

Sub resetbackglassOFF
	bModePbgActive = False
	PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),0,"{'mt':2,'color':5267966, 'size': 6 }"
	PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),0,"{'mt':2,'color':54741, 'size': 6 }"
	PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),0,"{'mt':2,'color':10460928, 'size': 6 }"
	PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),0,"{'mt':2,'color':2362620, 'size': 6 }"
	PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(PlayerScore(1),0),0,"{'mt':2,'color':5267966, 'size': 3 }"
	PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(PlayerScore(2),0),0,"{'mt':2,'color':54741, 'size': 3 }"
	PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(PlayerScore(3),0),0,"{'mt':2,'color':10460928, 'size': 3 }"
	PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(PlayerScore(4),0),0,"{'mt':2,'color':2362620, 'size': 3 }"
	PuPlayer.LabelSet pBackglass,"titleimg2","PUPAlphas\\OverlayJ2.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"titleimg3","PUPAlphas\\OverlayJ3.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"titleimg4","PUPAlphas\\OverlayJ4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
End Sub

	Dim prevNumberOfPlayers : prevNumberOfPlayers = 0
	Sub pUpdateScores
		PuPlayer.LabelSet pBackglass,"rulecopy1","" & Credits,1,"{'mt':2,'color':15066597, 'size': 3, 'xpos': 8.8, 'xalign': 1, 'ypos': 82, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"rulecopy2","" & FormatNumber(PlayersPlayingGame, 0),1,"{'mt':2,'color':15066597, 'size': 3, 'xpos': 8.8, 'xalign': 1, 'ypos': 92, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"rulecopy3","" & FormatNumber(Relics_Sav(CurrentPlayer), 0),1,"{'mt':2,'color':15066597, 'size': 3, 'xpos': 90, 'xalign': 1, 'ypos': 82, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"rulecopy4","" & FormatNumber(Gems_Sav(CurrentPlayer), 0),1,"{'mt':2,'color':15066597, 'size': 3, 'xpos': 90, 'xalign': 1, 'ypos': 92, 'yalign': 1}"
'		PuPlayer.LabelSet pBackglass,"curscore",FormatNumber(PlayerScore(CurrentPlayer),0),1,""   'Score centre
'		PuPlayer.LabelSet pBackglass,"curplayer","Player " &  FormatNumber(CurrentPlayer),1,""
		If PlayersPlayingGame = 1 Then
			bModePbgActive = False
			resetbackglassOFF
		Elseif PlayersPlayingGame = 2 Then
			bModePbgActive = True
			PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(PlayerScore(1),0),1,"{'mt':2,'color':5267966, 'size': 3 }"
			PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(PlayerScore(2),0),1,"{'mt':2,'color':54741, 'size': 3 }"
			If (prevNumberOfPlayers <> 2) Then
				PuPlayer.LabelSet pBackglass,"titleimg2","PUPAlphas\\OverlayJ2.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titleimg3","PUPAlphas\\OverlayJ3.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titleimg4","PUPAlphas\\OverlayJ4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(PlayerScore(3),0),0,"{'mt':2,'color':10460928, 'size': 3 }"
				PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(PlayerScore(4),0),0,"{'mt':2,'color':2362620, 'size': 3 }"
			End If
		Elseif PlayersPlayingGame = 3 Then
			bModePbgActive = True
			PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(PlayerScore(1),0),1,"{'mt':2,'color':5267966, 'size': 3 }"
			PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(PlayerScore(2),0),1,"{'mt':2,'color':54741, 'size': 3 }"
			PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(PlayerScore(3),0),1,"{'mt':2,'color':10460928, 'size': 3 }"
			If (prevNumberOfPlayers <> 3) Then
				PuPlayer.LabelSet pBackglass,"titleimg2","PUPAlphas\\OverlayJ2.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titleimg3","PUPAlphas\\OverlayJ3.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titleimg4","PUPAlphas\\OverlayJ4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(PlayerScore(4),0),0,"{'mt':2,'color':2362620, 'size': 3.3 }"
			End If
		Elseif PlayersPlayingGame = 4 Then
			bModePbgActive = True
			PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(PlayerScore(1),0),1,"{'mt':2,'color':5267966, 'size': 3 }"
			PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(PlayerScore(2),0),1,"{'mt':2,'color':54741, 'size': 3 }"
			PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(PlayerScore(3),0),1,"{'mt':2,'color':10460928, 'size': 3 }"
			PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(PlayerScore(4),0),1,"{'mt':2,'color':2362620, 'size': 3 }"
			If (prevNumberOfPlayers <> 3) Then
				PuPlayer.LabelSet pBackglass,"titleimg2","PUPAlphas\\OverlayJ2.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titleimg3","PUPAlphas\\OverlayJ3.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titleimg4","PUPAlphas\\OverlayJ4.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			End If
		End If

		If (prevNumberOfPlayers <> PlayersPlayingGame) Then
			If CurrentPlayer = 1 And PlayersPlayingGame >= 2 Then
				PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),1,"{'mt':2,'color':5267966, 'size': 6 }"
			Elseif CurrentPlayer = 2 Then
				PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),1,"{'mt':2,'color':54741, 'size': 6 }"
			Elseif CurrentPlayer = 3 Then
				PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),1,"{'mt':2,'color':10460928, 'size': 6 }"
			Elseif CurrentPlayer = 4 Then
				PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),1,"{'mt':2,'color':2362620, 'size': 6 }"
			End If
		End If

		prevNumberOfPlayers = PlayersPlayingGame
	end Sub


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
	Sub pSetPageLayouts

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


	if PuPDMDDriverType=pDMDTypeLCD THEN  'Using 4:1 Standard ratio LCD PuPDMD  ************ lcd **************
		
		'Page 1 (default score display)
		PuPlayer.LabelNew pDMD,"Credits" ,dmdscr,20,8388736   ,1,0,0,6,0,1,0 '
		PuPlayer.LabelNew pDMD,"Play1"   ,dmdscr,25,16744448   ,1,0,2,2,0,1,0
		PuPlayer.LabelNew pDMD,"Ball"    ,dmdscr,25,16744448   ,1,2,2,96,0,1,0 '
		PuPlayer.LabelNew pDMD,"MsgScore",dmdscr,45,33023   ,0,1,0, 0,40,1,0
		PuPlayer.LabelNew pDMD,"CurScore",dmdscr,60,33023   ,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Relics" ,dmdscr,20,5267966   ,1,0,0,80,20,1,0 '
		PuPlayer.LabelNew pDMD,"Artefacts" ,dmdscr,20,7714832   ,1,0,0,75,0,1,0 '

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

			pDMDStartBackLoop "DMDSplash","intro1.mp4"
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
	resetbackglassOFF
	playclear pBackglass
	pAttractStart
	end Sub

	Sub pAttractStart
	pDMDSetPage(pDMDBlank)   'set blank text overlay page.
	pCurAttractPos=0
	pInAttract=True 'Startup in AttractMode
	pAttractNext
	end Sub


	Sub pDMDStartUP
		pupDMDDisplay "attract","","@welcome.mp4",12,0,9
		'PlaySound "Mu_5"
		pInAttract=true
	end Sub

	Sub pDMDStartGame
	pInAttract=false
	pDMDSetPage(pScores)   'set blank text overlay page.
	PuPEvent 119
	end Sub

	DIM pCurAttractPos: pCurAttractPos=0


	'********************** gets called auto each page next and timed already in DMD_Timer.  make sure you use pupDMDDisplay or it wont advance auto.
	Sub pAttractNext
		pCurAttractPos=pCurAttractPos+1

		Select Case pCurAttractPos
			Case 1
				pupDMDDisplay "Splash4","Game^Over", "",3, 1,10
				PuPEvent 118
			Case 2
				pupDMDDisplay "highscore", "High Score^AAA   2451654^BBB   2342342", "", 5, 0, 10
			Case 3
				pupDMDDisplay "Splash4","Press^Start","",3,1,10
			Case 4
				pupDMDDisplay "Splash4","REPLAY AT^1 000 000","",3,0,10
			Case 5
				pupDMDDisplay "Splash4","Press^Start","",3,1,10
			Case Else
				pCurAttractPos=0
				pAttractNext 'reset to beginning
		end Select
	end Sub

'************************ called during gameplay to update Scores ***************************
Dim CurTestScore:CurTestScore=0
Sub pDMDUpdateScores  'call this ONLY on timer 300ms is good enough
	if pDMDCurPage <> pScores then Exit Sub
	'PuPlayer.LabelSet pBackglass,"rulecopy1",""& Credits,1,"{'mt':2,'color':15066597, 'size': 3, 'xpos': 8.8, 'xalign': 1, 'ypos': 82, 'yalign': 1}"
	'PuPlayer.LabelSet pBackglass,"rulecopy2","" & FormatNumber(PlayersPlayingGame, 0),1,"{'mt':2,'color':15066597, 'size': 3, 'xpos': 8.8, 'xalign': 1, 'ypos': 92, 'yalign': 1}"
	'PuPlayer.LabelSet pBackglass,"rulecopy3",""& FormatNumber(Relics_Sav(CurrentPlayer), 0),1,"{'mt':2,'color':15066597, 'size': 3, 'xpos': 90, 'xalign': 1, 'ypos': 82, 'yalign': 1}"
	'PuPlayer.LabelSet pBackglass,"rulecopy4",""& FormatNumber(Gems_Sav(CurrentPlayer), 0),1,"{'mt':2,'color':15066597, 'size': 3, 'xpos': 90, 'xalign': 1, 'ypos': 92, 'yalign': 1}"
	puPlayer.LabelSet pDMD,"CurScore","" & FormatNumber(PlayerScore(CurrentPlayer), 0),1,""
	puPlayer.LabelSet pDMD,"Play1","Player " & FormatNumber(CurrentPlayer, 0),1,""
	puPlayer.LabelSet pDMD,"Ball","Ball " & FormatNumber(BallinGame(CurrentPlayer), 0) & " / " & FormatNumber(BallsPerGame, 0),1,""
end Sub

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
		If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 1800000 End If

		x = LoadValue(TableName, "HighScore1Name")
		If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "TOM" End If

		x = LoadValue(TableName, "HighScore2")
		If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 1500000 End If

		x = LoadValue(TableName, "HighScore2Name")
		If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "AMA" End If

		x = LoadValue(TableName, "HighScore3")
		If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 1300000 End If

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
    HighScoreName(0) = "AAA"
    HighScoreName(1) = "BBB"
    HighScoreName(2) = "CCC"
    HighScoreName(3) = "DDD"
    HighScore(0) = 100000
    HighScore(1) = 95000
    HighScore(2) = 91000
    HighScore(3) = 90000
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
	End Sub

	Sub HighScoreEntryInit()
		hsbModeActive = True
		PlaySound "vo_enteryourinitials"

		hsEnteredDigits(1) = "A"
		hsEnteredDigits(2) = " "
		hsEnteredDigits(3) = " "

		hsCurrentDigit = 1
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
						playclear pBackglass
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
		PuPlayer.LabelSet pBackglass,"HighScore","Your Name",1,"{'mt':2,'color':16744448, 'size': 6, 'xpos': 34.5, 'xalign': 1, 'ypos': 25.8, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"HighScoreL1","A",1,"{'mt':2,'color':8454143, 'size': 12, 'xpos': 24.5, 'xalign': 1, 'ypos': 42.8, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"HighScoreL2","",1,"{'mt':2,'color':8454143, 'size': 12, 'xpos': 32.5, 'xalign': 1, 'ypos': 42.8, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"HighScoreL3","",1,"{'mt':2,'color':8454143, 'size': 12, 'xpos': 40.5, 'xalign': 1, 'ypos': 42.8, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"HighScoreL4",PlayerScore(CurrentPlayer),1,"{'mt':2,'color':157, 'size': 10, 'xpos': 17.5, 'xalign': 0, 'ypos': 58.8, 'yalign': 1}"
		hsletter = 1
	End Sub

	Sub HighScoreDisplayName()

		Select case hsLetter
		Case 0
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","<",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","<",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","<",1,""
		Case 1
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","A",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","A",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","A",1,""
		Case 2
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","B",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","B",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","B",1,""
		Case 3
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","C",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","C",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","C",1,""
		Case 4
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","D",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","D",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","D",1,""
		Case 5
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","E",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","E",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","E",1,""
		Case 6
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","F",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","F",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","F",1,""
		Case 7
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","G",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","G",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","G",1,""
		Case 8
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","H",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","H",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","H",1,""
		Case 9
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","I",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","I",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","I",1,""
		Case 10
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","J",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","J",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","J",1,""
		Case 11
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","K",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","K",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","K",1,""
		Case 12
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","L",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","L",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","L",1,""
		Case 13
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","M",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","M",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","M",1,""
		Case 14
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","N",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","N",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","N",1,""
		Case 15
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","O",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","O",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","O",1,""
		Case 16
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","P",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","P",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","P",1,""
		Case 17
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","Q",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","Q",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","Q",1,""
		Case 18
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","R",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","R",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","R",1,""
		Case 19
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","S",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","S",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","S",1,""
		Case 20
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","T",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","T",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","T",1,""
		Case 21
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","U",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","U",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","U",1,""
		Case 22
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","V",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","V",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","V",1,""
		Case 23
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","W",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","W",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","W",1,""
		Case 24
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","X",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","X",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","X",1,""
		Case 25
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","Y",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","Y",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","Y",1,""
		Case 26
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","Z",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","Z",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","Z",1,""
		End Select
	End Sub

	' post the high score letters


	Sub HighScoreCommitName()
		playclear pBackglass
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
		PuPlayer.LabelSet pBackglass,"HighScore","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL1","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL2","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL3","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL4","",1,""
		hsbModeActive = False
	End Sub

	sub playclear(chan)
		if chan = pBackglass Then
			PuPlayer.playstop pBackglass
			resetbackglassOFF
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
		PlayersPlayingGame = 1
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

		PuPlayer.LabelInit pDMD

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
	Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
	' pEventID = reference if application,  
	' pText = "text to show" separate lines by ^ in same string
	' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
	' also global variable useDMDVideos=true/false if user wishes only TEXT
	' TimeSec how long to display msg in Seconds
	' animation if any 0=none 1=Flasher
	' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345

	DIM curPos
	if pDMDCurPriority>=pPriority then Exit Sub  'if something is being displayed that we don't want interrupted.  same level will interrupt.
	pDMDCurPriority=pPriority
	if TimeSec=0 then TimeSec=1 'don't allow page default page by accident

	pLine1=""
	pLine2=""
	pLine3=""
	pLine1Ani=""
	pLine2Ani=""
	pLine3Ani=""

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
			 pLine3=Left(pText,curPos-1) 
		  Else 
			if pText<>"" Then pLine3=pText 
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
		pDMDShowCounter pLine1,pLine2,pLine3,TimeSec
	Elseif StrComp(pEventID,"target",1)=0 Then              'check eventIDs
		pDMDTargetLettersInfo pLine1,pLine2,TimeSec
	Elseif StrComp(pEventID,"highscore",1)=0 Then              'check eventIDs
		pDMDShowHS pLine1,pLine2,pLine3,TimeSec
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
		pDMDUpdateScores 
		pUpdateScores

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


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Minigame
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 
		'*************************
		' PUP MINI GAME (videomode)
		' create a timer (disable default) PuPGameTimer (interval 300)
		' when you want to start game call PuPGameStartMiniGame
		' look at PuPMiniGameEnd to do something when gameover.
		' game music be called PuPMiniGame.exe inside ofr MiniGame folder of puppack!
		' see sample of key_down and key_up in table script!
		'*************************

		DIM PuPGameRunning:PuPGameRunning=false
		DIM PuPGameTimeout
		DIM PuPGameInfo
		DIM PuPGameScore

		Sub PuPGameStartMiniGame
			if PuPGameRunning Then Exit Sub
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":16, ""EX"": ""MiniGame\\PuPMiniGame.exe"", ""WT"": ""PupMiniGame"", ""RS"":1 , ""TO"":15 , ""WZ"":0 , ""SH"": 1 , ""FT"":""Visual Pinball Player"" }"    
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":3, ""OT"": 0 }"      'this will hide overlay if applicable
			PuPGameTimeout=-3    'check for timeout  every 500 ms
			PuPGameRunning=true	
			PuPGameTimer.enabled=true
			PuPlayer.playpause pBackglass
		End Sub

		Sub PuPMiniGameEnd(gamescore)
			if PuPGameRunning Then Exit Sub
'			inminigame = 0
			PuPlayer.playresume pBackglass
			addscore PuPGameScore
			PuPlayer.LabelSet pBackglass,"mgscore",FormatNumber(PuPGameScore,0),1,""
'			playmedia "defensescore.mp4","videoscenes",pBackglass,"cineon",4000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			'msgbox "mini score "&PuPGameScore
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":3, ""OT"": 1 }"      'this will showsuccess overlay if applicable
			vpmtimer.addtimer 4000, "clearmgscore '"
		End Sub

		sub clearmgscore
			PuPlayer.LabelSet pBackglass,"mgscore","",2,""
		end Sub

		Sub PuPGameTimer_Timer()    
			PuPGameTimeout=PuPGameTimeout+1
			PuPGameInfo= PuPlayer.GameUpdate("PupMiniGame", 0 , 0 , "")   '0=game over, 1=game running
			'CHECK GAME OVER
			if PuPGameInfo=0 AND PuPGameTimeOut>12 Then  'gameover if more than 5 seconds passed
			   PuPGameTimer.enabled=false 
			   PupGameRunning=False
			   PuPGameScore= PuPlayer.GameUpdate("PupMiniGame", 6 , 0 , "\MiniGame\gameover.txt")   'grab score from minigame   3=gms 6=godot           
			   'msgbox PuPGameScore  DO something with the score if its over 0!!!
			   PuPMiniGameEnd(PuPGameScore) 
			   bModeMiniGameActive = False
			   Kicker004.timerinterval = 1000
			   kicker004.timerenabled = True
			End If 
		End Sub
