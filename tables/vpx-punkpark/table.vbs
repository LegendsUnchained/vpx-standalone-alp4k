'                        __                              __
'______   __ __   ____  |  | __ ______  _____   _______ |  | __
'\____ \ |  |  \ /    \ |  |/ / \____ \ \__  \  \_  __ \|  |/ /
'|  |_> >|  |  /|   |  \|    <  |  |_> > / __ \_ |  | \/|    <
'|   __/ |____/ |___|  /|__|_ \ |   __/ (____  / |__|   |__|_ \
'|__|                \/      \/ |__|         \/              \/
'
' ****************************************************************
'                       VISUAL PINBALL X
' ****************************************************************
'***********************************
' // User Settings in F12 menu //   
'***********************************
Dim ModeChangeBallActive
Dim UnlockAllMusic

Dim SidewallChoice: SidewallChoice = 0
Dim RailChoice: RailChoice = True

'----- VR Room -----
Dim VRRoomChoice : VRRoomChoice = 1			  ' 1 - Cab Only, 2 - Minimal Room, 3 - MEGA room
Dim VRTest : VRTest = False

'//////////////F12 Menu//////////////
' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings
Dim dspTriggered : dspTriggered = False
Sub Table1_OptionEvent(ByVal eventId)
	If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If
    
    ' VRRoom
	VRRoomChoice = Table1.Option("VR Room", 1, 3, 1, 3, 0, Array("CabOnly", "Minimal", "MEGA"))
	LoadVRRoom

	DMDTimer.Enabled = False ' stop FlexDMD timer
	Controller.Pause = True

	ModeChangeBallActive = Table1.Option("ChangeColorBall", 0, 1, 1, 0, 0, Array("Yes","No"))
	UnlockAllMusic = Table1.Option("UnlockMusic", 0, 1, 1, 0, 0, Array("No","Yes"))

	' -- start paused processes --
	DMDTimer.Enabled = True
	Controller.Pause = False
    If eventId = 3 Then DisableStaticPreRendering = False
    	RailChoice = Table1.Option("Rails Visible", 0, 1, 1, 1, 0, Array("False", "True (Default)"))
	SetRails RailChoice

	SidewallChoice = Table1.Option("Sidewall Art", 0, 1, 1, 1, 0, Array("Desktop", "PinCab"))
	SetSidewall SidewallChoice
        If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
	End Sub

Sub SetRails(Opt)
	Select Case Opt
		Case 0:
			Ramp15.Visible = 0
			Ramp16.Visible = 0
			Wall001.sidevisible = 0
			Wall002.sidevisible = 0
		Case 1:
			Ramp15.Visible = 1
			Ramp16.Visible = 1
            Wall001.sidevisible = 1
			Wall002.sidevisible = 1
			If VRRoom > 0 or VRTest Then Ramp15.visible = 0
			If VRRoom > 0 or VRTest Then Ramp16.visible = 0
			If VRRoom > 0 or VRTest Then Wall001.sidevisible = 0
			If VRRoom > 0 or VRTest Then Wall002.sidevisible = 0
	End Select
End Sub

Sub SetSidewall(Opt)
	Select Case Opt
		Case 0:
			PinCab_Blades.visible = 0
		Case 1:
			PinCab_Blades.Image = "Sidewalls_P"
			PinCab_Blades.visible = 1
			If VRRoom > 0 or VRTest Then PinCab_Blades.visible = 0
	End Select

End Sub

Sub TimerPlunger_Timer

  If VRCab_Plunger.Y < 32.38968 then
      VRCab_Plunger.Y = VRCab_Plunger.Y + 5
  End If
End Sub

Sub TimerPlunger2_Timer
    VRCab_Plunger.Y = -67.61032 + (5* Plunger.Position) -20
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
'110 
'111 
'112 AutoPlunger
'113 Spinner - Shaker
'114 Kicker002
'115 
'116 Drop Targets Reset
'117 AutoPlunger
'118 
'119 
'120 
'121 
'122 Knocker
'123 Ball Release
'124 Kicker001
'125 
'136 
'144 
'145 
'146 
'147 

'**************************
'   PinUp Player USER Config
'**************************
    dim usePuPDMD       : usePuPDMD=true       ' set to false to not use PuPDMD for a DMD (different that BG scoring)
	dim PuPDMDDriverType: PuPDMDDriverType=0   ' 0=LCD DMD, 1=RealDMD 2=FULLDMD (large/High LCD)
	dim useRealDMDScale : useRealDMDScale=0    ' 0 or 1 for RealDMD scaling.  Choose which one you prefer.
	dim useDMDVideos    : useDMDVideos=false   ' true or false to use DMD splash videos.
	Dim pGameName       : pGameName="Punk_Park"    'pupvideos foldername, probably set to cGameName in realworld

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
Const cGameName = "Punk_Park"
Const TableName = "Punk_Park"
Const typefont = "CF Punk Attitude"
Const numberfont = "Impact"
Const dmdalt="arial"
Const dmdfixed="Instruction"
Const dmdscr="Impact"    'main scorefont
Const dmddef="Zig"
Const dmdtomb="CF Punk Attitude"

Const zoomfont = "Magic School One"
Const zoombgfont = "Magic School One" ' needs to be an outline of the zoomfont
Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 15 ' in seconds
Const MaxMultiplier = 3  ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 3  ' usually 3 or 5
Const MaxMultiballs = 6  ' max number of balls during multiballs

Const Special1 = 1000000  ' High score to obtain an extra ball/game
Const Special2 = 3000000
Const Special3 = 5000000

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = True
Else
    UseFlexDMD = True
End If

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim UnlockMusicCount
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
Dim bModePbgActive
Dim SelectMusicActive
Dim MBLock_Sav(4)
Dim ModeSpecialActive(4)
Dim SpecialLetterComplet(4)
Dim SpinnerCount
Dim SpinnerModeActive
Dim	BumperCount
Dim BumperModeActive
Dim ModeCrimeActive
Dim GrueState
Dim LetterTargetS(4)
Dim LetterTargetK(4)
Dim LetterTargetA(4)
Dim LetterTargetT(4)
Dim LetterTargetE(4)
Dim LetterSpecialS(4)
Dim LetterSpecialP(4)
Dim LetterSpecialE(4)
Dim LetterSpecialC(4)
Dim LetterSpecialI(4)
Dim LetterSpecialA(4)
Dim LetterSpecialL(4)
Dim TargetSpecialS(4)
Dim TargetSpecialP(4)
Dim TargetSpecialE(4)
Dim TargetSpecialC(4)
Dim TargetSpecialI(4)
Dim TargetSpecialA(4)
Dim TargetSpecialL(4)
Dim CrimeCount
Dim	ExtraBCount(4)
Dim	TargetTVCount
Dim	RampLCount(4)
Dim	RampLGarageCount(4)
Dim	RampMBumperACount(4)
Dim	RampMBumperBCount(4)
Dim	RampLoopCount(4)
Dim	RampMCount(4)
Dim	RampRCount(4)
Dim	RampRMBCount(4)
Dim Bonus001 'Slides
Dim Bonus002 'GRinds
Dim Bonus003 'Grabs
Dim Bonus004 'RampTricks
Dim Bonus005 'FlipTricks
Dim Bonus006 'HotTricks
Dim Bonus001active
Dim Bonus002active
Dim Bonus003active
Dim Bonus004active
Dim Bonus005active
Dim Bonus006active
Dim SkillShotState
Dim bMusicOn
Dim bJustStarted
Dim bJackpot
Dim plungerIM 
Dim LastSwitchHit
'Dim FlexScenes(100)				 'Array of FlexDMD scenes
Dim VRRoom
Dim FlexOnPlayfield
'Dim tablewidth

'****************************************
'		USER OPTIONS
'****************************************

Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		' Level of ramp rolling volume. Value between 0 and 1
Dim StagedFlippers : StagedFlippers = 0         ' Staged Flippers. 0 = Disabled, 1 = Enabled

Dim tablewidth
tablewidth = Table1.width
Dim tableheight
tableheight = Table1.height
'****************************************
' core.vbs variables
Dim cbRight
' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
	PUPINIT
	'Reseths
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

    ' freeplay or coins
    bFreePlay = True 'we want coins

    'if bFreePlay = false Then DOF 125, DOFOn

	' Turn off the bumper lights

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

'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    RollingUpdate
    ' add any other real time update subs, like gates or diverters
    FlipperLSh.Rotz = LeftFlipper.CurrentAngle
    FlipperRSh.Rotz = RightFlipper.CurrentAngle
	LFLogo.RotZ = LeftFlipper.CurrentAngle
	RFlogo.RotZ = RightFlipper.CurrentAngle
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If Keycode = AddCreditKey Then
		DOF 202, DOFPulse
        Credits = Credits + 1
		PlaySound "Coin_In_1"
		pDMDStartGame
        if bFreePlay = False Then
            DOF 125, DOFOn
            If(Tilted = False) Then
            End If
        End If
    End If

    If keycode = PlungerKey And PlungerKeyActive = True Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
        PlaySoundAt "fx_reload", plunger
		pDMDStartGame
    End If

	If keycode = PlungerKey Then
		If VRRoom > 0 then 
			TimerPlunger.Enabled = True
			TimerPlunger2.Enabled = False
		End If
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
	
	If keycode = RightMagnaSave And SelectMusicActive = False Then CheckBGBonusAttract
	If keycode = LeftMagnaSave And SelectMusicActive = False Then ChangeMusicnr = (ChangeMusicnr + 1)MOD FormatNumber(UnlockMusicCount,0) : UpdateChangeMusic

	If keycode = LeftFlipperKey Then
		SolLFlipper True	'This would be called by the solenoid callbacks if using a ROM
		If VRRoom > 0 or VRTest Then VRCab_FlipperLeft.x = VRCab_FlipperLeft.x + 5
		If SelectMusicActive = True Then
			SelectMusicnr = (SelectMusicnr - 1)
			If SelectMusicnr <0 Then SelectMusicnr = 13
			UpdateBGSelectMusic
		End If
	Elseif keycode = LeftFlipperKey Then
'		SolLFlipper False	'This would be called by the solenoid callbacks if using a ROM
	End If
	
	If keycode = RightFlipperKey Then
		SolRFlipper True	'This would be called by the solenoid callbacks if using a ROM
		If VRRoom > 0 or VRTest Then VRCab_FlipperRight.x = VRCab_FlipperRight.x - 5
		If SelectMusicActive = True And BallsOnPlayfield = 1 Then
			SelectMusicnr = (SelectMusicnr + 1)MOD 14
			UpdateBGSelectMusic
		End If 
	Elseif keycode = RightFlipperKey Then
'		SolRFlipper False	'This would be called by the solenoid callbacks if using a ROM
	End If

	If keycode = PlungerKey Then
		If VRRoom > 0 then 
			TimerPlunger.Enabled = True
			TimerPlunger2.Enabled = False
		End If
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
 
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = PlungerKey And PlungerKeyActive = True Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
        If bBallInPlungerLane Then PlaySoundAt "fx_fire", plunger
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    If keycode = PlungerKey Then
		If VRRoom > 0 then
			TimerPlunger.Enabled = False
			TimerPlunger2.Enabled = True
			VRCab_Plunger.Y = -67.61032
		End if
	End If

    ' Table specific
		If keycode = RightMagnaSave And SelectMusicActive = False Then resetBGBonusAttract

    If bGameInPLay AND NOT Tilted Then
	pDMDStartGame
		If keycode = LeftFlipperKey Then
			If VRRoom > 0 or VRTest Then VRCab_FlipperLeft.x = 2115.269
			SolLFlipper False   'This would be called by the solenoid callbacks if using a ROM
		End If
	
		If keycode = RightFlipperKey Then
			If VRRoom > 0 or VRTest Then VRCab_FlipperRight.x = 2143.48
			SolRFlipper False   'This would be called by the solenoid callbacks if using a ROM
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
        pupDMDDisplay "Splash4","TILT","",5,1,22
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
        PlaySong "Mu_4"'"Mu_end"
        Exit Sub
    End If

    If bAttractMode Then
        PlaySong "Mu_4"'"Mu_end"
        Exit Sub
    End If
    If bMultiBallMode Then
        PlaySong "Mu_5"
    Else
        UpdateMusicNow
    end if
End Sub

'if you add more balls to the game use changesong then if bMultiBallMode = true, your multiball song will be played.

Sub UpdateMusicNow
'    Select Case UpdateMusic
'        Case 0:PlaySong "Mu_1"
'        Case 1:PlaySong "Mu_2"
'        Case 2:PlaySong "Mu_3"
'        Case 3:PlaySong "Mu_4"
'        Case 4:PlaySong "M_end"
'    End Select
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
				n.color = RGB(18, 18, 0)
				n.colorfull = RGB(255, 255, 0)
			Case darkgreen
				n.color = RGB(0, 8, 0)
				n.colorfull = RGB(0, 64, 0)
			Case green
				n.color = RGB(0, 255, 0)
'				n.color = RGB(0, 18, 0)
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
'			Case white
'				n.color = RGB(255, 252, 224)
'				n.colorfull = RGB(193, 91, 0)
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

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub ChangeLAMPH(col)
	Dim ledlb
	For each ledlb in aLightsLampH
		SetLightColor ledlb, col, -1
	Next
End Sub
Sub ChangeLAMPB(col)
	Dim ledlb
	For each ledlb in aLightsLampB
		SetLightColor ledlb, col, -1
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
 '   DOF 127, DOFOn
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
 '   DOF 127, DOFOff
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

'Function AudioFade(ball) 'only on VPX 10.4 and newer
'    Dim tmp
'    tmp = ball.y * 2 / Table1.height-1
'    If tmp > 0 Then
'        AudioFade = Csng(tmp ^10)
'    Else
'        AudioFade = Csng(-((- tmp) ^10))
'    End If
'End Function

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
	PlaySound "fx_FondPolice"
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
	CheckEvents
    
   'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'Reset any table specific
	JackpotsBonus = 0
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
	PlaySoundAt "fx_plunger", plunger
    CreateMultiballTimer.Enabled = True
	LiSkillShot.state = 0
	ResetSkillShotTimer.Enabled = False
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
	resetBGBonusAttract

    Dim AwardPoints, TotalBonus, ii
	AwardPoints = 0
    TotalBonus = 0

    'If NOT Tilted Then
	If(Tilted = False) Then
		
        'Bonus
		PuPEvent 120
		PuPlayer.LabelShowPage pBackglass,1,0,""
		PuPlayer.LabelSet pBackglass,"BonusTitre","PUPAlphas\\OverlayBonus.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	
		AwardPoints = Bonus001 * 2500 'Slides
       TotalBonus = TotalBonus + AwardPoints

		AwardPoints = Bonus002 * 4500 'GRinds
       TotalBonus = TotalBonus + AwardPoints

		AwardPoints = Bonus003 * 5000 'Grabs
       TotalBonus = TotalBonus + AwardPoints

		AwardPoints = Bonus004 * 3000 'RampTricks
       TotalBonus = TotalBonus + AwardPoints

		AwardPoints = Bonus005 * 4000 'FlipTricks
       TotalBonus = TotalBonus + AwardPoints

		AwardPoints = Bonus006 * 1500 'HotTricks
       TotalBonus = TotalBonus + AwardPoints
        
		PuPlayer.LabelSet pBackglass,"BonusTotal", FormatNumber(TotalBonus,0),1,"{'mt':2,'color':263172, 'size': 5 }"	
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
       
		PuPlayer.LabelSet pBackglass,"BonusMultip","Bonus x " & FormatNumber(BonusMultiplier(CurrentPlayer),0),1,"{'mt':2,'color':263172, 'size': 6 }"

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
'	PuPEvent 101
	DOF 132, DOFPulse
	pupDMDDisplay "Splash4","Ball^SAVED","",4,1,10
	PuPEvent 103
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
					PlaySong "Mu_1"
					FlInitBumper 1, "purple"
					FlInitBumper 2, "purple"
					FlInitBumper 3, "purple"
                End If
            End If
            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then

                ' End Mode and timers
				StopSong
				PlaySound "Vo_End"
				EcranTV2001.ImageA = "TVmu000"
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
		If LightShootAgain.state = 2 And bBallSaverActive = True Then
		SelectMusicActive = False
		Else
		DOF 310, DOFon
		SelectMusicActive = True
		UpdateBGSelectMusic
		Playsound "Vo_SelectSong"
		If SkillShotState = 0 Then LiSkillShot.state = 2
		End If
		If BallsOnPlayfield > 1 Then SelectMusicActive = False : LiSkillShot.state = 0 : DOF 310, DOFoff
End Sub

Sub Gate001_Hit()
	AddScore 25
	DOF 310, DOFoff
	SkillShotLoserTimer.Enabled = False
	If SelectMusicActive = True Then PLaySelectedSelectMusic
	If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then 
        EnableBallSaver BallSaverTime
		SelectMusicActive = False
		ResetSkillShotTimer.Enabled = True
        Else
        ' show the message to shoot the ball in case the player has fallen sleep
        Trigger1.TimerEnabled = 1
    End If
End Sub
' The ball is released from the plunger

Sub Trigger1_UnHit()
    bBallInPlungerLane = False
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

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
End Sub

Sub ResetSkillShotTimer_Timer()
	LiSkillShot.state = 0
'	LiSKILL.state = 0
	ResetSkillShotTimer.Enabled = False
End Sub

Sub SkillShotLoserTimer_Timer()
	If SkillShotState = 1 Then
	pupDMDDisplay "Splash4","BAD SHOT You Lost^SkillShot and BallSave","",5,0,11
	SkillShotState = 2
	LiSkillShot.state = 0
	SkillShotLoserTimer.Enabled = False
	BallSaverTimer.Enabled = False
	bBallSaverReady = False
    bBallSaverActive = False
    LightShootAgain.State = 0
	End If
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
	AwardMusic
	pupDMDDisplay "Splash4","EXTRA^BALL","",4,1,20
	PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
	DOF 121, DOFPulse
	DOF 451, DOFPulse
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
Dim JackpotsBonus

Sub Game_Init() 'called at the start of a new game
	playclear pBackglass
    Dim i, j
    ChangeSong
	ResetEvents
	JackpotsBonus = 0
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
Dim RStep, LStep
Dim RSlingCount, LSlingCount
Dim SprayPos

Sub RightSlingShot_Slingshot
	PlaySoundAt SoundFXDOF("Sling_L", 104, DOFPulse, DOFcontactors), Sling1
    DOF 111, DOFPulse
	RS.VelocityCorrect(ActiveBall)
	Addscore 51
	RSlingCount = RSlingCount + 1
	If RSlingCount <= 5 Then Flasher027.imageA = "Sms3" : FlashForMs Flasher027, 500, 10, 1 : FlasherSlingRendTimer.Enabled = True
	If RSlingCount = 6 Then Flasher027.imageA = "Sms2" : FlashForMs Flasher027, 500, 10, 1 : FlasherSlingRendTimer.Enabled = True  : AddScore 2500
	If RSlingCount = 10 Then Flasher027.imageA = "Sms" : FlashForMs Flasher027, 500, 10, 1 : FlasherSlingRendTimer.Enabled = True  : AddScore 5000
	If RSlingCount = 14 Then Flasher027.imageA = "Sms4" : FlashForMs Flasher027, 500, 10, 1 : FlasherSlingRendTimer.Enabled = True  : AddScore 5000
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
	LSlingCount = LSlingCount + 1
	If LSlingCount <= 5 Then ShakeSpray : Addscore 51
	If LSlingCount = 6 Then ShakeSpray : Primitive056.visible = 0 : Primitive050.visible = 1 : AddScore 8000 : Playsound "fx_Bottle"
	If LSlingCount >= 7 Then ShakeSpray : AddScore 110
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

Sub FlasherSlingRendTimer_Timer()
	Flasher027.visible = 0
	FlasherSlingRendTimer.Enabled = False
End Sub

Sub ShakeSpray 
    SprayPos = 8
    SprayTimer.Enabled = 1
End Sub

Sub SprayTimer_Timer
	If LSlingCount <=6  Then Primitive050.TransY = SprayPos
	Primitive051.TransY = SprayPos
	Primitive053.TransY = SprayPos
	Primitive056.TransY = SprayPos
    If SprayPos = 0 Then Me.Enabled = 0:Exit Sub
    If SprayPos < 0 Then
        SprayPos = ABS(SprayPos) - 1
    Else
        SprayPos = - SprayPos + 1
    End If
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
		If BumperModeActive = False Then bumperbiglight1.state = 1 : bumperbiglight2.state = 1 : bumperbiglight3.state = 1
		If BumperModeActive = True Then BumperCount = BumperCount + 1 : bumperbiglight1.state = 2 : bumperbiglight2.state = 2 : bumperbiglight3.state = 2
		If GrueState = 3 And BumperCount >= 20 And BumperModeActive = True Then MoveGrue : BumperModeActive = False  : bumperbiglight1.state = 1 : bumperbiglight2.state = 1 : bumperbiglight3.state = 1
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
		AddScore 450
		If BumperModeActive = False Then bumperbiglight1.state = 1 : bumperbiglight2.state = 1 : bumperbiglight3.state = 1
		If BumperModeActive = True Then BumperCount = BumperCount + 1 : bumperbiglight1.state = 2 : bumperbiglight2.state = 2 : bumperbiglight3.state = 2
		If GrueState = 3 And BumperCount >= 20 And BumperModeActive = True Then MoveGrue : BumperModeActive = False : bumperbiglight1.state = 1 : bumperbiglight2.state = 1 : bumperbiglight3.state = 1
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
		AddScore 450
		If BumperModeActive = False Then bumperbiglight1.state = 1 : bumperbiglight2.state = 1 : bumperbiglight3.state = 1
		If BumperModeActive = True Then BumperCount = BumperCount + 1 : bumperbiglight1.state = 2 : bumperbiglight2.state = 2 : bumperbiglight3.state = 2
		If GrueState = 3 And BumperCount >= 20 And BumperModeActive = True Then MoveGrue : BumperModeActive = False : bumperbiglight1.state = 1 : bumperbiglight2.state = 1 : bumperbiglight3.state = 1
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
'***********
' Rainspin
'***********
sub timer1_Timer()
SkateBoard.rotY=rainspin1.currentangle
SkateBoard.rotZ=rainspin2.currentangle
end Sub

Sub Rainspin1_Spin
    PlaySoundAt "fx_spinner", rainspin1
	DOF 113, DOFPulse
    If Tilted Then Exit Sub
    Addscore 100
	If SpinnerModeActive = True Then SpinnerCount = SpinnerCount + 1
	If GrueState = 2 And SpinnerCount >=20 And SpinnerModeActive = True Then MoveGrue : SpinnerModeActive = False
End Sub

'*****************
'Gates
'*****************
sub Gate_Hit()
End Sub

'*****************
'Kickers
'*****************
Sub Kicker001_Hit()
	AddScore 250
	If LiLOCK.state=2 or LiMB.state=2 Then
	Kicker001.Kick 0, 80
	DOF 124, DOFPulse
	Else
	Kicker001.Kick 0, 50
	DOF 124, DOFPulse
	End If
	If LiSkillShot.state = 2 And ResetSkillShotTimer.Enabled = True Then
	LiSkillShot.state = 0
	pupDMDDisplay "Splash4","SKILLSHOT^25K","",4,1,16
	Playsound "Vo_SkillShot"
	AddScore 25000
	ResetSkillShotTimer.Enabled = False
	End If
End Sub

Sub Kicker002_Hit()
	AddScore 5000
	MBLock_Sav(CurrentPlayer) = MBLock_Sav(CurrentPlayer) + 1
	CheckLightMB
	If MBLock_Sav(CurrentPlayer) = 2 or MBLock_Sav(CurrentPlayer) = 4 Then 
	ResetTargetMB
	PlaySound "Vo_OneBallLock"
	pupDMDDisplay "balllock","Ball^is^Locked", "",4, 1,13
		If MBLock_Sav(CurrentPlayer) = 2 Then DOF 452, DOFPulse
		If MBLock_Sav(CurrentPlayer) = 4 Then DOF 453, DOFPulse
	kicker002.destroyball
	Kicker002.timerinterval = 2000
	kicker002.timerenabled = True
	Else
	Kicker002.Kick 10, 30
	GateCloseTimer.enabled = True
	End If
End Sub

Sub GateCloseTimer_Timer()
	ShakePorteOFF
	GateCloseTimer.enabled = False
End Sub

Sub Kicker002_Timer()
	EnableBallSaver 15
	PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
	KickerAuto.CreateBall
	KickerAuto.Kick 10, 41
	DOF 114, DOFPulse
	PlaySoundAt "fx_plunger", plunger
	ShakePorteOFF
	kicker002.timerenabled = False
End Sub

Sub Kicker003_Hit()
End Sub

Sub Kicker003_Timer()
	kicker003.kick 180, 2
	kicker003.timerenabled = False
End Sub

Sub Kicker004_Hit() 'Kicker Aide Loop
	kicker004.kick 10, 55
	DOF 444, DOFPulse
	StartTournPAL
	PuPEvent 130
	AddScore 250
	RampLoopCount(CurrentPlayer) = RampLoopCount(CurrentPlayer) + 1
	If RampLoopCount(CurrentPlayer) <= 2 Then Playsound "fx_Photo"
	If RampLoopCount(CurrentPlayer) = 3 Then Playsound "fx_Photo2"
	If RampLoopCount(CurrentPlayer) = 4 Then Playsound "Vo_SJackpot" : AddScore 10000 : pupDMDDisplay "Splash4","SUPER JACKPOT^10K","",4,1,10 : DOF 450, DOFPulse
	If RampLoopCount(CurrentPlayer) = 5 Then Playsound "fx_Trick" : AddMBmission
	If RampLoopCount(CurrentPlayer) >= 6 And RampLoopCount(CurrentPlayer) <= 8 Then Playsound "fx_Photo" 
	If RampLoopCount(CurrentPlayer) = 9 Then Playsound "fx_Photo2"
	If RampLoopCount(CurrentPlayer) >= 10 Then Playsound "fx_Trick" : RampLoopCount(CurrentPlayer) = 0 : ExtraBCount(CurrentPlayer) = ExtraBCount(CurrentPlayer) + 1 : CheckExtraBcount : PlaySound "Vo_AddLetter" : pupDMDDisplay "Splash4","ADD^LETTER","",4,0,12
End Sub

Sub Kicker005_Hit()
	Kicker005.Kick -10, 35
	Playsound "fx_Applaus"
	AddScore 45000 
	pupDMDDisplay "Splash4","MEGA RAMP^45K","",4,1,11 : DOF 128, DOFPulse
End Sub

Sub Kicker006_Hit() 'Kicker Aide Outlane
	kicker006.kick 0, 55
End Sub

Sub KickerLoopNoActiveTimer_Timer()
	kicker004.Enabled = False
	KickerLoopNoActiveTimer.Enabled = False
End Sub
Sub KickerOutlaneNoActiveTimer_Timer()
	kicker006.Enabled = False
	KickerOutlaneNoActiveTimer.Enabled = False
End Sub
'***********
' TAXI / POLICE
'***********

Sub StartModeTaxi
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to2"
	FlInitBumper 1, "purple"
	FlInitBumper 2, "purple"
	FlInitBumper 3, "purple"
	Flasher019.Visible = 0
	Flasher020.Visible = 0
	Flasher021.Visible = 0
	Flasher022.Visible = 0
	Flasher023.Visible = 0
	Flasher024.Visible = 0
	Flasher025.Visible = 0
	Flasher026.Visible = 0
	FlasherPx.ImageA = "PxSkate"
	LampadaireOFF
	ChangeLAMPH yellow 'std
	ChangeLAMPB white 'std
	ModeCrimeActive = False
	CrimeCount = 0
	CaptTargetCount = 0
	Light2.state = 0
	Light3.state = 0
	Light4.state = 0
	Light5.state = 0
	CarTaxi.visible = 1
	PrimitiveGiro1.visible = 1
	CarPolice.visible = 0
	PrimitiveGiro2.visible = 0
	Flasher009.visible = 0
	Flasher010.visible = 0
	Flasher011.visible = 0
	Flasher012.visible = 0
	Flasher013.visible = 0
	Flasher014.visible = 0
	Flasher016.visible = 0
	Flasher017.visible = 0
	Flasher018.visible = 0
	Flasher029.visible = 0
	Flasher030.visible = 0
	Flasher031.visible = 0
	Flasher032.visible = 0
	Flasher033.visible = 0
	Flasher034.visible = 0
	Flasher035.visible = 0
	Flasher036.visible = 0
	Flasher049.Visible = 0
	Flasher051.Visible = 0
End Sub
Sub StartModePolice
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to4"
	FlInitBumper 1, "blacklight"
	FlInitBumper 2, "blacklight"
	FlInitBumper 3, "blacklight"
	ChangeLAMPH baseLH 'nuit
	ChangeLAMPB baseLB 'nuit
	Flasher019.Visible = 1
	Flasher020.Visible = 1
	Flasher021.Visible = 1
	Flasher022.Visible = 1
	Flasher023.Visible = 1
	Flasher024.Visible = 1
	Flasher025.Visible = 1
	Flasher026.Visible = 1
	FlasherPx.ImageA = "PxSkate2"
	Light2.state = 0
	Light3.state = 0
	Light4.state = 0
	Light5.state = 0
	CarTaxi.visible = 0
	PrimitiveGiro1.visible = 0
	CarPolice.visible = 1
	PrimitiveGiro2.visible = 1
	StartLightPoliceTimer.Enabled = True
	Flasher014.visible = 1
	Flasher016.visible = 1
	Flasher017.visible = 1
	Flasher018.visible = 1
	FlashForMs Flasher029, 1000, 30, 1 
	FlashForMs Flasher030, 1000, 30, 1 
	FlashForMs Flasher031, 1000, 30, 1 
	FlashForMs Flasher032, 1000, 30, 1 
	FlashForMs Flasher033, 1000, 30, 1 
	FlashForMs Flasher034, 1000, 30, 1 
	FlashForMs Flasher035, 1000, 30, 1 
	FlashForMs Flasher036, 1000, 30, 1 
	FlashForMs Flasher049, 1000, 30, 1
	FlashForMs Flasher051, 1000, 30, 1
End Sub
Sub StartLightPoliceTimer_Timer()
	EndLightPoliceTimer.Enabled = True
	Light2.state = 2
	Light3.state = 2
	Light4.state = 1
	Light5.state = 1
	StartLightPoliceTimer.Enabled = False
End Sub
Sub EndLightPoliceTimer_Timer()
	Light2.state = 0
	Light3.state = 0
	Light4.state = 0
	Light5.state = 0
	EndLightPoliceTimer.Enabled = False
End Sub
'***********
' Target
'***********
Dim CaptTargetCount
Sub CapTarget_Hit()
	AddScore 450
	CaptTargetCount = CaptTargetCount + 1
	If CaptTargetCount = 1 Then	PlaySound "Vo_Taxi2" 
	If CaptTargetCount = 2 Then PlaySound "Vo_Taxi" : LampadaireON : pupDMDDisplay "Splash4","ONE MORE^NIGHT MODE","",4,1,10
	If CaptTargetCount = 3 Then StartModePolice : PlaySound "Vo_Police3" : Flasher009.visible = 1 : Flasher010.visible = 1
	If CaptTargetCount = 4 Then PlaySound "Vo_Police" : StartLightPoliceTimer.Enabled = True : Flasher011.visible = 1 : Flasher012.visible = 1 : Flasher013.visible = 1 : ModeCrimeActive = True
	If CaptTargetCount = 5 Then PlaySound "Vo_Police4" : StartLightPoliceTimer.Enabled = True 
	If CaptTargetCount = 6 Then PlaySound "Vo_Police2" : StartLightPoliceTimer.Enabled = True 
	If CaptTargetCount >= 7 Then PlaySound "Vo_Police" : StartLightPoliceTimer.Enabled = True 
	FlashForMs Light013, 2000, 30, 0 
	FlashForMs Light014, 2000, 30, 0 
End Sub
Sub LampadaireON
	Light031.State = 1
	Light032.State = 1
	Light035.State = 1
	Light036.State = 1
	Light037.State = 1
	Light038.State = 1
End Sub
Sub LampadaireOFF
	Light031.State = 0
	Light032.State = 0
	Light035.State = 0
	Light036.State = 0
	Light037.State = 0
	Light038.State = 0
End Sub
Sub Target001_Hit()
	AddScore 250
	LetterTargetS(CurrentPlayer) = LetterTargetS(CurrentPlayer) + 1
	PlaySound "fx_target"
	DOF 334, DOFPulse
	Li014.state = 1
	If Target002.IsDropped = 1 And Target003.IsDropped = 1 And Target004.IsDropped = 1 And Target005.IsDropped = 1 Then
	MBLock_Sav(CurrentPlayer) = MBLock_Sav(CurrentPlayer) + 1
	CheckLightMB
	ShakePorteON
	DOF 401, DOFPulse
	End If
End Sub
Sub Target002_Hit()
	AddScore 250
	LetterTargetK(CurrentPlayer) = LetterTargetK(CurrentPlayer) + 1
	PlaySound "fx_target"
	DOF 335, DOFPulse
	Li013.state = 1
	If Target001.IsDropped = 1 And Target003.IsDropped = 1 And Target004.IsDropped = 1 And Target005.IsDropped = 1 Then
	MBLock_Sav(CurrentPlayer) = MBLock_Sav(CurrentPlayer) + 1
	CheckLightMB
	ShakePorteON
	DOF 401, DOFPulse
	End If
End Sub
Sub Target003_Hit()
	AddScore 250
	LetterTargetA(CurrentPlayer) = LetterTargetA(CurrentPlayer) + 1
	PlaySound "fx_target"
	DOF 336, DOFPulse
	Li012.state = 1
	If Target001.IsDropped = 1 And Target002.IsDropped = 1 And Target004.IsDropped = 1 And Target005.IsDropped = 1 Then
	MBLock_Sav(CurrentPlayer) = MBLock_Sav(CurrentPlayer) + 1
	CheckLightMB
	ShakePorteON
	DOF 401, DOFPulse
	End If
End Sub
Sub Target004_Hit()
	AddScore 250
	LetterTargetT(CurrentPlayer) = LetterTargetT(CurrentPlayer) + 1
	PlaySound "fx_target"
	DOF 337, DOFPulse
	Li011.state = 1
	If Target001.IsDropped = 1 And Target002.IsDropped = 1 And Target003.IsDropped = 1 And Target005.IsDropped = 1 Then
	MBLock_Sav(CurrentPlayer) = MBLock_Sav(CurrentPlayer) + 1
	CheckLightMB
	ShakePorteON
	DOF 401, DOFPulse
	End If
End Sub
Sub Target005_Hit()
	AddScore 250
	LetterTargetE(CurrentPlayer) = LetterTargetE(CurrentPlayer) + 1
	PlaySound "fx_target"
	DOF 338, DOFPulse
	Li010.state = 1
	If Target001.IsDropped = 1 And Target002.IsDropped = 1 And Target003.IsDropped = 1 And Target004.IsDropped = 1 Then
	MBLock_Sav(CurrentPlayer) = MBLock_Sav(CurrentPlayer) + 1
	CheckLightMB
	ShakePorteON
	DOF 401, DOFPulse
	End If
End Sub

Sub ResetTargetMB
	Li010.state = 2
	Li011.state = 2
	Li012.state = 2
	Li013.state = 2
	Li014.state = 2
	Target001.IsDropped = False
	Target002.IsDropped = False
	Target003.IsDropped = False
	Target004.IsDropped = False
	Target005.IsDropped = False
	LetterTargetS(CurrentPlayer) = 0
	LetterTargetK(CurrentPlayer) = 0
	LetterTargetA(CurrentPlayer) = 0
	LetterTargetT(CurrentPlayer) = 0
	LetterTargetE(CurrentPlayer) = 0
	ShakePorteOFF
	Wall036.Collidable = 1
	DOF 116, DOFPulse
End Sub

Sub Target006_Hit() 
	AddScore 1500
	DOF 126, DOFPulse
	TargetTVCount = TargetTVCount + 1
	If TargetTVCount = 3 Then AddScore 1500 : pupDMDDisplay "Splash4","TWO MORE^BALL SAVED ACTIVE","",4,1,10
	If TargetTVCount = 4 Then AddScore 5000 : li0001.state = 2 : pupDMDDisplay "Splash4","ONE MORE^BALL SAVED ACTIVE","",4,1,10
	If TargetTVCount >= 5 And BallsOnPlayfield <= 2 Then EnableBallSaver 15 : TargetTVCount = 0 : li0001.state = 0
	PlaySound "fx_Completion"
	StartFondTV
End Sub

Sub Target007_Hit()
	AddScore 550
	PlaySound "fx_target"
	If Li002.state = 2 And (LetterSpecialS(CurrentPlayer) + LetterSpecialP(CurrentPlayer) +	LetterSpecialE(CurrentPlayer) + LetterSpecialC(CurrentPlayer) +	LetterSpecialI(CurrentPlayer) +	LetterSpecialA(CurrentPlayer) +	LetterSpecialL(CurrentPlayer)) = 0 Then
	StartMissionSPECIAL
	PlaySound "Vo_CollectAllLetters"
	DOF 126, DOFPulse
	Li002.state = 0
	End If
End Sub

Sub CheckStartSpecial
	If Li003.state = 0 And Li004.state = 0 And Li005.state = 0 And Li006.state = 0 And Li007.state = 0 And Li008.state = 0 And Li009.state = 0 Then
	Li002.state = 2
	PlaySound "fx_win"
	End If
End Sub
Sub ResetLightSpecial
	Li002.state = 0
	Li003.state = 2 
	Li004.state = 2 
	Li005.state = 2 
	Li006.state = 2 	
	Li007.state = 2 	
	Li008.state = 2 
	Li009.state = 2
End Sub

Sub ResetTargetSpecial
	TargetSpecialS(CurrentPlayer) = 0
	TargetSpecialP(CurrentPlayer) = 0
	TargetSpecialE(CurrentPlayer) = 0
	TargetSpecialC(CurrentPlayer) = 0
	TargetSpecialI(CurrentPlayer) = 0
	TargetSpecialA(CurrentPlayer) = 0
	TargetSpecialL(CurrentPlayer) = 0
End Sub

Sub CheckLightSpecial
	If SpecialLetterComplet(CurrentPlayer) <= 1 And ModeSpecialActive(CurrentPlayer) = False Then
		If TargetSpecialS(CurrentPlayer) = 1 Then Li009.state = 0
		if TargetSpecialS(CurrentPlayer) = 0 Then Li009.state = 2
		If TargetSpecialP(CurrentPlayer) = 1 Then Li008.state = 0
		if TargetSpecialP(CurrentPlayer) = 0 Then Li008.state = 2
		If TargetSpecialE(CurrentPlayer) = 1 Then Li007.state = 0
		if TargetSpecialE(CurrentPlayer) = 0 Then Li007.state = 2
		If TargetSpecialC(CurrentPlayer) = 1 Then Li006.state = 0
		if TargetSpecialC(CurrentPlayer) = 0 Then Li006.state = 2
		If TargetSpecialI(CurrentPlayer) = 1 Then Li005.state = 0
		if TargetSpecialI(CurrentPlayer) = 0 Then Li005.state = 2
		If TargetSpecialA(CurrentPlayer) = 1 Then Li004.state = 0
		if TargetSpecialA(CurrentPlayer) = 0 Then Li004.state = 2
		If TargetSpecialL(CurrentPlayer) = 1 Then Li003.state = 0
		if TargetSpecialL(CurrentPlayer) = 0 Then Li003.state = 2
		If TargetSpecialS(CurrentPlayer) = 1 And TargetSpecialP(CurrentPlayer) = 1 And TargetSpecialE(CurrentPlayer) = 1 And TargetSpecialC(CurrentPlayer) = 1 And TargetSpecialI(CurrentPlayer) = 1 And TargetSpecialA(CurrentPlayer) = 1 And TargetSpecialL(CurrentPlayer) = 1 Then 
		li002.State = 2
		Else li002.State = 0
		End If
	End If
End Sub

Sub Target008_Hit()
	AddScore 250
	PlaySound "fx_Trick2"
	If Li003.state = 2 Then
		If SpecialLetterComplet(CurrentPlayer) <= 1 Then	
		Li003.state = 0
		TargetSpecialL(CurrentPlayer)=1
		CheckStartSpecial
		Elseif SpecialLetterComplet(CurrentPlayer) >= 2 Then	
		Li003.state = 1 
		End If
	End If
End Sub
Sub Target009_Hit()
	AddScore 250
	PlaySound "fx_Trick2"
	If Li004.state = 2 Then
		If SpecialLetterComplet(CurrentPlayer) <= 1 Then	
		Li004.state = 0	
		TargetSpecialA(CurrentPlayer)=1
		CheckStartSpecial
		Elseif SpecialLetterComplet(CurrentPlayer) >= 2 Then	
		Li004.state = 1 
		End If
	End If
End Sub
Sub Target010_Hit()
	AddScore 250
	PlaySound "fx_Trick2"
	If Li005.state = 2 Then
		If SpecialLetterComplet(CurrentPlayer) <= 1 Then	
		Li005.state = 0
		TargetSpecialI(CurrentPlayer)=1
		CheckStartSpecial
		Elseif SpecialLetterComplet(CurrentPlayer) >= 2 Then	
		Li005.state = 1 
		End If
	End If
End Sub
Sub Target011_Hit()
	AddScore 250
	PlaySound "fx_Trick2"
	If Li006.state = 2 Then
		If SpecialLetterComplet(CurrentPlayer) <= 1 Then	
		Li006.state = 0
		TargetSpecialC(CurrentPlayer)=1
		CheckStartSpecial
		Elseif SpecialLetterComplet(CurrentPlayer) >= 2 Then	
		Li006.state = 1 
		End If
	End If
End Sub
Sub Target012_Hit()
	AddScore 250
	PlaySound "fx_Trick2"
	If Li007.state = 2 Then
		If SpecialLetterComplet(CurrentPlayer) <= 1 Then	
		Li007.state = 0
		TargetSpecialE(CurrentPlayer)=1
		CheckStartSpecial
		Elseif SpecialLetterComplet(CurrentPlayer) >= 2 Then	
		Li007.state = 1 
		End If
	End If
End Sub
Sub Target013_Hit()
	AddScore 250
	PlaySound "fx_Trick2"
	If Li008.state = 2 Then
		If SpecialLetterComplet(CurrentPlayer) <= 1 Then	
		Li008.state = 0
		TargetSpecialP(CurrentPlayer)=1
		CheckStartSpecial
		Elseif SpecialLetterComplet(CurrentPlayer) >= 2 Then	
		Li008.state = 1 
		End If
	End If
End Sub
Sub Target014_Hit()
	AddScore 250
	PlaySound "fx_Trick2"
	If Li009.state = 2 Then
		If SpecialLetterComplet(CurrentPlayer) <= 1 Then	
		Li009.state = 0
		TargetSpecialS(CurrentPlayer)=1
		CheckStartSpecial
		Elseif SpecialLetterComplet(CurrentPlayer) >= 2 Then	
		Li009.state = 1 
		End If
	End If
End Sub

Sub CheckLightRamp
	If RampLCount(CurrentPlayer) = 3 Or RampLCount(CurrentPlayer) = 4 Or RampLCount(CurrentPlayer) = 4 Then Light039.State=2 Else Light039.State=0
	If RampLGarageCount(CurrentPlayer) = 3 Or RampLGarageCount(CurrentPlayer) = 4 Or RampLGarageCount(CurrentPlayer) = 9 Then Light040.State=2 Else Light040.State=0
	If RampMBumperACount(CurrentPlayer) = 3 Or RampMBumperACount(CurrentPlayer) = 4 Or RampMBumperACount(CurrentPlayer) = 9 Then Light033.State=2 Else Light033.State=0
'	If RampMBumperBCount(CurrentPlayer) = 4 Or RampMBumperBCount(CurrentPlayer) = 9 Then
	If RampLoopCount(CurrentPlayer) = 3 Or RampLoopCount(CurrentPlayer) = 4 Or RampLoopCount(CurrentPlayer) = 9 Then Light034.State=2 Else Light034.State=0
	If RampMCount(CurrentPlayer) = 3 Or RampMCount(CurrentPlayer) = 4 Or RampMCount(CurrentPlayer) = 9 Then Light041.State=2 Else Light041.State=0
	If RampRCount(CurrentPlayer) = 3 Or RampRCount(CurrentPlayer) = 4 Or RampRCount(CurrentPlayer) = 9 Or GrueState = 2 Then Light042.State=2 Else Light042.State=0
'	If RampRMBCount(CurrentPlayer) = 4 Or RampRMBCount(CurrentPlayer) = 9 Then 
End Sub
'*****************
'Triggers
'*****************

Sub TriggerDebug_Hit()
	CheckLightRamp
End Sub

Sub Trigger023_Hit()
	Trigger023.destroyball
	Trigger023.enabled = False
End Sub
Sub Trigger024_Hit()
	Trigger024.destroyball
	Trigger024.enabled = False
End Sub
Sub TriggerTEST_Hit()
	If SkillShotState = 0 Then SkillShotState = 1 : SkillShotLoserTimer.Enabled = True
'	Bonus001 = 3 'Slides
'	AddScore 800000
End Sub

Sub Trigger001_Hit()
	Trigger027.enabled = True
	FlashForMs Flasher037, 1000, 30, 1
	FlashForMs Flasher038, 1000, 30, 1
	FlashForMs Flasher039, 1000, 30, 1
	AddScore 500
	DOF 480, DOFPulse
	RampLGarageCount(CurrentPlayer) = RampLGarageCount(CurrentPlayer) + 1
	If RampLGarageCount(CurrentPlayer) <= 2 Then Playsound "fx_Photo"
	If RampLGarageCount(CurrentPlayer) = 3 Then Playsound "fx_Photo2"
	If RampLGarageCount(CurrentPlayer) = 4 Then Playsound "Vo_SJackpot" : AddScore 10000 : pupDMDDisplay "Splash4","SUPER JACKPOT^10K","",4,1,10 : DOF 450, DOFPulse : PuPEvent 104
	If RampLGarageCount(CurrentPlayer) = 5 Then Playsound "fx_Trick" : AddMBmission
	If RampLGarageCount(CurrentPlayer) >= 6 And RampLGarageCount(CurrentPlayer) <= 8 Then Playsound "fx_Photo" 
	If RampLGarageCount(CurrentPlayer) = 9 Then Playsound "fx_Photo2"
	If RampLGarageCount(CurrentPlayer) >= 10 Then Playsound "fx_Trick" : RampLGarageCount(CurrentPlayer) = 0 : ExtraBCount(CurrentPlayer) = ExtraBCount(CurrentPlayer) + 1 : CheckExtraBcount : PlaySound "Vo_AddLetter" : pupDMDDisplay "Splash4","ADD^LETTER","",4,0,12
	Wall032.SideVisible = True
End Sub
Sub Trigger002_Hit() ' LettreSpecial S
	DOF 441, DOFPulse
	AddScore 250
	If CaptTargetCount >= 2 Then FlashForMs Light031, 2000, 30, 1 :	FlashForMs Light032, 2000, 30, 1
	If ModeSpecialActive(CurrentPlayer) = True And FlasherSpecial001.visible = True Then
	AddScore 3250
	LetterSpecialS(CurrentPlayer) = 1
	CheckSpecialAward
	pupDMDDisplay "Splash4","S^COLLECTED","",4,1,10
	PlaySound "fx_unlock"
	FlasherSpecial001.visible = 0
	End If
End Sub
Sub Trigger003_Hit() ' LettreSpecial P
	AddScore 250
	If ModeSpecialActive(CurrentPlayer) = True And FlasherSpecial002.visible = True Then
	AddScore 3250
	LetterSpecialP(CurrentPlayer) = 1
	CheckSpecialAward
	pupDMDDisplay "Splash4","P^COLLECTED","",4,1,10
	PlaySound "fx_unlock"
	FlasherSpecial002.visible = 0
	End If
End Sub
Sub Trigger004_Hit() ' LettreSpecial E
	AddScore 250
	If ModeSpecialActive(CurrentPlayer) = True And FlasherSpecial003.visible = True Then
	AddScore 3250
	LetterSpecialE(CurrentPlayer) = 1
	CheckSpecialAward
	pupDMDDisplay "Splash4","E^COLLECTED","",4,1,10
	PlaySound "fx_unlock"
	FlasherSpecial003.visible = 0
	End If
End Sub
Sub Trigger005_Hit() ' LettreSpecial C
	AddScore 250
	If Bonus001 <6 And Bonus001active = True Then Bonus001 = Bonus001 + 1
	If Bonus001 = 6 And Bonus001active = True Then Bonus001active = False : BonusMultiplier(CurrentPlayer) = BonusMultiplier(CurrentPlayer) + 1
	DOF 445, DOFPulse
	If ModeSpecialActive(CurrentPlayer) = False Then
		RampRCount(CurrentPlayer) = RampRCount(CurrentPlayer) + 1
		If RampRCount(CurrentPlayer) <= 2 Then Playsound "fx_Photo"
		If RampRCount(CurrentPlayer) = 3 Then Playsound "fx_Photo2" : Light042.State = 2 : pupDMDDisplay "attract","","@dmd4.mp4",2,0,10
		If RampRCount(CurrentPlayer) = 4 Then Playsound "Vo_Jackpot" : AddScore 5000 : pupDMDDisplay "Splash4","JACPOT^5K","",2,1,11 : DOF 128, DOFPulse : PuPEvent 104 : : Light042.State = 2
		If RampRCount(CurrentPlayer) = 5 Then Playsound "fx_Trick" : AddMBmission : Light042.State = 0
		If RampRCount(CurrentPlayer) >= 6 And RampRCount(CurrentPlayer) <= 8 Then Playsound "fx_Photo" 
		If RampRCount(CurrentPlayer) = 9 Then Playsound "fx_Photo2" : : Light042.State = 2 : pupDMDDisplay "attract","","@dmd4.mp4",2,0,11
		If RampRCount(CurrentPlayer) >= 10 Then Playsound "fx_Trick" : RampRCount(CurrentPlayer) = 0 : ExtraBCount(CurrentPlayer) = ExtraBCount(CurrentPlayer) + 1 : CheckExtraBcount : PlaySound "Vo_AddLetter" : Light042.State = 0 : pupDMDDisplay "Splash4","ADD^LETTER","",4,0,12
	End If
	If ModeSpecialActive(CurrentPlayer) = True And FlasherSpecial004.visible = True Then
	AddScore 3250
	LetterSpecialC(CurrentPlayer) = 1
	CheckSpecialAward
	pupDMDDisplay "Splash4","C^COLLECTED","",4,1,10
	PlaySound "fx_unlock"
	FlasherSpecial004.visible = 0
	End If
End Sub
Sub Trigger006_Hit() ' LettreSpecial I
	AddScore 250
	DOF 443, DOFPulse
	If ModeSpecialActive(CurrentPlayer) = True And FlasherSpecial005.visible = True Then
	AddScore 3250
	LetterSpecialI(CurrentPlayer) = 1
	CheckSpecialAward
	pupDMDDisplay "Splash4","I^COLLECTED","",4,1,10
	PlaySound "fx_unlock"
	FlasherSpecial005.visible = 0
	End If
End Sub
Sub Trigger007_Hit() ' LettreSpecial A
	AddScore 250
	pupDMDDisplay "attract","","@dmd3.mp4",4,0,9
	If Bonus003 <6 And Bonus003active = True Then Bonus003 = Bonus003 + 1
	If Bonus003 = 6 And Bonus003active = True Then Bonus003active = False : BonusMultiplier(CurrentPlayer) = BonusMultiplier(CurrentPlayer) + 1
	If CaptTargetCount >= 2 Then FlashForMs Light036, 2000, 30, 1 :	FlashForMs Light035, 2000, 30, 1
	AddScore 500
	Wall032.SideVisible = False
	If ModeSpecialActive(CurrentPlayer) = True And FlasherSpecial006.visible = True Then
	AddScore 3250
	LetterSpecialA(CurrentPlayer) = 1
	pupDMDDisplay "Splash4","A^COLLECTED","",4,1,10
	CheckSpecialAward
	PlaySound "fx_unlock"
	FlasherSpecial006.visible = 0
	End If
End Sub
Sub Trigger008_Hit() ' LettreSpecial L
	AddScore 250
	DOF 440, DOFPulse
	If Bonus002 <6 And Bonus002active = True Then Bonus002 = Bonus002 + 1
	If Bonus002 = 6 And Bonus002active = True Then Bonus002active = False : BonusMultiplier(CurrentPlayer) = BonusMultiplier(CurrentPlayer) + 1
	If CaptTargetCount >= 2 Then FlashForMs Light038, 2000, 30, 1 :	FlashForMs Light037, 2000, 30, 1
	If ModeSpecialActive(CurrentPlayer) = False Then
		RampRMBCount(CurrentPlayer) = RampRMBCount(CurrentPlayer) + 1
		If RampRMBCount(CurrentPlayer) <= 2 Then Playsound "fx_Photo"
		If RampRMBCount(CurrentPlayer) = 3 Then Playsound "fx_Photo2" : pupDMDDisplay "attract","","@dmd2.mp4",4,0,10
		If RampRMBCount(CurrentPlayer) = 4 Then Playsound "Vo_SJackpot" : AddScore 10000 : pupDMDDisplay "Splash4","SUPER JACKPOT^10K","",4,1,11 : DOF 450, DOFPulse
		If RampRMBCount(CurrentPlayer) = 5 Then Playsound "fx_Trick" : AddMBmission
		If RampRMBCount(CurrentPlayer) >= 6 And RampRMBCount(CurrentPlayer) <= 8 Then Playsound "fx_Photo" 
		If RampRMBCount(CurrentPlayer) = 9 Then Playsound "fx_Photo2" : pupDMDDisplay "attract","","@dmd2.mp4",4,0,11
		If RampRMBCount(CurrentPlayer) >= 10 Then Playsound "fx_Trick" : RampRMBCount(CurrentPlayer) = 0 : ExtraBCount(CurrentPlayer) = ExtraBCount(CurrentPlayer) + 1 : CheckExtraBcount : PlaySound "Vo_AddLetter" : pupDMDDisplay "Splash4","ADD^LETTER","",4,0,12
	End If
	If ModeSpecialActive(CurrentPlayer) = True And FlasherSpecial007.visible = True Then
	AddScore 3250
	pupDMDDisplay "Splash4","L^COLLECTED","",4,1,10
	LetterSpecialL(CurrentPlayer) = 1
	CheckSpecialAward
	PlaySound "fx_unlock"
	FlasherSpecial007.visible = 0
	End If
End Sub

Sub Trigger009_Hit()
	If Flasher011.visible = True And ModeCrimeActive = True Then
	CrimeCount = CrimeCount + 1
	StartCrimeA
	PlaySound "fx_rubber_flipper"
	AddScore 5000
	CheckCrime
	End If
End Sub
Sub Trigger010_Hit()
	If Flasher012.visible = True And ModeCrimeActive = True  Then
	CrimeCount = CrimeCount + 1
	StartCrimeB
	PlaySound "fx_rubber_flipper"
	AddScore 5000
	CheckCrime
	End If
End Sub
Sub Trigger011_Hit()
	If Flasher013.visible = True And ModeCrimeActive = True  Then
	CrimeCount = CrimeCount + 1
	StartCrimeA2
	PlaySound "fx_rubber_flipper"
	AddScore 5000
	CheckCrime
	End If
End Sub

Sub Trigger012_Hit()
	If Bonus004 <7 And Bonus004active = True Then Bonus004 = Bonus004 + 1
	If Bonus004 = 7 And Bonus004active = True Then Bonus004active = False : BonusMultiplier(CurrentPlayer) = BonusMultiplier(CurrentPlayer) + 1
	RampLCount(CurrentPlayer) = RampLCount(CurrentPlayer) + 1
	If RampLCount(CurrentPlayer) <= 2 Then Playsound "fx_Photo"
	If RampLCount(CurrentPlayer) = 3 Then Playsound "fx_Photo2" : pupDMDDisplay "attract","","@dmd5.mp4",3,0,10
	If RampLCount(CurrentPlayer) = 4 Then Playsound "Vo_Jackpot" : AddScore 5000 : pupDMDDisplay "Splash4","JACKPOT^5K","",4,1,10 : DOF 128, DOFPulse : PuPEvent 104
	If RampLCount(CurrentPlayer) = 5 Then Playsound "fx_Trick" : AddMBmission
	If RampLCount(CurrentPlayer) >= 6 And RampLCount(CurrentPlayer) <= 8 Then Playsound "fx_Photo" 
	If RampLCount(CurrentPlayer) = 9 Then Playsound "fx_Photo2" : pupDMDDisplay "attract","","@dmd5.mp4",3,0,10
	If RampLCount(CurrentPlayer) >= 10 Then Playsound "fx_Trick" : RampLCount(CurrentPlayer) = 0 : ExtraBCount(CurrentPlayer) = ExtraBCount(CurrentPlayer) + 1 : CheckExtraBcount : PlaySound "Vo_AddLetter" : pupDMDDisplay "Splash4","ADD^LETTER","",4,0,12
	AddScore 500
End Sub

Sub Trigger013_Hit()
	If Bonus005 <7 And Bonus005active = True Then Bonus005 = Bonus005 + 1
	If Bonus005 = 7 And Bonus005active = True Then Bonus005active = False : BonusMultiplier(CurrentPlayer) = BonusMultiplier(CurrentPlayer) + 1
	RampMCount(CurrentPlayer) = RampMCount(CurrentPlayer) + 1
	If RampMCount(CurrentPlayer) <= 2 Then Playsound "fx_Photo"
	If RampMCount(CurrentPlayer) = 3 Then Playsound "fx_Photo2"
	If RampMCount(CurrentPlayer) = 4 Then Playsound "Vo_Jackpot" : AddScore 5000 : pupDMDDisplay "Splash4","JACKPOT^5K","",4,1,10 : DOF 128, DOFPulse : PuPEvent 104
	If RampMCount(CurrentPlayer) = 5 Then Playsound "fx_Trick" : AddMBmission
	If RampMCount(CurrentPlayer) >= 6 And RampMCount(CurrentPlayer) <= 8 Then Playsound "fx_Photo" 
	If RampMCount(CurrentPlayer) = 9 Then Playsound "fx_Photo2"
	If RampMCount(CurrentPlayer) >= 10 Then Playsound "fx_Trick" : RampMCount(CurrentPlayer) = 0 : ExtraBCount(CurrentPlayer) = ExtraBCount(CurrentPlayer) + 1 : CheckExtraBcount : PlaySound "Vo_AddLetter" : pupDMDDisplay "Splash4","ADD^LETTER","",4,0,12
	AddScore 500
End Sub

Sub Trigger014_Hit() 'Timer KickerLoop
	Kicker004.Enabled = True
	KickerLoopNoActiveTimer.Enabled = True
End Sub
Sub Trigger028_Hit() 'Timer KickerLoopEnd
	Kicker004.Enabled = False
End Sub
Sub Trigger021_Hit() 'Timer KickerOutlane
	Kicker006.Enabled = True
	KickerOutlaneNoActiveTimer.Enabled = True
End Sub
Sub Trigger025_Hit() 'Timer KickerMAXIRAMP
	Kicker005.Enabled = True
End Sub
Sub Trigger026_Hit() 'Timer KickerMAXIRAMP
	Kicker005.Enabled = False
End Sub
Sub Trigger015_Hit() 'Timer Ramp Normal
	RampMBumperATimer.Enabled = True
End Sub
Sub Trigger016_Hit() 'Timer Ramp inverse
	RampMBumperBTimer.Enabled = True
End Sub
Sub Trigger017_Hit() 'Timer Ramp Normal
	AddScore 500
	DOF 442, DOFPulse
	If RampMBumperATimer.Enabled = True And Bonus006 <7 And Bonus006active = True Then Bonus006 = Bonus006 + 1
	If RampMBumperATimer.Enabled = True And Bonus006 = 7 And Bonus006active = True Then Bonus006active = False : BonusMultiplier(CurrentPlayer) = BonusMultiplier(CurrentPlayer) + 1
	If RampMBumperATimer.Enabled = True Then RampMBumperACount(CurrentPlayer) = RampMBumperACount(CurrentPlayer) + 1 
	If RampMBumperBTimer.Enabled = True Then RampMBumperBCount(CurrentPlayer) = RampMBumperBCount(CurrentPlayer) + 1
	If RampMBumperACount(CurrentPlayer) <= 2 Then Playsound "fx_Photo"
	If RampMBumperACount(CurrentPlayer) = 3 Then Playsound "fx_Photo2"
	If RampMBumperACount(CurrentPlayer) = 4 Then Playsound "Vo_Jackpot" : AddScore 5000 : pupDMDDisplay "Splash4","JACKPOT^5K","",4,1,10 : DOF 128, DOFPulse : PuPEvent 104
	If RampMBumperACount(CurrentPlayer) = 5 Then Playsound "fx_Trick" : AddMBmission
	If RampMBumperACount(CurrentPlayer) >= 6 And RampMBumperACount(CurrentPlayer) <= 8 Then Playsound "fx_Photo" 
	If RampMBumperACount(CurrentPlayer) = 9 Then Playsound "fx_Photo2"
	If RampMBumperACount(CurrentPlayer) >= 10 Then Playsound "fx_Trick" : RampMBumperACount(CurrentPlayer) = 0 : ExtraBCount(CurrentPlayer) = ExtraBCount(CurrentPlayer) + 1 : CheckExtraBcount : PlaySound "Vo_AddLetter" : pupDMDDisplay "Splash4","ADD^LETTER","",4,0,12
	If RampMBumperBCount(CurrentPlayer) <= 2 Then Playsound "fx_Photo"
	If RampMBumperBCount(CurrentPlayer) = 3 Then Playsound "fx_Photo2" : pupDMDDisplay "attract","","@dmd1.mp4",3,0,10
	If RampMBumperBCount(CurrentPlayer) = 4 Then Playsound "Vo_Jackpot" : AddScore 5000 : pupDMDDisplay "Splash4","JACKPOT^5K","",4,1,10 : DOF 128, DOFPulse : PuPEvent 104
	If RampMBumperBCount(CurrentPlayer) = 5 Then Playsound "Vo_SJackpot" : AddScore 15000 : pupDMDDisplay "Splash4","SUPER JACKPOT^15K","",3,1,10 : DOF 450, DOFPulse : PuPEvent 104
	If RampMBumperBCount(CurrentPlayer) >= 6 And RampMBumperBCount(CurrentPlayer) <= 8 Then Playsound "fx_Photo" 
	If RampMBumperBCount(CurrentPlayer) = 9 Then Playsound "fx_Photo2" : pupDMDDisplay "attract","","@dmd1.mp4",3,0,10
	If RampMBumperBCount(CurrentPlayer) >= 10 Then Playsound "fx_Trick" : RampMBumperBCount(CurrentPlayer) = 0 : ExtraBCount(CurrentPlayer) = ExtraBCount(CurrentPlayer) + 1 : CheckExtraBcount : PlaySound "Vo_AddLetter" : pupDMDDisplay "Splash4","ADD^LETTER","",4,0,12
End Sub

Sub RampMBumperATimer_Timer()
	RampMBumperATimer.Enabled = False
End Sub
Sub RampMBumperBTimer_Timer()
	RampMBumperBTimer.Enabled = False
End Sub

Sub Trigger018_Hit()
	RampSauverTimer.Enabled = True
End Sub

Sub Trigger019_Hit()
	If RampSauverTimer.Enabled = True Then 
	AddScore 4500
	Playsound "Vo_WohaHaer"
	pupDMDDisplay "Splash","NICE","",3,0,10
	FlashForMs Flasher014, 1950, 150, 0
	If GrueState <= 1 Then MoveGrue
	RampSauverTimer.Enabled = False
	End If
End Sub
Sub Trigger020_Hit()
	AddScore 250
	DOF 481, DOFPulse
	FlashForMs Flasher047, 1000, 30, 1
	FlashForMs Flasher046, 1000, 30, 1
	FlashForMs Flasher045, 1000, 30, 1
	FlashForMs Flasher044, 1000, 30, 1
End Sub

Sub RampSauverTimer_Timer()
	RampSauverTimer.Enabled = False
End Sub

Sub Trigger022_Hit()'Kicker Debug cuvette
	If RampSauverTimer.Enabled = False Then
	Trigger022.destroyball
	Kicker001.CreateBall
	kicker001.kick 0, 50
	End If
End Sub

Sub Trigger029_Hit()
	FlashForMs Light_LOOP001, 100, 100, 0
End Sub
Sub Trigger030_Hit()
	FlashForMs Light_LOOP002, 100, 100, 0
End Sub
Sub Trigger031_Hit()
	FlashForMs Light_LOOP003, 100, 100, 0
End Sub
Sub Trigger032_Hit()
	FlashForMs Light_LOOP004, 100, 100, 0
End Sub
Sub Trigger033_Hit()
	FlashForMs Light_LOOP005, 100, 100, 0
End Sub
Sub Trigger034_Hit()
	FlashForMs Light_LOOP006, 100, 100, 0
End Sub

Sub TLeftOutlane_Hit() 's
	AddScore 250
	DOF 300, DOFPulse
	If LeftOutlane.State = 0 Then
	LeftOutlane.State = 2
	Elseif LeftOutlane.State = 2 Then
	DOF 352, DOFPulse
	LeftOutlane.State=1
	CheckLightLANE
	End If
End Sub
Sub TLeftInlane2_Hit() 'k
	AddScore 250
	leftInlaneSpeedLimit
	If LeftInlane2.State = 0 Then
	LeftInlane2.State = 2
	Elseif LeftInlane2.State = 2 Then
	DOF 353, DOFPulse
	LeftInlane2.State=1
	CheckLightLANE
	End If
End Sub
Sub TLeftInlane_Hit() 'e
	AddScore 250
	leftInlaneSpeedLimit
	If LeftInlane.State = 0 Then
	LeftInlane.State = 2
	Elseif LeftInlane.State = 2 Then
	DOF 354, DOFPulse
	LeftInlane.State=1
	CheckLightLANE
	End If
End Sub
Sub TRightInlane_Hit() 't
	AddScore 250
	rightInlaneSpeedLimit
	If RightInlane.State = 0 Then
	RightInlane.State = 2
	Elseif RightInlane.State = 2 Then
	RightInlane.State=1
	DOF 355, DOFPulse
	CheckLightLANE
	End If
End Sub
Sub TRightInlane2_Hit() 'c
	AddScore 250
	rightInlaneSpeedLimit
	If RightInlane2.State = 0 Then
	RightInlane2.State = 2
	Elseif RightInlane2.State = 2 Then
	RightInlane2.State=1
	DOF 356, DOFPulse
	CheckLightLANE
	End If
End Sub
Sub TRightOutlane_Hit() 'h
	DOF 309, DOFPulse
	AddScore 250
	If RightOutlane.State = 0 Then
	RightOutlane.State = 2
	Elseif RightOutlane.State = 2 Then
	RightOutlane.State=1
	DOF 357, DOFPulse
	CheckLightLANE
	End If
End Sub
Sub CheckLightLANE
	If LeftOutlane.State=1 And LeftInlane2.State=1 And LeftInlane.State=1 And RightInlane.State=1 And RightInlane2.State=1 And RightOutlane.State=1 Then
	AddScore 8500
	ExtraBCount(CurrentPlayer) = ExtraBCount(CurrentPlayer) + 1
	PlaySound "Vo_AddLetter"
	pupDMDDisplay "Splash4","ADD^LETTER","",4,0,12
	ResetLightLANE	
	End If
End Sub
Sub ResetLightLANE
	LeftOutlane.State=0 
	LeftInlane2.State=0 
	LeftInlane.State=0 
	RightInlane.State=0 
	RightInlane2.State=0
	RightOutlane.State=0
	CheckExtraBcount
End Sub

Sub CheckExtraBcount
	If ExtraBCount(CurrentPlayer) = 0 Then Light015.state = 0 : Light016.state = 0 : Light017.state = 0 : Light018.state = 0 : Light019.state = 0 : Light020.state = 0 : Light021.state = 0 : Light022.state = 0
	If ExtraBCount(CurrentPlayer) = 1 Then Light015.state = 1 : Light016.state = 0 : Light017.state = 0 : Light018.state = 0 : Light019.state = 0 : Light020.state = 0 : Light021.state = 0 : Light022.state = 0
	If ExtraBCount(CurrentPlayer) = 2 Then Light015.state = 1 : Light016.state = 1 : Light017.state = 0 : Light018.state = 0 : Light019.state = 0 : Light020.state = 0 : Light021.state = 0 : Light022.state = 0
	If ExtraBCount(CurrentPlayer) = 3 Then Light015.state = 1 : Light016.state = 1 : Light017.state = 1 : Light018.state = 0 : Light019.state = 0 : Light020.state = 0 : Light021.state = 0 : Light022.state = 0
	If ExtraBCount(CurrentPlayer) = 4 Then Light015.state = 1 : Light016.state = 1 : Light017.state = 1 : Light018.state = 1 : Light019.state = 0 : Light020.state = 0 : Light021.state = 0 : Light022.state = 0
	If ExtraBCount(CurrentPlayer) = 5 Then Light015.state = 1 : Light016.state = 1 : Light017.state = 1 : Light018.state = 1 : Light019.state = 1 : Light020.state = 0 : Light021.state = 0 : Light022.state = 0
	If ExtraBCount(CurrentPlayer) = 6 Then Light015.state = 1 : Light016.state = 1 : Light017.state = 1 : Light018.state = 1 : Light019.state = 1 : Light020.state = 1 : Light021.state = 0 : Light022.state = 0
	If ExtraBCount(CurrentPlayer) = 7 Then Light015.state = 1 : Light016.state = 1 : Light017.state = 1 : Light018.state = 1 : Light019.state = 1 : Light020.state = 1 : Light021.state = 1 : Light022.state = 0
	If ExtraBCount(CurrentPlayer) = 8 Then Light015.state = 1 : Light016.state = 1 : Light017.state = 1 : Light018.state = 1 : Light019.state = 1 : Light020.state = 1 : Light021.state = 1 : Light022.state = 1 : LiEXTRA.state = 2
	If ExtraBCount(CurrentPlayer) >= 9 And LiEXTRA.state = 2 Then AwardExtraBall : ExtraBCount(CurrentPlayer) = 0 : LiEXTRA.state = 0 
End Sub
'*************
' Multiball
'*************
Sub CheckLightMB
	If MBLock_Sav(CurrentPlayer) = 2 Then
	liLOCK.State = 0
	FlashForMs Flasher041, 1000, 30, 0
	FlashForMs Flasher043, 1000, 30, 0
	FlashForMs Flasher040, 1000, 30, 1 
	FlashForMs Flasher042, 1000, 30, 1 
	Elseif MBLock_Sav(CurrentPlayer) = 4 Then
	liLOCK.State = 0
	FlashForMs Flasher040, 1000, 30, 0 
	FlashForMs Flasher042, 1000, 30, 0 
	FlashForMs Flasher041, 1000, 30, 0
	FlashForMs Flasher043, 1000, 30, 0
	Elseif MBLock_Sav(CurrentPlayer) = 6 Then
	LiLOCK.state=1
	liMB.State =1
	Mode_Multiball_Start
	FlashForMs Flasher041, 1000, 30, 1
	FlashForMs Flasher040, 1000, 30, 1 
	FlashForMs Flasher042, 1000, 30, 1 
	FlashForMs Flasher043, 1000, 30, 1
	PlaySound "Vo_MB"
	pupDMDDisplay "Splash", "MULTIBALL", "", 5, 1, 15
	Elseif MBLock_Sav(CurrentPlayer) = 0 Then
	liLOCK.State = 0
	LiMB.state=0
	FlashForMs Flasher041, 1000, 30, 1
	FlashForMs Flasher040, 1000, 30, 1 
	FlashForMs Flasher042, 1000, 30, 1 
	FlashForMs Flasher043, 1000, 30, 1 
	Elseif MBLock_Sav(CurrentPlayer) = 1 Then
	ShakePorteON
	FlashForMs Flasher041, 1000, 30, 1
	FlashForMs Flasher040, 1000, 30, 1 
	FlashForMs Flasher042, 1000, 30, 1 
	FlashForMs Flasher043, 1000, 30, 1
	Target001.IsDropped = True
	Target002.IsDropped = True
	Target003.IsDropped = True
	Target004.IsDropped = True
	Target005.IsDropped = True
	Wall036.Collidable = 0
	LiLOCK.state=2
	LiMB.state=0
	Elseif MBLock_Sav(CurrentPlayer) = 3 Then
	FlashForMs Flasher041, 1000, 30, 0
	FlashForMs Flasher040, 1000, 30, 1 
	FlashForMs Flasher042, 1000, 30, 1 
	FlashForMs Flasher043, 1000, 30, 0 
	Target001.IsDropped = True
	Target002.IsDropped = True
	Target003.IsDropped = True
	Target004.IsDropped = True
	Target005.IsDropped = True
	Wall036.Collidable = 0
	ShakePorteON
	LiLOCK.state=2
	LiMB.state=0
	Elseif MBLock_Sav(CurrentPlayer) = 5 Then
	FlashForMs Flasher041, 1000, 30, 0
	FlashForMs Flasher040, 1000, 30, 0 
	FlashForMs Flasher042, 1000, 30, 0 
	FlashForMs Flasher043, 1000, 30, 0 
	Target001.IsDropped = True
	Target002.IsDropped = True
	Target003.IsDropped = True
	Target004.IsDropped = True
	Target005.IsDropped = True
	Wall036.Collidable = 0
	ShakePorteON
	LiLOCK.state=0
	LiMB.state=2
	Elseif MBLock_Sav(CurrentPlayer) = 7 Then
	Target001.IsDropped = False
	Target002.IsDropped = False
	Target003.IsDropped = False
	Target004.IsDropped = False
	Target005.IsDropped = False
	Wall036.Collidable = 1
	ShakePorteOFF
	LiLOCK.state=1
	LiMB.state=1
	End If
End Sub

Sub CheckLightTargetMB
	If LetterTargetS(CurrentPlayer) = 1 Then 
	Target001.IsDropped = True
	Li014.state = 1
	Elseif LetterTargetS(CurrentPlayer) = 0 Then
	Target001.IsDropped = False
	Li014.state = 2
	End If
	If LetterTargetK(CurrentPlayer) = 1 Then
	Target002.IsDropped = True
	Li013.state = 1
	Elseif LetterTargetK(CurrentPlayer) = 0 Then
	Target002.IsDropped = False
	Li013.state = 2
	End If
	If LetterTargetA(CurrentPlayer) = 1 Then
	Target003.IsDropped = True
	Li012.state = 1
	Elseif LetterTargetA(CurrentPlayer) = 0 Then
	Target003.IsDropped = False
	Li012.state = 2
	End If
	If LetterTargetT(CurrentPlayer) = 1 Then
	Target004.IsDropped = True
	Li011.state = 1
	Elseif LetterTargetT(CurrentPlayer) = 0 Then
	Target004.IsDropped = False
	Li011.state = 2
	End If
	If LetterTargetE(CurrentPlayer) = 1 Then
	Target005.IsDropped = True
	Li010.state = 1
	Elseif LetterTargetE(CurrentPlayer) = 0 Then
	Target005.IsDropped = False
	Li010.state = 2
	End If
	If LetterTargetS(CurrentPlayer) = 1 And LetterTargetK(CurrentPlayer) = 1 And LetterTargetA(CurrentPlayer) = 1 And LetterTargetT(CurrentPlayer) = 1 And LetterTargetE(CurrentPlayer) = 1 Then
	ShakePorteON
	Li010.state = 1
	Li011.state = 1
	Li012.state = 1
	Li013.state = 1
	Li014.state = 1
	Target001.IsDropped = True
	Target002.IsDropped = True
	Target003.IsDropped = True
	Target004.IsDropped = True
	Target005.IsDropped = True
	Else
	ShakePorteOFF
	End If
End Sub

Sub Mode_Multiball_Start
	DOF 482, DOFOn
	MBLock_Sav(CurrentPlayer) = 7
	ChangeBall(3) 'Rouge
	FlInitBumper 1, "red"
	FlInitBumper 2, "red"
	FlInitBumper 3, "red"
	ChangeLAMPH orange 'MB
	ChangeLAMPB red 'MB
	AddMBmission2
	AddMBmission
'	EnableBallSaver 15
	AddScore 8000
	bMultiBallMode = True
	PlaySong "Mu_5"
	EcranTV2001.ImageA = "TVmu005"
End Sub
Sub Mode_Multiball_End
	DOF 482, DOFOff
	MBLock_Sav(CurrentPlayer) = 0
	ChangeBall(0)
	FlInitBumper 1, "purple"
	FlInitBumper 2, "purple"
	FlInitBumper 3, "purple"
	ChangeLAMPH yellow 'std
	ChangeLAMPB white 'std
	ResetTargetMB
	LiLOCK.state=0
	LiMB.state=0
	bMultiBallMode = False
	PlaySong "Mu_1"
	EcranTV2001.ImageA = "TVmu001"
End Sub

Sub AddMBmission
'	If BallsOnPlayfield <= 2 Then EnableBallSaver 15
	PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
	KickerAuto.CreateBall
	KickerAuto.Kick 10, 41
	PlaySoundAt "fx_plunger", plunger
	DOF 132, DOFPulse
	BallsOnPlayfield = BallsOnPlayfield +1
	LiSkillShot.state = 0
	ResetSkillShotTimer.Enabled = False
End Sub
Sub AddMBmission2 'passage Left
	Kicker003.CreateSizedBallWithMass BallSize / 2, BallMass / 2
	Kicker003.TimerInterval = 4000
	Kicker003.TimerEnabled = True
	BallsOnPlayfield = BallsOnPlayfield +1
End Sub
'*************
' Save CurrentPlayer
'*************
Sub ResetEvents
	StopTournPAL
	StartModeTaxi
	GrueState = 0 
	CheckGrue
	ShakeGrueOFF
	ShakeGrue2OFF
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to2"
	MBLock_Sav(CurrentPlayer) = 0
	ExtraBCount(CurrentPlayer) = 0
	ResetLightLANE
	ResetTargetMB
	ResetSpecial
	ResetTargetSpecial
	TargetTVCount = 0
	BumperModeActive = False
	SpinnerModeActive = False
	BumperCount = 0
	SpinnerCount = 0
	RSlingCount = 0 
	LSlingCount = 0
	FlasherSlingRendTimer.Enabled = True
	Primitive056.visible = 1 
	Primitive050.visible = 0
	RampLCount(CurrentPlayer) = 0
	RampLGarageCount(CurrentPlayer) = 0
	RampMBumperACount(CurrentPlayer) = 0
	RampMBumperBCount(CurrentPlayer) = 0
	RampLoopCount(CurrentPlayer) = 0
	RampMCount(CurrentPlayer) = 0
	RampRCount(CurrentPlayer) = 0
	RampRMBCount(CurrentPlayer) = 0
	ChangeBall(0)
	SkillShotState = 0
	Bonus001 = 0 'Slides
	Bonus002 = 0 'GRinds
	Bonus003 = 0 'Grabs
	Bonus004 = 0 'RampTricks
	Bonus005 = 0 'FlipTricks
	Bonus006 = 0 'HotTricks
	Bonus001active = True
	Bonus002active = True
	Bonus003active = True
	Bonus004active = True
	Bonus005active = True
	Bonus006active = True
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
	Flasher041.visible = 1
	Flasher040.visible = 1 
	Flasher042.visible = 1 
	Flasher043.visible = 1
	If WreckingBallActive = True Then
	WreckingBallActive = False
	ChaineOFF
	GrueState = 0 
	CheckGrue
	Trigger024.enabled = True
	Trigger023.enabled = True
	Primitive31.collidable = False
	End if
End Sub

Sub CheckEvents
	CheckLightRamp
	StopTournPAL
	StartModeTaxi
	RSlingCount = 0 
	LSlingCount = 0
	TargetTVCount = 0
	SkillShotState = 0
'	CheckLightMB
'	CheckLightTargetMB
	Bonus001 = 0 'Slides
	Bonus002 = 0 'GRinds
	Bonus003 = 0 'Grabs
	Bonus004 = 0 'RampTricks
	Bonus005 = 0 'FlipTricks
	Bonus006 = 0 'HotTricks
	Bonus001active = True
	Bonus002active = True
	Bonus003active = True
	Bonus004active = True
	Bonus005active = True
	Bonus006active = True
	FlasherSlingRendTimer.Enabled = True
	Primitive056.visible = 1 
	Primitive050.visible = 0
	If TotalGamesPlayed >= 2 And WreckingBallActive = False Then
'	If PlayersPlayingGame >= 2 And WreckingBallActive = False Then
	GrueState = 0 
	CheckGrue
	BumperModeActive = False
	SpinnerModeActive = False
	BumperCount = 0
	SpinnerCount = 0
	CheckLightMB
	CheckLightTargetMB
	CheckSpecialAdvance
	CheckLightSpecial
	End If
	If TotalGamesPlayed >= 2 And WreckingBallActive = True Then
'	If PlayersPlayingGame >= 2 And WreckingBallActive = True Then
	CheckLightMB
	CheckLightTargetMB
	CheckSpecialAdvance
	CheckLightSpecial
	End If
End Sub
'*****************
'Flasher Crime
'*****************
Dim CrimeApos, FramesCrimeA, CrimeA2pos, FramesCrimeA2, CrimeBpos, FramesCrimeB
FramesCrimeA = Array("Crime3a001", "Crime3a002", "Crime3a003", "Crime3a004", "Crime3a005", "Crime3a006" )
FramesCrimeA2 = Array("Crime3a001", "Crime3a002", "Crime3a003", "Crime3a004", "Crime3a005", "Crime3a006" )
FramesCrimeB = Array("Crime3b001", "Crime3b002", "Crime3b003", "Crime3b004", "Crime3b005", "Crime3b006" )
Sub StartCrimeA
	If StopCrimeATimer.Enabled = False Then
	Flasher011.visible = 1
	CrimeApos = 0
    CrimeATimer.Enabled = 1
	StopCrimeATimer.Enabled = 1
	End If
End Sub
Sub CrimeATimer_Timer
    Flasher011.ImageA = FramesCrimeA (CrimeApos)
    CrimeApos = (CrimeApos + 1) MOD 6
End Sub
Sub StopCrimeA
	Flasher011.visible = 0
	CrimeATimer.Enabled = 0
End Sub
Sub StopCrimeATimer_Timer
	StopCrimeA
Me.Enabled = 0
End Sub

Sub StartCrimeB
	If StopCrimeBTimer.Enabled = False Then
	Flasher012.visible = 1
	CrimeBpos = 0
    CrimeBTimer.Enabled = 1
	StopCrimeBTimer.Enabled = 1
	End If
End Sub
Sub CrimeBTimer_Timer
    Flasher012.ImageA = FramesCrimeB (CrimeBpos)
    CrimeBpos = (CrimeBpos + 1) MOD 6
End Sub
Sub StopCrimeB
	Flasher012.visible = 0
	CrimeBTimer.Enabled = 0
End Sub
Sub StopCrimeBTimer_Timer
	StopCrimeB
Me.Enabled = 0
End Sub

Sub StartCrimeA2
	If StopCrimeA2Timer.Enabled = False Then
	Flasher013.visible = 1
	CrimeA2pos = 0
    CrimeA2Timer.Enabled = 1
	StopCrimeA2Timer.Enabled = 1
	End If
End Sub
Sub CrimeA2Timer_Timer
    Flasher013.ImageA = FramesCrimeA2 (CrimeA2pos)
    CrimeA2pos = (CrimeA2pos + 1) MOD 6
End Sub
Sub StopCrimeA2
	Flasher013.visible = 0
	CrimeA2Timer.Enabled = 0
End Sub
Sub StopCrimeA2Timer_Timer
	StopCrimeA2
Me.Enabled = 0
End Sub

Sub CheckCrime
	If CrimeCount >= 3 Then
	AddScore 35000
	Flasher010.visible = 0
	ModeCrimeActive = False
	ExtraBCount(CurrentPlayer) = ExtraBCount(CurrentPlayer) + 1
	CheckExtraBcount
	pupDMDDisplay "Splash4","CLEARED CRIME^ADD LETTER","",4,0,15
	PlaySound "Vo_AddLetter"
	End If
End Sub
'*****************
'Flasher TV
'*****************
Dim FondTVPos, FramesFondTV
FramesFondTV = Array("TV_001", "TV_002", "TV_003", "TV_004", "TV_005", "TV_006", "TV_007", "TV_008", "TV_009", "TV_010", "TV_011", "TV_012", "TV_013", "TV_014", "TV_015", "TV_016", "TV_017", "TV_018", "TV_019", "TV_020", "TV_021", "TV_022", "TV_023", "TV_024", "TV_025", "TV_026", "TV_027", "TV_028", "TV_029", "TV_030", _
"TV_031", "TV_032", "TV_033","TV_034", "TV_035", "TV_036", "TV_037", "TV_038", "TV_039", "TV_040", "TV_041", "TV_042", "TV_043", "TV_044", "TV_045", "TV_046", "TV_047", "TV_048", "TV_049", "TV_050", "TV_051", "TV_052", "TV_053", "TV_054", "TV_055", "TV_056", "TV_057", "TV_058", "TV_059", "TV_060", _
"TV_061", "TV_062", "TV_063","TV_064", "TV_065", "TV_066")

Sub StartFondTV
	If StopFondTVTimer.Enabled = False Then
	FlasherTV.visible = 0
	FlasherTV2.visible = 1
	FondTVPos = 0
    FondTVTimer.Enabled = 1
	StopFondTVTimer.Enabled = 1
	End If
End Sub
Sub FondTVTimer_Timer
    FlasherTV2.ImageA = FramesFondTV (FondTVPos)
    FondTVPos = (FondTVPos + 1) MOD 66
End Sub
Sub StopFondTV
	FlasherTV.visible = 1
	FlasherTV2.visible = 0
	FondTVTimer.Enabled = 0
End Sub
Sub StopFondTVTimer_Timer
	StopFondTV
Me.Enabled = 0
End Sub
'****************************************
'StartWheelSpinner PAL BACKGROUND
'****************************************
Sub WheelImageSpin2_Timer '
	Flasher003.rotz = Flasher003.rotz - 5
	Flasher004.rotz = Flasher004.rotz - 5
	Flasher005.rotz = Flasher005.rotz + 5
End Sub
Sub spinning2_timer
	WheelImageSpin2.Enabled=False
	Flasher003.rotz = Flasher003.rotz + 5
	Flasher004.rotz = Flasher004.rotz + 5
	Flasher005.rotz = Flasher005.rotz - 5
End Sub
dim spinner2
Set spinner2 = New cvpmTurntable
With spinner2
	.InitTurntable spindisc_wheel2, 100
	.SpinDown = 20
	.CreateEvents "spinner2"
End With
spinner2.MotorOn = false

Sub StartTournPAL
	spinner2.MotorOn = True
	spinning2.enabled = True
	FumeeOFF
End Sub
Sub StopTournPAL
	spinner2.MotorOn = False
	spinning2.enabled = False
	FumeeON
End Sub
'****************************************
'StartWheelSpinner EYES
'*********************
dim spinner3
Set spinner3 = New cvpmTurntable
With spinner3
	.InitTurntable spindisc_wheel3, -10
	.SpinDown = 5
	.CreateEvents "spinner3"
End With
spinner3.MotorOn = false

Sub StartTournEYES
	spinner3.MotorOn = True
	spinning3.enabled = True
	Ramp050.Collidable = 1
	Wall043.Collidable = 1
	spinningStopTimer.enabled = True
End Sub
Sub StopTournEYES
	spinner3.MotorOn = False
	spinning3.enabled = False
	spinningStopTimer.enabled = False
	Wall043.Collidable = 0
'	Trigger027.enabled = True
End Sub
Sub spinningStopTimer_Timer
	Ramp050.Collidable = 0
	StopTournEYES
End Sub

Sub Trigger027_Hit()
	StartTournEYES
	Trigger027.enabled = False
End Sub
'***********
'  fumee
'***********
Dim i1, i2, i3, i4
i1 = 0
i2 = 4
i3 = 4
i4 = 4
Sub FumeeON
	fumeeONtimer.enabled = True
	fumeeOFFtimer.enabled = False
End Sub
Sub FumeeOFF
	fumeeONtimer.enabled = False
	fumeeOFFtimer.enabled = True
End Sub
Sub fumeetimer_timer
    fumee1.imageA = "fumee" &i1
	fumee2.imageA = "fumee" &i1
	fumee3.imageA = "fumee" &i1
    i1 = (i1 + 1) MOD 65
    i2 = (i2 + 1) MOD 8    
    i4 = (i4 + 1) MOD 8
End Sub
Sub fumeeONtimer_timer
	fumeetimer.enabled = True
    fumee1.visible = True
	fumee2.visible = True
	fumee3.visible = True
End Sub
Sub fumeeOFFtimer_timer
	fumeetimer.enabled = False
    fumee1.visible = False
	fumee2.visible = False
	fumee3.visible = False
End Sub
'***********
'  Eau
'***********
Dim EauPos, FramesEau
FramesEau = Array("Eau_000", "Eau_001", "Eau_002", "Eau_003", "Eau_004", "Eau_005", "Eau_006") 
EauPos = 0
Sub Eautimer_timer
    Eau1.ImageA = FramesEau (EauPos)
    EauPos = (EauPos + 1) MOD 7
End Sub
'****************************************
'StartWheelSpinner SPECIAL MODE
'****************************************
Sub WheelImageSpin1_Timer '
	FlasherSpecial001.roty = FlasherSpecial001.roty - 5
	FlasherSpecial002.roty = FlasherSpecial002.roty - 5
	FlasherSpecial003.roty = FlasherSpecial003.roty - 5
	FlasherSpecial004.roty = FlasherSpecial004.roty - 5
	FlasherSpecial005.roty = FlasherSpecial005.roty - 5
	FlasherSpecial006.roty = FlasherSpecial006.roty - 5
	FlasherSpecial007.roty = FlasherSpecial007.roty - 5
End Sub
Sub spinning1_timer
	WheelImageSpin1.Enabled=False
	FlasherSpecial001.roty = FlasherSpecial001.roty + 5
	FlasherSpecial002.roty = FlasherSpecial002.roty + 5
	FlasherSpecial003.roty = FlasherSpecial003.roty + 5
	FlasherSpecial004.roty = FlasherSpecial004.roty + 5
	FlasherSpecial005.roty = FlasherSpecial005.roty + 5
	FlasherSpecial006.roty = FlasherSpecial006.roty + 5
	FlasherSpecial007.roty = FlasherSpecial007.roty + 5
End Sub

dim spinner1
Set spinner1 = New cvpmTurntable
With spinner1
	.InitTurntable spindisc_wheel1, 100
	.SpinDown = 20
	.CreateEvents "spinner1"
End With
spinner1.MotorOn = false

Sub StartMissionSPECIAL
	ModeSpecialActive(CurrentPlayer) = True
	pupDMDDisplay "Splash4","COLLECT^ALL LETTERS","",4,0,15
	FlasherSPECIAL001.visible = 1
	FlasherSPECIAL002.visible = 1
	FlasherSPECIAL003.visible = 1
	FlasherSPECIAL004.visible = 1
	FlasherSPECIAL005.visible = 1
	FlasherSPECIAL006.visible = 1
	FlasherSPECIAL007.visible = 1
	spinner1.MotorOn = True
	spinning1.enabled = True
End Sub

Sub StopMissionSPECIAL
	ModeSpecialActive(CurrentPlayer) = False
	AddScore 90000
	SpecialLetterComplet(CurrentPlayer) = SpecialLetterComplet(CurrentPlayer) + 1
	ExtraBCount(CurrentPlayer) = ExtraBCount(CurrentPlayer) + 1
	CheckExtraBcount
	PlaySound "Vo_AddLetter"
	pupDMDDisplay "Splash4","SPECIAL COLLECTED^ADD LETTER + 90K","",4,0,15
	FlasherSPECIAL001.visible = 0
	FlasherSPECIAL002.visible = 0
	FlasherSPECIAL003.visible = 0
	FlasherSPECIAL004.visible = 0
	FlasherSPECIAL005.visible = 0
	FlasherSPECIAL006.visible = 0
	FlasherSPECIAL007.visible = 0
	spinner1.MotorOn = False
	spinning1.enabled = False
	ResetLightSpecial
	ResetTargetSpecial
	LetterSpecialS(CurrentPlayer) = 0
	LetterSpecialP(CurrentPlayer) = 0
	LetterSpecialE(CurrentPlayer) = 0
	LetterSpecialC(CurrentPlayer) = 0
	LetterSpecialI(CurrentPlayer) = 0
	LetterSpecialA(CurrentPlayer) = 0
	LetterSpecialL(CurrentPlayer) = 0
End Sub

Sub ResetSpecial
	ModeSpecialActive(CurrentPlayer) = False 
	SpecialLetterComplet(CurrentPlayer) = 0
	FlasherSpecial001.visible = 0
	FlasherSpecial002.visible = 0
	FlasherSpecial003.visible = 0
	FlasherSpecial004.visible = 0
	FlasherSpecial005.visible = 0
	FlasherSpecial006.visible = 0
	FlasherSpecial007.visible = 0
	spinner1.MotorOn = False
	spinning1.enabled = False
	LetterSpecialS(CurrentPlayer) = 0
	LetterSpecialP(CurrentPlayer) = 0
	LetterSpecialE(CurrentPlayer) = 0
	LetterSpecialC(CurrentPlayer) = 0
	LetterSpecialI(CurrentPlayer) = 0
	LetterSpecialA(CurrentPlayer) = 0
	LetterSpecialL(CurrentPlayer) = 0
	ResetLightSpecial
End Sub

Sub CheckSpecialAward
	If (LetterSpecialS(CurrentPlayer) + LetterSpecialP(CurrentPlayer) +	LetterSpecialE(CurrentPlayer) + LetterSpecialC(CurrentPlayer) +	LetterSpecialI(CurrentPlayer) +	LetterSpecialA(CurrentPlayer) +	LetterSpecialL(CurrentPlayer)) = 7 Then
	StopMissionSPECIAL
	DOF 402, DOFPulse
	End If
End Sub

Sub CheckSpecialAdvance
	If ModeSpecialActive(CurrentPlayer) = True Then
		li002.state = 0
		li003.state = 0
		li004.state = 0
		li005.state = 0
		li006.state = 0
		li007.state = 0
		li008.state = 0
		li009.state = 0
		spinner1.MotorOn = True
		spinning1.enabled = True
		If LetterSpecialS(CurrentPlayer) = 0 Then
			FlasherSpecial001.visible = 1
			Else FlasherSpecial001.visible = 0
		End If
		If LetterSpecialP(CurrentPlayer) = 0 Then 
			FlasherSpecial002.visible = 1
			Else FlasherSpecial002.visible = 0
		End If
		If LetterSpecialE(CurrentPlayer) = 0 Then 
			FlasherSpecial003.visible = 1
			Else FlasherSpecial003.visible = 0
		End If
		If LetterSpecialC(CurrentPlayer) = 0 Then 
			FlasherSpecial004.visible = 1
			Else FlasherSpecial004.visible = 0
		End If
		If LetterSpecialI(CurrentPlayer) = 0 Then 
			FlasherSpecial005.visible = 1
			Else FlasherSpecial005.visible = 0
		End If
		If LetterSpecialA(CurrentPlayer) = 0 Then 
			FlasherSpecial006.visible = 1
			Else FlasherSpecial006.visible = 0
		End If
		If LetterSpecialL(CurrentPlayer) = 0 Then 
			FlasherSpecial007.visible = 1
			Else FlasherSpecial007.visible = 0
		End If
	Elseif ModeSpecialActive(CurrentPlayer) = False Then
		FlasherSpecial001.visible = 0
		FlasherSpecial002.visible = 0
		FlasherSpecial003.visible = 0
		FlasherSpecial004.visible = 0
		FlasherSpecial005.visible = 0
		FlasherSpecial006.visible = 0
		FlasherSpecial007.visible = 0
		spinner1.MotorOn = False
		spinning1.enabled = False
	End If
End Sub
'******************
'******************
' Captive Ball Subs
'******************
Sub CapTrigger1_Hit : cbRight.TrigHit ActiveBall : End Sub
Sub CapTrigger1_UnHit : cbRight.TrigHit 0 : End Sub
Sub CapWall1_Hit : cbRight.BallHit ActiveBall : End Sub
Sub CapKicker1a_Hit : cbRight.BallReturn Me : End Sub

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

'***********
' Moove Porte
'***********
Sub PorteOFFTimer_timer()
PorteL.TransX = PorteL.TransX + 1
PorteR.TransX = PorteR.TransX - 1
if PorteL.TransX >= 50 then PorteOFFTimer.enabled = 0
End Sub
Sub PorteONTimer_timer()
PorteL.TransX = PorteL.TransX - 1
PorteR.TransX = PorteR.TransX + 1
if PorteL.TransX <= 1 then PorteONTimer.enabled = 0
End Sub

Sub ShakePorteON 'Close
	If PorteONTimer.enabled = 0 Then
	PorteOFFTimer.enabled = 1
	Wall036.collidable = 0
	Ramp004.collidable = 1
	End If
	if PorteL.TransZ <= 49 then PlaySound "fx_gateSlide"
End Sub
Sub ShakePorteOFF 'Open
	If PorteOFFTimer.enabled = 0 Then
	PorteONTimer.enabled = 1
	Wall036.collidable = 1
	Ramp004.collidable = 0
	if PorteL.TransZ >= 2 then PlaySound "fx_gateSlide"
	End If
End Sub

'*********
'SelectMusic
'*********
Dim PlungerKeyActive

Sub AwardMusic
	If UnlockAllMusic = 0 Then UnlockMusicCount = UnlockMusicCount + 1 : Playsound "fx_AddMusic"
End Sub

Sub SelectMusicEndTimer_Timer()
	LiSK000.State = 0
	LiSK001.State = 0
	LiSK002.State = 0
	LiSK003.State = 0
	LiSK004.State = 0
	LiSK005.State = 0
	LiSK006.State = 0
	LiSK007.State = 0
	LiSK008.State = 0
	LiSK009.State = 0
	LiSK010.State = 0
	LiSK011.State = 0
	LiSK012.State = 0
	LiSK013.State = 0
	EcranTV3.visible = 0
	PuPlayer.LabelSet pBackglass,"SelectMusicimg","",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	ClearMessageTimer.Enabled = True
	SelectMusicEndTimer.Enabled = False
	PuPEvent 101
	PuPEvent 102
End Sub

Dim bSelectMusic, SelectMusicnr
bSelectMusic = ""
SelectMusicnr = INT(RND * 3)

Sub PlayRandomSelectMusic
    SelectMusicnr = INT(RND * 3)
    PLaySelectedSelectMusic
End Sub

Sub PLaySelectedSelectMusic
	SelectMusicActive = False
    Select Case SelectMusicnr
        Case 0
			LiSK001.State = 1 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PlaySong "Mu_1"
			SelectMusicEndTimer.Enabled = True	
			ClearMessageTimer.Enabled = True
        Case 1
			LiSK000.State = 0 : LiSK001.State = 1 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PlaySong "Mu_2"
			SelectMusicEndTimer.Enabled = True
			ClearMessageTimer.Enabled = True
        Case 2
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 1 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PlaySong "Mu_3"
			SelectMusicEndTimer.Enabled = True 
			ClearMessageTimer.Enabled = True
        Case 3
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 1 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PlaySong "Mu_4"
			SelectMusicEndTimer.Enabled = True	
			ClearMessageTimer.Enabled = True
        Case 4
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 1 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PlaySong "Mu_5"
			SelectMusicEndTimer.Enabled = True
			ClearMessageTimer.Enabled = True
		Case 5
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 1 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PlaySong "Mu_6"
			SelectMusicEndTimer.Enabled = True
			ClearMessageTimer.Enabled = True
		Case 6
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 1 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PlaySong "Mu_7"
			SelectMusicEndTimer.Enabled = True
			ClearMessageTimer.Enabled = True
		Case 7
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 1 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PlaySong "Mu_8"
			SelectMusicEndTimer.Enabled = True
			ClearMessageTimer.Enabled = True
		Case 8
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 1 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PlaySong "Mu_9"
			SelectMusicEndTimer.Enabled = True
			ClearMessageTimer.Enabled = True
		Case 9
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 1  : LiSK010.State = 0 : LiSK011.State = 0 
			PlaySong "Mu_10"
			SelectMusicEndTimer.Enabled = True
			ClearMessageTimer.Enabled = True
		Case 10
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0  : LiSK010.State = 1 : LiSK011.State = 0 
			PlaySong "Mu_11"
			SelectMusicEndTimer.Enabled = True
			ClearMessageTimer.Enabled = True
		Case 11
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0  : LiSK010.State = 0 : LiSK011.State = 1 
			PlaySong "Mu_12"
			SelectMusicEndTimer.Enabled = True
			ClearMessageTimer.Enabled = True
		Case 12
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0  : LiSK010.State = 0 : LiSK011.State = 0 : LiSK012.State = 1 : LiSK013.State = 0 
			PlaySong "Mu_13"
			SelectMusicEndTimer.Enabled = True
			ClearMessageTimer.Enabled = True
		Case 13
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0  : LiSK010.State = 0 : LiSK011.State = 0 : LiSK012.State = 0 : LiSK013.State = 1 
			PlaySong "Mu_14"
			SelectMusicEndTimer.Enabled = True
			ClearMessageTimer.Enabled = True
    End Select
End Sub

Sub UpdateBGSelectMusic() 'Updates the BG with the chosen
	If UnlockAllMusic = 1 Then UnlockMusicCount = 14
    PuPlayer.LabelShowPage pBackglass,1,0,""
	PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU1.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
    Select Case SelectMusicnr
        Case 0	'	
			LiSK000.State = 2 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU1.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV2001.ImageA = "TVmu001"
			EcranTV3.visible = 0
        Case 1  '	
			LiSK000.State = 0 : LiSK001.State = 2 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU2.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV2001.ImageA = "TVmu002"
			EcranTV3.visible = 0
        Case 2  '	
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 2 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU3.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV2001.ImageA = "TVmu003"
			EcranTV3.visible = 0
        Case 3  '	
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 2 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU4.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV2001.ImageA = "TVmu004"
			EcranTV3.visible = 0
        Case 4  '	
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 2 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			EcranTV2001.ImageA = "TVmu005"
			If UnlockMusicCount >= 5 Then	
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU5.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV3.visible = 0
			Else
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MULock.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Playsound "fx_target"
			PlungerKeyActive = False
			EcranTV3.visible = 1
			End If 
		Case 5   '	
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 2 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			EcranTV2001.ImageA = "TVmu006"
			If UnlockMusicCount >= 6 Then
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU6.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV3.visible = 0
			Else
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MULock.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Playsound "fx_target"
			PlungerKeyActive = False
			EcranTV3.visible = 1
			End If 
		Case 6   '	
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 2 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			EcranTV2001.ImageA = "TVmu007"
			If UnlockMusicCount >= 7 Then
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU7.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV3.visible = 0
			Else
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MULock.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Playsound "fx_target"
			PlungerKeyActive = False
			EcranTV3.visible = 1
			End If 
		Case 7   '	
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 2 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			EcranTV2001.ImageA = "TVmu008"
			If UnlockMusicCount >= 8 Then
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU8.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV3.visible = 0
			Else
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MULock.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Playsound "fx_target"
			PlungerKeyActive = False
			EcranTV3.visible = 1
			End If 
		Case 8   '	
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 2 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 
			EcranTV2001.ImageA = "TVmu009"
			If UnlockMusicCount >= 9 Then
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU9.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV3.visible = 0
			Else
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MULock.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Playsound "fx_target"
			PlungerKeyActive = False
			EcranTV3.visible = 1
			End If 
		Case 9   '  
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 2 : LiSK010.State = 0 : LiSK011.State = 0 
			EcranTV2001.ImageA = "TVmu010"
			If UnlockMusicCount >= 10 Then
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU10.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV3.visible = 0
			Else
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MULock.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Playsound "fx_target"
			PlungerKeyActive = False
			EcranTV3.visible = 1
			End If 
		Case 10   '  
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 2 : LiSK011.State = 0 
			EcranTV2001.ImageA = "TVmu011"
			If UnlockMusicCount >= 11 Then
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU11.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV3.visible = 0
			Else
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MULock.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Playsound "fx_target"
			PlungerKeyActive = False
			EcranTV3.visible = 1
			End If 
		Case 11   '  
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 2 
			EcranTV2001.ImageA = "TVmu012"
			If UnlockMusicCount >= 12 Then
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU12.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV3.visible = 0
			Else
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MULock.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Playsound "fx_target"
			PlungerKeyActive = False
			EcranTV3.visible = 1
			End If 
		Case 12   '  
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 : LiSK012.State = 2 : LiSK013.State = 0
			EcranTV2001.ImageA = "TVmu013"
			If UnlockMusicCount >= 13 Then
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU13.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV3.visible = 0
			Else
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MULock.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Playsound "fx_target"
			PlungerKeyActive = False
			EcranTV3.visible = 1
			End If 
		Case 13   '  
			LiSK000.State = 0 : LiSK001.State = 0 : LiSK002.State = 0 : LiSK003.State = 0 : LiSK004.State = 0 : LiSK005.State = 0 : LiSK006.State = 0 : LiSK007.State = 0 : LiSK008.State = 0 : LiSK009.State = 0 : LiSK010.State = 0 : LiSK011.State = 0 : LiSK012.State = 0 : LiSK013.State = 2
			EcranTV2001.ImageA = "TVmu014"
			If UnlockMusicCount >= 14 Then
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MU14.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			PlungerKeyActive = True
			EcranTV3.visible = 0
			Else
			PuPlayer.LabelSet pBackglass,"SelectMusicimg","PUPAlphas\\MULock.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			Playsound "fx_target"
			PlungerKeyActive = False
			EcranTV3.visible = 1
			End If
    End Select
End Sub

Dim bChangeMusic, ChangeMusicnr
bChangeMusic = ""
ChangeMusicnr = INT(RND * 3)

Sub UpdateChangeMusic() 'Updates the BG with the chosen
	If UnlockAllMusic = 1 Then UnlockMusicCount = 14
    Select Case ChangeMusicnr
        Case 0	'
			PlaySong "Mu_1"
			EcranTV2001.ImageA = "TVmu001"
        Case 1  '	
			PlaySong "Mu_2"
			EcranTV2001.ImageA = "TVmu002"
        Case 2  '
			PlaySong "Mu_3"
			EcranTV2001.ImageA = "TVmu003"
        Case 3  '
			PlaySong "Mu_4"
			EcranTV2001.ImageA = "TVmu004"
        Case 4  '	
			PlaySong "Mu_5"
			If UnlockMusicCount >= 5 Then	
			EcranTV2001.ImageA = "TVmu005"
			End If
		Case 5   '	
			PlaySong "Mu_6"
			If UnlockMusicCount >= 6 Then	
			EcranTV2001.ImageA = "TVmu006"
			End If
		Case 6   '	
			PlaySong "Mu_7"
			If UnlockMusicCount >= 7 Then	
			EcranTV2001.ImageA = "TVmu007"
			End If
		Case 7   '	
			PlaySong "Mu_8"
			If UnlockMusicCount >= 8 Then	
			EcranTV2001.ImageA = "TVmu008"
			End If
		Case 8   '	
			PlaySong "Mu_9"
			If UnlockMusicCount >= 9 Then	
			EcranTV2001.ImageA = "TVmu009"
			End If
		Case 9   ' 
			PlaySong "Mu_10"
			If UnlockMusicCount >= 10 Then	
			EcranTV2001.ImageA = "TVmu010"
			End If
		Case 10   '
			PlaySong "Mu_11"
			If UnlockMusicCount >= 11 Then	
			EcranTV2001.ImageA = "TVmu011"
			End If
		Case 11   '  
			PlaySong "Mu_12"
			If UnlockMusicCount >= 12 Then	
			EcranTV2001.ImageA = "TVmu012"
			End If
		Case 12   ' 
			PlaySong "Mu_13"
			If UnlockMusicCount >= 13 Then
			EcranTV2001.ImageA = "TVmu013"
			End If
		Case 13   ' 
			PlaySong "Mu_14"
			If UnlockMusicCount >= 14 Then
			EcranTV2001.ImageA = "TVmu014"
			End If
    End Select
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

Dim pDMD
	if Renderingmode = 2 Then
		pDMD = 5
	Else
		pDMD = 1
	End If

	Const pTopper=0
'	Const pDMD=1
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
	PuPlayer.LabelNew pBackglass,"Play1score",dmdscr,		4,2697513  	,0,0,1,3,40,1,0 		'Score P1
	PuPlayer.LabelNew pBackglass,"Play2score",dmdscr,		4,2697513  	,0,0,1,3,60,1,0
	PuPlayer.LabelNew pBackglass,"Play3score",dmdscr,		4,2697513  	,0,0,1,80,40,1,0
	PuPlayer.LabelNew pBackglass,"Play4score",dmdscr,		4,2697513  	,0,0,1,80,60,1,0
	PuPlayer.LabelNew pBackglass,"ruletitle",numberfont,		3,15066597  ,0,1,1,50,12,1,0
	PuPlayer.LabelNew pBackglass,"rulecopy1",dmdscr,			2,15066597 	,0,1,1,50,15,1,1
	PuPlayer.LabelNew pBackglass,"rulecopy2",dmdscr,			2,15066597 	,0,1,1,50,15,1,1
	PuPlayer.LabelNew pBackglass,"rulecopy3",dmdscr,			2,15066597 	,0,1,1,50,15,1,1
	PuPlayer.LabelNew pBackglass,"rulecopy4",dmdscr,			2,15066597 	,0,1,1,50,15,1,1


	
	'onverlay
	PuPlayer.LabelNew pBackglass,"SelectMusicimg",zoomfont,		6,16777215 	,0,1,1, 0,0,1,0
	PuPlayer.LabelNew pBackglass,"titleimg",zoomfont,			6,16777215 	,0,1,1, 0,0,1,0
	PuPlayer.LabelNew pBackglass,"titleimg2",zoomfont,			6,16777215 	,0,1,1, 0,0,1,0
	PuPlayer.LabelNew pBackglass,"titleimg3",zoomfont,			6,16777215 	,0,1,1, 0,0,1,0
	PuPlayer.LabelNew pBackglass,"titleimg4",zoomfont,			6,16777215 	,0,1,1, 0,0,1,0
	'PuPlayer.LabelSet pBackglass,"titleimg","DMDImages\\popframe.png",1,"{'mt':2,'width': 0, 'height': 0, 'yalign': 0}"
	PuPlayer.LabelNew pBackglass,"titlebg",zoombgfont,			9,0  		,0,1,1,50,50,1,0
	PuPlayer.LabelNew pBackglass,"title",zoomfont,				9,16777215 	,0,1,1,50,50,1,0
	PuPlayer.LabelNew pBackglass,"titlebg2",zoombgfont,			6,0  		,0,1,1,50,50,1,0
	PuPlayer.LabelNew pBackglass,"title2",zoomfont,				6,16777215 	,0,1,1,50,50,1,0
	PuPlayer.LabelNew pBackglass,"mgscore",numberfont,			9,15066597	,0,1,1,50,47,1,0

	'Bonus
	PuPlayer.LabelNew pBackglass,"BonusTitre",zoomfont,			12,16777215  ,0,1,1,0,0,1,0
	PuPlayer.LabelNew pBackglass,"Bonus1",dmdscr,			    6,16777215  ,0,1,1,60,31,1,0 ' 
	PuPlayer.LabelNew pBackglass,"Bonus2",dmdscr,			    6,16777215  ,0,1,1,60,38,1,0 ' 
	PuPlayer.LabelNew pBackglass,"Bonus3",dmdscr,			    6,16777215  ,0,1,1,60,45,1,0 ' 
	PuPlayer.LabelNew pBackglass,"Bonus4",dmdscr,			    6,16777215  ,0,1,1,60,52,1,0 ' 
	PuPlayer.LabelNew pBackglass,"Bonus5",dmdscr,			    6,16777215  ,0,1,1,60,59,1,0 ' 
	PuPlayer.LabelNew pBackglass,"Bonus6",dmdscr,			    6,16777215  ,0,1,1,60,67,1,0 '
	PuPlayer.LabelNew pBackglass,"BonusTotal",dmdscr,			7,16777215  ,0,1,1,60,77,1,0
	PuPlayer.LabelNew pBackglass,"BonusMultip",typefont,		8,16777215  ,0,1,1,50,22,1,0

	'Bonus Attract
	PuPlayer.LabelNew pBackglass,"Bonus000",zoomfont,			12,16777215  ,0,1,1,0,0,1,0
	PuPlayer.LabelNew pBackglass,"Bonus010",zoomfont,			12,16777215  ,0,1,1,0,0,1,0
	PuPlayer.LabelNew pBackglass,"Bonus020",zoomfont,			12,16777215  ,0,1,1,0,0,1,0
	PuPlayer.LabelNew pBackglass,"Bonus030",zoomfont,			12,16777215  ,0,1,1,0,0,1,0
	PuPlayer.LabelNew pBackglass,"Bonus040",zoomfont,			12,16777215  ,0,1,1,0,0,1,0
	PuPlayer.LabelNew pBackglass,"Bonus050",zoomfont,			12,16777215  ,0,1,1,0,0,1,0
	PuPlayer.LabelNew pBackglass,"Bonus060",zoomfont,			12,16777215  ,0,1,1,0,0,1,0
	PuPlayer.LabelNew pBackglass,"BonusMultip2",typefont,		4,16777215  ,0,1,1,50,15,1,0

	'attract
	PuPlayer.LabelNew pBackglass,"Test1",typefont,			    12,16777215  ,0,1,1,15,7,1,0  'Player n°
	PuPlayer.LabelNew pBackglass,"high1name",typefont,			5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"high1score",numberfont,		5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"high2name",typefont,			5,16777215  ,0,1,1,22,38,1,1
	PuPlayer.LabelNew pBackglass,"high2score",numberfont,		5,16777215  ,0,1,1,36,38,1,1
	PuPlayer.LabelNew pBackglass,"high3name",typefont,			5,16777215  ,0,1,1,22,46,1,1
	PuPlayer.LabelNew pBackglass,"high3score",numberfont,		5,16777215  ,0,1,1,36,46,1,1
	PuPlayer.LabelNew pBackglass,"high4name",typefont,			5,16777215  ,0,1,1,22,54,1,1
	PuPlayer.LabelNew pBackglass,"high4score",numberfont,		5,16777215  ,0,1,1,36,54,1,1
	PuPlayer.LabelNew pBackglass,"HighScore",typefont,			12,16777215	,0,0,1,2,7,1,1 'Enter Your Name
	PuPlayer.LabelNew pBackglass,"HighScoreL1",numberfont,		6,16777215	,0,0,1,5,20,1,1 '
	PuPlayer.LabelNew pBackglass,"HighScoreL2",numberfont,		6,16777215	,0,0,1,12,20,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL3",numberfont,		6,16777215	,0,0,1,18,20,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL4",numberfont,		8,16777215	,0,0,1,5,35,1,1
	PuPlayer.LabelNew pBackglass,"CurImage","Arial",			50,391231   ,0,1,1, 0, 0,1,1  'new image type

Sub resetbackglassOFFScore
	PuPlayer.LabelSet pBackglass,"BonusTitre","PUPAlphas\\OverlayBonus.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"Bonus1", FormatNumber((Bonus001*2500),0),0,"{'mt':2,'color':5602052, 'size': 6 }"
	PuPlayer.LabelSet pBackglass,"Bonus2", FormatNumber((Bonus002*4500),0),0,"{'mt':2,'color':25542, 'size': 6 }"
	PuPlayer.LabelSet pBackglass,"Bonus3", FormatNumber((Bonus003*5000),0),0,"{'mt':2,'color':13264128, 'size': 6 }"
	PuPlayer.LabelSet pBackglass,"Bonus4", FormatNumber((Bonus004*3000),0),0,"{'mt':2,'color':8388736, 'size': 6 }"
	PuPlayer.LabelSet pBackglass,"Bonus5", FormatNumber((Bonus005*4000),0),0,"{'mt':2,'color':55660, 'size': 6 }"
	PuPlayer.LabelSet pBackglass,"Bonus6", FormatNumber((Bonus006*1500),0),0,"{'mt':2,'color':5602052, 'size': 6 }"
	PuPlayer.LabelSet pBackglass,"BonusTotal", FormatNumber(TotalBonus,0),0,"{'mt':2,'color':263172, 'size': 6 }"
	PuPlayer.LabelSet pBackglass,"BonusMultip","Bonus x " & FormatNumber(BonusMultiplier(CurrentPlayer),0),0,"{'mt':2,'color':263172, 'size': 7 }"
End Sub

Function BonusScoreTotal()
	BonusScoreTotal = BonusMultiplier(CurrentPlayer) * ((Bonus001 * 2500) + (Bonus002 * 4500) + (Bonus003 * 5000) + (Bonus004 * 3000) + (Bonus005 * 4000) + (Bonus006 * 1500))
End Function 
Sub SeqBonus
	vpmtimer.addtimer 200, "SeqBonus1 '"
	vpmtimer.addtimer 650, "SeqBonus2 '"
	vpmtimer.addtimer 1120, "SeqBonus3 '"
	vpmtimer.addtimer 1570, "SeqBonus4 '"
	vpmtimer.addtimer 2040, "SeqBonus5 '"
	vpmtimer.addtimer 2450, "SeqBonus6 '"
End Sub
Sub SeqBonus1
	PuPlayer.LabelSet pBackglass,"BonusMultip","Bonus x " & FormatNumber(BonusMultiplier(CurrentPlayer),0),1,"{'mt':2,'color':263172, 'size': 6 }"
	PuPlayer.LabelSet pBackglass,"Bonus1", FormatNumber(Bonus001*2500,0),1,"{'mt':2,'color':5602052, 'size': 5 }"
	PlaySound "Bat"
End Sub
Sub SeqBonus2
	PuPlayer.LabelSet pBackglass,"Bonus2", FormatNumber(Bonus002*4500,0),1,"{'mt':2,'color':25542, 'size': 5 }"
End Sub
Sub SeqBonus3
	PuPlayer.LabelSet pBackglass,"Bonus3", FormatNumber(Bonus003*5000,0),1,"{'mt':2,'color':13264128, 'size': 5 }"
End Sub
Sub SeqBonus4
	PuPlayer.LabelSet pBackglass,"Bonus4", FormatNumber(Bonus004*3000,0),1,"{'mt':2,'color':8388736, 'size': 5 }"
End Sub
Sub SeqBonus5
	PuPlayer.LabelSet pBackglass,"Bonus5", FormatNumber(Bonus005*4000,0),1,"{'mt':2,'color':55660, 'size': 5 }"
	PlayerScore(CurrentPlayer) = PlayerScore(CurrentPlayer) + BonusScoreTotal
End Sub
Sub SeqBonus6
	PuPlayer.LabelSet pBackglass,"Bonus6", FormatNumber(Bonus006*1500,0),1,"{'mt':2,'color':3739322, 'size': 5 }"
	PlaySound "Bat2"
End Sub

	'called on table load
Sub resetbackglass
'	PuPlayer.LabelSet pBackglass,"titleimg","",1,"{'mt':2,'color':111111, 'width': 0, 'height': 0, 'yalign': 0}"
	PuPlayer.LabelShowPage pBackglass,1,0,""
End Sub

Sub CheckBGBonusAttract
	PuPlayer.LabelSet pBackglass,"Bonus000","PUPAlphas\\Bonus000.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"BonusMultip2","Bonus x " & FormatNumber(BonusMultiplier(CurrentPlayer),0),1,"{'mt':2,'color':65535, 'size': 5 }"

	If Bonus001 = 0 Then PuPlayer.LabelSet pBackglass,"Bonus010","PUPAlphas\\BONUS010.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus001 = 1 Then PuPlayer.LabelSet pBackglass,"Bonus010","PUPAlphas\\BONUS011.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus001 = 2 Then PuPlayer.LabelSet pBackglass,"Bonus010","PUPAlphas\\BONUS012.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus001 = 3 Then PuPlayer.LabelSet pBackglass,"Bonus010","PUPAlphas\\BONUS013.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus001 = 4 Then PuPlayer.LabelSet pBackglass,"Bonus010","PUPAlphas\\BONUS014.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus001 = 5 Then PuPlayer.LabelSet pBackglass,"Bonus010","PUPAlphas\\BONUS015.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus001 = 6 Then PuPlayer.LabelSet pBackglass,"Bonus010","PUPAlphas\\BONUS016.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"

	If Bonus002 = 0 Then PuPlayer.LabelSet pBackglass,"Bonus020","PUPAlphas\\BONUS020.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus002 = 1 Then PuPlayer.LabelSet pBackglass,"Bonus020","PUPAlphas\\BONUS021.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus002 = 2 Then PuPlayer.LabelSet pBackglass,"Bonus020","PUPAlphas\\BONUS022.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus002 = 3 Then PuPlayer.LabelSet pBackglass,"Bonus020","PUPAlphas\\BONUS023.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus002 = 4 Then PuPlayer.LabelSet pBackglass,"Bonus020","PUPAlphas\\BONUS024.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus002 = 5 Then PuPlayer.LabelSet pBackglass,"Bonus020","PUPAlphas\\BONUS025.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus002 = 6 Then PuPlayer.LabelSet pBackglass,"Bonus020","PUPAlphas\\BONUS026.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"

	If Bonus003 = 0 Then PuPlayer.LabelSet pBackglass,"Bonus030","PUPAlphas\\BONUS030.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus003 = 1 Then PuPlayer.LabelSet pBackglass,"Bonus030","PUPAlphas\\BONUS031.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus003 = 2 Then PuPlayer.LabelSet pBackglass,"Bonus030","PUPAlphas\\BONUS032.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus003 = 3 Then PuPlayer.LabelSet pBackglass,"Bonus030","PUPAlphas\\BONUS033.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus003 = 4 Then PuPlayer.LabelSet pBackglass,"Bonus030","PUPAlphas\\BONUS034.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus003 = 5 Then PuPlayer.LabelSet pBackglass,"Bonus030","PUPAlphas\\BONUS035.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus003 = 6 Then PuPlayer.LabelSet pBackglass,"Bonus030","PUPAlphas\\BONUS036.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"

	If Bonus004 = 0 Then PuPlayer.LabelSet pBackglass,"Bonus040","PUPAlphas\\BONUS040.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus004 = 1 Then PuPlayer.LabelSet pBackglass,"Bonus040","PUPAlphas\\BONUS041.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus004 = 2 Then PuPlayer.LabelSet pBackglass,"Bonus040","PUPAlphas\\BONUS042.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus004 = 3 Then PuPlayer.LabelSet pBackglass,"Bonus040","PUPAlphas\\BONUS043.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus004 = 4 Then PuPlayer.LabelSet pBackglass,"Bonus040","PUPAlphas\\BONUS044.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus004 = 5 Then PuPlayer.LabelSet pBackglass,"Bonus040","PUPAlphas\\BONUS045.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus004 = 6 Then PuPlayer.LabelSet pBackglass,"Bonus040","PUPAlphas\\BONUS046.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus004 = 7 Then PuPlayer.LabelSet pBackglass,"Bonus040","PUPAlphas\\BONUS047.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	
	If Bonus005 = 0 Then PuPlayer.LabelSet pBackglass,"Bonus050","PUPAlphas\\BONUS050.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus005 = 1 Then PuPlayer.LabelSet pBackglass,"Bonus050","PUPAlphas\\BONUS051.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus005 = 2 Then PuPlayer.LabelSet pBackglass,"Bonus050","PUPAlphas\\BONUS052.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus005 = 3 Then PuPlayer.LabelSet pBackglass,"Bonus050","PUPAlphas\\BONUS053.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus005 = 4 Then PuPlayer.LabelSet pBackglass,"Bonus050","PUPAlphas\\BONUS054.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus005 = 5 Then PuPlayer.LabelSet pBackglass,"Bonus050","PUPAlphas\\BONUS055.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus005 = 6 Then PuPlayer.LabelSet pBackglass,"Bonus050","PUPAlphas\\BONUS056.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus005 = 7 Then PuPlayer.LabelSet pBackglass,"Bonus050","PUPAlphas\\BONUS057.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"

	If Bonus006 = 0 Then PuPlayer.LabelSet pBackglass,"Bonus060","PUPAlphas\\BONUS060.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus006 = 1 Then PuPlayer.LabelSet pBackglass,"Bonus060","PUPAlphas\\BONUS061.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus006 = 2 Then PuPlayer.LabelSet pBackglass,"Bonus060","PUPAlphas\\BONUS062.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus006 = 3 Then PuPlayer.LabelSet pBackglass,"Bonus060","PUPAlphas\\BONUS063.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus006 = 4 Then PuPlayer.LabelSet pBackglass,"Bonus060","PUPAlphas\\BONUS064.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus006 = 5 Then PuPlayer.LabelSet pBackglass,"Bonus060","PUPAlphas\\BONUS065.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus006 = 6 Then PuPlayer.LabelSet pBackglass,"Bonus060","PUPAlphas\\BONUS066.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	If Bonus006 = 7 Then PuPlayer.LabelSet pBackglass,"Bonus060","PUPAlphas\\BONUS067.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
End Sub

Sub resetBGBonusAttract
	PuPlayer.LabelSet pBackglass,"BonusMultip2","Bonus x " & FormatNumber(BonusMultiplier(CurrentPlayer),0),0,"{'mt':2,'color':65535, 'size': 4 }"
	PuPlayer.LabelSet pBackglass,"Bonus000","PUPAlphas\\Bonus000.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"Bonus010","PUPAlphas\\Bonus010.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"Bonus020","PUPAlphas\\Bonus020.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"Bonus030","PUPAlphas\\Bonus030.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"Bonus040","PUPAlphas\\Bonus040.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"Bonus050","PUPAlphas\\Bonus050.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"Bonus060","PUPAlphas\\Bonus060.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
End Sub

Sub resetbackglassOFF
	bModePbgActive = False
	PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),0,"{'mt':2,'color':5267966, 'size': 4 }"
	PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),0,"{'mt':2,'color':54741, 'size': 4 }"
	PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),0,"{'mt':2,'color':16711680, 'size': 4 }"
	PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),0,"{'mt':2,'color':3727104, 'size': 4 }"
	PuPlayer.LabelSet pBackglass,"titleimg2","PUPAlphas\\OverlayJ2.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"titleimg3","PUPAlphas\\OverlayJ3.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"titleimg4","PUPAlphas\\OverlayJ4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
End Sub

	Dim prevNumberOfPlayers : prevNumberOfPlayers = 0
	Sub pUpdateScores
		if SelectMusicActive = True Then
			If bModePbgActive = True Then resetbackglassOFF
			prevNumberOfPlayers = 0
			Exit Sub
		End If
		If PlayersPlayingGame = 1 Then
		PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(PlayerScore(1),0),0,"{'mt':2,'color':5267966, 'size': 4 }"
		PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(PlayerScore(2),0),0,"{'mt':2,'color':54741, 'size': 4 }"
		PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(PlayerScore(3),0),0,"{'mt':2,'color':16711680, 'size': 4 }"
		PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(PlayerScore(4),0),0,"{'mt':2,'color':3727104, 'size': 4 }"
		PuPlayer.LabelSet pBackglass,"titleimg2","PUPAlphas\\OverlayJ2.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"titleimg3","PUPAlphas\\OverlayJ3.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"titleimg4","PUPAlphas\\OverlayJ4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		bModePbgActive = False
		resetbackglassOFF
		Elseif PlayersPlayingGame = 2 Then
			bModePbgActive = True
			PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(PlayerScore(1),0),1,"{'mt':2,'color':5267966, 'size': 4 }"
			PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(PlayerScore(2),0),1,"{'mt':2,'color':54741, 'size': 4 }"
			If (prevNumberOfPlayers <> 2) Then
				PuPlayer.LabelSet pBackglass,"titleimg2","PUPAlphas\\OverlayJ2.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titleimg3","PUPAlphas\\OverlayJ3.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titleimg4","PUPAlphas\\OverlayJ4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(PlayerScore(3),0),0,"{'mt':2,'color':16711680, 'size': 4 }" '16744448
				PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(PlayerScore(4),0),0,"{'mt':2,'color':3727104, 'size': 4 }"
			End If
		Elseif PlayersPlayingGame = 3 Then 
			bModePbgActive = True
			PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(PlayerScore(1),0),1,"{'mt':2,'color':5267966, 'size': 4 }"
			PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(PlayerScore(2),0),1,"{'mt':2,'color':54741, 'size': 4 }"
			PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(PlayerScore(3),0),1,"{'mt':2,'color':16711680, 'size': 4 }"
			If (prevNumberOfPlayers <> 3) Then
				PuPlayer.LabelSet pBackglass,"titleimg2","PUPAlphas\\OverlayJ2.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titleimg3","PUPAlphas\\OverlayJ3.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titleimg4","PUPAlphas\\OverlayJ4.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(PlayerScore(4),0),0,"{'mt':2,'color':3727104, 'size': 4 }"
			End if
		Elseif PlayersPlayingGame = 4 Then
			bModePbgActive = True
			PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(PlayerScore(1),0),1,"{'mt':2,'color':5267966, 'size': 4 }"
			PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(PlayerScore(2),0),1,"{'mt':2,'color':54741, 'size': 4 }"
			PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(PlayerScore(3),0),1,"{'mt':2,'color':16711680, 'size': 4 }"
			PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(PlayerScore(4),0),1,"{'mt':2,'color':3727104, 'size': 4 }"
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
				PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),1,"{'mt':2,'color':16744448, 'size': 6 }"
			Elseif CurrentPlayer = 4 Then
				PuPlayer.LabelSet pBackglass,"Test1","PLAYER " & FormatNumber(CurrentPlayer,0),1,"{'mt':2,'color':3727104, 'size': 6 }"
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


	if PuPDMDDriverType=pDMDTypeReal Then 'using RealDMD Mirroring.  **********  128x32 Real Color DMD  
		'Page 1 (default score display)
			 PuPlayer.LabelNew pDMD,"Credits" ,dmdalt,24,16744448   ,1,0,0,6,0,1,0
			 PuPlayer.LabelNew pDMD,"Play1"   ,dmdalt,24,16744448   ,1,0,2,2,0,1,0
			 PuPlayer.LabelNew pDMD,"Ball"    ,dmdalt,24,16744448   ,1,2,2,96,0,1,0 
			 PuPlayer.LabelNew pDMD,"MsgScore",dmdscr,45,33023   ,0,1,0, 0,40,1,0
			 PuPlayer.LabelNew pDMD,"CurScore",dmdscr,60,33023   ,0,1,1, 0,0,1,0


		'Page 2 (default Text Splash 1 Big Line)
			 PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40,2697513,0,1,1,0,0,2,0

		'Page 3 (default Text Splash 2 and 3 Lines)
			 PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,2697513,0,1,0,0,2,3,0
			 PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,2697513,0,1,0,0,30,3,0
			 PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,2697513,0,1,0,0,55,3,0


		'Page 4 (2 Line Gameplay DMD)
'			 PuPlayer.LabelNew pDMD,"Splash4a",dmdtomb,42,16727710,0,1,0,0,30,4,0 'Violet 16727710 - JauneVert 6028716
			 PuPlayer.LabelNew pDMD,"Splash4a",dmddef,30,16727710,0,1,0,0,10,4,0
			 PuPlayer.LabelNew pDMD,"Splash4b",dmddef,25,6028716,0,1,2,0,85,4,0

		'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
			PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0

		'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
			PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
			PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
			PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0

		'Page 7 (Show High Scores Fixed Fonts)
			PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,8454143,0,1,0,0,2,7,0
			PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,33023,0,1,0,0,20,7,0
			PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,33023,0,1,0,0,50,7,0

			
			pDMDStartBackLoop "PuPFrames","1-default.png"
'			pDMDStartBackLoop "DMDSplash","intro1.mp4"

	END IF  ' use PuPDMDDriver

	if PuPDMDDriverType=pDMDTypeLCD THEN  'Using 4:1 Standard ratio LCD PuPDMD  ************ lcd **************		
		'Page 1 (default score display)
		PuPlayer.LabelNew pDMD,"Credits" ,dmdscr,25,16744448   ,1,0,0,2,0,1,0 ',20,16744448   ,1,0,0,6,0,1,0 
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
		PuPlayer.LabelNew pDMD,"Splash4a",dmdtomb,42,16727710,0,1,0,0,0,4,0 'Violet 16727710 - JauneVert 6028716 / ACIEN 8454143 - 157
		PuPlayer.LabelNew pDMD,"Splash4b",dmdtomb,30,6028716,0,1,2,0,75,4,0

		'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
			PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,2697513,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,2697513  ,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,2697513  ,0,1,1,0,0,5,0

		'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
			PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,2697513,0,0,0,15,1,6,0
			PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,2697513,0,1,0,60,0,6,0
			PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,2697513,0,1,0,60,50,6,0

		'Page 7 (Show High Scores Fixed Fonts)
			PuPlayer.LabelNew pDMD,"Splash7a",dmdscr,36,16727710,0,1,0,0,1,7,0
			PuPlayer.LabelNew pDMD,"Splash7b",dmdscr,32,6028716,0,1,0,0,36,7,0
			PuPlayer.LabelNew pDMD,"Splash7c",dmdscr,28,6028716,0,1,0,0,66,7,0

'			pDMDStartBackLoop "DMDSplash","intro1.mp4"
			pDMDStartBackLoop "PuPFrames","0-default.png"

	END IF  ' use PuPDMDDriver

	if PuPDMDDriverType=pDMDTypeFULL THEN  'Using FULL BIG LCD PuPDMD  ************ lcd **************

		'Page 1 (default score display)	
			PuPlayer.LabelNew pDMD,"Credits" ,dmdscr,12,16744448   ,1,0,0,2,0,1,0 '
			PuPlayer.LabelNew pDMD,"Play1"   ,dmdscr,12,16744448   ,1,0,2,2,0,1,0
			PuPlayer.LabelNew pDMD,"Ball"    ,dmdscr,12,16744448   ,1,2,2,96,0,1,0 '
			PuPlayer.LabelNew pDMD,"MsgScore",dmdscr,30,33023   ,0,1,0, 0,40,1,0
			PuPlayer.LabelNew pDMD,"CurScore",dmdscr,35,33023   ,0,1,1, 0,0,1,0


		'Page 2 (default Text Splash 1 Big Line)
			PuPlayer.LabelNew pDMD,"Splash",dmdtomb,35,16744448,0,1,1,0,0,2,0

		'Page 3 (default Text 3 Lines)
			PuPlayer.LabelNew pDMD,"Splash3a",dmddef,15,2697513,0,1,0,0,35,3,0
			PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,10,2697513,0,1,0,0,50,3,0
			PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,10,2697513,0,1,0,0,65,3,0


		'Page 4 (default Text 2 Line)
'			PuPlayer.LabelNew pDMD,"Splash4a",dmddef,10,2110797,0,1,0,0,40,4,0
'			PuPlayer.LabelNew pDMD,"Splash4b",dmddef,8,2110797,0,1,2,0,60,4,0
			PuPlayer.LabelNew pDMD,"Splash4a",dmdtomb,28,16727710,0,1,0,0,10,4,0 'Violet 16727710 - JauneVert 6028716 / ACIEN 8454143 - 157
			PuPlayer.LabelNew pDMD,"Splash4b",dmdtomb,22,6028716,0,1,2,0,60,4,0

		'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
			PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,2697513,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,2697513  ,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,2697513  ,0,1,1,0,0,5,0

		'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
			PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,2697513,0,0,0,15,1,6,0
			PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,2697513,0,1,0,60,0,6,0
			PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,2697513,0,1,0,60,50,6,0

		'Page 7 (Show High Scores Fixed Fonts)
'			PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,2697513,0,1,0,0,2,7,0
'			PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,30,2697513,0,1,0,0,20,7,0
'			PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,30,2697513,0,1,0,0,50,7,0
			PuPlayer.LabelNew pDMD,"Splash7a",dmdscr,30,16727710,0,1,0,0,1,7,0
			PuPlayer.LabelNew pDMD,"Splash7b",dmdscr,22,6028716,0,1,0,0,36,7,0
			PuPlayer.LabelNew pDMD,"Splash7c",dmdscr,18,6028716,0,1,0,0,66,7,0


	'		pDMDStartBackLoop "DMDSplash","intro2.mp4"
			pDMDStartBackLoop "PuPFrames","2-default.png"

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
	pupDMDDisplay "attract","Welcome","@welcome.mp4",3,0,9
	pInAttract=true
	end Sub

	Sub pDMDStartGame
	pInAttract=false
	pDMDSetPage(pScores)   'set blank text overlay page.
	end Sub

	DIM pCurAttractPos: pCurAttractPos=0


	'********************** gets called auto each page next and timed already in DMD_Timer.  make sure you use pupDMDDisplay or it wont advance auto.
	Sub pAttractNext
	pCurAttractPos=pCurAttractPos+1

	  Select Case pCurAttractPos

	  Case 1 pupDMDDisplay "Splash4","Game^Over", "",3, 1,10
'		 PuPEvent 118
	  Case 2 pupDMDDisplay "highscore", "High Score^AAA   16711808^BBB   602871", "", 5, 0, 10 'ANCIEN COLOR 2451654-2342342 / Violet 16711808 - JauneVert 6028716
      Case 3 pupDMDDisplay "Splash4","Press^Start","",3,1,10
      Case 4 pupDMDDisplay "Splash4","REPLAY AT^1 000 000","",3,0,10
      Case 5 pupDMDDisplay "Splash4","Press^Start","",3,1,10
	  Case Else
		pCurAttractPos=0
		pAttractNext 'reset to beginning
	  end Select

	end Sub

'************************ called during gameplay to update Scores ***************************
Dim CurTestScore:CurTestScore=0
Sub pDMDUpdateScores  'call this ONLY on timer 300ms is good enough
if pDMDCurPage <> pScores then Exit Sub
puPlayer.LabelSet pDMD,"CurScore","" & FormatNumber(PlayerScore(CurrentPlayer), 0),1,""
puPlayer.LabelSet pDMD,"Play1","Player " & FormatNumber(CurrentPlayer, 0),1,""
puPlayer.LabelSet pDMD,"Ball","Ball " & FormatNumber(BallinGame(CurrentPlayer), 0) & " / " & FormatNumber(BallsPerGame, 0),1,""
puPlayer.LabelSet pDMD,"Credits","Credits " & Credits,1,""

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

		x = LoadValue(TableName, "UnlockMusicCount") 
		If(x <> "") And UnlockAllMusic = 0 then 
			UnlockMusicCount = CInt(x)
		Elseif (x <> "") And UnlockAllMusic = 1 then 
			UnlockMusicCount = 12
		Else 
			UnlockMusicCount = 4 
		End If
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
		If UnlockAllMusic = 0 Then
		SaveValue TableName, "UnlockMusicCount", UnlockMusicCount
		End If
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
	UnlockMusicCount = 3
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

	Sub HighScorelabels 'Violet 16727710 - JauneVert 6028716
		PuPlayer.LabelSet pBackglass,"HighScore","Your Name",1,"{'mt':2,'color':33023, 'size': 6 }"
		PuPlayer.LabelSet pBackglass,"HighScoreL1","A",1,"{'mt':2,'color':6028716, 'size': 6 }"
		PuPlayer.LabelSet pBackglass,"HighScoreL2","",1,"{'mt':2,'color':6028716, 'size': 6 }"
		PuPlayer.LabelSet pBackglass,"HighScoreL3","",1,"{'mt':2,'color':6028716, 'size': 6 }"
		PuPlayer.LabelSet pBackglass,"HighScoreL4",PlayerScore(CurrentPlayer),1,"{'mt':2,'color':65280, 'size': 6 }"
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
		PuPlayer.LabelSet pBackglass,"HighScoreL2"," ",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL3"," ",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL4"," ",1,""
		hsbModeActive = False
	End Sub

sub playclear(chan)
		if chan = pMusic Then
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":0 }"
		End If

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
	if timeSec=0 then timeSec=1 'don't allow page default page by accident


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

'-----------------------------------
' Wrecking Ball Code
'-----------------------------------
Dim WreckingBallActive
Dim Wrecker,Wrecker2,bottomlimit,XBallX,YBallX,ScaleFactor,LowerBottomLimit,UpperBottomLimit,Wreckerballsize
Dim HoldCrane,WreckBallCenterX,WreckBallCenterY
LowerBottomLimit = 30 
UpperBottomLimit = 120
bottomlimit = LowerBottomLimit
HoldCrane = False
WreckBallCenterX = WreckerCenterTrigger.X
WreckBallCenterY = WreckerCenterTrigger.Y
ScaleFactor = 0.65    'length of wrecker ball chain

Sub InitWreckerBall
	WreckingBallActive = True
	WreckerBallSize = Ballsize
	set Wrecker = WreckBallKicker1.createsizedball(Wreckerballsize / 2)
	WreckBallKicker1.kick 0,0
	WreckBallKicker1.enabled = false
	set Wrecker2 = WreckBallKicker.createball
	Wrecker2.visible = False
	WreckBallKicker.kick 0,0
	WreckBallKicker.timerEnabled = true
End Sub

Dim ORotX,ORotY,TransZValue
const P1Z=62
const P2Z=72
const P3Z=82
const P4Z=92
const P5Z=102
const P6Z=112
const P7Z=122
const P8Z=132
const P9Z=142
const P10Z=152
const ChainOrigin=162	
const CraneUp=100 

Sub ChaineOFF
	PWBallCylinder.visible = 0
	PChain1.visible = 0	 			
	PChain2.visible = 0
	PChain3.visible = 0
	PChain4.visible = 0
	PChain5.visible = 0
	PChain6.visible = 0
	PChain7.visible = 0
	PChain8.visible = 0
	PChain9.visible = 0
	PChain10.visible = 0
	Primitive057.visible = 0
End Sub
Sub ChaineON
	PWBallCylinder.visible = 1
	PChain1.visible = 1	 			
	PChain2.visible = 1
	PChain3.visible = 1
	PChain4.visible = 1
	PChain5.visible = 1
	PChain6.visible = 1
	PChain7.visible = 1
	PChain8.visible = 1
	PChain9.visible = 1
	PChain10.visible = 1
	Primitive057.visible = 0
	Primitive046.visible = 0
	Primitive31.collidable = True
End Sub

Sub WreckBallKicker_timer()
	If Primitive31.collidable = True Then
	if Wrecker2.z > 350 Then
		Wrecker2.z = 350
	end If

	Wrecker2.Velx = Wrecker2.Velx + Wrecker.velx * 1.7
	Wrecker2.Vely = Wrecker2.Vely + Wrecker.vely * 1.7
	Wrecker2.Velz = Wrecker2.Velz + Wrecker.velz * 1.3

    Wrecker.velx = 0
    Wrecker.vely = 0
    Wrecker.velZ = 0
  
	XBallX = (Wrecker2.X - WreckerCenterTrigger1.X)
	YBallX = (Wrecker2.Y - WreckerCenterTrigger1.Y)

    Wrecker.X = WreckBallCenterX + XBallX
    Wrecker.Y = WreckBallCenterY + YBallX
    Wrecker.Z = Wrecker2.Z + BottomLimit - 30

	if abs(XballX) > 115 then 
		WreckingBallActive = False
		ChaineOFF
		GrueState = 6
		CheckGrue
		BallsOnPlayfield = BallsOnPlayfield +1
		BumperCount = 0
		SpinnerCount = 0
		Trigger024.enabled = True
		Primitive31.collidable = False
'		if abs(XballX) > 30 then
'			if XballX > 0 then
'				PCraneArm.RotZ = 2.7
'			end if
'			if XballX < 0 then
'				PCraneArm.RotZ = 3.3
'			end if		
'		else
'			if abs(XballX) > 20 then
'				if XballX > 0 then
'					PCraneArm.RotZ = 2.8
'				end if
'				if XballX < 0 then
'					PCraneArm.RotZ = 3.2
'				end if		
'			else
'				if XballX > 0 then
'					PCraneArm.RotZ = 2.9
'				end if
'				if XballX < 0 then
'					PCraneArm.RotZ = 3.1
'				end if		
'			end if
'		end if
'	else
'		PCraneArm.RotZ = 3
	end if

	PWBallCylinder.X = Wrecker.X 
	PWBallCylinder.Y = Wrecker.Y 
	PWBallCylinder.Z = Wrecker.Z
	PWBallCylinder.TransZ = 25

	PChain1.X = WreckBallCenterX + (XBallX * ScaleFactor *.87)
	PChain1.Y = WreckBallCenterY + (YBallX * ScaleFactor *.87)
	PChain2.X = WreckBallCenterX + (XBallX * ScaleFactor *.79)
	PChain2.Y = WreckBallCenterY + (YBallX * ScaleFactor *.79)
	PChain3.X = WreckBallCenterX + (XBallX * ScaleFactor *.71)
	PChain3.Y = WreckBallCenterY + (YBallX * ScaleFactor *.71)
	PChain4.X = WreckBallCenterX + (XBallX * ScaleFactor *.63)
	PChain4.Y = WreckBallCenterY + (YBallX * ScaleFactor *.63)
	PChain5.X = WreckBallCenterX + (XBallX * ScaleFactor *.55)
	PChain5.Y = WreckBallCenterY + (YBallX * ScaleFactor *.55)
	PChain6.X = WreckBallCenterX + (XBallX * ScaleFactor *.47)
	PChain6.Y = WreckBallCenterY + (YBallX * ScaleFactor *.47)
	PChain7.X = WreckBallCenterX + (XBallX * ScaleFactor *.39)
	PChain7.Y = WreckBallCenterY + (YBallX * ScaleFactor *.39)
	PChain8.X = WreckBallCenterX + (XBallX * ScaleFactor *.31)
	PChain8.Y = WreckBallCenterY + (YBallX * ScaleFactor *.31)
	PChain9.X = WreckBallCenterX + (XBallX * ScaleFactor *.23)
	PChain9.Y = WreckBallCenterY + (YBallX * ScaleFactor *.23)
	PChain10.X = WreckBallCenterX + (XBallX * ScaleFactor *.15)
	PChain10.Y = WreckBallCenterY + (YBallX * ScaleFactor *.15)

	OrotX = YBallX * 0.85 					'reduce MaxRotation to 55 degrees
	OrotY = -XBallX * 0.85
	PWBallCylinder.RotX = ORotX * 0.8
	PWBallCylinder.RotY = ORotY * 0.8
	PChain1.RotX = ORotX * 1.3	 			'add some distortion to chain angle for each link
	PChain1.RotY = ORotY * 1.3
	PChain2.RotX = ORotX * 1.2
	PChain2.RotY = ORotY * 1.2
	PChain3.RotX = ORotX * 1.1
	PChain3.RotY = ORotY * 1.1
	PChain4.RotX = ORotX * 1.05
	PChain4.RotY = ORotY * 1.05
	PChain5.RotX = ORotX
	PChain5.RotY = ORotY
	PChain6.RotX = ORotX * 0.95
	PChain6.RotY = ORotY * 0.95
	PChain7.RotX = ORotX * 0.9
	PChain7.RotY = ORotY * 0.9
	PChain8.RotX = ORotX * 0.85
	PChain8.RotY = ORotY * 0.85
	PChain9.RotX = ORotX * 0.7
	PChain9.RotY = ORotY * 0.7
	PChain10.RotX = ORotX * 0.7
	PChain10.RotY = ORotY * 0.7

	TransZValue = (ChainOrigin + (bottomlimit-lowerbottomlimit) * Craneup / (upperbottomlimit-lowerbottomlimit) - Wrecker.Z)/10
	PChain1.Z = Wrecker.Z + 25 + 0.7*TransZValue
	PChain2.Z = Wrecker.Z + 25 + 1.8*TransZValue
	PChain3.Z = Wrecker.Z + 25 + 2.9*TransZValue
	PChain4.Z = Wrecker.Z + 25 + 4*TransZValue
	PChain5.Z = Wrecker.Z + 25 + 5*TransZValue
	PChain6.Z = Wrecker.Z + 25 + 6*TransZValue
	PChain7.Z = Wrecker.Z + 25 + 7*TransZValue
	PChain8.Z = Wrecker.Z + 25 + 8*TransZValue
	PChain9.Z = Wrecker.Z + 25 + 9*TransZValue
	PChain10.Z = Wrecker.Z + 25 + 10*TransZValue
	End if
End Sub

'***********
'MOOVE GRUE
'***********
Sub MoveGrue
	If GrueOFFTimer.enabled = 0 And GrueONTimer.enabled = 0 And Grue2ONTimer.enabled = 0 And Grue2OFFTimer.enabled = 0 Then
	GrueState = GrueState + 1
	CheckGrue
	End If 
End Sub

Sub CheckGrue
		If GrueState = 0 Then Primitive046.visible = 0 : ShakeGrueOFF : ShakeGrue2OFF : Light043.state = 2 : Light042.state = 0 : bumperbiglight1.state = 1 : bumperbiglight2.state = 1 : bumperbiglight3.state = 1 : Primitive030.visible = 0 : Primitive058.visible = 1 : ChaineOFF
		If GrueState = 1 Then Primitive046.visible = 0 : ShakeGrue2ON : Light043.state = 2 : Primitive030.visible = 1 : Primitive058.visible = 1 'Hit bas trigger019
		If GrueState = 2 Then Primitive046.visible = 1 : ShakeGrue2OFF : Light043.state = 0 : SpinnerModeActive = True : Light042.state = 2 : pupDMDDisplay "Splash4","HITS SPINNER^TURN THE CRANE","",4,0,14'B
		If GrueState = 3 Then Primitive046.visible = 1 : ShakeGrueON : BumperModeActive = True : Light042.state = 0 : bumperbiglight1.state = 2 : bumperbiglight2.state = 2 : bumperbiglight3.state = 2 : pupDMDDisplay "Splash4","HITS BUMPERS^DOWN THE BALL","",4,0,14'S 
		If GrueState = 4 Then Primitive046.visible = 1 : ShakeGrue2ON : bumperbiglight1.state = 1 : bumperbiglight2.state = 1 : bumperbiglight3.state = 1 : Primitive030.visible = 1 : Primitive058.visible = 1  
		If GrueState = 5 Then Primitive046.visible = 0 : ShakeGrue2OFF 'Kicker Add et Kick ball
		If GrueState = 6 Then Primitive046.visible = 0 : ShakeGrueOFF : ShakeGrue2OFF : GrueState = 0  
End Sub
Sub GrueOFFTimer_timer() 
If Primitive029.RotY = -69 Then PlaySound "fx_Grue"
Primitive058.visible = 1
Primitive030.visible = 0
Primitive029.RotY = Primitive029.RotY + 1
Primitive030.RotY = Primitive030.RotY + 1
Primitive061.RotY = Primitive061.RotY + 1
Primitive057.RotY = Primitive057.RotY + 1
Primitive058.RotY = Primitive058.RotY + 1
Primitive046.RotY = Primitive046.RotY + 1
if Primitive029.RotY >= 8 then GrueOFFTimer.enabled = 0 
End Sub
Sub GrueONTimer_timer()
If Primitive029.RotY = 8 Then PlaySound "fx_Grue"
Primitive058.visible = 1
Primitive030.visible = 0
Primitive029.RotY = Primitive029.RotY - 1
Primitive030.RotY = Primitive030.RotY - 1
Primitive061.RotY = Primitive061.RotY - 1
Primitive057.RotY = Primitive057.RotY - 1
Primitive058.RotY = Primitive058.RotY - 1
Primitive046.RotY = Primitive046.RotY - 1
if Primitive029.RotY <= -70 then GrueONTimer.enabled = 0
End Sub

Sub ShakeGrueON ' Grue Tourne avec Boule
	If GrueONTimer.enabled = 0 And Primitive029.RotY = 8 Then
	GrueONTimer.enabled = 1
'	PlaySound "fx_Grue"
	End If
End Sub
Sub ShakeGrueOFF ' Grue Tourne Retour Avec ou Sans boule
	If GrueOFFTimer.enabled = 0 And Primitive029.RotY = -70 Then 
	GrueOFFTimer.enabled = 1
'	PlaySound "fx_Grue"
	End If
End Sub

Sub Grue2ONTimer_timer() '1
'If Primitive030.TransY = 19 Then PlaySound ""
Primitive057.visible = 1
Primitive030.TransY = Primitive030.TransY - 1
Primitive057.TransY = Primitive057.TransY - 1
Primitive046.TransY = Primitive046.TransY - 1
if Primitive030.TransY <= -45 then 
	Grue2ONTimer.enabled = 0 
	If GrueState = 4 Then ChaineON : InitWreckerBall : Primitive030.visible = 0 : Primitive058.visible = 1
End If
End Sub
Sub Grue2OFFTimer_timer() '2
'If Primitive030.TransY = -44 Then PlaySound ""
Primitive030.TransY = Primitive030.TransY + 1
Primitive057.TransY = Primitive057.TransY + 1
Primitive046.TransY = Primitive046.TransY + 1
if Primitive030.TransY >= 20 then Grue2OFFTimer.enabled = 0 
End Sub

Sub ShakeGrue2ON ' Roue Descend Seule avec Boule ou sans boule
	If Grue2ONTimer.enabled = 0 And Primitive030.TransY = 20 Then
	Grue2ONTimer.enabled = 1
	End If
End Sub
Sub ShakeGrue2OFF ' Roue Monte avec Boule ou sans boule
	If Grue2OFFTimer.enabled = 0 And Primitive030.TransY = -45 Then
	Grue2OFFTimer.enabled = 1
	End If
End Sub

'******************************************************
'VR Stuff
'******************************************************
DIM VRThings

Sub LoadVRRoom
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
	for each VRThings in VR_Mega:VRThings.visible = 0:Next

	If RenderingMode = 2 or VRTest Then
		VRRoom = VRRoomChoice
		'disable table objects that should not be visible
		ramp16.visible = 0
		ramp15.visible = 0
		ramp17.visible = 0
		PinCab_Blades.visible = 0
		Trigger024.visible = 0
		Primitive049.visible = 0
		wall028.sidevisible = 0
		Primitive006.size_z=20 : Primitive006.roty=80 : Primitive006.x = 970
		Primitive003.x=236

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
	End If
End Sub