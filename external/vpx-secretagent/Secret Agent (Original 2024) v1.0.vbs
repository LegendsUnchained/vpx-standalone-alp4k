Option Explicit
Randomize

'*******************************************************************************
' CREDITS
' Layout and code: Wizball
' Playfield art and 3D models: Joe Picasso
' Character art: Verminator studios
' Music, sound effects, post models, audio editing: Lumigado
' Music: Dark Fantasy Studios
' Mechanical sounds: Fleep, Robby King Pin
' Voice acting: Decimal Random, Paula, Smaug, Sonicmega, Stardust, Wizball
' DOF: DarrinVPCLE, CharlieVPCLE
' Testing: Pinballwiz45B
' Lampz: Aphophis, nFozzy
' Flashers and bumpers: Flupper
' Flipper and physics corrections: nFozzy, Rothbauerw, iaakki, wrd1972, apophis
' VR Cabinet & Room: Sixtoe, Flupper, 3rdaxis, DGrimmReaper
'*******************************************************************************

'*******************************************
' User Options
'*******************************************

'----- Display Options
Dim bEnableFlexDMD : bEnableFlexDMD = True 'Set to false to disable FlexDMD

'----- Staged flippers options -----
Const stagedFlipperStyle = 0		' 0 = MagnaSaves, 1 = A and apostrophe

'----- Outlane difficulty -----
Const LeftOutlaneDifficulty = 0		' 0 = easy (default), 1 = medium, 2 = hard, 3 = extra hard (no post)
Const RightOutlaneDifficulty = 0	' 0 = easy (default), 1 = medium, 2 = hard, 3 = extra hard (no post)

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

'----- General Sound Options -----
Const fMusicVolume = 0.4 ' Change to adjust volume of the music
Const fSFXVolume = 1 ' Change to adjust volume of game sound effects
Const fMechSoundVolume = 0.8 ' Change to adjust volume of mechanical sounds
Dim BallRollVolume : BallRollVolume = 0.5 	'Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5	'Level of ramp rolling volume. Value between 0 and 1
Const fCalloutVolume = 1.0 ' Change to adjust volume of callouts
Const fDuckFactor = 0.05 ' How much to lower music by during callouts

'//  PositionalSoundPlaybackConfiguration:
'//  Specifies the sound playback configuration. Options:
'//  1 = Mono
'//  2 = Stereo (Only L+R channels)
'//  3 = Surround (Full surround sound support for SSF - Front L + Front R + Rear L + Rear R channels)
Const PositionalSoundPlaybackConfiguration = 3

'----- Virtual Reality Options -----
Dim VRRoomChoice : VRRoomChoice = 1
'0 - Force disable (Use this on earlier versions than 10.7.2 if you don't use VR)
'1 - Minimal Room
'2 - Ultra minimal room

' VERSION HISTORY
' .24 Merged new sound code from Lumigado with the DOF code.
' .25 Nfozzy physics, fixed lingering text when starting a new game, changed how the up post in the loop behaves
'	  Changed physics of the loop post and removed the pin in its lane, to make the autoplunge feed cleaner
'	  Set the ball velocity manually for pulling the ball into the lock, to make locks more reliable
'	  Added compensation virtual lock if the Earth lock magnet fails to grab the ball
' .26 Fixed physics on several posts
'	  Merged duplicate flipper_collide subs
'	  Stopped timers if the ball is held by an up post or the earth magnet
' .27 Changed the left ramp so that the end does not trigger the inlane rollover
'	  Total Mayhem should now have 6 balls even if balls were in the Earth lock
'	  Fixed timers not resuming after a virtual lock
' .28 Redesigned mode inserts, moved insert text to a flasher
'	  Made the left ramp narrower, the left loop wider, and moved the saucer and targets between
'     Slightly changed the lower flipper angles to make later shots more comfortable
'	  Ball save is no longer a Bag of Tricks award during Total Mayhem
' .29 Multiball total score shown no longer carries over after a multiball stack ends
'	  Two left loops in rapid succession should no longer cause a right loop to register
'	  Now shows player and ball number in the text rotation at the start of a ball
'	  Changed shot order in Floor is Lava to allow for better flow
'	  Added lockbar button support
' 	  Moved autoplunger kicker to allow it to kick a ball resting at the plunger
'	  Fixed game losing track of ball count when using a multiball extender
'     Added drop target scripting to allow multiple targets to be dropped from one ball
'	  If an Earth lock is lit, the shot now registers when hitting the bank instead of after the lock.
'	  Add more ball save time when adding multiple balls during a showdown
'	  Playfield multiplier can no longer time out during the super jackpot presentation
'	  Completing a showdown makes the villain's associated shot always count as a combo (permanent 2X)
'	  Attemped to fix Total Mayhem / ball save interaction, Fixed physical lock / Mayhem interaction
'	  Ball save now saves the ball when it hits an outlane switch (in single ball play)
' .30 Fix standup lights in Avalanche when it's the second Ice mode
'	  Add characters to playfield artwork
'	  Nerfed Flying Embers to 125 (normal) and 200 (hard) switch hits (was 150 and 250)
'	  Buffed Flying Embers score to 600 / switch (was 500)
'	  Nerfed Ice Showdown to 4 HP (was 5 HP)
'	  Buffed Ice Showdown score for damage to 50K (was 40K)
'	  In modes, show ball number and current player (in a multi player game) along with score
' .32 Added playfield mesh
'	  Updated Rubberizer
' .33 Added callouts
'	  Fixed intro / loop interaction with ducking
'	  Multiball revive callout now plays correctly
'	  Ball save callout now plays correctly when draining down an outlane
'	  Fixed countdown callouts in Earth showdown
' .34 Prevented the super jackpot sequence from being interrupted by other callouts
'	  Adjusted size of scoop walls at the upper scoop to reduce rejects
'	  Lights at the villains now indicate which multiballs are active
'	  Balls that fall through the playfield mesh are now plunged back into play
'     Removed restrictions on nudging after a tilt or draining Total Mayhem
' .35 Added physical ball trough, replaced all GetBalls, CreateBall and DestroyBall calls with gBOT
'	  Fixed repeated callout in Air showdown
'	  Reduced digital nudge strength
' .36 Redesigned drain logic to be more robust when the drain train goes choo choo
'	  (A drain now registers when the ball enters the actual trough, not the kicker into the trough)
'	  Added 0.5 sec to all ball save timers to compensate for the new drain logic
'	  Adjusted Earth lock to try to prevent ball clipping
'	  Added countdown callouts to Total Mayhem
' .37 Add lights to bumpers
'     Fix Autoplunge regression on the game's first ball save
'	  Added one way gate to ball release
' .38 Added backglass support
' .39 Added dynamic ball shadows
'	  Added slingshot corrections
'     Optimized ball rolling code
' .40 Separated insert blink cycles into fast / medium / slow cycles
'	  Added mode progress sounds to Water modes
' .41 Added experimental 3D inserts / lampz
'	  Fixed lingering song on fast restart
'	  Earth showdown now recognises dirty pools
'	  Fixed what character is blinking during callouts with mutiple character round robins
'     Addded mechanical tilt
'	  Ensured tilt callout plays in full
' .42 Fixed insert for side ramp
'     Fixed lingering insert lights
'     Character lights now use Lampz fading instead of light sequencers
' .43 Optimized display not to update unless the text has changed
'	  Made some opaque materials not use active opacity
'     Added callouts to Evil United
'	  Fixed bug where Earth multiball could stack with Evil United
' .44 Added animation to Bag of Tricks award
'     Added PlayJingle sub to handle jingles
'     Added jingles for playfield multipliers
'     Added AddDelayedCallout for when voiceovers and effects should be separated
'     If Playfield X and Bag of Tricks are both lit at the bottom saucer,
'       the game will now show the awards in sequence
'     Disabled Bag of Tricks during Total Mayhem
'     Reworked display text priorities to be consistent with callout priorities
'     Added Tilt warning sound effects
' .45 Fix Bag of Tricks animation not showing if the award was Light Extra Ball
'     Increased playfield friction and slope, increased flipper strength
'     Added many light shows
'     For the 1st water multiball, the blue target lights 2 hurry-ups
'     The previous rules for the 1st water multiball now apply for the 2nd, and so on
' .46 Fix crash in water showdown
' .47 Fix crash in Evil United
'	  Prevent modes from registering double hits from the magnet throwing the ball into
'	  the earth targets (only the hit that triggered the magnet should count)
'	  Fix Bag of Tricks animation not showing if the award was Add a ball
'	  Excluded lane lights from light shows
' .48 Added bonus burndown sequence with music
'	  Removed temporary Fire and Water tracks
'	  Apron display now defaults to off
'	  Fix dSleeves collection
' .49 Added VR room copied from VPW example table
'	  The display that was previously on the apron is now used for VR.
' .50 Added light show during bonus
' .53 New playfield art, plastics, and models. Reworked villain inserts.
' .54 Added suitcase animation for Bag of tricks
'	  The flasher on the left slingshot now acts as a kickout warning for the lower saucer
'	  Increased intensity of Bag of Trick inserts
'	  Turntable texture now rotates with the turntable
' .55 Updated playfield art, Added adjustable outlane posts
' .56 Updated playfield art, new decal on the right ramp
' .57 New VR cabinet textures
'     Added crosshair and multiball decals to inserts
'	  Bumpers can now spot Bag of Tricks targets
'     Increased bonus build from shots and targets
'     Completing a character's modes or super jackpot now contributes to bonus
'     Capped bonus X at 10X
'	  Added using flipper buttons to change pages in attract mode
'     Increased high score slots from 4 to 5
' .57b Removed bottom post of slingshots from target bouncer
'      Animated plunger in VR
'	   Hide rails in full screen view
'      Fix high score slot 5 not saving
'      Touched up playfield art around the side ramp arrow insert
' .58 New playfield mesh from iaakki
'	  Added a wall under the playfield to mitigate playfield mesh issues
'	  Update crosshair icons
'	  Update physics code
'	  Fix Narnia catcher
'	  New art for lower saucer
'	  New model and textures for the upper saucer
'	  Changed the upper saucer so that the ball doesn't sit as low
' .59 Merge DOF
'	  Fix plunger lane trigger
' .60 Add new voiceovers
'	  Add dual leaf switch support
' .61 Volume rebalance for voiceovers
' .62 Add sound effects for hitting standup targets
'	  Add sound effects for enterign high scores
'	  Add flipper button animations in VR
'	  New playfield art with cutout for right loop switch
'	  Ball should no longer go into the super skill shot from a lockbar button launch
' .63 adjusted volume mixing of some sound effects
' .64 Replaced cooldown for standup targets sounds with only playing one at a time
'     Added jingle for lighting a playfield multiplier
'     Added sound effects for when a magnet affects the ball in Beware Glitches
' RC1 Add new super jackpot jingle
'     Fix sounds for hitting X targets
'     Fix crosshair visuals on the inserts
'     Add new bumper sounds
'     Add new drop target visuals

'First, try to load the Controller.vbs (DOF), which helps controlling additional hardware like lights, gears, knockers, bells and chimes (to increase realism)
'This table uses DOF via the 'SoundFX' calls that are inserted in some of the PlaySound commands,
'which will then fire an additional event, instead of just playing a sample/sound effect
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

'If using Visual PinMAME (VPM), place the ROM/game name in the constant below,
'both for VPM, and DOF to load the right DOF config from the Configtool, whether it's a VPM or an Original table

LoadCoreVBS

Sub LoadCoreVBS
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    On Error Goto 0
End Sub

Dim aPowersOfTwo
aPowersOfTwo = Array(1, 2, 4, 8, &h10, &h20, &h40, &h80,_
				&h100, &h200, &h400, &h800, &h1000, &h2000, &h4000, 32768,_ 
				&h10000, &h20000, &h40000, &h80000, &h100000, &h200000, &h400000, &h800000,_
				&h1000000, &h2000000, &h4000000, &h8000000, &h10000000, &h20000000, &h40000000)
Dim nLastTurntableUpdate	' Timestamp in msec of when the turntable animation was last updated
Dim bTurnClockwise			' True if the turntable should go clockwise, false for anticlockwise
Dim oMagnetA				' Object for the turntable magnet
Dim nMagnetPulses			' Count of magnet pulses when wiggling the ball
Dim oMagnetB, oMagnetC, oMagnetD ' Playfield magnets in glitches mode
Dim nTimePFMagnet		' Time in ms until the playfield magnets are active again
Dim nTimeMagnetSound	' Timestamp of when a magnet sound was last played
Const cMagnetSoundCooldown = 1000
Dim bLockBall				' True if the turntable should lock a ball
Dim bPullingBall
Dim bMagnetDroppedBall
Dim bReleasingLocks			' True if balls are being released from the Earth lock
Dim aoTopRowDigits
Dim aoBottomRowDigits
Dim nAutoPlungeBalls	' Count of balls to autoplunge
Dim nBallsToFeed		' Count of balls to feed from the trough
Dim nReleaseDelay
Dim bFirstSwitchHit		' True if the ball has hit a switch since the start of the ball
Dim nSkillShotLane		' Which bumper lane is lit for a skill shot
Dim bSkillShotActive	' True if skill shot is active
Dim bBallHeld			' Is the ball grabbed by a saucer (pausing timers)?
Dim nTimeBallStuck
Dim nTimeLeftRampPost	' Time in ms before the left ramp post is lowered
Dim nTimeRightRampPost	' Time in ms before the right ramp post is lowered
Dim nTimeLoopPost		' Time in ms before the post in the loop is lowered
Dim nTimeSkillPost		' Time in ms before the post in the skill shot lane is lowered
Dim anScore				' Array of player scores
Dim nPlayer				' Number of current player (array index, so player 1 is 0)
Dim nPlayersInGame		' Count of players in the current game
Dim nPlayersLastGame	' Count of players last game
Dim nBall				' Count of which ball is currently in play
Dim BIP					' Number of balls in play
Dim sMusicTrack			' The music track currently playing
Dim nTimeDuckMusic		' How long in ms to play music quieter
Dim sDuckedTrack		' Which track is currently being ducked
Dim nExtraBalls			' Count of how many extra balls the current player has
Dim anExtraBallsLit		' Array of counts of extra balls lit, per player
Dim bIsExtraBall		' Boolean, true if the current ball is an extra ball.
Dim bDrainingBalls		' Is the table draining balls after a tilt or timed out multiball?
Dim nTimeDisplay		' Time in milliseconds of how long the display has shown a message cycle
Dim nScoreBlinks		' Count of which digit of the score to blink on the scoreboard
Dim sTextTop			' The text on the top line of the display
Dim sTextBottom			' The text on the bottom line of the display
Dim nTextDuration		' Time in ms of how much longer to display the current message
Dim nTextPriority ' a low number is a high priority, 1 means top priority
				  ' 99 means "only display if otherwise idle"
Dim oQueueTextTop		' Queue for messages to show on top row of display
Dim oQueueTextBottom	' Queue for messages to show on bottom row of display
Dim oQueueTextDuration	' Queue for how long, in ms, to display the message in oQueueText*
Dim oQueueStatusTop		' Queue for status messages to show on top row of display
Dim oQueueStatusBottom	' Queue for status messages to show on bottom row of display
Dim oQueueStatusDuration' Queue for how long, in ms, to display the message in oQueueStatus*
Dim oQueueSecretTop		' Queue for text shown on top line in Top Secret
Dim oQueueSecretBottom	' Queue for text shown on bottom line in Top Secret
Dim oQueueSecretValue	' Queue for points awarded in Top Secret
Dim bShowingInstantInfo ' Is the display currently showing instant info?
Dim nInstantInfoPage    ' Count of which page instant info is currently showing
Dim nAttractModePage	' 0 based index of attract mode page to display
Const nAttractModePageCount = 8
Const nAttractModePageTime = 2500 ' Time in ms between switching attract mode pages
Dim bSelectingMode		' True if the player is currently selecting a mode
Dim nTimeModeSelectVO	' Time in ms until a "select a mode" voiceover plays
Dim nModeSelected		' Current choice in mode selection
Dim nModeStatusShowing	' Index of which mode is currently being displayed
Dim nModeStatusPage		' Index of which page of the mode status is being displayed
Const cLastInfoPage = 19
Dim anBonus				' Array of incidental bonus, per player
Dim nBonusX				' bonus multiplier
Dim bCountingBonus		' Is the game currently displaying the bonus count?
Dim nBonusFrame			' Count of which (25 fps) frame of the bonus count is currently showing
Dim anBonusElements(5)	' Array of bonus value per element
Dim nBonusTotal			' Total value of bonus
Dim nBonusDisplayed		' Value displayed during the bonus burndown
Dim nTiltWarnings		' Number of tilt warning used this ball
Dim nTiltLevel			' How much has the game been nudged?
Dim nTimeBallSave		' Time in ms left on the ball save
Dim bOutlaneSave		' Boolean of if a ball has been saved after an outlane drain
Dim anColorCycleLight	' Array of colors of lights turned on in the color cycle
Dim anColorCycleDark	' Array of colors of lights turned off in the color cycle
Dim nColorCycleIndex	' Index of which color the lights are during attract mode
Dim nFlashSweepStep		' Count of current step in a flasher sequence
Dim nFlashColor			' Color of the current flasher sequence
Dim nFlashRepeats		' Count or times to repeat the current flasher sequence
Dim avModeColors(7)		' Array of bitsets for which colors to blink per upper arrow
Dim avMballColors(7)	' Array of bitsets for which colors to blink per lower arrow
Dim avDropsHit(3)		' Bitset of which of the ICE drop targets have been hit
Dim nDropResetTimer		' Time in ms remaining before the drop targets reset
Dim nPlayfieldXTimer	' Time in ms remaining on playfield X
Dim nPlayfieldX			' Value of current playfield multiplier
Dim anComboTimer(7)		' Array per shot of time remaining on combos
Dim anComboX(7)			' Array per player of shot multiplier from combo
Dim avPermaCombo(3)		' Array per player of if the shot has a permanent combo from a showdown win
Dim nComboNoCallout		' Count of number of combos made without a callout
Dim avXtargets(3)		' Array of bitsets per player of X targets hit
Dim abXLit				' Array per player of whether playfield X is lit
Dim avBagTargets(3)		' Array of bitsets per player of Bag of Tricks targets hit
Dim anBagLevel(3)		' Array per player of Bag of Tricks level lit
Dim anBagLevelSum(3)	' Array per player of total Bag of Tricks 
Dim abBagExtraBall(3)	' Array per player of if Bag of Tricks has given an XBall
Dim anBagAwards(3)		' Array per player of counts of bag awards collected
Dim aanLastBagAwards(3,3)' Array per player of the last 3 bag of tricks awards
Dim asBagAward			' Array of text strings to be displayed on the Bag of Tricks animation
Dim anBumpersForBag(3)	' Count of bumper hits left before spotting a Bag target
Dim nTimeBagAnimation	' Time in ms remaining in the Bag of Tricks animation
Dim sLastTargetSFX		' Name of effect that was last played for hitting a spot target
Dim bDelayBagOfTricks	' Boolean, true if the Bag of Tricks animation should be delayed
Dim anAddABallUsed(3)	' Boolean per player if add a ball has been given this wizard mode
Dim vBumperLanes		' Bitset of which top lanes are lit
Dim aanShotLastHit(3, 7)' Time a certain shot was last hit, per player
Dim avFireLanes(3)		' Array of bitsets per player of bottom lanes lit
Dim nBallPhysLocked		' Count of balls physically in the earth lock
Dim aanLocksLit(3,5)	' Count of locks lit per player and lair
Dim aanLocksMade(3,5)	' Count of locks made per player and lair
Dim aanLockProgress(3,5)' Count of shots made for lighting lock per player and lair
Dim bInMode				' Boolean, true if a one ball mode is currently running
Dim aanModeShots(3,28)	' Count of mode shots made per player and mode
Dim aavShotsLit(3,28)	' BitSet per player and mode of shots lit
Dim avModesRunning(3)	' Array of bitsets per player of modes currently running
Dim anWizardModeRunning(3) ' Array per player of the wizard mode (if Any) running
Dim avWizardModesLit(3) ' BitSet per player of wizard modes available
Dim avModesPlayed(3)	' Array of bitsets per player of modes finished
Dim anModeScore(28)		' Array per mode of points earned this ball
Dim abModeExtraBall(3)	' Array per player of if playing modes has given an XBall
Dim avCyber1TargetsHit(3) ' BitSet per player of targets hit in Cyber 1 mode
Dim aanCyber3ShotsHit(3,7) ' Array per player and shot of counts of shots in Cyber 3
Dim anCyber3AttackShot(3)' Array per player of which shot is attacked in Cyber 3
Dim nCyber3AttackTime	' Time in ms before an attack happens in Cyber 3
Dim aanWater3TypesLeft(3,3) ' Count per player and shot type of hits in Water 3
Dim anTimeWater2(7)		' Time in ms before shots expire in Water 2 mode
Dim anFire1FirstShot(3)	' Leftmost shot lit in Fire 1 mode
Dim aanFire3ShotOrder(3, 3) ' Shots to hit in plase 2 of Fire 3 mode
Dim nTimeEarth3Open		' Time in ms before the turntable closes in Fire 3 mode
Dim anEarth3Misses(3)	' Count per player of how many times the turntable has timed out
Dim anAir1Shot(3)		' Leftmost shot hit in Air 1 mode
Dim nTimeAir1Move		' Time in ms before shots move in Air 1 mode
Dim bAir1MoveLeft		' Boolean, true if shots in Air 1 mode move left
Dim anAir3Shot(3)		' Center of the 5 shots lit in Air 3 mode
Dim nTimeAir3Move		' Time in ms before shots move in Air 3 mode
Dim anIce1LastShot(3)	' Most recent shot hit in Ice 1 mode
Dim anIce3SwitchesLeft(3) ' Count per player of switches needed to light a shot in Ice 3
Dim avMayhemTargets(3)	' BitSet per player of targets hit in Total Mayhem
Dim nTimeMayhem			' Time in ms remaining in Total Mayhem
Dim anEvilUnitedShots(7)' Count per shot of times made in Evil United
Dim nTimeTopSecretText	' Time in ms until the next message in the queue is shown
Dim bTopSecretUpdate	' Boolean, if true then recalculate score next update in Top Secret
Dim nTopSecretSound		' Which sound to play when showing the queue in Top Secret
Dim nTopSecretFlash		' Which color to flash in Top Secret
Dim anTimeSuperModes(28)' Time in ms remaining on super spinner, etc
Dim aanMballsPlayed(3,5)' Count of times played per player and multiball
Dim aanMballJackpots(3,5) ' Count of jackpots collected per player and lair
Dim anTimerMballGrace(5)' Grace periods for multiballs
Dim anMballExtenders(3)	' Array per player of number of multiball extensions lit
Dim nFirstMBallStarted	' Mode number of the first multiball in the current stack
Dim nTimeDelayMultiball ' Time in ms to delay multiball for a light show
Dim vCyberJackpots		' BitSet of jackpots lit in Cyber multiball
Dim nCyberTimer			' Time in ms remaining on multiplier in Cyber multiball
Dim anTimerTorpedo(7)	' Array per shot of time remaining on torpedo
Dim nWaterFirstJackpot	' Shot where the first water jackpot is in Water multiball
Dim nWaterJackpots		' Count of jackpots lit in Water multiball
Dim nTimerWaterMove		' Time in ms before the water jackpot moves
Dim anFireSpreadTimer(7)' Array per shot of time before a fire spreads in Fire Mball
Dim abFireSpreadLeft(7) ' Array per shot of direction fire spreads in Fire Mball
Dim anEarthJackpotsL(3)	' Count per player of jackpots on left side in Earth multiball
Dim anEarthJackpotsR(3)	' Count per player of jackpots on right side in Earth multiball
Dim avAirLettersHit(3)	' BitSet per player of AEROBASE letters collected
Dim avAirLettersLit(3)	' BitSet per player of AEROBASE letters lit
Dim vAirMultipliers		' BitSet of which shots to multiply the Air jackpot are hit
Dim vIceJackpots		' BitSet of jackpots lit in Cyber multiball
Dim nLastIceJPAdded		' Shot where the most recent jackpot in Ice Mball was added
Dim avSJCollected(3)	' Array of bitsets per player of lair super jackpots collected
Dim nSuperJacksShown	' Count of how many super jackpots have been presented
Dim vSuperJPsLit		' BitSet of which super jackpots are lit
Dim bBallAtPlunger		' Is a ball resting at the plunger lane?
Dim oQueueShotVO		' Collection of voiceover lines added from a shot
Dim oQueuePlayVO		' Queue of voiceover lines to play
Dim bVOQueueLocked		' boolean, true if the callout queue is locked
Dim nLastJackpot		' Which multiball last had a jackpot callout
Dim nLastCharacter		' Which character most recently had a mode selected
Dim sLastCountdown		' Which callout was most recently used in a countdown
Dim sSFXToPlay			' Which sound effect is queued to play after a shot
Dim nSFXPriority		' The priority of the currently queued sound effect
Dim fSFXQueuedVolume		' The volume of the currently queued sound effect
Dim sJinglePlaying		' The name of the jingle currently playing, empty string if none
Dim sCurrentCallout		' Which callout is currently playing
Dim nCalloutTimeLeft    ' Time in ms left until the current callout has finished playing
Dim nTimeDelayCallout	' Time in ms to delay the current callout
Dim nCharSpeaking		' Which character is speaking a callout
Dim nCharBlinkCycle		' Where in the blink cycle the caracter inserts are
Dim nLightShowPriority	' The priority of the light show currently playing
Dim nLightShowCurrent	' The number of the light show currently running, 0 = none
Dim nLightShowTime		' Time in ms remaining of the light show currently playing
Dim bLightShowBlinkOdd  ' Boolean: If odd or even lights should be on when strobing all lights
Const nTimeCalloutPadding = 200 ' Time in ms between callouts
Dim nTimeDelaySaucerUpper	' Time in ms to delay the upper saucer kickout
Dim nTimeDelaySaucerLower	' Time in ms to delay the lower saucer kickout
Dim nTimeLowerLeftFlip	' The time when the lower left flipper was last raised
Dim nTimeLowerRightFlip	' The time when the lower right flipper was last raised
Dim nTimeUpperLeftFlip	' The time when the upper left flippers were last raised
Dim bFlippersEnabled	' Boolean, true if flippers are enabled 
Dim nTimeBumperHit		' the time when a bumper was last hit
Dim nTimeLastSwitch		' The time when any switch was last hit
Dim nTimeStartPressed	' The time when the start button was pressed
Dim nSpinnerSoundVol	' The current volume of the spinner sound effect
Dim aSegments(128)		' Array of display segments for backglass, index = ASCII value of letter
Dim sTimersRun

Dim nSpins
Dim aoDigit0Lights, aoDigit1Lights, aoDigit2Lights
aoDigit0Lights = Array(LightD0S1, LightD0S3, LightD0S6, LightD0S7,_
	LightD0S5, LightD0S2, LightD0S4)
aoDigit1Lights = Array(LightD1S1, LightD1S3, LightD1S6, LightD1S7,_
	LightD1S5, LightD1S2, LightD1S4)
aoDigit2Lights = Array(LightD2S1, LightD2S3, LightD2S6, LightD2S7,_
	LightD2S5, LightD2S2, LightD2S4)
Dim anDigitSegments
Dim aoShotLightsMode, aoShotLightsMBall, aoTopLaneLights, aoFireLaneLights
Dim aoModeLights, aoWaterLockLights, aoFireLockLights, aoEarthLockLights
Dim aoIceLockLights, aoDropLights, aoXLights, aoBagLights
Dim aoSuperJackLights, aoComboLights, aoAerobaseLights, aoCharLights, aoMiscLights
aoShotLightsMode = Array(lightUnderFlipper001, lightLeftLoop1, lightLeftScoop001,_
	lightLeftRamp001, lightMole001, lightSideRamp, lightRightRamp001, lightRightLoop001)
aoShotLightsMBall = Array(lightUnderFlipper002, lightLeftLoop2, lightLeftScoop002,_
	lightLeftRamp002, lightMole002, LightSuperJackpot, lightRightRamp002, lightRightLoop002)
aoTopLaneLights = Array(lightTop001, lightTop002, lightTop003)
aoFireLaneLights = Array(lightLaneF, lightLaneI, lightLaneR, lightLaneE)
aoModeLights = Array(LightMode001, LightMode002, LightMode003, LightMode004, _
	LightMode005, LightMode006, LightMode007, LightMode008, LightMode009, _
	LightMode010, LightMode011, LightMode012)
aoWaterLockLights = Array(LightLock1Water, LightLock2Water, LightLock3Water)
aoFireLockLights = Array(LightLock1Fire, LightLock2Fire, LightLock3Fire)
aoEarthLockLights = Array(LightLock1Earth, LightLock2Earth, LightLock3Earth)
aoIceLockLights = Array(LightLock1Ice, LightLock2Ice, LightLock3Ice)
aoDropLights = Array(lightIce001, lightIce002, lightIce003)
aoXLights = Array(lightX1, lightX2, lightX003, lightX004)
aoBagLights = Array(lightMyst001, lightMyst002, lightMyst003, lightMyst004)
aoSuperJackLights = Array(LightSuperCyber, LightSuperWater, LightSuperFire, _
	LightSuperEarth, LightSuperAir, LightSuperIce)
aoComboLights = Array(lightComboShatz, lightComboLLoop, lightComboSaucer, _
	lightComboLRamp, lightComboEarth, LightComboDummy, lightComboRRamp, lightComboRLoop)
aoAerobaseLights = Array(lightAir001, lightAir002, lightAir003, lightAir004,_
	lightAir005, lightAir006, lightAir007, lightAir008)
aoCharLights = Array(LightCharacterCyber, LightCharacterWater, LightCharacterFire, _
	LightCharacterEarth, LightCharacterAir, LightCharacterIce, LightCharacterHero)
aoMiscLights = Array(LightScore2X, LightScore3X, lightShootAgain, lightMystery,_
	LightScoreX, LightExtraBall, lightTorpedo)
Dim aoLightForMode
aoLightForMode = Array(LightMode001, LightMode002, LightComboDummy, LightComboDummy, _
	LightMode003, LightMode004, LightComboDummy, LightComboDummy, _
	LightMode005, LightMode006, LightComboDummy, LightComboDummy, _
	LightMode007, LightMode008, LightComboDummy, LightComboDummy, _
	LightMode009, LightMode010, LightComboDummy, LightComboDummy, _
	LightMode011, LightMode012, LightComboDummy, LightComboDummy, _
	LightComboDummy, LightComboDummy, LightComboDummy, LightComboDummy, LightComboDummy)
Dim anCurLightColor(16)

Dim anSwitchLastHit(44)
Const nDebounceTime = 150 ' Time in ms to ignore a switch after its last Hit

' Assign a number to each shot
Const eShotNone = -1 : Const eShotUnderFlipper = 0 : Const eShotLeftLoop = 1
Const eShotSaucer = 2 : Const eShotLeftRamp = 3 : Const eShotEarth = 4
Const eShotSideRamp = 5 : Const eShotRightRamp = 6 : Const eShotRightLoop = 7
' Assign a number to each switch
Const eSwitchNone = -1 : Const eSwitchLeftSlingshot = 0 : Const eSwitchLeftInlane = 1
Const eSwitchLeftOutlane = 2 : Const eSwitchKickerBottom = 3 : Const eSwitchTargetX1 = 4
Const eSwitchTargetX2 = 5 : Const eSwitchLoopLBottom = 6 : Const eSwitchLoopLTop = 7
Const eSwitchTargetM1 = 8 : Const eSwitchKickerTop = 9 : Const eSwitchTargetBlue = 10
Const eSwitchRampL = 11 : Const eSwitchBumper1 = 12
Const eSwitchBumper2 = 13 : Const eSwitchTopSlinghot = 14 : Const eSwitchTargetM2 = 15
Const eSwitchSkillShot = 16 : Const eSwitchTargetEarth1 = 17 : Const eSwitchTargetEarth2 = 18
Const eSwitchTargetEarth3 = 19 : Const eSwitchTargetEarth4 = 20 : Const eSwitchLock2 = 21
Const eSwitchLock1 = 22 : Const eSwitchTurntable = 23 : Const eSwitchTargetX3 = 24
Const eSwitchTargetX4 = 25 : Const eSwitchRampSide = 26 : Const eSwitchTargetM3 = 27
Const eSwitchRampR = 28 : Const eSwitchTargetM4 = 29 : Const eSwitchLoopRBottom = 30
Const eSwitchLoopRTop = 31 : Const eSwitchDropI = 32 : Const eSwitchDropC = 33
Const eSwitchDropE = 34 : Const eSwitchBallRelease = 35
Const eSwitchRightOutlane = 36 : Const eSwitchRightInlane = 37
Const eSwitchRightSlingshot = 38 : Const eSwitchRampRDrop = 39
Const eSwitchLaneTop1 = 40 : Const eSwitchLaneTop2 = 41 : Const eSwitchLaneTop3 = 42
Const eSwitchSpinner = 43 : Const eSwitchLock3 = 44
Dim anXTargetSwitches
anXTargetSwitches = Array(eSwitchTargetX1, eSwitchTargetX2, eSwitchTargetX3, _
	eSwitchTargetX4)
Dim anBagTargetSwitches
anBagTargetSwitches = Array(eSwitchTargetM1, eSwitchTargetM2, eSwitchTargetM3, _
	eSwitchTargetM4)

anDigitSegments = Array(63, 6, 91, 79, 102, 109, 125, 7, 127, 111)
'Modes
Const eModeNone = -1 : Const eModeCyber1 = 0 : Const eModeCyber2 = 1
Const eModeCyberMBall = 2 : Const eModeCyberWizard = 3 : Const eModeWater1 = 4
Const eModeWater2 = 5 : Const eModeWaterMBall = 6 : Const eModeWaterWizard = 7
Const eModeFire1 = 8 : Const eModeFire2 = 9 : Const eModeFireMBall = 10
Const eModeFireWizard = 11 : Const eModeEarth1 = 12 : Const eModeEarth2 = 13
Const eModeEarthMBall = 14 : Const eModeEarthWizard = 15 : Const eModeAir1 = 16
Const eModeAir2 = 17 : Const eModeAirMBall = 18 : Const eModeAirWizard = 19
Const eModeIce1 = 20 : Const eModeIce2 = 21 : Const eModeIceMBall = 22
Const eModeIceWizard = 23 : Const eModeSuperSpinner = 24 : Const eModeSuperRamps = 25
Const eModeTotalMayhem = 26 : Const eModeEvilUnited = 27 : Const eModeMegaWizard = 28
Dim asModeNames
asModeNames = Array("INFO HIGHWAY", "BEWARE GLITCHES", "", "CYBER SHOWDOWN", _
	"SHARK ATTACK", "SUBMARINE LEAK", "", "WATER SHOWDOWN", _
	"FLOOR IS LAVA", "FLYING EMBERS", "", "FIRE SHOWDOWN", _
	"NOT A DRILL", "LIGHTS OUT", "", "EARTH SHOWDOWN", _
	"DOG FIGHTING", "NO PARACHUTE", "", "AIR SHOWDOWN", _
	"GONDOLA LIFT", "AVALANCHE", "", "ICE SHOWDOWN",_
	"", "", "TOTAL MAYHEM", "EVIL UNITED", "TOP SUPER SECRET")
'Shot types in Water wizard
Const cTypeLoops = 0 : Const cTypeRamps = 1 : Const cTypeKickers = 2
Const cTypeTargets = 3
'Mystery awards
Const eMysteryNone = -1 : Const eMysteryMballCyber = 0 : Const eMysteryMballWater = 1
Const eMysteryMballFire = 2 : Const eMysteryMballEarth = 3 : Const eMysteryMballAir = 4
Const eMysteryMballIce = 5 : Const eMysteryPlayfieldX = 6 : Const eMysteryBonusX = 7
Const eMysteryBallSave = 8 : Const eMysteryUpgrade = 9 : Const eMysteryPoints = 10
'Light sequencers
Dim aoShotLightSeqs
aoShotLightSeqs = Array(LightSeqUnderFlipper, LightSeqLLoop, LightSeqLScoop, _
	LightSeqLRamp, LightSeqEarth, LightSeqSideRamp, LightSeqRRamp, LightSeqRLoop)
'Voiceovers
Const eVOPrioTilt = 1 : Const eVOPrioNewPlayer = 2 : Const eVOPrioBallSave = 3
Const eVOPrioSuperJackpot = 4
Const eVOPrioTiltWarning = 5 : Const eVOPrioExtraBall = 6 : Const eVOPrioModeStart = 7
Const eVOPrioModeEnd = 8 : Const eVOPrioMystery = 9 : Const eVOPrioPlayfieldX = 10
Const eVOPrioMballStart = 11 : Const eVOPrioMballReady = 12 : Const eVOPrioMballLock = 13
Const eVOPrioModeProgress = 14 : Const eVOPrioJackpot = 15 : Const eVOPrioCombo = 16
Const eVOPrioModeSelect = 17 : Const eVOPrioCountdown = 18 : Const eVOPrioStandup = 19
Const eVOPrioIdle = 99 
'Multiballs
Const eMballCyber = 0 : Const eMBallWater = 1 : Const eMballFire = 2
Const eMballEarth = 3 : Const eMballAir = 4 : Const eMballIce = 5
' Characters (for callouts)
Const eCharacterCyber = 0 : Const eCharacterWater = 1 : Const eCharacterFire = 2
Const eCharacterEarth = 3 : Const eCharacterAir = 4 : Const eCharacterIce = 5
Const eCharacterHero = 6 : const eCharacterNone = -1
' Light shows
Const eLightShowCyberLong = 1 : Const eLightShowCyberShort = 2 : Const eLightShowWaterLong = 3
Const eLightShowWaterShort = 4 : Const eLightShowFireLong = 5 : Const eLightShowFireShort = 6
Const eLightShowEarthLong = 7 : Const eLightShowEarthShort = 8 : Const eLightShowAirLong = 9
Const eLightShowAirShort = 10 : Const eLightShowIceLong = 11 : Const eLightShowIceShort = 12
Const eLightShowXBegin = 13 : Const eLightShowMystery = 14 : Const eLightShowSuper = 15
Const eLightShowMayhem = 16 : Const eLightShowModeComplete = 17 : Const eLightShowCyberBonus = 18
Const eLightShowWaterBonus = 19 : Const eLightShowFireBonus = 20 : Const eLightShowEarthBonus = 21
Const eLightShowAirBonus = 22 : Const eLightShowIceBonus = 23 : Const eLightShowBonusX = 24
Const eLightShowNone = 0

Dim an1BallModes
an1BallModes = Array(eModeCyber1, eModeCyber2, eModeWater1, eModeWater2, eModeFire1, _
	eModeFire2, eModeEarth1, eModeEarth2, eModeAir1, eModeAir2, eModeIce1, eModeIce2)
Dim anWizardModes
anWizardModes = Array(eModeTotalMayhem, eModeEvilUnited, eModeMegaWizard, eModeCyberWizard, _
	eModeWaterWizard, eModeFireWizard, eModeEarthWizard, eModeAirWizard, eModeIceWizard)

Dim anJackpotsForSuper
anJackpotsForSuper = Array(15, 6, 6, 6, 5, 5)

Const Color_LtGreen = &h33FF33
Const Color_LtRed = &h3333FF
Const Color_LtBlue = &hFF3333
Const Color_DarkYellow = 44975
Const Color_LtYellow = &h33FFFF
Const Color_Yellow = &h01FFF2
Const Color_LtOrange = &h3377FF
Const Color_Orange = &h55FF
Const Color_LtPurple = &hFF3399
Const Color_Purple = &hFF00B0
Const Color_LtCyan = &hFFFF33
Const Color_Gray = 9422335 ' RGB(255, 197, 143)
Const Color_WarmWhite = 14744831 ' RGB(255, 252, 224)
anColorCycleLight = Array(RGB(185,0,20), RGB(255,85,20), RGB(255,200,0), _
	RGB(25,205,25), RGB(0,255,200), RGB(15,85,255), RGB(85,44,255), RGB(255,245,235))
'anColorCycleLight = Array(Color_LtRed, Color_LtOrange, Color_LtYellow,_
'	Color_LtGreen, Color_LtCyan, Color_LtBlue, Color_LtPurple, vbWhite)
anColorCycleDark = Array(vbRed, Color_Orange, vbYellow,_
	vbGreen, vbCyan, vbBlue, Color_Purple, Color_Gray)
Const eColorNone = -1
Const eColorRed = 0
Const eColorOrange = 1
Const eColorYellow = 2
Const eColorGreen = 3
Const eColorCyan = 4
Const eColorBlue = 5
Const eColorPurple = 6
Const eColorWhite = 7
Const eColorRainbow = 8
Const nColorsInCycle = 8

Dim anMballColors
anMballColors = Array(eColorGreen, eColorBlue, eColorRed, eColorYellow, eColorPurple, _
	eColorWhite)

Const cHighScoreAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ. <"
Const cGameName = "secretagent"
Const nHighScoreSlots = 5
Dim HighScore(4)
Dim HighScoreName(4)
Dim SortedHighscores()
Dim SortedHighscoreNames()
Dim aScoreRanks
Dim nHighScoreIter
Dim nTimePressedLeft	' For entering highscores: when L button was pressed
Dim nTimePressedRight	' For entering highscores: when R button was pressed
Dim HighscoreLetter		' The index of the current letter in the cHighScoreAlphabet
Dim InitialsEntered     ' The initials being entered in the highscore
Dim bEnteringHighScore	' Are we currently entering a high score?
Const PlayUntilHit = -1
Const cBallSaveStart = 12500 ' Ball save timer at start of ball
Const cBallSaveMball = 9500 ' Ball save timer at start of multiball
Const cCyber3AttackInterval = 20000 ' Time between attacks in Cyber 3 mode
Const BUMPERS_TO_SPOT_TARGET = 20

Dim Ballsize,BallMass
Ballsize = 50
BallMass = 1.0
Dim ETBall1, ETBall2, ETBall3, ETBall4, ETBall5, ETBall6, gBOT
Dim anBallSurface(5)
Dim anBallLastSurface(5)
Const eSurfacePlayfield = 0 : Const eSurfacePlasticRamp = 1
Const eSurfaceWireRamp = 2 : Const eSurfaceMetalRamp = 3

Sub Table1_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.PullBack
		SoundPlungerPull
		If VRRoom > 0 Then
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
	End If

	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	If keycode = LeftFlipperKey Then
		VRFlipperButtonLeft.X = VRFlipperButtonLeft.X + 8
	End If

	If keycode = RightFlipperKey Then
		VRFlipperButtonRight.X = VRFlipperButtonRight.X - 8
	End If

	If keycode = LeftTiltKey Then
		SoundNudgeLeft
		Nudge 90, 2.5
	End If

	If keycode = RightTiltKey Then
		SoundNudgeRight
		Nudge 270, 2.5
	End If

	If keycode = CenterTiltKey Then
		SoundNudgeCenter
		Nudge 0, 3
	End If

	If nPlayersInGame > 0 And not bDrainingBalls Then

		If keycode = LeftFlipperKey Then
			If bSelectingMode Then
				ModeSelectPrevious
				PlaySound "SFX_mode_move_beep",0,fSFXVolume,0,0,1,1,1
				If not bFirstSwitchHit Then Exit Sub
			End If
			If BIP > 1 And bInMode = False Then
				ModeSelectPrevious
			End If
			If Not bCountingBonus Then
				SolLFlipper True
				DOFGadget 101, DOFOn, DOFFlippers
				If stagedFlipperStyle <> 1 Then
					SolULFlipper True
				End If
			End If
			avFireLanes(nPlayer).RotateLeft
			vBumperLanes.RotateLeft
			If bSkillShotActive Then
				nSkillShotLane = (nSkillShotLane + 2) mod 3
			End If
			UpdateBumperLanes eSwitchNone
			UpdateLightsFireLanes
			nTimeUpperLeftFlip = GameTime
			' double flip skip if flips are less than 50 ms apart
			If (GameTime - nTimeLowerRightFlip) < 50 Then
				DoubleFlipHurry
			End If
			If bShowingInstantInfo Then
				nInstantInfoPage = (nInstantInfoPage + 1) mod (cLastInfoPage + 1)
				nTimeDisplay = 0
			End If
		End If

		If keycode = RightFlipperKey Then
			If bSelectingMode Then
				ModeSelectNext
				PlaySound "SFX_mode_move_beep",0,fSFXVolume,0,0,1,1,1
				If not bFirstSwitchHit Then Exit Sub
			End If
			If BIP > 1 And bInMode = False Then
				ModeSelectNext
			End If
			If Not bCountingBonus Then
				SolRFlipper True
				DOFGadget 102, DOFOn, DOFFlippers
			End If
			avFireLanes(nPlayer).RotateRight
			vBumperLanes.RotateRight
			UpdateLightsFireLanes
			If bSkillShotActive Then
				nSkillShotLane = (nSkillShotLane + 1) mod 3
			End If
			UpdateBumperLanes eSwitchNone
			nTimeLowerRightFlip = GameTime
			' double flip skip if flips are less than 50 ms apart
			If (GameTime - nTimeUpperLeftFlip) < 50 Then
				DoubleFlipHurry
			End If
			If bShowingInstantInfo Then
				nInstantInfoPage = (nInstantInfoPage + 1) mod (cLastInfoPage + 1)
				nTimeDisplay = 0
			End If
		End If

		If 0 = stagedFlipperStyle Then
			If keycode = LeftMagnaSave Then
				nTimeLowerLeftFlip = GameTime
				avFireLanes(nPlayer).RotateLeft
				vBumperLanes.RotateLeft
				UpdateLightsFireLanes
				If nTimeUpperLeftFlip = -1 And bCountingBonus = False Then
					SolLFlipper True
					DOFGadget 101,DOFOn,DOFFlippers
				End If
			End If
		End If

		If 1 = stagedFlipperStyle Then
			If 30 = keycode Then
				If (not bSelectingMode) and (not bCountingBonus) and (not bDrainingBalls) Then
					SolULFlipper True
				End If
			End If
		End If

		If keycode = LeftTiltKey Or keycode = RightTiltKey or _
		keycode = CenterTiltKey Then
			HandleTilt
		End If

		If keycode = MechanicalTilt Then
			nTiltLevel = nTiltLevel + 6
			HandleTilt
		End If

		If keycode = LockBarKey or keycode = RightMagnaSave Then
			If bBallAtPlunger And (not bFirstSwitchHit) Then
				KickerAutoPlunge.enabled = True
			End If
		End If

		If keycode = RightMagnaSave or keycode = PlungerKey or keycode = LockBarKey Then
			If bSelectingMode And bFirstSwitchHit then
				ModeSelectChoose
			End If
			SoundStartButton
		End If

		If keycode = StartGameKey Then
			If nBall = 1 and nPlayersInGame < 4 Then
				PlaySound "fx-start"
				nPlayersInGame = nPlayersInGame + 1
				If nPlayersInGame > 1 Then
					ShowText "PLAYER " & nPlayersInGame, "ENTERED THE GAME", 2000, eVOPrioNewPlayer
					AddCallout "VO_hero_player" & nPlayersInGame & "_", eVOPrioNewPlayer, eCharacterHero
				End If
			End If
			nTimeStartPressed = GameTime
		End If

		if keycode = AddCreditKey Then
		End If		
	Else
		If keycode = LeftFlipperKey Then
			If bEnteringHighScore Then
				HighScoreLeftPressed
			Else
				' Go back two pages and tell the DMD to show the next page
				nAttractModePage = (nAttractModePage + (nAttractModePageCount - 2)) Mod nAttractModePageCount
				nTimeDisplay = nAttractModePageTime
			End If
		End If

		If keycode = RightFlipperKey Then
			If bEnteringHighScore Then
				HighScoreRightPressed
			Else
				' tell the DMD to show the next page
				nTimeDisplay = nAttractModePageTime
			End If
		End If

		If bEnteringHighScore And _
		(keycode = LockBarKey or keycode = PlungerKey or keycode = RightMagnaSave) Then
			HighScoreEnterPressed
		End If

		If keycode = StartGameKey Then
			SoundStartButton
			If bEnteringHighScore Then
				HighScoreEnterPressed
			ElseIf bDrainingBalls = False Then
				InitGame False
			End If
		End If
	End If
End Sub

Sub Table1_KeyUp(ByVal keycode)

	bFlippersEnabled = False
	If nPlayersInGame > 0 And bCountingBonus = False And _
	bEnteringHighScore = False And bDrainingBalls = False And _
	bSelectingMode = False Then
		bFlippersEnabled = True
	End If

	If keycode = PlungerKey Then
		Plunger.Fire
		If bBallAtPlunger Then
			SoundPlungerReleaseBall()   'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall() 'Plunger release sound when there is no ball in shooter lane
		End If

		DOF 203, DOFPulse
		If VRRoom > 0 Then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
			PinCab_Shooter.Y = 20
		End If
	End If	

	If keycode = LeftFlipperKey Then
		VRFlipperButtonLeft.X = VRFlipperButtonLeft.X - 8
		If bEnteringHighScore Then
			HighScoreLeftReleased
		Else
			If 0 = stagedFlipperStyle Then
				nTimeUpperLeftFlip = -1
				SolULFlipper False
			End If
			If nTimeLowerLeftFlip = -1 Then
				SolLFlipper False
			End If
			If nPlayersInGame > 0 And bCountingBonus = False And _
			bEnteringHighScore = False And bDrainingBalls = False And _
			bSelectingMode = False Then
				DOFGadget 101, DOFOff, DOFFlippers
			End If
		End If
	End If

	If keycode = RightFlipperKey Then
		VRFlipperButtonRight.X = VRFlipperButtonRight.X + 8
		If bEnteringHighScore Then
			HighScoreRightReleased
		Else
			nTimeLowerRightFlip = -1
			SolRFlipper False
			If nPlayersInGame > 0 And bCountingBonus = False And _
			bEnteringHighScore = False And bDrainingBalls = False And _
			bSelectingMode = False Then
				DOFGadget 102, DOFOff, DOFFlippers
			End If
		End If
	End If

	If keycode = LeftMagnaSave And 0 = stagedFlipperStyle Then
		nTimeLowerLeftFlip = -1
		If nTimeUpperLeftFlip = -1 And bCountingBonus = False And _
		bEnteringHighScore = False And bDrainingBalls = False Then
			SolLFlipper False
			DOFGadget 101, DOFOff, DOFFlippers
		End If
	End If

	If 1 = stagedFlipperStyle And 30 = keycode Then
		nTimeUpperLeftFlip = -1
		SolULFlipper False
	End If

	If keycode = StartGameKey Then
		nTimeStartPressed = 0
		If (bEnteringHighScore=FALSE) Then DOF 201, DOFPulse
	End If
End Sub

Sub DoubleFlipHurry
	if nTimeLeftRampPost > 0 then nTimeLeftRampPost = 1
	if nTimeRightRampPost > 0 then nTimeRightRampPost = 1
	If nTimeSkillPost > 0 Then nTimeSkillPost = 1
	If nTimeDelayMultiball > 0 Then nTimeDelayMultiball = 1
	If nTimeDelaySaucerUpper > 0 Then
		nTimeDelaySaucerUpper = 1
		If bSelectingMode then ModeSelectChoose
	End If
	If nTimeDelaySaucerLower > 0 And BIP < 2 Then
		LightShowEnd
		If bDelayBagOfTricks Then
			bDelayBagOfTricks = False
			AwardMystery True
		End If
		nTimeDelaySaucerLower = 1
	End If
	If nTimeBagAnimation > 0 And BIP < 2 Then
		nTimeBagAnimation = 0
		LightShowEnd
	End If
	If True = bCountingBonus and nBonusFrame < 225 Then
'		LightShowEnd
		nBonusFrame = 224
'		StopAllMusic
'		PlaySound "MUS_bonus_count_end", 0, fMusicVolume
'		If anScore(nPlayer) < 1e7 Then ' score is less than 10 million
'			ShowText "PLAYER " & (1 + nPlayer) & " " & getFormattedBonus(anScore(nPlayer)), _
'			"  BONUS " & getFormattedBonus(nBonusX * nBonusTotal), 2000, eVOPrioNewPlayer
'		Else ' score is 10 million or more
'			ShowText "P" & (1 + nPlayer) & "  " & WilliamsFormatNum(anScore(nPlayer)), _
'			"  BONUS " & getFormattedBonus(nBonusX * nBonusTotal), 2000, eVOPrioNewPlayer
'		End If
'		PlaySound "SFX_missile" & Int(rnd*3),0,fSFXVolume,0,0,1,1,1
	End If
End Sub

'*******************************************
'  Drain, Trough, and Ball Release
'*******************************************
' It is best practice to never destroy balls. This leads to more stable and accurate pinball game simulations.
' The following code supports a "physical trough" where balls are not destroyed.
' To use this, 
'	- The trough geometry needs to be modeled with walls, and a set of kickers needs to be added to 
'     the trough. The number of kickers depends on the number of physical balls on the table.
' 	- A timer called "UpdateTroughTimer" needs to be added to the table. It should have an interval of 300 and be initially disabled.
'   - The balls need to be created within the Table1_Init sub. A global ball array (gBOT) can be created and used throughout the script


'TROUGH 
Sub KickerTrough1_Hit   : UpdateTrough : End Sub
Sub KickerTrough1_UnHit : UpdateTrough : End Sub
Sub KickerTrough2_Hit   : UpdateTrough : End Sub
Sub KickerTrough2_UnHit : UpdateTrough : End Sub
Sub KickerTrough3_Hit   : UpdateTrough : End Sub
Sub KickerTrough3_UnHit : UpdateTrough : End Sub
Sub KickerTrough4_Hit   : UpdateTrough : End Sub
Sub KickerTrough4_UnHit : UpdateTrough : End Sub
Sub KickerTrough5_Hit   : UpdateTrough : End Sub
Sub KickerTrough5_UnHit : UpdateTrough : End Sub
'Sub KickerTrough6_Hit   : UpdateTrough : End Sub
Sub KickerTrough6_UnHit : UpdateTrough : End Sub


Sub UpdateTrough
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer
'	sTimersRun = sTimersRun & " UpdateTroughTimer"
	nReleaseDelay = nReleaseDelay - 300
	If nReleaseDelay < 1 Then nReleaseDelay = 0
	If KickerTrough1.BallCntOver = 0 Then KickerTrough2.kick 57, 10
	If KickerTrough2.BallCntOver = 0 Then KickerTrough3.kick 57, 10
	If KickerTrough3.BallCntOver = 0 Then KickerTrough4.kick 57, 10
	If KickerTrough4.BallCntOver = 0 Then KickerTrough5.kick 57, 10
	If KickerTrough5.BallCntOver = 0 Then KickerTrough6.kick 57, 10
	If nBallsToFeed > 0 and KickerTrough1.BallCntOver > 0 _
	And nReleaseDelay < 1 And nTimeDelayMultiball < 1 Then
		BallAddRoll KickerTrough1
		RandomSoundShooterFeeder
		DOFGadget 123,DOFPulse,DOFContactors
		KickerTrough1.kick 90, 7
		nBallsToFeed = nBallsToFeed - 1
		nReleaseDelay = 1500
	End If
	If nReleaseDelay < 1 and nBallsToFeed < 1 then Me.Enabled = 0
End Sub

Sub Drain_Hit
	BallRemoveRoll Drain
	PlaySound "drain",0,1 * fMechSoundVolume,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain)
	Drain.TimerEnabled = True
	If (nTimeBallSave<=0) Then 
		if (BIP<2) Then 
			if (nPlayersInGame>0) Then DOF 219, DOFPulse
		end if
	elseif (BIP<2) then
			DOF 218, DOFPulse
	End If	
End Sub

Sub Drain_Timer
	Drain.kick 57, 20
	Drain.TimerEnabled = False
End Sub

Sub KickerTrough6_Hit
	Dim i

	UpdateTrough
	BIP = BIP - 1
	' Handle balls kicked out from locks after a game over
	If nPlayersInGame = 0 Then
		If BIP <= 0 Then
			BIP = 0
			bDrainingBalls = False
		End If
		Exit Sub
	End If

	' Handle ball that drained in an outlane during ball save
	If bOutlaneSave Then
		BIP = BIP + 1
		bOutlaneSave = False
	' handle ball saver
	Elseif (nTimeBallSave > 0) Or _
	(avModesRunning(nPlayer).Contains(eModeTotalMayhem) And (nTimeMayhem > 0)) Then
		BIP = BIP + 1
		If BIP < 2 Then
			If nExtraBalls > 0 Then
				LightShootAgain.state = LightStateOn
			Else
				LightShootAgain.state = LightStateOff
			End If
			nTimeBallSave = 0
			AddCallout "VO_hero_ball_save", eVOPrioBallSave, eCharacterHero
			ShowText "BALL SAVED", "PLAYER " & (nPlayer + 1), 2500, eVOPrioBallSave
		End If
		nBallsToFeed = nBallsToFeed + 1
		KickerAutoPlunge.Enabled = True
		Exit Sub
	End If

	If 1 = BIP Then
		If anMballExtenders(nPlayer) > 0 _
		And (Not avModesRunning(nPlayer).Contains(eModeTotalMayhem)) Then
			' TODO: sound
			ShowText "MULTIBALL", "EXTENDED", 2500, eVOPrioBallSave
			anMballExtenders(nPlayer) = anMballExtenders(nPlayer) - 1
			nTimeBallSave = nTimeBallSave + cBallSaveMball
			AddBalls 1
			Exit Sub
		End If
		If avModesRunning(nPlayer).Contains(eModeEvilUnited) Then
			If Not TimerSJCallout.Enabled Then EvilUnitedEnd True
			Exit Sub
		End If
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			for i = eMballCyber to eMballIce
				If vSuperJPsLit.Contains(i) Then aanMballJackpots(nPlayer, i) = 0
			Next
			avMballColors(eShotSideRamp).Clear
			If avModesRunning(nPlayer).Contains(eModeEarthMBall) Then
				anTimerMballGrace(eMballEarth) = 6000
				EarthMBallUpdate eShotNone
			End If
			If avModesRunning(nPlayer).Contains(eModeFireMBall) Then
				anTimerMballGrace(eMballFire) = 6000
				FireMBallUpdate eShotNone
			End If
			If avModesRunning(nPlayer).Contains(eModeCyberMBall) Then
				anTimerMballGrace(eMballCyber) = 6000
				CyberMBallUpdate eShotNone
			End If
			If avModesRunning(nPlayer).Contains(eModeIceMBall) Then
				anTimerMballGrace(eMballIce) = 6000
				IceMBallUpdate eShotNone
			End If
			If avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
				anTimerMballGrace(eMBallWater) = 6000
				WaterMBallUpdate eShotNone
			End If
			If avModesRunning(nPlayer).Contains(eModeAirMBall) Then
				anTimerMballGrace(eMballAir) = 6000
				AirMBallUpdate eShotNone
			End If
		Else
			If Not bDrainingBalls Then SuperSecretEnd True
		End If
	End If

	If BIP = 0 then
		LeftFlipper.RotateToStart
		LeftUpperFlipper.RotateToStart
		RightFlipper.RotateToStart
		DOF 101, DOFOff
		DOF 102, DOFOff

		If avModesRunning(nPlayer).Contains(eModeEarthWizard) Then
			AddCallout "VO_earth_showdown_drain", eVOPrioModeEnd, eCharacterEarth
			EarthMode3End True
		End If
		If bDrainingBalls Then
			Leftslingshot.disabled = False
			Rightslingshot.disabled = False
			Bumper1.HasHitEvent = True
			Bumper2.HasHitEvent = True
			TopSlingShot.disabled = False
			' handle tilt
			If nTiltWarnings < 3 Then
				BIP = 1
'				KickerAutoPlunge.enabled = False
				AutoPlunge 1
'				bBallLaunched = False
				if avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
					avModesRunning(nPlayer).Remove eModeTotalMayhem
					TotalMayhemEnd False
				End If
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					SuperSecretEnd False
				End If
			Else
				InitBonus
			End If
			bDrainingBalls = False
			Exit Sub
		Else
			If avModesRunning(nPlayer).Contains(eModeIceWizard) Then
				AddCallout "VO_ice_showdown_drain", eVOPrioModeEnd, eCharacterIce
			ElseIf avModesRunning(nPlayer).Contains(eModeAirWizard) Then
				AddCallout "VO_air_showdown_drain", eVOPrioModeEnd, eCharacterAir
			ElseIf avModesRunning(nPlayer).Contains(eModeFireWizard) Then
' FIXME: Missing voice line
'				AddCallout "VO_fire_showdown_drain", eVOPrioModeEnd, eCharacterFire
			End If
			InitBonus
		End If
	End If
End Sub

Sub EndBall
	ClearTextQueue
	ClearStatus
	if nPlayer = nPlayersInGame - 1 and nBall = 3 and nExtraBalls = 0 then
		nPlayersLastGame = nPlayersInGame
		nPlayersInGame = 0
		TimerLockCount.Enabled = False
		ShowText "GAME OVER", "", 5000, eVOPrioTilt
		CheckHighScore(anScore)
		If nBallPhysLocked > 0 Then
			bTurnClockwise = True
			TimerTurntable_Timer
			UpPostMove LockPost,LockPostPrimitive,"down"
			LockPosttimer.Enabled = True
			BIP = BIP + nBallPhysLocked
		End If
		If BIP > 0 Then bDrainingBalls = True
		exit sub
	elseif nExtraBalls > 0 then
		nExtraBalls = nExtraBalls - 1
		bIsExtraBall = True
		If nExtraBalls < 1 Then
			LightShootAgain.state = LightStateOff
		End If
		ShowText "PLAYER " + cStr(nPlayer + 1), "SHOOT AGAIN", 3000, eVOPrioNewPlayer
	else
		if nPlayer = nPlayersInGame - 1 then
			nPlayer = 0
			nBall = nBall + 1
		else 
			nPlayer = nPlayer + 1
		end if
	end if
	InitBall False
End Sub

Sub UpdateOutlanePosts
	Select Case LeftOutlaneDifficulty
		Case 0
			outlaneLeftEasyPost.collidable = True
			outlaneLeftEasyRubber.visible = True
			outlaneLeftEasyPin.visible = True
			outlaneLeftMediumPost.collidable = False
			outlaneLeftMediumRubber.visible = False
			outlaneLeftMediumPin.visible = False
			outlaneLeftHardPost.collidable = False
			outlaneLeftHardRubber.visible = False
			outlaneLeftHardPin.visible = False
		Case 1
			outlaneLeftEasyPost.collidable = False
			outlaneLeftEasyRubber.visible = False
			outlaneLeftEasyPin.visible = False
			outlaneLeftMediumPost.collidable = True
			outlaneLeftMediumPin.visible = True
			outlaneLeftMediumRubber.visible = True
			outlaneLeftHardPost.collidable = False
			outlaneLeftHardRubber.visible = False
			outlaneLeftHardPin.visible = False
		Case 2
			outlaneLeftEasyPost.collidable = False
			outlaneLeftEasyRubber.visible = False
			outlaneLeftEasyPin.visible = False
			outlaneLeftMediumPost.collidable = False
			outlaneLeftMediumRubber.visible = False
			outlaneLeftMediumPin.visible = False
			outlaneLeftHardPost.collidable = True
			outlaneLeftHardRubber.visible = True
			outlaneLeftHardPin.visible = True
		Case 3
			outlaneLeftEasyPost.collidable = False
			outlaneLeftEasyRubber.visible = False
			outlaneLeftEasyPin.visible = False
			outlaneLeftMediumPost.collidable = False
			outlaneLeftMediumRubber.visible = False
			outlaneLeftMediumPin.visible = False
			outlaneLeftHardPost.collidable = False
			outlaneLeftHardRubber.visible = False
			outlaneLeftHardPin.visible = False
	End Select

	Select Case RightOutlaneDifficulty
		Case 0
			outlaneRightEasyPost.collidable = True
			outlaneRightEasyRubber.visible = True
			outlaneRightEasyPin.visible = True
			outlaneRightMediumPost.collidable = False
			outlaneRightMediumRubber.visible = False
			outlaneRightMediumPin.visible = False
			outlaneRightHardPost.collidable = False
			outlaneRightHardRubber.visible = False
			outlaneRightHardPin.visible = False
		Case 1
			outlaneRightEasyPost.collidable = False
			outlaneRightEasyRubber.visible = False
			outlaneRightEasyPin.visible = False
			outlaneRightMediumPost.collidable = True
			outlaneRightMediumRubber.visible = True
			outlaneRightMediumPin.visible = True
			outlaneRightHardPost.collidable = False
			outlaneRightHardRubber.visible = False
			outlaneRightHardPin.visible = False
		Case 2
			outlaneRightEasyPost.collidable = False
			outlaneRightEasyRubber.visible = False
			outlaneRightEasyPin.visible = False
			outlaneRightMediumPost.collidable = False
			outlaneRightMediumRubber.visible = False
			outlaneRightMediumPin.visible = False
			outlaneRightHardPost.collidable = True
			outlaneRightHardRubber.visible = True
			outlaneRightHardPin.visible = True
		Case 3
			outlaneRightEasyPost.collidable = False
			outlaneRightEasyRubber.visible = False
			outlaneRightEasyPin.visible = False
			outlaneRightMediumPost.collidable = False
			outlaneRightMediumRubber.visible = False
			outlaneRightMediumPin.visible = False
			outlaneRightHardPost.collidable = False
			outlaneRightHardRubber.visible = False
			outlaneRightHardPin.visible = False
	End Select
End Sub


Sub HandleTilt
	Dim i

	If BIP < 1 Then Exit Sub
	nTiltLevel = nTiltLevel + 10
	If nTiltLevel > 15 Then
		nTiltWarnings = nTiltWarnings + 1
		Select Case nTiltWarnings
			Case 1
				LightShowStart eLightShowMayhem, eVOPrioTiltWarning
				PlaySound "SFX_warning_first"
				Select Case nLastCharacter
					Case eCharacterCyber
						AddDelayedCallout "VO_cyber_tilt_warning", eVOPrioTiltWarning, eCharacterCyber, 1150
					Case eCharacterWater
						AddDelayedCallout "VO_water_tilt_warning", eVOPrioTiltWarning, eCharacterWater, 1150
					Case eCharacterFire
						AddDelayedCallout "VO_fire_tilt_warning", eVOPrioTiltWarning, eCharacterFire, 1150
					Case eCharacterEarth
						AddDelayedCallout "VO_earth_tilt_warning", eVOPrioTiltWarning, eCharacterEarth, 1150
					Case eCharacterAir
						AddDelayedCallout "VO_air_tilt_warning", eVOPrioTiltWarning, eCharacterAir, 1150
					Case Else
						AddDelayedCallout "VO_ice_tilt_warning", eVOPrioTiltWarning, eCharacterIce, 1150
				End Select
				ShowText "DANGER", "", 2000, eVOPrioTiltWarning
			Case 2
				LightShowStart eLightShowMayhem, eVOPrioTiltWarning
				PlaySound "SFX_warning_second"
				AddDelayedCallout "VO_hero_tilt_warning2", eVOPrioTiltWarning, eCharacterHero, 1150
				ShowText "DANGER", "DANGER", 2000, eVOPrioTiltWarning
			Case 3
				bDrainingBalls = True
				nTimeBallSave = 0
				nTimeDelaySaucerUpper = 1
				nTimeDelaySaucerLower = 1
				nTimeBagAnimation = 0
				bDelayBagOfTricks = False

				Select Case nLastCharacter
					Case eCharacterCyber
						AddCallout "VO_cyber_tilt", eVOPrioTilt, eCharacterCyber
					Case eCharacterWater
						AddCallout "VO_water_tilt", eVOPrioTilt, eCharacterWater
					Case eCharacterFire
						AddCallout "VO_fire_tilt", eVOPrioTilt, eCharacterFire
					Case eCharacterEarth
						AddCallout "VO_earth_tilt", eVOPrioTilt, eCharacterEarth
					Case eCharacterAir
						AddCallout "VO_air_tilt", eVOPrioTilt, eCharacterAir
					Case Else
						AddCallout "VO_ice_tilt", eVOPrioTilt, eCharacterIce
				End Select
				for i = eMballCyber to eMballIce
					anTimerMballGrace(i) = 0
				Next
				If avModesRunning(nPlayer).Contains(eModeCyberMBall) Then
					CyberMballEnd
				End If
				If avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
					WaterMballEnd
				End If
				If avModesRunning(nPlayer).Contains(eModeFireMBall) Then
					FireMballEnd
				End If
				If avModesRunning(nPlayer).Contains(eModeEarthMBall) Then
					EarthMballEnd
				End If
				If avModesRunning(nPlayer).Contains(eModeAirMBall) Then
					AirMballEnd
				End If
				If avModesRunning(nPlayer).Contains(eModeIceMBall) Then
					IceMballEnd
				End If
				If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
					nTimeMayhem = 0
					avModesRunning(nPlayer).Remove eModeTotalMayhem
					TotalMayhemEnd True
				End If
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					SuperSecretEnd True
				End If
				oMagnetB.MagnetOn = False : oMagnetC.MagnetOn = False : oMagnetD.MagnetOn = False
				nTextDuration = 0
				ShowText "TILT", "", 500, eVOPrioTilt
				QueueText "", "TILT", 500
				QueueText "TILT", "", 500
				QueueText "*loop*", "", 500
				
				StopAllMusic
				AllLightsOff
				LightSeq1.play SeqDownOff
				LeftSlingshot.disabled = True
				RightSlingshot.disabled = True
				Bumper1.HasHitEvent = False
				Bumper2.HasHitEvent = False
				TopSlingShot.disabled = True
				LeftFlipper.RotateToStart
				LeftUpperFlipper.RotateToStart
				RightFlipper.RotateToStart
				DOF 101, DOFOff
				DOF 102, DOFOff
		End Select
	End If
End Sub

Sub Tilt_Timer
	Tilt.Enabled = True
	If nTiltLevel > 0 Then nTiltLevel = nTiltLevel - 1
End Sub

'*******************************************
' VR Room 
'*******************************************

Dim VRRoom
Sub InitVR
	If version < 10702 Then
		If VRRoomChoice <> 0 Then
			VRRoom = VRRoomChoice
			bEnableFlexDMD = False
		Else
			VRRoom = 0
		End If
	Else
		If RenderingMode = 2 And VRRoomChoice <> 0 Then
			VRRoom = VRRoomChoice
			bEnableFlexDMD = False
		Else
			VRRoom = 0
		End If
	End If
	DIM VRThings
	If VRRoom > 0 Then
		If VRRoom = 1 Then
			for each VRThings in VR_Cab:VRThings.visible = 1:Next
			PinCab_Rails.visible = 1
		End If
		If VRRoom = 2 Then
			for each VRThings in VR_Cab:VRThings.visible = 0:Next
			PinCab_Backglass.visible = 1
		End If
	Else
		for each VRThings in VR_Cab:VRThings.visible = 0:Next
		if Table1.ShowDT then PinCab_Rails.visible = 1
	End if
End Sub

'**********************************************************************************************************
' VR Plunger
'**********************************************************************************************************

Sub TimerVRPlunger_Timer
	If PinCab_Shooter.Y < 120 then
		PinCab_Shooter.Y = PinCab_Shooter.Y + 5
	End If
End Sub

Sub TimerVRPlunger2_Timer
	PinCab_Shooter.Y = 20 + (5* Plunger.Position)
End Sub


Sub Table1_Init
	Dim i, j

	Set oMagnetA = new cvpmMagnet
	With oMagnetA
		.InitMagnet trigger2, 30
		.GrabCenter = False
		.MagnetOn = False
		.CreateEvents "oMagnetA"
	End With
	Set oMagnetB = new cvpmMagnet
	With oMagnetB
		.InitMagnet TriggerMagnet2, 15
		.GrabCenter = False
		.MagnetOn = False
		.CreateEvents "oMagnetB"
	End With
	Set oMagnetC = new cvpmMagnet
	With oMagnetC
		.InitMagnet TriggerMagnet3, 15
		.GrabCenter = False
		.MagnetOn = False
		.CreateEvents "oMagnetC"
	End With
	Set oMagnetD = new cvpmMagnet
	With oMagnetD
		.InitMagnet TriggerMagnet4, 15
		.GrabCenter = False
		.MagnetOn = False
		.CreateEvents "oMagnetD"
	End With

	'Ball initializations need for physical trough
	Set ETBall1 = KickerTrough1.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set ETBall2 = KickerTrough2.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set ETBall3 = KickerTrough3.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set ETBall4 = KickerTrough4.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set ETBall5 = KickerTrough5.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set ETBall6 = KickerTrough6.CreateSizedballWithMass(Ballsize/2,Ballmass)

	'*** Use gBOT in the script wherever BOT is normally used. Then there is no need for GetBalls calls ***
	gBOT = Array(ETBall1, ETBall2, ETBall3, ETBall4, ETBall5, ETBall6)
	for i = 0 to 5
		anBallSurface(i) = eSurfacePlayfield
		anBallLastSurface(i) = eSurfacePlayfield
	next
	InitVR
	LoadEM
	InitLampsNF
	for i = 0 to 16
		anCurLightColor(i) = 0
	Next

	nBallPhysLocked = 0
	nMagnetPulses = 0
	nTimeDuckMusic = 0
	nPlayer = 0
	nPlayersInGame = 0
	nPlayersLastGame = 4
	anScore = Array(0, 0, 0, 0)
	bLockBall = False
	bPullingBall = False
	bDrainingBalls = False
	bShowingInstantInfo = False
	nTimeLowerLeftFlip = -1
	nTimeLowerRightFlip = -1
	nTimeUpperLeftFlip = -1
	bFlippersEnabled = True
	nTimeBallStuck = -1
	WallTurn1.isDropped = False
	WallTurn1Guard.isDropped = False
	WallTurn2.isDropped = True
	PostTurn1.isDropped = False
	PostTurn2.isDropped = True
	UpPostMove SkillPost,SkillPostPrimitive,"down"
	UpPostMove LoopPost,LoopPostPrimitive,"down"
	UpdateOutlanePosts

	For i = 0 to 7
		Set avModeColors(i) = New BitSet : avModeColors(i).Capacity = 9
		Set avMBallColors(i) = New BitSet : avMBallColors(i).Capacity = 9
	Next
	for i = 0 to 3
		Set avDropsHit(i) = new BitSet : avDropsHit(i).Capacity = 3
		Set avModesPlayed(i) = new BitSet : avModesPlayed(i).Capacity = 29
		Set avSJCollected(i) = new BitSet : avSJCollected(i).Capacity = 6
		Set avModesRunning(i) = new BitSet : avModesRunning(i).Capacity = 29
		Set avWizardModesLit(i) = New BitSet : avWizardModesLit(i).Capacity = 29
		Set avFireLanes(i) = New BitSet : avFireLanes(i).Capacity = 4
		Set avXtargets(i) = New BitSet : avXtargets(i).Capacity = 4
		Set avPermaCombo(i) = New BitSet : avPermaCombo(i).Capacity = 8
		Set avBagTargets(i) = New BitSet : avBagTargets(i).Capacity = 4
		Set avAirLettersLit(i) = New BitSet : avAirLettersLit(i).Capacity = 8
		Set avAirLettersHit(i) = New BitSet : avAirLettersHit(i).Capacity = 8
		Set avCyber1TargetsHit(i) = New BitSet : avCyber1TargetsHit(i).Capacity = 10
		Set avMayhemTargets(i) = New BitSet : avMayhemTargets(i).Capacity = 12
		for j = 0 to 28
			Set aavShotsLit(i,j) = New BitSet : aavShotsLit(i,j).Capacity = 8
		Next
	Next
	set vSuperJPsLit = New BitSet : vSuperJPsLit.Capacity = 6
	set vCyberJackpots = New BitSet : vCyberJackpots.Capacity = 8
	set vAirMultipliers = New BitSet : vAirMultipliers.Capacity = 8
	set vIceJackpots = New BitSet : vIceJackpots.Capacity = 8
	Set oQueueTextTop = New Queue
	Set oQueueTextBottom = New Queue
	Set oQueueTextDuration = New Queue
	Set oQueueStatusTop = New Queue
	Set oQueueStatusBottom = New Queue
	Set oQueueStatusDuration = New Queue
	Set oQueueSecretTop = New Queue
	Set oQueueSecretBottom = New Queue
	Set oQueueSecretValue = New Queue
	LightSeq1.Play SeqCircleOutOn, 40
	nLastTurntableUpdate = 0
	bTurnClockwise = True
	TimerDisplay.enabled = True
	InitVoiceovers
	nAttractModePage = 0
	InitDisplay
	If bEnableFlexDMD then InitFlexAlpha
	Loadhs
End Sub

Sub Table1_Exit
    If B2SOn Then Controller.Stop
	If Not bEnableFlexDMD Then Exit Sub
    If Not oFlexAlpha is Nothing Then
		oFlexAlpha.Show = False
		oFlexAlpha.Run = False
		oFlexAlpha = NULL
    End If
End Sub

Sub InitGame(bFastRestart)
	Dim i, j
	anScore = Array(0, 0, 0, 0)
	anBonus	= Array(0, 0, 0, 0)
	nPlayersInGame = 1
	nPlayer = 0
	nBall = 1
	nExtraBalls = 0
	nAutoPlungeBalls = 0
	nBallsToFeed = 0
	nReleaseDelay = 0
	anExtraBallsLit = Array(0, 0, 0, 0)
	abXLit = Array(False, False, False, False)
	nFlashSweepStep = -1
	nFlashColor = eColorWhite
	nFlashRepeats = 0
	bBallAtPlunger = bFastRestart
	nInstantInfoPage = 0
	set vBumperLanes = New BitSet : vBumperLanes.Capacity = 3
	sMusicTrack = ""
	sLastTargetSFX = ""
	nTextDuration = 0
	nLastJackpot = eMballCyber
	sCurrentCallout = ""
	nCalloutTimeLeft = 0
	nTimeDelayCallout = 0
	nComboNoCallout = 0
	nTextDuration = 0

	for i = 0 to 3
		for j = 0 to 5
			aanLocksLit(i, j) = 0
			aanLockProgress(i, j) = 0
			aanLocksMade(i, j) = 0
			aanMballsPlayed(i, j) = 0
			aanMballJackpots(i, j) = 0
		Next
		for j = 0 to 2 : aanLastBagAwards(i, j) = eMysteryNone : next
		for j = 0 to 7 : aanCyber3ShotsHit(i, j) = 0 : next
		for j = 0 to 28 : aanModeShots(i, j) = 0 : next
		for j = 0 to 28 : aavShotsLit(i, j).Clear : Next
		avDropsHit(i).Clear
		avModesPlayed(i).Clear
		avSJCollected(i).Clear
		avPermaCombo(i).Clear
		avModesRunning(i).Clear
		avWizardModesLit(i).Clear
		avFireLanes(i).Clear
		avXtargets(i).Clear
		avBagTargets(i).Clear
		anBagLevelSum(i) = 0
		anBagAwards(i) = 0
		abBagExtraBall(i) = False
		abModeExtraBall(i) = False
		anBagLevel(i) = 1
		anEarthJackpotsL(i) = 0
		anEarthJackpotsR(i) = 0
		anMballExtenders(i) = 0
		anWizardModeRunning(i) = eModeNone
		avAirLettersHit(i).Clear
		avAirLettersLit(i).Clear
		For j = 0 to 3 : avAirLettersLit(i).Add j : Next
		nTimeAir3Move = 0
		' Because of the torpedos and their mechanic of being lit at the 
		' least recently hit shot, I init the shots to different times
		aanShotLastHit(i, eShotUnderFlipper) = 1
		aanShotLastHit(i, eShotLeftLoop) = 3
		aanShotLastHit(i, eShotSaucer) = 7
		aanShotLastHit(i, eShotLeftRamp) = 3
		aanShotLastHit(i, eShotEarth) = 6
		aanShotLastHit(i, eShotSideRamp) = 0
		aanShotLastHit(i, eShotRightRamp) = 4
		aanShotLastHit(i, eShotRightLoop) = 2
	Next
	If False = bFastRestart Then
		anSwitchLastHit(eSwitchLock1) = -1
		anSwitchLastHit(eSwitchLock2) = -1
		anSwitchLastHit(eSwitchLock3) = -1
		bTurnClockwise = False
		TimerTurntable_Timer
	End If
	TimerLockCount.Enabled = True
	TimerBlinkFast.Enabled = True
	TimerBlinkChars.Enabled = True
	TimerBlinkMedium.Enabled = True
	TimerBlinkSlow.Enabled = True
	LightSeq1.StopPlay
	InitBall bFastRestart
End Sub

Sub InitBall(bFastRestart)
	Dim i, bIsWizard

	AllLightsOff
	StopAllMusic
	oQueueShotVO.Clear
	oQueuePlayVO.Clear
	If sCurrentCallout <> "" And Right(sCurrentCallout, 5) <> "_tilt" Then
		StopSound sCurrentCallout
		nCalloutTimeLeft = 0
	End If
	nCharSpeaking = eCharacterNone
	nCharBlinkCycle = 0
	nLastCharacter = eCharacterWater
	nLightShowCurrent = eLightShowNone
	nLightShowPriority = eVOPrioIdle
	nLightShowTime = 0
	bLightShowBlinkOdd = False
	bVOQueueLocked = False
	bDrainingBalls = False
	nTiltWarnings = 0
	nTiltLevel = 0
	bCountingBonus = False
	nBonusX = 1
	anBonus(nPlayer) = 0
	nPlayfieldX = 1
	nPlayfieldXTimer = 0
	BIP = 1
	bFirstSwitchHit = False
	bBallHeld = False
	nTimeBagAnimation = 0
	nTimeDelaySaucerUpper = 0
	nTimeDelaySaucerLower = 0
	bDelayBagOfTricks = False
	nTimePFMagnet = 0
	nTimeMagnetSound = 0
	nTimeEarth3Open = 0
	anBumpersForBag(nPlayer) = BUMPERS_TO_SPOT_TARGET + 5 * (anBagLevel(nPlayer))
	sSFXToPlay = ""
	nSFXPriority = 99
	fSFXQueuedVolume = 0
	sJinglePlaying = ""
	KickerAutoPlunge.enabled = False
	TimerMode.Enabled = True
	nCyber3AttackTime = cCyber3AttackInterval
	If False = bFastRestart Then
		nBallsToFeed = nBallsToFeed + 1
		UpdateTrough
	End If
	For i = 0 to 7
		avModeColors(i).Clear
		avMballColors(i).Clear
		anTimerTorpedo(i) = 0
		RemoveMballColor i, eColorBlue
		anComboTimer(i) = 0
		If avPermaCombo(nPlayer).Contains(i) Then
			anComboX(i) = 2
		Else
			anComboX(i) = 1
		End If
	Next
	For i = 0 to 5
		anTimerMballGrace(i) = 0
		anBonusElements(i) = 10
	Next
	nTimeDelayMultiball = 0
	For i = 0 to 28 : anModeScore(i) = 0 : Next
	vSuperJPsLit.Clear
	bReleasingLocks = False
	nTimeLeftRampPost = 0 : UpPostMove LeftRampPost,LeftRampPostPrimitive,"down"																																								
	nTimeRightRampPost = 0 : UpPostMove RightRampPost,RightRampPostPrimitive,"down"
	nTimeLoopPost = 0 : UpPostMove LoopPost,LoopPostPrimitive,"down"
	nTimeSkillPost = 0 : UpPostMove SkillPost,SkillPostPrimitive,"down"
	nDropResetTimer = 0
	If aanMballsPlayed(nPlayer, eMballIce) > 1 Then
		avDropsHit(nPlayer).Clear
	End If
	RaiseDrops
	UpdateLightsFireLanes
	UpdateLEDs SpinsForCyberLock - aanLockProgress(nPlayer, eMballCyber)
	vBumperLanes.Clear
	bSkillShotActive = True
	nSkillShotLane = 2
	UpdateBumperLanes eSwitchNone
	nModeStatusShowing = eModeNone
	nModeStatusPage = 0
	nFirstMBallStarted = eModeNone
	nTimeLastSwitch = -1
	bInMode = False
	bOutlaneSave = False
	For Each i in an1BallModes
		If avModesRunning(nPlayer).Contains(i) Then
			UpdateMode i, eShotNone, eSwitchNone
			Select Case i
				Case eModeCyber1, eModeCyber2
					nLastCharacter = eCharacterCyber
				Case eModeWater1, eModeWater2
					nLastCharacter = eCharacterWater
				Case eModeFire1, eModeFire2
					nLastCharacter = eCharacterFire
				Case eModeEarth1, eModeEarth2
					nLastCharacter = eCharacterEarth
				Case eModeAir1, eModeAir2
					nLastCharacter = eCharacterAir
				Case eModeIce1, eModeIce2
					nLastCharacter = eCharacterIce
			End Select
			bInMode = True
		End If
	Next
	bIsWizard = False	
	If avModesRunning(nPlayer).Contains(eModeAirWizard) Then nTimeAir3Move = 1500
	For Each i in anWizardModes
		If avModesRunning(nPlayer).Contains(i) Then
			UpdateMode i, eShotNone, eSwitchNone
			Select Case i
				Case eModeCyberWizard
					nLastCharacter = eCharacterCyber
				Case eModeWaterWizard
					nLastCharacter = eCharacterWater
				Case eModeFireWizard
					nLastCharacter = eCharacterFire
				Case eModeEarthWizard
					nLastCharacter = eCharacterEarth
				Case eModeAirWizard
					nLastCharacter = eCharacterAir
				Case eModeIceWizard
					nLastCharacter = eCharacterIce
			End Select
			bIsWizard = True
			bInMode = True
		End If
	Next
	If Not bIsWizard Then
		UpdateWizardModesLit
		If aanLocksLit(nPlayer, eMballCyber) > 0 Then
			For each i in aoDigit0Lights : i.state = 0 : i.state = 2 : Next
			For each i in aoDigit1Lights : i.state = 0 : i.state = 2 : Next
			For each i in aoDigit2Lights : i.state = 0 : i.state = 2 : Next
			AddMballColor eShotLeftLoop, eColorGreen
		End If
		If aanLocksLit(nPlayer, eMballFire) > 0 Then AddMballColor eShotLeftRamp, eColorRed
		If aanLocksLit(nPlayer, eMballEarth) > 0 Then AddMballColor eShotEarth, eColorOrange
		If aanLocksLit(nPlayer, eMballIce) > 0 Then AddMballColor eShotRightLoop, eColorWhite
		If aanLocksLit(nPlayer, eMballAir) > 0 Then AddMballColor eShotRightRamp, eColorPurple
		If aanLocksLit(nPlayer, eMballWater) > 0 Then AddMballColor eShotSaucer, eColorBlue
		If aanMballsPlayed(nPlayer, eMballEarth) > 1 _
		and aanLocksLit(nPlayer, eMballEarth) > 0 _
		and aanLocksMade(nPlayer, eMballEarth) < 2 _
		and nBallPhysLocked < 2 then
			bTurnClockwise = True
		Else
			bTurnClockwise = False
		End If
		TimerTurntable_Timer
	End If
	If avModesRunning(nPlayer).Contains(eModeEarthWizard) Then
		bTurnClockwise = False
		TimerTurntable_Timer
	End If
	If not bInMode Then
		for each i in an1BallModes
			If avModesPlayed(nPlayer).Contains(i) Then
				aoLightForMode(i).state = LightStateOn
			End If
		Next
		bSelectingMode = True
		nTimeModeSelectVO = 12000
		nModeSelected = an1BallModes(Int(Rnd * 12))
		ModeSelectNext
	End If
	If bIsExtraBall Then
		AddCallout "VO_hero_shoot_again", eVOPrioNewPlayer, eCharacterHero
		bIsExtraBall = False
	Elseif nPlayersInGame > 1 or 1 = nBall Then
		AddCallout "VO_hero_player" & (nPlayer + 1) & "_", eVOPrioNewPlayer, eCharacterHero
	End If
	PlayModeMusic
End Sub

Sub Plunger_Init()
End Sub

Sub TimerRestart_Timer
	If nPlayersInGame > 0 And nBall > 1 And True = bBallAtPlunger And BIP < 2 Then
		If nTimeStartPressed > 0 And ((GameTime - nTimeStartPressed) > 1999) Then
			InitGame True
		End If
	End If
End Sub

Sub AutoPlunge(nBalls)
	If nAutoPlungeBalls < 1 Then
		nAutoPlungeBalls = nAutoPlungeBalls + nBalls
		KickerAutoPlunge.enabled = True
		AutoPlungePull
	Else
		nAutoPlungeBalls = nAutoPlungeBalls + nBalls
	End If
End Sub

Sub AutoPlungePull
	If nAutoPlungeBalls > 0 Then
		nBallsToFeed = nBallsToFeed + nAutoPlungeBalls
		If nBallsToFeed > 5 Then nBallsToFeed = 6
		nAutoPlungeBalls = 0
		UpdateTrough
'		Plunger.PullBack
'		vpmTimer.AddTimer 1000, "AutoPlungeFire '"
	End If
End Sub

Sub AutoPlungeFire
	Plunger.Fire
	If nAutoPlungeBalls > 0 Then
		vpmTimer.AddTimer 500, "AutoPlungePull '"
	Else
		nAutoPlungeBalls = 0
	End If
End Sub

Sub KickerAutoPlunge_Hit
	KickerAutoPlunge.TimerEnabled = True
End Sub

Sub KickerAutoPlunge_Timer
	KickerAutoPlunge.TimerEnabled = False
	DOF 121, DOFPulse
	DOF 122, DOFPulse
	RandomSoundAutoPlunge
	If Not bFirstSwitchHit Then
		KickerAutoPlunge.kick 0, 55
	Else
		KickerAutoPlunge.kick 0, 48
	End If
	If (not avModesRunning(nPlayer).Contains(eModeTotalMayhem)) and bFirstSwitchHit Then
		UpPostMove LoopPost,LoopPostPrimitive,"up"
		nTimeLoopPost = 1600
	End If
End Sub

Sub KickerLeft_Hit
	if (BIP<2) Then DOF 242, DOFOn							   
	KickerLeft.TimerEnabled = True
	nTimeDelaySaucerUpper = 600
    PlaySoundAtVol "BlastZone_Hit" & Int(rnd*3), ActiveBall, 1 * fMechSoundVolume
	bBallHeld = True
	ShotHit eShotSaucer
	anBonusElements(eMBallWater) = anBonusElements(eMBallWater) + 190
	anScore(nPlayer) = anScore(nPlayer) + (490 * nPlayfieldX * anComboX(eShotSaucer))
	AnySwitchHit eSwitchKickerTop
End Sub

Sub KickerLeft_Timer
	If nTimeDelaySaucerUpper < 2 Then
		If aanLocksMade(nPlayer, eMBallWater) > 2 Then
			WaterMBallStart
		Else
			KickerLeft.TimerEnabled = False
			KickerLeft.kickZ 260, 7, 0, 10
			bBallHeld = False
			PlaySoundAtVol SoundFXDOF("StartCity_SolOut",125,DOFPulse,DOFContactors), KickerLeft, fMechSoundVolume
			DOF 242, DOFOff				  
		End If
	Else
		If nTimeDelaySaucerUpper > 0 and nTimeDelaySaucerUpper < 600 _
		And aanLocksMade(nPlayer, eMBallWater) < 3 then
			FlashOnce 2, eColorWhite
		end If
		nTimeDelaySaucerUpper = nTimeDelaySaucerUpper - KickerLeft.TimerInterval
		If nTimeDelaySaucerUpper < 0 then nTimeDelaySaucerUpper = 0
	End If
End Sub

Sub KickerShatz_Hit
	if (BIP<2) Then DOF 240, DOFOn							   
	KickerShatz.TimerEnabled = True
    PlaySoundAt "SaucerHit", KickerShatz
	bBallHeld = True
	anSwitchLastHit(eSwitchKickerBottom) = GameTime
	ShotHit eShotUnderFlipper
	AnySwitchHit eSwitchKickerBottom
	anBonusElements(eMBallWater) = anBonusElements(eMBallWater) + 190
	anScore(nPlayer) = anScore(nPlayer) + (490 * nPlayfieldX * anComboX(eShotUnderFlipper))
End Sub

Sub KickerShatz_Timer
	If nTimeDelaySaucerLower > 0 Then
		If (nTimeDelaySaucerLower > 0) and (nTimeDelaySaucerLower < 900) _
		And (Not bDelayBagOfTricks) then
			FlashOnce 1, eColorWhite
		end If
		nTimeDelaySaucerLower = nTimeDelaySaucerLower - KickerShatz.TimerInterval
		If nTimeDelaySaucerLower < 1 then
			nTimeDelaySaucerLower = 0
			If bDelayBagOfTricks Then
				bDelayBagOfTricks = False
				AwardMystery False
			End If
		End If
	End If
	If nTimeBagAnimation < 1 and nTimeDelaySaucerLower < 1 Then
		KickerShatz.TimerEnabled = False
		KickerShatz.kickZ 180, 5, 0, 25
		bBallHeld = False
		PlaySoundAtVol SoundFXDOF("SafeHouseKick", 124, DOFPulse, DOFContactors), KickerShatz, fMechSoundVolume
		DOF 240, DOFOff				 
        DOF 124, DOFPulse
	End If
End Sub

Sub WallTurn1_Hit
	DOFGadget 130, DOFPulse, DOFTargets
	PlayTargetSound
	If False = avModesRunning(nPlayer).Contains(eModeEarthMBall) _
	And False = IsWizardModeActive Then
		If nMagnetPulses < 1 And TimerMagnetCooldown.enabled = False Then
			If aanLocksLit(nPlayer, eMballEarth) > 0 Then
					nMagnetPulses = 1
					bLockBall = True
			Else
				nMagnetPulses = 2
			End If
			TimerMagnetOn.enabled = True
		End If
	End If
	If GameTime - anSwitchLastHit(eSwitchTargetEarth1) > TimerMagnetCooldown.interval Then
		anSwitchLastHit(eSwitchTargetEarth1) = GameTime
		If Not TimerMagnetCooldown.enabled Then ShotHit eShotEarth
	End if
	If Not TimerMagnetCooldown.enabled Then AnySwitchHit eSwitchTargetEarth1
	anBonusElements(eMballEarth) = anBonusElements(eMballEarth) + 190
	anScore(nPlayer) = anScore(nPlayer) + (490 * nPlayfieldX * anComboX(eShotEarth))
End Sub

Sub WallTurn2_Hit
	DOFGadget 130, DOFPulse, DOFTargets
	PlayTargetSound
end sub

Sub TimerMagnetOn_Timer
	PlaySound "Magnet_grab", 0, fMechSoundVolume
	If aanLocksLit(nPlayer, eMballEarth) > 0 Then
		If False = bPullingBall Then
			oMagnetA.GrabCenter = True
			TimerMagnetOff.Interval = 1200
			If nBallPhysLocked < 2 or 2 = aanLocksMade(nPlayer, eMballEarth) then
				If BIP < 2 Then TimerLock.Enabled = True
			End If
			bMagnetDroppedBall = False
			TimerMagnetCooldown.enabled = True
		End If
	Else 
		oMagnetA.GrabCenter = False
		If 2 = nMagnetPulses Then
			TimerMagnetOff.Interval = 300
		Else
			TimerMagnetOff.Interval = 100
		End If
	End If
	oMagnetA.MagnetOn = True
	TimerMagnetOn.enabled = False
	TimerMagnetOff.enabled = True	
End Sub

Sub TimerMagnetOff_Timer									 
	oMagnetA.MagnetOn = False
	oMagnetA.Strength = 30
	nMagnetPulses = nMagnetPulses - 1
	If nMagnetPulses < 1 Then
		nMagnetPulses = 0
		oMagnetA.GrabCenter = False
		TimerMagnetOn.enabled = False
		If aanLocksLit(nPlayer, eMballEarth) > 0 _
		And (Not IsWizardModeActive) Then
			If nBallPhysLocked > 1 Or BIP > 1 Then
				TimerMagnetOn.interval = 20
				TimerMagnetOff.enabled = False
				LockBallEarth False
			Elseif not bPullingBall Then
'				bPullingBall = True
'				nMagnetPulses = 1
'				TimerMagnetOn.interval = 250
'				TimerMagnetOff.interval = 65
'				oMagnetA.Strength = 30
'				TimerMagnetOn.enabled = True
				vpmTimer.addTimer 250, "FakeMagnetPull '"
				TimerMagnetOff.enabled = False
			Else
				bPullingBall = False
				TimerMagnetOn.interval = 20
				TimerMagnetOff.enabled = False
			End If
		Else
			TimerMagnetOn.interval = 20
			TimerMagnetOff.interval = 100
			TimerMagnetOff.enabled = False
			TimerMagnetCooldown.enabled = True
		End If
		Exit Sub
	Else
		TimerMagnetOn.interval = 100
		TimerMagnetOff.interval = 150 + CInt(75 * Rnd)
		TimerMagnetOn.enabled = True
		TimerMagnetOff.enabled = False		
	End If
End Sub

Sub FakeMagnetPull
	Dim ball, nCount
	nCount = 0
	bBallHeld = False
	For Each ball in oMagnetA.Balls
		nCount = nCount + 1
		ball.VelY = -19
	Next
	If nCount < 1 Then
		bTurnClockwise = False
		TimerTurntable_Timer
		LockBallEarth False
	End If
End Sub

Sub TimerMagnetCooldown_Timer
	TimerMagnetCooldown.enabled = False
	bPullingBall = False
	If bLockBall Then
		bLockBall = False
	Else
		EarthAdvanceLock
	End If
'	Msgbox "event"
End Sub

Sub TimerLock_Timer
	If Not bMagnetDroppedBall Then
		TimerTurntable_timer
	Else
		oMagnetA.MagnetOn = False
		oMagnetA.Strength = 30
		TimerMagnetOn.interval = 10
		TimerMagnetOff.interval = 100
		TimerMagnetOn.enabled = False
		TimerMagnetOff.enabled = False
		bPullingBall = False
		nMagnetPulses = 0
	End If
	TimerLock.enabled = False
End Sub

Sub TimerLockCount_Timer
	Dim nCount
'	sTimersRun = sTimersRun & " TimerLockCount"
	nCount = 0
	' The triggers can undercount the balls in the lock, but cannot overcount,
	' so use the highest reading
	If anSwitchLastHit(eSwitchLock1) > -1 _
	And (GameTime - anSwitchLastHit(eSwitchLock1)) > 500 Then
		nCount = nCount + 1
	End If
	If anSwitchLastHit(eSwitchLock2) > -1 _
	And (GameTime - anSwitchLastHit(eSwitchLock2)) > 500 Then
		nCount = nCount + 1
	End If
	If anSwitchLastHit(eSwitchLock3) > -1 _
	And (GameTime - anSwitchLastHit(eSwitchLock3)) > 500 Then
		nCount = nCount + 1
	End If
	If False = bReleasingLocks and nCount > nBallPhysLocked Then
		If avModesRunning(nPlayer).Contains(eModeEarthWizard) Then
			UpPostMove LockPost,LockPostPrimitive,"down"
			LockPostTimer.Enabled = True
			bReleasingLocks = True
		End If
		If Not (avModesRunning(nPlayer).Contains(eModeWaterWizard) _
		Or avModesRunning(nPlayer).Contains(eModeEarthWizard) _
		Or avModesRunning(nPlayer).Contains(eModeEvilUnited)) Then
			nBallPhysLocked = nCount
			LockBallEarth True
			If aanMballsPlayed(nPlayer, eMballEarth) > 1 Then ShotHit eShotEarth
		Elseif avModesRunning(nPlayer).Contains(eModeEarthWizard) Then
			EarthMode3Update eShotNone, eSwitchLock1
		Else
			ShotHit eShotEarth
		End If
	End If
End Sub

Sub TriggerStuckBall_Hit
	nTimeBallStuck = GameTime
	TimerStuckBall.enabled = True
End Sub

Sub TriggerStuckBall_UnHit
	nTimeBallStuck = -1
	TimerStuckBall.enabled = False
End Sub

Sub TimerStuckBall_Timer
'	sTimersRun = sTimersRun + " TimerStuckBall"
	If nTimeBallStuck > 0 and GameTime - nTimeBallStuck > 3000 Then
		bTurnClockwise = True : TimerTurntable_Timer
		vpmTimer.AddTimer 1100, "bTurnClockwise = False : TimerTurntable_Timer '"
	End If
End Sub

Sub Bumper1_Hit
	DOFGadget 109,DOFPulse,DOFContactors
	RandomSoundBumperUp Bumper1
	DOF 110, DOFPulse
	PlaySound "SFX_jetbumper" & Int(rnd*4), 0, 0.6 * fSFXVolume
	AnySwitchHit eSwitchBumper1
	AnyBumperHit
	FlBumperFadeTarget(2) = 1
	Bumper1.timerenabled = True
	nTimeBumperHit = GameTime
End Sub

Sub Bumper1_Timer
	FlBumperFadeTarget(2) = 0
	Bumper1.timerenabled = False
End Sub

Sub Bumper2_Hit
	DOFGadget 107,DOFPulse,DOFContactors
	RandomSoundBumperLeft Bumper2
	DOF 108, DOFPulse
	PlaySound "SFX_jetbumper" & Int(rnd*4), 0, 0.6 * fSFXVolume
	AnySwitchHit eSwitchBumper2
	AnyBumperHit
	FlBumperFadeTarget(1) = 1
	Bumper2.timerenabled = True
	nTimeBumperHit = GameTime
End Sub

Sub Bumper2_Timer
	FlBumperFadeTarget(1) = 0
	Bumper2.timerenabled = False
End Sub

' #####################################
' ###### Flupper Bumpers 		  #####
' #####################################

' prepare some global vars to dim/brighten objects when using day-night slider
Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0 : DNA45 = (NightDay-10)/20 : DNA90 = 0 : DayNightAdjust = 0.4
Else
	DNA30 = (NightDay-10)/30 : DNA45 = (NightDay-10)/45 : DNA90 = (NightDay-10)/90 : DayNightAdjust = NightDay/25
End If

Dim FlBumperFadeActual(2), FlBumperFadeTarget(2), FlBumperColor(2), FlBumperTop(2), FlBumperSmallLight(2), Flbumperbiglight(2)
Dim FlBumperDisk(2), FlBumperBase(2), FlBumperBulb(2), FlBumperscrews(2), FlBumperActive(2), FlBumperHighlight(2)
Dim cnt : For cnt = 1 to 2 : FlBumperActive(cnt) = False : Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight

FlInitBumper 1, "green"
FlInitBumper 2, "green"

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
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z^3) / (0.5 + DNA90)

		Case "green"	
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(16 + 16 * sin(Z*3.14),255,16 + 16 * sin(Z*3.14)), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 2.5 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z^3) / (1 + DNA90)
		
		Case "red" 
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 16 - 11*Z + 16 * sin(Z*3.14),0), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)	'20 -> 10. was too bright
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
'	sTimersRun = sTimersRun + " BumperTimer"
	For nr = 1 to 2
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

Sub LeftBumperLight(state)
	If state = 0 then
		FlBumperFadeTarget(1) = 0
	elseif state = 1 Then
		FlBumperFadeTarget(1) = 0.7
	end if
end sub

Sub RightBumperLight(state)
	If state = 0 then
		FlBumperFadeTarget(2) = 0
	elseif state = 1 Then
		FlBumperFadeTarget(2) = 0.7
	end if
end sub

LeftBumperLight 1
RightBumperLight 1

Sub TopSlingShot_Slingshot
	DOFGadget 111,DOFPulse,DOFContactors
	RandomSoundSlingshotUpper
	PlaySound "SFX_jetbumper" & Int(rnd*4), 0, 0.6 * fSFXVolume
	DOF 112, DOFPulse
	AnySwitchHit eSwitchTopSlinghot
	AnyBumperHit
	nTimeBumperHit = GameTime
    USling.Visible = 0
    USling1.Visible = 1
    sling001.rotX = 20
    UStep = 0
    TopSlingShot.TimerEnabled = 1
End Sub

Dim UStep : uStep = 0

Sub TopSlingShot_Timer
    Select Case UStep
        Case 3:USLing1.Visible = 0:USLing2.Visible = 1:sling001.rotX = 10
        Case 4:USLing2.Visible = 0:USLing.Visible = 1:sling001.rotX = 0:TopSlingShot.TimerEnabled = 0
    End Select
    UStep = UStep + 1
End Sub

Sub AnyBumperHit
	Dim sText, i

	anBonusElements(eMballIce) = anBonusElements(eMballIce) + 10
	anScore(nPlayer) = anScore(nPlayer) + (30 * nPlayfieldX)
	If anBagLevel(nPlayer) < 9 And BIP < 2 Then
		anBumpersForBag(nPlayer) = anBumpersForBag(nPlayer) - 1
		If anBumpersForBag(nPlayer) < 1 Then
			For i = 0 to 3
				If Not avBagTargets(nPlayer).Contains(i) Then
					avBagTargets(nPlayer).add i
					Exit For
				End If
			Next
			If avBagTargets(nPlayer).IsFull Then
				avBagTargets(nPlayer).Clear
				anBagLevel(nPlayer) = anBagLevel(nPlayer) + 1
				ShowText "BAG OF TRICKS", "LEVEL " & anBagLevel(nPlayer) & " LIT", 2000, eVOPrioMystery
				PlayJingle "SFX_mystery_lit_" & anBagLevel(nPlayer), 1400
			Else
				ShowText "BAG OF TRICKS", "TARGET SPOTTED", 2000, eVOPrioModeSelect
			End If
			anBumpersForBag(nPlayer) = BUMPERS_TO_SPOT_TARGET + 5 * (anBagLevel(nPlayer))
		Else
			sText = anBumpersForBag(nPlayer) & " SPOTS TARGET"
			ShowText sText, WilliamsFormatNum(anScore(nPlayer)), 1000, eVOPrioCountdown
		End If
	End If
End Sub

Sub TriggerMagnetDrop_Hit
	bMagnetDroppedBall = True
End Sub

Sub TriggerLock1_Hit
	anSwitchLastHit(eSwitchLock1) = GameTime
End Sub

Sub TriggerLock1_UnHit
	anSwitchLastHit(eSwitchLock1) = -1
End Sub

Sub TriggerLock2_Hit
	anSwitchLastHit(eSwitchLock2) = GameTime
End Sub

Sub TriggerLock2_UnHit
	anSwitchLastHit(eSwitchLock2) = -1
End Sub

Sub TriggerLock3_Hit
	anSwitchLastHit(eSwitchLock3) = GameTime
End Sub

Sub TriggerLock3_UnHit
	anSwitchLastHit(eSwitchLock3) = -1
End Sub

Sub PulseMagnets (mag1, mag2, mag3)
	Dim sFlashes(2)

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		nTimePFMagnet = 0
		Exit Sub
	End If
	If "oMagnetB" = mag1 Then
		sFlashes(0) = "FlashOnce 3, eColorGreen"
	Elseif "oMagnetC" = mag1 Then
		sFlashes(0) = "FlashOnce 2, eColorGreen"
	Else
		sFlashes(0) = "FlashOnce 5, eColorGreen"
	End If
	If "oMagnetB" = mag2 Then
		sFlashes(1) = "FlashOnce 3, eColorGreen"
	Elseif "oMagnetC" = mag2 Then
		sFlashes(1) = "FlashOnce 2, eColorGreen"
	Else
		sFlashes(1) = "FlashOnce 5, eColorGreen"
	End If
	If "oMagnetB" = mag3 Then
		sFlashes(2) = "FlashOnce 3, eColorGreen"
	Elseif "oMagnetC" = mag3 Then
		sFlashes(2) = "FlashOnce 2, eColorGreen"
	Else
		sFlashes(2) = "FlashOnce 5, eColorGreen"
	End If
	
	vpmTimer.AddTimer 300, "EnableMagnet " & mag1 & ", True : " & sFlashes(0) & " '"
	vpmTimer.AddTimer 350, "EnableMagnet " & mag1 & ", False '"
	vpmTimer.AddTimer 350, "EnableMagnet " & mag2 & ", True : " & sFlashes(1) & " '"
	vpmTimer.AddTimer 400, "EnableMagnet " & mag2 & ", False '"
	vpmTimer.AddTimer 400, "EnableMagnet " & mag3 & ", True : " & sFlashes(2) & " '"
	vpmTimer.AddTimer 450, "EnableMagnet " & mag3 & ", False '"
	vpmTimer.AddTimer 500, "EnableMagnet " & mag1 & ", True : " & sFlashes(0) & " '"
	vpmTimer.AddTimer 550, "EnableMagnet " & mag1 & ", False '"
	vpmTimer.AddTimer 600, "EnableMagnet " & mag2 & ", True : " & sFlashes(1) & " '"
	vpmTimer.AddTimer 650, "EnableMagnet " & mag2 & ", False '"
	vpmTimer.AddTimer 650, "EnableMagnet " & mag3 & ", True : " & sFlashes(2) & " '"
	vpmTimer.AddTimer 700, "EnableMagnet " & mag3 & ", False '"
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		vpmTimer.AddTimer 1200, "nTimePFMagnet = 0 '"
	Else
		vpmTimer.AddTimer 2000, "nTimePFMagnet = 0 '"
	End If
End Sub

Sub EnableMagnet (oMagnet, bEnabled)
	Dim nCount, i
	oMagnet.MagnetOn = bEnabled
	If bEnabled And ((GameTime - nTimeMagnetSound) > cMagnetSoundCooldown) _
	And (Not avModesRunning(nPlayer).Contains(eModeTotalMayhem)) Then
		nCount = 0
		For Each i in oMagnet.Balls
			nCount = nCount + 1
		Next
		If nCount > 0 Then
			sSFXToPlay = "sfx_electro" & RndInt(1, 4)
			fSFXQueuedVolume = 1
			nSFXPriority = eVOPrioStandup
			nTimeMagnetSound = GameTime
		End If
	End If
End Sub

Sub LockPostTimer_Timer
'	sTimersRun = sTimersRun + " LockPostTimer"
	If LockPost.IsDropped = False Then
		UpPostMove LockPost,LockPostPrimitive,"down"
		bReleasingLocks = True
	Else
		nBallPhysLocked = 0
		bReleasingLocks = False
		LockPostTimer.Enabled = False
		UpPostMove LockPost,LockPostPrimitive,"up"
		If False = avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
'		And False = avModesRunning(nPlayer).Contains(eModeEarthWizard) Then
			bTurnClockwise = False
			TimerTurntable_Timer
		End If
	End If
End Sub

Sub drop1_Dropped
	DOFGadget 131,DOFPulse,DOFDropTargets
	SoundDropTargetDrop drop1
	avDropsHit(nPlayer).add 0
	CheckDrops
	AnySwitchHit eSwitchDropI
	If avModesRunning(nPlayer).Contains(eModeIce2) Then
		UpdateMode eModeIce2, eShotNone, eSwitchDropI
	End If
	If avModesRunning(nPlayer).Contains(eModeEarth2) Then
		UpdateMode eModeEarth2, eShotNone, eSwitchDropI
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		UpdateMode eModeWaterWizard, eShotNone, eSwitchDropI
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		UpdateMode eModeTotalMayhem, eShotNone, eSwitchDropI
	End If
End Sub

Sub drop2_Dropped
	DOFGadget 132,DOFPulse,DOFDropTargets
	SoundDropTargetDrop drop2
	avDropsHit(nPlayer).add 1
	CheckDrops
	AnySwitchHit eSwitchDropC
	If avModesRunning(nPlayer).Contains(eModeIce2) Then
		UpdateMode eModeIce2, eShotNone, eSwitchDropC
	End If
	If avModesRunning(nPlayer).Contains(eModeEarth2) Then
		UpdateMode eModeEarth2, eShotNone, eSwitchDropC
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		UpdateMode eModeWaterWizard, eShotNone, eSwitchDropC
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		UpdateMode eModeTotalMayhem, eShotNone, eSwitchDropC
	End If
End Sub

Sub drop3_Dropped
	DOFGadget 133,DOFPulse,DOFDropTargets
	SoundDropTargetDrop drop3
	avDropsHit(nPlayer).add 2
	CheckDrops
	AnySwitchHit eSwitchDropE
	If avModesRunning(nPlayer).Contains(eModeIce2) Then
		UpdateMode eModeIce2, eShotNone, eSwitchDropE
	End If
	If avModesRunning(nPlayer).Contains(eModeEarth2) Then
		UpdateMode eModeEarth2, eShotNone, eSwitchDropE
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		UpdateMode eModeWaterWizard, eShotNone, eSwitchDropE
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		UpdateMode eModeTotalMayhem, eShotNone, eSwitchDropE
	End If
End Sub

Sub TriggerDrop1_Hit
	If GameTime - anSwitchLastHit(eSwitchDropI) > nDebounceTime Then
		anSwitchLastHit(eSwitchDropI) = GameTime
		If Not Drop1.IsDropped Then
			Drop1.IsDropped = True
		End If
	End If
End Sub

Sub TriggerDrop2_Hit
	If GameTime - anSwitchLastHit(eSwitchDropC) > nDebounceTime Then
		anSwitchLastHit(eSwitchDropC) = GameTime
		If Not Drop2.IsDropped Then
			Drop2.IsDropped = True
		End If
	End If
End Sub

Sub TriggerDrop3_Hit
	If GameTime - anSwitchLastHit(eSwitchDropE) > nDebounceTime Then
		anSwitchLastHit(eSwitchDropE) = GameTime
		If Not Drop3.IsDropped Then
			Drop3.IsDropped = True
		End If
	End If
End Sub

Sub CheckDrops
	Dim i, nNextShot, nLockNumber
	
	anBonusElements(eMBallIce) = anBonusElements(eMBallIce) + 60
	anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
	If avModesRunning(nPlayer).Contains(eModeCyber1) Then
		CyberMode1Update eShotNone, eSwitchDropI
	End If
	If nTimePFMagnet < 1 And (avModesRunning(nPlayer).Contains(eModeCyber2) _
	Or avModesRunning(nPlayer).Contains(eModeTotalMayhem) _
	Or (avModesRunning(nPlayer).Contains(eModeAirWizard)_
	  and aanModeShots(nPlayer, eModeAirWizard) > 74)) Then
		nTimePFMagnet = 2000
		PulseMagnets "oMagnetD", "oMagnetC", "oMagnetB"
	End If
	If (drop1.isDropped And drop2.isDropped And drop3.isDropped) or avDropsHit(nPlayer).IsFull Then
		avDropsHit(nPlayer).Clear
		Drop1.timerEnabled = True
		nDropResetTimer = 0
		nLockNumber = aanLocksLit(nPlayer, eMballIce) + aanLocksMade(nPlayer, eMballIce)
		If IsWizardModeActive Then
			' Do nothing
		ElseIf avModesRunning(nPlayer).Contains(eModeIceMBall) Then
			If vIceJackpots.Count < 7 Then
				ShowText "TWO ICE", "JACKPOTS ADDED", 2000, eVOPrioMballReady
			End If
			For i = 0 to 1
				nNextShot = (nLastIceJPAdded + 3) Mod 8
				If nNextShot > eShotEarth Then nNextShot = (nNextShot + 1) Mod 8
				nLastIceJPAdded = nNextShot
				vIceJackpots.add nNextShot
			Next
			IceMballUpdate eShotNone
		Elseif (0 = aanMballsPlayed(nplayer, eMballIce) and nLockNumber < 3) _
		Or (aanMballsPlayed(nplayer, eMballIce) > 0 And aanLocksLit(nPlayer, eMballIce) < 1) Then
			IceAdvanceLock
		end If
	Else
		Select Case aanMballsPlayed(nPlayer, eMballIce)
			Case 0, 1
				nDropResetTimer = 0
			Case 2
				nDropResetTimer = 18000
			Case Else
				nDropResetTimer = 12000
		End Select
	End If
End Sub

Sub Drop1_Timer
	RaiseDrops
	Drop1.timerEnabled = False
End Sub

Sub RaiseDrops
	If Drop1.isDropped Or Drop2.isDropped Or Drop1.isDropped Then
		DOFGadget 134,DOFPulse,DOFContactors
		RandomSoundDropTargetReset Drop2
	else PlaySoundAtVol "DropBankResetFull", Drop2, fMechSoundVolume
	End If
	nDropResetTimer = 0
	Drop1.isDropped = False : Drop2.isDropped = False : Drop3.isDropped = False
End Sub

Sub StandupBag1_hit
	TargetBouncer Activeball, 0.7
	DOFGadget 117,DOFPulse,DOFTargets
	PlayTargetSound
	AnySwitchHit eSwitchTargetM1
	CyanTargetHit 0
	If avModesRunning(nPlayer).Contains(eModeEarth2) Then
		UpdateMode eModeEarth2, eShotNone, eSwitchTargetM1
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		UpdateMode eModeWaterWizard, eShotNone, eSwitchTargetM1
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		UpdateMode eModeTotalMayhem, eShotNone, eSwitchTargetM1
	End If
	anBonusElements(eMballAir) = anBonusElements(eMballAir) + 60
	anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
End Sub

Sub StandupBag2_hit
	TargetBouncer Activeball, 0.7
	DOFGadget 118,DOFPulse,DOFTargets
	PlayTargetSound
	AnySwitchHit eSwitchTargetM2
	CyanTargetHit 1
	If avModesRunning(nPlayer).Contains(eModeEarth2) Then
		UpdateMode eModeEarth2, eShotNone, eSwitchTargetM2
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		UpdateMode eModeWaterWizard, eShotNone, eSwitchTargetM2
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		UpdateMode eModeTotalMayhem, eShotNone, eSwitchTargetM2
	End If
	anBonusElements(eMballAir) = anBonusElements(eMballAir) + 60
	anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
End Sub

Sub StandupBag3_hit
	TargetBouncer Activeball, 0.7
	DOFGadget 119,DOFPulse,DOFTargets
	PlayTargetSound
	AnySwitchHit eSwitchTargetM3
	CyanTargetHit 2
	If avModesRunning(nPlayer).Contains(eModeEarth2) Then
		UpdateMode eModeEarth2, eShotNone, eSwitchTargetM3
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		UpdateMode eModeWaterWizard, eShotNone, eSwitchTargetM3
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		UpdateMode eModeTotalMayhem, eShotNone, eSwitchTargetM3
	End If
	anBonusElements(eMballAir) = anBonusElements(eMballAir) + 60
	anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
End Sub

Sub StandupBag4_hit
	TargetBouncer Activeball, 0.7
	DOFGadget 120,DOFPulse,DOFTargets
	PlayTargetSound
	AnySwitchHit eSwitchTargetM4
	CyanTargetHit 3
	If avModesRunning(nPlayer).Contains(eModeEarth2) Then
		UpdateMode eModeEarth2, eShotNone, eSwitchTargetM4
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		UpdateMode eModeWaterWizard, eShotNone, eSwitchTargetM4
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		UpdateMode eModeTotalMayhem, eShotNone, eSwitchTargetM4
	End If
	anBonusElements(eMballAir) = anBonusElements(eMballAir) + 60
	anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
End Sub

Sub CyanTargetHit(nTarget)
	Dim nHits

	If eSwitchNone <> nTarget Then
		anSwitchLastHit(anBagTargetSwitches(nTarget)) = GameTime
		aoBagLights(nTarget).BlinkInterval = 60
		aoBagLights(nTarget).state = LightStateBlinking

		If avBagTargets(nPlayer).Contains(nTarget) Then
			If nSFXPriority > eVOPrioStandup _
			And Left(sJinglePlaying, 15) <> "SFX_mystery_lit" Then
				StopSound sLastTargetSFX
				sLastTargetSFX = "SFX_mystery_target_lit"
				sSFXToPlay = "SFX_mystery_target_lit"
				fSFXQueuedVolume = 0.3
				nSFXPriority = eVOPrioStandup
			End If
		Else
			avBagTargets(nPlayer).Add nTarget
			nHits = avBagTargets(nPlayer).Count
			If nHits >= 1 and nHits <= 3 And nSFXPriority > eVOPrioStandup _
			And Left(sJinglePlaying, 15) <> "SFX_mystery_lit" then
				StopSound sLastTargetSFX
				sSFXToPlay = "SFX_mystery_target_" & nHits
				sLastTargetSFX = sSFXToPlay
				fSFXQueuedVolume = 0.5
				nSFXPriority = eVOPrioStandup
			End If
		End If

		' Bag of Tricks is disabled during Total Mayhem
		If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
			avBagTargets(nPlayer).Remove nTarget
		End If
		If avBagTargets(nPlayer).IsFull Then
			avBagTargets(nPlayer).Clear
			If anBagLevel(nPlayer) < 9 Then
				anBagLevel(nPlayer) = anBagLevel(nPlayer) + 1
				ShowText "BAG OF TRICKS", "LEVEL " & anBagLevel(nPlayer) & " LIT", 2000, eVOPrioMystery
				StopSound sLastTargetSFX
				PlayJingle "SFX_mystery_lit_" & anBagLevel(nPlayer), 1400
			End If
		End If
		If avModesRunning(nPlayer).Contains(eModeCyber1) Then
			CyberMode1Update eShotNone, eSwitchTargetM1
		End If
		If nTimePFMagnet < 1 And (avModesRunning(nPlayer).Contains(eModeCyber2) _
		Or avModesRunning(nPlayer).Contains(eModeTotalMayhem) _
		Or (avModesRunning(nPlayer).Contains(eModeAirWizard)_
		  and aanModeShots(nPlayer, eModeAirWizard) > 74)) Then
			If nTarget < 1 Then
				nTimePFMagnet = 2000
				PulseMagnets "oMagnetC", "oMagnetB", "oMagnetD"
			ElseIf nTarget = 1 Then
				nTimePFMagnet = 2000
				PulseMagnets "oMagnetB", "oMagnetC", "oMagnetD"
			Else
				nTimePFMagnet = 2000
				PulseMagnets "oMagnetD", "oMagnetC", "oMagnetB"
			End If
		End If
	End If
End Sub

Sub StandupX1_hit
	TargetBouncer Activeball, 0.7
	DOFGadget 113,DOFPulse,DOFTargets
	PlayTargetSound
	If 1 = nPlayfieldX Then
		If True = avXTargets(nPlayer).Contains(0) and False = avXTargets(nPlayer).Contains(1) Then
			UpdateXTargets 1
		End If
	End if
	UpdateXTargets 0
	AnySwitchHit eSwitchTargetX1
	If avModesRunning(nPlayer).Contains(eModeIce2) Then
		UpdateMode eModeIce2, eShotNone, eSwitchTargetX1
	End If
	If avModesRunning(nPlayer).Contains(eModeEarth2) Then
		UpdateMode eModeEarth2, eShotNone, eSwitchTargetX1
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		UpdateMode eModeWaterWizard, eShotNone, eSwitchTargetX1
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		UpdateMode eModeTotalMayhem, eShotNone, eSwitchTargetX1
	End If
	anBonusElements(eMballAir) = anBonusElements(eMballAir) + 60
	anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
	If nTimePFMagnet < 1 And (avModesRunning(nPlayer).Contains(eModeCyber2) _
	Or avModesRunning(nPlayer).Contains(eModeTotalMayhem) _
	Or (avModesRunning(nPlayer).Contains(eModeAirWizard)_
	  and aanModeShots(nPlayer, eModeAirWizard) > 74)) Then
		nTimePFMagnet = 2000
		PulseMagnets "oMagnetC", "oMagnetD", "oMagnetB"
	End If
End Sub

Sub StandupX2_hit
	TargetBouncer Activeball, 0.7
	DOFGadget 114,DOFPulse,DOFTargets
	PlayTargetSound
	If 1 = nPlayfieldX Then
		If True = avXTargets(nPlayer).Contains(1) and False = avXTargets(nPlayer).Contains(0) Then
			UpdateXTargets 0
		Else
			UpdateXTargets 1
		End If
	Elseif 2 = nPlayfieldX Then
		UpdateXTargets 1
	Elseif 3 = nPlayfieldX and True = avXTargets(nPlayer).Contains(0) Then
		UpdateXTargets 1
	End If
	AnySwitchHit eSwitchTargetX2
	If avModesRunning(nPlayer).Contains(eModeIce2) Then
		UpdateMode eModeIce2, eShotNone, eSwitchTargetX2
	End If
	If avModesRunning(nPlayer).Contains(eModeEarth2) Then
		UpdateMode eModeEarth2, eShotNone, eSwitchTargetX2
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		UpdateMode eModeWaterWizard, eShotNone, eSwitchTargetX2
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		UpdateMode eModeTotalMayhem, eShotNone, eSwitchTargetX2
	End If
	anBonusElements(eMballAir) = anBonusElements(eMballAir) + 60
	anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
	If nTimePFMagnet < 1 And (avModesRunning(nPlayer).Contains(eModeCyber2) _
	Or avModesRunning(nPlayer).Contains(eModeTotalMayhem) _
	Or (avModesRunning(nPlayer).Contains(eModeAirWizard)_
	  and aanModeShots(nPlayer, eModeAirWizard) > 74)) Then
		nTimePFMagnet = 2000
		PulseMagnets "oMagnetC", "oMagnetD", "oMagnetB"
	End If
End Sub

Sub StandupX3_hit
	TargetBouncer Activeball, 0.7
	DOFGadget 115,DOFPulse,DOFTargets
	PlayTargetSound
	If 1 = nPlayfieldX Then
		If True = avXTargets(nPlayer).Contains(2) and False = avXTargets(nPlayer).Contains(3) Then
			UpdateXTargets 3
		End If
	End If
	UpdateXTargets 2
	AnySwitchHit eSwitchTargetX3
	If avModesRunning(nPlayer).Contains(eModeIce2) Then
		UpdateMode eModeIce2, eShotNone, eSwitchTargetX3
	End If
	If avModesRunning(nPlayer).Contains(eModeEarth2) Then
		UpdateMode eModeEarth2, eShotNone, eSwitchTargetX3
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		UpdateMode eModeWaterWizard, eShotNone, eSwitchTargetX3
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		UpdateMode eModeTotalMayhem, eShotNone, eSwitchTargetX3
	End If
	anBonusElements(eMballAir) = anBonusElements(eMballAir) + 60
	anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
End Sub

Sub StandupX4_hit
	TargetBouncer Activeball, 0.7
	DOFGadget 116,DOFPulse,DOFTargets
	PlayTargetSound
	If 1 = nPlayfieldX Then
		If True = avXTargets(nPlayer).Contains(3) and False = avXTargets(nPlayer).Contains(2) Then
			UpdateXTargets 2
		Else
			UpdateXTargets 3
		End If
	Elseif 2 = nPlayfieldX Then
		UpdateXTargets 3
	Elseif 3 = nPlayfieldX and True = avXTargets(nPlayer).Contains(2) Then
		UpdateXTargets 3
	End If
	AnySwitchHit eSwitchTargetX4
	If avModesRunning(nPlayer).Contains(eModeIce2) Then
		UpdateMode eModeIce2, eShotNone, eSwitchTargetX4
	End If
	If avModesRunning(nPlayer).Contains(eModeEarth2) Then
		UpdateMode eModeEarth2, eShotNone, eSwitchTargetX4
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		UpdateMode eModeWaterWizard, eShotNone, eSwitchTargetX4
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		UpdateMode eModeTotalMayhem, eShotNone, eSwitchTargetX4
	End If
	anBonusElements(eMballAir) = anBonusElements(eMballAir) + 60
	anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
End Sub

Sub UpdateXTargets(nTarget)
	Dim nHits

	If avModesRunning(nPlayer).Contains(eModeCyber1) Then
		CyberMode1Update eShotNone, eSwitchTargetX1
	End If
	anSwitchLastHit(anXTargetSwitches(nTarget)) = GameTime
	aoXLights(nTarget).BlinkInterval = 60
	aoXLights(nTarget).state = LightStateBlinking

	If avXTargets(nPlayer).Contains(nTarget) Then
		If nSFXPriority > eVOPrioStandup _
		And sJinglePlaying <> "SFX_playfield_X_lit" Then
			StopSound sLastTargetSFX
			sSFXToPlay = "SFX_X_target_lit"
			sLastTargetSFX = sSFXToPlay
			fSFXQueuedVolume = 0.25
			nSFXPriority = eVOPrioStandup
		End If
	Else
		avXTargets(nPlayer).Add nTarget
		If avXTargets(nPlayer).IsFull Then
			StopSound sLastTargetSFX
			PlayJingle "SFX_playfield_X_lit", 2800
			Select Case nPlayfieldX
				Case 2
					ShowText "3X SCORING", "IS LIT", 2500, eVOPrioPlayfieldX
				Case 3
					ShowText "3X SCORING", "EXTENSION IS LIT", 2500, eVOPrioPlayfieldX
				Case Else
					ShowText "2X SCORING", "IS LIT", 2500, eVOPrioPlayfieldX
			End Select
			FlashSweep eColorYellow
		end If

		nHits = avXTargets(nPlayer).Count
		If nHits >= 1 and nHits <= 3 And nSFXPriority > eVOPrioStandup _
		And sJinglePlaying <> "SFX_playfield_X_lit" then
			StopSound sLastTargetSFX
			sSFXToPlay = "SFX_X_target_" & nHits
			sLastTargetSFX = sSFXToPlay
			fSFXQueuedVolume = 0.5
			nSFXPriority = eVOPrioStandup
		End If
	End If
End Sub

Sub StandupWater_Hit
	TargetBouncer Activeball, 0.7
	DOFGadget 135,DOFPulse,DOFTargets
	PlayTargetSound
	If False = IsWizardModeActive Then WaterAddShot
	If False = avModesRunning(nPlayer).Contains(eModeAirMBall) _
	And False = IsWizardModeActive Then
		If True = avAirLettersLit(nPlayer).Contains(3) _
		or aanMballsPlayed(nPlayer, eMballAir) > 2 Then
			AirAdvanceLock 3
		End If
	End If
	AnySwitchHit eSwitchTargetBlue
	anBonusElements(eMBallWater) = anBonusElements(eMBallWater) + 60
	anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
	If nTimePFMagnet < 1 And (avModesRunning(nPlayer).Contains(eModeCyber2) _
	Or avModesRunning(nPlayer).Contains(eModeTotalMayhem) _
	Or (avModesRunning(nPlayer).Contains(eModeAirWizard)_
	  and aanModeShots(nPlayer, eModeAirWizard) > 74)) Then
		nTimePFMagnet = 2000
		PulseMagnets "oMagnetB", "oMagnetC", "oMagnetB"
	End If
	If avModesRunning(nPlayer).Contains(eModeEarth2) Then
		UpdateMode eModeEarth2, eShotNone, eSwitchTargetBlue
	End If
	If avModesRunning(nPlayer).Contains(eModeCyber1) Then
		CyberMode1Update eShotNone, eSwitchTargetBlue
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		UpdateMode eModeWaterWizard, eShotNone, eSwitchTargetBlue
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		UpdateMode eModeTotalMayhem, eShotNone, eSwitchTargetBlue
	End If
End Sub

' lanes
Sub TriggerPlungerLane_Hit
	bBallAtPlunger = True
	DOF 202, DOFon			   
End Sub

Sub TriggerPlungerLane_UnHit
	bBallAtPlunger = False
	DOF 202, DOFOff
	DOF 203, DOFPulse
End Sub

Sub RolloverU1_Hit
	dim bIsLit
	If GameTime - anSwitchLastHit(eSwitchLaneTop1) > nDebounceTime Then
		anSwitchLastHit(eSwitchLaneTop1) = GameTime
		AnySwitchHit eSwitchLaneTop1
		if vBumperLanes.Contains(0) then
			bIsLit = true
		else
			bIsLit = false
		end if
		vBumperLanes.add 0
		if bIsLit = true and not vBumperLanes.IsFull and not nSkillShotLane = 0 then
			PlaySound "SFX_bonus_x_lane_miss",0,fSFXVolume,0,0,1,1,1
		elseif bIsLit = false and not vBumperLanes.IsFull and not nSkillShotLane = 0 then
			PlaySound "SFX_bonus_x_lane",0,fSFXVolume,0,0,1,1,1
		end if
		UpdateBumperLanes 0
	End if
End Sub

Sub RolloverU2_Hit
	dim bIsLit
	If GameTime - anSwitchLastHit(eSwitchLaneTop2) > nDebounceTime Then
		anSwitchLastHit(eSwitchLaneTop2) = GameTime
		AnySwitchHit eSwitchLaneTop2
		if vBumperLanes.Contains(1) then
			bIsLit = true
		else
			bIsLit = false
		end if
		vBumperLanes.add 1
		if bIsLit = true and not vBumperLanes.IsFull and not nSkillShotLane = 1 then
			PlaySound "SFX_bonus_x_lane_miss",0,fSFXVolume,0,0,1,1,1
		elseif bIsLit = false and not vBumperLanes.IsFull and not nSkillShotLane = 1 then
			PlaySound "SFX_bonus_x_lane",0,fSFXVolume,0,0,1,1,1
		end if
		UpdateBumperLanes 1
	End if
End Sub

Sub RolloverU3_Hit
	dim bIsLit
	If GameTime - anSwitchLastHit(eSwitchLaneTop3) > nDebounceTime Then
		anSwitchLastHit(eSwitchLaneTop3) = GameTime
		AnySwitchHit eSwitchLaneTop3
		if vBumperLanes.Contains(2) then
			bIsLit = true
		else
			bIsLit = false
		end if
		vBumperLanes.add 2
		if bIsLit = true and not vBumperLanes.IsFull and not nSkillShotLane = 2 then
			PlaySound "SFX_bonus_x_lane_miss",0,fSFXVolume,0,0,1,1,1
		elseif bIsLit = false and not vBumperLanes.IsFull and not nSkillShotLane = 2 then
			PlaySound "SFX_bonus_x_lane",0,fSFXVolume,0,0,1,1,1
		end if
		UpdateBumperLanes 2
	End if
End Sub

Sub UpdateBumperLanes(nLane)
	Dim i, nPoints
	If bSkillShotActive Then
		If nLane = eSwitchNone Then
			For Each i In aoTopLaneLights
				i.state = LightStateOff
			Next
			aoTopLaneLights(nSkillShotLane).state = LightStateBlinking
			Exit Sub
		Else
			bSkillShotActive = False
			If nSkillShotLane = nLane Then
				' TODO: sound
				anScore(nPlayer) = anScore(nPlayer) + 7500
				ShowText "SKILL SHOT ",	WilliamsFormatNum(7500), 2000, eVOPrioJackpot
				DOF 204, DOFPulse					 
				PlaySound "SFX_skill_shot_7.5k",0,fSFXVolume,0,0,1,1,1
				FlashSweep eColorWhite
				For Each i In aoTopLaneLights
					i.state = LightStateOff
				Next
			end If
		End If
	End If
	if vBumperLanes.IsFull Then
		If nBonusX < 10 Then
			nBonusX = nBonusX + 1
			ShowText "BONUS " & nBonusX & "X", "", 2000, eVOPrioCountdown
		Else
			nPoints = 15000 * nPlayfieldX
			anScore(nPlayer) = anScore(nPlayer) + nPoints
			ShowText "BONUS X MAXED", WilliamsFormatNum(nPoints), 2000, eVOPrioCountdown
		End If
		vBumperLanes.Clear
		lightTop001.timerEnabled = True
		lightTop001.state = LightStateBlinking
		lightTop002.state = LightStateBlinking
		lightTop003.state = LightStateBlinking
		PlaySound "SFX_bonus_x",0,fSFXVolume,0,0,1,1,1
	End if
	If lightTop001.timerEnabled = False then
		If vBumperLanes.Contains(0) Then
			lightTop001.state = LightStateOn
		Else
			lightTop001.state = LightStateOff
		End If
		If vBumperLanes.Contains(1) Then
			lightTop002.state = LightStateOn
		Else
			lightTop002.state = LightStateOff
		End If
		If vBumperLanes.Contains(2) Then
			lightTop003.state = LightStateOn
		Else
			lightTop003.state = LightStateOff
		End If
	End If
End Sub

Sub lightTop001_timer
	lightTop001.timerEnabled = False
	UpdateBumperLanes eSwitchNone
end Sub

Sub RolloverSkill_Hit
	If bDrainingBalls Then Exit Sub
	If GameTime - anSwitchLastHit(eSwitchSkillShot) > nDebounceTime Then
		anSwitchLastHit(eSwitchSkillShot) = GameTime
		AnySwitchHit eSwitchSkillShot
		anBonusElements(eMballFire) = anBonusElements(eMballFire) + 60
		anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
		If bSkillShotActive Then
			anScore(nPlayer) = anScore(nPlayer) + 15000
			ShowText "SKILL SHOT", WilliamsFormatNum(15000), 2500, eVOPrioJackpot
			PlaySound "SFX_skill_shot_15k",0,fSFXVolume,0,0,1,1,1
			FlashSweep eColorWhite
			bSkillShotActive = False
			DOF 204, DOFPulse					
		End If
	End if
End Sub

Sub RolloverF_Hit
	If bDrainingBalls Then Exit Sub
	If (aoFireLaneLights(0).state = LightStateOff) and (nTimeBallSave=0) Then DOF 205, DOFPulse				  
	If GameTime - anSwitchLastHit(eSwitchLeftOutlane) > nDebounceTime Then
		anSwitchLastHit(eSwitchLeftOutlane) = GameTime
		HandleFireRollover 0
		AnySwitchHit eSwitchLeftOutlane
		anBonusElements(eMballFire) = anBonusElements(eMballFire) + 60
		anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
		If nTimeBallSave > 0 and BIP < 2 and (not (bOutlaneSave or bDrainingBalls)) Then
			OutlaneSave
		End If
	End If
End Sub

Sub RolloverI_Hit
	If bDrainingBalls Then Exit Sub
	If (aoFireLaneLights(1).state = LightStateOff) Then DOF 206, DOFPulse				  
	If GameTime - anSwitchLastHit(eSwitchLeftInlane) > nDebounceTime Then
		leftInlaneSpeedLimit
		anSwitchLastHit(eSwitchLeftInlane) = GameTime
		HandleFireRollover 1
		AnySwitchHit eSwitchLeftInlane
		anBonusElements(eMballFire) = anBonusElements(eMballFire) + 60
		anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
	End if
End Sub

Sub RolloverR_Hit
	If bDrainingBalls Then Exit Sub
	If (aoFireLaneLights(2).state = LightStateOff) Then DOF 207, DOFPulse				  
	If GameTime - anSwitchLastHit(eSwitchRightInlane) > nDebounceTime Then
		rightInlaneSpeedLimit
		anSwitchLastHit(eSwitchRightInlane) = GameTime
		HandleFireRollover 2
		AnySwitchHit eSwitchRightInlane
		anBonusElements(eMballFire) = anBonusElements(eMballFire) + 60
		anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
	End If
End Sub

Sub RolloverE_Hit
	If bDrainingBalls Then Exit Sub
	If (aoFireLaneLights(3).state = LightStateOff) and (nTimeBallSave=0) Then DOF 208, DOFPulse				  
	If GameTime - anSwitchLastHit(eSwitchRightOutlane) > nDebounceTime Then
		anSwitchLastHit(eSwitchRightOutlane) = GameTime
		HandleFireRollover 3
		AnySwitchHit eSwitchRightOutlane
		anBonusElements(eMballFire) = anBonusElements(eMballFire) + 60
		anScore(nPlayer) = anScore(nPlayer) + (170 * nPlayfieldX)
		If nTimeBallSave > 0 and BIP < 2 and (not (bOutlaneSave or bDrainingBalls)) Then
			OutlaneSave
		End If
	End if
End Sub

Sub OutlaneSave
	AutoPlunge 1
	bOutlaneSave = True
	If nExtraBalls > 0 Then
		LightShootAgain.state = LightStateOn
	Else
		LightShootAgain.state = LightStateOff
	End If
	nTimeBallSave = 0
	AddCallout "VO_hero_ball_save", eVOPrioBallSave, eCharacterHero
	ShowText "BALL SAVED", "PLAYER " & (nPlayer + 1), 2500, eVOPrioBallSave
	DOF 218, DOFPulse				  
End Sub

Sub HandleFireRollover(nIndex)
	If aanMballsPlayed(nPlayer, eMballFire) > 1 Then
		If avFireLanes(nPlayer).Contains(nIndex) Then
			If 2 = aanMballsPlayed(nPlayer, eMballFire) and true = canAdvanceFire Then
				avFireLanes(nPlayer).Remove nIndex
			Elseif aanMballsPlayed(nPlayer, eMballFire) > 2 Then
				avFireLanes(nPlayer).Remove nIndex
				If aanLocksLit(nPlayer, eMballFire) > 0 Then
					aanLocksLit(nPlayer, eMballFire) = 0
					RemoveMballColor eShotLeftRamp, eColorRed
				End If
			End if
		Else
			avFireLanes(nPlayer).add nIndex
		End If
	Elseif canAdvanceFire Then
		avFireLanes(nPlayer).add nIndex
	End If
	UpdateLightsFireLanes
End Sub

Sub TriggerBallLaunch_Hit
	If Not bFirstSwitchHit Then
		AnySwitchHit eSwitchLoopRBottom
		anScore(nPlayer) = anScore(nPlayer) + 10
	End If
End Sub

Sub TriggerLoopLBottom_Hit
	If GameTime - anSwitchLastHit(eSwitchLoopLBottom) > nDebounceTime Then
		anSwitchLastHit(eSwitchLoopLBottom) = GameTime
		AnySwitchHit eSwitchLoopLBottom
		If vSuperJPsLit.Count < 1 Then
			TimerGateOpen.Enabled = True
			GateUpperLanesR.open = True
		End If
		DOF 209, DOFPulse				   
		If bSkillShotActive Then bSkillShotActive = False
	End if	
End Sub

Sub TriggerUpperLanesL_Hit
	If GameTime - anSwitchLastHit(eSwitchLoopLTop) > nDebounceTime Then
		anSwitchLastHit(eSwitchLoopLTop) = GameTime
		If anSwitchLastHit(eSwitchLoopLBottom) > -1 _
		and GameTime - anSwitchLastHit(eSwitchLoopLBottom) < 1500 Then
			ShotHit eShotLeftLoop
			anSwitchLastHit(eSwitchLoopLBottom) = -1
		End If
		DOF 210, DOFPulse				   
		AnySwitchHit eSwitchLoopLTop
	End if	
End Sub

Sub TriggerLoopRBottom_Hit
	If GameTime - anSwitchLastHit(eSwitchLoopRBottom) > nDebounceTime Then
		anSwitchLastHit(eSwitchLoopRBottom) = GameTime
		AnySwitchHit eSwitchLoopRBottom
		If bSkillShotActive Then bSkillShotActive = False
		anScore(nPlayer) = anScore(nPlayer) + 10
		DOF 214, DOFPulse				   
	End if	
End Sub

Sub TriggerLoopRTop_Hit
	If GameTime - anSwitchLastHit(eSwitchLoopRTop) > nDebounceTime Then
		anSwitchLastHit(eSwitchLoopRTop) = GameTime
		If anSwitchLastHit(eSwitchLoopRBottom) > -1 _
		and (GameTime - anSwitchLastHit(eSwitchLoopRBottom) < 1500) _
		and (anSwitchLastHit(eSwitchLoopRBottom) > anSwitchLastHit(eSwitchLoopLTop)) Then
			ShotHit eShotRightLoop
			anSwitchLastHit(eSwitchLoopRBottom) = -1
		End If
		DOF 215, DOFPulse				   
		AnySwitchHit eSwitchLoopRTop
	End if	
End Sub

Sub TimerGateOpen_Timer
	TimerGateOpen.Enabled = False
	GateUpperLanesR.open = False	
End Sub

' ramps

Sub GateRampLeft_Hit
	If GameTime - anSwitchLastHit(eSwitchRampL) > nDebounceTime Then
		anSwitchLastHit(eSwitchRampL) = GameTime
		ShotHit eShotLeftRamp
		AnySwitchHit eSwitchRampL
		anBonusElements(eMballFire) = anBonusElements(eMballFire) + 190
		anScore(nPlayer) = anScore(nPlayer) + (490 * nPlayfieldX * anComboX(eShotLeftRamp))
		DOF 236, DOFPulse				   
	End if
End Sub

Sub GateRampSide_Hit
	If GameTime - anSwitchLastHit(eSwitchRampSide) > nDebounceTime Then
		anSwitchLastHit(eSwitchRampSide) = GameTime
		ShotHit eShotSideRamp
		AnySwitchHit eSwitchRampSide
		anBonusElements(eMballAir) = anBonusElements(eMballAir) + 190
		anScore(nPlayer) = anScore(nPlayer) + (490 * nPlayfieldX)
		DOF 237, DOFPulse				   
	End If
End Sub

Sub GateRampRight_Hit
	If GameTime - anSwitchLastHit(eSwitchRampR) > nDebounceTime Then
		anSwitchLastHit(eSwitchRampR) = GameTime
		ShotHit eShotRightRamp
		AnySwitchHit eSwitchRampR
		anBonusElements(eMballAir) = anBonusElements(eMballAir) + 190
		anScore(nPlayer) = anScore(nPlayer) + (490 * nPlayfieldX * anComboX(eShotRightRamp))
		DOF 237, DOFPulse				   
	End If
End Sub

Sub TriggerRRampDrop_Hit
	Dim nBall
	If GameTime - anSwitchLastHit(eSwitchRampRDrop) > nDebounceTime Then
		anSwitchLastHit(eSwitchRampRDrop) = GameTime
		PlaySoundAtBall "WireRamp_Hit"
		nBall = BallIndex(ActiveBall)
		If -1 <> nBall then anBallSurface(nBall) = eSurfacePlayfield
'	debug.print "Rrampdrop hit"
	DOF 216, DOFPulse				  
	End If
End Sub

Sub AnySwitchHit(nIndex)
	Dim i, j
	nTimeLastSwitch = GameTime
	If Not bFirstSwitchHit Then
		bFirstSwitchHit = True
		nTimeBallSave = cBallSaveStart
		LightShootAgain.state = LightStateBlinking
		If Not bInMode then
			For each j in aoXLights : j.BlinkInterval = 170 : j.state = 0 :  Next
			For each j in aoXLights : j.BlinkPattern = "10" : j.state = 0 : Next
			For each j in aoBagLights : j.BlinkInterval = 170 : j.state = 0 : Next
			For each j in aoBagLights : j.BlinkPattern = "10" : j.state = 0 : Next
			For each j in aoDropLights : j.BlinkInterval = 170 : j.state = 0 : Next
			For each j in aoDropLights : j.BlinkPattern = "10" : j.state = 0 : Next
			lightTorpedo.BlinkInterval = 170 : lightTorpedo.state = 0 
			For i = eColorRed to eColorWhite
				For j = eShotUnderFlipper to eShotRightLoop
					RemoveModeColor j, i
				Next
			Next
			bSelectingMode = False
			nTimeModeSelectVO = 0
			StartMode nModeSelected
			PlaySound "SFX_mode_select_beep",0,fSFXVolume,0,0,1,1,1
		End If
	End If
	If avModesRunning(nPlayer).Contains(eModeFire2) Then
		FireMode2Update eShotNone, nIndex
	End If
	If avModesRunning(nPlayer).Contains(eModeIceWizard) Then
		IceMode3Update eShotNone, nIndex
	End If
	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		SuperSecretUpdate eShotNone, nIndex
	End If
End Sub

' Multiballs

Sub CommonModeStart(nMode)
	Dim i
	bInMode = True
	RemoveModeColor eShotSaucer, eColorRainbow
	anAddABallUsed(nPlayer) = 0
	aanModeShots(nPlayer, nMode) = 0
	avModesRunning(nPlayer).add nMode
	UpdateMode nMode, eShotNone, eSwitchNone
	PlayModeMusic
End Sub

Sub CommonModeEnd
	Dim i
	bInMode = False
	AddModeColor eShotSaucer, eColorRainbow
	UpdateWizardModesLit
	PlayModeMusic
	If IsMultiballActive Then
		ModeSelectNext
	End If
End Sub

Sub ModeProgressSound(nMode, nShot)
	If (nSFXPriority > eVOPrioModeProgress) _
	and (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
		sSFXToPlay = "SFX_mode_shot_hit_single"
		If eShotNone <> nShot Then
			If eShotSideRamp = nShot or anComboX(nShot) > 1 Then
				sSFXToPlay = "SFX_mode_shot_hit_double"
			End If
		End If
		nSFXPriority = eVOPrioModeProgress
		fSFXQueuedVolume = 0.4
	End If
End Sub

Sub CommonMBallStart
	Dim i
	For i = eMballCyber to eMballIce
		If anTimerMballGrace(i) > 0 Then
			anTimerMballGrace(i) = 0
			Select Case i
				Case eMballCyber
					CyberMballUpdate eShotNone
				Case eMBallWater
					WaterMballUpdate eShotNone
				Case eMballFire
					FireMballUpdate eShotNone
				Case eMballEarth
					EarthMballUpdate eShotNone
				Case eMBallAir
					AirMballUpdate eShotNone
				Case eMballIce
					IceMballUpdate eShotNone
			End Select
		End If
	Next
End Sub

Function MBallStartCallout(nMball)
	Dim i, nMultiballs

	For i = eMballCyber to eMballIce
		If anTimerMballGrace(i) > 0 Then
			AddCallout "VO_hero_mball_revive", eVOPrioMballStart, eCharacterHero
			MBallStartCallout = False
			Exit Function
		End if
	Next

	nMultiballs = 0
	For Each i in Array(eModeCyberMBall, eModeFireMBall, eModeWaterMBall, _
	eModeEarthMBall, eModeAirMBall, eModeIceMBall)
		If avModesRunning(nPlayer).Contains(i) Then nMultiballs = nMultiballs + 1
	Next
	If nMultiballs > 0 Then MBallStartCallout = False Else MBallStartCallout = True

	Select Case nMball
		Case eMballCyber
			If nMultiballs > 0 Then
				AddCallout "VO_cyber_mball_join", eVOPrioMballStart, eCharacterCyber
			Else
				AddCallout "VO_cyber_mball_start", eVOPrioMballStart, eCharacterCyber
			End If
		Case eMBallWater
			If nMultiballs > 0 Then
				AddCallout "VO_water_mball_join", eVOPrioMballStart, eCharacterWater
			Else
				AddCallout "VO_water_mball_start", eVOPrioMballStart, eCharacterWater
			End If
		Case eMballFire
			If nMultiballs > 0 Then
				AddCallout "VO_fire_mball_join", eVOPrioMballStart, eCharacterFire
			Else
				AddCallout "VO_fire_mball_start", eVOPrioMballStart, eCharacterFire
			End If
		Case eMballEarth
			If nMultiballs > 0 Then
				AddCallout "VO_earth_mball_join", eVOPrioMballStart, eCharacterEarth
			Else
				AddCallout "VO_earth_mball_start", eVOPrioMballStart, eCharacterEarth
			End If
		Case eMBallAir
			If nMultiballs > 0 Then
				AddCallout "VO_air_mball_join", eVOPrioMballStart, eCharacterAir
			Else
				AddCallout "VO_air_mball_start", eVOPrioMballStart, eCharacterAir
			End If
		Case eMballIce
			If nMultiballs > 0 Then
				AddCallout "VO_ice_mball_join", eVOPrioMballStart, eCharacterIce
			Else
				AddCallout "VO_ice_mball_start", eVOPrioMballStart, eCharacterIce
			End If
	End Select
End Function

Sub CommonWizardStart
	Dim i
	For i = 0 to 7 : ClearMballColor i : Next
	For i = 0 to 7 : anTimerTorpedo(i) = 0 : Next
	If False = avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
'	And False = avModesRunning(nPlayer).Contains(eModeEarthWizard) Then
		bTurnClockwise = False
		TimerTurntable_Timer
	End If
End Sub

Sub CommonWizardEnd
	Dim i
	If aanLocksLit(nPlayer, eMballCyber) > 0 Then
		For each i in aoDigit0Lights : i.state = 0 : i.state = 2 : Next
		For each i in aoDigit1Lights : i.state = 0 : i.state = 2 : Next
		For each i in aoDigit2Lights : i.state = 0 : i.state = 2 : Next
		AddMballColor eShotLeftLoop, eColorGreen
	Else
		UpdateLEDs SpinsForCyberLock - aanLockProgress(nPlayer, eMballCyber)
	End If
	If aanLocksLit(nPlayer, eMballFire) > 0 Then AddMballColor eShotLeftRamp, eColorRed
	If aanLocksLit(nPlayer, eMballEarth) > 0 Then AddMballColor eShotEarth, eColorOrange
	If aanLocksLit(nPlayer, eMballIce) > 0 Then AddMballColor eShotRightLoop, eColorWhite
	If aanLocksLit(nPlayer, eMballAir) > 0 Then AddMballColor eShotRightRamp, eColorPurple
	If aanLocksLit(nPlayer, eMballWater) > 0 Then AddMballColor eShotSaucer, eColorBlue
	If aanMballsPlayed(nPlayer, eMballEarth) > 1 _
	and aanLocksLit(nPlayer, eMballEarth) > 0 _
	and aanLocksMade(nPlayer, eMballEarth) < 2 then
		bTurnClockwise = True
	Else
		bTurnClockwise = False
	End If
	TimerTurntable_Timer
	anWizardModeRunning(nPlayer) = eModeNone
End Sub

Sub ShowMBallScore
	Dim i, nTotal

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	nTotal = 0
	for each i in Array(eModeCyberMBall, eModeWaterMBall, eModeFireMBall, _
	eModeEarthMBall, eModeAirMBall, eModeIceMBall)
		nTotal = nTotal + anModeScore(i)
		anModeScore(i) = 0
	next
	ShowText "MULTIBALL TOTAL", WilliamsFormatNum(nTotal), 3000, eVOPrioModeEnd
End Sub

Sub AddBalls(nBalls)
	If BIP + nBalls + nBallPhysLocked < 7 Then
		AutoPlunge nBalls
		BIP = BIP + nBalls
	ElseIf BIP + nBallPhysLocked < 6 Then
		AutoPlunge 6 - (BIP + nBallPhysLocked)
		BIP = 6 - nBallPhysLocked
	End If
End Sub

Sub ShotHit(nShot)
	Dim nAirIndex, bXAwarded, i, n
	If eShotNone = nShot Then Exit Sub
	aanShotLastHit(nPlayer, nShot) = GameTime

	If bDrainingBalls Then
		If eShotSaucer = nShot Then
			KickerLeft.TimerEnabled = True
			nTimeDelaySaucerUpper = 600
		End If
		if eShotUnderFlipper = nShot Then KickerShatz.TimerEnabled = True
		Exit Sub
	End If

	bXAwarded = False
	If eShotUnderFlipper = nShot And true = avXTargets(nPlayer).IsFull Then
		bXAwarded = True
		nTimeDelaySaucerLower = 2500
		avXTargets(nPlayer).Clear
		Select Case nPlayfieldX
			Case 2
				ShowText "3X SCORING", "FOR 30 SECONDS", 2500, eVOPrioPlayfieldX
				PlayJingle "SFX_playfield_3X", 3000
			Case 3
				ShowText "3X SCORING", "EXTENDED", 2500, eVOPrioPlayfieldX
			Case Else
				ShowText "2X SCORING", "FOR 30 SECONDS", 2500, eVOPrioPlayfieldX
				PlayJingle "SFX_playfield_2X", 3000
		End Select
		nPlayfieldX = nPlayfieldX + 1
		If nPlayfieldX > 3 then nPlayfieldX = 3
		nPlayfieldXTimer = 32000
		LightShowStart eLightShowXBegin, eVOPrioPlayfieldX
		FlashSweep eColorYellow
	End If

	If avModesRunning(nPlayer).Contains(eModeCyberMBall) Then
		CyberMBallUpdate nShot
	End If

	If avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
		WaterMBallUpdate nShot
	End If

	If avModesRunning(nPlayer).Contains(eModeEarthMBall) Then
		EarthMBallUpdate nShot
	End If

	If avModesRunning(nPlayer).Contains(eModeFireMBall) Then
		FireMBallUpdate nShot
	End If

	If avModesRunning(nPlayer).Contains(eModeAirMBall) Then
		AirMBallUpdate nShot
	End If

	If avModesRunning(nPlayer).Contains(eModeIceMBall) Then
		IceMBallUpdate nShot
	End If

	If nShot = eShotSaucer And anExtraBallsLit(nPlayer) > 0 Then
		PlayJingle "SFX_extra_ball", 4500
		AddDelayedCallout "VO_hero_extra_ball", eVOPrioExtraBall, eCharacterHero, 4000
		nTimeDelaySaucerUpper = 4500
		DuckMusic 4500
		ShowText "EXTRA BALL", "EXTRA BALL", 2500, eVOPrioExtraBall
		nExtraBalls = nExtraBalls + 1
		anExtraBallsLit(nPlayer) = anExtraBallsLit(nPlayer) - 1
		FlashSweep Array(eColorWhite, eColorBlue, eColorGreen, eColorRed)
	End If

	For Each i in an1BallModes
		If avModesRunning(nPlayer).Contains(i) Then
			UpdateMode i, nShot, eSwitchNone
		End If
	Next

	For Each i in anWizardModes
		If avModesRunning(nPlayer).Contains(i) Then
			UpdateMode i, nShot, eSwitchNone
		End If
	Next

	If Not IsWizardModeActive Then
		If aanLocksLit(nPlayer, eMballCyber) > 0 and eShotLeftLoop = nShot Then
			UpPostMove LoopPost,LoopPostPrimitive,"up"
			UpPostMove SkillPost,SkillPostPrimitive,"up"
			If nTimeLoopPost < 1000 Then nTimeLoopPost = 1000
			nTimeSkillPost = 2000
			CyberMballStart
		End If

		If aanLocksLit(nPlayer, eMballFire) > 0 and eShotLeftRamp = nShot Then
			FireLockBall
		End If

		If aanLocksLit(nPlayer, eMballIce) > 0 and eShotRightLoop = nShot Then
			UpPostMove LoopPost,LoopPostPrimitive,"up"
			UpPostMove SkillPost,SkillPostPrimitive,"up"
			If nTimeLoopPost < 1000 Then nTimeLoopPost = 1000
			nTimeSkillPost = 2000
			IceLockBall
		End If

		If aanLocksLit(nPlayer, eMBallWater) > 0 and eShotSaucer = nShot _
		And (IsMultiballActive or bInMode or (0 = avWizardModesLit(nPlayer).Count)) Then
			WaterLockBall
		End If

		If anTimerTorpedo(nShot) > 0 Then
			WaterAdvanceLock nshot
		End If

		If aanLocksLit(nPlayer, eMballAir) > 0 and eShotRightRamp = nShot Then
			AirMballStart
		End If

		If eShotSideRamp <> nShot Then
			if eShotLeftRamp = nShot Then
				nAirIndex = 4
			elseif eShotEarth = nShot Then
				nAirIndex = 5
			Else
				nAirIndex = nShot
			End If
			If True = avAirLettersLit(nPlayer).Contains(nAirIndex) _
			or aanMballsPlayed(nPlayer, eMballAir) > 2 Then
				AirAdvanceLock nAirIndex
			End If
		End If
	End If

	If eShotSideRamp = nShot Then
		If vSuperJPsLit.Count > 0 And False = TimerSJCallout.Enabled Then
			nSuperJacksShown = 0
			avMballColors(eShotSideRamp).Clear
			UpPostMove RightRampPost,RightRampPostPrimitive,"up"
			TimerSJCallout_timer
			TimerSJCallout.Enabled = True
		End If
	End If

	n = 0
	For Each i in an1BallModes
		If avModesPlayed(nPlayer).Contains(i) Then n = n + 1
	Next
	If BIP < 2 And False = IsMultiballActive and false = bInMode and eShotSaucer = nShot Then
		nTimeDelaySaucerUpper = 999999999
		bSelectingMode = True
		nTimeModeSelectVO = 12000
		ModeSelectNext
		If nModeSelected = eModeCyberWizard Or nModeSelected = eModeWaterWizard _
		Or nModeSelected = eModeFireWizard Or nModeSelected = eModeEarthWizard _
		Or nModeSelected = eModeAirWizard Or nModeSelected = eModeIceWizard Then
			AddCallout "VO_hero_showdown_lit", eVOPrioModeSelect, eCharacterHero
		Elseif nModeSelected = eModeTotalMayhem Or nModeSelected = eModeEvilUnited _
		Or nModeSelected = eModeMegaWizard Then
			AddCallout "VO_hero_wizard_mode_lit", eVOPrioModeSelect, eCharacterHero
		Else
			AddCallout "VO_hero_mode_select", eVOPrioModeSelect, eCharacterHero
		End If
		PlayModeMusic ' spaghetti
	Elseif (true = IsMultiballActive or BIP > 1) and n < 12 _
	and false = bInMode and eShotSaucer = nShot Then
		StartMode nModeSelected
	End If

	If (nShot = eShotUnderFlipper) and (anBagLevel(nPlayer) > 0) _
	And (Not avModesRunning(nPlayer).Contains(eModeTotalMayhem)) Then
		If bXAwarded Then
			bDelayBagOfTricks = True
		Else
			bDelayBagOfTricks = False
			AwardMystery False
		End If
	End If

	EndCombo nShot
	StartCombo nShot
End Sub

Sub StartCombo (nFromSHot)
	Dim i
	Select Case nFromSHot
		' Shots that feed left flipper
		Case eShotUnderFlipper, eShotLeftRamp
			For each i in Array(eShotLeftRamp, eShotEarth, eShotRightRamp, eShotRightLoop)
				anComboTimer(i) = 4000
				anComboX(i) = 2
				aoComboLights(i).state = LightStateOn
			Next
		' Shots that feed right flipper
		Case eShotLeftLoop, eShotSideRamp, eShotRightRamp
			For each i in Array(eShotUnderFlipper, eShotLeftLoop, eShotSaucer, eShotLeftRamp)
				anComboTimer(i) = 4000
				anComboX(i) = 2
				aoComboLights(i).state = LightStateOn
			Next
	End Select
End Sub

Sub EndCombo (byVal nAtShot)
	Dim bEndLeftShots, i
	If eShotSideRamp = nAtShot Then Exit Sub
	If eShotLeftRamp = nAtShot Then
		If anComboTimer(eShotSaucer) = anComboTimer(eShotLeftRamp) Then
			bEndLeftShots = True
		Else
			bEndLeftShots = False
		End If
	Elseif eShotUnderFlipper = nAtShot Or eShotLeftLoop = nAtShot _
	Or eShotSaucer = nAtShot Then
		bEndLeftShots = True
	Else
		bEndLeftShots = False
	End If

	If bEndLeftShots Then
		For each i in Array(eShotUnderFlipper, eShotLeftLoop, eShotSaucer, eShotLeftRamp)
			anComboTimer(i) = 0
			If avPermaCombo(nPlayer).Contains(i) Then
				anComboX(i) = 2
				aoComboLights(i).state = LightStateOn
			Else
				anComboX(i) = 1
				aoComboLights(i).state = LightStateOff
			End If
		Next
	Else
		For each i in Array(eShotLeftRamp, eShotEarth, eShotRightRamp, eShotRightLoop)
			anComboTimer(i) = 0
			If avPermaCombo(nPlayer).Contains(i) Then
				anComboX(i) = 2
				aoComboLights(i).state = LightStateOn
			Else
				anComboX(i) = 1
				aoComboLights(i).state = LightStateOff
			End If
		Next
	End If
End Sub

Function IsMultiballActive
	If avModesRunning(nPlayer).Contains(eModeCyberMBall) _
	Or avModesRunning(nPlayer).Contains(eModeWaterMBall) _
	Or avModesRunning(nPlayer).Contains(eModeFireMBall) _
	Or avModesRunning(nPlayer).Contains(eModeEarthMBall) _
	Or avModesRunning(nPlayer).Contains(eModeAirMBall) _
	Or avModesRunning(nPlayer).Contains(eModeIceMBall) Then
		IsMultiballActive = True
	Else
		IsMultiballActive = False
	End If
End Function

Function IsWizardModeActive
	If avModesRunning(nPlayer).Contains(eModeCyberWizard) _
	Or avModesRunning(nPlayer).Contains(eModeWaterWizard) _
	Or avModesRunning(nPlayer).Contains(eModeFireWizard) _
	Or avModesRunning(nPlayer).Contains(eModeEarthWizard) _
	Or avModesRunning(nPlayer).Contains(eModeAirWizard) _
	Or avModesRunning(nPlayer).Contains(eModeIceWizard) _
	Or avModesRunning(nPlayer).Contains(eModeTotalMayhem) _
	Or avModesRunning(nPlayer).Contains(eModeEvilUnited) _
	Or avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		IsWizardModeActive = True
	Else
		IsWizardModeActive = False
	End If
End Function

Sub UpdateWizardModesLit
	Dim i, n
	If False = avModesPlayed(nPlayer).Contains(eModeTotalMayhem) Then
		n = 0
		For Each i in an1BallModes
			If avModesPlayed(nPlayer).Contains(i) Then n = n + 1
		Next
		If n > 5 Then
			avWizardModesLit(nPlayer).add eModeTotalMayhem
		End If
	End If
	If False = avModesPlayed(nPlayer).Contains(eModeEvilUnited) Then
		n = 0
		For Each i in an1BallModes
			If avModesPlayed(nPlayer).Contains(i) Then n = n + 1
		Next
		If n > 11 Then
			avWizardModesLit(nPlayer).add eModeEvilUnited
		End If
	End If
	If False = avModesPlayed(nPlayer).Contains(eModeMegaWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeCyberWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeWaterWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeFireWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeEarthWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeAirWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeIceWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeEvilUnited) Then
		avWizardModesLit(nPlayer).add eModeMegaWizard
	End If
	If False = avModesPlayed(nPlayer).Contains(eModeCyberWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeCyber1) _
	And True = avModesPlayed(nPlayer).Contains(eModeCyber2) _
	And True = avSJCollected(nPlayer).Contains(eMballCyber) Then
		avWizardModesLit(nPlayer).add eModeCyberWizard
	End If
	If False = avModesPlayed(nPlayer).Contains(eModeWaterWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeWater1) _
	And True = avModesPlayed(nPlayer).Contains(eModeWater2) _
	And True = avSJCollected(nPlayer).Contains(eMBallWater) Then
		avWizardModesLit(nPlayer).add eModeWaterWizard
	End If
	If False = avModesPlayed(nPlayer).Contains(eModeFireWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeFire1) _
	And True = avModesPlayed(nPlayer).Contains(eModeFire2) _
	And True = avSJCollected(nPlayer).Contains(eMballFire) Then
		avWizardModesLit(nPlayer).add eModeFireWizard
	End If
	If False = avModesPlayed(nPlayer).Contains(eModeEarthWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeEarth1) _
	And True = avModesPlayed(nPlayer).Contains(eModeEarth2) _
	And True = avSJCollected(nPlayer).Contains(eMballEarth) Then
		avWizardModesLit(nPlayer).add eModeEarthWizard
	End If
	If False = avModesPlayed(nPlayer).Contains(eModeAirWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeAir1) _
	And True = avModesPlayed(nPlayer).Contains(eModeAir2) _
	And True = avSJCollected(nPlayer).Contains(eMballAir) Then
		avWizardModesLit(nPlayer).add eModeAirWizard
	End If
	If False = avModesPlayed(nPlayer).Contains(eModeIceWizard) _
	And True = avModesPlayed(nPlayer).Contains(eModeIce1) _
	And True = avModesPlayed(nPlayer).Contains(eModeIce2) _
	And True = avSJCollected(nPlayer).Contains(eMballIce) Then
		avWizardModesLit(nPlayer).add eModeIceWizard
	End If
End Sub

Sub ModePreviewLights(nMode)
	Dim i, j, anShots

	For each j in aoXLights : j.BlinkInterval = 170 : j.state = 0 :  Next
	For each j in aoXLights : j.BlinkPattern = "10" : j.state = 0 : Next
	For each j in aoBagLights : j.BlinkInterval = 170 : j.state = 0 : Next
	For each j in aoBagLights : j.BlinkPattern = "10" : j.state = 0 : Next
	For each j in aoDropLights : j.BlinkInterval = 170 : j.state = 0 : Next
	For each j in aoDropLights : j.BlinkPattern = "10" : j.state = 0 : Next
	lightTorpedo.BlinkInterval = 170 : lightTorpedo.state = 0 
	For i = eColorRed to eColorWhite
		For j = eShotUnderFlipper to eShotRightLoop
			RemoveModeColor j, i
		Next
	Next
	Select Case nMode
		Case eModeCyber1
			For each i in aoXLights : i.BlinkInterval = 85 : Next
			aoXLights(1).BlinkPattern = "01" : aoXLights(3).BlinkPattern = "01"
			For each i in aoXLights : i.state = LightStateBlinking : Next
			For each i in aoBagLights : i.BlinkInterval = 85 : Next
			aoBagLights(1).BlinkPattern = "01" : aoBagLights(3).BlinkPattern = "01"
			For each i in aoBagLights : i.state = LightStateBlinking : Next
			For each i in aoDropLights : i.BlinkInterval = 85 : Next
			aoDropLights(1).BlinkPattern = "01"
			For each i in aoDropLights : i.state = LightStateBlinking : Next
			lightTorpedo.state = LightStateBlinking
			AddModeColor eShotEarth, eColorGreen
		Case eModeCyber2
			For i = eShotUnderFlipper to eShotRightLoop
				AddModeColor i, eColorGreen
			Next
		Case eModeWater1
			AddModeColor eShotLeftLoop, eColorBlue
			AddModeColor eShotLeftRamp, eColorBlue
			AddModeColor eShotSideRamp, eColorBlue
			AddModeColor eShotRightLoop, eColorBlue
		Case eModeWater2
			anShots = getLeastRecentShots(7)
			for i = 0 to 2 : AddModeColor anShots(i), eColorBlue : Next
		Case eModeFire1
			AddModeColor eShotLeftLoop, eColorRed
			AddModeColor eShotSideRamp, eColorRed
			AddModeColor eShotRightRamp, eColorRed
		Case eModeFire2
			AddModeColor eShotLeftLoop, eColorRed
			AddModeColor eShotRightLoop, eColorRed
		Case eModeEarth1
			AddModeColor eShotUnderFlipper, eColorOrange
			AddModeColor eShotLeftLoop, eColorOrange
			AddModeColor eShotEarth, eColorOrange
			AddModeColor eShotSideRamp, eColorOrange
		Case eModeEarth2
			For each i in aoXLights : i.BlinkInterval = 85 : Next
			aoXLights(1).BlinkPattern = "01" : aoXLights(3).BlinkPattern = "01"
			For each i in aoXLights : i.state = LightStateBlinking : Next
			For each i in aoBagLights : i.BlinkInterval = 85 : Next
			aoBagLights(1).BlinkPattern = "01" : aoBagLights(3).BlinkPattern = "01"
			For each i in aoBagLights : i.state = LightStateBlinking : Next
			For each i in aoDropLights : i.BlinkInterval = 85 : Next
			aoDropLights(1).BlinkPattern = "01"
			For each i in aoDropLights : i.state = LightStateBlinking : Next
			lightTorpedo.state = LightStateBlinking
		Case eModeAir1
			for i = 2 to 5 : AddModeColor i, eColorPurple : Next
		Case eModeAir2
			AddModeColor eShotSideRamp, eColorPurple
			AddModeColor eShotLeftRamp, eColorPurple
			AddModeColor eShotRightRamp, eColorPurple
		Case eModeIce1
			for i = 4 to 7 : AddModeColor i, eColorWhite : Next
		Case eModeIce2
			for i = 4 to 5 : AddModeColor i, eColorWhite : Next
	End Select
End Sub

Sub ModeSelectNext
	Dim i, n, nNextMode, nWizard

	If Not IsMultiballActive Then
		If avWizardModesLit(nPlayer).Count < 1 Then
			nWizard = eModeNone
		Elseif 1 = avWizardModesLit(nPlayer).Count Then
			nWizard = eModeNone
			For Each i in anWizardModes
				If avWizardModesLit(nPlayer).Contains(i) Then
					nWizard = i
					Exit For
				End If
			Next
		Else
			nWizard = eModeNone
			For i = 1 to 28
				If avWizardModesLit(nPlayer).Contains((nModeSelected + i) Mod 29) Then
					nWizard = (nModeSelected + i) Mod 29
					Exit For
				End If
			Next
		End If
		If eModeNone <> nWizard Then
			nModeSelected = nWizard
			Exit Sub
		End If
	End If

	n = 0
	nWizard = True
	For Each i in an1BallModes
		If avModesPlayed(nPlayer).Contains(i) Then n = n + 1
		If nModeSelected = i Then nWizard = False
	Next
	If n > 11 Then Exit Sub
	' If the current selection is a wizard mode and wizard modes cannot be	
	' played, select a random available mode
	If nWizard Then
		n = 0
		For Each i in an1BallModes
			If Not avModesPlayed(nPlayer).Contains(i) Then
				n = n + 1
				If Rnd < (1 / n) Then nModeSelected = i
			End If
		Next
	End If

	If avModesPlayed(nPlayer).Contains(nModeSelected) Then
		aoLightForMode(nModeSelected).state = LightStateOn
	Else
		aoLightForMode(nModeSelected).state = LightStateOff
	End If
	nNextMode = nModeSelected
	for i = 1 to 11
		Select Case 0 + nNextMode
			Case eModeCyber1
				nNextMode = eModeCyber2
			Case eModeCyber2
				nNextMode = eModeWater1
			Case eModeWater1
				nNextMode = eModeWater2
			Case eModeWater2
				nNextMode = eModeFire1
			Case eModeFire1
				nNextMode = eModeFire2
			Case eModeFire2
				nNextMode = eModeEarth1
			Case eModeEarth1
				nNextMode = eModeEarth2
			Case eModeEarth2
				nNextMode = eModeAir1
			Case eModeAir1
				nNextMode = eModeAir2
			Case eModeAir2
				nNextMode = eModeIce1
			Case eModeIce1
				nNextMode = eModeIce2
			Case eModeIce2
				nNextMode = eModeCyber1
		End Select
		If Not avModesPlayed(nPlayer).Contains(nNextMode) Then
'			msgbox "" & nModeSelected & " " & nNextMode
			nModeSelected = nNextMode
			aoLightForMode(nModeSelected).state = LightStateOn
			aoLightForMode(nModeSelected).state = LightStateBlinking
			If bSelectingMode Then ModePreviewLights nModeSelected
			Exit Sub
		End If
	Next
	If nTimeModeSelectVO > 0 then nTimeModeSelectVO = 12000
End Sub

Sub ModeSelectPrevious
	Dim i, n, nNextMode, nWizard

	If Not IsMultiballActive Then
		If avWizardModesLit(nPlayer).Count < 1 Then
			nWizard = eModeNone
		Elseif 1 = avWizardModesLit(nPlayer).Count Then
			nWizard = eModeNone
			For Each i in anWizardModes
				If avWizardModesLit(nPlayer).Contains(i) Then
					nWizard = i
					Exit For
				End If
			Next
		Else
			nWizard = eModeNone
			For i = 28 to 1 step -1
				If avWizardModesLit(nPlayer).Contains((nModeSelected + i) Mod 29) Then
					nWizard = (nModeSelected + i) Mod 29
					Exit For
				End If
			Next
		End If
		If eModeNone <> nWizard Then
			nModeSelected = nWizard
			Exit Sub
		End If
	End If

	n = 0
	nWizard = True
	For Each i in an1BallModes
		If avModesPlayed(nPlayer).Contains(i) Then n = n + 1
		If nModeSelected = i Then nWizard = False
	Next
	If n > 11 Then Exit Sub
	' If the current selection is a wizard mode and wizard modes cannot be	
	' played, select a random available mode
	If nWizard Then
		n = 0
		For Each i in an1BallModes
			If Not avModesPlayed(nPlayer).Contains(i) Then
				n = n + 1
				If Rnd < (1 / n) Then nModeSelected = i
			End If
		Next
	End If

	nNextMode = nModeSelected
	If avModesPlayed(nPlayer).Contains(nModeSelected) Then
		aoLightForMode(nModeSelected).state = LightStateOn
	Else
		aoLightForMode(nModeSelected).state = LightStateOff
	End If

	for i = 1 to 11
		Select Case 0 + nNextMode
			Case eModeCyber1
				nNextMode = eModeIce2
			Case eModeCyber2
				nNextMode = eModeCyber1
			Case eModeWater1
				nNextMode = eModeCyber2
			Case eModeWater2
				nNextMode = eModeWater1
			Case eModeFire1
				nNextMode = eModeWater2
			Case eModeFire2
				nNextMode = eModeFire1
			Case eModeEarth1
				nNextMode = eModeFire2
			Case eModeEarth2
				nNextMode = eModeEarth1
			Case eModeAir1
				nNextMode = eModeEarth2
			Case eModeAir2
				nNextMode = eModeAir1
			Case eModeIce1
				nNextMode = eModeAir2
			Case eModeIce2
				nNextMode = eModeIce1
		End Select
		If Not avModesPlayed(nPlayer).Contains(nNextMode) Then
			nModeSelected = nNextMode
			aoLightForMode(nModeSelected).state = LightStateOn
			aoLightForMode(nModeSelected).state = LightStateBlinking
			If bSelectingMode Then ModePreviewLights nModeSelected
			Exit For
		End If
	Next
	If nTimeModeSelectVO > 0 then nTimeModeSelectVO = 12000
End Sub

Sub ModeSelectChoose
	Dim i, j, isWizard
	bSelectingMode = False
	nTimeModeSelectVO = 0
	nTimeDelaySaucerUpper = 1
	For each j in aoXLights : j.BlinkInterval = 170 : j.state = 0 :  Next
	For each j in aoXLights : j.BlinkPattern = "10" : j.state = 0 : Next
	For each j in aoBagLights : j.BlinkInterval = 170 : j.state = 0 : Next
	For each j in aoBagLights : j.BlinkPattern = "10" : j.state = 0 : Next
	For each j in aoDropLights : j.BlinkInterval = 170 : j.state = 0 : Next
	For each j in aoDropLights : j.BlinkPattern = "10" : j.state = 0 : Next
	lightTorpedo.BlinkInterval = 170 : lightTorpedo.state = 0 
	For i = eColorRed to eColorWhite
		For j = eShotUnderFlipper to eShotRightLoop
			RemoveModeColor j, i
		Next
	Next
	If IsMultiballActive Then
		isWizard = True
		For Each i in an1BallModes
			if i = nModeSelected Then isWizard = False
		Next
		If isWizard Then
			nModeSelected = an1BallModes(Int(Rnd * 12))
			ModeSelectNext
		End If
	End If
	StartMode nModeSelected
	PlaySound "SFX_mode_select_beep",0,fSFXVolume,0,0,1,1,1
End Sub

Sub StartMode(nMode)
	Select Case nMode
		Case eModeCyber1
			nLastCharacter = eCharacterCyber
			CyberMode1Start
		Case eModeCyber2
			nLastCharacter = eCharacterCyber
			CyberMode2Start
		Case eModeCyberWizard
			nLastCharacter = eCharacterCyber
			CyberMode3Start
		Case eModeWater1
			nLastCharacter = eCharacterWater
			WaterMode1Start
		Case eModeWater2
			nLastCharacter = eCharacterWater
			WaterMode2Start
		Case eModeWaterWizard
			nLastCharacter = eCharacterWater
			WaterMode3Start
		Case eModeFire1
			nLastCharacter = eCharacterFire
			FireMode1Start
		Case eModeFire2
			nLastCharacter = eCharacterFire
			FireMode2Start
		Case eModeFireWizard
			nLastCharacter = eCharacterFire
			FireMode3Start
		Case eModeEarth1
			nLastCharacter = eCharacterEarth
			EarthMode1Start
		Case eModeEarth2
			nLastCharacter = eCharacterEarth
			EarthMode2Start
		Case eModeEarthWizard
			nLastCharacter = eCharacterEarth
			EarthMode3Start
		Case eModeAir1
			nLastCharacter = eCharacterAir
			AirMode1Start
		Case eModeAir2
			nLastCharacter = eCharacterAir
			AirMode2Start
		Case eModeAirWizard
			nLastCharacter = eCharacterAir
			AirMode3Start
		Case eModeIce1
			nLastCharacter = eCharacterIce
			IceMode1Start
		Case eModeIce2
			nLastCharacter = eCharacterIce
			IceMode2Start
		Case eModeIceWizard
			nLastCharacter = eCharacterIce
			IceMode3Start
		Case eModeTotalMayhem
			TotalMayhemStart
		Case eModeEvilUnited
			EvilUnitedStart
		Case eModeMegaWizard
			SuperSecretStart
	End Select
End Sub

Sub UpdateMode(nMode, nShot, nSwitch)
	Select Case nMode
		Case eModeCyber1
			CyberMode1Update nShot, nSwitch
		Case eModeCyber2
			CyberMode2Update nShot, nSwitch
		Case eModeCyberWizard
			CyberMode3Update nShot, nSwitch
		Case eModeWater1
			WaterMode1Update nShot, nSwitch
		Case eModeWater2
			WaterMode2Update nShot, nSwitch
		Case eModeWaterWizard
			WaterMode3Update nShot, nSwitch
		Case eModeFire1
			FireMode1Update nShot, nSwitch
		Case eModeFire2
			FireMode2Update nShot, nSwitch
		Case eModeFireWizard
			FireMode3Update nShot, nSwitch
		Case eModeEarth1
			EarthMode1Update nShot, nSwitch
		Case eModeEarth2
			EarthMode2Update nShot, nSwitch
		Case eModeEarthWizard
			EarthMode3Update nShot, nSwitch
		Case eModeAir1
			AirMode1Update nShot, nSwitch
		Case eModeAir2
			AirMode2Update nShot, nSwitch
		Case eModeAirWizard
			AirMode3Update nShot, nSwitch
		Case eModeIce1
			IceMode1Update nShot, nSwitch
		Case eModeIce2
			IceMode2Update nShot, nSwitch
		Case eModeIceWizard
			IceMode3Update nShot, nSwitch
		Case eModeTotalMayhem
			TotalMayhemUpdate nShot, nSwitch
		Case eModeEvilUnited
			EvilUnitedUpdate nShot, nSwitch
		Case eModeMegaWizard
			SuperSecretUpdate nShot, nSwitch
	End Select
End Sub

Sub EndMode(nMode, bDrained)
	Select Case nMode
		Case eModeCyber1
			CyberMode1End bDrained
		Case eModeCyber2
			CyberMode2End bDrained
		Case eModeCyberWizard
			CyberMode3End bDrained
		Case eModeWater1
			WaterMode1End bDrained
		Case eModeWater2
			WaterMode2End bDrained
		Case eModeWaterWizard
			WaterMode3End bDrained
		Case eModeFire1
			FireMode1End bDrained
		Case eModeFire2
			FireMode2End bDrained
		Case eModeFireWizard
			FireMode3End bDrained
		Case eModeEarth1
			EarthMode1End bDrained
		Case eModeEarth2
			EarthMode2End bDrained
		Case eModeEarthWizard
			EarthMode3End bDrained
		Case eModeAir1
			AirMode1End bDrained
		Case eModeAir2
			AirMode2End bDrained
		Case eModeAirWizard
			AirMode3End bDrained
		Case eModeIce1
			IceMode1End bDrained
		Case eModeIce2
			IceMode2End bDrained
		Case eModeIceWizard
			IceMode3End bDrained
		Case eModeEvilUnited
			EvilUnitedEnd bDrained
		Case eModeMegaWizard
			SuperSecretEnd bDrained
	End Select
End Sub

Sub CyberMode1Start
	avCyber1TargetsHit(nPlayer).clear
	aavShotsLit(nPlayer, eModeCyber1).Clear
	aavShotsLit(nPlayer, eModeCyber1).add eShotLeftLoop
	aavShotsLit(nPlayer, eModeCyber1).add eShotSaucer
	aavShotsLit(nPlayer, eModeCyber1).add eShotLeftRamp
	aavShotsLit(nPlayer, eModeCyber1).add eShotRightRamp
	aavShotsLit(nPlayer, eModeCyber1).add eShotRightLoop
	If avModesPlayed(nPlayer).Contains(eModeCyber2) _
	Or avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		aavShotsLit(nPlayer, eModeCyber1).add eShotUnderFlipper
		aavShotsLit(nPlayer, eModeCyber1).add eShotSideRamp
	End If
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		QueueDialogue "VO_hero_mode_cyber1_start", eVOPrioModeStart, eCharacterHero
		QueueDialogue "VO_cyber_mode1_start", eVOPrioModeStart, eCharacterCyber
		LightShowStart eLightShowCyberShort, eVOPrioModeStart
	End If
	CommonModeStart eModeCyber1
End Sub

Sub CyberMode1Update(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, nProgress

	bValidShot = False
	If aanModeShots(nPlayer, eModeCyber1) Mod 2 = 0 Then
		If False = avCyber1TargetsHit(nPlayer).Contains(eColorWhite) And _
			(eSwitchDropI = nSwitch or eSwitchDropI = nSwitch or eSwitchDropI = nSwitch) Then
			avCyber1TargetsHit(nPlayer).add eColorWhite
			bValidShot = True
		Elseif False = avCyber1TargetsHit(nPlayer).Contains(eColorCyan) And _
		(eSwitchTargetM4 = nSwitch Or eSwitchTargetM3 = nSwitch _
		Or eSwitchTargetM2 = nSwitch Or eSwitchTargetM1 = nSwitch) Then
			avCyber1TargetsHit(nPlayer).add eColorCyan
			bValidShot = True
		Elseif False = avCyber1TargetsHit(nPlayer).Contains(eColorYellow) And _
		(eSwitchTargetX4 = nSwitch Or eSwitchTargetX3 = nSwitch _
		Or eSwitchTargetX2 = nSwitch Or eSwitchTargetX1 = nSwitch) Then
			avCyber1TargetsHit(nPlayer).add eColorYellow
			bValidShot = True
		Elseif False = avCyber1TargetsHit(nPlayer).Contains(eColorOrange) And eShotEarth = nShot Then
			avCyber1TargetsHit(nPlayer).add eColorOrange
			RemoveModeColor eShotEarth, eColorGreen
			bValidShot = True
		Elseif False = avCyber1TargetsHit(nPlayer).Contains(eColorBlue) And _
		eSwitchTargetBlue = nSwitch Then 
			avCyber1TargetsHit(nPlayer).add eColorBlue
			bValidShot = True
		End If
	ElseIf eShotNone <> nShot Then
		If aavShotsLit(nPlayer, eModeCyber1).Contains(nShot) Then
			bValidShot = True
		End If
	End If
			
	If bValidShot Then
		nValue = 12000
		nValue = nValue * nPlayfieldX
		If eShotSideRamp = nShot Then
			nValue = nValue * 2
		ElseIf eShotNone <> nShot Then
			nValue = nValue * anComboX(nShot)
			If anComboX(nShot) > 1 Then ComboCallout
		End If
		anModeScore(eModeCyber1) = anModeScore(eModeCyber1) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		aanModeShots(nPlayer, eModeCyber1) = aanModeShots(nPlayer, eModeCyber1) + 1
		nProgress = aanModeShots(nPlayer, eModeCyber1)
		If eShotNone <> nShot and eShotEarth <> nShot Then
			aavShotsLit(nPlayer, eModeCyber1).Remove nShot
		End If
		If (not avModesPlayed(nPlayer).Contains(eModeCyber2)) _
		And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
			If nProgress > 6 Then nProgress = 6
			ShowText "INFO HIGHWAY", WilliamsFormatNum(nValue) & " " & nProgress & "/6", 2500, eVOPrioModeProgress
			If nProgress > 5 Then
				CyberMode1End False
				exit Sub
			ElseIf not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				LightShowStart eLightShowCyberShort, eVOPrioModeProgress
				FlashSweep eColorGreen
				ModeProgressSound eModeCyber1, nShot
			End If
		Else
			If nProgress > 10 Then nProgress = 10
			If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				TopSecretUpdate "INFO HIGHWAY", "", nValue
			Else
				ShowText "INFO HIGHWAY", WilliamsFormatNum(nValue) & " " & nProgress & "/10", 2500, eVOPrioModeProgress
			End If
			If nProgress > 9 Then
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					aanModeShots(nPlayer, eModeCyber1) = 0
					avCyber1TargetsHit(nPlayer).clear
					aavShotsLit(nPlayer, eModeCyber1).Clear
					aavShotsLit(nPlayer, eModeCyber1).add eShotLeftLoop
					aavShotsLit(nPlayer, eModeCyber1).add eShotSaucer
					aavShotsLit(nPlayer, eModeCyber1).add eShotLeftRamp
					aavShotsLit(nPlayer, eModeCyber1).add eShotRightRamp
					aavShotsLit(nPlayer, eModeCyber1).add eShotRightLoop
					aavShotsLit(nPlayer, eModeCyber1).add eShotUnderFlipper
					aavShotsLit(nPlayer, eModeCyber1).add eShotSideRamp
				Else
					CyberMode1End False
					exit Sub
				End If
			ElseIf not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				LightShowStart eLightShowCyberShort, eVOPrioModeProgress
				FlashSweep eColorGreen
				ModeProgressSound eModeCyber1, nShot
			End If
		End If
	End If

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	If aanModeShots(nPlayer, eModeCyber1) Mod 2 = 0 Then
		For i = 0 to 7 : RemoveModeColor i, eColorGreen : Next
		For each i in aoXLights : i.state = LightStateOff : Next
		If Not avCyber1TargetsHit(nPlayer).Contains(eColorYellow) Then
			For each i in aoXLights : i.BlinkInterval = 85 : Next
			aoXLights(1).BlinkPattern = "01" : aoXLights(3).BlinkPattern = "01"
			For each i in aoXLights : i.state = LightStateBlinking : Next
		End If
		For each i in aoBagLights : i.state = LightStateOff : Next
		If Not avCyber1TargetsHit(nPlayer).Contains(eColorCyan) Then
			For each i in aoBagLights : i.BlinkInterval = 85 : Next
			aoBagLights(1).BlinkPattern = "01" : aoBagLights(3).BlinkPattern = "01"
			For each i in aoBagLights : i.state = LightStateBlinking : Next
		End If
		For each i in aoDropLights : i.state = LightStateOff : Next
		If Not avCyber1TargetsHit(nPlayer).Contains(eColorWhite) Then
			For each i in aoDropLights : i.BlinkInterval = 85 : Next
			aoDropLights(1).BlinkPattern = "01"
			For each i in aoDropLights : i.state = LightStateBlinking : Next
		End If
		lightTorpedo.state = LightStateOff
		If Not avCyber1TargetsHit(nPlayer).Contains(eColorBlue) Then
			lightTorpedo.BlinkInterval = 85
			lightTorpedo.state = LightStateBlinking
		End If
		If Not avCyber1TargetsHit(nPlayer).Contains(eColorOrange) Then
			AddModeColor eShotEarth, eColorGreen
		End If
	Else
		For each i in aoXLights : i.BlinkInterval = 170 : Next
		For each i in aoXLights : i.BlinkPattern = "10" : Next
		For each i in aoBagLights : i.BlinkInterval = 170 : Next
		For each i in aoBagLights : i.BlinkPattern = "10" : Next
		For each i in aoDropLights : i.BlinkInterval = 170 : Next
		For each i in aoDropLights : i.BlinkPattern = "10" : Next
		lightTorpedo.BlinkInterval = 170

		For i = 0 to 7
			If aavShotsLit(nPlayer, eModeCyber1).Contains(i) Then
				AddModeColor i, eColorGreen
			Else
				RemoveModeColor i, eColorGreen
			End If
		Next
	End If
End Sub

Sub CyberMode1End(bDrained)
	Dim i
	For i = 0 to 7 : RemoveModeColor i, eColorGreen : Next
	If Not bDrained Then
		aavShotsLit(nPlayer, eModeCyber1).Clear
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			ShowText "INFO HIGHWAY", "TOTAL " & WilliamsFormatNum(anModeScore(eModeCyber1)), 5089, eVOPrioModeEnd
			PlayJingle "SFX_mode_total_jingle", 5089
			AddDelayedCallout "VO_hero_mode_cyber1_victory", eVOPrioModeEnd, eCharacterHero, 2500
			If KickerLeft.BallCntOver < 1 Then LightShowStart eLightShowModeComplete, eVOPrioModeEnd
			avModesPlayed(nPlayer).add eModeCyber1
		End If
		avModesRunning(nPlayer).Remove eModeCyber1
		CommonModeEnd
	End If
end Sub

Sub CyberMode2Start
	PlayModeMusic
    If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		QueueDialogue "VO_cyber_mode2_start", eVOPrioModeStart, eCharacterCyber
		QueueDialogue "VO_hero_mode_cyber2_start", eVOPrioModeStart, eCharacterHero
		LightShowStart eLightShowCyberShort, eVOPrioModeStart
	End If
	CommonModeStart eModeCyber2
End Sub

Sub CyberMode2Update(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, nProgress

	bValidShot = False
	If eShotNone <> nShot Then
		bValidShot = True
	End If
			
	If bValidShot Then
		nValue = 12000
		nValue = nValue * nPlayfieldX
		If eShotSideRamp = nShot Then
			nValue = nValue * 2
		ElseIf eShotNone <> nShot Then
			nValue = nValue * anComboX(nShot)
			If anComboX(nShot) > 1 Then ComboCallout
		End If
		anModeScore(eModeCyber2) = anModeScore(eModeCyber2) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		aanModeShots(nPlayer, eModeCyber2) = aanModeShots(nPlayer, eModeCyber2) + 1
		nProgress = aanModeShots(nPlayer, eModeCyber2)
		If (not avModesPlayed(nPlayer).Contains(eModeCyber1)) _
		And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
			If nProgress > 6 Then nProgress = 6
			ShowText "BEWARE GLITCHES", WilliamsFormatNum(nValue) & " " & nProgress & "/6", 2500, eVOPrioModeProgress
			If nProgress > 5 Then
				CyberMode2End False
				exit Sub
			ElseIf Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				LightShowStart eLightShowCyberShort, eVOPrioModeProgress
				FlashSweep eColorGreen
				ModeProgressSound eModeCyber2, nShot
			End If
		Else
			If nProgress > 10 Then nProgress = 10
			If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				TopSecretUpdate "BEWARE GLITCHES", "", nValue
			Else
				ShowText "BEWARE GLITCHES", WilliamsFormatNum(nValue) & " " & nProgress & "/10", 2500, eVOPrioModeProgress
			End If
			If nProgress > 9 Then
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					aanModeShots(nPlayer, eModeCyber2) = 0
				Else
					CyberMode2End False
					exit Sub
				End If
			ElseIf Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				LightShowStart eLightShowCyberShort, eVOPrioModeProgress
				FlashSweep eColorGreen
				ModeProgressSound eModeCyber2, nShot
			End If
		End If
	End If

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	For i = 0 to 7
		AddModeColor i, eColorGreen
	Next
End Sub

Sub CyberMode2End(bDrained)
	Dim i
	For i = 0 to 7 : RemoveModeColor i, eColorGreen : Next
	If Not bDrained Then
		aavShotsLit(nPlayer, eModeCyber2).Clear
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			If KickerLeft.BallCntOver < 1 Then LightShowStart eLightShowModeComplete, eVOPrioModeEnd
			ShowText "BEWARE GLITCHES", "TOTAL " & WilliamsFormatNum(anModeScore(eModeCyber2)), 5089, eVOPrioModeEnd
			PlayJingle "SFX_mode_total_jingle", 5089
			AddDelayedCallout "VO_hero_mode_cyber2_victory", eVOPrioModeEnd, eCharacterHero, 2500
			avModesPlayed(nPlayer).add eModeCyber2
		End If
		avModesRunning(nPlayer).Remove eModeCyber2
		CommonModeEnd
	End If
end Sub

Sub CyberMode3Start
	Dim i
	QueueDialogue "VO_cyber_showdown_start1", eVOPrioModeStart, eCharacterCyber
	QueueDialogue "VO_hero_showdown_cyber_start", eVOPrioModeStart, eCharacterHero
	QueueDialogue "VO_cyber_showdown_start2", eVOPrioModeStart, eCharacterCyber
	LightShowStart eLightShowCyberShort, eVOPrioModeStart
	For i = 0 to 7 : aanCyber3ShotsHit(nPlayer, i) = 0 : Next
	anCyber3AttackShot(nPlayer) = eShotEarth
	nCyber3AttackTime = cCyber3AttackInterval
	sLastCountdown = ""
	anWizardModeRunning(nPlayer) = eModeCyberWizard
	avWizardModesLit(nPlayer).Remove eModeCyberWizard
	CommonWizardStart
	CommonModeStart eModeCyberWizard
End Sub

Sub CyberMode3Update(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, bBossHit, nProgress
	Dim anAdjacentShots
	Dim nMaxAdjacent

	bValidShot = False
	If eShotNone <> nShot And eShotSideRamp <> nShot And anCyber3AttackShot(nPlayer) <> nShot Then
		bValidShot = True
	End If

	If anCyber3AttackShot(nPlayer) = nShot Then
		ShowText "SHOT IS UNDER", "ATTACK - NO HIT", 2000, eVOPrioModeProgress
	End If

	If bValidShot Then
		nProgress = aanCyber3ShotsHit(nPlayer, nShot)

		bBossHit = False
		If nProgress > 1 Then
			aanCyber3ShotsHit(nPlayer, nShot) = 0
			bBossHit = True
		Else
			nMaxAdjacent = 1
			anAdjacentShots = GetAdjacentShots(nShot, False)
			If anAdjacentShots(1) <> eShotNone Then
				If aanCyber3ShotsHit(nPlayer, anAdjacentShots(1)) > 0 Then
					If anAdjacentShots(0) <> eShotNone Then
						If aanCyber3ShotsHit(nPlayer, anAdjacentShots(0)) > 0 Then							
							aanCyber3ShotsHit(nPlayer, anAdjacentShots(0)) = aanCyber3ShotsHit(nPlayer, anAdjacentShots(0)) - 1
							aanCyber3ShotsHit(nPlayer, anAdjacentShots(1)) = aanCyber3ShotsHit(nPlayer, anAdjacentShots(1)) - 1
							bBossHit = True
						End if
					End If
					If False = bBossHit And anAdjacentShots(2) <> eShotNone Then
						If aanCyber3ShotsHit(nPlayer, anAdjacentShots(2)) > 0 Then
							aanCyber3ShotsHit(nPlayer, anAdjacentShots(1)) = aanCyber3ShotsHit(nPlayer, anAdjacentShots(1)) - 1
							aanCyber3ShotsHit(nPlayer, anAdjacentShots(2)) = aanCyber3ShotsHit(nPlayer, anAdjacentShots(2)) - 1
							bBossHit = True
						End If
					End If
					If False = bBossHit Then nMaxAdjacent = 2
				End If
			End if
			If False = bBossHit And anAdjacentShots(2) <> eShotNone Then
				If aanCyber3ShotsHit(nPlayer, anAdjacentShots(2)) > 0 Then
					If anAdjacentShots(3) <> eShotNone Then
						If aanCyber3ShotsHit(nPlayer, anAdjacentShots(3)) > 0 Then							
							aanCyber3ShotsHit(nPlayer, anAdjacentShots(2)) = aanCyber3ShotsHit(nPlayer, anAdjacentShots(2)) - 1
							aanCyber3ShotsHit(nPlayer, anAdjacentShots(3)) = aanCyber3ShotsHit(nPlayer, anAdjacentShots(3)) - 1
							bBossHit = True
						End If
					End If
					If False = bBossHit Then nMaxAdjacent = 2
				End If
			End If
		End If

		If bBossHit Then
			nValue = 40000
			nValue = nValue * nPlayfieldX
			If eShotNone <> nShot Then
				nValue = nValue * anComboX(nShot)
			End If
			anModeScore(eModeCyberWizard) = anModeScore(eModeCyberWizard) + nValue
			anScore(nPlayer) = anScore(nPlayer) + nValue
			aanModeShots(nPlayer, eModeCyberWizard) = aanModeShots(nPlayer, eModeCyberWizard) + 1
			nProgress = aanModeShots(nPlayer, eModeCyberWizard)
			If nProgress > 4 Then nProgress = 4
			ShowText "VILLAN HIT", WilliamsFormatNum(nValue) & "  " & (4 - nProgress) & "/4 HP", 2500, eVOPrioModeProgress
			LightShowStart eLightShowCyberLong, eVOPrioModeProgress

			If 1 = nProgress Then
				AddCallout "VO_cyber_showdown_damage1", eVOPrioModeProgress, eCharacterCyber
			ElseIf 2 = nProgress Then
				AddCallout "VO_hero_showdown_cyber_damage2", eVOPrioModeProgress, eCharacterHero
			ElseIf 3 = nProgress Then
				AddCallout "VO_cyber_showdown_damage3", eVOPrioModeProgress, eCharacterCyber
			ElseIf nProgress > 3 Then
				CyberMode3End False
				exit Sub
			End If
		Else
			nValue = 8000
			nValue = nValue * nPlayfieldX
			If eShotSideRamp = nShot Then
				nValue = nValue * 2
			ElseIf eShotNone <> nShot Then
				nValue = nValue * anComboX(nShot)
			End If
			aanCyber3ShotsHit(nPlayer, nShot) = aanCyber3ShotsHit(nPlayer, nShot) + 1
			If nMaxAdjacent < 2 Then
				If aanCyber3ShotsHit(nPlayer, nShot) > 1 Then
					nValue = nValue * 2
					ShowText "2 IN A COLUMN", WilliamsFormatNum(nValue), 2500, eVOPrioModeProgress
				Else
					ShowText "1 IN A ROW", WilliamsFormatNum(nValue), 2500, eVOPrioModeProgress
				End If
			Else
				nValue = nValue * 2
				ShowText "2 IN A ROW", WilliamsFormatNum(nValue), 2500, eVOPrioModeProgress
			End If
			anModeScore(eModeCyberWizard) = anModeScore(eModeCyberWizard) + nValue
			anScore(nPlayer) = anScore(nPlayer) + nValue
		End If
		
		FlashSweep eColorGreen
	End If

	For i = 0 to 7
		If eShotSideRamp <> i Then
			aoShotLightSeqs(i).stopPlay
			RemoveModeColor i, eColorGreen
			RemoveModeColor i, eColorRed
			RemoveMBallColor i, eColorGreen
			RemoveMBallColor i, eColorRed
			If i = anCyber3AttackShot(nPlayer) Then
				AddModeColor i, eColorRed
				AddMballColor i, eColorRed
				aoShotLightSeqs(i).Play SeqRandom,4,,999999999
			Else
				If 0 = aanCyber3ShotsHit(nPlayer, i) Then
					AddModeColor i, eColorGreen
				Elseif 1 = aanCyber3ShotsHit(nPlayer, i) Then
					AddMballColor i, eColorGreen
				Else
					AddModeColor i, eColorGreen
					AddMballColor i, eColorGreen
				End If
			End If
		End If
	Next
End Sub

Sub CyberMode3End(bDrained)
	Dim i
	For i = 0 to 7
		RemoveModeColor i, eColorGreen
		RemoveModeColor i, eColorRed
		RemoveMballColor i, eColorGreen
		RemoveMballColor i, eColorRed
		aoShotLightSeqs(i).stopPlay
	Next
	If Not bDrained Then
		aavShotsLit(nPlayer, eModeCyberWizard).Clear
		AddCallout "VO_hero_showdown_cyber_victory", eVOPrioModeEnd, eCharacterHero
		ShowText "CYBER SHOWDOWN", "TOTAL " & WilliamsFormatNum(anModeScore(eModeCyberWizard)), 3000, eVOPrioModeEnd
		avModesRunning(nPlayer).Remove eModeCyberWizard
		avModesPlayed(nPlayer).add eModeCyberWizard
		avPermaCombo(nPlayer).add eShotLeftLoop
		nModeSelected = an1BallModes(Int(Rnd * 12))
		ModeSelectNext
		CommonModeEnd
		CommonWizardEnd
	End If
end Sub

Sub CyberAdvanceLock
	dim i
	If aanLockProgress(nPlayer, eMballCyber) < SpinsForCyberLock _
	And False = IsWizardModeActive Then
		aanLockProgress(nPlayer, eMballCyber) = aanLockProgress(nPlayer, eMballCyber) + 1
		UpdateLEDs SpinsForCyberLock - aanLockProgress(nPlayer, eMballCyber)
		If aanLockProgress(nPlayer, eMballCyber) Mod 15 = 0 Then
			FlashOnce 2, eColorGreen
		end If
		If aanLockProgress(nPlayer, eMballCyber) >= SpinsForCyberLock Then
			For each i in aoDigit0Lights : i.state = 0 : i.state = 2 : Next
			For each i in aoDigit1Lights : i.state = 0 : i.state = 2 : Next
			For each i in aoDigit2Lights : i.state = 0 : i.state = 2 : Next
			AddCallout "VO_cyber_mball_ready", eVOPrioMballReady, eCharacterCyber
			LightShowStart eLightShowCyberShort, eVOPrioMballReady
			aanLocksLit(nPlayer, eMballCyber) = 1
			AddMballColor eShotLeftLoop, eColorGreen
		End If
	End If
End Sub

Sub CyberMballStart
	Dim i, bPlayShow
	bPlayShow = MBallStartCallout(eMballCyber)
	CommonMBallStart
	If nFirstMBallStarted = eModeNone Then nFirstMBallStarted = eModeCyberMBall
	If (BIP<2) Then DOF 222, DOFOn							   
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		aanLocksMade(nPlayer, eMballCyber) = 0
		aanLocksLit(nPlayer, eMballCyber) = 0
		aanLockProgress(nPlayer, eMballCyber) = 0
		ShowText "CYBER MULTIBALL",	"HIT GREEN SHOTS", 2500, eVOPrioMballStart
		If bPlayShow Then
			bBallHeld = True
			nTimeSkillPost = 3000
			nTimeDelayMultiball = 3000
			LightShowStart eLightShowCyberLong, eVOPrioMballStart
		End If
		FlashSweep eColorGreen
		AddBalls 1
	End If
	vCyberJackpots.Clear
	vCyberJackpots.add eShotUnderFlipper : vCyberJackpots.add eShotLeftLoop
	vCyberJackpots.add eShotSaucer : vCyberJackpots.add eShotLeftRamp
	vCyberJackpots.add eShotEarth : vCyberJackpots.add eShotRightRamp
	vCyberJackpots.add eShotRightLoop
	nCyberTimer = 5000
	anModeScore(eModeCyberMBall) = 0
	avModesRunning(nPlayer).add eModeCyberMBall
	If Not bInMode Then PlayModeMusic
	CyberMBallUpdate eShotNone
	nTimeBallSave = nTimeBallSave + cBallSaveMball
End Sub

Sub CyberMballUpdate(nShot)
	Dim i
	Dim bValidShot
	Dim nShotsLeft
	Dim nValue
	Dim nProgress
	Dim sTextTop, sTextBottom, sCallout

	bValidShot = False
	If nShot <> eShotNone Then
		If nCyberTimer < 1501 then
			If eShotSideRamp <> nShot Then bValidShot = True
		ElseIf vCyberJackpots.Contains(nShot) Then
			bValidShot = True
		End If
	End If

	If bValidShot Then
		If nCyberTimer < 1501 and false = vCyberJackpots.Contains(nShot) Then
			nProgress = 1
			vCyberJackpots.add eShotUnderFlipper : vCyberJackpots.add eShotLeftLoop
			vCyberJackpots.add eShotSaucer : vCyberJackpots.add eShotLeftRamp
			vCyberJackpots.add eShotEarth : vCyberJackpots.add eShotRightRamp
			vCyberJackpots.add eShotRightLoop
		Else
			nProgress = 8 - vCyberJackpots.Count
		End If
		aanMballJackpots(nPlayer, eMballCyber) = aanMballJackpots(nPlayer, eMballCyber) + nProgress
		nValue = 1500
		nValue = nValue * nPlayfieldX * nProgress * anComboX(nShot)
		Select Case nPlayfieldX * nProgress * anComboX(nShot)
			case 2
				sCallout = "VO_cyber_mball_jackpot02x"
				sTextTop = "DOUBLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 3
				sCallout = "VO_cyber_mball_jackpot03x"
				sTextTop = "TRIPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 4
				sCallout = "VO_cyber_mball_jackpot04x"
				sTextTop = "QUADRUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 5
				sCallout = "VO_cyber_mball_jackpot05x"
				sTextTop = "QUINTUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 6
				sCallout = "VO_cyber_mball_jackpot06x"
				sTextTop = "HEXTUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 7
				sCallout = "VO_cyber_mball_jackpot07x"
				sTextTop = "SEPTUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 8
				sCallout = "VO_cyber_mball_jackpot08x"
				sTextTop = "OCTUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 9
				sCallout = "VO_cyber_mball_jackpot09x"
				sTextTop = "NONUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 10
				sCallout = "VO_cyber_mball_jackpot10x"
				sTextTop = "DECUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 12
				sCallout = "VO_cyber_mball_jackpot12x"
				sTextTop = "DUODECUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 14
				sCallout = "VO_cyber_mball_jackpot14x"
				sTextTop = "QUATTUTODECUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 16
				sCallout = "VO_cyber_mball_jackpot16x"
				sTextTop = "HEXADECUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 18
				sCallout = "VO_cyber_mball_jackpot18x"
				sTextTop = "OCTODECUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 20
				sCallout = "VO_cyber_mball_jackpot20x"
				sTextTop = "VIGINTUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 21
				sCallout = "VO_cyber_mball_jackpot21x"
				sTextTop = "UNVIGINTUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 24
				sCallout = "VO_cyber_mball_jackpot24x"
				sTextTop = "QUATTUTOVIGIN-" : sTextBottom = "TUPLE JP " & WilliamsFormatNum(nValue)
			case 28
				sCallout = "VO_cyber_mball_jackpot28x"
				sTextTop = "OCTOVIGINTUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 30
				sCallout = "VO_cyber_mball_jackpot30x"
				sTextTop = "TRIGINTUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 35
				sCallout = "VO_cyber_mball_jackpot35x"
				sTextTop = "QUINTRIGIN-" : sTextBottom = "TUPLE JP " & WilliamsFormatNum(nValue)
			case 36
				sCallout = "VO_cyber_mball_jackpot36x"
				sTextTop = "HEXTRIGINTUPLE" : sTextBottom = "JACKPOT " & WilliamsFormatNum(nValue)
			case 42
				sCallout = "VO_cyber_mball_jackpot42x"
				sTextTop = "DUOQUADRAGIN-" : sTextBottom = "TUPLE JP " & WilliamsFormatNum(nValue)
			case Else
				sCallout = "VO_cyber_mball_jackpot01x"
				sTextTop = "CYBER JACKPOT" : sTextBottom = WilliamsFormatNum(nValue)
		End Select
		If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			TopSecretUpdate sTextTop, sTextBottom, nValue
		Else
			AddCallout sCallout, eVOPrioJackpot, eCharacterCyber
			ShowText sTextTop, sTextBottom, 2000, eVOPrioJackpot
			FlashSweep eColorGreen
			LightShowStart eLightShowCyberShort, eVOPrioJackpot
		End If
		anScore(nPlayer) = anScore(nPlayer) + nValue
		anModeScore(eModeCyberMBall) = anModeScore(eModeCyberMBall) + nValue
		vCyberJackpots.Remove nShot
		nCyberTimer = 5000
		If aanMballJackpots(nPlayer, eMballCyber) >= anJackpotsForSuper(eMballCyber) _
		And False = vSuperJPsLit.Contains(eMballCyber) Then
			AddCallout "VO_cyber_mball_sj_lit", eVOPrioMballReady, eCharacterCyber
			vSuperJPsLit.add eMballCyber
		End If
	End If

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	for i = 0 to 7 : RemoveMballColor i, eColorGreen : Next
	If anTimerMballGrace(eMballCyber) < 1 Then
		For i = 0 to 7
			If i <> eShotSideRamp Then
				If True = vCyberJackpots.Contains(i) Or nCyberTimer < 1501 Then
					AddMballColor i, eColorGreen
				End If
			End If
		Next
		nProgress = anJackpotsForSuper(eMballCyber) - aanMballJackpots(nPlayer, eMballCyber)
		If nProgress < 1 Then
			UpdateLEDs 0
		Else
			UpdateLEDs nProgress
		End If
		If vSuperJPsLit.Contains(eMballCyber) then AddMballColor eShotSideRamp, eColorGreen
	End If
End Sub

Sub CyberMballEnd
	Dim i
	for i = 0 to 7 : RemoveMballColor i, eColorGreen : Next
	vSuperJPsLit.Remove eMballCyber
	avModesRunning(nPlayer).Remove eModeCyberMBall
	DOF 222, DOFOff				
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		aanMballsPlayed(nPlayer, eMballCyber) = aanMballsPlayed(nPlayer, eMballCyber) + 1
	End If
	UpdateLEDs SpinsForCyberLock
	UpdateWizardModesLit
	If not IsMultiballActive then
		PlayModeMusic
		ShowMBallScore
	End If
End Sub

Sub WaterMode1Start
	aavShotsLit(nPlayer, eModeWater1).Clear
	aavShotsLit(nPlayer, eModeWater1).add eShotLeftLoop
	aavShotsLit(nPlayer, eModeWater1).add eShotLeftRamp
	aavShotsLit(nPlayer, eModeWater1).add eShotSideRamp
	aavShotsLit(nPlayer, eModeWater1).add eShotRightLoop
    If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		QueueDialogue "VO_water_mode1_start", eVOPrioModeStart, eCharacterWater
		QueueDialogue "VO_hero_mode_water1_start", eVOPrioModeStart, eCharacterHero
		LightShowStart eLightShowWaterShort, eVOPrioModeStart
	End If
	CommonModeStart eModeWater1
End Sub

Sub WaterMode1Update(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, nProgress

	bValidShot = False
	If eShotNone <> nShot Then
		If aavShotsLit(nPlayer, eModeWater1).Contains(nShot) Then
			bValidShot = True
		End If
	End If
			
	If bValidShot Then
		Select Case nShot
			Case eShotUnderFlipper
				aavShotsLit(nPlayer, eModeWater1).add eShotRightLoop
			Case eShotLeftLoop
				aavShotsLit(nPlayer, eModeWater1).add eShotRightRamp
			Case eShotSaucer
				aavShotsLit(nPlayer, eModeWater1).add eShotSideRamp
			Case eShotLeftRamp
				aavShotsLit(nPlayer, eModeWater1).add eShotEarth
			Case eShotEarth
				aavShotsLit(nPlayer, eModeWater1).add eShotLeftRamp
			Case eShotSideRamp
				aavShotsLit(nPlayer, eModeWater1).add eShotSaucer
			Case eShotRightRamp
				aavShotsLit(nPlayer, eModeWater1).add eShotLeftLoop
			Case eShotRightLoop
				aavShotsLit(nPlayer, eModeWater1).add eShotUnderFlipper
		End Select
		nValue = 12000
		nValue = nValue * nPlayfieldX
		If eShotSideRamp = nShot Then
			nValue = nValue * 2
		ElseIf eShotNone <> nShot Then
			If anComboX(nShot) > 1 Then ComboCallout
			nValue = nValue * anComboX(nShot)
		End If
		anModeScore(eModeWater1) = anModeScore(eModeWater1) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		aanModeShots(nPlayer, eModeWater1) = aanModeShots(nPlayer, eModeWater1) + 1
		nProgress = aanModeShots(nPlayer, eModeWater1)
		aavShotsLit(nPlayer, eModeWater1).Remove nShot
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			FlashSweep eColorBlue
			LightShowStart eLightShowWaterShort, eVOPrioModeProgress
		End If
		If (not avModesPlayed(nPlayer).Contains(eModeWater2)) _
		And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
			If nProgress > 6 Then nProgress = 6
			ShowText "SHARK ATTACK", WilliamsFormatNum(nValue) & " " & nProgress & "/6", 2500, eVOPrioModeProgress
			If nProgress > 5 Then
				WaterMode1End False
				exit Sub
			Else
				If nSFXPriority > eVOPrioModeProgress Then
					sSFXToPlay = "SFX_wave_sloosh" & Int(Rnd * 4)
					nSFXPriority = eVOPrioModeProgress
					fSFXQueuedVolume = 0.6
				End If
			End If
		Else
			If nProgress > 10 Then nProgress = 10
			If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				TopSecretUpdate "SHARK ATTACK", "", nValue
			Else
				ShowText "SHARK ATTACK", WilliamsFormatNum(nValue) & " " & nProgress & "/10", 2500, eVOPrioModeProgress
			End If
			If nProgress > 9 Then
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					aanModeShots(nPlayer, eModeWater1) = 0
				Else
					WaterMode1End False
					exit Sub
				End If
			Elseif not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				If nSFXPriority > eVOPrioModeProgress Then
					sSFXToPlay = "SFX_wave_sloosh" & Int(Rnd * 4)
					nSFXPriority = eVOPrioModeProgress
					fSFXQueuedVolume = 0.6
				End If
			End If
		End If
	End If

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	For i = 0 to 7
		If aavShotsLit(nPlayer, eModeWater1).Contains(i) Then
			AddModeColor i, eColorBlue
		Else
			RemoveModeColor i, eColorBlue
		End If
	Next
End Sub

Sub WaterMode1End(bDrained)
	Dim i
	For i = 0 to 7 : RemoveModeColor i, eColorBlue : Next
	If Not bDrained Then
		aavShotsLit(nPlayer, eModeWater1).Clear
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			ShowText "SHARK ATTACK", "TOTAL " & WilliamsFormatNum(anModeScore(eModeWater1)), 5089, eVOPrioModeEnd
			PlayJingle "SFX_mode_total_jingle", 5089
			AddDelayedCallout "VO_hero_mode_water1_victory", eVOPrioModeEnd, eCharacterHero, 2500
			If KickerLeft.BallCntOver < 1 Then LightShowStart eLightShowModeComplete, eVOPrioModeEnd
			avModesPlayed(nPlayer).add eModeWater1
		End If
		avModesRunning(nPlayer).Remove eModeWater1
		CommonModeEnd
	End If
end Sub

Sub WaterMode2Start
	Dim anShots, i
	for i = 0 to 7 : anTimeWater2(i) = 0 : Next
	anShots = getLeastRecentShots(7)
	anTimeWater2(anShots(0)) = 14000
	aavShotsLit(nPlayer, eModeWater2).add anShots(0)
	aanShotLastHit(nPlayer, anShots(0)) = GameTime - 2
	anTimeWater2(anShots(1)) = 11000
	aavShotsLit(nPlayer, eModeWater2).add anShots(1)
	aanShotLastHit(nPlayer, anShots(1)) = GameTime - 1
	anTimeWater2(anShots(2)) = 8000
	aavShotsLit(nPlayer, eModeWater2).add anShots(2)
    If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		QueueDialogue "VO_water_mode2_start", eVOPrioModeStart, eCharacterWater
		QueueDialogue "VO_hero_mode_water2_start", eVOPrioModeStart, eCharacterHero
		LightShowStart eLightShowWaterShort, eVOPrioModeStart
	End If
	CommonModeStart eModeWater2
	aanShotLastHit(nPlayer, anShots(2)) = GameTime 
End Sub

Sub WaterMode2Update(nShot, nSwitch)
	Dim i, j
	Dim nValue, bValidShot, nProgress, anShots

	bValidShot = False
	nProgress = aanModeShots(nPlayer, eModeWater2)
	If nShot <> eShotNone Then
		If aavShotsLit(nPlayer, eModeWater2).Contains(nShot) Then
			bValidShot = True
		End If
	End If

	If bValidShot Then
		nValue = 12000
		nValue = nValue * nPlayfieldX
		If eShotSideRamp = nShot Then
			nValue = nValue * 2
		ElseIf eShotNone <> nShot Then
			nValue = nValue * anComboX(nShot)
			If anComboX(nShot) > 1 Then ComboCallout
		End If
		anModeScore(eModeWater2) = anModeScore(eModeWater2) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		aavShotsLit(nPlayer, eModeWater2).Remove nShot
		aanModeShots(nPlayer, eModeWater2) = aanModeShots(nPlayer, eModeWater2) + 1
		nProgress = aanModeShots(nPlayer, eModeWater2)
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			FlashSweep eColorBlue
			LightShowStart eLightShowWaterShort, eVOPrioModeProgress
		End If
		If (not avModesPlayed(nPlayer).Contains(eModeWater1)) _
		And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
			ShowText "SUBMARINE LEAK", WilliamsFormatNum(nValue) & " " & nProgress & "/6", 2500, eVOPrioModeProgress
			If nProgress > 5 Then
				WaterMode2End False
				exit Sub
			Else
				If nSFXPriority > eVOPrioModeProgress Then
					sSFXToPlay = "SFX_wave_sloosh" & Int(Rnd * 4)
					nSFXPriority = eVOPrioModeProgress
					fSFXQueuedVolume = 0.6
				End If
			End If
		Else
			If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				TopSecretUpdate "SUBMARINE LEAK", "", nValue
			Else
				ShowText "SUBMARINE LEAK", WilliamsFormatNum(nValue) & " " & nProgress & "/10", 2500, eVOPrioModeProgress
			End If
			If nProgress > 9 Then
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					aanModeShots(nPlayer, eModeWater2) = 0
				Else
					WaterMode2End False
					exit Sub
				End If
			Elseif not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				If nSFXPriority > eVOPrioModeProgress Then
					sSFXToPlay = "SFX_wave_sloosh" & Int(Rnd * 4)
					nSFXPriority = eVOPrioModeProgress
					fSFXQueuedVolume = 0.6
				End If
			End If
		End If
	End If

	If aavShotsLit(nPlayer, eModeWater2).Count < 3 Then
		for i = 1 to (3 - aavShotsLit(nPlayer, eModeWater2).Count)
			anShots = getLeastRecentShots(7)
			For Each j in anShots
				If Not aavShotsLit(nPlayer, eModeWater2).Contains(j) Then
					aavShotsLit(nPlayer, eModeWater2).add j
					anTimeWater2(j) = 14000
					If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
						AddModeColor j, eColorBlue
					end if
					aanShotLastHit(nPlayer, j) = GameTime
					Exit For
				End If
			Next
		next
	End If
	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	For i = 0 to 7
		If aavShotsLit(nPlayer, eModeWater2).Contains(i) then
			AddModeColor i, eColorBlue
		Else
			RemoveModeColor i, eColorBlue
		End If
	Next
End Sub

Sub WaterMode2End(bDrained)
	Dim i
	For i = 0 to 7 : RemoveModeColor i, eColorBlue : Next
	If Not bDrained Then
		aavShotsLit(nPlayer, eModeWater2).Clear
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			ShowText "SUBMARINE LEAK", "TOTAL " & WilliamsFormatNum(anModeScore(eModeWater2)), 5089, eVOPrioModeEnd
			PlayJingle "SFX_mode_total_jingle", 5089
			AddDelayedCallout "VO_hero_mode_water2_victory", eVOPrioModeEnd, eCharacterHero, 2500
			If KickerLeft.BallCntOver < 1 Then LightShowStart eLightShowModeComplete, eVOPrioModeEnd
			avModesPlayed(nPlayer).add eModeWater2
		End If
		avModesRunning(nPlayer).Remove eModeWater2
		CommonModeEnd
	End If
end Sub

Sub WaterMode3Start
	Dim i, nBalls
	' TODO: Mode intro and rule explanation
	PlayModeMusic
	aanWater3TypesLeft(nPlayer, cTypeLoops) = 1
	aanWater3TypesLeft(nPlayer, cTypeRamps) = 1
	aanWater3TypesLeft(nPlayer, cTypeKickers) = 1
	aanWater3TypesLeft(nPlayer, cTypeTargets) = 6
	BIP = BIP + nBallPhysLocked
	nBalls = 3 - nBallPhysLocked
	nBallPhysLocked = 0
	bReleasingLocks = True
	UpPostMove LockPost,LockPostPrimitive,"up"
	LockPostTimer.Enabled = True
	If nBalls > 0 Then AddBalls nBalls
	bTurnClockwise = True
	TimerTurntable_Timer
	nTimeBallSave = cBallSaveStart
	LightShootAgain.state = LightStateBlinking
	avWizardModesLit(nPlayer).Remove eModeWaterWizard
	anWizardModeRunning(nPlayer) = eModeWaterWizard
	AddCallout "VO_water_showdown_start", eVOPrioModeStart, eCharacterWater
	LightShowStart eLightShowWaterShort, eVOPrioModeStart
	CommonModeStart eModeWaterWizard
	CommonWizardStart
End Sub

Sub WaterMode3Update(nShot, nSwitch)
	Dim bTargetsChanged
	Dim i, nValue, nShotHit, nProgress, nCount, sShotText, sStatus

	If eShotNone = nShot And eSwitchNone = nSwitch Then
		bTargetsChanged = True
	End If
	
	If eShotEarth = nShot Then
		UpPostMove LockPost,LockPostPrimitive,"down"
		LockPostTimer.Enabled = True
		bReleasingLocks = True
	End If

	nShotHit = eShotNone
	If eShotNone = nShot And aanWater3TypesLeft(nPlayer, cTypeTargets) > 0 Then
		If eSwitchDropI = nSwitch or eSwitchDropC = nSwitch or eSwitchDropE = nSwitch _
		or eSwitchTargetM4 = nSwitch Or eSwitchTargetM3 = nSwitch _
		Or eSwitchTargetM2 = nSwitch Or eSwitchTargetM1 = nSwitch _
		Or eSwitchTargetX4 = nSwitch Or eSwitchTargetX3 = nSwitch _
		Or eSwitchTargetX2 = nSwitch Or eSwitchTargetX1 = nSwitch _
		Or eSwitchTargetBlue = nSwitch Then
			nShotHit = cTypeTargets
			sShotText = "TARGET"
		End If
	End If

	If aanWater3TypesLeft(nPlayer, cTypeLoops) > 0 _
	And (eShotLeftLoop = nShot Or eShotRightLoop = nShot) Then
		nShotHit = cTypeLoops
		sShotText = "WHITE"
	Elseif aanWater3TypesLeft(nPlayer, cTypeRamps) > 0 _
	And (eShotLeftRamp = nShot Or eShotSideRamp = nShot Or eShotRightRamp = nShot) Then
		nShotHit = cTypeRamps
		sShotText = "GREEN"
	Elseif aanWater3TypesLeft(nPlayer, cTypeKickers) > 0 _
	And (eShotUnderFlipper = nShot Or eShotEarth = nShot Or eShotSaucer = nShot) Then
		nShotHit = cTypeKickers
		sShotText = "BLUE"
	End If

	If eShotNone <> nShotHit Then
		aanWater3TypesLeft(nPlayer, nShotHit) = aanWater3TypesLeft(nPlayer, nShotHit) - 1
		If cTypeTargets = nShotHit and aanWater3TypesLeft(nPlayer, cTypeTargets) < 1 Then
			bTargetsChanged = True
		End If
		nCount = 0
		For i = 0 to 3
			nCount = nCount + aanWater3TypesLeft(nPlayer, i)
		Next
		If nCount > 0 Then
			If nShotHit = cTypeTargets Then
				nValue = 2000
			Else
				nValue = 12000
				If eShotSideRamp = nShot Then
					nValue = nValue * 2
				ElseIf eShotNone <> nShot Then
					nValue = nValue * anComboX(nShot)
				End If
				LightShowStart eLightShowWaterShort, eVOPrioModeProgress
				FlashSweep eColorBlue
			End If
			If 0 = aanWater3TypesLeft(nPlayer, nShotHit) Then
				sSFXToPlay = "SFX_sonar_e"
				nSFXPriority = eVOPrioModeProgress
				fSFXQueuedVolume = 1.0
			End If
			nValue = nValue * nPlayfieldX
			anScore(nPlayer) = anScore(nPlayer) + nValue
			LightShowStart eLightShowWaterShort, eVOPrioModeProgress
			anModeScore(eModeWaterWizard) = anModeScore(eModeWaterWizard) + nValue
            sStatus = "W=" & aanWater3TypesLeft(nPlayer, cTypeLoops) _
					& " G=" & aanWater3TypesLeft(nPlayer, cTypeRamps) _
					& " B=" & aanWater3TypesLeft(nPlayer, cTypeKickers) _
					& " T=" & aanWater3TypesLeft(nPlayer, cTypeTargets)
			ShowText sShotText & " HIT " & WilliamsFormatNum(nValue), sStatus, 2500, eVOPrioModeProgress
		Else
			aanModeShots(nPlayer, eModeWaterWizard) = aanModeShots(nPlayer, eModeWaterWizard) + 1
			nProgress = aanModeShots(nPlayer, eModeWaterWizard)
			nValue = 20000 * nProgress
			If eShotSideRamp = nShot Then
				nValue = nValue * 2
			ElseIf eShotNone <> nShot Then
				nValue = nValue * anComboX(nShot)
			End If
			nValue = nValue * nPlayfieldX		
			anScore(nPlayer) = anScore(nPlayer) + nValue
			anModeScore(eModeWaterWizard) = anModeScore(eModeWaterWizard) + nValue
			ShowText "VILLAN HIT", WilliamsFormatNum(nValue) & "  " & (3 - nProgress) & "/3 HP", 2500, eVOPrioModeProgress
			If nProgress > 2 Then
				WaterMode3End False
				exit Sub
			Else
				LightShowStart eLightShowWaterLong, eVOPrioModeProgress
				sSFXToPlay = "SFX_sonar_b"
				nSFXPriority = eVOPrioModeProgress
				fSFXQueuedVolume = 1.0
				AddDelayedCallout "VO_water_showdown_damage" & nProgress, eVOPrioModeProgress, eCharacterWater, 1500
				For i = cTypeLoops to cTypeKickers
					aanWater3TypesLeft(nPlayer, i) = nProgress + 1
				Next
				aanWater3TypesLeft(nPlayer, cTypeTargets) = 6 * (nProgress + 1)
				bTargetsChanged = True
			End If
		End If
	End If

	UpdateLEDs -1
	If bTargetsChanged Then
		If aanWater3TypesLeft(nPlayer, cTypeTargets) > 0 Then
			For each i in aoXLights : i.BlinkInterval = 85 : Next
			aoXLights(1).BlinkPattern = "01" : aoXLights(3).BlinkPattern = "01"
			For each i in aoXLights : i.state = LightStateBlinking : Next
			For each i in aoBagLights : i.BlinkInterval = 85 : Next
			aoBagLights(1).BlinkPattern = "01" : aoBagLights(3).BlinkPattern = "01"
			For each i in aoBagLights : i.state = LightStateBlinking : Next
			For each i in aoDropLights : i.BlinkInterval = 85 : Next
			aoDropLights(1).BlinkPattern = "01"
			For each i in aoDropLights : i.state = LightStateBlinking : Next
			lightTorpedo.state = LightStateBlinking
		Else
			For each i in aoXLights : i.BlinkInterval = 170 : i.state = 0 :  Next
			For each i in aoXLights : i.BlinkPattern = "10" : i.state = 0 : Next
			For each i in aoBagLights : i.BlinkInterval = 170 : i.state = 0 : Next
			For each i in aoBagLights : i.BlinkPattern = "10" : i.state = 0 : Next
			For each i in aoDropLights : i.BlinkInterval = 170 : i.state = 0 : Next
			For each i in aoDropLights : i.BlinkPattern = "10" : i.state = 0 : Next
			lightTorpedo.BlinkInterval = 170 : lightTorpedo.state = 0 
		End If
	End If
	For i = 0 to 7
		ClearModeColor i
		ClearMBallColor i
	Next
	If aanWater3TypesLeft(nPlayer, cTypeLoops) > 0 Then
		AddModeColor eShotLeftLoop, eColorWhite
		AddMBallColor eShotLeftLoop, eColorWhite
		AddModeColor eShotRightLoop, eColorWhite
		AddMBallColor eShotRightLoop, eColorWhite
	End If
	If aanWater3TypesLeft(nPlayer, cTypeRamps) > 0 Then
		AddModeColor eShotLeftRamp, eColorGreen
		AddMBallColor eShotLeftRamp, eColorGreen
		AddModeColor eShotSideRamp, eColorGreen
		AddModeColor eShotRightRamp, eColorGreen
		AddMBallColor eShotRightRamp, eColorGreen
	End If
	If aanWater3TypesLeft(nPlayer, cTypeKickers) > 0 Then
		AddModeColor eShotUnderFlipper, eColorBlue
		AddMBallColor eShotUnderFlipper, eColorBlue
		AddModeColor eShotSaucer, eColorBlue
		AddMBallColor eShotSaucer, eColorBlue
		AddModeColor eShotEarth, eColorBlue
		AddMBallColor eShotEarth, eColorBlue
	End If
End Sub

Sub WaterMode3End (bDrained)
	Dim i
	For i = 0 to 7
		ClearModeColor i
		ClearMBallColor i
	Next
	For each i in aoXLights : i.BlinkInterval = 170 : i.state = 0 :  Next
	For each i in aoXLights : i.BlinkPattern = "10" : i.state = 0 : Next
	For each i in aoBagLights : i.BlinkInterval = 170 : i.state = 0 : Next
	For each i in aoBagLights : i.BlinkPattern = "10" : i.state = 0 : Next
	For each i in aoDropLights : i.BlinkInterval = 170 : i.state = 0 : Next
	For each i in aoDropLights : i.BlinkPattern = "10" : i.state = 0 : Next
	lightTorpedo.BlinkInterval = 170 : lightTorpedo.state = 0
	
	If Not bDrained Then
		aavShotsLit(nPlayer, eModeWaterWizard).Clear
		AddCallout "VO_water_showdown_damage3", eVOPrioModeEnd, eCharacterWater
		LightShowStart eLightShowWaterLong, eVOPrioModeEnd
		ShowText "WATER SHOWDOWN", "TOTAL " & WilliamsFormatNum(anModeScore(eModeWaterWizard)), 3000, eVOPrioModeEnd
		avModesRunning(nPlayer).Remove eModeWaterWizard
		avModesPlayed(nPlayer).add eModeWaterWizard
		avPermaCombo(nPlayer).add eShotSaucer
		nModeSelected = an1BallModes(Int(Rnd * 12))
		ModeSelectNext
		CommonModeEnd
		CommonWizardEnd
	End If
End Sub

Sub WaterAddShot
	Dim i, j, nShotsToLight, anShots, nHitsNeeded
	If avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
		If nWaterJackpots < 3 Then
			nHitsNeeded = 1
		Else
			nHitsNeeded = 2
		End If
		aanLockProgress(nPlayer, eMBallWater) = aanLockProgress(nPlayer, eMBallWater) + 1
		If aanLockProgress(nPlayer, eMBallWater) >= nHitsNeeded Then
			aanLockProgress(nPlayer, eMBallWater) = 0
			If nWaterJackpots < 7 Then
				nWaterJackpots = nWaterJackpots + 1
				ShowText "WATER JACKPOT", "ADDED", 2000, eVOPrioCombo
				WaterMBallUpdate eShotNone
			End If
		Else
			ShowText "1 MORE TO ADD", "WATER JACKPOT", 2000, eVOPrioCombo
		End If
	Else
		If aanLocksLit(nPlayer, eMballWater) > 0 then exit Sub
		anShots = getLeastRecentShots(7)
		If 0 = aanMballsPlayed(nPlayer, eMBallWater) Then
			nShotsToLight = 2
		Else
			nShotsToLight = 1
		End If
		For each i in anShots
			If i <> eShotSideRamp and i <> eShotSaucer and anTimerTorpedo(i) < 1 Then
				Select Case aanMballsPlayed(nPlayer, eMBallWater)
					Case 0, 1
						anTimerTorpedo(i) = 14000
					Case Else
						anTimerTorpedo(i) = 11500
				End Select
				AddMballColor i, eColorBlue
				aanShotLastHit(nPlayer, i) = GameTime
				nShotsToLight = nShotsToLight - 1
				If nShotsToLight < 1 Then
					PlaySound "SFX_torpedo_launch",0,1,0,0,0,1,1
					Exit For
				End If
			End If
		Next
	End If	
End Sub

Sub WaterAdvanceLock(nShot)
	Dim i, nLockNumber, nHitsNeeded
	If avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
		Exit Sub
	End If
	RemoveMballColor nShot, eColorBlue
	anTimerTorpedo(nShot) = 0
	nLockNumber = aanLocksMade(nPlayer, eMballWater) + aanLocksLit(nPlayer, eMballWater)
	Select Case aanMballsPlayed(nplayer, eMballWater)
		Case 0, 1, 2
			nHitsNeeded = 1
		Case Else
			nHitsNeeded = 2
	End Select
	aanLockProgress(nPlayer, eMBallWater) = aanLockProgress(nPlayer, eMBallWater) + 1
	If aanLockProgress(nPlayer, eMBallWater) >= nHitsNeeded Then
		aanLockProgress(nPlayer, eMBallWater) = 0
		If nLockNumber < 3 Then
			aanLocksLit(nPlayer, eMballWater) = aanLocksLit(nPlayer, eMballWater) + 1
			ShowText "WATER MULTIBALL", "LOCK " & (1 + nLockNumber) & " IS LIT", 2500, eVOPrioMballLock
			If 2 = nLockNumber Then
				AddCallout "VO_water_mball_ready", eVOPrioMballReady, eCharacterWater
			End If
			FlashOnce 2, eColorBlue
			LightShowStart eLightShowWaterShort, eVOPrioMballLock
			AddMballColor eShotSaucer, eColorBlue
		End If
		nLockNumber = aanLocksMade(nPlayer, eMballWater) + aanLocksLit(nPlayer, eMballWater)
		If nLockNumber > 0 Then
			for i = 0 to 7 : anTimerTorpedo(i) = 0 : next
			For Each i in Array(eShotLeftLoop, eShotUnderFlipper, eShotLeftRamp, eShotEarth, _
				eShotRightRamp, eShotRightLoop)
				RemoveMballColor i, eColorBlue
			Next
		End If
	Else
		ShowText "1 MORE TO LIGHT", "WATER LOCK", 2500, eVOPrioMballLock
	End If
	If nSFXPriority > eVOPrioJackpot Then
		sSFXToPlay = "SFX_torpedo_hit"
		nSFXPriority = eVOPrioJackpot
		fSFXQueuedVolume = 1.0
	End If
End Sub

Sub WaterLockBall
	If nSFXPriority > eVOPrioMballLock Then
		sSFXToPlay = "SFX_water_lock"
		nSFXPriority = eVOPrioMballLock
		fSFXQueuedVolume = 1.0
	End If
	aanLocksMade(nPlayer, eMballWater) = aanLocksMade(nPlayer, eMballWater) + 1
	aanLocksLit(nPlayer, eMballWater) = aanLocksLit(nPlayer, eMballWater) - 1
	If aanLocksLit(nPlayer, eMballWater) < 1 Then
		RemoveMballColor eShotSaucer, eColorBlue
	End If
	FlashOnce 2, eColorBlue
	If aanLocksMade(nPlayer, eMballWater) < 3 Then
		ShowText "WATER MULTIBALL", _	
			"BALL " & aanLocksMade(nPlayer, eMballWater) & " LOCKED", 2500, eVOPrioMballLock
		If 1 = aanLocksMade(nPlayer, eMballWater) Then
			AddCallout "VO_water_mball_lock1", eVOPrioMballLock, eCharacterWater
			if (BIP<2) Then DOF 243, DOFPulse									
		Else
			AddCallout "VO_water_mball_lock2", eVOPrioMballLock, eCharacterWater
			if (BIP<2) Then DOF 243, DOFPulse									
		End If
		LightShowStart eLightShowWaterShort, eVOPrioMballLock
		nTimeDelaySaucerUpper = 2000
	Else
		nTimeDelaySaucerUpper = 2000
	End If
End Sub

Sub WaterMballStart
	Dim i, bPlayShow
	bPlayShow = MBallStartCallout(eMBallWater)
	CommonMBallStart
	If (BIP<2) Then DOF 226, DOFOn							   
	If nFirstMBallStarted = eModeNone Then nFirstMBallStarted = eModeWaterMBall
	nTimeDelaySaucerUpper = 2000
	nTimerWaterMove = 3000
	nWaterFirstJackpot = eShotLeftLoop
	nWaterJackpots = 2
	for i = 0 to 7 : anTimerTorpedo(i) = 0 : next
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		ShowText "WATER MULTIBALL",	"HIT BLUE SHOTS", 2500, eVOPrioMballStart
		If bPlayShow Then
			bBallHeld = True
			nTimeDelaySaucerUpper = 3000
			nTimeDelayMultiball = 3000
			LightShowStart eLightShowWaterLong, eVOPrioMballStart
		End If
		FlashSweep eColorBlue
		AddBalls 2
		aanLocksLit(nPlayer, eMballWater) = 0
		aanLockProgress(nPlayer, eMballWater) = 0
		aanLocksMade(nPlayer, eMballWater) = 0
	End If
	anModeScore(eModeWaterMBall) = 0
	avModesRunning(nPlayer).add eModeWaterMBall
	If Not bInMode Then PlayModeMusic
	WaterMBallUpdate eShotNone
	nTimeBallSave = nTimeBallSave + cBallSaveMball
End Sub

Sub WaterMballUpdate(nShot)
	Dim i
	Dim bValidShot
	Dim nShotsLeft
	Dim nValue
	Dim nProgress
	Dim nNextShot, nLastShot

	bValidShot = False
	If nShot <> eShotNone and nShot <> eShotSideRamp Then
		nLastShot = (nWaterFirstJackpot + nWaterJackpots) - 1 
		If nLastShot > eShotEarth And nWaterFirstJackpot < eShotSideRamp Then
			nLastShot = (nLastShot + 1) Mod 8
		end If
		' If the sequence wraps around
		If nLastShot < nWaterFirstJackpot Then
			If nShot <= nLastShot Or nShot >= nWaterFirstJackpot Then
				bValidShot = True
			End If
		Else
			If nShot >= nWaterFirstJackpot And nShot <= nLastShot Then
				bValidShot = True
			End If
		End If
	End If

	If bValidShot Then
		aanMballJackpots(nPlayer, eMballWater) = aanMballJackpots(nPlayer, eMballWater) + 1
		nValue = 5000
		nValue = nValue * nPlayfieldX * anComboX(nShot)
		If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			TopSecretUpdate "WATER JACKPOT", "", nValue
		Else
			ShowText "WATER JACKPOT", WilliamsFormatNum(nValue), 2000, eVOPrioJackpot
			If nSFXPriority > eVOPrioJackpot Then
				sSFXToPlay = "SFX_water_jackpot"
				nSFXPriority = eVOPrioJackpot
				fSFXQueuedVolume = 1.0
			End If
			AddCallout "VO_water_mball_jackpot", eVOPrioJackpot, eCharacterWater
			FlashSweep eColorBlue
			LightShowStart eLightShowWaterShort, eVOPrioJackpot
		End If
		anScore(nPlayer) = anScore(nPlayer) + nValue
		anModeScore(eModeWaterMBall) = anModeScore(eModeWaterMBall) + nValue
		If nWaterJackpots > 1 Then nWaterJackpots = nWaterJackpots - 1
		If aanMballJackpots(nPlayer, eMballWater) >= anJackpotsForSuper(eMballWater) _
		And False = vSuperJPsLit.Contains(eMballWater) Then
			AddCallout "VO_water_mball_sj_lit", eVOPrioMballReady, eCharacterWater
			vSuperJPsLit.add eMballWater
		End If
	End If

	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		for i = 0 to 7 : RemoveMballColor i, eColorBlue : Next
	End If
	If anTimerMballGrace(eMballWater) < 1 Then
		nLastShot = (nWaterFirstJackpot + nWaterJackpots) - 1
		If nLastShot > eShotEarth And nWaterFirstJackpot < eShotSideRamp Then
			nLastShot = (nLastShot + 1) Mod 8
		end If

		For i = 0 to 7
			If i <> eShotSideRamp Then
				' If the sequence wraps around
				If nLastShot < nWaterFirstJackpot Then
					If i <= nLastShot Or i >= nWaterFirstJackpot Then
						If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
							AddMballColor i, eColorBlue
						End If
					End If
				Else
					If i >= nWaterFirstJackpot And i <= nLastShot Then
						If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
							AddMballColor i, eColorBlue
						End If
					End If
				End If
			End If
		Next
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			If vSuperJPsLit.Contains(eMballWater) then AddMballColor eShotSideRamp, eColorBlue
		End If
	End If
End Sub

Sub WaterMballEnd
	Dim i
	for i = 0 to 7 : RemoveMballColor i, eColorBlue : Next
	vSuperJPsLit.Remove eMballWater
	avModesRunning(nPlayer).Remove eModeWaterMBall
	DOF 226, DOFOff				
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		aanMballsPlayed(nPlayer, eMballWater) = aanMballsPlayed(nPlayer, eMballWater) + 1
	End If
	UpdateWizardModesLit
	If not IsMultiballActive then
		PlayModeMusic
		ShowMBallScore
	End If
End Sub

Sub FireMode1Start
    If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		AddCallout "VO_fire_mode1_start", eVOPrioModeStart, eCharacterFire
		LightShowStart eLightShowFireShort, eVOPrioModeStart
	End If
	CommonModeStart eModeFire1
	anFire1FirstShot(nPlayer) = eShotUnderFlipper
End Sub

Sub FireMode1Update(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, nProgress

	bValidShot = False
	nProgress = aanModeShots(nPlayer, eModeFire1)
	If nShot <> eShotNone Then
		If aavShotsLit(nPlayer, eModeFire1).Contains(nShot) Then
			bValidShot = True
		End If
		anFire1FirstShot(nPlayer) = (anFire1FirstShot(nPlayer) + 1) Mod 3
	End If

	If bValidShot Then
		nValue = 12000
		nValue = nValue * nPlayfieldX
		If eShotSideRamp = nShot Then
			nValue = nValue * 2
		ElseIf eShotNone <> nShot Then
			nValue = nValue * anComboX(nShot)
			If anComboX(nShot) > 1 Then ComboCallout
		End If
		anModeScore(eModeFire1) = anModeScore(eModeFire1) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		aanModeShots(nPlayer, eModeFire1) = aanModeShots(nPlayer, eModeFire1) + 1
		nProgress = aanModeShots(nPlayer, eModeFire1)
		
		If (not avModesPlayed(nPlayer).Contains(eModeFire2)) _
		And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
			ShowText "FLOOR IS LAVA", WilliamsFormatNum(nValue) & " " & nProgress & "/6", 2500, eVOPrioModeProgress
			If nProgress > 5 Then
				FireMode1End False
				exit Sub
			ElseIf not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				FlashSweep eColorRed
				LightShowStart eLightShowFireShort, eVOPrioModeProgress
				ModeProgressSound eModeFire1, nShot
			End If
		Else
			If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				TopSecretUpdate "FLOOR IS LAVA", "", nValue
			Else
				ShowText "FLOOR IS LAVA", WilliamsFormatNum(nValue) & " " & nProgress & "/10", 2500, eVOPrioModeProgress
			End If
			If nProgress > 9 Then
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					aanModeShots(nPlayer, eModeFire1) = 0
				Else
					FireMode1End False
					exit Sub
				End If
			ElseIf not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				FlashSweep eColorRed
				LightShowStart eLightShowFireShort, eVOPrioModeProgress
				ModeProgressSound eModeFire1, nShot
			End If
		End If
	End If

	aavShotsLit(nPlayer, eModeFire1).Clear
	Select Case anFire1FirstShot(nPlayer)
		Case 0
			aavShotsLit(nPlayer, eModeFire1).add eShotLeftLoop
			aavShotsLit(nPlayer, eModeFire1).add eShotRightRamp
			aavShotsLit(nPlayer, eModeFire1).add eShotSideRamp
		Case 1
			aavShotsLit(nPlayer, eModeFire1).add eShotUnderFlipper
			aavShotsLit(nPlayer, eModeFire1).add eShotLeftRamp
			aavShotsLit(nPlayer, eModeFire1).add eShotRightLoop
		Case Else
			aavShotsLit(nPlayer, eModeFire1).add eShotSaucer
			aavShotsLit(nPlayer, eModeFire1).add eShotEarth
	End Select
	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	For i = 0 to 7
		If aavShotsLit(nPlayer, eModeFire1).Contains(i) then
			AddModeColor i, eColorRed
		Else
			RemoveModeColor i, eColorRed
		End If
	Next
End Sub

Sub FireMode1End(bDrained)
	Dim i
	For i = 0 to 7 : RemoveModeColor i, eColorRed : Next
	If Not bDrained Then
		aavShotsLit(nPlayer, eModeFire1).Clear
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			ShowText "FLOOR IS LAVA", "TOTAL " & WilliamsFormatNum(anModeScore(eModeFire1)), 5089, eVOPrioModeEnd
			PlayJingle "SFX_mode_total_jingle", 5089
			If KickerLeft.BallCntOver < 1 Then LightShowStart eLightShowModeComplete, eVOPrioModeEnd
			AddDelayedCallout "VO_hero_mode_fire1_victory", eVOPrioModeEnd, eCharacterHero, 2500
			avModesPlayed(nPlayer).add eModeFire1
		end if
		avModesRunning(nPlayer).Remove eModeFire1
		CommonModeEnd
	End If
end Sub

Sub FireMode2Start
    If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		AddCallout "VO_fire_mode2_start", eVOPrioModeStart, eCharacterFire
		LightShowStart eLightShowFireShort, eVOPrioModeStart
	End If
	CommonModeStart eModeFire2
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		AddModeColor eShotLeftLoop, eColorRed
		AddModeColor eShotRightLoop, eColorRed
	End If
End Sub

Sub FireMode2Update(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, nProgress

	bValidShot = False
	If eSwitchNone <> nSwitch Then bValidShot = True
			
	If bValidShot Then
		nValue = 600
		nValue = nValue * nPlayfieldX
		anModeScore(eModeFire2) = anModeScore(eModeFire2) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		aanModeShots(nPlayer, eModeFire2) = aanModeShots(nPlayer, eModeFire2) + 1
		nProgress = aanModeShots(nPlayer, eModeFire2)
		If (not avModesPlayed(nPlayer).Contains(eModeFire1)) _
		And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
			If nProgress > 124 Then
				FireMode2End False
				exit Sub
			End If
		Else
			If nProgress > 199 And false = avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				FireMode2End False
				exit Sub
			End If
		End If
	End If

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	For i = 0 to 7 : RemoveModeColor i, eColorRed : Next
	AddModeColor eShotLeftLoop, eColorRed
	AddModeColor eShotRightLoop, eColorRed
End Sub

Sub FireMode2End(bDrained)
	Dim i
	For i = 0 to 7 : RemoveModeColor i, eColorRed : Next
	If Not bDrained Then
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			ShowText "FLYING EMBERS", "TOTAL " & WilliamsFormatNum(anModeScore(eModeFire2)), 5089, eVOPrioModeEnd
			If KickerLeft.BallCntOver < 1 Then LightShowStart eLightShowModeComplete, eVOPrioModeEnd
			PlayJingle "SFX_mode_total_jingle", 5089
			AddDelayedCallout "VO_hero_mode_fire2_victory", eVOPrioModeEnd, eCharacterHero, 2500
			avModesPlayed(nPlayer).add eModeFire2
		End If
		avModesRunning(nPlayer).Remove eModeFire2
		CommonModeEnd
	End If
end Sub

Sub FireMode3Start
	Dim i
	PlayModeMusic
	For i = 0 to 3 : aanFire3ShotOrder(nPlayer, i) = eShotNone : Next
	For i = 0 to 7 : aavShotsLit(nPlayer, eModeFireWizard).add i : Next
	anWizardModeRunning(nPlayer) = eModeFireWizard
	avWizardModesLit(nPlayer).Remove eModeFireWizard
	AddCallout "VO_fire_showdown_start", eVOPrioModeStart, eCharacterFire
	LightShowStart eLightShowFireLong, eVOPrioModeStart
	CommonModeStart eModeFireWizard
	CommonWizardStart
End Sub

Sub FireMode3Update(nShot, nSwitch)
	Dim i, j
	Dim nValue, bValidShot, nProgress, nTempShot, anColors, asColorNames, nShotX

	asColorNames = Array("BLUE", "GREEN", "YELLOW", "RED")
	bValidShot = False
	nProgress = aanModeShots(nPlayer, eModeFireWizard)
	If eShotNone = nShot Then
		' Do nothing, bValidShot remains false 
	ElseIf 0 = nProgress Then
		nShotX = 1
		bValidShot = True
		aanFire3ShotOrder(nPlayer, 0) = nShot
	Elseif 1 = nProgress Then
		nShotX = 1
		If nShot <> aanFire3ShotOrder(nPlayer, 0) Then
			bValidShot = True
			aanFire3ShotOrder(nPlayer, 1) = nShot
		End If
	Elseif 4 = nProgress Then
		nShotX = 1
		If nShot <> aanFire3ShotOrder(nPlayer, 0) _
		And nShot <> aanFire3ShotOrder(nPlayer, 1) Then
			bValidShot = True
			aanFire3ShotOrder(nPlayer, 2) = nShot
			for i = 0 to 1
				aavShotsLit(nPlayer, eModeFireWizard).add aanFire3ShotOrder(nPlayer, i)
			Next
		End If
	Elseif 8 = nProgress Then
		nShotX = 1
		If nShot <> aanFire3ShotOrder(nPlayer, 0) _
		And nShot <> aanFire3ShotOrder(nPlayer, 1) _
		And nShot <> aanFire3ShotOrder(nPlayer, 2) Then
			bValidShot = True
			aanFire3ShotOrder(nPlayer, 3) = nShot
			for i = 0 to 2
				aavShotsLit(nPlayer, eModeFireWizard).add aanFire3ShotOrder(nPlayer, i)
			Next
		End If
	Else
		For i = 0 to 3
			If aanFire3ShotOrder(nPlayer, i) <> eShotNone Then
				If nShot = aanFire3ShotOrder(nPlayer, i) _
				And aavShotsLit(nPlayer, eModeFireWizard).Contains(nShot) Then
					aavShotsLit(nPlayer, eModeFireWizard).Remove nShot
					nShotX = (i + 1) * 2
					bValidShot = True
					Exit For
				End If
			End If
		Next
	End If

	If bValidShot Then
		nValue = 8000
		nValue = nValue * nPlayfieldX * nShotX
		If eShotSideRamp = nShot Then
			nValue = nValue * 2
		ElseIf eShotNone <> nShot Then
			nValue = nValue * anComboX(nShot)
		End If
		anModeScore(eModeFireWizard) = anModeScore(eModeFireWizard) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		If nProgress = 0 Or nProgress = 1 Or nProgress = 4 Or nProgress = 8 Then
			ShowText "WHITE SHOT HIT", WilliamsFormatNum(nValue), 2500, eVOPrioModeProgress
		Else
			ShowText asColorNames((nShotX \ 2) - 1) & " SHOT HIT", WilliamsFormatNum(nValue), 2500, eVOPrioModeProgress
		End If

		If nProgress = 7 Or nProgress = 3 Or nProgress = 12 Then
			LightShowStart eLightShowFireLong, eVOPrioModeProgress
			If 3 = nProgress Then
				AddCallout "VO_fire_showdown_damage1", eVOPrioModeProgress, eCharacterFire
			Elseif 7 = nProgress Then
				AddCallout "VO_hero_showdown_fire_damage2", eVOPrioModeProgress, eCharacterHero
			End If
		Else
			LightShowStart eLightShowFireShort, eVOPrioModeProgress
		End If
		aanModeShots(nPlayer, eModeFireWizard) = aanModeShots(nPlayer, eModeFireWizard) + 1
		If aanModeShots(nPlayer, eModeFireWizard) > 12 Then
			FireMode3End False
		End If
		FlashSweep eColorRed
		' TODO: Sound
	End If

	UpdateLEDs -1
	For i = 0 to 7
		ClearModeColor i
		ClearMballColor i
	Next
	Select Case aanModeShots(nPlayer, eModeFireWizard)
		Case 0, 1, 4, 8
			For i = 0 to 7
				bValidShot = True
				For j = 0 to 3
					If aanFire3ShotOrder(nPlayer, j) = i Then
						bValidShot = False
						Exit For
					End if
				Next
				If bValidShot Then
					AddModeColor i, eColorWhite
					AddMballColor i, eColorWhite
				End If
			Next
		Case Else
			anColors = Array(eColorBlue, eColorGreen, eColorYellow, eColorRed)
			For i = 0 to 3
				nTempShot = aanFire3ShotOrder(nPlayer, i)
				If nTempShot <> eShotNone Then
					If aavShotsLit(nPlayer, eModeFireWizard).Contains(nTempShot) Then
						AddModeColor nTempShot, anColors(i)
						AddMballColor nTempShot, anColors(i)
					End If
				End If
			Next
	End Select
End Sub

Sub FireMode3End(bDrained)
	Dim i
	For i = 0 to 7
		ClearModeColor i
		ClearMballColor i
	Next
	If Not bDrained Then
		aavShotsLit(nPlayer, eModeFireWizard).Clear
		AddCallout "VO_hero_showdown_fire_victory", eVOPrioModeEnd, eCharacterHero
		LightShowStart eLightShowFireLong, eVOPrioModeEnd
		ShowText "FIRE SHOWDOWN", "TOTAL " & WilliamsFormatNum(anModeScore(eModeFireWizard)), 3000, eVOPrioModeEnd
		avModesRunning(nPlayer).Remove eModeFireWizard
		avModesPlayed(nPlayer).add eModeFireWizard
		avPermaCombo(nPlayer).add eShotLeftRamp
		nModeSelected = an1BallModes(Int(Rnd * 12))
		ModeSelectNext
		CommonModeEnd
		CommonWizardEnd
	End If
end Sub

Sub FireAdvanceLock
	Dim nHitsLeft
	Dim nLockNumber
	If avModesRunning(nPlayer).Contains(eModeFireMBall) _
	Or IsWizardModeActive Then
		Exit Sub
	End If
	nLockNumber = aanLocksLit(nPlayer, eMballFire) + aanLocksMade(nPlayer, eMballFire)
	FlashOnce 1, eColorRed
	FlashOnce 4, eColorRed
	LightShowStart eLightShowFireShort, eVOPrioMballLock
	If 0 = aanMballsPlayed(nplayer, eMballEarth) and nLockNumber < 3 Then
		aanLocksLit(nPlayer, eMballFire) = aanLocksLit(nPlayer, eMballFire) + 1
		ShowText "FIRE MULTIBALL", "LOCK " & (1 + nLockNumber) & " IS LIT", 2500, eVOPrioMballLock
		AddMballColor eShotLeftRamp, eColorRed
	ElseIf aanMballsPlayed(nplayer, eMballEarth) > 0 And aanLocksLit(nPlayer, eMballFire) < 1 Then
		aanLocksLit(nPlayer, eMballFire) = 1
		ShowText "FIRE MULTIBALL", "LOCK " & (1 + nLockNumber) & " IS LIT", 2500, eVOPrioMballLock
		AddMballColor eShotLeftRamp, eColorRed
		If 2 = nLockNumber then
			AddCallout "VO_fire_mball_ready", eVOPrioMballReady, eCharacterFire
		End If
	End if
	UpdateLightsFireLanes
End Sub

Sub FireLockBall
	aanLocksMade(nPlayer, eMballFire) = aanLocksMade(nPlayer, eMballFire) + 1
	aanLocksLit(nPlayer, eMballFire) = aanLocksLit(nPlayer, eMballFire) - 1
	If aanLocksLit(nPlayer, eMballFire) < 0 Then aanLocksLit(nPlayer, eMballFire) = 0
	UpPostMove LeftRampPost,LeftRampPostPrimitive,"up"
	nTimeLeftRampPost = 2000
	bBallHeld = True
	If aanMballsPlayed(nPlayer, eMballFire) > 0 then avFireLanes(nPlayer).Clear
	If aanLocksMade(nPlayer, eMballFire) < 3 Then
		ShowText "FIRE MULTIBALL", _	
			"BALL " & aanLocksMade(nPlayer, eMballFire) & " LOCKED", 2500, eVOPrioMballLock
		If aanLocksLit(nPlayer, eMballFire) < 1 Then RemoveMballColor eShotLeftRamp, eColorRed
		If aanLocksMade(nPlayer, eMballFire) < 2 Then
			AddCallout "VO_fire_mball_lock1", eVOPrioMballLock, eCharacterFire
			IF (BIP<2) THEN DOF 239, DOFPulse									
		Else
			AddCallout "VO_fire_mball_lock2", eVOPrioMballLock, eCharacterFire
			IF (BIP<2) THEN DOF 239, DOFPulse									
		End If
		FlashOnce 1, eColorRed
		LightShowStart eLightShowFireShort, eVOPrioMballLock
		vpmTimer.AddTimer 300, "FlashOnce 1, eColorRed '"
	Else
		RemoveMballColor eShotLeftRamp, eColorRed
		FireMBallStart
	End If
	UpdateLightsFireLanes
End Sub

Sub FireMBallStart
	Dim i, bPlayShow
	bPlayShow = MBallStartCallout(eMballFire)
	CommonMBallStart
	If nFirstMBallStarted = eModeNone Then nFirstMBallStarted = eModeFireMBall
	If (BIP<2) Then DOF 224, DOFOn							   
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		If bPlayShow Then
			bBallHeld = True
			nTimeLeftRampPost = 3000
			nTimeDelayMultiball = 3000
			LightShowStart eLightShowFireLong, eVOPrioMballStart
		End If
		ShowText "FIRE MULTIBALL",	"HIT RED SHOTS", 2500, eVOPrioMballStart
		FlashSweep eColorRed
		AddBalls 2
	End If
	for i = 0 to 7 : anFireSpreadTimer(i) = 0 : Next
	for i = 0 to 7 : abFireSpreadLeft(i) = True : Next
	anFireSpreadTimer(eShotSaucer) = 10000
	anFireSpreadTimer(eShotRightRamp) = 20000
	anFireSpreadTimer(eShotEarth) = 20000
	abFireSpreadLeft(eShotRightRamp) = False
	abFireSpreadLeft(eShotEarth) = False
	anModeScore(eModeFireMBall) = 0
	avModesRunning(nPlayer).add eModeFireMBall
	If Not bInMode Then PlayModeMusic
	FireMBallUpdate eShotNone
	nTimeBallSave = nTimeBallSave + cBallSaveMball
End Sub

Sub FireMBallUpdate(nShot)
	Dim i
	Dim bValidShot
	Dim nShotsLeft
	Dim nValue
	Dim nProgress

	bValidShot = False
	For Each i in Array(eShotLeftLoop, eShotSaucer, eShotLeftRamp, eShotEarth, _
		eShotRightRamp, eShotRightLoop)
		If i = nShot And anFireSpreadTimer(i) > 0 Then bValidShot = True
	Next

	If bValidShot Then
		nProgress = 0
		For Each i in Array(eShotLeftLoop, eShotSaucer, eShotLeftRamp, eShotEarth, _
			eShotRightRamp, eShotRightLoop)
			If anFireSpreadTimer(i) > 0 Then nProgress = nProgress + 1 
		Next
		Select Case nProgress
			Case 1, 2
				nValue = 6000
			Case 3, 4
				nValue = 4000
			Case Else
				nValue = 2000
		End Select
		nValue = nValue * nPlayfieldX * anComboX(nShot)
		If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			TopSecretUpdate "FIRE JACKPOT", "", nValue
		Else
			ShowText "FIRE JACKPOT", WilliamsFormatNum(nValue), 2000, eVOPrioJackpot
			AddCallout "VO_fire_mball_jackpot", eVOPrioJackpot, eCharacterFire
			LightShowStart eLightShowFireShort, eVOPrioJackpot
			FlashSweep eColorRed
		End If
		anFireSpreadTimer(nShot) = 0
		anScore(nPlayer) = anScore(nPlayer) + nValue
		anModeScore(eModeFireMBall) = anModeScore(eModeFireMBall) + nValue
		aanMballJackpots(nPlayer, eMballFire) = aanMballJackpots(nPlayer, eMballFire) + 1
		If nProgress < 2 Then
			AddCallout "VO_fire_mball_sj_ready", eVOPrioMballReady, eCharacterFire
			vSuperJPsLit.add eMballFire
			for i = 0 to 7 : abFireSpreadLeft(i) = True : Next
			anFireSpreadTimer(eShotSaucer) = 10000
			anFireSpreadTimer(eShotRightRamp) = 20000
			anFireSpreadTimer(eShotEarth) = 20000
			abFireSpreadLeft(eShotRightRamp) = False
			abFireSpreadLeft(eShotEarth) = False
		End If
	End If

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	for i = 0 to 7 : RemoveMballColor i, eColorRed : Next
	If anTimerMballGrace(eMballFire) < 1 Then
		For Each i in Array(eShotLeftLoop, eShotSaucer, eShotLeftRamp, eShotEarth, _
			eShotRightRamp, eShotRightLoop)
			If anFireSpreadTimer(i) > 0 Then AddMballColor i, eColorRed
		Next
		If vSuperJPsLit.Contains(eMballFire) then AddMballColor eShotSideRamp, eColorRed
	End If
End Sub

Sub FireMBallEnd
	Dim i
	vSuperJPsLit.Remove eMballFire
	avModesRunning(nPlayer).Remove eModeFireMBall
	DOF 224, DOFOff				
	For Each i in aoShotLightsMBall
		i.BlinkInterval = 130
	Next
	for i = 0 to 7 : RemoveMballColor i, eColorRed : Next
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		aanMballsPlayed(nPlayer, eMballFire) = aanMballsPlayed(nPlayer, eMballFire) + 1
		aanLocksMade(nPlayer, eMballFire) = 0
		aanLocksLit(nPlayer, eMballFire) = 0
		avFireLanes(nPlayer).Clear
	End If
	UpdateLightsFireLanes
	UpdateWizardModesLit
	If not IsMultiballActive then
		PlayModeMusic
		ShowMBallScore
	End If
End Sub

Sub EarthMode1Start
	aavShotsLit(nPlayer, eModeEarth1).Clear
	aavShotsLit(nPlayer, eModeEarth1).add eShotUnderFlipper
	aavShotsLit(nPlayer, eModeEarth1).add eShotLeftLoop
	aavShotsLit(nPlayer, eModeEarth1).add eShotEarth
	aavShotsLit(nPlayer, eModeEarth1).add eShotSideRamp
    If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		AddCallout "VO_earth_mode1_start", eVOPrioModeStart, eCharacterEarth
		LightShowStart eLightShowEarthShort, eVOPrioModeStart
	End If
	CommonModeStart eModeEarth1
End Sub

Sub EarthMode1Update(nShot, nSwitch)
	Dim i
	Dim nValue, nProgress, anUnlitShots

	If eShotNone <> nShot Then
		If aavShotsLit(nPlayer, eModeEarth1).Contains(nShot) Then
			Select Case nShot
				Case eShotUnderFlipper, eShotLeftLoop
					If aavShotsLit(nPlayer, eModeEarth1).Contains(eShotSaucer) Then
						anUnlitShots = Array(eShotEarth, eShotSideRamp, eShotRightRamp, eShotRightLoop)
					Elseif aavShotsLit(nPlayer, eModeEarth1).Contains(eShotEarth) Then
						anUnlitShots = Array(eShotSaucer, eShotLeftRamp, eShotRightRamp, eShotRightLoop)
					Else
						anUnlitShots = Array(eShotSaucer, eShotLeftRamp, eShotEarth, eShotSideRamp)
					End If
					aavShotsLit(nPlayer, eModeEarth1).Remove eShotUnderFlipper
					aavShotsLit(nPlayer, eModeEarth1).Remove eShotLeftLoop
				Case eShotSaucer, eShotLeftRamp
					If aavShotsLit(nPlayer, eModeEarth1).Contains(eShotUnderFlipper) Then
						anUnlitShots = Array(eShotEarth, eShotSideRamp, eShotRightRamp, eShotRightLoop)
					Elseif aavShotsLit(nPlayer, eModeEarth1).Contains(eShotEarth) Then
						anUnlitShots = Array(eShotUnderFlipper, eShotLeftLoop, eShotRightRamp, eShotRightLoop)
					Else
						anUnlitShots = Array(eShotUnderFlipper, eShotLeftLoop, eShotEarth, eShotSideRamp)
					End If
					aavShotsLit(nPlayer, eModeEarth1).Remove eShotSaucer
					aavShotsLit(nPlayer, eModeEarth1).Remove eShotLeftRamp
				Case eShotEarth, eShotSideRamp
					If aavShotsLit(nPlayer, eModeEarth1).Contains(eShotUnderFlipper) Then
						anUnlitShots = Array(eShotSaucer, eShotLeftRamp, eShotRightRamp, eShotRightLoop)
					Elseif aavShotsLit(nPlayer, eModeEarth1).Contains(eShotSaucer) Then
						anUnlitShots = Array(eShotUnderFlipper, eShotLeftLoop, eShotRightRamp, eShotRightLoop)
					Else
						anUnlitShots = Array(eShotUnderFlipper, eShotLeftLoop, eShotSaucer, eShotLeftRamp)
					End If
					aavShotsLit(nPlayer, eModeEarth1).Remove eShotEarth
					aavShotsLit(nPlayer, eModeEarth1).Remove eShotSideRamp
				Case Else
					If aavShotsLit(nPlayer, eModeEarth1).Contains(eShotUnderFlipper) Then
						anUnlitShots = Array(eShotSaucer, eShotLeftRamp, eShotEarth, eShotSideRamp)
					Elseif aavShotsLit(nPlayer, eModeEarth1).Contains(eShotSaucer) Then
						anUnlitShots = Array(eShotUnderFlipper, eShotLeftLoop, eShotEarth, eShotSideRamp)
					Else
						anUnlitShots = Array(eShotUnderFlipper, eShotLeftLoop, eShotSaucer, eShotLeftRamp)
					End If
					aavShotsLit(nPlayer, eModeEarth1).Remove eShotRightRamp
					aavShotsLit(nPlayer, eModeEarth1).Remove eShotRightLoop
			End Select
			If aanModeShots(nPlayer, eModeEarth1) Mod 2 = 0 Then
				aavShotsLit(nPlayer, eModeEarth1).add anUnlitShots(0)
				aavShotsLit(nPlayer, eModeEarth1).add anUnlitShots(1)
			Else
				aavShotsLit(nPlayer, eModeEarth1).add anUnlitShots(2)
				aavShotsLit(nPlayer, eModeEarth1).add anUnlitShots(3)
			End If

			nValue = 12000
			nValue = nValue * nPlayfieldX
			If eShotSideRamp = nShot Then
				nValue = nValue * 2
			ElseIf eShotNone <> nShot Then
				nValue = nValue * anComboX(nShot)
				If anComboX(nShot) > 1 Then ComboCallout
			End If
			anModeScore(eModeEarth1) = anModeScore(eModeEarth1) + nValue
			anScore(nPlayer) = anScore(nPlayer) + nValue
			aanModeShots(nPlayer, eModeEarth1) = aanModeShots(nPlayer, eModeEarth1) + 1
			nProgress = aanModeShots(nPlayer, eModeEarth1)
			If (not avModesPlayed(nPlayer).Contains(eModeEarth2)) _
			And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
				If nProgress > 6 Then nProgress = 6
				ShowText "NOT A DRILL", WilliamsFormatNum(nValue) & " " & nProgress & "/6", 2500, eVOPrioModeProgress
				If nProgress > 5 Then
					EarthMode1End False
					exit Sub
				Else
					FlashSweep eColorYellow
					LightShowStart eLightShowEarthShort, eVOPrioModeProgress
					ModeProgressSound eModeEarth1, nShot
				End If
			Else
				If nProgress > 10 Then nProgress = 10
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					TopSecretUpdate "NOT A DRILL", "", nValue
				Else
					ShowText "NOT A DRILL", WilliamsFormatNum(nValue) & " " & nProgress & "/10", 2500, eVOPrioModeProgress
				End If
				If nProgress > 9 Then
					If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
						aanModeShots(nPlayer, eModeEarth1) = 0
					Else
						EarthMode1End False
						exit Sub
					End If
				ElseIf Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					FlashSweep eColorYellow
					LightShowStart eLightShowEarthShort, eVOPrioModeProgress
					ModeProgressSound eModeEarth1, nShot
				End If
			End If
		End if
	End If

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	For i = 0 to 7
		If aavShotsLit(nPlayer, eModeEarth1).Contains(i) Then
			AddModeColor i, eColorOrange
		Else
			RemoveModeColor i, eColorOrange
		End If
	Next
End Sub

Sub EarthMode1End(bDrained)
	Dim i
	For i = 0 to 7 : RemoveModeColor i, eColorOrange : Next
	If Not bDrained Then
		aavShotsLit(nPlayer, eModeEarth1).Clear
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			ShowText "NOT A DRILL", "TOTAL " & WilliamsFormatNum(anModeScore(eModeEarth1)), 5089, eVOPrioModeEnd
			PlayJingle "SFX_mode_total_jingle", 5089
			AddDelayedCallout "VO_hero_mode_earth1_victory", eVOPrioModeEnd, eCharacterHero, 2500
			If KickerLeft.BallCntOver < 1 Then LightShowStart eLightShowModeComplete, eVOPrioModeEnd
			avModesPlayed(nPlayer).add eModeEarth1
		End If
		avModesRunning(nPlayer).Remove eModeEarth1
		CommonModeEnd
	End If
end Sub

Sub EarthMode2Start
    If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		AddCallout "VO_earth_mode2_start", eVOPrioModeStart, eCharacterEarth
		LightShowStart eLightShowEarthShort, eVOPrioModeStart
	End If
	CommonModeStart eModeEarth2
End Sub

Sub EarthMode2Update(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, nProgress

	bValidShot = False
	If eShotNone <> nShot Then
		If aavShotsLit(nPlayer, eModeEarth2).Contains(nShot) Then
			bValidShot = True
		End If
	Elseif eSwitchDropI = nSwitch or eSwitchDropC = nSwitch or eSwitchDropE = nSwitch Then
		aavShotsLit(nPlayer, eModeEarth2).add eShotRightLoop
	Elseif eSwitchTargetM4 = nSwitch Then 
		aavShotsLit(nPlayer, eModeEarth2).add eShotRightRamp
	Elseif eSwitchTargetM3 = nSwitch Then 
		aavShotsLit(nPlayer, eModeEarth2).add eShotSideRamp
	Elseif eSwitchTargetX3 = nSwitch or eSwitchTargetX4 = nSwitch Then
		aavShotsLit(nPlayer, eModeEarth2).add eShotEarth
	Elseif eSwitchTargetM2 = nSwitch Then 
		aavShotsLit(nPlayer, eModeEarth2).add eShotLeftRamp
	Elseif eSwitchTargetBlue = nSwitch Then 
		aavShotsLit(nPlayer, eModeEarth2).add eShotSaucer
	Elseif eSwitchTargetM1 = nSwitch Then 
		aavShotsLit(nPlayer, eModeEarth2).add eShotLeftLoop
	Elseif eSwitchTargetX1 = nSwitch or eSwitchTargetX2 = nSwitch Then
		aavShotsLit(nPlayer, eModeEarth2).add eShotUnderFlipper
	End If
			
	If bValidShot Then
		nValue = 12000
		nValue = nValue * nPlayfieldX
		If eShotSideRamp = nShot Then
			nValue = nValue * 2
		ElseIf eShotNone <> nShot Then
			nValue = nValue * anComboX(nShot)
			If anComboX(nShot) > 1 Then ComboCallout
		End If
		anModeScore(eModeEarth2) = anModeScore(eModeEarth2) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		aanModeShots(nPlayer, eModeEarth2) = aanModeShots(nPlayer, eModeEarth2) + 1
		nProgress = aanModeShots(nPlayer, eModeEarth2)
		aavShotsLit(nPlayer, eModeEarth2).Remove nShot
		If (not avModesPlayed(nPlayer).Contains(eModeEarth1)) _
		And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
			If nProgress > 6 Then nProgress = 6
			ShowText "LIGHTS OUT", WilliamsFormatNum(nValue) & " " & nProgress & "/6", 2500, eVOPrioModeProgress
			If nProgress > 5 Then
				EarthMode2End False
				exit Sub
			Else
				FlashSweep eColorYellow
				LightShowStart eLightShowEarthShort, eVOPrioModeProgress
				ModeProgressSound eModeEarth2, nShot
			End If
		Else
			If nProgress > 10 Then nProgress = 10
			If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				TopSecretUpdate "LIGHTS OUT", "", nValue
			Else
				ShowText "LIGHTS OUT", WilliamsFormatNum(nValue) & " " & nProgress & "/10", 2500, eVOPrioModeProgress
			End If
			If nProgress > 9 Then
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					aanModeShots(nPlayer, eModeEarth2) = 0
				Else
					EarthMode2End False
					exit Sub
				End If
			ElseIf Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				FlashSweep eColorYellow
				LightShowStart eLightShowEarthShort, eVOPrioModeProgress
				ModeProgressSound eModeEarth2, nShot
			End If
		End If
	End If

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	For i = 0 to 7
		If aavShotsLit(nPlayer, eModeEarth2).Contains(i) Then
			AddModeColor i, eColorOrange
		Else
			RemoveModeColor i, eColorOrange
		End If
	Next
End Sub

Sub EarthMode2End(bDrained)
	Dim i
	For i = 0 to 7 : RemoveModeColor i, eColorOrange : Next
	If Not bDrained Then
		aavShotsLit(nPlayer, eModeEarth2).Clear
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			ShowText "LIGHTS OUT", "TOTAL " & WilliamsFormatNum(anModeScore(eModeEarth2)), 5089, eVOPrioModeEnd
			PlayJingle "SFX_mode_total_jingle", 5089
			AddDelayedCallout "VO_hero_mode_earth2_victory", eVOPrioModeEnd, eCharacterHero, 2500
			If KickerLeft.BallCntOver < 1 Then LightShowStart eLightShowModeComplete, eVOPrioModeEnd
			avModesPlayed(nPlayer).add eModeEarth2
		End If
		avModesRunning(nPlayer).Remove eModeEarth2
		CommonModeEnd
	End If
end Sub

Sub EarthMode3Start
	Dim i, nBalls
	' TODO: Mode intro and rule explanation
	PlayModeMusic
	For i = 0 to 7 : aavShotsLit(nPlayer, eModeEarthWizard).add i : Next
	anEarth3Misses(nPlayer) = 0
	BIP = BIP + nBallPhysLocked
	nBalls = 3 - nBallPhysLocked
	nBallPhysLocked = 0
	bReleasingLocks = True
	UpPostMove LockPost,LockPostPrimitive,"down"
	LockPostTimer.Enabled = True
	If nBalls > 0 Then AddBalls nBalls
	nTimeBallSave = cBallSaveStart
	LightShootAgain.state = LightStateBlinking
	AddCallout "VO_earth_showdown_start", eVOPrioModeStart, eCharacterEarth
	LightShowStart eLightShowEarthLong, eVOPrioModeStart
	anWizardModeRunning(nPlayer) = eModeEarthWizard
	avWizardModesLit(nPlayer).Remove eModeEarthWizard
	CommonModeStart eModeEarthWizard
	CommonWizardStart
End Sub

Sub EarthMode3Update(nShot, nSwitch)
	Dim i, nValue, bValidShot, nShotCount, nShotsLeft, nProgress

	bValidShot = False
	If eSwitchLock1 = nSwitch Then
		bValidShot = True
	Elseif eShotNone <> nShot and nTimeEarth3Open < 1 Then
		If aavShotsLit(nPlayer, eModeEarthWizard).Contains(nShot) Then
			bValidShot = True
		End If
	End If

	If bValidShot Then
		nProgress = aanModeShots(nPlayer, eModeEarthWizard)
		FlashSweep eColorOrange
		If eSwitchLock1 = nSwitch Then
			If anEarth3Misses(nPlayer) < 1 Then
				nValue = 50000
			Elseif anEarth3Misses(nPlayer) < 2 Then
				nValue = 36000
			Else
				nValue = 24000
			End If
			nValue = nValue * anComboX(eShotEarth)
			nValue = nValue * nPlayfieldX		
			anScore(nPlayer) = anScore(nPlayer) + nValue
			anModeScore(eModeEarthWizard) = anModeScore(eModeEarthWizard) + nValue
			aanModeShots(nPlayer, eModeEarthWizard) = aanModeShots(nPlayer, eModeEarthWizard) + 1
			nProgress = aanModeShots(nPlayer, eModeEarthWizard)
			ShowText "VILLAN HIT", WilliamsFormatNum(nValue) & "  " & (3 - nProgress) & "/3 HP", 2500, eVOPrioModeProgress
			anEarth3Misses(nPlayer) = 0
			nTimeEarth3Open = 0
			LightSeqEarth.StopPlay
			For i = 0 to 7 : aavShotsLit(nPlayer, eModeEarthWizard).add i : Next
			If aanModeShots(nPlayer, eModeEarthWizard) > 1 Then
				aavShotsLit(nPlayer, eModeEarthWizard).Remove eShotLeftRamp
			End If
			If aanModeShots(nPlayer, eModeEarthWizard) > 0 Then
				aavShotsLit(nPlayer, eModeEarthWizard).Remove eShotEarth
			End If
			If aanModeShots(nPlayer, eModeEarthWizard) > 2 Then
				EarthMode3End False
				exit Sub
			Else
				AddCallout "VO_earth_showdown_damage" & nProgress, eVOPrioModeProgress, eCharacterEarth
				LightShowStart eLightShowEarthLong, eVOPrioModeProgress
			End If
		Else
			If anEarth3Misses(nPlayer) < 1 Then
				nValue = 12000
			Elseif anEarth3Misses(nPlayer) < 2 Then
				nValue = 8000
			Else
				nValue = 6000
			End If
			If eShotSideRamp = nShot Then
				nValue = nValue * 2
			ElseIf eShotNone <> nShot Then
				nValue = nValue * anComboX(nShot)
			End If
			nValue = nValue * nPlayfieldX
			anScore(nPlayer) = anScore(nPlayer) + nValue
			LightShowStart eLightShowEarthShort, eVOPrioModeProgress
			FlashSweep eColorYellow
			anModeScore(eModeEarthWizard) = anModeScore(eModeEarthWizard) + nValue
			aavShotsLit(nPlayer, eModeEarthWizard).Remove nShot
			nShotCount = (8 - nProgress) - aavShotsLit(nPlayer, eModeEarthWizard).Count
			Select Case anEarth3Misses(nPlayer)
				Case 0
					nShotsLeft = 4 - nShotCount
				Case 1, 2
					nShotsLeft = 2 - nShotCount
				Case Else
					nShotsLeft = 1 - nShotCount
			End Select
			If nShotsLeft < 1 Then
				bTurnClockwise = True
				TimerTurntable_Timer
				nTimeEarth3Open = 17499
				If anEarth3Misses(nPlayer) > 1 Then
					nTimeEarth3Open = nTimeEarth3Open + (10000 * (anEarth3Misses(nPlayer) - 1))
				End If
				ShowText WilliamsFormatNum(nValue) & " - SHOOT", "THE PASSAGE NOW", 2500, eVOPrioModeProgress
				AddCallout "VO_earth_showdown_open", eVOPrioModeProgress, eCharacterEarth
			Else
				ShowText WilliamsFormatNum(nValue) & " - " & nShotsLeft &  " MORE", "TO OPEN PASSAGE", 2500, eVOPrioModeProgress
			End If
		End If
	End If

	For i = 0 to 7
		ClearModeColor i
		ClearMBallColor i
	Next
	If nTimeEarth3Open > 0 Then
		AddModeColor eShotEarth, eColorOrange
		AddMBallColor eShotEarth, eColorOrange
		If nTimeEarth3Open < 5000 then
			LightSeqEarth.StopPlay
			LightSeqEarth.play SeqBlinking,,999,50
		Else
			LightSeqEarth.StopPlay
			LightSeqEarth.play SeqBlinking,,999,100
		End If
	Else
		UpdateLEDs -1
		LightSeqEarth.StopPlay
		For i = 0 to 7
			If aavShotsLit(nPlayer, eModeEarthWizard).Contains(i) Then
				AddModeColor i, eColorOrange
				AddMBallColor i, eColorOrange
			End If
		Next
	End If
End Sub

Sub EarthMode3End(bDrained)
	Dim i
	For i = 0 to 7
		ClearModeColor i
		ClearMBallColor i
	Next
	LightSeqEarth.StopPlay

	If Not bDrained Then
		aavShotsLit(nPlayer, eModeEarthWizard).Clear
		AddCallout "VO_hero_showdown_earth_victory", eVOPrioModeEnd, eCharacterHero
		ShowText "EARTH SHOWDOWN", "TOTAL " & WilliamsFormatNum(anModeScore(eModeEarthWizard)), 3000, eVOPrioModeEnd
		LightShowStart eLightShowEarthLong, eVOPrioModeEnd
		avModesRunning(nPlayer).Remove eModeEarthWizard
		avModesPlayed(nPlayer).add eModeEarthWizard
		avPermaCombo(nPlayer).add eShotEarth
		nModeSelected = an1BallModes(Int(Rnd * 12))
		ModeSelectNext
		CommonModeEnd
		CommonWizardEnd
	Elseif True = bDrained and nTimeEarth3Open > 0  Then
		nTimeEarth3Open = 0
		anEarth3Misses(nPlayer) = anEarth3Misses(nPlayer) + 1
		For i = 0 to 7 : aavShotsLit(nPlayer, eModeEarthWizard).add i : Next
		If aanModeShots(nPlayer, eModeEarthWizard) > 1 Then
			aavShotsLit(nPlayer, eModeEarthWizard).Remove eShotLeftRamp
		End If
		If aanModeShots(nPlayer, eModeEarthWizard) > 0 Then
			aavShotsLit(nPlayer, eModeEarthWizard).Remove eShotEarth
		End If
	End If
End Sub

Sub EarthAdvanceLock
	Dim nHitsLeft
	If aanLocksLit(nPlayer, eMballEarth) > 0 _
	Or true = avModesRunning(nPlayer).Contains(eModeEarthMBall)_
	Or true = IsWizardModeActive Then
		Exit Sub
	End If
	If 0 = aanMballsPlayed(nplayer, eMballEarth) Then
		ShowText "EARTH LAIR", "LOCK IS LIT", 2500, eVOPrioMballLock
		FlashOnce 3, eColorYellow
		LightShowStart eLightShowEarthShort, eVOPrioMballLock
		aanLocksLit(nPlayer, eMballEarth) = 1
		AddMballColor eShotEarth, eColorOrange
	Else
		aanLockProgress(nPlayer, eMballEarth) = aanLockProgress(nPlayer, eMballEarth) + 1
		If aanMballsPlayed(nPlayer, eMballEarth) < 3 Then
			nHitsLeft = 2 - aanLockProgress(nPlayer, eMballEarth)
		Else
			nHitsLeft = 3 - aanLockProgress(nPlayer, eMballEarth)
		End If
		If 1 = nHitsLeft Then
			ShowText "1 MORE HIT TO", "LIGHT EARTH LOCK", 2500, eVOPrioMballLock
		ElseIf nHitsLeft > 1 Then
			ShowText nHitsLeft & " MORE HITS TO", "LIGHT EARTH LOCK", 2500, eVOPrioMballLock
		Else
			ShowText "EARTH MULTIBALL", "LOCK IS LIT", 2500, eVOPrioMballLock
			FlashOnce 3, eColorYellow
			LightShowStart eLightShowEarthShort, eVOPrioMballLock
			aanLocksLit(nPlayer, eMballEarth) = 1
			If aanMballsPlayed(nPlayer, eMballEarth) > 1 _
			And aanLocksMade(nPlayer, eMballEarth) < 2 _
			And nBallPhysLocked < 2 Then
				bTurnClockwise = True
				TimerTurntable_Timer
			End If
			aanLockProgress(nPlayer, eMballEarth) = 0
			AddMballColor eShotEarth, eColorOrange
		End If
	End If
End Sub

Sub LockBallEarth(bPhysicalLock)
	aanLocksMade(nPlayer, eMballEarth) = aanLocksMade(nPlayer, eMballEarth) + 1
	aanLocksLit(nPlayer, eMballEarth) = 0
	If aanLocksMade(nPlayer, eMballEarth) < 3 Then
		ShowText "EARTH MULTIBALL", _	
			"BALL " & aanLocksMade(nPlayer, eMballEarth) & " LOCKED", 2500, eVOPrioMballLock
		If 1 = aanLocksMade(nPlayer, eMballEarth) Then
			AddCallout "VO_earth_mball_lock1", eVOPrioMballLock, eCharacterEarth
			if (BIP<2) then DOF 238, DOFPulse									
		Else
			AddCallout "VO_earth_mball_lock2", eVOPrioMballLock, eCharacterEarth
			if (BIP<2) then DOF 238, DOFPulse									
		End If
		RemoveMballColor eShotEarth, eColorOrange
		FlashOnce 3, eColorYellow
		LightShowStart eLightShowEarthShort, eVOPrioMballLock
		If bPhysicalLock Then
			bTurnClockwise = False
			TimerTurntable_Timer
			If BIP + nBallPhysLocked < 6 Then
				AutoPlunge 1
			Else
				BIP = BIP - 1
			End If
		End If
	Else
		RemoveMballColor eShotEarth, eColorOrange
		EarthMBallStart bPhysicalLock
	End If
End Sub

Sub EarthMBallStart(bPhysicalLock)
	Dim nBalls, bPlayShow
	bPlayShow = MBallStartCallout(eMballEarth)
	CommonMBallStart
	If nFirstMBallStarted = eModeNone Then nFirstMBallStarted = eModeFireMBall
	If (BIP<2) Then DOF 223, DOFOn							   
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		ShowText "EARTH MULTIBALL",	"HIT ORANGE SHOTS", 2500, eVOPrioMballStart
		FlashSweep eColorYellow
		If bPlayShow Then
			bBallHeld = True
			oMagnetA.MagnetOn = True
			nTimeDelayMultiball = 3000
			LightShowStart eLightShowEarthLong, eVOPrioMballStart
		Else
			UpPostMove LockPost,LockPostPrimitive,"down"
			LockPostTimer.Enabled = True
			bReleasingLocks = True
		End If
		BIP = BIP + nBallPhysLocked
		If bPhysicalLock Then
			BIP = BIP - 1
			nBalls = 3 - nBallPhysLocked
		Else
			nBalls = 2 - nBallPhysLocked
		End If
		If nBalls > 0 Then AddBalls nBalls
	End If
	anModeScore(eModeEarthMBall) = 0
	avModesRunning(nPlayer).add eModeEarthMBall
	If Not bInMode Then PlayModeMusic
	EarthMBallUpdate eShotNone
	nTimeBallSave = nTimeBallSave + cBallSaveMball
End Sub

Sub EarthMBallUpdate(nShot)
	Dim i
	Dim bValidShot
	Dim nShotsLeft
	Dim nValue
	Dim nProgress

	bValidShot = False
	If aanMballJackpots(nPlayer, eMballEarth) Mod 2 = 1 Then
		If eShotEarth = nShot Then bValidShot = True
	Else
		If anEarthJackpotsR(nPlayer) Mod 2 = 0 Then
			If eShotRightLoop = nShot Then bValidShot = True
		Else
			If eShotRightRamp = nShot Then bValidShot = True
		End If
		If bValidShot Then
			anEarthJackpotsR(nPlayer) = anEarthJackpotsR(nPlayer) + 1
		Else
			Select Case (anEarthJackpotsL(nPlayer) Mod 4)
				Case 0
					If eShotLeftRamp = nShot Or eShotSaucer = nShot Then bValidShot = True
				Case 1
					If eShotUnderFlipper = nShot Or eShotLeftRamp = nShot Then bValidShot = True
				Case 2
					If eShotUnderFlipper = nShot Or eShotLeftLoop = nShot Then bValidShot = True
				Case Else
					If eShotLeftLoop = nShot Or eShotSaucer = nShot Then bValidShot = True
			End Select
			If bValidShot Then anEarthJackpotsL(nPlayer) = anEarthJackpotsL(nPlayer) + 1
		End If
	End If

	If bValidShot Then
		nValue = 5000
		nValue = nValue * nPlayfieldX * anComboX(nShot)
		If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			TopSecretUpdate "EARTH JACKPOT", "", nValue
		Else
			ShowText "EARTH JACKPOT", WilliamsFormatNum(nValue), 2000, eVOPrioJackpot
			AddCallout "VO_earth_mball_jackpot", eVOPrioJackpot, eCharacterEarth
			FlashSweep eColorYellow
			LightShowStart eLightShowEarthShort, eVOPrioMballLock
		End If
		anScore(nPlayer) = anScore(nPlayer) + nValue
		anModeScore(eModeEarthMBall) = anModeScore(eModeEarthMBall) + nValue
		aanMballJackpots(nPlayer, eMballEarth) = aanMballJackpots(nPlayer, eMballEarth) + 1
		If aanMballJackpots(nPlayer, eMballEarth) >= anJackpotsForSuper(eMballEarth) _
		And False = vSuperJPsLit.Contains(eMballEarth) Then
			AddCallout "VO_earth_mball_sj_lit", eVOPrioMballReady, eCharacterEarth
			vSuperJPsLit.add eMballEarth
		End If
	End If

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	for i = 0 to 7 : RemoveMballColor i, eColorOrange : Next
	If anTimerMballGrace(eMballEarth) < 1 Then
		If aanMballJackpots(nPlayer, eMballEarth) Mod 2 = 1 Then
			AddMballColor eShotEarth, eColorOrange
		Else
			If anEarthJackpotsR(nPlayer) Mod 2 = 0 Then
				AddMballColor eShotRightLoop, eColorOrange
			Else
				AddMballColor eShotRightRamp, eColorOrange
			End If
			Select Case (anEarthJackpotsL(nPlayer) Mod 4)
				Case 0
					AddMballColor eShotSaucer, eColorOrange
					AddMballColor eShotLeftRamp, eColorOrange
				Case 1
					AddMballColor eShotUnderFlipper, eColorOrange
					AddMballColor eShotLeftRamp, eColorOrange
				Case 2
					AddMballColor eShotUnderFlipper, eColorOrange
					AddMballColor eShotLeftLoop, eColorOrange
				Case Else
					AddMballColor eShotSaucer, eColorOrange
					AddMballColor eShotLeftLoop, eColorOrange
			End Select
		End If
		If vSuperJPsLit.Contains(eMballEarth) then AddMballColor eShotSideRamp, eColorOrange
	End If
End Sub

Sub EarthMBallEnd
	Dim i
	vSuperJPsLit.Remove eMballEarth
	avModesRunning(nPlayer).Remove eModeEarthMBall
	DOF 223, DOFOff				
	for i = 0 to 7 : RemoveMballColor i, eColorOrange : Next
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		aanMballsPlayed(nPlayer, eMballEarth) = aanMballsPlayed(nPlayer, eMballEarth) + 1
		aanLocksMade(nPlayer, eMballEarth) = 0
		UpdateWizardModesLit
	End If
	If not IsMultiballActive then
		PlayModeMusic
		ShowMBallScore
	End If
End Sub

Sub AirMode1Start
    If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		AddCallout "VO_air_mode2_start", eVOPrioModeStart, eCharacterAir
		LightShowStart eLightShowAirShort, eVOPrioModeStart
	End If
	anAir1Shot(nPlayer) = eShotUnderFlipper
	nTimeAir1Move = 4000
	bAir1MoveLeft = False
	CommonModeStart eModeAir1
End Sub

Sub AirMode1Update(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, nProgress, nShotCount, nLastShot, nMoveTime

	bValidShot = False
	nProgress = aanModeShots(nPlayer, eModeAir1)
	If avModesPlayed(nPlayer).Contains(eModeAir2) _
	Or avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		Select Case nProgress
			Case 0, 1, 2, 3
				nMoveTime = 4000
			Case 4, 5, 6, 7
				nMoveTime = 3000
			Case Else
				nMoveTime = 2000
		End Select
	Else
		Select Case nProgress
			Case 0, 1, 2
				nMoveTime = 4000
			Case 3, 4
				nMoveTime = 3000
			Case Else
				nMoveTime = 2000
		End Select
	End If
	nLastShot = 1 + anAir1Shot(nPlayer) + aavShotsLit(nPlayer, eModeAir1).Count
	If nLastShot > eShotRightLoop Then nLastShot = eShotRightLoop
	If eShotNone <> nShot Then
		If aavShotsLit(nPlayer, eModeAir1).Contains(nShot) Then
			bValidShot = True
		Elseif bAir1MoveLeft Then
			If nMoveTime - nTimeAir1Move > 1500 And _
			aavShotsLit(nPlayer, eModeAir1).Contains(nLastShot) Then
				bValidShot = True
			End If
		Elseif (Not bAir1MoveLeft) and (nShot > eShotUnderFlipper) Then
			If nMoveTime - nTimeAir1Move > 1500 And _
			aavShotsLit(nPlayer, eModeAir1).Contains(nShot - 1) Then
				bValidShot = True
			End If
		End If
	End If
			
	If bValidShot Then
		nValue = 12000
		nValue = nValue * nPlayfieldX
		If eShotSideRamp = nShot Then
			nValue = nValue * 2
		ElseIf eShotNone <> nShot Then
			If anComboX(nShot) > 1 Then ComboCallout
			nValue = nValue * anComboX(nShot)
		End If
		anModeScore(eModeAir1) = anModeScore(eModeAir1) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		aanModeShots(nPlayer, eModeAir1) = aanModeShots(nPlayer, eModeAir1) + 1
		nProgress = aanModeShots(nPlayer, eModeAir1)
		If (not avModesPlayed(nPlayer).Contains(eModeAir2)) _
		And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
			If nProgress > 6 Then nProgress = 6
			ShowText "DOG FIGHTING", WilliamsFormatNum(nValue) & " " & nProgress & "/6", 2500, eVOPrioModeProgress
			If nProgress > 5 Then
				AirMode1End False
				exit Sub
			Else
				FlashSweep eColorPurple
				LightShowStart eLightShowAirShort, eVOPrioModeProgress
				ModeProgressSound eModeAir1, nShot
			End If
		Else
			If nProgress > 10 Then nProgress = 10
			If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				TopSecretUpdate "DOG FIGHTING", "", nValue
			Else
				ShowText "DOG FIGHTING", WilliamsFormatNum(nValue) & " " & nProgress & "/10", 2500, eVOPrioModeProgress
			End If
			If nProgress > 9 Then
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					aanModeShots(nPlayer, eModeAir1) = 0
					nProgress = 0
				Else
					AirMode1End False
					exit Sub
				End If
			ElseIf Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				FlashSweep eColorPurple
				LightShowStart eLightShowAirShort, eVOPrioModeProgress
				ModeProgressSound eModeAir1, nShot
			End If
		End If
	End If

	aavShotsLit(nPlayer, eModeAir1).Clear
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		For i = 0 to 7 : RemoveModeColor i, eColorPurple : Next
	End If
	If avModesPlayed(nPlayer).Contains(eModeAir2) _
	Or avModesRunning(nPlayer).Contains(eModeMegaWizard) then
		Select Case nProgress
			Case 0, 1, 2, 3
				nShotCount = 4
			Case 4, 5, 6, 7
				nShotCount = 3
			Case Else
				nShotCount = 2
		End Select
	Else
		Select Case nProgress
			Case 0, 1, 2
				nShotCount = 4
			Case 3, 4
				nShotCount = 3
			Case Else
				nShotCount = 2
		End Select
	End If
	For i = 0 to (nShotCount - 1)
		If anAir1Shot(nPlayer) + i <= eShotRightLoop Then
			If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				AddModeColor anAir1Shot(nPlayer) + i, eColorPurple
			End If
			aavShotsLit(nPlayer, eModeAir1).add anAir1Shot(nPlayer) + i
		End If
	Next
End Sub

Sub AirMode1End(bDrained)
	Dim i
	aavShotsLit(nPlayer, eModeAir1).Clear
	For i = 0 to 7 : RemoveModeColor i, eColorPurple : Next
	If Not bDrained Then
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			AddDelayedCallout "VO_hero_mode_air1_victory", eVOPrioModeEnd, eCharacterHero, 2500
			ShowText "DOG FIGHTING", "TOTAL " & WilliamsFormatNum(anModeScore(eModeAir1)), 5089, eVOPrioModeEnd
			PlayJingle "SFX_mode_total_jingle", 5089
			If KickerLeft.BallCntOver < 1 Then LightShowStart eLightShowModeComplete, eVOPrioModeEnd
			avModesPlayed(nPlayer).add eModeAir1
		End If
		avModesRunning(nPlayer).Remove eModeAir1
		CommonModeEnd
	End If
end Sub

Sub AirMode2Start
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		QueueDialogue "VO_air_mode1_start", eVOPrioModeStart, eCharacterAir
		QueueDialogue "VO_hero_mode_air2_start", eVOPrioModeStart, eCharacterHero
		ShowText "HIT RAMPS - SIDE", "RAMP COUNTS AS 2", 3000, eVOPrioModeStart
		LightShowStart eLightShowAirShort, eVOPrioModeStart
	End If
	CommonModeStart eModeAir2
End Sub

Sub AirMode2Update(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, nProgress

	bValidShot = False
	If eShotSideRamp = nShot Or eShotLeftRamp = nShot Or eShotRightRamp = nShot Then
		bValidShot = True
	End If
			
	If bValidShot Then
		nValue = 12000
		nValue = nValue * nPlayfieldX
		If eShotSideRamp = nShot Then
			nValue = nValue * 2
			aanModeShots(nPlayer, eModeAir2) = aanModeShots(nPlayer, eModeAir2) + 1
		ElseIf eShotNone <> nShot Then
			nValue = nValue * anComboX(nShot)
			If anComboX(nShot) > 1 Then ComboCallout
		End If
		anModeScore(eModeAir2) = anModeScore(eModeAir2) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		aanModeShots(nPlayer, eModeAir2) = aanModeShots(nPlayer, eModeAir2) + 1
		nProgress = aanModeShots(nPlayer, eModeAir2)
		If (not avModesPlayed(nPlayer).Contains(eModeAir1)) _
		And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
			If nProgress > 6 Then nProgress = 6
			ShowText "NO PARACHUTE", WilliamsFormatNum(nValue) & " " & nProgress & "/6", 2500, eVOPrioModeProgress
			If nProgress > 5 Then
				AirMode2End False
				exit Sub
			Else
				FlashSweep eColorPurple
				LightShowStart eLightShowAirShort, eVOPrioModeProgress
				ModeProgressSound eModeAir2, nShot
			End If
		Else
			If nProgress > 10 Then nProgress = 10
			If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				TopSecretUpdate "NO PARACHUTE", "", nValue
			Else
				ShowText "NO PARACHUTE", WilliamsFormatNum(nValue) & " " & nProgress & "/10", 2500, eVOPrioModeProgress
			End If
			If nProgress > 9 Then
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					aanModeShots(nPlayer, eModeAir2) = 0
				Else
					AirMode2End False
					exit Sub
				End If
			ElseIf Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				FlashSweep eColorPurple
				LightShowStart eLightShowAirShort, eVOPrioModeProgress
				ModeProgressSound eModeAir2, nShot
			End If
		End If
	End If

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	aavShotsLit(nPlayer, eModeAir2).Clear
	aavShotsLit(nPlayer, eModeAir2).add eShotSideRamp
	AddModeColor eShotSideRamp, eColorPurple
	aavShotsLit(nPlayer, eModeAir2).add eShotLeftRamp
	AddModeColor eShotLeftRamp, eColorPurple
	aavShotsLit(nPlayer, eModeAir2).add eShotRightRamp
	AddModeColor eShotRightRamp, eColorPurple
End Sub

Sub AirMode2End(bDrained)
	Dim i
	aavShotsLit(nPlayer, eModeAir2).Clear
	For i = 0 to 7 : RemoveModeColor i, eColorPurple : Next
	If Not bDrained Then
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			ShowText "NO PARACHUTE", "TOTAL " & WilliamsFormatNum(anModeScore(eModeAir2)), 5089, eVOPrioModeEnd
			PlayJingle "SFX_mode_total_jingle", 5089
			AddDelayedCallout "VO_hero_mode_air2_victory", eVOPrioModeEnd, eCharacterHero, 2500
			If KickerLeft.BallCntOver < 1 Then LightShowStart eLightShowModeComplete, eVOPrioModeEnd
			avModesPlayed(nPlayer).add eModeAir2
		End If
		avModesRunning(nPlayer).Remove eModeAir2
		CommonModeEnd
	End If
end Sub

Sub AirMode3Start
	PlayModeMusic
	anAir3Shot(nPlayer) = eShotSideRamp
	nTimeAir3Move = 2500
	anWizardModeRunning(nPlayer) = eModeAirWizard
	avWizardModesLit(nPlayer).Remove eModeAirWizard
	AddCallout "VO_air_showdown_start", eVOPrioModeStart, eCharacterAir
	LightShowStart eLightShowAirLong, eVOPrioModeStart
	CommonModeStart eModeAirWizard
	CommonWizardStart
End Sub

Sub AirMode3Update(nShot, nSwitch)
	Dim i, nValue, nDamage, nProgress

	nDamage = 0
	If nShot = (anAir3Shot(nPlayer) + 6) Mod 8 _
	Or nShot = (anAir3Shot(nPlayer) + 2) Mod 8 Then
		nDamage = 4
	Elseif nShot = (anAir3Shot(nPlayer) + 7) Mod 8 _
	Or nShot = (anAir3Shot(nPlayer) + 1) Mod 8 Then
		nDamage = 6
	Elseif nShot = anAir3Shot(nPlayer) Then
		nDamage = 8
	End If

	If nDamage > 0 Then
		If anComboX(nShot) > 1 Then nDamage = nDamage * 1.5
		nValue = 3500 * nDamage
		If eShotSideRamp = nShot Then
			nValue = nValue * 2
		Else
			nValue = nValue * anComboX(nShot)
		End If
		anModeScore(eModeAirWizard) = anModeScore(eModeAirWizard) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		aanModeShots(nPlayer, eModeAirWizard) = aanModeShots(nPlayer, eModeAirWizard) + nDamage
		If aanModeShots(nPlayer, eModeAirWizard) > 99 Then
			aanModeShots(nPlayer, eModeAirWizard) = 100
			AirMode3End False
		End If
		nProgress = aanModeShots(nPlayer, eModeAirWizard)
		If anComboX(nShot) > 1 Then
			ShowText "COMBO HIT " & WilliamsFormatNum(nValue), _
				"VILLAIN HP " & (100 - nProgress), 2500, eVOPrioModeProgress
		Else
			ShowText nDamage & " DAMAGE " & WilliamsFormatNum(nValue), _
				"VILLAIN HP " & (100 - nProgress), 2500, eVOPrioModeProgress
		End If
		If nProgress > 24 and (nProgress - nDamage) < 25 Then
			AddCallout "VO_air_showdown_damage1", eVOPrioModeProgress, eCharacterAir
			LightShowStart eLightShowAirLong, eVOPrioModeProgress
		ElseIf nProgress > 49 and (nProgress - nDamage) < 50 Then
			AddCallout "VO_air_showdown_damage2", eVOPrioModeProgress, eCharacterAir
			LightShowStart eLightShowAirLong, eVOPrioModeProgress
		ElseIf nProgress > 74 and (nProgress - nDamage) < 75 Then
			QueueDialogue "VO_hero_showdown_air_damage3", eVOPrioModeProgress, eCharacterHero
			QueueDialogue "VO_air_showdown_damage3", eVOPrioModeProgress, eCharacterAir
			LightShowStart eLightShowAirLong, eVOPrioModeProgress
		Else
			LightShowStart eLightShowAirShort, eVOPrioModeProgress
		End If
		FlashSweep eColorPurple
	End If

	For i = 0 to 7
		ClearModeColor i
		ClearMBallColor i
	Next
	If aanModeShots(nPlayer, eModeAirWizard) < 100 Then
		UpdateLEDs 100 - aanModeShots(nPlayer, eModeAirWizard)
		AddMballColor ((anAir3Shot(nPlayer) + 6) Mod 8), eColorOrange
		AddModeColor ((anAir3Shot(nPlayer) + 6) Mod 8), eColorOrange
		AddMballColor ((anAir3Shot(nPlayer) + 2) Mod 8), eColorOrange
		AddModeColor ((anAir3Shot(nPlayer) + 2) Mod 8), eColorOrange
		AddMballColor ((anAir3Shot(nPlayer) + 1) Mod 8), eColorRed
		AddModeColor ((anAir3Shot(nPlayer) + 1) Mod 8), eColorRed
		AddMballColor ((anAir3Shot(nPlayer) + 7) Mod 8), eColorRed
		AddModeColor ((anAir3Shot(nPlayer) + 7) Mod 8), eColorRed
		AddMballColor anAir3Shot(nPlayer), eColorPurple
		AddModeColor anAir3Shot(nPlayer), eColorPurple
	End If
End Sub

Sub AirMode3End(bDrained)
	Dim i
	For i = 0 to 7
		ClearModeColor i
		ClearMBallColor i
	Next
	
	If Not bDrained Then
		nTimeAir3Move = 0
		AddCallout "VO_air_showdown_defeat", eVOPrioModeEnd, eCharacterAir
		ShowText "AIR SHOWDOWN", "TOTAL " & WilliamsFormatNum(anModeScore(eModeAirWizard)), 3000, eVOPrioModeEnd
		LightShowStart eLightShowAirLong, eVOPrioModeEnd
		avModesRunning(nPlayer).Remove eModeAirWizard
		avModesPlayed(nPlayer).add eModeAirWizard
		avPermaCombo(nPlayer).add eShotRightRamp
		nModeSelected = an1BallModes(Int(Rnd * 12))
		ModeSelectNext
		CommonModeEnd
		CommonWizardEnd
	End If
End Sub

Sub AirAdvanceLock (nIndex)
	Dim nMaxLettersLit
	Dim nLetters
	Dim i

	If nIndex <> eShotNone Then
		If avAirLettersLit(nPlayer).Contains(nIndex) Then
			If aanMballsPlayed(nPlayer, eMballAir) < 3 then
				avAirLettersLit(nPlayer).Clear
			Else
				avAirLettersLit(nPlayer).Remove nIndex
			End If
			avAirLettersHit(nPlayer).add nIndex
			If avAirLettersHit(nPlayer).IsFull Then
				aanLocksLit(nPlayer, eMballAir) = 1
				AddMballColor eShotRightRamp, eColorPurple
				ShowText "AIR M-BALL LIT", "HIT RIGHT RAMP", 2500, eVOPrioMballReady
				FlashSweep Array(eColorPurple, eColorPurple, eColorPurple)
				LightShowStart eLightShowAirShort, eVOPrioMballReady
				If nSFXPriority > eVOPrioMballReady Then
					sSFXToPlay = "SFX_plane_passing"
					nSFXPriority = eVOPrioMballReady
					fSFXQueuedVolume = 0.5
				End If
				AddDelayedCallout "VO_air_mball_ready", eVOPrioMballReady, eCharacterAir, 3500
			else
				FlashSweep eColorPurple
				If nSFXPriority > eVOPrioJackpot Then
					sSFXToPlay = Array("SFX_missile" & Int(rnd*3), "SFX_missile_lock_beep")
					nSFXPriority = eVOPrioJackpot
					fSFXQueuedVolume = 1.0
				End If
			end if
		End If
	End If
	If Not avAirLettersHit(nPlayer).IsFull Then
		Select Case aanMballsPlayed(nPlayer, eMballAir)
			Case 0 : nMaxLettersLit = 4
			Case 1 : nMaxLettersLit = 2
			Case 2 : nMaxLettersLit = 1
			Case Else : nMaxLettersLit = 8
		End Select
		If aanMballsPlayed(nPlayer, eMballAir) < 3 then
			nLetters = 0
			For i = 0 to 7
				If Not avAirLettersHit(nPlayer).Contains(i) Then
					nLetters = nLetters + 1
					avAirLettersLit(nPlayer).add i
				End If
				If nLetters >= nMaxLettersLit Then Exit For
			Next
		ElseIf nIndex <> eShotNone Then
			If Not avAirLettersHit(nPlayer).Contains(nIndex) Then
				avAirLettersLit(nPlayer).add nIndex
			End If
		End If
	End If
End Sub

Sub AirMballStart
	Dim i, bPlayShow
	bPlayShow = MBallStartCallout(eMballAir)
	CommonMBallStart
	If nFirstMBallStarted = eModeNone Then nFirstMBallStarted = eModeAirMBall
	If (BIP<2) Then DOF 221, DOFOn							   
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		ShowText "AIR MULTIBALL", "HIT PURPLE SHOTS", 2500, eVOPrioMballStart
		If bPlayShow Then
			bBallHeld = True
			UpPostMove RightRampPost,RightRampPostPrimitive,"up"
			nTimeRightRampPost = 3000
			nTimeDelayMultiball = 3000
			LightShowStart eLightShowAirLong, eVOPrioMballStart
		End If
		FlashSweep eColorPurple
		AddBalls 1
		aanLocksMade(nPlayer, eMballAir) = 0
		aanLocksLit(nPlayer, eMballAir) = 0
		aanLockProgress(nPlayer, eMballAir) = 0
	End If
	vAirMultipliers.Clear
	anModeScore(eModeAirMBall) = 0
	avModesRunning(nPlayer).add eModeAirMBall
	If Not bInMode Then PlayModeMusic
	AirMBallUpdate eShotNone
	nTimeBallSave = nTimeBallSave + cBallSaveMball
End Sub

Sub AirMballUpdate(nShot)
	Dim i
	Dim bValidShot
	Dim nShotsLeft
	Dim nValue
	Dim nProgress

	bValidShot = False
	If nShot <> eShotNone Then
		If (eShotUnderFlipper = nShot And False = vAirMultipliers.Contains(eShotUnderFlipper)) Or _
		(eShotSaucer = nShot And False = vAirMultipliers.Contains(eShotSaucer)) Or _
		(eShotEarth = nShot And False = vAirMultipliers.Contains(eShotEarth)) Or _
		(eShotRightRamp = nShot And vAirMultipliers.Count > 0) Then
			bValidShot = True
		End if
	End If

	If bValidShot Then
		nProgress = vAirMultipliers.Count
		If eShotRightRamp = nShot Then
			aanMballJackpots(nPlayer, eMballAir) = aanMballJackpots(nPlayer, eMballAir) + nProgress
			nValue = 5000 * nProgress
			nValue = nValue * nPlayfieldX * anComboX(eShotRightRamp)
			Select Case nProgress
				Case 2
					If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
						TopSecretUpdate "DOUBLE AIR", "JACKPOT", nValue
					Else
						AddCallout "VO_air_mball_jackpot2x", eVOPrioJackpot, eCharacterAir
						ShowText "DOUBLE AIR", "JACKPOT " & WilliamsFormatNum(nValue), 2000, eVOPrioJackpot
					End If
				Case 3
					If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
						TopSecretUpdate "TRIPLE AIR", "JACKPOT", nValue
					Else
						AddCallout "VO_air_mball_jackpot3x", eVOPrioJackpot, eCharacterAir
						ShowText "TRIPLE AIR", "JACKPOT " & WilliamsFormatNum(nValue), 2000, eVOPrioJackpot
					End If
				Case Else
					If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
						TopSecretUpdate "AIR JACKPOT", "", nValue
					Else
						AddCallout "VO_air_mball_jackpot1x", eVOPrioJackpot, eCharacterAir
						ShowText "AIR JACKPOT", WilliamsFormatNum(nValue), 2000, eVOPrioJackpot
					End If
			End Select
			anScore(nPlayer) = anScore(nPlayer) + nValue
			anModeScore(eModeAirMBall) = anModeScore(eModeAirMBall) + nValue
			If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				FlashSweep eColorPurple
				LightShowStart eLightShowAirShort, eVOPrioJackpot
			End If
			vAirMultipliers.Clear
		Else
			vAirMultipliers.Add nShot
			Select Case vAirMultipliers.Count
				Case 2
					If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
						ShowText "DOUBLE AIR", "JACKPOT IS LIT", 2000, eVOPrioJackpot
					End If
				Case 3
					If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
						ShowText "TRIPLE AIR", "JACKPOT IS LIT", 2000, eVOPrioJackpot
					End If
				Case Else
					If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
						ShowText "AIR JACKPOT", "IS LIT", 2000, eVOPrioJackpot
					End If
			end Select
		End If
		If aanMballJackpots(nPlayer, eMballAir) >= anJackpotsForSuper(eMballAir) _
		And False = vSuperJPsLit.Contains(eMballAir) Then
			AddCallout "VO_air_mball_sj_lit", eVOPrioMballReady, eCharacterAir
			vSuperJPsLit.add eMballAir
		End If
	End If

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	for i = 0 to 7 : RemoveMballColor i, eColorPurple : Next
	If anTimerMballGrace(eMballAir) < 1 Then
		For each i in Array(eShotUnderFlipper, eShotSaucer, eShotEarth)
			If Not vAirMultipliers.Contains(i) Then
				AddMballColor i, eColorPurple
			End If
		Next
		If vAirMultipliers.Count > 0 Then AddMballColor eShotRightRamp, eColorPurple
		If vSuperJPsLit.Contains(eMballAir) then AddMballColor eShotSideRamp, eColorPurple
	End If
End Sub

Sub AirMballEnd
	Dim i
	for i = 0 to 7 : RemoveMballColor i, eColorPurple : Next
	vSuperJPsLit.Remove eMballAir
	avModesRunning(nPlayer).Remove eModeAirMBall
	DOF 221, DOFOff				
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		aanLocksMade(nPlayer, eMballAir) = 0
		aanMballsPlayed(nPlayer, eMballAir) = aanMballsPlayed(nPlayer, eMballAir) + 1
		avAirLettersHit(nPlayer).Clear
		avAirLettersLit(nPlayer).Clear
	End If
	AirAdvanceLock eShotNone
	UpdateWizardModesLit
	If not IsMultiballActive then
		PlayModeMusic
		ShowMBallScore
	End If
End Sub

Sub IceAdvanceLock
	Dim nLockNumber
	If avModesRunning(nPlayer).Contains(eModeIceMBall) Then
		Exit Sub
	End If
	nLockNumber = aanLocksMade(nPlayer, eMballIce) + aanLocksLit(nPlayer, eMballIce)
	FlashOnce 5, eColorWhite
	If 0 = aanMballsPlayed(nplayer, eMballIce) and nLockNumber < 3 Then
		aanLocksLit(nPlayer, eMballIce) = aanLocksLit(nPlayer, eMballIce) + 1
		ShowText "ICE MULTIBALL", "LOCK " & (1 + nLockNumber) & " IS LIT", 2500, eVOPrioMballLock
		AddMballColor eShotRightLoop, eColorWhite
	Elseif aanMballsPlayed(nplayer, eMballIce) > 0 and aanLocksLit(nPlayer, eMballIce) < 1 Then
		aanLocksLit(nPlayer, eMballIce) = 1
		ShowText "ICE MULTIBALL", "LOCK " & (1 + nLockNumber) & " IS LIT", 2500, eVOPrioMballLock
		AddMballColor eShotRightLoop, eColorWhite
	End If
End Sub

Sub IceLockBall
	aanLocksMade(nPlayer, eMballIce) = aanLocksMade(nPlayer, eMballIce) + 1
	aanLocksLit(nPlayer, eMballIce) = aanLocksLit(nPlayer, eMballIce) - 1
	If aanLocksLit(nPlayer, eMballIce) < 1 Then RemoveMballColor eShotRightLoop, eColorWhite
	If aanLocksLit(nPlayer, eMballIce) < 0 then aanLocksLit(nPlayer, eMballIce) = 0
	If aanLocksMade(nPlayer, eMballIce) < 3 Then
		FlashOnce 3, eColorWhite
		LightShowStart eLightShowIceShort, eVOPrioMballLock
		If 1 = aanLocksMade(nPlayer, eMballIce) Then
			AddCallout "VO_ice_mball_lock1", eVOPrioMballLock, eCharacterIce
			DOF 241, DOFPulse					
		Else
			AddCallout "VO_ice_mball_lock2", eVOPrioMballLock, eCharacterIce
			DOF 241, DOFPulse					
		End If
		ShowText "ICE MULTIBALL", _	
			"BALL " & aanLocksMade(nPlayer, eMballIce) & " LOCKED", 2500, eVOPrioMballLock
		If aanMballsPlayed(nPlayer, eMballIce) > 0 Then
			avDropsHit(nPlayer).Clear
			RaiseDrops
		End If
	Else
		IceMBallStart
	End If
End Sub

Sub IceMode1Start
    If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		AddCallout "VO_ice_mode1_start", eVOPrioModeStart, eCharacterIce
		LightShowStart eLightShowIceShort, eVOPrioModeStart
	End If
	anIce1LastShot(nPlayer) = eShotLeftRamp
	CommonModeStart eModeIce1
End Sub

Sub IceMode1Update(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, nProgress

	bValidShot = False
	nProgress = aanModeShots(nPlayer, eModeIce1)
	If nShot <> eShotNone Then
		If aavShotsLit(nPlayer, eModeIce1).Contains(nShot) Then
			bValidShot = True
		End If
	End If

	If bValidShot Then
		nValue = 12000
		nValue = nValue * nPlayfieldX
		If eShotSideRamp = nShot Then
			nValue = nValue * 2
		ElseIf eShotNone <> nShot Then
			nValue = nValue * anComboX(nShot)
			If anComboX(nShot) > 1 Then ComboCallout
		End If
		anModeScore(eModeIce1) = anModeScore(eModeIce1) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		aanModeShots(nPlayer, eModeIce1) = aanModeShots(nPlayer, eModeIce1) + 1
		nProgress = aanModeShots(nPlayer, eModeIce1)
		anIce1LastShot(nPlayer) = nShot
		If (not avModesPlayed(nPlayer).Contains(eModeIce2)) _
		And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
			ShowText "GONDOLA LIFT", WilliamsFormatNum(nValue) & " " & nProgress & "/6", 2500, eVOPrioModeProgress
			If nProgress > 5 Then
				IceMode1End False
				exit Sub
			Else
				FlashSweep eColorWhite
				LightShowStart eLightShowIceShort, eVOPrioModeProgress
				ModeProgressSound eModeIce1, nShot
			End If
		Else
			If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				TopSecretUpdate "GONDOLA LIFT", "", nValue
			Else
				ShowText "GONDOLA LIFT", WilliamsFormatNum(nValue) & " " & nProgress & "/10", 2500, eVOPrioModeProgress
			End If
			If nProgress > 9 Then
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					aanModeShots(nPlayer, eModeIce1) = 0
				Else
					IceMode1End False
					exit Sub
				End If
			ElseIf Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				FlashSweep eColorWhite
				LightShowStart eLightShowIceShort, eVOPrioModeProgress
				ModeProgressSound eModeIce1, nShot
			End If
		End If
	End If

	aavShotsLit(nPlayer, eModeIce1).Clear
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		For i = 0 to 7 : RemoveModeColor i, eColorWhite : Next
	End If
	If nProgress Mod 2 = 0 Then
		For i = 0 to 7
			If i > anIce1LastShot(nPlayer) then
				aavShotsLit(nPlayer, eModeIce1).add i
				If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					AddModeColor i, eColorWhite
				End If
			End If
		Next
	Else
		For i = 0 to 7
			If i < anIce1LastShot(nPlayer) then
				aavShotsLit(nPlayer, eModeIce1).add i
				If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					AddModeColor i, eColorWhite
				End If
			End If
		Next
	End If
End Sub

Sub IceMode1End(bDrained)
	Dim i
	aavShotsLit(nPlayer, eModeIce1).Clear
	For i = 0 to 7 : RemoveModeColor i, eColorWhite : Next
	If Not bDrained Then
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			ShowText "GONDOLA LIFT", "TOTAL " & WilliamsFormatNum(anModeScore(eModeIce1)), 5089, eVOPrioModeEnd
			AddDelayedCallout "VO_hero_mode_ice1_victory", eVOPrioModeEnd, eCharacterHero, 2500
			PlayJingle "SFX_mode_total_jingle", 5089
			If KickerLeft.BallCntOver < 1 Then LightShowStart eLightShowModeComplete, eVOPrioModeEnd
			avModesPlayed(nPlayer).add eModeIce1
		End If
		avModesRunning(nPlayer).Remove eModeIce1
		CommonModeEnd
	End If
end Sub

Sub IceMode2Start
    If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		QueueDialogue "VO_ice_mode2_start", eVOPrioModeStart, eCharacterIce
		QueueDialogue "VO_hero_mode_ice2_start", eVOPrioModeStart, eCharacterHero
		LightShowStart eLightShowIceShort, eVOPrioModeStart
	End If
	CommonModeStart eModeIce2
End Sub

Sub IceMode2Update(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, nProgress, anShotLitOrder
	anShotLitOrder = Array(eShotEarth, eShotSideRamp, eShotLeftRamp, eShotSaucer, _
		eShotLeftLoop, eShotRightRamp, eShotRightLoop)

	bValidShot = False
	nProgress = aanModeShots(nPlayer, eModeIce2)
	If (not avModesPlayed(nPlayer).Contains(eModeIce1)) _
	And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
		If nShot <> eShotNone Then
			If aavShotsLit(nPlayer, eModeIce2).Contains(nShot) Then
				bValidShot = True
			End If
		End If
	Else
		Select Case nProgress
			Case 0
				If eShotEarth = nShot Then bValidShot = True
				If eShotSideRamp = nShot Then bValidShot = True
			Case 1
				If eShotSideRamp = nShot Then bValidShot = True
				If eShotLeftRamp = nShot Then bValidShot = True
			Case 2
				If eShotSaucer = nShot Then bValidShot = True
				If eShotLeftRamp = nShot Then bValidShot = True
			Case 3
				If eShotSaucer = nShot Then bValidShot = True
				If eSwitchTargetX3 = nSwitch Then bValidShot = True
				If eSwitchTargetX4 = nSwitch Then bValidShot = True
			Case 4
				If eShotLeftLoop = nShot Then bValidShot = True
				If eSwitchTargetX3 = nSwitch Then bValidShot = True
				If eSwitchTargetX4 = nSwitch Then bValidShot = True
			Case 5
				If eShotLeftLoop = nShot Then bValidShot = True
				If eShotRightRamp = nShot Then bValidShot = True
			Case 6
				If eShotRightLoop = nShot Then bValidShot = True
				If eShotRightRamp = nShot Then bValidShot = True
			Case 7
				If eShotRightLoop = nShot Then bValidShot = True
				If eShotUnderFlipper = nShot Then bValidShot = True
			Case 8
				If eShotUnderFlipper = nShot Then bValidShot = True
				If eSwitchTargetX1 = nSwitch Then bValidShot = True
				If eSwitchTargetX2 = nSwitch Then bValidShot = True
			Case Else
				If eSwitchTargetX1 = nSwitch Then bValidShot = True
				If eSwitchTargetX2 = nSwitch Then bValidShot = True
				If eSwitchDropI = nSwitch Then bValidShot = True
				If eSwitchDropC = nSwitch Then bValidShot = True
				If eSwitchDropE = nSwitch Then bValidShot = True
		End Select
	End If

	If bValidShot Then
		nValue = 12000
		nValue = nValue * nPlayfieldX
		If eShotSideRamp = nShot Then
			nValue = nValue * 2
		ElseIf eShotNone <> nShot Then
			nValue = nValue * anComboX(nShot)
			If anComboX(nShot) > 1 Then ComboCallout
		End If
		anModeScore(eModeIce2) = anModeScore(eModeIce2) + nValue
		anScore(nPlayer) = anScore(nPlayer) + nValue
		aanModeShots(nPlayer, eModeIce2) = aanModeShots(nPlayer, eModeIce2) + 1
		nProgress = aanModeShots(nPlayer, eModeIce2)
		If (not avModesPlayed(nPlayer).Contains(eModeIce1)) _
		And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
			ShowText "AVALANCHE", WilliamsFormatNum(nValue) & " " & nProgress & "/6", 2500, eVOPrioModeProgress
			If nProgress > 5 Then
				IceMode2End False
				exit Sub
			Else
				FlashSweep eColorWhite
				LightShowStart eLightShowIceShort, eVOPrioModeProgress
				ModeProgressSound eModeIce2, nShot
			End If
		Else
			If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				TopSecretUpdate "AVALANCHE", "", nValue
			Else
				ShowText "AVALANCHE", WilliamsFormatNum(nValue) & " " & nProgress & "/10", 2500, eVOPrioModeProgress
			End If
			If nProgress > 9 Then
				If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					 aanModeShots(nPlayer, eModeIce2) = 0
				Else
					IceMode2End False
					exit Sub
				End if
			ElseIf Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				FlashSweep eColorWhite
				LightShowStart eLightShowIceShort, eVOPrioModeProgress
				ModeProgressSound eModeIce2, nShot
			End If
		End If
	End If

	aavShotsLit(nPlayer, eModeIce2).Clear
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		For i = 0 to 7 : RemoveModeColor i, eColorWhite : Next
	End If
	If (not avModesPlayed(nPlayer).Contains(eModeIce1)) _
	And (not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
		For i = 0 to 6
			If nProgress = i Or nProgress + 1 = i Then
				aavShotsLit(nPlayer, eModeIce2).add anShotLitOrder(i)
				If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					AddModeColor anShotLitOrder(i), eColorWhite
				End If
			End If
		Next
	Else
		Select Case nProgress
			Case 0
				aavShotsLit(nPlayer, eModeIce2).add eShotEarth
				aavShotsLit(nPlayer, eModeIce2).add eShotSideRamp
				If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					AddModeColor eShotEarth, eColorWhite
					AddModeColor eShotSideRamp, eColorWhite
				End If
			Case 1
				aavShotsLit(nPlayer, eModeIce2).add eShotLeftRamp
				aavShotsLit(nPlayer, eModeIce2).add eShotSideRamp
				If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					AddModeColor eShotLeftRamp, eColorWhite
					AddModeColor eShotSideRamp, eColorWhite
				End If
			Case 2
				aavShotsLit(nPlayer, eModeIce2).add eShotLeftRamp
				aavShotsLit(nPlayer, eModeIce2).add eShotSaucer
				If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					AddModeColor eShotLeftRamp, eColorWhite
					AddModeColor eShotSaucer, eColorWhite
				End If
			Case 3
				aavShotsLit(nPlayer, eModeIce2).add eShotSaucer
				If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					AddModeColor eShotSaucer, eColorWhite
					If bValidShot Or (eShotNone = nShot) Then
						aoXLights(2).BlinkInterval = 85
						aoXLights(3).BlinkInterval = 85
						aoXLights(3).BlinkPattern = "01"
						aoXLights(2).state = LightStateBlinking
						aoXLights(3).state = LightStateBlinking
					End If
				End If
			Case 4
				If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					If bValidShot Or (eShotNone = nShot) Then
						aoXLights(2).BlinkInterval = 85
						aoXLights(3).BlinkInterval = 85
						aoXLights(3).BlinkPattern = "01"
						aoXLights(2).state = LightStateBlinking
						aoXLights(3).state = LightStateBlinking
					End If
					AddModeColor eShotLeftLoop, eColorWhite
				End If
				aavShotsLit(nPlayer, eModeIce2).add eShotLeftLoop
			Case 5
				If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					AddModeColor eShotLeftLoop, eColorWhite
					AddModeColor eShotRightRamp, eColorWhite
					If bValidShot Or (eShotNone = nShot) Then
						aoXLights(2).BlinkInterval = 170
						aoXLights(3).BlinkInterval = 170
						aoXLights(3).BlinkPattern = "10"
					End If
				End If
				aavShotsLit(nPlayer, eModeIce2).add eShotLeftLoop
				aavShotsLit(nPlayer, eModeIce2).add eShotRightRamp
			Case 6
				aavShotsLit(nPlayer, eModeIce2).add eShotRightLoop
				aavShotsLit(nPlayer, eModeIce2).add eShotRightRamp
				If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					AddModeColor eShotRightLoop, eColorWhite
					AddModeColor eShotRightRamp, eColorWhite
				End If
			Case 7
				aavShotsLit(nPlayer, eModeIce2).add eShotRightLoop
				aavShotsLit(nPlayer, eModeIce2).add eShotUnderFlipper
				If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					AddModeColor eShotRightLoop, eColorWhite
					AddModeColor eShotUnderFlipper, eColorWhite
				End If
			Case 8
				aavShotsLit(nPlayer, eModeIce2).add eShotUnderFlipper
				If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					AddModeColor eShotUnderFlipper, eColorWhite
					If bValidShot Or (eShotNone = nShot) Then
						aoXLights(0).BlinkInterval = 85
						aoXLights(1).BlinkInterval = 85
						aoXLights(1).BlinkPattern = "01"
						aoXLights(0).state = LightStateBlinking
						aoXLights(1).state = LightStateBlinking
					End If
				End If
			Case Else
				If (bValidShot Or (eShotNone = nShot)) _
				And (Not avModesRunning(nPlayer).Contains(eModeMegaWizard)) Then
					aoXLights(0).BlinkInterval = 85
					aoXLights(1).BlinkInterval = 85
					aoXLights(1).BlinkPattern = "01"
					aoXLights(0).state = LightStateBlinking
					aoXLights(1).state = LightStateBlinking
					aoDropLights(1).BlinkPattern = "01"
					For Each i in aoDropLights
						i.BlinkInterval = 85
						i.state = LightStateBlinking
					Next
				End If
		End Select
	End If
End Sub

Sub IceMode2End(bDrained)
	Dim i
	For each i in aoXLights
		i.BlinkInterval = 170
		i.BlinkPattern = "10"
	Next
	For Each i in aoDropLights
		i.BlinkInterval = 170
		i.BlinkPattern = "10"
	Next
	aavShotsLit(nPlayer, eModeIce2).Clear
	For i = 0 to 7 : RemoveModeColor i, eColorWhite : Next
	If Not bDrained Then
		If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			ShowText "AVALANCHE TOTAL", WilliamsFormatNum(anModeScore(eModeIce2)), 5089, eVOPrioModeEnd
			AddDelayedCallout "VO_hero_mode_ice2_victory", eVOPrioModeEnd, eCharacterHero, 2500
			PlayJingle "SFX_mode_total_jingle", 5089
			If KickerLeft.BallCntOver < 1 Then LightShowStart eLightShowModeComplete, eVOPrioModeEnd
			avModesPlayed(nPlayer).add eModeIce2
		End If
		avModesRunning(nPlayer).Remove eModeIce2
		CommonModeEnd
	End If
End Sub

Sub IceMode3Start
	Dim i, nBalls
	' TODO: Mode intro and rule explanation
	PlayModeMusic
	aavShotsLit(nPlayer, eModeIceWizard).Clear
	aavShotsLit(nPlayer, eModeIceWizard).add eShotUnderFlipper
	aavShotsLit(nPlayer, eModeIceWizard).add eShotSaucer
	aavShotsLit(nPlayer, eModeIceWizard).add eShotLeftRamp
	aavShotsLit(nPlayer, eModeIceWizard).add eShotEarth
	aavShotsLit(nPlayer, eModeIceWizard).add eShotSideRamp
	aavShotsLit(nPlayer, eModeIceWizard).add eShotRightRamp
	anIce3SwitchesLeft(nPlayer) = 50
	BIP = BIP + nBallPhysLocked
	nBalls = 3 - nBallPhysLocked
	nBallPhysLocked = 0
	bReleasingLocks = True
	UpPostMove LockPost,LockPostPrimitive,"down"
	LockPostTimer.Enabled = True
	If nBalls > 0 Then AddBalls nBalls
	nTimeBallSave = cBallSaveStart
	LightShootAgain.state = LightStateBlinking
	QueueDialogue "VO_ice_showdown_start", eVOPrioModeStart, eCharacterIce
	QueueDialogue "VO_hero_showdown_ice_start", eVOPrioModeStart, eCharacterHero
	LightShowStart eLightShowIceLong, eVOPrioModeStart
	anWizardModeRunning(nPlayer) = eModeIceWizard
	avWizardModesLit(nPlayer).Remove eModeIceWizard
	CommonModeStart eModeIceWizard
	CommonWizardStart
End Sub

Sub IceMode3Update(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, nProgress

	bValidShot = False
	If anIce3SwitchesLeft(nPlayer) > 0 Then
		If eSwitchNone <> nSwitch Then bValidShot = True
	Elseif nShot <> eShotNone Then
		If aavShotsLit(nPlayer, eModeIceWizard).Contains(nShot) Then bValidShot = True
	End If
			
	If bValidShot Then
		If anIce3SwitchesLeft(nPlayer) > 0 Then
			nValue = 500
			nValue = nValue * nPlayfieldX
			anModeScore(eModeFire2) = anModeScore(eModeFire2) + nValue
			anScore(nPlayer) = anScore(nPlayer) + nValue
			anIce3SwitchesLeft(nPlayer) = anIce3SwitchesLeft(nPlayer) - 1
			If anIce3SwitchesLeft(nPlayer) < 1 Then
				FlashSweep eColorWhite
				LightShowStart eLightShowIceShort, eVOPrioModeProgress
			End If
		Else
			aavShotsLit(nPlayer, eModeIceWizard).Remove nShot
			aanModeShots(nPlayer, eModeIceWizard) = aanModeShots(nPlayer, eModeIceWizard) + 1
			nProgress = aanModeShots(nPlayer, eModeIceWizard)
			nValue = 50000
			If eShotSideRamp = nShot Then
				nValue = nValue * 2
			ElseIf eShotNone <> nShot Then
				nValue = nValue * anComboX(nShot)
			End If
			nValue = nValue * nPlayfieldX		
			anScore(nPlayer) = anScore(nPlayer) + nValue
			anModeScore(eModeIceWizard) = anModeScore(eModeIceWizard) + nValue
			ShowText "VILLAN HIT", WilliamsFormatNum(nValue) & "  " & (4 - nProgress) & "/4 HP", 2500, eVOPrioModeProgress
			If nProgress > 3 Then
				IceMode3End False
				exit Sub
			Else
				AddCallout "VO_ice_showdown_damage" & nProgress, eVOPrioModeProgress, eCharacterIce
				LightShowStart eLightShowIceLong, eVOPrioModeProgress
				anIce3SwitchesLeft(nPlayer) = 50 + (10 * nProgress)
			End If
		End If
	End If

	For i = 0 to 7
		ClearModeColor i
		ClearMBallColor i
	Next
	If anIce3SwitchesLeft(nPlayer) > 0 Then
		UpdateLEDs anIce3SwitchesLeft(nPlayer)
		AddModeColor eShotRightLoop, eColorWhite
		AddMballColor eShotRightLoop, eColorWhite
		AddModeColor eShotLeftLoop, eColorWhite
		AddMballColor eShotLeftLoop, eColorWhite
	Else
		UpdateLEDs -1
		For i = 0 to 7
			If aavShotsLit(nPlayer, eModeIceWizard).Contains(i) Then
				AddModeColor i, eColorWhite
				AddMballColor i, eColorWhite
			End If
		Next
	End If
End Sub

Sub IceMode3End(bDrained)
	Dim i
	For i = 0 to 7
		ClearModeColor i
		ClearMBallColor i
	Next
	
	If Not bDrained Then
		aavShotsLit(nPlayer, eModeIceWizard).Clear
		QueueDialogue "VO_ice_showdown_damage4", eVOPrioModeEnd, eCharacterIce
		QueueDialogue "VO_hero_showdown_ice_victory", eVOPrioModeEnd, eCharacterHero
		ShowText "ICE SHOWDOWN", "TOTAL " & WilliamsFormatNum(anModeScore(eModeIceWizard)), 5089, eVOPrioModeEnd
		PlayJingle "SFX_mode_total_jingle", 5089
		LightShowStart eLightShowIceLong, eVOPrioModeEnd
		avModesRunning(nPlayer).Remove eModeIceWizard
		avWizardModesLit(nPlayer).Remove eModeIceWizard
		avModesPlayed(nPlayer).add eModeIceWizard
		avPermaCombo(nPlayer).add eShotRightLoop
		nModeSelected = an1BallModes(Int(Rnd * 12))
		ModeSelectNext
		CommonModeEnd
		CommonWizardEnd
	End If
End Sub

Sub IceMballStart
	Dim i, bPlayShow
	bPlayShow = MBallStartCallout(eMballIce)
	CommonMBallStart
	If (BIP<2) Then DOF 225, DOFOn							   
	If nFirstMBallStarted = eModeNone Then nFirstMBallStarted = eModeIceMBall
	If Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		aanLocksLit(nPlayer, eMballIce) = 0
		aanLockProgress(nPlayer, eMballIce) = 0
		ShowText "ICE MULTIBALL", "HIT WHITE SHOTS", 2500, eVOPrioMballStart
		If bPlayShow Then
			bBallHeld = True
			nTimeSkillPost = 3000
			nTimeDelayMultiball = 3000
			LightShowStart eLightShowIceLong, eVOPrioMballStart
		End If
		FlashSweep eColorWhite
		AddBalls 2
		nTimeBallSave = nTimeBallSave + cBallSaveMball
	End If
	vIceJackpots.Clear
	vIceJackpots.add eShotLeftRamp
	nLastIceJPAdded = eShotLeftRamp
	anModeScore(eModeIceMBall) = 0
	avModesRunning(nPlayer).add eModeIceMBall
	If Not bInMode Then PlayModeMusic
	IceMBallUpdate eShotNone
End Sub

Sub IceMballUpdate(nShot)
	Dim i
	Dim bValidShot
	Dim nShotsLeft
	Dim nValue
	Dim nProgress
	Dim nNextShot

	bValidShot = False
	If nShot <> eShotNone Then
		If vIceJackpots.Contains(nShot) Then bValidShot = True
	End If

	If bValidShot Then
		nProgress = vIceJackpots.Count
		aanMballJackpots(nPlayer, eMballIce) = aanMballJackpots(nPlayer, eMballIce) + 1
		nValue = 6000
		nValue = nValue * nPlayfieldX * anComboX(nShot)
		If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
			TopSecretUpdate "ICE JACKPOT", "", nValue
		Else
			AddCallout "VO_ice_mball_jackpot", eVOPrioJackpot, eCharacterIce
			ShowText "ICE JACKPOT", WilliamsFormatNum(nValue), 2000, eVOPrioJackpot
			FlashSweep eColorWhite
			LightShowStart eLightShowIceShort, eVOPrioJackpot
		End If
		anScore(nPlayer) = anScore(nPlayer) + nValue
		anModeScore(eModeIceMBall) = anModeScore(eModeIceMBall) + nValue
		vIceJackpots.Remove nShot
		If nProgress < 2 Then
			nNextShot = (nLastIceJPAdded + 3) Mod 8
			If nNextShot > eShotEarth Then nNextShot = (nNextShot + 1) Mod 8
			vIceJackpots.add nNextShot
			nLastIceJPAdded = nNextShot
		End If
		If aanMballJackpots(nPlayer, eMballIce) >= anJackpotsForSuper(eMballIce) _
		And False = vSuperJPsLit.Contains(eMballIce) Then
			AddCallout "VO_ice_mball_sj_lit", eVOPrioMballReady, eCharacterIce
			vSuperJPsLit.add eMballIce
		End If
	End If

	If avModesRunning(nPlayer).Contains(eModeMegaWizard) Then Exit Sub
	for i = 0 to 7 : RemoveMballColor i, eColorWhite : Next
	If anTimerMballGrace(eMballIce) < 1 Then
		For i = 0 to 7
			If vIceJackpots.Contains(i) Then
				AddMballColor i, eColorWhite
			End If
		Next
		If vSuperJPsLit.Contains(eMballIce) then AddMballColor eShotSideRamp, eColorWhite
	End If
End Sub

Sub IceMballEnd
	Dim i
	for i = 0 to 7 : RemoveMballColor i, eColorWhite : Next
	vSuperJPsLit.Remove eMballIce
	avModesRunning(nPlayer).Remove eModeIceMBall
	DOF 225, DOFOff				
	If not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
		aanLocksMade(nPlayer, eMballIce) = 0
		aanMballsPlayed(nPlayer, eMballIce) = aanMballsPlayed(nPlayer, eMballIce) + 1
		UpdateWizardModesLit
	End If
	If not IsMultiballActive then
		PlayModeMusic
		ShowMBallScore
	End If
End Sub

Sub TotalMayhemStart
	Dim nBalls, i
	DOF 227, DOFPulse			  
	QueueDialogue "VO_water_mayhem_start", eVOPrioModeStart, eCharacterWater
	QueueDialogue "VO_hero_mayhem_start", eVOPrioModeStart, eCharacterHero
	' TODO: Mode intro and rule explanation
	PlayModeMusic
	BIP = BIP + nBallPhysLocked
	nBalls = 6 - BIP
	nBallPhysLocked = 0
	bReleasingLocks = True
	UpPostMove LockPost,LockPostPrimitive,"down"
	LockPostTimer.Enabled = True
	If nBalls > 0 Then AddBalls nBalls
	avMayhemTargets(nPlayer).Clear
	For i = 0 to 7
		aavShotsLit(nPlayer, eModeTotalMayhem).add i
	Next
	nTimeMayhem = 67499
	LightShootAgain.state = LightStateBlinking
	avWizardModesLit(nPlayer).Remove eModeTotalMayhem
	CommonModeStart eModeTotalMayhem
	CommonWizardStart
End Sub

Sub TotalMayhemUpdate(nShot, nSwitch)
	Dim i
	Dim nValue, bValidShot, nProgress, anTargets
	anTargets = Array(eSwitchTargetX1, eSwitchTargetX2, eSwitchTargetM1, eSwitchTargetBlue, _
		eSwitchTargetM2, eSwitchTargetX3, eSwitchTargetX4, eSwitchTargetM3, eSwitchTargetM4, _
		eSwitchDropI, eSwitchDropC, eSwitchDropE)

	If bDrainingBalls Then Exit Sub

	If eSwitchNone <> nSwitch Then
		for i = 0 to 11
			If False = avMayhemTargets(nPlayer).Contains(i) And nSwitch = anTargets(i) Then
				bValidShot = True
				avMayhemTargets(nPlayer).add i
			End If
		Next
	Elseif eShotNone <> nShot Then
		For i = 0 to 7
			If True = aavShotsLit(nPlayer, eModeTotalMayhem).Contains(i) And nShot = i Then
				bValidShot = True
				aavShotsLit(nPlayer, eModeTotalMayhem).remove i
			End If
		Next
	End If

	nProgress = (8 - aavShotsLit(nPlayer, eModeTotalMayhem).Count) _
				+ avMayhemTargets(nPlayer).count
	If bValidShot Then
		If eSwitchNone <> nSwitch Then
			nValue = 8000
			nValue = nValue * nPlayfieldX
			anModeScore(eModeTotalMayhem) = anModeScore(eModeTotalMayhem) + nValue
			anScore(nPlayer) = anScore(nPlayer) + nValue
			ShowText "TARGET HIT", WilliamsFormatNum(nValue) & " " & nProgress & "/20", 2500, eVOPrioJackpot
		Else
			nValue = 20000
			nValue = nValue * nPlayfieldX
			If eShotSideRamp = nShot Then
				nValue = nValue * 2
			ElseIf eShotNone <> nShot Then
				nValue = nValue * anComboX(nShot)
			End If
			anModeScore(eModeTotalMayhem) = anModeScore(eModeTotalMayhem) + nValue
			anScore(nPlayer) = anScore(nPlayer) + nValue
			ShowText "SHOT HIT", WilliamsFormatNum(nValue) & " " & nProgress & "/20", 2500, eVOPrioJackpot
			LightShowStart eLightShowMayhem, eVOPrioJackpot
			FlashSweep eColorYellow
		End If
	End If
	If nProgress > 19 or nTimeMayhem < 1 Then
		bDrainingBalls = True
		nTimeBallSave = 0
		nTimeDelaySaucerUpper = 1
		nTimeMayhem = 0
		BIP = BIP - (nAutoPlungeBalls + nBallsToFeed)
		nAutoPlungeBalls = 0
		nBallsToFeed = 0
		AllLightsOff
		LeftSlingshot.disabled = True
		RightSlingshot.disabled = True
		Bumper1.HasHitEvent = False
		Bumper2.HasHitEvent = False
		TopSlingShot.disabled = True
		LeftFlipper.RotateToStart
		LeftUpperFlipper.RotateToStart
		RightFlipper.RotateToStart
		DOF 101, DOFOff
		DOF 102, DOFOff

		If False = abModeExtraBall(nPlayer) and nProgress > 19 Then
			ShowText "TOTAL " & WilliamsFormatNum(anModeScore(eModeTotalMayhem)), _
				"EXTRA BALL LIT" , 3500, eVOPrioExtraBall
			abModeExtraBall(nPlayer) = True
			anExtraBallsLit(nPlayer) = anExtraBallsLit(nPlayer) + 1
			AddCallout "VO_hero_extra_ball_lit", eVOPrioExtraBall, eCharacterHero
			LightShowStart eLightShowModeComplete, eVOPrioExtraBall
		Else
			If nProgress > 19 Then
				LightShowStart eLightShowModeComplete, eVOPrioExtraBall
				anModeScore(eModeTotalMayhem) = anModeScore(eModeTotalMayhem) + 100000
				anScore(nPlayer) = anScore(nPlayer) + 100000
			End If
			ShowText "TOTAL MAYHEM", "SCORE " & WilliamsFormatNum(anModeScore(eModeTotalMayhem)), 3500, eVOPrioModeEnd
		End If
	End If

	If (eShotNone = nShot and eSwitchNone = nSwitch) _
	Or (eSwitchTargetX1 = nSwitch Or eSwitchTargetX2 = nSwitch _
	Or eSwitchTargetX3 = nSwitch Or eSwitchTargetX4 = nSwitch) Then
		For each i in aoXLights : i.BlinkInterval = 85 : Next
		aoXLights(1).BlinkPattern = "01" : aoXLights(3).BlinkPattern = "01"
		For each i in aoXLights : i.state = LightStateOff : Next
		If Not avMayhemTargets(nPlayer).Contains(0) Then aoXLights(0).state = 2
		If Not avMayhemTargets(nPlayer).Contains(1) Then aoXLights(1).state = 2
		If Not avMayhemTargets(nPlayer).Contains(5) Then aoXLights(2).state = 2
		If Not avMayhemTargets(nPlayer).Contains(6) Then aoXLights(3).state = 2
	End If
	If (eShotNone = nShot and eSwitchNone = nSwitch) _
	Or (eSwitchTargetM1 = nSwitch Or eSwitchTargetM2 = nSwitch _
	Or eSwitchTargetM3 = nSwitch Or eSwitchTargetM4 = nSwitch) Then
		For each i in aoBagLights : i.BlinkInterval = 85 : Next
		aoBagLights(1).BlinkPattern = "01" : aoBagLights(3).BlinkPattern = "01"
		For each i in aoBagLights : i.state = LightStateOff : Next
		If Not avMayhemTargets(nPlayer).Contains(2) Then aoBagLights(0).state = 2
		If Not avMayhemTargets(nPlayer).Contains(4) Then aoBagLights(1).state = 2
		If Not avMayhemTargets(nPlayer).Contains(7) Then aoBagLights(2).state = 2
		If Not avMayhemTargets(nPlayer).Contains(8) Then aoBagLights(3).state = 2
	End If
	If (eShotNone = nShot and eSwitchNone = nSwitch) _
	Or (eSwitchDropI = nSwitch Or eSwitchDropC = nSwitch Or eSwitchDropE = nSwitch) Then
		For each i in aoDropLights : i.BlinkInterval = 85 : Next
		aoDropLights(1).BlinkPattern = "01"
		For each i in aoDropLights : i.state = LightStateOff : Next
		If Not avMayhemTargets(nPlayer).Contains(9) Then aoDropLights(0).state = 2
		If Not avMayhemTargets(nPlayer).Contains(10) Then aoDropLights(1).state = 2
		If Not avMayhemTargets(nPlayer).Contains(11) Then aoDropLights(2).state = 2
	End If
	If (eShotNone = nShot and eSwitchNone = nSwitch) Or eSwitchTargetBlue = nSwitch Then
		lightTorpedo.BlinkInterval = 85
		lightTorpedo.state = LightStateOff
		If Not avMayhemTargets(nPlayer).Contains(3) then lightTorpedo.state = 2
	End If
	For i = 0 to 7
		ClearModeColor i
		ClearMballColor i
		If aavShotsLit(nPlayer, eModeTotalMayhem).Contains(i) Then
			AddMballColor i, eColorYellow
			AddModeColor i, eColorYellow
		End If
	Next
End Sub

Sub TotalMayhemEnd(bDrained)
	Dim i
	For each i in aoXLights : i.BlinkInterval = 170 : Next
	For each i in aoXLights : i.BlinkPattern = "10" : i.State = 0 : Next
	For each i in aoBagLights : i.BlinkInterval = 170 : Next
	For each i in aoBagLights : i.BlinkPattern = "10" : i.State = 0 : Next
	For each i in aoDropLights : i.BlinkInterval = 170 : Next
	For each i in aoDropLights : i.BlinkPattern = "10" : i.State = 0 : Next
	lightTorpedo.BlinkInterval = 170 : lightTorpedo.state = LightStateOff
	For i = 0 to 7 : ClearModeColor i : ClearMballColor i : Next

	aavShotsLit(nPlayer, eModeTotalMayhem).Clear
	' todo: callout
	avModesPlayed(nPlayer).add eModeTotalMayhem
	nTimeMayhem = 0
	UpdateWizardModesLit
	If not bDrained Then
		LeftSlingshot.disabled = False
		RightSlingshot.disabled = False
		Bumper1.HasHitEvent = True
		Bumper2.HasHitEvent = True
		TopSlingShot.disabled = False
		nModeSelected = an1BallModes(Int(Rnd * 12))
		ModeSelectNext
		CommonModeEnd
		CommonWizardEnd
		nTimeBallSave = cBallSaveStart
	End If
End Sub

Sub EvilUnitedStart
	Dim nBalls, i
	DOF 228, DOFPulse				  
	' TODO: Mode intro and rule explanation
	For i = 0 to 7
		anEvilUnitedShots(i) = 0
	Next
	vSuperJPsLit.Clear
	ClearMballColor eShotSideRamp
	nTimeBallSave = 32000
	LightShootAgain.state = LightStateBlinking
	UpdateLEDs -1
	QueueDialogue "VO_cyber_evil_united_start", eVOPrioModeStart, eCharacterCyber
	QueueDialogue "VO_water_evil_united_start", eVOPrioModeStart, eCharacterWater
	QueueDialogue "VO_fire_evil_united_start", eVOPrioModeStart, eCharacterFire
	QueueDialogue "VO_earth_evil_united_start", eVOPrioModeStart, eCharacterEarth
	QueueDialogue "VO_ice_evil_united_start", eVOPrioModeStart, eCharacterIce
	QueueDialogue "VO_air_evil_united_start", eVOPrioModeStart, eCharacterAir
	avWizardModesLit(nPlayer).Remove eModeEvilUnited
	anWizardModeRunning(nPlayer) = eModeEvilUnited
	PlayModeMusic
	CommonModeStart eModeEvilUnited
	UpPostMove LockPost,LockPostPrimitive,"up"
	CommonWizardStart
	LockPostTimer.Enabled = True
	BIP = BIP + nBallPhysLocked
	nBalls = 5 - nBallPhysLocked
	nBallPhysLocked = 0
	If nBalls > 0 Then AddBalls nBalls
End Sub

Sub EvilUnitedUpdate(nShot, nSwitch)
	Dim i, nValue, bValidShot, nProgress, anShotColors, asShotNames, anJackpots
	Dim asJPCallouts, asSJlitCallouts, anCharForShot, anShowForShot

	anShotColors = Array(eColorNone, eColorGreen, eColorBlue, eColorRed, _
		eColorOrange, eColorNone, eColorPurple, eColorWhite)
	anJackpots = Array(eShotNone, eMballCyber, eMBallWater, eMballFire, _
		eMballEarth, eShotNone, eMballAir, eMballIce)
	asShotNames = Array("", "CYBER", "WATER", "FIRE", "EARTH", "", "AIR", "ICE")
	asJPCallouts = Array("", "VO_cyber_mball_jackpot01x", "VO_water_mball_jackpot", _
		"VO_fire_mball_jackpot", "VO_earth_mball_jackpot", "", _
		"VO_air_mball_jackpot1x", "VO_water_mball_jackpot")
	asSJlitCallouts = Array("", "VO_cyber_mball_sj_lit", "VO_water_mball_sj_lit", _
		"VO_fire_mball_sj_ready", "VO_earth_mball_sj_lit", "", "VO_air_mball_sj_lit", _
		"VO_ice_mball_sj_lit")
	anCharForShot = Array(eCharacterHero, eCharacterCyber, eCharacterWater, _
		eCharacterFire, eCharacterEarth, eCharacterHero, eCharacterAir, eCharacterIce)
	anShowForShot = Array(eLightShowNone, eLightShowCyberShort, eLightShowWaterShort, _
		eLightShowFireShort, eLightShowEarthShort, eLightShowNone, _
		eLightShowAirShort, eLightShowIceShort)

	bValidShot = False
	For Each i in Array(eShotLeftLoop, eShotSaucer, eShotLeftRamp, eShotEarth, _
						eShotRightRamp, eShotRightLoop)
		If nShot = i and anEvilUnitedShots(i) < 3 Then bValidShot = True
	Next

	If bValidShot Then
		If 2 = anEvilUnitedShots(nShot) Then
			nValue = 20000
		ElseIf 1 = anEvilUnitedShots(nShot) Then
			nValue = 16000
		Else
			nValue = 16000
		End If
		nValue = nValue * nPlayfieldX * anComboX(nShot)
		anScore(nPlayer) = anScore(nPlayer) + nValue
		anModeScore(eModeEvilUnited) = anModeScore(eModeEvilUnited) + nValue
		aanModeShots(nPlayer, eModeEvilUnited) = aanModeShots(nPlayer, eModeEvilUnited) + 1
		anEvilUnitedShots(nShot) = anEvilUnitedShots(nShot) + 1
		If anEvilUnitedShots(nShot) < 3 Then
			ShowText asShotNames(nShot) & " JACKPOT ", WilliamsFormatNum(nValue), 2500, eVOPrioJackpot
			AddCallout asJPCallouts(nShot), eVOPrioJackpot, anCharForShot(nShot)
		Else
			ShowText asShotNames(nShot) & " JP " & WilliamsFormatNum(nValue), _
				asShotNames(nShot) & " SUPER LIT", 2500, eVOPrioModeProgress
			If eShotNone <> anJackpots(nShot) then
				vSuperJPsLit.add anJackpots(nShot)
				AddCallout asSJlitCallouts(nShot), eVOPrioMballReady, anCharForShot(nShot)
			End If
		End If
		FlashSweep anShotColors(nShot)
		LightShowStart anShowForShot(nShot), eVOPrioJackpot
		' todo: callout / sound
	End If

	For i = 0 to 7
		clearModeColor i
		If eColorNone <> anShotColors(i) Then
			ClearMballColor i
			If 0 = anEvilUnitedShots(i) Then
				AddModeColor i, anShotColors(i)
			Elseif 1 = anEvilUnitedShots(i) Then
				AddMballColor i, anShotColors(i)
			ElseIf 2 = anEvilUnitedShots(i) Then
				AddModeColor i, anShotColors(i)
				AddMballColor i, anShotColors(i)
			End If
		End If
	Next
	If vSuperJPsLit.Contains(eMballCyber) then AddMballColor eShotSideRamp, eColorGreen
	If vSuperJPsLit.Contains(eMballWater) then AddMballColor eShotSideRamp, eColorBlue
	If vSuperJPsLit.Contains(eMballFire) then AddMballColor eShotSideRamp, eColorRed
	If vSuperJPsLit.Contains(eMballEarth) then AddMballColor eShotSideRamp, eColorOrange
	If vSuperJPsLit.Contains(eMballAir) then AddMballColor eShotSideRamp, eColorPurple
	If vSuperJPsLit.Contains(eMballIce) then AddMballColor eShotSideRamp, eColorWhite
End Sub

Sub EvilUnitedEnd(bDrained)
	Dim i
	vSuperJPsLit.Clear
	For i = 0 to 7
		ClearModeColor i
		ClearMBallColor i
	Next
	avModesPlayed(nPlayer).add eModeEvilUnited
	avModesRunning(nPlayer).Remove eModeEvilUnited
	For Each i in an1BallModes
		avModesPlayed(nPlayer).Remove i
	Next
	ShowText "EVIL UNITED", "TOTAL " & WilliamsFormatNum(anModeScore(eModeEvilUnited)), 3000, eVOPrioModeEnd
	UpdateWizardModesLit
	ModeSelectNext
	CommonModeEnd
	CommonWizardEnd
	nTimeBallSave = cBallSaveStart
End Sub

Sub SuperSecretStart
	Dim i, j, nBalls
	oQueueSecretTop.Clear
	oQueueSecretBottom.Clear
	oQueueSecretValue.Clear
	nTimeTopSecretText = 0
	bTopSecretUpdate = True
	nTopSecretSound = 0
	nTopSecretFlash = 0
	anModeScore(eModeMegaWizard) = 0
	avModesRunning(nPlayer).add eModeMegaWizard
	For Each i in an1BallModes
		StartMode i
	Next
	CyberMballStart
	WaterMballStart
	FireMBallStart
	EarthMBallStart False
	AirMballStart
	IceMballStart
	BIP = BIP + nBallPhysLocked
	nBalls = 5 - nBallPhysLocked
	nBallPhysLocked = 0
	bReleasingLocks = True
	UpPostMove LockPost,LockPostPrimitive,"down"
	LockPosttimer.Enabled = True
	If nBalls > 0 Then AddBalls nBalls
	nTimeBallSave = cBallSaveStart * 3
	LightShootAgain.state = LightStateBlinking
	anWizardModeRunning(nPlayer) = eModeMegaWizard
	avWizardModesLit(nPlayer).Remove eModeMegaWizard
	AddCallout "VO_hero_super_wizard_start", eVOPrioModeStart, eCharacterHero
	CommonModeStart eModeMegaWizard
	CommonWizardStart
	For j = 0 to 7
		for Each i in array(eColorGreen, eColorBlue, eColorRed, eColorOrange, _
			eColorPurple, eColorWhite)
			AddModeColor j, i
			If j <> eShotSideRamp Then AddMBallColor j, i
		Next
	Next
End Sub

Sub SuperSecretUpdate(nShot, nSwitch)
	Dim i, nTotal
	If bTopSecretUpdate Then
		bTopSecretUpdate = False
		nTotal = 0
		For Each i in an1BallModes
			nTotal = nTotal + anModeScore(i)
		Next
		For Each i in Array(eModeCyberMBall, eModeWaterMBall, eModeFireMBall, _
		eModeEarthMBall, eModeAirMBall, eModeIceMBall)
			nTotal = nTotal + anModeScore(i)
		Next
		anModeScore(eModeMegaWizard) = nTotal

		If nTotal >= 2000000 Then
			oQueueSecretTop.Clear
			oQueueSecretBottom.Clear
			oQueueSecretValue.Clear
			bDrainingBalls = True
			nTimeBallSave = 0
			nTimeDelaySaucerUpper = 1
			nTimeMayhem = 0
			BIP = BIP - (nAutoPlungeBalls + nBallsToFeed)
			nAutoPlungeBalls = 0
			nBallsToFeed = 0
			AllLightsOff
			LeftSlingshot.disabled = True
			RightSlingshot.disabled = True
			Bumper1.HasHitEvent = False
			Bumper2.HasHitEvent = False
			TopSlingShot.disabled = True
			LeftFlipper.RotateToStart
			LeftUpperFlipper.RotateToStart
			RightFlipper.RotateToStart
			DOF 101, DOFOff
			DOF 102, DOFOff

			anModeScore(eModeMegaWizard) = anModeScore(eModeMegaWizard) + 2000000
			anScore(nPlayer) = anScore(nPlayer) + 2000000
			ShowText "2 MILLION BONUS", "TOTAL " & WilliamsFormatNum(anModeScore(eModeMegaWizard)), 3500, eVOPrioSuperJackpot
			' TODO: sound + lightshow
		End If
	End If
End Sub

Sub SuperSecretEnd(bDrained)
	Dim i
	oQueueSecretTop.Clear
	oQueueSecretBottom.Clear
	oQueueSecretValue.Clear
	For Each i in an1BallModes
		EndMode i, False
	Next
	CyberMballEnd
	WaterMballEnd
	FireMballEnd
	EarthMballEnd
	AirMballEnd
	IceMballEnd

	avModesRunning(nPlayer).Remove eModeMegaWizard
	If bDrained Then
		ShowText "TOP SUPER SECRET", "TOTAL " & WilliamsFormatNum(anModeScore(eModeMegaWizard)), 3000, eVOPrioModeEnd
	Else
		LeftSlingshot.disabled = False
		RightSlingshot.disabled = False
		Bumper1.HasHitEvent = True
		Bumper2.HasHitEvent = True
		TopSlingShot.disabled = False
		nModeSelected = an1BallModes(Int(Rnd * 12))
		ModeSelectNext
		CommonModeEnd
		CommonWizardEnd
		nTimeBallSave = cBallSaveStart
	End If
	avSJCollected(nPlayer).Clear
	For Each i in Array(eModeCyberWizard, eModeWaterWizard, eModeFireWizard, _
	eModeEarthWizard, eModeAirWizard, eModeIceWizard, eModeEvilUnited, eModeMegaWizard)
		avModesPlayed(nPlayer).Remove i
	Next
	avWizardModesLit(nPlayer).Remove eModeMegaWizard
	UpdateWizardModesLit
End Sub

Sub TopSecretUpdate(sTextTop, sTextBottom, nValue)
	oQueueSecretTop.Enqueue sTextTop
	oQueueSecretBottom.Enqueue sTextBottom
	oQueueSecretValue.Enqueue nValue
End Sub

Sub ComboCallout
	nComboNoCallout = nComboNoCallout + 1
	If nComboNoCallout mod 2 = 0 Then
		AddCallout "VO_hero_combo", eVOPrioCombo, eCharacterHero
	End If
End Sub

' modes
Sub TimerMode_Timer
	Dim i, j
	Dim vModesTimedOut, nTimeNext, nNextShot
'	sTimersRun = sTimersRun + " TimerMode"

	' Post timers always run
	If nTimeRightRampPost > 0 Then
		nTimeRightRampPost = nTimeRightRampPost - TimerMode.interval
		If nTimeRightRampPost < 1 then
			nTimeRightRampPost = 0
			UpPostMove RightRampPost,RightRampPostPrimitive,"down"
			bBallHeld = False
		End If
	End If

	If nTimeLeftRampPost > 0 Then
		nTimeLeftRampPost = nTimeLeftRampPost - TimerMode.interval
		If nTimeLeftRampPost < 1 then
			nTimeLeftRampPost = 0
			UpPostMove LeftRampPost,LeftRampPostPrimitive,"down"
			bBallHeld = False
		End If
	End If

	If nTimeLoopPost > 0 Then
		nTimeLoopPost = nTimeLoopPost - TimerMode.interval
		If nTimeLoopPost < 1 then
			nTimeLoopPost = 0
			UpPostMove LoopPost,LoopPostPrimitive,"down"
			bBallHeld = False
		End If
	End If

	If nTimeSkillPost > 0 Then
		nTimeSkillPost = nTimeSkillPost - TimerMode.interval
		If nTimeSkillPost < 1 then
			nTimeSkillPost = 0
			UpPostMove SkillPost,SkillPostPrimitive,"down"
			bBallHeld = False
		End If
	End If

	If nTimeDelayMultiball > 0 Then
		nTimeDelayMultiball = nTimeDelayMultiball - TimerMode.interval
		If nTimeDelayMultiball < 1 then
			nTimeDelayMultiball = 0
			bBallHeld = False
			if avModesRunning(nPlayer).Contains(eModeEarthMBall) Then
				oMagnetA.MagnetOn = False
				UpPostMove LockPost,LockPostPrimitive,"down"
				LockPostTimer.Enabled = True
				bReleasingLocks = True
			end if
		End If
	End If

	If nPlayersInGame < 1 Then Exit Sub
	If bCountingBonus Then Exit Sub
	
	' Halt timers while draining Balls
	If bDrainingBalls Then
		nTimeBallSave = 0
		Exit Sub
	End If

	' Do not count down timers while in the Bumpers, unless a multiball is running
	If BIP < 2 And (GameTime - nTimeBumperHit < 500) Then
		Exit Sub
	End If

	If nTimeModeSelectVO > 0 Then
		nTimeModeSelectVO = nTimeModeSelectVO -  TimerMode.interval
		If nTimeModeSelectVO < 0 Then nTimeModeSelectVO = 0
		If bSelectingMode and (nTimeModeSelectVO < 1) Then
			If Not bFirstSwitchHit Then
				AddCallout "VO_hero_mode_select_plunger", eVOPrioModeSelect, eCharacterHero
			Else
				AddCallout "VO_hero_mode_select_saucer", eVOPrioModeSelect, eCharacterHero
			End If
		End If
	End If

	If True = bFirstSwitchHit And ((False = bBallHeld) Or (BIP > 1 And nTimeDelayMultiball < 1)) Then
		If nTimeBallSave > 0 Then
			nTimeBallSave = nTimeBallSave - TimerMode.interval
			' Turn light off before the saver runs out, as a grace period
			If nTimeBallSave < 2000 Then
				If nExtraBalls > 0 Then
					LightShootAgain.state = LightStateOn
				Else
					LightShootAgain.state = LightStateOff
				End If
			End If
			' Make sure timer does not go negative
			If nTimeBallSave <= 0 then nTimeBallSave = 0
		End If

		If nPlayfieldXTimer > 0 Then
			nPlayfieldXTimer = nPlayfieldXTimer - TimerMode.interval
			If nPlayfieldXTimer < 1 then
				If TimerSJCallout.Enabled Then
					nPlayfieldXTimer = TimerMode.interval
				Else
					nPlayfieldXTimer = 0
					nPlayfieldX = 1
					PlayJingle "SFX_playfield_1X", 3000
				End If
			End If
		End If

		For i = 0 to 7
			If anComboTimer(i) > 0 Then
				anComboTimer(i) = anComboTimer(i) - TimerMode.interval
				If anComboTimer(i) < 1 then
					anComboTimer(i) = 0
					EndCombo i
					Exit For
				End If
			End If
		Next

		If avModesRunning(nPlayer).Contains(eModeCyberMBall) Then
			nCyberTimer = nCyberTimer - TimerMode.interval
			If nCyberTimer < 1 Then
				vCyberJackpots.add eShotUnderFlipper : vCyberJackpots.add eShotLeftLoop
				vCyberJackpots.add eShotSaucer : vCyberJackpots.add eShotLeftRamp
				vCyberJackpots.add eShotEarth : vCyberJackpots.add eShotRightRamp
				vCyberJackpots.add eShotRightLoop
				nCyberTimer = 5000
				CyberMballUpdate eShotNone
			End If
		End If

		If avModesRunning(nPlayer).Contains(eModeCyberWizard) Then
			Dim bAttacked, anAdjacentShots, nSecondsLeft, sCountdown
			bAttacked = False
			nCyber3AttackTime = nCyber3AttackTime - TimerMode.interval
			If nCyber3AttackTime < 1 Then
				nCyber3AttackTime = cCyber3AttackInterval
				' Check for 2 shots in a column
				For Each i in Array(eShotLeftRamp, eShotEarth, eShotSaucer, _
				eShotRightRamp, eShotLeftLoop, eShotRightLoop, eShotUnderFlipper)
					If aanCyber3ShotsHit(nPlayer, i) > 1 Then
						anCyber3AttackShot(nPlayer) = i
						bAttacked = True
						Exit For
					End If
				Next
				If Not bAttacked Then
					For Each i in Array(eShotLeftRamp, eShotEarth, eShotSaucer, _
					eShotRightRamp, eShotLeftLoop, eShotRightLoop, eShotUnderFlipper)
						If aanCyber3ShotsHit(nPlayer, i) > 0 Then
							anAdjacentShots = GetAdjacentShots(i, False)
							' Check for shot and shot to the left
							If anAdjacentShots(1) <> eShotNone Then
								If aanCyber3ShotsHit(nPlayer, anAdjacentShots(1)) > 0 Then
									If anAdjacentShots(1) = anCyber3AttackShot(nPlayer) Then
										anCyber3AttackShot(nPlayer) = i
									Else
										anCyber3AttackShot(nPlayer) = anAdjacentShots(1)
									End If
									bAttacked = True
									Exit For
								End If
							End If
							' Check for shot and shot to the right
							If anAdjacentShots(2) <> eShotNone Then
								If aanCyber3ShotsHit(nPlayer, anAdjacentShots(2)) > 0 Then
									If anAdjacentShots(2) = anCyber3AttackShot(nPlayer) Then
										anCyber3AttackShot(nPlayer) = i
									Else
										anCyber3AttackShot(nPlayer) = anAdjacentShots(2)
									End If
									bAttacked = True
									Exit For
								End If
							End If
							' Check for shot and shot two to the left (which would make
							' one to the left 3 in a row)
							If anAdjacentShots(0) <> eShotNone Then
								If aanCyber3ShotsHit(nPlayer, anAdjacentShots(0)) > 0 Then
									If anAdjacentShots(1) <> anCyber3AttackShot(nPlayer) Then
										anCyber3AttackShot(nPlayer) = anAdjacentShots(1)
										bAttacked = True
										Exit For
									End If
								End If
							End If
							' Check for shot and shot two to the right (which would make
							' one to the right 3 in a row)
							If anAdjacentShots(3) <> eShotNone Then
								If aanCyber3ShotsHit(nPlayer, anAdjacentShots(3)) > 0 Then
									If anAdjacentShots(2) <> anCyber3AttackShot(nPlayer) Then
										anCyber3AttackShot(nPlayer) = anAdjacentShots(2)
										bAttacked = True
										Exit For
									End If
								End If
							End If
						End If
					Next
				End If
				If Not bAttacked Then
					For Each i in Array(eShotLeftRamp, eShotEarth, eShotSaucer, _
					eShotRightRamp, eShotLeftLoop, eShotRightLoop, eShotUnderFlipper)
						If aanCyber3ShotsHit(nPlayer, i) > 0 _
						And i <> anCyber3AttackShot(nPlayer) Then
							anCyber3AttackShot(nPlayer) = i
							bAttacked = True
							Exit For
						End If
					Next
				End If
				If False = bAttacked and eShotEarth = anCyber3AttackShot(nPlayer) Then
					anCyber3AttackShot(nPlayer) = eShotLeftRamp
					bAttacked = True
				Elseif False = bAttacked Then
					anCyber3AttackShot(nPlayer) = eShotEarth
					bAttacked = True
				End If
				AddCallout "VO_showdown_cyber_attack", eVOPrioModeProgress, eCharacterCyber
				aanCyber3ShotsHit(nPlayer, anCyber3AttackShot(nPlayer)) = 0
				CyberMode3Update eShotNone, eSwitchNone
			Else
				nSecondsLeft = nCyber3AttackTime \ 1000
				If nSecondsLeft > 0 and nSecondsLeft < 6 Then
					sCountdown = "VO_cyber_showdown_countdown" & nSecondsLeft
					If sCountdown <> sLastCountdown Then
						AddCallout sCountdown, eVOPrioCountdown, eCharacterCyber
						sLastCountdown = sCountdown
					End if
				End If
			End If
			UpdateLEDs nCyber3AttackTime \ 1000
		End If

		If avModesRunning(nPlayer).Contains(eModeWater2) Then
			For i = 0 to 7
				If anTimeWater2(i) > 0 Then
					anTimeWater2(i) = anTimeWater2(i) - TimerMode.interval
					If anTimeWater2(i) < 1 then
						anTimeWater2(i) = 0
						aavShotsLit(nPlayer, eModeWater2).Remove i
						WaterMode2Update eShotNone, eSwitchNone
					End If
				End If
			Next
		End If

		If avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
			nTimerWaterMove = nTimerWaterMove - TimerMode.interval
			If nTimerWaterMove < 1 Then
				nTimerWaterMove = 3000
				nWaterFirstJackpot = (nWaterFirstJackpot + 1) Mod 8
				If nWaterFirstJackpot > eShotEarth Then
					nWaterFirstJackpot = (nWaterFirstJackpot + 1) Mod 8
				End If
				WaterMballUpdate eShotNone
			End If
		Else
			for i = 0 to 7
				If anTimerTorpedo(i) > 0 Then
					anTimerTorpedo(i) = anTimerTorpedo(i) - TimerMode.interval
					If anTimerTorpedo(i) < 1 Then
						anTimerTorpedo(i) = 0
						RemoveMballColor i, eColorBlue
					End If
				End If
			Next
		End If

		If avModesRunning(nPlayer).Contains(eModeFireMBall) Then
			Dim anNewTimers(7)
			Dim abNewDirections(7)
			Dim bNewFire : bNewFire = False
			Dim bDirectionChange
			Dim nChanges
			Dim nClusterEnd : nClusterEnd = -1
			For i = 0 to 7 : anNewTimers(i) = 0 : Next
			For Each i in Array(eShotLeftLoop, eShotSaucer, eShotLeftRamp, eShotEarth, _
				eShotRightRamp, eShotRightLoop)
				If i > nClusterEnd Then
					If anFireSpreadTimer(i) > 0 Then
						anFireSpreadTimer(i) = anFireSpreadTimer(i) - TimerMode.interval
						If anFireSpreadTimer(i) < 1 Then
							nClusterEnd = i
							for j = i to 7
								If j <> eShotSideRamp Then
									If anFireSpreadTimer(i) > 0 then nClusterEnd = j
								End If
							Next
							For j = i to nClusterEnd
								If j <> eShotSideRamp Then anFireSpreadTimer(j) = 20000
							Next
							nChanges = 0
							bDirectionChange = False
							While False = bDirectionChange and nChanges < 2
								nChanges = nChanges + 1
								If abFireSpreadLeft(i) and not bDirectionChange Then
									nNextShot = i - 1
									If nNextShot = eShotSideRamp then nNextShot = eShotEarth
									If nNextShot < eShotLeftLoop Then
										nNextShot = eShotNone
										bDirectionChange = not bDirectionChange
									End If
								Else
									nNextShot = nClusterEnd + 1
									If nNextShot = eShotSideRamp then nNextShot = eShotRightRamp
									If nNextShot > eShotRightLoop Then
										nNextShot = eShotNone
										bDirectionChange = not bDirectionChange
									End If
								End If
								If eShotNone <> nNextShot Then
									If nNextShot < 0 Or nNextShot > 7 then msgbox nNextShot
									bNewFire = True
									anNewTimers(nNextShot) = 20000
									abNewDirections(nNextShot) = abFireSpreadLeft(i)
								End If
							Wend
						End If
					End if
				End If
			Next
			If bNewFire Then
				For Each i in Array(eShotLeftLoop, eShotSaucer, eShotLeftRamp, eShotEarth, _
					eShotRightRamp, eShotRightLoop)
					If anNewTimers(i) > 0 Then
						anFireSpreadTimer(i) = anNewTimers(i)
						abFireSpreadLeft(i) = abNewDirections(i)
					End If
					If anFireSpreadTimer(i) = 20000 Then
						abFireSpreadLeft(i) = not abFireSpreadLeft(i)
					End If
				Next
				FireMBallUpdate eShotNone
			End If
		End If

		If avModesRunning(nPlayer).Contains(eModeEarthWizard) Then
			If nTimeEarth3Open > 0 Then
				nTimeEarth3Open = nTimeEarth3Open - TimerMode.interval
				If nTimeEarth3Open < 2500 Then
					UpdateLEDs 0
				Else
					UpdateLEDs CInt((nTimeEarth3Open - 2500) / 1000)
				End If
				If nTimeEarth3Open < 1 then
					nTimeEarth3Open = 0
					For i = 0 to 7 : aavShotsLit(nPlayer, eModeEarthWizard).add i : Next
					If aanModeShots(nPlayer, eModeEarthWizard) > 1 Then
						aavShotsLit(nPlayer, eModeEarthWizard).Remove eShotLeftRamp
					End If
					If aanModeShots(nPlayer, eModeEarthWizard) > 0 Then
						aavShotsLit(nPlayer, eModeEarthWizard).Remove eShotEarth
					End If
					AddCallout "VO_earth_showdown_close", eVOPrioModeProgress, eCharacterEarth
					bTurnClockwise = False
					TimerTurntable_Timer
					anEarth3Misses(nPlayer) = anEarth3Misses(nPlayer) + 1
					EarthMode3Update eShotNone, eSwitchNone
				Else
					nSecondsLeft = CInt((nTimeEarth3Open - 2500) / 1000)
					If nSecondsLeft > 0 and nSecondsLeft < 11 Then
						If 10 = nSecondsLeft Then
							sCountdown = "VO_earth_showdown_count10"
						Else
							sCountdown = "VO_earth_showdown_count0" & nSecondsLeft
						End If
						If sCountdown <> sLastCountdown Then
							AddCallout sCountdown, eVOPrioCountdown, eCharacterEarth
							sLastCountdown = sCountdown
						End if
					End If
				End If
			End If
		End If

		If avModesRunning(nPlayer).Contains(eModeAir1) Then
			nTimeAir1Move = nTimeAir1Move - TimerMode.interval
			If nTimeAir1Move < 1 Then
				If avModesPlayed(nPlayer).Contains(eModeAir1) then
					Select Case aanModeShots(nPlayer, eModeAir1)
						Case 0, 1, 2, 3
							nTimeNext = 4000
						Case 4, 5, 6, 7
							nTimeNext = 3000
						Case Else
							nTimeNext = 2000
					End Select
				Else
					Select Case aanModeShots(nPlayer, eModeAir1)
						Case 0, 1, 2
							nTimeNext = 4000
						Case 3, 4
							nTimeNext = 3000
						Case Else
							nTimeNext = 2000
					End Select
				End If
				nTimeAir1Move = nTimeNext
				If bAir1MoveLeft Then
					If anAir1Shot(nPlayer) < 1 Then
						anAir1Shot(nPlayer) = anAir1Shot(nPlayer) + 1
						bAir1MoveLeft = False
					Else
						anAir1Shot(nPlayer) = anAir1Shot(nPlayer) - 1
					End If
				Else
					nNextShot = anAir1Shot(nPlayer) + aavShotsLit(nPlayer, eModeAir1).Count
					If nNextShot > 7 Then
						anAir1Shot(nPlayer) = anAir1Shot(nPlayer) - 1
						bAir1MoveLeft = True
					Else
						anAir1Shot(nPlayer) = anAir1Shot(nPlayer) + 1
					End If
				End If
				AirMode1Update eShotNone, eSwitchNone
			End If
		End If

		If avModesRunning(nPlayer).Contains(eModeAirWizard) Then
			If nTimeAir3Move > 0 Then
				nTimeAir3Move = nTimeAir3Move - TimerMode.interval
				If nTimeAir3Move < 1 then
					If aanModeShots(nPlayer, eModeAirWizard) > 49 Then
						nTimeAir3Move = 1500
					Else
						nTimeAir3Move = 2500
					End If
					If aanModeShots(nPlayer, eModeAirWizard) > 24 _
					And aanModeShots(nPlayer, eModeAirWizard) < 75 Then
						anAir3Shot(nPlayer) = (anAir3Shot(nPlayer) + 7) Mod 8
					Else
						anAir3Shot(nPlayer) = (anAir3Shot(nPlayer) + 1) Mod 8
					End If
					AirMode3Update eShotNone, eSwitchNone
				End If
			End If
		End If

		If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
			If nTimeMayhem > 0 Then
				nTimeMayhem = nTimeMayhem - TimerMode.interval
				If nTimeMayhem > 2499 Then
					UpdateLEDs CInt((nTimeMayhem - 2500) / 1000)
				Else
					UpdateLEDs 0
				End If
				If nTimeMayhem < 1 then
					nTimeMayhem = 0
					TotalMayhemUpdate eShotNone, eSwitchNone
				End If
				nSecondsLeft = CInt((nTimeMayhem - 2500) / 1000)
				If nSecondsLeft > 0 and nSecondsLeft < 11 Then
					If 10 = nSecondsLeft Then
						sCountdown = "VO_earth_showdown_count10"
					Else
						sCountdown = "VO_earth_showdown_count0" & nSecondsLeft
					End If
					If sCountdown <> sLastCountdown Then
						AddCallout sCountdown, eVOPrioCountdown, eCharacterEarth
						sLastCountdown = sCountdown
					End if
				End If
			End If
		End If

		If BIP > 1 Or (Not TimerSJCallout.Enabled) Then
			For i = eMballCyber to eMballIce
				If anTimerMballGrace(i) > 0 Then
					anTimerMballGrace(i) = anTimerMballGrace(i) - TimerMode.interval
					If anTimerMballGrace(i) < 1 Then
						anTimerMballGrace(i) = 0
						Select Case i
							Case eMballCyber
								CyberMballEnd
							Case eMballWater
								WaterMballEnd
							Case eMballFire
								FireMballEnd
							Case eMballEarth
								EarthMballEnd
							Case eMballAir
								AirMballEnd
							Case eMballIce
								IceMballEnd
						End Select
						PlaySound "SFX_dong",,0.5
						nFirstMBallStarted = eModeNone
					End If
				End If
			Next
		End If

		If nDropResetTimer > 0 Then
			nDropResetTimer = nDropResetTimer - TimerMode.interval
			If nDropResetTimer < 1 Then
				nDropResetTimer = 0
				avDropsHit(nPlayer).Clear
				RaiseDrops
			End If
		End If

	End If
End Sub

Sub TimerSJCallout_timer
	Dim i
	Dim anSuperJackpotValues
	Dim nValue
	Dim asJackpotText, asJackpotCallout, anShowForJP
	asJackpotText = Array("CYBER CYBER", "WATER WATER", "FIRE FIRE FIRE",_
		"EARTH EARTH", "AIR AIR AIR AIR", "ICE ICE ICE ICE")
	asJackpotCallout = Array("VO_cyber_mball_sj", "VO_water_mball_sj", _
		"VO_fire_mball_sj", "VO_earth_mball_sj", "VO_air_mball_sj", "VO_ice_mball_sj")
	anShowForJP = Array(eLightShowCyberShort, eLightShowWaterShort, _
		eLightShowFireShort, eLightShowEarthShort, eLightShowAirShort, eLightShowIceShort)
	If vSuperJPsLit.Count > 0 Then
		If sJinglePlaying <> "" Then
			StopSound sJinglePlaying
			sJinglePlaying = ""
		End If
		PlayJingle "SFX_super_jackpot_" & (1 + vSuperJPsLit.Count), TimerSJCallout.interval
		nSuperJacksShown = nSuperJacksShown + 1
		If avModesRunning(nPlayer).Contains(eModeEvilUnited) Then
			If vSuperJPsLit.Contains(eMballCyber) Then
				anEvilUnitedShots(eShotLeftLoop) = 0
			End If
			If vSuperJPsLit.Contains(eMBallWater) Then
				anEvilUnitedShots(eShotSaucer) = 0
			End If
			If vSuperJPsLit.Contains(eMballFire) Then
				anEvilUnitedShots(eShotLeftRamp) = 0
			End If
			If vSuperJPsLit.Contains(eMballEarth) Then
				anEvilUnitedShots(eShotEarth) = 0
			End If
			If vSuperJPsLit.Contains(eMballAir) Then
				anEvilUnitedShots(eShotRightRamp) = 0
			End If
			If vSuperJPsLit.Contains(eMBallWater) Then
				anEvilUnitedShots(eShotRightLoop) = 0
			End If
		End If
		for i = eMballCyber to eMballIce
			If vSuperJPsLit.Contains(i) Then
				If Not avModesRunning(nPlayer).Contains(eModeEvilUnited) _
				And Not avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
					aanMballJackpots(nPlayer, i) = 0
					avSJCollected(nPlayer).add i
				End If
				vSuperJPsLit.Remove i
				ShowText asJackpotText(i), asJackpotText(i), TimerSJCallout.interval, eVOPrioSuperJackpot
				AddCallout asJackpotCallout(i), eVOPrioSuperJackpot, i
				FlashSweep Array(anMballColors(i), anMballColors(i))
				LightShowStart anShowForJP(i), eVOPrioSuperJackpot
				Exit For
			End If
		Next
	Else																																										
		UpPostMove RightRampPost,RightRampPostPrimitive,"down"
		TimerSJCallout.Enabled = False
		anSuperJackpotValues = Array(10, 30000, 90000, 180000, 300000, 450000, 630000)
		If nSuperJacksShown > 6 Then nSuperJacksShown = 6
		nValue = anSuperJackpotValues(nSuperJacksShown) * nPlayfieldX
		If sJinglePlaying <> "" Then
			StopSound sJinglePlaying
			sJinglePlaying = ""
		End If
		PlayJingle "SFX_super_jackpot_1", TimerSJCallout.interval * 4
		If nValue < 90000 Then
			AddCallout "VO_hero_mball_sj_medium", eVOPrioSuperJackpot, eCharacterHero
		ElseIf nValue < 180000 Then
			AddCallout "VO_hero_mball_sj_large", eVOPrioSuperJackpot, eCharacterHero
		Else
			AddCallout "VO_hero_mball_sj_xl", eVOPrioSuperJackpot, eCharacterHero
		End If
		ShowText "SUPER JACKPOT", WilliamsFormatNum(nValue), 3000, eVOPrioSuperJackpot
		LightShowStart eLightShowSuper, eVOPrioSuperJackpot
		FlashSweep Array(eColorWhite, eColorBlue, eColorGreen, eColorRed)
		anScore(nPlayer) = anScore(nPlayer) + nValue
		for each i in Array(eModeCyberMBall, eModeWaterMBall, eModeFireMBall, _
		eModeEarthMBall, eModeAirMBall, eModeIceMBall, eModeEvilUnited)
			if avModesRunning(nPlayer).Contains(i) then
				anModeScore(i) = anModeScore(i) + nValue
				exit For
			end if
		next
		If avModesRunning(nPlayer).Contains(eModeEvilUnited) Then
			If BIP < 2 Then
				EvilUnitedEnd True
			Else
				EvilUnitedUpdate eShotNone, eSwitchNone
			End If
		End If
	End If
End Sub

Function getLeastRecentShots(nShots)
	Dim aIndices
	Dim nLowestTime
	Dim nLowestIndex
	Dim anShotTimeCopy(7)
	Dim temp
	Dim i, j
	aIndices = Array(0, 1, 2, 3, 4, 5, 6, 7)

	' create a copy of the array of shot times so that we can sort it without
	' affecting the global aaEnemiesTime variable
	For i = 0 to 7
		anShotTimeCopy(i) = aanShotLastHit(nPlayer, i)
	Next

	' sort the enemy times and adjust the aIndices array after how the shot
	' times were sorted
	For i = 0 to 6
		nLowestTime = anShotTimeCopy(i)
		nLowestIndex = i
		For j = i + 1 to 6
			If anShotTimeCopy(j) < nLowestTime Then
				nLowestTime = anShotTimeCopy(j)
				nLowestIndex = j
			End If
		Next
		temp = anShotTimeCopy(i)
		anShotTimeCopy(i) = anShotTimeCopy(nLowestIndex)
		anShotTimeCopy(nLowestIndex) = temp
		temp = aIndices(i)
		aIndices(i) = aIndices(nLowestIndex)
		aIndices(nLowestIndex) = temp
	Next

	' truncate the array of indeces 
	ReDim Preserve aIndices(nShots - 1)

	getLeastRecentShots = aIndices

End Function

Function GetAdjacentShots(nShot, bWrapEdges)
	Select Case nShot
		Case eShotUnderFlipper
			If bWrapEdges Then
				GetAdjacentShots = Array(eShotRightRamp, eShotRightLoop, eShotLeftLoop, eShotSaucer)
			Else
				GetAdjacentShots = Array(eShotNone, eShotNone, eShotLeftLoop, eShotSaucer)
			End If
		Case eShotLeftLoop
			If bWrapEdges Then
				GetAdjacentShots = Array(eShotRightLoop, eShotUnderFlipper, eShotSaucer, eShotLeftRamp)
			Else
				GetAdjacentShots = Array(eShotNone, eShotUnderFlipper, eShotSaucer, eShotLeftRamp)
			End If
		Case eShotSaucer
			GetAdjacentShots = Array(eShotUnderFlipper, eShotLeftLoop, eShotLeftRamp, eShotEarth)
		Case eShotLeftRamp
			GetAdjacentShots = Array(eShotLeftLoop, eShotSaucer, eShotEarth, eShotRightRamp)
		Case eShotEarth
			GetAdjacentShots = Array(eShotSaucer, eShotLeftRamp, eShotRightRamp, eShotRightLoop)
		Case eShotRightRamp
			If bWrapEdges Then
				GetAdjacentShots = Array(eShotLeftRamp, eShotEarth, eShotRightLoop, eShotUnderFlipper)
			Else
				GetAdjacentShots = Array(eShotLeftRamp, eShotEarth, eShotRightLoop, eShotNone)
			End If
		Case eShotRightLoop
			If bWrapEdges Then
				GetAdjacentShots = Array(eShotEarth, eShotRightRamp, eShotUnderFlipper, eShotLeftLoop)
			Else
				GetAdjacentShots = Array(eShotEarth, eShotRightRamp, eShotNone, eShotNone)
			End If
		Case Else
			GetAdjacentShots = Array(eShotNone, eShotNone, eShotNone, eShotNone)
	End Select
End Function

' *** turntable animation

Dim thetaOffset
Dim aTurningTargets
Dim aHiddenTargets1
Dim aHiddenTargets2
Dim nTargetAngle
Dim bTableActive
Const cMagnetR = 60.28498983992619
Const cMagnetTheta = -1.1026977086123524
Const cPostR = 72.470683727974
Const CPostTheta = 3.62992660464620
Const cTableCenterY = 291
Const cTableCenterX = 656
Const revTime = 2 ' 1 rev takes 2 s
Dim nPi
nPi = 4 * Atn(1)

Sub TimerTurntable_Timer
	If bTurnClockwise Then
		nTargetAngle = 78
		WallTurn1.isDropped = True
		WallTurn1Guard.isDropped = True
		WallTurn2.isDropped = False
		PostTurn1.isDropped = True
		PostTurn2.isDropped = False
	Else
		nTargetAngle = 0
		WallTurn1.isDropped = False
		WallTurn1Guard.isDropped = False
		WallTurn2.isDropped = True
		PostTurn1.isDropped = False
		PostTurn2.isDropped = True
	End If
	bTableActive = True
	PlaySound "BookcaseMotor",-1,1 * fMechSoundVolume,AudioPan(Primitive008),0,0,1,0,AudioFade(Primitive008)
	nLastTurntableUpdate = GameTime
End Sub

Sub TimerAnimSpin_Init
	thetaOffset = 0
	bTableActive = False
	StopSound "BookcaseMotor"
	aTurningTargets = Array(Primitive18, Primitive008, Primitive27, Primitive28)
End Sub

Sub TimerAnimSpin_Timer
	Dim i
	If Not bTableActive then Exit Sub
'	sTimersRun = sTimersRun + " TimerAnimSpin"
	Dim msPassed
	Dim thetaOffsetRadians
	msPassed = GameTime - nLastTurntableUpdate
	nLastTurntableUpdate = GameTime
	If bTurnClockwise Then
		thetaOffset = thetaOffset + (360 * ((msPassed / 1000) / revTime))
		If thetaOffset > nTargetAngle Then
			thetaOffset = nTargetAngle
			bTableActive = False
			StopSound "BookcaseMotor"
			bTurnClockwise = False
		End If
		for i = 0 to 3
			aTurningTargets(i).RotZ = -thetaOffset
		next
		thetaOffsetRadians = thetaOffset * (nPi / 180)
		TriggerMagnet.x = cTableCenterX - (cMagnetR * cos(cMagnetTheta + thetaOffsetRadians))
		TriggerMagnet.y = cTableCenterY - (cMagnetR * sin(cMagnetTheta + thetaOffsetRadians))
		TurntablePin.x = cTableCenterX - (cPostR * cos(cPostTheta + thetaOffsetRadians))
		TurntablePin.y = cTableCenterY - (cPostR * sin(cPostTheta + thetaOffsetRadians))
		TurntablePost.x = TurntablePin.x
		TurntablePost.y = TurntablePin.y
		TriggerMagnet.RotY = thetaOffset
		EarthTurntable.RotY = thetaOffset
		oMagnetA.x = TriggerMagnet.x
		oMagnetA.y = TriggerMagnet.y
	Else
		thetaOffset = thetaOffset - (360 * ((msPassed / 1000) / revTime))
		If thetaOffset < nTargetAngle Then
			thetaOffset = nTargetAngle
			bTableActive = False
			StopSound "BookcaseMotor"
			bTurnClockwise = True
		End If
		for i = 0 to 3
			aTurningTargets(i).RotZ = -thetaOffset
		next
		thetaOffsetRadians = thetaOffset * (nPi / 180)
		TriggerMagnet.x = cTableCenterX - (cMagnetR * cos(cMagnetTheta + thetaOffsetRadians))
		TriggerMagnet.y = cTableCenterY - (cMagnetR * sin(cMagnetTheta + thetaOffsetRadians))
		TurntablePin.x = cTableCenterX - (cPostR * cos(cPostTheta + thetaOffsetRadians))
		TurntablePin.y = cTableCenterY - (cPostR * sin(cPostTheta + thetaOffsetRadians))
		TurntablePost.x = TurntablePin.x
		TurntablePost.y = TurntablePin.y
		TriggerMagnet.RotY = thetaOffset
		EarthTurntable.RotY = thetaOffset
		oMagnetA.x = TriggerMagnet.x
		oMagnetA.y = TriggerMagnet.y
	End If
End Sub

Function GetStatusText(nMode, nPage)
	Dim nJPsLeft, i, j
	Select Case nMode
		Case eModeCyber1
			If 0 = nPage Then
				GetStatusText = "INFO HIGHWAY"
			Elseif 1 = nPage Then
				If aanModeShots(nPlayer, eModeCyber1) Mod 2 = 0 Then
					GetStatusText = "HIT LIT TARGETS"
				Else
					GetStatusText = "HIT GREEN SHOTS"
				End If
			Elseif avModesPlayed(nPlayer).Contains(eModeCyber2) Then
				nJPsLeft = 10 - aanModeShots(nPlayer, eModeCyber1)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			Else
				nJPsLeft = 6 - aanModeShots(nPlayer, eModeCyber1)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			End If
		Case eModeCyber2
			If 0 = nPage Then
				GetStatusText = "BEWARE GLITCHES"
			Elseif 1 = nPage Then
				GetStatusText = "HIT GREEN SHOTS"
			Elseif avModesPlayed(nPlayer).Contains(eModeCyber1) Then
				nJPsLeft = 10 - aanModeShots(nPlayer, eModeCyber2)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			Else
				nJPsLeft = 6 - aanModeShots(nPlayer, eModeCyber2)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			End If
		Case eModeCyberWizard
			If 0 = nPage Then
				GetStatusText = "3 IN A LINE"
			Elseif 1 = nPage Then
				GetStatusText = "TO HIT VILLAIN"
			Elseif 2 = nPage Then
				GetStatusText = "VILLAIN HP " & (4 - aanModeShots(nPlayer, eModeCyberWizard)) & "/4"
			Else
				GetStatusText = "ATTACK IN " & (nCyber3AttackTime \ 1000) & " SEC"
			End If
		Case eModeCyberMBall
			If 0 = nPage Then
				GetStatusText = "CYBER MULTIBALL"
			Elseif 1 = nPage Then
				GetStatusText = "HIT GREEN SHOTS"
			Elseif vSuperJPsLit.Contains(eMballCyber) Then
				GetStatusText = "CYBER SJ IS LIT"
			Else
				nJPsLeft = anJackpotsForSuper(eMballCyber) - aanMballJackpots(nPlayer, eMballCyber)
				If nJPsLeft < 0 Then nJPsLeft = 0
				GetStatusText = nJPsLeft & " TO CYBER SJ"
			End If
		Case eModeWater1
			If 0 = nPage Then
				GetStatusText = "SHARK ATTACK"
			Elseif 1 = nPage Then
				GetStatusText = "HIT BLUE SHOTS"
			Elseif avModesPlayed(nPlayer).Contains(eModeWater2) Then
				nJPsLeft = 10 - aanModeShots(nPlayer, eModeWater1)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			Else
				nJPsLeft = 6 - aanModeShots(nPlayer, eModeWater1)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			End If
		Case eModeWater2
			If 0 = nPage Then
				GetStatusText = "SUBMARINE LEAK"
			Elseif 1 = nPage Then
				GetStatusText = "HIT BLUE SHOTS"
			Elseif avModesPlayed(nPlayer).Contains(eModeWater1) Then
				nJPsLeft = 10 - aanModeShots(nPlayer, eModeWater2)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			Else
				nJPsLeft = 6 - aanModeShots(nPlayer, eModeWater2)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			End If
		Case eModeWaterWizard
			Dim nPageCount : nPageCount = 1
			Dim nTypeLastShown : nTypeLastShown = -1
			If 1 = nPage Then
				GetStatusText = "W=" & aanWater3TypesLeft(nPlayer, cTypeLoops) _
					& " G=" & aanWater3TypesLeft(nPlayer, cTypeRamps) _
					& " B=" & aanWater3TypesLeft(nPlayer, cTypeKickers) _
					& " T=" & aanWater3TypesLeft(nPlayer, cTypeTargets)
			Else
				For i = 0 to 3
					if aanWater3TypesLeft(nPlayer, i) > 0 Then nPageCount = nPageCount + 1
				Next
				For i = 1 to nPageCount - 1
					If nTimeDisplay / 4000 < i / nPageCount Then
						If aanWater3TypesLeft(nPlayer, cTypeLoops) > 0 And nTypeLastShown < cTypeLoops Then
							GetStatusText = aanWater3TypesLeft(nPlayer, cTypeLoops) & " WHITE LEFT"
							nTypeLastShown = cTypeLoops
						ElseIf aanWater3TypesLeft(nPlayer, cTypeRamps) > 0 And nTypeLastShown < cTypeRamps Then
							GetStatusText = aanWater3TypesLeft(nPlayer, cTypeRamps) & " GREEN LEFT"
							nTypeLastShown = cTypeRamps
						ElseIf aanWater3TypesLeft(nPlayer, cTypeKickers) > 0 And nTypeLastShown < cTypeKickers Then
							GetStatusText = aanWater3TypesLeft(nPlayer, cTypeKickers) & " BLUE LEFT"
							nTypeLastShown = cTypeKickers
						ElseIf aanWater3TypesLeft(nPlayer, cTypeTargets) > 0 And nTypeLastShown < cTypeTargets Then
							GetStatusText = aanWater3TypesLeft(nPlayer, cTypeTargets) & " TARGETS LEFT"
							nTypeLastShown = cTypeTargets
						End If
					End If
				Next
				If nTypeLastShown = -1 Then
					GetStatusText = "VILLAIN HP " _
						& (3 - aanModeShots(nPlayer, eModeWaterWizard)) & "/3"
				End If
			End If
		Case eModeWaterMBall
			If 0 = nPage Then
				GetStatusText = "WATER MULTIBALL"
			Elseif 1 = nPage Then
				GetStatusText = "HIT BLUE SHOTS"
			Elseif vSuperJPsLit.Contains(eMBallWater) Then
				GetStatusText = "WATER SJ IS LIT"
			Else
				nJPsLeft = anJackpotsForSuper(eMBallWater) - aanMballJackpots(nPlayer, eMBallWater)
				If nJPsLeft < 0 Then nJPsLeft = 0
				GetStatusText = nJPsLeft & " TO WATER SJ"
			End If
		Case eModeFire1
			If 0 = nPage Then
				GetStatusText = "FLOOR IS LAVA"
			Elseif 1 = nPage Then
				GetStatusText = "HIT RED SHOTS"
			Elseif avModesPlayed(nPlayer).Contains(eModeFire2) Then
				nJPsLeft = 10 - aanModeShots(nPlayer, eModeFire1)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			Else
				nJPsLeft = 6 - aanModeShots(nPlayer, eModeFire1)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			End If
		Case eModeFire2
			if avModesPlayed(nPlayer).Contains(eModeFire1) Then
				nJPsLeft = 200 - aanModeShots(nPlayer, eModeFire2)
			Else
				nJPsLeft = 125 - aanModeShots(nPlayer, eModeFire2)
			End If
			If 0 = nPage Then
				GetStatusText = "FLYING EMBERS"
			Elseif 1 = nPage Then
				GetStatusText = "HIT " & nJPsLeft & " SWITCHES"
			Else
				GetStatusText = nJPsLeft & " TO WIN"
			End If
		Case eModeFireWizard
			If 0 = nPage or 2 = nPage Then
				Select Case aanModeShots(nPlayer, eModeFireWizard)
					Case 0
						GetStatusText = "2 MORE WHITE"
					Case 1, 4, 8
						GetStatusText = "1 MORE WHITE"
					Case 2, 3
						GetStatusText = "BLUE SHOT 1X"
					Case Else
						GetStatusText = "BLUE=1X GREEN=2X"
				End Select
			Else
				Select Case aanModeShots(nPlayer, eModeFireWizard)
					Case 0
						GetStatusText = "SHOTS TO GO"
					Case 1, 4, 8
						GetStatusText = "SHOT TO GO"
					Case 2, 3
						GetStatusText = "GREEN SHOT 2X"
					Case 5, 6, 7
						GetStatusText = "YELLOW SHOT 3X"
					Case Else
						GetStatusText = "YELLOW=3X RED=4X"
				End Select
			End If
		Case eModeFireMBall
			If 0 = nPage Then
				GetStatusText = "FIRE MULTIBALL"
			Elseif 1 = nPage Then
				GetStatusText = "HIT RED SHOTS"
			Elseif vSuperJPsLit.Contains(eMballFire) Then
				GetStatusText = "FIRE SJ IS LIT"
			Else
				nJPsLeft = 0
				For Each i in Array(eShotLeftLoop, eShotSaucer, eShotLeftRamp, eShotEarth, _
					eShotRightRamp, eShotRightLoop)
					If anFireSpreadTimer(i) > 0 Then nJPsLeft = nJPsLeft + 1 
				Next
				GetStatusText = nJPsLeft & " TO FIRE SJ"
			End If
		Case eModeEarth1
			If 0 = nPage Then
				GetStatusText = "NOT A DRILL"
			Elseif 1 = nPage Then
				GetStatusText = "HIT ORANGE SHOTS"
			Elseif avModesPlayed(nPlayer).Contains(eModeEarth2) Then
				nJPsLeft = 10 - aanModeShots(nPlayer, eModeEarth1)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			Else
				nJPsLeft = 6 - aanModeShots(nPlayer, eModeEarth1)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			End If
		Case eModeEarth2
			If 0 = nPage Then
				GetStatusText = "LIGHTS OUT"
			Elseif 1 = nPage Then
				GetStatusText = "TARGETS ADD SHOT"
			Elseif avModesPlayed(nPlayer).Contains(eModeEarth1) Then
				nJPsLeft = 10 - aanModeShots(nPlayer, eModeEarth2)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			Else
				nJPsLeft = 6 - aanModeShots(nPlayer, eModeEarth2)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			End If
		Case eModeEarthWizard
			Dim nTime, nShotCount, nShotsLeft
			If 0 = nPage or 2 = nPage Then
				If nTimeEarth3Open > 0 Then
					GetStatusText = "HIT THE PASSAGE"
				Else
					nShotCount = (8 - aanModeShots(nPlayer, eModeEarthWizard)) _
								  - aavShotsLit(nPlayer, eModeEarthWizard).Count
					Select Case anEarth3Misses(nPlayer)
						Case 0
							nShotsLeft = 4 - nShotCount
						Case 1, 2
							nShotsLeft = 2 - nShotCount
						Case Else
							nShotsLeft = 1 - nShotCount
					End Select
					If 1 = nShotsLeft Then
						GetStatusText = "1 MORE SHOT"
					Else
						GetStatusText = nShotsLeft & " MORE SHOTS"
					End If
				End If
			Else
				If nTimeEarth3Open > 0 Then
					If nTimeEarth3Open < 2500 Then
						nTime = 0
					Else
						nTime = cInt((nTimeEarth3Open - 2500) / 1000)
					End If
					If 1 = nTime Then
						GetStatusText = "1 SECOND LEFT"
					Else
						GetStatusText = nTime & " SECONDS LEFT"
					End If
				Else
					If 3 = nPage Then
						GetStatusText = "TO OPEN PASSAGE"
					Else
						GetStatusText = "VILLAIN HP " _
							& (3 - aanModeShots(nPlayer, eModeEarthWizard)) & "/3"
					End If
				End If
			End If
		Case eModeEarthMBall
			If 0 = nPage Then
				GetStatusText = "EARTH MULTIBALL"
			Elseif 1 = nPage Then
				GetStatusText = "HIT ORANGE SHOTS"
			Elseif vSuperJPsLit.Contains(eMballEarth) Then
				GetStatusText = "EARTH SJ IS LIT"
			Else
				nJPsLeft = anJackpotsForSuper(eMballEarth) - aanMballJackpots(nPlayer, eMballEarth)
				If nJPsLeft < 0 Then nJPsLeft = 0
				GetStatusText = nJPsLeft & " TO EARTH SJ"
			End If
		Case eModeAir1
			If 0 = nPage Then
				GetStatusText = "DOG FIGHTING"
			Elseif 1 = nPage Then
				GetStatusText = "HIT PURPLE SHOTS"
			Elseif avModesPlayed(nPlayer).Contains(eModeAir2) Then
				nJPsLeft = 10 - aanModeShots(nPlayer, eModeAir1)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			Else
				nJPsLeft = 6 - aanModeShots(nPlayer, eModeAir1)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			End If
		Case eModeAir2
			If 0 = nPage Then
				GetStatusText = "NO PARACHUTE"
			Elseif 1 = nPage Then
				GetStatusText = "HIT PURPLE SHOTS"
			Elseif avModesPlayed(nPlayer).Contains(eModeAir1) Then
				nJPsLeft = 10 - aanModeShots(nPlayer, eModeAir2)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			Else
				nJPsLeft = 6 - aanModeShots(nPlayer, eModeAir2)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			End If
		Case eModeAirWizard
			If 0 = nPage Then
				GetStatusText = "COMBOS & PURPLE"
			Elseif 1 = nPage Then
				GetStatusText = "DO MORE DAMAGE"
			Else
				GetStatusText = "VILLAIN HP " & (100 - aanModeShots(nPlayer, eModeAirWizard))
			End If
		Case eModeAirMBall
			If 0 = nPage Then
				GetStatusText = "AIR MULTIBALL"
			Elseif 1 = nPage Then
				GetStatusText = "HIT PURPLE SHOTS"
			Elseif vSuperJPsLit.Contains(eMballAir) Then
				GetStatusText = "AIR SUPER IS LIT"
			Else
				nJPsLeft = anJackpotsForSuper(eMballAir) - aanMballJackpots(nPlayer, eMballAir)
				If nJPsLeft < 0 Then nJPsLeft = 0
				GetStatusText = nJPsLeft & " TO AIR SUPER"
			End If
		Case eModeIce1
			If 0 = nPage Then
				GetStatusText = "GONDOLA LIFT"
			Elseif 1 = nPage Then
				GetStatusText = "HIT WHITE SHOTS"
			Elseif avModesPlayed(nPlayer).Contains(eModeIce2) Then
				nJPsLeft = 10 - aanModeShots(nPlayer, eModeIce1)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			Else
				nJPsLeft = 6 - aanModeShots(nPlayer, eModeIce1)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			End If
		Case eModeIce2
			If 0 = nPage Then
				GetStatusText = "AVALANCHE"
			Elseif 1 = nPage Then
				GetStatusText = "HIT WHITE SHOTS"
			Elseif avModesPlayed(nPlayer).Contains(eModeIce1) Then
				nJPsLeft = 10 - aanModeShots(nPlayer, eModeIce2)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			Else
				nJPsLeft = 6 - aanModeShots(nPlayer, eModeIce2)
				GetStatusText = nJPsLeft & " TO WIN MODE"
			End If
		Case eModeIceWizard
			If 1 = nPage Then
				GetStatusText = "VILLAIN HP " & (4 - aanModeShots(nPlayer, eModeIceWizard)) & "/4"
			Else
				If anIce3SwitchesLeft(nPlayer) > 1 Then
					GetStatusText = anIce3SwitchesLeft(nPlayer) & " SWITCHES LEFT"
				ElseIf anIce3SwitchesLeft(nPlayer) > 0 Then
					GetStatusText = "1 SWITCH LEFT"
				Else
					GetStatusText = "HIT WHITE SHOTS"
				End If
			End If
		Case eModeIceMBall
			If 0 = nPage Then
				GetStatusText = "ICE MULTIBALL"
			Elseif 1 = nPage Then
				GetStatusText = "HIT WHITE SHOTS"
			Elseif vSuperJPsLit.Contains(eMballIce) Then
				GetStatusText = "ICE SUPER IS LIT"
			Else
				nJPsLeft = anJackpotsForSuper(eMballIce) - aanMballJackpots(nPlayer, eMballIce)
				If nJPsLeft < 0 Then nJPsLeft = 0
				GetStatusText = nJPsLeft & " TO ICE SUPER"
			End If
		Case eModeTotalMayhem
			If 0 = nPage or 2 = nPage Then
				nJPsLeft = CInt((nTimeMayhem - 2500) / 1000)
				if nJPsLeft < 0 Then nJPsLeft = 0
				If bDrainingBalls then
					GetStatusText = "TOTAL MAYHEM"
				elseif 1 = nJPsLeft then
					GetStatusText = "1 SECOND LEFT"
				else
					GetStatusText = nJPsLeft & " SECONDS LEFT"
				end if
			Elseif 1 = nPage then
				If bDrainingBalls then
					GetStatusText = "SCORE " & WilliamsFormatNum(anModeScore(eModeTotalMayhem))
				Else
					GetStatusText = "HIT ALL LIGHTS"
				End If
			Else
				If abModeExtraBall(nPlayer) Then
					GetStatusText = "FOR " & WilliamsFormatNum(100000)
				Else
					GetStatusText = "TO LIGHT X-BALL"
				End If
			End If
		Case eModeEvilUnited
			If 0 = nPage Then
				GetStatusText = "HIT SHOT 3 TIMES"
			Elseif 1 = nPage Then
				GetStatusText = "FOR SUPER JP LIT"
			Elseif 1 = vSuperJPsLit.Count Then
				GetStatusText = "1 SUPER JP LIT"
			Else
				GetStatusText = vSuperJPsLit.Count & " SUPER JPS LIT"
			End If
	End Select
End Function

Sub AnimateBagOfTricks
	dim i, sLineFirst, sLineSecond, nBagAnimFrame, nBagAnimTicks, bTick, sChar

	nTimeBagAnimation = nTimeBagAnimation - TimerDisplay.Interval
	If nTimeBagAnimation < 1 Then
		nTimeBagAnimation = 0
		Exit Sub
	End if

	nBagAnimFrame = (3000 - nTimeBagAnimation) \ TimerDisplay.Interval
	If nBagAnimFrame <= 13 then
		bTick = True
		nBagAnimTicks = nBagAnimFrame
	Elseif nBagAnimFrame < 15 Then
		bTick = False
		nBagAnimTicks = 14
	Elseif nBagAnimFrame = 15 Then
		bTick = True
		nBagAnimTicks = 14
	Elseif nBagAnimFrame < 18 Then
		bTick = False
		nBagAnimTicks = 15
	Elseif nBagAnimFrame = 18 Then
		bTick = True
		nBagAnimTicks = 15
	Elseif nBagAnimFrame < 21 Then
		btick = False
		nBagAnimTicks = 16
	Elseif nBagAnimFrame = 21 Then
		bTick = True
		nBagAnimTicks = 16
	Elseif nBagAnimFrame < 27 Then
		bTick = False
		nBagAnimTicks = 17
	Elseif nBagAnimFrame = 27 Then
		bTick = True
		nBagAnimTicks = 17
	Elseif nBagAnimFrame < 33 Then
		bTick = False
		nBagAnimTicks = 18
	Elseif nBagAnimFrame = 33 Then
		bTick = True
		nBagAnimTicks = 18
	Elseif nBagAnimFrame < 40 Then
		bTick = False
		nBagAnimTicks = 19
	Elseif nBagAnimFrame = 40 Then
		bTick = True
		nBagAnimTicks = 19
	Elseif nBagAnimFrame < 48 Then
		bTick = False
		nBagAnimTicks = 20
	Elseif nBagAnimFrame = 48 Then
		bTick = True
		nBagAnimTicks = 20
	Else
		' Kickout warning
		If 74 = nBagAnimFrame or 66 = nBagAnimFrame Or 58 = nBagAnimFrame Then
			FlashOnce 1, eColorWhite
		End If
		bTick = False
		nBagAnimTicks = 21
	End If
	If bTick Then
		sSFXToPlay = "SFX_tick"
		nSFXPriority = eVOPrioMystery
		fSFXQueuedVolume = 1
	End If

	sLineFirst = ""
	sLineSecond = ""
	sChar = Mid("-\|/", 1 + (nBagAnimTicks Mod 4), 1)
	If nBagAnimTicks < 21 Then
		For i = 1 to 16
			If (i * 7) mod 16 < (21 - nBagAnimTicks) Then
				sLineSecond = sLineSecond & sChar
			Else
				sLineSecond = sLineSecond & Mid(asBagAward(1), i, 1)
			End If
		Next
		If Left(LTrim(asBagAward(0)), 13) <> "BAG OF TRICKS" Then
			For i = 1 to 16
				If (i * 11) mod 16 < (21 - nBagAnimTicks) Then
					sLineFirst = sLineFirst & sChar
				Else
					sLineFirst = sLineFirst & Mid(asBagAward(0), i, 1)
				End If
			Next
		Else
			sLineFirst = asBagAward(0)
		End If
	Else
		sLineFirst = asBagAward(0)
		sLineSecond = asBagAward(1)
	End If

	ShowText sLineFirst, sLineSecond, TimerDisplay.interval - 1, eVOPrioMystery
End Sub

Dim nTimeSuitcaseUpdate : nTimeSuitcaseUpdate = 0
const nTimeSuitcaseMove = 1000 ' ms between suitcase opening and closing

Sub AnimateSuitcase
	Dim dAngle
	If nTimeBagAnimation > 0 And Suitcasetop.RotZ > 10 Then
		' 80 degrees between open and closed states
		dAngle = Suitcasetop.RotZ - (80 * (FrameTime / nTimeSuitcaseMove))
		If dAngle < 10 then dAngle = 10
		Suitcasetop.RotZ = dAngle
	Elseif 0 = nTimeBagAnimation And Suitcasetop.RotZ < 90 Then
		' 80 degrees between open and closed states
		dAngle = Suitcasetop.RotZ + (80 * (FrameTime / nTimeSuitcaseMove))
		If dAngle > 90 then dAngle = 90
		Suitcasetop.RotZ = dAngle
	End If
	nTimeSuitcaseUpdate = GameTime
End Sub

Sub UpdateLEDs(ByVal nNumber)
	Dim i
	Dim nFirstDigit, nSecondDigit, nThirdDigit
	Dim bSegment1, bSegment2, bSegment3

	If nNumber < 0 Then
		For each i in aoDigit0Lights : i.state = LightStateOff : Next
		For each i in aoDigit1Lights : i.state = LightStateOff : Next
		For each i in aoDigit2Lights : i.state = LightStateOff : Next
	Else
		nNumber = Abs(nNumber)
		nFirstDigit = nNumber \ 100
		nSecondDigit = (nNumber \ 10) Mod 10
		nThirdDigit = nNumber Mod 10

		For i = 0 to 6
			bSegment1 = aPowersOfTwo(i) And anDigitSegments(nFirstDigit)
			bSegment2 = aPowersOfTwo(i) And anDigitSegments(nSecondDigit)
			bSegment3 = aPowersOfTwo(i) And anDigitSegments(nThirdDigit)
			If bSegment1 = 0 Then
				aoDigit0Lights(i).state = LightStateOff
			Else
				aoDigit0Lights(i).state = LightStateOn
			End If
			If bSegment2 = 0 Then
				aoDigit1Lights(i).state = LightStateOff
			Else
				aoDigit1Lights(i).state = LightStateOn
			End If
			If bSegment3 = 0 Then
				aoDigit2Lights(i).state = LightStateOff
			Else
				aoDigit2Lights(i).state = LightStateOn
			End If
		Next
		If nNumber < 10 Then
			For Each i In aoDigit1Lights
				i.state = LightStateOff
			Next
		End If
		If nNumber < 100 Then
			For Each i In aoDigit0Lights
				i.state = LightStateOff
			Next
		End If
		End If
End Sub

Sub InitDisplay
	Dim i
	If B2SOn or bEnableFlexDMD Then
		For i = 0 to 127 : aSegments(i) = 0 : Next
		aSegments(Asc("A")) = &h0877
		aSegments(Asc("B")) = &h2a0f
		aSegments(Asc("C")) = &h0039
		aSegments(Asc("D")) = &h220f
		aSegments(Asc("E")) = &h0879
		aSegments(Asc("F")) = &h0871
		aSegments(Asc("G")) = &h083d
		aSegments(Asc("H")) = &h0876
		aSegments(Asc("I")) = &h2209
		aSegments(Asc("J")) = &h001e
		aSegments(Asc("K")) = &h1470
		aSegments(Asc("L")) = &h0038
		aSegments(Asc("M")) = &h0536
		aSegments(Asc("N")) = &h1136
		aSegments(Asc("O")) = &h003f
		aSegments(Asc("P")) = &h0873
		aSegments(Asc("Q")) = &h103f
		aSegments(Asc("R")) = &h1873
		aSegments(Asc("S")) = &h086d
		aSegments(Asc("T")) = &h2201
		aSegments(Asc("U")) = &h003e
		aSegments(Asc("V")) = &h4430
		aSegments(Asc("W")) = &h5036
		aSegments(Asc("X")) = &h5500
		aSegments(Asc("Y")) = &h2500
		aSegments(Asc("Z")) = &h4409
		aSegments(Asc("0")) = &h003f
		aSegments(Asc("1")) = 6
		aSegments(Asc("2")) = &h085b
		aSegments(Asc("3")) = &h084f
		aSegments(Asc("4")) = &h0866
		aSegments(Asc("5")) = &h086d
		aSegments(Asc("6")) = &h087d
		aSegments(Asc("7")) = 7
		aSegments(Asc("8")) = &h087f
		aSegments(Asc("9")) = &h086f
		aSegments(39) = &h0400 ' apostrophe
		aSegments(34) = &h0202 ' double quote
		aSegments(Asc("&")) = &h135d
		aSegments(Asc("<")) = &h1400
		aSegments(Asc(">")) = &h4100
		aSegments(Asc("^")) = &h4406
		aSegments(Asc("*")) = &h7f40
		aSegments(Asc(".")) = &h0080
		aSegments(Asc("/")) = &h4400
		aSegments(Asc("\")) = &h1100
		aSegments(Asc("|")) = &h2200
		aSegments(Asc("+")) = &h2a40
		aSegments(Asc("-")) = &h0840
		aSegments(Asc("=")) = &h0848
		aSegments(Asc("_")) = 8
		aSegments(96) = &h00bf ' backtick, maps to "0,"
		aSegments(Asc("a")) = &h0086 '1,
		aSegments(Asc("b")) = &h08db '2,
		aSegments(Asc("c")) = &h08cf '3,
		aSegments(Asc("d")) = &h08e6 '4,
		aSegments(Asc("e")) = &h08ed	'5,
		aSegments(Asc("f")) = &h08fd	'6,
		aSegments(Asc("g")) = &h0087	'7,
		aSegments(Asc("h")) = &h08ff	'8,
		aSegments(Asc("i")) = &h08ef	'9,
		aSegments(Asc("j")) = &h0030 ' 20% progress
		aSegments(Asc("k")) = &h4170 ' 40% progress
		aSegments(Asc("l")) = &h6379 ' 60% progress
		aSegments(Asc("m")) = &h7779 ' 80% progress
		aSegments(Asc("n")) = &h7f7f ' 100% progress
	End If

	aoTopRowDigits = Array(digit001, digit002, digit003, digit004, digit005, digit006, _
		digit007, digit008, digit009, digit010, digit011, digit012, digit013, _
		digit014, digit015, digit016)
	aoBottomRowDigits = Array(digit017, digit018, digit019, digit020, digit021, _
		digit022, digit023, digit024, digit025, digit026, digit027, digit028, _
		digit029, digit030, digit031, digit032)
	If 0 = VRRoom Then
		For Each i in aoTopRowDigits
			i.visible = False
		Next
		For Each i in aoBottomRowDigits
			i.visible = False
		Next
		dmd_back.visible = False
	End If
'	UpdateDisplay "THE QUICK BROWN", "FOX JUMPS OVER"
End Sub

Sub ShowText(sTopRow, sBottomRow, nDuration, nPriority)
	if nPriority <= nTextPriority or nTextDuration = 0 then
		sTextTop = sTopRow
		sTextBottom = sBottomRow
		nTextDuration = nDuration
		nTextPriority = nPriority
	end if
End Sub

Sub QueueText(sTopRow, sBottomRow, nDuration)
	oQueueTextTop.Enqueue sTopRow
	oQueueTextBottom.Enqueue sBottomRow
	oQueueTextDuration.Enqueue nDuration
End Sub

Sub QueueTextFirst(sTopRow, sBottomRow, nDuration)
	oQueueTextTop.PushFirst sTopRow
	oQueueTextBottom.PushFirst sBottomRow
	oQueueTextDuration.PushFirst nDuration
End Sub

Sub ClearTextQueue
	oQueueTextTop.Clear
	oQueueTextBottom.Clear
	oQueueTextDuration.Clear
End Sub

Sub QueueStatus(sTopRow, sBottomRow, nDuration)
	oQueueStatusTop.Enqueue sTopRow
	oQueueStatusBottom.Enqueue sBottomRow
	oQueueStatusDuration.Enqueue nDuration
End Sub

Sub ClearStatus
	oQueueStatusTop.Clear
	oQueueStatusBottom.Clear
	oQueueStatusDuration.Clear
End Sub

Sub TimerDisplay_Timer
	dim sTopRow
	dim sBotRow
	dim sScore
	dim sBallText
	dim sReplacedTop, sReplacedBottom
	dim aInfo
	dim i, temp
	
'	sTimersRun = sTimersRun + " TimerDisplay"
	nTimeDisplay = nTimeDisplay + TimerDisplay.interval
	If nTimeBagAnimation > 0 Then AnimateBagOfTricks
	
	if nTextDuration <> 0 then
		UpdateDisplay CenterTextOnDisplay(sTextTop), CenterTextOnDisplay(sTextBottom)
		if nTextDuration <> PlayUntilHit then
			nTextDuration = nTextDuration - TimerDisplay.interval
			If nTextDuration < 0 Then nTextDuration = 0
		End If
		Exit Sub
	Elseif nPlayersInGame > 0 Then
		' Instant info
		If nTimeUpperLeftFlip = -1 And nTimeLowerRightFlip = -1 Then
			bShowingInstantInfo = False
		End If

		If nPlayersInGame > 0 And nTimeLastSwitch > 0 And (GameTime - nTimeLastSwitch > 2000) Then
			If ((nTimeUpperLeftFlip > 0) And (GameTime - nTimeUpperLeftFlip > 2000)) Or _
			  ((nTimeLowerRightFlip > 0) And (GameTime - nTimeLowerRightFlip > 2000)) Then
				If bShowingInstantInfo = False Then nTimeDisplay = 0
				bShowingInstantInfo = True
				If nTimeDisplay > 2000 Then
					nTimeDisplay = 0
					nInstantInfoPage = (nInstantInfoPage + 1)
					If nInstantInfoPage > cLastInfoPage Then
						nInstantInfoPage = 0
					End If
				End If
				If nInstantInfoPage > 0 Then
					aInfo = GetInstantInfoPage(nInstantInfoPage)
					UpdateDisplay CenterTextOnDisplay(aInfo(0)),  CenterTextOnDisplay(aInfo(1))
					Exit Sub
				End If
			End If
		End If

		sTopRow = "" : sBotRow = ""
		If bDrainingBalls and nTiltWarnings > 2 Then
			if nTimeDisplay < 1251 then
				sTopRow = Space(16)
				sBotRow = CenterTextOnDisplay("TILT")
			elseif nTimeDisplay < 2501 then
				sTopRow = CenterTextOnDisplay("TILT")
				sBotRow = Space(16)
			else
				nTimeDisplay = 0
			end if
			UpdateDisplay sTopRow, sBotRow
			Exit Sub
		End If

		If avModesRunning(nPlayer).Contains(eModeMegaWizard) And (Not bDrainingBalls) Then
			bTopSecretUpdate = True
			If 0 = oQueueSecretTop.Size Then
				sTopRow = "SCORE 2M TO WIN"
				sBotRow = WilliamsFormatNum(anModeScore(eModeMegaWizard)) _
					& "/" & WilliamsFormatNum(2000000)
				UpdateDisplay CenterTextOnDisplay(sTopRow), CenterTextOnDisplay(sBotRow)
				nTimeTopSecretText = 0
				nTimeDisplay = 0
			Elseif nTimeDisplay >= nTimeTopSecretText Then
				nTimeDisplay = 0
				sTopRow = oQueueSecretTop.Dequeue
				sBotRow = oQueueSecretBottom.Dequeue
				temp = oQueueSecretValue.Dequeue
				If "" = sBotRow Then
					If temp >= 100000 Then temp = Cint(temp / 1000) & "K"
					sBotRow = "+" & temp & " " _
						& WilliamsFormatNum(Cint(anModeScore(eModeMegaWizard) / 1000)) _
						& "K/2M"
				End If
				UpdateDisplay CenterTextOnDisplay(sTopRow), CenterTextOnDisplay(sBotRow)
				Select Case nTopSecretFlash Mod 6
					Case 0
						FlashOnce 1, eColorGreen : FlashOnce 3, eColorGreen : FlashOnce 5, eColorGreen
					Case 1
						FlashOnce 2, eColorBlue : FlashOnce 4, eColorBlue
					Case 2
						FlashOnce 1, eColorRed : FlashOnce 3, eColorRed : FlashOnce 5, eColorRed
					Case 3
						FlashOnce 2, eColorYellow : FlashOnce 4, eColorYellow
					Case 4
						FlashOnce 1, eColorPurple : FlashOnce 3, eColorPurple : FlashOnce 5, eColorPurple
					Case Else
						FlashOnce 2, eColorWhite : FlashOnce 4, eColorWhite
				End Select
				nTopSecretFlash = nTopSecretFlash + 1
				Select Case nTopSecretSound Mod 3
					Case 0
						PlaySound "Chime_10"
					Case 1
						PlaySound "Chime_100"
					Case Else
						PlaySound "Chime_1000"
				End Select
				If 0 = oQueueSecretTop.Size Then
					oQueueSecretTop.Clear
					oQueueSecretBottom.Clear
					oQueueSecretValue.Clear
					nTopSecretSound = nTopSecretSound + 1
				Elseif oQueueSecretTop.Size < 3 Then
					nTimeTopSecretText = 500
				Elseif oQueueSecretTop.Size > 4 Then
					nTimeTopSecretText = 200
				Else
					nTimeTopSecretText = cInt(1000 / oQueueSecretTop.Size)
				End If
			End If
			Exit Sub
		ElseIf Not bShowingInstantInfo And (avModesRunning(nPlayer).Count > 0) Then
			For i = nModeStatusShowing to nModeStatusShowing + 28
				If avModesRunning(nPlayer).Contains(i mod 28) Then
					nModeStatusShowing = i mod 28
					If nTimeDisplay < 2000 Then
						sTopRow = GetStatusText(i mod 28, 0)
						sBotRow = GetStatusText(i mod 28, 1)
						UpdateDisplay CenterTextOnDisplay(sTopRow), CenterTextOnDisplay(sBotRow)
						Exit Sub
					Elseif nTimeDisplay < 4000 Then
						sTopRow = GetStatusText(i mod 28, 2)
						If eModeCyberWizard = (i mod 28) or eModeEarthWizard = (i mod 28) _
						or eModeTotalMayhem = (i mod 28) Then
							sBotRow = GetStatusText(i mod 28, 3)
						ElseIf eModeWaterWizard = (i mod 28) Then
							sBotRow = GetStatusText(i mod 28, 1)
						Else
							sBotRow = sTopRow
							sScore = getFormattedScore(nPlayer)
							If nPlayersInGame < 2 Then
								sTopRow = sScore & Space(10 - len(sScore)) & "BALL " & nBall
							Else
								sTopRow = sScore & Space(11 - len(sScore)) & "P" & (1 + nPlayer) & " B" & nBall
							End If
						End If
						UpdateDisplay CenterTextOnDisplay(sTopRow), CenterTextOnDisplay(sBotRow)
						Exit Sub
					Else
						sTopRow = GetStatusText(i mod 28, 2)
						sBotRow = WilliamsFormatNum(anScore(nPlayer))
						UpdateDisplay CenterTextOnDisplay(sTopRow), CenterTextOnDisplay(sBotRow)
						nTimeDisplay = 0
					End If
				End If
			Next
			If "" = sTopRow then nModeStatusShowing = eModeNone Else Exit Sub
		Else
			nModeStatusShowing = eModeNone
		End If
	End If

	if bSelectingMode Then
		If 1 = avWizardModesLit(nPlayer).Count Then
			UpdateDisplay CenterTextOnDisplay("<SHOOT TO START>"), _
				CenterTextOnDisplay(asModeNames(nModeSelected))
			nTimeDisplay = 0
		elseif nTimeDisplay < 2001 then
			UpdateDisplay CenterTextOnDisplay("<SELECT A MODE >"), _
				CenterTextOnDisplay(asModeNames(nModeSelected))
		elseif nTimeDisplay < 4001 then
			UpdateDisplay CenterTextOnDisplay("< USE FLIPPERS >"), _
				CenterTextOnDisplay(asModeNames(nModeSelected))
		elseif nTimeDisplay < 6001 Then
			UpdateDisplay CenterTextOnDisplay("<SHOOT TO START>"), _
				CenterTextOnDisplay(asModeNames(nModeSelected))
		elseif nTimeDisplay < 8001 And (Not bFirstSwitchHit) Then
			UpdateDisplay CenterTextOnDisplay("PLAYER " & (1 + nPlayer) & "  BALL " & nBall), _
				CenterTextOnDisplay(asModeNames(nModeSelected))
		Else
			UpdateDisplay CenterTextOnDisplay("<SELECT A MODE >"), _
				CenterTextOnDisplay(asModeNames(nModeSelected))
			nTimeDisplay = 0
		End If
		Exit sub
	End If

	if nPlayersInGame = 0 then
		If nTimeDisplay > nAttractModePageTime Then
			nAttractModePage = (nAttractModePage + 1) Mod nAttractModePageCount
			nTimeDisplay = 0
		End If

		if 0 = nAttractModePage then
			UpdateDisplay CenterTextOnDisplay("SECRET AGENT"), _
				CenterTextOnDisplay("FREE PLAY")
		elseif 1 = nAttractModePage then
			UpdateDisplay CenterTextOnDisplay("SECRET AGENT"), _
				CenterTextOnDisplay("PRESS START")
		elseif 2 = nAttractModePage then
			temp = WilliamsFormatNum(HighScore(0)) & " " & HighScoreName(0)
			UpdateDisplay CenterTextOnDisplay("GRAND CHAMPION"), _
				CenterTextOnDisplay(temp)
		elseif 3 = nAttractModePage then
			temp = WilliamsFormatNum(HighScore(1)) & " " & HighScoreName(1)
			UpdateDisplay CenterTextOnDisplay("HIGH SCORE 1"), _
				CenterTextOnDisplay(temp)
		elseif 4 = nAttractModePage then
			temp = WilliamsFormatNum(HighScore(2)) & " " & HighScoreName(2)
			UpdateDisplay CenterTextOnDisplay("HIGH SCORE 2"), _
				CenterTextOnDisplay(temp)
		elseif 5 = nAttractModePage then
			temp = WilliamsFormatNum(HighScore(3)) & " " & HighScoreName(3)
			UpdateDisplay CenterTextOnDisplay("HIGH SCORE 3"), _
				CenterTextOnDisplay(temp)
		elseif 6 = nAttractModePage then
			temp = WilliamsFormatNum(HighScore(4)) & " " & HighScoreName(4)
			UpdateDisplay CenterTextOnDisplay("HIGH SCORE 4"), _
				CenterTextOnDisplay(temp)
		elseIf nTimeDisplay < 16501 Then
			Select Case nPlayersLastGame
				Case 1
					sTopRow = getFormattedScore(0) & Space(8)
					sBotRow = Space(16)
				Case 2
					sTopRow = getFormattedScore(0) & getFormattedScore(1)
					sBotRow = Space(16)
				Case 3
					sTopRow = getFormattedScore(0) & getFormattedScore(1)
					sBotRow = getFormattedScore(2) & Space(8)
				Case Else
					sTopRow = getFormattedScore(0) & getFormattedScore(1)
					sBotRow = getFormattedScore(2) & getFormattedScore(3)			
			End Select
			UpdateDisplay sTopRow, sBotRow
		Else
			nTimeDisplay = 0
		end if
	End If

	If nPlayersInGame = 1 then
		sScore = blinkScore(nTimeDisplay, nPlayer)
		if len(sScore) < 8 then
			sTopRow = Space(7 - len(sScore)) & sScore & Space(9)
		else
			sTopRow = sScore & Space(16 - len(sScore))
		end if
		
		if nBall < 10 then
			sBotRow = Space(10) & "BALL " & cStr(nBall)
		else
			sBotRow = Space(10) & "BALL" & cStr(nBall)
		end if
		
		UpdateDisplay sTopRow, sBotRow
		
		if nTimeDisplay >= 2160 then nTimeDisplay = 0
	Elseif nPlayersInGame = 2 Then
		If nPlayer = 0 Then
			sTopRow = blinkScore(nTimeDisplay, 0) & getFormattedScore(1)
		Else
			sTopRow = getFormattedScore(0) & blinkScore(nTimeDisplay, 1)
		End If
		
		if nBall < 10 then
			sBotRow = Space(10) & "BALL " & cStr(nBall)
		else
			sBotRow = Space(10) & "BALL" & cStr(nBall)
		end if

		UpdateDisplay sTopRow, sBotRow
		
		if nTimeDisplay >= 2160 then nTimeDisplay = 0
	Elseif nPlayersInGame = 3 Then
		if nBall < 10 then
			sBallText = Space(2) & "BALL " & cStr(nBall)
		else
			sBallText = Space(1) & "BALL " & cStr(nBall)
		end if

		If nPlayer = 0 Then
			sTopRow = blinkScore(nTimeDisplay, 0) & getFormattedScore(1)
			sBotRow = getFormattedScore(2) & sBallText
		ElseIf nPlayer = 1 Then
			sTopRow = getFormattedScore(0) & blinkScore(nTimeDisplay, 1)
			sBotRow = getFormattedScore(2) & sBallText
		Else
			sTopRow = getFormattedScore(0) & getFormattedScore(1)
			sBotRow = blinkScore(nTimeDisplay, 2) & sBallText
		End If
		
		UpdateDisplay sTopRow, sBotRow
		
		if nTimeDisplay >= 2160 then nTimeDisplay = 0
	Elseif nPlayersInGame = 4 Then
		If nPlayer = 0 Then
			sTopRow = blinkScore(nTimeDisplay, 0) & getFormattedScore(1)
			sBotRow = getFormattedScore(2) & getFormattedScore(3)
		ElseIf nPlayer = 1 Then
			sTopRow = getFormattedScore(0) & blinkScore(nTimeDisplay, 1)
			sBotRow = getFormattedScore(2) & getFormattedScore(3)
		ElseIf nPlayer = 2 Then
			sTopRow = getFormattedScore(0) & getFormattedScore(1)
			sBotRow = blinkScore(nTimeDisplay, 2) & getFormattedScore(3)
		Else
			sTopRow = getFormattedScore(0) & getFormattedScore(1)
			sBotRow = getFormattedScore(2) & blinkScore(nTimeDisplay, 3)
		End If
		
		UpdateDisplay sTopRow, sBotRow

		If nScoreBlinks < 3 And nPlayersInGame = 4 Then
			if nTimeDisplay >= 2160 Then
				nTimeDisplay = 0
				nScoreBlinks = nScoreBlinks + 1
			end if
		Else
			if nTimeDisplay >= 3200 And nPlayersInGame = 4 then
				nTimeDisplay = 0
				nScoreBlinks = 0
			End If
		End If
	end if
End Sub

Dim oFlexAlpha
const FlexDMD_RenderMode_SEG_2x16Alpha = 3

Sub InitFlexAlpha
	Dim i, AlphaNumSegs(31)
	Set oFlexAlpha = CreateObject("FlexDMD.FlexDMD")
	With oFlexAlpha
		.TableFile = Table1.Filename & ".vpx"
		.Color = RGB(255, 88, 32)
		.Width = 128
		.Height = 32
		.Clear = True
		.Run = True
		.GameName = cGameName+"-AlphaNum"
		.RenderMode = FlexDMD_RenderMode_SEG_2x16Alpha
	End With
	For i = 0 To 31
		AlphaNumSegs(i) = 0
	Next
	oFlexAlpha.Segments = AlphaNumSegs
End Sub

Dim sPrevLine1, sPrevLine2
Sub UpdateDisplay(sTopLine, sBottomLine)
	Dim i, j
	Dim sChar
	Dim sImage
	Dim aTextLines

	If sTopLine = sPrevLine1 And sBottomLine = sPrevLine2 then
		Exit Sub
	Else
		sPrevLine1 = sTopLine : sPrevLine2 = sBottomLine
	End If

	aTextLines = Array(sTopLine, sBottomLine)

	If bEnableFlexDMD and isObject(oFlexAlpha) Then
		If Not (oFlexAlpha is Nothing) Then
			Dim AlphaNumSegs(31)
			For i = 0 To 31
				AlphaNumSegs(i) = 0
			Next
			for i = 0 to 1
				If len(aTextLines(i)) > 16 Then aTextLines(i) = Left(aTextLines(i), 16)
				for j = 0 to (len(aTextLines(i)) - 1)
					sChar = Mid(aTextLines(i), j + 1, 1)
					if Asc(sChar) > 127 Then sChar = " "
					AlphaNumSegs(i * 16 + j) = aSegments(Asc(sChar))
				next
			Next
			oFlexAlpha.Segments = AlphaNumSegs
		End If
	End If

'	If false Then
	If b2sOn Then
		for i = 0 to 1
			If len(aTextLines(i)) > 16 Then aTextLines(i) = Left(aTextLines(i), 16)
			for j = 0 to (len(aTextLines(i)) - 1)
				sChar = Mid(aTextLines(i), j + 1, 1)
				if Asc(sChar) > 127 Then sChar = " "
				Controller.B2SSetLED i * 16 + j + 1, aSegments(Asc(sChar))
			next
		Next
	End if

	for i = 0 to 1
		' Truncate long strings to 16 characters
		If len(aTextLines(i)) > 16 Then aTextLines(i) = Left(aTextLines(i), 16)
		' Pad short strings to 16 characters
		If len(aTextLines(i)) < 16 Then
			aTextLines(i) = aTextLines(i) & Space(16 - len(aTextLines(i)))
		End If
		for j = 0 to 15
			sChar = Mid(aTextLines(i), j + 1, 1)
			' Handle characters that don't have images
			if sChar = "." or sChar = ":" or sChar = ";" or sChar = "?" Then sChar = " "
			If Asc(sChar) < 32 Then sChar = " "
			If Asc(sChar) > 105 Then sChar = " "

			If Asc(sChar) < 100 Then
				sImage = "d0" & Asc(sChar)
			Else
				sImage = "d" & Asc(sChar)
			End If
			If i = 0 Then
				aoTopRowDigits(j).ImageA = sImage
			Else
				aoBottomRowDigits(j).ImageA = sImage
			End If
		next
	Next
End Sub

Function getFormattedBonus(nValue)
	dim sScore
	sScore = WilliamsFormatNum(nValue)
	If len(sScore) < 8 Then
		sScore =  Space(7 - len(sScore)) & sScore & " "
	End If

	getFormattedBonus = sScore
End Function

Function getFormattedScore(nPlayer)
	dim sScore
	sScore = WilliamsFormatNum(anScore(nPlayer))
	If len(sScore) < 8 Then
		sScore =  Space(7 - len(sScore)) & sScore & " "
	End If

	getFormattedScore = sScore
End Function

Function blinkScore(nFrameNo, nPlayer)
	dim sBeforeBlank
	dim sAfterBlank
	dim sScore
	dim nFigureToBlink

	sScore = getFormattedScore(nPlayer)

	if nTimeDisplay > 999 And nPlayersInGame > 0 Then
		If nTimeDisplay < 1601 Then
			nFigureToBlink = (nTimeDisplay - 1000) \ 80
			sBeforeBlank = Left(sScore, 7 - nFigureToBlink)
			sAfterBlank = Right(sScore, nFigureToBlink)
			sScore = sBeforeBlank & " " & sAfterBlank
		ElseIf nTimeDisplay < 2161 Then
			nFigureToBlink = (nTimeDisplay - 1600) \ 80
			sBeforeBlank = Left(sScore, nFigureToBlink)
			sAfterBlank = Right(sScore, 7 - nFigureToBlink)
			sScore = sBeforeBlank & " " & sAfterBlank
		ElseIf nTimeDisplay < 3201 Then ' Only show ball nr if 4 players
			if nBall < 10 then
				sScore = Space(2) & "BALL " & cStr(nBall)
			else
				sScore = Space(1) & "BALL " & cStr(nBall)
			end if
		End If
	End If

	blinkScore = sScore
End Function

' Format an integer as a string, using the special characters to display a
' number and a comma in the same character. The "0," to "9," special characters
' are at ASCII code points 96 - 105.
Function WilliamsFormatNum(nNumber)
	Dim i
	dim sNum
	dim sPrefix
	dim sPostfix
	dim sDigitWithComma
	dim nLen
	if not IsNumeric(nNumber) then
		WilliamsFormatNum = "NAN"
		exit function
	end if
	If 0 = nNumber Then
		WilliamsFormatNum = "00"
		exit function
	End If
	WilliamsFormatNum = cStr(nNumber)
	nLen = len(WilliamsFormatNum)
	
	if nLen < 4 then exit function
	for i = nLen to 3 step -1
		if i mod 3 = 1 then
			sPrefix = left(WilliamsFormatNum, nLen - i)
			sPostfix = right(WilliamsFormatNum, i - 1)
			sDigitWithComma = chr(asc(mid(WilliamsFormatNum, nLen + 1 - i, 1)) + 48)
			WilliamsFormatNum = sPrefix & sDigitWithComma & sPostfix
		end if
	next
End Function

Function CenterTextOnDisplay(sText)
	dim nLen
	nLen = len(sText)
	if nLen > 16 then
		CenterTextOnDisplay = Left(sText, 16)
	Else
		CenterTextOnDisplay = Space((16 - nLen) \ 2) & sText &_
							  Space((17 - nLen) \ 2)
	end if
End Function

Function GetInstantInfoPage(nPage)
	Dim i
	Dim nCount
	Dim nShots
	Dim nMode

	Select Case nPage
		Case 1
			If 1 = nExtraBalls Then
				GetInstantInfoPage = Array("INSTANT INFO", "1 EXTRA BALL")
			Else
				GetInstantInfoPage = Array("INSTANT INFO", nExtraBalls & " EXTRA BALLS")
			End If
		Case 2
			nCount = anExtraBallsLit(nPlayer)
			If 1 = nCount Then
				GetInstantInfoPage = Array("INSTANT INFO", "1 EXTRA BALL LIT")
			Else
				GetInstantInfoPage = Array("INSTANT INFO", nCount & " X-BALLS LIT")
			End If
		Case 3
			nCount = 4 - avXtargets(nPlayer).Count
			If nPlayfieldX < 3 Then
				If 1 = nCount Then
					GetInstantInfoPage = Array("INFO - 1 TARGET", _
						"FOR " & (1 + nPlayfieldX) & "X SCORE LIT")
				Else
					GetInstantInfoPage = Array("INFO - " & nCount & " TARGETS", _
						"FOR " & (1 + nPlayfieldX) & "X SCORE LIT")
				End If
			Else
				If 1 = nCount Then
					GetInstantInfoPage = Array("INFO - 1 TARGET", _
						"FOR 3X EXTENSION")
				Else
					GetInstantInfoPage = Array("INFO - " & nCount & " TARGETS", _
						"FOR 3X EXTENSION")
				End If
			End If
		Case 4
			nCount = anBagLevel(nPlayer)
			If 0 = nCount Then
				GetInstantInfoPage = Array("BAG OF TRICKS", "HIT CYAN TARGETS")
			Else
				GetInstantInfoPage = Array("BAG OF TRICKS", "LEVEL " & nCount & " IS LIT")
			End If
		Case 5
			If avModesRunning(nPlayer).Contains(eModeCyberMBall) Then
				nCount = anJackpotsForSuper(eMballCyber) - aanMballJackpots(nPlayer, eMballCyber)
				if nCount < 1 Then
					GetInstantInfoPage = Array("INFO-CYBER MBALL", "SUPER JACKPOT LIT")
				Else
					GetInstantInfoPage = Array("INFO-CYBER MBALL", nCount & " JP FOR SUPER")
				End If
			Else
				nCount = SpinsForCyberLock - aanLockProgress(nPlayer, eMballCyber)
				if nCount < 1 Then
					GetInstantInfoPage = Array("INFO-CYBER MBALL", "SHOOT LEFT LOOP")
				Else
					GetInstantInfoPage = Array("INFO-CYBER MBALL", nCount & " SPINS LEFT")
				End If
			End If
		Case 6
			If avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
				nCount = anJackpotsForSuper(eMBallWater) - aanMballJackpots(nPlayer, eMBallWater)
				if nCount < 1 Then
					GetInstantInfoPage = Array("INFO-WATER MBALL", "SUPER JACKPOT LIT")
				Else
					GetInstantInfoPage = Array("INFO-WATER MBALL", nCount & " JP FOR SUPER")
				End If
			Else
				nCount = aanLocksLit(nPlayer, eMBallWater)
				if 1 = nCount Then
					GetInstantInfoPage = Array("INFO-WATER MBALL", nCount & " LOCK LIT")
				Else
					GetInstantInfoPage = Array("INFO-WATER MBALL", nCount & " LOCKS LIT")
				End If
			End If
		Case 7
			nCount = aanLocksMade(nPlayer, eMBallWater)
			If avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
				GetInstantInfoPage = Array("INFO-WATER MBALL", "3 BALLS LOCKED")
			elseif 1 = nCount Then
				GetInstantInfoPage = Array("INFO-WATER MBALL", nCount & " BALL LOCKED")
			Else
				GetInstantInfoPage = Array("INFO-WATER MBALL", nCount & " BALLS LOCKED")
			End If
		Case 8
			If avModesRunning(nPlayer).Contains(eModeFireMBall) Then
				nCount = anJackpotsForSuper(eMBallFire) - aanMballJackpots(nPlayer, eMBallFire)
				if nCount < 1 Then
					GetInstantInfoPage = Array("INFO-FIRE MBALL", "SUPER JACKPOT LIT")
				Else
					GetInstantInfoPage = Array("INFO-FIRE MBALL", nCount & " JP FOR SUPER")
				End If
			Else
				nCount = aanLocksLit(nPlayer, eMBallFire)
				if 1 = nCount Then
					GetInstantInfoPage = Array("INFO-FIRE MBALL", nCount & " LOCK LIT")
				Else
					GetInstantInfoPage = Array("INFO-FIRE MBALL", nCount & " LOCKS LIT")
				End If
			End If
		Case 9
			nCount = aanLocksMade(nPlayer, eMBallFire)
			If avModesRunning(nPlayer).Contains(eModeFireMBall) Then
				GetInstantInfoPage = Array("INFO-FIRE MBALL", "3 BALLS LOCKED")
			elseif 1 = nCount Then
				GetInstantInfoPage = Array("INFO-FIRE MBALL", nCount & " BALL LOCKED")
			Else
				GetInstantInfoPage = Array("INFO-FIRE MBALL", nCount & " BALLS LOCKED")
			End If
		Case 10
			If avModesRunning(nPlayer).Contains(eModeEarthMBall) Then
				nCount = anJackpotsForSuper(eMBallEarth) - aanMballJackpots(nPlayer, eMBallEarth)
				if nCount < 1 Then
					GetInstantInfoPage = Array("INFO-EARTH MBALL", "SUPER JACKPOT LIT")
				Else
					GetInstantInfoPage = Array("INFO-EARTH MBALL", nCount & " JP FOR SUPER")
				End If
			Else
				nCount = aanLocksLit(nPlayer, eMBallEarth)
				if 1 = nCount Then
					GetInstantInfoPage = Array("INFO-EARTH MBALL", nCount & " LOCK LIT")
				Else
					GetInstantInfoPage = Array("INFO-EARTH MBALL", nCount & " LOCKS LIT")
				End If
			End If
		Case 11
			nCount = aanLocksMade(nPlayer, eMBallEarth)
			If avModesRunning(nPlayer).Contains(eModeEarthMBall) Then
				GetInstantInfoPage = Array("INFO-EARTH MBALL", "3 BALLS LOCKED")
			elseif 1 = nCount Then
				GetInstantInfoPage = Array("INFO-EARTH MBALL", nCount & " BALL LOCKED")
			Else
				GetInstantInfoPage = Array("INFO-EARTH MBALL", nCount & " BALLS LOCKED")
			End If
		Case 12
			If avModesRunning(nPlayer).Contains(eModeAirMBall) Then
				nCount = anJackpotsForSuper(eMBallAir) - aanMballJackpots(nPlayer, eMBallAir)
				if nCount < 1 Then
					GetInstantInfoPage = Array("INFO-AIR MBALL", "SUPER JACKPOT LIT")
				Else
					GetInstantInfoPage = Array("INFO-AIR MBALL", nCount & " JP FOR SUPER")
				End If
			Else
				if aanMballsPlayed(nPlayer, eMballAir) < 3 then
					nCount = 8 - avAirLettersHit(nPlayer).Count
				else
					nCount = 16 - ((avAirLettersHit(nPlayer).Count * 2) + (avAirLettersLit(nPlayer).Count))
				end if
				if 0 = nCount Then
					GetInstantInfoPage = Array("INFO-AIR MBALL", "SHOOT RIGHT RAMP")
				Elseif 1 = nCount Then
					GetInstantInfoPage = Array("INFO-AIR MBALL", "1 SHOT LEFT")
				Else
					GetInstantInfoPage = Array("INFO-AIR MBALL", nCount & " SHOTS LEFT")
				End If
			End If
		Case 13
			If avModesRunning(nPlayer).Contains(eModeIceMBall) Then
				nCount = anJackpotsForSuper(eMBallIce) - aanMballJackpots(nPlayer, eMBallIce)
				if nCount < 1 Then
					GetInstantInfoPage = Array("INFO-ICE MBALL", "SUPER JACKPOT LIT")
				Else
					GetInstantInfoPage = Array("INFO-ICE MBALL", nCount & " JP FOR SUPER")
				End If
			Else
				nCount = aanLocksLit(nPlayer, eMBallIce)
				if 1 = nCount Then
					GetInstantInfoPage = Array("INFO-ICE MBALL", nCount & " LOCK LIT")
				Else
					GetInstantInfoPage = Array("INFO-ICE MBALL", nCount & " LOCKS LIT")
				End If
			End If
		Case 14
			nCount = aanLocksMade(nPlayer, eMBallIce)
			If avModesRunning(nPlayer).Contains(eModeIceMBall) Then
				GetInstantInfoPage = Array("INFO-ICE MBALL", "3 BALLS LOCKED")
			elseif 1 = nCount Then
				GetInstantInfoPage = Array("INFO-ICE MBALL", nCount & " BALL LOCKED")
			Else
				GetInstantInfoPage = Array("INFO-ICE MBALL", nCount & " BALLS LOCKED")
			End If
		Case 15
			nCount = avModesPlayed(nPlayer).Count
			If 1 = nCount Then
				GetInstantInfoPage = Array("INSTANT INFO", "1 MODE PLAYED")
			Else
				GetInstantInfoPage = Array("INSTANT INFO", nCount & " MODES PLAYED")
			End If
		Case 16
			GetInstantInfoPage = Array("GRAND CHAMPION", _
				WilliamsFormatNum(HighScore(0)) & " " & HighScoreName(0))
		Case 17
			GetInstantInfoPage = Array("HIGH SCORE 1", _
				WilliamsFormatNum(HighScore(1)) & " " & HighScoreName(1))
		Case 18
			GetInstantInfoPage = Array("HIGH SCORE 2", _
				WilliamsFormatNum(HighScore(2)) & " " & HighScoreName(2))
		Case 19
			GetInstantInfoPage = Array("HIGH SCORE 3", _
				WilliamsFormatNum(HighScore(3)) & " " & HighScoreName(3))
		Case Else
			GetInstantInfoPage = Array("INSTANT INFO", "INSTANT INFO")
	End Select
End Function

' Bit sets, the set data type implemented as a bit vector
' Only numbers 0 to 30 are valid elements
Class BitSet

	private mnCapacity
	private mnBits
	private i
	private temp
	private lsb
	private msb

	private sub Class_Initialize
		mnCapacity = 0
		mnBits = 0
	end Sub
	
	public property get Capacity
		Capacity = mnCapacity
	end property

	public property let Capacity(nSize)
		mnCapacity = nSize
	end property

	public Sub Add (nElement)
		If Not (nElement < 0 or nElement >= mnCapacity) Then
			mnBits = mnBits Or aPowersOfTwo(nElement)
		End If
	End Sub

	public Sub Remove (nElement)
		If Not (nElement < 0 or nElement >= mnCapacity) Then
			mnBits = mnBits And Not aPowersOfTwo(nElement)
		End If
	End Sub

	public Sub Clear
		mnBits = 0
	End Sub

	public function Contains(nElement)
		If nElement < 0 or nElement >= mnCapacity Then
			Contains = False
		Else
			temp = mnBits And aPowersOfTwo(nElement)
			Contains = Temp <> 0
		End If
	end Function

	public function IsFull
		temp = aPowersOfTwo(mnCapacity) - 1
		IsFull = Not (mnBits <> temp)
	end Function

	public function Count
		Dim nCount
		nCount = 0
		for i = 0 to (mnCapacity - 1)
			If Me.Contains(i) Then nCount = nCount + 1
		Next
		Count = nCount
	End Function

	public Sub RotateLeft
	' save least significant bit
		lsb = mnBits And 1
	' shift all bits one to the right
		mnBits = Int(mnBits / 2)
	' Set the most significant bit to what the least significant bit was
		If lsb <> 0 Then
			mnBits = mnBits Or aPowersOfTwo(mnCapacity - 1)
		Else
			mnBits = mnBits And (Not aPowersOfTwo(mnCapacity - 1))
		End If
	End Sub

	public Sub RotateRight
	' save most significant bit
		msb = mnBits And aPowersOfTwo(mnCapacity - 1)
	' shift all bits one to the left
		mnBits = mnBits * 2
	' Clear anything past the most significant bit
		mnBits = mnBits And (Not aPowersOfTwo(mnCapacity))
	' Set the least significant bit to what the most significant bit was
		If msb <> 0 Then
			mnBits = mnBits Or aPowersOfTwo(0)
		Else
			mnBits = mnBits And (Not aPowersOfTwo(0))
		End If
	End Sub

End Class

' *** Message Queue
Class Queue

	private maQueue()
	private mnSize
	private mnCapacity
	private mnIndex
	private i

	private sub Class_Initialize
		mnCapacity = 10
		mnIndex = 0
		mnSize = 0
		redim maQueue(mnCapacity)
	end Sub

	public property get Size
		Size = mnSize - mnIndex
	end property

	public Sub Enqueue(element)
		If mnSize >= mnCapacity Then
			mnCapacity = mnCapacity * 2
			Redim preserve maQueue(mnCapacity)
		End If
		maQueue(mnSize) = element
		mnSize = mnSize + 1
	end Sub

	public Sub PushFirst(element)
		If mnSize >= mnCapacity Then
			mnCapacity = mnCapacity * 2
			Redim preserve maQueue(mnCapacity)
		End If
		for i = mnSize to (mnIndex + 1) step -1
			maQueue(i) = maQueue(i - 1)
		Next
		maQueue(mnIndex) = element
		mnSize = mnSize + 1		
	End Sub

	public function Dequeue()
		If mnSize - mnIndex < 1 Then
			Dequeue = null
		Else
			Dequeue = maQueue(mnIndex)
			mnIndex = mnIndex + 1
		End If
	end Function

	' Return the first item without removing it
	public function Peek()
		If mnSize - mnIndex < 1 Then
			Peek = null
		Else
			Peek = maQueue(mnIndex)
		End If
	end Function

	public sub Rewind
		mnIndex = 0
	end Sub

	public sub Clear
		mnCapacity = 10
		mnIndex = 0
		mnSize = 0
		redim maQueue(mnCapacity)
	end Sub

End Class

' Light handling

Sub LightSeq1_PlayDone
	Dim i, j
	dim colorDark, colorFull
	If nPlayersInGame < 1 Then
		nColorCycleIndex = (nColorCycleIndex + 1) mod nColorsInCycle
		colorDark = anColorCycleDark(nColorCycleIndex)
		colorFull = anColorCycleLight(nColorCycleIndex)
		for i = 1 to 15 step 2
			ApplyLampRGB i, colorFull
			ApplyLampRGB i + 1, colorFull
'			aoShotLightsMode(i).color = colorDark
'			aoShotLightsMode(i).colorFull = colorFull
'			aoShotLightsMBall(i).color = colorDark
'			aoShotLightsMBall(i).colorFull = colorFull
		next
		LightSeq1.UpdateInterval = 25
		LightSeq1.Play SeqCircleOutOn, 40
	End If
End Sub

Function canAdvanceFire
	If avModesRunning(nPlayer).Contains(eModeFireMBall) Then
		canAdvanceFire = False
	Elseif IsWizardModeActive Then
		canAdvanceFire = False
	ElseIf 0 = aanMballsPlayed(nPlayer, eMballFire) _
	And (aanLocksMade(nPlayer, eMballFire) + aanLocksLit(nPlayer, eMballFire)) > 2 Then
		canAdvanceFire = False
	Elseif aanMballsPlayed(nPlayer, eMballFire) > 0 And aanLocksLit(nPlayer, eMballFire) > 0 Then
		canAdvanceFire = False
	Else
		canAdvanceFire = True
	End If
End Function

Sub UpdateLightsFireLanes
	Dim i
	
	If avFireLanes(nPlayer).IsFull Then
		If aanMballsPlayed(nPlayer, eMballFire) < 1 then avFireLanes(nPlayer).Clear
		If False = lightLaneF.timerEnabled And True = canAdvanceFire Then
			lightLaneF.timerEnabled = True
			for i = 0 to 3
				aoFireLaneLights(i).state = LightStateBlinking
					DOF 205, DOFOn
					DOF 206, DOFOn
					DOF 207, DOFOn
					DOF 208, DOFOn
				next
		End If
		If canAdvanceFire Then
			FireAdvanceLock
					DOF 205, DOFOn
					DOF 206, DOFOn
					DOF 207, DOFOn
					DOF 208, DOFOn
		End If
	End If
	if False = lightLaneF.timerEnabled Then
		for i = 0 to 3
			if avFireLanes(nPlayer).Contains(i) or Not canAdvanceFire Then
				aoFireLaneLights(i).state = LightStateOn
			else
				aoFireLaneLights(i).state = LightStateOff
			end if 
		next 
	end if 
End Sub

Sub lightLaneF_Timer
	lightLaneF.timerEnabled = False
	UpdateLightsFireLanes
End Sub

Sub AwardMystery (bSkipAnimation)
	Dim i, j, nAwardConsidered, nAwardGiven, bGoing, nAwardLevels, nLevel, sAdvance
	Dim nRemaining, nCount

	nAwardConsidered = Int(rnd * 11)
	nAwardGiven = eMysteryNone
	sAdvance = ""
	Select Case anBagLevel(nPlayer)
		Case 1
			nAwardLevels = Array(1, 2, 1)
		Case 2
			nAwardLevels = Array(2, 1, 2)
		Case 3
			nAwardLevels = Array(2, 3, 2)
		Case 4
			nAwardLevels = Array(3, 2, 3)
		Case 5
			nAwardLevels = Array(3, 4, 3)
		Case 6
			nAwardLevels = Array(4, 3, 4)
		Case 7
			nAwardLevels = Array(4, 5, 4)
		Case 8
			nAwardLevels = Array(5, 4, 5)
		Case Else
			nAwardLevels = Array(5, 5, 5)
	End Select
	nLevel = nAwardLevels(Int(rnd * 3))

	bGoing = True
	If False = abBagExtraBall(nPlayer) And anBagLevelSum(nPlayer) + anBagLevel(nPlayer) > 8 Then
		asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "EXTRA BALL LIT")
		AddCallout "VO_hero_extra_ball_lit", eVOPrioExtraBall, eCharacterHero
		anExtraBallsLit(nPlayer) = anExtraBallsLit(nPlayer) + 1
		abBagExtraBall(nPlayer) = True
		anBagLevelSum(nPlayer) = anBagLevelSum(nPlayer) + anBagLevel(nPlayer)
		anBagLevel(nPlayer) = 0
		If bSkipAnimation Then
			ShowText asBagAward(0), asBagAward(1), 2500, eVOPrioMystery
		Else
			if Not IsMultiballActive Then LightShowStart eLightShowMystery, eVOPrioMystery
			nTimeBagAnimation = 3000
			asBagAward(0) = CenterTextOnDisplay(asBagAward(0))
			asBagAward(1) = CenterTextOnDisplay(asBagAward(1))
		End If
		Exit Sub
	End If

	If anSwitchLastHit(eSwitchLeftInlane) > 0 _
    And anSwitchLastHit(eSwitchKickerBottom) - anSwitchLastHit(eSwitchLeftInlane) < 1000 _
	And anSwitchLastHit(eSwitchKickerBottom) > anSwitchLastHit(eSwitchLeftInlane) Then
		AddCallout "VO_hero_shatz", eVOPrioMystery, eCharacterHero
		anBagLevel(nPlayer) = anBagLevel(nPlayer) + 2
		If anBagLevel(nPlayer) > 9 then anBagLevel(nPlayer) = 9
	Else
		AddCallout "VO_hero_mystery", eVOPrioMystery, eCharacterHero
	End If

	If BIP < 6 And ((eModeNone <> anWizardModeRunning(nPlayer) And anAddABallUsed(nPlayer) < 1) _
	or (eModeEvilUnited = anWizardModeRunning(nPlayer) And anAddABallUsed(nPlayer) < 3) _
	or (eModeMegaWizard = anWizardModeRunning(nPlayer) And anAddABallUsed(nPlayer) < 5)) Then
		If aanModeShots(nPlayer, anWizardModeRunning(nPlayer)) > 0 _
		Or eModeMegaWizard = anWizardModeRunning(nPlayer) Then
			If nLevel < 6 - (BIP + nBallPhysLocked) Then
				nCount = nLevel
			Else
				nCount = 6 - (BIP + nBallPhysLocked)
			End If
			If 1 = nLevel Then
				asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADD A BALL")
				AddBalls 1
				nTimeBallSave = cBallSaveMball
			Else
				asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADD " & nCount & " BALLS")
				AddBalls nCount
				nTimeBallSave = cBallSaveMball + (2000 * (nCount - 1))
			End If
			If bSkipAnimation Then
				ShowText asBagAward(0), asBagAward(1), 2500, eVOPrioMystery
			Else
				if Not IsMultiballActive Then LightShowStart eLightShowMystery, eVOPrioMystery
				nTimeBagAnimation = 3000
				asBagAward(0) = CenterTextOnDisplay(asBagAward(0))
				asBagAward(1) = CenterTextOnDisplay(asBagAward(1))
			End If
			LightShootAgain.state = LightStateBlinking
			anAddABallUsed(nPlayer) = anAddABallUsed(nPlayer) + 1
			anBagLevelSum(nPlayer) = anBagLevelSum(nPlayer) + anBagLevel(nPlayer)
			anBagLevel(nPlayer) = 0
			Exit Sub
		End If
	End If

	For i = 1 to 11
		bGoing = True
		For j = 0 to 2
			If aanLastBagAwards(nPlayer, j) = nAwardConsidered Then bGoing = False
		Next
		If bGoing Then
			If nAwardConsidered = eMysteryMballCyber Then
				nRemaining = SpinsForCyberLock - aanLockProgress(nPlayer, eMballCyber)
				If avModesRunning(nPlayer).Contains(eModeCyberMBall) Then
				' do nothing
				ElseIf IsWizardModeActive Then
				' do nothing
				ElseIf 1 = nLevel And nRemaining > 15 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADVANCE CYBER")
					aanLockProgress(nPlayer, eMballCyber) = aanLockProgress(nPlayer, eMballCyber) + 15
					UpdateLEDs SpinsForCyberLock - aanLockProgress(nPlayer, eMballCyber)
					nAwardGiven = eMysteryMballCyber
				ElseIf 2 = nLevel And nRemaining > 15 Then
					If sAdvance = "" Then
						sAdvance = "CYBER"
					Else
						asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADV " & sAdvance & "+CYBER")
						nAwardGiven = eMysteryMballCyber
					End If
					aanLockProgress(nPlayer, eMballCyber) = aanLockProgress(nPlayer, eMballCyber) + 15
					UpdateLEDs SpinsForCyberLock - aanLockProgress(nPlayer, eMballCyber)
				ElseIf 3 = nLevel And nRemaining > 45 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADVANCE CYBER")
					aanLockProgress(nPlayer, eMballCyber) = aanLockProgress(nPlayer, eMballCyber) + 45
					UpdateLEDs SpinsForCyberLock - aanLockProgress(nPlayer, eMballCyber)
					nAwardGiven = eMysteryMballCyber
				ElseIf 4 = nLevel And nRemaining > 45 Then
					If sAdvance = "" Then
						sAdvance = "CYBER"
					Else
						asBagAward = Array("LOCK " & sAdvance & " +", "ADVANCE CYBER")
						nAwardGiven = eMysteryMballCyber
					End If
					aanLockProgress(nPlayer, eMballCyber) = aanLockProgress(nPlayer, eMballCyber) + 45
					nAwardGiven = eMysteryMballCyber
				Elseif 5 = nLevel And nRemaining > 30 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "CYBER MULTIBALL")
					CyberMballStart
					nAwardGiven = eMysteryMballCyber
				End If
			ElseIf nAwardConsidered = eMysteryMballWater Then
				nRemaining = 3 - (aanLocksMade(nPlayer, eMballWater) + aanLocksLit(nPlayer, eMballWater))
				If avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
				' do nothing
				ElseIf IsWizardModeActive Then
				' do nothing
				ElseIf 1 = nLevel And nRemaining > 1 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADVANCE WATER")
					If aanLocksLit(nPlayer, eMBallWater) < 1 Then
						WaterAdvanceLock eShotUnderFlipper
					Else
						WaterLockBall
					End If
					nAwardGiven = eMysteryMballWater
				ElseIf 2 = nLevel And nRemaining > 1 Then
					If sAdvance = "" Then
						sAdvance = "WATER"
					Else
						asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADV " & sAdvance & "+WATER")
						nAwardGiven = eMysteryMballWater
					End If
					If aanLocksLit(nPlayer, eMBallWater) < 1 Then
						WaterAdvanceLock eShotUnderFlipper
					Else
						WaterLockBall
					End If
				ElseIf 3 = nLevel And nRemaining > 1 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), _
						"WATER LOCK " & (aanLocksMade(nPlayer, eMBallWater) + 1))
					aanLocksLit(nPlayer,eMBallWater) = aanLocksLit(nPlayer,eMBallWater) + 1
					WaterLockBall
					nAwardGiven = eMysteryMballWater
				ElseIf 4 = nLevel And nRemaining > 1 Then
					If sAdvance = "" Then
						sAdvance = "WATER"
					Else
						If sAdvance = "CYBER" or sAdvance = "AIR" Then
							asBagAward = Array("ADVANCE " & sAdvance  & " +", _
							"WATER LOCK " & (aanLocksMade(nPlayer, eMBallWater) + 1))
						else
							asBagAward = Array("LOCK " & sAdvance  & " +", _
							"WATER LOCK " & (aanLocksMade(nPlayer, eMBallWater) + 1))
						End If
						nAwardGiven = eMysteryMballWater
					End If
					aanLocksLit(nPlayer,eMBallWater) = aanLocksLit(nPlayer,eMBallWater) + 1
					WaterLockBall
				Elseif 5 = nLevel And nRemaining > 2 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "WATER MULTIBALL")
					WaterMballStart
					nAwardGiven = eMysteryMballWater
				End If
			Elseif nAwardConsidered = eMysteryMballFire Then
				nRemaining = 3 - (aanLocksMade(nPlayer, eMballFire) + aanLocksLit(nPlayer, eMballFire))
				If avModesRunning(nPlayer).Contains(eModeFireMBall) Then
				' do nothing
				ElseIf IsWizardModeActive Then
				' do nothing
				ElseIf 1 = nLevel And nRemaining > 1 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADVANCE FIRE")
					If aanLocksLit(nPlayer, eMballFire) < 1 Then
						For j = 0 to 3
							If Not avFireLanes(nPlayer).Contains(j) Then
								HandleFireRollover j
								Exit for
							End If
						Next
					Else
						FireLockBall
					End If
					nAwardGiven = eMysteryMballFire
				ElseIf 2 = nLevel And nRemaining > 1 Then
					If sAdvance = "" Then
						sAdvance = "FIRE"
					Else
						asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADV " & sAdvance & "+FIRE")
						nAwardGiven = eMysteryMballFire
					End If
					If aanLocksLit(nPlayer, eMballFire) < 1 Then
						FireAdvanceLock
					Else
						FireLockBall
					End If
				ElseIf 3 = nLevel And nRemaining > 1 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), _
						"FIRE LOCK " & (aanLocksMade(nPlayer, eMballFire) + 1))
					aanLocksLit(nPlayer,eMBallFire) = aanLocksLit(nPlayer,eMBallFire) + 1
					FireLockBall
					nAwardGiven = eMysteryMballFire
				ElseIf 4 = nLevel And nRemaining > 1 Then
					If sAdvance = "" Then
						sAdvance = "FIRE"
					Else
						If sAdvance = "CYBER" or sAdvance = "AIR" Then
							asBagAward = Array("ADVANCE " & sAdvance  & " +", _
							"FIRE LOCK " & (aanLocksMade(nPlayer, eMballFire) + 1))
						else
							asBagAward = Array("LOCK " & sAdvance  & " +", _
							"FIRE LOCK " & (aanLocksMade(nPlayer, eMballFire) + 1))
						End If
						nAwardGiven = eMysteryMballFire
					End If
					aanLocksLit(nPlayer,eMBallFire) = aanLocksLit(nPlayer,eMBallFire) + 1
					FireLockBall
					nAwardGiven = eMysteryMballFire
				Elseif 5 = nLevel And nRemaining > 2 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "FIRE MULTIBALL")
					FireMballStart
					nAwardGiven = eMysteryMballFire
				End If
			Elseif nAwardConsidered = eMysteryMballEarth Then
				nRemaining = 3 - (aanLocksMade(nPlayer, eMballEarth) + aanLocksLit(nPlayer, eMballEarth))
				If avModesRunning(nPlayer).Contains(eModeEarthMBall) Then
				' do nothing
				ElseIf IsWizardModeActive Then
				' do nothing
				ElseIf 1 = nLevel And nRemaining > 1 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADVANCE EARTH")
					If aanLocksLit(nPlayer, eMballEarth) < 1 Then
						EarthAdvanceLock
					Else
						LockBallEarth False
					End If
					nAwardGiven = eMysteryMballEarth
				ElseIf 2 = nLevel And nRemaining > 1 Then
					If sAdvance = "" Then
						sAdvance = "EARTH"
					Else
						asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADV " & sAdvance & "+EARTH")
						nAwardGiven = eMysteryMballEarth
					End If
					If aanLocksLit(nPlayer, eMballEarth) < 1 Then
						EarthAdvanceLock
					Else
						LockBallEarth False
					End If
				ElseIf 3 = nLevel And nRemaining > 1 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), _
						"EARTH LOCK " & (aanLocksMade(nPlayer, eMballEarth) + 1))
					aanLocksLit(nPlayer, eMBallEarth) = aanLocksLit(nPlayer, eMBallEarth) + 1
					LockBallEarth False
					nAwardGiven = eMysteryMballEarth
				ElseIf 4 = nLevel And nRemaining > 1 Then
					If sAdvance = "" Then
						sAdvance = "EARTH"
					Else
						If sAdvance = "CYBER" or sAdvance = "AIR" Then
							asBagAward = Array("ADVANCE " & sAdvance  & " +", _
							"EARTH LOCK " & (aanLocksMade(nPlayer, eMballEarth) + 1))
						else
							asBagAward = Array("LOCK " & sAdvance  & " +", _
							"EARTH LOCK " & (aanLocksMade(nPlayer, eMballEarth) + 1))
						End If
						nAwardGiven = eMysteryMballEarth
					End If
					aanLocksLit(nPlayer,eMBallEarth) = aanLocksLit(nPlayer,eMBallEarth) + 1
					LockBallEarth False
				Elseif 5 = nLevel And nRemaining > 2 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "EARTH MULTIBALL")
					EarthMballStart False
					nAwardGiven = eMysteryMballEarth
				End If
			Elseif nAwardConsidered = eMysteryMballAir Then
				nRemaining = 8 - avAirLettersHit(nPlayer).Count
				If avModesRunning(nPlayer).Contains(eModeAirMBall) Then
				' do nothing
				ElseIf IsWizardModeActive Then
				' do nothing
				ElseIf 1 = nLevel And nRemaining > 1 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "1 AIR LETTER")
					nCount = 0
					For j = 0 to 7
						If avAirLettersLit(nPlayer).Contains(j) Then
							AirAdvanceLock j
							nCount = nCount + 1
							If nCount > 0 Then Exit For
						End If
					Next
					nAwardGiven = eMysteryMballAir
				ElseIf 2 = nLevel And nRemaining > 1 Then
					If sAdvance = "" Then
						sAdvance = "AIR"
					Else
						asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADV " & sAdvance & "+AIR")
						nAwardGiven = eMysteryMballAir
					End If
					nCount = 0
					For j = 0 to 7
						If avAirLettersLit(nPlayer).Contains(j) Then
							AirAdvanceLock j
							nCount = nCount + 1
							If nCount > 0 Then Exit For
						End If
					Next
				ElseIf 3 = nLevel And nRemaining > 4 Then
					nCount = 0
					For j = 0 to 7
						If avAirLettersLit(nPlayer).Contains(j) Then
							AirAdvanceLock j
							nCount = nCount + 1
							If nCount > 3 Then Exit For
						End If
					Next
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), nCount & " AIR LETTERS")
					nAwardGiven = eMysteryMballAir
				ElseIf 4 = nLevel And nRemaining > 4 Then
					If sAdvance = "" Then
						sAdvance = "AIR"
					Else
						If sAdvance = "CYBER" Then
							asBagAward = Array("ADVANCE " & sAdvance  & " +", "ADVANCE AIR")
						else
							asBagAward = Array("LOCK " & sAdvance  & " +", "ADVANCE AIR")
						End If
						nAwardGiven = eMysteryMballAir
					End If
					nCount = 0
					For j = 0 to 7
						If avAirLettersLit(nPlayer).Contains(j) Then
							AirAdvanceLock j
							nCount = nCount + 1
							If nCount > 3 Then Exit For
						End If
					Next
				Elseif 5 = nLevel And nRemaining > 2 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "AIR MULTIBALL")
					AirMballStart
					nAwardGiven = eMysteryMballAir
				End If
			Elseif nAwardConsidered = eMysteryMballIce Then
				nRemaining = 3 - (aanLocksMade(nPlayer, eMballIce) + aanLocksLit(nPlayer, eMballIce))
				If avModesRunning(nPlayer).Contains(eModeIceMBall) Then
				' do nothing
				ElseIf IsWizardModeActive Then
				' do nothing
				ElseIf 1 = nLevel And nRemaining > 1 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "1 ICE LETTER")
					If aanLocksLit(nPlayer, eMballIce) < 1 Then
						If Not drop1.isDropped Then
							drop1.isDropped = True
						ElseIf Not drop2.isDropped Then
							drop2.isDropped = True
						ElseIf Not drop2.isDropped Then
							drop3.isDropped = True
						End if
					Else
						IceLockBall
					End If
					nAwardGiven = eMysteryMballIce
				ElseIf 2 = nLevel And nRemaining > 1 Then
					If sAdvance = "" Then
						sAdvance = "ICE"
					Else
						asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ADV " & sAdvance & "+ICE")
						nAwardGiven = eMysteryMballIce
					End If
					If aanLocksLit(nPlayer, eMballIce) < 1 Then
						If Not drop1.isDropped Then
							drop1.isDropped = True
						ElseIf Not drop2.isDropped Then
							drop2.isDropped = True
						ElseIf Not drop2.isDropped Then
							drop3.isDropped = True
						End if
					Else
						IceLockBall
					End If
				ElseIf 3 = nLevel And nRemaining > 1 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), _
						"ICE LOCK " & (aanLocksMade(nPlayer, eMballIce) + 1))
					aanLocksLit(nPlayer,eMBallIce) = aanLocksLit(nPlayer,eMBallIce) + 1
					IceLockBall
					nAwardGiven = eMysteryMballIce
				ElseIf 4 = nLevel And nRemaining > 1 Then
					If sAdvance = "" Then
						sAdvance = "ICE"
					Else
						If sAdvance = "CYBER" or sAdvance = "AIR" Then
							asBagAward = Array("ADVANCE " & sAdvance  & " +", _
							"ICE LOCK " & (aanLocksMade(nPlayer, eMBallIce) + 1))
						else
							asBagAward = Array("LOCK " & sAdvance  & " +", _
							"ICE LOCK " & (aanLocksMade(nPlayer, eMBallIce) + 1))
						End If
						nAwardGiven = eMysteryMballIce
					End If
					aanLocksLit(nPlayer,eMBallIce) = aanLocksLit(nPlayer,eMBallIce) + 1
					IceLockBall
				Elseif 5 = nLevel And nRemaining > 2 Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "ICE MULTIBALL")
					IceMballStart
					nAwardGiven = eMysteryMballIce
				End If
			Elseif nAwardConsidered = eMysteryPlayfieldX and sAdvance = "" Then
				If 1 = nLevel Then
					for j = 0 to 3
						if Not avXtargets(nPlayer).Contains(j) Then
							UpdateXTargets j
							Exit For
						End If
					Next
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "SPOT X TARGET")
					nAwardGiven = eMysteryPlayfieldX
				Elseif 2 = nLevel Then
					nCount = 0
					for j = 0 to 3
						if Not avXtargets(nPlayer).Contains(j) Then
							UpdateXTargets j
							nCount = nCount + 1
							If nCount > 1 Then Exit For
						End If
					Next
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "SPOT X TARGETS")
					nAwardGiven = eMysteryPlayfieldX
				Elseif 3 = nLevel And avXtargets(nPlayer).Count < 3 Then
					for j = 0 to 2
						avXtargets(nPlayer).Add j
					Next
					UpdateXTargets 3
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "")
					Select Case nPlayfieldX
						Case 2
							asBagAward(1) = "3X SCORE LIT"
						Case 3
							asBagAward(1) = "3X EXTENSION LIT"
						Case Else
							asBagAward(1) = "2X SCORE LIT"
					End Select
					nAwardGiven = eMysteryPlayfieldX
				Elseif 4 = nLevel And nPlayfieldX < 3 Then
					avXTargets(nPlayer).Clear
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "")
					Select Case nPlayfieldX
						Case 2
							asBagAward(1) = "3X SCORE 30 SEC"
						Case 3
							asBagAward(1) = "EXTEND 3X SCORE"
						Case Else
							asBagAward(1) = "2X SCORE 30 SEC"
					End Select
					nPlayfieldX = nPlayfieldX + 1
					If nPlayfieldX > 3 then nPlayfieldX = 3
					nPlayfieldXTimer = 32000
					nAwardGiven = eMysteryPlayfieldX
				Elseif 5 = nLevel Then
					avXTargets(nPlayer).Clear
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "")
					Select Case nPlayfieldX
						Case 3
							asBagAward(1) = "EXTEND 3X SCORE"
						Case Else
							asBagAward(1) = "3X SCORE 30 SEC"
					End Select
					nPlayfieldX = 3
					nPlayfieldXTimer = 32000
					nAwardGiven = eMysteryPlayfieldX
				End If
			Elseif nAwardConsidered = eMysteryBonusX and sAdvance = "" Then
				If 1 = nLevel and nBonusX < 10 Then
					nBonusX = nBonusX + 1
					nAwardGiven = eMysteryBonusX
				Elseif 2 = nLevel and nBonusX < 9 Then
					nBonusX = nBonusX + 2
					nAwardGiven = eMysteryBonusX
				Elseif 3 = nLevel and nBonusX < 8 Then
					nBonusX = nBonusX + 3
					nAwardGiven = eMysteryBonusX
				Elseif 4 = nLevel and nBonusX < 7 Then
					nBonusX = nBonusX + 4
					nAwardGiven = eMysteryBonusX
				Elseif 5 = nLevel and nBonusX < 5 Then
					nBonusX = nBonusX + 6
					nAwardGiven = eMysteryBonusX
				End If
				If eMysteryBonusX = nAwardGiven Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer),_
						"BONUS NOW " & nBonusX & "X")
				End If
			Elseif nAwardConsidered = eMysteryBallSave and sAdvance = "" _
			and (not avModesRunning(nPlayer).Contains(eModeTotalMayhem)) Then
				If 1 = nLevel Then
					nTimeBallSave = nTimeBallSave + 10500
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "8 SEC BALL SAVE")
				Elseif 2 = nLevel Then
					nTimeBallSave = nTimeBallSave + 14500
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "12 SEC BALL SAVE")
				Elseif 3 = nLevel Then
					nTimeBallSave = nTimeBallSave + 18500
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "16 SEC BALL SAVE")
				Elseif 4 = nLevel Then
					nTimeBallSave = nTimeBallSave + 26500
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "24 SEC BALL SAVE")
				Elseif 5 = nLevel Then
					asBagAward = Array("BAG OF TRICKS " & anBagLevel(nPlayer), "M-BALL EXTENDER")
					anMballExtenders(nPlayer) = anMballExtenders(nPlayer) + 1
				End If
				nAwardGiven = eMysteryBallSave
			Elseif nAwardConsidered = eMysteryUpgrade and sAdvance = "" and anBagLevel(nPlayer) < 7 Then
				anBagLevel(nPlayer) = anBagLevel(nPlayer) + 2
				asBagAward = Array("BAG OF TRICKS ", "LEVEL " & anBagLevel(nPlayer) & " IS LIT")
				nAwardGiven = eMysteryUpgrade
			Elseif 11 = i Or (nAwardConsidered = eMysteryPoints and sAdvance = "") Then
				If 1 = nLevel Then
					nCount = (6000 * nPlayfieldX * anComboX(eShotUnderFlipper))
				Elseif 2 = nLevel Then
					nCount = (10000 * nPlayfieldX * anComboX(eShotUnderFlipper))
				Elseif 3 = nLevel Then
					nCount = (15000 * nPlayfieldX * anComboX(eShotUnderFlipper))
				Elseif 4 = nLevel Then
					nCount = (25000 * nPlayfieldX * anComboX(eShotUnderFlipper))
				Elseif 5 = nLevel Then
					nCount = (40000 * nPlayfieldX * anComboX(eShotUnderFlipper))
				End If
				anScore(nPlayer) = anScore(nPlayer) + nCount
				asBagAward = Array("BAG OF TRICKS ", WilliamsFormatNum(nCount) & " POINTS")
				nAwardGiven = eMysteryPoints
			End If
		End If
		If nAwardGiven <> eMysteryNone Then
			If bSkipAnimation Then
				ShowText asBagAward(0), asBagAward(1), 2500, eVOPrioMystery
			Else
				if Not IsMultiballActive Then LightShowStart eLightShowMystery, eVOPrioMystery
				nTimeBagAnimation = 3000
				asBagAward(0) = CenterTextOnDisplay(asBagAward(0))
				asBagAward(1) = CenterTextOnDisplay(asBagAward(1))
			End If
			Exit For
		Else
			nAwardConsidered = (nAwardConsidered + 1) Mod 11
		End If
	Next
	If nAwardGiven <> eMysteryUpgrade Then
		aanLastBagAwards(nPlayer, anBagAwards(nPlayer) Mod 3) = nAwardGiven
		anBagAwards(nPlayer) = anBagAwards(nPlayer) + 1
		anBagLevelSum(nPlayer) = anBagLevelSum(nPlayer) + anBagLevel(nPlayer)
		anBagLevel(nPlayer) = 0
	End If
End Sub

Sub AddModeColor(nShot, nColor)
	avModeColors(nShot).add nColor
End Sub

Sub RemoveModeColor(nShot, nColor)
	Dim nlampNr
	avModeColors(nShot).remove nColor
	If avModeColors(nShot).Count < 1 Then
		aoShotLightsMode(nShot).state = 0
		nlampNr = (nShot * 2) + 1
		Lampz.SetLamp nlampNr, 0
	End If
End Sub

Sub ClearModeColor(nShot)
	avModeColors(nShot).Clear
	aoShotLightsMode(nShot).state = 0
End Sub

Sub AddMballColor(nShot, nColor)
	avMBallColors(nShot).add nColor
End Sub

Sub RemoveMballColor(nShot, nColor)
	Dim nlampNr
	avMBallColors(nShot).Remove nColor
	If avMBallColors(nShot).Count < 1 Then
		aoShotLightsMBall(nShot).state = 0
		nlampNr = (nShot * 2) + 2
		Lampz.SetLamp nlampNr, 0
	End If
End Sub

Sub ClearMballColor(nShot)
	avMBallColors(nShot).Clear
	aoShotLightsMball(nShot).state = 0
End Sub

Sub TimerRainbow_Timer
	Dim nTick, nColor, i
'	sTimersRun = sTimersRun + " TimerRainbow"
	nTick = GameTime mod 1536
	If nPlayersInGame = 0 then exit sub ' Don't update colors during attract mode
	If True = bDrainingBalls Or True = bCountingBonus Then exit sub
	
	If nTick < 256 Then ' Red to yellow
		nColor = RGB(255, nTick, 0)
	Elseif nTick < 512 Then ' Yellow to green
		nColor = RGB(512 - nTick, 255, 0)
	Elseif nTick < 768 Then ' Green to cyan
		nColor = RGB(0, 255, nTick - 512)
	Elseif nTick < 1024 Then ' cyan to blue
		nColor = RGB(0, 1024 - nTick, 255)
	Elseif nTick < 1280 Then ' blue to magenta
		nColor = RGB(nTick - 1024, 0, 255)
	Else ' magenta to red
		nColor = RGB(255, 0, 1536 - nTick)
	End if
	For i = 0 to 7
		If avModeColors(i).Contains(eColorRainbow) Then
			ApplyLampRGB 1 + (i * 2), nColor
'			aoShotLightsMode(i).color = nColor
'			aoShotLightsMode(i).colorFull = nColor
		End If
	Next
	For i = 0 to 7
		If avMballColors(i).Contains(eColorRainbow) Then
			ApplyLampRGB 2 + (i * 2), nColor
'			aoShotLightsMBall(i).color = nColor
'			aoShotLightsMBall(i).colorFull = nColor
		End If
	Next
End Sub

Sub TimerBlinkSlow_Timer
	Dim i
'	sTimersRun = sTimersRun + " TimerBlinkSlow"
	If nPlayersInGame = 0 then exit sub ' Don't update colors during attract mode
	If True = bDrainingBalls Or True = bCountingBonus Then exit sub

	' Cyber multiball
	LightSuperCyber.state = LightStateOff
	If avModesRunning(nPlayer).Contains(eModeCyberMBall) Then
		If vSuperJPsLit.Contains(eMballCyber) Then
			LightSuperCyber.state = LightStateBlinking
		ElseIf avSJCollected(nPlayer).Contains(eMballCyber) Then
			LightSuperCyber.state = LightStateOn
		End If
	Elseif avModesRunning(nPlayer).Contains(eModeEvilUnited) _
	And vSuperJPsLit.Contains(eMballCyber) Then
		LightSuperCyber.state = LightStateBlinking
	ElseIf avSJCollected(nPlayer).Contains(eMballCyber) Then
		LightSuperCyber.state = LightStateOn
	End If

	' Water multiball
	LightSuperWater.state = LightStateOff
	If avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
		If vSuperJPsLit.Contains(eMBallWater) Then
			LightSuperWater.state = LightStateBlinking
		ElseIf avSJCollected(nPlayer).Contains(eMBallWater) Then
			LightSuperWater.state = LightStateOn
		End If
	Elseif avModesRunning(nPlayer).Contains(eModeEvilUnited) _
	And vSuperJPsLit.Contains(eMBallWater) Then
		LightSuperWater.state = LightStateBlinking
	ElseIf avSJCollected(nPlayer).Contains(eMBallWater) Then
		LightSuperWater.state = LightStateOn
	End If
	For each i in aoWaterLockLights : i.state = LightStateOff : Next
	For i = 0 to 2
		if IsWizardModeActive Then
			aoWaterLockLights(i).state = LightStateOff
		Elseif (i < aanLocksMade(nPlayer, eMBallWater)) _
		Or avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
			aoWaterLockLights(i).state = LightStateOn
		Elseif i < (aanLocksMade(nPlayer, eMBallWater) + aanLocksLit(nPlayer, eMBallWater)) Then
			aoWaterLockLights(i).state = LightStateBlinking
		Elseif IsWizardModeActive Then
			aoWaterLockLights(i).state = LightStateOff
		End If
	Next

	' Fire multiball
	LightSuperFire.state = LightStateOff
	If avModesRunning(nPlayer).Contains(eModeFireMBall) Then
		If vSuperJPsLit.Contains(eMballFire) Then
			LightSuperFire.state = LightStateBlinking
		ElseIf avSJCollected(nPlayer).Contains(eMballFire) Then
			LightSuperFire.state = LightStateOn
		End If
	Elseif avModesRunning(nPlayer).Contains(eModeEvilUnited) _
	And vSuperJPsLit.Contains(eMballFire) Then
		LightSuperFire.state = LightStateBlinking
	ElseIf avSJCollected(nPlayer).Contains(eMballFire) Then
		LightSuperFire.state = LightStateOn
	End If
	For each i in aoFireLockLights : i.state = LightStateOff : Next
	For i = 0 to 2
		if IsWizardModeActive Then
			aofireLockLights(i).state = LightStateOff
		ElseIf i < aanLocksMade(nPlayer, eMballFire) Then
			aoFireLockLights(i).state = LightStateOn
		Elseif i < (aanLocksMade(nPlayer, eMballFire) + aanLocksLit(nPlayer, eMballFire)) Then
			aoFireLockLights(i).state = LightStateBlinking
		End If
	Next

	' Earth multiball
	LightSuperEarth.state = LightStateOff
	If avModesRunning(nPlayer).Contains(eModeEarthMBall) Then
		If aanMballJackpots(nPlayer, eMballEarth) >= anJackpotsForSuper(eMballEarth) Then
			LightSuperEarth.state = LightStateBlinking
		ElseIf avSJCollected(nPlayer).Contains(eMballEarth) Then
			LightSuperEarth.state = LightStateOn
		End If
	Elseif avModesRunning(nPlayer).Contains(eModeEvilUnited) _
	And vSuperJPsLit.Contains(eMballEarth) Then
		LightSuperEarth.state = LightStateBlinking
	ElseIf avSJCollected(nPlayer).Contains(eMballEarth) Then
		LightSuperEarth.state = LightStateOn
	End If
	For each i in aoEarthLockLights : i.state = LightStateOff : Next
	For i = 0 to 2
		if IsWizardModeActive Then
			aoEarthLockLights(i).state = LightStateOff
		ElseIf i < aanLocksMade(nPlayer, eMballEarth) Then
			aoEarthLockLights(i).state = LightStateOn
		Elseif i < (aanLocksMade(nPlayer, eMballEarth) + aanLocksLit(nPlayer, eMballEarth)) Then
			aoEarthLockLights(i).state = LightStateBlinking
		End If
	Next

	' Letters for lighting Air multiball
	For i = 0 to 7
		aoAerobaseLights(i).state = LightStateOff
		if IsWizardModeActive Then
			aoAerobaseLights(i).state = LightStateOff
		ElseIf avAirLettersHit(nPlayer).Contains(i) Then
			aoAerobaseLights(i).state = LightStateOn
		Elseif avAirLettersLit(nPlayer).Contains(i) Then
			aoAerobaseLights(i).state = LightStateBlinking
		End If
	Next

	' Air multiball
	LightSuperAir.state = LightStateOff
	If avModesRunning(nPlayer).Contains(eModeAirMBall) Then
		If vSuperJPsLit.Contains(eMBallAir) Then
			LightSuperAir.state = LightStateBlinking
		ElseIf avSJCollected(nPlayer).Contains(eMBallAir) Then
			LightSuperAir.state = LightStateOn
		End If
	Elseif avModesRunning(nPlayer).Contains(eModeEvilUnited) _
	And vSuperJPsLit.Contains(eMballAir) Then
		LightSuperAir.state = LightStateBlinking
	ElseIf avSJCollected(nPlayer).Contains(eMBallAir) Then
		LightSuperAir.state = LightStateOn
	End If

	' Ice multiball
	LightSuperIce.state = LightStateOff
	If avModesRunning(nPlayer).Contains(eModeIceMBall) Then
		If aanMballJackpots(nPlayer, eMballIce) >= anJackpotsForSuper(eMballIce) Then
			LightSuperIce.state = LightStateBlinking
		ElseIf avSJCollected(nPlayer).Contains(eMballIce) Then
			LightSuperIce.state = LightStateOn
		End If
	Elseif avModesRunning(nPlayer).Contains(eModeEvilUnited) _
	And vSuperJPsLit.Contains(eMballIce) Then
		LightSuperIce.state = LightStateBlinking
	ElseIf avSJCollected(nPlayer).Contains(eMballIce) Then
		LightSuperIce.state = LightStateOn
	End If
	For each i in aoIceLockLights : i.state = LightStateOff : Next
	For i = 0 to 2
		if IsWizardModeActive Then
			' Do nothing, lights remain off
		ElseIf i < aanLocksMade(nPlayer, eMballIce) Then
			aoIceLockLights(i).state = LightStateOn
		Elseif i < (aanLocksMade(nPlayer, eMballIce) + aanLocksLit(nPlayer, eMballIce)) Then
			aoIceLockLights(i).state = LightStateBlinking
		End If
	Next

	' Mode lights
	If False = bSelectingMode Or (False = IsMultiballActive _
	and False = bInMode and avWizardModesLit(nPlayer).Count > 0) Then
		If False = IsMultiballActive and False = bInMode _
		And avWizardModesLit(nPlayer).Count > 0 Then
			for each i in an1BallModes
				If avModesPlayed(nPlayer).Contains(i) Then
					aoLightForMode(i).state = LightStateOn
				Else
					aoLightForMode(i).state = LightStateOff
				End If
			Next
			If avWizardModesLit(nPlayer).Contains(eModeTotalMayhem) _
			Or avWizardModesLit(nPlayer).Contains(eModeEvilUnited) Then
				for each i in an1BallModes
					aoLightForMode(i).state = LightStateBlinking
				Next
			Elseif avWizardModesLit(nPlayer).Contains(eModeMegaWizard) Then
				for each i in an1BallModes
					aoLightForMode(i).state = LightStateBlinking
				Next
				For each i in aoSuperJackLights : i.state = LightStateOff : Next
				For each i in aoSuperJackLights : i.state = LightStateBlinking : Next
			Else
				if avWizardModesLit(nPlayer).Contains(eModeCyberWizard) Then
					aoLightForMode(eModeCyber1).state = LightStateBlinking
					aoLightForMode(eModeCyber2).state = LightStateBlinking
					LightSuperCyber.state = LightStateOff
					LightSuperCyber.state = LightStateBlinking
				End If
				If avWizardModesLit(nPlayer).Contains(eModeWaterWizard) Then
					aoLightForMode(eModeWater1).state = LightStateBlinking
					aoLightForMode(eModeWater2).state = LightStateBlinking
					LightSuperWater.state = LightStateOff
					LightSuperWater.state = LightStateBlinking
				End If
				If avWizardModesLit(nPlayer).Contains(eModeFireWizard) Then
					aoLightForMode(eModeFire1).state = LightStateBlinking
					aoLightForMode(eModeFire2).state = LightStateBlinking
					LightSuperFire.state = LightStateOff
					LightSuperFire.state = LightStateBlinking
				End If
				If avWizardModesLit(nPlayer).Contains(eModeEarthWizard) Then
					aoLightForMode(eModeEarth1).state = LightStateBlinking
					aoLightForMode(eModeEarth2).state = LightStateBlinking
					LightSuperEarth.state = LightStateOff
					LightSuperEarth.state = LightStateBlinking
				End If
				If avWizardModesLit(nPlayer).Contains(eModeAirWizard) Then
					aoLightForMode(eModeAir1).state = LightStateBlinking
					aoLightForMode(eModeAir2).state = LightStateBlinking
					LightSuperAir.state = LightStateOff
					LightSuperAir.state = LightStateBlinking
				End If
				If avWizardModesLit(nPlayer).Contains(eModeIceWizard) Then
					aoLightForMode(eModeIce1).state = LightStateBlinking
					aoLightForMode(eModeIce2).state = LightStateBlinking
					LightSuperIce.state = LightStateOff
					LightSuperIce.state = LightStateBlinking
				End If
			End If
		Else
			for each i in an1BallModes
				aoLightForMode(i).state = LightStateOff
				If avModesPlayed(nPlayer).Contains(i) Then
					aoLightForMode(i).state = LightStateOn
				Elseif avModesRunning(nPlayer).Contains(i) Then
					aoLightForMode(i).state = LightStateBlinking
				End If
			Next
			If avModesRunning(nPlayer).Contains(eModeTotalMayhem) _
			Or avModesRunning(nPlayer).Contains(eModeEvilUnited) Then
				for each i in an1BallModes
					aoLightForMode(i).state = LightStateBlinking
				Next
			Elseif avModesRunning(nPlayer).Contains(eModeMegaWizard) Then
				for each i in an1BallModes
					aoLightForMode(i).state = LightStateBlinking
				Next
				For each i in aoSuperJackLights : i.state = LightStateOff : Next
				For each i in aoSuperJackLights : i.state = LightStateBlinking : Next
			Elseif avModesRunning(nPlayer).Contains(eModeCyberWizard) Then
				aoLightForMode(eModeCyber1).state = LightStateBlinking
				aoLightForMode(eModeCyber2).state = LightStateBlinking
				LightSuperCyber.state = LightStateOff
				LightSuperCyber.state = LightStateBlinking
			Elseif avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
				aoLightForMode(eModeWater1).state = LightStateBlinking
				aoLightForMode(eModeWater2).state = LightStateBlinking
				LightSuperWater.state = LightStateOff
				LightSuperWater.state = LightStateBlinking
			Elseif avModesRunning(nPlayer).Contains(eModeFireWizard) Then
				aoLightForMode(eModeFire1).state = LightStateBlinking
				aoLightForMode(eModeFire2).state = LightStateBlinking
				LightSuperFire.state = LightStateOff
				LightSuperFire.state = LightStateBlinking
			Elseif avModesRunning(nPlayer).Contains(eModeEarthWizard) Then
				aoLightForMode(eModeEarth1).state = LightStateBlinking
				aoLightForMode(eModeEarth2).state = LightStateBlinking
				LightSuperEarth.state = LightStateOff
				LightSuperEarth.state = LightStateBlinking
			Elseif avModesRunning(nPlayer).Contains(eModeAirWizard) Then
				aoLightForMode(eModeAir1).state = LightStateBlinking
				aoLightForMode(eModeAir2).state = LightStateBlinking
				LightSuperAir.state = LightStateOff
				LightSuperAir.state = LightStateBlinking
			Elseif avModesRunning(nPlayer).Contains(eModeIceWizard) Then
				aoLightForMode(eModeIce1).state = LightStateBlinking
				aoLightForMode(eModeIce2).state = LightStateBlinking
				LightSuperIce.state = LightStateOff
				LightSuperIce.state = LightStateBlinking
			End If
		End If
	End If
End Sub

Sub TimerBlinkChars_Timer
	Dim i, nState
	dim anMballModes
'	sTimersRun = sTimersRun + " TimerBlinkChars"
	If nPlayersInGame = 0 then exit sub ' Don't update colors during attract mode
	If True = bDrainingBalls Or True = bCountingBonus Then exit sub

	If nCharSpeaking <> eCharacterNone and nTimeDelayCallout < 1 Then
		Select Case nCharBlinkCycle
			Case 0, 1, 3, 4, 6, 7, 9, 10
				If eCharacterHero = nCharSpeaking Then
					Lampz.SetLamp 94, 0
				Else
					aoCharLights(nCharSpeaking).state = LightStateOff
				End If
			Case Else
				If eCharacterHero = nCharSpeaking Then
					Lampz.SetLamp 94, 1
				Else
					aoCharLights(nCharSpeaking).state = LightStateOn
				End If
		End Select
	End If
	If BIP > 1 Then
		anMballModes = Array (eModeCyberMBall, eModeWaterMBall, _
			eModeFireMBall, eModeEarthMBall, eModeAirMBall, eModeIceMBall)
		Select Case nCharBlinkCycle
			Case 0, 1, 4, 5, 8, 9
				nState = 0
			Case Else
				nState = 1
		End Select

		For i = eCharacterCyber to eCharacterIce
			If nCharSpeaking <> i Then
				If avModesRunning(nPlayer).Contains(anMballModes(i)) Then
					aoCharLights(i).state = nState
				Else
					aoCharLights(i).state = LightStateOff
				End If
			End If
		Next

		If eCharacterHero <> nCharSpeaking Then
			Lampz.SetLamp 94, 0
		End If
	Else
		For i = eCharacterCyber to eCharacterIce
			If nCharSpeaking <> i Then
				aoCharLights(i).state = LightStateOff
			End If
		Next
		If eCharacterHero <> nCharSpeaking Then
			Lampz.SetLamp 94, 0
		End If
	End If
	nCharBlinkCycle = (nCharBlinkCycle + 1) Mod 12
end Sub

Sub TimerBlinkMedium_Timer
	Dim i, j
	Dim nProgress, nFirstLight, nLastLight
'	sTimersRun = sTimersRun + " TimerBlinkMedium"
	If nPlayersInGame = 0 then exit sub ' Don't update colors during attract mode
	If True = bDrainingBalls Or True = bCountingBonus Then exit sub

	' Water Targets
	If avModesRunning(nPlayer).Contains(eModeCyber1) _
	And (aanModeShots(nPlayer, eModeCyber1) Mod 2 = 0) _
	And (Not avCyber1TargetsHit(nPlayer).Contains(eColorBlue)) Then
		' Do nothing, the mode code handles lights in this case
	ElseIf avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
		lightTorpedo.state = LightStateOff
		If nWaterJackpots < 7 Then
			lightTorpedo.state = LightStateBlinking
		End If
	Elseif IsWizardModeActive Then
		lightTorpedo.state = LightStateOff
	Else
		lightTorpedo.state = LightStateOff
		If aanLocksLit(nPlayer, eMBallWater) < 1 Then
			lightTorpedo.state = LightStateBlinking
		End If
	End If

	' Ice Targets
	Dim nLockNumber
	nLockNumber = aanLocksMade(nPlayer, eMballIce) + aanLocksLit(nPlayer, eMballIce)
	if IsWizardModeActive Then
		If avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
			' Do nothing, the mode handles the lights
		ElseIf avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
			' Do nothing, the mode handles the lights
		Else
			For Each i in aoDropLights : i.state = LightStateOff : Next
		End If
	ElseIf (0 = aanMballsPlayed(nplayer, eMballIce) and nLockNumber < 3) _
	Or (aanMballsPlayed(nplayer, eMballIce) > 0 And aanLocksLit(nPlayer, eMballIce) < 1) _
	Or (avModesRunning(nPlayer).Contains(eModeIceMBall) And vIceJackpots.Count < 7) Then
		If bSelectingMode Then
			' Do nothing, the selection handles the lights
		ElseIf avModesRunning(nPlayer).Contains(eModeCyber1) _
		And (aanModeShots(nPlayer, eModeCyber1) Mod 2 = 0) _
		And (Not avCyber1TargetsHit(nPlayer).Contains(eColorWhite)) Then
			' Do nothing, the mode handles the lights
		ElseIf avModesRunning(nPlayer).Contains(eModeIce2) _
		And (aanModeShots(nPlayer, eModeIce2) > 8) Then
			' Do nothing, the mode handles the lights
		ElseIf nDropResetTimer > 0 and nDropResetTimer < 3001 Then
			For Each i in aoDropLights
				i.state = LightStateOff
				i.blinkInterval = 85
				i.state = LightStateBlinking
			Next
		Else
			For Each i in aoDropLights : i.state = LightStateOff : Next
			If drop1.isDropped or avDropsHit(nPlayer).contains(0) then
				lightIce001.state = LightStateOn
			Else
				lightIce001.blinkInterval = 170
				lightIce001.state = LightStateBlinking
			End If
			If drop2.isDropped or avDropsHit(nPlayer).contains(1) then
				lightIce002.state = LightStateOn
			Else
				lightIce002.blinkInterval = 170
				lightIce002.state = LightStateBlinking
			End If
			If drop3.isDropped or avDropsHit(nPlayer).contains(2) then
				lightIce003.state = LightStateOn
			Else
				lightIce003.blinkInterval = 170
				lightIce003.state = LightStateBlinking
			End If
		End If
	Else
		For Each i in aoDropLights : i.state = LightStateOn : Next
	End If

	' X Lights

	nProgress = aanModeShots(nPlayer, eModeIce2)
	If avModesRunning(nPlayer).Contains(eModeIce2) _
	And avModesPlayed(nPlayer).Contains(eModeIce1) Then
		If 3 = nProgress or 4 = nProgress Then
			nFirstLight = 0 : nLastLight = 1
		ElseIf 8 = nProgress or 9 = nProgress Then
			nFirstLight = 2 : nLastLight = 3
		Else
			nFirstLight = 0 : nLastLight = 3
		End If
	ElseIf avModesRunning(nPlayer).Contains(eModeCyber1) _
	And (aanModeShots(nPlayer, eModeCyber1) Mod 2 = 0) _
	And (Not avCyber1TargetsHit(nPlayer).Contains(eColorYellow)) Then
		nFirstLight = -1
		' Do nothing, the mode code handles lights in this case
	ElseIf avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		nFirstLight = -1
		' Do nothing, the mode handles the lights
	ElseIf avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		nFirstLight = -1
		' Do nothing, the mode handles the lights
	Else
		nFirstLight = 0 : nLastLight = 3
	End If
	If nFirstLight > -1 Then 
		For i = nFirstLight to nLastLight
			If gameTime - anSwitchLastHit(anXTargetSwitches(i)) > 500 then
				If avXtargets(nPlayer).Contains(i) Then
					aoXLights(i).state = LightStateOn
				Else
					aoXLights(i).state = LightStateOff
					aoXLights(i).blinkInterval = 170
					If nPlayfieldX < 3 Or 0 = i Or 2 = i Then
						aoXLights(i).state = LightStateBlinking
					elseif avXtargets(nPlayer).Contains(i - 1) Then
						aoXLights(i).state = LightStateBlinking
					End If
				End If
			End If
		Next
	End if
	LightScoreX.state = LightStateOff
	If avXtargets(nPlayer).isFull Then LightScoreX.state = LightStateBlinking
	LightScore2X.state = LightStateOff : LightScore3X.state = LightStateOff
	If 2 = nPlayfieldX Then
		If nPlayfieldXTimer > 1999 and nPlayfieldXTimer < 6000 Then
			LightScore2X.blinkInterval = 85
			LightScore2X.state = LightStateBlinking
		Elseif nPlayfieldXTimer > 5999 Then
			LightScore2X.blinkInterval = 170
			LightScore2X.state = LightStateBlinking
		End If
	Elseif 3 = nPlayfieldX Then
		LightScore2X.state = LightStateOn
		If nPlayfieldXTimer > 1999 and nPlayfieldXTimer < 6000 Then
			LightScore3X.blinkInterval = 85
			LightScore3X.state = LightStateBlinking
		Elseif nPlayfieldXTimer > 5999 Then
			LightScore3X.blinkInterval = 170
			LightScore3X.state = LightStateBlinking
		Else
			LightScore2X.state = LightStateOff
			LightScore3X.state = LightStateOff
		End If
	End If

	' Mystery Lights
	If avModesRunning(nPlayer).Contains(eModeCyber1) _
		And (aanModeShots(nPlayer, eModeCyber1) Mod 2 = 0) Then
			' Do nothing, the mode code handles lights in this case
	ElseIf avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		' Do nothing, the mode handles the lights
	ElseIf avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		' Do nothing, the mode handles the lights
	Else
		For i = 0 to 3
			If gameTime - anSwitchLastHit(anBagTargetSwitches(i)) > 500 then
				If avBagtargets(nPlayer).Contains(i) Then
					aoBagLights(i).state = LightStateOn
				Else
					aoBagLights(i).state = LightStateOff
					If anBagLevel(nPlayer) < 9 Then
						aoBagLights(i).blinkInterval = 170						
						aoBagLights(i).state = LightStateBlinking
					End If
				End If
			End if
		Next
	End if
	lightMystery.state = LightStateOff
	If (anBagLevel(nPlayer) > 0) And (Not avModesRunning(nPlayer).Contains(eModeTotalMayhem)) Then
		lightMystery.state = LightStateBlinking
	End If
end Sub

Sub TimerBlinkFast_Timer
	Dim i, j
	Dim nProgress, nFirstLight, nLastLight
'	sTimersRun = sTimersRun + " TimerBlinkFast"
	If nPlayersInGame = 0 then exit sub ' Don't update colors during attract mode
	If True = bDrainingBalls Or True = bCountingBonus Then exit sub

	For each i in Array(True, False)
		For j = 0 to 7
			UpdateArrowColor i, j
		Next
	Next

	nColorCycleIndex = nColorCycleIndex + 1
	nColorCycleIndex = nColorCycleIndex mod 60

	' COmbo lights
	for i = 0 to 7
		If avPermaCombo(nPlayer).Contains(i) Then
			aoComboLights(i).state = LightStateOn
		Else
			aoComboLights(i).state = LightStateOff
			If anComboTimer(i) > 999 then aoComboLights(i).state = LightStateBlinking
		End If
	Next

	' Ball Save
	If nExtraBalls > 0 Then
		lightShootAgain.state = LightStateOn
	Else
		lightShootAgain.state = LightStateOff
	End If
	If avModesRunning(nPlayer).Contains(eModeTotalMayhem) Then
		If (nTimeMayhem > 1999 And nTimeMayhem < 4000) Then
			lightShootAgain.BlinkInterval = 65
			lightShootAgain.state = LightStateBlinking
		Elseif nTimeMayhem > 3999 Then
			lightShootAgain.BlinkInterval = 130
			lightShootAgain.state = LightStateBlinking
		End If
	Else
		If nTimeBallSave > 2499 and nTimeBallSave < 4500 Then
			lightShootAgain.BlinkInterval = 65
			lightShootAgain.state = LightStateBlinking
		Elseif nTimeBallSave > 4499 Then
			lightShootAgain.BlinkInterval = 130
			lightShootAgain.state = LightStateBlinking
		End If
	End If

	' Extra ball
	LightExtraBall.state = LightStateOff
	If anExtraBallslit(nPlayer) > 0 Then
		LightExtraBall.state = LightStateBlinking
	End If

End Sub

Sub UpdateArrowColor(isMode, nShot)
	Dim i
	Dim oLight, nLampNr
	Dim nColorCount
	Dim vColors
	Dim aActiveColors(8)
	Dim nColorToUse, nColor
	Dim bOff
	If isMode Then
		Set oLight = aoShotLightsMode(nShot)
		Set vColors = avModeColors(nShot)
		nLampNr = 1 + (nShot * 2)
	Else
		Set oLight = aoShotLightsMBall(nShot)
		Set vColors = avMballColors(nShot)
		nLampNr = 2 + (nShot * 2)
	End If

	nColorCount = 0
	for i = 0 to nColorsInCycle - 1 ' colors
		if vColors.Contains(i) Then
			aActiveColors(nColorCount) = i
			nColorCount = nColorCount + 1
		end If
	next
	If nColorCount > 0 Then
		nColorToUse = nColorCycleIndex mod nColorCount
		nColor = aActiveColors(nColorToUse)
	End If
	oLight.state = LightStateOff
	bOff = False
	If True = avModesRunning(nPlayer).Contains(eModeCyberWizard) _
	And 1 = aanCyber3ShotsHit(nPlayer, nShot) and True = isMode Then
		oLight.state = LightStateOn
	ElseIf True = avModesRunning(nPlayer).Contains(eModeEvilUnited) _
	And 1 = anEvilUnitedShots(nShot) and True = isMode Then
		oLight.state = LightStateOn
	ElseIf vColors.Contains(eColorRainbow) Then
		oLight.state = LightStateOn
	ElseIf nColorCount > 0 Then
		oLight.BlinkInterval = 130
'		oLight.color = anColorCycleDark(nColor)
'		oLight.colorFull = anColorCycleLight(nColor)
		If avModesRunning(nPlayer).Contains(eModeFireMBall) And eColorRed = nColor Then
			If False = isMode and anFireSpreadTimer(nShot) > 0 _
			and anFireSpreadTimer(nShot) < 3001 Then
				oLight.blinkInterval = 75
			End If
		End If
		If eColorBlue = nColor And True = isMode _
		And True = avModesRunning(nPlayer).Contains(eModeWater2) Then
			If anTimeWater2(nShot) > 1 And anTimeWater2(nShot) < 2001 Then
				bOff = True
			ElseIf anTimeWater2(nShot) > 2000 And anTimeWater2(nShot) < 5001 Then
				oLight.blinkInterval = 65
			End If
		End If
		If eColorBlue = nColor And False = isMode _
		And False = avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
			If anTimerTorpedo(nShot) > 1 And anTimerTorpedo(nShot) < 2001 Then
				bOff = True
			ElseIf anTimerTorpedo(nShot) > 2000 And anTimerTorpedo(nShot) < 5001 Then
				oLight.blinkInterval = 65
			End If
		End If
		If Not bOff Then
			ApplyLampRGB nLampNr, anColorCycleLight(nColor)
			oLight.state = LightStateBlinking
		Else
			oLight.state = LightStateOff
			Lampz.SetLamp nLampNr, 0
		End If
	Else
		oLight.state = LightStateOff
		Lampz.SetLamp nLampNr, 0
	end if
End Sub

Sub AllLightsOff
	Dim i

	For each i in aoDigit0Lights : i.state = LightStateOff : Next
	For each i in aoDigit1Lights : i.state = LightStateOff : Next
	For each i in aoDigit2Lights : i.state = LightStateOff : Next
	For each i in aoShotLightsMode : i.state = LightStateOff : Next
	For each i in aoShotLightsMBall : i.state = LightStateOff : Next
	For each i in aoTopLaneLights : i.state = LightStateOff : Next
	For each i in aoFireLaneLights : i.state = LightStateOff : Next
	For each i in aoModeLights : i.state = LightStateOff : Next
	For each i in aoWaterLockLights : i.state = LightStateOff : Next
	For each i in aoFireLockLights : i.state = LightStateOff : Next
	For each i in aoEarthLockLights : i.state = LightStateOff : Next
	For each i in aoIceLockLights : i.state = LightStateOff : Next
	For each i in aoDropLights
		i.state = LightStateOff : i.BlinkInterval = 170 : i.BlinkPattern = "10"
	Next
	For each i in aoXLights
		i.state = LightStateOff : i.BlinkInterval = 170 : i.BlinkPattern = "10"
	Next
	For each i in aoBagLights
		i.state = LightStateOff : i.BlinkInterval = 170 : i.BlinkPattern = "10"
	Next
	For each i in aoSuperJackLights : i.state = LightStateOff : Next
	For each i in aoComboLights : i.state = LightStateOff : Next
	For each i in aoAerobaseLights : i.state = LightStateOff : Next
	For each i in aoMiscLights : i.state = LightStateOff : Next
	For each i in aoShotLightSeqs : i.stopPlay : Next
	For i = eCharacterCyber to eCharacterIce
		aoCharLights(i).state = LightStateOff
	Next
	Lampz.SetLamp 94, 0 ' hero

	lightTorpedo.BlinkInterval = 170
End Sub

Sub LightShowEnd
	Dim i
	TimerLightShow.Enabled = False
	LightSeqAlmostAll.StopPlay
	LightSeqEven.StopPlay
	LightSeqOdd.StopPlay
	LightSeqAlmostAll.UpdateInterval = 17
	LightSeqBonusCyber.StopPlay
	LightSeqBonusWater.StopPlay
	LightSeqBonusFire.StopPlay
	LightSeqBonusEarth.StopPlay
	LightSeqBonusAir.StopPlay
	LightSeqBonusIce.StopPlay
	' Reset GI to incandescent warm white
	For Each i in GI
		i.color = RGB(255, 147, 41)
		i.colorFull = RGB(255, 197, 143)
	Next
	nLightShowCurrent = eLightShowNone
	nLightShowTime = 0
	nLightShowPriority = eVOPrioIdle
End Sub

Dim nLightShowUpdates
Sub TimerLightShow_Timer
	nLightShowUpdates = nLightShowUpdates + 1
	nLightShowTime = nLightShowTime - TimerLightShow.interval
	If nLightShowTime < 1 Then
		LightShowEnd
	End If
	If eLightShowCyberLong = nLightShowCurrent Then
		FlashOnce 1 + (nLightShowUpdates Mod 5), eColorGreen
	Elseif eLightShowWaterLong = nLightShowCurrent Then
		FlashOnce 1 + (nLightShowUpdates Mod 5), eColorBlue
	Elseif eLightShowFireLong = nLightShowCurrent Then
		FlashOnce 1 + (nLightShowUpdates Mod 5), eColorRed
	Elseif eLightShowEarthLong = nLightShowCurrent Then
		FlashOnce 1 + (nLightShowUpdates Mod 5), eColorYellow
	Elseif eLightShowAirLong = nLightShowCurrent Then
		FlashOnce 1 + (nLightShowUpdates Mod 5), eColorPurple
	Elseif eLightShowIceLong = nLightShowCurrent Then
		FlashOnce 1 + (nLightShowUpdates Mod 5), eColorWhite
	End If
	If (eLightShowSuper = nLightShowCurrent) or (eLightShowMayhem = nLightShowCurrent) _
	or ((eLightShowModeComplete = nLightShowCurrent) and (nLightShowUpdates Mod 2 = 0)) Then
		LightSeqEven.stopPlay
		LightSeqOdd.stopPlay
		If bLightShowBlinkOdd Then
			LightSeqEven.play SeqAllOff
			LightSeqOdd.play SeqAllOn
			bLightShowBlinkOdd = False
		Else
			LightSeqOdd.play SeqAllOff
			LightSeqEven.play SeqAllOn
			bLightShowBlinkOdd = True
		End If
	End If
End Sub

Sub LightShowStart (nEffectNumber, nPriority)
	Dim i

	If eLightShowNone = nEffectNumber Then Exit Sub
	' If a higher or equal priority show is running, don't play the new show
	If nLightShowCurrent <> eLightShowNone Then
		If nLightShowPriority <= nPriority Then
			Exit Sub
		Else
	' If a lower priority show is running, interrupt it with the new show
			LightShowEnd
		End If
	End If
	
	Select Case nEffectNumber
		Case eLightShowCyberShort
			LightSeqAlmostAll.UpdateInterval = 11
			LightSeqAlmostAll.play SeqStripe1HorizOn,40
			nLightShowTime = 1000
		Case eLightShowCyberLong
			LightSeqAlmostAll.UpdateInterval = 33
			LightSeqAlmostAll.play SeqStripe1HorizOn,40
			nLightShowTime = 3000
			For Each i in GI
				i.color = RGB(0, 255, 0)
				i.colorFull = RGB(0, 255, 0)
			Next
		Case eLightShowWaterShort
			LightSeqAlmostAll.UpdateInterval = 5
			LightSeqAlmostAll.play SeqRadarRightOn, 40
			nLightShowTime = 1000
		Case eLightShowWaterLong
			LightSeqAlmostAll.UpdateInterval = 17
			LightSeqAlmostAll.play SeqRadarRightOn, 40
			nLightShowTime = 3000
			For Each i in GI
				i.color = RGB(0, 128, 255)
				i.colorFull = RGB(0, 128, 255)
			Next
		Case eLightShowFireShort
			LightSeqAlmostAll.UpdateInterval = 8
			LightSeqAlmostAll.play SeqUpOn, 40
			nLightShowTime = 1000
		Case eLightShowFireLong
			LightSeqAlmostAll.UpdateInterval = 22
			LightSeqAlmostAll.play SeqUpOn, 60
			nLightShowTime = 3000
			For Each i in GI
				i.color = RGB(255, 0, 0)
				i.colorFull = RGB(255, 0, 0)
			Next
		Case eLightShowEarthShort
			LightSeqAlmostAll.UpdateInterval = 8
			LightSeqAlmostAll.play SeqDownOn, 60
			nLightShowTime = 1000
		Case eLightShowEarthLong
			LightSeqAlmostAll.UpdateInterval = 22
			LightSeqAlmostAll.play SeqDownOn, 60
			nLightShowTime = 3000
			For Each i in GI
				i.color = RGB(255, 64, 0)
				i.colorFull = RGB(255, 64, 0)
			Next
		Case eLightShowAirShort
			LightSeqAlmostAll.UpdateInterval = 5
			LightSeqAlmostAll.play SeqDiagUpLeftOn, 40
			nLightShowTime = 1000
		Case eLightShowAirLong
			LightSeqAlmostAll.UpdateInterval = 17
			LightSeqAlmostAll.play SeqDiagUpLeftOn, 40
			nLightShowTime = 3000
			For Each i in GI
				i.color = RGB(128, 0, 255)
				i.colorFull = RGB(128, 0, 255)
			Next
		Case eLightShowIceShort
			LightSeqAlmostAll.UpdateInterval = 8
			LightSeqAlmostAll.play SeqCircleInOn, 40
			nLightShowTime = 1000
		Case eLightShowIceLong
			LightSeqAlmostAll.UpdateInterval = 22
			LightSeqAlmostAll.play SeqCircleInOn, 40
			nLightShowTime = 3000
			For Each i in GI
				i.color = RGB(192, 192, 255)
				i.colorFull = RGB(192, 192, 255)
			Next
		Case eLightShowXBegin
			LightSeqAlmostAll.UpdateInterval = 11
			LightSeqAlmostAll.play SeqFanLeftUpOn, 40
			nLightShowTime = 2000
			For Each i in GI
				i.color = vbYellow
				i.colorFull = vbYellow
			Next
		Case eLightShowMystery
			LightSeqAlmostAll.UpdateInterval = 8
			LightSeqAlmostAll.play SeqScrewLeftOn, 25, 999
			nLightShowTime = 3000
			For Each i in GI
				i.color = vbCyan
				i.colorFull = vbCyan
			Next
		Case eLightShowSuper, eLightShowModeComplete
			LightSeqEven.play SeqAllOn
			LightSeqOdd.play SeqAllOff
			bLightShowBlinkOdd = True
			nLightShowTime = 2500
		Case eLightShowMayhem
			LightSeqEven.play SeqAllOff
			LightSeqOdd.play SeqAllOn
			bLightShowBlinkOdd = True
			nLightShowTime = 1000
		Case eLightShowCyberBonus
			DOF 229, DOFPulse					
			LightSeqBonusCyber.play SeqBlinking,,4,125
			For Each i in GI
				i.color = RGB(0, 255, 0)
				i.colorFull = RGB(0, 255, 0)
			Next
			nLightShowTime = 1000
		Case eLightShowWaterBonus
			DOF 230, DOFPulse					
			LightSeqBonusWater.play SeqBlinking,,4,125
			For Each i in GI
				i.color = RGB(0, 128, 255)
				i.colorFull = RGB(0, 128, 255)
			Next
			nLightShowTime = 1000
		Case eLightShowFireBonus
			DOF 231, DOFPulse				
			LightSeqBonusFire.play SeqBlinking,,4,125
			For Each i in GI
				i.color = RGB(255, 0, 0)
				i.colorFull = RGB(255, 0, 0)
			Next
			nLightShowTime = 1000
		Case eLightShowEarthBonus
			DOF 232, DOFPulse					
			LightSeqBonusEarth.play SeqBlinking,,4,125
			For Each i in GI
				i.color = RGB(255, 64, 0)
				i.colorFull = RGB(255, 64, 0)
			Next
			nLightShowTime = 1000
		Case eLightShowAirBonus
			DOF 233, DOFPulse				
			LightSeqBonusAir.play SeqBlinking,,4,125
			For Each i in GI
				i.color = RGB(128, 0, 255)
				i.colorFull = RGB(128, 0, 255)
			Next
			nLightShowTime = 1000
		Case eLightShowIceBonus
			DOF 234, DOFPulse					
			LightSeqBonusIce.play SeqBlinking,,4,125
			For Each i in GI
				i.color = RGB(192, 192, 255)
				i.colorFull = RGB(192, 192, 255)
			Next
			nLightShowTime = 1000
		Case eLightShowBonusX
			DOF 235, DOFPulse					
			LightSeqAlmostAll.play SeqAllOn
			nLightShowTime = 1000
	End Select

	If nEffectNumber <> eLightShowNone Then
		nLightShowCurrent = nEffectNumber
		nLightShowPriority = nPriority
		TimerLightShow.Enabled = True
		nLightShowUpdates = 0
	End If
End Sub

'*** Voiceover helpers
Dim oVoiceTimeDict   ' Dict of voiceover lengths in ms
Dim oVoicePlaysDict  ' Dict of times played, used for round robins
Dim oVoiceVariations ' Dict of number of variations a callout has

Sub InitVoiceovers
	Set oVoiceTimeDict = CreateObject("Scripting.Dictionary")
	Set oQueueShotVO = new Queue
	Set oQueuePlayVO = new Queue

	oVoiceTimeDict.add "VO_hero_ball_save1", 2375
	oVoiceTimeDict.add "VO_hero_ball_save2", 1612
	oVoiceTimeDict.add "VO_hero_ball_save3", 2219 
	oVoiceTimeDict.add "VO_hero_combo1", 1232
	oVoiceTimeDict.add "VO_hero_combo2", 1210
	oVoiceTimeDict.add "VO_hero_combo3", 1786
	oVoiceTimeDict.add "VO_hero_combo4", 1737
	oVoiceTimeDict.add "VO_hero_extra_ball_lit", 1286
	oVoiceTimeDict.add "VO_hero_extra_ball", 1991
	oVoiceTimeDict.add "VO_hero_mayhem_start", 4424
	oVoiceTimeDict.add "VO_hero_mball_revive1", 2527
	oVoiceTimeDict.add "VO_hero_mball_revive2", 1750
	oVoiceTimeDict.add "VO_hero_mball_sj_medium", 1701
	oVoiceTimeDict.add "VO_hero_mball_sj_large", 1884
	oVoiceTimeDict.add "VO_hero_mball_sj_xl", 2799
	oVoiceTimeDict.add "VO_hero_mode_air1_victory", 4357
	oVoiceTimeDict.add "VO_hero_mode_air2_start", 3511
	oVoiceTimeDict.add "VO_hero_mode_air2_victory", 4237
	oVoiceTimeDict.add "VO_hero_mode_cyber1_start", 4241
	oVoiceTimeDict.add "VO_hero_mode_cyber1_victory", 2804
	oVoiceTimeDict.add "VO_hero_mode_cyber2_start", 3366
	oVoiceTimeDict.add "VO_hero_mode_cyber2_victory", 2705
	oVoiceTimeDict.add "VO_hero_mode_earth1_victory", 2205
	oVoiceTimeDict.add "VO_hero_mode_earth2_victory", 3223
	oVoiceTimeDict.add "VO_hero_mode_fire1_victory", 3295
	oVoiceTimeDict.add "VO_hero_mode_fire2_victory", 3397
	oVoiceTimeDict.add "VO_hero_mode_ice1_victory", 4176
	oVoiceTimeDict.add "VO_hero_mode_ice2_start", 2415
	oVoiceTimeDict.add "VO_hero_mode_ice2_victory", 3920
	oVoiceTimeDict.add "VO_hero_mode_select_plunger", 1125
	oVoiceTimeDict.add "VO_hero_mode_select_saucer", 1777
	oVoiceTimeDict.add "VO_hero_mode_select1", 2067
	oVoiceTimeDict.add "VO_hero_mode_select2", 2125
	oVoiceTimeDict.add "VO_hero_mode_select3", 1219
	oVoiceTimeDict.add "VO_hero_mode_water1_start", 3272
	oVoiceTimeDict.add "VO_hero_mode_water1_victory", 3804
	oVoiceTimeDict.add "VO_hero_mode_water2_start", 3085
	oVoiceTimeDict.add "VO_hero_mode_water2_victory", 4826
	oVoiceTimeDict.add "VO_hero_mystery1", 3049
	oVoiceTimeDict.add "VO_hero_mystery2", 5371
	oVoiceTimeDict.add "VO_hero_mystery3", 2286
	oVoiceTimeDict.add "VO_hero_mystery4", 3379
	oVoiceTimeDict.add "VO_hero_player1_1", 969
	oVoiceTimeDict.add "VO_hero_player1_2", 812
	oVoiceTimeDict.add "VO_hero_player1_3", 746
	oVoiceTimeDict.add "VO_hero_player2_1", 1000
	oVoiceTimeDict.add "VO_hero_player2_2", 987
	oVoiceTimeDict.add "VO_hero_player2_3", 884
	oVoiceTimeDict.add "VO_hero_player3_1", 1089
	oVoiceTimeDict.add "VO_hero_player3_2", 1036
	oVoiceTimeDict.add "VO_hero_player3_3", 871
	oVoiceTimeDict.add "VO_hero_player4_1", 1080
	oVoiceTimeDict.add "VO_hero_player4_2", 1013
	oVoiceTimeDict.add "VO_hero_player4_3", 1040
	oVoiceTimeDict.add "VO_hero_shatz1", 2714
	oVoiceTimeDict.add "VO_hero_shatz2", 2527
	oVoiceTimeDict.add "VO_hero_shoot_again", 1067
	oVoiceTimeDict.add "VO_hero_showdown_air_damage3", 3134
	oVoiceTimeDict.add "VO_hero_showdown_cyber_attack1", 2969
	oVoiceTimeDict.add "VO_hero_showdown_cyber_attack2", 1357
	oVoiceTimeDict.add "VO_hero_showdown_cyber_attack3", 2576
	oVoiceTimeDict.add "VO_hero_showdown_cyber_damage2", 3013
	oVoiceTimeDict.add "VO_hero_showdown_cyber_start", 2853
	oVoiceTimeDict.add "VO_hero_showdown_cyber_victory", 4330
	oVoiceTimeDict.add "VO_hero_showdown_earth_open1", 1188
	oVoiceTimeDict.add "VO_hero_showdown_earth_open2", 2250
	oVoiceTimeDict.add "VO_hero_showdown_earth_open3", 1424
	oVoiceTimeDict.add "VO_hero_showdown_earth_victory", 5219
	oVoiceTimeDict.add "VO_hero_showdown_fire_damage2", 3308
	oVoiceTimeDict.add "VO_hero_showdown_fire_victory", 4071
	oVoiceTimeDict.add "VO_hero_showdown_ice_start", 2299
	oVoiceTimeDict.add "VO_hero_showdown_ice_victory", 2134
	oVoiceTimeDict.add "VO_hero_showdown_lit", 2040
	oVoiceTimeDict.add "VO_hero_super_wizard_start", 5835
	oVoiceTimeDict.add "VO_hero_tilt_warning2", 2589
	oVoiceTimeDict.add "VO_hero_wizard_mode_lit", 2085
	oVoiceTimeDict.add "VO_air_evil_united_start", 3451
	oVoiceTimeDict.add "VO_air_mball_jackpot1x", 1050
	oVoiceTimeDict.add "VO_air_mball_jackpot2x", 1617
	oVoiceTimeDict.add "VO_air_mball_jackpot3x", 1547
	oVoiceTimeDict.add "VO_air_mball_join", 3074
	oVoiceTimeDict.add "VO_air_mball_ready", 2446
	oVoiceTimeDict.add "VO_air_mball_sj", 984
	oVoiceTimeDict.add "VO_air_mball_sj_lit", 2709
	oVoiceTimeDict.add "VO_air_mball_start", 3180
	oVoiceTimeDict.add "VO_air_mode1_start", 3215
	oVoiceTimeDict.add "VO_air_mode2_start", 4955
	oVoiceTimeDict.add "VO_air_showdown_damage1", 4950
	oVoiceTimeDict.add "VO_air_showdown_damage2", 5243
	oVoiceTimeDict.add "VO_air_showdown_damage3", 2899
	oVoiceTimeDict.add "VO_air_showdown_defeat", 2948
	oVoiceTimeDict.add "VO_air_showdown_drain", 1677
	oVoiceTimeDict.add "VO_air_showdown_start", 4129
	oVoiceTimeDict.add "VO_air_tilt", 2900
	oVoiceTimeDict.add "VO_air_tilt_warning", 1551
	oVoiceTimeDict.add "VO_cyber_evil_united_start", 1800
	oVoiceTimeDict.add "VO_cyber_mball_jackpot01x", 684
	oVoiceTimeDict.add "VO_cyber_mball_jackpot02x", 1174
	oVoiceTimeDict.add "VO_cyber_mball_jackpot03x", 1311
	oVoiceTimeDict.add "VO_cyber_mball_jackpot04x", 1652
	oVoiceTimeDict.add "VO_cyber_mball_jackpot05x", 1527
	oVoiceTimeDict.add "VO_cyber_mball_jackpot06x", 1559
	oVoiceTimeDict.add "VO_cyber_mball_jackpot07x", 1537
	oVoiceTimeDict.add "VO_cyber_mball_jackpot08x", 1463
	oVoiceTimeDict.add "VO_cyber_mball_jackpot09x", 1446
	oVoiceTimeDict.add "VO_cyber_mball_jackpot10x", 1411
	oVoiceTimeDict.add "VO_cyber_mball_jackpot12x", 1671
	oVoiceTimeDict.add "VO_cyber_mball_jackpot14x", 2011
	oVoiceTimeDict.add "VO_cyber_mball_jackpot18x", 1841
	oVoiceTimeDict.add "VO_cyber_mball_jackpot20x", 1671
	oVoiceTimeDict.add "VO_cyber_mball_jackpot21x", 1843
	oVoiceTimeDict.add "VO_cyber_mball_jackpot24x", 2201
	oVoiceTimeDict.add "VO_cyber_mball_jackpot28x", 2016
	oVoiceTimeDict.add "VO_cyber_mball_jackpot30x", 1744
	oVoiceTimeDict.add "VO_cyber_mball_jackpot35x", 2048
	oVoiceTimeDict.add "VO_cyber_mball_jackpot36x", 2000
	oVoiceTimeDict.add "VO_cyber_mball_jackpot42x", 2303
	oVoiceTimeDict.add "VO_cyber_mball_join", 2229
	oVoiceTimeDict.add "VO_cyber_mball_ready", 1746
	oVoiceTimeDict.add "VO_cyber_mball_sj_lit", 2183
	oVoiceTimeDict.add "VO_cyber_mball_sj", 662
	oVoiceTimeDict.add "VO_cyber_mball_start", 1103
	oVoiceTimeDict.add "VO_cyber_mode1_start", 2071
	oVoiceTimeDict.add "VO_cyber_mode2_start", 2067
	oVoiceTimeDict.add "VO_cyber_showdown_attack", 2357
	oVoiceTimeDict.add "VO_cyber_showdown_countdown1", 323
	oVoiceTimeDict.add "VO_cyber_showdown_countdown2", 381
	oVoiceTimeDict.add "VO_cyber_showdown_countdown3", 410
	oVoiceTimeDict.add "VO_cyber_showdown_countdown4", 459
	oVoiceTimeDict.add "VO_cyber_showdown_countdown5", 586
	oVoiceTimeDict.add "VO_cyber_showdown_damage1", 1672
	oVoiceTimeDict.add "VO_cyber_showdown_damage3", 1684
	oVoiceTimeDict.add "VO_cyber_showdown_start1", 2670
	oVoiceTimeDict.add "VO_cyber_showdown_start2", 1463
	oVoiceTimeDict.add "VO_cyber_tilt_warning", 1335
	oVoiceTimeDict.add "VO_cyber_tilt", 894
	oVoiceTimeDict.add "VO_earth_evil_united_start", 4312
	oVoiceTimeDict.add "VO_earth_mball_jackpot1", 911
	oVoiceTimeDict.add "VO_earth_mball_jackpot2", 987
	oVoiceTimeDict.add "VO_earth_mball_jackpot3", 772
	oVoiceTimeDict.add "VO_earth_mball_join", 2043
	oVoiceTimeDict.add "VO_earth_mball_lock1", 1413
	oVoiceTimeDict.add "VO_earth_mball_lock2", 1254
	oVoiceTimeDict.add "VO_earth_mball_sj_lit", 2264
	oVoiceTimeDict.add "VO_earth_mball_sj", 615
	oVoiceTimeDict.add "VO_earth_mball_start1", 4057
	oVoiceTimeDict.add "VO_earth_mball_start2", 4040
	oVoiceTimeDict.add "VO_earth_mode1_start", 3727
	oVoiceTimeDict.add "VO_earth_mode2_start", 3274
	oVoiceTimeDict.add "VO_earth_showdown_close1", 1591
	oVoiceTimeDict.add "VO_earth_showdown_close2", 2392
	oVoiceTimeDict.add "VO_earth_showdown_close3", 1765
	oVoiceTimeDict.add "VO_earth_showdown_close4", 1846
	oVoiceTimeDict.add "VO_earth_showdown_count10", 669
	oVoiceTimeDict.add "VO_earth_showdown_count09", 789
	oVoiceTimeDict.add "VO_earth_showdown_count08", 461
	oVoiceTimeDict.add "VO_earth_showdown_count07", 427
	oVoiceTimeDict.add "VO_earth_showdown_count06", 453
	oVoiceTimeDict.add "VO_earth_showdown_count05", 422
	oVoiceTimeDict.add "VO_earth_showdown_count04", 398
	oVoiceTimeDict.add "VO_earth_showdown_count03", 591
	oVoiceTimeDict.add "VO_earth_showdown_count02", 494
	oVoiceTimeDict.add "VO_earth_showdown_count01", 769
	oVoiceTimeDict.add "VO_earth_showdown_damage1", 2394
	oVoiceTimeDict.add "VO_earth_showdown_damage2", 1985
	oVoiceTimeDict.add "VO_earth_showdown_drain", 2972
	oVoiceTimeDict.add "VO_earth_showdown_open1", 2635
	oVoiceTimeDict.add "VO_earth_showdown_open2", 2543
	oVoiceTimeDict.add "VO_earth_showdown_start", 4447
	oVoiceTimeDict.add "VO_earth_tilt", 2543
	oVoiceTimeDict.add "VO_earth_tilt_warning", 2229
	oVoiceTimeDict.add "VO_fire_evil_united_start", 2518
	oVoiceTimeDict.add "VO_fire_mball_jackpot", 936
	oVoiceTimeDict.add "VO_fire_mball_join", 2808
	oVoiceTimeDict.add "VO_fire_mball_lock1", 1518
	oVoiceTimeDict.add "VO_fire_mball_lock2", 1705
	oVoiceTimeDict.add "VO_fire_mball_ready", 2839
	oVoiceTimeDict.add "VO_fire_mball_sj", 735
	oVoiceTimeDict.add "VO_fire_mball_sj_ready", 3464
	oVoiceTimeDict.add "VO_fire_mball_start", 3839
	oVoiceTimeDict.add "VO_fire_mode1_start", 5567
	oVoiceTimeDict.add "VO_fire_mode2_start", 4626
	oVoiceTimeDict.add "VO_fire_showdown_damage1", 3670
	oVoiceTimeDict.add "VO_fire_showdown_drain", 1138
	oVoiceTimeDict.add "VO_fire_showdown_start", 4688
	oVoiceTimeDict.add "VO_fire_tilt_warning", 1054
	oVoiceTimeDict.add "VO_fire_tilt", 3692
	oVoiceTimeDict.add "VO_ice_evil_united_start", 3413
	oVoiceTimeDict.add "VO_ice_mball_jackpot1", 922
	oVoiceTimeDict.add "VO_ice_mball_jackpot2", 975
	oVoiceTimeDict.add "VO_ice_mball_jackpot3", 892
	oVoiceTimeDict.add "VO_ice_mball_join", 2241
	oVoiceTimeDict.add "VO_ice_mball_lock1", 1892
	oVoiceTimeDict.add "VO_ice_mball_lock2", 1882
	oVoiceTimeDict.add "VO_ice_mball_sj", 605
	oVoiceTimeDict.add "VO_ice_mball_sj_lit", 2782
	oVoiceTimeDict.add "VO_ice_mball_start", 4754
	oVoiceTimeDict.add "VO_ice_mode1_start", 3634
	oVoiceTimeDict.add "VO_ice_mode2_start", 1869
	oVoiceTimeDict.add "VO_ice_showdown_damage1", 2240
	oVoiceTimeDict.add "VO_ice_showdown_damage2", 6420
	oVoiceTimeDict.add "VO_ice_showdown_damage3", 3913
	oVoiceTimeDict.add "VO_ice_showdown_damage4", 2810
	oVoiceTimeDict.add "VO_ice_showdown_drain", 3529
	oVoiceTimeDict.add "VO_ice_showdown_start", 3913
	oVoiceTimeDict.add "VO_ice_tilt_warning", 4180
	oVoiceTimeDict.add "VO_ice_tilt", 3471
	oVoiceTimeDict.add "VO_water_evil_united_start", 2659
	oVoiceTimeDict.add "VO_water_mayhem_start", 4365
	oVoiceTimeDict.add "VO_water_mball_jackpot1", 886
	oVoiceTimeDict.add "VO_water_mball_jackpot2", 888
	oVoiceTimeDict.add "VO_water_mball_jackpot3", 838
	oVoiceTimeDict.add "VO_water_mball_jackpot4", 982
	oVoiceTimeDict.add "VO_water_mball_join", 3518
	oVoiceTimeDict.add "VO_water_mball_lock1", 1358
	oVoiceTimeDict.add "VO_water_mball_lock2", 1382
	oVoiceTimeDict.add "VO_water_mball_ready", 2010
	oVoiceTimeDict.add "VO_water_mball_sj", 803
	oVoiceTimeDict.add "VO_water_mball_sj_lit", 2025
	oVoiceTimeDict.add "VO_water_mball_start", 5108
	oVoiceTimeDict.add "VO_water_mode1_start", 3619
	oVoiceTimeDict.add "VO_water_mode2_start", 3678
	oVoiceTimeDict.add "VO_water_showdown_damage1", 3886
	oVoiceTimeDict.add "VO_water_showdown_damage2", 4145
	oVoiceTimeDict.add "VO_water_showdown_damage3", 4333
	oVoiceTimeDict.add "VO_water_showdown_start", 4505
	oVoiceTimeDict.add "VO_water_tilt_warning", 2951
	oVoiceTimeDict.add "VO_water_tilt", 3743

	Set oVoicePlaysDict = CreateObject("Scripting.Dictionary")
	oVoicePlaysDict.add "VO_hero_ball_save", 0
	oVoicePlaysDict.add "VO_hero_combo", 0
	oVoicePlaysDict.add "VO_hero_mball_revive", 0
	oVoicePlaysDict.add "VO_hero_mode_select", 0
	oVoicePlaysDict.add "VO_hero_mystery", 0
	oVoicePlaysDict.add "VO_hero_shatz", 0
	oVoicePlaysDict.add "VO_hero_player1_", 0
	oVoicePlaysDict.add "VO_hero_player2_", 0
	oVoicePlaysDict.add "VO_hero_player3_", 0
	oVoicePlaysDict.add "VO_hero_player4_", 0
	oVoicePlaysDict.add "VO_showdown_cyber_attack", 0
	oVoicePlaysDict.add "VO_earth_mball_jackpot", 0
	oVoicePlaysDict.add "VO_earth_mball_start", 0
	oVoicePlaysDict.add "VO_earth_showdown_close", 0
	oVoicePlaysDict.add "VO_earth_showdown_open", 0
	oVoicePlaysDict.add "VO_ice_mball_jackpot", 0
	oVoicePlaysDict.add "VO_water_mball_jackpot", 0

	Set oVoiceVariations = CreateObject("Scripting.Dictionary")
	oVoiceVariations.add "VO_hero_ball_save", 3
	oVoiceVariations.add "VO_hero_combo", 4
	oVoiceVariations.add "VO_hero_mball_revive", 2
	oVoiceVariations.add "VO_hero_mode_select", 3
	oVoiceVariations.add "VO_hero_mystery", 4
	oVoiceVariations.add "VO_hero_shatz", 2
	oVoiceVariations.add "VO_hero_player1_", 3
	oVoiceVariations.add "VO_hero_player2_", 3
	oVoiceVariations.add "VO_hero_player3_", 3
	oVoiceVariations.add "VO_hero_player4_", 3
	oVoiceVariations.add "VO_showdown_cyber_attack", 4
	oVoiceVariations.add "VO_earth_mball_jackpot", 3
	oVoiceVariations.add "VO_earth_mball_start", 2
	oVoiceVariations.add "VO_earth_showdown_close", 4
	oVoiceVariations.add "VO_earth_showdown_open", 5
	oVoiceVariations.add "VO_ice_mball_jackpot", 3
	oVoiceVariations.add "VO_water_mball_jackpot", 4
End Sub

' This is called when a shot is hit. Everything triggered by the shot is added
' to the temporary oQueueShotVO collection. On a timer, oQueueShotVO is cleared
' and the highest priority callout is added to the actual callout queue. The
' exception is urgent callouts, which interrupt the current callout and are
' played immediately.
Sub AddCallout (sCallout, nPriority, nCharacter)
	AddDelayedCallout sCallout, nPriority, nCharacter, 0
End Sub

Sub AddDelayedCallout (sCallout, nPriority, nCharacter, nDelay)
	Dim aCalloutParams, aFirstInQueue, nTime, nCount, sActualCallout
	Dim nQueuedPrio, nQueuedCharacter, nAdjustedChar

	' Don't interrupt the super jackpot sequence except for a tilt 
	If (nPriority > eVOPrioTilt) And (nPriority <> eVOPrioSuperJackpot) And TimerSJCallout.Enabled Then
		Exit Sub
	end If

	' Handle ball save round robin (other round robins are handled when played,
	' but the ball save is considered urgent and is never queued)
	If sCallout = "VO_hero_ball_save" Then
		nCount = oVoicePlaysDict.Item("VO_hero_ball_save")
		If Not IsNumeric(nCount) Then nCount = 0
		sActualCallout = sCallout & (nCount + 1)
		nCount = (nCount + 1) mod 3
		oVoicePlaysDict.Item("VO_hero_ball_save") = nCount
	Else
		sActualCallout = sCallout
	End If

	' Handle urgent callouts
	If nPriority < eVOPrioTiltWarning Then
		' Handle "player N" round robins
		If oVoiceVariations.Exists(sCallout) Then
			nCount = oVoicePlaysDict.Item(sCallout)
			If Not IsNumeric(nCount) Then nCount = 0
			sActualCallout = sCallout & (nCount + 1)
			nCount = (nCount + 1) mod oVoiceVariations.Item(sCallout)
			oVoicePlaysDict.Item(sCallout) = nCount
		End If
		StopSound sCurrentCallout
		PlaySound sActualCallout, 0, fCalloutVolume
		nCharSpeaking = nCharacter
		sCurrentCallout = sActualCallout
		nTime = oVoiceTimeDict.Item(sActualCallout)
		If IsNumeric(nTime) Then
			nCalloutTimeLeft = nTime + nTimeCalloutPadding
		Else
			nCalloutTimeLeft = nTimeCalloutPadding
		End If
		oQueueShotVO.Clear
		oQueuePlayVO.Clear
		DuckMusic nCalloutTimeLeft
	Else
		aCalloutParams = Array(sCallout, nPriority, nCharacter, nDelay)
		If oQueueShotVO.Size < 1 Then
			oQueueShotVO.Enqueue aCalloutParams
'			debug.print "Callout " & sCallout & " added to oQueueShotVO"
		Else
			aFirstInQueue = oQueueShotVO.Peek
			nQueuedPrio = aFirstInQueue(1)
			If nPriority < nQueuedPrio Then
				oQueueShotVO.Clear
				oQueueShotVO.Enqueue aCalloutParams
'				debug.print "Callout " & sCallout & " added to oQueueShotVO"
' Round robin for whose jackpot gets a callout if a shot triggers multiple JPs
			Elseif nPriority = eVOPrioJackpot and nQueuedPrio = eVOPrioJackpot Then
				nQueuedCharacter = aFirstInQueue(2)
				If nQueuedCharacter <= nLastJackpot Then
					nQueuedCharacter = nQueuedCharacter + 6
				End If
				If nCharacter <= nLastJackpot Then
					nAdjustedChar = nCharacter + 6
				Else
					nAdjustedChar = nCharacter
				End If
				If nAdjustedChar < nQueuedCharacter Then
					oQueueShotVO.Clear
					oQueueShotVO.Enqueue aCalloutParams
'					debug.print "Callout " & sCallout & " added to oQueueShotVO"
				Else
'					debug.print "Callout " & sCallout & " discarded"
				End If
			Else
'				debug.print "Callout " & sCallout & " discarded"
			End If
		End If
	End If
End Sub

Sub QueueDialogue(sCallout, nPriority, nCharacter)
	Dim aCalloutParams
	aCalloutParams = Array(sCallout, nPriority, nCharacter, 0)
	oQueuePlayVO.Enqueue aCalloutParams
End Sub

Sub TimerCallout_Timer
	Dim i, aCalloutParams, aFirstInQueue, nTime, nCount, sActualCallout
	Dim nPriority, sCallout, nQueuedPrio, nDelay, nCharacter
'	sTimersRun = sTimersRun + " TimerCallout"

	nCalloutTimeLeft = nCalloutTimeLeft - TimerCallout.interval
	If nCalloutTimeLeft < 0 then
		nCalloutTimeLeft = 0
		nTimeDelayCallout = 0
		sCurrentCallout = ""
		nCharSpeaking = eCharacterNone
	End If
	If bVOQueueLocked Then Exit Sub

	If nTimeDelayCallout > 0 then
		nTimeDelayCallout = nTimeDelayCallout - TimerCallout.interval
		if nTimeDelayCallout < 1 Then
			nTimeDelayCallout = 0
			DuckMusic nCalloutTimeLeft
			PlaySound sCurrentCallout, 0, fCalloutVolume
		End If
	End If

	If oQueueShotVO.Size > 0 Then
		aCalloutParams = oQueueShotVO.Dequeue
		nQueuedPrio = aCalloutParams(1)
		If 0 = oQueuePlayVO.Size Then
			nPriority = 99
		Else
			aFirstInQueue = oQueuePlayVO.Peek
			nPriority = aFirstInQueue(1)
		End If
		If nQueuedPrio < nPriority Then
			oQueuePlayVO.PushFirst aCalloutParams
		End If
		oQueueShotVO.Clear
	End If

	If nCalloutTimeLeft > 0 Then Exit Sub

	If TimerSJCallout.Enabled then Exit Sub

	If oQueuePlayVO.Size > 0 Then
		aCalloutParams = oQueuePlayVO.Dequeue
		sCallout = aCalloutParams(0)
		nPriority = aCalloutParams(1)
		nCharacter = aCalloutParams(2)
		nDelay = aCalloutParams(3)

		sActualCallout = ""
		' Handle round robins where more than one character is involved
		If sCallout = "VO_showdown_cyber_attack" Then
			nCount = oVoicePlaysDict.Item("VO_showdown_cyber_attack")
			If Not IsNumeric(nCount) Then nCount = 0
			Select Case nCount
				Case 0
					sActualCallout = "VO_hero_showdown_cyber_attack1"
					nCharacter = eCharacterHero
				Case 1
					sActualCallout = "VO_hero_showdown_cyber_attack2"
					nCharacter = eCharacterHero
				Case 2
					sActualCallout = "VO_hero_showdown_cyber_attack3"
					nCharacter = eCharacterHero
				Case Else
					sActualCallout = "VO_cyber_showdown_attack"
					nCharacter = eCharacterCyber
			End Select
			nCount = (nCount + 1) mod 4
			oVoicePlaysDict.Item("VO_showdown_cyber_attack") = nCount
		ElseIf sCallout = "VO_earth_showdown_open" Then
			nCount = oVoicePlaysDict.Item("VO_earth_showdown_open")
			If Not IsNumeric(nCount) Then nCount = 0
			Select Case nCount
				Case 0
					sActualCallout = "VO_hero_showdown_earth_open1"
					nCharacter = eCharacterHero
				Case 1
					sActualCallout = "VO_hero_showdown_earth_open2"
					nCharacter = eCharacterHero
				Case 2
					sActualCallout = "VO_hero_showdown_earth_open3"
					nCharacter = eCharacterHero
				Case 3
					sActualCallout = "VO_earth_showdown_open1"
					nCharacter = eCharacterEarth
				Case Else
					sActualCallout = "VO_earth_showdown_open2"
					nCharacter = eCharacterEarth
			End Select
			nCount = (nCount + 1) mod 5
			oVoicePlaysDict.Item("VO_earth_showdown_open") = nCount
		Else
		' Handle all other round robins
			If oVoiceVariations.Exists(sCallout) Then
				nCount = oVoicePlaysDict.Item(sCallout)
				If Not IsNumeric(nCount) Then nCount = 0
				sActualCallout = sCallout & (nCount + 1)
				nCount = (nCount + 1) mod oVoiceVariations.Item(sCallout)
				oVoicePlaysDict.Item(sCallout) = nCount
			End If
		End If
		If sActualCallout = "" then
			sActualCallout = sCallout
		Else
'			debug.print "Round Robin: " & sCallout & " to " & sActualCallout
		End If

		sCurrentCallout = sActualCallout
		nTime = oVoiceTimeDict.Item(sActualCallout)
		If IsNumeric(nTime) Then
			If eVOPrioCountdown = nPriority Then
				nCalloutTimeLeft = nTime
			Else
				nCalloutTimeLeft = nTime + nTimeCalloutPadding
			End If
		Else
			nCalloutTimeLeft = nTimeCalloutPadding
		End If
		If nDelay > 0 Then
			nTimeDelayCallout = nDelay
			nCalloutTimeLeft = nCalloutTimeLeft + nDelay
		Else
			DuckMusic nCalloutTimeLeft
			PlaySound sActualCallout, 0, fCalloutVolume
		End If
		nCharSpeaking = nCharacter
	Else
		oQueuePlayVO.Clear
	End If
End Sub

'*** music helpers

Dim MusicIntroDict ' Dict of song intro lengths in ms
Set MusicIntroDict = CreateObject("Scripting.Dictionary")
Dim fCurrentMusicVol : fCurrentMusicVol = 0

MusicIntroDict.add "MUS_plunger",4060

Sub SwitchMusic(sTrack,intro)
	If sTrack <> sMusicTrack and intro <> true Then
		StopSound sMusicTrack
		StopSound sMusicTrack & "_intro"
		StopSound sMusicTrack & "_loop"
		sMusicTrack = sTrack
		If nTimeDuckMusic > 0 Then
			fCurrentMusicVol = fMusicVolume * fDuckFactor
			PlaySound sTrack, -1, fMusicVolume * fDuckFactor
		Else
			fCurrentMusicVol = fMusicVolume
			PlaySound sTrack, -1, fMusicVolume
		End If
	elseif sTrack <> sMusicTrack and intro = true then
		StopSound sMusicTrack
		sMusicTrack = sTrack & "_intro"
		If nTimeDuckMusic > 0 Then
			fCurrentMusicVol = fMusicVolume * fDuckFactor
			PlaySound sMusicTrack, 0, fMusicVolume * fDuckFactor
		Else
			fCurrentMusicVol = fMusicVolume
			PlaySound sMusicTrack, 0, fMusicVolume
		End If		
		MusicIntroTimer.interval = MusicIntroDict.Item(sTrack)
		MusicIntroTimer.enabled = true
	end If
End Sub

Sub MusicIntroTimer_timer()
	MusicIntroTimer.enabled = false
	StopSound sMusicTrack
	sMusicTrack = Replace(sMusicTrack, "_intro", "_loop")
	If nTimeDuckMusic > 0 Then
		fCurrentMusicVol = fMusicVolume * fDuckFactor
		PlaySound sMusicTrack, 0, fMusicVolume * fDuckFactor
	Else
		fCurrentMusicVol = fMusicVolume
		PlaySound sMusicTrack, 0, fMusicVolume
	End If		
end sub

Sub PlayJingle(sJingle, nDuration)
	If "" = sJinglePlaying Then
		sJinglePlaying = sJingle
		If nTimeDuckMusic < nDuration Then
			DuckMusic nDuration
		End If
		PlaySound sJingle,0,fSFXVolume,0,0,1,1,1
	End If
End Sub

Sub DuckMusic(interval)
	If nTimeDuckMusic < interval Then
		nTimeDuckMusic = interval
	End If
	fCurrentMusicVol = fMusicVolume * fDuckFactor
	PlaySound sMusicTrack, -1, fMusicVolume * fDuckFactor,0,0,0,1,0
end sub

Sub TimerDuck_timer()
	Dim i
	Dim fVolumeDifference

'	sTimersRun = sTimersRun + " TimerDuck"
	If nSFXPriority < 99 Then
		If IsArray(sSFXToPlay) Then
			For Each i in sSFXToPlay
				PlaySound i,0,fSFXVolume * fSFXQueuedVolume,0,0,0,1,1
			Next
		Else
			PlaySound sSFXToPlay,0,fSFXVolume * fSFXQueuedVolume,0,0,0,1,1
		End If

		sSFXToPlay = ""
		fSFXQueuedVolume = 1.0
		nSFXPriority = 99
	End If

	If nTimeDuckMusic > 0 Then
		nTimeDuckMusic = nTimeDuckMusic - TimerDuck.interval
		If nTimeDuckMusic < 1 Then
			nTimeDuckMusic = 0
			sJinglePlaying = ""
'			The fade in code below handles the return to normal volume
		End If
	Elseif fCurrentMusicVol < fMusicVolume Then
		' fade in is 10 updates at 20 ms per update
		fVolumeDifference = fMusicVolume - (fMusicVolume * fDuckfactor)
		fCurrentMusicVol = fCurrentMusicVol + (fVolumeDifference / 10)
		If fCurrentMusicVol >= fMusicVolume Then
			fCurrentMusicVol = fMusicVolume
		End If
		PlaySound sMusicTrack, -1, fCurrentMusicVol,0,0,0,1,0
	End If
end sub

Sub PlayModeMusic
	if bSelectingMode then
		SwitchMusic "MUS_plunger",true
	elseif avModesRunning(nPlayer).contains(eModeMegaWizard) then
		SwitchMusic "music-TEMP-wizard2", false
	elseif avModesRunning(nPlayer).contains(eModeTotalMayhem) then
		SwitchMusic "music-TEMP-mayhem", false
	elseif avModesRunning(nPlayer).contains(eModeEvilUnited) then
		SwitchMusic "music-TEMP-wizard1", false
	elseIf avModesRunning(nPlayer).Contains(eModeCyber1) or avModesRunning(nPlayer).Contains(eModeCyber2)_
	or avModesRunning(nPlayer).Contains(eModeCyberWizard) Then
		SwitchMusic "music-TEMP-cyber1", false
	elseif avModesRunning(nPlayer).contains(eModeWater1) then
		SwitchMusic "MUS_shark_attack", false
	ElseIf avModesRunning(nPlayer).Contains(eModeWater2) or avModesRunning(nPlayer).Contains(eModeWaterWizard) Then
		SwitchMusic "MUS_shark_attack", False
	elseif avModesRunning(nPlayer).contains(eModeFire1) then
		SwitchMusic "MUS_floor_is_lava", false
	ElseIf avModesRunning(nPlayer).Contains(eModeFire2) or avModesRunning(nPlayer).Contains(eModeFireWizard) Then
		SwitchMusic "MUS_floor_is_lava", false
	ElseIf avModesRunning(nPlayer).Contains(eModeEarth1) or avModesRunning(nPlayer).Contains(eModeEarth2) _
	or avModesRunning(nPlayer).Contains(eModeEarthWizard) Then
		SwitchMusic "music-TEMP-earth1", false
	ElseIf avModesRunning(nPlayer).Contains(eModeAir1) or avModesRunning(nPlayer).Contains(eModeAir2) _
	or avModesRunning(nPlayer).Contains(eModeAirWizard) Then
		SwitchMusic "music-TEMP-air1", false
	ElseIf avModesRunning(nPlayer).Contains(eModeIce1) or avModesRunning(nPlayer).Contains(eModeIce2) _
	or avModesRunning(nPlayer).Contains(eModeIceWizard) Then
		SwitchMusic "music-TEMP-ice1", false
	Elseif avModesRunning(nPlayer).Contains(eModeCyberMBall) and (eModeCyberMBall = nFirstMBallStarted) Then
		SwitchMusic "music-TEMP-cyber1", false
	Elseif avModesRunning(nPlayer).Contains(eModeWaterMBall) and (eModeWaterMBall = nFirstMBallStarted) Then
		SwitchMusic "MUS_shark_attack", false
	Elseif avModesRunning(nPlayer).Contains(eModeFireMBall) and (eModeFireMBall = nFirstMBallStarted) Then
		SwitchMusic "MUS_floor_is_lava", false
	Elseif avModesRunning(nPlayer).Contains(eModeEarthMBall) and (eModeEarthMBall = nFirstMBallStarted) Then
		SwitchMusic "music-TEMP-earth1", false
	Elseif avModesRunning(nPlayer).Contains(eModeAirMBall) and (eModeAirMBall = nFirstMBallStarted) Then
		SwitchMusic "music-TEMP-air1", false
	Elseif avModesRunning(nPlayer).Contains(eModeIceMBall) and (eModeIceMBall = nFirstMBallStarted) Then
		SwitchMusic "music-TEMP-ice1", false
	Else
		SwitchMusic "music-TEMP-oom", false
	End If
End Sub

Sub StopAllMusic
	sMusicTrack = ""
	StopSound "MUS_plunger_intro"
	StopSound "MUS_plunger_loop"
	StopSound "MUS_shark_attack"
	StopSound "MUS_floor_is_lava"
	StopSound "music-TEMP-oom"
	StopSound "music-TEMP-cyber1"
	StopSound "music-TEMP-water2"
    StopSound "music-TEMP-fire2"
	StopSound "music-TEMP-earth1"
	StopSound "music-TEMP-air1"
	StopSound "music-TEMP-ice1"
	StopSound "MUS_bonus_count_main"
End Sub

'*** Bonus
Sub InitBonus
	Dim i
	' Kill flippers
	LeftFlipper.RotateToStart
	LeftUpperFlipper.RotateToStart
	RightFlipper.RotateToStart
	DOF 101, DOFOff
	DOF 102, DOFOff

	' End Modes
	for i = eMballCyber to eMballIce
		anTimerMballGrace(i) = 0
	Next
	If avModesRunning(nPlayer).Contains(eModeCyberMBall) Then
		CyberMballEnd
	End If
	If avModesRunning(nPlayer).Contains(eModeWaterMBall) Then
		WaterMballEnd
	End If
	If avModesRunning(nPlayer).Contains(eModeFireMBall) Then
		FireMballEnd
	End If
	If avModesRunning(nPlayer).Contains(eModeEarthMBall) Then
		EarthMballEnd
	End If
	If avModesRunning(nPlayer).Contains(eModeAirMBall) Then
		AirMballEnd
	End If
	If avModesRunning(nPlayer).Contains(eModeIceMBall) Then
		IceMballEnd
	End If
	StopSound sMusicTrack
	sMusicTrack = ""
	AllLightsOff
	LightShowEnd
	ClearTextQueue
	ClearStatus
	If False = bDrainingBalls Then
		ApplyLampRGB 3, anColorCycleLight(eColorGreen)
		ApplyLampRGB 4, anColorCycleLight(eColorGreen)
		ApplyLampRGB 5, anColorCycleLight(eColorBlue)
		ApplyLampRGB 6, anColorCycleLight(eColorBlue)
		ApplyLampRGB 7, anColorCycleLight(eColorRed)
		ApplyLampRGB 8, anColorCycleLight(eColorRed)
		ApplyLampRGB 9, anColorCycleLight(eColorOrange)
		ApplyLampRGB 10, anColorCycleLight(eColorOrange)
		ApplyLampRGB 13, anColorCycleLight(eColorPurple)
		ApplyLampRGB 14, anColorCycleLight(eColorPurple)
		ApplyLampRGB 15, anColorCycleLight(eColorWhite)
		ApplyLampRGB 16, anColorCycleLight(eColorWhite)

		bCountingBonus = True
		nBonusFrame = 0
		nBonusDisplayed = 0
		TimerBonus.enabled = True
		nBonusTotal = 0
		If avModesPlayed(nPlayer).Contains(eModeCyber1) Then
			anBonusElements(eMballCyber) = anBonusElements(eMballCyber) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeCyber2) Then
			anBonusElements(eMballCyber) = anBonusElements(eMballCyber) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeCyberWizard) Then
			anBonusElements(eMballCyber) = anBonusElements(eMballCyber) + 2000
		End If
		If avSJCollected(nPlayer).Contains(eMballCyber) Then
			anBonusElements(eMballCyber) = anBonusElements(eMballCyber) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeWater1) Then
			anBonusElements(eMballWater) = anBonusElements(eMballWater) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeWater2) Then
			anBonusElements(eMballWater) = anBonusElements(eMballWater) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeWaterWizard) Then
			anBonusElements(eMballWater) = anBonusElements(eMballWater) + 2000
		End If
		If avSJCollected(nPlayer).Contains(eMballWater) Then
			anBonusElements(eMballWater) = anBonusElements(eMballWater) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeFire1) Then
			anBonusElements(eMballFire) = anBonusElements(eMballFire) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeFire2) Then
			anBonusElements(eMballFire) = anBonusElements(eMballFire) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeFireWizard) Then
			anBonusElements(eMballFire) = anBonusElements(eMballFire) + 2000
		End If
		If avSJCollected(nPlayer).Contains(eMballFire) Then
			anBonusElements(eMballFire) = anBonusElements(eMballFire) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeEarth1) Then
			anBonusElements(eMballEarth) = anBonusElements(eMballEarth) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeEarth2) Then
			anBonusElements(eMballEarth) = anBonusElements(eMballEarth) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeEarthWizard) Then
			anBonusElements(eMballEarth) = anBonusElements(eMballEarth) + 2000
		End If
		If avSJCollected(nPlayer).Contains(eMballEarth) Then
			anBonusElements(eMballEarth) = anBonusElements(eMballEarth) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeAir1) Then
			anBonusElements(eMballAir) = anBonusElements(eMballAir) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeAir2) Then
			anBonusElements(eMballAir) = anBonusElements(eMballAir) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeAirWizard) Then
			anBonusElements(eMballAir) = anBonusElements(eMballAir) + 2000
		End If
		If avSJCollected(nPlayer).Contains(eMballAir) Then
			anBonusElements(eMballAir) = anBonusElements(eMballAir) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeIce1) Then
			anBonusElements(eMballIce) = anBonusElements(eMballIce) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeIce2) Then
			anBonusElements(eMballIce) = anBonusElements(eMballIce) + 1000
		End If
		If avModesPlayed(nPlayer).Contains(eModeIceWizard) Then
			anBonusElements(eMballIce) = anBonusElements(eMballIce) + 2000
		End If
		If avSJCollected(nPlayer).Contains(eMballIce) Then
			anBonusElements(eMballIce) = anBonusElements(eMballIce) + 1000
		End If
		for i = 0 to 5 : nBonusTotal = nBonusTotal + anBonusElements(i) : next
		anScore(nPlayer) = anScore(nPlayer) + (nBonusX * nBonusTotal)
	Else
		nBonusTotal = 0
		for i = 0 to 5 : nBonusTotal = nBonusTotal + anBonusElements(i) : next
		ShowText "BONUS LOST", WilliamsFormatNum(nBonusX * nBonusTotal), _
			2000, eVOPrioNewPlayer
		EndBall
	End If
End Sub

Sub TimerBonus_Timer
	Dim i
	Dim nTimesCountUp
	Dim nBonusCountUp
	Dim dFractionCounted
	Dim nBurnFrame

	nBonusFrame = nBonusFrame + 1
	If 1 = nBonusFrame Then
		PlaySound "MUS_bonus_count_main", 0, fMusicVolume
	End If

	If nBonusFrame < 13 then
		ShowText "END OF BALL", "BONUS PLAYER " & (1 + nPlayer), _
			TimerBonus.interval, eVOPrioNewPlayer
	Elseif nBonusFrame < 38 Then
		if 13 = nBonusFrame Then
			LightCharacterCyber.state = LightStateOn
'			Lampz.SetLamp 88, 1
			LightShowEnd
			LightShowStart eLightShowCyberBonus, eVOPrioNewPlayer
		end if
		ShowBonusBurn "  CYBER ", anBonusElements(eMballCyber), nBonusFrame - 12
	Elseif nBonusFrame < 63 Then
		if 38 = nBonusFrame Then
			LightCharacterCyber.state = LightStateOff
			LightCharacterWater.state = LightStateOn
'			Lampz.SetLamp 88, 0
'			Lampz.SetLamp 89, 1
			LightShowEnd
			LightShowStart eLightShowWaterBonus, eVOPrioNewPlayer
		end if
		ShowBonusBurn "  WATER ", anBonusElements(eMBallWater), nBonusFrame - 37
	Elseif nBonusFrame < 88 Then
		if 63 = nBonusFrame Then
			LightCharacterWater.state = LightStateOff
			LightCharacterFire.state = LightStateOn
'			Lampz.SetLamp 89, 0
'			Lampz.SetLamp 90, 1
			LightShowEnd
			LightShowStart eLightShowFireBonus, eVOPrioNewPlayer
		end if
		ShowBonusBurn "   FIRE ", anBonusElements(eMballFire), nBonusFrame - 62
	Elseif nBonusFrame < 113 Then
		if 88 = nBonusFrame Then
			LightCharacterFire.state = LightStateOff
			LightCharacterEarth.state = LightStateOn
'			Lampz.SetLamp 90, 0
'			Lampz.SetLamp 91, 1
			LightShowEnd
			LightShowStart eLightShowEarthBonus, eVOPrioNewPlayer
		end if
		ShowBonusBurn "  EARTH ", anBonusElements(eMballEarth), nBonusFrame - 87
	Elseif nBonusFrame < 138 Then
		if 113 = nBonusFrame Then
			LightCharacterEarth.state = LightStateOff
			LightCharacterAir.state = LightStateOn
'			Lampz.SetLamp 91, 0
'			Lampz.SetLamp 92, 1
			LightShowEnd
			LightShowStart eLightShowAirBonus, eVOPrioNewPlayer
		end if
		ShowBonusBurn "    AIR ", anBonusElements(eMballAir), nBonusFrame - 112
	Elseif nBonusFrame < 163 Then
		if 138 = nBonusFrame Then
			LightCharacterAir.state = LightStateOff
			LightCharacterIce.state = LightStateOn
'			Lampz.SetLamp 92, 0
'			Lampz.SetLamp 93, 1
			LightShowEnd
			LightShowStart eLightShowIceBonus, eVOPrioNewPlayer
		end if
		ShowBonusBurn "    ICE ", anBonusElements(eMballIce), nBonusFrame - 137
	Elseif nBonusFrame < 225 Then
		if 163 = nBonusFrame Then
			LightCharacterIce.state = LightStateOff
'			Lampz.SetLamp 93, 0
			LightShowEnd
			LightShowStart eLightShowBonusX, eVOPrioNewPlayer
		end if
		if 188 = nBonusFrame Then LightShowEnd : LightShowStart eLightShowModeComplete, eVOPrioNewPlayer
		nBurnFrame = nBonusFrame - 162
		if nBurnFrame < 27 Then
			dFractionCounted = 0
			nTimesCountUp = 1
			nBonusCountUp = 0
		Else
			dFractionCounted = (nBurnFrame - 26) / 37
			nTimesCountUp = Int(1.99 + ((nBonusX - 1) * dFractionCounted))
			nBonusCountUp = nBonusDisplayed * nBonusX * dFractionCounted
			' Round to multiple of 10
			nBonusCountUp = 10 * (nBonusCountUp \ 10)
		End If
		ShowText WilliamsFormatNum(nBonusTotal) & " X " & nTimesCountUp & " = ", _
			"  BONUS " & getFormattedBonus(nBonusCountUp), _
			TimerBonus.interval, _
			eVOPrioNewPlayer
	Elseif nBonusFrame = 225 Then
		LightShowEnd
		For each i in GI : i.State = 0 : Next
		For i = eCharacterCyber to eCharacterIce
			aoCharLights(i).state = LightStateOff
		Next
		StopSound "MUS_bonus_count_main"
		PlaySound "MUS_bonus_count_end", 0, fMusicVolume
		If anScore(nPlayer) < 1e7 Then ' score is less than 10 million
			ShowText "PLAYER " & (1 + nPlayer) & " " & getFormattedBonus(anScore(nPlayer)), _
			"  BONUS " & getFormattedBonus(nBonusX * nBonusTotal), 2000, eVOPrioNewPlayer
		Else ' score is 10 million or more
			ShowText "P" & (1 + nPlayer) & "  " & WilliamsFormatNum(anScore(nPlayer)), _
			"  BONUS " & getFormattedBonus(nBonusX * nBonusTotal), 2000, eVOPrioNewPlayer
		End If
	Elseif nBonusFrame < 275 Then
		' Do nothing, just show total for longer
	Else
		For each i in GI : i.State = 1 : Next
		TimerBonus.enabled = False
		bCountingBonus = False
		EndBall
		Exit Sub
	End If
End Sub

Sub ShowBonusBurn(sVillain, nValue, nFrame)
	Dim nValueCountDown
	Dim nBonusCountUp
	Dim dFractionCounted
	If nFrame < 7 Then
		nValueCountDown = nValue
		nBonusCountUp = nBonusDisplayed
	Else
		dFractionCounted = (nFrame - 6) / 19
		nValueCountDown = Int(nValue * (1 - dFractionCounted))
		nBonusCountUp = nBonusDisplayed + Int(nValue * (dFractionCounted))
		' Round to multiple of 10
		If nValue >= 100 and nFrame < 25 Then
			nValueCountDown = 10 * (nValueCountDown \ 10)
			nBonusCountUp = 10 * (nBonusCountUp \ 10)
		End If
	End If

	ShowText sVillain & getFormattedBonus(nValueCountDown), _
		"  BONUS " & getFormattedBonus(nBonusCountUp), _
		TimerBonus.interval, _
		eVOPrioNewPlayer
	If 25 = nFrame Then
		nBonusDisplayed = nBonusDisplayed + nValue
	End If
End Sub

Sub ApplyLampRGB( nr, RGBval )
	If nr > 0 And nr < 17 Then
		If anCurLightColor(nr) = RGBval Then
			Exit Sub
		Else
			anCurLightColor(nr) = RGBval
		End If
	End If
	If IsArray(Lampz.Obj(nr)) Then ' double check so it wont fail
		dim tmp : tmp = Lampz.Obj(nr)
		dim pon : set pon = Lampz.OnPrim(nr)
		tmp(1).ColorFull = RGBval
		UpdateMaterial "InsertRBG" & nr,0,0,1,0,0,0,0.9999,RGBval,0,0,False,True,0,0,0,0
'		pon.BlendDisableLighting = 300  'TODO: Make this a function of color
	Else
		'Debug.print "not an array in ApplyLampRGB " & nr
		' FIX can remove this check aswell when
	End If
End Sub

'******************************************************
'****  ZLMP : LAMPZ by nFozzy
'******************************************************
' 
' Lampz is a utility designed to manage and fade the lights and light-related objects on a table that is being driven by a ROM.
' To set up Lampz, one must populate the Lampz.MassAssign array with VPX Light objects, where the index of the MassAssign array
' corrisponds to the ROM index of the associated light. More that one Light object can be associated with a single MassAssign index (not shown in this example)
' Optionally, callbacks can be assigned for each index using the Lampz.Callback array. This is very useful for allowing 3D Insert primitives
' to be controlled by the ROM. Note, the aLvl parameter (i.e. the fading level that ranges between 0 and 1) is appended to the callback call.



Dim NullFader : set NullFader = new NullFadingObject
Dim NullPrim : set NullPrim = new NullPrimObject
Dim Lampz : Set Lampz = New LampFader

LampTimer.Interval = -1
LampTimer.Enabled = 1

'Sub LampTimer_Timer()
'	dim x, chglamp
'	If Not IsEmpty(chglamp) Then
'		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
'			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
'		next
'	End If
'	Lampz.Update2
'End Sub

Dim nTimeReadControlLights
nTimeReadControlLights = 0
Sub LampTimer_Timer()
   'apophis - Use the InPlayState of the 1st light in the Lampz.obj array to set the Lampz.state
	dim idx
	If GameTime - nTimeReadControlLights > 16 then
	nTimeReadControlLights = GameTime
	for idx = 0 to 93 'uBound(Lampz.Obj) - 7
		if Lampz.IsLight(idx) then 
			if IsArray(Lampz.obj(idx)) then
				dim tmp : tmp = Lampz.obj(idx)
				Lampz.state(idx) = tmp(0).GetInPlayStateBool
				'debug.print tmp(0).name & " " &  tmp(0).GetInPlayStateBool & " " & tmp(0).IntensityScale  & vbnewline
			Else
				Lampz.state(idx) = Lampz.obj(idx).GetInPlayStateBool
				'debug.print Lampz.obj(idx).name & " " &  Lampz.obj(idx).GetInPlayStateBool & " " & Lampz.obj(idx).IntensityScale  & vbnewline
			end if
		end if
	Next
	end if
	Lampz.Update2
End Sub

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub SetModLamp(id, val)
	Lampz.state(id) = val
End Sub

Sub UpdateLightmap(lightmap, intensity, ByVal aLvl)  ' additiveblend prims
'	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)    'Callbacks don't get this filter automatically
'    lightmap.Opacity = aLvl * intensity
End Sub


Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (max level / full MS fading time). The Modulate property must be set to 1 / max level if lamp is modulated.
	dim x : for x = 0 to 150 : Lampz.FadeSpeedUp(x) = 1/40 : Lampz.FadeSpeedDown(x) = 1/100 : Lampz.Modulate(x) = 1 : next
	

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	Lampz.MassAssign(1) = lightUnderFlipper001
	Lampz.MassAssign(1) = l01
	Lampz.Callback(1) = "DisableLighting p01, 250,"
	Lampz.SetOnPrim(1) = p01
	Lampz.OnPrim(1).blenddisablelighting = 250
	
	Lampz.MassAssign(2) = lightUnderFlipper002
	Lampz.MassAssign(2) = l02
	Lampz.Callback(2) = "DisableLighting p02, 250,"
	Lampz.SetOnPrim(2) = p02
	Lampz.OnPrim(2).blenddisablelighting = 250
	
	Lampz.MassAssign(3) = lightLeftLoop1
	Lampz.MassAssign(3) = l03
	Lampz.Callback(3) = "DisableLighting p03, 250,"
	Lampz.SetOnPrim(3) = p03
	Lampz.OnPrim(3).blenddisablelighting = 250
	
	Lampz.MassAssign(4) = lightLeftLoop2
	Lampz.MassAssign(4) = l04
	Lampz.Callback(4) = "DisableLighting p04, 250,"
	Lampz.SetOnPrim(4) = p04
	Lampz.OnPrim(4).blenddisablelighting = 250
	
	Lampz.MassAssign(5) = lightLeftScoop001
	Lampz.MassAssign(5) = l05
	Lampz.Callback(5) = "DisableLighting p05, 250,"
	Lampz.SetOnPrim(5) = p05
	Lampz.OnPrim(5).blenddisablelighting = 250
	
	Lampz.MassAssign(6) = lightLeftScoop002
	Lampz.MassAssign(6) = l06
	Lampz.Callback(6) = "DisableLighting p06, 250,"
	Lampz.SetOnPrim(6) = p06
	Lampz.OnPrim(6).blenddisablelighting = 250
	
	Lampz.MassAssign(7) = lightLeftRamp001
	Lampz.MassAssign(7) = l07
	Lampz.Callback(7) = "DisableLighting p07, 250,"
	Lampz.SetOnPrim(7) = p07
	Lampz.OnPrim(7).blenddisablelighting = 250
	
	Lampz.MassAssign(8) = lightLeftRamp002
	Lampz.MassAssign(8) = l08
	Lampz.Callback(8) = "DisableLighting p08, 250,"
	Lampz.SetOnPrim(8) = p08
	Lampz.OnPrim(8).blenddisablelighting = 250
	
	Lampz.MassAssign(9) = lightMole001
	Lampz.MassAssign(9) = l09
	Lampz.Callback(9) = "DisableLighting p09, 250,"
	Lampz.SetOnPrim(9) = p09
	Lampz.OnPrim(9).blenddisablelighting = 250
	
	Lampz.MassAssign(10) = lightMole002
	Lampz.MassAssign(10) = l10
	Lampz.Callback(10) = "DisableLighting p10, 250,"
	Lampz.SetOnPrim(10) = p10
	Lampz.OnPrim(10).blenddisablelighting = 250
	
	Lampz.MassAssign(11) = lightSideRamp
	Lampz.MassAssign(11) = l11
	Lampz.Callback(11) = "DisableLighting p11, 250,"
	Lampz.SetOnPrim(11) = p11
	Lampz.OnPrim(11).blenddisablelighting = 250
	
	Lampz.MassAssign(12) = LightSuperJackpot
	Lampz.MassAssign(12) = l12
	Lampz.Callback(12) = "DisableLighting p12, 250,"
	Lampz.SetOnPrim(12) = p12
	Lampz.OnPrim(12).blenddisablelighting = 250
	
	Lampz.MassAssign(13) = lightRightRamp001
	Lampz.MassAssign(13) = l13
	Lampz.Callback(13) = "DisableLighting p13, 250,"
	Lampz.SetOnPrim(13) = p13
	Lampz.OnPrim(13).blenddisablelighting = 250
	
	Lampz.MassAssign(14) = lightRightRamp002
	Lampz.MassAssign(14) = l14
	Lampz.Callback(14) = "DisableLighting p14, 250,"
	Lampz.SetOnPrim(14) = p14
	Lampz.OnPrim(14).blenddisablelighting = 250
	
	Lampz.MassAssign(15) = lightRightLoop001
	Lampz.MassAssign(15) = l15
	Lampz.Callback(15) = "DisableLighting p15, 250,"
	Lampz.SetOnPrim(15) = p15
	Lampz.OnPrim(15).blenddisablelighting = 250
	
	Lampz.MassAssign(16) = lightRightLoop002
	Lampz.MassAssign(16) = l16
	Lampz.Callback(16) = "DisableLighting p16, 250,"
	Lampz.SetOnPrim(16) = p16
	Lampz.OnPrim(16).blenddisablelighting = 250
	
	Lampz.MassAssign(17) = lightShootAgain
	Lampz.MassAssign(17) = l17
	Lampz.Callback(17) = "DisableLighting p17, 250,"
	Lampz.SetOnPrim(17) = p17
	Lampz.OnPrim(17).blenddisablelighting = 250
	
	Lampz.MassAssign(18) = LightExtraBall
	Lampz.MassAssign(18) = l18
	Lampz.Callback(18) = "DisableLighting p18, 250,"
	Lampz.SetOnPrim(18) = p18
	Lampz.OnPrim(18).blenddisablelighting = 250
	
	Lampz.MassAssign(19) = LightMode001
	Lampz.MassAssign(19) = l19
	Lampz.Callback(19) = "DisableLighting p19, 250,"
	Lampz.SetOnPrim(19) = p19
	Lampz.OnPrim(19).blenddisablelighting = 250
	
	Lampz.MassAssign(20) = LightMode003
	Lampz.MassAssign(20) = l20
	Lampz.Callback(20) = "DisableLighting p20, 250,"
	Lampz.SetOnPrim(20) = p20
	Lampz.OnPrim(20).blenddisablelighting = 250
	
	Lampz.MassAssign(21) = LightMode005
	Lampz.MassAssign(21) = l21
	Lampz.Callback(21) = "DisableLighting p21, 250,"
	Lampz.SetOnPrim(21) = p21
	Lampz.OnPrim(21).blenddisablelighting = 250
	
	Lampz.MassAssign(22) = LightMode007
	Lampz.MassAssign(22) = l22
	Lampz.Callback(22) = "DisableLighting p22, 250,"
	Lampz.SetOnPrim(22) = p22
	Lampz.OnPrim(22).blenddisablelighting = 250
	
	Lampz.MassAssign(23) = LightMode009
	Lampz.MassAssign(23) = l23
	Lampz.Callback(23) = "DisableLighting p23, 250,"
	Lampz.SetOnPrim(23) = p23
	Lampz.OnPrim(23).blenddisablelighting = 250
	
	Lampz.MassAssign(24) = LightMode011
	Lampz.MassAssign(24) = l24
	Lampz.Callback(24) = "DisableLighting p24, 250,"
	Lampz.SetOnPrim(24) = p24
	Lampz.OnPrim(24).blenddisablelighting = 250
	
	Lampz.MassAssign(25) = LightMode002
	Lampz.MassAssign(25) = l25
	Lampz.Callback(25) = "DisableLighting p25, 250,"
	Lampz.SetOnPrim(25) = p25
	Lampz.OnPrim(25).blenddisablelighting = 250
	
	Lampz.MassAssign(26) = LightMode004
	Lampz.MassAssign(26) = l26
	Lampz.Callback(26) = "DisableLighting p26, 250,"
	Lampz.SetOnPrim(26) = p26
	Lampz.OnPrim(26).blenddisablelighting = 250
	
	Lampz.MassAssign(27) = LightMode006
	Lampz.MassAssign(27) = l27
	Lampz.Callback(27) = "DisableLighting p27, 250,"
	Lampz.SetOnPrim(27) = p27
	Lampz.OnPrim(27).blenddisablelighting = 250
	
	Lampz.MassAssign(28) = LightMode008
	Lampz.MassAssign(28) = l28
	Lampz.Callback(28) = "DisableLighting p28, 250,"
	Lampz.SetOnPrim(28) = p28
	Lampz.OnPrim(28).blenddisablelighting = 250
	
	Lampz.MassAssign(29) = LightMode010
	Lampz.MassAssign(29) = l29
	Lampz.Callback(29) = "DisableLighting p29, 250,"
	Lampz.SetOnPrim(29) = p29
	Lampz.OnPrim(29).blenddisablelighting = 250
	
	Lampz.MassAssign(30) = LightMode012
	Lampz.MassAssign(30) = l30
	Lampz.Callback(30) = "DisableLighting p30, 250,"
	Lampz.SetOnPrim(30) = p30
	Lampz.OnPrim(30).blenddisablelighting = 250
	
	Lampz.MassAssign(31) = LightSuperCyber
	Lampz.MassAssign(31) = l31
	Lampz.Callback(31) = "DisableLighting p31, 250,"
	Lampz.SetOnPrim(31) = p31
	Lampz.OnPrim(31).blenddisablelighting = 250
	
	Lampz.MassAssign(32) = LightSuperWater
	Lampz.MassAssign(32) = l32
	Lampz.Callback(32) = "DisableLighting p32, 250,"
	Lampz.SetOnPrim(32) = p32
	Lampz.OnPrim(32).blenddisablelighting = 250
	
	Lampz.MassAssign(33) = LightSuperFire
	Lampz.MassAssign(33) = l33
	Lampz.Callback(33) = "DisableLighting p33, 250,"
	Lampz.SetOnPrim(33) = p33
	Lampz.OnPrim(33).blenddisablelighting = 250
	
	Lampz.MassAssign(34) = LightSuperEarth
	Lampz.MassAssign(34) = l34
	Lampz.Callback(34) = "DisableLighting p34, 250,"
	Lampz.SetOnPrim(34) = p34
	Lampz.OnPrim(34).blenddisablelighting = 250
	
	Lampz.MassAssign(35) = LightSuperAir
	Lampz.MassAssign(35) = l35
	Lampz.Callback(35) = "DisableLighting p35, 250,"
	Lampz.SetOnPrim(35) = p35
	Lampz.OnPrim(35).blenddisablelighting = 250
	
	Lampz.MassAssign(36) = LightSuperIce
	Lampz.MassAssign(36) = l36
	Lampz.Callback(36) = "DisableLighting p36, 250,"
	Lampz.SetOnPrim(36) = p36
	Lampz.OnPrim(36).blenddisablelighting = 250
	
	Lampz.MassAssign(37) = lightLaneF
	Lampz.MassAssign(37) = l37
	Lampz.Callback(37) = "DisableLighting p37, 250,"
	Lampz.SetOnPrim(37) = p37
	Lampz.OnPrim(37).blenddisablelighting = 250
	
	Lampz.MassAssign(38) = lightLaneI
	Lampz.MassAssign(38) = l38
	Lampz.Callback(38) = "DisableLighting p38, 250,"
	Lampz.SetOnPrim(38) = p38
	Lampz.OnPrim(38).blenddisablelighting = 250
	
	Lampz.MassAssign(39) = lightLaneR
	Lampz.MassAssign(39) = l39
	Lampz.Callback(39) = "DisableLighting p39, 250,"
	Lampz.SetOnPrim(39) = p39
	Lampz.OnPrim(39).blenddisablelighting = 250
	
	Lampz.MassAssign(40) = lightLaneE
	Lampz.MassAssign(40) = l40
	Lampz.Callback(40) = "DisableLighting p40, 250,"
	Lampz.SetOnPrim(40) = p40
	Lampz.OnPrim(40).blenddisablelighting = 250
	
	Lampz.MassAssign(41) = LightScore2X
	Lampz.MassAssign(41) = l41
	Lampz.Callback(41) = "DisableLighting p41, 250,"
	Lampz.SetOnPrim(41) = p41
	Lampz.OnPrim(41).blenddisablelighting = 250
	
	Lampz.MassAssign(42) = LightScore3X
	Lampz.MassAssign(42) = l42
	Lampz.Callback(42) = "DisableLighting p42, 250,"
	Lampz.SetOnPrim(42) = p42
	Lampz.OnPrim(42).blenddisablelighting = 250
	
	Lampz.MassAssign(43) = lightX1
	Lampz.MassAssign(43) = l43
	Lampz.Callback(43) = "DisableLighting p43, 250,"
	Lampz.SetOnPrim(43) = p43
	Lampz.OnPrim(43).blenddisablelighting = 250
	
	Lampz.MassAssign(44) = lightX2
	Lampz.MassAssign(44) = l44
	Lampz.Callback(44) = "DisableLighting p44, 250,"
	Lampz.SetOnPrim(44) = p44
	Lampz.OnPrim(44).blenddisablelighting = 250
	
	Lampz.MassAssign(45) = lightX003
	Lampz.MassAssign(45) = l45
	Lampz.Callback(45) = "DisableLighting p45, 250,"
	Lampz.SetOnPrim(45) = p45
	Lampz.OnPrim(45).blenddisablelighting = 250
	
	Lampz.MassAssign(46) = lightX004
	Lampz.MassAssign(46) = l46
	Lampz.Callback(46) = "DisableLighting p46, 250,"
	Lampz.SetOnPrim(46) = p46
	Lampz.OnPrim(46).blenddisablelighting = 250
	
	Lampz.MassAssign(47) = LightScoreX
	Lampz.MassAssign(47) = l47
	Lampz.Callback(47) = "DisableLighting p47, 250,"
	Lampz.SetOnPrim(47) = p47
	Lampz.OnPrim(47).blenddisablelighting = 250
	
	Lampz.MassAssign(48) = lightMystery
	Lampz.MassAssign(48) = l48
	Lampz.Callback(48) = "DisableLighting p48, 250,"
	Lampz.SetOnPrim(48) = p48
	Lampz.OnPrim(48).blenddisablelighting = 250
	
	Lampz.MassAssign(49) = lightMyst001
	Lampz.MassAssign(49) = l49
	Lampz.Callback(49) = "DisableLighting p49, 250,"
	Lampz.SetOnPrim(49) = p49
	Lampz.OnPrim(49).blenddisablelighting = 250
	
	Lampz.MassAssign(50) = lightMyst002
	Lampz.MassAssign(50) = l50
	Lampz.Callback(50) = "DisableLighting p50, 250,"
	Lampz.SetOnPrim(50) = p50
	Lampz.OnPrim(50).blenddisablelighting = 250
	
	Lampz.MassAssign(51) = lightMyst003
	Lampz.MassAssign(51) = l51
	Lampz.Callback(51) = "DisableLighting p51, 250,"
	Lampz.SetOnPrim(51) = p51
	Lampz.OnPrim(51).blenddisablelighting = 250
	
	Lampz.MassAssign(52) = lightMyst004
	Lampz.MassAssign(52) = l52
	Lampz.Callback(52) = "DisableLighting p52, 250,"
	Lampz.SetOnPrim(52) = p52
	Lampz.OnPrim(52).blenddisablelighting = 250
	
	Lampz.MassAssign(53) = lightComboShatz
	Lampz.MassAssign(53) = l53
	Lampz.Callback(53) = "DisableLighting p53, 250,"
	Lampz.SetOnPrim(53) = p53
	Lampz.OnPrim(53).blenddisablelighting = 250
	
	Lampz.MassAssign(54) = lightComboLLoop
	Lampz.MassAssign(54) = l54
	Lampz.Callback(54) = "DisableLighting p54, 250,"
	Lampz.SetOnPrim(54) = p54
	Lampz.OnPrim(54).blenddisablelighting = 250
	
	Lampz.MassAssign(55) = lightComboSaucer
	Lampz.MassAssign(55) = l55
	Lampz.Callback(55) = "DisableLighting p55, 250,"
	Lampz.SetOnPrim(55) = p55
	Lampz.OnPrim(55).blenddisablelighting = 250
	
	Lampz.MassAssign(56) = lightComboLRamp
	Lampz.MassAssign(56) = l56
	Lampz.Callback(56) = "DisableLighting p56, 250,"
	Lampz.SetOnPrim(56) = p56
	Lampz.OnPrim(56).blenddisablelighting = 250
	
	Lampz.MassAssign(57) = lightComboEarth
	Lampz.MassAssign(57) = l57
	Lampz.Callback(57) = "DisableLighting p57, 250,"
	Lampz.SetOnPrim(57) = p57
	Lampz.OnPrim(57).blenddisablelighting = 250
	
	Lampz.MassAssign(58) = LightComboDummy
	Lampz.MassAssign(58) = l58
	Lampz.Callback(58) = "DisableLighting p58, 250,"
	Lampz.SetOnPrim(58) = p58
	Lampz.OnPrim(58).blenddisablelighting = 250
	
	Lampz.MassAssign(59) = lightComboRRamp
	Lampz.MassAssign(59) = l59
	Lampz.Callback(59) = "DisableLighting p59, 250,"
	Lampz.SetOnPrim(59) = p59
	Lampz.OnPrim(59).blenddisablelighting = 250
	
	Lampz.MassAssign(60) = lightComboRLoop
	Lampz.MassAssign(60) = l60
	Lampz.Callback(60) = "DisableLighting p60, 250,"
	Lampz.SetOnPrim(60) = p60
	Lampz.OnPrim(60).blenddisablelighting = 250
	
	Lampz.MassAssign(61) = lightAir001
	Lampz.MassAssign(61) = l61
	Lampz.Callback(61) = "DisableLighting p61, 250,"
	Lampz.SetOnPrim(61) = p61
	Lampz.OnPrim(61).blenddisablelighting = 250
	
	Lampz.MassAssign(62) = lightAir002
	Lampz.MassAssign(62) = l62
	Lampz.Callback(62) = "DisableLighting p62, 250,"
	Lampz.SetOnPrim(62) = p62
	Lampz.OnPrim(62).blenddisablelighting = 250
	
	Lampz.MassAssign(63) = lightAir003
	Lampz.MassAssign(63) = l63
	Lampz.Callback(63) = "DisableLighting p63, 250,"
	Lampz.SetOnPrim(63) = p63
	Lampz.OnPrim(63).blenddisablelighting = 250
	
	Lampz.MassAssign(64) = lightAir004
	Lampz.MassAssign(64) = l64
	Lampz.Callback(64) = "DisableLighting p64, 250,"
	Lampz.SetOnPrim(64) = p64
	Lampz.OnPrim(64).blenddisablelighting = 250
	
	Lampz.MassAssign(65) = lightAir005
	Lampz.MassAssign(65) = l65
	Lampz.Callback(65) = "DisableLighting p65, 250,"
	Lampz.SetOnPrim(65) = p65
	Lampz.OnPrim(65).blenddisablelighting = 250
	
	Lampz.MassAssign(66) = lightAir006
	Lampz.MassAssign(66) = l66
	Lampz.Callback(66) = "DisableLighting p66, 250,"
	Lampz.SetOnPrim(66) = p66
	Lampz.OnPrim(66).blenddisablelighting = 250
	
	Lampz.MassAssign(67) = lightAir007
	Lampz.MassAssign(67) = l67
	Lampz.Callback(67) = "DisableLighting p67, 250,"
	Lampz.SetOnPrim(67) = p67
	Lampz.OnPrim(67).blenddisablelighting = 250
	
	Lampz.MassAssign(68) = lightAir008
	Lampz.MassAssign(68) = l68
	Lampz.Callback(68) = "DisableLighting p68, 250,"
	Lampz.SetOnPrim(68) = p68
	Lampz.OnPrim(68).blenddisablelighting = 250
	
	Lampz.MassAssign(69) = lightTop001
	Lampz.MassAssign(69) = l69
	Lampz.Callback(69) = "DisableLighting p69, 250,"
	Lampz.SetOnPrim(69) = p69
	Lampz.OnPrim(69).blenddisablelighting = 250
	
	Lampz.MassAssign(70) = lightTop002
	Lampz.MassAssign(70) = l70
	Lampz.Callback(70) = "DisableLighting p70, 250,"
	Lampz.SetOnPrim(70) = p70
	Lampz.OnPrim(70).blenddisablelighting = 250
	
	Lampz.MassAssign(71) = lightTop003
	Lampz.MassAssign(71) = l71
	Lampz.Callback(71) = "DisableLighting p71, 250,"
	Lampz.SetOnPrim(71) = p71
	Lampz.OnPrim(71).blenddisablelighting = 250
	
	Lampz.MassAssign(72) = LightLock1Water
	Lampz.MassAssign(72) = l72
	Lampz.Callback(72) = "DisableLighting p72, 250,"
	Lampz.SetOnPrim(72) = p72
	Lampz.OnPrim(72).blenddisablelighting = 250
	
	Lampz.MassAssign(73) = LightLock2Water
	Lampz.MassAssign(73) = l73
	Lampz.Callback(73) = "DisableLighting p73, 250,"
	Lampz.SetOnPrim(73) = p73
	Lampz.OnPrim(73).blenddisablelighting = 250
	
	Lampz.MassAssign(74) = LightLock3Water
	Lampz.MassAssign(74) = l74
	Lampz.Callback(74) = "DisableLighting p74, 250,"
	Lampz.SetOnPrim(74) = p74
	Lampz.OnPrim(74).blenddisablelighting = 250
	
	Lampz.MassAssign(75) = LightLock1Fire
	Lampz.MassAssign(75) = l75
	Lampz.Callback(75) = "DisableLighting p75, 250,"
	Lampz.SetOnPrim(75) = p75
	Lampz.OnPrim(75).blenddisablelighting = 250
	
	Lampz.MassAssign(76) = LightLock2Fire
	Lampz.MassAssign(76) = l76
	Lampz.Callback(76) = "DisableLighting p76, 250,"
	Lampz.SetOnPrim(76) = p76
	Lampz.OnPrim(76).blenddisablelighting = 250
	
	Lampz.MassAssign(77) = LightLock3Fire
	Lampz.MassAssign(77) = l77
	Lampz.Callback(77) = "DisableLighting p77, 250,"
	Lampz.SetOnPrim(77) = p77
	Lampz.OnPrim(77).blenddisablelighting = 250
	
	Lampz.MassAssign(78) = LightLock1Earth
	Lampz.MassAssign(78) = l78
	Lampz.Callback(78) = "DisableLighting p78, 250,"
	Lampz.SetOnPrim(78) = p78
	Lampz.OnPrim(78).blenddisablelighting = 250
	
	Lampz.MassAssign(79) = LightLock2Earth
	Lampz.MassAssign(79) = l79
	Lampz.Callback(79) = "DisableLighting p79, 250,"
	Lampz.SetOnPrim(79) = p79
	Lampz.OnPrim(79).blenddisablelighting = 250
	
	Lampz.MassAssign(80) = LightLock3Earth
	Lampz.MassAssign(80) = l80
	Lampz.Callback(80) = "DisableLighting p80, 250,"
	Lampz.SetOnPrim(80) = p80
	Lampz.OnPrim(80).blenddisablelighting = 250
	
	Lampz.MassAssign(81) = LightLock1Ice
	Lampz.MassAssign(81) = l81
	Lampz.Callback(81) = "DisableLighting p81, 250,"
	Lampz.SetOnPrim(81) = p81
	Lampz.OnPrim(81).blenddisablelighting = 250
	
	Lampz.MassAssign(82) = LightLock2Ice
	Lampz.MassAssign(82) = l82
	Lampz.Callback(82) = "DisableLighting p82, 250,"
	Lampz.SetOnPrim(82) = p82
	Lampz.OnPrim(82).blenddisablelighting = 250
	
	Lampz.MassAssign(83) = LightLock3Ice
	Lampz.MassAssign(83) = l83
	Lampz.Callback(83) = "DisableLighting p83, 250,"
	Lampz.SetOnPrim(83) = p83
	Lampz.OnPrim(83).blenddisablelighting = 250
	
	Lampz.MassAssign(84) = lightIce001
	Lampz.MassAssign(84) = l84
	Lampz.Callback(84) = "DisableLighting p84, 250,"
	Lampz.SetOnPrim(84) = p84
	Lampz.OnPrim(84).blenddisablelighting = 250
	
	Lampz.MassAssign(85) = lightIce002
	Lampz.MassAssign(85) = l85
	Lampz.Callback(85) = "DisableLighting p85, 250,"
	Lampz.SetOnPrim(85) = p85
	Lampz.OnPrim(85).blenddisablelighting = 250
	
	Lampz.MassAssign(86) = lightIce003
	Lampz.MassAssign(86) = l86
	Lampz.Callback(86) = "DisableLighting p86, 250,"
	Lampz.SetOnPrim(86) = p86
	Lampz.OnPrim(86).blenddisablelighting = 250
	
	Lampz.MassAssign(87) = lightTorpedo
	Lampz.MassAssign(87) = l87
	Lampz.Callback(87) = "DisableLighting p87, 250,"
	Lampz.SetOnPrim(87) = p87
	Lampz.OnPrim(87).blenddisablelighting = 250

	Lampz.MassAssign(88) = LightCharacterCyber
	Lampz.MassAssign(88) = l88
	Lampz.Callback(88) = "DisableLighting p88, 250,"
	Lampz.SetOnPrim(88) = p88
	Lampz.OnPrim(88).blenddisablelighting = 250

	Lampz.MassAssign(89) = LightCharacterWater
	Lampz.MassAssign(89) = l89
	Lampz.Callback(89) = "DisableLighting p89, 250,"
	Lampz.SetOnPrim(89) = p89
	Lampz.OnPrim(89).blenddisablelighting = 250

	Lampz.MassAssign(90) = LightCharacterFire
	Lampz.MassAssign(90) = l90
	Lampz.Callback(90) = "DisableLighting p90, 250,"
	Lampz.SetOnPrim(90) = p90
	Lampz.OnPrim(90).blenddisablelighting = 250

	Lampz.MassAssign(91) = LightCharacterEarth
	Lampz.MassAssign(91) = l91
	Lampz.Callback(91) = "DisableLighting p91, 250,"
	Lampz.SetOnPrim(91) = p91
	Lampz.OnPrim(91).blenddisablelighting = 250

	Lampz.MassAssign(92) = LightCharacterAir
	Lampz.MassAssign(92) = l92
	Lampz.Callback(92) = "DisableLighting p92, 250,"
	Lampz.SetOnPrim(92) = p92
	Lampz.OnPrim(92).blenddisablelighting = 250

	Lampz.MassAssign(93) = LightCharacterIce
	Lampz.MassAssign(93) = l93
	Lampz.Callback(93) = "DisableLighting p93, 250,"
	Lampz.SetOnPrim(93) = p93
	Lampz.OnPrim(93).blenddisablelighting = 250

	Lampz.MassAssign(94) = LightCharacterHero

	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub


'====================
'Class jungle nf
'====================

'No-op objects instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class
Class NullPrimObject 
	Public Property Let BlendDisableLighting(input) : : End Property
	Public Property Let Opacity(input) : : End Property
	Public Property Let ColorFull(input) : : End Property
	Public Property Let Color(input) : : End Property
End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks
'Version 0.14 - Updated to support modulated signals - Niwak
'Version 0.15 - added IsLight property and OnPrim objects to the class - apophis

Class LampFader
	Public IsLight(150)				'apophis
	Public OnPrim(150)				'apophis
	Public FadeSpeedDown(150), FadeSpeedUp(150)
	Private Lock(150), Loaded(150), OnOff(150)
	Public UseFunction
	Private cFilter
	Public UseCallback(150), cCallback(150)
	Public Lvl(150), Obj(150)
	Private Mult(150)
'	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = 0
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
			IsLight(x) = False   		'apophis
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x)) then Set Obj(x) = NullFader' : Loaded(x) = True
			if IsEmpty(OnPrim(x)) then Set OnPrim(x) = NullPrim' : Loaded(x) = True				'apophis
		Next
	End Sub

	Public Property Let SetOnPrim(aIdx, aInput) 	    'apophis
		If TypeName(aInput) = "Primitive" Then : set OnPrim(aIdx) = aInput : End If
	End Property

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		''debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next
		'msgbox "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if TypeName(input) <> "Double" and typename(input) <> "Integer"  and typename(input) <> "Long" then
			If input Then
				input = 1
			Else
				input = 0
			End If
		End If
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
				if typename(aInput) = "Light" then IsLight(aIdx) = True   'apophis - If first object in array is a light, this will be set true
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		''debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) > 0 then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= OnOff(x) then Lvl(x) = OnOff(x) : Lock(x) = True
				else 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) > 0 then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= OnOff(x) then Lvl(x) = OnOff(x) : Lock(x) = True
				else 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx, aLvl : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				aLvl = Lvl(x)*Mult(x)
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(aLvl) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = aLvl : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(aLvl)
					Else
						obj(x).Intensityscale = aLvl
					End If
				end if
				'if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" and typename(lvl(x)) <> "Long" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,aLvl	'Proc
				If Lock(x) Then
					if Lvl(x) = OnOff(x) or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class


'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
End Function


'Helper functions
Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String)
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
			AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function

'******************************************************
'****  END LAMPZ
'******************************************************


' Flupper's flashers

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim FlasherLevel(5), objbase(5), objlit(5), objflasher(5), objlight(5)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "white" : InitFlasher 2, "white" : InitFlasher 3, "white"
InitFlasher 4, "white" : InitFlasher 5, "white" 
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / nPi
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub SetFlasherColor(nr, nColor)
	Select Case nColor
		Case eColorBlue :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case eColorGreen:  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4) : objlight(nr).intensity = 1000
		Case eColorRed :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4) : objlight(nr).intensity = 1000
		Case eColorPurple : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) : objlight(nr).intensity = 1000
		Case eColorYellow : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50) : objlight(nr).intensity = 1000
		Case Else :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59) : objlight(nr).intensity = 1000
	End Select
	objlight(nr).colorfull = objlight(nr).color
End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * FlasherLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * FlasherLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * FlasherLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * FlasherLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,FlasherLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	FlasherLevel(nr) = FlasherLevel(nr) * 0.9 - 0.01
	If FlasherLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlashOnce(nFlasher, nColor)
	Dim nDOFNumber
	Select Case nColor
		Case eColorRed:		nDOFNumber = 140 + ((nFlasher - 1) * 6)
		Case eColorYellow:	nDOFNumber = 141 + ((nFlasher - 1) * 6)
		Case eColorGreen:	nDOFNumber = 142 + ((nFlasher - 1) * 6)
		Case eColorBlue:	nDOFNumber = 143 + ((nFlasher - 1) * 6)
		Case eColorPurple:	nDOFNumber = 144 + ((nFlasher - 1) * 6)
		Case Else:			nDOFNumber = 145 + ((nFlasher - 1) * 6)
	End Select
	DOF nDOFNumber, DOFPulse
	FlasherLevel(nFlasher) = 1 : SetFlasherColor nFlasher, nColor : FlashFlasher nFlasher
End Sub

Sub FlashSweep(nColor)
	If -1 = nFlashSweepStep Then
		If Not IsArray(nColor) Then
			If eColorNone = nColor Then
				Exit Sub
			End If
		End If
		nFlashSweepStep = 0
		nFlashColor = nColor
		nFlashRepeats = 0
		TimerFlasher.Enabled = True
	End If
End Sub

Sub TimerFlasher_timer
	Dim nColor, nLastIndex

	If IsArray(nFlashColor) Then
		nColor = nFlashColor(nFlashRepeats)
		nLastIndex = uBound(nFlashColor)
	Else
		nColor = nFlashColor
		nLastIndex = 0
	End If
	nFlashSweepStep = nFlashSweepStep + 1
	Select Case nFlashSweepStep
		Case 1
			FlashOnce 3, nColor
		Case 2
			FlashOnce 1, nColor
		Case 3
			FlashOnce 4, nColor
		Case 4
			FlashOnce 2, nColor
		Case 5
			FlashOnce 5, nColor
		Case Else
			If IsArray(nFlashColor) And (nFlashRepeats < nLastIndex) Then
				nFlashRepeats = nFlashRepeats + 1
				nFlashSweepStep = 0
			Else
				nFlashSweepStep = -1
				nFlashRepeats = 0
				TimerFlasher.Enabled = False
			End If
	End Select
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 

'***** High Score handling
Dim nHighScoreUpdates
Dim sLineOne
Dim sInitialsDisplayed

Sub HighscoreUpdate_Timer
	Dim i
	If HighscoreLetter = -1 Then
		SortedHighscoreNames(aScoreRanks(nHighScoreIter)) = InitialsEntered
		If nHighScoreIter < 3 Then
			For i = (nHighScoreIter + 1) to 3
				nHighScoreIter = i
				If aScoreRanks(i) < nHighScoreSlots Then
					InitHighscore
					Exit For
				End If
			Next
		Else
			HighscoreUpdate.enabled = False
			bEnteringHighScore = false
			savehs
			StopSound "music-TEMP-oom"
			nTextDuration = 0
			LightSeq1.UpdateInterval = 25
			LightSeq1.Play SeqCircleOutOn, 40
		End If
		Exit Sub
	End If
	If (nTimePressedRight > 0) And (gameTime - nTimePressedRight > 749) Then
		ChangeHighScoreLetter(1)
	End If
	If (nTimePressedLeft > 0) And (gameTime - nTimePressedLeft > 749) Then
		ChangeHighScoreLetter(-1)
	End If
	If nHighScoreUpdates < 5 Then
		sInitialsDisplayed = InitialsEntered & Mid(cHighScoreAlphabet, HighscoreLetter, 1) &_
			Space(2 - len(InitialsEntered))
	Else
		sInitialsDisplayed = InitialsEntered & "_" & Space(2 - len(InitialsEntered))
	End If
	ShowText sLineOne, getFormattedScore(nHighScoreIter) & " <" & _
		sInitialsDisplayed & ">", PlayUntilHit, eVOPrioNewPlayer
	nHighScoreUpdates = (nHighScoreUpdates + 1) mod 10
End Sub

Sub InitHighscore()
	bEnteringHighScore = True
	' SwitchMusic "music-oom"
	If aScoreRanks(nHighScoreIter) < 1 then
		sLineOne = "P" & (nHighScoreIter + 1) & " GRAND CHAMP"
	Else
		sLineOne = "P" & (nHighScoreIter + 1) & " HIGH SCORE " & (aScoreRanks(nHighScoreIter))
	End If
	ShowText sLineOne, getFormattedScore(nHighScoreIter) & " <   >", PlayUntilHit, eVOPrioNewPlayer
	InitialsEntered = ""
	HighscoreLetter = 1
	nHighScoreUpdates = 0
	nTimePressedLeft = -1
	nTimePressedRight = -1
	HighscoreUpdate.enabled = True
End Sub

Sub HighScoreRightPressed
	If nTimePressedRight < 0 Then nTimePressedRight = GameTime
End Sub

Sub HighScoreRightReleased
	If GameTime - nTimePressedRight < 750 Then ChangeHighScoreLetter(1)
	nTimePressedRight = -1
End Sub

Sub HighScoreLeftPressed
	If nTimePressedLeft < 0 Then nTimePressedLeft = GameTime
End Sub

Sub HighScoreLeftReleased
	If GameTime - nTimePressedLeft < 750 Then ChangeHighScoreLetter(-1)
	nTimePressedLeft = -1
End Sub

Const cBackspacePosition = 29 ' backspace is 29th letter in the highscore alphabet
Sub ChangeHighScoreLetter(offset)
	If HighscoreLetter = -1 then Exit Sub
	PlaySound "SFX_mode_move_beep",0,fSFXVolume,0,0,1,1,1
	PlaySound "fx-move_cursor", 0, 0.3
	HighscoreLetter = HighscoreLetter + offset
	If HighscoreLetter > len(cHighScoreAlphabet) Then HighscoreLetter = 1
	If HighscoreLetter < 1 Then HighscoreLetter = len(cHighScoreAlphabet)
	If HighscoreLetter = cBackspacePosition And 0 = Len(InitialsEntered) Then
		If offset < 0 Then
			HighscoreLetter = cBackspacePosition - 1
		Else
			HighscoreLetter = 1
		End If
	End If
	nHighScoreUpdates = 0
End Sub

Sub HighScoreEnterPressed
	' letter "<" means delete
	Dim Letter
	If HighscoreLetter = -1 then Exit Sub
	PlaySound "fx-start", 0, 1
	PlaySound "SFX_mode_select_beep",0,fSFXVolume,0,0,1,1,1
	Letter = Mid(cHighScoreAlphabet, HighscoreLetter, 1)
	If Letter = "<" And Len(InitialsEntered) > 0 Then
		Letter = Right(InitialsEntered, 1)
		InitialsEntered = Left(InitialsEntered, Len(InitialsEntered) - 1)
		sInitialsDisplayed = InitialsEntered & Letter & Space(2 - len(InitialsEntered))
		HighscoreLetter = inStr(1, cHighScoreAlphabet, Letter, vbTextCompare)
	Else
		InitialsEntered = InitialsEntered & Letter
		If Len(InitialsEntered) > 2 Then
			HighscoreLetter = -1
			sInitialsDisplayed = InitialsEntered
		Else
			sInitialsDisplayed = InitialsEntered & Letter & Space(2 - len(InitialsEntered))
		End If
	End If
	ShowText sLineOne, getFormattedScore(nHighScoreIter) & " <" & _
		sInitialsDisplayed & ">", PlayUntilHit, eVOPrioNewPlayer
End Sub

Sub Loadhs
    Dim x
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 926000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "SEC" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 750000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "RET" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 600000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "AGE" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 450000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "NT " End If
    x = LoadValue(cGameName, "HighScore5")
    If(x <> "") then HighScore(4) = CDbl(x) Else HighScore(4) = 350000 End If
    x = LoadValue(cGameName, "HighScore5Name")
    If(x <> "") then HighScoreName(4) = x Else HighScoreName(4) = "FOX" End If
End Sub

Sub Savehs
	Dim i
	For i = 0 to nHighScoreSlots - 1
		HighScore(i) = SortedHighscores(i)
		HighScoreName(i) = SortedHighscoreNames(i)
	Next
    SaveValue cGameName, "HighScore1", SortedHighscores(0)
    SaveValue cGameName, "HighScore1Name", SortedHighscoreNames(0)
    SaveValue cGameName, "HighScore2", SortedHighscores(1)
    SaveValue cGameName, "HighScore2Name", SortedHighscoreNames(1)
    SaveValue cGameName, "HighScore3", SortedHighscores(2)
    SaveValue cGameName, "HighScore3Name", SortedHighscoreNames(2)
    SaveValue cGameName, "HighScore4", SortedHighscores(3)
    SaveValue cGameName, "HighScore4Name", SortedHighscoreNames(3)
    SaveValue cGameName, "HighScore5", SortedHighscores(4)
    SaveValue cGameName, "HighScore5Name", SortedHighscoreNames(4)
End Sub

' Input: an array of scores of player 1, 2, etc.
' Output: an array of the position in the highscores of the player
Sub CheckHighScore(aScores)
	Dim aIndices()
	Dim scoreIndices()
	Dim i,j
	Dim highestScore
	Dim highestIndex
	Dim bWillEnterHighScore
	Dim temp

	Redim SortedHighscoreNames(nHighScoreSlots + UBound(aScores))
	Redim SortedHighscores(nHighScoreSlots + UBound(aScores))
	Redim aIndices(nHighScoreSlots + UBound(aScores))
	Redim scoreIndices(UBound(aScores))
	bWillEnterHighScore = False

	For i = 0 to UBound(SortedHighscores)
		aIndices(i) = i
		If i < nHighScoreSlots Then
			SortedHighscores(i) = HighScore(i)
			SortedHighscoreNames(i) = HighScoreName(i)
		Else
			temp = aScores(i - nHighScoreSlots)
			SortedHighscores(i) = temp
			SortedHighscoreNames(i) = "NEW"
		End If
	Next

	' sort the scores and adjust the aIndices array after how the scores
	' were sorted
	For i = 0 to UBound(SortedHighscores)
		highestScore = SortedHighscores(i)
		highestIndex = i
		For j = i + 1 to UBound(SortedHighscores)
			If SortedHighscores(j) > highestScore Then
				highestScore = SortedHighscores(j)
				highestIndex = j
			End If
		Next
		temp = SortedHighscoreNames(i)
		SortedHighscoreNames(i) = SortedHighscoreNames(highestIndex)
		SortedHighscoreNames(highestIndex) = temp
		temp = SortedHighscores(i)
		SortedHighscores(i) = SortedHighscores(highestIndex)
		SortedHighscores(highestIndex) = temp
		temp = aIndices(i)
		aIndices(i) = aIndices(highestIndex)
		aIndices(highestIndex) = temp
	Next

	' return an array of the positions of the new scores in the sorted scores
	For i = nHighScoreSlots to UBound(aIndices)
		For j = 0 to UBound(SortedHighscores)
			If aIndices(j) = i Then scoreIndices(i - nHighScoreSlots) = j
		Next
	Next

	aScoreRanks = scoreIndices

	For i = 0 to 3
		nHighScoreIter = i
		If aScoreRanks(i) < nHighScoreSlots Then
			nTextDuration = 0
			bWillEnterHighScore = True
			InitHighscore
			Exit For
		End If
	Next

	If bWillEnterHighScore = False Then
'		nTextDuration = 0
		LightSeq1.UpdateInterval = 25
		LightSeq1.Play SeqCircleOutOn, 40
		'InitMusic 2
	End If
End Sub

'*****GI Lights On
dim gilvl:gilvl = 1		'General Illumination light state tracked for Dynamic Ball Shadows
dim xx

For each xx in GI:xx.State = 1: Next

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	DOFGadget 104,DOFPulse,DOFcontactors
	RandomSoundSlingshotRight
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	AnySwitchHit eSwitchRightSlingshot
	anScore(nPlayer) = anScore(nPlayer) + (110 * nPlayfieldX)
	PlaySound "SFX_laser_gun",0,fSFXVolume,0,0.5,1,0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	DOFGadget 103,DOFPulse,DOFcontactors
	RandomSoundSlingshotLeft
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	AnySwitchHit eSwitchLeftSlingshot
	anScore(nPlayer) = anScore(nPlayer) + (110 * nPlayfieldX)
	PlaySound "SFX_laser_gun",0,fSFXVolume,0,0.5,1,0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAtVol soundname, ActiveBall, 1
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function VolRolling(ball) ' Calculates the Volume of ball rolling based on the ball speed
    Dim tmp
    tmp = Csng(BallVel(ball) ^2 / 1000)
	If tmp > 1 Then tmp = 1
	tmp = tmp * fMechSoundVolume
	VolRolling = tmp
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

'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds) - modified by Lumigado
'********************************************************************

Const tnob = 6 ' total number of balls
Const lob = 0
ReDim rolling(tnob)
Dim DropCount
ReDim DropCount(tnob)

InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
	Cor.Update 				'update ball tracking
    Dim b, ballpitch, ballvol, nSurface, fVolume
'	sTimersRun = sTimersRun + " RollingTimer"

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(gBOT)
		nSurface = anBallSurface(b)
		if nSurface <> eSurfacePlayfield and nSurface <> eSurfacePlasticRamp _
		and nSurface <> eSurfaceWireRamp and nSurface <> eSurfaceMetalRamp then
			anBallSurface(b) = eSurfacePlayfield
		End If

        If BallVel(gBOT(b) ) > 1 Then
            rolling(b) = true
			Select Case anBallSurface(b)
				Case eSurfacePlayfield
					fVolume = VolPlayfieldRoll(gBOT(b)) * BallRollVolume * fMechSoundVolume
					PlaySound ("BallRoll_" & b), -1, fVolume, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))
					If anBallSurface(b) <> anBallLastSurface(b) Then
						anBallLastSurface(b) = anBallSurface(b)
						StopSound("fx_plasticrolling" & b)
						StopSound("WireRamp" & b)
						StopSound("fx_metalrolling" & b)
					End If
				Case eSurfacePlasticRamp
					fVolume = max(VolRamp(gBOT(b)), 1) * RampRollVolume * fMechSoundVolume
					PlaySound("fx_plasticrolling" & b), -1, fVolume, AudioPan(gBOT(b)), 0, Pitch(gBOT(b)), 1, 0, AudioFade(gBOT(b))
					If anBallSurface(b) <> anBallLastSurface(b) Then
						anBallLastSurface(b) = anBallSurface(b)
						StopSound("BallRoll_" & b)
						StopSound("WireRamp" & b)
						StopSound("fx_metalrolling" & b)
					End If
				Case eSurfaceWireRamp
					fVolume = max(VolRamp(gBOT(b)), 1) * RampRollVolume * fMechSoundVolume
					PlaySound("WireRamp" & b), -1, fVolume, AudioPan(gBOT(b)), 0, Pitch(gBOT(b)), 1, 0, AudioFade(gBOT(b))
					If anBallSurface(b) <> anBallLastSurface(b) Then
						anBallLastSurface(b) = anBallSurface(b)
						StopSound("BallRoll_" & b)
						StopSound("fx_plasticrolling" & b)
						StopSound("fx_metalrolling" & b)
					End If
				Case eSurfaceMetalRamp
					fVolume = max(VolRamp(gBOT(b)), 1) * RampRollVolume * fMechSoundVolume
					PlaySound("fx_metalrolling" & b), -1, fVolume, AudioPan(gBOT(b)), 0, Pitch(gBOT(b)), 1, 0, AudioFade(gBOT(b))
					If anBallSurface(b) <> anBallLastSurface(b) Then
						anBallLastSurface(b) = anBallSurface(b)
						StopSound("WireRamp" & b)
						StopSound("BallRoll_" & b)
						StopSound("fx_plasticrolling" & b)
					End If
			end select
        Else
            If rolling(b) = True Then
                StopSound("BallRoll_" & b)
				StopSound("fx_plasticrolling" & b)
				StopSound("WireRamp" & b)
				StopSound("fx_metalrolling" & b)
                rolling(b) = False
            End If
        End If
        ' rothbauerw's Dropping Sounds
							   
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

Function BallIndex(oBall)
	Dim nBall, nID, i
	nID = oBall.ID
	nBall = -1
	For i = 0 to tnob - 1
		If gBOT(i).ID = nID then nBall = i
	Next
	BallIndex = nBall
End Function

Sub RampPlasticEnds_Hit(idx)
	Dim nBall
	nBall = BallIndex(ActiveBall)
	If -1 = nBall then exit sub
	anBallSurface(nBall) = eSurfacePlasticRamp
	PlaySoundAtVol "ferris_hit" & Int(rnd*2), ActiveBall, 1 * fMechSoundVolume
end sub

Sub RampWireStart_Hit(idx)
	Dim nBall
	nBall = BallIndex(ActiveBall)
	If -1 = nBall then exit sub
	anBallSurface(nBall) = eSurfaceWireRamp
	PlaySoundAtVol "WireRamp_Hit", ActiveBall, 1 * fMechSoundVolume
end sub

Sub RampMetalStart_Hit(idx)
	Dim nBall
	nBall = BallIndex(ActiveBall)
	If -1 = nBall then exit sub
	anBallSurface(nBall) = eSurfaceMetalRamp
end sub

Sub RampEntrances_Hit(idx)
	Dim nBall
	nBall = BallIndex(ActiveBall)
	If -1 = nBall then exit sub
	anBallSurface(nBall) = eSurfacePlayfield
end sub

Sub RampDrops_Hit(idx)
	Dim nBall
	nBall = BallIndex(ActiveBall)
	If -1 = nBall then exit sub
	If anBallSurface(nBall) <> eSurfacePlayfield Then
		PlaySoundAtVol SoundFX("WireRamp_Stop",DOFContactors), ActiveBall, 1 * fMechSoundVolume
'		PlaySoundAtVol SoundFX("fx_ball_drop" & Int(rnd*6),DOFContactors), ActiveBall, 1 * fMechSoundVolume
		anBallSurface(nBall) = eSurfacePlayfield 
	End If
end sub

'Dim ballsDict
'Set ballsDict = CreateObject("Scripting.Dictionary")

Sub BallAddRoll(kicker)
	Dim id, nBall
	set id = kicker.LastCapturedBall
	nBall = BallIndex(id)
	If -1 <> nBall then anBallSurface(nBall) = eSurfacePlayfield
'	If ballsDict.Exists(id) Then
'		ballsDict.Item(id) = "Playfield"
'	Else
'		ballsDict.add id, "Playfield"
'	end if
end sub

Sub BallRemoveRoll(kicker)
	Dim id, nBall
	set id = kicker.LastCapturedBall
	nBall = BallIndex(id)
	If -1 <> nBall then
		anBallSurface(nBall) = eSurfacePlayfield
		StopSound("BallRoll_" & nBall)
		StopSound("fx_plasticrolling" & nBall)
		StopSound("WireRamp" & nBall)
		StopSound("fx_metalrolling" & nBall)
    End If
'	BallsDict.Remove kicker.LastCapturedBall.ID
end sub

Sub LeftRampEnd002_Hit()
	Dim nBall
	nBall = BallIndex(ActiveBall)
	If -1 = nBall then exit sub
	anBallSurface(nBall) = eSurfacePlayfield
	DOF 213, DOFPulse				  
end sub

' ******************************************
' ********* Primitive animations ***********
' ******************************************

' ***** Up post animations

dim PostTypesDict ' A list of types of posts
set PostTypesDict = CreateObject("Scripting.Dictionary")
dim PostsDict ' Which posts are what type
set PostsDict = CreateObject("Scripting.Dictionary")
dim PostDirsDict ' What direction each post is moving
set PostDirsDict = CreateObject("Scripting.Dictionary")

PostTypesDict.add "PlayfieldPostMaxY",0
PostTypesDict.add "PlayfieldPostMinY",-39
PostTypesDict.add "RampPostMaxY",0
PostTypesDict.add "RampPostMinY",-25

PostsDict.add LeftRampPost,"RampPost"
PostsDict.add RightRampPost,"RampPost"
PostsDict.add LoopPost,"PlayfieldPost"
PostsDict.add SkillPost,"PlayfieldPost"
PostsDict.add LockPost,"PlayfieldPost"

PostDirsDict.add LeftRampPost,8
PostDirsDict.add RightRampPost,8
PostDirsDict.add LoopPost,8
PostDirsDict.add SkillPost,8
PostDirsDict.add LockPost,8

Sub UpPostMove(post,prim,dir)
	select case dir
		case "down"
			if post.isDropped = false then PlaySound "PostDown", 0, 1 * fMechSoundVolume, AudioPan(prim), 0, 0, 1, 0, AudioFade(prim)
			post.isDropped = true
			PostDirsDict.item(post) = abs(PostDirsDict.item(post)) * -1
		case "up"
			if post.isDropped = true then PlaySound "PostUp", 0, 1 * fMechSoundVolume, AudioPan(prim), 0, 0, 1, 0, AudioFade(prim)
			post.isDropped = false
			PostDirsDict.item(post) = abs(PostDirsDict.item(post))
	end select
	post.timerenabled = true
end sub

sub PostPrimUpdate(post,prim,dir)
	prim.TransY = prim.TransY + dir
	if prim.TransY >= PostTypesDict.item(PostsDict.item(post) & "MaxY") then
		prim.TransY = PostTypesDict.item(PostsDict.item(post) & "MaxY")
		post.timerenabled = false
	elseif prim.TransY <= PostTypesDict.item(PostsDict.item(post) & "MinY") then
		prim.TransY = PostTypesDict.item(PostsDict.item(post) & "MinY")
		post.timerenabled = false
	end if
end sub

sub LeftRampPost_timer()
'	sTimersRun = sTimersRun + " LeftRampPost"
	PostPrimUpdate LeftRampPost,LeftRampPostPrimitive,PostDirsDict.item(LeftRampPost)
end sub

sub RightRampPost_timer()
'	sTimersRun = sTimersRun + " RightRampPost"
	PostPrimUpdate RightRampPost,RightRampPostPrimitive,PostDirsDict.item(RightRampPost)
end sub

sub LoopPost_timer()
'	sTimersRun = sTimersRun + " LoopPost"
	PostPrimUpdate LoopPost,LoopPostPrimitive,PostDirsDict.item(LoopPost)
end sub

sub SkillPost_timer()
'	sTimersRun = sTimersRun + " SkillPost"
	PostPrimUpdate SkillPost,SkillPostPrimitive,PostDirsDict.item(SkillPost)
end sub

sub LockPost_timer()
'	sTimersRun = sTimersRun + " LockPost"
	PostPrimUpdate LockPost,LockPostPrimitive,PostDirsDict.item(LockPost)
end sub

'**********************
' Ball Collision Sound
'**********************

'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperVisualUpdate()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	LeftUpperFlipperSH.RotZ = LeftUpperFlipper.currentangle
	ULFLogo.RotZ = LeftUpperFlipper.currentangle
	LFLogo.RotZ = LeftFlipper.currentangle
	RFLogo.RotZ = RightFlipper.currentangle	
End Sub

' The frame timer interval is -1, so executes at the display frame rate
dim FrameTime, InitFrameTime : InitFrameTime = 0
Sub FrameTimer_Timer()
'	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime
'	If FrameTime > 25 then debug.print "FrameTime = " & frameTime & ", Timers: " & sTimersRun
'	sTimersRun = ""
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
	AnimateSuitcase
End Sub

' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'****** Part C:  The Magic ******

' *** These define the appearance of shadows in your table	***

'Ambient (Room light source)
Const AmbientBSFactor 		= 0.9	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 0		'Offset y position under ball	 (for example 5,5 if the light is in the back left corner)
'Dynamic (Table light sources)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

' ***														***

' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!
dim objrtx1(6), objrtx2(6)
dim objBallShadow(6)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

'Initialization
DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob - 1								'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii/1000 + 0.01			'Separate z for layering without clipping
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii/1000 + 0.02
		objrtx2(iii).visible = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
'		If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
end sub

Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed dynamically if you have a reason
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
'	Dim gBOT: gBOT=getballs	'Uncomment if you're deleting balls - Don't do it! #SaveTheBalls

	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 to tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(gBOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If gBOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + offsetY
				objBallShadow(s).visible = 1

				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + BallSize/5
				BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				BallShadowA(s).visible = 1
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + offsetY
'				objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If gBOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + BallSize/5
				BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				BallShadowA(s).visible = 1
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				BallShadowA(s).Y = gBOT(s).Y + Ballsize/10 + offsetY
				BallShadowA(s).height=gBOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).X < 850 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff:
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then
'					If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y
					'objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + offsetY
					'objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************

'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.45*defvalue
            Case 6: zMultiplier = 0.5*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
        'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

' Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer activeball, 1
End Sub



'******************************************************
'****  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these 
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Flippers: 	https://www.youtube.com/watch?v=FWvM9_CdVHw
' Dampeners: 	https://www.youtube.com/watch?v=tqsxx48C6Pg
' Physics: 		https://www.youtube.com/watch?v=UcRMG-2svvE
'
'
' Note: BallMass must be set to 1. BallSize should be set to 50 (in other words the ball radius is 25) 
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 9.5-10.5 |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
' 
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 4-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |



'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. an object or point to tell the script where the tip of the flipper is at rest (EndPointLp, EndPointRp)
'	4. and, special scripting
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
' | EOS Torque         | 0.3            | 0.3                   | 0.275                  | 0.275              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'*******************************************
' Flippers
'*******************************************

Const ReflipAngle = 20
Const QuickFlipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled)
	If Enabled Then
		FlipperActivate LeftFlipper, LFPress
		LF.Fire  'leftflipper.rotatetoend

		If LeftFlipper.currentangle < LeftFlipper.endangle + ReflipAngle Then 
			'Play partial flip sound and stop any flip down sound
			StopAnyFlipperLowerLeftDown()
			RandomSoundFlipperLowerLeftReflip LeftFlipper
		Else
			'Play full flip sound
			If BallNearLF = 0 Then 
				RandomSoundFlipperLowerLeftUpFullStroke LeftFlipper
			End If
			If BallNearLF = 1 Then 
				Select Case Int(Rnd*2)+1
					Case 1 : RandomSoundFlipperLowerLeftUpDampenedStroke LeftFlipper
					Case 2 : RandomSoundFlipperLowerLeftUpFullStroke LeftFlipper
				End Select		
			End If
		End If		
	Else
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart
		If bFlippersEnabled Then
			If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
				'Play flip down sound
				RandomSoundFlipperLowerLeftDown LeftFlipper
			End If
			If LeftFlipper.currentangle < LeftFlipper.startAngle + QuickFlipAngle _
			and LeftFlipper.currentangle <> LeftFlipper.endangle Then
				'Play quick flip sound and stop any flip up sound
				StopAnyFlipperLowerLeftUp()
				RandomSoundLowerLeftQuickFlipUp()
			Else
				FlipperLeftLowerHitParm = FlipperUpSoundLevel
			End If
		End If
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		FlipperActivate RightFlipper, RFPress
		RF.Fire 'rightflipper.rotatetoend

		If RightFlipper.currentangle > RightFlipper.endangle - ReflipAngle Then
			'Play partial flip sound and stop any flip down sound
			StopAnyFlipperLowerRightDown()
			RandomSoundFlipperLowerRightReflip RightFlipper
		Else 
			'Play full flip sound
			If BallNearRF = 0 Then 
				RandomSoundFlipperLowerRightUpFullStroke RightFlipper
			End If
			
			If BallNearRF = 1 Then 
				Select Case Int(Rnd*2)+1
					Case 1 : RandomSoundFlipperLowerRightUpDampenedStroke RightFlipper
					Case 2 : RandomSoundFlipperLowerRightUpFullStroke RightFlipper
				End Select		
			End If
		End If
	Else
		FlipperDeActivate RightFlipper, RFPress
		RightFlipper.RotateToStart
		If bFlippersEnabled Then
			If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
				'Play flip down sound
				RandomSoundFlipperLowerRightDown RightFlipper
			End If
			If RightFlipper.currentangle < RightFlipper.startAngle + QuickFlipAngle _
			and RightFlipper.currentangle <> RightFlipper.endangle Then
				'Play quick flip sound and stop any flip up sound
				StopAnyFlipperLowerRightUp()
				RandomSoundLowerRightQuickFlipUp()
			Else
				FlipperRightLowerHitParm = FlipperUpSoundLevel
			End If
		End If
	End If
End Sub

Sub SolULFlipper(Enabled)
	If Enabled Then
		LeftUpperFlipper.rotatetoend
		If LeftUpperFlipper.currentangle > LeftUpperFlipper.endangle - ReflipAngle Then
			'Play partial flip sound and stop any flip down sound
			StopAnyFlipperUpperLeftDown()
			RandomSoundFlipperUpperLeftReflip LeftUpperFlipper
		Else 
			RandomSoundFlipperUpperLeftUpFullStroke LeftUpperFlipper
		End If
	Else
		LeftUpperFlipper.rotatetostart
		If bFlippersEnabled Then
			If LeftUpperFlipper.currentangle > LeftUpperFlipper.startAngle - 5 Then
				'Play flip down sound
				RandomSoundFlipperLowerLeftDown LeftUpperFlipper
			End If
			If LeftUpperFlipper.currentangle < LeftUpperFlipper.startAngle + QuickFlipAngle _
			and LeftUpperFlipper.currentangle <> LeftUpperFlipper.endangle Then
				'Play quick flip sound and stop any LeftUpperFlipper up sound
				StopAnyFlipperUpperLeftUp()
				RandomSoundUpperLeftQuickFlipUp()
			Else
				FlipperLeftUpperHitParm = FlipperUpSoundLevel
			End If
		End If
	End If
End Sub
'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 2.7
'		x.AddPt "Polarity", 2, 0.33, - 2.7
'		x.AddPt "Polarity", 3, 0.37, - 2.7
'		x.AddPt "Polarity", 4, 0.41, - 2.7
'		x.AddPt "Polarity", 5, 0.45, - 2.7
'		x.AddPt "Polarity", 6, 0.576, - 2.7
'		x.AddPt "Polarity", 7, 0.66, - 1.8
'		x.AddPt "Polarity", 8, 0.743, - 0.5
'		x.AddPt "Polarity", 9, 0.81, - 0.5
'		x.AddPt "Polarity", 10, 0.88, 0
'
'		x.AddPt "Velocity", 0, 0, 1
'		x.AddPt "Velocity", 1, 0.16, 1.06
'		x.AddPt "Velocity", 2, 0.41, 1.05
'		x.AddPt "Velocity", 3, 0.53, 1 '0.982
'		x.AddPt "Velocity", 4, 0.702, 0.968
'		x.AddPt "Velocity", 5, 0.95,  0.968
'		x.AddPt "Velocity", 6, 1.03, 0.945
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
'
'
''*******************************************
'' Mid 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 3.7
'		x.AddPt "Polarity", 2, 0.33, - 3.7
'		x.AddPt "Polarity", 3, 0.37, - 3.7
'		x.AddPt "Polarity", 4, 0.41, - 3.7
'		x.AddPt "Polarity", 5, 0.45, - 3.7
'		x.AddPt "Polarity", 6, 0.576,- 3.7
'		x.AddPt "Polarity", 7, 0.66, - 2.3
'		x.AddPt "Polarity", 8, 0.743, - 1.5
'		x.AddPt "Polarity", 9, 0.81, - 1
'		x.AddPt "Polarity", 10, 0.88, 0
'
'		x.AddPt "Velocity", 0, 0, 1
'		x.AddPt "Velocity", 1, 0.16, 1.06
'		x.AddPt "Velocity", 2, 0.41, 1.05
'		x.AddPt "Velocity", 3, 0.53, 1 '0.982
'		x.AddPt "Velocity", 4, 0.702, 0.968
'		x.AddPt "Velocity", 5, 0.95,  0.968
'		x.AddPt "Velocity", 6, 1.03, 0.945
'
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
''*******************************************
''  Late 80's early 90's
'
Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, - 5
		x.AddPt "Polarity", 2, 0.4, - 5
		x.AddPt "Polarity", 3, 0.6, - 4.5
		x.AddPt "Polarity", 4, 0.65, - 4.0
		x.AddPt "Polarity", 5, 0.7, - 3.5
		x.AddPt "Polarity", 6, 0.75, - 3.0
		x.AddPt "Polarity", 7, 0.8, - 2.5
		x.AddPt "Polarity", 8, 0.85, - 2.0
		x.AddPt "Polarity", 9, 0.9, - 1.5
		x.AddPt "Polarity", 10, 0.95, - 1.0
		x.AddPt "Polarity", 11, 1, - 0.5
		x.AddPt "Polarity", 12, 1.1, 0
		x.AddPt "Polarity", 13, 1.3, 0

		x.AddPt "Velocity", 0, 0, 1
		x.AddPt "Velocity", 1, 0.16, 1.06
		x.AddPt "Velocity", 2, 0.41, 1.05
		x.AddPt "Velocity", 3, 0.53, 1 '0.982
		x.AddPt "Velocity", 4, 0.702, 0.968
		x.AddPt "Velocity", 5, 0.95,  0.968
		x.AddPt "Velocity", 6, 1.03,  0.945
	Next

	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
End Sub

'*******************************************
' Early 90's and after

'Sub InitPolarity()
'	Dim x, a
'	a = Array(LF, RF)
'	For Each x In a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'		x.DebugOn=False ' prints some info in debugger
'		
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, -5.5
'		x.AddPt "Polarity", 2, 0.4, -5.5
'		x.AddPt "Polarity", 3, 0.6, -5.0
'		x.AddPt "Polarity", 4, 0.65, -4.5
'		x.AddPt "Polarity", 5, 0.7, -4.0
'		x.AddPt "Polarity", 6, 0.75, -3.5
'		x.AddPt "Polarity", 7, 0.8, -3.0
'		x.AddPt "Polarity", 8, 0.85, -2.5
'		x.AddPt "Polarity", 9, 0.9,-2.0
'		x.AddPt "Polarity", 10, 0.95, -1.5
'		x.AddPt "Polarity", 11, 1, -1.0
'		x.AddPt "Polarity", 12, 1.05, -0.5
'		x.AddPt "Polarity", 13, 1.1, 0
'		x.AddPt "Polarity", 14, 1.3, 0
'		
'		x.AddPt "Velocity", 0, 0,	   1
'		x.AddPt "Velocity", 1, 0.160, 1.06
'		x.AddPt "Velocity", 2, 0.410, 1.05
'		x.AddPt "Velocity", 3, 0.530, 1'0.982
'		x.AddPt "Velocity", 4, 0.702, 0.968
'		x.AddPt "Velocity", 5, 0.95,  0.968
'		x.AddPt "Velocity", 6, 1.03,  0.945
'	Next
'	
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'	LF.SetObjects "LF", LeftFlipper, TriggerLF
'	RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub




'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef
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
			If Not IsEmpty(balls(x) ) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x) ) Then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef
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
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class





'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection

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
	AddSlingsPt 0, 0.00,	-4
	AddSlingsPt 1, 0.45,	-7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
		x.addpoint idx, aX, aY
	Next
End Sub

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
'Function RotPoint(x,y,angle)
'    dim rx, ry
'    rx = x*dCos(angle) - y*dSin(angle)
'    ry = x*dSin(angle) + y*dCos(angle)
'    RotPoint = Array(rx,ry)
'End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2

	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): Enabled = True : End Sub 

	Public Property let Object(aInput) : Set Slingshot = aInput : End Property
	Public Property Let EndPoint1(aInput) : SlingX1 = aInput.x: SlingY1 = aInput.y: End Property
	Public Property Let EndPoint2(aInput) : SlingX2 = aInput.x: SlingY2 = aInput.y: End Property

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 then Report
	End Sub

	Public Sub Report()         'debug, reports all coords in tbPL.text
		If not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub


	Public Sub VelocityCorrect(aBall)
		dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then 
			XL = SlingX1 : YL = SlingY1 : XR = SlingX2 : YR = SlingY2
		Else
			XL = SlingX2 : YL = SlingY2 : XR = SlingX1 : YR = SlingY1
		End If

		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then 
			If ABS(XR-XL) > ABS(YR-YL) Then 
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If

		'Velocity angle correction
		If not IsEmpty(ModIn(0) ) then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
			'debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'debug.print " " 
		End If
	End Sub

End Class



'******************************************************
'  FLIPPER POLARITY. RUBBER DAMPENER, AND SLINGSHOT CORRECTION SUPPORTING FUNCTIONS 
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
	'   Dim BOT
	'   BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper2) Then
					gBOT(b).velx = gBOT(b).velx / 1.3
					gBOT(b).vely = gBOT(b).vely - 0.5
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

'*****************
' Maths
'*****************
Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
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

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

Function RndInt(min, max)
    RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
    RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

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
'	Const EOSReturn = 0.025  'mid 90's and later

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
		Dim b', BOT
		'		BOT = GetBalls
		
		For b = 0 To UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If gBOT(b).vely >= - 0.4 Then gBOT(b).vely =  - 0.4
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

Const LiveDistanceMin = 30  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle)	'-1 for Right Flipper
	Dim LiveCatchBounce																														'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime
	CatchTime = GameTime - FCount
	
	If CatchTime <= LiveCatch And parm > 6 And Abs(Flipper.x - ball.x) > LiveDistanceMin And Abs(Flipper.x - ball.x) < LiveDistanceMax Then
		If CatchTime <= LiveCatch * 0.5 Then												'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		Else
			LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)		'Partial catch when catch happens a bit late
		End If
		
		If LiveCatchBounce = 0 And ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx = 0
		ball.angmomy = 0
		ball.angmomz = 0
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

Sub rubberpost11_Hit
	RubbersD.dampen Activeball
End Sub

Sub rubberpost5_Hit
	RubbersD.dampen Activeball
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
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
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



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************




' ****** Ramp entrance sound effects

Dim sLeftRampEntranceDirection


Sub LeftRampPlasticEntrance001_Hit()
	DOF 220, DOFPulse
end Sub							

Sub LeftRampEntrance_Hit()
	if sLeftRampEntranceDirection = "down" then
		PlaySound "SFX_ramp_entrance_down",0,fSFXVolume,0,0,0,1,0
		sLeftRampEntranceDirection = "up"
	else
		PlaySound "SFX_ramp_entrance_up",0,fSFXVolume,0,0,0,1,0
		sLeftRampEntranceDirection = "down"
		LeftRampEntrance.timerenabled = true
		DOF 212, DOFPulse				   
	end if
end sub

Sub LeftRampEntrance_timer()
	LeftRampEntrance.timerenabled = false
	sLeftRampEntranceDirection = "up"
end sub

Function SpinsForCyberLock
	Select Case aanMballsPlayed(nPlayer, eMballCyber)
		Case 0
			SpinsForCyberLock = 60
		Case 1
			SpinsForCyberLock = 90
		Case 2
			SpinsForCyberLock = 120
		Case Else
			SpinsForCyberLock = 150
	End Select
End Function

Sub Spinner_Spin
	Dim i
	SoundSpinner Spinner
	If nPlayersInGame > 0 and False = bDrainingBalls Then
		If False = avModesRunning(nPlayer).Contains(eModeCyberMBall) _
		And false = IsWizardModeActive Then
			CyberAdvanceLock
		End If
		AnySwitchHit eSwitchSpinner
		anBonusElements(eMballCyber) = anBonusElements(eMballCyber) + 40
		anScore(nPlayer) = anScore(nPlayer) + (50 * nPlayfieldX)
		Spinner.timerenabled = false
		Spinner.timerenabled = true
		nSpinnerSoundVol = 0.5
		PlaySound "SFX_spinner",-1,nSpinnerSoundVol * fSFXVolume * 0.75,0,0,0,1,0
		DOF 211, DOFPulse				   
	End If
End Sub

Sub Spinner_timer()
'	sTimersRun = sTimersRun + " Spinner"
	if nSpinnerSoundVol > 0 then
		nSpinnerSoundVol = nSpinnerSoundVol - 0.025
		PlaySound "SFX_spinner",-1,nSpinnerSoundVol * fSFXVolume * 0.75,0,0,0,1,0
	else
		' StopSound "SFX_spinner"
		nSpinnerSoundVol = 0
	end if
end sub



Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function

'////////////////////////////////////////////////////////////////////////////////
'////          Mechanical Sounds, by Fleep                                   ////
'////                     Last Updated: January, 2022                        ////
'////////////////////////////////////////////////////////////////////////////////
'
'/////////////////////////////////  CARTRIDGES  /////////////////////////////////
'
'//  General Mechanical Sounds Cartridges:
Const Cartridge_Bumpers					= "WS_PBT_REV01" 'Williams Pinbot Cartridge REV01
Const Cartridge_Slingshots				= "WS_PBT_REV01" 'Williams Pinbot Cartridge REV01
Const Cartridge_Flippers				= "WS_PBT_REV01" 'Williams Pinbot Cartridge REV01
Const Cartridge_Kickers					= "WS_WHD_REV01"
Const Cartridge_Diverters				= "WS_DNR_REV01" 'Williams Diner Cartridge REV01
Const Cartridge_Knocker					= "WS_WHD_REV02" 'Williams Whirlwind Cartridge REV02
Const Cartridge_Relays					= "WS_WHD_REV01"
Const Cartridge_Trough					= "WS_WHD_REV01"
Const Cartridge_Rollovers				= "WS_WHD_REV01"
Const Cartridge_Targets					= "WS_WHD_REV01"
Const Cartridge_Gates					= "WS_WHD_REV01"
Const Cartridge_Spinner					= "SY_TNA_REV01" 'Spooky Total Nuclear Annihilation Cartridge REV01
Const Cartridge_Rubber_Hits				= "WS_WHD_REV01"
Const Cartridge_Metal_Hits				= "WS_WHD_REV01"
Const Cartridge_Plastic_Hits			= "WS_WHD_REV01"
Const Cartridge_Wood_Hits				= "WS_WHD_REV01"
Const Cartridge_Cabinet_Sounds			= "WS_WHD_REV01"
Const Cartridge_Drain					= "WS_WHD_REV01"
Const Cartridge_Apron					= "WS_WHD_REV01"
Const Cartridge_Ball_Roll				= "BY_TOM_REV01" 'Bally Theatre of Magic Cartridge REV01
Const Cartridge_BallBallCollision		= "BY_WDT_REV01" 'Bally WHO Dunnit Cartridge REV01
Const Cartridge_Ball_Drop_Bump			= "WS_WHD_REV01"
Const Cartridge_Plastic_Ramps			= "WS_WHD_REV01"
Const Cartridge_Metal_Ramps				= "WS_WHD_REV01"
Const Cartridge_Ball_Guides				= "WS_WHD_REV01"
Const Cartridge_Table_Specifics			= "WS_WHD_REV01"

'////////////////////////////  SOUND SOURCE CREDITS  ////////////////////////////
'//  Special thanks go to the following contributors who have provided audio 
'//  footage recordings:
'//  
'//  Williams Whirlwind - Blackmoor, wrd1972
'//  Williams Diner - Nick Rusis
'//  Spooky Total Nuclear Annihilation - WildDogArcade, Ed and Gary
'//  Bally Theatre of Magic - CalleV, nickbuol
'//  Bally WHO Dunnit - Amazaley1
'//  Williams Pinbot - major_drain_pinball

'///////////////////////////  SOLENOIDS (COILS) CONFIG  /////////////////////////

'//  FLIPPER COILS:
'//  Flippers in this table: Lower Left Flipper, Lower Right Flipper, Upper Right Fliiper
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel
Dim FlipperLeftLowerHitParm, FlipperLeftUpperHitParm, FlipperRightLowerHitParm

'//  Flipper Up Attacks initialize during playsound subs
Dim FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel

FlipperUpSoundLevel = 1
FlipperDownSoundLevel = 0.65
FlipperUpAttackMinimumSoundLevel = 0.010
FlipperUpAttackMaximumSoundLevel = 0.435

'//  Flipper Hit Param initialize with FlipperUpSoundLevel
'//  and dynamically modified calculated by ball flipper collision
FlipperLeftLowerHitParm = FlipperUpSoundLevel
FlipperRightLowerHitParm = FlipperUpSoundLevel
FlipperLeftUpperHitParm = FlipperUpSoundLevel

Dim Solenoid_OutholeKicker_SoundLevel, Solenoid_ShooterFeeder_SoundLevel
Dim Solenoid_RightRampLifter_SoundLevel, Solenoid_LeftLockingKickback_SoundLevel
Dim Solenoid_TopEject_SoundLevel, Solenoid_Knocker_SoundLevel, Solenoid_DropTargetReset_SoundLevel
Dim Solenoid_Diverter_Enabled_SoundLevel, Solenoid_Diverter_Hold_SoundLevel, Solenoid_Diverter_Disabled_SoundLevel
Dim Solenoid_UnderPlayfieldKickbig_SoundLevel, Solenoid_Bumper_SoundMultiplier
Dim Solenoid_Slingshot_SoundLevel, Solenoid_RightRampDown_SoundLevel, AutoPlungerSoundLevel

AutoPlungerSoundLevel = 1												'volume level; range [0, 1]
Solenoid_OutholeKicker_SoundLevel = 1
Solenoid_ShooterFeeder_SoundLevel = 1
Solenoid_RightRampLifter_SoundLevel = 0.3
Solenoid_RightRampDown_SoundLevel = 0.3
Solenoid_LeftLockingKickback_SoundLevel = 1
Solenoid_TopEject_SoundLevel = 1
Solenoid_Knocker_SoundLevel = 1
Solenoid_DropTargetReset_SoundLevel = 1
Solenoid_Diverter_Enabled_SoundLevel = 1
Solenoid_Diverter_Hold_SoundLevel = 0.7
Solenoid_Diverter_Disabled_SoundLevel = 0.4
Solenoid_UnderPlayfieldKickbig_SoundLevel = 1
Solenoid_Bumper_SoundMultiplier = 0.004 '8
Solenoid_Slingshot_SoundLevel = 1

Dim RelayLowerGISoundLevel, RelayUpperGISoundLevel, RelaySolenoidACSelectSoundLevel, RelayFlasherSoundLevel
RelayLowerGISoundLevel = 0.45
RelayUpperGISoundLevel = 0.45
RelaySolenoidACSelectSoundLevel = 0.3
RelayFlasherSoundLevel = 0.015

Dim Solenoid_BlowerMotor_SoundLevel, Solenoid_SpinWheelsMotor_SoundLevel
Solenoid_BlowerMotor_SoundLevel = 0.2
Solenoid_SpinWheelsMotor_SoundLevel = 0.2

'////////////////////////////  SWITCHES SOUND CONFIG  ///////////////////////////
Dim Switch_Gate_SoundLevel, SpinnerSoundLevel, RolloverSoundLevel, OutLaneRolloverSoundLevel, TargetSoundFactor

Switch_Gate_SoundLevel = 1	
SpinnerSoundLevel = 0.1
RolloverSoundLevel = 0.55
OutLaneRolloverSoundLevel = 0.8
TargetSoundFactor = 0.8

'////////////////////  BALL HITS, BUMPS, DROPS SOUND CONFIG  ////////////////////
Dim BallWithBallCollisionSoundFactor, BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor
Dim WallImpactSoundFactor, MetalImpactSoundFactor, WireformAntiRebountRailSoundFactor
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor
Dim BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor, OutlaneWallsSoundFactor
Dim EjectBallBumpSoundLevel, HeadSaucerSoundLevel, EjectHoleEnterSoundLevel
Dim RightRampMetalWireDropToPlayfieldSoundLevel, LeftPlasticRampDropToLockSoundLevel, LeftPlasticRampDropToPlayfieldSoundLevel
Dim CellarLeftEnterSoundLevel, CellarRightEnterSoundLevel, CellerKickouBallDroptSoundLevel

BallWithBallCollisionSoundFactor = 3.2
BallBouncePlayfieldSoftFactor = 0.0015
BallBouncePlayfieldHardFactor = 0.0075
WallImpactSoundFactor = 0.075
MetalImpactSoundFactor = 0.075
RubberStrongSoundFactor = 0.045
RubberWeakSoundFactor = 0.055
RubberFlipperSoundFactor = 0.65
BottomArchBallGuideSoundFactor = 0.2
FlipperBallGuideSoundFactor = 0.015
WireformAntiRebountRailSoundFactor = 0.04
OutlaneWallsSoundFactor = 1
EjectBallBumpSoundLevel = 1
RightRampMetalWireDropToPlayfieldSoundLevel = 1
LeftPlasticRampDropToLockSoundLevel = 1
LeftPlasticRampDropToPlayfieldSoundLevel = 1
EjectHoleEnterSoundLevel = 0.75
HeadSaucerSoundLevel = 0.15
CellerKickouBallDroptSoundLevel = 1
CellarLeftEnterSoundLevel = 0.85
CellarRightEnterSoundLevel = 0.85

'///////////////////////  OTHER PLAYFIELD ELEMENTS CONFIG  //////////////////////
Dim RollingSoundFactor, RollingOnDiscSoundFactor, BallReleaseShooterLaneSoundLevel
Dim LeftPlasticRampEnteranceSoundLevel, RightPlasticRampEnteranceSoundLevel
Dim LeftPlasticRampRollSoundFactor, RightPlasticRampRollSoundFactor
Dim LeftMetalWireRampRollSoundFactor, RightPlasticRampHitsSoundLevel, LeftPlasticRampHitsSoundLevel
Dim SpinningDiscRolloverSoundFactor, SpinningDiscRolloverBumpSoundLevel
Dim LaneSoundFactor, LaneEnterSoundFactor, InnerLaneSoundFactor
Dim LaneLoudImpactMinimumSoundLevel, LaneLoudImpactMaximumSoundLevel
Dim GateSoundLevel

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
RollingSoundFactor = 50
RollingOnDiscSoundFactor = 1.5
BallReleaseShooterLaneSoundLevel = 1
LeftPlasticRampEnteranceSoundLevel = 0.1
RightPlasticRampEnteranceSoundLevel = 0.1
LeftPlasticRampRollSoundFactor = 0.2
RightPlasticRampRollSoundFactor = 0.2
LeftMetalWireRampRollSoundFactor = 1
RightPlasticRampHitsSoundLevel = 1
LeftPlasticRampHitsSoundLevel = 1
SpinningDiscRolloverSoundFactor = 0.05
SpinningDiscRolloverBumpSoundLevel = 0.3
LaneEnterSoundFactor = 0.9
InnerLaneSoundFactor = 0.0005
LaneSoundFactor = 0.0004
LaneLoudImpactMinimumSoundLevel = 0
LaneLoudImpactMaximumSoundLevel = 0.4


'///////////////////////////  CABINET SOUND PARAMETERS  /////////////////////////
Dim NudgeLeftSoundLevel, NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel
Dim PlungerReleaseSoundLevel, PlungerPullSoundLevel, CoinSoundLevel

NudgeLeftSoundLevel = 1
NudgeRightSoundLevel = 1
NudgeCenterSoundLevel = 1
StartButtonSoundLevel = 0.1
PlungerReleaseSoundLevel = 1
PlungerPullSoundLevel = 1
CoinSoundLevel = 1


'///////////////////////////  MISC SOUND PARAMETERS  ////////////////////////////
Dim LutToggleSoundLevel : 
LutToggleSoundLevel = 0.5


'////////////////////////////////  SOUND HELPERS  ///////////////////////////////
Dim SoundOn : SoundOn = 1
Dim SoundOff : SoundOff = 0
Dim Up : Up = 0
Dim Down : Down = 1
Dim RampUp : RampUp = 1
Dim RampDown : RampDown = 0
Dim RampDownSlow : RampDownSlow = 1
Dim RampDownFast : RampDownFast = 2
Dim CircuitA : CircuitA = 0
Dim CircuitC : CircuitC = 1

'//  Helper for Main (Lower) flippers dampened stroke
Dim BallNearLF : BallNearLF = 0
Dim BallNearRF : BallNearRF = 0

Sub TriggerBallNearLF_Hit()
	'Debug.Print "BallNearLF = 1"
	BallNearLF = 1
End Sub

Sub TriggerBallNearLF_UnHit()
	'Debug.Print "BallNearLF = 0"
	BallNearLF = 0
End Sub

Sub TriggerBallNearRF_Hit()
	'Debug.Print "BallNearRF = 1"
	BallNearRF = 1
End Sub

Sub TriggerBallNearRF_UnHit()
	'Debug.Print "BallNearLF = 0"
	BallNearRF = 0
End Sub


'///////////////////////  SOUND PLAYBACK SUBS / FUNCTIONS  //////////////////////
'//////////////////////  POSITIONAL SOUND PLAYBACK METHODS  /////////////////////

Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, min(1,aVol) * fMechSoundVolume, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, 0, min(1,aVol) * fMechSoundVolume, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, -1, min(1,aVol) * fMechSoundVolume, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStaticLoop(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, -1, min(1,aVol) * fMechSoundVolume, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
    PlaySound playsoundparams, 0, min(1,aVol) * fMechSoundVolume, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(1,aVol) * fMechSoundVolume, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(1,aVol) * fMechSoundVolume, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(1,aVol) * fMechSoundVolume, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(1,aVol) * fMechSoundVolume, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
    PlaySound playsoundparams, -1, min(1,aVol) * fMechSoundVolume, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub


'//////////////////////  SUPPORTING BALL & SOUND FUNCTIONS  /////////////////////

Function AudioFade(tableobj)
	Dim tmp
	Select Case PositionalSoundPlaybackConfiguration
		Case 1
			AudioFade = 0
		Case 2
			AudioFade = 0
		Case 3
			tmp = tableobj.y * 2 / tableheight-1
			If tmp > 0 Then
				AudioFade = Csng(tmp ^10)
			Else
				AudioFade = Csng(-((- tmp) ^10) )
			End If
	End Select
End Function

'//  Calculates the pan for a tableobj based on the X position on the table.
Function AudioPan(tableobj)
	Dim tmp
	Select Case PositionalSoundPlaybackConfiguration
		Case 1
			AudioPan = 0
		Case 2
			tmp = tableobj.x * 2 / tablewidth-1
			If tmp > 0 Then
				AudioPan = Csng(tmp ^10)
			Else
				AudioPan = Csng(-((- tmp) ^10) )
			End If
		Case 3
			tmp = tableobj.x * 2 / tablewidth-1

			If tmp > 0 Then
				AudioPan = Csng(tmp ^10)
			Else
				AudioPan = Csng(-((- tmp) ^10) )
			End If
	End Select
End Function

'//  Calculates the volume of the sound based on the ball speed
Function Vol(ball)
	Vol = Csng(BallVel(ball) ^2)
End Function

Function VolRamp(ball)
    Dim tmp
    tmp = Csng(BallVel(ball) ^2 / 400)
	If tmp > 1 Then tmp = 1
	tmp = tmp * fMechSoundVolume
	VolRamp = tmp
End Function

'//  Calculates the pitch of the sound based on the ball speed
Function Pitch(ball)
    Pitch = BallVel(ball) * 20
End Function

'//  Calculates the ball speed
Function BallVel(ball)
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'//  Calculates the roll volume of the sound based on the ball speed
Dim TempBallVel
Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	TempBallVel = Csng((INT(SQR((ball.VelX^2)+(ball.VelY^2))))/RollingSoundFactor)
	If TempBallVel = 1 Then TempBallVel = 0.999
	If TempBallVel = 0 Then TempBallVel = 0.001
	'debug.print TempBallVel
	TempBallVel = Csng(1/(1+(0.275*(((0.75*TempBallVel)/(1-TempBallVel))^(-2)))))
	VolPlayfieldRoll = TempBallVel
End Function

'//  Calculates the roll volume of the sound based on the ball speed
Function VolSpinningDiscRoll(ball)
	VolSpinningDiscRoll = RollingOnDiscSoundFactor * 0.1 * Csng(BallVel(ball) ^3)
End Function

'//  Calculates the roll volume of the sound based on the ball speed
Dim TempBallVelPlastic
Function VolPlasticMetalRampRoll(ball)
	'VolPlasticMetalRampRoll = RollingOnDiscSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
	TempBallVelPlastic = Csng((INT(SQR((ball.VelX^2)+(ball.VelY^2))))/RollingSoundFactor)
	If TempBallVelPlastic = 1 Then TempBallVelPlastic = 0.999
	If TempBallVelPlastic = 0 Then TempBallVelPlastic = 0.001
	'debug.print TempBallVel
	TempBallVelPlastic = Csng(1/(1+(0.275*(((0.75*TempBallVelPlastic)/(1-TempBallVelPlastic))^(-2)))))
	VolPlasticMetalRampRoll = TempBallVelPlastic
End Function

'//  Calculates the roll pitch of the sound based on the ball speed
Dim TempPitchBallVel
Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	'PitchPlayfieldRoll = BallVel(ball) ^2 * 15
	'PitchPlayfieldRoll = Csng(BallVel(ball))/50 * 10000
	'PitchPlayfieldRoll = (1-((Csng(BallVel(ball))/50)^0.2)) * 20000

	'PitchPlayfieldRoll = (2*((Csng(BallVel(ball)))^0.7))/(2+(Csng(BallVel(ball)))) * 16000
	TempPitchBallVel = Csng((INT(SQR((ball.VelX^2)+(ball.VelY^2))))/50)
	If TempPitchBallVel = 1 Then TempPitchBallVel = 0.999
	If TempPitchBallVel = 0 Then TempPitchBallVel = 0.001
	TempPitchBallVel = Csng(1/(1+(0.275*(((0.75*TempPitchBallVel)/(1-TempPitchBallVel))^(-2))))) * 10000
	PitchPlayfieldRoll = TempPitchBallVel
End Function

'//  Calculates the pitch of the sound based on the ball speed.
'//  Used for plastic ramps roll sound
Function PitchPlasticRamp(ball)
    PitchPlasticRamp = BallVel(ball) * 20
End Function

'//  Determines if a point (px,py) in inside a circle with a center of 
'//  (cx,cy) coordinates and circleradius
Function InCircle(px,py,cx,cy,circleradius)
	Dim distance
	distance = SQR(((px-cx)^2) + ((py-cy)^2))
	
	If (distance < circleradius) Then
		InCircle = True
	Else
		InCircle = False
	End If
End Function

'///////////////////////////  PLAY SOUNDS SUBROUTINES  //////////////////////////
'//  
'//  These Subroutines implement all mechanical playsounds including timers
'//  
'//////////////////////////  GENERAL SOUND SUBROUTINES  /////////////////////////
Sub SoundStartButton()
	PlaySoundAtLevelStatic (Cartridge_Cabinet_Sounds & "_Start_Button"), StartButtonSoundLevel, StartButtonPosition
End Sub

Sub SoundPlungerPull()
	PlaySoundAtLevelStatic (Cartridge_Cabinet_Sounds & "_Plunger_Pull_Slow"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerPullStop()
	StopSound Cartridge_Cabinet_Sounds & "_Plunger_Pull_Slow"
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic (Cartridge_Cabinet_Sounds & "_Plunger_Release_Ball_" & Int(Rnd*3)+1), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic (Cartridge_Cabinet_Sounds & "_Plunger_Release_Empty"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*3)+1), 0, NudgeLeftSoundLevel * fMechSoundVolume, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*3)+1), 0, NudgeRightSoundLevel * fMechSoundVolume, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySoundAtLevelStatic ("Nudge_" & Int(Rnd*3)+1), NudgeCenterSoundLevel * fMechSoundVolume, Drain
End Sub

'///////////////////////  JP'S VP10 BALL COLLISION SOUND  ///////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound (Cartridge_BallBallCollision & "_BallBall_Collide_" & Int(Rnd*7)+1), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * fMechSoundVolume, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
	FlipperCradleCollision ball1, ball2, velocity
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

'//////////////////////////  STADNING TARGET HIT SOUNDS  ////////////////////////
Sub RandomSoundTargetHitStrong()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_5",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_6",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_7",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_8",DOFTargets), Vol(ActiveBall) * TargetSoundFactor		
	End Select
End Sub

Sub RandomSoundTargetHitWeak()
	Select Case Int(Rnd*4)+1		
		Case 1 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_1",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_2",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_3",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX(Cartridge_Targets & "_Target_Hit_4",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
	End Select
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  /////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_2"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_12"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 3 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_14"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 4 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_18"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 5 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_19"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_20"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 7 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_21"), Vol(aBall) * BallBouncePlayfieldSoftFactor, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	Select Case Int(Rnd*12)+1
		Case 1 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_1"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 2 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_3"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 3 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_7"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 4 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_8"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 5 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_9"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 6 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_11"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 7 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_13"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 8 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_15"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 9 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_16"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 10 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_17"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 11 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_22"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 12 : PlaySoundAtLevelStatic (Cartridge_Ball_Drop_Bump & "_Ball_Rebound_23"), Vol(aBall) * BallBouncePlayfieldHardFactor, aBall
	End Select
End Sub

'////////////////////////////////////  DRAIN  ///////////////////////////////////
'///////////////////////////////  OUTHOLE SOUNDS  ///////////////////////////////
Sub RandomSoundOutholeHit(sw)
	PlaySoundAtLevelStatic (Cartridge_Trough & "_Outhole_Drain_Hit_" & Int(Rnd*4)+1), Solenoid_OutholeKicker_SoundLevel, sw
End Sub

Sub RandomSoundOutholeKicker()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Trough & "_Outhole_Kicker_" & Int(Rnd*4)+1,DOFContactors), Solenoid_OutholeKicker_SoundLevel, Drain
End Sub

'/////////////////////  BALL SHOOTER FEEDER SOLENOID SOUNDS  ////////////////////
Sub RandomSoundShooterFeeder()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Trough & "_Shooter_Feeder_" & Int(Rnd*6)+1,DOFContactors), Solenoid_ShooterFeeder_SoundLevel, Drain
End Sub

'///////  SHOOTER LANE - BALL RELEASE ROLL IN SHOOTER LANE SOUND - SOUND  ///////
Sub SoundBallReleaseShooterLane(toggle)
	Select Case toggle
		Case SoundOn
			PlaySoundAtLevelActiveBall (Cartridge_Table_Specifics & "_Ball_Launch_from_Shooter_Lane"), BallReleaseShooterLaneSoundLevel
		Case SoundOff
			StopSound Cartridge_Table_Specifics & "_Ball_Launch_from_Shooter_Lane"
	End Select
End Sub

'//////////////////////////////  AUTO PLUNGER  //////////////////////////////
Sub RandomSoundAutoPlunge()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Spinner & "_Auto_Launch_Coil_" & RndInt(1,5), DOFContactors), AutoPlungerSoundLevel, KickerAutoPlunge
End Sub

'//////////////////////////////  KNOCKER SOLENOID  //////////////////////////////
Sub KnockerSolenoid(enabled)
	'PlaySoundAtLevelStatic SoundFX(Cartridge_Knocker & "_Knocker_Coil",DOFKnocker), Solenoid_Knocker_SoundLevel, KnockerPosition
	if Enabled then
		PlaySound SoundFX(Cartridge_Knocker & "_Knocker_Coil",DOFKnocker), 0, Solenoid_Knocker_SoundLevel
	end if
End Sub

'//////////////////////////  SLINGSHOT SOLENOID SOUNDS  /////////////////////////
Sub RandomSoundSlingshotLeft()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Slingshots & "_Slingshot_Left_" & Int(Rnd*26)+1,DOFContactors), Solenoid_Slingshot_SoundLevel, SLING2
End Sub

Sub RandomSoundSlingshotRight()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Slingshots & "_Slingshot_Right_" & Int(Rnd*25)+1,DOFContactors), Solenoid_Slingshot_SoundLevel, SLING1
End Sub

Sub RandomSoundSlingshotUpper()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Slingshots & "_Slingshot_Left_" & Int(Rnd*26)+1,DOFContactors), Solenoid_Slingshot_SoundLevel, Sling001
End Sub

'///////////////////////////  BUMPER SOLENOID SOUNDS  ///////////////////////////
'////////////////////////////////  BUMPERS - TOP  ///////////////////////////////
Sub RandomSoundBumperLeft(Bump)
'	Debug.Print Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier
	PlaySoundAtLevelStatic SoundFX(Cartridge_Bumpers & "_Jet_Bumper_Left_" & Int(Rnd*22)+1,DOFContactors), Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier, Bump
End Sub

Sub RandomSoundBumperUp(Bump)
	'Debug.Print Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier
	PlaySoundAtLevelStatic SoundFX(Cartridge_Bumpers & "_Jet_Bumper_Up_" & Int(Rnd*25)+1,DOFContactors), Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier, Bump
End Sub

Sub RandomSoundBumperLow(Bump)
	'Debug.Print Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier
	PlaySoundAtLevelStatic SoundFX(Cartridge_Bumpers & "_Jet_Bumper_Low_" & Int(Rnd*28)+1,DOFContactors), Vol(ActiveBall) * Solenoid_Bumper_SoundMultiplier, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////
Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic (Cartridge_Spinner & "_Spinner_Spin_Loop"), SpinnerSoundLevel, spinnerswitch
End Sub

'///////////////////////  FLIPPER BATS SOUND SUBROUTINES  ///////////////////////
'//////////////////////  FLIPPER BATS SOLENOID CORE SOUND  //////////////////////
Sub RandomSoundFlipperLowerLeftUpFullStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Full_Stroke_" & RndInt(1,10),DOFFlippers), FlipperLeftLowerHitParm, Flipper
End Sub

Sub RandomSoundFlipperLowerLeftUpDampenedStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Dampened_Stroke_" & RndInt(1,23),DOFFlippers), FlipperLeftLowerHitParm * 1.2, Flipper
End Sub

Sub RandomSoundFlipperLowerRightUpFullStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Full_Stroke_" & RndInt(1,11),DOFFlippers), FlipperRightLowerHitParm, Flipper
End Sub

Sub RandomSoundFlipperLowerRightUpDampenedStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Dampened_Stroke_" & RndInt(1,23),DOFFlippers), FlipperLeftLowerHitParm * 1.2, Flipper
End Sub

Sub RandomSoundFlipperUpperLeftUpFullStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Full_Stroke_" & RndInt(1,10),DOFFlippers), FlipperLeftUpperHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpperLeftUpDampenedStroke(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Dampened_Stroke_" & RndInt(1,23),DOFFlippers), FlipperLeftUpperHitParm * 1.2, Flipper
End Sub

Sub RandomSoundFlipperLowerLeftReflip(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Reflip_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperLowerRightReflip(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_Reflip_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperUpperLeftReflip(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Reflip_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperLowerLeftDown(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Down_" & RndInt(1,10),DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperLowerRightDown(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_Down_" & RndInt(1,11),DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperUpperLeftDown(flipper)
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_Down_" & RndInt(1,10),DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundLowerLeftQuickFlipUp()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_QuickFlip_Up_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, LeftFlipper
End Sub

Sub RandomSoundLowerRightQuickFlipUp()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Right_QuickFlip_Up_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, RightFlipper
End Sub

Sub RandomSoundUpperLeftQuickFlipUp()
	PlaySoundAtLevelStatic SoundFX(Cartridge_Flippers & "_Flipper_Lower_Left_QuickFlip_Up_" & RndInt(1,3),DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, LeftFlipper
End Sub

Sub StopAnyFlipperLowerLeftUp()
	Dim anyFullStrokeSound
	Dim anyDampenedStrokeSound
	For anyFullStrokeSound = 1 to 10
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Full_Stroke_" & anyFullStrokeSound)
	Next
	For anyDampenedStrokeSound = 1 to 23
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Dampened_Stroke_" & anyDampenedStrokeSound)
	Next
End Sub

Sub StopAnyFlipperLowerRightUp()
	Dim anyFullStrokeSound
	Dim anyDampenedStrokeSound
	For anyFullStrokeSound = 1 to 11
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Full_Stroke_" & anyFullStrokeSound)
	Next
	For anyDampenedStrokeSound = 1 to 23
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Right_Up_Dampened_Stroke_" & anyDampenedStrokeSound)
	Next
End Sub

Sub StopAnyFlipperUpperLeftUp()
	Dim anyFullStrokeSound
	Dim anyDampenedStrokeSound
	For anyFullStrokeSound = 1 to 10
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Full_Stroke_" & anyFullStrokeSound)
	Next
	For anyDampenedStrokeSound = 1 to 23
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_Up_Dampened_Stroke_" & anyDampenedStrokeSound)
	Next
End Sub

Sub StopAnyFlipperLowerLeftDown()
	Dim anyFullDownSound
	For anyFullDownSound = 1 to 10
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_Down_" & anyFullDownSound)
	Next
	For anyFullDownSound = 1 to 3
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_QuickFlip_Up_" & anyFullDownSound)
	Next
End Sub

Sub StopAnyFlipperLowerRightDown()
	Dim anyFullDownSound
	For anyFullDownSound = 1 to 10
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Right_Down_" & anyFullDownSound)
	Next
	For anyFullDownSound = 1 to 3
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Right_QuickFlip_Up_" & anyFullDownSound)
	Next
End Sub

Sub StopAnyFlipperUpperLeftDown()
	Dim anyFullDownSound
	For anyFullDownSound = 1 to 10
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_Down_" & anyFullDownSound)
	Next
	For anyFullDownSound = 1 to 3
		StopSound(Cartridge_Flippers & "_Flipper_Lower_Left_QuickFlip_Up_" & anyFullDownSound)
	Next
End Sub

'///////////////////////  FLIPPER BATS BALL COLLIDE SOUND  //////////////////////
dim angdamp, veldamp
angdamp = 0.2
veldamp = 0.8

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm

	If parm => 22 Then
		' Strong hit safe values boundary
		' Flipper stroke dampened
		FlipperLeftLowerHitParm = FlipperUpSoundLevel * 0.1
	Else
		If parm =< 1 Then 
			' Weak hit safe values boundary
			' Flipper stroke full
			FlipperLeftLowerHitParm = FlipperUpSoundLevel
		Else
			' Fully modulated hit
			FlipperLeftLowerHitParm = FlipperUpSoundLevel * (1-(parm/25))
		End If
	End If

	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm

	If parm => 22 Then
		' Strong hit safe values boundary
		' Flipper stroke dampened
		FlipperRightLowerHitParm = FlipperUpSoundLevel * 0.1
	Else
		If parm =< 1 Then 
			' Weak hit safe values boundary
			' Flipper stroke full
			FlipperRightLowerHitParm = FlipperUpSoundLevel
		Else
			' Fully modulated hit
			FlipperRightLowerHitParm = FlipperUpSoundLevel * (1-(parm/25))
		End If
	End If

 	RandomSoundRubberFlipper(parm)
End Sub

Sub LeftUpperFlipper_Collide(parm)
	If parm => 22 Then
		' Strong hit safe values boundary
		' Flipper stroke dampened
		FlipperLeftUpperHitParm = FlipperUpSoundLevel * 0.1
	Else
		If parm =< 1 Then 
			' Weak hit safe values boundary
			' Flipper stroke full
			FlipperLeftUpperHitParm = FlipperUpSoundLevel
		Else
			' Fully modulated hit
			FlipperLeftUpperHitParm = FlipperUpSoundLevel * (1-(parm/25))
		End If
	End If

 	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Flipper_Hit_" & Int(Rnd*7)+1), parm / 25 * RubberFlipperSoundFactor
End Sub

'//////////////////////////  SOLENOID A/C SELECT RELAY  /////////////////////////
Sub Sound_Solenoid_AC(toggle)
	Select Case toggle
		Case CircuitA
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_AC_Select_Relay_Side_A"), RelaySolenoidACSelectSoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_AC_Select_Relay_Side_A"), RelaySolenoidACSelectSoundLevel, ACSelectPosition
		Case CircuitC
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_AC_Select_Relay_Side_C"), RelaySolenoidACSelectSoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_AC_Select_Relay_Side_C"), RelaySolenoidACSelectSoundLevel, ACSelectPosition
	End Select
End Sub

'//////////////////////////  GENERAL ILLUMINATION RELAYS  ///////////////////////
Sub Sound_LowerGI_Relay(toggle)
	Select Case toggle
		Case SoundOn
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Lower_Playfield_and_Backbox_GI_Relay_On"), RelayLowerGISoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Lower_Playfield_and_Backbox_GI_Relay_On"), RelayLowerGISoundLevel, GIUpperPosition
		Case SoundOff
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Lower_Playfield_and_Backbox_GI_Relay_Off"), RelayLowerGISoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Lower_Playfield_and_Backbox_GI_Relay_Off"), RelayLowerGISoundLevel, GIUpperPosition
	End Select
End Sub

Sub Sound_UpperGI_Relay(toggle)
	Select Case toggle
		Case SoundOn
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Upper_Playfield_GI_Relay_On"), RelayUpperGISoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Upper_Playfield_GI_Relay_On"), RelayUpperGISoundLevel, GILowerPosition
		Case SoundOff
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Upper_Playfield_GI_Relay_Off"), RelayUpperGISoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Upper_Playfield_GI_Relay_Off"), RelayUpperGISoundLevel, GILowerPosition
	End Select
End Sub

'///////////////////////////////  FLASHERS RELAY  ///////////////////////////////
Sub Sound_Flasher_Relay(toggle, tableobj)
	Select Case toggle
		Case SoundOn
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Flashers_Relay_On"), RelayFlasherSoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Flashers_Relay_On"), RelayFlasherSoundLevel, tableobj
		Case SoundOff
			If RelaysPosition = 1 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Flashers_Relay_Off"), RelayFlasherSoundLevel, GIUpperPosition
			If RelaysPosition = 2 Then PlaySoundAtLevelStatic (Cartridge_Relays & "_Relays_Flashers_Relay_Off"), RelayFlasherSoundLevel, tableobj
	End Select
End Sub

'////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  /////////////////////
'/////////////////////////////  RUBBERS AND POSTS  //////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ///////////////////////////////
Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
 		RandomSoundRubberStrong() 
	End if
	If finalspeed <= 10 then
 		RandomSoundRubberWeak()
 	End If	
End Sub

'/////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  /////////////////////
Sub RandomSoundRubberStrong()
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_Strong_10"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub

'///////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  /////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall (Cartridge_Rubber_Hits & "_Rubber_Hit_" & Int(Rnd*8)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub


'/////////////////////////////  WALL IMPACTS EVENTS  ////////////////////////////

Sub Wall46_Hit()
	RandomSoundMetal()
End Sub

'RandomSoundBottomArchBallGuideSoftHit - Soft Bounces
'Sub Wall017_Hit() : RandomSoundBottomArchBallGuideSoftHit() : End Sub
'Sub Wall26_Hit() : RandomSoundBottomArchBallGuideSoftHit() : End Sub

'RandomSoundBottomArchBallGuideHardHit - Hard Hit
Sub Wall017_Hit() : RandomSoundBottomArchBallGuideHardHit() : End Sub
'Sub Wall063_Hit() : RandomSoundBottomArchBallGuideHardHit() : End Sub

'RandomSoundFlipperBallGuide
'Sub Wall49_Hit() : RandomSoundFlipperBallGuide() : End Sub
'Sub Wall48_Hit() : RandomSoundFlipperBallGuide() : End Sub

'Outlane - Walls & Primitives
Sub Wall70_Hit() : RandomSoundOutlaneWalls() : End Sub
Sub Wall5_Hit() : RandomSoundOutlaneWalls() : End Sub
Sub Wall73_Hit() : RandomSoundOutlaneWalls() : End Sub
Sub Wall24_Hit() : RandomSoundOutlaneWalls() : End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  /////////////////////////////
Sub RandomSoundMetal()
	Select Case Int(Rnd*20)+1
		Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_1"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_2"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_3"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_4"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_5"), Vol(ActiveBall) * 0.02 * MetalImpactSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_6"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_7"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_8"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_9"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_10"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 11 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_11"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 12 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_12"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 13 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_13"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 14 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_14"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 15 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_15"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 16 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_16"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 17 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_17"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 18 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_18"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 19 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_19"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 20 : PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Metal_Hit_20"), Vol(ActiveBall) * MetalImpactSoundFactor
	End Select
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub HitsWoods_Hit(idx)
'	debug.print "wood hit"
	RandomSoundWood()      
End Sub

Sub RandomSoundWood()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub

'///////////////////////////////  OUTLANES WALLS  ///////////////////////////////
Sub RandomSoundOutlaneWalls()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Outlane_Wall_" & Int(Rnd*9)+1), OutlaneWallsSoundFactor
End Sub

'///////////////////////////  BOTTOM ARCH BALL GUIDE  ///////////////////////////
'///////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////
Sub RandomSoundBottomArchBallGuideSoftHit()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Arch_Ball_Guide_Hit_Soft_" & Int(Rnd*4)+1), BottomArchBallGuideSoundFactor
End Sub


'//////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall (Cartridge_Metal_Hits & "_Arch_Ball_Guide_Hit_Hard_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 3
End Sub

'//////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall (Cartridge_Apron & "_Apron_Hit_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall (Cartridge_Apron & "_Apron_Hit_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall (Cartridge_Apron & "_Apron_Hit_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
 	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall (Cartridge_Apron & "_Apron_Hit_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End if
End Sub

'/////////  PLASTIC LEFT RAMP - RIGHT EXIT HOLE - TO PLAYFIELD - EVENT  /////////

'sub BallDrop1_hit	'sometimes drop sound cannot be heard here.
''	debug.print activeball.velz
'	RandomSoundRightRampRightExitDropToPlayfield(BallDrop1)
'end sub


Sub BallDrop2_Hit()
	if abs(activeball.AngMomZ) > 70 then activeball.AngMomZ = 70
	activeball.AngMomZ = abs(activeball.AngMomZ) * 3
	RandomSoundRightRampRightExitDropToPlayfield(BallDrop2)
'	Call SoundRightPlasticRampPart2(SoundOff, ballvariablePlasticRampTimer1)
End Sub

'ss ramp
sub BallDrop3_hit
'	debug.print "SS drop"
	RandomSoundRightRampRightExitDropToPlayfield(activeball)
end sub

'/////////  METAL WIRE RIGHT RAMP - EXIT HOLE - TO PLAYFIELD - EVENT  //////////
Sub RHelper3_Hit()
	if abs(activeball.AngMomZ) > 70 then activeball.AngMomZ = 70
	activeball.AngMomZ = -abs(activeball.AngMomZ) * 3
	RandomSoundLeftRampDropToPlayfield()
End Sub

'
''***************************************************************
''Table MISC VP sounds
''***************************************************************

dim FaceGuideHitsSoundLevel : FaceGuideHitsSoundLevel = 0.002 * RightPlasticRampHitsSoundLevel
Sub ColFaceGuideL1_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_2"), FaceGuideHitsSoundLevel, activeball : End Sub
Sub F1Guide_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_1"), FaceGuideHitsSoundLevel, activeball : End Sub
Sub F1Guide2_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_1"), FaceGuideHitsSoundLevel, activeball : End Sub
Sub ColFaceGuideL2_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_2"), FaceGuideHitsSoundLevel, activeball : End Sub
Sub ColFaceGuideL3_Hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_3"), FaceGuideHitsSoundLevel, activeball : End Sub

sub pBlockBackhand_hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_4"), FaceGuideHitsSoundLevel, activeball : End Sub
sub pBlockBackhand001_hit(): PlaySoundAtLevelStatic (Cartridge_Plastic_Ramps & "_Ramp_Right_Plastic_Hit_5"), FaceGuideHitsSoundLevel, activeball : End Sub

													'volume level; range [0, 1]

' '///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
' Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim DelayedBallDropOnPlayfieldSoundLevel
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]

' '/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
' '/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * fMechSoundVolume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, aVol * fMechSoundVolume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub WireSwitches_Hit(idx)
	RandomSoundRollover
End Sub

' '/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
	RandomSoundWood()      
End Sub

' '/////////////////////////////  METAL - EVENTS  ////////////////////////////
Sub Metals2_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

' '/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub PlayTargetSound()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 10 then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else 
		RandomSoundTargetHitWeak()
	End If	
End Sub

'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
	'PlaySoundAtLevelStatic SoundFX("droptarget" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

Sub DOFGadget (nDOF, nState, nEffect)
	If DOFeffects(nEffect) <> 0 Then
		DOF nDOF, nState
	End If
End Sub

' *****************************
' ****** Music functions ******
' *****************************

' Hack to return Narnia ball back in play
Sub TimerNarnia_Timer
    Dim b
	For b = 0 to UBound(gBOT)
		if gBOT(b).z < -200 Then
													  
			gBOT(b).angmomx= 0 : gBOT(b).angmomy= 0 :	gBOT(b).angmomz= 0
			gBOT(b).velx = 0 : gBOT(b).vely = 0 : gBOT(b).velz = 0
			gBOT(b).x = 914 : gBOT(b).y = 1875  : gBOT(b).z = 1
			KickerAutoPlunge.enabled = True
		end if
	next
end sub
