' ****************************************************************
'                       VISUAL PINBALL X
'                Game FrameWork JP Salas using Darks DMD Flashes
'Scripting by JP Sals & RusstyT and scripting inspred by Scotty Wickburgh
'		plain VPX script using core.vbs for supporting functions
'                         Version Under A Black Flag2.0.1
' ****************************************************************
'This concept for this game has evolved from trying to make Williams 1980 era game Scorpion fun. Affter be being uninspired by my previous attempts I decided to 
'make the game a narrow body table based on the TimeLord base game template that had the scoring, Nfozzy, Fleep sounds and Luts set up with JPs Framework and DMD flashers
'Setup


'Version6a-Starts with Crackers mod of version 5g which he moved og. compression sounds from the table to back glass to prevent balckglass sounds being pulled across to the table
' This is a known issue that occurs with VPX if an ogg compression sound is alloccated to the table and it creates a serious issue to some peoples cabinets.The latest VPX10.8Candiate 3 has fixed this issue
'Removed The 4 Chicken Callouts & Replaced with 4 Chicken Sounds. Removed 3Gull callouts and replkaced these with gullSounds
'Removed music CO_PennyWhistle..redundant opening sound
'Removed CO_HerCompR, CO HeyKnockItOff, CO__liftOff, CO_Lock1Ready,CO_Lock2Ready(From TimeLord)
'Removed My Non usesd sounds- CO_oooooNiceShot,CO_LongWalkShortPlank,CO_LovelyMermaids,CO_LovelyMermaids2,CO_SkillShotPrepareThePlank
'Replaced BlueBackWalls and spinner wall with a chrome wall
'Rotated the DMD Flashers to 47degrees(from 50), Chnaged bumper water swirl image
'New Playfield,Plastics, PlayfieldOverlay & Apron Overlays with pirate style text

'Version6b
'Removed drain.ogg,fx_flip_hit_3.ogg,fx_flip_hit_2.wav,fx_flip_hit_1.wav,fx_arrowwoosh.ogg,fx_metalrolling.ogg,fx_metalhit.ogg,fx_minrampwoosh.wav,fx_Rolling_Metal0 to 5.ogg
'Removed fx_Rolling_Plastic0 to 5.ogg,fx_Rolling_Wood0 to 9.wav,fxrr1 to 7.wav,fx_rrenter.wav,fx_rubber_hit_1 to 3,fx_rubber.wav fx-rubberband.wav, fx_rubber2.wav, fx_rubberpin
'Removed fx_resetdrop.wav,fx_saucerHit.wav,fx_tinyrocket2.ogg,fx_turnstilerotate,fx_wirerampexit.wav,fx_wireramp_exit,fx_woodhit,fx_woosh.ogg,fx_woosh.wav,lane.wav
'Removed fx prefix from fxthunder.ogg, fx_rainthunder,Fx_Thunder3, fx_wave.ogg,FX_ShotHitsWater.ogg,FX_ShotHitsWater2.ogg
'Removed fx_Woosh3woosh.ogg,FX_WooshFireCrackle.ogg,fx_WooshHigh2, wireenter.wav
'Removed m_wiredtothemoonandwaves,m_PickleJarIntro,SixMonthsEqdStereoWidened
'Changed Wireloop1 from backglass to table
'Removed redundant Flipper_ReFlip_L02.wav Flipper_ReFlip_R02.wav,Flipper_Right_Down_1.wav 
'Replaced CO_Tilt & CO_TiltWarning with danger.wav

'Version6C Added flashers to top loop,chicken target and gull target, Add Flasher Lights to highlight playfield objects. Flasher Fires added to ship damage
' BallSaverSpinner for left lane, golden reef release and treasure chest release
' Removed sounds tlfanfare1 2 3 4.ogg, used fx_fanfare1 & 2.wav, removed redundantfx sounds for ball drop, gates, lr,fx_launchball,fx_kicker_enter,fx_metalhit.wav, fx_metalrolling.wav,fx_kicker.wav
' Removed Sounds fx_nudge.wav, fx_passivebumper.wav,fx_metalhit.wav, fx_plastichit.wav,fx_plunger_empty.wav,fx_plungerpull.wav,fx_popper.wav,fx_postrubber.wav
' Removed prefix fx from fx_Canon***.ogg sounds

'6D Added BarbaBlancaMagnet and light, Fixes to apron overlays, JoeSoap, Smuggy & Starting, Fixed side rails and rail shadow and side blades showing through rails.
'6F InvisibleGuides for canon to VariTarget
'6G Removed Hard Timers from 5Awards, Dirty Creature and Walk the plank. All Modes set to 100 seconds, HelloPolly trigger1 enabled when parrot moves Update
'	BarbaBlanca Magna grab implemented after a ship is sunk 
'6h Coverted m_pioneer, m_titus & m_DirtyCreature to .ogg sound, Rempved sound CO_wherDidAllThoseBallsCompR, New SailorCalloutsfor spotting Ships
' 	Callouts for SharkAttack, OffthePlank& RaiseTheSails, Removed redundant Callout files, Included Smuggy tavern callout, removed two The Bruce callouts
'	Tided all the CalloutActive Flags & CalloutTimer.Enabled commands.
'6i Scripted WildSeas, updated credits,
'6j Completed walk the Plank/FeedThe Fish/Mermaid Fantasies, Adjusted playfield lights,Completed Hidden Treasure
'6k Round The horn combo play left loop activates when no other awards are available, second loop gets round the horn which lights left aramp for combo 
'   PlasticOnlightFlasher, Animated palm trees,   
'6m Tidied up Dirty creature finish.NewEasy/Hard callouts, New walkThePlankCallouts, New Round the horn callouts, Saver wall that works with ball saver spinner to stop close flipper drains and still all post bounce.
'	Incorporated Mode Complete(CurrentPlayer) checks for award lights to stop wrong light states.Adjusted Inlane guides.
'6n Lightned top table, gi lighting improved, new colorflex/Ramp6 image applied to top walls. removed reflextionsfrom barrel wire guids and top Walls,
'   Last drinks award light turned off if mode is not complete, dirty ceature overlay stays on of second mode occurs
'	Turned off mode lights at reset for NewPlayer Ball and turned off Award lights if The Mode Complete(CurrentPlayer)=false.
'6o Changed overlay and playfield to round kickers in prep for playfield mesh.Fixed WalkThe Plank lights and removed extra JackPot. Moved Mermaid target wall behind target stems
'	changed colour of kickerJewel lights.
'6p-6q & 6r Lots of mucking round with lights and white ball light on playfield. Playfield light bling all thanks to Oqqsan teaching me these tricks. thankyou Oqqsan
' 	Fixed overlay radiusus to hide primitives and adhted the plafield kicker graphics in prep for plafield mesh. Icluded primitives ender kickers to stop Then
'	the ball falling through when the mesh is applied. Included a grace saver spinner change of colour as well. Thankyou captain Crackers.
'	Ships set to catch on fire with canon hit, Adjusted fish lights and primitives, Agusted parrot lights for double internal reflections. Adjusted mermaid spinner
'	Set ship award to 1Million for hot + 1MillxBonus multipler at the end of ball, Set up ship sink light sequence to circle out for sink and lights up for new ship.
'6s Fixed timer clash for spinner drain which left ring visible. Reset Dirty creature lights for player new ball. fixed awardjackpot1 in walk the plank to AwardJackpot
'6t,6ta New backdrop with ships and bluesky, rescripted Treasure chest kicker to stop the ball being trapped,Fixed Fishes lights to stop coming on when WalkThe PlankActive
'6tb new cllouts for sailor and pickles. re equed and normailised.
'6u DOF applied..Thankyou Outhere, Revenge DMD attact added when ship is sunk and Elinated overlay added to apron ..Thankyou Gramps for the idea,
' Went back to overlay without text shadow because this blurred text on the 4K cab.Fixed ship and Apron carryovers for multiplayers. Rest Canon targets at PlayerNewBall
' Stop Barba Blanca and DaveyJones at resetforplayernewball: Note Walk thePlank (CurrentPlayer) carries over and there is no new ball reset. 
' New ship counter Dim ShipBonusCounter to count total number of ships for End Of ball bonus, Shortened ship sunk light sequence 
'6v Went mad fixing the multiplayerships, lights, fires, and Ship Sunk counters. 3 days solid work. Seems to working now. I ran an extra super sub ShipNowSetForNewBallReset which is used for multiplayer Reset for new ball variable. Ship set now does the work for the in player ship sinking
' To avoid the light state of the vary target being 1 when the ball drains straight after sinking ShipNowSetForNewBallReset also has a check and reset for this event. Set KickerCanonLoad release to a new invisible ramp instead of onto the apron. Thanks Captain Crackers
' scripted hard-Easy plays for The tavern and wild seas. I'm adding JPs DMD for plays made with the intention that the callout timer will be extended to avoid overlap of callouts.
'6W This is a huge update-The easy hard variations are scripted in requiring many cahnges to get the modes to work and carry over to multiplayer, 2 new song tracks have been add and each time a mode is completed or or a ship is sunk the music changes. The rules have now been written in line With
' the game play and a rough score balance is now in place. The games is biased toward mode completion beacuse this is where lots of interesting thing happen and music not heard in some of the gtame play. No doubt this will require tweaqking
' The game now has DOF thanks to outhere who is credited in the DMD along with the Bruce (Gramps) who is helping shape the final game along with crackers.
'6x I change the apron flashers to match the new sink ship values. Added a lot of table flashers for awards.
'6y Fixed addscore mistype, Adjusted award flashers (timiming, duration, colour and opacity to match game flow), adjusted Sink ship DMD values to match reqards on apron overlays.Arrrr!
'6z Drop pickle jars ramp if ball has ended and not complete
'7a Scripted Callout check to Start of Awards line 5944. Added JPS Text DMDS to plays in case a call is prevented from playing
'7b Scripted Modes complete to count towards a superduper award. This resets awards lights and award flags but maintains the the total number of modes complete for EOB BonusCounter
'  Tidied up all the callout overlaps using the 5 second calloutactive timer. happy with the result. Quite simple and works ..fingers crossed. Put lots of play DMDs in if callout could not be heard There are other indicators including  table flashers
' Fixed accidental magna save select during golden reef award selection. Fixed Tilt DMDs, Adjust flashers, light sequecnce and song for multiplayer first ball.
'7c added DOF to SuperJackpots and Added a super Duper award for completing ships
'7d fixed post rubber sounds.
'7e Change gamename back to PiratesLife2.0 to match game name in DOF. Included Tomates attractDMD. allowed easy hard callouts to play at any time during attract. 
'7g Tomates ramps and script in place for the lift ramp. Bofhead and monkey jump when the ball enters the canon ramp
'7h . Added DC multiball, added wildseas multiball. added 5.2 sec Delay on barba blanca magna hurry up and a multiball for every second hurryup achieved
' adjusted ramp protector at lift pos. 
'7k Music volume is reduced by 1 dB to make table sound smore prominant. Changed the Ramp trap method changed to destroy and create ball because lift ramp methothod wasn't working.
'Added ballsaver timer to the tavern
'PiratesLife1.0.1 Added black flashers outside of cabinet, Adusted highScores 100Mill to 400Million, added gates to stop reverse ball hitting lane kicker saves
'Added extra gate in the ball release lane to prevent nania in multiball when a ball ejects while there is already one in the plunger lane1
'Changed shape of wall around kicker drain to stop balls from left and right hitting the drain kicker at the same time.
'1.0.3 KickerCanonLoadUK moves to RampATC to avoid ball hangup from bone hole, Change skulls to non collideable, added fan on for wild seas and drain spinners. trialed DOFon DOFoff but was not successful
'Incuded fan and beacon on for the wild seas spinner. Deactivated DOF for Drain Save spinner shaker
'1.0.4 Changed armed the canon targets drop sub routine to _Hit and .isdropped included in the sub. seperate sub routined added for skill shot dropping targets.
'1.0.5 Invisble wall prevents multipull balls entering pickles treasure lift ramp and portective walls raise when a trapped ball is released
'1.0.6 Removed Vertical kick component from KickerCanonLoadUKRelease.
'2.0 UnderBlackFlagMusic Mod. Removed remnants of vertical upckicker for canon ramp. This included and vertical kick complonent. 
'2.0.1 Adjusted hit wall for skeleton near Cave. Change song is instrumental from main theme for next ball. this is to add dynamics to the game.Put slope on ramp top to plank upkicker to stop ball catching
'adjust parrot calls, Music By AnttiMartikainen.Additional backwalls for dirty creature. Now only front collideable.Targets in plunger lane now don't contribute to DirtyCreature award.
'Fixed ramps to walk the plank upkicker. 
'2.02 reduce music by 2dB to hear callouts and table sounds, DaRdog added Mega VR pirate ship room, lowered canon fir flashers.Added JP's ball scratches, Drop wall added to ramp gate to help left lane shot, Extended ball save on Polly mode,
''Adjsuted rails or desktop and cabinet modeds. Adjusted the angle to desktop DMD
'2.1 Added random ship select, adjusted lighting, increased value of super jackpot
'*******************************************************************************************************************************
'  Credits
'GameConcept RusstyT & HP McDuck. So funny spitballing ideas and developing a story and characters. This game and story is our Brain child.  
'JP Salas Your an absolute Legend, friend and mentor. The base framework and anything that pops up and down and spins is directly scripted or copied from JPSalas. JP has always encouraged Me
' from the first time I asked if I could use his Serious Sam Game Framework. He guided me through my unconventional follow the lights scripting in TimeLord.
'Andre Pena worked with me for final character development with Scurvy Dick Narration and the Scumbag Sailor.What a privelege to work with this superstar and
' Voice and animation actor of Louis in resident evil4
'in the blockbuster game Resident Evil4. Bringing the game Characters to life with Andre is definately a highlight of this build.
' Ramps by Tomate...what a thing of beauty these are. Its fun hanging out with this fellah. and watch his magic come to life. 
'Rotating Bumper caps, Mermasid Spinner Spiner and tracking Dragon Eyeball ideas from Scotty Wickburghs tables
'Gedankekojote97 applied nFozzy/Roth physics,FleepSounds, Ramp Textures & luts carried over from TimeLord
'DOF setup by Outhere. Thanks mate
'incredible ramps by Tomate. These are stunners. Tomate taught me a on off light tricks with ramps. 
'Alpophis did the mechanical tilt in TimeLord and answered some much needed technical issues  Backglass design and text prompts came through discussions with Apophis. good honest feedback.
'Sixtoes has great ideas and followed the game developement. Bumper wash rings, barrel rings, easy/hard, eliminating chance of down the guts plays, plastics differentiation
' all come from discussion with Sixtoes. All with good will, honesty and humour. Sixtoe has made this journey enjoyable
'Crackers is the game play quality control and constant checker in the game build. He was my mainstay in Time Lord and has stood by me to bring Pirates Life home.
'Gramps...Good friend, honest adviser with some great ideas. 
'DarthVito pointed out some sound issues that hopefully have been elimanted. 
'Studlygoorite and Cliffy popped in as did MahahaKenji. Thankyou guys
'Thankyou to Flupper and Bord for tutorials 
'Thankyou to all who made  VPX framework
'Last but not least Smaug do the PR and release. Without Smaugs encouragement Pirates life and TimeLord 3 would have not eventuated. please not to much more encouragement
'

'********************************************************************************************************************************

''''''''''''''''''''DOF By Outhere

'101 - Flipper Left
'102 - Flipper Right
'103 - Slingshot Left
'104 - Slingshot Right
'105 - Bumper Back center (Top)
'106 - Bumper Back Right
'107 - Bumper Back Right (Outer)
'108 - Bumper Back Left
'109 - Bumper Back center (Bottom)
'110 - Put Ball in Plunger Line ()
'111 - Drop Target Reset
'112 - Auto Fire
'113 - UpThePlank (Kicker)
'114 - RightKickBack
'115 - Sub TriggerShipHit_Hit (Kicker)
'116 - Sub UpKicker (Right)
'117 - 
'118 - LeftKickBack
'119 - Shaker (With Nebula)
'120 - Shaker (TreasureImageSpin_Timer)
'121 - See script
'122 - Knocker
'123 - 
'124 - Beacon (TreasureImageSpin_Timer)
'125 - See script
'126 - Fan (With Nebula)
'127 - See script
'128 - KickerCanonLoadUKRelease
'129 - See script
'130 - 
'131 - 
'132 - Sub CanonL1Fire
'133 - Sub TreasureChest2MoveUp()
'134 - Sub KickerTreasureKick
'135 - Sub DirtyCreatureRelease
'136 - Sub DaveyJonesRelease
'137 - Sub UpKickerGRRelease
'138 - Sub GoldenReefRelease
'151 - See script
'152 - See script

'Wylte - Segregated user options, fixed and updated ball shadows, deleted BallShadowUpdate timer(s?!), plastics added to Walls for hit sounds and given plastic physics, fixed left sleeves with peg physics,
'			de-orphaned top-right post, increased pf friction 0.015 -> 0.1, elasticity 0.02 -> 0.2, lowered flipper power and raised return strength for flipper tricks, increased kicker2 power,
'			halved plunger pull speed for DT, added credit key 2 support, halved DT nudge strength, re-normalized rolling sounds, 
'	PoV: x/y scale below 1, x scale just barely "flattening" circles on pf (like viewing circles edge-on IRL), inclination matches rails, FoV&Layback matched and raised as high as possible while spheres at back still spheres

Option Explicit
Randomize
Dim VRRoom
Dim Stuff
Dim UseFlexDMD
Dim GlassScratchesOn

'*********************************************************************************************
'  Player Options
'************************************************************************************************

Const FlexDMDHighQuality =	True
Const ForceBackglass = 1		' 1 = on ( for dt if you want the backglass on it )
Const AmbientBallShadowOn =	1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const DynamicBallShadowsOn=	1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that behaves like a true shadow!
									'2 = flasher image shadow, but it moves like ninuzzu's
Dim eyeFollowS: eyeFollowS= 8	'ms timer interval of eye follow speed try 5-20 range
Const BallBright = 1			'0 - Normal, 1 - Bright

Const SongVolume = 0.8			'1 is full volume. Value is from 0 to 1
Const VolumeDial = 0.8			'Overall Mechanical sound effect volume. Recommended values should be no greater than 1
Const BallRollVolume = 0.8		'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 1		'Level of ramp rolling volume. Value between 0 and 1
Dim mMagnaSave1


'VR OPTIONS....
 
VRRoom =0    ' 0 - Desktop/FS   1 - VRRoom   2 - Ultra Minimal Room   3 - Pirate Ship
GlassScratchesOn = 1     ' 0 - Scratches OFF  1 - Scratches ON

'End VR Options
'*********************************************************************************************
'  End Player Options
'************************************************************************************************

'//////////////---- LUT (Colour Look Up Table) ----//////////////
'0 = VPW Original 1 to 1
'1 = Fleep Natural Dark 1
'2 = Fleep Natural Dark 2
'3 = Fleep Warm Dark
'4 = Fleep Warm Bright
'5 = Fleep Warm Vivid Soft
'6 = Fleep Warm Vivid Hard
'7 = Skitso Natural and Balanced
'8 = Skitso Natural High Contrast
'9 = 3rdaxis Referenced THX Standard
'10 = CalleV Punchy Brightness and Contrast
'11 = HauntFreaks Desaturated
'12 = Tomate Washed Out 
'13 = Bassgeige
'14 = Blacklight
'15 = B&W Comic Book

Dim LUTset, DisableLUTSelector, LutToggleSound, bLutActive
LutToggleSound = True
LoadLUT
'LUTset = 0			' Override saved LUT for debug
SetLUT
DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1

' Use FlexDMD if in FS mode

If Table1.ShowDT = True then
	UseFlexDMD = True'Dont use Flex in desktop
	for each Stuff in JPDMDAll: Stuff.visible = true: next  'make JP's flashers visible for desktop users
		digitgrid.Opacity=60
		Flasher6.Visible=1
		Flasher11.Visible=1
		Flasher6b.Visible=0
		Flasher11b.Visible=0
	lrail.visible=True
	rrail.visible=True
'	LeftSideBlade.Visible=False
'	SideBladeRight.Visible=False
	Ramp003.Visible=True
Else
	if VRRoom > 0 then 
	UseFlexDMD = False
	for each Stuff in JPDMDAll: Stuff.visible = true: next  'If VRroom is on (Some users DO have FS set on their VR cans), we want to force JP's Flasher DMD
	Else
	UseFlexDMD = true ' Use FlexDMD in FS mode for cabinets
		digitgrid.Opacity=60
		Flasher6.Visible=0
		Flasher11.Visible=0
		Flasher6b.Visible=1
		Flasher11b.Visible=1
	end If
End If


If VRRoom =0 then 
for each Stuff in VRShip: Stuff.visible = False: Next  'make VR ship stuff nonvisible
for each Stuff in VRRoomCOL: Stuff.visible = False: next  'make room stuff nonvisible
for each Stuff in VRClock: Stuff.visible = False: next 'make clock stuff nonvisible

ClockTimer.enabled = false
NewClockTimer.enabled = false
BeerTimer.enabled = false
end if

' Load VRRoom
If VRroom >0 Then
TimerPlunger2.enabled = true 
if GlassScratchesOn = 1 then GlassImpurities.visible = true

'move the DMD into place..
for each Stuff in JPDMDAll: Stuff.x = Stuff.x +1118: next
for each Stuff in JPDMDAll: Stuff.y = Stuff.y -722: next
for each Stuff in JPDMDAll: Stuff.height = Stuff.height +315: next
for each Stuff in JPDMDAll: Stuff.rotx = 274: next

for each Stuff in VRDMDTop: Stuff.y = Stuff.y +22: next
for each Stuff in VRDMDBottom: Stuff.y = Stuff.y -20: next
'DMD done..

for each Stuff in VRCab: Stuff.visible = true: next

'make desktop sideblades and rails invisible..  why so many?
'Wall35.visible = false 
rrail.visible = false 
lrail.visible = false
SideBladeRight2.Sidevisible = false  
LeftSideBlade.Sidevisible = false 
Flasher11.visible = false 
Flasher6.visible = false 
Flasher11b.visible = false 
Flasher6b.visible = false
Ramp007.visible = false 
Ramp003.visible = false 

'VR_Backbox_Backglass.blenddisablelighting = 4


If VRRoom = 3 then 
for each Stuff in VRShip: Stuff.visible = true: Next  'make VR ship stuff visible
for each Stuff in VRRoomCOL: Stuff.visible = False: next  'make room stuff nonvisible
for each Stuff in VRClock: Stuff.visible = False: next 'make clock stuff nonvisible
end if

If VRRoom = 2 then 
for each Stuff in VRShip: Stuff.visible = False: Next  'make VR ship stuff nonvisible
for each Stuff in VRRoomCOL: Stuff.visible = false: next  'make room stuff visible
for each Stuff in VRClock: Stuff.visible = true: next 'make clock stuff visible
ClockTimer.enabled = False
NewClockTimer.enabled = False
BeerTimer.enabled = False
end if


If VRRoom = 1 then 
for each Stuff in VRShip: Stuff.visible = False: Next  'make VR ship stuff nonvisible
for each Stuff in VRRoomCOL: Stuff.visible = true: next  'make room stuff visible
for each Stuff in VRClock: Stuff.visible = true: next 'make clock stuff visible
ClockTimer.enabled = true
NewClockTimer.enabled = true
BeerTimer.enabled = true
end if
end if

' End VR Init.....
'*****************************************************************************************************************************
Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1

' Load the core.vbs for supporting Subs and functions
LoadCoreFiles

Sub LoadCoreFiles
	On Error Resume Next
	ExecuteGlobal GetTextFile("core.vbs")
	If Err Then MsgBox "Can't open core.vbs"
	ExecuteGlobal GetTextFile("controller.vbs")
	If Err Then MsgBox "Can't open controller.vbs"
	On Error Goto 0
End Sub

Const cGameName = "PiratesLife2.0"
Const TableName = "PiratesLife2.0"
Const myVersion = "PiratesLife2.0.1"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 5  ' limit to 5x in this game, both bonus multiplier and playfield multiplier
Dim BallsPerGame: BallsPerGame = 3   ' usually 3 or 5
Const MaxMultiballs = 5  ' max number of balls during multiball
Const tnob = 9
Const lob = 2
Const RubberizerEnabled = 1
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7
Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

'**************************
'Variables
'**************************

Dim ballrolleron
Dim turnonultradmd
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim NewSong
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim BonusMultiplierActive(4)
Dim BonusCounter(4)
Dim PlayfieldMultiplier(4)
Dim TreasureKickerCount(4)
Dim ShipNow(4)
Dim ShipSink(4)
Dim ShipSunkCount(4)
Dim ShipBonusCount(4)
Dim DirtyCreatureComplete(4)
Dim SharkComplete(4)
Dim TavernComplete(4)
Dim WildSeasComplete (4)
Dim PollyComplete(4)
Dim RaiseTheSailsComplete(4)
Dim TreasureComplete(4)
Dim WalkThePlankComplete(4)
Dim PlankCount(4)
Dim PlankAward1Active(4)
Dim PlankAward2Active(4)
Dim PlankAward3Active(4)
Dim Ship1Sunk(4)
Dim Ship2Sunk(4)
Dim Ship3Sunk(4)
Dim Ship4Sunk(4)
Dim Ship5Sunk(4)
Dim Ship6Sunk(4)
Dim Ship7Sunk(4)
Dim Ship8Sunk(4)
Dim Ship9Sunk(4)
Dim Ship1Now (4)
Dim Ship2Now (4)
Dim Ship3Now (4)
Dim Ship4Now (4)
Dim Ship5Now (4)
Dim Ship6Now (4)
Dim Ship7Now (4)
Dim Ship8Now (4)
Dim Ship9Now (4)
Dim ShipsComplete(4)
Dim Ship1Visible (4)
Dim Ship2Visible (4)
Dim Ship3Visible (4)
Dim Ship4Visible (4)
Dim Ship5Visible (4)
Dim Ship6Visible (4)
Dim Ship7Visible (4)
Dim Ship8Visible (4)
Dim Ship9Visible (4)
Dim ScurvyDickActive (4)
Dim ShipsSunk(4)

Dim ShipCount(4)

Dim ModesCompleted(4)
Dim TreasureKickerAwardCount(4)
Dim TreasureChestCompleted (4)
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot
Dim Jackpot1
Dim Jackpot2
Dim SuperJackpot
Dim SuperJackpot1
Dim SuperJackpot2
Dim SuperJackpot3
Dim Tilt
Dim MechTilt
Dim TiltSensitivity
Dim Tilted
Dim bMechTiltJustHit
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue(4)
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode
Dim bFlippersEnabled
Dim cFlippersEnabled
Dim cFlipperPressed
Dim AwardSelection
Dim SelectAward
'define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInHole
Dim BallsInLock(4)

'Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bGameEnded(4)
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim ReadyShip1
Dim ReadyShip2
Dim ReadyShip3
Dim ReadyShip4
Dim ReadyShip5
Dim ReadyShip6
Dim ReadyShip7
Dim ReadyShip8
' core.vbs variables
Dim plungerIM 
Dim cbRight
Dim cbLock1
Dim cbLock2
Dim cbLock3

Dim objShell


Set mMagnaSave1 = New cvpmMagnet : With mMagnaSave1
	.InitMagnet magna1, 10
End With


Sub magna1_Hit():mMagnaSave1.AddBall ActiveBall: End Sub
Sub magna1_UnHit(): mMagnaSave1.RemoveBall ActiveBall: End Sub

Sub MagnetTrigger_hit()
 	mMagnaSave1.MagnetOn = 1
	magnettimer001.enabled=1
	BarbaBlancaReward
End Sub

Sub magnettimer001_timer()
	debug.print "magnet disabled"
	magnettimer001.enabled=0
	mMagnaSave1.MagnetOn = 0
	MagnetTrigger.Enabled=0
	magna1.Enabled=0
	CalloutTimer.Enabled=False:CalloutActive=False
	LightBarbaBlanca.State=0
	BarbaBlancaStopTimer.Enabled=False
End Sub

Dim BarbaBlancaActive
Sub StartBarbaBlanca
	BarbaBlancaActive=1
	BarbaBlancaStopTimer.Enabled=True
 	magna1.enabled = 1
	MagnetTrigger.Enabled=1
	LightBarbaBlanca.State=2
debug.print "MagnetTriggerEnabled-MagnetOn"
End Sub

Sub BarbaBlancaStop
	BarbaBlancaActive=0
	magnettimer001.enabled=0
	mMagnaSave1.MagnetOn = 0
	MagnetTrigger.Enabled=False
	magna1.Enabled=0
	LightBarbaBlanca.State=0
	BarbaBlancaStopTimer.Enabled=False
	CalloutTimer.Enabled=False:CalloutActive=False
End Sub

Sub BarbaBlancaStopTimer_Timer
	BarbaBlancaStop
End Sub

Dim BBCount(4)
Sub BarbaBlancaReward
	BBCount(CurrentPlayer)=BBCount(CurrentPlayer)+1
Select Case BBCount(CurrentPlayer)
	Case 1:		If CalloutActive=False Then CalloutTimer.Enabled=True:CalloutActive=true:PlaySound "CO_BarbaBlankaFearNotWet2":End If
				DMD CL(0, "  BARBA BLANCO "), CL(1, "250000 "), "", eNone, eBlink, eNone, 4000, True, "":AddScore 250000
				
	Case 2:		If CalloutActive=False Then CalloutTimer.Enabled=True:CalloutActive=true:PlaySound  "CO_BarbaBlankaDenaraWet2":End If
				DMD CL(0, "  BARBA BLANCO "), CL(1, "500000 "), "", eNone, eBlink, eNone, 4000, True, "":AddScore 500000
				If bMultiBallMode=False Then vpmtimer.addtimer 3500, "BarbaBlancaMultiball'": End If

	Case 3:		If CalloutActive=False Then CalloutTimer.Enabled=True:CalloutActive=true:PlaySound "CO_BarbaBlankaFearNotWet2":End If
				DMD CL(0, "  BARBA BLANCO "), CL(1, "750000 "), "", eNone, eBlink, eNone, 4000, True, "":AddScore 750000

	Case 4:		If CalloutActive=False Then CalloutTimer.Enabled=True:CalloutActive=true:PlaySound  "CO_BarbaBlankaDenaraWet2":End If
				DMD CL(0, "  BARBA BLANCO "), CL(1, "1000000 "), "", eNone, eBlink, eNone, 4000, True, "":AddScore 1000000
				If bMultiBallMode=False Then vpmtimer.addtimer 3500, "BarbaBlancaMultiball'": End If

	Case 5:		If CalloutActive=False Then CalloutTimer.Enabled=True:CalloutActive=true:PlaySound "CO_BarbaBlankaFearNotWet2":End If
				DMD CL(0, "  BARBA BLANCO "), CL(1, "1250000 "), "", eNone, eBlink, eNone, 4000, True, "":AddScore 1250000

	Case 6:		If CalloutActive=False Then CalloutTimer.Enabled=True:CalloutActive=true:PlaySound  "CO_BarbaBlankaDenaraWet2":End If
				DMD CL(0, "  BARBA BLANCO "), CL(1, "1500000 "), "", eNone, eBlink, eNone, 4000, True, "":AddScore 1500000
				If bMultiBallMode=False Then vpmtimer.addtimer 3500, "BarbaBlancaMultiball'": End If

	Case 7:	 	If CalloutActive=False Then CalloutTimer.Enabled=True:CalloutActive=true:PlaySound "CO_BarbaBlankaFearNotWet2":End If
				DMD CL(0, "  BARBA BLANCO "), CL(1, "1750000 "), "", eNone, eBlink, eNone, 4000, True, "":AddScore 1750000

	Case 8:	  	If CalloutActive=False Then CalloutTimer.Enabled=True:CalloutActive=true:PlaySound  "CO_BarbaBlankaDenaraWet2":End If
				DMD CL(0, "  BARBA BLANCO "), CL(1, "2000000 "), "", eNone, eBlink, eNone, 4000, True, "":AddScore 2000000:BBCount(CurrentPlayer)=0
				If bMultiBallMode=False Then vpmtimer.addtimer 3500, "BarbaBlancaMultiball'": End If

End Select
End Sub

Dim BarbaBlancaMultiballActive
Sub BarbaBlancaMultiball
		BarbaBlancaMultiballActive=1
		AddMultiball (1)
		EnableBallSaver (20)
		DMD CL(0, "  BARBA BLANCO "), CL(1, "MULTIBALL"), "", eNone, eBlink, eNone, 4000, True, ""
End Sub

' *********************************************************************
'                Visual Pinball Defined Script Events
' ********************************************************************* 

Sub Table1_Init()
	LoadLUT
	'		resetbackglass
	LoadEM
	Dim i
	Randomize

	'AutoPlunger 

	Const IMPowerSetting = 10 ' Plunger Power
	Const IMTime = 1.1        ' Time in seconds for Full Plunge
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP swPlunger, IMPowerSetting, IMTime
		.Random 1.5
		.InitExitSnd SoundFX("Popper", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
		.CreateEvents "plungerIM"
	End With



	' Misc. VP table objects Initialisation, droptargets, animations...
	VPObjects_Init

	' load saved values, highscore, names, jackpot
	Loadhs

	' Initalise the DMD display
	DMD_Init

	' freeplay or coins
	bFreePlay = True 'we want coins
	if bFreePlay Then DOF 125, DOFOn

	'		Loadhs
	bAttractMode = False
	bOnTheFirstBall = False
	bBallInPlungerLane = False
	bBallSaverActive = False
	bBallSaverReady = False
	bMultiBallMode = False
	bGameInPlay = False
	bAutoPlunger = False
	bMusicOn = True
	BallsOnPlayfield = 0
	BallsInHole = 0
	BallsInLock(1) = 0
	BallsInLock(2) = 0
	ShipSunkCount(CurrentPlayer)=0
Ship1Now(CurrentPlayer)=0
Ship2Now(CurrentPlayer)=0
Ship3Now(CurrentPlayer)=0
Ship4Now(CurrentPlayer)=0
Ship5Now(CurrentPlayer)=0
Ship6Now(CurrentPlayer)=0
Ship7Now(CurrentPlayer)=0
Ship8Now(CurrentPlayer)=0
Ship9Now(CurrentPlayer)=0
ShipsComplete(CurrentPlayer)=0
	LastSwitchHit = ""
	Tilt = 0
	MechTilt = 0
	en1.Visible=False
	TiltSensitivity = 6
	Tilted = False
	bBonusHeld = False
	bJustStarted = True
	cFlipperPressed=False
	GiOff
	DMDFlush
	ResetATCTargets
	StartAttractMode
	PlungerDropWall.IsDropped=True
    Shark1AnimTimer.Enabled = 0:Shark1AnimStopTimer.Enabled = 1:Shark1ShakeTimer.Enabled=0:Shark1LocationTimer.Enabled=0
	Skeleton1Wall.IsDropped=True:Skeleton2Wall.IsDropped=True:Skeleton3Wall.IsDropped=True:Skeleton4Wall.IsDropped=True
	Skeleton1Wallb.IsDropped=True:Skeleton2Wallb.IsDropped=True:Skeleton3Wallb.IsDropped=True:Skeleton4Wallb.IsDropped=True
	CanonGuideRight.IsDropped=True:CanonGuideLeft.IsDropped=True
	ShipPickles.Visible=True: ApronOverlayStarting.Visible=True
	WallLiftRampBlocker1.isdropped=True
	WallLiftRampBlocker2.isdropped=True
	WallLiftRampBlocker3.isdropped=True
	RampDropWall.IsDropped=False
	DropDirtyCreatureWalltargets
End Sub

'**************************************
'Object Initiation   See Serious Sam
'**************************************
' droptargets, animations, etc

Sub VPObjects_Init 'init objects
Dim x
    TurnOffPlayfieldLights()
    For each x in ent1: x.Isdropped = 1: next
	en1.Visible = 1
End Sub
'********************
' MATHS
'********************

Function RndNum(min,max)
	RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min AND max
End Function



'//////////////////////////////////////////////////////////////////////
'// LUT
'//////////////////////////////////////////////////////////////////////

Sub SetLUT  'AXS
	Table1.ColorGradeImage = "LUT" & LUTset
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
	Select Case LUTSet
		Case 0: LUTBox.text = "VPW original 1on1"
		Case 1: LUTBox.text = "Fleep Natural Dark 1"
		Case 2: LUTBox.text = "Fleep Natural Dark 2"
		Case 3: LUTBox.text = "Fleep Warm Dark"
		Case 4: LUTBox.text = "Fleep Warm Bright"
		Case 5: LUTBox.text = "Fleep Warm Vivid Soft"
		Case 6: LUTBox.text = "Fleep Warm Vivid Hard"
		Case 7: LUTBox.text = "Skitso Natural and Balanced"
		Case 8: LUTBox.text = "Skitso Natural High Contrast"
		Case 9: LUTBox.text = "3rdaxis Referenced THX Standard"
		Case 10: LUTBox.text = "CalleV Punchy Brightness and Contrast"
		Case 11: LUTBox.text = "HauntFreaks Desaturated"
		Case 12: LUTBox.text = "Tomate washed out"
		Case 13: LUTBox.text = "bassgeige"
		Case 14: LUTBox.text = "blacklight"
		Case 15: LUTBox.text = "B&W Comic Book"
		Case 16: LUTBox.text = "Skitso New ColorLut"
	End Select
	LUTBox.TimerEnabled = 1
End Sub

Sub SaveLUT
	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if

	if LUTset = "" then LUTset = 0 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "TimeLordLUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub
Sub LoadLUT
	bLutActive = False
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=0
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "TimeLordLUT.txt") then
		LUTset=0
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "TimeLordLUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
	If (TextStr.AtEndOfStream=True) then
		Exit Sub
	End if
	rLine = TextStr.ReadLine
	If rLine = "" then
		LUTset=0
		Exit Sub
	End if
	LUTset = int (rLine) 
	Set ScoreFile = Nothing
	Set FileObj = Nothing
End Sub

'**************************
'   KEYS
'**************************


Sub Table1_KeyDown(ByVal Keycode)
'added by jpsalas
If NOT bGameInPlay Then
    if keycode = RightFlipperKey Then BallSelection = (BallSelection + 1) MOD 4: UpdateBallSelection
    if keycode = LeftFlipperKey Then
       BallSelection = BallSelection - 1
       If BallSelection = -1 then BallSelection = 3
       UpdateBallSelection
    End If
End If


If  bGameInPlay and cFlippersEnabled=True Then
    If keycode = RightFlipperKey Then
		cFlipperPressed=True
		debug.print "cFlipperPressed"
		AwardSelection=(AwardSelection+1) MOD 8
		UpdateAwardSelection
      If AwardSelection = 5 then AwardSelection = -1
	End If
    If keycode = LeftFlipperKey Then
		cFlipperPressed=True
		debug.print "cFlipperPressed=True"
       AwardSelection = (AwardSelection - 1)
       If AwardSelection = -1 then AwardSelection = 4
       UpdateAwardSelection
    End If
End If


	'LUT controls & Canon control
     If CanonActive Then If keycode = PlungerKey Then CanonL1Fire
     If CanonActive Then If keycode = LockBarKey Then CanonL1Fire
	If keycode = LeftMagnaSave Then bLutActive = True
	If keycode = RightMagnaSave Then
		If bLutActive Then
			if DisableLUTSelector = 0 then
				LUTSet = LUTSet  - 1
				if LutSet < 0 then LUTSet = 16
				SetLUT
				ShowLUT
			End If
		End If
	End If

If  bGameInPlay and cFlippersEnabled=True Then
	If keycode = RightMagnaSave and cFlipperPressed=True Then SelectAwardTimer.Enabled=True:cFlipperPressed=False
	If keycode = RightMagnaSave and cFlipperPressed=False Then  DMD CL(0, "FLIPPERS FIRST" ), CL(1, "BOOFHEAD"), "", eNone, eNone, eNone, 3000, True, ""
End If


	If Keycode = AddCreditKey Or Keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
		If Credits < 15 Then: Credits = Credits + 1
		if bFreePlay = False Then DOF 125, DOFOn
		If(Tilted = False) Then
			DMDFlush
			DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True,""

			If NOT bGameInPlay Then ShowTableInfo
		End If
	End If 

	If keycode = PlungerKey Then Plunger.PullBack:SoundPlungerPull()

	If bGameInPlay Then
			If keycode = LeftTiltKey Then Nudge 90, 2.5:SoundNudgeLeft():CheckTilt
			If keycode = RightTiltKey Then Nudge 270, 2.5:SoundNudgeRight():CheckTilt
			If keycode = CenterTiltKey Then Nudge 0, 1.5:SoundNudgeCenter():CheckTilt
			If keycode = MechanicalTilt Then SoundNudgeCenter:CheckMechTilt
			'********************************************************************************************************************
			If keycode = LeftFlipperKey and bFlippersEnabled Then FlipperActivate LeftFlipper, LFPress:InstantInfoTimer.Enabled = True:SolLFlipper 1
			If keycode = RightFlipperKey and bFlippersEnabled Then FlipperActivate RightFlipper, RFPress:InstantInfoTimer.Enabled = True:SolRFlipper 1:RightFlipper1.rotatetoend
			'**********************************************************************************************************************
			If hsbModeActive Then
				EnterHighScoreKey(keycode)
				Exit Sub
			End If

			If keycode = StartGameKey Then
				If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

					If(bFreePlay = True)Then
						PlayersPlayingGame = PlayersPlayingGame + 1
						TotalGamesPlayed = TotalGamesPlayed + 1

						DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eNone, eNone, 500, True, "fx_fanfare1"

					Else
						If(Credits > 0)then
							PlayersPlayingGame = PlayersPlayingGame + 1
							TotalGamesPlayed = TotalGamesPlayed + 1
							Credits = Credits - 1
							DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eNone, eNone, 500, True, "fx_fanfare2"

						Else
							' Not Enough Credits to start a game.
							DOF 140, DOFOff
							DMD CL(0, "     CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, ""

						End If
					End If
				End If
			End If
	Else ' If (GameInPlay)

	If keycode = LeftFlipperKey Then DMDFlush:ShowTableInfo
	If keycode = RightFlipperKey Then DMDFlush:ShowTableInfo

		If keycode = StartGameKey Then
			If(bFreePlay = True)Then
				If(BallsOnPlayfield = 0)Then
					ResetForNewGame()

				End If
			Else
				If(Credits > 0)Then
					If(BallsOnPlayfield = 0)Then
						Credits = Credits - 1
						ResetForNewGame()

					End If
				Else
					' Not Enough Credits to start a game.
					DOF 140, DOFOff
					DMDFlush
					DMD CL(0, "    CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, ""

				End If
			End If
		End If

	End If ' If (GameInPlay)
	if keycode = "3" then Skeleton1MoveUp: Skeleton2MoveUp: Skeleton3MoveUp: Skeleton4MoveUp
End Sub



Sub PlayersCall
	DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eNone, eNone, 500, True, "fx_fanfare2"
End Sub

'added by jpsalas
Dim BallSelection: BallSelection = 0


Sub UpdateBallSelection
DMDFlush
Dim Hard
Select Case BallSelection
    Case 0: 'Easy3
           DMD CL(0, "    EASY MODE " ), CL(1, "3 BALLS"), "", eNone, eNone, eNone, 1500, True, ""
           SelectDifficutly 1
			Hard=0
           DMDEasyMode
		   PlaySound "CO_Easy3"
'		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_Easy3": End If
		   PlaySound "CO_Easy3"
           FlasherHardSelect5.Visible=0
           FlasherHardSelect3.Visible=0
           FlasherEasySelect5.Visible=0
           FlasherEasySelect3.Visible=1
           BallsPerGame = 3
    Case 1: 'Easy5
           DMD CL(0, "    EASY MODE " ), CL(1, "5 BALLS"), "", eNone, eNone, eNone, 1500, True, ""
           SelectDifficutly 1
			Hard=0
           DMDEasyMode
		    PlaySound "CO_Easy5"
'		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_Easy5": End If
           FlasherHardSelect5.Visible=0
           FlasherHardSelect3.Visible=0
           FlasherEasySelect5.Visible=1
           FlasherEasySelect3.Visible=0
           BallsPerGame = 5

    Case 2: 'Hard3
           DMD CL(0, "    HARD MODE " ), CL(1, "3 BALLS"), "", eNone, eNone, eNone, 1500, True, ""
           SelectDifficutly 2
			Hard=1
           DMDHardMode
			PlaySound "CO_Hard3"
'		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_Hard3": End If
           FlasherHardSelect5.Visible=0
           FlasherHardSelect3.Visible=1
           FlasherEasySelect5.Visible=0
           FlasherEasySelect3.Visible=0
           BallsPerGame = 3

    Case 3: 'Hard5
           DMD CL(0, "    HARD MODE " ), CL(1, "5 BALLS"), "", eNone, eNone, eNone, 1500, True, ""
           SelectDifficutly 2
			Hard=1
           DMDHardMode
			PlaySound "CO_Hard5"
'		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_Hard5": End If
		   FlasherHardSelect5.Visible=1
           FlasherHardSelect3.Visible=0
           FlasherEasySelect5.Visible=0
           FlasherEasySelect3.Visible=0
           BallsPerGame = 5
End Select
ShowTableInfo
End Sub

Dim EasyMode
Sub DMDEasyMode
	EasyMode=True
End Sub

Dim HardMode
Sub DMDHardMode
	HardMode=True
End Sub
 
'********************************************************************************************************

Sub Table1_KeyUp(ByVal keycode)

	If keycode = LeftMagnaSave Then bLutActive = False

	If KeyCode = PlungerKey Then Plunger.Fire : SoundPlungerReleaseBall()     

	If hsbModeActive Then
		InstantInfoTimer.Enabled = False
		bInstantInfo = False
		Exit Sub
	End If

	' Table specific

	If bGameInPLay AND NOT Tilted Then
		If keycode = LeftFlipperKey Then
			FlipperDeActivate LeftFlipper, LFPress
			SolLFlipper 0
			InstantInfoTimer.Enabled = False
			If bInstantInfo Then
				bInstantInfo = False
				DMDScoreNow
			End If
		End If
		If keycode = RightFlipperKey Then
			FlipperDeActivate RightFlipper, RFPress
			RightFlipper1.rotatetostart			
			SolRFlipper 0
			InstantInfoTimer.Enabled = False
			If bInstantInfo Then
				bInstantInfo = False
				DMDScoreNow
			End If
		End If
	End If

	If  Tilted Then
		If keycode = LeftFlipperKey Then
			FlipperDeActivate LeftFlipper, LFPress
			SolLFlipper 0
		End If
		If keycode = RightFlipperKey Then
			FlipperDeActivate RightFlipper, RFPress
			RightFlipper1.rotatetostart
			SolRFlipper 0
		End If
	End If
	if keycode = "3" then Skeleton1MoveDown: Skeleton2MoveDown: Skeleton3MoveDown: Skeleton4MoveDown
End Sub

Sub InstantInfo
	DMD CL(0, "INSTANT INFO"), "", "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "SHIPS SUNK"), CL(1, ShipsSunk(CurrentPlayer) ), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "SPINNER VALUE"), CL(1, ModesCompleted(CurrentPlayer) ), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "BUMPER VALUE"), CL(1, BonusMultiplier(CurrentPlayer) ), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "HIGHEST SCORE"), CL(1, HighScoreName(0) & " " & HighScore(0) ), "", eNone, eNone, eNone, 800, False, ""
End Sub

Sub EndFlipperStatus
	If bInstantInfo Then
		bInstantInfo = False
'		DMDScoreNow
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
	SaveLUT
	Savehs
	If UseFlexDMD Then FlexDMD.Run = False
	If B2SOn = true Then Controller.Stop
End Sub



'//////////////////////////////////////////////////////////////////////
'// FLIPPERS 
'//////////////////////////////////////////////////////////////////////

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
		DOF  101, DOFOn

		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		DOF  101, DOFOff
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend

		DOF  102, DOFOn
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		DOF  102, DOFOff
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub


' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

Sub RightFlipper1_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
	FlipperR1Sh.RotZ = RightFlipper1.CurrentAngle
End Sub

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
dim RF1 : Set RF1 = New FlipperPolarity
InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 80
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -2.7        
'        AddPt "Polarity", 2, 0.33, -2.7
'        AddPt "Polarity", 3, 0.37, -2.7        
'        AddPt "Polarity", 4, 0.41, -2.7
'        AddPt "Polarity", 5, 0.45, -2.7
'        AddPt "Polarity", 6, 0.576,-2.7
'        AddPt "Polarity", 7, 0.66, -1.8
'        AddPt "Polarity", 8, 0.743, -0.5
'        AddPt "Polarity", 9, 0.81, -0.5
'        AddPt "Polarity", 10, 0.88, 0
'
'        addpt "Velocity", 0, 0,         1
'        addpt "Velocity", 1, 0.16, 1.06
'        addpt "Velocity", 2, 0.41,         1.05
'        addpt "Velocity", 3, 0.53,         1'0.982
'        addpt "Velocity", 4, 0.702, 0.968
'        addpt "Velocity", 5, 0.95,  0.968
'        addpt "Velocity", 6, 1.03,         0.945
'
'        LF.Object = LeftFlipper        
'        LF.EndPoint = EndPointLp
'        RF.Object = RightFlipper
'        RF.EndPoint = EndPointRp
'End Sub
'
'
'
''*******************************************
'' Mid 80's
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 80
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -3.7        
'        AddPt "Polarity", 2, 0.33, -3.7
'        AddPt "Polarity", 3, 0.37, -3.7
'        AddPt "Polarity", 4, 0.41, -3.7
'        AddPt "Polarity", 5, 0.45, -3.7 
'        AddPt "Polarity", 6, 0.576,-3.7
'        AddPt "Polarity", 7, 0.66, -2.3
'        AddPt "Polarity", 8, 0.743, -1.5
'        AddPt "Polarity", 9, 0.81, -1
'        AddPt "Polarity", 10, 0.88, 0
'
'        addpt "Velocity", 0, 0,         1
'        addpt "Velocity", 1, 0.16, 1.06
'        addpt "Velocity", 2, 0.41,         1.05
'        addpt "Velocity", 3, 0.53,         1'0.982
'        addpt "Velocity", 4, 0.702, 0.968
'        addpt "Velocity", 5, 0.95,  0.968
'        addpt "Velocity", 6, 1.03,         0.945
'
'        LF.Object = LeftFlipper        
'        LF.EndPoint = EndPointLp
'        RF.Object = RightFlipper
'        RF.EndPoint = EndPointRp
'End Sub
'
'


'*******************************************
'  Late 80's early 90's

'Sub InitPolarity()
'	dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'	Next
'
'	AddPt "Polarity", 0, 0, 0
'	AddPt "Polarity", 1, 0.05, -5
'	AddPt "Polarity", 2, 0.4, -5
'	AddPt "Polarity", 3, 0.6, -4.5
'	AddPt "Polarity", 4, 0.65, -4.0
'	AddPt "Polarity", 5, 0.7, -3.5
'	AddPt "Polarity", 6, 0.75, -3.0
'	AddPt "Polarity", 7, 0.8, -2.5
'	AddPt "Polarity", 8, 0.85, -2.0
'	AddPt "Polarity", 9, 0.9,-1.5
'	AddPt "Polarity", 10, 0.95, -1.0
'	AddPt "Polarity", 11, 1, -0.5
'	AddPt "Polarity", 12, 1.1, 0
'	AddPt "Polarity", 13, 1.3, 0
'
'	addpt "Velocity", 0, 0,         1
'	addpt "Velocity", 1, 0.16, 1.06
'	addpt "Velocity", 2, 0.41,         1.05
'	addpt "Velocity", 3, 0.53,         1'0.982
'	addpt "Velocity", 4, 0.702, 0.968
'	addpt "Velocity", 5, 0.95,  0.968
'	addpt "Velocity", 6, 1.03,         0.945
'
'	LF.Object = LeftFlipper        
'	LF.EndPoint = EndPointLp
'	RF.Object = RightFlipper
'	RF.EndPoint = EndPointRp
'End Sub



'
''*******************************************
'' Early 90's and after
'
Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 20
	Next

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -5.5
	AddPt "Polarity", 2, 0.4, -5.5
	AddPt "Polarity", 3, 0.6, -5.0
	AddPt "Polarity", 4, 0.65, -4.5
	AddPt "Polarity", 5, 0.7, -4.0
	AddPt "Polarity", 6, 0.75, -3.5
	AddPt "Polarity", 7, 0.8, -3.0
	AddPt "Polarity", 8, 0.85, -2.5
	AddPt "Polarity", 9, 0.9,-2.0
	AddPt "Polarity", 10, 0.95, -1.5
	AddPt "Polarity", 11, 1, -1.0
	AddPt "Polarity", 12, 1.05, -0.5
	AddPt "Polarity", 13, 1.1, 0
	AddPt "Polarity", 14, 1.3, 0

	addpt "Velocity", 0, 0,         1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41,         1.05
	addpt "Velocity", 3, 0.53,         1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03,         0.945

	LF.Object = LeftFlipper        
	LF.EndPoint = EndPointLp
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub


' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub




'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)

	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub

	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property

	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
				case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub

	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
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
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next                
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
		if IsObject(a(x)) then 
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
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function


'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState

	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If        
End Function


'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress,RF1Press, LFCount, RFCount
dim LFState, RFState, RF1State
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle,LFEndAngle

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
Const EOSReturn = 0.025  'mid 90's and later

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
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST        
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If

	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	Else
		If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub


'************************************************************************
'   TILT
'************************************************************************


'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

'Sub CheckTilt                                    'Called when table is nudged
'	If NOT bGameInPlay Then Exit Sub
'	Tilt = Tilt + TiltSensitivity                'Add to tilt count
'	TiltDecreaseTimer.Enabled = True
'	If(Tilt > TiltSensitivity)AND(Tilt < 15)Then 'show a warning
'		DMD "_", CL(1, "CAREFUL!"), "", eNone, eBlinkFast, eNone, 500, True, ""
'	End if
'	If Tilt > 15 Then 'If more that 15 then TILT the table
'		Tilted = True
'		'display Tilt
'		DMDFlush
'		DMD "", CL(1, "TILT"), "", eNone, eBlink, eNone, 100, False, ""
'		DMD CL(0, "TILT"), "", "", eNone, eBlink, eNone, 100, False, ""
'		DisableTable True
'		TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
'	End If
'End Sub

Sub CheckTilt                                    'Called when table is nudged
	If NOT bGameInPlay Then Exit Sub
	Tilt = Tilt + TiltSensitivity                'Add to tilt count
	TiltDecreaseTimer.Enabled = True
	If(Tilt > TiltSensitivity) AND (Tilt <= 15) Then ShowTiltWarning  'show a warning
	If Tilt > 15 Then TiltMachine                'If more that 15 then TILT the table
End Sub

Sub CheckMechTilt                                	'Called when mechanical tilt bob switch closed
	If Not bGameInPlay Then Exit Sub
	If Not bMechTiltJustHit Then
		MechTilt = MechTilt + 1               		'Add to tilt count
		If(MechTilt > 0) AND (MechTilt <= 2) Then ShowTiltWarning 'show a warning
		If MechTilt > 2 Then TiltMachine  			'If more than 2 then TILT the table
		bMechTiltJustHit = True
		TiltDebounceTimer.Enabled = True
	End If
End Sub

Sub ShowTiltWarning
	PlaySound "danger"
	DMD " WARNING", "TILT DANGER", "", eBlink, eBlink, eNone, 2500, True, "" 'Light 
End Sub

Sub TiltMachine
	Tilted = True
	'display Tilt
	PlaySound "danger"
	DMD "   TILT", "YA BOOFHEAD", "", eNone, eNone, eNone, 2500, False, "" 'Light 
	DisableTable True
	TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
End Sub

Sub TiltDecreaseTimer_Timer
	' DecreaseTilt
	If Tilt > 0 Then
		Tilt = Tilt - 0.1
	Else
		TiltDecreaseTimer.Enabled = False
	End If
End Sub

Sub TiltDebounceTimer_Timer
	bMechTiltJustHit = False
	TiltDebounceTimer.Enabled = False
End Sub


Sub DisableTable(Enabled)
	If Enabled Then
		'turn off GI and turn off all the lights
		GiOff
		PlaySoundAt "Relay_GI_Off" , GISound
		LightSeqTilt.Play SeqAllOff
		'Disable slings, bumpers etc
		LeftFlipper.RotateToStart
		RightFlipper.RotateToStart
'		Bumper1.Force = 0
		Bumper2.Force = 0
		Bumper3.Force = 0
		LeftSlingshot.Disabled = 1
		RightSlingshot.Disabled = 1
		bFlippersEnabled = False
	Else
		PlaySoundAt "Relay_GI_On" , GISound
		'turn back on GI and the lights
		'GiOn
		LightSeqTilt.StopPlay
'		Bumper1.Force = 7
		Bumper2.Force = 7
		Bumper3.Force = 7
		LeftSlingshot.Disabled = 0
		RightSlingshot.Disabled = 0
		bFlippersEnabled = True
		'clean up the buffer display
		DMDFlush
	End If
End Sub

Sub TiltRecoveryTimer_Timer()
	' if all the balls have been drained then..
	If(BallsOnPlayfield = 0)Then
		' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
		EndOfBall()
		TiltRecoveryTimer.Enabled = False
	End If
	' else retry (checks again in another second or so)
End Sub


Dim Song
Song = ""

Sub PlaySong(name)
	If bMusicOn Then
		If Song <> name Then
			StopSound Song
			Song = name
			If Song = "m_end" Then
				PlaySound Song, 0, SongVolume  'this last number is the volume, from 0 to 1
			Else
				PlaySound Song, -1, SongVolume 'this last number is the volume, from 0 to 1
			End If
		End If
	End If
End Sub


Sub ChangeSong
'	If(BallsOnPlayfield > 0) Then
debug.print "Change Song Sub Active"
		NewSong=NewSong +1
		Select Case NewSong 
			Case 1 PlaySong "m_HymnOfTheHighSeasPart2-7dB":			Debug.Print "Case 1 m_HymnOfTheHighSeasPart2-7dB"
			Case 2 PlaySong "m_HymnOfTheHighSeas-7dB":	NewSong=0:	Debug.Print "Case 2 m_HymnOfTheHighSeas"
		End Select
'	End If
End Sub

'================================
'Helper Functions

Function NullFunctionZ(aEnabled):End Function	'1 argument null function placeholder	 TODO move me or replac eme

'*******************************************************
'   START GAME, END GAME- User Defined Script Events
'********************************************************

Sub ResetForNewGame()
	Dim i
	bGameInPLay = True
	'resets the score display, and turn off attract mode
	StopAttractMode
	GiOn
	TotalGamesPlayed = TotalGamesPlayed + 1
	CurrentPlayer = 1
	PlayersPlayingGame = 1
	bOnTheFirstBall = True
	For i = 1 To MaxPlayers
		Score(i) = 0
		BonusPoints(i) = 0
		BonusHeldPoints(i) = 0
		BonusMultiplier(i) = 1
		BonusMultiplierActive(i)=0
		BonusCounter(i)= 0
		PlayfieldMultiplier(i) = 1
'		ShipNow(i)=0
		BallsRemaining(i) = BallsPerGame
		ExtraBallsAwards(i) = 0
		TreasureKickerCount(i)=0
		ShipSunkCount(i)=0
	Next
	' initialise any other flags
	Tilt = 0
	' initialise Game variables
	Game_Init()
	PlaySoundAt "ball_trough", lane4
	' you may wish to start some music, play a sound, do whatever at this point
    vpmtimer.addtimer 1500, "FirstBall '"
End Sub





'***********FIRSTBALL
'FirstBall
'*************************************************

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with
Dim FirstBallStarted ' For DMD Tracking
Sub FirstBall
	FirstBallStarted=True
	' create a new ball in the shooters lane
	vpmtimer.addtimer 1500, " CreateNewBall'"

End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()

	' set the current players bonus multiplier back down to 1X
	'    SetBonusMultiplier 1
	' reset any drop targets, lights, game modes etc..

	If (BallsRemaining(CurrentPlayer) =BallsPerGame)  And 	bExtraBallWonThisBall = False Then
		ResetStartofGameVariables
		TurnOnStartOfGameLights 
		PlaySong "m_HymnOfTheHighSeas-7dB"
		SinkShipFlasherTimer.Enabled=1
		StartOfGameLightSequence		
	Else
	vpmtimer.addtimer 500, "ResetNewBallVariables '"

	End If
	bExtraBallWonThisBall = False

	'Reset any table specific


	'This is a new ball, so activate the ballsaver
	bBallSaverReady = True
	'and the skillshot
	'    bSkillShotPlayedOnce = False
	bSkillShotReady = True

End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()

	debug.print "CreateNewBall"
	PlaySoundAt "ball_trough", lane4 
	vpmtimer.addtimer 2000, "DMDScoreNow'"
	AddScore 0
	' create a ball in the plunger lane kicker.
	BallRelease.CreateSizedball BallSize / 2
	dof 110 ,DOFPulse

	' There is a (or another) ball on the playfield
	BallsOnPlayfield = BallsOnPlayfield + 1
	' kick it out..
'	RandomSoundBallRelease BallRelease
	BallRelease.Kick 90, 4

	' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
	' set the bAutoPlunger flag to kick the ball in play automatically
	If BallsOnPlayfield > 1 Then
		DOF 129, DOFPulse
		bMultiBallMode = True
		bAutoPlunger = True
	End If
End Sub

Sub BallReleaseSound
'	RandomSoundBallRelease BallRelease
End Sub


' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
	bAutoPlunger = True
	debug.print "addmultiball"
	mBalls2Eject = mBalls2Eject + nballs
	CreateMultiballTimer.Enabled = True
	'and eject the first ball
'	vpmtimer.addtimer 2000, "DMDScoreNow'"

	vpmtimer.addtimer 1200, "CreateMultiballTimer_Timer '"
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
	' wait if there is a ball in the plunger lane
	If bBallInPlungerLane Then
		Exit Sub
	Else
		If BallsOnPlayfield <MaxMultiballs Then
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
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 10 'yes 10 points :)
	If BonusMultiplier(CurrentPlayer)=0 Then:BonusMultiplier(CurrentPlayer)=1 
	FirstBallStarted=False
	debug.print "EndOfBall"
	'
	' the first ball has been lost. From this point on no new players can join in
	bOnTheFirstBall = False

	' only process any of this if the table is not tilted.  (the tilt recovery
	' mechanism will handle any extra balls or end of game)
'	vpmtimer.addtimer 200, "DMDScoreNow'"
	If NOT Tilted Then

 'Count the bonus. This table uses several bonus
        DMD CL(0, "BONUS"), "", "", eBlink, eNone, eNone, 1000, True, ""

        'ShipsSunkBonus
        AwardPoints = ShipBonusCount(CurrentPlayer) * 1000000 * BonusMultiplier(CurrentPlayer)
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "SHIPS SUNK " & ShipBonusCount(CurrentPlayer)), "", eBlink, eNone, eNone, 1000, True, ""

        'ModesCompleteBonus
        AwardPoints = ModesCompleted(CurrentPlayer) * 1000000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "MODES COMPLETE " & ModesCompleted(CurrentPlayer)), "", eBlink, eNone, eNone, 1000, True, ""

  '      'TreasureChestBonus
  '      AwardPoints = TreasureChestCompleted(CurrentPlayer) * 100000* BonusMultiplier(CurrentPlayer)
  '      TotalBonus = TotalBonus + AwardPoints
  '      DMD CL(0, FormatScore(AwardPoints)), CL(1, "Treasure " & TreasureChestCompleted(CurrentPlayer)), "", eBlink, eNone, eNone, 1000, True, ""

        ' calculate the totalbonus
        DMD CL(0, FormatScore(TotalBonus)), CL(1, "TOTAL BONUS " & " X" & BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 1500, True, ""
'        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        ' Add the bonus to the score

        AddScore TotalBonus


		vpmtimer.addtimer 6000, "EndOfBall2 '"
		Else 
		vpmtimer.addtimer 100, "EndOfBall2 '" 'If tilted add short delay and move to the second part of end of the ball
	End If
End Sub




' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
	' if were tilted, reset the internal tilted flag (this will also
	' set TiltWarnings back to zero) which is useful if we are changing player LOL
	Tilted = False
	Tilt = 0
	MechTilt = 0
	DisableTable False 'enable again bumpers and slingshots

	' has the player won an extra-ball ? (might be multiple outstanding)
	If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
		debug.print "Extra Ball"

		' yep got to give it to them
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1


		' if no more EB's then turn off any shoot again light
		If(ExtraBallsAwards(CurrentPlayer) = 0) Then
			LightShootAgain.State = 0
			EBActive=False
			Debug.Print "EBActive=False"
		End If

		' You may wish to do a bit of a song AND dance at this point
		DMD CL(0,"EXTRA BALL"), CL(1,"SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, ""

		' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball

		ResetForNewPlayerBall()

		' Create a new ball in the shooters lane

		CreateNewBall
		EBActive=False
		bExtraBallWonThisBall=False
		LightShootAgain.State=0
		debug.print "EndOfBall2"

	Else ' no extra balls

		BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

		' was that the last ball ?
		If(BallsRemaining(CurrentPlayer) <= 0) Then
			'debug.print "No More Balls, High Score Entry"

			' Submit the CurrentPlayers score to the High Score system
			CheckHighScore()
			' you may wish to play some music at this point

		Else

			' not the last ball (for that player)
			' if multiple players are playing then move onto the next one
			EndOfBallComplete()

			debug.print "EndOfBallComplete"

		End If
	End If
End Sub

' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Dim BallFinished
Sub EndOfBallComplete()
	Dim NextPlayer
	BallFinished=True
'	ResetAllModes 'Stop current modes

	debug.print "EndOfBall - Complete"

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

		' set the machine into game over mode
		EndOfGame()

		' you may wish to put a Game Over message on the desktop/backglass

	Else
		' set the next player
		CurrentPlayer = NextPlayer

		' make sure the correct display is up to date
		AddScore 0
		
		' reset the playfield for the new player (or new ball)
		vpmtimer.addtimer 500, "ResetForNewPlayerBall()'" 

		' AND create a new ball
		vpmtimer.addtimer 1500, "CreateNewBall()'"        

		' play a sound if more than 1 player
		If PlayersPlayingGame> 1 Then
			'         PlaySound "vo_player" &CurrentPlayer
			DMD "_", CL(1, "PLAYER " &CurrentPlayer), "_", eNone, eNone, eNone, 800, True, ""
		End If
	End If

End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
	MakeShipsInvisible
	debug.print "End Of Game"
	bGameInPLay = False
	bGameEnded(CurrentPlayer)=True
	' just ended your game then play the end of game tune
	If NOT bJustStarted Then

		PlaySong "m_Destination TortugaShort-6dB"
	End If

	bJustStarted = False
	'   ' ensure that the flippers are down
	SolLFlipper 0
	SolRFlipper 0

	' terminate all Mode - eject locked balls
	' most of the Mode/timers terminate at the end of the ball



	'    PlayQuote.Enabled = 0	refer to ghost buster slimer. requires timer and Sub



	' set any lights for the attract mode
	GiOff
	PlaySound "CO_TootlesOzone"
	StartAttractMode
	' you may wish to light any Game Over Light you may have
End Sub

Function Balls
	Dim tmp
	tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
	If tmp> BallsPerGame Then
		Balls = BallsPerGame
	Else
		Balls = tmp
	End If
End Function



Sub 	TurnOffStartOfGameLights

End Sub


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
	If Not bGameInPlay Then Exit Sub
	' Exit Sub ' only for debugging - this way you can add balls from the debug window

	BallsOnPlayfield = BallsOnPlayfield - 1

	' pretend to knock the ball into the ball storage mech
	RandomSoundDrain Drain
	'if Tilted the end Ball Mode
	If Tilted Then
		StopEndOfBallMode
	End If

	' if there is a game in progress AND it is not Tilted
	If(bGameInPLay = True) AND(Tilted = False) Then

		' is the ball saver active,
		If(bBallSaverActive = True) Then

			' yep, create a new ball in the shooters lane
			' we use the Addmultiball in case the multiballs are being ejected
			AddMultiball 1
			' we kick the ball with the autoplunger
			bAutoPlunger = True
			' you may wish to put something on a display or play a sound at this point
			DMD "_", CL(1, "BALL SAVED"), "_", eNone, eNone, eNone, 800, True, ""
		Else
			' cancel any multiball if on last ball (ie. lost all other balls)
			If(BallsOnPlayfield = 1) Then
				' AND in a multi-ball??
				If(bMultiBallMode = True) then
					' not in multiball mode any more
					bMultiBallMode = False
					' you may wish to change any music over at this point and
					BarbaBlancaMultiballActive=False:debug.print "BarbaBlancaMultiball=False" 'This ensures Golden reef Selection becomes active with 1 ball remaining
					DirtyCreatureMuliBallActive=False:debug.print "DirtyCreatureMultiball=False" 'This is a balls on table check. Othermodes are prevented from activated at golden reef when a golden reef mode is active
					WildSeasMultiballActive=False:debug.print "WildSeasMultiball=False" 'This is a balls on table check. Othermodes are prevented from activated at golden reef when a golden reef mode is active
				End If
			End If

			' was that the last ball on the playfield
			If(BallsOnPlayfield = 0) Then
				StopEndOfBallMode
				If ShipSink(CurrentPlayer)=False Then: vpmtimer.addtimer 2000, "EndOfBall'": End If
				If ShipSink(CurrentPlayer)=True Then: vpmtimer.addtimer 10000, "EndOfBall'":End If 'ShipSink(CurrentPlayer)=False: End If
'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
			End If
		End If
	End If
End Sub


' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.


Sub ballsavestarttrigger_hit
	' if there is a need for a ball saver, then start off a timer
	' only start if it is ready, and it is currently not running, else it will reset the time period
	If(bBallSaverReady = True) AND(20 <> 0) And(bBallSaverActive = False) Then
		If Hard=1 Then EnableBallSaver 20 End If
		If Hard=0 Then EnableBallSaver 25 End If
	End If

	LastSwitchHit=  "ballsavestarttrigger"	
End Sub

Sub swPlungerRest_Hit()
	debug.print "ball in plunger lane"
	bBallInPlungerLane = True 
	If bMultiBallMode=False And bBallSaverActive = False Then:bSkillshotReady = True

	' turn on Launch light is there is one
	'LaunchLight.State = 2
	' kick the ball in play if the bAutoPlunger flag is on
	If bMultiBallMode=True Then:bAutoPlunger=True
	If bAutoPlunger=True Then
		vpmtimer.addtimer 2500, "AutoPlungerDelay '"
	End If
	If bMultiBallMode=False Then:RaisePlungerDiversionWall:bSkillShotReady=True:Light_GR.State=2:End If
	LastSwitchHit = "swPlungerRest"
End Sub

Sub swPlungerRest_UnHit()
	bBallInPlungerLane = False
	swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
	If bSkillShotReady Then
		ResetSkillShotTimer.Enabled = 1
	End If
End Sub

Sub ResetSkillShotTimer_Timer
	 bSkillShotReady=False:Light_GR.State=0:ResetSkillShotTimer.Enabled = 0
End Sub

Sub AutoPlungerDelay
	PlungerIM.Strength = 0.2
	'PlungerIM.AutoFire
	PlungerIM.Strength = Plunger.MechStrength
	Plunger.AutoPlunger = True
	Plunger.Pullback 
	Plunger.Fire
	PlaySoundAt SoundFXDOF("Popper", 112, DOFPulse, DOFContactors), Plunger
	DOF 125, DOFPulse
	'DOF 112 ,DOFPulse
	bAutoPlunger = False
	Plunger.AutoPlunger = False
End Sub


Sub swPlungerRest_Timer
	swPlungerRest.TimerEnabled = 0
End Sub



Sub EnableBallSaver(seconds)
	debug.print "Ballsaver started"
	' set our game flag
	bBallSaverActive = True
	bBallSaverReady = False
	' start the timer
	BallSaverTimerExpired.Interval = 1000 * seconds
	BallSaverTimerExpired.Enabled = True
	BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
	BallSaverSpeedUpTimer.Enabled = True
	' if you have a ball saver light you might want to turn it on at this point (or make it flash)
	Light_BallSaver.BlinkInterval = 160
	Light_BallSaver.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag

Sub BallSaverTimerExpired_Timer()
	debug.print "Ballsaver ended skillshot not available"
'	BallSaverTimerExpired.Enabled = False
	' clear the flag
	bBallSaverActive = False
	' if you have a ball saver light then turn it off at this point
	'    Light_BallSaver.BlinkInterval = 80

	Light_BallSaver.State = 0
	ArmCanonSkillShot=False
	vpmtimer.addtimer 2000, "GraceTime '"
End Sub


Sub BallSaverSpeedUpTimer_Timer()
	'debug.print "Ballsaver Speed Up Light"
	BallSaverSpeedUpTimer.Enabled = False
	' Speed up the blinking
	Light_BallSaver.BlinkInterval = 80
	Light_BallSaver.State = 2
End Sub

Sub GraceTime
	bBallSaverActive = False
	BallSaverTimerExpired.Enabled = False
End Sub




' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
' In this table we use SecondRound variable to double the score points in the second round after killing Malthael
Sub AddScore(points)
	If(Tilted = False) Then
		' add the points to the current players score variable
		Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
	End if
	' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
	If(Tilted = False) Then
		' add the bonus to the current players bonus variable
		BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
	End if
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
	' Jackpots only generally increment in multiball mode AND not tilted
	' but this doesn't have to be the case
	'    If(Tilted = False) Then

	' If(bMultiBallMode = True) Then
	'       Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
	'       DMD "_", CL(1, "INCREASED JACKPOT"), "_", eNone, eNone, eNone, 800, True, ""
	' you may wish to limit the jackpot to a upper limit, ie..
	'	If (Jackpot >= 6000) Then
	'		Jackpot = 6000
	' 	End if
	'End if
	'   End if
End Sub

Sub AddSuperJackpot(points) 'not used in this table
	'    If(Tilted = False) Then
	'   End if
End Sub

Sub AddBonusMultiplier(n)
	'    Dim NewBonusLevel
	' if not at the maximum bonus level
	'    if(BonusMultiplier(CurrentPlayer) + n <= MaxMultiplier) then
	' then add and set the lights
'	NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
	'       SetBonusMultiplier(NewBonusLevel)
	'       DMD "_", CL(1, "BONUS X " &NewBonusLevel), "_", eNone, eNone, eNone, 2000, True, "fx_bonus"
	'    Else
	'        AddScore 50000
	'        DMD "_", CL(1, "50000"), "_", eNone, eNone, eNone, 800, True, ""
	'   End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly

Sub SetBonusMultiplier(Level)
	'   ' Set the multiplier to the specified level
	'   BonusMultiplier(CurrentPlayer) = Level
	'   UPdateBonusXLights(Level)
End Sub

Sub UpdateBonusXLights(Level)
	' Update the lights
	'    Select Case Level
	'       Case 1:light54.State = 0:light55.State = 0:light56.State = 0:light57.State = 0
	'       Case 2:light54.State = 1:light55.State = 0:light56.State = 0:light57.State = 0
	'      Case 3:light54.State = 0:light55.State = 1:light56.State = 0:light57.State = 0
	'      Case 4:light54.State = 0:light55.State = 0:light56.State = 1:light57.State = 0
	''      Case 5:light54.State = 0:light55.State = 0:light56.State = 0:light57.State = 1
	'   End Select
End Sub

Sub AddPlayfieldMultiplier(n)
	'    Dim NewPFLevel
	' if not at the maximum level x
	'   if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier) then
	' then add and set the lights
	'        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
	'        SetPlayfieldMultiplier(NewPFLevel)
	'        DMD "_", CL(1, "PLAYFIELD X " &NewPFLevel), "_", eNone, eNone, eNone, 2000, True, "fx_bonus"
	'    Else 'if the 5x is already lit
	'        AddScore 50000
	'        DMD "_", CL(1, "50000"), "_", eNone, eNone, eNone, 2000, True, ""
	'    End if
	'Start the timer to reduce the playfield x every 30 seconds
	'   pfxtimer.Enabled = 0
	'    pfxtimer.Enabled = 1
	'End Sub

	' Set the Playfield Multiplier to the specified level AND set any lights accordingly

	'Sub SetPlayfieldMultiplier(Level)
	' Set the multiplier to the specified level
	'   PlayfieldMultiplier(CurrentPlayer) = Level
	'   UpdatePFXLights(Level)
	'End Sub

	'Sub UpdatePFXLights(Level)
	' Update the lights
	'   Select Case Level
	'       Case 1:light3.State = 0:light2.State = 0:light1.State = 0:light4.State = 0
	'       Case 2:light3.State = 1:light2.State = 0:light1.State = 0:light4.State = 0
	'       Case 3:light3.State = 0:light2.State = 1:light1.State = 0:light4.State = 0
	'       Case 4:light3.State = 0:light2.State = 0:light1.State = 1:light4.State = 0
	'       Case 5:light3.State = 0:light2.State = 0:light1.State = 0:light4.State = 1
	'   End Select
	' show the multiplier in the DMD
	' in this table the multiplier is always shown in the score display sub
End Sub




Sub ExtraBallHurryUp
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_ExtraBallLitSailorOzone":End If
	DMD CL(0, "     EXTRA BALL "), CL(1, "  IS LIT SAILOR "), "", eBlink, eNone, eNone, 4000, True, ""
	If LightShootAgain.State=0 Then Light_ExtraBall.State=2                          ':Light_ExtraBall.TimerEnabled=True
End Sub

Sub Light_ExtraBall_Timer
	Light_ExtraBall.State=0
End Sub

Dim EBActive

Sub AwardExtraBall()
	SinkShipFlasherTimer.Enabled=1:
	If NOT bExtraBallWonThisBall Then
		DOF 122, DOFPulse
		EBActive=True
		PlaySoundAt "fx_knocker",KickerTarget
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:Playsound "CO_ExtraBall2Ozone":End If
		Light_ExtraBall.State=0
	DMD "", "", "DMD_EB1", eNone, eBlink, eNone, 500, False, "" 
	DMD "", "", "DMD_EB2", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB3", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_EB4", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB5", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB6", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB7", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_EB8", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB9", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB10", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB11", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB12", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB13", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_EB14", eNone, eBlink, eNone, 1500, False, "" 
	DMD "", "", "DMD_EB15", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB16", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB17", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_EB18", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB19", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB20", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_EB21", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_EB20", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_EB19", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB18", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB19", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_EB20", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_EB21", eNone, eBlink, eNone, 5000, False, ""
		vpmtimer.addtimer 8000, "DMDScoreNow'"

		EBActive=1
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
		bExtraBallWonThisBall = True
		LightShootAgain.State = 1 'light the shoot again lamp
		'       GiEffect 2
		'       LightEffect 2
		Debug.print "EBActive=True"
 
	End If
End Sub

Sub AwardSpecial()
	DMD "_", CL(1, ("REPLAY")), "_", eNone, eBlink, eNone, 2200, True,"" 
	DOF 122, DOFPulse
	vpmTimer.AddTimer 2000, "DMDScoreNow'"
	Credits = Credits + 1
	If bFreePlay = False Then DOF 125, DOFOn
	'    LightEffect 2
	'    FlashEffect 2
End Sub

Sub AwardJackpot 'award a normal jackpot, 
'	DMD "", "", "DMD_JP1Million1", eNone, eBlink, eNone, 200, False, "" 
	DMD "", "", "DMD_JP1Million2", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP1Million3", eNone, eBlink, eNone, 100, False, ""
	DMD "", "", "DMD_JP1Million4", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP1Million5", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP1Million6", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP1Million7", eNone, eBlink, eNone, 100, False, ""
	DMD "", "", "DMD_JP1Million8", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP1Million9", eNone, eBlink, eNone, 4000, False, "" 
	vpmtimer.addtimer 3500, "DMDScoreNow'" 
	AddScore 1000000
'	PlaySoundAt "fx_knocker",KickerGoldenReef
'	DOF 122, DOFPulse
	PlaySound "CO_JackPot1Ozone"
End Sub

Sub AwardJackpot2 'award a normal jackpot, 
'	DMD "", "", "DMD_JP2Million1", eNone, eBlink, eNone, 500, False, "" 
	DMD "", "", "DMD_JP2Million2", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP2Million3", eNone, eBlink, eNone, 100, False, ""
	DMD "", "", "DMD_JP2Million4", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP2Million5", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP2Million6", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP2Million7", eNone, eBlink, eNone, 100, False, ""
	DMD "", "", "DMD_JP2Million8", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP2Million9", eNone, eBlink, eNone, 4000, False, ""
	vpmtimer.addtimer 3500, "DMDScoreNow'" 
	AddScore 2000000
'	PlaySoundAt "fx_knocker",KickerGoldenReef
'	DOF 122, DOFPulse
	PlaySound "CO_JackPot2Ozone"

End Sub

Sub AwardJackpot3 'award a normal jackpot, 
'	DMD "", "", "DMD_JP3Million1", eNone, eBlink, eNone, 500, False, "" 
	DMD "", "", "DMD_JP3Million2", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP3Million3", eNone, eBlink, eNone, 100, False, ""
	DMD "", "", "DMD_JP3Million4", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP3Million5", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP3Million6", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP3Million7", eNone, eBlink, eNone, 100, False, ""
	DMD "", "", "DMD_JP3Million8", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_JP3Million9", eNone, eBlink, eNone, 4000, False, ""
	vpmtimer.addtimer 3500, "DMDScoreNow'" 
	AddScore 3000000
'	PlaySoundAt "fx_knocker",KickerGoldenReef
'	DOF 122, DOFPulse
	PlaySound "CO_JackPot3Ozone"
End Sub

Sub AwardSuperJackpot 
	PlaySoundAt "fx_knocker",KickerGoldenReef
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_SuperJackpot1Ozone":End If
	
	DMD "", "", "DMD_SuperJackpot", eNone, eBlink, eNone, 500, False, "" 
	DMD "", "", "DMD_SuperJackPot1", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot2", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot3", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot4", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot5", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot6", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot7", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot8", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot9", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot10", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot11", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot12", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot13", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot14", eNone, eBlink, eNone, 200, False, "" 
	DMD "", "", "DMD_SuperJackPot15", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot16", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot17", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot18", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot19", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot20", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot21", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot22", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot23", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot24", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot25", eNone, eBlink, eNone, 4000, False, ""
	vpmtimer.addtimer 3500, "DMDScoreNow'"
	AddScore 2500000 
	DOF 122, DOFPulse
End Sub

Sub AwardSuperJackpot2 
	PlaySoundAt "fx_knocker",KickerGoldenReef
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_SuperJackPot2Ozone":End If
	
	DMD "", "", "DMD_SuperJackpot", eNone, eBlink, eNone, 500, False, "" 
	DMD "", "", "DMD_SuperJackPot1", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot2", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot3", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot4", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot5", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot6", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot7", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot8", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot9", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot10", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot11", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot12", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot13", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot14", eNone, eBlink, eNone, 200, False, "" 
	DMD "", "", "DMD_SuperJackPot15", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot16", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot17", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot18", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot19", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot20", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot21", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot22", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot23", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_SuperJackPot24", eNone, eBlink, eNone, 75, False, "" 
	DMD "", "", "DMD_SuperJackPot25", eNone, eBlink, eNone, 3000, False, "" 
	vpmtimer.addtimer 3500, "DMDScoreNow'" 
	AddScore 3000000
	DOF 122, DOFPulse
End Sub

Sub AwardSkillshot() 'Notused in this game awards at the kickers
	'   Addscore SkillShotValue(CurrentPlayer)
	'    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 250000
'	SlayDragonInitiate
'	PlaySound "CO_AttackTheDragon"
	AddScore 100000
	'   ResetSkillShotTimer_Timer
	'show dmd animation
	'    DMD "", CL(1, "SKILLSHOT"), "", eNone, eBlink, eNone, 2000, True, ""
	'    DOF 127, DOFPulse
	' increment the skillshot value with 250.000
	'do some light show
	'    GiEffect 2
	'    LightEffect 2
End Sub





'*****************************
'    Load / Save / Highscore
'*****************************
'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
	Dim x
	x = LoadValue(TableName, "HighScore1")
	If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 2000000000 :End If
	x = LoadValue(TableName, "HighScore1Name")
	If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" :End If
	x = LoadValue(TableName, "HighScore2")
	If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 1000000000 :End If
	x = LoadValue(TableName, "HighScore2Name")
	If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" :End If
	x = LoadValue(TableName, "HighScore3")
	If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 750000000 :End If
	x = LoadValue(TableName, "HighScore3Name")
	If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" :End If
	x = LoadValue(TableName, "HighScore4")
	If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 500000000 :End If
	x = LoadValue(TableName, "HighScore4Name")
	If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" :End If
	x = LoadValue(TableName, "Credits")
	If(x <> "") then Credits = CInt(x) Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff : End If : End If
	x = LoadValue(TableName, "TotalGamesPlayed")
	If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 :End If
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
	SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
	HighScoreName(0) = "AAA"
	HighScoreName(1) = "BBB"
	HighScoreName(2) = "CCC"
	HighScoreName(3) = "DDD"
	HighScore(0) = 100000000
	HighScore(1) = 200000000
	HighScore(2) = 300000000
	HighScore(3) = 400000000
	Savehs
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
	Dim tmp
	tmp = Score(CurrentPlayer)

	If tmp > HighScore(0)Then 'add 1 credit for beating the highscore
		Credits = Credits + 1
		DOF 125, DOFOn
	End If

	If tmp > HighScore(3)Then
		PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
		DOF 121, DOFPulse
		HighScore(3) = tmp
		'enter player's name
		HighScoreEntryInit()
	Else
		EndOfBallComplete()
	End If
End Sub

Sub HighScoreEntryInit()
	hsbModeActive = True
	hsLetterFlash = 0

	hsEnteredDigits(0) = " "
	hsEnteredDigits(1) = " "
	hsEnteredDigits(2) = " "
	hsCurrentDigit = 0

	hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow
	hsCurrentLetter = 1
	DMDFlush()
	HighScoreDisplayNameNow()

	HighScoreFlashTimer.Interval = 250
	HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
	If keycode = LeftFlipperKey Then
		playsound "fx_Previous"
		hsCurrentLetter = hsCurrentLetter - 1
		if(hsCurrentLetter = 0)then
			hsCurrentLetter = len(hsValidLetters)
		end if
		HighScoreDisplayNameNow()
	End If

	If keycode = RightFlipperKey Then
		playsound "fx_Next"
		hsCurrentLetter = hsCurrentLetter + 1
		if(hsCurrentLetter > len(hsValidLetters))then
			hsCurrentLetter = 1
		end if
		HighScoreDisplayNameNow()
	End If

	If keycode = PlungerKey OR keycode = StartGameKey Then
		if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
			playsound "fx_Enter"
			hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
			hsCurrentDigit = hsCurrentDigit + 1
			if(hsCurrentDigit = 3)then
				HighScoreCommitName()
			else
				HighScoreDisplayNameNow()
			end if
		else
			playsound "fx_Esc"
			hsEnteredDigits(hsCurrentDigit) = " "
			if(hsCurrentDigit > 0)then
				hsCurrentDigit = hsCurrentDigit - 1
			end if
			HighScoreDisplayNameNow()
		end if
	end if
End Sub

Sub HighScoreDisplayNameNow()
	HighScoreFlashTimer.Enabled = False
	hsLetterFlash = 0
	HighScoreDisplayName()
	HighScoreFlashTimer.Enabled = True
End Sub

Dim dLine(2)
Sub HighScoreDisplayName()
	Dim i
	Dim TempTopStr
	Dim TempBotStr

	TempTopStr = "YOUR NAME:"
	dLine(0) = ExpandLine(TempTopStr,0)
	DMDUpdate 0

	TempBotStr = "    > "
	if(hsCurrentDigit > 0)then TempBotStr = TempBotStr & hsEnteredDigits(0)
	if(hsCurrentDigit > 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
	if(hsCurrentDigit > 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

	if(hsCurrentDigit <> 3)then
		if(hsLetterFlash <> 0)then
			TempBotStr = TempBotStr & "_"
		else
			TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
		end if
	end if

	if(hsCurrentDigit < 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
	if(hsCurrentDigit < 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

	TempBotStr = TempBotStr & " <    "
	dLine(1) = ExpandLine(TempBotStr,1)
	DMDUpdate 1
End Sub

Sub HighScoreFlashTimer_Timer()
	HighScoreFlashTimer.Enabled = False
	hsLetterFlash = hsLetterFlash + 1
	if(hsLetterFlash = 2)then hsLetterFlash = 0
	HighScoreDisplayName()
	HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
	HighScoreFlashTimer.Enabled = False
	hsbModeActive = False

	hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
	if(hsEnteredName = "   ")then
		hsEnteredName = "YOU"
	end if

	HighScoreName(3) = hsEnteredName
	SortHighscore
	EndOfBallComplete()
End Sub

Sub SortHighscore
	Dim tmp, tmp2, i, j
	For i = 0 to 3
		For j = 0 to 2
			If HighScore(j) < HighScore(j + 1)Then
				tmp = HighScore(j + 1)
				tmp2 = HighScoreName(j + 1)
				HighScore(j + 1) = HighScore(j)
				HighScoreName(j + 1) = HighScoreName(j)
				HighScore(j) = tmp
				HighScoreName(j) = tmp2
			End If
		Next
	Next
End Sub



' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text. 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Const dqSize = 64

Dim dqHead
Dim dqTail
Dim deSpeed
Dim deBlinkSlowRate
Dim deBlinkFastRate

Dim dCharsPerLine(2)

Dim deCount(2)
Dim deCountEnd(2)
Dim deBlinkCycle(2)

Dim dqText(2, 64)
Dim dqEffect(2, 64)
Dim dqTimeOn(64)
Dim dqbFlush(64)
Dim dqSound(64)

Dim FlexDMD
Dim DMDScene

Sub DMD_Init() 'default/startup values
	If UseFlexDMD Then
		Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
		If Not FlexDMD is Nothing Then
            If FlexDMDHighQuality Then
				FlexDMD.TableFile = Table1.Filename & ".vpx"
				FlexDMD.RenderMode = 2
				FlexDMD.Width = 256
				FlexDMD.Height = 64
				FlexDMD.Clear = True
				FlexDMD.GameName = cGameName
				FlexDMD.Run = True
				Set DMDScene = FlexDMD.NewGroup("Scene")
				DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
				DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
				For i = 0 to 40
					DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
					Digits(i).Visible = False
				Next
				digitgrid.Visible = False
				For i = 0 to 19 ' Top
					DMDScene.GetImage("Dig" & i).SetBounds 8 + i * 12, 6, 14, 22
				Next
				For i = 20 to 39 ' Bottom
					DMDScene.GetImage("Dig" & i).SetBounds 8 + (i - 20) * 12, 6 + 24 + 4, 14, 22
				Next
				FlexDMD.LockRenderThread
				FlexDMD.Stage.AddActor DMDScene
				FlexDMD.UnlockRenderThread
			Else
				FlexDMD.TableFile = Table1.Filename & ".vpx"
				FlexDMD.RenderMode = 2
				FlexDMD.Width = 128
				FlexDMD.Height = 32
				FlexDMD.Clear = True
				FlexDMD.GameName = cGameName
				FlexDMD.Run = True
				Set DMDScene = FlexDMD.NewGroup("Scene")
				DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
				DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
				For i = 0 to 40
					DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
					Digits(i).Visible = False
				Next
				digitgrid.Visible = False
				For i = 0 to 19 ' Top
					DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3, 7, 11
				Next
				For i = 20 to 39 ' Bottom
					DMDScene.GetImage("Dig" & i).SetBounds 4 + (i - 20) * 6, 3 + 12 + 2, 7, 11
				Next
				FlexDMD.LockRenderThread
				FlexDMD.Stage.AddActor DMDScene
				FlexDMD.UnlockRenderThread
			End If
		End If
	End If

	Dim i, j
	DMDFlush()
	deSpeed = 20
	deBlinkSlowRate = 5
	deBlinkFastRate = 2
	dCharsPerLine(0) = 16 'characters lower line
	dCharsPerLine(1) = 20 'characters top line
	dCharsPerLine(2) = 1  'characters back line
	For i = 0 to 2
		dLine(i) = Space(dCharsPerLine(i) )
		deCount(i) = 0
		deCountEnd(i) = 0
		deBlinkCycle(i) = 0
		dqTimeOn(i) = 0
		dqbFlush(i) = True
		dqSound(i) = ""
	Next
	For i = 0 to 2
		For j = 0 to 64
			dqText(i, j) = ""
			dqEffect(i, j) = eNone
		Next
	Next
	DMD dLine(0), dLine(1), dLine(2), eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDFlush()
	Dim i
	DMDTimer.Enabled = False
	DMDEffectTimer.Enabled = False
	dqHead = 0
	dqTail = 0
	For i = 0 to 2
		deCount(i) = 0
		deCountEnd(i) = 0
		deBlinkCycle(i) = 0
	Next
End Sub

Sub DMDScore()
	Dim tmp, tmp1, tmp2
	if(dqHead = dqTail) Then

		tmp = RL(0, FormatScore(Score(Currentplayer) ) )
		'       tmp = CL(0, FormatScore(Score(Currentplayer) ) )
		tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
		tmp2 = ""
		'        tmp2 = "bkborder"
	End If
	DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
	DMDFlush
	DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
	if(dqTail <dqSize) Then
		if(Text0 = "_") Then
			dqEffect(0, dqTail) = eNone
			dqText(0, dqTail) = "_"
		Else
			dqEffect(0, dqTail) = Effect0
			dqText(0, dqTail) = ExpandLine(Text0, 0)
		End If

		if(Text1 = "_") Then
			dqEffect(1, dqTail) = eNone
			dqText(1, dqTail) = "_"
		Else
			dqEffect(1, dqTail) = Effect1
			dqText(1, dqTail) = ExpandLine(Text1, 1)
		End If

		if(Text2 = "_") Then
			dqEffect(2, dqTail) = eNone
			dqText(2, dqTail) = "_"
		Else
			dqEffect(2, dqTail) = Effect2
			dqText(2, dqTail) = Text2 'it is always 1 letter in this table
		End If

		dqTimeOn(dqTail) = TimeOn
		dqbFlush(dqTail) = bFlush
		dqSound(dqTail) = Sound
		dqTail = dqTail + 1
		if(dqTail = 1) Then
			DMDHead()
		End If
	End If
End Sub

Sub DMDHead()
	Dim i
	deCount(0) = 0
	deCount(1) = 0
	deCount(2) = 0
	DMDEffectTimer.Interval = deSpeed

	For i = 0 to 2
		Select Case dqEffect(i, dqHead)
			Case eNone:deCountEnd(i) = 1
			Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead) )
			Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead) )
			Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
				deBlinkCycle(i) = 0
			Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
				deBlinkCycle(i) = 0
		End Select
	Next
	if(dqSound(dqHead) <> "") Then
		PlaySound(dqSound(dqHead) )
	End If
	DMDEffectTimer.Enabled = True
End Sub

Sub DMDEffectTimer_Timer()
	DMDEffectTimer.Enabled = False
	DMDProcessEffectOn()
End Sub

Sub DMDTimer_Timer()
	Dim Head
	DMDTimer.Enabled = False
	Head = dqHead
	dqHead = dqHead + 1
	if(dqHead = dqTail) Then
		if(dqbFlush(Head) = True) Then
			DMDScoreNow()
		Else
			dqHead = 0
			DMDHead()
		End If
	Else
		DMDHead()
	End If
End Sub

Sub DMDProcessEffectOn()
	Dim i
	Dim BlinkEffect
	Dim Temp

	BlinkEffect = False

	For i = 0 to 2
		if(deCount(i) <> deCountEnd(i) ) Then
			deCount(i) = deCount(i) + 1

			select case(dqEffect(i, dqHead) )
				case eNone:
					Temp = dqText(i, dqHead)
				case eScrollLeft:
					Temp = Right(dLine(i), dCharsPerLine(i) - 1)
					Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
				case eScrollRight:
					Temp = Mid(dqText(i, dqHead), (dCharsPerLine(i) + 1) - deCount(i), 1)
					Temp = Temp & Left(dLine(i), dCharsPerLine(i) - 1)
				case eBlink:
					BlinkEffect = True
					if((deCount(i) MOD deBlinkSlowRate) = 0) Then
						deBlinkCycle(i) = deBlinkCycle(i) xor 1
					End If

					if(deBlinkCycle(i) = 0) Then
						Temp = dqText(i, dqHead)
					Else
						Temp = Space(dCharsPerLine(i) )
					End If
				case eBlinkFast:
					BlinkEffect = True
					if((deCount(i) MOD deBlinkFastRate) = 0) Then
						deBlinkCycle(i) = deBlinkCycle(i) xor 1
					End If

					if(deBlinkCycle(i) = 0) Then
						Temp = dqText(i, dqHead)
					Else
						Temp = Space(dCharsPerLine(i) )
					End If
			End Select

			if(dqText(i, dqHead) <> "_") Then
				dLine(i) = Temp
				DMDUpdate i
			End If
		End If
	Next

	if(deCount(0) = deCountEnd(0) ) and(deCount(1) = deCountEnd(1) ) and(deCount(2) = deCountEnd(2) ) Then

		if(dqTimeOn(dqHead) = 0) Then
			DMDFlush()
		Else
			if(BlinkEffect = True) Then
				DMDTimer.Interval = 10
			Else
				DMDTimer.Interval = dqTimeOn(dqHead)
			End If

			DMDTimer.Enabled = True
		End If
	Else
		DMDEffectTimer.Enabled = True
	End If
End Sub

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
	If TempStr = "" Then
		TempStr = Space(dCharsPerLine(id) )
	Else
		if(Len(TempStr)> Space(dCharsPerLine(id) ) ) Then
			TempStr = Left(TempStr, Space(dCharsPerLine(id) ) )
		Else
			if(Len(TempStr) <dCharsPerLine(id) ) Then
				TempStr = TempStr & Space(dCharsPerLine(id) - Len(TempStr) )
			End If
		End If
	End If
	ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
	dim i
	dim NumString

	NumString = CStr(abs(Num) )

	For i = Len(NumString) -3 to 1 step -3
		if IsNumeric(mid(NumString, i, 1) ) then
			NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1) ) + 48) & right(NumString, Len(NumString) - i)
		end if
	Next
	FormatScore = NumString
End function

Function CL(id, NumString)
	Dim Temp, TempStr
	Temp = (dCharsPerLine(id) - Len(NumString) ) \ 2
	TempStr = Space(Temp) & NumString & Space(Temp)
	CL = TempStr
End Function

Function RL(id, NumString)
	Dim Temp, TempStr
	Temp = dCharsPerLine(id) - Len(NumString)
	TempStr = Space(Temp) & NumString
	RL = TempStr
End Function

Function FL(id, aString, bString) 'fill line
	Dim tmp, tmpStr
	aString = LEFT(aString, dCharsPerLine(id))
	bString = LEFT(bString, dCharsPerLine(id))
	tmp = dCharsPerLine(id)- Len(aString)- Len(bString)
	If tmp <0 Then tmp = 0
	tmpStr = aString & Space(tmp) & bString
	FL = tmpStr
End Function

'**************
' Update DMD
'**************

Sub DMDUpdate(id)
	Dim digit, value
	If UseFlexDMD Then FlexDMD.LockRenderThread
	Select Case id
		Case 0 'top text line
			For digit = 0 to 19
				DMDDisplayChar mid(dLine(0), digit + 1, 1), digit
			Next
		Case 1 'bottom text line
			For digit = 20 to 39
				DMDDisplayChar mid(dLine(1), digit -19, 1), digit
			Next
		Case 2 ' back image - back animations
			If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "bkempty"
			Digits(40).ImageA = dLine(2)
			If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
	End Select
	If UseFlexDMD Then FlexDMD.UnlockRenderThread
End Sub

Sub DMDDisplayChar(achar, adigit)
	If achar = "" Then achar = " "
	achar = ASC(achar)
	Digits(adigit).ImageA = Chars(achar)
	If UseFlexDMD Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap
End Sub

'****************************
' JP's new DMD using flashers
'****************************

Dim Digits, Chars(255), BumpS(30)

DMDInit

Sub DMDInit
	Dim i
	Digits = Array(digit001, digit002, digit003, digit004, digit005, digit006, digit007, digit008, digit009, digit010, _
	digit011, digit012, digit013, digit014, digit015, digit016, digit017, digit018, digit019, digit020, _
	digit021, digit022, digit023, digit024, digit025, digit026, digit027, digit028, digit029, digit030, _
	digit031, digit032, digit033, digit034, digit035, digit036, digit037, digit038, digit039, digit040, _
	digit041)

	For i = 0 to 255:Chars(i) = "dempty":Next

	Chars(32) = "dempty"
	Chars(43) = "dplus"    '+
	Chars(46) = "ddot"     '.
	Chars(48) = "d0"       '0
	Chars(49) = "d1"       '1
	Chars(50) = "d2"       '2
	Chars(51) = "d3"       '3
	Chars(52) = "d4"       '4
	Chars(53) = "d5"       '5
	Chars(54) = "d6"       '6
	Chars(55) = "d7"       '7
	Chars(56) = "d8"       '8
	Chars(57) = "d9"       '9
	Chars(60) = "dless"    '<
	Chars(61) = "dequal"   '=
	Chars(62) = "dmore"    '>
	Chars(64) = "bkempty"  '@
	Chars(65) = "da"       'A
	Chars(66) = "db"       'B
	Chars(67) = "dc"       'C
	Chars(68) = "dd"       'D
	Chars(69) = "de"       'E
	Chars(70) = "df"       'F
	Chars(71) = "dg"       'G
	Chars(72) = "dh"       'H
	Chars(73) = "di"       'I
	Chars(74) = "dj"       'J
	Chars(75) = "dk"       'K
	Chars(76) = "dl"       'L
	Chars(77) = "dm"       'M
	Chars(78) = "dn"       'N
	Chars(79) = "do"       'O
	Chars(80) = "dp"       'P
	Chars(81) = "dq"       'Q
	Chars(82) = "dr"       'R
	Chars(83) = "ds"       'S
	Chars(84) = "dt"       'T
	Chars(85) = "du"       'U
	Chars(86) = "dv"       'V
	Chars(87) = "dw"       'W
	Chars(88) = "dx"       'X
	Chars(89) = "dy"       'Y
	Chars(90) = "dz"       'Z
	Chars(94) = "dup"      '^
	'    Chars(95) = '_
	Chars(96) = "d0a"  '0.
	Chars(97) = "d1a"  '1. 'a
	Chars(98) = "d2a"  '2. 'b
	Chars(99) = "d3a"  '3. 'c
	Chars(100) = "d4a" '4. 'd
	Chars(101) = "d5a" '5. 'e
	Chars(102) = "d6a" '6. 'f
	Chars(103) = "d7a" '7. 'g
	Chars(104) = "d8a" '8. 'h
	Chars(105) = "d9a" '9  'i
	Chars(112) = "dp2" 'p 'p dark
	Chars(113) = "dk2" 'q 'k dark
	Chars(114) = "de2" 'r 'e dark
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


'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 10 colors: red, orange, amber, yellow...
'******************************************

Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white, base,lightgrey,midgrey,darkgrey

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
lightgrey=12
midgrey=13
darkgrey=14

Sub SetLightColor(n, col, stat)
	Select Case col
		Case red
			n.color = RGB(18, 0, 0)
			n.colorfull = RGB(255, 0, 0)
		Case orange
			n.color = RGB(18, 3, 0)
			n.colorfull = RGB(255, 64, 64)
		Case amber
			n.color = RGB(193, 49, 0)
			n.colorfull = RGB(255, 153, 0)
		Case yellow
			n.color = RGB(18, 18, 0)
			n.colorfull = RGB(240, 220, 0)
		Case darkgreen
			n.color = RGB(0, 8, 0)
			n.colorfull = RGB(0, 150, 0) '(0, 64, 0)
		Case green
			n.color = RGB(0, 18, 0)
			n.colorfull = RGB(0, 200, 100) '(0, 255, 0)
		Case blue
			n.color = RGB(0, 18, 18)
			n.colorfull = RGB(0, 255, 255) '0, 255, 255
		Case darkblue
			n.color = RGB(0, 8, 8)
			n.colorfull = RGB(0, 20, 230) '(0, 0, 255)
		Case purple
			n.color = RGB(128, 128, 255)	'128.0,128
			n.colorfull = RGB(3, 0, 240)
		Case white
			n.color = RGB(255, 252, 224)
			n.colorfull = RGB(193, 91, 0)
		Case white
			n.color = RGB(255, 252, 224)
			n.colorfull = RGB(193, 91, 0)
		Case base
			n.color = RGB(128, 128, 255)
			n.colorfull = RGB(255, 252, 224)
		Case lightgrey
			n.color = RGB(192, 192, 192)
		Case midgrey
			n.color = RGB(120, 120, 120)
		Case darkgrey
			n.color = RGB(50, 50, 50)
	End Select
	If stat <> -1 Then
		n.State = 0
		n.State = stat
	End If
End Sub

'*************************
' Rainbow Changing Lights
'*************************

Sub ResetAllLightsColor ' Called at a new game
'	SetLightColor LightLeftInlane,blue, -1
'	SetLightColor LightLeftEscape,orange, -1
'	SetLightColor LightRightInlane,blue, -1
'	SetLightColor Lightrightescape,orange, -1
'	SetLightColor LightShootAgain,red, -1
'	SetLightColor Light_ExtraBall,red, -1
'	SetLightColor Light_BallSaver,blue, -1
'	SetLightColor Light_easyhard,blue, -1

'	SetLightColor l_2x, orange, -1
'	SetLightColor l_3x, orange, -1
'	SetLightColor l_4x, orange, -1
'	SetLightColor l_5x, orange, -1

'	SetLightColor lv1, yellow, -1
'	SetLightColor lv2, yellow, -1
'	SetLightColor lv3, yellow, -1
'	SetLightColor lv4, yellow, -1
'	SetLightColor lv5, yellow, -1
'	SetLightColor lv5, yellow, -1

'	SetLightColor l23A, yellow, -1
'	SetLightColor l23B, blue, -1
'	SetLightColor l23C, green, -1
'	SetLightColor l23D, red, -1
'	SetLightColor l23E, blue, -1
'	SetLightColor l23F, yellow, -1
'	SetLightColor l23G, green, -1
'	SetLightColor l23H, red, -1

'	SetLightColor l1, orange, -1
'	SetLightColor l2, orange, -1
'	SetLightColor l3, orange, -1
'	SetLightColor Light_LoadCanon, orange, -1

'	SetLightColor l16, green, -1
'	SetLightColor l17, yellow, -1
'	SetLightColor l18, orange, -1
'	SetLightColor l19, blue, -1
'	SetLightColor l5, blue, -1
'	SetLightColor l6, blue, -1
'	SetLightColor l7, blue, -1
'	SetLightColor l8, blue, -1
	'SetLightColor l9, blue, -1
'	SetLightColor l10, blue, -1

'	SetLightColor Light_WS, yellow, -1
'	SetLightColor Light_WS1, yellow, -1
'	SetLightColor Light_WS2, yellow, -1
'	SetLightColor Light_WS3, yellow, -1
'	SetLightColor Light_WS4, yellow, -1

'	SetLightColor Light_NATT, blue, -1
'	SetLightColor Light_NATT1, blue, -1
'	SetLightColor Light_NATT2, blue, -1
'	SetLightColor Light_NATT3, blue, -1
'	SetLightColor Light_NATT4, blue, -1

'	SetLightColor Light_RTS, green, -1
'	SetLightColor Light_RTS1, green, -1
'	SetLightColor Light_RTS2, green, -1
''	SetLightColor Light_RTS3, green, -1
'	SetLightColor Light_RTS4, green, -1

'	SetLightColor Light_DC6, red, -1
	'SetLightColor Light_WTP2, red, -1
'	SetLightColor Light_WTP4, red, -1

'	SetLightColor Light_DirtyCreature1,red, -1
'	SetLightColor Light_DirtyCreature2,red, -1
'	SetLightColor Light_DirtyCreature3,red, -1
'	SetLightColor Light_DC4,red, -1
'	SetLightColor Light_DC5,red, -1

'	SetLightColor Light_POTP1, blue, -1
'	SetLightColor Light_POTP2, blue, -1
'	SetLightColor Light_POTP3, blue, -1
'	SetLightColor Light_POTP3, blue, -1

'	SetLightColor Light_GR, yellow, -1
'	SetLightColor Light_KickerGR, yellow, -1
'	SetLightColor Light_KickerTarget, red, -1
'	SetLightColor Light_DC5, red, -1
End Sub

Sub UpdateBonusColors
End Sub





'Sub RotateLaneLightsLeft
'		Dim TempState
'		'flipper lanes
'		TempState = ll1.State
'		ll1.State = ll2.State
'		ll2.State = ll3.State
'		ll3.State = ll4.State
'		ll4.State = ll5.State
'		ll5.State = TempState
'		ll8.state = ll1.state
'		ll7.state = ll2.state
'		ll6.state = ll3.state
'		ll9.state = ll4.state
'		ll20.state = ll5.state
'	End Sub

'	Sub RotateLaneLightsRight
'		Dim TempState
'		'flipperlanes
'		TempState = ll5.State
'		ll5.State = ll4.State
'		ll4.State = ll3.State
'		ll3.State = ll2.State
'		ll2.State = ll1.State
'		ll1.State = TempState
'		ll8.state = ll1.state
'		ll7.state = ll2.state
'		ll6.state = ll3.state
'		ll9.state = ll4.state
'		l20.state = ll5.state
'	End Sub



'*************************
' Rainbow Changing Lights
'*************************

Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

Sub StartRainbow(n)
'	set RainbowLights = n
'	RGBStep = 0
'	RGBFactor = 5
'	rRed = 255
'	rGreen = 0
'	rBlue = 0
'	RainbowTimer.Enabled = 1
End Sub


Sub StopRainbow(n)
'	Dim obj
'	RainbowTimer.Enabled = 0
'	RainbowTimer.Enabled = 0
'	For each obj in RainbowLights
'		SetLightColor obj, "white", 0
'	Next
End Sub


Sub RainbowTimer_Timer 'rainbow led light color changing
'	Dim obj
'	Select Case RGBStep
'		Case 0 'Green
'			rGreen = rGreen + RGBFactor
'			If rGreen > 255 then
'				rGreen = 255
'				RGBStep = 1
'			End If
'		Case 1 'Red
'			rRed = rRed - RGBFactor
'			If rRed < 0 then
'				rRed = 0
'				RGBStep = 2
'			End If
'		Case 2 'Blue
'			rBlue = rBlue + RGBFactor
'			If rBlue > 255 then
'				rBlue = 255
'				RGBStep = 3
'			End If
'		Case 3 'Green
'			rGreen = rGreen - RGBFactor
'			If rGreen < 0 then
'				rGreen = 0
'				RGBStep = 4
'			End If
'		Case 4 'Red
'			rRed = rRed + RGBFactor
'			If rRed > 255 then
'				rRed = 255
'				RGBStep = 5
'			End If
'		Case 5 'Blue
'			rBlue = rBlue - RGBFactor
'			If rBlue < 0 then
'				rBlue = 0
'				RGBStep = 0
'			End If
'	End Select
'	For each obj in RainbowLights
'		obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
'		obj.colorfull = RGB(rRed, rGreen, rBlue)
'	Next
End Sub

Sub RainbowTimer1_Timer 'rainbow led light color changing
'	Dim obj
'	Select Case RGBStep2
'		Case 0 'Green
'			rGreen2 = rGreen2 + RGBFactor2
'			If rGreen2 > 255 then
'				rGreen2 = 255
'				RGBStep2 = 1
'			End If
'		Case 1 'Red
'			rRed2 = rRed2 - RGBFactor2
'			If rRed2 < 0 then
'				rRed2 = 0
'				RGBStep2 = 2
'			End If
'		Case 2 'Blue
'			rBlue2 = rBlue2 + RGBFactor2
'			If rBlue2 > 255 then
'				rBlue2 = 255
'				RGBStep2 = 3
'			End If
'		Case 3 'Green
'			rGreen2 = rGreen2 - RGBFactor2
'			If rGreen2 < 0 then
'				rGreen2 = 0
'				RGBStep2 = 4
'			End If
'		Case 4 'Red
'			rRed2 = rRed2 + RGBFactor2
'			If rRed2 > 255 then
'				rRed2 = 255
'				RGBStep2 = 5
'			End If
'		Case 5 'Blue
'			rBlue2 = rBlue2 - RGBFactor2
'			If rBlue2 < 0 then
'				rBlue2 = 0
'				RGBStep2 = 0
'			End If
'	End Select
'	For each obj in RainbowLights2
'		obj.color = RGB(rRed2 \ 10, rGreen2 \ 10, rBlue2 \ 10)
'		obj.colorfull = RGB(rRed2, rGreen2, rBlue2)
'	Next
End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo

	If bGameInPLay = True Then: vpmtimer.addtimer 2200,  "Player1Now'": Exit Sub


	Dim tmp
	'info goes in a loop only stopped by the credits and the startkey
	If Score(1)Then
		DMD CL(0, "LAST SCORE"), CL(1, "PLAYER1 " &FormatScore(Score(1))), "", eNone, eNone, eNone, 10000, False, ""
	End If
	If Score(2)Then
		DMD CL(0, "LAST SCORE"), CL(1, "PLAYER2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 10000, False, ""
	End If
	If Score(3)Then
		DMD CL(0, "LAST SCORE"), CL(1, "PLAYER3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 10000, False, ""
	End If
	If Score(4)Then
		DMD CL(0, "LAST SCORE"), CL(1, "PLAYER4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 10000, False, ""
	End If
	DMD "", CL(1, "GAME OVER"), "", eNone, eBlink, eNone, 700, False, ""
	If bFreePlay Then
		DMD CL(0, "FREE PLAY"), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
	Else
		If Credits > 0 Then
			DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 400, False, ""
		Else
			DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
		End If
		If bGameInPlay=True Then:DMDFlush:DMD "", "", "bkborder", eNone, eNone, eNone, 100, False, "":vpmtimer.addtimer 2200,"Player1Now'"
		End If 


	DMD "", "", "DMD_Blank", eNone, eNone, eNone, 100, False, "" 'blank
	DMD "", "", "DMD_Select", eNone, eBlink, eNone, 8000, False, "" 
	DMD "", "", "DMD_Intro1", eNone, eBlink, eNone, 500, False, "" 
	DMD "", "", "DMD_Intro2", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro3", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro4", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro5", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro6", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro7", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro8", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro9", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro10", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro11", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro12", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro13", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro14", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro15", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro16", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro17", eNone, eBlink, eNone, 2800, False, "" 
	DMD "", "", "DMD_Intro17a", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_Intro17b", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_Intro17c", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_Intro17d", eNone, eBlink, eNone, 75, False, ""
	DMD "", "", "DMD_Intro18", eNone, eBlink, eNone, 2000, False, "" 
	DMD "", "", "DMD_Intro19", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro20", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro21", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro22", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro23", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro24", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro25", eNone, eBlink, eNone, 100, False, "" 
	DMD "", "", "DMD_Intro26", eNone, eBlink, eNone, 4000, False, "" 

	DMD "", "", "DMD_RUSSTYT2", eNone, eNone, eNone, 3000, False, "" 
	DMD "", "", "DMD_JPBarbaBlanca", eNone, eNone, eNone, 3000, False, "" 
	DMD "", "", "DMD_AnttiMartikainen", eNone, eNone, eNone, 3000, False, ""
	DMD "", "", "DMD_Turisas", eNone, eNone, eNone, 3000, False, ""
 	DMD "", "", "DMD_AndrePena", eNone, eNone, eNone, 3000, False, ""
	DMD "", "", "DMD_Tomate", eNone, eNone, eNone, 3000, False, ""
	DMD "", "", "DMD_Outhere5", eNone, eNone, eNone, 3000, False, ""
	DMD "", "", "DMD_DaRdog", eNone, eNone, eNone, 3000, False, ""
	DMD "", "", "DMD_Crackers", eNone, eNone, eNone, 3000, False, "" 
	DMD "", "", "DMD_TheBruce2", eNone, eNone, eNone, 3000, False, "" 
	DMD "", "", "DMD_Smaug3", eNone, eNone, eNone, 3000, False, "" 
'	DMD "", "", "DMD_SplitEnz3", eNone, eNone, eNone, 3000, False, ""
	DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
	DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
	DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD Space(dCharsPerLine(0)), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""

End Sub


'**************************************************************************************
'  ATTRACT MODE
'************************************************************************************* 
' 


Sub StartAttractMode()
	PlasticsOnFlasher.Visible=False
	PlaySong "m_HuntingPiratesWaveIntro2"
	vpmTimer.addtimer 1500, " AttractCallout'"
	bAttractMode = True
	StartLightSeq
	ShowTableInfo
	StartRainbow alights
'	SetLightColor Overlay, midgrey, -1
'	OverlayPiratesLife.Visible=True
'	SetLightColor OverlayPiratesLife, darkgrey, -1
	SetLightColor Light_TC2,white, -1:SetLightColor Light_TC3,white, -1:SetLightColor Light_TC4,white, -1
	ApronCharacterTimer.Enabled=True
	SkeltonAttractTimer.Enabled=True
End Sub

Sub AttractCallout
	If 	bGameEnded(CurrentPlayer)=True Then PlaySound "CO_TootlesOzone" : End If
	If 	bGameEnded(CurrentPlayer)=False Then CalloutTimer.Enabled=True:CalloutActive=True:PlaySound "CO_TakeYourPick":End If
End Sub

Sub BirdAttractTimer_Timer
	ParrotMoveUp
	Skeleton1MoveDown:Skeleton2MoveDown:Skeleton3MoveDown:Skeleton4MoveDown
	SkeletonSwirlTimer1.Enabled=False:SpinDiscSkeleton1.Visible=False
	SkeletonSwirlTimer2.Enabled=False:SpinDiscSkeleton2.Visible=False
	SkeletonSwirlTimer3.Enabled=False:SpinDiscSkeleton3.Visible=False
	SkeletonSwirlTimer4.Enabled=False:SpinDiscSkeleton4.Visible=False
	SharkAttractTimer.Enabled=True:SharkAttackTimer.Enabled=False:SpinDiscShark.Visible=False
	BirdAttractTimer.Enabled=False	
	SkeltonAttractTimer.Enabled=False
End Sub

Sub SharkAttractTimer_Timer
	Shark1MoveUp:SpinDiscShark.Visible=True:SharkAttackTimer.Enabled=True
	ParrotMoveDown
	SkeltonAttractTimer.Enabled=True
	SharkAttractTimer.Enabled=False
	BirdAttractTimer.Enabled=False
End Sub

Sub SkeltonAttractTimer_Timer
	Shark1Movedown:SpinDiscShark.Visible=False
	Skeleton1MoveUp:Skeleton2MoveUp:Skeleton3MoveUp:Skeleton4MoveUp
	SkeletonSwirlTimer1.Enabled=True:SpinDiscSkeleton1.Visible=True
	SkeletonSwirlTimer2.Enabled=True:SpinDiscSkeleton2.Visible=True
	SkeletonSwirlTimer3.Enabled=True:SpinDiscSkeleton3.Visible=True
	SkeletonSwirlTimer4.Enabled=True:SpinDiscSkeleton4.Visible=True
	SkeltonAttractTimer.Enabled=False
	SharkAttractTimer.Enabled=False:SharkAttackTimer.Enabled=False:SpinDiscShark.Visible=False
	BirdAttractTimer.Enabled=True
End Sub




Sub StopAttractMode()
	PlasticsOnFlasher.Visible=True
'	SetLightColor Light_TC2,yellow, -1:SetLightColor Light_TC3,yellow, -1:SetLightColor Light_TC4,yellow, -1
	bAttractMode = False
	If Hard=0 Then 
		DMDFlush
		DMD "", "", "DMD_Blank", eNone, eNone, eNone, 100, False, "" 'blank
		DMD CL(0, "    EASY MODE " ), CL(1, ""), "", eNone, eNone, eNone, 1500, True, ""

	End If
	If Hard=1 Then 
		DMDFlush
		DMD "", "", "DMD_Blank", eNone, eNone, eNone, 100, False, "" 'blank
		DMD CL(0, "    HARD MODE " ), CL(1, ""), "", eNone, eNone, eNone, 1500, True, ""

	End If
	bAttractMode = False

	LightSeqAttract.StopPlay
	LightSeqAttract2.StopPlay
	'-----------------------------------
	StopRainbow alights         
	'--------------------------------
	ResetAllLightsColor
'	SetLightColor Overlay, midgrey, -1
'	SetLightColor PiratesLife, lightgrey, -1
'	OverlayPiratesLife.Visible=False
	StopApronShipAttract
	SkeltonAttractTimer.Enabled=False
	BirdAttractTimer.Enabled=False
	SharkAttractTimer.Enabled=False
	Skeleton1MoveDown:Skeleton2MoveDown:Skeleton3MoveDown:Skeleton4MoveDown
	SkeletonSwirlTimer1.Enabled=False:SpinDiscSkeleton1.Visible=False
	SkeletonSwirlTimer2.Enabled=False:SpinDiscSkeleton2.Visible=False
	SkeletonSwirlTimer3.Enabled=False:SpinDiscSkeleton3.Visible=False
	SkeletonSwirlTimer4.Enabled=False:SpinDiscSkeleton4.Visible=False
	ParrotMoveDown
	Shark1Movedown:SpinDiscShark.Visible=False
End Sub


Dim ChangeCharacter
Sub ApronCharacterTimer_Timer
	ChangeCharacter=ChangeCharacter+1
	Select Case ChangeCharacter
		Case 1 ShipPickles.Visible=True: ApronOverlayStarting.Visible=True:Ship8.Visible=False: ApronOverlayLadyTeaghan.Visible=False
		Case 2 ShipBarbaBlanca.Visible=True: ApronOverlayBarbaBlanca.Visible=True:ShipPickles.Visible=False: ApronOverlayStarting.Visible=False
		Case 3 Ship1.Visible=True: ApronOverlayMolly.Visible=True:ApronOverlayBarbaBlanca.Visible=False:ShipBarbaBlanca.Visible=False
		Case 4 Ship2.Visible=True: ApronOverlayPrinceAndres.Visible=True:Ship1.Visible=False:ApronOverlayMolly.Visible=False
		Case 5 Ship3.Visible=True: ApronOverlayCrackers.Visible=True:Ship2.Visible=False: ApronOverlayPrinceAndres.Visible=False
		Case 6 Ship4.Visible=True: ApronOverlayTheBruce.Visible=True:Ship3.Visible=False: ApronOverlayCrackers.Visible=False
		Case 7 Ship5.Visible=True: ApronOverlayFrankie.Visible=True:Ship4.Visible=False: ApronOverlayTheBruce.Visible=False
		Case 8 Ship6.Visible=True: ApronOverlaySmaug.Visible=True:Ship5.Visible=False: ApronOverlayFrankie.Visible=False
		Case 9 Ship7.Visible=True: ApronOverlayJoeSoap.Visible=True:Ship6.Visible=False: ApronOverlaySmaug.Visible=False
		Case 10 Ship8.Visible=True: ApronOverlayMadCatMick.Visible=True:Ship7.Visible=False: ApronOverlayJoeSoap.Visible=False
		Case 11 ShipScurvyWave.Visible=True: ApronOverlayScurvyDick.Visible=True:Ship8.Visible=False: ApronOverlayMadCatMick.Visible=False
		Case 12 Ship2.Visible=True: ApronOverlayPrincessAndrea.Visible=True:ShipScurvyWave.Visible=False: ApronOverlayScurvyDick.Visible=False
		Case 13 Ship8.Visible=True: ApronOverlayLadyTeaghan.Visible=True:Ship2.Visible=False: ApronOverlayPrincessAndrea.Visible=False:ChangeCharacter=0
	End Select
End Sub

Sub StopApronShipAttract
	ApronCharacterTimer.Enabled=False
	Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
	ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
	ApronOverlayStarting.Visible=1:ApronOverlayPrincessAndrea.Visible=0:ApronOverlayLadyTeaghan.Visible=0:ApronOverlayScurvyDick.Visible=0
	ApronOverlayMadCatMick.Visible=0:ApronOverlayJoeSoap.Visible=0:ApronOverlayFrankie.Visible=0:ApronOverlayTheBruce.Visible=0
	ApronOverlayCrackers.Visible=0:ApronOverlayPrinceAndres.Visible=0:ApronOverlayMolly.Visible=0:ApronOverlaySmaug.Visible=0:ApronOverlayBarbaBlanca.Visible=0
End Sub

Sub StartLightSeq()

	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqUpOn, 30, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqDownOn, 25, 1

	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqStripe1VertOn, 15, 2
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqStripe2VertOn, 10, 2
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqCircleOutOn, 15, 2

	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 15
	LightSeqAttract.Play SeqCircleOutOn, 15, 3
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqRightOn, 50, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqLeftOn, 35, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqRightOn, 50, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqLeftOn, 40, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqRightOn, 40, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqLeftOn, 20, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqRightOn, 40, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqCircleOutOn, 15, 3
	LightSeqAttract.UpdateInterval = 100
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 100
	LightSeqAttract.Play SeqRightOn, 25, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.Play SeqStripe2VertOn, 50, 2
	LightSeqAttract.UpdateInterval = 20

	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqStripe1VertOn, 50, 2
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqStripe1VertOn, 50, 3
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqRightOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqStripe2VertOn, 50, 3
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqRightOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqStripe1VertOn, 25, 3
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqStripe2VertOn, 25, 3
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqUpOn, 15, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqDownOn, 15, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqRightOn, 15, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqLeftOn, 15, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqRightOn, 15, 1
	LightSeqAttract.UpdateInterval = 20
	LightSeqAttract.Play SeqLeftOn, 15, 1

End Sub

Sub LightSeqAttract_PlayDone()
	StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
	LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqSkillshot_PlayDone()
	LightSeqSkillshot.Play SeqAllOff
End Sub

Sub StartGoldenReefAwardSequence
	ReefAwardLightsSequence.UpdateInterval = 10
	ReefAwardLightsSequence.Play SeqBlinking,, 14,100
End Sub

Sub StartSinkShipSequence
	ReefAwardLightsSequence.UpdateInterval = 10
	ReefAwardLightsSequence.Play SeqCircleOutOn, 15, 6
	ReefAwardLightsSequence.UpdateInterval = 10
	ReefAwardLightsSequence.Play SeqUpOn, 15, 2
End Sub

Sub StartOfGameLightSequence
	GameStartLightsSequence.UpdateInterval = 10
	GameStartLightsSequence.Play SeqUpOn, 15, 8
End Sub
'***********************************************
'Flashers
'***************************************************
' generic award flasher
dim AwardFlash:AwardFlash=0
Sub AwardFlasherTimer_Timer
	AwardFlash=AwardFlash+1
	Select Case AwardFlash
		case 1: AwardFlasher.visible = 1
		case 3:	AwardFlasher.visible = 0
		case 5:	AwardFlasher.visible = 1
		case 7:	AwardFlasher.visible = 0
		case 9:	AwardFlasher.visible = 1
		case 11:AwardFlasher.visible = 0
		case 13:AwardFlasher.visible = 1
		case 15:AwardFlasher.visible = 0
		case 17:AwardFlasher.visible = 1
		case 19:AwardFlasher.visible = 0
		case 21:AwardFlasher.visible = 1
		case 23:AwardFlasher.visible = 0
		case 25:AwardFlasher.visible = 1
		case 26:AwardFlasher.visible = 0:AwardFlasherTimer.Enabled=0:AwardFlash=0
	End Select
End Sub	

dim SmallAwardFlash:SmallAwardFlash=0
Sub SmallAwardFlasherTimer_Timer
	SmallAwardFlash=SmallAwardFlash+1
	Select Case SmallAwardFlash
		case 1: AwardFlasher.visible = 1
		case 3:	AwardFlasher.visible = 0
		case 5:	AwardFlasher.visible = 1
		case 7:	AwardFlasher.visible = 0
		case 9:	AwardFlasher.visible = 1
		case 11:AwardFlasher.visible = 0
		case 13:AwardFlasher.visible = 1
		case 15: AwardFlasher.visible = 0:SmallAwardFlasherTimer.Enabled=0:SmallAwardFlash=0
	End Select
End Sub	

dim SinkShipFlash:SinkShipFlash=0
Sub SinkShipFlasherTimer_Timer
	SinkShipFlash=SinkShipFlash+1
	Select Case SinkShipFlash
		case 1: OverlayShipSink.visible = 1
		case 3:	OverlayShipSink.visible = 0
		case 5:	OverlayShipSink.visible = 1
		case 7:	OverlayShipSink.visible = 0
		case 9:	OverlayShipSink.visible = 1
		case 11:OverlayShipSink.visible = 0
		case 13:OverlayShipSink.visible = 1
		case 15:OverlayShipSink.visible = 0
		case 17:OverlayShipSink.visible = 1
		case 19:OverlayShipSink.visible = 0
		case 21:OverlayShipSink.visible = 1
		case 23:OverlayShipSink.visible = 0:SinkShipFlasherTimer.Enabled=0:SinkShipFlash=0
	End Select
End Sub







'HelloPolly Award Flasher
dim ParrotFlash:ParrotFlash=0
Sub ParrotFlasherTimer_Timer
	ParrotFlash=ParrotFlash+1
	Select Case ParrotFlash
		case 1: ParrotFlasher.visible = 1
		case 3:	ParrotFlasher.visible = 0
		case 5:	ParrotFlasher.visible = 1
		case 7:	ParrotFlasher.visible = 0
		case 9:	ParrotFlasher.visible = 1
		case 11:ParrotFlasher.visible = 0
		case 13:ParrotFlasher.visible = 1
		case 15:ParrotFlasher.visible = 0
		case 17:ParrotFlasher.visible = 1
		case 19:ParrotFlasher.visible = 0
		case 21:ParrotFlasher.visible = 1
		case 23:ParrotFlasher.visible = 0
		case 25:ParrotFlasher.visible = 1
		case 27:ParrotFlasher.visible = 0
		case 29:ParrotFlasher.visible = 1
		case 31:ParrotFlasher.visible = 0:ParrotFlasherTimer.Enabled=0:ParrotFlash=0
	End Select
End Sub

'TreasureFlashers
dim TreasureFlash:TreasureFlash=0
Sub TreasureFlasherTimer_Timer
	TreasureFlash=TreasureFlash+1
	Select Case TreasureFlash
		case 1: OverlayTreasureFlasher.visible = 1
		case 3:	OverlayTreasureFlasher.visible = 0
		case 5:	OverlayTreasureFlasher.visible = 1
		case 7:	OverlayTreasureFlasher.visible = 0
		case 9:	OverlayTreasureFlasher.visible = 1
		case 11:OverlayTreasureFlasher.visible = 0
		case 13:OverlayTreasureFlasher.visible = 1
		case 15:OverlayTreasureFlasher.visible = 0
		case 17:OverlayTreasureFlasher.visible = 1
		case 19:OverlayTreasureFlasher.visible = 0
		case 21:OverlayTreasureFlasher.visible = 1
		case 23:OverlayTreasureFlasher.visible = 0
		case 25:OverlayTreasureFlasher.visible = 1
		case 27:OverlayTreasureFlasher.visible = 0
		case 29:OverlayTreasureFlasher.visible = 1
		case 31:OverlayTreasureFlasher.visible = 0
		case 33:OverlayTreasureFlasher.visible = 1
		case 35:OverlayTreasureFlasher.visible = 0:TreasureFlasherTimer.Enabled=0:TreasureFlash=0
	End Select
End Sub

'GoldenReefFlashers
dim GoldenReefFlash:GoldenReefFlash=0
Sub GoldenReefFlasherTimer_Timer
	GoldenReefFlash=GoldenReefFlash+1
	Select Case GoldenReefFlash
		case 1: OverlayTreasureFlasher.visible = 1
		case 3:	OverlayTreasureFlasher.visible = 0
		case 5:	OverlayTreasureFlasher.visible = 1
		case 7:	OverlayTreasureFlasher.visible = 0
		case 9:	OverlayTreasureFlasher.visible = 1
		case 11:OverlayTreasureFlasher.visible = 0
		case 13:OverlayTreasureFlasher.visible = 1
		case 15:OverlayTreasureFlasher.visible = 0:GoldenReefFlasherTimer.Enabled=0:GoldenReefFlash=0
	End Select
End Sub
'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

Sub Player1Now
	DMD "", CL(1, "PLAYER 1 BALL 1 " ), "", eNone, eNone, eNone, 3000, True, ""
End Sub




Sub Game_Init() 'called at the start of a new game
	Dim i, j
	bExtraBallWonThisBall = False
	PlaySong "m_HymnOfTheHighSeas-7dB"
		If CalloutActive=False Then PlaySound"CO_RaiseTheSails":CalloutActive=True: CalloutTimer.Enabled=True:End If
		StartOfGameLightSequence
		SinkShipFlasherTimer.Enabled=1
	For i = 0 to 4
		SkillshotValue(i) = 1000000 ' increases by 1000000 each time it is collected
		NewSong=0
		ShipsSunk(i)=0
		ModesCompleted(i)=0
		BonusMultiplier(i)=0
		TreasureKickerAwardCount (i)=0
		TreasureChestCompleted(i)=0
		ShipNow(i)=0
		ShipSink(i)=0
		ShipSunkCount(i)=0
		ShipBonusCount(i)=0
		BBCount(i)=0
		bGameEnded(i)=0
		DirtyCreatureComplete(i)=0
		SharkComplete(i)=0
		TavernComplete(i)=0
		WildSeasComplete (i)=0
		PollyComplete(i)=0
		RaiseTheSailsComplete(i)=0
		WalkThePlankComplete(i)=0
		TreasureComplete(i)=0
		PlankAward1Active(i)=0
		PlankAward2Active(i)=0
		PlankAward3Active(i)=0
		PlankCount(i)=0
		Ship1Now(i)=0
		Ship3Now(i)=0
		Ship4Now(i)=0
		Ship5Now(i)=0
		Ship6Now(i)=0
		Ship7Now(i)=0
		Ship8Now(i)=0
		Ship9Now(i)=0
		Ship1Sunk(i)=0
		Ship3Sunk(i)=0
		Ship4Sunk(i)=0
		Ship5Sunk(i)=0
		Ship6Sunk(i)=0
		Ship7Sunk(i)=0
		Ship8Sunk(i)=0
		Ship9Sunk(i)=0
		Ship1Visible (i)=0
		Ship2Visible (i)=0
		Ship3Visible (i)=0
		Ship5Visible (i)=0
		Ship6Visible (i)=0
		Ship8Visible (i)=0
		Ship9Visible (i)=0
		ScurvyDickActive (i)=0
		ShipsComplete(i)=0
	ShipCount(i)=0
	Next
	lrflashtime.Enabled = False
	bExtraBallWonThisBall = False
	MechTilt = 0
	bMechTiltJustHit = False
	bFlippersEnabled = True
	cFlipperPressed=False
	StopAttractMode
	ResetStartofGameVariables()
	TurnOnStartOfGameLights


End Sub

'*****************************************************Not sure how to do this
Dim Hard
Hard=0 'default

Sub SelectDifficutly(easy_hard)
	Select Case easy_hard
		Case 1: Hard=0
		Case 2: Hard=1
	End Select
End Sub

'***********************************************************************************************************************

Sub StopEndOfBallMode() 'this sub is called after the last ball is drained
'	If bExtraBallWonThisBall = True Then:Exit Sub
	SaveLightStates
	clearlights
End Sub


Sub ResetNewBallVariables()
	ResetNewBallLights
	vpmtimer.addtimer 500, "ResetModesAndAwards'"	
	bFlippersEnabled = True
'	If ShipSink(CurrentPlayer)= 0 Then ShipSunkCount(CurrentPlayer)=ShipSunkCount(CurrentPlayer)-1:debug.print " Subtract1 from ShipSunkCount"
'	If bOnTheFirstBall=True Then ShipSunkCount(CurrentPlayer)=0
	ShipNowSetForNewBallReset
	ChangeSong

End Sub

Sub ResetNewBallLights()                                 'turn on or off the needed lights before a new ball is released
		LoadLightStates'LoadLightStates 'ensure the multiplier is displayed right
		Light_TC2.State=0:Light_TC3.State=0:Light_TC4.State=0
		Light_Patch.State=0:Light_Hook.State=0:Light_Leg.State=0
		Light_RoundTheHorn.State=0:Light_WTP2.State=0:Light_ExtraBall.State=0
		SetLightColor Light_TC2,white, -1:SetLightColor Light_TC3,white, -1:SetLightColor Light_TC4,white, -1
		Light_POTP1.State=0:Light_POTP2.State=0:Light_POTP3.State=0
		gi46.State=0:gi46C.State=2:gi30.State=1
		Light_DirtyCreature1.State=2:Light_DirtyCreature2.State=0:Light_DirtyCreature3.State=0
End Sub

Sub ResetStartofGameVariables()
	GiOn
	kickbacklg.open=True
	kickbackrg.open=True
	vpmTimer.AddTimer 500, "ResetModesAndAwards'"
	vpmTimer.AddTimer 8000, "RandomShipSelect'"
	WizardModeReady=False
	bBallSaverReady = True	
	StartWhirlpoool
	ResetATCTargets
	ResetTreasureTargets
	DropPlungerDiversionWall
	SharkyBoy1.Visible=False
	OutLaneCall=0:LCCallout=0:AngryParrotCall=0:DCHitCallout=0:NumCreatureHits=0
	Skeleton1HitCount=0:Skeleton2HitCount=0:Skeleton3HitCount=0:Skeleton4HitCount=0
	Skeleton1Complete=0:Skeleton2Complete=0:Skeleton3Complete=0:Skeleton4Complete=0
	ChickenTH=0:CalloutTimer.Enabled=False:CalloutActive=False
	BonusMultiplierActive(CurrentPlayer)=0
	ShipSunkCount(CurrentPlayer)=0
	ShipBonusCount(CurrentPlayer)=0
	SelectPlankAward1
	WildSeasPart1=False:WildSeasPart2=False
	TreasureCount=0
	NoPlay=0
	SaverWall.IsDropped=True
	StopShipFires
	PopValue0=1:PopValue1=0:PopValue2=0:PopValue3=0
End Sub

'Sub StartingShip
'	Ship1Now=True
'	Ship1.Visible=True:Ship2.Visible=False:Ship3.Visible=False:Ship4.Visible=False:Ship5.Visible=False:Ship6.Visible=False:Ship7.Visible=False:Ship8.Visible=False
'	ApronOverlayMolly.Visible=False:ApronOverlayStarting.Visible=True
'End Sub

Sub ResetModesAndAwards
	TreasureSpinnerWall.IsDropped=True
	DeactivateWalkThePlank
	ResetTreasureTargets
	If PollyComplete(CurrentPlayer)=False Then LightAward1.State=0	
	If TreasureComplete(CurrentPlayer)=False Then LightAward2.State=0	
	If DirtyCreatureComplete(CurrentPlayer)=False Then LightAward3.State=0	
	If SharkComplete(CurrentPlayer)=False Then LightAward4.State=0
	If WildSeasComplete(CurrentPlayer)=False Then LightAward5.State=0
	If TavernComplete(CurrentPlayer)=False Then LightAward6.State=0
	If RaiseTheSailsComplete(CurrentPlayer)=False Then LightAward7.State=0
	If WalkThePlankComplete(CurrentPlayer)=False Then LightAward8.State=0
	If DirtyCreatureActive= True Then StopDirtyCreature:DirtyCreatureComplete(CurrentPlayer)=False
	If SharkAttackActive=True Then StopAward1:SharkComplete(CurrentPlayer)=False
	If WildSeasActive=True Then ResetWS::RaiseTheSailsComplete(CurrentPlayer)=False
	If LastDrinksActive =True Then StopAward3:TavernComplete(CurrentPlayer)=False
	If HelloPollyActive=True Then StopAward4:PollyComplete(CurrentPlayer)=False
	If RTSActive=True Then StopAward5:RaiseTheSailsComplete(CurrentPlayer)=False
	If DaveyJonesActive=1 Then StopDaveyJones
	If 	BarbaBlancaActive=1 Then BarbaBlancaStop:End If

		WildSeasFinished=1
		ResetATCTargets	
		StopRoundTheHorn:debug.print "StopRoundTheHorn"
		BallFinished=False
		ArmCanonSkillShot=False	
	BarbaBlancaMultiballActive=0
End Sub

Sub TurnOffPlayfieldLights()
	Dim a
	For each a in aLights
		a.State = 0
	Next
End Sub

Sub TurnOnStartOfGameLights
	LightLeftInlane.State=1:LightRightInlane.State=1:LightLeftEscape.State=2:Lightrightescape.State=2  'Turn on in lane lights to activate kickbacks
	l_2x.State=2:l_3x.State=0:l_4x.State=0:l_5x.State=0:l_10x.State=0:l_20x.State=0	'Set Bonus lights
'	l23A.State=2	'Set Light Wheel
	l23B.State=0:l23C.State=0:l23D.State=0:l23E.State=0:l23F.State=0:l23G.State=0:l23H.State=0
	l1.State=2:	l2.State=2:	l3.State=2	'Set BonusTargetLights
	lv1.State=2:lv2.State=0:lv3.State=0:lv4.State=0 ' set Vari Target
	Light_KickerTarget.State=1:Light_KickerGR.State=1 'Turn on kicker Lights
	StartAwardLights
	Light_LoadCanon1.State=2:Light_LoadCanon2.State=0:Light_LoadCanon3.State=0:Light_LoadCanon.State=0:
	StartVariArrowLights:Light_VariTarget.State=1
	Light_DC5.State=1
	Light_DirtyCreature1.State=2:Light_DirtyCreature2.State=0:Light_DirtyCreature3.State=0
	Light_Treasure.State=0:Light_TreasureActive.State=1:Light_TreasureWall.state=0
	gi14.state=2 'DirtyCreatureKickerGi
	Light_Gull3.State=2:Light_Chicken3.State=2
	Reset_SOG_BonusLights:debug.print "ResetStartofGameBonusLights"
	ResetShipLightsStartOfGame:debug.print "ResetStartofGameShipLights"
	LightBarbaBlanca.State=0
	LightAward1.State=0:LightAward2.State=0:LightAward3.State=0:LightAward4.State=0:LightAward5.State=0:LightAward6.State=0:LightAward7.State=0:LightAward8.State=0
	Light_POTP1.State=0:Light_POTP2.State=0:Light_POTP3.State=0:Light_Patch.State=0:Light_Hook.State=0:Light_Leg.State=0
	Light_TC2.State=0:Light_TC3.State=0:Light_TC4.State=0
	Light_GR.state=0:Light_WTP2.state=0:Light_RoundTheHorn.State=0:Light_ExtraBall.State=0:LightShootAgain.State=0
	l5.State=0:l6.State=0:l7.State=0:l8.State=0:l9.State=0:l10.State=0	
	Light_easyhard.State=1
	Light_DaveyJonesRightEye.State=2:Light_DaveyJonesLefttEye.State=2
	LightPlankReady.State=1
End Sub


Sub UpdateSkillShot() 


End Sub	



'***********************************
'Light Save & restore- Thanks JP
'***********************************
Dim MyLightStates(4,200)

Sub SaveLightStates
	Dim i, tmp
	i = 0
	For each tmp in aLights
		MyLightStates(CurrentPlayer,i) = tmp.State
		i = i + 1
	Next

End Sub

Sub LoadLightStates



	Dim i, tmp
	i = 0
	For each tmp in aLights
		tmp.State = MyLightStates(CurrentPlayer,i)
		i = i + 1
	Next
'	If l38.State=1 Then:StartPlanetChaos : End If

End Sub

Sub clearlights
	dim i
	for each i in aLights
		i.State = 0
	next
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
	For each bulb in GI
		SetLightColor bulb, col,1
	Next
End Sub

'*************************************************************
'GIUpdateTimer is used if captive balls are used in the game. 
'This sub is not used in Pirates Life
'*************************************************************


Sub GIUpdateTimer_Timer
	Dim tmp, obj
	tmp = Getballs
	If UBound(tmp) <> OldGiState Then
		OldGiState = Ubound(tmp)
		If UBound(tmp) = 3 Then 'we have 4 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
			'GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
		Else
			'Gion
		End If
	End If
End Sub
'**************************************************************


Sub GiOn
	Dim bulb 
	Dim obj
	mainRamp.image="mainRamp_ON"
	liftRamp.image="liftRamp_ON"
	atcRamp.image="atcRamp_ON"
	PlaySound "Relay_GI_On"

	For each bulb in GI
		bulb.State = 1

	Next
	'Set GI light clours
'	SetLightColor gi1, 	blue, -1
'	gi1.intensity=2
'	SetLightColor gi2, 	white, -1
'	gi2.intensity=2
'	SetLightColor gi3, 	white, -1
'	gi3.intensity=2
'	SetLightColor gi4, 	blue, -1
'	gi4.intensity=6

End Sub

Sub GiOff
	Dim bulb
	Dim obj
	mainRamp.image="mainRamp_OFF"
	liftRamp.image="liftRamp_OFF"
	atcRamp.image="atcRamp_OFF"
	PlaySound "Relay_GI_Off"

	For each bulb in GI
		'			bulb.State = 0
	Next
End Sub


' GI & light sequence effects


Sub GiEffect(n)
	Select Case n
		Case 0 all off
			LightSeqGi.Play SeqAlloff
		Case 1 'all blink
			LightSeqGi.UpdateInterval = 4
			LightSeqGi.Play SeqBlinking, , 5, 100
		Case 2 random
			LightSeqGi.UpdateInterval = 10
			LightSeqGi.Play SeqRandom, 5, , 1000
		Case 3 upon
			LightSeqGi.UpdateInterval = 4
			LightSeqGi.Play SeqUpOn, 5, 1
		Case 4  left-right-left
			LightSeqGi.UpdateInterval = 5
			LightSeqGi.Play SeqLeftOn, 10, 1
			LightSeqGi.UpdateInterval = 5
			LightSeqGi.Play SeqRightOn, 10, 1
	End Select
End Sub

Sub LightEffect(n)
	Select Case n
		Case 0  all off
			LightSeqInserts.Play SeqAlloff
		Case 1 all blink
			LightSeqInserts.UpdateInterval = 4
			LightSeqInserts.Play SeqBlinking, , 5, 100
		Case 2 random
			LightSeqInserts.UpdateInterval = 10
			LightSeqInserts.Play SeqRandom, 5, , 1000
		Case 3 upon
			LightSeqInserts.UpdateInterval = 4
			LightSeqInserts.Play SeqUpOn, 10, 1
		Case 4  left-right-left
			LightSeqInserts.UpdateInterval = 5
			LightSeqInserts.Play SeqLeftOn, 10, 1
			LightSeqInserts.UpdateInterval = 5
			LightSeqInserts.Play SeqRightOn, 10, 1
		Case 5 random
			LightSeqbumper.UpdateInterval = 4
			LightSeqbumper.Play SeqBlinking, , 5, 10
		Case 6 random
			LightSeqRSling.UpdateInterval = 4
			LightSeqRSling.Play SeqBlinking, , 5, 6
		Case 7 random
			LightSeqLSling.UpdateInterval = 4
			LightSeqLSling.Play SeqBlinking, , 5, 6
		Case 8 random
			LightSeqBack.UpdateInterval = 4
			LightSeqBack.Play SeqBlinking, , 5, 6
		Case 12 random
			LightSeqlr.UpdateInterval = 4
			LightSeqlr.Play SeqBlinking, , 5, 10
	End Select
End Sub

' Flasher Effects using lights

'	Dim FEStep, FEffect
'	FEStep = 0
'	FEffect = 0
'
'	Sub FlashEffect(n)
'		Select case n
'			Case 0 ' all off
'				LightSeqFlasher.Play SeqAlloff
'			Case 1 'all blink
'				LightSeqFlasher.UpdateInterval = 4
'				LightSeqFlasher.Play SeqBlinking, , 5, 100
'			Case 2 'random
'				LightSeqFlasher.UpdateInterval = 10
'				LightSeqFlasher.Play SeqRandom, 5, , 1000
'			Case 3 'upon
'				LightSeqFlasher.UpdateInterval = 4
'				LightSeqFlasher.Play SeqUpOn, 10, 1
'			Case 4 ' left-right-left
'				LightSeqFlasher.UpdateInterval = 5
'				LightSeqFlasher.Play SeqLeftOn, 10, 1
'				LightSeqFlasher.UpdateInterval = 5
'				LightSeqFlasher.Play SeqRightOn, 10, 1
'			Case 5 ' top flashers blink fast
'		End Select
'	End Sub







'	'****************************
'	' Flashers - Thanks Flupper
'	'****************************
'
'	Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4, FlashLevel5, FlashLevel6
'	Flasherlight4.IntensityScale = 0
'
'	'*** right red flasher ***
'	Sub Flasherflash3_Timer()
'		dim flashx3, matdim
'		If not Flasherflash3.TimerEnabled Then 
'			Flasherflash3.TimerEnabled = True
'			Flasherflash3.visible = 1
'			Flasherlit3.visible = 1
'		End If
'		flashx3 = FlashLevel3 * FlashLevel3 * FlashLevel3
'		Flasherflash3.opacity = 1500 * flashx3
'		Flasherlit3.BlendDisableLighting = 10 * flashx3
'		Flasherbase3.BlendDisableLighting =  flashx3
'		Flasherlight4.IntensityScale = flashx3
'		matdim = Round(10 * FlashLevel3)
'		Flasherlit3.material = "domelit" & matdim
'		FlashLevel3 = FlashLevel3 * 0.9 - 0.01
'		If FlashLevel3 < 0.15 Then
'			Flasherlit3.visible = 0
'		Else
'			Flasherlit3.visible = 1
'		end If
'		If FlashLevel3 < 0 Then
'			Flasherflash3.TimerEnabled = False
'			Flasherflash3.visible = 0
'		End If
'	End Sub



'****************************
'  SECONDARY HIT EVENTS
'******************************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotRight SlingshotRightSound
	DOF 104, DOFPulse
	RSling.Visible = 0
	RSling1.Visible = 1
	sling1.rotx = 20
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	AddScore 0
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
		Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:gi1.State = 1:Gi2.State = 1:	RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub



Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotLeft SlingshotLeftSound
	DOF 103, DOFPulse
	LSling.Visible = 0
	LSling1.Visible = 1
	sling2.rotx = 20
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	gi3.State = 0:Gi4.State = 0
	AddScore 0
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
		Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0::gi3.State = 1:Gi4.State = 1:LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub







'************************************
'  MAIN SHOTS - PRIMARY HIT EVENTS
'************************************

'*********************************
'  BUMPERS
'**********************************

'*************************
'Bumper1
'*************************

spiralcaps1.enabled = 0
Sub spiralspin1
	spiralcaps1.enabled = 1
End Sub

Sub spiralcaps1_Timer
	BumperCap1.ObjRotZ = (BumperCap1.ObjRotZ - 1) Mod 360
	If BumperCap1.ObjRotZ = -359 Then
		spiralcaps1.enabled = 0
	End If
End Sub

Sub Bumper1_Hit

	If NOT Tilted Then
			DOF 105,DOFPulse
			RandomSoundBumperTop Bumper1
			DOF 304, DOFPulse   'DOF MX - Bumper 3
			LightSeqBumper1.UpdateInterval = 10
			LightSeqBumper1.Play SeqBlinking,, 2, 100
			BumperSequence.enabled = 1
			spiralspin1
		If PopValue0=1 Then AddScore 100: End If
		If PopValue1=1 Then AddScore 1000: End If
		If PopValue2=1 Then AddScore 50000: End If
		If PopValue3=1 Then AddScore 10000: End If
	End If
End Sub

'*************************
'Bumper2
'*************************

spiralcaps2.enabled = 0
Sub spiralspin2
	spiralcaps2.enabled = 1
End Sub

Sub spiralcaps2_Timer
	BumperCap2.ObjRotZ = (BumperCap2.ObjRotZ - 1) Mod 360
	If BumperCap2.ObjRotZ = -359 Then
		spiralcaps2.enabled = 0
	End If
End Sub

Sub Bumper2_Hit
	If NOT Tilted Then
			DOF 106,DOFPulse
			RandomSoundBumperMiddle Bumper2
			LightSeqBumper2.UpdateInterval = 10
			LightSeqBumper2.Play SeqBlinking,, 2, 100
			BumperSequence.enabled = 1
			spiralspin2
		If PopValue0=1 Then AddScore 100: End If
		If PopValue1=1 Then AddScore 1000: End If
		If PopValue2=1 Then AddScore 5000: End If
		If PopValue3=1 Then AddScore 10000: End If

	End If
End Sub

'*************************
'Bumper3
'*************************
spiralcaps3.enabled = 0
Sub spiralspin3
	spiralcaps3.enabled = 1
End Sub

Sub spiralcaps3_Timer
	BumperCap3.ObjRotZ = (BumperCap3.ObjRotZ - 1) Mod 360
	If BumperCap3.ObjRotZ = -359 Then
		spiralcaps3.enabled = 0
	End If
End Sub

Sub Bumper3_Hit

	If NOT Tilted Then
		DOF 107,DOFPulse
			RandomSoundBumperBottom Bumper3
			LightSeqBumper3.UpdateInterval = 10
			LightSeqBumper3.Play SeqBlinking,  2, 100
			BumperSequence.enabled = 1
			spiralspin3
		If PopValue0=1 Then AddScore 100: End If
		If PopValue1=1 Then AddScore 1000: End If
		If PopValue2=1 Then AddScore 5000: End If
		If PopValue3=1 Then AddScore 10000: End If
	End If
End Sub

'*************************
'Bumper4
'*************************
spiralcaps4.enabled = 0
Sub spiralspin4
	spiralcaps4.enabled = 1
End Sub

Sub spiralcaps4_Timer
	BumperCap4.ObjRotZ = (BumperCap4.ObjRotZ - 1) Mod 360
	If BumperCap4.ObjRotZ = -359 Then
		spiralcaps4.enabled = 0
	End If
End Sub

Sub Bumper4_Hit

	If NOT Tilted Then
			DOF 108,DOFPulse
			RandomSoundBumperBottom Bumper4
			LightSeqbumper4.UpdateInterval = 10
			LightSeqBumper4.Play SeqBlinking,  2, 100
			BumperSequence.enabled = 1
			spiralspin4
		If PopValue0=1 Then AddScore 100: End If
		If PopValue1=1 Then AddScore 1000: End If
		If PopValue2=1 Then AddScore 5000: End If
		If PopValue3=1 Then AddScore 10000: End If
	End If
End Sub

spiralcaps5.enabled = 0
Sub spiralspin5
	spiralcaps5.enabled = 1
End Sub

Sub spiralcaps5_Timer
	BumperCap5.ObjRotZ = (BumperCap5.ObjRotZ - 1) Mod 360
	If BumperCap5.ObjRotZ = -359 Then
		spiralcaps5.enabled = 0
	End If
End Sub

Sub Bumper5_Hit

	If NOT Tilted Then
		DOF 109,DOFPulse
			RandomSoundBumperBottom Bumper5
			LightSeqbumper5.UpdateInterval = 10
			LightSeqBumper5.Play SeqBlinking,  2, 100
			BumperSequence.enabled = 1
			spiralspin5
		If PopValue0=1 Then AddScore 100: End If
		If PopValue1=1 Then AddScore 1000: End If
		If PopValue2=1 Then AddScore 5000: End If
		If PopValue3=1 Then AddScore 10000: End If
	End If
End Sub

'*********************************
'BumperOceanFlashersRotate
'***********************************
Sub BumperWashTimer_Timer
	Flasher_Bumper1.rotz = (Flasher_Bumper1.rotz +2)mod 360
	Flasher_Bumper2.rotz = (Flasher_Bumper2.rotz +2)mod 360
	Flasher_Bumper3.rotz = (Flasher_Bumper3.rotz +2)mod 360
	Flasher_Bumper4.rotz = (Flasher_Bumper4.rotz +2)mod 360
	Flasher_Bumper5.rotz = (Flasher_Bumper5.rotz +3)mod 360
End Sub



'************************************
'Canon
'************************************

Sub Trigger_Canon_Hit
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_PowderTheMonkeysLadsOzone":End If
	DMD CL(0, "  POWDER THE"), CL(1, " MONKEYS SAILOR"), "", eBlink, eBlink, eNone, 3000, True, ""

End Sub

Sub Trigger_CanonLoad_Hit
'	ResetATCTargets
End Sub

Dim MyPi, CanonActive, CanonDir, CanonStep
MyPi = Round(4 * Atn(1), 6) / 90
CanonActive = False
CanonStep = 0

Sub KickerCanon_Hit
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_FireAtWillOzone":End If
	DMD CL(0, "  FIRE AT WILL"), CL(1, "  SAILOR"), "", eBlink, eBlink, eNone, 3000, True, ""
	PlaySoundAt "fx_kicker_catch",KickerCanon
     CanonActive = True
     KickerCanon.TimerEnabled = 1 ' to fire the canon if the player does not fire it.
     CanonMove.Enabled = 1 'rotate canon timer
	CanonGuideRight.IsDropped=False:CanonGuideLeft.IsDropped=False
End Sub

Sub KickerCanon_Timer 'auto plunger
    Kickercanon.TimerEnabled = 0
    CanonL1Fire
End Sub

Sub CanonMove_Timer 'rotates the canon left and right
    Dim i
    CanonDir = SIN(CanonStep * MyPi) * 20
    CanonStep = (CanonStep + 1)MOD 360
    For each i in CanonAndBase
    i.RotZ = CanonDir
    Next
End Sub

Dim shotl
Dim CanonJustFired

Sub CanonL1Fire
    KickerCanon.TimerEnabled = 0 'be sure it is turned off
	CanonJustFired=True
    CanonActive = False
    CanonMove.Enabled = 0
	LastSwitchHit = "KickerCanon"
	PlaySoundAt "CanonFire",KickerCanon
    DOF 132 ,2
	CanonFire.Visible=True
	TimeCanonFire.Enabled=True
	KickerCanon.Kick -CanonDir, 80
	vpmtimer.Addtimer 500, "CanonFireFinished'"

End Sub

Sub TimeCanonFire_Timer
	CanonFire.Visible=False
	TimeCanonFire.Enabled=False
End Sub

Sub CanonFireFinished
	CanonFire.Visible=False
	CanonJustFired=False
	CanonGuideRight.IsDropped=True:CanonGuideLeft.IsDropped=True
End Sub

'**********************************************************************************
'Skillshot Kicker destroys ball and creates ball at right outlane portal
'This is acombined Openingshot portal that awards a skillhot if v the light is lit

'***********************************************************************************
'********************
'SkillShot1
'********************

'*************
'  KICKBACKS 
'*************
' 

'kickbackl(CurrentPlayer), kickbackr(CurrentPlayer)

Sub closekickbacks
	kickbacklg.open = False
	LightLeftescape.State = 0
	kickbackrg.open = False
	Lightrightescape.State = 0
	LightLeftInlane.State = 0
	LightRightInlane.State = 0
End Sub

Sub kickbackleftenabled
	kickbacklg.open = True
	LightLeftescape.State = 2
	PlaySoundAt "Kickback2",kickbacklg
End Sub

Sub kickbackleftdisabled
	kickbacklg.open = False
	LightLeftescape.State = 0
	LightLeftInlane.State = 0
End Sub

Sub kickbackrightenabled
	kickbackrg.open = True
	Lightrightescape.State = 2
	PlaySoundAt "Kickback2",kickbackrg
End Sub

Sub kickbackrightdisabled
	kickbackrg.open = False
	Lightrightescape.State = 0
	LightRightInlane.State = 0
End Sub

Sub Kicker11_hit
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_WaitForItOzone"::End If
	DMD CL(0, "FIRE IN THE HOLE"), CL(1, ""), "", eBlink, eNone, eNone, 3000, True, ""
	AddScore 10
	PlaySoundAt SoundFXDOF("Popper", 152, DOFPulse, DOFContactors), Kicker11
	vpmtimer.addtimer 3500, "LeftKickBack'"	
End Sub

Sub LeftKickBack
	PlaySoundAt SoundFXDOF("Popper", 118, DOFPulse, DOFContactors), Kicker11
	Kicker11.Kick 0, 35
	PlaySound "Canon1":AddScore 10
	CanonFireLeftKickBack.Visible=True
	LeftKBCanonStoptimer.Enabled=True
	vpmtimer.addtimer 300, "kickbackleftdisabled'"
	LastSwitchHit = "Kicker11"
End Sub

Sub LeftKBCanonStoptimer_Timer
	CanonFireLeftKickBack.Visible=False
	LeftKBCanonStoptimer.Enabled=False
End Sub

Sub Kicker12_hit 
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_WaitForItOzone"::End If
	DMD CL(0, "FIRE IN THE HOLE"), CL(1, ""), "", eBlink, eNone, eNone, 3000, True, ""
	PlaySoundAt SoundFXDOF("Popper", 151, DOFPulse, DOFContactors), Kicker12
	vpmtimer.addtimer 3500, "RightKickBack'"	
End Sub

Sub RightKickBack
	PlaySound "Canon&Hit":AddScore 10
	PlaySoundAt SoundFXDOF("Popper", 114, DOFPulse, DOFContactors), Kicker12
	Kicker12.Kick 0, 35
	PlaySound "Canon2":AddScore 10
	CanonFireRightKickback.Visible=True
	RightKBCanonStopTimer.Enabled=True
	vpmtimer.addtimer 300, "kickbackrightdisabled'"	
	LastSwitchHit = "Kicker12"
End Sub

Sub RightKBCanonStoptimer_Timer
	CanonFireRightKickback.Visible=False
	RightKBCanonStopTimer.Enabled=False
End Sub

'***********************************
'Lanes
'*************************************


Sub lane1_hit
	If	LastSwitchHit = "Kicker11" Then:Exit Sub
	If LightLeftEscape.State=0 Then:OutLaneLostBall:AddScore 2000:End If
	If Tilted Then Exit Sub
	LastSwitchHit = "lane1"
'	LightLeftEscape.State=0
'	LightLeftInlane.State = 0
End Sub

Dim OutLaneCall
Sub OutLaneLostBall
	OutLaneCall=OutLaneCall+1
Select Case OutLaneCall

    Case 1: If CalloutActive=False Then CalloutActive=True:PlaySound "CO_OopsManOverboardOzone":CalloutTimer.Enabled=True: debug.print "OutlaneHit1":End If

    Case 2: If CalloutActive=False Then CalloutActive=True:PlaySound "CO_YouCouldTryOzone":CalloutTimer.Enabled=True: debug.print "OutlaneHit2": End If

    Case 3: If CalloutActive=False Then CalloutActive=True:PlaySound "CO_GiveThatBackWet2":CalloutTimer.Enabled=True: debug.print "OutlaneHit3": End If

    Case 4:	If CalloutActive=False Then CalloutActive=True:PlaySound "CO_ManOverboardOzone" :CalloutTimer.Enabled=True: debug.print "OutlaneHit4": End If 

    Case 5: If CalloutActive=False Then CalloutActive=True:PlaySound "CO_OoooOzone":CalloutTimer.Enabled=True:OutLaneCall=0: debug.print "OutlaneHit5": OutLaneCall=0:End If

End Select
End Sub

Sub TriggerOutlane1KickerCheck_Hit
	LastSwitchHit = "TriggerOutlane1KickerCheck"
End Sub

Sub TriggerOutlane4KickerCheck_Hit
	LastSwitchHit = "TriggerOutlane4KickerCheck"
End Sub


Sub lane2_hit 
	AddScore 50000
	If Tilted Then Exit Sub
	LastSwitchHit = "lane2"
	If 	LightLeftInlane.State = 1 Then: Exit Sub
	LightLeftInlane.State = 1
	LightLeftEscape.State=2
	kickbackleftenabled
End Sub

Sub lane3_hit
	AddScore 50000
	If Tilted Then Exit Sub 
	LastSwitchHit = "lane3"
	If 	LightRightInlane.State = 1 Then: Exit Sub
	LightRightInlane.State = 1
	Lightrightescape.State=2
	kickbackrightenabled
End Sub


Sub lane4_hit 
	If	LastSwitchHit = "Kicker12" Then:Exit Sub
	If Lightrightescape.State=0 Then: OutLaneLostBall:AddScore 2000: End If
	If Tilted Then Exit Sub
	LastSwitchHit = "lane4"
	'		checkforkickbacks
'	Lightrightescape.State=0
'	LightRightInlane.State = 0	
End Sub

'***********************'
'BonusTargerts
'***********************
Sub TargetATC1_Hit
	TargetATC1.isdropped=1
	CoinTarget1.Visible=False
	AddScore 1000
	PlaySoundAT "Drop_Target_Down_1" ,TargetATC1
	Light_LoadCanon1.State=1:	Light_LoadCanon2.State=2
	debug.print "ATC Target1 Down"
End Sub

Sub TargetATC2_Hit
	TargetATC2.isdropped=1
	CoinTarget2.Visible=False
	Light_LoadCanon2.State=1:	Light_LoadCanon3.State=2
	AddScore 1000
	TargetATC2.IsDropped= True 'This drops the target
	PlaySoundAT "Drop_Target_Down_1" ,TargetATC2
	debug.print "ATC Target2 Down"
End Sub

Sub TargetATC3_Hit
	SmallAwardFlasherTimer.Enabled=1
	TargetATC3.isdropped=1
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_YouArmedTheCanon_Ozone":End If
	DMD CL(0, "BONUS MULTIPLIER"), CL(1, "INCREASED"), "", eNone, eNone, eNone, 3000, True, ""
	
	Light_CanonArm.State=2
	CoinTarget3.Visible=False
	Light_LoadCanon3.State=1:	Light_LoadCanon.State=2
	AddScore 1000
	TargetATC3.IsDropped= True 'This drops the target
	PlaySoundAT "Drop_Target_Down_1" ,TargetATC3
	CheckBonusMultiplier
	debug.print "ATC Target3 Down"
End Sub


Sub CheckBonusMultiplier
	BonusMultiplierActive(CurrentPlayer)=1
	BonusCounter(CurrentPlayer)=BonusCounter(CurrentPlayer) + 1
	If BonusCounter(CurrentPlayer) =1 Then :BonusMultiplier(CurrentPlayer)=2: l_2x.State=1:l_3x.State=2: End If
	If BonusCounter(CurrentPlayer) =2 Then :BonusMultiplier(CurrentPlayer)=3: l_3x.State=1:l_4x.State=2: End If
	If BonusCounter(CurrentPlayer) =3 Then :BonusMultiplier(CurrentPlayer)=4: l_4x.State=1:l_5x.State=2: End If
	If BonusCounter(CurrentPlayer) =4 Then :BonusMultiplier(CurrentPlayer)=5: l_5x.State=1:l_10x.State=2: End If
	If BonusCounter(CurrentPlayer) = 5 Then :BonusMultiplier(CurrentPlayer)=10   :l_10x.State=1:l_20x.State=2:  End If
	If BonusCounter(CurrentPlayer) = 6 Then :BonusMultiplier(CurrentPlayer)=20  :l_20x.State=1: End If
	If BonusCounter(CurrentPlayer) > 6 Then :BonusMultiplier(CurrentPlayer)=20  : End If
End Sub

Sub ResetATCTargets
	TargetATC1.IsDropped= False
	TargetATC2.IsDropped= False
	TargetATC3.IsDropped= False
	CoinTarget1.Visible=True
	CoinTarget2.Visible=True
	CoinTarget3.Visible=True
	Light_LoadCanon1.State=2
	Light_LoadCanon2.State=0
	Light_LoadCanon3.State=0
	Light_LoadCanon.State=0
	Light_CanonArm.State=1
	PlaySoundAT "Drop_Target_Reset_1" ,TargetATC3
	debug.print "ResetATCTargets"
End Sub


Sub Reset_SOG_BonusLights
	l_2x.State=2:l_3x.State=0:l_4x.State=0:l_5x.State=0:l_10x.State=0:l_20x.State=0
End Sub

Sub ATCTargetsDown
	PlaySoundAT "Drop_Target_Down_1" ,TargetATC3
	TargetATC1.IsDropped= True
	TargetATC2.IsDropped= True
	TargetATC3.IsDropped= True
	CoinTarget1.Visible=False
	CoinTarget2.Visible=False
	CoinTarget3.Visible=False
	Light_LoadCanon1.State=1
	Light_LoadCanon2.State=1
	Light_LoadCanon3.State=1
	Light_LoadCanon.State=2
	Light_CanonArm.State=2
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_YouArmedTheCanon_Ozone":End If
	CheckBonusMultiplier
	debug.print "Arm the Canon Skillshot.Canon targets down"
End Sub



'****************************************
'StartWheelSpinner'
'****************************************
Dim SpinSave
Sub TriggerDrainSave_Hit
	If SpinSave=False Then
		SpinSave=True
		StartWheelSpinnerDrain
		debug.print "drainSpinStarted"
		spindiscimg_drainsave.Visible=1
		spindiscimg_drainsavefire.Visible=0
'		spinningdraintimer.Enabled=1
	If hard=0 Then
		vpmtimer.addtimer 7000, "DiscInvisible'"
		vpmtimer.addtimer 11000, "StopWheelSpinnerDrain'"
	End If
	If hard=1 Then
		vpmtimer.addtimer 3000, "DiscInvisible'"
		vpmtimer.addtimer 7000, "StopWheelSpinnerDrain'"
	End If

	End If
End Sub

Sub spinning3_timer
	spindiscimg_drainsavefire.TimerEnabled=true
	spindiscimg_drainsave.rotz = spindiscimg_drainsave.rotz - 10
	spindiscimg_drainsavefire.rotz = spindiscimg_drainsavefire.rotz - 10
'	DOF 119,DOFPulse 'shaker on
'	DOF 126,DOFPulse	'Fan on
End Sub

dim spinner3
Set spinner3 = New cvpmTurntable
With spinner3
	.InitTurntable spindisc_drainsave, -150
	.SpinDown = 20
	.CreateEvents "spinner3"
End With
spinner3.MotorOn = false


Sub StartWheelSpinnerDrain	
	spinner3.MotorOn = true
	spinning3.enabled = True
	SaverWall.IsDropped=False
End Sub

Sub StopWheelSpinnerDrain	
	spinner3.MotorOn = False
	spinning3.enabled = False
	SpinSave=False
	SaverWall.IsDropped=True
	spindiscimg_drainsave.Visible=0
	spindiscimg_drainsavefire.Visible=0
	SpinSave=False
End Sub

Sub DiscInvisible
	spindiscimg_drainsave.Visible=0
	spindiscimg_drainsavefire.Visible=1
End Sub

'***********************************************************
'TreasureChest
'***********************************************************
Sub sw3_Dropped
	AddScore 50000
	l1.State=1
	PlaySoundAt "Target_Hit_5", sw3
	CheckLeftTargetsComplete
End Sub

Sub sw4_Dropped
	AddScore 50000
	l2.State=1
	PlaySoundAt "Target_Hit_6", sw4
	CheckLeftTargetsComplete
End Sub

Sub sw5_Dropped
	AddScore 50000
	l3.State=1
	PlaySoundAt "Target_Hit_5", sw5
	CheckLeftTargetsComplete
End Sub

Dim TreasureWallReady
Sub CheckLeftTargetsComplete
	If l1.State +l2.State + l3.State=3 Then
		TreasureWallReady=1
		Light_TreasureActive.State=1:Light_TreasureWall.State=2: gi46.State=2:gi30.State=0:SetLightColor gi46, blue, -1

	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_BlastThatWall":End If
	DMD CL(0, "  BLAST THAT WALL"), CL(1, "  SCURVY DOG"), "", eBlink, eBlink, eNone, 3000, True, ""
		
	End If
End Sub

Sub ResetTreasureTargets
	PlaySoundAT "Drop_Target_Down_1" ,sw4
	sw3.IsDropped= False
	sw4.IsDropped= False
	sw5.IsDropped= False
	l1.State=2
	l2.State=2
	l3.State=2
	WallHiddenTreasure.IsDropped=False
	gi46.State=0:gi46b.State=0:gi46c.State=2:gi30.State=1:Light_Treasure.State=0:Light_TreasureWall.State=0:Light_TreasureActive.State=2
	TreasureChest2Pos = 20
	TreasureChest2MoveDown
End Sub

Sub TreasureTargetsDown
	PlaySoundAT "Drop_Target_Down_1" ,sw4
	sw3.IsDropped= True
	sw4.IsDropped= True
	sw5.IsDropped= True
	l1.State=1
	l2.State=1
	l3.State=1
End Sub

Dim WallHTHit
Sub WallHiddenTreasure_Hit
	If TreasureWallReady=1 Then 
	If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_TreasureFound":End If
	DMD CL(0, "  TREASURE WALL"), CL(1, "IS DROPPED"), "", eBlink, eBlink, eNone, 3000, True, ""
		Light_Treasure.State=2:Light_TreasureWall.State=1
		WallHiddenTreasure.IsDropped=True
		TreasureChest2MoveUp
		SetLightColor gi46, yellow, -1
		gi46b.State=2
		gi46c.State=0
		TreasureWallReady=0
	End If
End Sub


Sub KickerHiddenTreasure_Hit
	TreasureFlasherTimer.Enabled=1
	PlaySoundAt "fx_kicker_catch" ,KickerHiddenTreasure
	vpmtimer.addtimer 3000, "KickerTreasureKick'"
	GiOff 
	If CalloutActive=False Then CalloutActive=True:PlaySound "CO_MotherLoadWet":CalloutTimer.Enabled=True:End If
	vpmtimer.addtimer 5500, "TreasureCheck'"
End Sub

Dim TreasureCount
Sub TreasureCheck
	TreasureCount=TreasureCount+1
	Select Case  TreasureCount
		Case 1: If Hard=1 Then AwardJackpot :End If
				If Hard=0 Then AwardJackpot2 :End If
		Case 2: If Hard=1 Then AwardSuperJackpot :End If
				If Hard=0 Then AwardSuperJackpot :End If
		Case 3:	If Hard=1 Then AwardSuperJackpot2 :End If
				If Hard=0 Then AwardSuperJackpot2:TreasureCount=2 :End If
	End Select
End Sub

Sub KickerTreasureKick
	TreasureKickerAwardCount(CurrentPlayer)= TreasureKickerAwardCount(CurrentPlayer)+1: debug.print "TreasureKickerAwardCount"
'	If TreasureKickerAwardCount(CurrentPlayer)=0 Then:TreasureKickerAwardCount(CurrentPlayer)=1:End If
	PlaySoundAt SoundFX("fx_kicker2" , DOFContactors), KickerHiddenTreasure
	vpmtimer.addtimer 700, "ResetTreasureTargets'"
	KickerHiddenTreasure.DestroyBall
	vpmtimer.addtimer 700, "CreateBallKickerTreasureTrapEscape'": Debug.print "KickerTrapBallDestoryed"
	GiOn
	If TreasureComplete(CurrentPlayer)=False Then LightAward2.State=1: TreasureComplete(CurrentPlayer)=True:CheckModesCompleted:debug.print "CheckModesComplete-atTreasureChest":End If
'	Treasure
	SpinSave=True
	StartWheelSpinnerDrain
	debug.print "drainSpinStarted"
	spindiscimg_drainsave.Visible=1
	spinningdraintimer.Enabled=1
	vpmtimer.addtimer 10000, "DiscInvisible'"
	vpmtimer.addtimer 12000, "StopWheelSpinnerDrain'"
End Sub


Sub CreateBallKickerTreasureTrapEscape
	KickerTreasureTrapEscape.CreateSizedball BallSize / 2: Debug.print "Create ball TreasureEscape"
	KickerTreasureTrapEscape.Enabled=True: Debug.print "KickerTrapRelease Enabled"
	KickerTreasureTrapEscape.kick 70,20
	KickerTreasureTrapEscape.Enabled=False: Debug.print "KickerTrapReleaseDisnabled"
    DOF 134, DOFPulse
	'vpmtimer.addtimer 1000, "TurnOffTreasureFlasher'"
End Sub

Dim TreasureChestBonus
Sub Treasure
	TreasureKickerCount(CurrentPlayer)=TreasureKickerCount(CurrentPlayer)+1
	If BonusMultiplierActive(CurrentPlayer)=0 Then:BonusMultiplier(CurrentPlayer)=1:End If
	TreasureChestBonus=1000000 * BonusMultiplier(CurrentPlayer)*TreasureKickerCount(CurrentPlayer)
	AddScore TreasureChestBonus
    DMD CL(0, FormatScore(TreasureChestBonus)), CL(1, "TREASURE BONUS " & " X" & BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 1500, True, ""
End Sub



'************************
'DirtyCreature
'************************
Dim DirtyCreatureActive
Sub KickerDirtyCreature_Hit
	PlaySoundAt "fx_kicker_catch" , KickerDirtyCreature
	CheckDirtyCreature
	vpmtimer.addtimer 3000, "DirtyCreatureRelease'"
End Sub

Sub DirtyCreatureRelease
	PlaySoundAt SoundFXDOF("fx_kicker2" , 135, DOFPulse, DOFContactors), KickerDirtyCreature
	KickerDirtyCreature.kick 310,50
End Sub

Sub CheckDirtyCreature
'	If 	DirtyCreatureActive=True Then: PlaySound "CO_GetBackOnDeckWet":Exit Sub
	If Light_DirtyCreature3.State=2 And SelectAward=True Then:PlaySound "CO_GetBackOnDeckWet": Exit Sub
	If Light_DirtyCreature3.State=2 And SelectAward=False Then:Light_DirtyCreature3.State=1: DirtyCreatureLoose:End If
	If Light_DirtyCreature2.State=2 Then
			Light_DirtyCreature2.State=1: Light_DirtyCreature3.State=2:
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_IWouldntGoInThereOzone":End If
			DMD CL(0, "THIS IS GETTING"), CL(1, "SPOOKY     "), "", eBlink, eBlink, eNone, 3000, True, ""
	End If 
	If Light_DirtyCreature1.State=2 Then
			Light_DirtyCreature1.State=1: Light_DirtyCreature2.State=2
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_ScumbucketsOzone":End If
			DMD CL(0, "    OOOO IM"), CL(1, "GETTING NERVOUS"), "", eBlink, eBlink, eNone, 3000, True, ""
	End If
	If Light_DirtyCreature1.State=0 Then:Light_DirtyCreature1.State=2
End Sub

Dim DCLoose
Sub DirtyCreatureLoose
	DirtyCreatureActive= True
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_DirtyAndAngryOzone":End If
			DMD CL(0, "  ITS DIRTY"), CL(1, "AND ITS ANGRY"), "", eBlink, eBlink, eNone, 3000, True, ""
	DCLoose=True
	DCHitCallout=0
'	If DirtyCreatureComplete(CurrentPlayer)=False Then  LightAward3.State=2: End If
	PlaySong "m_DangerousTides-6dB"
	OverlayPiratesLifeYellow.visible=True
	en1.Visible=True
	en1Timer.Enabled = 1
	GreenSlime.Visible=1
	gi14.State=1	
	DirtyCreatureEasyStopTimer.Enabled=True 
End Sub

'DirtyCreatureTargets
Sub en1t001_Hit
'	If CalloutActive=False Then CalloutActive=True:PlaySound "CO_DirtyCreatureMultiballOzone":CalloutTimer.Enabled=True: debug.print "DirtyCreatureMultiball":End If 
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en1tHitTimer Enabled"
	en1t001.IsDropped=True
End Sub

Sub en1t002_Hit 
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en2tHitTimer Enabled"
	en1t002.IsDropped=True
End Sub

Sub en1t003_Hit
	DirtyCreatureHitCallout
 	en1hittimer.Enabled=True:debug.print " en3tHitTimer Enabled"
	en1t003.IsDropped=True
End Sub

Sub en1t004_Hit
	DirtyCreatureHitCallout 
	en1hittimer.Enabled=True:debug.print " en4tHitTimer Enabled"
	en1t004.IsDropped=True
End Sub

Sub en1t005_Hit
	DirtyCreatureHitCallout 
	en1hittimer.Enabled=True:debug.print " en5tHitTimer Enabled"
	en1t005.IsDropped=True
End Sub

Sub en1t006_Hit
	DirtyCreatureHitCallout 
	en1hittimer.Enabled=True:debug.print " en6tHitTimer Enabled"
	en1t006.IsDropped=True
End Sub

Sub en1t007_Hit
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en7tHitTimer Enabled" 
	en1t007.IsDropped=True
	AddScore 100000
	DirtyCreatureCount
	debug.print "target7Hit"
End Sub

Sub en1t008_Hit 
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en8tHitTimer Enabled"
	en1t008.IsDropped=True
End Sub

Sub en1t009_Hit 
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en9tHitTimer Enabled"
	en1t009.IsDropped=True
End Sub

Sub en1t010_Hit
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en10HitTimer Enabled" 
	en1t010.IsDropped=True
End Sub

Sub en1t011_Hit 
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en11HitTimer Enabled"
	en1t011.IsDropped=True
End Sub

Sub en1t012_Hit 
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en12HitTimer Enabled"
	en1t012.IsDropped=True
End Sub

Sub en1t013_Hit
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en13HitTimer Enabled" 
	en1t013.IsDropped=True
End Sub

Sub en1t014_Hit
	DirtyCreatureHitCallout 
	en1hittimer.Enabled=True:debug.print " en14HitTimer Enabled"
	en1t014.IsDropped=True
End Sub

Sub en1t015_Hit 
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en15HitTimer Enabled"
	en1t015.IsDropped=True
End Sub

Sub en1t016_Hit
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en16HitTimer Enabled" 
	en1t016.IsDropped=True
End Sub

Sub en1t017_Hit 
	DirtyCreatureHitCallout
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en17HitTimer Enabled"
	en1t017.IsDropped=True
End Sub

Sub en1t018_Hit
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en18tHitTimer Enabled" 
	en1t018.IsDropped=True
End Sub

Sub en1t019_Hit 
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en19HitTimer Enabled"
	en1t019.IsDropped=True
End Sub

Sub en1t020_Hit
	DirtyCreatureHitCallout
	en1hittimer.Enabled=True:debug.print " en20HitTimer Enabled" 
	en1t020.IsDropped=True
End Sub

Sub en1t021_Hit
	DirtyCreatureHitCallout 
'	en1hittimer.Enabled=True:debug.print " en21HitTimer Enabled"
	en1t021.IsDropped=True
End Sub

Sub en1t022_Hit 
	DirtyCreatureHitCallout
'	en1hittimer.Enabled=True:debug.print " en21HitTimer Enabled"
	en1t022.IsDropped=True
End Sub

Sub en1hittimer_timer
'	DirtyCreatureHitCallout 
	AddScore 50000
	DirtyCreatureCount	
	en1hittimer.Enabled=False:debug.print " en*****HitTimer****Disabled"
End Sub


Dim NumCreatureHits
Sub DirtyCreatureCount
	NumCreatureHits=NumCreatureHits+1
	If NumCreatureHits =3 Then 
		vpmtimer.addtimer 2000, "AwardDirtyCreature'"
	End If
End Sub


Sub DropDirtyCreatureWalltargets
	en1t001.IsDropped=True:en1t002.IsDropped=True:en1t003.IsDropped=True:en1t004.IsDropped=True:en1t005.IsDropped=True:en1t006.IsDropped=True:en1t007.IsDropped=True
	en1t008.IsDropped=True:en1t009.IsDropped=True:en1t010.IsDropped=True:en1t011.IsDropped=True:en1t012.IsDropped=True:en1t013.IsDropped=True:en1t014.IsDropped=True
	en1t015.IsDropped=True:en1t016.IsDropped=True:en1t017.IsDropped=True:en1t018.IsDropped=True:en1t019.IsDropped=True:en1t020.IsDropped=True:en1t021.IsDropped=True
	en1t022.IsDropped=True
	en2t001.IsDropped=True:en2t002.IsDropped=True:en2t003.IsDropped=True:en2t004.IsDropped=True:en2t005.IsDropped=True:en2t006.IsDropped=True:en2t007.IsDropped=True
	en2t008.IsDropped=True:en2t009.IsDropped=True:en2t010.IsDropped=True:en2t011.IsDropped=True:en2t012.IsDropped=True:en2t013.IsDropped=True:en2t014.IsDropped=True
	en2t015.IsDropped=True:en2t016.IsDropped=True:en2t017.IsDropped=True:en2t018.IsDropped=True:en2t019.IsDropped=True:en2t020.IsDropped=True:en2t021.IsDropped=True
	en2t022.IsDropped=True
End Sub


Dim DCHitCallout
Sub DirtyCreatureHitCallout
	DCHitCallout=DCHitCallout+1
	Select Case DCHitCallout

    Case 1: PlaySound "CO_DCHit4": debug.print "DirtyCreatureHit1"
				SmallAwardFlasherTimer.Enabled=1
				PlaySound "CO_DirtyCreatureMultiballOzone"
				vpmtimer.addtimer 3000, "DirtyCreatureMultiball'"
			If Hard=1 And bMultiBallMode=False Then 
				DMD CL(0, "DIRTY CREATURE"), CL(1, "MULTIBALL "), "", eBlink, eBlink, eNone, 3000, True, ""
				AddScore 500000
			End If
			If Hard=0 And bMultiBallMode=False Then 
				DMD CL(0, " ITS ANGRY"), CL(1, "750000 "), "", eBlink, eNone, eNone, 3000, True, ""
				AddScore 750000
			End If

    Case 2:		SmallAwardFlasherTimer.Enabled=1	
			If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_BetweenTheEyesOzone": debug.print "DirtyCreatureHit2":End If
				If Hard=1 Then 
				DMD CL(0, "BETWEEN THE EYES"), CL(1, "600000 "), "", eBlink, eNone, eNone, 3000, True, ""
				AddScore 600000
			End If
			If Hard=0 Then 
				DMD CL(0, "BETWEEN THE EYES"), CL(1, "850000 "), "", eBlink, eNone, eNone, 3000, True, ""
				AddScore 850000
			End If

    Case 3: 	SmallAwardFlasherTimer.Enabled=1
			If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_DownTheGobWet2": debug.print "DirtyCreatureHit3":End If
		
			If Hard=1 Then 
				DMD CL(0, "DOWN THE GOB"), CL(1, ""), "", eBlink, eNone, eNone, 3000, True, ""
				vpmtimer.addTimer 5500, "AwardJackpot2'"
			End If
			If Hard=0  Then 
				DMD CL(0, " ARRRRRRRRR"), CL(1, "MODE COMPLETE"), "", eBlink, eBlink, eNone, 3000, True, ""
				AwardDirtyCreature:LightAward3.State=1:vpmtimer.addTimer 5500, "AwardSuperJackpot'"
				debug.print "AwardDirtyCreature"
			End If

    Case 4: PlaySound "CO_DCHit4": debug.print "DirtyCreatureHit4"
				SmallAwardFlasherTimer.Enabled=1
			If Hard=1 Then 
				DMD CL(0, "SQUEAL LIKE A PIG"), CL(1, ""), "", eBlink, eNone, eNone, 3000, True, ""
				vpmtimer.addtimer 1500, "AwardJackPot2'"
			End If
			If Hard=0 Then 
				DMD CL(0, "SQUEAL LIKE A PIG"), CL(1, ""), "", eBlink, eNone, eNone, 3000, True, ""
				vpmtimer.addtimer 1500, "AwardJackPot2'"
			End If

    Case 5: 	SmallAwardFlasherTimer.Enabled=1
			If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_DownTheGobWet2": debug.print "DirtyCreatureHit5":End If
			If Hard=1 Then 
				DMD CL(0, "DOWN THE GOB"), CL(1, ""), "", eBlink, eNone, eNone, 3000, True, ""
				vpmtimer.addtimer 5500, "AwardJackpot2'"
			End If
			If Hard=0 Then 
				DMD CL(0, "DOWN THE GOB"), CL(1, ""), "", eBlink, eNone, eNone, 3000, True, ""
				vpmtimer.addtimer 5500, "AwardJackpot2'"
			End If
    Case 6: PlaySound "CO_DCHit6": debug.print "DirtyCreatureHit6"
				SmallAwardFlasherTimer.Enabled=1
			If Hard=1 Then 
				DMD CL(0, "MODE COMPLETE"), CL(1, ""), "", eBlink, eNone, eNone, 3000, True, ""
				AwardDirtyCreature:LightAward3.State=1:vpmtimer.AddTimer 4000, "AwardSuperJackpot2'"
				debug.print "AwardDirtyCreature"
			End If
			If Hard=0 Then 
				DMD CL(0, "  ITS HIDEOUS"), CL(1, " "), "", eBlink, eNone, eNone, 3000, True, ""
				vpmtimer.AddTimer 3000, "AwardSuperJackpot2'"

			End If
				vpmtimer.addtimer 2000, "StopDirtyCreature'":
				debug.print "StopDirtyCreature"
				DCHitCallout=0
	End Select
End Sub

Dim DirtyCreatureMuliBallActive
Sub DirtyCreatureMultiball
	DirtyCreatureMuliBallActive=1
	AddMultiball (2)
	If bBallsaverActive=false Then EnableBallSaver (30)
End Sub

Sub AwardDirtyCreature
If DirtyCreatureComplete(CurrentPlayer)=False Then DirtyCreatureComplete(CurrentPlayer)=True: CheckModesCompleted:debug.print "CheckModesComplete-atDirtyCreature":End If
End Sub

' Enemies animation
Dim MyPi2
MyPi2 = Round(4 * Atn(1), 6) / 90


Dim en1Step, en1Dir, en1frame, oldt1, newt1,oldt2, newt2, GreenSlimeDir, GreenSlimeStep,GreenSlimeframe
en1Step = 0
en1frame= 0
GreenSlimeStep = 0
GreenSlimeframe= 0
oldt1 = 0
newt1 = 0
oldt2 = 0
newt2 = 0

Sub en1Timer_Timer()
    en1Dir = SIN(en1Step * MyPi2)
    en1Step = (en1Step + 1)MOD 360
    en1.X = 475 + en1Dir * 410
    en1frame = (en1frame + 1) Mod 8
	en1.imageA = "en1_"&en1frame

    GreenSlimeDir = SIN(GreenSlimeStep * MyPi2)
    GreenSlimeStep = (GreenSlimeStep + 1)MOD 360
    GreenSlime.X = 475 + GreenSlimeDir * 410
    GreenSlimeframe = (GreenSlimeframe + 1) Mod 8
	GreenSlime.imageA = "GreenSlime_"&GreenSlimeframe

'check the targets
	newt1= INT(en1.x / 40) -1
	newt2= INT(en1.x / 40) -1
	If oldt1 <> newt1 Then
		ent1(newt1).Isdropped = 0:debug.print "en1t wall raised"
		ent1(oldt1).Isdropped = 1
		oldt1 = newt1
		ent2(newt1).Isdropped = 0:debug.print "en2t wall raised"
		ent2(oldt2).Isdropped = 1
		oldt2 = newt2
	End If
End Sub



Sub StopDirtyCreature
DirtyCreatureActive= 0
		DirtyCreatureEasyStopTimer.Enabled=False
	If BallFinished=False Then ChangeSong:End If
		DCLoose=False
		OverlayPiratesLifeYellow.visible=False
	If DirtyCreatureComplete(CurrentPlayer)=False Then DirtyCreatureComplete(CurrentPlayer)=1: LightAward3.State=1:CheckModesCompleted:debug.print "CheckModesComplete at Dirty Creature" :End If
		Light_DirtyCreature1.State=2:Light_DirtyCreature2.State=0:Light_DirtyCreature3.State=0
		en1.Visible=False
		GreenSlime.Visible=False
		en1Timer.Enabled = 0
		gi14.State=2
		DropDirtyCreatureWalltargets
		NumCreatureHits=0
End Sub

Sub DirtyCreatureEasyStopTimer_timer
DirtyCreatureActive= 0
		DirtyCreatureEasyStopTimer.Enabled=False
	If BallFinished=False Then ChangeSong:End If
		Debug.print "StopDirtyCreature"
		OverlayPiratesLifeYellow.visible=False
	If DirtyCreatureComplete(CurrentPlayer)=False Then LightAward3.State=0: End If
		Light_DirtyCreature1.State=2:Light_DirtyCreature2.State=0:Light_DirtyCreature3.State=0
		en1.Visible=False
		GreenSlime.Visible=False
		en1Timer.Enabled = 0
		gi14.State=2
		DropDirtyCreatureWalltargets
		NumCreatureHits=0
		DCLoose=False
End Sub


'************************
'GoldenReefKicker
'************************

Sub KickerGoldenReef_Hit
GoldenReefFlasherTimer.Enabled=1

	PlaySoundAt "fx_kicker_catch" ,KickerGoldenReef 
	If BarbaBlancaMultiballActive=1 Then vpmtimer.addtimer 4000, "AwardSuperJackpot'" 
	If WildSeasActive=True or DCLoose=True or BarbaBlancaMultiballActive=True Then:vpmtimer.addtimer 4000, "GoldenReefRelease'":PlaySound "CO_GetBackOnDeckWet":debug.print "MultiballActive-NoAwardSelectAvailable":Exit Sub

	If SelectAward=True Then:vpmtimer.addtimer 4000, "GoldenReefRelease'":PlaySound "CO_GetBackOnDeckWet":debug.print "AwardActiveNoAwardSelect":End If

	If SelectAward=False And DCLoose=False Then
		debug.print "AwardSelectionActive"
		AwardSelection=1  'First Count for flipper mode select
		cFlippersEnabled=True
		GiOff
'		OverlayPiratesLife.Visible=True
		SelectAward=True
		 DMD "", "", "DMD_AwardSelect", eNone, eNone, eNone, 1500, False, ""
		DMD CL(0, "  FLIPPER CHANGE "), CL(1, "MAGNA SELECT "), "", eBlink, eNone, eNone, 3000, True, ""
		PlaySound "CO_GoldenReefOzone"
		vpmtimer.addtimer 5000, "UpdateAwardSelection'"
	End If
End Sub

Sub GoldenReefKick
	PlaySoundAt SoundFXDOF("fx_kicker2" , 138, DOFPulse, DOFContactors), KickerGoldenReef
	KickerGoldenReef.kick 250,30
End Sub

Sub TurnOnLightsAfterAward
	GiOn
	OverlayPiratesLife.Visible=False
	End Sub

Sub GoldenReefRelease
	PlaySoundAt SoundFXDOF("fx_kicker2" , 138, DOFPulse, DOFContactors), KickerGoldenReef
	KickerGoldenReef.kick 250,20
	GiOn
	'******************
	'Start DrainSpinner
	'******************
	If SpinSave=False Then
		SpinSave=True
		StartWheelSpinnerDrain
		debug.print "drainSpinStarted"
		spindiscimg_drainsave.Visible=1
'		spinningdraintimer.Enabled=1
		vpmtimer.addtimer 9000, "DiscInvisible'"
		vpmtimer.addtimer 12000, "StopWheelSpinnerDrain'"
	End If
	'************************************************
End Sub

Sub GoldenReefAward
End Sub

Dim AwardCount
Sub NextReefAward

End Sub

Sub Kicker_GoldenReefReleaseUK_Hit()
	PlaySoundAt "fx_kicker_catch" , Kicker_GoldenReefReleaseUK
	vpmtimer.addtimer 3000, "UpKickerGRRelease'"
	StartGoldenReefAwardSequence
End Sub

Sub UpKickerGRRelease
	PlaySoundAt SoundFXDOF("fx_kicker2" , 137, DOFPulse, DOFContactors) , Kicker_GoldenReefReleaseUK
	Kicker_GoldenReefReleaseUK.kick  0, 60, 1.56
End Sub

Sub StartAwardLights
	Light_ModeSelect1.State=2:Light_ModeSelect2.State=2:Light_ModeSelect3.State=2:Light_ModeSelect4.State=2:Light_ModeSelect5a.State=2:Light_ModeSelect6.State=2:Light_ModeSelect7.State=2:Light_ModeSelect8.State=2:Light_KickerGR.State=2
	TurnOnLightsAfterAward
End Sub

Sub StopAwardLights
	Light_ModeSelect1.State=0:Light_ModeSelect2.State=0:Light_ModeSelect3.State=0:Light_ModeSelect4.State=0:Light_ModeSelect5a.State=0:Light_ModeSelect6.State=0:Light_ModeSelect7.State=0:Light_ModeSelect8.State=0
End Sub


'***********************
'GoldenReefAwardModes
'***********************
'StartGoldenReefLights
Dim AwardSelect
Dim A1OK,A2OK,A3OK,A4OK,A5OK

Sub UpdateAwardSelection

	DMDFlush
Select Case AwardSelection

    Case 1: DMD "", "", "DMD_SharkAttack", eNone, eBlink, eNone, 5000, False, ""
			A1OK=True:A2OK=False:A3OK=False:A4OK=False:A5OK=False

    Case 2:  DMD "", "", "DMD_WildSeas", eNone, eBlink, eNone, 5000, False, "" 
			A1OK=False:A2OK=True:A3OK=False:A4OK=False:A5OK=False

    Case 3:  DMD "", "", "DMD_TheTavern", eNone, eBlink, eNone, 5000, False, "" 
			A1OK=False:A2OK=False:A3OK=True:A4OK=False:A5OK=False

    Case 4: DMD "", "", "DMD_HelloPolly", eNone, eBlink, eNone, 5000, False, "" :
			A1OK=False:A2OK=False:A3OK=False:A4OK=True:A5OK=False

    Case 5:  DMD "", "", "DMD_RaiseTheSails", eNone, eBlink, eNone, 5000, False, "" 
			A1OK=False:A2OK=False:A3OK=False:A4OK=False:A5OK=True
End Select
End Sub

Sub SelectAwardTimer_Timer
	If A1OK=True Then 
		Award1
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_LookAtThoseOzone":End If
	End If
	If A2OK=True Then 
		Award2
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_BattonDownOzone":End If
	End If
	If A3OK=True Then 
		Award3
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_ToTheTavernOzone":End If
	End If
	If A4OK=True Then
		Award4
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_IdAdviseOzone":End If

 End If
	If A5OK=True Then 
		Award5
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_RaiseTheSails":End If
	End If
	StartGoldenReefAwardSequence
	GoldenReefReleaseTimer.Enabled=True
	SelectAwardTimer.Enabled=False
	cFlippersEnabled=False
End Sub

Sub GoldenReefReleaseTimer_Timer
	GoldenReefRelease
	GoldenReefReleaseTimer.Enabled=False
	DMDScoreNow
End Sub



Sub CheckModesCompleted
	ModesCompleted(CurrentPlayer)= ModesCompleted(CurrentPlayer) +1
	debug.print "CheckModesCompleted Sub Called" 

	If ModesCompleted(CurrentPlayer)=8 Then
	debug.print "ModesCompleted=8"
		vpmtimer.addtimer 10000, "ResetAwardLights'"
		vpmtimer.addtimer 10000, "ResetAwardStatus'"
		vpmtimer.addtimer 10000, "ModesCompleteSuperBonus'"
	End If
	If ModesCompleted(CurrentPlayer)=16 Then
	debug.print "ModesCompleted>8"
		vpmtimer.addtimer 10000, "ResetAwardLights'"
		vpmtimer.addtimer 10000, "ResetAwardStatus'"
		vpmtimer.addtimer 10000, "ModesCompleteSuperBonus'"
	End If
	If ModesCompleted(CurrentPlayer)=24 Then
		vpmtimer.addtimer 10000, "ResetAwardLights'"
		vpmtimer.addtimer 10000, "ResetAwardStatus'"
		vpmtimer.addtimer 10000, "ModesCompleteSuperBonus'"
	End If

End Sub

Sub ModesCompleteSuperBonus
		PlaySound "CO_SuperJackPot3Ozone"
		PlaySoundAt "fx_knocker",KickerGoldenReef
		DOF 122, DOFPulse
		DMD CL(0, "	ITS A MOTHERLOAD" ), CL(1, "MODES COMPLETE"), "", eNone, eBlink, eNone, 3000, True, ""
		If Hard=0 Then 
		DMD CL(0, "	TONIGHT WE DRINK" ), CL(1, "100000000"), "", eNone, eBlink, eNone, 5000, True, ""
			AddScore 100000000
		End If
		If Hard=1 Then 
		DMD CL(0, "	TONIGHT WE DRINK" ), CL(1, "50000000"), "", eNone, eBlink, eNone, 5000, True, ""
			AddScore 50000000
		End If
End Sub

Sub	ResetAwardStatus
	debug.print "ResetAwardCompleteFlags"
	WalkThePlankComplete(CurrentPlayer)=0:RaiseTheSailsComplete(CurrentPlayer)=0: SharkComplete(CurrentPlayer)=0: WildSeasComplete(CurrentPlayer)=0
	DirtyCreatureComplete(CurrentPlayer)=0:SharkComplete(CurrentPlayer)=0:PollyComplete(CurrentPlayer)=0:TreasureComplete(CurrentPlayer)=0:TavernComplete(CurrentPlayer)=0
	WildSeasPart1=1:WildSeasPart1=0
	PlankAward1Active(CurrentPlayer)=1:PlankAward2Active(CurrentPlayer)=0:PlankAward1Active(CurrentPlayer)=0
	LightPlankReady.State=1:LightPlankFish.State=0:LightPlankMermaid.State=0
End Sub

Sub ResetAwardLights
	debug.print "ResetAwardLights"
	LightAward1.State=0:LightAward2.State=0:LightAward3.State=0:LightAward4.State=0:LightAward5.State=0:LightAward6.State=0:LightAward7.State=0:LightAward8.State=0
End Sub

Sub ResetAwardLightsStartOfGame
	LightAward1.State=0:LightAward2.State=0:LightAward3.State=0:LightAward4.State=0:LightAward5.State=0:LightAward6.State=0:LightAward7.State=0:LightAward8.State=0
End Sub

'***************
'Award1_SharkAttack
'***************
Dim SharkAttackActive
 Sub Award1
	IF	bBallSaverActive = False Then EnableBallSaver (37): End If
		AwardFlasherTimer.Enabled=1
		SharkAttackTimer.Enabled=1
		SpinDiscShark.Visible=True
		SharkAttackActive=True
		SelectAward= True
	If 	DCLoose=False Then PlaySong "m_From the Fields of Gallia-Short-6dB":End If
		'AddMultiball (2)
		EnableBallSaver (200)
		StopAwardLights
		DMD CL(0, "SHARK ATTACK" ), CL(1, "HIT THE SHARK"), "", eNone, eBlink, eNone, 3000, True, ""
		AddScore 1000
		Shark1MoveUp
		EnableBallSaver (20)
	If SharkComplete(CurrentPlayer)=False Then LightAward4.State=2: End If
		SharkEasyStopTimer.Enabled=True
End Sub


Sub StopAward1
		SharkEasyStopTimer.Enabled=False
		Debug.print "StopSharkAttack"
	If BallFinished=False Then ChangeSong:End If
		SharkAttackTimer.Enabled=False
		SpinDiscShark.Visible=False
		Shark1MoveDown
		SharkAttackActive=False
		SelectAward=False
		StartAwardLights
	If SharkComplete(CurrentPlayer)=False Then LightAward4.State=0:End If
		TriggerShark1.Enabled = 0:TriggerShark2.Enabled = 0:TriggerShark3.Enabled = 0:TriggerShark4.Enabled = 0:TriggerShark5.Enabled = 0:debug.print "SharkTriggersDisabledStopAward1"
End Sub

Sub SharkAttackTimer_Timer
	SpinDiscShark.rotz = (SpinDiscShark.rotz + 5)mod 360
End Sub

Sub SharkEasyStopTimer_timer
		SharkEasyStopTimer.Enabled=False
		Debug.print "StopSharkAttack"
	If	 BallFinished=False Then ChangeSong:End If
		SharkAttackTimer.Enabled=False
		SpinDiscShark.Visible=False
		Shark1MoveDown
		SharkAttackActive=False
		SelectAward=False
		StartAwardLights
	If SharkComplete(CurrentPlayer)=False Then LightAward4.State=0:End If
		TriggerShark1.Enabled = 0:TriggerShark2.Enabled = 0:TriggerShark3.Enabled = 0:TriggerShark4.Enabled = 0:TriggerShark5.Enabled = 0:debug.print "SharkTriggersDisabledStopAward1"
End Sub


'***************
'Award3_WildSeas
'***************


Sub Award2
	AwardFlasherTimer.Enabled=1
	SelectAward= True
	StopAwardLights
	DMD CL(0, "WILD SEAS" ), CL(1, "TREASURE LIGHTS"), "", eBlink, eBlink, eNone, 5000, True, ""
	AddScore 1000
	StartWS
End Sub
'*********************************************
'StartWildSeas
'********************************************

Dim WildSeasActive
Dim WildSeasPart1
Dim WildSeasFinished

Sub StartWS
		WildSeasPart1=1
		SelectAward = True
		If bMultiBallMode=False Then vpmtimer.addtimer 5100, "WildSeasMultiball'":End If
		WildSeasActive=True
	If 	DCLoose=False Then PlaySong "m_DangerousSeasHighQual-6dB":End if
		debug.print "WildSeasActive"
		Light_TC2.State=2:	Light_TC3.State=2:	Light_TC4.State=2
	If WildSeasComplete(CurrentPlayer)=False Then LightAward5.State=2:End If
		spindiscimg_wheel.Visible=True
		StartWheelSpinner
		WildSeasEasyStopTimer.Enabled=True 
End Sub

Dim WildSeasMultiballActive
Sub WildSeasMultiball
		WildSeasMultiballActive=1
		AddMultiball (2)
		PlaySound "CO_WildSeasMultiballOzone"
		DMD CL(0, "WILD SEAS" ), CL(1, "MULTIBALL"), "", eBlink, eBlink, eNone, 3000, True, ""
		EnableBallSaver(30)	
End Sub

Sub ResetWS
		WildSeasFinished=1
		WildSeasEasyStopTimer.Enabled=False
		debug.print "WildSeasStopped"
		WildSeasPart1=False:WildSeasPart2=False
	If BallFinished=False Then ChangeSong:End If
'	If WildSeasComplete(CurrentPlayer)=False Then WildSeasComplete(CurrentPlayer)=True: WildSeasActive=False Then LightAward5.State=1: End If
	If WildSeasComplete(CurrentPlayer)=True Then LightAward5.State=1: End If
		StopSpinner2Motor
		StartAwardLights
		SelectAward=False
		WildSeasActive=False
		Light_TC2.State=0:	Light_TC3.State=0:	Light_TC4.State=0
		SetLightColor Light_TC2,white, -1:SetLightColor Light_TC3,white, -1:SetLightColor Light_TC4,white, -1
End Sub

Sub WildSeasEasyStopTimer_timer
		WildSeasFinished=1
		WildSeasEasyStopTimer.Enabled=False
		WildSeasPart1=False:WildSeasPart2=False
	If BallFinished=False Then ChangeSong:End If
	debug.print "WildSeasStopped"
	If WildSeasComplete(CurrentPlayer)=False And WildSeasActive=True Then LightAward5.State=0: End If
	If WildSeasComplete(CurrentPlayer)=True Then LightAward5.State=1: End If
		StopSpinner2Motor
		StartAwardLights
		SelectAward=False
		WildSeasActive=False
		Light_TC2.State=0:	Light_TC3.State=0:	Light_TC4.State=0
		SetLightColor Light_TC2,white, -1:SetLightColor Light_TC3,white, -1:SetLightColor Light_TC4,white, -1
End Sub

'*******************************************
'StartSecondPhaseOf WildSeas Treasure Hunt
'*******************************************


Sub CheckWildSeasAward
	debug.print "Check Wild Seas Award"
If Hard=0 Then vpmtimer.addtimer 3000, "WildSeasEasyModeAward'": End If
If Hard=1 Then vpmtimer.addtimer 3000, "WildSeasHardModeAward'": End If
End Sub

Sub WildSeasEasyModeAward
	If 	Light_TC2.State=1 And Light_TC3.State=1 And Light_TC4.State=1 And WildSeasPart2=1 Then 
		If 	CalloutActive=0 Then CalloutActive=1:CalloutTimer.Enabled=True:PlaySound "CO_MotherLoadWet":End If
				SmallAwardFlasherTimer.Enabled=1:LightAward5.State=1
				vpmtimer.addtimer 1000, "ResetWS'"
				vpmtimer.addtimer 2500, "AwardSuperJackPot2'":debug.print "SuperJackPot2EasyPart2"
		End If

	If 	Light_TC2.State=1 And Light_TC3.State=1 And Light_TC4.State=1 And WildSeasPart1=1 Then 
		If 	CalloutActive=0 Then CalloutActive=1:CalloutTimer.Enabled=True:PlaySound "CO_MotherLoadWet":End If
			If WildSeasComplete(CurrentPlayer)=False Then WildSeasComplete(CurrentPlayer)=True:CheckModesCompleted: debug.print "CheckModesComplete-atWildSeasEasyAward":End If
				SmallAwardFlasherTimer.Enabled=1:vpmtimer.addtimer 2500, "AwardSuperJackPot2'":LightAward5.State=1:debug.print "SuperJackPot2EasyPart1"
			vpmtimer.addtimer 3500, "StartWildSeasTreasure2'"
	End If
End Sub


Sub WildSeasHardModeAward

	If 	Light_TC2.State=1 And Light_TC3.State=1 And Light_TC4.State=1 And WildSeasPart2=1 Then 
		If 	CalloutActive=0 Then CalloutActive=1:CalloutTimer.Enabled=True:PlaySound "CO_MotherLoadWet":End If
			If WildSeasComplete(CurrentPlayer)=False Then WildSeasComplete(CurrentPlayer)=True:CheckModesCompleted: debug.print "CheckModesComplete-atWildSeasHardAward":End If
				SmallAwardFlasherTimer.Enabled=1:LightAward5.State=1
				vpmtimer.addtimer 1000, "ResetWS'"
				vpmtimer.addtimer 2500, "AwardSuperJackpot2'":debug.print "SuperJackPotHardPart2"
		End If

	If 	Light_TC2.State=1 And Light_TC3.State=1 And Light_TC4.State=1 And WildSeasPart1=1 Then 
		If 	CalloutActive=0 Then CalloutActive=1:CalloutTimer.Enabled=True:PlaySound "CO_MotherLoadWet":End If
			SmallAwardFlasherTimer.Enabled=1:vpmtimer.addtimer 2500, "AwardSuperJackPot'":debug.print "SuperJackPot2HardPart1"
			vpmtimer.addtimer 3500, "StartWildSeasTreasure2'"
	End If
End Sub


Dim WildSeasPart2
Sub StartWildSeasTreasure2
		debug.print "StartildSeasTreasure2"
		WildSeasPart2=1:WildSeasPart1=0
		SetLightColor Light_TC2,red, -1:SetLightColor Light_TC3,darkblue, -1:SetLightColor Light_TC4,white, -1
		Light_TC2.State=2:Light_TC3.State=2:Light_TC4.State=2
End Sub
'************************
'Award4_ParrotOffThePerch	See TriggerLeftLoopB for Award script
'************************

Dim HelloPollyActive

Sub Award4
		AwardFlasherTimer.Enabled=1
	IF	bBallSaverActive = False Then EnableBallSaver (50): End If
		HelloPollyActive=True
	If 	DCLoose=False Then PlaySong "m_OceanBound_HighQual-6dB":End If
		SelectAward=True
		StopAwardLights
		DMD CL(0, "HELLO POLLY" ), CL(1, "SHOOT LEFT LOOP"), "", eNone, eBlink, eNone, 3000, True, ""
		AddScore 1000
	If PollyComplete(CurrentPlayer)=False Then LightAward1.State=2: End If
		Light_POTP1.State=2
		PollyEasyStopTimer.Enabled=True 
End Sub


Sub StopAward4
		PollyEasyStopTimer.Enabled=False
	If BallFinished=False Then ChangeSong:End If
		SelectAward=False
		vpmtimer.addtimer 1000, "StartAwardLights'"
		Light_POTP1.State=0:Light_POTP2.State=0:Light_POTP3.State=0
	If  PollyComplete(CurrentPlayer)=False Then  LightAward1.State=0:End If
		ParrotMoveDown
		HelloPollyActive=False
End Sub

Sub ParrotSwirlTimer_Timer
	SpinDiscPolly1.rotz = (SpinDiscPolly1.rotz + 5)mod 360
End Sub

Sub PollyEasyStopTimer_timer
		PollyEasyStopTimer.Enabled=False
	If BallFinished=False Then ChangeSong:End If
		SelectAward=False
		vpmtimer.addtimer 1000, "StartAwardLights'"
		Light_POTP1.State=0:Light_POTP2.State=0:Light_POTP3.State=0
	If  PollyComplete(CurrentPlayer)=False Then  LightAward1.State=0:End If
		ParrotMoveDown
		HelloPollyActive=False
End Sub

'****************************************
'StartWheelSpinner'
'****************************************

Sub WheelImageSpin_Timer
	spindiscimg_wheel.rotz = spindiscimg_wheel.rotz + 2
End Sub


Sub spinning2_timer
	WheelImageSpin.Enabled=False
	spindiscimg_wheel.rotz = spindiscimg_wheel.rotz + 10
	DOF 124,DOFPulse 'Beacon WildSeasSpinner on
	DOF 120,DOFPulse	'WildSeas Shaker on'
	DOF 126,DOFPulse	'Fan on
End Sub

dim spinner2
Set spinner2 = New cvpmTurntable
With spinner2
	.InitTurntable spindisc_wheel, 100
	.SpinDown = 20
	.CreateEvents "spinner2"
End With
spinner2.MotorOn = false


Sub StartWheelSpinner	
	AddScore 1000000
	spinner2.MotorOn = true
	spinning2.enabled = True
End Sub


'*************************************
'StopMotorSpinner
'**************************************
Sub StopSpinner2Motor
	Motor2SpinnerTimer.Enabled=False
	spinner2.MotorOn = false
	spinning2.enabled = false
	spindiscimg_wheel.Visible=False
End Sub

'******************
'Award3_LastDrinks
'******************
Dim Skeleton1HitCount,Skeleton2HitCount,Skeleton3HitCount,Skeleton4HitCount
Dim Skeleton1Complete,Skeleton2Complete,Skeleton3Complete,Skeleton4Complete
Dim LastDrinksActive
Sub Award3
		IF	bBallSaverActive = False Then EnableBallSaver (37): End If
			AwardFlasherTimer.Enabled=1
			SkeletonSwirlTimer1.Enabled=True:SkeletonSwirlTimer2.Enabled=True:SkeletonSwirlTimer3.Enabled=True:SkeletonSwirlTimer4.Enabled=True
			SpinDiscSkeleton1.Visible=True:SpinDiscSkeleton2.Visible=True:SpinDiscSkeleton3.Visible=True:SpinDiscSkeleton4.Visible=True
			LastDrinksActive=True
			If bBallSaverActive = False Then EnableBallSaver (37)
			PlaySong "m_DangerousTides-6dB"
			SelectAward= True:LastDrinksActive=True
			Skeleton1MoveUp:Skeleton2MoveUp:Skeleton3MoveUp:Skeleton4MoveUp
			Skeleton1HitCount=0:Skeleton2HitCount=0:Skeleton3HitCount=0:Skeleton4HitCount=0
'			Addmultiball (2):EnableBallSaver (20):
			StopAwardLights
			DMD CL(0, "3 SHOTS" ), CL(1, "PER TARGET"), "", eNone, eBlink, eNone, 3000, True, ""
			AddScore 1000
		If TavernComplete(CurrentPlayer)=False Then LightAward6.State=2:End If
			TavernEasyStopTimer.Enabled=True 
End Sub

Sub StopAward3
			TavernEasyStopTimer.Enabled=False
			debug.print "StopAward3-LastDrinksStopped"
			SelectAward=False:LastDrinksActive=False
		If BallFinished=False Then ChangeSong:End If
			Skeleton1MoveDown:Skeleton2MoveDown:Skeleton3MoveDown:Skeleton4MoveDown
			Skeleton1Wall.Collidable=False:Skeleton2Wall.Collidable=False:Skeleton3Wall.Collidable=False:Skeleton4Wall.Collidable=False
			Skeleton1Wallb.Collidable=False:Skeleton2Wallb.Collidable=False:Skeleton3Wallb.Collidable=False:Skeleton4Wallb.Collidable=False
			Skeleton1Complete=False:Skeleton2Complete=False:Skeleton2Complete=False:Skeleton2Complete=False
			StartAwardLights
			Skeleton1HitCount=0:Skeleton2HitCount=0:Skeleton3HitCount=0:Skeleton4HitCount=0
			Skeleton1Complete=0:Skeleton2Complete=0:Skeleton3Complete=0:Skeleton4Complete=0
		If TavernComplete(CurrentPlayer)=False Then LightAward6.State=0: End If
			LastDrinksActive=False
End Sub

Sub TavernEasyStopTimer_timer
			TavernEasyStopTimer.Enabled=False
			debug.print "StopAward3-LastDrinksStopped"
			SelectAward=False:LastDrinksActive=False
	If BallFinished=False Then ChangeSong:End If
			Skeleton1MoveDown:Skeleton2MoveDown:Skeleton3MoveDown:Skeleton4MoveDown
			Skeleton1Wall.Collidable=False:Skeleton2Wall.Collidable=False:Skeleton3Wall.Collidable=False:Skeleton4Wall.Collidable=False
			Skeleton1Wallb.Collidable=False:Skeleton2Wallb.Collidable=False:Skeleton3Wallb.Collidable=False:Skeleton4Wallb.Collidable=False
			Skeleton1Complete=False:Skeleton2Complete=False:Skeleton2Complete=False:Skeleton2Complete=False
			StartAwardLights
			Skeleton1HitCount=0:Skeleton2HitCount=0:Skeleton3HitCount=0:Skeleton4HitCount=0
			Skeleton1Complete=0:Skeleton2Complete=0:Skeleton3Complete=0:Skeleton4Complete=0
		If TavernComplete(CurrentPlayer)=False Then LightAward6.State=0: End If
			LastDrinksActive=False
End Sub


Sub Skeleton1Wall_Hit
		PlaySound "CO_BurpShortCompR"
		debug.print "Skeleton1Hit"
		AddScore 1000
		PlaySoundAt "fx_bumper",KickerCanonLoad
		Skeleton1HitCount=Skeleton1HitCount+1
	If Hard=1 And Skeleton1HitCount=1 Then PlaySound "CO_BurpShortCompR": AddScore 250000: End If
	If Hard=1 And Skeleton1HitCount=2 Then PlaySound "CO_BurpShortCompR": AddScore 500000: End If
	If Hard=1 And Skeleton1HitCount=3 Then
		AwardFlasherTimer.Enabled=1
		Skeleton1MoveDown:Skeleton1Wall.IsDropped=True:Skeleton1Wallb.IsDropped=True:Skeleton1HitCount=0
		Skeleton1Complete=True:CheckSkeletonsDownComplete:DMD CL(0, "BONES IS" ), CL(1, "PISSED AS    "), "", eBlink, eBlink, eNone, 3000, True, ""
		PlaySound "CO_HoHoHoBurpOzone"
		AddScore 1000000
	End If
	If Hard=0 And Skeleton1HitCount=1 Then PlaySound "CO_BurpShortCompR": AddScore 750000: End If
	If Hard=0 And Skeleton1HitCount=2 Then
		AwardFlasherTimer.Enabled=1
		Skeleton1MoveDown:Skeleton1Wall.IsDropped=True:Skeleton1Wallb.IsDropped=True:Skeleton1HitCount=0
		Skeleton1Complete=True:CheckSkeletonsDownComplete:DMD CL(0, "BONES IS" ), CL(1, "PISSED AS    "), "", eBlink, eBlink, eNone, 3000, True, ""
		PlaySound "CO_HoHoHoBurpOzone"
		AddScore 1000000
	End If
End Sub

Sub Skeleton2Wall_Hit
		debug.print "Skeleton2Hit"
		PlaySound "CO_BurpShortCompR"
		AddScore 10000
		PlaySoundAt "fx_bumper",KickerCanonLoad
		Skeleton2HitCount=Skeleton2HitCount+1
	If Hard=1 And Skeleton2HitCount=1 Then PlaySound "CO_BurpShortCompR": AddScore 500000: End If
	If Hard=1 And Skeleton2HitCount=2 Then PlaySound "CO_BurpShortCompR": AddScore 500000: End If
	If Hard=1 And Skeleton2HitCount=3 Then 
		AwardFlasherTimer.Enabled=1
		Skeleton2MoveDown:Skeleton2Wall.IsDropped=True:Skeleton2Wallb.IsDropped=True:Skeleton2HitCount=0
		Skeleton2Complete=True:CheckSkeletonsDownComplete:DMD CL(0, "SLIM HAS" ), CL(1, "CHUNDERED    "), "", eBlink, eBlink, eNone, 3000, True, ""
		PlaySound "CO_HoHoHoBurpOzone"
		AddScore 1000000
	End If
	If Hard=0 And Skeleton2HitCount=1 Then PlaySound "CO_BurpShortCompR": AddScore 750000: End If
	If Hard=0 And Skeleton2HitCount=2 Then
		AwardFlasherTimer.Enabled=1
		Skeleton2MoveDown:Skeleton2Wall.IsDropped=True:Skeleton2Wallb.IsDropped=True:Skeleton2HitCount=0
		Skeleton2Complete=True:CheckSkeletonsDownComplete:DMD CL(0, "SLIM HAS" ), CL(1, "CHUNDERED    "), "", eBlink, eBlink, eNone, 3000, True, ""
		PlaySound "CO_HoHoHoBurpOzone"
		AddScore 1000000
	End If
End Sub

Sub Skeleton3Wall_Hit
		PlaySound "CO_BurpShortCompR"
		debug.print "Skeleton3Hit"
		AddScore 1000	
		PlaySoundAt "fx_bumper",KickerTarget
		Skeleton3HitCount=Skeleton3HitCount+1
	If Hard=1 And Skeleton3HitCount=1 Then PlaySound "CO_BurpShortCompR": AddScore 500000: End If
	If Hard=1 And Skeleton3HitCount=2 Then PlaySound "CO_BurpShortCompR": AddScore 500000: End If
	If Hard=1 And Skeleton3HitCount=3 Then 
		AwardFlasherTimer.Enabled=1
		Skeleton3MoveDown:Skeleton3Wall.IsDropped=True:Skeleton3Wallb.IsDropped=True:Skeleton3HitCount=0
		Skeleton3Complete=True:CheckSkeletonsDownComplete:DMD CL(0, "JONESY IS" ), CL(1, "OFF THE BARREL "), "", eBlink, eBlink, eNone, 3000, True, ""
		PlaySound "CO_HoHoHoBurpOzone"
		AddScore 1000000
	End If
	If Hard=0 And Skeleton3HitCount=1 Then PlaySound "CO_BurpShortCompR": AddScore 750000: End If
	If Hard=0 And Skeleton3HitCount=2 Then
		AwardFlasherTimer.Enabled=1
		Skeleton3MoveDown:Skeleton3Wall.IsDropped=True:Skeleton3Wallb.IsDropped=True:Skeleton3HitCount=0
		Skeleton3Complete=True:CheckSkeletonsDownComplete:DMD CL(0, "JONESY IS" ), CL(1, "OFF THE BARREL "), "", eBlink, eBlink, eNone, 3000, True, ""
		PlaySound "CO_HoHoHoBurpOzone"
		AddScore 1000000
	End If
End Sub

Sub Skeleton4Wall_Hit
		debug.print "Skeleton4Hit"
		PlaySoundAt "fx_bumper",KickerTarget
		Skeleton4HitCount=Skeleton4HitCount+1
	If Hard=1 And Skeleton4HitCount=1 Then PlaySound "CO_BurpShortCompR": AddScore 50000: End If
	If Hard=1 And Skeleton4HitCount=2 Then PlaySound "CO_BurpShortCompR": AddScore 500000: End If
	If Hard=1 And Skeleton4HitCount=3 Then 
		AwardFlasherTimer.Enabled=1
		Skeleton4MoveDown:Skeleton4Wall.IsDropped=True:Skeleton4Wallb.IsDropped=True:Skeleton4HitCount=0
		Skeleton4Complete=True:CheckSkeletonsDownComplete:DMD CL(0, "SCOOBS IS" ), CL(1, "TOTALLY DRUNK    "), "", eBlink, eBlink, eNone, 3000, True, ""
		PlaySound "CO_HoHoHoBurpOzone"
		AddScore 1000000
	End If
	If Hard=0 And Skeleton4HitCount=1 Then PlaySound "CO_BurpShortCompR": AddScore 250000: End If
	If Hard=0 And Skeleton4HitCount=2 Then 
		AwardFlasherTimer.Enabled=1
		Skeleton4MoveDown:Skeleton4Wall.IsDropped=True:Skeleton4Wallb.IsDropped=True:Skeleton4HitCount=0
		Skeleton4Complete=True:CheckSkeletonsDownComplete:DMD CL(0, "SCOOBS IS" ), CL(1, "TOTALLY DRUNK    "), "", eBlink, eBlink, eNone, 3000, True, ""
		PlaySound "CO_HoHoHoBurpOzone"
		AddScore 1000000
	End If



End Sub

Sub CheckSkeletonsDownComplete
	If Skeleton1Complete=True And Skeleton2Complete=True And Skeleton3Complete=True And Skeleton4Complete=True Then AwardLastDrinks: LastDrinksActive=False:debug.print "skeletoncompletecheck":End If
End Sub

Sub AwardLastDrinks
		debug.print "AwardLastDrink"
		If Hard=0 Then vpmtimer.addtimer 4000, "AwardSuperJackpot2'":End If
		If Hard=1 Then vpmtimer.addtimer 4000, "AwardSuperJackpot'":End If		
		Skeleton1Complete=False:Skeleton2Complete=False:Skeleton2Complete=False:Skeleton2Complete=False
		LastDrinksActive=False
		If TavernComplete(CurrentPlayer)=False Then CheckModesCompleted:LightAward6.State=1: TavernComplete(CurrentPlayer)=True: debug.print "CheckModesComplete-atTheTavern:"End If
		TavernComplete(CurrentPlayer)=True		
		StopAward3
End Sub


'***************
'Award5_RaiseTheSails
'***************
Dim RTSActive

Sub Award5
	IF	bBallSaverActive = False Then EnableBallSaver (30): End If
		AwardFlasherTimer.Enabled=1
		RaiseTheSailsEasyStopTimer.Enabled=True 
		SelectAward = True
		RTSActive=True
	If 	DCLoose=False Then PlaySong "m_BlackSkullsHighQual-7dB"
		StopAwardLights
		DMD CL(0, "RAISE THE SAILS" ), CL(1, "BLUE LIGHTS"), "", eNone, eBlink, eNone, 3000, True, ""
		AddScore 1000
	If RaiseTheSailsComplete(CurrentPlayer)=False Then LightAward7.State=2: End If
		Light_Patch.State=2:Light_Hook.State=2:Light_Leg.State=2
		TavernEasyStopTimer.Enabled=True
End Sub

Sub StopAward5
		RaiseTheSailsEasyStopTimer.Enabled=False
	If BallFinished=False Then ChangeSong:End If
		RTSActive=False	
		SelectAward=False
	If 	RaiseTheSailsComplete(CurrentPlayer)=True  Then:LightAward7.State=1: End If
	If	RaiseTheSailsComplete(CurrentPlayer)=False Then:LightAward7.State=0: End If
		StartAwardLights
		Light_Patch.State=0:Light_Hook.State=0:Light_Leg.State=0
debug.print "StopAward5-RaiseTheSailsStopped"
End Sub

Sub RaiseTheSailsEasyStopTimer_timer
		RaiseTheSailsEasyStopTimer.Enabled=False
	If BallFinished=False Then ChangeSong:End If
		RTSActive=False	
		SelectAward=False
	If 	RaiseTheSailsComplete(CurrentPlayer)=True  Then:LightAward7.State=1: End If
	If	RaiseTheSailsComplete(CurrentPlayer)=False Then:LightAward7.State=0: End If
		StartAwardLights
		Light_Patch.State=0::Light_Hook.State=0:Light_Leg.State=0
		debug.print "StopAward5-RaiseTheSailsStopped"
End Sub

Sub ResetAllModes
	StopAward3:StopAward4:StopAward5:ResetWS:StopAward1:StopDirtyCreature
End Sub

'*********************************************
'StartArmTheCanon
'********************************************
Dim ATCAwardActive
Dim	ATCDone
Sub StartATCAward
'	PlaySound "CO_LoadTheCanons"
	Light_LoadCanon1.State=2
End Sub

Sub ResetArmTheCanonAward
	Light_LoadCanon1.State=2
End Sub


Sub KickerCanonLoad_Hit()
	SinkShipFlasherTimer.Enabled=1
	ArmCanonSkillShot=False
	LoadCanonCallout

	KickerCanonLoad.DestroyBall
	PlaySoundAt "fx_kicker_catch" ,KickerCanonLoad
	vpmtimer.addtimer 5100, "MonkeyCallOut'"
	vpmtimer.addtimer 6100, "CreateBallCanonLoadUK'"
End Sub

Sub MonkeyCallOut
	PlaySound "CO_Monkey3"
End Sub

Dim LCCallout
Sub LoadCanonCallout
	LCCallout= LCCallout+1
	Select Case LCCallout
		Case 1 :If 	CalloutActive=0 Then CalloutActive=1:CalloutTimer.Enabled=True:PlaySound "CO_PowderTheMonkeysLadsOzone"

		Case 2 :If 	CalloutActive=0 Then CalloutActive=1:CalloutTimer.Enabled=True:PlaySound "CO_BlastHerOutOzone": LCCallout=0 
	End Select

End Sub

Sub CreateBallCanonLoadUK
	KickerCanonLoadUK.CreateSizedball BallSize / 2
'	KickerCanonLoadUK.Enabled=True
	vpmtimer.addtimer 100, "KickerCanonLoadUKRelease'"
	End Sub

Sub KickerCanonLoadUKRelease
	PlaySoundAt SoundFXDOF("Popper", 128, DOFPulse, DOFContactors), KickerCanonLoadUK
	KickerCanonLoadUK.kick  0, 20
	AddScore 50000
	ResetATCTargets
	PlaySoundAt "fx_solenoid",KickerCanonLoad
Dim i
For each i in SailorMonkey: i.Transz = 50: Next
	BoofHeadMonkeyJumpTimer.Enabled=1
End Sub

Sub BoofHeadMonkeyJumpTimer_Timer()
	PlaySoundAt "fx_solenoid",KickerCanonLoad
Dim i
For each i in SailorMonkey: i.Transz = 0: Next
	BoofHeadMonkeyJumpTimer.Enabled=0
End Sub

'**************************************
'Locks
'**************************************

Dim DaveyJonesActive
Sub	StartDaveyJones
	DaveyJonesActive=1
	LockDiverter.rotatetoend
	PlaySoundAt "fx_diverter" , LockDiverter
	Light_Lock1.State=2
	Light_Lock2.State=2
	FlasherPicklesTreasure.Visible=True
	Light_DaveyJones.State=2
	gi25.State=0:gi24.State=2:gi8.State=2
End Sub


Sub StopDaveyJones
	DaveyJonesActive=0
	Light_Lock1.State=0
	Light_DaveyJones.State=0
	Light_Lock2.State=0
	gi25.State=1:gi8.State=1
	FlasherPicklesTreasure.Visible=False
	LockDiverter.rotatetostart
	LowerRampLock2	
	KickerRampBallTrapped.Enabled=0
	KickerRampLock.Enabled=1
End Sub


Sub KickerDaveyJones_Hit()
	StartGoldenReefAwardSequence
	SinkShipFlasherTimer.Enabled=1
	If 	CalloutActive=0 Then CalloutActive=1:CalloutTimer.Enabled=True:PlaySound "CO_SayHelloToDaveyJonesOzone":End If
	DMD CL(0, "RAID DAVEY JONES" ), CL(1, "LOCKER    "), "", eBlink, eBlink, eNone, 3000, True, ""	
	PlaySoundAt "fx_kicker_catch" , KickerDaveyJones
	vpmtimer.addtimer 5000, "DaveyJonesRelease'"
	LockDiverter.rotatetostart
	RaiseRampLock2

	vpmtimer.addtimer 5000, "AwardJackpot3'"
End Sub

Sub DaveyJonesRelease
	KickerDaveyJones.Kick 290, 30
	PlaySoundAt SoundFXDOF("fx_kicker2" , 136, DOFPulse, DOFContactors) , KickerDaveyJones
	If Hard=0 Then AwardJackpot3: End If
	If Hard=1 Then AwardJackpot2: End If
End Sub

Sub KickerRampLock_Hit()
	WallLiftRampBlocker1.isdropped=False
		DMD CL(0, "PICKLES TREASURE" ), CL(1, ""), "", eBlink, eBlink, eNone, 3000, True, ""
	StartGoldenReefAwardSequence
	TreasureFlasherTimer.Enabled=1
	PlaySoundAt "VUKEnter" , KickerRampLock

	If 	CalloutActive=0 Then CalloutActive=1:CalloutTimer.Enabled=True:	PlaySound "CO_MotherLoadWet":End If
	vpmtimer.addtimer 5500, "KickerRampLockKick'"
	vpmtimer.addtimer 5800, "LowerRampLock2'"
End Sub

Sub KickerRampLockKick
	Light_Lock1.State=0
	Light_DaveyJones.State=0
	Light_Lock2.State=0
	gi25.State=1:gi8.State=1
	FlasherPicklesTreasure.Visible=False
	PlaySoundAt "VUKOut" , KickerRampLock
	KickerRampLock.kick 260, 28
	KickerRampLock.Enabled=0
	If Hard=0 Then AwardSuperJackpot2: End If
	If Hard=1 Then AwardSuperJackpot: End If
	vpmtimer.addtimer 200, "DeactivateRampLockKick'"
End Sub

Sub DeactivateRampLockKick
	debug.print "DisableKickerRampLock"
'	KickerRampLock.Enabled=0
	vpmtimer.addtimer 200, "ActivateRampBallTrap'"
End Sub

Sub ActivateRampBallTrap
	KickerRampBallTrapped.Enabled=1
	debug.print "KickerballRampTrappEnabled"
End Sub

Sub KickerRampBallTrapped_Hit()
	WallLiftRampBlocker2.isdropped=False
	WallLiftRampBlocker3.isdropped=False
	debug.print "BallTrapKickerHit BallDestroyed"
	KickerRampBallTrapped.DestroyBall
	KickerRampBallTrappedReleased.Enabled=1
	KickerRampBallTrappedReleased.CreateSizedball BallSize / 2

	KickerRampBallTrapped.Enabled=0
	vpmtimer.addtimer 200, "DisableKickerRampTrapRelease'"
End Sub

Sub DisableKickerRampTrapRelease
	PlaySoundAt "VUKOut" , KickerRampLock
	KickerRampBallTrappedReleased.kick 260, 5
	KickerRampBallTrappedReleased.Enabled=0
	debug.print "KickerRampBallTrappedReleased and KickThenDisable"
	vpmtimer.addtimer 200, "DropReleaseKickerProtectiveWalls'"		
End Sub

Sub DropReleaseKickerProtectiveWalls
	WallLiftRampBlocker2.isdropped=True
	WallLiftRampBlocker3.isdropped=True
End Sub

Sub StopTreasureMultiball
	Light_TC2.State=0
	Light_TC3.State=0
	Light_TC4.State=0
End Sub

'********************
'Lock2Ramp-RaiseandLower
'********************

Sub RaiseRampLock2
Dim obj
	RampLock2.HeightBottom = 75
	RampLock2.HeightTop = 70
	RampLock2.Collidable = False
	liftRamp.TransZ=-138
	liftRamp.TransY=37
	liftRamp.OBJRotY=-7
	liftRamp.OBJRotX=30
	RampProtector.RotX=10
	RampProtector.TransZ=70
	RampProtector.TransY=70
	RampProtector.TransX=20
	PlaySoundAt "KickBack2", KickerRampLock
	KickerRampBallTrapped.Enabled=0
	KickerRampLock.Enabled=1
	RampDropWall.IsDropped=True
End Sub


Sub LowerRampLock2

	RampLock2.HeightBottom = 0
	RampLock2.HeightTop = 75
	RampLock2.Collidable = True
	liftRamp.OBJRotX=0
	liftRamp.OBJRotY=0
	liftRamp.TransZ=0
	liftRamp.TransY=0
	RampProtector.RotX=0
	RampProtector.RotY=0
	RampProtector.TransZ=0
	RampProtector.TransY=0
	RampProtector.TransX=0
	PlaySoundAt "KickBack2", KickerRampLock
	Light_Lock2.State=0
	WallLiftRampBlocker1.isdropped=True
	RampDropWall.IsDropped=False
debug.print "TriggerRampTrapped Enabled"
End Sub

Sub tbarrel1_hit ()
	tbarrel1.isdropped=True
End Sub

Sub tbarrel2_hit ()
	tbarrel2.isdropped=True
End Sub

Sub tbarrel3_hit ()
	tbarrel3.isdropped=True
End Sub
Sub tbarrel4_hit ()
	tbarrel4.isdropped=True
End Sub


'*********************************************
'Trigger Golden Reef 
'********************************************
Sub TriggerGoldenReef_Hit
	If bSkillshotReady = True Then ArmTheCanon:End If
	If RoundTheHornActive = True And Light_GR.State=2 Then
		Light_RoundTheHorn.State=0: Light_GR.State=0:RoundTheHornActive = False
		DMD CL(0, "ROUND THE HORN" ), CL(1, "300000"), "", eNone, eBlink, eNone, 3000, True, ""
		AddScore 300000
	End If
End Sub

Dim ArmCanonSkillShot
Sub ArmTheCanon
	SinkShipFlasherTimer.Enabled=1:ATCTargetsDown
	ArmCanonSkillShot=True
End Sub

'*************************************
'CheckAwards available
'**************************************
Dim WizardModeReady
Sub CheckNextAvailableAward
End Sub


'*********************************************
'Triggerleftloop
'********************************************

Sub TriggerLeftLoop_Hit()
	LastSwitchHit = "TriggerLeftLoop" 
		strip1.visible = 1
		vpmtimer.addtimer 350, "f1off '"
		debug.Print " LastSwitchHit-TriggerLeftLoop"	
End Sub

Sub f1off
		strip1.visible = 0
End Sub

Sub TriggerLeftLoopA_Hit()
	LastSwitchHit = "TriggerLeftLoopA" 
		Strip2.visible = 1
		vpmtimer.addtimer 350, "f2off '"
		debug.Print " LastSwitchHit-TriggerLeftLoopA"	
End Sub

Sub f2off
		Strip2.visible = 0
End Sub

Sub TriggerLeftLoopB_Hit()
		AddScore 50
		Strip3.visible = 1
		vpmtimer.addtimer 350, "f3off '"
	If LastSwitchHit = "TriggerLeftLoopA" Then
debug.Print " LastSwitchHit-TriggerLeftLoopB"
		If Light_Patch.State=2 And CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True: Light_Patch.State=1:PlaySound"CO_PatchWet":SinkShipFlasherTimer.Enabled=1:AwardFoundMyPatch: End If
		If Light_Patch.State=1 and Light_Hook.State=1 and Light_Leg.State=1 Then vpmtimer.addtimer 3000, "RTSAward'": End If
		If Light_POTP3.State=2 Then Light_POTP3.State=1:ParrotOffThePorchAward3:End If
		If Light_POTP2.State=2 Then Light_POTP2.State=1:Light_POTP3.State=2:ParrotOffThePorchAward2:End If
		If Light_POTP1.State=2 Then Light_POTP1.State=1:Light_POTP2.State=2:ParrotOffThePorchAward1:End If
		If Light_TC2.State=1 And CalloutActive=False Then NoPlayCallout:End If
		If Light_TC2.State=2 Then
			SinkShipFlasherTimer.Enabled=True
			Light_TC2.State=1:CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_Gull1"
		If hard=1 Then SinkShipFlasherTimer.Enabled=True : AwardJackpot        'vpmtimer.addtimer 2000, "AwardJackpot'"
		If hard=0 Then SinkShipFlasherTimer.Enabled=True : AwardJackpot2       'vpmtimer.addtimer 2000, "AwardJackpot2'"
			vpmtimer.addtimer 3500, "CheckWildSeasAward'"
		End If
		If	SelectAward=False Then CheckRoundTheHorn: debug.Print " CheckRoundTheHorn":End If
	End If
	LastSwitchHit = "TriggerLeftLoopB" 
End Sub

Dim NoPlay
Sub NoPlayCallout
	NoPlay= NoPlay +1
	Select Case NoPlay
		Case 1 : PlaySound "CO_GetBackOnDeckWet":debug.print "NoPlayCall1"
		Case 2 : PlaySound "CO_WollingsworthScurvyWet2":debug.print "NoPlayCall2"
		Case 3 : PlaySound "CO_GiveThatBackWet2": NoPlay=0:debug.print "NoPlayCall3"
	End Select
End Sub

Sub f3off
		Strip3.visible = 0
End Sub

Sub TriggerLeftLoopC_Hit()
	LastSwitchHit = "TriggerLeftLoopC" 
		Strip4.visible = 1
		vpmtimer.addtimer 350, "f4off '"
		debug.Print " LastSwitchHit-TriggerLeftLoopA"	
End Sub

Sub f4off
		Strip4.visible = 0
End Sub

Sub ParrotOffThePorchAward1
	If CalloutActive=False Then CalloutActive=True:PlaySound "CO_Parrot5ozone":CalloutTimer.Enabled=True: debug.print "ParrotAward1":End If
		SmallAwardFlasherTimer.Enabled=1
		DMD CL(0, " 2 MORE SHOTS FOR"), CL(1, "FOR POLLYS GHOST"), "", eBlink, eBlink, eNone, 3000, True, ""
		If Hard=1 Then Addscore 750000: End If
		If Hard=0 Then Addscore 1000000: End If

End Sub

Sub ParrotOffThePorchAward2
	If CalloutActive=False Then CalloutActive=True:PlaySound "CO_Parrot1ozone":CalloutTimer.Enabled=True: debug.print "ParrotAward2":End If
		SmallAwardFlasherTimer.Enabled=1
		DMD CL(0, " 1 MORE SHOTS FOR"), CL(1, "FOR POLLYS GHOST"), "", eBlink, eBlink, eNone, 3000, True, ""
		If Hard=1 Then Addscore 750000: End If
		If Hard=0 Then Addscore 1000000: End If
End Sub

Sub ParrotOffThePorchAward3
		ParrotFlasherTimer.Enabled=1
	If CalloutActive=False Then CalloutActive=True:PlaySound "CO_Parrot2ozone":CalloutTimer.Enabled=True: debug.print "ParrotAward3":End If
		SmallAwardFlasherTimer.Enabled=1
		
		If Hard=1 Then Addscore 750000:DMD CL(0, "OFF THE PERCH"), CL(1, "750000"), "", eBlink, eBlink, eNone, 3000, True, "": End If
		If Hard=0 Then Addscore 1000000:DMD CL(0, "OFF THE PERCH"), CL(1, "1000000"), "", eBlink, eBlink, eNone, 3000, True, "": End If
		vpmtimer.addTimer 2500, "StartParrotGhosts'"
End Sub

Sub StartParrotGhosts
	If CalloutActive=False Then CalloutActive=True:PlaySound "CO_GhostOfPollyOzone":CalloutTimer.Enabled=True: debug.print "ParrotAward3":End If
	ParrotMoveUp
End Sub


Sub AwardFoundMyPatch
		debug.print "AwardFoundMyPatch Active"
		Light_Patch.State=1
	If 	CalloutActive=False Then 
		CalloutActive=True:PlaySound "CO_PatchWet":CalloutTimer.Enabled=True: debug.print "HookFound"
	End If
		
	If Light_Patch.State=1 and Light_Hook.State=1 and Light_Leg.State=1 Then vpmtimer.addtimer 3000, "RTSAward'": End If
	If Hard=1 Then	
		DMD CL(0, "FOUND YA PATCH"), CL(1, "1000000"), "", eNone, eBlink, eNone, 3000, True, ""
		vpmtimer.addtimer 3000, "AwardJackpot'"
		debug.print "HardPatchAward"
	End If

	If Hard=0 Then	
		DMD CL(0, "FOUND YA PATCH"), CL(1, "2000000"), "", eNone, eBlink, eNone, 3000, True, ""
		vpmtimer.addtimer 3000, "AwardJackpot2'"
		debug.print "HardPatchAward"
	End If
	

End Sub


Dim RoundTheHornActive
Sub CheckRoundTheHorn
	RoundTheHornActive=True
	If Light_GR.State=2 And bBallSaverActive = False Then:Light_RoundTheHorn.State=0: Light_GR.State=0:DMD CL(0, "ROUND THE WORLD" ), CL(1, "200000"), "", eNone, eBlink, eNone, 3000, True, "":AddScore 20000:Exit Sub


	If Light_RoundTheHorn.State=2 Then 
		Light_RoundTheHorn.State=1: Light_GR.State=2
		DMD CL(0, "ROUND THE HORN" ), CL(1, "200000"), "", eNone, eBlink, eNone, 3000, True, ""
		AddScore 200000
	End If

	If Light_RoundTheHorn.State=1 Then 
		If CalloutActive=False Then: CalloutActive=True:CalloutTimer.Enabled=True:NoPlayCallout:End If
'		DMD CL(0, "BACK ON DECK" ), CL(1, "10"), "", eNone, eBlink, eNone, 3000, True, ""
		AddScore 10
	End If

	If Light_RoundTheHorn.State=0 Then 
		Light_RoundTheHorn.State=2
		DMD CL(0, "ROUND THE HORN" ), CL(1, "    IS LIT"), "", eNone, eBlink, eNone, 3000, True, ""
		AddScore 1
	End If
End Sub

Sub StopRoundTheHorn
	Light_RoundTheHorn.State=0:Light_GR.State=0: RoundTheHornActive=False
End Sub




'*********************************************
'TriggerLeftRamp
'********************************************
Sub TriggerLeftRamp_Hit()
	If Light_Hook.State=2 Then AwardFoundMyHook: End If
	If Light_TC3.State=1 And CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:NoPlayCallout:End If
 	If Light_TC3.State=2 Then
		Light_TC3.State=1
		PlaySound "CO_Gull2"
		If hard=1 Then SinkShipFlasherTimer.Enabled=True :AwardJackpot		'vpmtimer.addtimer 2000, "AwardJackpot'"
		If hard=0 Then SinkShipFlasherTimer.Enabled=True :AwardJackpot2 	'vpmtimer.addtimer 2000, "AwardJackpot2'"
		CheckWildSeasAward
	End If
End Sub

Sub AwardFoundMyHook
		SinkShipFlasherTimer.Enabled=1
		Light_Hook.State=1
	If 	CalloutActive=False Then 
		CalloutActive=True:PlaySound "CO_HookWet2":CalloutTimer.Enabled=True: debug.print "HookFound"
	End If
	If Light_Patch.State=1 and Light_Hook.State=1 and Light_Leg.State=1 Then vpmtimer.addtimer 3000, "RTSAward'": End If
'	If 	CalloutActive=False Then 
'		CalloutActive=True:PlaySound "CO_HookWet2":CalloutTimer.Enabled=True: debug.print "HookFound"
'	End If
	If Hard=1 Then	DMD CL(0, "FOUND YA HOOK "), CL(1, "1000000 "), "", eBlink, eNone, eNone, 3000, True, "":vpmtimer.addtimer 3500, "AwardJackpot'":End If
	If Hard=0 Then	DMD CL(0, "FOUND YA HOOK "), CL(1, "2000000 "), "", eBlink, eNone, eNone, 3000, True, "":vpmtimer.addtimer 3500, "AwardJackpot'":End If
'	If Light_Patch.State=1 and Light_Hook.State=1 and Light_Leg.State=1 Then RTSAward: End If
End Sub



'*********************************************
'Right Kicker Target
'********************************************

Sub KickerTarget_Hit()
	If Light_Leg.State=2 Then AwardFoundMyLeg: AddScore 100000: End If
	If Light_Patch.State=1 and Light_Hook.State=1 and Light_Leg.State=1 Then vpmtimer.addtimer 3000, "RTSAward'": End If

	If Light_TC4.State=1  And CalloutActive=False Then: CalloutActive=True:CalloutTimer.Enabled=True: NoPlayCallout:End If
	If Light_TC4.State=2 Then
		Light_TC4.State=1:CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_Gull3"
		If hard=1 Then SmallAwardFlasherTimer.Enabled=True : AwardJackpot			'vpmtimer.addtimer 3000, "AwardJackpot'"
		If hard=0 Then SmallAwardFlasherTimer.Enabled=True : AwardJackpot2			'vpmtimer.addtimer 3000, "AwardJackpot2'"
		CheckWildSeasAward 
	End If
	If Light_ExtraBall.State=2 Then AwardExtraBall:End If
	PlaySoundAt "fx_kicker2" , KickerTarget
	vpmtimer.addtimer 1500, "UpKicker'"
	AddScore 50
End Sub

Sub AwardFoundMyLeg
	SinkShipFlasherTimer.Enabled=1	
	Light_Leg.State=1
	If 	CalloutActive=False Then 
		CalloutActive=True:PlaySound "CO_LegWet":CalloutTimer.Enabled=True: debug.print "Leg Found"
	End If
	If Hard=1 Then	DMD CL(0, "FOUND YA LEG "), CL(1, "1000000 "), "", eBlink, eBlink, eNone, 3000, True, "":vpmtimer.addtimer 3000, "AwardJackpot'":End If
	If Hard=0 Then	DMD CL(0, "FOUND YA LEG"), CL(1, "2000000 "), "", eNone, eBlink, eNone, 3000, True, "":vpmtimer.addtimer 3000, "AwardJackpot2'":End If
	If Light_Patch.State=1 and Light_Hook.State=1 and Light_Leg.State=1 Then vpmtimer.addtimer 3000, "RTSAward'": End If
End Sub

Sub WildSeasAward
	AwardSuperJackpot2
End Sub


Dim RTSComplete
Sub RTSAward
		
	If RaiseTheSailsComplete(CurrentPlayer)=False Then CheckModesCompleted:LightAward7.State=1: RaiseTheSailsComplete(CurrentPlayer)=True: debug.print "CheckModesComplete-At RaiseTheSails":End If
	If Hard=1 Then	vpmtimer.addtimer 4500, "AwardSuperJackpot'":End If
	If Hard=0 Then	vpmtimer.addtimer 4500, "AwardSuperJackpot2'":End If		
		RTSComplete=True
		StartGoldenReefAwardSequence
		StopAward5
End Sub


Sub GatePlungerLane_Hit
	LastSwitchHit="GatePlungerLane"
	debug.Print "PlungerGateHit"
End Sub

Sub TriggerDropPlungerwall_Hit
	If LastSwitchHit="GatePlungerLane" Then PlungerWalldropTimer.Enabled=1 
	debug.Print "TriggerPlungerWallDropHit"
End Sub

Sub PlungerWalldropTimer_Timer
	DropPlungerDiversionWall
	PlungerWalldropTimer.Enabled=0
End Sub


Sub DropPlungerDiversionWall
	PlungerDropWall.IsDropped=True
	debug.Print "PlungerWallDropped"
End Sub

Sub RaisePlungerDiversionWall
	PlungerDropWall.IsDropped=False
End Sub

'*********************************************
'UpKicker
'********************************************

Sub UpKicker
	If Light_POTP3.state=2 Then: AwardJackpot
	PlaySoundAt SoundFXDOF("Popper" , 116, DOFPulse, DOFContactors),KickerTarget
	KickerTarget.kick   0, 60, 1.56
	'DOF 116,DOFPulse
End Sub



'************************
'Mermaid Targets
'************************

Sub Target1_Hit
	If WalkThePlankActive=True Then Exit Sub
		l5. State = 1
		LightTargetF.State=1
		AddScore 1
		CheckTreasureTargetLights
End Sub

Sub Target2_Hit
	If WalkThePlankActive=True Then Exit Sub
		l6. State = 1
		LightTargetI.State=1
		AddScore 1
		CheckTreasureTargetLights
End Sub


Sub Target3_Hit
	If WalkThePlankActive=True Then Exit Sub
		l7. State = 1
		LightTargetS1.State=1
		AddScore 1
		CheckTreasureTargetLights
End Sub

Sub Target4_Hit
	If WalkThePlankActive=True Then Exit Sub
		l8. State = 1
		LightTargetH.State=1
		AddScore 1
		CheckTreasureTargetLights
End Sub

Sub Target5_Hit
	If WalkThePlankActive=True Then Exit Sub
		l9. State = 1
		LightTargetE.State=1
		AddScore 1
		CheckTreasureTargetLights
End Sub


Sub Target6_Hit
	If WalkThePlankActive=True Then Exit Sub
		l10. State = 1
		LightTargetS2.State=1
		AddScore 1
		CheckTreasureTargetLights
End Sub


Dim collecttreasure
Sub 	CheckTreasureTargetLights
	If WalkThePlankActive=True Then Exit Sub
	If l5.State + l6.State + l7.State +l8.State + l9.State + l10.State = 6 Then  
		vpmtimer.addtimer 500, "StartWTP'"
		raisewall
	End If
		
'		CheckTotalTreasureCollected
End Sub

'**************************************************************
'WalkThe Plank 
'**************************************************************


Sub KickerWTP_Hit
	PlaySoundAt "fx_kicker_catch" , KickerWTP
	DeactivateWalkThePlank
	vpmtimer.addtimer 3000, "UpThePlank'"
End Sub

Sub TriggerWTPOff_Hit
	BallLightActive=False: Debug.Print "BallLightOff"
	PlaySound "HitUpToTheGunnels"
	Light_WTP2.State=0
End Sub

Sub UpThePlank
	PlaySoundAt SoundFXDOF("Popper" , 113, DOFPulse, DOFContactors) , KickerWTP
	KickerWTP.kick  0, 60, 1.56
'	WTP.Enabled=False
End Sub

Sub TriggerPlankHold_Hit()
		SinkShipFlasherTimer.Enabled=1
		PlankCallAward
		vpmtimer.addtimer 3000, "OffThePlank'" 'Pirate lets ball go, ball light on, ball saver spinner on
End Sub

Sub PlankCallAward
		If PlankAward3Active(CurrentPlayer)=1 Then CalloutActive=True:CalloutTimer.Enabled=1:PlaySound "CO_LongWalkWet":vpmtimer.addtimer 1000, "CheckMermaidEasyHard'":SelectPlankAward1:debug.print "Set Plank1Light Red":Exit Sub

		If PlankAward2Active(CurrentPlayer)=1 Then CalloutActive=True:CalloutTimer.Enabled=1:PlaySound "CO_FeedTheFishWet":vpmtimer.addtimer 1000, "CheckFishEasyHard'":SelectPlankAward3:debug.print "Set Plank3 Light Green":Exit Sub

		If PlankAward1Active(CurrentPlayer)=1  Then CalloutActive=True:CalloutTimer.Enabled=1: PlaySound "CO_TootlesOzone":vpmtimer.addtimer 1000, "CheckPlankEasyHard'":SelectPlankAward2:debug.print "Plank Light2 Blue": Exit Sub
End Sub


Sub CheckMermaidEasyHard


		If Hard=1 Then
				DMD CL(0, " MERMAID FANTASY" ), CL(1, "  HAHAHAHA"), "", eNone, eBlink, eNone, 3000, True, ""
				vpmtimer.addtimer 5000, "AwardSuperJackpot2'"
		End If
		If Hard=0 Then
				DMD CL(0, " MERMAID FANTASY" ), CL(1, "  HAHAHAHA"), "", eNone, eBlink, eNone, 3000, True, ""
				vpmtimer.addtimer 5000, "AwardSuperJackpot2'"
		End If
End Sub

Sub CheckFishEasyHard


		If Hard=1 Then
				DMD CL(0, " FEED THE FISH" ), CL(1, "SCURVY DOG"), "", eNone, eBlink, eNone, 3000, True, ""
				vpmtimer.addtimer 5000, "AwardSuperJackpot2'"
				LightAward8.State=1
		If WalkThePlankComplete(CurrentPlayer)=False Then CheckModesCompleted:debug.print "CheckModesComplete-WalkThePlank": End If
				WalkThePlankComplete(CurrentPlayer)=True
		End If

		If Hard=0 Then
				DMD CL(0, " FEED THE FISH" ), CL(1, "SCURVY DOG"), "", eNone, eBlink, eNone, 3000, True, ""
				vpmtimer.addtimer 5000, "AwardSuperJackpot2'"
		End If
End Sub

Sub CheckPlankEasyHard


		If Hard=1 Then
				DMD CL(0, " WALK THE PLANK" ), CL(1, "SCUMBAG"), "", eNone, eBlink, eNone, 3000, True, ""
				vpmtimer.addtimer 5000, "AwardSuperJackpot2'"
		End If
		If Hard=0 Then
				DMD CL(0, " WALK THE PLANK" ), CL(1, "SCUMBAG"), "", eNone, eBlink, eNone, 3000, True, ""
				vpmtimer.addtimer 5000, "AwardSuperJackpot2'"
				LightAward8.State=1
		If WalkThePlankComplete(CurrentPlayer)=False Then CheckModesCompleted: debug.print "CheckModesComplete-WalkThePlank":End If
				WalkThePlankComplete(CurrentPlayer)=True
		End If
End Sub


Dim BallLightActive
Sub OffThePlank
	BallLightActive=True:Debug.Print "BallLightOn"
	Pirate.Collidable=False
	vpmtimer.addtimer 1000, "PirateStopper'"
	if SpinSave=False Then
		SpinSave=True
		StartWheelSpinnerDrain
		debug.print "drainSpinStarted"
		spindiscimg_drainsave.Visible=1
'		spinningdraintimer.Enabled=1
		vpmtimer.addtimer 9000, "DiscInvisible'"
		vpmtimer.addtimer 12000, "StopWheelSpinnerDrain'"
	End If
End Sub

Sub PirateStopper
	Pirate.Collidable=True
	WalkThePlankActive=False
	DeactivateWalkThePlank
End Sub

'*********************************************
'StartWalkThe Plank
'********************************************
Dim WalkThePlankActive
Sub StartWTP
	WalkThePlankActive=True
	AcitivateWalkThePlank
	ResetFishesLights
	WalkThePlankStopTimer.Enabled=1
End Sub


Dim WTPDone

Sub WalkThePlankStopTimer_Timer
'		If WalkThePlankActive=False Then Exit Sub
		WalkThePlankActive=False
		WTPDone=True
If  LightPlankReady.State=2 Then SetPlank1: Exit Sub
If  LightPlankFish.State=2  Then SetPlank2: Exit Sub
If 	LightPlankMermaid.State=2 Then SetPlank3: Exit Sub
	Light_WTP2.State=0 
	WalkThePlankStopTimer.Enabled=0
End Sub

'***********************
'Walk the plank Diverter
'************************


Sub AcitivateWalkThePlank
	PlankAwardLight
	PlankDiv1.rotatetostart
	PlankDiv2.rotatetostart
	PlaySoundAt "fx_diverter" , PlankDiv1
End Sub


Sub PlankAwardLight
	Light_WTP2.State=2
	If PlankAward1Active(CurrentPlayer)=1 Then LightPlankReady.State=2:LightPlankFish.State=0:LightPlankMermaid.State=0: End If
	If PlankAward2Active(CurrentPlayer)=1 Then LightPlankFish.State=2: LightPlankReady.State=0:LightPlankMermaid.State=0:End If
	If PlankAward3Active(CurrentPlayer)=1 Then LightPlankMermaid.State=2:LightPlankFish.State=0:LightPlankReady.State=0: End If
End Sub


Sub DeactivateWalkThePlank
	debug.print "Walk The Plank Timed Out"
		WalkThePlankActive=False
	'	If WalkThePlankActive=True Then Exit Sub
		PlankDiv1.rotatetoend
		PlankDiv2.rotatetoend
		PlaySoundAt "fx_diverter" , PlankDiv1
		Light_WTP2.State=0
		WalkThePlankStopTimer.Enabled=0
End Sub

'*************************************************************************************************
'Select the next plank light if walk the plank is active or reset the plank award if not completed
'*************************************************************************************************
Sub SelectPlankAward1
	If WalkThePlankActive=False And PlankAward1Active(CurrentPlayer)=1 Then SetPlank1: Exit Sub 						'Reset Plank1 deady for next mermaid spinner activation
	PlankAward1Active(CurrentPlayer)=1:PlankAward2Active(CurrentPlayer)=0:PlankAward3Active(CurrentPlayer)=0
	LightPlankReady.State=1:LightPlankFish.State=0:LightPlankMermaid.State=0
	debug.print "Deactivate Plank Award3 Activate Award 1"
End Sub

Sub SelectPlankAward2
	If WalkThePlankActive=False And PlankAward2Active(CurrentPlayer)=1 Then SetPlank3: Exit Sub					'Reset Plank1 deady for next mermaid spinner activation
	PlankAward1Active(CurrentPlayer)=0:PlankAward2Active(CurrentPlayer)=1:PlankAward3Active(CurrentPlayer)=0
	LightPlankReady.State=0:LightPlankFish.State=1:LightPlankMermaid.State=0
	debug.print "Deactivate Plank Award1 Activate Award 2"
End Sub

Sub SelectPlankAward3
	If WalkThePlankActive=False And PlankAward3Active(CurrentPlayer)=1 Then SetPlank2: Exit Sub					'Reset Plank1 deady for next mermaid spinner activation
	PlankAward1Active(CurrentPlayer)=0:PlankAward2Active(CurrentPlayer)=0:PlankAward3Active(CurrentPlayer)=1
	LightPlankReady.State=0:LightPlankFish.State=0:LightPlankMermaid.State=1
	debug.print "Deactivate Plank Award2 Activate Award 3"
End Sub

'Resets if plank is not completed and the walk the plank timer has timed out

Sub SetPlank1
	PlankAward1Active(CurrentPlayer)=1:PlankAward2Active(CurrentPlayer)=0:PlankAward3Active(CurrentPlayer)=0
	LightPlankReady.State=1:LightPlankFish.State=0:LightPlankMermaid.State=0
	debug.print "Plank1Award reset because nt achieved in time"
End Sub

Sub SetPlank2
	PlankAward1Active(CurrentPlayer)=0:PlankAward2Active(CurrentPlayer)=1:PlankAward3Active(CurrentPlayer)=0
	LightPlankReady.State=0:LightPlankFish.State=1:LightPlankMermaid.State=0
	debug.print "Plank2Award reset because nt achieved in time"
End Sub

Sub SetPlank3
	PlankAward1Active(CurrentPlayer)=0:PlankAward2Active(CurrentPlayer)=0:PlankAward3Active(CurrentPlayer)=1
	LightPlankReady.State=0:LightPlankFish.State=0:LightPlankMermaid.State=1
	debug.print "Plank3Award reset because nt achieved in time"
End Sub


'************************
'TreasureJacvkots
'************************


Sub LightTreasure
'	If Light_TC1.State=0 Then:Light_TC1.State=2
End Sub



'***********************
'Mermaid Turntable
'***********************
Sub TreasureImageSpin_Timer
'	Debug.print "MermaidSpinDiscEnabledatFishes"
	spindiscimg.rotz = spindiscimg.rotz + 2
	spindiscimg.Visible=True
End Sub


Sub spinning_timer
'	TreasureImageSpin.Enabled=False
	spindiscimg.rotz = spindiscimg.rotz + 10
	DOF 124,DOFpulse 'Beacon on -for WildSeasSpinner
	DOF 119,DOFpulse	'Shaker on for wild seas'
	DOF 126,DOFpulse	'Fan on- for main nebula /Mermaid spinner)
End Sub

dim spinner
Set spinner = New cvpmTurntable
With spinner
	.InitTurntable spindisc, 100
	.SpinDown = 20
	.CreateEvents "spinner"
End With
spinner.MotorOn = false

'****************************************
'Raisewall Sub'
'****************************************
Sub raisewall	
	TriggerTrapped.Enabled=0
	AddScore 1000000
	'		if bMultiBallMode = true then exit Sub
	'------------------------------------------------
	TreasureSpinnerWall.IsDropped= False
	'------------------------------------------------
	spinner.MotorOn = true
	spinning.enabled = True
	vpmtimer.addtimer 3000, "dropwall '"
End Sub

Sub StopMotorTimer
	MotorSpinnerTimer.Enabled=False
End Sub


'*************************
'LiftWall
'*************************
Sub LiftWall
	TreasureSpinnerWall.IsDropped= False
	TriggerTrapped.Enabled=0
	debug.print "LiftWall"
End Sub



'*************************************
'dropwall
'************************************
Sub dropwall
	TreasureSpinnerWall.IsDropped= True
	spinner.MotorOn = false
	spinning.enabled = false
	vpmtimer.addtimer 500, "ResetFishesLights'"
End Sub

'*************************************
'StopMotorSpinner
'**************************************
Sub 	StopSpinnerMotor
	debug.print "StopMotor"
	spinner.MotorOn = false
	spinning.enabled = false
	spindiscimg.rotz = spindiscimg.rotz + 10
End Sub

Sub ResetFishesLights
	l5.State=0:l6.State=0:l7.State=0:l8.State=0:l9.State=0:l10.State=0
	LightTargetF.State=0:LightTargetI.State=0:LightTargetS1.State=0:LightTargetH.State=0:LightTargetE.State=0:LightTargetS2.State=0
End Sub


'****************************************
' Enable Kicker in case a ball gets stuck This is enable for 6 seconds after the ball is spun out

Sub TriggerTrappedStartTimer_Timer()
	EnableTriggerTrapped
	debug.print "TriggerTrappedTimerEnabled"
End Sub


Sub EnableTriggerTrapped
	TriggerTrapped.Enabled=1
	debug.print "TriggerTrappedEnabled"
End Sub

Sub TriggerTrapped_Hit()
	dropwall
'	vpmtimer.addtimer 500, "LiftWall '"
End Sub


Sub DisableTriggerTrapped
	TriggerTrapped.Enabled=0	
End sub


'************
' Varitarget
'************

Sub StartVariArrowLights
	Light_VariArrow1.State=1:Light_VariArrow2.State=1:Light_VariArrow3.State=1:Light_VariArrow4.State=1
End Sub

Sub StopVariArrowLights
	Light_VariArrow1.State=0:Light_VariArrow2.State=0:Light_VariArrow3.State=0:Light_VariArrow4.State=0
End Sub

Dim variawarded, vtpos, varipos
variawarded = False
vtpos= Array(20,18,16,14,12,10,8,6,4,2,0,-2,-4,-6,-8,-10,-12,-14,-16,-18,-20,-22)

Sub vt_Hit(idx)
Dim x
If idx = 21 Then StartTikiShake:StartChickenJump 'the last vt pos
If ActiveBall.VelY <0 Then
varipos = idx
	ActiveBall.VelY = ActiveBall.VelY * 0.915
    PlaySound "fx_solenoidoff"
    varitarget.roty = vtpos(idx)
Else
	If VTTimer.Enabled = False Then 
	 VTTimer.Interval = 300
	 VTTimer.Enabled = True
	End If
End If
End Sub

Sub VTTimer_Timer()
	VTTimer.Interval = 30
varipos = varipos -1
If varipos <0 Then varipos = 0
If varipos > 21 Then varipos = 21
    varitarget.roty = vtpos(varipos)
		If varipos = 19 AND variawarded = False Then 
			AddScore 5000: AddBonus 5:variawarded = True
		End If
		If varipos = 16 AND variawarded = False Then 
			AddScore 4000: variawarded = True
		End If
		If varipos = 13 AND variawarded = False Then 
			AddScore 3000: variawarded = True
		End If
		If varipos = 10 AND variawarded = False Then 
			AddScore 2000: variawarded = True
		End If
		If varipos = 7 AND variawarded = False Then 
			AddScore 1000: variawarded = True
		End If
		If varipos = 4 AND variawarded = False Then 
			AddScore 1000: variawarded = True
		End If
        If varipos = 0 Then
		  variawarded = False
        VTTimer.Enabled = False
		End If
End Sub

Sub TriggerShipHit_Hit 
	DOF 115,DOFPulse
	TotemLightTimer.Enabled=1
	LightTikiRightEye.State=1:LightTikiLeftEye.State=1
	TikiLightTimer.Enabled=True
	CheckSinkShipStatus

End Sub

dim totemlightflash:totemlightflash=0
Sub TotemLightTimer_Timer
	totemlightflash=totemlightflash+1
	Select Case totemlightflash
		case 1: Strip1.visible = 1:Strip2.visible = 1:Strip3.visible = 1:Strip4.visible = 1:Strip5.visible = 1:FlasherVariHit.Visible=1:CanonVariTargetFlash2.Visible=1
		case 3:	Strip1.visible = 0:Strip2.visible = 0:Strip3.visible = 0:Strip4.visible = 0:Strip5.visible = 0:FlasherVariHit.Visible=0:CanonVariTargetFlash2.Visible=0
		case 5:	Strip1.visible = 1:Strip2.visible = 1:Strip3.visible = 1:Strip4.visible = 1:Strip5.visible = 1:FlasherVariHit.Visible=1:CanonVariTargetFlash2.Visible=1
		case 7:	Strip1.visible = 0:Strip2.visible = 0:Strip3.visible = 0:Strip4.visible = 0:Strip5.visible = 0:FlasherVariHit.Visible=0:CanonVariTargetFlash2.Visible=0
		case 9:	Strip1.visible = 1:Strip2.visible = 1:Strip3.visible = 1:Strip4.visible = 1:Strip5.visible = 1:FlasherVariHit.Visible=1:CanonVariTargetFlash2.Visible=1
		case 11:Strip1.visible = 0:Strip2.visible = 0:Strip3.visible = 0:Strip4.visible = 0:Strip5.visible = 0:FlasherVariHit.Visible=0:CanonVariTargetFlash2.Visible=0
		case 13:Strip1.visible = 1:Strip2.visible = 1:Strip3.visible = 1:Strip4.visible = 1:Strip5.visible = 1
		case 15:Strip1.visible = 0:Strip2.visible = 0:Strip3.visible = 0:Strip4.visible = 0:Strip5.visible = 0
		case 17:Strip1.visible = 1:Strip2.visible = 1:Strip3.visible = 1:Strip4.visible = 1:Strip5.visible = 1
		case 19:Strip1.visible = 0:Strip2.visible = 0:Strip3.visible = 0:Strip4.visible = 0:Strip5.visible = 0
		case 21:Strip1.visible = 1:Strip2.visible = 1:Strip3.visible = 1:Strip4.visible = 1:Strip5.visible = 1
		case 23:Strip1.visible = 0:Strip2.visible = 0:Strip3.visible = 0:Strip4.visible = 0:Strip5.visible = 0:TotemLightTimer.Enabled=0:totemlightflash=0
	End Select
End Sub

Sub CheckSinkShipStatus     '******This is a light setting routine and checks requirements the ship damage to set flags*******
	If 	CanonJustFired=True Then: lv4.State=1:lv3.State=1:lv2.State=1:lv1.State=1:CheckStatusOfDamage:OverlayShipSink.Visible=1:Exit Sub
	If lv4.State=2 Then:lv4.State=1:Fire4:CheckStatusOfDamage:OverlayShipSink.Visible=1:End If
	If lv3.State=2 Then
		lv3.State=1:lv4.State=2:Fire3:CheckStatusOfDamage:SmallAwardFlasherTimer.Enabled=1
		DMD CL(0, "WHOS LAUGHING NOW"), CL(1, "SURVY DOG"), "", eBlink, eBlink, eNone, 3000, True, ""
	End If
	If lv2.State=2 Then
		lv2.State=1:lv3.State=2:Fire2:CheckStatusOfDamage:SmallAwardFlasherTimer.Enabled=1
		DMD CL(0, "    MY NAME IS"), CL(1, "CAPTAIN OCTIVA"), "", eBlink, eBlink, eNone, 3000, True, ""
	End If
	If lv1.State=2 Then
		lv1.State=1:lv2.State=2:Fire1:CheckStatusOfDamage:SmallAwardFlasherTimer.Enabled=1
		DMD CL(0, "  COP THAT"), CL(1, "SCUMBAG  "), "", eBlink, eBlink, eNone, 3000, True, ""
	End If
End Sub


Sub Fire1
	CanonFlashFireLarge1.Visible=True
	CanonFlashFireLarge2.Visible=False
	CanonFlashFireLarge3.Visible=False
	CanonFlashFireLarge1.TimerEnabled=True
	CanonFlashFireLarge2.TimerEnabled=False
	CanonFlashFireLarge3.TimerEnabled=False
End Sub

Sub CanonFlashFireLarge1_Timer
	CanonFlashFireLarge1.Visible=False
	CanonFlashFireLarge2.Visible=True
	CanonFlashFireLarge3.Visible=False
	CanonFlashFireLarge1.TimerEnabled=False
	CanonFlashFireLarge2.TimerEnabled=True
	CanonFlashFireLarge3.TimerEnabled=False
End Sub

Sub CanonFlashFireLarge2_Timer
	CanonFlashFireLarge1.Visible=False
	CanonFlashFireLarge2.Visible=False
	CanonFlashFireLarge3.Visible=True
	CanonFlashFireLarge1.TimerEnabled=False
	CanonFlashFireLarge2.TimerEnabled=False
	CanonFlashFireLarge3.TimerEnabled=True
End Sub

Sub CanonFlashFireLarge3_Timer
	CanonFlashFireLarge1.Visible=True
	CanonFlashFireLarge2.Visible=False
	CanonFlashFireLarge3.Visible=False
	CanonFlashFireLarge1.TimerEnabled=True
	CanonFlashFireLarge2.TimerEnabled=False
	CanonFlashFireLarge3.TimerEnabled=False
End Sub

Sub Fire2
	CanonFlashFireSmall1.Visible=True
	CanonFlashFireSmall2.Visible=False
	CanonFlashFireSmall1.TimerEnabled=True
	CanonFlashFireSmall2.TimerEnabled=False
End Sub

Sub CanonFlashFireSmall1_Timer
	CanonFlashFireSmall1.Visible=False
	CanonFlashFireSmall2.Visible=True
	CanonFlashFireSmall1.TimerEnabled=False
	CanonFlashFireSmall2.TimerEnabled=True
End Sub

Sub CanonFlashFireSmall2_Timer
	CanonFlashFireSmall2.Visible=False
	CanonFlashFireSmall1.Visible=True
	CanonFlashFireSmall2.TimerEnabled=False
	CanonFlashFireSmall1.TimerEnabled=True
End Sub

Sub Fire3
	CanonFlashTopFire1.Visible=True
	CanonFlashTopFire2.Visible=False
	CanonFlashTopFire1.TimerEnabled=True
	CanonFlashTopFire2.TimerEnabled=False
End Sub

Sub CanonFlashTopFire1_Timer
	CanonFlashTopFire1.Visible=False
	CanonFlashTopFire2.Visible=True
	CanonFlashTopFire1.TimerEnabled=False
	CanonFlashTopFire2.TimerEnabled=True
End Sub

Sub CanonFlashTopFire2_Timer
	CanonFlashTopFire2.Visible=False
	CanonFlashTopFire1.Visible=True
	CanonFlashTopFire2.TimerEnabled=False
	CanonFlashTopFire1.TimerEnabled=True
End Sub

Sub Fire4
	CanonFlashFireRain1.Visible=True
	CanonFlashFireRain2.Visible=False
	CanonFlashFireRain3.Visible=False
	CanonFlashFireRain4.Visible=False
	CanonFlashFireRain5.Visible=False
	CanonFlashFireRain1.TimerEnabled=True
	CanonFlashFireRain2.TimerEnabled=False
	CanonFlashFireRain3.TimerEnabled=False
	CanonFlashFireRain4.TimerEnabled=False
	CanonFlashFireRain5.TimerEnabled=False
End Sub

Sub CanonFlashFireRain1_Timer

	CanonFlashFireRain1.Visible=False
	CanonFlashFireRain2.Visible=True
	CanonFlashFireRain3.Visible=False
	CanonFlashFireRain4.Visible=False
	CanonFlashFireRain5.Visible=False
	CanonFlashFireRain1.TimerEnabled=False
	CanonFlashFireRain2.TimerEnabled=True
	CanonFlashFireRain3.TimerEnabled=False
	CanonFlashFireRain4.TimerEnabled=False
	CanonFlashFireRain5.TimerEnabled=False
End Sub

Sub CanonFlashFireRain2_Timer
	CanonFlashFireRain1.Visible=False
	CanonFlashFireRain2.Visible=False
	CanonFlashFireRain3.Visible=True
	CanonFlashFireRain4.Visible=False
	CanonFlashFireRain5.Visible=False
	CanonFlashFireRain1.TimerEnabled=False
	CanonFlashFireRain2.TimerEnabled=False
	CanonFlashFireRain3.TimerEnabled=True
	CanonFlashFireRain4.TimerEnabled=False
	CanonFlashFireRain5.TimerEnabled=False
End Sub

Sub CanonFlashFireRain3_Timer
	CanonFlashFireRain1.Visible=False
	CanonFlashFireRain2.Visible=False
	CanonFlashFireRain3.Visible=False
	CanonFlashFireRain4.Visible=True
	CanonFlashFireRain5.Visible=False
	CanonFlashFireRain1.TimerEnabled=False
	CanonFlashFireRain2.TimerEnabled=False
	CanonFlashFireRain3.TimerEnabled=False
	CanonFlashFireRain4.TimerEnabled=True
	CanonFlashFireRain5.TimerEnabled=False
End Sub

Sub CanonFlashFireRain4_Timer
	CanonFlashFireRain1.Visible=False
	CanonFlashFireRain2.Visible=False
	CanonFlashFireRain3.Visible=False
	CanonFlashFireRain4.Visible=False
	CanonFlashFireRain5.Visible=True
	CanonFlashFireRain1.TimerEnabled=False
	CanonFlashFireRain2.TimerEnabled=False
	CanonFlashFireRain3.TimerEnabled=False
	CanonFlashFireRain4.TimerEnabled=False
	CanonFlashFireRain5.TimerEnabled=True
End Sub

Sub CanonFlashFireRain5_Timer
	CanonFlashFireRain1.Visible=True
	CanonFlashFireRain2.Visible=False
	CanonFlashFireRain3.Visible=False
	CanonFlashFireRain4.Visible=False
	CanonFlashFireRain5.Visible=False
	CanonFlashFireRain1.TimerEnabled=True
	CanonFlashFireRain2.TimerEnabled=False
	CanonFlashFireRain3.TimerEnabled=False
	CanonFlashFireRain4.TimerEnabled=False
	CanonFlashFireRain5.TimerEnabled=False
End Sub

Sub StopShipFires
	CanonFlashFireLarge1.TimerEnabled=False
	CanonFlashFireLarge2.TimerEnabled=False
	CanonFlashFireLarge3.TimerEnabled=False
	CanonFlashFireSmall1.TimerEnabled=False
	CanonFlashFireSmall2.TimerEnabled=False
	CanonFlashTopFire2.TimerEnabled=False
	CanonFlashTopFire1.TimerEnabled=False
	CanonFlashFireRain1.TimerEnabled=False
	CanonFlashFireRain2.TimerEnabled=False
	CanonFlashFireRain3.TimerEnabled=False
	CanonFlashFireRain4.TimerEnabled=False
	CanonFlashFireRain5.TimerEnabled=False
	CanonFlashFireLarge1.Visible=False
	CanonFlashFireLarge2.Visible=False
	CanonFlashFireLarge3.Visible=False
	CanonFlashFireSmall1.Visible=False
	CanonFlashFireSmall2.Visible=False
	CanonFlashTopFire2.Visible=False
	CanonFlashTopFire1.Visible=False
	CanonFlashFireRain1.Visible=False
	CanonFlashFireRain2.Visible=False
	CanonFlashFireRain3.Visible=False
	CanonFlashFireRain4.Visible=False
	CanonFlashFireRain5.Visible=False
End Sub

Sub StartShipFires
	CanonFlashFireLarge1.TimerEnabled=True
	CanonFlashFireLarge2.TimerEnabled=True
	CanonFlashFireLarge3.TimerEnabled=True
	CanonFlashFireSmall1.TimerEnabled=True
	CanonFlashFireSmall2.TimerEnabled=True
	CanonFlashTopFire2.TimerEnabled=True
	CanonFlashTopFire1.TimerEnabled=True
	CanonFlashFireRain1.TimerEnabled=True
	CanonFlashFireRain2.TimerEnabled=True
	CanonFlashFireRain3.TimerEnabled=True
	CanonFlashFireRain4.TimerEnabled=True
	CanonFlashFireRain5.TimerEnabled=True
	CanonFlashFireLarge1.Visible=True
	CanonFlashFireLarge2.Visible=True
	CanonFlashFireLarge3.Visible=True
	CanonFlashFireSmall1.Visible=True
	CanonFlashFireSmall2.Visible=True
	CanonFlashTopFire2.Visible=True
	CanonFlashTopFire1.Visible=True
	CanonFlashFireRain1.Visible=True
	CanonFlashFireRain2.Visible=True
	CanonFlashFireRain3.Visible=True
	CanonFlashFireRain4.Visible=True
	CanonFlashFireRain5.Visible=True
End Sub


Sub CheckShipsSunk
	ShipBonusCount(CurrentPlayer)=ShipBonusCount(CurrentPlayer)+1
	ShipsSunk(CurrentPlayer)=ShipsSunk(CurrentPlayer)+1
	If ShipCount(CurrentPlayer)=8 Then ShipCount(CurrentPlayer)=0:debug.print "ShipCount(currentPlayer)Reset=0"
ShipCount(CurrentPlayer)=ShipCount(CurrentPlayer)+1
Select Case ShipCount(CurrentPlayer)
			Case 0:Flasher1.Visible=0:Flasher2.Visible=0:Flasher3.Visible=0:Flasher4.Visible=0:Flasher5.Visible=0:Flasher6a.Visible=0:Flasher7.Visible=0:Flasher8.Visible=0:Flasher9.Visible=0

			Case 1:Flasher1.Visible=0:Flasher2.Visible=0:Flasher3.Visible=0:Flasher4.Visible=0:Flasher5.Visible=0:Flasher6a.Visible=0:Flasher7.Visible=0:Flasher8.Visible=0:Flasher9.Visible=0

			Case 2:Flasher1.Visible=0:Flasher2.Visible=0:Flasher3.Visible=0:Flasher4.Visible=0:Flasher5.Visible=0:Flasher6a.Visible=0:Flasher7.Visible=0:Flasher8.Visible=0:Flasher9.Visible=0
					vpmtimer.addtimer 6000, "ExtraBallHurryUp'"
			Case 3:Flasher1.Visible=0:Flasher2.Visible=0:Flasher3.Visible=0:Flasher4.Visible=0:Flasher5.Visible=0:Flasher6a.Visible=0:Flasher7.Visible=0:Flasher8.Visible=0:Flasher9.Visible=0
					vpmtimer.addtimer 6000, "StartDaveyJones'"

			Case 4:Flasher1.Visible=0:Flasher2.Visible=0:Flasher3.Visible=0:Flasher4.Visible=0:Flasher5.Visible=0:Flasher6a.Visible=0:Flasher7.Visible=0:Flasher8.Visible=0:Flasher9.Visible=0
					vpmtimer.addtimer 6000, "ExtraBallHurryUp'"
			Case 5:Flasher1.Visible=0:Flasher2.Visible=0:Flasher3.Visible=0:Flasher4.Visible=0:Flasher5.Visible=0:Flasher6a.Visible=0:Flasher7.Visible=0:Flasher8.Visible=0:Flasher9.Visible=0

			Case 6:Flasher1.Visible=0:Flasher2.Visible=0:Flasher3.Visible=0:Flasher4.Visible=0:Flasher5.Visible=0:Flasher6a.Visible=0:Flasher7.Visible=0:Flasher8.Visible=0:Flasher9.Visible=0
					vpmtimer.addtimer 6000, "StartDaveyJones'"

			Case 7:Flasher1.Visible=0:Flasher2.Visible=0:Flasher3.Visible=0:Flasher4.Visible=0:Flasher5.Visible=0:Flasher6a.Visible=0:Flasher7.Visible=0:Flasher8.Visible=0:Flasher9.Visible=0
					vpmtimer.addtimer 6000, "ExtraBallHurryUp'"

			Case 8:Flasher1.Visible=0:Flasher2.Visible=0:Flasher3.Visible=0:Flasher4.Visible=0:Flasher5.Visible=0:Flasher6a.Visible=0:Flasher7.Visible=0:Flasher8.Visible=0:Flasher9.Visible=0
					vpmtimer.addtimer 6000, "StartDaveyJones'"

			Case 9:Flasher1.Visible=0:Flasher2.Visible=0:Flasher3.Visible=0:Flasher4.Visible=0:Flasher5.Visible=0:Flasher6a.Visible=0:Flasher7.Visible=0:Flasher8.Visible=0:Flasher9.Visible=0:vpmtimer.addtimer 3000,"ResetShipCount'":debug.print "ShipCountReset=0"
					
End Select

debug.print "ShipsSunk..Can be greater than 9"
End Sub

Sub ResetShipCount
		ShipCount(CurrentPlayer)=0
		Flasher9.Visible=0:StartDaveyJones
		vpmtimer.addtimer 6000, "ExtraBallHurryUp'"
End Sub

Sub FirstShipSelect
	Select Case Int(Rnd*8)
		Case 0: 	debug.print "Ship1RandomSelection"
					If Ship1Sunk(CurrentPlayer)=0 Then Ship1Now(CurrentPlayer)=1:ShipNowSet

		Case 1: 	debug.print "Ship2RandomSelection"
					If Ship2Sunk(CurrentPlayer)=0 Then Ship2Now(CurrentPlayer)=1:ShipNowSet

		Case 2: 	debug.print "Ship3RandomSelection"
					If Ship3Sunk(CurrentPlayer)=0 Then Ship3Now(CurrentPlayer)=1:ShipNowSet


		Case 3:		debug.print "Ship4RandomSelection"
					If Ship4Sunk(CurrentPlayer)=0 Then Ship4Now(CurrentPlayer)=1:ShipNowSet

		Case 4:		debug.print "Ship5RandomSelection"
					If Ship5Sunk(CurrentPlayer)=0 Then Ship5Now(CurrentPlayer)=1:ShipNowSet

		Case 5: 	debug.print "Ship6RandomSelection"
					If Ship6Sunk(CurrentPlayer)=0 Then Ship6Now(CurrentPlayer)=1:ShipNowSet

		Case 6:		debug.print "Ship7RandomSelection"
					If Ship7Sunk(CurrentPlayer)=0 Then Ship7Now(CurrentPlayer)=1:ShipNowSet

		Case 7:		debug.print "Ship8RandomSelection"
					If Ship8Sunk(CurrentPlayer)=0 Then Ship8Now(CurrentPlayer)=1:ShipNowSet

	End Select
End Sub




Sub NextShip
debug.print "NextShipSelect"

	If ShipsSunk(CurrentPlayer) < 8 Then RandomShipSelect: debug.print "<8 ShipsSunk"
	If ShipsSunk(CurrentPlayer) = 8 Then StartScurvyDick: debug.print "=8 ShipsSunk-StartScurvyDick"
End Sub

Sub RandomShipSelect
	Select Case Int(Rnd*8)
		Case 0: 	debug.print "Ship1RandomSelection"
					If Ship1Sunk(CurrentPlayer)=0 Then Ship1Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship1Available":Exit Sub
					If Ship2Sunk(CurrentPlayer)=0 Then Ship2Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship1NotAvailable 2in":Exit Sub
					If Ship3Sunk(CurrentPlayer)=0 Then Ship3Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship1NotAvailable 3in":Exit Sub
					If Ship4Sunk(CurrentPlayer)=0 Then Ship4Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship1NotAvailable 4in":Exit Sub
					If Ship5Sunk(CurrentPlayer)=0 Then Ship5Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship1NotAvailable 5in":Exit Sub
 					If Ship6Sunk(CurrentPlayer)=0 Then Ship6Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship1NotAvailable 6in":Exit Sub
					If Ship7Sunk(CurrentPlayer)=0 Then Ship7Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship1NotAvailable 7in":Exit Sub
					If Ship8Sunk(CurrentPlayer)=0 Then Ship8Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship1NotAvailable 8in":Exit Sub

		Case 1: 	debug.print "Ship2RandomSelection"
					If Ship2Sunk(CurrentPlayer)=0 Then Ship2Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship2Available":Exit Sub
					If Ship3Sunk(CurrentPlayer)=0 Then Ship3Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship2NotAvailable 3in":Exit Sub
					If Ship4Sunk(CurrentPlayer)=0 Then Ship4Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship2NotAvailable 4in":Exit Sub
					If Ship5Sunk(CurrentPlayer)=0 Then Ship5Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship2NotAvailable 5in":Exit Sub
					If Ship6Sunk(CurrentPlayer)=0 Then Ship6Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship2NotAvailable 6in":Exit Sub
 					If Ship7Sunk(CurrentPlayer)=0 Then Ship7Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship2NotAvailable 7in":Exit Sub
					If Ship8Sunk(CurrentPlayer)=0 Then Ship8Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship2NotAvailable 8in":Exit Sub
					If Ship1Sunk(CurrentPlayer)=0 Then Ship1Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship2NotAvailable 1in":Exit Sub

		Case 2: 	debug.print "Ship3RandomSelection"
					If Ship3Sunk(CurrentPlayer)=0 Then Ship3Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship3Available":Exit Sub
					If Ship4Sunk(CurrentPlayer)=0 Then Ship4Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship3NotAvailable 4in":Exit Sub
					If Ship5Sunk(CurrentPlayer)=0 Then Ship5Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship3NotAvailable 5in":Exit Sub
					If Ship6Sunk(CurrentPlayer)=0 Then Ship6Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship3NotAvailable 6in":Exit Sub
					If Ship7Sunk(CurrentPlayer)=0 Then Ship7Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship3NotAvailable 7in":Exit Sub
 					If Ship8Sunk(CurrentPlayer)=0 Then Ship8Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship3NotAvailable 8in":Exit Sub
					If Ship1Sunk(CurrentPlayer)=0 Then Ship1Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship3NotAvailable 1in":Exit Sub
					If Ship2Sunk(CurrentPlayer)=0 Then Ship2Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship3NotAvailable 2in":Exit Sub

		Case 3:		debug.print "Ship4RandomSelection"
					If Ship4Sunk(CurrentPlayer)=0 Then Ship4Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship4Available":Exit Sub
					If Ship5Sunk(CurrentPlayer)=0 Then Ship5Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship4NotAvailable 5in":Exit Sub
					If Ship6Sunk(CurrentPlayer)=0 Then Ship6Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship4NotAvailable 6in":Exit Sub
					If Ship7Sunk(CurrentPlayer)=0 Then Ship7Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship4NotAvailable 7in":Exit Sub
					If Ship8Sunk(CurrentPlayer)=0 Then Ship8Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship4NotAvailable 8in":Exit Sub
 					If Ship1Sunk(CurrentPlayer)=0 Then Ship1Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship4NotAvailable 1in":Exit Sub
					If Ship2Sunk(CurrentPlayer)=0 Then Ship2Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship4NotAvailable 2in":Exit Sub
					If Ship3Sunk(CurrentPlayer)=0 Then Ship3Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship4NotAvailable 3in":Exit Sub

		Case 4:		debug.print "Ship5RandomSelection"
					If Ship5Sunk(CurrentPlayer)=0 Then Ship5Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship5Available":Exit Sub
					If Ship6Sunk(CurrentPlayer)=0 Then Ship6Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship5NotAvailable 6in":Exit Sub
					If Ship7Sunk(CurrentPlayer)=0 Then Ship7Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship5NotAvailable 7in":Exit Sub
					If Ship8Sunk(CurrentPlayer)=0 Then Ship8Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship5NotAvailable 8in":Exit Sub
					If Ship1Sunk(CurrentPlayer)=0 Then Ship1Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship5NotAvailable 1in":Exit Sub
 					If Ship2Sunk(CurrentPlayer)=0 Then Ship2Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship5NotAvailable 2in":Exit Sub
					If Ship3Sunk(CurrentPlayer)=0 Then Ship3Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship5NotAvailable 3in":Exit Sub
					If Ship4Sunk(CurrentPlayer)=0 Then Ship4Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship5NotAvailable 4in":Exit Sub

		Case 5: 	debug.print "Ship6RandomSelection"
					If Ship6Sunk(CurrentPlayer)=0 Then Ship6Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship6Available ":Exit Sub
					If Ship7Sunk(CurrentPlayer)=0 Then Ship7Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship6NotAvailable 7in":Exit Sub
					If Ship8Sunk(CurrentPlayer)=0 Then Ship8Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship6NotAvailable 8in":Exit Sub
					If Ship1Sunk(CurrentPlayer)=0 Then Ship1Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship6NotAvailable 1in":Exit Sub
					If Ship2Sunk(CurrentPlayer)=0 Then Ship2Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship6NotAvailable 2in":Exit Sub
 					If Ship3Sunk(CurrentPlayer)=0 Then Ship3Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship6NotAvailable 3in":Exit Sub
					If Ship4Sunk(CurrentPlayer)=0 Then Ship4Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship6NotAvailable 4in":Exit Sub
					If Ship5Sunk(CurrentPlayer)=0 Then Ship5Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship6NotAvailable 5in":Exit Sub

		Case 6:		debug.print "Ship7RandomSelection"
					If Ship7Sunk(CurrentPlayer)=0 Then Ship7Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship7Available":Exit Sub
					If Ship8Sunk(CurrentPlayer)=0 Then Ship8Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship7NotAvailable 8in":Exit Sub
					If Ship1Sunk(CurrentPlayer)=0 Then Ship1Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship7NotAvailable 1in":Exit Sub
					If Ship2Sunk(CurrentPlayer)=0 Then Ship2Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship7NotAvailable 2in":Exit Sub
					If Ship3Sunk(CurrentPlayer)=0 Then Ship3Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship7NotAvailable 3in":Exit Sub
 					If Ship4Sunk(CurrentPlayer)=0 Then Ship4Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship7NotAvailable 4in":Exit Sub
					If Ship5Sunk(CurrentPlayer)=0 Then Ship5Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship7NotAvailable 5in":Exit Sub
					If Ship6Sunk(CurrentPlayer)=0 Then Ship6Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship7NotAvailable 6in":Exit Sub

		Case 7:		debug.print "Ship8RandomSelection"
					If Ship8Sunk(CurrentPlayer)=0 Then Ship8Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship8Available":Exit Sub
					If Ship1Sunk(CurrentPlayer)=0 Then Ship1Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship8NotAvailable 1in":Exit Sub
					If Ship2Sunk(CurrentPlayer)=0 Then Ship2Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship8NotAvailable 2in":Exit Sub
					If Ship3Sunk(CurrentPlayer)=0 Then Ship3Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship8NotAvailable 3in":Exit Sub
					If Ship4Sunk(CurrentPlayer)=0 Then Ship4Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship8NotAvailable 4in":Exit Sub
 					If Ship5Sunk(CurrentPlayer)=0 Then Ship5Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship8NotAvailable 5in":Exit Sub
					If Ship6Sunk(CurrentPlayer)=0 Then Ship6Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship8NotAvailable 6in":Exit Sub
					If Ship7Sunk(CurrentPlayer)=0 Then Ship7Now(CurrentPlayer)=1:ShipNowSet:debug.print "Ship8NotAvailable 7in":Exit Sub

	End Select

End Sub


Sub ResetShipLightsStartOfGame
	l23A.State=0:l23B.State=0:l23C.State=0:l23D.State=0:l23E.State=0:l23F.State=0:l23G.State=0:l23H.State=0
End Sub

Sub ResetShipSunkFlags
If ScurvyDickActive(CurrentPlayer)=1 Then ShipSunkCount(CurrentPlayer)=0:ScurvyDickActive(CurrentPlayer)=0:debug.print "ShipSunkCount set to -1 to zero 2nd round of ships"
debug.print "ResetShipSunkFlags"
	Ship1Sunk(CurrentPlayer)=0:Ship2Sunk(CurrentPlayer)=0:Ship3Sunk(CurrentPlayer)=0:Ship4Sunk(CurrentPlayer)=0:Ship5Sunk(CurrentPlayer)=0
	Ship6Sunk(CurrentPlayer)=0:Ship7Sunk(CurrentPlayer)=0:Ship8Sunk(CurrentPlayer)=0:Ship9Sunk(CurrentPlayer)=0:ScurvyDickActive(CurrentPlayer)=0
	ShipsSunk(currentPlayer)=0
	ShipCount(CurrentPlayer)=0
End Sub

Sub TurnOffShipSinkFlasher
		OverlayShipSink.Visible=0
End Sub


'Sub StartScurvyDick
'		ScurvyDickActive(CurrentPlayer)=1
'		vpmtimer.addtimer 1500, "TurnOffShipSinkFlasher'"
'debug.print "ShipSunkCount=0"
'		StopShipFires
'		EliminatedFlasher.Visible=False
'		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_LadyCutlas2Wet": End If
'		lv1.State=2:lv2.state=0:lv3.state=0:lv4.State=0:l23H.State=1
' 		Ship1.Visible=1:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
'		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
'		Ship9Now=False
'		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=True:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
'		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
'		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
'		ApronOverlayStarting.Visible=False
'debug.print "ShipSunk=9"
'		ResetShipLights
'		vpmtimer.addtimer 5200, "StartBarbaBlanca'"
'		AddScore 10000000
'	End If

'End Sub


Sub StartScurvyDick
		ShipsComplete(CurrentPlayer)=0	
		ScurvyDickActive(CurrentPlayer)=1
	PlaySong "m_BlackSkullsHighQual-7dB"
		vpmtimer.addtimer 1500, "TurnOffShipSinkFlasher'"
		StopShipFires
		EliminatedFlasher.Visible=False

		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_ScurvyDickWet2": End If
		lv1.State=2:lv2.state=0:lv3.state=0:lv4.State=0
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=1:ShipBarbaBlanca.Visible=0
		ShipSink(CurrentPlayer)=False
		ApronOverlayScurvyDick.Visible=True:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False	

		vpmtimer.addtimer 5200, "StartBarbaBlanca'"
		AddScore 20000000
End Sub

 
Sub ShipNowSet

	If Ship8Now(CurrentPlayer)=1 Then
		vpmtimer.addtimer 1500, "TurnOffShipSinkFlasher'"
		StopShipFires
		EliminatedFlasher.Visible=False

		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_MadCatMickWet2": End If
		lv1.State=2:lv2.State=0:lv3.State=0:lv4.State=0:l23H.State=2
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=1
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
	
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=True:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False
		ShipSink(CurrentPlayer)=False
		vpmtimer.addtimer 5200, "StartBarbaBlanca'"
		AddScore 5000000
	End If

	If Ship7Now(CurrentPlayer)=1 Then
debug.print "JoeSop-Ship7NowShow"
		vpmtimer.addtimer 1500, "TurnOffShipSinkFlasher'"
		StopShipFires
		EliminatedFlasher.Visible=False

		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_JoeSoapWet2": End If
		lv1.State=1:lv2.state=1:lv3.state=2:lv4.State=0:l23G.State=2
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=1:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
	
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=True
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False
		ShipSink(CurrentPlayer)=False
		vpmtimer.addtimer 5200, "StartBarbaBlanca'"
		AddScore 4000000
	End If

	If Ship6Now(CurrentPlayer)=1 Then
debug.print "Smuggy-Ship6NowShow"
		vpmtimer.addtimer 1500, "TurnOffShipSinkFlasher'"
		StopShipFires
		EliminatedFlasher.Visible=False

		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_SmuggySmaugWet2": End If
		lv1.State=1:lv2.state=2:lv3.State=0:lv4.State=0:l23F.State=2:l23F.State=2
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=1:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
	
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=True:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False
		ShipSink(CurrentPlayer)=False
		vpmtimer.addtimer 5200, "StartBarbaBlanca'"
		AddScore 3500000
	End If

	If Ship5Now(CurrentPlayer)=1 Then
debug.print "Franky-Ship5NowShow"
		vpmtimer.addtimer 1500, "TurnOffShipSinkFlasher'"
		StopShipFires
		EliminatedFlasher.Visible=False

		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_PiddlerWet2": End If
		lv1.State=1:lv2.state=1:lv3.State=2:lv4.State=0:l23E.State=2
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=1:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
	
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=True:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False
		ShipSink(CurrentPlayer)=False
		vpmtimer.addtimer 5200, "StartBarbaBlanca'"
		AddScore 60000000
	End If


	If Ship4Now(CurrentPlayer)=1 Then
		vpmtimer.addtimer 1500, "TurnOffShipSinkFlasher'"
		StopShipFires
		EliminatedFlasher.Visible=False

		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_TheBruceWet2": End If
		lv1.State=2:lv2.State=0:lv3.State=0:lv4.State=0:l23D.State=2
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=1:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
	
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=True:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False
		ShipSink(CurrentPlayer)=False
		vpmtimer.addtimer 5200, "StartBarbaBlanca'"
		AddScore 4500000
	End If

	If Ship3Now(CurrentPlayer)=1 Then
		vpmtimer.addtimer 1500, "TurnOffShipSinkFlasher'"
		StopShipFires
		EliminatedFlasher.Visible=False

		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_HolyCrackWet2": End If
		lv1.State=1:lv2.state=2:lv3.state=0:lv4.State=0:l23C.State=2
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=1:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0

		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=True:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False	
		ShipSink(CurrentPlayer)=False
		vpmtimer.addtimer 5200, "StartBarbaBlanca'"
		AddScore 4500000
	End If

	If Ship2Now(CurrentPlayer)=1 Then
		StopShipFires
		vpmtimer.addtimer 1500, "TurnOffShipSinkFlasher'"
		EliminatedFlasher.Visible=False

		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_SaintaAnaWet2": End If
		l23B.State=2
		lv1.State=1:lv2.State=2:lv3.State=0:lv4.State=0
		Ship1.Visible=0:Ship2.Visible=1:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0

		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=True
		ApronOverlayStarting.Visible=False

		ShipSink(CurrentPlayer)=False
		vpmtimer.addtimer 5200, "StartBarbaBlanca'"
		AddScore 6000000
	End If

	If Ship1Now(CurrentPlayer)=1 Then 
		vpmtimer.addtimer 1500, "TurnOffShipSinkFlasher'"
		StopShipFires
		EliminatedFlasher.Visible=False
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_LadyCutlas2Wet": End If
		lv1.State=2:lv2.State=0:lv3.State=0:lv4.State=0
		l23A.State=2
		Ship1.Visible=1:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=True:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False

		ShipSink(CurrentPlayer)=False
		If bOnTheFirstBall=False Then AddScore 6000000
	End If
End Sub

Sub ShipSinkTableFlasherStop
	OverlayShipSink.Visible =0	
End Sub

Sub MakeShipsInvisible
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0: ShipScurvyWave.Visible=0
End Sub
'*********************************************************
'ResetShip and lights Start of a new ball
'**********************************************************
Sub ShipNowSetForNewBallReset
'	If ShipSunkCount(CurrentPlayer)=1 Then 
'		StopShipFires
'		EliminatedFlasher.Visible=False
'		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_LadyCutlas2Wet": End If
 '		Ship1.Visible=1:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
'		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
'		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=True:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
'		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
'		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
'		ApronOverlayStarting.Visible=False
'		debug.print "Ship1 Show"
'		If lv2.State=2 Then Fire1: End If
'		If lv3.State=2 Then Fire1:Fire2: End If
'		If lv4.State=2 Then Fire1:Fire2:Fire3: End If
'		If lv4.State=1 Then lv1.State=2:lv2.State=0:lv3.State=0:lv4.State=0:End If
'	End If

	If ScurvyDickActive(CurrentPlayer)=1 Then
		StopShipFires
		EliminatedFlasher.Visible=False
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_ScurvyDickWet2": End If
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=1:ShipBarbaBlanca.Visible=0

		ApronOverlayScurvyDick.Visible=True:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False	
		debug.print "Ship9Show"
		If lv2.State=2 Then Fire1: End If
		If lv3.State=2 Then Fire1:Fire2: End If
		If lv4.State=2 Then Fire1:Fire2:Fire3: End If
		If lv4.State=1 Then lv1.State=2:lv2.State=0:lv3.State=0:lv4.State=0:End If
	End If

	If Ship8Now(CurrentPlayer)=1 Then
		StopShipFires
		EliminatedFlasher.Visible=False
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_MadCatMickWet2": End If
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=1
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
		l23H.State=2
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=True:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False
		debug.print "Ship8 Show"
		If lv2.State=2 Then Fire1: End If
		If lv3.State=2 Then Fire1:Fire2: End If
		If lv4.State=2 Then Fire1:Fire2:Fire3: End If
		If lv4.State=1 Then lv1.State=2:lv2.State=0:lv3.State=0:lv4.State=0:End If
	End If

	If Ship7Now(CurrentPlayer)=1 Then
		StopShipFires
		EliminatedFlasher.Visible=False
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_JoeSoapWet2": End If
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=1:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=True
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False
		debug.print "Ship7Show"
		l23G.State=2
'		If lv2.State=2 Then Fire1: End If
'		If lv3.State=2 Then Fire1:Fire2: End If
		If lv4.State=2 Then Fire1:Fire2:Fire3: End If
		If lv4.State=1 Then lv1.State=1:lv2.State=1:lv3.State=2:lv4.State=0:End If
	End If

	If Ship6Now(CurrentPlayer)=1 Then 
		StopShipFires
		EliminatedFlasher.Visible=False
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_SmuggySmaugWet2": End If
		lv1.State=1:lv2.state=2:lv3.State=0:lv4.State=0:l23E.State=1:l23F.State=2
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=1:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
		l23F.State=2	
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=True:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False
		debug.print "Ship6Show"
'		If lv2.State=2 Then Fire1: End If
		If lv3.State=2 Then Fire1:Fire2: End If
		If lv4.State=2 Then Fire1:Fire2:Fire3: End If
		If lv4.State=1 Then lv1.State=1:lv2.State=2:lv3.State=0:lv4.State=0:End If
	End If

	If Ship5Now(CurrentPlayer)=1 Then
		StopShipFires
		EliminatedFlasher.Visible=False
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_PiddlerWet2": End If
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=1:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
		l23E.State=2	
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=True:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False
		debug.print "Ship5Show"
'		If lv2.State=2 Then Fire1: End If
'		If lv3.State=2 Then Fire1:Fire2: End If
		If lv4.State=2 Then Fire1:Fire2:Fire3: End If
		If lv4.State=1 Then lv1.State=1:lv2.State=1:lv3.State=2:lv4.State=0:End If
	End If


	If Ship4Now(CurrentPlayer)=1 Then
		StopShipFires
		EliminatedFlasher.Visible=False
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_TheBruceWet2": End If
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=1:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
		l23D.State=2	
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=True:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False
		debug.print "Ship4Show"
		If lv2.State=2 Then Fire1: End If
		If lv3.State=2 Then Fire1:Fire2: End If
		If lv4.State=2 Then Fire1:Fire2:Fire3: End If
		If lv4.State=1 Then lv1.State=2:lv2.State=0:lv3.State=0:lv4.State=0:End If
	End If

	If Ship3Now(CurrentPlayer)=1 Then
		StopShipFires
		EliminatedFlasher.Visible=False
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_HolyCrackWet2": End If
		Ship1.Visible=0:Ship2.Visible=0:Ship3.Visible=1:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
		l23C.State=2
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=True:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False	
		debug.print "Ship3Show"
'		If lv2.State=2 Then Fire1: End If
		If lv3.State=2 Then Fire1:Fire2: End If
		If lv4.State=2 Then Fire1:Fire2:Fire3: End If
		If lv4.State=1 Then lv1.State=1:lv2.State=2:lv3.State=0:lv4.State=0:End If
	End If

	If Ship2Now(CurrentPlayer)=1 Then
		StopShipFires
		EliminatedFlasher.Visible=False
		If CalloutActive=False Then CalloutActive=True:CalloutTimer.Enabled=True:PlaySound "CO_SaintaAnaWet2": End If
		Ship1.Visible=0:Ship2.Visible=1:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
		l23B.State=2
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=False:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=True
		ApronOverlayStarting.Visible=False
		debug.print "Ship2Show"
'		If lv2.State=2 Then Fire1: End If
		If lv3.State=2 Then Fire1:Fire2: End If
		If lv4.State=2 Then Fire1:Fire2:Fire3: End If
		If lv4.State=1 Then lv1.State=2:lv2.State=0:lv3.State=0:lv4.State=0:End If
	End If

	If Ship1Now(CurrentPlayer)=1 Then
		StopShipFires
		debug.print "ShipSunkCounter=1"
'		PlaySound "CO_LadyCutlas2Wet"
		l23A.State=2
		Ship1.Visible=1:Ship2.Visible=0:Ship3.Visible=0:Ship4.Visible=0:Ship5.Visible=0:Ship6.Visible=0:Ship7.Visible=0:Ship8.Visible=0
		ShipPickles.Visible=0:ShipScurvyWave.Visible=0:ShipBarbaBlanca.Visible=0
		ApronOverlayScurvyDick.Visible=False:ApronOverlayMolly.Visible=True:ApronOverlaySmaug.Visible=False:ApronOverlayBarbaBlanca.Visible=False
		ApronOverlayPrincessAndrea.Visible=False:ApronOverlayLadyTeaghan.Visible=False:ApronOverlayMadCatMick.Visible=False:ApronOverlayJoeSoap.Visible=False
		ApronOverlayFrankie.Visible=False:ApronOverlayTheBruce.Visible=False:ApronOverlayCrackers.Visible=False:ApronOverlayPrinceAndres.Visible=False
		ApronOverlayStarting.Visible=False
		debug.print "Ship1FunnyShow"
	End If
End Sub

'************************************************************
'WheelLights
'************************************************************
Sub SinkShip
	If l23H.State=2 Then:l23H.State=1:Exit Sub
	If l23G.State=2 Then:l23G.State=1:Exit Sub
	If l23F.State=2 Then:l23F.State=1:Exit Sub
	If l23E.State=2 Then:l23E.State=1:Exit Sub
	If l23D.State=2 Then:l23D.State=1:Exit Sub
	If l23C.State=2 Then:l23C.State=1:Exit Sub
	If l23B.State=2 Then:l23B.State=1:Exit Sub
	If l23A.State=2 Then:l23A.State=1
End Sub

Sub ResetShipLights
	l23A.State=0:l23B.State=0:l23C.State=0:l23D.State=0:l23E.State=0:l23F.State=0:l23G.State=0:l23H.State=0:lv1.State=2:lv2.State=0:lv3.State=0:lv4.State=0
End Sub

'************************************************************
'ShipDamage and AwardSelect
'************************************************************

Sub CheckStatusOfDamage
	If ScurvyDickActive(CurrentPlayer)=1 Then: ScurvyDickDamage:Exit Sub
	If Ship8Now(CurrentPlayer)=1 Then Ship8Damage
	If Ship7Now(CurrentPlayer)=1 Then Ship7Damage
	If Ship6Now(CurrentPlayer)=1 Then Ship6Damage
	If Ship5Now(CurrentPlayer)=1 Then Ship5Damage
	If Ship4Now(CurrentPlayer)=1 Then Ship4Damage
	If Ship3Now(CurrentPlayer)=1 Then Ship3Damage
	If Ship2Now(CurrentPlayer)=1 Then Ship2Damage
	If Ship1Now(CurrentPlayer)=1 Then Ship1Damage
End Sub

Sub Ship1Damage
	If lv4.State=1 Then:Canon4: CalloutTimer.Enabled=True:CalloutActive=True:Ship1Now(CurrentPlayer)=0:ShipSink(CurrentPlayer)=True:vpmtimer.addtimer 7000,"CheckShipsSunk'": SinkShip:StartSinkShipSequence:Ship1Award:Exit Sub
	If lv4.State=2 Then:Canon4:ShipSink(CurrentPlayer)=False:Exit Sub
	If lv3.State=2 Then:Canon3:ShipSink(CurrentPlayer)=False:Exit Sub
	If lv2.State=2 Then:Canon2:ShipSink(CurrentPlayer)=False:Exit Sub
	If lv1.State=2 Then:Canon1:ShipSink(CurrentPlayer)=False:Exit Sub
End Sub

Sub SinkShipFlasherTimerDelayed
		SinkShipFlasherTimer.Enabled=1
End Sub

Sub Ship1Award
		Ship1Sunk(CurrentPlayer)=1: debug.print "Ship1Sunk(CurrentPlayer)"
		vpmtimer.AddTimer 8200, "SinkShipFlasherTimerDelayed'"
		StartShipFires
		l23A.State=1
		EliminatedFlasher.Visible=True
		PlaySound "CO_LadyCutlas2Finish"
		DMD CL(0, " BOUNTY COLLECTED" ), CL(1, " 6000000"), "", eNone, eBlink, eNone, 3000, True, ""
		vpmtimer.addtimer 3200, "RevengeDMD'"
		vpmtimer.addtimer 10000, "NextShip'"		
End Sub

Sub Ship2Damage
	If lv4.State=1 Then: Canon4: CalloutTimer.Enabled=True:CalloutActive=True:Ship2Now(CurrentPlayer)=0:vpmtimer.addtimer 7000,"CheckShipsSunk'":SinkShip: StartSinkShipSequence:Ship2Award:Exit Sub
	If lv4.State=2 Then:Canon4:Exit Sub
	If lv3.State=2 Then:Canon3:Exit Sub
	If lv2.State=2 Then:Canon2:Exit Sub
	If lv1.State=2 Then:Canon1
End Sub

Sub Ship2Award
		l23B.State=1
		Ship2Sunk(CurrentPlayer)=1: debug.print "Ship2Sunk(CurrentPlayer)"
		StartShipFires
		vpmtimer.AddTimer 8200, "SinkShipFlasherTimerDelayed'"
		EliminatedFlasher.Visible=True
		PlaySound "CO_PrinceAndre2Finish"
		DMD CL(0, " BOUNTY COLLECTED" ), CL(1, " 4500000"), "", eNone, eBlink, eNone, 3000, True, ""
		vpmtimer.addtimer 3200, "RevengeDMD'" 
		vpmtimer.addtimer 10000, "NextShip'"		
End Sub


Sub Ship3Damage
	If lv4.State=1 Then:Canon4: CalloutTimer.Enabled=True:CalloutActive=True:Ship3Now(CurrentPlayer)=0:vpmtimer.addtimer 7000,"CheckShipsSunk'":SinkShip:StartSinkShipSequence:Ship3Award:Exit Sub
	If lv4.State=2 Then:Canon4:Exit Sub
	If lv3.State=2 Then:Canon3:Exit Sub
	If lv2.State=2 Then:Canon2:Exit Sub
	If lv1.State=2 Then:Canon1
End Sub

Sub Ship3Award
		l23C.State=1
		Ship3Sunk(CurrentPlayer)=1: debug.print "Ship3Sunk(CurrentPlayer)"
		StartShipFires
		vpmtimer.AddTimer 8200, "SinkShipFlasherTimerDelayed'"
		EliminatedFlasher.Visible=True
		PlaySound "CO_Crackers2Finish"
		DMD CL(0, " BOUNTY COLLECTED" ), CL(1, " 4500000"), "", eNone, eBlink, eNone, 3000, True, ""
		vpmtimer.addtimer 3200, "RevengeDMD'" 	

		vpmtimer.addtimer 10000, "NextShip'"	
End Sub

Sub Ship4Damage
	If lv4.State=1 Then: Canon4:CalloutTimer.Enabled=True:CalloutActive=True:Ship4Now(CurrentPlayer)=0:vpmtimer.addtimer 8500,"CheckShipsSunk'":SinkShip:StartSinkShipSequence:Ship4Award:Exit Sub
	If lv4.State=2 Then:Canon4:PlaySound "CO_TheBruceLadysAwaitMe":Exit Sub
	If lv3.State=2 Then:Canon3:Exit Sub
	If lv2.State=2 Then:Canon2:PlaySound "CO_TheBruceGettingPersonalM":Exit Sub
	If lv1.State=2 Then:Canon1
End Sub

Sub Ship4Award
		l23D.State=1
		Ship4Sunk(CurrentPlayer)=1: debug.print "Ship4Sunk(CurrentPlayer)"
		StartShipFires
		vpmtimer.AddTimer 8200, "SinkShipFlasherTimerDelayed'"
		EliminatedFlasher.Visible=True
		PlaySound "CO_TheBruce2Finish"
		DMD CL(0, " BOUNTY COLLECTED" ), CL(1, " 6000000"), "", eNone, eBlink, eNone, 3000, True, ""	
		vpmtimer.addtimer 3200, "RevengeDMD'" 
		vpmtimer.addtimer 10000, "NextShip'"	
End Sub

Sub Ship5Damage
	If lv4.State=1 Then:Canon4: CalloutTimer.Enabled=True:CalloutActive=True:Ship5Now(CurrentPlayer)=0:vpmtimer.addtimer 8500,"CheckShipsSunk'":SinkShip:StartSinkShipSequence:Ship5Award:Exit Sub
	If lv4.State=2 Then:Canon4:Exit Sub
	If lv3.State=2 Then:Canon3:Exit Sub
	If lv2.State=2 Then:Canon2:Exit Sub
	If lv1.State=2 Then:Canon1
End Sub

Sub Ship5Award
		l23E.State=1
		Ship5Sunk(CurrentPlayer)=1: debug.print "Ship5Sunk(CurrentPlayer)"
		StartShipFires
		vpmtimer.AddTimer 8200, "SinkShipFlasherTimerDelayed'"
		PlaySound "CO_Frankie2Finish"
 		EliminatedFlasher.Visible=True
		DMD CL(0, " BOUNTY COLLECTED" ), CL(1, " 3500000"), "", eNone, eBlink, eNone, 3000, True, ""
		vpmtimer.addtimer 3200, "RevengeDMD'" 
		vpmtimer.addtimer 10000, "NextShip'"	
End Sub

Sub Ship6Damage
	If lv4.State=1 Then:Canon4: CalloutTimer.Enabled=True:CalloutActive=True:Ship6Now(CurrentPlayer)=0:vpmtimer.addtimer 8000,"CheckShipsSunk'":SinkShip:StartSinkShipSequence:Ship6Award:Exit Sub
	If lv4.State=2 Then:Canon4:PlaySound "CO_SmaugTavernWet":Exit Sub
	If lv3.State=2 Then:Canon3:Exit Sub
	If lv2.State=2 Then:Canon2:Exit Sub
	If lv1.State=2 Then:Canon1
End Sub

Sub Ship6Award
		l23F.State=1
		Ship6Sunk(CurrentPlayer)=1: debug.print "Ship6Sunk(CurrentPlayer)"
		StartShipFires
		vpmtimer.AddTimer 8200, "SinkShipFlasherTimerDelayed'"
		EliminatedFlasher.Visible=True
		PlaySound "CO_Smuggy2Finish"
		DMD CL(0, " BOUNTY COLLECTED" ), CL(1, " 4000000"), "", eNone, eBlink, eNone, 3000, True, ""
		vpmtimer.addtimer 3200, "RevengeDMD'" 	
		vpmtimer.addtimer 10000, "NextShip'"	
End Sub

Sub Ship7Damage
	If lv4.State=1 Then:Canon4: CalloutTimer.Enabled=True:CalloutActive=True:Ship7Now(CurrentPlayer)=0:vpmtimer.addtimer 10000,"CheckShipsSunk'":SinkShip:StartSinkShipSequence:Ship7Award:Exit Sub
	If lv4.State=2 Then:Canon4:Exit Sub
	If lv3.State=2 Then:Canon3:Exit Sub
	If lv2.State=2 Then:Canon2:Exit Sub
	If lv1.State=2 Then:Canon1
End Sub

Sub Ship7Award
		l23G.State=1
		Ship7Sunk(CurrentPlayer)=1: debug.print "Ship7Sunk(CurrentPlayer)"
		StartShipFires
		vpmtimer.AddTimer 8200, "SinkShipFlasherTimerDelayed'"
		EliminatedFlasher.Visible=True
		PlaySound "CO_JoeSoapFarewellWet"
		DMD CL(0, " BOUNTY COLLECTED" ), CL(1, " 5000000"), "", eNone, eBlink, eNone, 3000, True, ""	
		vpmtimer.addtimer 3200, "RevengeDMD'" 
		vpmtimer.addtimer 10000, "NextShip'"	
End Sub

Sub Ship8Damage
	If lv4.State=1 Then:Canon4: CalloutTimer.Enabled=True:CalloutActive=True:Ship8Now(CurrentPlayer)=0:vpmtimer.addtimer 10000,"CheckShipsSunk'":SinkShip:StartSinkShipSequence:Ship8Award:Exit Sub
	If lv4.State=2 Then:Canon4:Exit Sub
	If lv3.State=2 Then:Canon3:Exit Sub
	If lv2.State=2 Then:Canon2:Exit Sub
	If lv1.State=2 Then:Canon1
End Sub


Sub Ship8Award
		l23H.State=1
		Ship8Sunk(CurrentPlayer)=1: debug.print "Ship8Sunk(CurrentPlayer)"
		StartShipFires
		vpmtimer.AddTimer 8200, "SinkShipFlasherTimerDelayed'"
		EliminatedFlasher.Visible=True
		PlaySound "CO_MadCat2Finish"
		DMD CL(0, " BOUNTY COLLECTED" ), CL(1, " 6000000"), "", eNone, eBlink, eNone, 3000, True, ""	
		vpmtimer.addtimer 3200, "RevengeDMD'" 
		vpmtimer.addtimer 10000, "NextShip'"	
End Sub

Sub ScurvyDickDamage
	If lv4.State=1 Then:Canon4: CalloutTimer.Enabled=True:CalloutActive=True:ScurvyDickActive(CurrentPlayer)=0:PlaySound "CO_NastyBattleWet2":SinkShip:StartSinkShipSequence:Ship9Award:Exit Sub
	If lv4.State=2 Then:Canon4:Exit Sub
	If lv3.State=2 Then:Canon3:Exit Sub
	If lv2.State=2 Then:Canon2:Exit Sub
	If lv1.State=2 Then:Canon1
End Sub

Sub Ship9Award
		ResetShipSunkFlags
 debug.print "ScurvyDickComplete"
		ShipsComplete(CurrentPlayer)=1
		l23A.State=0:l23B.State=0:l23C.State=0:l23D.State=0:l23E.State=0:l23F.State=0:l23G.State=0:l23H.State=0
		StartShipFires
		vpmtimer.AddTimer 8200, "SinkShipFlasherTimerDelayed'"
		EliminatedFlasher.Visible=True
		TotemLightTimer.Enabled=True
		DMD CL(0, " BOUNTY COLLECTED" ), CL(1, " 10000000"), "", eNone, eBlink, eNone, 3000, True, ""	
		vpmtimer.addtimer 3200, "RevengeDMD'" 
		vpmtimer.addtimer 15000, "ShipsCompleteSuperBonus'"


		vpmtimer.addtimer 10000, "RandomShipSelect'"	
End Sub

Sub ShipsCompleteSuperBonus
		PlaySound "CO_SuperJackPot3Ozone"
		PlaySoundAt "fx_knocker",KickerGoldenReef
		DOF 122, DOFPulse
		DMD CL(0, "	ITS A MOTHERLOAD" ), CL(1, "MODES COMPLETE"), "", eNone, eBlink, eNone, 3000, True, ""
		If Hard=0 Then 
		DMD CL(0, "	TONIGHT WE DRINK" ), CL(1, "100000000"), "", eNone, eBlink, eNone, 5000, True, ""
			AddScore 100000000
		End If
		If Hard=1 Then 
		DMD CL(0, "	TONIGHT WE DRINK" ), CL(1, "50000000"), "", eNone, eBlink, eNone, 5000, True, ""
			AddScore 50000000
		End If
End Sub

Sub Canon1
	PlaySound "Canon_MastHit"
End Sub

Sub Canon2
	PlaySound "Canon_MastHit"
End Sub

Sub Canon3
	PlaySound "Canon&Hit"
End Sub

Sub Canon4
	PlaySound "Canon&Hit"
End Sub

Sub Canon5
	PlaySound "Canon_MastHit"
End Sub

Sub ResetWheelLights
	l23A.State=0:l23B.State=0:l23C.State=0:l23D.State=0:l23E.State=0:l23F.State=0:l23G.State=0:l23H.State=0
End Sub

Sub ResetVariLights
	lv1.State=2:lv2.state=0:lv3.state=0:lv4.State=0:lv5.State=0
End Sub

Sub RevengeDMD 
'	DMD "", "", "DMD_Revenge1", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge2", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge3", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge4", eNone, eBlink, eNone, 150, False, ""
	DMD "", "", "DMD_Revenge5", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge6", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge7", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge8", eNone, eBlink, eNone, 150, False, ""
	DMD "", "", "DMD_Revenge9", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge10", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge11", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge12", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge13", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge14", eNone, eBlink, eNone, 150, False, ""
	DMD "", "", "DMD_Revenge15", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge16", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge17", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge18", eNone, eBlink, eNone, 150, False, ""
	DMD "", "", "DMD_Revenge19", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge20", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge21", eNone, eBlink, eNone, 150, False, ""
	DMD "", "", "DMD_Revenge22", eNone, eBlink, eNone, 150, False, ""
	DMD "", "", "DMD_Revenge23", eNone, eBlink, eNone, 150, False, "" 
	DMD "", "", "DMD_Revenge24", eNone, eBlink, eNone, 150, False, ""
	DMD "", "", "DMD_Revenge25", eNone, eBlink, eNone, 2500, False, "" 
	
	vpmtimer.addtimer 6000, "DMDScoreNow'" 
End Sub






'******************************************************************************


'LightsOff
	pPlankReadyOff.BlendDisableLighting =1
	pPlankFishOff.BlendDisableLighting =1
	pPlankMermaidOff.BlendDisableLighting =1

	p23aoff.blenddisablelighting = 4  ' Wheel
	p23boff.blenddisablelighting = 4  ' Wheel
	p23coff.blenddisablelighting = 4  ' Wheel
	p23doff.blenddisablelighting = 4  ' Wheel
	p23eoff.blenddisablelighting = 4  ' Wheel
	p23foff.blenddisablelighting = 4  ' Wheel
	p23goff.blenddisablelighting = 4  ' Wheel
	p23hoff.blenddisablelighting = 4  ' Wheel

	p1off.blenddisablelighting = 4  ' LeftTarget
	p2off.blenddisablelighting = 4  ' LeftTarget
	p3off.blenddisablelighting = 4  ' LeftTarget
	pLoadCanon.blenddisablelighting = 4  ' Canon
	pLoadCanon1off.blenddisablelighting = 2  ' Canon
	pLoadCanon2off.blenddisablelighting = 2  ' Canon
	pLoadCanon3off.blenddisablelighting = 2  ' Canon

	pGulloff.blenddisablelighting = 4  'Gull
	pGull2off.blenddisablelighting = 4  'Gull2
	pGull3off.blenddisablelighting = 4  'Gull3
	pChickenoff.blenddisablelighting = 4  'Chicken
	pChicken2off.blenddisablelighting = 4  'Chicken
	pChicken3off.blenddisablelighting = 4  'Chicken
	pKickerTargetoff.blenddisablelighting = 4  'GoldenReefKickerLight
	pKickerGRoff.blenddisablelighting = 4 

	pGRoff.blenddisablelighting = 4  ' GoldenReef
	pWTP2off.blenddisablelighting = 4  ' WalkThePlank
	pDC1off.blenddisablelighting = 4  ' DirtyCreature1
	pDC2off.blenddisablelighting = 4  ' DirtyCreature1
	pDCoff.blenddisablelighting = 4  ' DirtyCreature1
	pDC4off.blenddisablelighting = 4  ' DirtyCreature1
'	pDC5off.blenddisablelighting = 4  ' DirtyCreature1

	pv1off.blenddisablelighting = 4  ' VariTarget
	pv2off.blenddisablelighting = 4  ' VariTarget
	pv3off.blenddisablelighting = 4  ' VariTarget
	pv4off.blenddisablelighting = 4  ' VariTarget

	p5off.blenddisablelighting = 4  ' SpinDiscTarget
	p6off.blenddisablelighting = 4  ' SpinDiscTarget
	p7off.blenddisablelighting = 4  ' SpinDiscTarget
	p8off.blenddisablelighting = 4  ' SpinDiscTarget
	p9off.blenddisablelighting = 4  ' SpinDiscTarget
	p10off.blenddisablelighting = 4  'SpinDiscTarget

	pTFoff.blenddisablelighting = 4  ' SpinDiscTarget
	pTIoff.blenddisablelighting = 4  ' SpinDiscTarget
	pTS1off.blenddisablelighting = 4  ' SpinDiscTarget
	pTHoff.blenddisablelighting = 4  ' SpinDiscTarget
	pTEoff.blenddisablelighting = 4  ' SpinDiscTarget
	pTS2off.blenddisablelighting = 4  'SpinDiscTarget

	p2xoff.blenddisablelighting = 2  ' BonusLight
	p3xoff.blenddisablelighting = 2  ' BonusLight
	p4xoff.blenddisablelighting = 2  ' BonusLight
	p5xoff.blenddisablelighting = 2  'BonusLight
	p10xoff.blenddisablelighting = 2  ' BonusLight
	p20xoff.blenddisablelighting = 2  'BonusLight


	pLight_BallSaveroff.blenddisablelighting = 1
	pLight_easyhardoff.blenddisablelighting = 1
	pLight_easyhardoff.blenddisablelighting = 4  'LightBallSaver
	pEBoff.blenddisablelighting = 4  'ExtraBall

	pPOTP1off.blenddisablelighting = 4  'ParrotsOffThePearch
	pPOTP2off.blenddisablelighting = 4  'ParrotsOffThePearch
	pPOTP3off.blenddisablelighting = 4  'ParrotsOffThePearch
	pPOTP4off.blenddisablelighting = 4  'ParrotsOffThePearch

	PWS4off.blenddisablelighting = 4  ' WildSeasComplete
	PWTP4off.blenddisablelighting = 4  'WalkThePlankComplete
	pRTS4off.blenddisablelighting = 4  ' RaiseTheSailsComplete
'	pDC4off.blenddisablelighting = 4  'DirtyCreatureComplete
	pPOTP4off.blenddisablelighting = 4  ' ParrotOffThePearch
	pTC4off.blenddisablelighting = 4  ' TreasureChest
	pNATT4off.blenddisablelighting = 4  ' NightAtTheTavernComplete
	pSWTM4off.blenddisablelighting = 4  'SwimWiththeMermaidsComplete

	pROLoff.blenddisablelighting = 4  ' RightOutLane
	pRILoff.blenddisablelighting = 4  ' RightInlane
	pLILoff.blenddisablelighting = 4  ' LeftInlane
	pLOLoff.blenddisablelighting = 4  'LeftOutlane

	pTC2off.blenddisablelighting = 2  'TreasureChest
	pTC3off.blenddisablelighting = 2  'TreasureChest
	pTC4off.blenddisablelighting = 2  'TreasureChest
	pTC5off.blenddisablelighting = 2  'TreasureChest

	pLegoff.blenddisablelighting = 4  'leg
	pPatchoff.blenddisablelighting = 4  'patch
	pHookoff.blenddisablelighting = 4  'hook
	pRoundTheHornoff.blenddisablelighting = 4  'hook

	pLightShootAgainoff.blenddisablelighting = 4  'LightShootAgain
	pLock2off.blenddisablelighting = 4  'Lock2
	pLock1off.blenddisablelighting = 4  'Lock1
	pCanonArmoff.blenddisablelighting = 4  'CanonArmKicker
	pDJoff.blenddisablelighting = 4  'DaveyJonesLocker
	pCRoff.blenddisablelighting = 4  'DaveyJonesLocker
	pTreasureoff.blenddisablelighting = 1  'TresaureChest
	pTreasureWalloff.blenddisablelighting = 1  'TresaureWall
	pBonusActiveoff.blenddisablelighting = 1  'TresaureChest
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'End of scrypt
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 

'//////////////////////////////////////////////////////////////////////
'// TIMERS
'//////////////////////////////////////////////////////////////////////


' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate
     ChickenPenny.Z = ChickenF.CurrentAngle
     MaskRaven2.RotZ = TotemF.CurrentAngle

'LightsOn

'	dim L,X
'	L = gi_laneguide1.GetInPlayIntensity
'	For each x in Pegs
'		x.blenddisablelighting = 0.067 * L
'	Next

	pPlankReady.blenddisablelighting = 1.7 * LightPlankReady.GetInPlayIntensity 
	pPlankFish.blenddisablelighting = 1.7 * LightPlankFish.GetInPlayIntensity 
	pPlankMermaid.blenddisablelighting = 1.7 * LightPlankMermaid.GetInPlayIntensity 
	pTikiLeftEye.blenddisablelighting = 1.7 * LightTikiLeftEye.GetInPlayIntensity 
	pTikiRightEye.blenddisablelighting = 1.7 * LightTikiRightEye.GetInPlayIntensity 

	p23a.blenddisablelighting = 10 * l23A.GetInPlayIntensity ' Wheel
	p23b.blenddisablelighting = 10 * l23B.GetInPlayIntensity  ' Wheel
	p23c.blenddisablelighting = 10 * l23C.GetInPlayIntensity  ' Wheel
	p23d.blenddisablelighting = 10 * l23D.GetInPlayIntensity  ' Wheel
	p23e.blenddisablelighting = 10* l23E.GetInPlayIntensity  ' Wheel
	p23f.blenddisablelighting = 10 * l23F.GetInPlayIntensity  ' Wheel
	p23g.blenddisablelighting = 10 * l23G.GetInPlayIntensity  ' Wheel
	p23h.blenddisablelighting = 10 * l23H.GetInPlayIntensity  ' Wheel

	p1.blenddisablelighting = 7 * l1.GetInPlayIntensity  ' LeftTarget
	p2.blenddisablelighting = 7 * l2.GetInPlayIntensity  ' LeftTarget
	p3.blenddisablelighting = 7 * l3.GetInPlayIntensity  ' LeftTarget
	pLoadCanon.blenddisablelighting = 10 * Light_LoadCanon.GetInPlayIntensity  ' Canon
	pLoadCanon1.blenddisablelighting = 10 * Light_LoadCanon1.GetInPlayIntensity  ' Canon
	pLoadCanon2.blenddisablelighting = 10 * Light_LoadCanon2.GetInPlayIntensity  ' Canon
	pLoadCanon3.blenddisablelighting = 10 * Light_LoadCanon3.GetInPlayIntensity  ' Canon
	pCanonArm.blenddisablelighting = 10 * Light_CanonArm.GetInPlayIntensity 'CanonLoadKicker

	p2x.blenddisablelighting = 15 * l_2x.GetInPlayIntensity ' Bonus
	p3x.blenddisablelighting = 15 * l_3x.GetInPlayIntensity  ' Bonus
	p4x.blenddisablelighting = 15 * l_4x.GetInPlayIntensity  ' Bonus
	p5x.blenddisablelighting = 15 * l_5x.GetInPlayIntensity  ' Bonus
	p10x.blenddisablelighting = 15* l_10x.GetInPlayIntensity  ' Bonus
	p20x.blenddisablelighting = 15 * l_20x.GetInPlayIntensity  ' Bonus

	'pKickerGR.blenddisablelighting = 8 * Light_KickerGR.GetInPlayIntensity  'GoldenReefKickerLight
	pChicken.blenddisablelighting = 8 * Light_Chicken.GetInPlayIntensity  'chickenPot
	pChicken2.blenddisablelighting = 8 * Light_Chicken2.GetInPlayIntensity  'chickenPot
	pChicken3.blenddisablelighting = 8 * Light_Chicken3.GetInPlayIntensity  'chickenPot
	pGull.blenddisablelighting = 8 * Light_Gull.GetInPlayIntensity  'GullPot
	pGull2.blenddisablelighting = 8 * Light_Gull2.GetInPlayIntensity  'GullPot
	pGull3.blenddisablelighting = 8 * Light_Gull3.GetInPlayIntensity  'GullPot
	pKickerTarget.blenddisablelighting = 8 * Light_KickerTarget.GetInPlayIntensity  'GoldenReefKickerLight	
	pGR.blenddisablelighting = 10  * Light_GR.GetInPlayIntensity  ' Goldenreef
	pWTP2.blenddisablelighting = 10  * Light_WTP2.GetInPlayIntensity  ' WalkThePlank

	pDC1.blenddisablelighting = 4  * Light_DirtyCreature1.GetInPlayIntensity  
	pDC2.blenddisablelighting = 4 * Light_DirtyCreature2.GetInPlayIntensity 
	pDC.blenddisablelighting = 4  * Light_DirtyCreature3.GetInPlayIntensity 
	pDC5.blenddisablelighting = 4 * Light_DC5.GetInPlayIntensity 		' DCKickerLight

	pPOTP4.blenddisablelighting = 10 * LightAward1.GetInPlayIntensity 	' AwardModeLights_ParrotOffThePerch
	pTC4.blenddisablelighting = 20 * LightAward2.GetInPlayIntensity  	' AwardModeLights_TreasureChest
	pNATT4.blenddisablelighting = 10 * LightAward6.GetInPlayIntensity 	' AwardModeLights_LastDrinks
	pSWTM4.blenddisablelighting = 10 * LightAward4.GetInPlayIntensity 	' AwardModeSharkAttack
	pWTP4.blenddisablelighting = 10 * LightAward3.GetInPlayIntensity 	' DirtyCreature-LightPosition changed to WalkThePlank
	pWS4.blenddisablelighting = 10 * LightAward5.GetInPlayIntensity 	'AwardWildSeas
	pRTS4.blenddisablelighting = 10 * LightAward7.GetInPlayIntensity 	'AwardModeRaiseTheSails
	pDC4.blenddisablelighting = 8 * LightAward8.GetInPlayIntensity 		' WalkThe Plank (Changed With DirtyCreatureActive)

	pv1.blenddisablelighting = 10 * lv1.GetInPlayIntensity  ' VaryTarget
	pv2.blenddisablelighting = 15 * lv2.GetInPlayIntensity  ' VaryTarget
	pv3.blenddisablelighting = 15* lv3.GetInPlayIntensity  ' VaryTarget
	pv4.blenddisablelighting = 15 * lv4.GetInPlayIntensity  ' VaryTarget


	p5.blenddisablelighting = 10 * l5.GetInPlayIntensity  ' SpinDiscTarget
	p6.blenddisablelighting = 10 * l6.GetInPlayIntensity  ' SpinDiscTarget
	p7.blenddisablelighting = 10 * l7.GetInPlayIntensity  ' SpinDiscTarget
	p8.blenddisablelighting = 10 * l8.GetInPlayIntensity  ' SpinDiscTarget
	p9.blenddisablelighting = 10 * l9.GetInPlayIntensity  ' SpinDiscTarget
	p10.blenddisablelighting = 10 * l10.GetInPlayIntensity  ' SpinDiscTarget

	pTF.blenddisablelighting = 10 * LightTargetF.GetInPlayIntensity  ' SpinDiscTarget
	pTI.blenddisablelighting = 10 * LightTargetI.GetInPlayIntensity  ' SpinDiscTarget
	pTS1.blenddisablelighting = 10 * LightTargetS1.GetInPlayIntensity  ' SpinDiscTarget
	pTH.blenddisablelighting = 10 * LightTargetH.GetInPlayIntensity  ' SpinDiscTarget
	pTE.blenddisablelighting = 10 * LightTargetE.GetInPlayIntensity  ' SpinDiscTarget
	pTS2.blenddisablelighting = 10 * LightTargetS2.GetInPlayIntensity  ' SpinDiscTarget

	pROL.blenddisablelighting = 10 * Lightrightescape.GetInPlayIntensity  ' RightOutlane
	pRIL.blenddisablelighting = 10 * LightRightInlane.GetInPlayIntensity  ' RightInlane
	pLIL.blenddisablelighting = 10 * LightLeftInlane.GetInPlayIntensity  ' LeftInlane
	pLOL.blenddisablelighting = 10 * LightLeftEscape.GetInPlayIntensity  ' LeftOutlane


	pLightShootAgain.blenddisablelighting = 10 * LightShootAgain.GetInPlayIntensity  ' ShootAgain
	pLight_BallSaver.blenddisablelighting = Light_BallSaver.GetInPlayIntensity / 10
	pLight_easyhard.blenddisablelighting = Light_easyhard.GetInPlayIntensity * 7
	pEB.blenddisablelighting = Light_ExtraBall.GetInPlayIntensity * 10
	pLock2.blenddisablelighting = Light_Lock2.GetInPlayIntensity * 10
	pLock1.blenddisablelighting = Light_Lock1.GetInPlayIntensity * 10
	pDJ.blenddisablelighting = Light_DaveyJones.GetInPlayIntensity * 10

	pPOTP1.blenddisablelighting = Light_POTP1.GetInPlayIntensity * 10
	pPOTP2.blenddisablelighting = Light_POTP2.GetInPlayIntensity * 10
	pPOTP3.blenddisablelighting = Light_POTP3.GetInPlayIntensity * 10

	pTC2.blenddisablelighting = Light_TC2.GetInPlayIntensity * 30
	pTC3.blenddisablelighting = Light_TC3.GetInPlayIntensity * 30
	pTC4.blenddisablelighting = Light_TC4.GetInPlayIntensity * 30
	pTreasure.blenddisablelighting = Light_Treasure.GetInPlayIntensity * 30	
	pTreasureWall.blenddisablelighting = Light_TreasureWall.GetInPlayIntensity * 30	
	pBonusActive.blenddisablelighting = Light_TreasureActive.GetInPlayIntensity * 30	

	pPatch.blenddisablelighting = Light_Patch.GetInPlayIntensity * 30
	pHook.blenddisablelighting = Light_Hook.GetInPlayIntensity * 30
	pLeg.blenddisablelighting = Light_Leg.GetInPlayIntensity * 30

	pRoundTheHorn.blenddisablelighting = Light_RoundTheHorn.GetInPlayIntensity * 30
	pCR.blenddisablelighting = Light_CanonRelease.GetInPlayIntensity * 30
End Sub

'*********************************************************
Sub StartWhirlpoool
	WhirlPoolTimer.Enabled = 1
End Sub

Sub StopWhirlpool
	WhirlPoolTimer.Enabled = 0
End Sub

Sub WhirlpoolTimer_Timer
	WhirlPool.rotz = (WhirlPool.rotz + 2)MOD 360
End Sub

'******************************************************


' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

'//////////////////////////////////////////////////////////////////////
'// Ball
'//////////////////////////////////////////////////////////////////////

If BallBright Then
	table1.BallImage = "ball_HDR_brighter"
Else
	table1.BallImage = "MRBallDark2b"
End If

'//////////////////////////////////////////////////////////////////////
'// Dynamic Ball Shadows
'//////////////////////////////////////////////////////////////////////
' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

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
dim objrtx1(9), objrtx2(9)
dim objBallShadow(9)
Dim OnPF(9)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadow5,BallShadow6,BallShadow7,BallShadow8,BallShadow9)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

Dim ClearSurface:ClearSurface = True		'Variable for hiding flasher shadow on wire and clear plastic ramps
									'Intention is to set this either globally or in a similar manner to RampRolling sounds

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


Sub BallOnPlayfieldNow(yeh, num)		'Only update certain things once, save some cycles
	If yeh Then
		OnPF(num) = True
'		debug.print "Back on PF"
		UpdateMaterial objBallShadow(num).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(num).size_x = 5
		objBallShadow(num).size_y = 4.5
		objBallShadow(num).visible = 1
		BallShadowA(num).visible = 0
	Else
		OnPF(num) = False
'		debug.print "Leaving PF"
		If Not ClearSurface Then
			BallShadowA(num).visible = 1
			objBallShadow(num).visible = 0
		Else
			objBallShadow(num).visible = 1
		End If
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed dynamically if you have a reason
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim gBOT: gBOT=getballs	'Uncomment if you're deleting balls - Don't do it! #SaveTheBalls

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
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update

				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + BallSize/5
					BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					If gBOT(s).X < tablewidth/2 Then
						objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
					Else
						objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
					End If
					objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + offsetY
					objBallShadow(s).size_x = 5 * ((gBOT(s).Z+BallSize)/80)			'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z+BallSize)/80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(30/(gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				End If

			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				If Not OnPF(s) Then BallOnPlayfieldNow True, s

				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + offsetY
'				objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf

			Else												'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If gBOT(s).Z > 30 Then							'In a ramp
				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + BallSize/5
					BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + offsetY
				End If
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height=0.1
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
					If LSd < falloff Then
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
'//////////////////////////////////////////////////////////////////////
'// TargetBounce
'//////////////////////////////////////////////////////////////////////

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
Sub TargetBounce_Hit
	TargetBouncer activeball, 1
End Sub

'//////////////////////////////////////////////////////////////////////
'// PHYSICS DAMPENERS
'//////////////////////////////////////////////////////////////////////

' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR



Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm) 'Rubberizer is handle here
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
			aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class

'//////////////////////////////////////////////////////////////////////
'// TRACK ALL BALL VELOCITIES FOR RUBBER DAMPENER AND DROP TARGETS
'//////////////////////////////////////////////////////////////////////

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

'//////////////////////////////////////////////////////////////////////
'// RAMP ROLLING SFX
'//////////////////////////////////////////////////////////////////////

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
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(6)	

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

'//////////////////////////////////////////////////////////////////////
'// RAMP TRIGGERS
'//////////////////////////////////////////////////////////////////////

Sub ramptrigger01_hit() 'Turn on ramp roll after going above ramp
	WireRampOn True 'Play Plastic Ramp Sound
	debug.print "Start Ramp Roll on at Centre Ramp"
End Sub

Sub ramptrigger02on_hit() 'Turn on ramp roll after released at walk the plank
	WireRampOn True 'Play Plastic Ramp Sound
	debug.print "Ramp Roll on at Start of Invisible WTP Ramp"
End Sub

Sub ramptrigger03on_hit() 'Turn on ramp roll after going above ramp
	WireRampOn True 'Play Plastic Ramp Sound
	debug.print "Ramp Rollon at Start Upkicker rampramp"
End Sub 

Sub ramptrigger04on_hit() 'Turn on ramp roll for load the canon ramp
	WireRampOn True 'Play Plastic Ramp Sound
	debug.print "Ramp Roll on at Start of Arm the Canon ramp"
End Sub

Sub ramptrigger01off_hit()'turn off ramp roll at end of ramp at right lane
	WireRampOff 'Play Plastic Ramp Sound
	debug.print "Ramp Roll Off at End Of centre ramp at the RightLane"
End Sub


Sub ramptrigger01aoff_hit()'turn off ramp roll at walk the plank upkicker
	WireRampOff  'Play Plastic Ramp Sound
	debug.print "Ramp Roll Off PlankUpkicker"
End Sub

Sub ramptrigger01boff_hit()'turn off ramp roll at daveyJones diverter
	WireRampOff  'Play Plastic Ramp Sound
	debug.print "Ramp Roll Off at davey Jones"
End Sub


Sub ramptrigger02off_hit()'turn off ramp roll at end of invisible plank ramp
	WireRampOff  'Play Plastic Ramp Sound
	debug.print "Ramp Roll Off at End of Invisible Ramp"
End Sub

Sub ramptrigger04off_hit()'turn off ramp roll at end of canon load ramp
	WireRampOff  'Play Plastic Ramp Sound
	debug.print "Ramp Roll Off at End of Arm Canon ramp"
End Sub


Sub Wall017_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub


Sub Wall017_unhit()
	PlaySoundAt "WireRamp_Stop", ramptrigger03
End Sub

'//////////////////////////////////////////////////////////////////////
'// Ball Rolling
'//////////////////////////////////////////////////////////////////////

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

Dim BallLights
BallLights = array(BallLight001,BallLight002,BallLight003,BallLight004,BallLight005,BallLight006,BallLight007,BallLight008,BallLight009,BallLight010,BallLight011,BallLight012,BallLight013,BallLight014,BallLight015,BallLight016)
Dim BallColors
BallColors = array( RGB(255,255,0),RGB(0,255,255),RGB(0,255,255),RGB(0,255,0),RGB(255,0,0),RGB(0,255,0),RGB(0,255,255),RGB(255,0,255) )
Dim ColorBalls
Dim B_Color
Dim B_image

Sub StartColouredBalls
	ColorBalls = True
	B_Color = rnd(Int(1)*8)
	B_image = "ballgreydragon2"
End Sub

Sub StopColouredBalls
	ColorBalls = False
	B_Color = RGB(255,255,255)
	B_image = "ballgreydragon2"
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory,i
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        ' Comment the next line if you are not implementing Dyanmic Ball Shadows
        If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
        rolling(b) = False
        StopSound("BallRoll_" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' Pirates follow the ball
	PirateFishy.RotZ = BOT(0).Y\12 +15
	For each i in PirateGunSword
          i.RotZ = - BOT(0).Y\12 -15
     Next
    ' play the rolling sound for each ball

    For b = 0 to UBound(BOT)

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
'                ballpitch = Pitch(BOT(b))
'                ballvol = Vol(BOT(b))
''            Else
 '               ballpitch = Pitch(BOT(b)) + 35000 'increase the pitch on a ramp
  '              ballvol = Vol(BOT(b)) * 10
           End If
            rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial * 0.7, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
		Else
            If rolling(b) = True Then
                StopSound("BallRoll_" & b)
                rolling(b) = False
           End If
        End If

		' Ball Drop Sounds
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If


 '       if b > 1 Then
'            If ColorBalls = True Then
 '               BOT(b).image = B_image
 '               If Bot(b).ID < 22222 Then BOT(b).id = BOT(b).id + 22222 : BOT(b).color = BallColors(B_Color) : B_Color = B_Color + 1 : If B_Color > 7 Then B_Color = 0
 '           Else
 '               BOT(b).color = RGB(255,255,255)
 '               BOT(b).image = "ball_HDR_brighter"
 '               If BOT(b).id > 22222 Then BOT(b).id = BOT(b).id - 22222
 '           End If
		If BOT(b).z > 50 and BallLightActive=True Then 'TurnOnWhiteLight if height is>50
            BallLights(b).x = bot(b).x
            BallLights(b).y = bot(b).y 
            BallLights(b+8).x = bot(b).x 
            BallLights(b+8).y = bot(b).y
        End If

	Next
End Sub





'//////////////////////////////////////////////////////////////////////
'// Mechanic Sounds
'//////////////////////////////////////////////////////////////////////

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.  
' Create the following new collections:
' 	Metals (all metal objects, metal walls, metal posts, metal wire guides)
' 	Apron (the apron walls and plunger wall)
' 	Walls (all wood or plastic walls)
' 	Rollovers (wire rollover triggers, star triggers, or button triggers)
' 	Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
' 	Gates (plate gates)
' 	GatesWire (wire gates)
' 	Rubbers (all rubbers including posts, sleeves, pegs, and bands)
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
' Tutorial vides by Apophis
' Part 1: 	https://youtu.be/PbE2kNiam3g
' Part 2: 	https://youtu.be/B5cm1Y8wQsk
' Part 3: 	https://youtu.be/eLhWyuYOyGg


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 0.5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.5           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel =1								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 1.5											'volume multiplier; must not be zero
RubberWeakSoundFactor =1.2											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.8										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.3											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


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
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

	If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
	Else
		AudioFade = Csng(-((- tmp) ^10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

	If tmp > 0 Then
		AudioPan = Csng(tmp ^10)
	Else
		AudioPan = Csng(-((- tmp) ^10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
	RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

'Function RndNum(min, max)
'	RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
'End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
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
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
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
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then		
		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If	
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
	RandomSoundWall()      
End Sub

Sub RandomSoundWall()
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

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
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
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()		
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

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

Sub Targets_Hit (idx)
	PlayTargetSound	
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()			
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)	
	SoundPlayfieldGate	
End Sub	

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx < -8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If Activeball.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If activeball.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
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


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd*6)+1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315									'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05									'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025*RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025*RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025*RelayFlashSoundLevel, obj			
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025*RelayFlashSoundLevel, obj		
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************

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

Function RotPoint(x,y,angle)
	dim rx, ry
	rx = x*dCos(angle) - y*dSin(angle)
	ry = x*dSin(angle) + y*dCos(angle)
	RotPoint = Array(rx,ry)
End Function

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

Dim PopValue0 '100
Dim PopValue1 '1000
Dim PopValue2 '10000
Dim PopValue3 '500000


Dim ChickenTH
Sub Target_Chicken_Hit
		ChickenLightTimer.Enabled=1
		StartChickenLittleJump
		ChickenTH= ChickenTH+1
	If ChickenTH=3 Then PlaySound "CO_Chicken3":Light_Chicken2.State=0:Light_Chicken.State=0:Light_Chicken3.State=2 :debug.print "ChickenTargetCount=0":ChickenTH=0:SeagullSoup:PopValue3=1:PopValue2=0:PopValue1=0: Exit Sub
	If ChickenTH=2 Then PlaySound "CO_Chicken2":Light_Chicken3.State=0:Light_Chicken2.State=0:Light_Chicken.State=2 :AddScore 50000: debug.print "ChickenTarget2":PopValue3=0:PopValue2=1:PopValue1=0: Exit Sub
	If ChickenTH=1 Then PlaySound "CO_Chicken1":Light_Chicken.State=0:Light_Chicken3.State=0:Light_Chicken2.State=2 :AddScore 50000:debug.print "ChickenTarget1":PopValue3=0:PopValue2=0:PopValue1=1 
End Sub





Sub SeagullSoup
	DMD CL(0, "SEAGULL SOUP " ), CL(1, " 20000"), "", eNone, eBlink, eNone, 3000, True, ""	
	AddScore 20000
End Sub 

dim chickenlightflash:chickenlightflash=0
Sub ChickenLightTimer_timer
	chickenlightflash=chickenlightflash+1
	Select Case chickenlightflash
		case 1: Strip4.visible = 1
		case 3:	Strip4.visible = 0
		case 5:	Strip4.visible = 1
		case 7:	Strip4.visible = 0
		case 9:	Strip4.visible = 1
		case 11:Strip4.visible = 0
		case 13:Strip4.visible = 1
		case 15:Strip4.visible = 0
		case 17:Strip4.visible = 1
		case 19:Strip4.visible = 0
		case 21:Strip4.visible = 1
		case 23:Strip4.visible = 0:ChickenLightTimer.enabled=0:chickenlightflash=0
	End Select
End Sub

' Shake animation Target_PopBonus


Dim ChickenLittleJump
Sub StartChickenLittleJump
    ChickenLittleJump=6
    ChickenLittleTimer.Enabled = True
End Sub

Sub ChickenLittleTimer_Timer
	ChickenLittle.TransZ=ChickenLittleJump
    If ChickenLittleJump = 0 Then ChickenLittleTimer.Enabled = True:Exit Sub
    If ChickenLittleJump  <0 Then
        ChickenLittleJump = ABS(ChickenLittleJump)- 0.1
    Else
        ChickenLittleJump = - ChickenLittleJump+ 0.1
    End If
End Sub


Dim GullTH
Sub Target_Gull_Hit
	GullLightTimer.Enabled=1
    StartTotemShake
	StartGullJump
	GullTH= GullTH+1
	If GullTH=3 Then PlaySound "CO_Gull3":Light_Gull.State=0:Light_Gull2.State=0:Light_Gull3.State=2 :debug.print "GullTargetCount=0":GullTH=0:ChickenSoup:CheckPopValue3:Exit Sub
	If GullTH=2 Then PlaySound "CO_Gull2":Light_Gull2.State=0:Light_Gull.State=2:Light_Gull3.State=0:AddScore 1000: debug.print "GullTarget2": CheckPopValue2: Exit Sub
	If GullTH=1 Then PlaySound "CO_Gull1":Light_Gull.State=0:Light_Gull3.State=0:Light_Gull2.State=2 :AddScore 1000:debug.print "GullTarget1": CheckPopValue1:End If
End Sub

Sub CheckPopValue3
	PopValue3=1
	DMD CL(0, "POPS VALUE " ), CL(1, " 10000"), "", eNone, eBlink, eNone, 3000, True, "":Exit Sub
End Sub

Sub CheckPopValue2
	If PopValue3=1 Then 	DMD CL(0, "POPS VALUE " ), CL(1, " 50000"), "", eNone, eBlink, eNone, 3000, True, "":End If
	If PopValue1=1 Then PopValue2=1:PopValue1=0:PopValue3=0: DMD CL(0, "   POPS VALUE " ), CL(1, "   5000"), "", eNone, eBlink, eNone, 3000, True, "":End If
	If PopValue2=0 Then PopValue1=0:PopValue2=1:PopValue3=0:DMD CL(0, "    POPS VALUE " ), CL(1, "   5000"), "", eNone, eBlink, eNone, 3000, True, "" End If
End Sub

Sub CheckPopValue1
	If PopValue3=1 Then 	DMD CL(0, "POPS VALUE " ), CL(1, " 100000"), "", eNone, eBlink, eNone, 3000, True, "":End If
	If PopValue1=0 Then PopValue1=1:PopValue2=0:PopValue3=0: DMD CL(0, "   POPS VALUE " ), CL(1, " 1000"), "", eNone, eBlink, eNone, 3000, True, "":End If
	If PopValue2=1 Then PopValue1=0:PopValue2=1:PopValue3=0:DMD CL(0, "    POPS VALUE " ), CL(1, " 1000"), "", eNone, eBlink, eNone, 3000, True, "" End If
End Sub

Sub ChickenSoup
	DMD CL(0, "CHICKEN SOUP " ), CL(1, " 20000"), "", eNone, eBlink, eNone, 3000, True, ""	
	AddScore 20000
End Sub 

dim gulllightflash:gulllightflash=0
Sub GullLightTimer_timer
	gulllightflash=gulllightflash+1
	Select Case gulllightflash
		case 1: Strip5.visible = 1
		case 3:	Strip5.visible = 0
		case 5:	Strip5.visible = 1
		case 7:	Strip5.visible = 0
		case 9:	Strip5.visible = 1
		case 11:Strip5.visible = 0
		case 13:Strip5.visible = 1
		case 15:Strip5.visible = 0
		case 17:Strip5.visible = 1
		case 19:Strip5.visible = 0
		case 21:Strip5.visible = 1
		case 23:Strip5.visible = 0:GullLightTimer.enabled=0:gulllightflash=0
	End Select
End Sub


Dim SeagullJump:SeagullJump = 0
Sub StartGullJump
	SeagullJump=6
    GullTimer.Enabled = True
End Sub

Sub GullTimer_Timer
	Seagull.Roty=SeagullJump
    If SeagullJump = 0 Then GullTimer.Enabled = False:Exit Sub
    If SeagullJump <0 Then
        SeagullJump = ABS(SeagullJump)- 0.1
    Else
        SeagullJump = - SeagullJump + 0.1
    End If
 End Sub


Sub StartTotemShake
    TotemF.RotatetoEnd
    TotemF.TimerEnabled = True
End Sub

Sub TotemF_Timer
    TotemF.RotatetoStart
    TotemF.TimerEnabled = False
End Sub

' Tiki Shake

Dim TikiShake:TikiShake = 0
Dim TikiREShake:TikiREShake=0
Dim TikiLEShake:TikiLEShake=0

Sub StartTikiShake
    TikiShake = 6:TikiREShake=6:TikiLEShake=6
    ShakeTiki.Enabled = True
End Sub

Sub ShakeTiki_Timer
    Tiki.Roty = TikiShake
	pTikiRightEye.TransX=TikiREShake
	pTikiLeftEye.TransX=TikiLEShake
    If TikiShake = 0 Then ShakeTiki.Enabled = False:TikiLightTimer.Enabled=True:Exit Sub
    If TikiShake <0 Then

        TikiShake = ABS(TikiShake)- 0.1
		TikiREShake= ABS(TikiREShake)- 0.1
		TikiLEShake= ABS(TikiLEShake)-1
    Else
        TikiShake = - TikiShake + 0.1
		TikiREShake= -TikiREShake + 0.1
		TikiLEShake= -TikiREShake -0.1
    End If
End Sub

Sub TikiLightTimer_Timer
	debug.print "TikiLightsoff"
	LightTikiRightEye.State=0:LightTikiLeftEye.State=0
	TikiLightTimer.Enabled=False
End Sub

' Chicken Jump

Sub StartChickenJump
    ChickenF.RotatetoEnd
    ChickenF.TimerEnabled = True
End Sub

Sub ChickenF_Timer
    ChickenF.RotatetoStart
    ChickenF.TimerEnabled = False
End Sub

'*************************
'Parrot UP/DOWN Animation
'*************************

Dim ParrotPos, ParrotDir, ParrotShakePos, ParrotShakeDir,ParrotHitPos, ParrotHits

Dim bParrotUp, bPlayfieldSlimed
ParrotPos = -137
ParrotShakePos = 0

Sub ParrotLocation(param)
    Select Case param
        Case 1:ParrotRed.X = 121:ParrotRed.Y = 507:TriggerRedBird1.Enabled = 1:TriggerRedBird2.Enabled = 0:debug.print "TriggerRedbird1 enabled=true- Trig2 disabled"
				SpinDiscPolly1.X=121:SpinDiscPolly1.Y=497:debug.print "ParrotTrigger1Enabled"
        Case 2:ParrotRed.X = 263:ParrotRed.Y = 507:TriggerRedBird2.Enabled = 1:TriggerRedBird1.Enabled = 0:debug.print "TriggerRedbird2 enabled=true- Trig1 disabled"
				SpinDiscPolly1.X=263:SpinDiscPolly1.Y=497:debug.print "ParrotTrigger2Enabled"
    End Select
End Sub



Sub ParrotAnimTimer_Timer()
    ParrotShakeTimer.Enabled = 0
	ParrotLocationTimer.enabled=1
    ParrotPos = ParrotPos + ParrotDir
    'Parrot is moving up
    If ParrotPos >= 0 Then
        DOF 127, DOFOff
        Me.Enabled = 0
        ParrotPos = 0
        ParrotShakeDir = 1
        ParrotShakeTimer.Enabled = 1
    End If
    'Parrot is moving down
    If ParrotPos <= -137 Then
        DOF 127, DOFOff
        Me.Enabled = 0
        ParrotPos = -137
		TriggerRedBird1.Enabled = 0
		TriggerRedBird2.Enabled = 0
    End If
    ParrotRed.Transz = ParrotPos

End Sub

Sub ParrotShakeTimer_Timer
    ParrotShakePos = ParrotShakePos + ParrotShakeDir
    'Parrot is moving up
    If ParrotShakePos > 10 Then
        ParrotShakeDir = -1
    End If
    'Parrot is moving down
    If ParrotShakePos < 0 Then
        ParrotShakeDir = 1
    End If
    ParrotRed.Transz = ParrotShakePos

End Sub

Sub ParrotMoveUp()
    'Play a Sound?
    ParrotDir = 2
    ParrotAnimTimer.Enabled = 1
	ParrotSwirlTimer.Enabled=1
	SpinDiscPolly1.Visible=1
	TriggerRedBird1.Enabled = 1
End Sub

Sub ParrotMoveDown()
    ParrotDir = -2
    ParrotAnimTimer.Enabled = 1
	ParrotLocationTimer.Enabled=0:debug.print "ParrotLocationTimer-DisabledatParrotMoveDown"
	ParrotFinishTimer.Enabled=1:debug.print "ParrotFinishTimerEnabled"
	ParrotSwirlTimer.Enabled=0
	SpinDiscPolly1.Visible=0
	TriggerRedBird1.Enabled = 0:TriggerRedBird2.Enabled = 0:
End Sub:

Sub ParrotFinishTimer_Timer()
	ParrotShakeTimer.Enabled = 0:debug.print "ParrotShakeTimer.Enabled=false"
	ParrotLocationTimer.Enabled=0
    ParrotAnimTimer.Enabled = 0:debug.print "ParrotAnimTimer.Enabled"	
    TriggerRedBird1.Enabled = 0:debug.print "TriggerRedbird1 enabled=false"
    TriggerRedBird2.Enabled = 0:debug.print "TriggerRedbird1 enabled=false"
	ParrotFinishTimer.Enabled=0:debug.print "ParrotFinishTimer enabled=false"
End Sub

Sub TriggerRedBird1_Hit()
    If bParrotUp AND ActiveBall.VelY < 0 Then
 '       If Modes(0) = 4 Then
 '           CompleteHeSlimedMe
 '           Exit Sub
        End If
        ParrotHitPos = 6:ParrotHitTimer.Enabled = 1
        DOF 139, DOFOn
        ParrotHits = ParrotHits + 1
        ParrotHitCheck
  '  End If
End Sub

Sub TriggerRedBird2_Hit()
    If bParrotUp AND ActiveBall.VelY < 0 Then
   '     If Modes(0) = 4 Then
  '          CompleteHeSlimedMe
   '         Exit Sub
        End If
        ParrotHitPos = 6:ParrotHitTimer.Enabled = 1
        DOF 139, DOFOn
        ParrotHits = ParrotHits + 1
        ParrotHitCheck
'    End If
End Sub



Sub ParrotHitTimer_Timer()
    ParrotRed.TransX = ParrotHitPos
    If ParrotHitPos <= 0.1 AND ParrotHitPos >= -0.1 Then Me.Enabled = False:DOF 139, DOFOff:Exit Sub
    If ParrotHitPos < 0 Then
        ParrotHitPos = ABS(ParrotHitPos)- 0.1
    Else
        ParrotHitPos = - ParrotHitPos + 0.1
    End If
End Sub

Sub ParrotLocationTimer_Timer
    ParrotLocation INT(RND * 3 + 1)
End Sub



Sub ParrotHitCheck
	AngryParrotCallout
	If ParrotHits=3 Then:ParrotMoveDown: AwardParrotGhost: ParrotHits=0:StopAward4:End If
End Sub

Sub AwardParrotGhost
	If Hard=1 Then vpmtimer.addtimer 5000, "AwardSuperJackpot'":End If
	If Hard=0 Then vpmtimer.addtimer 5000, "AwardSuperJackpot2'":End If
	If PollyComplete(CurrentPlayer)=False Then CheckModesCompleted: LightAward1.State=1:debug.print "CheckModesComplete-atPolly":End If
		PollyComplete(CurrentPlayer)=True
End Sub


Dim AngryParrotCall
Sub AngryParrotCallout
	AngryParrotCall=AngryParrotCall+1
	Select Case AngryParrotCall

     
		Case 1:		PlaySound "CO_Parrot5ozone":CalloutTimer.Enabled=True: debug.print "DeadParrotCall1"
					
					ParrotFlasherTimer.Enabled=1
				If Hard=1 Then DMD CL(0, "    NOT HAPPY" ), CL(1, "1200000"), "", eBlink, eBlink, eNone, 3500, True, "": AddScore 1200000:End If
				If Hard=0 Then DMD CL(0, "    NOT HAPPY" ), CL(1, "1500000"), "", eBlink, eBlink, eNone, 3500, True, "": AddScore 1500000:End If				
			

      
		Case 2:		PlaySound "CO_Parrot2ozone":CalloutTimer.Enabled=True: debug.print "DeadParrotCall2"
					DMD CL(0, "   STOLE MY" ), CL(1, "LITTLE BELL"), "", eBlink, eBlink, eNone, 3500, True, ""
					ParrotFlasherTimer.Enabled=1
				If Hard=1 Then AddScore 1200000:End If
				If Hard=0 Then AddScore 1500000:End If
			

     
		Case 3:		PlaySound "CO_Parrot4ozone":CalloutTimer.Enabled=True:AngryParrotCall=0: debug.print "DeadParrotCall3"
					DMD CL(0, "   VERY UPSET" ), CL(1, "NINCOMPOOP"), "", eBlink, eBlink, eNone, 3500, True, ""
					ParrotFlasherTimer.Enabled=1
					AngryParrotCall=0
				If Hard=1 Then AddScore 1200000:End If
				If Hard=0 Then AddScore 1500000:End If			
	End Select
End Sub







'*************************
'DrunkSkeleton1-UP/DOWN Animation
'*************************

Dim Skeleton1Pos, Skeleton1Dir, Skeleton1ShakePos, Skeleton1ShakeDir

Skeleton1Pos = -260
Skeleton1ShakePos = 0

Sub Skeleton1AnimTimer_Timer()
    Skeleton1ShakeTimer.Enabled = 0
    Skeleton1Pos = Skeleton1Pos + Skeleton1Dir
    'Skeleton1 is moving up
    If Skeleton1Pos >= 0 Then
        Me.Enabled = 0
        Skeleton1Pos = 0
        Skeleton1ShakeDir = 1
        Skeleton1ShakeTimer.Enabled = 1
    End If
    'Skeleton1 is moving down
    If Skeleton1Pos <= -260 Then
        Me.Enabled = 0
        Skeleton1Pos = -260
    End If
Dim i
For each i in DrunkSkeleton1: i.Transz = Skeleton1Pos: Next
End Sub

Sub Skeleton1ShakeTimer_Timer
    Skeleton1ShakePos = Skeleton1ShakePos + Skeleton1ShakeDir
    'Skeleton1 is moving up
    If Skeleton1ShakePos > 10 Then
        Skeleton1ShakeDir = -1
    End If
    'Skeleton1 is moving down
    If Skeleton1ShakePos < 0 Then
        Skeleton1ShakeDir = 1
    End If
Dim i
For each i in DrunkSkeleton1: i.Transz = Skeleton1ShakePos: Next
End Sub

Dim Skeleton1Down
Sub Skeleton1MoveUp()
	Skeleton1Down=false
    'Play a Sound?
    Skeleton1Dir = 2
    Skeleton1AnimTimer.Enabled = 1
	Skeleton1Wall.IsDropped=False:Skeleton1Wallb.IsDropped=False
End Sub

Sub Skeleton1MoveDown()
	Skeleton1Down=True
    Skeleton1Dir = -2
    Skeleton1AnimTimer.Enabled = 1
	Skeleton1Wall.Isdropped=True:Skeleton1Wallb.Isdropped=True
	SkeletonSwirlTimer1.Enabled=False:SpinDiscSkeleton1.Visible=False
End Sub

Sub SkeletonSwirlTimer1_Timer
	SpinDiscSkeleton1.rotz = (SpinDiscSkeleton1.rotz + 5)mod 360
End Sub
'*************************
'DrunkSkeleton2-UP/DOWN Animation
'*************************

Dim Skeleton2Pos, Skeleton2Dir, Skeleton2ShakePos, Skeleton2ShakeDir

Skeleton2Pos = -200
Skeleton2ShakePos = 0

Sub Skeleton2AnimTimer_Timer()
    Skeleton2ShakeTimer.Enabled = 0
    Skeleton2Pos = Skeleton2Pos + Skeleton2Dir
    'skeleton2 is moving up
    If Skeleton2Pos >= 0 Then
        Me.Enabled = 0
        Skeleton2Pos = 0
        Skeleton2ShakeDir = 1
        Skeleton2ShakeTimer.Enabled = 1
    End If
    'Skeleton2 is moving down
    If Skeleton2Pos <= -200 Then
        Me.Enabled = 0
        Skeleton2Pos = -200
    End If
Dim i
For each i in DrunkSkeleton2: i.Transz = Skeleton2Pos: Next
End Sub

Sub Skeleton2ShakeTimer_Timer
    Skeleton2ShakePos = Skeleton2ShakePos + Skeleton2ShakeDir
    'Skeleton2 is moving up
    If Skeleton2ShakePos > 10 Then
        Skeleton2ShakeDir = -1
    End If
    'Skeleton2 is moving down
    If Skeleton2ShakePos < 0 Then
        Skeleton2ShakeDir = 1
    End If
Dim i
For each i in DrunkSkeleton2: i.Transz = Skeleton2ShakePos: Next
End Sub

Dim Skeleton2Down
Sub Skeleton2MoveUp()
	Skeleton2Down=false
    'Play a Sound?
    Skeleton2Dir = 2
    Skeleton2AnimTimer.Enabled = 1
	Skeleton2Wall.IsDropped=False:Skeleton2Wallb.IsDropped=False
End Sub


Sub Skeleton2MoveDown()
	Skeleton2Down=True
    Skeleton2Dir = -2
    Skeleton2AnimTimer.Enabled = 1
	Skeleton2Wall.IsDropped=True:Skeleton2Wallb.IsDropped=True
	SkeletonSwirlTimer2.Enabled=False:SpinDiscSkeleton2.Visible=False
End Sub

Sub SkeletonSwirlTimer2_Timer
	SpinDiscSkeleton2.rotz = (SpinDiscSkeleton2.rotz + 5)mod 360
End Sub

'*************************
'DrunkSkeleton3-UP/DOWN Animation
'*************************

Dim Skeleton3Pos, Skeleton3Dir, Skeleton3ShakePos, Skeleton3ShakeDir

Skeleton3Pos = -200
Skeleton3ShakePos = 0

Sub Skeleton3AnimTimer_Timer()
    Skeleton3ShakeTimer.Enabled = 0
    Skeleton3Pos = Skeleton3Pos + Skeleton3Dir
    'Skeleton3 is moving up
    If Skeleton3Pos >= 0 Then
        Me.Enabled = 0
        Skeleton3Pos = 0
        Skeleton3ShakeDir = 1
        Skeleton3ShakeTimer.Enabled = 1
    End If
    'Skeleton3 is moving down
    If Skeleton3Pos <= -200 Then
        Me.Enabled = 0
        Skeleton3Pos = -200
    End If
Dim i
For each i in DrunkSkeleton3: i.Transz = Skeleton3Pos: Next
End Sub

Sub Skeleton3ShakeTimer_Timer
    Skeleton3ShakePos = Skeleton3ShakePos + Skeleton3ShakeDir
    'Skeleton3 is moving up
    If Skeleton3ShakePos > 10 Then
        Skeleton3ShakeDir = -1
    End If
    'Skeleton3 is moving down
    If Skeleton3ShakePos < 0 Then
        Skeleton3ShakeDir = 1
    End If
Dim i
For each i in DrunkSkeleton3: i.Transz = Skeleton3ShakePos: Next
End Sub

Dim Skeleton3Down
Sub Skeleton3MoveUp()
	Skeleton3Down=False
    'Play a Sound?
    Skeleton3Dir = 2
    Skeleton3AnimTimer.Enabled = 1
	Skeleton3Wall.IsDropped=False:Skeleton3Wallb.IsDropped=False
End Sub

Sub Skeleton3MoveDown()
	Skeleton3Down=True
    Skeleton3Dir = -2
    Skeleton3AnimTimer.Enabled = 1
	Skeleton3Wall.IsDropped=True:Skeleton3Wallb.IsDropped=True
	SkeletonSwirlTimer3.Enabled=False:SpinDiscSkeleton3.Visible=False
End Sub

Sub SkeletonSwirlTimer3_Timer
	SpinDiscSkeleton3.rotz = (SpinDiscSkeleton3.rotz + 5)mod 360
End Sub

'*************************
'DrunkSkeleton4-UP/DOWN Animation
'*************************

Dim Skeleton4Pos, Skeleton4Dir, Skeleton4ShakePos, Skeleton4ShakeDir

Skeleton4Pos = -200
Skeleton4ShakePos = 0

Sub Skeleton4AnimTimer_Timer()
    Skeleton4ShakeTimer.Enabled = 0
    Skeleton4Pos = Skeleton4Pos + Skeleton4Dir
    'Skeleton4 is moving up
    If Skeleton4Pos >= 0 Then
        Me.Enabled = 0
        Skeleton4Pos = 0
        Skeleton4ShakeDir = 1
        Skeleton4ShakeTimer.Enabled = 1
    End If
    'Skeleton4 is moving down
    If Skeleton4Pos <= -200 Then
        Me.Enabled = 0
        Skeleton4Pos = -200
    End If
Dim i
For each i in DrunkSkeleton4: i.Transz = Skeleton4Pos: Next
End Sub

Sub Skeleton4ShakeTimer_Timer
    Skeleton4ShakePos = Skeleton4ShakePos + Skeleton4ShakeDir
    'Skeleton4 is moving up
    If Skeleton4ShakePos > 10 Then
        Skeleton4ShakeDir = -1
    End If
    'Skeleton4 is moving down
    If Skeleton4ShakePos < 0 Then
        Skeleton4ShakeDir = 1
    End If
Dim i
For each i in DrunkSkeleton4: i.Transz = Skeleton4ShakePos: Next
End Sub

Dim Skeleton4Down
Sub Skeleton4MoveUp()
	Skeleton4Down=False
    'Play a Sound?
    Skeleton4Dir = 2
    Skeleton4AnimTimer.Enabled = 1
	Skeleton4Wall.IsDropped=False:Skeleton4Wallb.IsDropped=False
End Sub


Sub Skeleton4MoveDown()
	Skeleton4Down=True
    Skeleton4Dir = -2
    Skeleton4AnimTimer.Enabled = 1
	Skeleton4Wall.IsDropped=True:Skeleton4Wallb.IsDropped=True
	SkeletonSwirlTimer4.Enabled=False:SpinDiscSkeleton4.Visible=False
End Sub

Sub SkeletonSwirlTimer4_Timer
	SpinDiscSkeleton4.rotz = (SpinDiscSkeleton4.rotz + 5)mod 360
End Sub
'*************************
'TreasureChest-UP/DOWN Animation
'*************************

Dim TreasureChest2Pos, TreasureChest2Dir
TreasureChest2Pos = 20


Sub TreasureChest2AnimTimer_Timer()
    TreasureChest2Pos = TreasureChest2Pos + TreasureChest2Dir
    'Skeleton4 is moving up
    If TreasureChest2Pos >= 20 Then
        Me.Enabled = 0
        TreasureChest2Pos = 75
    End If
    'TreasureChest2 is moving down
    If TreasureChest2Pos <= 20 Then
        Me.Enabled = 0
        TreasureChest2Pos= 20
    End If
Dim i
For each i in TreasureChest2: i.Transz = TreasureChest2Pos: Next
End Sub


Sub TreasureChest2MoveUp()
    'Play a Sound?
    TreasureChest2Dir = 2
    TreasureChest2AnimTimer.Enabled = 1
	DOF 133, DOFPulse
End Sub

Sub TreasureChest2MoveDown()
	debug.print "TreasureCest2MoveDown"
    TreasureChest2Dir = -2
    TreasureChest2AnimTimer.Enabled = 1
End Sub





'*************************
'Shark1-UP/DOWN Animation
'*************************


Dim Shark1Pos, Shark1Dir, Shark1ShakePos, Shark1ShakeDir,Shark1HitPos, Shark1Hits
Dim bSharkUp
Shark1Pos = -110
Shark1ShakePos = 0

Sub Shark1Location(param)
    Select Case param
        Case 1:	SharkyBoy1.X = 233:SharkyBoy1.Y = 859:SharkyBoy1.RotZ=150:TriggerShark1.Enabled = 1
				TriggerShark2.Enabled = 0:TriggerShark3.Enabled = 0:TriggerShark4.Enabled = 0:TriggerShark5.Enabled = 0
				SpinDiscShark.X=233:SpinDiscShark.Y=831
        Case 2:	SharkyBoy1.X = 427:SharkyBoy1.Y = 871:SharkyBoy1.RotZ=170:TriggerShark2.Enabled = 1
				TriggerShark1.Enabled = 0:TriggerShark3.Enabled = 0:TriggerShark4.Enabled = 0:TriggerShark5.Enabled = 0
				SpinDiscShark.X=437:SpinDiscShark.Y=861
        Case 3:	SharkyBoy1.X = 479:SharkyBoy1.Y = 719:SharkyBoy1.RotZ=150:TriggerShark3.Enabled = 1
				TriggerShark1.Enabled = 0:TriggerShark2.Enabled = 0:TriggerShark4.Enabled = 0:TriggerShark5.Enabled = 0
				SpinDiscShark.X=489:SpinDiscShark.Y=709
        Case 4:	SharkyBoy1.X = 595:SharkyBoy1.Y = 511:SharkyBoy1.RotZ=165:TriggerShark4.Enabled = 1
				TriggerShark1.Enabled = 0:TriggerShark2.Enabled = 0:TriggerShark3.Enabled = 0:TriggerShark5.Enabled = 0
				SpinDiscShark.X=605:SpinDiscShark.Y=501
        Case 5:	SharkyBoy1.X = 717:SharkyBoy1.Y = 1033:SharkyBoy1.RotZ=220:TriggerShark5.Enabled =1
				TriggerShark1.Enabled = 0:TriggerShark2.Enabled = 0:TriggerShark3.Enabled = 0:TriggerShark4.Enabled = 0
				SpinDiscShark.X=727:SpinDiscShark.Y=1013
    End Select
End Sub


Sub Shark1AnimTimer_Timer()
    Shark1ShakeTimer.Enabled = 0
	Shark1LocationTimer.Enabled=1
    Shark1Pos = Shark1Pos + Shark1Dir
    'Shark1 is moving up
    If Shark1Pos >= -20 Then
        DOF 127, DOFOff
        Me.Enabled = 0
        Shark1Pos = 0
        Shark1ShakeDir = 1
        Shark1ShakeTimer.Enabled = 1
    End If
    'Shark1 is moving down
    If Shark1Pos <= -110 Then
debug.print "SharkPos<-110"
        DOF 127, DOFOff
        Me.Enabled = 0
        Shark1Pos = -110

    End If
    SharkyBoy1.Transz = Shark1Pos

End Sub

Sub Shark1ShakeTimer_Timer
    Shark1ShakePos = Shark1ShakePos + Shark1ShakeDir
    'Shark1 is moving up
    If Shark1ShakePos > 10 Then
        Shark1ShakeDir = -1
    End If
    'Shark1 is moving down
    If Shark1ShakePos < 0 Then
        Shark1ShakeDir = 1
    End If
    SharkyBoy1.Transz = Shark1ShakePos
End Sub

Sub Shark1MoveUp()
    'Play a Sound?
	SharkyBoy1.Visible=True
    Shark1Dir = 2
    Shark1AnimTimer.Enabled = 1
End Sub

Sub Shark1MoveDown()
debug.print "SharkMoveDown"
	Shark1Pos = -110
    Shark1Dir = -2
    Shark1AnimTimer.Enabled = 1

	Shark1AnimStopTimer.Enabled=1
	SharkAttackTimer.Enabled=0
	SpinDiscShark.Visible=0
	Shark1Hits=0
End Sub

Sub Shark1AnimStopTimer_Timer
debug.print "SharkAnimStopTimer"
	Shark1AnimTimer.Enabled=0:		debug.print "Shark1AnimationTimer-Disabled"
	Shark1AnimStopTimer.Enabled=0:	debug.print "Shark1AnimationStopTimer-Disabled"
	SharkyBoy1.Visible=False:		debug.print "Shark1-Invisible"
	Shark1LocationTimer.enabled=0:	debug.print "Shark1LoacationTimer-Disabled"
    TriggerShark1.Enabled = 0:TriggerShark2.Enabled = 0:TriggerShark3.Enabled = 0:TriggerShark4.Enabled = 0:TriggerShark5.Enabled = 0:debug.print "Shark Triggers Disabled"
End Sub

Sub TriggerShark1_Hit()
    If bSharkUp AND ActiveBall.VelY < 0 Then
 '       If Modes(0) = 4 Then
 '           CompleteHeSlimedMe
 '           Exit Sub
        End If
        Shark1HitPos = 6:Shark1HitTimer.Enabled = 1
        DOF 139, DOFOn
        Shark1Hits = Shark1Hits + 1
        Shark1HitCheck
	debug.print "Shark1_GutsInTheLuts"
  '  End If
End Sub

Sub TriggerShark2_Hit()
    If bSharkUp AND ActiveBall.VelY < 0 Then
 '       If Modes(0) = 4 Then
 '           CompleteHeSlimedMe
 '           Exit Sub
        End If
        Shark1HitPos = 6:Shark1HitTimer.Enabled = 1
        DOF 139, DOFOn
        Shark1Hits = Shark1Hits + 1
        Shark1HitCheck
	debug.print "Shark2_GutsInTheLuts"
  '  End If
End Sub

Sub TriggerShark3_Hit()
    If bSharkUp AND ActiveBall.VelY < 0 Then
 '       If Modes(0) = 4 Then
 '           CompleteHeSlimedMe
 '           Exit Sub
        End If
        Shark1HitPos = 6:Shark1HitTimer.Enabled = 1
        DOF 139, DOFOn
        Shark1Hits = Shark1Hits + 1
        Shark1HitCheck
	debug.print "Shark3_GutsInTheLuts"
  '  End If
End Sub

Sub TriggerShark4_Hit()
    If bSharkUp AND ActiveBall.VelY < 0 Then
 '       If Modes(0) = 4 Then
 '           CompleteHeSlimedMe
 '           Exit Sub
        End If
        Shark1HitPos = 6:Shark1HitTimer.Enabled = 1
        DOF 139, DOFOn
        Shark1Hits = Shark1Hits + 1
        Shark1HitCheck
	debug.print "Shark4_GutsInTheLuts"
  '  End If
End Sub

Sub TriggerShark5_Hit()
    If bSharkUp AND ActiveBall.VelY < 0 Then
 '       If Modes(0) = 4 Then
 '           CompleteHeSlimedMe
 '           Exit Sub
        End If
        Shark1HitPos = 6:Shark1HitTimer.Enabled = 1
        DOF 139, DOFOn
        Shark1Hits = Shark1Hits + 1
        Shark1HitCheck
	debug.print "Shark5_GutsInTheLuts"
  '  End If
End Sub

Sub SharkyBoyCallout

End Sub



Sub Shark1HitTimer_Timer()
    SharkyBoy1.TransX = Shark1HitPos
    If Shark1HitPos <= 0.1 AND Shark1HitPos >= -0.1 Then Me.Enabled = False:DOF 139, DOFOff:Exit Sub
    If Shark1HitPos < 0 Then
        Shark1HitPos = ABS(Shark1HitPos)- 0.1
    Else
        Shark1HitPos = - Shark1HitPos + 0.1
    End If
End Sub

Sub Shark1LocationTimer_Timer
    Shark1Location INT(RND * 5 + 1) 'Random Position Selection
End Sub


Sub Shark1HitCheck
	If Shark1Hits=1 And CalloutActive=False Then CalloutActive=True:PlaySound "CO_ItsOrangeWet":CalloutTimer.Enabled=True: End If
	If Shark1Hits=1 Then DMD CL(0, "    TIGGER "), CL(1, "500000 "), "", eNone, eBlink, eNone, 3000, True, "":AddScore 500000:AwardFlasherTimer.Enabled=1: End If
	If Shark1Hits=2 And CalloutActive=False Then CalloutActive=True:PlaySound "CO_DownTheGobWet2":CalloutTimer.Enabled=True:End If
	If Shark1Hits=2	Then DMD CL(0, "  DOWN THE GOB "), CL(1, "700000 "), "", eNone, eBlink, eNone, 3000, True, "":AddScore 700000:AwardFlasherTimer.Enabled=1:End If
	If Shark1Hits=3 Then CheckSharkHardEasy1:AwardFlasherTimer.Enabled=1:End If
	If Shark1Hits=4 And CalloutActive=False Then CalloutActive=True:PlaySound "CO_WhatYouFinkWet":CalloutTimer.Enabled=True: End If
	If Shark1Hits=4 Then DMD CL(0, "  WHAT YOU FINK "), CL(1, "900000 "), "", eNone, eBlink, eNone, 3000, True, "":AddScore 900000:AwardFlasherTimer.Enabled=1:End If
	If Shark1Hits=5 And CalloutActive=False Then CalloutActive=True:PlaySound "CO_DownTheGobWet2":CalloutTimer.Enabled=True: End If
	If Shark1Hits=5 Then DMD CL(0, "  DOWN THE GOB "), CL(1, "1000000 "), "", eNone, eBlink, eNone, 3000, True, "":AwardJackpot:AwardFlasherTimer.Enabled=1:End If
	If Shark1Hits=6 Then CheckSharkHardEasy2:Shark1MoveDown:Shark1Hits=0: AwardFlasherTimer.Enabled=1:StopAward1:End If
End Sub

Sub CheckSharkHardEasy1
			If Hard=1 Then 	
					SharkModeComplete:CalloutActive=True
					PlaySound "CO_NervousWet":CalloutTimer.Enabled=True
					AwardJackpot2
			End If
			If Hard=0 Then 
					SharkModeComplete:CalloutActive=True:PlaySound "CO_NervousWet":CalloutTimer.Enabled=True
					DMD CL(0, "  MODE COMPLETE "), CL(1, " "), "", eNone, eBlink, eNone, 3000, True, ""
					vpmtimer.addtimer 3500, "AwardJackpot3'"
			End If
End Sub

Sub CheckSharkHardEasy2
			If Hard=1 Then 
					SharkModeComplete:CalloutActive=True:PlaySound "CO_NervousWet":CalloutTimer.Enabled=True
					DMD CL(0, "MODE COMPLETE "), CL(1, "BYE BYE TIGGER "), "", eNone, eBlink, eNone, 3000, True, ""
					vpmtimer.addtimer 6000, "AwardSuperJackpot2'"
			End If
			If Hard=0 Then 
					CalloutActive=True:PlaySound "CO_NervousWet":CalloutTimer.Enabled=True
					DMD CL(0, "  OOOOOO "), CL(1, "BYE BYE TIGGER "), "", eNone, eBlink, eNone, 3000, True, ""
					vpmtimer.addtimer 6000, "AwardSuperJackpot2'"
			End If
End Sub

Sub SharkModeComplete
		LightAward4.State=1
	If SharkComplete(CurrentPlayer)=False Then CheckModesCompleted: debug.print "CheckModesComplete-atSharkAttack":End If
		SharkComplete(CurrentPlayer)=True
End Sub
 


Dim CalloutActive
Sub CalloutTimer_Timer()
	CalloutActive=False
	debug.print "Callout Timer Activated" 
	CalloutTimer.Enabled=False
End Sub

' Tree animation
Dim MyPi3, TreeStep, TreeDir
MyPi3 = Round(4 * Atn(1), 6) / 90
TreeStep = 0

Sub PalmSway_Timer()
    TreeDir = SIN(TreeStep * MyPi3)
    TreeStep = (TreeStep + 1) MOD 360
    Palms4.RotY = - TreeDir
    Palms4Trunks.RotY = - TreeDir
    Palm1Leaves.RotY = + TreeDir
    Palm1Trunk.RotY = + TreeDir
End Sub

'VRSTUFF (Rawd)
'******************* VR Plunger **********************
Sub TimerPlunger_Timer
  If VR_Primary_plunger.Y < -71 then
      VR_Primary_plunger.Y = VR_Primary_plunger.Y + 3
  End If
End Sub

Sub TimerPlunger2_Timer
	VR_Primary_plunger.Y = -151.488 + (5* Plunger.Position) -20
End Sub
 '*** End VR Plunger **********************************

' VRClock Model cogs...
Sub NewClockTimer_timer()
ClockPiece1.roty = ClockPiece1.roty + 0.06
ClockPiece2.roty = ClockPiece2.roty - 0.16
ClockCylinder1.roty = ClockCylinder1.roty - 0.16
ClockPiece3.roty = ClockPiece3.roty - 0.16
ClockPiece4.roty = ClockPiece4.roty + 0.07
ClockCylinder2.roty = ClockCylinder2.roty + 0.1
End Sub

' ***************** VR Clock code below ******************
Dim CurrentMinute ' for VR clock 
Sub ClockTimer_Timer()
NewSecondsHand.RotAndTra2 = (Second(Now()))*6 - 270
NewMinutessHand.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6 -270
NewHoursHand.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2) - 270
CurrentMinute=Minute(Now())
End Sub
 ' ********************** END CLOCK CODE   *********************************

' ***** Beer Bubble Code - Rawd *****
Sub BeerTimer_Timer()

Randomize(21)
BeerBubble1.z = BeerBubble1.z + Rnd(1)*0.5
if BeerBubble1.z > -771 then BeerBubble1.z = -955
BeerBubble2.z = BeerBubble2.z + Rnd(1)*1
if BeerBubble2.z > -768 then BeerBubble2.z = -955
BeerBubble3.z = BeerBubble3.z + Rnd(1)*1
if BeerBubble3.z > -768 then BeerBubble3.z = -955
BeerBubble4.z = BeerBubble4.z + Rnd(1)*0.75
if BeerBubble4.z > -774 then BeerBubble4.z = -955
BeerBubble5.z = BeerBubble5.z + Rnd(1)*1
if BeerBubble5.z > -771 then BeerBubble5.z = -955
BeerBubble6.z = BeerBubble6.z + Rnd(1)*1
if BeerBubble6.z > -774 then BeerBubble6.z = -955
BeerBubble7.z = BeerBubble7.z + Rnd(1)*0.8
if BeerBubble7.z > -768 then BeerBubble7.z = -955
BeerBubble8.z = BeerBubble8.z + Rnd(1)*1
if BeerBubble8.z > -771 then BeerBubble8.z = -955
End Sub

' End VR Stuff........***********************************************************
'********************************************************************************


