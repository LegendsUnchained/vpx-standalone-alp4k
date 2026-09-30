' ****************************************************************
' mrjcrane WOOLY to SCOOBY retheme, 2022 for VISUAL PINBALL X 10.7
'                 Includes JP's Arcade Physics 3.0.1
' Table based on the VPX 2022 v1.1 of Wrath of Olympus by JP
' 1. No deviation of the ruleset or game play use WOOLY playguide
' 2. Added modular sound library and "variablied" sound names
' 3. Remastered audio track based on TV shows for each villain
' 4. Added additonal DOF testing capabilities and variable names
' 5. Soundtrack and image libraries are superimposed over WOOLY

' ****************************************************************
' NOTES REGARDING SOURCE TABLE USED 
' ****************************************************************
'             JP's Wrath of Olympus for VISUAL PINBALL X 10.7
'                 Including JP's Arcade Physics 3.0.1
' Table based on the VP9 prototype I made for T-800,
' I simply added a few of my ideas for the VPX version.
' So, this is not the Riot Pinball table, but an updated prototype.
' ****************************************************************

Option Explicit
Randomize

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1.7   ' standard ball mass in JP's VPX Physics 3.0.1
Const SongVolume = 0.3 ' 1 is full volume, but I set it quite low to listen better the other sounds since I use headphones, adjust to your setup :)

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
'or keep it False if you have a real DMD at 128x32 in size
' WOOLY Const FlexDMDHighQuality = False
Const FlexDMDHighQuality = True

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

' Define any Constants (Deployed V1.0 of Scooby with "Wooly" rom, transistioning to "Scooby" in version v1.0.1 - mrjcrane)
'Const cGameName = "Wooly"
Const cGameName = "Scooby" 'this is a place hold for future rom name for when an entry is made at the DOF Config site archive, for now use "Wooly"

'***** Additional DOF instructions
'DOF commands for Wooly = 
'Wooly,E101,E102,E103/E110,E104/E107/E113,E111/E117/E119 @t@/E124|E125 @t@,E106/E126|E127 @t@/E133|E134|E135|E136 @t@/E143 @t@,E132 m600 I28,E108 I20
'or
'Wooly,E101,E102,E103/E110,E104/E107/E113,E105/E111/E117/E119 @t@/E124|E125 @t@,E106/E126|E127 @t@/E133|E134|E135|E136 @t@/E143 @t@,E132 m600 I28,E108 I20

'DOF simplified command string, commnds for Scooby =
'Scooby,E101,E102,E103,E104,E105,E106,E132 m600 I28,E108 
'DOF Connection String for Sainsmart 8 channel control board
'Sainsmart Channels Bank 1: Channel-01 = Left Flipper	Channel-02 = Right Flipper	Channel-03 = Left Slingshot	Channel-04 = Right Slingshot
'Sainsmart Channels Bank 2: Channel-05 = Left PopBump	Channel-06 = Right PopBump	Channel-07 = Shaker Motor	Channel-08 = Knocker/Free Game

Const myVersion = "1.0.1"
Const MaxPlayers = 4          ' from 1 to 4
Const MaxMultiplier = 10      ' limit playfield multiplier
Const MaxBonusMultiplier = 10 'limit Bonus multiplier
'WOOLY Const BallsPerGame = 3        ' usually 3 or 5
Const BallsPerGame = 3        ' usually 3 or 5 'SCOOBY code for testing tables set @5
Const MaxMultiballs = 6       ' max number of balls during multiballs


' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If
'UseFlexDMD = True 'SCOOBY code to force FlexDMD on to override FLEX Dmd as primary even when in desktop mode.

' Define Global Variables
Dim BallSaverTime ' in seconds of the first ball and during the game
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim PlayfieldMultiplier(4)
Dim PFxSeconds
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot(4)
Dim SuperJackpot(4)
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue(4)
Dim SuperSkillshotValue(4)
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode
Dim x 'used in loops

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock(4)
Dim BallsInHole

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJackpot

'** SCOOBY SPECIAL TABLE MODES - SCOOBY GAME FLAGS (ON or OFF)
Dim sd_testtable	: sd_testtable			= "OFF"	'(ON OFF)
Dim sd_doftesting	: sd_doftesting			= "OFF"	'(ON OFF)
Dim sd_beermode		: sd_beermode			= "ON"  '(ON OFF)
Dim sd_jukebox		: sd_jukebox			= "ON"	'(ON OFF) INSERT KEY will start jukebox next song, DEL KEY will stop jukebox song
Dim sd_favsongnum	: sd_favsongnum			= 7 	'Myst Inc TV ShowMaps to F12 then DEL key will stop the song or game may change
Dim sd_favoritesong
    IF sd_favsongnum = 1 then sd_favoritesong = "mu_game_start_scooby"		'Computer game version of the intro song
    IF sd_favsongnum = 2 then sd_favoritesong = "sd_mu_song_tv_season1"		'Classic Scooby intro tv song season 1 1969
    IF sd_favsongnum = 3 then sd_favoritesong = "sd_mu_song_tv_season2"		'Classic Scooby intro tv song season 2 (English Accent Singer)
    IF sd_favsongnum = 4 then sd_favoritesong = "sd_mu_song_tv_movies"		'From the New Scooby Doo movies with 60's/70's celebrities (1972-1973)
    IF sd_favsongnum = 5 then sd_favoritesong = "sd_mu_song_tv_13ghosts"	'13 Ghosts tv series 1985 with Vincent VanGouhl
    IF sd_favsongnum = 6 then sd_favoritesong = "sd_mu_song_tv_whatsnew"	'What's new Scooby Doo tv series 2002-2006
    IF sd_favsongnum = 7 THEN sd_favoritesong = "sd_mu_song_tv_mystinc"		'Mystery Incorporated tv series 2010

Dim sd_introsong	: sd_introsong			= sd_favoritesong


'***** DOF DEVICE TRANSLATION TABLE ***** SCOOBY code added initialization and variables for DOF
'** Initial Configuration for 8 Channel Control board (Your DOF trigger codes may be different just fix them here)
'** Fold these variables into the table script when the DOF commands trigger.
Dim dof_off					: dof_off 				= 000
Dim dof_plunger				: dof_plunger 			= 000
Dim dof_autoplunger			: dof_autoplunger		= 141			'SCOOBY triggers right slingshot??
'** Primary DOF Devices 8 Channel Sainsmart Board
Dim dof_flipper_left 		: dof_flipper_left 		= 101
Dim dof_flipper_right		: dof_flipper_right		= 102
Dim dof_slingshot_left		: dof_slingshot_left 	= 103
Dim dof_slingshot_right		: dof_slingshot_right 	= 104
Dim dof_popbumper_left		: dof_popbumper_left	= 105
Dim dof_popbumper_right		: dof_popbumper_right 	= 106
Dim dof_shaker				: dof_shaker	 		= 132			'SCOOBY code Shaker motor (119/128/132) triggers on Mystery Machine Magnasave
Dim dof_knocker				: dof_knocker 			= 108			'SCOOBY used in FREE GAME award (107/108)
'** Additional DOF Triggers Physical Objects and useful variables
Dim dof_spinner_left		: dof_spinner_left 		= 000			'SCOOBY code making the call to the shaker motor
Dim dof_spinner_right		: dof_spinner_right 	= 000			'SCOOBY code making the call to the shaker motor
Dim dof_gear				: dof_gear				= 000
Dim dof_blower				: dof_blower			= 000
Dim dof_magnet				: dof_magnet			= 132
'** Additional DOF Triggers Games specific variables and alias names to trigger DOF activity
Dim dof_add_credit			: dof_add_credit		= 000
Dim dof_captive_ball		: dof_captive_ball		= 132 			'SCOOBY code captive ball triggers shaker motor pulse code 119
Dim dof_test_channel		: dof_test_channel		= 105			'SCOOBY code test problematic DOF Channels and Commands 105=LeftPopBumper

' ** Scooby Doo TV Episodes
'1) What the hex is going on (The Ghost of Elias Kingston)
'2) A Tiki Scare Is No Fair (Witch Doctor aka Mono Tiki Tia)
'3) A Clue for Scooby Doo (Sea Going Ghost aka Captain Cutler)
'4) Bedlam In The Big Top (Ghost Clown)
'5) A Night of Fright Is No Delight (Green Ghost, aka The Phantom Shadow)
'6) A Gaggle of Galloping Ghosts (Has the big Frankenstein Monster in purple clothes & Dracula).
'8) What a Night for a Knight (Black Knight)
'7) Which Witch is Which (Zombie episode)
'9) Go away ghost ship (Ghost of Red Beard the Pirate)

'The monsters/ghosts are played in this order:	Minotaur=Kingston, 	Hydra=Tiki, 		Cerberus=Cutler 	Medusa=Clown.
'The gods/ghosts are played in this order:		Ares=Green Ghost, 	Poseidon=Frankens,	Hades=Knight,		Zeus=Zombie.
'The special ModeStep							Monster=Monster		Gods=Ghosts			DemiGod=DemiGhost	GodMode=MegaGhost	GhostMasher

'***** S C O O B Y     S O U N D     L I B R A R Y    &    I N I T I A L     S O U N D S *****
Dim mu_game_start		: mu_game_start		= sd_favoritesong				'SCOOBY PS2 Computer Game Theme Song
Dim mu_theme			: mu_theme			= "mu_theme_scooby"				'SCOOBY Doo song computer version when game has started slow theme
Dim mu_theme_tv			: mu_theme_tv		= "mu_theme_scooby_tv"			'SCOOBY Doo main intro song from TV show
Dim mu_plunger			: mu_plunger		= "mu_plunger_scooby"			'SCOOBY It's a Mystery Song

'***** SCOOBY DOO MUSIC TRACKS *****
Dim sd_mu_mystinc_intro	: sd_mu_mystinc_intro	= "sd_mu_mystinc_intro"		'SCOOBY Theme song intro Mystery Inc 2020
Dim sd_mu_mystinc_extro	: sd_mu_mystinc_extro	= "sd_mu_mystinc_extro"		'SCOOBY Theme song extro Mystery Inc 2020
Dim sd_mu_multiball_song: sd_mu_multiball_song  = "sd_mu_mystinc_intro"		'SCOOBY Theme song intro Mystery Inc 2020
 
'***** VOICE TRACKS *****
Dim vo_game_start		: vo_game_start			= "vo_game_start_scooby"	'SCOOBY Drac says not welcome, Fred says found a mystery
Dim vo_game_over		: vo_game_over			= "sd_get_away_with_it"		'SCOOBY double check on this one !
Dim vo_live_again		: vo_live_again			= "vo_live_again_scooby"	'SCOOBY We saved you Scooby
Dim vo_kickbackislit 	: vo_kickbackislit		= "vo_kickbackislit_scooby"	'SCOOBY Shaggy, I'll give it to him again!
Dim vo_outlane			: vo_outlane			= "sd_outlane_sd"			'SCOOBY Redbeard will make a pirate stew out of you!
Dim vo_mystery			: vo_mystery			= "sd_pirates_chest_open"	'SCOOBY Fred, pirates chest let's open it up!
Dim vo_godmode			: vo_godmode			= "sd_scooby_snack"			'SCOOBY Velma, Would you do it for a scooby snack?
Dim vo_hurricane		: vo_hurricane			= "sd_hurricane"			'SCOOBY Hurricane in the Bermuda Triangle.
Dim vo_ball_drained		: vo_ball_drained		= "sd_redbeard_pirate_stew"
Dim sd_vo_hurricane		: sd_vo_hurricane		= "sd_vo_hurricane"

Dim sd_vo_giveyourcoins	: sd_vo_giveyourcoins 	= "sd_vo_giveyourcoins"		'SCOOBY Mummy says "coin coin". Shaggy "Here's a quarter" !!
Dim sd_vo_givecoins     : sd_vo_givecoins		= "sd_vo_givecoins"
Dim sd_vo_givequarters	: sd_vo_givequarters	= "sd_vo_givequarters"
Dim sd_vo_whats_in_here	: sd_vo_whats_in_here	= "sd_vo_whats_in_here"		'SCOOBY for captive ball hit.
Dim sd_vo_headforhills	: sd_vo_headforhills	= "sd_vo_headforhills"		'SCOOBY voice track for Right Ramp

'** TILT WARNINGS
Dim sd_tilt_warn		: sd_tilt_warn		= "sd_fx_tilt_warn"
Dim sd_tilt_tilt		: sd_tilt_tilt		= "sd_fx_tilt_tilt"
'
Dim vo_beast_master		: vo_beast_master	= "sd_vo_defeated_monsters"
Dim vo_god_of_gods      : vo_god_of_gods	= "sd_vo_defeated_ghosts"
'
Dim vo_chain_lightning	: vo_chain_lightning= "sd_vo_lightning_storm"			'SCOOBY To Do JAC convert Chain Lightening & music from WOOLY to "LIGHTENING STORM"
Dim vo_The_fates		: vo_The_fates		= "sd_vo_mystery_machine_started"	'SCOOBY "For the mystery award starting

'** OTHER VOCAL TRACKS **
Dim sd_dracula_notwelcome 	: sd_dracula_notwelcome	= "sd_dracula_notwelcome"

'***** VOICE TRACKS START MONSTERS & GHOSTS MODES *****'
Dim vo_Minotaur_mode	: vo_Minotaur_mode	= "vo_mode1_minotaur_kingston"		' Minotaur AKA Scooby Doo Elias Kingston
Dim vo_Hydra_mode		: vo_Hydra_mode		= "vo_mode2_hydra_monotiki"			' Minotaur AKA Scooby Doo Mono Tiki Tia 
Dim vo_Cerberus_mode	: vo_Cerberus_mode	= "vo_mode3_cerberus_cutler"		' Minotaur AKA Scooby Doo Captain Cutler
Dim vo_Medusa_mode		: vo_Medusa_mode	= "vo_mode4_medusa_ghostclown"		' Minotaur AKA Scooby Doo The Ghost Clown
Dim vo_Ares_mode		: vo_Ares_mode		= "vo_mode5_ares_greenghost"		' Minotaur AKA Scooby Doo The Green Ghost
Dim vo_Poseidon_mode	: vo_Poseidon_mode	= "vo_mode6_poseidon_frankenstein"	' Minotaur AKA Scooby Doo Frankenstein
Dim vo_Hades_mode		: vo_Hades_mode		= "vo_mode7_hades_blackknight"		' Minotaur AKA Scooby Doo The Black Knight
Dim vo_Zeus_mode		: vo_Zeus_mode		= "vo_mode8_zeus_zombie"			' Minotaur AKA Scooby Doo The Zombie
Dim vo_DemiGod_mode

'** SKILLSHOTS (3)
Dim vo_skillshot			: vo_skillshot			= "sd_skillshot1"		'SCOOBY, Good Boy Scooby !!!
Dim vo_superskillshot		: vo_superskillshot		= "sd_skillshot2"		'SCOOBY, Way to go Scooby !!!
Dim vo_freakyskillshot		: vo_freakyskillshot	= "sd_skillshot3"		'SCOOBY, Way to go Scooby the full gang !!!
Dim sd_vo_skillshot			: sd_vo_skillshot		= "sd_skillshot1"		'SCOOBY, Good Boy Scooby !!!
Dim sd_vo_superskillshot	: sd_vo_superskillshot	= "sd_skillshot2"		'SCOOBY, Way to go Scooby !!!
Dim sd_vo_freakyskillshot	: sd_vo_freakyskillshot	= "sd_skillshot3"		'SCOOBY, Way to go Scooby the full gang !!!

'** COMBOS (7)
Dim sd_vo_combo1x		: sd_vo_combo1x 	= "sd_vo_combo1x"
Dim sd_vo_combo2x		: sd_vo_combo2x 	= "sd_vo_combo2x"
Dim sd_vo_combo3x		: sd_vo_combo3x 	= "sd_vo_combo3x"
Dim sd_vo_combo4x		: sd_vo_combo4x 	= "sd_vo_combo4x"	
Dim sd_vo_combo5x		: sd_vo_combo5x 	= "sd_vo_combo5x"
Dim sd_vo_combo6x		: sd_vo_combo6x 	= "sd_vo_combo6x"
Dim sd_vo_combosuper	: sd_vo_combosuper	= "sd_vo_combosuper"
'** Additional phrases used in combo mode (1-5)
Dim sd_super_sleuth			: sd_super_sleuth		= "sd_vo_fred_supersleuth"			'SCOOBY cde used for Captive Ball 25 Hits (Ball Breaker)
Dim sd_vo_jinkies			: sd_vo_jinkies 		= "sd_vo_jinkies" 					'SCOOBY code used for 5X Combo (Combo King)
Dim sd_vo_zoinks			: sd_vo_zoinks 			= "sd_vo_zoinks3" 					'SCOOBY code used for 6X Combo (Wind Rider)
'** MAHERS & WIZARDS
Dim sd_vo_masher_monster	: sd_vo_masher_monster		= "sd_vo_mashermonster"		'SCOOBY code used for (WOOLY formerly Beast Master)
Dim sd_vo_masher_ghost		: sd_vo_masher_ghost		= "sd_vo_masherghost"		'SCOOBY code used for (WOOLY formerly God of Gods)
Dim sd_vo_wiz_apprentice	: sd_vo_wiz_apprentice		= "sd_vo_wiz_apprentice" 	'SCOOBY mod v1.0.1 Demi-God ===> Scooby Apprentice
Dim sd_vo_wiz_master		: sd_vo_wiz_master			= "sd_vo_wiz_master"		'SCOOBY mod v1.0.1 God Mode ===> Scooby Master Wizard		
'** MYSRERY MACHINE
Dim sd_vo_mysterymachine	: sd_vo_mysterymachine  = "sd_vo_mystery_machine_started" 	'SCOOBY code used for (The Fates)

'** BALL LOCKS AND AUTO LAUNCH BATTLE HELMET **
Dim vo_lock0			: vo_lock0				= "sd_scooby_laugh1"
Dim vo_lock1			: vo_lock1 				= "sd_ball_launch_fire1"
Dim vo_lock2			: vo_lock2 				= "sd_ball_launch_fire2"
Dim vo_lock3			: vo_lock3				= "sd_ball_launch_fire3"

'** MULTIBALL MODES ** (1*Minotaur, 2*Medusa, 3*Zeus, 4*Pandora & *5Wizard Mode) 
Dim vo_minotaur_multiball 			: vo_minotaur_multiball				= "sd_vo_multiball_kingston" 		'SCOOBY code v1.0.1
Dim sd_multiball					: sd_multiball						= "sd_vo_multiball"
Dim sd_vo_mystery_machine_multiball : sd_vo_mystery_machine_multiball	= "sd_vo_mystery_machine_multiball"
Dim vo_medusa_multiball				: vo_medusa_multiball				= "sd_vo_multiball_ghostclown"		'SCOOBY code v1.0.1
Dim sd_vo_clown_multiball			: sd_vo_clown_multiball				= "vo_medusa_multiball"
Dim vo_zeus_multiball				: vo_zeus_multiball					= "sd_vo_zombie_multiball"
Dim sd_vo_zombie_multiball			: sd_vo_zombie_multiball			= "sd_vo_zombie_multiball"

Dim sd_vo_pirate_multiball			: sd_vo_pirate_multiball			= "sd_vo_pirate_multiball"

'** AWARD SHOTS (Pandora's Box / Pirates Treasure Chest)
'1 Million
'5 Million
Dim sd_vo_won_extraball			: sd_vo_won_extraball 			= "sd_vo_won_extraball"
Dim sd_vo_won_extragame			: sd_vo_won_extragame			= "sd_vo_won_extragame"
Dim sd_award_1_million			: sd_award_1_million 			= "sd_award_1_million"
Dim sd_award_bonus				: sd_award_bonus				= "sd_award_bonus"
Dim sd_award_bumper_multiplier	: sd_award_bumper_multiplier 	= "sd_award_bumper_multiplier"
Dim sd_award_points				: sd_award_points				= "sd_award_points"

'** SPECIAL TIMERS
Dim sd_ghost_mode_start	: sd_ghost_mode_start	= "sd_get_to_mansion"
Dim sd_time_running_out	: sd_time_running_out	= "sd_shaggy_lets_get_out"

'** CAPTURE MONSTERS & GHOSTS SOUNDTRACK WIN LOOSE
Dim sd_mode_win			: sd_mode_win				= " "				  'SCOOBY mod v1.0.1 leaving sound clip blank for now.
Dim sd_mode_winner		: sd_mode_winner			= "sd_vo_mode_winner" 'SCOOBY mod v1.0.1 added WINNER DMD and Audio code		
Dim sd_mode_loose		: sd_mode_loose				= "sd_mystery_closed" 'SCOOBY code Shaggy says mystery is closed
Dim sd_mode_looser		: sd_mode_looser			= "sd_vo_mode_looser" 'SCOOBY mod v1.0.1 added LOOSER DMD and Audio code
Dim sd_time_is_up		: sd_time_is_up				= "sd_wha_wha_wha"

'** Monsters and Ghosts Captured added in SCOOBY v1.0.1
Dim sd_vo_captured_kingston		: sd_vo_captured_kingston		= "sd_vo_captured_kingston"
Dim sd_vo_captured_monotiki		: sd_vo_captured_monotiki		= "sd_vo_captured_monotiki"
Dim sd_vo_captured_cutler		: sd_vo_captured_cutler			= "sd_vo_captured_cutler"
Dim sd_vo_captured_ghostclown	: sd_vo_captured_ghostclown		= "sd_vo_captured_ghostclown"
Dim sd_vo_captured_frankenstein	: sd_vo_captured_frankenstein	= "sd_vo_captured_frankenstein"
Dim sd_vo_captured_blackknight	: sd_vo_captured_blackknight	= "sd_vo_captured_blackknight"
Dim sd_vo_captured_greenghost	: sd_vo_captured_greenghost		= "sd_vo_captured_greenghost"
Dim sd_vo_captured_zombie		: sd_vo_captured_zombie			= "sd_vo_captured_zombie"

'** BATTLE SOUNDS
Dim sd_scoop2_hit			: sd_scoop2_hit 		= "sd_skillshot"
Dim vo_og_growl				: vo_og_growl			= "sd_fx_batsfly1" 			'SCOOBY Bats flying out
Dim fx_hole_enter			: fx_hole_enter 		= "fx_hole_enter"			'WOOLY normal default sound
Dim fx_hole_enter_scooby	: fx_hole_enter_scooby 	= "sd_fx_batsfly1"			'SCOOBY Center Lane multi hit

Dim fx_hole_enter_battle	: fx_hole_enter_battle 	= "sd_secret_passage_shaggy"'SCOOBY Secret passage Shaggy from center scoop2
Dim fx_hole_enter_right		: fx_hole_enter_right	= "sd_secret_passage_velma"	'SCOOBY Secret passage Velma (from ball launch)
Dim sd_secret_passage1		: sd_secret_passage1 	= "sd_secret_passage_shaggy"'SCOOBY Secret passage Shaggy from center scoop2
Dim sd_secret_passage2		: sd_secret_passage2	= "sd_secret_passage_velma"	'SCOOBY Secret passage Velma (see TrapdoorUp)
Dim sd_secret_passage3		: sd_secret_passage3	= "sd_secret_passage3"		'SCOOBY Secret passage Velma (see TrapdoorUp)
Dim sd_secret_passage4		: sd_secret_passage4	= "sd_secret_passage4"		'SCOOBY Secret passage Velma (see TrapdoorUp)

Dim sd_scoop2_enter         : sd_scoop2_enter 		= " "
Dim fx_hole_enter_pandora	: fx_hole_enter_pandora	= "sd_pirates_chest"		'SCOOBY Center Lane multi hit

'** OTHER FX SOUNDS **
Dim sd_fx_collide				: sd_fx_collide				= "sd_vo_whats_in_here"		'SCOOBY Daphanie askes "Whats in here"
Dim sd_ball_launch				: sd_ball_launch			= " "						'SCOOBY Auto ball relaunch on ball save via Fred
Dim sd_fx_trapdoor_up			: sd_fx_trapdoor_up			= "sd_fx_drawbridge_open"
Dim sd_fx_trapdoor_down			: sd_fx_trapdoor_down		= "sd_fx_drawbridge_close"
Dim sd_fx_drawbridge_open		: sd_fx_drawbridge_open		= "sd_fx_drawbridge_open"
Dim sd_fx_drawbridge_close		: sd_fx_drawbridge_close	= "sd_fx_drawbridge_close"
Dim sd_lower_iron_door			: sd_lower_iron_door		= "sd_lower_iron_door"
Dim sd_fx_wolfhowl1				: sd_fx_wolfhowl1			= "sd_fx_wolfhowl1"
Dim sd_fx_bats_fly				: sd_fx_bats_fly			= "sd_fx_batsfly1"
Dim sd_fx_spinner				: sd_fx_spinner				= "sd_fx_spinner_tone"
Dim sd_fx_magnasave				: sd_fx_magnasave			= "sd_fx_magnasave"
Dim sd_fx_spinner_left			: sd_fx_spinner_left		= "fx_spinner_tone_chirplow"
Dim sd_fx_spinner_right			: sd_fx_spinner_right		= "fx_spinner_tone_chirphigh"
Dim sd_fx_squeaky_door_open 	: sd_fx_squeaky_door_open 	= "sd_fx_squeaky_door_open"
Dim sd_fx_squeaky_door_close 	: sd_fx_squeaky_door_close 	= "sd_fx_squeaky_door_close"
Dim sd_fx_captiveball			: sd_fx_captiveball			= "sd_vo_whats_in_here"
Dim sd_fx_thunder				: sd_fx_thunder				= "sfx_lightning"			'Special note on Scooby this is a random index sound

'** RAMPS 'Note, thanks JP, the parrot was a very nice touch !! Squawk Squwk !!
Dim sd_ramp_sound_left			: sd_ramp_sound_left		= "sd_fx_parrot"
Dim sd_ramp_sound_right			: sd_ramp_sound_right		= "sd_vo_headforhills"
Dim sd_ramp_sound_top			: sd_ramp_sound_top		    = "sd_fx_monster1"
Dim sd_fx_parrot				: sd_fx_parrot				= "sd_fx_parrot"

'Randomizer Function Possible Options for random phrases        
'JAC Play It's Redbeard when it goes into the secrete hidden hole then another Redbeard sound when the ball exits.
'JAC Play Dracula sound 2 in the center and Dracula sound 3 when entering the center scoop.

'** WOOLY MONSTER & GOD SONGS (Remapped to MONSTERS & GHOSTS for Scooby Doo sounds and songs) v1.0.1
Dim mu_minotaur		: mu_minotaur 	= "sd_mu_spooky1"	  	' MINOTAUR 	- AKA Scooby Doo Elias Kingston
Dim mu_hydra		: mu_hydra		= "sd_mu_spooky1"		' HYDRA		- AKA Mono-Tikki-Tia Witch Doctor
Dim mu_cerberus		: mu_cerberus 	= "sd_mu_spooky1"  		' CERBERUS	- AKA Scooby Doo Captain Cutler
Dim mu_medusa		: mu_medusa 	= "sd_mu_spooky1"		' MEDUSA	- AKA Scooby Doo Ghost Clown
'SCOOBY To Do v1.0.1 add different jam for God/Ghost battle mode.
Dim mu_ares			: mu_ares		= "sd_mu_battlemode"							' Aeres
Dim mu_poseidon		: mu_poseidon	= "sd_mu_battlemode"							' Poseidon
Dim mu_hades		: mu_hades		= "sd_mu_battlemode"							' Hades
Dim mu_zeus			: mu_zeus		= "sd_mu_battlemode"							' Zeus
'Customization available here to insert a different song when in multiball, for now I'll use the WOOLY default rock jam
Dim mu_multiball	: mu_multiball	= "mu_multiball"								' God or Demi-God mode -

		' ** ORIGINAL SONGS FROM WOOLY **
        'Case 1:PlaySong "mu_minotaur"  ' Minotaur
        'Case 2:PlaySong "mu_hydra"     ' Hydra
        'Case 3:PlaySong "mu_cerberus"	' Cerberus
        'Case 4:PlaySong "mu_medusa"	' Medusa
        'Case 5:PlaySong "mu_ares"		' Ares
        'Case 6:PlaySong "mu_poseidon"	' Poseidon
        'Case 7:PlaySong "mu_hades"		' Hades
        'Case 8:PlaySong "mu_zeus"		' Zeus
        'Case 9:PlaySong "mu_multiball"	' God or Demi-God mode -

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs
Dim mMagnet
Dim cbLeft    'captive ball at the magnet

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
    Randomize

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 45 ' Plunger Power
    Const IMTime = 0.5        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        'WOOLY.InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .InitExitSnd SoundFXDOF("fx_kicker", dof_autoplunger, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", dof_autoplunger, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Magnet
    Set mMagnet = New cvpmMagnet
    With mMagnet
        .InitMagnet Magnet, 35
        .GrabCenter = True
        .CreateEvents "mMagnet"
    End With

    Set cbLeft = New cvpmCaptiveBall
    With cbLeft
        .InitCaptive CapTrigger, CapWall, CapKicker, 0
        .ForceTrans = .7
        .MinForce = 3.5
        .CreateEvents "cbLeft"
        .Start
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Credits = 0
    Loadhs

    ' Initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = True 'we want coins

    if bFreePlay Then DOF 121, DOFOn

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    PFxSeconds = 0
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bBonusHeld = False
    bJackpot = False
    bInstantInfo = False
    ' set any lights for the attract mode
    GiOff
    StartAttractMode

    ' Start the RealTime timer
    RealTime.Enabled = 1

    ' Load table color
    LoadLut
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

'** SCOOBY KEYS DEBUG AND TESTING SECTION: table debuggers test for different modes
    IF sd_beermode = "ON" THEN
       If keycode = "14"  Then sd_KeysFlippersUp   		  'SCOOBY mod v1.0.1 "BACKSPACE KEY" = Hold flippers in UP position cradle the ball pause game
       If keycode = "14"  Then PlaySound "sd_vo_beermode" 'SCOOBY mod v1.0.1 Beer Mode Active, go grab another beer!
	   if keycode = "14"  Then DMD "","","d_sd_flippers", eNone, eNone, eNone, 1500, True, ""
       'If keycode = "197" Then sd_KeysFlippersUp   		  'SCOOBY mod v1.0.1 "PAUSE KEY" = Beer Mode Activate Both Flippers Up, hold ball, go grab another beer!
       'If keycode = "197" Then PlaySound "sd_vo_beermode" 'SCOOBY mod v1.0.1 Beer Mode Active, go grab another beer!
	   'If keycode = "70"  Then sd_KeysFlippersDown        'SCOOBY mod v1.0.1 "SCROLL LOCK KEY" = Beer Mode Release: to release the dual-flippers being held ON	
	  'If Keycode = "207" Then sd_DoNothing
    END IF

'** Jukebox Music Players
    IF sd_jukebox = "ON" THEN
	   If keycode = "210" Then sd_JukeboxStart 		'SCOOBY mod v1.0.1 	"INSERT KEY" play jukebox scooby songs, hit again for next song, "DEL" key stops the song
	   If keycode = "211" Then sd_JukeboxStop		'SCOOBY mod v1.0.1 	"DEL KEY" will stop the jukebox song takes a few seconds, OR game may stop the song on mode chagne
	   If keycode = "88"  Then sd_PlayFavoriteSong	'SCOOBY mod v1.0.1	"F12" KEY
    END IF

'** DOF Key Cod & Channel testing ' SCOOBY code for testing Sainsmart 8 control board
	IF sd_doftesting = "ON" THEN
		If keycode = "59" Then DOF dof_flipper_left, 	DOFOn	'F1
		If keycode = "60" Then DOF dof_flipper_right, 	DOFOn	'F2
		If keycode = "61" Then DOF dof_slingshot_left, 	DOFOn	'F3
		If keycode = "62" Then DOF dof_slingshot_right, DOFOn	'F4
		If keycode = "63" Then DOF dof_popbumper_left, 	DOFOn	'F5
		If keycode = "64" Then DOF dof_popbumper_right, DOFOn	'F6
		If keycode = "65" Then DOF dof_shaker, 			DOFPulse'F7
		If keycode = "66" Then DOF dof_knocker, 		DOFOn	'F8

		dof_test_channel = dof_knocker
	    If keycode = "67" Then DOF dof_test_channel, 	DOFPulse	'F9
    'Else sd_doftesting = "Off" 'ELSE is probably not necessary here
    End If

    IF sd_testtable = "ON" THEN
       	'** GAME
		'** DEBUG Special Awards, Jackpots, Skillshots, Combos.
        sd_DoNothing
	END IF

		'** Mystery Machine
			If keycode = "56"	Then
				bLutActive = True
				windshield_light.State = 2
				DOF dof_magnet, DOFPulse 
				PlaySound "sd_fx_magna_save" 
				PlaySound "sd_fx_vanhonk1"  
				PlaySound "sd_fx_vanstart"
			End If

			If Keycode = "00" 	Then sd_DoNothing
    
'** NORMAL WOOLY CODE
    If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25

    'SCOOBY special game mod added illumination to the windshield of the Mystery Machine when the Magna Save button is pressed.
	'03/31/2021: Notes from mrjcrane, if you think this section is overkill you are free to deactivate any of the commands below.
    IF keycode = LeftMagnaSave THEN
			bLutActive = True
			windshield_light.State = 2
			DOF dof_magnet, DOFPulse 
			PlaySound "sd_fx_magna_save" 
			PlaySound "sd_fx_vanhonk1"  
			PlaySound "sd_fx_vanstart"
	END IF

     '03/31/2022-mrjcrane: place holder to switch magnasave to right side in future release
     If keycode = RightMagnaSave Then
        'PlaySound "sd_fx_magna_save" : PlaySound "sd_fx_vanhonk1" : PlaySound "sd_fx_vanstart1" : windshield_light.State = 2 : DOF dof_magnet, DOFPulse 
        If bLutActive Then
            NextLUT
        End If
    End If

    If Keycode = AddCreditKey Then
        If Credits < 99 Then Credits = Credits + 1
        if bFreePlay = False Then DOF 121, DOFOn
        If(Tilted = False)Then
            DMDFlush
            DMD "", CL("CREDITS " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
            PlaySound sd_vo_givequarters 	'SCOOBY code speical mod when adding coins to the game Mummy is not happy !!
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then CheckTilt 'only check the tilt during game
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 1
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 0
        'WOOLY If keycode = LeftMagnaSave Then mMagnet.MagnetOn = True:DOF 132, DOFOn
        If keycode = LeftMagnaSave Then mMagnet.MagnetOn = True:DOF dof_magnet, DOFOn 'SCOOBY code for DOF Shaker Motor
        If keycode = RightMagnaSave Then kickBallOut 'sometimes all the balls don't come out od the scoop (!?)

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                        If Credits < 1 And bFreePlay = False Then DOF 121, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, "vo_givemeyourmoney"
                            PlaySound sd_vo_givecoins
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True)Then
                    If(BallsOnPlayfield = 0)Then
                        ResetForNewGame()
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            If Credits < 1 And bFreePlay = False Then DOF 121, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, "vo_givemeyourmoney"
                        PlaySound sd_vo_givecoins
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = LeftMagnaSave Then bLutActive = False
    If keycode = LeftMagnaSave OR keycode = RightMagnaSave Then ReleaseMagnetBalls
    If keycode = LeftMagnaSave Then windshield_light.State = 0 'SCOOBY code turn off windshield light in Mystery Machine
    If keycode = LeftMagnaSave Then DOF dof_magnet, DOFOff 'SCOOBY code shut off the Shaker motor when key is released

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
    End If

'** DOF Key Code & Channel testing ' SCOOBY code for testing Sainsmart 8 control board
	IF sd_doftesting = "ON" THEN
		If keycode = "59" Then DOF dof_flipper_left, 	DOFOff	'F1
		If keycode = "60" Then DOF dof_flipper_right, 	DOFOff	'F2
		If keycode = "61" Then DOF dof_slingshot_left, 	DOFOff	'F3
		If keycode = "62" Then DOF dof_slingshot_right, DOFOff	'F4
		If keycode = "63" Then DOF dof_popbumper_left, 	DOFOff	'F5
		If keycode = "64" Then DOF dof_popbumper_right, DOFOff	'F6
		If keycode = "65" Then DOF dof_shaker, 			DOFOff	'F7
		If keycode = "66" Then DOF dof_knocker, 		DOFOff	'F8

        If keycode = "67" Then DOF dof_test_channel,	DOFOff	'F9
    END IF

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
    End If
End Sub

Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = False
    If NOT hsbModeActive Then
        bInstantInfo = True
        DMDFlush
        InstantInfo
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
    If UseFlexDMD Then FlexDMD.Run = False
    If B2SOn = true Then Controller.Stop
End Sub

'********************
'     Flippers & DOF
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        'WOOLY PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        PlaySoundAt SoundFXDOF("fx_flipperup", dof_flipper_left, DOFOn, DOFFlippers), LeftFlipper 'SCOOBY code DOF
        LeftFlipper.EOSTorque = 0.75:LeftFlipper.RotateToEnd
        LeftFlipper2.EOSTorque = 0.75:LeftFlipper2.RotateToEnd
        LeftFlipper001.EOSTorque = 0.75:LeftFlipper001.RotateToEnd
    Else
        'WOOLY PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        PlaySoundAt SoundFXDOF("fx_flipperdown", dof_flipper_left, DOFOff, DOFFlippers), LeftFlipper 'SCOOBY code DOF
        LeftFlipper.EOSTorque = 0.2:LeftFlipper.RotateToStart
        LeftFlipper2.EOSTorque = 0.2:LeftFlipper2.RotateToStart
        LeftFlipper001.EOSTorque = 0.2:LeftFlipper001.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        'WOOLY PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        PlaySoundAt SoundFXDOF("fx_flipperup", dof_flipper_right, DOFOn, DOFFlippers), RightFlipper 'SCOOBY code DOF
        RightFlipper.EOSTorque = 0.75:RightFlipper.RotateToEnd
        RightFlipper2.EOSTorque = 0.75:RightFlipper2.RotateToEnd
        RightFlipper001.EOSTorque = 0.75:RightFlipper001.RotateToEnd
    Else
        'WOOLY PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        PlaySoundAt SoundFXDOF("fx_flipperdown", dof_flipper_right, DOFOff, DOFFlippers), RightFlipper 'SCOOBY code DOF
        RightFlipper.EOSTorque = 0.2:RightFlipper.RotateToStart
        RightFlipper2.EOSTorque = 0.2:RightFlipper2.RotateToStart
        RightFlipper001.EOSTorque = 0.2:RightFlipper001.RotateToStart
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper2_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper001_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper001_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper2_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RotateLaneLights(n) 'n is the direction, 1 = left or 0 = right
    Dim tmp
    If bRotateLights Then
        If n = 1 Then
            tmp = li002.State
            li002.State = li003.State
            li003.State = li004.State
            li004.State = li005.State
            li005.State = tmp
        Else
            tmp = li005.State
            li005.State = li004.State
            li004.State = li003.State
            li003.State = li002.State
            li002.State = tmp
        End If
    End If
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt 'Called when table is nudged
    Dim BOT
    BOT = GetBalls
    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub
    Tilt = Tilt + TiltSensitivity                 'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt > TiltSensitivity)AND(Tilt <= 15)Then 'show a warning
        'WOOLY DMD "_", CL("CAREFUL"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
        DMD CL("TILT WARNING"), CL("BE CAREFUL"), "d_sd_warning", eBlinkFast, eBlinkFast, eNone, 2500, True, "" 'Scooby code v1.0.1 added/activated sound
        PlaySound sd_tilt_warn
		DOF dof_shaker, DOFPulse 'SCOOBY code v1.0.1 pulse the shake motor shake table before tilting v
    End if
    If(NOT Tilted)AND Tilt > 15 Then 'If more that 15 then TILT the table
        'display Tilt
        InstantInfoTimer.Enabled = False
        DMDFlush
        'WOOLY DMD CL("YOU"), CL("TILTED"), "", eNone, eNone, eNone, 200, False, ""
        DMD CL("YOU TILTED"), CL("THE TABLE"), "d_sd_tilted", eBlinkFast, eBlinkFast, eNone, 3000, False, ""
        PlaySound sd_tilt_tilt 'Scooby code v1.0.1 added/activated sound
        DOF_FireAllChannels 'SCOOBY code v1.0.1 added to make it loud when table is TILTED, you were warned !!!
        'WOOLY PlaySound "vo_yousuck" &RndNbr(5)
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
        bMultiBallMode = False           'normally disabled in the drain sub
        StopMBmodes
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        Tilted = True
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        LeftFlipper001.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        Tilted = False
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper1.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        vpmtimer.Addtimer 2000, "EndOfBall() '"
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'*****************************************
'         Music as wav sounds
' in VPX 10.7 you may use also mp3 or ogg
'*****************************************

Dim Song
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

Sub ChangeSong
    Select Case Mode(CurrentPlayer, 0)
        Case 0:
            iF bMultiBallMode OR bMinotaurMBStarted Then
                 'WOOLY PlaySong "mu_multiball"
                  sd_PlayMultiballMusic 'SCOOBY code go to music subroutine Play Mystery Inc. Short Version & TV Show Song
            Else
                'WOOLY PlaySong "mu_theme"
                PlaySong mu_theme 'SCOOBY sound mod convert to variable
            End If
        'WOOLY Case 1:PlaySong "mu_minotaur"  ' Minotaur
        'WOOLY Case 2:PlaySong "mu_hydra"     ' Hydra
        'WOOLY Case 3:PlaySong "mu_cerberus"  ' Cerberus
        'WOOLY Case 4:PlaySong "mu_medusa"    ' Medusa
        'WOOLY Case 5:PlaySong "mu_ares"      ' Ares
        'WOOLY Case 6:PlaySong "mu_poseidon"  ' Poseidon
        'WOOLY Case 7:PlaySong "mu_hades"     ' Hades
        'WOOLY Case 8:PlaySong "mu_zeus"      ' Zeus
        'WOOLY Case 9:PlaySong "mu_multiball" ' God or Demi-God mode -

		'SCOOBY code swap in alternate music tracks from actual Scooby episodes for each ghost
        Case 1:PlaySong mu_minotaur		' Minotaur AKA Scooby Doo Elias Kingston
        Case 2:PlaySong mu_hydra		' Hydra
        Case 3:PlaySong mu_cerberus		' Cerberus
        Case 4:PlaySong mu_medusa		' Medusa
        Case 5:PlaySong mu_ares			' Ares
        Case 6:PlaySong mu_poseidon		' Poseidon
        Case 7:PlaySong mu_hades		' Hades
        Case 8:PlaySong mu_zeus			' Zeus
        Case 9:PlaySong mu_multiball	' God or Demi-God mode -
    End Select
End Sub

Sub StopSong(name)
    StopSound name
End Sub

'******************************
' Play random quotes & sounds
'******************************

Sub PlayThunder
    PlaySound "sfx_thunder" &RndNbr(9)
End Sub

Sub PlayLightning
    PlaySound "sfx_lightning" &RndNbr(6)
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

Sub ChangeGIIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGILights
        bulb.IntensityScale = factor
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 0 Then '-1 means no balls, 0 is the first captive ball, 1 is the second captive ball...)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    PlaySoundAt "fx_GiOn", li008 'about the center of the table
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    PlaySoundAt "fx_GiOff", li008 'about the center of the table
    DOF 118, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 40
            LightSeqGi.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqGi.UpdateInterval = 25
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 20
            LightSeqGi.Play SeqBlinking, , 10, 10
        Case 4 'seq up
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqUpOn, 25, 3
        Case 5 'seq down
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqDownOn, 25, 3
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 40
            LightSeqInserts.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqInserts.UpdateInterval = 25
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 20
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 4 'center
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqCircleOutOn, 15, 2
        Case 5 'top down
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 15, 1
        Case 6 'down to top
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 15, 1
    End Select
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v3.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
	'JAC bug found a divide by zero problem here on next line if/when dim sound clip <> sound clip name
    tmp = ball.x * 2 / TableWidth-1
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
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10) '04/09/2022 mrjcrane: strange table crash on this line don't know the cause yet 
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.4, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls, 20 balls, from 0 to 19
Const lob = 1     'number of locked balls
Const maxvel = 40 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z -24

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'************************************
' Diverse Collection Hit Sounds v3.0
'************************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_MetalWire":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_LongBands_Hit(idx):PlaySoundAtBall "fx_rubber_longband":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aRubber_Pegs_Hit(idx):PlaySoundAtBall "fx_rubber_peg":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub
Sub aCBallsHit_Hit(idx):PlaySoundAt "fx_collide", CapKicker: PlaySound sd_fx_collide: End Sub 'just the sound of the ball hitting the captive ball

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
        PlayfieldMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    ' initialise any other flags
    Tilt = 0

    ' initialise specific Game variables
    Game_Init()

    ' you may wish to start some music, play a sound, do whatever at this point

    vpmtimer.addtimer 1500, "FirstBall '"
    'Adding test timer for SCOOBY next
    vpmtimer.addtimer 1000, "EndModeTimer_Timer '" 'Scooby code for sequence testng of audio clips timer testing 
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
    'SCOOBY optional sound on first ball: PlaySound "sd_dracula_notwelcome" 'SCOOBY code Dracula says you not welcome when game starts
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    DMDScoreNow

    ' set the current players bonus multiplier back down to 1X
    SetBonusMultiplier 1

    ' set the playfield multiplier
    SetPlayfieldMultiplier 1

    ' reset any drop targets, lights, game Mode etc..

    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False

    'Reset any table specific
    ResetNewBallVariables

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    bSkillShotReady = True

'Change the music ?
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 107, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4

' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield > 1 Then
        DOF 131, DOFPulse
        bMultiBallMode = True
        bAutoPlunger = True
    End If
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
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 10 'yes 10 points :)
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.
    '(the tilt recovery mechanism will handle any extra balls or end of game)

    If NOT Tilted Then
        'WOOLY PlaySong "mu_plunger"
         PlaySong mu_plunger 'SCOOBY sound mod "It's a mystery" Play slow repeating song
        'Count the bonus. This table uses several bonus
        DMD CL("BONUS"), "", "", eNone, eNone, eNone, 750, True, ""

        'Targets Hit x 3,500
        AwardPoints = BonusTargets(CurrentPlayer) * 3500
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("TARGETS HIT " & BonusTargets(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""

        'Ramps Hit x 9,000
        AwardPoints = BonusRamps(CurrentPlayer) * 9000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("RAMPS HIT " & BonusRamps(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""

        'Orbits Hit x 12,500
        AwardPoints = BonusOrbits(CurrentPlayer) * 12500
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("ORBITS HIT " & BonusOrbits(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""

        'Trees collected x 12,500
        AwardPoints = TreeHits(CurrentPlayer) * 12500
        TotalBonus = TotalBonus + AwardPoints
        'WOOLY DMD CL("TREES COLLECTED " & TreeHits(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""
        DMD CL("COINS COLLECTED " & TreeHits(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""

        'Hidden Shots x 12,500
        AwardPoints = HiddenShots(CurrentPlayer) * 12500
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("HIDDEN SHOTS " & HiddenShots(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""

        'Combo Hits x 25,000
        AwardPoints = ComboHits(CurrentPlayer) * 25000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("COMBO HITS " & ComboHits(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""

        'X Hits	x 50,000
        AwardPoints = BonusXHits(CurrentPlayer) * 50000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("X HITS " & BonusXHits(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""

        'Monsters defeated x 300,000
        AwardPoints = TotalMonsters(CurrentPlayer) * 300000
        TotalBonus = TotalBonus + AwardPoints
        'WOOLY DMD CL("MONSTERS DEFEATED " & TotalMonsters(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""
        DMD CL("MONSTERS CAPTURED " & TotalMonsters(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, "" 'SCOOBY code dmd

        'Gods defeated x 300,000
        AwardPoints = TotalGods(CurrentPlayer) * 300000
        TotalBonus = TotalBonus + AwardPoints
        'WOOLY DMD CL("GODS DEFEATED " & TotalGods(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""
        DMD CL("GHOSTS CAPTURED " & TotalGods(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, "" 'SCOOBY code dmd

        DMD CL("BONUS X MULTIPLIER"), CL(FormatScore(TotalBonus) & " X " & BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 1500, True, ""
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        DMD CL("TOTAL BONUS"), CL(FormatScore(TotalBonus)), "", eNone, eNone, eNone, 2000, True, ""
        AddScore2 TotalBonus

        'SCOOBY add Redbeard threatening Players

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 11000, "EndOfBall2 '"
    Else 'if tilted then only add a short delay and move to the 2nd part of the end of the ball
        vpmtimer.addtimer 200, "EndOfBall2 '"
    End If
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If ExtraBallsAwards(CurrentPlayer) > 0 Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if no more EB's then turn off any Extra Ball light if there was any
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        'WOOLY DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, "vo_live_again"
        DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, vo_live_again 'SCOOBY sound mod "We saved you Scooby!"

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            ' debug.print "No More Balls, High Score Entry"
            ' Submit the CurrentPlayers score to the High Score system
            CheckHighScore()
        ' you may wish to play some music at this point

        Else
            ' SCOOBY Gypsy welcome back
             PlaySound "sd_gypsy_suprise" ' SCOOBY sound mod gypsy
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
    If(PlayersPlayingGame > 1)Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer > PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then
        PlaySound "sd_game_over" 'Play final song before EndOfGame subroutine executes SCOOBY sound mod Game Over
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode

        ' set the machine into game over mode
        EndOfGame()

    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        DMDScoreNow

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame > 1 Then
            Select Case CurrentPlayer
                Case 1:DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, "vo_player1"
                Case 2:DMD "", CL("PLAYER 2"), "", eNone, eNone, eNone, 1000, True, "vo_player2"
                Case 3:DMD "", CL("PLAYER 3"), "", eNone, eNone, eNone, 1000, True, "vo_player3"
                Case 4:DMD "", CL("PLAYER 4"), "", eNone, eNone, eNone, 1000, True, "vo_player4"
            End Select
        Else
            DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, "vo_youareup"
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    ' PlaySound "mu_death"
    
    ' vpmtimer.AddTimer 2500, "PlayEndQuote '"
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
    StartAttractMode
' you may wish to light any Game Over Light you may have
End Sub

'this calculates the ball number in play
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
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
    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball
    ' Exit Sub ' only for debugging - this way you can add balls from the debug window

    BallsOnPlayfield = BallsOnPlayfield - 1

    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain
    'PlaySoundAt "sd_Redbeard_laugh", Drain 'SCOOBY RedBear laughs as you lose your ball
    DOF 109, DOFPulse
    'if Tilted the end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' is the ball saver active,
        If(bBallSaverActive = True)Then

            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            ' stop the ballsaver timer during the launch ball saver time, but not during multiballs
            If NOT bMultiBallMode Then
                'WOOLY DMD "_", CL("BALL SAVED"), "_", eNone, eBlinkfast, eNone, 2500, True, "vo_live_again"
                DMD "_", CL("BALL SAVED"), "_", eNone, eBlinkfast, eNone, 2500, True, vo_live_again 'SCOOBY sound mod "We saved you Scooby!"
                'DMD "_", CL("FIRE 1"), "_", eNone, eBlinkfast, eNone, 2500, True, sd_ball_launch 'SCOOBY sound mod "We saved you Scooby!"
            'BallSaverTimerExpired_Timer 'enable this line to stop the ballsaver timer
            End If
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' turn off any multiball specific lights
                    If Mode(CurrentPlayer, 0) = 0 Then
                        ChangeGi white
                        ChangeGIIntensity 1
                    End If
                    'stop any multiball modes of this game
                    StopMBmodes
                    ' you may wish to change any music over at this point
                    changesong
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
                PlaySound "sd_redbeard_pirate_stew"'SCOOBY RedBear laughs as you lose your ball
                ' End Mode and timers
                'StopSong Song
                ChangeGi white
                ChangeGIIntensity 1
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, if there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
    'WOOLY If bPlayIntro Then PlaySound "vo_game_start":bPlayIntro = False
    If bPlayIntro Then StopSong mu_theme : PlaySound vo_game_start:bPlayIntro = False 'SCOOBY code
	'SCOOBY code notes this is where fred starts the game
	PlaySound "sd_scooby_laugh_delay10" 'SCOOBY code


    PlaySoundAt "fx_sensor", swPlungerRest
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    'LaunchLight.State = 2
    ' be sure to update the Scoreboard after the animations, if any
    'Start the skillshot lights & variables if any
    If bSkillShotReady Then
        'WOOLY PlaySong "mu_plunger"
        PlaySong mu_plunger 'SCOOBY sound mod "It's a mystery computer game song" Play slow repeating song
        UpdateSkillshot()
        ' show the message to shoot the ball in case the player has fallen sleep
        swPlungerRest.TimerEnabled = 1
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

Sub swPLunger2_Hit 'extra trigger to detect a ball resting down on the plunger
    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        vpmtimer.addtimer 1500, "PlungerIM.AutoFire:DOF 113, DOFPulse:DOF 130, DOFPulse:PlaySoundAt ""fx_kicker"", swPlungerRest '"
    End If
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    lighteffect 6
    bBallInPlungerLane = False
    bAutoPlunger = False           'disable the autoplunger as the ball has left the plunger lane
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
        ChangeSong
        ResetSkillShotTimer.Enabled = 1
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
        EnableBallSaver BallSaverTime
    End If
' turn off LaunchLight
' LaunchLight.State = 0
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds
Sub swPlungerRest_Timer
    IF bOnTheFirstBall Then
        Select Case RndNbr(10)
			'WOOLY Callouts 1st Ball
            'WOOLY Case 1:DMD CL("YOU WILL"), CL("NEVER WIN"), "_", eNone, eNone, eNone, 2000, True, ""
            'WOOLY Case 2:DMD CL("I KNEW YOU"), CL("YOU WOULD BE BACK"), "_", eNone, eNone, eNone, 2000, True, ""
            'WOOLY Case 3:DMD CL("I AM ZEUS"), CL("BOW BEFORE ME"), "_", eNone, eNone, eNone, 2000, True, "sfx_thunder" &RndNbr(9):FlashEffect 2:ZeusF
            'WOOLY Case 4:DMD CL("HEY YOU"), CL("SHOOT THE BALL"), "_", eNone, eNone, eNone, 2000, True, ""
            'WOOLY Case 5:DMD CL("HEY YOU"), CL("WELCOME BACK"), "_", eNone, eNone, eNone, 2000, True, ""

            'SCOOBY callouts & tauntings 1st Ball taunt taunts
            Case 1:DMD CL("THE GANG SEES"), CL("A SPOOKY OLD MANSION"), "_", eNone, eNone, eNone, 2000, True, "sd_get_to_mansion"
            Case 2:DMD CL("THUNDER LIGHTENING"), CL("SEEK SHELTER NOW"), "_", eNone, eNone, eNone, 2000, True, "sfx_thunder" &RndNbr(9):FlashEffect 2:ZeusF
            Case 3:DMD CL("VAMPIRE WARNING"), CL("NOT WELCOME HERE"), "_", eNone, eNone, eNone, 2000, True, "sd_dracula_notwelcome"
            Case 4:DMD CL("BATS FLYING"), CL("FROM OLD MANSION"), "_", eNone, eNone, eNone, 2000, True, "sd_fx_bats_fly"
            Case 5:DMD CL("WOLF HOWLING"), CL("ITS A FULL MOON"), "_", eNone, eNone, eNone, 2000, True, "sd_fx_wolfhowl1"
            Case 6:DMD CL("GYPSY WARNING"), CL("MEET YOUR DOOM"), "_", eNone, eNone, eNone, 2000, True, "sd_gypsy_doom"
            Case 7:DMD CL("SCOOBY DOO"), CL("WHERE ARE YOU"), "_", eNone, eNone, eNone, 2000, True, "sd_where_r_u"
            Case 8:DMD CL("MONSTERS GHOSTS"), CL("WITCHES VILLIANS"), "_", eNone, eNone, eNone, 2000, True, "sd_fx_monster1"
            Case 9:DMD CL("BATS FLYING"), CL("FROM OLD MANSION"), "_", eNone, eNone, eNone, 2000, True, "sd_fx_bats_fly"
            Case 10:DMD CL("IM SCARED SHAGGY"), CL(""), "_", eNone, eNone, eNone, 2000, True, "sd_vo_whimper1"

        End Select
    Else
        Select Case RndNbr(8)
			'WOOLY Callouts Remaining Balls
            'WOOLY Case 1:DMD CL("ARE YOU"), CL("SLEEPING"), "_", eNone, eNone, eNone, 2000, True, ""
            'WOOLY Case 2:DMD CL("WHAT ARE"), CL("YOU WAITING FOR"), "_", eNone, eNone, eNone, 2000, True, ""
            'WOOLY Case 3:DMD CL("HEY"), CL("PULL THE PLUNGER"), "_", eNone, eNone, eNone, 2000, True, ""
            'WOOLY Case 4:DMD CL("ARE YOU PLAYING"), CL("THIS GAME"), "_", eNone, eNone, eNone, 2000, True, ""

			'SCOOBY Callouts and Tauntings Remaining Balls
            Case 1:DMD CL("BATS FLYING"), CL("FROM OLD MANSION"), "_", eNone, eNone, eNone, 2000, True, "sd_fx_bats_fly"
            Case 2:DMD CL("YOU ARE NOT"), CL("WELCOME HERE"), "_", eNone, eNone, eNone, 2000, True, "sd_dracula_gonow"
            Case 3:DMD CL("THUNDER LIGHTENING"), CL("SEEK SHELTER NOW"), "_", eNone, eNone, eNone, 2000, True, "sfx_thunder" &RndNbr(9):FlashEffect 2:ZeusF
            Case 4:DMD CL("WALK THE PLANK"), CL("YOU SWABS"), "_", eNone, eNone, eNone, 2000, True, "sd_redbeard_walk_plank"
            Case 5:DMD CL("A PIRATES SHIP"), CL("IN THE FOG BANK"), "_", eNone, eNone, eNone, 2000, True, "sd_fx_foghorn"
            Case 6:DMD CL("R I M   R A R E D"), CL("SHAGGY"), "_", eNone, eNone, eNone, 2000, True, "sd_vo_whimper1"
            Case 7:DMD CL("MONSTERS GHOSTS"), CL("WITCHES VILLIANS"), "_", eNone, eNone, eNone, 2000, True, "sd_fx_monster1"
            Case 8:DMD CL("MONSTERS GHOSTS"), CL("WITCHES VILLIANS"), "_", eNone, eNone, eNone, 2000, True, "sd_fx_witch"

        End Select
    End If
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimerExpired.Enabled = False
    BallSaverSpeedUpTimer.Enabled = False
    BallSaverTimerExpired.Interval = 1000 * seconds
    BallSaverTimerExpired.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
    li006.BlinkInterval = 160
    li006.State = 2 'God mode light
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimerExpired_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimerExpired.Enabled = False
    BallSaverSpeedUpTimer.Enabled = False 'ensure this timer is also stopped
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
    LightShootAgain.State = 0
    li006.State = 0 'God mode light
    ' if the table uses the same lights for the extra ball or replay then turn them on if needed
    If ExtraBallsAwards(CurrentPlayer) > 0 Then
        LightShootAgain.State = 1
    End If
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
    li006.BlinkInterval = 80
    li006.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board

Sub AddScore(points) 'normal score routine
    If Tilted Then Exit Sub
    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer) * FatesMultiplier
' you may wish to check to see if the player has gotten a replay
End Sub

Sub AddScore2(points) 'used in jackpots, skillshots, combos, and bonus as they doe not use the PlayfieldMultiplier
    If Tilted Then Exit Sub
    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If Tilted Then Exit Sub
    ' add the bonus to the current players bonus variable
    BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If Tilted Then Exit Sub

    ' If(bMultiBallMode = True) Then
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
    DMD "_", CL("INCREASED JACKPOT"), "_", eNone, eNone, eNone, 1000, True, ""
' you may wish to limit the jackpot to a upper limit, ie..
'	If (Jackpot >= 6000000) Then
'		Jackpot = 6000000
' 	End if
'End if
End Sub

Sub AddSuperJackpot(points) 'not used in this table
    If Tilted Then Exit Sub
End Sub

Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxBonusMultiplier)then
        ' then add and set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
        SetBonusMultiplier(NewBonusLevel)
        DMD "_", CL("BONUS X " &NewBonusLevel), "_", eNone, eBlink, eNone, 2000, True, ""
    Else
        AddScore2 500000
        DMD "_", CL("500000"), "_", eNone, eNone, eNone, 1000, True, ""
    End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly

Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level
    UpdateBonusXLights(Level)
End Sub

Sub UpdateBonusXLights(Level) 'no lights in this table
    ' Update the lights
    Select Case Level
    '        Case 1:li021.State = 0:li022.State = 0:li023.State = 0:li024.State = 0
    '        Case 2:li021.State = 1:li022.State = 0:li023.State = 0:li024.State = 0
    '        Case 3:li021.State = 1:li022.State = 1:li023.State = 0:li024.State = 0
    '        Case 4:li021.State = 1:li022.State = 1:li023.State = 1:li024.State = 0
    '        Case 5:li021.State = 1:li022.State = 1:li023.State = 1:li024.State = 1
    End Select
End Sub

Sub AddPlayfieldMultiplier(n)
    Dim NewPFLevel
    ' if not at the maximum level x
    if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier)then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
        SetPlayfieldMultiplier(NewPFLevel)
        DMD "_", CL("PLAYFIELD X " &NewPFLevel), "_", eNone, eBlink, eNone, 2000, True, "sfx_thunder" &RndNbr(7)
        LightEffect 4
    ' Play a voice sound
    Else 'if the max is already lit
        AddScore2 500000
        DMD "_", CL("500000"), "_", eNone, eNone, eNone, 2000, True, ""
    End if
    ' restart the PlayfieldMultiplier timer to reduce the multiplier
    PFXTimer.Enabled = 0
    PFXTimer.Enabled = 1
End Sub

Sub PFXTimer_Timer
    DecreasePlayfieldMultiplier
End Sub

Sub DecreasePlayfieldMultiplier 'reduces by 1 the playfield multiplier
    Dim NewPFLevel
    ' if not at 1 already
    if(PlayfieldMultiplier(CurrentPlayer) > 1)then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer)- 1
        SetPlayfieldMultiplier(NewPFLevel)
    Else
        PFXTimer.Enabled = 0
    End if
End Sub

' Set the Playfield Multiplier to the specified level AND set any lights accordingly

Sub SetPlayfieldMultiplier(Level)
    ' Set the multiplier to the specified level
    PlayfieldMultiplier(CurrentPlayer) = Level
    UpdatePFXLights(Level)
End Sub

Sub UpdatePFXLights(Level) 'no lights in this table
    ' Update the playfield multiplier lights
    Select Case Level
    '        Case 1:li025.State = 0:li026.State = 0:li027.State = 0:li027.State = 0
    '        Case 2:li025.State = 1:li026.State = 0:li027.State = 0:li027.State = 0
    '        Case 3:li025.State = 0:li026.State = 1:li027.State = 0:li027.State = 0
    '        Case 4:li025.State = 0:li026.State = 0:li027.State = 1:li027.State = 0
    '        Case 5:li025.State = 0:li026.State = 0:li027.State = 0:li027.State = 1
    End Select
' perhaps show also the multiplier in the DMD?
End Sub

'***SCOOBY Note: this is a great place to add key codes for testing the different AWARD MODES
Sub AwardExtraBall()
    '   If NOT bExtraBallWonThisBall Then 'in this table you can win several extra balls
    'WOOLY DMD "_", CL("EXTRA BALL WON"), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 108, DOFPulse, DOFKnocker)
    DMD "_", CL("EXTRA BALL WON"), "d_sd_winner", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", dof_knocker, DOFPulse, DOFKnocker) 'SCOOBY mod v1.0.1
    DOF 130, DOFPulse
    DOF dof_shaker, DOFPulse 'SCOOBY v1.0.1 added shaker motor when EXTRABALL is awarded
    PLaySound "vo_extraball"
    PlaySound sd_vo_won_extraball 'SCOOBY code win the extra ball
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
    'bExtraBallWonThisBall = True
    light009.State = 0        'turn off extra ball light
    LightShootAgain.State = 1 'light the shoot again lamp
    GiEffect 1
    LightEffect 2
'    END If
End Sub

Sub AwardSpecial()
    'WOOLY DMD "_", CL("EXTRA GAME WON"), "_", eNone, eBlink, eNone, 2000, True, SoundFXDOF("fx_Knocker", 108, DOFPulse, DOFKnocker)
    DMD "_", CL("EXTRA GAME WON"), "d_sd_winner", eNone, eBlink, eNone, 2000, True, SoundFXDOF("fx_Knocker", dof_knocker, DOFPulse, DOFKnocker) 'SCOOBY code DOF Knocker
    PlaySound sd_vo_won_extragame 'SCOOBY code
    DOF 130, DOFPulse
    DOF dof_shaker, DOFPulse 'SCOOBY v1.0.1 added shaker motor when SPECIAL is awarded
    Credits = Credits + 1
    AddScore2 3000000 '3 mill only for this table
    If bFreePlay = False Then DOF 121, DOFOn
    LightEffect 2
    GiEffect 1
    Light010.State = 0 'turn off special light
End Sub

Sub AwardJackpot()     'only used for the final mode
    DMD CL("JACKPOT"), CL(FormatScore(Jackpot(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_Jackpot"
    DOF 137, DOFPulse
    AddScore2 Jackpot(CurrentPlayer)
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + 100000
    LightEffect 2
    GiEffect 1
    FlashEffect 1
End Sub

Sub AwardSuperJackpot() 'not used in this table as there are several superjackpots but I keep it as a reference
    DMD CL("SUPER JACKPOT"), CL(FormatScore(SuperJackpot(CurrentPlayer))), "d_border", eNone, eBlink, eNone, 2000, True, "vo_super_jackpot"
    DOF 137, DOFPulse
    AddScore2 SuperJackpot(CurrentPlayer)
    LightEffect 2
    GiEffect 1
End Sub

'SCOOBY Skillshots
Sub AwardSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL("SKILLSHOT"), CL(FormatScore(SkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_skillshot"
    DMD CL("SKILLSHOT"), CL(FormatScore(SkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, vo_skillshot 'SCOOBY code
    DOF 127, DOFPulse
    Addscore2 SkillShotValue(CurrentPlayer)
    ' increment the skillshot value with 50.000
    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 50000
    'do some light show
    GiEffect 1
    LightEffect 2
End Sub

Sub AwardSuperSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL("SUPER SKILLSHOT"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_superskillshot"
    DMD CL("SUPER SKILLSHOT"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, vo_superskillshot 'SCOOBY code
    DOF 138, DOFPulse
    Addscore2 SuperSkillshotValue(CurrentPlayer)
    ' increment the skillshot value with 500.000
    SuperSkillshotValue(CurrentPlayer) = SuperSkillshotValue(CurrentPlayer) + 500000
    'do some light show
    GiEffect 1
    LightEffect 2
End Sub

Sub AwardFreakySkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL("FREAKY SKILLSHOT"), CL(FormatScore(FreakySkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_freakyskillshot"
    DMD CL("FREAKY SKILLSHOT"), CL(FormatScore(FreakySkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, vo_freakyskillshot 'SCOOBY code 
    DOF 138, DOFPulse
    Addscore2 FreakySkillshotValue(CurrentPlayer)
    ' increment the skillshot value with 500.000
    FreakySkillshotValue(CurrentPlayer) = FreakySkillshotValue(CurrentPlayer) + 500000
    'do some light show
    GiEffect 1
    LightEffect 2
End Sub

Sub aSkillshotTargets_Hit(idx) 'stop the skillshot if any other target/switch is hit
    If bSkillshotReady then ResetSkillShotTimer_Timer
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 100000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 100000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 100000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 100000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(cGameName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0:If bFreePlay = False Then DOF 121, DOFOff:End If
    x = LoadValue(cGameName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
    SaveValue cGameName, "HighScore1", HighScore(0)
    SaveValue cGameName, "HighScore1Name", HighScoreName(0)
    SaveValue cGameName, "HighScore2", HighScore(1)
    SaveValue cGameName, "HighScore2Name", HighScoreName(1)
    SaveValue cGameName, "HighScore3", HighScore(2)
    SaveValue cGameName, "HighScore3Name", HighScoreName(2)
    SaveValue cGameName, "HighScore4", HighScore(3)
    SaveValue cGameName, "HighScore4Name", HighScoreName(3)
    SaveValue cGameName, "Credits", Credits
    SaveValue cGameName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
    HighScoreName(0) = "AAA"
    HighScoreName(1) = "BBB"
    HighScoreName(2) = "CCC"
    HighScoreName(3) = "DDD"
    HighScore(0) = 1500000
    HighScore(1) = 1400000
    HighScore(2) = 1300000
    HighScore(3) = 1200000
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
        DOF 121, DOFOn
    End If

    If tmp > HighScore(3)Then
        PlaySound SoundFXDOF("fx_Knocker", 108, DOFPulse, DOFKnocker)
        DOF 130, DOFPulse
        HighScore(3) = tmp
        'Play HighScore sound
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    PlaySound "vo_enterinitials"
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

Sub HighScoreDisplayName()
    Dim i
    Dim TempTopStr
    Dim TempBotStr

    TempTopStr = "YOUR NAME:"
    dLine(0) = ExpandLine(TempTopStr)
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
    dLine(1) = ExpandLine(TempBotStr)
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

'*********
'   LUT
'*********

Dim bLutActive, LUTImage
Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "")Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 10:UpdateLUT:SaveLUT:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0"
        Case 1:table1.ColorGradeImage = "LUT1"
        Case 2:table1.ColorGradeImage = "LUT2"
        Case 3:table1.ColorGradeImage = "LUT3"
        Case 4:table1.ColorGradeImage = "LUT4"
        Case 5:table1.ColorGradeImage = "LUT5"
        Case 6:table1.ColorGradeImage = "LUT6"
        Case 7:table1.ColorGradeImage = "LUT7"
        Case 8:table1.ColorGradeImage = "LUT8"
        Case 9:table1.ColorGradeImage = "LUT9"
    End Select
End Sub

' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text.
' 1st and 2nd lines are 20 characters long
' 3rd line is just 1 character
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

Dim dLine(2)
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
    Dim i, j
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
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_border")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + i * 12, 6, 12, 22
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + (i - 20) * 12, 34, 12, 22
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
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_border")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3, 6, 11
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + (i - 20) * 6, 17, 6, 11
                Next
                FlexDMD.LockRenderThread
                FlexDMD.Stage.AddActor DMDScene
                FlexDMD.UnlockRenderThread
            End If
        End If
    End If

    DMDFlush()
    deSpeed = 20
    deBlinkSlowRate = 10
    deBlinkFastRate = 5
    For i = 0 to 2
        dLine(i) = Space(20)
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
    dLine(2) = " "
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
    Dim tmp, tmp1, tmp1a, tmp1b, tmp2
    if(dqHead = dqTail)Then
        ' default when no modes are active
        tmp = RL(FormatScore(Score(Currentplayer)))
        tmp1 = FL("PLAYER " &CurrentPlayer, "BALL " & Balls)
        tmp2 = "d_border"
        'info on the second line & background
        Select Case Mode(CurrentPlayer, 0)
            Case 0 'no battle active
                If bMedusaMBStarted Then
                    'WOOLY tmp2 = "d_medusa"
                    tmp2 = "d_medusa_alt" 'SCOOBY code, new DMD screents for Scooby Villains and Ghosts
                End If
            Case 1 ' Minotaur
                'WOOLY tmp2 = "d_minotaur"
                tmp2 = "d_minotaur_alt" 'SCOOBY code, new DMD screents for Scooby Villains and Ghosts
                Select Case ModeStep
                    Case 1:tmp1 = CL("SHOOT LIT LIGHTS")
                    Case 2:tmp1 = CL("SHOOT THE SCOOP")
                End Select
            Case 2 ' Hydra
                'WOOLY tmp2 = "d_hydra"
                tmp2 = "d_hydra_alt" 'SCOOBY code, new DMD screents for Scooby Villains and Ghosts
                Select Case ModeStep
                    Case 1, 2, 3:tmp1 = CL("SHOOT LIT LIGHT")
                    Case 4:tmp1 = CL("SHOOT THE SCOOP")
                End Select
            Case 3 ' Cerberus
                'WOOLY tmp2 = "d_cerberus"
                tmp2 = "d_cerberus_alt" 'SCOOBY code, new DMD screents for Scooby Villains and Ghosts
                Select Case ModeStep
                    Case 1:tmp1 = CL("SHOOT LIT LIGHTS")
                    Case 2:tmp1 = CL("SHOOT THE SCOOP")
                End Select
            Case 4 ' Medusa
                'WOOLY tmp2 = "d_medusa"
                tmp2 = "d_medusa_alt" 'SCOOBY code, new DMD screents for Scooby Villains and Ghosts
                Select Case ModeStep
                    Case 1, 2, 3:tmp1 = CL("SHOOT LIT LIGHT")
                    Case 4:tmp1 = CL("SHOOT THE SCOOP")
                End Select
            Case 5 ' Ares
                'WOOLY tmp2 = "d_ares"
                tmp2 = "d_ares_alt" 'SCOOBY code, new DMD screents for Scooby Villains and Ghosts
                Select Case ModeStep
                    Case 1:tmp1 = CL("SPINNERS LEFT " & (100-SpinnerHits))
                    Case 2:tmp1 = CL("SHOOT THE SCOOP")
                End Select
            Case 6 ' Poseidon
                'WOOLY tmp2 = "d_poseidon"
                tmp2 = "d_poseidon_alt" 'SCOOBY code, new DMD screents for Scooby Villains and Ghosts
                Select Case ModeStep
                    Case 1:tmp1 = CL("SHOOT LIT TARGETS")
                    Case 2:tmp1 = CL("SHOOT THE SCOOP")
                End Select
            Case 7 ' Hades
                'WOOLY tmp2 = "d_hades"
                tmp2 = "d_hades_alt" 'SCOOBY code, new DMD screents for Scooby Villains and Ghosts
                'WOOLY tmp1 = CL("HITS LEFT " & (5-TrapDoorHits))
                tmp1 = CL("KNIGHT HITS LEFT " & (5-TrapDoorHits))
            Case 8 ' Zeus
                'WOOLY tmp2 = "d_zeus"
                tmp2 = "d_zeus_alt" 'SCOOBY code, new DMD screents for Scooby Villains and Ghosts
                'WOOLY tmp1 = "HIT THE ZEUS TARGETS"
                tmp1 = "  HIT ZOMB TARGETS" 'SCOOBY code, new DMD screents for Scooby Villains and Ghosts
            Case 9 ' God or Demi-God mode
                tmp1 = CL("SHOOT THE JACKPOTS")
                'WOOLY tmp2 = "d_zeus"
                tmp2 = "d_zeus_alt" 'SCOOBY code, new DMD screents for Scooby Villains and Ghosts
        End Select
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail < dqSize)Then
        if(Text0 = "_")Then
            dqEffect(0, dqTail) = eNone
            dqText(0, dqTail) = "_"
        Else
            dqEffect(0, dqTail) = Effect0
            dqText(0, dqTail) = ExpandLine(Text0)
        End If

        if(Text1 = "_")Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1)
        End If

        if(Text2 = "_")Then
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
        if(dqTail = 1)Then
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
            Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead))
            Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead))
            Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
            Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
        End Select
    Next
    if(dqSound(dqHead) <> "")Then
        PlaySound(dqSound(dqHead))
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
    if(dqHead = dqTail)Then
        if(dqbFlush(Head) = True)Then
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
        if(deCount(i) <> deCountEnd(i))Then
            deCount(i) = deCount(i) + 1

            select case(dqEffect(i, dqHead))
                case eNone:
                    Temp = dqText(i, dqHead)
                case eScrollLeft:
                    Temp = Right(dLine(i), 19)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), 21 - deCount(i), 1)
                    Temp = Temp & Left(dLine(i), 19)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkSlowRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(20)
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkFastRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(20)
                    End If
            End Select

            if(dqText(i, dqHead) <> "_")Then
                dLine(i) = Temp
                DMDUpdate i
            End If
        End If
    Next

    if(deCount(0) = deCountEnd(0))and(deCount(1) = deCountEnd(1))and(deCount(2) = deCountEnd(2))Then

        if(dqTimeOn(dqHead) = 0)Then
            DMDFlush()
        Else
            if(BlinkEffect = True)Then
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

Function ExpandLine(TempStr) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(20)
    Else
        if Len(TempStr) > Space(20)Then
            TempStr = Left(TempStr, Space(20))
        Else
            if(Len(TempStr) < 20)Then
                TempStr = TempStr & Space(20 - Len(TempStr))
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

    NumString = CStr(abs(Num))

    For i = Len(NumString)-3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1))then
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1)) + 48) & right(NumString, Len(NumString)- i)
        end if
    Next
    FormatScore = NumString
End function

Function FL(NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    Temp = 20 - Len(NumString1)- Len(NumString2)
    TempStr = NumString1 & Space(Temp) & NumString2
    FL = TempStr
End Function

Function CL(NumString) 'center line
    Dim Temp, TempStr
    ' WOOLY Temp = (20 - Len(NumString)) \ 2
    Temp = ((20 - Len(NumString)) * .5) 'SCOOBY code modified to use factor .5 instead of division
    'DMD "DEBUG           TEMP",("VALUE IS " & Temp), "_", eNone, eBlinkFast, eNone, 2000, True, "" 'SCOOBY debugging code
    TempStr = Space(Temp) & NumString & Space(Temp)
    'DMD "DEBUG     TEMPSTR IS",TempStr, "_", eNone, eBlinkFast, eNone, 2000, True, "" 'SCOOBY debugging code
    CL = TempStr
End Function

Function RL(NumString) 'right line
    Dim Temp, TempStr
    Temp = 20 - Len(NumString)
    TempStr = Space(Temp) & NumString
    RL = TempStr
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
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "d_border"
            Digits(40).ImageA = dLine(2)
			'SCOOBY bug crash on next line
            'WOOLY If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
            If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("_", "VPX." & dLine(2) & "&dmd=2").Bitmap
    End Select
    If UseFlexDMD Then FlexDMD.UnlockRenderThread
End Sub

Sub DMDDisplayChar(achar, adigit)
    If achar = "" Then achar = " "
    achar = ASC(achar)
    Digits(adigit).ImageA = Chars(achar)
    'WOOLY If UseFlexDMD Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap
    If UseFlexDMD Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("_", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap 'SCOOBY code
End Sub

'****************************
' JP's new DMD using flashers
'****************************

Dim Digits, Chars(255), Images(255)

DMDInit

Sub DMDInit
    Dim i
    Digits = Array(digit001, digit002, digit003, digit004, digit005, digit006, digit007, digit008, digit009, digit010, _
        digit011, digit012, digit013, digit014, digit015, digit016, digit017, digit018, digit019, digit020,            _
        digit021, digit022, digit023, digit024, digit025, digit026, digit027, digit028, digit029, digit030,            _
        digit031, digit032, digit033, digit034, digit035, digit036, digit037, digit038, digit039, digit040,            _
        digit041)
    For i = 0 to 255:Chars(i) = "d_empty":Next

    Chars(32) = "d_empty"
    Chars(33) = ""        '!
    Chars(34) = ""        '"
    Chars(35) = ""        '#
    Chars(36) = ""        '$
    Chars(37) = ""        '%
    Chars(38) = ""        '&
    Chars(39) = ""        ''
    Chars(40) = ""        '(
    Chars(41) = ""        ')
    Chars(42) = ""        '*
    Chars(43) = "d_plus"  '+
    Chars(44) = ""        '
    Chars(45) = "d_minus" '-
    Chars(46) = "d_dot"   '.
    Chars(47) = ""        '/
    Chars(48) = "d_0"     '0
    Chars(49) = "d_1"     '1
    Chars(50) = "d_2"     '2
    Chars(51) = "d_3"     '3
    Chars(52) = "d_4"     '4
    Chars(53) = "d_5"     '5
    Chars(54) = "d_6"     '6
    Chars(55) = "d_7"     '7
    Chars(56) = "d_8"     '8
    Chars(57) = "d_9"     '9
    Chars(60) = "d_less"  '<
    Chars(61) = ""        '=
    Chars(62) = "d_more"  '>
    Chars(64) = ""        '@
    Chars(65) = "d_a"     'A
    Chars(66) = "d_b"     'B
    Chars(67) = "d_c"     'C
    Chars(68) = "d_d"     'D
    Chars(69) = "d_e"     'E
    Chars(70) = "d_f"     'F
    Chars(71) = "d_g"     'G
    Chars(72) = "d_h"     'H
    Chars(73) = "d_i"     'I
    Chars(74) = "d_j"     'J
    Chars(75) = "d_k"     'K
    Chars(76) = "d_l"     'L
    Chars(77) = "d_m"     'M
    Chars(78) = "d_n"     'N
    Chars(79) = "d_o"     'O
    Chars(80) = "d_p"     'P
    Chars(81) = "d_q"     'Q
    Chars(82) = "d_r"     'R
    Chars(83) = "d_s"     'S
    Chars(84) = "d_t"     'T
    Chars(85) = "d_u"     'U
    Chars(86) = "d_v"     'V
    Chars(87) = "d_w"     'W
    Chars(88) = "d_x"     'X
    Chars(89) = "d_y"     'Y
    Chars(90) = "d_z"     'Z
    Chars(94) = "d_up"    '^
    '    Chars(95) = '_
    Chars(96) = "d_0a"  '0.
    Chars(97) = "d_1a"  '1. 'a
    Chars(98) = "d_2a"  '2. 'b
    Chars(99) = "d_3a"  '3. 'c
    Chars(100) = "d_4a" '4. 'd
    Chars(101) = "d_5a" '5. 'e
    Chars(102) = "d_6a" '6. 'f
    Chars(103) = "d_7a" '7. 'g
    Chars(104) = "d_8a" '8. 'h
    Chars(105) = "d_9a" '9. 'i
    Chars(106) = ""     'j
    Chars(107) = ""     'k
    Chars(108) = ""     'l
    Chars(109) = ""     'm
    Chars(110) = ""     'n
    Chars(111) = ""     'o
    Chars(112) = ""     'p
    Chars(113) = ""     'q
    Chars(114) = ""     'r
    Chars(115) = ""     's
    Chars(116) = ""     't
    Chars(117) = ""     'u
    Chars(118) = ""     'v
    Chars(119) = ""     'w
    Chars(120) = ""     'x
    Chars(121) = ""     'y
    Chars(122) = ""     'z
    Chars(123) = ""     '{
    Chars(124) = ""     '|
    Chars(125) = ""     '}
    Chars(126) = ""     '~
End Sub

'********************
' Real Time updates
'********************
'used for all the real time updates

Sub Realtime_Timer
    RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    LeftFlipperTop001.RotZ = LeftFlipper001.CurrentAngle
    LeftFlipperTop002.RotZ = LeftFlipper2.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
    RightFlipperTop001.RotZ = RightFlipper001.CurrentAngle
    RightFlipperTop002.RotZ = RightFlipper2.CurrentAngle
' add any other real time update subs, like gates or diverters, flippers
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
' 11 colors: red, orange, amber, yellow...
'******************************************

'colors
Const red = 5
Const orange = 4
Const amber = 6
Const yellow = 3
Const darkgreen = 7
Const green = 2
Const blue = 1
Const darkblue = 8
Const purple = 9
Const white = 11
Const teal = 10

Sub SetLightColor(n, col, stat) 'stat 0 = off, 1 = on, 2 = blink, -1= no change
    Select Case col
        Case red
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(255, 0, 0)
        Case orange
            n.color = RGB(18, 3, 0)
            n.colorfull = RGB(255, 64, 0)
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
            n.color = RGB(0, 16, 0)
            n.colorfull = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 18, 18)
            n.colorfull = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 8, 8)
            n.colorfull = RGB(0, 64, 64)
        Case purple
            n.color = RGB(64, 0, 96)
            n.colorfull = RGB(128, 0, 192)
        Case white
            n.color = RGB(193, 91, 0)
            n.colorfull = RGB(255, 197, 143)
        Case teal
            n.color = RGB(1, 64, 62)
            n.colorfull = RGB(2, 128, 126)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

Sub SetFlashColor(n, col, stat) 'stat 0 = off, 1 = on, -1= no change - no blink for the flashers, use FlashForMs
    Select Case col
        Case red
            n.color = RGB(255, 0, 0)
        Case orange
            n.color = RGB(255, 64, 0)
        Case amber
            n.color = RGB(255, 153, 0)
        Case yellow
            n.color = RGB(255, 255, 0)
        Case darkgreen
            n.color = RGB(0, 64, 0)
        Case green
            n.color = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 64, 64)
        Case purple
            n.color = RGB(128, 0, 192)
        Case white
            n.color = RGB(255, 197, 143)
        Case teal
            n.color = RGB(2, 128, 126)
    End Select
    If stat <> -1 Then
        n.Visible = stat
    End If
End Sub

'*************************
' Rainbow Changing Lights
'*************************

Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

Sub StartRainbow(n) 'n is a collection
    set RainbowLights = n
    RGBStep = 0
    RGBFactor = 5
    rRed = 255
    rGreen = 0
    rBlue = 0
    RainbowTimer.Enabled = 1
End Sub

Sub StopRainbow()
    RainbowTimer.Enabled = 0
End Sub

Sub RainbowTimer_Timer 'rainbow led light color changing
    Dim obj
    Select Case RGBStep
        Case 0 'Green
            rGreen = rGreen + RGBFactor
            If rGreen > 255 then
                rGreen = 255
                RGBStep = 1
            End If
        Case 1 'Red
            rRed = rRed - RGBFactor
            If rRed < 0 then
                rRed = 0
                RGBStep = 2
            End If
        Case 2 'Blue
            rBlue = rBlue + RGBFactor
            If rBlue > 255 then
                rBlue = 255
                RGBStep = 3
            End If
        Case 3 'Green
            rGreen = rGreen - RGBFactor
            If rGreen < 0 then
                rGreen = 0
                RGBStep = 4
            End If
        Case 4 'Red
            rRed = rRed + RGBFactor
            If rRed > 255 then
                rRed = 255
                RGBStep = 5
            End If
        Case 5 'Blue
            rBlue = rBlue - RGBFactor
            If rBlue < 0 then
                rBlue = 0
                RGBStep = 0
            End If
    End Select
    For each obj in RainbowLights
        obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
        obj.colorfull = RGB(rRed, rGreen, rBlue)
    Next
End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim ii
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1)Then
        DMD CL("LAST SCORE"), CL("PLAYER 1 " &FormatScore(Score(1))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2)Then
        DMD CL("LAST SCORE"), CL("PLAYER 2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3)Then
        DMD CL("LAST SCORE"), CL("PLAYER 3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4)Then
        DMD CL("LAST SCORE"), CL("PLAYER 4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    DMD "", CL("GAME OVER"), "", eNone, eBlink, eNone, 2000, False, ""
    If bFreePlay Then
        DMD "", CL("FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits > 0 Then
            DMD CL("CREDITS " & Credits), CL("PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
'** WOOLY DMD ORIGINAL CREDITS FROM WOOLY 2022 NEXT
    'WOOLY DMD "        JPSALAS", "          AND", "d_jppresents", eNone, eNone, eNone, 2000, False, ""
    'WOOLY DMD "    T-800", "  PRESENTS", "d_t800", eNone, eNone, eNone, 2000, False, ""
    'WOOLY DMD "", "", "d_title", eNone, eNone, eNone, 4000, False, ""

'** SCOOBY DOO TABLE CREDITS AUTHORS AND DEVELOPERS FROM WOOLY AND VP9 SCOOBY TABLE & MYSELF (DEVELOPER Credits)
    DMD " MRJCRANE      ", "          AND", "d_jcpresents", eNone, eNone, eNone, 3500, False, "" 'SCOOBY code
    DMD "       JP SALAS", "          AND", "d_jppresents", eNone, eNone, eNone, 2000, False, "" 'SCOOBY code
    DMD "    T-800", "   PRESENT", "d_t800", eNone, eNone, eNone, 2000, False, "" 'SCOOBY code
    DMD "", "", "d_title_scooby", eNone, eNone, eNone, 4000, False, "" 'SCOOBY code
    DMD "   DOF BY ARNGRIM", "", "d_border", eNone, eNone, eNone, 2000, False, "" 'SCOOBY code
    DMD "ORIGINAL VP9 TABLE", "JP SALAS - MASALAJA", "d_border", eNone, eNone, eNone, 2000, False, "" 'SCOOBY code

'** SCOOBY DOO END OF AUTHORS, DEVELOPERS AND CONTRIBUTORS

    DMD "", CL("ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("HIGHSCORES"), Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL("HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL("HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

'SCOOOBY This is the pre-game sequence when the game is launch but players have not started yet
Sub StartAttractMode
    StartLightSeq
    DMDFlush
    ShowTableInfo
    'WOOLY PlaySong "mu_game_start"
    PlaySound "sd_vo_VincentPrice"
    StopSong mu_theme : PlaySong sd_introsong: PlaySound " " 'SCOOBY play computer game version of theme song
    sd_MysteryMachineStartup
End Sub

Sub StopAttractMode
    'SCOOBY optional sound: PlaySound "sd_scooby_laugh1" 'SCOOBY code Scooby laughs before the game starts
    PlaySound "sd_dracula_notwelcome" 'SCOOBY code dracula tries to run you off before the game starts
	PlaySound "sd_scooby_laugh_delay20" 'SCOOBY code 20 second delay then Scooby starts laughing when game starts
    StopRainbow
    DMDScoreNow
    LightSeqAttract.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqDiagUpRightOn, 25, 2
    LightSeqAttract.Play SeqStripe1VertOn, 25
    LightSeqAttract.Play SeqClockRightOn, 180, 2
    LightSeqAttract.Play SeqFanLeftUpOn, 50, 2
    LightSeqAttract.Play SeqFanRightUpOn, 50, 2
    LightSeqAttract.Play SeqScrewRightOn, 50, 2
    LightSeqAttract.Play SeqDiagDownLeftOn, 25, 2
    LightSeqAttract.Play SeqStripe2VertOn, 25, 2
    LightSeqAttract.Play SeqFanLeftDownOn, 50, 2
    LightSeqAttract.Play SeqFanRightDownOn, 50, 2
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

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, timers, etc
Sub VPObjects_Init
    TrapdoorDown
End Sub

' tables variables and Mode init
Dim bRotateLights
Dim bPlayIntro
Dim Mode(4, 9) '4 players, 8 modes
Dim BumperHits 'used for the skillshot and
Dim FreakySkillshotValue(4)
Dim ComboValue(4)
Dim ComboHits(4)
Dim ComboCount
Dim BumperAward
Dim OrbitHits
Dim RampHits
Dim CurrentMode(4)       'the current selected mode, used to increase the modes or battles
Dim bModeReady           'used to enable the mode at the start battle hole
Dim ModeStep             'use for the different steps during the modes/battles
Dim EndModeCountdown
Dim ZeusTargetsCompleted 'used in Zeus mode to count the time the Zeus targets has been completed
Dim ZeusHits
Dim ZeusHitsNeeded
Dim ZeusCount(4)  'hits needed to start zeus multiball
Dim bMedusaMBStarted
'WOOLY Dim MedussaX      'multiplier during medusa MB
Dim MedusaX      'multiplier during medusa MB fixing Medussa to Medusa Scooby mod v1.0.1
Dim ExtraBallHits 'used in medusa multiball
Dim SpinnerHits   'used in Ares mode
Dim TrapDoorHits  'used in Hades mode
Dim bJackpotsEnabled
Dim bLockEnabled
Dim MinotaurJackpot(4)              'jackpot value
Dim bMinotaurMBStarted
Dim Minotaur1, Minotaur2, Minotaur3 'to check if all 3 holes has been hit
Dim bZeusMBStarted
Dim PandorasHits(4)                 'targets hits
Dim PandorasNeeded(4)               'number of hits needed to start the random award
Dim TreeHits(4)
Dim kickbackHits(4)
Dim kickbackNeeded(4)
Dim GodModeHits(4)   'the number of times the god mode targets has been hit
Dim GodModeNeeded(4) 'number of hits required to start
Dim FatesHits(4)     'number of hits to start the Fates mode: PlayfieldMultiplier
Dim bFatesStarted
Dim FatesMultiplier
Dim CBHits(4) 'captive ball hits
Dim BonusTargets(4)
Dim BonusRamps(4)
Dim BonusOrbits(4)
Dim BonusXHits(4)
Dim HiddenShots(4)
Dim TotalMonsters(4)
Dim TotalGods(4)

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    'Init Variables
    bPlayIntro = True
    BallSaverTime = 20
    bExtraBallWonThisBall = False
    BumperHits = 0
    bRotateLights = True
    BumperAward = 5000
    ComboCount = 0
    OrbitHits = 0
    RampHits = 0
    EndModeCountdown = 0
    ZeusHits = 0
    ZeusHitsNeeded = 12
    ModeStep = 0
    bModeReady = False
    bMedusaMBStarted = False
    'WOOLY MedussaX = 1
    MedusaX = 1 'SCOOBY code v1.0.1
    ExtraBallHits = 0
    SpinnerHits = 0
    TrapDoorHits = 0
    ZeusTargetsCompleted = 0
    bJackpotsEnabled = False
    bLockEnabled = False
    Minotaur1 = 0
    Minotaur2 = 0
    Minotaur3 = 0
    bMinotaurMBStarted = False
    bZeusMBStarted = False
    bFatesStarted = False
    FatesMultiplier = 1
    For i = 0 to 4
        SkillshotValue(i) = 100000
        SuperSkillshotValue(i) = 1000000
        FreakySkillshotValue(i) = 1500000
        CurrentMode(i) = 0
        ComboValue(i) = 250000
        ZeusCount(i) = 0
        MinotaurJackpot(i) = 250000
        BallsInLock(i) = 0
        SuperJackpot(i) = 5000000
        Jackpot(i) = 1000000
        PandorasHits(i) = 0
        PandorasNeeded(i) = 2
        TreeHits(i) = 0
        kickbackHits(i) = 0
        kickbackNeeded(i) = 2
        GodModeHits(i) = 0
        GodModeNeeded(i) = 1
        FatesHits(i) = 0
        CBHits(i) = 0
        BonusTargets(i) = 0
        BonusRamps(i) = 0
        BonusOrbits(i) = 0
        BonusXHits(i) = 0
        ComboHits(i) = 0
        HiddenShots(i) = 0
        TotalMonsters(i) = 0
        TotalGods(i) = 0
    Next
    For i = 0 to 4
        For j = 0 to 9
            Mode(i, j) = 0
        Next
    Next
    TurnOffPlayfieldLights()
End Sub

Sub InstantInfo
    'SCOOBY code added expanded CLUES for the player when INSTANT INFO is pressed
    Dim tmp
    DMD CL("INSTANT INFO"), "", "", eNone, eNone, eNone, 1000, True, ""
    Select Case Mode(CurrentPlayer, 0)
        Case 0 ' no Battle active
        Case 1 ' Minotaur
            'WOOLY DMD CL("CURRENT MODE"), CL("MINOTAUR"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("CURRENT MODE"), CL("KINGSTON ALL ORBITS"), "", eNone, eNone, eNone, 2000, False, "" 'AKA SCOOBY dmd Elias Kingston
            DMD CL("SHOOT THE LIGHTS"), CL("AND SCOOP TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL(" C   L   U   E   S"), CL("3 ORBITS THEN SCOOP"), "", eNone, eNone, eNone, 8000, False, ""
        Case 2 ' Hydra
            'WOOLY DMD CL("CURRENT MODE"), CL("HYDRA"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("CURRENT MODE"), CL("TIKI RAMPS L R L R"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE LIGHTS"), CL("AND SCOOP TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL(" C   L   U   E   S"), CL("RAMPS L R L R SCOOP"), "", eNone, eNone, eNone, 8000, False, ""
        Case 3 ' Cerberus
            'WOOLY DMD CL("CURRENT MODE"), CL("CERBERUS"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("CURRENT MODE"), CL("CUTLER ALL RAMPS"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE LIGHTS"), CL("AND SCOOP TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL(" C   L   U   E   S"), CL("ALL 3 RAPMPS SCOOP"), "", eNone, eNone, eNone, 8000, False, ""
        Case 4 ' Medusa
            'WOOLY DMD CL("CURRENT MODE"), CL("MEDUSA"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("CURRENT MODE"), CL("GHOST CLOWN"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE LIGHTS"), CL("AND SCOOP TO FINISH "), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL(" C   L   U   E   S"), CL("ALL 4 ORBITS SCOOP"), "", eNone, eNone, eNone, 8000, False, ""
        Case 5 ' Ares
            'WOOLY DMD CL("CURRENT MODE"), CL("ARES"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("CURRENT MODE"), CL("GREEN GHOST"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE SPINNERS"), CL("AND SCOOP TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL(" C   L   U   E   S"), CL("100 SPINNERS SCOOP"), "", eNone, eNone, eNone, 8000, False, ""
        Case 6 ' Poseidon
            'WOOLY DMD CL("CURRENT MODE"), CL("POSEIDON"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("CURRENT MODE"), CL("FRANKENSTEIN"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT LIT TARGETS"), CL("AND SCOOP TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL(" C   L   U   E   S"), CL("LIT TARGETS SCOOP"), "", eNone, eNone, eNone, 8000, False, ""
        Case 7 ' Hades
            'WOOLY DMD CL("CURRENT MODE"), CL("HADES"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("CURRENT MODE"), CL("THE BLACK KNIGHT"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE TRAPDOOR"), "", "", eNone, eNone, eNone, 2000, False, ""
            DMD CL(" C   L   U   E   S"), CL("TRAP DOOR 5 TIMES"), "", eNone, eNone, eNone, 8000, False, ""
        Case 8 ' Zeus
           'WOOLY DMD CL("CURRENT MODE"), CL("ZEUS"), "", eNone, eNone, eNone, 2000, False, ""
           'WOOLY DMD CL("HIT THE ZEUS TARGETS"), CL("AND SCOOP TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("CURRENT MODE"), CL("ZOMBIE"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("HIT ZOMBIE TARGETS"), CL("AND SCOOP TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL(" C   L   U   E   S"), CL("HIT ZOMB 3X SCOOP"), "", eNone, eNone, eNone, 8000, False, ""
        Case 9 ' God or Demi-God mode
            DMD CL("CURRENT MODE"), CL("WIZARD MODE"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("HIT THE JACKPOTS"), "", "", eNone, eNone, eNone, 2000, False, ""
    End Select

    DMD CL("YOUR SCORE"), CL(FormatScore(Score(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("EXTRA BALLS"), CL(ExtraBallsAwards(CurrentPlayer)), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("BONUS MULTIPLIER"), CL(FormatScore(BonusMultiplier(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("PLAYFIELD MULTIPLIER"), CL(FormatScore(PlayfieldMultiplier(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SKILLSHOT VALUE"), CL(FormatScore(SkillshotValue(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SUPR SKILLSHOT VALUE"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("TARGETS HIT"), CL(FormatScore(BonusTargets(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("RAMPS HIT"), CL(FormatScore(BonusRamps(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("ORBITS HIT"), CL(FormatScore(BonusOrbits(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("COMBO VALUE"), CL(FormatScore(ComboValue(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("X HITS"), CL(FormatScore(BonusXHits(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    'WOOLY DMD CL("MONSTERS DEFEATED"), CL(TotalMonsters(CurrentPlayer)), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("MONSTERS CAPTURED"), CL(TotalMonsters(CurrentPlayer)), "", eNone, eNone, eNone, 2000, False, "" 'SCOOBY code dmd
    'WOOLY DMD CL("GODS DEFEATED"), CL(TotalGods(CurrentPlayer)), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("GHOSTS CAPTURED"), CL(TotalGods(CurrentPlayer)), "", eNone, eNone, eNone, 2000, False, "" 'SCOOBY code dmd
    If Score(1)Then
        DMD CL("PLAYER 1 SCORE"), CL(FormatScore(Score(1))), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(2)Then
        DMD CL("PLAYER 2 SCORE"), CL(FormatScore(Score(2))), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(3)Then
        DMD CL("PLAYER 3 SCORE"), CL(FormatScore(Score(3))), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(4)Then
        DMD CL("PLAYER 4 SCORE"), CL(FormatScore(Score(4))), "", eNone, eNone, eNone, 2000, False, ""
    End If
End Sub

Sub StopMBmodes 'stop multiball modes after loosing the last multibal
    bMedusaMBStarted = False
    'WOOLY MedussaX = 1
    MedusaX = 1 'SCOBY code v1.0.1 fixing spelling Medussa to Medusa
    If Mode(CurrentPlayer, 0) = 9 Then StopMode 'stop the God or Demi God multiball
    If bMinotaurMBStarted Then
        bMinotaurMBStarted = False
        If Mode(CurrentPlayer, 0) <> 7 Then 'Not in the Hades mode
            TrapdoorDown
            PlaySound sd_lower_iron_door 'SCOOBY sound mod "Lower the iron door says RedBeard"
            PlaySound sd_fx_trapdoor_down 'SCOOBY code play trapdoor closing sound
			DOF dof_shaker, DOFPulse 'SCOOBY code pulse the shake motor at the end of the round        
        End If
    End If
    bZeusMBStarted = False
    ZeusMBFlashTimer.Enabled = 0
End Sub

Sub StopEndOfBallMode()      'this sub is called after the last ball in play is drained, modes, timers
    StopMode                 'stop current mode
    TrapdoorDown
    PlaySound sd_fx_trapdoor_down 'SCOOBY code play trapdoor closing sound
    'DOF dof_shaker, DOFPulse 'SCOOBY code pulse the shake motor at the end of the round
    LightSeqBumpers.StopPlay 'in case it was on.
End Sub

Sub ResetNewBallVariables()  'reset variables and lights for a new ball or player
    'turn on or off the needed lights before a new ball is released
    TurnOffPlayfieldLights 'this also turn off extra ball and special lights
    BumperLights 0
    'set up the lights according to the player achievments
    UpdateModeLights
    BonusMultiplier(CurrentPlayer) = 1
    PlayfieldMultiplier(CurrentPlayer) = 1
    Light007.State = 1
    bJackpotsEnabled = False
    If BallsInLock(CurrentPlayer)Then
        li039.State = 2
        bLockEnabled = True
    End If
    BumperHits = 0       'prepare for skillshot
    BumperAward = 5000
    gatekf.RotateToStart 'close the kickback
   'SCOOBY code to play squeaky door open sound when kicback diverter gate is closed
    PlaySound sd_fx_squeaky_door_close 'SCOOBY code play squeaky door close sound when kickback gate is closed
    DOF dof_shaker, DOFPulse 'SCOOBY code pulse the shaker motor when kickback gate is closed.
    leftoutlane.Enabled = 1
    If bFatesStarted Then
        bFatesStarted = False
        FatesHits(CurrentPlayer) = 0
        FatesMultiplier = 1
    End If
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub BumperLights(stat)
    Dim x
    For each x in aBumperLights
        x.State = stat
    Next
End Sub

Sub TurnOffXlights 'turn off the other lights after selecting a X light
    If li024.State = 2 Then li024.State = 0
    If li025.State = 2 Then li025.State = 0
    If li026.State = 2 Then li026.State = 0
    If li027.State = 2 Then li027.State = 0
    If li028.State = 2 Then li028.State = 0
End Sub

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
    LightSeqSkillshot.Play SeqAllOff
    DMD CL("HIT LIT LIGHT"), CL("FOR SKILLSHOT"), "", eNone, eNone, eNone, 3000, True, ""
    li044.State = 2
    li040.State = 2
    BumperLights 2            'blinking
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    bRotateLights = True
    LightSeqSkillshot.StopPlay
    li044.State = 0
    li040.State = 0
    BumperLights 1 'on
    DMDScoreNow
End Sub

Sub CheckSkillshot
    If bSkillShotReady Then
        If BumperHits >= 3 Then
            AwardSkillshot
        End If
    End If
End Sub

Sub CheckSuperSkillshot
    If bSkillShotReady Then
        If LastSwitchHit = "hurrican" Then
            AwardSuperSkillshot
        End If
    End If
End Sub

Sub CheckFreakySkillshot
    If bSkillShotReady Then
        If LastSwitchHit = "swPlungerRest" Then
            AwardFreakySkillshot
        End If
    End If
End Sub

'********************
' Flasher light seq.
'********************

Sub RBF 'right bottom Flasher
    LightSeqRBF.Play SeqBlinking, , 8, 40
    DOF 301, DOFPulse
End Sub

Sub RMF
    LightSeqRMF.Play SeqBlinking, , 8, 40
    DOF 304, DOFPulse
End Sub

Sub RTF
    LightSeqRTF.Play SeqBlinking, , 8, 40
    DOF 307, DOFPulse
End Sub

Sub LBF 'left bottom Flasher
    LightSeqLBF.Play SeqBlinking, , 8, 40
    DOF 310, DOFPulse
End Sub

Sub LMF
    LightSeqLMF.Play SeqBlinking, , 8, 40
    DOF 313, DOFPulse
End Sub

Sub LTF
    LightSeqLTF.Play SeqBlinking, , 8, 40
    DOF 316, DOFPulse
End Sub

Sub HelmetF
    LightSeqHelmet.Play SeqBlinking, , 8, 40
    DOF 319, DOFPulse
End Sub

Sub ZeusF
    LightSeqZeusF.Play SeqRandom, 1, , 1500
    DOF 322, DOFPulse
End Sub

Sub FlashEffect(n)
    Select Case n
        Case 1 'all blink
            LightSeqRBF.Play SeqBlinking, , 8, 40:DOF 301, DOFPulse
            LightSeqRMF.Play SeqBlinking, , 8, 40:DOF 304, DOFPulse
            LightSeqRTF.Play SeqBlinking, , 8, 40:DOF 307, DOFPulse
            LightSeqLBF.Play SeqBlinking, , 8, 40:DOF 310, DOFPulse
            LightSeqLMF.Play SeqBlinking, , 8, 40:DOF 313, DOFPulse
            LightSeqLTF.Play SeqBlinking, , 8, 40:DOF 316, DOFPulse
            LightSeqHelmet.Play SeqBlinking, , 8, 40:DOF 319, DOFPulse
        Case 2 'random
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqRBF.Play SeqBlinking, , 5, 40: DOF 302, DOFPulse '"
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqRMF.Play SeqBlinking, , 5, 40: DOF 305, DOFPulse '"
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqRTF.Play SeqBlinking, , 5, 40: DOF 308, DOFPulse '"
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqLBF.Play SeqBlinking, , 5, 40: DOF 311, DOFPulse '"
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqLMF.Play SeqBlinking, , 5, 40: DOF 314, DOFPulse '"
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqLTF.Play SeqBlinking, , 5, 40: DOF 317, DOFPulse '"
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqHelmet.Play SeqBlinking, , 5, 40: DOF 320, DOFPulse '"
        Case 3 'all blink fast
            LightSeqRBF.Play SeqBlinking, , 4, 30:DOF 302, DOFPulse
            LightSeqRMF.Play SeqBlinking, , 4, 30:DOF 305, DOFPulse
            LightSeqRTF.Play SeqBlinking, , 4, 30:DOF 308, DOFPulse
            LightSeqLBF.Play SeqBlinking, , 4, 30:DOF 311, DOFPulse
            LightSeqLMF.Play SeqBlinking, , 4, 30:DOF 314, DOFPulse
            LightSeqLTF.Play SeqBlinking, , 4, 30:DOF 317, DOFPulse
            LightSeqHelmet.Play SeqBlinking, , 4, 30:DOF 320, DOFPulse
        Case 4 'center
            vpmtimer.addtimer 800, "LightSeqRBF.Play SeqBlinking, , 6, 30: DOF 302, DOFPulse '"
            vpmtimer.addtimer 400, "LightSeqRMF.Play SeqBlinking, , 6, 30: DOF 305, DOFPulse '"
            vpmtimer.addtimer 800, "LightSeqRTF.Play SeqBlinking, , 6, 30: DOF 308, DOFPulse '"
            vpmtimer.addtimer 800, "LightSeqLBF.Play SeqBlinking, , 6, 30: DOF 311, DOFPulse '"
            vpmtimer.addtimer 400, "LightSeqLMF.Play SeqBlinking, , 6, 30: DOF 314, DOFPulse '"
            vpmtimer.addtimer 800, "LightSeqLTF.Play SeqBlinking, , 6, 30: DOF 317, DOFPulse '"
            LightSeqHelmet.Play SeqBlinking, , 6, 30:DOF 320, DOFPulse
        Case 5 'top down
            vpmtimer.addtimer 200, "LightSeqRBF.Play SeqBlinking, , 2, 40: DOF 303, DOFPulse '"
            vpmtimer.addtimer 100, "LightSeqRMF.Play SeqBlinking, , 2, 40: DOF 306, DOFPulse '"
            LightSeqRTF.Play SeqBlinking, , 2, 40:DOF 309, DOFPulse
            vpmtimer.addtimer 200, "LightSeqLBF.Play SeqBlinking, , 2, 40: DOF 312, DOFPulse '"
            vpmtimer.addtimer 100, "LightSeqLMF.Play SeqBlinking, , 2, 40: DOF 315, DOFPulse '"
            LightSeqLTF.Play SeqBlinking, , 2, 40:DOF 318, DOFPulse
            vpmtimer.addtimer 50, "LightSeqHelmet.Play SeqBlinking, , 2, 40: DOF 321, DOFPulse '"
        Case 6 'down to top
            LightSeqRBF.Play SeqBlinking, , 2, 40:DOF 303, DOFPulse
            vpmtimer.addtimer 100, "LightSeqRMF.Play SeqBlinking, , 2, 40: DOF 306, DOFPulse '"
            vpmtimer.addtimer 200, "LightSeqRTF.Play SeqBlinking, , 2, 40: DOF 309, DOFPulse '"
            LightSeqLBF.Play SeqBlinking, , 2, 40:DOF 312, DOFPulse
            vpmtimer.addtimer 100, "LightSeqLMF.Play SeqBlinking, ,2, 40: DOF 315, DOFPulse '"
            vpmtimer.addtimer 200, "LightSeqLTF.Play SeqBlinking, , 2, 40: DOF 318, DOFPulse '"
            vpmtimer.addtimer 150, "LightSeqHelmet.Play SeqBlinking, , 2, 40: DOF 321, DOFPulse '"
        Case 7 'circle 2 rounds
            vpmtimer.addtimer 250, "LightSeqRBF.Play SeqBlinking, , 1, 40: DOF 303, DOFPulse '"
            vpmtimer.addtimer 200, "LightSeqRMF.Play SeqBlinking, , 1, 40: DOF 306, DOFPulse '"
            vpmtimer.addtimer 150, "LightSeqRTF.Play SeqBlinking, , 1, 40: DOF 309, DOFPulse '"
            LightSeqLBF.Play SeqBlinking, , 1, 40:DOF 312, DOFPulse
            vpmtimer.addtimer 50, "LightSeqLMF.Play SeqBlinking, , 1, 40: DOF 315, DOFPulse '"
            vpmtimer.addtimer 100, "LightSeqLTF.Play SeqBlinking, , 1, 40: DOF 318, DOFPulse '"
            vpmtimer.addtimer 550, "LightSeqRBF.Play SeqBlinking, , 1, 40: DOF 303, DOFPulse '"
            vpmtimer.addtimer 500, "LightSeqRMF.Play SeqBlinking, , 1, 40: DOF 306, DOFPulse '"
            vpmtimer.addtimer 450, "LightSeqRTF.Play SeqBlinking, , 1, 40: DOF 309, DOFPulse '"
            vpmtimer.addtimer 300, "LightSeqLBF.Play SeqBlinking, , 1, 40: DOF 312, DOFPulse '"
            vpmtimer.addtimer 350, "LightSeqLMF.Play SeqBlinking, , 1, 40: DOF 315, DOFPulse '"
            vpmtimer.addtimer 400, "LightSeqLTF.Play SeqBlinking, , 1, 40: DOF 318, DOFPulse '"
    End Select
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************

'*********************************************************
' Slingshots has been hit
' In this table the slingshots change the outlanes lights

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    'WOOLY PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Lemk
    PlaySoundAt SoundFXDOF("fx_slingshot", dof_slingshot_left, DOFPulse, DOFcontactors), Lemk 'SCOOBY code
    DOF 144, DOFPulse
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing004.Visible = 0:LeftSLing003.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing003.Visible = 0:LeftSLing002.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing002.Visible = 0:Lemk.RotX = -20:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    'WOOLY PlaySoundAt SoundFXDOF("fx_slingshot", 104, DOFPulse, DOFcontactors), Remk
    PlaySoundAt SoundFXDOF("fx_slingshot", dof_slingshot_right, DOFPulse, DOFcontactors), Remk
    DOF 145, DOFPulse
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:Remk.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub SlingTimer_Timer
    Select case SlingCount
        Case 0, 2, 4, 6, 8:Controller.B2SSetData 10, 1
        Case 1, 3, 5, 7, 9:Controller.B2SSetData 10, 0
        Case 10:SlingTimer.Enabled = 0
    End Select
    SlingCount = SlingCount + 1
End Sub

'****************************
'        Bumper - Pop Bumpers
'****************************

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    Dim tmp
    'WOOLY PlaySoundAt SoundFXDOF("fx_bumper", 105, DOFPulse, DOFContactors), Bumper1
    PlaySoundAt SoundFXDOF("fx_bumper", dof_popbumper_left, DOFPulse, DOFContactors), Bumper1 'SCOOBY code Left Pop Bumper
    DOF 147, DOFPulse
    BumperHits = BumperHits + 1
    AddScore BumperAward
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper1"
    'checkmodes this switch is part of
    CheckSkillshot
    CheckBumperHits
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    Dim tmp
    'WOOLY PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), Bumper2
    PlaySoundAt SoundFXDOF("fx_bumper", dof_popbumper_right, DOFPulse, DOFContactors), Bumper2
    DOF 147, DOFPulse
    BumperHits = BumperHits + 1
    AddScore BumperAward
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper2"
    'checkmodes this switch is part of
    CheckSkillshot
    CheckBumperHits
End Sub

Sub CheckBumperHits
    If BumperHits MOD 20 = 0 Then 'add a ball if in multiball Mode
        If bMultiBallMode Then
            DMD "_", CL("ADD A BALL"), "_", eNone, eNone, eNone, 1500, True, ""
            AddMultiball 1
        End If
    End If
    If BumperHits MOD 25 = 0 Then 'activate chain lightning, bumpers score 4x
        BumperAward = BumperAward * 4
        'WOOLY DMD CL("CHAIN LIGHTNING"), CL("BUMPER VALUE " &FormatScore(BumperAward)), "_", eNone, eNone, eNone, 3000, True, "vo_chain_lightning"
        DMD CL("LIGHTNING STORM"), CL("BUMPER VALUE " & FormatScore(BumperAward)), "_", eNone, eNone, eNone, 3000, True, vo_chain_lightning
        'SCOOBY code JAC be sure to add a smoking fast audio music track for this mode !!!
        FlashEffect RndNbr(7)
        LightSeqBumpers.Play SeqRandom, 10, , 1000
    End If
End Sub

Sub LightSeqBumpers_PlayDone()
    LightSeqBumpers.Play SeqRandom, 10, , 1000
End Sub
'*********
' Lanes
'*********
' in and outlanes
Sub leftoutlane_Hit
    PLaySoundAt "fx_sensor", leftoutlane
    If Tilted Then Exit Sub
    'score & bonus
    AddScore 250000
    If activeball.VelY > 0 Then 'only when going down
        If li002.State Then     'if the skull is lit then reduce the ball saver time
            'WOOLY PlaySound "vo_outlane"
            PlaySound vo_outlane 'SCOOBY sound mod "Redbeard will make pirate stew out of you!"
            BallSaverTime = BallSaverTime -10
        End If
    End If
    LastSwitchHit = "leftoutlane"
End Sub

Sub leftinlane_Hit
    PLaySoundAt "fx_sensor", leftinlane
    If Tilted Then Exit Sub
    'score & bonus
    AddScore 100000
    If li003.State = 0 Then 'count the trees that are being lit up
        li003.State = 1
        TreeHits(CurrentPlayer) = TreeHits(CurrentPlayer) + 1
        CheckTrees
    End If
'LastSwitchHit = "leftinlane"
End Sub

Sub rightinlane_Hit
    PLaySoundAt "fx_sensor", rightinlane
    If Tilted Then Exit Sub
    'score & bonus
    AddScore 100000
    If li004.State = 0 Then 'count the trees that are being lit up
        li004.State = 1
        TreeHits(CurrentPlayer) = TreeHits(CurrentPlayer) + 1
        CheckTrees
    End If
'LastSwitchHit = "rightinlane"
End Sub

Sub rightoutlane_Hit
    PLaySoundAt "fx_sensor", rightoutlane
    If Tilted Then Exit Sub
    'score & bonus
    AddScore 250000
    If li005.State Then 'if the skull is lit then reduce the ball saver time
        'WOOLY PlaySound "vo_outlane"
        PlaySound vo_outlane 'SCOOBY sound mod "Redbeard will make a pirate stew out of you!"
        BallSaverTime = BallSaverTime -10
        DMD CL("BALLSAVE DECREASED"), CL("IT IS NOW " &BallSaverTime& " SEC"), "", eNone, eNone, eNone, 2000, True, ""
    End If
    LastSwitchHit = "rightoutlane"
End Sub

Sub CheckTrees                               'check for 6 treehits and if all the lights are lit
    If TreeHits(CurrentPlayer)MOD 6 = 0 Then 'every 6 trees adds 3 seconds to the ball saver value
        If BallSaverTime < 50 then
            BallSaverTime = BallSaverTime + 3
            DMD CL("BALLSAVE INCREASED"), CL("IT IS NOW " &BallSaverTime& " SEC"), "", eNone, eNone, eNone, 2000, True, ""
        End If
    End If
    If li002.State + li003.State + li004.State + li005.State = 4 Then 'all lights are lit then turn them off
        li002.State = 0
        li003.State = 0
        li004.State = 0
        li005.State = 0
        LightSeqLanes.Play SeqRandom, 4, , 1000
    End If
End Sub

' loops

Sub hurrican_Hit
    PLaySoundAt "fx_sensor", hurrican
    If Tilted Then Exit Sub
    'score & bonus
    'WOOLY AddScore 100000 * MedussaX
    AddScore 100000 * MedusaX 'SCOOBY code v1.0.1 fixing spelling Medussa to Medusa
    If li024.State = 1 Then
        'WOOLY Addscore 100000 * MedussaX 'double the score
        Addscore 100000 * MedusaX 'double the score SCOOBY code v1.0.1 fixing spealling Medussa to Medusa
    End If
    If li024.State = 2 Then
        li024.State = 1
        TurnOffXlights
    End If
    If li030.State = 0 Then
        li030.State = 1
        CheckBonusX
    End If
    'Modes
    Select Case Mode(CurrentPlayer, 0)
        Case 1:li060.State = 0:CheckWinMode
        Case 4
            If ModeStep = 1 Then
                CheckWinMode
            End If
    End Select
    'Combos
    If activeBall.VelY < 0 Then 'ball going up
        If LastSwitchHit = "upperloop2" OR LastSwitchHit = "upperloop4" Then
            AwardCombo
        Else
            ComboCount = 1
        End If
    End If
    LastSwitchHit = "hurrican"
End Sub

Sub upperloop1_Hit
    PLaySoundAt "fx_sensor", upperloop1
    If Tilted Then Exit Sub
    'score & bonus
    'WOOLY AddScore 100000 * MedussaX
    AddScore 100000 * MedusaX 'SCOOBY code v1.0.1 fixing spelling Medussa to Medusa
    'Modes
    Select Case Mode(CurrentPlayer, 0)
        Case 1:li037.State = 0:CheckWinMode
        Case 4
            If ModeStep = 3 Then
                CheckWinMode
            End If
    End Select
    'Combos
    If activeBall.VelY < 0 Then 'ball going up
        If LastSwitchHit = "upperloop4" OR LastSwitchHit = "hurrican" Then
            AwardCombo
        Else
            ComboCount = 1
        End If
    End If
    LastSwitchHit = "upperloop1"
End Sub

Sub upperloop2_Hit
    PLaySoundAt "fx_sensor", upperloop2
    If Tilted Then Exit Sub
    'score & bonus
    'WOOLY AddScore 100000 * MedussaX
    AddScore 100000 * MedusaX 'SCOOBY code v1.0.1 fixing spelling Medussa to Medusa
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 0:OrbitHits = OrbitHits + 1:CheckStartModes
    End Select
    'Medusa MB
    If bMedusaMBStarted Then
        ExtraBallHits = ExtraBallHits + 1
        CheckExtraBallHits
    End If
    LastSwitchHit = "upperloop2"
End Sub

Sub upperloop3_Hit
    PLaySoundAt "fx_sensor", upperloop3
    If Tilted Then Exit Sub
    'score & bonus
    'WOOLY AddScore 100000 * MedussaX
    AddScore 100000 * MedusaX 'SCOOBY code v1.0.1 
    LastSwitchHit = "upperloop3"
    CheckFreakySkillshot
    'Modes
    Select Case Mode(CurrentPlayer, 0)
        Case 1:li044.State = 0:CheckWinMode
        Case 4
            If ModeStep = 4 Then
                CheckWinMode
            End If
    End Select
End Sub

Sub upperloop4_Hit
    PLaySoundAt "fx_sensor", upperloop4
    If Tilted Then Exit Sub
    'score & bonus
    'WOOLY AddScore 100000 * MedussaX
    AddScore 100000 * MedusaX 'SCOOBY code v1.0.1 fixing spelling Medussa to Medusa
    'Modes
    If li028.State = 1 Then
        Addscore 100000 'double the score
    End If
    If li028.State = 2 Then
        li028.State = 1
        TurnOffXlights
    End If
    If li034.State = 0 Then
        li034.State = 1
        CheckBonusX
    End If
    Select Case Mode(CurrentPlayer, 0)
        Case 1:li041.State = 0:CheckWinMode
        Case 4
            If ModeStep = 2 Then
                CheckWinMode
            End If
    End Select
    'Combos
    If activeBall.VelY < 0 Then 'ball going up
        If LastSwitchHit = "upperloop2" OR LastSwitchHit = "hurrican" Then
            AwardCombo
        Else
            ComboCount = 1
        End If
    End If
    LastSwitchHit = "upperloop4"
End Sub

'ramps completed

'** LEFT RAMP
Sub lramp_Hit
    PLaySoundAt "fx_sensor", lramp
    'PlaySound sd_ramp_sound_left 'Scooby code have the Parrot squawk each time left ramp is hit.
       'SCOOBY code AUDIO RANDOMIZER: 03/28/2022 added randomizer to pick from different audio samples more can be added later in case logic
       SELECT Case RndNbr(3)
              Case 1 PlaySound "sd_fx_parrot" 'Scooby code left ramp is hit Shaggy heading for the hills.
              Case 2 PlaySound "sd_vo_likeparrot" 	'Scooby code sample Shaggy says quit foolin around !!
              Case 3 PlaySound "sd_fx_parrot"
       END SELECT
    If Tilted Then Exit Sub
    'score & bonus
    If Light009.State Then
        AwardExtraBall
    End If
    If bMedusaMBStarted Then
        AddScore 50000
    Else
        AddScore 100000
    End If
    If bJackpotsEnabled Then AwardJackpot
    'Fates
    If bFatesStarted Then 'change the scoring multiplier
        light006.State = 0
        Light007.State = 0
        Light008.State = 0
        Select Case RndNbr(3)
            'SCOOBY code: Random award given from left award FATES RAMP
            Case 1:FatesMultiplier = 0.5:light006.State = 1
            Case 2:FatesMultiplier = 1:light007.State = 1
            Case 3:FatesMultiplier = 2:light008.State = 1
        End Select
    Else
        FatesHits(CurrentPlayer) = FatesHits(CurrentPlayer) + 1
        CheckFates
    End If
    'fire & X lights
    If li025.State = 1 Then
        AddScore 100000
    End If
    If li025.State = 2 Then
        li025.State = 1
        TurnOffXlights
    End If
    If li031.State = 0 Then
        li031.State = 1
        CheckBonusX
    End If
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 0
            RampHits = RampHits + 1:CheckStartModes
        Case 2
            If ModeStep = 1 OR ModeStep = 3 Then
                CheckWinMode
            End If
        Case 3:li036.State = 0:CheckWinMode
        Case 4
            If ModeStep = 2 Then
                CheckWinMode
            End If
        Case 9
            AwardJackpot
    End Select
    'Combos
    If LastSwitchHit = "tramp" OR LastSwitchHit = "rramp" Then
        AwardCombo
    Else
        ComboCount = 1
    End If
    LastSwitchHit = "lramp"
End Sub

'** RIGHT RAMP
Sub rramp_Hit
    PLaySoundAt "fx_sensor", rramp

       'SCOOBY code AUDIO RANDOMIZER: 03/28/2022 added randomizer to pick from different audio samples more can be added later in case logic
       SELECT Case RndNbr(20)
              Case 1 PlaySound "sd_vo_shaggy_hamburger" 	'Scooby code Shaggy finds a hamburger
              Case 2 PlaySound "sd_vo_shaggy_quitfoolin" 	'Scooby code sample Shaggy says quit foolin around !!
              Case 3 PlaySound "sd_vo_daphane_betterluck"
              Case 4 PlaySound "sd_vo_fred_foundaclue"
              Case 5 PlaySound "sd_vo_fred_stopwaiting"
              Case 6 PlaySound "sd_vo_shaggy_zoinks2"
              Case 7 PlaySound "sd_vo_daphane_bardance"
              Case 8 PlaySound "sd_vo_fred_maltshop1"
              Case 9 PlaySound "sd_vo_fred_shortroute"
              Case 10 PlaySound "sd_vo_fred_groovysign"
              Case 11 PlaySound "sd_vo_shaggy_whereareyou"
              Case 12 PlaySound "sd_vo_shaggy_hamsandwich"
              Case 13 PlaySound "sd_vo_phantom1"
              Case 14 PlaySound "sd_vo_phantom2"
              Case 15 PlaySound "sd_vo_scooby_doobie"
              Case 16 PlaySound "sd_vo_daphane_someonecoming"
              Case 17 PlaySound "sd_vo_fred_supersleuth"
              Case 18 PlaySound "sd_vo_whimper1"
              Case 19 PlaySound "sd_scooby_laugh1"
              Case 20 PlaySound " " 'Play nothing on occasion
       END SELECT

    If Tilted Then Exit Sub
    'score & bonus
    CheckSuperSkillshot
    If Light010.State Then
        AwardSpecial
    End If
    If bMedusaMBStarted Then
        AddScore 50000
    Else
        AddScore 100000
    End If
    If bJackpotsEnabled Then AwardJackpot
    If li027.State = 1 Then
        AddScore 100000 'double the score
    End If
    If li027.State = 2 Then
        li027.State = 1
        TurnOffXlights
    End If
    If li033.State = 0 Then
        li033.State = 1
        CheckBonusX
    End If
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 0
            RampHits = RampHits + 1:CheckStartModes
        Case 2
            If ModeStep = 2 OR ModeStep = 4 Then
                CheckWinMode
            End If
        Case 3:li040.State = 0:CheckWinMode
        Case 9
            AwardJackpot
    End Select
    'Combos
    If LastSwitchHit = "tramp" OR LastSwitchHit = "lramp" Then
        AwardCombo
    Else
        ComboCount = 1
    End If
    LastSwitchHit = "rramp"
End Sub

'** TOP RAMP
Sub tramp_Hit
    PLaySoundAt "fx_sensor", tramp

       'SCOOBY code AUDIO RANDOMIZER: 03/28/2022 added randomizer to pick from different audio samples more can be added later in case logic
       SELECT Case RndNbr(3)
              Case 1 PlaySound "sd_fx_batsfly1"
              Case 2 PlaySound "sd_fx_batsfly2"
              Case 3 PlaySound "sd_fx_batsfly3"
       END SELECT

    If Tilted Then Exit Sub
LTF:RTF
'score & bonus
If bMedusaMBStarted Then
    AddScore 250000
Else
    AddScore 500000
End If
    If bJackpotsEnabled Then AwardJackpot
    If bZeusMBStarted Then AwardSuperJackpot
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 0
            RampHits = RampHits + 1:CheckStartModes
        Case 3:li043.State = 0:CheckWinMode
        Case 9
            AwardJackpot
    End Select
    LastSwitchHit = "tramp"
End Sub

'Effect triggers

Sub Trigger001_Hit:LMF:End Sub
Sub Trigger002_Hit:LBF:End Sub
Sub Trigger003_Hit:RMF:End Sub
Sub Trigger004_Hit:RBF:End Sub

Sub Trigger005_Hit 'hidden entrance to start battle from right side ???
    'WOOLY DMD "_", CL("HIDDEN SHOT"), "_", eNone, eBlinkFast, eNone, 1500, True, "vo_og-growl"
    DMD "_", CL("SECRET PASSAGE"), "_", eNone, eBlinkFast, eNone, 1500, True, "sd_fx_bats_fly" 'SCOOBY sound mod, upper hole?
    sd_scoop2_enter = "RIGHT" 'SCOOBY code enter scoop 2 from right side for a custom sound
	'PlaySound "sd_secret_passage_velma" 'SCOOBY sound mod play Velma when entering from right side secret hole. 
    
    HiddenShots(CurrentPlayer) = HiddenShots(CurrentPlayer) + 1
    If HiddenShots(CurrentPlayer)MOD 3 = 0 Then
        AddPlayfieldMultiplier 1
        FlashEffect 7
    End If
    AddScore 10000
End Sub

'***********
' Targets
'***********

' kickback targets
Sub leftkb_Hit 'left lower kickback target
    PLaySoundAtBall SoundFXDOF("fx_Target", 124, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
    'WOOLY V1.0.0 Deactivating-> kickbackHits(CurrentPlayer) = kickbackHits(CurrentPlayer) + 1
    'WOOLY V1.0.0 Deactivating-> Checkkickback
    Select Case Mode(CurrentPlayer, 0)
        Case 6:
            If li015.State Then
                PlayLightning
                GiEffect 3
                li015.State = 0
                AddScore 15000
                CheckWinMode
            End If
        Case Else
             'WOOLY V1.0.0 Deactivating-> FlashForms li015, 1000, 50, 0
             kickbackHits(CurrentPlayer) = kickbackHits(CurrentPlayer) + 1 'JP's Mod for WOOLY v1.0.1
             Checkkickback
    End Select
    LastSwitchHit = "leftkb"
End Sub

Sub rightkb_Hit 'left upper kickback target
    PLaySoundAtBall SoundFXDOF("fx_Target", 125, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
    kickbackHits(CurrentPlayer) = kickbackHits(CurrentPlayer) + 1
    Checkkickback
    Select Case Mode(CurrentPlayer, 0)
        Case 6:
            If li016.State Then
                PlayLightning
                GiEffect 3
                li016.State = 0
                AddScore 15000
                CheckWinMode
            End If
        Case Else
            FlashForms li016, 1000, 50, 0
    End Select
    LastSwitchHit = "rightkb"
End Sub

'Pandora's targets

Sub lmyst_Hit 'right upper pandora target
    PLaySoundAtBall SoundFXDOF("fx_Target", 126, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
    PandorasHits(CurrentPlayer) = PandorasHits(CurrentPlayer) + 1
    CheckPandora
    Select Case Mode(CurrentPlayer, 0)
        Case 6:
            If li018.State Then
                PlayLightning
                GiEffect 3
                li018.State = 0
                AddScore 15000
                CheckWinMode
            End If
        Case Else
            FlashForms li018, 1000, 50, 0
    End Select
    LastSwitchHit = "lmyst"
End Sub

Sub rmyst_Hit 'right lower pandora target
    PLaySoundAtBall SoundFXDOF("fx_Target", 127, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
    PandorasHits(CurrentPlayer) = PandorasHits(CurrentPlayer) + 1
    CheckPandora
    Select Case Mode(CurrentPlayer, 0)
        Case 6:
            If li017.State Then
                PlayLightning
                GiEffect 3
                li017.State = 0
                AddScore 15000
                CheckWinMode
            End If
        Case Else
            FlashForms li017, 1000, 50, 0
    End Select
    LastSwitchHit = "rmyst"
End Sub

' captive ball
Sub cball_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 119, DOFPulse, DOFTargets)
    PlaySound sd_fx_captiveball
    If Tilted Then Exit Sub
    Addscore 75000
    LightEffect 3
    PlaySound sd_vo_whats_in_here
    'Modes
    CBHits(CurrentPlayer) = CBHits(CurrentPlayer) + 1
    'mrjcrane cant figure out how to make this work to display the current HIT COUNT 
    DMD CL("CAPT BALL HIT COUNT"), CL("COUNT IS " & CBHits(CurrentPlayer)), "_", eNone, eNone, eNone, 3000, True, ""
    If CBHits(CurrentPlayer)MOD 10 = 0 Then 'open trapdoor for jackpot
       'WOOLY vpmTimer.AddTimer 1500, "PLaySound""vo_trapdoor"":TrapdoorUp '"
      'PlaySound "sd_fx_trapdoor_up" 'SCOOBY code
       vpmTimer.AddTimer 1500, "PlaySound""vo_trapdoor_sd"":TrapdoorUp '" 'SCOOBY code

  End If
    If CBHits(CurrentPlayer)MOD 25 = 0 Then 'ball breaker
        'WOOLY DMD CL("BALL BREAKER"), CL(FormatScore(750000)), "_", eNone, eNone, eNone, 3000, True, "vo_ball_breaker"
        DMD CL("SUPER SLEUTH 25 HITS"), CL(FormatScore(750000)), "_", eNone, eNone, eNone, 3000, True, sd_super_sleuth
        Addscore2 750000
        li045.State = 1
    End If
    LastSwitchHit = "cball"
End Sub

' thin targets
Sub liup1_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 142, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
    Select Case Mode(CurrentPlayer, 0)
        Case 6:
            If li019.State Then
                PlayLightning
                GiEffect 3
                li019.State = 0
                AddScore 15000
                CheckWinMode
            End If
        Case Else
            If li019.State = 0 Then
                PlayLightning
                ZeusF
                li019.State = 1
                CheckGodMode
            End If
    End Select
    LastSwitchHit = "liup1"
End Sub

Sub liup2_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 142, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
    Select Case Mode(CurrentPlayer, 0)
        Case 6:
            If li020.State Then
                PlayLightning
                GiEffect 3
                li020.State = 0
                AddScore 15000
                CheckWinMode
            End If
        Case Else
            If li020.State = 0 Then
                PlayLightning
                ZeusF
                li020.State = 1
                CheckGodMode
            End If
    End Select
    LastSwitchHit = "liup2"
End Sub

Sub liup3_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 142, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
    Select Case Mode(CurrentPlayer, 0)
        Case 6:
            If li021.State Then
                PlayLightning
                GiEffect 3
                li021.State = 0
                AddScore 15000
                CheckWinMode
            End If
        Case Else
            If li021.State = 0 Then
                PlayLightning
                ZeusF
                li021.State = 1
                CheckGodMode
            End If
    End Select
    LastSwitchHit = "liup3"
End Sub

Sub liup4_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 142, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
    Select Case Mode(CurrentPlayer, 0)
        Case 6:
            If li022.State Then
                PlayLightning
                GiEffect 3
                li022.State = 0
                AddScore 15000
                CheckWinMode
            End If
        Case Else
            If li022.State = 0 Then
                PlayLightning
                ZeusF
                li022.State = 1
                CheckGodMode
            End If
    End Select
    LastSwitchHit = "liup4"
End Sub

Sub liup5_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 143, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
    Select Case Mode(CurrentPlayer, 0)
        Case 6:
            If li023.State Then
                PlayLightning
                GiEffect 3
                li023.State = 0
                AddScore 15000
                CheckWinMode
            End If
        Case Else
            If li023.State = 0 Then
                PlayLightning
                ZeusF
                li023.State = 1
                CheckGodMode
            End If
    End Select
    LastSwitchHit = "liup5"
End Sub

Sub TargetLightsAll(stat) 'same as light.state
    For each x in aTargetsAll
        x.State = stat
    Next
End Sub

Sub TargetLightsThunder(stat) 'same as light.state
    For each x in aTargetsThunder
        x.State = stat
    Next
End Sub

' Zeus targets
Sub ztgt_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 133, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 50000
    li061.State = 1
    ZeusF
    CheckStartModes
    LastSwitchHit = "ztgt"
End Sub

Sub etgt_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 134, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 50000
    li062.State = 1
    ZeusF
    CheckStartModes
    LastSwitchHit = "etgt"
End Sub

Sub utgt_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 135, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 50000
    li063.State = 1
    ZeusF
    CheckStartModes
    LastSwitchHit = "utgt"
End Sub

Sub stgt_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 136, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 50000
    li064.State = 1
    ZeusF
    CheckStartModes
    LastSwitchHit = "stgt"
End Sub

'*************
'  Spinners
'*************

Sub rspin_Spin 'right
    'WOOLY PlaySoundAt "fx_spinner", rspin
    PlaySoundAt sd_fx_spinner_right, rspin 'SCOOBY code play tone A4 instead of mechanical click
    'WOOLY DOF 129, DOFPulse
    DOF dof_spinner_right, DOFPulse 'SCOOBY code trigger Shaker Motor from previous variable
    If Tilted Then Exit Sub
    Addscore 10000
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5:SpinnerHits = SpinnerHits + 1:CheckWinMode
    End Select
End Sub

Sub lspin_Spin 'left
    'WOOLY PlaySoundAt "fx_spinner", lspin
    PlaySoundAt sd_fx_spinner_left, lspin 'SCOOBY code play tone C4 instead of mechanical click
    'WOOLY DOF 128, DOFPulse
    DOF dof_spinner_left, DOFPulse 'SCOOBY code trigger Shaker Motor from previous variable
    If Tilted Then Exit Sub
    Addscore 10000
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5:SpinnerHits = SpinnerHits + 1:CheckWinMode
    End Select
End Sub

'*********
' scoops
'*********

Sub scoop1_Hit 'helmet scoop (HOLE2)
    'WOOLY PlaySoundAt "fx_hole_enter", scoop1 play the normal game sound from the line below on scoop enter
    PlaySoundAt fx_hole_enter, scoop1
    scoop1.Destroyball
    BallsinHole = BallsInHole + 1
    If Tilted Then vpmtimer.addtimer 500, "kickBallOut '":Exit Sub
    FlashEffect 7
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    ' Modes
    If bMinotaurMBStarted AND Minotaur1 = 0 Then
        AwardMinotaurJackpot
        Minotaur1 = 1
        CheckMinotaurMBHits
    Else
        If NOT bLockEnabled Then 'this is the first hit so enable the lock and light
            'WOOLY DMD "_", CL("LOCK IS LIT"), "_", eNone, eNone, eNone, 1500, True, ""
            DMD "_", CL("LOCK IS LIT"), "_", eNone, eNone, eNone, 1500, True, "sd_fx_bats_fly" 'SCOOBY sound mod 
            bLockEnabled = True
            li039.State = 2
        Else 'add a ball to the lock
            BallsInLock(CurrentPlayer) = BallsInLock(CurrentPlayer) + 1
            CheckMinotaurMB
        End If
    End If
    Addscore 5000
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 1, 3, 5, 6, 8
            If ModeStep = 2 Then
                WinMode
            Else
                vpmtimer.addtimer 1500, "kickBallOut '"
            End If
        Case 2, 4
            If ModeStep = 4 Then
                WinMode
            Else
                vpmtimer.addtimer 1500, "kickBallOut '"
            End If
        Case Else
            ' Nothing left to do, so kick out the ball
            vpmtimer.addtimer 1500, "kickBallOut '"
    End Select
End Sub

Sub kickBallOut 'from all the holes
    If BallsinHole > 0 Then
        BallsinHole = BallsInHole - 1
        'WOOLY PlaySoundAt SoundFXDOF("fx_popper", 111, DOFPulse, DOFcontactors), scoopexit
        PlaySoundAt SoundFXDOF("fx_popper", dof_popbumper_left, DOFPulse, DOFcontactors), scoopexit
        DOF 130, DOFPulse
        scoopexit.CreateSizedBallWithMass BallSize / 2, BallMass
        scoopexit.kick 196, 28
        HelmetF
        LightEffect 5
        vpmtimer.addtimer 1500, "kickBallOut '" 'kick out the rest of the balls, if any
    End If
End Sub

' hole2 - Start Battle

Sub scoop2_Hit 'Start Battle (This is in the center of the table1) (CENTER HOLE/LANE)
    PlaySoundAt "fx_hole_enter", scoop2
    sd_scoop2_enter = "CENTER"
    IF sd_scoop2_enter = "CENTER" THEN 				'SCOOBY code
       'SCOOBY code .... 03/28/2022 added a randomizer for the center battle hole to pick from 9 different audio samples.
       SELECT Case RndNbr(9)
              Case 1 PlaySound "sd_secret_passage_shaggy"
              Case 2 PlaySound "sd_fx_batsfly1"
              Case 3 PlaySound "sd_dracula1"
              Case 4 PlaySound "sd_dracula2"
              Case 5 PlaySound "sd_dracula3"
              Case 6 PlaySound "sd_gypsy_enjoy"
              Case 7 PlaySound "" 'Play Blank sound just as a filler controlls the ratio of Sound:No-Sound @ 33.3%
              Case 8 PlaySound "" 'Play Blank sound just as a filler controlls the ratio of Sound:No-Sound @ 33.3%
              Case 9 PlaySound "" 'Play Blank sound just as a filler controlls the ratio of Sound:No-Sound @ 33.3%
       END SELECT
    ELSE									'SCOOBY code
       PlaySound "sd_secret_passage_velma"	'SCOOBY code
    END IF
    scoop2.Destroyball
    BallsinHole = BallsInHole + 1
    If Tilted Then vpmtimer.addtimer 500, "kickBallOut '":Exit Sub
    ' Modes
    Addscore 25000
    If li026.State = 1 Then
        AddScore 25000 'double the score
    End If
    If li026.State = 2 Then
        li026.State = 1
        TurnOffXlights
    End If
    If li032.State = 0 Then
        li032.State = 1
        CheckBonusX
    End If
    If bMinotaurMBStarted AND Minotaur2 = 0 Then
        AwardMinotaurJackpot
        Minotaur2 = 1
        CheckMinotaurMBHits
    End If
    If bModeReady Then
        bModeReady = False
        li001.State = 0
        li038.State = 0
        li042.State = 0
        StartNextMode
    Else
        vpmtimer.addtimer 500, "kickBallOut '"
    End If
End Sub

'***********
' Trapdoor
'***********
'hole3 - HadesTrapdoor

Sub TrapDoorK_Hit 'Hades Battle (HOLE3)
    'WOOLY PlaySoundAt "fx_hole_enter", TrapDoorK
    PlaySoundAt "fx_hole_enter", TrapDoorK 'SCOOBY code use WOOLY default sound for now
    TrapDoorK.Destroyball
    BallsinHole = BallsInHole + 1
    If Tilted Then vpmtimer.addtimer 500, "kickBallOut '":Exit Sub
    ' Modes
    If bMinotaurMBStarted AND Minotaur3 = 0 Then
        AwardMinotaurJackpot
        Minotaur3 = 1
        CheckMinotaurMBHits
    End If
    Select Case Mode(CurrentPlayer, 0)
        Case 7
            TrapDoorHits = TrapDoorHits + 1
            AddScore2 500000
            CheckWinMode
        Case Else
            If NOT bMinotaurMBStarted Then
                'score a Jackpot
                AwardJackpot
                ' and close the trap door
                vpmTimer.AddTimer 1000, "TrapdoorDown '"
                PlaySound sd_lower_iron_door 'SCOOBY code
            End If
    End Select
    vpmtimer.addtimer 2000, "kickBallOut '"
End Sub

'SCOOBY Main routine and sound fx for raising and lowering trapdoor
Sub TrapdoorUp
    PlaySoundAt SoundFXDOF("fx_SolenoidOn", 117, DOFPulse, DOFContactors), TrapDoorK
    PlaySound sd_fx_trapdoor_up 'SCOOBY code Drawbridge raising sound fx
    TrapdoorA.IsDropped = 1
    TrapdoorB.IsDropped = 0
    TrapdoorC.IsDropped = 0
    TrapdoorD.Collidable = 0
    TrapDoorK.Enabled = 1
    Light012.State = 2
End Sub

Sub TrapdoorDown
    PlaySoundAt SoundFXDOF("fx_SolenoidOff", 117, DOFPulse, DOFContactors), TrapDoorK
    TrapdoorA.IsDropped = 0
    TrapdoorB.IsDropped = 1
    TrapdoorC.IsDropped = 1
    TrapdoorD.Collidable = 1
    TrapDoorK.Enabled = 0
    Light012.State = 0
End Sub

'***********
' Kick back
'***********

Sub Checkkickback
    'check if enough hits to enable kickback
    If kickbackHits(CurrentPlayer) = kickbackNeeded(CurrentPlayer)Then
        'WOOLY DMD "_", CL("KICKBACK IS KIT"), "", eNone, eNone, eNone, 1500, True, "vo_kickbackislit"
        DMD "_", CL("KICKBACK IS LIT"), "", eNone, eNone, eNone, 1500, True, vo_kickbackislit 'SCOOBY sound mod "Shaggy, Give it again!"
        PlaySoundAt "fx_diverter", gatekf
		'SCOOBY to do add code that turns the kick back target light to the ON position until the kickback is used up.
        gatekf.RotateToEnd
        'SCOOBY code to play squeaky door open sound when kicback diverter gate is opened
        PlaySound "sd_fx_squeaky_door_open"
        DOF dof_shaker, DOFPulse 'SCOOBY code pulse the shaker motor when kickback gate is closed.
        DOF dof_slingshot_left, DOFOn 'SCOOBY code flipe the left slingshot to make some noise when squeaky door is opened
        leftoutlane.Enabled = 0 'disable the outlane kicker so it doesn't remove 10 seconds in case of the skull light being lit.
        LMF:LBF
        'li015.State = 2 'SCOOBY code instructions for leaving the left kickbak hit lights illuminated (JAC Remove later)
    End If
    'SCOOBY folding in JP's mod for illumination on Kickbacks 03/23/2022
    If kickbackHits(CurrentPlayer) >= kickbackNeeded(CurrentPlayer)Then
        FlashForms li015, 1000, 50, 1
        FlashForms li016, 1000, 50, 1
    Else
        FlashForms li015, 1000, 50, 0
        FlashForms li016, 1000, 50, 0
    End If

End Sub

Sub Kickback_Hit
    PlaySoundAt "fx_kicker_enter", Kickback
    vpmtimer.addtimer 2000, "DOF 110, DOFPulse:PlaySoundAt SoundFX(""fx_kicker"",DOFContactors),Kickback:kickback.kick 0,45 '"
    'SCOOBY BUG vpmtimer.addtimer 2000, "dof_shaker, DOFPulse:PlaySoundAt SoundFX(""fx_kicker"",DOFContactors),Kickback:kickback.kick 0,45 '" 'SCOOBY code v1.0
    DOF dof_slingshot_right, DOFPulse 'Scooby code v1.0
	DOF dof_shaker, DOFPulse 'SCOOBY code v1.0.1 pulse the shake motor added shaker as separate line
    vpmtimer.addtimer 2500, "PlaySoundAt""fx_diverter"", gatekf:leftoutlane.Enabled =1:gatekf.RotateToStart '"
    kickbackNeeded(CurrentPlayer) = kickbackNeeded(CurrentPlayer) + 2
    kickbackHits(CurrentPlayer) = 0
    'SCOOBY to do add code here that turns off the 2 illuminated kickback lights
End Sub

'******************************
' Pandora's Box - Extra awards
'******************************
'hole4 - Pandora's box

Sub pandorak_Hit
    PlaySoundAt "fx_hole_enter", pandorak
    PlaySound   "sd_pirates_chest" 'SCOOBY sound mod Velma says "It's a pireates chest"
    pandorak.Destroyball
    BallsinHole = BallsInHole + 1
    If Tilted Then vpmtimer.addtimer 500, "kickBallOut '":Exit Sub
    RMF:RBF
If li029.State Then
    StartPandora
    PandorasHits(CurrentPlayer) = 0
    If PandorasNeeded(CurrentPlayer) < 6 Then
        PandorasNeeded(CurrentPlayer) = PandorasNeeded(CurrentPlayer) + 2
    End If
    li029.State = 0
Else
    vpmtimer.addtimer 1500, "kickBallOut '"
End If
End Sub

Sub CheckPandora
    'check if enough hits to light Pandora's light
    If PandorasHits(CurrentPlayer) = PandorasNeeded(CurrentPlayer)Then
        'WOOLY DMD "_"	, CL("PANDORAS BOX IS LIT"), "", eNone, eNone, eNone, 1500, True, "vo_mystery"
		DMD "_"			, CL("PIRATES CHEST LIT"), "", eNone, eNone, eNone, 1500, True, vo_mystery 'SCOOBY code dmd Fed says "Let's see what's in it"
        li029.State = 1
    End If
    'SCOOBY folding in JP's mod for illumination on Pandora Hits 03/23/2022
    If PandorasHits(CurrentPlayer) >= PandorasNeeded(CurrentPlayer)Then
       FlashForms li018, 1000, 50, 1
       FlashForms li017, 1000, 50, 1
    Else
       FlashForms li018, 1000, 50, 0
       FlashForms li017, 1000, 50, 0
    End If
End Sub

Sub StartPandora
    'do some animation
    DMDFlush
    'WOOLY DMD CL("PANDORAS BOX"), CL("LIGHT EXTRABALL"), "", eNone, eNone, eNone, 200, False, ""
    'WOOLY DMD "_", CL("1 MILLION"), "", eNone, eNone, eNone, 200, False, ""
    'WOOLY DMD "_", CL("LIT SPECIAL"), "", eNone, eNone, eNone, 200, False, ""
    'WOOLY DMD "_", CL("5 MILLION"), "", eNone, eNone, eNone, 200, False, ""
    'WOOLY DMD "_", CL("START MODE"), "", eNone, eNone, eNone, 200, False, ""

'SCOOBY AWARDS Pandora's Box & Pirates Chest announcement
    'WOOLY DMD CL("PANDORAS BOX"), CL("LIGHT EXTRABALL"), "", eNone, eNone, eNone, 200, False, ""
    DMD CL("PIRATES CHEST"), CL("LIGHT EXTRABALL"), "", eNone, eNone, eNone, 200, False, ""
    DMD "_", CL("1 MILLION"), "", eNone, eNone, eNone, 200, False, ""
    DMD "_", CL("5 MILLION"), "", eNone, eNone, eNone, 200, False, ""
    DMD "_", CL("LIT SPECIAL"), "", eNone, eNone, eNone, 200, False, ""
    DMD "_", CL("START MODE"), "", eNone, eNone, eNone, 200, False, ""

    'give award 'SCOOBY codes special award announcments
    Dim tmp
	'JAC to do need to rewpork next section PANDORA to PIRATE
    Select case RndNbr(20)
        Case 1, 8 'light extraball
            If Light009.State = 0 Then
                'WOOLY DMD CL("PANDORAS BOX"), CL("EXTRA BALL IS LIT"), "_", eNone, eBlink, eNone, 2500, True, "vo_extraballislit"
                DMD CL("PIRATES CHEST"), CL("EXTRA BALL IS LIT"), "_", eNone, eBlink, eNone, 2500, True, "vo_extraballislit" ' SCOOBY code
                Light009.State = 2
            Else
                'WOOLY DMD CL("PANDORAS BOX"), CL(FormatScore(250000)), "_", eNone, eBlink, eNone, 2000, True, ""
                DMD CL("PIRATES CHEST"), CL(FormatScore(250000)), "_", eNone, eBlink, eNone, 2000, True, "" 'SCOOBY code
                Addscore 250000
            End If
        Case 2, 9 'light special
            If Light010.State = 0 Then
                ' WOOLU DMD CL("PANDORAS BOX"), CL("SPECIAL IS LIT"), "_", eNone, eBlink, eNone, 2500, True, "vo_specialislit"
                 DMD CL("PIRATES CHEST"), CL("SPECIAL IS LIT"), "_", eNone, eBlink, eNone, 2500, True, "vo_specialislit" 'SCOOBY code
                Light010.State = 2
            Else
                'WOOLY DMD CL("PANDORAS BOX"), CL(FormatScore(250000)), "_", eNone, eBlink, eNone, 2000, True, ""
                DMD CL("PIRATES CHEST"), CL(FormatScore(250000)), "_", eNone, eBlink, eNone, 2000, True, "" 'SCOOBY code
                Addscore2 250000
            End If
        Case 3, 10 'start 3 balls multiball, double playfield scores
            'WOOLY DMD CL("PANDORAS BOX"), CL("MULTIBALL"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_multiball"
            DMD CL("PIRATES"), CL("MULTIBALL"), "_", eNone, eBlinkFast, eNone, 1000, True, sd_vo_pirate_multiball 'SCOOBY code
            'WOOLY DMD "_", CL("PLAYFIELD X 2"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            DMD "_", CL("PLAYFIELD X 2"), "d_sd_pirate", eNone, eBlinkFast, eNone, 2000, True, ""
            AddPlayfieldMultiplier 1
            AddMultiball 2
        Case 4, 11 'award from 250k to 5 million
            tmp = 250000 * RndNbr(20)
            'DMD CL("PANDORAS BOX"), CL(FormatScore(tmp)), "_", eNone, eBlink, eNone, 2000, True, ""
            DMD CL("PIRATES CHEST"), CL(FormatScore(tmp)), "_", eNone, eBlink, eNone, 2000, True, sd_award_1_million 'SCOOBY code
            Addscore2 tmp
        Case 5, 12 'increment bonus multiplier
            AddBonusMultiplier 1
            'WOOLY DMD CL("PANDORAS BOX"), CL("BONUS X " & BonusMultiplier(CurrentPlayer)), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            DMD CL("PIRATES CHEST"), CL("BONUS X " & BonusMultiplier(CurrentPlayer)), "_", eNone, eBlinkFast, eNone, 2000, True, sd_award_bonus 'SCOOBY code dmd & sound
        Case 6, 13 'add 10 pop bumper values
            BumperAward = BumperAward * 10
            'WOOLY DMD CL("PANDORAS BOX"), CL("BUMPERS 2X VALUE"), "_", eNone, eNone, eNone, 1000, True, ""
            DMD CL("PIRATES CHEST"), CL("BUMPERS 2X VALUE"), "_", eNone, eNone, eNone, 1000, True, sd_award_bumper_multiplier 'SCOOBY code dmd & sound
            DMD CL("BUMPERS VALUE"), CL(FormatScore(BumperAward)), "_", eNone, eNone, eNone, 1500, True, ""
        Case 7, 14 '20 or more seconds ball saver
            StartGodMode
        Case Else  'hahaha just from 1000 to 25000 points
            tmp = 1000 * RndNbr(25)
            'WOOLY DMD CL("PANDORAS BOX"), CL(FormatScore(tmp)), "_", eNone, eBlink, eNone, 2000, True, "vo_evilmuah"
            DMD CL("PIRATES CHEST"), CL(FormatScore(tmp)), "_", eNone, eBlink, eNone, 2000, True, sd_award_points 'SCOOBY code dmd & sound
            Addscore2 tmp
    End Select
    vpmtimer.addtimer 4500, "kickBallOut '"
End Sub

Sub StartGodMode
    ' SetB2SData 6, 1 'backglass gods flashing
    ' WOOLY DMD CL("GOD MODE"), CL("FOR " &BallSaverTime& " SECONDS"), "_", eNone, eNone, eNone, 2000, True, "vo_godmode"
	' SCOOBY notes: God mode just adds more time to the ball saver
    DMD CL("SCOOBY SNACK"), CL("FOR " &BallSaverTime& " SECONDS"), "_", eNone, eNone, eNone, 2000, True, vo_godmode 'SCOOBY Scooby snack
    EnableBallSaver BallSaverTime
    PlayThunder
    FlashEffect RndNbr(7)
End Sub

Sub CheckGodMode
    If li019.State + li020.State + li021.State + li022.State + li023.State = 5 Then
        GodModeHits(CurrentPlayer) = GodModeHits(CurrentPlayer) + 1
        If GodModeHits(CurrentPlayer) = GodModeNeeded(CurrentPlayer)Then
            StartGodMode
            GodModeHits(CurrentPlayer) = 0
            GodModeNeeded(CurrentPlayer) = GodModeNeeded(CurrentPlayer) + 1 'increase the number of times needed to start God Mode
        End If
        li019.State = 0
        li020.State = 0
        li021.State = 0
        li022.State = 0
        li023.State = 0
        LightEffect 2
    End If
End Sub

'***********************************
' Modes - Monsters and Gods battles
'***********************************
' only one mode can be played at one time
' This table has 8 modes or battles, with a nr. 9 being the end Wizard mode

Sub CheckStartModes 'check for the requirements to start a new mode, and activate the "start battle" light/scoop
    Dim tmp
    tmp = li061.State + li062.State + li063.State + li064.State
    Select Case Mode(CurrentPlayer, 0)
        Case 0 'no battle is active
            'are all zeus targets lit? Or there is enough ramp/orbits hits to start a new battle?
            If tmp = 4 OR(OrbitHits + RampHits = 4)Then
                'WOOLY DMD "_", CL("START BATTLE IS LIT"), "_", eNone, eBlink, eNone, 2500, True, ""
                DMD "_", CL("START CAPTURE IS LIT"), "_", eNone, eBlink, eNone, 2500, True, sd_ghost_mode_start 'Scooby sound mod
                bModeReady = True
                li001.State = 2 'the start battle lights
                li038.State = 2
                li042.State = 2
            End If
            'reset the zeus lights and count the number of times they have been lit to start the Zeus multiball
            If tmp = 4 Then
                AddPlayfieldMultiplier 1
                li061.State = 0
                li062.State = 0
                li063.State = 0
                li064.State = 0
                ZeusCount(CurrentPlayer) = ZeusCount(CurrentPlayer) + 1
                'is zeus multiball ready?
                If ZeusCount(CurrentPlayer)MOD 3 = 0 then 'start zeus multiball
                    StartZeusMB
                End If
            End If
        Case 8 'Zeus battle
            If tmp = 4 Then
                AddPlayfieldMultiplier 1
                li061.State = 0
                li062.State = 0
                li063.State = 0
                li064.State = 0
                ZeusTargetsCompleted = ZeusTargetsCompleted + 1
                If Light009.State = 0 Then
                    DMD "_", CL("EXTRA BALL IS LIT"), "_", eNone, eBlink, eNone, 2500, True, "vo_extraballislit"
                    Light009.State = 2
                End If
                CheckWinMode
            End If
        Case Else 'check also for Zeus multiball during other battles
            'reset the zeus lights and count the number of times they have been lit to start the Zeus multiball
            If tmp = 4 Then
                AddPlayfieldMultiplier 1
                li061.State = 0
                li062.State = 0
                li063.State = 0
                li064.State = 0
                ZeusCount(CurrentPlayer) = ZeusCount(CurrentPlayer) + 1
                'is zeus multiball ready?
                If ZeusCount(CurrentPlayer)MOD 3 = 0 then 'start zeus multiball
                    StartZeusMB
                End If
            End If
    End Select
End Sub

Sub StartNextMode 'starts the mode in the Mode(CurrentPlayer, 0) variable
    Dim tmp
    CurrentMode(CurrentPlayer) = CurrentMode(CurrentPlayer) + 1
    Mode(CurrentPlayer, 0) = CurrentMode(CurrentPlayer)
    ChangeSong

'***** START THE 4 MONSTER MODES *****
'** SCOOBY-This is where all the major battle modes are initiated BattleModes and Countdown Timers
    Select Case Mode(CurrentPlayer, 0)
        Case 1 ' Minotaur AKA monster = Elias Kingston
            '120 second timer
            'Must hit following shots:
            'left orbit, right orbit, center orbit, and upper orbit, (in any order) final shot being the center scoop.
            'WOOLY DMD CL("MINOTAUR STARTED"), CL("HIT LIT SHOTS"), "d_minotaur", eNone, eBlink, eNone, 2500, True, "vo_Minotaur-mode"
            DMD CL("KINGSTON STARTED"), CL("HIT LIT SHOTS"), "d_minotaur_alt", eNone, eBlink, eNone, 2500, True, vo_Minotaur_mode 'SCOOBY code
            li013.BlinkInterval = 100
            Mode(CurrentPlayer, 1) = 2
            UpdateModeLights
            EndModeCountdown = 120
            EndModeTimer.Enabled = 1
            ModeStep = 1
            'arrows used in the mode
            li060.State = 2
            li041.State = 2
            li037.State = 2
            li044.State = 2
            ChangeGi red
            ChangeGIIntensity 1.6
        Case 2 ' Hydra AKA monster = MonoTikiTia
            '120 second timer
            'Must hit following shots:
            'left ramp, right ramp, left ramp, right ramp, final shot being the center scoop.
            'WOOLY DMD CL("HYDRA STARTED"), CL("HIT LIT SHOTS"), "d_hydra", eNone, eBlink, eNone, 2500, True, "vo_Hydra-mode"
            DMD CL("MONO TIKI STARTED"), CL("HIT LIT SHOTS"), "d_hydra_alt", eNone, eBlink, eNone, 2500, True, vo_Hydra_mode
            li011.BlinkInterval = 100
            Mode(CurrentPlayer, 2) = 2
            UpdateModeLights
            EndModeCountdown = 120
            EndModeTimer.Enabled = 1
            ModeStep = 1
            'arrows used in the mode
            li036.State = 2
            ChangeGi green
            ChangeGIIntensity 1.6
        Case 3 ' Cerberus AKA monster = Captain Cutler
'120 second timer
'Must hit following shots:  left ramp, right ramp, and upper ramp, (any order) final shot being the center scoop.
            'WOOLY DMD CL("CERBERUS STARTED"), CL("HIT LIT SHOTS"), "d_cerberus", eNone, eBlink, eNone, 2500, True, "vo_Cerberus-mode"
            DMD CL("CPTN CUTLER STARTED"), CL("HIT LIT SHOTS"), "d_cerberus_alt", eNone, eBlink, eNone, 2500, True, vo_Cerberus_mode
			li012.BlinkInterval = 100
            Mode(CurrentPlayer, 3) = 2
            UpdateModeLights
            EndModeCountdown = 120
            EndModeTimer.Enabled = 1
            ModeStep = 1
            'arrows used in the mode
            li036.State = 2
            li040.State = 2
            li043.State = 2
            ChangeGi red
            ChangeGIIntensity 1.6

'***** START THE 4 GHOST MODES *****
        Case 4 ' Medusa
'120 second timer
'Must hit following shots:  left orbit, right orbit, center orbit, and upper orbit, final shot being the center scoop.
'Completing the mode awards a five ball Medusa Multi-ball. 15 seconds ball saver.
            'WOOLY DMD CL("MEDUSA STARTED"), CL("HIT LIT SHOTS"), "d_medusa", eNone, eBlink, eNone, 2500, True, "vo_Medusa-mode"
            DMD CL("GHOST CLOWN STARTED"), CL("HIT LIT SHOTS"), "d_medusa_alt", eNone, eBlink, eNone, 2500, True, vo_Medusa_mode
            li014.BlinkInterval = 100
            Mode(CurrentPlayer, 4) = 2
            UpdateModeLights
            EndModeCountdown = 120
            EndModeTimer.Enabled = 1
            ModeStep = 1
            'arrows used in the mode
            li060.State = 2
            ChangeGi green
            ChangeGIIntensity 1.6
        Case 5 ' Ares
		'SCOOBY code this is The Green Ghost
            '120 second timer
            'Spinners: collect 50 spinners and hit the scoop to finish.
            'check for extra jackpots
            If li014.State = 1 AND li012.State = 1 Then 'turn on jackpots at the ramps
                DMD "JACKPOTS ARE ENABLED", "HIT RAMPS TO COLLECT", "_", eNone, eNone, eNone, 2500, True, " "
                li043.State = 2
                li036.State = 2
                li040.State = 2
                bJackpotsEnabled = True
            End If
            'WOOLY DMD CL("ARES STARTED"), CL("HIT THE SPINNERS"), "d_ares", eNone, eBlink, eNone, 2500, True, "vo_Ares-mode"
            DMD CL("GREEN GHOST STARTED"), CL("HIT THE SPINNERS"), "d_ares_alt", eNone, eBlink, eNone, 2500, True, vo_Ares_mode
            li009.BlinkInterval = 100
            Mode(CurrentPlayer, 5) = 2
            UpdateModeLights
            EndModeCountdown = 120
            EndModeTimer.Enabled = 1
            ModeStep = 1
            SpinnerHits = 0
            'arrows used in the mode
            li037.State = 2
            li041.State = 2
            ChangeGi red
            ChangeGIIntensity 1.6
        Case 6 ' Poseidon
		'SCOOBY code this is Frankenstein
            '120 second timer
            'Must hit all the lit targets and finish at the scoop.
            'check for extra ball
            If li013.State = 1 AND li011.State = 1 Then
                If Light009.State = 0 Then
                    DMD "_", CL("EXTRA BALL IS LIT"), "_", eNone, eBlink, eNone, 2500, True, "vo_extraballislit"
                    Light009.State = 2
                End If
            End If
            If bJackpotsEnabled Then
                bJackpotsEnabled = False 'stops the jackpots if they were enabled
                li043.State = 0
                li036.State = 0
                li040.State = 0
            End If
            'WOOLY DMD CL("POSEIDON STARTED"), CL("HIT THE LIT TARGETS"), "d_poseidon", eNone, eBlink, eNone, 2500, True, "vo_poseidon_mode"
            DMD CL("FRANKENSTEIN STARTED"), CL("HIT THE LIT TARGETS"), "d_poseidon_alt", eNone, eBlink, eNone, 2500, True, vo_Poseidon_mode
            li007.BlinkInterval = 100
            Mode(CurrentPlayer, 6) = 2
            UpdateModeLights
            EndModeCountdown = 120
            EndModeTimer.Enabled = 1
            ModeStep = 1
            'lights used in the mode
            TargetLightsAll 2
            ChangeGi blue
            ChangeGIIntensity 2
        Case 7 ' Hades
            '120 second timer
            'Hit the trap door 5 times
            'check for Shoot Again
            If li012.State = 1 AND li011.State = 1 Then
                AwardExtraBall
            End If
            'WOOLY DMD CL("HADES STARTED"), CL("HIT THE TRAPDOOR"), "d_hades", eNone, eBlink, eNone, 2500, True, "vo_hades_mode"
            DMD CL("BLACK KNIGHT STARTED"), CL("HIT THE TRAPDOOR"), "d_hades_alt", eNone, eBlink, eNone, 2500, True, vo_Hades_mode
            li010.BlinkInterval = 100
            Mode(CurrentPlayer, 7) = 2
            UpdateModeLights
            EndModeCountdown = 120
            EndModeTimer.Enabled = 1
            ModeStep = 1
            TrapDoorHits = 0
            TrapdoorUp
            PlaySound sd_secret_passage4 'SCOOBY code Velma says "It's a secret passage"
            ChangeGi red
            ChangeGIIntensity 1.6
        Case 8 ' Zeus
            '180 second timer
            'Complete ZEUS targets 3 times and finish at the scoop.
            'check for special
            If li012.State = 1 AND li014.State = 1 Then
                DMD CL("SPECIAL"), CL("IS LIT"), "_", eNone, eNone, eNone, 2500, True, "vo_extraballislit"
                Light010.State = 2
            End If
            'WOOLY DMD CL("ZEUS STARTED"), CL("SPELL ZEUS 3 TIMES"), "d_zeus", eNone, eBlink, eNone, 2500, True, "vo_specialislit"
            DMD CL("ZOMBIE MODE STARTED"), CL("SPELL ZOMB 3 TIMES"), "d_zeus_alt", eNone, eBlink, eNone, 2500, True,  vo_Zeus_mode
            PlaySound "vo_specialislit"
            li008.BlinkInterval = 100
            Mode(CurrentPlayer, 8) = 2
            UpdateModeLights
            EndModeCountdown = 180
            EndModeTimer.Enabled = 1
            ModeStep = 1
            'lights used in the mode
            li043.State = 2
            ChangeGi blue
            ChangeGIIntensity 2
            ZeusTargetsCompleted = 0 'reset the count
        Case 9                       ' Wizards modes: God or Demi-God mode
            '60 or 120 seconds timer
            'all balls are on the table, 5 balls, Jackpots enabled on the ramps and orbits.
            For each x in aBattleLights
                tmp = tmp + x.State
            Next
            If tmp = 8 Then 'all lights are lit and the battles are all completed
                'WOOLY DMD CL("GOD MODE STARTED"), CL("HIT THE JACKPOTS"), "d_zeus", eNone, eBlink, eNone, 2500, True, ""
                DMD CL("MASTER WIZARD MODE"), CL("HIT THE JACKPOTS"), "d_sd_wizard", eNone, eBlink, eNone, 3500, True, sd_vo_wiz_master 'SCOOBY mod v1.0.1
                PlaySound "sd_vo_allghosts" 'SCOOBY mod v1.0.1
                EndModeCountdown = 120
            Else
                'WOOLY DMD CL("DEMIGOD MODE STARTED"), CL("HIT THE JACKPOTS"), "d_zeus", eNone, eBlink, eNone, 2500, True, ""
                DMD CL("APPRENTICE MODE"), CL("HIT THE JACKPOTS"), "d_sd_apprentice", eNone, eBlink, eNone, 3500, True, sd_vo_wiz_apprentice 'SCOOBY mod v1.0.1
                PlaySound "sd_vo_allghosts" 'SCOOBY mod v1.0.1
                EndModeCountdown = 60
            End If
            Mode(CurrentPlayer, 9) = 2
            EndModeTimer.Enabled = 1
            'lights used in the mode - jackpots on the ramps
            li043.State = 2
            li036.State = 2
            li040.State = 2
            StartRainbow aGiLights
            ChangeGIIntensity 2
            AddMultiball 4
    End Select
    vpmtimer.addtimer 1500, "kickBallOut '"
End Sub

' Update the lights according to the mode's state, 0 not started, 1 finished, 2 started
Sub UpdateModeLights
    li013.State = Mode(CurrentPlayer, 1)
    li011.State = Mode(CurrentPlayer, 2)
    li012.State = Mode(CurrentPlayer, 3)
    li014.State = Mode(CurrentPlayer, 4)
    li009.State = Mode(CurrentPlayer, 5)
    li007.State = Mode(CurrentPlayer, 6)
    li010.State = Mode(CurrentPlayer, 7)
    li008.State = Mode(CurrentPlayer, 8)
End Sub

'sub ok
Sub CheckWinMode
    Dim tmp                                                                       'when you complete one the tasks
    Select Case Mode(CurrentPlayer, 0)
        Case 1                                                                    ' Minotaur
            If ModeStep = 1 Then
                If li060.State + li041.State + li037.State + li044.State = 0 Then 'all 4 lights are out then move to step 2
                    ModeStep = 2
                    TurnOnHelmet
                End If
            End If
        Case 2 ' Hydra
            Select Case ModeStep
                'WOOLY Case 1:li036.State = 0:li040.State = 2:ModeStep = 2:PlaySound "vo_og-growl"
                'WOOLY Case 2:li040.State = 0:li036.State = 2:ModeStep = 3:PlaySound "vo_og-growl"
                'WOOLY Case 3:li036.State = 0:li040.State = 2:ModeStep = 4:PlaySound "vo_og-growl"
                Case 1:li036.State = 0:li040.State = 2:ModeStep = 2:PlaySound vo_og_growl
                Case 2:li040.State = 0:li036.State = 2:ModeStep = 3:PlaySound vo_og_growl
                Case 3:li036.State = 0:li040.State = 2:ModeStep = 4:PlaySound vo_og_growl
                Case 4:
                    li040.State = 0
                    TurnOnHelmet
            End Select
        Case 3                                                      ' Cerberus
            If ModeStep = 1 Then
                If li036.State + li040.State + li043.State = 0 Then 'all 3 ramp lights are out then move to step 2
                    ModeStep = 2
                    TurnOnHelmet
                End If
            End If
        Case 4 ' Medusa
            Select Case ModeStep
                Case 1:li060.State = 0:li041.State = 2:ModeStep = 2
                Case 2:li041.State = 0:li037.State = 2:ModeStep = 3
                Case 3:li037.State = 0:li044.State = 2:ModeStep = 4
                Case 4:
                    li044.State = 0
                    TurnOnHelmet
            End Select
        Case 5                             ' Ares
            If ModeStep = 1 Then
                If SpinnerHits >= 100 Then 'move to step 2
                    li037.State = 0
                    li041.State = 0
                    ModeStep = 2
                    TurnOnHelmet
                End If
            End If
        Case 6 ' Poseidon
            If ModeStep = 1 Then
                tmp = 0
                For each x in aTargetsAll
                    tmp = tmp + x.State
                Next
                If tmp = 0 Then 'move to step 2
                    ModeStep = 2
                    TurnOnHelmet
                End If
            End If
        Case 7 ' Hades
            If TrapDoorHits = 5 Then
                WinMode
            Else
                vpmtimer.addtimer 2000, "kickBallOut '"
            End If
        Case 8 ' Zeus
            If ModeStep = 1 Then
                If ZeusTargetsCompleted = 3 Then
                    ModeStep = 2
                    TurnOnHelmet
                End If
            End If
        Case 9 ' God or Demi-God mode 'the mode runs until time is up or loose all the balls
    End Select
End Sub

'sub ok
Sub WinMode 'when you complete all the tasks (Scooby Doo win the special monster/god ModeStep winbattlemode)
    GiEffect 1
    LightEffect 2
    FlashEffect RndNbr(7)
    ZeusF
    Select Case Mode(CurrentPlayer, 0)
        Case 1                         ' Minotaur
            Mode(CurrentPlayer, 1) = 1 'set the mode as finished, and it will make the light solid lit (UpdateModeLights)
            TotalMonsters(CurrentPlayer) = TotalMonsters(CurrentPlayer) + 1
            DMDFlush
            'WOOLY DMD "_", CL("MINOTAUR COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            'WOOLY DMD "_", CL("MINOTAUR COMPLETED"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
            'WOOLY DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, ""
			'*** SCOOBY WIN ***
            DMD "_", CL("KINGSTON CAPTURED"), "_", eNone, eScrollLeft, eNone, 20, True, sd_fx_thunder & RndNbr(1)
            DMD "_", CL("KINGSTON CAPTURED"), "d_sd_captured", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
            DMD "_", "_", "d_sd_winner", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
            'SCOOBY v1.0.1 possible overkill sound PlaySound "sd_vo_shaggy_wedidit" 'Scooby code v1.0.1
            DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, sd_vo_captured_kingston
            ModeStep = 0
            AddScore2 1000000
            TurnOffHelmet
            vpmtimer.addtimer 2500, "kickBallOut '"
        Case 2 ' Hydra
            Mode(CurrentPlayer, 2) = 1
            TotalMonsters(CurrentPlayer) = TotalMonsters(CurrentPlayer) + 1
            DMDFlush
            'WOOLY DMD "_", CL("HYDRA COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            'WOOLY DMD "_", CL("HYDRA COMPLETED"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
            'WOOLY DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, ""
			'*** SCOOBY WIN ***
            DMD "_", CL("MONOTIKI CAPTURED"), "_", eNone, eScrollLeft, eNone, 20, True, sd_fx_thunder & RndNbr(1)
            DMD "_", CL("MONOTIKI CAPTURED"), "d_sd_captured", eNone, eBlinkFast, eNone, 1500, True, ""
            DMD "_", "_", "d_sd_winner", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
            PlaySound "sd_vo_shaggy_wedidit" 'Scooby code v1.0.1
            DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, sd_vo_captured_monotiki
            ModeStep = 0
            AddScore2 1000000
            TurnOffHelmet
            vpmtimer.addtimer 2500, "kickBallOut '"
        Case 3 ' Cerberus
            Mode(CurrentPlayer, 3) = 1
            TotalMonsters(CurrentPlayer) = TotalMonsters(CurrentPlayer) + 1
            DMDFlush
            'WOOLY DMD "_", CL("CERBERUS COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            'WOOLY DMD "_", CL("CERBERUS COMPLETED"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
            'WOOLY DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, ""
			'*** SCOOBY WIN ***
            DMD "_", CL("CPTN CUTLER CAPTURED"), "_", eNone, eScrollLeft, eNone, 20, True, sd_fx_thunder & RndNbr(1)
            DMD "_", CL("CPTN CUTLER CAPTURED"), "d_sd_captured", eNone, eBlinkFast, eNone, 1500, True, ""
            DMD "_", "_", "d_sd_winner", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
            'PlaySound "sd_vo_shaggy_wedidit" 'Scooby code v1.0.1
            DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, sd_vo_captured_cutler
            ModeStep = 0
            AddScore2 1000000
            TurnOffHelmet
            vpmtimer.addtimer 2500, "kickBallOut '"
        Case 4 ' Medusa
            Mode(CurrentPlayer, 4) = 1
            TotalMonsters(CurrentPlayer) = TotalMonsters(CurrentPlayer) + 1
            DMDFlush
            'WOOLY DMD "_", CL("MEDUSA COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            'WOOLY DMD "_", CL("MEDUSA COMPLETED"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
            'WOOLY DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, ""
			'*** SCOOBY WIN
            DMD "_", CL("GHOST CLOWN CAPTURED"), "_", eNone, eScrollLeft, eNone, 20, True, sd_fx_thunder & RndNbr(1)
            DMD "_", CL("GHOST CLOWN CAPTURED"), "d_sd_captured", eNone, eBlinkFast, eNone, 1500, True, ""
            DMD "_", "_", "d_sd_winner", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
            'PlaySound "sd_vo_shaggy_wedidit" 'Scooby code v1.0.1
            DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, sd_vo_captured_ghostclown
            ModeStep = 0
            AddScore2 1000000
            TurnOffHelmet
            'WOOLY StartMedussaMB 'start medusa multiball
            StartMedusaMB 'start medusa multiball, SCOOBY code v1.0.1 fixing Medusa spelling
            vpmtimer.addtimer 4000, "kickBallOut '"
            If TotalMonsters(CurrentPlayer) = 4 Then 'all monsters have been defeated, turn on the beast master light
                'WOOLY DMD "YOU ARE THE", CL("BEAST MASTER"), "_", eNone, eNone, eNone, 1500, True, "vo_beast_master"
                DMD CL("YOU ARE THE"), CL("MONSTER MASHER"), "_", eNone, eNone, eNone, 1500, True, sd_vo_masher_monster 'SCOOBY code v1.0.1
                DMD "_", "_", "d_sd_masher", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
                li047.State = 1
                AddScore2 1000000
            End If

        Case 5 ' Ares
            Mode(CurrentPlayer, 5) = 1
            TotalGods(CurrentPlayer) = TotalGods(CurrentPlayer) + 1
            DMDFlush
            'WOOLY DMD "_", CL("ARES DEFEATED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            'WOOLY DMD "_", CL("ARES DEFEATED"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            'WOOLY DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, ""
			'*** SCOOBY code ARES/GREEN GHOST DEFEATED
            DMD "_", CL("GREEN GHOST CAPTURED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("GREEN GHOST CAPTURED"), "d_sd_captured", eNone, eBlinkFast, eNone, 2000, True, ""
            DMD "_", "_", "d_sd_winner", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
            'PlaySound "sd_vo_shaggy_wedidit" 'Scooby code v1.0.1
            PlaySound "sd_vo_captured_greenghost" 'Scooby code v1.0.1
            DMD "_", SPACE(20), "d_sd_captured", eNone, eScrollLeft, eNone, 20, True, sd_mode_win 'SCOOBY  v1.0.1 removed winner audio clip
            ModeStep = 0
            AddScore2 1000000
            TurnOffHelmet
            vpmtimer.addtimer 3000, "kickBallOut '"
        Case 6 ' Poseidon
            Mode(CurrentPlayer, 6) = 1
            TotalGods(CurrentPlayer) = TotalGods(CurrentPlayer) + 1
            DMDFlush
            'WOOLY DMD "_", CL("POSEIDON DEFEATED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            'WOOLY DMD "_", CL("POSEIDON DEFEATED"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            'WOOLY DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, ""
			'*** SCOOBY code POSEIDON/FRANKENSTEIN DEFEATED
            DMD "_", CL("FRANKNSTEIN CAPTURED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("FRANKNSTEIN CAPTURED"), "d_sd_captured", eNone, eBlinkFast, eNone, 2000, True, ""
            DMD "_", "_", "d_sd_winner", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
            PlaySound "sd_vo_captured_frankenstein" 'Scooby code v1.0.1
            DMD "_", SPACE(20), "d_sd_captured", eNone, eScrollLeft, eNone, 20, True, sd_mode_win
            ModeStep = 0
            AddScore2 1000000
            TurnOffHelmet
            vpmtimer.addtimer 3000, "kickBallOut '"
        Case 7 ' Hades
            Mode(CurrentPlayer, 7) = 1
            TotalGods(CurrentPlayer) = TotalGods(CurrentPlayer) + 1
            DMDFlush
            'WOOLY DMD "_", CL("HADES DEFEATED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            'WOOLY DMD "_", CL("HADES DEFEATED"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            'WOOLY DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, ""
			'*** SCOOBY code HADES/BLACK KNIGHT DEFEATED !
            DMD "_", CL("BLAK KNIGHT CAPTURED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("BLAK KNIGHT CAPTURED"), "d_sd_captured", eNone, eBlinkFast, eNone, 2000, True, ""
            DMD "_", "_", "d_sd_winner", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
            PlaySound "sd_vo_captured_blackknight" 'Scooby code v1.0.1
            DMD "_", SPACE(20), "d_sd_captured", eNone, eScrollLeft, eNone, 20, True, sd_mode_win

            ModeStep = 0
            AddScore2 5000000
            TurnOffHelmet
            vpmtimer.addtimer 3000, "kickBallOut '"
        Case 8 ' Zeus
            Mode(CurrentPlayer, 8) = 1
            TotalGods(CurrentPlayer) = TotalGods(CurrentPlayer) + 1
            DMDFlush
            'WOOLY DMD "_", CL("ZEUS DEFEATED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            'WOOLY DMD "_", CL("ZEUS DEFEATED"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            'WOOLY DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, ""
            '*** SCOOBY code ZEUS/ZOMBIE DEFEATED ***
            DMD "_", CL("ZOMBIE CAPTURED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("ZOMBIE CAPTURED"), "d_sd_captured", eNone, eBlinkFast, eNone, 2000, True, ""
            DMD "_", "_", "d_sd_winner", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
            PlaySound "sd_vo_captured_zombie" 'Scooby code v1.0.1
            DMD "_", SPACE(20), "_", eNone, eScrollLeft, eNone, 20, True, sd_mode_win

            ModeStep = 0
            AddScore2 5000000
            TurnOffHelmet
            vpmtimer.addtimer 3000, "kickBallOut '"
            If TotalGods(CurrentPlayer) = 4 Then 'all gods has been defeated
                'WOOLY DMD "YOU ARE THE", CL("GOD OF GODS"), "_", eNone, eNone, eNone, 1500, True, "vo_god_of_gods"
                DMD CL("YOU ARE THE"), CL("GHOST MASHER"), "_", eNone, eNone, eNone, 1500, True, sd_vo_masher_ghost 'SCOOBY code v1.0.1
                DMD "_", "_", "d_sd_masher", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
                AddScore2 2000000
            End If
        Case 9 ' God or Demi-God mode - (the mode is a multiball, it stops when you run out of time or los the balls)
        'SCOOBY code converting these 2 modes to "MASTER WIZARD" and "APPRENTICE WIZARD" for the SCOOBY Table v1.0.1
            DMD "_", "_", "d_sd_wizards", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
            PlaySound "sd_vo_allghosts"
    End Select
    StopMode
End Sub

'sub ok
Sub StopMode                 'called after a win or at the end of a ball to stop the current mode variables and timers
    EndModeTimer.Enabled = 0 'ensure it is stopped
    TurnOffArrows
    Select Case Mode(CurrentPlayer, 0)
        Case 1                         ' Minotaur
            li013.BlinkInterval = 1000 'slow blink in case the mode is not finished
        Case 2                         ' Hydra
            li011.BlinkInterval = 1000
        Case 3                         ' Cerberus
            li012.BlinkInterval = 1000
        Case 4                         ' Medusa
            li014.BlinkInterval = 1000
        Case 5                         ' Ares
            li009.BlinkInterval = 1000
        Case 6                         ' Poseidon
            li007.BlinkInterval = 1000
            TargetLightsAll 0
        Case 7 ' Hades
            li010.BlinkInterval = 1000
            vpmtimer.addtimer 2500, "TrapdoorDown '"
        Case 8 ' Zeus
            li008.BlinkInterval = 1000
            ' and start the wizard mode
            vpmtimer.addtimer 1500, "EnableBallSaver 15:StartNextMode '" 'wait a little before start the wizard mode, so the last mode stops and all the variables are set up right
        Case 9                                                           ' God or Demi-God mode -
            ResetModes
    End Select
    ' reset variables
    ModeStep = 0
    UpdateModeLights
    OrbitHits = 0 'start counting again for the next mode
    RampHits = 0
    Mode(CurrentPlayer, 0) = 0
    bModeReady = False
    ChangeGi white
    ChangeGIIntensity 1
    ChangeSong
End Sub

'sub ok	
Sub ResetModes 'called after the last wizard mode to start all over again
    Dim i, j
    For i = 0 to 9
        Mode(CurrentPlayer, i) = 0
    Next
    StopRainbow
    UpdateModeLights
    'reset Mode variables
    CurrentMode(CurrentPlayer) = 0
    bModeReady = False
End Sub

'SCOOBY End Timer mode final countdown FINAL COUNT DOWN
Sub EndModeTimer_Timer '1 second timer to count down to end the timed modes
    EndModeCountdown = EndModeCountdown - 1
    Select Case EndModeCountdown
 'WOOLY Case 16:DMD "_", CL("TIME IS RUNNING OUT"), "_", eNone, eNone, eNone, 1000, True, " "
        Case 16:DMD "_", CL("TIME IS RUNNING OUT"), "d_sd_countdown", eNone, eNone, eNone, 2000, True, sd_lower_iron_door
        'SCOOBY bug table is crashing here when in Zeus Mode
        Case 10:DMD "_", CL("10"), "_", eNone, eNone, eNone, 500, True, sd_time_running_out
        Case 9:DMD "_", CL("9"), "_", eNone, eNone, eNone, 500, True, ""
        Case 8:DMD "_", CL("8"), "_", eNone, eNone, eNone, 500, True, ""
        Case 7:DMD "_", CL("7"), "_", eNone, eNone, eNone, 500, True, "sd_number_07"
        Case 6:DMD "_", CL("6"), "_", eNone, eNone, eNone, 500, True, "sd_number_06"
        Case 5:DMD "_", CL("5"), "_", eNone, eNone, eNone, 500, True, "sd_number_05"
        Case 4:DMD "_", CL("4"), "_", eNone, eNone, eNone, 500, True, "sd_number_04"
        Case 3:DMD "_", CL("3"), "_", eNone, eNone, eNone, 500, True, "sd_numbers_3"
        Case 2:DMD "_", CL("2"), "_", eNone, eNone, eNone, 500, True, "sd_numbers_2"
        Case 1:DMD "_", CL("1"), "_", eNone, eNone, eNone, 500, True, "sd_numbers_1"
        Case 0
            'WOOLY DMD CL("TIME IS UP"), CL("BATTLE TERMINATED"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
            'SCOOBY code: DMD CL("TIME IS UP"), CL("GHOST ESCAPED"), "_", eNone, eBlinkFast, eNone, 1500, True, "sd_fx_batsfly2"
			'SCOOBY code: Looks like I revised this entire section, seems to work ok.
            If Mode(CurrentPlayer, 0) = 9 Then
                'SCOOBY code: DMD CL("GET READY"), CL("THE GODS ARE BACK"), "_", eNone, eBlinkFast, eNone, 2000, True, "sd_vo_giveitagain"
                DMD CL("GET READY"), CL("GAME IS RESTARTING"), "d_sd_startover", eNone, eBlinkFast, eNone, 4500, True, "sd_vo_giveitagain"
				PlaySound "sd_mu_gameover" 'SCOOBY mod added game over thems song ending from Season 1 TV show as audio clue game is restarting
            Else
                'WOOY DMD CL("TIME IS UP"), CL("YOU LOOSE"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
				'MRJCRANE Insert special LOOSER audio clip her you LOOSERS when mode is not completed whah whah whah!!!!!!!!!!
                DMD CL("TIME IS UP"), CL("YOU LOOSE"), "d_sd_escaped", eNone, eBlinkFast, eNone, 2500, True, sd_mode_looser 'SCOOY mod v1.0.1 added "LOOSER" audio
				PlaySound sd_mode_loose 'SCOOBY mod v1.0.1 moved this to its own line
                PlaySound sd_fx_drawbridge_close 
            End If
            StopMode
    End Select
   'DMD CL("DEBUG EXIT"), CL("END MODE COUNTDOWN"), "_", eNone, eBlinkFast, eNone, 4000, True, "" 'SCOOBY debug code active
End Sub

Sub TurnOnHelmet 'to end a battle
    Light001.State = 2
    Light002.State = 2
    Light003.State = 2
    Light011.State = 2
End Sub

Sub TurnOffHelmet
    Light001.State = 0
    Light002.State = 0
    Light003.State = 0
    Light011.State = 0
End Sub

Sub TurnOffArrows 'at the end of the ball or timed mode
    li060.State = 0
    li036.State = 0
    li038.State = 0
    li041.State = 0
    li037.State = 0
    li044.State = 0
    li040.State = 0
    li043.State = 0
    li042.State = 0
    li039.State = 0
End Sub

'*************
' Magnet
'*************

Sub ReleaseMagnetBalls 'mMagnet off and release the ball if any
    Dim ball
    mMagnet.MagnetOn = False
    DOF 132, DOFOff
    For Each ball In mMagnet.Balls
        With ball
            .VelX = 1
            .VelY = 1
        End With
    Next
End Sub

'**************
'   COMBOS - Modified for Scooby Doo sound files. mrjcrane 03/20/22 This section looks complete
'**************

Sub AwardCombo
    ComboCount = ComboCount + 1
    DOF 130, DOFPulse
    Select Case ComboCount
        Case 0, 1:Exit Sub 'this should never happen though
        Case 2
            'WOOLY DMD CL("COMBO"), CL(FormatScore(ComboValue(CurrentPlayer))), "", eNone, eNone, eNone, 1500, True, "vo_combo"
            DMD CL("COMBO"), CL(FormatScore(ComboValue(CurrentPlayer))), "", eNone, eNone, eNone, 1500, True, sd_vo_combo1x
            ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 3
            'WOOLY DMD CL("2X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1500, True, "vo_combo"
            DMD CL("2X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1500, True, sd_vo_combo2x
            ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 4
            'WOOLY DMD CL("3X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1500, True, "vo_combo"
            DMD CL("3X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1500, True, sd_vo_combo3x
            ComboValue(CurrentPlayer) = ComboValue(CurrentPlayer) + 100000:ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 5
            'WOOLY DMD CL("4X HURRICANE"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1500, True, "vo_hurricane"
            DMD CL("4X HURRICANE"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1500, True, sd_vo_combo4x 
			PlaySound sd_vo_hurricane
            ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
            If Light009.State = 0 Then
                DMD "_", CL("EXTRA BALL IS LIT"), "_", eNone, eBlink, eNone, 2500, True, "vo_extraballislit"
                Light009.State = 2
            End If
        Case 6
            'WOOLY DMD CL("5X COMBO KING"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1500, True, "vo_combo_king"
            DMD CL("5X JINKIES COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1500, True, sd_vo_jinkies
            ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
            li046.State = 1
            If Light010.State = 0 Then
                DMD CL("_"), CL("SPECIAL IS LIT"), "_", eNone, eBlink, eNone, 2000, True, "vo_specialislit"
                Light010.State = 2
            End If
        Case 7
            'WOOLY DMD CL("6X WIND RIDER"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1500, True, "vo_wind_rider"
            DMD CL("6X ZOINKS COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1500, True, sd_vo_zoinks
            ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
            li048.State = 1
        Case Else
            'WOOLY DMD CL("SUPERDUPER COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1500, True, "vo_combo"
            DMD CL("SUPERDUPER COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1500, True, sd_vo_combosuper
            'PlaySound sd_vo_scoobydoobie
			PlaySound sd_vo_freakyskillshot
            ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
    End Select
    AddScore2 ComboValue(CurrentPlayer) * ComboCount
    ComboValue(CurrentPlayer) = ComboValue(CurrentPlayer) + 100000
End Sub

Sub aComboTargets_Hit(idx) 'reset the combo count if the ball hits another target/trigger
    ComboCount = 0
End Sub

'    MULTIBALLS MULTIBALL

'********************
' MEDUSA MB - LOOPS
'********************
' Starts after the medusa monster Mode
' orbit shots doubles their values
' ramp shots are worth 1/2 of their value
' orbithits + ramphits are 5 or more then light the extra ball

'WOOLY Sub StartMedussaMB
Sub StartMedusaMB 'SCOOBY code v1.0.1 fixing spelling issue Medussa to Medusa
    'WOOLY DMD CL("MEDUSA MULTIBALL"), CL("SHOOT THE LOOPS"), "_", eNone, eNone, eNone, 1500, True, "vo_medusa_multiball"
    DMD CL("GHOSTCLOWN MULTIBALL"), CL("SHOOT THE LOOPS"), "_", eNone, eNone, eNone, 1500, True, vo_medusa_multiball 'SCOOBY code
    DMD "_", "_", "d_sd_multiball", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1
    bMedusaMBStarted = True
    ExtraBallHits = 0
    AddMultiball 4
    EnableBallSaver 15
End Sub

' during medusa MB check for 5 orbits and light extra ball
Sub CheckExtraBallHits
    If bMedusaMBStarted Then
        If ExtraBallHits MOD 5 = 0 Then
            If Light009.State = 0 Then
                DMD "_", CL("EXTRA BALL IS LIT"), "_", eNone, eBlink, eNone, 2500, True, "vo_extraballislit"
                Light009.State = 2
            End If
        End If
    End If
End Sub

'****************************************************
' Minotaur MB - holes & lock system at the Ares hole
'****************************************************
' lock 3 balls, and MB starts
' holes score Jackpots
' value doubles each time all three holes has been Hit
' each hole gives just 1 jackpot until all three has been hit again.

Sub CheckMinotaurMB
    If BallsInLock(CurrentPlayer) = 1 Then
        'WOOLY DMD "_", CL("BALL 1 LOCKED"), "_", eNone, eNone, eNone, 1500, True, "vo_lock1"
        DMD "_", CL("BALL 1 LOCKED"), "_", eNone, eNone, eNone, 1500, True, vo_lock1 'SCOOBY sound mod ball lock message
		PlaySound "sd_scooby_laugh_delay05" 'SCOOBY code
    End If
    If BallsInLock(CurrentPlayer) = 2 Then
        'WOOLY DMD "_", CL("BALL 2 LOCKED"), "_", eNone, eNone, eNone, 1500, True, "vo_lock2"
        DMD "_", CL("BALL 2 LOCKED"), "_", eNone, eNone, eNone, 1500, True, vo_lock2
		PlaySound "sd_scooby_laugh_delay10" 'SCOOBY code
    End If
    If BallsInLock(CurrentPlayer) = 3 Then
        'WOOLY DMD CL("MINOTAUR MULTIBALL"), CL("SHOOT THE HOLES"), "_", eNone, eNone, eNone, 1500, True, "vo_minotaur_multiball"
        'SCOOBY code MINOTAUR MULTIBALL = MYSTERY MULTIBALL
        DMD CL("KINGSTON MULTIBALL"), CL("SHOOT 3 SCOOP HOLES"), "d_minotaur_alt", eNone, eNone, eNone, 1500, True, vo_lock3 'SCOOBY code v1.0.1
		DMD "_", "_", "d_sd_multiball", eNone, eBlinkFast, eNone, 1500, True, "" 'Scooby code v1.0.1 
       'SCOOBY code starting StartMinotaurMB/Elias Kingston Multiball (Hit 3 of the scoop holes)
        PlaySound sd_fx_bats_fly 'SCOOBY code v1.0
        'PlaySound "sd_vo_multiball_kingston" 'SCOOBY code v1.0.1
        sd_PlayMultiballMusic    
        bMinotaurMBStarted = True
        bLockEnabled = False
        li039.State = 0
        Minotaur1 = 0
        Minotaur2 = 0
        Minotaur3 = 0
        AddMultiball 2
        BallsInLock(CurrentPlayer) = 0
        TrapdoorUp
		'SCOOBY code special audio clip for when the trap door opens
        PlaySound sd_secret_passage4 'SCOOBY code Velma says "It's a secret passage"
        'WOOLY ChangeSong
        'SCOOBY code add the Mystery Inc intro song for multiball modes via ChangeSong
    End If
End Sub
'sub ok

Sub CheckMinotaurMBHits
    If Minotaur1 + Minotaur2 + Minotaur3 = 3 Then 'all 3 holes has been hit so double the jackpot
        MinotaurJackpot(CurrentPlayer) = MinotaurJackpot(CurrentPlayer) * 2
        'WOOLY DMD CL("MINOTAUR JACKPOT IS"), CL(FormatScore(MinotaurJackpot(CurrentPlayer))), "_", eNone, eNone, eNone, 1500, True, ""
        'SCOOBY MINOTAUR JACKPOT = MYSTERY JACKPOT
        DMD CL("MYSTERY JACKPOT IS"), CL(FormatScore(MinotaurJackpot(CurrentPlayer))), "_", eNone, eNone, eNone, 1500, True, ""
        Minotaur1 = 0
        Minotaur2 = 0
        Minotaur3 = 0
    End If
End Sub
'sub ok

Sub AwardMinotaurJackpot()
    DOF 130, DOFPulse
    'WOOLY DMD CL("MINOTAUR JACKPOT"), CL(FormatScore(MinotaurJackpot(CurrentPlayer))), "_", eNone, eBlinkFast, eNone, 2000, True, "vo_Jackpot"
    'SCOOBY MINOTAUR JACKPOT = MYSTERY JACKPOT
    DMD CL("MYSTERY JACKPOT"), CL(FormatScore(MinotaurJackpot(CurrentPlayer))), "_", eNone, eBlinkFast, eNone, 2000, True, "vo_Jackpot" 'SCOOBY code

    'SCOOBY code tod fix (JAC Add new Jackpot Sound)
    
	DOF 126, DOFPulse
    AddScore2 Jackpot(CurrentPlayer)
    MinotaurJackpot(CurrentPlayer) = MinotaurJackpot(CurrentPlayer) + 100000
    LightEffect 2
    GiEffect 1
    FlashEffect 1
End Sub

'**********************
' Zeus MB - Upper Ramp
'**********************

Sub StartZeusMB
    'WOOLY DMD CL("ZEUS MULTIBALL"), "SHOOT THE UPPER RAMP", "_", eNone, eNone, eNone, 1500, True, "vo_zeus_multiball"
    'SCOOBY ZEUS MULTIBALL = ZOMBIE MULTIBALL
    DMD CL("ZOMBIE MULTIBALL"), CL("SHOOT UPPER RAMP"), "d_zeus_alt", eNone, eNone, eNone, 1500, True, vo_zeus_multiball 'SCOOBY code
    
	'SCOOBY code tod fix (JAC Add new Zombie Sound)
    
	bZeusMBStarted = True
    AddMultiball 3
    EnableBallSaver 15
    ZeusMBFlashTimer.Enabled = 1
End Sub
'sub ok

Sub ZeusMBFlashTimer_Timer
    LTF
    vpmtimer.addtimer 250, "RTF '" 'delay a little the right flasher
End Sub

'*******************
' BONUS MULTIPLIER = OK
'*******************
' fire shots

Sub CheckBonusX
    If li030.State + li031.State + li032.State + li033.State + li034.State = 5 Then 'all the fire lights are on
        AddBonusMultiplier 1
        FlashEffect 5
        LightEffect 5
        li030.State = 0
        li031.State = 0
        li032.State = 0
        li033.State = 0
        li034.State = 0
        'blink the X lights fast but only the ones that were off
        If li024.State = 0 Then li024.State = 2
        If li025.State = 0 Then li025.State = 2
        If li026.State = 0 Then li026.State = 2
        If li027.State = 0 Then li027.State = 2
        If li028.State = 0 Then li028.State = 2
    End If
End Sub

'************
' The Fates = OK AKA MYSTERY MACHINE in SCOOBY Table
'************

Sub CheckFates 'checks for hits and start the Fates mode
    If FatesHits(CurrentPlayer)MOD 10 = 0 Then
        'WOOLY DMD CL("THE FATES"), "HAVE STARTED", "_", eNone, eNone, eNone, 1500, True, "vo_The_fates"
		'SCOOBY THE FATES = THE MYSTERY MACHINE
        DMD CL("THE MYSYERY MACHINE"), CL("HAS STARTED"), "_", eNone, eNone, eNone, 1500, True, sd_vo_mysterymachine
        PlaySound "fx_shaker" ' SCOOBY code v1.0
        PlaySound "sd_fx_vanrev"
        DOF dof_shaker, DOFPulse 'SCOOBY code v1.0
        'SCOOBY to do, add sound of the Mystery Machine here
        bFatesStarted = True
        li035.State = 2
        windshield_light.State = 2 'SCOOBY code v1.0
    End If
End Sub

'*****************
' BONUS HIT SUBS
'*****************

Sub aBonusTargets_Hit(idx):BonusTargets(CurrentPlayer) = BonusTargets(CurrentPlayer) + 1:End Sub
Sub aBonusRamps_Hit(idx):BonusRamps(CurrentPlayer) = BonusRamps(CurrentPlayer) + 1:End Sub
Sub aBonusOrbits_Hit(idx):BonusOrbits(CurrentPlayer) = BonusOrbits(CurrentPlayer) + 1:End Sub

' DMD CL(""), CL(""), "", eNone, eNone, eNone, 3000, True, ""

'SCOOBY SPECIFIC SUB ROUTINES ADDED TO TAIL END OF WOOLY CODE *******************************************************

'**************************
' TURN OFF ALL DOF TRIGGERS so nothing sneaks in unxpected SCOOBY code v1.0 DOF
'**************************
Sub dof_all_off
	'SCOOBY add code in phase 2 of table deployment
End Sub

'*********************
' mrjcrane additional sub routines for table testing added above what was already here for WOOLY
'*********************
Sub KeysDofTestSingleChannel
    'this is for testing the left pop pumper dof code 105 which seems to be having some issues
    DOF dof_popbumper_left, DOFPulse
End Sub

Sub DOF_FireAllChannels
	'*** ALL ON 
    DOF dof_flipper_left, 		DOFOn	'F1
    DOF dof_flipper_right, 		DOFOn	'F2
    DOF dof_slingshot_left, 	DOFOn	'F3
    DOF dof_slingshot_right, 	DOFOn	'F4
    DOF dof_popbumper_left, 	DOFOn	'F5
    DOF dof_popbumper_right, 	DOFOn	'F6
    DOF dof_shaker, 			DOFPulse'F7
    DOF dof_knocker, 			DOFOn	'F8
    '*** ALL OFF
    DOF dof_flipper_left, 		DOFOff	'F1
    DOF dof_flipper_right, 		DOFOff	'F2
    DOF dof_slingshot_left, 	DOFOff	'F3
    DOF dof_slingshot_right, 	DOFOff	'F4
    DOF dof_popbumper_left, 	DOFOff	'F5
    DOF dof_popbumper_right, 	DOFOff	'F6
    DOF dof_shaker, 			DOFOff	'F7
    DOF dof_knocker, 			DOFOff	'F8
End Sub

'   mrjcrane custom for SCOOBY DOO table put both flippers in up position, hitting flipper again will lower it.
Sub sd_KeysFlippersUp
    SolLFlipper 1:InstantInfoTimer.Enabled = False:RotateLaneLights 1
	SolRFlipper 1:InstantInfoTimer.Enabled = False:RotateLaneLights 0
End Sub
Sub sd_KeysFlippersDown
    SolLFlipper 0:InstantInfoTimer.Enabled = False:RotateLaneLights 1
	SolRFlipper 0:InstantInfoTimer.Enabled = False:RotateLaneLights 0
End Sub

Sub FxParrotSound 
    'mrjcrane add code in phase 2 deployment make the parrot squalk
    PlaySound sd_fx_parrot
End Sub

Sub sd_PlayMultiballMusic
       'SCOOBY code v1.0 AUDIO RANDOMIZER: 03/28/2022 added randomizer to pick from different audio samples more can be added later in case logic
       SELECT Case RndNbr(2)
              Case 0 PlaySong " " 'SCOOBY code v1.0 this is just a place holder for the original WOOLY fast guitar music
              Case 1 PlaySong "sd_mu_multiball2" 'SCOOBY code v1.0 Fast Music from 70's TV show
              Case 2 Playsong "sd_mu_multiball3" 'SCOOBY code v1.0 Fast Music short version of Mystery Inc Song
       END SELECT
End Sub

'** Skillshot Debugging"
Sub Debug_Skillshot
    AwardSkillshot()
End Sub
Sub Debug_SuperSkillshot
    AwardSuperSkillshot()
End Sub
Sub Debug_FreakySkillshot
    AwardFreakySkillshot()
End Sub

Sub Debug_Jackpot
    AwardJackpot()
End Sub
Sub Debug_SuperJackpot
    AwardSuperJackpot()
End Sub
Sub Debug_Combo
    AwardCombo()
End Sub

Sub Debug_ExtraBall
    AwardExtraBall()
End Sub
Sub Debug_Special
	AwardSpecial()
End Sub
Sub Debug_Fates
    CheckFates
End Sub

Sub Debug_NextMode
	StartNextMode
End Sub	
Sub Debug_StopMode
	StopMode
End Sub
Sub Debug_StopAllModes
	StopMode
	StopMBmodes
	StopEndOfBallMode
End Sub
Sub Debug_StopMultiball
	StopMBmodes
End Sub

Sub Debug_RestetModes
	ResetModes
End Sub
Sub Debug_ResetGame
End Sub

'Debug Multiball
Sub Debug_MB_Kingston
    BallsInLock(CurrentPlayer)= 3
    CheckMinotaurMB
End Sub
Sub Debug_MB_Clown
    BallsInLock(CurrentPlayer)= 3
    StartMedusaMB
End Sub
Sub Debug_MB_Zomb
    BallsInLock(CurrentPlayer)= 3
    StartZeusMB
End Sub

'** Other Debug Routines **
Sub Debug_WinMode
    Mode(CurrentPlayer, 1) = 1
    WinMode
End Sub
Sub Debug_Countdown
	EndModeTimer_Timer
End Sub
Sub Debug_CaptiveBall
	CBHits(CurrentPlayer) = 24
    Cball_Hit
End Sub
Sub Debug_Kickback
    kickbackNeeded(CurrentPlayer) = 1
    kickbackHits(CurrentPlayer) = kickbackNeeded(CurrentPlayer)
    checkkickback
End Sub
Sub Debug_RightRamp
    rramp_Hit
End Sub

Sub sd_MysteryMachineStartup
    'PlaySound "sd_fx_honk1"
    'PlaySound "sd_fx_vanstart"
    DOF dof_shaker, DOFPulse
End Sub



'** Juke Box Music Routine added in SCOOBY mod v1.0.1 mrjcrane
'** Notes about the jukebox, it will play song in a loop until you hit "INSERT" for next song or "DELETE" to stop it OR if game changes it
Sub sd_JukeboxStart
    'SCOOBY mod v1.0.1 AUDIO RANDOMIZER: 04/06/2022 added randomizer to pick from different songs, more can be added later in case logic
    SELECT Case RndNbr(7)
           Case 0 PlaySong " " 'SCOOBY code v1.0 this is just a place holder
           Case 1 PlaySong "mu_game_start_scooby" 		'SCOOBY mod v1.0.1 
           Case 2 PlaySong "sd_mu_song_tv_season1" 		'SCOOBY mod v1.0.1 
           Case 3 PlaySong "sd_mu_song_tv_season2" 		'SCOOBY mod v1.0.1 
           Case 4 PlaySong "sd_mu_song_tv_movies" 		'SCOOBY mod v1.0.1 
           Case 5 PlaySong "sd_mu_song_tv_whatsnew"		'SCOOBY mod v1.0.1 
           Case 6 PlaySong "sd_mu_song_tv_mystinc"		'SCOOBY mod v1.0.1 
           Case 7 PlaySong "sd_mu_song_tv_13ghosts"		'SCOOBY mod v1.0.1 
          'Case 10 PlaySong "insertyoursonghere"		'SCOOBY mod v1.0.1 allow players to add 1 song to the jukebox, ?Metalica/Sandman?
    END SELECT
End Sub

Sub sd_JukeboxStop
    StopSong "mu_game_start_scooby" 
    StopSong "sd_mu_song_tv_season1"
    StopSong "sd_mu_song_tv_season2"
    StopSong "sd_mu_song_tv_season3"
    StopSong "sd_mu_song_tv_movies"
	StopSong "sd_mu_song_tv_scoobyshow"	
    StopSong "sd_mu_song_tv_olympics"
    StopSong "sd_mu_song_tv_whatsnew"
	StopSong "sd_mu_song_tv_mystinc"
	StopSong "sd_mu_song_tv_13ghosts"
	StopSong "insertyoursonghere"
End Sub

Sub sd_PlayFavoriteSong
    PlaySong sd_favoritesong
End Sub

'** "Don't just do something, Stand there !!"; or the reverse?? ;) Have fun with the game everyone !
Sub sd_DoNothing
End Sub
