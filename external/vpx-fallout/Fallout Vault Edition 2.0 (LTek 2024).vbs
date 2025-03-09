' //////////////////////////////////////////////////////////////
'         Fallout Vault Edition 2.0 (LTek 2024)
'
'	Everyone wants to save the world, they just disagree on how
'
'   				   released 2024-09-20
' //////////////////////////////////////////////////////////////
' 
'  Vault Edition 2.0 is a large visual update + FlexDMD + VR + nFozzy Physics + Fleep Sounds and much more.
' 
' Many thanks to...
'   Clairvius for the original concept with Fallout Season 1 (reskin of Sword of Fury) 
'	GMan for the PuP Pack which prefectly compliments the table.
'
'*******************************************************************************
' Install Instructions
'
' -- ROM : copy swrds_l2.zip to your \vpinmame\roms
' -- add VPMalias :  fallout,swrds_l2
' -- copy Fallout.FlexDMD folder to your \tables
' -- update your DOFconfig
'
' Media :
' -- included :  Fallout_FullDMD_FlexDMD.mp4 (modified to use with the custom FlexDMD built-into the table)
'  -- PuP Pack not Required but reccomended. 
'    -- Table has No Music and No Call Outs without the PuP. I reccomend using at least the Music Only version.
'    -- Copy 'fallout' folder to \PinUPSystem\PUPVideos
'
'*******************************************************************************
' CREDITS
' Game Play & Layout: LTek (v2), Clairvius (v1)
' Coding: LTek (v2), Clairvious (v1)
' Playfield art : LTek (v2), Clarvious (v1)
' 3D models: LTek (v2), cyberpez
' Music, sound effects, audio editing: LTek (v2), Clairvius (v1)
' Mechanical sounds: LTek (v2), Clairvius (v1), Fleep, Robby King Pin
' DOF: LTek
' Testing: Studlygoorite, somatik, Colvert, Darth Vito, Mike Da Spike, Ext2k, passion4pins
' Flipper and physics: nFozzy, Rothbauerw, Robby King Pin, Primetime5k
' VR Cabinet & Room: Ext2k
' PuP Pack: GMan
' Original Concept: Clairvius
'*******************************************************************************
'
' TROUBLESHOOTING... If your Flippers get stuck (usually in the up position), Delete the NVRAM and restart the table, twice.
'
'
' ////////////////////////////////////////////////////////////////////////////////////////
' *** Changelog ***
'
'
' 2024-09-20 // 2.0
'	
'	- fixed and/or changed a lot of the playfield graphics and lighting  *LTek
'	- visual modifications of many playfield plastics nd inset lights  *LTek
'	- added and animated several Fallout 3D models (primitives)  *LTek
'	- added new wire ramp and extend/retract animation code   *LTek
'	- custom audio and sound effects added  *LTek
'	- v1 code fixes   *LTek
'	- nFozzy Physics and Fleep sound *Robby
'	- FlexDMD *Scutters, Oqqsan, LTek
'   - VR Room *Ext2k
'	- moved LUT controls to F12 User Settigns Menu (disabled Magna buttons)  *LTek
'	- User Settings Menu option to tweak FlexDMD ms timer rate, may be needed for some systems.
'
' 2024-06-04 // 1.0
'   - initial reskin of Sword of Fury into Fallout Season 1 buy Clairvius and PuP-Pack by Gman77 

''
' ////////////////////////////////////////////////////////////////////////////////////////
' *** Reccomended Visual Settings ***
'	Tone Mapping : Tony McMapFace
'	LUT : "Fleep Natural Dark 1" or "Fleep Natural Dark 2"
' ////////////////////////////////////////////////////////////////////////////////////////
' *** Basic Rules ***
'
'	Multiball Scores x2 or x3
'	... lights Handy Bonus and Drop Target for Jackpot
'
'	Vault Drop Targets score 50K to 250K
'	... Drop targets open areas to enter vault for Jackpot
'	... Drop all to advance Mr Handy bonus and Multiplier
'
'	Nuka-Cap tunnel for a chance at 1 Million Points
'
'	Light Up Mr Handy to win trip to Los Angeles
'
'	Center Ramp advances Mr Handy bonus 20K to 500K points
'
'	Hideout Under Center Ramp advances or collects values from 20K to 500K
'	... Outlanes score Hideout values lit
'
'	H-A-N-D-Y lights
'	... scores lit value from 50K to EXTRA BALL and lights Handy Bonus
'	... Inner Return Lanes light next letter in and start flashing LA Tunnel
'
' ////////////////////////////////////////////////////////////////////////////////////////
' ////////////////////////////////////////////////////////////////////////////////////////
Option Explicit
Randomize
SetLocale 1033			'Forces VBS to use english to stop crashes.

'*************************************************************************
' USER Config - Only edit these setting if you know you must
'*************************************************************************
const UseFlexDMD = 1			' 0=off,1=on
const FlexDMDHD = 1				' 0=128x32,1=256x64
'*************************************************************************
' DANGER - DO NOT GO BELOW THIS LINE - A Ghoul Will Eat You!
'*************************************************************************
const cGameName = "fallout" 	' Name of both PuP Pack and the table ROM alias : add to VPMAlias.txt... fallout,swrds_l2
const cROMname = "swrds_l2"		' original name of ROM needed for specific sounds/dmd settings
const ShowLoadingBanner = 1		' 0 = no, 1 = yes // shows while ROM is initializing; setting to 'no' will not make you able to play sooner.
LoadingFlasher.ImageA = "Post-Nuke_News"		' image for banner, set to "" to disable
const cStartAudioClip = "vo_SecuritronYouveDoneTheRightThing"  ' audio clip to play when game starts
dim bRulesActive		' tracks if Rules popup is onscreen
'*******************************************
'  ZOPT: User Options
'*******************************************
Dim VolumeDial : VolumeDial = 0.5           		' 0.1 to 1.0 :: Overall Mechanical sound effect volume.
Dim BallRollVolume : BallRollVolume = 0.5   		' 0.1 to 1.0 :: Overall ball rolling volume. Value between 0 & 1
Dim RampRollVolume : RampRollVolume = 2.0 			' 0.1 to 1.0 :: Overall ramp rolling volume. Value between 1 & 5
Dim LUTimage : LUTimage = 0
Dim PuP : PuP = 1
Dim usePuP : usePUP   = False        ' enable Pinup Player functions for this table
' -- LTek's modified Fleep code settings --
Dim ArchSoundFactor	:ArchSoundFactor = 5.0			' 0.1 to 5.0 :: Ball rolling on Arches, like Loops and Lanes : sound factor / volume multiplier; must not be zero
Dim RollingSoundFactor : RollingSoundFactor = 1.0	' 0.1 to 5.0 :: Playfield Ball rolling sound factor / volume multiplier; must not be zero 5 max
'*************************************************************************
' // User Settings in F12 menu //
'*************************************************************************
Sub Table1_OptionEvent(ByVal eventId)
	' 10.8 only : called when options are tweaked by the player.
	'... 0: game has started, good time to load options and adjust accordingly
	'... 1: an option has changed
	'... 2: options have been reseted
	'... 3: player closed the tweak UI, good time to update staticly prerendered parts
'	Table1.Option arguments... option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional Array for Menu Text fot that Option/Setting (strings)
'
	' -- pause processes not needed to run --
    If eventId = 1 Then DisableStaticPreRendering = True
	FlexDMDTimer.Enabled = False ' stop FlexDMD
	Controller.Pause = True

	' --- Table LUT --- 
	LUTimage = Table1.Option("Table Color Setting (LUT)", 0, 15, 1, 0, 0, Array("Fleep Natural Dark 1","Fleep Natural Dark 2","Fleep Warm Dark","Fleep Warm Bright","Fleep Warm Vivid Soft","Fleep Warm Vivid Hard","Skitso Natural and Balanced","Skitso Natural High Contrast","3rdaxis Referenced THX Standard","CalleV Punchy Brightness and Contrast","HauntFreaks Desaturated","Tomate Washed Out","VPW Original 1 to 1","Bassgeige","Blacklight","B&W Comic Book"))
	Table1.ColorGradeImage = "LUT" & LUTimage
		'0 = Fleep Natural Dark 1
		'1 = Fleep Natural Dark 2
		'2 = Fleep Warm Dark
		'3 = Fleep Warm Bright
		'4 = Fleep Warm Vivid Soft
		'5 = Fleep Warm Vivid Hard
		'6 = Skitso Natural and Balanced
		'7 = Skitso Natural High Contrast
		'8 = 3rdaxis Referenced THX Standard
		'9 = CalleV Punchy Brightness and Contrast
		'10 = HauntFreaks Desaturated
		'11 = Tomate Washed Out 
		'12 = VPW Original 1 to 1
		'13 = Bassgeige
		'14 = Blacklight
		'15 = B&W Comic Book	

	' --- Volume Controls ---	
	VolumeDial= Table1.Option("Mechanical Volume", 0.1, 1.0, 0.1, 0.5,1)				' Value 0.1 to 1 :: Overall Mechanical sound effect volume. 
	BallRollVolume = Table1.Option("Playfield Roll Volume", 0.1, 1, 0.1, 0.6,0)			' Value 0.1 to 1 :: General Ball Rolling Volume
	RampRollVolume = Table1.Option("Ramp Roll Volume Boost", 1, 5, 1, 4,0)				' Value 1 to 5 :: Ramp specific Rolling Volume
	RollingSoundFactor = Table1.Option("General Roll Vol Boost", 0.1, 1.0, 0.1, 0.5,0)	' Value 1 to 5 :: Fleep code default 1.1 / 5
	ArchSoundFactor = Table1.Option("Arch Roll Vol Boost", 0.1, 1.0, 0.1, 0.5,0)		' Value 1 to 5 :: Fleep code default 0.025 / 5	

	' --- PuP Pack ---
	PuP = Table1.Option("PuP Pack", 1, 2, 1, 1, 0, Array("Enabled","Disabled"))
	If PuP = 2 Then usePuP = False	' default set to true

	' --- VR Room ---
	VRRoom = Table1.Option("VR Room", 1, 3, 1, 3, 0, Array("Minimal", "Mixed Reality", "Street"))
	SetupVRRoom

	' -- start paused processes --
	FlexDMDTimer.Enabled = True
	Controller.Pause = False
    If eventId = 3 Then DisableStaticPreRendering = False

End Sub

' **** Added Music routines *****

Dim Track(18)

Track(1) =  "./pupvideos/fallout/Nuka-Jukebox/Bing Crosby And The Andrew Sisters,  Pistol Packin' Mama.mp3"
Track(2) =  "./pupvideos/fallout/Nuka-Jukebox/Diamond City Radio - Atom Bomb Baby.mp3"
Track(3) =  "./pupvideos/fallout/Nuka-Jukebox/Diamond City Radio - Crawl Out Through the Fallout.mp3"
Track(4) =  "./pupvideos/fallout/Nuka-Jukebox/Diamond City Radio - Dear Hearts and Gentle People.mp3"
Track(5) =  "./pupvideos/fallout/Nuka-Jukebox/Diamond City Radio - Right Behind You Baby.mp3"
Track(6) =  "./pupvideos/fallout/Nuka-Jukebox/Diamond City Radio - Rocket 69.mp3"
Track(7) =  "./pupvideos/fallout/Nuka-Jukebox/Diamond City Radio - Sixty Minute Man.mp3"
Track(8) =  "./pupvideos/fallout/Nuka-Jukebox/Diamond City Radio - The Wanderer.mp3"
Track(9) =  "./pupvideos/fallout/Nuka-Jukebox/Diamond City Radio - Undecided.mp3"
Track(10) =  "./pupvideos/fallout/Nuka-Jukebox/Diamond City Radio - Uranium Rock.mp3"
Track(11) =  "./pupvideos/fallout/Nuka-Jukebox/Diamond City Radio - Whole Lotta Shakin' Goin' On.mp3"
Track(12) =  "./pupvideos/fallout/Nuka-Jukebox/Fallout 3 Soundtrack - Dear Hearts and Gentle People.mp3"
Track(13) =  "./pupvideos/fallout/Nuka-Jukebox/Fallout 3 Soundtrack - I dont want to set the World on Fire.mp3"
Track(14) =  "./pupvideos/fallout/Nuka-Jukebox/Fallout 3 Soundtrack - Way Back Home.mp3"
Track(15) =  "./pupvideos/fallout/Nuka-Jukebox/Fallout New Vegas - Ain't That a Kick in the Head - Dean Martin.mp3"
Track(16) =  "./pupvideos/fallout/Nuka-Jukebox/Fallout New Vegas - Blue Moon - Frank Sinatra.mp3"
Track(17) =  "./pupvideos/fallout/Nuka-Jukebox/Fallout New Vegas (Bonus Trailer) - Orange Colored Sky - Nat King Cole.mp3"
Track(18) =  "./pupvideos/fallout/Nuka-Jukebox/The Five Stars - Atom Bomb Baby (Fallout 4 Gameplay Showcase).mp3"

Sub NextTrack(startSound)
	Randomize
	If startSound Then
		PlayMusic "./pupvideos/fallout/Nuka Sound Effects/" & (INT(RND*6)+1) & ".mp3"
	else
		PlayMusic Track(INT(RND*18)+1)
	End If
End Sub

Sub Table1_MusicDone
    NextTrack(0)
End Sub

'*************************** PuP Settings for this table ********************************
Dim cPuPPack: Dim PuPlayer
Dim PUPStatus: PUPStatus = False ' dont edit this line!!!
 ' Dim usePuP : usePUP   = true         ' enable Pinup Player functions for this table
cPuPPack = cGameName    ' name of the PuP-Pack / PuPVideos folder for this table
'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////
' This is used for the startup and control of Pinup Player

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub

Sub pupevent(EventNum)
	if (usePUP=false or PUPStatus=false) then Exit Sub
    'PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

' ******* How to use PUPEvent to trigger / control a PuP-Pack *******
' Usage: pupevent(EventNum)
' EventNum = PuP Exxx trigger from the PuP-Pack
' Example: pupevent 102
' This will trigger E102 from the table's PuP-Pack
' DO NOT use any Exxx triggers already used for DOF (if used) to avoid any possible confusion
'************ PuP-Pack Startup **************
'debug.print "usePuP : " & usePuP & " // PUPStatus : " & PUPStatus
If usePUP = True Then PuPStart(cPuPPack) 'Check for PuP - If found, then start Pinup Player / PuP-Pack

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'******************************************************
'  ZVAR: Constants and Global Variables
'******************************************************
Const BallSize = 50		'Ball size must be 50
Const BallMass = 1		'Ball mass must be 1
Const tnob = 15			'Total number of balls on the playfield including captive balls.
Const lob = 1			'Total number of locked balls

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

Dim DesktopMode:DesktopMode = Table1.ShowDT AND RenderingMode <> 2 
Dim UseVPMColoredDMD
UseVPMColoredDMD = DesktopMode

LoadVPM"00990300","S11.VBS",3.10

'********************
'Standard definitions
'********************
Const UseSolenoids = 2
Const UseLamps = 1
Const UseGI=0
Const UseSync = 0
Const HandleMech = 0
' Standard Sounds
Const SSolenoidOn = ""			'Sound sample used for this, obsolete.
Const SSolenoidOff = ""			' ^
Const SFlipperOn = ""			' ^
Const SFlipperOff = ""			' ^
Const SCoin = ""				' ^

Dim bGameInPlay, bsTrough, bsL, bsR, dtDrop, x, BallFrame, plungerIM, bsSaucer, bsLock, MagicTunnelLight

Set MotorCallback = GetRef("UpdateMultipleLamps")

'******************************************************
'  ZTIM: Timers
'******************************************************

'The FrameTimer interval should be -1, so executes at the display frame rate
'The frame timer should be used to update anything visual, like some animations, shadows, etc.
'However, a lot of animations will be handled in their respective _animate subroutines.

Dim FrameTime, InitFrameTime
InitFrameTime = 0

Dim PlungerPulled, BallInLane

FrameTimer.Interval = -1
Sub FrameTimer_Timer() 
	FrameTime = gametime - InitFrameTime 'Calculate FrameTime as some animuations could use this
	InitFrameTime = gametime	'Count frametime
	'Add animation stuff here
	RollingUpdate				'Update rolling sounds
	'DoSTAnim					'Standup target animations
	'DoDTAnim					'Drop target animations

	' tracks plunger position to properly engage rocket launcher animation
	If Plunger.position > 10 and BallInLane = 1 Then
		PlungerPulled = 1
		LauncherPullBack
		'debug.print "FrameTimer, Plunger PullBack"
	End If

End Sub

'The CorTimer interval should be 10. It's sole purpose is to update the Cor (physics) calculations
CorTimer.Interval = 10
'dim imagestatic : imagestatic = 1
'dim count

Sub CorTimer_Timer()

' readout static animation, cycles through 4 images
'	count = count + 1
'	If count mod 4 = 1 Then Flasher_ReadoutStatic.imageA = "PF_ReadoutStatic" & imagestatic
'	imagestatic = imagestatic + 1
'	if imagestatic > 4 Then imagestatic = 1

' If flasherheight > 0 then flasherheight = flasherheight + 0.1 : Flasher_Readout.height = - flasherheight-0.1 :  if flasherheight > 2 then flasherheight = 0

Cor.Update
End Sub

' ///////////////////////////////////////
'  Table Initialization
' ///////////////////////////////////////
Sub Table1_Init
	vpmInit Me
	If UseFlexDMD Then FlexDMD_Init
	With Controller
		.GameName = cROMName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Fallout Vault Edition 2.0 (LTek 2024)"
		.HandleKeyboard=0
		.ShowTitle=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.HandleMechanics=0				
		.Games(cROMname).Settings.Value("sound") = 0	'Disable Rom Sounds - when using the original ROM name
		.Games(cROMname).Settings.Value("samples") = 0	'Disable Rom Samples - when using the original ROM name

        'ExternalEnabled = Controller.Games(cGameName).Settings.Value("showpindmd")

		If UseFlexDMD Then .Games(cROMname).Settings.Value("showpindmd")=0

        'If usePUP = True Then .Games(cROMname).Settings.Value("showpindmd") = 1  ' uncomment this line when "PuP capture"used but NOT when Controller W triggers or script PuPevent
		If usePUP = False Then .PuPHide = 1 	' 1 = disable pupb2s plugin, 0 = enable (default)

		If DesktopMode then
		   .Hidden = 0 
			else
			If B2SOn then
				.Hidden = 1
			else
				.Hidden = 0
			End If
        End If
       
		.Run GetPlayerHWnd

    End With

	On Error Resume Next
	'Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0
'
' -- table specific startup settings --
'
	bGameInPlay = False ' tell table its not in play yet
 	RulesWall.IsDropped = True	' Left Magna Rules pop up
	LoadingBanner
	PlaySound cStartAudioClip
	MrHandyTimer.enabled = 1  ' start Mr Handy animation
		' Leds.Enabled=1
'
' -- generic table startup settings --
'
	'Main Timer init
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

    ' Nudging
    vpmNudge.TiltSwitch=swTilt
    vpmNudge.Sensitivity=3
    vpmNudge.TiltObj=Array(LeftSlingshot)
 
    ' Trough
    Set bsTrough=New cvpmBallStack
    bsTrough.InitSw 10,11,12,13,0,0,0,0
    bsTrough.InitKick BallRelease, 80, 6
    'bsTrough.InitEntrySnd "Solenoid", "Solenoid"
    bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("fx_Solenoid",DOFContactors)
    bsTrough.Balls=3

 	Set bsSaucer=New cvpmBallStack
	bsSaucer.InitSaucer S54,54,180,2
	bsSaucer.InitExitSnd SoundFX("ballrelease",DOFContactors),SoundFX("SolOn",DOFContactors) 

 	Set bsLock=New cvpmVLock
	bsLock.InitVLock Array(TriggerA,TriggerB),Array(Lock1,Lock2),Array(55,56)
	bsLock.InitSnd SoundFX("SolOn",DOFContactors),SoundFX("SolOn",DOFContactors)
	bsLock.ExitDir=0 
	bsLock.ExitForce=25
	bsLock.CreateEvents"bsLock"
 
    KickBack.Pullback	    ' Init Kickback

'
' -- For Desktop mode --
'
	If Table1.ShowDT = true then
		debug.print "View = Desktop"
		Prim_WireRampExtended.visible = True	' show static ramp in VR and Desktop
		Prim_WireRampMoves.visible = False		' hide moving ramp in VR and Desktop
		B6.Visible = 1
		B7.Visible = 1
		B41.Visible = 1
		B42.Visible = 1
		B51.Visible = 1
		B52.Visible = 1
		B57.Visible = 1
		B58.Visible = 1
		B59.Visible = 1
		B60.Visible = 1
		B61.Visible = 1
		B62.Visible = 1
		B63.Visible = 1
		B64.Visible = 1
		Jackpot.Visible = 1
	else
'
' -- For Fullscreen / Cabinet mode --
'
		debug.print "View = Fullscreen / Cabinet"
		Prim_WireRampExtended.visible = False	' hide static ramp 
		Prim_WireRampMoves.visible = True		' show moving ramp
		B6.Visible = 0
		B7.Visible = 0
		B41.Visible = 0
		B42.Visible = 0
		B51.Visible = 0
		B52.Visible = 0
		B57.Visible = 0
		B58.Visible = 0
		B59.Visible = 0
		B60.Visible = 0
		B61.Visible = 0
		B62.Visible = 0
		B63.Visible = 0
		B64.Visible = 0
		Jackpot.Visible = 0
	End If
'
' -- For VR mode --
'
	If Renderingmode = 2 Then	' 0 = Normal 2D, 1 = Stereo 3D, 2 = VR
		debug.print "View = VR"
		Prim_WireRampExtended.visible = True	' show static ramp in VR and Desktop
		Prim_WireRampMoves.visible = False		' hide moving ramp in VR and Desktop
		PinCab_WallRight.visible = 0
		PinCab_WallLeft.visible = 0
		PinCab_RailRight.visible = 0
		PinCab_RailLeft.visible = 0
		PinCab_LockdownBar.visible = 0
		PinCab_Body.visible = 0
	End If

End Sub

Sub Table1_Paused : Controller.Pause=True : End Sub
Sub Table1_unPaused : Controller.Pause=False : End Sub

Sub Table1_exit()
	Controller.Games(cROMname).Settings.Value("sound") = 1     					'enable ROM soundsp																			 
	Controller.Games(cROMname).Settings.Value("samples") = 1   					'enable ROM samples
	Controller.Games(cROMname).Settings.Value("showpindmd") = ExternalEnabled		'restore DMD setting

	'Close FlexDMD
	If Not FlexDMD Is Nothing Then
		FlexDMD.Show = False
		FlexDMD.Run = False
		FlexDMD = Null
	End If

	Controller.Stop		
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

' ////////////////////////////////////////////////////
' -- Moving Ramp --
' ////////////////////////////////////////////////////
' To calculate a path along a straight line in 3D space, we calculate XYZ values by adjusting only parameter t:
' thus if t = 0.0, the equation becomes p = p1 (starting point) -or- t = 1.0, the equation becomes p = p2 (ending point)
' t + (1-t) is always equal to 1.0 so we always get 100% for both points (be at the end)
' For any value of t, you are taking t% of p2 and the leftover percentage of p1.
' so when t = 0.25, the equation becomes p = 0.75*p1 + 0.25*p2. The point p is 25% away from p1 and 75% away from p2.
'
' px = (1-t)*p1x + t*p2x
' py = (1-t)*p1y + t*p2y
' pz = (1-t)*p1z + t*p2z
'
' 
' -- Set Below settings to your needs --
' 
' enter your prim coordinates (xyz) below
	' start
	const p1x = 1021.153
	const p1y = 428
	const p1z = 140
	' end
	const p2x = 494
	const p2y = 570
	const p2z = 121

	RampTimer.interval = 5			' timer dictates movement speed lower # = faster
	const RampStepSize = 0.006		' step size, remember, smaller steps "slow" movement down for a given interval; Default = 0.004
	const RetractStop = 0	' default = 0 (origin) // set value for retraction to stop. Small increments make a large difference when steps are small.

'
' -- core code --
' 
' NOTES: 
	' Bounce3_Hit triggers ramp retraction when ball hits is (ball drop off Ramp)
	' BottomDiverter triggers ramp to extend
											  
dim t : t = 0	' tracks distance from starting point; 0.0 to 1.0 in any increment. 1.0 = 100%
dim RampExtend, RampWait, px,py,pz
RampWait = 0
Sub Bounce3_Hit()  ' at end of retractable ramp
	'debug.print "Bounce3_Hit"
	If RampExtend = 1 Then RampWaitTimer.enabled = 1
End Sub

Sub RampWaitTimer_Timer()
	'debug.print "RampWaitTimer"
	RampWait = RampWait + 1
	If RampWait = 2 Then 
		RampExtend = 0
		RampTimer.enabled = 1
		RampWait = 0
		RampWaitTimer.enabled = 0
	End IF
End Sub

'
'Sub BottomDiverter_animate	' triggers Ramp to Extend 
'	debug.print "BottomDiverter_animate, t = " & t
'
'   If BottomDiverter.CurrentAngle <> BottomDiverter.StartAngle AND RampExtend = 0 Then	' only extend if retracted, and diverter closed
'	debug.print "BottomDiverter closed, t = " & t
'		RampExtend = 1
'		RampTimer.enabled = 1
'   End If
'
'   If BottomDiverter.CurrentAngle = BottomDiverter.StartAngle AND RampExtend = 1 Then	' only retract when diverter open
'	debug.print "BottomDiverter open, t = " & t
'		RampExtend = 0
'		RampTimer.enabled = 1
'   End If
' End sub

Sub RampTimer_Timer()
	If Table1.ShowDT = true Then RampTimer.enabled = 0 : Exit Sub  ' do not run in Desktop mode	
	If RenderingMode = 2 Then RampTimer.enabled = 0 : Exit Sub  ' do not run in VR mode	
						   
	PlaySound "fx_motor",0,3,0,0.25

	If RampExtend = 1 Then	' Extend
		If t >= 1 Then RampTimer.enabled = 0 : RampExtend = 1 : Exit Sub	' stop when at end (t=1)
		t = t+RampStepSize	' Extend Ramp
	'debug.print "Ramp Extending, t = " & t
	Else 	' Retract
		If t <= RetractStop Then RampTimer.enabled = 0 : RampExtend = 0 : Exit Sub	' stop when at start (t-0)
		t = t-RampStepSize	' Retract
	'debug.print "Ramp Retracting, t = " & t
	End If

' -- move primative --
	px = (1-t)*p1x + t*p2x
	py = (1-t)*p1y + t*p2y
	pz = (1-t)*p1z + t*p2z
	Prim_WireRampMoves.X=px   ' insert name of Prim you are animating/moving
	Prim_WireRampMoves.Y=py
	Prim_WireRampMoves.Z=pz
'debug.print "t = " & t
'debug.print "XYZ... " & px & " , " & py & " , " & pz
End Sub
'
' ////////////////////////////////////////////////////
'
' ////////////////////////////////////////////////////
' -- Loading Banner --
' ////////////////////////////////////////////////////

dim iLoading : iLoading = 0

Sub LoadingFlasher_Timer() ' runs LoadingBanner sub while timer is on
    LoadingBanner
End Sub

Sub LoadingBanner 	' display banner on playfield while DMD loads, distracts players
	If ShowLoadingBanner = 0 Then Exit Sub
	iLoading = iLoading + 1		' increment count
	LoadingFlasher.timerenabled = False
	LoadingFlasher.visible = False
	LoadingWall.visible = False
	If iLoading < 6 Then  ' 5 seconds; 1000ms timer
		LoadingFlasher.timerenabled = True 
		LoadingFlasher.visible = True  	' show on playfield
		LoadingWall.visible = True		' wall required since flashers are semi-transparent
	  Else							' needed # of cycles reached
		LoadingFlasher.timerenabled = False
		LoadingFlasher.visible = False
		LoadingWall.visible = False
	End If
End Sub

'**********
' Keys
'**********

Sub Table1_KeyDown(ByVal keycode)

	If EnterInitials <> 1 Then	' Intial Entry does not work without this
		If Keycode = LeftFlipperKey Then Controller.Switch(60) = 1
		If Keycode = RightFlipperKey Then Controller.Switch(58) = 1
	End If
	If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft			' Sets the nudge angle and power
	If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight		' ^
	If keycode = CenterTiltKey Then Nudge 0, 1 : SoundNudgeCenter		' ^
	If keycode = StartGameKey Then SoundStartButton
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

    If keycode = PlungerKey and bGameInPlay = true Then
		Plunger.Pullback
		LauncherPullBack  ' moves launcher back over ball
		Else SoundPlungerPull	' no ball in lane, empty pull back sound
	End If

        If keycode = StartGameKey and bGameInPlay = false Then Controller.Switch(4) = 1:NextTrack(1):end If
		If keycode = RightMagnaSave Then NextTrack(0) ' changes music the PuP Pack is playing
        If keycode = LeftMagnaSave Then
            If RulesFlasher.visible = False and bGameInPlay = false Then	' Show Rules on PF -only- ball not in play 
                RulesFlasher.visible = True
                RulesWall.visible = True
            else								'toggle off if is on the screen
                RulesFlasher.visible = False
                RulesWall.visible = False
            End If
        End If
   
    If vpmKeyDown(keycode)Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If keycode = LeftFlipperKey Then Controller.Switch(60) = 0
    If keycode = RightFlipperKey Then Controller.Switch(58) = 0
    If keycode = PlungerKey and bGameInPlay = true Then Plunger.Fire : LauncherShoot	'Launcher firing sound when there is a ball in shooter lane
    If keycode = PlungerKey and bGameInPlay = false Then SoundPlungerReleaseNoBall() 	'Plunger release sound when there is no ball in shooter lane
    If vpmKeyUp(keycode)Then Exit Sub
End Sub

'******************************************************
'	ZFLP: FLIPPERS
'******************************************************
Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire
		DOF 101,1
		LeftFlipper1.RotateToEnd ' comment out when using Staged Flippers
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		DOF 101,0
		LeftFlipper.RotateToStart
		LeftFlipper1.RotateToStart  ' comment out when using Staged Flippers
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire
		DOF 102,1
		RightFlipper1.RotateToEnd ' comment out when using Staged Flippers
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		DOF 102,0
		RightFlipper.RotateToStart
		RightFlipper1.RotateToStart ' comment out when using Staged Flippers
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub
'
' --- beflow for Stage Flippers -- requires changes to VPMkeys file

'Sub SolULFlipper(Enabled)
'	If Enabled Then
'		LeftFlipper1.RotateToEnd
'		If leftflipper1.currentangle < leftflipper1.endangle + ReflipAngle Then 
'			RandomSoundReflipUpLeft LeftFlipper1
'		Else 
'			SoundFlipperUpAttackLeft LeftFlipper1
'			RandomSoundFlipperUpLeft LeftFlipper1
'		End If		
'	Else
'		LeftFlipper1.RotateToStart
'		If LeftFlipper1.currentangle < LeftFlipper1.startAngle - 5 Then
'			RandomSoundFlipperDownLeft LeftFlipper1
'		End If
'		FlipperLeftHitParm = FlipperUpSoundLevel
'	End If
'End Sub
'
'Sub SolURFlipper(Enabled)
'	If Enabled Then
'		RightFlipper1.RotateToEnd
'		If rightflipper1.currentangle > rightflipper1.endangle - ReflipAngle Then
'			RandomSoundReflipUpRight RightFlipper1
'		Else 
'			SoundFlipperUpAttackRight RightFlipper1
'			RandomSoundFlipperUpRight RightFlipper1
'		End If
'	Else
'		RightFlipper1.RotateToStart
'		If RightFlipper1.currentangle > RightFlipper1.startAngle + 5 Then
'			RandomSoundFlipperDownRight RightFlipper1
'		End If	
'		FlipperRightHitParm = FlipperUpSoundLevel
'	End If
'End Sub

'Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	RightFlipperCollide parm
End Sub

Sub LeftFlipper1_Collide(parm)
	LeftFlipperCollide parm
End Sub

Sub RightFlipper1_Collide(parm)
	RightFlipperCollide parm
End Sub

'******************************************************
'  ZANI: Misc Animations
'******************************************************

Sub LeftFlipper_Animate
	dim a: a = LeftFlipper.CurrentAngle
	FlipperLSh.RotZ = a
	LFLogo.RotZ = LeftFlipper.CurrentAngle
	'Add any left flipper related animations here
End Sub

Sub RightFlipper_Animate
	dim a: a = RightFlipper.CurrentAngle
	FlipperRSh.RotZ = a
	RFlogo.RotZ = RightFlipper.CurrentAngle
	'Add any right flipper related animations here
End Sub

Sub LeftFlipper1_Animate
    dim a: a = LeftFlipper1.CurrentAngle    
    LFLogo1.RotZ = a
End Sub

Sub RightFlipper1_Animate
    dim a: a = RightFlipper1.CurrentAngle    
    RFLogo1.RotZ = a
End Sub
'*********
'Solenoids
'*********

SolCallback(1)="bsTrough.SolIn"
SolCallback(2)="bsTrough.SolOut"
SolCallback(4)="bsSaucer.Solout"
SolCallback(5)="dtDrop.SolunHit 2,"
SolCallback(6)="bsLock.SolExit"
SolCallback(7)="vpmSolSound SoundFX(""fx_knocker"",DOFKnocker),"
SolCallback(8)="dtDrop.SolunHit 3,"
SolCallback(9)="Flash9"
SolCallback(10)="SolGIUpdate"
'SolCallback(11)="SolFlasher10" 'lion man sword on backglass
'REM SolCallback(12)="SolACSelect"
SolCallback(14)="SolKickback"
SolCallback(15)="dtDrop.SolunHit 1,"
SolCallback(16)="Flash16" 'flasher below kickback insert
SolCallback(17)="vpmSolDiverter BottomDiverter,True,"
SolCallback(18)="vpmSolSound SoundFX(""fx_slingshot"",DOFContactors),"
SolCallback(19)="vpmSolDiverter TopDiverter,True,"
SolCallback(20)="vpmSolSound SoundFX(""fx_slingshot"",DOFContactors),"
SolCallback(21)="dtDrop.SolunHit 4,"
SolCallback(22)="dtDrop.SolunHit 5,"
SolCallback(25)="Flash25" 'Bullet Holes in Value Door, upper left PF
SolCallback(26)="Flash26" 'SolFlasher 2C
SolCallback(27)="Flash27" 'SolFlasher 4C
SolCallback(28)="Flash28" 'SolFlasher 3C
SolCallback(29)="Flash29" 'SolFlasher 5C
SolCallback(30)="Flash30" 'SolFlasher 6C
SolCallback(31)="Flash31" 'SolFlasher 7C
SolCallback(32)="Flash32" 'SolFlasher 8C

SolCallback(sLRFlipper) = "SolRFlipper"	'Right Flipper
SolCallback(sLLFlipper) = "SolLFlipper"	'Left Flipper
'SolCallback(sURFlipper) = "SolURFlipper"	'Upper Right Flipper - uncomment for Staged Flippers
'SolCallback(sULFlipper) = "SolULFlipper"	'Upper Left Flipper - uncomment for Staged Flippers

Sub Flash9(enabled)
	If enabled Then
		F9.State = 1
		F9a.State = 1
	else
		F9.State = 0
		F9a.State = 0
	End If
End Sub

Sub Flash16(enabled)
	If enabled Then
		F16.State = 1
		F16a.State = 1
	else
		F16.State = 0
		F16a.State = 0
	End If
End Sub

Sub Flash25(enabled)
	If enabled Then
		F25.State = 1
		F25a.State = 1
		F25b.State = 1
		F25b1.State = 1
		F25b2.State = 1
		F25b3.State = 1
		F25b4.State = 1
		F25b5.State = 1
		F25b6.State = 1
		F25b7.State = 1
		F25b8.State = 1
		F25b9.State = 1
		F25b10.State = 1
		F25b11.State = 1
		F25b12.State = 1
		F25b13.State = 1
		F25b14.State = 1
		F25b15.State = 1
		F25b16.State = 1
		F25b17.State = 1
		F25b18.State = 1
		F25b19.State = 1
		F25b20.State = 1
		F25b21.State = 1
		F25b22.State = 1
		F25b23.State = 1
		F25b24.State = 1
		F25b25.State = 1
		F25b26.State = 1
		F25b27.State = 1
		F25b28.State = 1
		F25b29.State = 1
		F25b30.State = 1
		F25b31.State = 1
		F25b32.State = 1
		F25b33.State = 1
		F25b34.State = 1
		F25b35.State = 1
		F25b36.State = 1
		F25b37.State = 1
		F25b38.State = 1
		F25b39.State = 1
	else
		F25.State = 0
		F25a.State = 0
		F25b.State = 0
		F25b1.State = 0
		F25b2.State = 0
		F25b3.State = 0
		F25b4.State = 0
		F25b5.State = 0
		F25b6.State = 0
		F25b7.State = 0
		F25b8.State = 0
		F25b9.State = 0
		F25b10.State = 0
		F25b11.State = 0
		F25b12.State = 0
		F25b13.State = 0
		F25b14.State = 0
		F25b15.State = 0
		F25b16.State = 0
		F25b17.State = 0
		F25b18.State = 0
		F25b19.State = 0
		F25b20.State = 0
		F25b21.State = 0
		F25b22.State = 0
		F25b23.State = 0
		F25b24.State = 0
		F25b25.State = 0
		F25b26.State = 0
		F25b27.State = 0
		F25b28.State = 0
		F25b29.State = 0
		F25b30.State = 0
		F25b31.State = 0
		F25b32.State = 0
		F25b33.State = 0
		F25b34.State = 0
		F25b35.State = 0
		F25b36.State = 0
		F25b37.State = 0
		F25b38.State = 0
		F25b39.State = 0
	End If
End Sub

Sub Flash26(enabled)
	If enabled Then
		L23.State = 1
		L24.State = 1
	else
		L23.State = 0
		L24.State = 0
	End If
End Sub

Sub Flash27(enabled)
	If enabled Then
		F27.State = 1
		F27a.State = 1
	else
		F27.State = 0
		F27a.State = 0
	End If
End Sub

Sub Flash28(enabled)
	If enabled Then
		F28.State = 1
		F28a.State = 1
	else
		F28.State = 0
		F28a.State = 0
	End If
End Sub

Sub Flash29(enabled)
	If enabled Then
		F29.State = 1
		F29a.State = 1
	else
		F29.State = 0
		F29a.State = 0
	End If
End Sub

Sub Flash30(enabled)
	If enabled Then
		F30.State = 1
		F30a.State = 1
	else
		F30.State = 0
		F30a.State = 0
	End If
End Sub

Sub Flash31(enabled)
	If enabled Then
		F31.State = 1
		F31a.State = 1
		F31b.State = 1
		F31b1.State = 1
	else
		F31.State = 0
		F31a.State = 0
		F31b.State = 0
		F31b1.State = 0
	End If
End Sub

Sub Flash32(enabled)
	If enabled Then
		F32.State = 1
		F32a.State = 1
		F32e.State = 1
		F32e1.State = 1
		F32e2.State = 1
		F32e3.State = 1
		F32f.State = 1
		F32f1.State = 1
	else
		F32.State = 0
		F32a.State = 0
		F32e.State = 0
		F32e1.State = 0
		F32e2.State = 0
		F32e3.State = 0
		F32f.State = 0
		F32f1.State = 0
	End If
End Sub

Sub SolKickBack(enabled)
    If enabled Then
		KickBack.Fire
		PlaySound SoundFX("fx_RocketLauncherSingleShot2",DOFContactors),0,3,0,0.25
		pupevent 805
		DOF 139, DOFPulse
    Else
		KickBack.PullBack
    End If
End Sub

'************
'Pop Bumpers
'************

Sub Bumper001_Hit:vpmTimer.PulseSw 28:RandomSoundBumperTop Bumper001:End Sub
Sub Bumper002_Hit:vpmTimer.PulseSw 29:RandomSoundBumperMiddle Bumper002 :End Sub
Sub Bumper003_Hit:vpmTimer.PulseSw 30:RandomSoundBumperBottom Bumper003 :End Sub

'*********
'Switches
'*********

Sub Drain_Hit()
	RampExtend = 0 : RampTimer.enabled = 1  ' retract Ramp if extended
	RandomSoundDrain Drain
    bsTrough.AddBall Me
	LauncherNoBall	' move the launcher to ready for ball
	bGameInPLay = False
End Sub
'
' -- Ball Shooter Lane / Launcher Sounds and Animation --
'
Sub BallRelease_UnHit() : RandomSoundBallRelease BallRelease : bGameInPLay = True : End Sub

Sub S14_Hit
	Controller.Switch(14)=1
	PlaySound "fx_MetalClick",0,3,0,0.25
	BallInLane = 1
End Sub

Sub S14_unHit
	Controller.Switch(14)=0
	PlaySound "fx_MetalShot_deep",0,3,0,0.25
	BallInLane = 0
	If PlungerPulled = 1 Then LauncherShoot ' launcher fired, animate
'	debug.print "Plunger Fired"
End Sub
'
' -- RAMP SOUNDS --
'

' -- 2-way Shooter Lane Wire --

Sub Trigger_WireRampShooterStart_hit
'	debug.print "Wire Ramp Shooter Start"
		If ActiveBall.VelY < 0 Then WireRampOn False	
		IF ActiveBall.VelY > 0 Then WireRampOff
End Sub
Sub Trigger_WireRampShooterEnd_hit
'	debug.print  "Wire Ramp Shooter End"
		If ActiveBall.VelY < 0 Then WireRampOff
		IF ActiveBall.VelY > 0 Then WireRampOn False
End Sub

' --  1-Way Wire Bridges --

Sub Trigger_WireRampLowerStart_hit
'	debug.print "Wire Ramp Lower Start"
	RampExtend = 1
	RampTimer.enabled = 1
	WireRampOff			 ' Stop Plastic Ramp Sound
	WireRampOn False	 ' Start Wire Ramp Sound
End Sub
Sub Trigger_WireRampLowerEnd_unhit
'	debug.print  "Wire Ramp Lower End"
	WireRampOff			' Stop Wire Ramp Sound
End Sub
Sub Trigger_WireRampUpperStart_hit
'	debug.print "Wire Ramp Upper Start"
	WireRampOff			 ' Stop Plastic Ramp Sound
	WireRampOn False	 ' Start Wire Ramp Sound
End Sub
Sub Trigger_WireRampUpperEnd_unhit
'	debug.print  "Wire Ramp Upper End"
	WireRampOff			' Stop Wire Ramp Sound
	WireRampOn True		' Start Plastic Ramp Sound
End Sub

' -- Plastic --

Sub Trigger_BlackRamp1_hit	' Entrance Only : start of black ramp, sound starts when -Y (up the PF)
	WireRampOff			' Stop Wire Ramp Sound
	WireRampOn True		' Start Plastic Ramp Sound
	PlaySound "fx_MetalHit_vibrate",0,3,0,0.25
	ShakeBomb
End Sub

Sub Trigger_BlackRamp2_hit	' Entrance & Exit : sound starts when -Y (up the PF)
	If ActiveBall.VelY < 0 Then WireRampOn True		' Start Plastic Ramp Sound
	If ActiveBall.VelY > 0 Then WireRampOff			' Stop Plastic Ramp Sound
End Sub

Sub Trigger_BlackRamp3_hit	' Exit Only : sound ends when +Y
	WireRampOff			' Stop Ramp Sound
End Sub

Sub MTLOff1_unHit()	' Exit Only : drops off upper PF,  sound ends when +Y
	WireRampOff			' Stop Ramp Sound
End sub

'
' -- CONTROLLER --
'

Sub S15_Hit:Controller.Switch(15)=1:End Sub
Sub S15_unHit:Controller.Switch(15)=0:End Sub
Sub S16_Hit:Controller.Switch(16)=1:End Sub
Sub S16_unHit:Controller.Switch(16)=0:End Sub


' - vault drop targets -

Sub SW17_Hit:DOF 138, DOFPulse:End Sub
Sub SW18_Hit:DOF 138, DOFPulse:End Sub
Sub SW19_Hit:DOF 138, DOFPulse:End Sub
Sub SW20_Hit:DOF 138, DOFPulse:End Sub
Sub SW21_Hit:DOF 138, DOFPulse:End Sub

Sub Spinner1_Spin:vpmTimer.PulseSw 23:SoundSpinner Spinner1:End Sub
Sub Spinner2_Spin:vpmTimer.PulseSw 24:SoundSpinner Spinner2:End Sub
Sub S25_Hit:Controller.Switch(25)=1:End Sub
Sub S25_unHit:Controller.Switch(25)=0:End Sub
Sub sw26_Hit:vpmTimer.PulseSw 26:PlaySound SoundFX("target",DOFContactors):End Sub
Sub sw27_Hit:vpmTimer.PulseSw 27:PlaySound SoundFX("target",DOFContactors):End Sub
Sub sw28_Hit:vpmTimer.PulseSw 28:PlaySound SoundFX("target",DOFContactors):End Sub
Sub sw29_Hit:vpmTimer.PulseSw 29:PlaySound SoundFX("target",DOFContactors):End Sub
Sub sw30_Hit:vpmTimer.PulseSw 30:PlaySound SoundFX("target",DOFContactors):End Sub
Sub sw31_Hit:vpmTimer.PulseSw 31:PlaySound SoundFX("target",DOFContactors):End Sub
Sub sw32_Hit:vpmTimer.PulseSw 32:PlaySound SoundFX("target",DOFContactors):End Sub
Sub S37_Hit:vpmTimer.PulseSw 37:End Sub
Sub S38_Hit:Controller.Switch(38)=1:End Sub
Sub S38_unHit:Controller.Switch(38)=0:End Sub
Sub Spinner7_Spin:vpmTimer.PulseSw 39:SoundSpinner Spinner7:End Sub
Sub S40_Hit:vpmTimer.PulseSw 40:End Sub
Sub S46_Hit:Controller.Switch(46)=1:End Sub
Sub S46_unHit:Controller.Switch(46)=0:End Sub
Sub S47_Hit:Controller.Switch(47)=1:End Sub
Sub S47_unHit:Controller.Switch(47)=0:End Sub
Sub S48_Hit:Controller.Switch(48)=1: End Sub
Sub S48_unHit:Controller.Switch(48)=0:End Sub
Sub S54_Hit:bsSaucer.Addball 0:End Sub
Sub S57_Hit:Controller.Switch(57)=1:End Sub
Sub S57_unHit:Controller.Switch(57)=0: 	KickBack.PullBack : End Sub   ' failsafe for Kickback
Sub S59_Hit:Controller.Switch(59)=1:End Sub
Sub S59_unHit:Controller.Switch(59)=0 : RifleHit : End Sub
Sub S61_Hit:Controller.Switch(61)=1:End Sub
Sub S61_unHit:Controller.Switch(61)=0:End Sub
Sub S62_Hit:Controller.Switch(62)=1:End Sub
Sub S62_unHit:Controller.Switch(62)=0:End Sub

'*********
' Targets
'*********
Set dtDrop=New cvpmDropTarget
dtDrop.InitDrop Array(sw17,sw18,sw19,sw20,sw21),Array(17,18,19,20,21)
dtDrop.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)
dtDrop.CreateEvents"dtDrop"

'*********
' Kicker
'*********
Sub Kicker4_Hit:Kicker4.DestroyBall:Kicker3.CreateBall:Kicker3.Kick 180,10:End Sub

'*********
' GI Lights On
'*********
dim xx
Sub SolGIUpdate(enabled)
If enabled Then
For each xx in GI:xx.State = 0: Next
        Jackpot.SetValue 1
else
For each xx in GI:xx.State = 1: Next
        Jackpot.SetValue 0
End If
End Sub

Set LampCallback = GetRef("Lamps")
	Sub Lamps
		L46.State = Controller.Lamp(46)
		L46b.State = Controller.Lamp(46)
		L46c.State = Controller.Lamp(46)
		L47.State = Controller.Lamp(47)

		L47b.State = Controller.Lamp(47)
		L47c.State = Controller.Lamp(47)
		L48.State = Controller.Lamp(48)

		L48b.State = Controller.Lamp(48)
		L48c.State = Controller.Lamp(48)
		If Table1.ShowDT = False Then
			L48a.State = Controller.Lamp(48)
			L47a.State = Controller.Lamp(47)
			L46a.State = Controller.Lamp(46)
		End If
End Sub

'**************************************
' Backglass EM Reels
'**************************************
Dim readout
Set motorcallback = GetRef("UpdateMultipleLamps") 
 
Sub UpdateMultipleLamps 
	 B6.SetValue ABS(Controller.Lamp(6) )
	 B7.SetValue ABS(Controller.Lamp(7) )
	 B41.SetValue ABS(Controller.Lamp(41) )
	 B42.SetValue ABS(Controller.Lamp(42) )
	 B51.SetValue ABS(Controller.Lamp(51) )
	 B52.SetValue ABS(Controller.Lamp(52) )
	 B57.SetValue ABS(Controller.Lamp(57) )
	 B58.SetValue ABS(Controller.Lamp(58) )
	 B59.SetValue ABS(Controller.Lamp(59) )
	 B60.SetValue ABS(Controller.Lamp(60) )
	 B61.SetValue ABS(Controller.Lamp(61) )
	 B62.SetValue ABS(Controller.Lamp(62) )
	 B63.SetValue ABS(Controller.Lamp(63) )
	 B64.SetValue ABS(Controller.Lamp(64) )
	 TDP.ObjRotZ = TopDiverter.CurrentAngle - 137
	 BDP.ObjRotZ = BottomDiverter.CurrentAngle - 137 
End Sub

'VPM Light Subs - VP10 Built in Fading Routines are used.
Set Lights(1) = L1
Set Lights(2) = L2
Set Lights(3) = L3
Set Lights(4) = L4
Set Lights(5) = L5
Set Lights(6) = L6
Set Lights(7) = L7

Set Lights(9) = L9
Set Lights(10) = L10
Set Lights(11) = L11
Set Lights(12) = L12
Set Lights(13) = L13
Set Lights(14) = L14
Set Lights(15) = L15
Set Lights(16) = L16
Set Lights(17) = L17
Set Lights(18) = L18
Set Lights(19) = L19
Set Lights(20) = L20
Set Lights(21) = L21
Set Lights(22) = L22
Set Lights(23) = L23
Set Lights(24) = L24

Set Lights(26) = L26
Set Lights(27) = L27
Set Lights(28) = L28
Set Lights(29) = L29
Set Lights(30) = L30
Set Lights(31) = L31
Set Lights(32) = L32
Set Lights(33) = L33
Set Lights(34) = L34
Set Lights(35) = L35
Set Lights(36) = L36

Set Lights(38) = L38
Set Lights(39) = L39
Set Lights(40) = L40
Set Lights(41) = L41
Set Lights(42) = L42
Set Lights(43) = L43
Set Lights(44) = L44
Set Lights(45) = L45

Set Lights(49) = L49
Set Lights(50) = L50
Set Lights(51) = L51
Set Lights(52) = L52
Set Lights(53) = L53
Set Lights(54) = L54
Set Lights(55) = L55
Set Lights(56) = L56

Sub LampTimer_Timer()
	l1f.visible = L1.state
	l2f.visible = L2.state
	l3f.visible = L3.state
	l4f.visible = L4.state
	l5f.visible = L5.state
	l6f.visible = L6.state
	l7f.visible = L7.state
	l9f.visible = L9.state
	l9f1.visible = L9.state
	l9f2.visible = L9.state
	l10f.visible = L10.state
	l10f1.visible = L10.state
	l10f2.visible = L10.state
	l11f.visible = L11.state
	l11f1.visible = L11.state
	l11f2.visible = L11.state
	l12f.visible = L12.state
	l12f1.visible = L12.state
	l12f2.visible = L12.state
	l13f.visible = L13.state
	l14f.visible = L14.state
	l15f.visible = L15.state
	l16f.visible = L16.state
	l16f1.visible = L16.state
	l16f2.visible = L16.state

	F17.visible = L17.state
	F18.visible = L18.state
	F19.visible = L19.state
	F20.visible = L20.state
	F21.visible = L21.state

	l22f.visible = L22.state
	l23f.visible = L23.state
	l24f.visible = L24.state

	l26f.visible = L26.state
	l27f.visible = L27.state
	l28f.visible = L28.state
	l29f.visible = L29.state
	l30f.visible = L30.state
	l31f.visible = L31.state
	l32f.visible = L32.state
	l33f.visible = L33.state
	l34f.visible = L34.state
	l35f.visible = L35.state
	l36f.visible = L36.state

	l38f.visible = L38.state
	l39f.visible = L39.state
	l40f.visible = L40.state
	l41f.visible = L41.state
	l42f.visible = L42.state
	l43f.visible = L43.state
	l44f.visible = L44.state
	l45f.visible = L45.state

	l49f.visible = L49.state
	l49f1.visible = L49.state
	l49f2.visible = L49.state
	l50f.visible = L50.state
	l50f1.visible = L50.state
	l50f2.visible = L50.state
	l51f.visible = L51.state
	l52f.visible = L52.state
	l53f.visible = L53.state
	l54f.visible = L54.state
	l55f.visible = L55.state
	l56f.visible = L56.state
	if MagicTunnelLight = 1 then
		F32b.state = Lightstateon		
		F32c.state = Lightstateon
	else
		F32b.state = Lightstateoff
		F32c.state = Lightstateoff
	end if

' Playfield Readout : mini terminal screen
	readout = readout + 1
	Select Case readout
		case 1 : Flasher_Readout.imageA= "PF_Readout0"
		case 1000 : Flasher_Readout.imageA= "PF_Readout1"
		case 1400 : Flasher_Readout.imageA= "PF_Readout2"
		Case 1800 : Flasher_Readout.imageA= "PF_Readout3"
		case 2200 : Flasher_Readout.imageA= "PF_Readout4"
		Case 2600 : Flasher_Readout.imageA= "PF_Readout5"
		Case 3000 : Flasher_Readout.imageA= "PF_Readout6"
		Case 3400 : readout = 0  ' start over
	End Select
End Sub

'MagicTunnelLight
Sub MTLOn_Hit()
	MagicTunnelLight = 1
	DOF 139, DOFpulse
End sub

Sub MTLOff1_Hit()
	MagicTunnelLight = 0
End sub

Sub MTLOff2_Hit()
	MagicTunnelLight = 0
End sub

Sub MTLOff3_Hit()
	MagicTunnelLight = 0
End sub


'****************
' Sling Shot Animations
'****************
Dim Lstep, RStep ' variables to increment the animation

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(Activeball)
	RandomSoundSlingshotLeft sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    vpmTimer.PulseSw 63
    LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval = 10
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

' ///////////////////////////////////
'  3D Model Animation
' ///////////////////////////////////
'
' -- Launcher --
' Important: triggering code in GameTimer
dim PullBackCounter : PullBackCounter = 0

Sub LauncherPullBack
	Launcher.Y = 1628  ' add 60 to starting coordinate
	PullBackCounter = PullBackCounter + 1
	If PullBackCounter = 1 Then PlaySound "fx_Loading",0,3,0,0,25
End Sub

Sub LauncherNoBall
	Launcher.Y = 1568
	PullBackCounter = 0
	TimerPlunger2.enabled = False ' disable VR plunger anination when no ball in shooter lane
	LauncherTimer.enabled = 0
End Sub

dim LauncherCounter

Sub LauncherShoot
	'debug.print "LauncherShoot sub"
	LauncherCounter = 10
	PlaySound "fx_MetalShot_deep",0,3,0,0,25
	'debug.print "Sub LauncherShoot"
	LauncherTimer.enabled = 1
	DOF 139, DOFPulse
End Sub

Sub LauncherTimer_Timer
	'debug.print "LauncherTimer sub"
    Launcher.TransX = LauncherCounter  ' forward/back
    Launcher.TransY = LauncherCounter /4	' up/down
    'Launcher.TransZ = LauncherCounter /2  ' left/right
    If LauncherCounter = 0 Then Me.Enabled = 0: LauncherNoBall : Exit Sub
    If LauncherCounter < 0 Then
        LauncherCounter = ABS(LauncherCounter)- 1
    Else
        LauncherCounter = - LauncherCounter + 1
    End If
End Sub

Sub BallReleaseGate_hit
	TimerPlunger.enabled = True ' enable VR plunger anination when ball enters shooter lane; we disable after the launcher animates
End Sub


'
' -- Bomb --
'
dim BombCounter
Sub ShakeBomb
	BombCounter = 10
	BombTimer.enabled = 1
	DOF 139, DOFPulse
End Sub
Sub BombTimer_Timer
    Bomb.TransZ = BombCounter /2
    Bomb.ObjRotZ = BombCounter /2
    If BombCounter = 0 Then Me.Enabled = 0:Exit Sub
    If BombCounter < 0 Then
        BombCounter = ABS(BombCounter)- 1
    Else
        BombCounter = - BombCounter + 1
    End If
End Sub
'
' -- Mr Handy --
'
dim ModelAngle, ModelStep, MyPi, TurnAngle, HandyCounter, AnimRest, AnimCycle
ModelStep = 0
HandyCounter = 0
TurnAngle = 90
AnimCycle = 180	 ' multiply with Timer Interval (50ms in this Case) to get time in Seconds.
AnimRest = 400   ' multiply with Timer Interval (50ms in this Case) to get time in Seconds.
MyPi = Round(4*ATN(1),6)/TurnAngle   ' do not modify
Sub MrHandyTimer_Timer()

	HandyCounter = HandyCounter + 1
	If HandyCounter > AnimRest then HandyCounter = 1 ' controls rest period between cycles

	If HandyCounter > 1 AND HandyCounter < AnimCycle Then  ' controls single animation cycle
		ModelAngle = SIN(ModelStep * MyPi)
		ModelStep = (ModelStep + 1)MOD 360  ' stops it from going more then 360 degrees
		MrHandy.RotX = MrHandy.RotX + ModelAngle * -0.2
		MrHandy.RotY = MrHandy.RotY + ModelAngle * -0.25
		'MrHandy.RotZ = MrHandy.RotZ + ModelAngle * -0.1
		MrHandy.Y = MrHandy.Y + ModelAngle * 0.25
		'MrHandy.X = MrHandy.X + ModelAngle * 0.25  ' left/right
		MrHandy.Z = MrHandy.Z + ModelAngle * 0.5	' up/down
	End If

End Sub
'
' -- Rifle --
'
dim RifleCounter,RifleCounter2,RifleSpeed
Sub RifleHit
	PlaySound "fx_MetalHit_light",0,3,0,0.25
	RifleCounter = 120
	RifleCounter2 = 60
	RifleTimer.enabled = 1
' rotation range 190-280, prim start is 220
End Sub
Sub RifleTimer_Timer
	' rotate
    If RifleCounter <= 0 Then Me.Enabled = 0:Exit Sub

	RifleSpeed = RifleCounter/120   ' makes # lower as time goes on, slowing movement
		If RifleCounter >0 and RifleCounter <= 20 Then Rifle.ObjRotZ = Rifle.ObjRotZ - RifleSpeed
		If RifleCounter >20 and RifleCounter <= 60 Then Rifle.ObjRotZ = Rifle.ObjRotZ + RifleSpeed
		If RifleCounter >60 and RifleCounter <= 100 Then Rifle.ObjRotZ = Rifle.ObjRotZ - RifleSpeed
		If RifleCounter >100 and RifleCounter <= 120 Then Rifle.ObjRotZ = Rifle.ObjRotZ + RifleSpeed
	
    RifleCounter = RifleCounter - 1

	' shake 
    Rifle.TransX = RifleCounter2 / 120
    'If RifleCounter2 = 0 Then Me.Enabled = 0:Exit Sub
    If RifleCounter2 < 0 Then
        RifleCounter2 = ABS(RifleCounter2)- 2
    Else
        RifleCounter2 = - RifleCounter2 + 2
    End If

End Sub

'******************************************************
'	ZPHY:  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Adding nFozzy roth physics : pt1 rubber dampeners 				https://youtu.be/AXX3aen06FM?si=Xqd-rcaqTlgEd_wx
' Adding nFozzy roth physics : pt2 flipper physics 					https://youtu.be/VSBFuK2RCPE?si=i8ne8Ao2co8rt7fy
' Adding nFozzy roth physics : pt3 other elements 					https://youtu.be/JN8HEJapCvs?si=hvgMOk-ej1BEYjJv
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
' | Force         | 12-15    |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
'
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 3-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |

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
Dim ULF : Set ULF = New FlipperPolarity

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
'
'        x.AddPt "Polarity", 0, 0, 0
'        x.AddPt "Polarity", 1, 0.05, - 2.7
'        x.AddPt "Polarity", 2, 0.16, - 2.7
'        x.AddPt "Polarity", 3, 0.22, - 0
'        x.AddPt "Polarity", 4, 0.25, - 0
'        x.AddPt "Polarity", 5, 0.3, - 1
'        x.AddPt "Polarity", 6, 0.4, - 2
'        x.AddPt "Polarity", 7, 0.5, - 2.7
'        x.AddPt "Polarity", 8, 0.65, - 1.8
'        x.AddPt "Polarity", 9, 0.75, - 0.5
'        x.AddPt "Polarity", 10, 0.81, - 0.5
'        x.AddPt "Polarity", 11, 0.88, 0
'        x.AddPt "Polarity", 12, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
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
'		x.AddPt "Polarity", 2, 0.16, - 3.7
'		x.AddPt "Polarity", 3, 0.22, - 0
'		x.AddPt "Polarity", 4, 0.25, - 0
'		x.AddPt "Polarity", 5, 0.3, - 2
'		x.AddPt "Polarity", 6, 0.4, - 3
'		x.AddPt "Polarity", 7, 0.5, - 3.7
'		x.AddPt "Polarity", 8, 0.65, - 2.3
'		x.AddPt "Polarity", 9, 0.75, - 1.5
'		x.AddPt "Polarity", 10, 0.81, - 1
'		x.AddPt "Polarity", 11, 0.88, 0
'		x.AddPt "Polarity", 12, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
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
		x.AddPt "Polarity", 2, 0.16, - 5
		x.AddPt "Polarity", 3, 0.22, - 0
		x.AddPt "Polarity", 4, 0.25, - 0
		x.AddPt "Polarity", 5, 0.3, - 2
		x.AddPt "Polarity", 6, 0.4, - 3
		x.AddPt "Polarity", 7, 0.5, - 4.0
		x.AddPt "Polarity", 8, 0.7, - 3.5
		x.AddPt "Polarity", 9, 0.75, - 3.0
		x.AddPt "Polarity", 10, 0.8, - 2.5
		x.AddPt "Polarity", 11, 0.85, - 2.0
		x.AddPt "Polarity", 12, 0.9, - 1.5
		x.AddPt "Polarity", 13, 0.95, - 1.0
		x.AddPt "Polarity", 14, 1, - 0.5
		x.AddPt "Polarity", 15, 1.1, 0
		x.AddPt "Polarity", 16, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.15, 0.85
		x.AddPt "Velocity", 2, 0.2, 0.9
		x.AddPt "Velocity", 3, 0.23, 0.95
		x.AddPt "Velocity", 4, 0.41, 0.95
		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
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

'*******************************************
' Early 90's and after

'Sub InitPolarity()
'	Dim x, a
'	a = Array(LF, RF, ULF)
'	For Each x In a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 5.5
'		x.AddPt "Polarity", 2, 0.16, - 5.5
'		x.AddPt "Polarity", 3, 0.20, - 0.75
'		x.AddPt "Polarity", 4, 0.25, - 1.25
'		x.AddPt "Polarity", 5, 0.3, - 1.75
'		x.AddPt "Polarity", 6, 0.4, - 3.5
'		x.AddPt "Polarity", 7, 0.5, - 5.25
'		x.AddPt "Polarity", 8, 0.7, - 4.0
'		x.AddPt "Polarity", 9, 0.75, - 3.5
'		x.AddPt "Polarity", 10, 0.8, - 3.0
'		x.AddPt "Polarity", 11, 0.85, - 2.5
'		x.AddPt "Polarity", 12, 0.9, - 2.0
'		x.AddPt "Polarity", 13, 0.95, - 1.5
'		x.AddPt "Polarity", 14, 1, - 1.0
'		x.AddPt "Polarity", 15, 1.05, -0.5
'		x.AddPt "Polarity", 16, 1.1, 0
'		x.AddPt "Polarity", 17, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.23, 0.85
'		x.AddPt "Velocity", 2, 0.27, 1
'		x.AddPt "Velocity", 3, 0.3, 1
'		x.AddPt "Velocity", 4, 0.35, 1
'		x.AddPt "Velocity", 5, 0.6, 1 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
'	Next
'	
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'	LF.SetObjects "LF", LeftFlipper, TriggerLF
'	RF.SetObjects "RF", RightFlipper, TriggerRF
'    ULF.SetObjects "ULF", LeftFlipper1, TriggerLF1
'End Sub

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

Dim LFPress, RFPress, ULFPress, URFPress, LFCount, RFCount
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
'   Const EOSReturn = 0.035  'mid 80's to early 90's
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
		If DebugOn Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
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

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
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
	a = Array(LS)
	Dim x
	For Each x In a
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
'	dim rx, ry
'	rx = x*dCos(angle) - y*dSin(angle)
'	ry = x*dSin(angle) + y*dCos(angle)
'	RotPoint = Array(rx,ry)
'End Function

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

'******************************************************
'	ZBRL:  BALL ROLLING AND DROP SOUNDS
'******************************************************

' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 To tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b
	Dim BOT
	BOT = GetBalls
	
	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 To tnob - 1
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(BOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(BOT)

		If BallVel(BOT(b)) > 1 And BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If
	
	' ---- Enables ball rolling sound above a given height when NOT on a wire ramp ----
'		If BallVel(BOT(b)) > 1 And BOT(b).z > 79 and OnWireRamp = false Then
'            rolling(b) = True
'            PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
'        End If
	' ---- Enables ball rolling sound on specific coordinates and specific height above playfield ----
'		If BallVel(BOT(b)) > 77 And BOT(b).z < 98 and InRect(BOT(b).x,BOT(b).y,0,0,0,375,304,580,0,580) Then   ' example code for when only wanting sounds in a specific space on the table
'            rolling(b) = True
'            PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
'        End If
	' ---- Enable Ramp Rolling on ramps above the playfield at specific height ----
	'	If BallVel(BOT(b)) > 1 AND BOT(b).z > 30 AND OnWireRamp = false THEN WireRampOn True
	' ------------------------------------------------------------------------------------
		
		' Ball Drop Sounds
		If BOT(b).VelZ <  - 1 And BOT(b).z < 55 And BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz >  - 7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If
			End If
		End If
		
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
	Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************

'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'		  * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'		  * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'		  * Create a Timer called RampRoll, that is enabled, with a interval of 100
'		  * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'		  * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'		  * To stop tracking ball
'				 * call WireRampOff
'				 * Otherwise, the ball will auto remove if it's below 30 vp units
'

Dim RampMinLoops
RampMinLoops = 4

' RampBalls
' Setup:  Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RampBalls(6,2)
Dim RampBalls(6,2)
'x,0 = ball x,1 = ID, 2 = Protection against ending early (minimum amount of updates)

'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
' Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
' Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
Dim RampType(6)

Sub WireRampOn(input)
	Waddball ActiveBall, input
	RampRollUpdate
End Sub

Sub WireRampOff()
	WRemoveBall ActiveBall.ID
End Sub

' WaddBall (Active Ball, Boolean)
Sub Waddball(input, RampInput) 'This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	Dim x
	For x = 1 To UBound(RampBalls)	'Check, don't add balls twice
		If RampBalls(x, 1) = input.id Then
			If Not IsEmpty(RampBalls(x,1) ) Then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next
	
	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 To UBound(RampBalls)
		If IsEmpty(RampBalls(x, 1)) Then
			Set RampBalls(x, 0) = input
			RampBalls(x, 1) = input.ID
			RampType(x) = RampInput
			RampBalls(x, 2) = 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			Exit Sub
		End If
		If x = UBound(RampBalls) Then	 'debug
			debug.print "WireRampOn error, ball queue Is full: " & vbNewLine & _
			RampBalls(0, 0) & vbNewLine & _
			TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbNewLine & _
			TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbNewLine & _
			TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbNewLine & _
			TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbNewLine & _
			TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbNewLine & _
			" "
		End If
	Next
End Sub

' WRemoveBall (BallId)
Sub WRemoveBall(ID) 'This subroutine is called from the RampRollUpdate subroutine and is used to remove and stop the ball rolling sounds
	'   Debug.Print "In WRemoveBall() + Remove ball from loop array"
	Dim ballcount
	ballcount = 0
	Dim x
	For x = 1 To UBound(RampBalls)
		If ID = RampBalls(x, 1) Then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		If Not IsEmpty(Rampballs(x,1)) Then ballcount = ballcount + 1
	Next
	If BallCount = 0 Then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer()
	RampRollUpdate
End Sub

Sub RampRollUpdate()	'Timer update
	Dim x
	For x = 1 To UBound(RampBalls)
		If Not IsEmpty(RampBalls(x,1) ) Then
			If BallVel(RampBalls(x,0) ) > 1 Then ' if ball is moving, play rolling sound
				If RampType(x) Then
					PlaySound("RampLoop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2) = RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			End If
			If RampBalls(x,0).Z < 30 And RampBalls(x, 2) > RampMinLoops Then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
	Next
	If Not RampBalls(0,0) Then RampRoll.enabled = 0
End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	Me.text = "on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbNewLine & _
	"1 " & TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbNewLine & _
	"2 " & TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbNewLine & _
	"3 " & TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbNewLine & _
	"4 " & TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbNewLine & _
	"5 " & TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbNewLine & _
	"6 " & TypeName(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbNewLine & _
	" "
End Sub

Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
	BallPitch = pSlope(BallVel(ball), 1, - 1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, - 4000, 60, 7000)
End Function

Sub RandomSoundRampStop(obj)
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtVol "wireramp_stop1", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtVol "wireramp_stop2", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtVol "wireramp_stop3", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************

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
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]

' -- LTek's modified sound multipliers ... moved to user settings --
' Dim RollingSoundFactor
' RollingSoundFactor = 1.1 / 5  ' ** moved to User Setting Menu

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

' -- LTek's modified sound multipliers ... moved to user settings --
' Dim ArchSoundFactor
' ArchSoundFactor = 0.025 / 5		'volume multiplier; must not be zero   ** 


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

' \\\\\\\\\\\\\\\\\\\\\\\\\\\
' -- FlexDMD --
' \\\\\\\\\\\\\\\\\\\\\\\\\\\
'
' IMPORTANT.... create a VPX timer element for FlexDMDtimer that is Enabled with Interval 10 
'
Dim FlexDMD, ExternalEnabled, FlexDMDDict, FDMDimage
Dim L1Chars, L2Chars, L3Chars , L4Chars
Dim strLine1, strLine2, strLine3, strLine4
Dim FDMDcount, FDMDseconds, FDMDcycles, EnterInitials

FDMDimage = "logo"  ' do not change this, must have starting image or code errors

'**********************************************************
'  2*7 alphannumeric + 2*7 numeric segment to flexdmd display conversion
'**********************************************************
Sub FlexDMD_Init() 'default/startup values
	' flex dmd variables
	DIm FlexDMDFont
	Dim FlexPath
	Dim FlexDMDScene
	
	'popualte arrays to hold characters to display converted from segment codes with defaults
	'28 segments
	L1Chars = Array(" "," "," "," "," "," "," ")
	L2Chars = Array(" "," "," "," "," "," "," ")
	L3Chars = Array(" "," "," "," "," "," "," ")
	L4Chars = Array(" "," "," "," "," "," "," ")	
	' populate the lookup dictionary for mapping display characters
	FlexDictionary_Init
	
	'setup flex dmd
	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
	If Not FlexDMD is Nothing Then
		
		Dim fso,curdir
		Set fso = CreateObject("Scripting.FileSystemObject")
		curDir = fso.GetAbsolutePathName(".")
		FlexPath = curDir & "\Fallout.FlexDMD\"

		FlexDMD.LockRenderThread
		
		If FlexDMDHD = 0 Then

			FlexDMD.RenderMode = 2
			FlexDMD.Width = 128
			FlexDMD.Height = 32
			FlexDMD.Clear = True
			FlexDMD.GameName = cGameName
			FlexDMD.Run = True

			Set FlexDMDScene = FlexDMD.NewGroup("Scene")
		
			'black background
				'FlexDMDScene.AddActor FlexDMD.NewImage("Back", "FlexDMD.Resources.dmds.black.png")
			
			'display font
			Set FlexDMDFont = FlexDMD.NewFont(FlexPath & "udmd-f6by12.fnt", RGB(66,159,82), vbBlue, 0)  
			' Yellow RGB(254,210,65), GreenBright RGB(132,216,124), GreenDark RGB(62,151,85),GreenFallout RGB(66,159,82)

			FlexDMDScene.AddActor(FlexDMD.NewImage("back",FlexPath & "back128.png"))
			FlexDMDScene.GetImage("back").SetBounds 0, 0, 128, 32
		
			' add a label for each line of text to display (i.e. one per segment strip).
			FlexDMDScene.AddActor(FlexDMD.NewLabel("Line1", FlexDMDFont, "       "))
			FlexDMDScene.GetLabel("Line1").SetAlignedPosition  5,2,0
			FlexDMDScene.AddActor(FlexDMD.NewLabel("Line2", FlexDMDFont,  "       "))
			FlexDMDScene.GetLabel("Line2").SetAlignedPosition 69,2,0
			FlexDMDScene.AddActor(FlexDMD.NewLabel("Line3", FlexDMDFont, "       "))
			FlexDMDScene.GetLabel("Line3").SetAlignedPosition  5,16,0
			FlexDMDScene.AddActor(FlexDMD.NewLabel("Line4", FlexDMDFont,  "       "))
			FlexDMDScene.GetLabel("Line4").SetAlignedPosition 69,16,0

			FlexDMDScene.AddActor(FlexDMD.NewImage("logo",FlexPath & "logo128.png" ))
			FlexDMDScene.GetImage("logo").SetBounds 0, 0, 128, 32
			FlexDMDScene.GetImage("logo").visible = False

			FlexDMDScene.AddActor(FlexDMD.NewImage("standby",FlexPath & "standby128.png" ))
			FlexDMDScene.GetImage("standby").SetBounds 0, 0, 128, 32
			FlexDMDScene.GetImage("standby").visible = False

			FlexDMDScene.AddActor(FlexDMD.NewImage("titan",FlexPath & "titan128.png" ))
			FlexDMDScene.GetImage("titan").SetBounds 0, 0, 256, 64
			FlexDMDScene.GetImage("titan").visible = False

			FlexDMDScene.AddActor(FlexDMD.NewImage("hounds",FlexPath & "hounds128.png" ))
			FlexDMDScene.GetImage("hounds").SetBounds 0, 0, 256, 64
			FlexDMDScene.GetImage("hounds").visible = False	

			FlexDMD.Stage.AddActor FlexDMDScene

		Else

			FlexDMD.RenderMode = 2
			FlexDMD.Width = 256
			FlexDMD.Height = 65
			FlexDMD.Clear = True
			FlexDMD.GameName = cGameName
			FlexDMD.Run = True

			Set FlexDMDScene = FlexDMD.NewGroup("Scene")
		
			' background standard black image
			'FlexDMDScene.AddActor FlexDMD.NewImage("Back", "FlexDMD.Resources.dmds.black.png")
			' background custom image
			FlexDMDScene.AddActor(FlexDMD.NewImage("back",FlexPath & "back256.png"))
			FlexDMDScene.GetImage("back").SetBounds 0, 0, 256, 64
			
			'display font
			Set FlexDMDFont = FlexDMD.NewFont(FlexPath & "udmd-f12by24.fnt", RGB(132,216,124), vbBlue, 0)  ' Yellow RGB(254,210,65), GreenGlow RGB(132,216,124)
		
			' add a label for each line of text to display (i.e. one per segment strip).
			FlexDMDScene.AddActor(FlexDMD.NewLabel("Line1", FlexDMDFont, "       "))
			FlexDMDScene.GetLabel("Line1").SetAlignedPosition  10,4,0
			FlexDMDScene.AddActor(FlexDMD.NewLabel("Line2", FlexDMDFont,  "       "))
			FlexDMDScene.GetLabel("Line2").SetAlignedPosition 138,4,0
			FlexDMDScene.AddActor(FlexDMD.NewLabel("Line3", FlexDMDFont, "       "))
			FlexDMDScene.GetLabel("Line3").SetAlignedPosition  10,32,0
			FlexDMDScene.AddActor(FlexDMD.NewLabel("Line4", FlexDMDFont,  "       "))
			FlexDMDScene.GetLabel("Line4").SetAlignedPosition 138,32,0

			' define non-background images you will use; name them anything you want.
			FlexDMDScene.AddActor(FlexDMD.NewImage("logo",FlexPath & "logo256.png" ))
			FlexDMDScene.GetImage("logo").SetBounds 0, 0, 256, 64
			FlexDMDScene.GetImage("logo").visible = False

			FlexDMDScene.AddActor(FlexDMD.NewImage("standby",FlexPath & "standby256.png" ))
			FlexDMDScene.GetImage("standby").SetBounds 0, 0, 256, 64
			FlexDMDScene.GetImage("standby").visible = False		

			FlexDMDScene.AddActor(FlexDMD.NewImage("titan",FlexPath & "titan256.png" ))
			FlexDMDScene.GetImage("titan").SetBounds 0, 0, 256, 64
			FlexDMDScene.GetImage("titan").visible = False		

			FlexDMDScene.AddActor(FlexDMD.NewImage("hounds",FlexPath & "hounds256.png" ))
			FlexDMDScene.GetImage("hounds").SetBounds 0, 0, 256, 64
			FlexDMDScene.GetImage("hounds").visible = False	

			FlexDMD.Stage.AddActor FlexDMDScene

		End If
		
		FlexDMD.Show = True
		FlexDMD.UnlockRenderThread

		' show image on start; this cover the ROM startup text, looks much better.
		FDMDseconds = 1 : FDMDcount = 1 : FDMDimage = "logo"  

	End If
End Sub

Sub FlexDMDpixels	' Copy PinMAME pixels into the DMD flasher
' ** normally at bottom of sub FlexDMDTimer_Timer ***
		Dim DMDp
		DMDp = FlexDMD.DmdColoredPixels
		If Not IsEmpty(DMDp) Then
			DMDWidth = FlexDMD.Width
			DMDHeight = FlexDMD.Height
			DMDColoredPixels = DMDp
		End If
End Sub

'**********************************************************
'  FlexDMD Timers
'**********************************************************

Sub FlexDMDTimer_Timer  ' timer enabled at end of FlexDMD_Init

	Dim ChgLED, ii, jj, num, stat', obj, b, x, chg
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED)Then
		For ii=0 To UBound(chgLED)
			num=chgLED(ii, 0) : stat=chgLED(ii, 2)': chg=chgLED(ii, 1) 
			If num < 28 Then UpdateFlexChar num, stat			
		Next

	FlexDMDupdate	' scan native DMD, change what is send to FlexDMD as needed

	End If

	FlexDMDpixels	' copy PinMAME pixels into the DMD flasher

 End Sub

Sub FlexDMDupdate

	FlexDMD.LockRenderThread

	With FlexDMD.Stage

		' use the Join function to concatenate the LxChars arrays into a string of text
	strLine1 = Join(L1Chars,"")
	strLine2 = Join(L2Chars,"")
	strLine3 = Join(L3Chars,"")
	strLine4 = Join(L4Chars,"")

	'debug.print "input::" & strLine1 & "|" & strLine2 & "|" & strLine3 & "|" & strLine4

		If 	L1Chars(4) = "U" and L1Chars(5) = "U" and L1Chars(6) = "U" Then 
			L1Chars(4) = "0"
			L1Chars(5) = "0"
			L1Chars(6) = "0"
		End If

		Select Case strLine1

		Case "KNIGHT1","KNIGHT2","KNIGHT3","KNIGHT4"	' High Score, Enter Initials
			EnterInitials = 1 : FDMDcount = 0 ': strLine1 = "KNIGHT " 
		Case "5W0RD5 ","5 8R 5 ","  8R 5 ","5 8RD5 ","  0RD5 "
			FDMDseconds = 1 : FDMDcount = 1 : FDMDimage = "logo" : EnterInitials = 0 ' show image instead of "SWORDS OF FURY" text
			strLine1 = "       " ' clear both lines
			strLine2 = "       "
		Case "LI0NMAN","LI0NMA ","LI0N 8 "
			FDMDcount = 0 : strLine1 = "  HANDY"
		Case "AVENGER","  ENGER","AVEN   ","AVE    "
			FDMDcount = 0 : strLine1 = "  VAULT"		
		Case "AVENGE "
			FDMDcount = 0 : strLine1 = " BATTLE"
		Case "K0B0LD "
			FDMDcount = 0 : strLine1 = " MUTANT"
		Case "5NUFFED"		' ,"  A5HED","5LA5   ","5LA5H  ","5LA5HED",
			FDMDcount = 0 : strLine1 = " KILLED"
		Case "0GRE5  "," 0GR65 ", "  0GRE5", " 0   65", " 0GRE5 "
			FDMDcount = 0 : strLine1 = "  GHOUL"
		Case "GOBLIN "
			FDMDcount = 0 : strLine1 = "GULPER "
		Case "1111111"
			FDMDcount = 0 : strLine1 = ")))))))"
		Case "H E R E","  D A  "
			FDMDseconds = 2 : FDMDcount = 1 : FDMDimage = "hounds"  ' show image instead of "H E R E   C U M   D A  GUARDS"	
			strLine1 = "       "
			strLine2 = "       "
		Case "2      ","22     ","222    ","2222   ","22222  "," 2     "," 22222 "," 222222","  22222","   2222","    222","     22","      2"
			strLine1 = "       "
			'FDMDseconds = 0.5 : FDMDcount = 1 : FDMDimage = "standby"  ' show image instead of series of 2's
		End Select

		Select Case strLine2

		Case "F R E E"
			' If FDMDcount = 0 Then FDMDcount = 1 : FDMDimage = "titan" ' show image instead of "F R E E  TITANS" text
		Case " GOBLIN"
			FDMDcount = 0 : strLine2 = " GULPER"
		Case "BALR0D "
			FDMDcount = 0 : strLine2 = " HOUND"
		Case "P0WER 1"
			FDMDcount = 0 : strLine2 = "P0WER!!"
		Case "1111111"
			strLine2 = "((((((("
		Case " C U M ","GUARD5 "		' text is "H E R E   C U M   DA GUARDS"
			'FDMDseconds = 0.5 : FDMDcount = 1 : FDMDimage = "hounds"  
			strLine1 = "       "
			strLine2 = "       "	
		Case "5NUFFED","5NUF   ","   FFED","5NU    "
			FDMDcount = 0 :strLine2 = " KILLED"
		Case "0GRE5  "," 0GR65 ", "  0GRE5", " 0   65", " 0GRE5 "
			FDMDcount = 0 :strLine2 = " GHOUL "
		Case "2      ","22     ","222    ","2222   ","22222  "," 2     "," 22222 "," 222222","  22222","   2222","    222","     22","      2"
			strLine2 = "       "
			'FDMDseconds = 0.5 : FDMDcount = 1 : FDMDimage = "standby"  ' show image instead of series of 2's
		End Select 

		Select Case strLine3
		Case " 80 US "
			FDMDcount = 0 : strLine3 = " BONUS " ' fix BONUS callout due to font issue; original ROM DMD is fine.
		Case "2      ","22     ","222    ","2222   ","22222  "," 2     "," 22222 "," 222222","  22222","   2222","    222","     22","      2"
			strLine3 = "       "
			'FDMDseconds = 0.5 : FDMDcount = 1 : FDMDimage = "standby"  ' show image instead of series of 2's
		Case "228,8,8,8,8,","  228,8,8,","   228,8,","    28,8,","     28,"
			strLine3 = "       "
			'FDMDseconds = 0.5 : FDMDcount = 1 : FDMDimage = "standby"  ' show image instead of series of 2's
		End Select

		Select Case strLine4
		Case "2      ","22     ","222    ","2222   ","22222  "," 2     "," 22222 "," 222222","  22222","   2222","    222","     22","      2"
			strLine4 = "       "
			'FDMDseconds = 1 : FDMDcount = 1 : FDMDimage = "standby"  ' show image instead of series of 2's
		Case "228,8,8,8,8,","  228,8,8,","   228,8,","    28,8,","     28,"
			strLine4 = "       "
			'FDMDseconds = 1 : FDMDcount = 1 : FDMDimage = "standby"  ' show image instead of series of 2's
		End Select

		' update labels
		.GetLabel("Line1").Text = strLine1
		.GetLabel("Line2").Text = strLine2
		.GetLabel("Line3").Text = strLine3
		.GetLabel("Line4").Text = strLine4

	'debug.print "output::" & strLine1 & "|" & strLine2 & "|" & strLine3 & "|" & strLine4

		FDMDcycles = (FDMDseconds * 1000) / FlexDMDTimer.Interval 	' convert seconds into # of cycles based on Timer's Interval
		If FDMDcount = 2 Then .GetImage(FDMDimage).visible = True	' show image
		If FDMDcount = 0 or FDMDcount = FDMDcycles Then .GetImage(FDMDimage).visible = False : FDMDcount = 0  	'reset counter
		If FDMDcount >0 Then FDMDcount = FDMDcount + 1
				
	End With

	FlexDMD.UnlockRenderThread

	'FlexDMDpixels

End Sub 

Sub FlexDictionary_Init
	Set FlexDMDDict = CreateObject("Scripting.Dictionary")
	With FlexDMDDict	 		 
		.Add 63, "0"
		.Add 6, "1"
		.Add 2139, "2"
		.Add 2127, "3"
		.Add 2150, "4"
		.Add 2157, "5"
		.Add 2173, "6"
		.Add 7, "7"
		.Add 2175,"8"
		.Add 2151,"9"
		.Add 8704, "1"
		.Add 2159,"9"

		.Add 32959,"0,"
		.Add 32902, "1,"
		.Add 35035, "2,"
		.Add 35023, "3,"
		.Add 35046, "4,"
		.Add 35053, "5,"
		.Add 35069, "6,"
		.Add 32903, "7,"
		.Add 35071, "8,"
		.Add 35047, "9,"
		.Add 35055, "9,"
	
		.Add 2167, "A"
		.Add 10767, "B"
		.Add 57, "C"
		.Add 8719, "D"
		.Add 121, "E"
		.Add 113, "F"
		.Add 2109, "G"
		.Add 2166, "H"
		.Add 8713, "I"
		.Add 30, "J"
		.Add 5232, "K"
		.Add 56, "L"
		.Add 1334, "M"
		.Add 4406, "N"
		' "O" = 0
		.Add 2163, "P"
		.Add 4159, "Q"
		.Add 6259, "R"
		' "S" = 5
		.Add 8705, "T"
		.Add 62, "U"
		.Add 17456, "V"
		.Add 20534, "W"
		.Add 21760, "X"
		.Add 9472, "Y"
		.Add 17417, "Z"

		.Add 50224, "V."
		.Add 54528, "X."
		.Add 34934, "H."
		.Add 34925, "S."
		.Add 41473, "T."
		.Add 41487, "D."
		.Add 32825, "C."
		.Add 32831, "O."
		.Add 32774, "1."
		.Add 34907, "2."
		.Add 34935, "A."
		.Add 43535, "B."
		.Add 37174, "N."
		.Add 34931, "P."
		.Add 39027, "R."
		.Add 53302, "W."
		.Add 34102, "M."
		.Add 32830, "U."
		.Add 32824, "L."
		.Add 41481, "I."
		.Add 32889, "E."	

		.Add 4352, "\"
		.Add 17408, "/"	
		.Add &h400, "'"
		.Add 16640, ")" ' optionally ">"
		.Add 5120, "("	' optionally "<"
		.Add 2120, "="
		.Add 10275, "?"
		.Add 2112, "-"
		.Add 10861, "$"
		.Add 6144, "<"
		.Add 65535, "#"
		.Add 32576, "*"
		.Add 10816, "+"
		.Add 32678, "."
	
		'extra values for the numeric style display in last 14 digits (first 14 are alphanumeric)
		.Add 91, "2"
		.Add 79, "3"
		.Add 102, "4"
		.Add 109, "S"
		.Add 125, "6"
		.Add 127,"8"
		.Add 111,"9"
		.Add 191,"0,"
		.Add 134, "1,"
		.Add 219, "2,"
		.Add 207, "3,"
		.Add 230, "4,"
		.Add 237, "S,"
		.Add 253, "6,"
		.Add 135, "7,"
		.Add 255, "8,"
		.Add 239, "9,"
		.Add 119,"A"
		.Add 124, "B"
		.Add 94, "D"
		.Add 64, "-"
		.Add 49, "R"
		.Add 120, "T"
		.Add 110, "Y"
		.Add 115, "P"
			'.Add 55, "N" 'lcase n, can appear as half drawn 0 in segments though so don't map it
		' three horizontal lines in segments, closest match in font is '='
		.Add 2121,"=" 
		.Add 73, "="
		.Add 2057, "="
		.Add 8, "_"
		.Add 512, "1"
	End With

End Sub

Sub UpdateFlexChar(id, value)
	'map segment code to character in LnChars arrays
	Dim chr
	if FlexDMDDict.Exists (value) then
		chr = FlexDMDDict.Item (value)

		if id < 7 then
			L1Chars(id) = chr
		elseif id < 14 then
			L2Chars(id - 7) = chr
		elseif id < 21 then
			L3Chars(id - 14) = chr
		elseif id < 28 then
			L4Chars(id - 21) = chr
		end if
	else
		chr = " " ' unrecognised transitioning characters just replace with a space
		if id < 7 then
			L1Chars(id) = chr
		elseif id < 14 then
			L2Chars(id - 7) = chr
		elseif id < 21 then
			L3Chars(id - 14) = chr
		elseif id < 28 then
			L4Chars(id - 21) = chr
		end if
	end if 	
End Sub




'----- VR Room Auto-Detect -----
Dim VR_Obj, VRRoom

' Important: place this code int Table1_OptionEvent  ' adds to user option menu
		'VRRoom = Table1.Option("VR Room", 0, 3, 1, 3, 0, Array("Off","Minimal","Mixed Reality","Street"))
		'SetupVRRoom

Sub SetupVRRoom()
	If RenderingMode = 2 Then
		If VRRoom = 3 Then
			For Each VR_Obj in VR_MinimalRoom : VR_Obj.Visible = 0 : Next
			For Each VR_Obj in VR_Mega : VR_Obj.Visible = 1 : Next
			For Each VR_Obj in VR_Sphere : VR_Obj.Visible = 0 : Next
		ElseIf VRRoom = 2 Then
			For Each VR_Obj in VR_MinimalRoom : VR_Obj.Visible = 0 : Next
			For Each VR_Obj in VR_Mega : VR_Obj.Visible = 0 : Next
			For Each VR_Obj in VR_Sphere : VR_Obj.Visible = 1 : Next
		Else
			For Each VR_Obj in VR_MinimalRoom : VR_Obj.Visible = 1 : Next
			For Each VR_Obj in VR_Mega : VR_Obj.Visible = 0 : Next
			For Each VR_Obj in VR_Sphere : VR_Obj.Visible = 0 : Next
		End If
	Else
		VRRoom = 0
		For Each VR_Obj in VR_MinimalRoom : VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VR_Mega : VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VR_Sphere : VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VR_Table : VR_Obj.Visible = 0 : Next
		
	End If
End Sub
'*****************************************************************************************************
' VR PLUNGER ANIMATION
'
' Code needed to animate the plunger. If you pull the plunger it will move in VR.
' IMPORTANT: there are two numeric values in the code that define the postion of the plunger and the 
' range in which it can move. The fists numeric value is the actual y position of the plunger primitive
' and the second is the actual y position + 135 to determine the range in which it can move.
'
' You need to to select the Primary_plunger primitive you copied from the
' template you need to select the Primary_plunger primitive and copy the value of the Y position 
' (e.g. 1269.286) into the code. The value that determines the range of the plunger is always the y 
' position + 135 (e.g. 1404).
'
'*****************************************************************************************************
Sub TimerShooter_Timer
  If PinCab_Shooter.Y < -80 then
  		PinCab_Shooter.Y = PinCab_Shooter.Y + 15
  End If
End Sub

Sub TimerShooter2_Timer
 'debug.print plunger.position
  PinCab_Shooter.Y = -215 + (5* Plunger.Position) -20
End Sub
