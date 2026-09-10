'
'  ___   _     _____ _____ _   _   _____ _____ ___  ______ 
' / _ \ | |   |_   _|  ___| \ | | /  ___|_   _/ _ \ | ___ \
'/ /_\ \| |     | | | |__ |  \| | \ `--.  | |/ /_\ \| |_/ /
'|  _  || |     | | |  __|| . ` |  `--. \ | ||  _  ||    / 
'| | | || |_____| |_| |___| |\  | /\__/ / | || | | || |\ \ 
'\_| |_/\_____/\___/\____/\_| \_/ \____/  \_/\_| |_/\_| \_|
'
'
'               Mylstar Electronics, Incorporated (1983-1984) [Trade Name: Gottlieb]
'				Alien Star / IPD No. 49 / June, 1984 / 4 Players
'               https://www.ipdb.org/machine.cgi?id=49
'
'
'********************************************************************************************************************************
'******** Work done by UnclePaulie on Hybrid version 2.0.3 - 3.0 *********
'		Completely redid the table from ground up, Corrected table dimensions from prior table.
' 		Implemented latest VPW standards (2025), physics, debounce, raytracing, SSF, GI VLM, 3D inserts, AllLamps routines, roth standups,
'		standalone compatibility, updated flipper physics, new VR cabinet and fully hybrid, Updated Flupper style bumpers and lighting,
'		Baked GI, metal walls, interior wood walls, spinner, plastic posts, and the reflections on them, also DIP adjustments, sling corrections.
'		Full details at the bottom of the script.  
'		Redbone did the playfield, plastics, and other graphics.  
'		Original scans, image assets, cab artwork stencils all used with permission from Borgdog.
'********************************************************************************************************************************
'

' PLEASE NOTE:
' - All game options can be adjusted by pressing F12 and then left flipper button during the game. Press start button to save your options.
'

Option Explicit
Randomize
SetLocale 1033			'Forces VBS to use english to stop crashes.

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0


'***********************************************************************
'* TABLE OPTIONS *******************************************************
'***********************************************************************

' ALL OPTIONS ARE IN THE F12 MENU, EXCEPT FOR THE SetDIPSwitches and ForceVR BELOW

const ForceVR = 0			'Force VR mode on desktop, 0 is off (default): 1 is to force VR on.
const SetDIPSwitches= 1		'If you want to set the dips differently, set to 1, and then hit F6 to launch.

				' These are the default DIPS that are and set at original table launch:
					'1=Coin Chute same
					'0=3rd coin chute no effect
					'0=Game mode of replay for hitting threshold
					'1=Number of balls = 3
					'0=Unlimited number of replays can be earned per game
					'1=Playfield Special of extra ball  (Tourney mode this is zero)
					'0=Novelty of normal game mode		(Tourney mode this set to 1)
					'1=Match feature
					'1=Background Sound

'******************************************************
'	The default ROM menu has the replay threshold values to off.  The scorecard shows replay levels of 600,000 and 1,400,000.  To set these
'	follow the instructions below.  If you downloaded UnclePaulie's version off VPU, a .nvram file is included which has those settings
'	preset to those levels.  

' Here is the procedure to set the selftest postions on this Gottlieb system 80 table for the preset replay levels:

' 1. Press 7 to enter test mode.
' 2. Continue to press 7 until you see 11 in the credit display.
' 3. Press and release the 1 key to zero out any score that is currently stored there (You probably have zero there currently, but you still have to perform this step)
' 4. Press and hold the 1 key until you get to the replay score you want to set it to. The score will increment by 10,000 points while you are holding the 1 key in.
' 5. Press 7 again to see 12 in the credit display
' 6. Repeat steps 3 and 4 to set the second replay score.
' 7. Press 7 again to see 13 in the credit display.
' 8. Repeat steps 3 and 4 to set the third replay score.
' 9. Press 7 again to see 14 in the credit display
' 10. Repeat steps 3 and 4 to set the high game to date score.
' 11. Press F3 to save your changes to the nvram.  (I recommend to also restart the table)

' Here are the recommended values to change to:

	' 11: 600,000 - First threshold to award a replay
	' 12: 1,400,000 - First threshold to award a replay
	' 13: 0 - leave blank... no replay for 3rd tier.

'User Defined acrylic edge colors (This is if user defined option is selected for plastic edge color... Choose your own RGB)

	Dim UserAcrylColor 

	UserAcrylColor = RGB(255,255,240)	'default is RGB(255,255,240)


'***********************************************************************

'
'*********************************************************************************************************************************
' === TABLE OF CONTENTS  ===
'
' You can quickly jump to a section by searching the four letter tag (ZXXX)
'
'	ZVAR: Constants and Global Variable
'	ZTIM: Timers
'	ZINI: Table Initialization and Exiting
'   ZOPT: User Options
' 	ZMAT: General Math Functions
'	ZANI: Misc Animations
' 	ZBBR: Ball Brightness
' 	ZRBR: Room Brightness
'	ZKEY: Key Press Handling
'	ZSOL: Solenoids & Flashers
'	ZDRN: Drain, Trough, and Ball Release
'	ZFLP: Flippers
'	ZSLG: Slingshot Animations
'	ZSSC: Slingshot Corrections
'	ZSWI: Switches
' 	ZBRL: Ball Rolling and Drop Sounds
' 	ZRRL: Ramp Rolling Sound Effects
' 	ZFLE: Fleep Mechanical Sounds
'	ZNFF: Flipper Corrections
' 	ZDMP: Rubber Dampeners
' 	ZRDT: Drop Targets
'	ZRST: Stand-Up Targets
' 	ZBOU: VPW TargetBouncer for targets and posts
' 	ZGIU: GI updates
' 	Z3DI: 3D Inserts
' 	ZFLD: Flupper Domes
' 	ZFLB: Flupper Bumpers
'	ZSHA: Ambient Ball Shadows
' 	ZVRR: VR Room / VR Cabinet
'
'*********************************************************************************************************************************

	Dim VR_Room, cab_mode, DesktopMode

	If ForceVR = 1 Then 
		DesktopMode = False
		VR_Room = 1
		cab_mode = 0
	Else
		DesktopMode = Table1.ShowDT
		If RenderingMode = 2 Then VR_Room=1 Else VR_Room=0
		if Not DesktopMode and VR_Room=0 Then cab_mode=1 Else cab_mode=0
	End If

' ****************************************************

'******************************************************
'  ZVAR: Constants and Global Variables
'******************************************************
Const BallSize = 50
Const BallMass = 1

Const tnob = 2 ' total number of balls
Const lob = 0   'number of locked balls

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Dim BIPL : BIPL = False		'Ball in plunger lane
Dim flbumpvalue1: flbumpvalue1 = 0
Dim flbumpvalue2: flbumpvalue2 = 0
Dim flbumpvalue3: flbumpvalue3 = 0
Dim gilvl
dim PlasProt, Ttents, Tscrewcap, TFlipper
Const rcolor = 201
Const gcolor = 204
Const bcolor = 202

Dim ABall1, ABall2, gBOT

'  Standard definitions
Const cGameName = "alienstr"	'PinMAME ROM name
Const UseSolenoids = 2			'1 = Normal Flippers, 2 = Fastflips
Const UseLamps = 1				'0 = Custom lamp handling, 1 = Built-in VPX handling (using light number in light timer)
Const UseSync = 0
Const HandleMech = 0

' Insert brightness scale
const insertfactor 		= 1		' adjusts the level of the brightness of the inserts when on

LoadVPM "01210000", "sys80.VBS", 3.1

'******************************************************
'	VLM Arrays
'******************************************************

' VLM  Arrays - Start
' Arrays per baked part
Dim BP_Parts: BP_Parts=Array(BM_Parts, LM_All_Lights_gi_005_Parts, LM_All_Lights_gi_009_Parts, LM_All_Lights_gi_010_Parts, LM_All_Lights_gi_014_Parts, LM_All_Lights_gi_015_Parts, LM_All_Lights_gi_016_Parts, LM_All_Lights_l29_Parts, LM_All_Lights_l30_Parts, LM_All_Lights_l31_Parts, LM_All_Lights_l32_Parts, LM_All_Lights_l33_Parts, LM_All_Lights_l34_Parts, LM_All_Lights_l35_Parts, LM_All_Lights_l36_Parts, LM_All_Lights_l40_Parts, LM_All_Lights_l41_Parts, LM_All_Lights_l42_Parts, LM_All_Lights_l43_Parts)
Dim BP_SW_Parts: BP_SW_Parts=Array(BM_SW_Parts, LM_All_Lights_gi_001_SW_Parts, LM_All_Lights_gi_002_SW_Parts, LM_All_Lights_gi_003_SW_Parts, LM_All_Lights_gi_020_SW_Parts, LM_All_Lights_gi_021_SW_Parts, LM_All_Lights_gi_022_SW_Parts, LM_All_Lights_gi_023_SW_Parts, LM_All_Lights_l45_SW_Parts, LM_All_Lights_l46_SW_Parts)
Dim BP_SW_Parts1: BP_SW_Parts1=Array(BM_SW_Parts1, LM_All_Lights_gi_001_SW_Parts1, LM_All_Lights_gi_002_SW_Parts1, LM_All_Lights_gi_003_SW_Parts1, LM_All_Lights_gi_020_SW_Parts1, LM_All_Lights_gi_021_SW_Parts1, LM_All_Lights_gi_022_SW_Parts1, LM_All_Lights_gi_023_SW_Parts1, LM_All_Lights_l45_SW_Parts1, LM_All_Lights_l46_SW_Parts1)
Dim BP_SP_Parts: BP_SP_Parts=Array(LM_All_Lights_l44_SP_Parts)
Dim BP_SP_PartsB: BP_SP_PartsB=Array(LM_All_Lights_l44_SP_PartsB)
Dim BP_Posts: BP_Parts=Array(BM_Posts)
' Arrays per lighting scenario
Dim BL_All_Lights_gi_001: BL_All_Lights_gi_001=Array(LM_All_Lights_gi_001_Playfield,LM_All_Lights_gi_001_SW_Parts)
Dim BL_All_Lights_gi_002: BL_All_Lights_gi_002=Array(LM_All_Lights_gi_002_Playfield,LM_All_Lights_gi_002_SW_Parts)
Dim BL_All_Lights_gi_003: BL_All_Lights_gi_003=Array(LM_All_Lights_gi_003_Playfield,LM_All_Lights_gi_003_SW_Parts)
Dim BL_All_Lights_gi_004: BL_All_Lights_gi_004=Array(LM_All_Lights_gi_004_Playfield)
Dim BL_All_Lights_gi_005: BL_All_Lights_gi_005=Array(LM_All_Lights_gi_005_Playfield,LM_All_Lights_gi_005_Parts)
Dim BL_All_Lights_gi_006: BL_All_Lights_gi_006=Array(LM_All_Lights_gi_006_Playfield)
Dim BL_All_Lights_gi_007: BL_All_Lights_gi_007=Array(LM_All_Lights_gi_007_Playfield)
Dim BL_All_Lights_gi_008: BL_All_Lights_gi_008=Array(LM_All_Lights_gi_008_Playfield)
Dim BL_All_Lights_gi_009: BL_All_Lights_gi_009=Array(LM_All_Lights_gi_009_Playfield,LM_All_Lights_gi_009_Parts)
Dim BL_All_Lights_gi_010: BL_All_Lights_gi_010=Array(LM_All_Lights_gi_010_Playfield,LM_All_Lights_gi_010_Parts)
Dim BL_All_Lights_gi_011: BL_All_Lights_gi_011=Array(LM_All_Lights_gi_011_Playfield)
Dim BL_All_Lights_gi_012: BL_All_Lights_gi_012=Array(LM_All_Lights_gi_012_Playfield)
Dim BL_All_Lights_gi_013: BL_All_Lights_gi_013=Array(LM_All_Lights_gi_013_Playfield)
Dim BL_All_Lights_gi_014: BL_All_Lights_gi_014=Array(LM_All_Lights_gi_014_Playfield,LM_All_Lights_gi_014_Parts)
Dim BL_All_Lights_gi_015: BL_All_Lights_gi_015=Array(LM_All_Lights_gi_015_Playfield,LM_All_Lights_gi_015_Parts)
Dim BL_All_Lights_gi_016: BL_All_Lights_gi_016=Array(LM_All_Lights_gi_016_Playfield,LM_All_Lights_gi_016_Parts)
Dim BL_All_Lights_gi_017: BL_All_Lights_gi_017=Array(LM_All_Lights_gi_017_Playfield)
Dim BL_All_Lights_gi_018: BL_All_Lights_gi_018=Array(LM_All_Lights_gi_018_Playfield)
Dim BL_All_Lights_gi_019: BL_All_Lights_gi_019=Array(LM_All_Lights_gi_019_Playfield)
Dim BL_All_Lights_gi_020: BL_All_Lights_gi_020=Array(LM_All_Lights_gi_020_Playfield,LM_All_Lights_gi_020_SW_Parts)
Dim BL_All_Lights_gi_021: BL_All_Lights_gi_021=Array(LM_All_Lights_gi_021_Playfield,LM_All_Lights_gi_021_SW_Parts)
Dim BL_All_Lights_gi_022: BL_All_Lights_gi_022=Array(LM_All_Lights_gi_022_Playfield,LM_All_Lights_gi_022_SW_Parts)
Dim BL_All_Lights_gi_023: BL_All_Lights_gi_023=Array(LM_All_Lights_gi_023_Playfield,LM_All_Lights_gi_023_SW_Parts)
Dim BL_All_Lights_l29: BL_All_Lights_l29=Array(LM_All_Lights_l29_Parts)
Dim BL_All_Lights_l30: BL_All_Lights_l30=Array(LM_All_Lights_l30_Parts)
Dim BL_All_Lights_l31: BL_All_Lights_l31=Array(LM_All_Lights_l31_Parts)
Dim BL_All_Lights_l32: BL_All_Lights_l32=Array(LM_All_Lights_l32_Parts)
Dim BL_All_Lights_l33: BL_All_Lights_l33=Array(LM_All_Lights_l33_Parts)
Dim BL_All_Lights_l34: BL_All_Lights_l34=Array(LM_All_Lights_l34_Parts)
Dim BL_All_Lights_l35: BL_All_Lights_l35=Array(LM_All_Lights_l35_Parts)
Dim BL_All_Lights_l36: BL_All_Lights_l36=Array(LM_All_Lights_l36_Parts)
Dim BL_All_Lights_l40: BL_All_Lights_l40=Array(LM_All_Lights_l40_Parts)
Dim BL_All_Lights_l41: BL_All_Lights_l41=Array(LM_All_Lights_l41_Parts)
Dim BL_All_Lights_l42: BL_All_Lights_l42=Array(LM_All_Lights_l42_Parts)
Dim BL_All_Lights_l43: BL_All_Lights_l43=Array(LM_All_Lights_l43_Parts)
Dim BL_All_Lights_l45: BL_All_Lights_l45=Array(LM_All_Lights_l45_SW_Parts)
Dim BL_All_Lights_l46: BL_All_Lights_l46=Array(LM_All_Lights_l46_SW_Parts)
Dim BL_All_Lights_l44: BL_All_Lights_l44=Array(LM_All_Lights_l44_SP_Parts,LM_All_Lights_l44_SP_PartsB)
Dim BL_World: BL_World=Array(BM_Parts,BM_SW_Parts,BM_SW_Parts1,BM_Posts)
' Global arrays
Dim BG_Bakemap: BG_Bakemap=Array(BM_Parts,BM_SW_Parts,BM_SW_Parts1,BM_Posts)
Dim BG_Lightmap: BG_Lightmap=Array(LM_All_Lights_gi_001_Playfield, LM_All_Lights_gi_002_Playfield, LM_All_Lights_gi_003_Playfield, LM_All_Lights_gi_004_Playfield, LM_All_Lights_gi_005_Playfield, LM_All_Lights_gi_006_Playfield, LM_All_Lights_gi_007_Playfield, LM_All_Lights_gi_008_Playfield, LM_All_Lights_gi_009_Playfield, LM_All_Lights_gi_010_Playfield, LM_All_Lights_gi_011_Playfield, LM_All_Lights_gi_012_Playfield, LM_All_Lights_gi_013_Playfield, LM_All_Lights_gi_014_Playfield, LM_All_Lights_gi_015_Playfield, LM_All_Lights_gi_016_Playfield, LM_All_Lights_gi_017_Playfield, LM_All_Lights_gi_018_Playfield, LM_All_Lights_gi_019_Playfield, LM_All_Lights_gi_020_Playfield, LM_All_Lights_gi_021_Playfield, LM_All_Lights_gi_022_Playfield, LM_All_Lights_gi_023_Playfield,LM_All_Lights_gi_005_Parts, LM_All_Lights_gi_009_Parts, LM_All_Lights_gi_010_Parts, LM_All_Lights_gi_014_Parts, LM_All_Lights_gi_015_Parts, LM_All_Lights_gi_016_Parts, LM_All_Lights_l29_Parts, LM_All_Lights_l30_Parts, LM_All_Lights_l31_Parts, LM_All_Lights_l32_Parts, LM_All_Lights_l33_Parts, LM_All_Lights_l34_Parts, LM_All_Lights_l35_Parts, LM_All_Lights_l36_Parts, LM_All_Lights_l40_Parts, LM_All_Lights_l41_Parts, LM_All_Lights_l42_Parts, LM_All_Lights_l43_Parts,LM_All_Lights_gi_001_SW_Parts, LM_All_Lights_gi_002_SW_Parts, LM_All_Lights_gi_003_SW_Parts, LM_All_Lights_gi_020_SW_Parts, LM_All_Lights_gi_021_SW_Parts, LM_All_Lights_gi_022_SW_Parts, LM_All_Lights_gi_023_SW_Parts, LM_All_Lights_l45_SW_Parts, LM_All_Lights_l46_SW_Parts,LM_All_Lights_gi_001_SW_Parts1, LM_All_Lights_gi_002_SW_Parts1, LM_All_Lights_gi_003_SW_Parts1, LM_All_Lights_gi_020_SW_Parts1, LM_All_Lights_gi_021_SW_Parts1, LM_All_Lights_gi_022_SW_Parts1, LM_All_Lights_gi_023_SW_Parts1, LM_All_Lights_l45_SW_Parts1, LM_All_Lights_l46_SW_Parts1,LM_All_Lights_l44_SP_Parts,LM_All_Lights_l44_SP_PartsB)
Dim BG_All: BG_All=Array(BM_Parts, LM_All_Lights_gi_001_Playfield, LM_All_Lights_gi_002_Playfield, LM_All_Lights_gi_003_Playfield, LM_All_Lights_gi_004_Playfield, LM_All_Lights_gi_005_Playfield, LM_All_Lights_gi_006_Playfield, LM_All_Lights_gi_007_Playfield, LM_All_Lights_gi_008_Playfield, LM_All_Lights_gi_009_Playfield, LM_All_Lights_gi_010_Playfield, LM_All_Lights_gi_011_Playfield, LM_All_Lights_gi_012_Playfield, LM_All_Lights_gi_013_Playfield, LM_All_Lights_gi_014_Playfield, LM_All_Lights_gi_015_Playfield, LM_All_Lights_gi_016_Playfield, LM_All_Lights_gi_017_Playfield, LM_All_Lights_gi_018_Playfield, LM_All_Lights_gi_019_Playfield, LM_All_Lights_gi_020_Playfield, LM_All_Lights_gi_021_Playfield, LM_All_Lights_gi_022_Playfield, LM_All_Lights_gi_023_Playfield,BM_Parts, LM_All_Lights_gi_005_Parts, LM_All_Lights_gi_009_Parts, LM_All_Lights_gi_010_Parts, LM_All_Lights_gi_014_Parts, LM_All_Lights_gi_015_Parts, LM_All_Lights_gi_016_Parts, LM_All_Lights_l29_Parts, LM_All_Lights_l30_Parts, LM_All_Lights_l31_Parts, LM_All_Lights_l32_Parts, LM_All_Lights_l33_Parts, LM_All_Lights_l34_Parts, LM_All_Lights_l35_Parts, LM_All_Lights_l36_Parts, LM_All_Lights_l40_Parts, LM_All_Lights_l41_Parts, LM_All_Lights_l42_Parts, LM_All_Lights_l43_Parts,BM_SW_Parts, LM_All_Lights_gi_001_SW_Parts, LM_All_Lights_gi_002_SW_Parts, LM_All_Lights_gi_003_SW_Parts, LM_All_Lights_gi_020_SW_Parts, LM_All_Lights_gi_021_SW_Parts, LM_All_Lights_gi_022_SW_Parts, LM_All_Lights_gi_023_SW_Parts, LM_All_Lights_l45_SW_Parts, LM_All_Lights_l46_SW_Parts,BM_SW_Parts1, LM_All_Lights_gi_001_SW_Parts1, LM_All_Lights_gi_002_SW_Parts1, LM_All_Lights_gi_003_SW_Parts1, LM_All_Lights_gi_020_SW_Parts1, LM_All_Lights_gi_021_SW_Parts1, LM_All_Lights_gi_022_SW_Parts1, LM_All_Lights_gi_023_SW_Parts1, LM_All_Lights_l45_SW_Parts1, LM_All_Lights_l46_SW_Parts1, LM_All_Lights_l44_SP_Parts,LM_All_Lights_l44_SP_PartsB,BM_Posts)
' VLM  Arrays - End


'******************************************************
'  ZTIM: Timers
'******************************************************

'The FrameTimer interval should be -1, so executes at the display frame rate
'The frame timer should be used to update anything visual, like some animations, shadows, etc.
'However, a lot of animations will be handled in their respective _animate subroutines.

Dim FrameTime, InitFrameTime
InitFrameTime = 0

FrameTimer.Interval = -1
Sub FrameTimer_Timer() 
	FrameTime = gametime - InitFrameTime 'Calculate FrameTime as some animuations could use this
	InitFrameTime = gametime	'Count frametime
	'Add animation stuff here
	BSUpdate
	UpdateBallBrightness		'Updates the brightness of the ball in plunger lane when GI is on or not.
	RollingUpdate				'Update rolling sounds
	DoSTAnim					'Standup target animations
	BSUpdate 					'Update ambient ball shadows
	AnimateBumperSkirts			'Base skirts will animate when a ball touches them

    If VR_Room = 0 and cab_mode = 0 Then
        DisplayTimer
    End If

	If VR_Room=1 Then
		VRDisplayTimer
	End If

End Sub

'The CorTimer interval should be 10. It's sole purpose is to update the Cor (physics) calculations

CorTimer.Interval = 10
Sub CorTimer_Timer(): Cor.Update: End Sub

'******************************************************
'  ZINI: Table Initialization and Exiting
'******************************************************

Dim NTM

Sub Table1_Init
	vpminit me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Alien Star (Gottlieb 1984)"&chr(13)&"by UnclePaulie"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Games(cGameName).Settings.Value("sound") = 1 ' Set sound (0=OFF, 1=ON)	
	End With
	On Error Resume Next
	Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0

	'Map all lamps to the corresponding ROM output using the value of TimerInterval of each light object
	vpmMapLights AllLamps 		'Make a collection called "AllLamps" and put all the light objects in it.

	'Nudging
	vpmNudge.TiltSwitch = 57
	vpmNudge.Sensitivity = 3
	vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot,LeftFlipper,RightFlipper)

	'Trough - Creates a ball in the kicker switch and gives that ball used an individual name.
	Set ABall1 = BallRelease.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set ABall2 = Slot1.CreateSizedballWithMass(Ballsize/2,Ballmass)

	'***Setting up a ball array (collection), must contain all the balls you create on the table.
	gBOT = Array(ABall1, ABall2)

	'Forces the trough switches to correct state at table boot so the game logic knows there are balls in the trough.
	controller.Switch(14) = 1
	controller.Switch(44) = 1

	'Saucer initializations
	Controller.Switch(4) = 0

	NTM=0    'turn off number to match box for desktop backglass
	FindDips  'find if match enabled, if so turn back on number to match box


	' Backglass GI and VR level initially off
	PFGI 0
	gilvl = 0

' GI on at game start with slight delay
	vpmTimer.AddTimer 2000,"PFGI 1'"

 	if VR_Room = 1 Then
		setup_backglass()
	End If

' If user wants to set dip switches themselves it will force them to set it via F6.
	If SetDIPSwitches = 0 Then
		SetDefaultDips
	End If

' Initialize Bumpers
		FlFadeBumper 1, 0
		FlFadeBumper 2, 0
		FlFadeBumper 3, 0
		flbumpvalue1 = 0
		flbumpvalue2 = 0
		flbumpvalue3 = 0

End Sub

Sub Table_Paused:Controller.Pause = 1:End Sub
Sub Table_unPaused:Controller.Pause = 0:End Sub
Sub Table_exit()
	Controller.Pause = False
	Controller.Stop
End Sub


'*******************************************
'  ZOPT: User Options
'*******************************************

' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings

' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts

const lightlevelinitial = 0.15					' Initial Level of room lighting (0 to 1), where 0 is dark and 100 is brightest
Dim LightLevel : LightLevel = lightlevelinitial	' Level of room lighting
Dim ColorLUT : ColorLUT = 1						' Color desaturation LUTs: 1 to 11, where 1 is normal and 11 is black'n'white
Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		' Level of ramp rolling volume. Value between 0 and 1
Dim ROMVolume									' Level of ROM volume. Value between 0 and 1
Dim WallClock, poster, poster2, CustomWalls
Dim Tourney
Dim ChooseBall: ChooseBall = 4
							'0 = dark, 1 = not as dark, 2 = bright, 3 = brightest, 4 = warm, 5 = MrBallDark, 6 = DarthVito Ball1
							'7 = DarthVito HDR1, 8 = DarthVito HDR Bright, 9 = DarthVito HDR2 (Default), 10 = DarthVito ballHDR3,
							'11 = DarthVito ballHDR4, 12 = DarthVito ballHDRdark, 13 = Borg Ball, 14 = SteelBall2

' Bumper brightness scale
dim bumperscale: 		bumperscale =  		0.22	' adjusts the level of blenddisablelighting and intensity on bumpers when lights are not on
dim bumperflashlevel: 	bumperflashlevel = 	0.4		' adjusts the level of bumper lights when on and / or flashing


Dim FlipperColor, ScrewCapColor, cabsideblades, PlasticColor, SideWallColor, undercab, LaneColor, insertmop

Dim dspTriggered : dspTriggered = False
Sub Table1_OptionEvent(ByVal eventId)
    If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If

	ROMVolume = Table1.Option("ROM Volume (-32 off; 0 max) - Requires Restart; altsound set to 0", -32, 0, 1, 0, 0)

    ' Ball Choice
    ChooseBall = Table1.Option("Ball Choice", 0, 14, 1, 4, 0, Array("0: Hauntfreaks dark", "1: Hauntfreaks not as dark", _
			"2: Hauntfreaks bright", "3: Hauntfreaks brightest", "4: Hauntfreaks warm (default)", "5: MrBallDark", "6: DarthVito Ball1", _
			"7: DarthVito HDR1", "8: DarthVito HDR Bright", "9: DarthVito HDR2", "10: DarthVito ballHDR3", _
			"11: DarthVito ballHDR4", "12: DarthVito ballHDRdark", "13: Borg Ball", "14: SteelBall2"))
	ChangeBall(ChooseBall)

	' Bumper Options
	bumperscale = Table1.Option("Bumper brightness when not lit (default 22%)", 0, 1, 0.01, 0.22, 1)
	bumperflashlevel = Table1.Option("Bumper brightness when lit or flashing (default 40%)", 0, 1, 0.01, 0.40, 1)

	b1_animate
	b2_animate
	b3_animate

	' General Options

	Tourney = Table1.Option("Tourney Mode - Requires Table Restart", 0, 1, 1, 0, 0, Array("Off", "On"))

	FlipperColor = Table1.Option("Flipper Color", 0, 3, 1, 0, 0, Array("Red (default)", "Yellow", "Black", "Orange"))
	LaneColor = Table1.Option("Lane Guide and Tent Color", 0, 1, 1, 0, 0, Array("Red (default)", "Yellow"))
	PlasticColor = Table1.Option("Plastic Edge Color", 0, 6, 1, 0, 0, Array("Off (default)", "Orange/Red", "Yellow", "Blue", "User Defined in Script", "Rainbow", "Random"))
	SideWallColor = Table1.Option("Interior Wall Color", 0, 1, 1, 1, 0, Array("Black", "Wood (default)"))
	ScrewCapColor = Table1.Option("Screw Cap Color", 0, 5, 1, 0, 0, Array("White", "Black", "Bronze", "Silver", "Blue/Green", "Gold"))
	cabsideblades = Table1.Option("Cabinet Sideblades - full cabinet only", 0, 1, 1, 0, 0, Array("Extra Tall - default in cab mode", "Lowered Blades"))
	insertmop = Table1.Option("Mother Of Pearl Solid Inserts?", 0, 1, 1, 0, 0, Array("No/Standard (default)", "Yes"))

	' VR Room Environment

	WallClock = Table1.Option("VR Wall Clock On/Off", 0, 1, 1, 1, 0, Array("Off", "On"))
    CustomWalls = Table1.Option("VR Room Environment", 0, 4, 1, 2, 0, _
		Array("UP Original", "Sixtoe Arcade", "DarthVito Home", "DarthVito Plaster", "DarthVito Blue"))
	Poster = Table1.Option("VR Poster 1 On/Off", 0, 1, 1, 1, 0, Array("Off", "On"))
	Poster2 = Table1.Option("VR Poster 2 On/Off", 0, 1, 1, 1, 0, Array("Off", "On"))
	undercab = Table1.Option("VR Undercab lighting On/Off", 0, 1, 1, 1, 0, Array("Off", "On"))

	SetupOptions

    ' Sound volumes
	VolumeDial = Table1.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
	BallRollVolume = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.5, 1)
	RampRollVolume = Table1.Option("Ramp Roll Volume", 0, 1, 0.01, 0.5, 1)

	' Color Saturation
    ColorLUT = Table1.Option("Color Saturation", 1, 11, 1, 1, 0, _
		Array("Normal", "Desaturated 10%", "Desaturated 20%", "Desaturated 30%", "Desaturated 40%", "Desaturated 50%", _
        "Desaturated 60%", "Desaturated 70%", "Desaturated 80%", "Desaturated 90%", "Black 'n White"))
	if ColorLUT = 1 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1" ' the image was blank for default, i.e.:  just ""  - this fix if for VR Headtracking
	if ColorLUT = 2 Then Table1.ColorGradeImage = "colorgradelut256x16-10"
	if ColorLUT = 3 Then Table1.ColorGradeImage = "colorgradelut256x16-20"
	if ColorLUT = 4 Then Table1.ColorGradeImage = "colorgradelut256x16-30"
	if ColorLUT = 5 Then Table1.ColorGradeImage = "colorgradelut256x16-40"
	if ColorLUT = 6 Then Table1.ColorGradeImage = "colorgradelut256x16-50"
	if ColorLUT = 7 Then Table1.ColorGradeImage = "colorgradelut256x16-60"
	if ColorLUT = 8 Then Table1.ColorGradeImage = "colorgradelut256x16-70"
	if ColorLUT = 9 Then Table1.ColorGradeImage = "colorgradelut256x16-80"
	if ColorLUT = 10 Then Table1.ColorGradeImage = "colorgradelut256x16-90"
	if ColorLUT = 11 Then Table1.ColorGradeImage = "colorgradelut256x16-100"

	' Room brightness
'	LightLevel = Table1.Option("Table Brightness (Ambient Light Level)", 0, 1, 0.01, .5, 1)
	LightLevel = NightDay/100

	' Calls the generic room brightness sub to adjust bdl's on prims as appropriate if the day/night level changes
	SetRoomBrightness    'Uncomment this line for lightmapped tables.

    If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If

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
'  ZANI: Misc Animations
'******************************************************

''''' Flipper Animations

Sub LeftFlipper_Animate
	dim a: a = LeftFlipper.CurrentAngle
	FlipperLSh.RotZ = a
	lflipb.RotZ = a
	lflipr.RotZ = a
End Sub

Sub RightFlipper_Animate
	dim a: a = RightFlipper.CurrentAngle
	FlipperRSh.RotZ = a
	rflipb.RotZ = a
	rflipr.RotZ = a
End Sub

''''' Gate Animations

Sub Gate1_Animate
	dim a: a = -Gate1.currentangle*0.5
	pGate1.Rotx = a
End Sub

Sub Gate2_Animate
	dim a: a = -Gate2.currentangle*0.5
	pGate2.Rotx = a
End Sub

Sub Gate3_Animate
	dim a: a = Gate3.CurrentAngle
	pGate3.rotx = a
	pScoreSpring.rotz = -a*4/90
End Sub


''''' Bumper Animations

Sub Bumper1_Animate
	Dim a
	a = Bumper1.CurrentRingOffset
	BumperRing1.transz = a
End Sub

Sub Bumper2_Animate
	Dim a
	a = Bumper2.CurrentRingOffset
	BumperRing2.transz = a
End Sub

Sub Bumper3_Animate
	Dim a
	a = Bumper3.CurrentRingOffset
	BumperRing3.transz = a
End Sub

Dim Bumpers : Bumpers = Array(Bumper1, Bumper2, Bumper3)

Sub AnimateBumperSkirts  
	dim r, g, s, x, y, b, tz
	' Animate Bumper switch (experimental)
	For r = 0 To 2
		g = 10000.0
		For s = 0 to UBound(gBOT)
			x = Bumpers(r).x - gBOT(s).x
			y = Bumpers(r).y - gBOT(s).y
			b = x * x + y * y
			If b < g Then g = b
		Next
		tz = 0
		If g < 80 * 80 Then
			tz = -3
		End If
		If r = 0 Then bumperdisk1.transZ = tz
		If r = 1 Then bumperdisk2.transZ = tz
		If r = 2 Then bumperdisk3.transZ = tz
	Next
End Sub


'******************************************************
' 	ZBBR: BALL BRIGHTNESS
'******************************************************

Const BallBrightness =  1      	'Ball brightness - Value between 0 and 1 (0=Dark ... 1=Bright)


' Constants for plunger lane ball darkening.
' You can make a temporary wall in the plunger lane area and use the co-ordinates from the corner control points.
Const PLOffset = 0.5			'Minimum ball brightness scale in plunger lane
Const PLLeft = 850 			'X position of punger lane left
Const PLRight = 952 			'X position of punger lane right
Const PLTop = 395 				'Y position of punger lane top
Const PLBottom = 1900 			'Y position of punger lane bottom
Dim PLGain: PLGain = (1-PLOffset)/(PLTop-PLBottom)

Sub UpdateBallBrightness
	Dim s, b_base, b_r, b_g, b_b, d_w
	b_base = 120 * BallBrightness + 135*gilvl	' orig was 120 and 70

	For s = 0 To UBound(gBOT)
		' Handle z direction
		d_w = b_base*(1 - (gBOT(s).z-25)/500)
		If d_w < 30 Then d_w = 30
		' Handle plunger lane
		If InRect(gBOT(s).x,gBOT(s).y,PLLeft,PLBottom,PLLeft,PLTop,PLRight,PLTop,PLRight,PLBottom) Then  
			d_w = d_w*(PLOffset+PLGain*(gBOT(s).y-PLBottom))
		End If
		' Assign color
		b_r = Int(d_w)
		b_g = Int(d_w)
		b_b = Int(d_w)
		If b_r > 255 Then b_r = 255
		If b_g > 255 Then b_g = 255
		If b_b > 255 Then b_b = 255
		gBOT(s).color = b_r + (b_g * 256) + (b_b * 256 * 256)
		'debug.print "--- ball.color level="&b_r
	Next
End Sub


'****************************
' 	ZRBR: Room Brightness
'****************************

Sub SetRoomBrightness
Dim girubbercolor_r, girubbercolor_g, girubbercolor_b, x

' General GI Lights
	For each x in GI: x.State = gilvl: Next

' VR Room
	For each x in VRRoomLL: x.blenddisablelighting = 45*LightLevel*lightlevelinitial: Next
	For each x in VRCabLL: x.blenddisablelighting = 25*LightLevel*lightlevelinitial: Next
	For each x in LLVRButtons: x.blenddisablelighting = 5*LightLevel*lightlevelinitial: Next
	VRposter.blenddisablelighting = 22*LightLevel*lightlevelinitial
	VRposter2.blenddisablelighting = 22*LightLevel*lightlevelinitial
	VR_Clockface.opacity = 4467*LightLevel*lightlevelinitial
	BGBright.opacity = 25 + 3333*LightLevel*lightlevelinitial
	BGDark.opacity = 2222*LightLevel*lightlevelinitial
	VRNewFloor.opacity = 4467*LightLevel*lightlevelinitial
	For each x in TopHardware: x.blenddisablelighting = 2*LightLevel*lightlevelinitial: Next
	UndercabLight.intensity = 15 * gilvl

' rubbers
	girubbercolor_r = (gilvl * 53 * LightLevel/lightlevelinitial) + rcolor
	girubbercolor_g = (gilvl * 50 * LightLevel/lightlevelinitial) + gcolor
	girubbercolor_b = (gilvl * 42 * LightLevel/lightlevelinitial) + bcolor
	MaterialColor "Rubber White",RGB(girubbercolor_r,girubbercolor_g,girubbercolor_b)

' targets
	For each x in GITargets: x.blenddisablelighting = (0.1 * gilvl) + (0.025 * LightLevel/lightlevelinitial): Next

' prims
	ApronPrim.blenddisablelighting = 0.075 * gilvl * LightLevel/lightlevelinitial
	PlungerCoverPrim.blenddisablelighting = 0.025 * gilvl * LightLevel/lightlevelinitial
	LaneGuard.blenddisablelighting = 0.025 * gilvl * LightLevel/lightlevelinitial
	For each x in GIScrewCaps: x.blenddisablelighting = ((0.025 * gilvl) + (0.025)) * (LightLevel/lightlevelinitial) + 0.01: Next
	OuterPrimBlack_DT.blenddisablelighting = (0.03 * gilvl) + (0.03 * LightLevel/lightlevelinitial)
	Backwall.blenddisablelighting = (0.03 * gilvl) + (0.03 * LightLevel/lightlevelinitial)
	TopRail_Bot.blenddisablelighting = 0.005 * gilvl + (0.005 * LightLevel/lightlevelinitial)
	For each x in WallColor: x.blenddisablelighting = 0.02 * gilvl + (0.0125 * LightLevel/lightlevelinitial): Next
	For each x in BgBrackets: x.blenddisablelighting = 0.01 * gilvl + (0.04 * LightLevel/lightlevelinitial): Next
	For each x in GIBulbPrims: x.blenddisablelighting = (0.4 * (gilvl)): Next
	For each x in GICutoutPrims: x.blenddisablelighting = (0.15 * (gilvl)) + (0.005 * LightLevel/lightlevelinitial): Next
	BM_Parts.blenddisablelighting = LightLevel/lightlevelinitial
	BM_SW_Parts.blenddisablelighting = LightLevel/lightlevelinitial
	BM_SW_Parts1.blenddisablelighting = 0.1*LightLevel/lightlevelinitial
	BM_Posts.opacity = ((35 * gilvl) + (40 * LightLevel/lightlevelinitial))
	For each x in GILaneGuides: x.blenddisablelighting = (0.075 * (gilvl)) + (0.025 * LightLevel/lightlevelinitial) : Next
	For each x in GILaneTents: x.blenddisablelighting = (0.175 * (gilvl)) + (0.025 * LightLevel/lightlevelinitial) : Next
	For each x in GIMetal: x.blenddisablelighting = (0.015 * gilvl) + (0.01 * LightLevel/lightlevelinitial): Next
	For each x in GIMetalRails: x.blenddisablelighting = (0.005 * gilvl) + (0.005 * LightLevel/lightlevelinitial): Next
	For each x in GIMetalPosts: x.blenddisablelighting = (0.005 * gilvl) + (0.005 * LightLevel/lightlevelinitial): Next
	For each x in GILeafswitches: x.blenddisablelighting = (0.2) * (gilvl) * LightLevel/lightlevelinitial: Next
	SlingL.blenddisablelighting = (0.005) * (gilvl)
	SlingR.blenddisablelighting = (0.005) * (gilvl)
	Lflipb.blenddisablelighting = (0.0375 * (gilvl)) + (0.1125 * LightLevel/lightlevelinitial)
	Rflipb.blenddisablelighting = (0.0375 * (gilvl)) + (0.1125 * LightLevel/lightlevelinitial)
	Lflipr.blenddisablelighting = (0.0375 * (gilvl)) + (0.1125 * LightLevel/lightlevelinitial)
	Rflipr.blenddisablelighting = (0.0375 * (gilvl)) + (0.1125 * LightLevel/lightlevelinitial)
	PegPlasticT8Twin2.blenddisablelighting = (0.05 * (gilvl)) + (0.05 * LightLevel/lightlevelinitial)
	PegPlasticT8Twin1.blenddisablelighting = (0.05 * (gilvl)) + (0.05 * LightLevel/lightlevelinitial)
	MaterialColor "Plastic with an image cards",RGB(girubbercolor_r,girubbercolor_g,girubbercolor_b)
	SpinnerPrim.blenddisablelighting = (0.05 * gilvl) + (0.05 * LightLevel/lightlevelinitial)
	pScoreSpring.blenddisablelighting = (0.05 * gilvl) + (0.25 * LightLevel/lightlevelinitial)

'saucer
	Pkickerhole1.blenddisablelighting = (0.125 * gilvl) + (0.025 * LightLevel/lightlevelinitial)
	PkickarmSW4.blenddisablelighting = (0.125 * gilvl) + (0.025 * LightLevel/lightlevelinitial)
	pSaucerFloor.blenddisablelighting =(0.125 * gilvl) + (0.025 * LightLevel/lightlevelinitial)

'Center Red Insert
	pcenter.BlendDisableLighting = insert_red_off + insert_red_on * (gilvl)
	pcenteroff.BlendDisableLighting = a_insert_red_off - a_insert_red_on * (gilvl)

End Sub


'******************************************************
'	ZKEY: Key Press Handling
'******************************************************

'*********************
' VR Plunger code
'*********************

Sub TimerVRPlunger_Timer
  If VR_Shooter.Y < 86 then
  		VR_Shooter.Y = VR_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger1_Timer
	VR_Shooter.Y = -4 + (5* Plunger.Position) -20
End Sub


'******************************************************

Sub Table1_KeyDown(ByVal keycode)	'***What to do when a button is pressed***

	If keycode = LeftTiltKey Then Nudge 90, 0.5 : SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 0.5 : SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 0.5 : SoundNudgeCenter

	'If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then 'Use this for ROM based games
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	If KeyCode = PlungerKey Then
		Plunger.Pullback:SoundPlungerPull()
		TimerVRPlunger.Enabled = True
		TimerVRPlunger1.Enabled = False
		VR_Shooter.Y = -4
	End If

	If keycode = LeftFlipperKey Then 
		VRFlipperButtonLeft.X = 2112 + 6
	End If
	If keycode = RightFlipperKey Then
		VRFlipperButtonRight.X = 2094 - 6
	End If

	If keycode = StartGameKey Then
		StartButton.y = 2793 - 5
		SoundStartButton
	End If

	If KeyDownHandler(keycode) Then Exit Sub
End Sub
 
Sub Table1_KeyUp(ByVal keycode)		'***What to do when a button is released***
	If keycode = PlungerKey Then 
		Plunger.firespeed = RndInt (108,110)
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		TimerVRPlunger.Enabled = False
		TimerVRPlunger1.Enabled = True
		VR_Shooter.Y = -4
	End If

	If keycode = LeftFlipperKey Then 
		VRFlipperButtonLeft.X = 2112
		Controller.Switch(21) = 0
	End If

	If keycode = RightFlipperKey Then
		VRFlipperButtonRight.X = 2094
		Controller.Switch(21) = 0
	End If

	If keycode = StartGameKey Then
		StartButton.y = 2793
	End If

	If KeyUpHandler(keycode) Then Exit Sub
End Sub


'******************************************************
'	ZSOL: Solenoids & Flashers
'******************************************************


SolCallback(1)  = "SolSaucer"		'Saucer kick
SolCallback(8)  = "SolKnocker"		'Knocker
SolCallback(9)  = "SolTrough"		'Outhole kick to plunger lane
SolCallback(10) = "SolGameOver"		'GameOver Sub

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"


Dim GameInPlay:GameInPlay=0
Dim GameInPlayFirstTime:  GameInPlayFirstTime = 0

Sub SolGameOver(enabled)
	If enabled Then
		GameInPlay = 1
		GameInPlayFirstTime = 1
	Else
		GameInPlay = 0
	End If
End Sub


'******************************************************
'	ZDRN: Drain, Trough, and Ball Release
'******************************************************

'********************* TROUGH *************************


' *** NOTE:  The Ball Release is handled under the Controller.Lamp(12) call in the updatelamps callback routines

Sub BallRelease_Hit : Controller.Switch(14) = 1 : UpdateTrough : End Sub
Sub BallRelease_UnHit :Controller.Switch(14) = 0 : UpdateTrough : End Sub
Sub Slot1_Hit()		: Controller.Switch(44) = 1: 	UpdateTrough:End Sub
Sub Slot1_UnHit()	: Controller.Switch(44) = 0:	UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If Slot1.BallCntOver = 0 Then Drain.kick 60, 12
	Me.Enabled = 0
End Sub

'*****************  DRAIN & RELEASE  ******************

Sub Drain_Hit()
	RandomSoundDrain Drain
	UpdateTrough
End Sub


Sub SolTrough(enabled)
	If enabled Then 
		If BallRelease.BallCntOver = 0 Then Slot1.kick 60, 8
	End If
End Sub


'******************************************************
'	ZFLP: FLIPPERS
'******************************************************
Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		FlipperActivate LeftFlipper, LFPress
		LF.Fire
		Controller.Switch(21) = 1
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart
		Controller.Switch(21) = 0
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		FlipperActivate RightFlipper, RFPress
		RF.Fire
		Controller.Switch(21) = 1
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		FlipperDeActivate RightFlipper, RFPress
		RightFlipper.RotateToStart
		Controller.Switch(21) = 0
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub


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


'************************************************************
'	ZSLG: Slingshot Animations
'************************************************************

Dim Lstep, Rstep

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotLeft slingL
    DOF 104, DOFPulse
	vpmTimer.PulseSw 55
	LSling.Visible = 0
	LSling1.Visible = 1
	slingL.objroty = 15
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:slingL.objroty = 7
		Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:slingL.objroty = 0:LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotRight slingR
    DOF 105, DOFPulse
	vpmTimer.PulseSw 55
	RSling.Visible = 0
	RSling1.Visible = 1
	slingR.objroty = -15
	RStep = 0
	RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 2:RSLing1.Visible = 0:RSLing2.Visible = 1:slingR.objroty = -7
		Case 3:RSLing2.Visible = 0:RSLing.Visible = 1:slingR.objroty = 0:RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub


'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS: Set LS = New SlingshotCorrection
Dim RS: Set RS = New SlingshotCorrection

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
	a = Array(RS)
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


'************************************************************
'	ZSWI: SWITCHES 
'************************************************************


'*******************************************
' Bumpers
'*******************************************

Sub Bumper1_Hit 'Left
	RandomSoundBumperMiddle  LeafSwitch004 'Bumper1 - pushing wider left
	vpmTimer.PulseSw 45
	DOF 101, DOFPulse 'ROM isn't sending solenoid call... matches what's in DOF config utility
End Sub

Sub Bumper2_Hit 'Middle
	RandomSoundBumperTop Bumper2
	vpmTimer.PulseSw 45
	DOF 102, DOFPulse 'ROM isn't sending solenoid call... matches what's in DOF config utility
End Sub

Sub Bumper3_Hit 'Right
	RandomSoundBumperMiddle LeafSwitch003 'Bumper3 - pushing wider right
	vpmTimer.PulseSw 45
	DOF 103, DOFPulse 'ROM isn't sending solenoid call... matches what's in DOF config utility
End Sub


'*******************************************
'Standup Targets
'*******************************************

Sub sw1_Hit: STHit 1: End Sub
Sub sw1o_Hit: TargetBouncer ActiveBall, 1: End Sub
Sub sw2_Hit: STHit 2: End Sub
Sub sw2o_Hit: TargetBouncer ActiveBall, 1: End Sub
Sub sw3_Hit: STHit 3: End Sub
Sub sw3o_Hit: TargetBouncer ActiveBall, 1: End Sub
Sub sw11_Hit: STHit 11: End Sub
Sub sw11o_Hit: TargetBouncer ActiveBall, 1: End Sub
Sub sw41_Hit: STHit 41: End Sub
Sub sw41o_Hit: TargetBouncer ActiveBall, 1: End Sub
Sub sw51_Hit: STHit 51: End Sub
Sub sw51o_Hit: TargetBouncer ActiveBall, 1: End Sub


'*******************************************
'Sensors
'*******************************************

Sub Sensor55_Hit(idx)
	vpmTimer.PulseSw 55
End Sub


'************************ Rollovers *************************

Sub BIPL_Trigger_Hit(): BIPL = 1: End Sub	'Plunger Lane Rollover Switch
Sub BIPL_Trigger_UnHit: BIPL = 0: End Sub

Sub sw00_Hit:Controller.Switch(0) = 1:End Sub
Sub sw00_UnHit:Controller.Switch(0) = 0:End Sub
Sub sw10_Hit:Controller.Switch(10) = 1:End Sub
Sub sw10_UnHit:Controller.Switch(10) = 0:End Sub
Sub sw12_Hit:Controller.Switch(12) = 1:End Sub
Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub
Sub sw13_Hit:Controller.Switch(13) = 1:End Sub
Sub sw13_UnHit:Controller.Switch(13) = 0:End Sub
Sub sw40_Hit:Controller.Switch(40) = 1:End Sub
Sub sw40_UnHit:Controller.Switch(40) = 0:End Sub
Sub sw42_Hit:Controller.Switch(42) = 1:End Sub
Sub sw42_UnHit:Controller.Switch(42) = 0:End Sub
Sub sw43_Hit:Controller.Switch(43) = 1:End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub
Sub sw50_Hit:Controller.Switch(50) = 1:End Sub
Sub sw50_UnHit:Controller.Switch(50) = 0:End Sub


'************************ Rollover Animationss *************************

Sub sw00_Animate:	psw00.transz = sw00.CurrentAnimOffset: End Sub
Sub sw10_Animate:	psw10.transz = sw10.CurrentAnimOffset: End Sub
Sub sw12_Animate:	psw12.transz = sw12.CurrentAnimOffset: End Sub
Sub sw13_Animate:	psw13.transz = sw13.CurrentAnimOffset: End Sub
Sub sw40_Animate:	psw40.transz = sw40.CurrentAnimOffset: End Sub
Sub sw42_Animate:	psw42.transz = sw42.CurrentAnimOffset: End Sub
Sub sw43_Animate:	psw43.transz = sw43.CurrentAnimOffset: End Sub
Sub sw50_Animate:	psw50.transz = sw50.CurrentAnimOffset: End Sub


'*******************************************
'  Knocker
'*******************************************

Sub SolKnocker(enabled)
	If Enabled Then
		If GameInPlayFirstTime = 1 Then
			KnockerSolenoid
		End If
	End If
End Sub


'*******************************************
'  Saucer
'*******************************************

Dim RNDKickScenario	'Random Values for saucer kick and angles

Sub SolSaucer(Enabled)
  If Enabled Then
	If sw4.BallCntOver = 1 Then
		RNDKickScenario = RndInt(1,10)

		Select case RNDKickScenario
			case 1: sw4.kick 168,9
			case 2: sw4.kick 169,10
			case 3: sw4.kick 169,10
			case 4: sw4.kick 170,9
			case 5: sw4.kick 170,9
			case 6: sw4.kick 170,9
			case 7: sw4.kick 170,9
			case 8: sw4.kick 170,9
			case 9: sw4.kick 170,9
			case 10: sw4.kick 170,9
		End Select

		sw4.uservalue=1
		sw4.timerenabled=1
		PkickarmSW4.rotz=15
	End If
  End if
End Sub

Sub sw4_timer
  select case sw4.uservalue
    case 2:
      PkickarmSW4.rotz=0
      me.timerenabled=0
  end Select
  sw4.uservalue=sw4.uservalue+1
End Sub

Sub sw4_Hit
	SoundSaucerLock
	Controller.Switch(4) = 1
End Sub

Sub sw4_Unhit
	SoundSaucerKick 1, sw4
	controller.switch(4) = 0
End Sub


'*******************************************
'  Block Reflections in saucer
'*******************************************

sub BallReflectionMask_Hit() : table1.BallPlayfieldReflectionScale = 0 : end Sub
sub BallReflectionMask_UnHit() : table1.BallPlayfieldReflectionScale = 1 : end Sub

'*******************************************
' Spinners
'*******************************************

Sub Spinner_Spin()
	vpmtimer.PulseSw 53
	SoundSpinner Spinner
End Sub


'***********Rotate Spinner

Dim SpinnerRadius: SpinnerRadius = 7

Sub Spinner_Animate
	Dim BP, a, b
	a = Spinner.CurrentAngle 
	SpinnerPrim.Rotx = Spinner.CurrentAngle
	SpinnerRod.TransZ = (cos((Spinner.CurrentAngle + 180) * (PI/180))+1) * SpinnerRadius
	SpinnerRod.TransY = sin((Spinner.CurrentAngle) * (PI/180)) * -SpinnerRadius

	If a >= 0 And a < 60 Then
		b = 0
	ElseIf a >= 60 And a < 120 Then
		b = (a - 60) / 60
	ElseIf a >= 120 And a < 240 Then
		b = 1
	ElseIf a >= 240 And a < 300 Then
		b = 1 + (240 - a) / 60
	Else
		b = 0
	End If

	For Each BP in BP_SP_Parts
		BP.RotX = a
		BP.Opacity = 100 * (1 - b)
	Next
	For Each BP in BP_SP_PartsB
		BP.RotX = -a
		BP.Opacity = 100 * b
	Next


End Sub


'*******************************************
' Gate Triggers
'*******************************************

Sub sw52_Hit(): vpmTimer.PulseSw (52): Gate3.Damping=0.85: sw52step=0: me.timerenabled = True: End Sub

Dim sw52step

Sub Sw52_timer()
	Select case sw52step
		Case 0:'Gate3.Damping = .9:'Gate3.GravityFactor = 5
		Case 1:Gate3.Damping = .9:Gate3.GravityFactor = 3
		Case 2:Gate3.Damping = .95
		Case 3:Gate3.Damping = .99999
		Case 4:
		Case 5: me.timerenabled = false:sw52step = 0:Gate3.Damping = .85:Gate3.GravityFactor = 5
	End Select
	sw52step = sw52step + 1
End Sub


'******************************************************
'	ZBRL: BALL ROLLING AND DROP SOUNDS
'******************************************************
'
' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

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
	Dim b'

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


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
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
		AudioFade = CSng(tmp ^ 5) 'was 10
	Else
		AudioFade = CSng( - (( - tmp) ^ 5) ) 'was 10
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
		AudioPan = CSng(tmp ^ 5) 'was 10
	Else
		AudioPan = CSng( - (( - tmp) ^ 5) ) 'was 10
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
Const RelayGISoundLevel = 1	  'volume level; range [0, 1];

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
'****  END FLEEP MECHANICAL SOUNDS
'******************************************************




'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
Sub InitPolarity()
   dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 80
		x.DebugOn=False ' prints some info in debugger

        x.AddPt "Polarity", 0, 0, 0
        x.AddPt "Polarity", 1, 0.05, - 2.7
        x.AddPt "Polarity", 2, 0.16, - 2.7
        x.AddPt "Polarity", 3, 0.22, - 0
        x.AddPt "Polarity", 4, 0.25, - 0
        x.AddPt "Polarity", 5, 0.3, - 1
        x.AddPt "Polarity", 6, 0.4, - 2
        x.AddPt "Polarity", 7, 0.5, - 2.7
        x.AddPt "Polarity", 8, 0.65, - 1.8
        x.AddPt "Polarity", 9, 0.75, - 0.5
        x.AddPt "Polarity", 10, 0.81, - 0.5
        x.AddPt "Polarity", 11, 0.88, 0
        x.AddPt "Polarity", 12, 1.3, 0

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
Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
'Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
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

Const LiveCatch = 12 	'16 'recommendation to make it less easier.
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
   Const EOSReturn = 0.045  'late 70's to mid 80's
'	Const EOSReturn = 0.035  'mid 80's to early 90's
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
		Dim b
		
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

' Put all the Post and Pin objects in dPosts collection. Make sure dPosts fires hit events.
Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

' This collection contains the bottom sling posts. They are not in the dPosts collection so that the TargetBouncer is not applied to them, but they should still have dampening applied
' If you experience airballs with posts or targets, consider adding them to this collection
Sub NoTargetBouncer_Hit
    RubbersD.dampen ActiveBall
End Sub

' Put all the Sleeve objects in dSleeves collection. Make sure dSleeves fires hit events.
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
'		aBall.velz = aBall.velz * coef
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


'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************


'******************************************************
'	ZRST: STAND-UP TARGET INITIALIZATION
'******************************************************

Class StandupTarget
  Private m_primary, m_prim, m_sw, m_animate

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public default Function init(primary, prim, sw, animate)
    Set m_primary = primary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate

    Set Init = Me
  End Function
End Class


'Define a variable for each stand-up target

Dim ST1, ST2, ST3, ST11, ST41, ST51


'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
' 
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts

Set ST1  = (new StandupTarget)(sw1, psw1,1, 0)
Set ST2  = (new StandupTarget)(sw2, psw2,2, 0)
Set ST3  = (new StandupTarget)(sw3, psw3,3, 0)
Set ST11  = (new StandupTarget)(sw11, psw11,11, 0)
Set ST41  = (new StandupTarget)(sw41, psw41,41, 0)
Set ST51  = (new StandupTarget)(sw51, psw51,51, 0)


'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
'   STAnimationArray = Array(ST1, ST2, ....)

Dim STArray
STArray = Array(ST1, ST2, ST3, ST11, ST41, ST51)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 		'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit

Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)
	
	PlayTargetSound
	STArray(i).animate = STCheckHit(ActiveBall,STArray(i).primary)
	
	If STArray(i).animate <> 0 Then
		STBallPhysics ActiveBall, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 To UBound(STArray)

		if switch > 100 Then		' This is for the other three targets to get the correct id with same switch ID.
			STArrayID = switch - 120
			Exit Function
		Else

			If STArray(i).sw = switch Then
				STArrayID = i
				Exit Function
			End If
		End If
	Next
End Function

Function STCheckHit(aBall, target) 'Check if target is hit on it's face
	Dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		STCheckHit = 1
	Else
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i = 0 To UBound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime
	
	STAnimate = animate
	
	If animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If animate = 1 Then
		primary.collidable = 0
		prim.transy =  - STMaxOffset
		vpmTimer.PulseSw switch mod 100
		STAnimate = 2
		Exit Function
	ElseIf animate = 2 Then
		prim.transy = prim.transy + STAnimStep
		If prim.transy >= 0 Then
			prim.transy = 0
			primary.collidable = 1
			STAnimate = 0
			Exit Function
		Else
			STAnimate = 2
		End If
	End If
End Function


Sub STBallPhysics(aBall, angle, mass)
	Dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	
	calc1 = cor.BallVel(aball.id) * Cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Cos(rangle + 4 * Atn(1) / 2)
	calc3 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Sin(rangle + 4 * Atn(1) / 2)
	
	aBall.velx = calc1 * Cos(rangle) + calc2
	aBall.vely = calc1 * Sin(rangle) + calc3
End Sub

'******************************************************
'***	END STAND-UP TARGETS
'******************************************************


'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9 	'Level of bounces. Recommmended value of 0.7-1.0

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

'****************************************************************
'****  END VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'****************************************************************


'******************************************************
' 	Z3DI:   3D INSERTS
'******************************************************

dim insert_red_on: insert_red_on = insertfactor * 200
const insert_red_off = 4
dim insert_orange_on: insert_orange_on = insertfactor * 200
const insert_orange_off = 30
dim insert_green_on: insert_green_on = insertfactor * 180
const insert_green_off = 0.25
dim insert_blue_on: insert_blue_on = insertfactor * 300
const insert_blue_off = 10
dim insert_white_on: insert_white_on = insertfactor * 1.35
const insert_white_off = 0.25
dim insert_yellow_on: insert_yellow_on = insertfactor * 1.25
const insert_yellow_off = 0.25
dim insert_pink_on: insert_pink_on = insertfactor * 1.5
const insert_pink_off = 0.25

const a_insert_white_on = 0.5
const a_insert_white_off = 0.5
const a_insert_red_on = 0.1
const a_insert_red_off = 0.15
const a_insert_orange_on = 0.1
const a_insert_orange_off = 0.105
const a_insert_green_on = 1
const a_insert_green_off = 1
const a_insert_blue_on = 0.105
const a_insert_blue_off = 0.105
const a_insert_yellow_on = 0.105
const a_insert_yellow_off = 0.105
const a_insert_pink_on = 0.105
const a_insert_pink_off = 0.105

const bulb_on = 10
const bulb_off = 1


'************************
'**** Blue Inserts

Sub l44_animate
	p44.BlendDisableLighting = insert_blue_off + insert_blue_on * (l44.GetInPlayIntensity / l44.Intensity)
	p44off.BlendDisableLighting = a_insert_blue_off - a_insert_blue_on * (l44.GetInPlayIntensity / l44.Intensity)
End Sub


'************************
'**** Orange Inserts

Sub l24_animate
	p24.BlendDisableLighting = insert_orange_off + insert_orange_on * (l24.GetInPlayIntensity / l24.Intensity)
	p24off.BlendDisableLighting = a_insert_orange_off - a_insert_orange_on * (l24.GetInPlayIntensity / l24.Intensity)
End Sub

Sub l25_animate
	p25.BlendDisableLighting = insert_orange_off + insert_orange_on * (l25.GetInPlayIntensity / l25.Intensity)
	p25off.BlendDisableLighting = a_insert_orange_off - a_insert_orange_on * (l25.GetInPlayIntensity / l25.Intensity)
End Sub

Sub l26_animate
	p26.BlendDisableLighting = insert_orange_off + insert_orange_on * (l26.GetInPlayIntensity / l26.Intensity)
	p26off.BlendDisableLighting = a_insert_orange_off - a_insert_orange_on * (l26.GetInPlayIntensity / l26.Intensity)
End Sub

Sub l27_animate
	p27.BlendDisableLighting = insert_orange_off + insert_orange_on * (l27.GetInPlayIntensity / l27.Intensity)
	p27off.BlendDisableLighting = a_insert_orange_off - a_insert_orange_on * (l27.GetInPlayIntensity / l27.Intensity)
End Sub

Sub l28_animate
	p28.BlendDisableLighting = insert_orange_off + insert_orange_on * (l28.GetInPlayIntensity / l28.Intensity)
	p28off.BlendDisableLighting = a_insert_orange_off - a_insert_orange_on * (l28.GetInPlayIntensity / l28.Intensity)
End Sub

Sub l29_animate
	p29.BlendDisableLighting = insert_orange_off + insert_orange_on * (l29.GetInPlayIntensity / l29.Intensity)
	p29off.BlendDisableLighting = a_insert_orange_off - a_insert_orange_on * (l29.GetInPlayIntensity / l29.Intensity)
End Sub

Sub l30_animate
	p30.BlendDisableLighting = insert_orange_off + insert_orange_on * (l30.GetInPlayIntensity / l30.Intensity)
	p30off.BlendDisableLighting = a_insert_orange_off - a_insert_orange_on * (l30.GetInPlayIntensity / l30.Intensity)
End Sub

Sub l31_animate
	p31.BlendDisableLighting = insert_orange_off + insert_orange_on * (l31.GetInPlayIntensity / l31.Intensity)
	p31off.BlendDisableLighting = a_insert_orange_off - a_insert_orange_on * (l31.GetInPlayIntensity / l31.Intensity)
End Sub

Sub l32_animate
	p32.BlendDisableLighting = insert_orange_off + insert_orange_on * (l32.GetInPlayIntensity / l32.Intensity)
	p32off.BlendDisableLighting = a_insert_orange_off - a_insert_orange_on * (l32.GetInPlayIntensity / l32.Intensity)
End Sub


'************************
'**** Red Inserts 

Sub l45_animate
	p45.BlendDisableLighting = insert_red_off + insert_red_on * (l45.GetInPlayIntensity / l45.Intensity)
	p45off.BlendDisableLighting = a_insert_red_off - a_insert_red_on * (l45.GetInPlayIntensity / l45.Intensity)
End Sub

Sub l46_animate
	p46.BlendDisableLighting = insert_red_off + insert_red_on * (l46.GetInPlayIntensity / l46.Intensity)
	p46off.BlendDisableLighting = a_insert_red_off - a_insert_red_on * (l46.GetInPlayIntensity / l46.Intensity)
End Sub


'************************
'**** Green Inserts

Sub l4_animate
	p4.BlendDisableLighting = insert_green_off + insert_green_on * (l4.GetInPlayIntensity / l4.Intensity)
	p4off.BlendDisableLighting = a_insert_green_off - a_insert_green_on * (l4.GetInPlayIntensity / l4.Intensity)
End Sub

Sub l5_animate
	p5.BlendDisableLighting = insert_green_off + insert_green_on * (l5.GetInPlayIntensity / l5.Intensity)
	p5off.BlendDisableLighting = a_insert_green_off - a_insert_green_on * (l5.GetInPlayIntensity / l5.Intensity)
End Sub

Sub l6_animate
	p6.BlendDisableLighting = insert_green_off + insert_green_on * (l6.GetInPlayIntensity / l6.Intensity)
	p6off.BlendDisableLighting = a_insert_green_off - a_insert_green_on * (l6.GetInPlayIntensity / l6.Intensity)
End Sub

Sub l7_animate
	p7.BlendDisableLighting = insert_green_off + insert_green_on * (l7.GetInPlayIntensity / l7.Intensity)
	p7off.BlendDisableLighting = a_insert_green_off - a_insert_green_on * (l7.GetInPlayIntensity / l7.Intensity)
End Sub

Sub l34_animate
	p34.BlendDisableLighting = insert_green_off + insert_green_on * (l34.GetInPlayIntensity / l34.Intensity)
	p34off.BlendDisableLighting = a_insert_green_off - a_insert_green_on * (l34.GetInPlayIntensity / l34.Intensity)
End Sub

Sub l37_animate
	p37.BlendDisableLighting = insert_green_off + insert_green_on * (l37.GetInPlayIntensity / l37.Intensity)
	p37off.BlendDisableLighting = a_insert_green_off - a_insert_green_on * (l37.GetInPlayIntensity / l37.Intensity)
End Sub

Sub l38_animate
	p38.BlendDisableLighting = insert_green_off + insert_green_on * (l38.GetInPlayIntensity / l38.Intensity)
	p38off.BlendDisableLighting = a_insert_green_off - a_insert_green_on * (l38.GetInPlayIntensity / l38.Intensity)
End Sub

Sub l39_animate
	p39.BlendDisableLighting = insert_green_off + insert_green_on * (l39.GetInPlayIntensity / l39.Intensity)
	p39off.BlendDisableLighting = a_insert_green_off - a_insert_green_on * (l39.GetInPlayIntensity / l39.Intensity)
End Sub


'************************
'**** Yellow Inserts

Sub l35_animate
	p35.BlendDisableLighting = insert_yellow_off + insert_yellow_on * (l35.GetInPlayIntensity / l35.Intensity)
	p35off.BlendDisableLighting = a_insert_yellow_off - a_insert_yellow_on * (l35.GetInPlayIntensity / l35.Intensity)
	bulb35.BlendDisableLighting = bulb_off + bulb_on * (l35.GetInPlayIntensity / l35.Intensity)
End Sub

Sub l43_animate
	p43.BlendDisableLighting = insert_yellow_off + insert_yellow_on * (l43.GetInPlayIntensity / l43.Intensity)
	p43off.BlendDisableLighting = a_insert_yellow_off - a_insert_yellow_on * (l43.GetInPlayIntensity / l43.Intensity)
	bulb43.BlendDisableLighting = bulb_off + bulb_on * (l43.GetInPlayIntensity / l43.Intensity)
End Sub

Sub l47_animate
	p47.BlendDisableLighting = insert_yellow_off + insert_yellow_on * (l47.GetInPlayIntensity / l47.Intensity)
	p47off.BlendDisableLighting = a_insert_yellow_off - a_insert_yellow_on * (l47.GetInPlayIntensity / l47.Intensity)
	bulb47.BlendDisableLighting = bulb_off + bulb_on * (l47.GetInPlayIntensity / l47.Intensity)
End Sub


'************************
'**** Pink Inserts

Sub l3_animate
	p3.BlendDisableLighting = insert_pink_off + insert_pink_on * (l3.GetInPlayIntensity / l3.Intensity)
	p3off.BlendDisableLighting = a_insert_pink_off - a_insert_pink_on * (l3.GetInPlayIntensity / l3.Intensity)
	bulb3.BlendDisableLighting = bulb_off + bulb_on * (l3.GetInPlayIntensity / l3.Intensity)
End Sub

Sub l36_animate
	p36.BlendDisableLighting = insert_pink_off + insert_pink_on * (l36.GetInPlayIntensity / l36.Intensity)
	p36off.BlendDisableLighting = a_insert_pink_off - a_insert_pink_on * (l36.GetInPlayIntensity / l36.Intensity)
	bulb36.BlendDisableLighting = bulb_off + bulb_on * (l36.GetInPlayIntensity / l36.Intensity)
End Sub


'************************
'**** White Inserts 

Sub l13_animate
	p13.BlendDisableLighting = insert_white_off + insert_white_on * (l13.GetInPlayIntensity / l13.Intensity)
	p13off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l13.GetInPlayIntensity / l13.Intensity)
	bulb13.BlendDisableLighting = bulb_off + bulb_on * (l13.GetInPlayIntensity / l13.Intensity)
End Sub

Sub l14_animate
	p14.BlendDisableLighting = insert_white_off + insert_white_on * (l14.GetInPlayIntensity / l14.Intensity)
	p14off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l14.GetInPlayIntensity / l14.Intensity)
	bulb14.BlendDisableLighting = bulb_off + bulb_on * (l14.GetInPlayIntensity / l14.Intensity)
End Sub

Sub l15_animate
	p15.BlendDisableLighting = insert_white_off + insert_white_on * (l15.GetInPlayIntensity / l15.Intensity)
	p15off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l15.GetInPlayIntensity / l15.Intensity)
	bulb15.BlendDisableLighting = bulb_off + bulb_on * (l15.GetInPlayIntensity / l15.Intensity)
End Sub

Sub l16_animate
	p16.BlendDisableLighting = insert_white_off + insert_white_on * (l16.GetInPlayIntensity / l16.Intensity)
	p16off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l16.GetInPlayIntensity / l16.Intensity)
	bulb16.BlendDisableLighting = bulb_off + bulb_on * (l16.GetInPlayIntensity / l16.Intensity)
End Sub

Sub l17_animate
	p17.BlendDisableLighting = insert_white_off + insert_white_on * (l17.GetInPlayIntensity / l17.Intensity)
	p17off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l17.GetInPlayIntensity / l17.Intensity)
	bulb17.BlendDisableLighting = bulb_off + bulb_on * (l17.GetInPlayIntensity / l17.Intensity)
End Sub

Sub l18_animate
	p18.BlendDisableLighting = insert_white_off + insert_white_on * (l18.GetInPlayIntensity / l18.Intensity)
	p18off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l18.GetInPlayIntensity / l18.Intensity)
	bulb18.BlendDisableLighting = bulb_off + bulb_on * (l18.GetInPlayIntensity / l18.Intensity)
End Sub

Sub l19_animate
	p19.BlendDisableLighting = insert_white_off + insert_white_on * (l19.GetInPlayIntensity / l19.Intensity)
	p19off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l19.GetInPlayIntensity / l19.Intensity)
	bulb19.BlendDisableLighting = bulb_off + bulb_on * (l19.GetInPlayIntensity / l19.Intensity)
End Sub

Sub l20_animate
	p20.BlendDisableLighting = insert_white_off + insert_white_on * (l20.GetInPlayIntensity / l20.Intensity)
	p20off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l20.GetInPlayIntensity / l20.Intensity)
	bulb20.BlendDisableLighting = bulb_off + bulb_on * (l20.GetInPlayIntensity / l20.Intensity)
End Sub

Sub l21_animate
	p21.BlendDisableLighting = insert_white_off + insert_white_on * (l21.GetInPlayIntensity / l21.Intensity)
	p21off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l21.GetInPlayIntensity / l21.Intensity)
	bulb21.BlendDisableLighting = bulb_off + bulb_on * (l21.GetInPlayIntensity / l21.Intensity)
End Sub

Sub l22_animate
	p22.BlendDisableLighting = insert_white_off + insert_white_on * (l22.GetInPlayIntensity / l22.Intensity)
	p22off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l22.GetInPlayIntensity / l22.Intensity)
	bulb22.BlendDisableLighting = bulb_off + bulb_on * (l22.GetInPlayIntensity / l22.Intensity)
End Sub

Sub l23_animate
	p23.BlendDisableLighting = insert_white_off + insert_white_on * (l23.GetInPlayIntensity / l23.Intensity)
	p23off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l23.GetInPlayIntensity / l23.Intensity)
	bulb23.BlendDisableLighting = bulb_off + bulb_on * (l23.GetInPlayIntensity / l23.Intensity)
End Sub

Sub l33_animate
	p33.BlendDisableLighting = insert_white_off + insert_white_on * (l33.GetInPlayIntensity / l33.Intensity)
	p33off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l33.GetInPlayIntensity / l33.Intensity)
	bulb33.BlendDisableLighting = bulb_off + bulb_on * (l33.GetInPlayIntensity / l33.Intensity)
End Sub

Sub l40_animate
	p40.BlendDisableLighting = insert_white_off + insert_white_on * (l40.GetInPlayIntensity / l40.Intensity)
	p40off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l40.GetInPlayIntensity / l40.Intensity)
	bulb40.BlendDisableLighting = bulb_off + bulb_on * (l40.GetInPlayIntensity / l40.Intensity)
End Sub

Sub l41_animate
	p41.BlendDisableLighting = insert_white_off + insert_white_on * (l41.GetInPlayIntensity / l41.Intensity)
	p41off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l41.GetInPlayIntensity / l41.Intensity)
	bulb41.BlendDisableLighting = bulb_off + bulb_on * (l41.GetInPlayIntensity / l41.Intensity)
End Sub

Sub l42_animate
	p42.BlendDisableLighting = insert_white_off + insert_white_on * (l42.GetInPlayIntensity / l42.Intensity)
	p42off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l42.GetInPlayIntensity / l42.Intensity)
	bulb42.BlendDisableLighting = bulb_off + bulb_on * (l42.GetInPlayIntensity / l42.Intensity)
End Sub

Sub l132_animate
	p132.BlendDisableLighting = insert_white_off + insert_white_on * (l132.GetInPlayIntensity / l132.Intensity)
	p132off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l132.GetInPlayIntensity / l132.Intensity)
	bulb132.BlendDisableLighting = bulb_off + bulb_on * (l132.GetInPlayIntensity / l132.Intensity)
End Sub

Sub l142_animate
	p142.BlendDisableLighting = insert_white_off + insert_white_on * (l142.GetInPlayIntensity / l142.Intensity)
	p142off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l142.GetInPlayIntensity / l142.Intensity)
	bulb142.BlendDisableLighting = bulb_off + bulb_on * (l142.GetInPlayIntensity / l142.Intensity)
End Sub

Sub l152_animate
	p152.BlendDisableLighting = insert_white_off + insert_white_on * (l152.GetInPlayIntensity / l152.Intensity)
	p152off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l152.GetInPlayIntensity / l152.Intensity)
	bulb152.BlendDisableLighting = bulb_off + bulb_on * (l152.GetInPlayIntensity / l152.Intensity)
End Sub

Sub l162_animate
	p162.BlendDisableLighting = insert_white_off + insert_white_on * (l162.GetInPlayIntensity / l162.Intensity)
	p162off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l162.GetInPlayIntensity / l162.Intensity)
	bulb162.BlendDisableLighting = bulb_off + bulb_on * (l162.GetInPlayIntensity / l162.Intensity)
End Sub

Sub l172_animate
	p172.BlendDisableLighting = insert_white_off + insert_white_on * (l172.GetInPlayIntensity / l172.Intensity)
	p172off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l172.GetInPlayIntensity / l172.Intensity)
	bulb172.BlendDisableLighting = bulb_off + bulb_on * (l172.GetInPlayIntensity / l172.Intensity)
End Sub

Sub l182_animate
	p182.BlendDisableLighting = insert_white_off + insert_white_on * (l182.GetInPlayIntensity / l182.Intensity)
	p182off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l182.GetInPlayIntensity / l182.Intensity)
	bulb182.BlendDisableLighting = bulb_off + bulb_on * (l182.GetInPlayIntensity / l182.Intensity)
End Sub

Sub l192_animate
	p192.BlendDisableLighting = insert_white_off + insert_white_on * (l192.GetInPlayIntensity / l192.Intensity)
	p192off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l192.GetInPlayIntensity / l192.Intensity)
	bulb192.BlendDisableLighting = bulb_off + bulb_on * (l192.GetInPlayIntensity / l192.Intensity)
End Sub

Sub l202_animate
	p202.BlendDisableLighting = insert_white_off + insert_white_on * (l202.GetInPlayIntensity / l202.Intensity)
	p202off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l202.GetInPlayIntensity / l202.Intensity)
	bulb202.BlendDisableLighting = bulb_off + bulb_on * (l202.GetInPlayIntensity / l202.Intensity)
End Sub

Sub l212_animate
	p212.BlendDisableLighting = insert_white_off + insert_white_on * (l212.GetInPlayIntensity / l212.Intensity)
	p212off.BlendDisableLighting = a_insert_white_off - a_insert_white_on * (l212.GetInPlayIntensity / l212.Intensity)
	bulb212.BlendDisableLighting = bulb_off + bulb_on * (l212.GetInPlayIntensity / l212.Intensity)
End Sub

'************************
'**** Bumper Lamps

' bumpers
Sub b1_animate
	flbumpvalue1 = bumperflashlevel * (b1.GetInPlayIntensity / b1.Intensity)
	FlFadeBumper 1, flbumpvalue1
End Sub

Sub b2_animate
	flbumpvalue2 = bumperflashlevel * (b2.GetInPlayIntensity / b2.Intensity)
	FlFadeBumper 2, flbumpvalue2
End Sub

' bumper 3
Sub b3_animate
	flbumpvalue3 = bumperflashlevel * (b3.GetInPlayIntensity / b3.Intensity)
	FlFadeBumper 3, flbumpvalue3
End Sub

'******************************************************
'*****   END 3D INSERTS
'******************************************************



'******************************************************
'****  ZGIU:	GI Control
'******************************************************


sub PFGI(Enabled)
dim x
	if Enabled = 1 Then

		If DesktopMode = True or VR_Room = 1 Then:	Sound_GI_Relay 1, KnockerPosition

		If DesktopMode = True then
			L_DT_1.intensity = 12
			L_DT_2.intensity = 6
			L_DT_2a.intensity = 6
			for each x in DT_Text
				x.visible = 1
			Next
		Else
			L_DT_1.intensity = 0.1
			L_DT_2.intensity = 0.1
			L_DT_2a.intensity = 0.1
			for each x in DT_Text
				x.visible = 0
			Next
		End If

		if VR_Room = 1 Then
			BGDark.Visible = 0
			BGBright.Visible = 1
		Else
			BGDark.Visible = 0
			BGBright.Visible = 0
		End If

	gilvl = 1

    Else

		If DesktopMode = True or VR_Room = 1 Then:	Sound_GI_Relay 0, KnockerPosition

		L_DT_1.intensity = 0.1
		L_DT_2.intensity = 0.1
		L_DT_2a.intensity = 0.1
			for each x in DT_Text
				x.visible = 0
			Next

		if VR_Room = 1 Then
			BGDark.Visible = 1
			BGBright.Visible = 0
		Else
			BGDark.Visible = 0
			BGBright.Visible = 0
		End If

	gilvl = 0

    End If

	SetRoomBrightness

End Sub


'******************************************************
'****  END GI Control
'******************************************************

'******************************************************
' 	ZFLB:  FLUPPER BUMPERS
'******************************************************

' prepare some global vars to dim/brighten objects when using day-night slider
Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0
	DNA45 = (NightDay - 10) / 20
	DNA90 = 0
	DayNightAdjust = 0.4
Else
	DNA30 = (NightDay - 10) / 30
	DNA45 = (NightDay - 10) / 45
	DNA90 = (NightDay - 10) / 90
	DayNightAdjust = NightDay / 25
End If

Dim FlBumperFadeActual(6), FlBumperFadeTarget(6), FlBumperColor(6), FlBumperTop(6), FlBumperCapBase(6), FlBumperSmallLight(6), Flbumperbiglight(6)
Dim FlBumperDisk(6), FlBumperBase(6), FlBumperBulb(6), FlBumperscrews(6), FlBumperActive(6), FlBumperHighlight(6), FLBumperlight(6), FlBumperRing(6)
Dim cnt
For cnt = 1 To 6
	FlBumperActive(cnt) = False
Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight
FlInitBumper 1, "alien"
FlInitBumper 2, "alien"
FlInitBumper 3, "alien"

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	
	' store all objects in an array for use in FlFadeBumper subroutine

	FlBumperFadeActual(nr) = 1
	FlBumperFadeTarget(nr) = 1.1
	FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr)
	Set FlBumperCapBase(nr) = Eval("bumpercapbase" & nr)
	Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	FlBumperTop(nr).material = "bumpertopmat" & nr
	FlBumperCapBase(nr).material = "bumpertopmata" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr)
	Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr)
	Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr)
	FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr)
	FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	Set FlBumperLight(nr) = Eval("bumperlight" & nr)
	Set FlBumperRing(nr) = Eval("BumperRing" & nr)


	' set the color for the two VPX lights
	Select Case col
		
		Case "alien"
			FlBumperSmallLight(nr).color = RGB(248,228,164)
			FlBumperSmallLight(nr).colorfull = RGB(253,245,224)
			FlBumperBigLight(nr).color = RGB(248,228,164)
			FlBumperBigLight(nr).colorfull = RGB(253,245,224)
			FlBumperHighlight(nr).color = RGB(253,245,224)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
			FlBumperTop(nr).BlendDisableLighting = 0.005
			MaterialColor "bumpertopmat" & nr, RGB(255,245,220)
			FlBumperCapBase(nr).BlendDisableLighting = 0.005
			MaterialColor "bumpertopmata" & nr, RGB(255,245,220)
			MaterialColor "bumperdisk" & nr, RGB(103,102,85)
			FlBumperDisk(nr).BlendDisableLighting = 0.5
			FlBumperRing(nr).BlendDisableLighting = 0
			FLBumperLight(nr).intensity = 0

	End Select
End Sub

Sub FlFadeBumper(nr, Z)

	'   UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
	'			   OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
	'			   float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material

	FlBumperDisk(nr).BlendDisableLighting = (bumperscale * 1.5 * LightLevel/lightlevelinitial) + (Z * 4 * bumperflashlevel)
	FlBumperBase(nr).BlendDisableLighting = (bumperscale * 2.25 * LightLevel/lightlevelinitial) + (2.5 * Z * bumperflashlevel)

	Select Case FlBumperColor(nr)

		Case "alien"
			FlBumperSmallLight(nr).intensity = bumperscale * (17 + 200 * Z)
			FlBumperSmallLight(nr).color = RGB(248,228 - 20*Z,164-65*Z) : FlBumperSmallLight(nr).colorfull = RGB(253,245 - 20*Z,224-65*Z)
			FlBumperBulb(nr).BlendDisableLighting = 12 + 2000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = bumperscale * (2.5 * Z)
			FlBumperHighlight(nr).opacity = bumperscale * (1000 * (Z^3) )
			FlBumperTop(nr).BlendDisableLighting = (bumperscale * (.45 * DayNightAdjust) * LightLevel/lightlevelinitial) + (0.625 * Z * bumperflashlevel / (1 + DNA90))
			FLBumperCapBase(nr).BlendDisableLighting = (bumperscale * (.45 * DayNightAdjust) * LightLevel/lightlevelinitial) + (0.625 * Z * bumperflashlevel / (1 + DNA90))
			MaterialColor "bumpertopmat" & nr, RGB(255,250 - z*6,220 - Z*15)
			MaterialColor "bumpertopmata" & nr, RGB(255,250 - z*6,220 - Z*15)
			MaterialColor "bumperbase" & nr, RGB(255,235 - z * 36,220 - Z * 90)
			FLBumperLight(nr).intensity =  ((30 * Z)*bumperflashlevel) - 2
			FlBumperLight(nr).color = RGB(248,228 - 20*Z,164-65*Z) : FlBumperLight(nr).colorfull = RGB(253,245 - 20*Z,224-65*Z)
			FLBumperRing(nr).BlendDisableLighting = (bumperscale * (0.01 * DayNightAdjust) * LightLevel/lightlevelinitial) + (0.015 * Z * bumperflashlevel / (1 + DNA90))

	End Select
End Sub


'******************************************************
'******  END FLUPPER BUMPERS
'******************************************************


'***************************************************************
'	ZSHA: Ambient ball shadows
'***************************************************************

' For dynamic ball shadows, Check the "Raytraced ball shadows" box for the specific light. 
' Also make sure the light's z position is around 25 (mid ball)

'Ambient (Room light source)
Const AmbientBSFactor = 0.9    '0 To 1, higher is darker
Const AmbientMovement = 1	   '1+ higher means more movement as the ball moves left and right
Const offsetX = 0			   'Offset x position under ball (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY = 0			   'Offset y position under ball (^^for example 5,5 if the light is in the back left corner)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
Dim objBallShadow(6)

'Initialization
BSInit

Sub BSInit()
	Dim iii
	'Prepare the shadow objects before play begins
	For iii = 0 To tnob - 1
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 3 + iii / 1000
		objBallShadow(iii).visible = 0
	Next
End Sub


Sub BSUpdate
	Dim s: For s = lob To UBound(gBOT)
		' *** Normal "ambient light" ball shadow
		
		'Primitive shadow on playfield, flasher shadow in ramps
		'** If on main and upper pf
		If gBOT(s).Z > 20 And gBOT(s).Z < 30 Then
			objBallShadow(s).visible = 1
			objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
			objBallShadow(s).Y = gBOT(s).Y + offsetY
			'objBallShadow(s).Z = gBOT(s).Z + s/1000 + 1.04 - 25	

		'** No shadow if ball is off the main playfield (this may need to be adjusted per table)
		Else
			objBallShadow(s).visible = 0
		End If
	Next
End Sub


'*******************************************
'Digital Display
'*******************************************

Dim Digits(32)

Digits(0)=Array(a001,a002,a003,a004,a005,a006,a007,LXM,a008)
Digits(1)=Array(a1,a2,a3,a4,a5,a6,a7,LXM,a8)
Digits(2)=Array(a9,a10,a11,a12,a13,a14,a15,LXM,a16)
Digits(3)=Array(a17,a18,a19,a20,a21,a22,a23,LXM,a24)
Digits(4)=Array(a25,a26,a27,a28,a29,a30,a31,LXM,a32)
Digits(5)=Array(a33,a34,a35,a36,a37,a38,a39,LXM,a40)

Digits(6)=Array(a41,a42,a43,a44,a45,a46,a47,LXM,a48)
Digits(7)=Array(a009,a010,a011,a012,a013,a014,a015,LXM,a016)
Digits(8)=Array(a49,a50,a51,a52,a53,a54,a55,LXM,a56)
Digits(9)=Array(a57,a58,a59,a60,a61,a62,a63,LXM,a64)
Digits(10)=Array(a65,a66,a67,a68,a69,a70,a71,LXM,a72)
Digits(11)=Array(a73,a74,a75,a76,a77,a78,a79,LXM,a80)

Digits(12)=Array(a81,a82,a83,a84,a85,a86,a87,LXM,a88)
Digits(13)=Array(a89,a90,a91,a92,a93,a94,a95,LXM,a96)
Digits(14)=Array(a017,a018,a019,a020,a021,a022,a023,LXM,a024)
Digits(15)=Array(a97,a98,a99,a100,a101,a102,a103,LXM,a104)
Digits(16)=Array(a105,a106,a107,a108,a109,a110,a111,LXM,a112)
Digits(17)=Array(a113,a114,a115,a116,a117,a118,a119,LXM,a120)

Digits(18)=Array(a121,a122,a123,a124,a125,a126,a127,LXM,a128)
Digits(19)=Array(a129,a130,a131,a132,a133,a134,a135,LXM,a136)
Digits(20)=Array(a137,a138,a139,a140,a141,a142,a143,LXM,a144)
Digits(21)=Array(a025,a026,a027,a028,a029,a030,a031,LXM,a032)
Digits(22)=Array(a145,a146,a147,a148,a149,a150,a151,LXM,a152)
Digits(23)=Array(a153,a154,a155,a156,a157,a158,a159,LXM,a160)

Digits(24)=Array(a161,a162,a163,a164,a165,a166,a167,LXM,a168)
Digits(25)=Array(a169,a170,a171,a172,a173,a174,a175,LXM,a176)
Digits(26)=Array(a177,a178,a179,a180,a181,a182,a183,LXM,a184)
Digits(27)=Array(a185,a186,a187,a188,a189,a190,a191,LXM,a192)

'Ball in Play and Credit displays

Digits(28)=Array(f00,f01,f02,f03,f04,f05,f06,LXM)
Digits(29)=Array(f10,f11,f12,f13,f14,f15,f16,LXM)
Digits(30)=Array(e00,e01,e02,e03,e04,e05,e06,LXM)
Digits(31)=Array(e10,e11,e12,e13,e14,e15,e16,LXM)

Sub DisplayTimer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 32) then
				For Each obj In Digits(num)
					If cab_mode = 1 OR VR_Room =1 Then
						obj.intensity=0
					Else
						obj.intensity= 1.5
					End If
					If chg And 1 Then obj.State = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
            Else
                For Each obj In Digits(num)
                    If chg And 1 Then obj.State = stat And 1
                    chg = chg \ 2:stat = stat \ 2
                Next
			end If
		next
	end if
End Sub


'******************************************************
' VR Backglass Digital Display
'******************************************************

Dim xoff,yoff1, yoff2, yoff3, yoff4, yoff5, zoff,xrot,zscale, xcen,ycen

Sub Setup_Backglass()

	xoff = -20
	yoff1 = 35 ' this is where you adjust the forward/backward position for player 1 score
	yoff2 = 35 ' this is where you adjust the forward/backward position for player 2 score
	yoff3 = 35 ' this is where you adjust the forward/backward position for player 3 score
	yoff4 = 35 ' this is where you adjust the forward/backward position for player 4 score
	yoff5 = 35 ' this is where you adjust the forward/backward position for credits and ball in play
	zoff = 638
	xrot = -90

	CenterVRDigits()

End Sub

Sub CenterVRDigits()

Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen, ix, xx, yy, xobj

	xoff =480
	yoff = 30
	zoff = 180
	xrot = -85

	zscale = 0.0000001

	xcen =(1032 /2) - (74 / 2)
	ycen = (1020 /2 ) + (194 /2)

	for ix = 0 to 31
		For Each xobj In VRDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

End Sub



'*****************************************************************************************************
' VR LED Displays
'*****************************************************************************************************

dim DisplayColor
DisplayColor =  RGB(1,48,135)


Dim VRDigits(32)

VRDigits(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6, n1, LED1x8)
VRDigits(1) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6, n1, LED2x8)
VRDigits(2) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6, n1, LED3x8)
VRDigits(3) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6, n1, LED4x8)
VRDigits(4) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6, n1, LED5x8)
VRDigits(5) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6, n1, LED6x8)
VRDigits(6) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6, n1, LED7x8)

VRDigits(7) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6, n1, LED8x8)
VRDigits(8) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6, n1, LED9x8)
VRDigits(9) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6, n1, LED10x8)
VRDigits(10) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6, n1, LED11x8)
VRDigits(11) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6, n1, LED12x8)
VRDigits(12) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6, n1, LED13x8)
VRDigits(13) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6, n1, LED14x8)

VRDigits(14) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006, n1, LED1x008)
VRDigits(15) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106, n1, LED1x108)
VRDigits(16) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206, n1, LED1x208)
VRDigits(17) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306, n1, LED1x308)
VRDigits(18) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406, n1, LED1x408)
VRDigits(19) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506, n1, LED1x508)
VRDigits(20) = Array(LED1x600,LED1x601,LED1x602,LED1x603,LED1x604,LED1x605,LED1x606, n1, LED1x608)

VRDigits(21) = Array(LED2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006, n1, LED2x008)
VRDigits(22) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106, n1, LED2x108)
VRDigits(23) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206, n1, LED2x208)
VRDigits(24) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306, n1, LED2x308)
VRDigits(25) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406, n1, LED2x408)
VRDigits(26) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506, n1, LED2x508)
VRDigits(27) = Array(LED2x600,LED2x601,LED2x602,LED2x603,LED2x604,LED2x605,LED2x606, n1, LED2x608)

'Ball in Play and Credit displays

VRDigits(28) = Array(LEDax300,LEDax301,LEDax302,LEDax303,LEDax304,LEDax305,LEDax306, n1)
VRDigits(29) = Array(LEDbx400,LEDbx401,LEDbx402,LEDbx403,LEDbx404,LEDbx405,LEDbx406, n1)
VRDigits(30) = Array(LEDcx500,LEDcx501,LEDcx502,LEDcx503,LEDcx504,LEDcx505,LEDcx506, n1)
VRDigits(31) = Array(LEDdx600,LEDdx601,LEDdx602,LEDdx603,LEDdx604,LEDdx605,LEDdx606, n1)


Sub VRDisplayTimer
	Dim ii, jj, obj, b, x
	Dim ChgLED,num, chg, stat
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED) Then
			For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				if num < 32 Then
					For Each obj In VRDigits(num)
	 '                  If chg And 1 Then obj.visible=stat And 1    'if you use the object color for off; turn the display object visible to not visible on the playfield, and uncomment this line out.
						If chg And 1 Then FadeDisplay obj, stat And 1	
						chg=chg\2 : stat=stat\2
					Next
				Else
					For Each obj In VRDigits(num)
						If chg And 1 Then obj.State = stat And 1
						chg = chg \ 2:stat = stat \ 2
					Next
				End If
			Next
		End If
End Sub


Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
		Object.Opacity = 5
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 1
	End If
End Sub

Sub InitDigits()
	dim tmp, x, xobj
	for x = 0 to uBound(VRDigits)
		if IsArray(VRDigits(x) ) then
			For each xobj in VRDigits(x)
				xobj.height = xobj.height + 0
				FadeDisplay xobj, 0
			next
		end If
	Next
End Sub



If VR_Room=1 Then
	InitDigits
End If



'******************************************************
' Ball brightness code
'******************************************************



'****************************************************************
' Ball option setup
'****************************************************************

Dim CustomBallImage(14), CustomBallLogoMode(14), CustomBallDecal(14), CustomBallRS(14), CustomBulbIntensity(14)

' dark
CustomBallImage(0) = 		"ball-dark"
CustomBallLogoMode(0) = 	False
CustomBallDecal(0) = 		"JPBall-Scratches"
CustomBulbIntensity(0) = 	1.0
CustomBallRS(0) = 			0.4

' not as dark
CustomBallImage(1) = 		"ball_HDR"
CustomBallLogoMode(1) = 	False
CustomBallDecal(1) = 		"Scratches"
CustomBulbIntensity(1) = 	1.0
CustomBallRS(1) = 			0.4

' bright
CustomBallImage(2) = 		"ball-light-hf"
CustomBallLogoMode(2) = 	False
CustomBallDecal(2) = 		"scratchedmorelight"
CustomBulbIntensity(2) = 	1.0
CustomBallRS(2) = 			0.4

' brightest
CustomBallImage(3) = 		"ball-lighter-hf"
CustomBallLogoMode(3) = 	False
CustomBallDecal(3) = 		"scratchedmorelight"
CustomBulbIntensity(3) = 	1.0
CustomBallRS(3) = 			0.4

' warm
CustomBallImage(4) = 		"ball-hf-warm"
CustomBallLogoMode(4) = 	False
CustomBallDecal(4) = 		"scratchedmorelight"
CustomBulbIntensity(4) = 	1.0
CustomBallRS(4) = 			0.4

' MrBallDark
CustomBallImage(5) = 		"ball_MRBallDark"
CustomBallLogoMode(5) = 	False
CustomBallDecal(5) = 		"ball_scratches"
CustomBulbIntensity(5) = 	1.0
CustomBallRS(5) = 			0.4

' DarthVito Ball
CustomBallImage(6) = 		"ball"
CustomBallLogoMode(6) = 	False
CustomBallDecal(6) = 		"ball_scratches"
CustomBulbIntensity(6) = 	1.0
CustomBallRS(6) = 			0.4

' DarthVito HDR1
CustomBallImage(7) = 		"ball_HDR"
CustomBallLogoMode(7) = 	False
CustomBallDecal(7) = 		"ball_scratches"
CustomBulbIntensity(7) = 	1.0
CustomBallRS(7) = 			0.4

' DarthVito HDR Bright
CustomBallImage(8) = 		"ball_HDR_bright"
CustomBallLogoMode(8) = 	False
CustomBallDecal(8) = 		"ball_scratches"
CustomBulbIntensity(8) = 	1.0
CustomBallRS(8) = 			0.4

' DarthVito HDR2
CustomBallImage(9) = 		"ball_HDR3b"
CustomBallLogoMode(9) = 	False
CustomBallDecal(9) = 		"ball_scratches"
CustomBulbIntensity(9) = 	1.0
CustomBallRS(9) = 			0.4

' DarthVito ballHDR3
CustomBallImage(10) = 		"ball_HDR3b2"
CustomBallLogoMode(10) = 	False
CustomBallDecal(10) = 		"ball_scratches"
CustomBulbIntensity(10) = 	1.0
CustomBallRS(10) = 			0.4

' DarthVito ballHDR4
CustomBallImage(11) = 		"ball_HDR3b3"
CustomBallLogoMode(11) = 	False
CustomBallDecal(11) = 		"ball_scratches"
CustomBulbIntensity(11) = 	1.0
CustomBallRS(11) = 			0.4

' DarthVito ballHDRdark
CustomBallImage(12) = 		"ball_HDR3dark"
CustomBallLogoMode(12) = 	False
CustomBallDecal(12) = 		"ball_scratches"
CustomBulbIntensity(12) = 	1.0
CustomBallRS(12) = 			0.4

' Borg Ball
CustomBallImage(13) = 		"BorgBall"
CustomBallLogoMode(13) = 	False
CustomBallDecal(13) = 		"ball_scratches"
CustomBulbIntensity(13) = 	1.0
CustomBallRS(13) = 			0.4

' SteelBall2
CustomBallImage(14) = 		"SteelBall2"
CustomBallLogoMode(14) = 	False
CustomBallDecal(14) = 		"ball_scratches"
CustomBulbIntensity(14) = 	1.0
CustomBallRS(14) = 			0.4


Sub ChangeBall(ballnr)
	Dim ii
	table1.BallDecalMode = CustomBallLogoMode(ballnr)
	table1.BallFrontDecal = CustomBallDecal(ballnr)
	table1.DefaultBulbIntensityScale = CustomBulbIntensity(ballnr)
	table1.BallImage = CustomBallImage(ballnr)

	For ii = 0 to UBound(gBOT) 
		gBOT(ii).Image = CustomBallImage(ballnr)
		gBOT(ii).FrontDecal = CustomBallDecal(ballnr)
		gBOT(ii).PlayfieldReflectionScale = CustomBallRS(ballnr)
	Next
End Sub

'******************************************************
' Options
'******************************************************

Sub SetupOptions

Dim C, Tscrewcap, TWallColor, TFlipper, TTents, TMoP

'  ROM Volume Settings
    With Controller
        .Games(cGameName).Settings.Value("volume") = ROMVolume
	End With

If Tourney = 1 Then

	TFlipper = 2
	Tscrewcap = 4	
	TTents = 1
	TWallColor = 0	
	TMoP = 1

Else

	TFlipper = FlipperColor
	Tscrewcap = ScrewCapColor
	TWallColor = SideWallColor
	TTents = LaneColor
	TMoP = insertmop

End If

' Plastic Edge Color

	If Tourney = 1 Then
		PlasProt = 1
	Else
		If PlasticColor = 6 Then
			PlasProt = RndInt(0,5)
		Else
			PlasProt = PlasticColor
		End if
	End If

	SetAcrylicColor PlasProt

' Flipper Color

	If TFlipper = 1 Then
		rflipr.material = "Rubber Yellow1"
		lflipr.material = "Rubber Yellow2"
	Elseif TFlipper = 2 Then
		rflipr.material = "Rubber Black1"
		lflipr.material = "Rubber Black2"
	Elseif TFlipper = 3 Then
		rflipr.material = "Rubber Orange1"
		lflipr.material = "Rubber Orange2"
	Else
		rflipr.material = "Red Rubber1"
		lflipr.material = "Red Rubber2"
	End If

' Lane Tent Color
	If Ttents = 1 Then
		For each C in GILaneGuides
			C.material = "Plastic Lane Guides Yellow"
		Next
		For each C in GILaneTents
			C.material = "Plastic Lane Guides Yellow"
		Next
	Else
		For each C in GILaneGuides
			C.material = "Plastic Lane Guides Red"
		Next
		For each C in GILaneTents
			C.material = "Plastic Lane Guides Red"
		Next
	End If

' Screw Cap Color
	If Tscrewcap  = 1  Then
		For each C in GIScrewcaps
			C.material = "Metal Lamp Black"
		Next
	Elseif Tscrewcap  = 2 Then
		For each C in GIScrewcaps
			C.material = "Metal Lamp Bronze"
		Next
	Elseif Tscrewcap  = 3 Then
		For each C in GIScrewcaps
			C.material = "Metal Lamp Silver"
		Next
	Elseif Tscrewcap  = 4 Then
		For each C in GIScrewcaps
			C.material = "Metal Lamp BlueGreen"
		Next
	Elseif Tscrewcap  = 5 Then
		For each C in GIScrewcaps
			C.material = "Metal Lamp Gold"
		Next
	Else
		For each C in GIScrewCaps
			C.material = "Metal Lamp White"
		Next
	End If

' Interior Walls Color
	If TWallColor  = 1  Then
		For each C in WallColor
			C.image = "sidewood_h"
			C.sideimage = "sidewood_h"
		Next
		For each C in VLMSWLight: C.visible = 1: Next
		For each C in VLMSWDark: C.visible = 0: Next
	Else
		For each C in WallColor
			C.image = "sidewood_h-dark"
			C.sideimage = "sidewood_h-dark"
		Next
		For each C in VLMSWLight: C.visible = 0: Next
		For each C in VLMSWDark: C.visible = 1: Next
	End If


' Mother of Pearl Inserts
	If TMoP = 0 Then
		For each C in SolidOn: C.image = "SolidOn": Next
		For each C in SolidOff: C.image = "SolidOff": Next
	Else
		p13.image = "SolidOn-MP1"
		p14.image = "SolidOn-MP2"
		p15.image = "SolidOn-MP3"
		p16.image = "SolidOn-MP4"
		p17.image = "SolidOn-MP5"
		p18.image = "SolidOn-MP6"
		p19.image = "SolidOn-MP7"
		p20.image = "SolidOn-MP1"
		p21.image = "SolidOn-MP2"
		p22.image = "SolidOn-MP3"
		p142.image = "SolidOn-MP4"
		p152.image = "SolidOn-MP5"
		p162.image = "SolidOn-MP6"
		p23.image = "SolidOn-MP7"
		p132.image = "SolidOn-MP1"
		p212.image = "SolidOn-MP2"
		p202.image = "SolidOn-MP3"
		p192.image = "SolidOn-MP4"
		p182.image = "SolidOn-MP5"
		p172.image = "SolidOn-MP6"
		p43.image = "SolidOn-MP7"
		p33.image = "SolidOn-MP1"
		p35.image = "SolidOn-MP2"
		p36.image = "SolidOn-MP3"
		p47.image = "SolidOn-MP4"
		p40.image = "SolidOn-MP5"
		p41.image = "SolidOn-MP6"
		p42.image = "SolidOn-MP7"
		p3.image = "SolidOff-MP5"
		p13off.image = "SolidOff-MP1"
		p14off.image = "SolidOff-MP2"
		p15off.image = "SolidOff-MP3"
		p16off.image = "SolidOff-MP4"
		p17off.image = "SolidOff-MP5"
		p18off.image = "SolidOff-MP6"
		p19off.image = "SolidOff-MP7"
		p20off.image = "SolidOff-MP1"
		p21off.image = "SolidOff-MP2"
		p22off.image = "SolidOff-MP3"
		p142off.image = "SolidOff-MP4"
		p152off.image = "SolidOff-MP5"
		p162off.image = "SolidOff-MP6"
		p23off.image = "SolidOff-MP7"
		p132off.image = "SolidOff-MP1"
		p212off.image = "SolidOff-MP2"
		p202off.image = "SolidOff-MP3"
		p192off.image = "SolidOff-MP4"
		p182off.image = "SolidOff-MP5"
		p172off.image = "SolidOff-MP6"
		p43off.image = "SolidOff-MP7"
		p33off.image = "SolidOff-MP1"
		p35off.image = "SolidOff-MP2"
		p36off.image = "SolidOff-MP3"
		p47off.image = "SolidOff-MP4"
		p40off.image = "SolidOff-MP5"
		p41off.image = "SolidOff-MP6"
		p42off.image = "SolidOff-MP7"
		p3off.image = "SolidOff-MP5"
	End If


SetupVRRoom

End Sub


'******************************************************
' DIP Switches
'******************************************************


'Finding an individual dip state based on scapino's Strikes and spares dip code - from unclewillys pinball pool, added another section to get high score award to set replay cards

Dim TheDips(32)
Sub FindDips
  Dim DipsNumber
  DipsNumber = Controller.Dip(1)
  TheDips(16) = Int(DipsNumber/128)
  If TheDips(16) = 1 then DipsNumber = DipsNumber - 128 end if
  TheDips(15) = Int(DipsNumber/64)
  If TheDips(15) = 1 then DipsNumber = DipsNumber - 64 end if
  TheDips(14) = Int(DipsNumber/32)
  If TheDips(14) = 1 then DipsNumber = DipsNumber - 32 end if
  TheDips(13) = Int(DipsNumber/16)
  If TheDips(13) = 1 then DipsNumber = DipsNumber - 16 end if
  TheDips(12) = Int(DipsNumber/8)
  If TheDips(12) = 1 then DipsNumber = DipsNumber - 8 end if
  TheDips(11) = Int(DipsNumber/4)
  If TheDips(11) = 1 then DipsNumber = DipsNumber - 4 end if
  TheDips(10) = Int(DipsNumber/2)
  If TheDips(10) = 1 then DipsNumber = DipsNumber - 2 end if
  TheDips(9) = Int(DipsNumber)
  DipsNumber = Controller.Dip(2)
  TheDips(24) = Int(DipsNumber/128)
  If TheDips(24) = 1 then DipsNumber = DipsNumber - 128 end if
  TheDips(23) = Int(DipsNumber/64)
  If TheDips(23) = 1 then DipsNumber = DipsNumber - 64 end if
  TheDips(22) = Int(DipsNumber/32)
  If TheDips(22) = 1 then DipsNumber = DipsNumber - 32 end if
  TheDips(21) = Int(DipsNumber/16)
  If TheDips(21) = 1 then DipsNumber = DipsNumber - 16 end if
  TheDips(20) = Int(DipsNumber/8)
  If TheDips(20) = 1 then DipsNumber = DipsNumber - 8 end if
  TheDips(19) = Int(DipsNumber/4)
  If TheDips(19) = 1 then DipsNumber = DipsNumber - 4 end if
  TheDips(18) = Int(DipsNumber/2)
  If TheDips(18) = 1 then DipsNumber = DipsNumber - 2 end if
  TheDips(17) = Int(DipsNumber)
  DipsNumber = Controller.Dip(3)
  TheDips(32) = Int(DipsNumber/128)
  If TheDips(32) = 1 then DipsNumber = DipsNumber - 128 end if
  TheDips(31) = Int(DipsNumber/64)
  If TheDips(31) = 1 then DipsNumber = DipsNumber - 64 end if
  TheDips(30) = Int(DipsNumber/32)
  If TheDips(30) = 1 then DipsNumber = DipsNumber - 32 end if
  TheDips(29) = Int(DipsNumber/16)
  If TheDips(29) = 1 then DipsNumber = DipsNumber - 16 end if
  TheDips(28) = Int(DipsNumber/8)
  If TheDips(28) = 1 then DipsNumber = DipsNumber - 8 end if
  TheDips(27) = Int(DipsNumber/4)
  If TheDips(27) = 1 then DipsNumber = DipsNumber - 4 end if
  TheDips(26) = Int(DipsNumber/2)
  If TheDips(26) = 1 then DipsNumber = DipsNumber - 2 end if
  TheDips(25) = Int(DipsNumber)
  DipsTimer.Enabled=1
End Sub


Sub DipsTimer_Timer()
  dim match
  match = TheDips(26)
  if match = 1 Then
		If DesktopMode = True or VR_Room = 1 Then
			NTM=1
		else
			NTM=0
		end if
  end If
  DipsTimer.enabled=0
End Sub

Sub editDips
	if SetDIPSwitches = 1 Then
		Dim vpmDips : Set vpmDips = New cvpmDips
		With vpmDips
		.AddForm 700,400,"System 80A with Sound only (S)board - DIP switches"
		.AddFrame 0,0,190,"Maximum credits",49152,Array("8 credits",0,"10 credits",32768,"15 credits",&H00004000,"25 credits",49152)'dip 15&16
		.AddFrame 0,76,190,"Coin chute 1 and 2 control",&H00002000,Array("seperate",0,"same",&H00002000)'dip 14
		.AddFrame 0,122,190,"3rd coin chute credits control",&H20000000,Array("no effect",0,"add 9",&H20000000)'dip 30
		.AddFrame 205,0,190,"High score to date awards",&H00C00000,Array("not displayed and no award",0,"displayed and no award",&H00800000,"displayed and 2 credits",&H00400000,"displayed and 3 credits",&H00C00000)'dip 23&24
		.AddFrame 205,76,190,"Game mode",&H10000000,Array("replay",0,"extra ball",&H10000000)'dip 29
		.AddFrame 205,122,190,"Playfield special",&H00200000,Array("replay",0,"extra ball",&H00200000)'dip 22
		.AddFrame 205,168,190,"Replay limit",&H04000000,Array("no limit",0,"one per game",&H04000000)'dip 27
		.AddFrame 205,214,190,"Balls per game",&H01000000,Array("5 balls",0,"3 balls",&H01000000)'dip 25
		.AddFrame 205,260,190,"Novelty mode",&H08000000,Array("normal game mode",0,"50K per special/extra ball",&H08000000)'dip 28
		.AddChk 0,170,190,Array("Match feature",&H02000000)'dip 26
		.AddChk 0,185,190,Array("Background sound",&H40000000)'dip 31
		.AddLabel 50,310,300,20,"After hitting OK, press F3 to reset game with new settings."
		End With
	Dim extra
		extra = Controller.Dip(4) + Controller.Dip(5)*256
		extra = vpmDips.ViewDipsExtra(extra)
		Controller.Dip(4) = extra And 255
		Controller.Dip(5) = (extra And 65280)\256 And 255
	End If
End Sub
Set vpmShowDips = GetRef("editDips")

'  *** This is the default dips settings ***

	'1-5 = 0 left coin chute
	'6-8 = 0 spares
	'9-13 = 0 right coin chute
	'14 = 1 coin chute the same
	'15 = 1 max credits = 25
	'16 = 1 max credits =25
	'17-21 = 0 : center coin chute
	'22 = 1 in normal mode:  0 for tourney... this is extra ball in playfield special
	'23 = 1 HSTD reward - displayed and 3 replays
	'24 = 1 HSTD reward - displayed and 3 replays
	'25 = 1: 3 Balls per game... 0 = 5 balls
	'26 = 1: Match enabled
	'27 = 0: replay limit = unlimited
	'28 = 0: Novelty for normal.... 1 for tourney
	'29 = 0: Game mode.  0 = replay ... 1 = extra ball.
	'30 = 0 : this is 3rd coin chute no effect
	'31	= 1 : background sound
	'32 = 0 : spare



Sub SetDefaultDips
	if Tourney = 1 Then
		Controller.Dip(0) = 0		'8-1 = 00000000
		Controller.Dip(1) = 224		'16-9 = 11100000
		Controller.Dip(2) = 192		'24-17 = 11000000  'tourney mode turns off playfield special of extra ball
		Controller.Dip(3) = 75		'32-25 = 01001011 'tourney mode turns to novelty mode(28)
		Controller.Dip(4) = 0
		Controller.Dip(5) = 0
	Else
		If SetDIPSwitches = 0 Then
			Controller.Dip(0) = 0		'8-1 = 00000000
			Controller.Dip(1) = 224		'16-9 = 11100000
			Controller.Dip(2) = 224		'24-17 = 11100000
			Controller.Dip(3) = 67		'32-25 = 01000011
			Controller.Dip(4) = 0
			Controller.Dip(5) = 0
		End If
	End If
End Sub


'******************************************************
' ZVRR
'******************************************************

Sub SetupVRRoom

Dim VRThings

if VR_Room = 0 and cab_mode = 0 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in VRBackglass:VRThings.visible = 0:Next
	for each VRThings in DTBGLights:VRThings.visible = 1: Next
	for each VRThings in BGBrackets:VRThings.visible = 0:Next
	for each VRThings in BGBrackets:VRThings.sidevisible = 0:Next
	for each VRThings in BGScrews:VRThings.visible = 0:Next
	TopRailGot.visible = 1
	OuterPrimBlack_DT.z = -318
	OuterPrimBlack_DT.size_y = 165
	FrontWall.sidevisible = 1
	L_DT_1.State = 1
	L_DT_2.State = 1
	L_DT_2a.State = 1

Elseif VR_Room = 0 and cab_mode = 1 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in VRBackglass:VRThings.visible = 0:Next
	for each VRThings in DTBackglass:VRThings.visible = 0: Next
	for each VRThings in DTBGLights:VRThings.visible = 0: Next
	for each VRThings in TopHardware:VRThings.visible = 0:Next
	for each VRThings in BGBrackets:VRThings.visible = 0:Next
	for each VRThings in BGBrackets:VRThings.sidevisible = 0:Next
	for each VRThings in BGScrews:VRThings.visible = 0:Next
	TopRailGot.visible = 0
		if cabsideblades = 0 Then
			OuterPrimBlack_DT.z = -180
			OuterPrimBlack_DT.size_y = 220
		Else
			OuterPrimBlack_DT.z = -318
			OuterPrimBlack_DT.size_y = 165
		End If
	FrontWall.sidevisible = 0
	L_DT_1.Visible = 0
	L_DT_1.State = 0
	L_DT_2.Visible = 0
	L_DT_2.State = 0
	L_DT_2a.Visible = 0
	L_DT_2a.State = 0

Else
	for each VRThings in VRStuff:VRThings.visible = 1:Next
	for each VRThings in VRClock:VRThings.visible = WallClock:Next
	for each VRThings in DTBackglass:VRThings.visible = 0: Next
	for each VRThings in DTBGLights:VRThings.visible = 0: Next
	for each VRThings in BGBrackets:VRThings.visible = 1:Next
	for each VRThings in BGBrackets:VRThings.sidevisible = 1:Next
	for each VRThings in BGScrews:VRThings.visible = 1:Next
	TopRailGot.visible = 1
	OuterPrimBlack_DT.z = -318
	OuterPrimBlack_DT.size_y = 165
	FrontWall.sidevisible = 1
	L_DT_1.Visible = 0
	L_DT_1.State = 0
	L_DT_2.Visible = 0
	L_DT_2.State = 0
	L_DT_2a.Visible = 0
	L_DT_2a.State = 0

'Custom Walls, Floor, and Roof
	if CustomWalls = 1 Then
		VR_Wall_Left.image = "VR_Wall_Left"
		VR_Wall_Right.image = "VR_Wall_Right"
		VR_Floor.image = "VR_Floor"
		VR_Roof.image = "VR_Roof"
	Elseif CustomWalls = 2 Then
		VR_Wall_Left.image = "VR_Wall_Left_DV2"
		VR_Wall_Right.image = "VR_Wall_Right_DV2"
		VR_Floor.image = "VR_Room Floor"
		VR_Roof.image = "VR_Room Roof 2"
	Elseif CustomWalls = 3 Then
		VR_Wall_Left.image = "VR_Wall_Left_DV1"
		VR_Wall_Right.image = "VR_Wall_Right_DV1"
		VR_Floor.image = "VR_Room Floor"
		VR_Roof.image = "VR_Room Roof 2"
	Elseif CustomWalls = 4 Then
		VR_Wall_Left.image = "VR_Wall_Left_DV3"
		VR_Wall_Right.image = "VR_Wall_Right_DV3"
		VR_Floor.image = "VR_Room Floor"
		VR_Roof.image = "VR_Room Roof 2"
	Else
		VR_Wall_Left.image = "wallpaper left"
		VR_Wall_Right.image = "wallpaper_right"
		VR_Floor.image = "FloorCompleteMap2"
		VR_Roof.image = "Light Gray"
	end if

	If poster = 1 Then
		VRposter.visible = 1
	Else
		VRposter.visible = 0
	End If

	If poster2 = 1 Then
		VRposter2.visible = 1
	Else
		VRposter2.visible = 0
	End If

	if undercab = 1 Then
		UndercabLight.state = 1
	Else
		UndercabLight.state = 0
	End If

End If

End Sub


'******************************************************
' VR CLOCK
'******************************************************

Dim CurrentMinute ' for VR clock

Sub ClockTimer_Timer()

    'ClockHands Below *********************************************************
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
	Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())

End Sub


'******************************************************
' LAMP CALLBACK for the bckglass flasher lamps
'******************************************************

if DesktopMode = True Then
	Set LampCallback = GetRef("UpdateDTLamps")
End If

if VR_Room = 1 Then
	Set LampCallback = GetRef("UpdateVRLamps")
End If

If cab_mode = 1 Then
	Set LampCallback=GetRef("UpdateCabLamps")
End If

Sub UpdateDTLamps()

' Ball Release Routine
    If Controller.Lamp(12) = True And BallRelease.BallCntOver = 1 Then 'Kick ball to plunger lane
		BallRelease.kick 60, 12
		RandomSoundBallRelease BallRelease
	End If

    If Controller.Lamp(11) = False Then 'Game Over triggers match and BIP Then
		If GameInPlay = 1 Then 
			BIPReel.setValue(1)
		End If
			MatchReel.setValue(0)
			GameOverReel.setValue(0)
    Else
			BIPReel.setValue(0)
			GameOverReel.setValue(1)

		If GameInPlay = 0 and GameInPlayFirstTime = 1 and NTM = 1 Then
			MatchReel.setValue(1)
		End If
    End If

	If Controller.Lamp(10) = False Then: HighScoreReel.setValue(0): 		Else: HighScoreReel.setValue(1) 'High Score

	If GameInPlayFirstTime = 1 Then
		If Controller.Lamp(3)  = False Then: ShootAgainReel.setValue(0):	Else: ShootAgainReel.setValue(1) 'Shoot Again
		If Controller.Lamp(8)  = False Then: TiltReel.setValue(0):			Else: TiltReel.setValue(1)
	End If

End Sub


Sub UpdateVRLamps()

' Ball Release Routine
    If Controller.Lamp(12) = true And BallRelease.BallCntOver = 1 Then 'Kick ball to plunger lane
		BallRelease.kick 60, 12
		RandomSoundBallRelease BallRelease
	End If

	If Controller.Lamp(10) = 0 Then: VR_HighScore.visible=0:	Else: VR_HighScore.visible=1
	If Controller.Lamp(11) = 0 Then: VR_GameOver.visible=0:		Else: VR_GameOver.visible=1

	If GameInPlayFirstTime = 1 Then
		If Controller.Lamp(3) = False Then:	VR_ShootAgain.visible=0:	Else: VR_ShootAgain.visible=1
		If Controller.Lamp(8) = False Then: VR_Tilt.visible=0:			Else: VR_Tilt.visible=1
	End If

End Sub

Sub UpdateCabLamps()

' Ball Release Routine
    If Controller.Lamp(12) = True And BallRelease.BallCntOver = 1 Then 'Kick ball to plunger lane
		BallRelease.kick 60, 12
		RandomSoundBallRelease BallRelease
	End If

End Sub


'*******************************
'Acrylic plastics - iaakki acrylic redbone
'*******************************


Sub SetMaterialColor(name, new_color)
    Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    base = new_color
    UpdateMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub

Sub SetMaterialGlossyColor(name, new_color)
    Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    glossy = new_color
    UpdateMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub

Sub SetMaterialCKColor(name, new_color)
    Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    clearcoat = new_color
    UpdateMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub


DIM ProtCol

sub SetAcrylicColor(ColorOption)

	for each Protcol in Acrylics:Protcol.SideVisible = 1:Next
	for each Protcol in AcrylicsLEDMod:Protcol.SideVisible = 0:Next

	if PlasProt = 0 Then
		for each Protcol in AcrylicsLEDMod:Protcol.visible = 0:Next
		for each Protcol in Acrylics:Protcol.visible = 1:Next
	Else
		for each Protcol in AcrylicsLEDMod:Protcol.visible = 1:Next
		for each Protcol in Acrylics:Protcol.visible = 0:Next
	end If

	if Not ColorOption = 5 then 
		RGBAcrylTimer.enabled = false
	End If

	if Not ColorOption = 0 then 
		for each Protcol in Acrylics:Protcol.blenddisablelighting = 1:Next
		for each Protcol in Acrylics:Protcol.Topmaterial = "AcrylicBLTops":Next
		for each Protcol in Acrylics:Protcol.Sidematerial = "AcrylicBLSides":Next
		for each Protcol in Acrylics:Protcol.image = "Colour_Trans_Grey":Next
		for each Protcol in Acrylics:Protcol.Sideimage = "Colour_Cream_Trans":Next
	End If

	If ColorOption = 0 Then    'clear
		for each Protcol in Acrylics:Protcol.blenddisablelighting = 0:Next
		for each Protcol in Acrylics:Protcol.Topmaterial = "acrylic top":Next
		for each Protcol in Acrylics:Protcol.Sidematerial = "acrylic side":Next
		for each Protcol in Acrylics:Protcol.image = "":Next
		for each Protcol in Acrylics:Protcol.Sideimage = "":Next
    End If

	If ColorOption = 1 Then    'orangered
        SetMaterialColor "AcrylicBLSides",GIlvlRGB(RGB(255,8,0))
        SetMaterialColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
        SetMaterialGlossyColor "AcrylicBLSides",GIlvlRGB(RGB(255,8,0))
        SetMaterialGlossyColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
        SetMaterialCKColor "AcrylicBLSides",GIlvlRGB(RGB(255,8,0))
        SetMaterialCKColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
    End If

	If ColorOption = 2 Then    'yellow
        SetMaterialColor "AcrylicBLSides",GIlvlRGB(RGB(255,128,0))
        SetMaterialColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
        SetMaterialGlossyColor "AcrylicBLSides",GIlvlRGB(RGB(255,128,0))
        SetMaterialGlossyColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
        SetMaterialCKColor "AcrylicBLSides",GIlvlRGB(RGB(255,128,0))
        SetMaterialCKColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
    End If

	If ColorOption = 3 Then    'blue
        SetMaterialColor "AcrylicBLSides",GIlvlRGB(RGB(15,65,100))
        SetMaterialColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
        SetMaterialGlossyColor "AcrylicBLSides",GIlvlRGB(RGB(15,65,100))
        SetMaterialGlossyColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
        SetMaterialCKColor "AcrylicBLSides",GIlvlRGB(RGB(15,65,100))
        SetMaterialCKColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
    End If

	If ColorOption = 4 Then	'user defined
		dim currentColor : currentColor = GIlvlRGB(UserAcrylColor)
		SetMaterialColor "AcrylicBLSides",GIlvlRGB(UserAcrylColor)
		SetMaterialColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
        SetMaterialGlossyColor "AcrylicBLSides",GIlvlRGB(UserAcrylColor)
        SetMaterialGlossyColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
		SetMaterialCKColor "AcrylicBLSides",GIlvlRGB(UserAcrylColor)
		SetMaterialCKColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
	end if

	If ColorOption = 5 Then	'rainbow
		RGBAcrylTimer.enabled = true
	end if

end sub


Function GIlvlRGB(color)
	Dim red, green, blue, dimFactor, dimEquation

	dimFactor = 5
'	dimEquation = ((1+((dimFactor-1)*gilvl))/dimFactor) 'decided to leave LED eges always on if option chosen... rather than turn on and off with each GI.
	dimEquation = ((1+(dimFactor-1))/dimFactor)

	red = color And &HFF
	green = (color \ &H100) And &HFF
	blue = (color \ &H10000) And &HFF
	
	GIlvlRGB = RGB(red*dimEquation, green*dimEquation, blue*dimEquation)
End Function



dim RGBAcrylHueAngle : RGBAcrylHueAngle = 0
sub RGBAcrylTimer_timer
	RGBAcrylHueAngle = RGBAcrylHueAngle + 1
	if RGBAcrylHueAngle > 359 then RGBAcrylHueAngle = 0

	RGBAcrylHue RGBAcrylHueAngle

	SetMaterialColor "AcrylicBLSides",RGBAcrylColor
	SetMaterialColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
	SetMaterialGlossyColor "AcrylicBLSides",RGBAcrylColor
	SetMaterialGlossyColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
	SetMaterialCKColor "AcrylicBLSides",RGBAcrylColor
	SetMaterialCKColor "AcrylicBLTops",GIlvlRGB(RGB(0,0,0))
end sub

dim RGBAcrylColor: RGBAcrylColor = RGB(240,20,20)

Sub RGBAcrylHue(HueDegrees)
    dim ColorR, ColorG, ColorB 
    dim outR, outG, outB

    'UW's from Hue angle
    dim U : U = cos(HueDegrees * PI/180)
    dim W : W = sin(HueDegrees * PI/180)
    'Initial color
    ColorR = 240
    ColorG = 20
    ColorB = 20
    'RGB values from Hue
    outR = Cint((.299+.701*U+.168*W)*ColorR + (.587-.587*U+.330*W)*ColorG + (.114-.114*U-.497*W) * ColorB) 
    outG = Cint((.299-.299*U-.328*W)*ColorR + (.587+.413*U+.035*W)*ColorG + (.114-.114*U+.292*W) * ColorB) 
    outB = Cint((.299-.300*U+1.25*W)*ColorR + (.587-.588*U-1.05*W)*ColorG + (.114+.886*U-.203*W) * ColorB)

    'limits as equations above may not be that precise
    if outR > 255 then outR = 255
    if outG > 255 then outG = 255
    if outB > 255 then outB = 255

    if outR < 0 then outR = 0
    if outG < 0 then outG = 0
    if outB < 0 then outB = 0

    RGBAcrylColor = GIlvlRGB(RGB(outR, outG, outB))
end sub


'*******************************
'end plastics REDBONE
'*******************************



'********************************************************************************************************************************
'******** Work done by UnclePaulie on Hybrid version 0.01 - 2.0 *********
' 		Added latest VPW standards, physics, GI baking, Lampz and 3D inserts, shadows, updated backglass for desktop, 
'		roth standup targets, standalone compatibility, sling corrections, 
'		optional DIP adjustments, updated flipper physics, VR and fully hybrid, Flupper style bumpers and lighting. 
'		Full details at the bottom of the script.  Redbone did the playfield, plastics, and other graphics.  
'		Original scans, image assets, cab artwork stencils all used with permission from Borgdog.
'********************************************************************************************************************************

'v0.01	Started with UnclePaulie's Super Orbit table for all physics, materials, VR structure 
'		Redbone created several images including the apron, target, instruction cards, plastics, and playfield.
'		Several assets from Borgdog's table; https://www.youtube.com/watch?v=NwFih3zu-4Y
'		Added all elements to table.
'		Got the trough working for two balls and gBOT.
'		Added simple ruleset to table info.
'		Added plunger groove and trigger for BIPL
'		Enabled the desktop backglass digits
'		Added some randomness to the plunger.
'		Added Flupper Bumpers, and adjusted color, dL, of tops, base, skirt, etc.
'		Tied GI and bumpers to Lampz
'		Added the gottlieb side rails from passion4pins.  
'		Updated target, side walls, plunger cover, and bumper images.
'		Added the Spinner
'		Added the top right Gate
'		Implemented sling corrections.
'		Added the metal side walls and tweaked to match real table rolloff
'		Implemented the two sensors
'		Cut holes in the playfield for inserts and cutouts.  Also added text overlay
'		Implemented new standup targets by Rothbaurer; and ensured standalone
'		Added the saucer and associated movements
'		Updated all elements to have some disablelighting and controlled by GI script
'		Added cutouts for triggers and other holes.
'		Added all bulb prims
'v0.02	Adjusted the table slope to 5.75
'		Fine tuned the saucer settings with some saucer angle scatter (0.35) 
'		Fine tuned the plungerfirespeed with a bit of randomness in script.
'		Added 3D primitive inserts.
'		Tweaked insert prim materials and lampz constants to get best look.
'		Red center insert is tied to GI, not a ROM lamp callout.
'		Created code to mask a corner condition of the tilt light coming on when multiball starts.  
'		Fixed blue material issue.
'v0.03	Changed to individual rubbers by outlanes, adjusted a couple post alignments
'		Implemented new recommendations for EOSTNew, moved FlipperActivate and FlipperDeactivate to the flipper Sol subs
'		Put plastic walls in.
'		Added inner peg screws
'		Added GI for plastics.
'		Added option for yellow lane rollovers and tents
'		Added option for yellow and black flippers
'		Added option for plastic edge colors and glowing/rainbow from Redbone and iaakki with orange/red, yellow, and blue variants
'		Got the backbox functional for DesktopMode, updated new picture, and added GI bg lights for Destktop.
' 		Tied the GI in correctly with the saucer and tilt to solenoid 11.
'		Added all the screws and screw caps. and tied to GI
'		Adjusted GI on the bumper cap ring down a touch.
'		Had an extra GI by the upper 2-way right gate.  Removed.
'		Added a gate prim and spring for the upper right 2-way gate, and added to GI.
'		Added new rollover switch prims and animated them.
'		Added options for Screw Cap Color
'		Added dynamic shadows, and code to stop shadows from going into the plunger lane via notrtlane and rtlanesense trigger.
'		Adjusted height of upper lane Rollovers
'		Changed the GI top to proper height.
'		Added physics walls
'		Prepped playfield GI for baking
'v0.04	Baked all the GI lighting, as well as the AO shadows.  All controlled by Lampz.
'		Adjusted all the GI colors to warm it up on the black payfield (from 255 150 68 to a warmer yellow of 243 210 101)
'		Increased the GI light under plastics to 30
'		Added disable lighting to GI bulbs
'		Adjusted the spinner dampening to 0.99 to spin a little longer.
'		Changed material on plastic yellow and plastic red slightly.
'		Added new flipper and DT backdrop images that Redbone enhanced.
'		Added VR Cabinet and VR Room.  Used Borgdog's stencils for the cab artwork.  Was fantastic and very accurate!
'		Updated the VR Backglass displays, color, position, etc.
'		Added shoot again reel for DT and VR
'		Updated the pov on Cab
'v0.05	Applied a fix for cab mode with the L12 ball release.
'		Updated several items based on recommendations from Redbone
' 		Added orange Flipper Option
'		Adjusted the black flipper material
'		Adjusted the acrylic colors slightly, and adjusted the useroption RGB.
'		Adjusted the spinner dampening to .9935; more closely matches real table spinner spin.
'		Adjusted the slope of table to min 5.5 and max of 6.0... with 50% difficulty (so avg of 5.75).
'		Changed the spinner Prim and rod; and new image.
'		Redbone altered plastics image
'		Updated the GI color to somewhere between warm white and warm yellow (RGB 248 228 164)
'		Adjusted the bumper on and off levels for just a bit higher when on.
'		Updated additional lights on desktop backglass
'		Changed playfield friction to 0.24 and changed to slope of 6.  Had to adjust saucer power and plunger speed
'		Reduced playfield reflectivity
'		Added S-T-A-R reflections on back metal wall based on Borgdog's original table.
'		Added a tourny mode setting, with precanned user settings.
'		Converted images to webp
'		Only tied the VR backglass to first turn on.  It does not dim with tilt or saucer multiball hit.
'v0.06	Redbone updated playfield, plastics, spinner, GI image, star reflections, and target
'		Fixed a small error in L40 DisableLighting
'		Made the star reflections a little larger, and updated the DisableLighting
'v0.07	Lots of updates based on Borgdog's feedback from his real table including:
'		Corrected the spinner movement (had the physics spinner at 180 degrees off)
'		Updated to correct Gottlieb sling arms
'		Changed to different metal wall images
'		Adjusted opacity of the STAR reflections
'		Changed to different tent prims.  Also adjusted the dL on the tents and rollover caps.
'		Changed the small metal posts, to thinner ones, and adjusted the rubber and physics sizes accordingly.
'		Lowered the rollover switches down slightly.
'		Replaced the screws on the plastics to hex style and set to right height
'		Redbone updated the bumper, spinner, and target images
'v0.08	Changed the bumpers to different also include a light leaking out, and adjusted the color of prims and lights.
'		Redbone updated the bumper disk and flipper images
'		Updated the GI Rubber routine.
'		Added a missing post under the apron near the drain.
'		Added numbers for LUT name options
'		Changed VR Coin Door Prim to be the correct style and color of black.  (some were narrow, some were wider.. I chose wider)
'v0.09	Updated to Roth's latest flipper code, intended to be more accurate for flippers.  
'		 - Changed the triggers to 27 VPU spacing.  
'		 - Added the LF.ReProcxessBalls to collide subs.
'		 - Updated flipper code for trajectory correction.
'		 - Added function Function DistanceFromFlipperAngle
'		Switched to mid 80's Flippers, and adjustments from Rothbauerw.
'		Slight reduction (4.1 to 4.0) on slingshots.
'v0.10	Adjusted the flipper end angles to 72, to maintain 50 degree swing.  Had to update the triggersLF and RF as well.
'v0.11	Corrected error in z_wood material physics.  
'		Added a higher friction metal material for upper right metal wall... hits the target on a proper left flipper shot now.
'		Slight adjustment to apron prim to cover edge of right wall.
'		Adjusted the EOS torque angle to 4.  And the torqe strength.  Helps with alley pass.
'v2.0.0	Updated the setdips code and ensured default dips working
'		Very slight adjustment to color on bumper caps and ring.
'		Small adjustment to right orbit wall to ensure ability to hit target on rolling left flipper shot.
'		Enabled tourney mode to adjust the DIP settings for no extra ball / novelty mode.
'		Added "click" sound to LUT changes.
'		Released
'v2.0.1	ROM isn't sending solenoid calls, and the DOF config utility fakes it out. Updated the Bumper_hit subs to add DOF 10X, DOFPulse
'v2.0.2	Minor error in the FindDips... dip 26 should have been /2
'		Updated the DIPS default code.  Simpler code and easier to manage.
'		Updated the sling_hit subs to add DOF 10X, DOFPulse
'		Adjusted the "pink" insert color slightly.
'		Had to modify the flipper trigers for addiitonal 3 degrees of trigger.
'		Ball could get stuck on rail near flipper.  Rounded corners of physics rail wall, and made bottom collidable.
'		Updated the wall "ramps" to be walls, and put new images
'		Redbone provided new images for the gate prims.
'		Rounded the small rubber peg rubbers some more.
'		Randomized the screw orientations
'		Added all options to menu by using F12
'		Removed old LUT code, and replaced in F12 menu.
'		Updated the bumper lighting to disclude the day/night mode.
'		For tourney mode, removed forcing the rainbow plastic edge
'		Added option for STAR reflections on backwall.  Users could turn up SSR if want other style reflections.
'		Added independant ROM control volume in the options menu.
'		Ball color change is in the script, and you can turn the option off to choose ball color and decal via table drop down menu.
'		Updated the black sidewood color image.
'v2.0.3	Changed the modulate to 0.99 for the FlasherGI per Hauntfreaks recommendation.
'		Lowered the playfield reflections on ball to 0.5.
'		Adjusted the flipper prims slightly.
'2.0.4	Lowered the playfield reflections on ball to 0.5.
'		Adjusted the flipper prims slightly.
'		Updated the table lighting, and general physics
'
'
'********************************************************************************************************************************
'******** Work done by UnclePaulie on Hybrid version 2.0.3 - 3.0 *********
'		Completely redid the table from ground up, Corrected table dimensions from prior table.
' 		Implemented latest VPW standards (2025), physics, debounce, raytracing, SSF, GI VLM, 3D inserts, AllLamps routines, roth standups,
'		standalone compatibility, updated flipper physics, new VR cabinet and fully hybrid, Updated Flupper style bumpers and lighting,
'		Baked GI, metal walls, interior wood walls, spinner, plastic posts, and the reflections on them, also DIP adjustments, sling corrections.
'		Full details at the bottom of the script.  
'		Redbone did the playfield, plastics, and other graphics.  
'		Original scans, image assets, cab artwork stencils all used with permission from Borgdog.
'********************************************************************************************************************************
'
'v2.1.1	Completely redid the table, as old table was incorrect dimension, and some elements (like walls) were not set correctly.
'		Set all walls, posts, prims, etc. to correct placement and dimensions.
'		Updated to latest VPW standards including flipper tricks, debounce, SSF, raytracing, alllamps, etc.
'		Brushed metal images on brackets, saucer block, no bounce on lower sling pegs
'		Updated lighting on table to newer UnclePaulie standards
'		Cutout prims, raised a touch so the bevel edge can be seen
'		Put peg screws inside the pegs
'		Updated VR to the correctly sized cabinet.  
'		Adjusted strength of slings lower.  Was WAY too strong before.  Also lowered bumper strength
'		Precisely placed the apron and plunger cover.  
'		Precisely adjusted all the mini rubber post physcis to correct size of 3/8" OD (VPunits = 17.65).  All large are correct. 
'		Updated playfield mesh.  
'		Changed slope to 6.
'		Added back plastics metal brace.  
'		Updated all the inserts to alllamps routine, and adjusted colors and sizes, falloffs, and fading
'		Implemented the full GI approach that I used in centaur:  side walls, everything, and tied to lightlevel 
'		Updated the flupper bumpers and tied to GI animation
'		Added Gottlieb hammered metal rails, and brushed lockdown bar
'		Solid inserts now have level of 1 intensity.
'		Updated plastics, edited the GI Top edges; and placed screws correctly
'		Added ball in saucer reflection mask
'		Updated lane and tent guides in blender to merge by distance the meshes... then... smoothed vecotrs for tents and average the corner angles of the normals. 
'		Put in automated rings on bumpers, and updated bumperbase.
'		Added ball brightness setting in plunger lane.
'		Adjust playfield physics friction to 23.
'		Implement the same LED option for acrylics as I did in Centaur
'		Added undercab lighting option to VR and tied to gilvl.
'		Updated the wire guide rails to prims.  Also updated the physics and made them z_col_metal_FO.
'		Added all new ball images and options and ball brightness code.
'		Added bumper brightness lit and not lit options to F12 menu
'		Updated the GI to be controlled by not only GI, but also F12 lightlevel changes.
'		Added ball options into the F12 menu based off the lotr's VPW table code
'		Added bumper ring primitives and animated them.  Also animated the bumper skirts based off code from Beat the Clock by VPW
'		Updated the flipper physics dampeners to exclude the aball.velz line.  Could cause issue with weird ball bounces.
'		Lowered the window for livecatches to make it a little harder to live catch.
'		Added nvram file for replay thresholds, and instructions on how to change it.
'		Decided to leave LED eges always on if option chosen... rather than turn on and off with each GI.  Adjusted LED code.
'		Significantly updated the script to account for individual GI on prims in the associated strings, as well as tied to day/night and F12 room brightness options.
'		Updated VR setroom to accomodate for roombrightness levels
'		Updated the material on acrylics for LED acrylic top... essentially a second layer of acrylics to control the top when LED was on.  Real reason is need bdl at zero or 1.  Couldn't do both on same prim.
'		Corrected the audiopan and audiofade functions to raise to power of 5 instead of 10.  It wasn't panning correctly.
'		Updated the sounds for the slings SSF by unbalancing them.  
'v2.1.2	Updated plastics image
'		Updated VR cab and correct, larger backbox to match actual table.
'		Added back top overall gottlieb rail edge
'		Animated backglass
'		Added backglass brackets and screws.
'		Adjusted material and GI on pegs to be more clear
'		Updated an error in the Dip switch settings
'		Updated playfield friction to 0.22, and slings tweaked.
'		Did VLM GI Bake
'		Added a slight frosted texture to the solid inserts.
'		Created a gamehelp image for the F12 rules
'		Updated material on the yellow lane guide tents
'		Added a mother of pearl insert option to the solid inserts, and rotated them
'		Reimplented the tourney mode
'		Updated the saucer logic to ensure typical rolls out of saucer and where ball bounces.
'		Updated the slings a touch.
'		Reduced the off white inserts a touch.
'		Adjusted the flipper angles to align just right, as well as adjusted physics to be that of early 80's.
'		Converted images to webp
'		Removed unused images and materials.
'		Put the star reflection flashers in for now.  Will look at baking walls.
'		Changed the fade up time for the inserts to 30ms.  (was 60 that was copied In).  Too slow before start of a ball, as the middle ones spin fast.
'		Updated the glow rings on the ship inserts to allow light to come over the yellow portions.
'		Updated the metal walls and other metal object gi levels (walls needed to have active material.
'v2.1.3	Baked the metal walls, and wood side walls for GI and reflections.
'		Baked black side walls for reflections and modified the options.
'		Baked spinner reflection light from L44 insert.
'		Ensured new baked walls worked with lightlevel.
'v2.1.4	Updated metal wall bakes to allow more inserts to reflect on the metal Walls
'		Updated the spinner lightmap to be seen on both sides of spinner when spinning.  (added a duplicate lightmap, rotated objrotx and y by 180, and moved to new postion.
'		Utilized code to turn on and off, and adjust opacity when spinning.  (used from apophis from gorgar)
'		Had a stuck ball on left flipper/rail edge.  Moved physics end of left guide rail slight to avoid it
'		Upped the blend disable lighting on the VR cab.
'		Removed options for star reflections flash image, since baked now.
'		Corrected F12 save option for LED rainbow edges
'		Updated GI on pegs (material and bdl)
'		Widened out the sounds for right and left bumpers slightly.
'		Adjusted rubber material color and gi effects
'v2.1.5	Baked the plastic peg/posts, applied a new material to them.  Added to GI.
'v2.1.6	Guide rail primitives by flippers were set to collidable, and overriding the physics portions.  Changed.
'		Added some playfield reflections to the playfield mesh and visible mesh
'		Slight increase on lane guide and lane tents GI full on.  (bdl and gitop)
'		Adjusted the opacity of the tent and guide materials slightly to make a bit more transparent.
'		Redbone updated the metal wall nestmap image to have slightly more orange reflections from star inserts.
'v3.0.0	Released
'v3.0.1	Mistake in the slingshot right sound... needed to be "right" in the script.
'v3.0.2	Updated the physics of the lower wall under the flippers to stop the ball from catching.  Changed to zCol_Playfield
'		Updated the inlane guide rails to have some springiness to them.  


' Thanks to the VPW team for testing, especially Redbone, Studlygoorite, Borgdog, and others.




' Thanks to Borgdog for the playfield, cab, and other assets!  Also his video of game play of his real machine to match as close as possible.
'	https://www.youtube.com/watch?v=NwFih3zu-4Y
' Thanks to Redbone for the playfield and plastic images, and Hauntfreaks for the updated backglass image!
' Also thanks to the VPW team for testing, and everyone else that provided feedback!

