'/////////////IMPORTANT NOTE/////////////

' The authors of this recreation have shared with the visual pinball community for personal use only.
' Please do not sell or redistribute on machines produced for commercial sale.

'////////////////////////////////////////
' The Addams Family 
' Originally manufactured 1992 by Bally Manufacturing Co. and designed by Pat Lawlor (20,270 units)
'
'////////////////////////////////////////
'////////////////////////////////////////
' Recreation for VPX by Sliderpoint, g5k and 3rdaxis
'
' USES ROM - TAF_L7
' For DMD Colorization visit : http://vpuniverse.com/forums/topic/3746-the-addams-family-colorization/
'
' NOTE: THIS TABLE INCLUDES THE IN-GAME OPTIONS MENU
' To open the menu, press both magnasave buttons at the same time in the game.
'                           _____
'                           )   (
'                          / oOo \
'                         /_______\
'                         |       |
'                         |  .-.  |
'                         |  |~|  |
' I-I-I-I-I-I-I-I-I-I-I-I-|  |_|  |I-I-I-I-I-I-I-I-I-I-I-I-I
' ).**.(~~~~~~~~~~~~~~~~~~|       |~~~~~~~~~~~~~~~~~~~).**.(
'/ |  | \                 |       |                  / |  | \
'/ |  | \                 |       |                  / |  | \
') |__| (                _|_.---._|_                 ) |__| (
'|______|_________________|' .-. '|__________________|______|
'|      |  .-.  .-.       |,-|~|-,|        .-.  .-.  |      |
'\ .    /  |~|  |~|       || | | ||        |~|  |~|  \    . /
' )H   (   |_|  |_|       ||_|_|_||        |_|  |_|   )   H(
' |    |                  |       |                   |    |
' |    |                  |       |                   |    |
' \    /   ...  ...       |_.=~=._|        ...  ...   \    /
'  ). (    |~|  |~|       |I|   |I|        |~|  |~|    ) .(
'  |H |    |_|  |_|       |I|  .|I|        |_|  |_|    | H|
'  |  |                   |I|___|I|                    |  |
'  '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'



'////////////////////////////////////////
' v 2.4.41 fluffhead35     - small end ramp rubber physic tweak
' v 2.4.40 fluffhead35     - small tweaks
' v 2.4.39 fluffheae35     - changed depth bias to 100 on insert light l36
' v 2.4.38 Fluffhead35     - fixed sidewall so that image looked correct, rotated rollovers 180, fixed plunger wall, changed left ramp exit to make ball drop more naturally.Removed unused sounds. upped left and right flipper to 2600 wihich is ax for year.
' v 2.4.37 TastyWasps      - Reduced visual size of TAF desktop logo a bit.
' v 2.4.36 Fluffhead35     - Fixing mirrored sideblades for cab mode. Removed not needed siderails, duplicated the text fester, removed text ramp, made PF image have alpha 200
' v 2.4.34 TastyWasps      - Flasher THING in back panel to track progress. Desktop VPinMame DMD placed in standard top left position with standard orange colors / sizing.  Script option to disable.
'							 TAF Logo in desktop background script option.   THING helper script option for purists.
' v 2.4.33 DGrimmReaper    - Replaced Decal with Reel for the ThingLights so it would not show for Cab users
' v 2.4.32 iaakki	       - ThePower insert reworked, small left flip power adjusted, magnet strength reduced
' v 2.4.30 DGrimmReaper    - Flasher VR DMD THING replaced with images (assets from 3rdaxis VR Room).  Moved Desktop-THING Lights to align with backwall image.  Modified script so Desktop-THING only displays for desktop.
' v 2.4.29 passion4pins    - Add THING lights to rear cab for desktop, create new Desktop-THING collection,  add red ornate wallpaper desktop backdrop (Backdrop-DT)
' v 2.4.28 TastyWasps      - Flasher VR Backglass added. Other various VR tweaks.
' v 2.4.27 fluffhead35     - Changed flipper strength from 2200 to 2500 as looked to weak from videos.  Updated end of wire ramp rubber on right ramp to simulate video better.
' v 2.4.26 fluffhead35     - Changed the ScreenBrightness sounds
' v 2.4.25 fluffhead35     - Changed Ball Image/Scratched based on Darth Vito's suggestions.  Updated flipper strength to be more in line with with physics of that era.  updated Target Bouncer to .9
' v 2.4.24 Darth Vito      - fixed some vr elements
' v 2.4.23 fluffhead35     - set desktop z position from -75 to -52 for desktop sideblades
' v 2.4.22 fluffhead35     - adjusted siderails and lockdown bar so they would be aligned correctly
' v 2.4.21 fluffhead35     - Fixed the VRRoom AutoSelect.  Added cabinet to destop mode to close light gaps.
' v 2.4.20 Darth Vito      - IUpdated minimal vr room and fixed some issues and animations.
' v 2.4.19 DaRdog          - Removed the big vr room
' v 2.4.18 fluffhead35     - updated lighting for table.  Fixed some missaligned primitives. Updated the middle ramp back mateterial settings to help prevent rejections. Fixed some z fighting.
'                          - Moved Playfield mesh to own layer.  Added in new playfield with cousin it and popbumper insert cut out.  Wired up new primitives for cousin it and other insert.  Adjusted cousin its lighting.
'                          - Changed cousin its primitive image and material.  Updated the insert text to be cleaner version and have text for insert at pop bumpers.  Adjusting physical ramps to be inline more with primitives.
'                          - Adusted on/off primitives for flasters.  Adjusted dynamic ball shadows to turn off hide behind.  Changed DL on center wire ramp to make less shiny. 
' v 2.4 17 passion4pins    - updated inlane and slings lights along with playfield reflections
' v 2.4.16 fluffhead35     - fixed GI Primitives DL.  Fixed playfield reflections and moverd insert text to a flasher to improve text quality.
' v 2.4.15 fluffehad35     - added image to stand up target that has been missing since original table.  Added wall under flipper to prevent ball from falling through playfield.
' v 2.4.14 fluffehad35     - adjusted inlane corners arround flippers for better post passing.
' v 2.4.13 fluffhead35     - adjusted collidables to have correct materials.
' v 2.4.12 fluffhead35     - added end ramp rubber stopper to rubbers collection.  Fixed depth bias issue with lighting.
' v 2.4.11 fluffhead35     - Updated physics to latest, updated stand up target collidable primitives. Increased sling speed.  Adjusted flipper triggers areas.
' v 2.4.10 fluffhead35     - Adding back in missing sounds that need to be there from original table. Added in ramp bump sounds.  Reverted the flexdmd menu back to original F6 menu which fixed sideblades.
'                          - Fixed VR AutoDetect. Adjusted right ramp material for end rubber to slow ball down. Added StandUp Target Code, Changed physics on targets to be nfozzy, added Stand Up Target Inclined Objects
'                          - to TargetBounce Collections
' v 2.4.6 fluffhead35      - New playfield with holes cut, 3d insert images imported, 3d  insert materials imported, Insert Text Layer added, Adjusted InsertLights and Bulbs. Added FlipperCradleCollision.
' v.2.4.5 RobbyKingPin     - Brought back bsrtx8 wich was deleted by accident
' v.2.4.4 fluffhead35      - Replaced the webp images with the pngs from the original table, which has now fixed all the artifacting issues
' v 2.4.3 fluffhead35      - Changed Saucers to have kick and hits be based on fleep sounds
'                          - Fixed swap entry sound when getting there from plunger lane
'                          - Fixed thing kick sounds
'                          - Fixed Vault Entry Sounds
'                          - Updated knocker to use fleep Knocker
'                          - Removed the ExtraSSFThunk code and sounds
'                          - Added SSF code for cabinet and thing hand sounds
'                          - Moved ramptrigger04 to correct ramp.
' v 2.4.2 fluffhead35      - Added in Sounds for Trigger1, Ramp55, and ThingRampS
'                          - Adjusted some Ramp Triggers to be bigger and locked them
'                          - Changed ThingSaucerHit to be PlaySoundAtBall
'                          - Added hit events to a lot of the walls
' v.2.4.1 RobbyKingPin     - Added all the old options into the VPW Options UI
' v.2.4.0 RobbyKingPin     - Added VPW Options UI, needs more options being converted from the old interface when pressing f6
' v.2.3.9 RobbyKingPin     - Removed shadowtriggers no longer being used
' v.2.3.8 fluffhead35      - Added back SwampScoopS_Hit for scoop entry sound.
'                          - Adjusted ramp endpoints for ramprolling sounds.
'                          - Added RandomRampStopSounds.
'                          - Added in ChairKickerS sound.
' v.2.3.7 RobbyKingPin     - Added more rampsound triggers and tried to add saucerlock sounds
' v.2.3.6 RobbyKingPin     - Cleaning up old sound codes where needed
' v.2.3.5 RobbyKingPin     - Fixed the problems with the zCol rubberposts on the bookcase
' v.2.3.4 Clark Kent       - Adjusted the flippers/slingshot area to enable post passes and tricks
'                          - Upper left and upper right flipper adjusted
'                          - Inlane walls adjusted all 4 to have the same angle and shape on both sides
' v.2.3.2.1.3 3rdaxis      - Converted all png images to webP
'                          - Changed Lut selection sounds
'                          - Updated physics
'                          - Updated sounds
' v.2.3.2.1.0 RobbyKingPin - Added nFozzy/RothbauerW physics, Fleep sound and VPW Dynamic Ball Shadows
' v2.1 Changelog
'- SSF audio fixes
'- Added missing DOF (Arngrim)
'- Thing flipper texture fix for flasher fires
'- Flipper return strength - adjust from 0.07 to 0.045 to allow for tip passing
'- Bumper caps less transparent
'- Adjusted lamp fading speeds for bumper lights and mansion inserts (were too slow, suggested by flupper)
'- Uncompressed textures from original (see the compressed v2.1 if you have trouble with this version)
'- Tweaks to solve mysterious ball occasionally passing through things that they shouldn't
'- Fixed Seance insert bulb error
'- More adjustments to chair scoop hit sound timing.

'////////////////////////////////////////

Option explicit
Randomize 

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'*******************************************
'  ZOPT: User Options
'*******************************************

Dim LightLevel : LightLevel = 50	'Level of room lighting (0 to 100), where 0 is dark and 100 is brightest
Dim ColorLUT : ColorLUT = 3
Dim VolumeDial : VolumeDial = 0.8			'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5 	'Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 	'Level of ramp rolling volume. Value between 0 and 1

'Dim Cabinetmode	: Cabinetmode = 0			'0 - Siderails On, 1 - Siderails Off

Dim DynamicBallShadowsOn : DynamicBallShadowsOn = 1	  '0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Dim AmbientBallShadowOn : AmbientBallShadowOn = 1	  '0 = Static shadow under ball ("flasher" image, like JP's)
									                  '1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									                  '2 = flasher image shadow, but it moves like ninuzzu's

Dim VRRoomChoice : VRRoomChoice = 1					  '1 - Minimal Room,  2 - Ultra-Minimal Room 
Const ShowVPMDMD = 0								  '1 - Show VPinMame native DMD on desktop mode, 0 = Do not show VPinMame native DMD in desktop mode
Const ShowTAFLogo = 0								  '1 - Show TAF logo in desktop background, 0 - Do not  show
Const ShowTHINGHelper = 1							  '1 - Show THING lights in back on desktop, 0 - Do not show


'*******************************************
'  ZCON: Constants and Global Variables
'*******************************************

Const BallSize = 50				 'Ball diameter in VPX units; must be 50
Const BallMass = 1				  'Ball mass must be 1
Const tnob = 3					  'Total number of balls the table can hold
Const lob = 0					   'Locked balls
Const cGameName = "TAF_L7"		 'The unique alphanumeric name for this table

Dim tablewidth
tablewidth = Table1.width
Dim tableheight
tableheight = Table1.height
Dim BIP							 'Balls in play
BIP = 0
Dim BIPL							'Ball in plunger lane
BIPL = False

Dim OptReset, ThingTrainer, DefaultOptions,  MyPrefs_Brightness, MyPrefs_DisableBrightness, MyPrefs_Gamestarted

'***********	Use staged flippers (dual leaf switches)	*******************************

Dim StagedFlipperMod
StagedFlipperMod = 0 	'0 = not staged, 1 - staged (dual leaf switches)

'Script Options
Const FlipperShadows = 1 ' change to 0 to turn off flipper shadows
Const BallShadows = 1 ' change to 0 to turn off ball shadow
Const PreloadMe = 1  ' Go through flasher sequence at table start, to prevent in-game slowdowns
'OptReset = 1  'Uncomment to reset to default the F6 options in case of error OR keep all changes temporary
ThingTrainer = 0 ' set to 1 if you want to run the auto-play thing trainer
MyPrefs_Brightness = 4                'change to 1= 30% brightness adj, 2= 50% brightness adj, 3= 70% brightness adj, 4= 100% brightness adj, 
MyPrefs_Gamestarted = 0              'change to 1 to disable brightness adjustment once the game has started. 
MyPrefs_DisableBrightness = 0  'change to 1 to disable brightness adjustyment in game completely.


'Standard Definitions
Const UseSolenoids = 2
Const UseSync = 0
Const SCoin="Coin3"

Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim UseVPMColoredDMD
Const UseVPMModSol = 1

UseVPMColoredDMD = 1

LoadVPM "01120100", "WPC.VBS", 3.26 

Sub LoadCoreVBS
     On Error Resume Next
     ExecuteGlobal GetTextFile("core.vbs")
     If Err Then MsgBox "Can't open core.vbs"
     On Error Goto 0
End Sub

Const MaxLut = 4
Set GICallback2 = GetRef("UpdateGI")
vpmMapLights AllLamps
Dim bsThingSaucer, BookMech, ThingMech
Dim UpperMagnet, RightMagnet, LeftMagnet
Dim LeftFlipperButton, LGION, RGION
Dim PrimCase1, PrimCase2, PrimCase3, PrimCase4, PrimCase6, PrimCase7, PrimCase8
Dim DayNight
DayNight = table1.NightDay

'-------Realish Trough Stuff--------------------
dim bWaitForFinish,TroughCount, MaxBalls, DoInit, EjectTime, TroughEject, AddABall
' some initial values
MaxBalls=3	' Enter number of balls you would like in the game, maximum of 8 will fit in the trough
DoInit=1
EjectTime=0
TroughEject=0
TroughCount=0
bWaitForFinish = False
AddABall = 0

'Create balls in trough
Sub InitTimer_Timer()
	If TroughCount<MaxBalls Then
		BallInit.Enabled = 1
		TroughCount=TroughCount+1
	elseIf TroughCount = MaxBalls Then
		me.enabled = false
		DrainC.enabled = 0
	End If
End Sub

Sub BallInit_Timer()
	DrainC.CreateSizedBallwithMass 25, 1.30
	me.Enabled = 0
End Sub
'------end Trough Stuff---------------

' Auto Detect VR
Dim VR_Obj, VRMode

Const LiveViewVRSim = 0		' 0 = Default, 1 = View table in VR mode in "Live View Editor" 

If RenderingMode = 2 or LiveViewVRSim = 1 Then

	VRMode = True

	For Each VR_Obj in VRCab: VR_Obj.visible = 1: Next

	If VRRoomChoice = 1 Then
		For Each VR_Obj in VRMin: VR_Obj.visible = 1: Next
	Else
		For Each VR_Obj in VRMin: VR_Obj.visible = 0: Next
	End If

	Set LampCallback = GetRef("UpdateVRBGlamps")
	SetVRBackglass

Else
	VRMode = False
	For Each VR_Obj in VRCab: VR_Obj.visible = 1: Next
	For Each VR_Obj in VRCab2: VR_Obj.visible = 0: Next
	For Each VR_Obj in VRMin: VR_Obj.visible = 0: Next
	For Each VR_Obj in VRBackglass: VR_Obj.visible = 0: Next
End If

' Initialize Table
Sub Table1_Init()
	vpmInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine="The Addams Family (Bally 1992)"
		.Games(cGameName).Settings.Value("rol") = 0
		.HandleKeyboard=0
		.ShowTitle=0
		.ShowDMDOnly=1
		.HandleMechanics=0
		.DIP(0)=&H00
		.ShowFrame=0
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With

	If DesktopMode = True Then 
		Controller.Hidden = 1
		HeadlightL.Intensity = 300
		HeadlightR.Intensity = 300

		If ShowTAFLogo = 1 Then
			TAF_Logo.Visible = 1
		Else
			TAF_Logo.Visible = 0
		End If

		If ShowVPMDMD = 1 Then
			ScoreText.visible = 1
		Else
			ScoreText.visible = 0
		End If

		If ShowThingHelper = 1 Then
			If VRMode = False Then
				ThingLights.visible = 1
			Else
				ThingLights.visible = 1
			End If
		Else
			ThingLights.visible = 1
		End If

		If oBlades = 0 Then
			BladeLeftDTOff.Visible = 1
			BladeLeftDTON.Visible = 1
			BladeRightDTOff.Visible = 1
			BladeRightDTON.Visible = 1
		End if
		Siderails.visible=False
		Lockdownbar.visible=True
	  Else
		Controller.Hidden = 0
		HeadlightL.Intensity = 150	
		HeadlightR.Intensity = 150	
		ScoreText.visible = 0
		ThingLights.visible = 0
		If oBlades = 0 Then
			BladeLeftDTOff.Visible = 1
			BladeLeftDTON.Visible = 1
			BladeRightDTOff.Visible = 1
			BladeRightDTON.Visible = 1
		End If
		Siderails.visible=False
		Lockdownbar.visible=False
		L81.Visible = False
		L82.Visible = False
		L83.Visible = False
		L84.Visible = False
		L85.Visible = False
		L86.Visible = False
		L87.Visible = False
	end If

	Set BookMech = New cvpmMyMech 
		With BookMech
			.Sol1 = 27
			.Length = 100
			.Steps = 90 
			.MType = vpmMechOneSol + vpmMechReverse + vpmMechNonLinear
			.AddSw 81, 89, 90 					' Bookcase Open
			.AddSw 82, 0, 1 					' Bookcase Close
			.Callback = GetRef("BookCaseMotor")
			.Start
		End With

	Set ThingMech = New cvpmMyMech 
		With ThingMech
			.Sol1 = 25
			.Length = 175
			.Steps = 60
			.MType = vpmMechOneSol + vpmMechReverse + vpmMechNonLinear
			.AddSw 85, 59, 60 					' Thing Up Opto
			.AddSw 84, 0, 1 					' Thing Down Opto
			.Callback = GetRef("ThingMotor")
			.Start
		End With

	Set UpperMagnet = New cvpmMagnet
		With UpperMagnet
			.InitMagnet UMagnet, 6  
			.GrabCenter = 0 
			.solenoid = 23						'Upper Magnet
			.CreateEvents "UpperMagnet"
		End With

	Set RightMagnet = New cvpmMagnet  
		With RightMagnet
			.InitMagnet RMagnet, 6 
			.GrabCenter = 0 	
			.solenoid = 24						'Right Magnet
			.CreateEvents "RightMagnet"
		End With

	Set LeftMagnet = New cvpmMagnet  
		With LeftMagnet
			.InitMagnet LMagnet, 6  
			.GrabCenter = 0 
			.solenoid = 16						'Left Magnet
			.CreateEvents "LeftMagnet"
		End With
	
	Wall69.isDropped = 1
	Wall70.isDropped = 1
	troughW1.isDropped = 1
	troughW2.isDropped = 1
End Sub

Sub SetLUT
    if gamestarted = 0 Then
	Select Case MyPrefs_Brightness
		Case 4:table1.ColorGradeImage = 0:EMRealBValue.Image = "Brightness4G":EMRealBValueFS.Image = "Brightness4GFS"
		Case 3:table1.ColorGradeImage = "AA_FS_Lut30perc":EMRealBValue.Image = "Brightness3G":EMRealBValueFS.Image = "Brightness3GFS"
		Case 2:table1.ColorGradeImage = "AA_FS_Lut50perc":EMRealBValue.Image = "Brightness2G":EMRealBValueFS.Image = "Brightness2GFS"
		Case 1:table1.ColorGradeImage = "AA_FS_Lut70perc":EMRealBValue.Image = "Brightness1G":EMRealBValueFS.Image = "Brightness1GFS"
		Case 0:table1.ColorGradeImage = "AA_FS_Lut100perc":EMRealBValue.Image = "Brightness0G":EMRealBValueFS.Image = "Brightness0GFS"
	end Select
    end if
end sub 

Sub ShowLUT
        if gamestarted = 0 Then
	if SHowDT = True Then
		EMRealBrightness.Visible = True
		EMRealBValue.Visible = True
		EMRealBrightness.TimerEnabled = 1
	else
		EMRealBrightnessFS.Visible = True
		EMRealBValueFS.Visible = True
		EMRealBrightnessFS.TimerEnabled = 1
	end if
    end if
End Sub

' GMJ - Added by robby need to Remove
Sub SetLightLevel(lvl)
	Dim v
	v = Int(lvl * 2 + 55)
'	Dim x: For Each x in BM_Room: x.Color = RGB(v, v, v): Next
'	ramp_giOn.blenddisablelighting = 0.7*((1-0.8)*lvl/100 + 0.8)
'	ramp_giOff.blenddisablelighting = 0.3*((1-0.5)*lvl/100 + 0.5)
End Sub

Sub EMRealBrightness_Timer
	EMRealBrightness.Visible = False
	EMRealBValue.Visible = False
	EMRealBrightness.TimerEnabled = 0
End Sub

Sub EMRealBrightnessFS_Timer
	EMRealBrightnessFS.Visible = False
	EMRealBValueFS.Visible = False
	EMRealBrightnessFS.TimerEnabled = 0
End Sub

'-------other game functions----------
Sub TAF_Paused:Controller.Pause = 1:End Sub
Sub TAF_unPaused:Controller.Pause = 0:End Sub
Sub TAF_Exit:Controller.Stop:End Sub  

Dim GIInit: GIInit=10 * 4

'*******************************************
'	ZTIM:Timers
'*******************************************

Sub GameTimer_Timer() 'The game timer interval; should be 10 ms
	Cor.Update	  'update ball tracking (this sometimes goes in the RDampen_Timer sub)
	RollingUpdate   'update rolling sounds
	'DoDTAnim		'handle drop target animations
	DoSTAnim		'handle stand up target animations
	'queue.Tick	  'handle the queue system

	If PreloadMe = 1 and GIInit > 0 Then
		GIInit = GIInit -1
		select case (GIInit \ 4) ' Divide by 4, this is not a frame timer, so we want to be sure frame is visible 
		case 0:
				FlipperL.image="leftflipper_giON"
				FlipperT.image="thingflipper_giON"
				Packard.image="car_packard_GiON_lampsON"
				ThingBOX.image="thingboxlid_GION"
				FlipperR.image="rightflipper_giON"
				FlipperR1.image="rightUPPERflipper_giON"
				ChairPrim.image="chair_GION"
				Thing.image="hand_GION"
				bumper_YELLOWBASE.image="RGP3_yellowbumpbase_LIGHTON"
				bumper_YELLOWBASEACTIVE.image="RGP3_yellowBBACTIVE_LIGHTON"
				Bumper_BLUEBASE.image="RGP3_bluebumpbase_LIGHTON"
				Bumper_BLUEBASEACTIVE.image="RGP3_blueBBACTIVE_LIGHTON"
				bumper_REDBASE.image="RGP3_redbumpbase_LIGHTON"
				bumper_REDBASEACTIVE.image="RGP3_redBBACTIVE_LIGHTON"
				bumper_ORANGEBASE.image="RGP3_orangebumpbase_LIGHTON"
				bumper_ORANGEBASEACTIVE.image="RGP3_orangeBBACTIVE_LIGHTON"
				bumper_CLEARBASE.image="RGP3_clearbumpbase_LIGHTON"
				bumper_CLEARBASEACTIVE.image="RGP3_clearBBACTIVE_LIGHTON"
				RGP10_steelbrushedLeft.image="RGP10_steelbrushed_GION_ChairRY"
				RGP8_B_GI2.image="RGP8B_GI2_GION_GRNREDL"
				RGP9_GI1_4.image="RGP9_GI1_4_GION_GrnYelL"
				vault_base.image="base"
				gRGP6_WhiteFlasher3.image="rgp6_flasherwhite3_on_off"
				gRGP5_habittrail_metrampsRight.image="rgp5_railsmetals_GION"
				RGP5_rampPlates.image="RGP5_rampplatesfix_GION"
				RGP8_GI2_2.image="RGP8_GI2_2_GION"
				gRGP2_plastic.image="RGP2_gion"
				gRGP6_RedFlasher1.image="rgp6_flasherred1_on_off"
				gRGP5_habittrail_metrampsLeft.image="rgp5_railsmetals_GION"
				RGP9_GI1_5.image="RGP9_GI1_5_GION"
				gRGP6_RedFlasher2.image="rgp6_flasherred2_on_off"
				gRGP6_WhiteFlasher1.image="rgp6_flasherwhite1_on_off"
				RGP9_GI1_3.image="RGP9_GI1_3_GION"
				gRGP6_RedFlasher3.image="rgp6_flasherred3_on_off"
				gRGP6_WhiteFlasher2.image="rgp6_flasherwhite2_on_off"
				RGP8_GI2_3.image="RGP8_GI2_3_GION"
				RGP8_GI2_4.image="RGP8_GI2_4_GION"
				gRGP4_clearrampREFRACT.image="rgp4_clearramp_refract_GION"

		case 1:
				FlipperL.image="leftflipper_giOFF"
				FlipperT.image="thingflipperUP_giOFF"
				Packard.image="car_packard_GiOff"
				ThingBOX.image="thingboxlid_GIOFF"
				FlipperR.image="rightflipperUP_giOFF"
				FlipperR1.image="rightUPPERflipperUP_giOFF"
				ChairPrim.image="Chair_ColouredBulbLR"
				Thing.image="hand_gioff"
				bumper_YELLOWBASE.image="RGP3_yellowbumpbase_GION"
				bumper_YELLOWBASEACTIVE.image="RGP3_yellowBBACTIVE_LIGHTOFF"
				Bumper_BLUEBASE.image="RGP3_bluebumpbase_GION"
				Bumper_BLUEBASEACTIVE.image="RGP3_blueBBACTIVE_LIGHTOFF"
				bumper_REDBASE.image="RGP3_redbumpbase_GION"
				bumper_REDBASEACTIVE.image="RGP3_redBBACTIVE_LIGHTOFF"
				bumper_ORANGEBASE.image="RGP3_orangebumpbase_GION"
				bumper_ORANGEBASEACTIVE.image="RGP3_orangeBBACTIVE_LIGHTOFF"
				bumper_CLEARBASE.image="RGP3_clearbumpbase_GION"
				bumper_CLEARBASEACTIVE.image="RGP3_clearBBACTIVE_LIGHTOFF"
				RGP10_steelbrushedLeft.image="RGP10_steelbrushed_GION_ChairR"
				RGP8_B_GI2.image="RGP8B_GI2_GION_GRNL"
				RGP9_GI1_4.image="RGP9_GI1_4_GION_YelL"
				gRGP6_WhiteFlasher3.image="rgp6_flasherwhite3_on_on"
				gRGP5_habittrail_metrampsRight.image="rgp5_railsmetals_LowerFlasher"
				RGP5_rampPlates.image="RGP5_rampplatesfix_Flasher"
				RGP8_GI2_2.image="RGP8_GI2_2_FlasherON"
				gRGP2_plastic.image="RGP2_gion_flon"
				gRGP6_RedFlasher1.image="rgp6_flasherred1_on_on"
				gRGP5_habittrail_metrampsLeft.image="rgp5_railsmetals_RedFlasher"
				RGP9_GI1_5.image="RGP9_GI1_5_GION_FlaREDON"
				gRGP6_RedFlasher2.image="rgp6_flasherred2_on_on"
				gRGP6_WhiteFlasher1.image="rgp6_flasherwhite1_on_on"
				RGP9_GI1_3.image="RGP9_GI1_3_FlasherWHITEON"
				gRGP6_RedFlasher3.image="rgp6_flasherred3_on_on"
				gRGP6_WhiteFlasher2.image="rgp6_flasherwhite2_on_on"
				RGP8_GI2_3.image="RGP8_GI2_3_FlasherON"
				RGP8_GI2_4.image="RGP8_GI2_4_FlasherON"
		case 2:
				FlipperL.image="leftflipperUP_giON"
				FlipperT.image="thingflipper_giOFF"
				Packard.image="car_packard_GiON"
				FlipperR.image="rightflipper_giOFF"
				FlipperR1.image="rightUPPERflipper_giOFF"
				ChairPrim.image="Chair_ColouredBulbR"
				RGP10_steelbrushedLeft.image="RGP10_steelbrushed_GION_ChairY"
				RGP8_B_GI2.image="RGP8B_GI2_GION_REDL"
				RGP9_GI1_4.image="RGP9_GI1_4_GION_GrnL"
				RGP9_GI1_5.image="RGP9_GI1_5_GION_FlaWHITEON"
		case 3:
				FlipperL.image="leftflipperUP_giOFF"
				FlipperT.image="thingflipperUP_giON"
				Packard.image="car_packard_GiOff_lampsOFF"
				FlipperR.image="rightflipperUP_giON"
				FlipperR1.image="rightUPPERflipperUP_giON"
				ChairPrim.image="Chair_ColouredBulbL"
				RGP10_steelbrushedLeft.image="RGP10_steelbrushed_GION"
				RGP8_B_GI2.image="RGP8B_GI2_GION"
				RGP9_GI1_4.image="RGP9_GI1_4_GION"
		case 4:
				ChairPrim.image="Chair_GION"
				RGP8_B_GI2.image="RGP8B_GI2_FlasherON"
				RGP9_GI1_4.image="RGP9_GI1_4_FlasherWHITEON"
		case 5:
				ChairPrim.image="chair_LeftFlasher"
		case 6:
				ChairPrim.image="chair_LeftFlasBulbLR"
		case 7:
				ChairPrim.image="Chair_LeftFlasBulbL"
		case 8:
				ChairPrim.image="Chair_LeftFlasBulbR"
		case 9:
				ChairPrim.image="chair_ColouredBulbLR"			
		end select 
					'exit sub
	End If

	IF LGION = 0 Then
'		FlipperL.disablelighting = 0
'		FlipperT.disablelighting = 0
		If LeftFlipper.CurrentAngle < 80 Then
			If oFlippers = 0 Then
				FlipperL.image = "leftflipperUP_giOFF"
			Else
				FlipperL.image = "leftflipperUp_giOff_BLK"
			End if
		Else
			If oFlippers = 0 Then
				FlipperL.image = "leftflipper_giOFF"
			Else	
				FlipperL.image = "leftflipper_giOFF_BLK"
			End if
		End If

		If LeftFlipper1.CurrentAngle > 128 Then
			If Prim7On = 1 Then
				If oFlippers = 0 Then
					FlipperT.image = "thing_down_GIOFF_FlashON"
				Else
					FlipperT.image = "thing_down_GIOFF_FlashON_BLK"
				End If
			Else
				If oFlippers = 0 Then
					FlipperT.image = "thingflipper_giOFF"
				Else
					FlipperT.image = "thing_down_GIOff_BLK"
				End If
			End If
		Else
			If oFlippers = 0 Then
				FlipperT.image = "thingflipperUP_giOFF"
			Else
				FlipperT.image = "thing_UP_GIOff_BLK"
			End If
		End If


	Elseif LGION = 1 Then
'		FlipperL.disablelighting = 0.5
'		FlipperT.disablelighting = 0.5
		If LeftFlipper.CurrentAngle < 80 Then
			If oFlippers = 0 Then
				FlipperL.image = "leftflipperUP_giON"
			Else
				FlipperL.image = "leftflipperUP_giON_BLK"
			End If
		Else
			If oFlippers = 0 Then
				FlipperL.image = "leftflipper_giON"
			Else
				FlipperL.image = "leftflipper_giON_BLK"
			End If
		End If

		If LeftFlipper1.CurrentAngle > 128 Then
			If Prim7On = 1 Then
				If oFlippers = 0 Then
					FlipperT.image = "thing_down_GION_FlashON"
				Else
					FlipperT.image = "thing_down_GION_FlashON_BLK"
				End If
			Else
				If oFlippers = 0 Then
					FlipperT.image = "thingflipper_giON"
				Else
					FlipperT.image = "thing_down_GION_BLK"
				End If
			End If
		Else
			If oFlippers = 0 Then
				FlipperT.image = "thingflipperUP_giON"
			Else
				FlipperT.image = "thing_UP_GION_BLK"
			End If
		End If

		ChairPrim.image = "chair_GION"
	End if	

	IF RGION = 0 Then
		LimoInterior.State = 0 'AXS
		Packard.Image = "car_packard_GiOff"
		HeadlightL.State = 0 
		HeadlightR.State = 0 
		HeadlightTrain.State = 0
		BearRugEyesNEW.Material = "Black"
		Thing.image = "hand_gioff"
		ThingBOX.image = "thingboxlid_GIOFF"
'		FlipperR.disablelighting = 0
'		FlipperR1.disablelighting = 0
		If RightFlipper.CurrentAngle < -80 Then
			If oFlippers = 0 Then
				FlipperR.image = "rightflipper_giOFF"
			Else
				FlipperR.image = "rightflipper_giOFF_BLK"
			End If
		Else
			If oFlippers = 0 Then
				FlipperR.image = "rightflipperUP_giOFF"
			Else
				FlipperR.image = "rightflipperUP_giOFF_BLK"
			End If
		End If

		If RightFlipper1.CurrentAngle < -125 Then
			If oFlippers = 0 Then
				FlipperR1.image = "rightUPPERflipper_giOFF"
			Else
				FlipperR1.image = "rightUPPERflipper_giOFF_BLK"
			End If
		Else
			If oFlippers = 0 Then
				FlipperR1.image = "rightUPPERflipperUP_giOFF"
			Else
				FlipperR1.image = "rightUPPERflipperUp_giOFF_BLK"
			End If
		End If

	Elseif RGION = 1 Then
		LimoInterior.State = 1 'AXS
		Packard.Image = "car_packard_GiON"
	   ' Packard.Image = "car_packard_GiON_lampsON"
		HeadlightL.State = 1 
		HeadlightR.State = 1 
		HeadlightTrain.State = 0
		BearRugEyesNEW.Material = "BearEyesNew"
		Thing.image = "hand_GION"
		ThingBOX.image = "thingboxlid_GION"
'		FlipperR.disablelighting = 0.5
'		FlipperR1.disablelighting = 0.5
		If RightFlipper.CurrentAngle < -80 Then
			If oFlippers = 0 Then
				FlipperR.image = "rightflipper_giON"
			Else
				FlipperR.image = "rightflipper_giON_BLK"
			End If
		Else
			If oFlippers = 0 Then
				FlipperR.image = "rightflipperUP_giON"
			Else
				FlipperR.image = "rightflipperUP_giON_BLK"
			End if
		End If

		If RightFlipper1.CurrentAngle < -125 Then
			If oFlippers = 0 Then
				FlipperR1.image = "rightUPPERflipper_giON"
			Else
				FlipperR1.image = "rightUPPERFlipper_giON_BLK"
			End If
		Else
			If oFlippers = 0 Then
				FlipperR1.image = "rightUPPERflipperUP_giON"
			Else
				FlipperR1.image = "rightUPPERflipperUP_giON_BLK"
			End If
		End If

	End If

	If L13.State=1 Then 'AXS
		Packard.Image = "car_packard_GiON"
		HeadlightL.State = 0
		HeadlightR.State = 0
		HeadlightTrain.State = 0
	Else
		Packard.Image = "car_packard_GiON_lampsON"
		HeadlightL.State = 1
		HeadlightR.State = 1
		HeadlightTrain.State = 0
	End If

	flipperL.RotZ = LeftFlipper.CurrentAngle
    flipperR.RotZ = RightFlipper.CurrentAngle
    flipperR1.RotZ = RightFlipper1.CurrentAngle
	FlipperT.RotZ = LeftFlipper1.CurrentAngle
	DiverterPrim.ObjRotZ = Diverter.CurrentAngle - 137
	SW61Lever.ObjRotX = -SW61.currentAngle * .10
	SW64Lever.ObjRotX = -SW64.currentAngle * .10

	' Desktop flasher objects for THING display
	If VRMode = False and DesktopMode = True Then
		If ShowThingHelper = 1 Then
			F81LeftStar.Visible = L81.State
			F82T.Visible = L82.State
			F83H.Visible = L83.State
			F84I.Visible = L84.State
			F85N.Visible = L85.State
			F86G.Visible = L86.State
			F87RightStar.Visible = L87.State
		End If
	End If

	' VR Flasher objects for THING display
	If VRMode = True Then
		L81F.visible = L81.state
		L82F.visible = L82.state
		L83F.visible = L83.state
		L84F.visible = L84.state
		L85F.visible = L85.state
		L86F.visible = L86.state
		L87F.visible = L87.state
	End If

	If FlipperShadows = 1 Then
		FlipperShadowL.RotZ = LeftFlipper.currentAngle
		FlipperShadowR.RotZ = RightFlipper.currentAngle
	End If

	If L23.State = 1 Then
		bumper_YELLOWBASE.image = "RGP3_yellowbumpbase_LIGHTON"
		bumper_YELLOWBASEACTIVE.image = "RGP3_yellowBBACTIVE_LIGHTON"
	Else
		bumper_YELLOWBASE.image = "RGP3_yellowbumpbase_GION"
		bumper_YELLOWBASEACTIVE.image = "RGP3_yellowBBACTIVE_LIGHTOFF"
	End If

	If L24.State = 1 Then
		Bumper_BLUEBASE.image = "RGP3_bluebumpbase_LIGHTON"
		Bumper_BLUEBASEACTIVE.image = "RGP3_blueBBACTIVE_LIGHTON"
	Else
		Bumper_BLUEBASE.image = "RGP3_bluebumpbase_GION"
		Bumper_BLUEBASEACTIVE.image = "RGP3_blueBBACTIVE_LIGHTOFF"
	End If

	If L21.State = 1 Then
		bumper_REDBASE.image = "RGP3_redbumpbase_LIGHTON"
		bumper_REDBASEACTIVE.image = "RGP3_redBBACTIVE_LIGHTON"
	Else
		bumper_REDBASE.image = "RGP3_redbumpbase_GION"
		bumper_REDBASEACTIVE.image = "RGP3_redBBACTIVE_LIGHTOFF"
	End If
	
	If L25.State = 1 Then
		bumper_ORANGEBASE.image = "RGP3_orangebumpbase_LIGHTON"
		bumper_ORANGEBASEACTIVE.image = "RGP3_orangeBBACTIVE_LIGHTON"
	Else
		bumper_ORANGEBASE.image = "RGP3_orangebumpbase_GION"
		bumper_ORANGEBASEACTIVE.image = "RGP3_orangeBBACTIVE_LIGHTOFF"
	End If

	If L22.state = 1 Then
		bumper_CLEARBASE.image = "RGP3_clearbumpbase_LIGHTON"
		bumper_CLEARBASEACTIVE.image = "RGP3_clearBBACTIVE_LIGHTON"
	Else
		bumper_CLEARBASE.image = "RGP3_clearbumpbase_GION"
		bumper_CLEARBASEACTIVE.image = "RGP3_clearBBACTIVE_LIGHTOFF"
	End If

	If L47.state = 1 and L64.State = 1 Then
		ChairPrim.image = "Chair_ColouredBulbLR"
		RGP10_steelbrushedLeft.image = "RGP10_steelbrushed_GION_ChairRY"
	Elseif L47.State = 1 and L64.State = 0 Then
		ChairPrim.image = "Chair_ColouredBulbR"
		RGP10_steelbrushedLeft.image = "RGP10_steelbrushed_GION_ChairR"
	Elseif L47.State = 0 and L64.State = 1 Then
		ChairPrim.image = "Chair_ColouredBulbL"
		RGP10_steelbrushedLeft.image = "RGP10_steelbrushed_GION_ChairY"
	Elseif L47.state = 0 and L64.state = 0 Then
		ChairPrim.image = "Chair_GION"
		RGP10_steelbrushedLeft.image = "RGP10_steelbrushed_GION"
	End If
	
	If L74.State = 1 and L75.State = 1 Then
		RGP8_B_GI2.image = "RGP8B_GI2_GION_GRNREDL"
	Elseif L74.State = 1 and L75.State = 0 Then
		RGP8_B_GI2.image = "RGP8B_GI2_GION_GRNL"
	Elseif L74.State = 0 and L75.State = 1 Then
		RGP8_B_GI2.image = "RGP8B_GI2_GION_REDL"
	Elseif L74.State = 0 and L75.State = 0 Then
		RGP8_B_GI2.image = "RGP8B_GI2_GION"
	End If

	If L77.State = 1 and L78.State = 1 Then
		RGP9_GI1_4.image = "RGP9_GI1_4_GION_GrnYelL"
	Elseif L77.State = 1 and L78.State = 0 Then
		RGP9_GI1_4.image = "RGP9_GI1_4_GION_YelL"
	Elseif L77.State = 0 and L78.State = 1 Then
		RGP9_GI1_4.image = "RGP9_GI1_4_GION_GrnL"
	Elseif L77.State = 0 and L78.State = 0 Then
		RGP9_GI1_4.image = "RGP9_GI1_4_GION"
	End If

 End Sub

Dim FrameTime, InitFrameTime
InitFrameTime = 0
Sub FrameTimer_Timer() 'The frame timer interval should be -1, so executes at the display frame rate
	FrameTime = GameTime - InitFrameTime
	InitFrameTime = GameTime	'Count frametime
	FlipperVisualUpdate		 'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

Dim gamestarted, MyOrganNote
gamestarted = 0

'---------------Keys---------------
Sub Table1_KeyDown(ByVal keycode)
	'DebugShotTableKeyDownCheck keycode
	If vpmKeyDown(keycode) Then Exit Sub

	If keycode = LeftFlipperKey Then
		Pincab_Flipper_Button_Left.X = Pincab_Flipper_Button_Left.X +10
	End If

	If keycode = RightFlipperKey Then
		Pincab_Flipper_Button_Right.X = Pincab_Flipper_Button_Right.X - 10
	End If

	If keycode = PlungerKey Then
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If

	If Keycode = StartGameKey Then	
		Pincab_Start_Button.y = Pincab_Start_Button.y -5
	End If
  	
    If keycode = RightMagnaSave  Then
          If gamestarted = 0 Then
		if MyPrefs_DisableBrightness = 0 then
			MyPrefs_Brightness = MyPrefs_Brightness + 1
			if MyPrefs_Brightness > MaxLut then MyPrefs_Brightness = 0
			SetLUT
			ShowLUT
			PlaySound "LutChange"
		end if
             end if
	end if
	If keycode = LeftMagnaSave   Then
          If gamestarted = 0 Then
            If MyPrefs_DisableBrightness = 0 then
			MyPrefs_Brightness= MyPrefs_Brightness - 1
			if MyPrefs_Brightness < 0 then MyPrefs_Brightness = MaxLut
			SetLUT
			ShowLUT
			PlaySound "LutChange2"
		end if
            end if
        end if

  	If keycode = LeftFlipperKey Then
		FlipperActivate LeftFlipper, LFPress
		If StagedFlipperMod <> 1 Then
			FlipperActivate LeftFlipper1, LFPress1
		End If
	End If
	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		If StagedFlipperMod <> 1 Then
			FlipperActivate RightFlipper1, RFPress1
		End If
	End If
	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperLeft Then FlipperActivate LeftFlipper1, LFPress1
		If keycode = KeyUpperRight Then FlipperActivate RightFlipper1, RFPress1
	End If
	If keycode = PlungerKey Then
		Plunger.Pullback
		SoundPlungerPull
	End If
	If keycode = LeftTiltKey Then
		Nudge 90, 1
		SoundNudgeLeft
	End If
	If keycode = RightTiltKey Then
		Nudge 270, 1
		SoundNudgeRight
	End If
	If keycode = CenterTiltKey Then
		Nudge 0, 1
		SoundNudgeCenter
	End If
	If keycode = MechanicalTilt Then
		SoundNudgeCenter() 'Send the Tilting command to the ROM (usually by pulsing a Switch), or run the tilting code for an orginal table
	End If
	If keycode = StartGameKey Then
		SoundStartButton
	End If
	'   If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then 'Use this for ROM based games
	If keycode = AddCreditKey Or keycode = AddCreditKey2 Then
		Select Case Int(Rnd * 3)
			Case 0
				PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1
				PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2
				PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If


End Sub

Sub Trigger001_Hit
    if MyPrefs_Gamestarted = 1 then
            gamestarted = 1
    end if
End Sub

Sub Table1_KeyUp(ByVal keycode)
	'DebugShotTableKeyUpCheck keycode
	If vpmKeyUp(keycode) Then Exit Sub

	If keycode = LeftFlipperKey Then
		Pincab_Flipper_Button_Left.X = Pincab_Flipper_Button_Left.X -10
	End If

	If keycode = RightFlipperKey Then
		Pincab_Flipper_Button_Right.X = Pincab_Flipper_Button_Right.X +10
	End If

	If Keycode = StartGameKey Then	
		Pincab_Start_Button.y = Pincab_Start_Button.y +5
	End If

	If keycode = LeftFlipperKey Then
		LeftFlipperButton = 0
		FlipperDeActivate LeftFlipper, LFPress
		If StagedFlipperMod <> 1 Then
			FlipperDeActivate LeftFlipper1, LFPress1
		End If
	End If
	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		If StagedFlipperMod <> 1 Then
			FlipperDeActivate RightFlipper1, RFPress1
		End If
	End If
	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperLeft Then 
			FlipperDeActivate LeftFlipper1, LFPress1
		End If
		If keycode = KeyUpperRight Then 
			FlipperDeActivate RightFlipper1, RFPress1
		End If
	End If
	If KeyCode = PlungerKey Then
		Plunger.Fire
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		Pincab_Plunger.Y = 1068.178
		Pincab_Plunger2.Y = 1068.178
		If BIPL = 1 Then
			SoundPlungerReleaseBall()   'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall() 'Plunger release sound when there is no ball in shooter lane
		End If
	End If
End Sub

'----------Nudging---------------
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 5

'---------Solonoid definitions------------
SolCallback(1)="Chair" 											'Chair Kickout
SolCallback(2)="Knocker" 										'Thing Knocker
SolCallback(3)="DivertRamp"										'Ramp Diverter
SolCallback(4)="BallKick"  										'Ball Release
SolCallBack(5)="ballDrain" 										'Outhole
SolCallback(6)="ThingMagnet" 									'Thing Magnet
SolCallback(7)="ThingKickout" 									'Thing Kickout
SolCallback(8)="LockupKickout"									'Lockup Kickout
'SolCallback(9)= 												'Upper Left Jet
'SolCallback(10)= 												'Upper Right Jet
'SolCallback(11)=												'Center Left Jet
'SolCallBack(12)=			  									'Center Right Jet
'SolCallback(13)= 												'Lower Jet
'SolCallback(14)= 												'Left Slingshot
'SolCallback(15)= 												'Right Slingshot
'SolCallback(16)=		 										'Left Magnet
SolModCallback(17)="TelephoneFlasher"							'Telephone/Upper Right Ramp - flasher
SolModCallback(18)="TrainFlasher" 								'Train/Upper Left Ramp - flasher
SolModCallback(19)="LowerRampFlasher" 							'Lower Ramp/Jet Bumpers (2) - flasher
SolModCallback(20)="LlightningBoltFlasher" 						'Left Lighting bolt/Mini Flipper - flasher
SolModCallback(21)="RlightningBoltFlasher"						'Right Lightning bolt/Swampt - flasher
SolModCallback(22)="ThePowerFlasher"							'The Power/Backbox Cloud(3) - flasher
'SolCallback(23)=												'Upper Magnet
'SolCallback(24)=												'Right Magnet
'SolCallback(25)=												'Thing Motor
SolCallback(26)="ThingEjectHole"								'Thing Eject Hole - flasher
'SolCallback(27)=												'Bookcase Motor
SolCallback(28)="SwampRelease"									'Swamp Release 

'-----------Flippers-------------------
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sURFlipper) = "SolURFlipper"
SolCallback(sULFlipper) = "SolULFlipper"
	
'*******************************************
'	ZFLP: Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled) 'Left flipper solenoid callback
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
		
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper
		Else
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
	Else
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled) 'Right flipper solenoid callback
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend
		
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolULFlipper(Enabled)
	If Enabled Then
		If StagedFlipperMod = 1 Then 
			If leftflipper1.currentangle < leftflipper1.endangle + ReflipAngle Then
				RandomSoundReflipUpLeft LeftFlipper1
			Else
				SoundFlipperUpAttackLeft LeftFlipper1
				RandomSoundFlipperUpLeft LeftFlipper1
			End If
		End If
		LeftFlipper1.RotateToEnd
	Else
		If StagedFlipperMod = 1 Then 
			If LeftFlipper1.currentangle < LeftFlipper1.startAngle - 5 Then
				RandomSoundFlipperDownLeft LeftFlipper1
			End If
			FlipperLeftHitParm = FlipperUpSoundLevel
		End If
		LeftFlipper1.RotateToStart
	End If
End Sub

Sub SolURFlipper(Enabled)
	If Enabled Then
		If StagedFlipperMod = 1 Then
			If rightflipper1.currentangle > rightflipper1.endangle - ReflipAngle Then
				RandomSoundReflipUpRight RightFlipper1
			Else
				SoundFlipperUpAttackRight RightFlipper1
				RandomSoundFlipperUpRight RightFlipper1
			End If
		End If
		RightFlipper1.RotateToEnd
	Else
		If StagedFlipperMod = 1 Then 
			If RightFlipper1.currentangle > RightFlipper1.startAngle + 5 Then
				RandomSoundFlipperDownRight RightFlipper1
			End If
			FlipperRightHitParm = FlipperUpSoundLevel
		End If
		RightFlipper1.RotateToStart
	End If
End Sub

' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	LeftFlipperCollide parm
End Sub

Sub LeftFlipper1_Collide(parm)
	CheckLiveCatch ActiveBall, LeftFlipper1, LFCount1, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	RightFlipperCollide parm
End Sub

Sub RightFlipper1_Collide(parm)
	CheckLiveCatch ActiveBall, RightFlipper1, RFCount1, parm
	RightFlipperCollide parm
End Sub

Sub FlipperVisualUpdate 'This subroutine updates the flipper shadows and visual primitives
	flipperL.RotZ = LeftFlipper.CurrentAngle
    flipperR.RotZ = RightFlipper.CurrentAngle
    flipperR1.RotZ = RightFlipper1.CurrentAngle
	FlipperT.RotZ = LeftFlipper1.CurrentAngle
	If FlipperShadows = 1 Then
		FlipperShadowL.RotZ = LeftFlipper.currentAngle
		FlipperShadowR.RotZ = RightFlipper.currentAngle
	End If
End Sub

'------------Sling Shot Animations----------
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    RS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotRight Sling1
    RSling1.Visible = 1
	sling1.TransX = -30
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	vpmTimer.pulsesw 37											'RightSlingShot
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransX = -20
        Case 4:RSLing2.Visible = 0::sling1.TransX = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotLeft Sling2
    LSling1.Visible = 1
    sling2.TransX = -30
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	vpmTimer.pulsesw 36										'LeftSlingShot
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransX = -20
		Case 4:LSLing2.Visible = 0:sling2.TransX = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

'-------------------Solenoids/Flashers---------------
Sub Chair (enabled)
	If Enabled Then 
		ChairKicker.TimerEnabled = 1
	End If
End Sub

Sub DivertRamp(Enabled)
	If Enabled Then
		DiverterWall.isDropped = 1
		Diverter.rotatetoEnd
		PlaySound SoundFX ("DiverterSol", DOFContactors),0,1,-1,0
	Else
		DiverterWall.isDropped = 0
		Diverter.rotatetostart
		PlaySound SoundFX("LeftFlipperDown",DOFContactors), 0, .4, -1, 0
	End If
End Sub

Sub ThingMagnet(Enabled)
	If Enabled = true and ThingBall = True Then
		Set Tball = ThingSaucer.lastcapturedball
		ThingSaucer.Enabled = 0
		ThingSaucer.kickz 45, 9, 5, 20
		Controller.Switch(87) = 0
		ThingSaucer.TimerEnabled = 1
		Playsound "ThingGrab"
		TballTimer2.Enabled = 1
	Elseif Enabled = True and ThingBall = False Then
	Else
		TballTimer2.Enabled = 0
	End If
End Sub
	
Sub LockupKickout(Enabled)
	If Enabled Then
		SwampLockUp.TimerEnabled = 1
	End If
End Sub

Sub	 LlightningBoltFlasher(Level)
	 If Level > 0 Then
		LLightning.intensityScale = (Level / 2.55)/100
		LLightningb.intensityScale = (Level / 2.55)/100
		Light13c.intensityScale = (Level / 2.55)/100
		Light15.intensityScale = (Level / 2.55)/100
		Light25.intensityScale = (Level / 2.55)/100
		LLightning.State = 1
		LLightningb.State = 1
'		Light13c.state = 1
		Light15.state = 1
'		Light25.state = 1
		PLAYFIELD_flasher7_w.Opacity = Level
		PrimCase7 = 1
		Prim7.Enabled = 1
	Else
		LLightning.State = 0
		LLightningb.State = 0
'		Light13c.state = 0
		Light15.state = 0
		PLAYFIELD_flasher7_w.Opacity = 0
		'Light25.state = 0
		PrimCase7 = 4
		Prim7.Enabled = 1
	End If
End Sub

Sub	 RlightningBoltFlasher(Level)
	 If Level > 0 Then
		RLightning.intensityScale = (Level / 2.55)/100
		RLightningb.intensityScale = (Level / 2.55)/100
		Light6.intensityScale = (Level / 2.55)/100
		PLAYFIELD_flasher6_w.Opacity = Level
		RLightning.State = 1
		RLightningb.State = 1
		Light13c.state = 0
		Light6.state = 1
		PrimCase3 = 1
		Prim3.Enabled = 1
	Else
		RLightning.State = 0
		RLightningb.State = 0
		Light6.state = 0
		PLAYFIELD_flasher6_w.Opacity = 0
		PrimCase3 = 4
		Prim3.Enabled = 1
	End If
End Sub

Sub LowerRampFlasher(level)
	If Level > 0 Then
		Light12.intensityScale = (Level / 2.55)/100
		Light24.intensityScale = (Level / 2.55)/100
		Light24b.intensityScale = (Level / 2.55)/100
		PLAYFIELD_flasher1_r.Opacity = Level
		Light12.State = 1
		Light24.State = 1
		Light24b.State = 1
		PrimCase1 = 1
		Prim1.Enabled = 1
	Else
		Light12.State = 0
		Light24.State = 0
		Light24b.State = 0
		PLAYFIELD_flasher1_r.Opacity = 0
		PrimCase1 = 4
		Prim1.Enabled = 1
	End If
End Sub

Sub ThePowerFlasher(Level)
	If Level > 0 Then
		ThePower.intensityScale = (Level / 2.55)/100
		ThePowerb.intensityScale = (Level / 2.55)/100
		pThePower.blenddisablelighting = 700 * Level / 255
		ThePower.State = 1
		ThePowerb.State = 1
	Else
		pThePower.blenddisablelighting = 0
		ThePower.State = 0
		ThePowerb.State = 0
	End If
End Sub

Sub TrainFlasher(level)
	If Level > 0 Then
		Light3.intensityScale = (Level / 2.55)/100
			Light3c.intensityScale = (Level / 2.55)/100
		Light35.intensityScale = (Level / 2.55)/100
		PLAYFIELD_flasher2_r.Opacity = Level
		PLAYFIELD_flasher4_w.Opacity = Level
		Light3.State = 1
		Light3c.State = 1
		Light35.State = 1
		PrimCase2 = 1
		Prim2.Enabled = 1
	Else
		Light3.State = 0
		Light3c.State = 0
		Light35.State = 0
		PLAYFIELD_flasher2_r.Opacity = 0
		PLAYFIELD_flasher4_w.Opacity = 0
		PrimCase2 = 4
		Prim2.Enabled = 1
	End If
End Sub

Sub TelephoneFlasher(level)
	If Level > 0 Then
		If oPhone = 0 Then
			Select Case Int(Rnd()*4)  'AXS Phone Mod Animation
			  Case 0
				PhoneMod_9.Playanim  0,.1
			  Case 1
				PhoneMod_9.Playanim  0,.07
			  Case 2
				PhoneMod_9.Playanim  0,0
			  Case 3
				PhoneMod_9.Playanim  0,0
			End Select
		End If
		Light18.intensityScale = (Level / 2.55)/100
		Light5.intensityScale = (Level / 2.55)/100
		PLAYFIELD_flasher3_r.Opacity = Level
		PLAYFIELD_flasher5_w.Opacity = Level
		Light5.State = 1
		Light18.State = 1
		PrimCase4 = 1
		Prim4.Enabled = 1
	Else
		Light5.state = 0
		Light18.State = 0
		PLAYFIELD_flasher3_r.Opacity = 0
		PLAYFIELD_flasher5_w.Opacity = 0
		PrimCase4 = 4
		Prim4.Enabled = 1
	End if
End Sub

Sub BallKick(enabled)
	If Enabled Then
		BIP = BIP + 1
		RandomSoundBallRelease BallRelease
		BallRelease.KickZ 90, 10, 5, 110
		troughW1.isDropped = 1
		Controller.Switch(17) = 0
	End If
End Sub

Sub BallDrain(enabled)
	If Enabled Then	
		BIP = BIP - 1
		RandomSoundDrain Drain
		Drain.Kick 70, 5
	End If
End Sub

' GMJ - verify
Sub Knocker(Enabled)
	If Enabled Then
		KnockerSolenoid
	End If
End Sub

'-----------Chair---------
Sub ChairKicker_Hit : Controller.Switch(43) = 1 : SoundSaucerLock : End Sub						'Chair KickOut
Sub ChairKicker_unHit : Controller.Switch(43) = 0 : End Sub						'Chair KickOut

Sub ChairKicker_Timer
	ChairKicker.kick 0, 39, 5												'chair kicker  
	SoundSaucerKick 1, ChairKicker
	'PlaySound SoundFX("ChairKickOut",DOFContactors),0,.75,0,0.25   
	ChairKicker.TimerEnabled = 0
End Sub

'----------swamp-----------
Sub SwampRelease(Enabled)
	If Enabled Then
		SwampReleaseKicker.TimerEnabled = 1
	End If
End Sub

Sub SwampLockUp_Hit
	Debug.Print "SwampLockUp"
	SoundSaucerLock ' GMJ - Maby Remove
	Controller.Switch(74) = 1     							'LockUp Kickout
End Sub
Sub SwampLockUp_UnHit:Controller.Switch(74) = 0:End Sub 	 'LockUp Kickout

Sub SwampLockUp_Timer
	SwampLockUp.kick 22, 45, 5								'swamp kicker
	SoundSaucerKick 1, SwampLockUp
	'PlaySound SoundFX("SwampKick",DOFContactors),0,1,0,0.25
	SwampLockUp.TimerEnabled = 0
End Sub

Sub SwampReleaseKicker_Hit
	Controller.Switch(73) = 1    							'Swamp Lock Lower
	SoundSaucerLock
	Wall69.isdropped = 0
End Sub

Sub SwampReleaseKicker_Timer
	SwampReleaseKicker.kick 300, 15, 5
	SoundSaucerKick 1, SwampReleaseKicker
	'PlaySound SoundFX("SwampSol",DOFContactors),0,1,0,0.25
	Controller.Switch(73) = 0     							'Swamp Lock Lower
	Wall69.isDropped = 1
	SwampReleaseKicker.TimerEnabled = 0
End Sub

Sub SW71_Hit:Controller.Switch(71) = 1:End Sub  			'Swamp Lock Upper
Sub SW71_UnHit:Controller.Switch(71) = 0:End Sub 			'Swamp Lock Upper
Sub SW72_Hit:Controller.Switch(72) = 1:Wall70.isDropped = 0:End Sub   'Swamp Lock Middle
Sub SW72_UnHit:Controller.Switch(72) = 0:Wall70.isDropped = 1:End Sub 'Swamp Lock Middle

'--------Bumpers-----------
Sub Bumper1_Hit
	vpmTimer.PulseSw 31  									'Upper Left Jet
	If L21.State = 1 Then
		bumper_REDBASEACTIVE.image = "RGP3_redBBACTIVE_LIGHTON"
	Else
		bumper_REDBASEACTIVE.image = "RGP3_redBBACTIVE_LIGHTOFF"
	End If
	bumper_REDBASEACTIVE.visible = 1
	bumper_REDBASE.visible = 0
	RandomSoundBumperTop Bumper1
	Bumper1.TimerEnabled = 1	
End Sub

Sub Bumper1_timer
	bumper_REDBASEACTIVE.visible = 0
	bumper_REDBASE.visible = 1
	Bumper1.TimerEnabled = 0
End Sub

Sub Bumper2_Hit
	vpmTimer.PulseSw 32  									'Upper Right Jet
	If L22.State = 1 Then
		bumper_CLEARBASEACTIVE.image = "RGP3_clearBBACTIVE_LIGHTON"
	Else
		bumper_CLEARBASEACTIVE.image = "RGP3_clearBBACTIVE_LIGHTOFF"
	End If
	bumper_CLEARBASEACTIVE.visible = 1
	bumper_CLEARBASE.visible = 0
	RandomSoundBumperTop Bumper2
	Bumper2.TimerEnabled = 1
End Sub

Sub Bumper2_timer
	bumper_CLEARBASEACTIVE.visible = 0
	bumper_CLEARBASE.visible = 1
	Bumper2.TimerEnabled = 0
End Sub

Sub Bumper3_Hit
	vpmTimer.PulseSw 33 									'Center Left Jet
	If L24.State = 1 Then
		Bumper_BLUEBASEACTIVE.image = "RGP3_blueBBACTIVE_LIGHTON"
	Else
		Bumper_BLUEBASEACTIVE.image = "RGP3_blueBBACTIVE_LIGHTOFF"
	End If
	Bumper_BLUEBASEACTIVE.visible = 1
	Bumper_BLUEBASE.visible = 0
	RandomSoundBumperMiddle Bumper3
	Bumper3.TimerEnabled = 1
End Sub

Sub Bumper3_timer
	Bumper_BLUEBASEACTIVE.visible = 0
	Bumper_BLUEBASE.visible = 1
	Bumper3.TimerEnabled = 0
End Sub

Sub Bumper4_Hit
	vpmTimer.PulseSw 34 									'Center Right Jet
	If L23.State = 1 Then
		bumper_YELLOWBASEACTIVE.image = "RGP3_yellowBBACTIVE_LIGHTON"
	Else
		bumper_YELLOWBASEACTIVE.image = "RGP3_yellowBBACTIVE_LIGHTOFF"
	End If
	bumper_YELLOWBASEACTIVE.visible = 1
	bumper_YELLOWBASE.visible = 0
	RandomSoundBumperMiddle Bumper4
	Bumper4.TimerEnabled = 1
End Sub

Sub Bumper4_timer
	bumper_YELLOWBASEACTIVE.visible = 0
	bumper_YELLOWBASE.visible = 1
	Bumper4.TimerEnabled = 0
End Sub

Sub Bumper5_Hit
	vpmTimer.PulseSw 35 									'Lower Jet
	If L25.State = 1 Then
		bumper_ORANGEBASEACTIVE.image = "RGP3_orangeBBACTIVE_LIGHTON"
	Else
		bumper_ORANGEBASEACTIVE.image = "RGP3_orangeBBACTIVE_LIGHTOFF"
	End If
	bumper_ORANGEBASEACTIVE.visible = 1
	bumper_ORANGEBASE.visible = 0
	RandomSoundBumperBottom Bumper5
	Bumper5.TimerEnabled = 1
End Sub

Sub Bumper5_timer
	bumper_ORANGEBASEACTIVE.visible = 0
	bumper_ORANGEBASE.visible = 1
	Bumper5.TimerEnabled = 0
End Sub

'-------------Boookcase-----------------
Sub BookCaseMotor(aNewPos,aSpeed,aLastPos)
	DIM OBJ
 	'PlaySound SoundFX("BookCaseMotor",DOFGear)
	PlaySoundAtLevelStatic SoundFX("BookCaseMotor",DOFGear), 1, SW53
	vault_base.rotZ = aNewPos
	vault_plastic.rotZ = aNewPos
	vault_post.rotZ = aNewPos
	vault_postsback.rotZ = aNewPos
	vault_screws.rotZ = aNewPos
	vault_upright.rotZ = aNewPos
	vault_Bug.rotZ = aNewPos
	vault_base.image = ("base" & (Round(aNewPos/10))) 'change shadow on bookcase base
	if aNewPos >60 then
		for each obj in BookcaseOpen
			obj.collidable = true
		next
		for each obj in BookcaseClosed
			obj.collidable = false
		next
	else 
		for each obj in BookcaseOpen
			obj.collidable = false
		next
		for each obj in BookcaseClosed
			obj.collidable = true
		next
	end if
 End Sub

'----------Thing-----------
Sub ThingKickOut(Enabled)
	If Enabled Then
		ThingKickOutKicker.TimerEnabled = 1
	End If
End Sub

Sub ThingKickOutKicker_Timer
	ThingKickOutKicker.Kick 180, 6, 5   					'thingkickoutkicker
    Controller.switch(77) = 0								'Thing KickOut
	'Playsound "SubWayKick"
	' GMJ 
	SoundSaucerKick 1, ThingKickOutKicker
	ThingKickOutKicker.TimerEnabled = 0
End Sub

Sub ThingKickOutKicker_Hit:Controller.Switch(77) = 1:End Sub

Sub ThingEjectHole(Enabled)
	If Enabled Then
		Debug.Print "ThingEjectHole Enabled"
		ThingSaucer.KickZ 105, 9, 25, 15	 								'ThingSaucer
		SoundSaucerKick 1, ThingSaucer
		'PlaySound SoundFX("ThingSaucerKick",DOFContactors), 0, .3,.6,0
		Controller.Switch(87) = 0
		ThingBall = False
	End If
End Sub

Sub ThingSaucer_Hit
	Debug.Print "ThingSoucer_hit"
	SoundSaucerLock
	'PlaySoundAtBall "ThingSaucerHit"
	Controller.Switch(87) = 1
	ThingBall = True
End Sub

Sub ThingSaucer_Timer
	ThingSaucer.Enabled = 1
End Sub

'---Thing Hand Movement
Dim Position, HandPosition, Thingball
Dim Tball,xstart,ystart,zstart,dradius, xyangle, zangle

  Sub ThingMotor(aNewPos,aSpeed,aLastPos)
	dim BoxPosition
 	'PlaySound SoundFX("ThingMotor",DOFGear)
	PlaySoundAtLevelStatic SoundFX("ThingMotor", DOFGear), 1, ThingKickOutKicker
	position = aNewPos * 2' - 90
	If aNewPos < 36 Then
 		BoxPosition = aNewPos * 1.1
	Elseif aNewPos =< 37 Then
		BoxPosition = aNewPos 
	elseif aNewPos =< 38 then
		BoxPosition = 36.7
	elseif aNewPos =< 39 then
		BoxPosition = 36.4
	elseif aNewPos =< 40 then
		BoxPosition = 36.1
	elseif aNewPos =< 41 then
 		BoxPosition = 35.8
	elseif aNewPos =< 42 then
		BoxPosition = 35.5
	elseif aNewPos =< 43 then
		BoxPosition = 35.2
	elseif aNewPos =< 44 then
		BoxPosition = 34.9
	elseif aNewPos =< 45 then
		BoxPosition = 34.6
	elseif aNewPos =< 46 then
		BoxPosition = 34.3
	elseif aNewPos =< 47 then
		BoxPosition = 34
	elseif aNewPos =< 48 then
		BoxPosition = 33.7
	elseif aNewPos =< 49 then
		BoxPosition = 33.4
	elseif aNewPos =< 50 then
		BoxPosition = 33.1
	elseif aNewPos =< 51 then
		BoxPosition = 32.8
	elseif aNewPos =< 52 then
		BoxPosition = 32.5
	elseif aNewPos =< 53 then
		BoxPosition = 32.2
	elseif aNewPos =< 54 then
		BoxPosition = 31.9
	elseif aNewPos =< 55 then
		BoxPosition = 31.6
	elseif aNewPos =< 56 then
		BoxPosition = 31.3
	elseif aNewPos =< 57 then
		BoxPosition = 31
	elseif aNewPos =< 58 then
		BoxPosition = 30.7
	elseif aNewPos =< 59 then
		BoxPosition = 30.4
	elseif aNewPos = 60 then
		BoxPosition = 30.1
	Else
		BoxPosition = 0
	End If
	Thing.RotY =  position
	handMAGNET.RotY = Position
	thingBox.Rotx = BoxPosition '-90
	ThingBOXmods.Rotx = BoxPosition
 End Sub

dradius= 125.88
xyangle = 45
zangle = 302.69

xstart = 711.1898'dradius*cos(radians(xyangle))*Sin(Radians(zangle))    'center of pivot
ystart = 241.0114'dradius*sin(radians(xyangle))*Sin(Radians(zangle))
zstart = -68'dradius*cos(radians(zangle))

	Sub TballTimer2_timer
		Dim thingoffset
		thingoffset=72
		tball.vely=	0
		tball.velz= 0
		tball.velx= 0
		Tball.x = xstart + dradius*cos(Radians(xyangle))*Sin(Radians(thingoffset-thing.roty))
		Tball.y = ystart - dradius*sin(radians(xyangle))*Sin(Radians(thingoffset-thing.roty))
		Tball.z = zstart + dradius*cos(radians(thingoffset-thing.roty))
	End Sub

'---------Switches--------------------
'---trough
Sub Trough3_Hit: Controller.Switch(15) = 1: End Sub
Sub Trough3_unHit: Controller.Switch(15) = 0: End Sub
Sub Trough2_Hit: Controller.Switch(16) = 1:troughW2.isDropped = 0: End Sub
Sub Trough2_unHit: Controller.Switch(16) = 0:troughW2.isDropped = 1: End Sub
Sub BallRelease_Hit: Controller.Switch(17) = 1:troughW1.isDropped = 0: end Sub
Sub Drain_Hit: Controller.Switch(18) = 1 : Outhole.enabled = 0:End Sub
Sub Drain_unHit: Controller.Switch(18) = 0:Outhole.enabled = 1: End Sub
' GMJ Need to see if this needs to be reenabled
'Sub OutHole_Hit:playSound "DrainHit",0,.25,1,0.25:End Sub

'---end trough
Sub SW27_Hit:Controller.Switch(27) = 1:End Sub      'Ball Shooter
Sub SW27_UnHit:Controller.Switch(27) = 0:End Sub	'Ball Shooter
Sub SW26_hit:Controller.Switch(26) = 1:End Sub	    'Right Outlane
Sub SW26_unHit:Controller.Switch(26) = 0:End Sub	'Right Outlane
Sub SW25_Hit:Controller.Switch(25) = 1:End Sub	    'Right Flipper Lane
Sub SW25_unHit:Controller.Switch(25) = 0:End Sub  	'Right Flipper Lane
Sub SW38_Hit:Controller.Switch(38) = 1:End Sub	    'Upper Left Loop
Sub SW38_UnHit:Controller.Switch(38) = 0:End Sub	'Upper Left Loop

' Extra Sounds
Sub SwampScoopS_Hit:PlaySoundAt "SwampScoopHit", SwampScoopS :End Sub
Sub ChairKickerS_Hit:PlaySoundAt "ChairEntry", ChairKickerS :End Sub
Sub ThingRampS_Hit:PlaySoundAt "ThingRamp", ThingRampS:End Sub
Sub ThingDropS_Hit:PlaySoundAt "ThingDrop", ThingDropS :End Sub
Sub SwampTrigger_hit:PlaysoundAtBall "SwampHit":End Sub
Sub Ramp55_Hit:PlaySoundAtBall "ThingRampHit2" :End Sub
Sub Trigger1_hit:PlaysoundAt "ThingRampHit", Trigger1:End Sub
Sub VaultTrigger_hit
	PlaySoundAtBall "VaultHit"
	PlaySoundAtBall "VaultHitAXS"
End Sub
Sub LaunchRampS_Hit:PlaySoundAt "LaunchRamp", LaunchRampS :End Sub
Sub JPRampS_Hit:PlaySoundAt "JPRampHit", JPRampS :End Sub



'-----------------Targets-------------------
Sub SW41_Hit 'Grave "G"
	STHit 41
	'vpmTimer.PulseSw 41:RGP7_ANIM_target1.playanim 4,.6         '(Frame0-10,Speed) 
End Sub

Sub SW42_Hit 'Grave "R"
	STHit 42
	'vpmTimer.PulseSw 42:RGP7_ANIM_target2.playanim 4,.6         '(Frame0-10,Speed)
End Sub

Sub SW44a1_Hit										'Cousin It (a1)
	'If RGP7_ANIM_target5.ObjRotY = 0 Then
		STHit 4411
		'vpmTimer.PulseSw 44:RGP7_ANIM_target5.ObjRotY = -0.35:Me.TimerEnabled = 1
		If oCousinIt = 0 Then
			CousinITT_5.playanim 0,.1 'Wave 
		End if
	'End If
End Sub

Sub SW44a1_Timer:RGP7_ANIM_target5.ObjRotY = 0:Me.TimerEnabled = 0:End Sub

Sub SW44a2_Hit										'Cousin It (a1)
	'If RGP7_ANIM_target3.ObjRotY = 0 Then
		STHit 4412
		'vpmTimer.PulseSw 44:RGP7_ANIM_target3.ObjRotY = -0.35:Me.TimerEnabled = 1
		If oCousinIt = 0 Then
			CousinITT_5.playanim 0,.13 'Wave 
		End If
	'End If
End Sub

Sub SW44a2_Timer:RGP7_ANIM_target3.ObjRotY = 0:Me.TimerEnabled = 0:End Sub

Sub SW44b1_Hit										'Cousin It (b1)
	'If RGP7_ANIM_target6.ObjRotY = 0 Then
		STHit 4421
		'vpmTimer.PulseSw 44:RGP7_ANIM_target6.ObjRotY = -0.35:Me.TimerEnabled = 1
        If oCousinIt = 0 Then
			CousinITT_5.playanim 0,.17 'Wave 
		End If
	'End If
End Sub

Sub SW44b1_Timer:RGP7_ANIM_target6.ObjRotY = 0:Me.TimerEnabled = 0:End Sub

Sub SW44b2_Hit										'Cousin It (b2)
	'If RGP7_ANIM_target4.ObjRotY = 0 Then
		STHit 4422
		'vpmTimer.PulseSw 44:RGP7_ANIM_target4.ObjRotY = -0.35:Me.TimerEnabled = 1
		If oCousinIt = 0 Then
			CousinITT_5.playanim 0,.2 'Wave
		End If
	'End If
End Sub

Sub SW44b2_Timer:RGP7_ANIM_target4.ObjRotY = 0:Me.TimerEnabled = 0:End Sub

Sub SW45_Hit										'Lower Swamp Million
	'If RGP7_ANIM_target11.ObjRotY = 0 Then
		STHit 45
		'vpmTimer.PulseSw 45:RGP7_ANIM_target11.ObjRotY = 0.35:Me.TimerEnabled = 1
	'End If
End Sub

Sub SW45_Timer:RGP7_ANIM_target11.ObjRotY = 0:Me.TimerEnabled = 0:End Sub

Sub SW47_Hit										'Center Swamp Million
	'If RGP7_ANIM_target10.ObjRotY = 0 Then
		STHit 47
		'vpmTimer.PulseSw 47:RGP7_ANIM_target10.ObjRotY = 0.35:Me.TimerEnabled = 1
	'End If
End Sub

Sub SW47_Timer:RGP7_ANIM_target10.ObjRotY = 0:Me.TimerEnabled = 0:End Sub

Sub SW48_Hit										'Upper Swamp Million
	'If RGP7_ANIM_target9.ObjRotY = 0 Then	
		STHit 48
		'vpmTimer.PulseSw 48:RGP7_ANIM_target9.ObjRotY = 0.35:Me.TimerEnabled = 1
	'End If
End Sub

Sub SW48_Timer:RGP7_ANIM_target9.ObjRotY = 0:Me.TimerEnabled = 0:End Sub

Sub SW51_Hit:VPMTimer.PulseSw 51:End Sub			'Shooter Lane
Sub SW53_Hit:vpmTimer.PulseSw 53:End Sub			'Bookcase Opto 1
Sub SW54_Hit:vpmTimer.PulseSw 54:End Sub			'Bookcase Opto 2
Sub SW55_Hit:vpmTimer.PulseSw 55:End Sub			'Bookcase Opto 3
Sub SW56_Hit:vpmTimer.PulseSw 56:End Sub			'Bookcase Opto 4
Sub SW57_Hit:Controller.Switch(57) = 1:End Sub 		'Bumper Lane Opto
Sub SW57_unHit:Controller.Switch(57) = 0:End Sub	'Bumper Lane Opto
Sub SW58_Hit
	vpmTimer.PulseSw 58			                    'Right Ramp Exit
	SwitchLever2.RotZ = 80
	me.timerenabled = 1
End Sub

Sub SW58_timer
	SwitchLever2.Rotz = 95
	me.timerenabled = 0
End Sub

Sub SW61_Hit:vpmTimer.PulseSw 61:End Sub			'Left Ramp Enter

Sub SW62_Hit	                                                    'Train Wreck								
	STHit 62
	'If RGP7_ANIM_target1.ObjRotX = 0 Then	
	'	vpmTimer.PulseSw 62:RGP7_ANIM_target7.ObjRotX = 0.35:Me.TimerEnabled = 1           
	'End If
End Sub

Sub SW62_Timer:RGP7_ANIM_target7.ObjRotX = 0:Me.TimerEnabled = 0:End Sub

Sub SW63_hit:Controller.Switch(63) = 1:End Sub	    'Thing Eject Lane
Sub SW63_unHit:Controller.Switch(63) = 0:End Sub	'Thing Eject Lane
Sub SW64_Hit:vpmTimer.PulseSw 64:End Sub			'Right Ramp Enter
Sub SW65_Hit:vpmTimer.PulseSw 65			        'Right Ramp Top
	SwitchLever1.ObjRotx = 10:SwitchLever1.ObjRotZ = 10
	Me.TimerEnabled = 1
End Sub

Sub SW65_Timer
	SwitchLever1.ObjRotx = 0:SwitchLever1.ObjRotz = 0
	me.timerEnabled = 0
End Sub

Sub SW66_Hit:vpmTimer.PulseSw 66                    'Left Ramp Top
	SwitchLever3.ObjRotx = 10:SwitchLever3.ObjRotZ = 10
	Me.TimerEnabled = 1
End Sub

Sub SW66_Timer
	SwitchLever3.ObjRotx = 0:SwitchLever3.ObjRotz = 0
	me.timerEnabled = 0
End Sub		
	
Sub SW67_hit:Controller.Switch(67) = 1:End Sub	    'Upper Right Loop
Sub SW67_unHit:Controller.Switch(67) = 0:End Sub	'Upper Right Loop
Sub SW68_Hit:vpmTimer.PulseSw 68:End Sub			'Vault
Sub SW75_hit:Controller.Switch(75) = 1:End Sub	    'Left Outlane
Sub SW75_unHit:Controller.Switch(75) = 0:End Sub	'Left Outlane
Sub SW76_hit:Controller.Switch(76) = 1:End Sub	    'Left Flipper Lane 2
Sub SW76_unHit:Controller.Switch(76) = 0:End Sub	'Left Flipper Lane 2
Sub SW78_hit:Controller.Switch(78) = 1:End Sub	    'Left Flipper Lane 1
Sub SW78_unHit:Controller.Switch(78) = 0:End Sub	'Left Flipper Lane 1

Sub SW86_Hit										'Grave "A"
	STHit 86
	'vpmTimer.PulseSw 86:RGP7_ANIM_target8.playanim 0,.5       '(Frame0-10,Speed)
End Sub

'-------------Flasher prim. image changes----------
Sub Prim3_Timer()
   Select Case PrimCase3
		Case 1:gRGP6_WhiteFlasher3.Image = "rgp6_flasherwhite3_on_on":BladeRightDTON.image = "bladeRIGHT_giON_FrFlasher":gRGP5_habittrail_metrampsRight.Image = "rgp5_railsmetals_LowerFlasher"
				RGP5_rampPlates.image = "RGP5_rampplatesfix_Flasher":RGP8_GI2_2.image = "RGP8_GI2_2_FlasherON"
				PrimCase3 = 2' 
		Case 2:gRGP6_WhiteFlasher3.Image = "rgp6_flasherwhite3_on_on":BladeRightDTON.image = "bladeRIGHT_giON_FrFlasher":gRGP5_habittrail_metrampsRight.Image = "rgp5_railsmetals_LowerFlasher"
				RGP5_rampPlates.image = "RGP5_rampplatesfix_Flasher":RGP8_GI2_2.image = "RGP8_GI2_2_FlasherON"
				PrimCase3 = 3'
		Case 3:gRGP6_WhiteFlasher3.Image = "rgp6_flasherwhite3_on_on":BladeRightDTON.image = "bladeRIGHT_giON_FrFlasher":gRGP5_habittrail_metrampsRight.Image = "rgp5_railsmetals_LowerFlasher"
				RGP5_rampPlates.image = "RGP5_rampplatesfix_Flasher":RGP8_GI2_2.image = "RGP8_GI2_2_FlasherON"
				Me.Enabled = 0
		Case 4:gRGP6_WhiteFlasher3.Image = "rgp6_flasherwhite3_on_off":BladeRightDTON.image = "bladeRIGHT_giON":gRGP5_habittrail_metrampsRight.Image = "rgp5_railsmetals_GION"
				RGP5_rampPlates.image = "RGP5_rampplatesfix_GION":RGP8_GI2_2.image = "RGP8_GI2_2_GION"
				PrimCase3 = 5'
		Case 5:gRGP6_WhiteFlasher3.Image = "rgp6_flasherwhite3_on_off":BladeRightDTON.image = "bladeRIGHT_giON":gRGP5_habittrail_metrampsRight.Image = "rgp5_railsmetals_GION"
				RGP5_rampPlates.image = "RGP5_rampplatesfix_GION":RGP8_GI2_2.image = "RGP8_GI2_2_GION"
				PrimCase3 = 6'
		Case 6:gRGP6_WhiteFlasher3.Image = "rgp6_flasherwhite3_on_off":BladeRightDTON.image = "bladeRIGHT_giON":gRGP5_habittrail_metrampsRight.Image = "rgp5_railsmetals_GION"
				RGP5_rampPlates.image = "RGP5_rampplatesfix_GION":RGP8_GI2_2.image = "RGP8_GI2_2_GION"
				Me.Enabled = 0'
   End Select 
End Sub

Dim Prim7On

Sub Prim7_Timer()
   Select Case PrimCase7
		Case 1:gRGP2_plastic.Image = "RGP2_gion_flon":BladeLeftDTON.image = "bladeLEFT_giON_FrLFlash"
				If L64.State = 0 and L47.State = 0 Then
					ChairPrim.image = "chair_LeftFlasher"
				ElseIf L64.State = 1 and L47.State = 1 Then
					ChairPrim.image = "chair_LeftFlasBulbLR"
				ElseIF L64.State = 1 and L47.State = 0 Then
					ChairPrim.image = "Chair_LeftFlasBulbL"
				ElseIf L64.State = 0 and L47.State = 1 Then
					ChairPrim.image = "Chair_LeftFlasBulbR"
				End If
				Prim7On = 1
				PrimCase7 = 2
		Case 2:gRGP2_plastic.Image = "RGP2_gion_flon":BladeLeftDTON.image = "bladeLEFT_giON_FrLFlash"
				If L64.State = 0 and L47.State = 0 Then
					ChairPrim.image = "chair_LeftFlasher"
				ElseIf L64.State = 1 and L47.State = 1 Then
					ChairPrim.image = "chair_LeftFlasBulbLR"
				ElseIF L64.State = 1 and L47.State = 0 Then
					ChairPrim.image = "Chair_LeftFlasBulbL"
				ElseIf L64.State = 0 and L47.State = 1 Then
					ChairPrim.image = "Chair_LeftFlasBulbR"
				End If
				Prim7On = 1
				PrimCase7 = 3'
		Case 3:gRGP2_plastic.Image = "RGP2_gion_flon":BladeLeftDTON.image = "bladeLEFT_giON_FrLFlash"
				If L64.State = 0 and L47.State = 0 Then
					ChairPrim.image = "chair_LeftFlasher"
				ElseIf L64.State = 1 and L47.State = 1 Then
					ChairPrim.image = "chair_LeftFlasBulbLR"
				ElseIF L64.State = 1 and L47.State = 0 Then
					ChairPrim.image = "Chair_LeftFlasBulbL"
				ElseIf L64.State = 0 and L47.State = 1 Then
					ChairPrim.image = "Chair_LeftFlasBulbR"
				End If
				Prim7On = 1
				me.Enabled = 0
		Case 4:gRGP2_plastic.Image = "RGP2_gion":BladeLeftDTON.image = "bladeLEFT_giON"
				If L64.State = 0 and L47.State = 0 Then
					ChairPrim.image = "chair_GION"
				ElseIf L64.State = 1 and L47.State = 1 Then
					ChairPrim.image = "chair_ColouredBulbLR"
				ElseIF L64.State = 1 and L47.State = 0 Then
					ChairPrim.image = "Chair_ColouredBulbL"
				ElseIf L64.State = 0 and L47.State = 1 Then
					ChairPrim.image = "Chair_ColouredBulbR"
				End If
				Prim7On = 0
				PrimCase7 = 5'
		Case 5:gRGP2_plastic.Image = "RGP2_gion":BladeLeftDTON.image = "bladeLEFT_giON"
				If L64.State = 0 and L47.State = 0 Then
					ChairPrim.image = "chair_GION"
				ElseIf L64.State = 1 and L47.State = 1 Then
					ChairPrim.image = "chair_ColouredBulbLR"
				ElseIF L64.State = 1 and L47.State = 0 Then
					ChairPrim.image = "Chair_ColouredBulbL"
				ElseIf L64.State = 0 and L47.State = 1 Then
					ChairPrim.image = "Chair_ColouredBulbR"
				End If
				Prim7On = 0
				PrimCase7 = 6'
		Case 6:gRGP2_plastic.Image = "RGP2_gion":BladeLeftDTON.image = "bladeLEFT_giON"
				If L64.State = 0 and L47.State = 0 Then
					ChairPrim.image = "chair_GION"
				ElseIf L64.State = 1 and L47.State = 1 Then
					ChairPrim.image = "chair_ColouredBulbLR"
				ElseIF L64.State = 1 and L47.State = 0 Then
					ChairPrim.image = "Chair_ColouredBulbL"
				ElseIf L64.State = 0 and L47.State = 1 Then
					ChairPrim.image = "Chair_ColouredBulbR"
				End If
				Prim7On = 0
				Me.Enabled = 0'
   End Select
End Sub

Sub Prim1_Timer()
   Select Case PrimCase1
		Case 1:gRGP6_RedFlasher1.Image = "rgp6_flasherred1_on_on":gRGP5_habittrail_metrampsLeft.Image = "rgp5_railsmetals_RedFlasher":RGP9_GI1_5.Image = "RGP9_GI1_5_GION_FlaREDON":PrimCase1 = 2'
		Case 2:gRGP6_RedFlasher1.Image = "rgp6_flasherred1_on_on":gRGP5_habittrail_metrampsLeft.Image = "rgp5_railsmetals_RedFlasher":RGP9_GI1_5.Image = "RGP9_GI1_5_GION_FlaREDON":PrimCase1 = 3'
		Case 3:gRGP6_RedFlasher1.Image = "rgp6_flasherred1_on_on":gRGP5_habittrail_metrampsLeft.Image = "rgp5_railsmetals_RedFlasher":RGP9_GI1_5.Image = "RGP9_GI1_5_GION_FlaREDON":Me.Enabled = 0
		Case 4:gRGP6_RedFlasher1.Image = "rgp6_flasherred1_on_off":gRGP5_habittrail_metrampsLeft.Image = "rgp5_railsmetals_GION":RGP9_GI1_5.Image = "RGP9_GI1_5_GION":PrimCase1 = 5'
		Case 5:gRGP6_RedFlasher1.Image = "rgp6_flasherred1_on_off":gRGP5_habittrail_metrampsLeft.Image = "rgp5_railsmetals_GION":RGP9_GI1_5.Image = "RGP9_GI1_5_GION":PrimCase1 = 6'
		Case 6:gRGP6_RedFlasher1.Image = "rgp6_flasherred1_on_off":gRGP5_habittrail_metrampsLeft.Image = "rgp5_railsmetals_GION":RGP9_GI1_5.Image = "RGP9_GI1_5_GION":Me.Enabled = 0'
   End Select
End Sub

' G added
' TrainFlasher

Sub Prim2_Timer()
   Select Case PrimCase2
		Case 1:gRGP6_RedFlasher2.Image = "rgp6_flasherred2_on_on":BladeLeftDTON.image = "bladeLEFT_giON_RrLFlash":gRGP6_WhiteFlasher1.Image = "rgp6_flasherwhite1_on_on":RGP9_GI1_3.Image = "RGP9_GI1_3_FlasherWHITEON":RGP9_GI1_4.Image = "RGP9_GI1_4_FlasherWHITEON":RGP9_GI1_5.Image = "RGP9_GI1_5_GION_FlaWHITEON":PrimCase2 = 2' 
		Case 2:gRGP6_RedFlasher2.Image = "rgp6_flasherred2_on_on":BladeLeftDTON.image = "bladeLEFT_giON_RrLFlash":gRGP6_WhiteFlasher1.Image = "rgp6_flasherwhite1_on_on":RGP9_GI1_3.Image = "RGP9_GI1_3_FlasherWHITEON":RGP9_GI1_4.Image = "RGP9_GI1_4_FlasherWHITEON":RGP9_GI1_5.Image = "RGP9_GI1_5_GION_FlaWHITEON":PrimCase2 = 3'
		Case 3:gRGP6_RedFlasher2.Image = "rgp6_flasherred2_on_on":BladeLeftDTON.image = "bladeLEFT_giON_RrLFlash":gRGP6_WhiteFlasher1.Image = "rgp6_flasherwhite1_on_on":RGP9_GI1_3.Image = "RGP9_GI1_3_FlasherWHITEON":RGP9_GI1_4.Image = "RGP9_GI1_4_FlasherWHITEON":RGP9_GI1_5.Image = "RGP9_GI1_5_GION_FlaWHITEON":Me.Enabled = 0
		Case 4:gRGP6_RedFlasher2.Image = "rgp6_flasherred2_on_off":BladeLeftDTON.image = "bladeLEFT_giON":gRGP6_WhiteFlasher1.Image = "rgp6_flasherwhite1_on_off":RGP9_GI1_3.Image = "RGP9_GI1_3_GION":RGP9_GI1_4.Image = "RGP9_GI1_4_GION":RGP9_GI1_5.Image = "RGP9_GI1_5_GION":PrimCase2 = 5'
		Case 5:gRGP6_RedFlasher2.Image = "rgp6_flasherred2_on_off":BladeLeftDTON.image = "bladeLEFT_giON":gRGP6_WhiteFlasher1.Image = "rgp6_flasherwhite1_on_off":RGP9_GI1_3.Image = "RGP9_GI1_3_GION":RGP9_GI1_4.Image = "RGP9_GI1_4_GION":RGP9_GI1_5.Image = "RGP9_GI1_5_GION":PrimCase2 = 6'
		Case 6:gRGP6_RedFlasher2.Image = "rgp6_flasherred2_on_off":BladeLeftDTON.image = "bladeLEFT_giON":gRGP6_WhiteFlasher1.Image = "rgp6_flasherwhite1_on_off":RGP9_GI1_3.Image = "RGP9_GI1_3_GION":RGP9_GI1_4.Image = "RGP9_GI1_4_GION":RGP9_GI1_5.Image = "RGP9_GI1_5_GION":Me.Enabled = 0'

   End Select 
End Sub

'TelephoneFlasher

Sub Prim4_Timer()
   Select Case PrimCase4
		Case 1:gRGP6_RedFlasher3.Image = "rgp6_flasherred3_on_on":gRGP6_WhiteFlasher2.Image = "rgp6_flasherwhite2_on_on":BladeRightDTON.image = "bladeRIGHT_giON_RrFlasher":RGP8_GI2_3.image = "RGP8_GI2_3_FlasherON":RGP8_GI2_4.image = "RGP8_GI2_4_FlasherON":RGP8_B_GI2.image = "RGP8B_GI2_FlasherON":PrimCase4 = 2' 
		Case 2:gRGP6_RedFlasher3.Image = "rgp6_flasherred3_on_on":gRGP6_WhiteFlasher2.Image = "rgp6_flasherwhite2_on_on":BladeRightDTON.image = "bladeRIGHT_giON_RrFlasher":RGP8_GI2_3.image = "RGP8_GI2_3_FlasherON":RGP8_GI2_4.image = "RGP8_GI2_4_FlasherON":RGP8_B_GI2.image = "RGP8B_GI2_FlasherON":PrimCase4 = 3'
		Case 3:gRGP6_RedFlasher3.Image = "rgp6_flasherred3_on_on":gRGP6_WhiteFlasher2.Image = "rgp6_flasherwhite2_on_on":BladeRightDTON.image = "bladeRIGHT_giON_RrFlasher":RGP8_GI2_3.image = "RGP8_GI2_3_FlasherON":RGP8_GI2_4.image = "RGP8_GI2_4_FlasherON":RGP8_B_GI2.image = "RGP8B_GI2_FlasherON":Me.Enabled = 0
		Case 4:gRGP6_RedFlasher3.Image = "rgp6_flasherred3_on_off":gRGP6_WhiteFlasher2.Image = "rgp6_flasherwhite2_on_off":BladeRightDTON.image = "bladeRIGHT_giON":RGP8_GI2_3.image = "RGP8_GI2_3_GION":RGP8_GI2_4.image = "RGP8_GI2_4_GION":RGP8_B_GI2.image = "RGP8B_GI2_GION":PrimCase4 = 5'
		Case 5:gRGP6_RedFlasher3.Image = "rgp6_flasherred3_on_off":gRGP6_WhiteFlasher2.Image = "rgp6_flasherwhite2_on_off":BladeRightDTON.image = "bladeRIGHT_giON":RGP8_GI2_3.image = "RGP8_GI2_3_GION":RGP8_GI2_4.image = "RGP8_GI2_4_GION":RGP8_B_GI2.image = "RGP8B_GI2_GION":PrimCase4 = 6'
		Case 6:gRGP6_RedFlasher3.Image = "rgp6_flasherred3_on_off":gRGP6_WhiteFlasher2.Image = "rgp6_flasherwhite2_on_off":BladeRightDTON.image = "bladeRIGHT_giON":RGP8_GI2_3.image = "RGP8_GI2_3_GION":RGP8_GI2_4.image = "RGP8_GI2_4_GION":RGP8_B_GI2.image = "RGP8B_GI2_GION":Me.Enabled = 0'

   End Select 
End Sub

'##### 3D Primitves

Sub RLightning_Animate
	Dim f
	f = RLightning.GetInPlayIntensity / RLightning.Intensity
	pRLightning.blenddisablelighting  =  500 * f
End Sub

Sub LLightning_Animate
	Dim f
	f = LLightning.GetInPlayIntensity / LLightning.Intensity
	pLLightning.blenddisablelighting  =  500 * f
End Sub


Sub ThePower_Animate
	Dim f
	f = ThePower.GetInPlayIntensity / ThePower.Intensity
	pThePower.blenddisablelighting  =  500 * f
End Sub

Sub L11_Animate
    Dim f
    f = L11.GetInPlayIntensity / L11.Intensity
    p11.blenddisablelighting =  500 * f
End Sub

Sub L12_Animate
    Dim f
    f = L12.GetInPlayIntensity / L12.Intensity
    p12.blenddisablelighting =  500 * f
End Sub

Sub L13_Animate
    Dim f
    f = L13.GetInPlayIntensity / L13.Intensity
    p13.blenddisablelighting =  500 * f
End Sub

Sub L14_Animate
    Dim f
    f = L14.GetInPlayIntensity / L14.Intensity
    p14.blenddisablelighting =  500 * f
End Sub

Sub L15_Animate
    Dim f
    f = L15.GetInPlayIntensity / L15.Intensity
    p15.blenddisablelighting =  500 * f
End Sub

Sub L16_Animate
    Dim f
    f = L16.GetInPlayIntensity / L16.Intensity
    p16.blenddisablelighting =  500 * f
End Sub

Sub L17_Animate
    Dim f
    f = L17.GetInPlayIntensity / L17.Intensity
    p17.blenddisablelighting =  500 * f
End Sub

Sub L18_Animate
    Dim f
    f = L18.GetInPlayIntensity / L18.Intensity
    p18.blenddisablelighting =  500 * f
End Sub

Sub Light24_Animate
    Dim f
    f = Light24.GetInPlayIntensity / Light24.Intensity
    p24.blenddisablelighting =  500 * f
End Sub

Sub L26_Animate
    Dim f
    f = L26.GetInPlayIntensity / L26.Intensity
    p26.blenddisablelighting =  500 * f
End Sub

Sub L27_Animate
    Dim f
    f = L27.GetInPlayIntensity / L27.Intensity
    p27.blenddisablelighting =  500 * f
End Sub

Sub L28_Animate
    Dim f
    f = L28.GetInPlayIntensity / L28.Intensity
    p28.blenddisablelighting =  500 * f
End Sub

Sub L31_Animate
    Dim f
    f = L31.GetInPlayIntensity / L31.Intensity
    p31.blenddisablelighting =  500 * f
End Sub

Sub L32_Animate
    Dim f
    f = L32.GetInPlayIntensity / L32.Intensity
    p32.blenddisablelighting =  500 * f
End Sub

Sub L33_Animate
    Dim f
    f = L33.GetInPlayIntensity / L33.Intensity
    p33.blenddisablelighting =  500 * f
End Sub


Sub L34_Animate
    Dim f
    f = L34.GetInPlayIntensity / L34.Intensity
    p34.blenddisablelighting =  500 * f
End Sub

Sub L35_Animate
    Dim f
    f = L35.GetInPlayIntensity / L35.Intensity
    p35.blenddisablelighting =  500 * f
End Sub

Sub L36_Animate
    Dim f
    f = L36.GetInPlayIntensity / L36.Intensity
    p36.blenddisablelighting =  500 * f
End Sub

Sub L37_Animate
    Dim f
    f = L37.GetInPlayIntensity / L37.Intensity
    p37.blenddisablelighting =  500 * f
End Sub

Sub L38_Animate
    Dim f
    f = L38.GetInPlayIntensity / L38.Intensity
    p38.blenddisablelighting =  500 * f
End Sub

Sub L42_Animate
    Dim f
    f = L42.GetInPlayIntensity / L42.Intensity
    p42.blenddisablelighting =  500 * f
End Sub

Sub L43_Animate
    Dim f
    f = L43.GetInPlayIntensity / L43.Intensity
    p43.blenddisablelighting =  500 * f
End Sub

Sub L44_Animate
    Dim f
    f = L44.GetInPlayIntensity / L44.Intensity
    p44.blenddisablelighting =  500 * f
End Sub

Sub L45old_Animate
    Dim f
    f = L45old.GetInPlayIntensity / L45old.Intensity
    p45.blenddisablelighting =  500 * f
End Sub

Sub L46_Animate
    Dim f
    f = L46.GetInPlayIntensity / L46.Intensity
    p46.blenddisablelighting =  500 * f
End Sub

Sub L48_Animate
    Dim f
    f = L48.GetInPlayIntensity / L48.Intensity
    p48.blenddisablelighting =  500 * f
End Sub

Sub L51_Animate
    Dim f
    f = L51.GetInPlayIntensity / L51.Intensity
    p51.blenddisablelighting =  500 * f
End Sub

Sub L52_Animate
    Dim f
    f = L52.GetInPlayIntensity / L52.Intensity
    p52.blenddisablelighting =  500 * f
End Sub

Sub L53_Animate
    Dim f
    f = L53.GetInPlayIntensity / L53.Intensity
    p53.blenddisablelighting =  500 * f
End Sub

Sub L54_Animate
    Dim f
    f = L54.GetInPlayIntensity / L54.Intensity
    p54.blenddisablelighting =  500 * f
End Sub

Sub L55_Animate
    Dim f
    f = L55.GetInPlayIntensity / L55.Intensity
    p55.blenddisablelighting =  500 * f
End Sub

Sub L56_Animate
    Dim f
    f = L56.GetInPlayIntensity / L56.Intensity
    p56.blenddisablelighting =  500 * f
End Sub

Sub L57_Animate
    Dim f
    f = L57.GetInPlayIntensity / L57.Intensity
    p57.blenddisablelighting =  500 * f
End Sub

Sub L58_Animate
    Dim f
    f = L58.GetInPlayIntensity / L58.Intensity
    p58.blenddisablelighting =  500 * f
End Sub

Sub L61_Animate
    Dim f
    f = L61.GetInPlayIntensity / L61.Intensity
    p61.blenddisablelighting =  400 * f
End Sub

Sub L62_Animate
    Dim f
    f = L62.GetInPlayIntensity / L62.Intensity
    p62.blenddisablelighting =  500 * f
End Sub

Sub L63_Animate
    Dim f
    f = L63.GetInPlayIntensity / L63.Intensity
    p63.blenddisablelighting =  500 * f
End Sub

Sub L65_Animate
    Dim f
    f = L65.GetInPlayIntensity / L65.Intensity
    p65.blenddisablelighting =  500 * f
End Sub

Sub L66_Animate
    Dim f
    f = L66.GetInPlayIntensity / L66.Intensity
    p66.blenddisablelighting =  500 * f
End Sub

Sub L68_Animate
    Dim f
    f = L68.GetInPlayIntensity / L68.Intensity
    p68.blenddisablelighting =  500 * f
End Sub

Sub L67_Animate
    Dim f
    f = L67.GetInPlayIntensity / L67.Intensity
    p67.blenddisablelighting =  500 * f
End Sub

Sub L71_Animate
    Dim f
    f = L71.GetInPlayIntensity / L71.Intensity
    p71.blenddisablelighting =  500 * f
End Sub

Sub L72_Animate
    Dim f
    f = L72.GetInPlayIntensity / L72.Intensity
    p72.blenddisablelighting =  500 * f
End Sub

Sub L73_Animate
    Dim f
    f = L73.GetInPlayIntensity / L73.Intensity
    p73.blenddisablelighting =  500 * f
End Sub

'----------------GI Strings---------
Sub UpdateGI(giNo, status)
Dim ii

   Select Case giNo
      Case 0  'Left String
		
		if status <= 2 then 
			gRGP4_clearrampREFRACT.image = "rgp4_clearramp_refract_GION":gRGP4_clearrampREFRACT.material = "GIMATERIALShading1"
			LGION = 0
			DOF 101, DOFOff
				else 
			LGION = 1
			DOF 101, DOFOn
			gRGP4_clearrampREFRACT.image = "rgp4_clearramp_refract_GION":gRGP4_clearrampREFRACT.material = ("GIMATERIALShading" & (status))
		End If 
		for each ii in GILeftString
			ii.state = abs(status):intensity ii.state, ii:PLAYFIELD_GI1.opacity = (status) * 14.5:Next
		for each ii in GILeftPrims
			ii.material = ("GIShading" & (status)):Next
'     Case 1  'Insert House String (backbox)
'     Case 2  'Insert People String (backbox)
'     Case 3  'not used
      Case 4  'Right String
		if status <= 2 then 
			RGION = 0
				else 
			RGION = 1
		End If 

		for each ii in GIRightString
			ii.state = abs(status):intensity ii.state, ii:PLAYFIELD_GI2.opacity = (status) * 14.5:Next
		for each ii in GIRightPrims
			ii.material = ("GIShading" & (status))
        Next
	End Select

	' Wait until the table turns on the GI before dropping balls into trough.  Prevents ball missing error on slow systems.
	if DoInit Then	
		DoInit = False
		InitTimer.enabled = True
	end if
End Sub

Dim GILevel
	If DayNight > 10 Then
	GILevel = (Round((1/DayNight),2))*12
	Else
	GILevel = 1
	End If

Sub Intensity(nr, Object)
		If nr = 2 Then
			object.State = 1
		End If
		Object.IntensityScale = nr * (GILevel/3)
End Sub

'RustyCardores/DJRobX bump sounds on ramps.
Dim NextOrbitHit:NextOrbitHit = 0

Sub PlasticRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 2, Pitch(ActiveBall)
		NextOrbitHit = Timer + .1 + (Rnd * .2)
	end if 
End Sub

Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "RampHit" & CStr(Int(Rnd*5)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

'---------------TABLE OPTIONS---------------
'REGISTERED LOCATIONS ***************************************************************************************************************************************
 
 Const optOpenAtStart	= 1
 Const optBear			= 6
 Const optFester		= 8
 Const optBox			= 3088
 Const optCousinIt		= 64
 Const optPhone			= 128
 Const optBetas			= 256
 Const optSwampLight	= 512
 Const optChairLight	= 32768
 Const optTrain			= 65536
 Const optPackard		= 4096
 Const optBlades		= 8192
 Const optFlippers		= 16384

'OPTIONS MENU *********************************************************************************************************************************************

 Dim TableOptions, TableOptions2, TableName
 Private vpmShowDips1, vpmDips1, vpmDips2

DefaultOptions = 1*optBear+1*optFester+1*optBox+1*optCousinIt+1*optPhone+1*optBetas+1*optSwampLight+1*optChairLight+1*optTrain+1*optPackard+1*optBlades+1*optFlippers

 Sub InitializeOptions
	TableName="TAF_VPX_V2"	'Replace with your descriptive table name, it will be used to save settings in VPReg.stg file
	Set vpmShowDips1 = vpmShowDips								'Reassigns vpmShowDips to vpmShowDips1 to allow usage of default dips menu
	Set vpmShowDips = GetRef("TableShowDips")					'Assigns new sub to vmpShowDips
 	TableOptions = LoadValue(TableName,"Options")				'Load saved table options
' 	TableOptions2 = LoadValue(TableName,"Options2")				'Load saved table options
	Set Controller = CreateObject("VPinMAME.Controller")		'Load vpm controller temporarily so options menu can be loaded if needed
'	If TableOptions2 = "" Then TableOptions2 = 0
	If TableOptions = "" Or optReset Then						'If no existing options, reset to default through optReset, then open Options menu
		TableOptions = DefaultOptions							'clear any existing settings and set table options to default options
		TableShowOptions
	ElseIf (TableOptions And optOpenAtStart) Then				'If Enable Next Start was selected then
		TableOptions = TableOptions - optOpenAtStart			'clear setting to avoid future executions
		TableShowOptions
	Else
		TableSetOptions
	End If
'	TableSetOptions2
	Set Controller = Nothing									'Unload vpm controller so selected controller can be loaded
 End Sub
 
 Private Sub TableShowDips
	vpmShowDips1												'Show original Dips menu
	TableShowOptions											'Show new options menu
	'TableShowOptions2											'Add more options menus...
 End Sub

 Private Sub TableShowOptions					'New options menu, additional menus can be added as well, just follow similar format and add call to TableShowDips
   Dim oldOptions : oldOptions = TableOptions
	If Not IsObject(vpmDips1) Then				'If creating an additional menus, need to declare additional vpmDips variables above (ex. vpmDips2 and TableOptions2, etc.)
		Set vpmDips1 = New cvpmDips
		With vpmDips1
			.AddForm 1000, 500, "TABLE OPTIONS MENU"
			.AddFrameExtra 0,0,155,"Bear Toy",optBear, Array("Rug", 0*2, "Head", 1*2, "None", 2*2)
			.AddFrameExtra 0,65,155,"Chair Options",optFester, Array("with Uncle Fester", 0, "No Fester", 8)
			.AddFrameExtra 0,115,155,"Thing Box",optBox, Array("TiltGraphics Mod", 1*16, "Red MezelMod Design", 1*1024, "Red Leather", 1*2048, "Default", 0*16)
			.AddFrameExtra 0,190,155,"Coustin It",optCousinIt, Array("Enabled", 0, "Disabled", 64)
			.AddFrameExtra 0,240,155, "Phone Toy", optPhone, Array("Enabled", 0, "Disabled", 128)	
			.AddFrameExtra 0,290,155,"Beta Plastics", optBetas, Array("Show Beta Plastics", 0, "No Beta Plastics", 256)
			.AddFrameExtra 175,0,155,"Swamp Lighting", optSwampLight, Array("Enable Green Light", 0, "Disabled", 512)
			.AddFrameExtra 175,50,155,"Chair Scoop Light", optChairLight, Array("Enable Light", 0, "Disabled", 32768)
			.AddFrameExtra 175,100,155,"Train Toy",optTrain, Array("Enabled", 0, "Disabled", 65536)
			.addFrameExtra 175,150,155,"Packard Toy",optPackard, Array("Enabled", 0, "Disabled", 4096)
			.addFrameExtra 175,200,155,"Mirror Blades",optBlades, Array("Enabled", 0, "Disabled", 8192)
			.addFrameExtra 175,250,155,"Flipper Rubber Color",optFlippers, Array("Red", 0, "Black", 16384)
			.AddLabel 0,350,175,30,"* Restart To Apply Settings"
			.AddChkExtra 180,350,135, Array("Enable Menu Next Start", optOpenAtStart)
		End With
	End If
	TableOptions = vpmDips1.ViewDipsExtra(TableOptions)
	SaveValue TableName,"Options",TableOptions
	TableSetOptions
 End Sub

 Dim oBear, oFester, oBox, oCousinIt, oPhone, oBetas, oSwampLight, oChairLight, oTrain, oPackard, oBlades, oFlippers

 Sub TableSetOptions		'defines required settings before table is run
	oBear = (TableOptions And optBear)
	oFester = (TableOptions And optFester)
	oBox = (TableOptions And optBox)
	oCousinIt = (TableOptions And optCousinIt)
	oPhone = (TableOptions And optPhone)
	oBetas = (TableOptions And optBetas)
	oSwampLight = (TableOptions And optSwampLight)
	oChairLight = (TableOptions And optChairLight)
	oTrain = (TableOptions And optTrain)
	oPackard = (TableOptions And optPackard)
	oBlades = (TableOptions And optBlades)
	oFlippers = (TableOptions And optFlippers)
	SaveValue TableName,"Options",TableOptions
 End Sub

If oBear = 0 Then
	BearRugNew.Visible = 1
	BearRugNew1.Visible = 1
	BearRugEyesNEW.Visible = 1

	BearHead.Visible = 0
	BearHeadLight1.Visible = 0
	BearHeadLight2.Visible = 0
	BearHeadEyes.Visible = 0
End If
If oBear = 2 Then
	BearRugNew.Visible = 0
	BearRugNew1.Visible = 0
	BearRugEyesNEW.Visible = 0

	BearHead.Visible = 1
	BearHeadLight1.Visible = 1
	BearHeadLight2.Visible = 1
	BearHeadEyes.Visible = 1
End If
If oBear = 4 Then
	BearRugNew.Visible = 0
	BearRugNew1.Visible = 0
	BearRugEyesNEW.Visible = 0

	BearHead.Visible = 0
	BearHeadLight1.Visible = 0
	BearHeadLight2.Visible = 0
	BearHeadEyes.Visible = 0
End If

If oFester = 0 Then
	FesterTrigger2.Enabled = 1
	FesterTrigger.Enabled = 1
	FesterBody_5.Visible = 1
	FesterHead_5.Visible = 1
	FesterBulbAXS.Visible = 1
	FesterBulbAXS1.Visible = 1
Else
	FesterTrigger2.Enabled = 0
	FesterTrigger.Enabled = 0
	FesterBody_5.Visible = 0
	FesterHead_5.Visible = 0
	FesterBulbAXS.Visible = 0
	FesterBulbAXS1.Visible = 0
End If

If oBox = 0 Then
	ThingBOXmods.visible = 0
Elseif oBox = 16 Then
	ThingBOXmods.visible = 1
	ThingBOXmods.image = "ThingBoxMod1"
Elseif oBox = 1024 Then
	ThingBOXmods.visible = 1
	ThingBOXmods.image = "ThingBoxMod2"
Elseif oBox = 2048 Then
	ThingBOXmods.visible = 1
	ThingBOXmods.image = "ThingBoxMod3"
End If

If oCousinIt = 0 Then
	CousinITT_5.Visible = 1
	CousinIttLight.Visible = 1
Else
	CousinITT_5.Visible = 0
	CousinIttLight.Visible = 0
End If

If oPhone = 0 Then
	PhoneMod_9.visible = 1
Else
	PhoneMod_9.visible = 0
End If

If oBetas = 0 Then
	BetaPlastics.visible = 1
	Beta1_2.Visible = 1
	Beta2_2.Visible = 1
	Beta4_2.Visible = 1
Else
	BetaPlastics.visible = 0
	Beta1_2.Visible = 0
	Beta2_2.Visible = 0
	Beta4_2.Visible = 0
End If

If oSwampLight = 0 Then
	SwampLight.Visible = 1
	SwampLight1.Visible = 1
	SwampLight1.Bulb = 1
Else
	SwampLight.Visible = 0
	SwampLight1.Visible = 0
	SwampLight1.Bulb = 0
End If

If oChairLight = 0 Then
	LLightning1.Visible = 1
Else
	LLightning1.Visible = 0
End If	


If oTrain = 65536 Then
	Train.visible = 0
	Light65.visible = 0
	Light66.visible = 0
	Locomotive.enabled = 0
Else
	Train.visible = 1
	Locomotive.enabled = 1
End If



 Sub Locomotive_Hit
    Cig_Smoke.visible = 1
	Cig_Smoke1.visible = 1
	Light65.visible = 1
	Light66.visible = 1
	Smoke.enabled = 1
	SmokeTimer.Enabled = 1
 End Sub

 Sub SmokeTimer_Timer
	Cig_Smoke.visible = 0
	Cig_Smoke1.visible = 0
	Light65.visible = 0
	Light66.visible = 0
	Smoke.enabled = 0
    SmokeTimer.Enabled = 0
 End Sub

Sub Smoke_Timer()
	Cig_Smoke.height = Cig_Smoke.height + 0.1
	Cig_Smoke1.height = Cig_Smoke1.height + 0.15
	If Cig_Smoke.height <= 100 Then
		Cig_smoke.opacity = Cig_smoke.opacity + 10
	End If
	If Cig_Smoke.height => 100 AND Cig_Smoke.height <= 149 Then
		Cig_smoke.opacity = Cig_smoke.opacity - 10
	End If
	If Cig_Smoke.height => 150 Then
		Cig_smoke.opacity = 0:Cig_Smoke.height = 80
	End If
	If Cig_Smoke1.height <= 90 Then
		Cig_Smoke1.opacity = Cig_Smoke1.opacity + 5
	End If
	If Cig_Smoke1.height => 90 AND Cig_Smoke1.height <= 99 Then
		Cig_smoke1.opacity = Cig_smoke1.opacity - 5
	End If
	If Cig_Smoke1.height => 100 Then
		Cig_Smoke1.opacity = 0:Cig_Smoke1.height = 80
	End If
End Sub

If oPackard = 0 Then
	HeadlightR.visible = 1
	HeadlightL.visible = 1
	LimoInterior.visible = 1
	Packard.visible = 1
Else
	HeadlightR.visible = 0
	HeadlightL.visible = 0
	LimoInterior.visible = 0
	Packard.visible = 0
End If


' oBlades See FS/DT options at the top of the ScriptsDirectory

' oFlippers set through out of the script

'************* Uncle Fester Anim *************

Sub FesterTrigger_Hit
       FesterBody_5.Playanim  0,.04 
       FesterHead_5.Playanim  0,.04
End Sub
Sub FesterTrigger2_Hit  
'    Select Case Int(Rnd()*5)
'       Case 0
          'Playsound "_FesterShock1"
       'Case 1
          PlaysoundAt "_FesterShock2", FesterTrigger2
      ' Case 2
          'Playsound "_FesterShock3"
     'End Select
	 FesterBody_5.PlayAnimEndless .95'Shock Fester
	 FesterHead_5.PlayAnimEndless .95
	 FesterBulbAXS.State = 2
	 FesterBulbAXS.Intensity = 60
	 FesterBulbAXS1.State = 2
	 FesterBulbAXS1.Intensity = 60
	 FesterTimer.Enabled = 1
End Sub
Sub FesterTimer_Timer 
'       FesterBody_5.StopAnim
'       FesterHead_5.StopAnim
       FesterBody_5.Playanim  0,.07
       FesterHead_5.Playanim  0,.07
       FesterBulbAXS.State = 0
       FesterBulbAXS.Intensity = 0
      FesterBulbAXS1.State = 0
       FesterBulbAXS1.Intensity = 0
       FesterTimer.Enabled = 0
End Sub
'********************************************


'******ThingTrainer******
Dim Shot
Shot = 0
If ThingTrainer = 1 Then
'	Thing1.enabled = 1
	Thing2.enabled = 1
	Thing3.enabled = 1
	TrainOn.Visible = 1
	Thing4.Collidable = true
	Thing5.Collidable = true
End If

Sub Thing3_hit
	Shot = Shot +1
	If Shot < 20 Then
	Thing3.destroyball
	Thing1.CreateSizedBallwithMass 25, 1.45
	Thing1.kick 180, 1
	Else
	ThingTrainer = 0
	Thing3.kick 180, 1
	TrainOn.Visible = 0
	End If
End Sub

Sub Thing2_Hit
	Thing2.Kick 10, 60
End Sub


' *****  cvpmMech replacement, all because the "Fast" parameter is not set on timer update!!   We can't get smooth reels without this.   Until this is addressed in Core.vbs, replacing here.

Class cvpmMyMech
	Public Sol1, Sol2, MType, Length, Steps, Acc, Ret
	Private mMechNo, mNextSw, mSw(), mLastPos, mLastSpeed, mCallback

	Private Sub Class_Initialize
		ReDim mSw(10)
		gNextMechNo = gNextMechNo + 1 : mMechNo = gNextMechNo : mNextSw = 0 : mLastPos = 0 : mLastSpeed = 0
		MType = 0 : Length = 0 : Steps = 0 : Acc = 0 : Ret = 0 : vpmTimer.addResetObj Me
		vpmTimer.InitTimer Thing1, True
	End Sub

	Public Sub AddSw(aSwNo, aStart, aEnd)
		mSw(mNextSw) = Array(aSwNo, aStart, aEnd, 0)
		mNextSw = mNextSw + 1
	End Sub

	Public Sub AddPulseSwNew(aSwNo, aInterval, aStart, aEnd)
		If Controller.Version >= "01200000" Then
			mSw(mNextSw) = Array(aSwNo, aStart, aEnd, aInterval)
		Else
			mSw(mNextSw) = Array(aSwNo, -aInterval, aEnd - aStart + 1, 0)
		End If
		mNextSw = mNextSw + 1
	End Sub

	Public Sub Start
		Dim sw, ii
		With Controller
			.Mech(1) = Sol1 : .Mech(2) = Sol2 : .Mech(3) = Length
			.Mech(4) = Steps : .Mech(5) = MType : .Mech(6) = Acc : .Mech(7) = Ret
			ii = 10
			For Each sw In mSw
				If IsArray(sw) Then
					.Mech(ii) = sw(0) : .Mech(ii+1) = sw(1)
					.Mech(ii+2) = sw(2) : .Mech(ii+3) = sw(3)
					ii = ii + 10
				End If
			Next
			.Mech(0) = mMechNo
		End With
		If IsObject(mCallback) Then mCallBack 0, 0, 0 : mLastPos = 0 : vpmTimer.EnableUpdate Me, True, True  ' <------- All for this.
	End Sub

	Public Property Get Position : Position = Controller.GetMech(mMechNo) : End Property
	Public Property Get Speed    : Speed = Controller.GetMech(-mMechNo)   : End Property
	Public Property Let Callback(aCallBack) : Set mCallback = aCallBack : End Property

	Public Sub Update
		Dim currPos, speed
		currPos = Controller.GetMech(mMechNo)
		speed = Controller.GetMech(-mMechNo)
		If currPos < 0 Or (mLastPos = currPos And mLastSpeed = speed) Then Exit Sub
		mCallBack currPos, speed, mLastPos : mLastPos = currPos : mLastSpeed = speed
	End Sub

	Public Sub Reset : Start : End Sub
	
End Class

Sub ramptrigger01_hit()
	WireRampOn True	 'Play Plastic Ramp Sound
	'bsRampOnClear	   'Shadow on ramp and pf below
End Sub

Sub ramptrigger01_unhit()
	If ActiveBall.VelY > 0 Then WireRampOff : End If
End Sub

Sub ramptrigger02_hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
	'bsRampOnWire	'Shadow only on pf
End Sub

Sub ramptrigger02_unhit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
End Sub

Sub ramptrigger03_hit()
	WireRampOff	 'Exiting Wire Ramp Stop Playing Sound
End Sub

Sub ramptrigger03_unhit()
	'PlaySoundAt "WireRamp_Stop", ramptrigger03
	RandomSoundWireRampStop ramptrigger03
End Sub

Sub ramptrigger04_hit()
	Debug.Print "ramptrigger04_hit"
	WireRampOn True	 'Play Plastic Ramp Sound
	'bsRampOnClear	   'Shadow on ramp and pf below
End Sub

Sub ramptrigger04_unhit()
	Debug.Print "ramptrigger04_unhit"
	If ActiveBall.VelY > 0 Then WireRampOff : End If
End Sub

Sub ramptrigger05_hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
	'bsRampOnWire	'Shadow only on pf
End Sub

Sub ramptrigger05_unhit()
	Debug.Print "unhit"
	RandomSoundRampStop ramptrigger05
End Sub

Sub ramptrigger06_hit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
End Sub

Sub ramptrigger06_unhit()
	If ActiveBall.VelY > 0 Then WireRampOff : End If
End Sub

Sub ramptrigger07_unhit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
End Sub

'****************************************************************
'	ZGII: GI
'****************************************************************

Dim gilvl   'General Illumination light state tracked for Dynamic Ball Shadows
gilvl = 1

Sub ToggleGI(Enabled)
	Dim xx
	If enabled Then
		For Each xx In GI
			xx.state = 1
		Next
		'PFShadowsGION.visible = 1
		gilvl = 1
	Else
		For Each xx In GI
			xx.state = 0
		Next
		'PFShadowsGION.visible = 0
		GITimer.enabled = True
		gilvl = 0
	End If
	Sound_GI_Relay enabled, bumper1
End Sub

Sub GITimer_Timer()
	Me.enabled = False
	ToggleGI 1
End Sub

'***************************************************************
'	ZSHA: VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx8" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* Count from 0 up, with at least as many objects each as there can be balls, including locked balls.  You'll get an "eval" warning if tnob is higher
'	* Warning:  If merging with another system (JP's ballrolling), you may need to check tnob math and add an extra BallShadowA# flasher (out of range error)
' Ensure you have a timer with a -1 interval that is always running
' Set plastic ramps DB to *less* than the ambient shadows (-11000) if you want to see the pf shadow through the ramp
' Place triggers at the start of each ramp *type* (solid, clear, wire) and one at the end if it doesn't return to the base pf
'	* These can share duties as triggers for RampRolling sounds

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
' It's recommended that you be selective in which lights go in this collection, as there are limitations:
' 1. The shadows can "pass through" solid objects and other light sources, so be mindful of where the lights would actually able to cast shadows
' 2. If there are more than two equidistant sources, the shadows can suddenly switch on and off, so places like top and bottom lanes need attention
' 3. At this time the shadows get the light on/off from tracking gilvl, so if you have lights you want shadows for that are on at different times you will need to either:
'	a) remove this restriction (shadows think lights are always On)
'	b) come up with a custom solution (see TZ example in script)
' After confirming the shadows work in general, use ball control to move around and look for any weird behavior

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
' Example timer sub:

'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
'Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top for easy access by players
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

' *** The following segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
' ** Change gBOT to BOT if using existing getballs code
' ** Double commented lines commonly found there included for reference:

''	' stop the sound of deleted balls
''	For b = UBound(gBOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
''		...rolling(b) = False
''		...StopSound("BallRoll_" & b)
''	Next
''
'' ...rolling and drop sounds...
''
''		If DropCount(b) < 5 Then
''			DropCount(b) = DropCount(b) + 1
''		End If
''
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			BallShadowA(b).visible = 1
'			BallShadowA(b).X = gBOT(b).X + offsetX
'			If gBOT(b).Z > 30 Then
'				BallShadowA(b).height=gBOT(b).z - BallSize/4 + b/1000	'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
'				BallShadowA(b).Y = gBOT(b).Y + offsetY + BallSize/10
'			Else
'				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 1.04 + b/1000
'				BallShadowA(b).Y = gBOT(b).Y + offsetY
'			End If
'		End If

' *** Place this inside the table init, just after trough balls are added to gBOT
'
' Add balls to shadow dictionary
'	For Each xx in gBOT
'		bsDict.Add xx.ID, bsNone
'	Next

' *** Example RampShadow trigger subs:

'Sub ClearRampStart_hit()
'	bsRampOnClear			'Shadow on ramp and pf below
'End Sub

'Sub SolidRampStart_hit()
'	bsRampOn				'Shadow on ramp only
'End Sub

'Sub WireRampStart_hit()
'	bsRampOnWire			'Shadow only on pf
'End Sub

'Sub RampEnd_hit()
'	bsRampOff ActiveBall.ID	'Back to default shadow behavior
'End Sub


' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

'Function Distance(ax,ay,bx,by)
'	Distance = SQR((ax - bx)^2 + (ay - by)^2)
'End Function

'Dim PI: PI = 4*Atn(1)

'Function Atn2(dy, dx)
'	If dx > 0 Then
'		Atn2 = Atn(dy / dx)
'	ElseIf dx < 0 Then
'		If dy = 0 Then
'			Atn2 = pi
'		Else
'			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
'		end if
'	ElseIf dx = 0 Then
'		if dy = 0 Then
'			Atn2 = 0
'		else
'			Atn2 = Sgn(dy) * pi / 2
'		end if
'	End If
'End Function

'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******

' *** These define the appearance of shadows in your table	***

Const BallBrightness =  1.0         'Ball brightness - Value between 0 and 1 (0=Dark ... 1=Bright)

'Ambient (Room light source)
Const AmbientBSFactor = 0.9	 '0 To 1, higher is darker
Const AmbientMovement = 1	   '1+ higher means more movement as the ball moves left and right
Const offsetX = 0			   'Offset x position under ball (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY = 5			   'Offset y position under ball (^^for example 5,5 if the light is in the back left corner)

'Dynamic (Table light sources)
Const DynamicBSFactor = 0.90	'0 To 1, higher is darker
Const Wideness = 20			 'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness = 5			  'Sets minimum as ball moves away from source

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
Dim objrtx1(3), objrtx2(3)
Dim objBallShadow(3)
Dim OnPF(3)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2)
Dim DSSources(9), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

' *** The Shadow Dictionary
Dim bsDict
Set bsDict = New cvpmDictionary
Const bsNone = "None"
Const bsWire = "Wire"
Const bsRamp = "Ramp"
Const bsRampClear = "Clear"

'Initialization
DynamicBSInit

Sub DynamicBSInit()
	Dim iii, source
	
	'Prepare the shadow objects before play begins
	For iii = 0 To tnob - 1
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii / 1000 + 0.01  'Separate z for layering without clipping
		objrtx1(iii).visible = 0
		
		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii / 1000 + 0.02
		objrtx2(iii).visible = 0
		
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii / 1000 + 0.04
		objBallShadow(iii).visible = 0
		
		BallShadowA(iii).Opacity = 100 * AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next
	
	iii = 0
	
	For Each Source In DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
		'   If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
End Sub

Sub BallOnPlayfieldNow(onPlayfield, ballNum)	'Whether a ball is currently on the playfield. Only update certain things once, save some cycles
	Dim BOT: BOT = getballs	'Uncomment if you're destroying balls - Not recommended! #SaveTheBalls
	If onPlayfield Then
		OnPF(ballNum) = True
		bsRampOff BOT(ballNum).ID
		'   debug.print "Back on PF"
		UpdateMaterial objBallShadow(ballNum).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(ballNum).size_x = 5
		objBallShadow(ballNum).size_y = 4.5
		objBallShadow(ballNum).visible = 1
		BallShadowA(ballNum).visible = 0
		BallShadowA(ballNum).Opacity = 100 * AmbientBSFactor
	Else
		OnPF(ballNum) = False
		'   debug.print "Leaving PF"
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff 'Max distance to light sources, can be changed dynamically if you have a reason
	falloff = 150
	Dim ShadowOpacity1, ShadowOpacity2
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim bsRampType
	Dim BOT: BOT = getballs	'Uncomment if you're destroying balls - Not recommended! #SaveTheBalls
	
	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 To tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next
	
	If UBound(BOT) < lob Then Exit Sub 'No balls in play, exit
	
	'The Magic happens now

' *** Compute ball lighting from GI and ambient lighting (including captive Balls)
	Dim b_base, b_r, b_g, b_b, d_w
	b_base = 210 * BallBrightness + 0.45 * LightLevel

	For s = lob To UBound(BOT)
		' *** Normal "ambient light" ball shadow
		'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your Elseif segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else (under 20)
		
		'Primitive shadow on playfield, flasher shadow in ramps
		If AmbientBallShadowOn = 1 Then
			'** Above the playfield
			If BOT(s).Z > 30 Then
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update
				bsRampType = getBsRampType(BOT(s).id)
				'   debug.print bsRampType
				
				If Not bsRampType = bsRamp Then 'Primitive visible on PF
					objBallShadow(s).visible = 1
					objBallShadow(s).X = BOT(s).X + (BOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
					objBallShadow(s).Y = BOT(s).Y + offsetY
					objBallShadow(s).size_x = 5 * ((BOT(s).Z + BallSize) / 80) 'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((BOT(s).Z + BallSize) / 80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor * (30 / (BOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else 'Opaque, no primitive below
					objBallShadow(s).visible = 0
				End If
				
				If bsRampType = bsRampClear Or bsRampType = bsRamp Then 'Flasher visible on opaque ramp
					BallShadowA(s).visible = 1
					BallShadowA(s).X = BOT(s).X + offsetX
					BallShadowA(s).Y = BOT(s).Y + offsetY + BallSize / 10
					BallShadowA(s).height = BOT(s).z - BallSize / 4 + s / 1000 'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
					If bsRampType = bsRampClear Then BallShadowA(s).Opacity = 50 * AmbientBSFactor
				ElseIf bsRampType = bsWire Or bsRampType = bsNone Then 'Turn it off on wires or falling out of a ramp
					BallShadowA(s).visible = 0
				End If
				
				'** On pf, primitive only
			ElseIf BOT(s).Z <= 30 And BOT(s).Z > 20 Then
				If Not OnPF(s) Then BallOnPlayfieldNow True, s
				objBallShadow(s).X = BOT(s).X + (BOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
				objBallShadow(s).Y = BOT(s).Y + offsetY
				'   objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf
				
				'** Under pf, flasher shadow only
			Else
				If OnPF(s) Then BallOnPlayfieldNow False, s
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 1
				BallShadowA(s).X = BOT(s).X + offsetX
				BallShadowA(s).Y = BOT(s).Y + offsetY
				BallShadowA(s).height = BOT(s).z - BallSize / 4 + s / 1000
			End If
			
			'Flasher shadow everywhere
		ElseIf AmbientBallShadowOn = 2 Then
			If BOT(s).Z > 30 Then 'In a ramp
				BallShadowA(s).X = BOT(s).X + offsetX
				BallShadowA(s).Y = BOT(s).Y + offsetY + BallSize / 10
				BallShadowA(s).height = BOT(s).z - BallSize / 4 + s / 1000 'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			ElseIf BOT(s).Z <= 30 And BOT(s).Z > 20 Then 'On pf
				BallShadowA(s).visible = 1
				BallShadowA(s).X = BOT(s).X + (BOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
				BallShadowA(s).Y = BOT(s).Y + offsetY
				BallShadowA(s).height = 1.04 + s / 1000
			Else 'Under pf
				BallShadowA(s).X = BOT(s).X + offsetX
				BallShadowA(s).Y = BOT(s).Y + offsetY
				BallShadowA(s).height = BOT(s).z - BallSize / 4 + s / 1000
			End If
		End If
		
		' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If BOT(s).Z < 30 And BOT(s).X < 850 Then 'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff
				dist2 = falloff
				For iii = 0 To numberofsources - 1 'Search the 2 nearest influencing lights
					LSd = Distance(BOT(s).x, BOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then
						'   If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1
					objrtx1(s).X = BOT(s).X
					objrtx1(s).Y = BOT(s).Y
					'   objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), BOT(s).X, BOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1 * DynamicBSFactor ^ 3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1
					objrtx2(s).X = BOT(s).X
					objrtx2(s).Y = BOT(s).Y + offsetY
					'   objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), BOT(s).X, BOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2 * DynamicBSFactor ^ 3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(s).visible = 0
				objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub

' *** Ramp type definitions

Sub bsRampOnWire()
	If bsDict.Exists(ActiveBall.ID) Then
		bsDict.Item(ActiveBall.ID) = bsWire
	Else
		bsDict.Add ActiveBall.ID, bsWire
	End If
End Sub

Sub bsRampOn()
	If bsDict.Exists(ActiveBall.ID) Then
		bsDict.Item(ActiveBall.ID) = bsRamp
	Else
		bsDict.Add ActiveBall.ID, bsRamp
	End If
End Sub

Sub bsRampOnClear()
	If bsDict.Exists(ActiveBall.ID) Then
		bsDict.Item(ActiveBall.ID) = bsRampClear
	Else
		bsDict.Add ActiveBall.ID, bsRampClear
	End If
End Sub

Sub bsRampOff(idx)
	If bsDict.Exists(idx) Then
		bsDict.Item(idx) = bsNone
	End If
End Sub

Function getBsRampType(id)
	Dim retValue
	If bsDict.Exists(id) Then
		retValue = bsDict.Item(id)
	Else
		retValue = bsNone
	End If
	getBsRampType = retValue
End Function

'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************

'******************************************************
'	ZPHY:  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Adding nFozzy roth physics : pt1 rubber dampeners 				https://youtu.be/AXX3aen06FM
' Adding nFozzy roth physics : pt2 flipper physics 					https://youtu.be/VSBFuK2RCPE
' Adding nFozzy roth physics : pt3 other elements 					https://youtu.be/JN8HEJapCvs
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
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF
Set LF = New FlipperPolarity
Dim RF
Set RF = New FlipperPolarity

InitPolarity


'
''*******************************************
''  Late 80's early 90's

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

Dim LFPress, LFPress1, RFPress, RFPress1, LFCount, LFCount1, RFCount, RFCount1
Dim LFState, LFState1, RFState, RFState1
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, RFEndAngle1, LFEndAngle, LFEndAngle1

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 1.2 '90's and later
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



'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9	 'Level of bounces. Recommmended value of 0.7

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

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
Function RotPoint(x,y,angle)
	dim rx, ry
	rx = x*dCos(angle) - y*dSin(angle)
	ry = x*dSin(angle) + y*dCos(angle)
	RotPoint = Array(rx,ry)
End Function

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
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
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
		
		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dynamic Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height = BOT(b).z - BallSize / 4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Else
				BallShadowA(b).height = 0.1
			End If
			BallShadowA(b).Y = BOT(b).Y + offsetY
			BallShadowA(b).X = BOT(b).X + offsetX
			BallShadowA(b).visible = 1
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
			Debug.print "WireRampOn error, ball queue Is full: " & vbNewLine & _
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
' Tutorial vides by Apophis
' Audio : Adding Fleep Part 1				https://youtu.be/rG35JVHxtx4
' Audio : Adding Fleep Part 2				https://youtu.be/dk110pWMxGo
' Audio : Adding Fleep Part 3				https://youtu.be/ESXWGJZY_EI


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
RubberFlipperSoundFactor = 0.375 / 5			'volume multiplier; must not be zero
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
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
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
	PlaySound playsoundparams, - 1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

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
	PlaySound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
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

Sub RandomSoundWireRampStop(obj)
	Select Case Int(rnd*8)
		Case 0: PlaySoundAtVol "wireramp_stop", obj, 0.2 * VolPlayfieldRoll(ActiveBall) * volumedial * 2
		Case 1: PlaySoundAtVol "wireramp_stop2", obj, 0.2 * VolPlayfieldRoll(ActiveBall) * volumedial * 2
		Case 2: PlaySoundAtVol "wireramp_stop3", obj, 0.2 * VolPlayfieldRoll(ActiveBall) * volumedial * 2
		Case 3: PlaySoundAtVol "wireramp_stop", obj, 0.1 * VolPlayfieldRoll(ActiveBall) * volumedial * 2
		Case 4: PlaySoundAtVol "wireramp_stop2", obj, 0.1 * VolPlayfieldRoll(ActiveBall) * volumedial * 2
		Case 5: PlaySoundAtVol "wireramp_stop3", obj, 0.1 * VolPlayfieldRoll(ActiveBall) * volumedial * 2
		Case 6: PlaySoundAtVol "WireRamp_Hit", obj, 0.1 * VolPlayfieldRoll(ActiveBall) * volumedial * 2
		Case 7: PlaySoundAtVol "WireRamp_Hit", obj, 0.2 * VolPlayfieldRoll(ActiveBall) * volumedial * 2
	End Select
End Sub

Sub RandomSoundRampStop(obj)
	'Debug.Print "RandomSoundRampStop = " & VolPlayfieldRoll(ActiveBall) * VolumeDial * 10
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), VolPlayfieldRoll(ActiveBall) * VolumeDial * 4
		Case 1: PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), VolPlayfieldRoll(ActiveBall) * VolumeDial * 3
		Case 2: PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), VolPlayfieldRoll(ActiveBall) * VolumeDial * 2 
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************


'******************************************************
'	ZRST: STAND-UP TARGETS by Rothbauerw
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
Dim ST41, ST42, ST44A1, ST44A2, ST45, ST47, ST48, ST62, ST44B1, ST44B2, ST86

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
'   primary:	vp target to determine target hit
'   prim:	   primitive target used for visuals and animation
'				   IMPORTANT!!!
'				   transy must be used to offset the target animation
'   switch:	 ROM switch number
'   animate:	Arrary slot for handling the animation instrucitons, set to 0
'
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts

Set ST41 = (new StandupTarget)(SW41, psw41,41, 0)
Set ST42 = (new StandupTarget)(sw42, psw42,42, 0)
Set ST44A1 = (new StandupTarget)(sw44a1, psw44a1,4411, 0)
Set ST44A2 = (new StandupTarget)(sw44a2, psw44a2,4412, 0)
Set ST45 = (new StandupTarget)(sw45, psw45,45, 0)
Set ST47 = (new StandupTarget)(sw47, psw47,47, 0)
Set ST48 = (new StandupTarget)(sw48, psw48,48, 0)
Set ST62 = (new StandupTarget)(sw62, psw62,62, 0)
Set ST44B1 = (new StandupTarget)(sw44b1, psw44b1,4421, 0)
Set ST44B2 = (new StandupTarget)(sw44b2, psw44b2,4422, 0)
Set ST86 = (new StandupTarget)(sw86, psw86,86, 0)


'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
'   STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST41, ST42, ST44A1, ST44A2, ST45, ST47, ST48, ST62, ST44B1, ST44B2, ST86)

'Configure the behavior of Stand-up Targets
Const STAnimStep = 1.5  'vpunits per animation step (control return to Start)
Const STMaxOffset = 9   'max vp units target moves when hit

Const STMass = 0.2	  'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub DTBallPhysics(aBall, angle, mass)
	Dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	
	calc1 = cor.BallVel(aball.id) * Cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Cos(rangle + 4 * Atn(1) / 2)
	calc3 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Sin(rangle + 4 * Atn(1) / 2)
	
	aBall.velx = calc1 * Cos(rangle) + calc2
	aBall.vely = calc1 * Sin(rangle) + calc3
End Sub

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)
	
	PlayTargetSound
	STArray(i).animate = STCheckHit(ActiveBall,STArray(i).primary)
	
	If STArray(i).animate <> 0 Then
		DTBallPhysics ActiveBall, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 To UBound(STArray)
		If STArray(i).sw = switch Then
			STArrayID = i
			Exit Function
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
		STAction switch
		'vpmTimer.PulseSw switch
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

Sub STAction(Switch)
	Select Case Switch
		Case 4411, 4412, 4421, 4422
			vpmTimer.PulseSw 44
			
		Case Else
			vpmTimer.PulseSw switch
	End Select
End Sub



'******************************************************
'****   END STAND-UP TARGETS
'******************************************************

'******************************************************
' 	ZTST:  Debug Shot Tester
'******************************************************

'****************************************************************
' Section; Debug Shot Tester v3.2
'
' 1.  Raise/Lower outlanes and drain posts by pressing 2 key
' 2.  Capture and Launch ball, Press and hold one of the buttons (W, E, R, Y, U, I, P, A) below to capture ball by flipper.  Release key to shoot ball
' 3.  To change the test shot angles, press and hold a key and use Flipper keys to adjust the shot angle.  Shot angles are saved into the User direction as cgamename.txt
' 4.  Set DebugShotMode = 0 to disable debug shot test code.
'
' HOW TO INSTALL: Copy all debug* objects from Layer 2 to table and adjust. Copy the Debug Shot Tester code section to the script.
'	Add "DebugShotTableKeyDownCheck keycode" to top of Table1_KeyDown sub and add "DebugShotTableKeyUpCheck keycode" to top of Table1_KeyUp sub
'****************************************************************
Const DebugShotMode = 1 'Set to 0 to disable.  1 to enable
Dim DebugKickerForce
DebugKickerForce = 55

' Enable Disable Outlane and Drain Blocker Wall for debug testing
Dim DebugBLState
debug_BLW1.IsDropped = 1
debug_BLP1.Visible = 0
debug_BLR1.Visible = 0
debug_BLW2.IsDropped = 1
debug_BLP2.Visible = 0
debug_BLR2.Visible = 0
debug_BLW3.IsDropped = 1
debug_BLP3.Visible = 0
debug_BLR3.Visible = 0

Sub BlockerWalls
	DebugBLState = (DebugBLState + 1) Mod 4
	'	debug.print "BlockerWalls"
	PlaySound ("Start_Button")
	
	Select Case DebugBLState
		Case 0
			debug_BLW1.IsDropped = 1
			debug_BLP1.Visible = 0
			debug_BLR1.Visible = 0
			debug_BLW2.IsDropped = 1
			debug_BLP2.Visible = 0
			debug_BLR2.Visible = 0
			debug_BLW3.IsDropped = 1
			debug_BLP3.Visible = 0
			debug_BLR3.Visible = 0
			
		Case 1
			debug_BLW1.IsDropped = 0
			debug_BLP1.Visible = 1
			debug_BLR1.Visible = 1
			debug_BLW2.IsDropped = 0
			debug_BLP2.Visible = 1
			debug_BLR2.Visible = 1
			debug_BLW3.IsDropped = 0
			debug_BLP3.Visible = 1
			debug_BLR3.Visible = 1
			
		Case 2
			debug_BLW1.IsDropped = 0
			debug_BLP1.Visible = 1
			debug_BLR1.Visible = 1
			debug_BLW2.IsDropped = 0
			debug_BLP2.Visible = 1
			debug_BLR2.Visible = 1
			debug_BLW3.IsDropped = 1
			debug_BLP3.Visible = 0
			debug_BLR3.Visible = 0
			
		Case 3
			debug_BLW1.IsDropped = 1
			debug_BLP1.Visible = 0
			debug_BLR1.Visible = 0
			debug_BLW2.IsDropped = 1
			debug_BLP2.Visible = 0
			debug_BLR2.Visible = 0
			debug_BLW3.IsDropped = 0
			debug_BLP3.Visible = 1
			debug_BLR3.Visible = 1
	End Select
End Sub

Sub DebugShotTableKeyDownCheck (Keycode)
	'Cycle through Outlane/Centerlane blocking posts
	'-----------------------------------------------
	If Keycode = 3 Then
		BlockerWalls
	End If
	
	If DebugShotMode = 1 Then
		'Capture and launch ball:
		'	Press and hold one of the buttons (W, E, R, T, Y, U, I, P) below to capture ball by flipper.  Release key to shoot ball
		'	To change the test shot angles, press and hold a key and use Flipper keys to adjust the shot angle.
		'--------------------------------------------------------------------------------------------
		If keycode = 17 Then 'W key
			debugKicker.enabled = True
			TestKickerVar = TestKickAngleW
		End If
		If keycode = 18 Then 'E key
			debugKicker.enabled = True
			TestKickerVar = TestKickAngleE
		End If
		If keycode = 19 Then 'R key
			debugKicker.enabled = True
			TestKickerVar = TestKickAngleR
		End If
		If keycode = 21 Then 'Y key
			debugKicker.enabled = True
			TestKickerVar = TestKickAngleY
		End If
		If keycode = 22 Then 'U key
			debugKicker.enabled = True
			TestKickerVar = TestKickAngleU
		End If
		If keycode = 23 Then 'I key
			debugKicker.enabled = True
			TestKickerVar = TestKickAngleI
		End If
		If keycode = 25 Then 'P key
			debugKicker.enabled = True
			TestKickerVar = TestKickAngleP
		End If
		If keycode = 30 Then 'A key
			debugKicker.enabled = True
			TestKickerVar = TestKickAngleA
		End If
		If keycode = 31 Then 'S key
			debugKicker.enabled = True
			TestKickerVar = TestKickAngleS
		End If
		If keycode = 33 Then 'F key
			debugKicker.enabled = True
			TestKickerVar = TestKickAngleF
		End If
		If keycode = 34 Then 'G key
			debugKicker.enabled = True
			TestKickerVar = TestKickAngleG
		End If
		
		If debugKicker.enabled = True Then		'Use Flippers to adjust angle while holding key
			If keycode = LeftFlipperKey Then
				debugKickAim.Visible = True
				TestKickerVar = TestKickerVar - 1
				Debug.print TestKickerVar
			ElseIf keycode = RightFlipperKey Then
				debugKickAim.Visible = True
				TestKickerVar = TestKickerVar + 1
				Debug.print TestKickerVar
			End If
			debugKickAim.ObjRotz = TestKickerVar
		End If
	End If
End Sub


Sub DebugShotTableKeyUpCheck (Keycode)
	' Capture and launch ball:
	' Release to shoot ball. Set up angle and force as needed for each shot.
	'--------------------------------------------------------------------------------------------
	If DebugShotMode = 1 Then
		If keycode = 17 Then 'W key
			TestKickAngleW = TestKickerVar
			debugKicker.kick TestKickAngleW, DebugKickerForce
			debugKicker.enabled = False
		End If
		If keycode = 18 Then 'E key
			TestKickAngleE = TestKickerVar
			debugKicker.kick TestKickAngleE, DebugKickerForce
			debugKicker.enabled = False
		End If
		If keycode = 19 Then 'R key
			TestKickAngleR = TestKickerVar
			debugKicker.kick TestKickAngleR, DebugKickerForce
			debugKicker.enabled = False
		End If
		If keycode = 21 Then 'Y key
			TestKickAngleY = TestKickerVar
			debugKicker.kick TestKickAngleY, DebugKickerForce
			debugKicker.enabled = False
		End If
		If keycode = 22 Then 'U key
			TestKickAngleU = TestKickerVar
			debugKicker.kick TestKickAngleU, DebugKickerForce
			debugKicker.enabled = False
		End If
		If keycode = 23 Then 'I key
			TestKickAngleI = TestKickerVar
			debugKicker.kick TestKickAngleI, DebugKickerForce
			debugKicker.enabled = False
		End If
		If keycode = 25 Then 'P key
			TestKickAngleP = TestKickerVar
			debugKicker.kick TestKickAngleP, DebugKickerForce
			debugKicker.enabled = False
		End If
		If keycode = 30 Then 'A key
			TestKickAngleA = TestKickerVar
			debugKicker.kick TestKickAngleA, DebugKickerForce
			debugKicker.enabled = False
		End If
		If keycode = 31 Then 'S key
			TestKickAngleS = TestKickerVar
			debugKicker.kick TestKickAngleS, DebugKickerForce
			debugKicker.enabled = False
		End If
		If keycode = 33 Then 'F key
			TestKickAngleF = TestKickerVar
			debugKicker.kick TestKickAngleF, DebugKickerForce
			debugKicker.enabled = False
		End If
		If keycode = 34 Then 'G key
			TestKickAngleG = TestKickerVar
			debugKicker.kick TestKickAngleG, DebugKickerForce
			debugKicker.enabled = False
		End If
		
		'		EXAMPLE CODE to set up key to cycle through 3 predefined shots
		'		If keycode = 17 Then	 'Cycle through all left target shots
		'			If TestKickerAngle = -28 then
		'				TestKickerAngle = -24
		'			ElseIf TestKickerAngle = -24 Then
		'				TestKickerAngle = -19
		'			Else
		'				TestKickerAngle = -28
		'			End If
		'			debugKicker.kick TestKickerAngle, DebugKickerForce: debugKicker.enabled = false			 'W key
		'		End If
		
	End If
	
	If (debugKicker.enabled = False And debugKickAim.Visible = True) Then 'Save Angle changes
		debugKickAim.Visible = False
		SaveTestKickAngles
	End If
End Sub

Dim TestKickerAngle, TestKickerAngle2, TestKickerVar, TeskKickKey, TestKickForce
Dim TestKickAngleWDefault, TestKickAngleEDefault, TestKickAngleRDefault, TestKickAngleYDefault, TestKickAngleUDefault, TestKickAngleIDefault
Dim TestKickAnglePDefault, TestKickAngleADefault, TestKickAngleSDefault, TestKickAngleFDefault, TestKickAngleGDefault
Dim TestKickAngleW, TestKickAngleE, TestKickAngleR, TestKickAngleY, TestKickAngleU, TestKickAngleI
Dim TestKickAngleP, TestKickAngleA, TestKickAngleS, TestKickAngleF, TestKickAngleG
TestKickAngleWDefault =  - 27
TestKickAngleEDefault =  - 20
TestKickAngleRDefault =  - 14
TestKickAngleYDefault =  - 8
TestKickAngleUDefault =  - 3
TestKickAngleIDefault = 1
TestKickAnglePDefault = 5
TestKickAngleADefault = 11
TestKickAngleSDefault = 17
TestKickAngleFDefault = 19
TestKickAngleGDefault = 5
If DebugShotMode = 1 Then LoadTestKickAngles

Sub SaveTestKickAngles
	Dim FileObj, OutFile
	Set FileObj = CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then Exit Sub
	Set OutFile = FileObj.CreateTextFile(UserDirectory & cGameName & ".txt", True)
	
	OutFile.WriteLine TestKickAngleW
	OutFile.WriteLine TestKickAngleE
	OutFile.WriteLine TestKickAngleR
	OutFile.WriteLine TestKickAngleY
	OutFile.WriteLine TestKickAngleU
	OutFile.WriteLine TestKickAngleI
	OutFile.WriteLine TestKickAngleP
	OutFile.WriteLine TestKickAngleA
	OutFile.WriteLine TestKickAngleS
	OutFile.WriteLine TestKickAngleF
	OutFile.WriteLine TestKickAngleG
	OutFile.Close
	
	Set OutFile = Nothing
	Set FileObj = Nothing
End Sub

Sub LoadTestKickAngles
	Dim FileObj, OutFile, TextStr
	
	Set FileObj = CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then
		MsgBox "User directory missing"
		Exit Sub
	End If
	
	If FileObj.FileExists(UserDirectory & cGameName & ".txt") Then
		Set OutFile = FileObj.GetFile(UserDirectory & cGameName & ".txt")
		Set TextStr = OutFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream = True) Then
			Exit Sub
		End If
		
		TestKickAngleW = TextStr.ReadLine
		TestKickAngleE = TextStr.ReadLine
		TestKickAngleR = TextStr.ReadLine
		TestKickAngleY = TextStr.ReadLine
		TestKickAngleU = TextStr.ReadLine
		TestKickAngleI = TextStr.ReadLine
		TestKickAngleP = TextStr.ReadLine
		TestKickAngleA = TextStr.ReadLine
		TestKickAngleS = TextStr.ReadLine
		TestKickAngleF = TextStr.ReadLine
		TestKickAngleG = TextStr.ReadLine
		TextStr.Close
	Else
		'create file
		TestKickAngleW = TestKickAngleWDefault
		TestKickAngleE = TestKickAngleEDefault
		TestKickAngleR = TestKickAngleRDefault
		TestKickAngleY = TestKickAngleYDefault
		TestKickAngleU = TestKickAngleUDefault
		TestKickAngleI = TestKickAngleIDefault
		TestKickAngleP = TestKickAnglePDefault
		TestKickAngleA = TestKickAngleADefault
		TestKickAngleS = TestKickAngleSDefault
		TestKickAngleF = TestKickAngleFDefault
		TestKickAngleG = TestKickAngleGDefault
		SaveTestKickAngles
	End If
	
	Set OutFile = Nothing
	Set FileObj = Nothing
	
End Sub
'****************************************************************
' End of Section; Debug Shot Tester 3.2
'****************************************************************

' VR CLOCK
Dim CurrentMinute

Sub ClockTimer_Timer()
	PMinutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
	PSeconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub

' VR PLUNGER ANIMATION
Sub TimerPlunger_Timer
	If Pincab_Plunger.Y < 1168 then
		Pincab_Plunger.Y = Pincab_Plunger.Y + 5
		Pincab_Plunger2.Y = Pincab_Plunger2.Y + 5
	End If
End Sub

Sub TimerPlunger2_Timer
	Pincab_Plunger.Y = 1068 + (5 * Plunger.Position) - 20
	Pincab_Plunger2.Y = 1068 + (5 * Plunger.Position) - 20
End Sub

'*******************************************
' Set Up VR Backglass Flashers
' This is for lining up the backglass flashers on top of a backglass image
'*******************************************

Sub SetVRBackglass
	Dim obj
	For Each obj In VRBackglass
		obj.x = obj.x + 15
		obj.height = - obj.y + 425
		obj.y = 45 ' adjusts the distance from the backglass towards the user
		obj.RotX = 270
	Next
End Sub


Sub UpdateVRBGlamps

	If Controller.Lamp(66) = 0 Then
		VRBGFL66_1.Visible = 0
	Else
		VRBGFL66_1.Visible = 1
	End If

	If Controller.Lamp(51) = 0 Then
		VRBGFL51_1.Visible = 0
		VRBGFL51_2.Visible = 0
		VRBGFL51_3.Visible = 0
		VRBGFL51_4.Visible = 0
		VRBGFL51_5.Visible = 0
	Else
		VRBGFL51_1.Visible = 1
		VRBGFL51_2.Visible = 1
		VRBGFL51_3.Visible = 1
		VRBGFL51_4.Visible = 1
		VRBGFL51_5.Visible = 1
	End If

	If Controller.Lamp(52) = 0 Then
		VRBGFL52_1.Visible = 0
		VRBGFL52_2.Visible = 0
	Else
		VRBGFL52_1.Visible = 1
		VRBGFL52_2.Visible = 1
	End If

	If Controller.Lamp(53) = 0 Then
		VRBGFL53_1.Visible = 0
	Else
		VRBGFL53_1.Visible = 1
	End If

	If Controller.Lamp(54) = 0 Then
		VRBGFL54_1.Visible = 0
	Else
		VRBGFL54_1.Visible = 1
	End If

	If Controller.Lamp(55) = 0 Then
		VRBGFL55_1.Visible = 0
		VRBGFL55_2.Visible = 0
	Else
		VRBGFL55_1.Visible = 1
		VRBGFL55_2.Visible = 1
	End If

	If Controller.Lamp(56) = 0 Then
		VRBGFL56_1.Visible = 0
		VRBGFL56_2.Visible = 0
	Else
		VRBGFL56_1.Visible = 1
		VRBGFL56_2.Visible = 1
	End If

	If Controller.Lamp(57) = 0 Then
		VRBGFL57_1.Visible = 0
	Else
		VRBGFL57_1.Visible = 1
	End If

	If Controller.Lamp(68) = 0 Then
		VRBGFL68_1.Visible = 0
	Else
		VRBGFL68_1.Visible = 1
	End If

	If Controller.Lamp(58) = 0 Then
		VRBGFL58_1.Visible = 0
	Else
		VRBGFL58_1.Visible = 1
	End If

	' Inverted solenoid
	If Controller.Solenoid(4) = 1 Then
		VRBGFL4_1.Visible = 0
		VRBGFL4_2.Visible = 0
		VRBGFL4_3.Visible = 0
		VRBGFL4_4.Visible = 0
		VRBGFL4_5.Visible = 0
		VRBGFL4_6.Visible = 0
		VRBGFL4_7.Visible = 0
		VRBGFL4_8.Visible = 0
		VRBGFL4_9.Visible = 0
		VRBGFL4_10.Visible = 0
		VRBGFL4_11.Visible = 0
		VRBGFL4_12.Visible = 0
		VRBGFL4_13.Visible = 0
	Else
		VRBGFL4_1.Visible = 1
		VRBGFL4_2.Visible = 1
		VRBGFL4_3.Visible = 1
		VRBGFL4_4.Visible = 1
		VRBGFL4_5.Visible = 1
		VRBGFL4_6.Visible = 1
		VRBGFL4_7.Visible = 1
		VRBGFL4_8.Visible = 1 
		VRBGFL4_9.Visible = 1
		VRBGFL4_10.Visible = 1
		VRBGFL4_11.Visible = 1
		VRBGFL4_12.Visible = 1
		VRBGFL4_13.Visible = 1
	End If

End Sub
