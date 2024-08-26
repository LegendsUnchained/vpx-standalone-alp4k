''Bram Stoker's Dracula / IPD No. 3072 / April, 1993 / 4 Players
' 
'01010111 01101001 01101110 01101110 01100101 01110010 01110011 00100000
'01100100 01101111 01101110 00100111 01110100 00100000 01110011 01100101
'01101100 01101100 00100000 01010110 01010000 01011000 00100000 01110100
'01100001 01100010 01101100 01100101 01110011 00101110
'
'
' Bram Stoker's Dracula [Csilk MOD]
'
' CREDITS
' Originally ported to VP by ICPjuggla and Dozer316, the original FP version was created by francisco666 and rom.
' Magnet script by Dorsolas/Lander's, with just small modifications
' RTX BS, dampening, sling corrections and flipper code based off the VPW example tutorial table
' Amy Dempsey for the coffin texture, thanks amers.
' Thalamus for guidance on slope, difficulty slider and kicker randomness script portion as well as some elements from his version
' Wiesshund and fluper for LUTs
' JPSalas for sound script
' andreslv for some testing of the table
' Patrick2610 for alternate apron
' H215 for pointing out the flipper not being controlled by ROM issue
' 
' Sorry if I have missed anyone else, if you feel you have been left out contact me on VPFORUMS and I will add your credits
'
' Known issues
' Spamming auto launch before ball is in position and ready to be launched can cause the machine to be forever stuck but if you can nudge the machine up (spacebar) you can get it off the roll over and it will allow for resuming play but you could accidently tilt depending on nudge values you have set
' Table is not VR ready (if you wish to make it VR - contact me and we can bundle it in the next release)
' Have seen the ball bounce off the table once with the ball moving very fast and hitting bottom sling rubber, if this occurs please let me know so I can investigate it further (it maybe fixed)
' Frame rate loss after continual play? unsure as to why
' Some issues with on the fly GI changes still remain but will be updated on table relaunch
'
' v1.6I
'
' Bugfix a few materials that were missing on items like metal walls
' Bugfix remaining sounds on materials now work...  to see what elements are being hit with callouts use Debug_Voice = 1 :)
' Bugfix a plastic piece not being visible (whoops)
' Bugfix VPW sling corrections can correctly be enabled or disabled (default off)
' Bugfix adjusted ramp materials so that launching ball doesn't fail and enter playfield incorrectly
' Bugfix restored a missing primitive from previous versions of the table (coffin ramp end)
' Added VPW dampening variable to turn on or off rubber dampening (default off)
' Added VPW target bouncer variable to turn on or off target bouncer (default off) this works in conjunction with target bouncer settings
' Added table will turn off trigger zones if inlane roll correction is off (default off) this may save some cycles
' Added wall to block ball shadow when going into the drain
' Added variable table slope support (6 - 7) use gameplay difficulty to set to your liking (50 difficulty is 6.5 slope and is my recommended value)
' Added a few more physics materials to try and replicate correct behaviours in some places
' Adjusted default setting to keep the pop bumper plastic visible
' Adjusted default setting to use the non-brightened plastics for the castle and village
' Adjusted reflection setting to better mirror how the table looks irl
' Adjusted ball reflections default amount
' Adjusted various wall elements
' Adjusted various friction values
'
' v1.5F
'
' Bugfix for flippers not being controlled by ROM
' Fixed visibility on some metal pieces (whoops)
' Fixed screws floating on non existent metal near inlanes
' Fixed some sound effect events not firing (not all are operational)
' Adjusted rubber band collision box sizes
' Adjusted some floating screws and improper Z value (they now should look like they are attaching a component)
' Adjusted collision on some walls
' Added an alternate apron graphics that is much higher quality (not in english though)
' Adjusted table slope to 6.5
' Adjusted playfield elasticity
' Adjusted many materials elasticity
' Adjusted forces of slings and pop bumpers
' Adjusted kicker velocities and angles
' Adjusted right and left inlanes
' Adjusted to less aggressive flipper settings (I'm happy with ramp shots now)
' Adjusted default lighting to all white lights
' Adjusted flipper elasticity
' Adjusted skill shot bounces before falling into pop bumpers and added plunger strength variance
'
' v1.4O
' Overhauled all the materials
' Bugfix for switching coffin mod off or on in script
' Bugfix for missing sound file
' Bugfix for custom coloring of sleeves, rubber rings, bands and posts
' Bugfix for GI not updating while adjusting intensity on the fly
' Remove ALP from table title as I no longer own an ALP (to much input delay!!!)
' Added more bulbs and ability to enable them or disable them
' Added flasher fade speed
' Added some missing screws
' Added some missing pegs and sleeves
' Added alternate apron graphics
' Added Thalamus kicker randomization
' Added TOM ripped sounds and added additional credits
' Added Thalamus' version apron rail primitive
' Added Red flippers variant
' Added LUT locking variable for those who wish to block LUT changes using magnasave
' Adjusted location of the info box
' Adjusted kicker for coffin multiball to reject the ball less often
' Adjusted kickers of the table using PAPA pinball footage / IFPA16 footage 
' Adjusted table slope again (table looks to be proper speed now)
' Adjusted flippers substantially as well as tweaked strength, flipper tricks such as post transfer should be more reliable now
' Adjusted slings force and hit threshold to be stronger sling but more forgiving hit threshold
' Adjusted rubberbands to have different elasticity depending on where the rubber band was hit
' Adjusted posts and rubber bands to be authentic colors by default (except a couple yellow Sleeves)
' Adjusted both inlanes again
' Adjusted element sizes to be inline with dracula manual
' Adjusted placement of many elements that were improperly aligned
' Adjusted default lighting and options

'
' v1.3
'
' Overhauled the physics materials
' Defaulted night/day slider to use whatever you want (real time of day)
' Added bloom control via script before table start
' Added flasher manipulation
' Added room lighting
' Added table flood lighting
' Adjusted default lighting
' Adjusted all rubber bands
' Adjusted all walls to be circle primitives for better collision
' Adjusted inlanes and post bumpers on outlanes
' Adjusted flipper arcs and settings
' Adjusted walls around coffin lock hole
' Adjusted sizes of posts
' Removed a few walls and screws that were out of place
'
' v1.20R (based off JPsalas Dracula Table)
'
' Added magnet speed, radius and strength options for easier fine tuning
' Added keybinding list on top of the script
' Added coffin mod and lut change option on opening coffin ramp diverter
' Bugfix bad castlelock not spitting out the ball correctly
' Bugfix for light falloff power not correctly updating when changed
' Adjusted bad z value and rotation on castle causing texture clipping and misalignment with hole
' Adjusted difficulty slope
' Adjusted skillshot area on plunge should now have a better bounce backwards
' Adjusted extra plastics toggle also removes/adds the additional screws/pegs used to mount them
' Adjusted materials
' Adjusted purple to actually look purple
' Added playfield tinting Flasher
' Added playfield shadows
' Added pop bumper plastic can be toggled on or off
' Added a few additional pegs/posts where I think they are needed
' Added brighter textures for village/castle/sling plastics can be toggled on or off
' Added moon lighting option
' Added target bouncer options on pop bumpers and targets
' Added nFozzy physics dampening
' Added RTX BS On
' Added nFozzy/roth flippers and flipper tricks
' Added Extra colors are added to the color codes
' Added Pegs, Posts and Rubberband colors can now be set in the script
' 
' v1.1 (available in the thread on VPFORUMS but not officially released)
' Added nFozzy/roth flippers
'
' v1.0 (based off Bigus Dracula V2.0 Table)
' Adjusted MIST Magnet 
' Added LUT control using magnasave buttons (Many thanks to Fluper and contributers to his TOTAN4K table as these are the same LUTs as found there as well as weisshound for his heavy metal LUTs)
' Added Light Intensity control
' Added Light Falloff control
' Added Light Falloff power control
' Added GI Intensity Control
' Added GI color control
' Added GI full color control 
' Added extra sling plastics toggle by pressing \ key
'
 
' Ingame Keybindings (you can also set these things at the top of the script so that they are always set the way you like)
' Holding right flipper is a modifier key
'
' Option								Keys
' GI Color								Up, Down, Right Arrow Keys
' Playfield Tinting						Left Arrow Key
' Table Flood Lighting					MODIFIER KEY + left Arrow Key
' Room Lighting Strength				MODIFIER KEY + NUMPAD PLUS and MINUS
' GI Full Color							MODIFIER KEY + Up, Down, Right Arrow Keys
' Light Intensity control				NUMPAD PLUS and MINUS
' Light Falloff control					use bracket Keys
' Light Falloff power control			use ; and ' keys
' GI Intensity Control					comma and period
' Coffin mod toggle 					NUMPAD *
' Extra sling plastics					NUMPAD /
' Pop plastic removal					NUMPAD .
Option Explicit
Randomize
Dim GI_Intensity, Light_Intensity, Light_Falloff, Light_Falloff_Power, GI_Top_Color, GI_Top_Full_Color, GI_Mid_Color, GI_Mid_Full_Color, GI_Bot_Color, GI_Bot_Full_Color, FlipperHeldModifier, ExtraPlastics, ApronShadowOpacity, PlayfieldShadowOpacity, PlayfieldTintingOpacity, TableLightingStrength, Tint_Color_Lighting, Table_Color_Lighting, RemovePopPlastic, OpenCoffinLutPos, MoonLighting, VPW_Sling_Correction, Sleeve_Color, Ring_Color, Post_Rubber_Color, Rubber_Band_Color, Coffin_Mod, Brighter_Props, RotateDMD, HideDMD, DesktopModeOverwrite, MagnetStrength, MagnetRadius, MagnetSpeed, SOSRampup, ModifiedElasticity, UnmodifiedElasticity, RollCorrection, Room_Light_Strength, Flasher_Intensity, Flasher_Falloff, Flasher_Falloff_Power, Light_Bloom_Strength, Locked_LUT
Dim AlternateApron, DesktopMode, Flipper_Image, No_Light_Edits, Additional_Bulbs, Additional_Flashers, Flash_Fade_Speed
Dim Debug_Voice, VPW_Dampening, VPW_Bouncer
'START EDITABLE STUFF
' GENERIC SETTINGS
RotateDMD = 0 'if using intable DMD rotation for the DMD display (0 for desktop, 1 for rotated 90 degrees portrait etc)
HideDMD = 0 'turn on or hide the DMD (1 is off)
DesktopModeOverwrite = 1 'if 0 then not desktop mode, if 1 it will let VPX decide if DMD should be shown rather than using the setting above
DesktopMode = True 'show rails for dekstop mode or not (False)
' END GENERIC TABLE STUFF
' SOUND STUFF
' Thalamus 2020-05-04 : Improved directional sounds
' Special thanks to DjRobX, Rothbauwerw, Dark, Fleep, Flupper, Kiwi, JPSalas and ICPJuggla

'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 1    ' Bumpers volume.
Const VolRol    = 1    ' Rollovers volume.
Const VolGates  = 1    ' Gates volume.
Const VolMetal  = 1    ' Metals volume.
'
Const VolRubber     = 2   ' Rubber bands hits volume.
Const VolPosts     = 1.4    ' Rubber posts hits volume loud.
Const VolPegs     = 1.3    ' Rubber posts hits volume loud.
Const VolPlastic = 1
'Const VolSpinner = 1
Const VolTargets   = 1    ' Targets volume.
Const VolFlipper = 1.5 ' flipper hit 
'Const VolMetalRamp = 1 'metal ramp
'
Const VolDrain  = 1    ' Drain volume.
Const VolSling  = 1    ' Slingshots volume.
Const VolBRol   = .02  ' Volume Ball Rolling.
Const VolPRol   = .2   ' Volume Plastics Rolling.
Const VolMRol   = .8   ' Volume Metals Rolling.
Const VolDrop   = .7   ' Balldrop volume.
'END SOUND STUFF
'START COLOR STUFF
'Color Codes are
'
'0 = White
'1 = Red
'2 = Green
'3 = Blue
'4 = Purple
'5 = Magenta
'6 = Orange
'7 = Yellow
'8 = Pink
'9 = Black
'
dim White, Red, Blue, Green, Purple, Magenta, Orange, FullWhite, FullRed, FullGreen, FullBlue, FullPurple, FullMagenta, FullOrange, Yellow, FullYellow, Pink, FullPink, Black, FullBlack
' FOR YOUR OWN TWEAKS TO THE COLORS
White = rgb(225,225,225)
FullWhite = rgb(255,255,255)
Red = rgb(225,0,0)
FullRed = rgb(255,0,0)
Green = rgb(0,225,0)
FullGreen = rgb(0,255,0)
Blue = rgb(0,0,225)
FullBlue = rgb(0,0,255)
Purple = rgb(176, 38, 255)
FullPurple = rgb(187,41,187)
Magenta = rgb(119,0,119)
FullMagenta = rgb(139,0,139)
Orange = rgb(255, 140, 0)
FullOrange = rgb(255, 165, 0)
Yellow = rgb(255, 255, 20)
FullYellow = rgb(255,255,0)
Pink = rgb(251,72,196)
FullPink = rgb(255, 16, 240)
Black = rgb(33,33,33)
FullBlack = rgb(0,0,0)
'COLOR OPTIONS
Tint_Color_Lighting = 3
Table_Color_Lighting = 3
GI_Top_Color = 3
GI_Top_Full_Color = 0
GI_Mid_Color = 4
GI_Mid_Full_Color = 2
GI_Bot_Color = 3
GI_Bot_Full_Color = 0
Sleeve_Color = 7
Ring_Color = 0
Post_Rubber_Color = 1
Rubber_Band_Color = 9
Flipper_Image = 1 ' 0 uses yellow flipper, 1 uses red
' END COLOR OPTIONS
' LIGHT PROPERTIES CONTROLS
Light_Intensity = 45
Light_Falloff = 50
Light_Falloff_Power = 5
Flasher_Intensity = 1
Flasher_Falloff = 30
Flasher_Falloff_Power = 5
Light_Bloom_Strength = 0
No_Light_Edits = 0 'disables editing of light settings if 1 (will use the last settings you have used)
Additional_Bulbs = 1 '0 will not enable the additional GI bulbs, 1 enables more GI bulbs
Additional_Flashers = 1 '0 will not enable the additional flasher bulbs, 1 enables more flashers
Flash_Fade_Speed = 33' how fast a flasher fades
' END LIGHT PROPERTIES
'	LOOK UP TABLE (LUT) 
'	There are 26 LUTS to choose from press the magnasave buttons to cycle the LUTS when in game and get the number you want
'	USAGE: Integer between 0 and 25 and defaults to the most neutral LUT
LutPos = 14
OpenCoffinLutPos = 15 'when coffin is open the LUT changes to this one (either set the same as LutPos or set a darker one for when dracula emerges from his coffin)
Locked_Lut = 0 'when value is 1 it will not allow magnasave toggling of the LUT during play
' END LUT OPTIONS
' PLAYFIELD LIGHTING/SHADOWING/APRON OPTIONS
AlternateApron = 2 '0 uses the authentic apron, 1 uses alternate by me, 2 uses 4K non-english version (default)
GI_Intensity = 0.4
Room_Light_Strength = 0.1 'showers the table at all time with the rooms light
PlayfieldShadowOpacity = 20  'shadow around playfield elements
PlayfieldTintingOpacity = 100 'tints the main playfield
TableLightingStrength = 5 'can brighten the inside of the table as if there is colored light reaching all corners of the playfield (use 0.0 to 10.0)
' END PLAYFIELD OPTIONS
' MAGNET OPTIONS
MagnetSpeed = 0.55 '0.55 was the magnet speed before
MagnetStrength = 0.75 'really depends on the magnet radius
MagnetRadius = 55 'size of a magnet force
' END MAGNET STUFF
' MISC USER PREFERENCES OPTIONS
ExtraPlastics = 1 '0 turns off sling extra plastics, 1 enables it can use NUMPAD SLASH mid game to turn on and off as well
RemovePopPlastic = 0 ' 0 turns on pop bumper purple plastic, 1 turns off
Brighter_Props = 0 '0 turns off brighter textures for village, castle and the extra sling plastics
MoonLighting = 1 '0 turns off the moon light, 1 enables it
Coffin_Mod = 1 '0 turns off and uses normal dracula coffin, 1 uses modified textures/primitives for coffin, 2 removes the coffins for debugging ramp
' END MISC OPTIONS
' PHYSICS STUFF
RollCorrection = 0 'if enabled then inlane roll correction will occur (personal choice if you want different roll over flipper behavior)
' the next 2 settings are only used if RollCorrection is enabled
ModifiedElasticity = 0.5 'use this setting when rolling from inlane (this eliminates the bounce as it goes over flipper for correct behavior as per the real machine)
UnModifiedElasticity = leftflipper.elasticity 'use the setting as set in the editor for when correction is not applied
Debug_Voice = 0 '0 uses actual sounds for the materials, 1 uses my voice calling out what was hit (was used to debug issues and its kinda interesting to see what is being hit and when)

' I have added the ability to turn on/off some of the physics corrections that are based on scripts which should make the table lighter to run when off
VPW_Sling_Correction = 0 '0 turns off sling corrections, 1 turns on VPW sling corrections
VPW_Dampening = 0 '0 turns off rubber dampening, 1 turns on VPW script for dampening rubbers
VPW_Bouncer = 0 '0 turns off target bouncer, 1 turns on and then uses the target bouncer values below

' these next variables effect only if VPW_Bouncer is enabled
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7
Const Bounciness = 0.4			'Level of bounces.
' END PHYSICS STUFF
' RTX BS SHADOW OPTIONS
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's
'Ambient (Room light source)
Const AmbientBSFactor 		= 1		'0 to 1, higher is darker
Const AmbientMovement		= 4		'1 to 4, higher means more movement as the ball moves left and right
Const offsetX				= 3		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 3		'Offset y position under ball	 (for example 5,5 if the light is in the back left corner)
'Dynamic (Table light sources)
Const DynamicBSFactor 		= 1	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source
'END RTX BS SHADOW OPTIONS
'nFozzy FLIPPERS
Const ReflipAngle = 20
Const LiveCatch = 32
Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 113  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
Const EOSTnew = 0.8 '1 for EM, 0.8 for other
Const EOSAnew = 1
Const EOSRampup = 0
SOSRampup = leftflipper.rampup ' emergency rampup speed should match the set ramp up for the flipper
Const LiveElasticity = 0.25
Const SOSEM = 0.815
Const EOSReturn = 0.025 '0.025 recommended for early 90s, 0.035 is for late 80s early 90s
' END EDITABLE STUFF DO NOT EDIT BELOW THIS LINE
'
'
'
'
Const BallSize = 50					'Ball size must be 50
Const BallMass = 1					'Ball mass must be 1
Const tnob = 5						'Total number of balls
Const lob = 0						'Locked balls
dim gilvl:gilvl = GI_Intensity		'General Illumination light state tracked for Dynamic Ball Shadows
Dim PI: PI = 4*Atn(1)
if ExtraPlastics = 0 Then
Primitive239.visible = 0
Primitive109.visible = 0
Else
Primitive239.visible = 1
Primitive109.visible = 1
End If

if RemovePopPlastic = 0 Then
Primitive110.visible = 1
Else
Primitive110.visible = 0
End If

if Coffin_Mod = 1 Then
ClosedCasket.visible = 1
OpenCasket.visible = 0
Primitive221.visible = 0
Primitive222.visible = 0
Primitive220.visible = 0
Primitive7.visible = 0
End If

if Coffin_Mod = 2 Then
ClosedCasket.visible = 1
OpenCasket.visible = 0
Primitive221.visible = 0
Primitive222.visible = 0
Primitive220.visible = 0
Primitive7.visible = 0
End If

if Coffin_Mod = 0 Then
ClosedCasket.visible = 0
OpenCasket.visible = 0
Primitive221.visible = 1
Primitive222.visible = 1
Primitive220.visible = 1
Primitive7.visible = 1
End If

if AlternateApron = 0 Then
 Flasher001.visible = 1
 AltApron.visible = 0
End If

if AlternateApron = 1 Then
Flasher001.visible = 0
AltApron.visible = 1
End If

if AlternateApron = 2 Then
Flasher001.visible = 0
AltApron.visible = 0
AltApron2.visible = 1
End If


LFlogo.visible = 0
RFlogo.visible = 0


if Flipper_Image = 1 Then
LFlogo.image = "bsd_redflipper_left"
RFlogo.image = "bsd_redflipper_right"
End If

if Flipper_Image = 0 Then
LFlogo.image = "bsd_flipper_left"
RFlogo.image = "bsd_flipper_right"
End If

LFlogo.visible = 1
RFLogo.visible = 1

if Brighter_Props = 0 Then
Primitive239.image = "Dracula Plastics 2"
Primitive29.image = "BurgTextureFINAL"
Primitive30.image = "Village Texture"
Else
Primitive239.image = "Dracula Plastics Brighter"
Primitive29.image = "BurgTextureFINALbrighter"
Primitive30.image = "Village Texture brighter"
End If

If RollCorrection = 0 Then
LFrollfix.enabled = 0
RFrollfix.enabled = 0
Else
LFrollfix.enabled = 1
RFrollfix.enabled = 1
End If


  On Error Resume Next
  ExecuteGlobal GetTextFile("Controller.vbs")
  If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the Scripts folder of Visual Pinball."
  On Error Goto 0
 
' Options
' Volume devided by - lower gets higher sound
 
Const VolDiv = 6000
 
Dim dtxx

 
' Const cSingleLFlip = 0
' Const cSingleRFlip = 0
 
If DesktopMode = True Then
  Ramp15.visible = 1
  Ramp16.visible = 1
  SideWood.visible = 1
else
  Ramp15.visible = 0
  Ramp16.visible = 0
  SideWood.visible = 0
End If
 
If DesktopModeOverwrite = 0 then
If Table1.ShowDT = true then
    HideDMD = 0
else
    HideDMD = 1
end if
end if
 
LoadVPM "01560000", "WPC.VBS", 3.26
 
' Thalamus - for Fast Flip v2
NoUpperRightFlipper
NoUpperLeftFlipper
 
'********************
'Standard definitions
'********************
 
Const UseSolenoids = 2
Const UseLamps = 1
Const UseSync = 0
Const HandleMech = 0
 
' Standard Sounds
'Const SSolenoidOn = "Solenoid"
'Const SSolenoidOff = ""
Const SCoin = "Coin"
 
Set GiCallback2 = GetRef("UpdateGI2")
 
Dim bsTrough, bsCryptPopper, bsBLPopper, bsCastlePopper, bsCoffinPopper
Dim mMagnet, dtLDrop, bsCastleLock, x, bumper1, bumper2, bumper3, plungerIM
Dim LutPos, luts, LutStatusText
luts = array("colorgradelut256x16_1to1SL10", "colorgradelut256x16_1to1SL20", "colorgradelut256x16_1to1SL30", "colorgradelut256x16_1to1SL40", "colorgradelut256x16_1to1SL50", "colorgradelut256x16_1to1SL60","LUTtotan1", "LUTtotan2", "LUTtotan3", "LUTtotan4", "LUTtotan5","LUTtotan6","LUTrobertmstotan0_darker", "LUTblacklight", "LUT1on1", "LUTVogliadicane70", "LUTVogliadicane80", "LUTmandolin", "LUTbassgeige1", "LUTbassgeige2", "LUTbassgeigemeddark", "LUTbassgeigemeddarkwhite", "LUTbassgeigeultrdark", "LUTbassgeigeultrdarkwhite", "LUtfleep", "LUTmlager8", "LUTmlager8night" )
 
On Error Resume Next
Dim i
For i=0 To 127
    If IsObject(eval("L" & i)) Then
    Execute "Set Lights(" & i & ")  = L" & i
    End If
Next
 
Lights(58)=Array(L58,L58A)

if Room_Light_Strength > 0 then
RoomLighting.intensity = Room_Light_Strength
Else
RoomLighting.state = "off"
end if

Table1.bloomstrength = Light_Bloom_Strength

'************
' Table init.
'************
Const cGameName = "drac_l1"

Const KickerAngleTol = 1   		'Number of degrees the kicker angle varies around its intended direction
Const KickerStrengthTol = 2.5   	'Number of strength units the kicker varies around its intended strength
 
Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Bram Stoker's Dracula, Williams, 1993" & vbNewLine & "VP91x table by JPSalas v1.03" & vbNewLine & "VPX conversion by Bigus, extras by Csilk"
        .Games(cGameName).Settings.Value("rol") = RotateDMD 
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = HideDMD   
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
        .Switch(22) = 1 'close coin door
        .Switch(24) = 1 'and keep it close
		'ApronShadow.Opacity = ApronShadowOpacity
		PlayfieldShadow.Opacity = PlayfieldShadowOpacity
		TintLighting.Opacity = PlayfieldTintingOpacity
		'if ApronShadowOpacity = 0 Then ApronShadow.visible = 0 :
		if PlayfieldShadowOpacity = 0 Then PlayfieldShadow.visible = 0 : if PlayfieldTintingOpacity = 0 Then TintLighting.visible = 0 'if they are off then make the whole flasher not visible (maybe save some cpu cycles?)
	

    End With
 
    ' Nudging
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 7
    vpmNudge.TiltObj = Array(bumper1, bumper2, bumper3, LeftSlingshot, RightSlingshot)
 
  ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 48, 41, 42, 43, 44, 0, 0, 0
        .InitKick BallRelease, 90, 3
        .InitEntrySnd "Solenoid", "Solenoid"
        .InitExitSnd SoundFX("ballrel",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .Balls = 4
    End With
 
    ' Crypt Popper
    set bsCryptPopper = new cvpmBallStack
    With bsCryptPopper
        .InitSw 0, 56, 0, 0, 0, 0, 0, 0
        .InitKick sw56, 110, 18.5
        .KickForceVar = RndNum(-KickerStrengthTol,KickerStrengthTol)
        .KickAngleVar = RndNum(-KickerAngleTol,KickerAngleTol) 
        .KickBalls = 2
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .Balls = 0
    End With
 
    ' Wire Ramp Popper
    set bsBLPopper = new cvpmBallStack
    With bsBLPopper
        .InitSw 0, 55, 0, 0, 0, 0, 0, 0
        .InitKick sw55, 180, 1
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .Balls = 0
    End With
 
    ' Castle Popper
    set bsCastlePopper = new cvpmBallStack
    With bsCastlePopper
        .InitSw 0, 71, 0, 0, 0, 0, 0, 0
        .InitKick sw71, 205, 17
        .KickForceVar = RndNum(-KickerStrengthTol,KickerStrengthTol)
        .KickAngleVar = RndNum(-KickerAngleTol,KickerAngleTol)
        .KickBalls = 2
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
    End With
 
    ' Coffin Popper
    set bsCoffinPopper = new cvpmBallStack
    With bsCoffinPopper
        .InitSw 0, 72, 0, 0, 0, 0, 0, 0
        .InitKick sw72, 180, 95 ' was 1... required large change
		.KickForceVar = RndNum(-KickerStrengthTol,KickerStrengthTol)
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
    End With
 
    ' Mist Magnet
    Set mMagnet = New cMagnet
    mMagnet.InitMagnet Magnet, MagnetStrength
    mMagnet.Size = MagnetRadius
    MagnetPos = 0:SetMagnetPosition
 
    ' Drop target
     Set dtLDrop = New cvpmDropTarget
    With dtLDrop
        .InitDrop sw15, 15
        .InitSnd SoundFX("droptarget_l",DOFDropTargets), SoundFX("Solenoid",DOFContactors)
        .CreateEvents "dtLDrop"
    End With
 
    ' Castle Lock
    Set bsCastleLock = new cvpmBallStack
    With bsCastleLock
        .initsw 0, 53, 54, 57, 0, 0, 0, 0
        .InitKick CastleLock, 135, 1
    End With
 
 
    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
 
Plunger.Pullback
Wdivert.Isdropped = 1
Table1.ColorGradeImage = luts(LutPos)
End Sub

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
 
Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub



 
'**********
' Keys
'**********
 
Sub Table1_KeyDown(ByVal Keycode)
    'If keycode = 3 Then SetFlash 131,1


        If keycode = LeftTiltKey Then
        Nudge 90, 5
    End If
 
    If keycode = RightTiltKey Then
        Nudge 270, 5
    End If
 
    If keycode = CenterTiltKey Then
        Nudge 0, 6
    End If
    If vpmKeyDown(keycode) Then Exit Sub


    If keycode = PlungerKey Then 
	Controller.Switch(34) = 1
	End If




'WHEN NOT BELOW

	If FlipperHeldModifier = false then
	if keycode = 181 then 'NUMPAD SLASH
		TextBox.visible = 1
		dim pStatusText : pStatusText = "Extra plastics toggled"
		TextBox.text = pStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + pStatusText + chr(34) + " then TextBox.visible = 0'"
	if Primitive109.visible = 0 then
		primitive239.visible = 1
		primitive109.visible = 1
	Else
		primitive239.visible = 0
		Primitive109.visible = 0
	end if 
	end if 


	If keycode = LeftMagnaSave Then
		if Locked_Lut = 0 Then
		TextBox.visible = 1
		lutpos = lutpos - 1 : If lutpos < 0 Then lutpos = ubound(luts) : end if
		Table1.ColorGradeImage = luts(lutpos)
		LutStatusText = "Using LUT:" & lutpos & " Named:" & " " & luts(lutpos)
		TextBox.text = LutStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + LutStatusText + chr(34) + " then TextBox.visible = 0'"
	End if
	End if

	If keycode = RightMagnaSave Then
		if Locked_Lut = 0 Then
		TextBox.visible = 1
		lutpos = lutpos + 1 : If lutpos > ubound(luts) Then lutpos = 0 : end if
		Table1.ColorGradeImage = luts(lutpos)
		LutStatusText = "Using LUT:" & lutpos & " Named:" & " " & luts(lutpos)
		TextBox.text = LutStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + LutStatusText + chr(34) + " then TextBox.visible = 0'"
	End if
	End if

	If keycode = 51 Then ' Comma
		TextBox.visible = 1
		GI_Intensity = (GI_Intensity - 0.01)
		If GI_Intensity < 0 Then GI_Intensity = 1.00
		dim MinusGIStatusText : MinusGIStatusText = "Global Illumination set to:" & GI_Intensity
		TextBox.text = MinusGIStatusText
		'abba
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + MinusGIStatusText + chr(34) + " then TextBox.visible = 0'" 
		'UpdateGI(0)
		'UpdateGI(1)
		'UpdateGI(2)	
	end if
	If keycode = 52 Then ' Period
		TextBox.visible = 1
		GI_Intensity = (GI_Intensity + 0.01)
		If GI_Intensity > 1.01 Then GI_Intensity = 0
		dim PlusGIStatusText : PlusGIStatusText = "Global Illumination set to:" & GI_Intensity
		TextBox.text = PlusGIStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + PlusGIStatusText + chr(34) + " then TextBox.visible = 0'"
		'UpdateGI(0)
		'UpdateGI(1)
		'UpdateGI(2)	
	end if

	If keycode = 78 Then ' NUMPAD PLUS
		TextBox.visible = 1
		Light_Intensity = (Light_Intensity + 1)
		If Light_Intensity > 201 Then Light_Intensity = 0
		dim PlusLIStatusText : PlusLIStatusText = "Light Intensity set to:" & Light_Intensity
		TextBox.text = PlusLIStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + PlusLIStatusText + chr(34) + " then TextBox.visible = 0'"
	end if

	If keycode = 74 Then ' NUMPAD MINUS
		TextBox.visible = 1
		Light_Intensity = (Light_Intensity - 1)
		If Light_Intensity < 0 Then Light_Intensity = 200
		dim MinusLIStatusText : MinusLIStatusText = "Light Intensity set to:" & Light_Intensity
		TextBox.text = MinusLIStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + MinusLIStatusText + chr(34) + " then TextBox.visible = 0'" 
	end if

	If keycode = 26 Then 'Left Bracket'
		TextBox.visible = 1
		Light_Falloff = (Light_Falloff - 1)
		If Light_Falloff < 0 Then Light_Falloff = 100
		dim Light_FalloffStatusText : Light_FalloffStatusText = "Light falloff: " & Light_Falloff
		TextBox.text = Light_FalloffStatusText
		vpmTimer.AddTimer 1000, "If TextBox.text =" + chr(34) + Light_FalloffStatusText + chr(34) + " then TextBox.visible = 0'"
	end if

	If keycode = 27 Then  'Right Bracket'
		TextBox.visible = 1
		Light_Falloff = (Light_Falloff + 1)
		If Light_Falloff > 100 Then Light_Falloff = 0
		dim Light_Falloff2StatusText : Light_Falloff2StatusText = "Light falloff: " & Light_Falloff
		TextBox.text = Light_Falloff2StatusText
		vpmTimer.AddTimer 1000, "If TextBox.text =" + chr(34) + Light_Falloff2StatusText + chr(34) + " then TextBox.visible = 0'"
	end if

	If keycode = 39 Then 'Colon'
		TextBox.visible = 1
		Light_Falloff_Power = (Light_Falloff_Power - 1)
		If Light_Falloff_Power < 0 Then Light_Falloff_Power = 5
		dim Light_Falloff_PowerStatusText : Light_Falloff_PowerStatusText = "Light falloff power:" & Light_Falloff_Power
		TextBox.text = Light_Falloff_PowerStatusText
		vpmTimer.AddTimer 1000, "If TextBox.text =" + chr(34) + Light_Falloff_PowerStatusText + chr(34) + " then TextBox.visible = 0'"
	end if

	If keycode = 40 Then 'Apostrophe'
		TextBox.visible = 1
		Light_Falloff_Power = (Light_Falloff_Power + 1)
		If Light_Falloff_Power > 5 Then Light_Falloff_Power = 0
		dim Light_Falloff_Power2StatusText : Light_Falloff_Power2StatusText = "Light falloff power:" & Light_Falloff_Power
		TextBox.text = Light_Falloff_Power2StatusText
		vpmTimer.AddTimer 1000, "If TextBox.text =" + chr(34) + Light_Falloff_Power2StatusText + chr(34) + " then TextBox.visible = 0'"
	end if

	if keycode = 203 Then ' left arrow
		TextBox.visible = 1
		Tint_Color_Lighting = (Tint_Color_Lighting + 1)
		If Tint_Color_Lighting > 9 Then Tint_Color_Lighting = 0
		dim TintColorStatusText : TintColorStatusText = "Tint set to:" & Tint_Color_Lighting
		TextBox.text = TintColorStatusText
		UpdateTintLighting(Tint_Color_Lighting)
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + TintColorStatusText + chr(34) + " then TextBox.visible = 0'"
	end if 

	If keycode = 200 Then ' Up arrow
		TextBox.visible = 1
		GI_Top_Color = (GI_Top_Color + 1)
		If GI_Top_Color > 9 Then GI_Top_Color = 0
		dim TopColorStatusText : TopColorStatusText = "Top Color set to:" & GI_Top_Color
		TextBox.text = TopColorStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + TopColorStatusText + chr(34) + " then TextBox.visible = 0'"
		'UpdateGI(0)
		'UpdateGI(1)
		'UpdateGI(2)	
	end if

	If keycode = 208 Then ' Down arrow
		TextBox.visible = 1
		GI_Bot_Color = (GI_Bot_Color + 1)
		If GI_Bot_Color > 9 Then GI_Bot_Color = 0
		dim BotColorStatusText : BotColorStatusText = "Bottom Color set to:" & GI_Bot_Color
		TextBox.text = BotColorStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + BotColorStatusText + chr(34) + " then TextBox.visible = 0'"
		'UpdateGI(0)
		'UpdateGI(1)
		'UpdateGI(2)	
	end if

	If keycode = 205 Then ' right arrow
		TextBox.visible = 1
		GI_Mid_Color = (GI_Mid_Color + 1)
		If GI_Mid_Color > 9 Then GI_Mid_Color = 0
		dim MidColorStatusText : MidColorStatusText = "Middle Color set to:" & GI_Mid_Color
		TextBox.text = MidColorStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + MidColorStatusText + chr(34) + " then TextBox.visible = 0'"
		'UpdateGI(0)
		'UpdateGI(1)
		'UpdateGI(2)	
	end if
	end if

	if keycode = 55 then 'NUMPAD MULTIPLY
		TextBox.visible = 1
		dim cStatusText : cStatusText = "Coffin Mod"
		TextBox.text = cStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + cStatusText + chr(34) + " then TextBox.visible = 0'"
	if ClosedCasket.visible = 0 then
		ClosedCasket.visible = 1
		Coffin_Mod = 1
		Primitive222.visible = 0
		Primitive220.visible = 0
		Primitive7.visible = 0
	Else
		ClosedCasket.visible = 0
		OpenCasket.visible = 0
		Coffin_Mod = 0
		Primitive222.visible = 1
		Primitive220.visible = 1
		Primitive7.visible = 1
	end if 
	end if 

	if keycode = 83 then 'NUMPAD PERIOD
		TextBox.visible = 1
		dim epStatusText : epStatusText = "Pop Plastics"
		TextBox.text = epStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + epStatusText + chr(34) + " then TextBox.visible = 0'"
	if Primitive110.visible = 0 then
		Primitive110.visible = 1
	Else
		Primitive110.visible = 0
	end if 
	end if 


'** WHEN HOLDING MOD KEY
	if FlipperHeldModifier = true then
	if keycode = 200 Then
		TextBox.visible = 1
		'GI_Top_Color = (GI_Top_Color + 1)
		GI_Top_Full_Color = (GI_Top_Full_Color + 1)
		'If GI_Top_Color > 6 Then GI_Top_Color = 0
		If GI_Top_Full_Color > 9 Then GI_Top_Full_Color = 0
		dim TopFullColorStatusText : TopFullColorStatusText = "Top Full Color set to:" & GI_Top_Full_Color
		TextBox.text = TopFullColorStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + TopFullColorStatusText + chr(34) + " then TextBox.visible = 0'"
	end if 



	if keycode = 203 Then ' left arrow
		TextBox.visible = 1
		Table_Color_Lighting = (Table_Color_Lighting + 1)
		If Table_Color_Lighting > 9 Then Table_Color_Lighting = 0
		dim RoomColorStatusText : RoomColorStatusText = "Table lighting color set to:" & Table_Color_Lighting
		TextBox.text = RoomColorStatusText
		UpdateTableLighting(Table_Color_Lighting)
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + RoomColorStatusText + chr(34) + " then TextBox.visible = 0'"
	end if 

	if keycode = 205 Then ' Right arrow
		TextBox.visible = 1
		GI_Mid_Full_Color = (GI_Mid_Full_Color + 1)
		If GI_Mid_Full_Color > 9 Then GI_Mid_Full_Color = 0
		dim MidFullColorStatusText : MidFullColorStatusText = "Mid Full Color set to:" & GI_Mid_Full_Color
		TextBox.text = MidFullColorStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + MidFullColorStatusText + chr(34) + " then TextBox.visible = 0'"
	end if 

	if keycode = 208 Then ' Down Arrow
		TextBox.visible = 1
		GI_Bot_Full_Color = (GI_Bot_Full_Color + 1)
		If GI_Bot_Full_Color > 9 Then GI_Bot_Full_Color = 0
		dim BotFullColorStatusText : BotFullColorStatusText = "Bot Full Color set to:" & GI_Bot_Full_Color
		TextBox.text = BotFullColorStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + BotFullColorStatusText + chr(34) + " then TextBox.visible = 0'"
	end if 
	

	If keycode = 78 Then ' NUMPAD PLUS
		TextBox.visible = 1
		TableLightingStrength = (TableLightingStrength + 0.1)
		If TableLightingStrength > 5 Then TableLightingStrength = 0
		dim PlusALStatusText : PlusALStatusText = "Flood Light Strength set to:" & TableLightingStrength
		TextBox.text = PlusALStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + PlusALStatusText + chr(34) + " then TextBox.visible = 0'"
		UpdateTableLighting(Table_Color_Lighting)
	end if

	If keycode = 74 Then ' NUMPAD MINUS
		TextBox.visible = 1
		TableLightingStrength = (TableLightingStrength - 0.1)
		If TableLightingStrength < 0 Then TableLightingStrength = 5
		dim MinusALStatusText : MinusALStatusText = "Flood Light Strength set to:" & TableLightingStrength
		TextBox.text = MinusALStatusText
		vpmTimer.AddTimer 2000, "If TextBox.text =" + chr(34) + MinusALStatusText + chr(34) + " then TextBox.visible = 0'" 
		UpdateTableLighting(Table_Color_Lighting)
	end if
end if





End Sub
 
Sub Table1_KeyUp(ByVal Keycode)
    'If keycode = 3 Then SetFlash 131,0
    If keycode = PlungerKey Then Controller.Switch(34) = 0

	if keycode = RightFlipperKey Then
	FlipperDeActivate RightFlipper, RFPress
	SolRFlipper false
	'PlaySoundAt SoundFX("flipperdown",DOFFlippers),RightFlipper:RightFlipper.RotateToStart
	End If

	if keycode = LeftFlipperKey Then
	FlipperDeActivate LeftFlipper, LFPress
	SolLFlipper false
	'PlaySoundAt SoundFX("flipperdown",DOFFlippers),LeftFlipper:LeftFlipper.RotateToStart
	End If

    If vpmKeyUp(keycode) Then Exit Sub
End Sub

'**********
' Coloring
'**********
SleeveColoring
PostRubberColoring
RingColoring
RubberBandColoring

'**********
' Update CSILK Flashers
'********** 
Sub UpdateTintLighting(col)
'0 = White
'1 = Red
'2 = Green
'3 = Blue
'4 = Purple
'5 = Magenta
'6 = Orange
'7 = Yellow
'8 = Pink
'9 = Black
if col = 0 then TintLighting.color = White
if col = 1 then TintLighting.color = Red
if col = 2 then TintLighting.color = Green
if col = 3 then TintLighting.color = Blue
if col = 4 then TintLighting.color = Purple
if col = 5 then TintLighting.color = Magenta
if col = 6 then TintLighting.color = Orange
if col = 7 then TintLighting.color = Yellow
if col = 8 then TintLighting.color = Pink
if col = 9 then TintLighting.color = Black

End Sub

Sub UpdateTableLighting(col)
'0 = White
'1 = Red
'2 = Green
'3 = Blue
'4 = Purple
'5 = Magenta
'6 = Orange
'7 = Yellow
'8 = Pink
'9 = Black
if col = 0 then
AmbientLighting.color = White
AmbientLighting.colorfull = FullWhite
end if
if col = 1 then
AmbientLighting.color = Red
AmbientLighting.colorfull = FullRed
end if
if col = 2 then
AmbientLighting.color = Green
AmbientLighting.colorfull = FullGreen
end if
if col = 3 then
AmbientLighting.color = Blue
AmbientLighting.colorfull = FullBlue
end if
if col = 4 then
AmbientLighting.color = Purple
AmbientLighting.colorfull = FullPurple
end if
if col = 5 then
AmbientLighting.color = Magenta
AmbientLighting.colorfull = FullMagenta
end if
if col = 6 then
AmbientLighting.color = Orange
AmbientLighting.colorfull = FullOrange
end if
if col = 7 then
AmbientLighting.color = Yellow
AmbientLighting.colorfull = FullYellow
end if
if col = 8 then
AmbientLighting.color = Pink
AmbientLighting.colorfull = FullPink
end if
if col = 9 then
AmbientLighting.color = Black
AmbientLighting.colorfull = FullBlack
end if

AmbientLighting.intensity = TableLightingStrength

End Sub

'*********
' Switches
'*********

' Slings & div switches
Dim LStep, RStep





Sub LeftSlingShot_Slingshot
if VPW_Sling_Correction = 1 then
' Access the ball that triggered the left slingshot event and correct it with VPW corrections
 LS.VelocityCorrect(ActiveBall)
end if

  ' PlaySound "slingshot", 0, 0.3, -0.1, 0.25, 0, 1, AudioFade(sling2)
  RandomSoundSlingshotLeft()
  vpmTimer.PulseSw 64
  LSling.Visible = 0
  LSling1.Visible = 1
  sling2.TransZ = -20
  LStep = 0
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




Sub RightSlingShot_Slingshot
if VPW_Sling_Correction = 1 then
' Access the ball that triggered the left slingshot event and correct it with VPW corrections
 RS.VelocityCorrect(ActiveBall)
end if


  ' PlaySound "slingshot", 0, 0.3, 0.1, 0.25, 0, 1, AudioFade(sling1)
  RandomSoundSlingshotRight()
  vpmTimer.PulseSw 65
  RSling.Visible = 0
  RSling1.Visible = 1
  sling1.TransZ = -20
  RStep = 0
  RightSlingShot.TimerEnabled = 1
  RightSlingShot.TimerInterval = 10
End Sub

Sub RightSlingShot_Timer
  Select Case RStep
    Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
    Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
  End Select
  RStep = RStep + 1
End Sub


' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 61:RandomSoundBumperA:End Sub

Sub Bumper1_Timer()
  Select Case bump1
    Case 1:Ring1a.IsDropped = 0:bump1 = 2
    Case 2:Ring1b.IsDropped = 0:Ring1a.IsDropped = 1:bump1 = 3
    Case 3:Ring1c.IsDropped = 0:Ring1b.IsDropped = 1:bump1 = 4
    Case 4:Ring1c.IsDropped = 1:Me.TimerEnabled = 0
  End Select
End Sub

Sub Bumper2_Hit:vpmTimer.PulseSw 62:RandomSoundBumperB:End Sub
Sub Bumper2_Timer()
  Select Case bump2
    Case 1:Ring2a.IsDropped = 0:bump2 = 2
    Case 2:Ring2b.IsDropped = 0:Ring2a.IsDropped = 1:bump2 = 3
    Case 3:Ring2c.IsDropped = 0:Ring2b.IsDropped = 1:bump2 = 4
    Case 4:Ring2c.IsDropped = 1:Me.TimerEnabled = 0
  End Select
End Sub

Sub Bumper3_Hit:vpmTimer.PulseSw 63:RandomSoundBumperC:End Sub
Sub Bumper3_Timer()
  Select Case bump3
    Case 1:Ring3a.IsDropped = 0:bump3 = 2
    Case 2:Ring3b.IsDropped = 0:Ring3a.IsDropped = 1:bump3 = 3
    Case 3:Ring3c.IsDropped = 0:Ring3b.IsDropped = 1:bump3 = 4
    Case 4:Ring3c.IsDropped = 1:Me.TimerEnabled = 0
  End Select
End Sub

' Drain holes, vuks & saucers
Sub Drain_Hit:RandomSoundDrain():bsTrough.AddBall Me:End Sub
Sub Drain1_Hit:RandomSoundDrain():ClearBallID:bsTrough.AddBall Me:End Sub
Sub Drain2_Hit:RandomSoundDrain():ClearBallID:bsTrough.AddBall Me:End Sub
Sub Drain3_Hit:RandomSoundDrain():ClearBallID:bsTrough.AddBall Me:End Sub
Sub Drain4_Hit:RandomSoundDrain():ClearBallID:bsTrough.AddBall Me:End Sub

'Sub sw72a_Hit:PlaySoundAt "fx_Hole2", sw72a:bsCoffinPopper.addball Me:End Sub

Dim rball

Sub sw72a_Hit:PlaySoundAtVol "fx_Hole2", sw72a, 1:me.destroyball:set rball = me.createball:drop.enabled = 1:End Sub

Sub drop_timer()
  If rball.Z <= -50 Then
    me.enabled = 0
    drop2.enabled = 1
  End If
  rball.Z = rball.Z - 1
End Sub

Sub drop2_timer()
  sw72a.destroyball
  bsCoffinPopper.addball rball
  me.enabled = 0
End Sub

Sub sw71_Hit
  PlaySound "hole_enter", 0, 0.3, AudioPan(sw71), 0.25, 0, 1, 0, AudioFade(sw71)
  vpmTimer.PulseSwitch 71, 0, 0
  mMagnet.RemoveBall ActiveBall
  Me.destroyball
  bsCastlePopper.AddBall Me
End Sub

Sub sw58_Hit
  PlaySound "hole_enter", 0, 0.3, AudioPan(sw58), 0.25, 0, 1, 0, AudioFade(sw58)
  Me.DestroyBall
  PlaySoundAtVol "subway2", sw58, 1
  vpmTimer.PulseSwitch 58, 1250, "bsBLPopper.AddBall 0 '"
End Sub

Sub sw56_Hit
  PlaySound "hole_enter", 0, 0.3, AudioPan(sw56), 0.25, 0, 1, 0, AudioFade(sw56)
  vpmTimer.PulseSwitch 56, 100, 0
  mMagnet.RemoveBall ActiveBall
  bsCryptPopper.AddBall Me
End Sub

Sub sw56a_Hit
  PlaySound "hole_enter", 0, 0.3, AudioPan(sw56a), 0.25, 0, 1, 0, AudioFade(sw56a)
  mMagnet.RemoveBall ActiveBall
  bsCryptPopper.AddBall Me
End Sub

Sub CastleLock_Hit()
  PlaysoundAtVol "metalhit2", CastleLock, 1
  bsCastleLock.AddBall Me
End Sub

' Rollovers & Ramp Switches
Sub sw35_Hit:Controller.Switch(35) = 1:RandomSoundRollover():End Sub
Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub

Sub sw36_Hit:Controller.Switch(36) = 1:RandomSoundRollover():End Sub
Sub sw36_UnHit:Controller.Switch(36) = 0:End Sub

Sub sw37_Hit:Controller.Switch(37) = 1:RandomSoundRollover():End Sub
Sub sw37_UnHit:Controller.Switch(37) = 0:End Sub

Sub sw38_Hit:Controller.Switch(38) = 1:RandomSoundRollover():End Sub
Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub

Sub sw25_Hit:Controller.Switch(25) = 1:RandomSoundRollover():End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub

Sub sw26_Hit:Controller.Switch(26) = 1:RandomSoundRollover():End Sub
Sub sw26_Unhit:Controller.Switch(26) = 0:End Sub

Sub sw27_Hit:Controller.Switch(27) = 1:RandomSoundRollover():End Sub
Sub sw27_Unhit:Controller.Switch(27) = 0:End Sub

Sub sw16_Hit:Controller.Switch(16) = 1:RandomSoundRollover():End Sub
Sub sw16_Unhit:Controller.Switch(16) = 0:End Sub

Sub sw28_Hit:Controller.Switch(28) = 1:End Sub
Sub sw28_Unhit:Controller.Switch(28) = 0:End Sub

Sub sw84_Hit:Controller.Switch(84) = 1:End Sub
Sub sw84_Unhit:Controller.Switch(84) = 0:End Sub

Sub sw85_Hit:Controller.Switch(85) = 1:End Sub
Sub sw85_Unhit:Controller.Switch(85) = 0:End Sub

Sub sw31_Hit:Controller.Switch(31) = 1:RandomSoundRollover():End Sub
Sub sw31_Unhit:Controller.Switch(31) = 0:End Sub

Sub sw51_Hit:Controller.Switch(51) = 1:RandomSoundRollover():End Sub
Sub sw51_Unhit:Controller.Switch(51) = 0:End Sub

Sub sw52_Hit:Controller.Switch(52) = 1:RandomSoundRollover():End Sub
Sub sw52_Unhit:Controller.Switch(52) = 0:End Sub

Sub sw73_Hit
  Controller.Switch(73) = 1
  If ActiveBall.VelY < -25 Then
    PlaySoundAt "Subway2",sw73
  End If
End Sub

Sub sw73_Unhit:Controller.Switch(73) = 0:End Sub

Sub sw17_Hit:Controller.Switch(17) = 1:End Sub
Sub sw17_Unhit:Controller.Switch(17) = 0:End Sub

' Targets
Sub sw66_Hit:vpmTimer.PulseSw 66:PlaySoundAt "target",sw66:End Sub
Sub sw66_Timer:sw66.IsDropped = 0:End Sub

Sub sw67_Hit:vpmTimer.PulseSw 67:PlaySoundAt "target",sw67:End Sub
Sub sw67_Timer:sw67.IsDropped = 0:End Sub

Sub sw68_Hit:vpmTimer.PulseSw 68:PlaySoundAt "target",sw68:End Sub
Sub sw68_Timer:sw68.IsDropped = 0:End Sub

Sub sw86_Hit:vpmTimer.PulseSw 86:PlaySoundAt "target",sw86:End Sub
Sub sw86_Timer:sw86.IsDropped = 0:End Sub

Sub sw87_Hit:vpmTimer.PulseSw 87:PlaySoundAt "target",sw87:End Sub
Sub sw87_Timer:sw87.IsDropped = 0:End Sub

Sub sw88_Hit:vpmTimer.PulseSw 88:PlaySoundAt "target",sw88:End Sub
Sub sw88_Timer:sw88.IsDropped = 0:Me.TimerEnabled = 0:End Sub
 
' Ramps helpers
'Sub RHelp1_Hit()
'    StopSound "metalrolling"
'    PlaySound "ballhit"
'End Sub
 
'Sub RHelp2_Hit()
 '   StopSound "metalrolling"
 '   PlaySound "ballhit"
'End Sub
 
'Sub RHelp3_Hit()
 '   StopSound "metalrolling"
 '   PlaySound "ballhit"
'End Sub
 
'*********
'Solenoids
'*********
 
SolCallback(1) = "Auto_Plunger"
SolCallback(2) = "bsCoffinPopper.SolOut"
SolCallback(3) = "bsCastlePopper.SolOut"
SolCallback(4) = "SolRRampDown"
SolCallback(5) = "bsCryptPopper.SolOut"
SolCallback(6) = "bsBLPopper.SolOut"
SolCallback(7) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(8) = "SolShooterRamp"
SolCallback(14) = "SolRRampUp"
SolCallback(15) = "bsTrough.SolIn"
SolCallback(16) = "bsTrough.SolOut"
 
SolCallback(17) = "Sol117"
SolCallback(18) = "Sol118"
SolCallback(19) = "Sol119"
SolCallback(20) = "Sol120"
SolCallback(21) = "Sol121"
SolCallback(22) = "Sol122"
SolCallback(23) = "Sol123"
SolCallback(24) = "Sol124"
SolCallback(25) = "dtLDrop.SolDropUp"
SolCallback(27) = "SolMistMagnet"
SolCallback(33) = "solTopDiverter"
SolCallback(34) = "SolRGate"
SolCallback(35) = "bsCastleLock.SolOut"
SolCallback(36) = "SolLGate"

SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sLRFlipper) = "SolRFlipper"
 
Sub Auto_Plunger(Enabled)
    If Enabled Then
        Plunger.Fire
        PlaySound SoundFX("solenoid",DOFContactors), 0, 0.3, 0.1, 0.25
Else
        Plunger.PullBack
    End If
End Sub
 
'*************
' Moving Ramp
'*************
 
Dim RRampDir, RRAmpCurrPos, RRamp
RRampCurrPos = 0 ' down
RRampDir = 1     '1 is up -1 is down dir
Controller.Switch(77) = False
'RightRamp.Collidable = True
 
if Coffin_mod = 2 Then
ClosedCasket.visible = 0
OpenCasket.visible = 0
end if

Sub SolRRampUp(Enabled)
    If Enabled Then
        RRampDir = 1
        Controller.Switch(77) = True
        RightRamp.Collidable = False
        UpdateRamp.Enabled = True
        ''SetLamp 116, 1
        playsound SoundFX("diverter",DOFGear), 0, 0.3, 0.1, 0.25
		if Coffin_Mod = 1 Then
		ClosedCasket.visible = 0
		OpenCasket.visible = 1
		Table1.ColorGradeImage = luts(OpenCoffinLutPos)
		End If
    End If
End Sub
 
Sub SolRRampDown(Enabled)
    If Enabled Then
        RRampDir = -1
        Controller.Switch(77) = False
        RightRamp.Collidable = True
        UpdateRamp.Enabled = True
        ''SetLamp 116, 0
        playsound SoundFX("diverter",DOFGear), 0, 0.3, 0.1, 0.25
		if Coffin_Mod = 1 Then
		ClosedCasket.visible = 1
		OpenCasket.visible = 0
		Table1.ColorGradeImage = luts(LutPos)
		End If
    End If
End sub
 
Sub UpdateRamp_Timer
    RRampCurrPos = RRampCurrPos + RRampDir
    If RRampCurrPos> 10 Then
        RRampCurrPos = 10
        UpdateRamp.Enabled = 0
    End If
    If RRampCurrPos <0 Then
        RRampCurrPos = 0
        UpdateRamp.Enabled = 0
    End If
    RightRamp2.HeightBottom = RRampCurrPos *5
    CoffinLiftRampOpaque.RotX = RRampCurrPos -11
    CoffinLiftRamp.RotX = RRampCurrPos -11
    Refresh.State = 1
    Refresh.State = 0
End Sub
 
' Shooter Ramp
 
Sub SolShooterRamp(Enabled)
    If Enabled Then
        sramp2.Collidable = 0
        dirsrt = 1:shootramp.enabled = 1
        Playsound SoundFX("solenoid",DOFGear), 0, 0.3, 0.1, 0.25
    Else
        sramp2.Collidable = 1
        dirsrt = 2:shootramp.enabled = 1
        Playsound SoundFX("solenoid",DOFGear), 0, 0.3, 0.1, 0.25
    End If
End Sub
 
' Top Ramp Diverter
 
Sub SolTopDiverter(Enabled)
    Playsound SoundFX("Diverter",DOFContactors)
    If Enabled Then
        'diverter1.isdropped = False
        'Controller.Switch(78) = True
 
        wDivert.isdropped = 0
        ddir = 1:Divert.enabled = 1
    Else
        'diverter1.isdropped = True
        'Controller.Switch(78) = False
        WDivert.isdropped = 1
        ddir = 2:Divert.enabled = 1
    End If
End sub
 
Dim FlashState(200), FlashLevel(200)
Dim FlashSpeedUp, FlashSpeedDown
'Dim x
 
FlashInit()
FlasherTimer.Interval = Flash_Fade_Speed 'flash fading speed
FlasherTimer.Enabled = 1
 
Sub FlashInit
    Dim i
    For i = 0 to 200
        FlashState(i) = 0
        FlashLevel(i) = 0
    Next
 
    FlashSpeedUp = 500   ' fast speed when turning on the flasher
    FlashSpeedDown = 100 ' slow speed when turning off the flasher, gives a smooth fading
    AllFlashOff()
End Sub
 
Sub AllFlashOff
    Dim i
    For i = 0 to 600
        FlashState(i) = 0
    Next
End Sub
 
Sub Flash(nr, object)
    Select Case FlashState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown
            If FlashLevel(nr) < 0 Then
                FlashLevel(nr) = 0
                FlashState(nr) = -1 'completely off
            End if
            Object.opacity = FlashLevel(nr)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp
            If FlashLevel(nr) > 1000 Then
                FlashLevel(nr) = 1000
                FlashState(nr) = -2 'completely on
            End if
            Object.opacity = FlashLevel(nr)
    End Select
End Sub
 
Sub Flashm(nr, object) 'multiple flashers, it doesn't change the flashstate
    Select Case FlashState(nr)
        Case 0         'off
            Object.opacity = FlashLevel(nr)
        Case 1         ' on
            Object.opacity = FlashLevel(nr)
    End Select
End Sub
 
Sub SetFlash(nr, stat)
    FlashState(nr) = ABS(stat)
End Sub
 
Sub FlasherTimer_Timer()
Flash 161, f61
Flash 162, f62
Flash 163, f63
'Flashm 158, f58
'Flash 158, f58a
Flashm 121, F121
Flash 121, F121a
Flashm 122, F122
Flash 122, F122a




End Sub
 
Dim ddir
 
Sub Divert_Timer()
Select Case ddir
Case 1:
If Diverter.Z = -315 Then
me.enabled = 0
Controller.Switch(78) = True
Diverter.Z = -315
End If
Diverter.Z = Diverter.Z - 1
Case 2:
If Diverter.Z = -280 Then
me.enabled = 0
Controller.Switch(78) = False
Diverter.Z = -280
End If
Diverter.Z = Diverter.Z + 1
End Select
End Sub
 
 
' Mist Gates
 
Sub SolLGate(Enabled)
    If Enabled then
        LGate.open = 1
        Wall_LO.isdropped = 1
    else
        LGate.open = 0
        Wall_LO.isdropped = 0
    End If
End Sub
 
Sub SolRGate(Enabled)
    If Enabled then
        RGate.open = 1
        'Light51.state = 1
        'RGate.Move 1, 1, 90
        RGateWall.IsDropped = True
    else
        RGate.open = 0
        'Light51.state = 0
        'RGate.Move 0, 1, 0
        RGateWall.IsDropped = False
    End If
End Sub
 
Sub Sol117(Enabled)
F17.State=(Enabled)
F17a.State=(Enabled)
F17b.State=(enabled)
F17.intensity = Flasher_Intensity
F17.falloff = Flasher_Falloff
F17.falloffpower = Flasher_Falloff_Power
F17a.intensity = Flasher_Intensity
F17a.falloff = Flasher_Falloff
F17a.falloffpower = Flasher_Falloff_Power
F17b.intensity = Flasher_Intensity
F17b.falloff = Flasher_Falloff
F17b.falloffpower = Flasher_Falloff_Power
	if Additional_Flashers = 1 Then
	NewFlash2.State=(Enabled)
	NewFlash2.intensity = Flasher_Intensity
	NewFlash2.falloff = Flasher_Falloff
	NewFlash2.falloffpower = Flasher_Falloff_Power
	End If
End Sub
 
Sub Sol118(Enabled)
F18.State=(Enabled)
F18.intensity = Flasher_Intensity
F18.falloff = Flasher_Falloff
End Sub
 
Sub Sol119(Enabled)
F19.State=(Enabled)
F19a.State=(Enabled)
F19.intensity = Flasher_Intensity
F19.falloff = Flasher_Falloff
F19.falloffpower = Flasher_Falloff_Power
F19a.intensity = Flasher_Intensity
F19a.falloff = Flasher_Falloff
F19a.falloffpower = Flasher_Falloff_Power
End Sub
 
Sub Sol120(Enabled)
F20.State=(Enabled)
F20a.State=(Enabled)
F20.intensity = Flasher_Intensity
F20.falloff = Flasher_Falloff
F20.falloffpower = Flasher_Falloff_Power
F20a.intensity = Flasher_Intensity
F20a.falloff = Flasher_Falloff
F20a.falloffpower = Flasher_Falloff_Power
End Sub
 
Sub Sol121(enabled)
F21.State=(Enabled)
F21a.State=(Enabled)
F21.intensity = Flasher_Intensity
F21.falloff = Flasher_Falloff
F21.falloffpower = Flasher_Falloff_Power
F21a.intensity = Flasher_Intensity
F21a.falloff = Flasher_Falloff
F21a.falloffpower = Flasher_Falloff_Power
End Sub
 
Sub Sol122(enabled)
F22.State=(Enabled)
F22.intensity = Flasher_Intensity
F22.falloff = Flasher_Falloff
F22.falloffpower = Flasher_Falloff_Power
End Sub
 
Sub Sol123(enabled)
F23.State=(Enabled)
F23.intensity = Flasher_Intensity
F23.falloff = Flasher_Falloff
F23.falloffpower = Flasher_Falloff_Power

End Sub
 
Sub Sol124(enabled)
F24.State=(Enabled)
F24.intensity = Flasher_Intensity
F24.falloff = Flasher_Falloff
F24.falloffpower = Flasher_Falloff_Power

	if Additional_Flashers = 1 Then
	NewFlash3.State=(Enabled)
	NewFlash3.intensity = Flasher_Intensity
	NewFlash3.falloff = Flasher_Falloff
	NewFlash3.falloffpower = Flasher_Falloff_Power
	End If

End Sub
 

Sub RingColoring
dim prims, peg, mat
'Primitive5.Material = "Red Peg"
'Primitive5.visible = 0




	if Ring_Color = 0 Then
	mat = "PegWhite"
	End If

	if Ring_Color = 1 Then
	mat ="PegRed"
	End If

	if Ring_Color = 2 Then
	mat = "PegGreen"
	End If

	if Ring_Color = 3 Then
	mat = "PegBlue"
	End If

	if Ring_Color = 4 Then
	mat = "PegPurple"
	End If

	if Ring_Color = 5 Then
	mat = "PegMagenta"
	End If

	if Ring_Color = 6 Then
	mat = "PegOrange"
	End If

	if Ring_Color = 7 Then
	mat = "PegYellow"
	End If

	if Ring_Color = 8 Then
	mat = "PegPink"
	End If

	if Ring_Color = 9 Then
	mat = "PegBlack"
	End If

for Each prims in Array(ColoredRings)
for Each peg In prims

		peg.material = mat
		'LutStatusText = "DEBUG"
		'TextBox.text = obj.name
		'vpmTimer.AddTimer 5000, "If TextBox.text =" + chr(34) + LutStatusText + chr(34) + " then TextBox.visible = 0'"

	next
next
end sub


'Color Codes are
'
'0 = White
'1 = Red
'2 = Green
'3 = Blue
'4 = Purple
'5 = Magenta
'6 = Orange
'7 = Yellow
'8 = Pink
'9 = Black
'

Sub SleeveColoring
dim prims, post, img
'Primitive5.Material = "Red Peg"
'Primitive5.visible = 0

	if Sleeve_Color = 0 Then
	img = "rpwhite"
	End If

	if Sleeve_Color = 1 Then
	img = "rpred"
	End If

	if Sleeve_Color = 2 Then
	img = "rpgreen"
	End If

	if Sleeve_Color = 3 Then
	img = "rpblue"
	End If

	if Sleeve_Color = 4 Then
	img = "rppurple"
	End If

	if Sleeve_Color = 5 Then
	img = "rporange"
	End If

	if Sleeve_Color = 6 Then
	img = "rpmagenta"
	End If

	if Sleeve_Color = 7 Then
	img = "rpyellow"
	End If

	if Sleeve_Color = 8 Then
	img = "rppink"
	End If

	if Sleeve_Color = 9 Then
	img = "rpblack"
	End If

for Each prims in Array(ColoredSleeves)
for Each post In prims

		post.image = img
		'LutStatusText = "DEBUG"
		'TextBox.text = obj.name
		'vpmTimer.AddTimer 5000, "If TextBox.text =" + chr(34) + LutStatusText + chr(34) + " then TextBox.visible = 0'"

	next
next
end sub

Sub PostRubberColoring
dim collectionitem, postrubber, mat

'Color Codes are
'
'0 = White
'1 = Red
'2 = Green
'3 = Blue
'4 = Purple
'5 = Magenta
'6 = Orange
'7 = Yellow
'8 = Pink
'9 = Black
'

	if Post_Rubber_Color = 0 Then
	mat = "PegWhite"
	End If

	if Post_Rubber_Color = 1 Then
	mat ="PegRed"
	End If

	if Post_Rubber_Color = 2 Then
	mat = "PegGreen"
	End If

	if Post_Rubber_Color = 3 Then
	mat = "PegBlue"
	End If

	if Post_Rubber_Color = 4 Then
	mat = "PegPurple"
	End If

	if Post_Rubber_Color = 5 Then
	mat = "PegMagenta"
	End If

	if Post_Rubber_Color = 6 Then
	mat = "PegOrange"
	End If

	if Post_Rubber_Color = 7 Then
	mat = "PegYellow"
	End If

	if Post_Rubber_Color = 8 Then
	mat = "PegPink"
	End If

	if Post_Rubber_Color = 9 Then
	mat = "PegBlack"
	End If

for Each collectionitem in Array(ColoredRubberPosts)
for Each postrubber In collectionitem

		postrubber.material = Mat
		'LutStatusText = "DEBUG"
		'TextBox.text = obj.name
		'vpmTimer.AddTimer 5000, "If TextBox.text =" + chr(34) + LutStatusText + chr(34) + " then TextBox.visible = 0'"

	next
next
end sub

Sub RubberBandColoring
dim collectionitem, rubberband, mat


	if Rubber_Band_Color = 0 Then
	mat = "PegWhite"
	End If

	if Rubber_Band_Color = 1 Then
	mat ="PegRed"
	End If

	if Rubber_Band_Color = 2 Then
	mat = "PegGreen"
	End If

	if Rubber_Band_Color = 3 Then
	mat = "PegBlue"
	End If

	if Rubber_Band_Color = 4 Then
	mat = "PegPurple"
	End If

	if Rubber_Band_Color = 5 Then
	mat = "PegMagenta"
	End If

	if Rubber_Band_Color = 6 Then
	mat = "PegOrange"
	End If

	if Rubber_Band_Color = 7 Then
	mat = "PegYellow"
	End If

	if Rubber_Band_Color = 8 Then
	mat = "PegPink"
	End If

	if Rubber_Band_Color = 9 Then
	mat = "PegBlack"
	End If

for Each collectionitem in Array(ColoredBands)
for Each rubberband In collectionitem

		rubberband.material = Mat
		'LutStatusText = "DEBUG"
		'TextBox.text = obj.name
		'vpmTimer.AddTimer 5000, "If TextBox.text =" + chr(34) + LutStatusText + chr(34) + " then TextBox.visible = 0'"

	next
next
end sub



UpdateTableLighting(Table_Color_Lighting)


'***********
' Update GI
'***********
 
Dim gistep, xxx
gistep = 1 / 8
 
Sub UpdateGI2(no, gistep)


dim obj, coll

dim Top_Color, Top_Full_Color, Mid_Color, Mid_Full_Color, Bot_Color, Bot_Full_Color

if GI_Top_Color = 0 Then Top_Color = White
if GI_Top_Color = 1 Then Top_Color = Red
if GI_Top_Color = 2 Then Top_Color = Green
if GI_Top_Color = 3 Then Top_Color = Blue
if GI_Top_Color = 4 then Top_Color = Purple
if GI_Top_Color = 5 then Top_Color = Magenta
if GI_Top_Color = 6 then Top_Color = Orange
if GI_Top_Color = 7 then Top_Color = Yellow
if GI_Top_Color = 8 then Top_Color = Pink
if GI_Top_Color = 9 then Top_Color = Black


if GI_Top_Full_Color = 0 Then Top_Full_Color = FullWhite
if GI_Top_Full_Color = 1 Then Top_Full_Color = FullRed
if GI_Top_Full_Color = 2 Then Top_Full_Color = FullGreen
if GI_Top_Full_Color = 3 Then Top_Full_Color = FullBlue
if GI_Top_Full_Color = 4 then Top_Full_Color = FullPurple
if GI_Top_Full_Color = 5 then Top_Full_Color = FullMagenta
if GI_Top_Full_Color = 6 then Top_Full_Color = FullOrange
if GI_Top_Full_Color = 7 then Top_Full_Color = FullYellow
if GI_Top_Full_Color = 8 then Top_Full_Color = FullPink
if GI_Top_Full_Color = 9 then Top_Full_Color = FullBlack

if GI_Bot_Color = 0 Then Bot_Color = White
if GI_Bot_Color = 1 Then Bot_Color = Red
if GI_Bot_Color = 2 Then Bot_Color = Green
if GI_Bot_Color = 3 Then Bot_Color = Blue
if GI_Bot_Color = 4 then Bot_Color = Purple
if GI_Bot_Color = 5 then Bot_Color = Magenta
if GI_Bot_Color = 6 then Bot_Color = Orange
if GI_Bot_Color = 7 then Bot_Color = Yellow
if GI_Bot_Color = 8 then Bot_Color = Pink
if GI_Bot_Color = 9 then Bot_Color = Black


if GI_Bot_Full_Color = 0 Then Bot_Full_Color = FullWhite
if GI_Bot_Full_Color = 1 Then Bot_Full_Color = FullRed
if GI_Bot_Full_Color = 2 Then Bot_Full_Color = FullGreen
if GI_Bot_Full_Color = 3 Then Bot_Full_Color = FullBlue
if GI_Bot_Full_Color = 4 then Bot_Full_Color = FullPurple
if GI_Bot_Full_Color = 5 then Bot_Full_Color = FullMagenta
if GI_Bot_Full_Color = 6 then Bot_Full_Color = FullOrange
if GI_Bot_Full_Color = 7 then Bot_Full_Color = FullYellow
if GI_Bot_Full_Color = 8 then Bot_Full_Color = FullPink
if GI_Bot_Full_Color = 9 then Bot_Full_Color = FullBlack

if GI_Mid_Color = 0 Then Mid_Color = White
if GI_Mid_Color = 1 Then Mid_Color = Red
if GI_Mid_Color = 2 Then Mid_Color = Green
if GI_Mid_Color = 3 Then Mid_Color = Blue
if GI_Mid_Color = 4 then Mid_Color = Purple
if GI_Mid_Color = 5 then Mid_Color = Magenta
if GI_Mid_Color = 6 then Mid_Color = Orange
if GI_Mid_Color = 7 then Mid_Color = Yellow
if GI_Mid_Color = 8 then Mid_Color = Pink
if GI_Mid_Color = 9 then Mid_Color = Black

if GI_Mid_Full_Color = 0 Then Mid_Full_Color = FullWhite
if GI_Mid_Full_Color = 1 Then Mid_Full_Color = FullRed
if GI_Mid_Full_Color = 2 Then Mid_Full_Color = FullGreen
if GI_Mid_Full_Color = 3 Then Mid_Full_Color = FullBlue
if GI_Mid_Full_Color = 4 then Mid_Full_Color = FullPurple
if GI_Mid_Full_Color = 5 then Mid_Full_Color = FullMagenta
if GI_Mid_Full_Color = 6 then Mid_Full_Color = FullOrange
if GI_Mid_Full_Color = 7 then Mid_Full_Color = FullYellow
if GI_Mid_Full_Color = 8 then Mid_Full_Color = FullPink
if GI_Mid_Full_Color = 9 then Mid_Full_Color = FullBlack



If No_Light_Edits = 0 Then 'only if lights are set to be editable
for Each coll in Array(Light)
for Each obj In coll
	obj.Intensity = Light_Intensity
	obj.Falloff = Light_Falloff
	obj.Falloffpower = Light_Falloff_Power
	obj.IntensityScale = 1
	next
next
End If

If No_Light_Edits = 1 Then
for Each coll in Array(Light)
for Each obj In coll
	obj.Intensity = obj.Intensity
	obj.Falloff = obj.falloff
	obj.Falloffpower = obj.Falloffpower
	obj.IntensityScale = obj.IntensityScale
	next
next
End If


 

if MoonLighting = 1 Then
		'obj.state = 1
		MoonLight.intensity = Light_Intensity
		MoonLight.falloff = Light_Falloff
		MoonLight.falloffpower = Light_Falloff_Power
		Else
		MoonLight.state = 0
		MoonLight.intensity = 0
		MoonLight.falloff = 0
		MoonLight.falloffpower = 0
end if


 
   Select Case no
 
        Case 0
 
   For each xxx in GIBOT:xxx.IntensityScale = GI_Intensity * gistep: xxx.Color = Bot_Color: xxx.ColorFull = Bot_Full_Color:next

		if Additional_Bulbs = 1 Then
		   For each xxx in GIBOTmore:xxx.IntensityScale = GI_Intensity * gistep: xxx.Color = Bot_Color: xxx.ColorFull = Bot_Full_Color:next
		End if
 
        Case 1
 
   For each xxx in GITOP:xxx.IntensityScale = GI_Intensity * gistep: xxx.Color = Top_Color: xxx.ColorFull = Top_Full_Color:next

		if Additional_Bulbs = 1 Then
		  For each xxx in GITOPmore:xxx.IntensityScale = GI_Intensity * gistep: xxx.Color = Top_Color: xxx.ColorFull = Top_Full_Color:next
		End if
 
        Case 2
 
   For each xxx in GIMID:xxx.IntensityScale = GI_Intensity * gistep: xxx.Color = Mid_Color: xxx.ColorFull = Mid_Full_Color: next

		if Additional_Bulbs = 1 Then
		   For each xxx in GIMIDmore:xxx.IntensityScale = GI_Intensity * gistep: xxx.Color = Mid_Color: xxx.ColorFull = Mid_Full_Color:next
		End if
	Case 4
 
  
 
    End Select
End Sub

Sub UpdateGI(GINo)
	'If GI_Intensity = 0 And Not isGIon Then Exit Sub
	' GINo: 2 = bottom GI, 4 = top GI
	' GILevel: value between 0 and 8
	'If GILevel = 1 Then GILevel = 0
	Select Case GINo
		Case 0 : For each xxx in GIBOT:xxx.IntensityScale = GI_Intensity : xxx.Color = GI_Bot_Color: xxx.ColorFull = GI_Bot_Full_Color:next 

		if Additional_Bulbs = 1 then
			For each xxx in GIBOTmore:xxx.IntensityScale = GI_Intensity: xxx.Color = GI_Bot_Color: xxx.ColorFull = GI_Bot_Full_Color:next
		End if

		Case 1 : For each xxx in GITOP:xxx.IntensityScale = GI_Intensity : xxx.Color = GI_Top_Color: xxx.ColorFull = GI_Top_Full_Color:next

		if Additional_Bulbs = 1 then
			For each xxx in GITOPmore:xxx.IntensityScale = GI_Intensity: xxx.Color = GI_Top_Color: xxx.ColorFull = GI_Top_Full_Color:next
		End if

		Case 2 : For each xxx in GIMID:xxx.IntensityScale = GI_Intensity : xxx.Color = GI_Mid_Color: xxx.ColorFull = GI_Mid_Full_Color: next
		if Additional_Bulbs = 1 then
			For each xxx in GIMIDmore:xxx.IntensityScale = GI_Intensity: xxx.Color = GI_Mid_Color: xxx.ColorFull = GI_Mid_Full_Color:next
		End if





	End Select
End Sub
 
'*******************************
'          Mist Magnet
'    taken from Lander's table
' with only a small modification
'*******************************
 
'-------------------------------
' Magnet Simulator Class
' (07/10/2001 Dorsola)
' Modified for Dracula (08/16/2001) by Dorsola
'-------------------------------
 
class cMagnet
    Private cX, cY, cStrength, cRange
    private cBalls, cClaimed
    private cTempX, cTempY
 
    Private Sub Class_Initialize()
        set cBalls = CreateObject("Scripting.Dictionary")
        cRange = 0
        cStrength = 0
    End Sub
 
    Public Sub InitMagnet(aTrigger, inStrength)
        cX = aTrigger.X
        cY = aTrigger.Y
        cRange = aTrigger.Radius
        cStrength = inStrength
    End Sub
 
    Public Sub MoveTo(inX, inY)
        cX = inX
        cY = inY
    End Sub
 
    Public Property Get X:X = cX:End Property
    Public Property Get Y:Y = cY:End Property
    Public Property Get Strength:Strength = cStrength:End Property
    Public Property Get Size:Size = cRange:End Property
    Public Property Get Range:Range = cRange:End Property
    Public Property Get Balls:Balls = cBalls.Keys:End Property
 
    Public Property Let X(inX):cX = inX:End Property
    Public Property Let Y(inY):cY = inY:End Property
    Public Property Let Strength(inStrength):cStrength = inStrength:End Property
    Public Property Let Size(inSize):cRange = inSize:End Property
    Public Property Let Range(inSize):cRange = inSize:End Property
 
    Public Sub AddBall(aBall)
        cBalls.Item(aBall) = 0
    End Sub
 
    Public Sub RemoveBall(aBall)
        ' This function tags balls for removal, but does not remove them.
        ' Another sub will be called to remove tagged objects from the dictionary.
        If cBalls.Exists(aBall) then
            if cClaimed then
                cBalls.Item(aBall) = 1
				cStrength = 0 ' try to fix ball swap
				cRange = 10 ' try to fix ball swap
            else
                cBalls.Remove(aBall)
            end if
        end if
    End Sub
 
    Public Sub Claim():cClaimed = True: End Sub
 
    Public Sub Release()
        cClaimed = False
        Dim tempobj
        for each tempobj in cBalls.Keys
            if cBalls.Item(tempobj) = 1 then cBalls.Remove(tempobj)
        next
    End Sub
 
    Public Sub ProcessBalls()
        Dim tempObj
        for each tempObj in cBalls.Keys:AttractBall tempObj:next
    End Sub
 
    Public Function GetDist(aBall)
        on error resume next
        if aBall is Nothing then
            GetDist = 100000
        else
            cTempX = aBall.X - cX
            cTempY = aBall.Y - cY
            GetDist = Sqr(cTempX * cTempX + cTempY * cTempY)
            if Err then GetDist = 100000
        end if
    End Function
 
    Public Sub AttractBall(aBall)
        if aBall is Nothing then Exit Sub
        Dim Dist
        Dist = GetDist(aBall)
        if Dist> cRange then Exit Sub
 
        ' Attract ball toward magnet center (cX,cY).
 
        ' Attraction force is determined by distance from center, and strength of magnet.
 
        Dim Force, Ratio
        Ratio = Dist / (1.5 * cRange)
 
        ' TODO: Figure out how to dampen the force when ball is near center and
        ' at low velocity, so that balls don't jitter on the magnets.
        ' Also shore up instability on moving magnet.
 
        Force = cStrength * exp(-0.2 / Ratio) / (Ratio * Ratio * 56)
        aBall.VelX = (aBall.VelX - cTempX * Force / Dist) * 0.985
        aBall.VelY = (aBall.VelY - cTempY * Force / Dist) * 0.985
    End Sub
End Class
 
'-----------------------------------------------
' Mist Multiball - courtesy of Dorsola
'-----------------------------------------------
 
' Method: Since any ball that can block the Mist opto is necessarily in the Mist Magnet's trigger area,
' we automatically have access to all balls in this range.  We can therefore check each ball's position
' against a line equation and see if it happens to be blocking the opto, and set the switch accordingly.
' This requires a timer loop.
 
Dim MagnetOn
MagnetOn = false
 
Sub MistTimer_Timer()
    ' Endpoints of the line are (108,1247) and (908,895)
    ' Slope: m = (y2-y1)/(x2-x1) = -0.44
    ' Y-intercept: b = y1 - m*x1 = 1294.52
 
    mMagnet.Claim
 
    Dim obj, CheckState, x, TargetY
    CheckState = 0
    on error resume next
    for each obj in mMagnet.Balls
        ' y = mx+b (m=slope, b=yint)
        TargetY = (-0.44) * obj.X + 1250.52
        if(obj.Y> TargetY - 25) and(obj.Y <TargetY + 25) then CheckState = 1
    next
    on error goto 0
 
    Controller.Switch(82) = CheckState
 
    if MagnetOn then mMagnet.ProcessBalls
 
    mMagnet.Release
End Sub
 
Sub SolMistMagnet(enabled)
    MagnetOn = enabled
End Sub
 
Sub Magnet_Hit()
    mMagnet.AddBall ActiveBall
End Sub
 
Sub Magnet_UnHit()
    mMagnet.RemoveBall ActiveBall
End Sub
 
'------------------------
' Handle the Mist Motor
'------------------------
' Method: Treat motor's position as a number from right to left (0-500)
' and compute its position based on the line equation given above.
 
const motorx1 = 880
const motorx2 = 108
const motorxrange = 800
const motory1 = 850
const motory2 = 1200
const motoryrange = -312
const motorslope = -0.44
const motoryint = 1249.52
 
' Endpoints of the line are (108,1247) and (908,895)
' Slope: m = (y2-y1)/(x2-x1) = -0.44
' Y-intercept: b = y1 - m*x1 = 1294.52
 
Dim MagnetPos, MagnetDir
MagnetPos = 0:MagnetDir = 0
 
' Coding for MagnetDir: 0 = left, 1 = right, toggle at endpoints.
 
Sub MotorTimer_Timer()
    if Controller.Solenoid(28) then
        if MagnetDir = 0 then
            MagnetPos = MagnetPos + MagnetSpeed
            'mist lights
            Select Case MagnetPos \ 33
                Case 0:ml1.State = 0:ml2.State = 0:ml3.State = 0:ml4.State = 0
                    ml5.State = 0:ml6.State = 0:ml7.State = 0:ml8.State = 0
                    ml9.State = 0:ml10.State = 0:ml11.State = 0:ml12.State = 0:ml13.State = 0
                Case 1:ml13.State = 1
                Case 2:ml12.State = 1:ml13.State = 0
                Case 3:ml11.State = 1:ml12.State = 0
                Case 4:ml10.State = 1:ml11.State = 0
                Case 5:ml9.State = 1:ml10.State = 0
                Case 6:ml8.State = 1:ml9.State = 0
                Case 7:ml7.State = 1:ml8.State = 0
                Case 8:ml6.State = 1:ml7.State = 0
                Case 9:ml5.State = 1:ml6.State = 0
                Case 10:ml4.State = 1:ml5.State = 0
                Case 11:ml3.State = 1:ml4.State = 0
                Case 12:ml2.State = 1:ml3.State = 0
                Case 13:ml1.State = 1:ml2.State = 0
                Case 14:ml1.State = 0
            End Select
 
            if MagnetPos >= 500 then
                MagnetPos = 500
                MagnetDir = 1
            end if
        else
            MagnetPos = MagnetPos - MagnetSpeed
            Select Case MagnetPos \ 33
                Case 0:ml1.State = 0:ml2.State = 0:ml3.State = 0:ml4.State = 0
                    ml5.State = 0:ml6.State = 0:ml7.State = 0:ml8.State = 0
                    ml9.State = 0:ml10.State = 0:ml11.State = 0:ml12.State = 0:ml13.State = 0
                Case 1:ml13.State = 0
                Case 2:ml12.State = 0:ml13.State = 1
                Case 3:ml11.State = 0:ml12.State = 1
                Case 4:ml10.State = 0:ml11.State = 1
                Case 5:ml9.State = 0:ml10.State = 1
                Case 6:ml8.State = 0:ml9.State = 1
                Case 7:ml7.State = 0:ml8.State = 1
                Case 8:ml6.State = 0:ml7.State = 1
                Case 9:ml5.State = 0:ml6.State = 1
                Case 10:ml4.State = 0:ml5.State = 1
                Case 11:ml3.State = 0:ml4.State = 1
                Case 12:ml2.State = 0:ml3.State = 1
                Case 13:ml1.State = 0:ml2.State = 1
                Case 14:ml1.State = 1
            End Select
            if MagnetPos <= 0 then
                MagnetPos = 0
                MagnetDir = 0
            end if
        end if
 
        SetMagnetPosition
        Controller.Switch(81) = (MagnetPos> 490)
        Controller.Switch(83) = (MagnetPos <10)
    end if
End Sub
 
Sub SetMagnetPosition()
    mMagnet.X = motorx1 -(motorxrange * (MagnetPos / 500) )
    mMagnet.Y = motorslope * mMagnet.X + motoryint
    If MagnetPos MOD 33 = 0 Then
        MotorTimer.Interval = 80
    Else
        MotorTimer.Interval = 8
    End If
End Sub
 
 
'******************************************
' Use the motor callback to call div subs
'******************************************
 
Set MotorCallback = GetRef("RealTimeUpdates")
 
Sub RealTime_Timer()

 
If L61.State = 1 Then
setflash 161,1
else
setflash 161,0
End If
 
If L62.State = 1 Then
setflash 162,1
else
setflash 162,0
End If
 
If L63.State = 1 Then
setflash 163,1
else
setflash 163,0
End If
 
If l58.state = 1 Then
setflash 158,1
else
setflash 158,0
End If
 
If l21.state = 1 Then
setflash 121,1
else
setflash 121,0
End If
 
If l22.state = 1 Then
setflash 122,1
else
setflash 122,0
End If
 
End Sub
 
Sub Trigger55off_Hit:sw55.Enabled=0:End Sub
Sub Trigger55on_Hit:sw55.Enabled=1:End Sub
 
Sub LHD_Hit()
PlaySound "ball_bounce", 0, 0.3, -0.1, 0.25, 0, 1, AudioFade(LHD)
End Sub
 
Sub CHD_Hit()
PlaySound "ball_bounce", 0, 0.3, 0, 0.25, 0, 1, AudioFade(CHD)
'StopSound "metalrolling"
End Sub
 
Sub CHM_Hit()
'PlaySound "metalrolling", 0, 0.3, 0, 0.25, 0, 1, AudioFade(CHM)
End Sub
 
Sub RHD_Hit()
'StopSound "subway2"
PlaySound "ball_bounce", 0, 0.3, 0.1, 0.25, 0, 1, AudioFade(RHD)
End Sub
 
Dim dirsrt
 
Sub shootramp_Timer()
Select Case dirsrt
Case 1:
If ramp_sl.heightbottom = 60 Then
me.enabled = 0
ramp_sl.heightbottom = 60
End If
ramp_sl.heightbottom = ramp_sl.heightbottom + 1
Liftramp.ObjRotx = LiftRamp.ObjRotx + 0.25
LiftRampRod.Z = LiftRampRod.Z + 0.25
Primitive108.ObjRotx = Primitive108.ObjRotx + 0.25
Primitive285.ObjRotx = Primitive285.ObjRotx + 0.25
Case 2:
If ramp_sl.heightbottom = 0 Then
me.enabled = 0
ramp_sl.heightbottom = 0
End If
ramp_sl.heightbottom = ramp_sl.heightbottom - 1
Liftramp.ObjRotx = LiftRamp.ObjRotx - 0.25
LiftRampRod.Z = LiftRampRod.Z - 0.25
Primitive108.ObjRotx = Primitive108.ObjRotx - 0.25
Primitive285.ObjRotx = Primitive285.ObjRotx - 0.25
End Select
End Sub

Sub sw55_UnHit(): StopSound "subway2" : End Sub

 
Sub TargetBounce_Hit (idx)
	if VPW_Bouncer = 1 then
	TargetBouncer activeball, Bounciness
	End If
    PlaySound "target", 0, VolMulti(ActiveBall,VolTargets), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub
 
Sub Metals_Hit (idx)
if Debug_Voice = 1 then
	PlaySound "metal", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
else
  RandomSoundMetal()
End If
End Sub

'Sub Metal_Ramps_Hit (idx)
 'PlaySound "Metal_Rolling", 0, VolMulti(ActiveBall,VolMetalRamp), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub

Sub Gates_Hit (idx)
  PlaySound "TOM_Gate3_3", 0, VolMulti(ActiveBall,VolGates), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub WireGate_Hit (idx)
	RandomGateSound()
End Sub

Sub Spinner_Spin
PlaySound "fx_spinner", 0, VolMulti(ActiveBall,VolSpinner), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Plastics_Hit(idx)
if Debug_Voice = 1 then
PlaySound "plastic", 0, VolMulti(ActiveBall,VolPlastic), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Else
	RandomPlasticSound()
End If
End Sub


Sub RubberBands_Hit(idx)
if Debug_Voice = 1 then
PlaySound "rubberbands", 0, VolMulti(ActiveBall,VolRubber), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Else
	RandomRubberBandSound()
End If
End Sub

Sub RubberPegs_Hit(idx)
if Debug_Voice = 1 then
PlaySound "peg", 0, VolMulti(ActiveBall,VolPegs), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Else
	RandomRubberPegSound()
End If
End Sub


Sub LeftFlipper_Collide (idx)
if Debug_Voice = 1 then
PlaySound "flipper", 0, VolMulti(ActiveBall,VolFlipper), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Else
RandomFlipperSound()
End If
End Sub

Sub RightFlipper_Collide (idx)
if Debug_Voice = 1 then
PlaySound "flipper", 0, VolMulti(ActiveBall,VolFlipper), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Else
RandomFlipperSound()
End If
End Sub
 
Sub RubberPosts_Hit(idx)
if Debug_Voice = 1 then
PlaySound "post", 0, VolMulti(ActiveBall,VolPosts), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Else
RandomRubberPostSound()
End If
End Sub

Sub Trigger001_Hit
  PlaySound "ramp_hit3", 0, RndNum(50,100)/500 , Pan(ActiveBall), 0, Pitch(ActiveBall)*RndNum(1,8000), AudioFade(Trigger001)
End Sub

Sub Trigger002_Hit
  If RightRamp.Collidable = True Then PlaySound "ramp_hit3", 0, RndNum(50,100)/500 , Pan(ActiveBall), 0, Pitch(ActiveBall)*RndNum(1,8000), AudioFade(Trigger002)
End Sub
 

Sub RandomPlasticSound()
Select Case Int(Rnd*5)+1
Case 1: PlaySound "flip_hit_1", 0, VolMulti(ActiveBall,VolPlastic), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Case 2: PlaySound "flip_hit_2", 0, VolMulti(ActiveBall,VolPlastic), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Case 3: PlaySound "flip_hit_3", 0, VolMulti(ActiveBall,VolPlastic), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Case 4: PlaySound "TOM_Rubber_Flipper_Normal_4", 0, VolMulti(ActiveBall,VolPlastic), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Case 5: PlaySound "TOM_Rubber_Flipper_Normal_5", 0, VolMulti(ActiveBall,VolPlastic), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Select
End Sub


Sub RandomRubberBandSound()
Select Case Int(Rnd*2)+1
Case 1: PlaySound "TOM_Rubber_Flipper_Normal_4", 0, VolMulti(ActiveBall,VolRubber), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Case 2: PlaySound "TOM_Rubber_Flipper_Normal_5", 0, VolMulti(ActiveBall,VolRubber), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Select
End Sub


Sub RandomRubberPostSound()
Select Case Int(Rnd*3)+1
Case 1: PlaySound "TOM_Rubber_Flipper_Normal_1", 0, VolMulti(ActiveBall,VolPosts), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Case 2: PlaySound "TOM_Rubber_Flipper_Normal_2", 0, VolMulti(ActiveBall,VolPosts), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Case 3: PlaySound "TOM_Rubber_Flipper_Normal_3", 0, VolMulti(ActiveBall,VolPosts), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Select
End Sub


Sub RandomRubberPegSound()
Select Case Int(Rnd*3)+1
Case 1: PlaySound "rubber_hit_1", 0, VolMulti(ActiveBall,VolPegs), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Case 2: PlaySound "rubber_hit_2", 0, VolMulti(ActiveBall,VolPegs), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Case 3: PlaySound "rubber_hit_3", 0, VolMulti(ActiveBall,VolPegs), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Select
End Sub

Sub RandomFlipperSound()
Select Case Int(Rnd*2)+1
Case 1: PlaySound "TOM_Rubber_Flipper_Normal_6", 0, VolMulti(ActiveBall,VolFlipper), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Case 2: PlaySound "TOM_Rubber_Flipper_Normal_7", 0, VolMulti(ActiveBall,VolFlipper), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Select
End Sub



Sub RandomGateSound()
Select Case Int(Rnd*2)+1
Case 1: PlaySound "gate", 0, VolMulti(ActiveBall,VolGates), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
Case 2: PlaySound "gate4", 0, VolMulti(ActiveBall,VolGates), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Select
End Sub

Sub RandomSoundMetal()
  Select Case Int(Rnd*13)+1
    Case 1 : PlaySound "TOM_Metal_Touch_1", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 2 : PlaySound "TOM_Metal_Touch_2", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Case 3 : PlaySound "TOM_Metal_Touch_3", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 4 : PlaySound "TOM_Metal_Touch_4", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Case 5 : PlaySound "TOM_Metal_Touch_5", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 6 : PlaySound "TOM_Metal_Touch_6", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Case 7 : PlaySound "TOM_Metal_Touch_7", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 8 : PlaySound "TOM_Metal_Touch_8", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Case 9 : PlaySound "TOM_Metal_Touch_9", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 10 : PlaySound "TOM_Metal_Touch_10", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Case 11 : PlaySound "TOM_Metal_Touch_11", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 12 : PlaySound "TOM_Metal_Touch_12", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Case 13 : PlaySound "TOM_Metal_Touch_13", 0, VolMulti(ActiveBall,VolMetal), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End Select
End Sub

Sub RandomSoundDrain()
  Select Case Int(Rnd*11)+1
    Case 1 : PlaySound "TOM_Drain_1", 0, VolMulti(ActiveBall,VolDrain), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 2 : PlaySound "TOM_Drain_2", 0, VolMulti(ActiveBall,VolDrain), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Case 3 : PlaySound "TOM_Drain_3", 0, VolMulti(ActiveBall,VolDrain), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 4 : PlaySound "TOM_Drain_4", 0, VolMulti(ActiveBall,VolDrain), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Case 5 : PlaySound "TOM_Drain_5", 0, VolMulti(ActiveBall,VolDrain), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 6 : PlaySound "TOM_Drain_6", 0, VolMulti(ActiveBall,VolDrain), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Case 7 : PlaySound "TOM_Drain_7", 0, VolMulti(ActiveBall,VolDrain), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 8 : PlaySound "TOM_Drain_8", 0, VolMulti(ActiveBall,VolDrain), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Case 9 : PlaySound "TOM_Drain_9", 0, VolMulti(ActiveBall,VolDrain), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 10 : PlaySound "TOM_Drain_10", 0, VolMulti(ActiveBall,VolDrain), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Case 11 : PlaySound "TOM_Drain_0", 0, VolMulti(ActiveBall,VolDrain), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End Select
End Sub

Sub RandomSoundRollover()
  Select Case Int(Rnd*4)+1
    Case 1 : PlaySoundAtVol "TOM_Rollover_1", ActiveBall, VolRol
    Case 2 : PlaySoundAtVol "TOM_Rollover_2", ActiveBall, VolRol
    Case 3 : PlaySoundAtVol "TOM_Rollover_3", ActiveBall, VolRol
    Case 4 : PlaySoundAtVol "TOM_Rollover_4", ActiveBall, VolRol
  End Select
End Sub

Sub RandomSoundSlingshotLeft()
  Select Case Int(Rnd*10)+1
    Case 1 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_L1_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 2 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_L2_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 3 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_L3_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 4 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_L4_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 5 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_L5_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 6 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_L6_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 7 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_L7_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 8 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_L8_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 9 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_L9_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 10 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_L10_Strong_Layered",DOFContactors), ActiveBall, VolSling
  End Select
End Sub

Sub RandomSoundSlingshotRight()
  Select Case Int(Rnd*8)+1
    Case 1 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_R1_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 2 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_R2_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 3 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_R3_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 4 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_R4_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 5 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_R5_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 6 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_R6_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 7 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_R7_Strong_Layered",DOFContactors), ActiveBall, VolSling
    Case 8 : PlaySoundAtVol SoundFX("TOM_Calle_Sling_Rework_R9_Strong_Layered",DOFContactors), ActiveBall, VolSling
  End Select
End Sub

Sub RandomSoundBumperA()
  Select Case Int(Rnd*5)+1
    Case 1 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Top_1",DOFContactors), ActiveBall, VolBump
    Case 2 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Top_2",DOFContactors), ActiveBall, VolBump
    Case 3 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Top_3",DOFContactors), ActiveBall, VolBump
    Case 4 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Top_4",DOFContactors), ActiveBall, VolBump
    Case 5 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Top_5",DOFContactors), ActiveBall, VolBump
  End Select
End Sub

Sub RandomSoundBumperB()
  Select Case Int(Rnd*5)+1
    Case 1 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Middle_1",DOFContactors), ActiveBall, VolBump
    Case 2 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Middle_2",DOFContactors), ActiveBall, VolBump
    Case 3 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Middle_3",DOFContactors), ActiveBall, VolBump
    Case 4 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Middle_4",DOFContactors), ActiveBall, VolBump
    Case 5 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Middle_5",DOFContactors), ActiveBall, VolBump
  End Select
End Sub

Sub RandomSoundBumperC()
  Select Case Int(Rnd*5)+1
    Case 1 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Bottom_1",DOFContactors), ActiveBall, VolBump
    Case 2 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Bottom_2",DOFContactors), ActiveBall, VolBump
    Case 3 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Bottom_3",DOFContactors), ActiveBall, VolBump
    Case 4 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Bottom_4",DOFContactors), ActiveBall, VolBump
    Case 5 : PlaySoundAtVol SoundFX("TOM_Bumpers_Reworked_v2_Bottom_5",DOFContactors), ActiveBall, VolBump
  End Select
End Sub






 
 
Sub Table1_Exit
  Controller.Stop
End Sub

' **********
' Csilk Inlane Roll Fix
' **********
Sub LFrollfix_hit
If RollCorrection = 1 then
leftflipper.elasticity = ModifiedElasticity
end if
end sub

Sub RFrollfix_hit
If RollCorrection = 1 then
rightflipper.elasticity = ModifiedElasticity
end if
end sub

Sub LFrollfix_unhit
If RollCorrection = 1 then
leftflipper.elasticity = UnmodifiedElasticity
end if
end sub

Sub RFrollfix_unhit
If RollCorrection = 1 then
rightflipper.elasticity = UnmodifiedElasticity
end if
end sub


' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX, Rothbauerw, Thalamus and Herweh
' PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

' set position as table object and Vol + RndPitch manually

Sub PlaySoundAtVolPitch(sound, tableobj, Vol, RndPitch)
  PlaySound sound, 1, Vol, AudioPan(tableobj), RndPitch, 0, 0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Volume)
  PlaySound sound, 1, Volume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallAbsVol(sound, VolMult)
  PlaySound sound, 0, VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

' requires rampbump1 to 7 in Sound Manager

Sub RandomBump(voladj, freq)
  Dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
  PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' set position as bumperX and Vol manually. Allows rapid repetition/overlaying sound

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
  PlaySound sound, 0, ABS(BOT.velz)/17, Pan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
End Sub

' play a looping sound at a location with volume
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
  PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub
 
'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************
 
Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "Dracula" is the name of the table
  Dim tmp
  tmp = tableobj.y * 2 / Table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function
 
Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "Dracula" is the name of the table
  Dim tmp
  tmp = tableobj.x * 2 / Table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function
 
Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "Dracula" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / Table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function
 
Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
End Function
 

'Added functions from Thalamus version

Function VolMulti(ball,Multiplier) ' Calculates the Volume of the sound based on the ball speed
  VolMulti = Csng(BallVel(ball) ^2 / 150 ) * Multiplier
End Function

Function DVolMulti(ball,Multiplier) ' Calculates the Volume of the sound based on the ball speed
  DVolMulti = Csng(BallVel(ball) ^2 / 150 ) * Multiplier
  debug.print DVolMulti
End Function

Function BallRollVol(ball) ' Calculates the Volume of the sound based on the ball speed
  BallRollVol = Csng(BallVel(ball) ^2 / (80000 - (79900 * Log(RollVol) / Log(100))))
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
  BallVelZ = INT((ball.VelZ) * -1 )
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
  VolZ = Csng(BallVelZ(ball) ^2 / 200)*1.2
End Function

Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
  Dim AB, BC, CD, DA
  AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
  BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
  CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
  DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)

  If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
    InRect = True
  Else
    InRect = False
  End If
End Function
 
'*******************************************
'   JP's VP10 Rolling Sounds
'*******************************************

'Const tnob = 5 ' total number of balls
'Const lob = 0  'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
  Dim i
  For i = 0 to tnob
    rolling(i) = False
  Next
End Sub

Sub RollingTimer_Timer()
  Dim BOT, b
  BOT = GetBalls
  ' TextBox001.Text = (UBound(BOT))+1

  ' stop the sound of deleted balls
  For b = UBound(BOT) + 1 to tnob
    rolling(b) = False
	' On Error Resume Next
    StopSound("fx_ballrolling" & b)
    StopSound("fx_Rolling_Plastic" & b)
    StopSound("WireLoop" & b)
  Next

  ' exit the sub if no balls on the table
  If UBound(BOT) = lob - 1 Then Exit Sub

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)


      If BallVel(BOT(b)) > 1 Then
        rolling(b) = True

        'Playfield
        If BOT(b).z < 30 Then
          StopSound("WireLoop" & b):StopSound("fx_Rolling_Plastic" & b)
          PlaySound("fx_ballrolling" & b), -1, VolMulti((BOT(b)),VolBRol), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else
          If InRect(BOT(b).x, BOT(b).y, 344,866,286,376,670,532,670,866) And BOT(b).z < 200 And BOT(b).z > 79 Then
            StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
            PlaySound("WireLoop" & b), -1, VolMulti((BOT(b)),VolMRol), Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
      ElseIf InRect(BOT(b).x, BOT(b).y, 293,378,556,176,617,211,363,407) And BOT(b).z < 200 And BOT(b).z > 79 Then
            StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
            PlaySound("WireLoop" & b), -1, VolMulti((BOT(b)),VolMRol), Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
      ElseIf InRect(BOT(b).x, BOT(b).y, 816,1183,388,867,529,867,855,1127) And BOT(b).z < 110 And BOT(b).z > 60 Then
            StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
            PlaySound("WireLoop" & b), -1, VolMulti((BOT(b)),VolMRol), Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
      ElseIf InRect(BOT(b).x, BOT(b).y, 857,1769,857,1380,944,1380,944,1769) And BOT(b).z < 100 And BOT(b).z > 30 Then
            StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
            PlaySound("WireLoop" & b), -1, VolMulti((BOT(b)),VolMRol), Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
      ElseIf InRect(BOT(b).x, BOT(b).y, 89,1476,64,771,275,771,159,1476) And BOT(b).z < 110 And BOT(b).z > 80 Then
            StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
            PlaySound("WireLoop" & b), -1, VolMulti((BOT(b)),VolMRol), Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
      ElseIf InRect(BOT(b).x, BOT(b).y, 191,770,210,286,446,771,278,770) And BOT(b).z < 110 And BOT(b).z > 80 Then
            StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
            PlaySound("WireLoop" & b), -1, VolMulti((BOT(b)),VolMRol), Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
            'Plastic Ramps
          Else
            StopSound("WireLoop" & b):StopSound("fx_ballrolling" & b)
            PlaySound("fx_Rolling_Plastic" & b), -1, VolMulti((BOT(b)),VolPRol), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
          End If
        End If

      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          StopSound("fx_Rolling_Plastic" & b)
          StopSound("WireLoop" & b)
          rolling(b) = False
        End If
      End If

      ' play ball drop sounds
      If BOT(b).VelZ < -4 and BOT(b).VelY < 8 And BOT(b).z < 50 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
        PlaySound ("fx_ball_drop" & b), 0, (ABS(BOT(b).velz)/17)*VolDrop, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
      End If


    Next
End Sub

'**********************
' Ball Collision Sound
'**********************
 
Sub OnBallBallCollision(ball1, ball2, velocity)
  If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 200, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
  Else
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 200, Pan(ball1), 0, Pitch(ball1), 0, 0
  End if
End Sub



		''SolLFlipper True						'This would be called by the solenoid callbacks if using a ROM



		
		''SolRFlipper True						'This would be called by the solenoid callbacks if using a ROM


'' VPW STUFF
'*******************************************
'  Flippers
'*******************************************
' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
		FlipperActivate LeftFlipper, LFPress
		PlaySoundAt SoundFX("flipperup",DOFFlippers),LeftFlipper
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
		'	RandomSoundReflipUpLeft LeftFlipper
		Else 
		'	SoundFlipperUpAttackLeft LeftFlipper
		'	RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		LeftFlipper.RotateToStart
		PlaySoundAt SoundFX("flipperdown",DOFFlippers),LeftFlipper
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
		
	'	RandomSoundFlipperDownLeft LeftFlipper
		End If
		'FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		FlipperActivate RightFlipper, RFPress
		FlipperHeldModifier = true
		RF.Fire 'rightflipper.rotatetoend
		PlaySoundAt SoundFX("flipperup",DOFFlippers),RightFlipper
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
		'	RandomSoundReflipUpRight RightFlipper
		Else 
			'SoundFlipperUpAttackRight RightFlipper
			'RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		FlipperHeldModifier = false
		RightFlipper.RotateToStart
		PlaySoundAt SoundFX("flipperdown",DOFFlippers),RightFlipper
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			'RandomSoundFlipperDownRight RightFlipper
		End If	
		'FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
'lfs.RotZ = LeftFlipper.CurrentAngle
'rfs.RotZ = RightFlipper.CurrentAngle
LFLogo.RotY =  LeftFlipper.CurrentAngle
RFlogo.RotY =  RightFlipper.CurrentAngle
WireGateLR.RotX=Spinner2.currentangle
WireGateLR1.RotX=Spinner1.currentangle
WireGateLR3.RotY=Spinner3.currentangle
End Sub

'*******************************************
'  Timers
'*******************************************



Sub RDampen_Timer()
	Cor.Update 						'update ball tracking
End Sub


' The frame timer interval is -1, so executes at the display frame rate
dim FrameTime, InitFrameTime : InitFrameTime = 0
Sub FrameTimer_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if VPW_Bouncer = 1 and aball.z < 30 then
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


sub ProperDeadflipperBounce
	dim zMultiplier, vel, vratio
	if DeadflipCorrection = 1 and aball.z < 30 then
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



'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* Count from 0 up, with at least as many objects each as there can be balls, including locked balls.  You'll get an "eval" warning if tnob is higher
'	* Warning:  If merging with another system (JP's ballrolling), you may need to check tnob math and add an extra BallShadowA# flasher (out of range error)
' Ensure you have a timer with a -1 interval that is always running
' Set plastic ramps DB to *less* than the ambient shadows (-10000) if you want to see the pf shadow through the ramp

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

' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
' *** Change gBOT to BOT if using existing getballs code
' *** Includes lines commonly found there, for reference:
'	' stop the sound of deleted balls
'	For b = UBound(gBOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		...rolling(b) = False
'		...StopSound("BallRoll_" & b)
'	Next
'
' ...rolling and drop sounds...

'		If DropCount(b) < 5 Then
'			DropCount(b) = DropCount(b) + 1
'		End If
'
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If gBOT(b).Z > 30 Then
'				BallShadowA(b).height=gBOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
'			Else
'				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = gBOT(b).Y + Ballsize/5 + offsetY
'			BallShadowA(b).X = gBOT(b).X + offsetX
'			BallShadowA(b).visible = 1
'		End If

' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

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

' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!
dim objrtx1(5), objrtx2(5)
dim objBallShadow(5)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)
Dim DSSources(33), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

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
'				FLIPPER AND RUBBER CORRECTION
'******************************************************


dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 60
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

        addpt "Velocity", 0, 0, 1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41, 1.05
        addpt "Velocity", 3, 0.53, 0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,  0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub


'******************************************************
'                        FLIPPER CORRECTION FUNCTIONS
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
         :                         VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

                                if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

                                if Enabled then aBall.Velx = aBall.Velx*VelCoef
                                if Enabled then aBall.Vely = aBall.Vely*VelCoef
                        End If

                        'Polarity Correction (optional now)
                        if not IsEmpty(PolarityIn(0) ) then
                                If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
                                dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
        
                                if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
                                'playsound "fx_knocker"
                        End If
                End If
                RemoveBall aBall
        End Sub
End Class

'******************************************************
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
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


'******************************************************
'                        FLIPPER TRICKS
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
        Dim BOT, b

        If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
                EOSNudge1 = 1
                'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
                If Flipper2.currentangle = EndAngle2 Then 
                        BOT = GetBalls
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
                If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 15 then 
                        EOSNudge1 = 0
                end if
        End If
End Sub

'*****************
' Maths
'*****************
'Const PI = 3.1415927

Function dSin(degrees)
        dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
        dcos = cos(degrees * Pi/180)
End Function

'*************************************************
' Check ball distance from Flipper for Rem
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

' Used for drop targets and stand up targets
Function Atn2(dy, dx)
        'dim pi
        'pi = 4*Atn(1)

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
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return


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
                Dim BOT, b
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



'######################### Add new dampener to CheckLiveCatch 
'#########################    Note the updated flipper angle check to register if the flipper gets knocked slightly off the end angle

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
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm, Rubberizer
    End If
End Sub


'********************************************************
'				FLIPPER AND RUBBER CORRECTION
'********************************************************

'****************************************************************************
'nFozzy PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR

Sub dRubbers_Hit(idx)
	if VPW_Dampening = 1 then
		RubbersD.dampen Activeball 
	End If	
End Sub

Sub dSleeves_Hit(idx)
	if VPW_Dampening = 1 then
	SleevesD.Dampen Activeball
	End If
End Sub

dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96 
RubbersD.addpoint 2, 5.76, 0.967 'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
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

'######################### Add Dampenf to Dampener Class 
'#########################    Only applies dampener when abs(velx) < 2 and vely < 0 and vely > -3.75  


Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
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
		if cor.ballvel(aBall.id) = 0 then
			RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.001) 'hack
		Else
			RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		end If
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm, ver)
		If ver = 1 Then
			dim RealCOR, DesiredCOR, str, coef
			DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
			if cor.ballvel(aBall.id) = 0 then
                RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.001) 'hack
            Else
                RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
            end If
			coef = desiredcor / realcor 
			If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
				aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
			End If
		Elseif ver = 2 Then
			If parm < 10 And parm > 2 And Abs(aball.angmomz) < 15 And aball.vely < 0 then
				aball.angmomz = aball.angmomz * 1.2
				aball.vely = aball.vely * (1.1 + (parm/50))
			Elseif parm <= 2 and parm > 0.2 And aball.vely < 0 Then
				if (aball.velx > 0 And aball.angmomz > 0) Or (aball.velx < 0 And aball.angmomz < 0) then
			        	aball.angmomz = aball.angmomz * -0.7
				Else
					aball.angmomz = aball.angmomz * 1.2
				end if
				aball.vely = aball.vely * (1.2 + (parm/10))
			End if
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report() 	'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub
	

End Class

'******************************************************
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

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



Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp if on the boundry lines
	'if L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'if L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

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
	AddSlingsPt 1, 0.45,	-8
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	8
	AddSlingsPt 5, 1.00,	4

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
		x.addpoint idx, aX, aY
	Next
End Sub

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


'*******************************************************
'	End nFozzy Dampening'
'******************************************************


' *********************************************************************
'					Supporting Ball & Sound Functions
' *********************************************************************

Function RndNum(min,max)
 RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min and max
End Function