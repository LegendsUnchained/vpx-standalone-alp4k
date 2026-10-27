'   ____     ___      _____             __          
'  / __/__ _/ _/__   / ___/______ _____/ /_____ ____
' _\ \/ _ `/ _/ -_) / /__/ __/ _ `/ __/  '_/ -_) __/
'/___/\_,_/_/ \__/  \___/_/  \_,_/\__/_/\_\\__/_/   
'
'*********************************************************************************************************************************
'  Safe Cracker (Bally 1996) / IPD No.  3782 / 4 Players
'
'      Original VPX table by fuzzel, flupper1, rothbauerw			
'      		Contributors: acronovum, hauntfreaks    	
'	   		VP9 Authors: ICPjuggla, OldSkoolGamer and Herweh
'	   		Code snippets from Destruk, JPSalas, and Unclewilly
'
'      VR cabinet art and one of the VR Rooms: b4asti
'
'	Updates by UnclePaulie version 2.0 include updated VPW code, 3D inserts, physics, standalone, rothbauerw targets, flupper bumpers
'	fleep sounds, flipper physics, sling corrections, full VLM for GI, other GI corrections and balancing, hybrid, EBisLit scanned playfield, 
'	F12 menu options, and other updates
'	
'*********************************************************************************************************************************


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
'	ZKEY: Key Press Handling
'	ZSOL: Solenoids & Flashers
'	ZDRN: Drain, Trough, and Ball Release
'	ZFLP: Flippers
'	ZSLG: Slingshot Animations
'	ZSSC: Slingshot Corrections
'	ZSWI: Switches
'	ZVUK: VUKs and Kickers
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


Option Explicit
Randomize
SetLocale 1033			'Forces VBS to use english to stop crashes.

'******************************************************
' Minimum Requirements
'******************************************************

' VPX: version 10.8.0 RC3 or higher
' VPM: version 3.6.0 or higher
' B2S: version 2.1.1 or higher (if using b2s)


'******************************************************
' Desktop Options
'******************************************************

const useownb2s = 0				'1 = player chooses to forego built in desktop backglass and use b2s instead.
								'0 = built in desktop backglass (default)
'******************************************************
' Staged Flipper fix for debug coin
'******************************************************

const cSingleLFlip = 0							

'******************************************************
' VR Room Options
'******************************************************

const ShowGlass = 0				'0 = no reflections on the glass, 1 = DMD and backglass reflections
const WallClock = 1	  			'1 Shows the clock in the VR minimal rooms only (b4asti's room has a different clock and is Enabled without choice)
const Siren = 1					'1 Shows the siren/flasher VR rooms only  (default is 1)
const SirenReflect = 1			'1 Shows reflections of the siren/flasher on the VR walls. (default = 1)
const BeaconVol = 0.3			'0-1; 0 = off, 1 = max.  Sounds only active for VR and if SirenFlasher is enabled.
const Topper = 1				'1 Shows the topper in the VR rooms only (default is 1, and recommend to only use if you have a Siren/flasher)
const Poster = 1				'1 Shows the flyer posters in the VR room only 
const Poster2 = 1				'1 Shows the flyer posters in the VR room only 
const VREnv = 5					'0=UP's Original Minimal Walls, floor, and roof
								'1=Sixtoe's arcade style
								'2=DarthVito's Updated home walls with lights (default)
								'3=DarthVito's plaster home walls
								'4=DarthVito's blue home walls
								'5=b4asti's room


'******************************************************
'  ZVAR: Constants and Global Variables
'******************************************************
Const BallSize = 50		'Ball size must be 50
Const BallMass = 1		'Ball mass must be 1

Const cGameName = "sc_18n11"	'PinMAME ROM name ***NOTE: Requires both sc_18s11 and sc18n11
Const UseSolenoids = 1			'1 = Normal Flippers, 2 = Fastflips
Const UseLamps = 1				'0 = Custom lamp handling, 1 = Built-in VPX handling (using light number in light timer)
Const UseSync = 0
Const UseGI = 0
Const UseVPMColoredDMD = True
Const UseVPMModSol = 2 			'Set to 2 for PWM flashers, inserts, and GI. Requires VPinMame 3.6
Const tnob = 4					'Total number of balls on the playfield including captive balls.
Const lob = 0					'Total number of locked balls

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Dim BIPL : BIPL = False				'Ball in plunger lane


'VR_Room set based on RenderingMode

Dim VR_Room, cab_mode, DesktopMode: DesktopMode = Table1.ShowDT
If RenderingMode = 2 Then VR_Room=1 Else VR_Room=0
if Not DesktopMode and VR_Room=0 Then cab_mode=1 Else cab_mode=0


On Error Resume Next
	ExecuteGlobal GetTextFile("controller.vbs")
	If Err Then MsgBox "You need the controller.vbs; and VPX 10.8.0, VPM 3.6.0, and B2S 2.1.1 or higher"
On Error Goto 0

LoadVPM "01560000", "WPC.VBS", 3.50
keyStagedFlipperL = ""


'******************************************************
'	VLM Arrays
'******************************************************

' VLM  Arrays - Start
' Arrays per baked part
Dim BP_Playfield: BP_Playfield=Array(LM_GI_gi_001_Playfield, LM_GI_gi_002_Playfield, LM_GI_gi_003_Playfield, LM_GI_gi_004_Playfield, LM_GI_gi_005_Playfield, LM_GI_gi_006_Playfield, LM_GI_gi_007_Playfield, LM_GI_gi_008_Playfield, LM_GI_gi_009_Playfield, LM_GI_gi_010_Playfield, LM_GI_gi_011_Playfield, LM_GI_gi_012_Playfield, LM_GI_gi_013_Playfield, LM_GI_gi_014_Playfield, LM_GI_gi_015_Playfield, LM_GI_gi_016_Playfield, LM_GI_gi_017_Playfield, LM_GI_gi_018_Playfield)
' Arrays per lighting scenario
Dim BL_GI_gi_001: BL_GI_gi_001=Array(LM_GI_gi_001_Playfield)
Dim BL_GI_gi_002: BL_GI_gi_002=Array(LM_GI_gi_002_Playfield)
Dim BL_GI_gi_003: BL_GI_gi_003=Array(LM_GI_gi_003_Playfield)
Dim BL_GI_gi_004: BL_GI_gi_004=Array(LM_GI_gi_004_Playfield)
Dim BL_GI_gi_005: BL_GI_gi_005=Array(LM_GI_gi_005_Playfield)
Dim BL_GI_gi_006: BL_GI_gi_006=Array(LM_GI_gi_006_Playfield)
Dim BL_GI_gi_007: BL_GI_gi_007=Array(LM_GI_gi_007_Playfield)
Dim BL_GI_gi_008: BL_GI_gi_008=Array(LM_GI_gi_008_Playfield)
Dim BL_GI_gi_009: BL_GI_gi_009=Array(LM_GI_gi_009_Playfield)
Dim BL_GI_gi_010: BL_GI_gi_010=Array(LM_GI_gi_010_Playfield)
Dim BL_GI_gi_011: BL_GI_gi_011=Array(LM_GI_gi_011_Playfield)
Dim BL_GI_gi_012: BL_GI_gi_012=Array(LM_GI_gi_012_Playfield)
Dim BL_GI_gi_013: BL_GI_gi_013=Array(LM_GI_gi_013_Playfield)
Dim BL_GI_gi_014: BL_GI_gi_014=Array(LM_GI_gi_014_Playfield)
Dim BL_GI_gi_015: BL_GI_gi_015=Array(LM_GI_gi_015_Playfield)
Dim BL_GI_gi_016: BL_GI_gi_016=Array(LM_GI_gi_016_Playfield)
Dim BL_GI_gi_017: BL_GI_gi_017=Array(LM_GI_gi_017_Playfield)
Dim BL_GI_gi_018: BL_GI_gi_018=Array(LM_GI_gi_018_Playfield)
' Global arrays
Dim BG_Bakemap: BG_Bakemap=Array()
Dim BG_Lightmap: BG_Lightmap=Array(LM_GI_gi_001_Playfield, LM_GI_gi_002_Playfield, LM_GI_gi_003_Playfield, LM_GI_gi_004_Playfield, LM_GI_gi_005_Playfield, LM_GI_gi_006_Playfield, LM_GI_gi_007_Playfield, LM_GI_gi_008_Playfield, LM_GI_gi_009_Playfield, LM_GI_gi_010_Playfield, LM_GI_gi_011_Playfield, LM_GI_gi_012_Playfield, LM_GI_gi_013_Playfield, LM_GI_gi_014_Playfield, LM_GI_gi_015_Playfield, LM_GI_gi_016_Playfield, LM_GI_gi_017_Playfield, LM_GI_gi_018_Playfield)
Dim BG_All: BG_All=Array(LM_GI_gi_001_Playfield, LM_GI_gi_002_Playfield, LM_GI_gi_003_Playfield, LM_GI_gi_004_Playfield, LM_GI_gi_005_Playfield, LM_GI_gi_006_Playfield, LM_GI_gi_007_Playfield, LM_GI_gi_008_Playfield, LM_GI_gi_009_Playfield, LM_GI_gi_010_Playfield, LM_GI_gi_011_Playfield, LM_GI_gi_012_Playfield, LM_GI_gi_013_Playfield, LM_GI_gi_014_Playfield, LM_GI_gi_015_Playfield, LM_GI_gi_016_Playfield, LM_GI_gi_017_Playfield, LM_GI_gi_018_Playfield)
' VLM  Arrays - End


'******************************************************
'  ZTIM: Timers
'******************************************************

Dim FrameTime, InitFrameTime
InitFrameTime = 0

FrameTimer.Interval = -1
Sub FrameTimer_Timer() 
	FrameTime = gametime - InitFrameTime 'Calculate FrameTime as some animuations could use this
	InitFrameTime = gametime	'Count frametime

	'Add animation stuff here
	BSUpdate
	UpdateBallBrightness
	RollingUpdate				'Update rolling sounds
	DoSTAnim					'Standup target animations
	DoDTAnim					'Drop target animations
	Gate4p.rotx = max(Gate4.CurrentAngle,0)
End Sub

CorTimer.Interval = 10
Sub CorTimer_Timer()
	Cor.Update
	DoVTAnim					'Vari-target animations
	if VR_Room = 1 AND VREnv = 5 Then
		BeerTimer
	End If
End Sub

'******************************************************
'  ZINI: Table Initialization and Exiting
'******************************************************
Dim SCBall1, SCBall2, SCBall3, SCBall4, gBOT, PlungerIM, SpinnerBall
Dim DT61up, DT62up, DT63up, DT64up, DT65up, DT66up, DT74up, DT75up, DT76up


Sub Table1_Init
	vpminit me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Safe Cracker (Bally 1996)" & vbNewLine & "Fuzzel, Flupper1, Rothbauerw, b4asti and UnclePaulie"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0

		if ShowDT=true then
			.hidden=1		
		end if
		.Games(cGameName).Settings.Value("sound") = 1 ' Set sound (0=OFF, 1=ON)	

         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0

	End With

	'Map all lamps to the corresponding ROM output using the value of TimerInterval of each light object
	vpmMapLights AllLamps 		'Make a collection called "AllLamps" and put all the light objects in it.

	'Nudging
	vpmNudge.TiltSwitch=14
	vpmNudge.Sensitivity=4
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

	'Trough - Creates a ball in the kicker switch and gives that ball used an individual name.
	Set SCBall1 = sw32.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set SCBall2 = sw33.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set SCBall3 = sw34.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set SCBall4 = sw35.CreateSizedballWithMass(Ballsize/2,Ballmass)

	Controller.Switch(31) = 0
	Controller.Switch(32) = 1
	Controller.Switch(33) = 1
	Controller.Switch(34) = 1
	Controller.Switch(35) = 1

	'***Setting up a ball array (collection), must contain all the balls you create on the table.
	gBOT = Array(SCBall1, SCBall2, SCBall3, SCBall4)

	sw77wall.collidable = false
	Controller.Switch(77) = 0
	Controller.Switch(41) = 1	'switch mapped backwards

	Set SpinnerBall = SpinnerKick.CreateSizedballWithMass(32/2,Ballmass/2)
	SpinnerBall.visible = False
	Spinnerkick.kick 0,0,0
	Spinnerkick.enabled = False

'Varitarget initializations
	controller.Switch(56) = 0
	controller.Switch(57) = 0
	controller.Switch(58) = 0

'Lockup Ball Switch Initializations
	controller.Switch(36) = 0
	controller.Switch(37) = 0

' Make drop target shadows visible
	Dim xx
	for each xx in dtshadows
		xx.visible=True
	Next

' Drop Target Variable state
	DT61up=1
	DT62up=1
	DT63up=1
	DT64up=1
	DT65up=1
	DT66up=1
	DT74up=1
	DT75up=1
	DT76up=1

	if VR_Room = 1 Then
		setup_backglassVR()
	Elseif desktopmode = True Then
		setup_backglass()
	End If

	Dim numl
	For each numl in mlights
		numl.visible = 0
	Next

	Pin1_WindowGlass2.visible = (ShowGlass * VR_Room)

	L_DT_1.intensity = 0

	' PERF: VR-only timers have no job outside VR - switch them off
	If VR_Room = 0 Then
		ClockTimer.Enabled = False
		TimerVRPlunger.Enabled = False
		TimerVRPlunger1.Enabled = False
		BeaconTimer.Enabled = False
		BeaconSoundTimer.Enabled = False
	End If

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

Dim LightLevel : LightLevel = 0.25				' Level of room lighting (0 to 1), where 0 is dark and 100 is brightest
Dim ColorLUT : ColorLUT = 1						' Color desaturation LUTs: 1 to 11, where 1 is normal and 11 is black'n'white
Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.1		' Level of ramp rolling volume. Value between 0 and 1

' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings
Sub Table1_OptionEvent(ByVal eventId)
    If eventId = 1 Then DisableStaticPreRendering = True


	' Color Saturation
    ColorLUT = Table1.Option("Color Saturation", 1, 11, 1, 1, 0, _
		Array("Normal", "Desaturated 10%", "Desaturated 20%", "Desaturated 30%", "Desaturated 40%", "Desaturated 50%", _
        "Desaturated 60%", "Desaturated 70%", "Desaturated 80%", "Desaturated 90%", "Black 'n White"))
	if ColorLUT = 1 Then Table1.ColorGradeImage = ""
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

    ' Sound volumes
    VolumeDial = Table1.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
    BallRollVolume = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.5, 1)
	RampRollVolume = Table1.Option("Ramp Roll Volume", 0, 1, 0.01, 0.1, 1)


	' Room brightness
'	LightLevel = Table1.Option("Table Brightness (Ambient Light Level)", 0, 1, 0.01, .5, 1)
	LightLevel = NightDay/100

    If eventId = 3 Then DisableStaticPreRendering = False
End Sub


'**********************************
' 	ZMAT: General Math Functions
'**********************************

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

Sub LeftFlipper_Animate
	dim a: a = LeftFlipper.CurrentAngle
	batleftshadow.objRotZ = a
	'Add any left flipper related animations here
	pLeftFlipper.objrotz = a - 0.5
End Sub

Sub RightFlipper_Animate
	dim a: a = RightFlipper.CurrentAngle
	batrightshadow.objRotZ = a
	'Add any right flipper related animations here
	pRightFlipper.objrotz = a + 0.5
End Sub

Sub RightUpperFlipper_Animate
	dim a: a = RightUpperFlipper.currentangle
	batrightuppershadow.objRotZ = a
	pRightUpperFlipper.objrotz = a + 180
End Sub


'******************************************************
' 	ZBBR: BALL BRIGHTNESS
'******************************************************

Const BallBrightness =  1      	'Ball brightness - Value between 0 and 1 (0=Dark ... 1=Bright)

' Constants for plunger lane ball darkening.
' You can make a temporary wall in the plunger lane area and use the co-ordinates from the corner control points.
Const PLOffset = 0.575			'Minimum ball brightness scale in plunger lane
Const PLLeft = 776 				'X position of punger lane left
Const PLRight = 846 			'X position of punger lane right
Const PLTop = 610 				'Y position of punger lane top
Const PLBottom = 1685 			'Y position of punger lane bottom
Dim PLGain: PLGain = (1-PLOffset)/(PLTop-PLBottom)

Dim BallColorCache(4)
Dim BallColorInit : BallColorInit = False

Sub UpdateBallBrightness
	Dim s, b_base, b_r, d_w, bx, by, bz
	' PERF: read each ball property once, run the plunger-lane polygon test only when the
	' ball is inside the lane's X band, and only write ball.color when the value changed.
	If Not BallColorInit Then
		For s = 0 To 4 : BallColorCache(s) = -1 : Next
		BallColorInit = True
	End If
	b_base = 120 * BallBrightness + 135*gilvl	' orig was 120 and 70

	For s = 0 To UBound(gBOT)
		bx = gBOT(s).x : by = gBOT(s).y : bz = gBOT(s).z
		' Handle z direction
		d_w = b_base*(1 - (bz-25)/500)
		If d_w < 30 Then d_w = 30
		' Handle plunger lane
		If bx >= PLLeft And bx <= PLRight Then
			If InRect(bx,by,PLLeft,PLBottom,PLLeft,PLTop,PLRight,PLTop,PLRight,PLBottom) Then  
				d_w = d_w*(PLOffset+PLGain*(by-PLBottom))
			End If
		End If
		' Assign color
		b_r = Int(d_w)
		If b_r > 255 Then b_r = 255
		If b_r <> BallColorCache(s) Then
			BallColorCache(s) = b_r
			gBOT(s).color = b_r + (b_r * 256) + (b_r * 65536)
		End If
	Next
End Sub


'******************************************************
'	ZKEY: Key Press Handling
'******************************************************

Sub Table1_KeyDown(ByVal keycode)	'***What to do when a button is pressed***

	If keycode = LeftFlipperKey Then 
		VRFlipperButtonLeft.X = 1921.245 + 8
	End If
	If keycode = RightFlipperKey Then
		VRFlipperButtonRight.X = 1926.281 - 8
	End If

	If keycode = StartGameKey Then
		VR_StartButton.y = 927.9619 - 5
		Controller.Switch(13) = 1
		SoundStartButton
	End If


	If KeyCode = PlungerKey Then
		Plunger.Pullback:SoundPlungerPull()
		If VR_Room = 1 Then TimerVRPlunger.Enabled = True
		If VR_Room = 1 Then TimerVRPlunger1.Enabled = False
		VR_Primary_plunger.Y = 810
	End If


'	If keycode = 20 Then 'T key to add Token for testing
'		vpmTimer.PulseSw 118
'		coinAnimation
'	End If

	If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft			' Sets the nudge angle and power
	If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight		' ^
	If keycode = CenterTiltKey Then Nudge 0, 1 : SoundNudgeCenter		' ^
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If
	If vpmKeyDown(keycode) Then Exit Sub   
End Sub
 
Sub Table1_KeyUp(ByVal keycode)		'***What to do when a button is released***


	If keycode = LeftFlipperKey Then 
		VRFlipperButtonLeft.X = 1921.245
	End If

	If keycode = RightFlipperKey Then
		VRFlipperButtonRight.X = 1926.281
	End If

	If keycode = StartGameKey Then
		VR_StartButton.y = 927.9619
		Controller.Switch(13) = 0
	End If


	If keycode = PlungerKey Then 
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		If VR_Room = 1 Then TimerVRPlunger.Enabled = False
		If VR_Room = 1 Then TimerVRPlunger1.Enabled = True
		VR_Primary_plunger.Y = 810
	End If

	If vpmKeyUp(keycode) Then Exit Sub
End Sub



'******************************************************
' VR Plunger code
'******************************************************

Sub TimerVRPlunger_Timer
  If VR_Primary_plunger.Y < 900 then
  		VR_Primary_plunger.Y = VR_Primary_plunger.Y + 5
  End If
End Sub

Sub TimerVRPlunger1_Timer
  VR_Primary_plunger.Y = 810 + (5* Plunger.Position) -20
End Sub


'******************************************************
'	ZSOL: Solenoids & Flashers
'******************************************************

SolCallBack(1)  		= "SolLeftKickBack"
SolCallBack(2)  		= "SolTokenRelease ""L"", 82, "
SolCallback(3)  		= "SolVarireset"
SolCallBack(4)  		= "SolTokenRelease ""R"", 81, "
SolCallback(5)  		= "SolBankKick"
SolCallback(6) 			= "SolPopperKickUp"
SolCallback(7) 			= "SolRampDiverter"
SolCallBack(8)  		= "SolRightKickBack"
SolCallback(9)	 		= "ReleaseBall"

SolCallback(15) 		= "SolUpperLeftTargetsUp"
SolCallback(16) 		= "SolUpperRightTargetsUp"

SolModCallBack(17) 		= "SolFlash17"					'Flasher Dome Top Left
SolModCallBack(18) 		= "SolFlash18"					'Flasher Dome Top Right
SolModCallBack(19) 		= "SolFlash19"					'Flasher Dome Middle Right
SolModCallBack(20) 		= "SolFlash20"					'Flasher Dome Lower Right
SolModCallBack(21) 		= "SolFlash21"					'Flasher Dome Middle Left
SolModCallBack(22) 		= "SolFlash22"					'Flasher Dome Lower Left

SolCallback(23) 		= "SolFlasherStripSeq1"
SolCallback(24) 		= "SolFlasherStripSeq2"

SolCallback(25)         = "SolPopperEject"
SolCallback(26)         = "SolRotateBeacons"
SolCallback(27) 		= "SolLowerLeftTargetsUp"
SolCallback(28) 		= "SolLowerRightTargetsUp"

SolCallback(35) 		= "SolPlunger"
SolCallback(36) 		= "SolLockupRelease"

SolCallback(sLRFlipper) 	= "SolRFlipper"
SolCallback(sLLFlipper) 	= "SolLFlipper"


'******************************************************
'	ZDRN: Drain, Trough, and Ball Release
'******************************************************

'********************* TROUGH *************************

Sub sw35_Hit():Controller.Switch(35) = 1:UpdateTrough:End Sub
Sub sw35_UnHit():Controller.Switch(35) = 0:UpdateTrough:End Sub
Sub sw34_Hit():Controller.Switch(34) = 1:UpdateTrough:End Sub
Sub sw34_UnHit():Controller.Switch(34) = 0:UpdateTrough:End Sub
Sub sw33_Hit():Controller.Switch(33) = 1:UpdateTrough:End Sub
Sub sw33_UnHit():Controller.Switch(33) = 0:UpdateTrough:End Sub
Sub sw32_Hit():Controller.Switch(32) = 1:UpdateTrough:End Sub
Sub sw32_UnHit():Controller.Switch(32) = 0:UpdateTrough:End Sub
Sub sw31_Hit():Controller.Switch(31) = 1:UpdateTrough:End Sub
Sub sw31_UnHit():Controller.Switch(31) = 0:UpdateTrough:End Sub


Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw31.BallCntOver = 0 Then Drain.kick 60, 9
	If sw32.BallCntOver = 0 Then sw33.kick 60, 9
	If sw33.BallCntOver = 0 Then sw34.kick 60, 9
	If sw34.BallCntOver = 0 Then sw35.kick 60, 9
	Me.Enabled = 0
End Sub


'*****************  DRAIN & RELEASE  ******************

Sub Drain_Hit() 'Drain
	UpdateTrough
	RandomSoundDrain Drain
End Sub

Sub ReleaseBall(enabled)
	If enabled Then 
		sw32.kick 60, 9
		sw31.kick 60, 9
		RandomSoundBallRelease sw31
		UpdateTrough
	End If
End Sub

Dim GameOn: GameOn=0

Sub sw18_hit()
	Controller.Switch(18) = 1
	BIPL=True
	GameOn = 1
End Sub

Sub sw18_unhit()
	Controller.Switch(18) = 0
	BIPL=False
End Sub


'*****************  Plunger  ******************

'Impulse Plunger
Sub SolPlunger(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
		SoundPlungerReleaseBall
	End If
End Sub

Const IMPowerSetting = 41
Const IMTime = 0.75
Set plungerIM = New cvpmImpulseP
With plungerIM
    .InitImpulseP swplunger, IMPowerSetting, IMTime
    .Random .5'.3
    .CreateEvents "plungerIM"
End With


'******************************************************
'	ZFLP: FLIPPERS
'******************************************************
Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		FlipperActivate LeftFlipper, LFPress
		LF.Fire
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

Sub SolRFlipper(Enabled)
	If Enabled Then
		FlipperActivate RightFlipper, RFPress
		RF.Fire
		RightUpperFlipper.RotateToEnd
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		RightUpperFlipper.RotateToStart
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


Sub RightUpperFlipper_Collide(parm)
	RightFlipperCollide parm
End Sub


'************************************************************
'	ZSLG: Slingshot Animations
'************************************************************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 48
	RandomSoundSlingshotRight SLING1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 12
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 47
	RandomSoundSlingshotLeft SLING2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 12
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
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


'************************************************************
'	ZSWI: SWITCHES 
'************************************************************

'************************* Bumpers **************************

Sub Bumper1_Hit(): RandomSoundBumperMiddle Bumper1: vpmTimer.PulseSw 46: End Sub
Sub Bumper2_Hit(): RandomSoundBumperTop Bumper2: vpmTimer.PulseSw 44: End Sub
Sub Bumper3_Hit(): RandomSoundBumperMiddle Bumper3: vpmTimer.PulseSw 45: End Sub


'*******************************************
' Rollovers
'******************************************* 

' Orbit
Sub sw15_Hit():	  Controller.Switch(15) = 1:End Sub
Sub sw15_UnHit(): Controller.Switch(15) = 0:End Sub

Sub sw28_Hit():   Controller.Switch(28) = 1:End Sub
Sub sw28_UnHit(): Controller.Switch(28) = 0:End Sub

' Top left and right lane
Sub sw67_Hit():   Controller.Switch(67) = 1:End Sub
Sub sw67_UnHit(): Controller.Switch(67) = 0:End Sub

Sub sw78_Hit():   Controller.Switch(78) = 1:End Sub
Sub sw78_UnHit(): Controller.Switch(78) = 0:End Sub

' Return lanes
Sub sw25_Hit():   Controller.Switch(25) = 1:End Sub
Sub sw25_UnHit(): Controller.Switch(25) = 0:End Sub

Sub sw26_Hit():   Controller.Switch(26) = 1:End Sub
Sub sw26_UnHit(): Controller.Switch(26) = 0:End Sub

Sub sw27_Hit():   Controller.Switch(27) = 1:End Sub
Sub sw27_UnHit(): Controller.Switch(27) = 0:End Sub

' Outlanes
Sub sw16_Hit():   Controller.Switch(16) = 1:End Sub
Sub sw16_UnHit(): Controller.Switch(16) = 0:End Sub

Sub sw17_Hit():   Controller.Switch(17) = 1:End Sub
Sub sw17_UnHit(): Controller.Switch(17) = 0:End Sub

' Right kickback switch
Sub sw41_Hit()
	Controller.Switch(41) = 0
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub sw41_Unhit():Controller.Switch(41) = 1:end sub


'*******************************************
' Rollover Animations
'******************************************* 

Sub sw15_Animate:	psw15.transz = sw15.CurrentAnimOffset: End Sub
Sub sw16_Animate:	psw16.transz = sw16.CurrentAnimOffset: End Sub
Sub sw17_Animate:	psw17.transz = sw17.CurrentAnimOffset: End Sub
Sub sw18_Animate:	psw18.transz = sw18.CurrentAnimOffset: End Sub
Sub sw26_Animate:	psw26.transz = sw26.CurrentAnimOffset: End Sub
Sub sw27_Animate:	psw27.transz = sw27.CurrentAnimOffset: End Sub
Sub sw28_Animate:	psw28.transz = sw28.CurrentAnimOffset: End Sub


'******************************************* 
' Ramp switches and ramp rolling sounds
'******************************************* 

Sub sw83_Hit()
	Controller.Switch(83) = 1
	WireRampOn True ' Play Plastic Ramp Sound
End sub

Sub sw83_Unhit():Controller.Switch(83) = 0:end sub

Sub sw84_Hit():Controller.Switch(84) = 1:end sub
Sub sw84_Unhit():Controller.Switch(84) = 0:end sub

Sub TriggerLeftEnd_Hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub TriggerWireStart_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub TriggerWireStart_unhit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub TriggerCellerStart_Hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub TriggerCellerEnd_Hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub TriggerRightStartE_Hit()
	WireRampOn True ' Play Plastic Ramp Sound
End Sub

Sub TriggerRightEndTop_Hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub TriggerPlasticStartRt_Hit()
	WireRampOn True ' Play Plastic Ramp Sound
End Sub

Sub TriggerPlasticStopRt_Hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub


'*******************************************
' Drop Targets
'******************************************* 

Sub sw61_Hit: DTHit 61: DT61up = 0: End Sub
Sub sw62_Hit: DTHit 62: DT62up = 0: End Sub
Sub sw63_Hit: DTHit 63: DT63up = 0: End Sub
Sub sw64_Hit: DTHit 64: DT64up = 0: End Sub
Sub sw65_Hit: DTHit 65: DT65up = 0: End Sub
Sub sw66_Hit: DTHit 66: DT66up = 0: End Sub
Sub sw71_Hit: DTHit 71: End Sub
Sub sw72_Hit: DTHit 72: End Sub
Sub sw73_Hit: DTHit 73: End Sub
Sub sw74_Hit: DTHit 74: DT74up = 0: End Sub
Sub sw75_Hit: DTHit 75: DT75up = 0: End Sub
Sub sw76_Hit: DTHit 76: DT76up = 0: End Sub


Sub SolUpperLeftTargetsUp(Enabled)
	If Enabled Then
		RandomSoundDropTargetReset sw62p
		DTRaise 61
		DTRaise 62
		DTRaise 63
		DT61up=1
		DT62up=1
		DT63up=1
		dtsh61.visible=True
		dtsh62.visible=True
		dtsh63.visible=True
	End If
End Sub

Sub SolUpperRightTargetsUp(Enabled)
	If Enabled Then
		RandomSoundDropTargetReset sw65p
		DTRaise 64
		DTRaise 65
		DTRaise 66
		DT64up=1
		DT65up=1
		DT66up=1
		dtsh64.visible=True
		dtsh65.visible=True
		dtsh66.visible=True
	End If
End Sub

Sub SolLowerLeftTargetsUp(Enabled)
	If Enabled Then
		RandomSoundDropTargetReset sw72p
		DTRaise 71
		DTRaise 72
		DTRaise 73
	End If
End Sub

Sub SolLowerRightTargetsUp(Enabled)
	If Enabled Then
		RandomSoundDropTargetReset sw75p
		DTRaise 74
		DTRaise 75
		DTRaise 76
		DT74up=1
		DT75up=1
		DT76up=1
		dtsh74.visible=True
		dtsh75.visible=True
		dtsh76.visible=True
	End If
End Sub


'********************* Standup Targets **********************

Sub sw51_hit: STHit 51: End Sub
Sub sw52_hit: STHit 52: End Sub
Sub sw53_hit: STHit 53: End Sub
Sub sw54_hit: STHit 54: End Sub
Sub sw55_hit: STHit 55: End Sub

Sub sw51o_Hit: TargetBouncer ActiveBall, 1: End Sub
Sub sw52o_Hit: TargetBouncer ActiveBall, 1: End Sub
Sub sw53o_Hit: TargetBouncer ActiveBall, 1: End Sub
Sub sw54o_Hit: TargetBouncer ActiveBall, 1: End Sub
Sub sw55o_Hit: TargetBouncer ActiveBall, 1: End Sub


'************************************************************
'	ZVUK: VUKs and Kickers
'************************************************************

'******************* VUKs **********************

' ===============================================================================================
' Roof ball trap
' ===============================================================================================

Dim RoofWalkStep

Sub RoofTrigger_Hit():   Controller.Switch(11) = 1: End Sub
Sub RoofTrigger_UnHit(): Controller.Switch(11) = 0: End Sub


' ===============================================================================================
' Kickers for bank, left, and right
' ===============================================================================================

' Bank Kicker

Dim KickerBall77		'Each VUK needs its own "kickerball"

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)	'Defines how KickBall works
	dim rangle
	rangle = PI * (kangle - 90) / 180
    
	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub

Sub sw77_Hit()	
    set KickerBall77 = activeball
	sw77wall.collidable = true
	Controller.Switch(77) = 1
	PlaySoundAtLevelActiveBall ("Metal_Touch_9"), Vol(ActiveBall) * .025
End Sub


Dim BallsInScoop, bis, bisz, TopBall

Sub SolBankKick(Enabled)
	If Enabled Then
        BallsInScoop = 0
        bisz = -130
		If Controller.Switch(77) <> 0 Then

            for each bis in gBOT 
                if InRect(bis.x, bis.y, 427,737,426,622,508,623,512,737) and bis.z < 0 then
                    if bis.z > bisz then
                        bisz = bis.z
                        Set TopBall = bis
                    end if
                    BallsInScoop = BallsInScoop + 1
                end if 
            Next
            If BallsInScoop > 1 Then
                TopBall.x = 471
                TopBall.y = 650
            End If

			KickBall KickerBall77, 0, 7, 100, 10
			SoundSaucerKick 1,sw77
			sw77wall.collidable = false
			Controller.Switch(77) = 0
		End If
	End If
End Sub

' Left Kicker

Dim BStep

Sub SolLeftKickBack(Enabled)
	If Enabled Then 
		Bob_Sling.rotx = 0
		BStep = 0
		LeftKickBack.Enabled = Enabled
	End If
End Sub

Sub LeftKickBack_Hit()
	LeftKickBack.Kick -1.5, 35 + Rnd() * 13
	RandomSoundMetal
	LeftKickBack.Enabled = False
End Sub

Sub LeftKickBack_UnHit()
	SoundSaucerKick 1,LeftKickBack
		Bob_Sling.rotx = 0
		BStep = 0
End Sub

Const KLeft = 50 			'X position of Left Kicker lane left
Const KRight = 110 			'X position of Left Kicker lane right
Const KTop = 990			'Y position of Left Kicker lane top
Const KBottom = 1055 		'Y position of Left Kicker lane bottom

Sub LeftKickTimer_Timer

	Dim inLeftKickBack, bkl
	inLeftKickBack = 0

	'Left Kickback
	For bkl = 0 To UBound(gBOT)
		If InRect(gBOT(bkl).x,gBOT(bkl).y,KLeft,KBottom,KLeft,KTop,KRight,KTop,KRight,KBottom) Then
			inLeftKickback = 1
		End If	
	Next

	if inLeftKickBack then
		Controller.Switch(42) = 1
	else
		Controller.Switch(42) = 0
			Select Case BStep
				Case 2:Bob_Sling.rotx = 10
				Case 30:Bob_Sling.rotx = 0
			End Select
			BStep = BStep + 1
	end if

End Sub


' Right Kicker

Sub RightKickBackTopTrigger_Hit()
	RightKickBackSlowDown.Enabled = True
End Sub
Sub RightKickBackSlowDown_Hit()
	RightKickBackSlowDown.Kick 180,0.1
End Sub

Sub SolRightKickBack(Enabled)
	If enabled=True then
		RightKickBack.Enabled = True
	end if
End Sub

Sub RightKickBack_Hit()
	RightKickBackSlowDown.Enabled = False
	RightKickBack.Kick 0, 50
	SoundSaucerKick 1,RightKickBack	
	RightKickBack.Enabled 		= False
End Sub


'******************************************************
' Top popper eject and kick up
'******************************************************

Dim PopperBall, PBStep
Sub sw68_Hit()
	Set PopperBall 		  		= Activeball
	SoundsaucerLock
	Controller.Switch(68) 		= 1
End Sub

Sub SolPopperKickUp(Enabled)
	If Enabled Then
		If Controller.Switch(68) Then
			SoundSaucerKick 1,sw68
			PBStep 			   	= 0
			sw68.TimerInterval 	= 15
			sw68.TimerEnabled  	= True
		End If
	End If
End Sub

Sub SolPopperEject(Enabled)
	If Enabled Then
		If Controller.Switch(68) Then		
			sw68.Kick 185 + Rnd() * 10, 9
			SoundSaucerKick 1,sw68
			PBStep 				= 99
			sw68.TimerInterval 	= 15
			sw68.TimerEnabled  	= True
		End if
	End If
End Sub

Sub sw68_Timer()
	Select Case PBStep
		Case 0
			PopperBall.z = 50
		Case 1
			PopperBall.z = 80 : PopperBall.y = PopperBall.y + 5 : Controller.Switch(68) = 0
		Case 2, 3, 4, 5, 6
			PopperBall.z = PopperBall.z + 25
		Case 7
			PopperBall.z = 240 : PopperBall.x = PopperBall.x - 25 : sw68.kick 270,4 : Me.TimerEnabled = False
		Case 99
			Me.TimerEnabled = False : Controller.Switch(68) = 0
	End Select
	PBStep = PBStep + 1
End Sub


'******************************************************
' Ramp diverter
'******************************************************

Dim diverterAng:diverterAng=0
Dim diverterStep:diverterStep=-1

InitRampDiverter

Sub InitRampDiverter()
	diverterAng=0
	diverterStep=-5
	pDiverter.rotz=diverterAng
	diverterWall.IsDropped = True
	diverterTimer.Enabled 	= False
End Sub

Sub SolRampDiverter(Enabled)
	diverterWall.IsDropped = Not Enabled
	If Enabled Then
		diverterStep=-8
		SoundRampDiverterDivert pDiverter
	Else
		diverterStep=10
		SoundRampDiverterBack pDiverter
	End If
	diverterTimer.Enabled 	= True
End Sub

Sub DiverterTimer_Timer()
	diverterAng = diverterAng + diverterStep
	if diverterAng<=-25 Then
		diverterTimer.Enabled 	= False
		diverterAng = -25
	end if
	if diverterAng>=0 Then
		diverterTimer.Enabled 	= False
		diverterAng = 0
	end if
	pDiverter.rotz = diverterAng
End Sub


'******************************************************
' Lock up
'******************************************************

Sub sw36_Hit(): Controller.Switch(36) = 1: End Sub
Sub sw37_Hit(): Controller.Switch(37) = 1: End Sub
Sub sw36_UnHit(): Controller.Switch(36) = 0: End Sub
Sub sw37_UnHit(): Controller.Switch(37) = 0: End Sub

dim pinAng:pinAng=0
dim pinStep:pinStep=10

Sub SolLockupRelease(Enabled)
	lockPinWall.IsDropped = Enabled
	if enabled=True Then
		pinStep=10
	Else	
		pinStep=-13
	end if
	lockPinWall.TimerEnabled=True
End Sub

Sub lockPinWall_Timer
	pinAng = pinAng + pinStep
	If pinAng>=45 Then
		pinAng=45
		lockPinWall.TimerEnabled=False
	end if
	if pinAng<=0 Then
		pinAng=0
		lockPinWall.TimerEnabled=False
	end If
	pLockPin.rotx=pinAng
end Sub


'******************************************************
'	ZBRL: BALL ROLLING AND DROP SOUNDS
'******************************************************

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

Dim RollSndName(5), RollSndInit
Sub RollingUpdate()
	Dim b
	If Not RollSndInit Then
		For b = 0 To 5 : RollSndName(b) = "BallRoll_" & b : Next
		RollSndInit = True
	End If

	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob - 1
		rolling(b) = False
		StopSound(RollSndName(b))
	Next

	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(gBOT)
		If BallVel(gBOT(b)) > 1 AND gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound (RollSndName(b)), -1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))

		Else
			If rolling(b) = True Then
				StopSound(RollSndName(b))
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
' 	ZRRL: RAMP ROLLING SFX
'******************************************************


dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(5,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(5)	

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

Sub RandomSoundRampStop(obj)
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtVol "wireramp_stop1", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtVol "wireramp_stop2", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtVol "wireramp_stop3", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub


'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************


'**************************************************************************************************
' Spinner ball code
'**************************************************************************************************

Dim discPosition, discSpinSpeed, discLastPos, SpinCounter, maxvel
dim spinAngle, degAngle, startAngle, postSpeedFactor
dim discX, discY
startAngle = 7
discX = 147
discY = 785
PostSpeedFactor = 90

Const cDiscSpeedMult = 77.5 '90             ' Affects speed transfer to object (deg/sec)
Const cDiscFriction = 2 '2.0              ' Friction coefficient (deg/sec/sec)
Const cDiscMinSpeed = 1               ' Object stops at this speed (deg/sec)
Const cDiscRadius = 50
Const cBallSpeedDampeningEffect = 0.45 ' The ball retains this fraction of its speed due to energy absorption by hitting the disc.

Sub SpinnerBallTimer_Timer()

	Dim oldDiscSpeed
	oldDiscSpeed = discSpinSpeed

	discPosition = discPosition + discSpinSpeed * Me.Interval / 1000
	discSpinSpeed = discSpinSpeed * (1 - cDiscFriction * Me.Interval / 1000)

	Do While discPosition < 0 : discPosition = discPosition + 360 : Loop
	Do While discPosition > 360 : discPosition = discPosition - 360 : Loop

	If Abs(discSpinSpeed) < cDiscMinSpeed Then
		discSpinSpeed = 0
	End If

	degAngle = -180 + startAngle + discPosition

	spinAngle = PI * (degAngle) / 180
	
	SpinnerBall.x = discX + (cDiscRadius * Cos(spinAngle))
	SpinnerBall.y = discY + (cDiscRadius * Sin(spinAngle))
	SpinnerBall.z = 25

	pdisc1.objrotz = discPosition

	If ABS(discSpinSpeed*sin(spinAngle)/postSpeedFactor) < 0.05 Then
		SpinnerBall.velx = 0.05
	Else
		SpinnerBall.velx = - discSpinSpeed*sin(spinAngle)/postSpeedFactor
	End If

	If Abs(discSpinSpeed*cos(spinAngle)/postSpeedFactor) < 0.05 Then
		SpinnerBall.vely = 0.05
	Else
		SpinnerBall.vely = discSpinSpeed*cos(spinAngle)/postSpeedFactor		'0.05
	End If

	SpinnerBall.velz = 0

End Sub


Dim oldSwitchPos, switchCount

Sub SpinDiscSwitches_Timer()

	dim discPosDiff

	discPosDiff = oldswitchpos - discPosition

	if discPosDiff > 270 then discPosDiff = discPosDiff - 360
	if discPosDiff < -270 then discPosDiff = discPosDiff + 360

	if discPosDiff >= 3.75 Then
		oldSwitchPos = discPosition
		switchCount = switchCount - 1
	elseif discPosDiff < -3.75 Then
		oldSwitchPos = discPosition
		switchCount = switchCount + 1
	end if

	If switchCount > 4 Then switchCount = 1
	If switchCount < 1 Then switchCount = 4

	Select Case switchCount
		Case 1:Controller.Switch(85) = 1
		Case 2:Controller.Switch(86) = 1
		Case 3:Controller.Switch(85) = 0
		Case 4:Controller.Switch(86) = 0
	End Select

End Sub

'********************************************
' Ball Collision, spinner collision and Sound
'********************************************


Function GetCollisionAngle(ax, ay, bx, by)
	Dim ang
	Dim collisionV:Set collisionV = new jVector
	collisionV.SetXY ax - bx, ay - by
	GetCollisionAngle = collisionV.ang
End Function

Function NormAngle(angle)
	NormAngle = angle
	Dim pi:pi = 3.14159265358979323846
	Do While NormAngle>2 * pi
		NormAngle = NormAngle - 2 * pi
	Loop
	Do While NormAngle <0
		NormAngle = NormAngle + 2 * pi
	Loop
End Function
 
Class jVector
     Private m_mag, m_ang, pi
 
     Sub Class_Initialize
         m_mag = CDbl(0)
         m_ang = CDbl(0)
         pi = CDbl(3.14159265358979323846)
     End Sub
 
     Public Function add(anothervector)
         Dim tx, ty, theta
         If TypeName(anothervector) = "jVector" then
             Set add = new jVector
             add.SetXY x + anothervector.x, y + anothervector.y
         End If
     End Function
 
     Public Function multiply(scalar)
         Set multiply = new jVector
         multiply.SetXY x * scalar, y * scalar
     End Function
 
     Sub ShiftAxes(theta)
         ang = ang - theta
     end Sub
 
     Sub SetXY(tx, ty)
 
         if tx = 0 And ty = 0 Then
             ang = 0
          elseif tx = 0 And ty <0 then
             ang = - pi / 180 ' -90 degrees
          elseif tx = 0 And ty>0 then
             ang = pi / 180   ' 90 degrees
         else
             ang = atn(ty / tx)
             if tx <0 then ang = ang + pi ' Add 180 deg if in quadrant 2 or 3
         End if
 
         mag = sqr(tx ^2 + ty ^2)
     End Sub
 
     Property Let mag(nmag)
         m_mag = nmag
     End Property
 
     Property Get mag
         mag = m_mag
     End Property
 
     Property Let ang(nang)
         m_ang = nang
         Do While m_ang>2 * pi
             m_ang = m_ang - 2 * pi
         Loop
         Do While m_ang <0
             m_ang = m_ang + 2 * pi
         Loop
     End Property
 
     Property Get ang
         Do While m_ang>2 * pi
             m_ang = m_ang - 2 * pi
         Loop
         Do While m_ang <0
             m_ang = m_ang + 2 * pi
         Loop
         ang = m_ang
     End Property
 
     Property Get x
         x = m_mag * cos(ang)
     End Property
 
     Property Get y
         y = m_mag * sin(ang)
     End Property
 
     Property Get dump
         dump = "vector "
         Select Case CInt(ang + pi / 8)
             case 0, 8:dump = dump & "->"
             case 1:dump = dump & "/'"
             case 2:dump = dump & "/\"
             case 3:dump = dump & "'\"
             case 4:dump = dump & "<-"
             case 5:dump = dump & ":/"
             case 6:dump = dump & "\/"
             case 7:dump = dump & "\:"
         End Select
 
         dump = dump & " mag:" & CLng(mag * 10) / 10 & ", ang:" & CLng(ang * 180 / pi) & ", x:" & CLng(x * 10) / 10 & ", y:" & CLng(y * 10) / 10
     End Property
End Class


'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************


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
	'debug.print tableobj.name
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
	dim collAngle,bvelx,bvely,hitball

	If ball1.radius < 23 or ball2.radius < 23 then
		
		If ball1.radius < 23 Then
			collAngle = GetCollisionAngle(ball1.x,ball1.y,ball2.x,ball2.y)
			bvelx = ball2.velx
			bvely = ball2.vely
			set hitball = ball2
		else 
			collAngle = GetCollisionAngle(ball2.x,ball2.y,ball1.x,ball1.y)
			bvelx = ball1.velx
			bvely = ball1.vely
			set hitball = ball1
		End If

		dim discAngle
		discAngle = NormAngle(spinAngle)

		Dim mball, mdisc, rdisc, idisc
		
		discSpinSpeed = discSpinSpeed + sqr(bVelX ^2 + bVelY ^2) * sin(collAngle - discAngle) * cDiscSpeedMult

' Rubber hit random sounds
		Dim finalspeed
		finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
		If finalspeed > 5 Then
			RandomSoundRubberStrong 1
		End If
		If finalspeed <= 5 Then
			RandomSoundRubberWeak()
		End If

	Else

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

	End If

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


'/////////////////////////////  Diverters  ////////////////////////////
Dim Solenoid_Diverter_Enabled_SoundLevel, Solenoid_Diverter_Disabled_SoundLevel

Solenoid_Diverter_Enabled_SoundLevel = 1
Solenoid_Diverter_Disabled_SoundLevel = 0.4

'///////////////////////  RAMP DIVERTER SOLENOID - DIVERT  //////////////////////
Sub SoundRampDiverterDivert(obj)
	PlaySoundAtLevelStatic ("Diverter_Divert"), Solenoid_Diverter_Enabled_SoundLevel, obj
End Sub

'///////////////////////  RAMP DIVERTER SOLENOID - BACK  ///////////////////////
Sub SoundRampDiverterBack(obj)
	PlaySoundAtLevelStatic ("Diverter_Back"), Solenoid_Diverter_Disabled_SoundLevel, obj
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



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************


'******************************************************
' 	ZRDT:  DROP TARGETS by Rothbauerw
'******************************************************


'******************************************************
'  DROP TARGETS INITIALIZATION
'******************************************************

Class DropTarget
  Private m_primary, m_secondary, m_prim, m_sw, m_animate, m_isDropped

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Secondary(): Set Secondary = m_secondary: End Property
  Public Property Let Secondary(input): Set m_secondary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
  Public Property Let IsDropped(input): m_isDropped = input: End Property

  Public default Function init(primary, secondary, prim, sw, animate, isDropped)
    Set m_primary = primary
    Set m_secondary = secondary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate
    m_isDropped = isDropped

    Set Init = Me
  End Function
End Class

'Define a variable for each drop target
Dim DT61, DT62, DT63, DT64, DT65, DT66, DT71, DT72, DT73, DT74, DT75, DT76

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
' 	primary: 			primary target wall to determine drop
'	secondary:			wall used to simulate the ball striking a bent or offset target after the initial Hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							rotz must be used for orientation
'							rotx to bend the target back
'							transz to move it up and down
'							the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'	switch:				ROM switch number
'	animate:			Array slot for handling the animation instrucitons, set to 0
'						Values for animate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 
'   isDropped:			Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.

Set DT61 = (new DropTarget)(sw61, sw61a, sw61p, 61, 0, false)
Set DT62 = (new DropTarget)(sw62, sw62a, sw62p, 62, 0, false)
Set DT63 = (new DropTarget)(sw63, sw63a, sw63p, 63, 0, false)
Set DT64 = (new DropTarget)(sw64, sw64a, sw64p, 64, 0, false)
Set DT65 = (new DropTarget)(sw65, sw65a, sw65p, 65, 0, false)
Set DT66 = (new DropTarget)(sw66, sw66a, sw66p, 66, 0, false)
Set DT71 = (new DropTarget)(sw71, sw71a, sw71p, 71, 0, false)
Set DT72 = (new DropTarget)(sw72, sw72a, sw72p, 72, 0, false)
Set DT73 = (new DropTarget)(sw73, sw73a, sw73p, 73, 0, false)
Set DT74 = (new DropTarget)(sw74, sw74a, sw74p, 74, 0, false)
Set DT75 = (new DropTarget)(sw75, sw75a, sw75p, 75, 0, false)
Set DT76 = (new DropTarget)(sw76, sw76a, sw76p, 76, 0, false)


Dim DTArray
DTArray = Array(DT61, DT62, DT63, DT64, DT65, DT66, DT71, DT72, DT73, DT74, DT75, DT76)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 80 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

' Initial Drop Target Shadows - Avoids a light DT hit and shadows go off when not strong enough hit to drop the target.

Dim DTShadow(9)

DTShadowInit 1
DTShadowInit 2
DTShadowInit 3
DTShadowInit 4
DTShadowInit 5
DTShadowInit 6
DTShadowInit 7
DTShadowInit 8
DTShadowInit 9

' Initializes the drop targets for shadow logic below
Sub DTShadowInit(dtnbr)

	if dtnbr = 1 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 61)
	elseif dtnbr = 2 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 62)
	elseif dtnbr = 3 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 63)
	elseif dtnbr = 4 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 64)
	elseif dtnbr = 5 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 65)
	elseif dtnbr = 6 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 66)
	elseif dtnbr = 7 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 74)
	elseif dtnbr = 8 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 75)
	elseif dtnbr = 9 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 76)
	End If
End Sub


Sub DTHit(switch)
	Dim i, swmod

	i = DTArrayID(switch)
	
	If switch = 61 Then
		swmod = 1
	Elseif switch = 62 then
		swmod = 2
	Elseif switch = 63 then
		swmod = 3
	Elseif switch = 64 then
		swmod = 4
	Elseif switch = 65 then
		swmod = 5
	Elseif switch = 66 then
		swmod = 6
	Elseif switch = 74 then
		swmod = 7
	Elseif switch = 75 then
		swmod = 8
	Elseif switch = 76 then
		swmod = 9
	End If

	PlayTargetSound
	DTArray(i).animate = DTCheckBrick(ActiveBall,DTArray(i).prim)
	If DTArray(i).animate = 1 Or DTArray(i).animate = 3 Or DTArray(i).animate = 4 Then
		DTBallPhysics ActiveBall, DTArray(i).prim.rotz, DTMass

'	Controls Drop Shadow for a direct hit only
		if swmod=1 or swmod=2 or swmod=3 or swmod=4 or swmod=5 or swmod=6 or swmod=7 or swmod=8 or swmod=9 then
			DTShadow(swmod).visible = 0
		End If


	End If
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch)
	
	DTArray(i).animate =  - 1
	DoDTAnim
End Sub

Sub DTDrop(switch)
	Dim i
	i = DTArrayID(switch)
	
	DTArray(i).animate = 1
	DoDTAnim
End Sub

Function DTArrayID(switch)
	Dim i
	For i = 0 To UBound(DTArray)
		If DTArray(i).sw = switch Then
			DTArrayID = i
			Exit Function
		End If
	Next
End Function

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

'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim)
	Dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	Xintersect = (aBall.y - dtprim.y - Tan(bangle) * aball.x + Tan(rangle2) * dtprim.x) / (Tan(rangle2) - Tan(bangle))
	Yintersect = Tan(rangle2) * Xintersect + (dtprim.y - Tan(rangle2) * dtprim.x)
	
	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		If DTEnableBrick = 1 And  perpvel > DTBrickVel And DTBrickVel <> 0 And cdist < 8 Then
			DTCheckBrick = 3
		Else
			DTCheckBrick = 1
		End If
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		DTCheckBrick = 4
	Else
		DTCheckBrick = 0
	End If
End Function

Sub DoDTAnim()
	Dim i
	For i = 0 To UBound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch, animate)
	Dim transz, switchid
	Dim animtime, rangle
	
	switchid = switch
	
	Dim ind
	ind = DTArrayID(switchid)
	
	rangle = prim.rotz * PI / 180
	
	DTAnimate = animate
	
	If animate = 0 Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If (animate = 1 Or animate = 4) And animtime < DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		DTAnimate = animate
		Exit Function
	ElseIf (animate = 1 Or animate = 4) And animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 1 'If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0 'updated by rothbauerw to account for edge case
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		animate = 2
		SoundDropTargetDrop prim
	End If
	
	If animate = 2 Then
		transz = (animtime - DTDropDelay) / DTDropSpeed * DTDropUnits *  - 1
		If prim.transz >  - DTDropUnits  Then
			prim.transz = transz
		End If
		
		prim.rotx = DTMaxBend * Cos(rangle) / 2
		prim.roty = DTMaxBend * Sin(rangle) / 2
		
		If prim.transz <= - DTDropUnits Then
			prim.transz =  - DTDropUnits
			secondary.collidable = 0
			DTArray(ind).isDropped = True 'Mark target as dropped
			controller.Switch(Switchid mod 100) = 1
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		End If
	End If
	
	If animate = 3 And animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
	ElseIf animate = 3 And animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If
	
	If animate =  - 1 Then
		transz = (1 - (animtime) / DTDropUpSpeed) * DTDropUnits *  - 1
		
		If prim.transz =  - DTDropUnits Then
			Dim b
			'Dim gBOT
			'gBOT = GetBalls
			
			For b = 0 To UBound(gBOT)
				If InRotRect(gBOT(b).x,gBOT(b).y,prim.x, prim.y, prim.rotz, - 25, - 10,25, - 10,25,25, - 25,25) And gBOT(b).z < prim.z + DTDropUnits + 25 Then
					gBOT(b).velz = 20
				End If
			Next
		End If
		
		If prim.transz < 0 Then
			prim.transz = transz
		ElseIf transz > 0 Then
			prim.transz = transz
		End If
		
		If prim.transz > DTDropUpUnits Then
			DTAnimate =  - 2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = GameTime
		End If
		primary.collidable = 0
		secondary.collidable = 1
		DTArray(ind).isDropped = False 'Mark target as not dropped
		controller.Switch(Switchid mod 100) = 0
	End If
	
	If animate =  - 2 And animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay) / DTDropSpeed * DTDropUnits *  - 1 + DTDropUpUnits
		If prim.transz < 0 Then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0
			
			primary.collidable = 1
			secondary.collidable = 0
		End If
	End If
End Function

Function DTDropped(switchid)
	Dim ind
	ind = DTArrayID(switchid)
	
	DTDropped = DTArray(ind).isDropped
End Function


'******************************************************
'****  END DROP TARGETS
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
Dim ST51, ST52, ST53, ST54, ST55

'Set array with stand-up target objects
'
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

Set ST51 = (new StandupTarget)(sw51, sw51p, 51, 0)
Set ST52 = (new StandupTarget)(sw52, sw52p, 52, 0)
Set ST53 = (new StandupTarget)(sw53, sw53p, 53, 0)
Set ST54 = (new StandupTarget)(sw54, sw54p, 54, 0)
Set ST55 = (new StandupTarget)(sw55, sw55p, 55, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST51, ST52, ST53, ST54, ST55)

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


'******************************************************
'***	END STAND-UP TARGETS
'******************************************************



''***********************************************************************************
''****				       		VariTarget Handling	     				    	****
''***********************************************************************************

Function Distance2Obj(obj1, obj2)
	Distance2Obj = SQR((obj1.x - obj2.x)^2 + (obj1.y - obj2.y)^2)
End Function

Function dArcSin(x)
	If X = 1 Then
		dArcSin = 90
	ElseIf x = -1 Then
		dArcSin = -90
	Else
		dArcSin = Atn(X / Sqr(-X * X + 1))*180/PI
	End If
End Function


'******************************************************
'*   VARI TARGET
'******************************************************

Sub solVarireset(enabled)
	VTReset 1, enabled
End Sub

Sub variTrigger_Hit():   Controller.Switch(12) = 1: End Sub
Sub variTrigger_UnHit(): Controller.Switch(12) = 0: End Sub


Sub VariTargetStart_Hit: VTHit 1:End Sub

'Define a variable for each vari target
Dim VT1

'Set array with vari target objects
'
'	VariTargetvar = Array(primary, prim, swtich)
'   primary:		vp target to determine target hit
'   secondary:		vp target at the end of the vari target path
'   prim:		   	primitive target used for visuals and animation
'				   	IMPORTANT!!!
'				   	rotx must be used to offset the target animation
'   num:	 		unique number to identify the vari target
'   plength:		length from the pivot point of the primitive to the hit point/center of the target
'   width:		 	width of the vari target
'   kspring:	 	Spring strength constant
'	stops:			Number of notches in the vari target including start position, defines where the target will stop
'   rspeed:			return speed of the target in vp units per second
'   animate:		Arrary slot for handling the animation instrucitons, set to 0


dim v1dist: v1dist = Distance2Obj(VariTargetStart, VariTargetStop)

Set VT1 = (new VariTarget)(VariTargetStart, VariTargetStop, VariTargetp, 1, 325, 50, 5, 7, 600, 0)

const rotxstart = 8	'This needs to match the rotx start angle on VariTargetp

' Index, distance from seconardy, switch number (the first switch should fire at the first Stop {number of stops - 1})
VT1.addpoint 0, v1dist/6, 56
VT1.addpoint 1, v1dist*2/4, 57
VT1.addpoint 2, v1dist*3/4, 58


'Add all the Vari Target Arrays to Vari Target Animation Array
'   VTArray = Array(VT1, VT2, ....)
Dim VTArray
VTArray = Array(VT1)
		
Class VariTarget
	Private m_primary, m_secondary, m_prim, m_num, m_plength, m_width, m_kspring, m_stops, m_rspeed, m_animate
	Public Distances, Switches, Ball

	Public Property Get Primary(): Set Primary = m_primary: End Property
	Public Property Let Primary(input): Set m_primary = input: End Property

	Public Property Get Secondary(): Set Secondary = m_secondary: End Property
	Public Property Let Secondary(input): Set m_secondary = input: End Property

	Public Property Get Prim(): Set Prim = m_prim: End Property
	Public Property Let Prim(input): Set m_prim = input: End Property

	Public Property Get Num(): Num = m_num: End Property
	Public Property Let Num(input): m_num = input: End Property

	Public Property Get PLength(): PLength = m_plength: End Property
	Public Property Let PLength(input): m_plength = input: End Property

	Public Property Get Width(): Width = m_width: End Property
	Public Property Let Width(input): m_width = input: End Property

	Public Property Get KSpring(): KSpring = m_kspring: End Property
	Public Property Let KSpring(input): m_kspring = input: End Property

	Public Property Get Stops(): Stops = m_stops: End Property
	Public Property Let Stops(input): m_stops = input: End Property

	Public Property Get RSpeed(): RSpeed = m_rspeed: End Property
	Public Property Let RSpeed(input): m_rspeed = input: End Property

	Public Property Get Animate(): Animate = m_animate: End Property
	Public Property Let Animate(input): m_animate = input: End Property

	Public default Function init(primary, secondary, prim, num, plength, width, kspring, stops, rspeed, animate)
		Set m_primary = primary
		Set m_secondary = secondary
		Set m_prim = prim
		m_num = num
		m_plength = plength
		m_width = width
		m_kspring = kspring
		m_stops = stops
		m_rspeed = rspeed
		m_animate = animate

		Set Init = Me
		redim Distances(0)
		redim Switches(0)
	End Function

	Public Sub AddPoint(aIdx, dist, sw) 
		ShuffleArrays Distances, Switches, 1 : Distances(aIDX) = dist : Switches(aIDX) = sw : ShuffleArrays Distances, Switches, 0
	End Sub
End Class

'''''' VARI TARGET FUNCTIONS

Sub VTHit(num)
	Dim i
	i = VTArrayID(num)

	If VTArray(i).animate <> 2 Then
		VTArray(i).animate = 1 'STCheckHit(ActiveBall,VTArray(i).primary) 'We don't need STCheckHit because VariTarget geometry should only allow a valid hit
	End If
	
	 Set VTArray(i).ball = Activeball

	Activeball.velx = cor.BallVelx(Activeball.id)
	Activeball.vely = cor.BallVely(Activeball.id)

	DoVTAnim
End Sub

Sub VTReset(num, enabled)
	Dim i
	i = VTArrayID(num)

	If enabled = true then
		VTArray(i).animate = 2
	Else
		VTArray(i).animate = 1
	End If

	DoVTAnim
End Sub

Function VTArrayID(num)
	Dim i
	For i = 0 To UBound(VTArray)
		If VTArray(i).num = num Then
			VTArrayID = i
			Exit Function
		End If
	Next
End Function

Sub DoVTAnim()
	Dim i
	For i = 0 To UBound(VTArray)
		VTArray(i).animate = VTAnimate(VTArray(i))
	Next
End Sub

Function VTAnimate(arr)
	VTAnimate = arr.animate

	If arr.animate = 0  Then
		arr.primary.uservalue = 0
		VTAnimate = 0
		arr.primary.collidable = 1
		Exit Function
	ElseIf arr.primary.uservalue = 0 Then
		arr.primary.uservalue = GameTime
	End If

	If arr.animate <> 0 Then 
		Dim animtime, length, btdist, btwidth, angle
		Dim tdist, transP, transPnew, cstop, x
		cstop = 0
		animtime = GameTime - arr.primary.uservalue
		arr.primary.uservalue = GameTime
		length = Distance2Obj(arr.primary, arr.secondary)
		angle = arr.primary.orientation
		transP = dSin(arr.prim.rotx - rotxstart)*arr.plength  'previous distance target has moved from start minus the rotx start angle
		transPnew = transP + arr.rspeed * animtime/1000

		If arr.animate = 1 then
			for x = 0 to (arr.Stops - 1)
				dim d: d = -length * x / (arr.Stops - 1) 'stops at end of path, remove  - 1 to stop short of the end of path
				If transP - 0.01 <= d and transPnew + 0.01 >= d Then
					transPnew = d
					cstop = d
' 					debug.print x & " " & d
				End If
			next
		End If

		if not isEmpty(arr.ball) Then 
			arr.primary.collidable = 0
			tdist = 31.31 'distance between ball and target location on hit event

			btdist = DistancePL(arr.ball.x,arr.ball.y,arr.secondary.x,arr.secondary.y,arr.secondary.x+dcos(angle),arr.secondary.y+dsin(angle))-tdist 'distance between the ball and secondary target
			btwidth = DistancePL(arr.ball.x,arr.ball.y,arr.primary.x,arr.primary.y,arr.primary.x+dcos(angle+90),arr.primary.y+dsin(angle+90)) 'distance between the ball and the parallel patch of the target

			If transPnew + length => btdist and btwidth < arr.width/2 + 25 Then
				arr.ball.velx = arr.ball.velx - (arr.kspring * dsin(angle) * abs(transP) * animtime/1000)
				arr.ball.vely = arr.ball.vely + (arr.kspring * dcos(angle) * abs(transP) * animtime/1000)
				transPnew = btdist - length
				If arr.secondary.uservalue <> 1 then:PlayVTargetSound(arr.ball):arr.secondary.uservalue = 1:End If

debug.print "ball.velx: " & arr.ball.velx
debug.print "ball.vely: " & arr.ball.velx

			End If
			If btdist > length + tdist Then
				arr.ball = Empty
				arr.primary.collidable = 1
				arr.secondary.uservalue = 0
			End If
		End If
		arr.prim.rotx = dArcSin(transPnew/arr.plength) + rotxstart
		VTSwitch arr, transPnew	
		if arr.prim.rotx >= rotxstart Then
			arr.prim.rotx = rotxstart
			VTSwitch arr, 0 
			VTAnimate = 0
			Exit Function
		elseif cstop = transPnew and isEmpty(arr.ball) and arr.animate <> 2 Then
			VTAnimate = 0
'			debug.print cstop & " " & Controller.Switch(56) & " " & Controller.Switch(57) & " " & Controller.Switch(58)
		end If
	End If	
End Function

Sub VTSwitch(arr, transP)
	Dim x, count, sw
	sw = 0
	count = 0
	For each x in arr.distances
		If abs(transP) > x Then 
			sw = arr.switches(Count)
			count = count + 1
		End If
	Next
	For each x in arr.switches
		If x <> 0 Then Controller.Switch(x) = 0
	Next
	If sw <> 0 Then Controller.Switch(sw) = 1
End Sub

Sub PlayVTargetSound(ball)
	PlaySound SoundFX("Metal_Touch_7",DOFTargets), 0, Vol(Ball), AudioPan(Ball), 0, Pitch(Ball), 0, 0, AudioFade(Ball)
End Sub


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
'


' When using the built-in VPX lamp handler (UseLamp=1) then you can update the primitive inserts by using the associlated
' light's _Animate subroutine, as shown below. Comment this section out if you want to use Lampz instead.

Sub l11_animate: p11.BlendDisableLighting = 200 * (l11.GetInPlayIntensity / l11.Intensity): End Sub
Sub l12_animate: p12.BlendDisableLighting = 200 * (l12.GetInPlayIntensity / l12.Intensity): End Sub
Sub l13_animate: p13.BlendDisableLighting = 200 * (l13.GetInPlayIntensity / l13.Intensity): End Sub
Sub l14_animate: p14.BlendDisableLighting = 200 * (l14.GetInPlayIntensity / l14.Intensity): End Sub
Sub l15_animate: p15.BlendDisableLighting = 200 * (l15.GetInPlayIntensity / l15.Intensity): End Sub
Sub l16_animate: p16.BlendDisableLighting = 200 * (l16.GetInPlayIntensity / l16.Intensity): End Sub
Sub l17_animate: p17.BlendDisableLighting = 200 * (l17.GetInPlayIntensity / l17.Intensity): End Sub
Sub l18_animate: p18.BlendDisableLighting = 200 * (l18.GetInPlayIntensity / l18.Intensity): End Sub
Sub l21_animate: p21.BlendDisableLighting = 200 * (l21.GetInPlayIntensity / l21.Intensity): End Sub
Sub l22_animate: p22.BlendDisableLighting = 200 * (l22.GetInPlayIntensity / l22.Intensity): End Sub
Sub l23_animate: p23.BlendDisableLighting = 200 * (l23.GetInPlayIntensity / l23.Intensity): End Sub
Sub l24_animate: p24.BlendDisableLighting = 200 * (l24.GetInPlayIntensity / l24.Intensity): End Sub
Sub l25_animate: p25.BlendDisableLighting = 200 * (l25.GetInPlayIntensity / l25.Intensity): End Sub
Sub l26_animate: p26.BlendDisableLighting = 200 * (l26.GetInPlayIntensity / l26.Intensity): End Sub
Sub l27_animate: p27.BlendDisableLighting = 200 * (l27.GetInPlayIntensity / l27.Intensity): End Sub
Sub l28_animate: p28.BlendDisableLighting = 200 * (l28.GetInPlayIntensity / l28.Intensity): End Sub
Sub l31_animate: p31.BlendDisableLighting = 200 * (l31.GetInPlayIntensity / l31.Intensity): End Sub
Sub l32_animate: p32.BlendDisableLighting = 200 * (l32.GetInPlayIntensity / l32.Intensity): End Sub
Sub l33_animate: p33.BlendDisableLighting = 200 * (l33.GetInPlayIntensity / l33.Intensity): End Sub
Sub l34_animate: p34.BlendDisableLighting = 200 * (l34.GetInPlayIntensity / l34.Intensity): End Sub
Sub l35_animate: p35.BlendDisableLighting = 200 * (l35.GetInPlayIntensity / l35.Intensity): End Sub
Sub l36_animate: p36.BlendDisableLighting = 200 * (l36.GetInPlayIntensity / l36.Intensity): End Sub
Sub l37_animate: p37.BlendDisableLighting = 200 * (l37.GetInPlayIntensity / l37.Intensity): End Sub
Sub l38_animate: p38.BlendDisableLighting = 200 * (l38.GetInPlayIntensity / l38.Intensity): End Sub
Sub l41_animate: p41.BlendDisableLighting = 200 * (l41.GetInPlayIntensity / l41.Intensity): End Sub
Sub l42_animate: p42.BlendDisableLighting = 200 * (l42.GetInPlayIntensity / l42.Intensity): End Sub
Sub l43_animate: p43.BlendDisableLighting = 200 * (l43.GetInPlayIntensity / l43.Intensity): End Sub
Sub l44_animate: p44.BlendDisableLighting = 200 * (l44.GetInPlayIntensity / l44.Intensity): End Sub
Sub l45_animate: p45.BlendDisableLighting = 200 * (l45.GetInPlayIntensity / l45.Intensity): End Sub
Sub l46_animate: p46.BlendDisableLighting = 200 * (l46.GetInPlayIntensity / l46.Intensity): End Sub
Sub l47_animate: p47.BlendDisableLighting = 200 * (l47.GetInPlayIntensity / l47.Intensity): End Sub
Sub l48_animate: p48.BlendDisableLighting = 200 * (l48.GetInPlayIntensity / l48.Intensity): End Sub
Sub l51_animate: p51.BlendDisableLighting = 200 * (l51.GetInPlayIntensity / l51.Intensity): End Sub
Sub l52_animate: p52.BlendDisableLighting = 200 * (l52.GetInPlayIntensity / l52.Intensity): End Sub
Sub l53_animate: p53.BlendDisableLighting = 200 * (l53.GetInPlayIntensity / l53.Intensity): End Sub
Sub l54_animate: p54.BlendDisableLighting = 200 * (l54.GetInPlayIntensity / l54.Intensity): End Sub
Sub l55_animate: p55.BlendDisableLighting = 200 * (l55.GetInPlayIntensity / l55.Intensity): End Sub
Sub l56_animate: p56.BlendDisableLighting = 200 * (l56.GetInPlayIntensity / l56.Intensity): End Sub
Sub l57_animate: p57.BlendDisableLighting = 200 * (l57.GetInPlayIntensity / l57.Intensity): End Sub
Sub l58_animate: p58.BlendDisableLighting = 200 * (l58.GetInPlayIntensity / l58.Intensity): End Sub
Sub l61_animate: p61.BlendDisableLighting = 200 * (l61.GetInPlayIntensity / l61.Intensity): End Sub
Sub l62_animate: p62.BlendDisableLighting = 200 * (l62.GetInPlayIntensity / l62.Intensity): End Sub
Sub l63_animate: p63.BlendDisableLighting = 200 * (l63.GetInPlayIntensity / l63.Intensity): End Sub
Sub l64_animate: p64.BlendDisableLighting = 200 * (l64.GetInPlayIntensity / l64.Intensity): End Sub
Sub l65_animate: p65.BlendDisableLighting = 200 * (l65.GetInPlayIntensity / l65.Intensity): End Sub
Sub l66_animate: p66.BlendDisableLighting = 200 * (l66.GetInPlayIntensity / l66.Intensity): End Sub
Sub l67_animate: p67.BlendDisableLighting = 200 * (l67.GetInPlayIntensity / l67.Intensity): End Sub
Sub l68_animate: p68.BlendDisableLighting = 200 * (l68.GetInPlayIntensity / l68.Intensity): End Sub
Sub l71_animate: p71.BlendDisableLighting = 200 * (l71.GetInPlayIntensity / l71.Intensity): End Sub
Sub l72_animate: p72.BlendDisableLighting = 200 * (l72.GetInPlayIntensity / l72.Intensity): End Sub
Sub l73_animate: p73.BlendDisableLighting = 200 * (l73.GetInPlayIntensity / l73.Intensity): End Sub
Sub l74_animate: p74.BlendDisableLighting = 200 * (l74.GetInPlayIntensity / l74.Intensity): End Sub
Sub l75_animate: p75.BlendDisableLighting = 200 * (l75.GetInPlayIntensity / l75.Intensity): End Sub
Sub l76_animate: p76.BlendDisableLighting = 200 * (l76.GetInPlayIntensity / l76.Intensity): End Sub
Sub l77_animate: p77.BlendDisableLighting = 200 * (l77.GetInPlayIntensity / l77.Intensity): End Sub
Sub l78_animate: p78.BlendDisableLighting = 200 * (l78.GetInPlayIntensity / l78.Intensity): End Sub


'******************************************************
'*****   END 3D INSERTS
'******************************************************


'******************************************************
'*****   Bumper and Backglass Lamps
'******************************************************

' PERF helper: only touch the VPX object when the ROM lamp state actually changed.
Dim LampVisCache(600), LampVisInit
Sub SetLampVis(n, obj, cacheOfs)
	Dim v, k
	If Not LampVisInit Then
		For k = 0 To 600 : LampVisCache(k) = -1 : Next
		For k = 0 To 87 : LampLastBank(k) = -1 : Next
		LampVisInit = True
	End If
	If controller.Lamp(n) = True Then v = 1 Else v = 0
	k = n + cacheOfs
	If LampVisCache(k) <> v Then
		LampVisCache(k) = v
		If v = 1 Then obj.visible = 1 Else obj.visible = 0
	End If
End Sub

Dim LampOnceHidden, LampLast88, LampLastBank(87)
Set LampCallback=GetRef("UpdateLamps")

Sub UpdateLamps()

' Bumpers

	If controller.Lamp(81) = True Then
		FlBumperFadeTarget(1) = 0.5
	Else
		FlBumperFadeTarget(1) = 0
	End If

	If controller.Lamp(82) = True Then
		FlBumperFadeTarget(2) = 0.5
	Else
		FlBumperFadeTarget(2) = 0
	End If

	If controller.Lamp(83) = True Then
		FlBumperFadeTarget(3) = 0.5
	Else
		FlBumperFadeTarget(3) = 0
	End If

' Start Button

	If controller.Lamp(88) = True Then 
		L88.opacity=40
		VR_StartButton.BlendDisableLighting = 0.75
	Else
		L88.opacity=0
		VR_StartButton.BlendDisableLighting = .2
	End If

' Bank, Cellar, and Roof lamps, 
	SetLampVis 84, bankLeftFlasher, 400
	If LampVisCache(84+400) <> LampLastBank(84) Then LampLastBank(84) = LampVisCache(84+400) : bankLeftLamp.BlendDisableLighting = 0.1 * LampVisCache(84+400)
	SetLampVis 85, bankRightFlasher, 400
	If LampVisCache(85+400) <> LampLastBank(85) Then LampLastBank(85) = LampVisCache(85+400) : bankRightLamp.BlendDisableLighting = 0.1 * LampVisCache(85+400)
	SetLampVis 86, cellarFlasher, 400
	If LampVisCache(86+400) <> LampLastBank(86) Then LampLastBank(86) = LampVisCache(86+400) : cellarLamp.BlendDisableLighting = 0.1 * LampVisCache(86+400)
	SetLampVis 87, roofFlasher, 400
	If LampVisCache(87+400) <> LampLastBank(87) Then LampLastBank(87) = LampVisCache(87+400) : roofLamp.BlendDisableLighting = 0.1 * LampVisCache(87+400)


' Backglass Lamps

dim xb2s
if desktopmode = true AND useownb2s = 1 Then
	' PERF: hide the built-in backglass lamps once instead of on every lamp update
	If Not LampOnceHidden Then
		for each xb2s in BGArr
			xb2s.visible = 0
		Next
	End If

elseif cab_mode = 1 Then
	If Not LampOnceHidden Then
		for each xb2s in BGArr
			xb2s.visible = 0
		Next
	End If

Else

if cab_mode = 0 Then

	SetLampVis 1, l1, 0
	SetLampVis 2, l2, 0
	SetLampVis 3, l3, 0
	SetLampVis 4, l4, 0
	SetLampVis 5, l5, 0
	SetLampVis 6, l6, 0
	SetLampVis 7, l7, 0
	SetLampVis 8, l8, 0
	SetLampVis 9, l9, 0
	SetLampVis 10, l10, 0
	SetLampVis 19, l19, 0
	SetLampVis 20, l20, 0
	SetLampVis 29, l29, 0
	SetLampVis 91, l91, 0
	SetLampVis 92, l92, 0
	SetLampVis 93, l93, 0
	SetLampVis 94, l94, 0
	SetLampVis 95, l95, 0
	SetLampVis 96, l96, 0
	SetLampVis 97, l97, 0
	SetLampVis 98, l98, 0
	SetLampVis 101, l101, 0
	SetLampVis 102, l102, 0
	SetLampVis 103, l103, 0
	SetLampVis 104, l104, 0
	SetLampVis 105, l105, 0
	SetLampVis 106, l106, 0
	SetLampVis 107, l107, 0
	SetLampVis 108, l108, 0
	SetLampVis 111, l111, 0
	SetLampVis 112, l112, 0
	SetLampVis 113, l113, 0
	SetLampVis 114, l114, 0
	SetLampVis 115, l115, 0
	SetLampVis 116, l116, 0
	SetLampVis 117, l117, 0
	SetLampVis 118, l118, 0
	SetLampVis 121, l121, 0
	SetLampVis 122, l122, 0
	SetLampVis 123, l123, 0
	SetLampVis 124, l124, 0
	SetLampVis 125, l125, 0
	SetLampVis 126, l126, 0
	SetLampVis 127, l127, 0
	SetLampVis 128, l128, 0
	SetLampVis 131, l131, 0
	SetLampVis 132, l132, 0
	SetLampVis 133, l133, 0
	SetLampVis 134, l134, 0
	SetLampVis 135, l135, 0
	SetLampVis 136, l136, 0
	SetLampVis 137, l137, 0
	SetLampVis 138, l138, 0
	SetLampVis 141, l141, 0
	SetLampVis 142, l142, 0
	SetLampVis 143, l143, 0
	SetLampVis 144, l144, 0
	SetLampVis 145, l145, 0
	SetLampVis 146, l146, 0
	SetLampVis 147, l147, 0
	SetLampVis 148, l148, 0

End if


' Backglass Lamp Reflections

	If ShowGlass = 1 AND VR_Room = 1 Then

		SetLampVis 2, l2m, 200
		SetLampVis 3, l3m, 200
		SetLampVis 4, l4m, 200
		SetLampVis 5, l5m, 200
		SetLampVis 6, l6m, 200
		SetLampVis 7, l7m, 200
		SetLampVis 8, l8m, 200
		SetLampVis 19, l19m, 200
		SetLampVis 20, l20m, 200
		SetLampVis 29, l29m, 200
		SetLampVis 91, l91m, 200
		SetLampVis 92, l92m, 200
		SetLampVis 93, l93m, 200
		SetLampVis 94, l94m, 200
		SetLampVis 95, l95m, 200
		SetLampVis 96, l96m, 200
		SetLampVis 97, l97m, 200
		SetLampVis 98, l98m, 200
		SetLampVis 101, l101m, 200
		SetLampVis 102, l102m, 200
		SetLampVis 103, l103m, 200
		SetLampVis 104, l104m, 200
		SetLampVis 105, l105m, 200
		SetLampVis 106, l106m, 200
		SetLampVis 107, l107m, 200
		SetLampVis 108, l108m, 200
		SetLampVis 111, l111m, 200
		SetLampVis 112, l112m, 200
		SetLampVis 113, l113m, 200
		SetLampVis 114, l114m, 200
		SetLampVis 115, l115m, 200
		SetLampVis 116, l116m, 200
		SetLampVis 117, l117m, 200
		SetLampVis 118, l118m, 200
		SetLampVis 121, l121m, 200
		SetLampVis 122, l122m, 200
		SetLampVis 123, l123m, 200
		SetLampVis 124, l124m, 200
		SetLampVis 125, l125m, 200
		SetLampVis 126, l126m, 200
		SetLampVis 127, l127m, 200
		SetLampVis 128, l128m, 200
		SetLampVis 131, l131m, 200
		SetLampVis 132, l132m, 200
		SetLampVis 133, l133m, 200
		SetLampVis 134, l134m, 200
		SetLampVis 135, l135m, 200
		SetLampVis 136, l136m, 200
		SetLampVis 137, l137m, 200
		SetLampVis 138, l138m, 200
		SetLampVis 141, l141m, 200
		SetLampVis 142, l142m, 200
		SetLampVis 143, l143m, 200
		SetLampVis 144, l144m, 200
		SetLampVis 145, l145m, 200
		SetLampVis 146, l146m, 200
		SetLampVis 147, l147m, 200
		SetLampVis 148, l148m, 200

	Else

		' PERF: hide the VR glass reflections once, not on every lamp update
		If Not LampOnceHidden Then
			l2m.visible = 0
			l3m.visible = 0
			l4m.visible = 0
			l5m.visible = 0
			l6m.visible = 0
			l7m.visible = 0
			l8m.visible = 0
			l19m.visible = 0
			l20m.visible = 0
			l29m.visible = 0
			l91m.visible = 0	
			l91m.visible = 0
			l92m.visible = 0
			l93m.visible = 0
			l94m.visible = 0
			l95m.visible = 0
			l96m.visible = 0
			l97m.visible = 0
			l98m.visible = 0
			l101m.visible = 0
			l102m.visible = 0
			l103m.visible = 0
			l104m.visible = 0
			l105m.visible = 0
			l106m.visible = 0
			l107m.visible = 0
			l108m.visible = 0
			l111m.visible = 0
			l112m.visible = 0
			l113m.visible = 0
			l114m.visible = 0
			l115m.visible = 0
			l116m.visible = 0
			l117m.visible = 0
			l118m.visible = 0
			l121m.visible = 0
			l122m.visible = 0
			l123m.visible = 0
			l124m.visible = 0
			l125m.visible = 0
			l126m.visible = 0
			l127m.visible = 0
			l128m.visible = 0
			l131m.visible = 0
			l132m.visible = 0
			l133m.visible = 0
			l134m.visible = 0
			l135m.visible = 0
			l136m.visible = 0
			l137m.visible = 0
			l138m.visible = 0
			l141m.visible = 0
			l142m.visible = 0
			l143m.visible = 0
			l144m.visible = 0
			l145m.visible = 0
			l146m.visible = 0
			l147m.visible = 0
			l148m.visible = 0
		End If

	End If

End If

	LampOnceHidden = True
End Sub


'******************************************************
'****  ZGIU:	GI Control
'******************************************************

dim gilvl

Set GICallback2 = GetRef("GIUpdates2")		'use this for stepped/modulated GI

'GIupdates2 is called always when some event happens to GI channel.
Dim GILastApplied : GILastApplied = -1
Dim GILastOnState : GILastOnState = -1
Sub GIUpdates2(aNr, aLvl)	
	' PERF: GI callbacks fire on every PWM step, often with an unchanged value. Skip those.
	If aNr = 0 Then
		If aLvl = GILastApplied Then Exit Sub
		GILastApplied = aLvl
	End If
'	debug.print "GIUpdates2 nr: " & aNr & " value: " & aLvl

	Select Case aNr 'Strings are selected here

		Case 0:  'GI String 0

			' Update the state for each GI light. The state will be a float value between 0 and 1.
			Dim Bulb, girubbercolor, girubbercolor2, girubbercolor3
			For Each Bulb in GI: Bulb.State = aLvl: Next

			' If the GI has an associated Relay sound, this can be played 
			If aLvl >= 0.5 And gilvl < 0.5 Then
				Sound_GI_Relay 1, Bumper1 'Note: Bumper1 is just used for sound positioning. Can be anywhere that makes sense.
			ElseIf aLvl <= 0.4 And gilvl > 0.4 Then
				Sound_GI_Relay 0, Bumper1
			End If

'	GI effects for various other elements on table

			For Each Bulb in dtshadows: Bulb.opacity = 130 * alvl: Next
			For Each Bulb in BulbPrims: Bulb.blenddisablelighting = 0.45 * aLvl + .05: Next
			Bulb_Prim_009.blenddisablelighting = 0.15 * aLvl + .05
			For Each Bulb in GICutouts: Bulb.blenddisablelighting = 0.25 * alvl + .15: Next
			For each Bulb in GICutoutWalls: Bulb.blenddisablelighting = .25 * alvl + .05: Next
			pLeftFlipper.blenddisablelighting = 0.045 * alvl + .005
			prightFlipper.blenddisablelighting = 0.045 * alvl + .005
			pRightUpperFlipper.blenddisablelighting = 0.2 * alvl + .15
			For Each Bulb in GITargets: Bulb.blenddisablelighting = 0.15 * alvl + .1: Next
			For Each Bulb in GIStandups: Bulb.blenddisablelighting = 0.1 * alvl + .05: Next
			For Each Bulb in GIPegs: Bulb.blenddisablelighting = .075 * alvl + .005: Next
			Apron.blenddisablelighting = 0.04 * alvl + .01
			For Each Bulb in GIScrews: Bulb.blenddisablelighting = .05 * alvl + .05: Next
			For Each Bulb in GILocknuts: Bulb.blenddisablelighting = .005 * alvl + .005: Next
			For Each Bulb in GIMetals: Bulb.blenddisablelighting = .0075 * alvl + .0075: Next
			For each Bulb in GILaneRollovers: Bulb.blenddisablelighting = .15 * alvl + .15: Next

			girubbercolor = 128*alvl + 128
			MaterialColor "Rubber White",RGB(girubbercolor,girubbercolor,girubbercolor)
			girubbercolor2 = 24*alvl + 220
			girubbercolor3 = 10*alvl + 14
			MaterialColor "Rubber red",RGB(girubbercolor2,girubbercolor3,girubbercolor3)

			' PERF: image swaps and DOF only on an actual off<->on transition, not every PWM step
			Dim giOnNow
			If aLvl = 0 Then giOnNow = 0 Else giOnNow = 1
			If giOnNow <> GILastOnState Then
				GILastOnState = giOnNow
				if aLvl = 0 Then
					scbgframe.imageA="SCBGFrame_Off"
					scbgframe2.imageA="SCBGFrame2_Off"
					scbgframe.imageB="SCBGFrame_Off"
					scbgframe2.imageB="SCBGFrame2_Off"
					DOF 101, DOFOff
				else
					scbgframe.imageA="SCBGFrame_On"
					scbgframe2.imageA="SCBGFrame2_On"
					scbgframe.imageB="SCBGFrame_On"
					scbgframe2.imageB="SCBGFrame2_On"
					DOF 101, DOFOn
				End If
			End If

			gilvl = aLvl		'Storing the latest GI fading state into global variable, so one can use it elsewhere too.

		Case 1:  'GI String 1

		Case 2:  'GI String 2

	End Select

End Sub

'******************************************************
'****  END GI Control
'******************************************************



'******************************************************
' 	ZFLD:  FLUPPER DOMES 
'******************************************************
' Based on FlupperDomes2.2

'------ Main Dome Code ---------'

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object			 ***
Set TableRef = Table1		   ' *** change this, if your table has another name				   ***
FlasherLightIntensity = 0.1	 ' *** lower this, if the VPX lights are too bright (i.e. 0.1)	   ***
FlasherFlareIntensity = 0.1	 ' *** lower this, if the flares are too bright (i.e. 0.1)		   ***
FlasherBloomIntensity = 0.2	 ' *** lower this, if the blooms are too bright (i.e. 0.1)		   ***	
FlasherOffBrightness = 0.5	  ' *** brightness of the flasher dome when switched off (range 0-2)  ***
' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20), ObjTargetLevel(20)

'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "white"
InitFlasher 2, "white"
InitFlasher 3, "yellow"
InitFlasher 4, "yellow"
InitFlasher 5, "red"
InitFlasher 6, "red"

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'   RotateFlasher 5,-12

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr)
	Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr)
	Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ = Atn( (tablewidth / 2 - objbase(nr).x) / (objbase(nr).y - tableheight * 1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ
		objflasher(nr).height = objbase(nr).z + 40
	End If
	
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0
	objlit(nr).visible = 0
	objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX
	objlit(nr).RotY = objbase(nr).RotY
	objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX
	objlit(nr).ObjRotY = objbase(nr).ObjRotY
	objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x
	objlit(nr).y = objbase(nr).y
	objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	
	'rothbauerw
	'Adjust the position of the flasher object to align with the flasher base.
	'Comment out these lines if you want to manually adjust the flasher object
	If objbase(nr).roty > 135 Then
		objflasher(nr).y = objbase(nr).y + 50
		objflasher(nr).height = objbase(nr).z + 20
	Else
		objflasher(nr).y = objbase(nr).y + 20
		objflasher(nr).height = objbase(nr).z + 50
	End If
	objflasher(nr).x = objbase(nr).x
	
	'rothbauerw
	'Adjust the position of the light object to align with the flasher base.
	'Comment out these lines if you want to manually adjust the flasher object
	objlight(nr).x = objbase(nr).x
	objlight(nr).y = objbase(nr).y
	objlight(nr).bulbhaloheight = objbase(nr).z - 10
	
	'rothbauerw
	'Assign the appropriate bloom image basked on the location of the flasher base
	'Comment out these lines if you want to manually assign the bloom images
	Dim xthird, ythird
	xthird = tablewidth / 3
	ythird = tableheight / 3
	If objbase(nr).x >= xthird And objbase(nr).x <= xthird * 2 Then
		objbloom(nr).imageA = "flasherbloomCenter"
		objbloom(nr).imageB = "flasherbloomCenter"
	ElseIf objbase(nr).x < xthird And objbase(nr).y < ythird Then
		objbloom(nr).imageA = "flasherbloomUpperLeft"
		objbloom(nr).imageB = "flasherbloomUpperLeft"
	ElseIf  objbase(nr).x > xthird * 2 And objbase(nr).y < ythird Then
		objbloom(nr).imageA = "flasherbloomUpperRight"
		objbloom(nr).imageB = "flasherbloomUpperRight"
	ElseIf objbase(nr).x < xthird And objbase(nr).y < ythird * 2 Then
		objbloom(nr).imageA = "flasherbloomCenterLeft"
		objbloom(nr).imageB = "flasherbloomCenterLeft"
	ElseIf  objbase(nr).x > xthird * 2 And objbase(nr).y < ythird * 2 Then
		objbloom(nr).imageA = "flasherbloomCenterRight"
		objbloom(nr).imageB = "flasherbloomCenterRight"
	ElseIf objbase(nr).x < xthird And objbase(nr).y < ythird * 3 Then
		objbloom(nr).imageA = "flasherbloomLowerLeft"
		objbloom(nr).imageB = "flasherbloomLowerLeft"
	ElseIf  objbase(nr).x > xthird * 2 And objbase(nr).y < ythird * 3 Then
		objbloom(nr).imageA = "flasherbloomLowerRight"
		objbloom(nr).imageB = "flasherbloomLowerRight"
	End If
	
	' set the texture and color of all objects
	Select Case objbase(nr).image
		Case "dome2basewhite"
		objbase(nr).image = "dome2base" & col
		objlit(nr).image = "dome2lit" & col
		
		Case "ronddomebasewhite"
		objbase(nr).image = "ronddomebase" & col
		objlit(nr).image = "ronddomelit" & col
		
		Case "domeearbasewhite"
		objbase(nr).image = "domeearbase" & col
		objlit(nr).image = "domeearlit" & col
	End Select
	If TestFlashers = 0 Then
		objflasher(nr).imageA = "domeflashwhite"
		objflasher(nr).visible = 0
	End If
	Select Case col
		Case "blue"
		objlight(nr).color = RGB(4,120,255)
		objflasher(nr).color = RGB(200,255,255)
		objbloom(nr).color = RGB(4,120,255)
		objlight(nr).intensity = 5000
		
		Case "green"
		objlight(nr).color = RGB(12,255,4)
		objflasher(nr).color = RGB(12,255,4)
		objbloom(nr).color = RGB(12,255,4)
		
		Case "red"
		objlight(nr).color = RGB(255,32,4)
		objflasher(nr).color = RGB(255,32,4)
		objbloom(nr).color = RGB(255,32,4)
		
		Case "purple"
		objlight(nr).color = RGB(230,49,255)
		objflasher(nr).color = RGB(255,64,255)
		objbloom(nr).color = RGB(230,49,255)
		
		Case "yellow"
		objlight(nr).color = RGB(200,173,25)
		objflasher(nr).color = RGB(255,200,50)
		objbloom(nr).color = RGB(200,173,25)
		
		Case "white"
		objlight(nr).color = RGB(255,240,150)
		objflasher(nr).color = RGB(100,86,59)
		objbloom(nr).color = RGB(255,240,150)
		
		Case "orange"
		objlight(nr).color = RGB(255,70,0)
		objflasher(nr).color = RGB(255,70,0)
		objbloom(nr).color = RGB(255,70,0)
	End Select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT And ObjFlasher(nr).RotX =  - 45 Then
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle)
	angle = ((angle + 360 - objbase(nr).ObjRotZ) Mod 180) / 30
	objbase(nr).showframe(angle)
	objlit(nr).showframe(angle)
End Sub


''------ Use this for PWM following domes ---------'

' PERF: skip work when the PWM level has not changed, hide the flare/bloom/lit
' objects while the dome is off (they were left visible at opacity 0, costing a
' transparent draw each), and avoid rebuilding the material name string per call.
Dim FlLastLevel(20), FlMatName(20), FlInitDone
Dim FlasherMinStep : FlasherMinStep = 0.01	'ignore PWM changes smaller than this (1% = invisible). 0 = update on every change.

Sub ModFlashFlasher(nr, aValue)
	If Not FlInitDone Then
		Dim fi
		For fi = 0 To 20 : FlLastLevel(fi) = -1 : FlMatName(fi) = "Flashermaterial" & fi : Next
		FlInitDone = True
	End If

	' unchanged, or a tiny step that cannot be seen (always let 0 and 1 through so it settles exactly)
	If aValue = FlLastLevel(nr) Then Exit Sub
	If aValue > 0 And aValue < 1 And FlLastLevel(nr) >= 0 Then
		If Abs(aValue - FlLastLevel(nr)) < FlasherMinStep Then Exit Sub
	End If
	FlLastLevel(nr) = aValue

	If aValue <= 0 Then
		' fully off: park everything at its "off" look and stop rendering the glow objects
		objflasher(nr).visible = 0 : objbloom(nr).visible = 0 : objlit(nr).visible = 0
		objlight(nr).IntensityScale = 0
		objbase(nr).BlendDisableLighting = FlasherOffBrightness
		objlit(nr).BlendDisableLighting = 0
		Exit Sub
	End If

	objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * aValue
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * aValue
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * aValue
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * aValue
	objlit(nr).BlendDisableLighting = 10 * aValue
	UpdateMaterial FlMatName(nr),0,0,0,0,0,0,aValue,RGB(255,255,255),0,0,False,True,0,0,0,0 
End Sub

Sub SolFlash17(level)				'Flasher Solonoid Name
	ModFlashFlasher 5,level			'Flasher Number assigned in flupper script
End Sub

Dim SF18Last : SF18Last = -1
Dim SF21Last : SF21Last = -1
Sub SolFlash18(level)				'Flasher Solonoid Name (Note both back right flasher dome and big PF flaher are tied to solenoid 18)
	ModFlashFlasher 6,level			'Flasher Number assigned in flupper script
	If level = SF18Last Then Exit Sub
	SF18Last = level
	F18.state = level				'Flasher light object name
	F18a.state = level				'Flasher light glow object name
	pF18.blenddisablelighting = 200 * level	'flasher primitive insert name
End Sub

Sub SolFlash19(level)				'Flasher Solonoid Name
	ModFlashFlasher 4,level			'Flasher Number assigned in flupper script
End Sub

Sub SolFlash20(level)				'Flasher Solonoid Name
	ModFlashFlasher 2,level			'Flasher Number assigned in flupper script
End Sub

Sub SolFlash21(level)				'Flasher Solonoid Name
	ModFlashFlasher 3,level			'Flasher Number assigned in flupper script
	If level = SF21Last Then Exit Sub
	SF21Last = level
	Fdisc.state = level				'Flasher light object name
	pdisc1.blenddisablelighting = 0.5 * level	'flasher primitive insert name
	OverheadLamp.blenddisablelighting = 0.15 * level	'flasher primitive insert name
End Sub

Sub SolFlash22(level)				'Flasher Solonoid Name
	ModFlashFlasher 1,level			'Flasher Number assigned in flupper script
End Sub



'******************************************************
'******  END FLUPPER DOMES
'******************************************************



'******************************************************
' 	ZFLB:  FLUPPER BUMPERS
'******************************************************
' Based on FlupperBumpers 0.145 final

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

Dim FlBumperFadeActual(6), FlBumperFadeTarget(6), FlBumperColor(6), FlBumperTop(6), FlBumperSmallLight(6), Flbumperbiglight(6)
Dim FlBumperDisk(6), FlBumperBase(6), FlBumperBulb(6), FlBumperscrews(6), FlBumperActive(6), FlBumperHighlight(6)
Dim cnt
For cnt = 1 To 6
	FlBumperActive(cnt) = False
Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight
FlInitBumper 1, "yellow"
FlInitBumper 2, "white"
FlInitBumper 3, "red"

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	
	' store all objects in an array for use in FlFadeBumper subroutine
	FlBumperFadeActual(nr) = 1
	FlBumperFadeTarget(nr) = 1.1
	FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr)
	FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr)
	Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr)
	Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr)
	FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr)
	FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	
	' set the color for the two VPX lights
	Select Case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0)
			FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0)
			FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
			
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255)
			FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255)
			FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8)
			FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32)
			FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255)
			MaterialColor "bumpertopmat" & nr, RGB(16,255,16)
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			
		Case "orange"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0)
			FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8)
			FlBumperBigLight(nr).colorfull = RGB(255,190,8)
			
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190)
			FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
			
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255)
			FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4)
			FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50)
			FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			FlBumperSmallLight(nr).TransmissionScale = 0
			
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255)
			FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255)
			FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	End Select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
	'   UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
	'			   OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
	'			   float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 ) * DayNightAdjust
	
	Select Case FlBumperColor(nr)
		Case "blue"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(38 - 24 * Z,130 - 98 * Z,255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z + 0.97 * Z ^ 3)
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z ^ 3) / (0.5 + DNA90)
			
		Case "green"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(16 + 16 * Sin(Z * 3.14),255,16 + 16 * Sin(Z * 3.14)), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z ^ 3) / (1 + DNA90)
			
		Case "red"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255, 16 - 11 * Z + 16 * Sin(Z * 3.14),0), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30 ^ 2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z ^ 3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z * 4,8 - Z * 8)
			
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255, 100 - 22 * z + 16 * Sin(Z * 3.14),Z * 32), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30 ^ 2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z ^ 3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,100 + Z * 50, 0)
			
		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 8 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z ^ 3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20 * Z,255 - 65 * Z)
			FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20 * Z,255 - 65 * Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z * 36,220 - Z * 90)
			
		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 1, RGB(30 - 27 * Z ^ 0.03,30 - 28 * Z ^ 0.01, 255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z ^ 3
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z ^ 3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255 - 240 * (Z ^ 0.1),255 - 240 * (Z ^ 0.1),255)
			FlBumperSmallLight(nr).colorfull = RGB(255 - 200 * z,255 - 200 * Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255 - 190 * Z,235 - z * 180,220 + 35 * Z)
			
		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255, 180 + 40 * z, 48 * Z), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30 ^ 2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z ^ 3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)
			
		Case "purple"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(128 - 118 * Z - 32 * Sin(Z * 3.14), 32 - 26 * Z ,255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15 + 200 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z + 0.97 * Z ^ 3)
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z ^ 3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128 - 60 * Z,32,255)
	End Select
End Sub

Sub BumperTimer_Timer
	Dim nr
	For nr = 1 To 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) And FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) And FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	Next
End Sub

'******************************************************
'******  END FLUPPER BUMPERS
'******************************************************


'***************************************************************
'	ZSHA: Ambient ball shadows
'***************************************************************

'Ambient (Room light source)
Const AmbientBSFactor = 0.9    '0 To 1, higher is darker
Const AmbientMovement = 1	   '1+ higher means more movement as the ball moves left and right
Const offsetX = 0			   'Offset x position under ball (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY = 0			   'Offset y position under ball (^^for example 5,5 if the light is in the back left corner)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
Dim objBallShadow(4)

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


Dim BSVis(4), BSLastX(4), BSLastY(4)
Sub BSUpdate
	' PERF: only write the shadow primitive when it is not yet shown / the ball actually moved
	Dim s, bz, bx, by, sx: For s = lob To UBound(gBOT)
		' *** Normal "ambient light" ball shadow
		
		'Primitive shadow on playfield, flasher shadow in ramps
		'** If on main and upper pf
		bz = gBOT(s).Z
		If bz > 20 And bz < 30 Then
			bx = gBOT(s).X : by = gBOT(s).Y
			If BSVis(s) <> 1 Then objBallShadow(s).visible = 1 : BSVis(s) = 1
			If bx <> BSLastX(s) Then
				BSLastX(s) = bx
				objBallShadow(s).X = bx + (bx - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
			End If
			If by <> BSLastY(s) Then
				BSLastY(s) = by
				objBallShadow(s).Y = by + offsetY
			End If
			'objBallShadow(s).Z = gBOT(s).Z + s/1000 + 1.04 - 25	

		'** No shadow if ball is off the main playfield (this may need to be adjusted per table)
		Else
'			objBallShadow(s).visible = 0
		End If
	Next
End Sub



'***************************************************************
' 	ZVRR: VR Room / VR Cabinet
'***************************************************************


' ***************************************************************************
'                    BASIC FSS(DMD,SS,EM) SETUP CODE
' ***************************************************************************

Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen

sub setup_backglassVR()

	xoff =-584
	yoff =-30
	zoff =1590
	xrot = -90


		xoff = xoff + 1000
		yoff = yoff + 300 
		zoff = zoff - 700
		xrot = -90
		Flasher3.visible = 1
		Flasher5.visible = 1
		Flasher4.visible = 1
		Flasher6.visible = 1
		Flasher7.visible = 1
		Flasher10.visible = 1
		Flasher11.visible = 1
		Flasher13.visible = 1
		Flasher15.visible = 1
		Flasher16.visible = 1
		Flasher17.visible = 1
		Flasher19.visible = 1

		scbgframe.visible = 1
		scbgframe2.visible = 0
		scbgFrameGlow.visible = 1

		Flasher20.visible = 0
		Flasher21.visible = 0
		Flasher22.visible = 0
		Flasher23.visible = 0
		

	scbghighsol23.x = xoff
	scbghighsol23.y = yoff
	scbghighsol23.height = zoff
	scbghighsol23.rotx = xrot
	scbghighsol23.visible = 0

	scbghighsol24.x = xoff
	scbghighsol24.y = yoff
	scbghighsol24.height = zoff
	scbghighsol24.rotx = xrot
	scbghighsol24.visible = 0

	scbgframe.x = xoff
	scbgframe.y = yoff 
	scbgframe.height = zoff
	scbgframe.rotx = xrot

	DMD.x = xoff + 20
	DMD.y = yoff
	DMD.height = zoff - 55
	DMD.rotx = xrot

	scbgFrameGlow.x = xoff
	scbgFrameGlow.y = yoff 
	scbgFrameGlow.height = zoff + 900
	scbgFrameGlow.rotx = xrot

	center_graphix()

end sub



sub setup_backglass()

	xoff =415
	yoff =0
	zoff =520
	xrot = -90

	xoff = xoff + 1000
	yoff = yoff + 300 

if cab_mode = 0 Then
	zoff = zoff - 700
Else
	zoff = zoff + 500
End If

	xrot = -90
	Flasher3.visible = 0
	Flasher5.visible = 0
	Flasher4.visible = 0
	Flasher6.visible = 0
	Flasher7.visible = 0
	Flasher10.visible = 0
	Flasher11.visible = 0
	Flasher13.visible = 0
	Flasher15.visible = 0
	Flasher16.visible = 0
	Flasher17.visible = 0
	Flasher19.visible = 0

	scbgframe.visible = 0

	if useownb2s = 1 Then 
		scbgframe2.visible = 0
		dmd.visible = 0
	Else
		scbgframe2.visible = 1
		dmd.visible = 1
	end If

	scbgFrameGlow.visible = 0

	Flasher20.visible = 0
	Flasher21.visible = 0
	Flasher22.visible = 0
	Flasher23.visible = 0

	scbghighsol23.x = xoff
	scbghighsol23.y = yoff
	scbghighsol23.height = zoff
	scbghighsol23.rotx = xrot
	scbghighsol23.visible = 0

	scbghighsol24.x = xoff
	scbghighsol24.y = yoff
	scbghighsol24.height = zoff
	scbghighsol24.rotx = xrot
	scbghighsol24.visible = 0

	scbgframe.x = xoff
	scbgframe.y = yoff 
	scbgframe.height = zoff
	scbgframe.rotx = xrot

	scbgframe2.x = xoff + 4
	scbgframe2.y = yoff 
	scbgframe2.height = zoff + 4
	scbgframe2.rotx = xrot

	DMD.x = xoff + 20
	DMD.y = yoff
	DMD.height = zoff - 55
	DMD.rotx = xrot

	scbgFrameGlow.x = xoff
	scbgFrameGlow.y = yoff 
	scbgFrameGlow.height = zoff + 900
	scbgFrameGlow.rotx = xrot

	center_graphix()

end sub



Dim BGArr 
BGArr=Array (l1, l2, l3, l4, l5, l6, l7, l8, l9,l10, l19,l20,l29, l91, l92, l93, l94, l95, l96, l97, l98, l101, l102, l103, l104, l105, l106, l107, l108,_
l111, l112, l113, l114, l115, l116, l117, l118, l121, l122, l123, l124, l125, l126, l127,_
l128, l131, l132, l133, l134, l135, l136, l137, l138, l141, l142, l143, l144, l145, l146, l147, l148,_
Flasher6,Flasher7,Flasher4,Flasher5,Flasher10,Flasher11,Flasher3,Flasher13,Flasher19,Flasher15,Flasher16,Flasher17,Flasher18)


Sub center_graphix()
	Dim xx,yy,yfact,xfact,xobj
	zscale = 0.0000001

	xcen =(888 /2) - (33 / 2)
	ycen = (793 /2 ) + (236 /2)


	yfact =140 'y fudge factor (ycen was wrong so fix)
	xfact =15

	For Each xobj In BGArr

		xx =xobj.x 
		
		xobj.x = (xoff -xcen) + xx +xfact
		yy = xobj.y ' get the yoffset before it is changed
		xobj.y =yoff 

		If(yy < 0.) then
			yy = yy * -1
		end if

	
		xobj.height =( zoff - ycen) + yy - (yy * zscale) + yfact
	
		xobj.rotx = xrot
		'xobj.visible =1 ' for testing
	Next

if VR_Room = 1 Then
	Flasher5.y=Flasher5.y + 55
	Flasher15.y=Flasher15.y + 55
	Flasher17.y=Flasher17.y + 55
End If

end sub

Sub SolFlasherStripSeq1(enabled)
	if enabled and cab_mode = 0 then

		if useownb2s = 1 Then
			scbghighsol23.visible = 0
		Else
			scbghighsol23.visible = 1
		end If

		If ShowGlass = 1 AND VR_Room = 1 Then
			FlMirrorSol23.visible = 1
		End If
	Else
		scbghighsol23.visible = 0
		FlMirrorSol23.visible = 0
	end If
end Sub

Sub SolFlasherStripSeq2(enabled)
	if enabled and cab_mode =0 then
		if useownb2s = 1 Then
			scbghighsol24.visible = 0
		Else
			scbghighsol24.visible = 1
		end If

		If ShowGlass = 1 AND VR_Room = 1 Then		
			FlMirrorSol24.visible = 1
		End If
	Else
		scbghighsol24.visible = 0
		FlMirrorSol24.visible = 0
	end If
end Sub

Dim nxx, DNS
DNS = Table1.NightDay
Dim OPSValues: OPSValues=Array (100,50,20,10 ,9,8 ,7 ,6 ,5, 4,3,2,1,0)
Dim DNSValues: DNSValues=Array (1.0,0.5,0.1,0.05,0.01,0.005,0.001,0.0005,0.0001, 0.00005,0.00001, 0.000005, 0.000001)
Dim SysDNSVal: SysDNSVal=Array (1.0,0.9,0.8,0.7,0.6,0.5,0.5,0.5,0.5, 0.5,0.5)
Dim DivValues: DivValues =Array (1,2,4,8,16,32,32,32,32, 32,32)
Dim DivValues2: DivValues2 =Array (1,1.5,2,2.5,3,3.5,4,4.5,5, 5.5,6)
Dim DivValues3: DivValues3 =Array (1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1)
Dim RValUP: RValUP=Array (30,60,90,120,150,180,210,240,255,255,255,255)
Dim GValUP: GValUP=Array (30,60,90,120,150,180,210,240,255,255,255,255)
Dim BValUP: BValUP=Array (30,60,90,120,150,180,210,240,255,255,255,255)
Dim RValDN: RValDN=Array (255,210,180,150,120,90,60,30,10,10,10)
Dim GValDN: GValDN=Array (255,210,180,150,120,90,60,30,10,10,10)
Dim BValDN: BValDN=Array (255,210,180,150,120,90,60,30,10,10,10)
Dim FValUP: FValUP=Array (35,40,45,50,55,60,65,70,75,80,85,90,95,100,105)
Dim FValDN: FValDN=Array (100,85,80,75,70,65,60,55,50,45,40,35,30)
Dim MVSAdd: MVSAdd=Array (0.9,0.9,0.8,0.8,0.7,0.7,0.6,0.6,0.5,0.5,0.4,0.3,0.2,0.1)
Dim ReflDN: ReflDN=Array (60,55,50,45,40,35,30,28,26,24,22,20,19,18,16,15,14,13,12,11,10)
Dim DarkUP: DarkUP=Array (1,2,3,4,5,6,6,6,6,6,6,6,6,6,6,6,6,6)

Dim DivLevel: DivLevel = 35
Dim DNSVal: DNSVal = Round(DNS/10)
Dim DNShift: DNShift = 1

' PLAYFIELD GLOBAL INTENSITY ILLUMINATION FLASHERS
Flasher22.opacity = OPSValues(DNSVal + DNShift) / DivValues(DNSVal)
Flasher22.intensityscale = DNSValues(DNSVal + DNShift) /DivValues(DNSVal)
Flasher23.opacity = OPSValues(DNSVal + DNShift) /DivValues(DNSVal)
Flasher23.intensityscale = DNSValues(DNSVal + DNShift) /DivValues(DNSVal)
Flasher20.opacity = OPSValues(DNSVal + DNShift) /DivValues(DNSVal)
Flasher20.intensityscale = DNSValues(DNSVal + DNShift) /DivValues(DNSVal)

'BACKBOX & BACKGLASS ILLUMINATION
scbgframe.ModulateVsAdd = MVSAdd(DNSVal)
scbgframe.Color = RGB(RValUP(DNSVal),GValUP(DNSVal),BValUP(DNSVal))
scbgframe.Amount = FValUP(DNSVal) / DivValues(DNSVal)

scbgframe2.ModulateVsAdd = MVSAdd(DNSVal)
scbgframe2.Color = RGB(RValUP(DNSVal),GValUP(DNSVal),BValUP(DNSVal))
scbgframe2.Amount = FValUP(DNSVal) / DivValues(DNSVal)

scbgframe.intensityscale = 0.4
scbgframe2.intensityscale = 0.4
scbghighsol23.intensityscale = 4.0
scbghighsol24.intensityscale = 4.0


'**************************************************************************************
'               [CC Color Correction] Textures must be CC corrected to work
'**************************************************************************************

Dim DivGlobal: DivGlobal = 0.6
Dim DivClrCor: DivClrCor = 0.6

Dim BlueDiv:BlueDiv  = 1.0* DivGlobal
Dim RedDiv:RedDiv  = 0.9* DivGlobal
Dim RedGreenDiv:RedGreenDiv  = 1.0* DivGlobal
Dim FullSpecDiv:FullSpecDiv  = 0.1* DivGlobal
Dim CCFull: CCFull = 1.0 * DivClrCor
Dim CCBlue: CCBlue = 1.0 * DivClrCor
Dim CCRedGreen: CCRedGreen = 0.9 * DivClrCor
Dim CCRed: CCRed = 0.9 * DivClrCor

Dim FlValues : FlValues=Array (1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0)
Dim AMTValues: AMTValues=Array (500,3000,4000,8000,16000,32000,64000,128000,128000,128000,128000,128000)
Dim BiasValues: BiasValues=Array (-100,-90,-80,-70,-60,-50,-40,-30,-20,-10,-9,-8)
Dim MixBlueChan: MixBlueChan=Array (8.0*BlueDiv, 16.0*BlueDiv, 34.0*BlueDiv, 64.0*BlueDiv, 72.0*BlueDiv, 80.0*BlueDiv, 84.0*BlueDiv, 82.0*BlueDiv, 80.0*BlueDiv, 78.0*BlueDiv, 76.0*BlueDiv)  
Dim MixRedChan: MixRedChan=Array (16.0 * RedDiv, 16.0* RedDiv, 34.0* RedDiv, 64.0* RedDiv, 72.0* RedDiv, 80.0* RedDiv, 84.0* RedDiv, 82.0* RedDiv, 80.0* RedDiv, 78.0* RedDiv, 76.0* RedDiv)  
Dim MixRedGreenChan: MixRedGreenChan=Array (8.0*RedGreenDiv, 16.0*RedGreenDiv, 34.0*RedGreenDiv, 64.0*RedGreenDiv, 72.0*RedGreenDiv, 80.0*RedGreenDiv, 84.0*RedGreenDiv, 82.0*RedGreenDiv, 80.0*RedGreenDiv, 78.0*RedGreenDiv, 76.0*RedGreenDiv)  
Dim MixFullSpectrum: MixFullSpectrum=Array (8.0*FullSpecDiv, 16.0*FullSpecDiv, 34.0*FullSpecDiv, 64.0*FullSpecDiv, 72.0*FullSpecDiv, 73.0*FullSpecDiv, 74.0*FullSpecDiv, 74.0*FullSpecDiv, 74.0*FullSpecDiv, 74.0*FullSpecDiv, 74.0*FullSpecDiv)  
Dim CCFSValues: CCFSValues=Array (100.0*CCFull, 1.0*CCFull, 0.1*CCFull, 0.01*CCFull, 0.001*CCFull, 0.0001*CCFull, 0.0001*CCFull, 0.0001*CCFull, 0.0001*CCFull,0.0001*CCFull,0.0001*CCFull)  
Dim CCBlueValues: CCBlueValues=Array (0.9*CCBlue, 1.0*CCBlue, 1.2*CCBlue, 1.4*CCBlue, 1.5*CCBlue, 1.55*CCBlue, 1.56*CCBlue, 1.565*CCBlue, 1.568*CCBlue, 1.569*CCBlue, 1.569*CCBlue)  

Dim FlData : FlData=Array (Flasher22,Flasher23,Flasher20,Flasher21,FlMirror,Empty,Empty,Empty)

'Full blue channel
FlData(0).amount = AMTValues(DNSVal ) * DivValues3(DNSVal)
FlData(0).DepthBias = BiasValues(DNSVal) * 100.0
FlData(0).intensityscale = FlData(0).intensityscale * MixBlueChan(DNSVal) '0.0008'
FlData(0).opacity = CCBlueValues(DNSVal) '* CCFSValues(DNSVal)'
'Half red/green channel
FlData(1).amount = AMTValues(DNSVal ) * DivValues3(DNSVal)
FlData(1).DepthBias = BiasValues(DNSVal) * 100.0
FlData(1).intensityscale = FlData(1).intensityscale * MixRedGreenChan(DNSVal)'* (MixRedChan(DNSVal) * SysDNSVal(DNSVal)) '0.0008'
FlData(1).opacity = CCRed
'Full red/green channel
FlData(2).amount = AMTValues(DNSVal ) * DivValues3(DNSVal)
FlData(2).DepthBias = BiasValues(DNSVal) * 100.0
FlData(2).intensityscale = FlData(2).intensityscale * MixRedGreenChan(DNSVal) '0.0008'
FlData(2).opacity = CCRedGreen
'Full Spectrum
FlData(3).amount = AMTValues(DNSVal ) * (DivValues3(DNSVal) * 0.1)
FlData(3).DepthBias = BiasValues(DNSVal) * 100.0
FlData(3).intensityscale = FlData(3).intensityscale * MixFullSpectrum(DNSVal) '0.0008'
FlData(3).opacity = FlData(3).opacity * CCFSValues(DNSVal)

'Full Spectrum Mirror
if VR_Room=0 Then
	FlData(4).amount = AMTValues(DNSVal ) * (DivValues3(DNSVal) * 0.1)
	FlData(4).DepthBias = BiasValues(DNSVal) * 100.0
	FlData(4).intensityscale = FlData(4).intensityscale * (MixFullSpectrum(DNSVal) /3.0)'0.0008'
	FlData(4).opacity = FlData(4).opacity * CCFSValues(DNSVal)
	FlValues(4) = FlData(4).intensityscale
End If

FlValues(0) = FlData(0).intensityscale
FlValues(1) = FlData(1).intensityscale
FlValues(2) = FlData(2).intensityscale
FlValues(3) = FlData(3).intensityscale


' ***************************************************************************
' ***************************************************************************

Const Forward = 190 ' 150
Const Height = 200 '250
Const RotX = -5 ' -5
' Full
FlData(0).y = FlData(0).y + Forward
FlData(0).Height = Height
FlData(0).RotX = Rotx
' upper half
FlData(1).y = FlData(1).y + Forward
FlData(1).Height = Height + 50
FlData(1).RotX = Rotx
'full
FlData(2).y = FlData(2).y + Forward
FlData(2).Height = Height
FlData(2).RotX = Rotx
' half
FlData(3).y = FlData(3).y + Forward
FlData(3).Height = Height+ 50
FlData(3).RotX = Rotx

if 0 then
'FlMirror.y = FlData().y + Forward '(THIS WAS COMMENTED OUT IN BOTH VERSIONS)

if VR_Room=0 Then
	FlMirror.Height = Height
	FlMirror.RotX = Rotx
End If

FlMirrorSol23.Height = Height
FlMirrorSol23.RotX = Rotx
FlMirrorSol24.Height = Height
FlMirrorSol24.RotX = Rotx
end if


'******************************************************
' COIN ANIM
'******************************************************
dim moveDown
sub coinAnimation
	coin.visible=1
	coin.x = 431
	coin.y = 0
	coin.z = 250
	coin.rotx=180
	coin.rotz=180+90
	moveDown=1
	Select Case Int(Rnd*4)+1
		Case 1 : coin.image="coin1"
		Case 2 : coin.image="coin2"
		Case 3 : coin.image="coin3"
		Case 4 : coin.image="coin4"
	End Select

	CoinTimer.enabled=1
end sub

Sub CoinTimer_Timer
	if moveDown then
		coin.y = coin.y + 20
'		PlaySound "fx_ballrolling8", -1, 1, AudioPan(coin), 0, 0, 1, 0, AudioFade(coin)
	end if

	If Table1.ShowDT = False Then
		If coin.y > 1200 Then	
			If coin.z > 150 then coin.z = coin.z - 20
		End If
	End If

	if coin.y>1700 And movedown then
		moveDown=0
		StopSound "fx_ballrolling8"
		PlaysoundAt "coin_spinning", coin
	end if
	if moveDown=0 Then
		coin.rotz=coin.rotz+10
		if coin.rotz>1080 Then
			coin.visible=0
			CoinTimer.enabled=0
			StopSound "coin_spinning"
		end if
	end if
end Sub

Sub SolTokenRelease(tube, switch, Enabled)
	If Enabled Then
		coinAnimation
		vpmTimer.PulseSw 43
		vpmTimer.PulseSw 118
	End If
	Controller.Switch(switch) = Enabled
End Sub


'******************************************************
' Options
'******************************************************

Dim VRThings

if VR_Room = 0 and cab_mode = 0 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRRoomb4asti:VRThings.visible = 0:Next
	for each VRThings in Rails:VRThings.visible = 1:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in VRTopper:VRThings.visible = 0:Next
	VR_Topper.visible = 0
	L_DT_1.Visible = 1

Elseif VR_Room = 0 and cab_mode = 1 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRRoomb4asti:VRThings.visible = 0:Next
	for each VRThings in Rails:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in VRTopper:VRThings.visible = 0:Next
	VR_Topper.visible = 0
	L_DT_1.Visible = 0
	L_DT_1.State = 0
	scbgframe2.visible = 0
	dmd.visible = 0
	scbgframe.visible = 0
	scbgframe2.visible = 0
	scbgFrameGlow.visible = 0
	Flasher20.visible = 0
	Flasher21.visible = 0
	Flasher22.visible = 0
	Flasher23.visible = 0

Else
	for each VRThings in VRStuff:VRThings.visible = 1:Next
	for each VRThings in Rails:VRThings.visible = 1:Next
	VR_Topper.visible = Topper
	L_DT_1.Visible = 0
	L_DT_1.State = 0

	If Siren = 1 Then
		for each VRThings in VRTopper:VRThings.visible = 1:Next
	Else
		for each VRThings in VRTopper:VRThings.visible = 0:Next
	End If

' b4asti's VR Room

	if VREnv = 5 Then
		for each VRThings in VRRoomb4asti:VRThings.visible = 1:Next
		for each VRThings in VRRoomstd:VRThings.visible = 0:Next
		for each VRThings in VRClock:VRThings.visible = 0:Next
		VRposter.visible = 0
		VRposter2.visible = 0
	Else
		for each VRThings in VRRoomb4asti:VRThings.visible = 0:Next
		for each VRThings in VRRoomstd:VRThings.visible = 1:Next
		for each VRThings in VRClock:VRThings.visible = WallClock:Next

' standard Walls, Floor, and Roof, UP, Sixtoe, or DarthVito

		if VREnv = 1 Then
			VR_Wall_Left.image = "VR_Wall_Left"
			VR_Wall_Right.image = "VR_Wall_Right"
			VR_Floor.image = "VR_Floor"
			VR_Roof.image = "VR_Roof"
		Elseif VREnv = 2 Then
			VR_Wall_Left.image = "VR_Wall_Left_DV2"
			VR_Wall_Right.image = "VR_Wall_Right_DV2"
			VR_Floor.image = "VR_Room Floor"
			VR_Roof.image = "VR_Room Roof 2"
		Elseif VREnv = 3 Then
			VR_Wall_Left.image = "VR_Wall_Left_DV1"
			VR_Wall_Right.image = "VR_Wall_Right_DV1"
			VR_Floor.image = "VR_Room Floor"
			VR_Roof.image = "VR_Room Roof 2"
		Elseif VREnv = 4 Then
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

	End If
End If


'******************************************************
' Relfection Options
'******************************************************

If ShowGlass = 1 and VR_Room = 1 Then
	DMDmirror.visible = 1
	FlMirror.visible = 1
	Pin1_WindowGlass2.visible = 1
Else 
	DMDmirror.visible = 0
	FlMirror.visible = 0
	Pin1_WindowGlass2.visible = 0
End If



' ***** Beer Bubble Code - Rawd *****
Sub BeerTimer()

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



' ***************** VR Clock code below - THANKS RASCAL ******************
Dim CurrentMinute ' for VR clock 
' VR Clock code below....
Sub ClockTimer_Timer()
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    Pseconds.RotAndTra2 = (Second(Now()))*6
	Pminutes001.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours001.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    Pseconds001.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub
 ' ********************** END CLOCK CODE   *********************************

'******************************************************
' Beacon Code - DJRobX and Sixtoe from F14 Tomcat
' and then continued VR wall reflections from Rawd's Getaway solution 
' updated with new equations and mods by UnclePaulie
'******************************************************

Dim BeaconPos:BeaconPos = 0

Sub BeaconTimer_Timer
	BeaconPos = BeaconPos + 2.7125
	if BeaconPos >= 360 then BeaconPos = 0
	BeaconRedInt.RotY = BeaconPos + 180
	BeaconFR.RotY = BeaconPos + 180
    BeaconRed.BlendDisableLighting= 0.75 * abs(sin((BeaconPos+90) * 6.28 / 360))

	if VREnv = 5 Then	'b4asti's room
		BeaconREF1.roty=180
		BeaconREF1b.roty=180
		BeaconREF2.opacity = 0
		BeaconREF2b.opacity = 0

		if BeaconPos <= 90 or BeaconPos >= 270 Then
			BeaconREF1.opacity = 0
			BeaconREF1b.opacity = 0
			BeaconREF1.x = -3500
			BeaconREF1.y = -700
			BeaconREF1b.x = -3500
			BeaconREF1b.y = -700
		End If

		if BeaconPos > 90 or BeaconPos < 270 Then

			BeaconREF1.x = BeaconREF1.x + (7000*2.7125/180)
			BeaconREF1b.x = BeaconREF1b.x + (7000*2.7125/180)

			If BeaconPos <= 180 Then
				BeaconREF1.opacity = 5/3*BeaconPos - 150
				BeaconREF1b.opacity = 2/9 * BeaconPos -20
			Elseif BeaconPos <= 270 Then
				BeaconREF1.opacity = -5/9*BeaconPos + 450
				BeaconREF1b.opacity = -2/9*BeaconPos + 60
			Else
				BeaconREF1.opacity = 0
				BeaconREF1b.opacity = 0
			End If
		End If

	Else

		if BeaconPos <= 90 or BeaconPos >= 270 Then
			BeaconREF1.opacity = 0
			BeaconREF1b.opacity = 0
			BeaconREF1.roty=130
			BeaconREF1b.roty=130
			BeaconREF1.x = -1017.5
			BeaconREF1.y = 110
			BeaconREF1b.x = -1017.5
			BeaconREF1b.y = 110

			BeaconREF2.opacity = 0
			BeaconREF2b.opacity = 0
			BeaconREF2.x = -88
			BeaconREF2.y = -2462
			BeaconREF2b.x = -88
			BeaconREF2b.y = -2462

		End If

		if BeaconPos > 90 or BeaconPos < 270 Then

			If BeaconPos <= 200 Then
				BeaconREF1.x = BeaconREF1.x + 44
				BeaconREF1b.x = BeaconREF1b.x + 44
				BeaconREF1.y = BeaconREF1.y - 56
				BeaconREF1b.y = BeaconREF1b.y - 56
				BeaconREF1.opacity = 5/3*BeaconPos - 150
				BeaconREF1b.opacity = 2/9 * BeaconPos -20
			End If

			If BeaconPos > 200 Then
				BeaconREF1.opacity = 0
				BeaconREF1b.opacity = 0
			End If

			If BeaconPos < 160 Then
				BeaconREF2.opacity = 0
				BeaconREF2b.opacity = 0
			End If

			If BeaconPos >= 160 Then
				BeaconREF2.x = BeaconREF2.x + 44
				BeaconREF2b.x = BeaconREF2b.x + 44
				BeaconREF2.y = BeaconREF2.y + 56
				BeaconREF2b.y = BeaconREF2b.y + 56
				BeaconREF2.opacity = -5/9*BeaconPos + 450
				BeaconREF2b.opacity = -2/9*BeaconPos + 60
			End If

		Else
			BeaconREF1.opacity = 0
			BeaconREF1b.opacity = 0
			BeaconREF2.opacity = 0
			BeaconREF2b.opacity = 0
		End If
	End If
End Sub

Sub BeaconSoundTimer_timer()
	BeaconSoundTimer.interval = 2080
	PlaysoundAtVol "Test_Beaconcut3", BeaconRed, BeaconVol*VolumeDial
End Sub

Sub SolRotateBeacons(Enabled)
	If VR_Room = 1 AND Siren = 1 Then
		If Enabled then
			BeaconTimer.Enabled = true
			BeaconSoundTimer.interval = 1
			BeaconSoundTimer.enabled = True
			PlaySoundAt "Relay_Flash_On", BeaconRed
			BeaconFR.visible = true
			if SirenReflect = 1 Then
				BeaconREF1.visible = true
				BeaconREF1b.visible = true
				BeaconREF2.visible = true
				BeaconREF2b.visible = true
			Else
				BeaconREF1.visible = false
				BeaconREF1b.visible = false
				BeaconREF2.visible = false
				BeaconREF2b.visible = false
			End If
		Else
			BeaconTimer.Enabled = False
			BeaconSoundTimer.enabled = False
			BeaconSoundTimer.interval = 1
			Stopsound "Test_Beaconcut3"
			If GameOn = 1 Then PlaySoundAt "Relay_Flash_Off", BeaconRed
			BeaconRed.BlendDisableLighting = .3
			BeaconFR.visible = false
			BeaconREF1.visible = false
			BeaconREF1b.visible = false
			BeaconREF2.visible = false
			BeaconREF2b.visible = false
		End If
	End If

	If DesktopMode = True Then
		If Enabled then
			L_DT_1.state = 2
			L_DT_1.intensity = 7
		Else
			L_DT_1.state = 0
		End If
	End If
		
End Sub

SolRotateBeacons False

'*********************************************************************************************************************************
'	Updates by UnclePaulie version 2.0 include updated VPW code, physics, standalone, rothbauerw targets, flupper bumpers
'	fleep sounds, flipper physics, sling corrections, full VLM for GI, other GI corrections and balancing, hybrid, EBisLit scanned playfield, 
'	F12 menu options, and other updates
'*********************************************************************************************************************************

' version 0.01 - 2.0 updates by UnclePaulie
' v.01	Updated all code to VPW standards
'		Used updated scanned playfield from EBisLit, and Redbone enhanced the image.
' 		Utilized all the objects from fuzzel's table, but had to move all the prims to aling with new playfield
'		Updated the existing table for updated physics materials, sounds, environment lighting, physics values, gates values, bumper values. 
'		Updated to hybrid mode
'		Incorporated flupper bumpers
'		Added fleep sounds
'		Updated desktop backglass and VR backglass to align for each.
'		Implemented F12 menu with colorgrade adjustments
'		Adjusted height of bigger pegs to zsize=0.96.  Added all new rubbers, and made height of 32.
'		Very important to have, keyStagedFlipperL = "" after load wpc.vbs.  Issue with tokens and left flipper.
'		Put all physics in for rubbers, posts, walls.
'		Fine tuned the plunger, created a new autoplunger with a trigger and script.
'		Created an all new slings and sling corrections
'		Redid flipper physics, placement, triggers and tricks
'		Updated the gate physics, and added all new post, metal, wood, rubber physics.
'		Adjusted the side walls and two side rubbers; added a small black wall underneath, as you could see through.
'		Created hybrid mode for desktop, cab, and VR.
'		Adjusted the side rails to be visible for both VR and desktop (not cab_mode), as well as objrotx and z height.
'		This required to adjust the cab sides, front, and coin door elements as well.  Also put in a VR Front Wall Block
'		Added Roth Drop and Standup Targets (used prior dt images but put dL on them.  Used Redbone's yellow Targets)
'		Added double rubbers behind all drop targets.  Many tables have them.
'		Added ramp rolling sounds.
'		Redid the varitarget prim to match real table, pivot point location and length.  Reimported into table.
'		Updated to the Roth varitarget code and modded to match the new varitarget prim.
'		Added a playfield mesh wit bevel holes for saucer, upper left trough hole entrance, varitarget hole, and bank hole.
'		Removed TopHoleMagnet trigger (old trigger, no longer needed).
'		Updated the subway walls and floor shape, material and location to accomodate the playfield holes now.
'		Added new rollover prims and animated them.
'		Added a new saucer wall but used standard cup prim.  
'		Added new sidewood images from redbone.
' v.02	Added all new Flupper Flasher Domes, and implemented a pwm approach to their fading.
'		Added diverter sounds
'		Merged disc spinning code with the OnBallBallCollision routines.  Also added random rubber hit sounds to the prim. Aligned on PF.
'		Adjusted cab pov.
'		Changed sounds for saucer locks, and autoplungers/kicks
'		Added an apron physics wall.
'		Redbone added updated images for drop and stand up targets.  Also updated the spinner disc and apron.
'		Added plywood cutout holes and gi bulb primitives.
'		Cut holes in playfield for 3D inserts, created a flasher text overlay.
'		Added all the 3d Light inserts on the playfield.
'		Added flasher18, tied to flupper dome pwm script.
'		Redid the subway and bank kicker logic to ensure balls will still come out during multiball.
'		Discovered an error in the manual picture where lamp insert 27 and 28 were reversed. (the written numbers were correct)
'		Updated code to control backglass lamps, and reflections on playfield glass if option is selected.
'		Removed "bigdmd" option from prior VR table.  
'		Added showglass and backglass mirror light option and logic for VR Room, and ensured mlights are off at table start.
'		Hooked up the cellar, roof, and bank flashers and lamps
'		Adjusted the bumper and sling forces.  Also the speed of ball coming out of bank.
'		Updated left kicker code, so it'll bounce a little.
'		Increased the depth of two drop targets by Bob's getaway left kicker.
'		Removed discflasher from all collections and tied it to flash with the middle yellow flasher.  Also made a light and adjusted dL on prim with intensity.
'		Updated ramp physics to VPW physics recommendations.
'		Updated leftkicker to be a bit stronger.
'		Changed bulb prims to depth bias of -100... could see through too easily through ramps.
'		Added new gate prim brackets, and one gate wire prim.
'		Added a prim for the leftkickback and animated it in the saucer timer.
' v.03	Added GI code to control all GI, and modified the GI lights and routines for new PWM and stepped control.
'		Added NoTargetBouncer collection to lower sling posts
' v.04	Added AO_Shadows to playfield
'		Added VLM lightmap images for the GI.
'		Cut a couple extra holes on the playfield.
'		Changed GI10 and 13-15 lightmaps to depth bias below the ramp prim.
'		Added an additional GI light (not lightmap) for just below the ramp entrance and right back ramp area to light it up slightly.
'		Changed red ramp material to thickness of 0.35 to get it to stand out just a little more.
'		Added dtShadows and associated drop target action.  Added logic to control dtshadows not going away if a light hit.
' 		Added remaining physic walls to all the plastics.
'		Added GI to plastic tops.  
'		Added blend disable lighting to plastics.
'		Added blend disable lighting to other objects for GI, including pegs, bulbs, targets, rubbers, cutouts, apron, rollovers, metals, etc.
'		Removed images not used.
'		Changed default ball.
'		Updated desktop backglass image.
'		Increased the fricton on the ramps from .15 to .175.
'		Adjusted the end angles of the flippers to match dead flip pinball table.  Readjusted the triggers.
'		Added blue rubber stopper on ramps
'		Added locknut and hex screws, and blue rubber bumpers in top ramp outlane ends.
'		Updated cab pov.
' v.05	Updated flasherflareintensity level down.
'		Added UnclePaulie, Sixtoe, and DarthVito VR room options, in addition to b4asti's room.
'		Cleaned up some VR reflection code
'		Updated start light to animate with lamp88.
'		Updated to latest VR_EZ Grab room by B4asti and distributed by Rawd.
'		Converted all images to webp to save about 50mb on the table file size.
' v.06	Added a VR Topper and Siren Flasher
'		Added a siren flasher to the desktop background
'		Redbone cleaned up the playfield a little.
'		Removed unused timers.
' v.07	Redid the desktop and VR backglass for better images.  Removed scbghigh, high1, and high2 objects from prior tables.
'		Increased physics walls in many areas to avoid balls jumping.
' 		Added DOF for undercab lights
' v.08	Apophis provided actual flipper measurements.  Updated accordingly, changed primitives to match, and redid the flipper triggers.
'		Utilized home version of ROM.. as number of tokens issued is based on game's earnings, and needed a "Non-percentaging version"
'		Added startgamekey code to toggle switch 13.
'		Increased the siren flasher desktop light intensity from 25 to 100. 
'		Increased the VR Siren/flasehr intensity as well, and glowed the backwall some.
'		Added sound to the flasher/siren for VR, as well as an option to adjust volume.
'		Added siren/flasher reflections on the VR room environment walls.  Two different solutions for standard VR environment and for b4asti's.
'		Added option to turn off desktop backglass for users that want to use their own b2s.
'		Turned reflection enabled off on blue stoppers and screws. Potential cause of ball reflections to be off in VR. Also turned off for physics walls.
' v.09	Changed lob to 0; not sure why it was set to 1
'		Changed trough code to NOT pulse sw31, but rather a normal switch on and off.  
'		Updated trough to have a drain and then move to trough.
'		Updated code for the lock ball logic and added two triggers on the lockbar ramp to independantly controll the switches.  
'		Also added a couple invisible gates on that lock up ramp to ensure it doesn't bounce back out.
'v2.0.0	Changed the mass of the spinner ball for better collisions.
'		Moved the varitarget to the core.timer so it has time to do calculations, and changed kspring constant from 2 to 5.
'		Changed the insert light faders to incandescent (from LED None).  Also increased the fade up and down time.  GI also at incandescent but 20/40 fader
'		Released.

' Thanks to apophis, rothbauerw, ebislit, fuzzle, flupper1, b4asti, redbone, and many others from the VPW team for testing and feedback.