' Based on Stern's World Poker Tour / IPD No. 5134 / 2006 / 4 Players
'
' VP9 - Original version by freneticamnesic - 2014
' VPX - Original version by freneticamnesic - 2018
' FSS version by Arconovum - 2018
' VIP physics mod beta by bord - 2021
' VR-Hybrid version by RobbyKingPin, DGRimmReaper, DaRdog, LoadedWeapon, Enthusiast, guus_8005, fidiko, Gravy & Retro27 based on bord's version - 2025/2026
'
' This mod contains the following improvements:
'
' RobbyKingPin:
' - nFozzy/Rothbauerw physics,Fleep sound with a lot of custom sounds from VPW's Rollercoaster Tycoon and VPW Ambient Ball Shadows (Updated v.2.4)
' - Rothbauerw Standup and Drop Target codes (Updated v.2.4)
' - Updated Rubber Dampeners and Slingshot Corrections (Updated v.2.4)
' - Flupper Domes, Flupper Bumpers with custom caps from the VLM library, Flupper flipperbats and 3D Inserts
' - New Trough system that saves the balls
' - More light tweaks to more modern standards based on VPW's settings
' - Inlane speed limiters, code created by Wylte
' - New ball image and scratches based on VPW's tables
' - Updated PWM on flashers GI and Inserts and added new GI lights
' - Big uprez in the playfield using AI
' - Added apron primitive from VPW's The Simpsons Party and Instruction Cards provided by Carny Priest
' - Repositioned a lot of items and lowered the upper playfield and ramps
' - Created new primitives for a missing plastic, a custom topper,a new backpanel, the left VUK and a new scoop for the bumper eject
' - Fully rebuild and reprogrammed the scoop area for the bumper eject, the shooter lane VUK and the left VUK
' - Added a bunch of VR backglasses using some of hauntfreaks and Wildman assets
' - VPW Tweak Menu added
' - Baked the entire VR Mega Room, added and edited a new baked minimal room from VPW's Count-Down and a third room for Mixed Reality
' - Created a new backpanel lights (v.2.4)
' - Baked textures for the sidewalls, custom blades, backpanel, plastics, decals, ramps and playfield shadows (v.2.4)
' - Blended shadows with the GI (v.2.4)
'
' DGrimmReaper:
' - Added VR assets from 24

' DaRdog:
' - Shared assets for the VR Mega Room. Provided feedback and guidance to help bake it all
'
' LoadedWeapon:
' - Created a custom POV to fit on Legends Cabinets
' - Added color corrections on the backpanel and one of the custom sideblades textures
'
' Enthusiast:
' - Tweaked the upward kicker settings on the shooter lane VUK and used AI for performance improvements on the script
'
' guus_8005:
' - Updated the texture for the bumpercaps
'
' fidiko:
' - Added some missing cablights on top of the targets and lighted up the backwall cabs as well. Added playfield light reflections on the sidewalls (Updated v.2,3,1)
' - Made adjustments on target lights and the playfield display settings (Updated v.2.3.1)

' Gravy:
' - Gave support and assisted on helping sorting out issues on refraction and depth bias on the upper playfield (v.2.3.1)
'
' Retro27 (v.2.4):
' - Updated VR Cabinet using assets from DaRdog and improved textures
'
' Special thanks to everybody involved during development and testing :guus_8005, Smaug, Tomate, Primetime5k, Studlygoorite, cl4ve, Dr.Nobody, ZANDYSARCADE, Wizball, BountyBob, bthlonewolf, m.carter78, Cliffy, Sinizin, mason, 
' Burger, kilonova, supergibson, chokeee
'
' Shoutout to:
' - Carny Priest for the instruction cards he gave us!
' - fidiko for helping me by adding the final improvements on the final day before this release!
' - freneticamnesic, Arconovum & bord for their work on previous versions
' 
' If I didn't mention anybody else in this development cycle I am unaware of this and just know that I still think you are awesome and also want to say you rock!
' And basically everybody from VPW I didn't mention just for having me as part of the family, you all absolutely rock!⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
'
Option Explicit : Randomize
SetLocale 1033

'******************************************************
'  ZVAR: Constants and Global Variables
'******************************************************

Const BallSize = 50 	'Ball size must be 50
Const BallMass = 1		'Ball mass must be 1
Const tnob = 4			'Total number of balls on the playfield including captive balls.
Const lob = 0			'Total number of locked balls

Dim tablewidth
tablewidth = Table1.width
Dim tableheight
tableheight = Table1.height
Dim BIP							 'Balls in play
BIP = 0
Dim BIPL							'Ball in plunger lane
BIPL = False
Dim plungerpress

'************************************************************************
Const DebugFlashers = False
Const DebugGI = False

'  Standard definitions
Const cGameName = "wpt_140a"
Const UseSolenoids = 1
Const UseLamps = 1
Const UseGI = 1
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""
	        
Dim PWMflashers

'----- VR Options -----
Dim VRRoomChoice : VRRoomChoice = 1 ' 1 = Mega Room 2 = Minimal Room	
Const VRTest = 0				    ' 1 = Testing VR in Live View, 0 = Do not force VR mode.

Dim ModSol : ModSol = 2
Dim UseVPMModSol : UseVPMModSol = 2

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'----- Desktop Visible Elements -----
Dim VarHidden, UseVPMDMD

If Table1.ShowDT = true and RenderingMode <> 2 Then
	UseVPMDMD = True
	VarHidden = 1
	PinCab_Rails.Visible = 1
	PinCab_TopRail.Visible = 1
Else
	UseVPMDMD = False
	VarHidden = 0
	PinCab_Rails.Visible = 0
	PinCab_TopRail.Visible = 0
	If Table1.ShowFSS = False And LegendsCabMod = 1 Then
		Flasherbloom1.Height = 500
		Flasherbloom2.Height = 500
		Flasherbloom3.Height = 500
		Flasherbloom4.Height = 500
		Flasherbloom5.Height = 500
		Flasherbloom6.Height = 500
		Flasherbloom7.Height = 500
		Flasherbloom8.Height = 500
		Flasherbloom9.Height = 500
	End If
End if

'----- VR Room Auto-Detect -----
Dim VRRoom, VR_Obj, VRMode

If RenderingMode = 2 or Table1.ShowFSS = True or VRTest = 1 Then
	Textbox1.visible = 0
	UseVPMDMD = True
	VarHidden = 0
	VRMode = True
    PinCab_Rails.Visible = 1
	PinCab_TopRail.Visible = 1
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 1 : Next
	If VRRoomChoice = 1 Then
		For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 1 : Next
		Room360.Visible = 0
	End If
	If VRRoomChoice = 2 Then
		For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 1 : Next
		For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 0 : Next
		Room360.Visible = 0
	End If
	If VRRoomChoice = 3 Then
		For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 0 : Next
		Room360.Visible = 1
	End If
Else
	VRMode = False
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 0 : Next
	Room360.Visible = 0
End If

if B2SOn = True then VarHidden = 1

LoadVPM "03060000", "sam.VBS", 3.61

'******************* Options *********************
Dim DivValue : DivValue = 2                 ' Change Value to 4 if LED array does not display properly on your system

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
	UpdatePlunger
	UpdateBallBrightness
	If AmbientBallShadowOn = 1 Then
		BSUpdate
	End If
	RollingUpdate
	DoSTAnim
	DoDTAnim
End Sub

'The CorTimer interval should be 10. It's sole purpose is to update the Cor calculations
CorTimer.Interval = 10
Sub CorTimer_Timer(): Cor.Update: End Sub

Sub UpdatePlunger()
	If plungerpress = 1 then
		If PinCab_Plunger.Y < 50 then
			PinCab_Plunger.Y = PinCab_Plunger.Y + 5
		End If
	Else
		PinCab_Plunger.Y = -50 + (5* Plunger.Position) -20
	End If
End Sub

Dim BumperOneIntensity, BumperTwoIntensity, BumperThreeIntensity
Dim LastBumperOneIntensity, LastBumperTwoIntensity, LastBumperThreeIntensity
LastBumperOneIntensity = -1
LastBumperTwoIntensity = -1
LastBumperThreeIntensity = -1

Sub l62_Animate()
	BumperOneIntensity = l62.GetInPlayIntensity / 100
	If BumperOneIntensity <> LastBumperOneIntensity Then
		FlFadeBumper 1,BumperOneIntensity
		LastBumperOneIntensity = BumperOneIntensity
	End If
End Sub

Sub l70_Animate()
	BumperTwoIntensity = l70.GetInPlayIntensity / 100
	If BumperTwoIntensity <> LastBumperTwoIntensity Then
		FlFadeBumper 2,BumperTwoIntensity
		LastBumperTwoIntensity = BumperTwoIntensity
	End If
End Sub

Sub l78_Animate()
	BumperThreeIntensity = l78.GetInPlayIntensity / 100
	If BumperThreeIntensity <> LastBumperThreeIntensity Then
		FlFadeBumper 3,BumperThreeIntensity
		LastBumperThreeIntensity = BumperThreeIntensity
	End If
End Sub

Sub L67_Animate()
	If L67.GetInPlayIntensity > 10 Then
		TargetLightBulb1.image = "TargetLightBulb1On"
		TargetLightFilament1.image = "TargetLightFilament1On"
		TargetLightSteel1.image = "TargetLightSteel1On"
	Else
		TargetLightBulb1.image = "TargetLightBulb1Off"
		TargetLightFilament1.image = "TargetLightFilament1Off"
		TargetLightSteel1.image = "TargetLightSteel1Off"
	End If
End Sub

Sub L68_Animate()
	If L68.GetInPlayIntensity > 10 Then
		TargetLightBulb2.image = "TargetLightBulb2On"
		TargetLightFilament2.image = "TargetLightFilament2On"
		TargetLightSteel2.image = "TargetLightSteel2On"
	Else
		TargetLightBulb2.image = "TargetLightBulb2Off"
		TargetLightFilament2.image = "TargetLightFilament2Off"
		TargetLightSteel2.image = "TargetLightSteel2Off"
	End If
End Sub

Sub L75_Animate()
	If L75.GetInPlayIntensity > 10 Then
		TargetLightBulb3.image = "TargetLightBulb13On"
		TargetLightFilament3.image = "TargetLightFilament3On"
		TargetLightSteel3.image = "TargetLightSteel3On"
	Else
		TargetLightBulb3.image = "TargetLightBulb3Off"
		TargetLightFilament3.image = "TargetLightFilament3Off"
		TargetLightSteel3.image = "TargetLightSteel3Off"
	End If
End Sub

Sub L76_Animate()
	If L76.GetInPlayIntensity > 10 Then
		TargetLightBulb4.image = "TargetLightBulb4On"
		TargetLightFilament4.image = "TargetLightFilament4On"
		TargetLightSteel4.image = "TargetLightSteel4On"
	Else
		TargetLightBulb4.image = "TargetLightBulb4Off"
		TargetLightFilament4.image = "TargetLightFilament4Off"
		TargetLightSteel4.image = "TargetLightSteel4Off"
	End If
End Sub

'******************************************************
'  ZINI: Table Initialization and Exiting
'******************************************************

'Variables
Dim xx
Dim PlungerIM
Dim WPTBall1, WPTBall2, WPTBall3, WPTBall4, gBOT

Sub Table1_Init
	vpmInit me
	InitVpmFFlipsSAM
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "World Poker Tour (Stern 2006)"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Hidden = 0
		.Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
		On Error Resume Next
		Controller.Run GetPlayerHWnd
		On Error Goto 0
	End With

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1
	vpmNudge.TiltSwitch = -7
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(Bumper1b, Bumper2b, Bumper3b, LeftSlingshot, RightSlingshot)

	'Trough
	Set WPTBall1 = sw18.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set WPTBall2 = sw19.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set WPTBall3 = sw20.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set WPTBall4 = sw21.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(WPTBall1, WPTBall2, WPTBall3, WPTBall4)
	
	Controller.Switch(18) = 1
	Controller.Switch(19) = 1
	Controller.Switch(20) = 1
	Controller.Switch(21) = 1

	Const IMPowerSetting = 50
	Const IMTime = 0.6
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP swplunger, IMPowerSetting, IMTime
		.Switch 23
		.Random 1.5
		.CreateEvents "plungerIM"
	End With

	vpmMapLights AllLamps

	FlFadeBumper 1,1
	FlFadeBumper 2,1
	FlFadeBumper 3,1
End Sub

Sub Table1_Paused() : Controller.Pause = 1 : End Sub
Sub Table1_unPaused() : Controller.Pause = 0 : End Sub
Sub Table1_exit() : Controller.stop : End Sub

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
' 	ZBBR: BALL BRIGHTNESS
'******************************************************

Const BallBrightness =  1      	'Ball brightness - Value between 0 and 1 (0=Dark ... 1=Bright)

' Constants for plunger lane ball darkening.
' You can make a temporary wall in the plunger lane area and use the co-ordinates from the corner control points.
Const PLOffset = 0.5			'Minimum ball brightness scale in plunger lane
Const PLLeft = 1000 			'X position of punger lane left
Const PLRight = 1060 			'X position of punger lane right
Const PLTop = 1225 				'Y position of punger lane top
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

'This code only applies to lightmapped tables. It is here for reference. 
'NOTE: Objects bightness will be affected by the Day/Night slider only if their blenddisablelighting property is less than 1.
'      Lightmapped table primitives have their blenddisablelighting equal to 1, therefore we need this SetRoomBrightness sub 
'      to handle updating their effective ambient brighness.

' Update these arrays if you want to change more materials with room light level
Dim RoomBrightnessMtlArray: RoomBrightnessMtlArray = Array("VLM.Bake.Active","VLM.Bake.Solid") 

Sub SetRoomBrightness(lvl)
	If lvl > 1 Then lvl = 1
	If lvl < 0 Then lvl = 0

	' Lighting level
	Dim v: v=(lvl * 245 + 10)/255

	Dim i: For i = 0 to UBound(RoomBrightnessMtlArray)
		ModulateMaterialBaseColor RoomBrightnessMtlArray(i), i, v
	Next
End Sub

Dim SavedMtlColorArray
SaveMtlColors
Sub SaveMtlColors
	ReDim SavedMtlColorArray(UBound(RoomBrightnessMtlArray))
	Dim i: For i = 0 to UBound(RoomBrightnessMtlArray)
		SaveMaterialBaseColor RoomBrightnessMtlArray(i), i
	Next
End Sub

Sub SaveMaterialBaseColor(name, idx)
	Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	SavedMtlColorArray(idx) = round(base,0)
End Sub


Sub ModulateMaterialBaseColor(name, idx, val)
	Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	Dim red, green, blue, saved_base, new_base
 
	'First get the existing material properties
	GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle

	'Get saved color
	saved_base = SavedMtlColorArray(idx)
    
	'Next extract the r,g,b values from the base color
	red = saved_base And &HFF
	green = (saved_base \ &H100) And &HFF
	blue = (saved_base \ &H10000) And &HFF
	'msgbox red & " " & green & " " & blue

	'Create new color scaled down by 'val', and update the material
	new_base = RGB(red*val, green*val, blue*val)
	UpdateMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, new_base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub

'******************************************************
'	ZKEY: Key Press Handling
'******************************************************

Sub Table1_KeyDown(ByVal Keycode)
	If keycode = LeftFlipperKey Then
		FlipperActivate LeftFlipper, LFPress
		PinCab_Left_Flipper_Button.X = PinCab_Left_Flipper_Button.X + 6
	End If
	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		PinCab_Right_Flipper_Button.X = PinCab_Right_Flipper_Button.X - 6
	End If
	If keycode = PlungerKey Then
		Plunger.Pullback
		SoundPlungerPull
		plungerpress = 1
	End If
	If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft			' Sets the nudge angle and power
	If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight		' ^
	If keycode = CenterTiltKey Then Nudge 0, 1 : SoundNudgeCenter		' ^
	If keycode = StartGameKey Then 
		SoundStartButton
		Pincab_Start_Button.Y = Pincab_Start_Button.Y -5 
	End If
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If
	If keycode = keyFront Then 
		Controller.Switch(15) = True
		soundStartButton() 
		Pincab_Tournament_Button.Z = Pincab_Tournament_Button.Z - 2 'Tournament mode mapped to key 2
	End If
    If vpmKeyDown(keycode) Then Exit Sub 
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = LeftFlipperKey Then
		PinCab_Left_Flipper_Button.X = PinCab_Left_Flipper_Button.X - 6
	End If
	If keycode = RightFlipperKey Then
		PinCab_Right_Flipper_Button.X = PinCab_Right_Flipper_Button.X + 6
	End If
	If Keycode = StartGameKey Then 
		Controller.Switch(16) = 0
		Pincab_Start_Button.Y = Pincab_Start_Button.Y +5 
	End If
	If KeyCode = PlungerKey Then
		Plunger.Fire
		plungerpress = 0
		PinCab_Plunger.Y = -50
		If BIPL = 1 Then
			SoundPlungerReleaseBall()   'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall() 'Plunger release sound when there is no ball in shooter lane
		End If
	End If
	If keycode = keyFront Then 
		Controller.Switch(15) = False
		Pincab_Tournament_Button.Z =Pincab_Tournament_Button.Z + 2 'Tournament mode mapped to key 2
	End if
	If vpmKeyUp(keycode) Then Exit Sub
End Sub

'******************************************************
'	ZSOL: Solenoids & Flashers
'******************************************************

SolCallback(1) = "SolRelease"
SolCallback(2) = "solAutofire"
SolCallback(3) = "VUKRight"
SolCallback(4) = "VUKLeft"
SolCallback(5) = "ResetDropsLD"
SolCallback(6) = "ResetDropsLU"
SolCallback(7) = "ResetDropsM"
SolCallback(8) = "ResetDropsR"
''SolCallback(9) = "SolLeftPop" 'left pop bumper
''SolCallback(10) = "SolRightPop" 'right pop bumper
''SolCallback(11) = "SolBottomPop" 'top pop bumper
SolCallback(12) = "SolJailUp"
SolCallback(13) = "SolULFlipper"
SolCallback(14) = "SolURFlipper"
SolCallback(15) = "SolLFlipper"
SolCallback(16) = "SolRFlipper"
'
SolCallback(17) = "solLSling"
SolCallback(18) = "solRSling"
SolCallback(19) = "SolJailLatch" 'jail latch
SolCallback(20) = "LRPost"
SolCallback(21) = "ScoopOut"
''SolCallback(24) = "vpmSolSound SoundFX(""knocker""),"
SolCallback(32) = "RRDownPost" 'right ramp post

'Solenoid Controlled Flashers
SolModCallback(22) = "SolModFlashPWM22" 'left slingshot flasher
SolModCallback(23) = "SolModFlashPWM23" 'right slingshot flasher
SolModCallback(25) = "SolModFlashPWM25" 'flash left spinner
SolModCallback(26) = "SolModFlashPWM26" 'back panel 1 left
SolModCallback(27) = "SolModFlashPWM27" 'back panel 2
SolModCallback(28) = "SolModFlashPWM28" 'back panel 3
SolModCallback(29) = "SolModFlashPWM29" 'back panel 4
SolModCallback(30) = "SolModFlashPWM30" 'back panel 5 right
SolModCallback(31) = "SolModFlashPWM31" 'right vuk flash

'*******************************************
'	ZFLP: Flippers
'*******************************************

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend	
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper
		Else
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
	Else
		FlipperDeActivate LeftFlipper, LFPress
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
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		FlipperDeActivate RightFlipper, RFPress
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolULFlipper(Enabled)
	If Enabled Then
		FlipperActivate LeftFlipper1, LFPress1		
		LeftFlipper1.RotateToEnd
		If leftflipper1.currentangle < leftflipper1.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper1
		Else
			SoundFlipperUpAttackLeft LeftFlipper1
			RandomSoundFlipperUpLeft LeftFlipper1
		End If
    Else
		FlipperDeActivate LeftFlipper1, LFPress1
		LeftFlipper1.RotateToStart
		If LeftFlipper1.currentangle < LeftFlipper1.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper1
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolURFlipper(Enabled)
	If Enabled Then
		FlipperActivate RightFlipper1, RFPress1
		RightFlipper1.RotateToEnd
		If rightflipper1.currentangle > rightflipper1.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper1
		Else
			SoundFlipperUpAttackRight RightFlipper1
			RandomSoundFlipperUpRight RightFlipper1
		End If
    Else
		FlipperDeActivate RightFlipper1, RFPress1
		RightFlipper1.RotateToStart
		If RightFlipper1.currentangle > RightFlipper1.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper1
		End If
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

Sub LeftFlipper1_Collide(parm)
	LeftFlipperCollide parm
End Sub

Sub RightFlipper1_Collide(parm)
	RightFlipperCollide parm
End Sub

Sub LeftFlipper_Animate
	dim a: a = LeftFlipper.CurrentAngle
	batleftshadow.RotZ = a
	LFLogo.RotZ = a
End Sub

Sub RightFlipper_Animate
	dim a: a = RightFlipper.CurrentAngle
	batrightshadow.RotZ = a
	RFLogo.RotZ = a
End Sub

Sub LeftFlipper1_Animate
	dim a: a = LeftFlipper1.CurrentAngle
	LFLogo1.RotY = a
End Sub

Sub RightFlipper1_Animate
	dim a: a = RightFlipper1.CurrentAngle
	RFLogo1.RotY = a
End Sub

'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************

'******************************************************
'						TROUGH 
'******************************************************

Sub sw18_Hit():Controller.Switch(18) = 1:UpdateTrough:End Sub
Sub sw18_UnHit():Controller.Switch(18) = 0:UpdateTrough:End Sub
Sub sw19_Hit():Controller.Switch(19) = 1:UpdateTrough:End Sub
Sub sw19_UnHit():Controller.Switch(19) = 0:UpdateTrough:End Sub
Sub sw20_Hit():Controller.Switch(20) = 1:UpdateTrough:End Sub
Sub sw20_UnHit():Controller.Switch(20) = 0:UpdateTrough:End Sub
Sub sw21_Hit():Controller.Switch(21) = 1:UpdateTrough:End Sub
Sub sw21_UnHit():Controller.Switch(21) = 0:UpdateTrough:End Sub

 ' Drain hole and kickers
Sub Drain_Hit() 
	RandomSoundDrain drain
	UpdateTrough
	vpmTimer.AddTimer 500, "Drain.kick 60, 20'"
End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw21.BallCntOver = 0 Then sw20.kick 60, 9
	If sw20.BallCntOver = 0 Then sw19.kick 60, 9
	If sw19.BallCntOver = 0 Then sw18.kick 60, 9
	Me.Enabled = 0
End Sub

Sub SolRelease(enabled)
	If enabled Then 
		vpmTimer.PulseSw 15
		sw21.kick 60, 9		
		RandomSoundBallRelease sw21
	End If
End Sub

Sub ScoopOut(enabled)
    If Enabled Then
		sw49.kickZ 295, 40, 11, 100
		Controller.Switch(49) = 0
		PlaySoundAtVol SoundFX("Stern_Scoop", DOFContactors), Scoop, VolumeDial
    End If
End Sub

Sub VUKLeft(enabled)
    If Enabled Then
		LeftVUK.kickZ 0, 30, 0, 180
		Controller.Switch(55) = 0
		PlaySoundAtVol SoundFX("Stern_Scoop", DOFContactors), LeftVUKScoop, VolumeDial
		WireRampOn True
    End If
End Sub

Sub VUKRight(enabled)
    If Enabled Then
		LaneKicker.kickZ 0, 45, 0, 200
		Controller.Switch(3) = 0
		PlaySoundAtVol SoundFX("Stern_Scoop", DOFContactors), RightVUK, VolumeDial
    End If
End Sub

Sub SolJailLatch(Enabled)
    If Enabled Then 'close jail
        JailDiv.IsDropped = false
        JailDiv1.IsDropped = false
        JailDiv2.IsDropped = false
        Controller.Switch(63) = 0
    End If
	PlaySoundAtVol SoundFX("Stern_Bar_Release", DOFContactors), Jail1, VolumeDial
End Sub

Sub SolJailUp(Enabled)
    If Enabled Then
        JailDiv.IsDropped = true
        JailDiv1.IsDropped = true
        JailDiv2.IsDropped = true
        Controller.Switch(63) = 1
    End If
	PlaySoundAtVol SoundFX("Stern_Bar_Release", DOFContactors), Jail1, VolumeDial
End Sub

Sub RRDownPost(Enabled)
    If Enabled Then
        RightPost.IsDropped = False
		RightPost.Collidable = True
    Else
        RightPost.IsDropped = True
		RightPost.Collidable = False
    End If
	UpPostSolenoid RPPrim
End Sub

Sub LRPost(Enabled)
    If Enabled Then
        LeftPost.IsDropped = False
		LeftPost.Collidable = True
    Else
        LeftPost.IsDropped = True
		LeftPost.Collidable = False
    End If
	UpPostSolenoid LeftPostPrim
End Sub

Sub solAutofire(Enabled)
    If Enabled Then
		PlaySoundAtVol SoundFX("Stern_Autolaunch", DOFContactors), Plunger, (VolumeDial / 2)
        PlungerIM.AutoFire
    End If
 End Sub

Dim RStep, Lstep

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 26
End Sub

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 27
End Sub

Sub solLSling(enabled)
	If enabled then
		LeftSling004.Visible = 1
		Left3.RotX = 26
		LStep = 0
		RandomSoundSlingshotLeft Left3
		LeftSlingshot.TimerEnabled = 1
	End If
End Sub

Sub solRSling(enabled)
	If enabled then
		RightSling004.Visible = 1
		Right3.RotX = 26
		RStep = 0
		RandomSoundSlingshotRight Right3
		RightSlingShot.TimerEnabled = 1
	End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing004.Visible = 0:LeftSLing003.Visible = 1:Left3.RotX = 14
        Case 2:LeftSLing003.Visible = 0:LeftSLing002.Visible = 1:Left3.RotX = 2
        Case 3:LeftSLing002.Visible = 0:Left3.RotX = -20:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Right3.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Right3.RotX = 2
        Case 3:RightSLing002.Visible = 0:Right3.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub Bumper1b_Hit() : vpmTimer.PulseSw 30 : RandomSoundBumperTop(Bumper1b) : End Sub
Sub Bumper2b_Hit() : vpmTimer.PulseSw 31 : RandomSoundBumperMiddle(Bumper2b) : End Sub
Sub Bumper3b_Hit() : vpmTimer.PulseSw 32 : RandomSoundBumperBottom(Bumper3b) : End Sub

Sub LaneKicker_Hit
	PlaySoundAtVol SoundFX("safehousehit",DOFContactors),LaneKicker,1
	Controller.Switch(3) = 1
End Sub

Sub LeftVUK_Hit
    SoundSaucerLock
	WireRampOff
	Controller.Switch(55) = 1
End Sub

Sub ScoopTrigger_Hit : Controller.Switch(54) = 1 : playsoundAtVol SoundFX("scoopleft",DOFContactors),ScoopTrigger,1 : End Sub
Sub ScoopTrigger_UnHit : Controller.Switch(54) = 0 : End Sub

Sub ScoopUp_Hit
    PlaySoundAtVol SoundFX("scoopleft",DOFContactors),ScoopUp,1
End Sub

Dim Jaildown, RPDown, LPUp
Dim PrimT

Sub PrimT_Timer
    if JailDiv.IsDropped = true then Jaildown = False else Jaildown = True
    if RightPost.IsDropped = true then RPDown = False else RPDown = True
    if LeftPost.IsDropped = true then LPUp = False else LPUp = True
End Sub

Sub DT_Timer()
    If Jaildown = true and Jail1.z > 160 then Jail1.z = Jail1.z - 3
    If Jaildown = true and Jail2.z > 160 then Jail2.z = Jail2.z - 3
    If RPDown = true and RPPrim.z > 160 then RPPrim.z = RPPrim.z - 3
    If LPUp = true and LeftPostPrim.z < 0 then LeftPostPrim.z = LeftPostPrim.z + 3
    If Jaildown = False and Jail1.z < 210 then Jail1.z = Jail1.z + 3
    If Jaildown = False and Jail2.z < 210 then Jail2.z = Jail2.z + 3
    If RPDown = False and RPPrim.z < 210 then RPPrim.z = RPPrim.z + 3
    If LPUp = False and LeftPostPrim.z > -50 then LeftPostPrim.z = LeftPostPrim.z - 3
    If Jail1.z <= 210 then Jaildown = false
    If Jail2.z <= 210 then Jaildown = false
End Sub

Dim BallInJail : BallInJail=  False
Sub JailDiv_Hit
    If BallInJail = True Then
        Controller.Switch(58) = 0
        sw58.TimerEnabled = 1
    Else
        vpmTimer.PulseSw 57
    End If
	PlaySoundAtVol SoundFX("Stern_Bar_Hit", DOFContactors), Jail2, VolumeDial
End Sub

Sub sw58_Hit
    BallInJail = True
    Controller.Switch(58) = 1
End Sub

Sub sw58_unHit
    BallInJail = False
    Controller.Switch(58 ) = 0
End Sub

Sub sw58_Timer
    sw58.TimerEnabled  = 0
    If BallInJail = True Then Controller.Switch(58) = 1
End Sub

' Inlane switch speedlimit code

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

Sub sw8s_Spin : vpmTimer.PulseSw 8 :  SoundSpinner sw8s : End Sub

Sub sw24_Hit : Controller.Switch(24) = 1 : End Sub
Sub sw24_UnHit : Controller.Switch(24) = 0 : End Sub
Sub sw25_Hit : Controller.Switch(25) = 1 : leftInlaneSpeedLimit : End Sub
Sub sw25_UnHit : Controller.Switch(25) = 0 : End Sub
Sub sw28_Hit : Controller.Switch(28) = 1 : rightInlaneSpeedLimit : End Sub
Sub sw28_UnHit : Controller.Switch(28) = 0 : End Sub
Sub sw29_Hit : Controller.Switch(29) = 1 : End Sub
Sub sw29_UnHit : Controller.Switch(29) = 0 : End Sub

Sub sw33_Hit : DTHit 33 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw34_Hit : DTHit 34 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw35_Hit : DTHit 35 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw36_Hit : DTHit 36 : TargetBouncer Activeball, 1.5 : End Sub

Sub sw37_Hit : DTHit 37 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw38_Hit : DTHit 38 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw39_Hit : DTHit 39 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw40_Hit : DTHit 40 : TargetBouncer Activeball, 1.5 : End Sub

Sub sw9_Hit : Controller.Switch(9) = 1 : End Sub 'right ramp enter
Sub sw9_UnHit: Controller.Switch(9) = 0 : End Sub

Sub sw10_Hit : DTHit 10 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw11_Hit : DTHit 11 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw12_Hit : DTHit 12 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw13_Hit : DTHit 13 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw14_Hit : vpmTimer.PulseSw 14 : End Sub

Sub sw4_Hit : DTHit 4 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw5_Hit : DTHit 5 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw6_Hit : DTHit 6 : TargetBouncer Activeball, 1.5 : End Sub
Sub sw7_Hit : DTHit 7 : TargetBouncer Activeball, 1.5 : End Sub

Sub ResetDropsR(Enabled)
	If Enabled then
		PlaySoundAt SoundFX(DTResetSound,DOFContactors),sw5p
		DTRaise 4
		DTRaise 5
		DTRaise 6
		DTRaise 7
	End if
End Sub

Sub ResetDropsM(Enabled)
	If Enabled then
		PlaySoundAt SoundFX(DTResetSound,DOFContactors),sw11p
		DTRaise 10
		DTRaise 11
		DTRaise 12
		DTRaise 13
	End if
End Sub

Sub ResetDropsLD(Enabled)
	If Enabled then
		PlaySoundAt SoundFX(DTResetSound,DOFContactors),sw34p
		DTRaise 33
		DTRaise 34
		DTRaise 35
		DTRaise 36
	End if
End Sub

Sub ResetDropsLU(Enabled)
	If Enabled then
		PlaySoundAt SoundFX(DTResetSound,DOFContactors),sw38p
		DTRaise 37
		DTRaise 38
		DTRaise 39
		DTRaise 40
	End if
End Sub

Sub sw41_Hit : vpmTimer.PulseSw 41 : End Sub
Sub sw42_Hit : STHit 42 : End Sub
Sub sw45_Hit : STHit 45 : End Sub
Sub sw46_Hit : STHit 46 : End Sub
Sub sw47_Hit : STHit 47 : End Sub
Sub sw48_Hit : STHit 48 : End Sub
Sub sw60_Hit : STHit 60 : End Sub
Sub sw61_Hit : STHit 61 : End Sub
Sub sw62_Hit : STHit 62 : End Sub

Sub sw43s_Spin : vpmTimer.PulseSw 43 : SoundSpinner sw43 : End Sub

Sub sw44_Hit  : Controller.Switch(44) = 1: End Sub 'left orbit made
Sub sw44_UnHit: Controller.Switch(44) = 0: End Sub

Sub sw49_hit
	SoundSaucerLock
	Controller.Switch(49) = 1
End Sub

Sub sw50_Hit : Controller.Switch(50) = 1 : End Sub 'right orbit made
Sub sw50_UnHit: Controller.Switch(50) = 0 : End Sub
Sub sw51_Hit : Controller.Switch(51) = 1 : End Sub 'bssvuk exit
Sub sw51_UnHit: Controller.Switch(51) = 0 : End Sub

Sub sw52_Hit : Controller.Switch(52) = 1 : End Sub 'right ramp made
Sub sw52_UnHit: Controller.Switch(52) = 0 : End Sub
Sub sw53_Hit : Controller.Switch(53) = 1 : End Sub 'middle ramp
Sub sw53_UnHit: Controller.Switch(53) = 0 : End Sub
Sub sw54_Hit : Controller.Switch(54) = 1 : End Sub 'left ramp opto
Sub sw54_UnHit: Controller.Switch(54) = 0 : End Sub

Sub sw56_Hit : Controller.Switch(56) = 1 : End Sub 'leftvuk opto
Sub sw56_UnHit: Controller.Switch(56) = 0 : End Sub

Sub sw59_Hit()
	vpmTimer.PulseSw 59
	WireRampOff
End Sub 'transfer tube

Sub sw23_Hit  : Controller.Switch(23) = 1: BIPL = True : End Sub ' shooter lane
Sub sw23_UnHit: Controller.Switch(23) = 0 : BIPL = False : End Sub

Sub sw8s_Animate()
	sw8p.RotX = -(sw8s.currentangle) + 90
End Sub

Sub sw43s_Animate()
	sw43p.RotX = -(sw43s.currentangle) + 90
End Sub

Sub sw44s_Animate()
	sw44p.RotX = -(sw44s.currentangle) + 90
End Sub

Sub sw50s_Animate()
	sw50p.RotX = -(sw50s.currentangle) + 90
End Sub

Sub sw51s_Animate()
	sw51p.RotX = -(sw51s.currentangle) + 90
End Sub

Sub sw53s_Animate()
	sw53p.RotX = -(sw53s.currentangle) + 90
End Sub

Sub leftramps_Animate()
	leftrampp.RotX = -(leftramps.currentangle) + 0
End Sub

'****************************************************************
'	ZGII: GI
'****************************************************************

Dim gilvl   'General Illumination light state tracked for Dynamic Ball Shadows
gilvl = 1
Dim GIisOn

Set GiCallBack2 = GetRef("GI_PWM")

Sub GI_PWM(nr, aLvl)
	If DebugGI=True Then debug.print "GI_PWM nr="&nr&" aLvl="&aLvl
	Dim ii
	For each ii in GI : ii.state = aLvl : Next
	For each ii in GIBulbs : ii.state = aLvl : Next
	If Not GIisOn And aLvl > 0.1 Then
		GIisOn = True
		Sound_GI_Relay 1, Bumper1b
		Backpanel.image = "BackwallGIOn"
		BackpanelFilament.image = "BackpanelFilamentGIOn"
		BackpanelLights.image = "BackpanelLightsGIOn"
		BackpanelMetal.image = "BackpanelMetalGIOn"
		LeftRamp.image = "LeftRampGIOn"
		LeftRampScoop.image = "LeftRampScoopGIOn"
		LeftVUKScoop.image = "LeftVUKScoopGIOn"
		RampWire.image = "RampWireGIOn"
		RightRamp.image = "RightRampGIOn"
		RightRamp2.image = "RightRamp2GIOn"
		RightRampTransfer.image = "RightRampTransferGIOn"
		RightVUK.image = "VukrampGIOn"
		Scoop.image = "ScoopGIOn"
		Scoopsticker.image = "RightscoopGIOn"
		WPTPlastics1.image = "WPTPlastics1GIOn"
		WPTPlastics2.image = "WPTPlastics2GIOn"
		WPTPlasticsClear.image = "WPTPlasticsClearGIOn"
		WPTUpperPF.image = "WPTUpperPFGIOn"
		If SideBladeMod = 0 Then
			Pincab_Blades.image = "PinCab_BladesGIOn"
		End If
		If SideBladeMod = 1 Then
			Pincab_Blades.image = "PinCab_Blades_Custom1GIOn"
		End If
		If SideBladeMod = 2 Then
			Pincab_Blades.image = "PinCab_Blades_Custom2GIOn"
		End If
		If SideBladeMod = 3 Then
			Pincab_Blades.image = "PinCab_Blades_Custom3GIOn"
		End If
		If SideBladeMod = 4 Then
			Pincab_Blades.image = "PinCab_Blades_Custom4GIOn"
		End If
		If SideBladeMod = 5 Then
			Pincab_Blades.image = "PinCab_Blades_Custom5GIOn"
		End If
	Elseif GIisOn And aLvl < 0.1 Then
		GIisOn = False
		Sound_GI_Relay 0, Bumper1b
		Backpanel.image = "BackwallGIOff"
		BackpanelFilament.image = "BackpanelFilamentGIOff"
		BackpanelLights.image = "BackpanelLightsGIOff"
		BackpanelMetal.image = "BackpanelMetalGIOff"
		LeftRamp.image = "LeftRampGIOff"
		LeftRampScoop.image = "LeftRampScoopGIOff"
		LeftVUKScoop.image = "LeftVUKScoopGIOff"
		RampWire.image = "RampWireGIOff"
		RightRamp.image = "RightRampGIOff"
		RightRamp2.image = "RightRamp2GIOff"
		RightRampTransfer.image = "RightRampTransferGIOff"
		RightVUK.image = "VukrampGIOff"
		Scoop.image = "ScoopGIOff"
		Scoopsticker.image = "RightscoopGIOff"
		WPTPlastics1.image = "WPTPlastics1GIOff"
		WPTPlastics2.image = "WPTPlastics2GIOff"
		WPTPlasticsClear.image = "WPTPlasticsClearGIOff"
		WPTUpperPF.image = "WPTUpperPFGIOff"
		If SideBladeMod = 0 Then
			Pincab_Blades.image = "PinCab_BladesGIOff"
		End If
		If SideBladeMod = 1 Then
			Pincab_Blades.image = "PinCab_Blades_Custom1GIOff"
		End If
		If SideBladeMod = 2 Then
			Pincab_Blades.image = "PinCab_Blades_Custom2GIOff"
		End If
		If SideBladeMod = 3 Then
			Pincab_Blades.image = "PinCab_Blades_Custom3GIOff"
		End If
		If SideBladeMod = 4 Then
			Pincab_Blades.image = "PinCab_Blades_Custom4GIOff"
		End If
		If SideBladeMod = 5 Then
			Pincab_Blades.image = "PinCab_Blades_Custom5GIOff"
		End If
	End If
	PFShadowGIOn.opacity = aLvl * 60
	PFShadowGIOff.opacity = 60 - (aLvl * 60)
	gilvl = alvl
End Sub

Dim LED(69)

LED(0) = Array(D1,D2,D3,D4,D5,D6,D7)
LED(1) = Array(D8,D9,D10,D11,D12,D13,D14)
LED(2) = Array(D15,D16,D17,D18,D19,D20,D21)
LED(3) = Array(D22,D23,D24,D25,D26,D27,D28)
LED(4) = Array(D29,D30,D31,D32,D33,D34,D35)
LED(5) = Array(D36,D37,D38,D39,D40,D41,D42)
LED(6) = Array(D43,D44,D45,D46,D47,D48,D49)
LED(7) = Array(D50,D51,D52,D53,D54,D55,D56)
LED(8) = Array(D57,D58,D59,D60,D61,D62,D63)
LED(9) = Array(D64,D65,D66,D67,D68,D69,D70)
LED(10) = Array(D71,D72,D73,D74,D75,D76,D77)
LED(11) = Array(D78,D79,D80,D81,D82,D83,D84)
LED(12) = Array(D85,D86,D87,D88,D89,D90,D91)
LED(13) = Array(D92,D93,D94,D95,D96,D97,D98)
LED(14) = Array(D99,D100,D101,D102,D103,D104,D105)
LED(15) = Array(D106,D107,D108,D109,D110,D111,D112)
LED(16) = Array(D113,D114,D115,D116,D117,D118,D119)
LED(17) = Array(D120,D121,D122,D123,D124,D125,D126)
LED(18) = Array(D127,D128,D129,D130,D131,D132,D133)
LED(19) = Array(D134,D135,D136,D137,D138,D139,D140)
LED(20) = Array(D141,D142,D143,D144,D145,D146,D147)
LED(21) = Array(D148,D149,D150,D151,D152,D153,D154)
LED(22) = Array(D155,D156,D157,D158,D159,D160,D161)
LED(23) = Array(D162,D163,D164,D165,D166,D167,D168)
LED(24) = Array(D169,D170,D171,D172,D173,D174,D175)
LED(25) = Array(D176,D177,D178,D179,D180,D181,D182)
LED(26) = Array(D183,D184,D185,D186,D187,D188,D189)
LED(27) = Array(D190,D191,D192,D193,D194,D195,D196)
LED(28) = Array(D197,D198,D199,D200,D201,D202,D203)
LED(29) = Array(D204,D205,D206,D207,D208,D209,D210)
LED(30) = Array(D211,D212,D213,D214,D215,D216,D217)
LED(31) = Array(D218,D219,D220,D221,D222,D223,D224)
LED(32) = Array(D225,D226,D227,D228,D229,D230,D231)
LED(33) = Array(D232,D233,D234,D235,D236,D237,D238)
LED(34) = Array(D239,D240,D241,D242,D243,D244,D245)
LED(35) = Array(D246,D247,D248,D249,D250,D251,D252)
LED(36) = Array(D253,D254,D255,D256,D257,D258,D259)
LED(37) = Array(D260,D261,D262,D263,D264,D265,D266)
LED(38) = Array(D267,D268,D269,D270,D271,D272,D273)
LED(39) = Array(D274,D275,D276,D277,D278,D279,D280)
LED(40) = Array(D281,D282,D283,D284,D285,D286,D287)
LED(41) = Array(D288,D289,D290,D291,D292,D293,D294)
LED(42) = Array(D295,D296,D297,D298,D299,D300,D301)
LED(43) = Array(D302,D303,D304,D305,D306,D307,D308)
LED(44) = Array(D309,D310,D311,D312,D313,D314,D315)
LED(45) = Array(D316,D317,D318,D319,D320,D321,D322)
LED(46) = Array(D323,D324,D325,D326,D327,D328,D329)
LED(47) = Array(D330,D331,D332,D333,D334,D335,D336)
LED(48) = Array(D337,D338,D339,D340,D341,D342,D343)
LED(49) = Array(D344,D345,D346,D347,D348,D349,D350)
LED(50) = Array(D351,D352,D353,D354,D355,D356,D357)
LED(51) = Array(D358,D359,D360,D361,D362,D363,D364)
LED(52) = Array(D365,D366,D367,D368,D369,D370,D371)
LED(53) = Array(D372,D373,D374,D375,D376,D377,D378)
LED(54) = Array(D379,D380,D381,D382,D383,D384,D385)
LED(55) = Array(D386,D387,D388,D389,D390,D391,D392)
LED(56) = Array(D393,D394,D395,D396,D397,D398,D399)
LED(57) = Array(D400,D401,D402,D403,D404,D405,D406)
LED(58) = Array(D407,D408,D409,D410,D411,D412,D413)
LED(59) = Array(D414,D415,D416,D417,D418,D419,D420)
LED(60) = Array(D421,D422,D423,D424,D425,D426,D427)
LED(61) = Array(D428,D429,D430,D431,D432,D433,D434)
LED(62) = Array(D435,D436,D437,D438,D439,D440,D441)
LED(63) = Array(D442,D443,D444,D445,D446,D447,D448)
LED(64) = Array(D449,D450,D451,D452,D453,D454,D455)
LED(65) = Array(D456,D457,D458,D459,D460,D461,D462)
LED(66) = Array(D463,D464,D465,D466,D467,D468,D469)
LED(67) = Array(D470,D471,D472,D473,D474,D475,D476)
LED(68) = Array(D477,D478,D479,D480,D481,D482,D483)
LED(69) = Array(D484,D485,D486,D487,D488,D489,D490)

Function CheckLED (num)
    Dim tot,ii,specialcase
    if num = 13 then specialcase = 1
    For ii = (num*5 - 5) to (num*5 - 1 - specialcase)
        tot = tot + CheckLEDColumn(ii)
    Next
    'debug.print Timer & "LED " & num & " = " & tot
    CheckLED = tot
End Function

Function CheckLEDColumn (num)
    Dim tot, obj,i
    i = 1
    For each obj in LED(num)
        tot = tot + obj.State * i
        i = i * 2
    Next
    'debug.print Timer & "LEDColumn " & num & " = " & tot
    CheckLEDColumn = tot
End Function

Sub SetLED14toL
    Dim ii, src, dest, iii, obj
    For each obj in LED(65)
        obj.state = 1
    Next
    For ii = 66 to 69
        For each obj in LED(ii)
            obj.state = 1
            Exit For
        Next
    Next
End Sub

Sub SetLED14toT
    Dim ii, src, dest, iii, obj
    D462.state = 255
    D469.state = 255
    D470.state = 255
    D471.state = 255
    D472.state = 255
    D473.state = 255
    D474.state = 255
    D475.state = 255
    D476.state = 255
    D483.state = 255
    D490.state = 255
End Sub

Sub SetLED14toLED9
    D456.state=D281.state
    D457.state=D282.state
    D458.state=D283.state
    D459.state=D284.state
    D460.state=D285.state
    D461.state=D286.state
    D462.state=D287.state
    D463.state=D288.state
    D464.state=D289.state
    D465.state=D290.state
    D466.state=D291.state
    D467.state=D292.state
    D468.state=D293.state
    D469.state=D294.state
    D470.state=D295.state
    D471.state=D296.state
    D472.state=D297.state
    D473.state=D298.state
    D474.state=D299.state
    D475.state=D300.state
    D476.state=D301.state
    D477.state=D302.state
    D478.state=D303.state
    D479.state=D304.state
    D480.state=D305.state
    D481.state=D306.state
    D482.state=D307.state
    D483.state=D308.state
    D484.state=D309.state
    D485.state=D310.state
    D486.state=D311.state
    D487.state=D312.state
    D488.state=D313.state
    D489.state=D314.state
    D490.state=D315.state
End Sub

Sub ClearLED14
    Dim ii, src, dest, iii, obj
    For ii = 65 to 69
        For each obj in LED(ii)
            obj.state = 0
        Next
    Next
End Sub

Dim Led128:Led128 = True

Sub DisplayTimer_Timer
	Dim ChgLED, ii, num, chg, stat, obj
	If Led128 Then 'If B2S not enabled, use 128 bit LED mode
		On Error Resume Next
		ChgLed = Controller.ChangedLEDs (&HFFFFFFFF, &HFFFFFFFF, &HFFFFFFFF, &HFFFFFFFF) 'displays both rows, dupe leds
		On Error Goto 0
		If Err.Number<>0 Then
			Led128 = False
			Exit Sub
		End If
		If Not IsEmpty (ChgLED) Then

            For ii = 0 To UBound (chgLED)
                'LEDs
                num = chgLED (ii, 0) : chg = chgLED (ii, 1) : stat = chgLED (ii, 2)
                For Each obj In LED (num)
                    If chg And 1 Then obj.State = stat And 1
                    chg = chg \ DivValue : stat = stat \ DivValue
                Next
            Next
        End If
    End If
End Sub

Sub ramptrigger01_hit()
	WireRampOn False
End Sub

Sub ramptrigger02_hit()
	WireRampOff
End Sub

Sub ramptrigger02_unhit()
	WireRampOn True
	RandomSoundRampStop ramptrigger02
End Sub

Sub ramptrigger03_hit()
	If (ActiveBall.VelY > 0) Then 
		RandomSoundRampFlapDown()
	ElseIf (ActiveBall.VelY < 0) Then
		RandomSoundRampFlapUp()
	End If
	WireRampOn False
End Sub

Sub ramptrigger04_hit()
	WireRampOff
End Sub

Sub ramptrigger04_unhit()
	WireRampOn True
End Sub

Dim leftdrop : leftdrop = 0
Sub leftdrop1_unhit : leftdrop = 1 : WireRampOn False : End Sub

Sub leftdrop2_Hit
    If leftdrop = 1 then
        leftdrop = 0
    End If
    WireRampOff
End Sub

Sub leftdrop2_unhit : RandomSoundRampStop leftdrop2 : End Sub

Dim rightdrop : rightdrop = 0

Sub rightdrop1_hit : rightdrop = 1 : WireRampOff : End Sub

Sub rightdrop1_unhit : WireRampOn False : End Sub

Sub rightdrop2_Hit
    If rightdrop = 1 then
        rightdrop = 0
    End If
    WireRampOff
End Sub

Sub rightdrop2_unhit : RandomSoundRampStop rightdrop2 : End Sub

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
Dim objBallShadow(7)

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

'******************************************************
'	ZPHY:  GENERAL ADVICE ON PHYSICS
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
	FlipperTricks LeftFlipper1, LFPress1, LFCount1, LFEndAngle1, LFState1
	FlipperTricks RightFlipper1, RFPress1, RFCount1, RFEndAngle1, RFState1
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	'Dim BOT
	'BOT = GetBalls
	
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

Dim LFPress1, RFPress1, LFCount1, RFCount1
Dim LFState1, RFState1
Dim RFEndAngle1, LFEndAngle1

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
LFState1 = 1
RFState1 = 1
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
LFEndAngle1 = LeftFlipper1.endangle
RFEndAngle1 = RightFlipper1.endangle

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
		'BOT = GetBalls
		
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
	AddSlingsPt 0, 0.00, - 3
	AddSlingsPt 1, 0.30, - 5
	AddSlingsPt 2, 0.40,	-30
	AddSlingsPt 3, 0.60,	30
	AddSlingsPt 4, 0.70,	5
	AddSlingsPt 5, 1.00,	3
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
' 	ZRDT:  DROP TARGETS by Rothbauerw
'******************************************************
' The Stand Up and Drop Target solutions improve the physics for targets to create more realistic behavior. It allows the ball
' to move through the target enabling the ability to score more than one target with a well placed shot.
' It also handles full target animation, switch handling and deflection on hit. For drop targets there is also a slight lift when
' the drop targets raise, bricking, and popping the ball up if it's over the drop target when it raises.
'
' Add a Timers named DTAnim and STAnim to editor to handle drop & standup target animations, or run them off an always-on 10ms timer (GameTimer)
' DTAnim.interval = 10
' DTAnim.enabled = True

' Sub DTAnim_Timer
' 	DoDTAnim
'	DoSTAnim
' End Sub

' For each drop target, we'll use two wall objects for physics calculations and one primitive for visuals and
' animation. We will not use target objects.  Place your drop target primitive the same as you would a VP drop target.
' The primitive should have it's pivot point centered on the x and y axis and at or just below the playfield
' level on the z axis. Orientation needs to be set using Rotz and bending deflection using Rotx. You'll find a hooded
' target mesh in this table's example. It uses the same texture map as the VP drop targets.
'
' For each stand up target we'll use a vp target, a laid back collidable primitive, and one primitive for visuals and animation.
' The visual primitive should should have it's pivot point centered on the x and y axis and the z should be at or just below the playfield.
' The target should animate backwards using transy.
'
' To create visual target primitives that work with the stand up and drop target code, follow the below instructions:
' (Other methods will work as well, but this is easy for even non-blender users to do)
' 1) Open a new blank table. Delete everything off the table in editor.
' 2) Copy and paste the VP target from your table into this blank table.
' 3) Place the target at x = 0, y = 0  (upper left hand corner) with an orientation of 0 (target facing the front of the table)
' 4) Under the file menu, select Export "OBJ Mesh"
' 5) Go to "https://threejs.org/editor/". Here you can modify the exported obj file. When you export, it exports your target and also 
'    the playfield mesh. You need to delete the playfield mesh here. Under the file menu, chose import, and select the obj you exported
'    from VPX. In the right hand panel, find the Playfield object and click on it and delete. Then use the file menu to Export OBJ.
' 6) In VPX, you can add a primitive and use "Import Mesh" to import the exported obj from the previous step. X,Y,Z scale should be 1.
'    The primitive will use the same target texture as the VP target object. 
'
' * Note, each target must have a unique switch number. If they share a same number, add 100 to additional target with that number.
' For example, three targets with switch 32 would use 32, 132, 232 for their switch numbers.
' The 100 and 200 will be removed when setting the switch value for the target.

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
Dim DT4, DT5, DT6, DT7, DT10, DT11, DT12, DT13, DT33, DT34, DT35, DT36, DT37, DT38, DT39, DT40

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

Set DT4 = (new DropTarget)(sw4, sw4a, sw4p, 4, 0, false)
Set DT5 = (new DropTarget)(sw5, sw5a, sw5p, 5, 0, false)
Set DT6 = (new DropTarget)(sw6, sw6a, sw6p, 6, 0, false)
Set DT7 = (new DropTarget)(sw7, sw7a, sw7p, 7, 0, false)
Set DT10 = (new DropTarget)(sw10, sw10a, sw10p, 10, 0, false)
Set DT11 = (new DropTarget)(sw11, sw11a, sw11p, 11, 0, false)
Set DT12 = (new DropTarget)(sw12, sw12a, sw12p, 12, 0, false)
Set DT13 = (new DropTarget)(sw13, sw13a, sw13p, 13, 0, false)
Set DT33 = (new DropTarget)(sw33, sw33a, sw33p, 33, 0, false)
Set DT34 = (new DropTarget)(sw34, sw34a, sw34p, 34, 0, false)
Set DT35 = (new DropTarget)(sw35, sw35a, sw35p, 35, 0, false)
Set DT36 = (new DropTarget)(sw36, sw36a, sw36p, 36, 0, false)
Set DT37 = (new DropTarget)(sw37, sw37a, sw37p, 37, 0, false)
Set DT38 = (new DropTarget)(sw38, sw38a, sw38p, 38, 0, false)
Set DT39 = (new DropTarget)(sw39, sw39a, sw39p, 39, 0, false)
Set DT40 = (new DropTarget)(sw40, sw40a, sw40p, 40, 0, false)

Dim DTArray
DTArray = Array(DT4, DT5, DT6, DT7, DT10, DT11, DT12, DT13, DT33, DT34, DT35, DT36, DT37, DT38, DT39, DT40)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 80 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 50 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "Stern_1Bank_Drop1" 'Drop Target Drop sound
Const DTResetSound = "Stern_3Bank_Reset1" 'Drop Target reset sound

Const DTMass = 0.1 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)
	
	PlayTargetSound
	DTArray(i).animate = DTCheckBrick(ActiveBall,DTArray(i).prim)
	If DTArray(i).animate = 1 Or DTArray(i).animate = 3 Or DTArray(i).animate = 4 Then
		DTBallPhysics ActiveBall, DTArray(i).prim.rotz, DTMass
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
			'Dim BOT
			'BOT = GetBalls
			
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
Dim ST42, ST45, ST46, ST47, ST48, ST60, ST61, ST62

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

Set ST42 = (new StandupTarget)(sw42, sw42p, 42, 0)
Set ST45 = (new StandupTarget)(sw45, sw45p, 45, 0)
Set ST46 = (new StandupTarget)(sw46, sw46p, 46, 0)
Set ST47 = (new StandupTarget)(sw47, sw47p, 47, 0)
Set ST48 = (new StandupTarget)(sw48, sw48p, 48, 0)
Set ST60 = (new StandupTarget)(sw60, sw60p, 60, 0)
Set ST61 = (new StandupTarget)(sw61, sw61p, 61, 0)
Set ST62 = (new StandupTarget)(sw62, sw62p, 62, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST42, ST45, ST46, ST47, ST48, ST60, ST61, ST62)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 		'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit

Const STMass = 0.1				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

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
	'Dim BOT
	'BOT = GetBalls
	
	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 To tnob - 1
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(gBOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(gBOT)
		If BallVel(gBOT(b)) > 1 And gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If
		
		' Ball Drop Sounds
		If gBOT(b).VelZ <  - 1 And gBOT(b).z < 55 And gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz >  - 7 Then
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
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

'///////////////////////-----Ramp Flaps-----///////////////////////
Dim FlapSoundLevel
FlapSoundLevel = 0.8

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
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel, UpPostSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010       'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635	   'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45				   'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.5					   'volume level; range [0, 1]
BumperSoundFactor = 1  						   'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]
UpPostSoundLevel = 0.5 						   'volume level; range [0, 1]

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

'/////////////////////////////  UPPOST SOLENOID  ////////////////////////////

Sub UpPostSolenoid(UpPostswitch)
	PlaySoundAtLevelStatic SoundFX("Stern_UpPost",DOFContactors), UpPostSoundLevel, UpPostswitch
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
	PlaySoundAtLevelStatic SoundFX("Stern_Sling_Left" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Stern_Sling_Right" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Stern_Bumper_Left_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Stern_Bumper_Right_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Stern_Bumper_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
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
	PlaySoundAtLevelStatic SoundFX("Stern_3Bank_Reset" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Stern_1Bank_Drop" & Int(Rnd * 6) + 1), 200, obj
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

'/////////////////////////////  PLASTIC RAMPS FLAPS - SOUNDS  ////////////////////////////
Sub RandomSoundRampFlapUp()
'	debug.print "flap up"
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Up_1"), FlapSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Up_2"), FlapSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Up_3"), FlapSoundLevel
	End Select
End Sub

Sub RandomSoundRampFlapDown()
'	debug.print "flap down"	
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Down_1"), FlapSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Down_2"), FlapSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Down_3"), FlapSoundLevel
	End Select
End Sub


'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

'******************************************************
' 	ZFLD:  FLUPPER DOMES 
'******************************************************
' Based on FlupperDomes2.2

' What you need in your table to use these flashers:
' Open this table and your table both in VPX
' Export all the materials domebasemat, Flashermaterial0 - 20 and import them in your table
' Export all textures (images) starting with the name "dome" and "ronddome" and import them into your table with the same names
' Export all textures (images) starting with the name "flasherbloom" and import them into your table with the same names
' Copy a set of 4 objects flasherbase, flasherlit, flasherlight and flasherflash from layer 7 to your table
' If you duplicate the four objects for a new flasher dome, be sure that they all end with the same number (in the 0-20 range)
' Copy the flasherbloom flashers from layer 10 to your table. you will need to make one per flasher dome that you plan to make
' Select the correct flasherbloom texture for each flasherbloom flasher, per flasher dome
' Copy the script below 

' Place your flasher base primitive where you want the flasher located on your Table
' Then run InitFlasher in the script with the number of your flasher objects and the color of the flasher.  This will align the flasher object, light object, and 
' flasher lit primitive.  It will also assign the appropriate flasher bloom images to the flasher bloom object.
'
' Example: InitFlasher 1, "green"
'
' Color Options: "blue", "green", "red", "purple", "yellow", "white", and "orange"

' You can use the RotateFlasher call to align the Rotz/ObjRotz of the flasher primitives with "handles".  Don't set those values in the editor,
' call the RotateFlasher sub instead (this call will likely crash VP if it's call for the flasher primitives without "handles")
'
' Example: RotateFlasher 1, 180		 'where 1 is the flasher number and 180 is the angle of Z rotation

' For flashing the flasher use in the script: "ObjLevel(1) = 1 : FlasherFlash1_Timer"
' This should also work for flashers with variable flash levels from the rom, just use ObjLevel(1) = xx from the rom (in the range 0-1)
'
' Notes (please read!!):
' - Setting TestFlashers = 1 (below in the ScriptsDirectory) will allow you to see how the flasher objects are aligned (need the targetflasher image imported to your table)
' - The rotation of the primitives with "handles" is done with a script command, not on the primitive itself (see RotateFlasher below)
' - Color of the objects are set in the script, not on the primitive itself
' - Screws are optional to copy and position manually
' - If your table is not named "Table1" then change the name below in the script
' - Every flasher uses its own material (Flashermaterialxx), do not use it for anything else
' - Lighting > Bloom Strength affects how the flashers look, do not set it too high
' - Change RotY and RotX of flasherbase only when having a flasher something other then parallel to the playfield
' - Leave RotX of the flasherflash object to -45; this makes sure that the flash effect is visible in FS and DT
' - If you want to resize a flasher, be sure to resize flasherbase, flasherlit and flasherflash with the same percentage
' - If you think that the flasher effects are too bright, change flasherlightintensity and/or flasherflareintensity below

' Some more notes for users of the v1 flashers and/or JP's fading lights routines:
' - Delete all textures/primitives/script/materials in your table from the v1 flashers and scripts before you start; they don't mix well with v2
' - Remove flupperflash(m) routines if you have them; they do not work with this new script
' - Do not try to mix this v2 script with the JP fading light routine (that is making it too complicated), just use the example script below

'------ Main Dome Code ---------'

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object			 ***
Set TableRef = Table1		   ' *** change this, if your table has another name				   ***
FlasherLightIntensity = 0.1	 ' *** lower this, if the VPX lights are too bright (i.e. 0.1)	   ***
FlasherFlareIntensity = 0.3	 ' *** lower this, if the flares are too bright (i.e. 0.1)		   ***
FlasherBloomIntensity = 0.2	 ' *** lower this, if the blooms are too bright (i.e. 0.1)		   ***	
FlasherOffBrightness = 0.5	  ' *** brightness of the flasher dome when switched off (range 0-2)  ***
' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20), ObjTargetLevel(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height

'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "red"
InitFlasher 2, "red"
InitFlasher 3, "red"
InitFlasher 4, "red"
InitFlasher 5, "red"
InitFlasher 6, "red"
InitFlasher 7, "red"
InitFlasher 8, "red"
InitFlasher 9, "red"

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'   RotateFlasher 1,17
'   RotateFlasher 2,0
   RotateFlasher 3,90
   RotateFlasher 4,90 

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

Sub ModFlashFlasher(nr, aValue)
	objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * aValue
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * aValue
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * aValue
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * aValue
	objlit(nr).BlendDisableLighting = 10 * aValue
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,aValue,RGB(255,255,255),0,0,False,True,0,0,0,0 
End Sub

Sub SolModFlashPWM22(level)				'Flasher Solonoid Name
	ModFlashFlasher 3,level			'Flasher Number assigned in flupper script
End Sub

Sub SolModFlashPWM23(level)				'Flasher Solonoid Name
	ModFlashFlasher 4,level			'Flasher Number assigned in flupper script
End Sub

Sub SolModFlashPWM25(level)				'Flasher Solonoid Name
	ModFlashFlasher 1,level			'Flasher Number assigned in flupper script
End Sub

Sub SolModFlashPWM26(level)				'Flasher Solonoid Name
	ModFlashFlasher 5,level			'Flasher Number assigned in flupper script
End Sub

Sub SolModFlashPWM27(level)				'Flasher Solonoid Name
	ModFlashFlasher 6,level			'Flasher Number assigned in flupper script
End Sub

Sub SolModFlashPWM28(level)				'Flasher Solonoid Name
	ModFlashFlasher 7,level			'Flasher Number assigned in flupper script
End Sub

Sub SolModFlashPWM29(level)				'Flasher Solonoid Name
	ModFlashFlasher 8,level			'Flasher Number assigned in flupper script
End Sub

Sub SolModFlashPWM30(level)				'Flasher Solonoid Name
	ModFlashFlasher 9,level			'Flasher Number assigned in flupper script
End Sub

Sub SolModFlashPWM31(level)				'Flasher Solonoid Name
	ModFlashFlasher 2,level			'Flasher Number assigned in flupper script
End Sub

'******************************************************
'******  END FLUPPER DOMES
'******************************************************

'******************************************************
' 	ZFLB:  FLUPPER BUMPERS
'******************************************************
' Based on FlupperBumpers 0.145 final

' Explanation of how these bumpers work:
' There are 10 elements involved per bumper:
' - the shadow of the bumper ( a vpx flasher object)
' - the bumper skirt (primitive)
' - the bumperbase (primitive)
' - a vpx light which colors everything you can see through the bumpertop
' - the bulb (primitive)
' - another vpx light which lights up everything around the bumper
' - the bumpertop (primitive)
' - the VPX bumper object
' - the bumper screws (primitive)
' - the bulb highlight VPX flasher object
' All elements have a special name with the number of the bumper at the end, this is necessary for the fading routine and the initialisation.
' For the bulb and the bumpertop there is a unique material as well per bumpertop.
' To use these bumpers you have to first copy all 10 elements to your table.
' Also export the textures (images) with names that start with "Flbumper" and "Flhighlight" and materials with names that start with "bumper".
' Make sure that all the ten objects are aligned on center, if possible with the exact same x,y coordinates
' After that copy the script (below); also copy the BumperTimer vpx object to your table
' Every bumper needs to be initialised with the FlInitBumper command, see example below;
' Colors available are red, white, blue, orange, yellow, green, purple and blacklight.
' In a GI subroutine you can then call set the bumperlight intensity with the "FlBumperFadeTarget(nr) = value" command
' where nr is the number of the bumper, value is between 0 (off) and 1 (full on) (so you can also use 0.3 0.4 etc).

' Notes:
' - There is only one color for the disk; you can photoshop it to a different color
' - The bumpertops are angle independent up to a degree; my estimate is -45 to + 45 degrees horizontally, 0 (topview) to 70-80 degrees (frontview)
' - I built in correction for the day-night slider; this might not work perfectly, depending on your table lighting
' - These elements, textures and materials do NOT integrate with any of the lighting routines I have seen in use in many VPX tables
'   (just find the GI handling routine and insert the FlBumperFadeTarget statement)
' - If you want to use VPX native bumperdisks just copy my bumperdisk but make it invisible

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
FlInitBumper 1, "red"
FlInitBumper 2, "red"
FlInitBumper 3, "red"

' ### uncomment the statement below to change the color for all bumpers ###
'   Dim ind
'   For ind = 1 To 5
'	   FlInitBumper ind, "green"
'   Next

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

'******************************************************
'******  END FLUPPER BUMPERS
'******************************************************

'*******************************************
'  VPW TWEAK MENU
'*******************************************

Dim ColorLUT : ColorLUT = 1						      ' Color desaturation LUTs: 1 to 11, where 1 is normal and 11 is black'n'white
Dim Anaglyph : Anaglyph = 0                           ' When set to 1 in the menu this will overwrite the default LUT slot to use the VPW Original 1 on 1
Dim LightLevel : LightLevel = 0.07					  ' Level of room lighting (0 to 1), where 0 is dark and 100 is brightest
Dim VolumeDial : VolumeDial = 0.8           	      ' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	      ' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		      ' Level of ramp rolling volume. Value between 0 and 1
Dim StagedFlippers : StagedFlippers = 0               ' Staged Flippers. 0 = Disabled, 1 = Enabled
Dim AmbientBallShadowOn : AmbientBallShadowon = 1     ' Ambient Ball Shadows. 0 = Disabled, 1 = Enabled 
Dim RenderProbeOpt : RenderProbeOpt = 1 		      ' 0 = No Refraction Probes (best performance), 1 = Full Refraction Probes (best visual)
Dim PlayfieldReflections : PlayfieldReflections = 100 ' Defines the reflection strength of the (dynamic) table elements on the playfield (0-100) / 5 = 20 Max 
Dim ReflOpt : ReflOpt = 1						      ' 0 = Reflections off, 1 = Reflections on
Dim LightReflOpt : LightReflOpt = 1				      ' 0 = Light Reflections off, 1 = Light Reflections on
Dim InsertReflOpt : InsertReflOpt = 1			      ' 0 = Insert Reflections off, 1 = Insert Reflections on
Dim BackglassReflOpt : BackglassReflOpt = 1	          ' 0 = Backglass Reflections off, 1 = Backglass Reflections on
Dim SidewallFlashOpt : SidewallFlashOpt = 1			  ' 0 = Sidewall Flashers off, 1 = Sidewall Flashers on

Dim SideBladeMod, LegendsCabMod, OutlaneDifficulty, InstMod, FlipperMod, GlassMod, DMDDecalMod, DMDDetail, GrillDecalmod, TopperMod, BackglassMod, CabinetMod, LogoPosterMod, Railsmod

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
	'Color Saturation
    ColorLUT = Table1.Option("Color Saturation", 1, 27, 1, 1, 0, _
		Array("Normal", "Desaturated 10%", "Desaturated 20%", "Desaturated 30%", "Desaturated 40%", "Desaturated 50%", "Desaturated 60%", "Desaturated 70%", "Desaturated 80%", "Desaturated 90%", "Black 'n White", _
		"Fleep Natural Dark 1", "Fleep Natural Dark 2", "Fleep Warm Dark", "Fleep Warm Bright", "Fleep Warm Vivid Soft", "Fleep Warm Vivid Hard", "Skitso Natural & Balance", "Skitso Natural High Contrast", _
		"3rdaxis THX Standard", "Callev Brightness & Contrast", "Hauntfreaks Desaturated", "Tomate Washed out", "VPW Original 1 on 1", "Bassgeige", "Blacklight", "B&W Comic Book"))
	if ColorLUT = 1 And Anaglyph = 0 Then Table1.ColorGradeImage = "" '"Normal"
	if ColorLUT = 1 And Anaglyph = 1 Then Table1.ColorGradeImage = "colorgradelut256x16-vpw-original-1-on-1" '"VPW Original 1 on 1"
	if ColorLUT = 2 Then Table1.ColorGradeImage = "colorgradelut256x16-10" '"Desaturated 10%"
	if ColorLUT = 3 Then Table1.ColorGradeImage = "colorgradelut256x16-20" '"Desaturated 20%"
	if ColorLUT = 4 Then Table1.ColorGradeImage = "colorgradelut256x16-30" '"Desaturated 30%"
	if ColorLUT = 5 Then Table1.ColorGradeImage = "colorgradelut256x16-40" '"Desaturated 40%"
	if ColorLUT = 6 Then Table1.ColorGradeImage = "colorgradelut256x16-50" '"Desaturated 50%"
	if ColorLUT = 7 Then Table1.ColorGradeImage = "colorgradelut256x16-60" '"Desaturated 60%"
	if ColorLUT = 8 Then Table1.ColorGradeImage = "colorgradelut256x16-70" '"Desaturated 70%"
	if ColorLUT = 9 Then Table1.ColorGradeImage = "colorgradelut256x16-80" '"Desaturated 80%"
	if ColorLUT = 10 Then Table1.ColorGradeImage = "colorgradelut256x16-90" '"Desaturated 90%"
	if ColorLUT = 11 Then Table1.ColorGradeImage = "colorgradelut256x16-100" '"Black 'n White"
	if ColorLUT = 12 Then Table1.ColorGradeImage = "colorgradelut256x16-fleep-natural-dark-1" '"Fleep Natural Dark 1"
	if ColorLUT = 13 Then Table1.ColorGradeImage = "colorgradelut256x16-fleep-natural-dark-2" '"Fleep Natural Dark 2"
	if ColorLUT = 14 Then Table1.ColorGradeImage = "colorgradelut256x16-fleep-warm-dark" '"Fleep Warm Dark "
	if ColorLUT = 15 Then Table1.ColorGradeImage = "colorgradelut256x16-fleep-warm-bright" '"Fleep Warm Bright"
	if ColorLUT = 16 Then Table1.ColorGradeImage = "colorgradelut256x16-fleep-warm-vivid-soft" '"Fleep Warm Vivid Soft"
	if ColorLUT = 17 Then Table1.ColorGradeImage = "colorgradelut256x16-fleep-warm-vivid-hard" '"Fleep Warm Vivid Hard"
	if ColorLUT = 18 Then Table1.ColorGradeImage = "colorgradelut256x16-skitso-natural-and-balance" '"Skitso Natural & Balance"
	if ColorLUT = 19 Then Table1.ColorGradeImage = "colorgradelut256x16-skitso-natural-high-contrast" '"Skitso Natural High Contrast"
	if ColorLUT = 20 Then Table1.ColorGradeImage = "colorgradelut256x16-3rdaxis-thx-standard" '"3rdaxis THX Standard"
	if ColorLUT = 21 Then Table1.ColorGradeImage = "colorgradelut256x16-callev-brightness-and-contrast" '"Callev Brightness & Contrast"
	if ColorLUT = 22 Then Table1.ColorGradeImage = "colorgradelut256x16-hauntfreaks-desaturated" '"Hauntfreaks Desaturated"
	if ColorLUT = 23 Then Table1.ColorGradeImage = "colorgradelut256x16-tomate-washed-out" '"Tomate Washed out"
	if ColorLUT = 24 Then Table1.ColorGradeImage = "colorgradelut256x16-vpw-original-1-on-1" '"VPW Original 1 on 1"
	if ColorLUT = 25 Then Table1.ColorGradeImage = "colorgradelut256x16-bassgeige" '"Bassgeige"
	if ColorLUT = 26 Then Table1.ColorGradeImage = "colorgradelut256x16-blacklight" '"Blacklight"
	if ColorLUT = 27 Then Table1.ColorGradeImage = "colorgradelut256x16-b-and-w-comic-book" '"B&W Comic Book"
	'Anaglyph
	Anaglyph = Table1.Option("Anaglyph 3D/Headtracking Fix", 0, 1, 1, 0, 0, Array("Off", "On"))
	SetupAnaglyph
    'Sound volumes
    VolumeDial = Table1.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
    BallRollVolume = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.5, 1)
	RampRollVolume = Table1.Option("Ramp Roll Volume", 0, 1, 0.01, 0.5, 1)
	'Ambient BallShadow
	AmbientBallShadowOn = Table1.Option("Ambient Ballshadow", 0, 1, 1, 1, 0, Array("Off", "On"))
	SetupAmbientBallshadow
	'Reflections
	ReflOpt = Table1.Option("Reflections", 0, 1, 1, 1, 0, Array("Off", "On"))
	SetRefl ReflOpt
	'Playfield Reflections
	PlayfieldReflections = Table1.Option("Playfield Reflections", 0, 1, 0.05, 1, 1)
	Table1.PlayfieldReflectionStrength = (PlayfieldReflections * 20)
	'Light Reflections
	LightReflOpt = Table1.Option("Light Reflections", 0, 1, 1, 1, 0, Array("Off", "On"))
	SetLightRefl LightReflOpt
	'Insert Reflections
	InsertReflOpt = Table1.Option("Insert Reflections", 0, 1, 1, 1, 0, Array("Off", "On"))
	SetInsertRefl InsertReflOpt
	'Backglass Reflections
	BackglassReflOpt = Table1.Option("Backglass Reflections", 0, 1, 1, 1, 0, Array("Off", "On"))
	SetBackglassRefl BackglassReflOpt
	'Sidewall Flashers
	SidewallFlashOpt = Table1.Option("Sidewall Flashers", 0, 1, 1, 1, 0, Array("Off", "On"))
	SetSidewallFlash SidewallFlashOpt
	'Refraction Probe Setting
	RenderProbeOpt = Table1.Option("Refraction Probe Setting", 0, 1, 1, 1, 0, Array("No Refraction (best performance)", "Full Refraction (best visuals)"))
	SetRenderProbes RenderProbeOpt
	'Outlane difficulty
	OutlaneDifficulty = Table1.Option("Outlane Difficulty", 0, 2, 1, 1, 0, Array("Simple", "Medium (Default)", "Hard"))
	SetupOutlanes
	'Instruction Cards
	InstMod = Table1.Option("Instruction Cards", 0, 4, 1, 0, 0, Array("Stern Instruction Cards", "White Instruction Cards", "Custom Instruction Cards 1", "Custom Instruction Cards 2", "Custom Instruction Cards 3"))
	SetupInstructionCards
	'Custom Flippers
	FlipperMod = Table1.Option("Flipperbats", 0, 1, 1, 0, 0, Array("Original", "Tiltgraphics"))
	SetupFlippers
	'Sideblades
	SideBladeMod = Table1.Option("Side Blades", 0, 5, 1, 0, 0, Array("Original", "Retrorefurbs 1", "Tiltgraphics 1", "Retrorefurbs 2", "Tiltgraphics 2", "Custom Pinball"))
	SetupBlades
	'LegendsCab
	LegendsCabMod = Table1.Option("Legends Cabinet", 0, 1, 1, 0, 0, Array("Off", "On"))
	SetupLegendsCab
	'Rails Cabinet Mode
	Railsmod = Table1.Option("Rails Cabinet Mode", 0, 3, 1, 0, 0, Array("Off", "Top And Sides", "Top Only", "Sides Only"))
	SetupRailsmod
	'Scratched Glass
	GlassMod = Table1.Option("Scratched Glass", 0, 1, 1, 0, 0, Array("Off", "On"))
	SetupGlass
	'VR Room
    VRRoomChoice = Table1.Option("VR Room (VR-FSS)", 1, 3, 1, 1, 0, Array("Mega Room", "Minimal Room", "Mixed Reality"))
	SetupRoom
	'Speaker/DMD Panel (VR-FSS)
	DMDDecalmod = Table1.Option("Speaker/DMD Panel (VR-FSS)", 0, 3, 1, 0, 0, Array("Original", "Retrorefurbs", "Pinballparts", "CPR Plastic Decal")) 
	SetupDMDDecal
	'Speaker/DMD Details (VR-FSS)
	DMDDetail = Table1.Option("Speaker/DMD Details (VR-FSS)", 0, 1, 1, 1, 0, Array("Low Poly", "High Detailed"))
	SetupDMDDetail
	'Speakergrill (VR-FSS)
	GrillDecalmod = Table1.Option("Speakergrill (VR-FSS)", 0, 1, 1, 0, 0, Array("Original", "Pinballparts"))
	SetupGrillDecal
	'Tournament Topper (VR-FSS)
	TopperMod = Table1.Option("Topper (VR-FSS)", 0, 2, 1, 0, 0, Array("Off", "Tournament Topper", "Laseriffic Topper"))
	SetupTopper
	'Backglass
	BackglassMod = Table1.Option("Backglass (VR-FSS)", 0, 5, 1, 0, 0, Array("Original", "Custom 1", "Custom 2", "Custom 3", "Custom 4", "Custom 5"))
	SetupBackglass
	'Cabinet
	CabinetMod = Table1.Option("Cabinet (VR-FSS)", 0, 1, 1, 0, 0, Array("Original", "Custom Pinball"))
	SetupCabinet
	'VR Minimal Room Logo & Poster (VR-FSS)
	LogoPosterMod = Table1.Option("VR Minimal Room Logo & Poster (VR-FSS)", 0, 3, 1, 1, 0, Array("Off", "Logo and Poster", "Logo", "Poster"))
	SetupLogoPoster
	LightLevel = NightDay/100
	SetRoomBrightness LightLevel
	If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub

Sub SetupAnaglyph
	if ColorLUT = 1 And Anaglyph = 0 Then Table1.ColorGradeImage = "" '"Normal"
	if ColorLUT = 1 And Anaglyph = 1 Then Table1.ColorGradeImage = "colorgradelut256x16-vpw-original-1-on-1" '"VPW Original 1 on 1"
End Sub

Sub SetupAmbientBallshadow
	If AmbientBallShadowOn = 1 Then
		For each xx in GILights : xx.Shadows = 1 : Next
	Else
		For each xx in GILights : xx.Shadows = 0 : Next
	End If
End Sub

Sub SetRefl(Opt)
	Select Case Opt
		Case 0:
			For each xx in Reflections: xx.ReflectionEnabled = False : Next
		Case 1:
			For each xx in Reflections: xx.ReflectionEnabled = True : Next
	End Select
End Sub

Sub SetPlayfieldRefl(Opt)
	Table1.PlayfieldReflectionStrength = (PlayfieldReflections * 20)
End Sub

Sub SetLightRefl(Opt)
	Select Case Opt
		Case 0:
			For each xx in LightReflections: xx.ShowReflectionOnBall = False : Next
		Case 1:
			For each xx in LightReflections: xx.ShowReflectionOnBall = True : Next
	End Select
End Sub

Sub SetInsertRefl(Opt)
	Select Case Opt
		Case 0:
			For each xx in InsertReflections: xx.ShowReflectionOnBall = False : Next
		Case 1:
			For each xx in InsertReflections: xx.ShowReflectionOnBall = True : Next
	End Select
End Sub

Sub SetBackglassRefl(Opt)
	Select Case Opt
		Case 0:
			For each xx in BackglassReflections: xx.ReflectionEnabled = False : Next
			If VRMode = False Then
				For Each VR_Obj in BackglassReflections : VR_Obj.Visible = 0 : Next
			End If
		Case 1:
			For each xx in BackglassReflections: xx.ReflectionEnabled = True : Next
			For Each VR_Obj in BackglassReflections : VR_Obj.Visible = 1 : Next
	End Select
End Sub

Sub SetSidewallFlash(Opt)
	Select Case Opt
		Case 0:
			For each xx in SidewallLights : xx.visible = False : Next
		Case 1:
			For each xx in SidewallLights : xx.visible = True : Next
	End Select
End Sub

Sub SetRenderProbes(Opt)		
	On Error Resume Next
		Select Case Opt
			Case 0:
				WPTUpperPF.RefractionProbe = ""
			Case 1:
				WPTUpperPF.RefractionProbe = "UPF"
		End Select
	On Error Goto 0
End Sub

Sub SetupOutlanes
	If OutlaneDifficulty = 0 Then
		zCol_Rubber_Peg003.Collidable = True
		zCol_Rubber_Peg013.Collidable = False
		zCol_Rubber_Peg014.Collidable = False
		zCol_Rubber_Peg004.Collidable = True
		zCol_Rubber_Peg015.Collidable = False
		zCol_Rubber_Peg016.Collidable = False
		OutlaneRubberLeft1.Visible = True
		OutlaneRubberLeft2.Visible = False
		OutlaneRubberLeft3.Visible = False
		OutlanePegLeft1.Visible = True
		OutlanePegLeft2.Visible = False
		OutlanePegLeft3.Visible = False
		OutlaneRubberRight1.Visible = True
		OutlaneRubberRight2.Visible = False
		OutlaneRubberRight3.Visible = False
		OutlanePegRight1.Visible = True
		OutlanePegRight2.Visible = False
		OutlanePegRight3.Visible = False
	End If
	If OutlaneDifficulty = 1 Then
		zCol_Rubber_Peg003.Collidable = False
		zCol_Rubber_Peg013.Collidable = True
		zCol_Rubber_Peg014.Collidable = False
		zCol_Rubber_Peg004.Collidable = False
		zCol_Rubber_Peg015.Collidable = True
		zCol_Rubber_Peg016.Collidable = False
		OutlaneRubberLeft1.Visible = False
		OutlaneRubberLeft2.Visible = True
		OutlaneRubberLeft3.Visible = False
		OutlanePegLeft1.Visible = False
		OutlanePegLeft2.Visible = True
		OutlanePegLeft3.Visible = False
		OutlaneRubberRight1.Visible = False
		OutlaneRubberRight2.Visible = True
		OutlaneRubberRight3.Visible = False
		OutlanePegRight1.Visible = False
		OutlanePegRight2.Visible = True
		OutlanePegRight3.Visible = False
	End If
	If OutlaneDifficulty = 2 Then
		zCol_Rubber_Peg003.Collidable = False
		zCol_Rubber_Peg013.Collidable = False
		zCol_Rubber_Peg014.Collidable = True
		zCol_Rubber_Peg004.Collidable = False
		zCol_Rubber_Peg015.Collidable = False
		zCol_Rubber_Peg016.Collidable = True
		OutlaneRubberLeft1.Visible = False
		OutlaneRubberLeft2.Visible = False
		OutlaneRubberLeft3.Visible = True
		OutlanePegLeft1.Visible = False
		OutlanePegLeft2.Visible = False
		OutlanePegLeft3.Visible = True
		OutlaneRubberRight1.Visible = False
		OutlaneRubberRight2.Visible = False
		OutlaneRubberRight3.Visible = True
		OutlanePegRight1.Visible = False
		OutlanePegRight2.Visible = False
		OutlanePegRight3.Visible = True
	End If
End Sub

Sub SetupInstructionCards
	If InstMod = 0 Then
		Cards.image = "InstructionCards"
	End If
	If InstMod = 1 Then
		Cards.image = "InstructionCardsWhite"
	End If
	If InstMod = 2 Then
		Cards.image = "InstructionCardsCustom1"
	End If
	If InstMod = 3 Then
		Cards.image = "InstructionCardsCustom2"
	End If
	If InstMod = 4 Then
		Cards.image = "InstructionCardsCustom3"
	End If
End Sub

Sub SetupFlippers
	If FlipperMod = 0 Then
		LFLogo.image = "BatWhiteBlack"
		RFLogo.image = "BatWhiteBlack"
	End If
	If FlipperMod = 1 Then
		LFLogo.image = "BatsWPTCustom1"
		RFLogo.image = "BatsWPTCustom1"
	End If
End Sub

Sub SetupBlades
	If SideBladeMod = 0 Then
		PinCab_Blades.image = "PinCab_BladesGIOn"
	End If
	If SideBladeMod = 1 Then
		PinCab_Blades.image = "PinCab_Blades_Custom1GIOn"
	End if
	If SideBladeMod = 2 Then
		PinCab_Blades.image = "PinCab_Blades_Custom2GIOn"
	End if
	If SideBladeMod = 3 Then
		PinCab_Blades.image = "PinCab_Blades_Custom3GIOn"
	End if
	If SideBladeMod = 4 Then
		PinCab_Blades.image = "PinCab_Blades_Custom4GIOn"
	End if
	If SideBladeMod = 5 Then
		PinCab_Blades.image = "PinCab_Blades_Custom5GIOn"
	End if
End Sub

Sub SetupLegendsCab
	If Table1.ShowDT = False And Table1.ShowFSS = False And RenderingMode <> 2 And LegendsCabMod = 1 Then
		Flasherbloom1.Height = 500
		Flasherbloom2.Height = 500
		Flasherbloom3.Height = 500
		Flasherbloom4.Height = 500
		Flasherbloom5.Height = 500
		Flasherbloom6.Height = 500
		Flasherbloom7.Height = 500
		Flasherbloom8.Height = 500
		Flasherbloom9.Height = 500
	Else
		Flasherbloom1.Height = 250
		Flasherbloom2.Height = 250
		Flasherbloom3.Height = 250
		Flasherbloom4.Height = 250
		Flasherbloom5.Height = 250
		Flasherbloom6.Height = 250
		Flasherbloom7.Height = 250
		Flasherbloom8.Height = 250
		Flasherbloom9.Height = 250
	End If
End Sub

Sub SetupRailsMod
	If Table1.ShowDT = False And Table1.ShowFSS = False And RenderingMode <> 2 Then
		If RailsMod = 1 Then
			PinCab_Rails.Visible = True
			PinCab_TopRail.Visible = True
		End If
		If RailsMod = 2 Then
			PinCab_Rails.Visible = False
			PinCab_TopRail.Visible = True
		End If
		If RailsMod = 3 Then
			PinCab_Rails.Visible = True
			PinCab_TopRail.Visible = False
		End If
		If RailsMod = 0 Then
			PinCab_Rails.Visible = False
			PinCab_TopRail.Visible = False
		End If
	Else
		PinCab_Rails.Visible = True
		PinCab_TopRail.Visible = True
	End If
End Sub

Sub SetupGlass
	If GlassMod = 1 Then
		PinCab_Glass_scratches.Visible = True
	Else
		PinCab_Glass_scratches.Visible = False
	End If
End Sub

Sub SetupRoom
	If RenderingMode = 2 or Table1.ShowFSS = True or VRTest = 1 Then
		VRMode = True
		PinCab_Rails.Visible = 1
		For Each VR_Obj in VRCabinet:VR_Obj.Visible = 1:Next
		If VRRoomChoice = 1 Then
			For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
			For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 1 : Next
			Room360.Visible = 0
			VR_Logo.Visible = False
			VR_Poster.Visible = False
		End If
		If VRRoomChoice = 2 Then
			For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 1 : Next
			For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 0 : Next
			Room360.Visible = 0
			If LogoPosterMod = 0 Then
				VR_Logo.Visible = False
				VR_Poster.Visible = False
			End If
			If LogoPosterMod = 1 Then
				VR_Logo.Visible = True
				VR_Poster.Visible = True
			End If
			If LogoPosterMod = 2 Then
				VR_Logo.Visible = True
				VR_Poster.Visible = False
			End If
			If LogoPosterMod = 3 Then
				VR_Logo.Visible = False
				VR_Poster.Visible = True
			End If
		End If
		If VRRoomChoice = 3 Then
			For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
			For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 0 : Next
			Room360.Visible = 1
			VR_Logo.Visible = False
			VR_Poster.Visible = False
		End If
	End If
End Sub

Sub SetupDMDDecal
	If RenderingMode = 2 or Table1.ShowFSS = True or VRTest = 1 or BackglassReflOpt = 1 Then
		If DMDDecalmod = 0 Then
			Pincab_DMD_Decal.visible = 0
		End If
		If DMDDecalmod = 1 Then
			Pincab_DMD_Decal.image = "Pincab_DMD_Decal_Custom1"
		End If
		If DMDDecalmod = 2 Then
			Pincab_DMD_Decal.image = "Pincab_DMD_Decal_Custom2"
		End If
		If DMDDecalmod = 3 Then
			Pincab_DMD_Decal.image = "Pincab_DMD_Decal"
			If DMDDetail = 1 Then
				Pincab_DMD_Decal_Custom.visible = False
				Pincab_DMD_Decal_Custom_HD.visible = True
				Pincab_DMD_Decal.visible = False
				PinCab_Grills_Right.visible = False
				PinCab_Grills_Left.visible = False
			Else
				Pincab_DMD_Decal_Custom.visible = True
				Pincab_DMD_Decal_Custom_HD.visible = False
				Pincab_DMD_Decal.visible = False
			End If
		Else
			Pincab_DMD_Decal_Custom.visible = False
			Pincab_DMD_Decal_Custom_HD.visible = False
		End If
	End If
End Sub

Sub SetupDMDDetail
	If RenderingMode = 2 or Table1.ShowFSS = True or VRTest = 1 Then
		If DMDDecalmod = 3 Then
			If DMDDetail = 1 Then
				Pincab_DMD_Decal_Custom.visible = False
				Pincab_DMD_Decal_Custom_HD.visible = True
			Else
				Pincab_DMD_Decal_Custom.visible = True
				Pincab_DMD_Decal_Custom_HD.visible = False
			End If
		End If
	End If
End Sub

Sub SetupGrillDecal
	If RenderingMode = 2 or Table1.ShowFSS = True or VRTest = 1 or BackglassReflOpt = 1 Then
		If GrillDecalmod = 0 Then
			PinCab_Grills_Right.image = "Pincab_DMD_Grill_Stern"
			PinCab_Grills_Left.image = "Pincab_DMD_Grill_Stern"
		End If
		If GrillDecalmod = 1 Then
			PinCab_Grills_Right.image = "Pincab_DMD_Grill_Custom"
			PinCab_Grills_Left.image = "Pincab_DMD_Grill_Custom"
		End If
	End If
End Sub

Sub SetupTopper
	If RenderingMode = 2 or Table1.ShowFSS = True or VRTest = 1  or BackglassReflOpt = 1Then
		If TopperMod = 0 Then
			PinCab_TTopper.Visible = False
			PinCab_Custom_Topper.Visible = False
			PinCab_Custom_Topper_On.Visible = False
		End If
		If TopperMod = 1 Then
			PinCab_TTopper.Visible = True
			PinCab_Custom_Topper.Visible = False
			PinCab_Custom_Topper_On.Visible = False
		End If
		If TopperMod = 2 Then
			PinCab_TTopper.Visible = False
			PinCab_Custom_Topper.Visible = True
			PinCab_Custom_Topper_On.Visible = True
		End If
	End If
End Sub

Sub SetupBackglass
	If BackglassMod = 0 Then
		Pincab_Backglass.image = "BackglassImage"
	End If
	If BackglassMod = 1 Then
		Pincab_Backglass.image = "BackglassImageCustom1"
	End If
	If BackglassMod = 2 Then
		Pincab_Backglass.image = "BackglassImageCustom2"
	End If
	If BackglassMod = 3 Then
		Pincab_Backglass.image = "BackglassImageCustom3"
	End If
	If BackglassMod = 4 Then
		Pincab_Backglass.image = "BackglassImageCustom4"
	End If
	If BackglassMod = 5 Then
		Pincab_Backglass.image = "BackglassImageCustom5"
	End If
End Sub

Sub SetupCabinet
	If CabinetMod = 0 Then
		PinCab_Cabinet.image = "Pincab_Cabinet"
		PinCab_Backbox.image = "Pincab_Backbox"
	End If
	If CabinetMod = 1 Then
		PinCab_Cabinet.image = "Pincab_Cabinet_Custom"
		PinCab_Backbox.image = "Pincab_Backbox_Custom"
	End If
End Sub

Sub SetupLogoPoster
	If RenderingMode = 2 or Table1.ShowFSS = True or VRTest = 1 Then
		If VRRoomChoice = 2 Then
			If LogoPosterMod = 0 Then
				VR_Logo.Visible = False
				VR_Poster.Visible = False
			End If
			If LogoPosterMod = 1 Then
				VR_Logo.Visible = True
				VR_Poster.Visible = True
			End If
			If LogoPosterMod = 2 Then
				VR_Logo.Visible = True
				VR_Poster.Visible = False
			End If
			If LogoPosterMod = 3 Then
				VR_Logo.Visible = False
				VR_Poster.Visible = True
			End If
		Else
			VR_Logo.Visible = False
			VR_Poster.Visible = False
		End If
	End If
End Sub
