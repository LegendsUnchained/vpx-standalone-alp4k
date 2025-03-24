' Fathom / IPD No. 829 / August, 1981 / 4 Players

' _____ ____  _____  _     ____  _     
'/    //  _ \/__ __\/ \ /|/  _ \/ \__/|
'|  __\| / \|  / \  | |_||| / \|| |\/||
'| |   | |-||  | |  | | ||| \_/|| |  ||
'\_/   \_/ \|  \_/  \_/ \|\____/\_/  \|


' Version 2.0 by UnclePaulie 2023
'	Table includes Hybrid VR/desktop/cabinet modes, enhanced playfield by Ebislit and Redbone, VPW physics, Fleep sounds, Lampz, 3D inserts, new GI, sling corrections, targets, saucers, playfield mesh, etc.  

'	Complete implementation details and credits found at the bottom of the script.

Option Explicit
Randomize


'*******************************************
' Desktop, Cab, and VR OPTIONS
'*******************************************

' Desktop, Cab, and VR Room are automatically selected.  However if in VR Room mode, you can change the environment with the magna save buttons.

const BallLightness = 1 '0 = dark, 1 = not as dark, 2 = bright (default), 3 = brightest 
const EnviroSoundsOn = 1	'0 = no underwater ocean sounds (default);  1 = underwater ocean sounds
const FlipperColor = 5 '0 = Worn Yellow (default), 1=Red, 2=Blue, 3=Yellow, 4=Worn Red, 5=Worn Blue, 6=Worn White, 7= Worn YellowOrange, 8=Worn Aqua, 9=Worn Light Green, 10=Worn Black, 11=Worn Light Blue, 12=Worn Lime


' LUT selection options
Dim LUTset, DisableLUTSelector, LutToggleSound, LutToggleSoundLevel

LutToggleSound = True
LutToggleSoundLevel = .1
DisableLUTSelector = 0  	' Disables the ability to change LUT option with magna saves in game when set to 1

' DIP Switch options
const SetDIPSwitches = 0	' 0 is the default dip settings for game play. (RECOMMENDED) If you want to set the dips differently, set to 1, and then hit F6 to launch.

' Option for Ocean Environment from Rawd and Circus

const OceanEnv = 0			'0 = normal room VR environment, 1 = Ocean Environment (default)

' *** If using Normal VR Room:

const CustomWalls = 0 'set to 0 for Modern Minimal Walls, floor, and roof, 1 for Sixtoe's original walls and floor
const WallClock = 0	  '1 Shows the clock in the VR minimal rooms only
const topper = 0		 '0 = Off 1= On - Topper visible in VR Room only
const poster = 0		 '1 Shows the flyer posters in the VR room only 
const poster2 = 0		 '1 Shows the flyer posters in the VR room only 

' ****************************************************


'----- Shadow Options -----
Const DynamicBallShadowsOn 	= 0		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn 	= 0		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's
'Ambient (Room light source)
Const AmbientBSFactor 		= 1 	'0 to 1, higher is darker
Const AmbientMovement		= 1		'1 to 4, higher means more movement as the ball moves left and right
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 5		'Offset y position under ball	 (for example 5,5 if the light is in the back left corner)

'Dynamic (Table light sources)
Const DynamicBSFactor 		= 0.5	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

'----- General Sound Options -----
Const VolumeDial = 0.8				' Recommended values should be no greater than 1.
Const BallRollVolume = 0.5 			'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5 			'Level of ramp rolling volume. Value between 0 and 1

'----- Phsyics Mods -----
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled


' ****************************************************

' Bumper brightness scale
const bumperscale = .2			' adjusts the level of blenddisablelighting and intensity on bumpers when lights are not on
const bumperflashlevel = 0.15	' adjusts the level of bumper lights when on and / or flashing


'//////////////---- LUT (Colour Look Up Table) ----//////////////
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
'16 = Skitso New Warmer LUT
'17 = Original LUT


LoadLUT

'LUTset = 16			' Override saved LUT for debug
SetLUT
ShowLUT_Init


' ****************************************************
' standard definitions
' ****************************************************

Dim VR_Room, cab_mode, DesktopMode: DesktopMode = Table1.ShowDT
If RenderingMode = 2 Then VR_Room=1 Else VR_Room=0      'VRRoom set based on RenderingMode in version 10.72
if Not DesktopMode and VR_Room=0 Then cab_mode=1 Else cab_mode=0

Const UseSolenoids 	= 2
Const UseLamps 		= 0
Const UseSync 		= 0
Const HandleMech 	= 0
Const UseGI			= 0
Const cGameName 	= "fathom"		'ROM name
Const ballsize 		= 50
Const ballmass 		= 1
Const UsingROM 		= True			'The UsingROM flag is to indicate code that requires ROM usage. 

'***********************

Const tnob = 3						'Total number of balls  (2 multiball, and 5 captive balls)
Const lob = 0						'Locked balls

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

Dim i, FBall1, Fball2, Fball3, gBOT
Dim BIPL : BIPL = False				'Ball in plunger lane

Dim gion: gion = 0

Dim DT27up, DT28up, DT29up, DT30up, DT31up, DT32up, DT33up, DT34up, DT35up, DT42up, DT43up, DT44up, DT46up, DT47up, DT48up

Dim EnviroSound : EnviroSound = "underwater_ambience"

' Flipper Color

if FlipperColor = 1 Then
	Lflipmesh1.image = "flipperAOred"
	Rflipmesh1.image = "flipperAOred"
	RflipmeshUp.image = "flipperAOred"
Elseif FlipperColor = 2 Then
	Lflipmesh1.image = "flipperAOblue"
	Rflipmesh1.image = "flipperAOblue"
	RflipmeshUp.image = "flipperAOblue"
Elseif FlipperColor = 3 Then
	Lflipmesh1.image = "flipperAOyellow"
	Rflipmesh1.image = "flipperAOyellow"
	RflipmeshUp.image = "flipperAOyellow"
Elseif FlipperColor = 4 Then
	Lflipmesh1.image = "flipperAOredworn-left"
	Rflipmesh1.image = "flipperAOredworn-right"
	RflipmeshUp.image = "flipperAOredworn-right"
Elseif FlipperColor = 5 Then
	Lflipmesh1.image = "flipperAOblueworn-left"
	Rflipmesh1.image = "flipperAOblueworn-right"
	RflipmeshUp.image = "flipperAOblueworn-right"
Elseif FlipperColor = 6 Then
	Lflipmesh1.image = "flipperAOwhiteworn-left"
	Rflipmesh1.image = "flipperAOwhiteworn-right"
	RflipmeshUp.image = "flipperAOwhiteworn-right"
Elseif FlipperColor = 7 Then
	Lflipmesh1.image = "flipperAOyelloworangeworn-left"
	Rflipmesh1.image = "flipperAOyelloworangeworn-right"
	RflipmeshUp.image = "flipperAOyelloworangeworn-right"
Elseif FlipperColor = 8 Then
	Lflipmesh1.image = "flipperAOaquaworn-left"
	Rflipmesh1.image = "flipperAOaquaworn-right"
	RflipmeshUp.image = "flipperAOaquaworn-right"
Elseif FlipperColor = 9 Then
	Lflipmesh1.image = "flipperAOlightgreenworn-left"
	Rflipmesh1.image = "flipperAOlightgreenworn-right"
	RflipmeshUp.image = "flipperAOlightgreenworn-right"
Elseif FlipperColor = 10 Then
	Lflipmesh1.image = "flipperAOblackworn-left"
	Rflipmesh1.image = "flipperAOblackworn-right"
	RflipmeshUp.image = "flipperAOblackworn-right"
Elseif FlipperColor = 11 Then
	Lflipmesh1.image = "flipperAOlightblueworn-left"
	Rflipmesh1.image = "flipperAOlightblueworn-right"
	RflipmeshUp.image = "flipperAOlightblueworn-right"
Elseif FlipperColor = 12 Then
	Lflipmesh1.image = "flipperAOlimeworn-left"
	Rflipmesh1.image = "flipperAOlimeworn-right"
	RflipmeshUp.image = "flipperAOlimeworn-right"
Else
	Lflipmesh1.image = "flipperAOyellowworn-left"
	Rflipmesh1.image = "flipperAOyellowworn-right"
	RflipmeshUp.image = "flipperAOyellowworn-right"
End If

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01130100", "Bally.VBS", 3.21


'**********************************************************************************************************
'Solenoids
'**********************************************************************************************************

SolCallback(1)  = "SolDTTop"				'Top 3 Bank Reset
'SolCallback(1a) = "SolDTRight1"			'3rd Green/Right Inline Drop Target (switched with lamp 47)
SolCallback(2)  = "SolDTLeft"				'Left 6 Bank Reset
'SolCallback(2a) = "SolDTRight2"			'2nd Green/Right Inline Drop Target (switched with lamp 47)
SolCallback(3)  = "SolDTMid"				'Middle 3 Bank Reset
'SolCallback(3a) = "SolDTRight3"				'1st Green/Right Inline Drop Target (switched with lamp 47)
SolCallback(4)  = "SolDTRight"				'Right 3 Bank Reset
SolCallback(6) 	= "SolKnocker"				'Knocker solenoid
SolCallback(7) 	= "SolOutholeKicker"		'Trough to plunger lane
SolCallback(13) = "SolSaucerTop"			'Top Saucer Kick
'SolCallback(13a) = "SolDTTop1"				'3rd Blue/Top Inline Drop Target (switched with lamp 47)
SolCallback(14) = "SolSaucerRight"			'Right Saucer Kick
'SolCallback(14a) = "SolDTTop2"				'2nd Blue Inline Drop Target (switched with lamp 47)
SolCallback(15) = "SolDTTop3"				'1st Blue Inline Drop Target

SolCallback(18) = "BGGI"					'Backglass GI
SolCallback(19) = "PFGI"					'Playfield GI

SolCallback(sllflipper)="SolLFlipper"
SolCallback(slrflipper)="SolRFlipper"


'*****************
'   GI
'*****************

Sub PFGI(Enabled)
    If Enabled Then
		SetLamp 140, 1
		gion = 1

' Set Drop Target Shadows to state when GI is back on.
	if DT27up=1 Then dtsh27.visible = 1
	if DT28up=1 Then dtsh28.visible = 1
	if DT29up=1 Then dtsh29.visible = 1
	if DT30up=1 Then dtsh30.visible = 1
	if DT31up=1 Then dtsh31.visible = 1
	if DT32up=1 Then dtsh32.visible = 1

    Else
		gion = 0
		SetLamp 140, 0

' Set Drop Target Shadows to off when GI is goes off.
	for each xx in ShadowDT
		xx.visible=False
	Next

    End If
End Sub


Sub BGGI(Enabled)
    If Enabled Then
		SetLamp 141, 1
    Else
		SetLamp 141, 0
    End If
End Sub


'*******************************************
'  Timers
'*******************************************

' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate					'update rolling sounds
	DoDTAnim 						'handle drop target animations
	DoSTAnim						'handle stand up target animations
	SpinnerTimer
End Sub


' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows

	If VR_Room=0 Then
		DisplayTimer
	End If

	If VR_Room=1 Then
		VRDisplayTimer
	End If
	LampTimer						'used for Lampz.  No need for separate timer.
	UpdateBallBrightness			'GI for the ball 
End Sub


' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate

	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	Lflipmesh1.RotZ = LeftFlipper.CurrentAngle
	Rflipmesh1.RotZ = RightFlipper.CurrentAngle
	RflipmeshUp.RotZ = RightFlipper1.CurrentAngle
	FlipperRShUp.RotZ = RightFlipper1.currentangle
	Gate1p.rotx = max(Gate1.CurrentAngle,0)
	Gate2p.rotx = max(Gate2.CurrentAngle,0)
	Gate3p.rotx = max(Gate3.CurrentAngle,0)
	Gate4p.rotx = max(Gate4.CurrentAngle,0)

End Sub


'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Fathom (Bally 1981)"&chr(13)&"by UnclePaulie"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
		.Games(cGameName).Settings.Value("sound") = 1 ' Set sound (0=OFF, 1=ON)	
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
		Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

	vpmNudge.TiltSwitch = 15 
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(LeftSlingshot,RightSlingshot, LeftFlipper, RightFlipper, RightFlipper1, Bumper1, Bumper2, Bumper3)

'Ball initializations need for physical trough and ball shadows
	Set FBall1 = BallRelease.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set FBall2 = Slot1.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set FBall3 = Slot2.CreateSizedballWithMass(Ballsize/2,Ballmass)

'Ball initializations
	gBOT = Array(FBall1, Fball2, Fball3)
	Controller.Switch(1) = 1
	Controller.Switch(2) = 1
	Controller.Switch(3) = 1

' Add balls to shadow dictionary
	For Each xx in gBOT
		bsDict.Add xx.ID, bsNone
	Next

'Saucer and Right Flipper initializations
	controller.switch(4) = 0
	Controller.Switch(5) = 0


' Set up VR Backglass
	if VR_Room = 1 Then
		setup_backglass()
		SetBackglass
	End If

' Make drop target shadows invisible
	Dim xx
	for each xx in ShadowDT
		xx.visible=False
	Next

' Drop Target Variable state
	DT27up=1
	DT28up=1
	DT29up=1
	DT30up=1
	DT31up=1
	DT32up=1
	DT33up=1
	DT34up=1
	DT35up=1
	DT42up=1
	DT43up=1
	DT44up=1
	DT46up=1
	DT47up=1
	DT48up=1

	FlasherGI.opacity = 0

' If user wants to set dip switches themselves it will force them to set it via F6.
If SetDIPSwitches = 0 Then
	SetDefaultDips
End If

if EnviroSoundsOn = 1 Then
	PlaySound EnviroSound, -1, 1.0, 0.0, 0.0, 0, 0, 0, 0.0
	PlaySound "intro", 1, 1.0, 0.0, 0.0, 0, 0, 0, 0.0
end If


End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit
	StopSound EnviroSound
	SaveLUT
	Controller.stop
End Sub

'********************************************
' Keys and Plunger code
'********************************************
 
Sub table1_KeyDown(ByVal Keycode)

	If keycode = LeftMagnaSave Then 
		bLutActive = True
	End If

	If keycode = RightMagnaSave Then 
		If bLutActive Then 
			if DisableLUTSelector = 0 then
				If LutToggleSound Then
					Playsound "click", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
				End If
				LUTSet = LUTSet  + 1
				if LutSet > 17 then LUTSet = 0
				SetLUT
				ShowLUT
			end if
		End If
    End If

	If keycode = LeftTiltKey Then Nudge 90, 0.5 ': SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 0.5 ': SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 0.5 ': SoundNudgeCenter


	If KeyCode = PlungerKey Then
		Plunger.PullBack
		SoundPlungerPull()
		TimerVRPlunger.Enabled = True
		TimerVRPlunger1.Enabled = False
		PinCab_Shooter.Y = 2121.572
	End If

	If keycode = LeftFlipperKey Then 
		VRFlipperButtonLeft.X = VRFlipperButtonLeft.X + 8
		FlipperActivate LeftFlipper, LFPress
	End If

	If keycode = RightFlipperKey Then
		VRFlipperButtonRight.X = VRFlipperButtonRight.X - 8
		FlipperActivate RightFlipper, RFPress
		Controller.Switch(7) = 1
	End If

	If keycode = StartGameKey Then
		StartButton.y = 1943.978 - 5
		SoundStartButton
	End If

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then 'Use this for ROM based games
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

    If vpmKeyDown(keycode) Then Exit Sub

End Sub


dim plungerspeed


Sub table1_KeyUp(ByVal Keycode)

    If keycode = LeftMagnaSave Then 
		bLutActive = False
	End If

	If keycode = PlungerKey Then 
		plunger.firespeed = RndInt(95,110) 	' Generate random value variance of +/- 1 from 110
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		TimerVRPlunger.Enabled = False
		TimerVRPlunger1.Enabled = True
		PinCab_Shooter.Y = 2121.572
	End If

	If keycode = LeftFlipperKey Then 
		VRFlipperButtonLeft.X = VRFlipperButtonLeft.X - 8
		FlipperDeActivate LeftFlipper, LFPress
	End If

	If keycode = RightFlipperKey Then
		VRFlipperButtonRight.X = VRFlipperButtonRight.X + 8
		FlipperDeActivate RightFlipper, RFPress
		Controller.Switch(7) = 0
	End If

	If keycode = StartGameKey Then
		StartButton.y = 1943.978
	End If

	if vpmKeyUp(keycode) Then Exit Sub

End Sub


'*******************************************
'  Flippers
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
		RightFlipper1.rotatetoend
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		RightFlipper1.RotateToStart
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
	RightFlipperCollide parm
End Sub


'******************************************************
'			TROUGH BASED ON FOZZY and Rothbauerw
'******************************************************

Sub BallRelease_Hit : Controller.Switch(1) = 1 : UpdateTrough : End Sub
Sub BallRelease_UnHit : Controller.Switch(1) = 0 : UpdateTrough : End Sub
Sub Slot1_Hit :	Controller.Switch(2) = 1 : UpdateTrough : End Sub
Sub Slot1_UnHit : Controller.Switch(2) = 0 : UpdateTrough : End Sub
Sub Slot2_Hit :	Controller.Switch(3) = 1 : UpdateTrough : End Sub
Sub Slot2_UnHit : Controller.Switch(3) = 0 : UpdateTrough : End Sub


Sub UpdateTrough
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer
	If BallRelease.BallCntOver = 0 Then Slot1.kick 60, 8
	If Slot1.BallCntOver = 0 Then Slot2.kick 60, 8
	If Slot2.BallCntOver = 0 Then Drain.kick 60, 12
	Me.Enabled = 0
End Sub


'********************************************
' Drain hole and saucer kickers
'********************************************

Dim RNDKickValue1, RNDKickAngle1	'Random Values for saucer kick and angles

Sub SolOutholeKicker(enabled)
	If enabled Then 
		BallRelease.kick 60, 12
		RandomSoundBallRelease BallRelease
	End If
End Sub

Sub Drain_Hit()
	UpdateTrough
	RandomSoundDrain Drain
End Sub

Sub sw4_Hit
	SoundSaucerLock
	Controller.Switch(4) = 1
End Sub

Sub SolSaucerTop(Enabled)
	If Enabled Then
		If Controller.Lamp(47) = 0 Then
			RNDKickAngle1 = RndInt(315,317) 	' Generate random value variance of +/- 1 from 316
			RNDKickValue1 = 40
			sw4.kick RNDKickAngle1, RNDKickValue1
			sw4Step = 0
			sw4.timerenabled = 1
		Else
			SolDTTop1(1)
		End If
	End If
End Sub

Dim SW4Step
Sub SW4_Timer()
	Select Case SW4Step
		Case 0:	pKickerArm.Rotx = 4
		Case 2: pKickerArm.Rotx = 8
		Case 3: pKickerArm.Rotx = 8
		Case 4:	pKickerArm.Rotx = 4
		Case 5: pKickerArm.Rotx = 0:sw4.timerenabled = 0:sw4Step = -1:
	End Select
	sw4Step = sw4Step + 1
End Sub

Sub sw4_Unhit
	SoundSaucerKick 1, sw4
	controller.switch(4) = 0
End Sub

Sub sw5_Hit
	SoundSaucerLock
	Controller.Switch(5) = 1
End Sub

Sub SolSaucerRight(Enabled)
	If Enabled Then
		If Controller.Lamp(47) = 0 Then
			RNDKickAngle1 = RndInt(178,182) 	' Generate random value variance of +/- 2 from 180
			RNDKickValue1 = RndInt(26,28)    	' Generate random value variance of +/- 1 from 27
			sw5.kick RNDKickAngle1, RNDKickValue1
			sw5Step = 0
			sw5.timerenabled = 1
		Else
			SolDTTop2(1)
		End If
	End If
End Sub

Dim SW5Step
Sub SW5_Timer()
	Select Case SW5Step
		Case 0:	pKickerArm2.Rotx = 4
		Case 2: pKickerArm2.Rotx = 8
		Case 3: pKickerArm2.Rotx = 8
		Case 4:	pKickerArm2.Rotx = 4
		Case 5: pKickerArm2.Rotx = 0:sw5.timerenabled = 0:sw5Step = -1:
	End Select
	sw5Step = sw5Step + 1
End Sub

Sub sw5_Unhit
	SoundSaucerKick 1, sw5
	controller.switch(5) = 0
End Sub


'*******************************************
' Rollovers
'*******************************************

Sub sw12_Hit   : Controller.Switch(12) = 1 : End Sub 
Sub sw12_Unhit : Controller.Switch(12) = 0 : End Sub
Sub sw13_Hit   : Controller.Switch(13) = 1 : End Sub 
Sub sw13_Unhit : Controller.Switch(13) = 0 : End Sub
Sub sw14_Hit   : Controller.Switch(14) = 1 : End Sub 
Sub sw14_Unhit : Controller.Switch(14) = 0 : End Sub
Sub sw21_Hit   : Controller.Switch(21) = 1 : End Sub 
Sub sw21_Unhit : Controller.Switch(21) = 0 : End Sub
Sub sw22_Hit   : Controller.Switch(22) = 1 : End Sub 
Sub sw22_Unhit : Controller.Switch(22) = 0 : End Sub
Sub sw23_Hit   : Controller.Switch(23) = 1 : End Sub 
Sub sw23_Unhit : Controller.Switch(23) = 0 : End Sub
Sub sw24_Hit   : Controller.Switch(24) = 1 : End Sub 
Sub sw24_Unhit : Controller.Switch(24) = 0 : End Sub



'*******************************************
' Ball in Plunger Lane
'*******************************************

Sub Trigger1_Hit
	BIPL = True
End Sub

Sub Trigger1_UnHit
	BIPL = False
End Sub


'*******************************************
'Star Triggers
'*******************************************

Sub sw201_Hit:vpmTimer.PulseSw 20:me.timerenabled = 0:AnimateStar star201, sw201, 1:End Sub
Sub sw201_UnHit:me.timerinterval = 7:me.timerenabled = 1:End Sub
Sub sw201_timer:AnimateStar star201, sw201, 0:End Sub

Sub sw202_Hit:vpmTimer.PulseSw 20:me.timerenabled = 0:AnimateStar star202, sw202, 1:End Sub
Sub sw202_UnHit:me.timerinterval = 7:me.timerenabled = 1:End Sub
Sub sw202_timer:AnimateStar star202, sw202, 0:End Sub

Sub sw203_Hit:vpmTimer.PulseSw 20:me.timerenabled = 0:AnimateStar star203, sw203, 1:End Sub
Sub sw203_UnHit:me.timerinterval = 7:me.timerenabled = 1:End Sub
Sub sw203_timer:AnimateStar star203, sw203, 0:End Sub

Sub sw25_Hit:vpmTimer.PulseSw 25:me.timerenabled = 0:AnimateStar star25, sw25, 1:End Sub
Sub sw25_UnHit:me.timerinterval = 7:me.timerenabled = 1:End Sub
Sub sw25_timer:AnimateStar star25, sw25, 0:End Sub

Sub sw26_Hit:vpmTimer.PulseSw 26:me.timerenabled = 0:AnimateStar star26, sw26, 1:End Sub
Sub sw26_UnHit:me.timerinterval = 7:me.timerenabled = 1:End Sub
Sub sw26_timer:AnimateStar star26, sw26, 0:End Sub


Sub AnimateStar(prim, sw, action) ' Action = 1 - to drop, 0 to raise
	If action = 1 Then
		prim.transz = -4
	Else
		prim.transz = prim.transz + 0.5
		if prim.transz = -2 and Rnd() < 0.05 Then
			sw.timerenabled = 0
		Elseif prim.transz >= 0 Then
			prim.transz = 0
			sw.timerenabled = 0
		End If
	End If
End Sub


'*******************************************
' Spinners
'*******************************************

Sub Spinner_Spin()
	vpmtimer.PulseSw 18
	SoundSpinner Spinner
End Sub


'***********Rotate Spinner

Sub SpinnerTimer
	SpinnerPrim.Rotx = Spinner.CurrentAngle
	SpinnerRod.TransX = sin( (Spinner.CurrentAngle+180) * (2*PI/360)) * 12
	SpinnerRod.TransZ = sin( (Spinner.CurrentAngle- 90) * (2*PI/360)) * 10
End Sub


'*******************************************
' Bumpers
'*******************************************

Sub Bumper1_Hit 'Left Top
	RandomSoundBumperTop Bumper1
	vpmTimer.PulseSw 40
End Sub

Sub Bumper2_Hit 'Right Top
	RandomSoundBumperTop Bumper2
	vpmTimer.PulseSw 38
End Sub

Sub Bumper3_Hit 'Bottom
	RandomSoundBumperBottom Bumper3
	vpmTimer.PulseSw 39
End Sub


'********************************************
'  Targets
'********************************************

'*******************************************
' Round Targets
'*******************************************

Sub sw17_Hit
	STHit 17
End Sub

Sub sw17o_Hit
	TargetBouncer Activeball, 1
End Sub


'********************************************
' Drop Target Hits
'********************************************

Sub sw27_Hit
	DTHit 27
	DT27up=0
End Sub

Sub sw28_Hit
	DTHit 28
	DT28up=0
End Sub

Sub sw29_Hit
	DTHit 29
	DT29up=0
End Sub

Sub sw30_Hit
	DTHit 30
	DT30up=0
End Sub

Sub sw31_Hit
	DTHit 31
	DT31up=0
End Sub

Sub sw32_Hit
	DTHit 32
	DT32up=0
End Sub

Sub sw33_Hit
	DTHit 33
	DT33up=0
End Sub

Sub sw34_Hit
	DTHit 34
	DT34up=0
End Sub

Sub sw35_Hit
	DTHit 35
	DT35up=0
End Sub

Sub sw42_Hit
	DTHit 42
	DT42up=0
End Sub

Sub sw43_Hit
	DTHit 43
	DT43up=0
End Sub

Sub sw44_Hit
	DTHit 44
	DT44up=0
End Sub

Sub sw46_Hit
	DTHit 46
	DT46up=0
End Sub

Sub sw47_Hit
	DTHit 47
	DT47up=0
End Sub

Sub sw48_Hit
	DTHit 48
	DT48up=0
End Sub


'********************************************
' Drop Target Solenoid Controls
'********************************************

Sub SolDTTop (enabled)
	If enabled then
		If Controller.Lamp(47) = 0 Then
			RandomSoundDropTargetReset sw43p
			DTRaise 42	
			DTRaise 43
			DTRaise 44
			DT42up=1
			DT43up=1
			DT44up=1
		Else
			SolDTRight1(1)
		End If
	End if
End Sub

Sub SolDTLeft (enabled)
	If enabled then
		If Controller.Lamp(47) = 0 Then
			RandomSoundDropTargetReset sw29p
			DTRaise 27
			DTRaise 28
			DTRaise 29
			DTRaise 30
			DTRaise 31
			DTRaise 32
			DT27up=1
			DT28up=1
			DT29up=1
			DT30up=1
			DT31up=1
			DT32up=1
			dtsh27.visible=True
			dtsh28.visible=True
			dtsh29.visible=True
			dtsh30.visible=True
			dtsh31.visible=True
			dtsh32.visible=True
		Else
			SolDTRight2(1)
		End If
	End if
End Sub

Sub SolDTMid (enabled)
	If enabled then
		If Controller.Lamp(47) = 0 Then
			RandomSoundDropTargetReset sw34p
			DTRaise 33
			DTRaise 34
			DTRaise 35
			DT33up=1
			DT34up=1
			DT35up=1
		Else
			SolDTRight3(1)
		End If
	End if
End Sub

Sub SolDTRight (enabled)
	If enabled then
		RandomSoundDropTargetReset sw47p
		DTRaise 46
		DTRaise 47
		DTRaise 48
		DT46up=1
		DT47up=1
		DT48up=1
	End if
End Sub

Sub SolDTTop1 (enabled)
	If enabled then
		if DT44up = 1 Then DTDrop 44
		DT44up=0
	End if
End Sub

Sub SolDTTop2 (enabled)
	If enabled then
		if DT43up = 1 Then 	DTDrop 43
		DT43up=0
	End if
End Sub

Sub SolDTTop3 (enabled)
	If enabled then
		if DT42up = 1 Then DTDrop 42
		DT42up=0
	End if
End Sub

Sub SolDTRight1 (enabled)
	If enabled then
		if DT48up = 1 Then DTDrop 48
		DT48up=0
	End if
End Sub

Sub SolDTRight2 (enabled)
	If enabled then
		if DT47up = 1 Then DTDrop 47
		DT47up=0
	End if
End Sub

Sub SolDTRight3 (enabled)
	If enabled then
		if DT46up = 1 Then DTDrop 46
		DT46up=0
	End if
End Sub


'*******************************************
' Leaf Standup Sensors
'*******************************************

Sub RubberBand006_Hit: vpmTimer.PulseSw 19:End Sub
Sub RubberBand010_Hit: vpmTimer.PulseSw 19:End Sub


'*******************************************
'  Knocker
'*******************************************

Sub SolKnocker(Enabled)
	If enabled Then 
		KnockerSolenoid
	End If
End Sub


'*******************************************
' Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'*******************************************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 36
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
	vpmTimer.PulseSw 37
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

'*************************************
' Bally Fathom DIP switches
'*************************************
'Code done by Inkochnito
Sub editDips
  if SetDIPSwitches = 1 Then
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 700,400,"Fathom - DIP switches"
		.AddChk 7,10,120,Array("Match feature",&H08000000)'dip 28
		.AddChk 130,10,120,Array("Game over attract",&H20000000)'dip 30
		.AddChk 260,10,120,Array("Credits display",&H04000000)'dip 27

		.AddFrame 2,30,190,"Maximum credits",&H03000000,Array("10 credits",0,"15 credits",&H01000000,"25 credits",&H02000000,"40 credits",&H03000000)'dip 25&26

		.AddFrame 2,106,190,"Locked ball adjustment",&H00000020,Array("eject ball at game end",0,"ball remains locked",&H00000020)'dip 6
		.AddFrame 2,152,190,"Bonus special lit at maximum",&H00000040,Array("blue and green bonus",0,"blue or green bonus",&H00000040)'dip 7
		.AddFrame 2,198,190,"Extra ball lite is lit for",&H00000080,Array("6 seconds",0,"10 seconds",&H00000080)'dip 8
		.AddFrame 2,244,190,"A-B-C special adjust",32768,Array("replay",0,"alternating points or replay",32768)'dip 16
		.AddFrame 205,30,190,"Balls per game",&HC0000000,Array("2 balls",&HC0000000,"3 balls",0,"4 balls",&H80000000,"5 balls",&H40000000)'dip 31&32
		.AddFrame 205,106,190,"Number of replays per game",&H10000000,Array("Only 1 replay per player per game",0,"All replays earned will be collected",&H10000000)'dip 29
		.AddFrame 205,152,190,"Drop target memory",&H00200000,Array("not in memory",0,"kept in memory",&H00200000)'dip 22
		.AddFrame 205,198,190,"55,000 bonus lites",&H00400000,Array("not in memory",0,"kept in memory",&H00400000)'dip 23
		.AddFrame 205,244,190,"A-B-C lites",&H00800000,Array("not in memory",0,"kept in memory",&H00800000)'dip 24
		.AddLabel 50,300,350,20,"Set selftest position 16,17,18 and 19 to 03 for the best gameplay."
		.AddLabel 50,320,300,20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
  End If
End Sub
Set vpmShowDips = GetRef("editDips")


Sub SetDefaultDips

' Number of Balls / game
	SetDip &H40000000,0		'Number of balls.  0,0 = 3, 0,1 = 3, 1,0 = 5, 1,1 = 5
	SetDip &H80000000,0

' All other DIPs
	SetDip &H08000000,1		'1=Match feature
	SetDip &H20000000,1		'1=Game Over Attract
	SetDip &H04000000,1		'1=Credits display
	SetDip &H00000020,0		'0=eject ball at game end
	SetDip &H00000040,1		'Memory for Bonus and Specials; 0=blue AND green; 1= blue OR green
	SetDip &H00000080,1		'Memory for extra ball lite; 0=6 seconds, 1=10 seconds
	SetDip 32768,1			'Memory for ABC Special adjust; 0=replay, 1= alternating points or replay
	SetDip &H10000000,1		'Number of replays per game; 0=1 per game, 1= all replays collected
	SetDip &H00200000,1		'Memory for drop targets; 0=not in memory, 1=in memory
	SetDip &H00400000,1		'Memory for 55,000 bonus lites; 0=not in memory; 1=in memory
	SetDip &H00800000,1		'Memory for ABC lites; 0=not in memory; 1=in memory

End Sub

Sub SetDip(pos,value)
	dim mask : mask = 255
	if value >= 0.5 then value = 1 else value = 0
	If pos >= 0 and pos <= 255 Then
		pos = pos And &H000000FF
		mask = mask And (Not pos)
		Controller.Dip(0) = Controller.Dip(0) And mask
		Controller.Dip(0) = Controller.Dip(0) + pos*value
	Elseif pos >= 256 and pos <= 65535 Then
		pos = ((pos And &H0000FF00)\&H00000100) And 255
		mask = mask And (Not pos)
		Controller.Dip(1) = Controller.Dip(1) And mask
		Controller.Dip(1) = Controller.Dip(1) + pos*value		
	Elseif pos >= 65536 and pos <= 16777215 Then
		pos = ((pos And &H00FF0000)\&H00010000) And 255
		mask = mask And (Not pos)
		Controller.Dip(2) = Controller.Dip(2) And mask
		Controller.Dip(2) = Controller.Dip(2) + pos*value
	Elseif pos >= 16777216 Then
		pos = ((pos And &HFF000000)\&H01000000) And 255
		mask = mask And (Not pos)
		Controller.Dip(3) = Controller.Dip(3) And mask
		Controller.Dip(3) = Controller.Dip(3) + pos*value
	End If
End Sub



'*******************************************
'  Block shadows in plunger lane
'*******************************************

dim notrtlane: notrtlane = 1
sub rtlanesense_Hit() : notrtlane = 0 : end Sub
sub rtlanesense_UnHit() : notrtlane = 1 : end Sub

'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function


' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
dim objrtx1(3), objrtx2(3)
dim objBallShadow(3)
Dim OnPF(3)
Dim BallShadowA
BallShadowA = Array (BallShadowA0, BallShadowA1, BallShadowA2)
Dim DSSources(30), numberofsources

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
	If onPlayfield Then
		OnPF(ballNum) = True
		bsRampOff gBOT(ballNum).ID
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
	'   Dim gBOT: gBOT=getballs	'Uncomment if you're destroying balls - Not recommended! #SaveTheBalls
	
	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 To tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next
	
	If UBound(gBOT) < lob Then Exit Sub 'No balls in play, exit
	
	'The Magic happens now
	For s = lob To UBound(gBOT)
		' *** Normal "ambient light" ball shadow
		'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your Elseif segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else (under 20)
		
		'Primitive shadow on playfield, flasher shadow in ramps
		If AmbientBallShadowOn = 1 Then
			'** Above the playfield
			If gBOT(s).Z > 30 Then
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update
				bsRampType = getBsRampType(gBOT(s).id)
				'   debug.print bsRampType
				
				If Not bsRampType = bsRamp Then 'Primitive visible on PF
					objBallShadow(s).visible = 1
					objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
					objBallShadow(s).Y = gBOT(s).Y + offsetY
					objBallShadow(s).size_x = 5 * ((gBOT(s).Z + BallSize) / 80) 'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z + BallSize) / 80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor * (30 / (gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else 'Opaque, no primitive below
					objBallShadow(s).visible = 0
				End If
				
				If bsRampType = bsRampClear Or bsRampType = bsRamp Then 'Flasher visible on opaque ramp
					BallShadowA(s).visible = 1
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + offsetY + BallSize / 10
					BallShadowA(s).height = gBOT(s).z - BallSize / 4 + s / 1000 'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
					If bsRampType = bsRampClear Then BallShadowA(s).Opacity = 50 * AmbientBSFactor
				ElseIf bsRampType = bsWire Or bsRampType = bsNone Then 'Turn it off on wires or falling out of a ramp
					BallShadowA(s).visible = 0
				End If
				
				'** On pf, primitive only
			ElseIf gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then
				If Not OnPF(s) Then BallOnPlayfieldNow True, s
				objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
				objBallShadow(s).Y = gBOT(s).Y + offsetY
				'   objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf
				
				'** Under pf, flasher shadow only
			Else
				If OnPF(s) Then BallOnPlayfieldNow False, s
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 1
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height = gBOT(s).z - BallSize / 4 + s / 1000
			End If
			
			'Flasher shadow everywhere
		ElseIf AmbientBallShadowOn = 2 Then
			If gBOT(s).Z > 30 Then 'In a ramp
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY + BallSize / 10
				BallShadowA(s).height = gBOT(s).z - BallSize / 4 + s / 1000 'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			ElseIf gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then 'On pf
				BallShadowA(s).visible = 1
				BallShadowA(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height = 1.04 + s / 1000
			Else 'Under pf
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height = gBOT(s).z - BallSize / 4 + s / 1000
			End If
		End If
		
		' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).X < 850 AND notrtlane = 1 Then 'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff
				dist2 = falloff
				For iii = 0 To numberofsources - 1 'Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
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
					objrtx1(s).X = gBOT(s).X
					objrtx1(s).Y = gBOT(s).Y
					'   objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1 * DynamicBSFactor ^ 3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1
					objrtx2(s).X = gBOT(s).X
					objrtx2(s).Y = gBOT(s).Y + offsetY
					'   objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), gBOT(s).X, gBOT(s).Y) + 90
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
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

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
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************


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
		x.AddPt "Polarity", 2, 0.33, - 2.7
		x.AddPt "Polarity", 3, 0.37, - 2.7
		x.AddPt "Polarity", 4, 0.41, - 2.7
		x.AddPt "Polarity", 5, 0.45, - 2.7
		x.AddPt "Polarity", 6, 0.576, - 2.7
		x.AddPt "Polarity", 7, 0.66, - 1.8
		x.AddPt "Polarity", 8, 0.743, - 0.5
		x.AddPt "Polarity", 9, 0.81, - 0.5
		x.AddPt "Polarity", 10, 0.88, 0

		x.AddPt "Velocity", 0, 0, 1
		x.AddPt "Velocity", 1, 0.16, 1.06
		x.AddPt "Velocity", 2, 0.41, 1.05
		x.AddPt "Velocity", 3, 0.53, 1 '0.982
		x.AddPt "Velocity", 4, 0.702, 0.968
		x.AddPt "Velocity", 5, 0.95,  0.968
		x.AddPt "Velocity", 6, 1.03, 0.945
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
Const EOSTnew = 1 'EM's to late 80's
'Const EOSTnew = 0.8 '90's and later
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
   Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
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
'****  PHYSICS DAMPENERS
'******************************************************
'
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


'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
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


'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************


'******************************************************
'****  DROP TARGETS by Rothbauerw
'******************************************************


'******************************************************
'  DROP TARGETS INITIALIZATION
'******************************************************

'Define a variable for each drop target

Dim DT27, DT28, DT29, DT30, DT31, DT32, DT33, DT34, DT35, DT42, DT43, DT44, DT46, DT47, DT48


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


Set DT27 = (new DropTarget)(sw27, sw27a, sw27p, 27, 0, false)
Set DT28 = (new DropTarget)(sw28, sw28a, sw28p, 28, 0, false)
Set DT29 = (new DropTarget)(sw29, sw29a, sw29p, 29, 0, false)
Set DT30 = (new DropTarget)(sw30, sw30a, sw30p, 30, 0, false)
Set DT31 = (new DropTarget)(sw31, sw31a, sw31p, 31, 0, false)
Set DT32 = (new DropTarget)(sw32, sw32a, sw32p, 32, 0, false)
Set DT33 = (new DropTarget)(sw33, sw33a, sw33p, 33, 0, false)
Set DT34 = (new DropTarget)(sw34, sw34a, sw34p, 34, 0, false)
Set DT35 = (new DropTarget)(sw35, sw35a, sw35p, 35, 0, false)
Set DT42 = (new DropTarget)(sw42, sw42a, sw42p, 42, 0, false)
Set DT43 = (new DropTarget)(sw43, sw43a, sw43p, 43, 0, false)
Set DT44 = (new DropTarget)(sw44, sw44a, sw44p, 44, 0, false)
Set DT46 = (new DropTarget)(sw46, sw46a, sw46p, 46, 0, false)
Set DT47 = (new DropTarget)(sw47, sw47a, sw47p, 47, 0, false)
Set DT48 = (new DropTarget)(sw48, sw48a, sw48p, 48, 0, false)


Dim DTArray
DTArray = Array(DT27, DT28, DT29, DT30, DT31, DT32, DT33, DT34, DT35, DT42, DT43, DT44, DT46, DT47, DT48)


'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 2 '8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const Sound = "" 'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

' Initial Drop Target Shadows - Avoids a light DT hit and shadows go off when not strong enough hit to drop the target.

Dim DTShadow(6)

DTShadowInit 1
DTShadowInit 2
DTShadowInit 3
DTShadowInit 4
DTShadowInit 5
DTShadowInit 6

' Initializes the drop targets for shadow logic below
Sub DTShadowInit(dtnbr)

	if dtnbr = 1 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 27)
	elseif dtnbr = 2 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 28)
	elseif dtnbr = 3 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 29)
	elseif dtnbr = 4 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 30)
	elseif dtnbr = 5 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 31)
	elseif dtnbr = 6 Then
		Set DTShadow(dtnbr) = Eval("dtsh" & 32)
	End If
End Sub

Sub DTHit(switch)
	Dim i, swmod

	i = DTArrayID(switch)
	If switch = 27 Then
		swmod = 1
	Elseif switch = 28 then
		swmod = 2
	Elseif switch = 29 then
		swmod = 3
	Elseif switch = 30 then
		swmod = 4
	Elseif switch = 31 then
		swmod = 5
	Elseif switch = 32 then
		swmod = 6
	Elseif switch = 33 then
		swmod = 7
	Elseif switch = 34 then
		swmod = 8
	Elseif switch = 35 then
		swmod = 9
	Elseif switch = 42 then
		swmod = 10
	Elseif switch = 43 then
		swmod = 11
	Elseif switch = 44 then
		swmod = 12
	Elseif switch = 46 then
		swmod = 13
	Elseif switch = 47 then
		swmod = 14
	Elseif switch = 48 then
		swmod = 15
	End If

	PlayTargetSound
	DTArray(i).animate =  DTCheckBrick(Activeball,DTArray(i).prim)
	If DTArray(i).animate = 1 or DTArray(i).animate = 3 or DTArray(i).animate = 4 Then
		DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass

'	Controls Drop Shadow for a direct hit only
		if swmod=1 or swmod=2 or swmod=3 or swmod=4 or swmod=5 or swmod=6 then
			DTShadow(swmod).visible = 0
		End If
	End If
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch)

	DTArray(i).animate = -1
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
	For i = 0 to uBound(DTArray) 
		If DTArray(i).sw = switch Then DTArrayID = i:Exit Function 
	Next
End Function


sub DTBallPhysics(aBall, angle, mass)
	dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))

	calc1 = cor.BallVel(aball.id) * cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
	calc3 = cor.BallVel(aball.id) * sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

	aBall.velx = calc1 * cos(rangle) + calc2
	aBall.vely = calc1 * sin(rangle) + calc3
End Sub


'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim) 
	dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	Xintersect = (aBall.y - dtprim.y - tan(bangle) * aball.x + tan(rangle2) * dtprim.x) / (tan(rangle2) - tan(bangle))
	Yintersect = tan(rangle2) * Xintersect + (dtprim.y - tan(rangle2) * dtprim.x)

	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
	paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

	If perpvel > 0 and  perpvelafter <= 0 Then
		If DTEnableBrick = 1 and  perpvel > DTBrickVel and DTBrickVel <> 0 and cdist < 8 Then
			DTCheckBrick = 3
		Else
			DTCheckBrick = 1
		End If
	ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
		DTCheckBrick = 4
	Else 
		DTCheckBrick = 0
	End If
End Function


Sub DoDTAnim()
	Dim i
	For i=0 to Ubound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch, animate)
	dim transz, switchid
	Dim animtime, rangle

	switchid = switch

	Dim ind
	ind = DTArrayID(switchid)

	rangle = prim.rotz * PI / 180

	DTAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then 
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If (animate = 1 or animate = 4) and animtime < DTDropDelay Then
		primary.collidable = 0
	If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		DTAnimate = animate
		Exit Function
		elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		animate = 2
		SoundDropTargetDrop prim
	End If

	if animate = 2 Then
		
		transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
		if prim.transz > -DTDropUnits  Then
			prim.transz = transz
		end if

		prim.rotx = DTMaxBend * cos(rangle)/2
		prim.roty = DTMaxBend * sin(rangle)/2

		if prim.transz <= -DTDropUnits Then 
			prim.transz = -DTDropUnits
			secondary.collidable = 0
			DTArray(ind).isDropped = true 'Mark target as dropped
			if UsingROM then controller.Switch(Switchid) = 1
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		end If 
	End If

	If animate = 3 and animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
	elseif animate = 3 and animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If

	if animate = -1 Then
		transz = (1 - (animtime)/DTDropUpSpeed) *  DTDropUnits * -1

		If prim.transz = -DTDropUnits Then
			Dim b

			For b = 0 to UBound(gBOT)
				If InRotRect(gBOT(b).x,gBOT(b).y,prim.x, prim.y, prim.rotz, -25,-10,25,-10,25,25,-25,25) and gBOT(b).z < prim.z+DTDropUnits+25 Then
					gBOT(b).velz = 20
				End If
			Next
		End If

		if prim.transz < 0 Then
			prim.transz = transz
		elseif transz > 0 then
			prim.transz = transz
		end if

		if prim.transz > DTDropUpUnits then 
			DTAnimate = -2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = gametime
		end if
		primary.collidable = 0
		secondary.collidable = 1
		DTArray(ind).isDropped = false 'Mark target as not dropped
		if UsingROM then controller.Switch(Switchid) = 0
	End If

	if animate = -2 and animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay)/DTDropSpeed *  DTDropUnits * -1 + DTDropUpUnits 
		if prim.transz < 0 then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0

			primary.collidable = 1
			secondary.collidable = 0
		end If 
	End If
End Function


'******************************************************
'  DROP TARGET
'  SUPPORTING FUNCTIONS 
'******************************************************


' Used for drop targets
'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
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

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
    Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
    Dim rotxy
    rotxy = RotPoint(ax,ay,angle)
    rax = rotxy(0)+px : ray = rotxy(1)+py
    rotxy = RotPoint(bx,by,angle)
    rbx = rotxy(0)+px : rby = rotxy(1)+py
    rotxy = RotPoint(cx,cy,angle)
    rcx = rotxy(0)+px : rcy = rotxy(1)+py
    rotxy = RotPoint(dx,dy,angle)
    rdx = rotxy(0)+px : rdy = rotxy(1)+py

    InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function


'******************************************************
'****  END DROP TARGETS
'******************************************************

'******************************************************
'		STAND-UP TARGET INITIALIZATION
'******************************************************

'Define a variable for each stand-up target

Dim ST17

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


Set ST17 = (new StandupTarget)(sw17, psw17,17, 0)



Dim STArray
STArray = Array(ST17)


'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 				'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit

Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)

	PlayTargetSound
	STArray(i).animate =  STCheckHit(Activeball,STArray(i).primary)

	If STArray(i).animate <> 0 Then
		DTBallPhysics Activeball, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
		If STArray(i).sw = switch Then STArrayID = i:Exit Function 
	Next
End Function

'Check if target is hit on it's face
Function STCheckHit(aBall, target) 
	dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180	
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
	paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

	If perpvel > 0 and  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
		STCheckHit = 1
	Else 
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i=0 to Ubound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime

	STAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then 
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If animate = 1 Then
		primary.collidable = 0
		prim.transy = -STMaxOffset
		if UsingROM then 
			vpmTimer.PulseSw switch
		else
			STAction switch
		end if
		STAnimate = 2
		Exit Function
	elseif animate = 2 Then
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
		Case 11:
			Addscore 1000
			Flash1 True								'Demo of the flasher
			vpmTimer.AddTimer 150,"Flash1 False'"	'Disable the flash after short time, just like a ROM would do
		Case 12:
			Addscore 1000
			Flash2 True								'Demo of the flasher
			vpmTimer.AddTimer 150,"Flash2 False'"	'Disable the flash after short time, just like a ROM would do
		Case 13:
			Addscore 1000
			Flash3 True								'Demo of the flasher
			vpmTimer.AddTimer 150,"Flash3 False'"	'Disable the flash after short time, just like a ROM would do
	End Select
End Sub

'******************************************************
'		END STAND-UP TARGETS
'******************************************************



'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
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
	Dim b

	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		'If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
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
		ElseIf BallVel(gBOT(b)) > 1 AND gBOT(b).z > 70 Then
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


'		' "Static" Ball Shadows
		If AmbientBallShadowOn = 0 Then
			BallShadowA(b).visible = 1
			BallShadowA(b).X = gBOT(b).X + offsetX
			If gBOT(b).Z > 30 Then
				BallShadowA(b).height=gBOT(b).z - BallSize/4 + b/1000	'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				BallShadowA(b).Y = gBOT(b).Y + offsetY + BallSize/10
			Else
				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 1.04 + b/1000
				BallShadowA(b).Y = gBOT(b).Y + offsetY
			End If
		End If



	Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************


'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************


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
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
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
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
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
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
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

Function RndNum(min, max)
	RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

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
'****  FLEEP MECHANICAL SOUNDS
'******************************************************



'******************************************************
'******  FLUPPER BUMPERS
'******************************************************

' prepare some global vars to dim/brighten objects when using day-night slider
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
' NOTE:  created a special color only for barracora table to work with the top, and colors that matched real table.

FlInitBumper 1, "blue"
FlInitBumper 2, "blue"
FlInitBumper 3, "blue"

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
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255)
			FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255)
			FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			MaterialColor "bumpertopmat" & nr, RGB(0,96,213)
			FlBumperHighlight(nr).color = RGB(255,16,8)
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
		Case "blue"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(38 - 24 * Z,130 - 98 * Z,255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = bumperscale*(20 + 500 * Z / (0.5 + DNA30))
			FlBumperTop(nr).BlendDisableLighting = bumperscale*(3 * DayNightAdjust + 50 * Z)
			FlBumperBulb(nr).BlendDisableLighting = bumperscale*(12 * DayNightAdjust + 5000 * (0.03 * Z + 0.97 * Z ^ 3))
			Flbumperbiglight(nr).intensity = bumperscale*(25 * Z / (1 + DNA45))
			FlBumperHighlight(nr).opacity = bumperscale*(10000 * (Z ^ 3) / (0.5 + DNA90))
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


'******************************************************
'******  END FLUPPER BUMPERS
'******************************************************




'******************************************************
'****  LAMPZ by nFozzy
'******************************************************
' 

Sub SetLamp(aNr, aOn)
	Lampz.state(aNr) = abs(aOn)
End Sub

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              	' Setup lamp assignments

Sub LampTimer()
	dim x, chglamp
	if UsingROM then chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update2	'update (fading logic only)

End Sub


Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

sub DisableLighting2(pri, DLintensityMax, DLintensityMin, ByVal aLvl)    'cp's script  DLintensity = disabled lighting intesity
    if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)    'Callbacks don't get this filter automatically
    pri.blenddisablelighting = (aLvl * (DLintensityMax-DLintensityMin)) + DLintensityMin
End Sub

Sub SetModLamp(id, val)
	Lampz.state(id) = val
End Sub

const insert_dl_on_red = 60
const insert_dl_on_green = 40
const insert_dl_on_orange = 80
const insert_dl_on_white = 60
const insert_dl_on_blue = 200
const insert_dl_on_yellow = 120
const insert_dl_off_yellow = 15
const greenbulb = 300
const bluebulb = 100
const redbulb = 400
const orangebulb = 300
const yellowbulb = 300
const whitebulb = 100

Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (max level / full MS fading time). The Modulate property must be set to 1 / max level if lamp is modulated.
	dim x : for x = 0 to 150 : Lampz.FadeSpeedUp(x) = 1/40 : Lampz.FadeSpeedDown(x) = 1/120 : Lampz.Modulate(x) = 1 : next

' GI Fading
	Lampz.FadeSpeedUp(140) = 1/40 : Lampz.FadeSpeedDown(140) = 1/16 : Lampz.Modulate(140) = 1
	Lampz.FadeSpeedUp(141) = 1/40 : Lampz.FadeSpeedDown(141) = 1/16 : Lampz.Modulate(141) = 1


	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

' Backwall Lights

	Lampz.MassAssign(66)= l66a
	Lampz.MassAssign(66)= l66a_pf
	Lampz.MassAssign(66)= l66a_top
	Lampz.MassAssign(66)= l66b
	Lampz.MassAssign(66)= l66b_top
	Lampz.MassAssign(66)= l66b_pf
	Lampz.MassAssign(66)= l66c
	Lampz.MassAssign(66)= l66c_pf
	Lampz.MassAssign(66)= l66c_top
	Lampz.MassAssign(82)= l82
	Lampz.MassAssign(82)= l82_pf
	Lampz.MassAssign(82)= l82_top
	Lampz.MassAssign(82)= l82a
	Lampz.MassAssign(82)= l82a_pf
	Lampz.MassAssign(82)= l82a_top
	Lampz.MassAssign(98)= l98
	Lampz.MassAssign(98)= l98_pf
	Lampz.MassAssign(98)= l98_top
	Lampz.MassAssign(98)= l98a
	Lampz.MassAssign(98)= l98a_pf
	Lampz.MassAssign(98)= l98a_top

	Lampz.Callback(66) = "dt66light"
	Lampz.Callback(82) = "dt82light"
	Lampz.Callback(98) = "dt98light"

'Star Targets

	Lampz.MassAssign(65)= l65
	Lampz.Callback(65) = "DisableLighting2 pstar201, 1.5, 0.2,"
	Lampz.MassAssign(66)= l66
	Lampz.Callback(66) = "DisableLighting2 pstar26, 1.5, 0.2,"
	Lampz.MassAssign(81)= l81
	Lampz.Callback(81) = "DisableLighting2 pstar202, 1.5, 0.2,"
	Lampz.MassAssign(97)= l97
	Lampz.Callback(97) = "DisableLighting2 pstar203, 1.5, 0.2,"
	Lampz.MassAssign(113)= l113
	Lampz.Callback(113) = "DisableLighting2 pstar25, 1.5, 0.2,"

'Playfield

	Lampz.MassAssign(1)= l1
	Lampz.MassAssign(1)= l1a
	Lampz.Callback(1) = "DisableLighting2 p1, insert_dl_on_yellow, 1,"
	Lampz.Callback(1) = "DisableLighting2 bulb1, yellowbulb, 5,"
	Lampz.Callback(1) = "DisableLighting2 p1off, .1, insert_dl_off_yellow,"
	Lampz.MassAssign(2)= l2
	Lampz.MassAssign(2)= l2a
	Lampz.Callback(2) = "DisableLighting2 p2, insert_dl_on_blue, 1,"
	Lampz.Callback(2) = "DisableLighting2 bulb2, bluebulb, 5,"
	Lampz.Callback(2) = "DisableLighting2 p2off, .1, 3,"
	Lampz.MassAssign(3)= l3
	Lampz.MassAssign(3)= l3a
	Lampz.Callback(3) = "DisableLighting2 p3, insert_dl_on_blue, 1,"
	Lampz.Callback(3) = "DisableLighting2 bulb3, bluebulb, 5,"
	Lampz.Callback(3) = "DisableLighting2 p3off, .1, 3,"
	Lampz.MassAssign(4)= l4
	Lampz.MassAssign(4)= l4a
	Lampz.Callback(4) = "DisableLighting2 p4, insert_dl_on_blue, 1,"
	Lampz.Callback(4) = "DisableLighting2 bulb4, bluebulb, 5,"
	Lampz.Callback(4) = "DisableLighting2 p4off, .1, 3,"
	Lampz.MassAssign(5)= l5
	Lampz.MassAssign(5)= l5a
	Lampz.Callback(5) = "DisableLighting2 p5, insert_dl_on_green, 1,"
	Lampz.Callback(5) = "DisableLighting2 bulb5, greenbulb, 5,"
	Lampz.Callback(5) = "DisableLighting2 p5off, .1, 3,"
	Lampz.MassAssign(6)= l6
	Lampz.MassAssign(6)= l6a
	Lampz.Callback(6) = "DisableLighting2 p6, insert_dl_on_green, 1,"
	Lampz.Callback(6) = "DisableLighting2 bulb6, greenbulb, 5,"
	Lampz.Callback(6) = "DisableLighting2 p6off, .1, 3,"
	Lampz.MassAssign(7)= l7
	Lampz.MassAssign(7)= l7a
	Lampz.Callback(7) = "DisableLighting2 p7, insert_dl_on_green, 1,"
	Lampz.Callback(7) = "DisableLighting2 bulb7, greenbulb, 5,"
	Lampz.Callback(7) = "DisableLighting2 p7off, .1, 3,"
	Lampz.MassAssign(8)= l8
	Lampz.MassAssign(8)= l8a
	Lampz.Callback(8) = "DisableLighting2 p8, insert_dl_on_green, 1,"
	Lampz.Callback(8) = "DisableLighting2 bulb8, greenbulb, 5,"
	Lampz.Callback(8) = "DisableLighting2 p8off, .1, 3,"
	Lampz.MassAssign(9)= l9
	Lampz.MassAssign(9)= l9a
	Lampz.Callback(9) = "DisableLighting2 p9, insert_dl_on_green, 1,"
	Lampz.Callback(9) = "DisableLighting2 bulb9, greenbulb, 5,"
	Lampz.Callback(9) = "DisableLighting2 p9off, .1, 3,"
	Lampz.MassAssign(10)= l10
	Lampz.MassAssign(10)= l10a
	Lampz.Callback(10) = "DisableLighting2 p10, insert_dl_on_yellow, 1,"
	Lampz.Callback(10) = "DisableLighting2 bulb10, yellowbulb, 5,"
	Lampz.Callback(10) = "DisableLighting2 p10off, .1, insert_dl_off_yellow,"
	Lampz.MassAssign(11)= l11
	Lampz.MassAssign(11)= l11a
	Lampz.Callback(11) = "DisableLighting2 p11, insert_dl_on_red, 1,"
	Lampz.Callback(11) = "DisableLighting2 bulb11, redbulb, 5,"
	Lampz.Callback(11) = "DisableLighting2 p11off, .1, 20,"
	Lampz.MassAssign(14)= l14
	Lampz.MassAssign(14)= l14a
	Lampz.Callback(14) = "DisableLighting2 p14, insert_dl_on_yellow, 1,"
	Lampz.Callback(14) = "DisableLighting2 bulb14, yellowbulb, 5,"
	Lampz.Callback(14) = "DisableLighting2 p14off, .1, insert_dl_off_yellow,"
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(15)= l15a
	Lampz.Callback(15) = "DisableLighting2 p15, insert_dl_on_blue, 1,"
	Lampz.Callback(15) = "DisableLighting2 bulb15, bluebulb, 5,"
	Lampz.Callback(15) = "DisableLighting2 p15off, .1, 3,"
	Lampz.MassAssign(17)= l17
	Lampz.MassAssign(17)= l17a
	Lampz.Callback(17) = "DisableLighting2 p17, insert_dl_on_red, 1,"
	Lampz.Callback(17) = "DisableLighting2 bulb17, redbulb, 5,"
	Lampz.Callback(17) = "DisableLighting2 p17off, .1, 20,"
	Lampz.MassAssign(18)= l18
	Lampz.MassAssign(18)= l18a
	Lampz.Callback(18) = "DisableLighting2 p18, insert_dl_on_blue, 1,"
	Lampz.Callback(18) = "DisableLighting2 bulb18, bluebulb, 5,"
	Lampz.Callback(18) = "DisableLighting2 p18off, .1, 3,"
	Lampz.MassAssign(19)= l19
	Lampz.MassAssign(19)= l19a
	Lampz.Callback(19) = "DisableLighting2 p19, insert_dl_on_blue, 1,"
	Lampz.Callback(19) = "DisableLighting2 bulb19, bluebulb, 5,"
	Lampz.Callback(19) = "DisableLighting2 p19off, .1, 3,"
	Lampz.MassAssign(20)= l20
	Lampz.MassAssign(20)= l20a
	Lampz.Callback(20) = "DisableLighting2 p20, insert_dl_on_blue, 1,"
	Lampz.Callback(20) = "DisableLighting2 bulb20, bluebulb, 5,"
	Lampz.Callback(20) = "DisableLighting2 p20off, .1, 3,"
	Lampz.MassAssign(21)= l21
	Lampz.MassAssign(21)= l21a
	Lampz.Callback(21) = "DisableLighting2 p21, insert_dl_on_yellow, 1,"
	Lampz.Callback(21) = "DisableLighting2 bulb21, yellowbulb, 5,"
	Lampz.Callback(21) = "DisableLighting2 p21off, .1, insert_dl_off_yellow,"
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(22)= l22a
	Lampz.Callback(22) = "DisableLighting2 p22, insert_dl_on_green, 1,"
	Lampz.Callback(22) = "DisableLighting2 bulb22, greenbulb, 5,"
	Lampz.Callback(22) = "DisableLighting2 p22off, .1, 3,"
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign(23)= l23a
	Lampz.Callback(23) = "DisableLighting2 p23, insert_dl_on_green, 1,"
	Lampz.Callback(23) = "DisableLighting2 bulb23, greenbulb, 5,"
	Lampz.Callback(23) = "DisableLighting2 p23off, .1, 3,"
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(24)= l24a
	Lampz.Callback(24) = "DisableLighting2 p24, insert_dl_on_green, 1,"
	Lampz.Callback(24) = "DisableLighting2 bulb24, greenbulb, 5,"
	Lampz.Callback(24) = "DisableLighting2 p24off, .1, 3,"
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(25)= l25a
	Lampz.Callback(25) = "DisableLighting2 p25, insert_dl_on_yellow, 1,"
	Lampz.Callback(25) = "DisableLighting2 bulb25, yellowbulb, 5,"
	Lampz.Callback(25) = "DisableLighting2 p25off, .1, insert_dl_off_yellow,"
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= l26a
	Lampz.Callback(26) = "DisableLighting2 p26, insert_dl_on_yellow, 1,"
	Lampz.Callback(26) = "DisableLighting2 bulb26, yellowbulb, 5,"
	Lampz.Callback(26) = "DisableLighting2 p26off, .1, insert_dl_off_yellow,"
	Lampz.MassAssign(30)= l30
	Lampz.MassAssign(30)= l30a
	Lampz.Callback(30) = "DisableLighting2 p30, insert_dl_on_yellow, 1,"
	Lampz.Callback(30) = "DisableLighting2 bulb30, yellowbulb, 5,"
	Lampz.Callback(30) = "DisableLighting2 p30off, .1, insert_dl_off_yellow,"
	Lampz.MassAssign(31)= l31
	Lampz.MassAssign(31)= l31a
	Lampz.Callback(31) = "DisableLighting2 p31, insert_dl_on_green, 1,"
	Lampz.Callback(31) = "DisableLighting2 bulb31, greenbulb, 5,"
	Lampz.Callback(31) = "DisableLighting2 p31off, .1, 3,"
	Lampz.MassAssign(33)= l33
	Lampz.MassAssign(33)= l33a
	Lampz.Callback(33) = "DisableLighting2 p33, insert_dl_on_red, 1,"
	Lampz.Callback(33) = "DisableLighting2 bulb33, redbulb, 5,"
	Lampz.Callback(33) = "DisableLighting2 p33off, .1, 20,"
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(34)= l34a
	Lampz.Callback(34) = "DisableLighting2 p34, insert_dl_on_blue, 1,"
	Lampz.Callback(34) = "DisableLighting2 bulb34, bluebulb, 5,"
	Lampz.Callback(34) = "DisableLighting2 p34off, .1, 3,"
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(35)= l35a
	Lampz.Callback(35) = "DisableLighting2 p35, insert_dl_on_blue, 1,"
	Lampz.Callback(35) = "DisableLighting2 bulb35, bluebulb, 5,"
	Lampz.Callback(35) = "DisableLighting2 p35off, .1, 3,"
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(36)= l36a
	Lampz.Callback(36) = "DisableLighting2 p36, insert_dl_on_green, 1,"
	Lampz.Callback(36) = "DisableLighting2 bulb36, greenbulb, 5,"
	Lampz.Callback(36) = "DisableLighting2 p36off, .1, 3,"
	Lampz.MassAssign(36)= l362
	Lampz.MassAssign(36)= l362a
	Lampz.Callback(36) = "DisableLighting2 p362, insert_dl_on_green, 1,"
	Lampz.Callback(36) = "DisableLighting2 bulb362, greenbulb, 5,"
	Lampz.Callback(36) = "DisableLighting2 p362off, .1, 3,"
	Lampz.MassAssign(37)= l37
	Lampz.MassAssign(37)= l37a
	Lampz.Callback(37) = "DisableLighting2 p37, insert_dl_on_white, 1,"
	Lampz.Callback(37) = "DisableLighting2 bulb37, whitebulb, 5,"
	Lampz.Callback(37) = "DisableLighting2 p37off, .1, 10,"
	Lampz.MassAssign(38)= l38
	Lampz.MassAssign(38)= l38a
	Lampz.Callback(38) = "DisableLighting2 p38, insert_dl_on_green, 1,"
	Lampz.Callback(38) = "DisableLighting2 bulb38, greenbulb, 5,"
	Lampz.Callback(38) = "DisableLighting2 p38off, .1, 3,"
	Lampz.MassAssign(39)= l39
	Lampz.MassAssign(39)= l39a
	Lampz.Callback(39) = "DisableLighting2 p39, insert_dl_on_green, 1,"
	Lampz.Callback(39) = "DisableLighting2 bulb39, greenbulb, 5,"
	Lampz.Callback(39) = "DisableLighting2 p39off, .1, 3,"
	Lampz.MassAssign(40)= l40
	Lampz.MassAssign(40)= l40a
	Lampz.Callback(40) = "DisableLighting2 p40, insert_dl_on_green, 1,"
	Lampz.Callback(40) = "DisableLighting2 bulb40, greenbulb, 5,"
	Lampz.Callback(40) = "DisableLighting2 p40off, .1, 3,"
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign(41)= l41a
	Lampz.Callback(41) = "DisableLighting2 p41, insert_dl_on_white, 1,"
	Lampz.Callback(41) = "DisableLighting2 bulb41, whitebulb, 5,"
	Lampz.Callback(41) = "DisableLighting2 p41off, .1, 10,"
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(42)= l42a
	Lampz.Callback(42) = "DisableLighting2 p42, insert_dl_on_yellow, 1,"
	Lampz.Callback(42) = "DisableLighting2 bulb42, yellowbulb, 5,"
	Lampz.Callback(42) = "DisableLighting2 p42off, .1, insert_dl_off_yellow,"
	Lampz.MassAssign(46)= l46
	Lampz.MassAssign(46)= l46a
	Lampz.Callback(46) = "DisableLighting2 p46, insert_dl_on_white, 1,"
	Lampz.Callback(46) = "DisableLighting2 bulb46, whitebulb, 5,"
	Lampz.Callback(46) = "DisableLighting2 p46off, .1, 10,"
	Lampz.MassAssign(49)= l49
	Lampz.MassAssign(49)= l49a
	Lampz.Callback(49) = "DisableLighting2 p49, insert_dl_on_yellow, 1,"
	Lampz.Callback(49) = "DisableLighting2 bulb49, yellowbulb, 5,"
	Lampz.Callback(49) = "DisableLighting2 p49off, .1, insert_dl_off_yellow,"
	Lampz.MassAssign(49)= l492
	Lampz.MassAssign(49)= l492a
	Lampz.Callback(49) = "DisableLighting2 p492, insert_dl_on_blue, 1,"
	Lampz.Callback(49) = "DisableLighting2 bulb492, bluebulb, 5,"
	Lampz.Callback(49) = "DisableLighting2 p492off, .1, 3,"
	Lampz.MassAssign(50)= l50
	Lampz.MassAssign(50)= l50a
	Lampz.Callback(50) = "DisableLighting2 p50, insert_dl_on_blue, 1,"
	Lampz.Callback(50) = "DisableLighting2 bulb50, bluebulb, 5,"
	Lampz.Callback(50) = "DisableLighting2 p50off, .1, 3,"
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= l51a
	Lampz.Callback(51) = "DisableLighting2 p51, insert_dl_on_blue, 1,"
	Lampz.Callback(51) = "DisableLighting2 bulb51, bluebulb, 5,"
	Lampz.Callback(51) = "DisableLighting2 p51off, .1, 3,"
	Lampz.MassAssign(52)= l52
	Lampz.MassAssign(52)= l52a
	Lampz.Callback(52) = "DisableLighting2 p52, insert_dl_on_blue, 1,"
	Lampz.Callback(52) = "DisableLighting2 bulb52, bluebulb, 5,"
	Lampz.Callback(52) = "DisableLighting2 p52off, .1, 3,"
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= l53a
	Lampz.Callback(53) = "DisableLighting2 p53, insert_dl_on_blue, 1,"
	Lampz.Callback(53) = "DisableLighting2 bulb53, bluebulb, 5,"
	Lampz.Callback(53) = "DisableLighting2 p53off, .1, 3,"
	Lampz.MassAssign(54)= l54
	Lampz.MassAssign(54)= l54a
	Lampz.Callback(54) = "DisableLighting2 p54, insert_dl_on_green, 1,"
	Lampz.Callback(54) = "DisableLighting2 bulb54, greenbulb, 5,"
	Lampz.Callback(54) = "DisableLighting2 p54off, .1, 3,"
	Lampz.MassAssign(55)= l55
	Lampz.MassAssign(55)= l55a
	Lampz.Callback(55) = "DisableLighting2 p55, insert_dl_on_green, 1,"
	Lampz.Callback(55) = "DisableLighting2 bulb55, greenbulb, 5,"
	Lampz.Callback(55) = "DisableLighting2 p55off, .1, 3,"
	Lampz.MassAssign(56)= l56
	Lampz.MassAssign(56)= l56a
	Lampz.Callback(56) = "DisableLighting2 p56, insert_dl_on_blue, 1,"
	Lampz.Callback(56) = "DisableLighting2 bulb56, bluebulb, 5,"
	Lampz.Callback(56) = "DisableLighting2 p56off, .1, 3,"
	Lampz.MassAssign(57)= l57
	Lampz.MassAssign(57)= l57a
	Lampz.Callback(57) = "DisableLighting2 p57, insert_dl_on_green, 1,"
	Lampz.Callback(57) = "DisableLighting2 bulb57, greenbulb, 5,"
	Lampz.Callback(57) = "DisableLighting2 p57off, .1, 3,"
	Lampz.MassAssign(58)= l58
	Lampz.MassAssign(58)= l58a
	Lampz.Callback(58) = "DisableLighting2 p58, insert_dl_on_red, 1,"
	Lampz.Callback(58) = "DisableLighting2 bulb58, redbulb, 5,"
	Lampz.Callback(58) = "DisableLighting2 p58off, .1, 20,"
	Lampz.MassAssign(60)= l60
	Lampz.MassAssign(60)= l60a
	Lampz.Callback(60) = "DisableLighting2 p60, insert_dl_on_white, 1,"
	Lampz.Callback(60) = "DisableLighting2 bulb60, whitebulb, 5,"
	Lampz.Callback(60) = "DisableLighting2 p60off, .1, 10,"
	Lampz.MassAssign(62)= l62
	Lampz.MassAssign(62)= l62a
	Lampz.Callback(62) = "DisableLighting2 p62, insert_dl_on_orange, 1,"
	Lampz.Callback(62) = "DisableLighting2 bulb62, orangebulb, 5,"
	Lampz.Callback(62) = "DisableLighting2 p62off, .1, 15,"
	Lampz.MassAssign(63)= l63
	Lampz.MassAssign(63)= l63a
	Lampz.Callback(63) = "DisableLighting2 p63, insert_dl_on_red, 1,"
	Lampz.Callback(63) = "DisableLighting2 bulb63, redbulb, 5,"
	Lampz.Callback(63) = "DisableLighting2 p63off, .1, 20,"


' Bumpers
	Lampz.MassAssign(44)= bumpersmalllight1
	Lampz.MassAssign(44)= bumperbiglight1
	Lampz.MassAssign(44)= bumpershadow1
	Lampz.MassAssign(44)= bumperhighlight1
	Lampz.Callback(44) = "bumperbulb1flash"
	Lampz.MassAssign(12)= bumpersmalllight2
	Lampz.MassAssign(12)= bumperbiglight2
	Lampz.MassAssign(12)= bumpershadow2
	Lampz.MassAssign(12)= bumperhighlight2
	Lampz.Callback(12) = "bumperbulb2flash"
	Lampz.MassAssign(28)= bumpersmalllight3
	Lampz.MassAssign(28)= bumperbiglight3
	Lampz.MassAssign(28)= bumpershadow3
	Lampz.MassAssign(28)= bumperhighlight3
	Lampz.Callback(28) = "bumperbulb3flash"



' Credit Light
	Lampz.MassAssign(59)= l59
	Lampz.MassAssign(59)= l59a
	Lampz.Callback(59) = "DisableLighting2 p59, 0.275, 0.1, "


' GI Callbacks
	Lampz.Callback(140) = "GIUpdates"
	Lampz.Callback(141) = "BGGIUpdates"


	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub


'====================
'Class jungle nf
'====================

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks
'Version 0.14 - Updated to support modulated signals - Niwak

Class LampFader
	Public FadeSpeedDown(150), FadeSpeedUp(150)
	Private Lock(150), Loaded(150), OnOff(150)
	Public UseFunction
	Private cFilter
	Public UseCallback(150), cCallback(150)
	Public Lvl(150), Obj(150)
	Private Mult(150)
	Public FrameTime
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
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

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


Sub bumperbulb1flash(ByVal aLvl)	'argument is unused
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	FlBumperFadeTarget(1) = aLvl*bumperflashlevel
End Sub

Sub bumperbulb2flash(ByVal aLvl)	'argument is unused
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	FlBumperFadeTarget(2) = aLvl*bumperflashlevel
End Sub

Sub bumperbulb3flash(ByVal aLvl)	'argument is unused
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	FlBumperFadeTarget(3) = aLvl*bumperflashlevel
End Sub

'*******************************************
'  Backwall Lamps during Drop Targets up or down
'*******************************************
Sub dt66light(ByVal aLvl)
	if dt44up = 0 Then 
		l66b_pf.IntensityScale = aLvl
		l66b_pf2.IntensityScale =0
	elseif dt44up = 1 Then
		l66b_pf2.IntensityScale = aLvl
		l66b_pf.IntensityScale = 0
	end If
End Sub

Sub dt82light(ByVal aLvl)
	if dt44up = 1 AND dt43up = 1 AND dt42up = 1 Then
		l82a_pf.IntensityScale = 0
		l82a_pf2.IntensityScale =0
		l82a_pf3.IntensityScale =0
		l82a_pf4.IntensityScale =aLvl
	elseif dt44up = 0 AND dt43up = 1 AND dt42up = 1 Then
		l82a_pf.IntensityScale = 0
		l82a_pf2.IntensityScale =aLvl
		l82a_pf3.IntensityScale =0
		l82a_pf4.IntensityScale =0
	elseif dt44up = 0 AND dt43up = 0 AND dt42up = 1 Then
		l82a_pf.IntensityScale = 0
		l82a_pf2.IntensityScale =0
		l82a_pf3.IntensityScale =aLvl
		l82a_pf4.IntensityScale =0
	Else 'all drop targets down
		l82a_pf.IntensityScale = aLvl
		l82a_pf2.IntensityScale =0
		l82a_pf3.IntensityScale =0
		l82a_pf4.IntensityScale =0
	End If
End Sub

Sub dt98light(ByVal aLvl)
	if dt42up = 0 Then 
		l98a_pf.IntensityScale = aLvl
		l98a_pf2.IntensityScale =0
	elseif dt42up = 1 Then
		l98a_pf2.IntensityScale = aLvl
		l98a_pf.IntensityScale = 0
	end If
End Sub


'*******************************************
'  Ball brightness code
'*******************************************

if BallLightness = 0 Then
	table1.BallImage="ball-dark"
	table1.BallFrontDecal="JPBall-Scratches"
elseif BallLightness = 1 Then
	table1.BallImage="ball_HDR"
	table1.BallFrontDecal="Scratches"
elseif BallLightness = 2 Then
	table1.BallImage="ball-light-hf"
	table1.BallFrontDecal="scratchedmorelight"
else
	table1.BallImage="ball-lighter-hf"
	table1.BallFrontDecal="scratchedmorelight"
End if

' ****************************************************



'*******************************************
' Hybrid code for VR, Cab, and Desktop
'*******************************************

Dim VRThings

if VR_Room = 0 and cab_mode = 0 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRMin:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in VRBackglass:VRThings.visible = 0:Next
	for each VRThings in DTRails:VRThings.visible = 1:Next
	for each VRThings in OceanEnvironment:VRThings.visible = 0:Next
	OuterPrimBlack_dt.visible = 1
	OuterPrimBlack_cab_VR.visible = 0
	OuterPrimBlack_cab.visible = 0
	L_DT_1.State = 1
	L_DT_2.State = 1
	SharkTimer.enabled = 0

Elseif VR_Room = 0 and cab_mode = 1 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRMin:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in VRBackglass:VRThings.visible = 0:Next
	for each VRThings in DTBackglass:VRThings.visible = 0: Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
	for each VRThings in OceanEnvironment:VRThings.visible = 0:Next
	OuterPrimBlack_dt.visible = 0
	OuterPrimBlack_cab_VR.visible = 0
	OuterPrimBlack_cab.visible = 1
	L_DT_1.Visible = 0
	L_DT_1.State = 0
	L_DT_2.Visible = 0
	L_DT_2.State = 0
	SharkTimer.enabled = 0

Else

	if OceanEnv = 0 Then
		for each VRThings in VRStuff:VRThings.visible = 1:Next
		for each VRThings in VRMin:VRThings.visible = 1:Next
		for each VRThings in VRClock:VRThings.visible = WallClock:Next
		for each VRThings in DTBackglass:VRThings.visible = 0: Next
		for each VRThings in DTRails:VRThings.visible = 0:Next
		for each VRThings in OceanEnvironment:VRThings.visible = 0:Next
		Primary_Cab_Body.blenddisablelighting = .6
		VR_Backbox.blenddisablelighting = .6
		OuterPrimBlack_dt.visible = 0
		OuterPrimBlack_cab_VR.visible = 1
		OuterPrimBlack_cab.visible = 0
		L_DT_1.Visible = 0
		L_DT_1.State = 0
		L_DT_2.Visible = 0
		L_DT_2.State = 0

	'Custom Walls, Floor, and Roof
		if CustomWalls = 1 Then
			VR_Wall_Left.image = "VR_Wall_Left"
			VR_Wall_Right.image = "VR_Wall_Right"
			VR_Floor.image = "VR_Floor"
			VR_Roof.image = "VR_Roof"
		end if


		If topper = 1 Then
			Primary_topper.visible = 1
		Else
			Primary_topper.visible = 0
		End If

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

		SharkTimer.enabled = 0

	Else
		for each VRThings in VRStuff:VRThings.visible = 1:Next
		for each VRThings in VRMin:VRThings.visible = 0:Next
		for each VRThings in VRClock:VRThings.visible = 0:Next
		for each VRThings in DTBackglass:VRThings.visible = 0: Next
		for each VRThings in DTRails:VRThings.visible = 0:Next
		for each VRThings in OceanEnvironment:VRThings.visible = 1:Next
		Primary_Cab_Body.blenddisablelighting = 1
		VR_Backbox.blenddisablelighting = 1
		OuterPrimBlack_dt.visible = 0
		OuterPrimBlack_cab_VR.visible = 1
		OuterPrimBlack_cab.visible = 0
		L_DT_1.Visible = 0
		L_DT_1.State = 0
		L_DT_2.Visible = 0
		L_DT_2.State = 0

		Primary_topper.visible = 0
		VRposter.visible = 0
		VRposter2.visible = 0


		VR_Shark.PlayAnimEndless(0.25)
		VR_Shark1.PlayAnimEndless(0.25)
		VR_Angelfish_1.PlayAnimEndless(0.25)
		VR_Angelfish_2.PlayAnimEndless(0.23)
		VR_Angelfish_3.PlayAnimEndless(0.23)
		VR_Angelfish_4.PlayAnimEndless(0.25)
		VR_Angelfish_5.PlayAnimEndless(0.22)
		VR_Angelfish_6.PlayAnimEndless(0.23)
		VR_Angelfish_7.PlayAnimEndless(0.25)
		VR_Angelfish_8.PlayAnimEndless(0.23)
		VR_Angelfish_9.PlayAnimEndless(0.24)
		VR_Angelfish_10.PlayAnimEndless(0.25)
		VR_Angelfish_11.PlayAnimEndless(0.23)
		VR_Angelfish_12.PlayAnimEndless(0.25)
		VR_Angelfish_13.PlayAnimEndless(0.25)
		VR_Angelfish_14.PlayAnimEndless(0.23)
		VR_MahiMahi_1.PlayAnimEndless(0.10)
		VR_MahiMahi_2.PlayAnimEndless(0.09)
		VR_MahiMahi_3.PlayAnimEndless(0.08)
		VR_MahiMahi_4.PlayAnimEndless(0.11)
		VR_MahiMahi_5.PlayAnimEndless(0.085)
		VR_MahiMahi_6.PlayAnimEndless(0.095)
		VR_MahiMahi_7.PlayAnimEndless(0.15)
		SharkTimer.enabled = 1

	End If

End If

'*******************************************
' VR Ocean Environment animation
'*******************************************

Dim sharkSpeed : sharkSpeed = 0.04
Dim fishSpeed : fishSpeed = 0.02
Dim bigFishSpeed : bigFishSpeed = 0.02
Dim swimSpeedMod : swimSpeedMod = 1.0

Sub SharkTimer_Timer
	VR_Shark.ObjRotZ = VR_Shark.ObjRotZ + (sharkSpeed * swimSpeedMod)
	VR_Shark1.ObjRotZ = VR_Shark1.ObjRotZ - (sharkSpeed * swimSpeedMod)

	VR_Angelfish_1.ObjRotZ = VR_Angelfish_1.ObjRotZ - (fishSpeed * swimSpeedMod)
	VR_Angelfish_2.ObjRotZ = VR_Angelfish_2.ObjRotZ - (fishSpeed * swimSpeedMod)
	VR_Angelfish_3.ObjRotZ = VR_Angelfish_3.ObjRotZ - (fishSpeed * swimSpeedMod)
	VR_Angelfish_4.ObjRotZ = VR_Angelfish_4.ObjRotZ - (fishSpeed * swimSpeedMod)
	VR_Angelfish_5.ObjRotZ = VR_Angelfish_5.ObjRotZ - (fishSpeed * swimSpeedMod)
	VR_Angelfish_6.ObjRotZ = VR_Angelfish_6.ObjRotZ - (fishSpeed * swimSpeedMod)
	VR_Angelfish_7.ObjRotZ = VR_Angelfish_7.ObjRotZ - (fishSpeed * swimSpeedMod)
	VR_Angelfish_8.ObjRotZ = VR_Angelfish_8.ObjRotZ + (fishSpeed * swimSpeedMod)
	VR_Angelfish_9.ObjRotZ = VR_Angelfish_9.ObjRotZ + (fishSpeed * swimSpeedMod)
	VR_Angelfish_10.ObjRotZ = VR_Angelfish_10.ObjRotZ + (fishSpeed * swimSpeedMod)
	VR_Angelfish_11.ObjRotZ = VR_Angelfish_11.ObjRotZ - (fishSpeed * swimSpeedMod)
	VR_Angelfish_12.ObjRotZ = VR_Angelfish_12.ObjRotZ - (fishSpeed * swimSpeedMod)
	VR_Angelfish_13.ObjRotZ = VR_Angelfish_13.ObjRotZ - (fishSpeed * swimSpeedMod)
	VR_Angelfish_14.ObjRotZ = VR_Angelfish_14.ObjRotZ - (fishSpeed * swimSpeedMod)

	VR_MahiMahi_1.ObjRotZ = VR_MahiMahi_1.ObjRotZ - (bigFishSpeed * swimSpeedMod)
	VR_MahiMahi_2.ObjRotZ = VR_MahiMahi_2.ObjRotZ - (bigFishSpeed * swimSpeedMod)
	VR_MahiMahi_3.ObjRotZ = VR_MahiMahi_3.ObjRotZ - (bigFishSpeed * swimSpeedMod)
	VR_MahiMahi_4.ObjRotZ = VR_MahiMahi_4.ObjRotZ - (bigFishSpeed * swimSpeedMod)
	VR_MahiMahi_5.ObjRotZ = VR_MahiMahi_5.ObjRotZ - (bigFishSpeed * swimSpeedMod)
	VR_MahiMahi_6.ObjRotZ = VR_MahiMahi_6.ObjRotZ - (bigFishSpeed * swimSpeedMod)
	VR_MahiMahi_7.ObjRotZ = VR_MahiMahi_7.ObjRotZ - (bigFishSpeed * swimSpeedMod)
End Sub



'*******************************************
' VR Clock
'*******************************************

Dim CurrentMinute ' for VR clock

Sub ClockTimer_Timer()
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
	Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())

End Sub


'*******************************************
' VR Plunger Code
'*******************************************

Sub TimerVRPlunger_Timer
  If PinCab_Shooter.Y < 2211.572 then
       PinCab_Shooter.Y = PinCab_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger1_Timer
  PinCab_Shooter.Y = 2121.572 + (5* Plunger.Position) -20
End Sub


'*******************************************
'Digital Display
'*******************************************

Dim Digits(31)
' 1st Player
Digits(0) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16,LEDc17)
Digits(1) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(2) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(3) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46,LEDc47)
Digits(4) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(5) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)
Digits(6) = Array(LED70,LED71,LED72,LED73,LED74,LED75,LED76)

' 2nd Player
Digits(7) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86,LEDc87)
Digits(8) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(9) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(10) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116,LEDc117)
Digits(11) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(12) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)
Digits(13) = Array(LED140,LED141,LED142,LED143,LED144,LED145,LED146)

' 3rd Player
Digits(14) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156,LEDc157)
Digits(15) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(16) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(17) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186,LEDc187)
Digits(18) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(19) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)
Digits(20) = Array(LED210,LED211,LED212,LED213,LED214,LED215,LED216)

' 4th Player
Digits(21) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226,LEDc227)
Digits(22) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(23) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(24) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256,LEDc257)
Digits(25) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(26) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)
Digits(27) = Array(LED280,LED281,LED282,LED283,LED284,LED285,LED286)

' Credits
Digits(28) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(29) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)

' Balls
Digits(30) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(31) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)


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
						obj.intensity=30
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


'*******************************************
' Setup Backglass
'*******************************************

Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen, ix, xx, yy, xobj

Sub setup_backglass()

	xoff = -20
	yoff = 50 '78
	zoff = 699
	xrot = -90
	zscale = 0.0000001

	xcen = 0  '(130 /2) - (92 / 2)
	ycen = (780 /2 ) + (203 /2)

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
end sub


Dim VRDigits(32)
' 1st Player
VRDigits(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6,LEDc1x7)
VRDigits(1) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6)
VRDigits(2) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6)
VRDigits(3) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6,LEDc4x7)
VRDigits(4) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6)
VRDigits(5) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6)
VRDigits(6) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6)

' 2nd Player
VRDigits(7) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6,LEDc8x7)
VRDigits(8) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6)
VRDigits(9) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6)
VRDigits(10) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6,LEDc11x7)
VRDigits(11) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6)
VRDigits(12) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6)
VRDigits(13) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6)

' 3rd Player
VRDigits(14) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006,LEDc1x007)
VRDigits(15) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106)
VRDigits(16) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206)
VRDigits(17) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306,LEDc1x307)
VRDigits(18) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406)
VRDigits(19) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506)
VRDigits(20) = Array(LED1x600,LED1x601,LED1x602,LED1x603,LED1x604,LED1x605,LED1x606)

' 4th Player
VRDigits(21) = Array(LED2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006,LEDc2x007)
VRDigits(22) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106)
VRDigits(23) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206)
VRDigits(24) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306,LEDc2x307)
VRDigits(25) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406)
VRDigits(26) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506)
VRDigits(27) = Array(LED2x600,LED2x601,LED2x602,LED2x603,LED2x604,LED2x605,LED2x606)

' Credits
VRDigits(28) = Array(LEDax300,LEDax301,LEDax302,LEDax303,LEDax304,LEDax305,LEDax306)
VRDigits(29) = Array(LEDbx400,LEDbx401,LEDbx402,LEDbx403,LEDbx404,LEDbx405,LEDbx406)

' Balls
VRDigits(30) = Array(LEDcx500,LEDcx501,LEDcx502,LEDcx503,LEDcx504,LEDcx505,LEDcx506)
VRDigits(31) = Array(LEDdx600,LEDdx601,LEDdx602,LEDdx603,LEDdx604,LEDdx605,LEDdx606)


dim DisplayColor
DisplayColor =  RGB(255,40,1)

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
					For Each obj In Digits(num)
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
		Object.Opacity = 20
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 8
	End If
End Sub

Sub InitDigits()
	dim tmp, x, obj
	for x = 0 to uBound(VRDigits)
		if IsArray(VRDigits(x) ) then
			For each obj in VRDigits(x)
				obj.height = obj.height + 0
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

If VR_Room=1 Then
	InitDigits
End If

'*******************************************
' LAMP CALLBACK for the 6 backglass flasher lamps (not the solenoid conrolled ones)
'*******************************************


if VR_Room = 0 and cab_mode = 0 Then
	Set LampCallback = GetRef("UpdateDTLamps")
End If

if VR_Room = 1 Then
	Set LampCallback = GetRef("UpdateVRLamps")
End If

Sub UpdateDTLamps()

  Dim DTL

	If Controller.Lamp(43) = 0 Then: ShootAgainReel.setValue(0):	Else: ShootAgainReel.setValue(1) 'Shoot Again
	If Controller.Lamp(13) = 0 Then: BIPReel.setValue(0):			Else: BIPReel.setValue(1) 'Ball in Play
	If Controller.Lamp(61) = 0 Then: TiltReel.setValue(0):			Else: TiltReel.setValue(1) 'Tilt
	If Controller.Lamp(45) = 0 Then: GameOverReel.setValue(0):		Else: GameOverReel.setValue(1) 'Game Over
	If Controller.Lamp(27) = 0 Then: MatchReel.setValue(0):			Else: MatchReel.setValue(1) 'Match
	If Controller.Lamp(29) = 0 Then: HighScoreReel.setValue(0):		Else: HighScoreReel.setValue(1) 'High Score

End Sub


Sub UpdateVRLamps()

	If Controller.Lamp(43) = 0 Then: VR_ShootAgain.visible=0: else: VR_ShootAgain.visible=1 'Shoot again
	If Controller.Lamp(13) = 0 Then: VR_BIP.visible=0: else: VR_BIP.visible=1 'Ball in  play
	If Controller.Lamp(61) = 0 Then: VR_Tilt.visible=0: else: VR_Tilt.visible=1 'Tilt
	If Controller.Lamp(45) = 0 Then: VR_GameOver.visible=0: else: VR_GameOver.visible=1 'Game Over
	If Controller.Lamp(27) = 0 Then: VR_Match.visible=0: else: VR_Match.visible=1 'Match
	If Controller.Lamp(29) = 0 Then: VR_HighScore.visible=0: else: VR_HighScore.visible=1 'High Score

End Sub



'******************************************************
'****  Ball GI Brightness Level Code
'******************************************************

const BallBrightMax = 255			'Brightness setting when GI is on (max of 255). Only applies for Normal ball.
const BallBrightMin = 100			'Brightness setting when GI is off (don't set above the max). Only applies for Normal ball.

Sub UpdateBallBrightness
	Dim b, brightness
	For b = 0 to UBound(gBOT)
		if ballbrightness >=0 Then gBOT(b).color = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)
			if b = UBound(gBOT) then 'until last ball brightness is set, then reset to -1
			if ballbrightness = ballbrightMax Or ballbrightness = ballbrightMin then ballbrightness = -1
		end if
	Next
End Sub


'*******************************************
' GI Routines
'*******************************************

dim ballbrightness
dim gilvl:gilvl = 1


'*******************************************
' Backglass GI and lamps
'*******************************************


Sub BGGIUpdates(ByVal aLvl)

	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically

	if vr_room = 1 and cab_mode = 0 then

		if alvl = 0 Then
			VR_Backglass.blenddisablelighting = 0.75
		Elseif aLvl = 1 then
			VR_Backglass.blenddisablelighting = 3.5
		Else
			VR_Backglass.blenddisablelighting = 2.75*alvl + 0.75
		end if
	elseif vr_room = 0 and cab_mode = 0 then
		if aLvl = 0 then
			L_DT_1.visible = 0
			L_DT_2.visible = 0
		Else
			L_DT_1.visible = 1
			L_DT_2.visible = 1
		End If
	Else
			L_DT_1.visible = 0
			L_DT_2.visible = 0
	End If
End Sub


Sub GIUpdates(ByVal aLvl)

	dim x, girubbercolor, giflippercolor

	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically

	if aLvl = 0 then										'GI OFF, let's hide ON prims
		if ballbrightness <> -1 then ballbrightness = ballbrightMin

	Elseif aLvl = 1 then									'GI ON, let's hide OFF prims
		if ballbrightness <> -1 then ballbrightness = ballbrightMax

	Else
		if gilvl = 0 Then								'GI has just changed from OFF to fading, let's show ON
			ballbrightness = ballbrightMin + 1
		elseif gilvl = 1 Then							'GI has just changed from ON to fading, let's show OFF
			ballbrightness = ballbrightMax - 1
		Else
			'no change
		end if
	end if

	dim mm,bp, bpl

	For each xx in GI:xx.Intensityscale = aLvl: Next

' Flippers (2 main and the one upper right)

	Lflipmesh1.blenddisablelighting = .1 * alvl + .075
	Rflipmesh1.blenddisablelighting = .1 * alvl + .075
	RflipmeshUp.blenddisablelighting = .1 * alvl + .075

' flippers
	giflippercolor = 127*alvl + 128
	MaterialColor "Plastic Flippers",RGB(giflippercolor,giflippercolor,giflippercolor)

' targets
	For each x in GITargets
		x.blenddisablelighting = 0.15 * alvl + .1
	Next

	For each x in GIColorDropTarget
		x.blenddisablelighting = 0.05 * alvl + .1
	Next

' prims (Blue Pegs)
	For each x in GIPegs
		x.blenddisablelighting = .015 * alvl + .005
	Next

' prims (Blue Top Lane Rollovers)
	For each x in GILaneRollovers
		x.blenddisablelighting = -.025 * alvl + .05
	Next

' prims (Cutouts)
	For each x in GICutoutPrims
		x.blenddisablelighting = .25 * alvl + .15
	Next

	For each x in GICutoutWalls
		x.blenddisablelighting = .1 * alvl + .2
	Next

' prims (Spinner, apron, plastics)

	SpinnerPrim.blenddisablelighting = 0.2 * alvl + .1
	ApronTopVisible.blenddisablelighting = 0.075 * alvl + .075
	PlungerCover.blenddisablelighting = 0.075 * alvl + .075
	pKickerArm.blenddisablelighting = 0.2 * alvl + .1

	for each x in GIPlastics
		x.blenddisablelighting = -.1 * alvl +.15
	Next

' prims (White Screws Main)
	For each x in ScrewWhiteCaps
		x.blenddisablelighting = .05 * alvl + .005
	Next

' prims (Star Triggers)
	For each x in GIStars
		x.blenddisablelighting = .02 * alvl + .005
	Next

' rubbers
	girubbercolor = 128*alvl + 128
	MaterialColor "Rubber White",RGB(girubbercolor,girubbercolor,girubbercolor)

' Metal Gates
	For each x in GIMetal
		x.blenddisablelighting = 0.0075 * alvl + .0075
	Next

'GI PF bakes
	FlasherGI.opacity = 190 * alvl

' relaysounds for GI
	if gilvl = 1 then
		Sound_GI_Relay 1, Relay_GI
	end If

	if gilvl = 0 then
		Sound_GI_Relay 0, Relay_GI
	end If

' ball
	if ballbrightness <> ballbrightMax Or ballbrightness <> ballbrightMin Or ballbrightness <> -1 then ballbrightness = INT(alvl * (ballbrightMax - ballbrightMin) + ballbrightMin)

	gilvl = alvl


End Sub



'*******************************************
' Set Up Backglass Flashers
'   this is for lining up the backglass flashers on top of a backglass image
'*******************************************

Sub SetBackglass()
	Dim obj

	For Each obj In VRBackglassFlash
		obj.x = obj.x
		obj.height = - obj.y
		obj.y = 60 '78 'adjusts the distance from the backglass towards the user
	Next

End Sub

'*******************************************
' LUT
'*******************************************

Dim bLutActive

Sub SetLUT
	Table1.ColorGradeImage = "LUT" & LUTset
	VRFlashLUT.imageA = "FlashLUT" & LUTset
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
	VRFlashLUT.opacity = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
	VRFlashLUT.opacity = 100
	Select Case LUTSet
		Case 0: LUTBox.text = "Fleep Natural Dark 1"
		Case 1: LUTBox.text = "Fleep Natural Dark 2"
		Case 2: LUTBox.text = "Fleep Warm Dark"
		Case 3: LUTBox.text = "Fleep Warm Bright"
		Case 4: LUTBox.text = "Fleep Warm Vivid Soft"
		Case 5: LUTBox.text = "Fleep Warm Vivid Hard"
		Case 6: LUTBox.text = "Skitso Natural and Balanced"
		Case 7: LUTBox.text = "Skitso Natural High Contrast"
		Case 8: LUTBox.text = "3rdaxis Referenced THX Standard"
		Case 9: LUTBox.text = "CalleV Punchy Brightness and Contrast"
		Case 10: LUTBox.text = "HauntFreaks Desaturated"
  		Case 11: LUTBox.text = "Tomate washed out"
        Case 12: LUTBox.text = "VPW original 1on1"
        Case 13: LUTBox.text = "bassgeige"
        Case 14: LUTBox.text = "blacklight"
        Case 15: LUTBox.text = "B&W Comic Book"
        Case 16: LUTBox.text = "Skitso New Warmer LUT"
        Case 17: LUTBox.text = "Original LUT"
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

	if LUTset = "" then LUTset = 16 'failsafe to original

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "Embryon_LUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub


Sub LoadLUT
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=16
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "Embryon_LUT.txt") then
		LUTset=16
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "Embryon_LUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=16
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub

Sub ShowLUT_Init
	LUTBox.visible = 0
End Sub


'******** Work done by UnclePaulie on Hybrid version 0.01 - 2.0 *********

' v.01	Started from Medusa as a base, and enabled all code for shadows, gBOT, sounds, rolling, hybrid, base table physics, flippers, flipper physics, materials, images, LUT, POV, ball colors, fastflips, lampz, trough logic, knocker
'		All graphics came from Redbone for playfield, plastics, apron, bumper, drop targets, spinner and others.
'		Updated the bumper image to blend with Flupper bumpers.
'		Enabled gBOT and trough system for 3 balls
'		Added option for flippers to be red, blue, or yellow.
'		Added bumpers.  Modified Flupper bumper code solution for this table.  Added constants to control DL on bumpers, and light/flash level
'		Enabled right flipper functionality
'		Updated colors of bumpers, pegs, plastic lane overlays to blue.
'		Updated the Apron image (redbone) and apron physics.  Also the plunger cover primitive.  
'		Ensured the DT targets were not multi-dimension array assignments
' v.02	Added playfield mesh and saucers.
'		Enabled updated saucer functionality.
'		Updated the apron visuals and physics.
'		Put a different pincab shooter primitive in.
'		Added walls and rails from goldchicco's table.  Had to modify, move, place, etc.
'		Modified goldchicco's backwall prim in blender, as it wrapped around saucer (real one doesn't do that)
'		Updated desktop backglass image to something less busy. Added backglass lamps
'		Added all posts, pegs, rubber physics, and associated physics and sounds.
'		Added walls and collidables, flippers, rail guides, apron, plunger, balls, rollovers, etc.
'		Added small leaf sensors, slings, spinner, spinner prim, gates, star trigger and animation.
'		Created new guide rails
'		Created new blender gate prims for lower and top gates.
' v.03	Added plastics and acrylics, redid graphics.
'		Redid the bottom gates in blender after the lower plastic wall was in, to get the right height on one leg.
'		Redid all element placement with EBisLit playfield.
'		Corrected bumper lamp assignments.
'		Added metal screws and white screw caps.
'		Updated physics walls in plastics areas.
' 		Added stand up and drop targets code solution developed by Rothbauerw
' v.04	Updated the drop target images to match real targets.
'		Changed the solenoid callouts, as there's a lamp controller 47 that switches the active solenoid for 5 of them
'		Added the cutout prims for holes and GI bulbs
'		Redid playfield_mesh to align with exact saucer placement on playfield
'		Added GI for bulbs, top plastics, and temporary  playfield (to be removed when baking) and Lampz callouts for GI.
'		Changed depth bias of bulb prims under top rollovers 
'		Adjusted heights of bulb prims and bulb halos under top left lower plastics.
'		Adjusted intensity of some GI, as it was too bright 
'		Adjusted the desktop mode backglass lamps
'		Added GI bulbs to dynnamic sources
'		Added code for adjusting the ball brightness during GI.
'		Added dynamic shadows, and code to stop shadows from going into the plunger lane via notrtlane and rtlanesense trigger.
'		Color the plunger area on the playfield black.
'		Updated the playfield with insert cutouts
'		Lights under backwall prim and 3 under slim plastic on upper left are controlled via Lampz.  
'		Added text overlay
'		Added 3D insert prims, lights, and blooms.
' 		The lamps were not defined in manual.  The work to figure them out was done in a prior version.  I used those assignments, and verified.
' v.05	Updated the flipper physics, location, start and end angles, and ensure could replicate shots on online videos.
'		Changed the flippers to early 80's to match shots.  However increased strength, torque and torque angle.
'		Added baked GI lighting
'		Added baked AO shadows
'		Made rubbers near long gate a bit more bouncy.  Lowered friction on backwall.
'		Increased plunger release speed slightly.
'		Adjusted the playfield friction down to 0.21
'		Added a rubber by upper gate.  Also moved backwall lights slightly.
'		Adjusted the lower flipper start angle, and then some flipper physics.
'		Modded the backwall lamps to not go over prims, and added additional lights with drop target code to turn on or off instead of direct shadows.
'		Hours spent on studying videos for shots, physics, etc.
'		Updated table info / rules
'		Added Drop Target Shadows and added logic in script to control 
'		Updated DTHit subroutines to account for a "soft" drop target hit.  Shadow will NOT come on with a light hit... target HAS to drop.
'		Adjusted location of top drop targets slightly
'		Updated cab pov
'		Updated playfield imasge to make the under bumper area black. Removed one hole that could be seen.
'		Adjusted saucer strength to match PAPA video.
'		Made drop targets a little narrower.
'		Updated user manual recommended DIP settings, and added a user setting to turn default DIP switches off, and allow to be changed via F6.
' 		Tuned upper orbit wall and flippers to be able to make the upper orbit saucer on the right flipper shot.  
'		PAPA said that you should be able to make it on the right flipper shot as well, and I can.
'		Removed unused images and materials.
'		Adjusted the GI Bake to be less bright in the middle.  Looked too washed out.
' v.06	Added the VR backglass image and associated lamps and text.  Image from Blacksad B2S.
'		Added code for GI for VR backglass 
'		Removed playfield reflectivity from drop targets.  Was getting weird black lines/blocks on DT's. (exists in vpx 10.74 and earlier).
' v.07	Updated the drop targets to a new primitive.
'		Recommendations from apophis to reduce slingshot force (below 4), increase playfield friction to 0.25, and then add the latest flipper tricks code
'		Also updated other flipper physics.
' 		After friction changes... needed to mod the top saucers again
'		Adjusted dL on white nuts down a touch
' v.08	Changed the zCol_playfield mesh pysics material to match playfield friction (didn't have to, I guess)
' v.09	Changed material on star triggers to "insertoff"... was missing for some reason.
'		Ensured latest code for targetbouncer
'		Updated sounds for ball collision
'		Updated the drop target primitive, and the associated images.
'		Added randomness to the plunger
'		Adjusted the depth bias and the dL on the star triggers.
'		Changed the material on the white screws to be a bit darker, and adjusted the GI levels some.
'		Redbone updated the graphics for the apron, plunger cover, and worn yellow flipper
' v.10	Adjusted the slingshot threshold.  The sensitivity was too high.
'		Redbone updated the white drop target images.  I lowered the dL on them slightly.
'		Reduced slings to 3.75
'		Adjusted the table slope to 5.5  Allowed me to change the flipper physics strength back down to 1800, and the upper right one to 2300.
'		Small adjustment to the curved walls coming to out/inlane gates.
' v.11	Updated plastics image from Redbone.  Had to move some lower screws and standoffs to align.
'		Required minor update to plastic image for holes and a minor update to GI and shadow bake image under plastic area by flipper.
'		Had to adjust the pegs by the middle right flipper.  
' 		Redbone modded graphics for targets, text overlay, plunger cover, plastics, bumper cover, and apron credit.
'		Hauntfreaks modded the VR Backglass image.
'		Moved bumper small lights up to 59 halo
' v.11o:Added Ocean environment option for VR based off Circus' VR table.
' v.12	DGrimmReaper converted the ocean images to webp and reduce the table size.  
'		DGrimmReaper also adjusted the starfish and shellfish on top of the pinball machine to fit better on the table.
' v.13	Studlygoorite fixed all of the fish animations with the static renderings off
' v.14	Adjusted a couple sticky points near the flippers after the slope was adjusted. Essentially rounded a couple corners on plastics by flippers.
'		Updated the VR room flyers to Fathom.
'		Changed all the prims in the ocean environment to not be static. (enables the animation on the fish)
'		Changed the ambient and intro sounds to backglass position
' 		Changed the metal gates material and added to GI routine.
' v.15	Defaulted the VR room to the ocean environment.
'		Updated the VR backglass to be a max of 3.5 dL.  Also changed the minimal room cab dL to 0.6
'		Rebaked the GI and AO Shadows, since I moved a couple standups near the flippers.
'		Added a very small wall block by left bumper.  Could get ball stuck with new slope.
'		Lowered the GI Bulb prims.  Looks better in VR.  Also lowered the associated GI halos.
'		Adjusted the bumper color slightly and the bumper scale down 
'		Modified the arrow insert primitives.  
'		Lowered the blue bulb dl to 100 and the yellow bulbs to 300
'		Removed logo from ball scratches
'		Adjusted material for VR Cab Metal objects
'		Redbone enhanced graphics for playfield, more flippers, bumper tops, spinner, cab body, topper, and small fixes on plastics holes
'v2.0.0	Changed images to webp format for further compression without losing quality.
'		Released
'V2.0.1	An issue showed up in vpx10.8 and a glow around balls.  Reimported webp images.


' Thank you to the VPW team for testing and graphics, especially redbone, apophis, bountybob, dgrimmreaper, studlygoorite, pinstratsdan, wylte, trytotilt, bietekwiet, astronasty, and hauntfreaks.
