'Who Dunnit 1947 - a mod of Who Dunnit
'� Bally(Midway) 1995
'a bord mod/retheme of the great ninuzzu/djrobx build

Option Explicit
Randomize

'///////////////////////-----VR Room-----///////////////////////
Const VRRoom = 0 ' 0 - VR Room off, 1 - Minimal Room, 2 - Ultra Minimal

Dim FlipperCoilRampupMode

'***********	Flipper Coil Rampup Mode	*******************************

' 0 - Static - flipper coil-ramp up is static; Underpowered systems may need to use this mode.
' 1 - Dynamic - flipper coil-ramp up changes dynamically for a better simulation of tap pass capabilities. Requires a fast system - otherwise may introduce a possible flipper lag

FlipperCoilRampupMode = 1

'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8

'///////////////////////-----ALP4K performance options (script-only)-----///////////////////////
' PFOverlayMode controls the playfield-wide PLAYFIELD_flasherN overlays, the main GPU cost on flasher shows.
'   0 = off (fastest; local flasher domes/glows still flash)
'   1 = one at a time (newest flasher's overlay wins; recommended)
'   2 = all at once (original behaviour)
Const PFOverlayMode = 1

Const FlasherIntensityGIOn = .65
Const FlasherIntensityGIOff = .85 

Const BallSize = 50
Const BallMass = 1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD:UseVPMDMD = DesktopMode
Const UseVPMModSol = 0

' Rom Name
Const cGameName = "wd_12"

LoadVPM "02000000", "WPC.VBS", 3.50

' Standard Options
Const UseSolenoids = 2
Const UseLamps = 1
Const UseSync = 1
Const HandleMech = 0 

' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = ""
Const SCoin = "fx_Coin"

Const cSingleLFlip = False
Const cSingleRFlip = False

'************************************************************************
'						 INIT TABLE
'************************************************************************

Dim bsTrough, bsLSaucer, bsRBPopper, bsRFPopper, plungerIM, wdball1, wdball2, wdball3, wdball4

Sub Table1_Init
	vpmInit Me 
	With Controller      
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Who Dunnit (Bally 1995)"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Hidden = DesktopMode
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
		.Switch(22) = 1 'close coin door
		.Switch(24) = 0 'always closed
	End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Nudging
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(sw61, sw62, sw63, sw64, sw65)

	'************  Trough	**************************
	Set wdBall4 = sw35.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set wdBall3 = sw34.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set wdBall2 = sw33.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set wdBall1 = sw32.CreateSizedballWithMass(Ballsize/2,Ballmass)
	
	Controller.Switch(35) = 1
	Controller.Switch(34) = 1
	Controller.Switch(33) = 1
	Controller.Switch(32) = 1

	'Impulse Plunger
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP sw15, 45, 0.7
		.Switch 15
		.Random 0.05
		.InitExitSnd SoundFX("AutoPlunger",DOFContactors), SoundFX("AutoPlunger",DOFContactors)
		.CreateEvents "plungerIM"
	End With

	SlotInit:TargetsInit:RampInit:mapLights
	UpdateGI 0,0:UpdateGI 1,0:UpdateGI 2,0

	If DesktopMode Then
		'lower right flasher position
		flasherflash4.Y=1443.816
		flasherflash4.RotX=-65
		flasherflash4.Height=185
		'spinner flasher position
		flasherflash5.Height=210
		Primitive21.image="VR_RampCentral"
		Primitive21.blenddisablelighting=0.1
		UpDownRamp.image="VR_RampCentral"
		UpDownRamp.blenddisablelighting=0.1
		Primitive23.image="VR_RampSides"
		Primitive23.blenddisablelighting=0.1
	Else
		flasherflash4.Y=1439
		flasherflash4.RotX=-45
		flasherflash4.Height=195
		Flasherflash5.Height=245
	End If
End Sub

'************************************************************************
'							KEYS
'************************************************************************

Sub Table1_KeyDown(ByVal Keycode)
	If keycode = keyFront Then Controller.Switch(23) = 1		'buy-in
	If KeyCode = PlungerKey Then Plunger.Pullback:SoundPlungerPull()
	If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter()
	If keycode = LeftFlipperKey Then 
			FlipperActivate LeftFlipper, LFPress
	End If
	If keycode = RightFlipperKey Then 
			FlipperActivate RightFlipper, RFPress 
	End If
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal Keycode)
	If keycode = keyFront Then Controller.Switch(23) = 0		'buy-in

	'If Keycode = PlungerKey Then Plunger.Fire:StopSound "fx_plungerpull":PlaySoundAt "fx_plunger",Plunger
	
	If KeyCode = PlungerKey Then
		Plunger.Fire
		If ballinlane=1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
	End If

	If keycode = LeftFlipperKey Then 
		FlipperDeActivate LeftFlipper, LFPress
	End If
	If keycode = RightFlipperKey Then 
		FlipperDeActivate RightFlipper, RFPress 
	End If
	If vpmKeyUp(keycode) Then Exit Sub
End Sub

Sub Table1_Paused:Controller.Pause = True:End Sub
Sub Table1_unPaused:Controller.Pause = False:End Sub
Sub Table1_exit():Controller.Pause = False:Controller.Stop:End Sub

'*****************************************
'      		General Illumination
'*****************************************
Set GiCallBack2 = GetRef("UpdateGi")

Dim GiOnState(2): GiOnState(0) = 1: GiOnState(1) = 1: GiOnState(2) = 1	'1 = unknown, forces first update
Dim LastColorGrade: LastColorGrade = ""

Sub UpdateGi(nr,step)
	Dim ii, xx, giOn, cg
	giOn = CInt(step > 0)
	Select Case nr

	Case 0 		'Left Playfield
	  If giOn <> GiOnState(0) Then
		GiOnState(0) = giOn
		If step=0 Then
			For each ii in GI_Left:ii.state=0:Next
			For each xx in layer1GI: xx.image = "layer1off": Next
			For each xx in layer2GI: xx.image = "layer2off": Next
			For each xx in layer3GI: xx.image = "layer3off": Next
			For each xx in layer4GI: xx.image = "layer4off": Next
			leftflipperp.image="williamsbatwhitereddim"
			If DesktopMode Then
			Else
				Primitive21.image="Ramp_Centraloff"
				UpDownRamp.image="Ramp_Centraloff"
				Primitive23.image="Ramp_Sidesoff"
			End if
		Else
			For each ii in GI_Left:ii.state=1:Next
			For each xx in layer1GI: xx.image = "layer1": Next
			For each xx in layer2GI: xx.image = "layer2": Next
			For each xx in layer3GI: xx.image = "layer3": Next
			For each xx in layer4GI: xx.image = "layer4": Next
			leftflipperp.image="williamsbatwhitereddim"
			If DesktopMode Then
			Else
				Primitive21.image="Ramp_Central"
				UpDownRamp.image="Ramp_Central"
				Primitive23.image="Ramp_Sides"
			End if
		End If
	  End If
		For each ii in GI_Left:ii.IntensityScale = 0.125 * step:Next
		PLAYFIELD_GI1.IntensityScale = 0.125 * step

	Case 1		'Right Playfield
	  If giOn <> GiOnState(1) Then
		GiOnState(1) = giOn
		If step=0 Then
			For each ii in GI_Right:ii.state=0:Next
			rightflipperp.image="williamsbatwhitereddim"
			plasticmag.image="plasticmagoff"
		Else
			For each ii in GI_Right:ii.state=1:Next
			rightflipperp.image="williamsbatwhitered"
			plasticmag.image="plasticmag"
		End If
	  End If
		For each ii in GI_Right:ii.IntensityScale = 0.125 * step:Next
		PLAYFIELD_GI2.IntensityScale = 0.125 * step

	Case 2		'Top Playfield
	  If giOn <> GiOnState(2) Then
		GiOnState(2) = giOn
		If step=0 Then
			For each ii in GI_Top:ii.state=0:Next
			layer5.image="chandlersoff"
		Else
			For each ii in GI_Top:ii.state=1:Next
			layer5.image="chandlers"
		End If
	  End If
		PLAYFIELD_GI3.IntensityScale = 0.125 * step
		For each ii in GI_Top:ii.IntensityScale = 0.125 * step:Next
		If Step>=7 Then cg = "ColorGrade_8" Else cg = "ColorGrade_" & (step+1)
		If cg <> LastColorGrade Then LastColorGrade = cg : Table1.ColorGradeImage = cg
		If step>4 Then DOF 103, DOFOn : Else DOF 103, DOFOff:End If

	Case 3		'Insert 1

	Case 4		'Insert 2

	End Select
End Sub

'*****************************************
' 			Lights Mapping
'*****************************************
Sub mapLights
	Dim obj
	For Each obj In Lamps
		set Lights(CInt(mid(obj.Name,2))) = obj
	Next
	Lights(16)= Array(l16,l16a,l16b)
	Lights(17)= Array(l17,l17a,l17b)
	Lights(18)= Array(l18,l18a,l18b)
End Sub

dim playfieldspotleft, playfieldspotright, bumperflash

Sub SpotLightsUpdate
	Dim l83v, l84v
	l84v = Controller.Lamp(84)
	l83v = Controller.Lamp(83)
	bumperflash = Controller.Lamp(18)
	L84.state = l84v
	L84a.Visible = l84v
	PlayfieldSpotRight = l84v
	L83.state = l83v
	L83a.Visible = l83v
	PlayfieldSpotLeft = l83v
	PLAYFIELD_SpotRight.visible = l84v
	PLAYFIELD_SpotLeft.visible = l83v
	PLAYFIELD_bumpers.visible = bumperflash		'was controller.LampCallback(18), not a Controller property
End Sub

Sub SpotLightTimer
If PlayfieldSpotLeft = 1 Then
If Playfield_SpotLeft.intensityscale < 1 Then
Playfield_SpotLeft.intensityscale = Playfield_SpotLeft.intensityscale + 0.25
End If
End If
If PlayfieldSpotLeft = 0 Then
If Playfield_SpotLeft.intensityscale > 0 Then
Playfield_SpotLeft.intensityscale = Playfield_SpotLeft.intensityscale - 0.25
End If
End If
If PlayfieldSpotRight = 1 Then
If Playfield_SpotRight.intensityscale < 1 Then
Playfield_SpotRight.intensityscale = Playfield_SpotRight.intensityscale + 0.25
End If
End If
If PlayfieldSpotRight = 0 Then
If Playfield_SpotRight.intensityscale > 0 Then
Playfield_SpotRight.intensityscale = Playfield_SpotRight.intensityscale - 0.25
End If
End If
If bumperflash = 1 Then
If Playfield_bumpers.intensityscale < 1 Then
Playfield_bumpers.intensityscale = Playfield_bumpers.intensityscale + 0.25
End If
End If
If bumperflash = 0 Then
If Playfield_bumpers.intensityscale > 0 Then
Playfield_bumpers.intensityscale = Playfield_bumpers.intensityscale - 0.25
End If
End If
End Sub

'*****************************************
'      		Solenoids Mapping
'*****************************************
SolCallback(1)= "SolRelease"					'1-Trough
SolCallback(2)= "SolAutoPlungerIM"			'2-AutoPlunger
SolCallback(3)= "SolLeftSaucer"			'	3-Left Lock Up
SolCallback(4)= "SolRightBackKick"			'4-Right Back Popper
SolCallback(5)= "SolRampDown"				'5-Ramp Down
'SolCallback(6)= ""							'6-N.U.
SolCallback(7)= "vpmSolSound SoundFX(""fx_knocker"",DOFKnocker),"		'7-Knocker
SolCallback(8)= "SolRightFrontKick" 		'8-Right Front Popper
'SolCallback(9)= ""							'9-Left Sling
'SolCallback(10)= ""						'10-Right Sling
'SolCallback(11)= ""						'11-Left Jet
'SolCallback(12)= ""						'12-Bottom Jet
'SolCallback(13)= ""						'13-Right Jet
SolCallback(14)= "SolPhoneFlasher"			'14-Flasher: Phone
'SolCallback(15)= ""						'15-N.U.
SolCallback(16)= "SolRampUp"				'16-Ramp Up
SolCallback(17)= "SolBackFlashers"			'17-Flasher: Back (x2)
SolCallback(18)= "FL18.State="				'18-Flasher: AutoFire
SolCallback(19)= "SolLowerLeftFlasher"		'19-Flasher: Lower Left
SolCallback(20)= "SolSpinnerFlasher"		'20-Flasher: Spinner
SolCallback(21)= "SolLowerRightFlasher"		'21-Flasher: Lower Right
SolCallback(22)= "SolBank"					'22-Motor: 3-Bank
'SolCallback(23)= ""						'23-Motor: Left Slot B
'SolCallback(24)= ""						'24-Motor: Left Slot A
'SolCallback(25)= ""						'25-Motor: Center Slot B
'SolCallback(26)= ""						'26-Motor: Center Slot A
'SolCallback(27)= ""						'27-Motor: Right Slot B
'SolCallback(28)= ""						'28-Motor: Right Slot A
'
SolCallback(36)= "SolPost"					'36-Up-Down Post

'******************************************************
'				NFOZZY'S FLIPPERS
'******************************************************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
 
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

	If abs(Flipper1.currentangle) < abs(Endangle1) + 3 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		If Flipper2.currentangle = EndAngle2 Then 
			BOT = GetBalls
				For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx /1.5
					BOT(b).vely = BOT(b).vely - 1
				end If
			Next
		End If
	Else 
		If abs(Flipper1.currentangle) > abs(Endangle1) + 30 then EOSNudge1 = 0
	End If
End Sub

'*************************************************
' Check ball distance from Flipper for Rem
'*************************************************

Const Pi = 3.1415927

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

Function Atn2(dy, dx)
	dim pi
	pi = 4*Atn(1)

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
Const EOSTnew = 0.8 	'1.0 FEOST
Const EOSAnew = 1		'0.2
Const EOSRampup = 0	'0.5 
Dim SOSRampup
	Select Case FlipperCoilRampupMode
		Case 0:
			SOSRampup = 2.5
		Case 1:
			SOSRampup = 8.5
	End Select
Const LiveCatch = 8
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST 						'new
	Flipper.eostorqueangle = EOSA 				'new
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST*EOSReturn/FReturn 	'EOST

	
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
	Dir = Flipper.startangle/Abs(Flipper.startangle)	'-1 for Right Flipper

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

'		if GameTime - FCount < LiveCatch Then
'			Flipper.Elasticity = LiveElasticity
'		elseif GameTime - FCount < LiveCatch * 2 Then
'			Flipper.Elasticity = 0.1
'		Else
'			Flipper.Elasticity = FElasticity
'		end if

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup			
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST						'EOST
			Flipper.eostorqueangle = EOSA				'EOSA
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
	Dim LiveCatchBounce															'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then						'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)	'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	End If
End Sub

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

'******************************************************
'				FLIPPER AND RUBBER CORRECTION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		'x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 0	'don't mess with these
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		'x.DebugOn = True : stickL.visible = True : tbpl.visible = True : vpmSolFlipsTEMP.DebugOn = True
		x.TimeDelay = 60
	Next

	'rf.report "Velocity"
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.1, 	1.07
	addpt "Velocity", 2, 0.2, 	1.15
	addpt "Velocity", 3, 0.3, 	1.25
	addpt "Velocity", 4, 0.41, 1.05
	addpt "Velocity", 5, 0.65, 	1.0'0.982
	addpt "Velocity", 6, 0.702, 0.968
	addpt "Velocity", 7, 0.95,  0.968
	addpt "Velocity", 8, 1.03, 	0.945

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

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

'Trigger Hit - .AddBall activeball
'Trigger UnHit - .PolarityCorrect activeball

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
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

	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
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
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
	
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
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)						'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
	 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
	
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'debug.print BallPos & " " & AddX & " " & Ycoef & " "& PartialFlipcoef & " "& VelCoef
				'playsound "fx_knocker"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class


'================================
'Helper Functions


Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
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
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub


Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

Function NullFunctionZ(aEnabled):End Function	'1 argument null function placeholder	 TODO move me or replac eme

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

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function


'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then		
 		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then		
 		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
End Sub

dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.935 '0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.935 '0.96
RubbersD.addpoint 2, 5.76, 0.942 '0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.75 '0.85

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
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		'playsound "fx_knocker"
		if debugOn then TBPout.text = str
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

'Tracks ball velocity for judging bounce calculations & angle
'apologies to JimmyFingers is this is what his script does. I know his tracks ball velocity too but idk how it works in particular
dim cor : set cor = New CoRTracker
cor.debugOn = False
'cor.update() - put this on a low interval timer
Class CoRTracker
	public DebugOn 'tbpIn.text
	public ballvel

	Private Sub Class_Initialize : redim ballvel(0) : End Sub 
	'TODO this would be better if it didn't do the sorting every ms, but instead every time it's pulled for COR stuff
	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs
		'if uBound(allballs) < 0 then if DebugOn then str = "no balls" : TBPin.text = str : exit Sub else exit sub end if: end if
		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
'			if DebugOn then 
'				dim s, bs 'debug spacer, ballspeed
'				bs = round(BallSpeed(b),1)
'				if bs < 10 then s = " " else s = "" end if
'				str = str & b.id & ": " & s & bs & vbnewline 
'				'str = str & b.id & ": " & s & bs & "z:" & b.z & vbnewline 
'			end if
		Next
		'if DebugOn then str = "ubound ballvels: " & ubound(ballvel) & vbnewline & str : if TBPin.text <> str then TBPin.text = str : end if
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

Sub RDampen_Timer()
	Cor.Update
End Sub


'************************************************************************
'						 AUTOPLUNGER 
'************************************************************************
Sub SolAutoPlungerIM(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
End Sub

'************************************************************************
'						3-BANK TARGETS
'************************************************************************
dim tbrake,tshake,tdir
Const targvel= 0.5			'the up-down speed

Sub TargetsInit
	'starts the bank in up position
    TargetBank.z=0
	tswbank.z=0
	sw66.Isdropped = 0:sw67.Isdropped = 0:sw68.Isdropped = 0:sw6X.IsDropped = 0
	Controller.Switch(11) = 0			
	Controller.Switch(73) = 1
	tdir=-1:tbrake=0:tshake=0
End Sub

Dim BankDisableTime: BankDisableTime=0

Sub SolBank(Enabled)
	If Enabled then
		PlayLoopSoundAtVol SoundFX("fx_mine_motor", DOFGear), TargetBank, 0.5
	else 
		StopSound "fx_mine_motor"
		BankDisableTime = 0
	End If
	BankMove.Enabled=Enabled
End Sub

Sub BankMove_timer()
	' Are we waiting for the ROM to disable the movement?  If yes, exit 
	if BankDisableTime > 0 and BankDisableTime > Timer Then Exit Sub
	TargetBank.z=TargetBank.z+targvel*tdir
	tswbank.z=tswbank.z+targvel*tdir
	If TargetBank.z<-60 AND tdir=-1 then
		Controller.Switch(11) = 1:Controller.Switch(73) = 0:TargetBank.z=-60:tswbank.z=-60
		sw66.Isdropped = 1:sw67.Isdropped = 1:sw68.Isdropped = 1:sw6X.IsDropped = 1:tdir=1
		BankDisableTime = Timer + 1
	End If
	If TargetBank.z>0 AND tdir=1 then
		Controller.Switch(73) = 1:Controller.Switch(11) = 0:TargetBank.z=0:tswbank.z=0
		sw66.Isdropped = 0:sw67.Isdropped = 0:sw68.Isdropped = 0:sw6X.IsDropped = 0:tdir=-1
		BankDisableTime = Timer + 1
	End if
End Sub

sub bankshake_timer()
	if tbrake>0 then			
		tshake=tshake+30
		tbrake=tbrake-0.08
		TargetBank.transy =((sin(tshake)))*tbrake
		tswbank.transy =((sin(tshake)))*tbrake
	else
		me.Enabled=0:tbrake=0:tshake=0
	End if
End Sub

'************************************************************************
'						UP-DOWN RAMP
'************************************************************************
Dim rdir
Const rampvel= 5			'the up-down speed

Sub RampInit
	'starts the ramp in up position
	UpDownRamp.rotX=0:UpDownRamp1.rotX=0
	UpRamp.collidable=0
	Controller.Switch(74) = 1
End Sub

Sub SolRampDown(Enabled)
	If Enabled then
		PlaySoundAt SoundFX("Taxi Ramp Down",DOFContactors), UpDownRamp
		rdir=-1:RampMove.Enabled=1:UpRamp.collidable=1
	End If
End Sub

Sub SolRampUp(Enabled)
	If Enabled then
		PlaySoundAt SoundFX("Taxi Ramp Up",DOFContactors), UpDownRamp
		rdir=1:RampMove.Enabled=1:UpRamp.collidable=0
	End If
End Sub

Sub RampMove_timer()
	UpDownRamp.rotX=UpDownRamp.rotX+rampvel*rdir
	UpDownRamp1.rotX=UpDownRamp.rotX
	If UpDownRamp.rotX<-15 and rdir=-1 then Controller.Switch(74) = 0:Me.Enabled=0:UpDownRamp.rotX=-15:UpDownRamp1.rotX=-15
	If UpDownRamp.rotX>0 and rdir=1 then Controller.Switch(74) = 1:Me.Enabled=0:UpDownRamp.rotX=0:UpDownRamp1.rotX=0
End Sub

'************************************************************************
'						UP-DOWN POST
'************************************************************************

Sub diverter_Init
	diverter.IsDropped = 1							'up-down post
End Sub

Sub SolPost(Enabled)
	if Enabled then
		diverter.isdropped=0:PlaySoundAt SoundFX("fx_solenoid",DOFContactors),diverterP:diverterP.transZ = 50
	else
		diverter.isdropped=1:PlaySoundAt SoundFX("fx_solenoid",DOFContactors),diverterP:diverterP.transZ = 5
	End if
End Sub

'************************************************************************
'						SLOT REEL
'************************************************************************
dim MechReelL, MechReelM, MechReelR, ReelSnd, Reel1Speed, Reel2speed, Reel3Speed
Reel1Speed=0:Reel2Speed=0:Reel3Speed=0:ReelSnd = 0

Sub UpdateReelSound
	If Reel1Speed + Reel2Speed + Reel3Speed > 0 Then
		If ReelSnd=0 Then PlayLoopSoundAtVol SoundFX("Reel Motor",DOFGear), Reel1, 0.25
		ReelSnd=Timer
	Else
		if ReelSnd > 0 and Timer > ReelSnd + .5 then StopSound "Reel Motor":ReelSnd=0 
	End If
End Sub

Sub ReelLPos(aCurrPos, aSpeed, aLastPos)
	Reel1Speed = abs(aSpeed)
	Reel1.RotX = (-aCurrPos) + 275
End Sub

Sub ReelMPos(aCurrPos, aSpeed, aLastPos)
	Reel2Speed = abs(aSpeed)
	Reel2.RotX = (-aCurrPos) + 275
End Sub

Sub ReelRPos(aCurrPos, aSpeed, aLastPos)
	Reel3Speed = abs(aSpeed)
	Reel3.RotX = (-aCurrPos) + 275
End Sub

Sub SlotInit
	Set MechReelL = New cvpmMyMech : With mechReelL
		.Sol1 = 23 : .Sol2 = 24
		.MType = vpmMechLinear + vpmMechCircle + vpmMechStepSol + vpmMechFast
		.Length = 200 : .Steps = 360
		.AddSw 12, 0, 8
		.CallBack = GetRef("ReelLPos")
		.Start
	End With

	Set MechReelM = New cvpmMyMech : With mechReelM
		.Sol1 = 25 : .Sol2 = 26
		.MType = vpmMechLinear + vpmMechCircle + vpmMechStepSol + vpmMechFast
		.Length = 200 : .Steps = 360
		.AddSw 25, 0, 8
		.CallBack = GetRef("ReelMPos")
		.Start
	End With

	Set MechReelR = New cvpmMyMech : With mechReelR
		.Sol1 = 27 : .Sol2 = 28
		.MType = vpmMechLinear + vpmMechCircle + vpmMechStepSol + vpmMechFast
		.Length = 200 : .Steps = 360
		.AddSw 48, 0, 8
		.CallBack = GetRef("ReelRPos")
		.Start
	End With
End Sub

'*****************************************
'      			Switches
'*****************************************
'******************************************************
'						TROUGH 
'******************************************************

Sub sw34_Hit():Controller.Switch(34) = 1:UpdateTrough:End Sub
Sub sw34_UnHit():Controller.Switch(34) = 0:UpdateTrough:End Sub
Sub sw33_Hit():Controller.Switch(33) = 1:UpdateTrough:End Sub
Sub sw33_UnHit():Controller.Switch(33) = 0:UpdateTrough:End Sub
Sub sw32_Hit():Controller.Switch(32) = 1:UpdateTrough:End Sub
Sub sw32_UnHit():Controller.Switch(32) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw32.BallCntOver = 0 Then sw33.kick 60, 9
	If sw33.BallCntOver = 0 Then sw34.kick 60, 9
	If sw34.BallCntOver = 0 Then sw35.kick 60, 20
	Me.Enabled = 0
End Sub

'******************************************************
'					LEFT SAUCER
'******************************************************

Sub sw51_hit
	PlaySoundAtBall "fx_saucerhit"
	Controller.switch(51)=1
End Sub

Sub SolLeftSaucer(enabled)
	controller.Switch(51) = 0
	If sw51.BallCntOver = 0 Then
		PlaySoundAt SoundFX("fx_Solenoid",DOFContactors), sw51
	Else
		PlaySoundAt SoundFX("fx_saucer_exit",DOFContactors), sw51
	End If
	sw51.kick 180,10,1
End Sub

'******************************************************
'					RIGHT BACK KICKER
'******************************************************

Sub sw43_hit
	PlaySoundAtBall "fx_saucerhit"
	Controller.switch(43)=1
End Sub

Sub SolRightBackKick(enabled)
	controller.Switch(43) = 0
	If sw43.BallCntOver = 0 Then
		PlaySoundAt SoundFX("fx_Solenoid",DOFContactors), sw43
	Else
		PlaySoundAt SoundFX("fx_saucer_exit",DOFContactors), sw43
	End If
	sw43.kick 0,45,1.56
End Sub

'******************************************************
'					RIGHT FRONT VUK
'******************************************************

sw57.timerinterval = 250

Sub sw57_hit
    PlaySoundAtBall "fx_saucerhit"
    Controller.switch(57)=1
    righthole.enabled=1
    centerhole.enabled=1
    If controller.Switch(44) = 0 then MoveBall
End Sub

Sub sw57_timer()
    MoveBall
    me.timerenabled = false
End Sub

Sub MoveBall()
    sw57.destroyball
    sw44.CreateSizedballWithMass Ballsize/2,Ballmass
    Controller.switch(57)=0
    Controller.switch(44)=1
End Sub

Sub SolRightFrontKick(enabled)
    controller.Switch(44) = 0
    If sw44.BallCntOver = 0 Then
        PlaySoundAt SoundFX("fx_Solenoid",DOFContactors), sw44
    Else
        PlaySoundAt SoundFX("fx_saucer_exit",DOFContactors), sw44
    End If
    sw44.kick 210,10
    If controller.Switch(57) = true then sw57.timerenabled = true
End Sub

'******************************************************
'					DRAIN & RELEASE
'******************************************************

Sub sw35_Hit() 'Drain
	UpdateTrough
	Controller.Switch(35) = 1
	RandomSoundDrain(sw35)
End Sub

Sub sw35_UnHit()  'Drain
	Controller.Switch(35) = 0
End Sub

Sub SolRelease(enabled)
	If enabled Then 
		If sw32.BallCntOver = 0 Then
			PlaySoundAt SoundFX("fx_Solenoid",DOFContactors), sw32
		Else
			RandomSoundBallRelease(sw32)
			vpmTimer.PulseSw 31
		End If
		sw32.kick 60, 9		
	End If
End Sub

dim ballinlane

'rollover
Sub sw15_hit:Controller.switch(15) = 1:ballinlane=1:End Sub
Sub sw15_unhit:Controller.switch(15) = 0:ballinlane=0:End Sub
Sub sw16_hit:Controller.switch(16) = 1:psw16.transz=-6:End Sub
Sub sw16_unhit:Controller.switch(16) = 0:psw16.transz=0:End Sub
Sub sw17_hit:Controller.switch(17) = 1:psw17.transz=-6:End Sub
Sub sw17_unhit:Controller.switch(17) = 0:psw17.transz=0:End Sub
Sub sw18_hit:Controller.switch(18) = 1:psw18.transz=-6:End Sub
Sub sw18_unhit:Controller.switch(18) = 0:psw18.transz=0:End Sub
Sub sw26_hit:Controller.switch(26) = 1:psw26.transz=-6:End Sub
Sub sw26_unhit:Controller.switch(26) = 0:psw26.transz=0:End Sub
Sub sw27_hit:Controller.switch(27) = 1:psw27.transz=-6:End Sub
Sub sw27_unhit:Controller.switch(27) = 0:psw27.transz=0:End Sub
Sub sw28_hit:Controller.switch(28) = 1:psw28.transz=-6:End Sub
Sub sw28_unhit:Controller.switch(28) = 0:psw28.transz=0:End Sub

'ramp optos
Sub sw36_hit:vpmtimer.pulsesw 36: End Sub
Sub sw37_hit:vpmtimer.pulsesw 37: End Sub

'subway optos
Dim SubwaySnd:SubwaySnd=0
Sub sw41_hit:controller.Switch(41)=1:If SubwaySnd=0 Then SubwaySnd=1:PlaySoundAt "fx_subway",ActiveBall:End If: End Sub
Sub sw41_unhit:controller.Switch(41)=0:End Sub	
Sub sw42_hit:controller.Switch(42)=1:If SubwaySnd=0 Then SubwaySnd=1:PlaySoundAt "fx_subway",ActiveBall:End If: End Sub
Sub sw42_unhit:controller.Switch(42)=0:End Sub
Sub sw47_hit:controller.Switch(47)=1:If SubwaySnd=0 Then SubwaySnd=1:PlaySoundAt "fx_subway",ActiveBall:End If: End Sub
Sub sw47_unhit:controller.Switch(47)=0:End Sub
Sub SubwayStop_Hit():StopSound "fx_subway":SubwaySnd=0:End Sub

'4-bank targets
Sub sw52_hit:vpmtimer.pulsesw 52:End Sub
Sub sw53_hit:vpmtimer.pulsesw 53:End Sub
Sub sw54_hit:vpmtimer.pulsesw 54:End Sub
Sub sw55_hit:vpmtimer.pulsesw 55:End Sub

'Mystery target
Sub sw56_hit:vpmtimer.pulsesw 56:End Sub

'Red target
Sub sw58_hit:vpmtimer.pulsesw 58:End Sub

'Slingshots
Dim LStep, RStep

Sub sw61_slingshot
	vpmTimer.PulseSw 61
	RandomSoundSlingshotLeft(sling1)
	LSling.Visible = 0: LSling2.Visible = 1: sling1.TransZ = -20: LStep = 0
	Me.TimerInterval = 50: Me.TimerEnabled = 1
End Sub

Sub sw62_slingshot
	vpmTimer.PulseSw 62
	RandomSoundSlingshotRight(sling2)
	RSling.Visible = 0: RSling2.Visible = 1: sling2.TransZ = -20: RStep = 0
	Me.TimerInterval = 50: Me.TimerEnabled = 1
End Sub

Sub sw61_Timer
	Select Case LStep
		Case 3:LSLing2.Visible = 0:LSLing1.Visible = 1:sling1.TransZ = -10
		Case 4:LSLing1.Visible = 0:LSLing.Visible = 1:sling1.TransZ = 0:Me.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

Sub sw62_Timer
	Select Case RStep
		Case 3:RSLing2.Visible = 0:RSLing1.Visible = 1:sling2.TransZ = -10
		Case 4:RSLing1.Visible = 0:RSLing.Visible = 1:sling2.TransZ = 0:Me.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

'Bumpers
Sub sw63_hit:vpmtimer.pulsesw 63:RandomSoundBumperTop(sw63):End Sub
Sub sw64_hit:vpmtimer.pulsesw 64:RandomSoundBumperMiddle(sw64):End Sub
Sub sw65_hit:vpmtimer.pulsesw 65:RandomSoundBumperBottom(sw65):End Sub

'3-bank drop targets
Sub sw66_hit:vpmtimer.pulsesw 66:bankshake.enabled=1:tbrake=5:PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw67_hit:vpmtimer.pulsesw 67:bankshake.enabled=1:tbrake=5:PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw68_hit:vpmtimer.pulsesw 68:bankshake.enabled=1:tbrake=5:PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub

'2-bank targets
Sub sw71_hit:vpmtimer.pulsesw 71:PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub
Sub sw72_hit:vpmtimer.pulsesw 72:PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub

'Scoop Roof
Sub sw75_hit:Controller.switch(75) = 1: PlaySoundAt "fx_sensor",ActiveBall: sw75p.RotZ=-35: Me.TimerInterval=100: Me.TimerEnabled=1: End Sub
Sub sw75_timer():Me.TimerEnabled=0:sw75p.RotZ=-70:End Sub
Sub sw75_unhit:Controller.switch(75) = 0: End Sub
Sub sw76_hit:Controller.switch(76) = 1: PlaySoundAt "fx_sensor",ActiveBall: sw76p.RotZ=25: Me.TimerInterval=100: Me.TimerEnabled=1: End Sub
Sub sw76_timer():Me.TimerEnabled=0:sw76p.RotZ=-10:End Sub
Sub sw76_unhit:Controller.switch(76) = 0: End Sub
Sub sw77_hit:Controller.switch(77) = 1: PlaySoundAt "fx_sensor",ActiveBall: sw77p.RotZ=35: Me.TimerInterval=100: Me.TimerEnabled=1: End Sub
Sub sw77_timer():Me.TimerEnabled=0:sw77p.RotZ=0:End Sub
Sub sw77_unhit:Controller.switch(77) = 0: End Sub

'Black target
Sub sw78_hit:vpmtimer.pulsesw 78: PlaySoundAt SoundFX("fx_target",DOFTargets), ActiveBall:End Sub

'Spinner
Sub sw115_spin:vpmtimer.pulsesw 115: PlaySoundAt "fx_spinner", sw115:End Sub

'******************************************************
'       	Flupper Flashers
'******************************************************

Dim FlashLevel1, Flashlevel2, FlashLevel3, FlashLevel4, FlashLevel5, FlashLevel6

FlasherLight1.IntensityScale = 0
Flasherlight2.IntensityScale = 0
Flasherlight3.IntensityScale = 0
Flasherlight4.IntensityScale = 0
Flasherlight5.IntensityScale = 0
Flasherlight6.IntensityScale = 0

'---- Playfield overlay limiter (see PFOverlayMode at top) ----
Dim PFOverlayCur : PFOverlayCur = Empty

Sub PFOverlayOn(obj)
	Select Case PFOverlayMode
		Case 0
			obj.visible = 0
		Case 1
			If IsObject(PFOverlayCur) Then
				If Not (PFOverlayCur Is obj) Then PFOverlayCur.visible = 0
			End If
			Set PFOverlayCur = obj
			obj.visible = 1
		Case Else
			obj.visible = 1
	End Select
End Sub

Sub PFOverlayOff(obj)
	obj.visible = 0
	If IsObject(PFOverlayCur) Then
		If PFOverlayCur Is obj Then PFOverlayCur = Empty
	End If
End Sub

Sub SolBackFlashers(enabled)
	If Enabled then
		Dim Intensity : If Light15.state = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
		FlashLevel1 = Intensity
		PFOverlayOn PLAYFIELD_flasher1
		FlasherFlash1.TimerEnabled=0
		Roulette.BlendDisableLightingFromBelow = 0
		FlasherFlash1.TimerEnabled=1
	end if
End Sub

Sub SolLowerLeftFlasher(enabled)
	if Enabled then 
		Dim Intensity : If Light15.state = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
		FlashLevel3 = Intensity
		PFOverlayOn PLAYFIELD_flasher3
		FlasherFlash3_Timer
	end if
End Sub

Sub SolLowerRightFlasher(enabled)
	if Enabled then 
		Dim Intensity : If Light15.state = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
		FlashLevel4 = Intensity
		PFOverlayOn PLAYFIELD_flasher4
		FlasherFlash4_Timer
	end if
End Sub

Sub SolSpinnerFlasher(enabled)
	if Enabled then 
		Dim Intensity : If Light15.state = 0 then Intensity = FlasherIntensityGIOff else Intensity = FlasherIntensityGIOn
		FlashLevel5 = Intensity
		PFOverlayOn PLAYFIELD_flasher5
		FlasherFlash5_Timer
	end if
End Sub

Sub SolPhoneFlasher(enabled)
	If enabled Then
		FlashLevel6=1
		PFOverlayOn PLAYFIELD_flasher6
		FlasherFlash6_Timer
	End If
End Sub

Sub FlasherFlash1_Timer()
	dim flashx3, matdim
	FlasherFlash1.visible = 1
	FlasherFlash2.visible = 1
	FlasherFlash1a.visible = 1:FlasherFlash2a.visible = 1
	FlasherLit1.visible = 1
	FlasherLit2.visible = 1
	flashx3 = FlashLevel1 * FlashLevel1 * FlashLevel1
	Flasherflash1.opacity = 200 * flashx3
	Flasherflash2.opacity = 200 * flashx3
	Flasherflash1a.opacity = 250 * flashx3:Flasherflash2a.opacity = 500 * flashx3
	PLAYFIELD_flasher1.opacity = 300 * flashx3
	FlasherLit1.BlendDisableLighting = 10 * flashx3
	FlasherLit2.BlendDisableLighting = 10 * flashx3
	If Flasherbase1.BlendDisableLighting > 0.4 Then Flasherbase1.BlendDisableLighting =  flashx3
	If Flasherbase2.BlendDisableLighting > 0.4 Then Flasherbase2.BlendDisableLighting =  flashx3
	FlasherLight1.IntensityScale = flashx3
	FlasherLight2.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel1)
	FlasherLit1.material = "domelit" & matdim
	FlasherLit2.material = "domelit" & matdim
	FlashLevel1 = FlashLevel1 * 0.85 - 0.01
	If FlashLevel1 < 0.8 Then Roulette.BlendDisableLightingFromBelow = 1
	If FlashLevel1 < 0.15 Then
		FlasherLit1.visible = 0
		FlasherLit2.visible = 0
	Else
		FlasherLit1.visible = 1
		FlasherLit2.visible = 1
	end If
	If FlashLevel1 < 0 Then
		FlasherFlash1.TimerEnabled = False
		FlasherFlash1.visible = 0
		FlasherFlash2.visible = 0
		FlasherFlash1a.visible = 0:FlasherFlash2a.visible = 0
		PFOverlayOff PLAYFIELD_flasher1
		Roulette.BlendDisableLightingFromBelow=1
	End If
End Sub

Sub FlasherFlash3_Timer()
	dim flashx3, matdim
	If not Flasherflash3.TimerEnabled Then 
		Flasherflash3.TimerEnabled = True
		Flasherflash3.visible = 1
		Flasherlit3.visible = 1
	End If
	flashx3 = FlashLevel3 * FlashLevel3 * FlashLevel3
	Flasherflash3.opacity = 200 * flashx3
	PLAYFIELD_flasher3.opacity = 300 * flashx3
	Flasherlit3.BlendDisableLighting = 10 * flashx3
	Flasherbase3.BlendDisableLighting =  flashx3
	Flasherlight3.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel3)
	Flasherlit3.material = "domelit" & matdim
	FlashLevel3 = FlashLevel3 * 0.85 - 0.01
	If FlashLevel3 < 0.15 Then
		Flasherlit3.visible = 0
	Else
		Flasherlit3.visible = 1
	end If
	If FlashLevel3 < 0 Then
		Flasherflash3.TimerEnabled = False
		Flasherflash3.visible = 0
		PFOverlayOff PLAYFIELD_flasher3
	End If
End Sub

Sub FlasherFlash4_Timer()
	dim flashx3, matdim
	If not Flasherflash4.TimerEnabled Then 
		Flasherflash4.TimerEnabled = True
		Flasherflash4.visible = 1
		Flasherlit4.visible = 1
	End If
	flashx3 = FlashLevel4 * FlashLevel4 * FlashLevel4
	Flasherflash4.opacity = 200 * flashx3
	PLAYFIELD_flasher4.opacity = 300 * flashx3
	Flasherlit4.BlendDisableLighting = 10 * flashx3
	Flasherbase4.BlendDisableLighting =  flashx3
	Flasherlight4.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel3)
	Flasherlit4.material = "domelit" & matdim
	FlashLevel4 = FlashLevel4 * 0.85 - 0.01
	If FlashLevel4 < 0.15 Then
		Flasherlit4.visible = 0
	Else
		Flasherlit4.visible = 1
	end If
	If FlashLevel4 < 0 Then
		Flasherflash4.TimerEnabled = False
		Flasherflash4.visible = 0
		PFOverlayOff PLAYFIELD_flasher4
	End If
End Sub

Sub FlasherFlash5_Timer()
	dim flashx3, matdim
	If not Flasherflash5.TimerEnabled Then 
		Flasherflash5.TimerEnabled = True
		Flasherflash5.visible = 1
		Flasherlit5.visible = 1
	End If
	flashx3 = FlashLevel5 * FlashLevel5 * FlashLevel5
	Flasherflash5.opacity = 400 * flashx3
	PLAYFIELD_flasher5.opacity = 300 * flashx3
	Flasherlit5.BlendDisableLighting = 10 * flashx3
	Flasherbase5.BlendDisableLighting =  flashx3
	Flasherlight5.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel3)
	Flasherlit5.material = "domelit" & matdim
	FlashLevel5 = FlashLevel5 * 0.85 - 0.01
	If FlashLevel5 < 0.15 Then
		Flasherlit5.visible = 0
	Else
		Flasherlit5.visible = 1
	end If
	If FlashLevel5 < 0 Then
		Flasherflash5.TimerEnabled = False
		Flasherflash5.visible = 0
		PFOverlayOff PLAYFIELD_flasher5
	End If
End Sub

Sub FlasherFlash6_Timer()
	dim flashx3
	If not FlasherFlash6.TimerEnabled Then 
		FlasherFlash6.TimerEnabled = True
		FlasherFlash6.visible = 1
	End If
	flashx3 = FlashLevel6 * FlashLevel6 * FlashLevel6
	FlasherFlash6.opacity = 600 * flashx3
	PLAYFIELD_flasher6.opacity = 170 * flashx3
	Flasherlight6.IntensityScale = flashx3
	FlashLevel6 = FlashLevel6 * 0.85 - 0.01
	If FlashLevel6 < 0 Then
		FlasherFlash6.TimerEnabled = False
		FlasherFlash6.visible = 0
		PFOverlayOff PLAYFIELD_flasher6
	End If
End Sub

'******************************************************
'       	RealTime Updates
'******************************************************
Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates()
	LeftFlipperP.RotZ = LeftFlipper.currentangle
	RightFlipperP.RotZ = RightFlipper.currentangle
	LeftFlipperSh.RotZ = LeftFlipper.currentangle
	RightFlipperSh.RotZ = RightFlipper.currentangle
	ScoopGate.RotX = Gate4.currentangle/2
	sw115p.RotX = -15*ABS(cos (sw115.currentangle*3.14/360))
	PinCab_Shooter.Y = -216 + (5* Plunger.Position) -20
	RollingSound
	BallShadowUpdate
	SpotLightsUpdate
	UpdateReelSound
End Sub

' *********************************************************************
'					Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

function AudioFade(ball)
	Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function RndNum(min,max)
 RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min and max
End Function

'Set position as table object (Use object or light but NOT wall) and Vol to 1
Sub PlaySoundAt(sound, tableobj)
	PlaySound sound, 1, 1, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.
Sub PlaySoundAtBall(sound)
	PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as table object and Vol manually.
Sub PlaySoundAtVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, Pan(tableobj), 0, 0, 0, 1, AudioFade(tableobj)
End Sub

Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, Pan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3
Sub PlaySoundAtBallVol(sound, VolMult)
	PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.
Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Dim NextOrbitHit:NextOrbitHit = 0
Sub PlasticRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 10, -20000
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .1 + (Rnd * .2)
	end if 
End Sub

' Requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
	PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' *********************************************************************
' 						Other Sound FX
' *********************************************************************

Sub OnBallBallCollision(ball1, ball2, velocity)
	if ball1.z >= 0 then 
		PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 500, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
	Else ' in playfield tunnel, muffled!
		PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 1000, Pan(ball1), 0, -20000, 0, 0, AudioFade(ball1)
	end if 
End Sub

'Sub LeftFlipper_Collide(parm)
 ''   RandomSoundFlipper
'End Sub

'Sub Rightflipper_Collide(parm)
' '   RandomSoundFlipper
'End Sub

Sub Pins_Hit(idx)
	 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then		
 		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
End Sub

Sub Posts_Hit(idx)
	 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then		
 		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
End Sub

'Sub Rubbers_Hit(idx)
''	RandomSoundRubber()
'End Sub

'Sub Gates_Hit(idx)
''	PlaySoundAtBallVol "gate4",1
'End Sub

Sub RandomSoundRubber()
	Select Case RndNum(1,3)
		Case 1 : PlaySoundAtBallVol "fx_rubber_hit_1",10
		Case 2 : PlaySoundAtBallVol "fx_rubber_hit_2",10
		Case 3 : PlaySoundAtBallVol "fx_rubber_hit_3",10
	End Select
End Sub

Sub RandomSoundFlipper()
	Select Case RndNum(1,3)
		Case 1 : PlaySoundAtBallVol "flip_hit_1", 20
		Case 2 : PlaySoundAtBallVol "flip_hit_2", 20
		Case 3 : PlaySoundAtBallVol "flip_hit_3", 20
	End Select
End Sub

Sub LeftHole_hit:PlaySoundAt "fx_hole3", ActiveBall:centerhole.enabled=0:righthole.enabled=0:End Sub
Sub RightHole_hit:PlaySoundAt "fx_hole2", ActiveBall:End Sub
Sub CenterHole_hit:PlaySoundAt "fx_hole1", ActiveBall:righthole.enabled=0:End Sub
Sub Trigger1_hit:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub Trigger1_timer:Me.TimerEnabled=0:PlaysoundAt "fx_BallDrop",Trigger1:End Sub
Sub Trigger2_hit:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub Trigger2_timer:Me.TimerEnabled=0:PlaysoundAt "fx_BallDrop",Trigger2:End Sub
Sub Trigger3_hit:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub Trigger3_timer:Me.TimerEnabled=0:PlaysoundAt "fx_BallDrop",Trigger3:End Sub
Sub Trigger4_hit:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub Trigger4_timer:Me.TimerEnabled=0:PlaysoundAt "fx_BallDrop",Trigger4:End Sub

'******************************************************
'		BALL ROLLING AND DROP SOUNDS
'******************************************************

Const tnob = 4 ' total number of balls
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

Sub RollingSound()
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		'***Ball Drop Sounds***
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
	Next
End Sub

' *********************************************************************
'						BALL SHADOW
' *********************************************************************
ReDim BallShadow(tnob-1)
InitBallShadow

Sub InitBallShadow
	Dim i: For i=0 to tnob-1
		ExecuteGlobal "Set BallShadow(" & i & ")=BallShadow" & (i+1) & ":"
	Next
End Sub

Sub BallShadowUpdate()
    Dim BOT, b
    BOT = GetBalls
	' hide shadow of deleted balls
	If UBound(BOT)<(tnob-1) Then
		For b = (UBound(BOT) + 1) to (tnob-1)
			BallShadow(b).visible = 0
		Next
	End If
	' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
	' render the shadow for each ball
    For b = 0 to UBound(BOT)
		If BOT(b).X < Table1.Width/2 Then
			BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/10)) + 10
		Else
			BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/10)) - 10
		End If
	    ballShadow(b).Y = BOT(b).Y + 20
		If BOT(b).Z > 20 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
End Sub


' *****  cvpmMech replacement, all because the "Fast" parameter is not set on timer update!!   We can't get smooth reels without this.   Until this is addressed in Core.vbs, replacing here.

Class cvpmMyMech
	Public Sol1, Sol2, MType, Length, Steps, Acc, Ret
	Private mMechNo, mNextSw, mSw(), mLastPos, mLastSpeed, mCallback

	Private Sub Class_Initialize
		ReDim mSw(10)
		gNextMechNo = gNextMechNo + 1 : mMechNo = gNextMechNo : mNextSw = 0 : mLastPos = 0 : mLastSpeed = 0
		MType = 0 : Length = 0 : Steps = 0 : Acc = 0 : Ret = 0 : vpmTimer.addResetObj Me
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

'/////////////////////////////////////////
''		Begin Fleep Sound Package'
'/////////////////////////////////////////'

'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

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


' *********************************************************************
'                     Fleep  Supporting Ball & Sound Functions
' *********************************************************************

Dim tablewidth, tableheight : tablewidth = table1.width : tableheight = table1.height

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / tableheight-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1
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

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01", DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
		PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01", DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
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
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*8)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then
 		RandomSoundRubberStrong 1 
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
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
		RandomSoundBottomArchBallGuide
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


'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////


'******************************************************
'					KNOCKER
'******************************************************
SolCallback(6)	= "SolKnocker" 'Change the solenoid number to the correct number for your table.

Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid 'Add knocker position object
	End If
End Sub

'******************************************************
'					Saucer Kick
'******************************************************

Sub SolSaucer(Enabled)
	If Enabled Then
		If BallInSaucer = True Then
			SoundSaucerKick 1, Kicker
		Else
			SoundSaucerKick 0, Kicker
		End If
	End If
End Sub

DIM VRThings
If VRRoom > 0 Then
	ScoreText.visible = 0
	PinCab_Backglass.blenddisablelighting = 5	
	If VRRoom = 1 Then
		for each VRThings in VRStuff:VRThings.visible = 1:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		PinCab_Backglass.visible = 1
		PinCab_Backbox.visible = 1
		DMD.visible = 1
	End If
Else
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		If DesktopMode then ScoreText.visible = 1:PinCab_Rails.visible = 1 else ScoreText.visible = 0:PinCab_Rails.visible=0 End If
End if