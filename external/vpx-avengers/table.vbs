'*********Hanibal 4k Mod *****************
'********* Avengers (Stern 2012) by GTXJOE
'********* VPX version by sliderpoint
' VP10.7x V1.48 by TastyWasps - VPW Enhancements - July 2023
' ----------------------------------------------------------
' nFozzy Physics, Fleep Sounds, Flupper Domes, Playfield Mesh, Hybrid VR Room
' LUT Selector, Dynamic Ball Shadows, Gameplay Balance, Upscaled Images

Option Explicit
Randomize

Const BallSize = 50
Const BallMass = 1

'************************************************************************
' 							Table Options
'************************************************************************
SideBladeMod = 1					' 0 = Black Side Blades , 1 = Custom Blades, 2 = Alt Custom Blades
InstMod = 0							' 0 = Stern Intruction Cards, 1 = French Intruction Cards, 2 = German Intruction Cards
GlassMod = 0						' 0 = Scratched Glass Off, 1 = Scratched Glass On

'----- Bouncer Options -----
Const TargetBouncerEnabled = 1	  	' 0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor  = 0.7  	' Level of bounces. Recommmended value of 0.7

'----- Volume Options -----
Const VolumeDial = 1			  	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.8		  	' Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.8		  	' Level of ramp rolling volume. Value between 0 and 1

'----- Shadow Options -----			' If performance is suffering, turn off Dynamic Shadow options below as they are subtle.
Const DynamicBallShadowsOn = 0		' 0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 0		' 0 = Static shadow under ball ("flasher" image, like JP's)
									' 1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									' 2 = flasher image shadow, but it moves like ninuzzu's
' Controller Setting
Const cController = 1				' Set to 1=VPinMAME, 2=UVP backglass server, 3=B2S backglass server

' Optional Desktop Backgrounds located in Image Manager under "Desktop_" images.

''************************************************************************
'							VR Options
'*************************************************************************
DMDDecalmod	= 0						' 0 = VR Stern Standard DMD Decal, 1 = Custom DMD Decal		
LogoMod = 0							' 0 = VR Logo Off, 1 = VR Logo On
PosterMod = 0						' 0 = VR Poster Off, 1 = VR Poster On
TopperMod = 0						' 0 = VR Tournament Topper Off, 1 = Tournament Topper On, 2 Custom Topper On
AltBackglass = 0					' 0 = Standard, 1 = Hauntfreaks

Dim UseVPMModSol
UseVPMModSol = True

Dim PWMflashers
PWMflashers = True 

' Desktop Auto Detect for Cab Sides
If Table1.ShowDT = False Then PinCab_Rails.Visible = False

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

LoadVPM "01560000", "sam.VBS", 3.10

Sub LoadVPM(VPMver, VBSfile, VBSver)
	On Error Resume Next
	If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
	ExecuteGlobal GetTextFile(VBSfile)
	If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description

	Select Case cController
		Case 1
			Set Controller = CreateObject("VPinMAME.Controller")
			If Err Then MsgBox "Can't Load VPinMAME." & vbNewLine & Err.Description
			If VPMver>"" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required."
			If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
		Case 2
			Set Controller = CreateObject("UltraVP.BackglassServ")
		Case 3
			Set Controller = CreateObject("B2S.Server")
	End Select

	On Error Goto 0
End Sub

'********************
' Standard definitions
'********************
Const cGameName = "avs_170"
'Const cGameName = "avs_170c" 'Colour Rom

Const UseSolenoids = 1
Const UseLamps = 1
Const UseSync = 1
Const HandleMech = 0
Const tnob = 6

Dim tablewidth, tableheight
tablewidth = Table1.width
tableheight = Table1.height

' Standard Sounds
Const SSolenoidOn = "fx_solenoid"
Const SSolenoidOff = ""
Const SCoin = "Coin_In"

' Auto Detect for PWM Flashers
If UseVPMModSol = True Then
	If PWMflashers And Controller.Version < "03060000" Then 
		PWMflashers = False
	Else
		UseVPMModSol = cBool(PWMflashers)
	End If
End If

' 1001 to 1032 for solenoids, 1201 to 1025 for GIs (WPC only), 1301+ for lamps (not yet implemented)
Const VPM_MODOUT_DEFAULT              =   0 ' Uses default driver modulated solenoid implementation
Const VPM_MODOUT_PWM_RATIO            =   1 ' pulse ratio over the last integration period, allow (approximated) device emulation by the calling app
Const VPM_MODOUT_BULB_44_6_3V_AC      = 100 ' Incandescent #44/555 Bulb connected to 6.3V, commonly used for GI
Const VPM_MODOUT_BULB_47_6_3V_AC      = 101 ' Incandescent #47 Bulb connected to 6.3V, commonly used for (darker) GI with less heat
Const VPM_MODOUT_BULB_86_6_3V_AC      = 102 ' Incandescent #86 Bulb connected to 6.3V, seldom use: TZ, CFTBL,...
Const VPM_MODOUT_BULB_44_18V_DC_WPC   = 201 ' Incandescent #44/555 Bulb connected to 18V, commonly used for lamp matrix with short strobing
Const VPM_MODOUT_BULB_44_18V_DC_GTS3  = 202 ' Incandescent #44/555 Bulb connected to 18V, commonly used for lamp matrix with short strobing
Const VPM_MODOUT_BULB_44_18V_DC_S11   = 203 ' Incandescent #44/555 Bulb connected to 18V, commonly used for lamp matrix with short strobing
Const VPM_MODOUT_BULB_89_20V_DC_WPC   = 301 ' Incandescent #89/906 Bulb connected to 12V, commonly used for flashers
Const VPM_MODOUT_BULB_89_20V_DC_GTS3  = 302 ' Incandescent #89/906 Bulb connected to 12V, commonly used for flashers
Const VPM_MODOUT_BULB_89_32V_DC_S11   = 303 ' Incandescent #89/906 Bulb connected to 32V, used for flashers on S11 with output strobing
Const VPM_MODOUT_LED                  = 400 ' LED PWM (in fact mostly human eye reaction, since LED are nearly instantaneous)

Sub InitPWM() ' called from Table_Init
	dim BulbType : BulbType = VPM_MODOUT_BULB_89_20V_DC_WPC
	Controller.SolMask(1017) = BulbType
	Controller.SolMask(1018) = BulbType 
	Controller.SolMask(1019) = BulbType
	Controller.SolMask(1020) = BulbType
	Controller.SolMask(1021) = BulbType
	Controller.SolMask(1023) = BulbType 
	Controller.SolMask(1025) = BulbType
	Controller.SolMask(1026) = BulbType 
	Controller.SolMask(1027) = BulbType 
	Controller.SolMask(1028) = BulbType
	Controller.SolMask(1029) = BulbType
	Controller.SolMask(1031) = BulbType
	Controller.SolMask(1032) = BulbType
End Sub

' Variables
Dim bsL,mHulkMag,tbTrough,dtBank,Bump1, Bump2, Bump3, bsRHole, MsLHole, bsHole, bsTrough, visibleLock ,bsSaucer, mDiverter, LCHit
Dim PlungerIM

Dim DesktopMode: DesktopMode = Table1.ShowDT

' SpinKicker
Dim SpBall(1), sCntrX, sCntrY, PiTess, sDegs, sRad, sVel, bipJump, sPostRadius

' VR Room Auto-Detect
Dim VR_Obj, VR_Room

If RenderingMode = 2 Then
	VR_Room = 1
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 1 : Next
	For Each VR_Obj in VRRoom : VR_Obj.Visible = 1 : Next

	If LogoMod = 1 Then
		VR_Room_Logo.Visible = True
	Else
		VR_Room_Logo.Visible = False
	End If

	If PosterMod = 1 Then
		VR_Room_Poster.Visible = True
	Else
		VR_Room_Poster.Visible = False
	End If

	If TopperMod = 1 Then
		PinCab_TTopper.Visible = True
	Else
		PinCab_TTopper.Visible = False
	End If

	If TopperMod = 2 Then
		PinCab_TTopper_Cust_ft.Visible = True
		PinCab_TTopper_Cust_bk.Visible = True
		PinCab_TTopper_Cust_Cube.Visible = True
	Else
		PinCab_TTopper_Cust_ft.Visible = False
		PinCab_TTopper_Cust_bk.Visible = False
		PinCab_TTopper_Cust_Cube.Visible = False
	End If

	If AltBackglass = 1 Then
		PinCab_Backglass.Image = "backglass_hauntfreaks"
	End If

Else
	VR_Room = 0
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VRRoom : VR_Obj.Visible = 0 : Next
End If

Dim SideBladeMod, GlassMod,DMDDecalmod,InstMod, LogoMod, PosterMod, AltBackglass, TopperMod, x

If SideBladeMod = 0 Then
	PinCab_Blades.image = "PinCab_Blades"
End If

If SideBladeMod = 1 Then
	PinCab_Blades.image = "PinCab_Blades_Cust"
End if

If SideBladeMod = 2 Then
	PinCab_Blades.image = "PinCab_Blades_Cust1"
End If

If GlassMod = 1 Then
	PinCab_Glass_scratches.Visible = True
Else
	PinCab_Glass_scratches.Visible = False
End If

If InstMod = 0 Then
	CardL.image = "Inst_Left"
	CardR.image = "Inst_Right"
End If

If InstMod = 1 Then
	CardL.image = "Inst_Left_French"
	CardR.image = "Inst_Right"
End If

If InstMod = 2 Then
	CardL.image = "Inst_Left_White_German"
	CardR.image = "Inst_Right_White"
End If

If DMDDecalmod = 1 Then
	Pincab_DMD_Decal.image = "Pincab_DMD_Decal_cust"
Else
	Pincab_DMD_Decal.image = "Pincab_DMD_Decal"
End If


' Table Init
Sub Table1_Init
	' Spinning Tesseract Toy
	sPostRadius = 14
	PiTess = Round(4*Atn(1),6)		'3.14159
	sCntrX = CubeB.x
	sCntrY = CubeB.y
	sRad = sCntrX - 451	'spinner center - left post center
	sVel = 0

	Set SpBall(0) = SpinKicker.CreateSizedBall(sPostRadius)
	SpBall(0).x = 451
 	SpBall(0).y = 801
 	'SpBall(0).z = 25
	SpBall(0).Mass = 0.3
	SpBall(0).color = RGB(0,0,0)
	SpinKicker.Kick 0,0,0

	Set SpBall(1) = SpinKicker.CreateSizedBall(sPostRadius)
	SpBall(1).x = 571
 	SpBall(1).y = 801
	SpBall(1).Mass = 0.3
	SpBall(1).color = RGB(0,0,0)
	SpinKicker.Kick 0,0,0
	' End Spinning Tesseract Toy 

	vpmInit Me
	With Controller
        .GameName = cGameName
        .SplashInfoLine = "Avengers Pro Stern 2012"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 1
		.Hidden = 0

		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
	End With

    On Error Goto 0

    '*** Trough
    Set bsTrough = New cvpmBallStack
    bsTrough.InitSw 0, 21, 20, 19, 18, 0, 0, 0
    bsTrough.InitKick BallRelease, 80	, 8
    bsTrough.InitExitSnd "fx_ballrel", "fx_solenoid"
    bsTrough.Balls = 4

	' Loki Lock Init - Optos used so switch is opposite (1=no ball, 0=ball).  Default to 1
	Controller.Switch(51) = 1
	Controller.Switch(50) = 1
	Controller.Switch(49) = 1
	Post51.IsDropped = 1 
	Post50.IsDropped = 1
	Post49.IsDropped = 0 

	' Drop Targets
	Set dtBank = New cvpmDropTarget ' HULK
	dtBank.InitDrop Array(Sw52,Sw53,Sw54,Sw55),Array(52,53,54,55) 'Walls<->Switch No.
	dtBank.InitSnd "droptarget","droptargetreset"

	RampControlGate.IsDropped = 1

	' Impulse Plunger
	Const IMPowerSetting = 55
	Const IMTime = 0.6
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP swplunger, IMPowerSetting, IMTime
		.Random .5
		.InitExitSnd "fx_plunger2", "fx_plunger"
		.CreateEvents "plungerIM"
	End With

    ' Hulk Eject
    Set bsHole = New cvpmSaucer
        bsHole.InitKicker HulkEject, 62, 25, 15, 2
        bsHole.InitSounds "kicker_enter_left","fx_solenoid","popper_ball"

	Set mHulkMag= New cvpmMagnet
 	With mHulkMag
		.InitMagnet HulkMag, 16  
		.GrabCenter = True	
 		.solenoid=23
		.CreateEvents "mHulkMag"
	End With

	' Main Timer Init
	PinMAMETimer.Enabled = 1

	'Nudging
    vpmNudge.TiltSwitch = -7
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(Bumper1b,Bumper2b,Bumper3b,LeftSlingshot,RightSlingshot)

	vpmMapLights AllLights

	DayNight = table1.NightDay
	Intensity ' Sets GI brightness depending on day/night slider settings

	' Initialize beta PWM
	If UseVPMModSol And PWMflashers Then InitPWM()

	' Fast Flips coding for Stern SAM
	On Error Resume Next 
	InitVpmFFlipsSAM
	If Err Then MsgBox "You need the latest sam.vbs in order to run this table, available with vp10.5 rev3434"
	On Error Goto 0

End Sub

' Spinning Tesseract Toy
Sub SpinKicker_Timer
	'Immobilize the spinner balls, neg Y velocity offset reflects the timer interval
	SpBall(0).vely = -0.01
 	SpBall(1).vely = -0.01
 	SpBall(0).velx = 0		
 	SpBall(1).velx = 0
 	SpBall(0).velz = 0.01	
 	SpBall(1).velz = 0.01
 	SpBall(0).z = 25		'sPostRadius
 	SpBall(1).z = 25		'sPostRadius
	sDegs = CubeB.RotZ 										
	SpBall(0).x = sRad*cos(sDegs*(PI/180)) + sCntrX	'Place spinner balls to follow 3D CubeB
	SpBall(0).y = sRad*sin(sDegs*(PI/180)) + sCntrY
	SpBall(1).y = sCntrY - (SpBall(0).y - sCntrY)			'Reverse clone ball(0) movement for ball(1)
	SpBall(1).x = sCntrX - (SpBall(0).x - sCntrX)
	If sVel > 7 Then  sVel = 7 End If
	If sVel < -7 Then  sVel = -7 End If
	If sVel > .007 Then 
		sVel = sVel - .007
	ElseIf sVel < -.007 Then 
		sVel = Round(sVel + .007, 3)
	Else 
		sVel = 0
	End If

	sVel = Round(sVel, 3)
	If CubeB.RotZ <= 0 Then CubeB.RotZ = 360':If cController = 3 Then Controller.B2SSetData 101, 1
	If CubeB.RotZ > 360 Then CubeB.RotZ = 1
		CubeB.RotZ = CubeB.RotZ + sVel
		Cube.RotZ = CubeB.RotZ
		tessbase.RotZ = CubeB.RotZ
	If bipJump > 0 then bipJump = bipJump -1: ballnplay.velz = 0: Else bipJump = 0:End If
'	If cController = 3 Then Controller.B2SSetData 101, 0
'        If lampLastPos >= 0 Then ' not first time
'            colLampPoles(lampLastPos).IsDropped = True
'            colLampPoles2(lampLastPos).IsDropped = True

'****			If cController = 3 Then  101, 1

'			cube.ObjRotZ = -curPos*10
'			cubeb.ObjRotZ = -curPos*10
'			tessbase.ObjRotZ = -curPos*10

'        End If
'		If cController = 3 Then Controller.B2SSetData 101, 0

End Sub

Dim BallnPlay, SpinBall, SpRadian, sCollisionRadn, spRadiangle, RotAdj, PiFilln

Sub OnBallBallCollision(ball1, ball2, velocity)
	' Determine which ball is which, the Spinner Ball or the Ball-in-play
	If ball1.ID = SpBall(0).ID then				'SpBall(0)'s table ID is 0
		Set SpinBall = SpBall(0)'or ball1		'Set spinner ball to = ball1
		Set BallnPlay = ball2						'Set ball-in-play to = ball2
		RotAdj = abs(CubeB.RotZ-360)		'* Take a sample of the primitive angle and adjust it's reading for calculations *
	ElseIf ball1.ID = SpBall(1).ID then
		Set SpinBall = SpBall(1)
		Set BallnPlay = ball2
		RotAdj = abs(CubeB.RotZ-180)		'*
		If abs(CubeB.RotZ-360) < 180 Then RotAdj = abs(CubeB.RotZ-360) + 180		'*
	ElseIf ball2.ID = SpBall(0).ID then
		Set SpinBall = SpBall(0)
		Set BallnPlay = ball1
		RotAdj = abs(CubeB.RotZ-360)		'*
	ElseIf ball2.ID = SpBall(1).ID then
		Set SpinBall = SpBall(1)
		Set BallnPlay = ball1
		RotAdj = abs(CubeB.RotZ-180)		'*
		If abs(CubeB.RotZ-360) < 180 Then RotAdj = abs(CubeB.RotZ-360) + 180		'*
	Else
		PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
		Exit Sub	'Incase of multi-ball or any other non spinner ball collision, exit sub
	End If
	'If you want to effect the ball-in-play after the collision put it here
	bipJump = 2	'BallInPlay.velz = 0
'		PiFilln accounts for pos/neg values in the collision to provide a proper pos/neg spin velocity
	If SpinBall.X < BallnPlay.X Then PiFilln = PiTess Else PiFilln = 0
		
'		So basically..... the new spin velocity = old spin velocity +- (spinner angle +- ball collision angle) * collision velocity
	sVel = sVel + sin((RotAdj * Pi/180) - atn(((SpinBall.Y - BallnPlay.Y) * -1)/(SpinBall.X - BallnPlay.X)) + PiFilln) * velocity/8

End Sub
'	^^^^^^^^^^^  Spinning Tesseract Toy ^^^^^^^^^^^^^

' Keys
Sub Table1_KeyDown(ByVal keycode)
	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress:PinCab_LeftFlipperButton.X = PinCab_LeftFlipperButton.X + 10
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress:PinCab_RightFlipperButton.X = PinCab_RightFlipperButton.X - 10
	If Keycode = StartGameKey Then Controller.Switch(16) = 1:SoundStartButton

	If keycode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull()
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If

	If keycode = LeftTiltKey Then Nudge 90, 1:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 1:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 2:SoundNudgeCenter()

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	If keycode = StartGameKey Then 
		Controller.Switch(16)  = True
        soundStartButton() 
		Pincab_StartButton.Y = Pincab_StartButton.Y - 5 
		Pincab_StartButton2.Y = Pincab_StartButton2.Y - 5
	End if

	If keycode = keyFront Then 
		Controller.Switch(15) = True
		soundStartButton() 
		Pincab_TourneyButton.Y =Pincab_TourneyButton.Y - 5 'Tournament mode mapped to key 2
	End If

	If KeyCode = LeftMagnaSave Then
		bLutActive = True
	End If

	If KeyCode = RightMagnaSave Then
		If bLutActive = True Then
			If DisableLUTSelector = 0 Then
				LUTSet = LUTSet + 1
				
				If LutSet > 16 Then
					LUTSet = 0
				End If

				If LutSet <> 12 Then
					Playsound "click", 0, 1, 0.1, 0.25 
				End If
				
				If LutSet = 12 Then
					Playsound "gun", 0, 1, 0.1, 0.25 
				End If

				LutSlctr.Enabled = True

				SetLUT
				ShowLUT
			End If
		End If
	End If

	If vpmKeyDown(keycode) Then Exit Sub 
End Sub


Sub Table1_KeyUp(ByVal keycode)
	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress:PinCab_LeftFlipperButton.X = PinCab_LeftFlipperButton.X - 10
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress:PinCab_RightFlipperButton.X = PinCab_RightFlipperButton.X + 10
	If Keycode = StartGameKey Then Controller.Switch(16) = 0
    If keycode = PlungerKey Then
        Plunger.Fire
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		PinCab_Plunger.Y = 2307.621
        If(BallinPlunger = 1) Then 'the ball is in the plunger lane
            SoundPlungerReleaseBall() 
        Else
            SoundPlungerReleaseNoBall() 
        End If
	End If
	' Left Magna has to be held down with Right Magna to cycle through LUTs.
	If keycode = LeftMagnaSave Then bLutActive = False

	If keycode = StartGameKey Then 
		Controller.Switch(16)  = False
		Pincab_StartButton.Y = Pincab_StartButton.Y + 5
		Pincab_StartButton2.Y = Pincab_StartButton2.Y + 5
	End If

	If keycode = keyFront Then 
		Controller.Switch(15) = False
		Pincab_TourneyButton.Y = Pincab_TourneyButton.Y + 5 ' Tournament mode mapped to key 2
	End If

	If vpmKeyUp(keycode) Then Exit Sub
End Sub

' Solenoids
SolCallback(1)  = "solTrough"
SolCallback(2)  = "solAutofire"
SolCallback(3)  = "solHulkCCW"
SolCallback(4)  = "solHulkCW"
SolCallback(5)  = "bsHole.SolOut"
SolCallback(6)  = "solRaiseDtBank"
SolCallback(7)  = "OrbitControlGate"
SolCallback(9)  = "vpmSolSound ""bumper"","
SolCallback(10) = "vpmSolSound ""bumper"","
SolCallback(11) = "vpmSolSound ""bumper"","
SolCallback(12) = "solRampControlGate"
SolCallback(15) = "SolLFlipper" 
SolCallback(16) = "SolRFlipper"
SolCallBack(17) = "solHulkArms" 'Hulk Arms

' Flashers
If UseVPMModSol = False Then
	SolCallBack(18) = "FlashBasic18"  ' Top Left Dome - Flupper Dome #3
	SolCallback(20) = "FlashBasic20"  ' Sling Domes - Flupper Dome #1 and #2
	SolCallback(21) = "Flash21"       ' Hulk Spotlight Flashers
	SolCallback(22) = "solLokiLockup" ' Loki Post
	SolCallback(25) = "Flash25"       ' Pop Bumper Flashers
	SolCallback(26) = "Flash26"       ' Teseract Cube Flasher
	SolCallback(27) = "FlashBasic27"  ' Backwall Flasher #1 - Flupper Dome #4
	SolCallback(28) = "FlashBasic28"  ' Backwall Flasher #2 - Flupper Dome #5
	SolCallback(29) = "FlashBasic29"  ' Backwall Flasher #3 - Flupper Dome #6
	SolCallback(30) = "FlashBasic30"  ' Backwall Flasher #4 - Flupper Dome #7
	SolCallback(31) = "FlashBasic31"  ' Backwall Flasher #5 - Flupper Dome #8
	SolCallback(32) = "FlashBasic32"  ' Backwall Flasher #6 - Flupper Dome #9
Else
	If PWMflashers = True Then
		SolModCallBack(18) = "FlashPWM18"  ' Top Left Dome - Flupper Dome #3
		SolModCallback(20) = "FlashPWM20"  ' Sling Domes - Flupper Dome #1 and #2
		SolCallback(21) = "Flash21"        ' Hulk Spotlight Flashers
		SolCallback(22) = "solLokiLockup"  ' Loki Post
		SolCallback(25) = "Flash25"        ' Pop Bumper Flashers
		SolCallback(26) = "Flash26"        ' Tesseract Cube Flasher
		SolModCallback(27) = "FlashPWM27"  ' Backwall Flasher #1 - Flupper Dome #4
		SolModCallback(28) = "FlashPWM28"  ' Backwall Flasher #2 - Flupper Dome #5
		SolModCallback(29) = "FlashPWM29"  ' Backwall Flasher #3 - Flupper Dome #6
		SolModCallback(30) = "FlashPWM30"  ' Backwall Flasher #4 - Flupper Dome #7
		SolModCallback(31) = "FlashPWM31"  ' Backwall Flasher #5 - Flupper Dome #8
		SolModCallback(32) = "FlashPWM32"  ' Backwall Flasher #6 - Flupper Dome #9
	Else
		SolModCallBack(18) = "FlashMod18"  ' Top Left Dome - Flupper Dome #3
		SolModCallback(20) = "FlashMod20"  ' Sling Domes - Flupper Dome #1 and #2
		SolCallback(21) = "Flash21"        ' Hulk Spotlight Flashers
		SolCallback(22) = "solLokiLockup"  ' Loki Post
		SolCallback(25) = "Flash25"        ' Pop Bumper Flashers
		SolCallback(26) = "Flash26"        ' Tesseract Cube Flasher
		SolModCallback(27) = "FlashMod27"  ' Backwall Flasher #1 - Flupper Dome #4
		SolModCallback(28) = "FlashMod28"  ' Backwall Flasher #2 - Flupper Dome #5
		SolModCallback(29) = "FlashMod29"  ' Backwall Flasher #3 - Flupper Dome #6
		SolModCallback(30) = "FlashMod30"  ' Backwall Flasher #4 - Flupper Dome #7
		SolModCallback(31) = "FlashMod31"  ' Backwall Flasher #5 - Flupper Dome #8
		SolModCallback(32) = "FlashMod32"  ' Backwall Flasher #6 - Flupper Dome #9
	End If
End If

set GICallback = GetRef("UpdateGI")

'  Flupper Dome #3 - Top Left Flasher
Sub FlashBasic18(enabled)
	If enabled Then
		ObjTargetLevel(3) = 1
	Else
		ObjTargetLevel(3) = 0
	End If
	FlasherFlash3_Timer
End Sub

Sub FlashMod18(level) 
	ObjTargetLevel(3) = level/255
	FlasherFlash3_Timer
End Sub

Sub FlashPWM18(aValue) 
	UpdateCaps 3, aValue/255
End Sub

' Flupper Dome #1 and #2 - Slingshot Domes
Sub FlashBasic20(enabled) 
	If enabled Then
		ObjTargetLevel(1) = 1
	Else
		ObjTargetLevel(1) = 0
	End If
	FlasherFlash1_Timer
	If enabled Then
		ObjTargetLevel(2) = 1
	Else
		ObjTargetLevel(2) = 0
	End If
	FlasherFlash2_Timer
End Sub

Sub FlashMod20(level)
	ObjTargetLevel(1) = level/255
	FlasherFlash1_Timer
	ObjTargetLevel(2) = level/255
	FlasherFlash2_Timer
End Sub

Sub FlashPWM20(aValue) 
	UpdateCaps 1, aValue/255
	UpdateCaps 2, aValue/255
End Sub

' Hulk Spotlight Flasher
Sub Flash21(enabled)
	If Enabled Then	
		Spot21a.State = 1
		Spot21b.State = 1
		Spot21c.State = 1
	Else
		Spot21a.State = 0
		Spot21b.State = 0
		Spot21c.State = 0
	End If
End Sub

' Pop Bumper Flashers
Sub Flash25(Enabled)
	If Enabled then
		Flasher25.State = 1
	Else
		Flasher25.State = 0
	End If
End Sub

' Tesseract Flasher
Sub Flash26(Enabled)
	If Enabled then
		Flasher26a.State = 1
		Flasher26b.State = 1
'		Flasher26c.State = 1
	Else
		Flasher26a.State = 0
		Flasher26b.State = 0
'		Flasher26c.State = 0
	End If
End Sub

' Backpanel Flasher - Flupper Dome #4
Sub FlashBasic27(enabled)
	If enabled Then
		ObjTargetLevel(4) = 1
	Else
		ObjTargetLevel(4) = 0
	End If
	FlasherFlash4_Timer
End Sub

Sub FlashMod27(level)
	ObjTargetLevel(4) = level/255
	FlasherFlash4_Timer
End Sub

Sub FlashPWM27(aValue) 
	UpdateCaps 4, aValue/255
End Sub

' Backpanel Flasher - Flupper Dome #5
Sub FlashBasic28(enabled)
	If enabled Then
		ObjTargetLevel(5) = 1
	Else
		ObjTargetLevel(5) = 0
	End If
	FlasherFlash5_Timer
End Sub

Sub FlashMod28(level)
	ObjTargetLevel(5) = level/255
	FlasherFlash5_Timer
End Sub

Sub FlashPWM28(aValue) 
	UpdateCaps 5, aValue/255
End Sub

' Backpanel Flasher - Flupper Dome #6
Sub FlashBasic29(enabled)
	If enabled Then
		ObjTargetLevel(6) = 1
	Else
		ObjTargetLevel(6) = 0
	End If
	FlasherFlash6_Timer
End Sub

Sub FlashMod29(level)
	ObjTargetLevel(6) = level/255
	FlasherFlash6_Timer
End Sub

Sub FlashPWM29(aValue) 
	UpdateCaps 6, aValue/255
End Sub

' Backpanel Flasher - Flupper Dome #7
Sub FlashBasic30(enabled)
	If enabled Then
		ObjTargetLevel(7) = 1
	Else
		ObjTargetLevel(7) = 0
	End If
	FlasherFlash7_Timer
End Sub

Sub FlashMod30(level)
	ObjTargetLevel(7) = level/255
	FlasherFlash7_Timer
End Sub

Sub FlashPWM30(aValue) 
	UpdateCaps 7, aValue/255
End Sub

' Backpanel Flasher - Flupper Dome #8
Sub FlashBasic31(enabled)
	If enabled Then
		ObjTargetLevel(8) = 1
	Else
		ObjTargetLevel(8) = 0
	End If
	FlasherFlash8_Timer
End Sub

Sub FlashMod31(level)
	ObjTargetLevel(8) = level/255
	FlasherFlash8_Timer
End Sub

Sub FlashPWM31(aValue) 
	UpdateCaps 8, aValue/255
End Sub

' Backpanel Flasher - Flupper Dome #9
Sub FlashBasic32(enabled)
	If enabled Then
		ObjTargetLevel(9) = 1
	Else
		ObjTargetLevel(9) = 0
	End If
	FlasherFlash9_Timer
End Sub

Sub FlashMod32(level)
	ObjTargetLevel(9) = level/255
	FlasherFlash9_Timer
End Sub

Sub FlashPWM32(aValue) 
	UpdateCaps 9, aValue/255
End Sub

'***DEBUG
'SolCallBack(3)="solcheck 3," 'Hulk CCW
'SolCallBack(4)="solcheck 4," 'Hulk CW
'SolCallBack(13)="solcheck 13,"
'SolCallBack(14)="solcheck 14,"
'SolCallBack(17)="solcheck 17," 'Hulk Arms
'SolCallBack(23)="solcheck 23," 'Hulk Magnet
'SolCallBack(24)="solcheck 24," 'Coin Meter
'SolCallBack(33)="solcheck 33,"
'SolCallBack(34)="solcheck 34,"

'SolCallBack(35)="solcheck 35," 'toggled a lot
'SolCallBack(36)="solcheck 36," 'toggled a lot
'SolCallBack(37)="solcheck 37,"
'SolCallBack(38)="solcheck 38,"
'SolCallBack(39)="solcheck 39,"
'SolCallBack(40)="solcheck 40,"
SolCallBack(41)="solcheck 41,"
SolCallBack(42)="solcheck 42,"
SolCallBack(43)="solcheck 43,"
SolCallBack(44)="solcheck 44,"
SolCallBack(45)="solcheck 45,"
SolCallBack(46)="solcheck 46,"
SolCallBack(47)="solcheck 47,"
SolCallBack(48)="solcheck 48,"

Sub solcheck(value,enabled)
	dim solx
    Select Case value
         Case 3: solx = "Hulk CCW"
         Case 4: solx = "Hulk CW"
         Case 5: solx = "Hulk Eject"
         Case 7: solx = "Orbit Control Gate"
         Case 8: solx = "Shaker"
         Case 12: solx = "Ramp Control Gate Left"
         Case 13: solx = "LeftSling"
         Case 14: solx = "RightSling"
         Case 17: solx = "Hulk Arms"
         Case 18: solx = "Flash Left"
         Case 19: solx = "Flash Right"
         Case 20: solx = "Flash Slings"
         Case 21: solx = "Flash Hulk"
         Case 23: solx = "Hulk Magnet"
         Case 24: solx = "Coin Meter"
         Case 25: solx = "Flash Pop Bumper"
         Case 27: solx = "Flash Back1"
         Case 28: solx = "Flash Back2"
         Case 29: solx = "Flash Back3"
         Case 30: solx = "Flash Back4"
         Case 31: solx = "Flash Back5"
         Case 32: solx = "Flash Back6"
    End Select
	
	'debug.print solx &": solenoid " & value &"="&enabled
'	Msgbox solx &": solenoid " & value &"="&enabled
End Sub

Sub solTrough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		vpmTimer.PulseSw 22
	End If
End Sub

sub solHulkCCW (enabled)
	If enabled Then
		HulkPrim.Roty = HulkPrim.roty + 20
		HulkPrim1.Roty = HulkPrim.Roty
		HulkPrim2.Roty = HulkPrim.Roty
		HulkArm1.RotateToEnd
	Else
		HulkPrim.Roty = 180
		HulkPrim1.Roty = HulkPrim.Roty
		HulkPrim2.Roty = HulkPrim.Roty
		HulkArm1.RotateToStart
	End If
End Sub

sub solHulkCW (enabled)
	If enabled Then
		HulkPrim.Roty = HulkPrim.roty - 20
		HulkPrim1.Roty = HulkPrim.Roty
		HulkPrim2.Roty = HulkPrim.Roty
		HulkArm2.RotateToEnd
	Else
		HulkPrim.Roty = 180	
		HulkPrim1.Roty = HulkPrim.Roty
		HulkPrim2.Roty = HulkPrim.Roty
		HulkArm2.RotateToStart
	End If
End Sub

Dim armsup:armsup=False
Sub solHulkArms (enabled)
	If enabled Then
		HulkArm1.Enabled = 0
		HulkArm2.Enabled = 0
		armsup = True
		HulkPrim.blenddisablelighting=0.8
		HulkPrim1.blenddisablelighting=0.8
		HulkPrim2.blenddisablelighting=0.8
	Else
		HulkArm1.Enabled = 1
		HulkArm2.Enabled = 1
		armsup = False
		HulkPrim.blenddisablelighting=0.4
		HulkPrim1.blenddisablelighting=0.4
		HulkPrim2.blenddisablelighting=0.4
	End If
End Sub

Dim armmove
Sub HulkArms_Timer()
	armmove = 0
	If armsup = false and HulkPrim1.RotX > 130 then armmove = -4
	If armsup = true and HulkPrim1.RotX < 200 then armmove = 4
	HulkPrim1.RotX = HulkPrim1.RotX + armmove
End Sub

Sub solRaiseDtBank(enabled)
	If enabled Then
		dtBank.DropSol_On
		sw52.IsDropped=0
		sw53.IsDropped=0
		sw54.IsDropped=0
		sw55.IsDropped=0
	End If
End Sub

Sub solLokiLockup(Enabled)
	If Enabled Then
 		post49.isdropped = 1: playsound "fx_woodhit2"
	Else
		post49.isdropped = 0: playsound "fx_woodhit"
	End If
End Sub

Sub solRampControlGate(Enabled)
	If Enabled Then
		RampControlGate.IsDropped = 0: PlaySound "fx_diverter"
	Else
		RampControlGate.IsDropped = 1: PlaySound "fx_diverter"
	End If
End Sub

Sub OrbitControlGate(enabled)
	If Enabled Then 
		gate1.open=True
	Else
		gate1.open=False
	End If
End Sub

Sub solAutofire(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
End Sub

Sub HulkEject_Hit
	SoundSaucerLock
	bsHole.addball me
'	HulkEject.Enabled = False
End Sub  

Sub Drain_Hit:RandomSoundDrain Drain:bsTrough.AddBall Me:End Sub

'Sub sw1_Hit:Controller.Switch(1) = 1:PlaySound "fx_target_loud":End Sub 'THOR targets
'Sub sw1_UnHit:Controller.Switch(1) = 0:End Sub
'Sub sw2_Hit:Controller.Switch(2) = 1:PlaySound "fx_target_loud":End Sub 'THOR targets
'Sub sw2_UnHit:Controller.Switch(2) = 0:End Sub
'Sub sw3_Hit:Controller.Switch(3) = 1:PlaySound "fx_target_loud":End Sub 'THOR targets
'Sub sw3_UnHit:Controller.Switch(3) = 0:End Sub
'Sub sw4_Hit:Controller.Switch(4) = 1:PlaySound "fx_target_loud":End Sub 'THOR targets
'Sub sw4_UnHit:Controller.Switch(4) = 0:End Sub
Sub sw7_Hit: vpmTimer.PulseSw 7:End Sub '	Controller.Switch(7) = 1	Tesseract single target
'Sub sw7_UnHit:Controller.Switch(7) = 0:End Sub
Sub sw10_Hit:Controller.Switch(10) = 1:End Sub ' Top lanes
Sub sw10_UnHit:Controller.Switch(10) = 0:End Sub
Sub sw11_Hit:Controller.Switch(11) = 1:End Sub
Sub sw11_UnHit:Controller.Switch(11) = 0:End Sub
Sub sw12_Hit:Controller.Switch(12) = 1:End Sub
Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub
Sub sw23_Hit:Controller.Switch(23) = 1:clockwise=0:End Sub ' Shooter lane
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
Sub sw24_Hit:Controller.Switch(24) = 1:End Sub ' Lanes
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub


Sub sw25_Hit
	Controller.Switch(25) = 1
End Sub 

Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub

Sub sw28_Hit:Controller.Switch(28) = 1:End Sub 
Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub
Sub sw29_Hit:Controller.Switch(29) = 1:End Sub 
Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub
Sub sw33_Hit:Controller.Switch(33) = 1:End Sub 'Capt America inner loop
Sub sw33_UnHit:Controller.Switch(33) = 0:End Sub
'Sub sw34_Hit:Controller.Switch(34) = 1:PlaySound "fx_target_loud":End Sub 'Shield Target
'Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub
'Sub sw35_Hit:Controller.Switch(35) = 1:PlaySound "fx_target_loud":End Sub 'Right 2 bank
'Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub
'Sub sw36_Hit:Controller.Switch(36) = 1:PlaySound "fx_target_loud":End Sub 'Right 2 bank
'Sub sw36_UnHit:Controller.Switch(36) = 0:End Sub
Sub sw43_Hit:Controller.Switch(43) = 1:End Sub 'Black widow ramp
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub
Sub sw44_Spin():vpmtimer.pulsesw 44:End Sub 'Spinner
'	vvvvvvvvvvvvvvv Spinning Tesseract Toy vvvvvvvvvvvvvvvvv
Sub sw45_Hit: If ActiveBall.ID = 0 Then vpmtimer.pulsesw 45:End If:end sub	'Use SpBall(0).ID to trig below playfield tesseract switches
'Sub sw45_UnHit:Controller.Switch(45) = 0:End Sub
Sub sw46_Hit: If ActiveBall.ID = 0 Then vpmtimer.pulsesw 46:End If:end sub	'Use SpBall(0).ID to trig below playfield tesseract switches
'Sub sw46_UnHit:Controller.Switch(46) = 0:End Sub
'	^^^^^^^^^^^  Spinning Tesseract Toy ^^^^^^^^^^^^^
Sub sw47_Hit:Controller.Switch(47) = 1:End Sub ' Hawkeye outer loop
Sub sw47_UnHit:Controller.Switch(47) = 0:End Sub
Dim clockwise
Sub sw48_Hit
	if clockwise = 1 then ' only register from left loop
		Controller.Switch(48) = 1
		clockwise = 0
	end if
End Sub 
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub
Sub Sw48a_Hit: clockwise = 1: end sub
Sub sw51_hit():Controller.Switch(51) = false::end sub 'Loki Locks
Sub sw51_unhit():Controller.Switch(51) = true:end sub
Sub sw50_hit():Controller.Switch(50) = false:post51.isdropped = false:debug.print "POST3 UP":end sub
Sub sw50_unhit():Controller.Switch(50) = true:post51.isdropped = true:debug.print "POST3 DOWN": end sub
Sub sw49_hit():Controller.Switch(49) = false:post50.isdropped = false:debug.print "POST2 UP":end sub
Sub sw49_unhit():Controller.Switch(49) = true:post50.isdropped = true:debug.print "POST2 DOWN":end sub
Sub sw52_Hit: dtBank.Hit 1:TargetBouncer Activeball, 1:End Sub'sw52back.IsDropped=1:HulkEjectDisable:End Sub 
Sub sw53_Hit: dtBank.Hit 2:TargetBouncer Activeball, 1:End Sub 'sw53back.IsDropped=1:HulkEjectDisable:End Sub
Sub sw54_Hit: dtBank.Hit 3:TargetBouncer Activeball, 1:End Sub 'sw54back.IsDropped=1:HulkEjectDisable:End Sub
Sub sw55_Hit: dtBank.Hit 4:TargetBouncer Activeball, 1:End Sub  'sw55back.IsDropped=1
Sub sw57_Hit:Controller.Switch(57) = 1:PlaySound "fx_sensor":End Sub ' Hulk magnet
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
Sub sw61_Hit:Controller.Switch(61) = 1:End Sub 'Iron Man outer loop
Sub sw61_UnHit:Controller.Switch(61) = 0:End Sub
Sub swTopRight_Hit:PlaySound "fx_gate":End Sub

' Stand-Up Targets
Sub Sw1_Hit:vpmTimer.PulseSw 1:TargetBouncer Activeball, 1:End Sub
Sub Sw2_Hit:vpmTimer.PulseSw 2:TargetBouncer Activeball, 1:End Sub
Sub Sw3_Hit:vpmTimer.PulseSw 3:TargetBouncer Activeball, 1:End Sub
Sub Sw4_Hit:vpmTimer.PulseSw 4:TargetBouncer Activeball, 1:End Sub
Sub sw13_Hit:vpmTimer.PulseSw 13:TargetBouncer Activeball, 1:End Sub 
Sub sw14_Hit:vpmTimer.PulseSw 14:TargetBouncer Activeball, 1:End Sub	
Sub Sw34_Hit:vpmTimer.PulseSw 34:TargetBouncer Activeball, 1:End Sub
Sub Sw35_Hit:vpmTimer.PulseSw 35:TargetBouncer Activeball, 1:End Sub
Sub Sw36_Hit:vpmTimer.PulseSw 36:TargetBouncer Activeball, 1:End Sub
Sub Sw63_Hit:vpmTimer.PulseSw 63:TargetBouncer Activeball, 1:End Sub

Const ReflipAngle = 20
  
Sub SolLFlipper(Enabled)
	If Enabled Then
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
		RF.Fire
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

Sub BallRelease_UnHit():End Sub

' Slings
Dim LStep, RStep
 
Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 26
	RandomSoundSlingshotLeft zCol_Rubber_Corner_002
End Sub
 
Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
    vpmTimer.PulseSw 27
	RandomSoundSlingshotRight zCol_Rubber_Corner_005
End Sub
 
' Bumpers
Sub Bumper1b_Hit:vpmTimer.PulseSw 31:RandomSoundBumperMiddle bumper1b:bump1 = 1:End Sub
Sub Bumper2b_Hit:vpmTimer.PulseSw 30:RandomSoundBumperTop bumper2b:bump2 = 1:End Sub
Sub Bumper3b_Hit:vpmTimer.PulseSw 32:RandomSoundBumperBottom bumper3b:bump3 = 1:End Sub


Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

Sub TriggerLaunchRamp_Hit
	WireRampOn False
End Sub

Sub TriggerHawkeyeRamp_Hit
	WireRampOn True
End Sub

Sub TriggerBlackWidowRamp_Hit
	WireRampOn True
End Sub

' Staged Left In-Lane Ball Speed Governor for Ski Jumps
Sub DropLeft_Hit
	activeball.velx = 0
	activeball.vely = 0
	activeball.velz = 0
End Sub

Sub LeftChute_Hit
	activeball.velx = 0
	activeball.vely = 0
	activeball.velz = 0
	LCHit = 1
End Sub

Sub LeftChute2_Hit
	If LCHit = 1 Then
		activeball.velx = 0
		activeball.vely = 0
		activeball.velz = 0
		LCHit = 0
	End If
End Sub

' Right In-Lane Ball Speed Governor for  Ski Jumps
Sub RightChute_Hit
	activeball.velx = 0
	activeball.vely = 0
	activeball.velz = 0
End Sub

Sub FrameTimer_Timer()
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate ' Uupdate ball shadows
End Sub


'Sub DropLeft2_Hit
'    PlaySound "drop_left"
'End Sub

'Sub DropRight_Hit
'	debug.print "rc hit"
'	activeball.vely = 0
'	activeball.AngMomZ = 0
'End Sub

'Sub Post49_Hit
'	PlaySound "fx_plastichit"
'End Sub
 
'Sub Post50_Hit
'	PlaySound "fx_ballpool"
'End Sub

'Sub Post51_Hit
'	PlaySound "fx_ballpool"
'End Sub

'Sub MetalRamp_Hit
'		PlaySound "rail"
'End Sub
'Sub MetalRamp_UnHit
'		StopSound "rail"
'End Sub

'GI - add any new GI lights to the collection "GI" to control them together.
Dim ig

Sub UpdateGI(no, Enabled)
	Select Case no
		Case 0 'Top
			If Enabled Then
				For each ig in GI:ig.State = 1:Next
				For each ig in GIbulbs:ig.State = 1:Next
				Pincab_Backglass.blenddisablelighting= 0.8
			Else
				For each ig in GI:ig.State = 0:Next
				For each ig in GIbulbs:ig.State = 0:Next
				Pincab_Backglass.blenddisablelighting= 0.2
			End If
	End Select
End Sub

Dim GILevel, DayNight, xx

Sub Intensity
	If DayNight <= 20 Then 
			GILevel = .5
	ElseIf DayNight <= 40 Then 
			GILevel = .4125
	ElseIf DayNight <= 60 Then 
			GILevel = .325
	ElseIf DayNight <= 80 Then 
			GILevel = .2375
	Elseif DayNight <= 100  Then 
			GILevel = .15
	End If

	For each xx in GI: xx.IntensityScale = xx.IntensityScale * (GILevel*2): Next
	For each xx in GIbulbs: xx.IntensityScale = xx.IntensityScale * (GILevel*2): Next
End Sub
' end GI subs

'***********************
'Alpha Ramp Plunger
'***********************
Dim PNewPos, POldPos
Dim BallinPlunger

Sub swPlunger_Hit:BallinPlunger = 1:End Sub  'CallGI(1)   ' In this sub you may add a switch, for example Controller.Switch(14) = 1
Sub swPlunger_UnHit:BallinPlunger = 0:End Sub             ' In this sub you may add a switch, for example Controller.Switch(14) = 0

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_exit():Controller.Stop:	End sub

'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************

'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

Dim LF
Set LF = New FlipperPolarity
Dim RF
Set RF = New FlipperPolarity

InitPolarity

'*******************************************
' Early 90's and after

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, -5.5
		x.AddPt "Polarity", 2, 0.4, -5.5
		x.AddPt "Polarity", 3, 0.6, -5.0
		x.AddPt "Polarity", 4, 0.65, -4.5
		x.AddPt "Polarity", 5, 0.7, -4.0
		x.AddPt "Polarity", 6, 0.75, -3.5
		x.AddPt "Polarity", 7, 0.8, -3.0
		x.AddPt "Polarity", 8, 0.85, -2.5
		x.AddPt "Polarity", 9, 0.9,-2.0
		x.AddPt "Polarity", 10, 0.95, -1.5
		x.AddPt "Polarity", 11, 1, -1.0
		x.AddPt "Polarity", 12, 1.05, -0.5
		x.AddPt "Polarity", 13, 1.1, 0
		x.AddPt "Polarity", 14, 1.3, 0

		x.AddPt "Velocity", 0, 0,	   1
		x.AddPt "Velocity", 1, 0.160, 1.06
		x.AddPt "Velocity", 2, 0.410, 1.05
		x.AddPt "Velocity", 3, 0.530, 1'0.982
		x.AddPt "Velocity", 4, 0.702, 0.968
		x.AddPt "Velocity", 5, 0.95,  0.968
		x.AddPt "Velocity", 6, 1.03,  0.945
	Next

	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF 
	RF.SetObjects "RF", RightFlipper, TriggerRF 
End Sub

'' Flipper trigger hit subs
'Sub TriggerLF_Hit()
'	LF.Addball activeball
'End Sub
'Sub TriggerLF_UnHit()
'	LF.PolarityCorrect activeball
'End Sub
'Sub TriggerRF_Hit()
'	RF.Addball activeball
'End Sub
'Sub TriggerRF_UnHit()
'	RF.PolarityCorrect activeball
'End Sub

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
	private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	private Name
 
	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub
 
	Public Sub SetObjects(aName, aFlipper, aTrigger) 
 
		if typename(aName) <> "String" then msgbox "FlipperPolarity: .SetObjects error: first argument must be a string (and name of Object). Found:" & typename(aName) end if
		if typename(aFlipper) <> "Flipper" then msgbox "FlipperPolarity: .SetObjects error: second argument must be a flipper. Found:" & typename(aFlipper) end if
		if typename(aTrigger) <> "Trigger" then msgbox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & typename(aTrigger) end if
		if aFlipper.EndAngle > aFlipper.StartAngle then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper : FlipperStart = aFlipper.x 
		FlipperEnd = Flipper.Length * sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
 
		dim str : str = "sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)  
 
	End Sub
 
	Public Property Let EndPoint(aInput) :  : End Property ' Legacy: just no op 
 
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
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
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function		'Timer shutoff for polaritycorrect
 
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
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				end if
			Next
 
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
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
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
 
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			if DebugOn then debug.print "PolarityCorrect" & " " & Name & " @ " & gametime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
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
	for x = 0 to uBound(aArray)		'Shuffle objects in a temp array
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
	redim aArray(aCount-1+offset)		'Resize original array
	for x = 0 to aCount-1				'set objects back into original array
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
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
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
	dim ii : for ii = 1 to uBound(xKeyFrame)		'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
 
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )		 'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )		'Clamp upper
 
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

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point is px,py
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
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
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

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
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
		If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
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
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports in debugger (in vel, out cor); cor bounce curve (linear)

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
SleevesD.Print = False	  'debug, reports in debugger (in vel, out cor)
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
		If gametime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " in vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
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
		allBalls = getballs
		
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
Sub RDampen_Timer
	Cor.Update
End Sub

Sub GameTimer_Timer
	RollingUpdate 
End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
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
	TargetBouncer activeball, 1
End Sub

'******************************************************
'****  END TARGET BOUNCER
'******************************************************

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

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script in-game
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
		If gametime > 100 Then Report
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
'****  END SLINGSHOT CORRECTIONS
'******************************************************

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
	finalspeed = Sqr(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
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
	finalspeed = Sqr(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
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
	finalspeed = Sqr(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
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
	If Abs(cor.ballvelx(activeball.id) < 4) And cor.ballvely(activeball.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
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
	finalspeed = Sqr(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 10 Then
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
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, Activeball
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
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx <  - 8 Then
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
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, Activeball
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

' This is replaced above by a customer routine for the Tesseract.

'Sub OnBallBallCollision(ball1, ball2, velocity)
'	Dim snd
'	Select Case Int(Rnd * 7) + 1
'		Case 1
'		snd = "Ball_Collide_1"
'		Case 2
'		snd = "Ball_Collide_2"
'		Case 3
'		snd = "Ball_Collide_3"
'		Case 4
'		snd = "Ball_Collide_4"
'		Case 5
'		snd = "Ball_Collide_5"
'		Case 6
'		snd = "Ball_Collide_6"
'		Case 7
'		snd = "Ball_Collide_7"
'	End Select
	
'	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
'End Sub


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

'******************************************************
'****  END FLEEP MECHANICAL SOUNDS
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
Dim RampBalls(7,2)
'x,0 = ball x,1 = ID, 2 = Protection against ending early (minimum amount of updates)

'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
' Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
' Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
Dim RampType(7)

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
			Debug.print "WireRampOn error, ball queue is full: " & vbNewLine & _
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
' 	ZFLD:  FLUPPER DOMES 
'******************************************************
' Based on FlupperDoms2.2

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

' example script for rom based tables (non modulated):

' SolCallback(25)="FlashRed"
'
' Sub FlashRed(flstate)
'	If Flstate Then
'		ObjTargetLevel(1) = 1
'	Else
'		ObjTargetLevel(1) = 0
'	End If
'   FlasherFlash1_Timer
' End Sub

' example script for rom based tables (modulated):

' SolModCallback(25)="FlashRed"
'
' Sub FlashRed(level)
'	ObjTargetLevel(1) = level/255 : FlasherFlash1_Timer
' End Sub

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object			 ***
Set TableRef = Table1		   ' *** change this, if your table has another name				   ***
FlasherLightIntensity = 0.1	 ' *** lower this, if the VPX lights are too bright (i.e. 0.1)	   ***
FlasherFlareIntensity = 0.3	 ' *** lower this, if the flares are too bright (i.e. 0.1)		   ***
FlasherBloomIntensity = 0.1	 ' *** lower this, if the blooms are too bright (i.e. 0.1)		   ***	
FlasherOffBrightness = 0.5	  ' *** brightness of the flasher dome when switched off (range 0-2)  ***
' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20), ObjTargetLevel(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height

'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "red"
InitFlasher 2, "red"
InitFlasher 3, "red"
InitFlasher 4, "blue"
InitFlasher 5, "purple"
InitFlasher 6, "green"
InitFlasher 7, "red"
InitFlasher 8, "white"
InitFlasher 9, "yellow"

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'   RotateFlasher 1,17
'   RotateFlasher 2,0
'   RotateFlasher 3,90
'   RotateFlasher 4,90 

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

Sub FlashFlasher(nr)
	If Not objflasher(nr).TimerEnabled Then
		objflasher(nr).TimerEnabled = True
		objflasher(nr).visible = 1
		objbloom(nr).visible = 1
		objlit(nr).visible = 1
	End If
	objflasher(nr).opacity = 1000 * FlasherFlareIntensity * ObjLevel(nr) ^ 2.5
	objbloom(nr).opacity = 100 * FlasherBloomIntensity * ObjLevel(nr) ^ 2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr) ^ 3
	objbase(nr).BlendDisableLighting = FlasherOffBrightness + 10 * ObjLevel(nr) ^ 3
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr) ^ 2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0
	If Round(ObjTargetLevel(nr),1) > Round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) + 0.3
		If ObjLevel(nr) > 1 Then ObjLevel(nr) = 1
	ElseIf Round(ObjTargetLevel(nr),1) < Round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) * 0.85 - 0.01
		If ObjLevel(nr) < 0 Then ObjLevel(nr) = 0
	Else
		ObjLevel(nr) = Round(ObjTargetLevel(nr),1)
		objflasher(nr).TimerEnabled = False
	End If
	'   ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then
		objflasher(nr).TimerEnabled = False
		objflasher(nr).visible = 0
		objbloom(nr).visible = 0
		objlit(nr).visible = 0
	End If
End Sub

Sub UpdateCaps(nr, aValue) ' nf UseVPMModSol dynamic fading version of 'FlashFlasher' (PWM Update). You can see most of the ramp up has been commented out in favor of Pinmame's curves
	if aValue > 0 then 
		objflasher(nr).visible = 1 : objlit(nr).visible = 1 ': objbloom(nr).visible = 1
		Select Case nr
			Case 1:							'1,2,3 are same event, so doing this only for 1
				Flasherbloom1.ImageA="flasherbloomLowerLeft"
				Flasherbloom1.ImageB="flasherbloomLowerLeft"
				Flasherbloom1.rotx = -186.1
				Flasherbloom1.roty = 180
				Flasherbloom1.color = RGB(230,49,255)
				Flasherbloom1.amount = 750
				Flasherbloom1.visible = 1
			Case 2:
				Flasherbloom2.ImageA="flasherbloomLowerRight"
				Flasherbloom2.ImageB="flasherbloomLowerRight"
				FlasherBloom2.rotx = -6.1
				Flasherbloom2.roty = 180
				Flasherbloom2.color = RGB(255,32,4)
				Flasherbloom2.amount = 750
				Flasherbloom2.visible = 1
			Case 3:
				Flasherbloom3.ImageA="flasherbloomUpperLeft"
				Flasherbloom3.ImageB="flasherbloomUpperLeft"
				Flasherbloom3.rotx = -6.1
				Flasherbloom3.roty = 0
				Flasherbloom3.color = RGB(230,49,255)
				Flasherbloom3.amount = 750
				Flasherbloom3.visible = 1
			Case 4:
				Flasherbloom4.ImageA="flasherbloomUpperRight"
				Flasherbloom4.ImageB="flasherbloomUpperRight"
				Flasherbloom4.rotx = -186.1
				Flasherbloom4.roty = 180
				Flasherbloom4.color = RGB(255,32,4)
				Flasherbloom4.amount = 500
				Flasherbloom4.visible = 1
		End Select
	Else 
		objflasher(nr).visible = 0 : objlit(nr).visible = 0 ': objbloom(nr).visible = 0 
	End If

	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * aValue' * ObjLevel(nr)^2.5

	'objbloom(nr).opacity = 100 *  FlasherBloomIntensity * aValue' * ObjLevel(nr)^2.5
	Select Case nr
		Case 1:
			Flasherbloom1.opacity = 100 *  FlasherBloomIntensity * aValue '* MaxLevel^2.5
		Case 2:
			Flasherbloom2.opacity = 100 *  FlasherBloomIntensity * aValue '* MaxLevel^2.5
		Case 3:
			Flasherbloom3.opacity = 100 *  FlasherBloomIntensity * aValue '* MaxLevel^2.5
		Case 4:
			Flasherbloom4.opacity = 100 *  FlasherBloomIntensity * aValue '* MaxLevel^2.5
	End Select
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * aValue' * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * aValue '* ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * aValue '* ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr, 0,0,0,0,0,0,Round(aValue,1),RGB(255,255,255),0,0,False,True,0,0,0,0 

End Sub

Sub FlasherFlash1_Timer()
	FlashFlasher(1)
End Sub
Sub FlasherFlash2_Timer()
	FlashFlasher(2)
End Sub
Sub FlasherFlash3_Timer()
	FlashFlasher(3)
End Sub
Sub FlasherFlash4_Timer()
	FlashFlasher(4)
End Sub
Sub FlasherFlash5_Timer()
	FlashFlasher(5)
End Sub
Sub FlasherFlash6_Timer()
	FlashFlasher(6)
End Sub
Sub FlasherFlash7_Timer()
	FlashFlasher(7)
End Sub
Sub FlasherFlash8_Timer()
	FlashFlasher(8)
End Sub
Sub FlasherFlash9_Timer()
	FlashFlasher(9)
End Sub
'******************************************************
'******  END FLUPPER DOMES
'******************************************************

'***************************************************************
'	ZSHA: VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************
Const lob = 0	' Locked balls on start
Const gilvl = 1

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

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******

' *** These define the appearance of shadows in your table	***

'Ambient (Room light source)
Const AmbientBSFactor = 0.9	 '0 to 1, higher is darker
Const AmbientMovement = 1	   '1+ higher means more movement as the ball moves left and right
Const offsetX = 0			   'Offset x position under ball (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY = 5			   'Offset y position under ball (^^for example 5,5 if the light is in the back left corner)

'Dynamic (Table light sources)
Const DynamicBSFactor = 0.90	'0 to 1, higher is darker
Const Wideness = 20			 'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness = 5			  'Sets minimum as ball moves away from source

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
Dim objrtx1(6), objrtx2(6)
Dim objBallShadow(6)
Dim OnPF(6)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4, BallShadowA5)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

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
	Dim BOT: BOT=getballs
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
	Dim BOT: BOT=getballs	
	
	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 To tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next
	
	If UBound(BOT) < lob Then Exit Sub 'No balls in play, exit
	
	'The Magic happens now
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
					'   objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
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

' VR PLUNGER ANIMATION
Sub TimerPlunger_Timer
	If PinCab_Plunger.Y < 2407.621  then
		PinCab_Plunger.Y = PinCab_Plunger.Y + 5
	End If
End Sub

Sub TimerPlunger2_Timer
	PinCab_Plunger.Y = 2307.621 + (5* Plunger.Position) - 20
End Sub

'****************************************************************
'****  LUT SELECTOR
'****************************************************************

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
'12 = VPW Original (Default)
'13 = Bassgeige
'14 = Blacklight
'15 = B&W Comic Book
'16 = Tyson171's Skitso Mod2

Dim LUTset, DisableLUTSelector, bLutActive
DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1

LoadLUT
SetLUT

Sub SetLUT  'AXS
	table1.ColorGradeImage = "LUT" & LUTset
End Sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = False 
	LUTBox.Visible = 0
	VRLutdesc.Visible = 0
End Sub

' LUT Selector Timer
Sub LutSlctr_timer
	LutSlctr.Enabled = False
End Sub

Sub ShowLUT

	LUTBox.visible = 1
	VRLutdesc.visible = 1

	Select Case LUTSet
        Case 0: LUTBox.text = "Fleep Natural Dark 1": VRLUTdesc.imageA = "LUTcase0"
		Case 1: LUTBox.text = "Fleep Natural Dark 2": VRLUTdesc.imageA = "LUTcase1"
		Case 2: LUTBox.text = "Fleep Warm Dark": VRLUTdesc.imageA = "LUTcase2"
		Case 3: LUTBox.text = "Fleep Warm Bright": VRLUTdesc.imageA = "LUTcase3"
		Case 4: LUTBox.text = "Fleep Warm Vivid Soft": VRLUTdesc.imageA = "LUTcase4"
		Case 5: LUTBox.text = "Fleep Warm Vivid Hard": VRLUTdesc.imageA = "LUTcase5"
		Case 6: LUTBox.text = "Skitso Natural and Balanced": VRLUTdesc.imageA = "LUTcase6"
		Case 7: LUTBox.text = "Skitso Natural High Contrast": VRLUTdesc.imageA = "LUTcase7"
		Case 8: LUTBox.text = "3rdaxis Referenced THX Standard": VRLUTdesc.imageA = "LUTcase8"
		Case 9: LUTBox.text = "CalleV Punchy Brightness and Contrast": VRLUTdesc.imageA = "LUTcase9"
		Case 10: LUTBox.text = "HauntFreaks Desaturated" : VRLUTdesc.imageA = "LUTcase10"
  		Case 11: LUTBox.text = "Tomate washed out": VRLUTdesc.imageA = "LUTcase11"
		Case 12: LUTBox.text = "VPW Default": VRLUTdesc.imageA = "LUTcase12"
		Case 13: LUTBox.text = "bassgeige": VRLUTdesc.imageA = "LUTcase13"
		Case 14: LUTBox.text = "blacklight": VRLUTdesc.imageA = "LUTcase14"
		Case 15: LUTBox.text = "B&W Comic Book": VRLUTdesc.imageA = "LUTcase15"
		Case 16: LUTBox.text = "Tyson171's Skitso Mod2": VRLUTdesc.imageA = "LUTcase16"
	End Select

	LUTBox.TimerEnabled = 1

End Sub

Sub SaveLUT
	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End If

	If LUTset = "" Then LUTset = 12 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "AvengersLUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub

Sub LoadLUT
    bLutActive = False
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then 
		LUTset = 12
		Exit Sub
	End If
	If Not FileObj.FileExists(UserDirectory & "AvengersLUT.txt") Then
		LUTset = 12
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "AvengersLUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
	If (TextStr.AtEndOfStream=True) Then
		Exit Sub
	End If
	rLine = TextStr.ReadLine
	If rLine = "" Then
		LUTset = 12
		Exit Sub
	End If
	LUTset = int (rLine) 
	Set ScoreFile = Nothing
	Set FileObj = Nothing
End Sub

'****************************************************************
'****  END LUT SELECTOR
'****************************************************************

