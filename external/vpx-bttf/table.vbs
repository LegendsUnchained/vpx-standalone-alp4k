' Back to the Future Collectors Edition
' Based on Back to the Future / IPD No. 126 / Data East June, 1990 / 4 Players
' VP911 version 1.0 by JPSalas Mars 2011
' VPX version 1.0 - cyberpez


Option Explicit
Randomize


Dim InstructionCardsLeft, InstructionCardsRight, FlipperColor, SolidStateSitcker, BallRadius, BallMass, GIColorMod, LeftHillValleyMod, RightHillValleyMod, DeloreanColorMod, PlasticProtectors, BallMod, WobblePlastic, HologramUpdateStep, IsItMultiball, HologramPhoto, BallsLocked, BallsInPlay, enableBallControl, musicsnippet, RubberColor
Dim DesktopMode: DesktopMode = bttf.ShowDT

if DesktopMode = False Then Ramp3.visible = 0:Ramp4.visible = 0

'***************************************************************************'
'***************************************************************************'
'								OPTIONS
'***************************************************************************'
'***************************************************************************'

Const UseFlexDMD = 0	' 1 is on to replace external dmd.
		

'Ball Mod
'0=Normal balls
'1=Yellow/Orange/Red balls

BallMod = 0


'GI ColorMod

''0 = Random
''1 = normal
''2 = Blue / Yellow / 
''3 = Blue / Pink /

GIColorMod = 1


'Hill Valley box ColorMod
'0 = Off
'1 = On

LeftHillValleyMod = 0
RightHillValleyMod = 0


'Delorean ColorMod
'0 = white
'1 = red

DeloreanColorMod = 0


'Plastic Protectors
'0 = Random
'1 = Clear
'2 = BlackLight Yellow
'3 = BlackLight Red
'4 = BlackLight Orange

PlasticProtectors = 1


'''''''''''''''''''''''''''''''''''''''''''''''
'  Instruction Cards  --  You can Mix and Match
'''''''''''''''''''''''''''''''''''''''''''''''
' -1 = No Cards
' 0 = Random
' 1 = Standard
' 2 = Alt1
' 3 = Alt2

InstructionCardsLeft = 1
InstructionCardsRight = 1


''''''''''''''''''''''''
'Hologram Photo
'(photo on apron changes as balls locked)
''''''''''''''''''''''''
'0 = No Hologram Photo
'1 = Show Hologram Photo

HologramPhoto = 0

''''''''''''''''''''''''''''''
'  Flipper 
''''''''''''''''''''''''''''''

'Flipper Colors
'0 = Random

'1 = White Flipper Black Rubber
'2 = White Flipper Red Rubber
'3 = White Flipper Yellow Rubber


'4 = Yellow Flipper Black Rubber
'5 = Yellow Flipper Red Rubber
'6 = Yellow Flipper Yellow Rubber

'7 = Metalic Yellow Flipper Black Rubber
'8 = Metalic Yellow Flipper Red Rubber
'9 = Metalic Yellow Flipper Yellow Rubber

FlipperColor = 5


'Solid State Sitcker
'0 = Random
'1 = None
'2 = Black
'3 = Red
'4 = Yellow

SolidStateSitcker = 1


'Intro music
'Play music snippet on game load.
'0 = Off
'1 = On

MusicSnippet = 0


' Wobble Plastic...  
'0 = no Wobble
'1 = Wobbles

WobblePlastic = 0


' Rubber Color
'0-White
'1-Black

RubberColor = 1




'''''''''''''''''''''''''''''''''''''

'Ball Size and Weight
BallRadius = 25
BallMass = 1

enableBallControl = 0

'***************************************************************************'
'***************************************************************************'
'								OPTIONS
'***************************************************************************'
'***************************************************************************'




' ======================--=======================================================================
' load game controller
' ===============================================================================================

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01560000", "DE.VBS", 3.26

'********************
'Standard definitions
'********************

Const cGameName = "bttf_a27"
Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 1
Const UseSync = 0
Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = ""
Const SFlipperOn = "FlipperUp"
Const SFlipperOff = "FlipperDown"
Const SCoin = "Coin"

Dim bsTrough, bsVuk, bsTR, vLock, x, bump1, bump2, bump3, DTBank
Dim MaxBalls, InitTime, EjectTime, TroughEject, TroughCount, iBall, fgBall

'************
' Table init.
'************

Sub bttf_Init

	' initalise the FlexDMD display
    If UseFlexDMD Then FlexDMD_Init

    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Back to the Future Collectors Edition" & vbNewLine & "Data East - 1990"
        ' DMD position and size, example
        '.Games(cGameName).Settings.Value("dmd_pos_x")=500
        '.Games(cGameName).Settings.Value("dmd_pos_y")=2
        '.Games(cGameName).Settings.Value("dmd_width")=400
        '.Games(cGameName).Settings.Value("dmd_height")=90

		.Games(cGameName).Settings.value("sound") = 1 ' - Test table sounds...  disables ROM sounds

        '.Games(cGameName).Settings.Value("rol") = 0 'rotate DMD to the left
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
		If UseFlexDMD Then ExternalEnabled = Controller.Games(cGameName).Settings.Value("showpindmd")
		If UseFlexDMD Then .Games(cGameName).Settings.Value("showpindmd") = 0
		If DesktopMode = true then .hidden = 1 Else .hidden = 1 End If
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With




    ' Nudging
    vpmNudge.TiltSwitch = swTilt
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(bumper1, bumper2, bumper3, LeftSlingshot, RightSlingshot)


    ' Drop Targets
   	Set DTBank = New cvpmDropTarget  
   	  With DTBank
  		.InitDrop Array(Array(sw41),Array(sw42),Array(sw43)), Array(41,42,43)
		.InitSnd SoundFX("_droptarget",DOFDropTargets),SoundFX("resetdrop",DOFContactors)
       End With

    ' Top Right Saucer
    Set bsTR = New cvpmBallStack
    With bsTR
        .InitSaucer sw45, 45, 194, 10
        .KickForceVar = 2
        .KickBalls = 1
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors) 
    End With


    ' Main Timer init
    PinMAMETimer.Interval = PinMameInterval
    PinMAMETimer.Enabled = 1


    bump1 = 0:bump2 = 0:bump3 = 0
    SolGi 1
	CheckInstructionCards
	SetFlipperColor
	SetGIColor
	StartLevel
	SetHillValleyColorMod
	SetDeloreanColorMod
	SetRubberColor
	Backdrop_Init

'	vpmInit me

' ball through system
	MaxBalls=3	
	InitTime=61
	EjectTime=0
	TroughEject=1
	TroughCount=0
	iBall = 3
	fgBall = false

    CreatBalls

	PrevGameOver = 0



End Sub

Sub bttf_Paused:Controller.Pause = 1:End Sub
Sub bttf_unPaused:Controller.Pause = 0:End Sub

'**********
' Keys
'**********

Sub bttf_KeyDown(ByVal Keycode)
	If keycode = PlungerKey Then Plunger.Pullback:PlaySoundAt"plungerpull",Plunger
    If keycode = LeftTiltKey Then
		LeftNudge 270, 5
	End If
    If keycode = RightTiltKey Then 
		RightNudge 90, 5
	End If
    If keycode = CenterTiltKey Then 
		CenterNudge 0, 5
	End If

	'* Test Kicker
'	If keycode = 37 Then TestKick ' K
'	If keycode = 19 Then return_to_test ' R return ball to kicker
'	If keycode = 46 Then create_testball ' C create ball ball in test kicker
'	If keycode = 205 Then TKickAngle = TKickAngle + 3:fKickDirection.Visible=1:fKickDirection.RotZ=TKickAngle'+90 ' right arrow
'	If keycode = 203 Then TKickAngle = TKickAngle - 3:fKickDirection.Visible=1:fKickDirection.RotZ=TKickAngle'+90 'left arrow
'	If keycode = 200 Then TKickPower = TKickPower + 2:debug.print "TKickPower: "&TKickPower ' up arrow
'	If keycode = 208 Then TKickPower = TKickPower - 2:debug.print "TKickPower: "&TKickPower ' down arrow

	'* Ball Control
	If enableBallControl Then
		if keycode = 46 then	 			' C Key
			If contball = 1 Then
				contball = 0
			Else
				contball = 1
			End If
		End If
		if keycode = 48 then 				'B Key
			If bcboost = 1 Then
				bcboost = bcboostmulti
			Else
				bcboost = 1
			End If
		End If
		if keycode = 203 then bcleft = 1		' Left Arrow
		if keycode = 200 then bcup = 1			' Up Arrow
		if keycode = 208 then bcdown = 1		' Down Arrow
		if keycode = 205 then bcright = 1		' Right Arrow
	End If

    If vpmKeyDown(keycode) Then Exit Sub

	If keycode = 21 then  ''''''''''''''''''''y Key used for testing
		pCRLock.collidable = false
		pCRLock.RotZ = 50
	End If

	If keycode = 22 then  ''''''''''''''''''''u Key used for testing
		WobbleCount = 5
		tWobblePlastic.Enabled = true
	End If

End Sub

Sub bttf_KeyUp(ByVal Keycode)
    If KeyUpHandler(KeyCode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySoundAt"plunger",Plunger
	'* Test Kicker
'	If keycode = 205 Then fKickDirection.Visible=0 ' right arrow
'	If keycode = 203 Then fKickDirection.Visible=0 'left arrow

	'* Ball Control
	If enableBallControl Then
		if keycode = 203 then bcleft = 0		' Left Arrow
		if keycode = 200 then bcup = 0			' Up Arrow
		if keycode = 208 then bcdown = 0		' Down Arrow
		if keycode = 205 then bcright = 0		' Right Arrow
	End If

End Sub

'******************************************************
'					Test Kicker
'******************************************************

Dim TKickAngle, TKickPower, TKickBall
TKickAngle = 0
TKickPower = 10

Sub testkick()
	test.kick TKickAngle,TKickPower
End Sub

Sub create_testball():Set TKickBall = test.CreateBall:End Sub
Sub test_hit():Set TKickBall=ActiveBall:End Sub
Sub return_to_test():TKickBall.velx=0:TKickBall.vely=0:TKickBall.x=test.x:TKickBall.y=test.y-50:test.timerenabled=0:End Sub


'#############################
'  Rotate Primitive Things
'#############################
Const PI = 3.14
Dim Gate3Angle, Gate4Angle

'***********	Ball Control
Sub StartControl_Hit()
	Set ControlBall = ActiveBall
	contballinplay = true
End Sub

Sub StopControl_Hit()
	contballinplay = false
End Sub	

Dim bcup, bcdown, bcleft, bcright, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti

bcboost = 1		'Do Not Change - default setting
bcvel = 4		'Controls the speed of the ball movement
bcyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3	'Boost multiplier to ball veloctiy (toggled with the B key) 
'***********	Ball Control

Dim prevgameover

Sub MyTimer_Timer()


    p_gate1.Rotx = gate1.CurrentAngle + 120
    p_gate2.Rotx = gate2.CurrentAngle
    p_gate3.Rotx = gate3.CurrentAngle' + 90

	Gate3Angle = Int(gate3.CurrentAngle)
	If Gate3Angle > 0 then
	pGate3_switch.ObjRotY = sin( (Gate3Angle * 1) * (2*PI/180)) * 10
	Else
	pGate3_switch.ObjRotY = sin( (Gate3Angle * -1) * (2*PI/180)) * 10
	End If

	Gate4Angle = Int(gate4.CurrentAngle)
	If Gate4Angle > 0 then
	pGate4_switch.ObjRotY = sin( (Gate4Angle * -1) * (2*PI/180)) * 10
	Else
	pGate4_switch.ObjRotY = sin( (Gate4Angle * 1) * (2*PI/180)) * 10
	End If



    p_gate4.Rotx = gate4.CurrentAngle' + 90 

	pLeftFlipperLogo.Roty = LeftFlipper.Currentangle' + 180
'	pLSS.Roty = LeftFlipper.Currentangle - 90 
	FlipperLSh.RotZ = LeftFlipper.currentangle

	pRightFlipperLogo.Roty = RightFlipper.Currentangle' + 180
	pRSS.Roty = RightFlipper.Currentangle - 90 
	FlipperRSh.RotZ = RightFlipper.currentangle

	pSpinner.RotX = sw28.Currentangle * -1

	pSpinnerRod.TransX = sin( (sw28.CurrentAngle+180) * (2*PI/360)) * 5
	pSpinnerRod.TransY = sin( (SW28.CurrentAngle- 90) * (2*PI/360)) * 5

	'***********	Ball Control
	If Contball and ContBallInPlay then
		If bcright = 1 Then
			ControlBall.velx = bcvel*bcboost
		ElseIf bcleft = 1 Then
			ControlBall.velx = - bcvel*bcboost
		Else
			ControlBall.velx=0
		End If

		If bcup = 1 Then
			ControlBall.vely = -bcvel*bcboost
		ElseIf bcdown = 1 Then
			ControlBall.vely = bcvel*bcboost
		Else
			ControlBall.vely= bcyveloffset
		End If
	End If
	'***********	Ball Control




End Sub


''''''''''''''''''''''''
''''Bubble Level
''''''''''''''''''''''''

Dim lBallY, lBallX
Sub StartLevel()

'Y
	kLevel.Enabled = 1
	Set lBallY = kLevel.CreateSizedBallWithMass(4, .008)
	kLevel.kick 0, 0 
	kLevel.Enabled = 0


'X
	kLevel1.Enabled = 1
	Set lBallX = kLevel1.CreateSizedBallWithMass(4, .008)
	kLevel1.kick 0, 0 
	kLevel1.Enabled = 0

End Sub

Sub Level_Timer()


	xBubble.x = lBallX.x
	yBubble.y = lBallY.y

End Sub


Sub LeftNudge(angle, strength)
    Dim a
   lBallX.velx = 2*(RND(1)-RND(1))
End Sub

Sub RightNudge(angle, strength)
    Dim a
   lBallX.velx = 2*(RND(1)-RND(1))

End Sub

Sub CenterNudge(angle, strength)
    Dim a
	kLevel.Enabled = 1
    lBallY.vely = 2*(RND(1)-RND(1))
	PlaySound SoundFX("knocker",DOFKnocker)
	kLevel.Enabled = 0
End Sub


'******************************
'  Setup Desktop
'******************************

Sub Backdrop_Init
	Dim bdl
	If DesktopMode = True then

		l56c.visible = true
		l34.visible = true
		l35.visible = true
		l36.visible = true
		l37.visible = true
		l38.visible = true
		l39.visible = true
		l47c.visible = true


''''Delorean Lights

		l25d.BulbHaloHeight = 185
		l26d.BulbHaloHeight = 185
		l27d.BulbHaloHeight = 185
		l28d.BulbHaloHeight = 185
		l29d.BulbHaloHeight = 185
		l30d.BulbHaloHeight = 185
		l31d.BulbHaloHeight = 185
		l32d.BulbHaloHeight = 185

	Else


		l56c.visible = false
		l34.visible = false
		l35.visible = false
		l36.visible = false
		l37.visible = false
		l38.visible = false
		l39.visible = false
		l47c.visible = false


''''Delorean Lights

		l25d.BulbHaloHeight = 175
		l26d.BulbHaloHeight = 175
		l27d.BulbHaloHeight = 175
		l28d.BulbHaloHeight = 175
		l29d.BulbHaloHeight = 175
		l30d.BulbHaloHeight = 175
		l31d.BulbHaloHeight = 175
		l32d.BulbHaloHeight = 175

	End If
End Sub



'''''''''''''''''''''''''''''''''
''''''''''''Set options
'''''''''''''''''''''''''''''''''

'Instruction Cards

Dim InstructionCardsLeftType, InstructionCardsRightType

Sub CheckInstructionCards()

If InstructionCardsLeft = 0 Then
	InstructionCardsLeftType = Int(Rnd*3)+1
Else
	InstructionCardsLeftType = InstructionCardsLeft
End If

	If InstructionCardsLeftType = -1 Then
		pInstructionCardLeft.visible = False
	End If
	If InstructionCardsLeftType = 1 Then
		pInstructionCardLeft.image = "bttf_InstructionCardLeft1"
	End If
	If InstructionCardsLeftType = 2 Then
		pInstructionCardLeft.image = "bttf_InstructionCardLeft2"
	End If
	If InstructionCardsLeftType = 3 Then
		pInstructionCardLeft.image = "bttf_InstructionCardLeft3"
	End If


If InstructionCardsRight = 0 Then
	InstructionCardsRightType = Int(Rnd*3)+1
Else
	InstructionCardsRightType = InstructionCardsRight
End If

	If InstructionCardsRight = -1  Then
		pInstructionCardRight.visible = False
	End If
	If InstructionCardsRightType = 1  Then
		pInstructionCardRight.image = "bttf_InstructionCardRight1"
	End If
	If InstructionCardsRightType = 2  Then
		pInstructionCardRight.image = "bttf_InstructionCardRight2"
	End If
	If InstructionCardsRightType = 3  Then
		pInstructionCardRight.image = "bttf_InstructionCardRight3"
	End If


'Hologram Photo

	If HologramPhoto = 1 then
		pHologram.Visible = True
	Else
		pHologram.Visible = False
	End If


End Sub

''Flipper Color

Dim FlipperColorType

Sub SetFlipperColor()

If FlipperColor = 0 Then
	FlipperColorType = Int(Rnd*9)+1
Else
	FlipperColorType = FlipperColor
End If

If FlipperColorType = 1 Then
	RightFlipper.Material = "Plastic White" 
	RightFlipper.RubberMaterial = "Rubber Black"
	pRightFlipperLogo.Material = "Plastic White"

	LeftFlipper.Material = "Plastic White" 
	LeftFlipper.RubberMaterial = "Rubber Black"
	pLeftFlipperLogo.Material = "Plastic White"
End If

If FlipperColorType = 2 Then
	RightFlipper.Material = "Plastic White" 
	RightFlipper.RubberMaterial = "Rubber Red"
	pRightFlipperLogo.Material = "Plastic White"

	LeftFlipper.Material = "Plastic White" 
	LeftFlipper.RubberMaterial = "Rubber Red"
	pLeftFlipperLogo.Material = "Plastic White"
End If

If FlipperColorType = 3 Then
	RightFlipper.Material = "Plastic White" 
	RightFlipper.RubberMaterial = "Rubber Yellow"
	pRightFlipperLogo.Material = "Plastic White"

	LeftFlipper.Material = "Plastic White" 
	LeftFlipper.RubberMaterial = "Rubber Yellow"
	pLeftFlipperLogo.Material = "Plastic White"
End If

If FlipperColorType = 4 Then
	RightFlipper.Material = "Plastic Yellow" 
	RightFlipper.RubberMaterial = "Rubber Black"
	pRightFlipperLogo.Material = "Plastic Yellow"

	LeftFlipper.Material = "Plastic Yellow" 
	LeftFlipper.RubberMaterial = "Rubber Black"
	pLeftFlipperLogo.Material = "Plastic Yellow"
End If

If FlipperColorType = 5 Then
	RightFlipper.Material = "Plastic Yellow" 
	RightFlipper.RubberMaterial = "Rubber Red"
	pRightFlipperLogo.Material = "Plastic Yellow"

	LeftFlipper.Material = "Plastic Yellow" 
	LeftFlipper.RubberMaterial = "Rubber Red"
	pLeftFlipperLogo.Material = "Plastic Yellow"
End If

If FlipperColorType = 6 Then
	RightFlipper.Material = "Plastic Yellow" 
	RightFlipper.RubberMaterial = "Rubber Yellow"
	pRightFlipperLogo.Material = "Plastic Yellow"

	LeftFlipper.Material = "Plastic Yellow" 
	LeftFlipper.RubberMaterial = "Rubber Yellow"
	pLeftFlipperLogo.Material = "Plastic Yellow"
End If

If FlipperColorType = 7 Then
	RightFlipper.Material = "Plastic Metalic Yellow" 
	RightFlipper.RubberMaterial = "Rubber Black"
	pRightFlipperLogo.Material = "Plastic Metalic Yellow"

	LeftFlipper.Material = "Plastic Metalic Yellow" 
	LeftFlipper.RubberMaterial = "Rubber Black"
	pLeftFlipperLogo.Material = "Plastic Metalic Yellow"
End If

If FlipperColorType = 8 Then
	RightFlipper.Material = "Plastic Metalic Yellow" 
	RightFlipper.RubberMaterial = "Rubber Red"
	pRightFlipperLogo.Material = "Plastic Metalic Yellow"

	LeftFlipper.Material = "Plastic Metalic Yellow" 
	LeftFlipper.RubberMaterial = "Rubber Red"
	pLeftFlipperLogo.Material = "Plastic Metalic Yellow"
End If

If FlipperColorType = 9 Then
	RightFlipper.Material = "Plastic Metalic Yellow" 
	RightFlipper.RubberMaterial = "Rubber Yellow"
	pRightFlipperLogo.Material = "Plastic Metalic Yellow"

	LeftFlipper.Material = "Plastic Metalic Yellow" 
	LeftFlipper.RubberMaterial = "Rubber Yellow"
	pLeftFlipperLogo.Material = "Plastic Metalic Yellow"
End If


Dim SolidStateSitckerType

'Solid State Sitcker

If SolidStateSitcker = 0 Then
	SolidStateSitckerType = Int(Rnd*4)+1
Else
	SolidStateSitckerType = SolidStateSitcker
End If

If SolidStateSitckerType = 1 Then
	pLSS.Visible = False
	pRSS.Visible = False

	pLSS.Image = "SolidStateBlackLeft_texture"
	pRSS.Image = "SolidStateBlackRight_texture"
End If

If SolidStateSitckerType = 2 Then
	pLSS.Visible = true
	pRSS.Visible = true

	pLSS.Image = "SolidStateBlackLeft_texture"
	pRSS.Image = "SolidStateBlackRight_texture"
End If

If SolidStateSitckerType = 3 Then
	pLSS.Visible = true
	pRSS.Visible = true

	pLSS.Image = "SolidStateRedLeft_texture"
	pRSS.Image = "SolidStateRedRight_texture"
End If

If SolidStateSitckerType = 4 Then
	pLSS.Visible = true
	pRSS.Visible = true

	pLSS.Image = "SolidStateYellowLeft_texture"
	pRSS.Image = "SolidStateYellowRight_texture"
End If


'Plastic Protectors

Dim PlasticProtectorsType

If PlasticProtectors = 0 Then
	PlasticProtectorsType = Int(Rnd*4)+1
Else
	PlasticProtectorsType = PlasticProtectors
End If

If PlasticProtectorsType = 1 Then
	pPlasticProtectorsA.Material = "AcrylicClear2"
	pPlasticProtectorsA.DisableLighting = False
	pPlasticProtectorsB.Material = "AcrylicClear2"
	pPlasticProtectorsB.DisableLighting = False
End If

If PlasticProtectorsType = 2 Then
	pPlasticProtectorsA.Material = "AcrilicBLYellow"
	pPlasticProtectorsA.DisableLighting = True
	pPlasticProtectorsB.Material = "AcrilicBLYellow"
	pPlasticProtectorsB.DisableLighting = True
End If

If PlasticProtectorsType = 3 Then
	pPlasticProtectorsA.Material = "AcrylicBLRed"
	pPlasticProtectorsA.DisableLighting = True
	pPlasticProtectorsB.Material = "AcrylicBLRed"
	pPlasticProtectorsB.DisableLighting = True
End If

If PlasticProtectorsType = 4 Then
	pPlasticProtectorsA.Material = "AcrylicBLOrange"
	pPlasticProtectorsA.DisableLighting = True
	pPlasticProtectorsB.Material = "AcrylicBLOrange"
	pPlasticProtectorsB.DisableLighting = True
End If

End Sub


''''Rubber Color

Dim xxRubberColor 

Sub SetRubberColor()

If RubberColor = 1 Then

for each xxRubberColor in aRubbers2
xxRubberColor.Material="Rubber Black"
Primitive14.Material="Rubber Black"
next

Else

for each xxRubberColor in aRubbers2
xxRubberColor.Material="Rubber White"
Primitive14.Material="Rubber White"
next

End If

End Sub


''''''''''''''''''''''''''''''''''
''''''  GI Color
''''''''''''''''''''''''''''''''''

Dim RedFull, Red, RedI, PinkFull, Pink, PinkI, WhiteFull, White, WhiteI, BlueFull, Blue, BlueI, YellowFull, Yellow, YellowI, GreenFull, Green, GreenI
Dim GIColorModType


RedFull = rgb(255,0,0) 
Red = rgb(255,0,0)
RedI = 5
PinkFull = rgb(255,0,128)
Pink = rgb(255,0,255)
PinkI = 5
WhiteFull = rgb(255,255,128) 
White = rgb(255,255,255)
WhiteI = 7
BlueFull = rgb(0,128,255)
Blue = rgb(0,255,255)
BlueI = 20
YellowFull = rgb(255,255,128)
Yellow = rgb(255,255,0)
YellowI = 20
GreenFull = rgb(128,255,128)
Green = rgb(0,255,0)
GreenI = 20


Sub SetGIColor()


If GIColorMod = 0 Then
	GIColorModType = Int(Rnd*3)+1
Else
	GIColorModType = GIColorMod
End If

	If GIColorModType = 1 Then

	End If


	If GIColorModType = 2 Then
		gi1a.colorfull = BlueFull 'Blue
		gi1a.color = Blue 'Blue
		gi1b.colorfull = BlueFull 'Blue
		gi1b.color = Blue 'Blue
		gi1c.colorfull = BlueFull 'Blue
		gi1c.color = Blue 'Blue
		gi1a.Intensity = BlueI 'Blue

		gi2a.colorfull = YellowFull 'Yellow
		gi2a.color = Yellow ' Yellow
		gi2b.colorfull = YellowFull 'Yellow
		gi2b.color = Yellow ' Yellow
		gi2c.colorfull = YellowFull 'Yellow
		gi2c.color = Yellow ' Yellow
		gi2a.intensity = YellowI
'		gi2b.intensity = 13

		gi3a.colorfull = YellowFull 'Yellow
		gi3a.color = YellowFull 'Yellow
		gi3b.colorfull = YellowFull 'Yellow
		gi3b.color = Yellow 'Yellow
		gi3a.intensity = YellowI
'		gi3b.intensity = 13
		gi3c.colorfull = YellowFull 'Yellow
		gi3c.color = Yellow 'Yellow

		gi4a.colorfull = BlueFull 'Blue
		gi4a.color = Blue 'Blue
		gi4b.colorfull = BlueFull 'Blue
		gi4b.color = Blue 'Blue
		gi4c.colorfull = BlueFull 'Blue
		gi4c.color = Blue 'Blue
		gi4a.Intensity = BlueI 'Blue

		gi5a.colorfull = YellowFull 'Yellow
		gi5a.color = Yellow ' Yellow
		gi5b.colorfull = YellowFull 'Yellow
		gi5b.color = Yellow ' Yellow
		gi5c.colorfull = YellowFull 'Yellow
		gi5c.color = Yellow ' Yellow
		gi5a.intensity = YellowI

		gi6a.colorfull = YellowFull 'Yellow
		gi6a.color = Yellow ' Yellow
		gi6b.colorfull = YellowFull 'Yellow
		gi6b.color = Yellow ' Yellow
		gi6c.colorfull = YellowFull 'Yellow
		gi6c.color = Yellow ' Yellow
		gi6a.intensity = YellowI

		gi8a.colorfull = YellowFull 'Yellow
		gi8a.color = Yellow ' Yellow
		gi8b.colorfull = YellowFull 'Yellow
		gi8b.color = Yellow ' Yellow
		gi8c.colorfull = YellowFull 'Yellow
		gi8c.color = Yellow ' Yellow
		gi8a.intensity = YellowI

		gi9a.colorfull = YellowFull 'Yellow
		gi9a.color = Yellow ' Yellow
		gi9b.colorfull = YellowFull 'Yellow
		gi9b.color = Yellow ' Yellow
		gi9c.colorfull = YellowFull 'Yellow
		gi9c.color = Yellow ' Yellow
		gi9a.intensity = YellowI

		gi12a.colorfull = RedFull 'Red
		gi12a.color = Red ' Red
		gi12b.colorfull = RedFull 'Red
		gi12b.color = Red ' Red
		gi12c.colorfull = RedFull 'Red
		gi12c.color = Red ' Red
		gi12a.intensity = RedI

		gi13a.colorfull = RedFull 'Red
		gi13a.color = Red ' Red
		gi13b.colorfull = RedFull 'Red
		gi13b.color = Red ' Red
		gi13c.colorfull = RedFull 'Red
		gi13c.color = Red ' Red
		gi13a.intensity = RedI

		gi15.colorfull = rgb(0,128,255) 'Blue
		gi15.color = rgb(0,255,255) 'Blue

		gi16.colorfull = rgb(0,128,255) 'Blue
		gi16.color = rgb(0,255,255) 'Blue

	End If

	If GIColorModType = 3 Then

		gi1a.Color=Blue
		gi1a.ColorFull=BlueFull
		gi1b.Color=Blue
		gi1b.ColorFull=BlueFull
		gi1c.Color=Blue
		gi1c.ColorFull=BlueFull
		gi1a.Intensity = BlueI

		gi2a.Color=Pink
		gi2a.ColorFull=PinkFull
		gi2b.Color=Pink
		gi2b.ColorFull=PinkFull
		gi2c.Color=Pink
		gi2c.ColorFull=PinkFull
		gi2a.Intensity = PinkI

		gi3a.Color=Pink
		gi3a.ColorFull=PinkFull
		gi3b.Color=Pink
		gi3b.ColorFull=PinkFull
		gi3c.Color=Pink
		gi3c.ColorFull=PinkFull
		gi3a.Intensity = PinkI

		gi4a.Color=Blue
		gi4a.ColorFull=BlueFull
		gi4b.Color=Blue
		gi4b.ColorFull=BlueFull
		gi4c.Color=Blue
		gi4c.ColorFull=BlueFull
		gi4a.Intensity = BlueI

		gi5a.Color=Pink
		gi5a.ColorFull=PinkFull
		gi5b.Color=Pink
		gi5b.ColorFull=PinkFull
		gi5c.Color=Pink
		gi5c.ColorFull=PinkFull
		gi5a.Intensity = PinkI

		gi6a.Color=Pink
		gi6a.ColorFull=PinkFull
		gi6b.Color=Pink
		gi6b.ColorFull=PinkFull
		gi6c.Color=Pink
		gi6c.ColorFull=PinkFull
		gi6a.Intensity = PinkI

		gi8a.colorfull = YellowFull 'Yellow
		gi8a.color = Yellow ' Yellow
		gi8b.colorfull = YellowFull 'Yellow
		gi8b.color = Yellow ' Yellow
		gi8c.colorfull = YellowFull 'Yellow
		gi8c.color = Yellow ' Yellow
		gi8a.intensity = YellowI

		gi9a.colorfull = YellowFull 'Yellow
		gi9a.color = Yellow ' Yellow
		gi9b.colorfull = YellowFull 'Yellow
		gi9b.color = Yellow ' Yellow
		gi9c.colorfull = YellowFull 'Yellow
		gi9c.color = Yellow ' Yellow
		gi9a.intensity = YellowI

		gi12a.colorfull = RedFull 'Red
		gi12a.color = Red ' Red
		gi12b.colorfull = RedFull 'Red
		gi12b.color = Red ' Red
		gi12c.colorfull = RedFull 'Red
		gi12c.color = Red ' Red
		gi12a.intensity = RedI

		gi13a.colorfull = RedFull 'Red
		gi13a.color = Red ' Red
		gi13b.colorfull = RedFull 'Red
		gi13b.color = Red ' Red
		gi13c.colorfull = RedFull 'Red
		gi13c.color = Red ' Red
		gi13a.intensity = RedI

		gi15.colorfull = rgb(0,128,255) 'Blue
		gi15.color = rgb(0,255,255) 'Blue

		gi16.colorfull = rgb(0,128,255) 'Blue
		gi16.color = rgb(0,255,255) 'Blue
	End If


End Sub


'Hill Valley Mod

Sub SetHillValleyColorMod()

	If LeftHillValleyMod = 1 Then
		l15.colorfull = rgb(255,255,128) 'Yellow
		l15.color = rgb(255,255,0) ' Yellow
		l15a.colorfull = rgb(255,255,128) 'Yellow
		l15a.color = rgb(255,255,0) ' Yellow
		l56.colorfull = rgb(255,255,128) 'Yellow
		l56.color = rgb(255,255,0) ' Yellow
		l56a.colorfull = rgb(255,255,128) 'Yellow
		l56a.color = rgb(255,255,0) ' Yellow
		l60.colorfull = rgb(255,255,128) 'Yellow
		l60.color = rgb(255,255,0) ' Yellow
		l60a.colorfull = rgb(255,255,128) 'Yellow
		l60a.color = rgb(255,255,0) ' Yellow
	End If

	If RightHillValleyMod = 1 Then
		l46.colorfull = rgb(0,128,255) 'Blue
		l46.color = rgb(0,255,255) 'Blue
		l46a.colorfull = rgb(0,128,255) 'Blue
		l46a.color = rgb(0,255,255) 'Blue
		l47.colorfull = rgb(0,128,255) 'Blue
		l47.color = rgb(0,255,255) 'Blue
		l47a.colorfull = rgb(0,128,255) 'Blue
		l47a.color = rgb(0,255,255) 'Blue
		l48.colorfull = rgb(0,128,255) 'Blue
		l48.color = rgb(0,255,255) 'Blue
		l48a.colorfull = rgb(0,128,255) 'Blue
		l48a.color = rgb(0,255,255) 'Blue
	End If

End Sub


Sub SetDeloreanColorMod()
	If DeloreanColorMod = 1 Then
		l25d.colorfull = rgb(255,0,0) 'Red
		l25d.color = rgb(255,0,0) ' Red
		l26d.colorfull = rgb(255,0,0) 'Red
		l26d.color = rgb(255,0,0) ' Red
		l27d.colorfull = rgb(255,0,0) 'Red
		l27d.color = rgb(255,0,0) ' Red
		l28d.colorfull = rgb(255,0,0) 'Red
		l28d.color = rgb(255,0,0) ' Red
		l29d.colorfull = rgb(255,0,0) 'Red
		l29d.color = rgb(255,0,0) ' Red
		l30d.colorfull = rgb(255,0,0) 'Red
		l30d.color = rgb(255,0,0) ' Red
		l31d.colorfull = rgb(255,0,0) 'Red
		l31d.color = rgb(255,0,0) ' Red
		l32d.colorfull = rgb(255,0,0) 'Red
		l32d.color = rgb(255,0,0) ' Red
	End If
End Sub

'''''''''''''''''''''''''''''
''''Color Ramp Ball Lock
'''''''''''''''''''''''''''''

Dim CRBLStep, WPStep, WP2Step, PlasticWobbling

Sub sw40_Hit()
	Psw40.rotY = 20
	Controller.Switch(40) = 1
	BallsLocked = BallsLocked + 1
	If HologramPhoto = 1 Then
		HologramUpdateStep = HologramUpdateStep +1
		HologramUpdate
	End If
End Sub
Sub sw40_UnHit:Psw40.rotY = 0:Controller.Switch(40) = 0:End Sub

Sub sw39_Hit:Psw39.rotY = 20:Controller.Switch(39) = 1:End Sub
Sub sw39_UnHit:Psw39.rotY = 0:Controller.Switch(39) = 0:End Sub

Sub sw38_Hit()
	Psw38.rotY = 20
	Controller.Switch(38) = 1
	If WobblePlastic = 1 then 
		If PlasticWobbling = 1 then 
		Else

			WobbleCount = 3
			tWobblePlastic.Enabled = True
		End If
	End If	
	PlaySoundAt "metalhit_medium",sw38
End Sub
Sub sw38_UnHit:Psw38.rotY = 0:Controller.Switch(38) = 0:End Sub


Sub CRBallLock(Enabled)
		PlaySound SoundFX("fx_Rudysol1",DOFContactors),0,1,-.3
		pCRLock.collidable = false
		pCRLock.RotZ = 50
		If WobblePlastic = 1 then
			PlasticWobbling = 1
			WobbleCount = 5
			tWobblePlastic.Enabled = True
		End If
		CRBallLockTimer.Enabled = true
End Sub


Sub CRBallLockTimer_Timer()
	Select Case CRBLStep
		Case 0:
		Case 1:
		Case 2:
		Case 3:
		Case 4:
		Case 5:pCRLock.collidable = true:pCRLock.RotZ = 0:CRBallLockTimer.Enabled = false:CRBLStep = 0
	End Select
	CRBLStep = CRBLStep + 1

End Sub

'******************************************
'			Plastic Wobble
'******************************************

Dim WobbleStep, WobbleCount, Wdir

WobbleStep = 0.5	' Controls the size of the wobble
WobbleCount = 5		' Controls the number of wobbles
WDir = 1
tWobblePlastic.interval = 15 ' Controls the speed of the wobble

Sub tWobblePlastic_timer()

	pWabblePlastic0.rotx=pWabblePlastic0.rotx + WDir*WobbleStep

	If WDIR = 1 And PWabblePlastic0.rotx > 89.99 + WobbleCount * WobbleStep Then
		WobbleCount = WobbleCount - 1
		WDir = -1
	ElseIf WDir = -1 And PWabblePlastic0.rotx < 90.01 Then
		WDir = 1
		If WobbleCount = 0 Then
			tWobblePlastic.Enabled = false
			PlasticWobbling = 0
'			WobbleCount = 5
		End If
	End If
	pWabbleScrews.Rotx = pWabblePlastic0.rotx
End Sub



'###############################
'    Holigram Photo
'###############################

Sub HologramUpdate ()
    Select Case HologramUpdateStep
        Case 0:pHologram.Image = "bttf_photo_8"						'Default
        Case 1:pHologram.Image = "bttf_photo_7"						'Animation
        Case 2:pHologram.Image = "bttf_photo_6"						'Lock1
        Case 3:pHologram.Image = "bttf_photo_5"						'Animation
        Case 4:pHologram.Image = "bttf_photo_4"						'Lock2
		Case 5:pHologram.Image = "bttf_photo_3"						'Animation
		Case 6:pHologram.Image = "bttf_photo_2":IsItMultiball = 1	'Lock3
		Case 7:pHologram.Image = "bttf_photo_1"						'Multiball
    End Select

End Sub

Dim IsItMultiballTimerStep

Sub IsItMultiballTimer_Timer()
  Select Case IsItMultiballTimerStep
        Case 0:
        Case 1:
        Case 2:
        Case 3:
        Case 4:If BallsLocked > 1 then IsItMultiball = 0:HologramUpdateStep = 4:HologramUpdate: Else HologramUpdateStep = 7:HologramUpdate: End If
		Case 5:IsItMultiballTimer.Enabled = false:IsItMultiballTimerStep = 0
    End Select
 
    IsItMultiballTimerStep = IsItMultiballTimerStep + 1
 
End Sub


'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
'Slingshot animation
'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


Dim LeftSlingshotStep,RightSlingshotStep


Sub LeftSlingShot_Slingshot:LeftSlingshota.visible = false:pSlingL.TransZ = -8:LeftSlingshotb.visible = true:PlaySoundAt SoundFX("slingshot_l",DOFContactors),pSlingL:vpmTimer.PulseSw 21:LeftSlingshotStep = 0:Me.TimerEnabled = 1:If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 2:tWobblePlastic.Enabled = True:End If:End Sub
Sub LeftSlingshot_Timer
    Select Case LeftSlingshotStep
        Case 0:LeftSlingshotb.visible = false:pSlingL.TransZ = -16:LeftSlingshotc.visible = true
        Case 1:LeftSlingshotc.visible = false:pSlingL.TransZ = -24:LeftSlingshotd.visible = true
        Case 2:LeftSlingshotd.visible = false:pSlingL.TransZ = -16:LeftSlingshotc.visible = true
        Case 3:LeftSlingshotc.visible = false:pSlingL.TransZ = -8:LeftSlingshotb.visible = true
        Case 4:LeftSlingshotb.visible = false:pSlingL.TransZ = 0:LeftSlingshota.visible = true:Me.TimerEnabled = 0 '
    End Select

    LeftSlingshotStep = LeftSlingshotStep + 1
End Sub


Sub RightSlingShot_Slingshot:RightSlingshota.visible = false:pSlingR.TransZ = -8:RightSlingshotb.visible = true:PlaySoundAt SoundFX("slingshot_r",DOFContactors),pSlingR:vpmTimer.PulseSw 22:RightSlingshotStep = 0:Me.TimerEnabled = 1:If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 2:tWobblePlastic.Enabled = True:End If:End Sub
Sub RightSlingshot_Timer
    Select Case RightSlingshotStep
        Case 0:RightSlingshotb.visible = false:pSlingR.TransZ = -16:RightSlingshotc.visible = true
        Case 1:RightSlingshotc.visible = false:pSlingR.TransZ = -24:RightSlingshotd.visible = true
        Case 2:RightSlingshotd.visible = false:pSlingR.TransZ = -16:RightSlingshotc.visible = true
        Case 3:RightSlingshotc.visible = false:pSlingR.TransZ = -8:RightSlingshotb.visible = true
        Case 4:RightSlingshotb.visible = false:pSlingR.TransZ = 0:RightSlingshota.visible = true:Me.TimerEnabled = 0 '
    End Select

    RightSlingshotStep = RightSlingshotStep + 1
End Sub




''''''''''''''''''
'Rubber
''''''''''''''''''
Dim wRubber1aStep, wRubber1bStep, wRubber2Step, wRubber4Step

Sub wRubber1a_hit()
	Rubber1.visible = false:Rubber1a.visible = true:me.timerEnabled = true
End Sub

Sub wRubber1a_Timer
    Select Case wRubber1aStep
        Case 0:
        Case 1:
        Case 2:Rubber1.visible = true:Rubber1a.visible = false:Me.TimerEnabled = 0:wRubber1aStep = 0
    End Select

    wRubber1aStep = wRubber1aStep + 1
End Sub

Sub wRubber1b_hit()
	Rubber1.visible = false:Rubber1b.visible = true:me.timerEnabled = true
End Sub

Sub wRubber1b_Timer
    Select Case wRubber1bStep
        Case 0:
        Case 1:
        Case 2:Rubber1.visible = true:Rubber1b.visible = false:Me.TimerEnabled = 0:wRubber1bStep = 0
    End Select

    wRubber1bStep = wRubber1bStep + 1
End Sub

Sub wRubber2_hit()
	Rubber2.visible = false:Rubber2b.visible = true:me.timerEnabled = true
End Sub

Sub wRubber2_Timer
    Select Case wRubber2Step
        Case 0:
        Case 1:
        Case 2:Rubber2.visible = true:Rubber2b.visible = false:Me.TimerEnabled = 0:wRubber2Step = 0
    End Select

    wRubber2Step = wRubber2Step + 1
End Sub

Sub wRubber4_hit()
	Rubber4.visible = false:Rubber4b.visible = true:me.timerEnabled = true
End Sub

Sub wRubber4_Timer
    Select Case wRubber4Step
        Case 0:
        Case 1:
        Case 2:Rubber4.visible = true:Rubber4b.visible = false:Me.TimerEnabled = 0:wRubber4Step = 0
    End Select

    wRubber4Step = wRubber4Step + 1
End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 46:PlaySoundAtBumperVol SoundFX("bumper1",DOFContactors),Bumper1,1:bump1 = 1:Me.TimerEnabled = 1:If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 2:tWobblePlastic.Enabled = True:End If::End Sub
Sub Bumper1_Timer()
    Select Case bump1
        Case 1:
        Case 2:
        Case 3:
        Case 4:Me.TimerEnabled = 0
    End Select
End Sub

Sub Bumper2_Hit:vpmTimer.PulseSw 47:PlaySoundAtBumperVol SoundFX("bumper2",DOFContactors),Bumper2,1:bump2 = 1:Me.TimerEnabled = 1:If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 2:tWobblePlastic.Enabled = True:End If:End Sub
Sub Bumper2_Timer()
    Select Case bump2
        Case 1:
        Case 2:
        Case 3:
        Case 4:Me.TimerEnabled = 0
    End Select
End Sub

Sub Bumper3_Hit:vpmTimer.PulseSw 48:PlaySoundAtBumperVol SoundFX("bumper1",DOFContactors),Bumper3,1:bump3 = 1:Me.TimerEnabled = 1:If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 2:tWobblePlastic.Enabled = True:End If:End Sub
Sub Bumper3_Timer()
    Select Case bump3
        Case 1:
        Case 2:
        Case 3:
        Case 4:Me.TimerEnabled = 0
    End Select
End Sub

Sub sw45_Hit:bsTR.AddBall 0:Psw45.TransY = -5:PlaySoundAtVol "solenoid2",sw45,2:End Sub
Sub sw45_UnHit:Psw45.TransY = 0:If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 1:tWobblePlastic.Enabled = True:End If:End Sub

'VUK Lock
Sub sw29_Hit()
	PlaySoundat "kicker_enter",sw29
	Controller.Switch(29) = 1
	If HologramPhoto = 1 Then
		HologramUpdateStep = HologramUpdateStep +1
		HologramUpdate
	End If
End Sub

Sub KickBallUp(Enabled)
	Playsoundat SoundFX("Solenoid",DOFContactors),sw29
	sw29.timerenabled = 1
 	sw29.Kick 0,165,1.53
	Controller.Switch(29) = 0
	If WobblePlastic = 1 then
		PlasticWobbling = 1
		WobbleCount = 5:
		tWobblePlastic.Enabled = True
	End If:
End Sub

Dim sw29step

Sub sw29_timer()
	Select Case sw29step
		Case 0:pUpKicker.TransY = 10
		Case 1:pUpKicker.TransY = 20
		Case 2:pUpKicker.TransY = 30
		Case 3:
		Case 4:
		Case 5:pUpKicker.TransY = 25
		Case 6:pUpKicker.TransY = 20
		Case 7:pUpKicker.TransY = 15
		Case 8:pUpKicker.TransY = 10
		Case 9:pUpKicker.TransY = 5
		Case 10:pUpKicker.TransY = 0:sw29.timerEnabled = 0:sw29step = 0
	End Select
	sw29step = sw29step + 1
End Sub

' Rollovers & Ramp Switches
Sub sw17_Hit()
	Switch17dir = 1
	Sw17Move = 1
	Me.TimerEnabled = true
	Controller.Switch(17) = 1
	PlaySoundAt "sensor",sw17
End Sub

Sub sw17_unHit()
	Switch17dir = -1
	Sw17Move = 5
	Me.TimerEnabled = true
	Controller.Switch(17) = 0
End Sub

Dim Switch17dir, SW17Move

Sub sw17_timer()
Select case Sw17Move

	Case 0:me.TimerEnabled = false:pRollover4.RotX = 90

	Case 1:pRollover4.RotX = 95

	Case 2:pRollover4.RotX = 100

	Case 3:pRollover4.RotX = 105

	Case 4:pRollover4.RotX = 110

	Case 5:pRollover4.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover4.RotX = 120

End Select

SW17Move = SW17Move + Switch17dir

End Sub

Sub sw18_Hit()
	Switch18dir = 1
	Sw18Move = 1
	Me.TimerEnabled = true
	Controller.Switch(18) = 1
	PlaySoundAt "sensor",sw18
End Sub

Sub sw18_unHit()
	Switch18dir = -1
	Sw18Move = 5
	Me.TimerEnabled = true
	Controller.Switch(18) = 0
End Sub

Dim Switch18dir, SW18Move

Sub sw18_timer()
Select case Sw18Move

	Case 0:me.TimerEnabled = false:pRollover3.RotX = 90

	Case 1:pRollover3.RotX = 95

	Case 2:pRollover3.RotX = 100

	Case 3:pRollover3.RotX = 105

	Case 4:pRollover3.RotX = 110

	Case 5:pRollover3.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover3.RotX = 120

End Select

SW18Move = SW18Move + Switch18dir

End Sub

Sub sw20_Hit()
	Switch20dir = 1
	Sw20Move = 1
	Me.TimerEnabled = true
	Controller.Switch(20) = 1
	PlaySoundAt "sensor",sw20
End Sub

Sub sw20_unHit()
	Switch20dir = -1
	Sw20Move = 5
	Me.TimerEnabled = true
	Controller.Switch(20) = 0
End Sub

Dim Switch20dir, SW20Move
'Switch19dir = -2

Sub sw20_timer()
Select case Sw20Move

	Case 0:me.TimerEnabled = false:pRollover2.RotX = 90

	Case 1:pRollover2.RotX = 95

	Case 2:pRollover2.RotX = 100

	Case 3:pRollover2.RotX = 105

	Case 4:pRollover2.RotX = 110

	Case 5:pRollover2.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover2.RotX = 120

End Select

SW20Move = SW20Move + Switch20dir

End Sub




Sub sw19_Hit()
	Switch19dir = 1
	Sw19Move = 1
	Me.TimerEnabled = true
	Controller.Switch(19) = 1
	PlaySoundAt "sensor",sw19
End Sub

Sub sw19_unHit()
	Switch19dir = -1
	Sw19Move = 5
	Me.TimerEnabled = true
	Controller.Switch(19) = 0
End Sub


Dim Switch19dir, SW19Move

Sub sw19_timer()
Select case Sw19Move

	Case 0:me.TimerEnabled = false:pRollover1.RotX = 90

	Case 1:pRollover1.RotX = 95

	Case 2:pRollover1.RotX = 100

	Case 3:pRollover1.RotX = 105

	Case 4:pRollover1.RotX = 110

	Case 5:pRollover1.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover1.RotX = 120

End Select

SW19Move = SW19Move + Switch19dir

End Sub



Sub sw30_Hit:Controller.Switch(30) = 1:PlaySoundAt "sensor",sw30:End Sub
Sub sw30_Unhit:Controller.Switch(30) = 0:End Sub

Sub sw31_Hit:Controller.Switch(31) = 1:PlaySoundAt "sensor",sw31:End Sub
Sub sw31_Unhit:Controller.Switch(31) = 0:End Sub

Sub sw14_Hit()
	Switch14dir = 1
	Sw14Move = 1
	Me.TimerEnabled = true
	PlaySoundAt "sensor",sw14
	Controller.Switch(14) = 1
Light001.state = 2
End Sub

Sub sw14_unHit()
	Switch14dir = -1
	Sw14Move = 5
	Me.TimerEnabled = true
	Controller.Switch(14) = 0
Light001.state = 0
End Sub

Dim Switch14dir, SW14Move

Sub sw14_timer()
Select case Sw14Move

	Case 0:me.TimerEnabled = false:pRollover5.RotX = 90

	Case 1:pRollover5.RotX = 95

	Case 2:pRollover5.RotX = 100

	Case 3:pRollover5.RotX = 105

	Case 4:pRollover5.RotX = 110

	Case 5:pRollover5.RotX = 115

	Case 6:me.TimerEnabled = false:pRollover5.RotX = 120

End Select

SW14Move = SW14Move + Switch14dir

End Sub

'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'   Drop Targets
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	dim sw41Dir, sw42Dir, sw43Dir
	dim sw41Pos, sw42Pos, sw43Pos
	Dim sw41step, sw42step, sw43step
 
	sw41Dir = 1:sw42Dir = 1:sw43Dir = 1
	sw41Pos = 0:sw42Pos = 0:sw43Pos = 0

  'Targets Init
	sw41.TimerEnabled = 1:sw42.timerEnabled = 1:sw43.TimerEnabled = 1

Sub DoubleDrop1_HIt:sw41.timerenabled = True:sw42.timerenabled = True: End Sub
Sub DoubleDrop2_HIt:sw42.timerenabled = True:sw43.timerenabled = True: End Sub

	Sub sw41_Hit:me.timerEnabled = 1:If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 1:tWobblePlastic.Enabled = True:End If:End Sub
	Sub sw42_Hit:me.timerEnabled = 1:If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 1:tWobblePlastic.Enabled = True:End If:End Sub
	Sub sw43_Hit:me.timerEnabled = 1:If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 1:tWobblePlastic.Enabled = True:End If:End Sub


Sub sw41_timer()
	Select Case sw41step
		Case 0:
		Case 1:sw41P.RotX = 2
		Case 2:sw41P.RotX = 5
		Case 3:DTBank.Hit 1:sw41Dir = 0:sw41a.Enabled = 1:DoubleDrop1.isDropped = True
		Case 4:sw41P.RotX = 3
		Case 5:sw41P.RotX = 0:me.timerEnabled = 0:sw41step = 0
	End Select
	sw41step = sw41step + 1
End Sub


'''Target animation


 Sub sw41a_Timer()	
  Select Case sw41Pos
        Case 0: sw41P.TransZ=0
				 If sw41Dir = 1 then
					sw41a.Enabled = 0
				 else
			     end if        
        Case 1: sw41P.TransZ=0
        Case 2: sw41P.TransZ=-6
        Case 3: sw41P.TransZ=-8
        Case 4: sw41P.TransZ=-18
        Case 5: sw41P.TransZ=-24
        Case 6: sw41P.TransZ=-30
        Case 7: sw41P.TransZ=-36
        Case 8: sw41P.TransZ=-42
        Case 9: sw41P.TransZ=-48
        Case 10: sw41P.TransZ=-52
				 If sw41Dir = 1 then
				 else
					sw41a.Enabled = 0
			     end if

 
End Select
	If sw41Dir = 1 then
		If sw41pos>0 then sw41pos=sw41pos-1
	else
		If sw41pos<10 then sw41pos=sw41pos+1
	end if
  End Sub


Sub sw42_timer()
	Select Case sw42step
		Case 0:
		Case 1:sw42P.RotX = 2
		Case 2:sw42P.RotX = 5
		Case 3:DTBank.Hit 2:sw42Dir = 0:sw42a.Enabled = 1:DoubleDrop1.isDropped = True:DoubleDrop2.isDropped = True
		Case 4:sw42P.RotX = 3
		Case 5:sw42P.RotX = 0:me.timerEnabled = 0:sw42step = 0
	End Select
	sw42step = sw42step + 1
End Sub


 Sub sw42a_Timer()	
  Select Case sw42Pos
        Case 0: sw42P.TransZ=0
				 If sw42Dir = 1 then
					sw42a.Enabled = 0
				 else
			     end if        
        Case 1: sw42P.TransZ=0
        Case 2: sw42P.TransZ=-6
        Case 3: sw42P.TransZ=-12
        Case 4: sw42P.TransZ=-18
        Case 5: sw42P.TransZ=-24
        Case 6: sw42P.TransZ=-30
        Case 7: sw42P.TransZ=-36
        Case 8: sw42P.TransZ=-42
        Case 9: sw42P.TransZ=-48
        Case 10: sw42P.TransZ=-52
				 If sw42Dir = 1 then
				 else
					sw42a.Enabled = 0
			     end if

 
End Select
	If sw42Dir = 1 then
		If sw42pos>0 then sw42pos=sw42pos-1
	else
		If sw42pos<10 then sw42pos=sw42pos+1
	end if
  End Sub

Sub sw43_timer()
	Select Case sw43step
		Case 0:
		Case 1:sw43P.RotX = 2
		Case 2:sw43P.RotX = 5
		Case 3:DTBank.Hit 3:sw43Dir = 0:sw43a.Enabled = 1:DoubleDrop2.isDropped = True
		Case 4:sw43P.RotX = 3
		Case 5:sw43P.RotX = 0:me.timerEnabled = 0:sw43step = 0
	End Select
	sw43step = sw43step + 1
End Sub


Sub sw43a_Timer()	
	Select Case sw43Pos
        Case 0: sw43P.TransZ=0
				 If sw43Dir = 1 then
					sw43a.Enabled = 0
				 else
			     end if        
        Case 1: sw43P.TransZ=0
        Case 2: sw43P.TransZ=-6
        Case 3: sw43P.TransZ=-12
        Case 4: sw43P.TransZ=-18
        Case 5: sw43P.TransZ=-24
        Case 6: sw43P.TransZ=-30
        Case 7: sw43P.TransZ=-36
        Case 8: sw43P.TransZ=-42
        Case 9: sw43P.TransZ=-48
        Case 10: sw43P.TransZ=-52
				 If sw43Dir = 1 then
				 else
					sw43a.Enabled = 0
			     end if
	End Select
	If sw43Dir = 1 then
		If sw43pos>0 then sw43pos=sw43pos-1
	else
		If sw43pos<10 then sw43pos=sw43pos+1
	end if
End Sub

'DT Subs
   Sub ResetDrops(Enabled)
		If Enabled Then
			sw41Dir = 1:sw42Dir = 1:sw43Dir = 1
			sw41a.Enabled = 1:sw42a.Enabled = 1:sw43a.Enabled = 1:DoubleDrop1.isDropped = False:DoubleDrop2.isDropped = False
			DTBank.DropSol_On
			If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 2:tWobblePlastic.Enabled = True:End If:
		End if
   End Sub

'***************************************
'       Targets
'***************************************

Dim Target25Step, Target26Step, Target27Step, Target33Step, Target34Step, Target35Step, Target36Step, Target37Step

Sub sw25_Hit:vpmTimer.PulseSw(25):P_Target25.TransX = -5:Target25Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw25:End Sub			
Sub sw25_timer()
	Select Case Target25Step
		Case 1:P_Target25.TransX = 3
        Case 2:P_Target25.TransX = -2
        Case 3:P_Target25.TransX = 1
        Case 4:P_Target25.TransX = 0:Me.TimerEnabled = 0
     End Select
	Target25Step = Target25Step + 1
End Sub

Sub DoubleTarget6_hit:vpmTimer.PulseSw(26):vpmTimer.PulseSw(25):P_Target26.TransX = -5:Target26Step = 1:sw26.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw26:P_Target25.TransX = -5:Target25Step = 1:sw25.TimerEnabled = 1:End Sub

Sub sw26_Hit:vpmTimer.PulseSw(26):P_Target26.TransX = -5:Target26Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw26:End Sub			
Sub sw26_timer()
	Select Case Target26Step
		Case 1:P_Target26.TransX = 3
        Case 2:P_Target26.TransX = -2
        Case 3:P_Target26.TransX = 1
        Case 4:P_Target26.TransX = 0:Me.TimerEnabled = 0
     End Select
	Target26Step = Target26Step + 1
End Sub

Sub DoubleTarget5_hit:vpmTimer.PulseSw(27):vpmTimer.PulseSw(326):P_Target27.TransX = -5:Target27Step = 1:sw27.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw27:P_Target25.TransX = -5:Target26Step = 1:sw26.TimerEnabled = 1:End Sub

Sub sw27_Hit:vpmTimer.PulseSw(27):P_Target27.TransX = -5:Target27Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw27:End Sub			
Sub sw27_timer()
	Select Case Target27Step
		Case 1:P_Target27.TransX = 3
        Case 2:P_Target27.TransX = -2
        Case 3:P_Target27.TransX = 1
        Case 4:P_Target27.TransX = 0:Me.TimerEnabled = 0
     End Select
	Target27Step = Target27Step + 1
End Sub

Sub sw33_Hit:vpmTimer.PulseSw(33):P_Target33.TransX = -5:Target33Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw33:End Sub			
Sub sw33_timer()
	Select Case Target33Step
		Case 1:P_Target33.TransX = 3
        Case 2:P_Target33.TransX = -2
        Case 3:P_Target33.TransX = 1
        Case 4:P_Target33.TransX = 0:Me.TimerEnabled = 0
     End Select
	Target33Step = Target33Step + 1
End Sub


Sub DoubleTarget1_hit:vpmTimer.PulseSw(33):vpmTimer.PulseSw(34):P_Target33.TransX = -5:Target33Step = 1:sw33.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw33:P_Target34.TransX = -5:Target34Step = 1:sw34.TimerEnabled = 1:End Sub

Sub sw34_Hit:vpmTimer.PulseSw(34):P_Target34.TransX = -5:Target34Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw34:End Sub			
Sub sw34_timer()
	Select Case Target34Step
		Case 1:P_Target34.TransX = 3
        Case 2:P_Target34.TransX = -2
        Case 3:P_Target34.TransX = 1
        Case 4:P_Target34.TransX = 0:Me.TimerEnabled = 0
     End Select
	Target34Step = Target34Step + 1
End Sub

Sub DoubleTarget2_hit:vpmTimer.PulseSw(34):vpmTimer.PulseSw(35):P_Target34.TransX = -5:Target34Step = 1:sw34.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw34:P_Target35.TransX = -5:Target35Step = 1:sw35.TimerEnabled = 1:End Sub

Sub sw35_Hit:vpmTimer.PulseSw(35):P_Target35.TransX = -5:Target35Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw35:End Sub			
Sub sw35_timer()
	Select Case Target35Step
		Case 1:P_Target35.TransX = 3
        Case 2:P_Target35.TransX = -2
        Case 3:P_Target35.TransX = 1
        Case 4:P_Target35.TransX = 0:Me.TimerEnabled = 0
     End Select
	Target35Step = Target35Step + 1
End Sub

Sub DoubleTarget3_hit:vpmTimer.PulseSw(35):vpmTimer.PulseSw(36):P_Target35.TransX = -5:Target35Step = 1:sw35.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw35:P_Target36.TransX = -5:Target36Step = 1:sw36.TimerEnabled = 1:End Sub

Sub sw36_Hit:vpmTimer.PulseSw(36):P_Target36.TransX = -5:Target36Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw36:End Sub			
Sub sw36_timer()
	Select Case Target36Step
		Case 1:P_Target36.TransX = 3
        Case 2:P_Target36.TransX = -2
        Case 3:P_Target36.TransX = 1
        Case 4:P_Target36.TransX = 0:Me.TimerEnabled = 0
     End Select
	Target36Step = Target36Step + 1
End Sub

Sub DoubleTarget4_hit:vpmTimer.PulseSw(36):vpmTimer.PulseSw(37):P_Target36.TransX = -5:Target36Step = 1:sw36.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw36:P_Target37.TransX = -5:Target37Step = 1:sw37.TimerEnabled = 1:End Sub

Sub sw37_Hit:vpmTimer.PulseSw(37):P_Target37.TransX = -5:Target37Step = 1:Me.TimerEnabled = 1:PlaySoundAt SoundFX("_spottarget",DOFTargets),sw37:End Sub			
Sub sw37_timer()
	Select Case Target37Step
		Case 1:P_Target37.TransX = 3
        Case 2:P_Target37.TransX = -2
        Case 3:P_Target37.TransX = 1
        Case 4:P_Target37.TransX = 0:Me.TimerEnabled = 0
     End Select
	Target37Step = Target37Step + 1
End Sub		

' Spinners
Sub sw28_Spin():vpmTimer.PulseSw 28:PlaySoundAt "spinner",sw28:End Sub

' Ramps helpers
Sub RHelp1_Hit:StopSound "plasticroll":PlaySoundAt "BallHit",RHelp1:End Sub
Sub RHelp2_Hit:StopSound "plasticroll":PlaySoundAt "BallHit",RHelp2:End Sub

'Sub BallRol1_Hit:PlaySound "ballrolling":End Sub


'*********
'Solenoids
'*********

SolCallBack(1) = "SetLamp 101,"
SolCallBack(2) = "SetLamp 102,"
SolCallback(3) = "SetLamp 103,"
SolCallback(4) = "SetLamp 104,"
SolCallback(5) = "SetLamp 105,"
SolCallback(6) = "SetLamp 106," 'left side
 'SolCallback(7) = "SetLamp 107," center
SolCallback(8) = "SetLamp 108,"
SolCallback(9) = "SetLamp 109," 'right side
SolCallback(11) = "SolGi"
SolCallback(12) = "SetLamp 112,"
SolCallback(13) = "SetLamp 113,"
SolCallback(14) = "SetLamp 114,"
SolCallback(15) = "SetLamp 115,"
SolCallback(16) = "SetLamp 116,"
SolCallBack(25) = "kisort"
SolCallBack(26) = "KickBallToLane"
SolCallBack(27) = "CRBallLock"
'SolCallBack(27) = "vLock.SolExit"
SolCallBack(28) = "bsTR.SolOut"
SolCallBack(29) = "KickBallUp"
SolCallBack(30) = "ResetDrops"
SolCallBack(32) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"

'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
		PlaySoundAt SoundFX("flipperup_l",DOFFlippers),LeftFlipper:LeftFlipper.RotateToEnd
    Else

        PlaySoundAt SoundFX("flipperdown",DOFFlippers),LeftFlipper:LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("flipperup_r",DOFFlippers),RightFlipper:RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("flipperdown",DOFFlippers),RightFlipper:RightFlipper.RotateToStart
    End If
End Sub

'**********
' Gi Lights
'**********

Sub SolGi(Enabled)
    Dim obj
    If Enabled Then
		SetLamp 200, 0
		Playsound "fx_relay_off"
    Else
		SetLamp 200, 1
		Playsound "fx_relay_on"
    End If
End Sub



''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''  Ball Through system''''''''''''''''''''''''''
'''''''''''''''''''''by cyberpez''''''''''''''''''''''''''''''''
''''''''''''''''based off of EalaDubhSidhe's''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Dim BallCount
Dim cBall1, cBall2, cBall3

dim bstatus

Sub CreatBalls()
	Controller.Switch(11) = 1
	Controller.Switch(12) = 1
	Controller.Switch(13) = 1
	Set cBall1 = Kicker1.CreateSizedballWithMass(BallRadius,Ballmass)
	Set cBall2 = Kicker2.CreateSizedballWithMass(BallRadius,Ballmass)
	Set cBall3 = Kicker3.CreateSizedballWithMass(BallRadius,Ballmass)

	If BallMod = 1 Then
		cBall1.Image = "PinballLaserLemon"	
		cBall2.Image = "PinballOutrageousOrange"
		cBall3.Image = "PinballRadicalRed"
	End If
End Sub


Sub Kicker3_Hit():Controller.Switch(11) = 1:UpdateTrough:End Sub
Sub Kicker3_UnHit():Controller.Switch(11) = 0:UpdateTrough:End Sub
Sub Kicker2_Hit():Controller.Switch(12) = 1:UpdateTrough:End Sub
Sub Kicker2_UnHit():Controller.Switch(12) = 0:UpdateTrough:End Sub
Sub Kicker1_Hit():Controller.Switch(13) = 1:UpdateTrough:End Sub
Sub Kicker1_UnHit():Controller.Switch(13) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	CheckBallStatus.Interval = 300
	CheckBallStatus.Enabled = 1
End Sub

Sub CheckBallStatus_timer()
	If Kicker1.BallCntOver = 0 Then Kicker2.kick 60, 9
	If Kicker2.BallCntOver = 0 Then Kicker3.kick 60, 9
	Me.Enabled = 0
End Sub

Dim Kicker1active, Kicker2active, Kicker3active, Kicker4active, Kicker5active, Kicker6active


'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub Drain_Hit()
	PlaySoundAtVol "drain",Drain,.8
	UpdateTrough
	Controller.Switch(10) = 1
	fgBall = true
	iBall = iBall + 1
	BallsInPlay = BallsInPlay - 1
	If BallsInPlay = 1 and IsItMultiball = 1 Then IsItMultiball = 0:HologramUpdateStep = 0:HologramUpdate: End If
End Sub

Sub Drain_UnHit()
	Controller.Switch(10) = 0
End Sub

sub kisort(enabled)
	If enabled then
		if fgBall then
			Drain.Kick 70,20
			iBall = iBall + 1
			fgBall = false
		end if
	end if
end sub

Sub KickBallToLane(Enabled)
	if enabled then
		StopSound "intro"
		PlaySoundAt SoundFX("BallRelease",DOFContactors),kicker1
		Kicker1.Kick 70,40
		If WobblePlastic = 1 then
			PlasticWobbling = 1
			WobbleCount = 2
			tWobblePlastic.Enabled = True
		End If
		iBall = iBall - 1
		fgBall = false
		BallsInPlay = BallsInPlay + 1
		UpdateTrough
	end if
End Sub


'================Light Handling==================
'       GI, Flashers, and Lamp handling
'Based on JP's VP10 fading Lamp routine, based on PD's Fading Lights
'       Mod FrameTime and GI handling by nFozzy
'================================================
'Short installation
'Keep all non-GI lamps/Flashers in a big collection called aLampsAll
'Initialize SolModCallbacks: Const UseVPMModSol = 1 at the top of the script, before LoadVPM. vpmInit me in table1_Init()
'LUT images (optional)
'Make modifications based on era of game (setlamp / flashc for games without solmodcallback, use bonus GI subs for games with only one GI control)
 
Dim LampState(340), FadingLevel(340), CollapseMe   
Dim FlashSpeedUp(340), FlashSpeedDown(340), FlashMin(340), FlashMax(340), FlashLevel(340)
Dim SolModValue(340)    'holds 0-255 modulated solenoid values
 
'These are used for fading lights and flashers brighter when the GI is darker
Dim LampsOpacity(340, 2) 'Columns: 0 = intensity / opacity, 1 = fadeup, 2 = FadeDown
Dim GIscale(4)  '5 gi strings
Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image1")

 
InitLamps
 
reDim CollapseMe(1) 'Setlamps and SolModCallBacks   (Click Me to Collapse)
    Sub SetLamp(nr, value)
        If value <> LampState(nr) Then
            LampState(nr) = abs(value)
            FadingLevel(nr) = abs(value) + 4
        End If
    End Sub
 
    Sub SetLampm(nr, nr2, value)    'set 2 lamps
        If value <> LampState(nr) Then
            LampState(nr) = abs(value)
            FadingLevel(nr) = abs(value) + 4
        End If
        If value <> LampState(nr2) Then
            LampState(nr2) = abs(value)
            FadingLevel(nr2) = abs(value) + 4
        End If
    End Sub
 
    Sub SetModLamp(nr, value)
        If value <> SolModValue(nr) Then
            SolModValue(nr) = value
            if value > 0 then LampState(nr) = 1 else LampState(nr) = 0
            FadingLevel(nr) = LampState(nr) + 4
        End If
    End Sub
 
    Sub SetModLampM(nr, nr2, value) 'set 2 modulated lamps
        If value <> SolModValue(nr) Then
            SolModValue(nr) = value
            if value > 0 then LampState(nr) = 1 else LampState(nr) = 0
            FadingLevel(nr) = LampState(nr) + 4
        End If
        If value <> SolModValue(nr2) Then
            SolModValue(nr2) = value
            if value > 0 then LampState(nr2) = 1 else LampState(nr2) = 0
            FadingLevel(nr2) = LampState(nr2) + 4
        End If
    End Sub
    'Flashers via SolModCallBacks
'  SolModCallBack(17) = "SetModLamp 117," 'Billions
'  SolModCallBack(18) = "SetModLamp 118," 'Left ramp
'  SolModCallBack(19) = "SetModLamp 119," 'jackpot
'  SolModCallBack(20) = "SetModLamp 120," 'SkillShot
'  SolModCallBack(21) = "SetModLamp 121," 'Left Helmet
'  SolModCallBack(22) = "SetModLamp 122," 'Right Helmet
'  SolModCallBack(23) = "SetModLamp 123," 'Jets Enter
'  SolModCallBack(24) = "SetModLamp 124," 'Left Loop
 
'#end section
reDim CollapseMe(2) 'InitLamps  (Click Me to Collapse)
    Sub InitLamps() 'set fading speeds and other stuff here
        GetOpacity aLampsAll    'All non-GI lamps and flashers go in this object array for compensation script!
        Dim x
        for x = 0 to uBound(LampState)
            LampState(x) = 0    ' current light state, independent of the fading level. 0 is off and 1 is on
            FadingLevel(x) = 4  ' used to track the fading state
            FlashSpeedUp(x) = 0.1   'Fading speeds in opacity per MS I think (Not used with nFadeL or nFadeLM subs!)
            FlashSpeedDown(x) = 0.1
           
            FlashMin(x) = 0.001         ' the minimum value when off, usually 0
            FlashMax(x) = 1             ' the minimum value when off, usually 1
            FlashLevel(x) = 0.001       ' Raw Flasher opacity value. Start this >0 to avoid initial flasher stuttering.
           
            SolModValue(x) = 0          ' Holds SolModCallback values
           
        Next
       
        for x = 0 to uBound(giscale)
            Giscale(x) = 1.625          ' lamp GI compensation multiplier, eg opacity x 1.625 when gi is fully off
        next
           
        for x = 11 to 110 'insert fading levels (only applicable for lamps that use FlashC sub)
            FlashSpeedUp(x) = 0.015
            FlashSpeedDown(x) = 0.009
        Next       
       
        for x = 111 to 186  'Flasher fading speeds 'intensityscale(%) per 10MS
            FlashSpeedUp(x) = 1.1
            FlashSpeedDown(x) = 0.9
        next
       
        for x = 200 to 203      'GI relay on / off  fading speeds
            FlashSpeedUp(x) = 0.01
            FlashSpeedDown(x) = 0.008
            FlashMin(x) = 0
        Next
        for x = 300 to 303      'GI 8 step modulation fading speeds
            FlashSpeedUp(x) = 0.01
            FlashSpeedDown(x) = 0.008
            FlashMin(x) = 0
        Next
       
        UpdateGIon 0, 1:UpdateGIon 1, 1: UpdateGIon 2, 1 : UpdateGIon 3, 1:UpdateGIon 4, 1
        UpdateGI 0, 7:UpdateGI 1, 7:UpdateGI 2, 7 : UpdateGI 3, 7:UpdateGI 4, 7
    End Sub
 
    Sub GetOpacity(a)   'Keep lamp/flasher data in an array
        Dim x
        for x = 0 to (a.Count - 1)
            On Error Resume Next
            if a(x).Opacity > 0 then a(x).Uservalue = a(x).Opacity
            if a(x).Intensity > 0 then a(x).Uservalue = a(x).Intensity
            If a(x).FadeSpeedUp > 0 then LampsOpacity(x, 1) = a(x).FadeSpeedUp : LampsOpacity(x, 2) = a(x).FadeSpeedDown
        Next
        for x = 0 to (a.Count - 1) : LampsOpacity(x, 0) = a(x).UserValue : Next
    End Sub
 
    sub DebugLampsOn(input):Dim x: for x = 10 to 100 : setlamp x, input : next :  end sub
 
'#end section
 
reDim CollapseMe(3) 'LampTimer  (Click Me to Collapse)
    LampTimer.Interval = -1 '-1 is ideal, but it will technically work with any timer interval
    Dim FrameTime, InitFadeTime : FrameTime = 10    'Count Frametime
    Sub LampTimer_Timer()
        FrameTime = gametime - InitFadeTime
        Dim chgLamp, num, chg, ii
        chgLamp = Controller.ChangedLamps
        If Not IsEmpty(chgLamp) Then
            For ii = 0 To UBound(chgLamp)
                LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
                FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
            Next
        End If
       
        UpdateGIstuff
        UpdateLamps
        UpdateFlashers
 
        InitFadeTime = gametime
    End Sub
'#end section
reDim CollapseMe(4) 'ASSIGNMENTS: Lamps, GI, and Flashers (Click Me to Collapse)
    Sub UpdateGIstuff()

    End Sub
   
    Sub UpdateFlashers()


    End Sub
   
    Sub UpdateLamps()


FadeGI 200
UpdateGIobjectsSingle 200, theGicollection
GiCompensationSingle 200, aLampsAll, GIscale(0)
FadeLUTsingle 200, "LUTCont_", 28
SubtleDL 200, pColorRamp

	If LampState(3) = 1 Then
		If MusicSnippet = 1 And PrevGameOver = 0 Then	
			PlaySound "intro"
			PrevGameOver = 1
		End If
	else 

	End If


 	NFadeL 1, l1
	NFadeL 2, l2
	NFadeL 3, l3
	NFadeL 4, l4
	NFadeL 5, l5
	NFadeL 6, l6
	NFadeL 7, l7
	NFadeL 8, l8
	NFadeL 9, l9
	NFadeL 10, l10
	NFadeL 11, l11
	NFadeL 12, l12
	NFadeL 13, l13
	NFadeL 14, l14
	FadeMaterialP 15, pHillSignLeftA, TextureArray1	
	NFadeL 15, l15
	NFadeL 16, l16
	NFadeL 17, l17
	NFadeL 18, l18
	NFadeL 19, l19
	NFadeL 20, l20
	NFadeL 21, l21
	NFadeL 22, l22
	NFadeL 23, l23
	NFadeL 24, l24
	NfadeL 25, l25d
	NfadeL 26, l26d
	NfadeL 27, l27d
	NfadeL 28, l28d
	NfadeL 29, l29d
	NfadeL 30, l30d
	NfadeL 31, l31d
	NfadeL 32, l32d
	NFadeL 33, l33
    FadeR 34, l34
    FadeR 35, l35
    FadeR 36, l36
    FadeR 37, l37
    FadeR 38, l38
    FadeR 39, l39
	NFadeL 40, l40
	NFadeL 41, l41
	NFadeL 42, l42
	NFadeL 43, l43
	NFadeL 44, l44
	NFadeL 45, l45
	FadeMaterialP 46, pHillSignRightA, TextureArray1	
	NFadeL 46, l46
    FadeRm 47, l47c
	FadeMaterialP 47, pHillSignRightA, TextureArray1	
	NFadeL 47, l47
	FadeMaterialP 48, pHillSignRightA, TextureArray1	
	NFadeL 48, l48
	NFadeL 49, l49
	NFadeL 50, l50
	NFadeL 51, l51
	NFadeL 52, l52
	NFadeL 53, l53
	NFadeL 54, l54
	NFadeL 55, l55
    FadeRm 56, l56c
	FadeMaterialP 56, pHillSignLeftA, TextureArray1	
	NFadeL 56, l56
	NFadeL 57, l57
	NFadeL 58, l58
	NFadeL 59, l59
	FadeMaterialP 60, pHillSignLeftA, TextureArray1	
	NFadeL 60, l60
	NFadeL 61, l61
	NFadeL 62, l62
	NFadeL 63, l63
	NFadeL 64, l64

    'Flashers
	NFadeLm 101, f1a
	NFadeLm 101, f1b
	NFadeLm 101, f1c
	NFadeL 101, f1d

	NFadeLm 102, f2a
	NFadeLm 102, f2b
	NFadeLm 102, f2c
	NFadeL 102, f2d

	NFadeLm 103, f3a
	NFadeLm 103, f3b
	NFadeLm 103, f3c
	NFadeL 103, f3d

	FadeMaterial2P 104, pWabblePlastic0, TextureArray1
	FadeDisableLighting 104, Primitive10	
	FadeMaterialP 104, Primitive10, TextureArray1
	NFadeLm 104, f4a
	NFadeL 104, f4b

	FadeDisableLighting 105, Primitive3
	FadeMaterialP 105, Primitive3, TextureArray1
	NFadeLm 105, f5a
	NFadeL 105, f5b

	NFadeLm 106, f6d1
	NFadeLm 106, f6d2
	NFadeLm 106, f6c1
	NFadeLm 106, f6c2
	NFadeLm 106, f6a
	NFadeL 106, f6b

	FadeDisableLighting 108, pPlasticClockTower
	FadeMaterial2P 108, pPlasticClockTower, TextureArray1
	FadeDisableLighting 108, Primitive2
	FadeMaterialP 108, Primitive2, TextureArray1

	NFadeLm 108, f8ca 
	NFadeL 108, f8cb 

	NFadeLm 109, f9d1
	NFadeLm 109, f9d2
	NFadeLm 109, f9c1
	NFadeLm 109, f9c2
	NFadeLm 109, f9a
	NFadeL 109, f9b

	FadeDisableLighting 112, Primitive1
	FadeMaterialP 112, Primitive1, TextureArray1	
	NFadeLm 112, f12a
	NFadeL 112, f12b



	NFadeL 113, f13
	NFadeL 114, f14
	NFadeL 115, f15

	FadeDisableLighting 116, Primitive16
	FadeMaterialP 116, Primitive16, TextureArray1
	NFadeLm 116, f16a
	NFadeL 116, f16b
 
    End Sub
   
'#end section


''''Additions by CP

Dim aa


Sub FadeDisableLighting(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.DisableLighting = 0
        Case 5:a.DisableLighting = 1
    End Select
End Sub

Sub SubtleDL(nr, aa)
    Select Case FadingLevel(nr)

		Case 0:aa.DisableLighting = 0
		Case 1:aa.DisableLighting = .01
		Case 2:aa.DisableLighting = .02
		Case 3:aa.DisableLighting = .03
        Case 4:aa.DisableLighting = .04
        Case 5:aa.DisableLighting = .05
    End Select
End Sub

'trxture swap	
dim itemw, itemp, itemp2

Sub FadeMaterialW(nr, itemw, group)
    Select Case FadingLevel(nr)
        Case 4:itemw.TopMaterial = group(1):itemw.SideMaterial = group(1)
        Case 5:itemw.TopMaterial = group(0):itemw.SideMaterial = group(0)
    End Select
End Sub


Sub FadeMaterialP(nr, itemp, group)
    Select Case FadingLevel(nr)
        Case 4:itemp.Material = group(1)
        Case 5:itemp.Material = group(0)
    End Select
End Sub


Sub FadeMaterial2P(nr, itemp2, group)
    Select Case FadingLevel(nr)
        Case 4:itemp2.Material = group(1)
        Case 5:itemp2.Material = group(0)
    End Select
End Sub


'Reels

Sub FadeR(nr, a)
    Select Case FadingLevel(nr)
        Case 2:a.SetValue 3:FadingLevel(nr) = 0
        Case 3:a.SetValue 2:FadingLevel(nr) = 2
        Case 4:a.SetValue 1:FadingLevel(nr) = 3
        Case 5:a.SetValue 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub FadeRm(nr, a)
    Select Case FadingLevel(nr)
        Case 2:a.SetValue 3
        Case 3:a.SetValue 2
        Case 4:a.SetValue 1
        Case 5:a.SetValue 1
    End Select
End Sub


''''End Of Additions by CP


 
reDim CollapseMe(5) 'Combined GI subs / functions (Click Me to Collapse)
    Set GICallback = GetRef("UpdateGIon")       'On/Off GI to NRs 200-203
    Sub UpdateGIOn(no, Enabled) : Setlamp no+200, cInt(enabled) : End Sub  
   
    Set GICallback2 = GetRef("UpdateGI")
    Sub UpdateGI(no, step)                      '8 step Modulated GI to NRs 300-303
        Dim ii, x', i
        If step = 0 then exit sub 'only values from 1 to 8 are visible and reliable. 0 is not reliable and 7 & 8 are the same so...
        SetModLamp no+300, ScaleGI(step, 0)
        LampState((no+300)) = 0
    '   if no = 2 then tb.text = no & vbnewline & step & vbnewline & ScaleGI(step,0) & SolModValue(102)
    End Sub
 
    Function ScaleGI(value, scaletype)  'returns an intensityscale-friendly 0->100% value out of 1>8 'it does go to 8
        Dim i
        Select Case scaletype   'select case because bad at maths
            case 0  : i = value * (1/8) '0 to 1
            case 25 : i = (1/28)*(3*value + 4)
            case 50 : i = (value+5)/12
            case else : i = value * (1/8)   '0 to 1
    '           x = (4*value)/3 - 85    '63.75 to 255
        End Select
        ScaleGI = i
    End Function
 
'   Dim LSstate : LSstate = False   'fading sub handles SFX 'Uncomment to enable
    Sub FadeGI(nr) 'in On/off       'Updates nothing but flashlevel
        Select Case FadingLevel(nr)
            Case 3 : FadingLevel(nr) = 0
            Case 4 'off
    '           If Not LSstate then Playsound "FX_Relay_Off",0,LVL(0.1) : LSstate = True    'handle SFX
                FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * FrameTime)
                If FlashLevel(nr) < FlashMin(nr) Then
                   FlashLevel(nr) = FlashMin(nr)
                   FadingLevel(nr) = 3 'completely off
    '               LSstate = False
                End if
            Case 5 ' on
    '           If Not LSstate then Playsound "FX_Relay_On",0,LVL(0.1) : LSstate = True 'handle SFX
                FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * FrameTime)
                If FlashLevel(nr) > FlashMax(nr) Then
                    FlashLevel(nr) = FlashMax(nr)
                    FadingLevel(nr) = 6 'completely on
    '               LSstate = False
                End if
            Case 6 : FadingLevel(nr) = 1
        End Select
    End Sub
    Sub ModGI(nr2) 'in 0->1     'Updates nothing but flashlevel 'never off
        Dim DesiredFading
        Select Case FadingLevel(nr2)
            case 3 : FadingLevel(nr2) = 0   'workaround - wait a frame to let M sub finish fading
    '       Case 4 : FadingLevel(nr2) = 3   'off -disabled off, only gicallback1 can turn off GI(?) 'experimental
            Case 5, 4 ' Fade (Dynamic)
                DesiredFading = SolModValue(nr2)
                if FlashLevel(nr2) < DesiredFading Then '+
                    FlashLevel(nr2) = FlashLevel(nr2) + (FlashSpeedUp(nr2)  * FrameTime )
                    If FlashLevel(nr2) >= DesiredFading Then FlashLevel(nr2) = DesiredFading : FadingLevel(nr2) = 1
                elseif FlashLevel(nr2) > DesiredFading Then '-
                    FlashLevel(nr2) = FlashLevel(nr2) - (FlashSpeedDown(nr2) * FrameTime    )
                    If FlashLevel(nr2) <= DesiredFading Then FlashLevel(nr2) = DesiredFading : FadingLevel(nr2) = 6
                End If
            Case 6
                FadingLevel(nr2) = 1
        End Select
    End Sub
 
    Sub UpdateGIobjects(nr, nr2, a) 'Just Update GI
        If FadingLevel(nr) > 1 or FadingLevel(nr2) > 1 Then
            Dim x, Output : Output = FlashLevel(nr2) * FlashLevel(nr)
            for each x in a : x.IntensityScale = Output : next
        End If
    end Sub
   
    Sub GiCompensation(nr, nr2, a, GIscaleOff)  'One NR pairing only fading
    '   tbgi.text = "GI: " & SolModValue(nr) & " " & FlashLevel(nr) & " " & FadingLevel(nr) & vbnewline & _
    '               "ModGI: " & SolModValue(nr2) & " " & FlashLevel(nr2) & " " & FadingLevel(nr2) & vbnewline & _
    '               "Solmodvalue, Flashlevel, Fading step"
        if FadingLevel(nr) > 1 or FadingLevel(nr2) > 1 Then
            Dim x, Giscaler, Output : Output = FlashLevel(nr2) * FlashLevel(nr)
            Giscaler = ((Giscaleoff-1) * (ABS(Output-1) )  ) + 1    'fade GIscale the opposite direction
 
            for x = 0 to (a.Count - 1) 'Handle Compensate Flashers
                On Error Resume Next
                a(x).Opacity = LampsOpacity(x, 0) * Giscaler
                a(x).Intensity = LampsOpacity(x, 0) * Giscaler
                a(x).FadeSpeedUp = LampsOpacity(x, 1) * Giscaler
                a(x).FadeSpeedDown = LampsOpacity(x, 2) * Giscaler
            Next
            '       tbbb.text = giscaler & " on:" & FadingLevel(nr) & vbnewline & "flash: " & output & " onmod:" & FadingLevel(nr2) & vbnewline & l37.intensity
            '       tbbb1.text = FadingLevel(nr) & vbnewline & FadingLevel(nr2)
            '   tbgi1.text = Output & " giscale:" & giscaler    'debug
        End If
        '       tbbb1.text = FLashLevel(nr) & vbnewline & FlashLevel(nr2)
    End Sub
   
    Sub GiCompensationAvg(nr, nr2, nr3, nr4, a, GIscaleOff) 'Two pairs of NRs averaged together
    '   tbgi.text = "GI: " & SolModValue(nr) & " " & FlashLevel(nr) & " " & FadingLevel(nr) & vbnewline & _
    '               "ModGI: " & SolModValue(nr2) & " " & FlashLevel(nr2) & " " & FadingLevel(nr2) & vbnewline & _
    '               "Solmodvalue, Flashlevel, Fading step"
        if FadingLevel(nr) > 1 or FadingLevel(nr2) > 1 or FadingLevel(nr3) > 1 or FadingLevel(nr4) > 1 Then
            Dim x, Giscaler, Output : Output = (((FlashLevel(nr)*FlashLevel(nr2)) + (FlashLevel(nr3)*Flashlevel(nr4))) /2)
            Giscaler = ((Giscaleoff-1) * (ABS(Output-1) )  ) + 1    'fade GIscale the opposite direction
 
            for x = 0 to (a.Count - 1) 'Handle Compensate Flashers
                On Error Resume Next
                a(x).Opacity = LampsOpacity(x, 0) * Giscaler
                a(x).Intensity = LampsOpacity(x, 0) * Giscaler
                a(x).FadeSpeedUp = LampsOpacity(x, 1) * Giscaler
                a(x).FadeSpeedDown = LampsOpacity(x, 2) * Giscaler
            Next
       
               
        REM tbgi1.text = "Output:" & output & vbnewline & _
                    REM "GIscaler" & giscaler & vbnewline & _
                    REM "..."
        End If
        REM tbgi.text = "GI0 " & flashlevel(200) & " " & flashlevel(300) & vbnewline & _
                    REM "GI1 " & flashlevel(201) & " " & flashlevel(301) & vbnewline & _
                    REM "GI2 " & flashlevel(202) & " " & flashlevel(302) & vbnewline & _
                    REM "GI3 " & flashlevel(203) & " " & flashlevel(303) & vbnewline & _
                    REM "GI4 " & flashlevel(204) & " " & flashlevel(304) & vbnewline & _
                    REM "..."
    End Sub
   
    Sub GiCompensationAvgM(nr, nr2, nr3, nr4, nr5, nr6, a, GIscaleOff)  'Three pairs of NRs averaged together
    '   tbgi.text = "GI: " & SolModValue(nr) & " " & FlashLevel(nr) & " " & FadingLevel(nr) & vbnewline & _
    '               "ModGI: " & SolModValue(nr2) & " " & FlashLevel(nr2) & " " & FadingLevel(nr2) & vbnewline & _
    '               "Solmodvalue, Flashlevel, Fading step"
        if FadingLevel(nr) > 1 or FadingLevel(nr2) > 1 Then
            Dim x, Giscaler, Output
            Output = (((FlashLevel(nr)*FlashLevel(nr2)) + (FlashLevel(nr3)*Flashlevel(nr4)) + (FlashLevel(nr5)*FlashLevel(nr6)))/3)
           
            Giscaler = ((Giscaleoff-1) * (ABS(Output-1) )  ) + 1    'fade GIscale the opposite direction
 
            for x = 0 to (a.Count - 1) 'Handle Compensate Flashers
                On Error Resume Next
                a(x).Opacity = LampsOpacity(x, 0) * Giscaler
                a(x).Intensity = LampsOpacity(x, 0) * Giscaler
                a(x).FadeSpeedUp = LampsOpacity(x, 1) * Giscaler
                a(x).FadeSpeedDown = LampsOpacity(x, 2) * Giscaler
            Next
            '       tbbb.text = giscaler & " on:" & FadingLevel(nr) & vbnewline & "flash: " & output & " onmod:" & FadingLevel(nr2) & vbnewline & l37.intensity
            '       tbbb1.text = FadingLevel(nr) & vbnewline & FadingLevel(nr2)
            '   tbgi1.text = Output & " giscale:" & giscaler    'debug
        End If
        '       tbbb1.text = FLashLevel(nr) & vbnewline & FlashLevel(nr2)
    End Sub
 
    Sub FadeLUT(nr, nr2, LutName, LutCount) 'fade lookuptable NOTE- this is a bad idea for darkening your table as
        If FadingLevel(nr) >2 or FadingLevel(nr2) > 2 Then              '-it will strip the whites out of your image
            Dim GoLut
            GoLut = cInt(LutCount * (FlashLevel(nr)*FlashLevel(nr2) )   )
            bttf.ColorGradeImage = LutName & GoLut
    '       tbgi2.text = Table1.ColorGradeImage & vbnewline & golut 'debug
        End If
    End Sub
   
    Sub FadeLUTavg(nr, nr2, nr3, nr4, LutName, LutCount)    'FadeLut for two GI strings (WPC)
        If FadingLevel(nr) >2 or FadingLevel(nr2) > 2 or FadingLevel(nr3) >2 or FadingLevel(nr4) > 2 Then
            Dim GoLut
            GoLut = cInt(LutCount * (((FlashLevel(nr)*FlashLevel(nr2)) + (FlashLevel(nr3)*Flashlevel(nr4))) /2) )
            bttf.ColorGradeImage = LutName & GoLut
            REM tbgi2.text = Table1.ColorGradeImage & vbnewline & golut 'debug
        End If
    End Sub
   
    Sub FadeLUTavgM(nr, nr2, nr3, nr4, nr5, nr6, LutName, LutCount) 'FadeLut for three GI strings (WPC)
        If FadingLevel(nr) >2 or FadingLevel(nr2) > 2 or FadingLevel(nr3) >2 or FadingLevel(nr4) > 2 or _
        FadingLevel(nr5) >2 or FadingLevel(nr6) > 2 Then
            Dim GoLut
            GoLut = cInt(LutCount * (((FlashLevel(nr)*FlashLevel(nr2)) + (FlashLevel(nr3)*Flashlevel(nr4)) + (FlashLevel(nr5)*FlashLevel(nr6)))/3)  )   'what a mess
            bttf.ColorGradeImage = LutName & GoLut
    '       tbgi2.text = Table1.ColorGradeImage & vbnewline & golut 'debug
        End If
    End Sub
   
'#end section
 
reDim CollapseMe(6) 'Fading subs     (Click Me to Collapse)
    Sub nModFlash(nr, object, scaletype, offscale)  'Fading with modulated callbacks
        Dim DesiredFading
        Select Case FadingLevel(nr)
            case 3 : FadingLevel(nr) = 0    'workaround - wait a frame to let M sub finish fading
            Case 4  'off
                If Offscale = 0 then Offscale = 1
                FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * FrameTime   ) * offscale
                If FlashLevel(nr) < 0 then FlashLevel(nr) = 0 : FadingLevel(nr) = 3
                Object.IntensityScale = ScaleLights(FlashLevel(nr),0 )
            Case 5 ' Fade (Dynamic)
                DesiredFading = ScaleByte(SolModValue(nr), scaletype)
                if FlashLevel(nr) < DesiredFading Then '+
                    FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * FrameTime )
                    If FlashLevel(nr) >= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
                elseif FlashLevel(nr) > DesiredFading Then '-
                    FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * FrameTime   )
                    If FlashLevel(nr) <= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 6
                End If
                Object.Intensityscale = ScaleLights(FlashLevel(nr),0 )
            Case 6 : FadingLevel(nr) = 1
        End Select
    End Sub
 
    Sub nModFlashM(nr, Object)
        Select Case FadingLevel(nr)
            Case 3, 4, 5, 6 : Object.Intensityscale = ScaleLights(FlashLevel(nr),0 )
        End Select
    End Sub
 
    Sub Flashc(nr, object)  'FrameTime Compensated. Can work with Light Objects (make sure state is 1 though)
        Select Case FadingLevel(nr)
            Case 3 : FadingLevel(nr) = 0
            Case 4 'off
                FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * FrameTime)
                If FlashLevel(nr) < FlashMin(nr) Then
                    FlashLevel(nr) = FlashMin(nr)
                   FadingLevel(nr) = 3 'completely off
                End if
                Object.IntensityScale = FlashLevel(nr)
            Case 5 ' on
                FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * FrameTime)
                If FlashLevel(nr) > FlashMax(nr) Then
                    FlashLevel(nr) = FlashMax(nr)
                    FadingLevel(nr) = 6 'completely on
                End if
                Object.IntensityScale = FlashLevel(nr)
            Case 6 : FadingLevel(nr) = 1
        End Select
    End Sub
 
    Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
        select case FadingLevel(nr)
            case 3, 4, 5, 6 : Object.IntensityScale = FlashLevel(nr)
        end select
    End Sub
   
    Sub NFadeL(nr, object)  'Simple VPX light fading using State
   Select Case FadingLevel(nr)
        Case 3:object.state = 0:FadingLevel(nr) = 0
        Case 4:object.state = 0:FadingLevel(nr) = 3
        Case 5:object.state = 1:FadingLevel(nr) = 6
        Case 6:object.state = 1:FadingLevel(nr) = 1
    End Select
    End Sub
 
    Sub NFadeLm(nr, object) ' used for multiple lights
        Select Case FadingLevel(nr)
            Case 3:object.state = 0
            Case 4:object.state = 0
            Case 5:object.state = 1
            Case 6:object.state = 1
        End Select
    End Sub
 
'#End Section
 
reDim CollapseMe(7) 'Fading Functions (Click Me to Collapse)
    Function ScaleLights(value, scaletype)  'returns an intensityscale-friendly 0->100% value out of 255
        Dim i
        Select Case scaletype   'select case because bad at maths   'TODO: Simplify these functions. B/c this is absurdly bad.
            case 0  : i = value * (1 / 255) '0 to 1
            case 6  : i = (value + 17)/272  '0.0625 to 1
            case 9  : i = (value + 25)/280  '0.089 to 1
            case 15 : i = (value / 300) + 0.15
            case 20 : i = (4 * value)/1275 + (1/5)
            case 25 : i = (value + 85) / 340
            case 37 : i = (value+153) / 408     '0.375 to 1
            case 40 : i = (value + 170) / 425
            case 50 : i = (value + 255) / 510   '0.5 to 1
            case 75 : i = (value + 765) / 1020  '0.75 to 1
            case Else : i = 10
        End Select
        ScaleLights = i
    End Function
 
    Function ScaleByte(value, scaletype)    'returns a number between 1 and 255
        Dim i
        Select Case scaletype
            case 0 : i = value * 1  '0 to 1
            case 9 : i = (5*(200*value + 1887))/1037 'ugh
            case 15 : i = (16*value)/17 + 15
            Case 63 : i = (3*(value + 85))/4
            case else : i = value * 1   '0 to 1
        End Select
        ScaleByte = i
    End Function
 
'#end section
 
reDim CollapseMe(8) 'Bonus GI Subs for games with only simple On/Off GI (Click Me to Collapse)
    Sub UpdateGIobjectsSingle(nr, a)    'An UpdateGI script for simple (Sys11 / Data East or whatever)
        If FadingLevel(nr) > 1 Then
            Dim x, Output : Output = FlashLevel(nr)
            for each x in a : x.IntensityScale = Output : next
        End If
    end Sub
   
    Sub GiCompensationSingle(nr, a, GIscaleOff) 'One NR pairing only fading
        if FadingLevel(nr) > 1 Then
            Dim x, Giscaler, Output : Output = FlashLevel(nr)
            Giscaler = ((Giscaleoff-1) * (ABS(Output-1) )  ) + 1    'fade GIscale the opposite direction
 
            for x = 0 to (a.Count - 1) 'Handle Compensate Flashers
                On Error Resume Next
                a(x).Opacity = LampsOpacity(x, 0) * Giscaler
                a(x).Intensity = LampsOpacity(x, 0) * Giscaler
                a(x).FadeSpeedUp = LampsOpacity(x, 1) * Giscaler
                a(x).FadeSpeedDown = LampsOpacity(x, 2) * Giscaler
            Next
        End If
        '       tbbb1.text = FLashLevel(nr) & vbnewline & FlashLevel(nr2)
    End Sub
   
    Sub FadeLUTsingle(nr, LutName, LutCount)    'fade lookuptable NOTE- this is a bad idea for darkening your table as
        If FadingLevel(nr) >2 Then              '-it will strip the whites out of your image
            Dim GoLut
            GoLut = cInt(LutCount * FlashLevel(nr)  )
            bttf.ColorGradeImage = LutName & GoLut
    '       tbgi2.text = Table1.ColorGradeImage & vbnewline & golut 'debug
        End If
    End Sub
   
'#end section
 
Sub theend() : End Sub
 
 
REM Troubleshooting :
REM Flashers/gi are intermittent or aren't showing up
REM Ensure flashers start visible, light objects start with state = 1
 
REM No lamps or no GI
REM Make sure these constants are set up this way
REM Const UseSolenoids = 1
REM Const UseLamps = 0
REM Const UseGI = 1
 
REM SolModCallback error
REM Ensure you have the latest scripts. Clear out any loose scripts in your tables that might be causing conflicts.
 
REM Table1 Error
REM Rename the table to Table1 or find/Replace table1 with whatever the table's name is
 
REM SolModCallbacks aren't sending anything
REM Two important things to get SolModCallbacks to initialize properly:
REM Put this at the top of the script, before LoadVPM
    REM Const UseVPMModSol = 1
REM Put this in the table1_Init() section
    REM vpmInit me

'***********************
'   Visible Locks
' Adapted to this table
' based on the core.vbs
'***********************

Class cvpmVLock2
    Private mTrig, mKick, mSw(), mSize, mBalls, mGateOpen, mRealForce, mBallSnd, mNoBallSnd
    Public ExitDir, ExitForce, KickForceVar

    Private Sub Class_Initialize
        mBalls = 0:ExitDir = 0:ExitForce = 0:KickForceVar = 0:mGateOpen = False
        vpmTimer.addResetObj Me
    End Sub

    Public Sub InitVLock(aTrig, aKick, aSw)
        Dim ii
        mSize = vpmSetArray(mTrig, aTrig)
        If vpmSetArray(mKick, aKick) <> mSize Then MsgBox "cvpmVLock: Unmatched kick+trig":Exit Sub
        On Error Resume Next
        ReDim mSw(mSize)
        If IsArray(aSw) Then
            For ii = 0 To UBound(aSw):mSw(ii) = aSw(ii):Next
        ElseIf aSw = 0 Or Err Then
            For ii = 0 To mSize:mSw(ii) = mTrig(ii).TimerInterval:Next
        Else
            mSw(0) = aSw
        End If
    End Sub

    Public Sub InitSnd(aBall, aNoBall):mBallSnd = aBall:mNoBallSnd = aNoBall:End Sub
    Public Sub CreateEvents(aName)
        Dim ii
        If Not vpmCheckEvent(aName, Me) Then Exit Sub
        For ii = 0 To mSize
            vpmBuildEvent mTrig(ii), "Hit", aName & ".TrigHit ActiveBall," & ii + 1

            vpmBuildEvent mTrig(ii), "Unhit", aName & ".TrigUnhit ActiveBall," & ii + 1

            vpmBuildEvent mKick(ii), "Hit", aName & ".KickHit " & ii + 1
        Next
    End Sub

    Public Sub SolExit(aEnabled)
        Dim ii
        mGateOpen = aEnabled
        If Not aEnabled Then Exit Sub
        If mBalls> 0 Then PlaySound mBallSnd:Else PlaySound mNoBallSnd:Exit Sub
        For ii = 0 To mBalls-1
            mKick(ii).Enabled = False:If mSw(ii) Then Controller.Switch(mSw(ii) ) = False
        Next
        '		If ExitForce > 0 Then ' Up
        '			mRealForce = ExitForce + (Rnd - 0.5)*KickForceVar : mKick(mBalls-1).Kick ExitDir, mRealForce
        '		Else ' Down
        mRealForce = ExitForce + (Rnd - 0.5) * KickForceVar:mKick(0).Kick ExitDir, mRealForce
    '		End If
    End Sub

    Public Sub Reset
        Dim ii:If mBalls = 0 Then Exit Sub
        For ii = 0 To mBalls-1
            If mSw(ii) Then Controller.Switch(mSw(ii) ) = True
        Next
    End Sub

    Public Property Get Balls:Balls = mBalls:End Property

    Public Property Let Balls(aBalls)
        Dim ii:mBalls = aBalls
        For ii = 0 To mSize
            If ii >= aBalls Then
                mKick(ii).DestroyBall:If mSw(ii) Then Controller.Switch(mSw(ii) ) = False
                Else
                    vpmCreateBall mKick(ii):If mSw(ii) Then Controller.Switch(mSw(ii) ) = True
            End If
        Next
    End Property

    Public Sub TrigHit(aBall, aNo)
        aNo = aNo - 1:If mSw(aNo) Then Controller.Switch(mSw(aNo) ) = True
        If aBall.VelY <-1 Then Exit Sub ' Allow small upwards speed
        If aNo = mSize Then mBalls = mBalls + 1
        If mBalls> aNo Then mKick(aNo).Enabled = Not mGateOpen
    End Sub

    Public Sub TrigUnhit(aBall, aNo)
        aNo = aNo - 1:If mSw(aNo) Then Controller.Switch(mSw(aNo) ) = False
        If aBall.VelY> -1 Then
            If aNo = 0 Then mBalls = mBalls - 1
            If aNo <mSize Then mKick(aNo + 1).Kick 0, 0
            Else
                If aNo = mSize Then mBalls = mBalls - 1
                If aNo> 0 Then mKick(aNo-1).Kick ExitDir, mRealForce
        End If
    End Sub

    Public Sub KickHit(aNo):mKick(aNo-1).Enabled = False:End Sub
End Class



'*****************************************
'			BALL SHADOW
'*****************************************
Sub BallShadowUpdate_timer()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7)
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
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10                       
        If BOT(b).Z > 20 and BOT(b).Z < 140 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 110
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub



' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 200)*1.2
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
    VolZ = Csng(BallVelZ(ball) ^2 / 200)*1.2
End Function


Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / bttf.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

function AudioFade(ball)
    Dim tmp
    tmp = ball.y * 2 / bttf.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
    BallVelZ = INT((ball.VelZ) * -1 )
End Function

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


'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 7 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub CollisionTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

       ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 Then
			rolling(b) = True
			if BOT(b).z < 30 Then ' Ball on playfield
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.7, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
			Else ' Ball on raised ramp
				If OnWireRamp = 1 Then
						PlaySound("metalrolling" & b), -1, Vol(BOT(b) )*.5, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) ) ' vol was *.7 then nothing
				Else
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, Pan(BOT(b) ), 0, Pitch(BOT(b) )+60000, 1, 0, AudioFade(BOT(b) ) ' vol was *.5 then nothing
				End If
				End If
		Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

		'Ball Drop Hits

		If BOT(b).VelZ < 0 and BOT(b).z < 60 and BOT(b).z > 40 Then
			If InRect(BOT(b).x,BOT(b).y,(pRampWedge1.x-40),(pRampWedge1.y-40),(pRampWedge1.x+40),(pRampWedge1.y-40),(pRampWedge1.x+40),(pRampWedge1.y+40),(pRampWedge1.x-40),(pRampWedge1.y+40)) Then
			ElseIf InRect(BOT(b).x,BOT(b).y,(pRampWedge2.x-40),(pRampWedge2.y-40),(pRampWedge2.x+40),(pRampWedge2.y-40),(pRampWedge2.x+40),(pRampWedge2.y+40),(pRampWedge2.x-40),(pRampWedge2.y+40)) Then
			Else
				PlaySoundAtBOTBallZ "Ball_Bounce", BOT(b)
				'debug.print BOT(b).z
			End If
		End If

    Next
End Sub

'RampHelpers

Dim OnWireRamp

Sub tRampHelper1a_hit()
	OnWireRamp = 1
End Sub

Sub tRampHelper1b_hit()
	OnWireRamp = 0
    StopSound("metalrolling")
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 200, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******************************************************
' 				JP's Sound Routines
'******************************************************

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub aRubbers_Hit(idx)
    PlaySoundAtBallVol "rubber_hit_" & Int(Rnd*3)+1, .8
End Sub

Sub aRubbers2_Hit(idx)
    PlaySoundAtBallVol "rubber_hit_" & Int(Rnd*3)+1, .8
End Sub

Sub Posts_Hit(idx)
    PlaySoundAtBallVol "rubber_hit_" & Int(Rnd*3)+1, .8
End Sub


Sub LeftFlipper_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 2
End Sub

Sub RightFlipper_Collide(parm)
    PlaySoundAtBallVol "flip_hit_" & Int(Rnd*3)+1, 2
End Sub

''Flux Ramp Sounds
Sub PlasticRampHit1_Hit:PlaySound "PlasticRamp_Hit3", 0, 0, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall): End Sub
Sub PlasticRampHit2_Hit:PlaySound "PlasticRamp_Hit1", 0, 0, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 1:tWobblePlastic.Enabled = True:End If: End Sub
Sub PlasticRampHit3_Hit:PlaySound "PlasticRamp_Hit4", 0, 0, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 2:tWobblePlastic.Enabled = True:End If: End Sub
Sub PlasticRampHit4_Hit:PlaySound "PlasticRamp_Hit2", 0, 0, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):If WobblePlastic = 1 then:PlasticWobbling = 1:WobbleCount = 3:tWobblePlastic.Enabled = True:End If: End Sub
Sub PlasticRampHit5_Hit:PlaySound "PlasticRamp_Hit3", 0, 0, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall): End Sub

''Color Ramp Sounds
Sub Sound1_Hit:PlaySound "rail_low_slowerNOTUSED", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):End Sub
Sub Sound2_Hit()
	PlaySound "rail_low_slowerNOTUSED", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):
	BallsLocked = BallsLocked - 1
	If HologramPhoto = 1 Then
		If IsItMultiball = 1 then
			IsItMultiballTimer.Enabled = true
		Else
			HologramUpdateStep = HologramUpdateStep - 2
			HologramUpdate
		End If
	End If
End Sub

'**************************************************************************
'                 Positional Sound Playback Functions by DJRobX
'**************************************************************************

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
		PlaySound sound, 1, Vol, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
		PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
		PlaySound sound, 0, VolZ(BOT), Pan(BOT), 0, Pitch(BOT), 1, 1, AudioFade(BOT)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
		PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

'*****************************
'Random Ramp and Orbit Sounds
'*****************************

Dim NextOrbitHit:NextOrbitHit = 0 

Sub WireRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump3 .1, Pitch(ActiveBall)+5
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .4 + (Rnd * .2)
	end if 
End Sub

Sub PlasticRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .4 and Timer > NextOrbitHit then
		RandomBump 5, Pitch(ActiveBall)
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub


Sub MetalGuideBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump2 2, Pitch(ActiveBall)
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub

Sub MetalWallBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 2, 20000 'Increased pitch to simulate metal wall
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub


' Requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "fx_rampbump" & CStr(Int(Rnd*7)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' Requires metalguidebump1 to 2 in Sound Manager
Sub RandomBump2(voladj, freq)
	dim BumpSnd:BumpSnd= "metalguidebump" & CStr(Int(Rnd*2)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' Requires WireRampBump1 to 5 in Sound Manage
Sub RandomBump3(voladj, freq)
	dim BumpSnd:BumpSnd= "WireRampBump" & CStr(Int(Rnd*5)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub



' Stop Bump Sounds
Sub BumpSTOP1_Hit ()
dim i:for i=1 to 4:StopSound "WireRampBump" & i:next
NextOrbitHit = Timer + 1
End Sub

Sub BumpSTOP2_Hit ()
dim i:for i=1 to 4:StopSound "PlasticRampBump" & i:next
NextOrbitHit = Timer + 1
End Sub

Sub bttf_Exit()
	Controller.Pause = False
	Controller.Stop
	If UseFlexDMD then
		If Not FlexDMD is Nothing Then 
			FlexDMD.Show = False
			FlexDMD.Run = False
			FlexDMD = NULL
		End if
		Controller.Games(cGameName).Settings.Value("showpindmd") = ExternalEnabled
	End if
End Sub



'*****************************
' Flex DMD Display
'*****************************


'**********************************************************
'  2*216 segment alphannumeric to flexdmd display conversion
'**********************************************************
Dim FlexDMD
DIm FlexDMDDict
Dim FlexDMDScene
Dim ExternalEnabled

Sub FlexDMD_Init() 'default/startup values

	' flex dmd variables
	DIm FlexDMDFont
	Dim FlexPath

	' populate the lookup dictionary for mapping display characters
	FlexDictionary_Init

	
	On Error Resume Next

	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
	If Not FlexDMD is Nothing Then
	
		
		FlexDMD.GameName = cGameName
		FlexDMD.TableFile = bttf.Filename & ".vpx"
		FlexDMD.RenderMode = 2
		FlexDMD.Width = 128
		FlexDMD.Height = 32
		FlexDMD.Clear = True
		FlexDMD.Run = True

		FlexDMD.LockRenderThread

		Set FlexDMDScene = FlexDMD.NewGroup("Scene")
		

		With FlexDMDScene
		
			'populate background 
			.AddActor FlexDMD.NewImage("Back", "VPX.DMD_Background")
			'32 segment display holders
			dim i 
			For i = 0 to 15
				'line 1
				.AddActor FlexDMD.NewImage("Seg" & cstr(i), "VPX.DMD_Space")
				.GetImage("Seg" & cstr(i)).SetAlignedPosition i * 8,1,0
				'line 2
				.AddActor FlexDMD.NewImage("Seg" & cstr(i + 16), "VPX.DMD_Space")
				.GetImage("Seg" & cstr(i + 16)).SetAlignedPosition i * 8,17,0
			Next 

			.AddActor FlexDMD.NewImage("Lightning", "VPX.DMD_Lightning_" & "1")
			.GetImage("Lightning").SetAlignedPosition 0,0,0
			.GetImage("Lightning").Visible = False

		End With
	
		FlexDMD.Stage.AddActor FlexDMDScene
		
		FlexDMD.Show = True
		FlexDMD.UnlockRenderThread

		FlexTimer.Enabled = True
		FlexTimer.Interval = 30

	Else
		
		UseFlexDMD = 0
	
	End If

	On Error Goto 0

End Sub


Sub FlexDictionary_Init

	Set FlexDMDDict = CreateObject("Scripting.Dictionary")

	FlexDMDDict.Add 0, "VPX.DMD_Space"

	FlexDMDDict.Add 63, "VPX.DMD_0"
	FlexDMDDict.Add 6, "VPX.DMD_1"
	FlexDMDDict.Add 2139, "VPX.DMD_2"
	FlexDMDDict.Add 2127, "VPX.DMD_3"
	FlexDMDDict.Add 2150, "VPX.DMD_4"
	FlexDMDDict.Add 2157, "VPX.DMD_5"
	FlexDMDDict.Add 2173, "VPX.DMD_6"
	FlexDMDDict.Add 7, "VPX.DMD_7"
	FlexDMDDict.Add 2175,"VPX.DMD_8"
	FlexDMDDict.Add 2151,"VPX.DMD_9"

	FlexDMDDict.Add 32959,"VPX.DMD_0C"
	FlexDMDDict.Add 32902, "VPX.DMD_1C"
	FlexDMDDict.Add 35035, "VPX.DMD_2C"
	FlexDMDDict.Add 35023, "VPX.DMD_3C"
	FlexDMDDict.Add 35046, "VPX.DMD_4C"
	FlexDMDDict.Add 35053, "VPX.DMD_5C"
	FlexDMDDict.Add 35069, "VPX.DMD_6C"
	FlexDMDDict.Add 32903, "VPX.DMD_7C"
	FlexDMDDict.Add 35071, "VPX.DMD_8C"
	FlexDMDDict.Add 35047, "VPX.DMD_9C"
	
	FlexDMDDict.Add 2167, "VPX.DMD_A"
	FlexDMDDict.Add 10767, "VPX.DMD_B"
	FlexDMDDict.Add 57, "VPX.DMD_C"
	FlexDMDDict.Add 8719, "VPX.DMD_D"
	FlexDMDDict.Add 121, "VPX.DMD_E"
	FlexDMDDict.Add 113, "VPX.DMD_F"
	FlexDMDDict.Add 2109, "VPX.DMD_G"
	FlexDMDDict.Add 2166, "VPX.DMD_H"
	FlexDMDDict.Add 8713, "VPX.DMD_I"
	FlexDMDDict.Add 30, "VPX.DMD_J"
	FlexDMDDict.Add 5232, "VPX.DMD_K"
	FlexDMDDict.Add 56, "VPX.DMD_L"
	FlexDMDDict.Add 1334, "VPX.DMD_M"
	FlexDMDDict.Add 4406, "VPX.DMD_N"
	FlexDMDDict.Add 2163, "VPX.DMD_P"
	FlexDMDDict.Add 4159, "VPX.DMD_Q"
	FlexDMDDict.Add 6259, "VPX.DMD_R"
	FlexDMDDict.Add 8705, "VPX.DMD_T"
	FlexDMDDict.Add 62, "VPX.DMD_U"
	FlexDMDDict.Add 17456, "VPX.DMD_V"
	FlexDMDDict.Add 20534, "VPX.DMD_W"
	FlexDMDDict.Add 21760, "VPX.DMD_X"
	FlexDMDDict.Add 9472, "VPX.DMD_Y"
	FlexDMDDict.Add 17417, "VPX.DMD_Z"

	FlexDMDDict.Add 50224, "VPX.DMD_Vd"
	FlexDMDDict.Add 54528, "VPX.DMD_Xd"
	FlexDMDDict.Add 34934, "VPX.DMD_Hd"
	FlexDMDDict.Add 34925, "VPX.DMD_5d"
	FlexDMDDict.Add 41473, "VPX.DMD_Td"
	FlexDMDDict.Add 41487, "VPX.DMD_Dd"
	FlexDMDDict.Add 32825, "VPX.DMD_Cd"

	FlexDMDDict.Add 15, "VPX.DMD_Ddelorean"
	FlexDMDDict.Add 2136, "VPX.DMD_CLower"

	FlexDMDDict.Add 17408, "VPX.DMD_FSlash"
	FlexDMDDict.Add 8, "VPX.DMD_Underscore"
	FlexDMDDict.Add 2112, "VPX.DMD_Minus"
	FlexDMDDict.Add 16640, "VPX.DMD_GreaterThan"
	FlexDMDDict.Add 5120, "VPX.DMD_LessThan"
	FlexDMDDict.Add 32768, "VPX.DMD_Dot"
	FlexDMDDict.Add 256, "VPX.DMD_ApostB"
	FlexDMDDict.Add 1024, "VPX.DMD_Apost"
	FlexDMDDict.Add 10816, "VPX.DMD_Plus"
	'FlexDMDDict.Add 10861, "VPX.DMD_Dollar" 'dollar can appear as transitional character
	
End sub


Sub UpdateFlexChar(id, value)

	If id < 32 Then
		if FlexDMDDict.Exists (value) then
			FlexDMDScene.GetImage("Seg" & id).Bitmap = FlexDMD.NewImage("", FlexDMDDict.Item (value)).Bitmap
		Else
			'FlexDMDScene.GetImage("Seg" & id).Bitmap = FlexDMD.NewImage("", "VPX.DMD_Space").Bitmap
		end if
	End If

End Sub

Sub FlexTimer_Timer


		Dim ChgLED, ii, num, stat
		ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)

		If Not IsEmpty(ChgLED)Then

			FlexDMD.LockRenderThread
			
			For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : stat=chgLED(ii, 2)
				UpdateFlexChar num, stat
			Next

			if FlexDMDScene.GetImage("Lightning").Visible = False then
				if (LampState(106) = 1 and LampState(109) = 1) or  (LampState(104) = 1 and LampState(114) = 1)   then 
					FlexLightning
				End if
			else
				FlexDMDScene.GetImage("Lightning").Visible = False
			end if

			FlexDMD.UnlockRenderThread

		End if
		
End Sub


Sub FlexLightning

	FlexDMDScene.GetImage("Lightning").Bitmap = FlexDMD.NewImage("", "VPX.DMD_Lightning_" & Int(4 * Rnd) + 1).Bitmap
	FlexDMDScene.GetImage("Lightning").SetAlignedPosition Int(100 * Rnd),0,0
	FlexDMDScene.GetImage("Lightning").Visible = True

End sub
 