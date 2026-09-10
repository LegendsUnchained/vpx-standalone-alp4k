Option Explicit
Randomize

'*****************************************************************************************************
' Teenage Mutant Ninja Turtles
' IPDB No. 2509 / Data East May, 1991 / 4 Players
' VPX version 1.1 - Cyberpez, VR Convertion Retro27
'*****************************************************************************************************

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0


Dim EnableBallControl, ballmod, FlipperMod, BlacklightOoze, BlacklightLaneGuides, PostsColor, BlacklightPegs, PlasticProtectors, LBCOnorOff, SideFlasherColor, GIColorMod, RubberMod, TurtleWeapons, CustomICs, GameType, TurtlesColorMod
EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys

'***************************************************************************'
'***************************************************************************'
'							  VR ROOM OPTIONS
'***************************************************************************'
'***************************************************************************'

'VR Room
'0= No VR Room
'1= Show VR Room
Const VR_Room = 0

'Scratched Glass
'0= None
'1= Normal
'2= More
'3= Less
Const Sglass = 3

'Flyer Poster
'0= No Poster
'1= Show Flyer Poster
Const Poster = 1

'Logo
'0= No logo
'1= Show Logo
Const VRlogo = 1

'Topper
'0= No Topper
'1= Show Topper 
const topper = 1

'Side Blades
'0= Standard Green
'1= Standard Black 
'2= Custom Green by Retro Refurds. https://www.retrorefurbs.com/
'3= Custom by Pinball Centre. https://www.pinball.center/en/
Const Sblades = 0

'DMD Reflection
'0= No DMD Reflection
'1= Show DMD Reflection
Const DMDreflection = 1

'Backglass Reflection
'0= No Backglass Reflection
'1= Show Backglass Reflection
Const BGreflection = 1

'VR Room Interactive Lighting
'0= No Interactive Lighting
'1= Show Interactive Lighting
Const Roomfade = 0

'Enable B2S Backglass
' 0 = B2S Off
' 1 = B2S On
Const B2Son1 = 0

'***************************************************************************'
'***************************************************************************'
'							  TABLE OPTIONS
'***************************************************************************'
'***************************************************************************'

Const BallBright = 0				'0 - Normal, 1 - Bright
Const VolumeDial = 0.8
Const BallRollVolume = 0.5 			'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5 			'Level of ramp rolling volume. Value between 0 and 1


'GI ColorMod
'0= Random
'1= Normal
'2= CoolWhite
'3= MultiColor
'4= AllGreen
GIColorMod = 4

'T U R T L E S color mod
'0= Normal
'1= Green
TurtlesColorMod = 1

'Flipper Colors
'0= Random
'1= White/Red
'2= White/Black
'3= White/Yellow
'4= White/Green
'5= White/BlackLightGreen
'6= Yellow/Red
'7= Yellow/Black
'8= Yellow/Green
'9= Yellow/BlackLightGreen
FlipperMod = 8

'Colored Rubbers
'0= Random
'1= White
'2= Black
'3= Colored
RubberMod = 2

'Instructions Card
'0= Random
'1= Normal
'2= Custom1
'3= Custom2
'4= Normal Green
CustomICs = 1

'0= FreePlay
'1= Coin
GameType = 1

'Plastic Protectors
'0= Random
'1= Clear
'2= OozeGreen
PlasticProtectors = 1

'LightBoxCover
'0= Off
'1= On
LBCOnorOff = 0

'SideFlasherColor
'0= Random
'1= Yellow
'2= Green
SideFlasherColor = 1

'Turtle Weapons
'0= Hidden
'1= Visible
TurtleWeapons = 0

'Post Colors
'0= Random
'1= Black
'2= Yellow
'3= Green
'4= Ooze Green
PostsColor = 3

'''''''''''''''''''''
' Blacklight things '
'''''''''''''''''''''

'Blacklight Ooooze
'0= Off
'1= On
BlacklightOoze = 1

'Blacklight LaneGuides
'0= Off
'1= On
BlacklightLaneGuides = 0

'Blacklight Pegs
'0= Off
'1= On
BlacklightPegs = 0

'***************************************************************************'
'***************************************************************************'
'						End of OPTIONS
'***************************************************************************'
'***************************************************************************'




' ball size
Const BallRadius = 25
Const BallMass = 1

Const tnob = 7

Const AmbientBallShadowOn = 1
Const DynamicBallShadowsOn = 1
Const RubberizerEnabled = 1
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7

Dim tablewidth: tablewidth = tmnt.width
Dim tableheight: tableheight = tmnt.height


' VPinMAME ROM name

Const cGameName 			= "tmnt_104"	



' ===============================================================================================
' some general constants and variables
' ===============================================================================================
	
Const UseSolenoids 		= 2
Const UseLamps 			= False
Const UseGI 			= True
Const UseSync 			= False
Const HandleMech 		= False

Const SSolenoidOn 		= "SOL_on"
Const SSolenoidOff 		= "SOL_off"
Const SCoin 			= ""
Const SKnocker 			= "Knocker"
Dim UseVPMDMD
    UseVPMDMD = true

LoadVPM "01560000", "DE.VBS", 3.26


' ===============================================================================================
' solenoids
' ===============================================================================================


SolCallback(sLLFlipper) = "solLFlipper"
SolCallback(sLRFlipper) = "solRFlipper"

Solcallback(1)  ="kisort"
SolCallback(2)	= "KickBallToLane"

SolCallback(4)	= "SolAutofire"			
SolCallBack(5)	= "SetLamp 105,"
SolCallBack(6)	= "SetLamp 106,"
SolCallback(7)	= "SewerUpKick"
SolCallback(8)	= "vpmSolSound ""knocker"","
SolCallBack(9)	= "SetLamp 109,"

SolCallback(11) = "SolGi" 'gi
SolCallBack(12)	= "SetLamp 112,"
SolCallBack(13)	= "SetLamp 113,"
SolCallBack(14) = "SetLamp 114,"
SolCallBack(16)	= "SewerOpen"
'SolCallBack(17)			    = "vpmSolSound ""bumper2"","
'SolCallBack(18)			    = "vpmSolSound ""bumper2"","
'SolCallBack(19)			    = "TurtleB"
'SolCallback(20)             = "vpmSolSound ""left_slingshot_new"","
'SolCallback(21)             = "vpmSolSound ""right_slingshot_new"","

SolCallBack(22) ="SolPizzaSpin"


SolCallBack(25)  = "SetLamp 125,"
SolCallBack(26)  = "SetLamp 126,"
SolCallBack(27)  = "SetLamp 127,"
SolCallBack(28)  = "SetLamp 128,"
SolCallBack(29)  = "SetLamp 129,"
SolCallBack(30)  = "SetLamp 130,"
SolCallBack(31)  = "SetLamp 131,"
SolCallBack(32)  = "SetLamp 132,"


Dim Ball(6)
Dim InitTime
Dim TroughTime
Dim EjectTime
Dim MaxBalls
Dim TroughCount
Dim TroughBall(7)
Dim TroughEject
Dim Momentum
Dim UpperGIon
Dim Multiball
Dim BallsInPlay
Dim	iBall
Dim	fgBall

Dim bsLEjet, bsUpperEject, Lnell, mNell, plungerIM, dtDrop, ttPizza, AutoPlunger


Sub InitVPM()
    With Controller
        .GameName = cGameName
'        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "TMNT" & vbNewLine & "VPX - cyberpez"
'        .Games(cGameName).Settings.Value("rol") = 0 'rotate DMD to the left
'        .HandleKeyboard = 0
'        .ShowTitle = 0
'        .ShowDMDOnly = 1
'        .ShowFrame = 0
'        .HandleMechanics = 0
'	  If DesktopMode = true then .hidden = 0 Else .hidden = 1 End If
'        .Run GetPlayerHWnd
'       If Err Then MsgBox Err.Description
 '       On Error Goto 0
    End With
     Controller.SolMask(0) = 0
     vpmTimer.AddTimer 4000, "Controller.SolMask(0)=&Hffffffff'" 
     Controller.Run
End Sub



Sub tmnt_Init
LoadLUT
	' table initialization
	InitVPM


		
	' basic pinmame timer
	PinMAMETimer.Interval	= PinMAMEInterval
	PinMAMETimer.Enabled	= True

	' nudging
	vpmNudge.TiltSwitch		= 1
	vpmNudge.Sensitivity	= 3
'	vpmNudge.TiltObj 		= Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)
 

    ' Impulse Plunger
    Const IMPowerSetting = 75 'Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set AutoPlunger = New cvpmImpulseP
    With AutoPlunger
        .InitImpulseP AutoPlung, IMPowerSetting, IMTime
        .Random 0
        '.switch 18
        .InitExitSnd "solon", ""
        .CreateEvents "AutoPlunger"
    End With

''''''''''''''''''''''''''''''''''''''
	' spinning Pizza
''''''''''''''''''''''''''''''''''''''
	Set ttPizza = New cvpmTurnTable
	With ttPizza
		.InitTurnTable PizzaTrigger, 150
		.SpinCW = True
		.SpinUp = 200 : .SpinDown = 200
		.CreateEvents "ttPizza"
	End With

	TurtleBall = 1
	CreatBalls

	If showDT = False Then
		l43.visible = False
		l44.visible = False
		l45.visible = False
		l46.visible = False
		l47.visible = False
		l48.visible = False
	Else
		l43.visible = True
		l44.visible = True
		l45.visible = True
		l46.visible = True
		l47.visible = True
		l48.visible = True
	End If

	SetOptions

	vpmInit me


End Sub



'//////////////////////////////////////////////////////////////////////
'// Ball
'//////////////////////////////////////////////////////////////////////

If BallBright Then
	tmnt.BallImage = "ball_HDR_brighter"
Else
	tmnt.BallImage = "ball_HDR"
End If

'//////////////////////////////////////////////////////////////////////
'// LUT
'//////////////////////////////////////////////////////////////////////

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

Dim LUTset, DisableLUTSelector, LutToggleSound, bLutActive
LutToggleSound = True
LoadLUT
'LUTset = 0			' Override saved LUT for debug
SetLUT
DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1

Sub tmnt_exit:SaveLUT:Controller.Stop:End Sub

Sub SetLUT  'AXS
	tmnt.ColorGradeImage = "LUT" & LUTset
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
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
		Case 16: LUTBox.text = "Skitso New ColorLut"
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

	if LUTset = "" then LUTset = 0 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "TMNTLUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub
Sub LoadLUT
    bLutActive = False
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=0
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "TMNTLUT.txt") then
		LUTset=0
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "TMNTLUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=0
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub

'//////////////////////////////////////////////////////////////////////
'// Keys
'//////////////////////////////////////////////////////////////////////

Sub tmnt_KeyDown(ByVal keycode)
If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
                Select Case Int(rnd*3)
                        Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
                        Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
                        Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
End Select
        End If
'LUT controls
If keycode = LeftMagnaSave Then bLutActive = True
    If keycode = RightMagnaSave Then
            If bLutActive Then
                    if DisableLUTSelector = 0 then
                LUTSet = LUTSet  - 1
                if LutSet < 0 then LUTSet = 16
                SetLUT
                ShowLUT
            End If
        End If
        End If
If keycode = PlungerKey Then Plunger.PullBack:SoundPlungerPull()

	If keycode = PlungerKey Then
		plunger.PullBack
		' Move VR plunger
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If

	If keycode = LeftTiltKey Then
		Nudge 90, 2
	End If

	If keycode = RightTiltKey Then
		Nudge 270, 2
	End If

	If keycode = CenterTiltKey Then
		Nudge 0, 2
	End If
    If vpmKeyDown(keycode) Then Exit Sub

	If keycode = 21 then  ''''''''''''''''''''y Key used for testing
		Kicker5.enabled = true
'		Kicker5.Kick 180,20
'		Kicker5.enabled = false
	End If

	If keycode = 22 then  ''''''''''''''''''''u Key used for testing
		Kicker5.Kick 165,400
		Kicker5.enabled = false
	End If

If keycode = LeftFlipperKey Then 
Primary_flipperbuttonleft.X = Primary_flipperbuttonleft.X + 10
end if
If keycode = RightFlipperKey Then 
Primary_flipperbuttonright.X = Primary_flipperbuttonright.X - 10
end if

If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress End If

If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress End If

If Keycode = StartGameKey Then		
Primary_startbutton1.y = Primary_startbutton1.y - 5
Primary_startbutton2.y = Primary_startbutton2.y - 5
End If

End Sub

Sub tmnt_KeyUp(ByVal keycode)

'LUT controls
If keycode = LeftMagnaSave Then bLutActive = False
    If KeyUpHandler(KeyCode) Then Exit Sub
	If keycode = PlungerKey Then
		Plunger.Fire
		SoundPlungerReleaseBall()
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True

	End If

If keycode = LeftFlipperKey Then 
Primary_flipperbuttonleft.X = Primary_flipperbuttonleft.X - 10
end if
If keycode = RightFlipperKey Then 
Primary_flipperbuttonright.X = Primary_flipperbuttonright.X + 10
end if

If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress End If

If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress End If 

'*****************************************************************************************************
'Move and Cancel ambient sound when Start button is pressed
'*****************************************************************************************************
If Keycode = StartGameKey Then		
Primary_startbutton1.y = Primary_startbutton1.y + 5
Primary_StartButton2.y = Primary_StartButton2.y + 5
'StopSound "ambientSound"
End If
End Sub




'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
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


' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
End Sub

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 80
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -2.7        
'        AddPt "Polarity", 2, 0.33, -2.7
'        AddPt "Polarity", 3, 0.37, -2.7        
'        AddPt "Polarity", 4, 0.41, -2.7
'        AddPt "Polarity", 5, 0.45, -2.7
'        AddPt "Polarity", 6, 0.576,-2.7
'        AddPt "Polarity", 7, 0.66, -1.8
'        AddPt "Polarity", 8, 0.743, -0.5
'        AddPt "Polarity", 9, 0.81, -0.5
'        AddPt "Polarity", 10, 0.88, 0
'
'        addpt "Velocity", 0, 0,         1
'        addpt "Velocity", 1, 0.16, 1.06
'        addpt "Velocity", 2, 0.41,         1.05
'        addpt "Velocity", 3, 0.53,         1'0.982
'        addpt "Velocity", 4, 0.702, 0.968
'        addpt "Velocity", 5, 0.95,  0.968
'        addpt "Velocity", 6, 1.03,         0.945
'
'        LF.Object = LeftFlipper        
'        LF.EndPoint = EndPointLp
'        RF.Object = RightFlipper
'        RF.EndPoint = EndPointRp
'End Sub
'
'
'
''*******************************************
'' Mid 80's
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 80
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -3.7        
'        AddPt "Polarity", 2, 0.33, -3.7
'        AddPt "Polarity", 3, 0.37, -3.7
'        AddPt "Polarity", 4, 0.41, -3.7
'        AddPt "Polarity", 5, 0.45, -3.7 
'        AddPt "Polarity", 6, 0.576,-3.7
'        AddPt "Polarity", 7, 0.66, -2.3
'        AddPt "Polarity", 8, 0.743, -1.5
'        AddPt "Polarity", 9, 0.81, -1
'        AddPt "Polarity", 10, 0.88, 0
'
'        addpt "Velocity", 0, 0,         1
'        addpt "Velocity", 1, 0.16, 1.06
'        addpt "Velocity", 2, 0.41,         1.05
'        addpt "Velocity", 3, 0.53,         1'0.982
'        addpt "Velocity", 4, 0.702, 0.968
'        addpt "Velocity", 5, 0.95,  0.968
'        addpt "Velocity", 6, 1.03,         0.945
'
'        LF.Object = LeftFlipper        
'        LF.EndPoint = EndPointLp
'        RF.Object = RightFlipper
'        RF.EndPoint = EndPointRp
'End Sub
'
'


'*******************************************
'  Late 80's early 90's

'Sub InitPolarity()
'	dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'	Next
'
'	AddPt "Polarity", 0, 0, 0
'	AddPt "Polarity", 1, 0.05, -5
'	AddPt "Polarity", 2, 0.4, -5
'	AddPt "Polarity", 3, 0.6, -4.5
'	AddPt "Polarity", 4, 0.65, -4.0
'	AddPt "Polarity", 5, 0.7, -3.5
'	AddPt "Polarity", 6, 0.75, -3.0
'	AddPt "Polarity", 7, 0.8, -2.5
'	AddPt "Polarity", 8, 0.85, -2.0
'	AddPt "Polarity", 9, 0.9,-1.5
'	AddPt "Polarity", 10, 0.95, -1.0
'	AddPt "Polarity", 11, 1, -0.5
'	AddPt "Polarity", 12, 1.1, 0
'	AddPt "Polarity", 13, 1.3, 0
'
'	addpt "Velocity", 0, 0,         1
'	addpt "Velocity", 1, 0.16, 1.06
'	addpt "Velocity", 2, 0.41,         1.05
'	addpt "Velocity", 3, 0.53,         1'0.982
'	addpt "Velocity", 4, 0.702, 0.968
'	addpt "Velocity", 5, 0.95,  0.968
'	addpt "Velocity", 6, 1.03,         0.945
'
'	LF.Object = LeftFlipper        
'	LF.EndPoint = EndPointLp
'	RF.Object = RightFlipper
'	RF.EndPoint = EndPointRp
'End Sub



'
''*******************************************
'' Early 90's and after
'
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

        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
End Sub


' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub




'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
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
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
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

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function


'******************************************************
'  FLIPPER TRICKS 
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
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
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
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
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
'  Check ball distance from Flipper for Rem
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


'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

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
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
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
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
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

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

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
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
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

Sub TimerPlunger_Timer
  If Primary_plunger.Y < -130 Then
  	Primary_plunger.Y = Primary_plunger.Y + 5
  End If
End Sub

Sub TimerPlunger2_Timer
 'debug.print plunger.position
  Primary_plunger.Y = -230 + (5* Plunger.Position) -20
End Sub


'''''''''''''''''''''''''''''''''''''
'''''  Rotate Primitives
'''''''''''''''''''''''''''''''''''''

Sub RotateThings_Timer()

	pLeftFlipperLogo.Roty = LeftFlipper.Currentangle' + 180
	flipperlRubber.RotY = LeftFlipper.CurrentAngle
	flipperlBat.RotY = LeftFlipper.CurrentAngle

	pRightFlipperLogo.Roty = RightFlipper.Currentangle' + 180
	flipperrRubber.RotY = RightFlipper.CurrentAngle
	flipperrBat.RotY = RightFlipper.CurrentAngle

	pSpinner.RotX = sw56.Currentangle * -1

	pSpinnerRod.TransX = sin( (sw56.CurrentAngle+180) * (2*PI/360)) * 5
	pSpinnerRod.TransY = sin( (SW56.CurrentAngle- 90) * (2*PI/360)) * 5

End Sub


Dim BIP,xxBLPeg, xxPostsColor, FlipperColorType, PlasticProtectorsType, SideFlasherColorType, PostsColorType, CustomICsType
BIP = 0


Sub SetOptions()

'Flipper Colors

'0=Random
'1=White/Red
'2=White/Black
'3=White/Yellow
'4=White/Green
'5=White/BlackLightGreen
'6=Yellow/Red
'7=Yellow/Black
'8=Yellow/Green
'9=Yellow/BlackLightGreen

If FlipperMod = 0 Then
		FlipperColorType = Int(Rnd*9)+1
	Else
		FlipperColorType = FlipperMod
End If

If FlipperColorType = 1 Then
	flipperrBat.Material = "Plastic White"
	flipperrRubber.Material = "Red Rubber"
	pRightFlipperLogo.Material = "Plastic White"
	flipperlBat.Material = "Plastic White"
	flipperlRubber.Material = "Red Rubber"
	pLeftFlipperLogo.Material = "Plastic White"
End If

If FlipperColorType = 2 Then
	flipperrBat.Material = "Plastic White"
	flipperrRubber.Material = "Black Rubber"
	pRightFlipperLogo.Material = "Plastic White"
	flipperlBat.Material = "Plastic White"
	flipperlRubber.Material = "Black Rubber"
	pLeftFlipperLogo.Material = "Plastic White"
End If

If FlipperColorType = 3 Then
	flipperrBat.Material = "Plastic White"
	flipperrRubber.Material = "Yellow Rubber"
	pRightFlipperLogo.Material = "Plastic White"
	flipperlBat.Material = "Plastic White"
	flipperlRubber.Material = "Yellow Rubber"
	pLeftFlipperLogo.Material = "Plastic White"
End If

If FlipperColorType = 4 Then
	flipperrBat.Material = "Plastic White"
	flipperrRubber.Material = "Green Rubber"
	pRightFlipperLogo.Material = "Plastic White"
	flipperlBat.Material = "Plastic White"
	flipperlRubber.Material = "Green Rubber"
	pLeftFlipperLogo.Material = "Plastic White"
End If

If FlipperColorType = 5 Then
	flipperrBat.Material = "Plastic White"
	flipperrRubber.Material = "Green Rubber"
	flipperrRubber.DisableLighting = 1
	pRightFlipperLogo.Material = "Plastic White"
	flipperlBat.Material = "Plastic White"
	flipperlRubber.Material = "Green Rubber"
	flipperlRubber.DisableLighting = 1
	pLeftFlipperLogo.Material = "Plastic White"
End If

If FlipperColorType = 6 Then
	flipperrBat.Material = "Plastic Yellow"
	flipperrRubber.Material = "Red Rubber"
	pRightFlipperLogo.Material = "Plastic Yellow"
	flipperlBat.Material = "Plastic Yellow"
	flipperlRubber.Material = "Red Rubber"
	pLeftFlipperLogo.Material = "Plastic Yellow"
End If

If FlipperColorType = 7 Then
	flipperrBat.Material = "Plastic Yellow"
	flipperrRubber.Material = "Black Rubber"
	pRightFlipperLogo.Material = "Plastic Yellow"
	flipperlBat.Material = "Plastic Yellow"
	flipperlRubber.Material = "Black Rubber"
	pLeftFlipperLogo.Material = "Plastic Yellow"
End If

If FlipperColorType = 8 Then
	flipperrBat.Material = "Plastic Yellow"
	flipperrRubber.Material = "Green Rubber"
	pRightFlipperLogo.Material = "Plastic Yellow"
	flipperlBat.Material = "Plastic Yellow"
	flipperlRubber.Material = "Green Rubber"
	pLeftFlipperLogo.Material = "Plastic Yellow"
End If

If FlipperColorType = 9 Then
	flipperrBat.Material = "Plastic Yellow"
	flipperrRubber.Material = "Green Rubber"
	flipperrRubber.DisableLighting = 1
	pRightFlipperLogo.Material = "Plastic Yellow"
	flipperlBat.Material = "Plastic Yellow"
	flipperlRubber.Material = "Green Rubber"
	flipperlRubber.DisableLighting = 1
	pLeftFlipperLogo.Material = "Plastic Yellow"
End If





'''''''''''''''''''''''''''
''''''GI
'''''''''''''''''''''''''''
Dim Red, RedFull, RedI, Pink, PinkFull, PinkI, White, WhiteFull, WhiteI, Blue, BlueFull, BlueI, Yellow, YellowFull, YellowI, Green, GreenFull, GreenI, GreenI2, Orange, OrangeFull, OrangeI, Purple, PurpleFull, PurpleI, AmberFull, Amber, AmberI
Dim GIColorModType

RedFull = rgb(255,0,0) 
Red = rgb(255,0,0)
RedI = 20
PinkFull = rgb(255,0,128)
Pink = rgb(255,0,255)
PinkI = 20
WhiteFull = rgb(255,255,128) 
White = rgb(255,255,255)
WhiteI = 10
BlueFull = rgb(0,128,255)
Blue = rgb(0,255,255)
BlueI = 20
YellowFull = rgb(255,255,128)
Yellow = rgb(255,255,0)
YellowI = 10
GreenFull = rgb(128,255,128)
Green = rgb(0,255,0)
GreenI = 20
GreenI2 = 2
PurpleFull = rgb(128,0,255)
Purple = rgb(64,0,128)
PurpleI = 20
OrangeFull = rgb(255,128,64)
Orange = rgb(128,128,0)
OrangeI = 20
AmberFull = rgb(255,197,143)
Amber = rgb(255,197,143)
AmberI = 20



If GIColorMod = 0 Then
		GIColorModType = Int(Rnd*4)+1
	Else
		GIColorModType = GIColorMod
End If


If GIColorModType = 1 Then
	gi1a.Color=Amber
	gi1a.ColorFull=AmberFull
	gi1b.Color=Amber
	gi1b.ColorFull=AmberFull
	gi1c.Color=White
	gi1c.ColorFull=WhiteFull
	gi1a.Intensity = AmberI

	gi2a.Color=Amber
	gi2a.ColorFull=AmberFull
	gi2b.Color=Amber
	gi2b.ColorFull=AmberFull
	gi2c.Color=White
	gi2c.ColorFull=WhiteFull
	gi2a.Intensity = AmberI

	gi3a.Color=Amber
	gi3a.ColorFull=AmberFull
	gi3b.Color=Amber
	gi3b.ColorFull=AmberFull
	gi3c.Color=White
	gi3c.ColorFull=WhiteFull
	gi3a.Intensity = AmberI

	gi4a.Color=Amber
	gi4a.ColorFull=AmberFull
	gi4b.Color=Amber
	gi4b.ColorFull=AmberFull
	gi4c.Color=White
	gi4c.ColorFull=WhiteFull
	gi4a.Intensity = AmberI

	gi5a.Color=Amber
	gi5a.ColorFull=AmberFull
	gi5b.Color=Amber
	gi5b.ColorFull=AmberFull
	gi5c.Color=White
	gi5c.ColorFull=WhiteFull
	gi5a.Intensity = AmberI

	gi6a.Color=Amber
	gi6a.ColorFull=AmberFull
	gi6b.Color=Amber
	gi6b.ColorFull=AmberFull
	gi6c.Color=White
	gi6c.ColorFull=WhiteFull
	gi6a.Intensity = AmberI

	gi7a.Color=Amber
	gi7a.ColorFull=AmberFull
	gi7b.Color=Amber
	gi7b.ColorFull=AmberFull
	gi7c.Color=White
	gi7c.ColorFull=WhiteFull
	gi7a.Intensity = AmberI

	gi8a.Color=Amber
	gi8a.ColorFull=AmberFull
	gi8b.Color=Amber
	gi8b.ColorFull=AmberFull
	gi8c.Color=White
	gi8c.ColorFull=WhiteFull
	gi8a.Intensity = AmberI

	gi9a.Color=Amber
	gi9a.ColorFull=AmberFull
	gi9b.Color=Amber
	gi9b.ColorFull=AmberFull
	gi9c.Color=White
	gi9c.ColorFull=WhiteFull
	gi9a.Intensity = AmberI

	gi10a.Color=Amber
	gi10a.ColorFull=AmberFull
	gi10b.Color=Amber
	gi10b.ColorFull=AmberFull
	gi10c.Color=White
	gi10c.ColorFull=WhiteFull
	gi10a.Intensity = AmberI

	gi11a.Color=Amber
	gi11a.ColorFull=AmberFull
	gi11b.Color=Amber
	gi11b.ColorFull=AmberFull
	gi11c.Color=White
	gi11c.ColorFull=WhiteFull
	gi11a.Intensity = AmberI

	gi12a.Color=Amber
	gi12a.ColorFull=AmberFull
	gi12b.Color=Amber
	gi12b.ColorFull=AmberFull
	gi12c.Color=White
	gi12c.ColorFull=WhiteFull
	gi12a.Intensity = AmberI

	gi13a.Color=Amber
	gi13a.ColorFull=AmberFull
	gi13b.Color=Amber
	gi13b.ColorFull=AmberFull
	gi13c.Color=White
	gi13c.ColorFull=WhiteFull
	gi13a.Intensity = AmberI

	gi14a.Color=Amber
	gi14a.ColorFull=AmberFull
	gi14b.Color=Amber
	gi14b.ColorFull=AmberFull
	gi14c.Color=White
	gi14c.ColorFull=WhiteFull
	gi14a.Intensity = AmberI

	gi15a.Color=Amber
	gi15a.ColorFull=AmberFull
	gi15b.Color=Amber
	gi15b.ColorFull=AmberFull
	gi15c.Color=White
	gi15c.ColorFull=WhiteFull
	gi15a.Intensity = AmberI

	gi16a.Color=Amber
	gi16a.ColorFull=AmberFull
	gi16b.Color=Amber
	gi16b.ColorFull=AmberFull
	gi16c.Color=White
	gi16c.ColorFull=WhiteFull
	gi16a.Intensity = AmberI

	gi17a.Color=Amber
	gi17a.ColorFull=AmberFull
	gi17b.Color=Amber
	gi17b.ColorFull=AmberFull
	gi17c.Color=White
	gi17c.ColorFull=WhiteFull
	gi17a.Intensity = AmberI

	gi18a.Color=Amber
	gi18a.ColorFull=AmberFull
	gi18b.Color=Amber
	gi18b.ColorFull=AmberFull
	gi18c.Color=White
	gi18c.ColorFull=WhiteFull
	gi18a.Intensity = AmberI

	gi19a.Color=Amber
	gi19a.ColorFull=AmberFull
	gi19b.Color=Amber
	gi19b.ColorFull=AmberFull
	gi19c.Color=White
	gi19c.ColorFull=WhiteFull
	gi19a.Intensity = AmberI

	gi20a.Color=Amber
	gi20a.ColorFull=AmberFull
	gi20b.Color=Amber
	gi20b.ColorFull=AmberFull
	gi20c.Color=White
	gi20c.ColorFull=WhiteFull
	gi20a.Intensity = AmberI

	gi21a.Color=Amber
	gi21a.ColorFull=AmberFull
	gi21b.Color=Amber
	gi21b.ColorFull=AmberFull
	gi21c.Color=White
	gi21c.ColorFull=WhiteFull
	gi21a.Intensity = AmberI

End If


If GIColorModType = 2 Then
	gi1a.Color=White
	gi1a.ColorFull=WhiteFull
	gi1b.Color=Yellow
	gi1b.ColorFull=YellowFull
	gi1c.Color=White
	gi1c.ColorFull=WhiteFull
	gi1a.Intensity = WhiteI

	gi2a.Color=White
	gi2a.ColorFull=WhiteFull
	gi2b.Color=Yellow
	gi2b.ColorFull=YellowFull
	gi2c.Color=White
	gi2c.ColorFull=WhiteFull
	gi2a.Intensity = WhiteI

	gi3a.Color=White
	gi3a.ColorFull=WhiteFull
	gi3b.Color=Yellow
	gi3b.ColorFull=YellowFull
	gi3c.Color=White
	gi3c.ColorFull=WhiteFull
	gi3a.Intensity = WhiteI

	gi4a.Color=White
	gi4a.ColorFull=WhiteFull
	gi4b.Color=Yellow
	gi4b.ColorFull=YellowFull
	gi4c.Color=White
	gi4c.ColorFull=WhiteFull
	gi4a.Intensity = WhiteI

	gi5a.Color=White
	gi5a.ColorFull=WhiteFull
	gi5b.Color=Yellow
	gi5b.ColorFull=YellowFull
	gi5c.Color=White
	gi5c.ColorFull=WhiteFull
	gi5a.Intensity = WhiteI

	gi6a.Color=White
	gi6a.ColorFull=WhiteFull
	gi6b.Color=Yellow
	gi6b.ColorFull=YellowFull
	gi6c.Color=White
	gi6c.ColorFull=WhiteFull
	gi6a.Intensity = WhiteI

	gi7a.Color=White
	gi7a.ColorFull=WhiteFull
	gi7b.Color=Yellow
	gi7b.ColorFull=YellowFull
	gi7c.Color=White
	gi7c.ColorFull=WhiteFull
	gi7a.Intensity = WhiteI

	gi8a.Color=White
	gi8a.ColorFull=WhiteFull
	gi8b.Color=Yellow
	gi8b.ColorFull=YellowFull
	gi8c.Color=White
	gi8c.ColorFull=WhiteFull
	gi8a.Intensity = WhiteI

	gi9a.Color=White
	gi9a.ColorFull=WhiteFull
	gi9b.Color=Yellow
	gi9b.ColorFull=YellowFull
	gi9c.Color=White
	gi9c.ColorFull=WhiteFull
	gi9a.Intensity = WhiteI

	gi10a.Color=White
	gi10a.ColorFull=WhiteFull
	gi10b.Color=Yellow
	gi10b.ColorFull=YellowFull
	gi10c.Color=White
	gi10c.ColorFull=WhiteFull
	gi10a.Intensity = WhiteI

	gi11a.Color=White
	gi11a.ColorFull=WhiteFull
	gi11b.Color=Yellow
	gi11b.ColorFull=YellowFull
	gi11c.Color=White
	gi11c.ColorFull=WhiteFull
	gi11a.Intensity = WhiteI

	gi12a.Color=White
	gi12a.ColorFull=WhiteFull
	gi12b.Color=Yellow
	gi12b.ColorFull=YellowFull
	gi12c.Color=White
	gi12c.ColorFull=WhiteFull
	gi12a.Intensity = WhiteI

	gi13a.Color=White
	gi13a.ColorFull=WhiteFull
	gi13b.Color=Yellow
	gi13b.ColorFull=YellowFull
	gi13c.Color=White
	gi13c.ColorFull=WhiteFull
	gi13a.Intensity = WhiteI

	gi14a.Color=White
	gi14a.ColorFull=WhiteFull
	gi14b.Color=Yellow
	gi14b.ColorFull=YellowFull
	gi14c.Color=White
	gi14c.ColorFull=WhiteFull
	gi14a.Intensity = WhiteI

	gi15a.Color=White
	gi15a.ColorFull=WhiteFull
	gi15b.Color=Yellow
	gi15b.ColorFull=YellowFull
	gi15c.Color=White
	gi15c.ColorFull=WhiteFull
	gi15a.Intensity = WhiteI

	gi16a.Color=White
	gi16a.ColorFull=WhiteFull
	gi16b.Color=Yellow
	gi16b.ColorFull=YellowFull
	gi16c.Color=White
	gi16c.ColorFull=WhiteFull
	gi16a.Intensity = WhiteI

	gi17a.Color=White
	gi17a.ColorFull=WhiteFull
	gi17b.Color=Yellow
	gi17b.ColorFull=YellowFull
	gi17c.Color=White
	gi17c.ColorFull=WhiteFull
	gi17a.Intensity = WhiteI

	gi18a.Color=White
	gi18a.ColorFull=WhiteFull
	gi18b.Color=Yellow
	gi18b.ColorFull=YellowFull
	gi18c.Color=White
	gi18c.ColorFull=WhiteFull
	gi18a.Intensity = WhiteI

	gi19a.Color=White
	gi19a.ColorFull=WhiteFull
	gi19b.Color=Yellow
	gi19b.ColorFull=YellowFull
	gi19c.Color=White
	gi19c.ColorFull=WhiteFull
	gi19a.Intensity = WhiteI

	gi20a.Color=White
	gi20a.ColorFull=WhiteFull
	gi20b.Color=Yellow
	gi20b.ColorFull=YellowFull
	gi20c.Color=White
	gi20c.ColorFull=WhiteFull
	gi20a.Intensity = WhiteI

	gi21a.Color=White
	gi21a.ColorFull=WhiteFull
	gi21b.Color=Yellow
	gi21b.ColorFull=YellowFull
	gi21c.Color=White
	gi21c.ColorFull=WhiteFull
	gi21a.Intensity = WhiteI

End If

If GIColorModType = 3 Then
	gi1a.Color=Green
	gi1a.ColorFull=GreenFull
	gi1b.Color=Green
	gi1b.ColorFull=GreenFull
	gi1c.Color=Green
	gi1c.ColorFull=GreenFull
	gi1a.Intensity = GreenI

	gi2a.Color=Purple
	gi2a.ColorFull=PurpleFull
	gi2b.Color=Purple
	gi2b.ColorFull=PurpleFull
	gi2c.Color=Purple
	gi2c.ColorFull=PurpleFull
	gi2a.Intensity = PurpleI

	gi3a.Color=Purple
	gi3a.ColorFull=PurpleFull
	gi3b.Color=Purple
	gi3b.ColorFull=PurpleFull
	gi3c.Color=Purple
	gi3c.ColorFull=PurpleFull
	gi3a.Intensity = PurpleI

	gi4a.Color=Green
	gi4a.ColorFull=GreenFull
	gi4b.Color=Green
	gi4b.ColorFull=GreenFull
	gi4c.Color=Green
	gi4c.ColorFull=GreenFull
	gi4a.Intensity = GreenI2

	gi5a.Color=Purple
	gi5a.ColorFull=PurpleFull
	gi5b.Color=Purple
	gi5b.ColorFull=PurpleFull
	gi5c.Color=Purple
	gi5c.ColorFull=PurpleFull
	gi5a.Intensity = PurpleI

	gi6a.Color=Purple
	gi6a.ColorFull=PurpleFull
	gi6b.Color=Purple
	gi6b.ColorFull=PurpleFull
	gi6c.Color=Purple
	gi6c.ColorFull=PurpleFull
	gi6a.Intensity = PurpleI

	gi7a.Color=Red
	gi7a.ColorFull=RedFull
	gi7b.Color=Red
	gi7b.ColorFull=RedFull
	gi7c.Color=Red
	gi7c.ColorFull=RedFull
	gi7a.Intensity = RedI

	gi8a.Color=Green
	gi8a.ColorFull=GreenFull
	gi8b.Color=Green
	gi8b.ColorFull=GreenFull
	gi8c.Color=Green
	gi8c.ColorFull=GreenFull
	gi8a.Intensity = GreenI

	gi9a.Color=Purple
	gi9a.ColorFull=PurpleFull
	gi9b.Color=Purple
	gi9b.ColorFull=PurpleFull
	gi9c.Color=Purple
	gi9c.ColorFull=PurpleFull
	gi9a.Intensity = PurpleI

	gi10a.Color=Red
	gi10a.ColorFull=RedFull
	gi10b.Color=Red
	gi10b.ColorFull=RedFull
	gi10c.Color=Red
	gi10c.ColorFull=RedFull
	gi10a.Intensity = RedI

	gi11a.Color=Orange
	gi11a.ColorFull=OrangeFull
	gi11b.Color=Orange
	gi11b.ColorFull=OrangeFull
	gi11c.Color=Orange
	gi11c.ColorFull=OrangeFull
	gi11a.Intensity = OrangeI

	gi12a.Color=Orange
	gi12a.ColorFull=OrangeFull
	gi12b.Color=Orange
	gi12b.ColorFull=OrangeFull
	gi12c.Color=Orange
	gi12c.ColorFull=OrangeFull
	gi12a.Intensity = OrangeI

	gi13a.Color=Red
	gi13a.ColorFull=RedFull
	gi13b.Color=Red
	gi13b.ColorFull=RedFull
	gi13c.Color=Red
	gi13c.ColorFull=RedFull
	gi13a.Intensity = RedI

	gi14a.Color=Blue
	gi14a.ColorFull=BlueFull
	gi14b.Color=Blue
	gi14b.ColorFull=BlueFull
	gi14c.Color=Blue
	gi14c.ColorFull=BlueFull
	gi14a.Intensity = BlueI

	gi15a.Color=Blue
	gi15a.ColorFull=BlueFull
	gi15b.Color=Blue
	gi15b.ColorFull=BlueFull
	gi15c.Color=Blue
	gi15c.ColorFull=BlueFull
	gi15a.Intensity = BlueI

	gi16a.Color=Purple
	gi16a.ColorFull=PurpleFull
	gi16b.Color=Purple
	gi16b.ColorFull=PurpleFull
	gi16c.Color=Purple
	gi16c.ColorFull=PurpleFull
	gi16a.Intensity = PurpleI

	gi17a.Color=Purple
	gi17a.ColorFull=PurpleFull
	gi17b.Color=Purple
	gi17b.ColorFull=PurpleFull
	gi17c.Color=Purple
	gi17c.ColorFull=PurpleFull
	gi17a.Intensity = PurpleI

	gi18a.Color=Purple
	gi18a.ColorFull=PurpleFull
	gi18b.Color=Purple
	gi18b.ColorFull=PurpleFull
	gi18c.Color=Purple
	gi18c.ColorFull=PurpleFull
	gi18a.Intensity = PurpleI

	gi19a.Color=Green
	gi19a.ColorFull=GreenFull
	gi19b.Color=Green
	gi19b.ColorFull=GreenFull
	gi19c.Color=Green
	gi19c.ColorFull=GreenFull
	gi19a.Intensity = GreenI

	gi20a.Color=Green
	gi20a.ColorFull=GreenFull
	gi20b.Color=Green
	gi20b.ColorFull=GreenFull
	gi20c.Color=Green
	gi20c.ColorFull=GreenFull
	gi20a.Intensity = GreenI

	gi21a.Color=Green
	gi21a.ColorFull=GreenFull
	gi21b.Color=Green
	gi21b.ColorFull=GreenFull
	gi21c.Color=Green
	gi21c.ColorFull=GreenFull
	gi21a.Intensity = GreenI

	gi22.Color=Green
	gi22.ColorFull=GreenFull

	gi23.Color=Green
	gi23.ColorFull=GreenFull

	gi24.Color=Green
	gi24.ColorFull=GreenFull

End If


If GIColorModType = 4 Then
	gi1a.Color=Green
	gi1a.ColorFull=GreenFull
	gi1b.Color=Green
	gi1b.ColorFull=GreenFull
	gi1c.Color=Green
	gi1c.ColorFull=GreenFull
	gi1a.Intensity = GreenI

	gi2a.Color=Green
	gi2a.ColorFull=GreenFull
	gi2b.Color=Green
	gi2b.ColorFull=GreenFull
	gi2c.Color=Green
	gi2c.ColorFull=GreenFull
	gi2a.Intensity = GreenI

	gi3a.Color=Green
	gi3a.ColorFull=GreenFull
	gi3b.Color=Green
	gi3b.ColorFull=GreenFull
	gi3c.Color=Green
	gi3c.ColorFull=GreenFull
	gi3a.Intensity = GreenI

	gi4a.Color=Green
	gi4a.ColorFull=GreenFull
	gi4b.Color=Green
	gi4b.ColorFull=GreenFull
	gi4c.Color=Green
	gi4c.ColorFull=GreenFull
	gi4a.Intensity = GreenI

	gi5a.Color=Green
	gi5a.ColorFull=GreenFull
	gi5b.Color=Green
	gi5b.ColorFull=GreenFull
	gi5c.Color=Green
	gi5c.ColorFull=GreenFull
	gi5a.Intensity = GreenI

	gi6a.Color=Green
	gi6a.ColorFull=GreenFull
	gi6b.Color=Green
	gi6b.ColorFull=GreenFull
	gi6c.Color=Green
	gi6c.ColorFull=GreenFull
	gi6a.Intensity = GreenI

	gi7a.Color=Green
	gi7a.ColorFull=GreenFull
	gi7b.Color=Green
	gi7b.ColorFull=GreenFull
	gi7c.Color=Green
	gi7c.ColorFull=GreenFull
	gi7a.Intensity = GreenI

	gi8a.Color=Green
	gi8a.ColorFull=GreenFull
	gi8b.Color=Green
	gi8b.ColorFull=GreenFull
	gi8c.Color=Green
	gi8c.ColorFull=GreenFull
	gi8a.Intensity = GreenI

	gi9a.Color=Green
	gi9a.ColorFull=GreenFull
	gi9b.Color=Green
	gi9b.ColorFull=GreenFull
	gi9c.Color=Green
	gi9c.ColorFull=GreenFull
	gi9a.Intensity = GreenI

	gi10a.Color=Green
	gi10a.ColorFull=GreenFull
	gi10b.Color=Green
	gi10b.ColorFull=GreenFull
	gi10c.Color=Green
	gi10c.ColorFull=GreenFull
	gi10a.Intensity = GreenI

	gi11a.Color=Green
	gi11a.ColorFull=GreenFull
	gi11b.Color=Green
	gi11b.ColorFull=GreenFull
	gi11c.Color=Green
	gi11c.ColorFull=GreenFull
	gi11a.Intensity = GreenI

	gi12a.Color=Green
	gi12a.ColorFull=GreenFull
	gi12b.Color=Green
	gi12b.ColorFull=GreenFull
	gi12c.Color=Green
	gi12c.ColorFull=GreenFull
	gi12a.Intensity = GreenI

	gi13a.Color=Green
	gi13a.ColorFull=GreenFull
	gi13b.Color=Green
	gi13b.ColorFull=GreenFull
	gi13c.Color=Green
	gi13c.ColorFull=GreenFull
	gi13a.Intensity = GreenI

	gi14a.Color=Green
	gi14a.ColorFull=GreenFull
	gi14b.Color=Green
	gi14b.ColorFull=GreenFull
	gi14c.Color=Green
	gi14c.ColorFull=GreenFull
	gi14a.Intensity = GreenI

	gi15a.Color=Green
	gi15a.ColorFull=GreenFull
	gi15b.Color=Green
	gi15b.ColorFull=GreenFull
	gi15c.Color=Green
	gi15c.ColorFull=GreenFull
	gi15a.Intensity = GreenI

	gi16a.Color=Green
	gi16a.ColorFull=GreenFull
	gi16b.Color=Green
	gi16b.ColorFull=GreenFull
	gi16c.Color=Green
	gi16c.ColorFull=GreenFull
	gi16a.Intensity = GreenI

	gi17a.Color=Green
	gi17a.ColorFull=GreenFull
	gi17b.Color=Green
	gi17b.ColorFull=GreenFull
	gi17c.Color=Green
	gi17c.ColorFull=GreenFull
	gi17a.Intensity = GreenI

	gi18a.Color=Green
	gi18a.ColorFull=GreenFull
	gi18b.Color=Green
	gi18b.ColorFull=GreenFull
	gi18c.Color=Green
	gi18c.ColorFull=GreenFull
	gi18a.Intensity = GreenI

	gi19a.Color=Green
	gi19a.ColorFull=GreenFull
	gi19b.Color=Green
	gi19b.ColorFull=GreenFull
	gi19c.Color=Green
	gi19c.ColorFull=GreenFull
	gi19a.Intensity = GreenI

	gi20a.Color=Green
	gi20a.ColorFull=GreenFull
	gi20b.Color=Green
	gi20b.ColorFull=GreenFull
	gi20c.Color=Green
	gi20c.ColorFull=GreenFull
	gi20a.Intensity = GreenI

	gi21a.Color=Green
	gi21a.ColorFull=GreenFull
	gi21b.Color=Green
	gi21b.ColorFull=GreenFull
	gi21c.Color=Green
	gi21c.ColorFull=GreenFull
	gi21a.Intensity = GreenI

End If

If TurtlesColorMod = 1 Then
	l9.Color=Green
	l9.ColorFull=GreenFull
	l10.Color=Green
	l10.ColorFull=GreenFull
	l11.Color=Green
	l11.ColorFull=GreenFull
	l12.Color=Green
	l12.ColorFull=GreenFull
	l13.Color=Green
	l13.ColorFull=GreenFull
	l14.Color=Green
	l14.ColorFull=GreenFull
	l15.Color=Green
	l15.ColorFull=GreenFull
Else
	l9.Color=Amber
	l9.ColorFull=AmberFull
	l10.Color=Amber
	l10.ColorFull=AmberFull
	l11.Color=Amber
	l11.ColorFull=AmberFull
	l12.Color=Amber
	l12.ColorFull=AmberFull
	l13.Color=Amber
	l13.ColorFull=AmberFull
	l14.Color=Amber
	l14.ColorFull=AmberFull
	l15.Color=Amber
	l15.ColorFull=AmberFull
End If

Dim RubberModType

'''''Rubbers

If RubberMod = 0 Then
		RubberModType = Int(Rnd*3)+1
	Else
		RubberModType = RubberMod
End If


If RubberModType = 1 Then

	LeftSlingshota.Material = "Rubber White"
	LeftSlingshotb.Material = "Rubber White"
	LeftSlingshotc.Material = "Rubber White"
	LeftSlingshotd.Material = "Rubber White"

	RightSlingshota.Material = "Rubber White"
	RightSlingshotb.Material = "Rubber White"
	RightSlingshotc.Material = "Rubber White"
	RightSlingshotd.Material = "Rubber White"

	Rubber1.Material = "Rubber White"
	Rubber3.Material = "Rubber White"
	Rubber4.Material = "Rubber White"
	Rubber5.Material = "Rubber White"
	Rubber6.Material = "Rubber White"
	Rubber7.Material = "Rubber White"
	Rubber8.Material = "Rubber White"
	Rubber9.Material = "Rubber White"
	Rubber21.Material = "Rubber White"
	Rubber10.Material = "Rubber White"
	Rubber22.Material = "Rubber White"
	Rubber11.Material = "Rubber White"

	Rubber32.Material = "Rubber White"

	Pin3.Material = "Rubber White"
	Pin4.Material = "Rubber White"
	PegRubber3.Material = "Rubber White"
	PegRubber4.Material = "Rubber White"
	Pin7.Material = "Rubber White"
	Pin8.Material = "Rubber White"
	PegRubber1.Material = "Rubber White"
	PegRubber2.Material = "Rubber White"

End If

If RubberModType = 2 Then

	LeftSlingshota.Material = "Black Rubber"
	LeftSlingshotb.Material = "Black Rubber"
	LeftSlingshotc.Material = "Black Rubber"
	LeftSlingshotd.Material = "Black Rubber"

	RightSlingshota.Material = "Black Rubber"
	RightSlingshotb.Material = "Black Rubber"
	RightSlingshotc.Material = "Black Rubber"
	RightSlingshotd.Material = "Black Rubber"

	Rubber1.Material = "Black Rubber"
	Rubber3.Material = "Black Rubber"
	Rubber4.Material = "Black Rubber"
	Rubber5.Material = "Black Rubber"
	Rubber6.Material = "Black Rubber"
	Rubber7.Material = "Black Rubber"
	Rubber8.Material = "Black Rubber"
	Rubber9.Material = "Black Rubber"
	Rubber21.Material = "Black Rubber"
	Rubber10.Material = "Black Rubber"
	Rubber22.Material = "Black Rubber"
	Rubber11.Material = "Black Rubber"

	Rubber32.Material = "Black Rubber"

	Pin3.Material = "Black Rubber"
	Pin4.Material = "Black Rubber"
	PegRubber3.Material = "Black Rubber"
	PegRubber4.Material = "Black Rubber"
	Pin7.Material = "Black Rubber"
	Pin8.Material = "Black Rubber"
	PegRubber1.Material = "Black Rubber"
	PegRubber2.Material = "Black Rubber"

End If

If RubberModType = 3 Then

	LeftSlingshota.Material = "Rubber Dark Green"
	LeftSlingshotb.Material = "Rubber Dark Green"
	LeftSlingshotc.Material = "Rubber Dark Green"
	LeftSlingshotd.Material = "Rubber Dark Green"

	RightSlingshota.Material = "Rubber Dark Green"
	RightSlingshotb.Material = "Rubber Dark Green"
	RightSlingshotc.Material = "Rubber Dark Green"
	RightSlingshotd.Material = "Rubber Dark Green"

	Rubber1.Material = "Rubber Purple"
	Rubber3.Material = "Rubber Blue"
	Rubber4.Material = "Rubber Red"
	Rubber5.Material = "Rubber Purple"
	Rubber6.Material = "Rubber Blue"
	Rubber7.Material = "Rubber Purple"
	Rubber8.Material = "Rubber Purple"
	Rubber9.Material = "Rubber Purple"
	Rubber21.Material = "Rubber Purple"
	Rubber10.Material = "Rubber Orange"
	Rubber22.Material = "Rubber Orange"
	Rubber11.Material = "Rubber Purple"

	Rubber32.Material = "Rubber Purple"

	Pin3.Material = "Rubber Dark Green"
	Pin4.Material = "Rubber Dark Green"
	PegRubber3.Material = "Rubber Dark Green"
	PegRubber4.Material = "Rubber Dark Green"

	Pin7.Material = "Black Rubber"
	Pin8.Material = "Black Rubber"
	PegRubber1.Material = "Black Rubber"
	PegRubber2.Material = "Black Rubber"


End If


'Plastic Protectors
'0=Random
'1=Clear
'2=OozeGreen

If PlasticProtectors = 0 Then
		PlasticProtectorsType = Int(Rnd*2)+1
	Else
		PlasticProtectorsType = PlasticProtectors
End If

If PlasticProtectorsType = 1 Then
	pClearPlastic1.Material = "ClearPlastic"
	pClearPlastic1.DisableLighting = 0
	pClearPlastic2.Material = "ClearPlastic"
	pClearPlastic2.DisableLighting = 0
	pClearPlastic3.Material = "ClearPlastic"
	pClearPlastic3.DisableLighting = 0
	pClearPlastic4.Material = "ClearPlastic"
	pClearPlastic4.DisableLighting = 0
	pClearPlastic5.Material = "ClearPlastic"
	pClearPlastic5.DisableLighting = 0
	pClearPlastic6.Material = "ClearPlastic"
	pClearPlastic6.DisableLighting = 0
	pClearPlastic7.Material = "ClearPlastic"
	pClearPlastic7.DisableLighting = 0
End If

If PlasticProtectorsType = 2 Then
	pClearPlastic1.Material = "ClearPlasticNeonGreen"
	pClearPlastic1.DisableLighting = 1
	pClearPlastic2.Material = "ClearPlasticNeonGreen"
	pClearPlastic2.DisableLighting = 1
	pClearPlastic3.Material = "ClearPlasticNeonGreen"
	pClearPlastic3.DisableLighting = 1
	pClearPlastic4.Material = "ClearPlasticNeonGreen"
	pClearPlastic4.DisableLighting = 1
	pClearPlastic5.Material = "ClearPlasticNeonGreen"
	pClearPlastic5.DisableLighting = 1
	pClearPlastic6.Material = "ClearPlasticNeonGreen"
	pClearPlastic6.DisableLighting = 1
	pClearPlastic7.Material = "ClearPlasticNeonGreen"
	pClearPlastic7.DisableLighting = 1
End If


If LBCOnorOff = 1 Then
	pLightBoxCover.visible = True
Else
	pLightBoxCover.visible = False
End If

'SideFlasherColor
'0=Random
'1=Yellow
'2=Green

If SideFlasherColor = 0 Then
		SideFlasherColorType = Int(Rnd*2)+1
	Else
		SideFlasherColorType = SideFlasherColor
End If


If SideFlasherColorType = 1 Then
	pDome14a.Image = "TopFlasherYellow_off"
	f14aa.Color = RGB(255,128,0)
	f14aa.ColorFull = RGB(255,255,255)
	f14ab.Color = RGB(255,128,0)
	f14ab.ColorFull = RGB(255,255,255)
	pDome14b.Image = "TopFlasherYellow_off"
	f14ba.Color = RGB(255,128,0)
	f14ba.ColorFull = RGB(255,255,255)
	f14bb.Color = RGB(255,128,0)
	f14bb.ColorFull = RGB(255,255,255)
End If

If SideFlasherColorType = 2 Then
	pDome14a.Image = "TopFlasherGreen_off"
	f14aa.Color = RGB(0,128,0)
	f14aa.ColorFull = RGB(0,255,0)
	f14ab.Color = RGB(0,128,0)
	f14ab.ColorFull = RGB(0,255,0)
	pDome14b.Image = "TopFlasherGreen_off"
	f14ba.Color = RGB(0,128,0)
	f14ba.ColorFull = RGB(0,255,0)
	f14bb.Color = RGB(0,128,0)
	f14bb.ColorFull = RGB(0,255,0)
End If

If Sglass = 0 Then
		Primary_Glass_scratches.imageA= "VR Table Cab_glass_no_scratches"
	End If
If Sglass = 1 Then
		Primary_Glass_scratches.imageA= "VR Table Cab_glass_scratches"
	End If
If Sglass = 2 Then
		Primary_Glass_scratches.imageA= "VR Table Cab_glass_scratches1"
	End If
If Sglass = 3 Then
		Primary_Glass_scratches.imageA= "VR Table Cab_glass_scratches2"
	End If

If Sblades = 0 Then
		PinCab_Blades.image= "PinCab_Blades1"
	End If
If Sblades = 1 Then
		PinCab_Blades.image= "PinCab_Blades"
		PinCab_Blades.material= "colormaxnoreflectionhalf"
	End If
If Sblades = 2 Then
		PinCab_Blades.image= "PinCab_Blades2"
	End If
If Sblades = 3 Then
		PinCab_Blades.image= "PinCab_Blades3"
		PinCab_Blades.material= "colormaxnoreflectionhalf"
	End If

If poster = 0 Then 
		VR_Poster.image= "VR Table Cab_glass_no_scratches"
	End If
If Poster = 1 Then
		VR_Poster.image= "flyer"
	End If

If B2Son1 = 0 Then
		PinCab_Backglass.image= "backglass"
	End If
If B2Son1 = 1 Then 
		PinCab_Backglass.image= "backglassimage"
	End If

If VRlogo = 0 Then 
		VR_logo.imageA= "VR Table Cab_glass_no_scratches"
	End If
If VRlogo = 1 Then
		VR_logo.imageA= "VR_logo"
	End If

If topper = 0 Then 
		VR_topper.imageA= "VR Table Cab_glass_no_scratches"
	End If
If topper = 1 Then
		VR_topper.imageA= "VR_topper"
	End If

If VR_Room = 0 Then
		Primitive52.visible = 1
	Else
		Primitive52.visible = 0
	End If

If DMDreflection = 0 Then  
		Primary_DMD_reflection.dmd = 0
		Primary_DMD_reflection.imageA = "VR Table Cab_glass_no_scratches"
	End If
If DMDreflection = 1 Then  
		Primary_DMD_reflection.dmd = 1
	End If


If BGreflection = 0 Then 
		Primary_backglass_reflection.imageA= "VR Table Cab_glass_no_scratches"
	End If
If BGreflection = 1 Then
		Primary_backglass_reflection.imageA= "Backglass_Reflection"
	End If



Dim VRThings

if VR_Room = 0 Then
	for each VRThings  in VRStuff:VRThings.visible = 0:Next
Else if VR_Room = 0  Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
Else
	for each VRThings in VRStuff:VRThings.visible = 1:Next
	end if

End If



If TurtleWeapons = 1 Then
	pNunChuk1a.Visible = 1
	pNunChuk1b.Visible = 1
	pNunChuk1c.Visible = 1
	pNunChuk2a.Visible = 1
	pNunChuk2b.Visible = 1
	pNunChuk2c.Visible = 1
	pMike_Belt2.Visible = 1
	pMike_Belt1.Visible = 0
	pBoStaff.Visible = 1
	pSai1a.Visible = 1
	pSai1b.Visible = 1
	pSai2a.Visible = 1
	pSai2b.Visible = 1
	pSword1a.Visible = 1
	pSword1b.Visible = 1
	pSword2a.Visible = 1
	pSword2b.Visible = 1
Else
	pNunChuk1a.Visible = 0
	pNunChuk1b.Visible = 0
	pNunChuk1c.Visible = 0
	pNunChuk2a.Visible = 0
	pNunChuk2b.Visible = 0
	pNunChuk2c.Visible = 0
	pMike_Belt2.Visible = 0
	pMike_Belt1.Visible = 1
	pBoStaff.Visible = 0
	pSai1a.Visible = 0
	pSai1b.Visible = 0
	pSai2a.Visible = 0
	pSai2b.Visible = 0
	pSword1a.Visible = 0
	pSword1b.Visible = 0
	pSword2a.Visible = 0
	pSword2b.Visible = 0
End If

'Blacklight Ooooze
'0=Off
'1=On

If BlacklightOoze = 1 Then

	bl_green.visible = True
	fApronOozeL.visible = True
	fApronOozeR.visible = True

Else

	bl_green.visible = False
	fApronOozeL.visible = False
	fApronOozeR.visible = False

End If


'Blacklight LaneGuides
'0=Off
'1=On

If BlacklightLaneGuides = 1 Then

	Primitive4.DisableLighting = 1
	Primitive13.DisableLighting = 1

Else

	Primitive4.DisableLighting = 0
	Primitive13.DisableLighting = 0

End If


'Post Colors
'0=Random
'1=Black
'2=Yellow
'3=Green
'4=Ooze Green

If PostsColor = 0 Then
		PostsColorType = Int(Rnd*4)+1
	Else
		PostsColorType = PostsColor
End If

If PostsColorType = 1 Then
	for each xxPostsColor in RubberPosts
		xxPostsColor.DisableLighting = 0
		xxPostsColor.Image = "rubber-post_black"
		next
End If

If PostsColorType = 2 Then
	for each xxPostsColor in RubberPosts
		xxPostsColor.DisableLighting = 0
		xxPostsColor.Image = "rubber-post_yellow"
		next
End If

If PostsColorType = 3 Then
	for each xxPostsColor in RubberPosts
		xxPostsColor.DisableLighting = 0
		xxPostsColor.Image = "rubber-post_green"
		next
End If

If PostsColorType = 4 Then
	for each xxPostsColor in RubberPosts
		xxPostsColor.DisableLighting = 1
		xxPostsColor.Image = "rubber-post_oozegreen"
		next
End If


'Blacklight Pegs
'0=Off
'1=On

If BlacklightPegs = 1 Then
	for each xxBLPeg in Pegs
		xxBLPeg.DisableLighting = 1
		next
Else
	for each xxBLPeg in Pegs
		xxBLPeg.DisableLighting = 0
		next
End If

'Instruction Cards

	If CustomICs = 0 Then
		CustomICsType = Int(Rnd*4)+1
	Else
		CustomICsType = CustomICs
	End If

	If CustomICsType = 1 Then
		pICL.Image = "IC1_left"
		If GameType = 1 Then
			pICR.Image = "dataeast-coin"
		Else
			pICR.Image = "dataeast-freeplay"
		End If
	End If

	If CustomICsType = 2 Then
		pICL.Image = "tmnt_cICL1"
		If GameType = 1 Then
			pICR.Image = "tmnt_cICR1-C"
		Else
			pICR.Image = "tmnt_cICR1-F"
		End If
	End If

	If CustomICsType = 4 Then
		pICL.Image = "tmnt_cICL2"
		If GameType = 1 Then
			pICR.Image = "tmnt_cICR2-C"
		Else
			pICR.Image = "tmnt_cICR2-F"
		End If
	End If
	If CustomICsType = 3 Then
		pICL.Image = "tmnt_cICL3"
		If GameType = 1 Then
			pICR.Image = "tmnt_cICR3-C"
		Else
			pICR.Image = "tmnt_cICR3-F"
		End If
	End If

End Sub


''''
'Auto Plunger
''''

Sub solAutofire(Enabled)
	If Enabled Then
		AutoPlunger.autoFire
PlaysoundAt "Popper", Plunger
		'Playsound "solon"
	End If
End Sub

''''''''
'Targets
''''''''
Dim Target25Step, Target26Step, Target27Step, Target28Step, Target29Step, Target30Step, Target31Step, Target32Step, Target33Step, Target34Step, Target35Step, Target36Step, Target37Step, Target38Step, Target49Step, Target55Step


Sub t25_Hit:vpmTimer.PulseSw(25):pT25A.RotZ = 5:Target25Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t25_timer()
	Select Case Target25Step
		Case 1:pT25A.RotZ = 3
        Case 2:pT25A.RotZ = -2
        Case 3:pT25A.RotZ = 1
        Case 4:pT25A.RotZ = 0:Me.TimerEnabled = 0:Target25Step = 0
     End Select
	Target25Step = Target25Step + 1
End Sub

Sub DoubleTarget5_hit:vpmTimer.PulseSw(26):vpmTimer.PulseSw(25):pT26A.RotZ = 5:Target26Step = 1:t26.TimerEnabled = 1:pT25A.RotZ = 5:Target25Step = 1:t25.TimerEnabled = 1:End Sub

Sub t26_Hit:vpmTimer.PulseSw(26):pT26A.RotZ = 5:Target26Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t26_timer()
	Select Case Target26Step
		Case 1:pT26A.RotZ = 3
        Case 2:pT26A.RotZ = -2
        Case 3:pT26A.RotZ = 1
        Case 4:pT26A.RotZ = 0:Me.TimerEnabled = 0:Target26Step = 0
     End Select
	Target26Step = Target26Step + 1
End Sub

Sub DoubleTarget4_hit:vpmTimer.PulseSw(27):vpmTimer.PulseSw(26):pT27A.RotZ = 5:Target27Step = 1:t27.TimerEnabled = 1:pT26A.RotZ = 5:Target26Step = 1:t26.TimerEnabled = 1:End Sub

Sub t27_Hit:vpmTimer.PulseSw(27):pT27A.RotZ = 5:Target27Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t27_timer()
	Select Case Target27Step
		Case 1:pT27A.RotZ = 3
        Case 2:pT27A.RotZ = -2
        Case 3:pT27A.RotZ = 1
        Case 4:pT27A.RotZ = 0:Me.TimerEnabled = 0:Target27Step = 0
     End Select
	Target27Step = Target27Step + 1
End Sub

Sub DoubleTarget3_hit:vpmTimer.PulseSw(28):vpmTimer.PulseSw(27):pT28A.RotZ = 5:Target28Step = 1:t28.TimerEnabled = 1:pT27A.RotZ = 5:Target27Step = 1:t27.TimerEnabled = 1:End Sub

Sub t28_Hit:vpmTimer.PulseSw(28):pT28A.RotZ = 5:Target28Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t28_timer()
	Select Case Target28Step
		Case 1:pT28A.RotZ = 3
        Case 2:pT28A.RotZ = -2
        Case 3:pT28A.RotZ = 1
        Case 4:pT28A.RotZ = 0:Me.TimerEnabled = 0:Target28Step = 0
     End Select
	Target28Step = Target28Step + 1
End Sub

Sub DoubleTarget2_hit:vpmTimer.PulseSw(29):vpmTimer.PulseSw(28):pT29A.RotZ = 5:Target29Step = 1:t29.TimerEnabled = 1:pT28A.RotZ = 5:Target28Step = 1:t28.TimerEnabled = 1:End Sub

Sub t29_Hit:vpmTimer.PulseSw(29):pT29A.RotZ = 5:Target29Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t29_timer()
	Select Case Target29Step
		Case 1:pT29A.RotZ = 3
        Case 2:pT29A.RotZ = -2
        Case 3:pT29A.RotZ = 1
        Case 4:pT29A.RotZ = 0:Me.TimerEnabled = 0:Target29Step = 0
     End Select
	Target29Step = Target29Step + 1
End Sub

Sub DoubleTarget1_hit:vpmTimer.PulseSw(30):vpmTimer.PulseSw(29):pT30A.RotZ = 5:Target30Step = 1:t30.TimerEnabled = 1:pT29A.RotZ = 5:Target29Step = 1:t29.TimerEnabled = 1:End Sub

Sub t30_Hit:vpmTimer.PulseSw(30):pT30A.RotZ = 5:Target30Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t30_timer()
	Select Case Target30Step
		Case 1:pT30A.RotZ = 3
        Case 2:pT30A.RotZ = -2
        Case 3:pT30A.RotZ = 1
        Case 4:pT30A.RotZ = 0:Me.TimerEnabled = 0:Target30Step = 0
     End Select
	Target30Step = Target30Step + 1
End Sub

Sub t33_Hit:vpmTimer.PulseSw(33):pT33A.RotZ = 5:Target33Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t33_timer()
	Select Case Target33Step
		Case 1:pT33A.RotZ = 3
        Case 2:pT33A.RotZ = -2
        Case 3:pT33A.RotZ = 1
        Case 4:pT33A.RotZ = 0:Me.TimerEnabled = 0:Target33Step = 0
     End Select
	Target33Step = Target33Step + 1
End Sub

Sub DoubleTarget6_hit:vpmTimer.PulseSw(33):vpmTimer.PulseSw(34):pT33A.RotZ = 5:Target33Step = 1:t33.TimerEnabled = 1:pT34A.RotZ = 5:Target34Step = 1:t34.TimerEnabled = 1:End Sub

Sub t34_Hit:vpmTimer.PulseSw(34):pT34A.RotZ = 5:Target34Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t34_timer()
	Select Case Target34Step
		Case 1:pT34A.RotZ = 3
        Case 2:pT34A.RotZ = -2
        Case 3:pT34A.RotZ = 1
        Case 4:pT34A.RotZ = 0:Me.TimerEnabled = 0:Target34Step = 0
     End Select
	Target34Step = Target34Step + 1
End Sub

Sub DoubleTarget7_hit:vpmTimer.PulseSw(34):vpmTimer.PulseSw(35):pT34A.RotZ = 5:Target34Step = 1:t34.TimerEnabled = 1:pT35A.RotZ = 5:Target35Step = 1:t35.TimerEnabled = 1:End Sub

Sub t35_Hit:vpmTimer.PulseSw(35):pT35A.RotZ = 5:Target35Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t35_timer()
	Select Case Target35Step
		Case 1:pT35A.RotZ = 3
        Case 2:pT35A.RotZ = -2
        Case 3:pT35A.RotZ = 1
        Case 4:pT35A.RotZ = 0:Me.TimerEnabled = 0:Target35Step = 0
     End Select
	Target35Step = Target35Step + 1
End Sub

Sub t36_Hit:vpmTimer.PulseSw(36):pT36A.RotZ = 5:Target36Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t36_timer()
	Select Case Target36Step
		Case 1:pT36A.RotZ = 3
        Case 2:pT36A.RotZ = -2
        Case 3:pT36A.RotZ = 1
        Case 4:pT36A.RotZ = 0:Me.TimerEnabled = 0:Target36Step = 0
     End Select
	Target36Step = Target36Step + 1
End Sub

Sub DoubleTarget8_hit:vpmTimer.PulseSw(36):vpmTimer.PulseSw(37):pT36A.RotZ = 5:Target36Step = 1:t36.TimerEnabled = 1:pT37A.RotZ = 5:Target37Step = 1:t37.TimerEnabled = 1:End Sub

Sub t37_Hit:vpmTimer.PulseSw(37):pT37A.RotZ = 5:Target37Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t37_timer()
	Select Case Target37Step
		Case 1:pT37A.RotZ = 3
        Case 2:pT37A.RotZ = -2
        Case 3:pT37A.RotZ = 1
        Case 4:pT37A.RotZ = 0:Me.TimerEnabled = 0:Target37Step = 0
     End Select
	Target37Step = Target37Step + 1
End Sub

Sub DoubleTarget9_hit:vpmTimer.PulseSw(37):vpmTimer.PulseSw(38):pT37A.RotZ = 5:Target37Step = 1:t37.TimerEnabled = 1:pT38A.RotZ = 5:Target38Step = 1:t38.TimerEnabled = 1:End Sub

Sub t38_Hit:vpmTimer.PulseSw(38):pT38A.RotZ = 5:Target38Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t38_timer()
	Select Case Target38Step
		Case 1:pT38A.RotZ = 3
        Case 2:pT38A.RotZ = -2
        Case 3:pT38A.RotZ = 1
        Case 4:pT38A.RotZ = 0:Me.TimerEnabled = 0:Target38Step = 0
     End Select
	Target38Step = Target38Step + 1
End Sub

Sub t49_Hit:vpmTimer.PulseSw(49):pT49A.RotZ = 5:Target49Step = 0:Me.TimerEnabled = 1:End Sub			
Sub t49_timer()
	Select Case Target49Step
		Case 1:pT49A.RotZ = 3
        Case 2:pT49A.RotZ = -2
        Case 3:pT49A.RotZ = 1
        Case 4:pT49A.RotZ = 0:Me.TimerEnabled = 0:Target49Step = 0
     End Select
	Target49Step = Target49Step + 1
End Sub

Sub t55_Hit:vpmTimer.PulseSw(55):pT55A.RotZ = 5:Target55Step = 0:Me.TimerEnabled = 1:End Sub		
'Sub t55_Hit:vpmTimer.PulseSw(55):pT55A.RotZ = 5:Target55Step = 0:Me.TimerEnabled = 1:PlaySoundAt SoundFX("Knocker",DOFTargets),t55:End Sub		
Sub t55_timer()
	Select Case Target55Step
		Case 1:pT55A.RotZ = 3
        Case 2:pT55A.RotZ = -2
        Case 3:pT55A.RotZ = 1
        Case 4:pT55A.RotZ = 0:Me.TimerEnabled = 0:Target49Step = 0
     End Select
	Target55Step = Target55Step + 1
End Sub

''''''''''''''''''''''''''''
''' Ramp Switches
''''''''''''''''''''''''''''

Sub sw32_Hit:Controller.Switch(32) = 1:sw32.timerenabled = true:End Sub
Sub sw32_Unhit:Controller.Switch(32) = 0:End Sub

'switch 32 animation

Const Switch32min = 0
Const Switch32max = -20
Dim Switch32dir
Switch32dir = -2

Sub sw32_timer()
 pRampSwitch2B.RotY = pRampSwitch2B.RotY + Switch32dir
	If pRampSwitch2B.RotY >= Switch32min Then
		sw32.timerenabled = False
		pRampSwitch2B.RotY = Switch32min
		Switch32dir = -2
	End If
	If pRampSwitch2B.RotY <= Switch32max Then
		Switch32dir = 4
	End If
End Sub

Sub sw40_Hit:Controller.Switch(40) = 1:sw40.timerenabled = true:End Sub
Sub sw40_Unhit:Controller.Switch(40) = 0:End Sub

'switch 40 animation

Const Switch40min = 0
Const Switch40max = -20
Dim Switch40dir
Switch40dir = -2

Sub sw40_timer()
 pRampSwitch1B.RotY = pRampSwitch1B.RotY + Switch40dir
	If pRampSwitch1B.RotY >= Switch40min Then
		sw40.timerenabled = False
		pRampSwitch1B.RotY = Switch40min
		Switch40dir = -2
	End If
	If pRampSwitch1B.RotY <= Switch40max Then
		Switch40dir = 4
	End If
End Sub

''''''''''''''''''''''''''''
''' Spinner
''''''''''''''''''''''''''''

Sub sw56_Spin():vpmTimer.PulseSw 56:End Sub

''''''''''''''''''''''''''''
''' Bumpers
''''''''''''''''''''''''''''

Dim bump1, bump2, bump3

Sub Bumper1_Hit:vpmTimer.PulseSw 47:RandomSoundBumperTop Bumper1:pBumpCap1.TransZ = -5:bump1 = 1:Me.TimerEnabled = 1:End Sub
Sub Bumper1_Timer()
    Select Case bump1
        Case 1:pBumpCap1.TransZ = -10:bump1 = 2
        Case 2:pBumpCap1.TransZ = -15:bump1 = 3
        Case 3:pBumpCap1.TransZ = -20:bump1 = 4
        Case 4:pBumpCap1.TransZ = -15:bump1 = 5
		Case 5:pBumpCap1.TransZ = -10:bump1 = 6
		Case 6:pBumpCap1.TransZ = -5:bump1 = 7
		Case 7:pBumpCap1.TransZ = 0:bump1 = 8
		Case 8:pBumpCap1.TransZ = 5:bump1 = 9
		Case 9:pBumpCap1.TransZ = 0:bump1 = 1:Me.TimerEnabled = 0
    End Select
	pMike_ArmL.ObjRotX = pBumpCap1.TransZ
	pMike_ArmR.ObjRotX = pBumpCap1.TransZ
	pMike_LegL.ObjRotX = pBumpCap1.TransZ
	pMike_LegR.ObjRotX = pBumpCap1.TransZ
	pMike_Head.ObjRotX = pBumpCap1.TransZ
	pMike_Belt1.ObjRotX = pBumpCap1.TransZ
	pMike_Belt2.ObjRotX = pBumpCap1.TransZ
	pMike_Torso.ObjRotX = pBumpCap1.TransZ
	pNunChuk1a.ObjRotX = pBumpCap1.TransZ
	pNunChuk1b.ObjRotX = pBumpCap1.TransZ
	pNunChuk1c.ObjRotX = pBumpCap1.TransZ
	pNunChuk2a.ObjRotX = pBumpCap1.TransZ
	pNunChuk2b.ObjRotX = pBumpCap1.TransZ
	pNunChuk2c.ObjRotX = pBumpCap1.TransZ

End Sub

Sub Bumper2_Hit:vpmTimer.PulseSw 46:RandomSoundBumperMiddle Bumper2:bump2 = 1:Me.TimerEnabled = 1:End Sub
Sub Bumper2_Timer()
    Select Case bump2
        Case 1:
        Case 2:
        Case 3:
        Case 4:Me.TimerEnabled = 0
    End Select
End Sub

Sub Bumper3_Hit:vpmTimer.PulseSw 48:RandomSoundBumperBottom Bumper3:bump3 = 1:Me.TimerEnabled = 1:End Sub
Sub Bumper3_Timer()
    Select Case bump3
        Case 1:
        Case 2:
        Case 3:
        Case 4:Me.TimerEnabled = 0
    End Select
End Sub


''''''''''''''''''''''''''''
''' Slingshots
''''''''''''''''''''''''''''

Dim LeftSlingshotStep,RightSlingshotStep


Sub LeftSlingShot_Slingshot:vpmTimer.PulseSw 21:LeftSlingshota.visible = false:pSlingL.TransZ = -8:LeftSlingshotb.visible = true:RandomSoundSlingshotLeft pSlingL:LeftSlingshotStep = 0:Me.TimerEnabled = 1:End Sub
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


Sub RightSlingShot_Slingshot:vpmTimer.PulseSw 22:RightSlingshota.visible = false:pSlingR.TransZ = -8:RightSlingshotb.visible = true:RandomSoundSlingshotRight pSlingR:vpmTimer.PulseSw 21:RightSlingshotStep = 0:Me.TimerEnabled = 1:End Sub
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

''''''''''''''''''''''''''''
''' Switches
''''''''''''''''''''''''''''

Sub sw14_Hit()
	Switch14dir = 1
	Sw14Move = 1
	Me.TimerEnabled = true
	Controller.Switch(14) = 1
End Sub

Sub sw14_unHit()
	Switch14dir = -1
	Sw14Move = 5
	Me.TimerEnabled = true
	Controller.Switch(14) = 0
End Sub

Sub sw17_Hit()
	Switch17dir = 1
	Sw17Move = 1
	Me.TimerEnabled = true
	Controller.Switch(17) = 1
End Sub

Sub sw17_unHit()
	Switch17dir = -1
	Sw17Move = 5
	Me.TimerEnabled = true
	Controller.Switch(17) = 0
End Sub

Sub sw18_Hit()
	Switch18dir = 1
	Sw18Move = 1
	Me.TimerEnabled = true
	Controller.Switch(18) = 1
End Sub

Sub sw18_unHit()
	Switch18dir = -1
	Sw18Move = 5
	Me.TimerEnabled = true
	Controller.Switch(18) = 0
End Sub

Sub sw19_Hit()
	Switch19dir = 1
	Sw19Move = 1
	Me.TimerEnabled = true
	Controller.Switch(19) = 1
End Sub

Sub sw19_unHit()
	Switch19dir = -1
	Sw19Move = 5
	Me.TimerEnabled = true
	Controller.Switch(19) = 0
End Sub

Sub sw20_Hit()
	Switch20dir = 1
	Sw20Move = 1
	Me.TimerEnabled = true
	Controller.Switch(20) = 1
End Sub

Sub sw20_unHit()
	Switch20dir = -1
	Sw20Move = 5
	Me.TimerEnabled = true
	Controller.Switch(20) = 0
End Sub

Sub sw50_Hit()
	Controller.Switch(50) = 1
End Sub

Sub sw50_unHit()
	Controller.Switch(50) = 0
End Sub

'Rollover Animations


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



Dim Switch17dir, SW17Move
'Switch19dir = -2

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


Dim Switch20dir, SW20Move

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

' ===============================================================================================
' Spinning Pizza
' ===============================================================================================

Dim discAngle, stepAngle, stopDiscs, discsAreRunning

InitDiscs()

Sub InitDiscs()
	discAngle 			= 0
	discsAreRunning		= False
End Sub

Sub SolPizzaSpin(Enabled)

	ttPizza.MotorOn = Enabled

	If Enabled Then
PlaysoundAtVol "Wheel_Spinn" ,PizzaTrigger, 0.03
		stepAngle			= 20.0
		discsAreRunning		= True
		stopDiscs			= False
		DiscsTimer.Interval = 20
		DiscsTimer.Enabled 	= True
	Else
		stopDiscs			= True
		discsAreRunning		= True
	End If
End Sub

Sub DiscsTimer_Timer()
	' calc angle
	discAngle = discAngle + stepAngle
	If discAngle >= 360 Then
		discAngle = discAngle - 360
	End If

	' rotate discs

	pPizza.RotY = discAngle

	If stopDiscs Then
		stepAngle = stepAngle - 0.1
		If stepAngle <= 0 Then
			DiscsTimer.Enabled 	= False
		End If
	End If
End Sub

''''''''''''''''''''''''''''
''' Sewer
''''''''''''''''''''''''''''

Sub Kicker4_Hit():PlaySoundAt "VUKEnter",Kicker4:End Sub


Dim TempBallImage
Sub sw41_Hit()
	TempBallImage = ActiveBall.image
	If TempBallImage = "pinball_ball2" Then
		ActiveBall.image = "pinball_ball2_dim"
	End If
	If TempBallImage = "PinballBlizzardBlue" Then
		ActiveBall.image = "PinballBlizzardBlue_dim"
	End If
	If TempBallImage = "PinballOutrageousOrange" Then
		ActiveBall.image = "PinballOutrageousOrange_dim"
	End If
	If TempBallImage = "PinballPurplePizzazz" Then
		ActiveBall.image = "PinballPurplePizzazz_dim"
	End If
	If TempBallImage = "PinballRadicalRed" Then
		ActiveBall.image = "PinballRadicalRed_dim"
	End If
	Controller.Switch(41) = 1
End Sub

Sub sw41_unHit():ActiveBall.image = TempBallImage:End Sub

Sub SewerUpKick(Enabled)
	PlaySoundAt "VUKOut",sw41
 	sw41.Kick 0,45,1.56  'Power = 45
	Controller.Switch(41) = 0
End Sub

Sub SewerOpen(enabled)
	If Enabled Then
		SewerDir = 1
		SewerCover.Collidable = true
		pSewerCap.RotX = 95
		SewerOpenStep = 1
		SewerOpenTimer.Enabled = True
	Else
		SewerOpenStep = 7
		SewerDir = -1
	End If
End Sub

Dim SewerOpenStep, SewerDir

Sub SewerOpenTimer_timer()
	Select Case SewerOpenStep
		Case 0:pSewerCap.RotX = 90:fSewerShadow.visible = 0
		Case 1:pSewerCap.RotX = 95:fSewerShadow.visible = 1:fSewerShadow.imageA = "sewershadow1":If SewerDir = 1 then PlaySoundAt "SOL_on",sw41 End If
		Case 2:	pSewerCap.RotX = 100:fSewerShadow.imageA = "sewershadow1"
		Case 3:	pSewerCap.RotX = 105:fSewerShadow.imageA = "sewershadow2"
		Case 4:	pSewerCap.RotX = 110:fSewerShadow.imageA = "sewershadow3"
		Case 5:	pSewerCap.RotX = 115:fSewerShadow.imageA = "sewershadow4"
		Case 6:	pSewerCap.RotX = 120:fSewerShadow.imageA = "sewershadow5"
		Case 7:	pSewerCap.RotX = 125:fSewerShadow.imageA = "sewershadow6":If SewerDir = -1 then PlaySoundAt "SOL_off",sw41 End If		
		Case 8:	pSewerCap.RotX = 130:fSewerShadow.imageA = "sewershadow7"
	End Select
	pSewerCapB.RotX = pSewerCap.RotX
If SewerOpenStep = 8 Then
	SewerOpenStep = 8 
	Else
	If SewerOpenStep = 0 Then 
		SewerOpenStep = 0 
		SewerCover.Collidable = False
		SewerOpenTimer.Enabled = False
	Else
		SewerOpenStep = SewerOpenStep + SewerDir
	End If
End If
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''  Ball Through system''''''''''''''''''''''''''
'''''''''''''''''''''by cyberpez''''''''''''''''''''''''''''''''
''''''''''''''''based off of EalaDubhSidhe's''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Dim BallCount
Dim cBall1, cBall2, cBall3, cBall4

dim bstatus

Sub CreatBalls()
	Controller.Switch(11) = 1
	Controller.Switch(12) = 1
	Controller.Switch(13) = 1
	Set cBall1 = Kicker1.CreateSizedballWithMass(BallRadius,Ballmass)
	Set cBall2 = Kicker2.CreateSizedballWithMass(BallRadius,Ballmass)
	Set cBall3 = Kicker3.CreateSizedballWithMass(BallRadius,Ballmass)
	Set cBall4 = Kicker5.CreateSizedballWithMass(BallRadius,Ballmass)

	If BallMod = 2 Then
		cBall1.Image = "PinballLaserLemon"	
		cBall2.Image = "PinballOutrageousOrange"
		cBall3.Image = "PinballRadicalRed"
		cBall4.Image = "PinballRadicalRed"
	End If
	Kicker5.Kick 0,1
	Kicker5.enabled = false
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

Dim TurtleBall

Sub SetBallMod_hit()

	If BallMod = 1 then

	Select Case TurtleBall
		Case 1: ActiveBall.Image = "PinballBlizzardBlue":TurtleBall=2
		Case 2: ActiveBall.Image = "PinballPurplePizzazz":TurtleBall=3
		Case 3: ActiveBall.Image = "PinballRadicalRed":TurtleBall=4
		Case 4: ActiveBall.Image = "PinballOutrageousOrange":TurtleBall=1
	End Select
	End If
End Sub


'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub Drain_Hit()
	RandomSoundDrain Drain
	UpdateTrough
	Controller.Switch(10) = 1
	fgBall = true
	iBall = iBall + 1
	BallsInPlay = BallsInPlay - 1
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
		RandomSoundBallRelease Kicker1
		Kicker1.Kick 70,5
		iBall = iBall - 1
		fgBall = false
		BallsInPlay = BallsInPlay + 1
		UpdateTrough
	end if
End Sub

'**********
' Gi Lights
'**********

dim GION

Sub SolGi(Enabled)
    Dim obj
    If Enabled Then

SetLamp 200, 0
SetLamp 111, 0
	If 	GION = 1 then playsound "flasher_relay_off", 0
	GION = 0
    Else

SetLamp 200, 1
SetLamp 111, 1
	If 	GION = 0 then playsound "flasher_relay_on", 0
	GION = 1
    End If
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
Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image")
Dim TextureRedBulbArray: TextureRedBulbArray = Array("Bulb Red trans", "Bulb Red")
Dim TextureBlueBulbArray: TextureBlueBulbArray = Array("Bulb Blue trans", "Bulb Blue")
Dim TextureGreenBulbArray: TextureGreenBulbArray = Array("Bulb Green trans", "Bulb Green")
Dim TextureYellowBulbArray: TextureYellowBulbArray = Array("Bulb Yellow trans", "Bulb Yellow")
Dim BulbArray1: BulbArray1 = Array("Bulb_on_texture", "Bulb_texture")
Dim RedLight: RedLight = Array("Bulb_texture_on", "Bulb_texture_66", "Bulb_texture_33", "Bulb_texture_off")
Dim BlueLight: BlueLight = Array("Bulb_Blue_texture_on", "Bulb_Blue_texture_66", "Bulb_Blue_texture_33", "Bulb_Blue_texture_off")
Dim GreenLight: GreenLight = Array("Bulb_Green_texture_on", "Bulb_Green_texture_66", "Bulb_Green_texture_33", "Bulb_Green_texture_off")
Dim YellowLight: YellowLight = Array("Bulb_Yellow_texture_on", "Bulb_Yellow_texture_66", "Bulb_Yellow_texture_33", "Bulb_Yellow_texture_off")
Dim RedBumperCap: RedBumperCap = Array("RaB_Bumpercap_redon", "RaB_Bumpercap_red66", "RaB_Bumpercap_red33", "RaB_Bumpercap")
Dim YellowDome: YellowDome = Array("domeyellowbase", "domeyellowlit")
Dim RedDome: RedDome = Array("domeRedbase", "domeRedlit")
Dim YellowRoundDome: YellowRoundDome = Array("TopFlasherYellow_on", "TopFlasherYellow_66", "TopFlasherYellow_33", "TopFlasherYellow_off")
Dim GreenRoundDome: GreenRoundDome = Array("TopFlasherGreen_on", "TopFlasherGreen_66", "TopFlasherGreen_33", "TopFlasherGreen_off")
Dim YellowDome4: YellowDome4 = Array("domeyellow_on", "domeyellow_66", "domeyellow_33", "domeyellow_off")


Dim TestLight: TestLight = Array("Bulb_Yellow_texture_on", "Bulb_texture_66", "Bulb_Green_texture_33", "Bulb_Blue_texture_off")

 
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
'		CheckDropShadows

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

If Roomfade = 0 Then
		'FadeLUTsingle 200, "LUTCont_", 28
	End If
If Roomfade = 1 Then 
	End If

	NFadeL 1, L1
	NFadeL 2, L2
	NFadeL 3, L3
	NFadeL 4, L4
	NFadeL 5, L5
	NFadeL 6, L6
	NFadeL 7, L7
	NFadeL 8, L8
	NFadeL 9, L9
	NFadeL 10, L10
	NFadeL 11, L11
	NFadeL 12, L12
	NFadeL 13, L13
	NFadeL 14, L14
	NFadeL 15, L15
	NFadeL 16, L16
	NFadeL 17, L17
	NFadeL 18, L18
	NFadeL 19, L19
	NFadeL 20, L20
	NFadeL 21, L21
	NFadeL 22, L22
	NFadeL 23, L23
	NFadeL 24, L24
	NFadeL 25, L25
	NFadeLm 26, L26a
	NFadeLm 26, L26b
	NFadeLm 26, L26c
	NFadeL 26, L26d
	NFadeL 27, L27
	NFadeL 28, L28
	NFadeLm 29, L29a
	NFadeLm 29, L29b
	NFadeLm 29, L29c
	NFadeL 29, L29d
	NFadeL 30, L30
	FadePri4m 31, pBulb31, YellowLight
	FadeDisableLighting 31, pBulb31
	FadeMaterialP 31, pBulb31, TextureArray1

	FadePri4m 32, pBulb32, RedLight
	FadeDisableLighting 32, pBulb32
	FadeMaterialP 32, pBulb32, TextureArray1

	NFadeL 33, L33
	NFadeLm 34, L34a
	NFadeL 34, L34b
	NFadeL 35, L35
	NFadeL 36, L36
	NFadeLm 37, L37a
	NFadeLm 37, L37b
	NFadeLm 37, L37c
	NFadeL 37, L37d
	NFadeL 38, L38

	FadePri4m 39, pBulb39, RedLight
	FadeDisableLighting 39, pBulb39
	FadeMaterialP 39, pBulb39, TextureArray1

	FadePri4m 40, pBulb40, GreenLight
	FadeDisableLighting 40, pBulb40
	FadeMaterialP 40, pBulb40, TextureArray1

	NFadeL 41, L41
	NFadeL 42, L42

If VR_Room = 0 Then
	FadeR 43, l43
    FadeR 44, l44
    FadeR 45, l45
    FadeR 46, l46
    FadeR 47, l47
    FadeR 48, l48
	Else
	FadeObj 43, VRFlasher1, "l43-1", "l43-2", "l43-3","l43-4"
    FadeObj 44, VRFlasher2, "l44-1", "l44-2", "l44-3","l44-4"
    FadeObj 45, VRFlasher3, "l45-1", "l45-2", "l45-3","l45-4"
    FadeObj 46, VRFlasher4, "l46-1", "l46-2", "l46-3","l46-4"
    FadeObj 47, VRFlasher5, "l47-1", "l47-2", "l47-3","l47-4"
    FadeObj 48, VRFlasher6, "l48-1", "l48-2", "l48-3","l48-4"
	End If




	NFadeL 49, L49
	NFadeL 50, L50

	NFadeL 52, L52

	NFadeL 55, L55
	NFadeL 56, L56
	NFadeL 57, L57
	NFadeL 58, L58
	NFadeL 59, L59
	NFadeL 60, L60
	NFadeL 61, L61

	NFadeLm 130, FlashLight6a
	NFadeLm 130, FlashLight6b
	NFadeLm 130, FlashLight6c
	NFadeLm 130, FlashLight6d
	FadeMaterialP 130, pFlasherDome6b, TextureArray1
	FadePri2m 130, pFlasherDome6b, RedDome
	nFadelm 130, RedBloom
	nFadelm 130, RedBloom1
	flashm 130, flasher1
	flashm 130, flasher2
	flashm 130, flasher3
	flash 130, flasher4




	NFadeLm 109, f9a
	NFadeL 109, f9b

	NFadeLm 112, f12a
	NFadeL 112, f12b

	NFadeLm 113, f13a
	NFadeL 113, f13b



	NFadeLm 114, f14c1
	NFadeLm 114, f14c1
	NFadeLm 114, f14c1
	NFadeLm 114, f14ac
	If SideFlasherColorType = 1 then
		FadePri4m 114, pDome14a, YellowRoundDome
	End If
	If SideFlasherColorType = 2 then
		FadePri4m 114, pDome14a, GreenRoundDome
	End If
	FadeMaterialP 114, pDome14a, TextureArray1
	FadeDisableLighting 114, pDome14a
	NFadeLm 114, f14aa
	NFadeLm 114, f14ab
	NFadeLm 114, f14ba
	NFadeLm 114, f14bc
	If SideFlasherColorType = 1 then
		FadePri4m 114, pDome14b, YellowRoundDome
	End If
	If SideFlasherColorType = 2 then
		FadePri4m 114, pDome14b, GreenRoundDome
	End If
	FadeMaterialP 114, pDome14b, TextureArray1
	FadeDisableLighting 114, pDome14b
	NFadeL 114, f14bb

	NFadeLm 125, f25a
	NFadeLm 125, f25a2
	NFadeLm 125, f25b2
	NFadeL 125, f25b

	NFadeLm 126, f26a
	NFadeLm 126, f26a2
	NFadeLm 126, f26b2
	NFadeL 126, f26b

	NFadeLm 127, f27a
	NFadeLm 127, f27a2
	NFadeLm 127, f27b
	NFadeLm 127, f27b2
	NFadeL 127, f27c

	NFadeLm 128, f28a
	NFadeLm 128, f28a2
	NFadeLm 128, f28b2
	NFadeL 128, f28b


	FadeDisableLighting 129, pFlasherDome5a
	NFadeLm 129, FlashLight5a
    NFadeLm 129, FlashLight5b
    NFadeLm 129, FlashLight5c
    NFadeLm 129, FlashLight5d
	FadeMaterialP 129, pFlasherDome5a, TextureArray1
	FadePri4m 129, pFlasherDome5a, YellowDome4

	FadeDisableLighting 129, pFlasherDome5b
	Flashm 129, Flashlight5b

	NFadeLm 129, FlashLight5b
	FadeMaterialP 129, pFlasherDome5b, TextureArray1
	FadePri4m 129, pFlasherDome5b, YellowDome4



	NFadeL 131, f131

	NFadeLm 132, f132a
	NFadeLm 132, f132b
	NFadeLm 132, f132c
	NFadeL 132, f132d

   End Sub
   
'#end section


''''Additions by CP


Sub FadeDisableLighting(nr, a)
    Select Case FadingLevel(nr)
		Case 2:a.BlendDisableLighting = .33
		Case 3:a.BlendDisableLighting = .66
		Case 4:a.BlendDisableLighting = 0
		Case 5:a.BlendDisableLighting = 1
    End Select
End Sub

Sub FadeDisableLighting1(nr, a)
    Select Case FadingLevel(nr)
		Case 2:a.BlendDisableLighting = .033
		Case 3:a.BlendDisableLighting = .066
		Case 4:a.BlendDisableLighting = 0
		Case 5:a.BlendDisableLighting = .1
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

Sub FadeObj(nr, object, a, b, d, e)
    Select Case FadingLevel(nr)
        Case 2:object.imageA = e:FadingLevel(nr) = 0                   'off...
        Case 3:object.imageA = d:FadingLevel(nr) = 2                   
        Case 4:object.imageA = b:FadingLevel(nr) = 3
        Case 5:object.imageA = a:FadingLevel(nr) = 1                  'On
    End Select
End Sub

Sub FadePri4m(nr, pri, group)
    Select Case FadingLevel(nr)
		Case 2:pri.image = group(1) 'Off
        Case 3:pri.image = group(3) 'Fading...
        Case 4:pri.image = group(2) 'Fading...
        Case 5:pri.image = group(0) 'ON
    End Select
End Sub

Sub FadePri4(nr, pri, group)
    Select Case FadingLevel(nr)
        Case 2:pri.image = group(1):FadingLevel(nr) = 0 'Off
        Case 3:pri.image = group(3):FadingLevel(nr) = 2 'Fading...
        Case 4:pri.image = group(2):FadingLevel(nr) = 3 'Fading...
        Case 5:pri.image = group(0):FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadePri2m(nr, pri, group)
    Select Case FadingLevel(nr)
		Case 2:pri.image = group(1) 'Off
'        Case 3:pri.image = group(3) 'Fading...
'        Case 4:pri.image = group(2) 'Fading...
        Case 5:pri.image = group(0) 'ON
    End Select
End Sub

Sub FadePri2(nr, pri, group)
    Select Case FadingLevel(nr)
        Case 2:pri.image = group(1):FadingLevel(nr) = 0 'Off
'        Case 3:pri.image = group(3):FadingLevel(nr) = 2 'Fading...
'        Case 4:pri.image = group(2):FadingLevel(nr) = 3 'Fading...
        Case 5:pri.image = group(0):FadingLevel(nr) = 1 'ON
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
    '       tbgi2.text = Table1.ColorGradeImage & vbnewline & golut 'debug
        End If
    End Sub
   
    Sub FadeLUTavg(nr, nr2, nr3, nr4, LutName, LutCount)    'FadeLut for two GI strings (WPC)
        If FadingLevel(nr) >2 or FadingLevel(nr2) > 2 or FadingLevel(nr3) >2 or FadingLevel(nr4) > 2 Then
            Dim GoLut
            GoLut = cInt(LutCount * (((FlashLevel(nr)*FlashLevel(nr2)) + (FlashLevel(nr3)*Flashlevel(nr4))) /2) )
            REM tbgi2.text = Table1.ColorGradeImage & vbnewline & golut 'debug
        End If
    End Sub
   
    Sub FadeLUTavgM(nr, nr2, nr3, nr4, nr5, nr6, LutName, LutCount) 'FadeLut for three GI strings (WPC)
        If FadingLevel(nr) >2 or FadingLevel(nr2) > 2 or FadingLevel(nr3) >2 or FadingLevel(nr4) > 2 or _
        FadingLevel(nr5) >2 or FadingLevel(nr6) > 2 Then
            Dim GoLut
            GoLut = cInt(LutCount * (((FlashLevel(nr)*FlashLevel(nr2)) + (FlashLevel(nr3)*Flashlevel(nr4)) + (FlashLevel(nr5)*FlashLevel(nr6)))/3)  )   'what a mess
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


Sub Flash(nr, object)
    Select Case FadingLevel(nr)
		Case 3
			FadingLevel(nr) = 0
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - (1/FlashSpeedDown(nr) * FrameTime)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 3 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + (1/FlashSpeedUp(nr) * FrameTime)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 6 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
		Case 6
			FadingLevel(nr) = 1
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





'*****************************************
'   rothbauerw's Manual Ball Control
'*****************************************

Dim BCup, BCdown, BCleft, BCright
Dim ControlBallInPlay, ControlActiveBall
Dim BCvel, BCyveloffset, BCboostmulti, BCboost

BCboost = 1				'Do Not Change - default setting
BCvel = 4				'Controls the speed of the ball movement
BCyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
BCboostmulti = 3		'Boost multiplier to ball veloctiy (toggled with the B key) 

ControlBallInPlay = false

Sub StartBallControl_Hit()
	Set ControlActiveBall = ActiveBall
	ControlBallInPlay = true
End Sub

Sub StopBallControl_Hit()
	ControlBallInPlay = false
End Sub	

Sub BallControlTimer_Timer()
	If EnableBallControl and ControlBallInPlay then
		If BCright = 1 Then
			ControlActiveBall.velx =  BCvel*BCboost
		ElseIf BCleft = 1 Then
			ControlActiveBall.velx = -BCvel*BCboost
		Else
			ControlActiveBall.velx = 0
		End If

		If BCup = 1 Then
			ControlActiveBall.vely = -BCvel*BCboost
		ElseIf BCdown = 1 Then
			ControlActiveBall.vely =  BCvel*BCboost
		Else
			ControlActiveBall.vely = bcyveloffset
		End If
	End If
End Sub






'*****************************************
'			BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7)

Sub BallShadowUpdate_timer()
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
        If BOT(b).X < tmnt.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (tmnt.Width/2))/7)) '+ 13
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (tmnt.Width/2))/7)) '- 13
        End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
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
End Sub

Sub tRampHelper2a_hit()
	OnWireRamp = 1
End Sub

Sub tRampHelper2b_hit()
	OnWireRamp = 0
End Sub

'//////////////////////////////////////////////////////////////////////
'// TargetBounce
'//////////////////////////////////////////////////////////////////////

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

' Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit
	TargetBouncer activeball, 1
End Sub

'//////////////////////////////////////////////////////////////////////
'// PHYSICS DAMPENERS
'//////////////////////////////////////////////////////////////////////

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

'//////////////////////////////////////////////////////////////////////
'// TRACK ALL BALL VELOCITIES FOR RUBBER DAMPENER AND DROP TARGETS
'//////////////////////////////////////////////////////////////////////

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

'//////////////////////////////////////////////////////////////////////
'// RAMP ROLLING SFX
'//////////////////////////////////////////////////////////////////////

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(6)	

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

'//////////////////////////////////////////////////////////////////////
'// RAMP TRIGGERS
'//////////////////////////////////////////////////////////////////////

Sub ramptrigger01_hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger001_hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger0001_hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger002_hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub ramptrigger002_unhit()
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound
End Sub

Sub ramptrigger02_hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub ramptrigger02_unhit()
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound
End Sub

Sub ramptrigger03_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub ramptrigger03_unhit()
	PlaySoundAt "WireRamp_Stop", ramptrigger03
End Sub

Sub ramptrigger003_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub ramptrigger003_unhit()
	PlaySoundAt "WireRamp_Stop", ramptrigger003
End Sub

'//////////////////////////////////////////////////////////////////////
'// Ball Rolling
'//////////////////////////////////////////////////////////////////////

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
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
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

		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = BOT(b).X
			BallShadowA(b).visible = 1
		End If
	Next
End Sub

'//////////////////////////////////////////////////////////////////////
'// Mechanic Sounds
'//////////////////////////////////////////////////////////////////////

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.  
' Create the following new collections:
' 	Metals (all metal objects, metal walls, metal posts, metal wire guides)
' 	Apron (the apron walls and plunger wall)
' 	Walls (all wood or plastic walls)
' 	Rollovers (wire rollover triggers, star triggers, or button triggers)
' 	Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
' 	Gates (plate gates)
' 	GatesWire (wire gates)
' 	Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.  
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).  
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Tutorial vides by Apophis
' Part 1: 	https://youtu.be/PbE2kNiam3g
' Part 2: 	https://youtu.be/B5cm1Y8wQsk
' Part 3: 	https://youtu.be/eLhWyuYOyGg


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
	PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
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

'//////////////////////////////////////////////////////////////////////
'// TIMERS
'//////////////////////////////////////////////////////////////////////


' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate					'update rolling sounds
End Sub


' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
End Sub
