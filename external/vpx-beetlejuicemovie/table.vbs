Option Explicit
Randomize

'Strange Science (Bally 1986)
'Bally MPU A084-91786-AH06 (6803)
'
' Development by agentEighty6 2020 for Visual Pinball X
' Version 1.0.0

'
' I would like to send a very special thanks to the following.
' - destruk : For being able to freely use the original table design from VP V9
' - BrandonLaw : Invaluable contributor of table resources, improvements, and beta testing
' - bord : Creator of those awesome primitives (because i can only use Blender to build ramps!)
' - jturner : LOTS of tear down / reference pics i found in Flickr
' - nFozzy, flupper, ninuzzu - newer VPX routines and techniques
' - bigus1 : Playtesting and improvements
' - Anyone I may have forgotten. Please let me know and I'll add you.

' Release notes:
' Version 1.0.2 - Fix incorrect lock ball counter in a very rare scenario.
' Version 1.0.1 - Minor update to side rails for DT users. 
' Version 1.0.0 - New VPX version of Strange Science built loosely from the V9 version.  
'				Most of the script was rewritten and the table elements were overhauled.
'				New VPX routines (flipper physics, shadowing, flasher lighting, ball jumping, etc.).
'				Playfield cleanup/optimization.


'Table Components
' Layer1 - Table Mechanics
' Layer2 - Workarea
' Layer3 - Primitives
' Layer4 - Plastics
' Layer5 - Walls
' Layer6 - Misc, timers, base walls, testing tools, etc, and Level 3 Elements
' Layer7 - Export to Blender
' Layer8 - GI Lights Plastic Bases
' Layer9 - Flashers and Flasher Lighting
' Layer10 - Table Lamps
' Layer11 - GI Lights Plastic Tops


' ****************************************************
' OPTIONS
' ****************************************************

' Volume devided by - lower gets higher sound
Const VolDiv = 900    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )

Dim AutoPowerSaver, DynamicLampIntensity, DynamicGIIntensity, LetTheBallJump, ShowBallShadow, SubstMagnaSaveButtons

' LET THE BALL JUMP A BIT
'	0 = off
'	1 to 6 = ball jump intensity (4 = default)
LetTheBallJump = 4

' AUTOMATICALLY ACTIVATE POWER SAVER WHEN AVAILABLE
'	False = manual power saver via left magna save (default)
'	True = automatically activate power saver
AutoPowerSaver = True

' DYNAMIC LAMP INTENSITY MULTIPLIER
'	Numeric value to dynamically multiply every playfield lamp's intensity (default=1)
DynamicLampIntensity = 1

' DYNAMIC GI INTENSITY MULTIPLIER
'	Numeric value to dynamically multiply every GI lamp's intensity (default=1, 0=Off)
DynamicGIIntensity = 1

' SHOW BALL SHADOWS
'	0 = no ball shadows
'	1 = ball shadows are visible
ShowBallShadow = 1

' FLIPPERS ALSO ACTIVATE MAGNASAVE 
' Set to True if Your cabinet has no Magnasave Buttons. (default=False)
' Setting this to true allows right flipper button to act as right magnasave
' Setting this to true also turns on AutoPowerSaver instead of using left magnasave
SubstMagnaSaveButtons = False
	If SubstMagnaSaveButtons = True Then
		AutoPowerSaver = True
	End If

' ****************************************************
' standard definitions
' ****************************************************

Const cGameName="strngsci"
Const UseSolenoids 	= 2
Const UseLamps 		= 1
Const UseSync 		= 1
Const HandleMech 	= 0
Const UseGI			= 0

'Standard Sounds
Const SSolenoidOn="solon"
Const SSolenoidOff="soloff"
Const SFlipperOn="FlipperUp"
Const SFlipperOff="FlipperDown"
Const sCoin="Coin3"

Const BallSize = 50
Const BallMass = 1

Const sMiddleJet=2
Const sLeftJet=3
Const sRightJet=4
Const sLSling=5
Const sRSling=6
Const sLeftPowerSaver=7
Const sRSaucer=9
Const sLowerBallStop=10
Const sRaiseBallStop=11
Const sOutKicker=12
Const sOutHole=14
Const sKnocker=15
Const sGiLights=17
Const sBackBoxLight=20
Const sEnable=19



If Version < 10500 Then
	MsgBox "This table requires Visual Pinball 10.6 or newer!" & vbNewLine & "Your version: " & Replace(Version/1000,",","."), , "Strange Science VPX"
End If

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package."
On Error Goto 0

LoadVPM "01500100", "6803.VBS", 3.2


SolCallback(sMiddleJet)="vpmSolSound ""Bumper1"","				'Sol2
SolCallback(sLeftJet)="vpmSolSound ""Bumper2"","				'Sol3
SolCallback(sRightJet)="vpmSolSound ""Bumper3"","				'Sol4
SolCallback(sLSling)="vpmSolSound ""sling"","				'Sol5
SolCallback(sRSling)="vpmSolSound""sling"","				'Sol6

SolCallback(sLeftPowerSaver)="SolPowerSaver"				'sol7

SolCallback(sRSaucer)="bsRSaucer.SolOut Not"				'Sol9

SolCallback(sLowerBallStop)="LowBallStop Not"				'sol10
SolCallback(sRaiseBallStop)="RaiseBallStop Not"				'sol11

SolCallback(sOutKicker)="TroughOut"							'sol12
SolCallback(sOuthole)="HandleDrain"							'Sol14

SolCallback(sKnocker)="vpmSolSound ""Knocker"","			'Sol15
SolCallback(sGiLights)="GILightControl"						'Sol17

'SolCallback(sBackboxLight)="vpmFlasher "					'Sol20
SolCallback(sEnable)="vpmNudge.SolGameOn"					'Sol19

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"



If LetTheBallJump > 6 Then LetTheBallJump = 6 : If LetTheBallJump < 0 Then LetTheBallJump = 0

' **** enables manual ball control with C key (enable/disable control) ********************
' **** and B key (speed boost) and arrow keys *********************************************
Dim IsDebugBallsModeOn : IsDebugBallsModeOn = False

Dim DesktopMode: DesktopMode = Science.ShowDT
If DesktopMode = True Then
	TextBox001.visible=True
	TextBox002.visible=True
	RampLBTop.visible=True
	RampRBTop.visible=True
	RampLBTop2.visible=False
	RampRBTop2.visible=False
Else
	TextBox001.visible=False
	TextBox002.visible=False
	RampLBTop.visible=False
	RampRBTop.visible=False
	RampLBTop2.visible=True
	RampRBTop2.visible=True
End if


Sub Science_Init
    T17a.IsDropped=1:T18a.IsDropped=1:T19a.IsDropped=1
    T22a.IsDropped=1:T21a.IsDropped=1:T20a.IsDropped=1
	BallRelease.CreateBall
	Kicker11.CreateBall
	Kicker10.CreateBall
	Kicker9.CreateBall
	Kicker8.CreateBall
	PowerSaverOff
	PowerSaverWall.IsDropped=1

	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName=cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine="Breaking Bad, Balutito 2021"
		.HandleMechanics=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.Hidden= 1
		.Games("strngsci").Settings.Value("sound") = 0
		.Games("strngsci").Settings.Value("samples") = 0
		On Error Resume Next
		.Run 
		If Err Then MsgBox Err.Description
		On Error Goto 0
		End With
	On Error Goto 0


	' init timers
	PinMAMETimer.Interval 				= 1 'PinMAMEInterval
    PinMAMETimer.Enabled  				= 1

	DisplayTimer.Enabled = DesktopMode

	vpmNudge.TiltSwitch=15
	vpmNudge.Sensitivity=3
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot,Wall70)

 	Set bsRSaucer=New cvpmBallStack
    bsRSaucer.InitSaucer Kicker1,32,262,15
    bsRSaucer.InitExitSnd "popper","solon"
 	bsRSaucer.KickAngleVar=4

    Controller.Switch(33)=1
    Controller.Switch(34)=1
    Controller.Switch(35)=1
    Controller.Switch(36)=1
    Controller.Switch(37)=1
	
	Set Lights(1) = L1
	Set Lights(2) = L2
	Set Lights(3) = L3
	Set Lights(4) = L4
	Set Lights(5) = L5
	Set Lights(6) = L6
	Set Lights(7) = L7
	Set Lights(8) = L8
	Set Lights(9) = L9
	Set Lights(10) = Bumper3a 
	Set Lights(11) = L11
	Set Lights(12) = L12
	Set Lights(13) = L13	
	Set Lights(14) = L14
	Set Lights(15) = L15
	Set Lights(17) = L17
	Set Lights(18) = L18
	Set Lights(19) = L19
	Set Lights(20) = L20
	Set Lights(21) = L21
	Set Lights(22) = L22
	Set Lights(23) = L23
	Set Lights(24) = L24
	Set Lights(25) = Bumper1a 
	Set Lights(26) = L26
	Set Lights(27) = L27
	Set Lights(28) = L28
	Set Lights(29) = L29 ' Spark 3
	Set Lights(30) = L30
	Set Lights(31) = L31
	Set Lights(33) = L33
	Set Lights(34) = L34
	Set Lights(35) = L35
	Set Lights(36) = L36
	Set Lights(37) = L37
	Set Lights(38) = L38 
	Set Lights(39) = L39
	Set Lights(40) = L40
	Set Lights(41) = Bumper2a 
	Set Lights(42) = L42
	Set Lights(43) = L43
	Set Lights(44) = L44 ' Spark 1
	Set Lights(45) = L45 
	Set Lights(46) = L46
	Set Lights(47) = L47
	Set Lights(49) = L49
	Set Lights(50) = L50
	Set Lights(51) = L51
	Set Lights(52) = L52
	Set Lights(53) = L53
	Set Lights(54) = L54
	Set Lights(55) = L55
	Set Lights(56) = L56
	Set Lights(57) = L57
	Set Lights(58) = L58
	Set Lights(59) = L59 'A
	Set Lights(60) = L60 ' Dbl Flash 3 Lightning Arrow (curr rt electrode)
	Set Lights(61) = L61 '  Atomic smasher bottom (loaded)
	Set Lights(62) = L62 ' Dbl Flash 2 Lightning Arrow (curr lft electrode)	
	Set Lights(63) = L63
	Set Lights(65) = L65
	Set Lights(66) = L66
	Set Lights(67) = L67
	Set Lights(68) = L68
	Set Lights(69) = L69
	Set Lights(70) = L70
	Set Lights(71) = L71
	Set Lights(72) = L72
	Set Lights(73) = L73
	Set Lights(74) = L74
	Set Lights(75) = L75 'B
	Set Lights(76) = L76 ' Dbl Flash 1 Lightning Arrow (curr large atom)	
	Set Lights(77) = L77 '  Atomic smasher top (loaded)
	Set Lights(78) = L78
	Set Lights(79) = L79
	Set Lights(81) = L81
	Set Lights(82) = L82
	Set Lights(83) = L83
	Set Lights(84) = L84
	Set Lights(85) = L85
	Set Lights(86) = L86
	Set Lights(87) = L87
	Set Lights(88) = L88
	Set Lights(89) = L89
	Set Lights(90) = L90 'L
	Set Lights(91) = L91
	Set Lights(92) = L92 ' Spark 2
	Set Lights(93) = L93 
	Set Lights(94) = L94
	Set Lights(95) = L95
	
	SetDynamicGILightIntensity
	SetDynamicLampIntensity

	
End Sub
'**********
' Music
'**********



DIM music
music = "BeetleJuice.mp3"
PlayMusic "BeetleJuice.mp3" 
	
Sub Table1_MusicDone()
    Playmusic
End Sub

' ****************************************************
' keys
' ****************************************************
Sub Science_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then Plunger.PullBack 
	If keycode = LeftMagnaSave Then Controller.Switch(5) = True  ' LeftMagnaSave PowerSaver
	if SubstMagnaSaveButtons = False then
		If keycode = RightMagnaSave Then Controller.Switch(7) = True  ' RightMagnaSave 
	else
		If keycode = RightFlipperKey Then Controller.Switch(7) = True  ' RightMagnaSave 
	end if
	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Science_KeyUp(ByVal keycode)
    If keycode = PlungerKey Then Plunger.Fire 
	If keycode = LeftMagnaSave Then Controller.Switch(5) = False  ' LeftMagnaSave PowerSaver Reset
	if SubstMagnaSaveButtons = False then
		If keycode = RightMagnaSave Then Controller.Switch(7) = False  ' RightMagnaSave 
	else
		If keycode = RightFlipperKey Then Controller.Switch(7) = False  ' RightMagnaSave 
	end if
    If vpmKeyUp(keycode) Then Exit Sub
End Sub


' ****************************************************
' flipper subs
' ****************************************************
Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.fire
		PlaySound "fx_Flipperup1"
		'LeftFlipper.RotateToEnd
    Else
		PlaySound "fx_Flipperdown1"
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.fire
		PlaySound "fx_Flipperup2"
		'RightFlipper.RotateToEnd
		RightFlipper1.RotateToEnd
    Else 
		PlaySound "fx_Flipperdown2"
        RightFlipper.RotateToStart
		RightFlipper1.RotateToStart
    End If
End Sub


Sub HandleDrain(Enabled)
	If Enabled Then
		Drain.Kick 60,20
		TroughBalls=TroughBalls+1
		PlaySound"Popper"
		Controller.Switch(8)=0
	End If
End Sub

Sub Kicker8_Hit
If TroughBalls<5 Then Me.Kick 60,5
End Sub
Sub Kicker9_Hit
If TroughBalls<4 Then Me.Kick 60,5
End Sub
Sub Kicker10_Hit
If TroughBalls<3 Then Me.Kick 60,5
End Sub
Sub Kicker11_Hit
If TroughBalls<2 Then Me.Kick 60,5
End Sub

Sub Trigger15_Hit:Controller.Switch(33)=1:End Sub
Sub Trigger15_unHit:Controller.Switch(33)=0:End Sub
Sub Trigger16_Hit:Controller.Switch(34)=1:End Sub
Sub Trigger16_unHit:Controller.Switch(34)=0:End Sub
Sub Trigger17_Hit:Controller.Switch(35)=1:End Sub
Sub Trigger17_unHit:Controller.Switch(35)=0:End Sub
Sub Trigger18_Hit:Controller.Switch(36)=1:End Sub
Sub Trigger18_unHit:Controller.Switch(36)=0:End Sub
Sub Trigger19_Hit:Controller.Switch(37)=1:End Sub
Sub Trigger19_unHit:Controller.Switch(37)=0:End Sub

Dim bsRSaucer,TroughBalls,bump1,bump2,bump3
TroughBalls=5

Sub TroughOut(Enabled)
	If Enabled Then
		TroughBalls=TroughBalls-1
		Playsound"ballrel"
		BallRelease.Kick 60,5
		Kicker11.Kick 60,5
		Kicker10.Kick 60,5
		Kicker9.Kick 60,5
		Kicker8.Kick 60,5
	End If
End Sub


Sub SolPowerSaver(Enabled)
	If Enabled Then
		PowerSaverOn
	Else
		PowerSaverOff
	End If
End Sub

Sub PowerSaverTrigger_Hit
	If AutoPowerSaver = true then 
		vpmTimer.PulseSw 5
	End If
End Sub


Sub PowerSaverReset_Hit
	PowerSaverWall.IsDropped=1
End Sub


Sub PowerSaverOn
	PowerSaverRubberOn.visible=1:	PowerSaverRubberOn.collidable=1
	PowerSaverWall01On.visible=1:	PowerSaverWall01On.sideVisible=1: PowerSaverWall01On.collidable=1
	PowerSaverWall02On.visible=1:	PowerSaverWall02On.sideVisible=1: PowerSaverWall02On.collidable=1
	PowerSaverWall03On.visible=1:	PowerSaverWall03On.sideVisible=1: PowerSaverWall03On.collidable=1
	PowerSaverWall04On.visible=1:	PowerSaverWall04On.sideVisible=1: PowerSaverWall04On.collidable=1
	PowerSaverWall05On.visible=1:	PowerSaverWall05On.sideVisible=1: PowerSaverWall05On.collidable=1
	PowerSaverWall06On.visible=1:	PowerSaverWall06On.sideVisible=1: PowerSaverWall06On.collidable=1
	PowerSaverWall07On.visible=1:	PowerSaverWall07On.sideVisible=1: PowerSaverWall07On.collidable=1

	PowerSaverRubberOff.visible=0:	PowerSaverRubberOff.collidable=0
	PowerSaverWall01Off.visible=0:	PowerSaverWall01Off.sideVisible=0: PowerSaverWall01Off.collidable=0
	PowerSaverWall02Off.visible=0:	PowerSaverWall02Off.sideVisible=0: PowerSaverWall02Off.collidable=0
	PowerSaverWall03Off.visible=0:	PowerSaverWall03Off.sideVisible=0: PowerSaverWall03Off.collidable=0
	PowerSaverWall04Off.visible=0:	PowerSaverWall04Off.sideVisible=0: PowerSaverWall04Off.collidable=0
	PowerSaverWall05Off.visible=0:	PowerSaverWall05Off.sideVisible=0: PowerSaverWall05Off.collidable=0
	PowerSaverWall06Off.visible=0:	PowerSaverWall06Off.sideVisible=0: PowerSaverWall06Off.collidable=0
	PowerSaverWall07Off.visible=0:	PowerSaverWall07Off.sideVisible=0: PowerSaverWall07Off.collidable=0

	PowerSaverWall.IsDropped=0
End Sub

Sub PowerSaverOff
	PowerSaverRubberOn.visible=0:	PowerSaverRubberOn.collidable=0
	PowerSaverWall01On.visible=0:	PowerSaverWall01On.sideVisible=0: PowerSaverWall01On.collidable=0
	PowerSaverWall02On.visible=0:	PowerSaverWall02On.sideVisible=0: PowerSaverWall02On.collidable=0
	PowerSaverWall03On.visible=0:	PowerSaverWall03On.sideVisible=0: PowerSaverWall03On.collidable=0
	PowerSaverWall04On.visible=0:	PowerSaverWall04On.sideVisible=0: PowerSaverWall04On.collidable=0
	PowerSaverWall05On.visible=0:	PowerSaverWall05On.sideVisible=0: PowerSaverWall05On.collidable=0
	PowerSaverWall06On.visible=0:	PowerSaverWall06On.sideVisible=0: PowerSaverWall06On.collidable=0
	PowerSaverWall07On.visible=0:	PowerSaverWall07On.sideVisible=0: PowerSaverWall07On.collidable=0

	PowerSaverRubberOff.visible=1:	PowerSaverRubberOff.collidable=1
	PowerSaverWall01Off.visible=1:	PowerSaverWall01Off.sideVisible=1: PowerSaverWall01Off.collidable=1
	PowerSaverWall02Off.visible=1:	PowerSaverWall02Off.sideVisible=1: PowerSaverWall02Off.collidable=1
	PowerSaverWall03Off.visible=1:	PowerSaverWall03Off.sideVisible=1: PowerSaverWall03Off.collidable=1
	PowerSaverWall04Off.visible=1:	PowerSaverWall04Off.sideVisible=1: PowerSaverWall04Off.collidable=1
	PowerSaverWall05Off.visible=1:	PowerSaverWall05Off.sideVisible=1: PowerSaverWall05Off.collidable=1
	PowerSaverWall06Off.visible=1:	PowerSaverWall06Off.sideVisible=1: PowerSaverWall06Off.collidable=1
	PowerSaverWall07Off.visible=1:	PowerSaverWall07Off.sideVisible=1: PowerSaverWall07Off.collidable=1

End Sub


Sub RightOutlane_Hit:Controller.Switch(1)=1:End Sub '			switch 1
Sub RightOutlane_unHit:Controller.Switch(1)=0:End Sub'			switch 1
Sub LeftOutlane_Hit:Controller.Switch(2)=1:End Sub' 			switch 2
Sub LeftOutlane_unHit:Controller.Switch(2)=0:End Sub' 			switch 2

Sub Trigger001_Hit:vpmTimer.PulseSw 3:End Sub '				switch 3 exit bottom lane
Sub Trigger002_Hit:vpmTimer.PulseSw 4:End Sub ' 			switch 4 AntiGrav entrance

Sub Trigger9_Hit:vpmTimer.PulseSw 3:End Sub '				switch 3 exit top lane

Sub Drain_Hit:Controller.Switch(8)=1:End Sub '					switch 8 ' Outhole

Sub Trigger6_Hit:Controller.Switch(12)=1:End Sub '				switch 12 'Bubble lane
Sub Trigger6_unHit:Controller.Switch(12)=0:End Sub'				switch 12 'Bubble lane
Sub Trigger7_Hit:Controller.Switch(13)=1:End Sub '				switch 13 'Bubble lane
Sub Trigger7_unHit:Controller.Switch(13)=0:End Sub'				switch 13 'Bubble lane

															'	switch 16 not used

Sub T17_Hit:vpmTimer.PulseSw 17:StandupTargetHit:T17.IsDropped=1:T17a.IsDropped=0:T17.TimerEnabled=1:End Sub
Sub T17_Timer:T17a.IsDropped=1:T17.IsDropped=0:T17.TimerEnabled=0:End Sub
Sub T18_Hit:vpmTimer.PulseSw 18:StandupTargetHit:T18.IsDropped=1:T18a.IsDropped=0:T18.TimerEnabled=1:End Sub
Sub T18_Timer:T18a.IsDropped=1:T18.IsDropped=0:T18.TimerEnabled=0:End Sub
Sub T19_Hit:vpmTimer.PulseSw 19:StandupTargetHit:T19.IsDropped=1:T19a.IsDropped=0:T19.TimerEnabled=1:End Sub
Sub T19_Timer:T19a.IsDropped=1:T19.IsDropped=0:T19.TimerEnabled=0:End Sub
Sub T20_Hit:vpmTimer.PulseSw 20:StandupTargetHit:T20.IsDropped=1:T20a.IsDropped=0:T20.TimerEnabled=1:End Sub
Sub T20_Timer:T20a.IsDropped=1:T20.IsDropped=0:T20.TimerEnabled=0:End Sub
Sub T21_Hit:vpmTimer.PulseSw 21:StandupTargetHit:T21.IsDropped=1:T21a.IsDropped=0:T21.TimerEnabled=1:End Sub
Sub T21_Timer:T21a.IsDropped=1:T21.IsDropped=0:T21.TimerEnabled=0:End Sub
Sub T22_Hit:vpmTimer.PulseSw 22:StandupTargetHit:T22.IsDropped=1:T22a.IsDropped=0:T22.TimerEnabled=1:End Sub
Sub T22_Timer:T22a.IsDropped=1:T22.IsDropped=0:T22.TimerEnabled=0:End Sub

															'	switch 23 not used
															'	switch 24 not used

Sub LeftInlane_Hit:Controller.Switch(25)=1:End Sub' 			switch 25
Sub LeftInlane_unHit:Controller.Switch(25)=0:End Sub' 			switch 25
Sub RightInlane_Hit:Controller.Switch(25)=1:End Sub' 			switch 25
Sub RightInlane_unHit:Controller.Switch(25)=0:End Sub' 			switch 25

Sub Bumper1_Hit:vpmTimer.PulseSw 26:bump1 = 1:Me.TimerEnabled = 1:End Sub

Sub Bumper2_Hit:vpmTimer.PulseSw 27:bump2 = 1:Me.TimerEnabled = 1:End Sub

Sub Bumper3_Hit:vpmTimer.PulseSw 28:bump3 = 1:Me.TimerEnabled = 1:End Sub


Sub LeftSlingshot_Slingshot:vpmTimer.PulseSw(29):End Sub'		switch 29
Sub RightSlingshot_Slingshot:vpmTimer.PulseSw(30):End Sub'		switch 30

															'	switch 31 not used

Sub Kicker1_Hit:bsRSaucer.AddBall Me:End Sub				'   switch 32
'Dim KickerBall
'Sub Kicker1_unhit
'	set KickerBall = activeball
'	KickBall KickerBall, 262, 15, 10, 30
'End Sub
															'	switch 33  trough switch
															'	switch 34  trough switch
															'	switch 35  trough switch
															'	switch 36  trough switch
															'	switch 37  trough switch

Sub Trigger1_Hit:Controller.Switch(38)=1:End Sub 	'	switch 38
Sub Trigger1_unHit:Controller.Switch(38)=0:End Sub	'	switch 38
Sub Trigger2_Hit:Controller.Switch(39)=1:End Sub 	'	switch 39
Sub Trigger2_unHit:Controller.Switch(39)=0:End Sub	'	switch 39
Sub Trigger3_Hit:Controller.Switch(40)=1:End Sub 	'	switch 40
Sub Trigger3_unHit:Controller.Switch(40)=0:End Sub	'	switch 40

Sub SW41_Hit:Controller.Switch(41)=1:End Sub 		'	switch 41 brain rollover left
Sub SW41_unHit:Controller.Switch(41)=0:End Sub		'	switch 41 brain rollover left
Sub SW42_Hit:Controller.Switch(42)=1:End Sub 		'	switch 42 brain rollover right
Sub SW42_unHit:Controller.Switch(42)=0:End Sub		'	switch 42 brain rollover right

Sub SW45_Hit:vpmTimer.PulseSw(45):End Sub			' 	switch 45

Sub Trigger5_Hit:vpmTimer.PulseSw 46:End Sub
Sub Trigger8_Hit:vpmTimer.PulseSw 47:End Sub
Sub Trigger10_Hit:vpmTimer.PulseSw 48:End Sub


' Atom Smasher
Sub Trigger14_Hit
	vpmTimer.PulseSw 44
 	LockBalls=LockBalls+1
End Sub

' Atom Smasher Exit
Sub SpeedTrigger_Hit
	vpmTimer.PulseSw 43
 	If LockBalls>0 Then	LockBalls=LockBalls-1
End Sub


'Antigravity Hole
Dim MyBall7,MyBallY7
Sub Trigger010_Hit
	Set MyBall7=ActiveBall
	MyBallY7=MyBall7.VelY
	If MyBallY7>15 Then
		Kicker010.Enabled = False
	Else
		Kicker010.Enabled = True
		MyBall7.VelY = 0
		MyBall7.VelX = 0
	End If
End Sub

Sub Trigger011_Hit
	Kicker011.Enabled = False
End Sub

'Atom Smasher
Dim LockBalls,MyBall6,MyBallY6
LockBalls=0
Sub Trigger20_Hit
	MyBallY6=ActiveBall.VelY
	If AtomSmasherIsOpen = True And LockBalls>0 Then 
		LockBalls=LockBalls-1
	Else
		Select Case LockBalls
				Case 1:LK1.Kick 0,-MyBallY6      '20
				Case 2:LK2.Kick 0,-MyBallY6      '19
				Case 3:LK3.Kick 0,-MyBallY6      '18
				Case 4:LK4.Kick 0,-MyBallY6      '17
		End Select
	End If
End Sub

Dim MYBLY
MYBLY=0

Sub LK5_Hit
MYBLY=ActiveBall.VelY
If MYBLY>0 And LockBalls<5 Then
LK5.Kick 180,MYBLY
Else
If LockBalls<1 Then
LK5.Kick 180,MYBLY+1
Exit Sub
End If
If LockBalls<5 Then LK5.Kick 180,MYBLY-2
End If
End Sub

Sub LK4_Hit
MYBLY=ActiveBall.VelY
If MYBLY>0 And LockBalls<4 Then
LK4.Kick 180,MYBLY
Else
If LockBalls<1 Then
LK4.Kick 180,MYBLY+1
Exit Sub
End If
If LockBalls<4 Then LK4.Kick 180,MYBLY-2
End If
End Sub

Sub LK3_Hit
MYBLY=ActiveBall.VelY
If MYBLY>0 And LockBalls<3 Then
LK3.Kick 180,MYBLY
Else
If LockBalls<1 Then
LK3.Kick 180,MYBLY+1
Exit Sub
End If
If LockBalls<3 Then LK3.Kick 180,MYBLY-2
End If
End Sub

Sub LK2_Hit
MYBLY=ActiveBall.VelY
If MYBLY>0 And LockBalls<2 Then
LK2.Kick 180,MYBLY
Else
If LockBalls<1 Then
LK2.Kick 180,MYBLY+1
Exit Sub
End If
If LockBalls<2 Then LK2.Kick 180,MYBLY-2
End If
End Sub

Sub LK1_Hit
MYBLY=ActiveBall.VelY
If BallStop2.IsDropped Then
 	LK1.Kick 180,MYBLY
 End If
End Sub

Dim AtomSmasherIsOpen

Sub LowBallStop(Enabled)
	If Enabled Then
		BallStop.IsDropped=1
		BallStop2.IsDropped=1
		AtomSmasherIsOpen = True
		LK1.Kick 180,1
		LK2.Kick 180,1
		LK3.Kick 180,1
		LK4.Kick 180,1
		LK5.Kick 180,1
	End If
End Sub

Sub RaiseBallStop(Enabled)
	If Enabled Then
		BallStop.IsDropped=0
		BallStop2.IsDropped=0
		AtomSmasherIsOpen = False
	End If
End Sub


'*** PI returns the value for PI
Function PI()
	PI = 4*Atn(1)
End Function

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = PI * (kangle - 90) / 180

	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub


' *********************************************************************
' digital display
' *********************************************************************

Dim Digits(28)
Digits(0)	= Array(a00,a01,a02,a03,a04,a05,a06,a07,a08)
Digits(1)	= Array(a10,a11,a12,a13,a14,a15,a16,a17,a18)
Digits(2)	= Array(a20,a21,a22,a23,a24,a25,a26,a27,a28)
Digits(3)	= Array(a30,a31,a32,a33,a34,a35,a36,a37,a38)
Digits(4)	= Array(a40,a41,a42,a43,a44,a45,a46,a47,a48)
Digits(5)	= Array(a50,a51,a52,a53,a54,a55,a56,a57,a58)
Digits(6)	= Array(a60,a61,a62,a63,a64,a65,a66,a67,a68)

Digits(7)	= Array(b00,b01,b02,b03,b04,b05,b06,b07,b08)
Digits(8)	= Array(b10,b11,b12,b13,b14,b15,b16,b17,b18)
Digits(9)	= Array(b20,b21,b22,b23,b24,b25,b26,b27,b28)
Digits(10)	= Array(b30,b31,b32,b33,b34,b35,b36,b37,b38)
Digits(11)	= Array(b40,b41,b42,b43,b44,b45,b46,b47,b48)
Digits(12)	= Array(b50,b51,b52,b53,b54,b55,b56,b57,b58)
Digits(13)	= Array(b60,b61,b62,b63,b64,b65,b66,b67,b68)

Digits(14)	= Array(c00,c01,c02,c03,c04,c05,c06,c07,c08)
Digits(15)	= Array(c10,c11,c12,c13,c14,c15,c16,c17,c18)
Digits(16)	= Array(c20,c21,c22,c23,c24,c25,c26,c27,c28)
Digits(17)	= Array(c30,c31,c32,c33,c34,c35,c36,c37,c38)
Digits(18)	= Array(c40,c41,c42,c43,c44,c45,c46,c47,c48)
Digits(19)	= Array(c50,c51,c52,c53,c54,c55,c56,c57,c58)
Digits(20)	= Array(c60,c61,c62,c63,c64,c65,c66,c67,c68)

Digits(21)	= Array(d00,d01,d02,d03,d04,d05,d06,d07,d08)
Digits(22)	= Array(d10,d11,d12,d13,d14,d15,d16,d17,d18)
Digits(23)	= Array(d20,d21,d22,d23,d24,d25,d26,d27,d28)
Digits(24)	= Array(d30,d31,d32,d33,d34,d35,d36,d37,d38)
Digits(25)	= Array(d40,d41,d42,d43,d44,d45,d46,d47,d48)
Digits(26)	= Array(d50,d51,d52,d53,d54,d55,d56,d57,d58)
Digits(27)	= Array(d60,d61,d62,d63,d64,d65,d66,d67,d68)

Sub DisplayTimer_Timer()
    Dim chgLED, ii, num, chg, stat, obj
	chgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(chgLED) Then
		If DesktopMode Then
			For ii = 0 To UBound(chgLED)
				num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
				if (num < 32) then
					For Each obj In Digits(num)
						If chg And 1 Then obj.State = stat And 1
						chg = chg\2 : stat = stat\2
					Next
				End If
			Next
		End If
    End If
End Sub
 

Sub GILightControl (enabled)
    If enabled Then
        GiON
    Else
        GiOFF
    End If
End Sub

Sub GiON
	dim xx
    For each xx in GILights
        xx.State = LightStateOn
    Next
End Sub

Sub GiOFF
	dim xx
    For each xx in GILights
        xx.State = LightStateOff
    Next
End Sub


Sub SetDynamicLampIntensity
	dim xx
	For each xx in AllLights
	If xx.name <> "FBackWallAtom" and xx.name <> "F90" and xx.name <> "F75" and xx.name <> "F59" Then
		xx.intensity = (xx.intensity + 1) * DynamicLampIntensity 
	End If
	Next
End Sub


Sub SetDynamicGILightIntensity
	dim xx
	For each xx in GILights
	'xx.state = LightStateOn
	xx.intensity = xx.intensity * DynamicGIIntensity * 0.7
	Next
End Sub


' Flasher routine controller by Lights
Sub LSampleTimer_Timer()
	'FBackWallAtom.visible = LBackWallAtom.state
	F59.visible = (L59.state = LightStateOn)
	F75.visible = (L75.state = LightStateOn) 
	F90.visible = (L90.state = LightStateOn)

	F45.visible = (L45.state = LightStateOn)
	F14.visible = (L14.state = LightStateOn)
	F93.visible = (L93.state = LightStateOn)
	F13.visible = (L13.state = LightStateOn)
	F61.visible = (L61.state = LightStateOn)
	F28.visible = (L28.state = LightStateOn)
	F77.visible = (L77.state = LightStateOn)

	Bumper1b.state = Bumper1a.state
	Bumper2b.state = Bumper2a.state
	Bumper3b.state = Bumper3a.state

End Sub


'******************************************************
'			STEPS 2-4 (FLIPPER POLARITY SETUP
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

'InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		x.TimeDelay = 44
	Next

	'"Polarity" Profile
	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.368, -4
	AddPt "Polarity", 2, 0.451, -3.7
	AddPt "Polarity", 3, 0.493, -3.88
	AddPt "Polarity", 4, 0.65, -2.3
	AddPt "Polarity", 5, 0.71, -2
	AddPt "Polarity", 6, 0.785,-1.8
	AddPt "Polarity", 7, 1.18, -1
	AddPt "Polarity", 8, 1.2, 0


	'"Velocity" Profile
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

	LF.Object = LeftFlipper
	LF.EndPoint = EndPointLp
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub



'Trigger Hit - .AddBall activeball
'Trigger UnHit - .PolarityCorrect activeball

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub


'******************************************************
'		FLIPPER CORRECTION SUPPORTING FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

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
	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
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
	Public Property Let EndPoint(aInput) : if IsObject(aInput) then FlipperEnd = aInput.x else FlipperEnd = aInput : end if : End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property

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
				if DebugOn then StickL.visible = True : StickL.x = balldata(x).x		'debug TODO
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
		if abs(Flipper.currentAngle - Flipper.EndAngle) < 30 Then
			PartialFlipCoef = 0
		End If
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
			dim teststr : teststr = "Cutoff"
			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
			if tmp < 0.1 then 'if real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
				if DebugOn then TestStr = "real pos < 0.1 ( " & round(tmp,2) & ")" : tbpl.text = Teststr
				'RemoveBall aBall
				'Exit Sub
			end if

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				if DebugOn then teststr = "y velocity: " & round(aBall.vely, 3) & "exit sub" : tbpl.text = teststr
				RemoveBall aBall
				exit Sub
			end if
			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					'TB.TEXT = balldata(x).id & " " & BALLDATA(X).X & VBNEWLINE & FLIPPERSTART & " " & FLIPPEREND
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				if DebugOn then set tmp = new spoofball : tmp.data = aBall : End If
				if IsEmpty(BallData(idx).id) and aBall.VelY < -12 then 'if tip hit with no collected data, do vel correction anyway
					if PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 then 'adjust plz
						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
						if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
						if Enabled then aBall.Velx = aBall.Velx*VelCoef'VelCoef
						if Enabled then aBall.Vely = aBall.Vely*VelCoef'VelCoef
						if DebugOn then teststr = "tip protection" & vbnewline & "velcoef: " & round(velcoef,3) & vbnewline & round(PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1),3) & vbnewline
						'debug.print teststr
					end if
				Else
		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
					if Enabled then aBall.Velx = aBall.Velx*VelCoef
					if Enabled then aBall.Vely = aBall.Vely*VelCoef
				end if
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			'debug
			if DebugOn then
				TestStr = teststr & "%pos:" & round(BallPos,2)
				if IsEmpty(PolarityOut(0) ) then
					teststr = teststr & vbnewline & "(Polarity Disabled)" & vbnewline
				else
					teststr = teststr & "+" & round(1 *(AddX*ycoef*PartialFlipcoef),3)
					if BallPos >= PolarityOut(uBound(PolarityOut) ) then teststr = teststr & "(MAX)" & vbnewline else teststr = teststr & vbnewline end if
					if Ycoef < 1 then teststr = teststr &  "ycoef: " & ycoef & vbnewline
					if PartialFlipcoef < 1 then teststr = teststr & "PartialFlipcoef: " & round(PartialFlipcoef,4) & vbnewline
				end if

				teststr = teststr & vbnewline & "Vel: " & round(BallSpeed(tmp),2) & " -> " & round(ballspeed(aBall),2) & vbnewline
				teststr = teststr & "%" & round(ballspeed(aBall) / BallSpeed(tmp),2)
				tbpl.text = TestSTR
			end if
		Else
			'if DebugOn then tbpl.text = "td" & timedelay
		End If
		RemoveBall aBall
	End Sub
End Class


'******************************************************
'		HELPER FUNCTIONS
'******************************************************


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

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function RndNum(min, max)
    RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.y * 2 / table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.x * 2 / table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = ball.x * 2 / table1.width-1
  If tmp > 0 Then
    Pan = Csng(tmp ^10)
  Else
    Pan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
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



' *********************************************************************
' some special physics behaviour
' *********************************************************************
' target or rubber post is hit so let the ball jump a bit
Sub DropTargetHit()
	DropTargetSound
	TargetHit
End Sub

Sub StandupTargetHit()
	StandUpTargetSound
	TargetHit
End Sub

Sub TargetHit()
    ActiveBall.VelZ = ActiveBall.VelZ * (0.5 + (Rnd()*LetTheBallJump + Rnd()*LetTheBallJump + 1) / 6)
End Sub

Sub RubberPostHit()
	ActiveBall.VelZ = ActiveBall.VelZ * (0.9 + (Rnd()*(LetTheBallJump-1) + Rnd()*(LetTheBallJump-1) + 1) / 6)
End Sub

Sub RubberRingHit()
	ActiveBall.VelZ = ActiveBall.VelZ * (0.8 + (Rnd()*(LetTheBallJump-1) + 1) / 6)
End Sub


' *********************************************************************
' more realtime sounds
' *********************************************************************
' ball collision sound
Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

' rubber hit sounds
Sub RubberWalls_Hit(idx)
    PlaySoundAtBallVol "fx_rubber_hit_" & Int(Rnd*3)+1, 10
	RubberRingHit
End Sub

Sub RubberPosts_Hit(idx)
    PlaySoundAtBallVol "fx_rubber_hit_" & Int(Rnd*3)+1, 10
	RubberPostHit
End Sub

' metal hit sounds
Sub MetalWalls_Hit(idx)
	'PlaySoundAtBallAbsVol "fx_metalhit" & Int(Rnd*3), Minimum(Vol(ActiveBall),0.5)
End Sub

' plastics hit sounds
Sub Plastics_Hit(idx)
	'PlaySoundAtBallAbsVol "fx_ball_hitting_plastic", Minimum(Vol(ActiveBall),0.5)
End Sub

' gates sound
Sub Gates_Hit(idx)
	GateSound
End Sub

' sound at ramp rubber at the diverter
Sub RampRubber_Hit()
	PlaySoundAtBallVol "fx_flip_hit_" & Int(Rnd*3)+1, 10
End Sub


'
' *********************************************************************
' sound stuff
' *********************************************************************
Sub RollOverSound()
	PlaySoundAtVolPitch SoundFX("fx_rollover",DOFContactors), ActiveBall, 0.02, .25
End Sub  
Sub DropTargetSound()
	PlaySoundAtVolPitch SoundFX("fx_droptarget",DOFTargets), ActiveBall, 2, .25
End Sub
Sub StandUpTargetSound()
	PlaySoundAtVolPitch SoundFX("fx_target",DOFTargets), ActiveBall, 2, .25
End Sub
Sub GateSound()
	PlaySoundAtVolPitch SoundFX("fx_gate",DOFContactors), ActiveBall, 0.02, .25
End Sub


' *********************************************************************
' Supporting Surround Sound Feedback (SSF) functions
' *********************************************************************
' set position as table object (Use object or light but NOT wall) and Vol to 1
Sub PlaySoundAt(sound, tableobj)
	PlaySound sound, 1, 1, AudioPan(tableobj), 0, 0, 0, 1, AudioFade(tableobj)
End Sub
' set position as table object and Vol manually.
Sub PlaySoundAtVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, AudioPan(tableobj), 0, 0, 0, 1, AudioFade(tableobj)
End Sub
' set position as table object and Vol + RndPitch manually 
Sub PlaySoundAtVolPitch(sound, tableobj, Vol, RndPitch)
	PlaySound sound, 1, Vol, AudioPan(tableobj), RndPitch, 0, 0, 1, AudioFade(tableobj)
End Sub

' set all as per ball position & speed.
Sub PlaySoundAtBall(sound)
	PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub
' set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3
Sub PlaySoundAtBallVol(sound, VolMult)
	PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub
Sub PlaySoundAtBallAbsVol(sound, VolMult)
	PlaySound sound, 0, VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

' play a looping sound at a location with volume
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

' requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	Dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
	PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' set position as bumperX and Vol manually. Allows rapid repetition/overlaying sound
Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
	PlaySound sound, 1, Vol, AudioPan(tableobj), 0, 0, 1, 1, AudioFade(tableobj)
End Sub


'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc


'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 5 ' total number of balls
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

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

'***Ball Drop Sounds***

		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			PlaySound "ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If

    Next
End Sub


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER COVERS AND SHADOWS
'*****************************************

Dim BallShadow : BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5)

sub GraphicsTimer_Timer()
	Dim ii

	' maybe show ball shadows
	If ShowBallShadow <> 0 Then
		Dim BOT
		BOT = GetBalls
		' hide shadow of deleted balls
		If UBound(BOT) < tnob - 1 Then
			For ii = UBound(BOT) + 1 To tnob - 1
				If BallShadow(ii).Visible Then BallShadow(ii).Visible = False
			Next
		End If
		' render the shadow for each ball
		For ii = 0 to UBound(BOT)
			If BOT(ii).X < Science.Width/2 Then
				BallShadow(ii).X = ((BOT(ii).X) - (Ballsize/6) + ((BOT(ii).X - (Science.Width/2))/7)) + 6
			Else
				BallShadow(ii).X = ((BOT(ii).X) + (Ballsize/6) + ((BOT(ii).X - (Science.Width/2))/7)) - 6
			End If
			BallShadow(ii).Y = BOT(ii).Y + 12
'			If TroughBalls <= 0 Then
				BallShadow(ii).Visible = True '(BOT(ii).Z > 20)
'			Else
'				BallShadow(ii).Visible = (BOT(ii).Z > 20 And ii >= TroughBalls And ii <> lockedBallID)
'			End If
		Next
	End If

	LeftFlipperShadow.RotY = LeftFlipper.CurrentAngle - 90
	RightFlipperShadow.RotY = RightFlipper.CurrentAngle - 90
	RightFlipper1Shadow.RotY = RightFlipper1.CurrentAngle - 90

	LFlip.RotY = LeftFlipper.CurrentAngle + 240
	RFlip.RotY = RightFlipper.CurrentAngle + 120
	RFlip1.RotY = RightFlipper1.CurrentAngle + 52

End Sub



Function GetBallID(actBall)
	Dim b, BOT, ret
	ret = -1
	BOT = GetBalls
	For b = 0 to UBound(BOT)
		If actBall Is BOT(b) Then
			ret = b : Exit For
		End If
	Next
	GetBallId = ret
End Function

''''''''''''''''''''''''
''' Test Kicker
''''''''''''''''''''''''
Sub TestKickerIn_Hit
	TestKickerIn.DestroyBall
	TestKickerOut.CreateBall
	'TestKickerOut.Kick Angle,velocity 
	TestKickerOut.Kick 300, 45
End Sub
Sub TestKickerLoop_Hit
	TestKickerLoop.DestroyBall
	TestKickerOut.CreateBall
	'TestKickerOut.Kick Angle,velocity 
	TestKickerOut.Kick 300, 40
End Sub
Sub TestKickerOut_Hit
	'TestKickerOut.Kick Angle,velocity 
	TestKickerOut.Kick 300, 40
End Sub

'''''''''''''''''''''''''
'******************
' Rom sound ON wwhen close table
'******************
Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub