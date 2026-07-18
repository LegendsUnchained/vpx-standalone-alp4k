'	---------------------------------------------
'	TIME 2000
'	---------------------------------------------
'	Visual Pinball table ED-16
'	Original table by Atari Inc., June 1977
'	VP adaptation by Dave Sanders, May 2002
'	---------------------------------------------

Option Explicit

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01200000", "ATARI1b.VBS", 3.1

'Sub LoadVPM(VPMver, VBSfile, VBSver)
	'On Error Resume Next
		'If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
		'ExecuteGlobal GetTextFile(VBSfile)
	'	If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description : Err.Clear
	'	Set Controller = CreateObject("VPinMAME.Controller")
		'If Err Then MsgBox "Can't Load VPinMAME." & vbNewLine & Err.Description
	'	If VPMver>"" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required." : Err.Clear
	'	If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
'End Sub

Const cGameName     = "time2000"   ' PinMAME short name
Const cCredits      = "Time 2000 Table by Eala Dubh Sidhe, VPinMAMEd by Destruk, updated to VP10 by MaX"
Const UseSolenoids  = 1
Const UseLamps      = 1
Const UseGI         = 0

' Standard Sounds
Const SSolenoidOn   = "solon"
Const SSolenoidOff  = "soloff"
Const SFlipperOn    = "FlipperUptime2000"
Const SFlipperOff   = "FlipperDowntime2000"
Const SCoin         = "coin3"

'Solenoid Definitions
Const sGate=3'1
Const sRightJet=16'2
Const sLeftJet=4'3
Const sRightKicker=14'4
Const sCenterKicker=8'5
'Const sRightFlipper=1'6
'Const sRightCFlipper=12'7
'Const sLeftCFlipper=5'8
'Const sLeftFlipper=11'9
Const sRSling=13'10
Const sLSling=9'11
Const sOutHole=10'12
Const sTopDrop=2'13
Const sBottomDrop=6'14
Const sKnocker=7

'If ShowDT=false Then
'dim objekt : for each objekt in backdropobjs : objekt.visible = 0 : next
'End If

'Solenoid Callbacks
SolCallback(sGate)			= "SolRightGate"
SolCallback(sRightJet)		= "vpmSolSound ""bumpertime2000"","
SolCallback(sLeftJet)		= "vpmSolSound ""bumpertime2000"","
SolCallback(sRightKicker)	= "bsRightSaucer.SolOut"
SolCallback(sCenterKicker)	= "bsCenterSaucer.SolOut"
SolCallback(sRSling)		= "vpmSolSound ""sling"","
SolCallback(sLSling)		= "vpmSolSound ""sling"","
SolCallback(sOutHole)		= "bsTrough.SolOut"
SolCallback(sTopDrop)		= "SolTopDrop"
SolCallback(sBottomDrop)	= "SolBottomDrop"
SolCallback(sKnocker)		= "vpmSolSound ""knocker"","

Sub SolTopDrop(enabled)
	if enabled then
		dtTop.DropSol_On
		if droptarget2pos=48 then rdt2.enabled=1
	end if
End Sub
Sub SolBottomDrop(enabled)
	if enabled then
		dtBottom.DropSol_On
		if droptarget1pos=48 then rdt1.enabled=1
	end if
End Sub

Dim droptarget1pos,droptarget2pos
droptarget1pos=0:droptarget2pos=0

Sub drop1_timer()
	droptarget1pos=droptarget1pos+4
	droptarget1b.rotandtra5=0-droptarget1pos
	if droptarget1pos=48 then me.timerenabled=0
End Sub
Sub rdt1_timer()
	droptarget1pos=droptarget1pos-12
	droptarget1b.rotandtra5=0-droptarget1pos
	if droptarget1pos=0 then me.enabled=0
End Sub

Sub drop2_timer()
	droptarget2pos=droptarget2pos+4
	droptarget2b.rotandtra5=0-droptarget2pos
	if droptarget2pos=48 then me.timerenabled=0
End Sub
Sub rdt2_timer()
	droptarget2pos=droptarget2pos-12
	droptarget2b.rotandtra5=0-droptarget2pos
	if droptarget2pos=0 then me.enabled=0
End Sub

'Smoothed Solenoid routines for gates
Sub SolRightGate(Enabled)
	If Not RightDelay.Enabled Then vpmSolDiverter RightGate,True,True
	RightDelay.Enabled=0
	RightDelay.Enabled=1
End Sub

Sub RightDelay_Timer:RightDelay.Enabled=0:vpmSolDiverter RightGate,True,False:End Sub
'End of solenoid smoothing for gates 


'*****GI Lights On
dim xx
For each xx in GI:xx.State = 1: Next 


Dim bsTrough,bsRightSaucer,bsCenterSaucer,dtTop,dtBottom

Sub Table1_Init
    On Error Resume Next
	With Controller
		.GameName=cGameName
		If Err Then MsgBox "Can't start Game" & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine=cCredits
		.HandleMechanics=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden=1
		.SetDisplayPosition 1200,135,GetPlayerHWnd
		.Run
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval
    PinMAMETimer.Enabled=True
    ' Nudging
	vpmNudge.TiltSwitch=4
	vpmNudge.Sensitivity=5
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,LeftSlingshot,RightSlingshot)

	Set bsTrough=New cvpmBallStack
		bsTrough.InitSw 0,38,0,0,0,0,0,0
		bsTrough.InitKick BallRelease,70,10
		bsTrough.InitExitSnd "Ballreleasetime2000","solon"
		bsTrough.Balls=1
		
	Set bsRightSaucer=New cvpmBallStack
		bsRightSaucer.InitSaucer PMHole,24,195,4
		bsRightSaucer.InitExitSnd "popper","solon"
		bsRightSaucer.KickAngleVar=5

	Set bsCenterSaucer=New cvpmBallStack
		bsCenterSaucer.InitSaucer AMHole,23,185,4
		bsCenterSaucer.InitExitSnd "popper","solon"
		bsCenterSaucer.KickAngleVar=5
			
	Set dtTop=New cvpmDropTarget
		dtTop.InitDrop Drop2,40
		dtTop.InitSnd "droptargetdown","bankresetA 100P"

	Set dtBottom=New cvpmDropTarget
		dtBottom.InitDrop Drop1,41
		dtBottom.InitSnd "droptargetdown","bankresetA 100P"
End Sub

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyCode=LeftFlipperKey And MacTilt=0 And LeftF=0 Then
		Controller.Switch(20)=1
		LeftFlipper.RotateToEnd
		LeftFlipperTop.RotateToEnd
		PlaySound"flipperuptime2000"
		LeftF=1
		Exit Sub
	End If
	If KeyCode=RightFlipperKey And MacTilt=0 And RightF=0 Then
		Controller.Switch(19)=1
		RightFlipper.RotateToEnd
		RightFlipperTop.RotateToEnd
		PlaySound"flipperuptime2000"
		RightF=1
		Exit Sub
	End If
	If vpmKeyDown(KeyCode) Then Exit Sub
	If KeyCode=PlungerKey Then Plunger.PullBack:PlaySound "plungerpull",0,1,0.25,0.25
End	Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyCode=LeftFlipperKey And LeftF=1 Then
		Controller.Switch(20)=0
		LeftFlipper.RotateToStart
		LeftFlipperTop.RotateToStart
		PlaySound"flipperdowntime2000"
		LeftF=0
		Exit Sub
	End If
	If KeyCode=RightFlipperKey And RightF=1 Then
		Controller.Switch(19)=0
		RightFlipper.RotateToStart
		RightFlipperTop.RotateToStart
		PlaySound"flipperdowntime2000"
		RightF=0
		Exit Sub
	End If
	If vpmKeyUp(KeyCode) Then Exit Sub
	If KeyCode=PlungerKey Then PLunger.fire:PlaySound "plunger",0,1,0.25,0.25
End Sub

Sub Table1_Paused:Controller.Pause=True:End Sub 
Sub Table1_UnPaused:Controller.Pause=False:End Sub

'1 Coin1
'2 Coin2
'3 Start Button
'4 Tilt
Sub Bumper1_Hit:vpmTimer.PulseSw 21:End Sub					'21 OK
Sub Bumper2_Hit:vpmTimer.PulseSw 22:End Sub					'22 OK
Sub AMHole_Hit:bsCenterSaucer.AddBall 0:End Sub				'23 OK
Sub PMHole_Hit:bsRightSaucer.AddBall 0:End Sub				'24 OK
Sub LeftSlingshot_Slingshot:vpmTimer.PulseSw 33:End Sub		'33 OK
Sub RightSlingshot_Slingshot:vpmTimer.PulseSw 34:End Sub	'34 OK
Sub Point1_Hit:vpmTimer.PulseSw 35:End Sub					'35 OK
Sub Point2_Hit:vpmTimer.PulseSw 35:End Sub
Sub Point3_Hit:vpmTimer.PulseSw 35:End Sub
Sub Point4_Hit:vpmTimer.PulseSw 35:End Sub
Sub Point5_Hit:vpmTimer.PulseSw 36:End Sub					'36 OK
Sub Point6_Hit:vpmTimer.PulseSw 37:End Sub					'37 OK
Sub Drain_Hit:PlaySound"Draintime2000":bsTrough.AddBall Me:End Sub	'38 OK
Sub Sling3_hit:playsound"sling":end Sub


Sub AM1_Hit
vpmTimer.PulseSw 39
	AM1p.transx = -10
	Me.TimerEnabled = 1
End Sub

Sub AM1_Timer
	AM1p.transx = 0
	Me.TimerEnabled = 0
End Sub

Sub AM2_Hit
vpmTimer.PulseSw 39
	AM2p.transx = -10
	Me.TimerEnabled = 1
End Sub

Sub AM2_Timer
	AM2p.transx = 0
	Me.TimerEnabled = 0
End Sub

Sub Drop2_Hit:dtTop.Hit 1:me.timerenabled=1:End Sub							'40 OK	May be switched with 41
Sub Drop1_Hit:dtBottom.Hit 1::me.timerenabled=1:End Sub						'41 OK  May be switched with 40

Sub PM1_Hit
	vpmTimer.PulseSw 42
	PM1p.transx = -10
	Me.TimerEnabled = 1
End Sub

Sub PM1_Timer
	PM1p.transx = 0
	Me.TimerEnabled = 0
End Sub

Sub PM2_Hit
	vpmTimer.PulseSw 42
	PM2p.transx = -10
	Me.TimerEnabled = 1
End Sub

Sub PM2_Timer
	PM2p.transx = 0
	Me.TimerEnabled = 0
End Sub

Sub AMPM_Hit
	vpmTimer.PulseSw 43
	AMPMp.transx = -10
	Me.TimerEnabled = 1
End Sub

Sub AMPM_Timer
	AMPMp.transx = 0
	Me.TimerEnabled = 0
End Sub

Sub LeftLane_Hit:Controller.Switch(44)=1:Me.TimerEnabled=0:Me.TimerEnabled=1:LI6.State=0:End Sub'44 OK
Sub LeftLane_Timer:Me.TimerEnabled=0:LI6.State=1:End Sub
Sub LeftLane_unHit:Controller.Switch(44)=0:End Sub
Sub Triple1_Hit:Controller.Switch(45)=1:End Sub				'45 OK
Sub Triple1_unHit:Controller.Switch(45)=0:End Sub
Sub Triple2_Hit:Controller.Switch(46)=1:End Sub				'46 OK
Sub Triple2_unHit:Controller.Switch(46)=0:End Sub
Sub Roll1_Hit:Controller.Switch(47)=1:Me.TimerEnabled=0:Me.TimerEnabled=1:LI5.State=0:End Sub'47 OK
Sub Roll1_Timer:Me.TimerEnabled=0:LI5.State=1:End Sub
Sub Roll1_unHit:Controller.Switch(47)=0:End Sub
Sub Roll2_Hit:Controller.Switch(47)=1:Me.TimerEnabled=0:Me.TimerEnabled=1:LI3.State=0:End Sub
Sub Roll2_Timer:Me.TimerEnabled=0:LI3.State=1:End Sub
Sub Roll2_unHit:Controller.Switch(47)=0:End Sub
Sub Roll3_Hit:Controller.Switch(47)=1:Me.TimerEnabled=0:Me.TimerEnabled=1:LI4.State=0:End Sub
Sub Roll3_Timer:Me.TimerEnabled=0:LI4.State=1:End Sub
Sub Roll3_unHit:Controller.Switch(47)=0:End Sub
Sub LeftOutlane_Hit:Controller.Switch(59)=1:Me.TimerEnabled=0:Me.TimerEnabled=1:LI1.State=0:End Sub'59 OK
Sub LeftOutlane_Timer:Me.TimerEnabled=0:LI1.State=1:End Sub
Sub LeftOutlane_unHit:Controller.Switch(59)=0:End Sub
Sub RightOutlane_Hit:Controller.Switch(60)=1:Me.TimerEnabled=0:Me.TimerEnabled=1:LI2.State=0:End Sub'60 OK
Sub RightOutlane_Timer:Me.TimerEnabled=0:LI2.State=1:End Sub
Sub RightOutlane_unHit:Controller.Switch(60)=0:End Sub

'Missing Match Light

Set Lights(1)=Left1
Set Lights(2)=Left2
Set Lights(3)=Left3
Set Lights(4)=Bonus3X
Set Lights(5)=Left4
Set Lights(6)=Left5
Set Lights(7)=Left6
Set Lights(8)=Light8
Set Lights(9)=Left7
Set Lights(10)=Left8
Set Lights(11)=Left9
Set Lights(12)=Light12
Set Lights(13)=Left10
Set Lights(14)=Left11
Set Lights(15)=Left12
Set Lights(16)=Light16
Set Lights(17)=Right1
Set Lights(18)=Right2
Set Lights(19)=Right3
Set Lights(20)=Light20
Set Lights(21)=Right4
Set Lights(22)=Right5
Set Lights(23)=Right6
Set Lights(24)=Light24
Set Lights(25)=Right7
Set Lights(26)=Right8
Set Lights(27)=Right9
Set Lights(28)=Light28
Set Lights(29)=Right10
Set Lights(30)=Right11
Set Lights(31)=Right12
Set Lights(32)=Light32 
Set Lights(33)=SPLight1
Set Lights(34)=XBLight1
Set Lights(35)=XBLight2
Set Lights(36)=Light36
Set Lights(37)=LeftOn
Set Lights(38)=Bumper1l
Set Lights(39)=SPLight5
Set Lights(40)=Light40
Set Lights(41)=TLight 'Tilt
Set Lights(42)=SPSA
Set Lights(43)=Bonus2X
Set Lights(44)=Light44
Set Lights(45)=Light45
Set Lights(46)=Light46
Set Lights(47)=Light47
Set Lights(48)=Light48
Set Lights(49)=SPLight3
Set Lights(50)=XBLight3
Set Lights(51)=SPLight2
Set Lights(52)=Light52
Set Lights(53)=RightOn
Set Lights(54)=Bumper2l
Set Lights(55)=SPLight4
Set Lights(56)=Light56
Set Lights(57)=P1l'LightGamer1
Set Lights(58)=P2l'LightGamer2
Set Lights(59)=GameOver
Set Lights(60)=Light60
Set Lights(61)=P3l'LightGamer3
Set Lights(62)=P4l'LightGamer4
Set Lights(63)=Light63
Set Lights(64)=Light64
Set Lights(65)=Light65
Set Lights(66)=Light66
Set Lights(67)=Light67
Set Lights(68)=Light68
Set Lights(69)=Light69
Set Lights(70)=Light70
Set Lights(71)=LEAdvance
Set Lights(72)=Light72
Set Lights(73)=Light73
Set Lights(74)=Light74
Set Lights(75)=Light75
Set Lights(76)=Light76
Set Lights(77)=Light77
Set Lights(78)=Light78
Set Lights(79)=Light79
Set Lights(80)=Light80
Set Lights(81)=Light81
Set Lights(82)=Light82
Set Lights(83)=Light83
Set Lights(84)=Light84
Set Lights(85)=Light85
Set Lights(86)=Light86
Set Lights(87)=RIAdvance'also 85
Set Lights(88)=Light88
Set Lights(89)=Light89
Set Lights(90)=Light90
Set Lights(91)=Light91
Set Lights(92)=Light92
Set Lights(93)=Light93
Set Lights(94)=Light94
Set Lights(95)=Light95
Set Lights(96)=Light96
Set Lights(97)=Light97
Set Lights(98)=Light98
Set Lights(99)=Light99
Set Lights(100)=Light100
Set Lights(101)=Light101
Set Lights(102)=Light102
Set Lights(103)=Light103
Set Lights(104)=Light104
Set Lights(105)=Light105
Set Lights(107)=Light107
Set Lights(109)=Light109
Set Lights(110)=Light110
Set Lights(111)=Light111
Set Lights(112)=Light112
Set Lights(113)=Light113
Set Lights(114)=Light114
Set Lights(115)=Light115
Set Lights(116)=Light116
Set Lights(117)=Light117
Set Lights(118)=Light118
Set Lights(119)=Light119
Set Lights(120)=Light120
Set Lights(121)=Light121
Set Lights(122)=Light122
Set Lights(123)=Light123
Set Lights(124)=Light124

Set Lights(125)=Light125
Set Lights(126)=Light126
Set Lights(127)=Light127
Set Lights(128)=Light128

Set Lights(129)=LightS1
Set Lights(130)=LightS2
Set Lights(131)=LightS3
Set Lights(132)=LightS4
Set Lights(133)=Light133
Set Lights(134)=Light134
Set Lights(135)=Light135
Set Lights(136)=Light136
Set Lights(137)=Light137
Set Lights(138)=Light138
Set Lights(139)=Light139
Set Lights(140)=Light140

Dim MacTilt,LeftF,RightF
MacTilt=0:LeftF=0:RightF=0

Set LampCallback=GetRef("UpdateMultipleLamps") 'Tilt Control

'Needs Game Over Light
Sub UpdateMultipleLamps

'	P1l.state=LightGamer1.State
'	P1.IsDropped=LightGamer1.State
'	P2.IsDropped=LightGamer2.State
'	P3.IsDropped=LightGamer3.State
'	P4.IsDropped=LightGamer4.State
	WallTextShootAgain.IsDropped=SPSA.State
	WallTextGameOver.IsDropped=GameOver.State
	MacTilt=TLight.State
	WallTextTilt.IsDropped=MacTilt
'	If LightGamer1.State=0 And LightGamer2.State=0 And LightGamer3.State=0 And LightGamer4.State=0 Then
'	    MacTilt=1
'	End If    
	If GameOver.State=1 Then MacTilt=1
		If MacTilt=1 And LeftF=1 Then
			Controller.Switch(20)=0
			LeftFlipper.RotateToStart
			LeftFlipperTop.RotateToStart
			PlaySound"flipperdowntime2000"
			LeftF=0
		End If
		If MacTilt=1 And RightF=1 Then
			Controller.Switch(19)=0
			RightFlipper.RotateToStart
			RightFlipperTop.RotateToStart
			PlaySound"flipperdowntime2000"
			RightF=0
		End If
		If MacTilt=1 Then
			Bumper1.Force=0
			Bumper2.Force=0
'			LeftSlingshot.SlingshotForce=0
'			RightSlingshot.SlingshotForce=0
		Else
			Bumper1.Force=10
			Bumper2.Force=10
'			LeftSlingshot.SlingshotForce=8
'			RightSlingshot.SlingshotForce=8
		End If
End Sub

'Atari Time 2000
'added by Inkochnito
Sub editDips
	Dim vpmDips:Set vpmDips=New cvpmDips
 	With vpmDips
		.AddForm 700,400,"Time 2000 - DIP switches"
		.AddFrame 0,5,190,"Coins per credit",&H000000C3,Array("1 coin 1 credit",0,"1 coin 2 credits",&H00000002,"1 coin 3 credits",&H00000001,"1 coin 4 credits",&H00000003,"2 coins 1 credits",&H00000080)'SW2-3&SW2-4&SW2-5&SW2-6 (dip 2&1&8&7)
		.AddFrame 0,95,190,"Maximum credits",49152,Array("8 credits",0,"12 credits",32768,"15 credits",&H00004000,"20 credits",49152)'SW1-5&SW1-6 (dip 16&15)
		.AddFrame 0,172,190,"Game option",&H00000100,Array("off",0,"on",&H00000100)'SW1-4 (dip 9)
		.AddFrame 0,220,190,"Balls per game",&H00000008,Array("5 balls",0,"3 balls",&H00000008)'SW2-1 (dip 4)
		.AddFrame 0,268,190,"Game option",&H00001000,Array("off",0,"on",&H00001000)'SW1-8 (dip 13)
		.AddFrame 210,5,190,"Score threshold level",&H000F0000,Array("40K-60K-80K",0,"80K-120K-160K",&H00040000,"110K-160K-210K",&H00070000,"150K-220K-290K",&H000B0000,"190K-280K-370K",&H000F0000)'Rotary (dip 17&18&19&20)
		.AddFrame 210,95,190,"Special award",&H00000030,Array("200,000 points",0,"10,000 points",&H00000010,"extra ball",&H00000020,"replay",&H00000030)'SW2-7&SW2-8 (dip 6&5)
 		.AddFrame 210,172,190,"Game option",&H00000400,Array("off",0,"on",&H00000400)'SW1-2 (dip 11)
 		.AddFrame 210,220,190,"Game option",&H00000200,Array("off",0,"on",&H00000200)'SW1-3 (dip 10)
		.AddFrame 210,268,190,"Game option",&H00002000,Array("off",0,"on",&H00002000)'SW1-7 (dip 14)
		.AddChk 0,320,190,Array("Match feature",&H00000004)'SW2-2 (dip 3)
		.AddChk 210,320,190,Array("Diagnostic mode (must be off)",&H00000800)'SW1-1 (dip 12)
		.AddLabel 50,350,300,20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
End Sub

Set vpmShowDips=GetRef("editDips")
 
Dim Digits(27)
Digits(0)=Array(LX1,LX2,LX3,LX4,LX5,LX6,LX7)
Digits(1)=Array(LX8,LX9,LX10,LX11,LX12,LX13,LX14)
Digits(2)=Array(LX15,LX16,LX17,LX18,LX19,LX20,LX21)
Digits(3)=Array(LX22,LX23,LX24,LX25,LX26,LX27,LX28)
Digits(4)=Array(LX29,LX30,LX31,LX32,LX33,LX34,LX35)
Digits(5)=Array(LX36,LX37,LX38,LX39,LX40,LX41,LX42)
Digits(6)=Array(LX43,LX44,LX45,LX46,LX47,LX48,LX49)
Digits(7)=Array(LX50,LX51,LX52,LX53,LX54,LX55,LX56)
Digits(8)=Array(LX57,LX58,LX59,LX60,LX61,LX62,LX63)
Digits(9)=Array(LX64,LX65,LX66,LX67,LX68,LX69,LX70)
Digits(10)=Array(LX71,LX72,LX73,LX74,LX75,LX76,LX77)
Digits(11)=Array(LX78,LX79,LX80,LX81,LX82,LX83,LX84)
Digits(12)=Array(LX85,LX86,LX87,LX88,LX89,LX90,LX91)
Digits(13)=Array(LX92,LX93,LX94,LX95,LX96,LX97,LX98)
Digits(14)=Array(LX99,LX100,LX101,LX102,LX103,LX104,LX105)
Digits(15)=Array(LX106,LX107,LX108,LX109,LX110,LX111,LX112)
Digits(16)=Array(LX113,LX114,LX115,LX116,LX117,LX118,LX119)
Digits(17)=Array(LX120,LX121,LX122,LX123,LX124,LX125,LX126)
Digits(18)=Array(LX127,LX128,LX129,LX130,LX131,LX132,LX133)
Digits(19)=Array(LX134,LX135,LX136,LX137,LX138,LX139,LX140)
Digits(20)=Array(LX141,LX142,LX143,LX144,LX145,LX146,LX147)
Digits(21)=Array(LX148,LX149,LX150,LX151,LX152,LX153,LX154)
Digits(22)=Array(LX155,LX156,LX157,LX158,LX159,LX160,LX161)
Digits(23)=Array(LX162,LX163,LX164,LX165,LX166,LX167,LX168)
Digits(24)=Array(LX169,LX170,LX171,LX172,LX173,LX174,LX175)
Digits(25)=Array(LX176,LX177,LX178,LX179,LX180,LX181,LX182)
Digits(26)=Array(LX183,LX184,LX185,LX186,LX187,LX188,LX189) 'CREDITS X10
Digits(27)=Array(LX190,LX191,LX192,LX193,LX194,LX195,LX196) 'CREDITS X1

Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLED=Controller.ChangedLEDs(&Hffffffff,&Hffffffff)
	If Not IsEmpty(ChgLED) Then
		For ii=0 To UBound(chgLED)
			num=chgLED(ii,0):chg=chgLED(ii,1):stat=chgLED(ii,2)
			For Each obj In Digits(num)
				If chg And 1 Then obj.State=stat And 1
				chg=chg\2:stat=stat\2
			Next
		Next
	End If
End Sub



' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Vol2(ball1, ball2) ' Calculates the Volume of the sound based on the speed of two balls
    Vol2 = (Vol(ball1) + Vol(ball2) ) / 2
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp> 0 Then
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

'*****************************************
'    JP's VP10 Collision & Rolling Sounds
'*****************************************

Const tnob = 8 ' total number of balls
ReDim rolling(tnob)
ReDim collision(tnob)
Initcollision

Sub Initcollision
    Dim i
    For i = 0 to tnob
        collision(i) = -1
        rolling(i) = False
    Next
End Sub

Sub CollisionTimer_Timer()
    Dim BOT, B, B1, B2, dx, dy, dz, distance, radii
    BOT = GetBalls

    ' rolling
	
	For B = UBound(BOT) +1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
	Next

    If UBound(BOT) = -1 Then Exit Sub

    For B = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next

    'collision

    If UBound(BOT) < 1 Then Exit Sub

    For B1 = 0 to UBound(BOT)
        For B2 = B1 + 1 to UBound(BOT)
            dz = INT(ABS((BOT(b1).z - BOT(b2).z) ) )
            radii = BOT(b1).radius + BOT(b2).radius
			If dz <= radii Then

            dx = INT(ABS((BOT(b1).x - BOT(b2).x) ) )
            dy = INT(ABS((BOT(b1).y - BOT(b2).y) ) )
            distance = INT(SQR(dx ^2 + dy ^2) )

            If distance <= radii AND (collision(b1) = -1 OR collision(b2) = -1) Then
                collision(b1) = b2
                collision(b2) = b1
                PlaySound("fx_collide"), 0, Vol2(BOT(b1), BOT(b2)), Pan(BOT(b1)), 0, Pitch(BOT(b1)), 0, 0
            Else
                If distance > (radii + 10)  Then
                    If collision(b1) = b2 Then collision(b1) = -1
                    If collision(b2) = b1 Then collision(b2) = -1
                End If
            End If
			End If
        Next
    Next
End Sub


'************************************
' What you need to add to your table
'************************************

' a timer called CollisionTimer. With a fast interval, like from 1 to 10
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

' The sound is played using the VOL, PAN and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the PAN function will change the stereo position according
' to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The Double For loop: This is a double cycle used to check the collision between a ball and the other ones.
' If you look at the parameters of both cycles, youll notice they are designed to avoid checking 
' collision twice. For example, I will never check collision between ball 2 and ball 1, 
' because I already checked collision between ball 1 and 2. So, if we have 4 balls, 
' the collision checks will be: ball 1 with 2, 1 with 3, 1 with 4, 2 with 3, 2 with 4 and 3 with 4.

' Sum first the radius of both balls, and if the height between them is higher then do not calculate anything more,
' since the balls are on different heights so they can't collide.

' The next 3 lines calculates distance between xth and yth balls with the Pytagorean theorem,

' The first "If": Checking if the distance between the two balls is less than the sum of the radius of both balls, 
' and both balls are not already colliding.

' Why are we checking if balls are already in collision? 
' Because we do not want the sound repeting when two balls are resting closed to each other.

' Set the collision property of both balls to True, and we assign the number of the ball colliding

' Play the collide sound of your choice using the VOL, PAN and PITCH functions

' Last lines: If the distance between 2 balls is more than the radius of a ball,
' then there is no collision and then set the collision property of the ball to False (-1).


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlippertop_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub leftFlippertop_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub Table1_Exit
Controller.Stop
End Sub
