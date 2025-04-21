'*************************************************************************************************************************************************************
'FISH TALES
'Williams 1992
'version 1.1

'VPX SS recreation by pinball58
'Thanks to the authors(PacDude,Melon,Loaded Weapon,ICPjuggla,Zany)who made this table before for the stuff and ideas that I borrowed from their VP9 tables
'(especially Zany for primitives)
'Thanks to Tom Tower and Ninuzzu for helping me finalize the table
'Thanks to Arngrim for helping me with DOF
'Thanks to VPDev Team for the freaking amazing VPX

'Reel modification by ma1299
'Thanks to agentEighty6 for Updating the reel to make it look like it was actually loading balls
'*************************************************************************************************************************************************************

Option Explicit
Randomize

Const Ballsize= 50
Const BallMass = 1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim DesktopMode:DesktopMode = FishTales.ShowDT
Dim UseVPMDMD:UseVPMDMD = DesktopMode

Dim bsTrough,bsFishFinder,bsVUK,bsCatapult,dtDrop,RampDecals,FlippersColor,FlipColor,DMDColor,DMDCol,plungerIM

LoadVPM "02700000", "WPC.VBS", 3.52

NoUpperLeftFlipper
NoUpperRightFlipper

'****************************** TABLE OPTIONS ********************************************************************************************************
'*****************************************************************************************************************************************************

RampDecals = 0		                    'original(no decals) = 0    Ramp with decals = 1

FlippersColor = 0                       'original(Red Rubber) = 0    Green Rubber = 1    Black Rubber = 2    Random Rubber Color = 3

'DMDColor = 4 '(only for DesktopMode)    'Orange = 0    Red = 1    Green = 2    Blue = 3    Random DMD Color = 4

'*****************************************************************************************************************************************************
'*****************************************************************************************************************************************************

'*********** Standard definitions ****************

 Const UseSolenoids = 2
 Const UseLamps = 0
 Const UseSync = 0
 Const UseGI = 1

'Standard Sounds
 Const SSolenoidOn = "solenoid"
 Const SSolenoidOff = ""
 Const SFlipperOn = ""
 Const SFlipperOff = ""
 Const SCoin = "CoinIn"

'Rom name
 Const cGameName = "ft_l5"

'*************************************************

'************ Fish Tales Init *****************

Sub FishTales_Init
	vpmInit me
	With Controller
		.GameName = cGameName
        .SplashInfoLine = "Fish Tales - Williams 1992"
		 If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
		.HandleKeyboard = 0
        .HandleMechanics = 0
        .Hidden = 0
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
		.Switch(22) = 1 'close coin door
		.Switch(24) = 1 'and keep it closed
	End With

	BallLock1=False
	BallLock2=False
	BallLock3=False

' Main Timer init
PinMAMETimer.Interval = PinMAMEInterval
PinMAMETimer.Enabled = 1

'Nudging
	vpmNudge.TiltSwitch = 14
	vpmNudge.Sensitivity = 4
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingShot, RightSlingShot)

'Trough
Set bsTrough = New cvpmTrough
    With bsTrough
		.Size = 3
		.InitSwitches Array(18, 17, 16)
		.InitExit BallRelease, 55, 7
		'.InitEntrySounds "drain_in", "",""
		.InitExitSounds  SoundFX(SSolenoidOn,DOFContactors),SoundFX("ballrelease",DOFContactors)
		.Balls = 3
		.CreateEvents "bsTrough", ballOutholed
    End With

'Fish Finder Kicker
Set bsFishFinder=New cvpmSaucer
 	With bsFishFinder
 		.InitKicker sw63, 63, 270, 10, 0
 		.InitSounds "kicker_enter_center",SoundFX(SSolenoidOn,DOFContactors),SoundFX("kicker_kick",DOFContactors)
		.CreateEvents "bsFishFinder", sw63
 	End With

'Caster Club Vertical Kicker
Set bsVUK=New cvpmSaucer
 	With bsVUK
 		.InitKicker sw47, 47, 0, 45, 1.56
 		.InitSounds "kicker_enter_center",SoundFX(SSolenoidOn,DOFDropTargets),SoundFX("vuk_exit",DOFDropTargets)
        .CreateEvents "bsVUK", Sw47
 	End With
 
'Catapult
Set bsCatapult = new cvpmSaucer
 	With bsCatapult
 		.InitKicker Catapult, 36, 0, 50, 40
 		.InitSounds "catapult_in",SoundFX("diverter",DOFDropTargets),SoundFX("catapult_fire",DOFDropTargets)
        .CreateEvents "bsCatapult", Catapult
 	End With

'Caster Club Drop Target
 Set dtDrop=New cvpmDropTarget
	With dtDrop
	    .InitDrop sw48, 48
	    .InitSnd SoundFX("droptarget",DOFDropTargets),SoundFX("resetdrop",DOFContactors)
	End With

'Boat Captive Ball
	Kicker1.createsizedballwithmass ballsize/2, ballmass:Kicker1.kick 0,1:Kicker1.enabled=0

'Init other stuff
	InitOptions
	Ramp15.visible=DesktopMode:Ramp16.visible=DesktopMode
	f27Fs.Visible = Not DesktopMode
	If Not DesktopMode Then FsSetup 

Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP Plunger, 52, 0.6
		.Random 0.3
		.InitExitSnd "fx_AutoPlunger", "fx_AutoPlunger"
		.CreateEvents "plungerIM"
	End With

End Sub

'*************************************************

'********* Flippers *************

SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sLRFlipper) = "SolRFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
		 PlaySound SoundFX("FlipperUp",DOFFlippers),1,0.7,-0.1,0.05
		 LeftFlipper.RotateToEnd
     Else
		 PlaySound SoundFX("FlipperDown",DOFFlippers),1,0.06,-0.1,0.05
		 LeftFlipper.RotateToStart
     End If
 End Sub

Sub SolRFlipper(Enabled)
	If enabled Then
		 PlaySound SoundFX("FlipperUp",DOFFlippers),1,0.7,0.1,0.05
		 RightFlipper.RotateToEnd
     Else
		 PlaySound SoundFX("FlipperDown",DOFFlippers),1,0.06,0.1,0.05
		 RightFlipper.RotateToStart
     End If
 End Sub

'*******************************

'************** Keys *************************

Dim resolution

Sub FishTales_KeyDown(ByVal keycode)
If keycode = PlungerKey Then Controller.Switch(31) = 1
If keycode = LeftTiltKey Then PlaySound SoundFX("fx_nudge",0)
If keycode = RightTiltKey Then PlaySound SoundFX("fx_nudge",0)
If keycode = CenterTiltKey Then PlaySound SoundFX("fx_nudge",0)
'If keycode = LeftMagnaSave then ReelBall1.visible=true:ReelBall2.visible=true:ReelBall3.visible=true: ReelMotor(true)
If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub FishTales_KeyUp(ByVal keycode)
If keycode = PlungerKey Then Controller.Switch(31) = 0
'If keycode = LeftMagnaSave then ReelMotor(false)
If vpmKeyUp(keycode) Then Exit Sub
End Sub

'*********************************************

'********* Solenoids ************
  
SolCallback(1)  =	"Auto_Plunger"
SolCallback(2)	=	"SolCatapult"
SolCallback(3)  =	"bsVUK.SolOut"
SolCallback(6)  =   "GateRotate"	
SolCallback(7)  =	"vpmSolSound SoundFX(""knocker"",DOFKnocker),"
SolCallback(9)  = 	"ResetDrain"
SolCallback(10) = 	"bsTrough.SolOut"
SolCallback(11) = 	"bsFishFinder.SolOut"
SolCallback(12) =	"dtDrop.SolDropUp"							
SolCallback(13) =	"dtDrop.SolDropDown"					
SolCallback(28) =   "ReelMotor"

'Flasher Solenoids

SolCallback(17) =	"setlamp 117,"  'Jackpot Flasher
SolCallback(18) =	"setlamp 118,"  'Super Jackpot Flasher
SolCallback(19) =	"setlamp 119,"  'Instant Multiball Flasher
SolCallback(20) =	"setlamp 120,"  'Light Extraball Flasher
SolCallback(21) =	"setlamp 121,"  'Rock the Boat Flasher
SolCallback(22) =	"setlamp 122,"  'Video Mode Flasher
SolCallback(23) =	"setlamp 123,"  'Hold Bonus Flasher
SolCallback(25) =	"setlamp 125,"  'Reel Flasher
SolCallback(26) =	"setlamp 126,"  'Top Left Flasher
SolCallback(27) =	"setlamp 127,"  'Caster Club Flasher

'********************************************

'********************** Auto Plunger ****************************

Sub Auto_Plunger(Enabled)
	If Enabled Then
		plungerIM.AutoFire
		PlaySound SoundFX("fx_AutoPlunger",DOFContactors)
	End If
End Sub

'****************************************************************

'********* Gate ***********

Sub GateRotate(Enabled)
	If enabled Then
		Gate.Open = True
		Wall135.IsDropped = True
		Primitive100.ObjRotY = -75
		PlaySound "diverter"
	Else
		Gate.Open = False
		Wall135.IsDropped = False
		Primitive100.ObjRotY = -15
	End If
End Sub
	
'****************************************************************

'********* Catapult ***********

Dim catdir

Sub SolCatapult(enabled)
	If enabled Then
		catdir=1
		CatapultTimer.enabled=1
		bsCatapult.ExitSol_On
	End If
End Sub

Sub CatapultTimer_timer()
Primitive98.RotX=Primitive98.RotX+catdir
If Primitive98.RotX>=90 And catdir=1 Then catdir=-1
If Primitive98.RotX<=1 Then CatapultTimer.enabled=0
End Sub

Sub ResetDrain(Enabled)
	If Enabled Then
		'Controller.Switch(15) = 0
		sw15_Unhit
	End If
End Sub

'*******************************

'********************** Fishing Reel Motor ****************************
Dim lockedballs
lockedballs = 0

Dim ReelPosition
ReelPosition = 330

Sub ReelMotor(enabled)
 	If enabled Then 
 		ReelTimer.enabled=1
		PlaySound SoundFX("motor_on",DOFGear),1,0.6,-0.1
 	Else
		StopSound "motor_on"
		ReelTimer.enabled=0
		If lockedballs > 0 Then BallOut()
 	End If
End Sub

'REELPOSITION-TABLE
Dim ReelPosTable(360)

'"Switch 38 state : Switch 37 state
ReelPosTable(0)   = "1:1"		'Ball 3 out
ReelPosTable(1)   = "1:1"
ReelPosTable(2)   =	"1:1"
ReelPosTable(3)   = "1:1"
ReelPosTable(4)   = "1:1"
ReelPosTable(5)   = "1:1"
ReelPosTable(6)   = "1:1"
ReelPosTable(7)   = "1:1"
ReelPosTable(8)   = "1:1"
ReelPosTable(9)   = "1:1"
ReelPosTable(10)  = "1:1"
ReelPosTable(11)  = "1:1"
ReelPosTable(12)  = "1:1"
ReelPosTable(13)  = "1:1"
ReelPosTable(14)  = "1:1"
ReelPosTable(15)  = "1:1"
ReelPosTable(16)  = "1:1"
ReelPosTable(17)  = "1:1"
ReelPosTable(18)  = "1:1"
ReelPosTable(19)  = "1:1"
ReelPosTable(20)  = "0:1"
ReelPosTable(21)  = "0:1"
ReelPosTable(22)  = "0:1"
ReelPosTable(23)  = "0:1"
ReelPosTable(24)  = "0:1"
ReelPosTable(25)  = "0:1"
ReelPosTable(26)  = "0:1"
ReelPosTable(27)  = "0:1"
ReelPosTable(28)  = "0:1"
ReelPosTable(29)  = "0:1"
ReelPosTable(30)  = "1:1"
ReelPosTable(31)  = "1:1"
ReelPosTable(32)  = "1:1"
ReelPosTable(33)  = "1:1"
ReelPosTable(34)  = "1:1"
ReelPosTable(35)  = "1:1"
ReelPosTable(36)  = "1:1"
ReelPosTable(37)  = "1:1"
ReelPosTable(38)  = "1:1"
ReelPosTable(39)  = "1:1"
ReelPosTable(40)  = "1:1"
ReelPosTable(41)  = "1:1"
ReelPosTable(42)  = "1:1"
ReelPosTable(43)  = "1:1"
ReelPosTable(44)  = "1:1"
ReelPosTable(45)  = "1:1"
ReelPosTable(46)  = "1:1"
ReelPosTable(47)  = "1:1"
ReelPosTable(48)  = "1:1"
ReelPosTable(49)  = "1:1"
ReelPosTable(50)  = "0:0"
ReelPosTable(51)  = "0:0"
ReelPosTable(52)  = "0:0"
ReelPosTable(53)  = "0:0"
ReelPosTable(54)  = "0:0"
ReelPosTable(55)  = "0:0"
ReelPosTable(56)  = "0:0"
ReelPosTable(57)  = "0:0"
ReelPosTable(58)  = "0:0"
ReelPosTable(59)  = "0:0"
ReelPosTable(60)  = "0:1"		'Lock 2
ReelPosTable(61)  = "0:1"
ReelPosTable(62)  = "0:1"
ReelPosTable(63)  = "0:1"
ReelPosTable(64)  = "0:1"
ReelPosTable(65)  = "0:1"
ReelPosTable(66)  = "0:1"
ReelPosTable(67)  = "0:1"
ReelPosTable(68)  = "0:1"
ReelPosTable(69)  = "0:1"
ReelPosTable(70)  = "0:1"
ReelPosTable(71)  = "0:1"
ReelPosTable(72)  = "0:1"
ReelPosTable(73)  = "0:1"
ReelPosTable(74)  = "0:1"
ReelPosTable(75)  = "0:1"
ReelPosTable(76)  = "0:1"
ReelPosTable(77)  = "0:1"
ReelPosTable(78)  = "0:1"
ReelPosTable(79)  = "0:1"
ReelPosTable(80)  = "0:0"
ReelPosTable(81)  = "0:0"
ReelPosTable(82)  = "0:0"
ReelPosTable(83)  = "0:0"
ReelPosTable(84)  = "0:0"
ReelPosTable(85)  = "0:0"
ReelPosTable(86)  = "0:0"
ReelPosTable(87)  = "0:0"
ReelPosTable(88)  = "0:0"
ReelPosTable(89)  = "0:0"
ReelPosTable(90)  = "1:0"
ReelPosTable(91)  = "1:0"
ReelPosTable(92)  = "1:0"
ReelPosTable(93)  = "1:0"
ReelPosTable(94)  = "1:0"
ReelPosTable(95)  = "1:0"
ReelPosTable(96)  = "1:0"
ReelPosTable(97)  = "1:0"
ReelPosTable(98)  = "1:0"
ReelPosTable(99)  = "1:0"

ReelPosTable(100) = "1:0"
ReelPosTable(101) = "1:0"
ReelPosTable(102) = "1:0"
ReelPosTable(103) = "1:0"
ReelPosTable(104) = "1:0"
ReelPosTable(105) = "1:0"
ReelPosTable(106) = "1:0"
ReelPosTable(107) = "1:0"
ReelPosTable(108) = "1:0"
ReelPosTable(109) = "1:0"
ReelPosTable(110) = "0:0"
ReelPosTable(111) = "0:0"
ReelPosTable(112) = "0:0"
ReelPosTable(113) = "0:0"
ReelPosTable(114) = "0:0"
ReelPosTable(115) = "0:0"
ReelPosTable(116) = "0:0"
ReelPosTable(117) = "0:0"
ReelPosTable(118) = "0:0"
ReelPosTable(119) = "0:0"
ReelPosTable(120) = "0:1"		'Ball 1 out
ReelPosTable(121) = "0:1"
ReelPosTable(122) = "0:1"
ReelPosTable(123) = "0:1"
ReelPosTable(124) = "0:1"
ReelPosTable(125) = "0:1"
ReelPosTable(126) = "0:1"
ReelPosTable(127) = "0:1"
ReelPosTable(128) = "0:1"
ReelPosTable(129) = "0:1"
ReelPosTable(130) = "0:1"
ReelPosTable(131) = "0:1"
ReelPosTable(132) = "0:1"
ReelPosTable(133) = "0:1"
ReelPosTable(134) = "0:1"
ReelPosTable(135) = "0:1"
ReelPosTable(136) = "0:1"
ReelPosTable(137) = "0:1"
ReelPosTable(138) = "0:1"
ReelPosTable(139) = "0:1"
ReelPosTable(140) = "0:0"
ReelPosTable(141) = "0:0"
ReelPosTable(142) = "0:0"
ReelPosTable(143) = "0:0"
ReelPosTable(144) = "0:0"
ReelPosTable(145) = "0:0"
ReelPosTable(146) = "0:0"
ReelPosTable(147) = "0:0"
ReelPosTable(148) = "0:0"
ReelPosTable(149) = "0:0"
ReelPosTable(150) = "1:0"
ReelPosTable(151) = "1:0"
ReelPosTable(152) = "1:0"
ReelPosTable(153) = "1:0"
ReelPosTable(154) = "1:0"
ReelPosTable(155) = "1:0"
ReelPosTable(156) = "1:0"
ReelPosTable(157) = "1:0"
ReelPosTable(158) = "1:0"
ReelPosTable(159) = "1:0"
ReelPosTable(160) = "1:0"
ReelPosTable(161) = "1:0"
ReelPosTable(162) = "1:0"
ReelPosTable(163) = "1:0"
ReelPosTable(164) = "1:0"
ReelPosTable(165) = "1:0"
ReelPosTable(166) = "1:0"
ReelPosTable(167) = "1:0"
ReelPosTable(168) = "1:0"
ReelPosTable(169) = "1:0"
ReelPosTable(170) = "0:0"
ReelPosTable(171) = "0:0"
ReelPosTable(172) = "0:0"
ReelPosTable(173) = "0:0"
ReelPosTable(174) = "0:0"
ReelPosTable(175) = "0:0"
ReelPosTable(176) = "0:0"
ReelPosTable(177) = "0:0"
ReelPosTable(178) = "0:0"
ReelPosTable(179) = "0:0"
ReelPosTable(180) = "0:1"		'Lock 3
ReelPosTable(181) = "0:1"
ReelPosTable(182) = "0:1"
ReelPosTable(183) = "0:1"
ReelPosTable(184) = "0:1"
ReelPosTable(185) = "0:1"
ReelPosTable(186) = "0:1"
ReelPosTable(187) = "0:1"
ReelPosTable(188) = "0:1"
ReelPosTable(189) = "0:1"
ReelPosTable(190) = "0:1"
ReelPosTable(191) = "0:1"
ReelPosTable(192) = "0:1"
ReelPosTable(193) = "0:1"
ReelPosTable(194) = "0:1"
ReelPosTable(195) = "0:1"
ReelPosTable(196) = "0:1"
ReelPosTable(197) = "0:1"
ReelPosTable(198) = "0:1"
ReelPosTable(199) = "0:1"

ReelPosTable(200) = "0:0"
ReelPosTable(201) = "0:0"
ReelPosTable(202) = "0:0"
ReelPosTable(203) = "0:0"
ReelPosTable(204) = "0:0"
ReelPosTable(205) = "0:0"
ReelPosTable(206) = "0:0"
ReelPosTable(207) = "0:0"
ReelPosTable(208) = "0:0"
ReelPosTable(209) = "0:0"
ReelPosTable(210) = "1:0"
ReelPosTable(211) = "1:0"
ReelPosTable(212) = "1:0"
ReelPosTable(213) = "1:0"
ReelPosTable(214) = "1:0"
ReelPosTable(215) = "1:0"
ReelPosTable(216) = "1:0"
ReelPosTable(217) = "1:0"
ReelPosTable(218) = "1:0"
ReelPosTable(219) = "1:0"
ReelPosTable(220) = "1:0"
ReelPosTable(221) = "1:0"
ReelPosTable(222) = "1:0"
ReelPosTable(223) = "1:0"
ReelPosTable(224) = "1:0"
ReelPosTable(225) = "1:0"
ReelPosTable(226) = "1:0"
ReelPosTable(227) = "1:0"
ReelPosTable(228) = "1:0"
ReelPosTable(229) = "1:0"
ReelPosTable(230) = "0:0"
ReelPosTable(231) = "0:0"
ReelPosTable(232) = "0:0"
ReelPosTable(233) = "0:0"
ReelPosTable(234) = "0:0"
ReelPosTable(235) = "0:0"
ReelPosTable(236) = "0:0"
ReelPosTable(237) = "0:0"
ReelPosTable(238) = "0:0"
ReelPosTable(239) = "0:0"
ReelPosTable(240) = "0:1"		'Ball 2 out
ReelPosTable(241) = "0:1"
ReelPosTable(242) = "0:1"
ReelPosTable(243) = "0:1"
ReelPosTable(244) = "0:1"
ReelPosTable(245) = "0:1"
ReelPosTable(246) = "0:1"
ReelPosTable(247) = "0:1"
ReelPosTable(248) = "0:1"
ReelPosTable(249) = "0:1"
ReelPosTable(250) = "0:1"
ReelPosTable(251) = "0:1"
ReelPosTable(252) = "0:1"
ReelPosTable(253) = "0:1"
ReelPosTable(254) = "0:1"
ReelPosTable(255) = "0:1"
ReelPosTable(256) = "0:1"
ReelPosTable(257) = "0:1"
ReelPosTable(258) = "0:1"
ReelPosTable(259) = "0:1"
ReelPosTable(260) = "0:0"
ReelPosTable(261) = "0:0"
ReelPosTable(262) = "0:0"
ReelPosTable(263) = "0:0"
ReelPosTable(264) = "0:0"
ReelPosTable(265) = "0:0"
ReelPosTable(266) = "0:0"
ReelPosTable(267) = "0:0"
ReelPosTable(268) = "0:0"
ReelPosTable(269) = "0:0"
ReelPosTable(270) = "1:0"
ReelPosTable(271) = "1:0"
ReelPosTable(272) = "1:0"
ReelPosTable(273) = "1:0"
ReelPosTable(274) = "1:0"
ReelPosTable(275) = "1:0"
ReelPosTable(276) = "1:0"
ReelPosTable(277) = "1:0"
ReelPosTable(278) = "1:0"
ReelPosTable(279) = "1:0"
ReelPosTable(280) = "1:0"
ReelPosTable(281) = "1:0"
ReelPosTable(282) = "1:0"
ReelPosTable(283) = "1:0"
ReelPosTable(284) = "1:0"
ReelPosTable(285) = "1:0"
ReelPosTable(286) = "1:0"
ReelPosTable(287) = "1:0"
ReelPosTable(288) = "1:0"
ReelPosTable(289) = "1:0"
ReelPosTable(290) = "0:0"
ReelPosTable(291) = "0:0"
ReelPosTable(292) = "0:0"
ReelPosTable(293) = "0:0"
ReelPosTable(294) = "0:0"
ReelPosTable(295) = "0:0"
ReelPosTable(296) = "0:0"
ReelPosTable(297) = "0:0"
ReelPosTable(298) = "0:0"
ReelPosTable(299) = "0:0"

ReelPosTable(300) = "0:1"		'Lock 1
ReelPosTable(301) = "0:1"
ReelPosTable(302) = "0:1"
ReelPosTable(303) = "0:1"
ReelPosTable(304) = "0:1"
ReelPosTable(305) = "0:1"
ReelPosTable(306) = "0:1"
ReelPosTable(307) = "0:1"
ReelPosTable(308) = "0:1"
ReelPosTable(309) = "0:1"
ReelPosTable(310) = "0:1"
ReelPosTable(311) = "0:1"
ReelPosTable(312) = "0:1"
ReelPosTable(313) = "0:1"
ReelPosTable(314) = "0:1"
ReelPosTable(315) = "0:1"
ReelPosTable(316) = "0:1"
ReelPosTable(317) = "0:1"
ReelPosTable(318) = "0:1"
ReelPosTable(319) = "0:1"
ReelPosTable(320) = "0:0"
ReelPosTable(321) = "0:0"
ReelPosTable(322) = "0:0"
ReelPosTable(323) = "0:0"
ReelPosTable(324) = "0:0"
ReelPosTable(325) = "0:0"
ReelPosTable(326) = "0:0"
ReelPosTable(327) = "0:0"
ReelPosTable(328) = "0:0"
ReelPosTable(329) = "0:0"
ReelPosTable(330) = "1:0"
ReelPosTable(331) = "1:0"
ReelPosTable(332) = "1:0"
ReelPosTable(333) = "1:0"
ReelPosTable(334) = "1:0"
ReelPosTable(335) = "1:0"
ReelPosTable(336) = "1:0"
ReelPosTable(337) = "1:0"
ReelPosTable(338) = "1:0"
ReelPosTable(339) = "1:0"
ReelPosTable(340) = "1:0"
ReelPosTable(341) = "1:0"
ReelPosTable(342) = "1:0"
ReelPosTable(343) = "1:0"
ReelPosTable(344) = "1:0"
ReelPosTable(345) = "1:0"
ReelPosTable(346) = "1:0"
ReelPosTable(347) = "1:0"
ReelPosTable(348) = "1:0"
ReelPosTable(349) = "1:0"
ReelPosTable(350) = "1:0"
ReelPosTable(351) = "1:0"
ReelPosTable(352) = "1:0"
ReelPosTable(353) = "1:0"
ReelPosTable(354) = "1:0"
ReelPosTable(355) = "1:0"
ReelPosTable(356) = "1:0"
ReelPosTable(357) = "1:0"
ReelPosTable(358) = "1:0"
ReelPosTable(359) = "1:0"


Dim PosToRot, BallLock1, BallLock2, BallLock3, BallLock1Angle, BallLock2Angle, BallLock3Angle

resolution = 1

ReelTimer.Interval = 5
'ReelTimer.Interval = 1700 / (360 / resolution)

Dim rotateReelBalls
rotateReelBalls = False

Sub ReelTimer_Timer()
	rotateReelBalls = True
	RotateReel()
End Sub


Sub RotateReel()
 	ReelPosition=ReelPosition + resolution
	If ReelPosition >= 360 then ReelPosition = ReelPosition - 360

	Dim SwitchNumbers
	SwitchNumbers = Split(ReelPosTable(ReelPosition), ":")

	Controller.Switch(37) = CInt(SwitchNumbers(1))
	Controller.Switch(38) = CInt(SwitchNumbers(0))


	If rotateReelBalls = True Then
		BallLock1Angle=BallLock1Angle + resolution
		If BallLock1Angle >= 360 then BallLock1Angle = BallLock1Angle - 360
		BallLock2Angle=BallLock2Angle + resolution
		If BallLock2Angle >= 360 then BallLock2Angle = BallLock2Angle - 360
		BallLock3Angle=BallLock3Angle + resolution
		If BallLock3Angle >= 360 then BallLock3Angle = BallLock3Angle - 360
		rotateReelBalls = False
	End if
End Sub

Sub BallOut()

	If ReelPosition >= 40  And ReelPosition <= 100 And lockedballs > 0 Then ReelExit.CreateBall:ReelExit.kick 250, 2:BallLock3=False 
	If ReelPosition >= 160 And ReelPosition <= 220 And lockedballs > 0 Then ReelExit.CreateBall:ReelExit.kick 250, 2:BallLock1=False 
	If ReelPosition >= 280 And ReelPosition <= 350 And lockedballs > 0 Then ReelExit.CreateBall:ReelExit.kick 250, 2:BallLock2=False 

End Sub

Sub ReelEnter_Hit() 	
    Stopsound "metal"
	PlaySound "hop2"
	Me.DestroyBall
	lockedballs = lockedballs + 1
	dim Offset

	If ReelPosition >= 10  And ReelPosition <= 30 Then Offset = 40 + 42:BallLock1=True:BallLock1Angle=ReelPosition + Offset
	If ReelPosition >= 130 And ReelPosition <= 150 Then Offset = 280 + 42:BallLock2=True:BallLock2Angle=ReelPosition + Offset
	If ReelPosition >= 250 And ReelPosition <= 270 Then Offset = 160 + 42:BallLock3=True:BallLock3Angle=ReelPosition + Offset

	'If lockedballs = 1 then BallLock1=True:BallLock1Angle=ReelPosition + Offset
	'If lockedballs = 2 then BallLock2=True:BallLock2Angle=ReelPosition + Offset
	'If lockedballs = 3 then BallLock3=True:BallLock3Angle=ReelPosition + Offset

End Sub

Sub outhole()
	PlaySound "outhole"
	Drain.kick 80, 10
	Controller.Switch(15) = 0
End Sub
 
'*********************************************************************

'****************** Switches *********************
Sub sw25_Hit():VPMTimer.PulseSw 25:PlaySound "metalhit_thin",1,0.2:End Sub 'Left OutLane
Sub sw26_Hit():VPMTimer.PulseSw 26:PlaySound "metalhit_thin",1,0.2:End Sub 'Left InLane
Sub sw27_Hit():VPMTimer.PulseSw 27:PlaySound SoundFX("target",DOFTargets):End Sub 'Standup Target
Sub sw28_Hit():VPMTimer.PulseSw 28:PlaySound SoundFX("target",DOFTargets):End Sub 'Standup Target
Sub sw32_Hit():VPMTimer.PulseSw 32:Primitive147.RotX=67:PlaySound "target":Me.TimerInterval=150:Me.Timerenabled=1:End Sub 'Ramp Left Sensor
Sub sw32_timer():Me.Timerenabled=0:Primitive147.RotX=80:End Sub
Sub sw33_Hit():VPMTimer.PulseSw 33:Primitive148.RotX=67:PlaySound "target":Me.TimerInterval=150:Me.Timerenabled=1:End Sub 'Ramp Right Sensor
Sub sw33_timer():Me.Timerenabled=0:Primitive148.RotX=80:End Sub
Sub sw34_Spin():VPMTimer.PulseSw 34:End Sub 'Spinner
Sub sw41_Hit():VPMTimer.PulseSw 41:PlaySound SoundFX("target",DOFTargets):End Sub 'Target Boat Captive Ball
Sub sw42_Hit():VPMTimer.PulseSw 42:PlaySound "metalhit_thin":End Sub 'Boat Ramp Right Trigger
Sub sw35_Hit():Controller.Switch(35) = 1:End Sub 'Reel Entry Trigger
Sub sw35_UnHit():Controller.Switch(35)= 0:End Sub
Sub sw43_Hit():VPMTimer.PulseSw 43:PlaySound "metalhit_thin",1,0.2:End Sub 'Boat Ramp Left Trigger
Sub sw44_Hit():VPMTimer.PulseSw 44:PlaySound "metalhit_thin",1,0.2:End Sub 'E Trigger
Sub sw45_Hit():VPMTimer.PulseSw 45:PlaySound "metalhit_thin",1,0.2:End Sub 'I Trigger
Sub sw46_Hit():VPMTimer.PulseSw 46:PlaySound "metalhit_thin",1,0.2:End Sub 'L Trigger
Sub sw47_Hit():Controller.Switch(47) = 1:End Sub
Sub sw47_UnHit():Primitive146.TransY=20:Me.TimerInterval=200:Controller.Switch(47) = 0:Me.TimerEnabled=1:End Sub
Sub sw47_timer():Me.TimerEnabled=0:Primitive146.TransY=0:End Sub
Sub sw48_dropped():dtDrop.Hit 1:Controller.Switch(48) = 1:End Sub 'Drop Target
Sub sw54_Hit():VPMTimer.PulseSw 54:PlaySound SoundFX("target",DOFTargets):End Sub 'Standup Target
Sub sw55_Hit():VPMTimer.PulseSw 55:PlaySound SoundFX("target",DOFTargets):End Sub 'Standup Target
Sub sw56_Hit():Controller.Switch (56)=1:End Sub 'Shooter Lane Trigger
Sub sw56_UnHit():Controller.Switch (56)=0:End Sub
Sub sw61_Hit():VPMTimer.PulseSW 61:PlaySound SoundFX("target",DOFTargets):End Sub 'Oblong Target
Sub sw62_Hit():VPMTimer.PulseSw 62:PlaySound "metal_thin":End Sub 'Right Green Lane Trigger
Sub sw64_Hit():VPMTimer.PulseSw 64:PlaySound "metalhit_thin",1,0.2:End Sub 'Left Green Lane Trigger
Sub sw65_Hit():VPMTimer.PulseSw 65:PlaySound "metalhit_thin",1,0.2:End Sub 'Right InLane
Sub sw66_Hit():VPMTimer.PulseSw 66:PlaySound "metalhit_thin",1,0.2:End Sub 'Right OutLane
Sub sw15_Hit():PlaySound "drain_in":Controller.Switch(15) = 1:End Sub
Sub sw15_Unhit():StopSound "drain_in":outhole():End Sub

Sub Gate_Hit()
	PlaySound "gate"
	Gate.Open = true
	Wall135.IsDropped=True
	Primitive100.ObjRotY = -75
	GateUnhit.Enabled = true
End Sub


Sub GateUnhit_Timer()
	Gate.Open = False
	Wall135.IsDropped=False
	Primitive100.ObjRotY = -15
	GateUnhit.Enabled = False
End Sub
		

'**************************************************

'********** Bumpers **********************************************
   
Sub Bumper1_Hit
VPMTimer.PulseSw 51 
	Dim BumpSound         
	BumpSound = Int(rnd*4)+1
	Select Case BumpSound
	Case 1: PlaySound SoundFX("fx_bumper1",DOFContactors)
	Case 2: PlaySound SoundFX("fx_bumper2",DOFContactors)
	Case 3: PlaySound SoundFX("fx_bumper3",DOFContactors)
	Case 4: PlaySound SoundFX("fx_bumper4",DOFContactors)
	End Select	  
End Sub

Sub Bumper2_Hit
VPMTimer.PulseSw 52           
	Dim BumpSound         
	BumpSound = Int(rnd*4)+1
	Select Case BumpSound
	Case 1: PlaySound SoundFX("fx_bumper1",DOFContactors)
	Case 2: PlaySound SoundFX("fx_bumper2",DOFContactors)
	Case 3: PlaySound SoundFX("fx_bumper3",DOFContactors)
	Case 4: PlaySound SoundFX("fx_bumper4",DOFContactors)
	End Select
End Sub

Sub Bumper3_Hit
VPMTimer.PulseSw 53            
	Dim BumpSound         
	BumpSound = Int(rnd*4)+1
	Select Case BumpSound
	Case 1: PlaySound SoundFX("fx_bumper1",DOFContactors)
	Case 2: PlaySound SoundFX("fx_bumper2",DOFContactors)
	Case 3: PlaySound SoundFX("fx_bumper3",DOFContactors)
	Case 4: PlaySound SoundFX("fx_bumper4",DOFContactors)
	End Select 
End Sub

'*****************************************************************

'********** Slingshots ***************

Dim Lstep,RStep

Sub LeftSlingShot_Slingshot
	VPMTimer.PulseSw 57
    PlaySound SoundFX("LSling",DOFContactors),0,1,-0.05,0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -32
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -17
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1

End Sub

Sub RightSlingShot_Slingshot
	VPMTimer.PulseSw 58
    PlaySound SoundFX("RSling",DOFContactors), 0, 1, 0.05, 0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -32
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
       Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -17
       Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'*****************************************************

'*************** General Illumination *****************

 Set GiCallback = GetRef("UpdateGI")
 Set GiCallback2 = GetRef("UpdateGI2")

Dim gistep, xx

Sub UpdateGI(nr, enabled)
	Select Case nr
        Case 2	'Top GI
            For xx = 0 to 30:TopGI(xx).state=enabled:next
            For xx = 31 to 39:TopGI(xx).visible=enabled:next
        Case 4	'Bottom GI
            For each xx in BottomGI:xx.state=enabled:next
    End Select
End Sub

Sub UpdateGI2(no, step)
    If step=0 Then exit Sub 
    gistep=(step-1)/7

	If gistep = 1 Then
		DOF 101, DOFOn
	Else
		DOF 101, DOFOff
	End If

    Select Case no
        Case 2	'Top GI
            For each xx in TopGI:xx.IntensityScale=gistep:next
        Case 4	'Bottom GI		
            For each xx in BottomGI:xx.IntensityScale=gistep:next
    End Select
	FishTales.ColorGradeImage = "ColorGrade_" & step
End Sub

'*****************************************************
 
'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 5  'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
    UpdateLamps
End Sub

Sub UpdateLamps
    'NFadeL 1, l1
    'NFadeL 2, l2
    'NFadeL 3, l3
    'NFadeL 4, l4
    'NFadeL 5, l5
    'NFadeL 6, l6
    'NFadeL 7, l7
    'NFadeL 8, l8
    'NFadeL 9, l9
    'NFadeL 10, l10
	NFadeLm 11, l11a
    FadeObj 11, l11, "mpfl4", "mpfl3", "mpfl2", "mpfl1"
	NFadeLm 12, l12a
    FadeObj 12, l12, "mpfl4", "mpfl3", "mpfl2", "mpfl1"
	NFadeLm 13, l13a
    FadeObj 13, l13, "mpfl4", "mpfl3", "mpfl2", "mpfl1"
	NFadeLm 14, l14a
    FadeObj 14, l14, "mpfl4", "mpfl3", "mpfl2", "mpfl1"
	NFadeLm 15, l15a
    FadeObj 15, l15, "mpfl4", "mpfl3", "mpfl2", "mpfl1"
    NFadeLm 16, l16
	Flash 16, l16f
    NFadeLm 17, l17
	Flash 17, l17f
    NFadeLm 18, l18
	Flash 18, l18f
    'NFadeL 19, l19
    'NFadeL 20, l20
    NFadeL 21, l21
    NFadeL 22, l22
    NFadeL 23, l23
    NFadeL 24, l24
    NFadeL 25, l25
	NFadeL 26, l26
    NFadeL 27, l27
    NFadeL 28, l28
    'NFadeL 29, l29
    'NFadeL 30, l30
    NFadeL 31, l31
    NFadeL 32, l32
    NFadeL 33, l33
    NFadeL 34, l34
	NFadeLm 35, l35a
    FadeObj 35, l35, "mpfl4", "mpfl3", "mpfl2", "mpfl1"
	NFadeLm 36, l36a
    FadeObj 36, l36, "mpfl4", "mpfl3", "mpfl2", "mpfl1"
	NFadeLm 37, l37a
    FadeObj 37, l37, "mpfl4", "mpfl3", "mpfl2", "mpfl1"
	NFadeLm 38, l38a
    FadeObj 38, l38, "mpfl4", "mpfl3", "mpfl2", "mpfl1"
    'NFadeL 39, l39
    'NFadeL 40, l40
	NFadeL 41, l41
    NFadeL 42, l42
    NFadeL 43, l43
    NFadeL 44, l44
    NFadeL 45, l45
    NFadeL 46, l46
    NFadeL 47, l47
    NFadeLm 48, l48
	NFadeL 48, l48a
    'NFadeL 49, l49
    'NFadeL 50, l50
    NFadeL 51, l51
    NFadeL 52, l52
    NFadeL 53, l53
    NFadeL 54, l54
    NFadeL 55, l55
    NFadeL 56, l56
    NFadeL 57, l57
    NFadeL 58, l58
    'NFadeL 59, l59
    'NFadeL 60, l60
    NFadeL 61, l61
    NFadeL 62, l62
    NFadeL 63, l63
    NFadeL 64, l64
	NFadeL 65, l65
	NFadeL 66, l66
	NFadeL 67, l67
	NFadeL 68, l68
	'NFadeL 69, l69
	'NFadeL 70, l70
	Flash 71, l71
	NFadeL 72, l72
	NFadeL 73, l73
	NFadeL 74, l74
	NFadeL 75, l75
	NFadeL 76, l76
	NFadeL 77, l77
	NFadeL 78, l78
	'NFadeL 79, l79
	'NFadeL 80, l80
	Flashm 81, l81
	Flash 81, l81b
	Flashm 82, l82
	Flash 82, l82b
	Flashm 83, l83
	Flash 83, l83b
	Flashm 84, l84
	Flash 84, l84b
	Flashm 85, l85
	Flash 85, l85b
	Flash 86, l86
	
'Flashers
	Flash 117, f17
	Flash 118, f18
	Flash 119, f19
	Flash 120, f20
	Flash 121, f21
	Flash 122, f22
	Flash 123, f23

	Flashm 125, f25
	Flashm 125, f25a
	Flashm 125, f25b
	Flash 125, f25c

	Flashm 126, f26
	Flash 126, f26b

	Flashm 127, f27
	Flashm 127, f27b
	Flash 127, f27Fs

End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.4   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.2 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself

Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

' Flasher objects

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(nr)
End Sub

'*******************************************************

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 4000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / FishTales.width-1
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

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 4 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub BallRolling()
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
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'*******************************************

'********** In Game Updates ************
Dim radians, angle, radius, angleb, balx ,baly, balyb ,balz, rada, radab

Set MotorCallback = GetRef ("RealTimeUpdates")

Sub RealTimeUpdates()	
	Reel.Rotx = ReelPosition + 46
	If Reel.Rotx >= 360 then Reel.Rotx = Reel.Rotx - 360
	Reel1.RotX=Reel.RotX

	radians = 3.1415926 / 180
	radius 	= 60

	'Ball1
	ReelBall1.visible=BallLock1
	angle= BallLock1Angle 
	rada=radians*angle
	radab=radians*angleb
	ReelBall1.z=sin(rada)*radius+Reel.z
	ReelBall1.y=cos(rada)*radius+Reel.y

	'ball2
	ReelBall2.visible=BallLock2
	angle= BallLock2Angle 
	rada=radians*angle
	radab=radians*angleb
	ReelBall2.z=sin(rada)*radius+Reel.z
	ReelBall2.y=cos(rada)*radius+Reel.y
	
	'Ball3
	ReelBall3.visible=BallLock3
	angle= BallLock3Angle 
	rada=radians*angle
	radab=radians*angleb
	ReelBall3.z=sin(rada)*radius+Reel.z
	ReelBall3.y=cos(rada)*radius+Reel.y
	

	Primitive109.RotY=LeftFlipper.CurrentAngle-90
	Primitive110.RotY=RightFlipper.CurrentAngle+90

	BallRolling()

End Sub

'***********************************************************************************************

'******************* Others Table Sounds *************************

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*2, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*2, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*2, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*2, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber2", 0, parm / 50, -0.1, 0.15
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber2", 0, parm / 50, 0.1, 0.15
End Sub

Sub Trigger1_Hit():PlaySound "metal":End Sub 'Caster Club Wire Ramp Hit
Sub Trigger2_Hit():vpmtimer.addtimer 50, "ballhop'":End Sub 'Exit Catapult Ramp Playfield Hit
Sub Trigger3_Hit():PlaySound "fx_metalrolling":lockedballs = lockedballs - 1:End Sub 'Catapult Ramp Hit
Sub Trigger4_Hit():PlaySound "metal":End Sub
Sub Trigger5_Hit():PlaySound "fx_metalrolling":End Sub 'Left Boat Ramp Hit
Sub Trigger6_Hit():PlaySound "fx_metalrolling":End Sub 'Right Boat ramp Hit
Sub Trigger7_Hit():Trigger8.enabled=1:StopSound "fx_metalrolling":End Sub 'Exit Left Boat Ramp Playfield Hit
Sub Trigger8_Hit():PlaySound "ballhop",1,0.3:Trigger8.enabled=0:End Sub 
Sub Trigger9_Hit():Trigger10.enabled=1:StopSound "fx_metalrolling":End Sub 'Exit Right Boat Ramp Playfield Hit
Sub Trigger10_Hit():PlaySound "ballhop",1,0.3:Trigger10.enabled=0:End Sub
Sub Wall48_Hit():PlaySound "rubber_hit_3",1,0.1:End Sub 'Left Boat Ramp Stopper Hit
Sub Wall110_Hit():PlaySound "rubber_hit_3",1,0.1:End Sub 'Right Boat Ramp Stopper Hit
Sub Trigger11_Hit():PlaySound "metalhit_medium",1,0.2:End Sub 'ShooterLane Ramp Hit
Sub Trigger13_Hit():StopSound "metal":Trigger14.enabled=1:End Sub
Sub Trigger14_Hit():PlaySound "ballhoptwice",1,0.05:Trigger14.enabled=0:End Sub 'Exit ShooterLane Ramp Playfield Hit
Sub Wall97_Hit():PlaySound "metalhit_thin":End Sub 'Release Ball Hit
Sub Wall216_Hit():PlaySound "metalhit_medium",1,0.01:End Sub 'Plunger Hit
Sub Trigger12_Hit():PlaySound "metalhit2",1,0.1:End Sub 'Boat Ramp Hit
Sub Wall53_Hit():PlaySound "metalhit_medium",0,Vol(ActiveBall)*3,Pan(ActiveBall),0,Pitch(ActiveBall),1,0:End Sub 'Apron Hit
Sub Wall146_Hit():PlaySound "metalhit_thin":End Sub 'Little Wire Guide of I lane Hit
Sub Wall147_Hit():PlaySound "metalhit_thin":End Sub 'Little Wire Guide of I lane Hit
Sub Wall148_Hit():PlaySound "metalhit_thin":End Sub 'Fish Finder metal guide on enter Hit
Sub ballhop:PlaySound "ballhoptwice",1,0.3:End Sub

'*****************************************************************

'********* Table Options **********

Sub InitOptions
	If RampDecals=0 Then Ramp34.visible=0:Ramp35.visible=0:Primitive132.visible=0:Primitive142.visible=1:Primitive133.visible=0:Primitive143.visible=1:End If
	If FlippersColor=1 Then Primitive109.image="ft_flipper_left_GREEN":Primitive110.image="ft_flipper_right_GREEN":End If
	If FlippersColor=2 Then Primitive109.image="ft_flipper_left_BLACK":Primitive110.image="ft_flipper_right_BLACK":End If
	If FlippersColor=3 Then FlipColor=Int(Rnd*3)+1 End If   
	Select Case FlipColor
		Case 1 : Primitive109.image="ft_flipper_left":Primitive110.image="ft_flipper_right"
		Case 2 : Primitive109.image="ft_flipper_left_GREEN":Primitive110.image="ft_flipper_right_GREEN"
		Case 3 : Primitive109.image="ft_flipper_left_BLACK":Primitive110.image="ft_flipper_right_BLACK"
	End Select
'	If DMDColor=0 Then ScoreText1.Visible=0:ScoreText2.Visible=0:ScoreText3.Visible=0:End If
'	If DMDColor=1 Then ScoreText.Visible=0:ScoreText1.Visible=1:ScoreText2.Visible=0:ScoreText3.Visible=0:End If
'	If DMDColor=2 Then ScoreText.Visible=0:ScoreText1.Visible=0:ScoreText2.Visible=1:ScoreText3.Visible=0:End If
'	If DMDColor=3 Then ScoreText.Visible=0:ScoreText1.Visible=0:ScoreText2.Visible=0:ScoreText3.Visible=1:End If
'	If DMDColor=4 Then DMDCol=Int(Rnd*4)+1 End If
'	Select Case DMDCol
'		Case 1 : ScoreText1.Visible=0:ScoreText2.Visible=0:ScoreText3.Visible=0
'		Case 2 : ScoreText.Visible=0:ScoreText1.Visible=1:ScoreText2.Visible=0:ScoreText3.Visible=0
'		Case 3 : ScoreText.Visible=0:ScoreText1.Visible=0:ScoreText2.Visible=1:ScoreText3.Visible=0
'		Case 4 : ScoreText.Visible=0:ScoreText1.Visible=0:ScoreText2.Visible=0:ScoreText3.Visible=1
'	End Select
End Sub

'**********************************

'******** Cabinet Mode Adjustment *********

Sub FsSetup()
l86.Height=280:l16f.Height=270:l17f.Height=270:l18f.Height=270:l81.RotX=90:l82.RotX=90:l83.RotX=90:l84.RotX=90:l85.RotX=90:l81.Height=320:l82.Height=280:l83.Height=250:l84.Height=215:l85.Height=170
l81b.Opacity=700:l82b.Opacity=700:l83b.Opacity=700:l84b.Opacity=700:l85b.Opacity=700:l81b.Height=325:l82b.Height=290:l83b.Height=260:l84b.Height=220:l85b.Height=185
l71.Height=200:LMoray.Height=320:LPufferFish.Height=240:LMermaid.Height=320:LTail.Height=310:LLittleFish.Height=320:Light11.Intensity=5:Light12.Intensity=5
f26.Height=500:f26.RotX=0:Lboat.Y=650:Lboat.RotX=0:Lboat.Height=170:f27.Height=250:f27.Opacity=800
End Sub

'******************************************
