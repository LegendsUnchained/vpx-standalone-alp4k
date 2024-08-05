'The Who's Tommy Pinball Wizard - IPDB No. 2579
'© Data East 1994
'Table Recreation by ninuzzu for Visual Pinball 10
'Credits/Thanks (in no particular order):
'Franzleo for the ramps and the airplane models
'Freneticamnesic for the help with the mirror animation
'Dark and Zany for some primitive templates I used and edited
'Javier1515,JPSalas,GTXJoe and Shoopity for some code and sounds effects I borrowed from their tables
'Arngrim for the DOF support
'VPDev Team for the new freaking amazing VPX

'VPW Mod:
'Physical, visual, and sonic overhaul: nFozzy physics, Lampz, Flupper flashers, Fleep sounds, Skitso lighting overhaul, 3D inserts, Roth targets, Apophis sling corrections,
'Rawd & Leojreimroc VR Room and functioning VR backglass, new art assets, new ramp models, physical table mesh, staged flippers, Dynamic Ball Shadows, Fluffhead35 RampRolling sound code
'And thank you to testers PinStratsDan, BountyBob, Bietekwiet, PrimeTime5k, Rajo Joey, and GameClubCentral's Smaug!

Option Explicit
Randomize

'***********************************************************************
'* TABLE OPTIONS *******************************************************
'***********************************************************************

'***********	General Sound Options	********************************
Const VolumeDial = 0.8			'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.5 		'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.9 		'Level of ramp rolling volume. Value between 0 and 1

'***********	Set VR Room	  ******************************************
Const VRRoomChoice = 1			'1 = BaSti/Rawd Minimal Room, 2 = Minimal Room, 3 = Ultra Minimal Room
Const VRBGBlink = 1				'1 = Blinking middle bulbs, 2 = Off

'***********	Toggle Staged Left Flippers		************************
Const StagedFlipper = 0			'0 = No staged flipper, 1 = Staged Flipper (Seperate input for Upper Left flipper)

'***********	Set Outlane Difficulty		****************************
Const LeftDifficulty	= 1		'0 = Easy, 1 = Medium (Factory), 2 = Hard
Const RightDifficulty	= 1		'0 = Easy, 1 = Medium (Factory), 2 = Hard
Const InlaneDifficulty	= 0		'0 = Easy, 1 = Hard (Factory)

'***********	Set the Left Scoop kick direction	********************
Const Scoop_kick = 2			'0 = left flipper, 1 = right flipper, 2 = random

'***********	Set the Color of Flippers Rubbers	********************
FlipperColor = 1				'0 = black, 1 = red, 2 = random

'***********	Set the Color of the Rubber Post Sleeves	************
PostColor = 1					'0 = black, 1 = yellow, 2 = orange (TNT Amusements mod color), 3 = random

'***********	Set the Color of Rubbers	****************************
RubberColor = 0					'0 = black, 1 = white, 2 = random

'***********	Protective Washers Visibility	************************
Const Washers = 1				'0 = disabled , 1 = enabled

'***********	Enable Union Jack Mod (Targets with UK flag)	********
Const UnionJackMod = 1			'0 = disabled, 1 = enabled

'***********	Enable TNT Amusements Mod (Bulbs on plane)	************
Const TNTMod = 1				'0 = disabled , 1 = enabled

'***********	Ball Appearance		************************************
Const BallType = 3				'0 = Black , 1 = Eye , 2 = Dots , 3 = Normal , 4 = Bright

'***********	Ball Shadow Options		********************************
Const DynamicBallShadowsOn = 1	'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1	'0 = Static shadow under ball ("flasher" image, like JP's)
								'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
								'2 = flasher image shadow, but it moves like ninuzzu's

'***********************************************************************
'* END TABLE OPTIONS ***************************************************
'***********************************************************************




'******************************************************
'* VPM INIT *******************************************
'******************************************************

Const Ballsize = 50							'Ball size must be 50
Const BallMass = 1							'Ball mass must be 1
Const tnob = 7								'Total number of balls
Const lob = 1								'Locked Balls
Dim tablewidth: tablewidth = Tommy.width
Dim tableheight: tableheight = Tommy.height
Dim BIPL : BIPL = False						'Ball in plunger lane
Dim DesktopMode:DesktopMode = Tommy.ShowDT
Dim UseVPMDMD, VRRoom
Dim FlipperColor, PostColor, RubberColor
If RenderingMode = 2 Then VRRoom = VRRoomChoice Else VRRoom = 0
If VRRoom <> 0 Then
	UseVPMDMD = True
	Tommy.Playfieldreflectionstrength = 4
Else
	UseVPMDMD = DesktopMode
End If

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "02000000", "de.vbs", 3.5

Sub Tommy_Paused:Controller.Pause = 1:End Sub
Sub Tommy_unPaused:Controller.Pause = 0:End Sub
Sub Tommy_exit():RightSlingShot.SlingshotThreshold=SlingDefault:LeftSlingShot.SlingshotThreshold=SlingDefault:Controller.Stop:End Sub

'******************************************************
'* STANDARD DEFINITIONS *******************************
'******************************************************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0
Const SSolenoidOn ="fx_solon"
Const SSolenoidOff = ""
Const SFlipperOn = ""
Const SFlipperOff = ""
Const SCoin = "fx_coin"

'******************************************************
'* ROM VERSION ****************************************
'******************************************************

'Const cGameName = "tomy_400"	'Less bugs, but not as fun
Const cGameName = "tomy_500"	'Custom rom by Soren - better scoring/rules and more blinder!

'******************************************************
'* KEYS ***********************************************
'******************************************************
Dim LeftPress:LeftPress=0
Dim vrLeftX, vrRightX, vrStartY, vrEBallY, Flasher1y, Flasher2y, Flasher3y
vrLeftX = VRFlipperButtonLeft.x
vrRightX = VRFlipperButtonRight.x
vrStartY = LaunchButton.y
vrEBallY = ExtraBallButton.y
Flasher1Y = Flasher1.y
Flasher2Y = Flasher2.y
Flasher3Y = Flasher3.y

Sub Tommy_KeyDown(ByVal Keycode)
	If keycode = 3 Then		'extra ball button
		Controller.Switch(8)=1
		If VRRoom > 0 And VRRoom < 3 Then
			ExtraBallButton.y = vrEBallY - 2
			Flasher1.y = Flasher1.y - 2
		End If
	End If
	If keycode = LeftTiltKey Then Nudge 90,1.5 : SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270,1.5 : SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0,1.5 : SoundNudgeCenter
	If keycode = PlungerKey Then Plunger.Pullback: SoundPlungerPull

	If StagedFlipper = 1 Then
		If keycode = LeftFlipperKey Then
			FlipperActivate LeftFlipper, LFPress
			LeftPress = 1
			If VRroom > 0 And VRroom < 3 Then VRFlipperButtonLeft.x = vrLeftX + 2.5
		End If
		If keycode = KeyUpperLeft Then
			FlipperActivate LeftFlipper1, LFPress1
			SolULFlipper True
			If LeftPress = 0 Then
				FlipperActivate LeftFlipper, LFPress
				SolLFlipper True
			End If
			If VRroom > 0 And VRroom < 3 Then VRFlipperButtonLeft.x = vrLeftX + 5
		End If
	Else
		If keycode = LeftFlipperKey Then
			FlipperActivate LeftFlipper, LFPress
			FlipperActivate LeftFlipper1, LFPress1
			SolULFlipper True
			If VRroom > 0 And VRroom < 3 Then VRFlipperButtonLeft.x = vrLeftX + 5
		End If
	End If

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		If VRroom > 0 And VRroom < 3 Then VRFlipperButtonRight.x = vrRightX - 5
	End If
	If keycode = StartGameKey Then
		SoundStartButton
		If VRroom > 0 And VRroom < 3 Then
			LaunchButton.y = vrStartY - 2
			Flasher2.y = Flasher2y - 2
			Flasher3.y = Flasher3y - 2
		End If
	End If
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If
	If VRRoom > 0 and VRRoom < 3 Then
		If keycode = PlungerKey Then
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
	End If
	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Tommy_KeyUp(ByVal Keycode)
	If keycode = 3 Then
		Controller.Switch(8) = 0
		If VRRoom > 0 And VRRoom < 3 Then
			ExtraBallButton.y = vrEBallY
			Flasher1.y = Flasher1y
		End If
	End If
	If KeyCode = PlungerKey Then
		Plunger.Fire
		If BIPL Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		If VRRoom > 0 and VRRoom < 3 Then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
		End If
	End If
	If keycode = LeftFlipperKey Then
		LeftPress = 0
		FlipperDeActivate LeftFlipper, LFPress
		FlipperDeActivate LeftFlipper1, LFPress1
		SolULFlipper False
		If VRroom > 0 And VRroom < 3 Then VRFlipperButtonLeft.x = vrLeftX
	End If

	If StagedFlipper = 1 Then
		If keycode = KeyUpperLeft Then
			FlipperDeActivate LeftFlipper1, LFPress1
			SolULFlipper False
			If LeftPress = 0 Then
				FlipperDeActivate LeftFlipper, LFPress
				SolLFlipper False
				If VRroom > 0 And VRroom < 3 Then VRFlipperButtonLeft.x = vrLeftX
			Else
				If VRroom > 0 And VRroom < 3 Then VRFlipperButtonLeft.x = vrLeftX + 2.5
			End If
		End If
	End If

	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		If VRroom > 0 And VRroom < 3 Then VRFlipperButtonRight.x = vrRightX
	End If

	If VRRoom > 0 and VRRoom < 3 Then
		If keycode = StartGameKey Then
			LaunchButton.y = vrStartY
			Flasher2.y = Flasher2y
			Flasher3.y = Flasher3y
		End If
	End If
	If vpmKeyUp(keycode) Then Exit Sub
End Sub

'******************************************************
'* TABLE INIT *****************************************
'******************************************************

Dim TommyBall1, TommyBall2, TommyBall3, TommyBall4, TommyBall5, TommyBall6, TommyCaptiveBall, gBOT
Dim SlingDefault

Sub Tommy_Init

	'* ROM AND DMD ****************************************

	vpminit me
	vpmFlips.FlipperSolNumber(2) = 47

	With Controller
		.GameName = cGameName
		.SplashInfoLine = "The Who's Tommy the Pinball Wizard - Data East 1994" & vbNewLine & "VPW"
		.HandleMechanics = 0
		.HandleKeyboard = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.ShowTitle = 0
'		.Hidden = DesktopMode
	End With
	On Error Resume Next
	Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0

	'* PINMAME TIMER **************************************

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	'* NUDGE **********************************************

	vpmNudge.TiltSwitch=1
	vpmNudge.Sensitivity=7
	vpmNudge.TiltObj=Array(bumper1,bumper2,bumper3,LeftSlingshot,RightSlingshot)

	SlingDefault=LeftSlingShot.SlingshotThreshold

	'* TROUGH *********************************************

	Select Case BallType
		Case 0: Tommy.BallImage="Black1": Tommy.BallFrontDecal="Calle_Scratches_Less"
		Case 1: Tommy.BallImage="Black1": Tommy.BallFrontDecal="BlinderEye1"
		Case 2: Tommy.BallImage="Black1": Tommy.BallFrontDecal="white_dots"
		Case 3: Tommy.BallImage="Ball_HDR": Tommy.BallFrontDecal="Calle_Scratches_Less"
		Case 4: Tommy.BallImage="old_ass_eyes_ball": Tommy.BallFrontDecal="Calle_Scratches_Less"
	End Select

	Set TommyBall1 = sw14.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TommyBall2 = sw13.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TommyBall3 = sw12.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TommyBall4 = sw11.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TommyBall5 = sw10.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TommyBall6 = sw9.CreateSizedballWithMass(Ballsize/2,Ballmass)

	Controller.Switch(9) = 1
	Controller.Switch(10) = 1
	Controller.Switch(11) = 1
	Controller.Switch(12) = 1
	Controller.Switch(13) = 1
	Controller.Switch(14) = 1


	'* CAPTIVE BALL *********************************************

	Set TommyCaptiveBall = captiveBall.CreateSizedballWithMass(Ballsize/2,Ballmass)

	gBOT = Array(TommyCaptiveBall,TommyBall1,TommyBall2,TommyBall3,TommyBall4,TommyBall5,TommyBall6)

	vpmTimer.AddTimer 300, "captiveball.kick 180,1 '"
	vpmTimer.AddTimer 310, "captiveball.enabled= 0 '"
	

	'* STARTUP CALLS **************************************

	TopDiverter.IsDropped = 1

	If DesktopMode = true then
		l39.visible= 0
		l39a.visible= 1
		l40.visible= 0
		l40a.visible= 1
'		l56.visible= 0
		'l56a.visible= 1
		l60.visible= 0
		l60a.visible= 1
		l61.visible= 0
		l61a.visible= 1
		If VRRoom = 0 Then
			leftrail.visible= 1
			rightrail.visible= 1
		End If
	Else
		l39.visible= 1
		l39a.visible= 0
		l40.visible= 1
		l40a.visible= 0
		'l56.visible= 1
		'l56a.visible= 0
		l60.visible= 1
		l60a.visible= 0
		l61.visible= 1
		l61a.visible= 0
		leftrail.visible= 0
		rightrail.visible= 0
	End If

	Select Case LeftDifficulty
		Case 0: Rubber2.visible=True:Post018.collidable=True:PegMetalT28.Y=1393:screw2.Y=1393
		Case 1: Rubber007.visible=True:Post029.collidable=True
		Case 2: Rubber001.visible=True:Post026.collidable=True:PegMetalT28.Y=1371:screw2.Y=1371
	End Select

	Select Case RightDifficulty
		Case 0: Rubber1.visible=True:Post027.collidable=True:PegMetalT20.Y=1392:screw1.Y=1392
		Case 1: Rubber006.visible=True:Post028.collidable=True
		Case 2: Rubber002.visible=True:Post024.collidable=True:PegMetalT20.Y=1370:screw1.Y=1370
	End Select

	Select Case InlaneDifficulty
		Case 0: Rubber15.visible=True:Peg001.collidable=True:Rubber14.visible=True:Peg002.collidable=True
		Case 1: Rubber16.visible=True:Peg003.collidable=True:Rubber17.visible=True:Peg005.collidable=True:Pin1.Y=1474.3:Pin2.Y=1477.2
	End Select

'	Randomize
	If FlipperColor = 2 then FlipperColor = RndInt(0,1)
	If FlipperColor = 1 then
		LeftFlipperP.image = "flippers-red"
		LeftFlipper1P.image = "flippers-red"
		RightFlipperP.image = "flippers-red"
	End If

'	Randomize
	If PostColor = 3 then PostColor = RndInt(0,2)
	If PostColor = 1 Then
		RubberPost1.image = "rubber-post-t1-yellow"
		RubberPost2.image = "rubber-post-t1-yellow"
		RubberPost5.image = "rubber-post-t1-yellow"
		RubberPost6.image = "rubber-post-t1-yellow"
		RubberPost9.image = "rubber-post-t1-yellow"
		RubberPost10.image = "rubber-post-t1-yellow"
		RubberPost11.image = "rubber-post-t1-yellow"
	Elseif PostColor = 2 Then
		RubberPost1.image = "rubber-post-t1-orange"
		RubberPost2.image = "rubber-post-t1-orange"
		RubberPost5.image = "rubber-post-t1-orange"
		RubberPost6.image = "rubber-post-t1-orange"
		RubberPost9.image = "rubber-post-t1-orange"
		RubberPost10.image = "rubber-post-t1-orange"
		RubberPost11.image = "rubber-post-t1-orange"
	End If

	Dim x
'	Randomize
	If RubberColor = 2 then RubberColor = RndInt(0,1)
	If RubberColor = 1 then
		for each x in vRubbers:x.material = "Rubber White":next
	End If

	If UnionJackMod = 1 then
		sw20.image = "targetT1Small_UJ"
		sw21.image = "targetT1Small_UJ"
		sw22.image = "targetT1Small_UJ"
		sw38p.image = "targetT1Round_UJ"
		sw43p.image = "targetT1Round_UJ"
	End If

	Dim xx
	If TNTMod = 1 then
'		for each xx in GITNTBlue: xx.color = RGB (0,0,255):xx.intensity = 150: next
'		for each xx in GITNTBlue1: xx.color = RGB (0,0,255): next
		for each xx in GITNT:xx.intensity = 20: next
		FR9.opacity = 2500
		FR10.opacity = 2500
		bulb5.visible = 1
		bulb6.visible = 1
		bulb7.visible = 1
		bulb8.visible = 1
	End If

	Dim xxx
	for each xxx in LexanWashers:xxx.visible = Washers:next

	If VRRoom <> 0 Then
		For Each x in VRFloatyLights
			x.BulbHaloHeight = 0.1
		Next
	End If

End Sub

' save the insert intensities so they can be updated when GI is off
InitInsertIntensities
Sub InitInsertIntensities
	dim bulb
	for each bulb in Inserts
		bulb.uservalue = bulb.intensity
	next
End sub


'******************************************************
'* SOLENOIDS ******************************************
'******************************************************

SolCallback(1)	= "SolTrough"                           				'6-ball lockout							(1L)
SolCallback(2)	= "SolRelease"                     						'ball eject
SolCallback(3)  = "AutoLaunch"                          				'autolaunch                           	(3L)
SolCallback(4)	= "ExitVUK"                             				'VUK                                  	(4L)
SolCallback(5)	= "ExitScoop"                           				'LeftScoop.                           	(5L)
SolCallback(6)	= "SolEject"                            				'Top Left Eject                       	(6L)
SolCallback(8)	= "vpmSolSound SoundFX(""Knocker_1"",DOFKnocker),"		'Knocker							  	(8L)
'SolCallback(9)                                         				'Shaker motor                         	(09)    
'SolCallback(10)														'L/R Relay                            	(10)
SolCallback(11) = "GIRelay"                             				'GI Relay                             	(11)
SolCallback(12) = "Diverter"                            				'Top Diverter                         	(12)
SolCallback(13) = "PropellerMove"                       				'Airplane propellers                  	(13)
SolCallBack(14) = "MirrorMove"                          				'Mirror Motor                        	(14)
SolCallBack(15) = "FlashSol15"					   						'Flashlamp X1 Mirror                  	(15)
SolCallback(17) = "SolBumper1"                          				'Top Bumper                           	(17)
SolCallback(18) = "SolBumper2"                          				'Center bumper                        	(18)
SolCallback(19) = "SolBumper3"                          				'Right bumper                         	(19)
SolCallback(20) = "SolLSling"                           				'Left Slinghshot                      	(20)
SolCallback(21) = "SolRSling"                           				'Right Slingshot                      	(21)
SolCallback(23) = "SolEnableFlips"										'Flipper Board
SolCallBack(25) = "Setlamp 125,"					    				'Flashlamp X4 Bottom Arch L/R         	(1R)
SolCallBack(26) = "Flash26"												'Flashlamp X4 Upper Right Corner		(2R)
SolCallBack(27) = "Flash27"  											'Flashlamp X2 Left Scoop             	(3R)
SolCallBack(28) = "FlashSol28"											'Flashlamp X2 Top Eject					(4R)
SolCallBack(29) = "Setlamp 129,"  										'Flashlamp X4 Bumpers Hot Dog			(5R)
SolCallBack(30) = "Flash30" 											'Flashlamp X4 Back Panel				(6R)
SolCallBack(31) = "FlashSol31"					 						'Flashlamp X2 Lower Right Hot Dogs		(7R)
SolCallBack(32) = "Flash32"     										'Flashlamp X4 Top Hot Dogs				(8R)
SolCallback(46) = "SolRFlipper"                         				'Right Flipper
'SolCallback(47) = "SolULFlipper"                        				'Upper Left Flipper
SolCallback(48) = "SolLFlipper"                         				'Left Flipper
SolCallback(51) = "BlinderMove"                         				'Blinder Motor

'******************************************************
'* TIMERS *********************************************
'******************************************************
Sub FrameTimer_Timer()
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
	CheckBallLocations
End Sub

Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate					'update rolling sounds
	DoSTAnim						'Animations
End Sub


'******************************************************
'* Ball location checks
'******************************************************

ReDim bBallInTrough(tnob)
Sub CheckBallLocations
	Dim b
	For b = 0 to UBound(gBOT)
		'Check if ball is in the trough
		If InRect(gBOT(b).X, gBOT(b).Y, 851,1924,467,2177,405,2120,851,1825) Then
			bBallInTrough(b) = True
		Else
			bBallInTrough(b) = False
		End If
		'Check for narnia balls
		If gBOT(b).z < -200 Then
			'msgbox "Ball " &b& " in Narnia X: " & gBOT(b).x &" Y: "&gBOT(b).y & " Z: "&gBOT(b).z
			'debug.print "Move narnia ball ("& gBOT(b).x &" Y: "&gBOT(b).y & " Z: "&gBOT(b).z&") to left scoop"
			gBOT(b).x = 98
			gBOT(b).y = 1055
			gBOT(b).z = -30
		end if
	Next
End Sub


'******************************************************
'* TROUGH *********************************************
'******************************************************

Sub sw14_Hit   : Controller.Switch(14) = 1 : UpdateTrough : End Sub
Sub sw14_UnHit : Controller.Switch(14) = 0 : UpdateTrough : End Sub
Sub sw13_Hit   : Controller.Switch(13) = 1 : UpdateTrough : End Sub
Sub sw13_UnHit : Controller.Switch(13) = 0 : UpdateTrough : End Sub
Sub sw12_Hit   : Controller.Switch(12) = 1 : UpdateTrough : End Sub
Sub sw12_UnHit : Controller.Switch(12) = 0 : UpdateTrough : End Sub
Sub sw11_Hit   : Controller.Switch(11) = 1 : UpdateTrough : End Sub
Sub sw11_UnHit : Controller.Switch(11) = 0 : UpdateTrough : End Sub
Sub sw10_Hit   : Controller.Switch(10) = 1 : UpdateTrough : End Sub
Sub sw10_UnHit : Controller.Switch(10) = 0 : UpdateTrough : End Sub
Sub sw9_Hit    : Controller.Switch(9)  = 1 : UpdateTrough : RandomSoundDrain sw9 : End Sub
Sub sw9_UnHit  : Controller.Switch(9)  = 0 : UpdateTrough : End Sub

Sub UpdateTrough
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer
	If sw14.BallCntOver = 0 Then sw13.kick 57, 10
	If sw13.BallCntOver = 0 Then sw12.kick 57, 10
	If sw12.BallCntOver = 0 Then sw11.kick 57, 10
	If sw11.BallCntOver = 0 Then sw10.kick 57, 10
	If sw10.BallCntOver = 0 Then sw9.kick 57, 10
	Me.Enabled = 0
End Sub

'******************************************************
' DRAIN & RELEASE
'******************************************************

Sub SolTrough(enabled)
	If enabled Then 
		sw14.kick 57, 20
		Controller.Switch(15) = 1
	End If
End Sub

Sub SolRelease(enabled)
	If enabled Then 
		sw15.kick 57, 10		
		Controller.Switch(15) = 0
		RandomSoundBallRelease sw15
	End If
End Sub

'******************************************************
'* AUTOLAUNCH *****************************************
'******************************************************

Dim plungerIM
Const IMPowerSetting = 48
Const IMTime = 0.5
Set plungerIM = New cvpmImpulseP
With plungerIM
	.InitImpulseP swPlunger, IMPowerSetting, IMTime
	.Switch 16
	.Random 1.5
	'.InitExitSnd "fx_launch", SoundFX("fx_solon",DOFContactors)
	.CreateEvents "plungerIM"
End With

Sub AutoLaunch(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
		SoundAutoPlunger
	Else
		SoundAutoPlungerDown
	End If
End Sub

'Sub ShootTrigger_Hit
'	If ActiveBall.Z > 30  Then			'ball is flying
'		vpmTimer.AddTimer 150, "BallHitSound"
'	End If
'End Sub

'Sub BallHitSound(dummy):PlaySound "fx_balldrop":End Sub

'******************************************************
'* MIRROR, SKILLSHOT AND VUK THROUGH HOLES ************
'******************************************************
Dim inSubway:inSubway=0

Sub sw42_Hit
	vpmTimer.PulseSw 42
	WireRampOn True
	SoundSaucerLock
End Sub

Sub sw41_Hit
	vpmTimer.PulseSw 41
	WireRampOn True
	inSubway=1
End Sub

Sub GrabbyKicker_Hit
	SoundSaucerLock
End Sub

'******************************************************
'* VUK ************************************************
'******************************************************
Dim BIK:BIK = 0

Sub exitVUK(enabled)
	If enabled then
		sw19.kick 0, 40, 1.56
		Controller.Switch(19) = 0
		If BIK > 0 Then
			SoundSaucerKick 1, sw19
		Else
			SoundSaucerKick 0, sw19
		End If
	End If
End Sub

Sub sw19_hit
	BIK = BIK + 1
	RandomSoundWall()
	If inSubway=0 Then SoundSaucerLock
	inSubway=0
	Controller.Switch(19) = 1
End Sub

Sub sw19_Unhit
	BIK = BIK - 1
	WireRampOn False
End Sub

Sub Vukstop_Hit
	PlaySoundAtBall "Metal_touch_3"
End Sub

'******************************************************
'* LEFT SCOOP *****************************************
'******************************************************
Dim BIS:BIS = 0

Sub ExitScoop(enabled)
	If enabled then
		sw23.Kick 0, 28, 1.56
		Controller.Switch(23) = 0
'		vpmTimer.AddTimer 2000, "sw23.enabled=1 '"
		If BIS > 0 Then
			SoundSaucerKick 1, sw23
		Else
			SoundSaucerKick 0, sw23
		End If
	End If
End Sub

sub sw23_hit
	BIS = BIS + 1
	Dim opt
'	Randomize
	opt= RndInt(0,1)
	If Scoop_kick = 2 then
		ramp12.collidable=opt
		ramp13.collidable=opt
		If opt=1 then
			ramp17.collidable=0
			ramp18.collidable=0
		Else
			ramp17.collidable=1
			ramp18.collidable=1
		End If
	End If
	If Scoop_kick = 1 then
		ramp12.collidable=0
		ramp13.collidable=0
		ramp17.collidable=1
		ramp18.collidable=1
	End If

	SoundSaucerLock
	Controller.Switch(23) = 1
'	sw23.enabled= 0
End Sub

Sub sw23_unhit
	BIS = BIS - 1
End Sub

'******************************************************
'* TOP TROUGH EJECT **********************************
'******************************************************

Sub SolEject(enabled)
	If enabled then
		sw47.kick 180,10
		SoundSaucerKick 1, sw47
		Controller.Switch(47) = 0
	End If
End Sub

Sub sw47_Hit
	SoundSaucerLock
	Controller.Switch(47) = 1
End Sub

'******************************************************
'* TOP DIVERTER ***************************************
'******************************************************

Sub Diverter(enabled)
	If enabled then
		PlaysoundAt SoundFX("fx_diverter",DOFContactors), Backwall_Up
		TopDiverter.IsDropped = 0
	Else
		PlaysoundAt SoundFX("fx_diverter",DOFContactors), Backwall_Up
		TopDiverter.IsDropped = 1
	End If
End Sub

'******************************************************
'* PROPELLERS ANIMATION *******************************
'******************************************************

Dim discAngle:discAngle=0
Dim stepAngle, stopRotation

Sub PropellerMove(enabled)
	Dim xx
	If Enabled Then 
		PropellerTimer.Enabled = 1
		PlaysoundAtLevelStaticLoop SoundFX("fx_propellers_on",DOFGear), 0.25, Airplane
		stepAngle=15
		stopRotation=0
		for each xx in GITNT:xx.state = 2: next
	Else
		StopSound "fx_propellers_on"
		PlaySoundAt SoundFX("fx_propellers_off",0), Airplane
		stopRotation=1
		for each xx in GITNT:xx.state = 0: next
	End If
End Sub

Sub PropellerTimer_Timer()
	discAngle = discAngle + stepAngle
	If discAngle >= 360 Then
		discAngle = discAngle - 360
	End If
	Propeller1.RotZ = 360 - discAngle
	Propeller2.RotZ = 360 - discAngle
	If stopRotation Then
		stepAngle = stepAngle - 0.2
		If stepAngle <= 0 Then
			PropellerTimer.Enabled 	= 0
		End If
	End If
End Sub

'******************************************************
'* MIRROR ANIMATION ***********************************
'******************************************************

Dim MirrorDown:MirrorDown = 0

Sub MirrorMove(Enabled)
	If MirrorP.Z <= 3 then
		If Enabled then
			MirrorDown = False
			PlaysoundAtLevelStaticLoop SoundFX("fx_motor",DOFGear), 1, MirrorP
		Else
			StopSound "fx_motor"
		End If
	End If
	If MirrorP.Z >= 137 then
		If Enabled then
			MirrorDown = True
			PlaysoundAtLevelStaticLoop SoundFX("fx_motor",DOFGear), 1, MirrorP
		Else
			StopSound "fx_motor"
		End If
	End If
End Sub

Sub MirrorTimer_Timer()
	If MirrorDown = True and MirrorP.Z >= 0 then 
		MirrorP.Z = MirrorP.Z - 2
		If MirrorP.Z < 25 Then sw32.isdropped=1
	End If
	If MirrorDown = False and MirrorP.Z <= 140 then 
		MirrorP.Z = MirrorP.Z + 2
		If MirrorP.Z > 25 Then sw32.isdropped=0
	End If
	
	If MirrorP.Z >= 137 then
		Controller.Switch(28) = 1':sw32.isdropped=0
	Else
		Controller.Switch(28) = 0
	End If
	If MirrorP.Z <= 3 then
		Controller.Switch(31) = 1':sw32.isdropped=1
	Else
		Controller.Switch(31) = 0
	End If
End Sub

'******************************************************
'* MIRROR SHAKE CODE BASED ON JP'S ********************
'******************************************************

Sub sw32_Hit
	PlaySoundAt "fx_mirror_hit", MirrorP
	ShakeMirror
End Sub

Dim MirrorPos

Sub ShakeMirror
	Dim finalspeed
	finalspeed=INT(1 + SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely))
	If finalspeed > 4 Then vpmTimer.PulseSw 32
	If finalspeed >= 8 then
		MirrorPos = 8
	Else
		MirrorPos = INT(9 - 8 / finalspeed)
	End If
	MirrorShakeTimer.Enabled = 1
End Sub

Sub MirrorShakeTimer_Timer
	MirrorP.TransX = MirrorPos
	If MirrorPos = 0 Then MirrorShakeTimer.Enabled = 0:Exit Sub
	If MirrorPos < 0 Then
		MirrorPos = ABS(MirrorPos) - 1
	Else
		MirrorPos = - MirrorPos + 1
	End If
End Sub

'******************************************************
'* BLINDER ANIMATION **********************************
'******************************************************

Dim StatusBlinder:StatusBlinder=1      'initial status is 1 and timers are off (no movement)
BlinderForward.enabled=0
BlinderBack.enabled=0

Sub BlinderMove(enabled)
	If enabled then
		If StatusBlinder=1 then   
			BlinderForward.enabled=1
			BlinderBack.enabled=0
			PlaysoundAtVol "fx_blinder", BlinderP1, 0.5
		End If
	End If
	If not enabled then
		If StatusBlinder=2 then
			BlinderBack.enabled=1
			BlinderForward.enabled=0
			PlaysoundAtVol "fx_blinder", BlinderP1, 0.5
		End If
	End If
End Sub

Sub BlinderForward_timer
	BlinderP2.rotY= BlinderP2.rotY - 3
	if BlinderP2.rotY <=-52 then blinderP1.rotY= blinderP1.rotY - 3
	if BlinderP1.rotY <=-48 then BlinderForward.enabled=0:BlinderCollide.Collidable=True:StatusBlinder=2
End Sub

Sub BlinderBack_timer
	BlinderP2.rotY= BlinderP2.rotY + 3
	if BlinderP2.rotY >=-52 then blinderP1.rotY= blinderP1.rotY + 3
	if BlinderP1.rotY >=0 then BlinderP2.rotY=0:BlinderBack.enabled=0:BlinderCollide.Collidable=False:StatusBlinder=1
End Sub

'******************************************************
'* SLINGSHOTS *****************************************
'******************************************************

Dim Lstep, RStep

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 17
	RandomSoundSlingshotLeft sling1
	LSling.Visible = 0
	LSling1.Visible = 1
	sling1.TransZ = -23
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
End Sub

Sub solLSling(enabled)
	If Enabled Then
		LeftSlingShot.SlingshotThreshold = 2500
	Else
		LeftSlingShot.SlingshotThreshold = SlingDefault
	End If
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:sling1.TransZ = -12
		Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:sling1.TransZ = 0:Me.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 18
	RandomSoundSlingshotRight sling2
	RSling.Visible = 0
	RSling1.Visible = 1
	sling2.TransZ = -23
	RStep = 0
	RightSlingShot.TimerEnabled = 1
End Sub

Sub solRSling(enabled)
	If Enabled Then
		RightSlingShot.SlingshotThreshold = 2500
	Else
		RightSlingShot.SlingshotThreshold = SlingDefault
	End If
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 2:RSLing1.Visible = 0:RSLing2.Visible = 1:sling2.TransZ = -12
		Case 3:RSLing2.Visible = 0:RSLing.Visible = 1:sling2.TransZ = 0:Me.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

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

'	Public Sub Report()         'debug, reports all coords in tbPL.text
'		If not debugOn then exit sub
'		dim a1, a2 : a1 = ModIn : a2 = ModOut
'		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
'		TBPout.text = str
'	End Sub


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
'			debug.print " BallPos=" & BallPos &" Angle=" & Angle 
'			debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
'			debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
'			debug.print " " 
		End If
	End Sub

End Class

'************************************************
' BUMPER RING AND SKIRT ANIMATIONS **************
'************************************************

Dim dirRing1:dirRing1 = -1
Dim dirRing2:dirRing2 = -1
Dim dirRing3:dirRing3 = -1

Sub Bumper1_Hit
	vpmTimer.PulseSw 49
	RandomSoundBumperTop Bumper1
	BS1.rotx=skirtAX(me,Activeball)+90
	BS1.roty=skirtAY(me,Activeball)-80
	BSkirt1.Enabled = 1
	Bumper1.TimerEnabled = 1
End Sub

Sub BSkirt1_Timer
	BS1.rotx=90
	BS1.roty=-80
	me.Enabled = 0
End Sub

Sub Bumper1_timer()
	BR1.Z = BR1.Z + (5 * dirRing1)
	If BR1.Z <= -40 Then dirRing1 = 1
	If BR1.Z >= -5 Then
		dirRing1 = -1
		BR1.Z = -5
		Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper2_Hit
	vpmTimer.PulseSw 50
	RandomSoundBumperMiddle Bumper2
	BS2.rotx=skirtAX(me,Activeball)+90
	BS2.roty=skirtAY(me,Activeball)-60
	BSkirt2.Enabled = 1
	Bumper2.TimerEnabled = 1
End Sub

Sub BSkirt2_Timer
	BS2.rotx=90
	BS2.roty=-60
	me.Enabled = 0
End Sub

Sub Bumper2_timer()
	BR2.Z = BR2.Z + (5 * dirRing2)
	If BR2.Z <= -40 Then dirRing2 = 1
	If BR2.Z >= -5 Then
		dirRing2 = -1
		BR2.Z = -5
		Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper3_Hit
	vpmTimer.PulseSw 51
	RandomSoundBumperBottom Bumper3
	BS3.rotx=skirtAX(me,Activeball)+90
	BS3.roty=skirtAY(me,Activeball)-90
	BSkirt3.Enabled = 1
	Bumper3.TimerEnabled = 1
End Sub

Sub BSkirt3_Timer
	BS3.rotx=90
	BS3.roty=-90
	me.Enabled = 0
End Sub

Sub Bumper3_timer()
	BR3.Z = BR3.Z + (5 * dirRing3)
	If BR3.Z <= -40 Then dirRing3 = 1
	If BR3.Z >= -5 Then
		dirRing3 = -1
		BR3.Z = -5
		Me.TimerEnabled = 0
	End If
End Sub

Sub solBumper1 (enabled)
'	If enabled then
'		Bumper1.TimerEnabled = 1
'	End If
End Sub

Sub solBumper2 (enabled)
'	If enabled then
'		Bumper2.TimerEnabled = 1
'	End If
End Sub

Sub solBumper3 (enabled)
'	If enabled then
'		Bumper3.TimerEnabled = 1
'	End If
End Sub

'******************************************************
'			SKIRT ANIMATION FUNCTIONS
'******************************************************
' NOTE: set bumper object timer to around 150-175 in order to be able
' to actually see the animaation, adjust to your liking

Const SkirtTilt=10		'angle of skirt tilting in degrees

Function SkirtAX(bumper, bumperball)
	skirtAX=cos(skirtA(bumper,bumperball))*(SkirtTilt)		'x component of angle
	if (bumper.y<bumperball.y) then	skirtAX=skirtAX*-1	'adjust for ball hit bottom half
End Function

Function SkirtAY(bumper, bumperball)
	skirtAY=sin(skirtA(bumper,bumperball))*(SkirtTilt)		'y component of angle
	if (bumper.x>bumperball.x) then	skirtAY=skirtAY*-1	'adjust for ball hit left half
End Function

Function SkirtA(bumper, bumperball)
	dim hitx, hity, dx, dy
	hitx=bumperball.x
	hity=bumperball.y

	dy=Abs(hity-bumper.y)					'y offset ball at hit to center of bumper
	if dy=0 then dy=0.0000001
	dx=Abs(hitx-bumper.x)					'x offset ball at hit to center of bumper
	skirtA=(atn(dx/dy)) '/(PI/180)			'angle in radians to ball from center of Bumper1
End Function


'******************************************************
'* STANDUP TARGETS ************************************
'******************************************************

'* Silverball Target **********************************
Sub sw24_Hit
	TargetBouncer Activeball, 1.1
	vpmTimer.PulseSw 24
	sw24p.transx = -10
	PlaySoundAt "Ball_Collide_7", sw24p
	Me.TimerEnabled = 1
End Sub

Sub sw24_Timer:sw24p.transx = 0:Me.TimerEnabled = 0:End Sub

'* Left Ramp Left Standup Target **********************
'Sub sw20_Hit:vpmTimer.PulseSw 20:PlayTargetSound:TargetBouncer Activeball, 1:End Sub
Sub sw20_Hit:STHit 20:End Sub
'* Left Ramp Right Standup Target *********************
'Sub sw21_Hit:vpmTimer.PulseSw 21:PlayTargetSound:TargetBouncer Activeball, 1:End Sub
Sub sw21_Hit:STHit 21:End Sub
'* Right Ramp Standup Target **************************
'Sub sw22_Hit:vpmTimer.PulseSw 22:PlayTargetSound:TargetBouncer Activeball, 1:End Sub
Sub sw22_Hit:STHit 22:End Sub

'* Middle Standup Target ******************************
Sub sw38_Hit:STHit 38:End Sub' vpmTimer.PulseSw 38:End Sub

'* Targets Bank Left **********************************
Sub sw25_Hit:STHit 25:End Sub' vpmTimer.PulseSw 25:End Sub
Sub sw26_Hit:STHit 26:End Sub' vpmTimer.PulseSw 26:End Sub
Sub sw27_Hit:STHit 27:End Sub' vpmTimer.PulseSw 27:End Sub

'* Targets Bank Right *********************************
Sub sw33_Hit:STHit 33:End Sub' vpmTimer.PulseSw 33:End Sub
Sub sw34_Hit:STHit 34:End Sub' vpmTimer.PulseSw 34:End Sub
Sub sw35_Hit:STHit 35:End Sub' vpmTimer.PulseSw 35:End Sub

'* Captive Ball Standup Target ************************
Sub sw43_Hit:STHit 43:End Sub' vpmTimer.PulseSw 43:End Sub

'Sub TargetO_Hit()
'	TargetBouncer Activeball, 1
'End Sub

'******************************************************
'		STAND-UP TARGET INITIALIZATION
'******************************************************

'Define a variable for each stand-up target
Dim ST20, ST21, ST22, PT24	'Silver Ball Targets
Dim ST25, ST26, ST27		'Left Target Bank
Dim ST33, ST34, ST35		'Right Target Bank
Dim ST38, ST43				'More Time, Locked Ball targets

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, switch)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
' 
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts


ST20 = Array(sw20, sw20p,20, 0)
ST21 = Array(sw21, sw21p,21, 0)
ST22 = Array(sw22, sw22p,22, 0)
PT24 = Array(sw24,24, 0)

ST25 = Array(sw25, sw25p,25, 0)
ST26 = Array(sw26, sw26p,26, 0)
ST27 = Array(sw27, sw27p,27, 0)
ST33 = Array(sw33, sw33p,33, 0)
ST34 = Array(sw34, sw34p,34, 0)
ST35 = Array(sw35, sw35p,35, 0)
ST38 = Array(sw38, sw38p,38, 0)
ST43 = Array(sw43, sw43p,43, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST20, ST21, ST22, ST25, ST26, ST27, ST33, ST34, ST35, ST38, ST43)
Dim STArray0, STArray1, STArray2, STArray3
Redim STArray0(UBound(STArray)), STArray1(UBound(STArray)), STArray2(UBound(STArray)), STArray3(UBound(STArray))

Dim STIdx
For STIdx = 0 to UBound(STArray)
   Set STArray0(STIdx) = STArray(STIdx)(0)
   Set STArray1(STIdx) = STArray(STIdx)(1)
   STArray2(STIdx) = STArray(STIdx)(2)
   STArray3(STIdx) = STArray(STIdx)(3)
Next

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
'	STArray(i)(3) =  STCheckHit(Activeball,STArray(i)(0))
	STArray3(i) =  STCheckHit(Activeball,STArray0(i))

'	If STArray(i)(3) <> 0 Then
	If STArray3(i) <> 0 Then
'		DTBallPhysics Activeball, STArray(i)(0).orientation, STMass
		DTBallPhysics Activeball, STArray0(i).orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
'		If STArray(i)(2) = switch Then STArrayID = i:Exit Function 
		If STArray2(i) = switch Then STArrayID = i:Exit Function 
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
'		STArray(i)(3) = STAnimate(STArray(i)(0),STArray(i)(1),STArray(i)(2),STArray(i)(3))
		STArray3(i) = STAnimate(STArray0(i),STArray1(i),STArray2(i),STArray3(i))
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
		vpmTimer.PulseSw switch
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

'******************************************************
'		END STAND-UP TARGETS
'******************************************************

'******************************************************
'* ROLLOVER SWITCHES **********************************
'******************************************************

Sub sw16_Hit():Controller.Switch(16) = 1: BIPL = True : End Sub
Sub sw16_Unhit:Controller.Switch(16) = 0: BIPL = False : End Sub
Sub sw29_Hit():Controller.Switch(29) = 1:End Sub
Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub
Sub sw36_Hit()
'	debug.print "Spin in: "& activeball.AngMomZ
'	debug.print "Speed in:"& activeball.vely
	activeball.AngMomZ = activeball.AngMomZ * RndNum(2,4)
	If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
	If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
	If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
	If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)
	if abs(activeball.vely) > 10 then activeball.vely = 0.8 * activeball.vely
	if abs(activeball.vely) > 15 then activeball.vely = 0.8 * activeball.vely
	if activeball.vely > 16 then activeball.vely = RndNum(14,16)
	if activeball.vely < -16 then activeball.vely = RndNum(-14,-16)
	Controller.Switch(36) = 1
'	debug.print "Spin out: "& activeball.AngMomZ
'	debug.print "Speed out:"& activeball.vely
End Sub
Sub sw36_UnHit:Controller.Switch(36) = 0:End Sub
Sub sw37_Hit()
'	debug.print "Spin in: "& activeball.AngMomZ
'	debug.print "Speed in:"& activeball.vely
	activeball.AngMomZ = activeball.AngMomZ * RndNum(2,4)
	If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
	If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
	If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
	If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)
	if abs(activeball.vely) > 10 then activeball.vely = 0.8 * activeball.vely
	if abs(activeball.vely) > 15 then activeball.vely = 0.8 * activeball.vely
	if activeball.vely > 16 then activeball.vely = RndNum(14,16)
	if activeball.vely < -16 then activeball.vely = RndNum(-14,-16)
	Controller.Switch(37) = 1
'	debug.print "Spin out: "& activeball.AngMomZ
'	debug.print "Speed out:"& activeball.vely
End Sub
Sub sw37_UnHit:Controller.Switch(37) = 0:End Sub
Sub sw48_Hit():Controller.Switch(48) = 1:End Sub
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub
Sub sw55_Hit():Controller.Switch(55) = 1:End Sub
Sub sw55_UnHit:Controller.Switch(55) = 0:End Sub
Sub sw56_Hit():Controller.Switch(56) = 1:End Sub
Sub sw56_UnHit:Controller.Switch(56) = 0:End Sub

'******************************************************
'* RAMP SWITCHES **************************************
'******************************************************

Sub sw30_Hit():Controller.Switch(30) = 1:If ActiveBall.VelX > 0 Then PlaySoundAtLevelActiveBall ("Ramp_Enter"), VolumeDial:WireRampOn False:bsRampOn:End If:End Sub	'ball is going up
Sub sw30_UnHit:Controller.Switch(30) = 0:If ActiveBall.VelX < 0 Then StopSound "Ramp_Enter":WireRampOff:End If:End Sub		'ball is going down
Sub sw57_Hit():Controller.Switch(57) = 1:If ActiveBall.VelY < 0 Then PlaySoundAtLevelActiveBall ("Ramp_Enter"), VolumeDial:WireRampOn False:bsRampOn:End If:End Sub	'ball is going up
Sub sw57_UnHit:Controller.Switch(57) = 0:If ActiveBall.VelY > 0 Then StopSound "Ramp_Enter":WireRampOff:End If:End Sub		'ball is going down
Sub sw58_Hit():Controller.Switch(58) = 1:flipper58.rotatetoend:bsRampOnWire:End Sub
Sub sw58_UnHit:Controller.Switch(58) = 0:flipper58.rotatetostart:End Sub
Sub sw62_Hit():Controller.Switch(62) = 1:flipper62.rotatetoend:bsRampOnWire:End Sub
Sub sw62_UnHit:Controller.Switch(62) = 0:flipper62.rotatetostart:End Sub

Sub VUKtop_Hit:bsRampOnWire:End Sub
Sub RailEndTrigger_Hit:PlaySoundAtLevelActiveBall "WireRamp_Stop", 0.1:End Sub
Sub RailEndTrigger_UnHit:activeball.AngMomZ = -40:End Sub
Sub RailEndTrigger2_Hit:PlaySoundAtLevelActiveBall "WireRamp_Stop", 0.1:End Sub
Sub RailEndTrigger2_UnHit:activeball.AngMomZ = 40:End Sub

'******************************************************
'* SPINNERS *******************************************
'******************************************************

Sub sw39_Spin:vpmTimer.PulseSw 39:SoundSpinner sw39:End Sub
Sub sw40_Spin:vpmTimer.PulseSw 40:SoundSpinner sw40:End Sub

'******************************************************
'* FLIPPERS *******************************************
'******************************************************
Const ReflipAngle = 20

Dim bFlippersEnabled : bFlippersEnabled=False

Sub SolEnableFlips(Enabled)
	bFlippersEnabled = Enabled
End Sub

Sub SolLFlipper(Enabled)
	If Enabled And bFlippersEnabled Then
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
	If Enabled And bFlippersEnabled Then
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

Sub SolULFlipper(Enabled)
	If Enabled And bFlippersEnabled Then
		LeftFlipper1.RotateToEnd
		If leftflipper1.currentangle < leftflipper1.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper1
		Else 
			SoundFlipperUpAttackLeft LeftFlipper1
			RandomSoundFlipperUpLeft LeftFlipper1
		End If		
	Else
		LeftFlipper1.RotateToStart
		If LeftFlipper1.currentangle < LeftFlipper1.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper1
		End If
		FlipperLeft1HitParm = FlipperUpSoundLevel
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


Sub LeftFlipper1_Collide(parm)
	LeftFlipper1Collide parm
End Sub

'******************************************************
'* REALTIME UPDATES ***********************************
'******************************************************

Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
	UpdateMechs
	RollingSound
End Sub

Sub UpdateMechs
	LeftFlipperP.RotZ = LeftFlipper.CurrentAngle
	LeftFlipper1P.RotZ = LeftFlipper1.CurrentAngle
	RightFlipperP.RotZ = RightFlipper.CurrentAngle
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperL1Sh.RotZ = LeftFlipper1.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
	sw57p.RotX = Spinner_LeftRamp.currentangle+90
	sw30p.RotX = Spinner_RightRamp.currentangle+90
	sw58p.RotZ = -Flipper58.currentangle
	sw62p.RotZ = -Flipper62.currentangle
End Sub



'******************************************************
'****  LAMPZ by nFozzy
'******************************************************

Sub SetLamp(aNr, aOn)
	Lampz.state(aNr) = abs(aOn)
End Sub

Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function

Dim NullFader
Set NullFader = New NullFadingObject
Dim Lampz
Set Lampz = New LampFader
Dim ModLampz : Set ModLampz = new NoFader2 ' NoFader2 requires .Update
InitLampsNF				  ' Setup lamp assignments
LampTimer.Interval =  - 1
LampTimer.Enabled = 1

Sub LampTimer_Timer()
	Dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		Next
	End If
	Lampz.Update2 'update (fading logic only)
	ModLampz.Update ' update (object updates only, pinmame itself sends the fading info)
End Sub

'dim FrameTime, InitFrameTime : InitFrameTime = 0
'LampTimer2.Interval = -1
'LampTimer2.Enabled = True
'Sub LampTimer2_Timer()
'	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
'	Lampz.Update 'updates on frametime (Object updates only)
'End Sub

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	If Lampz.UseFunc Then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub InsertIntensityUpdate(ByVal aLvl)
	if Lampz.UseFunc then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	dim bulb
	for each bulb in Inserts
		bulb.intensity = bulb.uservalue*(1 + aLvl*2)
	next
End Sub


Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	Dim x
	For x = 0 To 114
		Lampz.FadeSpeedUp(x) = 1 / 40
		Lampz.FadeSpeedDown(x) = 1 / 150
		Lampz.Modulate(x) = 1
	Next
	
	Dim xx
	For xx = 115 To 131
		Lampz.FadeSpeedUp(xx) = 1 / 2
		Lampz.FadeSpeedDown(xx) = 1 / 80
'		Lampz.Modulate(x) = 1
	Next

	'Adjust fading speeds (1 / full MS fading time)
'	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/2 : Lampz.FadeSpeedDown(x) = 1/10 : next
'	dim xx : for xx = 115 to 131 : Lampz.FadeSpeedUp(xx) = 1 : Lampz.FadeSpeedDown(xx) = 1/4 : next

	'GI speed
'	Lampz.FadeSpeedUp(100) = 1/4 : Lampz.FadeSpeedDown(100) = 1/16
'	Lampz.FadeSpeedUp(101) = 1/4 : Lampz.FadeSpeedDown(101) = 1/16
'	Lampz.FadeSpeedUp(102) = 1/4 : Lampz.FadeSpeedDown(102) = 1/16

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

					'GI

	Lampz.obj(100) = ColtoArray(GI)
	Lampz.state(100) = 0
	Lampz.Callback(100) = "DisableLighting primitive6, 0.1,"
	Lampz.Callback(100) = "DisableLighting primitive87, 0.3,"
	Lampz.obj(101) = ColtoArray(GICab)
	Lampz.state(101) = 0
	Lampz.Callback(102) = "InsertIntensityUpdate"

					'3D Inserts
		'No Text
	Lampz.MassAssign(24)= l24							'Silverball
	Lampz.MassAssign(24)= l24h
	Lampz.MassAssign(24)= TL24							'Silverball reflection flasher
	Lampz.Callback(24) = "DisableLighting p24, 100,"
	Lampz.MassAssign(25)= l25							'Target Arrows vvv
	Lampz.MassAssign(25)= TL25
	Lampz.MassAssign(25)= l25h
	Lampz.Callback(25) = "DisableLighting p25, 10,"
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= l26h
	Lampz.MassAssign(26)= TL26
	Lampz.Callback(26) = "DisableLighting p26, 5,"
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign(27)= l27h
	Lampz.MassAssign(27)= TL27
	Lampz.Callback(27) = "DisableLighting p27, 0,"
	Lampz.MassAssign(33)= l33
	Lampz.MassAssign(33)= l33h
	Lampz.MassAssign(33)= TL33
	Lampz.Callback(33) = "DisableLighting p33, 0,"
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(34)= l34h
	Lampz.MassAssign(34)= TL34
	Lampz.Callback(34) = "DisableLighting p34, 5,"
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(35)= l35h
	Lampz.MassAssign(35)= TL35
	Lampz.Callback(35) = "DisableLighting p35, 100,"
	Lampz.MassAssign(41)= l41							'T
	Lampz.MassAssign(41)= l41h
	Lampz.Callback(41) = "DisableLighting p41, 2,"
	Lampz.MassAssign(42)= l42							'O
	Lampz.MassAssign(42)= l42h
	Lampz.Callback(42) = "DisableLighting p42, 3,"
	Lampz.MassAssign(43)= l43							'M
	Lampz.MassAssign(43)= l43h
	Lampz.Callback(43) = "DisableLighting p43, 4,"
	Lampz.MassAssign(44)= l44							'M
	Lampz.MassAssign(44)= l44h
	Lampz.Callback(44) = "DisableLighting p44, 4,"
	Lampz.MassAssign(45)= l45							'Y
	Lampz.MassAssign(45)= l45h
	Lampz.Callback(45) = "DisableLighting p45, 5,"
	Lampz.MassAssign(56)= l56
'	Lampz.Callback(56) = "DisableLighting Airplane, 150,"
		'Printed Text
	Lampz.MassAssign(17)= l17							'JACKPOT
	Lampz.MassAssign(17)= l17h
	Lampz.Callback(17) = "DisableLighting p17, 1,"
	Lampz.MassAssign(18)= l18							'DOUBLE JACKPOT
	Lampz.MassAssign(18)= l18h
	Lampz.Callback(18) = "DisableLighting p18, 1,"
	Lampz.MassAssign(29)= l29							'SPINNER BONUS L
	Lampz.MassAssign(29)= l29h
	Lampz.Callback(29) = "DisableLighting p29, 1,"
	Lampz.MassAssign(30)= l30							'EXTRA BALL
	Lampz.MassAssign(30)= l30h
	Lampz.Callback(30) = "DisableLighting p30, 1,"
	Lampz.MassAssign(36)= l36							'MYSTERY
	Lampz.MassAssign(36)= l36h
	Lampz.Callback(36) = "DisableLighting p36, 5,"
	Lampz.MassAssign(38)= l38							'MORE TIME
	Lampz.MassAssign(38)= l38h
	Lampz.MassAssign(38)= TL38
	Lampz.Callback(38) = "DisableLighting p38, 10,"
	Lampz.MassAssign(47)= l47							'MULTIBALL
	Lampz.MassAssign(47)= l47h
	Lampz.Callback(47) = "DisableLighting p47, 5,"
	Lampz.MassAssign(48)= l48							'SHOOT AGAIN
	Lampz.MassAssign(48)= l48h
	Lampz.Callback(48) = "DisableLighting p48, 5,"
	Lampz.MassAssign(52)= l52							'SPINNER BONUS R
	Lampz.MassAssign(52)= l52h
	Lampz.Callback(52) = "DisableLighting p52, 1,"
	Lampz.MassAssign(54)= l54							'HOLIDAY CAMP
	Lampz.MassAssign(54)= l54h
	Lampz.Callback(54) = "DisableLighting p54, 5,"
	Lampz.MassAssign(55)= l55							'DOUBLE SPINNER VALUE L
	Lampz.MassAssign(55)= l55h
	Lampz.Callback(55) = "DisableLighting p55, 5,"
	Lampz.MassAssign(55)= l55a							'DOUBLE SPINNER VALUE R
	Lampz.MassAssign(55)= l55ah
	Lampz.Callback(55) = "DisableLighting p55a, 5,"
	Lampz.MassAssign(62)= l62							'MULTI BALL
	Lampz.MassAssign(62)= l62h
	Lampz.Callback(62) = "DisableLighting p62, 5,"
		'Sticker Text
	Lampz.MassAssign(46)= l46							'SPECIAL L
	Lampz.MassAssign(46)= l46a							'SPECIAL R
	Lampz.MassAssign(46)= l46h
	Lampz.MassAssign(46)= l46ah
	Lampz.MassAssign(46)= TL46
	Lampz.MassAssign(46)= TL46a
	Lampz.Callback(46) = "DisableLighting p46, 1,"
	Lampz.Callback(46) = "DisableLighting p46a, 1,"
	Lampz.MassAssign(57)= l57							'ACID QUEEN Left
	Lampz.MassAssign(57)= l57h
	Lampz.Callback(57) = "DisableLighting p57, 1,"
	Lampz.MassAssign(58)= l58							'Top
	Lampz.MassAssign(58)= l58h
	Lampz.Callback(58) = "DisableLighting p58, 1,"
	Lampz.MassAssign(59)= l59							'Right
	Lampz.MassAssign(59)= l59h
	Lampz.Callback(59) = "DisableLighting p59, 1,"
		'Bumpers
	Lampz.MassAssign(49)= l49
	Lampz.MassAssign(49)= l49b
	Lampz.MassAssign(49)= l49c
	Lampz.MassAssign(49)= l49d
	Lampz.Callback(49) = "DisableLighting Bumper1_cap, 0.2,"
	Lampz.MassAssign(50)= l50
	Lampz.MassAssign(50)= l50b
	Lampz.MassAssign(50)= l50c
	Lampz.MassAssign(50)= l50d
	Lampz.Callback(50) = "DisableLighting Bumper2_cap, 0.2,"
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= l51b
	Lampz.MassAssign(51)= l51c
	Lampz.MassAssign(51)= l51d
	Lampz.Callback(51) = "DisableLighting Bumper3_cap, 0.2,"
		'Ramp Signs
	Lampz.MassAssign(39)= l39
	Lampz.MassAssign(39)= l39a
	Lampz.Callback(39) = "DisableLighting Bulb2, 1,"
	Lampz.MassAssign(60)= l60
	Lampz.MassAssign(60)= l60a
	Lampz.Callback(60) = "DisableLighting Bulb1, 1,"
	Lampz.MassAssign(40)= l40
	Lampz.MassAssign(40)= l40a
	Lampz.MassAssign(40)= l40b
	Lampz.Callback(40) = "DisableLighting Bulb3, 1,"
	Lampz.MassAssign(61)= l61
	Lampz.MassAssign(61)= l61a
	Lampz.MassAssign(61)= l61b
	Lampz.Callback(61) = "DisableLighting Bulb4, 1,"

				'Skitso Inserts

	Lampz.MassAssign(9)= l9		'SKILL SHOT
	Lampz.MassAssign(32)= l32	'GENIUS-WOW (All 1 light)
		'Missions Flag
	Lampz.MassAssign(2)= l2		'CHRISTMAS
	Lampz.MassAssign(3)= l3		'COUSIN KEVIN
	Lampz.MassAssign(4)= l4		'HOLIDAY CAMP
	Lampz.MassAssign(5)= l5		'LITE EXTRA BALL
	Lampz.MassAssign(6)= l6		'SILVER BALL
	Lampz.MassAssign(7)= l7		'CAPTAIN WALKER
	Lampz.MassAssign(8)= l8		'PINBALL WIZARD
	Lampz.MassAssign(11)= l11	'SMASH THE MIRROR
	Lampz.MassAssign(12)= l12	'FIDDLE ABOUT
	Lampz.MassAssign(13)= l13	'ACID QUEEN
	Lampz.MassAssign(14)= l14	'THERE'S A DOCTOR
	Lampz.MassAssign(15)= l15	'TOMMY SCORING
	Lampz.MassAssign(16)= l16	'SALLY SIMPSON
		'Flags
	Lampz.MassAssign(31)= l31	'LITE L
	Lampz.MassAssign(53)= l53	'LITE R
	Lampz.MassAssign(63)= l63	'COLLECT
		'SilverBalls
	Lampz.MassAssign(20)= l20	'Left
	Lampz.MassAssign(20)= TL20
	Lampz.MassAssign(21)= l21	'Center
	Lampz.MassAssign(21)= TL21
	Lampz.MassAssign(22)= l22	'Right
	Lampz.MassAssign(22)= TL22

		'Backglass flashers
	Lampz.MassAssign(1) = VRBGBulb1 : 	Lampz.MassAssign(1) = VRBGBulb2 : 	Lampz.MassAssign(1) = VRBGBulb14
	Lampz.MassAssign(10) = VRBGBulb3 : 	Lampz.MassAssign(1) = VRBGBulb4 : 	Lampz.MassAssign(1) = VRBGBulb15
	Lampz.MassAssign(19) = VRBGBulb5 : 	Lampz.MassAssign(1) = VRBGBulb6 : 	Lampz.MassAssign(1) = VRBGBulb16
	Lampz.MassAssign(28) = VRBGBulb7 : 	Lampz.MassAssign(1) = VRBGBulb8 : 	Lampz.MassAssign(1) = VRBGBulb17
	Lampz.MassAssign(37) = VRBGBulb9 : 	Lampz.MassAssign(1) = VRBGBulb10 : 	Lampz.MassAssign(1) = VRBGBulb18

'	If Controller.Lamp(1) = 0 Then: VRBGBulb1.visible=0: VRBGBulb2.visible =0: VRBGBulb14.visible =0 else: VRBGBulb1.visible=1 : VRBGBulb2.visible = 1: VRBGBulb14.visible =1
'	If Controller.Lamp(10) = 0 Then: VRBGBulb3.visible=0: VRBGBulb4.visible =0: VRBGBulb15.visible =0 else: VRBGBulb3.visible=1 : VRBGBulb4.visible = 1: VRBGBulb15.visible =1
'	If Controller.Lamp(19) = 0 Then: VRBGBulb5.visible=0: VRBGBulb6.visible =0: VRBGBulb16.visible =0 else: VRBGBulb5.visible=1 : VRBGBulb6.visible = 1: VRBGBulb16.visible =1
'	If Controller.Lamp(28) = 0 Then: VRBGBulb7.visible=0: VRBGBulb8.visible =0: VRBGBulb17.visible =0 else: VRBGBulb7.visible=1 : VRBGBulb8.visible = 1: VRBGBulb17.visible =1
'	If Controller.Lamp(37) = 0 Then: VRBGBulb9.visible=0: VRBGBulb10.visible =0: VRBGBulb18.visible =0 else: VRBGBulb9.visible=1 : VRBGBulb10.visible = 1: VRBGBulb18.visible =1
	

		'Playfield flashers

	Lampz.MassAssign(115)= F15							'Mirror
	Lampz.Callback(115)= "DisableLighting pF15, 300,"
	Lampz.MassAssign(125)= F25A							'Outlane L
	Lampz.MassAssign(125)= F25B
	Lampz.MassAssign(125)= F25L
	Lampz.MassAssign(125)= F25C							'Outlane R
	Lampz.MassAssign(125)= F25D
	Lampz.MassAssign(125)= F25R
	Lampz.MassAssign(126)= F26A							'Upper Right (Skillshot)
	Lampz.MassAssign(126)= F26B
	Lampz.MassAssign(126)= F26C
	Lampz.MassAssign(127)= F27							'Scoop
	Lampz.MassAssign(128)= F28A							'L Ramp
	Lampz.MassAssign(128)= F28B
	Lampz.MassAssign(129)= F29A 						'Bumper Tommy
	Lampz.MassAssign(129)= F29B
	Lampz.Callback(129) = "DisableLighting pF29, 150,"
	Lampz.MassAssign(131)= F31A							'Captive Ball
	Lampz.MassAssign(131)= F31B
	Lampz.MassAssign(131)= F31C
	Lampz.Callback(131) = "DisableLighting pF31A, 100,"
	Lampz.Callback(131) = "DisableLighting pF31B, 100,"
	Lampz.MassAssign(132)= F32A							'L Orbit
	Lampz.Callback(132) = "DisableLighting pF32A, 150,"
	Lampz.MassAssign(132)= F32C							'L Orbit 1945
	Lampz.MassAssign(132)= F32B

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
Class NullFadingObject
	Public Property Let IntensityScale(input)
		
	End Property
End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks
'Version 0.14 - Updated to support modulated signals - Niwak
'Version 0.15 - Added IsLight property - apophis

Class LampFader
	Public IsLight(150)
	Public FadeSpeedDown(150), FadeSpeedUp(150)
	Private Lock(150), Loaded(150), OnOff(150)
	Public UseFunc
	Private cFilter
	Public UseCallback(150), cCallback(150)
	Public Lvl(150), Obj(150)
	Private Mult(150)
	Public FrameTime
	Private InitFrame
	Public Name
	
	Sub Class_Initialize()
		InitFrame = 0
		Dim x
		For x = 0 To UBound(OnOff)	 'Set up fade speeds
			FadeSpeedDown(x) = 1 / 100	'fade speed down
			FadeSpeedUp(x) = 1 / 80		'Fade speed up
			UseFunc = False
			lvl(x) = 0
			OnOff(x) = 0
			Lock(x) = True
			Loaded(x) = False
			Mult(x) = 1
			IsLight(x) = False
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		For x = 0 To UBound(OnOff)		 'clear out empty obj
			If IsEmpty(obj(x) ) Then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub
	
	Public Property Get Locked(idx)
		Locked = Lock(idx)
		'   debug.print Lampz.Locked(100)	'debug
	End Property
	
	Public Property Get state(idx)
		state = OnOff(idx)
	End Property
	
	Public Property Let Filter(String)
		Set cFilter = GetRef(String)
		UseFunc = True
	End Property
	
	Public Function FilterOut(aInput)
		If UseFunc Then 
			FilterOut = cFilter(aInput) 
		Else 
			FilterOut = aInput
		End If
	End Function
	
	'   Public Property Let Callback(idx, String)
	'	   cCallback(idx) = String
	'	   UseCallBack(idx) = True
	'   End Property
	
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'   cCallback(idx) = String 'old execute method
		
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _
		
		Dim tmp
		tmp = Split(cCallback(idx), "___")
		
		Dim str, x
		For x = 0 To UBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x) = "" Then str = str & tmp(x) & " aLVL:"
		Next
		'   msgbox "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		Dim out
		out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		ExecuteGlobal Out
	End Property
	
	Public Property Let state(ByVal idx, input) 'Major update path
		If TypeName(input) <> "Double" And TypeName(input) <> "Integer"  And TypeName(input) <> "Long" Then
			If input Then
				input = 1
			Else
				input = 0
			End If
		End If
		If Input <> OnOff(idx) Then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property
	
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput) 'Mass assign, Builds arrays where necessary
		If TypeName(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			If IsArray(aInput) Then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
				If typename(aInput) = "Light" Then 
					IsLight(aIdx) = True
				End If
			End If
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		End If
	End Property
	
	Sub SetLamp(aIdx, aOn)  'If obj contains any light objects, set their states to 1 (Fading is our job!)
		state(aIdx) = aOn
	End Sub
	
	Public Sub TurnOnStates() 'turn state to 1
		Dim debugstr
		Dim idx
		For idx = 0 To UBound(obj)
			If IsArray(obj(idx)) Then
				'debugstr = debugstr & "array found at " & idx & "..."
				Dim x, tmp
				tmp = obj(idx) 'set tmp to array in order to access it
				For x = 0 To UBound(tmp)
					If TypeName(tmp(x)) = "Light" Then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				If TypeName(obj(idx)) = "Light" Then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			End If
		Next
		'   debug.print debugstr
	End Sub
	
	Private Sub DisableState(ByRef aObj)
		aObj.FadeSpeedUp = 1000
		aObj.State = 1
	End Sub
	
	Public Sub Init() 'Just runs TurnOnStates right now
		TurnOnStates
	End Sub
	
	Public Property Let Modulate(aIdx, aCoef)
		Mult(aIdx) = aCoef
		Lock(aIdx) = False
		Loaded(aIdx) = False
	End Property
	Public Property Get Modulate(aIdx)
		Modulate = Mult(aIdx)
	End Property
	
	Public Sub Update1() 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		Dim x
		For x = 0 To UBound(OnOff)
			If Not Lock(x) Then 'and not Loaded(x) then
				If OnOff(x) > 0 Then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					If Lvl(x) >= OnOff(x) Then
						Lvl(x) = OnOff(x)
						Lock(x) = True
					End If
				Else 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					If Lvl(x) <= 0 Then
						Lvl(x) = 0
						Lock(x) = True
					End If
				End If
			End If
		Next
	End Sub
	
	Public Sub Update2() 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame
		InitFrame = GameTime	'Calculate frametime
		Dim x
		For x = 0 To UBound(OnOff)
			If Not Lock(x) Then 'and not Loaded(x) then
				If OnOff(x) > 0 Then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					If Lvl(x) >= OnOff(x) Then
						Lvl(x) = OnOff(x)
						Lock(x) = True
					End If
				Else 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					If Lvl(x) <= 0 Then
						Lvl(x) = 0
						Lock(x) = True
					End If
				End If
			End If
		Next
		Update
	End Sub
	
	Public Sub Update() 'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		Dim x,xx, aLvl
		For x = 0 To UBound(OnOff)
			If Not Loaded(x) Then
				aLvl = Lvl(x) * Mult(x)
				If IsArray(obj(x) ) Then	'if array
					If UseFunc Then
						For Each xx In obj(x)
							xx.IntensityScale = cFilter(aLvl)
						Next
					Else
						For Each xx In obj(x)
							xx.IntensityScale = aLvl
						Next
					End If
				Else						'if single lamp or flasher
					If UseFunc Then
						obj(x).Intensityscale = cFilter(aLvl)
					Else
						obj(x).Intensityscale = aLvl
					End If
				End If
				'   if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" and typename(lvl(x)) <> "Long" then msgbox "uhh " & 2 & " = " & lvl(x)
				'   If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) Then Proc name & x,aLvl	'Proc
				If Lock(x) Then
					If Lvl(x) = OnOff(x) Or Lvl(x) = 0 Then Loaded(x) = True	'finished fading
				End If
			End If
		Next
	End Sub
End Class

' version 0.1alt - Drop-in replacement of DynamicLamps, but stripped down For PWM pinmame update. removed all fading, objects are simply updated on a timer
' Setup: 
'	.MassAssign index, object or array of objects	-- Appends flasher or lamp object to a lamp index. For other objects, see '.Callback'
'	.MassAssign(index) = object or array of objects	-- same as above
'	.Init	--	Optional, makes sure all currently assigned lamp object have their states set to 1
'	.Update	--	must be set

' Usage: 
'	.SetLamp index value	--	 set a lamp index
'	.SetModLamp index value	--	set a lamp index - input will be divided by 255
'	.state(index) = value	--	same as SetLamp but uses a different syntax

' Special features: 
'	.Callback = "methodname" -- calls a method with current fading value (0 to 1 float value). For handling .blenddisablelighting or other lighting tricks
'	.Mult(index) = value  -- sets a multiplier on the lighting, for balancing lamps against each GI or other tricks
'	.Filter = "functionname" -- puts *all* lamp indexes through a function. Function must take a numeric value and return a numeric value

Class NoFader2 'Lamps that fade up and down. GI and Flasher handling for PWM pinmame update
	Private UseCallback(32), cCallback(32)
	private Lvl(32)
	private Obj(32)
	private Lock(32)
	private Mult(32)
	'Private UseFunction, cFilter

	Private Sub Class_Initialize() 
		dim x : for x = 0 to uBound(Lock) 
			mult(x) = 1
			Lock(x)=True
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader
		next
	End Sub

	Public Sub Init() : TurnOnStates : end sub	'just call turnonstates for now

	Public Property Let Callback(aIdx, String) : cCallback(aIdx) = String : UseCallBack(aIdx) = True : End Property 'execute
	'Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	'Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function

	'solcallback (solmodcallback) handler 
	Public Sub SetLamp(aIdx, aInput) : Lvl(aIdx) = aInput: Lock(aIdx)=false : End Sub	'0->1 Input
	Public Sub SetModLamp(aIdx, aInput) : Lvl(aIdx) = aInput/255: Lock(aIdx)=false : End Sub	'0->255 Input
	'REM Sub SetModLamp(aIdx, aInput) : Update aIDX, aInput/255 : arrayFLASH(aIDX) = aInput : End Sub	'0->255 Input DEBUG

	Public Property Let State(aIdx, aInput) : Lvl(aIdx) = aInput: Lock(aIdx)=false : End Property
	Public Property Get state(aIdx ) : state = Lvl(aIdx) : end Property

	'Mass assign, Builds arrays where necessary
'	Public Sub MassAssign(aIdx, aInput)
'		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
'			if IsArray(aInput) then
'				obj(aIdx) = aInput
'			Else
'				Set obj(aIdx) = aInput
'			end if
'		Else
'			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
'		end if	
'	end Sub
	
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

	Private Sub TurnOnStates()	'If obj contains any light objects, set their states to 1
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then 
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) ': debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.0000001
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) ': debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.0000001
			end if
		Next
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1
		
	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx)=False : End Property          ' timer update
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property 


	Public Sub Update()	'Handle object updates. Call on timer 
		dim x,xx : for x = 0 to uBound(Obj)
			if not Lock(x) then
				Lock(x)=True
				if IsArray(obj(x)) Then
					for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
				else 
					obj(x).IntensityScale = Lvl(x)*Mult(x)
				end If

				If UseCallBack(x) then 
'					msgbox "execute" & cCallback(x) & " " & (Lvl(x))
					'execute cCallback(x) & " " & (Lvl(x))
					proc cCallback(x), (Lvl(x))
					
				end If
			end if
		Next
		
	End Sub	
	
End Class


'Helper function 
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

'Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl ^ 1.6	'exponential curve?
End Function

'Helper functions
Sub Proc(String, Callback)	'proc using a string and one argument
	'On Error Resume Next
	Dim p
	Set P = GetRef(String)
	P Callback
	If err.number = 13 Then  MsgBox "Proc error! No such procedure: " & vbNewLine & String
	If err.number = 424 Then MsgBox "Proc error! No such Object"
End Sub

'******************************************************
'****  END LAMPZ
'******************************************************


'******************************************************
'*****   FLUPPER DOMES 
'******************************************************


Sub Flash26(Enabled)	'Upper Right
	If Enabled Then
		Objlevel(1) = 1 : FlasherFlash1_Timer
		Lampz.SetLamp 126, 1
	Else
		Lampz.SetLamp 126, 0
	End If
End Sub

Sub Flash27(Enabled)	'Scoop
	If Enabled Then
		Objlevel(2) = 1 : FlasherFlash2_Timer
		Lampz.SetLamp 127, 1
	Else
		Lampz.SetLamp 127, 0
	End If
End Sub

Sub Flash30(Enabled)	'Back Panel
	If Enabled Then
		Objlevel(3) = 1 : FlasherFlash3_Timer
		Objlevel(4) = 1 : FlasherFlash4_Timer
		Objlevel(5) = 1 : FlasherFlash5_Timer
		Objlevel(6) = 1 : FlasherFlash6_Timer
	End If
End Sub

Sub Flash32(Enabled)	'Upper Left
	If Enabled Then
		Objlevel(7) = 1 : FlasherFlash7_Timer
		Lampz.SetLamp 132, 1
	Else
		Lampz.SetLamp 132, 0
	End If
End Sub




Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Tommy   			' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.3		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.6		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.2		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"

InitFlasher 1, "red"
InitFlasher 2, "red"
InitFlasher 3, "white"
InitFlasher 4, "white"
InitFlasher 5, "white"
InitFlasher 6, "white"
InitFlasher 7, "red"

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 1,17 : RotateFlasher 2,0 : RotateFlasher 3,90 : RotateFlasher 4,90 


Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness

	'rothbauerw
	'Adjust the position of the flasher object to align with the flasher base.
	'Comment out these lines if you want to manually adjust the flasher object
	If objbase(nr).roty > 135 then
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
	objlight(nr).bulbhaloheight = objbase(nr).z -10

	'rothbauerw
	'Assign the appropriate bloom image basked on the location of the flasher base
	'Comment out these lines if you want to manually assign the bloom images
	dim xthird, ythird
	xthird = tablewidth/3
	ythird = tableheight/3

	If objbase(nr).x >= xthird and objbase(nr).x <= xthird*2 then
		objbloom(nr).imageA = "flasherbloomCenter"
		objbloom(nr).imageB = "flasherbloomCenter"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird then
		objbloom(nr).imageA = "flasherbloomUpperLeft"
		objbloom(nr).imageB = "flasherbloomUpperLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird then
		objbloom(nr).imageA = "flasherbloomUpperRight"
		objbloom(nr).imageB = "flasherbloomUpperRight"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird*2 then
		objbloom(nr).imageA = "flasherbloomCenterLeft"
		objbloom(nr).imageB = "flasherbloomCenterLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird*2 then
		objbloom(nr).imageA = "flasherbloomCenterRight"
		objbloom(nr).imageB = "flasherbloomCenterRight"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird*3 then
		objbloom(nr).imageA = "flasherbloomLowerLeft"
		objbloom(nr).imageB = "flasherbloomLowerLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird*3 then
		objbloom(nr).imageA = "flasherbloomLowerRight"
		objbloom(nr).imageB = "flasherbloomLowerRight"
	end if

	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objbloom(nr).color = RGB(4,120,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4) : objbloom(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4) : objbloom(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) : objbloom(nr).color = RGB(230,49,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50) : objbloom(nr).color = RGB(200,173,25)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59) : objbloom(nr).color = RGB(255,240,150)
		Case "orange" :  objlight(nr).color = RGB(255,70,0) : objflasher(nr).color = RGB(255,70,0) : objbloom(nr).color = RGB(255,70,0)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then 
		objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1
		Select Case nr
			Case 2:
				If VRRoom > 0 and VRRoom < 3 Then   'VR Backglass Flasher
					VRBGFL27_1.visible = 1
					VRBGFL27_2.visible = 1
					VRBGFL27_3.visible = 1
					VRBGFL27_4.visible = 1
					VRBGFL27_5.visible = 1
					VRBGFL27_6.visible = 1
					VRBGFL27_7.visible = 1
					VRBGFL27_8.visible = 1
					VRBGFL27_9.visible = 1
					VRBGFLbig.visible = 1
				End If	
		End Select	
		If nr<=3 or nr=5 or nr=6 Then objbloom(nr).visible = 1
	End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	Select Case nr
		Case 2:
			If VRRoom > 0 and VRRoom < 3 Then
				VRBGFL27_1.opacity = 90 * ObjLevel(nr)^2.5
				VRBGFL27_2.opacity = 90 * ObjLevel(nr)^2.5
				VRBGFL27_3.opacity = 90 * ObjLevel(nr)^2.5
				VRBGFL27_4.opacity = 100 * ObjLevel(nr)^3.5
				VRBGFL27_5.opacity = 90 * ObjLevel(nr)^2.5
				VRBGFL27_6.opacity = 90 * ObjLevel(nr)^2.5
				VRBGFL27_7.opacity = 90 * ObjLevel(nr)^2.5
				VRBGFL27_8.opacity = 100 * ObjLevel(nr)^3.5
				VRBGFL27_9.opacity = 100 * ObjLevel(nr)^3.5
				VRBGFLbig.opacity = 60 * ObjLevel(nr)^3.5
			End If
	End Select
	ObjLevel(nr) = ObjLevel(nr) * 0.8 - 0.01
	If ObjLevel(nr) < 0 Then 
		objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objbloom(nr).visible = 0 : objlit(nr).visible = 0
		Select Case nr
			Case 2:
				If VRRoom > 0 and VRRoom < 3 Then
					VRBGFL27_1.visible = 0
					VRBGFL27_2.visible = 0
					VRBGFL27_3.visible = 0
					VRBGFL27_4.visible = 0
					VRBGFL27_5.visible = 0
					VRBGFL27_6.visible = 0
					VRBGFL27_7.visible = 0
					VRBGFL27_8.visible = 0
					VRBGFL27_9.visible = 0
					VRBGFLbig.visible = 0
				End If
		End Select
		If nr = 2 Then SetLamp 127,0
	End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 
Sub FlasherFlash7_Timer() : FlashFlasher(7) : End Sub 


'******************************************************
'******  END FLUPPER DOMES
'******************************************************


'******************************************************
'******  VR Backglass Flashers
'******************************************************

dim sol15lvl, sol28lvl, sol31lvl

sub FlashSol15(enabled)
	If Enabled = False Then
		Setlamp 115, 0
	Else
		If VRRoom > 0 and VRRoom < 3 Then
			sol15lvl = 1
			sol15timer_timer
		End If
		Setlamp 115, 1
	End If
end sub

sub sol15timer_timer                            
    if Not sol15timer.enabled then
		VRBGFL15_1.visible = true
        VRBGFL15_2.visible = true
        VRBGFL15_3.visible = true
        VRBGFL15_4.visible = true
        VRBGFL15_5.visible = true
        VRBGFL15_6.visible = true
        VRBGFL15_7.visible = true
        VRBGFL15_8.visible = true
        VRBGFL15_9.visible = true
        VRBGFL15_10.visible = true
        VRBGFL15_11.visible = true
        VRBGFL15_12.visible = true
        VRBGFL15_13.visible = true
        VRBGFL15_14.visible = true
		VRBGFLbig.visible = true
        sol15timer.enabled = true
    end if
		VRBGFL15_1.opacity = 90 * sol15lvl^2
		VRBGFL15_2.opacity = 90 * sol15lvl^2
		VRBGFL15_3.opacity = 90 * sol15lvl^2
		VRBGFL15_4.opacity = 100 * sol15lvl^3
		VRBGFL15_5.opacity = 90 * sol15lvl^2
		VRBGFL15_6.opacity = 90 * sol15lvl^2
		VRBGFL15_7.opacity = 90 * sol15lvl^2
		VRBGFL15_8.opacity = 90 * sol15lvl^3
		VRBGFL15_9.opacity = 50 * sol15lvl^2
		VRBGFL15_10.opacity = 90 * sol15lvl^2
		VRBGFL15_11.opacity = 90 * sol15lvl^2
		VRBGFL15_12.opacity = 100 * sol15lvl^3
		VRBGFL15_13.opacity = 90 * sol15lvl
		VRBGFL15_14.opacity = 100 * sol15lvl^3
		VRBGFLbig.opacity = 60 * sol15lvl^3.5
    sol15lvl = 0.80 * sol15lvl - 0.01
    if sol15lvl < 0 then sol15lvl = 0
    if sol15lvl =< 0 Then
		VRBGFL15_2.visible = false
		VRBGFL15_3.visible = false
		VRBGFL15_4.visible = false
		VRBGFL15_5.visible = false
		VRBGFL15_6.visible = false
		VRBGFL15_7.visible = false
		VRBGFL15_8.visible = false
		VRBGFL15_9.visible = false
		VRBGFL15_10.visible = false
		VRBGFL15_11.visible = false
		VRBGFL15_12.visible = false
		VRBGFL15_13.visible = false
		VRBGFL15_13.visible = false
		VRBGFL15_14.visible = false
		VRBGFLbig.visible = false
		sol15timer.enabled = false
    end if
end sub

sub FlashSol28(enabled)
	If Enabled = False Then
		Setlamp 128, 0
	Else
		Setlamp 128, 1
		If VRRoom > 0 and VRRoom < 3 Then
			sol28lvl = 1
			sol28timer_timer
		End If
	End If
end sub

sub sol28timer_timer                            
    if Not sol28timer.enabled then
        VRBGFL28_1.visible = true
        VRBGFL28_2.visible = true
        VRBGFL28_3.visible = true
        VRBGFL28_4.visible = true
        VRBGFL28_5.visible = true
        VRBGFL28_6.visible = true
        VRBGFL28_7.visible = true
        VRBGFL28_8.visible = true
        VRBGFL28_9.visible = true
		VRBGFLbig.visible = true
        sol28timer.enabled = true
    end if
		VRBGFL28_1.opacity = 90 * sol28lvl^2
		VRBGFL28_2.opacity = 90 * sol28lvl^2
		VRBGFL28_3.opacity = 90 * sol28lvl^2
		VRBGFL28_4.opacity = 100 * sol28lvl^3
		VRBGFL28_5.opacity = 90 * sol28lvl^2
		VRBGFL28_6.opacity = 90 * sol28lvl^2
		VRBGFL28_7.opacity = 90 * sol28lvl^2
		VRBGFL28_8.opacity = 100 * sol28lvl^3
		VRBGFL28_9.opacity = 100 * sol28lvl^3
		VRBGFLbig.opacity = 60 * sol28lvl^3.5
    sol28lvl = 0.80 * sol28lvl - 0.01
    if sol28lvl < 0 then sol28lvl = 0
    if sol28lvl =< 0 Then
		VRBGFL28_1.visible = false
		VRBGFL28_2.visible = false
		VRBGFL28_3.visible = false
		VRBGFL28_4.visible = false
		VRBGFL28_5.visible = false
		VRBGFL28_6.visible = false
		VRBGFL28_7.visible = false
		VRBGFL28_8.visible = false
		VRBGFL28_9.visible = false
		VRBGFLbig.visible = false
		sol28timer.enabled = false
    end if
end sub

sub FlashSol31(enabled)
	If Enabled = False Then
		Setlamp 131, 0
	Else
		If VRRoom > 0 and VRRoom < 3 Then
			sol31lvl = 1
			sol31timer_timer
		End If
		Setlamp 131, 1
	End If
end sub

sub sol31timer_timer                            
    if Not sol31timer.enabled then
		VRBGFL31_1.visible = true
        VRBGFL31_2.visible = true
        VRBGFL31_3.visible = true
        VRBGFL31_4.visible = true
        VRBGFL31_5.visible = true
        VRBGFL31_6.visible = true
        VRBGFL31_7.visible = true
        VRBGFL31_8.visible = true
		VRBGFLbig.visible = true
        sol31timer.enabled = true
    end if
		VRBGFL31_1.opacity = 80 * sol31lvl^2
		VRBGFL31_2.opacity = 80 * sol31lvl^2
		VRBGFL31_3.opacity = 80 * sol31lvl^2
		VRBGFL31_4.opacity = 100 * sol31lvl^3
		VRBGFL31_5.opacity = 80 * sol31lvl^2
		VRBGFL31_6.opacity = 100 * sol31lvl^2
		VRBGFL31_7.opacity = 80 * sol31lvl^2
		VRBGFL31_8.opacity = 100 * sol31lvl^3
		VRBGFLbig.opacity = 60 * sol31lvl^3.5
    sol31lvl = 0.80 * sol31lvl - 0.01
    if sol31lvl < 0 then sol31lvl = 0
    if sol31lvl =< 0 Then
		VRBGFL31_1.visible = false
		VRBGFL31_2.visible = false
		VRBGFL31_3.visible = false
		VRBGFL31_4.visible = false
		VRBGFL31_5.visible = false
		VRBGFL31_6.visible = false
		VRBGFL31_7.visible = false
		VRBGFL31_8.visible = false
		VRBGFLbig.visible = false
		sol31timer.enabled = false
    end if
end sub

dim vrgilvl

sub VRGI
	vrgilvl = 1
	VRGItimer_timer
end sub

sub VRGItimer_timer
	dim VRGIobj
    if Not VRGItimer.enabled then
        VRGItimer.enabled = true
    end if
		For Each VRGIobj in VRBGGI:VRGIobj.opacity = 50 * vrgilvl: Next
		BGBright.opacity = 100 * vrgilvl
		VRBGBulb27.opacity = 15 * vrgilvl
		VRBGBulb39.opacity = 25 * vrgilvl
		VRBGBulb40.opacity = 40 * vrgilvl
		vrgilvl = 0.81 * vrgilvl - 0.01
		if vrgilvl < 0 then vrgilvl = 0
		if vrgilvl =< 0 Then
			For Each VRGIobj in VRBGGI:VRGIobj.visible = 0: Next
			BGBright.visible = 0
			VRBGBulb27.visible = 0
			VRGItimer.enabled = false
		end if
end sub

'******************************************************
'* GI *************************************************
'******************************************************
Dim bulb
dim gilvl:gilvl = 0				'this will be updated in giupdates sub and will hold the current fading level of the GI

Sub GIRelay(enabled)
	If enabled Then
		gilvl = 0
		SetLamp 100,0
		SetLamp 101,0
		SetLamp 102,1
		Playsound "relay_off"
		Tommy.ColorGradeImage="ColorGrade_off"

		If VRRoom > 0 and VRRoom < 3 Then
			VRGI
			inslvl = 0
			BGFlashersFast.enabled = false
			FL1 = 2: FL2 = 2
			VRBGBulb004.opacity = 0
			VRBGBulb005.opacity = 0
			VRBGBulb006.opacity = 0
			VRBGBulb007.opacity = 0
		End If

		FlasherFlash3.opacity = 40000
		for each bulb in GITNTAir:bulb.IntensityScale=0: Next
	Else
		gilvl = 1
		SetLamp 100,1
		SetLamp 101,1
		SetLamp 102,0
		Playsound "relay_on"
		Tommy.ColorGradeImage="ColorGrade_on"

		If VRRoom > 0 and VRRoom < 3 Then
			VRGItimer.enabled = false
			BGFlashersFast.enabled = true
			VRBGBulb27.visible = 1
			BGBright.visible = 1
			For Each bulb in VRBGGI:bulb.visible = 1: Next
			For Each bulb in VRBGGI:bulb.opacity = 50: Next
			BGBright.opacity = 100
			VRBGBulb27.opacity = 15
			VRBGBulb39.opacity = 25
			VRBGBulb40.opacity = 40
		End If

		FlasherFlash3.opacity = 5000
		If TNTMod = 1 then
			for each bulb in GITNTAir:bulb.IntensityScale=1: Next
		End If
	End If
End Sub


'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
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

Sub RollingUpdate()
	Dim b

	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(gBOT)
		If BallVel(gBOT(b)) > 1 AND gBOT(b).z < 30 AND NOT bBallInTrough(b) Then
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

		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If gBOT(b).Z > 30 Then
				BallShadowA(b).height=gBOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				BallShadowA(b).visible = 1
			Elseif gBOT(b).Z<30 And gBOT(b).Z>0 Then
				BallShadowA(b).height=0.1
				BallShadowA(b).visible = 1
			Else
				BallShadowA(b).visible = 0
			End If
			BallShadowA(b).Y = gBOT(b).Y + offsetY
			BallShadowA(b).X = gBOT(b).X + offsetX
		End If
	Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************


'******************************************************
'**** RAMP ROLLING SFX
'******************************************************

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
dim RampBalls(7,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(7)	

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
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
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

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************


'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

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


Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub


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

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b', BOT
'	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper2) Then
					gBOT(b).velx = gBOT(b).velx / 1.3
					gBOT(b).vely = gBOT(b).vely - 0.5
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

dim LFPress, RFPress, LFCount, RFCount, LFPress1, LFCount1
dim LFState, RFState, LFState1
dim EOST, EOSA, Frampup, FElasticity, FReturn
dim RFEndAngle, LFEndAngle

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
Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
		LeftFlipper1.rampup = 1.5
	Case 1:
		SOSRampup = 6
		LeftFlipper1.rampup = 2
	Case 2:
		SOSRampup = 8.5
		LeftFlipper1.rampup = 2.5
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
		Dim b

		For b = 0 to UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If gBOT(b).vely >= -0.4 Then gBOT(b).vely = -0.4
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


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************


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

Sub dPostsB_Hit(idx)
	RubbersD.dampen Activeball
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

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
'Sub RDampen_Timer
'	Cor.Update
'End Sub



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

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

'******************************************************
'  DROP TARGET
'  SUPPORTING FUNCTIONS 
'******************************************************
' Used for drop targets, (here used for sling rotation point and standup target calculations)

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
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9 	'Level of bounces. Recommmended value of 0.7

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

'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
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
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'Function Distance(ax,ay,bx,by)
'	Distance = SQR((ax - bx)^2 + (ay - by)^2)
'End Function

'Dim PI: PI = 4*Atn(1)

'Function Atn2(dy, dx)
'	If dx > 0 Then
'		Atn2 = Atn(dy / dx)
'	ElseIf dx < 0 Then
'		If dy = 0 Then 
'			Atn2 = pi
'		Else
'			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
'		end if
'	ElseIf dx = 0 Then
'		if dy = 0 Then
'			Atn2 = 0
'		else
'			Atn2 = Sgn(dy) * pi / 2
'		end if
'	End If
'End Function

'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******

' *** These define the appearance of shadows in your table	***

'Ambient (Room light source)
Const AmbientBSFactor 		= 0.9	'0 to 1, higher is darker
Const AmbientMovement		= 1		'1+ higher means more movement as the ball moves left and right
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 5		'Offset y position under ball	 (^^for example 5,5 if the light is in the back left corner)
'Dynamic (Table light sources)
Const DynamicBSFactor 		= 0.99	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

' ***														***

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
dim objrtx1(7), objrtx2(7)
dim objBallShadow(7)
Dim OnPF(7)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7)
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

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob - 1								'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii/1000 + 0.01			'Separate z for layering without clipping
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii/1000 + 0.02
		objrtx2(iii).visible = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
'		If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
end sub

Sub BallOnPlayfieldNow(yeh, num)		'Only update certain things once, save some cycles
	If yeh Then
		OnPF(num) = True
		bsRampOff gBOT(num).ID
'		debug.print "Back on PF"
		UpdateMaterial objBallShadow(num).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(num).size_x = 5
		objBallShadow(num).size_y = 4.5
		objBallShadow(num).visible = 1
		BallShadowA(num).visible = 0
		BallShadowA(num).Opacity = 100 * AmbientBSFactor
	Else
		OnPF(num) = False
'		debug.print "Leaving PF"
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed dynamically if you have a reason
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim bsRampType
'	Dim gBOT: gBOT=getballs	'Uncomment if you're destroying balls - Don't do it! #SaveTheBalls

	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 to tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(gBOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your Elseif segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else (under 20)

	'Primitive shadow on playfield, flasher shadow in ramps
		If AmbientBallShadowOn = 1 Then

		'** Above the playfield
			If gBOT(s).Z > 30 Then
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update
				bsRampType = getBsRampType(gBOT(s).id)
'				debug.print bsRampType

				If Not bsRampType = bsRamp Then		'Primitive visible on PF
					objBallShadow(s).visible = 1
					objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement) + offsetX
					objBallShadow(s).Y = gBOT(s).Y + offsetY
					objBallShadow(s).size_x = 5 * ((gBOT(s).Z+BallSize)/80)		'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z+BallSize)/80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(30/(gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else								'Opaque, no primitive below
					objBallShadow(s).visible = 0
				End If

				If bsRampType = bsRampClear Or bsRampType = bsRamp Then		'Flasher visible on opaque ramp
					BallShadowA(s).visible = 1
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + offsetY + BallSize/10
					BallShadowA(s).height=gBOT(s).z - BallSize/4 + s/1000		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
					If bsRampType = bsRampClear Then BallShadowA(s).Opacity = 50 * AmbientBSFactor
				Elseif bsRampType = bsWire or bsRampType = bsNone Then		'Turn it off on wires or falling out of a ramp
					BallShadowA(s).visible = 0
				End If

		'** On pf, primitive only
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then
				If Not OnPF(s) Then BallOnPlayfieldNow True, s
				objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement) + offsetX
				objBallShadow(s).Y = gBOT(s).Y + offsetY
'				objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf																																						 

		'** Under pf, flasher shadow only
			Else
				If OnPF(s) Then BallOnPlayfieldNow False, s
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 1
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height=gBOT(s).z - BallSize/4 + s/1000
			end if

	'Flasher shadow everywhere
		Elseif AmbientBallShadowOn = 2 Then
			If gBOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY + BallSize/10
				BallShadowA(s).height=gBOT(s).z - BallSize/4 + s/1000		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				BallShadowA(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement) + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height= 1.04 + s/1000
			Else											'Under pf
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height=gBOT(s).z - BallSize/4 + s/1000
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).Z > 0 And gBOT(s).X < 850 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff:
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then
'					If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y
					'objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + offsetY
					'objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
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
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

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
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperLeft1HitParm, FlipperRightHitParm
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
RubberFlipperSoundFactor = 0.075/4										'volume multiplier; must not be zero
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
SpinnerSoundLevel = 0.2                              					'volume level; range [0, 1]

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

Sub SoundAutoPlunger()
	PlaySoundAtLevelStatic ("Flipper_R09"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundAutoPlungerDown()
	PlaySoundAtLevelStatic ("Flipper_Right_Down_8"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub


'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid 'Add knocker position object
	End If
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

Sub LeftFlipper1Collide(parm)
	FlipperLeft1HitParm = parm/10
	If FlipperLeft1HitParm > 1 Then
		FlipperLeft1HitParm = 1
	End If
	FlipperLeft1HitParm = FlipperUpSoundLevel * FlipperLeft1HitParm
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
'****  END FLEEP MECHANICAL SOUNDS
'******************************************************


'****************************************************************
'		VR Mode
'****************************************************************
DIM VRThings
If VRRoom > 0 Then
	rightrail.visible = 0
	leftrail.visible = 0
	lockbar.visible = 0
	TextBox1.visible = 0
	DMD.visible = 1
	BGFlicker.enabled = 1
	If VRRoom = 1 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_BaSti:VRThings.visible = 1:Next
		BeerTimer.enabled = 1
		ClockTimer.enabled = 1
		BGDark.visible = 1
		Setbackglass
		For Each VRthings in VRBGGI:VRThings.visible = 1: Next
		BGBright.visible = 1
		VRBGBulb27.visible = 1
	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
		for each VRThings in VR_BaSti:VRThings.visible = 0:Next
		BeerTimer.enabled = 0
		ClockTimer.enabled = 0
		BGDark.visible = 1
		Setbackglass
		For Each VRthings in VRBGGI:VRThings.visible = 1: Next
		BGBright.visible = 1
		VRBGBulb27.visible = 1
	End If
	If VRRoom = 3 Then
		for each VRThings in VR_Cab:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_BaSti:VRThings.visible = 0:Next
		PinCab_Backbox.visible = 1
		BeerTimer.enabled = 0
		ClockTimer.enabled = 0
		PinCab_Backglass.visible = 1
	End If
	If VRBGBlink = 1 Then
		VRBGFLtimer.enabled = true
		BGFlashersFast.enabled = True
		BGBright.imageA = "Backglassimagebright"
		BGBright.imageB = "Backglassimagebright"
	Else
		VRBGFLtimer.enabled = false
		BGFlashersFast.enabled = false
		BGBright.imageA = "Backglassimage2"
		BGBright.imageB = "Backglassimage2"
	End If
Else
		for each VRThings in VR_Cab:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_BaSti:VRThings.visible = 0:Next
		BeerTimer.enabled = 0
		ClockTimer.enabled = 0
		BGFlicker.enabled = 0
		BGFlashersFast.enabled = 0
		VRBGFLTimer.enabled = 0
End if


'******************************************************
'*******	Set Up Backglass Flashers	*******
'******************************************************

Sub SetBackglass()
	Dim obj

	For Each obj In VRBackglass
		obj.x = obj.x 
		obj.height = - obj.y + 300
		obj.y = -155 'adjusts the distance from the backglass towards the user
		obj.rotx=-84
	Next


	For Each obj In VRBackglassHigh
		obj.x = obj.x
		obj.height = - obj.y + 300
		obj.y = -185 'adjusts the distance from the backglass towards the user
		obj.rotx=-84
	Next
End Sub

' ******************************************************************************************
'      LAMP CALLBACK for the 6 backglass flasher lamps (not the solenoid controlled ones)
' ******************************************************************************************

'Set LampCallback = GetRef("UpdateMultipleLamps")
'
Sub UpdateMultipleLamps()
  IF VRRoom <> 0 Then
'	If Controller.Lamp(1) = 0 Then: VRBGBulb1.visible=0: VRBGBulb2.visible =0: VRBGBulb14.visible =0 else: VRBGBulb1.visible=1 : VRBGBulb2.visible = 1: VRBGBulb14.visible =1
'	If Controller.Lamp(10) = 0 Then: VRBGBulb3.visible=0: VRBGBulb4.visible =0: VRBGBulb15.visible =0 else: VRBGBulb3.visible=1 : VRBGBulb4.visible = 1: VRBGBulb15.visible =1
'	If Controller.Lamp(19) = 0 Then: VRBGBulb5.visible=0: VRBGBulb6.visible =0: VRBGBulb16.visible =0 else: VRBGBulb5.visible=1 : VRBGBulb6.visible = 1: VRBGBulb16.visible =1
'	If Controller.Lamp(28) = 0 Then: VRBGBulb7.visible=0: VRBGBulb8.visible =0: VRBGBulb17.visible =0 else: VRBGBulb7.visible=1 : VRBGBulb8.visible = 1: VRBGBulb17.visible =1
'	If Controller.Lamp(37) = 0 Then: VRBGBulb9.visible=0: VRBGBulb10.visible =0: VRBGBulb18.visible =0 else: VRBGBulb9.visible=1 : VRBGBulb10.visible = 1: VRBGBulb18.visible =1
	If Controller.Lamp(23) = 0 Then: ExtraBallButton.blenddisablelighting=0.20 else: ExtraBallButton.blenddisablelighting=0.25
	If Controller.Lamp(64) = 0 Then: Launchbutton.blenddisablelighting=0.20: Flasher3.visible = 0: else: Launchbutton.blenddisablelighting=0.25: Flasher3.visible = 1
  End If
End Sub

'******************* VR Plunger **********************

Sub TimerVRPlunger_Timer
  If Pincab_Shooter.Y < 60 then
       Pincab_Shooter.Y = Pincab_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger2_Timer
	Pincab_Shooter.Y = -70 + (5* Plunger.Position) -20
	timervrplunger2.enabled = 0
End Sub

' ***** Beer Bubble Code - Rawd *****
Sub BeerTimer_Timer()

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
	CurrentMinute=Minute(Now())
End Sub
 ' ********************** END CLOCK CODE   *********************************

dim Inslvl, l2num, l3num, l4num, l5num, l6num, l7num, l8num, l9num, l10num, l11num, l12num, l13num, l14num, l15num, l16num, l17num, l18num, l19num, l20num, l21num, l22num, l24num, l25num, l26num, l27num, l28num, l29num
dim l30num, l31num, l32num, l33num, l34num, l35num, l36num, l37num, l38num, l39num, l41num, l42num, l43num, l44num, l45num, l46num, l47num, l48num, l49num, l40num
dim l50num, l51num, l52num, l53num, l54num, l55num, l56num, l57num, l58num, l59num, l60num, l61num, l62num, l63num
Inslvl = 0
Sub BGFlicker_timer
	dim BGlight
	If l2.intensityscale > 0 Then 
		If l2num = 0 Then l2num = 1 : inslvl = inslvl - 1
	End If
	If l2.intensityscale = 0 Then
		If l2num = 1 Then l2num = 0 : inslvl = inslvl + 1
	End If
	If l3.intensityscale > 0 Then 
		If l3num = 0 Then l3num = 1 : inslvl = inslvl - 1
	End If
	If l3.intensityscale = 0 Then
		If l3num = 1 Then l3num = 0 : inslvl = inslvl + 1
	End If
	If l4.intensityscale > 0 Then 
		If l4num = 0 Then l4num = 1 : inslvl = inslvl - 1
	End If
	If l4.intensityscale = 0 Then
		If l4num = 1 Then l4num = 0 : inslvl = inslvl + 1
	End If
	If l5.intensityscale > 0 Then 
		If l5num = 0 Then l5num = 1 : inslvl = inslvl - 1
	End If
	If l5.intensityscale = 0 Then
		If l5num = 1 Then l5num = 0 : inslvl = inslvl + 1
	End If
	If l6.intensityscale > 0 Then 
		If l6num = 0 Then l6num = 1 : inslvl = inslvl - 1
	End If
	If l6.intensityscale = 0 Then
		If l6num = 1 Then l6num = 0 : inslvl = inslvl + 1
	End If
	If l7.intensityscale > 0 Then 
		If l7num = 0 Then l7num = 1 : inslvl = inslvl - 1
	End If
	If l7.intensityscale = 0 Then
		If l7num = 1 Then l7num = 0 : inslvl = inslvl + 1
	End If
	If l8.intensityscale > 0 Then 
		If l8num = 0 Then l8num = 1 : inslvl = inslvl - 1
	End If
	If l8.intensityscale = 0 Then
		If l8num = 1 Then l8num = 0 : inslvl = inslvl + 1
	End If
	If l9.intensityscale > 0 Then 
		If l9num = 0 Then l9num = 1 : inslvl = inslvl - 1
	End If
	If l9.intensityscale = 0 Then
		If l9num = 1 Then l9num = 0 : inslvl = inslvl + 1
	End If
	If l11.intensityscale > 0 Then 
		If l11num = 0 Then l11num = 1 : inslvl = inslvl - 1
	End If
	If l11.intensityscale = 0 Then
		If l11num = 1 Then l11num = 0 : inslvl = inslvl + 1
	End If
	If l12.intensityscale > 0 Then 
		If l12num = 0 Then l12num = 1 : inslvl = inslvl - 1
	End If
	If l12.intensityscale = 0 Then
		If l12num = 1 Then l12num = 0 : inslvl = inslvl + 1
	End If
	If l13.intensityscale > 0 Then 
		If l13num = 0 Then l13num = 1 : inslvl = inslvl - 1
	End If
	If l13.intensityscale = 0 Then
		If l13num = 1 Then l13num = 0 : inslvl = inslvl + 1
	End If
	If l14.intensityscale > 0 Then 
		If l14num = 0 Then l14num = 1 : inslvl = inslvl - 1
	End If
	If l14.intensityscale = 0 Then
		If l14num = 1 Then l14num = 0 : inslvl = inslvl + 1
	End If
	If l15.intensityscale > 0 Then 
		If l15num = 0 Then l15num = 1 : inslvl = inslvl - 1
	End If
	If l15.intensityscale = 0 Then
		If l15num = 1 Then l15num = 0 : inslvl = inslvl + 1
	End If
	If l16.intensityscale > 0 Then 
		If l16num = 0 Then l16num = 1 : inslvl = inslvl - 1
	End If
	If l16.intensityscale = 0 Then
		If l16num = 1 Then l16num = 0 : inslvl = inslvl + 1
	End If
	If l17.intensityscale > 0 Then 
		If l17num = 0 Then l17num = 1 : inslvl = inslvl - 1
	End If
	If l17.intensityscale = 0 Then
		If l17num = 1 Then l17num = 0 : inslvl = inslvl + 1
	End If
	If l18.intensityscale > 0 Then 
		If l18num = 0 Then l18num = 1 : inslvl = inslvl - 1
	End If
	If l18.intensityscale = 0 Then
		If l18num = 1 Then l18num = 0 : inslvl = inslvl + 1
	End If
	If l20.intensityscale > 0 Then 
		If l20num = 0 Then l20num = 1 : inslvl = inslvl - 1
	End If
	If l20.intensityscale = 0 Then
		If l20num = 1 Then l20num = 0 : inslvl = inslvl + 1
	End If
	If l21.intensityscale > 0 Then 
		If l21num = 0 Then l21num = 1 : inslvl = inslvl - 1
	End If
	If l21.intensityscale = 0 Then
		If l21num = 1 Then l21num = 0 : inslvl = inslvl + 1
	End If
	If l22.intensityscale > 0 Then 
		If l22num = 0 Then l22num = 1 : inslvl = inslvl - 1
	End If
	If l22.intensityscale = 0 Then
		If l22num = 1 Then l22num = 0 : inslvl = inslvl + 1
	End If
	If l24.intensityscale > 0 Then 
		If l24num = 0 Then l24num = 1 : inslvl = inslvl - 1
	End If
	If l24.intensityscale = 0 Then
		If l24num = 1 Then l24num = 0 : inslvl = inslvl + 1
	End If
	If l25.intensityscale > 0 Then 
		If l25num = 0 Then l25num = 1 : inslvl = inslvl - 1
	End If
	If l25.intensityscale = 0 Then
		If l25num = 1 Then l25num = 0 : inslvl = inslvl + 1
	End If
	If l26.intensityscale > 0 Then 
		If l26num = 0 Then l26num = 1 : inslvl = inslvl - 1
	End If
	If l26.intensityscale = 0 Then
		If l26num = 1 Then l26num = 0 : inslvl = inslvl + 1
	End If
	If l27.intensityscale > 0 Then 
		If l27num = 0 Then l27num = 1 : inslvl = inslvl - 1
	End If
	If l27.intensityscale = 0 Then
		If l27num = 1 Then l27num = 0 : inslvl = inslvl + 1
	End If
	If l29.intensityscale > 0 Then 
		If l29num = 0 Then l29num = 1 : inslvl = inslvl - 1
	End If
	If l29.intensityscale = 0 Then
		If l29num = 1 Then l29num = 0 : inslvl = inslvl + 1
	End If
	If l30.intensityscale > 0 Then 
		If l30num = 0 Then l30num = 1 : inslvl = inslvl - 1
	End If
	If l30.intensityscale = 0 Then
		If l30num = 1 Then l30num = 0 : inslvl = inslvl + 1
	End If
	If l31.intensityscale > 0 Then 
		If l31num = 0 Then l31num = 1 : inslvl = inslvl - 1
	End If
	If l31.intensityscale = 0 Then
		If l31num = 1 Then l31num = 0 : inslvl = inslvl + 1
	End If
	If l32.intensityscale > 0 Then 
		If l32num = 0 Then l32num = 1 : inslvl = inslvl - 1
	End If
	If l32.intensityscale = 0 Then
		If l32num = 1 Then l32num = 0 : inslvl = inslvl + 1
	End If
	If l33.intensityscale > 0 Then 
		If l33num = 0 Then l33num = 1 : inslvl = inslvl - 1
	End If
	If l33.intensityscale = 0 Then
		If l33num = 1 Then l33num = 0 : inslvl = inslvl + 1
	End If
	If l34.intensityscale > 0 Then 
		If l34num = 0 Then l34num = 1 : inslvl = inslvl - 1
	End If
	If l34.intensityscale = 0 Then
		If l34num = 1 Then l34num = 0 : inslvl = inslvl + 1
	End If
	If l35.intensityscale > 0 Then 
		If l35num = 0 Then l35num = 1 : inslvl = inslvl - 1
	End If
	If l35.intensityscale = 0 Then
		If l35num = 1 Then l35num = 0 : inslvl = inslvl + 1
	End If
	If l36.intensityscale > 0 Then 
		If l36num = 0 Then l36num = 1 : inslvl = inslvl - 1
	End If
	If l36.intensityscale = 0 Then
		If l36num = 1 Then l36num = 0 : inslvl = inslvl + 1
	End If
	If l38.intensityscale > 0 Then 
		If l38num = 0 Then l38num = 1 : inslvl = inslvl - 1
	End If
	If l38.intensityscale = 0 Then
		If l38num = 1 Then l38num = 0 : inslvl = inslvl + 1
	End If
	If l39.intensityscale > 0 Then 
		If l39num = 0 Then l39num = 1 : inslvl = inslvl - 1
	End If
	If l39.intensityscale = 0 Then
		If l39num = 1 Then l39num = 0 : inslvl = inslvl + 1
	End If
	If l40.intensityscale > 0 Then 
		If l40num = 0 Then l40num = 1 : inslvl = inslvl - 1
	End If
	If l40.intensityscale = 0 Then
		If l40num = 1 Then l40num = 0 : inslvl = inslvl + 1
	End If
	If l41.intensityscale > 0 Then 
		If l41num = 0 Then l41num = 1 : inslvl = inslvl - 1
	End If
	If l41.intensityscale = 0 Then
		If l41num = 1 Then l41num = 0 : inslvl = inslvl + 1
	End If
	If l42.intensityscale > 0 Then 
		If l42num = 0 Then l42num = 1 : inslvl = inslvl - 1
	End If
	If l42.intensityscale = 0 Then
		If l42num = 1 Then l42num = 0 : inslvl = inslvl + 1
	End If
	If l43.intensityscale > 0 Then 
		If l43num = 0 Then l43num = 1 : inslvl = inslvl - 1
	End If
	If l43.intensityscale = 0 Then
		If l43num = 1 Then l43num = 0 : inslvl = inslvl + 1
	End If
	If l44.intensityscale > 0 Then 
		If l44num = 0 Then l44num = 1 : inslvl = inslvl - 1
	End If
	If l44.intensityscale = 0 Then
		If l44num = 1 Then l44num = 0 : inslvl = inslvl + 1
	End If
	If l45.intensityscale > 0 Then 
		If l45num = 0 Then l45num = 1 : inslvl = inslvl - 1
	End If
	If l45.intensityscale = 0 Then
		If l45num = 1 Then l45num = 0 : inslvl = inslvl + 1
	End If
	If l46.intensityscale > 0 Then 
		If l46num = 0 Then l46num = 1 : inslvl = inslvl - 1
	End If
	If l46.intensityscale = 0 Then
		If l46num = 1 Then l46num = 0 : inslvl = inslvl + 1
	End If
	If l47.intensityscale > 0 Then 
		If l47num = 0 Then l47num = 1 : inslvl = inslvl - 1
	End If
	If l47.intensityscale = 0 Then
		If l47num = 1 Then l47num = 0 : inslvl = inslvl + 1
	End If
	If l48.intensityscale > 0 Then 
		If l48num = 0 Then l48num = 1 : inslvl = inslvl - 1
	End If
	If l48.intensityscale = 0 Then
		If l48num = 1 Then l48num = 0 : inslvl = inslvl + 1
	End If
	If l49.intensityscale > 0 Then 
		If l49num = 0 Then l49num = 1 : inslvl = inslvl - 1
	End If
	If l49.intensityscale = 0 Then
		If l49num = 1 Then l49num = 0 : inslvl = inslvl + 1
	End If
	If l50.intensityscale > 0 Then 
		If l50num = 0 Then l50num = 1 : inslvl = inslvl - 1
	End If
	If l50.intensityscale = 0 Then
		If l50num = 1 Then l50num = 0 : inslvl = inslvl + 1
	End If
	If l51.intensityscale > 0 Then 
		If l51num = 0 Then l51num = 1 : inslvl = inslvl - 1
	End If
	If l51.intensityscale = 0 Then
		If l51num = 1 Then l51num = 0 : inslvl = inslvl + 1
	End If
	If l52.intensityscale > 0 Then 
		If l52num = 0 Then l52num = 1 : inslvl = inslvl - 1
	End If
	If l52.intensityscale = 0 Then
		If l52num = 1 Then l52num = 0 : inslvl = inslvl + 1
	End If
	If l53.intensityscale > 0 Then 
		If l53num = 0 Then l53num = 1 : inslvl = inslvl - 1
	End If
	If l53.intensityscale = 0 Then
		If l53num = 1 Then l53num = 0 : inslvl = inslvl + 1
	End If
	If l54.intensityscale > 0 Then 
		If l54num = 0 Then l54num = 1 : inslvl = inslvl - 1
	End If
	If l54.intensityscale = 0 Then
		If l54num = 1 Then l54num = 0 : inslvl = inslvl + 1
	End If
	If l55.intensityscale > 0 Then 
		If l55num = 0 Then l55num = 1 : inslvl = inslvl - 1
	End If
	If l55.intensityscale = 0 Then
		If l55num = 1 Then l55num = 0 : inslvl = inslvl + 1
	End If
	If l56.intensityscale > 0 Then 
		If l56num = 0 Then l56num = 1 : inslvl = inslvl - 1
	End If
	If l56.intensityscale = 0 Then
		If l56num = 1 Then l56num = 0 : inslvl = inslvl + 1
	End If
	If l57.intensityscale > 0 Then 
		If l57num = 0 Then l57num = 1 : inslvl = inslvl - 1
	End If
	If l57.intensityscale = 0 Then
		If l57num = 1 Then l57num = 0 : inslvl = inslvl + 1
	End If
	If l58.intensityscale > 0 Then 
		If l58num = 0 Then l58num = 1 : inslvl = inslvl - 1
	End If
	If l58.intensityscale = 0 Then
		If l58num = 1 Then l58num = 0 : inslvl = inslvl + 1
	End If
	If l59.intensityscale > 0 Then 
		If l59num = 0 Then l59num = 1 : inslvl = inslvl - 1
	End If
	If l59.intensityscale = 0 Then
		If l59num = 1 Then l59num = 0 : inslvl = inslvl + 1
	End If
	If l60.intensityscale > 0 Then 
		If l60num = 0 Then l60num = 1 : inslvl = inslvl - 1
	End If
	If l60.intensityscale = 0 Then
		If l60num = 1 Then l60num = 0 : inslvl = inslvl + 1
	End If
	If l61.intensityscale > 0 Then 
		If l61num = 0 Then l61num = 1 : inslvl = inslvl - 1
	End If
	If l61.intensityscale = 0 Then
		If l61num = 1 Then l61num = 0 : inslvl = inslvl + 1
	End If
	If l62.intensityscale > 0 Then 
		If l62num = 0 Then l62num = 1 : inslvl = inslvl - 1
	End If
	If l62.intensityscale = 0 Then
		If l62num = 1 Then l62num = 0 : inslvl = inslvl + 1
	End If
	If l63.intensityscale > 0 Then 
		If l63num = 0 Then l63num = 1 : inslvl = inslvl - 1
	End If
	If l63.intensityscale = 0 Then
		If l63num = 1 Then l63num = 0 : inslvl = inslvl + 1
	End If
	For each BGlight in TommyBG : BGlight.opacity = 40 + (inslvl/2.2) : Next
	For each BGlight in TommyBG2 : BGlight.opacity = 20 + (inslvl/4.5) : Next
End Sub

Dim Fl1, Fl2, Flasherseq1
FL1 = 0
FL2 = 0
VRBGBulb004.opacity = 0
VRBGBulb005.opacity = 0
VRBGBulb006.opacity = 0
VRBGBulb007.opacity = 0

Sub BGFlashersFast_Timer
	Select Case Flasherseq1 'Int(Rnd*6)+1
		Case 1:Fl1 = 1 : VRBGFL1lvl = 0.0

		Case 2:Fl1 = 2
		Case 10:Fl1 = 1 : VRBGFL1lvl = 0.05

		Case 11:Fl1 = 2
		Case 13:Fl2 = 1 : VRBGFL2lvl = 0.05
		Case 14:FL2 = 2
		Case 20:Fl1 = 1 : VRBGFL1lvl = 0.05
		Case 21:Fl1 = 2
		Case 26:Fl2 = 1 : VRBGFL2lvl = 0.05
		Case 27:FL2 = 2
		Case 30:Fl1 = 1 : VRBGFL1lvl = 0.05
		Case 31:Fl1 = 2
		Case 38:Fl2 = 1 : VRBGFL2lvl = 0.05
		Case 39:FL2 = 2
	End Select
	Flasherseq1 = Flasherseq1 + 1
	If Flasherseq1 > 41 Then
	Flasherseq1 = 1
	End if
End Sub


dim VRBGFL1lvl, VRBGFL2lvl

sub VRBGFLTimer_timer
	If FL1 = 1 Then
		If VRBGFL1lvl < 0.15 Then
			VRBGFL1lvl = 4 * VRBGFL1lvl + 0.01
		Else	
			VRBGFL1lvl = 1.25 * VRBGFL1lvl + 0.01
		End If
		VRBGBulb004.opacity = 30 * VRBGFL1lvl^3
		VRBGBulb005.opacity = 20 * VRBGFL1lvl^4
		If VRBGFL1lvl > 1 Then
			VRBGFL1lvl = 1
			FL1 = 0
		End If
	End If
	If FL1 = 2 Then
		VRBGFL1lvl = 0.75 * VRBGFL1lvl - 0.01
		VRBGBulb004.opacity = 30 * VRBGFL1lvl^2
		VRBGBulb005.opacity = 20 * VRBGFL1lvl^3
		if VRBGFL1lvl < 0 then 
			VRBGFL1lvl = 0
			FL1 = 0
		End If
	End if
	If FL2 = 1 Then
		If VRBGFL2lvl < 0.15 Then
			VRBGFL2lvl = 4 * VRBGFL2lvl + 0.01
		Else	
			VRBGFL2lvl = 1.5 * VRBGFL2lvl + 0.01
		End If
		VRBGBulb006.opacity = 50 * VRBGFL2lvl^2
		VRBGBulb007.opacity = 35 * VRBGFL2lvl^3
		If VRBGFL2lvl > 1 Then
			VRBGFL2lvl = 1
			FL2 = 0
		End If
	End If
	If FL2 = 2 Then
		VRBGFL2lvl = 0.75 * VRBGFL2lvl - 0.01
		VRBGBulb006.opacity = 50 * VRBGFL2lvl^2
		VRBGBulb007.opacity = 35 * VRBGFL2lvl^3
		if VRBGFL2lvl < 0 then 
			VRBGFl2lvl = 0
			Fl2 = 0
		End If
	End if
End Sub


'VPW Mod Changelog:
'0.01 Wylte		- Changed default rom to 500, FastFlips enabled, PoV changed, Added Dynamic Ball Shadows (old), nFozzy physics (Progress: 1/4)
'0.02 apophis	- Reformatted script
'0.03 Wylte		- nFozzy physics progress (Parts 1-3 done), flipper positions adjusted to match RL table - currently 59 degrees rotation, from 124 to 65, LOTS of small alignment tweaks based on pf hole positions, commented out ramp boosts
'0.03b Wylte	- Right ramp and Mystery and Mirror chutes set to original material override for now, flipper rampup and return adjusted to match videos
'0.04 Wylte		- Fleep sound added, collections created, still need to go through and replace any "fx" sounds, Dynamic Ball Shadows updated to latest, TargetBouncer and Rubberizer added, some plastic walls set to collidable to prevent stuck balls
'0.05 apophis	- Fixed bumper sound effect call. Added better relay on and off sounds, and better spinner sound. Removed BIP variable. Various Fleep-related script fixes.
'0.06 Wylte		- Table slope lowered to 5-6, default flipperrampupmode 0>1, UL flipper tied to rampup and values tuned, right ramp entrance made slightly easier, Rubberizer1 y mult lowered 1.4>1.3, ramprolling and rolling amp sounds and code copied in, more alignment tweaks
'0.07 Skitso	- Completely reworked lighting, added Skitso style inserts, small improvements to flashers, overall polish tweaks.
'0.08 fluffhead35 - Tweaked Ball Rolling and ramp rolling logic
'0.09 Skitso	- Fixed right missing flasher, improved top left GI lighting (spots)
'0.10 Wylte		- PF Mesh, reworked holes, VUK/Scoop tuning, DynamicSources collection rebuilt for skitso lighting
'0.11 Skitso    - Fixed genius insert to be like it should.
'0.12 Skitso    - Removed the unbelievable mess, also known as the top red flashers (waiting for Fluppers). Remade backwall flashers, and captive ball flashers. Tweaked top corner walls, plastics and GI to be more like the real thing. Reverted GI lighting a small step back towards more yellow.
'0.13 Wylte		- Hole walls and triggers tuned, FrameTimer turns off if not in use, Left ramp bottom widened to hit target on brick, outlane difficulty options fixed to actually work now, ramp frictions lowered to increase return speed, Eject hole kickout force increased 8->10
'0.14 Wylte		- Primitive inserts, other small changes I've probably forgotten by now
'0.14.1	Wylte	- Skitso inserts added to Lampz, materials/textures/objects for flupper flashers imported
'0.15 apophis	- Added Flupper domes.
'0.16 apophis	- Added physical trough. Removed all GetBalls calls. Updated to latest ball and ramp rolling scripts and sounds. Updated to latest physics scripts. Moved physics options away from player options area. Fine-tuned flipper trigger shapes. Adjusted slingshot animation and strength. Adjusted playfield friction.
'0.17 Wylte		- Misc. fixes, Ball shadows updated, bumper and sign lights added to Lampz, center nudge reduced, some light tuning
'0.18 apophis	- Fixed bumper cap lighting. Added TLxx flasher objects to Lampz.
'0.19 bord		- Fixed VUK hole/ball interaction.
'0.20 apophis	- Fixed bulb1 and bulb2 lamp assignments. Added some missing PF flashers. Updated playfield image with cleaner hole edges. Put insert overlay on ramp instead of flasher (so that it interacts with lights). Tweaks on the inserts.
'0.21 Wylte		- Better pf mesh, ramps to mirror hole and kicker, match right ramp points, hole ramp tuning, cover ramp behind mirror
'0.22 apophis	- Fixed material assignment on Flasherlit objects
'0.23 Wylte		- Organizing Lampz, insert tuning, mirror wall adjust, outlane post default+light shape changed to Difficult, default slope changed to 5.75 (range 5.5-6.5), Mystery bulb color changed (primitive still too light/purple), other fine-tooth comb tweaks
'0.24 tomate    - Some fixes on the wireRamps prims, added 3 new wireRamps textures (medium, high and very high contrast)
'0.25 Wylte		- Flipper tuning, Sound stuff (a lot), ramp rolling logic triggers changed, upped intensity of spotlight landing, Gi_Bulb035 added (spotlight pointed at mirror), ShootTrigger deleted (ball drop sound dummy), dummy animation flippers renamed for clarity, sling animation sped up
'0.26 leojreimroc - VR Room and backglass
'0.27 Sixtoe 	- Set playfield opacity amount to 1 (to stop lights swimming), ticked "metal" in metal material (it was just white), added TextBox1.visible = 0 when vrroom > 0 as the extra dmd is being show from the desktop page (that's what it's called there), turned DL to 0.2 for PinCab_Backbox and PinCab_Cabinet, added -10h wall and used that as the surface for all the rollover triggers to drop them below the playfield, cut out rollover and drop target holes in playfield and added drop walls for added depth, added union jack speaker grills, set flasher blooms to use different images as all were using "centreleft", changed white blooms to white colour, tried to tidy up cabinet image but bailed, added underplayfield flasher with colour_dark texture (you can't use black or it masks it out), added bloom layer to all inserts on layer 9 and turned down DL on all inserts (not playing here if skitso is going to have a look)
'0.28 apophis	- Adjusted flipper trigger shapes. Modified DynamicSources collection. Wired GI collection to Lampz. Updated desktop backdrop image and Env Emission settings (thanks HauntFreaks). Carved out Gi_Bulb012 to accommodate adjacent inserts. Forced PF slope to 5.7 (was 5.75 selected by difficultly level)
'0.29 Wylte		- Put text inserts back on flasher, gave blinders a bit of DL and separation, added TargetBouncer to targets, checked posts, pegs, & code (no reason for weird R outlane bounce I can see), HDR ball, random options fixed, slings and pops weakened, wrapped sling rubbers around proper primitive (fucking tedious!)
'0.30 leojreimroc - Added fade timer to backglass GI to match table GI.  Fixed VR Couch Shadow.
'0.31 apophis	- Fixed my copy/paste error in GIRelay sub
'0.32 Rawd		- New cabinet and Backbox textures (Not perfect but I think much better), New Topper - 11,000 polys less, and much smoother, Added VR under apron blocker, Adjusted DL on cabinet and flipper button parts, Put in the Tommy VR Flyer
'0.33 Wylte		- Sling position tweaked, various rubbers tightened, Added: Rawd VR Cab art, Darkstar apron, HauntFreaks blinders
'0.34 Wylte		- New sling angle code, installed roth standup targets, modified metal walls appearance, plunger lane ramps, slingshot arms don't clip now, various other nitpicks
'0.35 Rawd		- Shaped all lights on layer 6 and 7 inside the cabinet.  PLEASE make sure they are still shaped OK and look good in Desktop/FS.  (I didn't move them, just shaped them, so I assume they will look the same)
'				- Moved one backglass flasher that was sticking out the side of the backbox when flashing - 'VRBGFL15_14'
'				- Adjusted Minimal room floors/walls/ceiling so they are 90 degrees with the world, and all leg levelers are seen
'0.36 Wylte		- Fixing targets - silverballs done separately, PT code unused, slings fixed?
'0.37 Wylte		- Blinder wall, sling deadzone and correction angle, stuck ball spot, apron image, silverball targets, ramp return, right scoop posts widened, flipper rail height, medium post setting (default)
'0.38 Skitso	- Apron brightness adjustment, numerous small visual tweaks, touched all inserts, small GI adjustments
'0.39 Wylte		- Metal material metals, wire ramps darkened, ball scratches buffed, sling and bumper sol subs emptied, bumper threshold lowered, tilt sense up, stuck ball posts tweaked
'0.40 apophis	- Made Inserts intensity update with GI alvl so that the lights look nicer when GI is off.
'0.41 Wylte		- Insert image cleaned up, apron top collidable wall added
'0.42 apophis	- Added ball location checker including Narnia ball recovery.
'0.43 tomate    - Wooden walls remapped
'				- Geometry of the two metal ramps modified and new baked textures added, changed wall "Arch1" to metal0.2 (as it was in previous versions)
'RC1 Wylte		- Stuck ball blocker walls, digital plunger pull speed lowered, clipping post hidden, walls under primitive wires hidden
'RC1.1 Wylte  	- Adjusted insert intensity during GI off
'RC1.2 apophis  - Removed stray GI light in VR room, adjusted wall above upper right bumper to prevent ball stucks. Adjusted lights around bulb1 thru bulb4
'v1.0 Release
'1.0.1 Wylte	- Collections cleaned up, TNTMod fixed (except for the bright LEDs, those are (still) gone), random options no longer constants, inlane clear opacity lowered 0.4->0.1
'1.0.2 leojreimroc - Slight VR Backglass lighting adjustments
'1.0.3 Wylte	- New new ramp models from Tomate, physical ramps tuned, plastic roof added to left ramp, TNTmod bulbs given lights (tied to propeller spin)
'1.0.4 Wylte	- Wire to skill shot added, knocker moved, outer walls made metal, mirror and R ramp reject walls tuned, VUK area rebuilt, shaped silverball target (sigma bonus possible now), targetbouncer way up, many visual settings set to match mine :)
'1.0.5 Wylte	- Mystery scoop rebuilt, visible fly ball protector walls added
'1.0.6 Wylte	- Flipping pegs, re-aligning rubbers, fly ball walls finished, plastics tweaked, ramp bulbs sunk, slings given dead time again, airplane light started, lots of small tweaks that I've forgotten
'1.0.7 Wylte	- Migrated to 10.7, TargetO collisions fixed, starting flasher skillshot light, sound tuning, sling updates
'1.0.8 Wylte	- shadow update, improved plunger area (still some dirty plunges), physics/sound code updated
'1.0.9 Wylte	- Rail exit randomization, staged flipper
'1.0.10 Wylte	- Lighting tweaks, GI updates for VPX DBS, GrabbyKicker added to end mirror rejects, saucer improved, zcol_targets material updated, flippers sized and positioned more accurately, plunger/arch collision fixed, mirror bricking added,
'				- Bumpers skirt animation added, bumper hit size/sensitivity tweaked, Skillshot flashers tuned, rubbers more bouncy, various tweaks
'1.0.11 Wylte	- Washers visibility option added, positions fixed, bumper light heights set, holes cut for under-ramp bulbs, GI tweaks, saucer even grabbier, post passes improved 
'1.0.12 Wylte	- Staged flipper board fix (thanks apophis), bumpers further tweaked, subway sounds, rolling sounds updated, Roth targets finished (and fixed), ball options, inlane post options, misc tweaking
'1.0.13 Wylte	- Automate VR, Lampz update, Flasher fixes
'1.0.14 leojreimroc - VR Backglass level adjustments.  Added flickering "Tommy" on backglass. Added 2 blinking lights option.
'1.0.15 Wylte	- Right ramp fixed, slope increased, flasher tuning, insert tuning, ramp signs dl, Ramp sign blocker walls, VUK acrylic positioned to roll stuck balls, launch/orbit protective acrylic added
'1.0.16 Wylte	- Shadow-casting lights dropped to playfield height in VR, flipper polarity code update (eliminate endpoints), code updates for VPX Standalone, autoplunger sound change, small tweaks
'1.1 Wylte		- Sign dl moved to Lampz, bumper Tommy intensity lowered, right orbit fixed/reverted, shooter lane & exit fixed for real this time, blinder bottom collidable, inlane ball spin limiter/randomizer, plastic hole, screw placement, defaults set for release
'1.1.1 Wylte	- Min slope increased, flipper power increased to top of 90's, slingshot force increased, autoplunger power increased
'1.1.1b Rawd	- New cabinet graphics, front buttons adjusted slightly
'1.2 Wylte		- Slope and flipper strength backed off slightly, ramp sign transparency fix, ramps given proper physics material (slightly higher friction for higher flipper strength), bumper force increased again, slings given some falloff, fix inlane spin speed limiters, Release
'1.2.1 Wylte	- Re-wrote button code for staged flippers and VR fixes and fixed UL flipper sounds (thanks Retro27), improved inlane spin/speed code, ramp entry trigger heights increased (hopefully prevent occasional silent ramps)
