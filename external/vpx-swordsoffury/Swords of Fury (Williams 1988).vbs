'*************************************
'Swords of Fury (Williams 1988) - IPDB No. 2486
'VPX by bord
'VR Room
'VR Room Minimal, Cab, and Backglass by Uncle Paulie/Sixtoe
'Based heavily on a script by rothbauerw
'************************************


' Updates by UnclePaulie

' v1	Upper PF RightFlipper001 needed slight adjustment in size and angle.  Changed radius to match flippers at bottom. Adjusted length from 80 to 78.5.  
'		 base radius of 20, end of 12.5.  Also the start and end angles needed to be adjusted, as you should be able to cradle the ball.  -121.5 and -75.5
' 		LeftFlipper001 wasn't visible.  Turned on.
' 		Automated DesktopMode and cabmode.  VR mode still selectable. (defaulted to off)
' v2	Added desktop displays
' v3	Removed the huge space theme for VR.  Went to basic rooms.
' v4	Lots of mods to VR room
' v5	Added VR Backglass elements
' v6	Animated VR Backglass and tied to Lampz fading routines.
' v7	Complete redo of the desktop backglass.  Animated the jackpot lights.  New image, and adjusted the colors of the displays.
'		Fully animated VR backglass, and added new VR graphics
'		The pin between the flippers wasn't collidable (only visible prims / toys).  Added a collidable prim.
'		Major issue corrected of ball getting stuck behind the top left upper playfield. 
'		  When you hit the drop targets on upper playfield, and finally break through... sometimes the ball will get stuck underneath in the rampsubway
'		  I added a wall and a ramp to ensure ball rolls out of subway correctly.
'		Finished the VR digit displays
' v8	Fixed the sling1 and sling2 prim.  Out of place, and the right one was rotated weird.
'		Added option to reduce the renderbrightness on the fXX_render flashers (They were super bright in VR)
'		Adjusted the desktop pov
'		Added lflip002.rotz = leftflipper001.currentangle
'		I enabled the mesh on the GI bulbs.  Really looked weird in VR having open holes where lights were.  
' v9	Removed BOT and Getballs calls, and replaced with global gBOT
'		Updated the script to be in line with latest VPW, including ramprolling and ballrolling, shadows, drop targets, fleep sounds, etc.
'		Added Slingshot corrections

'	If you have better light bulb prims... just using the generic ones now.
'	The standup targets are a little weird.  You have the pswXX still there, but it looks like you are using that for the GI off.  
'	  The standup targets dont' seem to animate like other roth standup target solutions.

' Apophis updates
'   Added slingshot correction calls to _slingshot subs. Tuned strength.
'   Added flipper tricks and flipper correction object to upper left flipper
'   Reduced digital nudge strength. Increased tilt Sensitivity
'   Fixed some positional sound issues
'

' Additional Uncle Paulie Updates:
' 004	Updated the dynamic shadow code per latest VPW updates... small correction.

'bord
' 005 Updated upper playfield positioning, overall friction, slope 

Option Explicit
Randomize

' ****************************************************
' OPTIONS
' ****************************************************

'----- Desktop, VR, and Cabinet Options -----

Const VR_Room = 0					'0 = Desktop/Cabinet/FSS, 1 VR Room Minimal
	Dim cabmode, DesktopMode: DesktopMode = table1.ShowDT
	If Not DesktopMode and VR_Room=0 Then cabmode=1 Else cabmode=0  'Cabinet Mode turns off cabinet side rails, lockdown bar, and backbox


' Under Plastic Flasher Brightness (Recommend lower value for VR users, and higher value for desktop / cab users)
dim renderopacity

if VR_Room = 1 Then
  renderopacity = 15 'default is 15
Else
  renderopacity = 100 'default is 100.  Bord orginally had at 100
End if


' *** If using VR Room:

const CustomWalls = 0 'set to 0 for Modern Minimal Walls, floor, and roof, 1 for Sixtoe's original walls and floor
const WallClock = 1	  '1 Shows the clock in the VR minimal rooms only
const topper = 1		 '0 = Off 1= On - Topper visible in VR Room only
const poster = 1		 '1 Shows the flyer posters in the VR room only 
const poster2 = 1		 '1 Shows the flyer posters in the VR room only 

'----- General Sound Options -----

Const VolumeDial = 0.8		'Values 0-1: global volume multiplier for mechanical sounds 
Const BallRollVolume = 0.5 			'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5 			'Level of ramp rolling volume. Value between 0 and 1

'----- Shadow Options -----

Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's)
									'2 = flasher image shadow, but it moves like ninuzzu's

' ****************************************************
' END OPTIONS
' ****************************************************

Dim Ballsize,BallMass
BallSize = 50
BallMass = 1
Const tnob = 4' total number of balls
Const lob = 0	

' using table width and height in script slows down the performance
dim tablewidth: tablewidth = Table1.width
dim tableheight: tableheight = Table1.height

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

LoadVPM "00990300","S11.VBS",3.10

'********************
'Standard definitions
'********************

Const cGameName = "swrds_l2"

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 1

Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""

'******************************************************
' 					TABLE INIT
'******************************************************

Dim ii, collobj, GIObj(30), GICount
Dim SOFBall1, SOFBall2, SOFBall3, gBOT

Sub Table1_Init
	SetLocale(1033)
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "LIONMAN" & chr(13) & "by bord"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 0
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With
	On Error Goto 0

	'************  Main Timer init  ******************** 

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	'************  Nudging   **************************

	vpmNudge.TiltSwitch=1
	vpmNudge.Sensitivity=7
	vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot)

	'************  Trough	**************************
	Set SOFBall1 = Slot1.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set SOFBall2 = Slot2.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set SOFBall3 = Slot3.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(SOFBall1,SOFBall2,SOFBall3)
	
	Controller.Switch(11) = 1
	Controller.Switch(12) = 1
	Controller.Switch(13) = 1

	'************  Misc Stuff  ******************	
	PinCab_Backglass.blenddisablelighting = 3

	PFGI(False)

	if VR_Room = 1 Then
		setup_backglass()
		SetBackglass
	End If

End Sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_Exit:Controller.Stop:End Sub

'******************************************************
' 						KEYS
'******************************************************


Sub Table1_KeyDown(ByVal KeyCode)
	If KeyCode = RightFlipperKey then VR_Cab_ButtonRight.transx = -8
	If KeyCode = LeftFlipperKey then VR_Cab_ButtonLeft.transx = 8

	If KeyCode = LeftFlipperKey then FlipperActivate LeftFlipper, LFPress : FlipperActivate LeftFlipper001, LFPress1
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress
	
	If keycode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull()
		TimerVRPlunger.Enabled = True
		TimerVRPlunger1.Enabled = False
		PinCab_Shooter.Y = -351
	End If

	if KeyCode = LeftTiltKey Then Nudge 90, 1.5:SoundNudgeLeft()
	if KeyCode = RightTiltKey Then Nudge 270, 1.5:SoundNudgeRight()
	if KeyCode = CenterTiltKey Then Nudge 0, 1.5:SoundNudgeCenter()

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	if keycode = StartGameKey then 
		soundStartButton()
		VR_Cab_startbutton.transy = 5
	End If

	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyCode = PlungerKey Then
		Plunger.Fire
		TimerVRPlunger.Enabled = False
        TimerVRPlunger1.Enabled = True
		PinCab_Shooter.Y = -351

		If controller.switch(14) Then
			SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()                        'Plunger release sound when there is no ball in shooter lane
		End If
	
	End If

	If KeyCode = RightFlipperKey then VR_Cab_ButtonRight.transx = 0
	If KeyCode = LeftFlipperKey then VR_Cab_ButtonLeft.transx = 0		

	If KeyCode = LeftFlipperKey then FlipperDeActivate LeftFlipper, LFPress : FlipperDeActivate LeftFlipper001, LFPress1
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress

	if keycode=StartGameKey then 		
		VR_Cab_startbutton.transy = 0
	End If

	If KeyUpHandler(keycode) Then Exit Sub
End Sub

'******************************************************
'					SOLENOIDS
'******************************************************

SolCallback(1) = "SolOuthole"							'1 - Outhole
SolCallback(2) = "ReleaseBall"							'2 - Ball Release
SolCallback(4) = "SolLock"								'4 - Lock Kicker
SolCallback(5)	= "dt2up"								'5 drop2
SolCallback(6) = "SolKick"								'6 - Left Kicker
solcallback(7) = "SolKnocker" 							'7 - Bell 
SolCallback(8)	= "dt3up"								'8 drop3
SolCallback(9) = "Flash9"								'9 top center flashers
SolCallback(10) = "PFGI"								'10 - GI Relay
'SolCallback(11) = "Flash11"							'11 backglass sword flasher
'SolCallback(13) = "Diverter"							'13 - Outlane Gate
SolCallback(14)="SolKickback"							'14 - outlane kickback
SolCallback(15)	= "dt1up"								'15 drop1
SolCallback(16)="Flash16" 								'16 kickback flasher
SolCallback(17)="BottomDiverter"
SolCallback(19)="TopDiverter"
SolCallback(21)	= "dt4up"								'21 drop4
SolCallback(22)	= "dt5up"								'22 drop5
SolCallback(25)="Flash25" 								'Sword and Stars Upper Playfield 1C, and VR BG Left sword monkey
SolCallback(26)="Flash26" 								'left loop flashers 2C
SolCallback(27)="Flash27" 								'tunnel flasher 3C
SolCallback(28)="Flash28" 								'ramp flasher 4C
SolCallback(29)="Flash29"								'left lockup flasher 5C
SolCallback(30)="Flash30" 								'balrog flasher 6C, and VR BG Upper Mid lighting chain
SolCallback(31)="Flash31" 								'left spinner flasher 7C
SolCallback(32)="Flash32" 								'shooter lane flasher 8C, and VR BG Main Monkey top
SolCallback(46)="Flash46"								'VR BG Small Lighting Chain
SolCallback(47)="Flash47"								'VR BG Small Lighting Chain (2nd one)


SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

'******************************************************
'			TROUGH BASED ON NFOZZY'S
'******************************************************

Sub Slot3_Hit():Controller.Switch(13) = 1:UpdateTrough:End Sub
Sub Slot3_UnHit():Controller.Switch(13) = 0:UpdateTrough:End Sub
Sub Slot2_Hit():Controller.Switch(12) = 1:UpdateTrough:End Sub
Sub Slot2_UnHit():Controller.Switch(12) = 0:UpdateTrough:End Sub
Sub Slot1_Hit():Controller.Switch(11) = 1:UpdateTrough:End Sub
Sub Slot1_UnHit():Controller.Switch(11) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If Slot1.BallCntOver = 0 Then Slot2.kick 60, 9
	If Slot2.BallCntOver = 0 Then Slot3.kick 60, 9
	Me.Enabled = 0
End Sub

'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub Drain_Hit()
	RandomSoundDrain drain
	UpdateTrough
	Controller.Switch(10) = 1
End Sub

Sub Drain_UnHit()
	Controller.Switch(10) = 0
End Sub

Sub SolOuthole(enabled)
	If enabled Then 
		Drain.kick 60,20
		'SoundSaucerKick 0, Drain
	End If
End Sub

Sub ReleaseBall(enabled)
	If enabled Then 
		RandomSoundBallRelease Slot1
		Slot1.kick 60, 7
		UpdateTrough
	End If
End Sub

'******************************************************
'				LEFT KICKER
'******************************************************

Sub SolLock(enabled)
	If enabled Then
		If Controller.Switch(54) Then
			SoundSaucerKick 1, sw54
		Else
			SoundSaucerKick 0, sw54
		End If
		sw54.kick 180 + Rnd*4, 6 + Rnd*4
		Controller.Switch(54) = 0
	End If
End Sub

Sub sw54_hit()
	Controller.Switch(54) = 1
	SoundSaucerLock	
End sub

'******************************************************
'				RIGHT KICKER
'******************************************************

Sub SolKick(enabled)
	If enabled Then
		SoundSaucerKick 1, sw55
		sw55.kick 0 + Rnd*4, 46 + Rnd*4
		Controller.Switch(55) = 0
	End If
End Sub

Sub sw55_hit()
	Controller.Switch(55) = 1
	SoundSaucerLock
End sub

'******************************************************
'				OUTLANE KICKER
'******************************************************

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = PI * (kangle - 90) / 180

	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub

Dim KickerBall

Sub sw57_Hit
	set KickerBall = activeball	
	Controller.Switch(57) = 1
	wire001.transz=-5
End Sub

Sub sw57_unHit
	Controller.Switch(57) = 0
	wire001.transz=0
End Sub

Sub SolKickback(Enable)
    If Enable then
		If sw57.ballcntover > 0 then
			KickBall KickerBall, 0, 34, 2, 2
		Else
		End If
		SoundSaucerKick 1, sw57
    End If
End Sub

'******************************************************
'				DROP TARGETS
'******************************************************

Sub dt1up(enabled)
	if enabled then
		RandomSoundDropTargetReset psw17
		DTRaise 17
	end if
End Sub

Sub dt2up(enabled)
	if enabled then
		RandomSoundDropTargetReset psw18
		DTRaise 18
	end if
End Sub

Sub dt3up(enabled)
	if enabled then
		RandomSoundDropTargetReset psw19
		DTRaise 19
	end if
End Sub

Sub dt4up(enabled)
	if enabled then
		RandomSoundDropTargetReset psw20
		DTRaise 20
	end if
End Sub

Sub dt5up(enabled)
	if enabled then
		RandomSoundDropTargetReset psw21
		DTRaise 21
	end if
End Sub


'******************************************************
'					RAMP DIVERTERS
'******************************************************

Sub BottomDiverter(Enabled)
 	If Enabled then
		Playsoundat "soloff", knockerposition
		bottomgate.rotatetoend
	Else
		Playsoundat "soloff", knockerposition
		bottomgate.rotatetostart
 	end if
End Sub

Sub TopDiverter(Enabled)
 	If Enabled then
		Playsoundat "soloff", knockerposition
		topgate.rotatetoend
	Else
		Playsoundat "soloff", knockerposition
		topgate.rotatetostart
 	end if
End Sub

'******************************************************
'					KNOCKER (BELL)
'******************************************************

'Modified to play bell instead of knocker
Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid 'Add knocker position object
	End If
End Sub

'******************************************************
'				SLINGSHOTS
'******************************************************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(activeball)
	vpmTimer.PulseSw 64
    RandomSoundSlingshotRight Sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 16
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
	LS.VelocityCorrect(activeball)
	vpmTimer.PulseSw 63
    RandomSoundSlingshotLeft Sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
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

'******************************************************
'				SWITCHES
'******************************************************

Sub bottomgate_Collide
	RandomSoundFlipperBallGuide
End Sub

Sub topgate_Collide
	RandomSoundFlipperBallGuide
End Sub

'Drop Targets
Sub Sw17_Hit:DTHit 17:End Sub
Sub Sw18_Hit:DTHit 18:End Sub
Sub Sw19_Hit:DTHit 19:End Sub
Sub Sw20_Hit:DTHit 20:End Sub
Sub Sw21_Hit:DTHit 21:End Sub

'Stand Up Targets
Sub Tsw26_Hit:STHit 26: End Sub
Sub Tsw27_Hit:STHit 27: End Sub
Sub Tsw28_Hit:STHit 28: End Sub
Sub Tsw29_Hit:STHit 29: End Sub
Sub Tsw30_Hit:STHit 30: End Sub
Sub Tsw31_Hit:STHit 31: End Sub
Sub Tsw32_Hit:STHit 32: End Sub

'Wire Triggers
Sub sw14_Hit:Controller.Switch(14)=1:End Sub
Sub sw14_unHit:Controller.Switch(14)=0:End Sub
Sub sw25_Hit:Controller.Switch(25)=1:wire002.transz=-5:End Sub
Sub sw25_unHit:Controller.Switch(25)=0:wire002.transz=0:End Sub
Sub sw48_Hit:Controller.Switch(48)=1:wire003.transz=-5:End Sub
Sub sw48_unHit:Controller.Switch(48)=0:wire003.transz=0:End Sub
Sub sw56_Hit:Controller.Switch(56)=1:End Sub
Sub sw56_unHit:Controller.Switch(56)=0:End Sub
Sub sw59_Hit:Controller.Switch(59)=1:wire004.transz=-5:End Sub
Sub sw59_unHit:Controller.Switch(59)=0:wire004.transz=0:End Sub

'Ramp Switches
Sub sw15_Hit:vpmTimer.PulseSw 15:End Sub
Sub sw16_Hit:vpmTimer.PulseSw 16:End Sub
Sub sw38_Hit:vpmTimer.PulseSw 38:End Sub
Sub sw46_Hit:vpmTimer.PulseSw 46:End Sub
Sub sw47_Hit:vpmTimer.PulseSw 47:End Sub

'Loop Switches
Sub sw61_Hit:vpmTimer.PulseSw 61:End Sub
Sub sw62_Hit:vpmTimer.PulseSw 62:End Sub

'Spinners
Sub sw23_Spin():VPMTimer.PulseSw 23:SoundSpinner sw23:End Sub
Sub sw24_Spin():VPMTimer.PulseSw 24:SoundSpinner sw24:End Sub
Sub sw39_Spin():VPMTimer.PulseSw 39:SoundSpinner sw39:End Sub

'Rubber Switchtes
Sub sw37_Hit:vpmTimer.PulseSw 37:End Sub
Sub sw40_Hit:vpmTimer.PulseSw 40:End Sub

'******************************************************
'                DROP TARGETS INITIALIZATION
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

'Set array with drop target objects

dim DT17, DT18, DT19, DT20, DT21
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
'         primary:                         primary target wall to determine drop
'        secondary:                        wall used to simulate the ball striking a bent or offset target after the initial Hit
'        prim:                                primitive target used for visuals and animation
'                                                        IMPORTANT!!! 
'                                                        rotz must be used for orientation
'                                                        rotx to bend the target back
'                                                        transz to move it up and down
'                                                        the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'        switch:                                ROM switch number
'        animate:                        Arrary slot for handling the animation instrucitons, set to 0
'
'        Values for annimate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 

Set DT17 = (new DropTarget)(sw17, sw17y, psw17, 17, 0, false)
Set DT18 = (new DropTarget)(sw18, sw18y, psw18, 18, 0, false)
Set DT19 = (new DropTarget)(sw19, sw19y, psw19, 19, 0, false)
Set DT20 = (new DropTarget)(sw20, sw20y, psw20, 20, 0, false)
Set DT21 = (new DropTarget)(sw21, sw21y, psw21, 21, 0, false)

'Add all the Drop Target Arrays to Drop Target Animation Array
' DTAnimationArray = Array(DT1, DT2, ....)
Dim DTArray
DTArray = Array(DT17, DT18, DT19, DT20, DT21)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110							'in milliseconds
Const DTDropUpSpeed = 40						'in milliseconds
Const DTDropUnits = 49							'VP units primitive drops
Const DTDropUpUnits = 5							'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8								'max degrees primitive rotates when hit
Const DTDropDelay = 20							'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40							'time in milliseconds before target drops back to normal up position after the solendoid fires to raise the target
Const DTBrickVel = 30							'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0							'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 							'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 			'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 			'Drop Target reset sound

Const DTMass = 0.2								'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)

	PlayTargetSound
	DTArray(i).animate =  DTCheckBrick(Activeball,DTArray(i).prim)
	If DTArray(i).animate = 1 or DTArray(i).animate = 3 or DTArray(i).animate = 4 Then
		DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass
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

Function DTAnimate(primary, secondary, prim, switch,  animate)
	dim transz, switchid
	Dim animtime, rangle

	switchid = switch

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
			controller.Switch(Switchid) = 1
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
			Dim b', BOT
'			BOT = GetBalls

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
		controller.Switch(Switchid) = 0

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
Dim ST26, ST27, ST28, ST29, ST30, ST31, ST32

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0

Set ST26 = (new StandupTarget)(tsw26, primt26,26, 0)
Set ST27 = (new StandupTarget)(tsw27, primt27,27, 0)
Set ST28 = (new StandupTarget)(tsw28, primt28,28, 0)
Set ST29 = (new StandupTarget)(tsw29, primt29,29, 0)
Set ST30 = (new StandupTarget)(tsw30, primt30,30, 0)
Set ST31 = (new StandupTarget)(tsw31, primt31,31, 0)
Set ST32 = (new StandupTarget)(tsw32, primt32,32, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST26, ST27, ST28, ST29, ST30, ST31, ST32)

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
'						FLIPPERS
'******************************************************
Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire
		LF1.Fire
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If   
	Else
		LeftFlipper.RotateToStart
		leftflipper001.rotatetostart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub
 
Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend
		RightFlipper001.rotatetoend
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		RightFlipper001.rotatetostart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If        
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

'******************************************************
'                        FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks LeftFlipper001, LFPress1, LFCount1, LFEndAngle1, LFState1
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b

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


dim LFPress, LFPress1, RFPress, LFCount, LFCount1, RFCount
dim LFState, LFState1, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle, LFEndAngle1

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
LFState1 = 1
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
Const EOSReturn = 0.045  'late 70's to mid 80's

LFEndAngle = Leftflipper.endangle
LFEndAngle1 = Leftflipper001.endangle
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
' 				FLIPPER COLLIDE
'******************************************************

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
 	RightFlipperCollide parm
End Sub

Sub LeftFlipper001_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper001, LFCount1, parm
	LeftFlipperCollide parm
End Sub

'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim LF1 : Set LF1 = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, LF1, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 80
	Next


'	AddPt "Polarity", 0, 0, 0
'	AddPt "Polarity", 1, 0.05, -2.7        
'	AddPt "Polarity", 2, 0.33, -2.7
'	AddPt "Polarity", 3, 0.37, -2.7        
'	AddPt "Polarity", 4, 0.41, -2.7
'	AddPt "Polarity", 5, 0.45, -2.7
'	AddPt "Polarity", 6, 0.576,-2.7
'	AddPt "Polarity", 7, 0.66, -1.8
'	AddPt "Polarity", 8, 0.743, -0.5
'	AddPt "Polarity", 9, 0.81, -0.5
'	AddPt "Polarity", 10, 0.88, 0

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -3.7        
	AddPt "Polarity", 2, 0.33, -3.7
	AddPt "Polarity", 3, 0.37, -3.7
	AddPt "Polarity", 4, 0.41, -3.7
	AddPt "Polarity", 5, 0.45, -3.7 
	AddPt "Polarity", 6, 0.576,-3.7
	AddPt "Polarity", 7, 0.66, -2.3
	AddPt "Polarity", 8, 0.743, -1.5
	AddPt "Polarity", 9, 0.81, -1
	AddPt "Polarity", 10, 0.88, 0

	addpt "Velocity", 0, 0,         1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41,         1.05
	addpt "Velocity", 3, 0.53,         1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03,         0.945

	LF.Object = LeftFlipper        
	LF.EndPoint = EndPointLp
	LF1.Object = LeftFlipper001        
	LF1.EndPoint = EndPointLp001
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerLF1_Hit() : LF1.Addball activeball : End Sub
Sub TriggerLF1_UnHit() : LF1.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub



'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

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
'  FLIPPER POLARITY. RUBBER DAMPENER, AND SLINGSHOT CORRECTION SUPPORTING FUNCTIONS 
'******************************************************


Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, LF1, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub


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
'****  PHYSICS DAMPENERS
'******************************************************

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
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.8 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2

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
Sub TargetBounce_Hit(idx)
	TargetBouncer activeball, 1
End Sub



'******************************************************
'	REAL-TIME UPDATES (Ball Rolling, Shadows, Etc)
'******************************************************

' ***** Physics, Animations, Shadows, and Sounds
Sub GameTimer_Timer()
	Cor.Update			'Dampener
	DoDTAnim			'Drop Target Animations
	DoSTAnim			'Stand Up Target Animations
	UpdateMechs			'Flipper, gates, and plunger updates
	RollingUpdate
End Sub


' ***** Lights Flashers and Scoring Display
Sub FrameTimer_Timer()
	LampTimer
	Lampztimer
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows

    If VR_Room = 0 and cabmode = 0 Then
        DisplayTimer
	Else
		VRDisplayTimer
	End If

End Sub


'*****************************************
'	MECHANICAL UPDATES
'*****************************************

sub UpdateMechs ()
	batleftshadow.rotz = LeftFlipper.CurrentAngle
	batrightshadow.rotz  = RightFlipper.CurrentAngle
	lflip.rotz = leftflipper.currentangle
	rflip.rotz = rightflipper.currentangle
	lflip001.rotz = leftflipper001.currentangle
	gate001.rotx = ballreleasegate002.currentangle
	gate003.rotx = ballreleasegate001.currentangle
	gate002.rotx = ballreleasegate003.currentangle
	diverter001.rotz = topgate.currentangle
	diverter002.rotz = bottomgate.currentangle
	psw26a.transy = primt26.transy
	psw27a.transy = primt27.transy
	psw28a.transy = primt28.transy
	psw29a.transy = primt29.transy
	psw30a.transy = primt30.transy
	psw31a.transy = primt31.transy
	psw32a.transy = primt32.transy

End Sub


'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 0		'Offset y position under ball	 (for example 5,5 if the light is in the back left corner)

' *** Required Functions

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

Dim DSSources(30), numberofsources

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(5), objrtx2(5)
dim objBallShadow(5)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)

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

Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
'	Dim BOT: BOT=getballs

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
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If gBOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + offsetY
				objBallShadow(s).visible = 1

				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + BallSize/5
				BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				BallShadowA(s).visible = 1
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + offsetY
'				objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If gBOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + BallSize/5
				BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				BallShadowA(s).visible = 1
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				BallShadowA(s).Y = gBOT(s).Y + Ballsize/10 + offsetY
				BallShadowA(s).height=gBOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If
' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).X < 850 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
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
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************



'******************************************************
'				Lights & Flashers
'******************************************************

Dim Sol9lvl
Dim Sol10lvl
Dim Sol16lvl
Dim Sol25lvl
Dim Sol26lvl
Dim Sol27lvl
Dim Sol28lvl
Dim Sol29lvl
Dim Sol31lvl
Dim Sol32lvl

f9_render.visible = false
f16_render.visible = false
f16_wall.visible = false
f25_render.visible = false
f26_render.visible = false
f27_render.visible = False
f28_render.visible = False
f29_render.visible = False
f29_wall.visible = false
f31_render.visible = False
f31_wall.visible = false
f32_render.visible = false

sub Flash9(enabled)
	If enabled Then
		Lampz.state(109) = 1
		Sol9lvl = 1
		f9_render_timer
	else
		Lampz.state(109) = 0
		Sol9lvl = Sol9lvl * 0.7 'minor tweak to force faster fade
	End If
	Sound_Flash_Relay enabled, Relay_9
end sub


sub f9_render_timer
	if Not f9_render.TimerEnabled then				' This is here to detect the first call to this timer
		f9_render.visible = true
		f9_render.TimerEnabled = true				' enabling this timer
	end if

	'debug.print Sol9lvl

	f9_render.opacity = renderopacity * Sol9lvl^2

	Sol9lvl = 0.9 * Sol9lvl - 0.01			' Fading equation. 0.9 is rather slow. 0.6 is faster. Example 0.99 is too slow, but good for demo
	if Sol9lvl < 0 then Sol9lvl = 0			' failsafe so we are not going under 0

	if Sol9lvl =< 0 Then
		f9_render.visible = false
		f9_render.TimerEnabled = false				' disabling this timer
	end if

end sub

sub Flash16(enabled)
	If enabled Then
		Sol16lvl = 1
		f16_render_timer
		Lampz.state(116) = 1
	else
		Lampz.state(116) = 0
		Sol16lvl = Sol16lvl * 0.7 'minor tweak to force faster fade
	End If
	Sound_Flash_Relay enabled, Relay_16
end sub


sub f16_render_timer
	if Not f16_render.TimerEnabled then				' This is here to detect the first call to this timer
		f16_render.visible = true
		f16_wall.visible = true
		f16_render.TimerEnabled = true				' enabling this timer
	end if

	'debug.print Sol16lvl

	f16_render.opacity = renderopacity * Sol16lvl^2
	f16_wall.opacity = 125 * Sol16lvl^2

	Sol16lvl = 0.9 * Sol16lvl - 0.01			' Fading equation. 0.9 is rather slow. 0.6 is faster. Example 0.99 is too slow, but good for demo
	if Sol16lvl < 0 then Sol16lvl = 0			' failsafe so we are not going under 0

	if Sol16lvl =< 0 Then
		f16_render.visible = false
		f16_wall.visible = false
		f16_render.TimerEnabled = false				' disabling this timer
	end if
end sub

sub Flash25(enabled)
	If enabled Then
		Sol25lvl = 1
		f25_render_timer
		Lampz.state(125) = 1
	else
		Sol25lvl = Sol25lvl * 0.7 'minor tweak to force faster fade
		Lampz.state(125) = 0
		bgfl25.visible = 0
	End If
	Sound_Flash_Relay enabled, Relay_9
end sub



sub f25_render_timer
	if Not f25_render.TimerEnabled then				' This is here to detect the first call to this timer
		f25_render.visible = true
		f25_render.TimerEnabled = true				' enabling this timer
	end if

	'debug.print Sol25lvl

	f25_render.opacity = renderopacity * Sol25lvl^2

	Sol25lvl = 0.9 * Sol25lvl - 0.01			' Fading equation. 0.9 is rather slow. 0.6 is faster. Example 0.99 is too slow, but good for demo
	if Sol25lvl < 0 then Sol25lvl = 0			' failsafe so we are not going under 0

	if Sol25lvl =< 0 Then
		f25_render.visible = false
		f25_render.TimerEnabled = false				' disabling this timer
	end if
end sub

sub Flash26(enabled)
	If enabled Then
		Sol26lvl = 1
		f26_render_timer
		Lampz.state(126) = 1
	else
		Lampz.state(126) = 0
		Sol26lvl = Sol26lvl * 0.7 'minor tweak to force faster fade
	End If
	Sound_Flash_Relay enabled, Relay_16
end sub


sub f26_render_timer
	if Not f26_render.TimerEnabled then				' This is here to detect the first call to this timer
		f26_render.visible = true
		f26_render.TimerEnabled = true				' enabling this timer
	end if

	'debug.print Sol26lvl

	p23.blenddisablelighting = 120 * Sol26lvl^1
	p24.blenddisablelighting = 120 * Sol26lvl^1
	p23o.blenddisablelighting = 20 * Sol26lvl^1
	p24o.blenddisablelighting = 20 * Sol26lvl^1
	f26_render.opacity = 227 * Sol27lvl^2

	Sol26lvl = 0.9 * Sol26lvl - 0.01			' Fading equation. 0.9 is rather slow. 0.6 is faster. Example 0.99 is too slow, but good for demo
	if Sol26lvl < 0 then Sol26lvl = 0			' failsafe so we are not going under 0

	if Sol26lvl =< 0 Then
		p23.blenddisablelighting = 0
		p24.blenddisablelighting = 0
		p23o.blenddisablelighting = 0 * Sol26lvl^1
		p24o.blenddisablelighting = 0 * Sol26lvl^1
		f26_render.visible = false
		f26_render.TimerEnabled = false				' disabling this timer
	end if
end sub

sub Flash27(enabled)
	If enabled Then
		Sol27lvl = 1
		Lampz.state(127) = 1
		f27_render_timer
	else
		Lampz.state(127) = 0
		Sol27lvl = Sol27lvl * 0.7 'minor tweak to force faster fade
	End If
	Sound_Flash_Relay enabled, Relay_9
end sub


sub f27_render_timer
	if Not f27_render.TimerEnabled then				' This is here to detect the first call to this timer
		f27_render.visible = true
		f27_render.TimerEnabled = true				' enabling this timer
	end if

	'debug.print Sol27lvl

	f27_render.opacity = renderopacity * Sol27lvl^2

	Sol27lvl = 0.9 * Sol27lvl - 0.01			' Fading equation. 0.9 is rather slow. 0.6 is faster. Example 0.99 is too slow, but good for demo
	if Sol27lvl < 0 then Sol27lvl = 0			' failsafe so we are not going under 0

	if Sol27lvl =< 0 Then
		f27_render.visible = false
		f27_render.TimerEnabled = false				' disabling this timer
	end if
end sub

sub Flash28(enabled)
	If enabled Then
		Sol28lvl = 1
		Lampz.state(128) = 1
		f28_render_timer
	else
		Lampz.state(128) = 0
		Sol28lvl = Sol28lvl * 0.7 'minor tweak to force faster fade
	End If
	Sound_Flash_Relay enabled, Relay_9
end sub


sub f28_render_timer
	if Not f28_render.TimerEnabled then				' This is here to detect the first call to this timer
		f28_render.visible = true
		f28_render.TimerEnabled = true				' enabling this timer
	end if

	'debug.print Sol28lvl

	f28_render.opacity = renderopacity * Sol28lvl^2

	Sol28lvl = 0.9 * Sol28lvl - 0.01			' Fading equation. 0.9 is rather slow. 0.6 is faster. Example 0.99 is too slow, but good for demo
	if Sol28lvl < 0 then Sol28lvl = 0			' failsafe so we are not going under 0

	if Sol28lvl =< 0 Then
		f28_render.visible = false
		f28_render.TimerEnabled = false				' disabling this timer
	end if
end sub

sub Flash29(enabled)
	If enabled Then
		Sol29lvl = 1
		f29_render_timer
		Lampz.state(129) = 1
	else
		Lampz.state(129) = 0
		Sol29lvl = Sol29lvl * 0.7 'minor tweak to force faster fade
	End If
	Sound_Flash_Relay enabled, Relay_9
end sub


sub f29_render_timer
	if Not f29_render.TimerEnabled then				' This is here to detect the first call to this timer
		f29_render.visible = true
		f29_wall.visible = true
		f29_render.TimerEnabled = true				' enabling this timer
	end if

	'debug.print Sol29lvl

	f29_render.opacity = renderopacity * Sol29lvl^2
	f29_wall.opacity = 129 * Sol29lvl^2

	Sol29lvl = 0.9 * Sol29lvl - 0.01			' Fading equation. 0.9 is rather slow. 0.6 is faster. Example 0.99 is too slow, but good for demo
	if Sol29lvl < 0 then Sol29lvl = 0			' failsafe so we are not going under 0

	if Sol29lvl =< 0 Then
		f29_render.visible = false
		f29_wall.visible = false
		f29_render.TimerEnabled = false				' disabling this timer
	end if
end sub

sub Flash30(Enabled)
	If Enabled Then
		Lampz.state(130) = 1
	Else
		Lampz.state(130) = 0
		bgfl30.visible = 0
	End If
End Sub

sub Flash31(enabled)
	If enabled Then
		Lampz.state(131) = 1
		Sol31lvl = 1
		f31_render_timer
	else
		Lampz.state(131) = 0
		Sol31lvl = Sol31lvl * 0.7 'minor tweak to force faster fade
	End If
	Sound_Flash_Relay enabled, Relay_9
end sub


sub f31_render_timer
	if Not f31_render.TimerEnabled then				' This is here to detect the first call to this timer
		f31_render.visible = true
		f31_wall.visible = true
		f31_render.TimerEnabled = true				' enabling this timer
	end if

	'debug.print Sol31lvl

	f31_render.opacity = renderopacity * Sol31lvl^2
	f31_wall.opacity = 131 * Sol31lvl^2

	Sol31lvl = 0.9 * Sol31lvl - 0.01			' Fading equation. 0.9 is rather slow. 0.6 is faster. Example 0.99 is too slow, but good for demo
	if Sol31lvl < 0 then Sol31lvl = 0			' failsafe so we are not going under 0

	if Sol31lvl =< 0 Then
		f31_render.visible = false
		f31_wall.visible = false
		f31_render.TimerEnabled = false				' disabling this timer
	end if
end sub

sub Flash32(enabled)
	If enabled Then
		Sol32lvl = 1
		f32_render_timer
		Lampz.state(132) = 1
	else
		Sol32lvl = Sol32lvl * 0.7 'minor tweak to force faster fade
		Lampz.state(132) = 0
		bgfl32.visible = 0
	End If
	Sound_Flash_Relay enabled, Relay_9
end sub


sub f32_render_timer
	if Not f32_render.TimerEnabled then				' This is here to detect the first call to this timer
		f32_render.visible = true
		f32_render.TimerEnabled = true				' enabling this timer
	end if

	'debug.print Sol32lvl

	f32_render.opacity = renderopacity * Sol32lvl^2

	Sol32lvl = 0.9 * Sol32lvl - 0.01			' Fading equation. 0.9 is rather slow. 0.6 is faster. Example 0.99 is too slow, but good for demo
	if Sol32lvl < 0 then Sol32lvl = 0			' failsafe so we are not going under 0

	if Sol32lvl =< 0 Then
		f32_render.visible = false
		f32_render.TimerEnabled = false				' disabling this timer
	end if
end sub

sub Flash46(Enabled)
	If Enabled Then
		Lampz.state(138) = 1
	Else
		Lampz.state(138) = 0
		bgfl46.visible = 0
	End If
End Sub

sub Flash47(Enabled)
	If Enabled Then
		Lampz.state(139) = 1
	Else
		Lampz.state(139) = 0
		bgfl47.visible = 0
	End If
End Sub


dim GiIsOff

'Playfield GI
Sub PFGI(Enabled)
	If Enabled Then  ' GI state inverted, enabled = OFF
		dim xx
'		For each xx in UV1col:xx.image="UV1off": Next
'		sw37.image="spin1off"
		SetLamp 111, 0
        GIIsOff=true
	Else
'		For each xx in UV1col:xx.image="UV1": Next
'		sw37.image="spin1"
		SetLamp 111, 5
        GIIsOff=false
	End If
	' Update lamps that may swap textures when GI changes here
'	SetLamp 110, Not Enabled
'	SetLamp 111, Enabled
'	UpdateLamp25
'	UpdateLamp26
'	UpdateLamp27
	Sound_GI_Relay enabled, Relay_11
End Sub

'******************************************************
'****  LAMPZ
'******************************************************

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
Dim ModLampz : Set ModLampz = New DynamicLamps
InitLampsNF              ' Setup lamp assignments

Sub LampTimer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
	ModLampz.Update1
'	Lampz.Update	'update (fading logic only)
'	ModLampz.Update
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
Sub Lampztimer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
	ModLampz.Update

End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'Material swap arrays.
'Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image trans","Plastic with an image trans","Plastic with an image")
Dim DLintensity

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

sub DisableLightingMinMax(pri, DLintensityMin, DLintensityMax, ByVal aLvl)    'cp's script  DLintensity = disabled lighting intesity
    if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)    'Callbacks don't get this filter automatically
    pri.blenddisablelighting = (aLvl * (DLintensityMax-DLintensityMin)) + DLintensityMin
End Sub

Sub InitLampsNF()
	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating
	ModLampz.Filter = "LampFilter"

	'Adjust fading speeds (1 / full MS fading time)
	dim x
	for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/3 : Lampz.FadeSpeedDown(x) = 1/9 : next'1/20 : next
	for x = 0 to 28 : ModLampz.FadeSpeedUp(x) = 1/2 : ModLampz.FadeSpeedDown(x) = 1/30 : Next

	'for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/80 : Lampz.FadeSpeedDown(x) = 1/100 : next
	Lampz.FadeSpeedUp(111) = 1/6 'GI
	Lampz.FadeSpeedDown(111) = 1/18

	'Lamp Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

	Lampz.MassAssign(1)= l1
	Lampz.MassAssign(1)= l1B
	Lampz.Callback(1) = "DisableLighting p1, 12,"
	Lampz.Callback(1) = "DisableLighting p1o, 22,"
	Lampz.MassAssign(2)= l2
	Lampz.MassAssign(2)= l2B
	Lampz.Callback(2) = "DisableLighting p2, 12,"
	Lampz.Callback(2) = "DisableLighting p2o, 22,"
	Lampz.MassAssign(3)= l3
	Lampz.MassAssign(3)= l3B
	Lampz.Callback(3) = "DisableLighting p3, 12,"
	Lampz.Callback(3) = "DisableLighting p3o, 22,"
	Lampz.MassAssign(4)= l4
	Lampz.MassAssign(4)= l4B
	Lampz.Callback(4) = "DisableLighting p4, 12,"
	Lampz.Callback(4) = "DisableLighting p4o, 22,"
	Lampz.MassAssign(5)= l5
	Lampz.MassAssign(5)= l5B
	Lampz.Callback(5) = "DisableLighting p5, 12,"
	Lampz.Callback(5) = "DisableLighting p5o, 22,"
	Lampz.MassAssign(6)= l6
	Lampz.MassAssign(6)= l6B
	Lampz.Callback(6) = "DisableLighting p6, 12,"
	Lampz.Callback(6) = "DisableLighting p6o, 22,"
	Lampz.MassAssign(7)= l7
	Lampz.MassAssign(7)= l7B
	Lampz.Callback(7) = "DisableLighting p7, 12,"
	Lampz.Callback(7) = "DisableLighting p7o, 22,"
	Lampz.MassAssign(9)= l9
	Lampz.MassAssign(9)= l9B
	Lampz.Callback(9) = "DisableLighting p9, 12,"
	Lampz.Callback(9) = "DisableLighting p9o, 22,"
	Lampz.MassAssign(10)= l10
	Lampz.MassAssign(10)= l10B
	Lampz.Callback(10) = "DisableLighting p10, 12,"
	Lampz.Callback(10) = "DisableLighting p10o, 22,"
	Lampz.MassAssign(11)= l11
	Lampz.MassAssign(11)= l11B
	Lampz.Callback(11) = "DisableLighting p11, 12,"
	Lampz.Callback(11) = "DisableLighting p11o, 22,"
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign(12)= l12B
	Lampz.Callback(12) = "DisableLighting p12, 12,"
	Lampz.Callback(12) = "DisableLighting p12o, 22,"
	Lampz.MassAssign(13)= l13
	Lampz.MassAssign(13)= l13B
	Lampz.Callback(13) = "DisableLighting p13, 12,"
	Lampz.Callback(13) = "DisableLighting p13o, 22,"
	Lampz.MassAssign(14)= l14
	Lampz.MassAssign(14)= l14B
	Lampz.Callback(14) = "DisableLighting p14, 12,"
	Lampz.Callback(14) = "DisableLighting p14o, 22,"
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(15)= l15B
	Lampz.Callback(15) = "DisableLighting p15, 12,"
	Lampz.Callback(15) = "DisableLighting p15o, 22,"
	Lampz.MassAssign(16)= l16
	Lampz.MassAssign(16)= l16B
	Lampz.Callback(16) = "DisableLighting p16, 12,"
	Lampz.Callback(16) = "DisableLighting p16o, 12,"
	Lampz.Callback(16) = "DisableLightingMinMax speciallamp, 1, 40,"
	Lampz.Callback(17) = "DisableLighting droplamp1, 6,"
	Lampz.Callback(18) = "DisableLighting droplamp2, 6,"
	Lampz.Callback(19) = "DisableLighting droplamp3, 6,"
	Lampz.Callback(20) = "DisableLighting droplamp4, 6,"
	Lampz.Callback(21) = "DisableLighting droplamp5, 6,"
'	Lampz.MassAssign(17)= l17
'	Lampz.MassAssign(17)= l17B
'	Lampz.Callback(17) = "DisableLighting p17, 12,"
'	Lampz.Callback(17) = "DisableLighting p17o, 22,"
'	Lampz.MassAssign(18)= l18
'	Lampz.MassAssign(18)= l18B
'	Lampz.Callback(18) = "DisableLighting p18, 12,"
'	Lampz.Callback(18) = "DisableLighting p18o, 22,"
'	Lampz.MassAssign(19)= l19
'	Lampz.MassAssign(19)= l19B
'	Lampz.Callback(19) = "DisableLighting p19, 12,"
'	Lampz.Callback(19) = "DisableLighting p19o, 22,"
'	Lampz.MassAssign(20)= l20
'	Lampz.MassAssign(20)= l20B
'	Lampz.Callback(20) = "DisableLighting p20, 12,"
'	Lampz.Callback(20) = "DisableLighting p20o, 22,"
'	Lampz.MassAssign(21)= l21
'	Lampz.MassAssign(21)= l21B
'	Lampz.Callback(21) = "DisableLighting p21, 12,"
'	Lampz.Callback(21) = "DisableLighting p21o, 22,"
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(22)= l22B
	Lampz.Callback(22) = "DisableLighting p22, 12,"
	Lampz.Callback(22) = "DisableLighting p22o, 22,"
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign(23)= l23B
	Lampz.Callback(23) = "DisableLighting p23, 12,"
	Lampz.Callback(23) = "DisableLighting p23o, 22,"
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(24)= l24B
	Lampz.Callback(24) = "DisableLighting p24, 12,"
	Lampz.Callback(24) = "DisableLighting p24o, 12,"
'	Lampz.MassAssign(25)= l25
'	Lampz.MassAssign(25)= l25B
'	Lampz.Callback(25) = "DisableLightingMinMax p25, 1, 1.8,"
'	Lampz.Callback(27) = "DisableLightingMinMax p25off, 1, 2,"
'	Lampz.Callback(25) = "DisableLightingMinMax bumperskirt001, 1, 1.01,"
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= l26B
	Lampz.Callback(26) = "DisableLighting p26, 12,"
	Lampz.Callback(26) = "DisableLighting p26o, 12,"
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign(27)= l27B
	Lampz.Callback(27) = "DisableLighting p27, 12,"
	Lampz.Callback(27) = "DisableLighting p27o, 12,"
	Lampz.MassAssign(28)= l28
	Lampz.MassAssign(28)= l28B
	Lampz.Callback(28) = "DisableLighting p28, 12,"
	Lampz.Callback(28) = "DisableLighting p28o, 12,"
	Lampz.MassAssign(29)= l29
	Lampz.MassAssign(29)= l29B
	Lampz.Callback(29) = "DisableLighting p29, 12,"
	Lampz.Callback(29) = "DisableLighting p29o, 22,"
	Lampz.MassAssign(30)= l30
	Lampz.MassAssign(30)= l30B
	Lampz.Callback(30) = "DisableLighting p30, 12,"
	Lampz.Callback(30) = "DisableLighting p30o, 22,"
	Lampz.MassAssign(31)= l31
	Lampz.MassAssign(31)= l31B
	Lampz.Callback(31) = "DisableLighting p31, 12,"
	Lampz.Callback(31) = "DisableLighting p31o, 22,"
	Lampz.MassAssign(32)= l32
	Lampz.MassAssign(32)= l32B
	Lampz.Callback(32) = "DisableLighting p32, 12,"
	Lampz.Callback(32) = "DisableLighting p32o, 22,"
	Lampz.MassAssign(33)= l33
	Lampz.MassAssign(33)= l33B
	Lampz.Callback(33) = "DisableLighting p33, 12,"
	Lampz.Callback(33) = "DisableLighting p33o, 12,"
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(34)= l34B
	Lampz.Callback(34) = "DisableLighting p34, 12,"
	Lampz.Callback(34) = "DisableLighting p34o, 12,"
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(35)= l35B
	Lampz.Callback(35) = "DisableLighting p35, 12,"
	Lampz.Callback(35) = "DisableLighting p35o, 12,"
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(36)= l36B
	Lampz.Callback(36) = "DisableLighting p36, 12,"
	Lampz.Callback(36) = "DisableLighting p36o, 12,"
'	Lampz.MassAssign(37)= l37
'	Lampz.MassAssign(37)= l37B
'	Lampz.Callback(37) = "DisableLighting p37, 12,"
'	Lampz.Callback(37) = "DisableLighting p37o, 22,"
	Lampz.MassAssign(38)= l38
	Lampz.MassAssign(38)= l38B
	Lampz.Callback(38) = "DisableLighting p38, 12,"
	Lampz.Callback(38) = "DisableLighting p38o, 12,"
	Lampz.MassAssign(39)= l39
	Lampz.MassAssign(39)= l39B
	Lampz.Callback(39) = "DisableLighting p39, 12,"
	Lampz.Callback(39) = "DisableLighting p39o, 12,"
	Lampz.MassAssign(40)= l40
	Lampz.MassAssign(40)= l40B
	Lampz.Callback(40) = "DisableLighting p40, 12,"
	Lampz.Callback(40) = "DisableLighting p40o, 22,"
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign(41)= l41B
	Lampz.Callback(41) = "DisableLighting p41, 12,"
	Lampz.Callback(41) = "DisableLighting p41o, 22,"
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(42)= l42B
	Lampz.Callback(42) = "DisableLighting p42, 12,"
	Lampz.Callback(42) = "DisableLighting p42o, 22,"
	Lampz.MassAssign(43)= l43
	Lampz.MassAssign(43)= l43B
	Lampz.Callback(43) = "DisableLighting p43, 12,"
	Lampz.Callback(43) = "DisableLighting p43o, 22,"	
	Lampz.MassAssign(44)= l44
	Lampz.MassAssign(44)= l44B
	Lampz.Callback(44) = "DisableLighting p44, 12,"
	Lampz.Callback(44) = "DisableLighting p44o, 22,"	
	Lampz.MassAssign(45)= l45
	Lampz.MassAssign(45)= l45B
	Lampz.Callback(45) = "DisableLighting p45, 12,"
	Lampz.Callback(45) = "DisableLighting p45o, 22,"
	Lampz.Callback(46) = "DisableLightingMinMax ramplamp001, 1, 75,"
	Lampz.Callback(47) = "DisableLightingMinMax ramplamp002, 1, 75,"
	Lampz.Callback(48) = "DisableLightingMinMax ramplamp003, 1, 75,"
'	Lampz.MassAssign(46)= l46
'	Lampz.MassAssign(46)= l46B
'	Lampz.MassAssign(47)= l47
'	Lampz.MassAssign(47)= l47B
'	Lampz.Callback(47) = "DisableLighting p47, 12,"
'	Lampz.Callback(47) = "DisableLighting p47o, 22,"
'	Lampz.MassAssign(48)= l48
'	Lampz.MassAssign(48)= l48B
'	Lampz.Callback(48) = "DisableLighting p48, 12,"
'	Lampz.Callback(48) = "DisableLighting p48o, 22,"
	Lampz.MassAssign(49)= l49
	Lampz.MassAssign(49)= l49B
	Lampz.Callback(49) = "DisableLighting p49, 12,"
	Lampz.Callback(49) = "DisableLighting p49o, 12,"
	Lampz.MassAssign(50)= l50
	Lampz.MassAssign(50)= l50B
	Lampz.Callback(50) = "DisableLighting p50, 12,"
	Lampz.Callback(50) = "DisableLighting p50o, 12,"
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= l51B
	Lampz.Callback(51) = "DisableLighting p51, 12,"
	Lampz.Callback(51) = "DisableLighting p51o, 22,"
	Lampz.MassAssign(52)= l52
	Lampz.MassAssign(52)= l52B
	Lampz.Callback(52) = "DisableLighting p52, 12,"
	Lampz.Callback(52) = "DisableLighting p52o, 22,"
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= l53B
	Lampz.Callback(53) = "DisableLighting p53, 12,"
	Lampz.Callback(53) = "DisableLighting p53o, 22,"
	Lampz.MassAssign(54)= l54
	Lampz.MassAssign(54)= l54B
	Lampz.Callback(54) = "DisableLighting p54, 12,"
	Lampz.Callback(54) = "DisableLighting p54o, 22,"
	Lampz.MassAssign(55)= l55
	Lampz.MassAssign(55)= l55B
	Lampz.Callback(55) = "DisableLighting p55, 12,"
	Lampz.Callback(55) = "DisableLighting p55o, 22,"
	Lampz.MassAssign(56)= l56
	Lampz.MassAssign(56)= l56B
	Lampz.Callback(56) = "DisableLighting p56, 12,"
	Lampz.Callback(56) = "DisableLighting p56o, 22,"
	Lampz.MassAssign(109)= FL_9
'	Lampz.MassAssign(125)= FL_25
	Lampz.Callback(125) = "DisableLighting p125, 42,"
	Lampz.Callback(125) = "DisableLighting p125o, 52,"	
	Lampz.MassAssign(125)= FL_25
	Lampz.MassAssign(127)= FL_27
	Lampz.MassAssign(128)= FL_28
	Lampz.MassAssign(129)= FL_29
	Lampz.MassAssign(130)= FL_30
	Lampz.MassAssign(131)= FL_31
	Lampz.MassAssign(132)= FL_32

'	'Sol 10 GI relay and assignments
'	Lampz.obj(111) = ColtoArray(GI)	
'
	For each x in GI:Lampz.MassAssign(111) = x:Next	
	Lampz.Callback(111) = "GIUpdates"

'	Lampz.state(111) = 1		'Turn on GI to Start


  if VR_Room = 1 Then
	Lampz.MassAssign(57) = VR_05Mil
	Lampz.MassAssign(58) = VR_1Mil
	Lampz.MassAssign(59) = VR_15Mil
	Lampz.MassAssign(60) = VR_2Mil
	Lampz.MassAssign(61) = VR_25Mil
	Lampz.MassAssign(62) = VR_3Mil
	Lampz.MassAssign(63) = VR_35Mil
	Lampz.MassAssign(64) = VR_4Mil
	Lampz.MassAssign(41) = VR_20K
	Lampz.MassAssign(42) = VR_30K
	Lampz.MassAssign(51) = VR_40K
	Lampz.MassAssign(52) = VR_50K
	Lampz.MassAssign(6) = VR_100K
	Lampz.MassAssign(7) = VR_500K
	Lampz.MassAssign(125) = BGFL25
	Lampz.Callback(125) = "BGLamps"
	Lampz.MassAssign(130) = BGFL30
	Lampz.Callback(130) = "BGLamps"
	Lampz.MassAssign(132) = BGFL32
	Lampz.Callback(132) = "BGLamps"
	Lampz.MassAssign(138) = BGFL46
	Lampz.Callback(138) = "BGLamps"
	Lampz.MassAssign(139) = BGFL47
	Lampz.Callback(139) = "BGLamps"
End If


	'Turn off all lamps on startup
	lampz.Init	'This just turns state of any lamps to 1
	ModLampz.Init

	'Immediate update to turn on GI, turn off lamps
	lampz.update
	ModLampz.Update

End Sub

Sub BGLamps(ByVal aLvl)

	if alvl=0 Then
		BGFL25.visible = 0
		BGFL30.visible = 0
		BGFL32.visible = 0
		BGFL46.visible = 0
		BGFL47.visible = 0
	Elseif aLvl=1 Then
		BGFL25.visible = 1
		BGFL25.opacity = 350
		BGFL30.visible = 1
		BGFL30.opacity = 350
		BGFL32.visible = 1
		BGFL32.opacity = 350
		BGFL46.visible = 1
		BGFL46.opacity = 350
		BGFL47.visible = 1
		BGFL47.opacity = 350
	Else
		BGFL25.visible = 1
		BGFL25.opacity = 350*aLvl
		BGFL30.visible = 1
		BGFL30.opacity = 350*aLvl
		BGFL32.visible = 1
		BGFL32.opacity = 350*aLvl
		BGFL46.visible = 1
		BGFL46.opacity = 350*aLvl
		BGFL47.visible = 1
		BGFL47.opacity = 350*aLvl
	End If

end Sub


'***************************************
'System 11 GI On/Off
'***************************************
Sub GIOn  : SetGI False: End Sub 'These are just debug commands now
Sub GIOff : SetGI True : End Sub


Dim GIoffMult : GIoffMult = 2 'adjust how bright the inserts get when the GI is off
Dim GIoffMultFlashers : GIoffMultFlashers = 2	'adjust how bright the Flashers get when the GI is off


'Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image trans","Plastic with an image trans","Plastic with an image")
dim gilvl
'const ballbrightMax = 105
'const ballbrightMin = 15

Dim GIX
Sub GIupdates(ByVal aLvl)	'GI update odds and ends go here
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically

	'debug.print aLvl

	UpdateMaterial "meshtop",0,0,0,0,0,0,aLvl^3,RGB(247,247,247),0,0,False,True,0,0,0,0
	UpdateMaterial "meshtop1",0,0,0,0,0,0,aLvl,RGB(247,247,247),0,0,False,True,0,0,0,0
	UpdateMaterial "meshtop2",0,0,0,0,0,0,aLvl,RGB(247,247,247),0,0,False,True,0,0,0,0
	UpdateMaterial "meshtop3",0,0,0,0,0,0,aLvl^3,RGB(247,247,247),0,0,False,True,0,0,0,0
	UpdateMaterial "pfoff",0,0,0,0,0,0,1-aLvl,RGB(247,247,247),0,0,False,True,0,0,0,0

'	If aLvl = 0 Then numberofsources = 0 else numberofsources = numberofsources_hold 'Dynamic Ball Shadows

'	if ObjLevel(1) <= 0 And ObjLevel(2) <= 0 Then
'		'commenting this out for now, as it has issues with flashers
'		if aLvl = 0 then										'GI OFF, let's hide ON prims
'			OnPrimsVisible False
'			for each GIX in GI:GIX.state = 0:Next
'			if ballbrightness <> -1 then ballbrightness = ballbrightMin
'		Elseif aLvl = 1 then									'GI ON, let's hide OFF prims
'			OffPrimsVisible False
'			for each GIX in GI:GIX.state = 1:Next
'			if ballbrightness <> -1 then ballbrightness = ballbrightMax
'		Else
'			if giprevalvl = 0 Then								'GI has just changed from OFF to fading, let's show ON
'				'fx_relay_on
'				OnPrimsVisible True
'				ballbrightness = ballbrightMin + 1
'			elseif giprevalvl = 1 Then							'GI has just changed from ON to fading, let's show OFF
'				'fx_relay_off
'				OffPrimsVisible true
'				ballbrightness = ballbrightMax - 1
'			Else
'				'no change
'			end if
'		end if
'
'		UpdateMaterial "GI_ON_CAB",		0,0,0,0,0,0,aLvl^2,RGB(255,255,255),0,0,False,True,0,0,0,0
'		UpdateMaterial "GI_ON_Plastic",	0,0,0,0,0,0,aLvl^3,RGB(255,255,255),0,0,False,True,0,0,0,0
'		UpdateMaterial "GI_ON_Metals",	0,0,0,0,0,0,aLvl^1,RGB(255,255,255),0,0,False,True,0,0,0,0
'		UpdateMaterial "GI_ON_Bulbs",	0,0,0,0,0,0,aLvl^1,RGB(255,255,255),0,0,False,True,0,0,0,0
'	Elseif ObjLevel(1) > 0 Or ObjLevel(2) > 0 then 
'		if aLvl = 0 Or aLvl = 1 then
'			'nothing, flashers just fading and no real change to gi
'		Elseif giprevalvl = 0 then 'gi went ON while some flasher was fading
'			'debug.print "##on prims to on image"
'			OnPrimSwap "ON"
'		elseif giprevalvl = 1 Then 'gi went OFF while some flasher was fading
'			'debug.print "##on prims to OFF images"
'			OnPrimSwap "OFF"
'		end if
'		
'	end If

'Sideblades: ^5 (fastest to go off)
'Plastics: ^3 (medium speed)
'Bulbs: ^0.5 (not sure how this would look. Would be the slowest)
'metals:^2
'
'GI_ON_Bulbs
'GI_ON_CAB
'GI_ON_Metals
'GI_ON_Plastic

	'debug.print aLvl
	'debug.print aLvl^5
'
'	PLAYFIELD_GI1.opacity = PFGIOFFOpacity - (PFGIOFFOpacity * alvl^3) 'TODO 60
'
'	'debug.print "*** --> " & FlashLevelToIndex(aLvl, 3)
'
'    Select case FlashLevelToIndex(aLvl, 3)
'		Case 0:plastics.Image = "plastics_000"
'		Case 1:plastics.Image = "plastics_033"
'		Case 2:plastics.Image = "plastics_066"
'        Case 3:plastics.Image = "plastics_100"
'    End Select
'	
'	'0.7 - 0.05
'	FlasherOffBrightness = 0.7*aLvl
'	Flasherbase2.blenddisablelighting = FlasherOffBrightness
'	Flasherbase1.blenddisablelighting = FlasherOffBrightness
'
'	lamp_bulbs.blenddisablelighting = 10 * aLvl : lamp_bulbsOFF.blenddisablelighting = 10 * aLvl
'	bulbs.blenddisablelighting = 0.5 * aLvl : bulbsOFF.blenddisablelighting = 0.5 * aLvl
'
'	'ball
'	if ballbrightness <> ballbrightMax Or ballbrightness <> ballbrightMin Or ballbrightness <> -1 then ballbrightness = INT(alvl * (ballbrightMax - ballbrightMin) + ballbrightMin)
'	

	gilvl = alvl

End Sub

'Lamp Filter
Function LampFilter(aLvl)

	LampFilter = aLvl^1.6	'exponential curve?
End Function



'Helper functions

Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function

'Set GICallback2 = GetRef("SetGI")

'Sub SetGI(aNr, aValue)
'	msgbox "GI nro: " & aNr & " and step: " & aValue
'	ModLampz.SetGI aNr, aValue 'Redundant. Could reassign GI indexes here
'End Sub


'Dim GiOffFOP
'Sub SetGI(aOn)
''	PlayRelay aOn, 13
'	Select Case aOn
'		Case True  'GI off
'			'fx_relay_off
'			PlaySoundAtLevelStatic ("fx_relay_off"), RelaySoundLevel, p30off
'			SetLamp 111, 0	'Inverted, Solenoid cuts GI circuit on this era of game
'			l57.intensity=66:l58.intensity=66:l59.intensity=66
'			l57.falloff=250:l58.falloff=250:l59.falloff=250
'		Case False 
'			'fx_relay_on
'			PlaySoundAtLevelStatic ("fx_relay_on"), RelaySoundLevel, p30off
'			SetLamp 111, 5
'			l57.intensity=11:l58.intensity=11:l59.intensity=11
'			l57.falloff=200:l58.falloff=200:l59.falloff=200
'	End Select
'End Sub

Sub SetLamp(aNr, aOn)
'	if aNr = 111 then
'		msgbox gametime & " GI: " & aOn
'	end if
	Lampz.state(aNr) = abs(aOn)
End Sub

Sub SetModLamp(aNr, aInput)
	ModLampz.state(aNr) = abs(aInput)/255
End Sub

'****************************************************************
'				Class jungle nf (what does this mean?!?)
'****************************************************************

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

Class LampFader
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Public UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)
	Private Mult(140)
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
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		'debug.print Lampz.Locked(100)	'debug
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

		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
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
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 0.2 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
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
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class




'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be publicly accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
'Version 0.13a - fixed DynamicLamps hopefully
' Note: if using multiple 'DynamicLamps' objects, change the 'name' variable to avoid conflicts with callbacks

Class DynamicLamps 'Lamps that fade up and down. GI and Flasher handling
	Public Loaded(50), FadeSpeedDown(50), FadeSpeedUp(50)
	Private Lock(50), SolModValue(50)
	Private UseCallback(50), cCallback(50)
	Public Lvl(50)
	Public Obj(50)
	Private UseFunction, cFilter
	private Mult(50)
	Public Name

	Public FrameTime
	Private InitFrame

	Private Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(Obj)
			FadeSpeedup(x) = 0.01
			FadeSpeedDown(x) = 0.01
			lvl(x) = 0.0001 : SolModValue(x) = 0
			Lock(x) = True : Loaded(x) = False
			mult(x) = 1
			Name = "DynamicFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function

	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'debugstr = debugstr & x & "=" & tmp(x) & vbnewline
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next

		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property


	Public Property Let State(idx,Value)
		'If Value = SolModValue(idx) Then Exit Property ' Discard redundant updates
		If Value <> SolModValue(idx) Then ' Discard redundant updates
			SolModValue(idx) = Value
			Lock(idx) = False : Loaded(idx) = False
		End If
	End Property
	Public Property Get state(idx) : state = SolModValue(idx) : end Property

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

	'solcallback (solmodcallback) handler
	Sub SetLamp(aIdx, aInput) : state(aIdx) = aInput : End Sub	'0->1 Input
	Sub SetModLamp(aIdx, aInput) : state(aIdx) = aInput/255 : End Sub	'0->255 Input
	Sub SetGI(aIdx, ByVal aInput) : if aInput = 8 then aInput = 7 end if : state(aIdx) = aInput/7 : End Sub	'0->8 WPC GI input

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) ': debugstr = debugstr & tmp(x).name & " state'd" & vbnewline

				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) ': debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline

			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'just call turnonstates for now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all numeric fading. If done fading, Lock(x) = True
		'dim stringer
		dim x : for x = 0 to uBound(Lvl)
			'stringer = "Locked @ " & SolModValue(x)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					'stringer = "Fading Up " & lvl(x) & " + " & FadeSpeedUp(x)
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					'stringer = "Fading Down " & lvl(x) & " - " & FadeSpeedDown(x)
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				End If
			end if
		Next
		'tbF.text = stringer
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(Lvl)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				End If
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx
		for x = 0 to uBound(Lvl)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(abs(Lvl(x))*mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(abs(Lvl(x))*mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)*mult(x)
					End If
				end if
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x)*mult(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					Loaded(x) = True
				end if
			end if
		Next
	End Sub
End Class

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
	Dim b', BOT
'	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
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
				BallShadowA(b).height=gBOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = gBOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = gBOT(b).X
			BallShadowA(b).visible = 1
		End If
	Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************




'******************************************************
'**** RAMP ROLLING SFX
'******************************************************

dim RampMinLoops : RampMinLoops = 4

dim RampBalls(5,2)

RampBalls(0,0) = False

dim RampType(5)	

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



'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************


'*******************************************
'  Ramp Triggers
'*******************************************

Sub RampOff1_hit
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub RampOff2_hit
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub RampOff2_Unhit
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound
End Sub

Sub RampOff3_hit
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub RampOff3_Unhit
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound
End Sub

Sub RampOn1_Hit
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub RampOn2_Hit
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub RampOff4_hit
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub RampOff5_hit
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub RampOff6_hit
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub RampOff7_hit
	WireRampOn True 'Play Plastic Ramp Sound
End Sub





'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


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
'    dim rx, ry
'    rx = x*dCos(angle) - y*dSin(angle)
'    ry = x*dSin(angle) + y*dCos(angle)
'    RotPoint = Array(rx,ry)
'End Function

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




'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************


'*************************************************************************************************************************************************
' Desktop Digits.... *****************************************************************************************************************************

Dim Digits(28)
Digits(0)=Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
Digits(1)=Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
Digits(2)=Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
Digits(3)=Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
Digits(4)=Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
Digits(5)=Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
Digits(6)=Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
 
Digits(7)=Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
Digits(8)=Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
Digits(9)=Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
Digits(10)=Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
Digits(11)=Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
Digits(12)=Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
Digits(13)=Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
 
Digits(14) = Array(b00, b05, b0c, b0d, b08, b01, b06, b0f)
Digits(15) = Array(b10, b15, b1c, b1d, b18, b11, b16, b1f)
Digits(16) = Array(b20, b25, b2c, b2d, b28, b21, b26, b2f)
Digits(17) = Array(b30, b35, b3c, b3d, b38, b31, b36, b3f)
Digits(18) = Array(b40, b45, b4c, b4d, b48, b41, b46, b4f)
Digits(19) = Array(b50, b55, b5c, b5d, b58, b51, b56, b5f)
Digits(20) = Array(b60, b65, b6c, b6d, b68, b61, b66, b6f)
Digits(21) = Array(b70, b75, b7c, b7d, b78, b71, b76, b7f)
Digits(22) = Array(b80, b85, b8c, b8d, b88, b81, b86, b8f)
Digits(23) = Array(b90, b95, b9c, b9d, b98, b91, b96, b9f)
Digits(24) = Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf)
Digits(25) = Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf)
Digits(26) = Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf)
Digits(27) = Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf)


 Sub DisplayTimer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
		For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
              For Each obj In Digits(num)
                   If chg And 1 Then obj.State=stat And 1
                   chg=chg\2 : stat=stat\2
              Next
		Next
    End If
 End Sub



'******************************
' Setup Backglass
'******************************

Dim xoff,yoff_up, yoff_down,zoff,xrot,zscale, xcen,ycen, ix, xx, yy, xobj

Sub setup_backglass()

	xoff = -20
	yoff_up = 98
	yoff_down = 100
	zoff = 699
	xrot = -90
	zscale = 0.0000001

	xcen = 0  '(130 /2) - (92 / 2)
	ycen = (780 /2 ) + (203 /2)

	for ix = 0 to 13
		For Each xobj In VRDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff_up 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 14 to 27
		For Each xobj In VRDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff_down

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

end sub


'**********************************************
'*******	Set Up Backglass Flashers	*******
'**********************************************
' this is for lining up the backglass flashers on top of a backglass image

Sub SetBackglass()
	Dim obj

	For Each obj In BackglassLow
		obj.x = obj.x
		obj.height = - obj.y - 8
		obj.y = 76 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In BackglassMid
		obj.x = obj.x
		obj.height = - obj.y - 8
		obj.y = 78 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In BackglassHigh
		obj.x = obj.x
		obj.height = - obj.y
		obj.y = 80 'adjusts the distance from the backglass towards the user
	Next

End Sub



'**************************************************************************************************************************************************
'VRDigits........  ********************************************************************************************************************************


Dim VRDigits(28)

VRDigits(0)=Array(ax00, ax05, ax0c, ax0d, ax08, ax01, ax06, ax0f, ax02, ax03, ax04, ax07, ax0b, ax0a, ax09, ax0e)
VRDigits(1)=Array(ax10, ax15, ax1c, ax1d, ax18, ax11, ax16, ax1f, ax12, ax13, ax14, ax17, ax1b, ax1a, ax19, ax1e)
VRDigits(2)=Array(ax20, ax25, ax2c, ax2d, ax28, ax21, ax26, ax2f, ax22, ax23, ax24, ax27, ax2b, ax2a, ax29, ax2e)
VRDigits(3)=Array(ax30, ax35, ax3c, ax3d, ax38, ax31, ax36, ax3f, ax32, ax33, ax34, ax37, ax3b, ax3a, ax39, ax3e)
VRDigits(4)=Array(ax40, ax45, ax4c, ax4d, ax48, ax41, ax46, ax4f, ax42, ax43, ax44, ax47, ax4b, ax4a, ax49, ax4e)
VRDigits(5)=Array(ax50, ax55, ax5c, ax5d, ax58, ax51, ax56, ax5f, ax52, ax53, ax54, ax57, ax5b, ax5a, ax59, ax5e)
VRDigits(6)=Array(ax60, ax65, ax6c, ax6d, ax68, ax61, ax66, ax6f, ax62, ax63, ax64, ax67, ax6b, ax6a, ax69, ax6e)

VRDigits(7)=Array(ax70, ax75, ax7c, ax7d, ax78, ax71, ax76, ax7f, ax72, ax73, ax74, ax77, ax7b, ax7a, ax79, ax7e)
VRDigits(8)=Array(ax80, ax85, ax8c, ax8d, ax88, ax81, ax86, ax8f, ax82, ax83, ax84, ax87, ax8b, ax8a, ax89, ax8e)
VRDigits(9)=Array(ax90, ax95, ax9c, ax9d, ax98, ax91, ax96, ax9f, ax92, ax93, ax94, ax97, ax9b, ax9a, ax99, ax9e)
VRDigits(10)=Array(axa0, axa5, axac, axad, axa8, axa1, axa6, axaf, axa2, axa3, axa4, axa7, axab, axaa, axa9, axae)
VRDigits(11)=Array(axb0, axb5, axbc, axbd, axb8, axb1, axb6, axbf, axb2, axb3, axb4, axb7, axbb, axba, axb9, axbe)
VRDigits(12)=Array(axc0, axc5, axcc, axcd, axc8, axc1, axc6, axcf, axc2, axc3, axc4, axc7, axcb, axca, axc9, axce)
VRDigits(13)=Array(axd0, axd5, axdc, axdd, axd8, axd1, axd6, axdf, axd2, axd3, axd4, axd7, axdb, axda, axd9, axde)
 ' 3rd Player
VRDigits(14) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6,LED1x7)
VRDigits(15) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6,LED2x7)
VRDigits(16) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6,LED3x7)
VRDigits(17) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6,LED4x7)
VRDigits(18) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6,LED5x7)
VRDigits(19) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6,LED6x7)
VRDigits(20) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6,LED7x7)
' 4th Player
VRDigits(21) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6,LED8x7)
VRDigits(22) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6,LED9x7)
VRDigits(23) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6,LED10x7)
VRDigits(24) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6,LED11x7)
VRDigits(25) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6,LED12x7)
VRDigits(26) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6,LED13x7)
VRDigits(27) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6,LED14x7)

dim DisplayColor, DisplayColorG
DisplayColor =  RGB(255,40,1)

InitDigits

Sub VRDisplayTimer
	Dim ii, jj, obj, b, x
	Dim ChgLED,num, chg, stat
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED) Then
			For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				For Each obj In VRDigits(num)
 '                  If chg And 1 Then obj.visible=stat And 1    'if you use the object color for off; turn the display object visible to not visible on the playfield, and uncomment this line out.
					If chg And 1 Then FadeDisplay obj, stat And 1	
					chg=chg\2 : stat=stat\2
				Next
			Next
		End If
End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
		Object.Opacity = 12
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 6
	End If
End Sub

Sub InitDigits()
	dim tmp, x, obj
	for x = 0 to uBound(VRDigits)
		if IsArray(VRDigits(x) ) then
			For each obj in VRDigits(x)
				obj.height = obj.height + 18
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

' END VR Digits code ***********************************************************************************************************
'*******************************************************************************************************************************



'*************************************************************************************** 
' Hybrid code for VR, Cab, and Desktop
'***************************************************************************************

'******************************************************
' 	      			VR Plunger Code
'******************************************************

Sub TimerVRPlunger_Timer
  If PinCab_Shooter.Y < -260 then
       PinCab_Shooter.Y = PinCab_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger1_Timer
  PinCab_Shooter.Y = -351 + (5* Plunger.Position) -20
End Sub

Dim VRThings

if VR_Room = 0 and cabmode = 0 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in VRBackglass:VRThings.visible = 0:Next
	for each VRThings in DTRails:VRThings.visible = 1:Next
	for each VRThings in DTReels:VRThings.visible = 1:Next
Elseif VR_Room = 0 and cabmode = 1 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in VRBackglass:VRThings.visible = 0:Next
	for each VRThings in DTBackglass:VRThings.visible = 0: Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
	for each VRThings in DTReels:VRThings.visible = 0:Next
Else
	for each VRThings in VRStuff:VRThings.visible = 1:Next
	for each VRThings in VRClock:VRThings.visible = WallClock:Next
	for each VRThings in DTBackglass:VRThings.visible = 0: Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
	for each VRThings in DTReels:VRThings.visible = 0:Next
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

End If


'*************************************************************************************** 
' CODE BELOW IS FOR THE VR CLOCK.  This was originally taken from Rascal VP9 clock table 
'***************************************************************************************

Dim CurrentMinute ' for VR clock

Sub ClockTimer_Timer()

    'ClockHands Below *********************************************************
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
	Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())

End Sub



'**************************************
' Backglass EM Reels
'**************************************


if VR_Room = 0 and cabmode = 0 Then
	Set LampCallback = GetRef("UpdateDTLamps")
End If

Sub UpdateDTLamps()

	If Controller.Lamp(6) = 0 Then: BG6.setValue(0):	Else: BG6.setValue(1)
	If Controller.Lamp(7) = 0 Then: BG7.setValue(0):	Else: BG7.setValue(1)
	If Controller.Lamp(41) = 0 Then: BG41.setValue(0):	Else: BG41.setValue(1)
	If Controller.Lamp(42) = 0 Then: BG42.setValue(0):	else: BG42.setValue(1)
	If Controller.Lamp(51) = 0 Then: BG51.setValue(0):	Else: BG51.setValue(1)
	If Controller.Lamp(52) = 0 Then: BG52.setValue(0):	Else: BG52.setValue(1)
	If Controller.Lamp(57) = 0 Then: BG57.setValue(0):	Else: BG57.setValue(1)
	If Controller.Lamp(58) = 0 Then: BG58.setValue(0):	Else: BG58.setValue(1)
	If Controller.Lamp(59) = 0 Then: BG59.setValue(0):	Else: BG59.setValue(1)
	If Controller.Lamp(60) = 0 Then: BG60.setValue(0):	Else: BG60.setValue(1)
	If Controller.Lamp(61) = 0 Then: BG61.setValue(0):	Else: BG61.setValue(1)
	If Controller.Lamp(62) = 0 Then: BG62.setValue(0):	Else: BG62.setValue(1)
	If Controller.Lamp(63) = 0 Then: BG63.setValue(0):	Else: BG63.setValue(1)
	If Controller.Lamp(64) = 0 Then: BG64.setValue(0):	Else: BG64.setValue(1)

End Sub





