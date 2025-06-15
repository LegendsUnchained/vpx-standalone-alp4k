'==================================================================================================================================================='
'																								                                                    ' 			  	 
'        	                 		     APOLLO 13	     			                			                                                    '
'          	              		    Sega Pinball (1995)             		                   	                                                    '
'		  				  http://www.ipdb.org/machine.cgi?id=3592								                                                    '
'																								                                                    '
' 	  		 	 	  		  Created by ICPjuggla and Herweh	vp9								                                                    '
'										 Balater vpx											                                                    '
'								Contribution	Thalamus, JP's, DJRobX, Rothbauerw, Amgrim, Bigus1, 32assassin,ClarKent, Chucky87, agentEighty6     '			  	 
'==================================================================================================================================================='

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const UseVPMModSol = 1

Dim  varhidden, UseVPMColoredDMD, bgsko, preLoad8Balls
	bgsko=0
If Table1.ShowDT = false then
Ramp032.visible = 0:Ramp033.visible = 0
    UseVPMColoredDMD = false
	varhidden=1
Else
	Ramp032.visible = 0:Ramp033.visible = 0
    UseVPMColoredDMD = true
	varhidden=0
End If

' ***********************************************************************************************
' OPTIONS
' ***********************************************************************************************


' VPinMAME ROM name
Const cGameName 			= "apollo13"		' enter string of valid ROM

' ball size
Const BallSize 				= 50				' sets the ball size

' Preload 8 ball ramp
'	0 = not preloaded (will load before first game)
'	1 = balls are preloaded
preLoad8Balls = 1

' ***********************************************************************************************
' OPTIONS END
' ***********************************************************************************************
' Thalamus 2020 January : Improved directional sounds/
' !! NOTE : Table not verified yet !!

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 400    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )

' The rest of the values are multipliers
'
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 0.5  ' Bumpers volume.
Const VolRol    = 0.1    ' Rollovers volume.
Const VolGates  = 0.1  ' Gates volume.
Const VolMetal  = 0.2   ' Metals volume.
Const VolRB     = 5   ' Rubber bands volume.
Const VolRH     = 5    ' Rubber hits volume.
Const VolPo     = 1   ' Rubber posts volume.
Const VolPi     = 1    ' Rubber pins volume.
Const VolPlast  = 0.01    ' Plastics volume.
Const VolTarg   = 2   ' Targets volume.
Const VolWood   = 0.1   ' Woods volume.
Const VolKick   = 0.1    ' Kicker volume.
Const VolSpin   = 0.2  ' Spinners volume.
Const VolFlip   = 5    ' Flipper volume.
Const VolDrain  = 1    ' Drain volume.
Const VolSling  = 1    ' Slingshots volume.
Const VolBRol   = .01  ' Volume Ball Rolling.
Const VolPRol   = .1   ' Volume Plastics Rolling.
Const VolMRol   = .2   ' Volume Metals Rolling.
Const VolDrop   = .4   ' Balldrop volume.



' ===============================================================================================
' some general constants and variables
' ===============================================================================================
	
Const UseLamps 			= False
Const UseGI 			= False
Const UseSync 			= True
Const HandleMech 		= False

Const SSolenoidOn 		= "SolenoidOn"
Const SSolenoidOff 		= "SolenoidOff"
Const SCoin 			= "Coin"
Const SKnocker 			= "Knocker"

Dim I, x, obj, bsTroughSuperVUK, drp, rfd, lfd, flipp, mmoon, bkgg

Const SFlipperOn="Flipper2",SFlipperOff="Flipper"

' ===============================================================================================
' load game controller
' ===============================================================================================

flipp=0 '1 flipper works continuously
bkgg=1	'1 GI Animation on Backglass

LoadVPM  "01560000", "SEGA2.VBS", 3.56



' ===============================================================================================
' solenoids
' ===============================================================================================

SolCallback(1) 			= "SolTroughVUKOut"
SolCallback(2) 			= "SolAPlunger"
SolCallback(3) 			= "SolRightEject"
SolCallback(4) 			= "SolTopEject"
SolCallback(5) 			= "SolRocketEject"
SolCallback(7) 			= "ShakeRocket"
SolCallback(8) 			= "SolKnocker"
SolCallback(14)         = "SolTroughSuperVUKOut"
SolCallback(15)         = "SolLFlipperd"
SolCallback(16)         = "SolRFlipperd"
SolCallback(17)			= "SolTroughOut"
SolCallback(18)			= "SolRampLift"
SolCallback(19) 		= "SolmoonLift"
SolCallback(20) 		= "SolRocketLift"
SolCallback(21) 		= "SolTrapDoor"
SolCallback(22)			= "Sol8BLockPlunger"
SolCallback(23)			= "SolDrainPost"
SolCallback(34)			= "SolMoon"


' flasher
SolCallback(25)			= "SolFLamp1"
SolCallback(26)			= "SolFLamp2"
SolCallback(27)			= "SolFLamp3"
SolCallback(28)			= "SolFLamp4"
SolCallback(29)			= "SolFLamp5"
SolCallback(30)			= "SolFLamp6"
SolCallback(31)			= "SolFLamp7"
SolCallback(32)			= "SolFLamp8"


' =====================================================================================================
' table events
' =====================================================================================================
Set GICallback = GetRef("GIUpdate")

Sub GIUpdate(no, Enabled)
If Enabled Then
		dim xx
		For each xx in GI:xx.State = 1:	Next
        Table1.colorgradeimage = "ColorGradeLUT256x16_ConSat"
Else 
		For each xx in GI:xx.State = 0: Next
        Table1.colorgradeimage = "ColorGrade_4"
End If
	If bgsko Then
		If enabled Then
			Controller.b2ssetdata 79,1
		Else
			Controller.b2ssetdata 79,0
		End If
	End If
End Sub


Sub Table1_Init
	On Error Resume Next
	With Controller
		.GameName=cGameName
		.SplashInfoLine							="Apollo 13, Sega, 1995"
		.HandleKeyboard 						= False
		.ShowTitle								= False
		.ShowDMDOnly							= True
		.ShowFrame								= False
		.Hidden 								= VarHidden
		.Games(cGameName).Settings.Value("rol")	= 0
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
	If b2son Then bgsko = bkgg	
Controller.SolMask(0)=0
      vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
	'	Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0
	
	' basic pinmame timer
	PinMAMETimer.Interval	= PinMAMEInterval
	PinMAMETimer.Enabled	= True

	' nudging
	vpmNudge.TiltSwitch		= 1
	vpmNudge.Sensitivity	= 3
	vpmNudge.TiltObj 		= Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)
 
	

	Set bsTroughSuperVUK = New cvpmBallStack
		bsTroughSuperVUK.InitSw 0, 48, 0, 0, 0, 0, 0, 0
		bsTroughSuperVUK.InitKick BallReleaseSuperVUK, 260, 5
		bsTroughSuperVUK.InitExitSnd SoundFX("BallRelease",DOFContactors), SoundFX("SolOn",DOFContactors)

     ' magnet
	Set mmoon = New cvpmMagnet
		With mmoon
			.InitMagnet mMoonLock, 90
			.GrabCenter = True
			.MagnetOn = 0
			.CreateEvents "mmoon"
		End With

		wall173.collidable=1
		moonballstop.collidable=0
		moonballstopb.collidable=0
		mmoon.MagnetOn = 0
		moonlock.Enabled=0
		Ramp139.collidable=1
		Ramp030.collidable=0
		MultiBallPost.IsDropped = 0

		sw14k.CreateSizedBall(25)
		sw14k.kick 0,0
		sw13k.CreateSizedBall(25)
		sw13k.kick 0,0
		sw12k.CreateSizedBall(25)
		sw12k.kick 0,0
		sw11k.CreateSizedBall(25)
		sw11k.kick 0,0
		sw10k.CreateSizedBall(25)
		sw10k.kick 0,0

		If preLoad8Balls = 0 Then
			sw24b.enabled=1
			sw23b.enabled=0
			sw22b.enabled=0
			sw21b.enabled=0
			sw20b.enabled=0
			sw19b.enabled=0
			sw18b.enabled=0
			sw17b.enabled=0
			bsTroughSuperVUK.AddBall BallReleaseSuperVUK
			bsTroughSuperVUK.AddBall BallReleaseSuperVUK
			bsTroughSuperVUK.AddBall BallReleaseSuperVUK
			bsTroughSuperVUK.AddBall BallReleaseSuperVUK
			bsTroughSuperVUK.AddBall BallReleaseSuperVUK
			bsTroughSuperVUK.AddBall BallReleaseSuperVUK
			bsTroughSuperVUK.AddBall BallReleaseSuperVUK
			bsTroughSuperVUK.AddBall BallReleaseSuperVUK
		Else
			sw24b.enabled=1
			sw23b.enabled=1
			sw22b.enabled=1
			sw21b.enabled=1
			sw20b.enabled=1
			sw19b.enabled=1
			sw18b.enabled=1
			sw17b.enabled=1
			sw24b.CreateSizedBall(25)
			sw23b.CreateSizedBall(25)
			sw22b.CreateSizedBall(25)
			sw21b.CreateSizedBall(25)
			sw20b.CreateSizedBall(25)
			sw19b.CreateSizedBall(25)
			sw18b.CreateSizedBall(25)
			sw17b.CreateSizedBall(25)
			Controller.Switch(17) = 1
			Controller.Switch(18) = 1
			Controller.Switch(19) = 1
			Controller.Switch(20) = 1
			Controller.Switch(21) = 1
			Controller.Switch(22) = 1
			Controller.Switch(23) = 1
			Controller.Switch(24) = 1
		End If
		Wallpost001.collidable=0
		InitMoon
		InitRocket
End Sub
 
Sub Table1_Exit()
	Controller.Stop
End Sub

Sub Table1_Paused
	Controller.Pause = True
End Sub
Sub Table1_UnPaused
	Controller.Pause = False
End Sub

Sub Table1_KeyDown(ByVal keycode)
	If keycode = LeftFlipperKey Then SolLFlipper 1
	If keycode = RightFlipperKey Then SolRFlipper 1
	If vpmKeyDown(keycode) Then Exit Sub
	If keycode = PlungerKey Then Controller.Switch(9) = True
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = LeftFlipperKey Then SolLFlipper 0
	If keycode = RightFlipperKey Then SolRFlipper 0
	If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode = PlungerKey Then Controller.Switch(9) = False
End Sub



' ===============================================================================================
' ball draining, release and auto plunger
' ===============================================================================================

Sub SolTroughOut(Enabled)
	If controller.Switch(16)=0 Then
	If Enabled Then 
		sw14k.destroyball
		Controller.Switch(14) 	= False
		TroughVUKOut.Createball
		TroughVUKOut.kick 0,0
		If Controller.Switch(13) = True Then sw13k.kick 60,15
		PlaySound SoundFX("SolOn",DOFContactors)
	End If
	End If
End Sub

Sub SolTroughVUKOut(Enabled)
	If Enabled Then
		TroughVUKOut.Kick 60,10
		PlaySound SoundFX("BallRelease",DOFContactors)
	End If
End Sub

Sub sw10k_Hit()
	Controller.Switch(10) 	= True
	If Controller.Switch(11) = False Then sw10k.kick 60,15
End Sub
Sub sw10k_Unhit()
	Controller.Switch(10) 	= False
End Sub

Sub sw11k_Hit()
	Controller.Switch(11) 	= True
	If Controller.Switch(12) = False Then sw11k.kick 60,15
End Sub
Sub sw11k_Unhit()
	Controller.Switch(11) 	= False
	If Controller.Switch(10) = True Then sw10k.kick 60,15
End Sub

Sub sw12k_Hit()
	Controller.Switch(12) 	= True
	If Controller.Switch(13) = False Then sw12k.kick 60,15
End Sub
Sub sw12k_Unhit()
	Controller.Switch(12) 	= False
	If Controller.Switch(11) = True Then sw11k.kick 60,15
End Sub

Sub sw13k_Hit()
	Controller.Switch(13) 	= True
	If Controller.Switch(14) = False Then sw13k.kick 60,15
End Sub
Sub sw13k_Unhit()
	Controller.Switch(13) 	= False
	If Controller.Switch(12) = True Then sw12k.kick 60,15
End Sub

Sub sw14k_Hit()
	Controller.Switch(14) 	= True
End Sub
Sub sw14k_Unhit()
	Controller.Switch(14) 	= False
	If Controller.Switch(13) = True Then sw13k.kick 60,15
End Sub



Sub sw15_Hit()
	Controller.Switch(15) 	= True
End Sub
Sub sw15_Unhit()
	Controller.Switch(15) 	= False
End Sub


Sub sw16_Hit()
	Controller.Switch(16) 	= True
End Sub
Sub sw16_Unhit()
	Controller.Switch(16) 	= False
End Sub


Sub sw17b_Hit()
	Controller.Switch(17) 	= True
End Sub
Sub sw17b_Unhit()
	Controller.Switch(17) 	= False
End Sub
Sub sw18b_Hit()
	Controller.Switch(18) 	= True
	sw17b.enabled=1
End Sub
Sub sw18b_Unhit()
	Controller.Switch(18) 	= False
	sw17b.enabled=0
End Sub
Sub sw19b_Hit()
	Controller.Switch(19) 	= True
	sw18b.enabled=1
End Sub
Sub sw19b_Unhit()
	Controller.Switch(19) 	= False
	sw18b.enabled=0
End Sub
Sub sw20b_Hit()
	Controller.Switch(20) 	= True
	sw19b.enabled=1
End Sub
Sub sw20b_Unhit()
	Controller.Switch(20) 	= False
	sw19b.enabled=0
End Sub
Sub sw21b_Hit()
	Controller.Switch(21) 	= True
	sw20b.enabled=1
End Sub
Sub sw21b_Unhit()
	Controller.Switch(21) 	= False
	sw20b.enabled=0
End Sub
Sub sw22b_Hit()
	Controller.Switch(22) 	= True
	sw21b.enabled=1
End Sub
Sub sw22_Unhit()
	Controller.Switch(22) 	= False
	sw21b.enabled=0
End Sub
Sub sw23b_Hit()
	Controller.Switch(23) 	= True
	sw22b.enabled=1
End Sub
Sub sw23b_Unhit()
	Controller.Switch(23) 	= False
	sw22b.enabled=0
End Sub
Sub sw24b_Hit()
	Controller.Switch(24) 	= True
	sw23b.enabled=1
End Sub
Sub sw24b_Unhit()
	Controller.Switch(24) 	= False
	sw23b.enabled=0
End Sub




Sub sw32_hit()
vpmTimer.PulseSw 32
End Sub
Sub sw31_hit()
vpmTimer.PulseSw 31
End Sub
Sub sw30_hit()
vpmTimer.PulseSw 30
End Sub
Sub sw29_hit()
vpmTimer.PulseSw 29
End Sub
Sub sw28_hit()
vpmTimer.PulseSw 28
End Sub
Sub sw27_hit()
vpmTimer.PulseSw 27
End Sub
Sub sw26_hit()
vpmTimer.PulseSw 26
End Sub
Sub sw25_hit()
vpmTimer.PulseSw 25
End Sub
Sub sw38_hit()
vpmTimer.PulseSw 38
End Sub
Sub sw40_hit()
vpmTimer.PulseSw 40
End Sub
Sub sw49a_hit()
vpmTimer.PulseSw 49
End Sub
Sub sw49b_hit()
vpmTimer.PulseSw 49
End Sub
Sub sw49c_hit()
vpmTimer.PulseSw 49
End Sub

Sub SolAPlunger(Enabled)
	If Enabled Then
		AutoPlunger.Kick 0, 35-int(Rnd(30))
		PlaySound SoundFX("SolenoidOn",DOFContactors)
	end If
End Sub


'flipper
Sub sollFlipperd(Enabled)
     lfd=Enabled
 End Sub
Sub solrFlipperd(Enabled)
     rfd=Enabled
End Sub
Sub sollFlipper(Enabled)
	if lfd  or rfd or flipp Then
     If Enabled Then
         PlaySound SoundFXDOF("fx_Flipperup",101,DOFOn,DOFFlippers):LeftFlipper.RotateToEnd
     Else
         PlaySound SoundFXDOF("fx_Flipperdown",101,DOFOff,DOFFlippers):LeftFlipper.RotateToStart
     End If
	Else
		LeftFlipper.RotateToStart
	End If
  End Sub

Sub solrflipper(Enabled)
	if rfd or lfd or flipp Then
     If Enabled Then
         PlaySound SoundFXDOF("fx_Flipperup",102,DOFOn,DOFFlippers):RightFlipper.RotateToEnd
     Else
         PlaySound SoundFXDOF("fx_Flipperdown",102,DOFOff,DOFFlippers):RightFlipper.RotateToStart
     End If
	Else
		RightFlipper.RotateToStart
	End If
End Sub





' ===============================================================================================
' bumpers
' ===============================================================================================
Sub Bumper1_Hit()
vpmTimer.PulseSw 35	
PlayBumperSound
End Sub

Sub Bumper2_Hit()
vpmTimer.PulseSw 33	
PlayBumperSound
End Sub

Sub Bumper3_Hit()
vpmTimer.PulseSw 34	
PlayBumperSound
End Sub

Sub PlayBumperSound()
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySound SoundFX("bumper_1",DOFContactors)
			Case 2 : PlaySound SoundFX("bumper_2",DOFContactors)
			Case 3 : PlaySound SoundFX("bumper_3",DOFContactors)
		End Select
End Sub


' ===============================================================================================
' knocker
' ===============================================================================================

Sub SolKnocker(Enabled)
	If Enabled Then PlaySound SoundFX("knocker",DOFKnocker)
End Sub

' ===============================================================================================
' ramp diverter
' ===============================================================================================

Dim diverterAdd

InitDiverter

Sub InitDiverter()
	Diverter2.collidable = 0
	Diverter1.collidable = 1
	diverterAdd			= 1
End Sub

Sub DiverterTriggerLeft_Hit()
	ActiveBall.VelY 			= 0
	Diverter1.collidable 		= 1
	Diverter2.collidable 		= 0
	diverterAdd					= -3
	RampDiverterTimer.Enabled 	= True
End Sub
Sub DiverterTriggerRight_Hit()
	ActiveBall.VelY 			= 0
	Diverter1.collidable 		= 0
	Diverter2.collidable 		= 1
	diverterAdd					= 3
	RampDiverterTimer.Enabled 	= True
End Sub

Sub RampDiverterTimer_Timer()
	RampDiverter.RotY = RampDiverter.RotY + diverterAdd
	If RampDiverter.RotY <= 0 Or RampDiverter.RotY => 30 Then RampDiverterTimer.Enabled = False
End Sub


' ===============================================================================================
' spinner
' ===============================================================================================

Sub sw41_Spin()
	vpmTimer.PulseSw 41

	Playsound SoundFX("Gate2_low",DOFContactors)
End Sub

Sub sw55_Hit()
	vpmTimer.PulseSw 55
	Playsound SoundFX("Gate2_low",DOFContactors)
End Sub


' ===============================================================================================
' drain post
' ===============================================================================================

Sub SolDrainPost(Enabled)
	If enabled Then
		DrainPost.TransZ = 40
		wallpost.HeightTop = 41
		Wallpost001.collidable=1
		PlaySound SoundFX("SolenoidOn",DOFContactors)
	Else
		DrainPost.TransZ = 0
		wallpost.HeightTop = 1
		Wallpost001.collidable=0
		PlaySound SoundFX("SolenoidOff",DOFContactors)
	end If
End Sub



' ===============================================================================================
' trap door to 8-ball lock
' ===============================================================================================

dim TrapDoorDir, TrapDoorHeight
Sub SolTrapDoor(Enabled)
	if enabled Then
		Ramp139.collidable=0
		ramp030.collidable=1
		TrapDoorDir=1 'Go Up
		TrapDoorHeight= 155
		Ramp029.HeightBottom=TrapDoorHeight
		TrapDoorTimer.Interval=5
		TrapDoorTimer.Enabled=True
	Else
		Ramp139.collidable=1
		TrapDoorDir=0 'Go Down
		TrapDoorHeight= 260
		Ramp029.HeightBottom=TrapDoorHeight
		TrapDoorTimer.Interval=5
		TrapDoorTimer.Enabled=True
		ramp030.collidable=0
	end If
End Sub

Sub TrapDoorTimer_Timer()
	if TrapDoorDir=1 then
		TrapDoorHeight=TrapDoorHeight+TrapDoorTimer.Interval
		If TrapDoorHeight >= 260 then TrapDoorTimer.enabled=False:PlaySound SoundFX("SolenoidOff",DOFContactors)
	Else
		TrapDoorHeight=TrapDoorHeight-TrapDoorTimer.Interval
		If TrapDoorHeight <= 155 then TrapDoorTimer.enabled=False:PlaySound SoundFX("SolenoidOff",DOFContactors)
	end If
	Ramp029.HeightBottom=TrapDoorHeight
End Sub


sub Sol8BLockPlunger(Enabled)
		MultiBallPost.IsDropped = 1
		wall173.collidable= 0
		drp=1
		sw24b.Kick 180,1
		sw24b.enabled=false
		sw23b.Kick 180,1
		sw23b.enabled=false
		sw22b.Kick 180,1
		sw22b.enabled=false
		sw21b.Kick 180,1
		sw21b.enabled=false
		sw20b.Kick 180,1
		sw20b.enabled=false
		sw19b.Kick 180,1
		sw19b.enabled=false
		sw18b.Kick 180,1
		sw18b.enabled=false
		sw17b.Kick 180,1
		sw17b.enabled=false
		Controller.Switch(17) = 0
		Controller.Switch(18) = 0
		Controller.Switch(19) = 0
		Controller.Switch(20) = 0
		Controller.Switch(21) = 0
		Controller.Switch(22) = 0
		Controller.Switch(23) = 0
		Controller.Switch(24) = 0
End Sub

Sub sw16b_hit()
	If drp=1 Then
		drp=0
		MultiBallPost.IsDropped = 0
		wall173.collidable= 1
		sw24b.enabled=1
		sw23b.enabled=0
		sw22b.enabled=0
		sw21b.enabled=0
		sw20b.enabled=0
		sw19b.enabled=0
		sw18b.enabled=0
		sw17b.enabled=0
	End If
End Sub



'********************************************
' ramp lift - Rewritten by UnclePaulie to get the back ramp lift to work correctly with orbit and moon gravity mission
'********************************************

' RampLiftVisual is the visibleramp, and it's not collidable. 
' RampLiftDown is a non visible ramp.  Bottom is at zero.  And set at not collidable
' RampLiftUP is the other non visible ramp.  Bottom is at 70.  And set at collidable.

dim RampLiftPos, RampLiftHeight

Sub SolRampLift(Enabled)
	if enabled then 
		RampLiftPos=1
		RampLiftHeight= 70
		RampLiftTimer.Interval=5
		RampLiftTimer.Enabled=1
		PlaySound SoundFX("DropTarget_Down",DOFContactors)
	Else
		RampLiftPos=2
		RampLiftHeight= 0
		RampLiftTimer.Interval=5
		RampLiftTimer.Enabled=1
		PlaySound SoundFX("DropTarget_Down",DOFContactors)
	end if
End Sub

Sub RampLiftTimer_Timer()
Select Case RampLiftPos
	Case 1:			'ramp is going down
		RampLiftHeight=RampLiftHeight-RampLiftTimer.Interval
		RampLiftDown.collidable = 1
		RampLiftUp.collidable = 0
		RampLiftVisual.HeightBottom=RampLiftHeight
		If RampLiftHeight <= 0 then RampLiftTimer.enabled=False
	Case 2:			'ramp is going up
		RampLiftHeight=RampLiftHeight+RampLiftTimer.Interval
		RampLiftDown.collidable = 0
		RampLiftUp.collidable = 1
		RampLiftVisual.HeightBottom=RampLiftHeight
		If RampLiftHeight >= 70 then RampLiftTimer.enabled=False
End Select
End Sub


'********************************************
' rocket  - Rewritten by UnclePaulie to get multiball functions to work with subway and timing
'********************************************

Sub sw43k_Hit()
	Controller.Switch(43) 	= True
End Sub

Sub sw43k_Unhit()
	Controller.Switch(43) 	= False
End Sub


Dim RocketDirection '0 = ready to go up or moving up, 1 = ready to go down or moving down

Sub InitRocket()
	If RocketDirection = 1 Then
		RocketTimer.Interval = 24
		RocketTimer.Enabled  = True	
		PlaySoundAtVol SoundFX("Motor_long",DOFContactors), Rocket, 0.5
	Else
		RocketDirection = 0
		Controller.Switch(50) = True
		Controller.Switch(51) = False
		RocketTrough.Enabled = 0
		RocketTroughHole.Enabled = 0
	End If
End Sub


Sub SolRocketEject(Enabled)
	If Enabled Then
		sw43k.kick 155+int(rnd(10)), 0.1
		PlaySound SoundFX("SolenoidOn",DOFContactors)
	End If
End Sub


Sub SolRocketLift(Enabled)
	If Enabled Then
		RocketTimer.Interval = 24
		RocketTimer.Enabled  = True	
		PlaySoundAtVol SoundFX("Motor_long",DOFContactors), Rocket, 0.5
	End If
End Sub

Sub RocketTimer_Timer()
	If RocketDirection = 0 Then
		If Rocket.RotX <= -18 Then
			PlaySound SoundFX("DropTarget_Down",DOFContactors)
			sw43k.kick 155+int(rnd(10)), 0.1
			PlaySound SoundFX("SolenoidOn",DOFContactors)
			RocketTimer.Enabled 	= False
			Controller.Switch(51) 	= True  ' it's up
			Controller.Switch(50) 	= False  ' it's not home
			RocketTrough.Enabled = True
			RocketTroughHole.Enabled = True ' open rocket hole when all the way up
			RocketDirection = 1
			Exit Sub
		Else
			Controller.Switch(51) 	= False  ' it's not up
			Controller.Switch(50) 	= False  ' it's not home
			RocketTimer.Enabled 	= True
			RocketTrough.Enabled = True
			RocketTroughHole.Enabled = True ' open rocket hole when all the way up
			Rocket.RotX = Rocket.RotX - 0.1
		End If

	Else
		If Rocket.RotX >=0 Then 
			PlaySound SoundFX("DropTarget_Down",DOFContactors)
			RocketTimer.Enabled 	= False
			Controller.Switch(51) 	= False  ' it's not up
			Controller.Switch(50) 	= True  ' it's home
			RocketTrough.Enabled = False
			RocketTroughHole.Enabled = False ' rocket hole closed
			RocketDirection = 0
			Rocket.RotX =0
			Exit Sub
		Elseif Rocket.RotX <-12 Then
			Controller.Switch(51) 	= False  ' it's not up
			Controller.Switch(50) 	= False  ' it's not home
			RocketTrough.Enabled = True
			RocketTroughHole.Enabled = True ' open rocket hole when all the way up
			Rocket.RotX = Rocket.RotX + 0.1
		Else
			Controller.Switch(51) 	= False  ' it's not up
			Controller.Switch(50) 	= False  ' it's not home
			RocketTrough.Enabled = False
			RocketTroughHole.Enabled = False ' open rocket hole when all the way up
			Rocket.RotX = Rocket.RotX + 0.1
		End If
	End If
End Sub


Sub SubwayBallReleaseSuperVUK_Hit()
	Playsound SoundFX("ball_bounce_low",DOFContactors)
	me.destroyball
	bsTroughSuperVUK.AddBall Me
End Sub	

'Sub ShakeRocket(direction)
Sub ShakeRocket (Enabled)
	If Not RocketTimer.Enabled Then
		RocketShakeTimer.Interval = 5+Int(Rnd(5))
		RocketShakeTimer.Enabled  = True
	End If
End Sub

Sub RocketShakeTimer_Timer()
	' nudging
	Select Case RocketShakeTimer.Interval
	Case 0
		Rocket.RotX = Rocket.RotY - 1
	Case 1, 2
		Rocket.RotX = Rocket.RotX + 1
	Case 3
		Rocket.RotX = Rocket.RotY - 1
	Case 5
		Rocket.RotY = Rocket.RotY - 1
	Case 6, 7
		Rocket.RotY = Rocket.RotY + 1
	Case 8
		Rocket.RotY = Rocket.RotY - 1
	Case 10
		Rocket.RotY = Rocket.RotY + 1
	Case 11, 12
		Rocket.RotY = Rocket.RotY - 1
	Case 13
		Rocket.RotY = Rocket.RotY + 1
	Case Else
		RocketShakeTimer.Enabled = False
	End Select
	RocketShakeTimer.Interval = RocketShakeTimer.Interval + 1
End Sub


' ===============================================================================================
' moon
' ===============================================================================================

Dim MoonBall, radians, angle, radius,angleb, baly, balyb, rada, radab

Sub InitMoon()
	' rotation stuff
	radians 				= 3.1415926 / 180
	angle 					= 0
	angleb					=180-Moon.objrotz
	radius 					= 126

	' moon is at home
	Controller.Switch(36) 	= True
	Controller.Switch(37) 	= False
End Sub


Sub SolMoon(Enabled)
	if enabled Then
		if Controller.Switch(36) Then
			mmoon.MagnetOn = 1
			MoonLock.Enabled = 1
		End If
	Else
		mmoon.MagnetOn = 0
		MoonLock.Enabled = 0
		moonballstop.collidable=0
		moonballstopb.collidable=0
		if Controller.Switch(36) Then
			If Controller.Switch(19) = 0 Then
				actvb=0
				set MoonBall = Nothing
			End If
		End If
	End If
End Sub

Dim actvb


Sub MoonLock_Hit()
	If Controller.Switch(36) Then
		moonballstop.collidable=1
		moonballstopb.collidable=1
		actvb=1
		PlaySound SoundFX("collide1",DOFContactors)
		Set MoonBall 	= ActiveBall
		MoonBall.VelX	= 0
		MoonBall.VelY	= 0
		MoonBall.VelZ	= 0
	End If
		mmoon.MagnetOn = 0
		MoonLock.Enabled = 0	
End Sub


sub SolmoonLift(Enabled)
	if Enabled Then
	MoonTimer.Interval 	= 20
	MoonTimer.Enabled  	= True
	PlaySound SoundFX("SolenoidOn",DOFContactors)
	End If
End Sub

Sub MoonTimer_Timer()
	mmoon.MagnetOn = 0	
	moonballstop.collidable=0
	moonballstopb.collidable=0
	moonlock.enabled=0
	if controller.Solenoid(19) Then
		If Moon.RotX <= -180 Then
				Controller.Switch(37) 	= True
			If Controller.Solenoid(34)=0 Then
				set MoonBall = Nothing
				If actvb = 1 Then
					PlaySound SoundFX("ball_bounce_low",DOFContactors)
					actvb=0
				End If
				Exit Sub
			End If
		Else
			Controller.Switch(36) = False
			Moon.RotX 	= Moon.RotX - 1		
		End If
	Else
		If Moon.RotX >= Moon.ObjRotX Then
				Controller.Switch(36) 	= True
			If Controller.Solenoid(34)=0 Then
				MoonTimer.Enabled 		= False
				set MoonBall = Nothing
				PlaySound SoundFX("ball_bounce_low",DOFContactors)
				actvb=0
				Exit Sub
			End If
		Else
			Controller.Switch(37) = False
			Moon.RotX 	= Moon.RotX +2 -actvb	
		End If
	End If
			If actvb Then
				angle= abs(moon.rotX)
				rada=radians*angle
				radab=radians*angleb
				MoonBall.VelX	= 0
				MoonBall.VelY	= 0
				MoonBall.VelZ	= 0
				MoonBall.z= sin (rada)*radius+moon.z
				balyb=cos( rada)*radius
				MoonBall.y= balyb*cos(radab)+Moon.y
				MoonBall.x=Sin(radab)*balyb+Moon.x
			End If
End Sub


' ===============================================================================================
' super VUK
' ===============================================================================================

Sub SolTroughSuperVUKOut(Enabled)
	If Enabled Then
		If bsTroughSuperVUK.Balls > 0 Then bsTroughSuperVUK.ExitSol_On
	End If
End Sub



' ===============================================================================================
' top saucer
' ===============================================================================================

Sub sw54k_Hit()
	Controller.Switch(54) 	= True
	PlaySound SoundFX("kicker_enter",DOFContactors)
End Sub
Sub sw54k_Unhit()
	Controller.Switch(54) 	= False
End Sub
 
Sub SolTopEject(Enabled)
    If Enabled Then
		sw54k.Kick 215,5+ Int(Rnd()*5)
		PlaySound SoundFX("SolenoidOn",DOFContactors)
	End If
End Sub

' ===============================================================================================
' right saucer
' ===============================================================================================

Sub sw47k_Hit()
	Controller.Switch(47) 	= True
	PlaySound SoundFX("kicker_enter",DOFContactors)
End Sub
Sub sw47k_Unhit()
	Controller.Switch(47) 	= False
End Sub
 
Sub SolRightEject(Enabled)
    If Enabled Then
		sw47k.Kick 200,5+ Int(Rnd()*5)
		PlaySound SoundFX("SolenoidOn",DOFContactors)
	End If
End Sub

' ===============================================================================================
' switches
' ===============================================================================================

' top lanes
Sub sw52_Hit()
	controller.Switch(52)=1
End Sub
Sub sw52_Unhit()
	controller.Switch(52)=0
End Sub
Sub sw53_Hit()
	controller.Switch(53)=1
End Sub
Sub sw53_Unhit()
	controller.Switch(53)=0
End Sub

' orbit
Sub sw45_Hit()
	controller.Switch(45)=1
End Sub
Sub sw45_Unhit()
	controller.Switch(45)=0
End Sub
Sub sw46_Hit()
	controller.Switch(46)=1
End Sub
Sub sw46_Unhit()
	controller.Switch(46)=0
End Sub

' return lanes
Sub sw59_Hit()
	controller.Switch(59)=1
End Sub
Sub sw59_Unhit()
	controller.Switch(59)=0
End Sub
Sub sw60_Hit()
	controller.Switch(60)=1
End Sub
Sub sw60_Unhit()
	controller.Switch(60)=0
End Sub

' out lanes
Sub sw57_Hit()
	controller.Switch(57)=1
End Sub
Sub sw57_Unhit()
	controller.Switch(57)=0
End Sub
Sub sw58_Hit()
	controller.Switch(58)=1
End Sub
Sub sw58_Unhit()
	controller.Switch(58)=0
End Sub

' right ramp
Sub sw44_Hit()
	controller.Switch(44)=1
End Sub
Sub sw44_Unhit()
	controller.Switch(44)=0
End Sub

Sub sw56_Hit()
	controller.Switch(56)=1
End Sub
Sub sw56_Unhit()
	controller.Switch(56)=0
End Sub





' ===============================================================================================
' sound effects and some physics for gates, metals, plastics, woods and rubbers
' ===============================================================================================

Dim MetalsSoundActive
Dim WoodsSoundActive

Sub FallingBall_Hit(idx)
	PlaySound SoundFX("ballhit4",DOFContactors)
End Sub

Sub Gate1_Hit()
	PlaySoundEffect 1, "gate2_low", 8, "gate2", 0, "", 1, 0
End Sub
Sub Gate2_Hit()
	PlaySoundEffect 1, "gate2_low", 8, "gate2", 0, "", 1, 0
End Sub
Sub Gate2Back_Hit()
	PlaySoundEffect 2, "gateback_low", 6, "gateback", 0, "", -1, 0
End Sub

' targets
 Sub Targets_Hit(idx)
	PlaySoundEffect 2, "target_low", 8, "target", 0, "", 0, 0
End Sub

' plastics
Sub Plastics_Hit()
	PlaySound SoundFX("PlasticHit",DOFContactors)
End Sub

' rubbers
Sub PostRubbers_Hit(idx)
	PlaySoundEffect 1, RandomSoundRubberLowVolume(), 4, RandomSoundRubber(), 14, "bump", 0, 0
End Sub
Sub MiscRubbers_Hit(idx)
	PlaySoundEffect 1, RandomSoundRubberLowVolume(), 4, RandomSoundRubber(), 14, "bump", 0, 0
End Sub



' metal rails
Sub MetalRails_Hit(idx)
	PlaySound "metalrolling", 0, VolMulti(ActiveBall,Volmetal), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
'	PlaySound "metalrolling"
End Sub
Sub MetalRails_unHit(idx)
	stopSound "metalrolling", 0, VolMulti(ActiveBall,VolMetal), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
'	StopSound "metalrolling"
End Sub

' plastic rails
Sub PlasticRail_Hit(idx)
	PlaySound "fx_ballrolling0", 0, VolMulti(ActiveBall,VolPlast), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
'	PlaySound "metalrolling"
End Sub
Sub PlasticRail_unHit(idx)
	stopSound "fx_ballrolling0", 0, VolMulti(ActiveBall,VolPlast), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
'	StopSound "metalrolling"
End Sub

' metal hits
Sub Metals_Hit(idx)
	If MetalsSoundActive Then Exit Sub
	PlaySoundEffect 2, "metalhit_medium_low", 6, "metalhit_medium", 0, "", 0, 0
	MetalsSoundActive		= True
	MetalSoundTimer.Enabled	= True
End Sub
Sub MetalSoundTimer_Timer(idx)
	Me.Enabled 			= False
	MetalsSoundActive 	= False
End Sub

Sub MetalEnds_Hit(idx)
	PlaySoundEffect 1.5, "metalhit", 6, "metalhit", 0, "", 0, 0
End Sub

' woods
Sub Woods_Hit(idx)
	If WoodsSoundActive Then Exit Sub
	PlaySoundEffect 1.5, "woodhit_low", 5, "woodhit", 0, "", 0, 0
	WoodsSoundTimer.Enabled	= True
	WoodsSoundActive		= True
End Sub
Sub WoodsSoundTimer_Timer()
	Me.Enabled			= False
	WoodsSoundActive	= False
End Sub


Sub SolFLamp1(Enabled)
	lf1a.state=Enabled
	lf1b.state=Enabled
	lf1c.state=Enabled
End Sub
Sub SolFLamp2(Enabled)
	lf2a.state=Enabled
	lf2b.state=Enabled
	lf2c.state=Enabled
End Sub
Sub SolFLamp3(Enabled)
	lf3a.state=Enabled
	lf3b.state=Enabled
	lf3c.state=Enabled
End Sub
Sub SolFLamp4(Enabled)
	lf4a.state=Enabled
	lf4b.state=Enabled
	lf4c.state=Enabled
	f4.state = Enabled
Primitive031.blenddisablelighting = 1
if lf4a.state = 0 then Primitive031.blenddisablelighting = 0
End Sub
Sub SolFLamp5(Enabled)
	lf5.state=Enabled
	lf5a.state=Enabled
	lf5b.state=Enabled
	lf5c.state=Enabled
lf5d.state=Enabled
lf5e.state=Enabled
	f5.state = Enabled
Primitive045.blenddisablelighting = 1
if lf5.state = 0 then Primitive045.blenddisablelighting = 0
Primitive037.blenddisablelighting = 1
if lf5.state = 0 then Primitive037.blenddisablelighting = 0
Primitive16.blenddisablelighting = 1
if lf5.state = 0 then Primitive16.blenddisablelighting = 0
	End Sub
Sub SolFLamp6(Enabled)
	lf6a.state=Enabled
	lf6b.state=Enabled
	lf6c.state=Enabled	
Primitive039.blenddisablelighting = 3
if lf6a.state = 0 then Primitive039.blenddisablelighting = 0
End Sub
Sub SolFLamp7(Enabled)
	lf7.state= Enabled
	lf7a.state=Enabled
	f7.state = Enabled
Primitive034.blenddisablelighting = 1
if lf7.state = 0 then Primitive034.blenddisablelighting = 0
End Sub
Sub SolFLamp8(Enabled)
	lf8a.state= Enabled
	lf8b.state= Enabled
Primitive044.blenddisablelighting = 2
if lf8a.state = 0 then Primitive044.blenddisablelighting = 0
End Sub



' ===============================================================================================
' some general methods
' ===============================================================================================

Function BallSpeed()
	On Error Resume Next
	BallSpeed = SQR(ActiveBall.VelX ^ 2 + ActiveBall.VelY ^ 2)
End Function

Function Sign(number)
	If number < 0 Then
		Sign = -1
	ElseIf number > 0 Then
		Sign = 1
	Else
		Sign = 0
	End If
End Function

Function PlaySoundEffect(minspeed, lowsound, lowspeed, sound, highspeed, highsound, velX, velY)
	Dim currentspeed
	currentspeed = BallSpeed
	If currentspeed >= minspeed Then
		If (velX = 0 Or Sign(ActiveBall.VelX) = Sign(velX)) And (velY = 0 Or Sign(ActiveBall.VelY) = Sign(velY)) Then
			If highspeed > 0 And currentspeed > highspeed Then
				PlaySound highsound
			ElseIf currentspeed < lowspeed Then
				PlaySound lowsound
			Else
				PlaySound sound
			End If
		End If
	End If
End Function

Function IIF(ifcond, iftrue, iffalse)
	If ifcond Then
		IIF = iftrue
	Else
		IIF = iffalse
	End If
End Function



' ===============================================================================================

Set LampCallback = GetRef("Lamps") ' individual lampcallbacks instead of using vpmMapLights (no reason, just because)
Sub Lamps
	pUpdateLED ()
		L1.State = Controller.Lamp(1)
		L2.State = Controller.Lamp(2)
		L3.State = Controller.Lamp(3)
		L4.State = Controller.Lamp(4)
		L5.State = Controller.Lamp(5)
		L6.State = Controller.Lamp(6)
		L7.State = Controller.Lamp(7)
		L8.State = Controller.Lamp(8)
		L9.State = Controller.Lamp(9)
		L10.State = Controller.Lamp(10)
		L11.State = Controller.Lamp(11)
		L12.State = Controller.Lamp(12)
		L13.State = Controller.Lamp(13)
		'L14.State = Controller.Lamp(14)
		L15.State = Controller.Lamp(15)
		L16.State = Controller.Lamp(16)
		L17.State = Controller.Lamp(17)
		L18.State = Controller.Lamp(18)
		L19.State = Controller.Lamp(19)
		L20.State = Controller.Lamp(20)
		L21.State = Controller.Lamp(21)
		L22.State = Controller.Lamp(22)
		L23.State = Controller.Lamp(23)
		L24.State = Controller.Lamp(24)
		L25.State = Controller.Lamp(25)
		L26.State = Controller.Lamp(26)
		L27.State = Controller.Lamp(27)
		L28.State = Controller.Lamp(28)
		L29.State = Controller.Lamp(29)
		L30.State = Controller.Lamp(30)
		L31.State = Controller.Lamp(31)
		L32.State = Controller.Lamp(32)
		L33.State = Controller.Lamp(33)
		L34.State = Controller.Lamp(34)
		L35.State = Controller.Lamp(35)
		L36.State = Controller.Lamp(36)
		L37.State = Controller.Lamp(37)
		L38.State = Controller.Lamp(38)
		L39.State = Controller.Lamp(39)
		L40.State = Controller.Lamp(40)
		L41.State = Controller.Lamp(41)
		L42.State = Controller.Lamp(42)
		L43.State = Controller.Lamp(43)
		L44.State = Controller.Lamp(44)
		L45.State = Controller.Lamp(45)
		L46.State = Controller.Lamp(46)
		L47.State = Controller.Lamp(47)
		L48.State = Controller.Lamp(48)
		L49.State = Controller.Lamp(49)
		L50.State = Controller.Lamp(50)
		L51.State = Controller.Lamp(51)
		L52.State = Controller.Lamp(52)
		L53.State = Controller.Lamp(53)
		L54.State = Controller.Lamp(54)
L54a.State = Controller.Lamp(54)
		L55.State = Controller.Lamp(55)
L55a.State = Controller.Lamp(55)
		L56.State = Controller.Lamp(56)
L56a.State = Controller.Lamp(56)
		L60.State = Controller.Lamp(60)
		L61.State = Controller.Lamp(61)
		L62.State = Controller.Lamp(62)
		l63.State = Controller.Lamp(63)
		f63.visible = Controller.lamp(63)
Primitive047.blenddisablelighting = 1 
if l63.state = 0 then Primitive047.blenddisablelighting = 0.1
		L64.State = Controller.Lamp(64)
	If varhidden=0 Then
		f65.visible = Controller.lamp(65)
		f66.visible = Controller.Lamp(66)
		f67.visible = Controller.Lamp(67)
		f68.visible = Controller.Lamp(68)
		f69.visible = Controller.Lamp(69)
		f70.visible = Controller.Lamp(70)
		f71.visible = Controller.Lamp(71)
		f72.visible = Controller.Lamp(72)
	End If
	sw41Prim.Rotx = sw41.CurrentAngle-90


'        Controller.Lamp(110)   'Controls GI
'		 Controller.Lamp(111) 'Prim and Ball GI Updates
End Sub

 


' ===============================================================================================
' LED's display
' ===============================================================================================

Dim Digits(0), digitState, nummb,mb0, mb1, mb2, mb3, mb4, mb5, mb6, mb7, mb8, mb9, mbl

Digits(0) 	= Array(led0,led1,led2,led3,led4,led5,led6)
digitState	= Array(0,0,0,0,0,0,0)

Sub pUpdateLED()
	Dim chgLED, ii, iii, num, chg, stat, digit
	chgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(chgLED) Then
		For ii = 0 To UBound(chgLED)
			num  = chgLED(ii, 0)
			chg  = chgLED(ii, 1)
			stat = chgLED(ii, 2)
			iii  = 0
			For Each digit In Digits(num)
				If Num = 0 Then
					If (chg And 1) Then digit.State = (stat And 1)
					digitState(iii) = digit.State
					chg  = chg \ 2
					stat = stat \ 2
				End If
				iii = iii + 1
			Next
		Next
	End If
	nummb=led0.state+led1.state*2+led2.state*2+led3.state*3+led4.state*4+led5.state+led6.state-3
	mbl=1
	Select Case nummb
        Case 1:mbf.imageA= "pf1 apollo13":mbf.imageB= "pf1 apollo13"
        Case 8:mbf.imageA= "pf2 apollo13":mbf.imageB= "pf2 apollo13"
        Case 6:mbf.imageA= "pf3 apollo13":mbf.imageB= "pf3 apollo13"
        Case 3:mbf.imageA= "pf4 apollo13":mbf.imageB= "pf4 apollo13"
        Case 5:mbf.imageA= "pf5 apollo13":mbf.imageB= "pf5 apollo13"
        Case 9:mbf.imageA= "pf6 apollo13":mbf.imageB= "pf6 apollo13"
        Case 2:mbf.imageA= "pf7 apollo13":mbf.imageB= "pf7 apollo13"
        Case 11:mbf.imageA= "pf8 apollo13":mbf.imageB= "pf8 apollo13"
        Case 7:mbf.imageA= "pf9 apollo13":mbf.imageB= "pf9 apollo13"
        Case 10:mbf.imageA= "pf0 apollo13":mbf.imageB= "pf0 apollo13"
		Case Else  : mbl=0:mbf.imageA="pf00 apollo13":mbf.imageB="pf00 apollo13"
	End Select
	mb.state=mbl
End Sub






'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep,LStep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw(62)
    PlaySoundAtVol SoundFX("right_slingshot",DOFContactors), sling1, 1
    DOF 121, DOFPulse
	RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub


Sub leftSlingShot_Slingshot
	vpmTimer.PulseSw(61)
    PlaySoundAtVol SoundFX("left_slingshot",DOFContactors), sling2, 1
	DOF 122, DOFPulse
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    leftSlingShot.TimerEnabled = 1
End Sub

Sub leftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:leftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX, Rothbauerw, Thalamus and Herweh
' PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

' set position as table object and Vol + RndPitch manually

Sub PlaySoundAtVolPitch(sound, tableobj, Vol, RndPitch)
  PlaySound sound, 1, Vol, AudioPan(tableobj), RndPitch, 0, 0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Volume)
  PlaySound sound, 1, Volume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallAbsVol(sound, VolMult)
  PlaySound sound, 0, VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

' requires rampbump1 to 7 in Sound Manager

Sub RandomBump(voladj, freq)
  Dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
  PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' set position as bumperX and Vol manually. Allows rapid repetition/overlaying sound

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
  PlaySound sound, 0, ABS(BOT.velz)/17, AudioPan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
End Sub

' play a looping sound at a location with volume
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
  PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

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

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / 400)
End Function

Function VolMulti(ball,Multiplier) ' Calculates the Volume of the sound based on the ball speed
  VolMulti = Csng(BallVel(ball) ^2 / 150 ) * Multiplier
End Function

Function BallRollVol(ball) ' Calculates the Volume of the sound based on the ball speed
  BallRollVol = Csng(BallVel(ball) ^2 / (80000 - (79900 * Log(RollVol) / Log(100))))
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


'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 13 ' total number of balls
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
      If BallVel(BOT(b) ) > 1 Then
        rolling(b) = True
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/12, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/12, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          rolling(b) = False
        End If
      End If
 ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


Sub rollingmetal_Hit (idx)
'PlaySound SoundFX("Wire ramp",DOFContactors), 0, 1, 0.05, 0.05
PlaySound "Wire Ramp", 0, VolMulti(ActiveBall,VolMetal), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub rollingplastic_Hit (idx)
	PlaySound "fx_plasticrolling0", 0, VolMulti(ActiveBall,VolPlast), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, VolMulti(ActiveBall,VolPi), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub


Sub Switches_Hit (idx)
	PlaySound "switch_hit1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub



Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, VolMulti(ActiveBall,VolMetal), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, VolMulti(ActiveBall,VolMetal), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub


Sub Gates_Hit (idx)
	PlaySound "gate4", 0, VolMulti(ActiveBall,VolGates), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub



Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then
		PlaySound "fx_rubber2", 0, VolMulti(ActiveBall,VolRH), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub



Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, VolMulti(ActiveBall,VolRH), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, VolMulti(ActiveBall,VolRH), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, VolMulti(ActiveBall,VolRH), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub RandomSoundbumper()
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtBallVol SoundFX("fx_Bumper",DOFContactors), 1
		Case 2 : PlaySoundAtBallVol SoundFX("fx_Bumper1",DOFContactors), 1
		Case 3 : PlaySoundAtBallVol SoundFX("fx_Bumper2",DOFContactors), 1
		Case 4 : PlaySoundAtBallVol SoundFX("fx_bumper3",DOFContactors), 1
		Case 5 : PlaySoundAtBallVol SoundFX("fx_bumper4",DOFContactors), 1
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub
Sub LeftFlipper1_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub
Sub RightFlipper1_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, VolMulti(ActiveBall,VolFlip), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, VolMulti(ActiveBall,VolFlip), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, VolMulti(ActiveBall,VolFlip), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub




