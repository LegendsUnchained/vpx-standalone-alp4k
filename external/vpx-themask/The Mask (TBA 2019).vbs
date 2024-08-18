'############################################################################################
'############################################################################################
'#######                                                                             ########
'#######          Lortium                                                            ########
'#######          (Juegos Populares 1987)                                            ########
'#######                                                                             ########
'############################################################################################
'############################################################################################
'
' Version 1.0
'
' - Playfield provided by Akiles50000


Option Explicit

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0
Randomize


'-----------------------------------
'------  Global Configurations ------
'-----------------------------------

Const FreePlay = True
Const DimGI=5					'set GI lights intensity (base value is 10)
Const VolumeMultiplier = 3 		'adjusts table sound volume

Const cGameName = "Lortium"
Const UseSolenoids=1,UseLamps=1,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="",SFlipperOff="",SCoin="coin3"
LoadVPM "01560000","juegos.vbs",3.2


'-------------------------------
'------  Keybord Handler  ------
'-------------------------------
Sub Lortium_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySound "plungerpull",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	if Flipperactive then
		If keycode = LeftFlipperKey Then
			LeftFlipper.RotateToEnd
			PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
		End If

		If keycode = RightFlipperKey Then
			RightFlipper.RotateToEnd
			URightFlipper.RotateToEnd
			PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
		End If
	end If

	if FreePlay and keycode = Startgamekey then
		vpmtimer.pulsesw 27 
	end if

	If vpmKeyDown(KeyCode) Then Exit Sub
End Sub

Sub Lortium_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySound "plunger",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	if Flipperactive then
		If keycode = LeftFlipperKey Then
			LeftFlipper.RotateToStart
			PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
		End If

		If keycode = RightFlipperKey Then
			RightFlipper.RotateToStart
			URightFlipper.RotateToStart
			PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
		End If
	end if 

	If vpmKeyUp(KeyCode) Then Exit Sub
End Sub


'-----------------------------------
'------  Solenoid Assignment  ------
'-----------------------------------
'SolCallback(1)="Sol1"							'Coin
'SolCallback(2)="Sol2"							'Coin
'SolCallback(3)="Sol3"							'Coin
'SolCallback(4)="Sol4"							'always on
'SolCallback(5)="Sol5"							'always on
'SolCallback(6)="Sol6"							'always on
SolCallback(7)="Sol7"							'SolGameOn
SolCallback(8)="Sol8"							'"O" insert light of LORTIUM
SolCallback(10)="Sol10"							'Right Drop Target Bank
SolCallback(16)="bsTrough.SolOut"				
SolCallback(13)="Sol13"							'Left Drop Target Bank
SolCallback(14)="Sol14"							'Gates
SolCallback(15)="vpmSolSound ""knocker"","		

'SolCallback(sLLFlipper)="vpmSolFlipper LeftFlipper,Nothing,"
'SolCallback(sLRFlipper)="vpmSolFlipper RightFlipper,URightFlipper,"


' Sol7 is not working all the times, so GameOn functionality has been moved to Flipper timer
Sub Sol7(enabled)
'	VpmNudge.SolGameOn enabled
'	Flipperactive = enabled
'
'	if not enabled Then
'		LeftFlipper.rotatetostart
'		URightFlipper.rotatetostart
'		RightFlipper.rotatetostart
'	end If
'
'	for each obj in GI
'		obj.state = abs(Enabled)
'	next
'	for each obj in GIBulbs
'		obj.state = abs(Enabled)
'	next
'
'	if enabled Then
'		P_Bumpercap1.image = "JP_Bumper_lit_red"
'		P_Bumpercap2.image = "JP_Bumper_lit_red"
'	Else
'		P_Bumpercap1.image = "JP_Bumper_red"
'		P_Bumpercap2.image = "JP_Bumper_red"
'	end if
end sub

Sub Sol8(enabled)
	OLamp.state = abs(Enabled)
End Sub

Sub Sol13(enabled)
	if enabled then
		LeftDropTargetBank.DropSol_On
	end if
End Sub

Sub Sol10(enabled)
	if enabled then
		RightDropTargetBank.DropSol_On
	end if
End Sub

Sub Sol14(enabled)
	if enabled Then
		RightGatex.collidable = False
		RightGatex.visible = False
		RightGate.visible = True
		rightGate.Open = True 

		LeftGatex.collidable = False
		LeftGatex.visible = False
		LeftGate.visible = True
		LeftGate.Open = True 

		P_RGate.rotx = 10
		P_LGate.rotx = 10
	Else
		RightGatex.collidable = True
		RightGatex.visible = true
		RightGate.visible = False
		rightGate.Open = False 

		LeftGatex.collidable = True
		LeftGatex.visible = True
		LeftGate.visible = false
		LeftGate.Open = false 

		P_RGate.rotx = 20
		P_LGate.rotx = 20
	end If
End Sub

'--------------------------
'------  Table Init  ------
'--------------------------
Dim DesktopMode: DesktopMode = Lortium.ShowDT
If DesktopMode = True Then 'Show Desktop components
	Ramp17.visible=1
	Ramp16.visible=1
	Ramp15.visible=1
	SideWood.visible=1
Else
	Ramp17.visible=0
	Ramp16.visible=0
	Ramp15.visible=0
	SideWood.visible=0
End if

for each obj in GI
	obj.intensity = DimGI
next

Dim bsTrough,obj,LeftDropTargetBank,RightDropTargetBank,cUpperCaptive,cLowerCaptive,Flipperactive
Sub Lortium_init
	vpminit me
	
    Controller.GameName=cGameName
    Controller.SplashInfoLine="The Mask (TBA 2019)" & vbNewLine & "created by IVANTBA"
    Controller.HandleKeyboard=False
    Controller.ShowTitle=0
    Controller.ShowFrame=0
    Controller.ShowDMDOnly=1
	'Controller.Hidden = 1			'enable to hide DMD if You use a B2S backglass

'   DMD position for 3 Monitor Setup
'   Controller.Games(cGameName).Settings.Value("dmd_pos_x")=3850		'set this to 0 if You cannot find the DMD
'   Controller.Games(cGameName).Settings.Value("dmd_pos_y")=300		'set this to 0 if You cannot find the DMD
'   Controller.Games(cGameName).Settings.Value("dmd_width")=505
'   Controller.Games(cGameName).Settings.Value("dmd_height")=155
   Controller.Games(cGameName).Settings.Value("sound")=0				
'	Controller.Games(cGameName).Settings.Value("ddraw") = 0             'set to 0 if You have problems with DMD showing or table stutter
		   
    Controller.HandleMechanics=0
    Controller.Run 
    If Err Then MsgBox Err.Description
    On Error Goto 0

PlaySound "Intro",-0  
PlaySound "Music",-1  

    PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled = true

    vpmNudge.TiltSwitch=31
    vpmNudge.Sensitivity=5
	vpmNudge.TiltObj = Array(RightSlingshot,URightFlipper,RightFlipper,LeftFlipper) 

    vpmMapLights AllLights

    Set bsTrough=New cvpmBallStack
        bsTrough.InitSw 0,25,0,0,0,0,0,0 		'0.1 = Switch 0
        bsTrough.InitKick BallRelease,90,5
        bsTrough.InitExitSnd SoundFX("BallRelease",DOFContactors),SoundFX("Solenoid",DOFContactors)
        bsTrough.Balls=1

	set LeftDropTargetBank = new cvpmDropTarget
		LeftDropTargetBank.InitDrop Array(LeftTarget1,LeftTarget2,LeftTarget3), Array(8,7,6)
		LeftDropTargetBank.InitSnd SoundFX("Targetdrop1",DOFContactors),SoundFX("TargetBankreset1",DOFContactors)
		LeftDropTargetBank.CreateEvents "LeftDropTargetBank"

	set RightDropTargetBank = new cvpmDropTarget
		RightDropTargetBank.InitDrop Array(RightTarget1,RightTarget2,RightTarget3), Array(5,4,3)
		RightDropTargetBank.InitSnd SoundFX("Targetdrop1",DOFContactors),SoundFX("TargetBankreset1",DOFContactors)
		RightDropTargetBank.CreateEvents "RightDropTargetBank"

	Set cUpperCaptive=New cvpmCaptiveBall
		cUpperCaptive.InitCaptive UpperCaptiveTrigger,UpperCaptiveWall,UpperCaptiveKicker,-28
		cUpperCaptive.Start
		cUpperCaptive.ForceTrans = 0.75
		cUpperCaptive.MinForce = 2		'3.5
		cUpperCaptive.CreateEvents "cUpperCaptive"

	Set cLowerCaptive=New cvpmCaptiveBall
		cLowerCaptive.InitCaptive LowerCaptiveTrigger,LowerCaptiveWall,LowerCaptiveKicker,12
		cLowerCaptive.Start
		cLowerCaptive.ForceTrans = 0.75
		cLowerCaptive.MinForce = 2		'3.5
		cLowerCaptive.CreateEvents "cLowerCaptive"
End Sub

Sub Lortium_Exit()
	Controller.Pause = False
	Controller.Stop
End Sub

'------------------------------
'------  Trough Handler  ------
'------------------------------
Sub Drain_Hit()
	PlaySound "drain",0,1,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain)
	bsTrough.AddBall Me
End Sub



'------------------------------
'------  Switch Handler  ------
'------------------------------

Sub SW12_Hit:vpmtimer.pulsesw(16):PlaySound SoundFX("fx_sensor7",DOFContactors),0,1,0,0.05:End Sub
Sub SW11_Hit:vpmtimer.pulsesw(15):PlaySound SoundFX("fx_sensor8",DOFContactors),0,1,0.2,0.05:End Sub
Sub SW10_Hit:vpmtimer.pulsesw(16):PlaySound SoundFX("fx_sensor9",DOFContactors),0,1,0,0.05:End Sub
Sub SW9_Hit:vpmtimer.pulsesw(15):PlaySound SoundFX("fx_sensor10",DOFContactors),0,1,0.2,0.05:End Sub


'Sub SW12_Hit:Controller.Switch(12) = 1:End Sub
'Sub SW12_unhit:Controller.Switch(12) = 0:End Sub
'Sub SW11_Hit:Controller.Switch(11) = 1:End Sub
'Sub SW11_Unhit:Controller.Switch(11) = 0:End Sub
'Sub SW10_Hit:Controller.Switch(10) = 1:End Sub
'Sub SW10_Unhit:Controller.Switch(10) = 0:End Sub
'Sub SW9_Hit:Controller.Switch(9) = 1:End Sub
'Sub SW9_Unhit:Controller.Switch(9) = 0:End Sub

Sub Bumper1_Hit:vpmtimer.pulsesw(16):PlaySound SoundFX("Jet2",DOFContactors),0,1,0,0.05:End Sub
Sub Bumper2_Hit:vpmtimer.pulsesw(15):PlaySound SoundFX("Jet1",DOFContactors),0,1,0.2,0.05:End Sub

Sub Spinner1_Spin:vpmtimer.pulsesw(17):PlaySound SoundFX("fx_spinner",DOFContactors),0,1,0,0.05:End Sub
Sub Spinner2_Spin:vpmtimer.pulsesw(17):PlaySound SoundFX("fx_spinner",DOFContactors),0,1,0,0.05:End Sub

Sub TopRTarget1_Hit:vpmtimer.pulsesw(20):End Sub
Sub TopRTarget2_Hit:vpmtimer.pulsesw(21):End Sub

Sub RightRTarget1_Hit:vpmtimer.pulsesw(19):End Sub
Sub RightRTarget2_Hit:vpmtimer.pulsesw(19):End Sub

Sub UpperCaptiveTarget_Hit:vpmtimer.pulsesw(13):End Sub
Sub LowerCaptiveTarget_Hit:vpmtimer.pulsesw(22):End Sub

Sub RampTrigger_Hit:vpmtimer.pulsesw(24):End Sub

Sub Rubber1_Hit:vpmtimer.pulsesw(18):End Sub
Sub Rubber2_Hit:vpmtimer.pulsesw(18):End Sub
Sub Rubber3_Hit:vpmtimer.pulsesw(18):End Sub
Sub Rubber4_Hit:vpmtimer.pulsesw(18):End Sub
Sub Rubber5_Hit:vpmtimer.pulsesw(18):End Sub
Sub Rubber6_Hit:vpmtimer.pulsesw(18):End Sub
Sub Rubber7_Hit:vpmtimer.pulsesw(18):End Sub


Sub LeftOutlane_Hit:vpmtimer.pulsesw(16):PlaySound SoundFX("fx_sensor2",DOFContactors),0,1,0,0.05:End Sub
Sub RightOutlane_Hit:vpmtimer.pulsesw(15):PlaySound SoundFX("fx_sensor",DOFContactors),0,1,0.2,0.05:End Sub
Sub RightInlane_Hit:vpmtimer.pulsesw(16):PlaySound SoundFX("fx_sensor4",DOFContactors),0,1,0,0.05:End Sub
Sub LeftInlane_Hit:vpmtimer.pulsesw(15):PlaySound SoundFX("fx_sensor3",DOFContactors),0,1,0.2,0.05:End Sub
Sub LeftInlane1_Hit:vpmtimer.pulsesw(15):PlaySound SoundFX("fx_sensor5",DOFContactors),0,1,0.2,0.05:End Sub
Sub LeftCorridor_Hit:vpmtimer.pulsesw(15):PlaySound SoundFX("fx_sensor6",DOFContactors),0,1,0.2,0.05:End Sub


'Sub LeftOutlane_Hit:Controller.Switch(23) = 1:End Sub
'Sub LeftOutlane_Unhit:Controller.Switch(23) = 0:End Sub
'Sub RightOutlane_Hit:Controller.Switch(23) = 1:End Sub
'Sub RightOutlane_Unhit:Controller.Switch(23) = 0:End Sub

'Sub RightInlane_Hit:vpmtimer.pulsesw(2):End Sub
'Sub LeftInlane_Hit:vpmtimer.pulsesw(2):End Sub
'Sub LeftInlane1_Hit:vpmtimer.pulsesw(1):End Sub

'Sub LeftCorridor_Hit:vpmtimer.pulsesw(33):End Sub


'--------------------------------
'------  Helper Functions  ------
'--------------------------------
'Launch
Sub LaunchTrigger_Hit
	if activeball.vely < 6 Then
		Playsound "Launch",0,0.6,0.8
	end if	
End Sub

'Diverter
if Rnd < 0.5 Then 
	DiverterRight.isdropped = True
	DiverterLeft.isdropped = False
	P_Diverter.RotZ = 27	'27=LeftOpen, -36=RightOpen
Else
	DiverterRight.isdropped = False
	DiverterLeft.isdropped = True
	P_Diverter.RotZ = -36	'27=LeftOpen, -36=RightOpen
end If


Dim DivDir
Sub DiverterTrigger_Hit
	Activeball.Vely = Activeball.Vely * 0.25
	if DiverterLeft.isdropped then
		DiverterRight.isdropped = True
		DiverterLeft.isdropped = False		
		DivDir = 1
	else
		DiverterRight.isdropped = False
		DiverterLeft.isdropped = True		
		DivDir = -1
	end if
	DiverterTrigger.timerenabled = True	
End Sub

Sub DiverterTrigger_Timer
	P_Diverter.RotZ = P_Diverter.RotZ + DivDir
	if P_Diverter.RotZ >= 27 then
'		DOF 117, 2
		DiverterTrigger.timerenabled = False
		P_Diverter.RotZ = 27
		
	end if
	if P_Diverter.RotZ <= -36 then
'		DOF 117, 2
		DiverterTrigger.timerenabled = False
		P_Diverter.RotZ = -36		
	end if		
End Sub


'Metal Gates
Dim RightGateDir
Sub RightGateTrigger_Hit
	if not RightGateTrigger.Timerenabled Then
		RightGateDir = -1.2
		RightGateTrigger.Timerenabled = True
	End If
End Sub

Sub RightGateTrigger_Timer
	P_RightGate.objrotz = P_RightGate.objrotz + RightGateDir
	if P_RightGate.objrotz <= -14 Then
		RightGateDir = 1
	End If
	if P_RightGate.objrotz >= 0 Then
		P_RightGate.objrotz = 0
		RightGateTrigger.Timerenabled = False
	End If
End Sub

Dim LeftGateDir
Sub LeftGateTrigger_Hit
	if not LeftGateTrigger.Timerenabled Then
		LeftGateDir = 1.2
		LeftGateTrigger.Timerenabled = True
	End If
End Sub

Sub LeftGateTrigger_Timer
	P_LeftGate.objrotz = P_LeftGate.objrotz + LeftGateDir
	if P_LeftGate.objrotz >= 22 Then
		LeftGateDir = -1
	End If
	if P_LeftGate.objrotz <= 0 Then
		P_LeftGate.objrotz = 0
		LeftGateTrigger.Timerenabled = False
	End If
End Sub


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05
	vpmtimer.pulsesw(14)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
	RStep = 0	
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub



'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / Lortium.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / Lortium.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 300) * VolumeMultiplier
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
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************
Dim GameOnVar
sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperURSh.RotZ = URightFlipper.currentangle


	'use lamp 32 (GameOber) and 40 (Tilt) instead of Sol7
	GameOnVar = (not controller.Lamp(32)) and (not controller.Lamp(40))

	VpmNudge.SolGameOn GameOnVar
	Bumper1Wall.isdropped = GameOnVar
	Bumper2Wall.isdropped = GameOnVar
	Flipperactive = GameOnVar

	if not GameOnVar Then
		LeftFlipper.rotatetostart
		URightFlipper.rotatetostart
		RightFlipper.rotatetostart

		RightGatex.collidable = True
		RightGatex.visible = true
		RightGate.visible = False
		rightGate.Open = False 

		LeftGatex.collidable = True
		LeftGatex.visible = True
		LeftGate.visible = false
		LeftGate.Open = false 

		P_RGate.rotx = 20
		P_LGate.rotx = 20
	end If

	for each obj in GI
		obj.state = abs(GameOnVar)
	next
	for each obj in GIBulbs
		obj.state = abs(GameOnVar)
	next

	if GameOnVar Then
		P_Bumpercap1.image = "JP_Bumper_lit_red"
		P_Bumpercap2.image = "JP_Bumper_lit_red"
	Else
		P_Bumpercap1.image = "JP_Bumper_red"
		P_Bumpercap2.image = "JP_Bumper_red"
	end if

End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

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
        If BOT(b).X < Lortium.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Lortium.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Lortium.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
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


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, 0.5, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 2 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 3 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub URightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub


'-------------------------------
'------  DIP Switch Menu  ------
'-------------------------------

Sub editDips
	Dim vpmDips:Set vpmDips=New cvpmDips
	With vpmDips
		.AddForm 200,280,"Lortium DIP switches"
		.AddFrame 0, 0,270,"Balls per game",&H00000400,Array("3 balls",0,"5 balls",&H00000400)
		.AddChk   5,50,270,Array("Match feature",32768)
		.AddFrame 0,75,270,"Replay threshold (Extra ball/credit/credit)",&H00000300,Array("4.0M/4.7M/5.5M points",&H00000300,"3.0M/3.7M/4.5M points",&H00000100,"2.5M/3.2M/4.0M points",&H00000200,"2.0M/2.7M/3.2M points",0)
		.AddChk   5,155,270,Array("Enable replay extra ball",&H00000008)
		
		.AddLabel 0,180,280,20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
End Sub
Set vpmShowDips=GetRef("editDips")

Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub