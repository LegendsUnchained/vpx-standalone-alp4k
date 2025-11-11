Option Explicit

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

if Table1.showdt = false then ramp001.visible = 0:ramp003.visible = 0

LoadVPM "01530000","sega.vbs",3.1

'********************************************
'**     Game Specific Code Starts Here     **
'********************************************

Const UseSolenoids=2,UseSync=1
Const SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="fx_FlipperUp",SFlipperOff="fx_FlipperDown",SCoin="Coin3"

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyCode=KeyUpperLeft Then Controller.Switch(1)=1
	If KeyCode=KeyUpperRight Then Controller.Switch(8)=1
	If KeyCode=PlungerKey Then Controller.Switch(53)=1
	If vpmKeyDown(KeyCode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyCode=KeyUpperLeft Then Controller.Switch(1)=0
	If KeyCode=KeyUpperRight Then Controller.Switch(8)=0
	If KeyCode=PlungerKey Then Controller.Switch(53)=0
	If vpmKeyUp(KeyCode) Then Exit Sub
End Sub

'**************************************
'**     Bind Events To Solenoids     **
'**************************************

SolCallback(1)="SolTrough"
SolCallback(2)="vpmSolAutoPlunger Plunger,0,"
SolCallback(3)="bsTRVUK.SolOut"
SolCallback(4)="bsBRVUK.SolOut"
SolCallback(6)="bsCVUK.SolOut"
SolCallback(9)="vpmSolSound ""Jet3"","
SolCallback(10)="vpmSolSound ""Jet3"","
SolCallback(11)="vpmSolSound ""Jet3"","
SolCallback(12)="vpmSolSound ""Jet3"","
SolCallback(13)="vpmSolSound ""Jet3"","
SolCallback(14)="mDMag.MagnetOn="
SolCallback(sLLFlipper)="vpmSolFlipper LeftFlipper,Nothing,"
SolCallback(sLRFlipper)="vpmSolFlipper RightFlipper,Nothing,"
SolCallback(17)="SolSpinWheelsMotor"
SolCallback(18)="vpmSolSound ""Sling"","
SolCallback(19)="vpmSolSound ""Sling"","
SolCallback(20)="SolRobot"
'FLASHERS
SolCallback(25)="vpmFlasher Array(F1A,F1B,F1C,F1D),"'25'F1=Red*4
SolCallback(26)="vpmFlasher Array(F2A,F2B,F2C,F2D),"'26'F2=Yellow*4
SolCallback(27)="vpmFlasher Array(F3A,F3B,F3C,F3D),"'27'F3=Green*4
'28'F4=WARNing*4
'29'F5=warnING*4
'SolCallback(30)="vpmFlasher Array(Light6,Light7,Light8),"
'31'F7=Pops*2
'32'F8=Ramp*2

'*********************Flashers
Sub Sol25(Enabled)
	If Enabled Then
F1A.State=LightStateOn
F1B.State=LightStateOn
F1C.State=LightStateOn
F1D.State=LightStateOn
Else
F1A.State=LightStateOff
F1B.State=LightStateOff
F1C.State=LightStateOff
F1D.State=LightStateOff
End If
End Sub

Sub Sol26(Enabled)
	If Enabled Then
F2A.State=LightStateOn
F2B.State=LightStateOn
F2C.State=LightStateOn
F2D.State=LightStateOn
Else
F2A.State=LightStateOff
F2B.State=LightStateOff
F2C.State=LightStateOff
F2D.State=LightStateOff
End If
End Sub

Sub Sol27(Enabled)
	If Enabled Then
F3A.State=LightStateOn
F3B.State=LightStateOn
F3C.State=LightStateOn
F3D.State=LightStateOn
Else
F3A.State=LightStateOff
F3B.State=LightStateOff
F3C.State=LightStateOff
F3D.State=LightStateOff
End If
End Sub
'*****************************
    
Sub SolRobot(Enabled)
	If Enabled Then
		Robot1.TimerEnabled=1
		Robot1_Timer
	Else
		Robot1.TimerEnabled=0
	End If
End Sub

Sub Robot1_Timer
	If Robot2.IsDropped Then
		Robot1.IsDropped=1
		Robot2.IsDropped=0
	Else
		Robot2.IsDropped=1
		Robot1.IsDropped=0
	End If
End Sub

Sub SolTrough(Enabled)
	If Enabled Then
		If bsTrough.Balls Then
			vpmTimer.PulseSw 15
			bsTrough.ExitSol_On
		End If
	End If
End Sub

'********************************************
'**     Init The Table, Start VPinMAME     **
'********************************************
 
Dim bsTrough,bsCVUK,bsTRVUK,bsBRVUK,mTLMag,mTRMag,mDMag,X,Ttable


Sub Table1_Init
    Robot2.IsDropped=1
'*********************FlasherF
F1A.State=LightStateOff
F1B.State=LightStateOff
F1C.State=LightStateOff
F1D.State=LightStateOff
F2A.State=LightStateOff
F2B.State=LightStateOff
F2C.State=LightStateOff
F2D.State=LightStateOff
F3A.State=LightStateOff
F3B.State=LightStateOff
F3C.State=LightStateOff
F3D.State=LightStateOff
'*****************************

	Plunger.PullBack
    On Error Resume Next
	With Controller
	.GameName="lostspc"
	.SplashInfoLine=""
	.HandleKeyboard=0
	.ShowTitle=0
	.ShowDMDOnly=1
    .HandleMechanics=0
	.ShowFrame=0
		If Table1.ShowDT = false then			
			.Hidden = 0
		End If

		If Table1.ShowDT = true then		
			.Hidden = 0
		End If
	End With
	Controller.Run
	If Err Then MsgBox Err.Description
		On Error Goto 0

    PinMAMETimer.Interval=PinMAMEInterval:PinMAMETimer.Enabled=1
	vpmNudge.TiltSwitch=56:vpmNudge.Sensitivity=5:vpmNudge.TiltObj=Array(S36,S37,S38,S39,S40,LeftSlingshot,RightSlingshot)

	Set bsTrough=New cvpmBallStack
	bsTrough.InitSw 0,14,13,12,11,0,0,0
	bsTrough.InitKick BallRelease,95,5
	bsTrough.Balls=4
	bsTrough.InitExitSnd"BallRel","SolOn"

	Set bsCVUK=New cvpmBallStack
	bsCVUK.InitSw 0,46,0,0,0,0,0,0
	bsCVUK.InitKick CenterHole,160,8
	bsCVUK.InitExitSnd"fx_kicker","SolOn"

	Set bsTRVUK=New cvpmBallStack
	bsTRVUK.InitSw 0,47,0,0,0,0,0,0
	bsTRVUK.InitKick TRVExit,280,20
	bsTRVUK.InitExitSnd"fx_kicker","SolOn"

	Set bsBRVUK=New cvpmBallStack
	bsBRVUK.InitSw 0,48,0,0,0,0,0,0
	bsBRVUK.InitKick BRVExit,300,20
	bsBRVUK.InitExitSnd"fx_kicker","SolOn"

	Set mTLMag=New cvpmMagnet
	mTLMag.InitMagnet Magnet1,200
	mTLMag.Solenoid=5
	mTLMag.GrabCenter=1
	mTLMag.CreateEvents"mTLMag"

	Set mTRMag=New cvpmMagnet
	mTRMag.InitMagnet Magnet2,50
	mTRMag.Solenoid=7
	mTRMag.GrabCenter=1
	mTRMag.CreateEvents"mTRMag"

	Set mDMag=New cvpmMagnet
	mDMag.InitMagnet Magnet3,25
	mDMag.GrabCenter=1

    Set Ttable = New cvpmTurntable
	Ttable.InitTurntable TT, 50
	Ttable.SpinDown = 10
	Ttable.CreateEvents "Ttable"

	vpmMapLights AllLights
End Sub

set GICallback = GetRef("UpdateGI")
Sub UpdateGI(no, Enabled)
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 1:	Next
        PlaySound "fx_relay"
Table1.colorgradeimage = "ColorGradeLUT256x16_ConSat"
	Else For each xx in GI:xx.State = 0: Next
        PlaySound "fx_relay"
Table1.colorgradeimage = "ColorGrade_4"
	End If
End Sub


'********************  Spinning Discs Animation Timer ****************************
Dim SpinnerMotorOff, SpinnerStep, ss

Sub SolSpinWheelsMotor(enabled)
	If enabled Then
		Ttable.MotorOn = True
		SpinnerStep = 10
		SpinnerMotorOff = False
		SpinnerTimer.Interval = 10
		SpinnerTimer.enabled = True
	Else
		SpinnerMotorOff = True
		Ttable.MotorOn = False
	end If
End Sub

Sub SpinnerTimer_Timer()
	If Not(SpinnerMotorOff) Then
		spindisk.ObjRotZ  = ss
		ss = ss + SpinnerStep
	Else
		if SpinnerStep < 0 Then
			SpinnerTimer.enabled = False
		Else
		'slow the rate of spin by decreasing rotation step
			SpinnerStep = SpinnerStep - 0.05
			
			spindisk.ObjRotZ  = ss
			ss = ss + SpinnerStep
		End If
	End If
	if ss > 360 then ss = ss - 360
End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    vpmTimer.PulseSw 62:PlaySound SoundFX("rightslingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
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
    vpmTimer.PulseSw 59:PlaySound SoundFX("rightslingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
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



'SWITCHES
'--------------------------------------------------------------------------
																				'1=Left UK Button
																				'2=Coin4
																				'3=Coin6
																				'4=Coin2
																				'5=Coin3
																				'6=Coin1
																				'7=Coin5
																				'8=Right UK Button
																				'9=NOT USED
																				'10=NOT USED
Sub Drain_Hit:bsTrough.AddBall Me:Playsound "drain":End Sub										'11,12,13,14,15
Sub S16_Hit:Controller.Switch(16)=1:End Sub										'16
Sub S16_unHit:Controller.Switch(16)=0:End Sub
Sub S17_Hit:vpmTimer.PulseSw 17:End Sub											'17
Sub S18_Hit:vpmTimer.PulseSw 18:End Sub											'18
Sub S19_Hit:vpmTimer.PulseSw 19:End Sub											'19
Sub S20_Hit:Controller.Switch(20)=1:End Sub										'20
Sub S20_unHit:Controller.Switch(20)=0:End Sub
Sub S21_Hit:Controller.Switch(21)=1:End Sub										'21
Sub S21_unHit:Controller.Switch(21)=0:End Sub
																				'22=NOT USED
																				'23=NOT USED
																				'24=NOT USED
Sub S25_Hit:vpmTimer.PulseSw 25:End Sub											'25
Sub S26_Hit:vpmTimer.PulseSw 26:End Sub											'26
Sub S27_Hit:vpmTimer.PulseSw 27:End Sub											'27
																				'28=NOT USED
																				'29=NOT USED
Sub S30_Hit:Controller.Switch(30)=1:PlaySound "rollover":End Sub										'30
Sub S30_unHit:Controller.Switch(30)=0:End Sub
Sub S31_Hit:Controller.Switch(31)=1:PlaySound "rollover":End Sub										'31
Sub S31_unHit:Controller.Switch(31)=0:End Sub
Sub S32_Hit:Controller.Switch(32)=1:PlaySound "rollover":End Sub										'32
Sub S32_unHit:Controller.Switch(32)=0:End Sub
Sub S33_Hit:vpmTimer.PulseSw 33:End Sub											'33
Sub S34_Hit:vpmTimer.PulseSw 34:End Sub											'34
Sub S35_Hit:vpmTimer.PulseSw 35:End Sub											'35
Sub S36_Hit:vpmTimer.PulseSw 36:End Sub											'36
Sub S37_Hit:vpmTimer.PulseSw 37:End Sub											'37
Sub S38_Hit:vpmTimer.PulseSw 38:End Sub											'38
Sub S39_Hit:vpmTimer.PulseSw 39:End Sub											'39
Sub S40_Hit:vpmTimer.PulseSw 40:End Sub											'40
Sub PopExit_Hit:Me.DestroyBall:vpmTimer.PulseSwitch 41,100,"AddMystery":Playsound "fx_kicker_enter":End Sub	'41
																				'42=NOT USED
																				'43=NOT USED
																				'44=NOT USED
Sub RobotEnter_Hit:Me.DestroyBall:vpmTimer.PulseSwitch 45,100,"AddBRV":Playsound "fx_kicker_enter":Primitive006.z=-40:primitive007.z=-40:Light035.state=2:End Sub	'45=UTrough Robot
Sub AddBRV(swNo):bsBRVUK.AddBall 0:End Sub
Sub CenterHole_Hit:bsCVUK.AddBall Me:Playsound "fx_kicker_enter":End Sub										'46
Sub AddMystery(swNo):bsCVUK.AddBall 0:End Sub
Sub TRHole_Hit:bsTRVUK.AddBall Me:End Sub										'47
																				'48=Bottom Right VUK
Sub S49_Spin:vpmTimer.PulseSw 49:PlaySound "fx_spinner":End Sub										'49
Sub S50_Spin:vpmTimer.PulseSw 50:PlaySound "fx_spinner":End Sub										'50
Sub S51_Hit:Controller.Switch(51)=1:PlaySound "rollover":End Sub										'51
Sub S51_unHit:Controller.Switch(51)=0:End Sub
Sub S52_Hit:Controller.Switch(52)=1:PlaySound "rollover":End Sub										'52
Sub S52_unHit:Controller.Switch(52)=0:End Sub
																				'53=Launch Button
																				'54=Start Button
																				'55=Slam Tilt
																				'56=Plumb Bob Tilt
Sub S57_Hit:Controller.Switch(57)=1:PlaySound "rollover":End Sub										'57
Sub S57_unHit:Controller.Switch(57)=0:End Sub
Sub S58_Hit:Controller.Switch(58)=1:PlaySound "rollover":End Sub										'58
Sub S58_unHit:Controller.Switch(58)=0:End Sub
Sub S60_Hit:Controller.Switch(60)=1:PlaySound "rollover":End Sub										'60
Sub S60_unHit:Controller.Switch(60)=0:End Sub
Sub S61_Hit:Controller.Switch(61)=1:PlaySound "rollover":End Sub										'61
Sub S61_unHit:Controller.Switch(61)=0:End Sub

'SUPPORTING ROUTINES
'--------------------------------------------------------------------------
Sub Magnet3_Hit
	mDMag.AddBall ActiveBall
	mDMag.AttractBall ActiveBall
End Sub
Sub Magnet3_unHit
	mDMag.RemoveBall ActiveBall
End Sub

Sub TT_Hit
	mTurnTable.AddBall ActiveBall
	mTurnTable.AffectBall ActiveBall
End Sub
Sub TT_unHit
	mTurnTable.RemoveBall ActiveBall
End Sub

Sub UpdateTurnTable(aNewPos,aSpeed,aLastPos)
	If aLastPos>-1 And aLastPos<10 Then SpinCounter(ALastPos).IsDropped=1
	If aNewPos>-1 And aNewPos<10 Then SpinCounter(aNewPos).IsDropped=0
End Sub

Sub Trigger1_Hit:ActiveBall.VelY=.1:ActiveBall.VelX=0:End Sub
Sub Trigger2_Hit:ActiveBall.VelY=1:ActiveBall.VelX=0:End Sub
Sub Trigger3_Hit:ActiveBall.VelY=1:ActiveBall.VelX=0:Primitive006.z=0:primitive007.z=0:Light035.state=0:End Sub

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
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
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

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function


'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds)
'********************************************************************

Const tnob = 7 ' total number of balls
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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/2, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/3, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          rolling(b) = False
        End If
      End If
 ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 29 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    dim mball
    for each mball in mDMag.Balls
        if mball.ID = ball1.ID OR mball.ID = ball2.ID then exit sub
    next
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub BallShadow_Timer()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,Ballshadow7)
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
'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	lfs.RotZ = LeftFlipper.currentangle
	rfs.RotZ = RightFlipper.currentangle

    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle

    sw28p.RotY = sw28.CurrentAngle

End Sub



Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall)*8, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub Table1_Exit()
Controller.Stop
End Sub