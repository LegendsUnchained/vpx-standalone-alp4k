Option Explicit
Randomize

Const ballsize = 35
Const ballmass = .75

Const fastballspeed = 17.5
Const changeuppercent = 0.6
Const magnetstrength = 10

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="pfevr_p3",UseSolenoids=2,UseLamps=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"
Const DoubleTapTime = 250		'Max. interval in ms for the Double Tap to register. Please set this to the minimal interval, You can live with
Const DoubleTap = 1	

LoadVPM "01530000","S7.VBS",3.1


'*********************************************************************
'Solenoid Call backs
'*********************************************************************

Dim Changeup

SolCallback(1)="SolChangeup"
SolCallback(2)="Bell"
SolCallback(4)="vpmNudge.SolGameOn"
SolCallback(5)="SolmMagnet"
SolCallback(6)="SolTrough"
SolCallback(7)="SolSwingBat" 

Sub SolSwingBat(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd
	vpmTimer.AddTimer 125, "PlaySound SoundFX(""fx_Flipperdown"",DOFContactors):LeftFlipper.RotateToStart'"
     End If
  End Sub

Sub SolChangeup(Enabled)
	If Enabled Then Changeup = 1:debug.print "changeup"
End Sub

Sub SolmMagnet(Enabled)
     'If Enabled Then Playsound "knocker"
End Sub

'*********************************************************************
'Solenoid Controlled toys
'*********************************************************************

Sub SolTrough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		'Move primtive
	Else
		'Move primtive
	End If
End Sub

Sub Bell(Enabled)
	If Enabled Then
		PlaySound "bell"
	Else
	End If
End Sub

'*********************************************************************
'Initiate Table
'*********************************************************************

Dim bsTrough, mMagnet

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
			.GameName = cGameName
			If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
			.SplashInfoLine = "Pennant Fever"&chr(13)&""
			.HandleMechanics=0
			.HandleKeyboard=0
			.ShowDMDOnly=1
			.ShowFrame=0
			.ShowTitle=0
			.hidden = 1
			On Error Resume Next
			.Run GetPlayerHWnd
			If Err Then MsgBox Err.Description
			On Error Goto 0
		End With
	On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1
	vpmNudge.TiltSwitch=swTilt
	vpmNudge.Sensitivity=5
	Wall13.visible = 1
	Wall4.visible = 0
	Wall18.visible = 1
	Wall21.visible = 0
	Wall18.sidevisible = 1
	Wall21.sidevisible = 0
	Set bsTrough=New cvpmBallStack
	With bsTrough
		.InitNoTrough BallRelease,9,180,fastballspeed 
		.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
		.KickForceVar = 2
	End With

	Set mMagnet = New cvpmMagnet
	With mMagnet
		.InitMagnet Magnet, magnetstrength
		.Solenoid = 5
		.CreateEvents "mMagnet"
	End With
End Sub 

'*********************************************************************
'Player Controls
'*********************************************************************
Dim change:change=1
Dim pitchcount: pitchcount = 0

Sub Table1_KeyDown(ByVal KeyCode)

	If KeyCode=LeftMagnaSave Then 
		If bsTrough.Balls Then 
			Controller.Switch(12)=1
		End If
	End If

	If KeyCode=RightFlipperKey Then 
		Controller.Switch(10)=1
	End If

	If keycode=AddCreditKey Then 
		'Controller.Switch(3)=1
		playsound "CoinIn"
		'vpmTimer.pulseSW (swCoin1): end if	
	End If
	If keycode = LeftFlipperKey Then
		pitchcount = pitchcount + 1
		DoubleTapLeftTimer.enabled = True
	End If


	If KeyCode=LeftFlipperKey Then
	   	If bsTrough.Balls Then Controller.Switch(11)=1
	End If

'*********************************************************************
'Prototype Plastic invert
'*********************************************************************

	If keycode = 23 Then
		If change=0 then 
			wall8.image = "Black_Plastics"
			wall7.image = "Black_Plastics"
			Primitive48.image = ""
			batflipper. image = "ash"
			Wall13.visible = 1
			Wall4.visible = 0
			Wall18.visible = 1
			Wall21.visible = 0
			Wall18.sidevisible = 1
			Wall21.sidevisible = 0
			Primitive30.image =""
			change=1
		ElseIf change=1 Then 
			wall8.image = "Green_Plastics"
			wall7.image = "Green_Plastics"
			Primitive48.image = "Green"
			batflipper. image = "blackwood"
			Primitive30.image ="Green"
			Wall13.visible = 0
			Wall4.visible = 1
			Wall18.visible = 0
			Wall21.visible = 1
			Wall18.sidevisible = 0
			Wall21.sidevisible = 1
			change=0 
		End If
	End If
    
    If vpmKeyDown(KeyCode) Then Exit Sub 
End Sub  

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyCode=RightFlipperKey Then Controller.Switch(10)=0
	If KeyCode=LeftMagnaSave Then Controller.Switch(12)=0
	'If KeyCode=AddCreditKey Then Controller.Switch(3)=0
	If vpmKeyUp(KeyCode) Then Exit Sub
End Sub  

DoubleTapLeftTimer.interval = DoubleTapTime

Sub DoubleTapLeftTimer_Timer
	If Pitchcount = 1 Then
		vpmTimer.PulseSw 11
		'debug.print "Fastball"
	Else
		vpmTimer.PulseSw 13
		'debug.print "Changeup"
	End If
	Pitchcount = 0
	DoubleTapLeftTimer.enabled = False
End Sub


'*********************************************************************
'Drains
'*********************************************************************

Sub Drain_Hit:bsTrough.addball me : playsound "Drain" : End Sub
Sub Drain2_Hit:bsTrough.addball me : End Sub
Sub Drain1_Hit:move_to_drain(Activeball):playsound "Drain5" : End Sub
Sub Drain8_Hit:move_to_drain(Activeball): playsound "drain" : End Sub

Sub Move_to_Drain(ball)
	ball.velx = 0
	ball.vely = 0
	ball.velz = 0
	ball.x = 767
	ball.y = 500
	ball.z = -75
End Sub


'*********************************************************************
'Targets moving
'*********************************************************************

sub TargetTimer_Timer()
	Primitive22.Rotx = Gate1.currentangle
	Primitive21.Rotx = Gate2.currentangle
	Primitive18.Rotx = Gate3.currentangle
	Primitive19.Rotx = Gate4.currentangle
	Primitive20.Rotx = Gate5.currentangle
	Primitive23.Rotx = Gate6.currentangle
	Primitive24.Rotx = Gate7.currentangle
End Sub

'*********************************************************************
'Targets
'*********************************************************************

Sub Gate1_hit:vpmTimer.PulseSw 17:PlaySoundAtBall SoundFX("fx_target",DOFTargets):End Sub
Sub Gate2_hit:vpmTimer.PulseSw 18:PlaySoundAtBall SoundFX("fx_target",DOFTargets):End Sub
Sub Gate3_hit:vpmTimer.PulseSw 19:PlaySoundAtBall SoundFX("fx_target",DOFTargets):End Sub
Sub Gate4_hit:vpmTimer.PulseSw 20:PlaySoundAtBall SoundFX("fx_target",DOFTargets):End Sub
Sub Gate5_hit:vpmTimer.PulseSw 21:PlaySoundAtBall SoundFX("fx_target",DOFTargets):End Sub
Sub Gate6_hit:vpmTimer.PulseSw 22:PlaySoundAtBall SoundFX("fx_target",DOFTargets):End Sub
Sub Gate7_hit:vpmTimer.PulseSw 23:PlaySoundAtBall SoundFX("fx_target",DOFTargets):End Sub
Sub trigger1_hit
	vpmTimer.PulseSw 24
	PlaySoundAtBall SoundFX("",DOFTargets):
	If Activeball.vely<-10 then 
		Activeball.vely=2
		PlaySoundAtBall "Metalhit_medium"
	end if
End Sub'

Sub trigger2_hit
	vpmTimer.PulseSw 25
	PlaySoundAtBall SoundFX("",DOFTargets)
	If Activeball.vely<-10 then 
		Activeball.vely=2
		PlaySoundAtBall "Metalhit_medium"
	end if
End Sub

Sub trigger3_hit
	vpmTimer.PulseSw 26
	PlaySoundAtBall SoundFX("",DOFTargets)
	If Activeball.vely<-10 then 
		Activeball.vely=2
		PlaySoundAtBall "Metalhit_medium"
	end if
End Sub
Sub trigger7_hit
	PlaysoundAtBall "Ramphit"
	'vpmTimer.AddTimer 100, "StopSound ""Ramp""'"
End Sub

'*********************************************************************
'Lights
'*********************************************************************

set lights(5)=light5
set lights(6)=light6
set lights(7)=light7
set lights(8)=light8
set lights(9)=light9
set lights(10)=light10
set lights(11)=light11
set lights(12)=light12
set lights(13)=light13
set lights(14)=light14
set lights(15)=light15
set lights(16)=light16
set lights(17)=light17
set lights(18)=light18
set lights(19)=light19
set lights(20)=light20
set lights(21)=light21
set lights(22)=light22
set lights(23)=light23
set lights(24)=light24
set lights(25)=light25
set lights(26)=light26
set lights(27)=light27
set lights(28)=light28
set lights(29)=light29
set lights(30)=light30
set lights(31)=light31
set lights(32)=light32
set lights(33)=light33
set lights(34)=light34
set lights(35)=light35
set lights(36)=light36
set lights(37)=light37
set lights(38)=light38
set lights(39)=light39
set lights(40)=light40
set lights(51)=light51
set lights(52)=light52
set lights(53)=Light53


'*********************************************************************
'Pitcher Cover
'*********************************************************************


Sub Timer1_timer
	timer2.enabled=false
	If multiguard_prim.objrotx < 12 Then
		multiguard_prim.objrotx = multiguard_prim.objrotx +1
		Controller.Switch(29) = 0
	'Else
	ENd If
End Sub

Sub Timer2_timer
	If multiguard_prim.objrotx > 0 Then
		multiguard_prim.objrotx = multiguard_prim.objrotx -1	
	Else
		Controller.Switch(29) = 1
	End If
End Sub

Sub Trigger4_hit
	timer1.enabled=true
	vpmtimer.pulsesw 28
End sub

Sub Trigger5_hit
	'Playsound "Bell"

	If Changeup = 1 Then
		activeball.vely = activeball.vely * changeuppercent
		activeball.velx = activeball.velx + (Rnd*0.75*changeuppercent) + 0.25*changeuppercent
		Changeup = 0
	Else
		activeball.velx = activeball.velx + (Rnd*0.75) + 0.25
	End If 

	timer1.enabled=false
	timer2.enabled=true
end sub

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
    Vol = Csng(BallVel(ball) ^2 / 1500)
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
	StopSound("fx_Rolling_Metal" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
	For b = 0 to UBound(BOT)
		If BallVel(BOT(b) ) > 1 AND BOT(b).z < 20 Then
			rolling(b) = True
			PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b))*10, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		Elseif BOT(b).z > 140 and BOT(b).z < 200 and BOT(b).y < 278 Then
			rolling(b) = True
			PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b))*10, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		Else
			If rolling(b) = True Then
				StopSound("fx_ballrolling" & b)
				StopSound("fx_Rolling_Metal" & b)
				rolling(b) = False
			End If
		End If	
		If BOT(b).VelZ < 0 and (BOT(b).z < 55 and BOT(b).z > 27) or (BOT(b).z < 300 and BOT(b).z > 200 and BOT(b).y < 278) Then 'height adjust for ball drop sounds
			PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If
	Next

'***Ball Drop Sounds***


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

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	batflipper.objrotz = LeftFlipper.currentangle
	Primitive25.Roty = LeftFlipper.currentangle
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

	BallShadow(b).X = BOT(b).X
         If BOT(b).Z > 17 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
	If BOT(b).z > 140 and BOT(b).y < 275 Then
		BallShadow(b).rotx = 10
		BallShadow(b).material = "BallShadow1"
		BallShadow(b).z = BOT(b).z  - ballsize/2 + 3
		ballShadow(b).Y = BOT(b).Y + 5
	Else
		BallShadow(b).rotx = 0
		BallShadow(b).material = "BallShadow"
		BallShadow(b).z = 1
		ballShadow(b).Y = BOT(b).Y + 10
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
	PlaySound "fx_target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
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

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub Glass_hit:PlaySoundAtBall "fx_glass":End Sub
Sub Ramp2_hit:PlaySoundAtBall "fx_glass":End Sub
