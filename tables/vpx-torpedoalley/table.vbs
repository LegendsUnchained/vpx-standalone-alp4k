Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

Const BallSize = 50
if Table1.showDT = False then ramp15.visible = 0:ramp16.visible = 0
' Load the core.vbs for supporting subs and functions

LoadVPM "01500000","DE.VBS",3.10

Sub LoadVPM(VPMver, VBSfile, VBSver)
End Sub

Const cGameName="torp_e21",UseSolenoids=2,UseLamps=1,UseSync=0,UseGI=0
Const SSolenoidOn="solon",SSolenoidOff="soloff",SFlipperOn="fx_FlipperUp",SFlipperOff="fx_FlipperDown",SCoin="coin3"

Const sBallRelease=16	'ok
Const sSolBRelay=10'Not Used
Const sGI=11'Not Used
Const sLSKicker=25		'ok
Const sLeftKick=26		'ok
Const sUpKicker=27		'ok
Const sCenterKicker=28	'ok
Const s3BankReset=29	'ok
Const sKnocker=30		'ok
Const sOutHole=31		'ok
Const sSinkingShip=32

'SolCallback(3)="vpmFlasher Flasher3L,"
'SolCallback(8)="vpmFlasher Flash8,"
SolCallback(sBallRelease)="bsTrough.SolOut"				'Sol16
SolCallback(sLSKicker)="vpmSolAutoPlunger Plunger1,1,"	'Sol25
SolCallback(SGI)="GIUpdate"
SolCallback(sLeftKick)="bsLeftLock.SolOut"				'Sol26
SolCallback(sUpKicker)="VUKKick"						'Sol27
SolCallback(sCenterKicker)="bsRightLock.SolOut"			'Sol28
SolCallback(s3BankReset)="dtT.SolDropUp"				'Sol29
SolCallback(sKnocker)="vpmSolSound ""knocker"","		'Sol30
SolCallback(sOutHole)="bsTrough.SolIn"					'Sol31
SolCallback(sSinkingShip)="SinkIt"

'FLASHERS
Const sDestroyHotdog=1
Const sTorpedoHotdog=2
Const sFlagshipHotdog=3
Const sAircraftHotdog=4
Const sSpecialHotdog=5
Const sCruiserHotdog=6
Const sScope=7
Const sInsert=8
Const sLeftPair=9
Const sCenterPair=14
Const sRightPair=15

SolCallback(sDestroyHotdog)="DESTHD"				'Sol1
SolCallback(sTorpedoHotdog)="TorpHD"				'Sol2
SolCallback(sFlagshipHotdog)="FlagHD"			'Sol3
SolCallback(sAircraftHotdog)="ACHD"			'Sol4
SolCallback(sSpecialHotdog)="SpecialHD"				'Sol5
SolCallback(sCruiserHotdog)="CRUISEHD"				'Sol6
SolCallback(sScope)="ScopeHD"						'Sol7
'SolCallback(sInsert)="vpmFlasher ,"					'Sol8
SolCallback(sLeftPair)="SolLeft"					'Sol9
SolCallback(sCenterPair)="SolCentre"				'Sol14
SolCallback(sRightPair)="SolRight"					'Sol15	

SolCallback(sLRFlipper)="vpmSolFlipper RightFlipper,RightFlipper1,"
SolCallback(sLLFlipper)="vpmSolFlipper LeftFlipper,Nothing,"

Dim bsTrough,dtT,bsLeftLock,bsTopLock,bsRightLock
Dim obj
Dim shpos

Sub Table1_Init

On Error Resume Next
		With Controller
			.GameName=cGameName
			If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
			.SplashInfoLine = "Torpedo Alley - Data East" & vbnewline & "Table by Destruk/TAB"
			.HandleMechanics=0
			.HandleKeyboard=0
			.ShowDMDOnly=1
			.ShowFrame=0
			.ShowTitle=0
			.DIP(0)=&H00
			.Run
 			.Hidden=1
			If Err Then MsgBox Err.Description
		End With
	On Error Goto 0

	vpmNudge.TiltSwitch=1
	vpmNudge.Sensitivity=5
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)

	Set bsTrough=New cvpmBallStack
	bsTrough.InitSw 0,13,12,11,10,0,0,0
	bsTrough.InitKick BallRelease,90,6
	bsTrough.InitEntrySnd "solenoid", "solenoid"
	bsTrough.InitExitSnd "ballrel", "solenoid"
	bsTrough.Balls=3

	Set dtT=New cvpmDropTarget
	dtT.InitDrop Array(SW29,SW30,SW31),Array(29,30,31)
	dtT.InitSnd "target_drop","reset_drop"

	Set bsLeftLock=New cvpmBallStack
	bsLeftLock.InitSw 0,42,41,0,0,0,0,0
	bsLeftLock.InitKick Kicker2,40,15
	bsLeftLock.InitExitSnd "popper","solenoid"
	'bsLeftLock.KickBalls=2

	Set bsTopLock=New cvpmBallStack
	bsTopLock.InitSw 0,43,0,0,0,0,0,0
	bsTopLock.InitKick VUK,45,10
	bsTopLock.InitExitSnd "popper","solenoid"

	Set bsRightLock=New cvpmBallStack
	bsRightLock.InitSw 0,45,44,0,0,0,0,0
	bsRightLock.InitKick Kicker1,45,35
	bsRightLock.InitExitSnd "popper","solenoid"
	bsRightLock.KickBalls=2

    SW37A.isdropped = 1 
    SW56A.isdropped = 1 
    Plunger1.Pullback
End Sub

Sub BallReflect_hit:table1.BallReflection = false:end Sub
Sub BallReflect_unhit:table1.BallReflection = true:end Sub

Sub Kicker1_Hit():Kicker1.kick 45,35:End Sub 
Sub Kicker2_Hit():Kicker2.kick 40,15:End Sub

If Table1.ShowDT = false then
    Scoretext.Visible = false
End If

Sub Table1_KeyDown(ByVal keycode)

	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySound "plungerpull",0,1,0.25,0.25
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
    
    If KeyCode=LeftFlipperKey Then Controller.Switch(15)=1
	If KeyCode=RightFlipperKey Then Controller.Switch(16)=1
	If vpmKeyDown(KeyCode) Then Exit Sub    
End Sub

Sub Table1_KeyUp(ByVal keycode)

	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySound "plunger",0,1,0.25,0.25
	End If
	If KeyCode=LeftFlipperKey Then Controller.Switch(15)=0
	If KeyCode=RightFlipperKey Then Controller.Switch(16)=0
	If vpmKeyUp(KeyCode) Then Exit Sub
End Sub

Sub Drain_Hit:me.destroyball:bsTrough.AddBall Me:PlaySound "drain":End Sub


Sub Plunger_Init()
	'PlaySound "ballrelease",0,0.5,0.5,0.25
	'Plunger.CreateBall
	'BallRelease.CreateBall
	'BallRelease.Kick 90, 8
End Sub


Sub Bumper1_Hit
    vpmTimer.PulseSw 50 
	PlaySound "fx_bumper4"
	'B1L1.State = 1:B1L2. State = 1
	Me.TimerEnabled = 1
End Sub

Sub Bumper1_Timer
	'B1L1.State = 0:B1L2. State = 0
	Me.Timerenabled = 0
End Sub

Sub Bumper2_Hit
    vpmTimer.PulseSw 51
	PlaySound "fx_bumper4"
	'B2L1.State = 1:B2L2. State = 1
	Me.TimerEnabled = 1
End Sub

Sub Bumper2_Timer
	'B2L1.State = 0:B2L2. State = 0
	Me.Timerenabled = 0
End Sub	

Sub Bumper3_Hit
    vpmTimer.PulseSw 49
	PlaySound "fx_bumper4"
	'B3L1.State = 1:B3L2. State = 1
	Me.TimerEnabled = 1
End Sub

Sub Bumper3_Timer
	'B3L1.State = 0:B3L2. State = 0
	Me.Timerenabled = 0
End Sub

Sub SolCentre(Enabled)
If enabled Then
CB.state = 1
CB1.state = 1
Flasher2.state = 1
Flasher2a.state = 1
else
CB.state = 0
CB1.state = 0
Flasher2.state = 0
Flasher2a.state = 0
End If
End Sub

Sub SolLeft(Enabled)
If enabled Then
LB.state = 1
LB1.state = 1
Flasher3.state = 1
Flasher3a.state = 1
else
LB.state = 0
LB1.state = 0
Flasher3.state = 0
Flasher3a.state = 0
End If
End Sub

Sub SolRight(Enabled)
If enabled Then
RB.state = 1
RB1.state = 1
Flasher1.state = 1
Flasher1a.state = 1
else
RB.state = 0
RB1.state = 0
Flasher1.state = 0
Flasher1a.state = 0
End If
End Sub

Sub SpecialHD(Enabled)
If enabled Then
F30.state = 1
else
F30.state = 0
End If
End Sub

Sub ScopeHD(Enabled)
If enabled Then
FScope.State = 1
else
FScope.State = 0
End If
End Sub

Sub ACHD(Enabled)
If enabled Then
F22.state = 1
else
F22.state = 0
End If
End Sub

Sub TorpHD(Enabled)
If enabled Then
F8.State = 1
else
F8.State = 0
End If
End Sub

Sub FlagHD(Enabled)
If enabled Then
F45.state = 1
else
F45.state = 0
End If
End Sub

Sub DestHD(Enabled)
If enabled Then
F36.state = 1
else
F36.state = 0
End If
End Sub

Sub CruiseHD(Enabled)
If enabled Then
F40.state = 1
else
F40.state = 0
End If
End Sub

Sub RLKICK_hit()
SinkShip.enabled = 0
me.destroyball
BsRightLock.AddBall Me
PlaySound "Scoopenter"
End Sub

Sub LLKICK_hit()
SinkShip.enabled = 0
me.destroyball
BsLeftLock.addball Me
PlaySound "Scoopenter"
End Sub

Sub VUK_hit()
SinkShip.enabled = 0
hasbeenhit = 1
Vtimer.enabled = 1
PlaySound "Scoopenter"
End Sub

Sub Vtimer_timer()
Controller.Switch(43) = 1
me.enabled = 0
End Sub

Sub MRIN_Hit()
me.destroyball
mballout.enabled = 1
End Sub

Sub mballout_timer()
mrout.createball
mrout.kick 95, 10
'PlaySound "ball_bounce"
me.enabled = 0
End Sub

Sub trigger1_hit()
ActiveBall.VelY = ActiveBall.VelY * 2
End Sub

Dim raiseballsw, raiseball, hasbeenhit

 Sub VUKKick(Enabled)
	if(enabled) AND hasbeenhit = 1 then
 		VUK.destroyball
        PlaySound "Kicker_enter_center"
		 'bsRTPop.balls = bsRTPop.balls  -1
		'VUK.DestroyBall
 		Set raiseball = VUK.CreateBall
 		raiseballsw = True
 		Vukraiseballtimer.Enabled = True 
		'VUK.Enabled=TRUE
	end if
End Sub
 
 Sub Vukraiseballtimer_Timer()
 	If raiseballsw = True then
 		raiseball.z = raiseball.z + 10
 		If raiseball.z > 120 then
 			VUK.Kick 130, 10
			PlaySound "popper"
 			Set raiseball = Nothing
 			Vukraiseballtimer.Enabled = False
 			raiseballsw = False
		Controller.Switch(43) = 0
        hasbeenhit = 0
 		End If
 	End If
 End Sub

Sub Sinkit(enabled)
If enabled Then
shpos = 1
SinkShip.enabled = 1
End If
End Sub

Sub SinkShip_Timer()
Select Case shpos
Case 1:Ship.imageA = "Sink1":shpos = 2
Case 2:Ship.imageA = "Sink2":shpos = 3 
Case 3:Ship.imageA = "Sink1":shpos = 4 
Case 4:Ship.imageA = "Sink2":shpos = 5 
Case 5:Ship.imageA = "Sink3":shpos = 6 
Case 6:Ship.imageA = "Sink4":Ship.color = rgb(80,80,80):L31.intensity = 0:L32.intensity = 0:shpos = 7 
Case 7:vpmTimer.addtimer 800, "shpos = 8'"
Case 8:Ship.imageA = "Sink3":Ship.color = rgb(60,60,60):shpos = 9  
Case 9:Ship.imageA = "Sink5":Ship.color = rgb(128,128,128):L31.intensity = 50:L32.intensity = 50:SinkShip.enabled = 0 
End Select
End Sub
'****Targets

'Sub SW14_hit():vpmTimer.PulseSw 14:End Sub
Sub SW14a_hit():Controller.Switch(14) = 1:End Sub
Sub SW14a_Unhit():Controller.Switch(14) = 0:End Sub
Sub SW54_hit():vpmTimer.PulseSw 54:End Sub
Sub SW55_hit():vpmTimer.PulseSw 55:End Sub
Sub SW17_hit():Controller.Switch(17) = 1:End Sub
Sub SW17_Unhit():Controller.Switch(17) = 0:End Sub
Sub SW18_hit():vpmTimer.PulseSw 18:End Sub
Sub SW23_hit():vpmTimer.PulseSw 23:End Sub
Sub SW26_hit():vpmTimer.PulseSw 26:End Sub
Sub SW27_hit():vpmTimer.PulseSw 27:End Sub
Sub SW28_hit():vpmTimer.PulseSw 28:End Sub
Sub SW36_hit():vpmTimer.PulseSw 36:End Sub
Sub SW25_Spin():vpmTimer.PulseSw 25:PlaySound "fx_spinner":End Sub
Sub SW32_Spin():vpmTimer.PulseSw 32:PlaySound "fx_spinner":End Sub
Sub SW29_hit():Dtt.Hit 1:End Sub
Sub SW30_hit():Dtt.Hit 2:End Sub
Sub SW31_hit():Dtt.Hit 3:End Sub

Sub SW46_Hit():vpmTimer.PulseSw 46:SinkShip.enabled = 0:End Sub

Sub SW24_hit():sw24p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 24:End Sub
Sub SW24_Timer():sw24p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW33_hit():sw33p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 33:End Sub
Sub SW33_Timer():sw33p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW34_hit():sw34p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 34:End Sub
Sub SW34_Timer():sw34p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW35_hit():sw35p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 35:End Sub
Sub SW35_Timer():sw35p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW20_hit():sw20p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 20:End Sub
Sub SW20_Timer():sw20p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW21_hit():sw21p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 21:End Sub
Sub SW21_Timer():sw21p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW22_hit():sw22p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 22:End Sub
Sub SW22_Timer():sw22p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW38_hit():sw38p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 38:End Sub
Sub SW38_Timer():sw38p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW39_hit():sw39p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 39:End Sub
Sub SW39_Timer():sw39p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW40_hit():sw40p.transx = -10:Me.TimerEnabled = 1:vpmTimer.PulseSw 40:End Sub
Sub SW40_Timer():sw40p.transx = 0:Me.TimerEnabled = 0:End Sub

Sub SW37_Hit():vpmTimer.PulseSw 37:SW37.isdropped = 1:SW37a.isdropped = 0:me.timerenabled = 1:End Sub
Sub SW37_Timer():SW37.isdropped = 0:SW37a.isdropped = 1:me.timerenabled = 0:End Sub

Sub SW56_Hit():vpmTimer.PulseSw 56:SW56.isdropped = 1:SW56a.isdropped = 0:me.timerenabled = 1:End Sub
Sub SW56_Timer():SW56.isdropped = 0:SW56a.isdropped = 1:me.timerenabled = 0:End Sub

'*****GI Lights On
dim xx
Sub GIUpdate(enabled)
If enabled Then
For each xx in GI:xx.State = 0: Next
else
For each xx in GI:xx.State = 1: Next
End If 
End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    vpmTimer.PulseSw 53
    PlaySound "left_slingshot", 0, 1, 0.05, 0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	'gi1.State = 0:Gi2.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    vpmTimer.PulseSw 52
    PlaySound "right_slingshot",0,1,-0.05,0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	'gi3.State = 0:Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Set MotorCallback=GetRef("UpdateMultipleLamps")

Sub UpdateMultipleLamps
If Controller.Switch(43) = True Then
Light1.State = 1
else
Light1.State = 0
End If
End Sub

Set Lights(1)=L1
Set Lights(2)=L2
Set Lights(3)=L3
Set Lights(4)=L4
Set Lights(5)=L5	
Set Lights(6)=L6
Set Lights(7)=L7
Set Lights(8)=L8
Set Lights(9)=L9
Set Lights(10)=L10
Set Lights(11)=L11
Set Lights(12)=L12
Set Lights(13)=L13
Set Lights(14)=L14
Set Lights(15)=L15
Set Lights(16)=L16
Set Lights(17)=L17
Set Lights(18)=L18
Set Lights(19)=L19
Set Lights(20)=L20
Set Lights(21)=L21
Set Lights(22)=L22
Lights(23)=Array(L23,L23A)
'Lights(24)=Array(L24,L24a)
Set Lights(25)=L25
Set Lights(26)=L26
Set Lights(27)=L27
Set Lights(28)=L28
Set Lights(29)=L29
Set Lights(30)=L30
Set Lights(31)=L31
Set Lights(32)=L32
Set Lights(33)=L33
Set Lights(34)=L34
Set Lights(35)=L35
Lights(36)=Array(L36,L36A)
Set Lights(37)=L37
Set Lights(38)=L38
Set Lights(39)=L39
Lights(40)=Array(L40,L40A)
Set Lights(41)=L41
Set Lights(42)=L42
Set Lights(43)=L43
Set Lights(44)=L44
Set Lights(45)=L45
Set Lights(46)=L46
Set Lights(47)=L47
Set Lights(48)=L48
Set Lights(49)=L49
Set Lights(50)=L50
Set Lights(51)=L51
Set Lights(52)=L52
Set Lights(53)=L53
Set Lights(54)=L54
Set Lights(55)=L55
Set Lights(56)=L56
Set Lights(57)=L57
Set Lights(58)=L58
Set Lights(59)=L59
Set Lights(60)=L60
Set Lights(61)=L61
Set Lights(62)=L62
Set Lights(63)=L63
Set Lights(64)=L64

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
    Vol = Csng(BallVel(ball) ^2 / 5000)
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

Const tnob = 6 ' total number of balls
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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
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
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

 '**********************
'Flipper Shadows
'***********************
Sub RealTime_Timer
  lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
BallShadowUpdate
if SinkShip.enabled = 0 then Ship.imageA = "Sink5":Ship.color = rgb(128,128,128):L31.intensity = 50:L32.intensity = 50
End Sub


Sub BallShadowUpdate()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6)
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
        If BOT(b).Z > 20 and BOT(b).Z < 200 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 90
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 80
End If
    Next	
End Sub

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target"
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Rollovers_Hit (idx)
	PlaySound "rollover", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub Table1_Exit
Controller.Stop
End Sub