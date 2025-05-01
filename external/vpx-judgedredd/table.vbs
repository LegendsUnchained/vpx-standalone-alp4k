Option Explicit
Randomize

'************************************
'******* Standard definitions *******
'************************************
Const UseSolenoids = 2
Const UseLamps = 1
Const UseSync = 0
Const HandleMech = 0
Const UseGI = 1

if Table1.showDT = False Then Ramp15.visible = 0:Ramp16.visible = 0

' Thalamus, added suggested change for ballsize and msss from BrandownLaw.
' https://www.vpforums.org/index.php?showtopic=43616&p=442964
' I agree, this improves upon the release.

Dim BallSize, BallMass
BallSize = 50
BallMass = .93

' Standard Sounds
Const SSolenoidOn  = "Solenoid"
Const SSolenoidOff = ""
Const SFlipperON   = "FlipperUP"
Const SFlipperOFF  = "FlipperDown"
Const SCoin        = "Coin"


' Thalamus 2020 February : Improved directional sounds
' !! NOTE : Table not verified yet !!

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 2000    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )

' The rest of the values are multipliers
'
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 2    ' Bumpers volume.
Const VolRol    = 1    ' Rollovers volume.
Const VolGates  = 1    ' Gates volume.
Const VolMetal  = 1    ' Metals volume.
Const VolRB     = 1    ' Rubber bands volume.
Const VolRH     = 1    ' Rubber hits volume.
Const VolPo     = 1    ' Rubber posts volume.
Const VolPi     = 1    ' Rubber pins volume.
Const VolPlast  = 1    ' Plastics volume.
Const VolTarg   = 1    ' Targets volume.
Const VolWood   = 1    ' Woods volume.
Const VolKick   = 1    ' Kicker volume.
Const VolSpin   = 1.5  ' Spinners volume.
Const VolFlip   = 1    ' Flipper volume.


'*************************************************
'********** Antes de nada ************************
'*************************************************
Dim bsTrough, bsLeftPopper, bsRightPopper, jdDrop

'***********************
'***** Table init ******
'***********************
Const cGameName = "jd_l1"

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

LoadVPM "01560000", "WPC.VBS", 3.36

Sub Table1_Init()
  vpmInit Me
  With Controller
    .GameName = cGameName
    If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
    .SplashInfoLine = "Judge Dredd - based on the Williams's 1993 Table" & vbNewLine & "VP9 table and Script by Lord Hiryu v1.0"
    .HandleKeyboard = 0
    .ShowTitle = 0
    .ShowDMDOnly = 1
    .ShowFrame = 0
    .Games(cGameName).Settings.Value("sound") = 1
    If Table1.ShowDT = true then
      .Games(cGameName).Settings.Value("dmd_pos_x")=20
      .Games(cGameName).Settings.Value("dmd_pos_y")=20
      .Games(cGameName).Settings.Value("dmd_width")=260
      .Games(cGameName).Settings.Value("dmd_height")=83
      .Games(cGameName).Settings.Value("rol")=0
    End If
    .HandleMechanics = 0
    .Hidden = 0
    On Error Resume Next
    .Run GetPlayerHWnd
    If Err Then MsgBox Err.Description
      On Error Goto 0
  End With

  '********************************
  '     Setup Machine State       *
  '********************************
  Controller.Switch(22)=1

  '*************************************
  'Trough is manually built to work around super sensitive ball timing optos.

  'TROUGH
  'Set bsTrough = New cvpmTrough
  '	With bsTrough
  '       .IsTrough = True
  '	.InitSwitches Array(86,85,84,83,82,81)
  '	.InitExit BallRelease, 90, 3
  '   .Size = 6
  '.Balls = 6
  '.InitExitSounds "BallRelease","Solenoid"
  'End With

  Set bsLeftPopper=new cvpmBallStack
  With bsLeftPopper
    .InitSw 0,0,0,0,0,0,0,0
    .InitKick SWT,180,5
    '.kickZ=90
    .InitExitSnd "Solenoid", "Solenoid"
    .Balls = 0
  End With

  Set jdDrop = New cvpmDropTarget
  With jdDrop
    .InitDrop Array(Array(sw54,sw54a),Array(sw55,sw55a),Array(sw56,sw56a),Array(sw57,sw57a),Array(sw58,sw58a)),Array(54,55,56,57,58)
    .InitSnd "Target_Drop", "ResetDrop"
  End With

  'Main Timer init

  PinMAMETimer.Interval = PinMAMEInterval
  PinMAMETimer.Enabled = 1
  vpmNudge.TiltSwitch = 14
  vpmNudge.Sensitivity = 4

  'Captive ball handling

  CapBall1.createball
  CapBall1.kick 180,1

  CapBall2.createball
  CapBall2.kick 180,1

  CapBall3.createball
  CapBall3.kick 180,1

  W81.isdropped = 1
  W82.isdropped = 1
  W83.isdropped = 1
  W84.isdropped = 1
  W85.isdropped = 1
  W86.isdropped = 1
  Sol8.Pullback
  Sol9.Pullback
  Dead_Block1.isdropped = 1
  jdbp = 0
  jdbp1 = 0
  jdbp2 = 0
  jdb = 0
  jdb1 = 0
  jdb2 = 0
End Sub

 Sub table1_Paused:Controller.Pause = 1:End Sub
 Sub table1_unPaused:Controller.Pause = 0:End Sub

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyDownHandler(KeyCode) Then Exit Sub
	If KeyCode=PlungerKey Then Controller.Switch(12)=1
  If KeyCode=3 Then Controller.Switch(31)=1
 	If keycode = RightMagnaSave Then Controller.Switch(44) = True  ' Super Game        (-)
 	If keycode = LeftMagnaSave Then Controller.Switch(11) = True   ' Left Fire Button  (z)
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(KeyCode) Then Exit Sub
	If KeyCode=PlungerKey Then Controller.Switch(12)=0
  If KeyCode=3 Then Controller.Switch(31)=0
  If keycode = RightMagnaSave Then Controller.Switch(44) = False ' Super Game        (-)
 	If keycode = LeftMagnaSave Then Controller.Switch(11) = False  ' Left Fire Button  (z)
End Sub

 '******************************************
 '********* Solenoid Callbacks *************
 '******************************************

SolCallBack(1)	= "GlobeMag"
SolCallBack(2)	= "VUK2Kick"
SolCallBack(3)	= "VUKKick"
SolCallBack(4)	= "GlobeArm"
SolCallBack(5)	= "jdDrop.solDropUp"
SolCallBack(6)	= "GlobeMotor"
SolCallBack(7)	= "vpmSolSound ""Knocker"","
SolCallBack(8)	= "JDPlunger"
SolCallBack(9)	= "KickBack"
SolCallBack(10) = "jdDrop.Hit 3 '"
SolCallBack(11)	= "Diverter"
SolCallBack(13)	= "JDTrough"
'SolCallBack(15)	= "vpmSolSound ""WrongSound"","
'SolCallBack(16)	= "vpmSolSound ""WrongSound"","

SolCallBack(17)	= "fF17"     	'"SetLamp 100,"     'Judge Fire
SolCallBack(18)	= "fF18"			'"SetLamp 101,"     'Judge Fear
SolCallBack(19)	= "fF19"			'"SetLamp 102,"     'Judge Death
SolCallBack(20)	= "fF20"			'"SetLamp 103,"     'Judge Mortis
SolCallBack(21)	= "LRF"
SolCallBack(22)	= "RRF"
SolCallBack(23) = "Flash23"

SolCallBack(24)	= "U_Globe_Flash"
SolCallBack(25) = "Flash25"
SolCallBack(26)	= "Globe_Flash"
SolCallBack(27) = "Flash27"

SolCallback(sURFlipper) = "SolFlipper RightFlipper2,Nothing,"
SolCallback(sULFlipper) = "SolFlipper LeftFlipper2,Nothing,"

SolCallback(sLRFlipper) = "SolFlipper RightFlipper,Nothing,"
SolCallback(sLLFlipper) = "SolFlipper LeftFlipper,Nothing,"

'**************		GI		*****************

Set GiCallback2 = GetRef("UpdateGI")
Dim xxx
Sub UpdateGI(nr,step)
				For each xxx in GI:xxx.IntensityScale = 0.4 * step:next
		If Step>=7 Then Table1.ColorGradeImage = "ColorGrade8":Else Table1.ColorGradeImage = "ColorGrade_" & (step+1):End If
        		For each xxx in GI:xxx.state=step:Next
End Sub

'Light Handler - Simple because of inbuilt fading lamps (Thanks Toxie & Fuzzel!)

Set Lights(11)=L11
Set Lights(12)=L12
Set Lights(13)=L13
Set Lights(14)=L14

Set Lights(15)=L15
Set Lights(16)=L16
Set Lights(17)=L17
Set Lights(18)=L18

Set Lights(21)=L21
Set Lights(22)=L22
Set Lights(23)=L23
Set Lights(24)=L24

Set Lights(25)=L25
Set Lights(26)=L26
Set Lights(27)=L27
Set Lights(28)=L28

Set Lights(31)=L31
Set Lights(32)=L32
Set Lights(33)=L33
Set Lights(34)=L34

Lights(35)=Array(L35,L35a)
Lights(36)=Array(L36,L36a)
Lights(37)=Array(L37,L37a)

Set Lights(41)=L41
Set Lights(42)=L42
Set Lights(43)=L43
Set Lights(44)=L44

Set Lights(45)=L45
Set Lights(47)=L47
Set Lights(46)=L46
Set Lights(48)=L48
Set Lights(51)=L51
Set Lights(54)=L54
Set Lights(55)=L55
Set Lights(52)=L52
Set Lights(53)=L53
Set Lights(56)=L57
Set Lights(57)=L57

Set Lights(52)=L52
Set Lights(53)=L53
Set Lights(56)=L56
Set Lights(57)=L57
Set Lights(58)=L58

Lights(61)=Array(L61,L61a)

Set Lights(62)=L62
Set Lights(63)=L63
Set Lights(64)=L64
Set Lights(65)=L65
Set Lights(66)=L66
Set Lights(67)=L67
Set Lights(68)=L68

Set Lights(71)=L71
Set Lights(72)=L72
Set Lights(73)=L73
Set Lights(74)=L74
Set Lights(75)=L75

Set Lights(76)=L76
Set Lights(77)=L77
Set Lights(78)=L78

Set Lights(81)=L81
Set Lights(82)=L82

Set Lights(85)=L85
Lights(83)=Array(L83,L83a,L83b)
Set Lights(84)=L84
Set Lights(86)=L86

' Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************************************************************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
  PlaySoundAtVol "left_slingshot", sling1, 1
  RSling.Visible = 0
  RSling1.Visible = 1
  sling1.TransZ = -20
  RStep = 0
  RightSlingShot.TimerEnabled = 1
  RightSlingShot.TimerInterval = 10
  vpmTimer.PulseSw 52
End Sub

Sub RightSlingShot_Timer
  Select Case RStep
    Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
    Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
  End Select
  RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
  PlaySoundAtVol "right_slingshot", sling2, 1
  LSling.Visible = 0
  LSling1.Visible = 1
  sling2.TransZ = -20
  LStep = 0
  LeftSlingShot.TimerEnabled = 1
  LeftSlingShot.TimerInterval = 10
  vpmTimer.PulseSw 51
End Sub

Sub LeftSlingShot_Timer
  Select Case LStep
    Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
    Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
  End Select
  LStep = LStep + 1
End Sub

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX and Rothbauerw
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

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
    PlaySound sound, 0, ABS(BOT.velz)/17, Pan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
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

Const tnob = 15 ' total number of balls
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

Sub BallShadowUpdate()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7,BallShadow8,BallShadow9,BallShadow10,BallShadow11,BallShadow12,BallShadow13,BallShadow14,BallShadow15)
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
        If BOT(b).Z > 20 and BOT(b).Z < 240 Then
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

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
  PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'Hit Index

Sub Pins_Hit (idx)
  PlaySound "pinhit_low", 0, Vol(ActiveBall)*VolPi, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
  PlaySound "target", 0, Vol(ActiveBall)*VolTarg, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
  PlaySound "metalhit_thin", 0, Vol(ActiveBall)*VolMetal, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
  PlaySound "metalhit_medium", 0, Vol(ActiveBall)*VolMetal, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
  PlaySound "metalhit2", 0, Vol(ActiveBall)*VolMetal, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
  PlaySound "gate4", 0, Vol(ActiveBall)*VolGates, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rollover_Hit (idx)
  PlaySoundAtVol "rollover", ActiveBall, VolRol
End Sub

Sub Spinner_Spin
  PlaySoundAtVol "fx_spinner", Spinner, VolSpin
End Sub

Sub Rubbers_Hit(idx)
  dim finalspeed
  finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 20 then
    PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End if
  If finalspeed >= 6 AND finalspeed <= 20 then
    RandomSoundRubber()
  End If
End Sub

Sub Posts_Hit(idx)
  dim finalspeed
  finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 16 then
    PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End if
  If finalspeed >= 6 AND finalspeed <= 16 then
    RandomSoundRubber()
  End If
End Sub

Sub RandomSoundRubber()
  Select Case Int(Rnd*3)+1
    Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
    Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End Select
End Sub


'SOLENOID SUBS

Sub Diverter(enabled)
  If enabled Then
    JDDiverter.RotateToEnd
    PlaySoundAtVol "fx_solenoid", JDDiverter, 1
  else
    JDDiverter.RotateToStart
    PlaySoundAtVol "fx_solenoid", JDDiverter, 1
  End If
End Sub

Sub KickBack(enabled)
  If enabled Then
    Sol9.Fire
    PlaySoundAtVol "fx_solenoid", Sol9, 1
  else
    Sol9.Pullback
  End If
End Sub

Sub JDPlunger(enabled)
  If enabled Then
    Sol8.Fire
    PlaySoundAtVol "fx_solenoid", Sol8, 1
  else
    Sol8.Pullback
  End If
End Sub

Sub JDTrough(enabled)
  If enabled Then
    SW86.kick 37,30
    PlaySoundAtVol "ballrelease", sw86, 1
    vpmTimer.PulseSw 87
  End If
End Sub

Sub GlobeMotor(enabled)
  If enabled Then
    DeadWorld.enabled = 1
  else
    DeadWorld.enabled = 0
  End If
End Sub

Sub GlobeArm(enabled)
  If enabled AND Crane_X.enabled = False Then
    Armstate = 1:Arm_Logic.enabled = 1
  else
    Armstate = 2:Arm_Logic.enabled = 1
  End If
End Sub

Dim Armstate

Sub Arm_Logic_Timer()
  Select Case ArmState
    Case 1:Controller.Switch(71) = 1:me.enabled = 0
    Case 2:Controller.Switch(71) = 0:me.enabled = 0
  End Select
End Sub

Dim Magon

Sub GlobeMag(enabled)
  If enabled Then
    Magon = 1
    Controller.Switch(28) = 1
  else
    Mag_off.enabled = 1
    Controller.Switch(28) = 0
  End If
End Sub

Sub Mag_off_Timer()
  Magon = 0
  me.enabled = 0
End Sub

Dim raiseballsw, raiseball, hasbeenhit, mball

Sub VUKKick(Enabled)
  if(enabled) AND hasbeenhit = 1 then
    Sol3.destroyball
    PlaySoundAtVol "Kicker_enter_center", Sol3, 1
    Set mball = Sol3.CreateBall
    Vpos = 1
    Vukraiseballtimer.Enabled = True
  End if
End Sub

dim vpos

sub Vukraiseballtimer_timer()
  Select Case vpos
    Case 1:
      mball.z = mball.z + 1
      If mball.z = 210 Then
        vpos = 2
      End If
    Case 2:
      If mball.x <= 800 Then
        vpos = 3
      End If
      mball.x = mball.x - 1
      mball.z = mball.z + 1.5
      mball.y = mball.y - 0.8
    Case 3:
      If mball.x <= 734 Then
        vpos = 4
      End If
      mball.x = mball.x - 1
      mball.y = mball.y - 0.8
    Case 4:
      If mball.z <= 150 Then
        vpos = 5
      End If
      mball.y = mball.y - 0.8
      mball.z = mball.z - 2
      mball.x = mball.x - 0.5
    Case 5:
      me.enabled = 0
      'Sol3.Kick 180, 5
      Sol3.destroyball
      VUK_Exit.createball
      VUK_Exit.kick 170,2
      PlaySoundAtVol "popper", VUK_Exit, 1
      Set raiseball = Nothing
      Controller.Switch(74) = 0
      hasbeenhit = 0
  End Select
End Sub

Dim raiseballsw2, raiseball2, hasbeenhit2

Sub VUK2Kick(Enabled)
  if(enabled) AND hasbeenhit2 = 1 then
    PlaySoundAtVol "Kicker_enter_center", Ball1, 1
    Set raiseball2 = Sol2.CreateBall
    raiseballsw2 = True
    Vukraiseballtimer2.Enabled = True
  end if
End Sub

Sub Vukraiseballtimer2_Timer()
  If raiseballsw2 = True then
    raiseball2.z = raiseball2.z + 10
    If raiseball2.z > 120 then
      Sol2.Kick 180, 10
      PlaySoundAtVol "popper", Sol2, 1
      Set raiseball = Nothing
      Vukraiseballtimer2.Enabled = False
      raiseballsw2 = False
      Controller.Switch(73) = 0
      BallsInHole = BallsInHole - 1
      hasbeenhit2 = 0
    End If
  End If
End Sub

 Sub Globe_Flash(enabled)
   If enabled Then
     gflashup = 1:Globe_Flash_Up.enabled = 1
   else
     gflashdown = 1:Globe_Flash_Down.enabled = 1
   End If
 End Sub

Sub U_Globe_Flash(enabled)
  If enabled Then
    FDW1.state = 1
  else
    FDW1.state = 0
  End If
End Sub

Sub LRF(enabled)
  If enabled Then
    F21.state = 1
    F21a.state = 1
    F21b.state = 1
    F21c.state = 1
F21d.state = 1
    F21e.state = 1
  else
    F21.state = 0
    F21a.state = 0
    F21b.state = 0
    F21c.state = 0
F21d.state = 0
    F21e.state = 0
  End If
End Sub

Sub RRF(enabled)
  If enabled Then
    F22.state = 1
    F22a.state = 1
    F22b.state = 1
    F22c.state = 1
 F22d.state = 1
    F22e.state = 1
  else
    F22.state = 0
    F22a.state = 0
    F22b.state = 0
    F22c.state = 0
F22d.state = 0
    F22e.state = 0
  End If
End Sub

Sub Flash23(enabled)
  If enabled Then
    F23.state = 1
  else
    F23.state = 0
  End If
End Sub

Sub Flash27(enabled)
  If enabled Then
    F27.state = 1
  else
    F27.state = 0
  End If
End Sub

Sub Flash25(enabled)
  If enabled Then
    F25.state = 1
    F25a.state = 1
    F25b.state = 1
  else
    F25.state = 0
    F25a.state = 0
    F25b.state = 0
  End If
End Sub

Sub FF17(enabled)
  If enabled Then
    F17.state = 1
  else
    F17.state = 0
  End If
End Sub

Sub FF18(enabled)
  If enabled Then
    F18.state = 1
  else
    F18.state = 0
  End If
End Sub

Sub FF19(enabled)
  If enabled Then
    F19.state = 1
  else
    F19.state = 0
  End If
End Sub

Sub FF20(enabled)
  If enabled Then
    F20.state = 1
  else
    F20.state = 0
  End If
End Sub

Dim gflashup

Sub Globe_Flash_Up_Timer()
  FDW.state = 1
  Select Case gflashup
    Case 1:Nipple.Image = "Deadworld":gflashup = 2
    Case 2:Nipple.Image = "Deadworld_R1":gflashup = 3
    Case 3:Nipple.Image = "Deadworld_R2":gflashup = 4
    Case 4:Nipple.Image = "Deadworld_R3":gflashup = 5
    Case 5:Nipple.Image = "Deadworld_R4":gflashup = 6
    Case 6:Nipple.Image = "Deadworld_R5":gflashup = 7
    Case 7:Nipple.Image = "Deadworld_Red":me.enabled = 0
  End Select
End Sub

Dim gflashdown

Sub Globe_Flash_Down_Timer()
  FDW.state = 0
  Select Case gflashdown
    Case 1:Nipple.Image = "Deadworld_Red":gflashdown = 2
    Case 2:Nipple.Image = "Deadworld_R5":gflashdown = 3
    Case 3:Nipple.Image = "Deadworld_R4":gflashdown = 4
    Case 4:Nipple.Image = "Deadworld_R3":gflashdown = 5
    Case 5:Nipple.Image = "Deadworld_R2":gflashdown = 6
    Case 6:Nipple.Image = "Deadworld_R1":gflashdown = 7
    Case 7:Nipple.Image = "Deadworld":me.enabled = 0
  End Select
End Sub

'END SOLENOIDS

'############# DEAD WORLD WATCHDOG ############
'This sub handles the planet primitive rotation,
'Opto Interruptor logic, and planet ball feed logic
'including some rudimentary error handling in case
'balls backup at the feed entry

Dim jdb,jdb1,jdb2

Sub DeadWorld_Timer()
  If Nipple.RotY => 360 Then
    Nipple.RotY = 0
  End If

  '### Switch 71 Opto ###
  If Nipple.RotY >= 115 AND Nipple.RotY <=125 Then
    Controller.Switch(77) = 1
  else
    Controller.Switch(77) = 0
  End If
  If Nipple.RotY >= 235 AND Nipple.RotY <=245 Then
    Controller.Switch(77) = 1
  End If
  If Nipple.RotY >= 350 AND Nipple.RotY <=360 Then
    Controller.Switch(77) = 1
  End If

  '### Switch 61 Opto ###

  If Nipple.RotY >= 65 AND Nipple.RotY <=70 Then
    Controller.Switch(61) = 1
  else
    Controller.Switch(61) = 0
  End If

  If Nipple.RotY >= 185 AND Nipple.RotY <=190 Then
    Controller.Switch(61) = 1
  End If

  If Nipple.RotY >= 305 AND Nipple.RotY <=310 Then
    Controller.Switch(61) = 1
  End If

  '### Feed Pause ###

  If Nipple.RotY >= 70 AND Nipple.RotY <=75 AND jdbp = 0 Then
    Dead_Block.Isdropped = 1
    Block_Reset.enabled = 1
    jdb = 1
  End If
  If Nipple.RotY >= 185 AND Nipple.RotY <=192 AND jdbp1 = 0 Then
    Dead_Block.Isdropped = 1
    Block_Reset.enabled = 1
    jdb1 = 1
  End If
  If Nipple.RotY >= 302 AND Nipple.RotY <=310 AND jdbp2 = 0 Then
    Dead_Block.Isdropped = 1
    Block_Reset.enabled = 1
    jdb2 = 1
  End If
  Ball.RotZ=Ball.RotZ - 1
  Ball1.RotZ=Ball1.RotZ - 1
  Ball2.RotZ=Ball2.RotZ - 1
  Disc2.RotZ=Disc2.RotZ - 1
  Disc1.RotZ=Disc1.RotZ - 1
  Disc3.RotZ=Disc3.RotZ - 1
  Screw1.ObjRotZ=Screw1.ObjRotZ - 1
  Screw2.ObjRotZ=Screw2.ObjRotZ - 1
  Screw3.ObjRotZ=Screw3.ObjRotZ - 1
  Nipple.RotY=Nipple.RotY + 1
End Sub


'Workaround for the crane arm which is SUPER sensitive to ball exit timing and opto monitoring.

Sub Planet_Watch_Timer()
  If jdbp = 1 AND Nipple.RotY >= 220 AND Nipple.RotY <=250 AND bout1 = 0 AND magon = 1 AND DeadWorld.enabled = FALSE Then
    CX_action = 1:Crane_X.enabled = 1:me.enabled = 0
  End If
  If jdbp1 = 1 AND Nipple.RotY >= 320 AND Nipple.RotY <=360 AND bout1 = 0 AND magon = 1 AND DeadWorld.enabled = FALSE Then
    CX_action = 1:Crane_X.enabled = 1:me.enabled = 0
  End If
  If jdbp2 = 1 AND Nipple.RotY >= 100 AND Nipple.RotY <=130 AND bout1 = 0 AND magon = 1 AND DeadWorld.enabled = FALSE Then
    CX_action = 1:Crane_X.enabled = 1:me.enabled = 0
  End If

  If jdbp = 0 AND Nipple.RotY >= 220 AND Nipple.RotY <=250 AND bout1 = 0 AND magon = 1 AND DeadWorld.enabled = FALSE Then
    CX2_action = 1:Crane2_X.enabled = 1:me.enabled = 0
  End If
  If jdbp1 = 0 AND Nipple.RotY >= 320 AND Nipple.RotY <=360 AND bout1 = 0 AND magon = 1 AND DeadWorld.enabled = FALSE Then
    CX2_action = 1:Crane2_X.enabled = 1:me.enabled = 0
  End If
  If jdbp2 = 0 AND Nipple.RotY >= 100 AND Nipple.RotY <=130 AND bout1 = 0 AND magon = 1 AND DeadWorld.enabled = FALSE Then
    CX2_action = 1:Crane2_X.enabled = 1:me.enabled = 0
  End If
End Sub

'Switch Handlers

Const WallPrefix 		= "T" 'Change this based on your naming convention
Const PrimitivePrefix 	= "PrimT"'Change this based on your naming convention
Const PrimitiveBumperPrefix = "BumperRing" 'Change this based on your naming convention
Dim primCnt(100), primDir(100), primBmprDir(100)

'****************************************************************************
'      Primitive Standup Target Animation
'****************************************************************************
'USAGE: 	Sub sw1_Hit: 	PrimStandupTgtHit  1, Sw1, PrimSw1: End Sub
'USAGE: 	Sub Sw1_Timer: 	PrimStandupTgtMove 1, Sw1, PrimSw1: End Sub

Const StandupTgtMovementDir = "TransX"
Const StandupTgtMovementMax = 6

Sub PrimStandupTgtHit (swnum, wallName, primName)
  PlaySoundAtVol "target", ActiveBall, 1
  vpmTimer.PulseSw swnum
  primCnt(swnum) = 0 									'Reset count
  wallName.TimerInterval = 20 	'Set timer interval
  wallName.TimerEnabled = 1 	'Enable timer
End Sub

Sub	PrimStandupTgtMove (swnum, wallName, primName)
  Select Case StandupTgtMovementDir
    Case "TransX":
      Select Case primCnt(swnum)
        Case 0: 	primName.TransX = -StandupTgtMovementMax * .5
        Case 1: 	primName.TransX = -StandupTgtMovementMax
        Case 2: 	primName.TransX = -StandupTgtMovementMax * .5
        Case 3: 	primName.TransX = 0
        Case else: 	wallName.TimerEnabled = 0
      End Select
    Case "TransY":
      Select Case primCnt(swnum)
        Case 0: 	primName.TransY = -StandupTgtMovementMax * .5
        Case 1: 	primName.TransY = -StandupTgtMovementMax
        Case 2: 	primName.TransY = -StandupTgtMovementMax * .5
        Case 3: 	primName.TransY = 0
        Case else: 	wallName.TimerEnabled = 0
      End Select
    Case "TransZ":
      Select Case primCnt(swnum)
        Case 0: 	primName.TransZ = -StandupTgtMovementMax * .5
        Case 1: 	primName.TransZ = -StandupTgtMovementMax
        Case 2: 	primName.TransZ = -StandupTgtMovementMax * .5
        Case 3: 	primName.TransZ = 0
        Case else: 	wallName.TimerEnabled = 0
      End Select
  End Select
  primCnt(swnum) = primCnt(swnum) + 1
End Sub

Sub CapBall1_Unhit()
  me.enabled = 0
End Sub

Sub CapBall2_Unhit()
  me.enabled = 0
End Sub

Sub CapBall3_Unhit()
  me.enabled = 0
End Sub

Sub sw54_Hit	: playsoundAtVol "fx_droptarget", ActiveBall, 1:jdDrop.hit 1 : End Sub
Sub sw55_Hit	: playsoundAtVol "fx_droptarget", ActiveBall, 1:jdDrop.hit 2 : End Sub
Sub sw56_Hit	: playsoundAtVol "fx_droptarget", ActiveBall, 1:jdDrop.hit 3 : End Sub
Sub sw57_Hit	: playsoundAtVol "fx_droptarget", ActiveBall, 1:jdDrop.hit 4 : End Sub
Sub sw58_Hit	: playsoundAtVol "fx_droptarget", ActiveBall, 1:jdDrop.hit 5 : End Sub

Sub Dead_Enter_Hit()
  Controller.Switch(63) = 1
  Dead_Block1.isdropped = 0
End Sub

Sub Dead_Enter_Unhit()
  Controller.Switch(63) = 0
  Dead_Backup.enabled = 1
End Sub

Sub Dead_Backup_Timer()
  Dead_Block1.isdropped = 1
  me.enabled = 0
End Sub

Sub SW62_Hit()
  Controller.Switch(62) = 1
End Sub

Sub SW62_UnHit()
  Controller.Switch(62) = 0
  PlaySoundAtVol "Ball_Bounce", ActiveBall, 1
  If bout1 = 1 Then
    bout1 = 0
  End If
  If bout2 = 1 Then
    bout2 = 0
  End If
  If bout3 = 1 Then
    bout3 = 0
  End If
End Sub

Sub SW32_Hit()
  If cGameName = "jd_l1" Then
    vpmTimer.PulseSw 32
  End If
End Sub

Sub SW67_Hit()
  If cGameName = "jd_l7" Then
    vpmTimer.PulseSw 67
  End If
End Sub

Sub SW41_Hit()
  Controller.Switch(41) = 1
f51.state = 2
End Sub

Sub SW41_UnHit()
  Controller.Switch(41) = 0
f51.state = 0
End Sub

Sub SW15_Hit()
  Controller.Switch(15) = 1
f50.state = 2
End Sub

Sub SW15_UnHit()
  Controller.Switch(15) = 0
f50.state = 0
End Sub

Sub SW66_Hit()
  vpmTimer.PulseSw 66
End Sub

Sub SW64_Hit()
  vpmTimer.PulseSw 64
End Sub

Sub SW26_Hit()
  Controller.Switch(26) = 1
End Sub

Sub SW26_UnHit()
  Controller.Switch(26) = 0
End Sub

Dim ballsinhole

Sub sw37_Hit
  playsoundAtVol "Scoopenter", sw37, 1
  Me.DestroyBall
  vpmTimer.PulseSw 37
  PlaySound "Subway"
  bsLeftPopper.AddBall 0
  ballsinhole = ballsinhole + 1
End Sub

Sub sw38_Hit()
  vpmTimer.PulseSw 38
End Sub

Sub SWB_Hit()
  Me.Destroyball
  Controller.Switch(73) = 1
  hasbeenhit2 = 1
  PlaySound "Scoopenter"
End Sub

Sub SW53_Hit()
  Controller.Switch(53) = 1
End Sub

Sub SW53_UnHit()
  Controller.Switch(53) = 0
End Sub

Sub sw16_Hit()
  vpmTimer.PulseSw 16
End Sub

Sub sw17_Hit()
  vpmTimer.PulseSw 17
End Sub

Sub sw34_Hit()
  vpmTimer.PulseSw 34
End Sub

Sub sw33_Hit()
  vpmTimer.PulseSw 33
End Sub

Sub sw35_Hit()
  vpmTimer.PulseSw 35
End Sub

Sub sw43_Hit()
  vpmTimer.PulseSw 43
End Sub

Sub sw42_Hit()
  vpmTimer.PulseSw 42
End Sub

Sub sw72_Hit()
  vpmTimer.PulseSw 72
End Sub

Sub SW65_Hit()
  vpmTimer.PulseSw 65
End Sub

Sub SW75_Hit()
  vpmTimer.PulseSw 75
End Sub

Sub SW76_Hit()
  vpmTimer.PulseSw 76
End Sub

Sub RRD_Hit()
  PlaySoundAtVol "ball_bounce", ActiveBall, 1
End Sub

Sub RRD2_Hit()
  PlaySoundAtVol "ball_bounce", ActiveBall, 1
End Sub

Sub LRD_Hit()
  PlaySoundAtVol "ball_bounce", ActiveBall, 1
End Sub

Sub LRD1_Hit()
  PlaySoundAtVol "ball_bounce", ActiveBall, 1
End Sub

Sub LRD2_Hit()
  PlaySoundAtVol "ball_bounce", ActiveBall, 1
End Sub

Sub BRD_Hit()
  PlaySoundAtVol "ball_bounce", ActiveBall, 1
End Sub

Sub t18_Hit: 	PrimStandupTgtHit 18, T18, PrimT18: End Sub
Sub t18_Timer: PrimStandupTgtMove 18, T18, PrimT18: End Sub

Sub t18a_Hit: 	PrimStandupTgtHit 18, T18a, PrimT18a: End Sub
Sub t18a_Timer: PrimStandupTgtMove 18, T18a, PrimT18a: End Sub

Sub t18b_Hit: 	PrimStandupTgtHit 18, T18b, PrimT18b: End Sub
Sub t18b_Timer: PrimStandupTgtMove 18, T18b, PrimT18b: End Sub

Sub t68_Hit: 	PrimStandupTgtHit 68, T68, PrimT68: End Sub
Sub t68_Timer: PrimStandupTgtMove 68, T68, PrimT68: End Sub

Sub t27_Hit: 	PrimStandupTgtHit 27, T27, PrimT27: End Sub
Sub t27_Timer: PrimStandupTgtMove 27, T27, PrimT27: End Sub

Sub t25_Hit: 	PrimStandupTgtHit 25, T25, PrimT25: End Sub
Sub t25_Timer: PrimStandupTgtMove 25, T25, PrimT25: End Sub

Sub t36_Hit: 	PrimStandupTgtHit 36, T36, PrimT36: End Sub
Sub t36_Timer: PrimStandupTgtMove 36, T36, PrimT36: End Sub

Sub Sol3_hit()
  hasbeenhit = 1
  PlaySoundAtVol "Scoopenter", ActiveBall, 1
  Controller.Switch(74) = 1
End Sub

Sub Drain_Hit
  Me.DestroyBall
  Kicker_Load.createball
  PlaySoundAtVol "Drain", Drain, 1
  Kicker_Load.kick 45,10
  Drain.enabled = 0
  TDrain.enabled = 1
End Sub

Sub TDrain_Timer()
  Drain.enabled = 1
  me.enabled = 0
End Sub

Dim jdbp,jdbp1,jdbp2

Sub Deadworld_Feed_Hit()
  If jdb = 1 AND jdbp = 0 Then
    me.destroyball
    Ball1.visible = 1
    jdbp = 1
  End If
  If jdb1 = 1 AND jdbp1 = 0 Then
    me.destroyball
    Ball2.visible = 1
    jdbp1 = 1
  End If
  If jdb2 = 1 AND jdbp2 = 0 Then
    me.destroyball
    Ball.visible = 1
    jdbp2 = 1
  End If
End Sub

Sub Block_Reset_Timer()
  Dead_Block.Isdropped = 0
  jdb = 0
  jdb1 = 0
  jdb2 = 0
  me.enabled = 0
End Sub

dim CX_action,CX2_action,bout1, bout2, bout3


'Crane action if balls exist in the Deadworld holes.

Sub Crane_X_Timer()
  Select Case CX_action
    Case 1:
      If Crane.RotY <= 76 Then
        CX_Action = 2
      End If
      Crane.Roty = Crane.Roty - 1
    Case 2:
      If Crane.RotZ <= -5 Then
        CX_Action = 3
      End If
      Crane.RotZ = Crane.RotZ - 1
    Case 3
      If jdbp = 1 AND Nipple.RotY >= 220 AND Nipple.RotY <=250 AND bout1 = 0 AND magon = 1 Then
        Ball1.Visible = 0
        Set Cball = Crane_Kick.createball:Cball.id = 200
        BPos = 1
        Ball_Move.enabled = 1
      End If
      If jdbp1 = 1 AND Nipple.RotY >= 320 AND Nipple.RotY <=360 AND bout3 = 0  AND magon = 1 Then
        Ball2.Visible = 0
        Set Cball = Crane_Kick.createball:Cball.id = 201
        BPos = 1
        Ball_Move.enabled = 1
      End If
      If jdbp2 = 1 AND Nipple.RotY >= 100 AND Nipple.RotY <=130 AND bout2 = 0  AND magon = 1 Then
        Ball.Visible = 0
        Set Cball = Crane_Kick.createball:Cball.id = 202
        BPos = 1
        Ball_Move.enabled = 1
      End If
      CX_Action = 4
    Case 4:
      If Crane.RotZ >=0 Then
        CX_Action = 5
      End If
      Crane.RotZ = Crane.RotZ + 1
    Case 5:
      If Crane.RotY => 90 Then
        CX_Action = 6
      End If
      Crane.Roty = Crane.Roty + 1
    Case 6:
      me.enabled = 0
  End Select
End Sub

'Crane action if balls are not in deadworld holes to simulate the real search action of the crane arm on the real game.

Sub Crane2_X_Timer()
  Select Case CX2_action
    Case 1:
      If Crane.RotY <= 76 Then
        CX2_Action = 2
      End If
      Crane.Roty = Crane.Roty - 1
    Case 2:
      If Crane.RotZ <= -5 Then
        CX2_Action = 3
      End If
      Crane.RotZ = Crane.RotZ - 1
    Case 3
      CX2_Action = 4
    Case 4:
      If Crane.RotZ >=0 Then
        CX2_Action = 5
      End If
      Crane.RotZ = Crane.RotZ + 1
    Case 5:
      If Crane.RotY => 90 Then
        CX2_Action = 6
      End If
      Crane.Roty = Crane.Roty + 1
    Case 6:
      Planet_Stub.enabled = 1
      me.enabled = 0
  End Select
End Sub

Sub Planet_Stub_Timer()
  Planet_Watch.enabled = 1
  me.enabled = 0
End Sub

'Ball move logic to simulate ball being picked up and exiting deadworld holes.

Dim Bpos, Cball

Sub Ball_Move_Timer()
  Select Case Bpos
    Case 1:
      If Cball.Z => 230 Then
        BPos = 2
      End If
      Cball.Z = Cball.Z + 7
    Case 2:
      If Cball.X <= 70 Then
        Bpos = 4
      End If
      Cball.X = Cball.X - 8.5
    Case 3:
      If Cball.Z <= 180 Then
        Bpos = 4
      End If
      Cball.Z = Cball.Z - 7
    Case 4:
      If jdbp = 1 AND Nipple.RotY >= 220 AND Nipple.RotY <=250 AND magon = 1 Then
        Crane_Kick.kick 180,0
        jdbp = 0
        bout1 = 1
      End If
      If jdbp1 = 1 AND Nipple.RotY >= 320 AND Nipple.RotY <=360 AND magon = 1 Then
        Crane_Kick.kick 180,0
        jdbp1 = 0
        bout3 = 1
      End If
      If jdbp2 = 1 AND Nipple.RotY >= 100 AND Nipple.RotY <=130 AND magon = 1 Then
        Crane_Kick.kick 180,0
        jdbp2 = 0
        bout2 = 1
      End If
      Planet_Watch.enabled = 1
      me.enabled = 0
  End Select
End Sub

Sub JDFlip_Timer()
  lfs.RotZ = LeftFlipper.CurrentAngle
  rfs.RotZ = RightFlipper.CurrentAngle
  LeftFlipperP.RotY = LeftFlipper.CurrentAngle
  LeftFlipperP2.RotY = LeftFlipper2.CurrentAngle
  RightFlipperP.RotY = RightFlipper.CurrentAngle
  RightFlipperP2.RotY = RightFlipper2.CurrentAngle

  If L81.State = 1 Then
    Frr.opacity=400
  else
    Frr.opacity=120
  End If

  If L82.State = 1 Then
    Ftc.opacity=400
  else
    Ftc.opacity=120
  End If

  If L58.State = 1 Then
    Fsc.opacity=300
  else
    Fsc.opacity=100
  End If
BallShadowUpdate
End Sub

Sub diag_timer()
  If ballsinhole > 0 Then
    bsLeftPopper.ExitSol_ON
  End If
End Sub

'Populate The Trough on Game Launch.

Dim tball
tball = 1

Sub load_trough_timer()
  Select Case tball
    Case 1:Kicker_Load.createball:Kicker_Load.kick 45,10:tball = 2
    Case 2:Kicker_Load.createball:Kicker_Load.kick 45,10:tball = 3
    Case 3:Kicker_Load.createball:Kicker_Load.kick 45,10:tball = 4
    Case 4:Kicker_Load.createball:Kicker_Load.kick 45,10:tball = 5
    Case 5:Kicker_Load.createball:Kicker_Load.kick 45,10:tball = 6
    Case 6:Kicker_Load.createball:Kicker_Load.kick 45,10:tball = 7
    Case 7:me.enabled = 0
  End Select
End Sub


'Trough switches.

Sub SW86_Hit()
  W86.isdropped = 0
  Controller.Switch(86) = 1
End Sub

Sub SW86_Unhit()
  W86.isdropped = 1
  Controller.Switch(86) = 0
End Sub

Sub SW85_Hit()
  W85.isdropped = 0
  Controller.Switch(85) = 1
End Sub

Sub SW85_Unhit()
  W85.isdropped = 1
  Controller.Switch(85) = 0
End Sub

Sub SW84_Hit()
  W84.isdropped = 0
  Controller.Switch(84) = 1
End Sub

Sub SW84_Unhit()
  W84.isdropped = 1
  Controller.Switch(84) = 0
End Sub

Sub SW83_Hit()
  W83.isdropped = 0
  Controller.Switch(83) = 1
End Sub

Sub SW83_Unhit()
  W83.isdropped = 1
  Controller.Switch(83) = 0
End Sub

Sub SW82_Hit()
  W82.isdropped = 0
  Controller.Switch(82) = 1
End Sub

Sub SW82_Unhit()
  W82.isdropped = 1
  Controller.Switch(82) = 0
End Sub

Sub SW81_Hit()
  Controller.Switch(81) = 1
End Sub

Sub SW81_Unhit()
  Controller.Switch(81) = 0
End Sub


'Debug Stuff (Timer not enabled)

Sub planet_diag_timer()
  If jdbp = 1 Then
    Light1.State = 1
  else
    Light1.State = 0
  End If

  If jdbp1 = 1 Then
    Light2.State = 1
  else
    Light2.State = 0
  End If

  If jdbp2 = 1 Then
    Light3.State = 1
  else
    Light3.State = 0
  End If

  If magon = 1 Then
    Light4.State = 1
  else
    Light4.State = 0
  End If

  If bout1 = 1 Then
    Light5.State = 1
  else
    Light5.State = 0
  End If

  If bout3 = 1 Then
    Light6.State = 1
  else
    Light6.State = 0
  End If

  If bout2 = 1 Then
    Light7.State = 1
  else
    Light7.State = 0
  End If
End Sub

Sub DOF(dofevent, dofstate)
	If cController = 3 Then
		If dofstate = 2 Then
			Controller.B2SSetData dofevent, 1:Controller.B2SSetData dofevent, 0
		Else
			Controller.B2SSetData dofevent, dofstate
		End If
	End If
End Sub

Sub Table1_Exit
  Controller.Stop
End Sub
