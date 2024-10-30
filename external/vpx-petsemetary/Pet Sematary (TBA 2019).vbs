Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

Const cGameName="dracula"
'Const cGameName="draculfp" 'FREE PLAY ROM

LoadVPM "01530000","BALLY.VBS",3.1
'LoadVPM "01530000","STERN.VBS",3.1

Const UseSolenoids=1
Const UseLamps=1
Const UseGI=0

' Standard Sounds
Const SSolenoidOn="fx_solenoid",SSolenoidOff="fx_solenoidoff",SCoin="fx_coin",SFlipperOn="",SFlipperOff=""

Dim bsTrough,bsSaucer,dtL,dtT,HiddenValue

'*********** Desktop/Cabinet settings ************************

If Table1.ShowDT = true Then 
	HiddenValue = 0
Else
	HiddenValue = 1
End If

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):RightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart
     End If
End Sub

'Solenoids
SolCallback(1)="bell10"
SolCallback(2)="bell100"
SolCallback(3)="bell1000"
SolCallback(4)="bell10"
SolCallback(5)="SolLeftTargetReset"
SolCallback(6)="Solknocker"
SolCallback(7)="bsTrough.SolOut"
SolCallback(9)="bsSaucer.SolOut"
SolCallback(10)="SolTopTargetReset"
'SolCallback(11)="vpmSolSound ""jet3"","	'bumper1
'SolCallback(12)="vpmSolSound ""jet3"","	'bumper2
'SolCallback(13)="vpmSolSound ""jet3"","	'bumper3
'SolCallback(14)="vpmSolSound ""sling"","	'right sling
'SolCallback(15)="vpmSolSound ""sling"","	'left sling
SolCallback(19)="vpmNudge.SolGameOn"

SolCallback(sLLFlipper)="SolLFlipper"
SolCallback(sLRFlipper)="SolRFlipper"

'Sub FlipperTimer_Timer
'	LFLogo1.RotY=LeftFlipper.CurrentAngle
'	LFLogo2.RotY=LeftFlipper1.CurrentAngle
'	RFLogo1.RotY=RightFlipper.CurrentAngle
'End Sub

Sub bell10(enabled)
	If enabled Then 
	PlaySound "fx3_em_bell10"
	Else
	StopSound "fx3_em_bell10"
	End If
End Sub

Sub bell100(enabled)
	If enabled Then 
	PlaySound "fx3_em_bell100"
	Else
	StopSound "fx3_em_bell100"
	End If
End Sub

Sub bell1000(enabled)
	If enabled Then 
	PlaySound "fx3_em_bell1000"
	Else
	StopSound "fx3_em_bell1000"
	End If
End Sub

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Pet Sematary (TBA 2019)"&chr(13)&"(by IVANTBA)"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.Games(cGameName).Settings.Value("sound")=0    'TURNS OFF ROM SOUNDS.
		.ShowTitle=0
        .Hidden = HiddenValue      
		.Run
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With

PlaySound "Intro",-0
PlaySound "Intro 2",-0

	PinmameTimer.Interval=PinMameInterval
	PinmameTimer.Enabled=1

	Set bsTrough=New cvpmBallstack
	with bsTrough
		.InitSw 0,8,0,0,0,0,0,0
'		.InitNoTrough BallRelease,8,125,3
		.InitKick BallRelease,90,7
		.InitExitSnd Soundfx("fx_ballrel",DOFContactors), Soundfx("fx_solenoid",DOFContactors)
		.Balls=1
	end with
	
	Set bsSaucer=New cvpmBallStack
	with bsSaucer
		.InitSaucer sw40,40,165,8
		.InitExitSnd Soundfx("fx2_kicker",DOFContactors), Soundfx("fx_solenoid",DOFContactors)
		.CreateEvents "bsSaucer", sw40 
 	end with

	Set dtL=New cvpmDropTarget
	dtL.InitDrop Array(sw29,sw21,sw37),Array(29,21,37)
	dtL.InitSnd SoundFX("fx2_droptarget",DOFContactors),SoundFX("fx2_DTReset",DOFContactors)

	Set dtT=New cvpmDropTarget
	dtT.InitDrop Array(sw12,sw20,sw28,sw36),Array(12,20,28,36)
	dtT.InitSnd SoundFX("fx2_droptarget2",DOFContactors),SoundFX("fx2_DTReset",DOFContactors)


'*****Drop Lights Off
	dim xx

	For each xx in DTLeftLights: xx.state=0:Next
	For each xx in DTTopLights: xx.state=0:Next

	GILights 1	

	vpmNudge.TiltSwitch=7
	vpmNudge.Sensitivity=5
	vpmNudge.TiltObj=Array(sw18,sw33,sw34,sw35,LeftSlingshot,RightSlingshot)
End Sub

Sub GILights (enabled)
	Dim light
	For each light in GI:light.State = Enabled: Next
End Sub

Sub Table1_KeyDown(ByVal keycode)
	If keycode = LeftTiltKey Then Nudge 90, 2
	If keycode = RightTiltKey Then Nudge 270, 2
	If keycode = CenterTiltKey Then	Nudge 0, 2

	If vpmKeyDown(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.PullBack: PlaySound "fx_plungerpull",0,1,0.25,0.25: 	End If
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then Plunger.Fire: PlaySound "fx_plunger",0,1,0.25,0.25
	If vpmKeyUp(keycode) Then Exit Sub

End Sub

Sub ShooterLane_Hit
End Sub

Sub BallRelease_UnHit	
End Sub

Sub Drain_Hit()
	PlaySound "fx2_drain2",0,1,0,0.25 : bstrough.addball me
End Sub

Sub sw40_Hit():PlaySound "fx_hole-enter": Controller.Switch(40) = 1:End Sub 'Left Kicker

'Drop Targets
 Sub Sw12_Dropped:dtT.Hit 1 : TDropA1.state=1 : End Sub  
 Sub Sw20_Dropped:dtT.Hit 2 : TDropA2a.state=1 : TDropA2b.state=1 : End Sub  
 Sub Sw28_Dropped:dtT.Hit 3 : TDropA3.state=1 : TDropB3.state=1 : End Sub
 Sub Sw36_Dropped:dtT.Hit 4 : TDropB4.state=1 : End Sub

 Sub Sw29_Dropped:dtL.Hit 1 : LDrop3.state=1 : End Sub  
 Sub Sw21_Dropped:dtL.Hit 2 : LDrop2.state=1 : End Sub
 Sub Sw37_Dropped:dtL.Hit 3 : LDrop1.state=1 : End Sub

Sub SolTopTargetReset(enabled)
	dim xx
	if enabled then
		dtT.SolDropUp enabled
		For each xx in DTTopLights: xx.state=0:Next
	end if
End Sub

Sub SolLeftTargetReset(enabled)
	dim xx
	if enabled then
		dtL.SolDropUp enabled
		For each xx in DTLeftLights: xx.state=0:Next
	end if
End Sub

'**********
' Music
'**********

'There are two types of music to choose from: (simply rename and replace the mp3 files) 

'OST "Pet Sematary ost music.mp3" 
'THEME Music "Pet Sematary theme music.mp3"

'Start the music

DIM music

music = "Pet Sematary music.mp3"
PlayMusic "Pet Sematary music.mp3" 

Sub Table1_MusicDone()
PlayMusic "Pet Sematary music.mp3"
End Sub 

'Bumpers

Sub sw18a_Hit : vpmTimer.PulseSw 18 : End Sub
Sub sw33_Hit : vpmTimer.PulseSw 33 : playsound SoundFX("fx2_bumper_1",DOFContactors): End Sub
Sub sw34_Hit : vpmTimer.PulseSw 34 : playsound SoundFX("fx2_bumper_2",DOFContactors): End Sub
Sub sw35_Hit : vpmTimer.PulseSw 35 : playsound SoundFX("fx2_bumper_3",DOFContactors): End Sub

'Rollovers
Sub SW17_Hit:Controller.Switch(17)=1 : End Sub
Sub SW17_unHit:Controller.Switch(17)=0:End Sub	
Sub SW17a_Hit:Controller.Switch(17)=1 : End Sub 
Sub SW17a_unHit:Controller.Switch(17)=0:End Sub
Sub SW17b_Hit:Controller.Switch(17)=1 : End Sub
Sub SW17b_unHit:Controller.Switch(17)=0:End Sub
Sub SW17c_Hit:Controller.Switch(17)=1 : End Sub
Sub SW17c_unHit:Controller.Switch(17)=0:End Sub
Sub SW32_Hit:Controller.Switch(32)=1 : End Sub
Sub SW32_unHit:Controller.Switch(32)=0:End Sub	

'Wire Triggers
Sub SW1_Hit:Controller.Switch(1)=1 : End Sub
Sub SW1_unHit:Controller.Switch(1)=0:End Sub
Sub SW2_Hit:Controller.Switch(2)=1 : End Sub
Sub SW2_unHit:Controller.Switch(2)=0:End Sub
Sub SW3_Hit:Controller.Switch(3)=1 : End Sub
Sub SW3_unHit:Controller.Switch(3)=0:End Sub
Sub SW4_Hit:Controller.Switch(4)=1 : End Sub
Sub SW4_unHit:Controller.Switch(4)=0:End Sub

'Leaf Switches
Sub SW19_Hit:Controller.Switch(19)=1 : End Sub
Sub SW19_unHit:Controller.Switch(19)=0:End Sub
Sub SW19a_Hit:Controller.Switch(19)=1 : End Sub
Sub SW19a_unHit:Controller.Switch(19)=0:End Sub

'Spinners
Sub sw15_Spin : vpmTimer.PulseSw (15) :PlaySound "fx_spinner": End Sub

'Targets
Sub sw5_Hit:vpmTimer.PulseSw (5):End Sub
Sub sw18_Hit:vpmTimer.PulseSw (18):End Sub
Sub sw22_Hit:vpmTimer.PulseSw (22):End Sub
Sub sw23_Hit:vpmTimer.PulseSw (23):End Sub
Sub sw24_Hit:vpmTimer.PulseSw (24):End Sub

Sub SolKnocker(Enabled)
	If Enabled Then PlaySound SoundFX("Knocker",DOFKnocker)
End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep, AStep, BStep, RWall1Step, RWall2Step

Sub RightSlingShot_Slingshot
    PlaySound SoundFX("fx_slingshot",DOFContactors), 0, 1, 0.05, 0.05
	vpmtimer.PulseSw(38)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing3.Visible = 1:sling1.TransZ = 0
        Case 5:RSLing3.Visible = 0:RSLing.Visible = 1:RightSlingShot.TimerEnabled = 0 
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("fx_slingshot",DOFContactors),0,1,-0.05,0.05
	vpmtimer.pulsesw(39)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing3.Visible = 1:sling2.TransZ = 0
        Case 5:LSLing3.Visible = 0:LSLing.Visible = 1:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RubberWall1_Slingshot
    rubber1.Visible = 0
    rubber1a.Visible = 1
    RWall1Step = 0
    RubberWall1.TimerEnabled = 1
End Sub

Sub RubberWall1_Timer
    Select Case RWall1Step
        Case 3:rubber1a.Visible = 0:rubber1b.Visible = 1
        Case 4:rubber1b.Visible = 0:rubber1.Visible = 1:RubberWall1.TimerEnabled = 0
    End Select
    RWall1Step = RWall1Step + 1
End Sub

Sub RubberWall2_Slingshot
    rubber2.Visible = 0
    rubber2a.Visible = 1
    RWall2Step = 0
    RubberWall2.TimerEnabled = 1
End Sub

Sub RubberWall2_Timer
    Select Case RWall2Step
        Case 3:rubber2a.Visible = 0:rubber2b.Visible = 1
        Case 4:rubber2b.Visible = 0:rubber2.Visible = 1:RubberWall2.TimerEnabled = 0
    End Select
    RWall2Step = RWall2Step + 1
End Sub

Sub sw19_Slingshot
    rubber4.Visible = 0
    rubber4a.Visible = 1
    AStep = 0
    sw19.TimerEnabled = 1
End Sub

Sub sw19_Timer
    Select Case AStep
        Case 3:rubber4a.Visible = 0:rubber4b.Visible = 1
        Case 4:rubber4b.Visible = 0:rubber4.Visible = 1:sw19.TimerEnabled = 0
    End Select
    AStep = AStep + 1
End Sub

Sub sw19a_Slingshot
    rsling5.Visible = 0
    rsling5a.Visible = 1
    BStep = 0
    sw19a.TimerEnabled = 1
End Sub

Sub sw19a_Timer
    Select Case BStep
        Case 3:rsling5a.Visible = 0:rsling5b.Visible = 1
        Case 4:rsling5b.Visible = 0:rsling5.Visible = 1:sw19a.TimerEnabled = 0
    End Select
    BStep = BStep + 1
End Sub
 
'-------------------------------------
' Map lights into array
' Set unmapped lamps to Nothing
'-------------------------------------
Set Lights(1)  = l1
Set Lights(2)  = l2
Set Lights(3)  = l3
Set Lights(4)  = l4
Set Lights(5)  = l5
Set Lights(6)  = l6
Set Lights(7)  = l7
Set Lights(8)  = l8
'Set Lights(9)  = l9
'Lights(10) = Array(10,10a)
Set Lights(11) = l11
'Set Lights(12) = l12
'Set Lights(13) = l13 'Ball In Play
'Set Lights(14) = l14
'Set Lights(15) = l15
'Set Lights(16) = unused
Set Lights(17) = l17
Set Lights(18) = l18
Set Lights(19) = l19
Set Lights(20) = l20
Set Lights(21) = l21
Set Lights(22) = l22
Set Lights(23) = l23
Set Lights(24) = l24
'Set Lights(25) = l25
'Set Lights(26) = l26
'Set Lights(27) = l27 'LightMatch
Set Lights(28) = l28
'Set Lights(29) = l29 'LightHighScore
'Set Lights(30) = l30
'Set Lights(31) = l31
'Set Lights(32) = unused
Set Lights(33) = l33
'Set Lights(34) = l34
Set Lights(35) = l35
'Set Lights(36) = l36
'Set Lights(37) = l37
'Set Lights(38) = l38
'Set Lights(39) = l39
Set Lights(40) = l40
'Set Lights(41) = l41
'Set Lights(42) = l42
Set Lights(43) = l43
'Set Lights(44) = l44
'Set Lights(45) = l45 'LightGameOver
''Set Lights(46) = l46
'Set Lights(47) = l47
'Set Lights(48) = unused
Set Lights(49) = l49
'Set Lights(50) = l50
Set Lights(51) = l51
Set Lights(52) = l52
Set Lights(53) = l53
Set Lights(54) = l54
Set Lights(55) = l55
Set Lights(56) = l56
'Set Lights(57) = l57
'Set Lights(58) = l58
Set Lights(59) = l59 'LightCredit
Set Lights(60) = l60
'Set Lights(61) = l61 'LightTilt
'Set Lights(62) = l62
'Set Lights(63) = l63
 
Set vpmShowDips = GetRef("editDips")

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 1000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
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

'*****************************************
'			FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperLSh1.RotZ = LeftFlipper1.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

End Sub

'*****************************************
'			BALL SHADOW
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
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 13
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 13
        End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

'Sub aBumpers_Hit (idx): PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, pan(ActiveBall): End Sub 
Sub aRollovers_Hit(idx):PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aMetals_Hit(idx):PlaySound "fx_MetalHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx2_resetdrop", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubbers_Hit(idx):PlaySound "fx2_postrubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx2_postrubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx2_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aTargets_Hit(idx):PlaySound "fx2_target", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub

'Stern Dracula
 'added by Inkochnito
 Sub editDips
 Dim vpmDips : Set vpmDips = New cvpmDips
 With vpmDips
 .AddForm 700,400,"Dracula - DIP switches"
 .AddChk 2,10,180,Array("Match feature",&H00100000)'dip 21
 .AddChk 205,10,115,Array("Credits display",&H00080000)'dip 20
 .AddFrame 2,30,190,"Maximum credits",&H00070000,Array("5 credits",0,"10 credits",&H00010000,"15 credits",&H00020000,"20 credits",&H00030000,"25 credits",&H00040000,"30 credits",&H00050000,"35 credits",&H00060000,"40 credits",&H00070000)'dip 17&18&19
 .AddFrame 2,160,190,"Special award",&HC0000000,Array("100,000 points",0,"free ball",&H40000000,"free game",&H80000000,"free ball and free game",&HC0000000)'dip 31&32
 .AddFrame 2,235,190,"High score to date",49152,Array("no award",0,"1 credit",&H00004000,"2 credits",32768,"3 credits",49152)'dip 15&16
 .AddFrame 2,310,120,"High score feature",&H00000020,Array("extra ball",0,"replay",&H00000020)'dip 6
 .AddFrame 205,30,190,"Balls per game",&H00000040,Array("3 balls",0,"5 balls",&H00000040)'dip 7
 .AddFrame 205,76,190,"Special",&H00200000,Array("unlimited specials",0,"1 special per ball",&H00200000)'dip 22
 .AddFrame 205,122,190,"Sound types",&H00400000,Array ("electronic chimes",0,"computer type sounds",&H00400000)'dip 23
 .AddFrame 205,168,190,"Bottom lane special",&H00800000,Array("unlimited specials",0,"1 special per ball",&H00800000)'dip 24
 .AddFrame 205,215,190,"Extra ball lane on after",&H01000000,Array("spotting 4 cats",0,"spotting 4 cat and 4 bats",&H01000000)'dip 25
 .AddFrame 205,263,190,"Bonus advances bottom lane",&H02000000,Array("2 advances",0,"3 advances",&H02000000)'dip 26
 .AddFrame 140,310,120,"Extra ball lite",&H04000000,Array("stays on",0,"alternates",&H04000000)'dip 27
 .AddFrame 275,310,120,"Music option",&H00000080,Array("tunes",0,"melody",&H00000080)'dip 8
 .AddLabel 50,382,300,20,"After hitting OK, press F3 to reset game with new settings."
 .ViewDips
 End With
 End Sub
 Set vpmShowDips = GetRef("editDips")

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub