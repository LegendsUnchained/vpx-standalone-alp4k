'****************************************************************************************
' FIREBALL
' Bally 1972
' version 1.1
' VPX EM Table Recreation by pinball58
' DOF by Arngrim
'****************************************************************************************

Option Explicit
Randomize

Const cGameName = "Fireball_1972"   

Dim Zipper
Dim KickIP
Dim Dir
Dim Strong
Dim Player
Dim Score(4)
Dim EMR(4)
Dim x
Dim Odin
Dim Wotan
Dim Tilt
Dim BarrierPos
Dim Credit
Dim Game
Dim BallN,BallN2,BallN3,BallN4
Dim Replay(4)
Dim ReplayScore1(4)
Dim ReplayScore2(4)
Dim ReplayScore3(4)
Dim TiltCount
Dim MatchNum
Dim Match
Dim MatchP(4)
Dim PlayerN
Dim AllowStart
Dim Init
Dim BallsNTot
Dim BallIP
Dim PUP
Dim OWBalls
Dim LButton
Dim RButton

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

ExecuteGlobal GetTextFile("core.vbs") : If Err Then MsgBox "Can't open ""core.vbs"""

If Fireball.ShowDT = false then
	EmReel1.Visible = false
	EMReel2.Visible = false
	EMReel3.Visible = false
	EMReel4.Visible = false
	P1up.Visible = false
	P2up.Visible = false
	P3up.Visible = false
	P4up.Visible = false
	Ramp16.Visible = false
	Ramp17.Visible = false
	Ramp1.Visible = false
	CreditsOsd.Visible = false
	CreditCount.Visible = false
	PlayerOsd.Visible = false
	PlayerPlay.Visible = false
	CanPlay.Visible = false 
	PlayerNum.Visible = false 
	BallsOsd.Visible = false 
	BallCount.Visible = false 
	m0.Visible = false : m1.Visible = false : m2.Visible = false : m3.Visible = false : m4.Visible = false : m5.Visible = false : m6.Visible = false : m7.Visible = false : m8.Visible = false : m9.Visible = false
	TILTtb.Visible = false 
	GameOvertb.Visible = false
	Light43.state=1
	Light44.state=1
End If

'********** Table Init ***********

Sub Fireball_Init
	LoadEM
	Kicker3.CreateBall
	Kicker3.Kick 90, 1
	Replay(1) = 1 : Replay(2) = 1 : Replay(3) = 1 : Replay(4) = 1 
	Game = 0
	AllowStart = 1
	Set EMR(1) = EMReel1
	Set EMR(2) = EMReel2
	Set EMR(3) = EMReel3
	Set EMR(4) = EMReel4
	ReplayScore1(1) = 56000 : ReplayScore2(1) = 70000 : ReplayScore3(1) = 82000
	ReplayScore1(2) = 56000 : ReplayScore2(2) = 70000 : ReplayScore3(2) = 82000
	ReplayScore1(3) = 56000 : ReplayScore2(3) = 70000 : ReplayScore3(3) = 82000
    ReplayScore1(4) = 56000 : ReplayScore2(4) = 70000 : ReplayScore3(4) = 82000
	Zipper=0
	BarrierPos=0
	OWBalls=0
	Wall80.IsDropped=true
	Wall81.IsDropped=true
	Wall90.IsDropped=true
	Wall91.IsDropped=true
End Sub

'*********************************

'********** InGame Updates ***********

Sub flipperTimer_timer()
	FlipperL.RotY = LeftFlipper.CurrentAngle+90
	FlipperL1.RotY = LeftFlipper1.CurrentAngle+90
	FlipperR.RotY = RightFlipper.CurrentAngle+90
	FlipperR1.RotY = RightFlipper1.CurrentAngle+90
	If Zipper=0 Then FlipperL1.visible=0 : FlipperL7.visible=0 : FlipperR1.visible=0 : FlipperR7.visible=0 End If
	If Zipper=1 Then FlipperL.visible=0 : FlipperL8.visible=0 : FlipperR.visible=0 : FlipperR8.visible=0 End If
	GWL.RotZ=Gate3.CurrentAngle+15
	GWL1.RotZ=Gate2.CurrentAngle+15
	GWL3.RotZ=Gate4.CurrentAngle
End Sub

Sub Trigger42_Hit
	GWL2.RotZ=70
	GateTimer.enabled=1
End Sub

Sub GateTimer_timer()
	GWL2.RotZ=46
End Sub

'**************************************

'********* Add Score & Replay ****************************************

Sub AddScore(points)

If Player = 1 Then x = 1 End If
If Player = 2 Then x = 2 End If
If Player = 3 Then x = 3 End If
If Player = 4 Then x = 4 End If

Score(x) = Score(x) + points

EMR(x).setvalue(score(x))

If B2SOn Then
Controller.B2SSetScorePlayer 1, Score(1)
Controller.B2SSetScorePlayer 2, Score(2)
Controller.B2SSetScorePlayer 3, Score(3)
Controller.B2SSetScorePlayer 4, Score(4)
End If

If points = +10 Then MatchP(x) = MatchP(x) + 10 End If
If MatchP(x) = 100 Then MatchP(x) = 0 End If

If Score(x) >= ReplayScore1(x) And Replay(x) = 1 Then
	If Credit <25 Then
	Credit = Credit + 1
	DOF 122, DOFOn
	CreditCount.text= (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
	Replay(x)=2
End If

If Score(x) >= (ReplayScore1(x)*2) And Replay(x) = 2 Then
	If Credit <25 Then
	Credit = Credit + 1
	DOF 122, DOFOn
	CreditCount.text= (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
	Replay(x)=3
End If

If Score(x) >= (ReplayScore1(x)*3) And Replay(x) = 3 Then
	If Credit <25 Then
	Credit = Credit + 1
	DOF 122, DOFOn
	CreditCount.text= (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
	Replay(x)=4
End If

If Score(x) >= (ReplayScore2(x)) And Replay(x) = 5 Then
	If Credit <25 Then
	Credit = Credit + 1
	DOF 122, DOFOn
	CreditCount.text= (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
	Replay(x)=6
End If

If Score(x) >= (ReplayScore2(x)*2)  And Replay(x) = 6 Then
	If Credit <25 Then
	Credit = Credit + 1
	DOF 122, DOFOn
	CreditCount.text= (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
	Replay(x)=7
End If

If Score(x) >= (ReplayScore2(x)*3) And Replay(x) = 7 Then
	If Credit <25 Then
	Credit = Credit + 1
	DOF 122, DOFOn
	CreditCount.text= (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
	Replay(x)=8
End If

If Score(x) >= (ReplayScore3(x)) And Replay(x) = 10 Then
	If Credit <25 Then
	Credit = Credit + 1
	DOF 122, DOFOn
	CreditCount.text= (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
	Replay(x)=11
End If

If Score(x) >= (ReplayScore3(x)*2) And Replay(x) = 11 Then
	If Credit <25 Then
	Credit = Credit + 1
	DOF 122, DOFOn
	CreditCount.text= (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
	Replay(x)=12
End If

If Score(x) >= (ReplayScore3(x)*3) And Replay(x) = 12 Then
	If Credit <25 Then
	Credit = Credit + 1
	DOF 122, DOFOn
	CreditCount.text= (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
	Replay(x)=0
End If

'**** Bells Sound Score based ***

If points = 10 And (Score(x) MOD 100)\10 = 0 Then 
	PlaySound SoundFXDOF("bell_100-1000",142,DOFPulse,DOFChimes),0,0.7
    ElseIf points = 1000 Then 
	PlaySound SoundFXDOF("bell_100-1000",142,DOFPulse,DOFChimes),0,0.7
	ElseIf points = 100 Then 
	PlaySound SoundFXDOF("bell_100-1000",142,DOFPulse,DOFChimes),0,0.7
	ElseIf points = 10 Then 
	PlaySound SoundFXDOF("bell_10",141,DOFPulse,DOFChimes)
End If

'*********************************

End Sub

'*******************************************************************

'********* Match **************

Sub MatchTimer_timer()
	
MatchNum = Int(Rnd*10)+1
Select Case MatchNum
    Case 1 : Match = 0 
    Case 2 : Match = 10 
	Case 3 : Match = 20 
	Case 4 : Match = 30 
	Case 5 : Match = 40 
	Case 6 : Match = 50 
	Case 7 : Match = 60 
	Case 8 : Match = 70 
	Case 9 : Match = 80
	Case 10 : Match = 90
End Select

If Match = 0 Then m0.text = "00" End If
If Match = 10 Then m1.text = "10" End If
If Match = 20 Then m2.text = "20" End If
If Match = 30 Then m3.text = "30" End If
If Match = 40 Then m4.text = "40" End If
If Match = 50 Then m5.text = "50" End If
If Match = 60 Then m6.text = "60" End If
If Match = 70 Then m7.text = "70" End If
If Match = 80 Then m8.text = "80" End If
If Match = 90 Then m9.text = "90" End If
If B2SOn Then
If Match = 0 Then
Controller.B2SSetMatch 34, 100
Else
Controller.B2SSetMatch 34, Match
End If
End If
MatchTimer.enabled = 0
CheckMatch
P1up.text = "" : P2up.text = "" : P3up.text = "" : P4up.text = ""
If B2SOn Then
Controller.B2SSetPlayerUp 30, 0
Controller.B2SSetData "p1",0
Controller.B2SSetData "p2",0
Controller.B2SSetData "p3",0
Controller.B2SSetData "p4",0
End If

End Sub

Sub CheckMatch

If PlayerN = 1 Then
	If Match = MatchP(1) Then 
	PlaySound "knocker"
	If Credit<25 Then 
	Credit = Credit+1 
	DOF 122, DOFOn
	CreditCount.text = (Credit) 
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	End If
	GameOverTimer.enabled = 1
	PlayerN = 0
	Player = 0
	Init= 0
End If

If PlayerN = 2 Then
	If Match = MatchP(1) Then 
	PlaySound "knocker"
	If Credit<25 Then 
	Credit = Credit+1 
	DOF 122, DOFOn
	CreditCount.text = (Credit) 
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	End If
	MatchTimer2.enabled = 1
End If

If PlayerN = 3 Then
	If Match = MatchP(1) Then 
	PlaySound "knocker"
	If Credit<25 Then 
	Credit = Credit+1 
	DOF 122, DOFOn
	CreditCount.text = (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	End If
	MatchTimer2.enabled = 1
	MatchTimer3.enabled = 1
End If

If PlayerN = 4 Then
	If Match = MatchP(1) Then 
	PlaySound "knocker"
	If Credit<25 Then 
	Credit = Credit+1 
	DOF 122, DOFOn
	CreditCount.text = (Credit) 
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	End If
	MatchTimer2.enabled = 1
	MatchTimer3.enabled = 1
	MatchTimer4.enabled = 1
End If

End Sub

Sub MatchTimer2_timer()
	If Match = MatchP(2) Then 
	PlaySound "knocker"
	If Credit<25 Then 
	Credit = Credit+1 
	DOF 122, DOFOn
	CreditCount.text = (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	End If
	MatchTimer2.enabled = 0
	If PlayerN = 2 Then 
	GameOverTimer.enabled = 1 
	PlayerN = 0
	Player = 0
	Init= 0
	End If
End Sub

Sub MatchTimer3_timer()
	If Match = MatchP(3) Then 
	PlaySound "knocker"
	If Credit<25 Then 
	Credit = Credit+1
	DOF 122, DOFOn
	CreditCount.text = (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	End If
	MatchTimer3.enabled = 0
	If PlayerN = 3 Then
	GameOverTimer.enabled = 1
	PlayerN = 0
	Player = 0
	Init= 0
	End If 
End Sub

Sub MatchTimer4_timer()
	If Match = MatchP(4) Then 
	PlaySound "knocker"
	If Credit<25 Then 
	Credit = Credit+1 
	DOF 122, DOFOn
	CreditCount.text = (Credit) 
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	End If
	MatchTimer4.enabled = 0
	GameOverTimer.enabled = 1
	PlayerN = 0
	Player = 0
	Init= 0
End Sub

'******************************

'********* Table Keys **********

Sub Fireball_KeyDown(ByVal keycode)

	If keycode = AddCreditKey And Credit<25 Then
	PlaySound "CoinIn"
	Credit = Credit+1
	DOF 122, DOFOn
	CreditCount.text= (Credit)
	If B2SOn Then
	Controller.B2SSetCredits Credit
	End If
	End If
	If keycode = AddCreditKey And Credit >=25 Then 
	PlaySound "CoinIn"
	End If

	If keycode = StartGameKey And Credit>0 And PlayerN <4 And AllowStart = 1 Then
	PlayerN = PlayerN + 1
	Credit = Credit-1
	If Credit < 1 Then DOF 122, DOFOff
	Init = Init + 1
	If Init = 1 Then
	FirstStartTimer.enabled = 1	
	StartupTimer.enabled = 1
	SpinDisc.enabled=1
	DiscNoise.enabled=1
	DiscNoise2.enabled=1
	End If
	PlaySound "start"
	CreditCount.text= (Credit)
	Game = 1
	BallN= 5
	BallN2= 5
	BallN3= 5
	BallN4= 5
	If PlayerN = 1 Then 
	BallsNTot = 5
	If Fireball.ShowDT = true Then 
	EMReel2.visible = false
	EMReel3.visible = false
	EMReel4.visible = false
	End If
	If B2SOn Then 
	Controller.B2SSetData "p1",1
	Controller.B2SSetData "p2",0
	Controller.B2SSetData "p3",0
	Controller.B2SSetData "p4",0
	End If
	End If
	If PlayerN = 2 Then 
	BallsNTot = 10 
	If Fireball.ShowDT = true then 
	EMReel2.visible = true
	EMReel3.visible = false
	EMReel4.visible = false 
	End If
	If B2SOn Then 
	Controller.B2SSetData "p1",1
	Controller.B2SSetData "p2",1
	Controller.B2SSetData "p3",0
	Controller.B2SSetData "p4",0
	End If
	End If
	If PlayerN = 3 Then 
	BallsNTot = 15 
	If Fireball.ShowDT = true then 
	EMReel2.visible = true
	EMReel3.visible = true
	EMReel4.visible = false 
	End If
	If B2SOn Then 
	Controller.B2SSetData "p1",1
	Controller.B2SSetData "p2",1
	Controller.B2SSetData "p3",1
	Controller.B2SSetData "p4",0
	End If
	End If
	If PlayerN = 4 Then
	BallsNTot = 20
	If Fireball.ShowDT = true then 
	EMReel2.visible = true 
	EMReel3.visible = true 
	EMReel4.visible = true 
	End If
	If B2SOn Then 
	Controller.B2SSetData "p1",1
	Controller.B2SSetData "p2",1
	Controller.B2SSetData "p3",1
	Controller.B2SSetData "p4",1
	End If
	End If
	m0.text = "" : m1.text = "" : m2.text = "" : m3.text = "" : m4.text = "" : m5.text = "" : m6.text = "" : m7.text = "" : m8.text = "" : m9.text = ""
	MatchP(1) = 0
	MatchP(2) = 0
	MatchP(3) = 0
	MatchP(4) = 0
	If Replay(1) >1 Or Replay(2) >1 Or Replay(3) >1 Or Replay(4) >1 Then
	Replay(1) = 5 : Replay(2) = 5 : Replay(3) = 5 : Replay(4) = 5 
	End If
	If Replay(1) >5 Or Replay(2) >5 Or Replay(3) >5 Or Replay(4) >5 Then
	Replay(1) = 10 : Replay(2) = 10 : Replay(3) = 10 : Replay(4) = 10
	End If
	GameOvertb.text = ""
	Tilttb.text= ""
	PlayerNum.text = (PlayerN)
	If Player = 0 Then 
	PlayerPlay.text = ""
	Else
	PlayerPlay.text = (Player)
	End If
	If B2SOn Then
	Controller.B2SSetCredits Credit 
	Controller.B2SSetCanPlay 31, PlayerN
	Controller.B2SSetGameOver 0
	Controller.B2SSetMatch 34,0
	End If
	End If

	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySound "plungerpull"
	End If

	If keycode = LeftFlipperKey And Game = 1 And Tilt = False Then
		LeftFlipper.RotateToEnd
		LeftFlipper1.RotateToEnd
		PlaySound SoundFXDOF("fx_flipperup",101,DOFOn,DOFContactors), 0, 1, -0.05, 0.05
		PlaySound "buzz",-1,0.3,-1
		If Zipper=0 Then FlipLAn.enabled=1 End If   
		If Zipper=1 Then FlipL1An.enabled=1 End If   
		LButton=1
	End If
    
	If keycode = RightFlipperKey And Game = 1 And Tilt = False Then
		RightFlipper.RotateToEnd
		RightFlipper1.RotateToEnd
		PlaySound SoundFXDOF("fx_flipperup",102,DOFOn,DOFContactors), 0, 1, 0.05, 0.05
		PlaySound "buzz1",-1,0.3,1
		If Zipper=0 Then FlipRAn.enabled=1 End If  
		If Zipper=1 Then FlipR1An.enabled=1 End If  
		RButton=1
	End If
    
	If keycode = LeftTiltKey Then
		Nudge 90, 2
		Playsound "fx_nudge"
		CheckTilt
	End If
    
	If keycode = RightTiltKey Then
		Nudge 270, 2
		Playsound "fx_nudge"
		CheckTilt
	End If
    
	If keycode = CenterTiltKey Then
		Nudge 0, 2
		Playsound "fx_nudge"
		CheckTilt
	End If 

End Sub

Sub Fireball_KeyUp(ByVal keycode)

	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySound "plung"
	End If
    
	If keycode = LeftFlipperKey And Game = 1 And Tilt = False Then
		LeftFlipper.RotateToStart
		LeftFlipper1.RotateToStart
		PlaySound "fx_flipperdown", 0, 1, -0.05, 0.05
		StopSound "buzz"
		If Tilt = true Then
		FlipOff
		End If
		LButton=0
	End If
    
	If keycode = RightFlipperKey And Game = 1 And Tilt = False Then
		RightFlipper.RotateToStart
		RightFlipper1.RotateToStart
		PlaySound "fx_flipperdown", 0, 1, 0.05, 0.05
		StopSound "buzz1"
		If Tilt = true Then
		FlipOff
		End If
		RButton=0
	End If

End Sub

'****************************

'******** Drain *************

Sub Drain_Hit()
	DOF 120, DOFPulse
	If Player = 1 Then BallN = BallN -1 End If
	If Player = 2 Then BallN2 = BallN2 -1 End If
	If Player = 3 Then BallN3 = BallN3 -1 End If
	If Player = 4 Then BallN4 = BallN4 -1 End If
	BallsNTot = BallsNTot - 1
	Dim DrainSound         
	DrainSound = Int(rnd*2)+1
	Select Case DrainSound
	Case 1: PlaySound "drain1"
	Case 2: PlaySound "drain2"
	End Select	  
	Drain.DestroyBall
	AllowStart = 0
	
	If BallsNTot = 0 Then
	MatchTimer.enabled=1
	CallFlipOff.enabled=1
	Game = 0.5
	BrakeDisc.enabled=1
	BlinkingOff.enabled=1
	End If

	If BallsNtot >0 Then 
	NewBall.enabled=1
	NewBallSound.enabled=1
	End If

End Sub

Sub OWDrain_Hit()
	Dim DrainSound         
	DrainSound = Int(rnd*2)+1
	Select Case DrainSound
	Case 1: PlaySound "drain1"
	Case 2: PlaySound "drain2"
	End Select
	OWDrain.Destroyball
	OWBalls=OwBalls-1
	If OWBalls=0 Then
	OWDrain.enabled=0 
	End If
End Sub

Sub BlinkingOff_timer()
	BlinkingSC.enabled=0 : BlinkingSC1.enabled=0 : BlinkingSC2.enabled=0 : BlinkingSC3.enabled=0 : BlinkingSC4.enabled=0
	BlinkingOff.enabled=0
End Sub
	
'***************************

'****** Stop SpinDisc ***************

Sub BrakeDisc_timer()
	SpinDisc.enabled=0
	Disc.RotY = Disc.RotY+4
	CallBrakeDisc2.enabled=1
End Sub

Sub CallBrakeDisc2_timer()
	BrakeDisc2.enabled=1
	CallBrakeDisc2.enabled=0
End Sub

Sub BrakeDisc2_timer()
	BrakeDisc.enabled=0
	Disc.RotY = Disc.RotY+3
	CallBrakeDisc3.enabled=1
End Sub

Sub CallBrakeDisc3_timer()
	BrakeDisc3.enabled=1
	CallBrakeDisc3.enabled=0
End Sub

Sub BrakeDisc3_timer()
	BrakeDisc2.enabled=0
	Disc.RotY = Disc.RotY+2
	StopSound "disc_noise"
	CallBrakeDisc4.enabled=1
End Sub

Sub CallBrakeDisc4_timer()
	BrakeDisc4.enabled=1
	CallBrakeDisc4.enabled=0
End Sub

Sub BrakeDisc4_timer()
	BrakeDisc3.enabled=0
	Disc.RotY = Disc.RotY+1
	CallStopDisc.enabled=1
End Sub

Sub CallStopDisc_timer()
	StopDisc.enabled=1
	CallStopDisc.enabled=0
End Sub

Sub StopDisc_timer()
	BrakeDisc4.enabled=0
	StopSound "disc_noise2"
	StopDisc.enabled=0
End Sub

'*************************************

'*************** Start & Drain Sequential Events *******************

Sub FirstStartTimer_timer()
	PlaySound "fire_init"
	If Odin=1 Then BlinkingOn.enabled=1 End If
	If B2SOn Then
	Controller.B2SStartAnimation "bally1"
	Controller.B2SStartAnimation "bally2"
	End If
	FirstStartTimer.enabled = 0
End Sub

Sub BlinkingOn_timer()
	BlinkingSC.enabled=1
	BlinkingOn.enabled=0
End Sub

Sub	StartupTimer_timer()
	EMR(1).ResetToZero()
	EMR(2).ResetToZero()
	EMR(3).ResetToZero()
	EMR(4).ResetToZero()
	Score(1) = 0
	Score(2) = 0
	Score(3) = 0
	Score(4) = 0
	If B2SOn Then
	Controller.B2SSetScorePlayer 1, Score(1)
	Controller.B2SSetScorePlayer 2, Score(2)
	Controller.B2SSetScorePlayer 3, Score(3)
	Controller.B2SSetScorePlayer 4, Score(4)
	End If
	StartupTimer.enabled = 0
	NewBall.enabled = 1
	NewBallSound.enabled = 1
	End Sub

Sub NewBallSound_timer()
	PlaySound SoundFXDOF("release2",110,DOFPulse,DOFContactors),0,1,0,0.25
	NewBallSound.enabled=0
End Sub

Sub NewBall_timer()
	BallRelease.CreateBall
	BallRelease.Kick 80, 12
	Light23.state=0
	Kicker4.enabled=0
	Light29.state=0 : Light30.state=0 : Light31.state=0 
	Light33.state=0 : Light36.state=0 : Light39.state=0 : Light32.state=0 : Light35.state=0 : Light38.state=0
	If BarrierPos=1 Then BarrierC.enabled=1 End If
	Wall78.IsDropped=false
	Wall79.IsDropped=true
	Wall90.IsDropped=true
	Wall91.IsDropped=true
	If Zipper=1 Then
	CallZipDOF.enabled=1
	If LButton=0 Then
	UnZipTimerL.enabled=1 
	End If
	If LButton=1 Then
	UnZipTimerCL.enabled=1
	End If
	If RButton=0 Then
	UnZipTimerR.enabled=1 
	End If
	If RButton=1 Then
	UnZipTimerCR.enabled=1
	End If
	LeftFlipper1.enabled=0 : RightFlipper1.enabled=0 : LeftFlipper.enabled=1 : RightFlipper.enabled=1
	End If
	Bumper1.force = 6
	Bumper2.force = 6
	Bumper3.force = 6
	Tilt = false
	Tilttb.text = ""
	If B2SOn Then
	Controller.B2SSetTilt 33,0
	End If
	If PlayerN = 1 Or Player=PlayerN Then
	Player = 1
	Else
	Player = Player + 1
	End If
	PlayerPlay.text = (Player)
	If Player = 1 And BallN = 5 Then BallIP = 1 : P1up.text = "1UP" : P2up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 1 End If
	If Player = 1 And BallN = 4 Then BallIP = 2 : P1up.text = "1UP" : P2up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 1 End If
	If Player = 1 And BallN = 3 Then BallIP = 3 : P1up.text = "1UP" : P2up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 1 End If
	If Player = 1 And BallN = 2 Then BallIP = 4 : P1up.text = "1UP" : P2up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 1 End If
	If Player = 1 And BallN = 1 Then BallIP = 5 : P1up.text = "1UP" : P2up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 1 End If
	If Player = 2 And BallN2 = 5 Then BallIP = 1 : P2up.text = "2UP" : P1up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 2 End If
	If Player = 2 And BallN2 = 4 Then BallIP = 2 : P2up.text = "2UP" : P1up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 2 End If
	If Player = 2 And BallN2 = 3 Then BallIP = 3 : P2up.text = "2UP" : P1up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 2 End If
	If Player = 2 And BallN2 = 2 Then BallIP = 4 : P2up.text = "2UP" : P1up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 2 End If
	If Player = 2 And BallN2 = 1 Then BallIP = 5 : P2up.text = "2UP" : P1up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 2 End If
	If Player = 3 And BallN3 = 5 Then BallIP = 1 : P3up.text = "3UP" : P1up.text = "" : P2up.text = "" : P4up.text = "" : PUP = 3 End If
	If Player = 3 And BallN3 = 4 Then BallIP = 2 : P3up.text = "3UP" : P1up.text = "" : P2up.text = "" : P4up.text = "" : PUP = 3 End If
	If Player = 3 And BallN3 = 3 Then BallIP = 3 : P3up.text = "3UP" : P1up.text = "" : P2up.text = "" : P4up.text = "" : PUP = 3 End If
	If Player = 3 And BallN3 = 2 Then BallIP = 4 : P3up.text = "3UP" : P1up.text = "" : P2up.text = "" : P4up.text = "" : PUP = 3 End If
	If Player = 3 And BallN3 = 1 Then BallIP = 5 : P3up.text = "3UP" : P1up.text = "" : P2up.text = "" : P4up.text = "" : PUP = 3 End If
	If Player = 4 And BallN4 = 5 Then BallIP = 1 : P4up.text = "4UP" : P1up.text = "" : P2up.text = "" : P3up.text = "" : PUP = 4 End If
	If Player = 4 And BallN4 = 4 Then BallIP = 2 : P4up.text = "4UP" : P1up.text = "" : P2up.text = "" : P3up.text = "" : PUP = 4 End If
	If Player = 4 And BallN4 = 3 Then BallIP = 3 : P4up.text = "4UP" : P1up.text = "" : P2up.text = "" : P3up.text = "" : PUP = 4 End If
	If Player = 4 And BallN4 = 2 Then BallIP = 4 : P4up.text = "4UP" : P1up.text = "" : P2up.text = "" : P3up.text = "" : PUP = 4 End If
	If Player = 4 And BallN4 = 1 Then BallIP = 5 : P4up.text = "4UP" : P1up.text = "" : P2up.text = "" : P3up.text = "" : PUP = 4 End If
	BallCount.text = (BallIP)
	If B2SOn Then
	Controller.B2SSetPlayerUp 30, PUP
	Controller.B2SSetBallinPlay 32, BallIP
	End If
	NewBall.enabled=0
End Sub

Sub GameOverTimer_timer()
GameOvertb.text = "GAME OVER"
Game = 0
AllowStart = 1
If B2SOn Then 
Controller.B2SSetGameOver 35,1
End If
PlaySound "gameover"
If B2SOn Then 
Controller.B2SStopAnimation "bally1"
Controller.B2SStopAnimation "bally2"
Controller.B2SSetData "ly",1
Controller.B2SSetData "ba",1
End If
GameOverTimer.enabled = 0
End Sub

'****************************************************************************

'************ Tilt **********************************

Sub TiltTimer_Timer()
	TiltTimer.Enabled = False
End Sub

Sub CheckTilt
	If TiltTimer.Enabled = True And Game = 1 Then 
	TiltCount = TiltCount + 1
	If TiltCount = 3 Then
	Bumper1.force = 0
	Bumper2.force = 0
	Bumper3.force = 0
	Tilt = True
	PlaySound "tilt"
	FlipOff
	Tilttb.text= "TILT"
	Wall90.IsDropped=false
	Wall91.IsDropped=false
	If B2SOn Then
	Controller.B2SSetTilt 33,1
	End If
	End If
	Else
	TiltCount = 0
	TiltTimer.Enabled = True
	End If
End Sub

Sub FlipOff
	If Zipper=0 Then
	LeftFlipper.rotatetostart
	RightFlipper.rotatetostart
	End If
	If Zipper=1 Then
	CallZipDOF.enabled=1
	LeftFlipper1.rotatetostart
	RightFlipper1.rotatetostart
	UnZipTimerL.enabled=1
	UnZipTimerR.enabled=1
	LeftFlipper1.enabled=0 : RightFlipper1.enabled=0 : LeftFlipper.enabled=1 : RightFlipper.enabled=1
	LeftFlipper.rotatetostart
	RightFlipper.rotatetostart
	End If
	StopSound "buzz"
	StopSound "buzz1"
	StopSound "fldown"
End Sub

'****************************************************

'******* Spinning Disc *********

Sub SpinDisc_timer()
	Disc.RotY = Disc.RotY+5
End Sub

Sub DiscNoise_timer()
	Playsound "disc_noise" ,-1,0.005
	DiscNoise.enabled=0
End Sub

Sub DiscNoise2_timer()
	Playsound "disc_noise2" ,-1,0.005
	DiscNoise2.enabled=0
End Sub

Dim mTrigger

Set mTrigger = New cvpmMagnet
	With mTrigger
		.InitMagnet Trigger29, 13
		.GrabCenter = 0           
		.MagnetOn = 1              
		.CreateEvents "mTrigger"
	End With

Sub Trigger20_Hit                          
	mTrigger.MagnetOn = 0
	MBAntiGrab.enabled = 1
End Sub

Sub MBAntiGrab_timer()
	mTrigger.MagnetOn = 0
	MBAntiGrab.enabled = 0
End Sub

Sub DiscTriggers_Hit(index)
	Dim mStrong          
	mTrigger.MagnetOn = 1
	mStrong = Int(rnd*8)+1
	Select Case mStrong
	Case 1: mTrigger.InitMagnet GrabM, 9 
	Case 2: mTrigger.InitMagnet GrabM, 8 
	Case 3: mTrigger.InitMagnet GrabM, 7
	Case 4: mTrigger.InitMagnet GrabM, 6
	Case 5: mTrigger.InitMagnet GrabM, 5
	Case 6: mTrigger.InitMagnet GrabM, 10
	Case 7: mTrigger.InitMagnet GrabM, 4
	Case 8: mTrigger.InitMagnet GrabM, 3
	End Select	
End Sub

Sub Trigger34_Hit
	PlaySound "disc_roll",1
End Sub

Sub Trigger35_Hit
	StopSound "disc_roll"	
End Sub

Sub Trigger36_Hit
	Dim DiscHop         
	DiscHop = Int(rnd*2)+1
	Select Case DiscHop
	Case 1: PlaySound "hop1",0,0.5
	Case 2: PlaySound "hop2",0,0.5
	End Select
End Sub

Sub Trigger37_Hit
	StopSound "disc_roll"	
End Sub

'********************************

'******* Hit, Events and Sounds ************

Sub Trigger19_Hit
	If Tilt=false Then AddScore (100) End If
End Sub

Sub Trigger16_Hit
	If Tilt=false Then AddScore (1000) End If
End Sub

Sub Trigger17_Hit
	Light32.state=1
	Light35.state=1
	Light38.state=1
	Light33.state=1
	Light36.state=1
	Light39.state=1
	If Tilt=false Then AddScore (1000) End If
End Sub

Sub Trigger18_Hit
	If Tilt=false Then AddScore (1000) End If
End Sub

Sub Trigger13_Hit
	If Tilt=false Then
	If Light31.state=0 Then 
	AddScore (100) 
	End If
	If Light31.state=1 Then 
	AddScore (1000) 
	End If
    End If
End Sub

Sub Trigger12_Hit
	If Tilt=false Then
	If Light30.state=0 Then 
	AddScore (100) 
	End If
	If Light30.state=1 Then 
	AddScore (1000) 
	End If
    End If
End Sub

Sub Trigger11_Hit
	If Tilt=false Then
	If Light29.state=0 Then 
	AddScore (100) 
	End If
	If Light29.state=1 Then 
	AddScore (1000) 
	End If
    End If
	If Wall78.IsDropped=true Then Trigger40.enabled=1 End If
End Sub

Sub Trigger40_Hit
	If BarrierPos=1 Then
	BarrierC.enabled=1 
	Wall78.IsDropped=false
	Wall79.IsDropped=true
	End If
End Sub

Sub Trigger40_UnHit
	Trigger40.enabled=0
End Sub

Sub Trigger43_Hit
	DOF 121, DOFPulse
End Sub

Sub Trigger14_Hit
	If Tilt=false Then AddScore (100) End If
End Sub

Sub Trigger15_Hit
	If Tilt=false Then AddScore (1000) End If
End Sub

Sub Trigger9_Hit
	DOF 117, DOFPulse
	If Tilt=false Then AddScore (1000) End If
	If Tilt=true Then Kicker4.enabled=0 End If
End Sub

Sub Trigger10_Hit
	DOF 118, DOFPulse
	If Tilt=false Then AddScore (1000) End If
End Sub

Sub Target1_Hit
	DOF 108, DOFPulse
	If Tilt=false Then
	AddScore (1000)
	If Odin=1 Then OdinKick.enabled=1 End If
	If Wotan=1 Then WotanKick.enabled=1 End If
	If BarrierPos=0 Then 
	Barrier.enabled=1 
	Wall78.IsDropped=true
	Wall79.IsDropped=false
	End If
	End If
End Sub

Sub Barrier_timer()
	Primitive15.RotY = Primitive15.RotY-10
	If Primitive15.RotY=100 Then Barrier.enabled=0 : Primitive15.RotY=100 : PlaySound "barrier_click",0,1,1 End If
	BarrierPos=1
End Sub

Sub BarrierC_timer()
	Primitive15.RotY = Primitive15.RotY+10
	If Primitive15.RotY=140 Then BarrierC.enabled=0 : Primitive15.RotY=140  : PlaySound "barrier_click",0,1,1 End If
	BarrierPos=0
End Sub

Sub Kicker1_Hit
	PlaySound "kicker_enter_center", 0, 0.3
	If OWBalls=0 Then ReleaseTimer.enabled=1 End If
	If OWBalls>0 Then
	OwBalls=OWBalls-1 
	CheckOWBalls
	End If
	BlinkingSC.enabled=1
	Odin=1
End Sub

Sub ReleaseTimer_timer()
	PlaySound "ballrelease",0,1,0,0.25
	BallRelease.CreateBall
	BallRelease.Kick 80, 12
	ReleaseTimer.enabled=0
End Sub

Sub CheckOWBalls
	If OWBalls=0 Then OWDrain.enabled=0 End If
End Sub

Sub BlinkingSC_timer()
	Light28.state=0
	Light27.state=0
	Light26.state=0
	Light25.state=0
	Light24.state=1
	BlinkingSC1.enabled=1
	BlinkingSC.enabled=0
End Sub
	
Sub BlinkingSC1_timer()
	Light24.state=0
	Light25.state=1
	BlinkingSC2.enabled=1
	BlinkingSC1.enabled=0
End Sub

Sub BlinkingSC2_timer()
	Light25.state=0
	Light26.state=1
	BlinkingSC3.enabled=1
	BlinkingSC2.enabled=0
End Sub

Sub BlinkingSC3_timer()
	Light26.state=0
	Light27.state=1
	BlinkingSC4.enabled=1
	BlinkingSC3.enabled=0
End Sub

Sub BlinkingSC4_timer()
	Light27.state=0
	Light28.state=1
	BlinkingSC.enabled=1
	BlinkingSC4.enabled=0
End Sub

Sub Kicker1_UnHit
	PlaySound "popper_ball",0,.75,0,0.25
End Sub

Sub Kicker2_Hit
	PlaySound "kicker_enter_center", 0,0.3
	If OWBalls=0 Then ReleaseTimer.enabled=1 End If
	If OWBalls>0 Then
	OWBalls=OwBalls-1
	CheckOWballs
	End If
	Wotan=1
End Sub

Sub Kicker2_UnHit
	PlaySound "popper_ball",0,.75,0,0.25
End Sub

Sub TrigMushL_Hit
	DOF 114, DOFPulse
	Dim MushLSound         
	MushLSound = Int(rnd*3)+1
	Select Case MushLSound
	Case 1: PlaySound "flip_hit_1"
	Case 2: PlaySound "flip_hit_2"
	Case 2: PlaySound "flip_hit_3"
	End Select	
	If Tilt=false Then
	If Zipper=1 Then 
	CallZipDOF.enabled=1
	If LButton=0 Then
	UnZipTimerL.enabled=1 
	End If
	If LButton=1 Then
	UnZipTimerCL.enabled=1
	End If
	If RButton=0 Then
	UnZipTimerR.enabled=1 
	End If
	If RButton=1 Then
	UnZipTimerCR.enabled=1
	End If
	LeftFlipper1.enabled=0 : RightFlipper1.enabled=0 : LeftFlipper.enabled=1 : RightFlipper.enabled=1
	End If
	TmushL.TransZ = 6
	me.timerenabled = 1
	If Wotan = 1 Then WotanKick.enabled=1 End If
	AddScore (100)
	Light31.state=1
	End If
End Sub

Sub TrigMushL_timer()
	TmushL.TransZ = -6
	me.timerenabled = 0
End Sub

Sub TrigMushC_Hit
	DOF 115, DOFPulse
	Dim MushCSound         
	MushCSound = Int(rnd*3)+1
	Select Case MushCSound
	Case 1: PlaySound "flip_hit_1"
	Case 2: PlaySound "flip_hit_2"
	Case 2: PlaySound "flip_hit_3"
	End Select
	If Tilt=false Then
	If Zipper=0 Then
	CallZipDOF.enabled=1
	If LButton=0 Then
	ZipTimerL.enabled=1 
	End If
	If LButton=1 Then
	ZipTimerCL.enabled=1
	End If
	If RButton=0 Then
	ZipTimerR.enabled=1 
	End If
	If RButton=1 Then
	ZipTimerCR.enabled=1
	End If
	LeftFlipper1.enabled=1 : RightFlipper1.enabled=1 : LeftFlipper.enabled=0 : RightFlipper.enabled=0
	End If
	TmushC.TransZ = 6
	me.timerenabled = 1
	AddScore (100)
	Light30.state=1
	End if
End Sub 

Sub TrigMushC_timer()
	TmushC.TransZ = -6
	me.timerenabled = 0
End Sub

Sub CallZipDOF_timer()
	DOF 119, DOFPulse
	CallZipDOF.enabled=0
End Sub

Sub TrigMushR_Hit
	DOF 116, DOFPulse
	Dim MushRSound         
	MushRSound = Int(rnd*3)+1
	Select Case MushRSound
	Case 1: PlaySound "flip_hit_1"
	Case 2: PlaySound "flip_hit_2"
	Case 2: PlaySound "flip_hit_3"
	End Select
	If tilt=false Then
	If Zipper=1 Then
	CallZipDOF.enabled=1
	If LButton=0 Then
	UnZipTimerL.enabled=1 
	End If
	If LButton=1 Then
	UnZipTimerCL.enabled=1
	End If
	If RButton=0 Then
	UnZipTimerR.enabled=1 
	End If
	If RButton=1 Then
	UnZipTimerCR.enabled=1
	End If
	LeftFlipper1.enabled=0 : RightFlipper1.enabled=0 : LeftFlipper.enabled=1 : RightFlipper.enabled=1
	End If
	TmushR.TransZ = 6
	me.timerenabled = 1
	If Odin = 1 Then OdinKick.enabled=1 End If
	AddScore (100)
	Light29.state=1
	End If
End Sub

Sub TrigMushR_timer()
	TmushR.TransZ = -6
	me.timerenabled = 0
End Sub

Sub OdinKick_timer()
	Kicker1.Kick 90, 3
	DOF 112, DOFPulse
	If Light24.state=1 Then AddScore 1000 : Light24.state=1 : BlinkingSC.enabled=0 : BlinkingSC1.enabled=0: BlinkingSC2.enabled=0: BlinkingSC3.enabled=0: BlinkingSC4.enabled=0 End If
	If Light25.state=1 Then AddScore 2000 : Light25.state=1 : BlinkingSC.enabled=0 : BlinkingSC1.enabled=0: BlinkingSC2.enabled=0: BlinkingSC3.enabled=0: BlinkingSC4.enabled=0 End If
	If Light26.state=1 Then AddScore 3000 : Light26.state=1 : BlinkingSC.enabled=0 : BlinkingSC1.enabled=0: BlinkingSC2.enabled=0: BlinkingSC3.enabled=0: BlinkingSC4.enabled=0 End If
	If Light27.state=1 Then AddScore 4000 : Light27.state=1 : BlinkingSC.enabled=0 : BlinkingSC1.enabled=0: BlinkingSC2.enabled=0: BlinkingSC3.enabled=0: BlinkingSC4.enabled=0 End If
	If Light28.state=1 Then AddScore 5000 : Light28.state=1 : BlinkingSC.enabled=0 : BlinkingSC1.enabled=0: BlinkingSC2.enabled=0: BlinkingSC3.enabled=0: BlinkingSC4.enabled=0 End If
	OWBalls=OWBalls+1
	Odin=0
	OWDrain.enabled=1
	OdinKick.enabled=0
End Sub

Sub WotanKick_timer()
	DOF 113, DOFPulse
	Kicker2.Kick 90 , 3
	OWBalls=OWBalls+1
	Wotan=0
	OWDrain.enabled=1
	WotanKick.enabled=0
End Sub

'********* Zipper Flippers *********

Sub ZipTimerL_timer()
	ZipAnL.enabled=1
	FlipperL.visible=0 : FlipperL2.visible=1 
	ZipTimerL.enabled=0
End Sub

Sub ZipAnL_timer()
	ZipAn2L.enabled=1
	FlipperL2.visible=0 : FlipperL3.visible=1
	ZipAnL.enabled=0
End Sub

Sub ZipAn2L_timer()
	FlipperL3.visible=0 : FlipperL1.visible=1 
	ZipAn3L.enabled=1
	ZipAn2L.enabled=0
	PlaySound "zipflip",0,0.6,-1
	Wall80.IsDropped=false
	Wall81.IsDropped=false
	Zipper=1
End Sub

Sub ZipAn3L_timer()
	FlipperL1.visible=0 : FlipperL4.visible=1
	ZipAn4L.enabled=1
	ZipAn3L.enabled=0
End Sub

Sub ZipAn4L_timer()
	FlipperL4.visible=0 : FlipperL5.visible=1
	ZipAn5L.enabled=1
	ZipAn4L.enabled=0
End Sub

Sub ZipAn5L_timer()
	FlipperL5.visible=0 : FlipperL1.visible=1
	ZipAn5L.enabled=0
End Sub

Sub ZipTimerR_timer()
	ZipAnR.enabled=1
	FlipperR.visible=0 : FlipperR2.visible=1 
	ZipTimerR.enabled=0
End Sub

Sub ZipAnR_timer()
	ZipAn2R.enabled=1
	FlipperR2.visible=0 : FlipperR3.visible=1
	ZipAnR.enabled=0
End Sub

Sub ZipAn2R_timer()
	FlipperR3.visible=0 : FlipperR1.visible=1 
	ZipAn3R.enabled=1
	ZipAn2R.enabled=0
	PlaySound "zipflip",0,0.6,1
	Wall80.IsDropped=false
	Wall81.IsDropped=false
	Zipper=1
End Sub

Sub ZipAn3R_timer()
	FlipperR1.visible=0 : FlipperR4.visible=1
	ZipAn4R.enabled=1
	ZipAn3R.enabled=0
End Sub

Sub ZipAn4R_timer()
	FlipperR4.visible=0 : FlipperR5.visible=1
	ZipAn5R.enabled=1
	ZipAn4R.enabled=0
End Sub

Sub ZipAn5R_timer()
	FlipperR5.visible=0 : FlipperR1.visible=1
	ZipAn5R.enabled=0
End Sub

Sub ZipTimerCL_timer()
	ZipAnCL.enabled=1
	FlipperL.visible=0 : FlipperL10.visible=1
	ZipTimerCL.enabled=0
End Sub

Sub ZipAnCL_timer()
	ZipAn2CL.enabled=1
	FlipperL10.visible=0 : FlipperL9.visible=1
	ZipAnCL.enabled=0
End Sub

Sub ZipAn2CL_timer()
	ZipAn3CL.enabled=1
	FlipperL9.visible=0 : FlipperL1.visible=1
	ZipAn2CL.enabled=0
	PlaySound "zipflip",0,0.6,-1
	Wall80.IsDropped=false
	Wall81.IsDropped=false
	Zipper=1
End Sub

Sub ZipAn3CL_timer()
	ZipAn4CL.enabled=1
	FlipperL1.visible=0 : FlipperL11.visible=1
	ZipAn3CL.enabled=0
End Sub

Sub ZipAn4CL_timer()
	FlipperL11.visible=0 : FlipperL1.visible=1
	ZipAn4CL.enabled=0
End Sub

Sub ZipTimerCR_timer()
	ZipAnCR.enabled=1
	FlipperR.visible=0 : FlipperR10.visible=1
	ZipTimerCR.enabled=0
End Sub

Sub ZipAnCR_timer()
	ZipAn2CR.enabled=1
	FlipperR10.visible=0 : FlipperR9.visible=1
	ZipAnCR.enabled=0
End Sub

Sub ZipAn2CR_timer()
	ZipAn3CR.enabled=1
	FlipperR9.visible=0 : FlipperR1.visible=1
	ZipAn2CR.enabled=0
	PlaySound "zipflip",0,0.6,-1
	Wall80.IsDropped=false
	Wall81.IsDropped=false
	Zipper=1
End Sub

Sub ZipAn3CR_timer()
	ZipAn4CR.enabled=1
	FlipperR1.visible=0 : FlipperR11.visible=1
	ZipAn3CR.enabled=0
End Sub

Sub ZipAn4CR_timer()
	FlipperR11.visible=0 : FlipperR1.visible=1
	ZipAn4CR.enabled=0
End Sub

Sub UnZipTimerL_timer()
	UnZipAnL.enabled=1
	FlipperL1.visible=0 : FlipperL3.visible=1
	UnZipTimerL.enabled=0
End Sub

Sub UnZipAnL_timer()
	UnZipAn2L.enabled=1
	FlipperL3.visible=0 : FlipperL2.visible=1
	UnZipAnL.enabled=0
End Sub

Sub UnZipAn2L_timer()
	FlipperL2.visible=0 : FlipperL.visible=1
	UnZipAn3L.enabled=1
	UnZipAn2L.enabled=0
	PlaySound "unzipflip",0,0.6,-1
	Wall80.IsDropped=true
	Wall81.IsDropped=true
	Zipper=0
End Sub

Sub UnZipAn3L_timer()
	FlipperL.visible=0 : FlipperL6.visible=1
	UnZipAn4L.enabled=1
	UnZipAn3L.enabled=0
End Sub

Sub UnZipAn4L_timer()
	FlipperL6.visible=0 : FlipperL.visible=1
	UnZipAn4L.enabled=0
End Sub

Sub UnZipTimerR_timer()
	UnZipAnR.enabled=1
	FlipperR1.visible=0 : FlipperR3.visible=1
	UnZipTimerR.enabled=0
End Sub

Sub UnZipAnR_timer()
	UnZipAn2R.enabled=1
	FlipperR3.visible=0 : FlipperR2.visible=1
	UnZipAnR.enabled=0
End Sub

Sub UnZipAn2R_timer()
	FlipperR2.visible=0 : FlipperR.visible=1
	UnZipAn3R.enabled=1
	UnZipAn2R.enabled=0
	PlaySound "unzipflip",0,0.6,1
	Wall80.IsDropped=true
	Wall81.IsDropped=true
	Zipper=0
End Sub

Sub UnZipAn3R_timer()
	FlipperR.visible=0 : FlipperR6.visible=1
	UnZipAn4R.enabled=1
	UnZipAn3R.enabled=0
End Sub

Sub UnZipAn4R_timer()
	FlipperR6.visible=0 : FlipperR.visible=1
	UnZipAn4R.enabled=0
End Sub

Sub UnZipTimerCL_timer()
	UnZipAnCL.enabled=1
	FlipperL1.visible=0 : FlipperL9.visible=1
	UnZipTimerCL.enabled=0
End Sub

Sub UnZipAnCL_timer()
	UnZipAn2CL.enabled=1
	FlipperL9.visible=0 : FlipperL10.visible=1
	UnZipAnCL.enabled=0
End Sub

Sub UnZipAn2CL_timer()
	UnZipAn3CL.enabled=1
	FlipperL10.visible=0 : FlipperL.visible=1
	UnZipAn2CL.enabled=0
	PlaySound "unzipflip",0,0.6,-1
	Wall80.IsDropped=true
	Wall81.IsDropped=true
	Zipper=0
End Sub

Sub UnZipAn3CL_timer()
	UnZipAn4CL.enabled=1
	FlipperL.visible=0 : FlipperL12.visible=1
	UnZipAn3CL.enabled=0	
End Sub

Sub UnZipAn4CL_timer()
	FlipperL12.visible=0 : FlipperL.visible=1
	UnZipAn4CL.enabled=0	
End Sub

Sub UnZipTimerCR_timer()
	UnZipAnCR.enabled=1
	FlipperR1.visible=0 : FlipperR9.visible=1
	UnZipTimerCR.enabled=0
End Sub

Sub UnZipAnCR_timer()
	UnZipAn2CR.enabled=1
	FlipperR9.visible=0 : FlipperR10.visible=1
	UnZipAnCR.enabled=0
End Sub

Sub UnZipAn2CR_timer()
	UnZipAn3CR.enabled=1
	FlipperR10.visible=0 : FlipperR.visible=1
	UnZipAn2CR.enabled=0
	PlaySound "unzipflip",0,0.6,-1
	Wall80.IsDropped=true
	Wall81.IsDropped=true
	Zipper=0
End Sub

Sub UnZipAn3CR_timer()
	UnZipAn4CR.enabled=1
	FlipperR.visible=0 : FlipperR12.visible=1
	UnZipAn3CR.enabled=0	
End Sub

Sub UnZipAn4CR_timer()
	FlipperR12.visible=0 : FlipperR.visible=1
	UnZipAn4CR.enabled=0	
End Sub

Sub FlipLAn_timer()
	FlipperL.visible=0 : FlipperL8.visible=1
	FlipLAn2.enabled=1
	FlipLAn.enabled=0
End Sub

Sub FlipLAn2_timer()
	FlipperL8.visible=0 : FlipperL.visible=1
	FlipLAn2.enabled=0
End Sub

Sub FlipRAn_timer()
	FlipperR.visible=0 : FlipperR8.visible=1
	FlipRAn2.enabled=1
	FlipRAn.enabled=0
End Sub

Sub FlipRAn2_timer()
	FlipperR.visible=1 : FlipperR8.visible=0
	FlipRAn2.enabled=0
End Sub

Sub FlipL1An_timer()
	FlipperL1.visible=0 : FlipperL7.visible=1
	FlipL1An2.enabled=1
	FlipL1An.enabled=0
End Sub

Sub FlipL1An2_timer()
	FlipperL7.visible=0 : FlipperL1.visible=1
	FlipL1An2.enabled=0
End Sub

Sub FlipR1An_timer()
	FlipperR1.visible=0 : FlipperR7.visible=1
	FlipR1An2.enabled=1
	FlipR1An.enabled=0
End Sub

Sub FlipR1An2_timer()
	FlipperR7.visible=0 : FlipperR1.visible=1
	FlipR1An2.enabled=0
End Sub

'***************************************

Sub Trigger4_Hit
	PlaySound "fx_trigger"
	If Tilt=false Then 
	Kicker4.enabled=1 
	Light23.state=1
	End If
	mTrigger.MagnetOn = 1
End Sub

Sub Trigger1_Hit
	PlaySound "fx_trigger"
	Kicker4.enabled=0
	Light23.state=0
	mTrigger.MagnetOn = 1
End Sub

Sub Trigger7_Hit
	PlaySound "fx_trigger"
	Kicker4.enabled=0
	Light23.state=0
	mTrigger.MagnetOn = 1
End Sub

Sub Trigger2_Hit
	PlaySound "fx_trigger"
	If Tilt=false Then AddScore (10) End If
	mTrigger.MagnetOn = 1
End Sub

Sub Trigger3_Hit
	PlaySound "fx_trigger"
	If Tilt=false Then AddScore (10) End If
	mTrigger.MagnetOn = 1
End Sub

Sub Trigger5_Hit
	PlaySound "fx_trigger"
	If Tilt=false Then AddScore (10) End If
	mTrigger.MagnetOn = 1
End Sub

Sub Trigger6_Hit
	PlaySound "fx_trigger"
	If Tilt=false Then AddScore (10) End If
	mTrigger.MagnetOn = 1
End Sub

Sub Trigger8_Hit
	PlaySound "fx_trigger"
	If Tilt=false Then AddScore (10) End If
	mTrigger.MagnetOn = 1
End Sub

Sub Kicker4_Hit
	me.timerenabled=1
End Sub

Sub Kicker4_Timer()
	DOF 111, DOFPulse
	StrongDir.enabled=1
	Kicker4.Kick Dir, Strong
	Stantuffo.enabled=1
	AddScore (1000)
	me.timerenabled=0
End Sub

Sub Stantuffo_timer()
	Primitive83.TransY=-70
	StantuffoR.enabled=1
	Stantuffo.enabled=0
End Sub

Sub StantuffoR_timer()
	Primitive83.TransY=Primitive83.TransY+5
	If Primitive83.TransY=0 Then
	StantuffoR.enabled=0
	End If
End Sub

Sub Kicker4_UnHit
	PlaySound "popper_ball",0,.75,0,0.25
End Sub

Sub StrongDir_timer()
KickIP = Int(Rnd*6)+1
Select Case KickIP
    Case 1 : Dir = 0 : Strong = 30  
    Case 2 : Dir = 0 : Strong = 27
	Case 3 : Dir = 0 : Strong = 24
	Case 4 : Dir = 0 : Strong = 21 
	Case 5 : Dir = 5 : Strong = 30
	Case 6 : Dir = 5 : Strong = 27
End Select
StrongDir.enabled=0
End Sub

Sub Wall66_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber1.visible=0
	Rubber11.visible=1
	RubberTimer.enabled=1
End Sub

Sub RubberTimer_timer()
	Rubber1.visible=1
	Rubber11.visible=0
	RubberTimer.enabled=0
End Sub

Sub Wall67_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber2.visible=0
	Rubber13.visible=1
	RubberTimer1.enabled=1
End Sub

Sub RubberTimer1_timer()
	Rubber2.visible=1
	Rubber13.visible=0
	RubberTimer1.enabled=0
End Sub

Sub Wall68_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber10.visible=0
	Rubber26.visible=1
	RubberTimer2.enabled=1
End Sub

Sub RubberTimer2_timer()
	Rubber10.visible=1
	Rubber26.visible=0
	RubberTimer2.enabled=0
End Sub

Sub Wall69_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber14.visible=0
	Rubber20.visible=1
	RubberTimer3.enabled=1
End Sub

Sub RubberTimer3_timer()
	Rubber14.visible=1
	Rubber20.visible=0
	RubberTimer3.enabled=0
End Sub

Sub Wall70_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber12.visible=0
	Rubber27.visible=1
	RubberTimer4.enabled=1
End Sub

Sub RubberTimer4_timer()
	Rubber12.visible=1
	Rubber27.visible=0
	RubberTimer4.enabled=0
End Sub

Sub Wall71_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber15.visible=0
	Rubber28.visible=1
	RubberTimer5.enabled=1
End Sub

Sub RubberTimer5_timer()
	Rubber15.visible=1
	Rubber28.visible=0
	RubberTimer5.enabled=0
End Sub

Sub Wall72_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber9.visible=0
	Rubber29.visible=1
	RubberTimer6.enabled=1
End Sub

Sub RubberTimer6_timer()
	Rubber9.visible=1
	Rubber29.visible=0
	RubberTimer6.enabled=0
End Sub

Sub Wall73_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber5.visible=0
	Rubber30.visible=1
	RubberTimer7.enabled=1
End Sub

Sub RubberTimer7_timer()
	Rubber5.visible=1
	Rubber30.visible=0
	RubberTimer7.enabled=0
End Sub

Sub Wall74_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber4.visible=0
	Rubber31.visible=1
	RubberTimer8.enabled=1
End Sub

Sub RubberTimer8_timer()
	Rubber4.visible=1
	Rubber31.visible=0
	RubberTimer8.enabled=0
End Sub

'*****************************************

'********** Sling Shot Animations ***************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySound SoundFXDOF("slingshot",106,DOFPulse,DOFContactors), 0, 1, 0.05, 0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -25
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	If Tilt = false Then
	Addscore (10)
	End If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySound SoundFXDOF("slingshot",107,DOFPulse,DOFContactors),0,1,-0.05,0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -25
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	If Tilt = false Then
	Addscore (10)
	End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1

End Sub

'*******************************************

'********** Bumpers **********************************************

Dim dirRing : dirRing = -1     
Dim dirRing2 : dirRing2 = -1     
Dim dirRing3 : dirRing3 = -1     
	
Sub Bumper1_Hit
If Tilt = false Then
Me.TimerEnabled = 1  
	Dim BumpSound         
	BumpSound = Int(rnd*4)+1
	Select Case BumpSound
	Case 1: PlaySound SoundFXDOF("bump1",105,DOFPulse,DOFContactors)
	Case 2: PlaySound SoundFXDOF("bump2",105,DOFPulse,DOFContactors)
	Case 3: PlaySound SoundFXDOF("bump3",105,DOFPulse,DOFContactors)
	Case 4: PlaySound SoundFXDOF("bump4",105,DOFPulse,DOFContactors)
	End Select	  
	If Light32.state = 0 Then 
	Addscore (10)
	End If
	If Light32.state = 1 Then 
	Addscore (100)
	End If
	End If
	
End Sub

Sub Bumper1_timer()
	If Tilt = false Then
	BumperRing1.Z = BumperRing1.Z + (5 * dirRing)    
	If BumperRing1.Z <= 0 Then dirRing = 1          
	If BumperRing1.Z >= 40 Then                      
		dirRing = -1       
		BumperRing1.Z = 40
		Me.TimerEnabled = 0
	End If
	End If

End Sub

Sub Bumper2_Hit
If Tilt = false Then
Me.TimerEnabled = 1            
	Dim BumpSound         
	BumpSound = Int(rnd*4)+1
	Select Case BumpSound
	Case 1: PlaySound SoundFXDOF("bump1",104,DOFPulse,DOFContactors)
	Case 2: PlaySound SoundFXDOF("bump2",104,DOFPulse,DOFContactors)
	Case 3: PlaySound SoundFXDOF("bump3",104,DOFPulse,DOFContactors)
	Case 4: PlaySound SoundFXDOF("bump4",104,DOFPulse,DOFContactors)
	End Select
	If Light33.state = 0 Then 
	Addscore (10)
	End If
	If Light33.state = 1 Then 
	Addscore (100)
	End If
	End If
	
End Sub

Sub Bumper2_timer()
	If Tilt = false Then
	BumperRing2.Z = BumperRing2.Z + (5 * dirRing2)    
	If BumperRing2.Z <= 0 Then dirRing2 = 1         
	If BumperRing2.Z >= 40 Then                     
		dirRing2 = -1       
		BumperRing2.Z = 40
		Me.TimerEnabled = 0
	End If
	End If

End Sub

Sub Bumper3_Hit
If Tilt = false Then
Me.TimerEnabled = 1             
	Dim BumpSound         
	BumpSound = Int(rnd*4)+1
	Select Case BumpSound
	Case 1: PlaySound SoundFXDOF("bump1",103,DOFPulse,DOFContactors)
	Case 2: PlaySound SoundFXDOF("bump2",103,DOFPulse,DOFContactors)
	Case 3: PlaySound SoundFXDOF("bump3",103,DOFPulse,DOFContactors)
	Case 4: PlaySound SoundFXDOF("bump4",103,DOFPulse,DOFContactors)
	End Select 
	Addscore (100)
	End If
End Sub

Sub Bumper3_timer()
	If Tilt = false Then
	BumperRing3.Z = BumperRing3.Z + (5 * dirRing3)    
	If BumperRing3.Z <= 0 Then dirRing3 = 1          
	If BumperRing3.Z >= 40 Then                      
		dirRing3 = -1       
		BumperRing3.Z = 40
		Me.TimerEnabled = 0
	End If
	End If

End Sub

'*****************************************************************************

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 4000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / Fireball.width-1
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

' The sound is played using the VOL, PAN and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the PAN function will change the stereo position according
' to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


'********* Others Sounds *************

Sub Trigger38_Hit
	PlaySound "metal_hop"
	Trigger41.enabled=1
End Sub

Sub Trigger41_Hit
	PlaySound "drop",0,0.5
	Trigger41.enabled=0
End Sub

Sub Wall75_Hit
	PlaySound "toc",0,0.5
	End Sub

Sub Wall76_Hit
	PlaySound "metalhit_thin"
	End Sub

Sub Wall77_Hit
	PlaySound "metalhit_medium"
	End Sub

Sub Rubber32_Hit
	PlaySound "flip_hit_1"
	End Sub

Sub Gate1_Hit
	PlaySound "gate4"
End Sub

Sub Gate3_Hit
	PlaySound "gate"
End Sub

Sub Wall57_Hit
	PlaySound "metalhit_medium"
	End Sub

Sub CallFlipOff_Timer()
	FlipOff
	CallFlipOff.enabled=0
End Sub

Sub Wall37_Hit
	PlaySound "metalhit_medium"
	End Sub

Sub Wall39_Hit
	PlaySound "metalhit_medium"
	End Sub

Sub Wall93_Hit
	PlaySound "metalhit_medium"
	End Sub

Sub Wall29_Hit
	PlaySound "metalhit_medium"
	End Sub

Sub Wall50_Hit
	PlaySound "metalhit_medium"
	End Sub

Sub Wall51_Hit
	PlaySound "metalhit_medium"
	End Sub

Sub Wall52_Hit
	PlaySound "metalhit_medium"
	End Sub

Sub Wall15_Hit
	PlaySound "metalhit_medium"
	End Sub

Sub Wall23_Hit
	PlaySound "metalhit_medium"
	End Sub

Sub Wall95_Hit
	PlaySound "metalhit_medium"
	End Sub

Sub Plastics_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 	RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber2", 0, parm / 10, -0.1, 0.15
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber2", 0, parm / 10, 0.1, 0.15
End Sub

Sub LeftFlipper1_Collide(parm)
    PlaySound "fx_rubber2", 0, parm / 10, -0.1, 0.15
End Sub

Sub RightFlipper1_Collide(parm)
    PlaySound "fx_rubber2", 0, parm / 10, 0.1, 0.15
End Sub

'*************************************
