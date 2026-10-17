'*
'*      Cuphead (D. Goblett & Co 2019)
'*		By Onevox. Core script by Loserman76
'*      "The Bees Knees": Loserman76, bord, BorgDog, Thalamus, Xenonph, cyberpez 
'*      Adapted from the "Cuphead" videogame by StudioMDHR
'*
'*		Wylte fixes:	Merged duplicate subroutines, created long flipper collide subs For sound, fixed flipper triggers, fixed spinner scoring,
'*			aligned rubberparty and all post primitives to layer1, removed duplicate/legacy primitives, aligned bumper and flipper elements,
'*			modified Ninuzzu ball shadow code, made kickers a tiny bit random, commented out debug code, scripted songs to be in "Cuphead" folder
'*
'*		Pup-Pack by: MauiPunter


option explicit

'******************************** PuP Variables *****************************************

Dim usePUP: Dim usePUPDMD: Dim dmdScreen: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings For this table ********************************

usePUP   	= true         ' enable Pinup Player functions For this table.  Must be true if you are using PUP DMD
usePUPDMD	= false			' enable PUP DMD
cPuPPack 	= "cuphead"    ' name of the PuP-Pack / PuPVideos folder For this table
'****************************************************************************************

Randomize

ExecuteGlobal GetTextFile("core.vbs")

On Error Resume Next
ExecuteGlobal GetTextFile("Controller.vbs")
If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the scripts folder."

B2SOff=Table1.showDT ' disable b2s For desktop

On Error Goto 0

Const cGameName = "cuphead"

Const ShadowFlippersOn = true
Const ShadowBallOn = true

Const ShadowConfigFile = false

Dim Controller  'B2S
Dim B2SScore	' B2S Score Displayed
Const HSFileName="CupheadVPX.txt"
Const LMEMTableConfig="LMEMTables.txt"
Const LMEMShadowConfig="LMEMShadows.txt"
Dim EnableBallShadow
Dim EnableFlipperShadow
Dim introStarted: introStarted = False

'* this value adjusts score motor behavior - 0 allows you to continue scoring while the score motor is running - 1 sets score motor to behave more like a real EM
Const ScoreMotorAdjustment=1

'* this is a debug setting to use an older scoring routine vs a newer score routine - don't change this value
Const ScoreAdditionAdjustment=1

'* this controls whether you hear bells (0) or chimes (1) when scoring
Const ChimesOn=1

'* This controls whether music is played (1) or not (0) during game time
Dim MusicOn
MusicOn = 1

'* This sets the music volume For all songs (Doesn't really - use the in-game menu)
Dim MusicVolume
MusicVolume = 1

'* Set to 1 For Free Play or 0 For Coin PlayChime
Dim FreePlay
FreePlay = 1

'* Set to 1 For Double Bonus on last ball 0
Dim LastBallDoubleBonus
LastBallDoubleBonus = 1

'* SoulLight Shifting Set to 0 For flippers and 1 For CupHead/Mugman Rubbers
Dim ShiftControl
ShiftControl = 0

'* Colored Balls For Multiball
Dim ColoredBall
ColoredBall = 1

'* Flipper Length Set to '2' For 2" Flippers and '3' For 3" Flippers
Dim FlipperLength
FlipperLength = 3

'* SOUL light reset - Set to 1 to keep SOUL lights lit from ball to ball
Dim SoulLightReset
SoulLightReset = 1
Dim SoulLightArray(4,4)

'* Plunger Sounds - Set to 1 to enable mechanical plunger sounds or 0 to not enable these sounds
Dim PlungerSound
PlungerSound = 1

Dim ScoreChecker
Dim CheckAllScores
Dim sortscores(4)
Dim sortplayers(4)
Dim TextStr,TextStr2
Dim i,xx,LStep,RStep

Dim obj
Dim bgpos
Dim dooralreadyopen
Dim TargetSpecialLit
Dim Points210counter
Dim Points500counter
Dim Points1000counter
Dim Points2000counter
Dim BallsPerGame
Dim InProgress
Dim BallInPlay
Dim CreditsPerCoin
Dim Score100K(4)
Dim Score(4)
Dim ScoreDisplay(4)
Dim HighScorePaid(4)
Dim HighScore
Dim HighScoreReward
Dim BonusMultiplier
Dim Credits
Dim Match
Dim Replay1
Dim Replay2
Dim Replay3
Dim Replay1Paid(4)
Dim Replay2Paid(4)
Dim Replay3Paid(4)
Dim TableTilted
Dim TiltCount
Dim OperatorMenu

Dim ExtraBallSetting
Dim BonusBooster
Dim BonusBoosterCounter
Dim BonusCounter
Dim HoleCounter

Dim AdvanceLightCounter
Dim ExtraBall

Dim Ones
Dim Tens
Dim Hundreds
Dim Thousands

Dim Player
Dim Players

Dim AlternateRelay

				   
						 

Dim LightSequenceCounter
Dim LightSequence2Counter
Dim SoulBonusCounter
Dim BonusMultiplierCounter
Dim ScoreMotorStepper

Dim rst
Dim bonuscountdown
Dim TempMultiCounter
Dim TempPlayerup
Dim RotatorTemp

Dim bump1
Dim bump2
Dim bump3

Dim LastChime10
Dim LastChime100
Dim LastChime1000

Dim Score10
Dim Score100

Dim MotorRunning
Dim Replay1Table(15)
Dim Replay2Table(15)
Dim Replay3Table(15)
Dim ReplayTableSet
Dim ReplayLevel
Dim ReplayTableMax
Dim ScoreMotorClicks

Sub Table1_Init()
	If Table1.ShowDT = false Then
		For Each obj in DesktopCrap
			obj.visible=False
		Next
	End If

	OperatorMenuBackdrop.image = "PostitBL"
	For XOpt = 1 to MaxOption
		Eval("OperatorOption"&XOpt).image = "PostitBL"
	Next

	For XOpt = 1 to 256
		Eval("Option"&XOpt).image = "PostItBL"
	Next

	'PUP: disable music when using pup pack
	If usePUP Then
		MusicOn = 0
	End If
	'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<


	LoadEM
	LoadLMEMConfig2
	HideOptions
	SetupReplayTables
	PlasticsOff
	BumpersOff
	OperatorMenu=0
	HighScore=0
	MotorRunning=0
	HighScoreReward=3
	BallsPerGame=5
	BonusBooster=0
	ReplayLevel=1
	ExtraBallSetting=1
	Credits=0
	loadhs	   
	If HighScore=0 Then HighScore=50000

	Replay1=80000
	Replay2=125000
	Replay3=140000
	TableTilted=false

	Match=int(Rnd*10)*10
	MatchReel.SetValue((Match/10)+1)
	GameOverReel.SetValue(1)
	TiltReel.SetValue(1)
	CanPlayReel.SetValue(0)
	BallInPlayReel.SetValue(0)

				  
	  
	For Each obj in PlayerScoresOn
		obj.ResetToZero
	Next
	For Each obj in PlayerScores
		obj.ResetToZero
	Next
	For Each obj in StarLights
		obj.state=0
	Next
	For Each obj in BonusX
		obj.state=0
	Next
	For Each obj in TopRolloverLights
		obj.state=0
	Next
								  
			  
	  
								   
			  
	  
	For x = 1 to 4
		PerditionEntry(x) = 1
	Next
	FireLight1.state=0
	FireLight2.state=0
	FireLight3.state=0
	LightSequenceCounter=0
	LightSequence2Counter=0
	SnakeEyesLight.state=2

	If FlipperLength = 3 Then	
		For Each obj in inch2
			obj.visible = False
		Next
		For Each obj in inch2col
			obj.collidable = False
		Next
		For Each obj in inch3
			obj.visible = True
		Next
		For Each obj in inch3col
			obj.collidable = True
		Next
		LeftFlipper.Enabled = 0
		RightFlipper.Enabled = 0
		LeftFlipper001.Enabled = 1
		RightFlipper001.Enabled = 1
	Else
		For Each obj in inch2
			obj.visible = True
		Next
		For Each obj in inch2col
			obj.collidable = True
		Next
		For Each obj in inch3
			obj.visible = False
		Next
		For Each obj in inch3col
			obj.collidable = False
		Next
		LeftFlipper.Enabled = 1
		RightFlipper.Enabled = 1
		LeftFlipper001.Enabled = 0
		RightFlipper001.Enabled = 0
	End If

	Replay1=Replay1Table(ReplayLevel)
	Replay2=Replay2Table(ReplayLevel)
	Replay3=Replay3Table(ReplayLevel)

	SoulBonusCounter=0
	BonusCounter=0
	HoleCounter=0
    bgpos=6
	AdvanceLightCounter=0

	ShadowMain.visible=False
	For Each obj in ShadowCup
		obj.visible=False
	Next
	For Each obj in ShadowMug
		obj.visible=False
	Next

 	dooralreadyopen=0
	InstructCard.image="IC_"+FormatNumber(BallsPerGame,0)

	RefreshReplayCard

			
					 

	TargetSpecialLit = 0
	Points210counter=0
	Points500counter=0
	Points1000counter=0
	Points2000counter=0

	BonusBoosterCounter=0
	Players=0
	RotatorTemp=1
	InProgress=false
	ExtraBall=false
	AlternateRelay=1


							

	If B2SOn Then
		If Match=0 Then
			Controller.B2SSetMatch 100
		Else
			Controller.B2SSetMatch Match
		End If
		Controller.B2SSetScoreRolloverPlayer1 0
		Controller.B2SSetScoreRolloverPlayer2 0
		Controller.B2SSetScoreRolloverPlayer3 0
		Controller.B2SSetScoreRolloverPlayer4 0
													   
		Controller.B2SSetTilt 0
		Controller.B2SSetCredits Credits
		Controller.B2SSetGameOver 1
		Controller.B2SSetShootAgain 0
	End If

	For i=1 to 4
    player=i
		If B2SOn Then
			Controller.B2SSetScorePlayer player, 0
		End If
	Next
	bump1=1
	bump2=1
	bump3=1
	InitPauser5.enabled=true
End Sub

Sub Table1_exit()
	savehs
	SaveLMEMConfig
	SaveLMEMConfig2
	If B2SOn Then Controller.Stop
End Sub

Sub Table1_KeyDown(ByVal keycode)

	' GNMOD
	If EnteringInitials Then
		CollectInitials(keycode)
		exit sub
	End If

	If EnteringOptions Then
		CollectOptions(keycode)
		exit sub
	End If

	If keycode = PlungerKey Then
		Plunger.PullBack
'		PlungerPulled = 1
		PlaySound"CHPlungerpull"

	End If

	If keycode = LeftFlipperKey and InProgress = false Then
		OperatorMenuTimer.Enabled = true
	End If
	' END GNMOD

	If keycode = LeftFlipperKey and InProgress=true and TableTilted=false and BotchedMB = 0 and contball = 0 Then
		LFPress = 1
		If FlipperLength = 2 Then
			lf.fire
		Else
			lf1.fire
		End If
		PlaySoundAt SoundFXDOF("FlipperUp",101,DOFOn,DOFFlippers), LeftFlipper
		PlayLoopSoundAtVol "buzzL", LeftFlipper, 1
		If ShiftControl = 0 Then SoulLightsLeft
	End If

	If keycode = RightFlipperKey and InProgress=true and TableTilted=false and BotchedMB = 0 and contball = 0 Then
		RFPress = 1
		If FlipperLength = 2 Then
			rf.fire
		Else
			rf1.fire
		End If
		PlaySoundAt SoundFXDOF("FlipperUp",102,DOFOn,DOFFlippers), RightFlipper
		PlayLoopSoundAtVol "buzz", RightFlipper, 1
		If ShiftControl = 0 Then SoulLightsRight
	End If

	If keycode = LeftTiltKey Then
		Nudge 90, 2
		TiltIt
	End If

	If keycode = RightTiltKey Then
		Nudge 270, 2
		TiltIt
	End If

	If keycode = CenterTiltKey Then
		Nudge 0, 2
		TiltIt
	End If

	If keycode = MechanicalTilt Then
		TiltCount=2
		TiltIt
	End If

	If keycode = AddCreditKey or keycode = 4 or keycode = 5 Then

		'PUP Event: trigger intro
		If Not introStarted Then
			pupevent 200
			introStarted = True
		End If
		'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<
					  
			 
	   

					  
						  
			   
					  
		
		PlaySoundAt "coinin", Drain
		playsound "CHCoinin"
		AddSpecial2
	End If



	If keycode = StartGameKey and InProgress=true and usePUP and Players>0 and Players<2 and BallInPlay<2 Then
		If FreePlay = 0 and Credits <1 Then Exit Sub

		If Credits > 0 Then Credits = Credits - 1
		If Credits < 1 Then DOF 126, DOFOff
		CreditsReel.SetValue(Credits)
		Players=Players+1
		PuPlayer.LabelSet dmdScreen,"Credits","CREDITS: " & CStr(Credits),1,""
		PuPlayer.LabelSet dmdScreen,"Players","PLAYERS: " & CStr(Players),1,""

		CanPlayReel.SetValue(Players)
		playsound "click":PB=1:If MusicOn = 1 Then s01.enabled=true
		If B2SOn Then
			Controller.B2SSetCanPlay Players
			If Players=2 Then
				Controller.B2SSetScoreRolloverPlayer2 0
			End If
			If Players=3 Then
				Controller.B2SSetScoreRolloverPlayer3 0
			End If
			If Players=4 Then
				Controller.B2SSetScoreRolloverPlayer4 0
			End If
			Controller.B2SSetCredits Credits
		End If
    End If

	If keycode = StartGameKey and InProgress=true and Not usePUP and Players>0 and Players<4 and BallInPlay<2 Then
		If FreePlay = 0 and Credits <1 Then Exit Sub

		If Credits > 0 Then Credits = Credits - 1
		If Credits < 1 Then DOF 126, DOFOff
		CreditsReel.SetValue(Credits)
		Players=Players+1
		CanPlayReel.SetValue(Players)
		playsound "click":PB=1:If MusicOn = 1 Then s01.enabled=true
		If B2SOn Then
			Controller.B2SSetCanPlay Players
			If Players=2 Then
				Controller.B2SSetScoreRolloverPlayer2 0
			End If
			If Players=3 Then
				Controller.B2SSetScoreRolloverPlayer3 0
			End If
			If Players=4 Then
				Controller.B2SSetScoreRolloverPlayer4 0
			End If
			Controller.B2SSetCredits Credits
		End If
    End If

	If keycode=StartGameKey and InProgress=false and Players=0 and EnteringOptions = 0 Then
		'GNMOD
		OperatorMenuTimer.Enabled = false
		'END GNMOD

		If FreePlay = 0 and Credits <1 Then Exit Sub

		If Credits > 0 Then Credits = Credits - 1
		If Credits < 1 Then DOF 126, DOFOff
		CreditsReel.SetValue(Credits)
		Players=1
		
		'PUP: Update PUP DMD
		If usePUP Then
			PuPlayer.LabelSet dmdScreen,"Credits","CREDITS: " & CStr(Credits),1,""
			PuPlayer.LabelSet dmdScreen,"Players","PLAYERS: " & CStr(Players),1,""
		End If
		'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

		CanPlayReel.SetValue(Players)
		MatchReel.SetValue(0)
		GameOverReel.SetValue(0)
		Player=1
		playsound "StartUpSequence":EndMusic:If MusicOn = 1 Then s01.enabled=True:AA=0
		TempPlayerUp=Player
		PlayerUpRotator.enabled=true
		rst=0
		BallInPlay=1
		'PUP: Update PUP DMD
		pupevent 410
		If Not usePUPDMD Then
			PuPlayer.LabelSet dmdScreen,"Ball","BALL: " & CStr(BallInPlay),1,""
		End If
		'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

		InProgress=true
		resettimer.enabled=true
		BonusMultiplier=1
		If B2SOn Then
			Controller.B2SSetTilt 0
			Controller.B2SSetGameOver 0
			Controller.B2SSetMatch 0
			Controller.B2SSetCredits Credits
									  
			Controller.B2SSetCanPlay 1
			Controller.B2SSetPlayerUp 1
			Controller.B2SSetBallInPlay BallInPlay
			Controller.B2SSetScoreRolloverPlayer1 0
		End If
		If Table1.ShowDT = True Then
			For Each obj in PlayerScores
				obj.ResetToZero
				obj.Visible=true
			Next
			For Each obj in PlayerScoresOn
				obj.ResetToZero
				obj.Visible=false
			Next

											 
					
		
													
				
		
													  
													   
			PlayerScores(Player-1).Visible=0
			PlayerScoresOn(Player-1).Visible=1
		
	   

							   
							
						
			  
						
			  
		  
 
							   
						   
								  
			  
					   
			  
		  

											   
 
										   
 
											   
 
												 

										

							 
							
					
						
					   
	   
						   
									   
	   
						
					  
	   
						   
						 
	   
						  
						   
										   
											 
	  
					
						
					  
	   
						   
						 
	   
						
					   
	   
						   
									   
	   
						  
						   
										   
											 
		End If
  
	End If

							 
  
		  
							   
												 
						 
						   
										  
												   
	   
	  
				  
						   
										  
												
	   
		

	   

End Sub

Dim ColorChange

Sub Table1_KeyUp(ByVal keycode)

	' GNMOD
	If EnteringInitials Then
		exit sub
	End If

	If keycode = PlungerKey Then	
 
							
			
		 
										
		PlaySoundAt "CHPlungerRelease", Plunger
		Plunger.Fire
	End If

	If keycode = LeftFlipperKey Then
		OperatorMenuTimer.Enabled = false
	End If

	' END GNMOD

	If keycode = LeftFlipperKey and InProgress=true and TableTilted=false Then
		lfpress = 0
		LeftFlipper.eosTorqueAngle = EOSA
		LeftFlipper.eosTorque = EOST
		LeftFlipper.RotateToStart
		LeftFlipper001.RotateToStart
		PlaySoundAt SoundFXDOF("FlipperDown",101,DOFOff,DOFFlippers), LeftFlipper
		StopSound "buzzL"
	End If

	If keycode = RightFlipperKey and InProgress=true and TableTilted=false Then
		rfpress = 0
		RightFlipper.eosTorqueAngle = EOSA
		RightFlipper.eosTorque = EOST
		RightFlipper.RotateToStart
		RightFlipper001.RotateToStart
		PlaySoundAt SoundFXDOF("FlipperDown",102,DOFOff,DOFFlippers), RightFlipper
		StopSound "buzz"
	End If

											   
 
										   
 
											   
 
												 

										

End Sub

Dim BotchedMB
Sub Drain_Hit()
	FireLight1.state=0
	FireLight2.state=0
	FireLight3.state=0
	PlaySoundAt SoundFXDOF("fx_drain",122,DOFPulse,DOFContactors), Drain
	If MultiballOn = 1 Then 
		EndMusic
		Drain.DestroyBall
		BotchedMB = 1
		MultiballOn = 0
		PlaySound "CHPerditionKicker"
		PlasticsOff
		BumpersOff
		For Each obj in StarLights
			obj.state=0
		Next
		bumpercap2.image="bcap-100wl-unlit devilBW"
		LeftFlipper.RotateToStart
		RightFlipper.RotateToStart
		Perditionpost.transz= -37
		PerditionWall.collidable = 0
		Dim  BOT, b
		BOT = GetBalls
		For b = 0 to ubound(BOT)
			If BOT(b).x  > 790 and BOT(b).y < 800 Then Set KickerBall1 = BOT(b)
		Next
		Pkickarm1.rotz=15
		KickerArm1.enabled=true
		kickBall KickerBall1, 0, 45, 5, 30
		PlaySoundAt SoundFXDOF("saucer",131,DOFPulse,DOFcontactors), Pkickarm1
		Exit Sub
	End If
	If MultiballReleased = 1 Then
		MultiballReleased = 0
		PerditionWall.collidable = 0
		perditionpost.transz = -37
		Drain.DestroyBall
		GIReset
		Exit Sub
	Else
		'PUP: trigger YOU DIED event here <<<<<<<<<<<<<<<<<<
		If SoulBonusCounter<10 Then
			pupevent INT("2" & CStr(SoulBonusCounter) & "3")
		Else 
			'PUP: trigger FINAL Boss Wizard Mode die <<<<<<<<
			If SoulBonusCounter = 10 Then
				pupevent 302
			Else
				pupevent INT("2" & CStr(SoulBonusCounter-10) & "3")
			End If
		End If
		'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<	
	End If
	EndMusic:PlaySound "CHDrain":AA=1:PB=0:If MusicOn = 1 Then m01.enabled=false:m02.enabled=false
	Drain.DestroyBall
	For x = 1 to 3
		EVAL("PerditionLight" & x).state = 0
	Next
	SnakeEyesLight.state=2
	Pause4Bonustimer.enabled=1
	BotchedMB = 0
End Sub

'************************Music Timers and Triggers

Sub BallReleaseGate_Hit()
    PB=1
    If AA=1 Then
    If MusicOn = 1 Then m01.enabled=True
    End If
End Sub

Sub m01_Timer
    Dim za
    za = INT(10 * RND(1) )
    Select Case za
		Case 0:PlayMusic"cuphead/CupMusic1.mp3", MusicVolume :m02.enabled=true:m02.interval=208049
		Case 1:PlayMusic"cuphead/CupMusic2.mp3", MusicVolume :m02.enabled=true:m02.interval=225284
		Case 2:PlayMusic"cuphead/CupMusic3.mp3", MusicVolume :m02.enabled=true:m02.interval=234633
		Case 3:PlayMusic"cuphead/CupMusic4.mp3", MusicVolume :m02.enabled=true:m02.interval=243706
		Case 4:PlayMusic"cuphead/CupMusic5.mp3", MusicVolume :m02.enabled=true:m02.interval=241058
		Case 5:PlayMusic"cuphead/CupMusic6.mp3", MusicVolume :m02.enabled=true:m02.interval=227391
		Case 6:PlayMusic"cuphead/CupMusic7.mp3", MusicVolume :m02.enabled=true:m02.interval=264930
		Case 7:PlayMusic"cuphead/CupMusic8.mp3", MusicVolume :m02.enabled=true:m02.interval=202942
		Case 8:PlayMusic"cuphead/CupMusic9.mp3", MusicVolume :m02.enabled=true:m02.interval=255216
		Case 9:PlayMusic"cuphead/CupMusic10.mp3", MusicVolume :m02.enabled=true:m02.interval=241848
    End Select
	m01.enabled=false
End Sub

Sub m02_Timer
    Dim zb
    zb = INT(10 * RND(1) )
    Select Case zb
		Case 0:PlayMusic"cuphead/CupMusic1.mp3", MusicVolume:m01.enabled=true:m01.interval=208049
		Case 1:PlayMusic"cuphead/CupMusic2.mp3", MusicVolume:m01.enabled=true:m01.interval=225284
		Case 2:PlayMusic"cuphead/CupMusic3.mp3", MusicVolume:m01.enabled=true:m01.interval=234633
		Case 3:PlayMusic"cuphead/CupMusic4.mp3", MusicVolume:m01.enabled=true:m01.interval=243706
		Case 4:PlayMusic"cuphead/CupMusic5.mp3", MusicVolume:m01.enabled=true:m01.interval=241058
		Case 5:PlayMusic"cuphead/CupMusic6.mp3", MusicVolume:m01.enabled=true:m01.interval=227391
		Case 6:PlayMusic"cuphead/CupMusic7.mp3", MusicVolume:m01.enabled=true:m01.interval=264930
		Case 7:PlayMusic"cuphead/CupMusic8.mp3", MusicVolume:m01.enabled=true:m01.interval=202942
		Case 8:PlayMusic"cuphead/CupMusic9.mp3", MusicVolume:m01.enabled=true:m01.interval=255216
		Case 9:PlayMusic"cuphead/CupMusic10.mp3", MusicVolume:m01.enabled=true:m01.interval=241848
    End Select
	m02.enabled=false
End Sub

Sub s01_Timer
    If PB=0 Then
     Dim zc
     zc = INT(4 * RND(1) )
     Select Case zc
     Case 0:PlaySound"CHballstart1":m01.enabled=true:m01.interval=2000
     Case 1:PlaySound"CHballstart2":m01.enabled=true:m01.interval=1818
     Case 2:PlaySound"CHballstart3":m01.enabled=true:m01.interval=1824
     Case 3:PlaySound"CHballstart4":m01.enabled=true:m01.interval=1704
     End Select
     s01.enabled=false
     End If
    If PB=1 Then
     Dim zd
     zd = INT(4 * RND(1) )
     Select Case zd
     Case 0:PlaySound"CHballstart1"
     Case 1:PlaySound"CHballstart2"
     Case 2:PlaySound"CHballstart3"
     Case 3:PlaySound"CHballstart4"
     End Select
     s01.enabled=false
     End If
End Sub

Dim AA
Dim PB
PB=0

'************************Bonus Timers

Sub Pause4Bonustimer_timer
	Pause4Bonustimer.enabled=0
	ScoreSoulBonus
End Sub

Sub NewBonusHolder_timer
	If NewBonusTimer.enabled=0 Then
		NewBonusHolder.enabled=0
		NextBallDelay.enabled=true
	End If

End Sub

'***********************
'     Flipper Logos
'***********************

Sub UpdateFlipperLogos_Timer
	LFlip.Rotz = LeftFlipper.CurrentAngle -121
	RFlip.Rotz = RightFlipper.CurrentAngle +121
	LFlipr.Rotz = LeftFlipper.CurrentAngle -121
	RFlipr.Rotz = RightFlipper.CurrentAngle +121
	PGate.Rotz = (Gate.CurrentAngle*.75) + 25
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
End Sub


'*********************** slingshots


Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFXDOF("right_slingshot",104,DOFPulse,DOFContactors), sling1
    RSling0.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -11
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	AddScore(10)
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -1
        Case 4:RSLing2.Visible = 0:RSling0.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFXDOF("left_slingshot",103,DOFPulse,DOFContactors), sling2
    LSling0.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -11
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	AddScore(10)
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -1
        Case 4:LSLing2.Visible = 0:LSLing0.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

'************************************ POP Bumpers

Sub Bumper1_Hit
	If TableTilted=false Then
		PlaySoundAt SoundFXDOF("bumper1",106,DOFPulse,DOFcontactors), Bumper1
		bump1 = 1
		If Bumper1Light.state = 1 Then
			AddScore(100)
		Else
			AddScore(10)
		End If
		
    End If

End Sub

Sub Bumper2_Hit
	If TableTilted=false Then
		PlaySoundAt SoundFXDOF("bumper1",107,DOFPulse,DOFcontactors), Bumper2
		bump3 = 1
		If Bumper2Light.state = 1 Then
			AddScore(100)
		Else
			AddScore(10)
		End If
		AlternateRelayFire
		If MultiballOn = 1 Then
			'PUP: trigger MULTIBALL event here  <<<<<<<<<<
			pupevent 326
			Callout.enabled=True
			'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<
			bumpercap2.image="bcap-100wl-unlit devilBW"
			Perditionpost.transz= -37
			PerditionWall.collidable = 0
			For x = 1 to 3
				EVAL("PerditionLight" & x).state = 0
				EVAL("FireLight" & x).state = 0
			Next
			Dim  BOT, b
			BOT = GetBalls
			For b = 0 to ubound(BOT)
				If BOT(b).x  > 790 and BOT(b).y < 800 Then Set KickerBall1 = BOT(b)
			Next
			Pkickarm1.rotz=15
			KickerArm1.enabled=true
			kickBall KickerBall1, 0, 45, 5, 30
			PostTimer.enabled = 1
			PlaySoundAt SoundFXDOF("saucer",131,DOFPulse,DOFcontactors), Pkickarm1
			PlaySound "CHPerditionKicker"
			MultiballReleased = 1
			MultiballOn = 0
			Bumper2Light.state = 1
			Bumper1Light.state = 1
			Bumper3Light.state = 1
			GIReset
		End If
    End If
End Sub

'PUP: hide callout event here  <<<<<<<<<<
Sub Callout_timer
	Callout.enabled=False
										  
	pupevent 330
																				  
End Sub
'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

Sub PostTimer_Timer
	PerditionWall.collidable = 1
	perditionpost.transz = 0
	PostTimer.enabled = 0
End Sub

Sub Bumper3_Hit
	If TableTilted=false Then
		PlaySoundAt SoundFXDOF("bumper1",108,DOFPulse,DOFcontactors), Bumper3
		bump2 = 1
		If Bumper3Light.state = 1 Then
			AddScore(100)
		Else
			AddScore(10)
		End If
    End If

End Sub

'**************************************** ROLLOVER TRIGGERS

Sub TriggerTopA_Hit()
	If TableTilted=false Then
		PlaySoundAt "sensor", ActiveBall
		PlaySound "CHBounce"
		DOF 120, DOFPulse
		SetMotor(100)
		SoulLight1.state=1
		CheckRollovers
		Special7
		If BonusMultiplier < 4 Then SoulLightArray(Player,1) = 1
	End If
End Sub

Sub TriggerTopB_Hit()
	If TableTilted=false Then
		DOF 119, DOFPulse
		PlaySoundAt "sensor", ActiveBall
		PlaySound "CHBounce"
		SetMotor(100)
		SoulLight2.state=1
		CheckRollovers
		Special7
		If BonusMultiplier < 4 Then SoulLightArray(Player,2) = 1
	End If
End Sub

Sub TriggerTopC_Hit()
	If TableTilted=false Then
		PlaySoundAt "sensor", ActiveBall
		PlaySound "CHBounce"
		DOF 121, DOFPulse
		SetMotor(100)
		SoulLight3.state=1
		CheckRollovers
		Special7
		If BonusMultiplier < 4 Then SoulLightArray(Player,3) = 1
	End If
End Sub

Sub TriggerTopD_Hit()
	If TableTilted=false Then
		PlaySoundAt "sensor", ActiveBall
		PlaySound "CHBounce"
		DOF 118, DOFPulse
		SetMotor(100)
		SoulLight4.state=1
		CheckRollovers
		Special7
		If BonusMultiplier < 4 Then SoulLightArray(Player,4) = 1
	End If
End Sub


Sub CheckRollovers
	If (SoulLight1.state=1) and (SoulLight2.state=1) and (SoulLight3.state=1) and (SoulLight4.state=1) Then
		Addscore(1000)
		BonusMultiplier=BonusMultiplier+1

		'PUP: trigger MULTIPLIER event here  <<<<<<<<<
		Select Case BonusMultiplier
			Case 2:
				pupevent 322
			Case 3:
				pupevent 323
			Case 4:
				pupevent 324
		End Select
		Callout.enabled=True
		'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

		For x = 1 to 4
			EVAL("SoulLight" & x).state = 0
			SoulLightArray(Player,x) = 0
		Next
	Else
		'PUP: trigger SOUL LANE event here  <<<<<<<
		pupevent 320
		'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<
	End If
	MultiplierBonus
	   

				
					 
End Sub

Sub MultiplierBonus
	If BonusMultiplier=2 Then
		Bonus2X.state=1
	End If
	If BonusMultiplier=3 Then
		Bonus3X.state=1
		Bonus2X.state=0
	End If
	If BonusMultiplier=4 Then
		Bonus4X.state=1
		Bonus3X.state=0
		SoulLight1.state=1
		SoulLight2.state=1
		SoulLight3.state=1
		SoulLight4.state=1
	End If
	If BonusMultiplier>4 Then
		BonusMultiplier=4
		Bonus4X.state=1
		SoulLight1.state=1
		SoulLight2.state=1
		SoulLight3.state=1
		SoulLight4.state=1
	End If
End Sub

'********************************LOWER ROLLOVERS
Sub TriggerUpperRightRollover_Hit()
	If TableTilted=false Then

								   
						  
		'PUP: Trigger PERDITION event here  <<<<<<<<<<
		pupevent 321
		'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

		DOF 133, DOFPulse
		PlaySoundAt "sensor", ActiveBall
		PlaySound "CHRollover"
		AddScore(100)
		PerditionEntry(Player) = PerditionEntry(Player) + 1
		If PerditionEntry(Player) > 6 Then PerditionEntry(Player) = 1
		If PerditionEntry(Player) mod 2 = 0 Then EVAL("PerditionLight" & (PerditionEntry(Player)/2)).state = 1	
	End If
	If PerditionEntry(Player) = 6 Then
		FireLight1.state=2
		FireLight2.state=2
		FireLight3.state=2
	End If
							   
					 
					 
					 
					 
													  
			   
	   
													   
			   
	   
									  
			   
	   
							   
			   
	   
							 
			   
	   
												 
			   
	   
												   
			   
	   
							 
			   
	   
							  
		
End Sub

Sub TriggerLowerRightRollover_Hit()
	If TableTilted=false Then
		DOF 134, DOFPulse
		PlaySoundAt "sensor", ActiveBall
		PlaySound "CHRollover"
		SetMotor(50)
	End If
End Sub


Sub TriggerLeftInlane_Hit()
	If TableTilted=false Then
		DOF 128, DOFPulse
		PlaySoundAt "sensor", ActiveBall
		PlaySound "CHInlanes"
		If LeftInlaneLight.state=1 Then
			SetMotor(500)
			LeftInlaneLight.state=0
		Else
			SetMotor(50)
		End If
	End If
End Sub


Sub TriggerRightInlane_Hit()
	If TableTilted=false Then
		DOF 129, DOFPulse
		PlaySoundAt "sensor", ActiveBall
		PlaySound "CHInlanes"
		If RightInlaneLight.state=1 Then
			SetMotor(500)
			RightInlaneLight.state=0
		Else
			SetMotor(50)
		End If
	End If
End Sub

Sub TriggerLeftOutlane_Hit()
	If TableTilted=false Then
		DOF 127, DOFPulse
		PlaySoundAt "sensor", ActiveBall
		PlaySound "CHOutlanes"
		If LeftOutlaneLight.state=1 Then
			SetMotor(1000)
			LeftOutlaneLight.state=0
		Else
			SetMotor(100)
		End If
	End If
End Sub

Sub TriggerRightOutlane_Hit()
	If TableTilted=false Then
		DOF 130, DOFPulse
		PlaySoundAt "sensor", ActiveBall
		PlaySound "CHOutlanes"
		If LeftOutlaneLight.state=1 Then
			SetMotor(1000)
			RightOutlaneLight.state=0
		Else
			SetMotor(100)
		End If
	End If
End Sub

Sub Trigger1_Hit
	BotchedMB = 0
	If MultiballOn Then MugBallLight.state = 2
	Set controlBall = ActiveBall
    contBallInPlay = True
End Sub

Sub Trigger1_Unhit
	MugBallLight.state = 0
	Trigger1.timerEnabled = 0
	PlungerPlaying = 0
End Sub

'*******************************STAR ROLLOVERS

Sub UpperLeftStar_Hit()
	If TableTilted=false Then
		DOF 135, 2
		If UpperLeftStarLight.state=2 Then
			SetMotor(500)
			LeftOutlaneLight.state=1
			RightOutlaneLight.state=1
			PlaySound"CHStars"
		Else
			SetMotor(50)
		End If
	End If
End Sub

Sub LowerLeftStar_Hit()
	If TableTilted=false Then
		DOF 136, 2
		If LowerLeftStarLight.state=2 Then
			SetMotor(500)
			PlaySound"CHStars"
		Else
			SetMotor(50)
		End If
	End If
	If SpecialLight.state=2 Then
		Credits=Credits+1
		PlaySound SoundFXDOF("knocker",117, DOFPulse, DOFKnocker)
		PlaySound "CHAlright"
		SpecialLight.state=0

		'PUP: update PUP DMD
		If usePUP Then
			PuPlayer.LabelSet dmdScreen,"Credits","CREDITS: " & CStr(Credits),1,""
		End If
		'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

	End If
End Sub

Sub RightStar_Hit()
	If TableTilted=false Then
		DOF 137, 2
		If RightStarLight.state=2 Then
			RightOutlaneLight.state=1
			LeftOutlaneLight.state=1
			SetMotor(500)
			PlaySound"CHStars"
		Else
			SetMotor(50)
		End If
	End If
End Sub

'*********************************** Special 7 Light1

Sub Special7()
	If (SpecialLight.state=2) and (SoulLight1.state=1) and (SoulLight2.state=1) and (SoulLight3.state=1) and (SoulLight4.state=1) Then
		PlaySound "CH7Special"
		Credits=Credits+1
		PlaySound SoundFXDOF("knocker",117, DOFPulse, DOFKnocker)
		SpecialLight.state=0

		'PUP: Update PUP DMD
		If usePUP Then
			PuPlayer.LabelSet dmdScreen,"Credits","CREDITS: " & CStr(Credits),1,""
		End If
		'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

	End If
End Sub


'*********************************** LoopLight Sequences

Sub LightSequence_timer
								
			  
	  
																				
													  
														  
																					 
		
End Sub

Sub LightSequence2_timer
								 
			  
	  
																				  
													   
															
																						
		
End Sub

'*********************************** KICKERS

Dim rkickstep, lkickstep, ktimer, kickFill, rKickerHit, lKickerHit


Dim kickBalls
Dim kickerBall1
Sub kickBall(kball, kangle, kvel, kvelz, kzlift)
	Dim rangle
	rangle = 3.14 * (kangle - 90) / 180
	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub


'****************************************************
' Kickers
'****************************************************
Dim PerditionEntry(4)
Sub Kicker1_Hit	
							  
	If TableTilted=false Then
		If PerditionEntry(Player) < 6 Then 
			Kicker1Hold.enabled=true
		ElseIf PerditionEntry(Player) = 6 Then
							 
											 
			SetMotor(300)
			PlaySound "CHPerditionKicker"
			If ColoredBall = 1 Then
				Dim BOT
				Bot = GetBalls
				BOT(0).color = RGB(255, 33, 0)
			End If
			Multiball.enabled = 1
		End If
	End If
End Sub

Dim RandomKicker
Sub PerditionFire_timer
	PerditionFire.enabled=False
	RandomKicker = int(RND * 4)
	If kicker1.ballcntover > 0 Then
		Pkickarm1.rotz=15
		KickerArm1.enabled=true
		kickBall KickerBall1, RandomKicker, 45, 5, 30
	PlaySoundAt SoundFXDOF("saucer",131,DOFPulse,DOFcontactors), Pkickarm1
	DOF 123, DOFPulse
	Else
	End If
End Sub


Sub Kicker1Hold_timer()
	Dim leftboosttemp
						 
		   
		
	Kicker1Hold.enabled=false
	leftboosttemp=0
	If PerditionLight1.state=1 Then
		leftboosttemp=leftboosttemp+1
	End If
	If PerditionLight2.state=1 Then
		leftboosttemp=leftboosttemp+1
	End If
	If PerditionLight3.state=1 Then
		leftboosttemp=leftboosttemp+1
	End If
	BonusBoosterCounter=leftboosttemp
	If BonusBoosterCounter>0 Then
		BonusBoost.enabled=true
	End If
	If TableTilted=false Then
		SetMotor(300)
	End If
							
	Kicker1.TimerEnabled=true
End Sub

Dim MultiballOn, MultiballReleased
Sub Multiball_Timer
	Ballrelease.CreateSizedBall 25
	MultiballOn = 1
	If ColoredBall =  1 Then
		Dim BOT
		Bot = GetBalls
		BOT(1).color = RGB(100, 150, 255)
	End If
    Ballrelease.Kick 40,7
    ' Thalamus added next
    PlaysoundAt "ballrelease", Plunger
	DOF 112, DOFPulse
	perditionpost.transz = 0
	PerditionWall.collidable = 1
	Multiball.enabled = 0
	bumpercap2.image="bcap-100wl-unlit devil"
	Bumper2Light.state = 2
	Bumper1Light.state = 0
	Bumper3Light.state = 0
	GISet
	PerditionEntry(Player) = 0
End Sub

Sub Kicker1_Timer()
	RandomKicker = int(RND * 4)
	If kicker1.ballcntover > 0 Then
		Dim  BOT, b
		BOT = GetBalls
		For b = 0 to ubound(BOT)
			If BOT(b).x  > 790 and BOT(b).y < 800 Then Set KickerBall1 = BOT(b)
		Next
		Pkickarm1.rotz=15
		KickerArm1.enabled=true
		kickBall KickerBall1, RandomKicker, 45, 5, 30
		PlaySoundAt SoundFXDOF("saucer",131,DOFPulse,DOFcontactors), Pkickarm1
	End If
				   
	Kicker1.TimerEnabled = False
End Sub

Sub KickerArm1_timer
					
	Pkickarm1.rotz=0
	KickerArm1.enabled=false
End Sub

Sub Kicker2_Hit
	set kickerBall1 = activeball
	If TableTilted=false Then
		Kicker2Hold.enabled=true
	Else
		Kicker2.TimerEnabled=true
	End If
	If RightExtraBallLight.state=1 Then
		PlaySound "CHExtraLifeAward"
		ExtraBall=True
		SamePlayerShootsAgain.state=1
		RightExtraBallLight.state=0
		If B2SOn Then
			Controller.B2SSetShootAgain 1
		End If
	End If
End Sub

Sub Kicker2Hold_timer()
	Dim rightboosttemp
	If MotorRunning<>0 Then
		exit sub
	End If
	Kicker2Hold.enabled=false
	rightboosttemp=0
	If PerditionLight1.state=1 Then
		rightboosttemp=rightboosttemp+1
	End If
	If PerditionLight2.state=1 Then
		rightboosttemp=rightboosttemp+1
	End If
	If PerditionLight3.state=1 Then
		rightboosttemp=rightboosttemp+1
	End If

	BonusBoosterCounter=rightboosttemp
	If BonusBoosterCounter>0 Then
		BonusBoost.enabled=true
	End If
	If TableTilted=false Then
		SetMotor(1000 + (1000 * rightboosttemp))
	End If
	Kicker2.TimerInterval=1000
	Kicker2.TimerEnabled=true
End Sub

Sub Kicker2_Timer()
	Kicker2.TimerEnabled=false
	RandomKicker = int(RND * 10)
	If kicker2.ballcntover > 0 Then
		Pkickarm2.rotz=15
		KickerArm2.enabled=true
		kickBall KickerBall1, (315 + RandomKicker), (15 + RandomKicker), 5, 25
	PlaySoundAt SoundFXDOF("saucer",132,DOFPulse,DOFcontactors), Pkickarm2
	Else
	End If
End Sub
	   

Sub KickerArm2_timer
					
	Pkickarm2.rotz=0
	KickerArm2.enabled=false
End Sub

Sub Kicker3_Hit()
	'PUP: trigger DASH event here  <<<<<<<<<<<<<<<
	pupevent 325
	Callout.enabled=True
	'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

	set kickerBall1 = activeball
	Kicker3.TIMERINTERVAL = 1000
	Kicker3.TIMERENABLED = TRUE
	If TableTilted=false Then
		SetMotor(100)
	End If
	If RightDashLight.state=1 Then
		SetMotor(500)
	End If
End Sub


				
Sub Kicker3_Timer()
	Kicker3.TIMERENABLED = FALSE
	RandomKicker = int(RND * 3)
						
	If kicker3.ballcntover > 0 Then
		kickBall KickerBall1, (307 + RandomKicker), 32.5, 5, 25
		PlaySoundAt SoundFXDOF("saucer",123,DOFPulse,DOFcontactors), Kicker3
		PlaySound "CHLowerKickers"
	Else
	End If
End Sub

Sub Kicker4_Hit()
	'PUP: trigger DASH event here  <<<<<<<<<<<<<<<
	pupevent 325
	Callout.enabled=True
	'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

	set kickerBall1 = activeball
	Kicker4.TIMERINTERVAL = 500
	Kicker4.TIMERENABLED = TRUE
	If TableTilted=false Then
		SetMotor(100)
	End If
	If LeftDashLight.state=1 Then
		SetMotor(500)
	End If
End Sub

Sub Kicker4_Timer()
	Kicker4.TIMERENABLED = FALSE
	RandomKicker = int(RND * 2)
	If kicker4.ballcntover > 0 Then
		kickBall KickerBall1, (56 + RandomKicker), 32, 5, 25
		PlaySoundAt SoundFXDOF("saucer",124,DOFPulse,DOFcontactors), Kicker4
		PlaySound "CHLowerKickers"
		'PUP: trigger release DASH event here  <<<<<<<<<<<<<<<
		pupevent 329
		'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<
	Else
	End If
End Sub



'****************  DROP Targets CUP & Mug

Dim zMultiplier: zMultiplier = 2.2

Dim DTR(3), DTL(3)


Sub DT1_hit
	DTL(1) = 1
	PlaySoundAt "droptargetdropped", DT1
	DOF 113, DOFPulse
	addscore(100)
	DTcheck
	ShadowCup1.visible=False
	activeball.velz = activeball.velz*zMultiplier
End Sub

Sub DT2_hit
	DTL(2) = 1
	PlaySoundAt "droptargetdropped", DT2
	DOF 114, DOFPulse
	addscore(100)
	DTcheck
	ShadowCup2.visible=False
	activeball.velz = activeball.velz*zMultiplier
End Sub

Sub DT3_hit
	DTL(3) = 1
	PlaySoundAt "droptargetdropped", DT3
	DOF 115, DOFPulse
	addscore(100)
	DTcheck
	ShadowCup3.visible=False
	activeball.velz = activeball.velz*zMultiplier
End Sub

Sub DT4_dropped
	DTR(1) = 1
	PlaySoundAt "droptargetdropped", DT4
	DOF 109, DOFPulse
	addscore(100)
	DTcheck2
	ShadowMug1.visible=False
	activeball.velz = activeball.velz*zMultiplier
End Sub

Sub DT5_dropped
	DTR(2) = 1
	PlaySoundAt "droptargetdropped", DT5
	DOF 110, DOFPulse
	addscore(100)
	DTcheck2
	ShadowMug2.visible=False
	activeball.velz = activeball.velz*zMultiplier
End Sub

Sub DT6_dropped
	DTR(3) = 1
	PlaySoundAt "droptargetdropped", DT6
	DOF 111, DOFPulse
	addscore(100)
	DTcheck2
	ShadowMug3.visible=False
	activeball.velz = activeball.velz*zMultiplier
End Sub

Sub DTcheck
	If DTL(1) = 1 and DTL(2) = 1 and DTL(3) = 1 Then 
		DT1.timerenabled = 1
	End If
End Sub

Sub DTcheck2
	If DTR(1) = 1 and DTR(2) = 1 and DTR(3) = 1 Then DT2.timerenabled=1
End Sub

Sub DT1_timer
	'PUP: trigger BOSS DEFEAT event here  <<<<<<<<
	If SoulBonusCounter<10 Then
		pupevent INT("2" & CStr(SoulBonusCounter) & "2")
	Else 
		If SoulBonusCounter = 10 Then
			pupevent 301
		Else
			pupevent INT("2" & CStr(SoulBonusCounter-10) & "2")
		End If
	End If
	'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

	playsoundat "droptargetreset", DT1
	For i = 1 to 3
		DTL(i) = 0
		EVAL("DT" & i).isdropped = False
	Next
	DT1.timerenabled=0
	IncreaseSoulBonus
	For Each obj in ShadowCup
		obj.visible=True
	Next

	'PUP: trigger next BOSS FIGHT event here <<<<<<<
	If SoulBonusCounter<10 Then
		pupevent INT("2" & CStr(SoulBonusCounter) & "1")
	Else
		'PUP: trigger FINAL BOSS Wizard Mode
		If SoulBonusCounter = 10 Then
			bumpercap2.image="bcap-100wl-unlit devil"
			Bumper2Light.state = 2
			Bumper1Light.state = 0
			Bumper3Light.state = 0
			FireLight1.state=2
			FireLight2.state=2
			FireLight3.state=2
			GISet
			pupevent 300
		Else
			pupevent INT("2" & CStr(SoulBonusCounter-10) & "1")
		End If
	End If
	'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<
End Sub

Sub DT2_timer
	'PUP: trigger BOSS DEFEAT event here  <<<<<<<<<
	If SoulBonusCounter<10 Then
		pupevent INT("2" & CStr(SoulBonusCounter) & "2")
	Else
		'PUP: trigger FINAL BOSS DEFEAT event here <<<<
		If SoulBonusCounter = 10 Then
			pupevent 301
		Else
			pupevent INT("2" & CStr(SoulBonusCounter-10) & "2")
		End If
	End If
	'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

	playsoundat "droptargetreset", DT2
	For i = 1 to 3
		EVAL("DT"&i+3).isdropped=False
		DTR(i) = 0
	Next

	DT2.timerenabled=0
	IncreaseSoulBonus
	For Each obj in ShadowMug
		obj.visible=True
	Next

	'PUP: trigger next BOSS FIGHT event here <<<<<<<<<
	If SoulBonusCounter<10 Then
		pupevent INT("2" & CStr(SoulBonusCounter) & "1")
	Else 
		'PUP: trigger FINAL BOSS Wizard Mode <<<<<<<<<
		If SoulBonusCounter = 10 Then
			bumpercap2.image="bcap-100wl-unlit devil"
			Bumper2Light.state = 2
			Bumper1Light.state = 0
			Bumper3Light.state = 0
			FireLight1.state=2
			FireLight2.state=2
			FireLight3.state=2
			GISet
			pupevent 300
		Else
			pupevent INT("2" & CStr(SoulBonusCounter-10) & "1")
		End If
	End If
	'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

End Sub


'************************ Cuphead Rubber 
Sub phys_bands001_hit
	AddScore(10)
	If ShiftControl = 1 Then SoulLightsLeft
End Sub

'************************ Mugman Rubber 
Sub phys_bands002_hit
	AddScore(10)
	If ShiftControl = 1 Then SoulLightsRight
End Sub


'************************ STANDUP Targets

Sub StandUpRight_hit()
	PlaySoundAtBall "target"
	PlaySound "CHStandups"
	DOF 125, DOFPulse
	AddScore(10)
	RightInlaneLight.state=1
	LeftInlaneLight.state=1
	RightStarLight.state=2
	UpperLeftStarLight.state=2
	LowerLeftStarLight.state=2
End Sub

Sub StandUpLeft1_hit()
	PlaySoundAtBall "target"
	PlaySound "CHStandups"
	DOF 138, DOFPulse
	AddScore(10)
	LeftStandupLight1.state=1
	CheckStandups
End Sub

Sub StandUpLeft2_hit()
	PlaySoundAtBall "target"
	PlaySound "CHStandups"
	DOF 139, DOFPulse
	AddScore(10)
	LeftStandupLight2.state=1
	CheckStandups
End Sub

Sub StandUpLeft3_hit()
	PlaySoundAtBall "target"
	PlaySound "CHStandups"
	DOF 140, DOFPulse
	AddScore(10)
	LeftStandupLight3.state=1
	CheckStandups
End Sub

Sub CheckStandups
	If LeftStandupLight1.state=1 and LeftStandupLight2.state=1 and LeftStandupLight3.state=1 Then
		LeftDashLight.state=1
		RightDashLight.state=1
	End If
End Sub



'********************** Spinner1
Sub Spinner1_Spin()
	AddScore(10)
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner1), 0.25, 0, 0, 1, AudioFade(Spinner1)
	PlaySound "CHWhip"
End Sub

				   
			  
	   

'********************** Relay

Sub AlternateRelayFire
	RightExtraBallLight.state=0
End Sub

							
						  
						  
						  
				   
		   
 
	  
						  
						  
						  
				   
		
	   

Sub AddSpecial()
	PlaySound SoundFXDOF("knocker",117, DOFPulse, DOFKnocker)
	Credits=Credits+1
	If Credits>15 Then Credits=15
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	CreditsReel.SetValue(Credits)

	'PUP: Update PUP DMD
	If usePUP Then
		PuPlayer.LabelSet dmdScreen,"Credits","CREDITS: " & CStr(Credits),1,""
	End If
	'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<
End Sub

Sub AddSpecial2()
	PlaySound"click"
	Credits=Credits+1
	DOF 105, DOFOn
	If Credits>15 Then Credits=15
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	CreditsReel.SetValue(Credits)

	'PUP: Update PUP DMD
	If usePUP Then
		PuPlayer.LabelSet dmdScreen,"Credits","CREDITS: " & CStr(Credits),1,""
	End If
	'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<
End Sub

Sub ResetBallDrops()

	RightExtraBallLight.state=0
	SnakeEyesLight.state=0
	SoulBonusCounter=0
	BonusMultiplierCounter=0
	BonusCounter=0
	SpecialLight.state=0
	For Each obj in BonusX
		obj.state=0
	Next
	For Each obj in SoulBonus
		obj.state=0
	Next
	For Each obj in TopRolloverLights
		obj.state=0
	Next
								  
			  
	  
								   
			  
	  
	For Each obj in StandUpsDash
		obj.state=0
	Next
	For Each obj in DashLights
		obj.state=0
	Next
	For Each obj in StarLights
		obj.state=0
	Next
	For Each obj in LoopLightsLeft
	Next
	For Each obj in LoopLightsRight
		obj.state=0
	Next
	For Each obj in RolloverLightsLeft
		obj.state=0
	Next
	For Each obj in RolloverLightsRight
		obj.state=0
	Next
	For Each obj in SlingLights
		obj.state=0
	Next
	For Each obj in DropTargetsCUP
		obj.IsDropped=0
		DOF 116, DOFPulse
	Next
	For x = 1 to 3
		DTR(x) = 0
		DTL(x) = 0
	Next
	For Each obj in DropTargetsMUG
		obj.IsDropped=0
		DOF 116, DOFPulse
	Next
End Sub


Sub ResetBalls()
	TempMultiCounter=BallsPerGame-BallInPlay
	ResetBallDrops
	BonusMultiplier=1
	For x = 1 to 3
		If PerditionEntry(Player) >= (x * 2) Then 
			EVAL("PerditionLight" & x).state = 1
		Else
			EVAL("PerditionLight" & x).state = 0
		End If
	Next
	If LastBallDoubleBonus = 1 Then
		If BallInPlay = BallsPerGame Then BonusMultiplier = 2: Bonus2X.state = 1
	End If
	Bumper1Light.State=1
	Bumper2Light.State=1
	Bumper3Light.State=1
	SnakeEyesLight.state=0
	If SoulLightReset =  1 Then
		For x = 1 to 4
			If SoulLightArray(Player,x) = 1 Then 
				EVAL("SoulLight" & x).state = 1
			Else
				EVAL("SoulLight" & x).state = 0
			End If
		Next
	End If
	BumpersOn
	For Each obj in StarLights
		obj.state=1
	Next
	SoulBonusCounter=0
	SoulBonus(SoulBonusCounter).state=1
	TableTilted=false
	TiltReel.SetValue(0)
	If B2Son Then
		Controller.B2SSetTilt 0
	End If
	PlasticsOn
	Ballrelease.CreateSizedBall 25
    Ballrelease.Kick 40,7
    ' Thalamus added next
    PlaysoundAt "ballrelease", Plunger
	DOF 112, DOFPulse
	BallInPlayReel.SetValue(BallInPlay)
End Sub

'**************************Soul Lights Right
Dim x, y
Dim TempSoulR(5), TempSoulL(5)
Sub SoulLightsRight
	For x = 1 to 4
		If EVAL("SoulLight" & x).state = 1 Then TempSoulR(x + 1) = 1
	Next
	For x = 1 to 4
		If TempSoulR(x) = 1 Then
			EVAL("SoulLight" & x).state = 1
			SoulLightArray(Player,x) = 1
		Else
			EVAL("SoulLight" & x).state = 0
			SoulLightArray(Player,x) = 0
		End If
	Next
	If TempSoulR(5) = 1 Then 
		SoulLight1.state = 1
		SoulLightArray(Player,1) = 1
	Else	
		SoulLight1.state = 0
		SoulLightArray(Player,1) = 0
	End If
	For x = 0 to 5
		TempSoulR(x) = 0
	Next
End Sub

'**************************Soul Lights Left 
Sub SoulLightsLeft
	For x = 1 to 4
		If EVAL("SoulLight" & x).state = 1 Then TempSoulL(x - 1) = 1
	Next
	For x = 1 to 4
		If TempSoulL(x) = 1 Then
			EVAL("SoulLight" & x).state = 1
			SoulLightArray(Player,x) = 1
		Else
			EVAL("SoulLight" & x).state = 0
			SoulLightArray(Player,x) = 0
		End If
	Next
	If TempSoulL(0) = 1 Then 
		SoulLight4.state = 1
		SoulLightArray(Player,4) = 1
	Else	
		SoulLight4.state = 0
		SoulLightArray(Player,4) = 0
	End If
	For x = 0 to 5
		TempSoulL(x) = 0
	Next
End Sub

'**************************NEW SCORE Bonus

Sub IncreaseSoulBonus
	If SoulBonusCounter<15 Then
		If SoulBonusCounter<10 Then
			SoulBonus(SoulBonusCounter).state=0
			SoulBonusCounter=SoulBonusCounter+1
			SoulBonus(SoulBonusCounter).state=1
		elseIf SoulBonusCounter>9 Then
			SoulBonus(SoulBonusCounter-10).state=0
			SoulBonusCounter=SoulBonusCounter+1
			SoulBonus(10).state=1
			SoulBonus(SoulBonusCounter-10).state=1
		End If
	End If
	If SoulBonusCounter > 4 Then
		RightExtraBallLight.state=1
		PlaySound "CHExtraLifeActive"
	End If
	If SoulBonusCounter = 7 Then
		SpecialLight.state = 2
	End If
	SoulSounds
End Sub

Sub ScoreBonusMultiplier

	If Bonus2X.state=0 Then
		ScoreMotorStepper=0
	Else
		ScoreMotorStepper=0
	End If
	CollectBonusMultiplier.enabled=1
	If Bonus3X.state=0 Then
		ScoreMotorStepper=0
	Else
		ScoreMotorStepper=0
	End If
	CollectBonusMultiplier.enabled=1
	If Bonus4X.state=0 Then
		ScoreMotorStepper=0
	Else
		ScoreMotorStepper=0
	End If
	CollectBonusMultiplier.enabled=1
End Sub

Sub CollectBonusMultiplier_timer
	If MotorRunning=1 Then
		exit sub
	End If
	If SoulBonusCounter<1 Then
		CollectBonusMultiplier.enabled=0
		SoulBonusCounter=1
		SoulBonus(SoulBonusCounter).state=1
	Else
		If ScoreMotorStepper<5 Then
		If TableTilted=false Then
				SetMotor(5000)
			End If
			If Bonus2X.state=0 Then
				If TableTilted=false Then
					SetMotor(5000)
				End If
			Else
				If TableTilted=false Then
					AddScore(10000)
				End If
			End If
			SoulBonus(SoulBonusCounter).state=0
			SoulBonusCounter=SoulBonusCounter-1
			If SoulBonusCounter>=0 Then
				SoulBonus(SoulBonusCounter).state=1
			End If
			ScoreMotorStepper=ScoreMotorStepper+1
		Else
			If Bonus2X.state=0 Then
				ScoreMotorStepper=0
			Else
				ScoreMotorStepper=0
			End If
			If Bonus3X.state=0 Then
				ScoreMotorStepper=0
			Else
				ScoreMotorStepper=0
			End If
			If Bonus4X.state=0 Then
				ScoreMotorStepper=0
			Else
				ScoreMotorStepper=0
			End If
		End If
	End If
End Sub

Sub ScoreSoulBonus
	ScoreMotorStepper=0
	CollectSoulBonus.interval=135
	CollectSoulBonus.enabled=1
End Sub

Sub CollectSoulBonus_timer
	If MotorRunning=1 Then
		exit sub
	End If
	If SoulBonusCounter<1 Then
		CollectSoulBonus.enabled=0
		NextBallDelay.enabled=true
	Else
		If Bonus2X.state=1 Then
			Select case ScoreMotorStepper
				case 0,1,3,4:
					AddScore(1000)

				case 2,5:
					PlaySound"pinhit_low"
					If SoulBonusCounter>10 Then
						SoulBonus(SoulBonusCounter-10).state=0
					Else
						SoulBonus(SoulBonusCounter).state=0
					End If

					SoulBonusCounter=SoulBonusCounter-1
					If SoulBonusCounter>=0 Then
						If SoulBonusCounter<=10 Then
							SoulBonus(SoulBonusCounter).state=1
						ElseIf SoulBonusCounter>10 Then
							SoulBonus(10).state=1
							SoulBonus(SoulBonusCounter-10).state=1
						End If
					End If

			end select
			ScoreMotorStepper=ScoreMotorStepper+1
			If ScoreMotorStepper>5 Then
				ScoreMotorStepper=0
			End If
		ElseIf Bonus3x.state=1 Then
			Select case ScoreMotorStepper
				case 0,1,2,4,5,6:
					AddScore(1000)

				case 3,7:
					PlaySound"pinhit_low"
					If SoulBonusCounter>10 Then
						SoulBonus(SoulBonusCounter-10).state=0
					Else
						SoulBonus(SoulBonusCounter).state=0
					End If

					SoulBonusCounter=SoulBonusCounter-1
					If SoulBonusCounter>=0 Then
						If SoulBonusCounter<=10 Then
							SoulBonus(SoulBonusCounter).state=1
						ElseIf SoulBonusCounter>10 Then
							SoulBonus(10).state=1
							SoulBonus(SoulBonusCounter-10).state=1
						End If
					End If

			end select
			ScoreMotorStepper=ScoreMotorStepper+1
			If ScoreMotorStepper>7 Then
				ScoreMotorStepper=0
			End If
		ElseIf Bonus4x.state=1 Then
			Select case ScoreMotorStepper
				case 0,1,2,3,5,6,7,8:
					AddScore(1000)

				case 4,9:
					PlaySound"pinhit_low"
					If SoulBonusCounter>10 Then
						SoulBonus(SoulBonusCounter-10).state=0
					Else
						SoulBonus(SoulBonusCounter).state=0

					End If

					SoulBonusCounter=SoulBonusCounter-1
					If SoulBonusCounter>=0 Then
						If SoulBonusCounter<=10 Then
							SoulBonus(SoulBonusCounter).state=1
						ElseIf SoulBonusCounter>10 Then
							SoulBonus(10).state=1
							SoulBonus(SoulBonusCounter-10).state=1
						End If
					End If
			end select

			ScoreMotorStepper=ScoreMotorStepper+1
			If ScoreMotorStepper>9 Then
				ScoreMotorStepper=0
			End If
		Else
			Select Case ScoreMotorStepper
				case 0,1,2,3,4:
					AddScore(1000)
					If SoulBonusCounter>10 Then
						SoulBonus(SoulBonusCounter-10).state=0
					Else
						SoulBonus(SoulBonusCounter).state=0

					End If
					SoulBonusCounter=SoulBonusCounter-1
					If SoulBonusCounter>=0 Then
						If SoulBonusCounter<=10 Then
							SoulBonus(SoulBonusCounter).state=1
						ElseIf SoulBonusCounter>10 Then
							SoulBonus(10).state=1
							SoulBonus(SoulBonusCounter-10).state=1
						End If
					End If
				case 5:
					PlaySound"pinhit_low"

			end select
			ScoreMotorStepper=ScoreMotorStepper+1
			If ScoreMotorStepper>7 Then
				ScoreMotorStepper=0
			End If
		End If
	End If
End Sub

'********************************* SOUL SOUNDS

Sub SoulSounds
	If Bonus1.state=1 Then
		PlaySound "SoulMusic1"
	ElseIf Bonus2.state=1 Then
		PlaySound "SoulMusic2"
	ElseIf Bonus3.state=1 Then
		PlaySound "SoulMusic3"
	ElseIf Bonus4.state=1 Then
		PlaySound "SoulMusic4"
	ElseIf Bonus5.state=1 Then
		PlaySound "SoulMusic5"
	ElseIf Bonus6.state=1 Then
		PlaySound "SoulMusic6"
	ElseIf Bonus7.state=1 Then
		PlaySound "SoulMusic7"
	ElseIf Bonus8.state=1 Then
		PlaySound "SoulMusic8"
	ElseIf Bonus9.state=1 Then
		PlaySound "SoulMusic9"
	ElseIf Bonus10.state=1 Then
		PlaySound "SoulMusic10"
	End If
End Sub

'********************************GI Light Subs
Sub GISet
	For Each obj in Flashers
		obj.color = RGB(255,0,0)
		obj.colorfull = RGB(255,0,0)		
	Next
	
	For Each obj in Flashers2
		obj.color = RGB(255,0,0)
	Next
End Sub

Sub GIReset
	For Each obj in Flashers
		obj.color = RGB(255,128,0)
		obj.colorfull = RGB(255,197,143)
		obj.state = 1
	Next
	
	For Each obj in Flashers2
		obj.color = RGB(128,0,0)
		obj.colorfull = RGB(255,0,0)
		obj.state = 1
	Next
End Sub

Sub GIOff
	For Each obj in Flashers
		obj.state = 0
	Next
	For Each obj in Flashers2
		obj.state = 0
	Next
End Sub


'*********************************

Sub resettimer_timer
    rst=rst+1
    For i=1 to 4
 	If B2SOn Then
		Controller.B2SSetScorePlayer i, 0
	End If
	Next
    If rst=20 Then
    playsound "StartBall1"
    End If
    If rst=24 Then
    newgame
    resettimer.enabled=false
    End If
End Sub

'***********************************

Sub NextBallDelay_timer()
	NextBallDelay.enabled=false
	Nextball
End Sub

Sub newgame
	InProgress=true
	queuedscore=0
	For i = 1 to 4
		Score(i)=0
		Score100K(1)=0
		HighScorePaid(i)=false
		Replay1Paid(i)=false
		Replay2Paid(i)=false
		Replay3Paid(i)=false
	Next
	If B2SOn Then
		Controller.B2SSetTilt 0
		Controller.B2SSetGameOver 0
		Controller.B2SSetMatch 0
												   
												   
												   
												   
		Controller.B2SSetBallInPlay BallInPlay
		Controller.B2SSetShootAgain 0
	End If
	Bumper1Light.state=1
	Bumper2Light.state=1
	Bumper3Light.state=1
	AlternateRelay=1
	BumpersOn
	For x = 1 to 4
		PerditionEntry(x) = 1
		For y = 1 to 4
			SoulLightArray(x,y) = 0
		Next
	Next
	ResetBalls
	For x = 1 to 3
		EVAL("PerditionLight" & x).state = 0
	Next
End Sub

Sub nextball
	If B2SOn Then
		Controller.B2SSetTilt 0
		Controller.B2SSetShootAgain 0
	End If
	If Bonus4X.state = 1 Then
		For x = 1 to 4
			SoulLightArray(Player, x) = 0
		Next
	End If
	If ExtraBall=false Then
		Player=Player+1
	Else
		ExtraBall=false
		SamePlayerShootsAgain.state=0
	End If
	If Player>Players Then
		BallInPlay=BallInPlay+1
		If BallInPlay>BallsPerGame Then
			'PUP: trigger GAMEOVER event here  <<<<<<<<<<<
			pupevent 366
			'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

			PlaySound("MotorLeer")
			InProgress=false
			If B2SOn Then
				Controller.B2SSetGameOver 1
				Controller.B2SSetPlayerUp 0
				Controller.B2SSetBallInPlay 0
				Controller.B2SSetCanPlay 0
			End If
											 
					
		
													
				
		
			If Table1.ShowDT = True Then
				For Each obj in PlayerScores
					obj.visible=1
				Next
				For Each obj in PlayerScoresOn
					obj.visible=0
				Next
			End If
			BallInPlayReel.SetValue(0)
			CanPlayReel.SetValue(0)
			GameOverReel.SetValue(1)
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			LightsOut
			PlasticsOff
			BumpersOff
			For Each obj in StarLights
				obj.state=0
			Next
			checkmatch
			CheckHighScore
			Players=0

			' PUP:
			If usePUP Then
				PuPlayer.LabElset dmdScreen,"Players","PLAYERS: " & CStr(Players),1,""
			End If
			'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

			HighScoreTimer.interval=100
			HighScoreTimer.enabled=True
		Else
			Player=1
			If B2SOn Then
				Controller.B2SSetPlayerUp Player
				Controller.B2SSetBallInPlay BallInPlay
			End If
			PlaySound("RotateThruPlayers")
			TempPlayerUp=Player
			PlayerUpRotator.enabled=true
			PlayStartBall.enabled=true
											 
					
		
													
				
		
			If Table1.ShowDT = True Then
				For Each obj in PlayerScores
					obj.visible=1
				Next
				For Each obj in PlayerScoresOn
					obj.visible=0
				Next
													   
									  
				PlayerScores(Player-1).visible=0
				PlayerScoresOn(Player-1).visible=1
			End If

			ResetBalls
		End If
	Else
		If B2SOn Then
			Controller.B2SSetPlayerUp Player
			Controller.B2SSetBallInPlay BallInPlay
		End If
		PlaySound("RotateThruPlayers")
		TempPlayerUp=Player
		PlayerUpRotator.enabled=true
		PlayStartBall.enabled=true
							 
				   
	   
												   
				
		
		If Table1.ShowDT = True Then
			For Each obj in PlayerScores
					obj.visible=1
			Next
			For Each obj in PlayerScoresOn
					obj.visible=0
			Next
'			PlayerHuds(Player-1).SetValue(1)
'			PlayerHUDScores(Player-1).state=1
			PlayerScores(Player-1).visible=0
			PlayerScoresOn(Player-1).visible=1
		End If
		ResetBalls
	End If

	If InProgress Then
		If Not usePUPDMD Then
			PuPlayer.LabelSet dmdScreen,"Ball","BALL: " & CStr(BallInPlay),1,""
		End If
		Select Case Player
			Case 1:
				'PUP: trigger PLAYER ONE event here  <<<<<<<<<<<
				Select Case BallInPlay
					Case 1:
						pupevent 410
					Case 2:
						pupevent 411
					Case 3:
						pupevent 412
					Case 4:
						pupevent 413
					Case 5:
						pupevent 414
				End Select
				'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<
			Case 2:
				'PUP: trigger PLAYER TWO event here  <<<<<<<<<<<
				Select Case BallInPlay
					Case 1:
						pupevent 415
					Case 2:
						pupevent 416
					Case 3:
						pupevent 417
					Case 4:
						pupevent 418
					Case 5:
						pupevent 419
				End Select
				'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<
		End Select
	End If
End Sub

Sub CheckHighScore
	Dim playertops
		Dim si
	Dim sj
	Dim stemp
	Dim stempplayers
	For i=1 to 4
		sortscores(i)=0
		sortplayers(i)=0
	Next
	playertops=0
	For i = 1 to Players
		sortscores(i)=Score(i)
		sortplayers(i)=i
	Next

	For si = 1 to Players
		For sj = 1 to Players-1
			If sortscores(sj)>sortscores(sj+1) Then
				stemp=sortscores(sj+1)
				stempplayers=sortplayers(sj+1)
				sortscores(sj+1)=sortscores(sj)
				sortplayers(sj+1)=sortplayers(sj)
				sortscores(sj)=stemp
				sortplayers(sj)=stempplayers
			End If
		Next
	Next
	ScoreChecker=4
	CheckAllScores=1
	NewHighScore sortscores(ScoreChecker),sortplayers(ScoreChecker)
	savehs
End Sub

	   


Sub checkmatch
	Dim tempmatch
	tempmatch=Int(Rnd*10)
	Match=tempmatch*10
	MatchReel.SetValue(tempmatch+1)
	If B2SOn Then
		If Match = 0 Then
			Controller.B2SSetMatch 100
		Else
			Controller.B2SSetMatch Match
		End If
	End If
	For i = 1 to Players
		If Match=(Score(i) mod 100) Then
			AddSpecial
		End If
	Next
End Sub

Sub TiltTimer_Timer()
	If TiltCount > 0 Then TiltCount = TiltCount - 1
	If TiltCount = 0 Then
		TiltTimer.Enabled = False
	End If
End Sub

Sub TiltIt()
		TiltCount = TiltCount + 1
		If TiltCount = 3 Then
			TableTilted=True
			TiltReel.SetValue(1)
			PlasticsOff
			BumpersOff
			For Each obj in StarLights
				obj.state=0
			Next
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			If B2Son Then
				Controller.B2SSetTilt 1
			End If
		Else
			TiltTimer.Interval = 500
			TiltTimer.Enabled = True
		End If

End Sub

Sub BonusBoost_Timer()
'	IncreaseBonus
	BonusBoosterCounter=BonusBoosterCounter-1
	If BonusBoosterCounter=0 Then
		BonusBoost.enabled=false
	End If

End Sub

Sub PlayStartBall_timer()
	PlayStartBall.enabled=false
	PlaySound("StartBall2-5")
End Sub

Sub PlayerUpRotator_timer()
		If RotatorTemp<5 Then
			TempPlayerUp=TempPlayerUp+1
			If TempPlayerUp>4 Then
				TempPlayerUp=1
			End If
											 
					
		
													
				
		
			If Table1.ShowDT = True Then
				For Each obj in PlayerScores
					obj.visible=1
				Next
				For Each obj in PlayerScoresOn
					obj.visible=0
				Next
																			
																			 
				PlayerScores(TempPlayerUp-1).visible=0
				PlayerScoresOn(TempPlayerUp-1).visible=1
			End If
			If B2SOn Then
				Controller.B2SSetPlayerUp TempPlayerUp
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetData 80+TempPlayerUp,1
			End If

		Else
			If B2SOn Then
				Controller.B2SSetPlayerUp Player
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetData 80+Player,1
			End If
			PlayerUpRotator.enabled=false
			RotatorTemp=1
											 
					
		
													
				
		
			If Table1.ShowDT = True Then
				For Each obj in PlayerScores
					obj.visible=1
				Next
				For Each obj in PlayerScoresOn
					obj.visible=0
				Next
													   
									  
				PlayerScores(Player-1).visible=0
				PlayerScoresOn(Player-1).visible=1
			End If
		End If
		RotatorTemp=RotatorTemp+1
End Sub

Sub savehs
	' Based on Black's Highscore routines
	Dim FileObj
	Dim ScoreFile
	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then
		Exit Sub
	End If
	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & HSFileName,True)
		ScoreFile.WriteLine
		ScoreFile.WriteLine MusicVolume
		ScoreFile.WriteLine Credits
		scorefile.writeline BallsPerGame
		scorefile.writeline ExtraBallSetting
		scorefile.writeline ReplayLevel
		For xx=1 to 5
			scorefile.writeline HSScore(xx)
		Next
		For xx=1 to 5
			scorefile.writeline HSName(xx)
		Next

		ScoreFile.Close
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub

Sub loadhs
    ' Based on Black's Highscore routines
	Dim FileObj
	Dim ScoreFile
    dim temp1
    dim temp2
	dim temp3
	dim temp4
	dim temp5
	dim temp6
	dim temp7
	dim temp8
	dim temp9
	dim temp10
	dim temp11
	dim temp12
	dim temp13
	dim temp14
	dim temp15
	dim temp16

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & HSFileName) then
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & HSFileName)
	Set textstr=ScoreFile.OpenAsTextStream(1,0)
	If (textstr.AtEndOfStream=True) then
		Exit Sub
	End If
	temp1=textstr.readline
	temp2=textstr.readline
	temp3=textstr.readline
	temp4=textstr.readline
	temp5=textstr.readline
	temp6=textstr.readline
	If HighScore<1 Then
		temp7=textstr.readline
		temp8=textstr.readline
		temp9=textstr.readline
		temp10=textstr.readline
		temp11=textstr.readline
		temp12=textstr.readline
		temp13=textstr.readline
		temp14=textstr.readline
		temp15=textstr.readline
		temp16=textstr.readline
	End If

	textstr.Close
	MusicVolume = cdbl(temp2)
	Credits=cdbl(temp3)
	If Credits > 0 Then DOF 126, DOFOn

	'PUP:
	If usePUP Then
		PuPlayer.LabelSet 5,"Credits","CREDITS: " & CStr(Credits),1,""
		If Credits > 0 Then
			pupevent 200
		 
		End If
	End If
	'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

	BallsPerGame=cdbl(temp4)
	ExtraBallSetting=cdbl(temp5)
	ReplayLevel=cdbl(temp6)

	If HighScore<1 Then
		HSScore(1) = int(temp7)
		HSScore(2) = int(temp8)
		HSScore(3) = int(temp9)
		HSScore(4) = int(temp10)
		HSScore(5) = int(temp11)

		HSName(1) = temp12
		HSName(2) = temp13
		HSName(3) = temp14
		HSName(4) = temp15
		HSName(5) = temp16
	End If

	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub

Sub SaveLMEMConfig
	Dim FileObj
	Dim LMConfig
	Dim temp1
	Dim tempb2s
	tempb2s=0
	If B2SOn Then
		tempb2s=1
	Else
		tempb2s=0
	End If
	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then
		Exit Sub
	End If
	Set LMConfig=FileObj.CreateTextFile(UserDirectory & LMEMTableConfig,True)
	LMConfig.WriteLine tempb2s
	LMConfig.Close
	Set LMConfig=Nothing
	Set FileObj=Nothing

End Sub

Sub LoadLMEMConfig
	Dim FileObj
	Dim LMConfig
	Dim tempC
	Dim tempb2s

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then
		Exit Sub
	End If
	If Not FileObj.FileExists(UserDirectory & LMEMTableConfig) Then
		Exit Sub
	End If
	Set LMConfig=FileObj.GetFile(UserDirectory & LMEMTableConfig)
	Set TextStr2=LMConfig.OpenAsTextStream(1,0)
	If (TextStr2.AtEndOfStream=True) Then
		Exit Sub
	End If
	tempC=TextStr2.ReadLine
	TextStr2.Close
	tempb2s=cdbl(tempC)
	If tempb2s=0 Then
		B2SOn=false
	Else
		B2SOn=true
	End If
	Set LMConfig=Nothing
	Set FileObj=Nothing
End Sub

Sub SaveLMEMConfig2
	If ShadowConfigFile=false Then exit sub
	Dim FileObj
	Dim LMConfig2
	Dim temp1
	Dim temp2
	Dim tempBS
	Dim tempFS

	If EnableBallShadow=true Then
		tempBS=1
	Else
		tempBS=0
	End If
	If EnableFlipperShadow=true Then
		tempFS=1
	Else
		tempFS=0
	End If

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then
		Exit Sub
	End If
	Set LMConfig2=FileObj.CreateTextFile(UserDirectory & LMEMShadowConfig,True)
	LMConfig2.WriteLine tempBS
	LMConfig2.WriteLine tempFS
	LMConfig2.Close
	Set LMConfig2=Nothing
	Set FileObj=Nothing

End Sub

Sub LoadLMEMConfig2
	If ShadowConfigFile=false Then
		EnableBallShadow = ShadowBallOn
		BallShadowUpdate.enabled = ShadowBallOn
		EnableFlipperShadow = ShadowFlippersOn
		FlipperLSh.visible = ShadowFlippersOn
		FlipperRSh.visible = ShadowFlippersOn
		exit sub
	End If
	Dim FileObj
	Dim LMConfig2
	Dim tempC
	Dim tempD
	Dim tempFS
	Dim tempBS

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then
		Exit Sub
	End If
	If Not FileObj.FileExists(UserDirectory & LMEMShadowConfig) Then
		Exit Sub
	End If
	Set LMConfig2=FileObj.GetFile(UserDirectory & LMEMShadowConfig)
	Set TextStr2=LMConfig2.OpenAsTextStream(1,0)
	If (TextStr2.AtEndOfStream=True) Then
		Exit Sub
	End If
	tempC=TextStr2.ReadLine
	tempD=TextStr2.Readline
	TextStr2.Close
	tempBS=cdbl(tempC)
	tempFS=cdbl(tempD)
	If tempBS=0 Then
		EnableBallShadow=false
		BallShadowUpdate.enabled=false
	Else
		EnableBallShadow=true
	End If
	If tempFS=0 Then
		EnableFlipperShadow=false
		FlipperLSh.visible=false
		FLipperRSh.visible=false
	Else
		EnableFlipperShadow=true
	End If
	Set LMConfig2=Nothing
	Set FileObj=Nothing
End Sub
	   


Sub DisplayHighScore
End Sub


	   

Sub InitPauser5_timer
		If B2SOn Then
			'Controller.B2SSetScore 6,HighScore
		End If
		DisplayHighScore
		CreditsReel.SetValue(Credits)
		InitPauser5.enabled=false
End Sub

Sub LightsOut
End Sub

Sub ToggleBumper
End Sub

				

	   

Sub BumpersOff
	Bumper1Light.Visible=false
	Bumper2Light.Visible=false
	Bumper3Light.Visible=false
End Sub

Sub BumpersOn
	Bumper1Light.Visible=True
	Bumper2Light.Visible=True
	Bumper3Light.Visible=True
End Sub


Sub PlasticsOn

	GIReset
	Light3.state=1
	Light2.state=1
	ShadowMain.visible=True
	For Each obj in ShadowCup
		obj.visible=True
	Next
	For Each obj in ShadowMug
		obj.visible=True
	Next
End Sub
	   

Sub PlasticsOff
	GIOff
	Light3.state=0
	Light2.state=0
	StopSound "buzzL"
	StopSound "buzz"
    If MusicOn = 1 and BotchedMB = 0 Then PlayMusic"cuphead/CupAttract.mp3", MusicVolume
	ShadowMain.visible=False
	For Each obj in ShadowCup
		obj.visible=False
	Next
	For Each obj in ShadowMug
		obj.visible=False
	Next
End Sub

Sub SetupReplayTables

	Replay1Table(1)=65000
	Replay1Table(2)=68000
	Replay1Table(3)=70000
	Replay1Table(4)=72000
	Replay1Table(5)=74000
	Replay1Table(6)=76000
	Replay1Table(7)=78000
	Replay1Table(8)=80000
	Replay1Table(9)=83000
	Replay1Table(10)=85000
	Replay1Table(11)=87000
	Replay1Table(12)=90000
	Replay1Table(13)=999000
	Replay1Table(14)=999000
	Replay1Table(15)=999000

	Replay2Table(1)=77000
	Replay2Table(2)=80000
	Replay2Table(3)=82000
	Replay2Table(4)=84000
	Replay2Table(5)=86000
	Replay2Table(6)=88000
	Replay2Table(7)=90000
	Replay2Table(8)=92000
	Replay2Table(9)=95000
	Replay2Table(10)=97000
	Replay2Table(11)=99000
	Replay2Table(12)=999000
	Replay2Table(13)=999000
	Replay2Table(14)=999000
	Replay2Table(15)=999000

	Replay3Table(1)=999000
	Replay3Table(2)=999000
	Replay3Table(3)=999000
	Replay3Table(4)=999000
	Replay3Table(5)=999000
	Replay3Table(6)=999000
	Replay3Table(7)=999000
	Replay3Table(8)=999000
	Replay3Table(9)=999000
	Replay3Table(10)=999000
	Replay3Table(11)=999000
	Replay3Table(12)=999000
	Replay3Table(13)=999000
	Replay3Table(14)=999000
	Replay3Table(15)=999000

	ReplayTableMax=12
End Sub

	   

Sub RefreshReplayCard
	Dim tempst1
	Dim tempst2

	tempst1=FormatNumber(BallsPerGame,0)
	tempst2=FormatNumber(ReplayLevel,0)

	ReplayCard.image = "SC" + tempst2
	Replay1=Replay1Table(ReplayLevel)
	Replay2=Replay2Table(ReplayLevel)
	Replay3=Replay3Table(ReplayLevel)
End Sub

'****************************************
'  SCORE MOTOR
'****************************************

ScoreMotorTimer.Enabled = 1
ScoreMotorTimer.Interval = 135 '135
AddScoreTimer.Enabled = 1
AddScoreTimer.Interval = 135

Dim queuedscore
Dim MotorMode
Dim MotorPosition

Sub SetMotor(y)
	Select Case ScoreMotorAdjustment
		Case 0:
			queuedscore=queuedscore+y
		Case 1:
			If MotorRunning<>1 And InProgress=true Then
				queuedscore=queuedscore+y
			End If
	End Select
End Sub

Sub SetMotor2(x)
	If MotorRunning<>1 And InProgress=true Then
		MotorRunning=1

		Select Case x
			Case 10:
				AddScore(10)
				MotorRunning=0
				BumpersOn

			Case 20:
				MotorMode=10
				MotorPosition=2
				BumpersOff
			Case 30:
				MotorMode=10
				MotorPosition=3
				BumpersOff
			Case 40:
				MotorMode=10
				MotorPosition=4
				BumpersOff
			Case 50:
				MotorMode=10
				MotorPosition=5
				BumpersOff
			Case 100:
				AddScore(100)
				MotorRunning=0
				BumpersOn
			Case 200:
				MotorMode=100
				MotorPosition=2
				BumpersOff
			Case 300:
				MotorMode=100
				MotorPosition=3
				BumpersOff
			Case 400:
				MotorMode=100
				MotorPosition=4
				BumpersOff
			Case 500:
				MotorMode=100
				MotorPosition=5
				BumpersOff
			Case 1000:
				AddScore(1000)
				MotorRunning=0
				BumpersOn
			Case 2000:
				MotorMode=1000
				MotorPosition=2
				BumpersOff
			Case 3000:
				MotorMode=1000
				MotorPosition=3
				BumpersOff
			Case 4000:
				MotorMode=1000
				MotorPosition=4
				BumpersOff
			Case 5000:
				MotorMode=1000
				MotorPosition=5
				BumpersOff
		End Select
	End If
End Sub

Sub AddScoreTimer_Timer
	Dim tempscore


	If MotorRunning<>1 And InProgress=true Then
		If queuedscore>=5000 Then
			tempscore=5000
			queuedscore=queuedscore-5000
			SetMotor2(5000)
			exit sub
		End If
		If queuedscore>=4000 Then
			tempscore=4000
			queuedscore=queuedscore-4000
			SetMotor2(4000)
			exit sub
		End If

		If queuedscore>=3000 Then
			tempscore=3000
			queuedscore=queuedscore-3000
			SetMotor2(3000)
			exit sub
		End If

		If queuedscore>=2000 Then
			tempscore=2000
			queuedscore=queuedscore-2000
			SetMotor2(2000)
			exit sub
		End If

		If queuedscore>=1000 Then
			tempscore=1000
			queuedscore=queuedscore-1000
			SetMotor2(1000)
			exit sub
		End If

		If queuedscore>=500 Then
			tempscore=500
			queuedscore=queuedscore-500
			SetMotor2(500)
			exit sub
		End If
		If queuedscore>=400 Then
			tempscore=400
			queuedscore=queuedscore-400
			SetMotor2(400)
			exit sub
		End If
		If queuedscore>=300 Then
			tempscore=300
			queuedscore=queuedscore-300
			SetMotor2(300)
			exit sub
		End If
		If queuedscore>=200 Then
			tempscore=200
			queuedscore=queuedscore-200
			SetMotor2(200)
			exit sub
		End If
		If queuedscore>=100 Then
			tempscore=100
			queuedscore=queuedscore-100
			SetMotor2(100)
			exit sub
		End If

		If queuedscore>=50 Then
			tempscore=50
			queuedscore=queuedscore-50
			SetMotor2(50)
			exit sub
		End If
		If queuedscore>=40 Then
			tempscore=40
			queuedscore=queuedscore-40
			SetMotor2(40)
			exit sub
		End If
		If queuedscore>=30 Then
			tempscore=30
			queuedscore=queuedscore-30
			SetMotor2(30)
			exit sub
		End If
		If queuedscore>=20 Then
			tempscore=20
			queuedscore=queuedscore-20
			SetMotor2(20)
			exit sub
		End If
		If queuedscore>=10 Then
			tempscore=10
			queuedscore=queuedscore-10
			SetMotor2(10)
			exit sub
		End If


	End If


End Sub

Sub ScoreMotorTimer_Timer
	If MotorPosition > 0 Then
		Select Case MotorPosition
			Case 5,4,3,2:
				If MotorMode=1000 Then
					AddScore(1000)
				End If
				If MotorMode=100 Then
					AddScore(100)
				End If
				If MotorMode=10 Then
					AddScore(10)
				End If
				MotorPosition=MotorPosition-1
			Case 1:
				If MotorMode=1000 Then
					AddScore(1000)
				End If
				If MotorMode=100 Then
					AddScore(100)
				End If
				If MotorMode=10 Then
					AddScore(10)
				End If
				MotorPosition=0:MotorRunning=0:BumpersOn
		End Select
	End If

End Sub


Sub AddScore(x)
	If TableTilted=true Then exit sub	
	If BotchedMB = 1 Then Exit Sub	
	Select Case ScoreAdditionAdjustment
		Case 0:
			AddScore1(x)
		Case 1:
			AddScore2(x)
	End Select
End Sub

	   


Sub AddScore1(x)
			 
					  
	Select Case x
		Case 1:
			PlayChime(10)
			Score(Player)=Score(Player)+1

		Case 10:
			PlayChime(10)
			Score(Player)=Score(Player)+10
							
			ToggleAlternatingRelay
		Case 100:
			PlayChime(100)
			Score(Player)=Score(Player)+100
							 

		Case 1000:
			PlayChime(1000)
			Score(Player)=Score(Player)+1000
											 
	End Select
	PlayerScores(Player-1).AddValue(x)
	PlayerScoresOn(Player-1).AddValue(x)
	If ScoreDisplay(Player)<100000 Then
		ScoreDisplay(Player)=Score(Player)
	Else
		Score100K(Player)=Int(Score(Player)/100000)
		ScoreDisplay(Player)=Score(Player)-100000
	End If
	If Score(Player)=>100000 Then
		If B2SOn Then
			If Player=1 Then
				Controller.B2SSetScoreRolloverPlayer1 Score100K(Player)
			End If
			If Player=2 Then
				Controller.B2SSetScoreRolloverPlayer2 Score100K(Player)
			End If

			If Player=3 Then
				Controller.B2SSetScoreRolloverPlayer3 Score100K(Player)
			End If

			If Player=4 Then
				Controller.B2SSetScoreRolloverPlayer4 Score100K(Player)
			End If
		End If
	End If
	If B2SOn Then
		Controller.B2SSetScorePlayer Player, ScoreDisplay(Player)
	End If
	If Score(Player)>Replay1 and Replay1Paid(Player)=false Then
		Replay1Paid(Player)=True
		AddSpecial
	End If
	If Score(Player)>Replay2 and Replay2Paid(Player)=false Then
		Replay2Paid(Player)=True
		AddSpecial
	End If
	If Score(Player)>Replay3 and Replay3Paid(Player)=false Then
		Replay3Paid(Player)=True
		AddSpecial
	End If
	'PUP:
	updatePuPDMD
	'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<
End Sub

Sub AddScore2(x)
	Dim OldScore, NewScore, OldTestScore, NewTestScore
    OldScore = Score(Player)

	Select Case x
        Case 1:
            Score(Player)=Score(Player)+1
		Case 10:
			Score(Player)=Score(Player)+10
		Case 100:
			Score(Player)=Score(Player)+100
		Case 1000:
			Score(Player)=Score(Player)+1000
	End Select
	NewScore = Score(Player)
	If Score(Player)=>100000 Then
		If B2SOn Then
			If Player=1 Then
				Controller.B2SSetScoreRolloverPlayer1 1
			End If
			If Player=2 Then
				Controller.B2SSetScoreRolloverPlayer2 1
			End If

			If Player=3 Then
				Controller.B2SSetScoreRolloverPlayer3 1
			End If

			If Player=4 Then
				Controller.B2SSetScoreRolloverPlayer4 1
			End If
		End If
	End If

	'PUP:
	updatePuPDMD
	'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

	OldTestScore = OldScore
	NewTestScore = NewScore
	Do
		If OldTestScore < Replay1 and NewTestScore >= Replay1 Then
			AddSpecial()
			NewTestScore = 0
		ElseIf OldTestScore < Replay2 and NewTestScore >= Replay2 Then
			AddSpecial()
			NewTestScore = 0
		ElseIf OldTestScore < Replay3 and NewTestScore >= Replay3 Then
			AddSpecial()
			NewTestScore = 0
		End If
		NewTestScore = NewTestScore - 100000
		OldTestScore = OldTestScore - 100000
	Loop While NewTestScore > 0

    OldScore = int(OldScore / 10)	' divide by 10 For games with fixed 0 in 1s position, by 1 For games with real 1s digits
    NewScore = int(NewScore / 10)	' divide by 10 For games with fixed 0 in 1s position, by 1 For games with real 1s digits
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore&", OldScore Mod 10="&OldScore Mod 10 & ", NewScore % 10="&NewScore Mod 10)

    If (OldScore Mod 10 <> NewScore Mod 10) Then
		PlayChime(10)

    End If

    OldScore = int(OldScore / 10)
    NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
    If (OldScore Mod 10 <> NewScore Mod 10) Then
		PlayChime(100)

    End If

    OldScore = int(OldScore / 10)
    NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
    If (OldScore Mod 10 <> NewScore Mod 10) Then
		PlayChime(1000)

    End If

    OldScore = int(OldScore / 10)
    NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
    If (OldScore Mod 10 <> NewScore Mod 10) Then
		PlayChime(1000)
    End If

	If B2SOn Then
		Controller.B2SSetScorePlayer Player, Score(Player)
	End If
'	EMReel1.SetValue Score(Player)
	PlayerScores(Player-1).AddValue(x)
	PlayerScoresOn(Player-1).AddValue(x)
End Sub



Sub PlayChime(x)
	If ChimesOn=0 Then
		Select Case x
			Case 10
				If LastChime10=1 Then
					PlaySound SoundFXDOF("SpinACard_1_10_Point_Bell",141,DOFPulse,DOFChimes)
					LastChime10=0
				Else
					PlaySound SoundFXDOF("SpinACard_1_10_Point_Bell",141,DOFPulse,DOFChimes)
					LastChime10=1
				End If
			Case 100
				If LastChime100=1 Then
					PlaySound SoundFXDOF("SpinACard_100_Point_Bell",142,DOFPulse,DOFChimes)
					LastChime100=0
				Else
					PlaySound SoundFXDOF("SpinACard_100_Point_Bell",142,DOFPulse,DOFChimes)
					LastChime100=1
				End If

		End Select
	Else
		Select Case x
			Case 10
				If LastChime10=1 Then
					PlaySound SoundFXDOF("SJ_Chime_10a",141,DOFPulse,DOFChimes)
					LastChime10=0
				Else
					PlaySound SoundFXDOF("SJ_Chime_10b",141,DOFPulse,DOFChimes)
					LastChime10=1
				End If
			Case 100
				If LastChime100=1 Then
					PlaySound SoundFXDOF("SJ_Chime_100a",142,DOFPulse,DOFChimes)
					LastChime100=0
				Else
					PlaySound SoundFXDOF("SJ_Chime_100b",142,DOFPulse,DOFChimes)
					LastChime100=1
				End If
			Case 1000
				If LastChime1000=1 Then
					PlaySound SoundFXDOF("SJ_Chime_1000a",143,DOFPulse,DOFChimes)
					LastChime1000=0
				Else
					PlaySound SoundFXDOF("SJ_Chime_1000b",143,DOFPulse,DOFChimes)
					LastChime1000=1
				End If
		End Select
	End If
End Sub

Sub HideOptions()
End Sub

	   

'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool For surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) For walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters For the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
    PlaySound sound, 0, ABS(BOT.velz)/17, AudioPan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (For surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan For a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
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

Sub RollingSoundTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

	' exit the Sub If no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound For Each ball
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
    ' Thalamus  - added next three lines For effect after popper - Roth's ball jump
    If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust For ball drop sounds
      PlaySoundAtBOTBallZ "fx_ball_drop" & b, BOT(b)
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
    ' exit the Sub If no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow For Each ball
    For b = 0 to UBound(BOT)
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/10) + ((BOT(b).X - (Table1.Width/2))/12.5))
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/10) + ((BOT(b).X - (Table1.Width/2))/12.5))
        End If
        ballShadow(b).Y = BOT(b).Y
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

'*****************************************
'	Object sounds
'*****************************************

Sub Plastics_Hit (idx)
	PlaySound "woodhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

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

				 
																					   
				   
	   

Sub Rubbers_Hit(idx)
 	Dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 Then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End If
	If finalspeed >= 6 AND finalspeed <= 20 Then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	Dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 Then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End If
	If finalspeed >= 6 AND finalspeed <= 16 Then
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

Sub LeftFlipper001_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper001_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub





' ============================================================================================
' GNMOD - Multiple High Score Display and Collection
' ============================================================================================
Dim EnteringInitials		' Normally zero, set to non-zero to enter initials
EnteringInitials = 0

'Dim PlungerPulled
'PlungerPulled = 0

Dim SelectedChar			' character under the "cursor" when entering initials

Dim HSTimerCount			' Pass counter For HS timer, scores are cycled by the timer
HSTimerCount = 5			' Timer is initially enabled, it'll wrap from 5 to 1 when it's displayed

Dim InitialString			' the string holding the player's initials as they're entered

Dim AlphaString				' A-Z, 0-9, space (_) and backspace (<)
Dim AlphaStringPos			' pointer to AlphaString, move forward and backward with flipper keys
AlphaString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_<"

Dim HSNewHigh				' The new score to be recorded

Dim HSScore(5)				' High Scores read in from config file
Dim HSName(5)				' High Score Initials read in from config file

' default high scores, remove this when the scores are available from the config file
HSScore(1) = 75000
HSScore(2) = 70000
HSScore(3) = 60000
HSScore(4) = 55000
HSScore(5) = 50000

HSName(1) = "CUPHEAD"
HSName(2) = "MUGMAN"
HSName(3) = "DEVIL"
HSName(4) = "KINGDICE"
HSName(5) = "ELDER"

InitialString = ""

Sub HighScoreTimer_Timer
	If EnteringInitials Then
		If HSTimerCount = 1 Then
			SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
			HSTimerCount = 2
		Else
			SetHSLine 3, InitialString
			HSTimerCount = 1
		End If
	ElseIf InProgress Then
		SetHSLine 1, "HIGH SCORE1"
		SetHSLine 2, HSScore(1)
		SetHSLine 3, HSName(1)
		HSTimerCount = 5	' set so the highest score will show after the game is over
		HighScoreTimer.enabled=false
	ElseIf CheckAllScores Then
		NewHighScore sortscores(ScoreChecker),sortplayers(ScoreChecker)
	
	Else
		' cycle through high scores
		HighScoreTimer.interval=2000
		HSTimerCount = HSTimerCount + 1
		If HsTimerCount > 5 Then
			HSTimerCount = 1
		End If
		SetHSLine 1, "HIGH SCORE"+FormatNumber(HSTimerCount,0)
		SetHSLine 2, HSScore(HSTimerCount)
		SetHSLine 3, HSName(HSTimerCount)
	End If
End Sub

Function GetHSChar(String, Index)
	Dim ThisChar
	Dim FileName
	ThisChar = Mid(String, Index, 1)
	FileName = "PostIt"
	If ThisChar = " " or ThisChar = "" Then
		FileName = FileName & "BL"
	ElseIf ThisChar = "<" Then
		FileName = FileName & "LT"
	ElseIf ThisChar = "_" Then
		FileName = FileName & "SP"
	Else
		FileName = FileName & ThisChar
	End If
	GetHSChar = FileName
End Function

Sub SetHsLine(LineNo, String)
	Dim Letter
	Dim ThisDigit
	Dim ThisChar
	Dim StrLen
	Dim LetterLine
	Dim Index
	Dim StartHSArray
	Dim EndHSArray
	Dim LetterName
	Dim xfor
	StartHSArray=array(0,1,12,22)
	EndHSArray=array(0,11,21,31)
	StrLen = len(string)
	Index = 1

	For xFor = StartHSArray(LineNo) to EndHSArray(LineNo)
		Eval("HS"&xfor).image = GetHSChar(String, Index)
		Index = Index + 1
	Next

End Sub

Sub NewHighScore(NewScore, PlayNum)
	If NewScore > HSScore(5) Then
		HighScoreTimer.interval = 500
		HSTimerCount = 1
		AlphaStringPos = 1		' start with first character "A"
		EnteringInitials = 1:If MusicOn = 1 Then PlayMusic"Cuphead/CupHighScore.mp3" , MusicVolume' intercept the control keys while entering initials
		InitialString = ""		' initials entered so far, initialize to empty
		SetHSLine 1, "PLAYER "+FormatNumber(PlayNum,0)
		SetHSLine 2, "ENTER NAME"
		SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
		HSNewHigh = NewScore
		For xx=1 to HighScoreReward
			AddSpecial
		Next
	End If
	ScoreChecker=ScoreChecker-1
	If ScoreChecker=0 Then
		CheckAllScores=0
	End If
End Sub

Sub CollectInitials(keycode)
	If keycode = LeftFlipperKey Then
		' back up to previous character
		AlphaStringPos = AlphaStringPos - 1
		If AlphaStringPos < 1 Then
			AlphaStringPos = len(AlphaString)		' handle wrap from beginning to end
			If InitialString = "" Then
				' Skip the backspace If there are no characters to backspace over
				AlphaStringPos = AlphaStringPos - 1
			End If
		End If
		SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
		PlaySound "DropTargetDropped"
	ElseIf keycode = RightFlipperKey Then
		' advance to next character
		AlphaStringPos = AlphaStringPos + 1
		If AlphaStringPos > len(AlphaString) or (AlphaStringPos = len(AlphaString) and InitialString = "") Then
			' Skip the backspace If there are no characters to backspace over
			AlphaStringPos = 1
		End If
		SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
		PlaySound "DropTargetDropped"
	ElseIf keycode = StartGameKey or keycode = PlungerKey Then
		SelectedChar = MID(AlphaString, AlphaStringPos, 1)
		If SelectedChar = "_" Then
			InitialString = InitialString & " "
			PlaySound("Ding10")
		ElseIf SelectedChar = "<" Then
			InitialString = MID(InitialString, 1, len(InitialString) - 1)
			If len(InitialString) = 0 Then
				' If there are no more characters to back over, don't leave the < displayed
				AlphaStringPos = 1
			End If
			PlaySound("Ding100")
		Else
			InitialString = InitialString & SelectedChar
			PlaySound("Ding10")
		End If
		If len(InitialString) < 3 Then
			SetHSLine 3, InitialString & SelectedChar
		End If
	End If
	If len(InitialString) = 3 Then
		' save the score
		For i = 5 to 1 step -1
			If i = 1 or (HSNewHigh > HSScore(i) and HSNewHigh <= HSScore(i - 1)) Then
				' Replace the score at this location
				If i < 5 Then
										  
					HSScore(i + 1) = HSScore(i)
					HSName(i + 1) = HSName(i)
				End If
																  
				EnteringInitials = 0
				HSScore(i) = HSNewHigh
				HSName(i) = InitialString
				HSTimerCount = 5
				HighScoreTimer_Timer
				HighScoreTimer.interval = 2000
				PlaySound("Ding1000")
				exit sub
			ElseIf i < 5 Then
				' move the score in this slot down by 1, it's been exceeded by the new score
										  
				HSScore(i + 1) = HSScore(i)
				HSName(i + 1) = HSName(i)
			End If
		Next
	End If

End Sub
' END GNMOD

' ============================================================================================
' GNMOD - New Options menu
' ============================================================================================
Dim EnteringOptions
Dim CurrentOption
Dim OptionCHS
Dim MaxOption
Dim OptionHighScorePosition
Dim XOpt
Dim StartingArray
Dim EndingArray

StartingArray=Array(0,1,2,30,33,61,89,117,145,173,201,229)
EndingArray=Array(0,1,29,32,60,88,116,144,172,200,228,256)
EnteringOptions = 0
MaxOption = 9
OptionCHS = 0
OptionHighScorePosition = 0
Const OptionLinesToMark="111100011"
Const OptionLine1="" 'do not use this line
Const OptionLine2="" 'do not use this line
Const OptionLine3="" 'do not use this line
Const OptionLine4="Extra Ball Light"
Const OptionLine5=""
Const OptionLine6=""
Const OptionLine7=""
Const OptionLine8="" 'do not use this line
Const OptionLine9="" 'do not use this line

Sub OperatorMenuTimer_Timer
	OperatorMenuBackdrop.image = "OperatorMenu"
	EnteringOptions = 1
	OptionCHS = 0
	CurrentOption = 1
	DisplayAllOptions
	OperatorOption1.image = "BluePlus"
	SetHighScoreOption

End Sub

Sub DisplayAllOptions
	Dim linecounter
	Dim tempstring
	For linecounter = 1 to MaxOption
		tempstring=Eval("OptionLine"&linecounter)
		Select Case linecounter
			Case 1:
				tempstring=tempstring + FormatNumber(BallsPerGame,0)
				SetOptLine 1,tempstring
			Case 2:
				If Replay2Table(ReplayLevel)=999000 Then
					tempstring=FormatNumber(Replay1Table(ReplayLevel),0)
				ElseIf Replay3Table(ReplayLevel)=999000 Then
					tempstring = tempstring + FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0)
				Else
					tempstring = tempstring + FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0) + "/" + FormatNumber(Replay3Table(ReplayLevel),0)
				End If
				SetOptLine 2,tempstring
			Case 3:
				If OptionCHS=0 Then
					tempstring = "NO"
				Else
					tempstring = "YES"
				End If
				SetOptLine 3,tempstring
			Case 4:
				SetOptLine 4, tempstring
				If ExtraBallSetting=1 Then
					tempstring = "Use Alternating Relay"
				Else
					tempstring = "Do Not Use Alternating Relay"
				End If
				SetOptLine 5, tempstring
			Case 5:
				SetOptLine 6, tempstring
					Tempstring = "Press MagnaSave Buttons"
				SetOptLine 7, tempstring
			Case 6:
				SetOptLine 8, tempstring
					TempString = "to change Music Volume"
				SetOptLine 9, tempstring

			Case 7:
				SetOptLine 10, tempstring
					tempstring = FormatNumber(MusicVolume)
				SetOptLine 11, tempstring

			Case 8:

			Case 9:


		End Select
	Next
End Sub

	 
	   

Sub MoveArrow
	Do
		CurrentOption = CurrentOption + 1
		If CurrentOption>Len(OptionLinesToMark) Then
			CurrentOption=1
		End If
	Loop Until Mid(OptionLinesToMark,CurrentOption,1)="1"
End Sub

Sub CollectOptions(ByVal keycode)
	If Keycode = LeftFlipperKey Then
		PlaySound "DropTargetDropped"
		For XOpt = 1 to MaxOption
			Eval("OperatorOption"&XOpt).image = "PostitBL"
		Next
		MoveArrow
		If CurrentOption<8 Then
			Eval("OperatorOption"&CurrentOption).image = "BluePlus"
		ElseIf CurrentOption=8 Then
			Eval("OperatorOption"&CurrentOption).image = "GreenCheck"
		Else
			Eval("OperatorOption"&CurrentOption).image = "RedX"
		End If

	ElseIf Keycode = RightFlipperKey Then
		PlaySound "DropTargetDropped"
		If CurrentOption = 1 Then
			If BallsPerGame = 3 Then
				BallsPerGame = 5
			Else
				BallsPerGame = 3
			End If
			DisplayAllOptions
		ElseIf CurrentOption = 2 Then
			ReplayLevel=ReplayLevel+1
			If ReplayLevel>ReplayTableMax Then
				ReplayLevel=1
			End If
			DisplayAllOptions
		ElseIf CurrentOption = 3 Then
			If OptionCHS = 0 Then
				OptionCHS = 1

			Else
				OptionCHS = 0
			End If
		 
			DisplayAllOptions
		ElseIf CurrentOption = 4 Then
			If ExtraBallSetting=1 Then
				ExtraBallSetting=2
			Else
				ExtraBallSetting=1
			End If
			DisplayAllOptions
		ElseIf CurrentOption = 8 or CurrentOption = 9 Then
				If OptionCHS=1 Then
					HSScore(1) = 75000
					HSScore(2) = 70000
					HSScore(3) = 60000
					HSScore(4) = 55000
					HSScore(5) = 50000

					HSName(1) = "CUPHEAD"
					HSName(2) = "MUGMAN"
					HSName(3) = "DEVIL"
					HSName(4) = "KINGDICE"
					HSName(5) = "ELDER"
				End If

				If CurrentOption = 8 Then
					savehs
				Else
					loadhs
				End If
				OperatorMenuBackdrop.image = "PostitBL"
				For XOpt = 1 to MaxOption
					Eval("OperatorOption"&XOpt).image = "PostitBL"
				Next

				For XOpt = 1 to 256
					Eval("Option"&XOpt).image = "PostItBL"
				Next
				RefreshReplayCard
				InstructCard.image="IC_"+FormatNumber(BallsPerGame,0)
				EnteringOptions = 0
		End If

	ElseIf keycode = LeftMagnaSave Then 
				MusicVolume = MusicVolume - .1
				If MusicVolume < 0 Then MusicVolume = 0
				PlayMusic "Cuphead/CupMusic5.mp3" , MusicVolume
				DisplayAllOptions

	ElseIf keycode =RightMagnaSave Then
				MusicVolume = MusicVolume + .1
				If MusicVolume > 1 Then MusicVolume = 1
				PlayMusic "Cuphead/CupMusic5.mp3" , MusicVolume
				DisplayAllOptions
	End If
End Sub

Sub SetHighScoreOption

End Sub

Function GetOptChar(String, Index)
	Dim ThisChar
	Dim FileName
	ThisChar = Mid(String, Index, 1)
	FileName = "PostIt"
	If ThisChar = " " or ThisChar = "" Then
		FileName = FileName & "BL"
	ElseIf ThisChar = "<" Then
		FileName = FileName & "LT"
	ElseIf ThisChar = "_" Then
		FileName = FileName & "SP"
	ElseIf ThisChar = "/" Then
		FileName = FileName & "SL"
	ElseIf ThisChar = "," Then
		FileName = FileName & "CM"
	Else
		FileName = FileName & ThisChar
	End If
	GetOptChar = FileName
End Function

Sub SetOptLine(LineNo, String)
	Dim xfor
	Dim Letter
	Dim ThisDigit
	Dim ThisChar
	Dim StrLen
	Dim LetterLine
	Dim Index
	Dim LetterName
	StrLen = len(string)
	Index = 1

	For xFor = StartingArray(LineNo) to EndingArray(LineNo)
		Eval("Option"&xfor).image = GetOptChar(string, Index)
		Index = Index + 1
	Next
End Sub


'******************************************************
'				FLIPPER AND RUBBER CORRECTION
'******************************************************
Dim LFPress, RFPress, EOST, EOSA, EOSTnew, EOSaNew,Plunge
Dim FStrength, FRampUp, fElasticity, EOSRampUp, SOSRampUp
Dim RFEndAngle, LFEndAngle, LF1EndAngle, RF1EndAngle, LFCount, RFCount, LiveCatch

LFEndAngle = Leftflipper.EndAngle
LF1Endangle = LeftFlipper001.EndAngle
RFEndAngle = RightFlipper.EndAngle
RF1EndAngle = RightFlipper001.EndAngle

EOST = leftflipper.eosTorque   			'End of Swing Torque
EOSA = leftflipper.eosTorqueAngle		'End of Swing Torque Angle
fStrength = LeftFlipper.strength		'Flipper Strength
fRampUp = LeftFlipper.RampUp			'Flipper Ramp Up
fElasticity = LeftFlipper.elasticity	'Flipper Elasticity
EOStNew = 1.0 		'new Flipper Torque 
EOSaNew = 0.2		'new FLipper Tprque Angle
EOSRampUp = 1.5		'new EOS Ramp Up weaker at EOS because of the weaker holding coil
SOSRampUp = 8.5 	'new SOS Ramp Up strong at start because of the stronger starting coil
LiveCatch = 8		'variable to check elapsed time from 

'********Need to have a flipper timer to check For these values
Dim PlungerCheck, PlungerCheck1, PlungerLoop, PlungerPlaying, PlungerPause
Dim PlungerMax, PlungerMin
Sub flipperTimer_Timer
	lFlip.rotz = leftflipper.currentangle -121
	rFlip.rotz = rightflipper.currentangle +121
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

	lFlip.rotz = leftflipper.CurrentAngle -121 'silver metal flipper obj
	lFlipR.rotz = leftflipper.CurrentAngle -121 
	rFlip.rotz = RightFlipper.CurrentAngle +121
	rFlipR.rotz = RightFlipper.CurrentAngle +121
	lFlip001.roty = leftflipper001.CurrentAngle
	rFlip001.roty = RightFlipper001.CurrentAngle 
	

												  
												   

	'--------------Flipper Tricks Section
	'What this code does is swing the flipper fast and make the flipper soft near its EOS to enable live catches.  It resets back to the base Table
	'settings once the flipper reaches the end of swing.  The code also makes the flipper starting ramp up high to simulate the stronger starting
	'coil strength and weaker at its EOS to simulate the weaker hold coil.

	If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle and LFPress = 1 Then 	'If the flipper is fully swung and the flipper button is pressed Then...
		LeftFlipper.eosTorqueAngle = EOSaNew	'sets flipper EOS Torque Angle to .2 
		LeftFlipper.eosTorque = EOStNew			'sets flipper EOS Torque to 1
		LeftFlipper.RampUp = EOSRampUp			'sets flipper ramp up to 1.5
'		If LFCount = 0 Then LFCount = GameTime	'sets the variable LFCount = to the elapsed game time
		If GameTime - LFCount < LiveCatch Then	'If less than 8ms have elasped Then we are in a "Live Catch" scenario
			TextBox1.text = "Catch"
			LeftFlipper.Elasticity = 0.1		'sets flipper elasticity WAY DOWN to allow Live Catches
			If LeftFlipper.EndAngle <> LFEndAngle Then LeftFlipper.EndAngle = LFEndAngle	'Keep the flipper at its EOS and don't let it deflect
		Else	
			LeftFlipper.Elasticity = fElasticity	'reset flipper elasticity to the base table setting
			TextBox1.text = fElasticity
		End If
	ElseIf LeftFlipper.CurrentAngle > LeftFlipper.startangle - 0.05  Then 	'If the flipper has started its swing, make it swing fast to nearly the end...
		LeftFlipper.RampUp = SOSRampUp				'set flipper Ramp Up high
		LeftFlipper.EndAngle = LFEndAngle - 3		'swing to within 3 degrees of EOS
		LeftFlipper.Elasticity = fElasticity		'Set the elasticity to the base table elasticity
		LFCount = 0									'Sets LF Count = 0 which would override the LFCount = GameTime set in hitting the flipper trigger zone (why??)
	ElseIf LeftFlipper.CurrentAngle > LeftFlipper.EndAngle + 0.01 Then  'If the flipper has swung past it's end of swing Then...
		LeftFlipper.eosTorque = EOST			'set the flipper EOS Torque back to the base table setting
		LeftFlipper.eosTorqueAngle = EOSA		'set the flipper EOS Torque Angle back to the base table setting
		LeftFlipper.RampUp = fRampUp			'set the flipper Ramp Up back to the base table setting
		LeftFlipper.Elasticity = fElasticity	'set the flipper Elasticity back to the base table setting
	End If

	If RightFlipper.CurrentAngle = RightFlipper.EndAngle and RFPress = 1 Then
		RightFlipper.eosTorqueAngle = EOSaNew
		RightFlipper.eosTorque = EOStNew
		RightFlipper.RampUp = EOSRampUp
										 
		If GameTime - RFCount < LiveCatch Then
			RightFlipper.Elasticity = 0.1
			If RightFlipper.EndAngle <> RFEndAngle Then RightFlipper.EndAngle = RFEndAngle
		Else
			RightFlipper.Elasticity = fElasticity
		End If
	ElseIf RightFlipper.CurrentAngle < RightFlipper.StartAngle + 0.05 Then
		RightFlipper.RampUp = SOSRampUp 
		RightFlipper.EndAngle = RFEndAngle + 3
		RightFlipper.Elasticity = fElasticity
		RFCount = 0 
	ElseIf RightFlipper.CurrentAngle < RightFlipper.EndAngle - 0.01 Then 
		RightFlipper.eosTorque = EOST
		RightFlipper.eosTorqueAngle = EOSA
		RightFlipper.RampUp = fRampUp
		RightFlipper.Elasticity = fElasticity
	End If

	If LeftFlipper001.CurrentAngle = LeftFlipper001.EndAngle and LFPress = 1 Then 
		LeftFlipper001.eosTorqueAngle = EOSaNew
		LeftFlipper001.eosTorque = EOStNew
		LeftFlipper001.RampUp = EOSRampUp
										 
		If GameTime - LFCount < LiveCatch Then
			LeftFlipper001.Elasticity = 0.1
			If LeftFlipper001.EndAngle <> LF1EndAngle Then LeftFlipper001.EndAngle = LF1EndAngle
		Else	
			LeftFlipper001.Elasticity = fElasticity
		End If
	ElseIf LeftFlipper001.CurrentAngle > LeftFlipper001.StartAngle - 0.05  Then
		LeftFlipper001.RampUp = SOSRampUp
		LeftFlipper001.EndAngle = LF1EndAngle - 3
		LeftFlipper001.Elasticity = fElasticity
		LFCount = 0
	ElseIf LeftFlipper001.CurrentAngle > LeftFlipper001.EndAngle + 0.01 Then 
		LeftFlipper001.eosTorque = EOST
		LeftFlipper001.eosTorqueAngle = EOSA
		LeftFlipper001.RampUp = fRampUp
		LeftFlipper001.Elasticity = fElasticity
	End If

	If RightFlipper001.CurrentAngle = RightFlipper.EndAngle and RFPress = 1 Then
		RightFlipper001.eosTorqueAngle = EOSaNew
		RightFlipper001.eosTorque = EOStNew
		RightFlipper001.RampUp = EOSRampUp
										 
		If GameTime - RFCount < LiveCatch Then
			RightFlipper001.Elasticity = 0.1
			If RightFlipper001.EndAngle <> RF1EndAngle Then RightFlipper001.EndAngle = RF1EndAngle
		Else
			RightFlipper001.Elasticity = fElasticity
		End If
	ElseIf RightFlipper001.CurrentAngle < RightFlipper001.StartAngle + 0.05 Then
		RightFlipper001.RampUp = SOSRampUp 
		RightFlipper001.EndAngle = RF1EndAngle + 3
		RightFlipper001.Elasticity = fElasticity
		RFCount = 0 
	ElseIf RightFlipper001.CurrentAngle < RightFlipper001.EndAngle - 0.01 Then 
		RightFlipper001.eosTorque = EOST
		RightFlipper001.eosTorqueAngle = EOSA
		RightFlipper001.RampUp = fRampUp
		RightFlipper001.Elasticity = fElasticity
	End If

	If PlungerSound=1 and Plunger.position>=8 and ShowDT=False Then
		If Plunge = 0 Then PlaySoundAt "CHPlungerPull", Plunger
		Plunge = 1
	End If
	If PlungerSound=1 and Plunger.position<=3 and ShowDT=False Then
		If Plunge = 1 Then PlaySoundAt "CHPlungerRelease", Plunger
		Plunge = 0
	End If
														 

													  
						  
														   
						 
													   
																																						  
										
							 
					  
																											  
										   
							 
					  
					
		 
								
						   
										   
	   
															  
	
		 
		
End Sub

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity
Dim LF1 : Set LF1 = New FlipperPolarity
Dim RF1 : Set RF1 = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	Dim x, a : a = Array(LF, RF, LF1, RF1)
	For Each x in a
		'safety coefficient (diminishes polarity correction only)
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		x.TimeDelay = 69    '*****Important, this variable is an offset For the speed that the ball travels down the table to determine If the flippers have been fired 
							'This is needed because the corrections to ball trajectory should only applied If the flippers have been fired and the ball is in the trigger zones.
							'FlipAT is set to GameTime when the ball enters the flipper trigger zones and If GameTime is less than FlipAT + this time delay Then changes to velocity
							'and trajectory are applied.  If the flipper is fired before the ball enters the trigger zone Then with this delay added to FlipAT the changes
							'to tragectory and velocity will not be applied.  Also If the flipper is in the final 20 degrees, changes to ball values will also not be applied.
							'"Faster" tables will need a smaller value while "slower" tables will need a larger value to give the ball more time to get to the flipper. 		
							'If this value is not set high enough the Flipper Velocity and Polarity corrections will NEVER be applied.
	Next

	'rf.report "Polarity"
	AddPt "Polarity", 0, 0, -2.7
	AddPt "Polarity", 1, 0.16, -2.7	
	AddPt "Polarity", 2, 0.33, -2.7
	AddPt "Polarity", 3, 0.37, -2.7	'4.2
	AddPt "Polarity", 4, 0.41, -2.7
	AddPt "Polarity", 5, 0.45, -2.7 '4.2
	AddPt "Polarity", 6, 0.576,-2.7
	AddPt "Polarity", 7, 0.66, -1.8'-2.1896
	AddPt "Polarity", 8, 0.743, -0.5
	AddPt "Polarity", 9, 0.81, -0.5
	AddPt "Polarity", 10, 0.88, 0

	'"Velocity" Profile
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

	LF.Object = LeftFlipper
	LF1.Object = LeftFlipper001
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	LF1.EndPoint = EndPointLp001
	RF.Object = RightFlipper
	RF1.Object = RightFlipper001
	RF1.EndPoint = EndPointRp001
	RF.EndPoint = EndPointRp
End Sub

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper For adjusting flipper script in-game
	Dim a : a = Array(LF, RF, LF1, RF1)
	Dim x : For Each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Sub TriggerLF_Hit()
	LFCount = gameTime
	If FlipperLength = 3 Then LF.Addball activeball 
End Sub
Sub TriggerLF_UnHit() 
	If FlipperLength = 3 Then LF.PolarityCorrect activeball 
End Sub
Sub TriggerRF_Hit()
	RFCount = gameTime
	If FlipperLength = 3 Then RF.Addball activeball 
End Sub
Sub TriggerRF_UnHit() 
	If FlipperLength = 3 Then RF.PolarityCorrect activeball  
End Sub
Sub TriggerLF1_Hit() 
	If FlipperLength = 2 Then LF1.Addball activeball 
End Sub
Sub TriggerLF1_UnHit() : 
	If FlipperLength = 2 Then LF1.PolarityCorrect activeball 
End Sub
Sub TriggerRF1_Hit()  
	If FlipperLength = 2 Then RF1.Addball activeball 
End Sub
Sub TriggerRF1_UnHit() 
	If FlipperLength = 2 Then RF1.PolarityCorrect activeball 
End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, If .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

'***************This is flipperPolarity's addPoint Sub
Class FlipperPolarity
	Public Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut

	Public Sub Class_Initialize 
		reDim PolarityIn(0) : reDim PolarityOut(0) : reDim VelocityIn(0) : reDim VelocityOut(0) : reDim YcoefIn(0) : reDim YcoefOut(0)
		Enabled = True: TimeDelay = 50 : LR = 1:  Dim x : For x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new spoofBall: next  
	End Sub
	
	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : If IsObject(aInput) Then FlipperStart = aInput.x Else FlipperStart = aInput : End If : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : If IsObject(aInput) Then FlipperEnd = aInput.x Else FlipperEnd = aInput : End If : End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property
	
	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select

	End Sub 

'********Triggered by a ball hitting the flipper trigger area	
	Public Sub AddBall(aBall) : Dim x : 
		For x = 0 to uBound(balls)  
			If IsEmpty(balls(x)) Then set balls(x) = aBall : exit Sub :End If  
		Next   
	End Sub

	Private Sub RemoveBall(aBall)
		Dim x : For x = 0 to uBound(balls)
			If TypeName(balls(x) ) = "IBall" Then 
				If aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub

'*********Used to rotate flipper since this is removed from the key down For the flippers	
	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
		FlipperOn
	End Sub

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x : For x = 0 to uBound(balls)
			If not IsEmpty(balls(x) ) Then balldata(x).Data = balls(x)
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))  '% of flipper swing
		PartialFlipCoef = abs(PartialFlipCoef-1) 'corrects For negative flipper angles
		If abs(Flipper.currentAngle - Flipper.EndAngle) < 20 Then 'last 20 degrees of swing is not dealt with
			PartialFlipCoef = 0
		End If
	End Sub

'***********gameTime is a global variable of how long the game has progressed in ms
'***********This function lets the table know If the flipper has been fired
	Private Function FlipperOn() 
																																																   
		If gameTime < FlipAt + TimeDelay Then FlipperOn = True 
	End Function	'Timer shutoff For polaritycorrect 
	
'***********This is turned on when a ball leaves the flipper trigger area
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then 'don't run this If the flippers are at rest
											  
			Dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
			Dim teststr : teststr = "Cutoff"
			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
			If tmp < 0.1 Then 'If real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
			End If

			'y safety Exit
			If aBall.VelY > -8 Then 'If ball going down Then remove the ball
				RemoveBall aBall
				exit Sub
			End If
			'Find balldata. BallPos = % on Flipper
			For x = 0 to uBound(Balls)
				If aBall.id = BallData(x).id AND not isempty(BallData(x).id) Then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				End If
			Next

			'Velocity correction
			If not IsEmpty(VelocityIn(0) ) Then
						 
				Dim VelCoef
				If IsEmpty(BallData(idx).id) and aBall.VelY < -12 Then 'If tip hit with no collected data, do vel correction anyway
					If PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 Then 'adjust plz
						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
						If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
						If Enabled Then aBall.Velx = aBall.Velx*VelCoef'VelCoef
						If Enabled Then aBall.Vely = aBall.Vely*VelCoef'VelCoef
					End If
				Else
		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
					If Enabled Then aBall.Velx = aBall.Velx*VelCoef
					If Enabled Then aBall.Vely = aBall.Vely*VelCoef
				End If
			End If

			'Polarity Correction (optional now)
			If not IsEmpty(PolarityIn(0) ) Then
				If StartPoint > EndPoint Then LR = -1	'Reverse polarity If left flipper
				Dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				If Enabled Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'================================
'Helper Functions


Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount : aCount = 0
	reDim a(uBound(aArray) )
	For x = 0 to uBound(aArray)	'Shuffle objects in a temp array
		If not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then 
				Set a(aCount) = aArray(x) 'Set creates an object in VB
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	reDim aArray(aCount-1+offset)	'Resize original array
	For x = 0 to aCount-1		'set objects back into original array
		If IsObject(a(x)) Then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

'**********Takes in more than one array and passes them to ShuffleArray
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

'**********Calculate ball speed as hypotenuse of velX/velY triangle
Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

'**********Calculates the value of Y For an input x using the slope intercept equation
Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

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

'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub

'*********This sets up the rubbers:
Dim RubbersD  
Set RubbersD = new Dampener  'Makes a Dampener Class Object 	
RubbersD.name = "Rubbers"

'cor bounce curve (linear)
'For best results, try to match in-game velocity as closely as possible to the desired curve
RubbersD.addpoint 0, 0, 0.935 '0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.935 '0.96
RubbersD.addpoint 2, 5.76, 0.942 '0.967	'dont take this as gospel. If you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

Dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.CopyCoef RubbersD, 0.85

'**********Class For dampener section of nfozzy's code
Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful For Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : reDim ModIn(0) : reDim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
	End Sub

	public Sub Dampen(aBall)
		If threshold Then If BallSpeed(aBall) < threshold Then exit Sub End If End If
		Dim RealCOR, DesiredCOR, str, coef

						 
																			   
													  
'               Uses the LinearEnvelope function to calculate the correction based upon where it's value sits in relation	
 
'               to the addpoint parameters set above.  Basically interpolates values between set points in a linear fashion
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		
'                Uses the function BallSpeed's value at the point of impact/the active ball's velocity which is constantly being updated	
'               RealCor is always less than 1 
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)

'               Divides the desired CoR by the real COR to make a multiplier to correct velocity in x and y
		coef = desiredcor / realcor 
'		tb.text = "Coef = " & coef
		
'               Applies the coef to x and y velocities
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
	End Sub

'***********This Sub sets the values For Sleeves (or any other future objects) to 85% (or whatever is passed in) of Posts
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x : For x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub
End Class

'*****************************Generates cor.ballVel For dampener
Sub RDampen_Timer() ' 1 ms timer always on
	CoR.Update
End Sub

'*********CoR is Coefficient of Restitution defined as "how much of the kinetic energy remains For the objects 
'to rebound from one another vs. how much is lost as heat, or work done deforming the objects 
Dim cor : set cor = New CoRTracker

Class CoRTracker

	public ballvel

	Private Sub Class_Initialize : reDim ballvel(0) : End Sub 
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, allBalls, highestID :
		allBalls = getballs

		For Each b in allballs
			If b.id >= HighestID Then highestID = b.id
		Next

		If uBound(ballvel) < highestID Then reDim ballvel(highestID)	'set bounds

		For Each b in allballs
			ballvel(b.id) = BallSpeed(b)
		Next
	End Sub
End Class

'********Interpolates the value For areas between the low and upper bounds sent to it
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	Dim ii : For ii = 1 to uBound(xKeyFrame)	'find active line
		If xInput <= xKeyFrame(ii) Then L = ii : exit For : End If
	Next
	If xInput > xKeyFrame(uBound(xKeyFrame) ) Then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'clamp 2.0
	If xInput <= xKeyFrame(lBound(xKeyFrame) ) Then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	If xInput >= xKeyFrame(uBound(xKeyFrame) ) Then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

'************************************************************************
'                         Ball Control
'************************************************************************

Dim Cup, Cdown, Cleft, Cright, Zup, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti
 
bcboost = 1 'Do Not Change - default setting
bcvel = 4 'Controls the speed of the ball movement
bcyveloffset = -0.1 'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3 'Boost multiplier to ball veloctiy (toggled with the B key)

Sub BallControl_Timer()
    If Contball and ContBallInPlay Then
        If Cright = 1 Then
            ControlBall.velx = bcvel*bcboost
          ElseIf Cleft = 1 Then
            ControlBall.velx = -bcvel*bcboost
          Else
            ControlBall.velx=0
        End If
        If Cup = 1 Then
            ControlBall.vely = -bcvel*bcboost
          ElseIf Cdown = 1 Then
            ControlBall.vely = bcvel*bcboost
          Else
            ControlBall.vely = bcyveloffset
        End If
        If Zup = 1 Then
            ControlBall.velz = bcvel*bcboost
		Else
			ControlBall.velz = -bcvel*bcboost
        End If
    End If
End Sub

'******* For ball control script
Sub endControl_Hit()              
    contBallInPlay = False
End Sub


Sub Table1_MusicDone()	
 
End Sub

Sub Table1_Paused()
 
End Sub

Sub Table1_UnPaused()
 
End Sub

'PUP: trigger First BOSS FIGHT event here if not multiball <<<<<<<<<<<<

	   

							
 
	   

Sub PgateTrigger_Hit()
																								
	If MultiballOn = 0 Then
		pupevent 201
	End If
	introStarted = False
End Sub

Sub PgateTrigger_Timer()	
End Sub
'>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<

						
 
	   

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup Player

Sub PuPStart(cPuPPack)
    If PUPStatus=true Then Exit Sub
    If usePUP=true then
        Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If

		If usePUPDMD Then
			dmdScreen=5
		Else
			dmdScreen=2
		End If

		PuPlayer.LabelInit dmdScreen
																																																								   
																																																												  
					  
		PuPlayer.LabelNew dmdScreen,"Players","Aafia",5,RGB(256, 256, 256),0,0,0,3,95,1,1
		PuPlayer.LabelNew dmdScreen,"Credits","Aafia",5,RGB(256, 256, 256),0,2,0,97,95,1,1
		PuPlayer.LabelSet dmdScreen,"Players","PLAYERS: 0",1,""
												 

		If Not usePUPDMD Then
			PuPlayer.LabelNew dmdScreen,"Ball","Aafia",5,RGB(256, 256, 256),0,2,0,59.4,95,1,1
			PuPlayer.LabelSet dmdScreen,"Ball","BALL: 0",1,""
		End If
		PuPlayer.LabelShowPage dmdScreen,1,0,""
    End If
End Sub

Sub pupevent(EventNum)
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

PuPStart(cPuPPack) 'Check for PuP - If found, then start Pinup Player / PuP-Pack


'//////////////////// PUP FULLDMD SCORING EVENTS //////////////////////////
'P1-X0000,E510
'P1-10000,E511
'P1-20000,E512
'P1-30000,E513
'P1-40000,E514
'P1-50000,E515
'P1-60000,E516
'P1-70000,E517
'P1-80000,E518
'P1-90000,E519
'P1-0X000,E610
'P1-01000,E611
'P1-02000,E612
'P1-03000,E613
'P1-04000,E614
'P1-05000,E615
'P1-06000,E616
'P1-07000,E617
'P1-08000,E618
'P1-09000,E619
'P1-00X00,E710
'P1-00100,E711
'P1-00200,E712
'P1-00300,E713
'P1-00400,E714
'P1-00500,E715
'P1-00600,E716
'P1-00700,E717
'P1-00800,E718
'P1-00900,E719
'P1-000X0,E810
'P1-00010,E811
'P1-00020,E812
'P1-00030,E813
'P1-00040,E814
'P1-00050,E815
'P1-00060,E816
'P1-00070,E817
'P1-00080,E818
'P1-00090,E819
'P1-0000X,E910
'P1-00001,E911
'P1-00002,E912
'P1-00003,E913
'P1-00004,E914
'P1-00005,E915
'P1-00006,E916
'P1-00007,E917
'P1-00008,E918
'P1-00009,E919
'P2-X0000,E520
'P2-10000,E521
'P2-20000,E522
'P2-30000,E523
'P2-40000,E524
'P2-50000,E525
'P2-60000,E526
'P2-70000,E527
'P2-80000,E528
'P2-90000,E529
'P2-0X000,E620
'P2-01000,E621
'P2-02000,E622
'P2-03000,E623
'P2-04000,E624
'P2-05000,E625
'P2-06000,E626
'P2-07000,E627
'P2-08000,E628
'P2-09000,E629
'P2-00X00,E720
'P2-00100,E721
'P2-00200,E722
'P2-00300,E723
'P2-00400,E724
'P2-00500,E725
'P2-00600,E726
'P2-00700,E727
'P2-00800,E728
'P2-00900,E729
'P2-000X0,E820
'P2-00010,E821
'P2-00020,E822
'P2-00030,E823
'P2-00040,E824
'P2-00050,E825
'P2-00060,E826
'P2-00070,E827
'P2-00080,E828
'P2-00090,E829
'P2-0000X,E920
'P2-00001,E921
'P2-00002,E922
'P2-00003,E923
'P2-00004,E924
'P2-00005,E925
'P2-00006,E926
'P2-00007,E927
'P2-00008,E928
'P2-00009,E929

Sub updatePuPDMD()
    If (usePUP=false or PUPStatus=false) Then Exit Sub

	Dim TenThousands, Thousands, Hundreds, Tens, Remainder, AdjustedScore

	AdjustedScore = Score(Player) Mod 100000
	If ( Score(Player) > 100000 ) Then
		pupevent INT("4" & CStr(Player+1) & CStr((Score(Player) / 100000) - 1))
	End If

	TenThousands = AdjustedScore / 10000
	Remainder = AdjustedScore Mod 10000
	Thousands = remainder / 1000
	Remainder = Remainder Mod 1000
	Hundreds = Remainder / 100
	remainder = Remainder Mod 100
	Tens = Remainder / 10
	Remainder = Remainder Mod 10

	pupevent INT("9" & CStr(Player) & CStr(Remainder))
	pupevent INT("8" & CStr(Player) & CStr(Tens))
	pupevent INT("7" & CStr(Player) & CStr(Hundreds))
	pupevent INT("6" & CStr(Player) & CStr(Thousands))
	pupevent INT("5" & CStr(Player) & CStr(TenThousands))

End Sub

