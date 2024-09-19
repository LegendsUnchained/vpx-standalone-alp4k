'****************************************************************
'
'         American Graffiti (Original 2024) by Onevox
'				        Script by Scottacus
'				  	   	      v 0.19
'					        August 2024
'
'	Basic DOF config
						 
'		101 Left Flipper, 102 Right Flipper
'		103 - 104 Slings
'		105 -107 Bumpers
'		110 DragLane Kicker
'		124 Drain, 125 Ball Release
'	 	127 credit light
'		128 Knocker
'		153 Chime1-10s, 154 Chime2-100s, 155 Chime3-1000s
'
'		Code Flow
'									 EndGame 
'										^														
'		Start Game -> New Game -> Check Continue -> Release Ball -> Drain -> Score Bouns -> Advance Players -> Next Ball
'									                                                                                |
'									EndGame = True <-----------------------------------------------------------------
'***********************************************************************************************************************
																 
Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

Const cGameName  = "AmericanGraffiti_1962"
Const hsFileName = "American Graffiti (Original 2024)"
Const cOptions   = "American_Graffiti_v0.17.txt"

Dim vrOption
'*************************** VR Room****************************************************
Dim VR_Room:
' NOTE VR Room can be toggled on/off in the left flipper options menu
If RenderingMode = 2 Then 
	vrOption = 1		' used for no VR Room
	'vrOption = 2		' used to see VR Room
Else
	vrOption = 0
End If

vrRoomToggle

Dim UseFlexDMD

UseFlexDMD = True
If UseFlexDMD = True Then DMD_Init

'********************************* Left Out Lane ***************************************
Dim leftOutLane 
leftOutLane = 0 'set to 1 for always guide to drag race lane, 0 to only allow one ball saved to drag race lane

'*************************** Glass Scratches *******************************************
Const VRGlassScratches = 0  ' set this to 1 to turn on VR glass scratches
If vrOption > 1 and VRGlassScratches > 0 then GlassImpurities.visible = true

'*************************** PinballY Settings *****************************************
'Set this variable to 1 to save a PinballY High Score file to your Tables Folder
'this will let the Pinball Y front end display the high scores when searching for tables
'0 = No PinballY High Scores, 1 = Save PinballY High Scores 
Const cPinballY = 1

'************************** New Ball Shadow Code ***************************************
Const DynamicBallShadowsOn = 1 '0 = no dynamic ball shadows, 1 = enable dynamic ball shadow 
Const AmbientBallShadowOn  = 0  '0 = no regular ball shadow   1 = regular ball shadow on

'***********************************************************************************************

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking (this sometimes goes in the RDampen_Timer sub)
	Pgate.rotz = GateR.CurrentAngle * 0.5
	Pgate001.rotz = GateL.CurrentAngle * 0.5
	dim xx
	For each xx in DropTargets
		If xx.isDropped = 1 Then xx.visible = 0
	Next
End Sub

'****** End New ball Shadow Code..  More at bottom of script ****************************************************************************
Dim bip
Dim balls
Dim replays
Dim maxPlayers
Dim players
Dim player
Dim credit
Dim score(6)
Dim hScore(6)
Dim sReel(6)
Dim state
Dim tilt
Dim i,j, f, ii, Object, Light, x, y
Dim freePlay 
Dim ballsize, BallMass
ballSize = 50
ballMass = (Ballsize^3)/125000
Dim gateState
Dim staticLights
Dim playerLights(5)
Dim musicOn
Dim musicVol
Dim checkOnOff

staticLights = Array("L001","L002","L003","L004","L010","L011","L012","L014","L110","L112","L114","L116","L118","L120")
For i = 1 to 4
	playerLights(i) = Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
Next
'Dim HSInitial0, HSInitial1, HSInitial2
Dim EnableInitialEntry
Dim ShowBallShadow
Dim Chime
Dim Options
Dim PFOption
Dim BonusScore
Dim FirstBallOut
Dim lutValue

'Desktop reel pans,  Valid values: "Lpan", "Mpan" and "Rpan"
Const dtcrReel = "Rpan"
Const dts1Reel = "Lpan"
Const dts2Reel = "Rpan"
Const dts3Reel = "Lpan"
Const dts4Reel = "Rpan"

'Backglass reel pans,  Valid values: "Lpan", "Mpan" and "Rpan"
Const bgcrReel = "Mpan"
Const bgs1Reel = "Lpan"
Const bgs2Reel = "Rpan"
Const bgs3Reel = "Lpan"
Const bgs4Reel = "Rpan"

Dim musicDir, voDir
Sub Table1_init
	LoadEM
	MaxPlayers=4

	player = 1
	loadHighScore

	If highScore(0)="" Then highScore(0)=50000
	If highScore(1)="" Then highScore(1)=45000
	If highScore(2)="" Then highScore(2)=40000
	If highScore(3)="" Then highScore(3)=35000
	If highScore(4)="" Then highScore(4)=30000	
										 		
	For x = 1 to maxPlayers
		Set sReel(x) = EVAL("scoreReel" & x)
	Next
				   
	If initial(0,1) = "" Then
		initial(0,1) = 19: initial(0,2) = 5: initial(0,3) = 13
		initial(1,1) = 1: initial(1,2) = 1: initial(1,3) = 1
		initial(2,1) = 2: initial(2,2) = 2: initial(2,3) = 2
		initial(3,1) = 3: initial(3,2) = 3: initial(3,3) = 3
		initial(4,1) = 4: initial(4,2) = 4: initial(4,3) = 4
	End If
	If credit = "" Then credit = 0
	If freePlay = "" Then freePlay = 1
	If balls = "" Then balls = 5
	If chime = "" Then chime = 0
	If showDT = True And vrOption = 0 Then pfOption = 1
	If pfOption = "" Then pfOption = 1
	If score(1) = "" Then score(1) = 0
	If musicOn = "" Then musicOn = 1
	If musicVol = "" Then musicVol = .5
	oldMusicVol = musicVol

	replaySettings

	firstBallOut = 0
	updatePostIt
	tiltReel.SetValue(1)
	If vrOption > 0 Then vrTilt.visible = 1
	creditReel.setvalue(credit)
	If vrOption > 0 Then vrCredit
	For Each xx in Lights:xx.state = 0: Next

	bootTable.Enabled = 1

	If showDT = True And vrOption = 0 Then
		For each xx in backdropstuff: xx.visible = 1: Next
	Else		   
		For each xx in backdropstuff: xx.visible = 0: Next
		table1.BackdropImage_DT = ""
	End If

	Table1.ColorGradeImage = "AGLUT4"

	musicDir = musicdirectory("AG")
	voDir = musicdirectory("AGSounds")

	For Each xx in MelCheckCol: xx.visible = 0 : Next

	startUpSound.enabled = 1
	tilt = False
	state = False
	gameState
	gateState = 1
	drain.CreateSizedBallWithMass Ballsize/2, BallMass
End Sub

Dim ssCount, lightCounter
Sub startUpSound_Timer
	ssCount = ssCount + 1
	If ssCount = 1 Then playOcxMusic "sfx_RadioScan.mp3"
	If ssCount > 23 Then
		ssCount = 0
		If musicOn = 1 Then playOcxMusic  "AGattract.mp3"
		If B2Son Then controller.B2SsetData 140, 1
		startUpSound.enabled = 0
	End If

End Sub


'****************KeyCodes								
Sub Table1_KeyDown(ByVal keycode)

	If enableInitialEntry = True Then enterIntitals(keycode)
   
	If keycode = addCreditKey Then
		playFieldSound "coinin",0,Drain,1 
		addCredit = 1
		scoreMotor5.enabled = 1
    End If

    If keycode = startGameKey Then
		'StartButton.y = 755 - 5
		If enableInitialEntry = False and operatorMenu = 0 Then
			If freePlay = 1 and players < MaxPlayers and firstBallOut = 0 Then startGame
			If freePlay = 0 and credit > 0 and players < MaxPlayers and firstBallOut = 0 Then
				credit = credit - 1
				If showDT = False Then PlayReelSound "Reel5", bgcrReel Else PlayReelSound "Reel5", dtcrReel
				creditReel.setvalue(credit)	
				If B2SOn Then 
					If freeplay = 0 Then controller.B2SSetCredits credit
					If freePlay = 0 and credit < 1 Then DOF 127, DOFOff
				End If
				startGame
			End If
		End If
	End If
	  
	If keycode = rightFlipperKey Then
		If vrOption > 0 Then VRFlipperButtonRight.X = 2190 - 8
	End If

	If keycode = leftFlipperKey and contball = 0 Then
		If vrOption > 0 Then VRFlipperButtonLeft.X = 2122 + 8
	End If
	  
	If keycode = PlungerKey Then
		plunger.PullBack
		If showDT = True Then playFieldSound "plungerpull", 0, plunger, 1
		If vrOption > 0 Then 
			TimerVRPlunger.Enabled = True
			TimerVRPlunger1.Enabled = False
			PinCab_Shooter.Y = -500
		End If
	End If

  If tilt = False and state = True Then
	If keycode = leftFlipperKey and contball = 0 and tilt = False Then
		FlipperActivate LeftFlipper, LFPress
		lf.fire
		playFieldSound "FlipUpL", 0, leftFlipper, 1
		If B2SOn Then DOF 101,DOFOn
		playFieldSound "FlipBuzzL", -1, leftFlipper, 1
		rotateLeft
	End If
    
	If keycode = RightFlipperKey and contball = 0 and tilt = False Then
		FlipperActivate RightFlipper, RFPress
		rf.fire
		playFieldSound "FlipUpR", 0, RightFlipper,1
		If B2SOn Then DOF 102,DOFOn
		playFieldSound "FlipBuzzR", -1, RightFlipper,1
		rotateRight
	End If
    
	If keycode = leftTiltKey Then
		Nudge 90, 2
		checkTilt
	End If
    
	If keycode = rightTiltKey Then
		Nudge 270, 2
		checkTilt
	End If
    
	If keycode = centerTiltKey Then
		Nudge 0, 2
		checkTilt
	End If
  End If

	If keycode = RightMagnaSave and contball = 0 Then
		If state = True Then
			If contball = 0 and musicOn = 1 Then
				songNumber = songNumber + 1
				If songNumber > 24 Then songNumber = 0
				songPlayer
			End If
		End If
	End If

	If keycode = LeftMagnaSave And contball = 0 Then
		updateMelCheck
		If checkOnOff = 0 Then
			checkOnOff = 1
			For Each xx in MelCheckCol: xx.visible = 1: Next
		Else
			checkOnOff = 0
			For Each xx in MelCheckCol: xx.visible = 0: Next
		End If
	End If

	If keycode = mechanicalTilt Then 
		tilt = True
		PlaySound "SFX_MusicOver", 0, musicVol
		tiltReel.SetValue(1)
		If vrOption > 0 Then vrTilt.visible = 1
		If B2SOn Then Controller.B2SSetTilt 1
		turnOff
	End If

    If keycode = leftFlipperKey and state = False and operatorMenu = 0 and enableInitialEntry = 0 Then
        operatorMenuTimer.Enabled = true
    end if
 
    If keycode = leftFlipperKey and state = False and operatorMenu = 1 Then
		options = options + 1
		If renderingMode <> 2 Then If options = 5 Then options = 6
		If showDt = True Then If options = 6 Then options = 7 'skips non DT options
        If options = 8 Then options = 0
		optionMenu.visible = true
         playFieldSound "target", 0, SoundPointScoreMotor, .2
        Select Case (Options)
            Case 0:
                optionMenu.image = "FreeCoin" & freePlay
            Case 1:
                optionMenu.image = balls & "Balls"
			Case 2:
				optionMenu.image = "Music" & musicOn
			Case 3:
				optionMenu.image = "musicVolume"
				optionMenu1.visible = 1
				If musicVol < 1 Then
					optionMenu1.image = "vol" & (musicVol * 10)
				Else
					optionMenu1.image = "vol10"
				End If
 			Case 4:
				If showDT = False Then optionMenu.image = "UnderCab"
				If showDT = True Then OptionMenu.visible = False
				optionMenu1.visible = 1					  
				optionMenu1.image = "Sound" & pfOption
				optionMenu2.visible = 1
				optionMenu2.image = "SoundChange"
				Select Case (PFOption)
					Case 1: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 0: speaker4.visible = 0
					Case 2: speaker5.visible = 1: speaker6.visible = 1: speaker1.visible = 0: speaker2.visible = 0
					Case 3: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 1: speaker4.visible = 1: speaker5.visible = 0: speaker6.visible = 0
				End Select
			Case 5:
				For x = 1 to 6
					EVAL("Speaker" & x).visible = 0
				Next
				optionMenu1.visible = 0
				OptionMenu2.visible = 0
				optionMenu.image = "VR" & vrOption
			Case 6:
				optionMenu2.visible = 0
				For x = 1 to 6
					EVAL("Speaker" & x).visible = 0
				Next
				optionMenu1.image = "DOF"
				optionMenu.image = "Chime" & chime
			Case 7:
				For x = 1 to 6
					EVAL("Speaker" & x).visible = 0
				Next
				optionMenu1.visible = 0
				optionMenu2.visible = 0
				optionMenu.image = "SaveExit"
        End Select
    End If
 
    If keycode = rightFlipperKey and state = False and operatorMenu = 1 Then
      playFieldSound "metalHitHigh", 0, SoundPointScoreMotor, 0.2
      Select Case (options)
		Case 0:
            If freePlay = 0 Then
                freePlay = 1				
              Else
                freePlay = 0											
            End If
            optionMenu.image= "FreeCoin" & freePlay
			If freePlay = 0 Then
				If credit > 0 and B2SOn Then DOF 127, DOFOn
				If credit < 1 and B2SOn Then DOF 127, DOFOff
			Else
				If B2SOn Then DOF 127, DOFOn
			End If
        Case 1:
            If balls = 3 Then
                balls = 5
              Else
                balls = 3
            End If
            OptionMenu.image = balls & "Balls"
		Case 2:
			If musicOn = 0 Then
				musicOn = 1
				playOcxMusic  "AGattract.mp3"
			Else
				musicOn = 0
				ocxPlayerMusic.controls.stop
			End If
			optionMenu.image = "music" & musicOn
		Case 3:
			musicVol = musicVol + .1
			If musicVol > 1 Then musicVol = .1
			oldmusicVol = musicVol
			If musicVol < 1 Then
				optionMenu1.image = "vol" & (musicVol *10)
			Else
				optionMenu1.image = "vol10"
			End If
			ocxPlayerMusic.settings.volume = musicVol*100
		Case 4:
			pfOption = pfOption + 1
			If showDt = True and pfOption = 2 Then pfOption = 3
			If pfOption = 4 Then pfOption = 1
			optionMenu1.visible = 1
			optionMenu1.image = "Sound" & pfOption
			Select Case (pfOption)
				Case 1: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 0: speaker4.visible = 0
				Case 2: speaker5.visible = 1: speaker6.visible = 1: speaker1.visible = 0: speaker2.visible = 0
				Case 3: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 1: speaker4.visible = 1: speaker5.visible = 0: speaker6.visible = 0
			End Select
		Case 5:
			If vrOption = 1 Then
				vrOption = 2
			Else
				vrOption = 1
			End If
			vrRoomToggle
			optionMenu.image = "VR" & vrOption	
		Case 6:
            If chime = 0 Then
                chime= 1
				If B2SOn Then DOF 155,DOFPulse
              Else
                chime = 0
				pts10
            End If
			optionMenu.image = "Chime" & chime		
        Case 7:
            operatorMenu = 0
            saveHighScore
			dynamicUpdatePostIt.enabled = 1
			optionMenu.image = "FreeCoin" & freePlay
            optionMenu1.visible = 0
			optionMenu.visible = 0
			optionsMenu.visible = 0	
			replay(1) = 136000
			replay(2) = 185000
			replay(3) = 100000000
		End Select
    End If

    If keycode = 46 Then' C Key
       stopSound "FlipBuzzLA"
       stopSound "FlipBuzzLB"
       stopSound "FlipBuzzLC"
       stopSound "FlipBuzzLD"
       stopSound "FlipBuzzRA"
       stopSound "FlipBuzzRB"
       stopSound "FlipBuzzRC"
       stopSound "FlipBuzzRD"
        If contball = 1 Then
            contball = 0
          Else
            contball = 1
        End If
    End If
 
    If keycode = 48 Then 'B Key
        If bcboost = 1 Then
            bcboost = bcboostmulti
          Else
            bcboost = 1
        End If
    End If
 
    If keycode = 203 Then Cleft = 1 ' Left Arrow
 
    If keycode = 200 Then Cup = 1 ' Up Arrow
 
    If keycode = 208 Then Cdown = 1 ' Down Arrow
 
    If keycode = 205 Then Cright = 1 ' Right Arrow

	If keycode = 52 Then Zup = 1 ' Period

'************************Start Of Test Keys****************************
	If Keycode = 30 Then 'a' key
		topLanes(player) = topLanes(player) + 1
		updateMelCheck
	End If

	If Keycode= 31 Then 's' key
		score(1) = 60000
		score(2) = 49000
		checkHighScores
	End If

	If Keycode = 33 Then 'f' key
		check20.image = "HS_3"
	End If
'************************End Of Test Keys**************************** 
End Sub
dim z

Sub Table1_KeyUp(ByVal keycode)

	If keycode = rightFlipperKey Then
		If vrOption > 0 Then VRFlipperButtonRight.X = 2190
	End If

	If keycode = leftFlipperKey and contball = 0 Then
		If vrOption > 0 Then VRFlipperButtonLeft.X = 2122
	End If

	If keycode = StartGameKey Then
		'StartButton.y = 1890
	End If

	If keycode = plungerKey Then
		If vrOption > 0 Then 
			TimerVRPlunger.Enabled = False
			TimerVRPlunger1.Enabled = True
			PinCab_Shooter.Y = -500
		End If
		plunger.Fire
		If showDt = True Then playFieldSound "PlungerFire", 0, plunger, 1
	End If

    If keycode = leftFlipperKey Then
        operatorMenuTimer.Enabled = False
    End If

   If tilt = False and state = True Then
		If keycode = leftFlipperKey and contBall = 0 Then
			FlipperDeActivate LeftFlipper, LFPress
			lfpress = 0
			LeftFlipper.eosTorqueAngle = EOSA
			LeftFlipper.eosTorque = EOST
			LeftFlipper.RotateToStart
			playFieldSound "FlipDownL", 0, leftFlipper, 1
			If B2SOn Then DOF 101,DOFOff
			stopSound "FlipBuzzLA"
			stopSound "FlipBuzzLB"
			stopSound "FlipBuzzLC"
			stopSound "FlipBuzzLD"
		End If
    
		If keycode = rightFlipperKey and contBall = 0 Then
			FlipperDeActivate RightFlipper, RFPress
			rfpress = 0
			RightFlipper.eosTorqueAngle = EOSA
			RightFlipper.eosTorque = EOST
			RightFlipper.rotateToStart
			playFieldSound "FlipDownR", 0, RightFlipper, 1
			If B2SOn Then DOF 102,DOFOff
			stopSound "FlipBuzzRA"
			stopSound "FlipBuzzRB"
			stopSound "FlipBuzzRC"
			stopSound "FlipBuzzRD"
		End If
   End If

    If keycode = 203 Then cLeft = 0' Left Arrow
 
    If keycode = 200 Then cUp = 0' Up Arrow
 
    If keycode = 208 Then cDown = 0' Down Arrow
 
    If keycode = 205 Then cRight = 0' Right Arrow	

	If keycode = 52 Then Zup = 0' Period									
End Sub

'************** Table Boot
Dim backGlassOn
Dim bootCount:bootCount = 0
Sub bootTable_Timer()
	bootCount = bootCount + 1
	If freePlay = 1 Then
		If B2SOn Then DOF 127, DOFOn
	End If
	If bootCount = 1 Then	
		If vrOption > 0 Then vrBGplane.image = "AG B2S VR_On"		   
		If B2SOn Then
			controller.B2SSetCredits credit
			controller.B2SSetGameOver 35,1
			controller.B2SSetTilt 33,1
			controller.B2SSetData 80, 1						 'turns the backglass image on
			controller.B2SSetPlayerUp 30,0
			If credit > 0 Then DOF 127, DOFOn		
			If freePlay = 1 Then DOF 127, DOFOn
		End If				 
		backGlassOn = 1
		me.enabled = False
	End If
	dim xx
	pfholes.image =  "pfholes_on"
	For each xx in filaments: xx.blenddisablelighting = 120: Next
	For each xx in bulbs: xx.blenddisablelighting = 12: Next
End Sub

'***********Replay Settings
Sub replaySettings
	replay(1) = 140000
	replay(2) = 156000
	replay(3) = 174000		   
End Sub

'***********Operator Menu
Dim operatorMenu
Sub operatorMenuTimer_Timer
    If optionMenu.visible = False Then playFieldSound "target", 0, SoundPointScoreMotor, .2
	options = 0
    operatorMenu = 1
	dynamicUpdatePostIt.enabled = 0
	updatePostIt
	options = 0
    optionsMenu.visible = True
	OptionsMenu.image = "AG Options"
    optionMenu.visible = True
	optionMenu.image = "FreeCoin" & freePlay
End Sub

'***********Start Game
Dim ballInPlay
Sub StartGame
	If state = False Then
		ballinPlay = 1
		If B2SOn Then 
			controller.B2SSetCredits credit
			controller.B2SSetBallinPlay 32, ballinplay
			controller.B2SSetPlayerup 30, 1
			controller.B2SSetData 91, 1
			controller.B2SSetGameOver 0
		End If
		For x = 1 to 4
			For y = 1 to 5
				extraBall(x,y) = 0
			Next
		Next
		dynamicUpdatePostIt.enabled = 0	
		giOn
		updatePostIt
		tilt = False
		state = True
		gameState
		players = 1
		cpReel.setValue(1)
		shutOffLights
		newGame
		If vrOption > 0 Then 
			vrBipSet
			vrCredit
			vrGameOver.visible = 0
			vrup.image = "up1"
			For Each xx in vrPlayer1Reels: xx.image = "VR_BallyScoreReel_on": Next
			vrCreditReel.image = "VR_BallyCreditReel_on"
		End If
		If showDT = True And vrOption = 0 Then
			ScoreReelOff1.visible = 0
			ScoreReel1.visible = 1
		End If
		semAttract.enabled = 0
		For each xx in Lights: xx.state = 0: Next
		L059.state = 1
		ocxPlayerMusic.controls.stop
		generateSongNumber(5)
		spinnerCount = 1
		XERBcycle
		If L057.state = 0 And L053.state = 0 Then 
			L057.state = 1
		End If
	Else If state = True and players < maxPlayers and ballinPlay = 1 Then
		players = players + 1
		If showDT = True and vrOption = 0 Then
			EVAL("ScoreReelOff" & players).visible = 0
			EVAL("ScoreReel" & players).visible = 1
		End If
		cpReel.setValue(players)
		creditReel.setvalue(credit)
		If vrOption > 0 Then 
			vrCredit
			For Each xx in EVAL("vrPlayer" & players & "Reels"): xx.image = "VR_BallyScoreReel_on": Next
		End If
			If B2SOn Then
				controller.B2SSetCredits credit
				controller.B2SSetData 90 + players, 1	
			End If
		shutOffLights
		End If
	End If 
End Sub

'***************New Game	
Dim endGame				
Sub newGame
	startUpSound.enabled = 0
	player = 1
    endGame = 0
	If B2SOn Then controller.B2SSetShootAgain 36,0
	gameState
	For x = 1 to 3
		EVAL("Bumper" & x).hashitevent = 1
	Next
	up1Reel.setValue(1)
	For x = 1 to 4
		reelDone(x) = 0
		playerDone(x) = 0
		dragRaceCount(x) = 0
		apCollect(x) = 0

		allPins(x) = 0
		cruiseCount(x) = 0
		bumperHits(x) = 0
		topLanes(x) = 0
		melCount(x) = 0
		dtCount(x) = 0
	Next
	For x = 80 to 84
		EVAL("L0" & x).state = 0
	Next
	lightPins
	resetTable
	lutValue = 0
	SetLUT
	resetReel.enabled = True
	updateMelCheck
End Sub

'**********Check if Game Should Continue
Dim relBall, rep(4)
Sub checkContinue
	If endGame = 1 Then
		fadeMusic.enabled = 1
		turnOff
		stopSound "FlipBuzzLA"
		stopSound "FlipBuzzLB"
		stopSound "FlipBuzzLC"
		stopSound "FlipBuzzLD"
		stopSound "FlipBuzzRA"
		stopSound "FlipBuzzRB"
		stopSound "FlipBuzzRC"
		stopSound "FlipBuzzRD"
		state = False
		bipReel.setValue(0)
		BIPText.text = " "
		For x = 1 to maxPlayers
			EVAL("up" & x & "Reel").setValue(0)
		Next
		gameState
		dynamicUpdatePostIt.enabled = 1
		cpReel.setValue(0)
		sortScores
		checkHighScores
		firstBallOut = 0		
		players = 0
		For x = 1 to 4
			rep(x) = 0
			repAwarded(x) = 0
		Next
		playOcxVo "vo_WMJGoodNight.mp3"
		saveHighScore
		If B2SOn Then 
			controller.B2SSetGameOver 35,1
			controller.B2SSetballinplay 32, 0
			controller.B2SSetPlayerUp 30, 0
			For x = 1 to 4
				controller.B2SSetData 90+x, 0
			Next
			If credit > 0 Then DOF 127, DOFOn
			If freePlay = 1 Then DOF 127, DOFOn
		End If
		If vrOption > 0 Then
			vrBipSet
			vrGameOver.visible = 1
			vrBip.image = "bip0"
			vrUp.image = "up0"
			vrCreditReel.image = "VR_BallyCreditReel_off"
			For Each xx in vrReels: xx.image = "VR_BallyScoreReel_off": next
		End If
		giOff
		semAttract.enabled = 1
	Else
		relBall = 1
		setLights
		scoreMotor5.enabled = 1
		bipReel.setValue(ballInPlay)
		BIPText.text = ballinPlay
		If vrOption > 0 Then 
			vrBipSet
			vrPlayerUP
		End If
	End If
End Sub

Sub resetTable
	DTRaise 1: DTRaise 2: DTRaise 3: DTRaise 4
	leftStar = 0
	rightStar = 0
	bumperCounter = 0
	spinnerHit = 0
	melLoop = 0
	melModeActive = 0
	bumperModeTimer.enabled = 0
	melTimer.enabled = 0
	Dim xx
	For each xx in betweenBallReset: xx.state = 0: Next
End Sub

'***************Drain and Release Ball
Dim drainActive, drainCount, drainBS
Sub drain_Hit()
	bip = bip - 1
	If bip = 1 Then drain.destroyBall
	If bip = 0 Then
		If bsOn = 0 or playerDone(player) = 1 Then
			sm1962Loop = 200
			psmLoop = 200
			clsmLoop = 200
			bsmLoop = 200
			msmLoop = 200
			dtsmLoop = 200
			phoneLoop = 200
			MelLoop = 200
			spinnerHit = 0
			stopSound "sfx_Payphone"
			scoreMotorLoop = 0
			repAwarded(Player) = 0
			scoreBonus	
			If ballInPlay +1 > Balls Then playerMusicTime.enabled = 0
			drainActive = 1	
			LeftSlingShot.collidable = 1
			RightSlingShot.collidable = 1
			shutOffLights
			OptionsMenu.visible = 0
			raceStart = 0  
			ballSaveOn = 0
		Else
			drainBS = 1
			releaseBall
		End If
	End If
End Sub

Sub releaseBall
	playFieldSound "FastKickIntoLaunchLane", 0, drain, 0.5
	If B2SOn Then DOF 125,DOFPulse 
	drain.kick 52, 30
	bipReel.setValue(ballInPlay)
	bipText.text = ballinPlay
	If B2SOn Then Controller.B2SSetBallinPlay 32, ballinPlay
	drainActive = 0
	scoreMotorDone = 0
	If vrOption > 0 Then vrBipSet
	bip = bip + 1
End Sub

Sub giOn
	For Each xx in GILights: xx.state = 1: Next
	Photo_Debbie.image =  "debbie_on"
	Photo_Curt.image = 	"kurt_on"
	Photo_Toad.image =  "toad_on"
	Photo_Joe.image =  "joe_on"
	Photo_Steve.image =  "steve_on"
	Photo_Laurie.image =  "laurie_on"
	Photo_Bob.image = "bob_on"
	Photo_John.image = "john_on"
	Photo_Carol.image = "carol_on"
	Photo_wolfMan.image = "wolf_on"
	LaneGuides.image = "laneguides_on"
	Walls_Metal.image = "wallsMetal2_on"
	Rubbers.image = "rubbers_on"
	lsling.image = "rubbers_on"
	rsling.image = "rubbers_on"
	lFlip.image = "flipperLeft_on"
	rFlip.image = "flipperRight_on"
	ROLI001.image = "rubbers_on"
	RORI001.image = "rubbers_on"
	SaucerCup.image ="saucer_on"
	Gilardis.image = "gilardis_on"
	Walls_WoodLow.image = "wallsWood_on"
	playfield_mesh2.image = "pf_on"
	MelsMain.image = "melsmain_on"
	MelsParking.image = "melspark_on"
	MelsSign.image = "melssignbase_on"
	GearShifter.image = "shifter_on"
	Skull.image = "skull_on"
	PhoneBooth.image = "phone_on"
	Payphone.image = "payphone_on"
	Posts_Orange.image = "postsOrange2_on"
	Posts_Teal.image = "postsTeal2_on"
	pfholes.image = "pfholes_on"
	plasticBottoms.image = "plastics_on"
	GIbulbs.image = "gi_on"
End Sub

Sub giOff
	For Each xx in GILights: xx.state = 0: Next
	Photo_Debbie.image =  "debbie_off"
	Photo_Curt.image = 	"kurt_off"
	Photo_Toad.image =  "toad_off"
	Photo_Joe.image =  "joe_off"
	Photo_Steve.image =  "steve_off"
	Photo_Laurie.image =  "laurie_off"
	Photo_Bob.image = "bob_off"
	Photo_John.image = "john_off"
	Photo_Carol.image = "carol_off"
	Photo_wolfMan.image = "wolf_off"
	LaneGuides.image = "laneguides_off"
	Walls_Metal.image = "wallsMetal2_off"
	Rubbers.image = "rubbers_off"
	lsling.image = "rubbers_off"
	rsling.image = "rubbers_off"
	lFlip.image = "flipperLeft_off"
	rFlip.image = "flipperRight_off"
	ROLI001.image = "rubbers_off"
	RORI001.image = "rubbers_off"
	SaucerCup.image ="saucer_off"
	Gilardis.image = "gilardis_off"
	Walls_WoodLow.image = "wallsWood_off"
	playfield_mesh2.image = "pf_off"
	MelsMain.image = "melsmain_off"
	MelsParking.image = "melspark_off"
	MelsSign.image = "melssignbase_off"
	GearShifter.image = "shifter_off"
	Skull.image = "skull_off"
	PhoneBooth.image = "phone_off"
	Payphone.image = "payphone_off"
	Posts_Orange.image = "postsOrange2_off"
	Posts_Teal.image = "postsTeal2_off"
	pfholes.image = "pfholes_off"
	plasticBottoms.image = "plastics_off"
	GIbulbs.image = "gi_off"
	Hcap.image = "bumperH_off"
	Ocap.image = "bumperO_off"
	Pcap.image = "bumperP_off"
End Sub

'*******************Check for Bonus Score
Dim bonusFlag
Sub scoreBonus
	flag10 = 0: flag100 = 0: flag1000 = 0: flag10k = 0
	If L068.state = 1 Then
		For Each xx in tailLights: xx.state = 0: Next
		leftStar = 0: rightStar = 0
		releaseBall
	Else
		fadeMusic.Enabled = 1
		bonusFlag = 1
		If playerDone(player) = 0 Then
			ScoreMotor.enabled = 1
		Else
			advancePlayers
		End If
		If laneState = 1 Then resetTopLanes
		saveLights
	End If
End Sub

'**************Store Player Light States
Sub saveLights
	playerLights(player) = Array(L001.state, L002.state, L003.state, L004.state, L010.state, L011.state, L012.state, L013.state, L110.state, L112.state, L114.state, L116.state, L118.state, L120.state)
	'staticLights = Array("L001","L002","L003","L004","L010","L011","L012","L013","L110","L112","L114","L116","L118","L120","L125","L126","L127","L128")
End Sub

Sub setLights
	For i = 0 To 13
		EVAL(staticLights(i)).state = playerLights(player)(i)
	Next
End Sub

'**************Advance Players
Sub advancePlayers
	 If players = 1 or player = players Then 
		player = 1
		EVAL("up" & players & "Reel").setValue(0)
		up1Reel.setValue(1)
	 Else
		player = player + 1
		EVAL ("up" & (player - 1) & "Reel").setValue(0)
		EVAL ("up" & player & "Reel").setValue(1)
	End If
	If B2SOn Then controller.B2SSetPlayerup 30, player
	If vrOption > 0 Then vrPlayerUp
	updateMelCheck
	nextBall
End Sub

'*********************Next Ball
Sub nextBall
	bottomgate.rotateToEnd
	bgState = 1
    If tilt = True Then
		For x = 1 to 3
			EVAL("bumper" & x).hashitevent = 1
		Next
		tilt = False
		tiltReel.SetValue(0)
		If vrOption > 0 Then vrTilt.visible = 0
		If B2SOn Then
			Controller.B2SSetTilt 33,0
			Controller.B2SSetData 1, 1
		End If
    End If
	bonus = 0												' reset the bonus counter for bonus multipliers
	If L122.state = 1Then									' this turns off all mode lights if a wolfman challenge was in play and not completed
		For Each xx in modeLights: xx.state = 0: Next
	End If
	OH = 0
	If player = 1 Then 
		ballinPlay = ballinPlay + 1
		If Balls = 3 Then
			lutValue = lutValue + 2
		Else
			lutValue = lutValue + 1
		End If
	End If

	If ballinPlay > balls Then
		endGame = 1
		checkContinue
	  Else
		setLUT
		generateSongNumber(6)
		If state = True Then
			checkContinue
		End If
	End If
	resetTable
	If playerDone(player) = 1 Then advancePlayers
End Sub

'************Game State Check
Sub gameState
	If state = False Then
		gameOverReel.SetValue(1)
		If showDt = True and vrOption = 0 Then 
			For i = 1 to 4
				EVAL("ScoreReelOff" & i).visible = 1
				EVAL("ScoreReel" & i).visible = 0
			Next
		End If
		If vrOption > 0 Then vrGameOver.visible = 1
		If B2SOn then controller.B2SSetGameOver 35,1
		stopSound "FlipBuzzLA"
		stopSound "FlipBuzzLB"
		stopSound "FlipBuzzLC"
		stopSound "FlipBuzzLD"
		stopSound "FlipBuzzRA"
		stopSound "FlipBuzzRB"
		stopSound "FlipBuzzRC"
		stopSound "FlipBuzzRD"
	Else 
		gameOverReel.SetValue(0)
		If vrOption > 0 Then vrGameOver.visible = 0
		tiltReel.SetValue(0)
		If vrOption > 0 Then vrTilt.visible = 0
		If B2SOn then 
			controller.B2SSetTilt 33,0
			controller.B2SSetGameOver 35,0
		End If
	End If
End Sub	

'*************Ball in Launch Lane on Plunger Tip
Dim ballREnabled, ballSaveOn
Sub ballHome_hit
	If bsOn Then
		drainCount = drainCount + 1
		If drainCount > 1 Then drainCount = 0
	End If 
		dragRaceTrigger.enabled = 1
	If bsOn = 0 And raceStart = 0 And ballSaveOn = 0 Then
		ballSave.enabled = 1
	End If
	If drainBS = 1 Then 
		If drainCount = 1 Then
			playOcxVo "vo_FunAsUsual.mp3"
		Else
			playOcxVo "vo_DontBeStupid.mp3"
		End If
		drainBS = 0
	End If
	ballREnabled = 1
	If B2SOn then DOF 160, DOFOn
	Set controlBall = ActiveBall   
    contballinplay = true
	bonusFlag = 0
End Sub


Sub ballHome_unhit
	If B2SOn Then DOF 160, DOFOff

End Sub

'************Check if Ball Out of Launch Lane
Sub ballsInPlay_hit
	If ballREnabled = 1 Then
		If B2SOn Then controller.B2SSetShootAgain 36,0
		L068.state = 0
		ballREnabled = 0
	End If
	firstBallOut = 1						
End Sub

'******* for ball control script
Sub endControl_Hit()      
    contballinplay = False	 
End Sub


Sub knock
	If B2SOn = True Then DOF 128, DOFPulse Else playSound "knocker"
End Sub


dim tempLight, tempMel
Sub rotateLeft
	If sm1962On = 0 Then
		tempLight = L001.state
		L001.state = L002.state
		L002.state = L003.state
		L003.state = L004.state
		L004.state = tempLight
	Else
		tempLight = tlArrayTemp(1)
		tlArrayTemp(1) = tlArrayTemp(2)
		tlArrayTemp(2) = tlArrayTemp(3)
		tlArrayTemp(3) = tlArrayTemp(4)
		tlArrayTemp(4) = tempLight
	End If
	tempMel = L010.state
	L010.state = L011.state
	L011.state = l012.state
	L012.state = L013.state
	L013.state = tempMel
End Sub

Sub rotateRight
	If sm1962On = 0 Then
		tempLight = L004.state
		L004.state = L003.state
		L003.state = L002.state
		L002.state = L001.state
		L001.state = tempLight
	Else
		tempLight = tlArrayTemp(4)
		tlArrayTemp(4) = tlArrayTemp(3)
		tlArrayTemp(3) = tlArrayTemp(2)
		tlArrayTemp(2) = tlArrayTemp(1)
		tlArrayTemp(1) = tempLight
	End If
	tempMel = L013.state
	L013.state = L012.state
	L012.state = l011.state
	L011.state = L010.state
	L010.state = tempMel
End Sub

'************** Shut Off Lights
Sub shutOffLights
End Sub


'************** Bumpers						   
Sub bumpers_hit(Index)
	If ballControlBlock = True Then Exit Sub
	If tilt = False Then
		Select Case (Index)
			case 0: playFieldSound "PopBump", 0, bumper1, 1	
					bumperRing0.enabled = 1
					If B2SOn Then DOF 105, DOFPulse	
					bumpHit = 1
					bumperScore (Bumper1Light.state)
			case 1: playFieldSound "PopBump", 0, bumper2, 1	
					bumperRing1.enabled = 1										   
					If B2SOn Then DOF 106, DOFPulse	
					bumpHit = 2
					bumperScore (Bumper2Light.state)
			case 2:	playFieldSound "PopBump", 0, bumper3, 1		  
					bumperRing2.enabled = 1 
					If B2SOn Then DOF 107, DOFPulse	
					bumpHit = 3
					bumperScore (Bumper3Light.state)
		End Select
	End If
End Sub	

Dim xx
Sub lightBumpers
	For each xx in bumperLights: xx.state = 1: Next
	Hcap.image = "bumperH_on"
	Ocap.image = "bumperO_on"
	Pcap.image = "bumperP_on"
End Sub

Sub unLightBumpers
	For each xx in bumperLights: xx.state = 0: Next
	Hcap.image = "bumperH_off"
	Ocap.image = "bumperO_off"
	Pcap.image = "bumperP_off"
End Sub

Dim bumperRingPos0
bumperRingPos0 = 0
Sub bumperRing0_Timer
	bumperRingPos0 = bumperRingPos0 + 1
	Select Case bumperRingPos0
		Case 1 : ring0.z = -20 
		Case 2 : ring0.z = -30
		Case 3 : ring0.z = -32
		Case 4 : ring0.z = -28
		Case 5 : ring0.z = -16
		Case 6 : ring0.z = -8
'		Case 7 : ring0.z =  0
					 bumperRingPos0 = 0
					 bumperRing0.enabled = 0

	End Select
End Sub

Dim bumperRingPos1
Sub bumperRing1_Timer
	bumperRingPos1 = bumperRingPos1 + 1
	Select Case bumperRingPos1
		Case 1 : ring1.z = -20 
		Case 2 : ring1.z = -30
		Case 3 : ring1.z = -32
		Case 4 : ring1.z = -28
		Case 5 : ring1.z = -16
		Case 6 : ring1.z =  -8
'		Case 7 : ring1.z =   0
					 bumperRingPos1 = 0
					 bumperRing1.enabled = 0
	End Select
End Sub

Dim bumperRingPos2
Sub bumperRing2_Timer
	bumperRingPos2 = bumperRingPos2 + 1
	Select Case bumperRingPos2
		Case 1 : ring2.z = -20 
		Case 2 : ring2.z = -30
		Case 3 : ring2.z = -32
		Case 4 : ring2.z = -28
		Case 5 : ring2.z = -16
		Case 6 : ring2.z =  -8
'		Case 7 : ring2.z =   0
					 bumperRingPos2 = 0
					 bumperRing2.enabled = 0
	End Select
End Sub


'************** Slings
Dim rStep, lStep, tstep, leftLight, rightLight

Sub leftSlingShot_Slingshot
	If ballControlBlock = True or tilt = True Then Exit Sub
	playfieldSound "SlingShot", 0, SoundPoint12, 1
	If B2SOn Then DOF 103, DOFPulse
    lSling2.visible = 1
    leftSling.transZ = -10
    lStep = 0
    leftSlingShot.timerEnabled = 1
	If tilt = False Then addScore 10
	If L057.state = 1 Then
		L057.state = 0
	Else
		L057.state = 1
	End If
End Sub


Sub leftSlingShot_Timer
    Select Case lStep
        Case 3: lSling2.visible = 0: lSling1.visible = 1
				leftSling.transZ = -10
        Case 4: lSling1.visible = 0
				leftSling.transZ = 0
				leftSlingShot.timerEnabled = 0
    End Select
    lStep = lStep + 1	  
End Sub

Sub rightSlingShot_Slingshot
	If ballControlBlock = True or tilt = True Then Exit Sub
	playfieldSound "SlingShot", 0, soundPoint13, 1
	If B2SOn Then DOF 104, DOFPulse
    rSling2.visible = 1
    rightSling.transZ = -10
    rStep = 0
    rightSlingShot.timerEnabled = 1
	If tilt = False Then addScore 10
		If L053.state = 1 Then
		L053.state = 0
	Else
		L053.state = 1
	End If
End Sub

Sub rightSlingShot_Timer
    Select Case rStep
        Case 3: rSling2.visible = 0: rSling1.visible = 1: 
				rightSling.transZ = -10
        Case 4: rSling1.visible = 0
				rightSling.transZ = 0
				rightSlingShot.timerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'*************** Ball Save 
Dim bsCount, bsOn
Sub ballSave_timer
	ballSaveOn = 1
	bsOn = 1
	If tilt = True Then bsCount = 22
	bsCount = bsCount + 1
	If bsCount Mod 2 = 0 Then
		L067.state = 1
	Else
		L067.state = 0
	End If
	If bsCount > 22 Then
		bsCount = 0
		bsOn = 0
		ballSave.enabled = 0
	End If
End Sub

Sub ballLaunched_Hit
	ballSaveOn = 0
End Sub


'*************** Triggers 
Dim wireNumber, topLanes(4), cruiseCount(4), laneState, bgState, lOLCount
Sub triggers_hit (Index)
	If ballControlBlock = True Then Exit Sub
	If tilt = False Then 
		If index < 7 or index = 9 Then
			wireNumber = Index
		End If
		Select Case Index
			Case 0: topLanesHit(1)
			Case 1: topLanesHit(2)
			Case 2:	topLanesHit(3)
			Case 3: topLanesHit(4)
			Case 4:	cruiseLanes(1)
			Case 5: cruiseLanes(2)
			Case 6: triggerScore L059.state, 500, 100 'RORO
					wireAnimation.enabled = 1
					rlCount = rlCount + 1
					If rlCount > 3 Then rlCount = 1
					If L058.state = 1 Then
						playOcxVo "vo_chicken" & rlCount & ".mp3"
					Else
						playOcxVo "vo_college" & rlCount & ".mp3"
					End If
			Case 7:	triggerScore L057.state, 300, 30
					If L057.state = 1 Then
						rightStar = rightStar + 1 
						starHits(1)  'RORI
					End If
			Case 8: triggerScore L053.state, 300, 30
					If L053.state = 1 Then
						leftStar = leftStar + 1
						starHits(0) 'ROLI
					End If
			Case 9: triggerScore L063.state, 0, 500 'RODL
					wireAnimation.enabled = 1
					If bgState = 0 Then	playOcxVo "vo_Homework.mp3"
					If bgState = 1 and L063.state <> 2 Then 
						Select Case lOLCount
							Case 0: playOcxVo "vo_leftout1.mp3"
							Case 1: playOcxVo "vo_leftout2.mp3"
							Case 2: playOcxVo "vo_leftout3.mp3"
						End Select
						lOLCount = lOLCount + 1
						If lOlCount > 2 Then lOLCount = 0
					End If
					If L063.state = 2 Then dragRace
			Case 10: oldHarper
		End Select

		If B2SOn Then DOF 108 + Index, DOFPulse
	End If
End Sub

Dim lightSelect, rlCount
lightSelect = 59
Sub rightOutlaneLights
	Eval("L0" & lightSelect).state = 0
	lightSelect = lightSelect + 1
	If lightSelect > 59 then lightSelect = 58
	Eval ("L0" & lightSelect).state = 1
End Sub

Dim leftStar, rightStar, extraBall(5,6)
Sub starHits(side)
	playSound "sfx_CarHorn", 0, musicVol
	If side = 0 Then
		If leftStar > 3 Then leftStar = 3
		EVAL("L0" & (leftStar+49)).state = 1
	Else
		If rightStar > 3 Then rightStar = 3
		EVAL("L0" & (rightStar+53)).state = 1
	End If
	If leftStar + rightStar > 5 and extraBall(player, ballInPlay) < 1 Then 
		L068.state = 1
		extraBall(player,ballInPlay) = 1
	End If
End Sub

Dim OH
Sub oldHarper
	Select Case OH
		Case 0: L020.state = 1: triggerScore L020.state, 100, 100: playOcxVo "vo_harper1.mp3"
		Case 1: L021.state = 1: triggerScore L021.state, 200, 200: playOcxVo "vo_harper2.mp3" 
		Case 2: L022.state = 1: triggerScore L022.state, 300, 300: playOcxVo "vo_harper3.mp3"
		Case 3: L023.state = 1: triggerScore L023.state, 1000, 1000: playOcxVo "vo_harper4.mp3"
	End Select
	OH = OH + 1
	If OH > 3 Then OH = 3
End Sub

Sub triggerScore(lightState, scoreLit, scoreUnlit)
	If lightState = 1 Then  
		addScore scoreLit
	Else
		addScore scoreUnlit
	End If	
End Sub

'************  StandupTargets
Dim phoneAnswer
Sub StandUpTargets_Hit(idx)
	If ballControlBlock = True Then Exit Sub
	PlayFieldSoundAB "target", 0, 1
	Select Case idx
		Case 0: checkMel(10) 
		Case 1: checkMel(11)
		Case 2: checkMel(12)
		Case 3: checkMel(13)
		Case 4: playSound "sfx_Pinball1", 0, musicVol: checkAllPins(1)
		Case 5: playSound "sfx_Pinball2", 0, musicVol: checkAllPins(2)
		Case 6: playSound "sfx_Pinball3", 0, musicVol: checkAllPins(3)
		Case 7: playSound "sfx_Pinball4", 0, musicVol: checkAllPins(4)
		Case 8: playSound "sfx_Pinball5", 0, musicVol: checkAllPins(5)
		Case 9: spinnerCount = spinnerCount - 1
				If spinnerCount < 1 Then spinnerCount = 4
				XERBcycle
				addScore 10
		Case 10:spinnerCount = spinnerCount + 1
				If spinnerCount > 4 Then spinnerCount = 1
				XERBcycle
				addScore 10
		Case 11:lightBumpers: BumperModeTimer.enabled = 1
				rightOutlaneLights
				If phoneOn = 1 Then
					addScore 2000
					phoneAnswer = INT(3 * RND(1) ) +1
					Select Case phoneAnswer
						Case 1: playOcxVo "vo_CallExcitingThing.mp3"
						Case 2: playOcxVo "vo_CallHelloHello.mp3"
						Case 3: playOcxVo "vo_CallKnowMe.mp3"
					End Select
					phone.Enabled = 0
					phoneLoop = 0
					phoneOn = 0
					StopSound "sfx_Payphone"
				Else
					addScore 100
				End If
	End Select
End Sub

'**************************************** Pinball StandUp Target Mode ******************************************
Dim pCount
Sub lightPins
	For Each xx in pins
		pCount = pCount + 1
		If EVAL("L0" & (79+pCount)).state = 1 Then
			xx.image = "P" & (79+pCount) & "_on"
		Else
			xx.image = "P" & (79+pCount) & "_off"
		End If
	Next
	pCount = 0
End Sub

Dim allPins(4), apCollect(5)
Sub checkAllPins(pinNumber)
	If wmcOn = 1 Then				' check wolfman callenge first
		pinScore(pinNumber)
		wmcArray(12+pinNumber) = 1
	ElseIf psmOn = 1 then			' check super mode second
		pinArrayTemp(pinNumber) = 1
		If pinArrayTemp(1) = 1 and pinArrayTemp(2) = 1 and pinArrayTemp(3) = 1 and pinArrayTemp(4) = 1 and pinArrayTemp(5) = 1 Then 
			addScore 20000
			playOcxVo "vo_WMJHowl.mp3"
			psmLoop = 100
		Else
			pinScore(pinNumber)
		End If
	Else							' check regualr mode last
		EVAL("L0" & (pinNumber+79)).state = 1
		If L080.state = 1 and L081.state = 1 and L082.state = 1 and L083.state = 1 and L084.state = 1 Then
			allPins(player)= allPins(player)+ 1
			playOcxVo "vo_Pin" & (allPins(player) Mod 5)  & ".mp3"

			updateMelCheck
			If allPins(player)= 3 and apCollect(player) = 0 Then 
				L110.state = 1
				addScore(20000)
				apCollect(player) = 1
				checkAllModes
			End If
			L080.state = 0: L081.state = 0: L082.state = 0: L083.state = 0: L084.state = 0
		Else
			pinScore(pinNumber)
		End If
	End If 
	lightPins
End Sub

Sub pinScore(pinNumber)
	If EVAL("L0" & (pinNumber +79)).state = 1 Then
		addScore 100
	Else
		addScore 1000
	End If
End Sub

'**************************************** Cruise Lane Rollover Mode ******************************************
Sub cruiseLanes(cLaneHit)
	wireAnimation.enabled = 1
	If wmcOn = 1 Then					' check wolfman callenge first
		If cLaneHit = 1 Then
			triggerScore L040.state, 300, 30
		Else
			triggerScore L041.state, 300, 30
		End If
		wmcArray(cLaneHit+17) = 1
	ElseIf clsmOn = 1 Then				' check super mode second
		clsmscore = clsmScore + 1
		If clsmscore = 3 Then
			clsmLoop = 80
			clsmScore = 0
			addScore 20000
			playOcxVo "vo_WMJHowl.mp3"
		Else
			addScore 500
		End If
	Else								' check regualr mode last
		If melModeActive = 1 Then
			triggerScore 1, 300, 30
			cruiseCount(player) = cruiseCount(player) + 1
			updateMelCheck
		Else
			triggerScore 0, 300, 30
		End If
		If cruiseCount(player)> 3 Then 
			L112.state = 1
			checkAllModes
		End If
	End If
End Sub

'**************************************** Bumper Hit Mode ******************************************
Dim bumperHits(4), bumpHit
Sub bumperScore(bLightState)
	If wmcOn = 1 Then					' check wolfman callenge first
		EVAL("Bumper" & bumpHit & "Light").state = 1
		wmcArray(19+bumpHit) = 1
		addScore 100
	ElseIf bsmOn = 1 Then				' check super mode second
		bsmScore = bsmScore + 1
		If bsmScore = 10 Then
			bsmScore = 0
			bsmLoop = 80
			addScore 20000
			playOcxVo "vo_WMJHowl.mp3"
		Else
			addScore 100
		End If
	Else								' check regualr mode last
		If bLightState = 1 Then
			addScore 100
			bumperHits(player) = bumperHits(player) + 1
			updateMelCheck
			If bumperHits(player) = 10 Then 
				L114.state = 1
				checkAllModes
			End If
		Else
			addScore 10
		End If
	End If
End Sub

Dim bumperCounter
Sub bumperModeTimer_Timer
	bumperCounter = bumperCounter + 1
	If bumperCounter > 100 Then
		If bumperCounter Mod 2 = 0 Then 
			lightBumpers
		Else
			unLightBumpers
		End If
	End If
	If bumperCounter > 120 Then
		bumperCounter = 0
		unLightBumpers
		bumperModeTimer.enabled = 0
	End If

End Sub

'**************************************** Top Lane 1962 Rollover Mode ******************************************
Sub topLanesHit(laneHit)
	wireAnimation.enabled  = 1
	If wmcOn Then							' check wolfman callenge first
		EVAL("L00" & laneHit).state = 1
		wmcArray(laneHit) = 1
		addScore 100
	ElseIf sm1962On = 1 Then				' check super mode second
		EVAL("L00" & laneHit).state = 1
		tlArrayTemp(laneHit) = 1
		If tlArrayTemp(1) = 1 and tlArrayTemp(2) = 1 and tlArrayTemp(3) = 1 and tlArrayTemp(4) = 1 Then 
			For Each xx in topLaneLights: xx.state = 0: Next:
			For x = 1 to 4: tlArrayTemp(x) = 0: Next
			sm1962Score = 0
			sm1962Loop = 200
			addScore 20000
			playOcxVo "vo_WMJHowl.mp3"
		Else
			triggerScore EVAL("L00" & laneHit).state, 100, 500
		End If
	Else									' check regular mode last
		triggerScore EVAL("L00" & laneHit).state, 100, 500
		If bip = 1 Then EVAL("L00" & laneHit).state = 1
		If L001.state = 1 and L002.state = 1 and L003.state = 1 and L004.state = 1 and L063.state = 0 Then 
			playOcxVo "vo_ProveIt.mp3"
			bottomgate.rotateToEnd
			bgState = 1
			L063.state = 2
			laneState = 1
			topLanes(player) = topLanes(player) + 1
			updateMelCheck
			If topLanes(player) > 1 Then 
				L116.state = 1
				checkAllModes
			End If
		End If
	End If
End Sub

'**************************************** Mel Diner StandUp Target Mode ******************************************
Dim melCount(4), melModeActive
Sub checkMel(suHit)
	If wmcOn = 1 Then					' check wolfman callenge first
		EVAL("L0" & suHit).state = 1
		addScore 1000
		wmcArray(suHit-5) = 1
	ElseIf msmOn = 1 Then				' check super mode second
		msmArrayTemp(suHit-9) = 1
		If msmArrayTemp(1) = 1 and msmArrayTemp(2) = 1 and msmArrayTemp(3) = 1 and msmArrayTemp(4) = 1 Then 
			For x = 1 to 4: msmArrayTemp(x) = 0 : Next
			For Each xx in msmLights: xx.state = 0: Next
			addScore 20000
			playOcxVo "vo_WMJHowl.mp3"
			msmLoop = 200
			msmScore = 0
		Else
			addScore 500
		End If
	Else								' check regualr mode last
		EVAL("L0" & suHit).state = 1
		If L010.state = 1 and L011.state = 1 and L012.state = 1 and L013.state = 1 Then 
			addScore 1000
			L010.state = 0: L011.state = 0: L012.state = 0: L013.state = 0
			melCount(player) = melCount(player) + 1
			updateMelCheck
			playOcxVo "vo_BurgerCity.mp3"
			If melCount(player) > 2 Then 
				L118.state = 1
				checkAllModes
			End If
			melTimer.enabled = 1
		Else
			addScore 500
		End If
End If
End Sub

Dim MelLoop
Sub melTimer_Timer
		melModeActive = 1
		MelLoop = MelLoop + 1
		If MelLoop Mod 2 = 0 Then 
			L040.state = 0
			L041.state = 0
		Else
			L040.state = 1
			L041.state = 1
		End If
		If MelLoop > 45 Then
			MelLoop = 0
			melModeActive = 0
			L040.state = 0
			L041.state = 0
			melTimer.enabled = 0
		End If
End Sub

'**************************************** Drop Target Mode ******************************************
Dim dtsmCount
Sub DropTargets_Hit(idx)
	PlayFieldSoundAB "dropTargetDropped", 0, 1
	dtModeHit(idx)
	Select Case idx
		Case 0:	L014.state = 1
		Case 1:	L015.state = 1
		Case 2:	L016.state = 1
		Case 3: L017.state = 1
	End Select
	addScore 100
End Sub

Sub dtModeHit(dHit)
	If wmcOn = 1 Then							' check wolfman callenge first
		addScore 50
		wmcArray(dHit+9) = 1
	ElseIf dtsmOn = 1 Then						' check super mode second
		EVAL("L0" & (14 + dHit)).state = 1
		addScore 50
		If L014.state = 1 and L015.state = 1 and L016.state = 1 and L017.state = 1 Then
			dtsmCount = dtsmCount + 1
			resetDropsTimer.enabled = 1
			If dtsmCount(player) = 2 Then
				dtsmLoop = 200
				addScore 20000
				playOcxVo "vo_WMJHowl.mp3"
			Else
				addScore 
			End If
		End If
	Else										' check regualr mode last
		dtModeArray(dHit+1) = 1
		If dtModeArray(1) = 1 and dtModeArray(2) = 1 and dtModeArray(3) = 1 and dtModeArray(4) = 1 Then
			bonusMultiplier
			dtCount(player) = dtCount(player) + 1
			updateMelCheck
			For x = 1 to 4: DTModeArray(x) = 0: Next
			resetDropsTimer.enabled = 1
			If dtCount(player) = 3 Then
				L120.state = 1
				checkAllModes
			Else
				addScore 100
			End If
		End If
	End If
End Sub

Sub resetDropsTimer_Timer
	Dim xx
	For each xx in dropTargetLights: xx.state = 0: Next
	DTRaise 1: DTRaise 2: DTRaise 3: DTRaise 4
	resetDropsTimer.enabled = 0
	playFieldSound "dropTargetReset", 0, drop003 , 1
	playSound "sfx_EngineRev", 0, musicVol
End Sub

'**************************************** Drag Race Mode ******************************************
Sub dragTrigger_hit
	If L063.state = 0 Then noDrag.enabled = 1						' no active drag race so kick out the ball
End Sub

Dim ndCount
Sub noDrag_timer																' if there is no active drag race then kick the ball out
	dragPlunger.Pullback
	If ndCount = 1 Then
		dragPlunger.Fire
		playFieldSound "SaucerKick", 0, dragPlunger, 0.6
		playSound "sfx_PeelOut2", 0, musicVol
		ndCount = 0
		If leftOutLane = 0 Then bottomgate.rotateToStart
		bgState = 0
		noDrag.enabled = 0
	End If
	ndCount = ndCount + 1
End Sub

Dim dragRaceCount(4)
Sub dragRace
	bip = bip + 1
	dragRaceCount(player) = dragRaceCount(player) + 1
	fadeMusic.enabled = 1
	bonusFlag = 1
	playSound "sfx_greenDrag2", 0, musicVol
	drain.CreateSizedBallWithMass Ballsize/2, BallMass
	If B2SOn Then DOF 110, DOFPulse 
	raceStart = 1
	drain.kick 52, 30
	playFieldSound "FastKickIntoLaunchLane", 0, drain, 0.5
	raceGo = 0
	L063.state = 2
	L064.state = 0
	L065.state = 0
	L066.state = 0
	resetTopLanes
	dragPlunger.Pullback
	dragRandom = INT(2 * RND(1) )
	flashLight.enabled = 1
End Sub

Sub resetTopLanes
	laneState = 0
	L001.state = 0
	L002.state = 0
	L003.state = 0
	L004.state = 0
End Sub

Dim flCount, dragRandom
Dim raceStart, raceGo  'raceStart indicates that a race has started, raceGo indicates that the flashlight timer hit the launch time
Sub flashLight_Timer											' signal the start of the drag race by flashing the "flash light" between the flippers three times
	flCount = flCount + 1
	If (flCount + 10) Mod 20 = 0 and flCount >49 and flCount < 95 Then
		L067.state = 1
	ElseIf flCount Mod 10 = 0 Then
		L067.state = 0
	End If
	If flCount = 10 Then 
		bottomgate.rotateToStart
		bgState = 1
	End If
	If flCount = 90 Then raceGo = 1
	If flCount = 90 + dragRandom Then
		dragPlunger.Fire
		L063.state = 0
		If B2SOn Then DOF 130, DOFOn
	End If
	If flCount = 40 or flCount = 70 or flCount = 90 and B2SOn = true Then DOF 129, DOFon
	If flCount = 42 or flCount = 72 or flCount = 92 and B2SOn = true Then 
		DOF 129, DOFoff
		DOF 130, DOFoff
	End If
	If flCount = 100 Then
		flashLight.enabled = 0
		flCount = 0
	End If
End Sub

Dim  racesWon, racesLost
Sub laneTrigger_hit(Index)										' determine who won the drag race based upon which lane's win trigger is hit first while a race is started
	If raceStart = 1 Then
		If index = 0  Then 
			L066.state = 1											' win
			addScore 5000
			racesWon = racesWon + 1
			If racesWon > 8 then racesWon = 8
			dragBonus
			stopSound "sfx_greenDrag2"
			If dragRaceCount(player) Mod 2 = 0 Then
				newSong = True
				playOcxVo ("vo_win" & racesWon & ".mp3")
				playOcxVo "vo_WMJHowl.mp3"
			Else
				newSong = True
				playOcxVo ("vo_win" & racesWon & ".mp3")
			End If
		Else
			L064.state = 1											' Loss
			racesLost = racesLost + 1
			If racesLost > 4 Then racesLost = 1
			stopSound "sfx_greenDrag2"
			newSong = True
			playOcxVo ("vo_loss" & racesLost & ".mp3")
		End If
	End If

	If dragRaceCount(player) Mod 2 = 0  and dragRaceCount(player) > 0 and raceStart = 1 Then   'get a super mode every two drag races
		chooseSuperMode
	End If
	dragResult.enabled = 1
	raceStart = 0
End Sub

Dim carLights
Sub dragBonus															' pay out bonus tree lights for winning a drag race
	EVAL("L0" & carLights+70).state = 1
	carLights = carLights + 1
	tb1.text = "CL " & carLights
	If carLights > 7 Then carLights = 7
End Sub

Sub dragResult_timer													' shut off the race result lights
	For Each xx in raceLights
		xx.state = 0
	Next
	dragResult.enabled = 0
End Sub

Dim raceBegin
Sub dragRaceTrigger_hit		 									' if the race is on but has not started and the player launches his ball then call a false start
	If raceGo = 0 and raceStart = 1 Then 
		stopSound "sfx_greenDrag2"
		playOcxVo "vo_BigWeenie.mp3"
		generateSongNumber(7)
		L065.state = 1 
		raceStart = 0
	End If
	dragRaceTrigger.enabled = 0
End Sub

'**************************************** Wolfman BOSS Mode ******************************************
Dim allModes, wmcOn
Sub checkAllModes
	If L110.state = 1 and L112.state = 1 and L114.state = 1 and L116.state = 1 and L118.state = 1 and L120.state =1 Then 
		addScore 20000
		If wmcOn = 0 Then 
			fadeMusic.Enabled = 1
			playOcxVo "vo_wmjChallenge.mp3"
			wolfManChallenge.Enabled = 1
		End If
	End If
End Sub

Dim wmcArray(23), wmcCount, wolfWin, playerDone(4)
Sub wolfManChallenge_Timer										' this is the super end of game mode where all shots must be made to collect
	L122.state = 1
	wmcOn = 1
	wmcCount = wmcCount + 1
	If wmcCount = 1 Then 
		flicker.Enabled = 1
		resetDropsTimer.enabled = 1
	End If
	If wmcCount = 284 Then generateSongNumber(8)
	If wmcCount = 6 Then
		flicker.enabled = 0
		flickerCount = 0
		giOn
	End If
	If wmcCount Mod 2 = 0 Then								' this section turns all of the lights in the collection on and off if not collected
		For each xx in wolfManLights: xx.state = 1: Next	' turn on
		lightPins
	Else													' turn off
		For x = 1 to 22
			If wmcArray(x) = 0 Then
				Select Case x
					Case 1:  L001.state = 0
					Case 2:  L002.state = 0
					Case 3:  L003.state = 0
					Case 4:  L004.state = 0
					Case 5:  L010.state = 0
					Case 6:  L011.state = 0
					Case 7:  L012.state = 0
					Case 8:  L013.state = 0
					Case 9:  L014.state = 0
					Case 10: L015.state = 0
					Case 11: L016.state = 0
					Case 12: L017.state = 0
					Case 13: L080.state = 0
					Case 14: L081.state = 0
					Case 15: L082.state = 0
					Case 16: L083.state = 0
					Case 17: L084.state = 0
					Case 18: L040.state = 0
					Case 19: L041.state = 0
					Case 20: Bumper1Light.state = 0
					Case 21: Bumper2Light.state = 0
					Case 22: Bumper3Light.state = 0
				End Select
			End If
		Next
		lightPins
	End If
	wolfWin = 1															' set this to 1 and if any shots have not been collected then the next section will set to 0 
	For x = 1 to 22
		If wmcArray(x) = 0 Then wolfWin = 0
	Next
	If wolfWin = 1 Then												' the challenge has been won!
		score(Player) = score(player) + 50000			' this code is here in case the shot that wins the challenge is a multiple score, if that were the case then the scoremotor would block the 50k
		sReel(Player).addValue(50000)
		EVAL("ScoreReelOff" & player).addValue(50000)
		If vrOption > 0 Then vrScore(player)
		If B2SOn Then Controller.B2SSetScorePlayer Player, Score(player)
		For ReplayX = Rep(Player) +1 to 3
			If Score(Player) => Replay(ReplayX) Then
				increaseCredits
				Rep(Player) = Rep(Player) + 1
				knock 
			End If
		Next
		fadeMusic.enabled = 1
		OptionsMenu.visible = 1
		OptionsMenu.image = "ChallengeComplete"
		For x = 1 to 22: wmcArray(x) = 0: Next
		L122.state = 0
		For Each xx in wolfManLights: xx.state = 0: Next
		For Each xx in modeLights: xx.state = 0: Next
		wmcCount = 0
		wmcOn = 0
		playOcxVo "vo_stillapunk.mp3"
		L063.state = 0
		turnOff
		playerDone(player) = 1
		wolfManChallenge.enabled = 0
	End If
End Sub

Dim flickerCount, flickerRnd
Sub flicker_timer
	flickerCount = flickerCount + 1
	If flickerCount = 1 Then flickerRnd = 2
	If flickerCount mod flickerRnd= 0 Then
		flickerRnd = INT(3 * RND(1) ) +1
		giOff
	Else
		giOn
	End If
End Sub

'*********** Bonus Lights
dim bonus
Sub bonusMultiplier																' set the bonus lights as a multiplier for the bonus tree
	bonus = bonus + 1
	If bonus > 3 Then bonus = 3
	Select Case bonus
		Case 1: L060.state = 1
		Case 2: L060.state = 0: L061.state = 1
		Case 3: L061.state = 0: L062.state = 1
	End  Select
End Sub

'***************************** Spinner Code ***********************************
Dim spinnerCount, spinnerHit

Sub spinner_spin
	addScore 10
	spinnerHit = 1
	spinnerCount = spinnerCount + 1
	If spinnerCount > 4 Then spinnerCount = 1
	XERBcycle 
End Sub

Sub XERBcycle																			' the spinner cycles the XERB lights
	Select Case spinnerCount
		Case 1: L033.state = 0: L030.state = 1: L031.state = 0
		Case 2:	L030.state = 0: L031.state = 1: L032.state = 0
		Case 3:	L031.state = 0: L032.state = 1: L033.state = 0
		Case 4: L032.state = 0: L033.state = 1: L030.state = 0
	End Select
End Sub

Dim phoneOn, phoneLoop
Sub phone_Timer
	phoneLoop = phoneLoop + 1
	phoneOn = 1
	If phoneLoop > 14 Then
		phoneOn = 0
		phone.enabled = 0
	End If
End Sub

Sub Gates_Hit(idx)
	PlayFieldSoundAB "gate", 0, 1
End Sub

'********************************** XERB Saucer *********************************
Dim xerbVO
Sub XERBkicker_Hit()
	xerbVO = xerbVO + 1
	If xerbVO > 5 Then xerbVO = 1
	saucer.enabled = True
	playOcxVo  "vo_WMJ" & xerbVO & ".mp3"
	Select Case spinnerCount
		Case 1: collectPlate
		Case 2: addScore 500
		Case 3: addScore 400
		Case 4: addScore 300: playSound"sfx_Payphone", 0, musicVol: phone.enabled = 1
	End Select
	L030.state = 0: L031.state = 0: L032.state = 0: L033.state = 0
End Sub

Dim cars
Sub collectPlate														' this pays out one of the four license plates on the playfield
	cars = cars + 1
	If cars > 4 Then 
		plate.Enabled = 1
		cars = 1
	End If
	EVAL ("L" & (124+cars)).state = 1
	addScore (1000 * cars)
	playsound "sfx_EngineRev", 0, musicVol
End Sub

Dim plateCount
Sub plate_timer
	plateCount = plateCount + 1
	If plateCount Mod 2 = 0 Then
		For Each xx in plates: xx.state = 1: Next
	Else
		for Each xx in plates: xx.state = 0: Next
	End If
	If plateCount > 7 Then
		plateCount = 0
		L125.state = 1
		For x = 126 to 128
			EVAL("L" & x).state = 0
		Next
		plate.Enabled = 0
	End If
End Sub

dim kickStep
Sub saucer_timer
    Select Case kickStep
        Case 7:
			SaucerArm.ObjRotX = 12
			XERBkicker.kick 188, 20, .1
			PlayFieldSound "SaucerKick", 0, Xerbkicker, 0.6
        Case 8:SaucerArm.ObjRotX = -45
        Case 9:SaucerArm.ObjRotX = -45
        Case 10:SaucerArm.ObjRotX = 24
        Case 11:SaucerArm.ObjRotX = 12
        Case 12:SaucerArm.ObjRotX = 0: saucer.enabled = 0: kickStep = 0
    End Select
    kickStep = kickStep + 1
End Sub

Dim wire
Sub wireAnimation_Timer
	wire = wire + 1
	Select Case wire
		Case 1: EVAL ("wire" & wireNumber).transz = -10
		Case 2: EVAL ("wire" & wireNumber).transz = -4
		Case 3: EVAL ("wire" & wireNumber).transz = -1
		Case 4: EVAL ("wire" & wireNumber).transz = 0	
				wire = 0
				wireAnimation.enabled = 0						  
	End Select
End Sub

Dim targetBend
Sub targetAnimation_timer
	targetBend = targetBend + 1
	Select Case targetBend
		Case 1: EVAL ("TargetMesh" & targetNumber).rotx = -5
		Case 2: EVAL ("TargetMesh" & targetNumber).rotx = -5
		Case 3: EVAL ("TargetMesh" & targetNumber).rotx = 5
		Case 4: EVAL ("TargetMesh" & targetNumber).rotx = 5
				targetBend = 0
				targetAnimation.enabled = 0					
	End Select
End Sub

'************************************ Super Modes *************************************************
'pins, cruise loop, bumper, 1962, Mels, DTs
Dim psmLoop,clsmLoop, bsmLoop, sm1962Loop, msmLoop, dtsmLoop
Dim psmOn, clsmOn, bsmOn, sm1962On, msmOn, dtsmOn
Dim psmScore, clsmScore, bsmScore, sm1962Score, msmScore, dtCount(4)

Dim superMode, smArray(6), smCount
Sub chooseSuperMode												' every two drag races one of the six super modes is selected
	superMode = superMode + 1
	If superModeLight.enabled = 0 Then
		For each xx in modeLights
			smCount = smCount + 1
			smArray(smCount) = xx.state
		Next
	End If
	smCount = 0
	superModeLight.enabled = 1	 
End Sub

Dim smlCount, tlArray(5), tlArrayTemp(5), tlCount, dCount, DTModeArray(5), pinArray(5), pinArrayTemp(5)
Sub superModeLight_timer										' this is a timer that flashes all of the lights in the modes as a part of the attract for the super modes
	smlCount = smlCount + 1
	If smlCount Mod 2 = 0 Then	
		For Each xx in modeLights: xx.state = 0: Next			' turn them all off and then ...
	Else
		For Each xx in modeLights: xx.state = 1: Next			' turn them all on
	End If
	If smlCount > 8 Then
		smlCount = 0
		For x = 1 to 6
			EVAL("L" & ((x*2)+108)).state = smArray(x)			' reset the mode lights so ones that have been achieved get relit
		Next
		Select Case superMode
			Case 1: L110.state = 2								' save the states of the pins before the supermode is started
					For x = 80 to 84
						If EVAL("L0" & x).state = 1 Then 
							pinArray(x-79) = 1
							pinArrayTemp(x-79) = 1
						Else
							pinArray(x-79) = 0
							pinArrayTemp(x-79) = 0
						End If
					Next
					pinsSuperMode.enabled = 1
			Case 2:	L112.state = 2: cruiseLoopSuperMode.enabled = 1
			Case 3:	L114.state = 2: bumperSuperMode.enabled = 1
			Case 4:	L116.state = 2								' save the states of the top 1962 lanes for resetting after the super mode is done
					For Each xx in topLaneLights
						tlCount = tlCount + 1
						tlArray(tlCount) = xx.state
						tlArrayTemp(tlCount) = xx.state
					Next
					tlCount = 0
					superMode1962.enabled = 1
			Case 5: L118.state = 2								' save the states of the Mel lights
					For Each xx in msmLights	
						mCount = mCount + 1
						msmArray(mCount) = xx.state			
						msmArrayTemp(mCount) = xx.state
					Next
					mCount = 0
					melSuperMode.enabled = 1
			Case 6:	L120.state = 2								' save the state of the drop target lights
					For Each xx in dtLights
						dCount = dCount + 1
						dtModeArray(dCount) = xx.state	
					Next
					dCount = 0
					dtSuperMode.enabled = 1
		End Select
		SuperModeLight.enabled = 0
	End If

End Sub

Dim checkTest
'pin, cruise, pop, top, mel, drop
Sub updateMelCheck
	checktest = convertToCheckTens(allPins(player))
	check11.image = checkTest
	checkTest = convertToCheckOnes(allPins(player))
	check10.image = checkTest

	checktest = convertToCheckTens(cruiseCount(player))
	check21.image = checkTest
	checkTest = convertToCheckOnes(cruiseCount(player))
	check20.image = checkTest

	checktest = convertToCheckTens(bumperHits(player))
	check31.image = checkTest
	checkTest = convertToCheckOnes(bumperHits(player))
	check30.image = checkTest

	checktest = convertToCheckTens(topLanes(player))
	check41.image = checkTest
	checkTest = convertToCheckOnes(topLanes(player))
	check40.image = checkTest

	checktest = convertToCheckTens(melCount(player))
	check51.image = checkTest
	checkTest = convertToCheckOnes(melCount(player))
	check50.image = checkTest
	
	checktest = convertToCheckTens(dtCount(player))
	check61.image = checkTest
	checkTest = convertToCheckOnes(dtCount(player))
	check60.image = checkTest		
End Sub

Dim ctct
Function convertToCheckTens(smValue)
	ctct = smValue\10	
	If ctct = 0 Then
		convertToCheckTens = "HS_Space"
	Else
		convertToCheckTens = "HS_" & ctct Mod 10
	End If
End Function

Dim ctco
Function convertToCheckOnes(smValue)
	ctco = smValue Mod 10
	If ctco = 0 Then
		If smValue = 0 Then
			convertToCheckOnes = "HS_Space"
		Else
			convertToCheckOnes = "HS_0"
		End If
	Else
		convertToCheckOnes = "HS_" & ctco
	End If
End Function

'**************************************************** Pinball Super Mode *************************************************
' Note the individual scoring of each of these super modes is hanfdled in the base modes of each of these sections
' this code is here to time the mode, save the existing state for reset and flash the shots
Sub pinsSuperMode_timer
	psmOn = 1
	psmLoop = psmLoop + 1
	If psmLoop Mod 2 = 0 Then								' this flahses the pins on/offf
		L110.state = 1
		For x = 80 to 84
			EVAL("L0" & x).state = 1
		Next
		lightPins
	Else
		L110.state = 0
		For x = 1 to 5
			If pinArrayTemp(x) = 0 Then EVAL("L0" & (x+79)).state = 0
		Next
		lightPins
	End If
	If psmLoop > 100 Then
		psmLoop = 0
		resetSuperModeLights
		psmOn = 0
		psmScore = 0
		pinsSuperMode.enabled = 0
	End If
End Sub

'**************************************************** Cruise Lane Super Mode *************************************************
Sub cruiseLoopSuperMode_timer
	clsmOn = 1
	clsmLoop = clsmLoop + 1
	If clsmLoop Mod 2 = 0 Then				' this flahses the Cruise Lane Lights on/off
		L112.state = 1
		L040.state = 1
		L041.state = 1
	Else
		L112.state = 0
		L040.state = 0
		L041.state = 0
	End If
	If clsmLoop > 80 Then
		clsmOn = 0
		clsmLoop = 0
		clsmScore = 0
		resetSuperModeLights
		cruiseLoopSuperMode.enabled = 0
	End If
End Sub

'**************************************************** Bumper Super Mode *************************************************
Sub bumperSuperMode_timer
	bsmOn = 1
	bsmLoop = bsmLoop + 1
	If bsmLoop Mod 2 = 0 Then				' this flashes the bumpers on/off
		L114.state = 1
		Bumper1Light.state = 1
		Bumper2Light.state = 1
		Bumper3Light.state = 1
	Else
		L114.state = 0
		Bumper1Light.state = 0
		Bumper2Light.state = 0
		Bumper3Light.state = 0
	End If
	If bsmLoop > 60 Then
		bsmLoop = 0
		bsmOn = 0
		bsmScore = 0
		resetSuperModeLights
		bumperSuperMode.enabled = 0
	End If
End Sub

'**************************************************** Top 1962 Super Mode *************************************************
Sub superMode1962_timer
	sm1962On = 1
	sm1962Loop = sm1962Loop + 1
	If sm1962Loop Mod 2 = 0 Then								' this flashes the top lane 1962 lights on/off
		For Each xx in topLaneLights: xx.state = 1: Next
	Else
		For Each xx in topLaneLights
			tlCount = tlCount + 1
			If tlArrayTemp(tlCount) = 1 Then 
				xx.state = 1
			Else
				xx.state = 0
			End If
		Next
		tlCount = 0
	End If
	If sm1962Loop > 200 Then
		sm1962On = 0
		sm1962Loop = 0
		sm1962Score = 0
		resetSuperModeLights
		For Each xx in topLaneLights
			tlCount = tlCount + 1
			If tlArray(tlCount) = 1 Then 
				xx.state = 1
			Else
				xx.state = 0
			End If
		Next			
		tlCount = 0
		superMode1962.enabled = 0
	End If
End Sub

'**************************************************** Mel's Diner Super Mode *************************************************
Dim msmArray(5), msmArrayTemp(5), mCount
Sub melSuperMode_timer
	msmOn = 1
	msmLoop = msmLoop + 1
	If msmLoop Mod 2 = 0 Then									' this flashes the Mel lights on/off
		L118.state = 1
		For Each xx in msmLights: xx.state = 1: Next
	Else
		For x = 1 to 4
			If msmArrayTemp(x) = 0 Then EVAL("L0" & (x+9)).state = 0
		Next
		L118.state = 0
	End If
	If msmLoop > 200 Then	
		msmLoop = 0
		msmOn = 0
		msmScore = 0
		resetSuperModeLights
		For x = 1 to 4
			EVAL("L0" & (x+9)).state = msmArray(x)
		Next
		melSuperMode.enabled = 0
	End If
End Sub

'**************************************************** Drop Target Super Mode *************************************************
Sub dtSuperMode_timer
	dtsmOn = 1
	dtsmLoop = dtsmLoop + 1
	If dtsmLoop Mod 2 = 0 Then								' this flashes the dt lights on/off
		For Each xx in dtLights: xx.state = 1: Next
	Else
		L120.state = 0
		For x = 1 to 4
			If dtModeArray(x) = 0 Then EVAL("L0" & (x+13)).state = 0
		Next
	End If
	If dtsmLoop > 200 Then
		dtsmLoop = 0
		dtsmOn = 0
		'dtCount(player) = 0
		resetSuperModeLights
		dtSuperMode.enabled = 0
	End If
End Sub

Sub resetSuperModeLights
	For x = 1 to 5
		EVAL ("L" & ((x*2)+108)).state = smArray(x)
	Next
End Sub

'**************Special
Sub special
	increaseCredits
	knock
End Sub

'************Music Playing Routines
Dim songList(25), songPlace, songNumber, gsnCount
Sub generateSongNumber(caller)
	If musicOn = 0 Then Exit Sub
    songNumber = INT(25 * RND(1) )
	newSong = False
	checkSongNumber(caller)
End Sub

Sub checkSongNumber(caller)
	For x = 0 to 24
		If songList(x) = songNumber Then 
			generateSongNumber(caller)
			Exit Sub
		End If
	Next
	tb.text = "Caller = " & caller
	songPlace = songPlace + 1
	songList(songPlace) = songNumber
	If songPlace > 23 Then clearSongList
	If caller = 4 or caller = 5 or caller = 6 Then 
		xerb
	Else
		songPlayer
	End If
End Sub

Dim xerbSlot
Sub xerb
	If Balls = 3 Then
		xerbSlot = ballInPlay + (ballInPlay - 1)
	Else
		xerbSlot = ballInPlay
	End If
	Select Case xerbSlot
		Case 1: playOcxXERB  "XERBbumper1.mp3" 
		Case 2: playOcxXERB  "XERBbumper2.mp3" 
		Case 3:	playOcxXERB  "XERBbumper3.mp3" 
		Case 4:	playOcxXERB  "XERBbumper4.mp3"
		Case 5:	playOcxXERB  "XERBbumper5.mp3"
	End Select
End Sub

Sub playOcxXERB(xerb)
    ocxPlayerXERB.URL = voDir & xerb
    ocxPlayerXERB.settings.volume = oldMusicVol*100
    ocxPlayerXERB.controls.play
    playerXERBTime.Enabled = 1
End Sub

Sub playerXERBTime_timer
    If ocxPlayerXERB.playState <> 3 And ocxPlayerXERB.playstate <> 9 Then 
        ocxPlayerXERB.controls.stop
		generateSongNumber(10)
        playerXERBTime.Enabled = 0
    End If
End Sub

Sub clearSongList
	songPlace = 0
	For x = 0 to 24
		songList(x) = ""
	Next
End Sub

Sub songPlayer
	For x = 140 to 142
		If B2SOn Then controller.B2SSetData x, 0
	Next
	
	For x = 160 to 185
		If B2SOn Then controller.B2SSetData x, 0
	Next
	If B2SOn Then controller.B2SsetData (songNumber+161), 1
	If musicOn = 1 and endGame = 0 Then
		Select Case songNumber
			Case 0:   playOcxMusic "AG1.mp3" 'Why Do Fools Fall in Love, Frankie Lymon
			Case 1:   playOcxMusic "AG2.mp3" 'At the Hop, Flash Cadillac
			Case 2:   playOcxMusic "AG3.mp3" 'The Book of Love, The Monotones
			Case 3:   playOcxMusic "AG4.mp3" 'A Thousand Miles Away, The Heartbeats
			Case 4:   playOcxMusic "AG5.mp3" 'Almost Grown, Chuck Berry
			Case 5:   playOcxMusic "AG6.mp3" 'Johnny B. Goode, Chuck Berry
			Case 6:   playOcxMusic "AG7.mp3" 'Fannie Mae, Buster Brown
			Case 7:   playOcxMusic "AG8.mp3" 'The Stroll, The Diamonds
			Case 8:   playOcxMusic "AG9.mp3" 'Regents, Barbara Ann"
			Case 9:   playOcxMusic "AG10.mp3" 'Buddy Holly, That'll Be the Day
			Case 10:  playOcxMusic "AG11.mp3" 'Flamingos, I Only Have Eyes
			Case 11:  playOcxMusic "AG12.mp3" 'Skyliners, Since I Don't Have You
			Case 12:  playOcxMusic "AG13.mp3" 'Silhouettes, Get a Job
			Case 13:  playOcxMusic "AG14.mp3" 'Spaniels, Goodnight Sweetheart
			Case 14:  playOcxMusic "AG15.mp3" 'Johnny Burnette, You're 16
			Case 15:  playOcxMusic "AG16.mp3" 'Starlighters, Peppermint Twist
			Case 16:  playOcxMusic "AG17.mp3" 'Platters, Smoke Gets In Your Eyes 
			Case 17:  playOcxMusic "AG18.mp3" 'Platters, The Great Pretender
			Case 18:  playOcxMusic "AG19.mp3" 'Del-Vikings, Come Go With Me
			Case 19:  playOcxMusic "AG20.mp3" 'Crests, Sixteen Candles
			Case 20:  playOcxMusic "AG21.mp3" 'Five Satins, To the Aisle
			Case 21:  playOcxMusic "AG22.mp3" 'Del Shannon, Runaway
			Case 22:  playOcxMusic "AG23.mp3" 'Fats Domino, Ain't That a Shame
			Case 23:  playOcxMusic "AG24.mp3" 'Beach Boys, Surfing Sufari
			Case 24:  playOcxMusic "AG25.mp3" 'Cleftones, Heart and Soul
		End Select
	End If
End Sub



'***************Scoring Routines
Dim Flag10, Flag100, Flag1000, Flag10k, Point, Point10, BellRing
Sub pts10
	If pts10Timer.enabled = False Then
		pts10Timer.enabled = True
		playFieldSound "Chime10", 0, soundPoint13, 1
	End If
End Sub

Sub pts100
	If pts100Timer.enabled = False Then
		pts100Timer.enabled = True
		playFieldSound "Chime100", 0, soundPoint13, 1
	End If
End Sub

Sub pts1000
	If pts1000Timer.enabled = False Then
		pts1000Timer.enabled = True
		playFieldSound "Chime1000", 0, soundPoint13, 1
	End If
End Sub

Sub pts10k
	If pts10kTimer.enabled = False Then
		pts10kTimer.enabled = True
		playFieldSound "Chime1000", 0, soundPoint13, 1
	End If
End Sub

Dim pts10Block, pts100Block, pts1000Block, pts10kBlock, intvl
intvl = 40
pts10Timer.interval = intvl
pts100Timer.interval = intvl
pts1000Timer.interval = intvl

Sub pts10Timer_Timer
	pts10Block = pts10Block + 1
	If pts10Block = 1 Then
		pts10Block = 0
		pts10Timer.enabled = False
	End If
End Sub

Sub pts100Timer_Timer
	pts100Block = pts100Block + 1
	If pts100Block = 1 Then
		pts100Block = 0
		pts100Timer.enabled = False
	End If
End Sub

Sub pts1000Timer_Timer
	pts1000Block = pts1000Block + 1
	If pts1000Block = 1 Then
		pts1000Block = 0
		pts1000Timer.enabled = False
	End If
End Sub

Sub pts10kTimer_Timer
	pts10kBlock = pts10kBlock + 1
	If pts10kBlock = 1 Then
		pts10kBlock = 0
		pts10kTimer.enabled = False
	End If
End Sub

Sub ballControlBlockTimer_Timer
	ballControlBlock = False
	ballControlBlockTimer.enabled = 0
End Sub

Dim ballControlBlock
Sub addScore(points) 
	If ballControlBlock = True or tilt = True Then Exit Sub
	If contball = 1 Then
		ballControlBlock = True
		BallControlBlockTimer.enabled = 1
	End If 
	If Tilt = False Then
		If Points <100 Then
			Point10 = (Points mod 100)/10
			If points > 10 Then
				BellRing = (Points / 10)
				Point = 10: Flag10 = 1: scoreMotor.enabled = 1
			Else
				If motorOn = 0 Then totalUp 10
			End If
			Exit Sub
		End If

		If Points > 99 and Points < 1000 Then 
			If points > 100 Then
				bellRing = (Points / 100)
				point = 100: flag100 = 1: scoreMotor.enabled = 1
			Else
				If motorOn = 0 Then totalUp 100	  
			End If																		  
			Exit Sub
		End If

		If Points > 999 and Points < 10000 Then 
			If points > 1000 Then
				BellRing = (Points / 1000)		
				point = 1000: flag1000 = 1: scoreMotor.enabled = 1
			Else
				If motorOn = 0 Then totalUp 1000
			End If  
		End If	

		If Points > 9999 Then 
			If points > 10000 Then
				BellRing = (Points / 10000)	 
				point = 10000: flag10k = 1: scoreMotor.enabled = 1
			Else
				If motorOn = 0 Then totalUp 10000
			End If  
		End If								 
	End If
End Sub

Dim replayX, replay(7), repAwarded(5), block
Sub totalUp(points)
	If pts10Timer.enabled = False and points = 10 Or pts100Timer.enabled = False and points = 100 Or pts1000Timer.enabled = False and points = 1000 or pts10kTimer.enabled = False and points = 10000 Then block = 0 Else block = 1

	If B2SOn And showDT = False Then
		If Player = 1 and block = 0 Then PlayReelSound "Reel1", bgs1Reel
		If Player = 2 and block = 0 Then PlayReelSound "Reel2", bgs2Reel
		If Player = 3 and block = 0 Then PlayReelSound "Reel3", bgs3Reel
		If Player = 4 and block = 0 Then PlayReelSound "Reel4", bgs4Reel
	End If
	
	If showDT = True Then
		If Player = 1 and block = 0 Then PlayReelSound "Reel1", dts1Reel
		If Player = 2 and block = 0 Then PlayReelSound "Reel2", dts2Reel
		If Player = 3 and block = 0 Then PlayReelSound "Reel3", dts3Reel
		If Player = 4 and block = 0 Then PlayReelSound "Reel4", dts4Reel
	End If
		   
	If flag10 = 1 or points = 10 Then
		If chime = 0 Then
			pts10
		Else 
			If B2SOn Then DOF 153,DOFPulse
		End If
	End If

	If flag100 = 1 or points = 100 Then
		If chime = 0 Then
			pts100
		Else 
			If B2SOn Then DOF 154,DOFPulse
		End If
	End If

	If flag1000 = 1 or points = 1000 Then
		If chime = 0 Then
			pts1000
		Else 
			If B2SOn Then DOF 155,DOFPulse
		End If	
	End If

	If flag10k = 1 or points = 10000 Then
		If chime = 0 Then
			pts10k
		Else 
			If B2SOn Then DOF 155,DOFPulse
		End If	
	End If

	If bonusFlag = 1 Then
							
		If chime = 0 Then
			pts1000
		Else 
			If B2SOn Then DOF 155,DOFPulse											   
		End If
	End If

	If block = 0 Then
		score(Player) = score(player) + points 
		sReel(Player).addValue(points)
		EVAL("ScoreReelOff" & player).addValue(points)
	End If
	If vrOption > 0 Then vrScore(player)
		  
	If B2SOn Then Controller.B2SSetScorePlayer Player, Score(player)
		
	For ReplayX = Rep(Player) +1 to 3
		If Score(Player) => Replay(ReplayX) Then
			increaseCredits
			Rep(Player) = Rep(Player) + 1
			knock 
		End If
	Next													
End Sub


'***************Score Motor Run one full rotation
Dim scoreMotorCount, addCredit
Sub scoremotor5_Timer
	If scoremotor.enabled = 1 Then scoreMotor5.enabled = 0: Exit Sub
	scoreMotorCount = scoreMotorCount + 1
	playFieldSound "ScoreMotorSingleFire", 0, SoundPointScoreMotor, 0.02
	If scoremotorCount = 5 Then
		If relBall = 1 Then
			relBall = 0
			releaseBall					
		End If
		If addCredit = 1 Then
			increaseCredits
			addCredit = 0
		End If
		scoreMotorCount = 0
		scoreMotor5.enabled = 0											
	End If	
End Sub

Sub increaseCredits
	credit = credit + 1
	If showDT = False Then PlayReelSound "Reel5", bgcrReel Else PlayReelSound "Reel5", dtcrReel
	If credit > 15 then credit = 15
	creditReel.setvalue(credit)
	If vrOption > 0 Then vrCredit
	If B2SOn Then 
		controller.B2SSetCredits credit
		If credit > 0 Then DOF 127, DOFOn						   
	End If
End Sub

'**************Score Motor Routine
Dim scoreMotorLoop, scoreMotorDone, motorOn
Sub scoreMotor_timer
	scoreMotorLoop = scoreMotorLoop + 1
	If scoreMotorLoop < 6 Then playFieldSound "ScoreMotorSingleFire", 0, Speaker5, 0.02
	motorOn = 1

'  These Flags are passed by scores with multiple of 10, 100 or 1000
	If Flag10 = 1 or Flag100 = 1 or Flag1000 = 1 or flag10k = 1 Then
		Select Case scoreMotorLoop
			Case 1: totalUp Point	
			Case 2: If bellRing > 3 Then totalUp point
			Case 3: totalUp Point
			Case 4: If bellRing > 3 Then totalUp point
			Case 5: If bellRing > 2 Then totalUp Point
			Case 6: scoreMotorLoop = 0  
					flag10 = 0 
					flag100 = 0
					flag1000 = 0
					motorOn = 0
					If drainActive = 0 Then scoreMotor.enabled = 0
		End Select			   
	End If
	   
' BonusFlag lets the sub know that the bonus score is being paid out
' The bonus is paid out on the 2nd, 3rd and 4th positions of the score motor, the 6th position is the index reel notch	
	If bonusFlag = 1 and playerDone(player) = 0 Then
		Select Case scoreMotorLoop
			Case 2:	If tilt = False and carLights > 0 Then 
							EVAL("L0" & (carLights+69)).state = 0
							carlights = carLights -1
							agBonus	
						End If
			Case 4: If tilt = False and carLights > 0 Then 
							EVAL("L0" & (carLights+69)).state = 0
							carlights = carLights -1 
							agBonus 
						End If
			Case 6: scoreMotorLoop = 0
					If carLights = 0 Then
						bonusFlag = 0
						advancePlayers
						motorOn = 0
						ScoreMotor.enabled = 0
					End If
		End Select
	End If
End Sub

'***************AG bonus
Dim agPoints
Sub agBonus
	Select Case bonus
		Case 0:	agPoints = 1000
		Case 1:	agPoints = 2000
		Case 2:	agPoints = 3000
		Case 3:	agPoints = 4000
	End Select
	If chime = 0 Then
		pts1000
	Else 
		If B2SOn Then DOF 155,DOFPulse
	End If
	score(player) = score(player) + agPoints
	sReel(Player).addValue(agPoints)
	EVAL("ScoreReelOff" & player).addValue(agPoints)
	If vrOption > 0 Then vrScore(player)  
	If B2SOn Then Controller.B2SSetScorePlayer Player, Score(player)
End Sub

'***************Tilt
Dim tiltSens  
Sub checkTilt
	If tilttimer.Enabled = True Then 
	 tiltSens = tiltSens + 1
	 If tiltSens = 3 Then
	    tilt = True
		tiltReel.SetValue(1)
		If vrOption > 0 Then vrTilt.visible = 1
       	If B2SOn Then controller.B2SSetTilt 33,1
       	If B2SOn Then controller.B2SSetdata 1, 0
	    turnOff
	 End If
	Else
	 tiltSens = 0
	 tiltTimer.Enabled = True	  
	End If					  
End Sub

Sub tiltTimer_Timer()
	tiltTimer.Enabled = False
End Sub

'***************************************************Reset Reels Section*******************************************************

'This Sub looks at each individual digit in each players score and sets them in an array RScore.  If the value is >0 and <9
'then the players score is increased by one times the position value of that digit (ie 1 * 1000 for the 1000's digit)
'If the value of the digit is 9 then it subtracts 9 times the postion value of that digit (ie 9*100 for the 100's digit)
'so that the score is not rolled over and the next digit in line gets incremented as well (ie 9 in the 10's positon gets 
'incremented so the 100's position rolls up by one as well since 90 -> 100).  Lastly the RScore array values get incremented
'by one to get ready for the next pass.

Dim rScore(4,5), resetLoop, test, playerTest, resetFlag, reelFlag, reelStop, reelDone(4)
Sub countUp
	For playerTest = 1 to maxPlayers
		For test = 0 to 4
			rScore(playerTest,test) = Int(score(playerTest)/10^test) mod 10
		Next
	Next  
	
	For playerTest = 1 to maxPlayers
		For x = 0 to 4
			If rScore(playerTest, x) > 0 And rScore(playerTest, x) < 9 Then score(playerTest) = score(playerTest) + 10^x
			If rScore(playerTest, x) = 9 Then score(playerTest) = score(playerTest) - (9 * 10^x)	
			If rScore(playerTest, x) > 0 Then rScore(playerTest, x) = rScore(playerTest, x) + 1
			If rScore(playerTest, x) = 10 Then rScore(playerTest, x) = 0
		Next 
	Next

	If score(1) = 0 and score(2) = 0 and score(3) = 0 and score(4) = 0 Then 
		reelFlag = 1
		For i = 1 to maxPlayers
			score(i) = 0
			rep(i) = 0
			repAwarded(i) = 0
		Next
	End If
End Sub

'This Sub sets each B2S reel or Desktop reels to their new values and then plays the score motor sound each time and the
'reel sounds only if the reels are being stepped

Sub updateReels
	For playerTest = 1 to 4
		If B2SOn And showDT = False And reelDone(playerTest) = 0 And reelStop = 0 Then
			controller.B2SSetScorePlayer playerTest, score(playerTest)
			If reelDone(1) = 0 and playerTest = 1 Then PlayReelSound "Reel1", bgs1Reel
			If reelDone(2) = 0 and playerTest = 2 Then PlayReelSound "Reel2", bgs2Reel
			If reelDone(3) = 0 and playerTest = 3 Then PlayReelSound "Reel3", bgs3Reel
			If reelDone(4) = 0 and playerTest = 4 Then PlayReelSound "Reel4", bgs4Reel
			If score(playerTest) = 0 Then reelDone(playerTest) = 1
		End If

		If showDT = True And reelDone(playerTest) = 0 And reelStop = 0 Then 
			sReel(playerTest).setvalue (score(playerTest))
			EVAL("ScoreReelOff" &playerTest).setValue(score(playerTest))
			If reelDone(1) = 0 and playerTest = 1 Then playReelSound "Reel1", dts1Reel
			If reelDone(2) = 0 and playerTest = 2 Then playReelSound "Reel2", dts2Reel
			If reelDone(3) = 0 and playerTest = 3 Then playReelsound "Reel3", dts3Reel
			If reelDone(4) = 0 and playerTest = 4 Then playReelSound "Reel4", dts4Reel
			If score(playerTest) = 0 Then reelDone(playerTest) = 1
		End If

		If vrOption > 0 Then vrScore(playerTest)

	Next
	If reelFlag = 1 Then reelStop = 1
End Sub

'This Timer runs a loop that calls the CountUp and UpdateReels routines to step the reels up five times and Then
'check to see if they are all at zero during a two loop pause and then step them the rest of the way to zero

Dim testFlag
Sub resetReel_Timer
	For x = 1 to 4
		score(x) = (score(x) Mod 100000)
	Next
	resetLoop = resetLoop + 1
	If resetLoop = 1 and score(1) = 0 and score(2) = 0 and score(3) = 0 and score(4) = 0 Then
		resetLoop = 0
		If testFlag = 0 Then releaseBall
		bottomgate.rotateToEnd
		bgState = 1
		testFlag = 0
		resetReel.enabled = 0
		Exit Sub
	End If
	Select Case resetLoop
		Case 1: countUp: updateReels
		Case 2: countUp: updateReels
		Case 3: countUp: updateReels
		Case 4: countUp: updateReels
		Case 5: countUp: updateReels
		Case 6: If reelStop = 1 Then 
					resetLoop = 0
					reelFlag = 0
					reelStop = 0
					If testFlag = 0 Then releaseBall
					bottomgate.rotateToEnd
					bgState = 1
					testFlag = 0
					resetReel.enabled = 0
					Exit Sub
				End If

		Case 7:
		Case 8: countUp: updateReels
		Case 9: countUp: updateReels
		Case 10: countUp: updateReels
		Case 11: countUp: updateReels
		Case 12: countUp: updateReels: 
			resetLoop = 0
			reelFlag = 0
			reelStop = 0
			If testFlag = 0 Then releaseBall
			bottomgate.rotateToEnd
			bgState = 1
			testFlag = 0
			resetReel.enabled = 0
			Exit Sub 	
	End Select
End Sub

'************************************************Post It Note Section**************************************************************************
'***************Static Post It Note Update
Dim hsY, shift, scoreMil, score100K, score10K, scoreK, score100, score10, scoreUnit
Dim hsInitial0, hsInitial1, hsInitial2
Dim hsArray: hsArray = Array("HS_0","HS_1","HS_2","HS_3","HS_4","HS_5","HS_6","HS_7","HS_8","HS_9","HS_Space","HS_Comma")
Dim hsiArray: hsIArray = Array("HSi_0","HSi_1","HSi_2","HSi_3","HSi_4","HSi_5","HSi_6","HSi_7","HSi_8","HSi_9","HSi_10","HSi_11","HSi_12","HSi_13","HSi_14","HSi_15","HSi_16","HSi_17","HSi_18","HSi_19","HSi_20","HSi_21","HSi_22","HSi_23","HSi_24","HSi_25","HSi_26")

Sub updatePostIt
	scoreMil = Int(highScore(0)/1000000)
	score100K = Int( (highScore(0) - (scoreMil*1000000) ) / 100000)
	score10K = Int( (highScore(0) - (scoreMil*1000000) - (score100K*100000) ) / 10000)														
	scoreK = Int( (highScore(0) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) ) / 1000)										
	score100 = Int( (highScore(0) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) ) / 100)						
	score10 = Int( (highScore(0) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) ) / 10)			
	scoreUnit = (highScore(0) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) - (score10*10) ) 

	pScore6.image = hsArray(scoreMil):If highScore(0) < 1000000 Then pScore6.image = hsArray(10)
	pScore5.image = hsArray(score100K):If highScore(0) < 100000 Then pScore5.image = hsArray(10)
	pScore4.image = hsArray(score10K):If highScore(0) < 10000 Then pScore4.image = hsArray(10)
	pScore3.image = hsArray(scoreK):If highScore(0) < 1000 Then pScore3.image = hsArray(10)
	pScore2.image = hsArray(score100):If highScore(0) < 100 Then pScore2.image = hsArray(10)
	pScore1.image = hsArray(score10):If highScore(0) < 10 Then pScore1.image = hsArray(10)
	pScore0.image = hsArray(scoreUnit):If highScore(0) < 1 Then pScore0.image = hsArray(10)
	If highScore(0) < 1000 Then
		PComma.image = hsArray(10)
	Else
		pComma.image = hsArray(11)
	End If
	If highScore(0) < 1000000 Then
		pComma1.image = hsArray(10)
	Else
		pComma1.image = hsArray(11)
	End If
	If highScore(0) > 999999 Then shift = 0 :pComma.transx = 0
	If highScore(0) < 1000000 Then shift = 1:pComma.transx = -10
	If highScore(0) < 100000 Then shift = 2:pComma.transx = -20
	If highScore(0) < 10000 Then shift = 3:pComma.transx = -30
	For hsY = 0 to 6
		EVAL("Pscore" & hsY).transx = (-10 * shift)
	Next
	initial1.image = hsIArray(initial(0,1))
	initial2.image = hsIArray(initial(0,2))
	initial3.image = hsIArray(initial(0,3))
End Sub

'***************Show Current Score
Sub showScore
	scoreMil = Int(highScore(activeScore(flag))/1000000)
	score100K = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) ) / 100000)
	score10K = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) ) / 10000)														
	scoreK = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) ) / 1000)										
	score100 = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) ) / 100)						
	score10 = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) ) / 10)			
	scoreUnit = (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) - (score10*10) ) 

	pScore6.image = hsArray(scoreMil):If highScore(activeScore(flag)) < 1000000 Then pScore6.image = hsArray(10)
	pScore5.image = hsArray(score100K):If highScore(activeScore(flag)) < 100000 Then pScore5.image = hsArray(10)
	pScore4.image = hsArray(score10K):If highScore(activeScore(flag)) < 10000 Then pScore4.image = hsArray(10)
	pScore3.image = hsArray(scoreK):If highScore(activeScore(flag)) < 1000 Then pScore3.image = hsArray(10)
	pScore2.image = hsArray(score100):If highScore(activeScore(flag)) < 100 Then pScore2.image = hsArray(10)
	pScore1.image = hsArray(score10):If highScore(activeScore(flag)) < 10 Then pScore1.image = hsArray(10)
	pScore0.image = hsArray(scoreUnit):If highScore(activeScore(flag)) < 1 Then pScore0.image = hsArray(10)
	If highScore(activeScore(flag)) < 1000 Then
		pComma.image = hsArray(10)
	Else
		pComma.image = hsArray(11)
	End If
	If highScore(activeScore(flag)) < 1000000 Then
		pComma1.image = hsArray(10)
	Else
		pComma1.image = hsArray(11)
	End If
	If highScore(flag) > 999999 Then shift = 0 :pComma.transx = 0
	If highScore(activeScore(flag)) < 1000000 Then shift = 1:pComma.transx = -10
	If highScore(activeScore(flag)) < 100000 Then shift = 2:pComma.transx = -20
	If highScore(activeScore(flag)) < 10000 Then shift = 3:pComma.transx = -30
	For HSy = 0 to 6
		EVAL("Pscore" & hsY).transx = (-10 * shift)
	Next
	initial1.image = hsIArray(initial(activeScore(flag),1))
	initial2.image = hsIArray(initial(activeScore(flag),2))
	initial3.image = hsIArray(initial(activeScore(flag),3))
End Sub

'***************Dynamic Post It Note Update
Dim scoreUpdate, dHSx
Sub dynamicUpdatePostIt_Timer
	scoreMil = Int(highScore(scoreUpdate)/1000000)
	score100K = Int( (highScore(ScoreUpdate) - (scoreMil*1000000) ) / 100000)
	score10K = Int( (highScore(scoreUpdate) - (ScoreMil*1000000) - (Score100K*100000) ) / 10000)														
	scoreK = Int( (highScore(scoreUpdate) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) ) / 1000)										
	score100 = Int( (highScore(ScoreUpdate) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) ) / 100)						
	score10 = Int( (highScore(ScoreUpdate) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) ) / 10)			
	scoreUnit = (highScore(ScoreUpdate) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) - (score10*10) ) 

	pScore6.image = hsArray(ScoreMil):If highScore(scoreUpdate) < 1000000 Then pScore6.image = hsArray(10)
	pScore5.image = hsArray(Score100K):If highScore(scoreUpdate) < 100000 Then pScore5.image = hsArray(10)
	pScore4.image = hsArray(Score10K):If highScore(scoreUpdate) < 10000 Then pScore4.image = hsArray(10)
	pScore3.image = hsArray(ScoreK):If highScore(scoreUpdate) < 1000 Then pScore3.image = hsArray(10)
	pScore2.image = hsArray(Score100):If highScore(scoreUpdate) < 100 Then pScore2.image = hsArray(10)
	pScore1.image = hsArray(Score10):If highScore(scoreUpdate) < 10 Then pScore1.image = hsArray(10)
	pScore0.image = hsArray(ScoreUnit):If highScore(scoreUpdate) < 1 Then pScore0.image = hsArray(10)
	If highScore(scoreUpdate) < 1000 Then
		pComma.image = hsArray(10)
	Else
		pComma.image = hsArray(11)
	End If
	If highScore(scoreUpdate) < 1000000 Then
		pComma1.image = hsArray(10)
	Else
		pComma1.image = hsArray(11)
	End If
	If highScore(scoreUpdate) > 999999 Then shift = 0 :pComma.transx = 0
	If highScore(scoreUpdate) < 1000000 Then shift = 1:pComma.transx = -10
	If highScore(scoreUpdate) < 100000 Then shift = 2:pComma.transx = -20
	If highScore(scoreUpdate) < 10000 Then shift = 3:pComma.transx = -30
	For dHSx = 0 to 6
		EVAL("Pscore" & dHSx).transx = (-10 * shift)
	Next
	initial1.image = hsIArray(initial(scoreUpdate,1))
	initial2.image = hsIArray(initial(scoreUpdate,2))
	initial3.image = hsIArray(initial(scoreUpdate,3))
	scoreUpdate = scoreUpdate + 1
	If scoreUpdate = 5 then scoreUpdate = 0
End Sub

'***************Bubble Sort
Dim tempScore(2), tempPos(3), position(5)
Dim bSx, bSy
'Scores are sorted high to low with Position being the player's number
Sub sortScores
	For bSx = 1 to 4
		position(bSx) = bSx
	Next
	For bSx = 1 to 4
		For bSy = 1 to 3
			If score(bSy) < score(bSy+1) Then	
				tempScore(1) = score(bSy+1)
				tempPos(1) = position(bSy+1)
				score(bSy+1) = score(bSy)
				score(bSy) = tempScore(1)
				position(bSy+1) = position(BSy)
				position(bSy) = tempPos(1)
			End If
		Next
	Next
End Sub

'*************Check for High Scores

Dim highScore(5), activeScore(5), hs, chX, chY, chZ, chIX, tempI(4), tempI2(4), flag, hsI, hsX
'goes through the 5 high scores one at a time and compares them to the player's scores high to 
'if a player's score is higher it marks that postion with ActiveScore(x) and moves all of the other
'	high scores down by one along with the high score's player initials
'	also clears the new high score's initials for entry later
Sub checkHighScores
	For hs = 1 to maxPlayers     									'look at 4 player scores		
		For chY = 0 to 4   					    					'look at all 5 saved high scores
			If score(hs) > highScore(chY) Then
				flag = flag + 1										'flag to show how many high scores needs replacing 
				tempScore(1) = highScore(chY)
				highScore(chY) = score(hs)
				activeScore(hs) = chY								'ActiveScore(x) is the high score being modified with x=1 the largest and x=4 the smallest
				For chIX = 1 to 3									'set initals to blank and make temporary initials = to intials being modifed so they can move down one high score
					tempI(chIX) = initial(chY,chIX)
					initial(chY,chIX) = 0
				Next
				
				If chY < 4 Then										'check if not on lowest high score for overflow error prevention
					For chZ = chY+1 to 4							'set as high score one more than score being modifed (CHy+1)
						tempScore(2) = highScore(chZ)				'set a temporaray high score for the high score one higher than the one being modified 
						highScore(chZ) = tempScore(1)				'set this score to the one being moved
						tempScore(1) = tempScore(2)					'reassign TempScore(1) to the next higher high score for the next go around
						For chIX = 1 to 3
							tempI2(chIX) = initial(chZ,chIX)		'make a new set of temporary initials
						Next
						For chIX = 1 to 3
							initial(chZ,chIX) = tempI(chIX)			'set the initials to the set being moved
							tempI(chIX) = tempI2(chIX)				'reassign the initials for the next go around
						Next
					Next
				End If
				chY = 4												'if this loop was accessed set CHy to 4 to get out of the loop
			End If
		Next
	Next
'	Goto Initial Entry
		hsI = 1														'go to the first initial for entry
		hsX = 1														'make the displayed inital be "A"
		If flag > 0 Then											'Flag 0 when all scores are updated so leave subroutine and reset variables
			showScore
			playerEntry.visible = 1
			playerEntry.image = "Player" & position(Flag)
			initial(activeScore(flag),1) = 1						'make first inital "A"
			For chY = 2 to 3
				initial(activeScore(flag),chY) = 0					'set other two to " "
			Next
			For chY = 1 to 3										'display the initals on the tape
				EVAL("Initial" & chY).image = hsIArray(initial(activeScore(flag),chY))		
			Next
			ocxPlayerVo.controls.stop
			playOcxVo "vo_GoodTimes.mp3"
			initialTimer1.enabled = 1								'flash the first initial
			dynamicUpdatePostIt.enabled = 0							'stop the scrolling intials timer
			knock
			enableInitialEntry = True
		End If
End Sub


'************Enter Initials Keycode Subroutine
Dim initial(6,5), initialsDone
Sub enterIntitals(keycode)
		If keyCode = leftFlipperKey Then 
			hsX = hsX - 1																		'HSx is the inital to be displayed A-Z plus " "
			If hsX < 0 Then hsX = 26
			If hsI < 4 Then EVAL("Initial" & hsI).image = hsIArray(hsX)		'HSi is which of the three intials is being modified
			playSound "metalHitHigh"
		End If
		If keycode = RightFlipperKey Then
			hsX = hsX + 1
			If hsX > 26 Then hsX = 0
			If hsI < 4 Then EVAL("Initial"& hsI).image = hsIArray(hsX)
			playSound "metalHitHigh"
		End If
		If keycode = startGameKey and initialsDone = 0 Then
			If hsI < 3 Then																		'if not on the last initial move on to the next intial
				EVAL("Initial" & hsI).image = hsIArray(hsX)						'display the initial
				initial(activeScore(flag), hsI) = hsX										'save the inital
				playSound "metalHitMedium"
				EVAL("InitialTimer" & hsI).enabled = 0								'turn that inital's timer off
				EVAL("Initial" & hsI).visible = 1											'make the initial not flash but be turn on
				initial(activeScore(flag),hsI + 1) = hsX									'move to the next initial and make it the same as the last initial
				EVAL("Initial" & hsI +1).image = hsIArray(hsX)					'display this intial
'				y = 1
				EVAL("InitialTimer" & hsI + 1).enabled = 1							'make the new intial flash
				hsI = hsI + 1																		'increment HSi
			Else																						'if on the last initial then get ready yo exit the subroutine 
				initial3.visible = 1																'make the intial visible
				playSound "metalHitMedium"
				initialTimer3.enabled = 0													'shut off the flashing
				initial(activeScore(flag),3) = hsX											'set last initial
				initialEntry																			'exit subroutine
			End If
		End If
End Sub

'************Update Initials and see if more scores need to be updated
Dim eIX
Sub initialEntry
	pts10
	flag = flag - 1
'	TextBox2.text = Flag
	hsI = 1
	If flag < 0 Then flag = 0: Exit Sub
	If flag = 0 Then 																						'exit high score entry mode and reset variables
		initialsDone = 1																					'prevents changes in intials while the highScoreDelay timer waits to finish
		players = 0
		For eIX = 1 to 4
			activeScore(eIX) = 0
			position(eIX) = 0
		Next
		For eIX = 1 to 3
			EVAL("InitialTimer" & eIX).enabled = 0
		Next
		playerEntry.visible = 0
		scoreUpdate = 0																				'go to the highest score
		updatePostIt																						'display that score
		highScoreDelay.enabled = 1
	Else
		showScore
		playerEntry.image = "Player" & position(flag)
		initial(activeScore(flag),1) = 1																'set the first initial to "A"
		For chY = 2 to 3
			initial(activeScore(flag),chY) = 0														'set the other two to " "
		Next
		For chY = 1 to 3
			EVAL("Initial" & chY).image = hsIArray(initial(activeScore(flag),chY))'display the intials
		Next
		hsX = 1																								'go to the letter "A"
		initialTimer1.enabled = 1																	'flash the first intial
	End If
End Sub

'************Delay to prevent start button push for last initial from starting game Update
Sub highScoreDelay_timer
	highScoreDelay.enabled = 0
	enableInitialEntry = False
	initialsDone = 0
	saveHighScore
	For eIX = 1 to 3
		EVAL("InitialTimer" & eIX).enabled = 0
	Next
	dynamicUpdatePostIt.enabled = 1																'turn scrolling high score back on
End Sub

'************Flash Initials Timers
Sub initialTimer1_Timer
	y = y + 1
	If y > 1 Then y = 0
	If y = 0 Then 
		initial1.visible = 1
	Else
		initial1.visible = 0	
	End If
End Sub

Sub initialTimer2_Timer
	y = y + 1
	If y > 1 Then y = 0
	If y = 0 Then 
		initial2.visible = 1
	Else
		initial2.visible = 0	
	End If
End Sub

Sub initialTimer3_Timer
	y = y + 1
	If y > 1 Then y = 0
	If y = 0 Then 
		initial3.visible = 1
	Else
		initial3.visible = 0	
	End If
End Sub

'************************************************** Change Table LUT Value *****************************************************
Sub setLUT
	If Balls = 3 Then
		Select Case lutValue
			Case 0: lutText.text = "AGLUT0"
			Case 1: lutText.text = "AGLUT2"
			Case 2: lutText.text = "AGLUT4"
		End Select
	Else
		Select Case lutValue
			Case 0: lutText.text = "AGLUT0"
			Case 1: lutText.text = "AGLUT1"
			Case 2: lutText.text = "AGLUT2"
			Case 3: lutText.text = "AGLUT3"
			Case 4: lutText.text = "AGLUT4"
		End Select
	End If
	Table1.ColorGradeImage = "AGLUT" & lutValue
End Sub


'**************************************************File Writing Section******************************************************

'*************Load Scores
Sub loadHighScore
	Dim fileObj
	Dim scoreFile
	Dim temp(40)
	Dim textStr
	Dim hiInitTemp(3)
	Dim hiInit(5)

    Set fileObj = CreateObject("Scripting.FileSystemObject")
	If Not fileObj.FolderExists(UserDirectory) Then 
		Exit Sub
	End If
	If Not fileObj.FileExists(UserDirectory & cOptions) Then
		Exit Sub
	End If
	Set scoreFile = fileObj.GetFile(UserDirectory & cOptions)
	Set textStr = scoreFile.OpenAsTextStream(1,0)
		If (textStr.AtEndOfStream = True) Then
			Exit Sub
		End If

		For x = 1 to 32
			temp(x) = textStr.readLine
		Next
		TextStr.Close	
		For x = 0 to 4
			highScore(x) = cdbl (temp(x+1))
		Next
		For x = 0 to 4
			hiInit(x) = (temp(x + 6))
		Next
		i = 10
		For x = 0 to 4
			For y = 1 to 3
				i = i + 1
				initial(x,y) = cdbl (temp(i))
			Next
		Next
		credit = CInt(Right(temp(26),1))
		freePlay = CInt(Right(temp(27),1))
		balls = CInt(Right(temp(28),1))
		chime = CInt(Right(temp(29),1))
		pfOption = CInt(Right(temp(30),1))
		musicOn = CInt(Right(temp(31),1))
		musicVol = Cdbl(Right(temp(32),2))
		oldMusicVol = musicVol
		Set scoreFile = Nothing
	    Set fileObj = Nothing
End Sub

'************Save Scores
Sub saveHighScore
Dim hiInit(5)
Dim hiInitTemp(5)
Dim FolderPath
	For x = 0 to 4
		For y = 1 to 3
			hiInitTemp(y) = chr(initial(x,y) + 64)
		Next
		hiInit(x) = hiInitTemp(1) + hiInitTemp(2) + hiInitTemp(3)
	Next
	Dim fileObj
	Dim scoreFile
	Set fileObj = createObject("Scripting.FileSystemObject")
	If Not fileObj.folderExists(userDirectory) Then 
		Exit Sub
	End If
	Set scoreFile = fileObj.createTextFile(userDirectory & cOptions,True)

		For x = 0 to 4
			scoreFile.writeLine highScore(x)
		Next
		For x = 0 to 4
			scoreFile.writeLine hiInit(x)
		Next
		For x = 0 to 4
			For y = 1 to 3
				scoreFile.writeLine initial(x,y)
			Next
		Next
		scoreFile.WriteLine "Credits: " & credit
		scorefile.writeline "FreePlay (0 = coin, 1 = free): " & freePlay
		scoreFile.WriteLine "Balls: " & balls
		scoreFile.WriteLine "Chime (0 = sound file, 1 = DOF chime): " & chime
		scoreFile.WriteLine "pfOption (1 = L/R Stereo, 2 = U/D Stereo, 3 = quad): " & pfOption
		scoreFile.WriteLine "Music On: " & musicOn
		scoreFile.WriteLine "Music Volume: " & musicVol
		scoreFile.Close
	Set scoreFile = Nothing
	Set fileObj = Nothing

'This section of code writes a file in the User Folder of VisualPinball that contains the High Score data for PinballY.
'PinballY can read this data and display the high scores on the DMD during game selection mode in PinballY.

	Set FileObj = CreateObject("Scripting.FileSystemObject")

	If cPinballY <> 1 Then Exit Sub

	If Not FileObj.FolderExists(UserDirectory) Then 
		Exit Sub
	End If

	FolderPath = FileObj.GetParentFolderName(UserDirectory)

	If cPinballY = 1 Then
		Set ScoreFile = FileObj.CreateTextFile(UserDirectory & hsFileName & ".PinballYHighScores",True)	
	End If

	For x = 0 to 4 
		ScoreFile.WriteLine HighScore(x) 
		ScoreFile.WriteLine HiInit(x)
	Next
	ScoreFile.Close
	Set ScoreFile = Nothing
	Set FileObj = Nothing

End Sub

'************Shut Down and De-energize for Tilt
Sub turnOff
	For x= 1 to 3
		EVAL("Bumper" & x).hashitevent = 0
	Next
	stopSound "FlipBuzzLA"
	stopSound "FlipBuzzLB"
	stopSound "FlipBuzzLC"
	stopSound "FlipBuzzLD"
	stopSound "FlipBuzzRA"
	stopSound "FlipBuzzRB"
	stopSound "FlipBuzzRC"
	stopSound "FlipBuzzRD"
  	leftFlipper.rotateToStart
	If B2SOn Then DOF 101, DOFOff
	rightFlipper.rotateToStart
	If B2SOn Then DOF 102, DOFOff
	tilt = True
End Sub  

'***************Exit Table
Sub Table1_Exit
	If B2SOn Then controller.stop
	If UseFlexDMD = True Then FlexDMD.Run = False
	saveHighScore
End Sub


'************VR Subs
Sub vrCredit
	vrCreditReel.objRotX = credit * 18
End Sub

Dim vrTen, vrHundred, vr1k, vr10k, vr100k, reelUp
Sub vrScore(reelUp)
	vrTen = (INT (score(reelUp)/10) )Mod 10
	vrHundred = (INT(score(reelUp)/100)) Mod 10
	vr1k =(INT (score(reelUp)/1000) )Mod 10
	vr10k = (INT (score(reelUp)/10000) )Mod 10
	vr100k = (INT(score(reelUp)/100000))Mod 10
	Select Case reelUp
		Case 1:	vrScoreReel110.objRotX = vrTen * 36
					vrScoreReel1100.objRotX = vrHundred * 36
					vrScoreReel11k.objRotX = vr1k * 36
					vrScoreReel110k.objRotX = vr10k * 36
					vrScoreReel1100k.objRotX = vr100k * 36
		Case 2:	vrScoreReel210.objRotX = vrTen * 36
					vrScoreReel2100.objRotX = vrHundred * 36
					vrScoreReel21k.objRotX = vr1k * 36
					vrScoreReel210k.objRotX = vr10k * 36
					vrScoreReel2100k.objRotX = vr100k * 36
		Case 3:	vrScoreReel310.objRotX = vrTen * 36
					vrScoreReel3100.objRotX = vrHundred * 36
					vrScoreReel31k.objRotX = vr1k * 36
					vrScoreReel310k.objRotX = vr10k * 36
					vrScoreReel3100k.objRotX = vr100k * 36
		Case 4:	vrScoreReel410.objRotX = vrTen * 36
					vrScoreReel4100.objRotX = vrHundred * 36
					vrScoreReel41k.objRotX = vr1k * 36
					vrScoreReel410k.objRotX = vr10k * 36
					vrScoreReel4100k.objRotX = vr100k * 36
	End Select
End Sub

Sub vrBipSet
	Select Case ballInPlay
		Case 0: vrBip.image = "bip0"
		Case 1: vrBip.image = "bip1"
		Case 2: vrBip.image = "bip2"
		Case 3: vrBip.image = "bip3"
		Case 4: vrBip.image = "bip4"
		Case 5: vrBip.image = "bip5"
	End Select
End Sub

Sub vrPlayerUp
	Select Case player
		Case 0: vrUp.image = "up0"
		Case 1: vrup.image = "up1"
		Case 2: vrup.image = "up2"
		Case 3: vrUp.image = "up3"
		Case 4: vrUp.image = "up4"
	End Select
End Sub

'*******************************************
' VR Room / VR Cabinet
'*******************************************

DIM VRThing
Sub vrRoomToggle
	If vrOption = 1 Then
		for each VRThing in vrCab: VRThing.visible = 1: Next
		for each VRThing in vrRoom: VRThing.visible = 0: Next
		for each VRThing in cabParts: VRThing.visible = 0: Next
	Elseif vrOption = 2 Then
		for each VRThing in vrCab: VRThing.visible = 1: Next
		for each VRThing in vrRoom: VRThing.visible = 1: Next
		for each VRThing in cabParts: VRThing.visible = 0: Next
	Else
		for each VRThing in vrCab: VRThing.visible = 0: Next
		for each VRThing in vrRoom: VRThing.visible = 0: Next
		tape.image = "tapeCab"
	End if
End Sub


'************************ ocxPlayers *****************************
' this is used to let voice overs complete before starting a song

Dim ocxPlayerVo, ocxPlayerXERB, currentVo, newSong, nextVO, playingVo, oldMusicVol
Set ocxPlayerVo = CreateObject("WMPlayer.OCX")
Set ocxPlayerXERB = CreateObject("WMPlayer.OCX")

Sub playOcxVO(vo)
	If playingVo = True Then 
		nextVo = vo
		Exit Sub
	End If
    currentVo = vo
    ocxPlayerVo.URL = voDir & currentVo
    ocxPlayerVo.settings.volume = oldMusicVol*100
	duckDirection = 0
	If musicVol = oldMusicVol Then duck.enabled = 1
	ocxPlayerMusic.settings.volume = 20
    ocxPlayerVo.controls.play
	playingVo = True
    playerVoTime.Enabled = 1
End Sub

Sub playerVoTime_timer
    If ocxPlayerVo.playState <> 3 And ocxPlayerVo.playstate <> 9 Then 
		duckDirection = 1
        ocxPlayerVo.controls.stop
		If nextVO <> "" Then
			playingVo = False
			playOcxVo(nextVO)
			nextVO = ""
			musicVol = oldMusicVol
			duck.Enabled = 1
			If newSong = True Then generateSongNumber(2)
			playerVoTime.Enabled = 0
		End If
		If endGame = 1 and musicOn = 1 Then playOcxMusic  "AGattract.mp3"
		playingVo = False
		musicVol = oldMusicVol
		duck.Enabled = 1
		If newSong = True Then generateSongNumber(3)
        playerVoTime.Enabled = 0
    End If
End Sub

Dim ocxPlayerMusic, currentSong
Set ocxPlayerMusic = createObject("WMPlayer.OCX")

Sub playOcxMusic(currentSong)
    ocxPlayerMusic.URL = musicDir & currentSong
    ocxPlayerMusic.settings.volume = oldMusicVol*100
	ocxPlayerMusic.controls.play
	playerMusicTime.enabled = 1
End Sub

Sub playerMusicTime_Timer
    If ocxPlayerMusic.playState <> 3 And ocxPlayerMusic.playstate <> 9 Then 
        ocxPlayerVo.controls.stop
		If raceStart = 0 Then generateSongNumber(4)
		playerMusicTime.enabled = 0
	End If
End Sub

Dim fadeStep, fadeVol, fadeLoop
Sub fadeMusic_Timer
	fadeLoop = fadeLoop + 1
	fadeStep = musicVol*10
	fadeVol = fadeStep*fadeLoop
	ocxPlayerMusic.settings.volume = (musicVol*100)-fadeVol
	If fadeLoop = 10 Then
		fadeLoop = 0
		ocxPlayerMusic.controls.stop
		playerMusicTime.enabled = 0
		fadeMusic.enabled = 0
	End If
End Sub

Dim fadeStep1, fadeVol1, fadeLoop1, duckDirection
Sub duck_Timer
	fadeLoop1 = fadeLoop1 + 1
	fadeStep1 = oldMusicVol*10
	fadeVol1 = fadeStep1*fadeLoop1

	If duckDirection = 0 Then
		ocxPlayerMusic.settings.volume = (musicVol*100)-fadeVol1
	Else
		ocxPlayerMusic.settings.volume = (musicVol*100)-(fadeStep1*8)+fadeVol1
	End If

	If fadeLoop1 = 8 Then
		fadeLoop1 = 0
		duck.enabled = 0
	End If
End Sub


'*****************************************************Supporting Code Written By Others*************************************  

'*********************************************
'  VR Plunger Code From Flash by Bord and Roth
'*********************************************

Sub TimerVRPlunger_Timer
  If PinCab_Shooter.Y < -500 then
       PinCab_Shooter.Y = PinCab_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger1_Timer
  PinCab_Shooter.Y = -500 + (5* Plunger.Position) -20
End Sub

'************************************************************************
'                         Ball Control - 3 Axis
'************************************************************************

Dim Cup, Cdown, Cleft, Cright, Zup, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti
 
bcboost = 1 'Do Not Change - default setting
bcvel = 4 'Controls the speed of the ball movement
bcyveloffset = -0.014 'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3 'Boost multiplier to ball veloctiy (toggled with the B key)

Sub BallControl_Timer()
    If Contball and ContBallInPlay then
        If Cright = 1 Then
            ControlBall.velx = bcvel*bcboost
          ElseIf Cleft = 1 Then
            ControlBall.velx = -bcvel*bcboost
          Else
            ControlBall.velx = 0
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

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function AudioPan(TableObj)	'Calculates the pan for a TableObj based on the X position on the table. "table1" is the name of the table.  New AudioPan algorithm for accurate stereo pan positioning.
    Dim tmp
    If PFOption=1 Then tmp = TableObj.x * 2 / table1.width-1
	If PFOption=2 Then tmp = TableObj.y * 2 / table1.height-1
	If tmp < 0 Then
		AudioPan = -((0.8745898957*(ABS(tmp)^12.78313661)) + (0.1264569796*(ABS(tmp)^1.000771219)))
	Else
		AudioPan = (0.8745898957*(ABS(tmp)^12.78313661)) + (0.1264569796*(ABS(tmp)^1.000771219))
	End If
End Function

Function xGain(TableObj)
'xGain algorithm calculates a PlaySound Volume parameter multiplier to provide a Constant Power "pan".
'PFOption=1:  xGain = 1 at PF Left, xGain = 0.32931 (-3dB for PlaySound's volume parameter) at PF Center and xGain = 1 at PF Right.  Used for Left & Right stereo PF Speakers.
'PFOption=2:  xGain = 1 at PF Top, xGain = 0.32931 (-3dB for PlaySound's volume parameter) at PF Center and xGain = 1 at PF Bottom.  Used for Top & Bottom stereo PF Speakers.
	Dim tmp
    If PFOption=1 Then tmp = TableObj.x * 2 / table1.width-1
	If PFOption=2 Then tmp = TableObj.y * 2 / table1.height-1
	If tmp < 0 Then
	xGain = 0.3293074856*EXP(-0.9652695455*tmp^3 - 2.452909811*tmp^2 - 2.597701999*tmp)
	Else
	xGain = 0.3293074856*EXP(-0.9652695455*-tmp^3 - 2.452909811*-tmp^2 - 2.597701999*-tmp)
	End If
End Function

Function XVol(tableobj)
'XVol algorithm calculates a PlaySound Volume parameter multiplier for a tableobj based on its X table position to provide a Constant Power "pan".
'XVol = 1 at PF Left, XVol = 0.32931 (-3dB for PlaySound's volume parameter) at PF Center and XVol = 0 at PF Right
Dim tmpx
	If PFOption = 3 Then
		tmpx = tableobj.x * 2 / table1.width-1
		XVol = 0.3293074856*EXP(-0.9652695455*tmpx^3 - 2.452909811*tmpx^2 - 2.597701999*tmpx)
	End If
End Function

Function YVol(tableobj)
'YVol algorithm calculates a PlaySound Volume parameter multiplier for a tableobj based on its Y table position to provide a Constant Power "fade".
'YVol = 1 at PF Top, YVol = 0.32931 (-3dB for PlaySound's volume parameter) at PF Center and YVol = 0 at PF Bottom
Dim tmpy
	If PFOption = 3 Then
		tmpy = tableobj.y * 2 / table1.height-1
		YVol = 0.3293074856*EXP(-0.9652695455*tmpy^3 - 2.452909811*tmpy^2 - 2.597701999*tmpy)
	End If
'	TB2.text = "yVol=" & Round(yVol,4)
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

'******************************************************
'      JP's VP10 Rolling Sounds - Modified by Whirlwind
'******************************************************

'******************************************
' Explanation of the rolling sound routine
'******************************************

' ball rolling sounds are played based on the ball speed and position
' the routine checks first for deleted balls and stops the rolling sound.
' The For loop goes through all the balls on the table and checks for the ball speed and 
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped.

'New algorithms added to make sounds for TopArch Hits, Arch Rolls, ball bounces and glass hits. 
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yeilds an inverse response.

Const tnob = 2 ' total number of balls

ReDim rolling(tnob)
InitRolling

ReDim ArchRolling(tnob)
InitArchRolling

Dim ArchHit
Sub TopArch_Hit						
	ArchHit = 1
	ArchTimer.Enabled = True
End Sub

Dim archCount
Sub ArchTimer_Timer
	archCount = archCount + 1
	If archCount = 1 Then
		archCount = 0
		ArchTimer.enabled = False
		If ArchTimer2.enabled = False Then ArchTimer2.enabled = True
	End If
End Sub

Dim archCount2
Sub ArchTimer2_Timer
	archCount2 = archCount2 + 1
	If archCount2 = 1 Then
		archCount2 = 0
		ArchTimer2.enabled = False
	End If
End Sub

Sub NotOnArch_Hit
	ArchHit = 0
End Sub

Sub NotOnArch2_Hit
	ArchHit = 0
End Sub

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub InitArchRolling
	Dim i
	For i = 0 to tnob
		ArchRolling(i) = False
	Next
End Sub

Sub RollingTimer_Timer()
	Dim BOT, b, paSub
	BOT = GetBalls
	paSub=35000	'Playsound pitch adder for subway rolling ball sound
	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		StopSound("BallrollingA" & b)
		StopSound("BallrollingB" & b)
		StopSound("BallrollingC" & b)
		StopSound("BallrollingD" & b)
	Next
	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub
	' play the rolling sound for each ball
	For b = 0 to UBound(BOT)

'	Ball Rolling sounds
'**********************
'	Ball=50 units=1.0625".  One unit = 0.02125"  Ball.z is ball center.
'	A ball in Adrian's saucer has a Z of -3.  Use <-5 for subway sounds.
	If PFOption = 1 or PFOption = 2 Then
		If BallVel(BOT(b)) > 1 AND BOT(b).z > 10 and BOT(b).z <26 Then	'Ball on playfield
			rolling(b) = True
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		ElseIf BallVel(BOT(b)) > 1 AND BOT(b).z < -5 Then	'Ball on subway
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b))+paSub, 1, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.			
		ElseIf rolling(b) = True Then
			StopSound("BallrollingA" & b)
			rolling(b) = False
		End If
	End If
	
	If PFOption = 3 Then
		If BallVel(BOT(b)) > 1 AND BOT(b).z > 10 and BOT(b).z < 26 Then	'Ball on playfield
			rolling(b) = True
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound("BallrollingB" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound("BallrollingC" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound("BallrollingD" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		ElseIf BallVel(BOT(b)) > 1 AND BOT(b).z < -5 Then	'Ball on subway
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b))+paSub, 1, 0, -1	'Top Left PF Speaker
			PlaySound("BallrollingB" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b))+paSub, 1, 0, -1	'Top Right PF Speaker
			PlaySound("BallrollingC" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b))+paSub, 1, 0,  1	'Bottom Left PF Speaker
			PlaySound("BallrollingD" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b))+paSub, 1, 0,  1	'Bottom Right PF Speaker
		ElseIf rolling(b) = True Then
			StopSound("BallrollingA" & b)		'Top Left PF Speaker
			StopSound("BallrollingB" & b)		'Top Right PF Speaker
			StopSound("BallrollingC" & b)		'Bottom Left PF Speaker
			StopSound("BallrollingD" & b)		'Bottom Right PF Speaker
			rolling(b) = False
		End If
	End If

'	Arch Hit and Arch Rolling sounds
'***********************************
	If PFOption = 1 or PFOption = 2 Then
		If BallVel(BOT(b)) > 1 And ArchHit =1 Then
			If ArchTimer2.enabled = 0 Then
				'PlaySound("ArchHit" & b), 0, (BallVel(BOT(b))/32)^5 * xGain(BOT(b)), AudioPan(BOT(b)), 0, (BallVel(BOT(b))/40)^7, 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
			End If	
			ArchRolling(b) = True																																											   
			PlaySound("ArchRollA" & b), -1, (BallVel(BOT(b))/40)^5 * xGain(BOT(b)), AudioPan(BOT(b)), 0, (BallVel(BOT(b))/40)^7, 1, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		Else
			If ArchRolling(b) = True Then
			StopSound("ArchRollA" & b)
			ArchRolling(b) = False
			End If
		End If
	End If
	
	If PFOption = 3 Then
		If BallVel(BOT(b)) > 1 And ArchHit =1 Then
			If ArchTimer2.enabled = 0 Then
			'	PlaySound("ArchHit" & b),   0, (BallVel(BOT(b))/32)^5 *    XVol(BOT(b))  *     YVol(BOT(b)),  -1, 0, (BallVel(BOT(b))/40)^5, 0, 0, -1	'Top Left PF Speaker
			'	PlaySound("ArchHit" & b),   0, (BallVel(BOT(b))/32)^5 * (1-XVol(BOT(b))) *     YVol(BOT(b)),   1, 0, (BallVel(BOT(b))/40)^5, 0, 0, -1	'Top Right PF Speaker
			'	PlaySound("ArchHit" & b),   0, (BallVel(BOT(b))/32)^5 *    XVol(BOT(b))  *  (1-YVol(BOT(b))), -1, 0, (BallVel(BOT(b))/40)^5, 0, 0,  1	'Bottom Left PF Speaker
			'	PlaySound("ArchHit" & b),   0, (BallVel(BOT(b))/32)^5 * (1-XVol(BOT(b))) *  (1-YVol(BOT(b))),  1, 0, (BallVel(BOT(b))/40)^5, 0, 0,  1	'Bottom Right PF Speaker
			End If																																							 
			ArchRolling(b) = True																																									  																																									 
			PlaySound("ArchRollA" & b), -1, (BallVel(BOT(b))/40)^5 *    XVol(BOT(b))  *     YVol(BOT(b)),  -1, 0, (BallVel(BOT(b))/40)^5, 1, 0, -1	'Top Left PF Speaker
			PlaySound("ArchRollB" & b), -1, (BallVel(BOT(b))/40)^5 * (1-XVol(BOT(b))) *     YVol(BOT(b)),   1, 0, (BallVel(BOT(b))/40)^5, 1, 0, -1	'Top Right PF Speaker
			PlaySound("ArchRollC" & b), -1, (BallVel(BOT(b))/40)^5 *    XVol(BOT(b))  *  (1-YVol(BOT(b))), -1, 0, (BallVel(BOT(b))/40)^5, 1, 0,  1	'Bottom Left PF Speaker
			PlaySound("ArchRollD" & b), -1, (BallVel(BOT(b))/40)^5 * (1-XVol(BOT(b))) *  (1-YVol(BOT(b))),  1, 0, (BallVel(BOT(b))/40)^5, 1, 0,  1	'Bottom Right PF Speaker
		Else
			If ArchRolling(b) = True Then
			StopSound("ArchRollA" & b)	'Top Left PF Speaker
			StopSound("ArchRollB" & b)	'Top Right PF Speaker
			StopSound("ArchRollC" & b)	'Bottom Left PF Speaker
			StopSound("ArchRollD" & b)	'Bottom Right PF Speaker
			ArchRolling(b) = False
			End If
		End If
	End If

'	Ball drop sounds
'*******************
'Four intensities of ball bounce sound files ranging from 1 to 4 bounces.  The number of bounces increases as the ball's downward Z velocity increases.
'A BOT(b).VelZ < -2 eliminates nuisance ball bounce sounds.

	If PFOption = 1 or PFOption = 2 Then
		If BOT(b).VelZ > -4 And BOT(b).VelZ < -2 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		ElseIf BOT(b).VelZ > -8 And BOT(b).VelZ < -4 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		ElseIf BOT(b).VelZ > -12 And BOT(b).VelZ < -8 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		ElseIf BOT(b).VelZ < -12 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		End If
	End If

	If PFOption = 3 Then
		If BOT(b).VelZ > -4 And BOT(b).VelZ < -2 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		ElseIf BOT(b).VelZ > -8 And BOT(b).VelZ < -4 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		ElseIf BOT(b).VelZ > -12 And BOT(b).VelZ < -8 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		ElseIf BOT(b).VelZ < -12 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		End If
	End If

'	Glass hit sounds
'*******************
'	Ball=50 units=1.0625".  Ball.z is ball center.  Balls are physically limited by the table's Top Glass Height property which is always parallel to the playfield
'	To ensure a ball can go high enough to trigger a glass hit the max ball.z is 25 units below Top Glass Height-5
'	Change GHB below if the glass is not parallel to the playfield 

	Dim GHT, GHB, PFL
	GHT = (table1.glassheight-5)*1.0625/50	'Glass height at top of real playfield in inches
	GHB = (table1.glassheight-5)*1.0625/50	'Glass height at bottom of real playfield in inches
	PFL = table1.height*1.0625/50		'Length of real playfield in inches


	If PFOption = 1 or PFOption = 2 Then
		If BOT(b).Z > (BOT(b).Y * ((GHT-GHB)/PFL)) + (GHB*50/1.0625) - BallSize/2 And BallinPlay => 1 Then
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		End If
	End If

	If PFOption = 3 Then
		If BOT(b).Z > (BOT(b).Y * ((GHT-GHB)/PFL)) + (GHB*50/1.0625) - Ballsize/2 And BallinPlay => 1 Then
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 0, 0, -1	'Top Left PF Speaker
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 0, 0, -1	'Top Right PF Speaker
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 0, 0,  1	'Bottom Left PF Speaker
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 0, 0,  1	'Bottom Right PF Speaker
		End If
	End If
	Next
End Sub

'*************Hit Sound Routines
'Eliminated the Hit Subs extra velocity criteria since the PlayFieldSoundAB command already incorporates the balls velocity.

Sub aMetalPins_Hit(idx)
	PlayFieldSoundAB "metalPinHit", 0, 1
End Sub

Sub aMetalsHigh_Hit(idx)
	PlayFieldSoundAB "metalHitHigh", 0, 1
End Sub

Sub aMetalsMedium_Hit(idx)
	PlayFieldSoundAB "metalHitMedium", 0, 1
End Sub

Sub aMetalsLow_Hit(idx)
	PlayFieldSoundAB "metalHitLow", 0, 1
End Sub

Sub aRubberBands_Hit(idx)
	If BallinPlay > 0 Then	'Eliminates the thump of Trough Ball Creation balls hitting walls 9 and 14 during table initiation
	PlayFieldSoundAB "rubberBand", 0, 1
	End If
End Sub

Sub aRubberWheel_hit(idx)
	PlayFieldSoundAB "rubberWheel", 0, 1
End Sub

Sub aRubberPosts_Hit(idx)
	PlayFieldSoundAB "rubberPost", 0, 1
End Sub

Sub aWoods_Hit(idx)
	PlayFieldSoundAB "wood", 0, 1
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlayFieldSoundAB "flip_hit_1", 0, 1
		Case 2 : PlayFieldSoundAB "flip_hit_2", 0, 1
		Case 3 : PlayFieldSoundAB "flip_hit_3", 0, 1
	End Select
End Sub

Sub ApronWalls_Hit(Index)
	Dim Volume
	If ActiveBall.vely < 0 Then	Volume = abs(ActiveBall.vely) / 1 Else Volume = ActiveBall.vely / 30	'The first bounce is -vely subsequent bounces are +vely
		If ActiveBall.z > 24 Then
			If PFOption = 1 Or PFOption = 2 Then
				PlaySound "ApronHit", 0, Volume * xGain(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
			End If
			If PFOption = 3 Then
				PlaySound "ApronHit", 0, Volume *    XVol(ActiveBall)  *    YVol(ActiveBall),  -1, 0, Pitch(ActiveBall), 0, 0, -1	'Top Left PF Speaker
				PlaySound "ApronHit", 0, Volume * (1-XVol(ActiveBall)) *    YVol(ActiveBall),   1, 0, Pitch(ActiveBall), 0, 0, -1	'Top Right PF Speaker
				PlaySound "ApronHit", 0, Volume *    XVol(ActiveBall)  * (1-YVol(ActiveBall)), -1, 0, Pitch(ActiveBall), 0, 0,  1	'Bottom Left PF Speaker
				PlaySound "ApronHit", 0, Volume * (1-XVol(ActiveBall)) * (1-YVol(ActiveBall)),  1, 0, Pitch(ActiveBall), 0, 0,  1	'Bottom Right PF Speaker
			End If
		End If
End Sub
	   
Sub Saucers_Hit(idx)
	If PFOption = 1 Or PFOption = 2 Then
		PlaySound "metalhit_medium", 0, 1 * xGain(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall)-11025, 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
	End If
	If PFOption = 3 Then
		PlaySound "metalhit_medium", 0, 1 *    XVol(ActiveBall)  *    YVol(ActiveBall),  -1, 0, Pitch(ActiveBall)-11025, 0, 0, -1	'Top Left PF Speaker
		PlaySound "metalhit_medium", 0, 1 * (1-XVol(ActiveBall)) *    YVol(ActiveBall),   1, 0, Pitch(ActiveBall)-11025, 0, 0, -1	'Top Right PF Speaker
		PlaySound "metalhit_medium", 0, 1 *    XVol(ActiveBall)  * (1-YVol(ActiveBall)), -1, 0, Pitch(ActiveBall)-11025, 0, 0,  1	'Bottom Left PF Speaker
		PlaySound "metalhit_medium", 0, 1 * (1-XVol(ActiveBall)) * (1-YVol(ActiveBall)),  1, 0, Pitch(ActiveBall)-11025, 0, 0,  1	'Bottom Right PF Speaker
	End If
End Sub																																				   																																				

'**********************
' Ball Collision Sound
'**********************

'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine.

'New algorithm for OnBallBallCollision
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yeilds an inverse response.

Sub OnBallBallCollision(ball1, ball2, velocity)
	If PFOption = 1 or PFOption = 2 Then
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) * xGain(ball1), AudioPan(ball1), 0, Pitch(ball1), 0, 1, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
	End If
	If PFOption = 3 Then
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) *    XVol(ball1)  *    YVol(ball1),  -1, 0, Pitch(ball1), 0, 1, -1	'Top Left Playfield Speaker
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) * (1-XVol(ball1)) *    YVol(ball1),   1, 0, Pitch(ball1), 0, 1, -1	'Top Right Playfield Speaker
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) *    XVol(ball1)  * (1-YVol(ball1)), -1, 0, Pitch(ball1), 0, 1,  1	'Bottom Left Playfield Speaker
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) * (1-XVol(ball1)) * (1-YVol(ball1)),  1, 0, Pitch(ball1), 0, 1,  1	'Bottom Right Playfield Speaker
	End If
End Sub

Sub PlayFieldSound (SoundName, Looper, TableObject, VolMult)
'Plays the sound of a table object at the table object's coordinates.
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yeilds an inverse response.

	If PFOption = 1 Or PFOption = 2 Then
		If Looper = -1 Then
			PlaySound SoundName&"A", Looper, VolMult * xGain(TableObject), AudioPan(TableObject), 0, 0, 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		End If
		If Looper = 0 Then
			PlaySound SoundName, Looper, VolMult * xGain(TableObject), AudioPan(TableObject), 0, 0, 0, 1, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		End If
	End If
	If PFOption = 3 Then
		If Looper = -1 Then
			PlaySound SoundName&"A", Looper, VolMult *    XVol(TableObject)  *    YVol(TableObject),  -1, 0, 0, 0, 0, -1	'Top Left PF Speaker
			PlaySound SoundName&"B", Looper, VolMult * (1-XVol(TableObject)) *    YVol(TableObject),   1, 0, 0, 0, 0, -1	'Top Right PF Speaker
			PlaySound SoundName&"C", Looper, VolMult *    XVol(TableObject)  * (1-YVol(TableObject)), -1, 0, 0, 0, 0,  1	'Bottom Left PF Speaker
			PlaySound SoundName&"D", Looper, VolMult * (1-XVol(TableObject)) * (1-YVol(TableObject)),  1, 0, 0, 0, 0,  1	'Bottom Right PF Speaker
		End If
		If Looper = 0 Then
			PlaySound SoundName, Looper, VolMult *    XVol(TableObject)  *    YVol(TableObject),  -1, 0, 0, 0, 1, -1	'Top Left PF Speaker
			PlaySound SoundName, Looper, VolMult * (1-XVol(TableObject)) *    YVol(TableObject),   1, 0, 0, 0, 1, -1	'Top Right PF Speaker
			PlaySound SoundName, Looper, VolMult *    XVol(TableObject)  * (1-YVol(TableObject)), -1, 0, 0, 0, 1,  1	'Bottom Left PF Speaker
			PlaySound SoundName, Looper, VolMult * (1-XVol(TableObject)) * (1-YVol(TableObject)),  1, 0, 0, 0, 1,  1	'Bottom Right PF Speaker
		End If
	End If
End Sub

Sub PlayFieldSoundAB (SoundName, Looper, VolMult)
'Plays the sound of a table object at the Active Ball's location.
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yeilds an inverse response.

	If PFOption = 1 Or PFOption = 2 Then
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) * xGain(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
	End If
	If PFOption = 3 Then
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) *    XVol(ActiveBall)  *    YVol(ActiveBall),  -1, 0, Pitch(ActiveBall), 0, 1, -1	'Top Left PF Speaker
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) * (1-XVol(ActiveBall)) *    YVol(ActiveBall),   1, 0, Pitch(ActiveBall), 0, 1, -1	'Top Right PF Speaker
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) *    XVol(ActiveBall)  * (1-YVol(ActiveBall)), -1, 0, Pitch(ActiveBall), 0, 1,  1	'Bottom Left PF Speaker
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) * (1-XVol(ActiveBall)) * (1-YVol(ActiveBall)),  1, 0, Pitch(ActiveBall), 0, 1,  1	'Bottom Right PF Speaker
	End If
End Sub

Sub PlayReelSound (SoundName, Pan)
Dim ReelVolAdj
ReelVolAdj = 0.1
'Provides a Constant Power Pan for the backglass reel sound volume to match the playfield's Constant Power Pan response
	If showDT = False Then	'-3dB for desktop mode
		If Pan = "Lpan" Then PlaySound SoundName, 0, ReelVolAdj * 1.00, -0.12, 0, 0, 0, 1, 0	'Panned 3/4 Left at 0dB * ReelVolAdj
		If Pan = "Mpan" Then PlaySound SoundName, 0, ReelVolAdj * 0.33,  0.00, 0, 0, 0, 1, 0	'Panned Middle at -3dB * ReelVolAdj
		If Pan = "Rpan" Then PlaySound SoundName, 0, ReelVolAdj * 1.00,  0.12, 0, 0, 0, 1, 0	'Panned 3/4 Right at 0dB * ReelVolAdj
	Else
		If Pan = "Lpan" Then PlaySound SoundName, 0, ReelVolAdj * 0.33, -0.12, 0, 0, 0, 1, 0	'Panned 3/4 Left at -3dB * ReelVolAdj
		If Pan = "Mpan" Then PlaySound SoundName, 0, ReelVolAdj * 0.11,  0.00, 0, 0, 0, 1, 0	'Panned Middle at -6dB * ReelVolAdj
		If Pan = "Rpan" Then PlaySound SoundName, 0, ReelVolAdj * 0.33,  0.12, 0, 0, 0, 1, 0	'Panned 3/4 Right at -3dB * ReelVolAdj
	End If
End Sub


'******************************************************
'****  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these 
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Flippers: 	https://www.youtube.com/watch?v=FWvM9_CdVHw
' Dampeners: 	https://www.youtube.com/watch?v=tqsxx48C6Pg
' Physics: 		https://www.youtube.com/watch?v=UcRMG-2svvE
'
'
' Note: BallMass must be set to 1. BallSize should be set to 50 (in other words the ball radius is 25) 
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 9.5-10.5 |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
' 
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 4-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |



'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level well need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. an object or point to tell the script where the tip of the flipper is at rest (EndPointLp, EndPointRp)
'	4. and, special scripting
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.  
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end 
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.3            | 0.3                   | 0.275                  | 0.275              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'


'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 80  '*****Important, this variable is an offset for the speed that the ball travels down the table to determine if the flippers have been fired 
'							'This is needed because the corrections to ball trajectory should only applied if the flippers have been fired and the ball is in the trigger zones.
'							'FlipAT is set to GameTime when the ball enters the flipper trigger zones and if GameTime is less than FlipAT + this time delay then changes to velocity
'							'and trajectory are applied.  If the flipper is fired before the ball enters the trigger zone then with this delay added to FlipAT the changes
'							'to tragectory and velocity will not be applied.  Also if the flipper is in the final 20 degrees changes to ball values will also not be applied.
'							'"Faster" tables will need a smaller value while "slower" tables will need a larger value to give the ball more time to get to the flipper. 		
'							'If this value is not set high enough the Flipper Velocity and Polarity corrections will NEVER be applied.
'
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -2.7        
'        AddPt "Polarity", 2, 0.33, -2.7
'        AddPt "Polarity", 3, 0.37, -2.7        
'        AddPt "Polarity", 4, 0.41, -2.7
'        AddPt "Polarity", 5, 0.45, -2.7
'        AddPt "Polarity", 6, 0.576,-2.7
'        AddPt "Polarity", 7, 0.66, -1.8
'        AddPt "Polarity", 8, 0.743, -0.5
'        AddPt "Polarity", 9, 0.81, -0.5
'        AddPt "Polarity", 10, 0.88, 0
'
'        addpt "Velocity", 0, 0,         1
'        addpt "Velocity", 1, 0.16, 1.06
'        addpt "Velocity", 2, 0.41,         1.05
'        addpt "Velocity", 3, 0.53,         1'0.982
'        addpt "Velocity", 4, 0.702, 0.968
'        addpt "Velocity", 5, 0.95,  0.968
'        addpt "Velocity", 6, 1.03,         0.945
'
'        LF.Object = LeftFlipper        
'        LF.EndPoint = EndPointLp  'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
'        RF.Object = RightFlipper
'        RF.EndPoint = EndPointRp
'End Sub
'
'

''*******************************************
'' Mid 80's
''
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 80
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -3.7        
'        AddPt "Polarity", 2, 0.33, -3.7
'        AddPt "Polarity", 3, 0.37, -3.7
'        AddPt "Polarity", 4, 0.41, -3.7
'        AddPt "Polarity", 5, 0.45, -3.7 
'        AddPt "Polarity", 6, 0.576,-3.7
'        AddPt "Polarity", 7, 0.66, -2.3
'        AddPt "Polarity", 8, 0.743, -1.5
'        AddPt "Polarity", 9, 0.81, -1
'        AddPt "Polarity", 10, 0.88, 0
'
'        addpt "Velocity", 0, 0,         1
'        addpt "Velocity", 1, 0.16, 1.06
'        addpt "Velocity", 2, 0.41,         1.05
'        addpt "Velocity", 3, 0.53,         1'0.982
'        addpt "Velocity", 4, 0.702, 0.968
'        addpt "Velocity", 5, 0.95,  0.968
'        addpt "Velocity", 6, 1.03,         0.945
'
'        LF.Object = LeftFlipper        
'        LF.EndPoint = EndPointLp
'        RF.Object = RightFlipper
'        RF.EndPoint = EndPointRp
'End Sub

'


'*******************************************
'  Late 80's early 90's
'
'Sub InitPolarity()
'	dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'	Next
'
'	AddPt "Polarity", 0, 0, 0
'	AddPt "Polarity", 1, 0.05, -5
'	AddPt "Polarity", 2, 0.4, -5
'	AddPt "Polarity", 3, 0.6, -4.5
'	AddPt "Polarity", 4, 0.65, -4.0
'	AddPt "Polarity", 5, 0.7, -3.5
'	AddPt "Polarity", 6, 0.75, -3.0
'	AddPt "Polarity", 7, 0.8, -2.5
'	AddPt "Polarity", 8, 0.85, -2.0
'	AddPt "Polarity", 9, 0.9,-1.5
'	AddPt "Polarity", 10, 0.95, -1.0
'	AddPt "Polarity", 11, 1, -0.5
'	AddPt "Polarity", 12, 1.1, 0
'	AddPt "Polarity", 13, 1.3, 0
'
'	addpt "Velocity", 0, 0,         1
'	addpt "Velocity", 1, 0.16, 1.06
'	addpt "Velocity", 2, 0.41,         1.05
'	addpt "Velocity", 3, 0.53,         1'0.982
'	addpt "Velocity", 4, 0.702, 0.968
'	addpt "Velocity", 5, 0.95,  0.968
'	addpt "Velocity", 6, 1.03,         0.945
'
'	LF.Object = LeftFlipper        
'	LF.EndPoint = EndPointLp
'	RF.Object = RightFlipper
'	RF.EndPoint = EndPointRp
'End Sub

'
'
'
'*******************************************
'' Early 90's and after
'
Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 60
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -5.5
        AddPt "Polarity", 2, 0.4, -5.5
        AddPt "Polarity", 3, 0.6, -5.0
        AddPt "Polarity", 4, 0.65, -4.5
        AddPt "Polarity", 5, 0.7, -4.0
        AddPt "Polarity", 6, 0.75, -3.5
        AddPt "Polarity", 7, 0.8, -3.0
        AddPt "Polarity", 8, 0.85, -2.5
        AddPt "Polarity", 9, 0.9,-2.0
        AddPt "Polarity", 10, 0.95, -1.5
        AddPt "Polarity", 11, 1, -1.0
        AddPt "Polarity", 12, 1.05, -0.5
        AddPt "Polarity", 13, 1.1, 0
        AddPt "Polarity", 14, 1.3, 0

        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
End Sub


' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.


'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

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

'********Triggered by a ball hitting the flipper trigger area
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

'*********Used to rotate flipper since this is removed from the key down for the flippers
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
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))   '% of flipper swing
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

'***********gameTime is a global variable of how long the game has progressed in ms
'***********This function lets the table know if the flipper has been fired
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

'***********This is turned on when a ball leaves the flipper trigger area
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
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)   'Set creates an object in VB
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
'**********Takes in more than one array and passes them to ShuffleArray
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
'**********Calculate ball speed as hypotenuse of velX/velY triangle
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
'**********Calculates the value of Y for an input x using the slope intercept equation
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
'********Interpolates the value for areas between the low and upper bounds sent to it
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
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
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

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 1   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque			'End of Swing Torque
EOSA = leftflipper.eostorqueangle		'End of Swing Torque Angle
Frampup = LeftFlipper.rampup			'Flipper Stregth Ramp Up
FElasticity = LeftFlipper.elasticity	'Flipper Elasticity
FReturn = LeftFlipper.return			'Flipper Return Strength
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 		'determines strength of coil field at start of swing
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16					'variable to check elapsed time
Const LiveElasticity = 0.45
Const SOSEM = 0.815						
Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
'Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
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
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	'What this code does is swing the flipper fast and make the flipper soft near its EOS to enable live catches.  It resets back to the base Table
	'settings once the flipper reaches the end of swing.  The code also makes the flipper starting ramp up high to simulate the stronger starting
	'coil strength and weaker at its EOS to simulate the weaker hold coil.

	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then  'If the flipper has started its swing, make it swing fast to nearly the end...
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 									'set flipper Ramp Up high
			Flipper.endangle = FEndAngle - 3*Dir						'swing to within 3 degrees of EOS
			Flipper.Elasticity = FElasticity * SOSEM					'Set the elasticity to the base table elasticity
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then   'If the flipper is fully swung and the flipper button is pressed then
		if FCount = 0 Then FCount = GameTime				'notes the Game Time to see if a live catch is possible

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew				'sets flipper EOS Torque Angle to .2 
			Flipper.eostorque = EOSTnew						'sets flipper EOS Torque to 1
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 'If the flipper has swung past it's end of swing then..
		If FState <> 3 Then													
			Flipper.eostorque = EOST        								'set the flipper EOS Torque back to the base table setting
			Flipper.eostorqueangle = EOSA									'set the flipper EOS Torque Angle back to the base table setting
			Flipper.rampup = Frampup										'set the flipper Ramp Up back to the base table setting
			Flipper.Elasticity = FElasticity								'set the flipper Elasticity back to the base table setting
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
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR



Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub

'*********This sets up the rubbers:
dim RubbersD : Set RubbersD = new Dampener     'Makes a Dampener Class Object 
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
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
'       		 Uses the LinearEnvelope function to calculate the correction based upon where it's value sits in relation
'       		 to the addpoint parameters set above.  Basically interpolates values between set points in a linear fashion
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
'       		 Uses the function BallSpeed's value at the point of impact/the active ball's velocity which is constantly being updated	
'				 RealCor is always less than 1 
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
'                Divides the desired CoR by the real COR to make a multiplier to correct velocity in x and y
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
'             	  Applies the coef to x and y velocities
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm) 'Rubberizer is handled here
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
'*********CoR is Coefficient of Restitution defined as "how much of the kinetic energy remains for the objects 
'to rebound from one another vs. how much is lost as heat, or work done deforming the objects 
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

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
'Sub RDampen_Timer
'	Cor.Update
'End Sub



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************




'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	lFlip.rotz = leftflipper.currentangle
	rFlip.rotz = rightflipper.currentangle
	FlipperLShadow.RotZ = LeftFlipper.currentangle
	FlipperRShadow.RotZ = RightFlipper.currentangle
	cattlegate.RotZ = bottomgate.currentAngle
End Sub

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* with at least as many objects each as there can be balls, including locked balls
' Ensure you have a timer with a -1 interval that is always running

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
'***If there are more than 3 lights that overlap in a playable area, exclude the less important lights***
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection, and more than 3 will cause "jumping" between which shadows are drawn
' The easiest way to keep track of this is to start with the group on the right slingshot and move anticlockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'G				H											^	E
'															^	B
'	A		 C												^	A
'	 B		D			your collection should look like	^	G		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F												^	H
'															^	C
'															^	D
'															^	F
'		When selecting them, you'd shift+click in this order^^^^^

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
dim gilvl:gilvl = 1
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

' *** Required Functions, enable these if they are not already present elswhere in your table
Function DistanceFast(x, y)
	dim ratio, ax, ay
	ax = abs(x)					'Get absolute value of each vector
	ay = abs(y)
	ratio = 1 / max(ax, ay)		'Create a ratio
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then			'Quickly determine if it's worth using
		DistanceFast = 1/ratio
	Else
		DistanceFast = 0
	End if
end Function

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'Dim PI: PI = 4*Atn(1)

'Function Atn2(dy, dx)
'	If dx > 0 Then
'		Atn2 = Atn(dy / dx)
'	ElseIf dx < 0 Then
'		If dy = 0 Then 
'			Atn2 = pi
'		Else
'			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
'		end if
'	ElseIf dx = 0 Then
'		if dy = 0 Then
'			Atn2 = 0
'		else
'			Atn2 = Sgn(dy) * pi / 2
'		end if
'	End If
'End Function
'
'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******
Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)

DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0

		currentShadowCount(iii) = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
		iii = iii + 1
	Next
	numberofsources = iii
	numberofsources_hold = iii
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, currentMat, AnotherSource, BOT, iii
	BOT = GetBalls

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(BOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If BOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + BallSize/10 + fovY
				objBallShadow(s).visible = 1

				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + fovY
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If BOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					BallShadowA(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				BallShadowA(s).Y = BOT(s).Y + Ballsize/10 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If BOT(s).Z < 30 Then 'And BOT(s).Y < (TableHeight - 200) Then 'Or BOT(s).Z > 105 Then		'Defining when and where (on the table) you can have dynamic shadows
				For iii = 0 to numberofsources - 1 
					LSd=DistanceFast((BOT(s).x-DSSources(iii)(0)),(BOT(s).y-DSSources(iii)(1)))	'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then						    'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = iii
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-ShadowOpacity)
							End If

						Elseif currentShadowCount(s) = 2 Then
																	'Same logic as 1 shadow, but twice
							currentMat = objrtx1(s).material
							AnotherSource = sourcenames(s)
							objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-DistanceFast((BOT(s).x-DSSources(AnotherSource)(0)),(BOT(s).y-DSSources(AnotherSource)(1))))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
	'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx2(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity2 = (falloff-LSd)/falloff
							objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2))
							End If
						end if
					Else
						currentShadowCount(s) = 0
						BallShadowA(s).Opacity = 100*AmbientBSFactor
					End If
				Next
			Else									'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************

'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2

sub TargetBouncer(aBall,defvalue)  'use defvalue to manipulate the bounce of the ball/target interaction
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
    elseif TargetBouncerEnabled = 2 and aball.z < 30 then
		'debug.print "velz: " & activeball.velz
		Select Case Int(Rnd * 4) + 1
			Case 1: zMultiplier = defvalue+1.1
			Case 2: zMultiplier = defvalue+1.05
			Case 3: zMultiplier = defvalue+0.7
			Case 4: zMultiplier = defvalue+0.3
		End Select
		aBall.velz = aBall.velz * zMultiplier * TargetBouncerFactor
		'debug.print "----> velz: " & activeball.velz
		'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

Sub TargetBounce_Hit
	TargetBouncer activeball, 1
End Sub

Sub ShadowHide_Hit()
Shadowblock.visible = false
End Sub

Sub ShadowShow_Hit()
Shadowblock.visible = true
End Sub



' Roth's drop target routine
'************************************************************************************************
'These are the hit subs for the DT walls that start things off and send the number of the wall hit to the DTHit sub
Sub DW001_Hit : DTHit 01 : DOF 113, DOFPulse : End Sub
Sub DW002_Hit : DTHit 02 : DOF 113, DOFPulse : End Sub
Sub DW003_Hit : DTHit 03 : DOF 113, DOFPulse : End Sub
Sub DW004_Hit : DTHit 04 : DOF 113, DOFPulse : End Sub

' sub to raise all DTs at once

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110             'in milliseconds
Const DTDropUpSpeed = 40            'in milliseconds
Const DTDropUnits = 44              'VP units primitive drops
Const DTDropUpUnits = 10            'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8                 'max degrees primitive rotates when hit
Const DTDropDelay = 20              'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40             'time in milliseconds before target drops back to normal up position after the solendoid fires to raise the target
Const DTBrickVel = 30               'velocity at which the target will brick, set to '0' to disable brick
Const DTEnableBrick = 1             'Set to 0 to disable bricking, 1 to enable bricking
Const DTDropSound = "DTDrop"        'Drop Target Drop sound
Const DTResetSound = "DTReset"      'Drop Target reset sound
Const DTHitSound = "Target_Hit_1"   'Drop Target Hit sound
Const DTMass = 0.2                  'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'                                DROP TARGETS FUNCTIONS
'******************************************************
'An array of objects for each DT of (primary wall, secondary wall, primitive, switch, animate variable)
Dim DT001, DT002, DT003, DT004
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

Set DT001 = (new DropTarget)(DW001, DOW001, drop001, 01, 0, false)
Set DT002 = (new DropTarget)(DW002, DOW002, drop002, 02, 0, false)
Set DT003 = (new DropTarget)(DW003, DOW003, drop003, 03, 0, false)
Set DT004 = (new DropTarget)(DW004, DOW004, drop004, 04, 0, false)


'An array of DT arrays
Dim DTArray
DTArray = Array(DT001, DT002, DT003, DT004)

' This function looks over the DTArray and polls the ID the target hit (ie 06) and returns its position in the array (ie 0)
Function DTArrayID(switch)
    Dim i
    For i = 0 to uBound(DTArray) 
		If DTArray(i).sw = switch Then DTArrayID = i: Exit Function 
    Next
End Function' This function looks over the DTArray and pulls the ID the target hit (ie 06))

Sub DTRaise(switch)
    Dim i
    i = DTArrayID(switch)
    DTArray(i).animate = -1 'this sets the last variable in the DT array to -1 from 0 to raise DT
    DoDTAnim
End Sub

Sub DTDrop(switch)
    Dim i
    i = DTArrayID(switch)
    DTArray(i).animate = 1 'this sets the last variable in the DT array to 1 from 0
    DoDTAnim
End Sub

Sub DTHit(switch)
    Dim i
    i = DTArrayID(switch) ' this sets i to be the position of the DT in the array DTArray

    DTArray(i).animate =  DTCheckBrick(Activeball, DTArray(i).prim) ' this sets the animate value (-1 raise, 1&4 drop, 0 do nothing, 3 bend backwards, 2 BRICK

    If DTArray(i).animate = 1 or DTArray(i).animate = 3 or DTArray(i).animate = 4 Then ' if the value from brick checking is not 2 then apply ball physics
	DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass
    End If
    DoDTAnim
End Sub

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

Sub DTAnim_Timer()  ' 10 ms timer
    DoDTAnim
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
    'debug.print "brick " & perpvel & " : " & paravel & " : " & perpvelafter & " : " & paravelafter

    If perpvel > 0 and perpvelafter <= 0 Then
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

	DTCheckBrick = 1
End Function

'**************************************How this all works********************************************
'This uses walls to register the hits and primitives that get dropped as well as an offsetwall which is a wall that is set behind the main wall.
'1) The wallTarget, offsetWallTarget, primitive, target number and animation value of 0 are put into arrays called DT006 - DT010
'2) A master array called DTArray contains all of the DT0xx arrays
'3) If a wall is hit then the position in the array of the wall hit is determined
'4) The animation value is calculated by DTCheckBrick
'5) If the animation value is 1,3 or 4 then DTBallPhysics applies velocity corrections to the active ball
'6) the animation sub is called and it passes each element of the DT array to the DTAnimate function
'  - if the animate value is 0 do nothing
'  - if the animate value is 1 or 4 then bend the dt back make the front wall not collidable and turn on the offset wall's collide and 
'    when the elapsed drop target delay time has passed change the animate value to 2
'  - if the animate value is two then drop the target and once it is dropped turn off the collide for the offset wall
'  - if the animate value is 3 then BRICK  bend the prim back and the forth but no drop 
'  - if the anumate value is -1 then raise the drop target past its resting point and drop back down and if a ball is over it kick the ball up in the air
'****************************************************************************************************

Sub DoDTAnim()
        Dim i
        For i = 0 to Ubound(DTArray)
	    DTArray(i).animate = DTAnimate(DTArray(i).primary, DTArray(i).secondary, DTArray(i).prim, DTArray(i).sw, DTArray(i).animate)
        Next
End Sub

' This is the function that animates the DT drop and raise
Function DTAnimate(primary, secondary, prim, switch,  animate)
        dim transz
        Dim animtime, rangle
        rangle = prim.rotz * 3.1416 / 180 ' number of radians

        DTAnimate = animate

        if animate = 0  Then  ' no action to be taken
                primary.uservalue = 0  ' primary.uservalue is used to keep track of gameTime
                DTAnimate = 0
                Exit Function
        Elseif primary.uservalue = 0 then 
                primary.uservalue = gametime ' sets primary.uservalue to game time for calculating how much time has elapsed
        end if

        animtime = gametime - primary.uservalue 'variable for elapsed time

        If (animate = 1 or animate = 4) and animtime < DTDropDelay Then 'if the time elapse is less than time for the dt to start to drop after impact
                primary.collidable = 0 'primary wall is not collidable
                If animate = 1 then secondary.collidable = 1 else secondary.collidable = 0 'animate 1 turns on offset wall collide and 4 turns offest collide off
                prim.rotx = DTMaxBend * cos(rangle) ' bend the primitive back the max value
                prim.roty = DTMaxBend * sin(rangle)
                DTAnimate = animate
                Exit Function
        elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then ' if the drop time has passed then
                primary.collidable = 0 ' primary wall is not collidable
                If animate = 1 then secondary.collidable = 1 else secondary.collidable = 0 'animate 1 turns on offset wall collide and 4 turns offest collide off
                prim.rotx = DTMaxBend * cos(rangle) ' bend the primitive back the max value
                prim.roty = DTMaxBend * sin(rangle)
                animate = 2 '**** sets animate to 2 for dropping the DT
                playFieldSound "DTDrop", 0, prim, 1  
        End If

        if animate = 2 Then ' DT drop time
                transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
                if prim.transz > -DTDropUnits  Then ' if not fully dropped then transz
                        prim.transz = transz
                end if

                prim.rotx = DTMaxBend * cos(rangle)/2
                prim.roty = DTMaxBend * sin(rangle)/2

                if prim.transz <= -DTDropUnits Then  ' if fully dropped then 
                        prim.transz = -DTDropUnits
                        secondary.collidable = 0 ' turn off collide for secondary wall now the rubber behind can be hit
                        'controller.Switch(Switch) = 1
                        primary.uservalue = 0 ' reset the time keeping value
                        DTAnimate = 0 ' turn off animation
                        Exit Function
                Else
                        DTAnimate = 2
                        Exit Function
                end If 
        End If

		'*** animate 3 is a brick!
        If animate = 3 and animtime < DTDropDelay Then ' if elapsed time is less than the drop time
                primary.collidable = 0 'turn off primary collide
                secondary.collidable = 1 'turn on secondary collide
                prim.rotx = DTMaxBend * cos(rangle) 'rotate back
                prim.roty = DTMaxBend * sin(rangle)
        elseif animate = 3 and animtime > DTDropDelay Then
                primary.collidable = 1 'turn on the primary collide
                secondary.collidable = 0 'turn off secondary collide
                prim.rotx = 0 'rotate back to start
                prim.roty = 0
                primary.uservalue = 0
                DTAnimate = 0
                Exit Function
        End If

        if animate = -1 Then ' If the value is -1 raise the DT past its resting point
                transz = (1 - (animtime)/DTDropUpSpeed) *  DTDropUnits * -1
                If prim.transz = -DTDropUnits Then
                        Dim BOT, b
                        BOT = GetBalls

                        For b = 0 to UBound(BOT) ' if a ball is over a DT that is rising, pop it up in the air with a vel of 20
                                If InRect(BOT(b).x,BOT(b).y,prim.x-25,prim.y-10,prim.x+25, prim.y-10,prim.x+25,prim.y+25,prim.x -25,prim.y+25) Then
                                        BOT(b).velz = 20
                                End If
                        Next
                End If

                if prim.transz < 0 Then
                        prim.transz = transz
                elseif transz > 0 then
                        prim.transz = transz
                end if

                if prim.transz > DTDropUpUnits then 'If the dt is at the top of its rise
                        DTAnimate = -2  ' set the dt animate to -2
                        prim.rotx = 0  'remove the rotation
                        prim.roty = 0
                        primary.uservalue = gametime
                end if
                primary.collidable = 0
                secondary.collidable = 1
                'controller.Switch(Switch) = 0

        End If

        if animate = -2 and animtime > DTRaiseDelay Then ' if the value is -2 then drop back down to resting height
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
'                DROP TARGET
'                SUPPORTING FUNCTIONS 
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


' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' *************************************************************************
Dim FlexDMD, DMDScene, FlexDMDHighQuality
FlexDMDHighQuality = False


Sub DMD_Init() 
	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")

	If FlexDMDHighQuality Then
		FlexDMD.TableFile = table1.Filename & ".vpx"
		FlexDMD.RenderMode = 2
		FlexDMD.Width = 256
		FlexDMD.Height = 64
		FlexDMD.Clear = True
		FlexDMD.GameName = cGameName
		FlexDMD.Run = True
		Set DMDScene = FlexDMD.NewGroup("Scene")
		DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.AGdmd")
		DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
		FlexDMD.LockRenderThread
		FlexDMD.Stage.AddActor DMDScene
		FlexDMD.UnlockRenderThread
	Else
		FlexDMD.TableFile = Table1.Filename & ".vpx"  'Needed
		FlexDMD.RenderMode = 2
		FlexDMD.Width = 128
		FlexDMD.Height = 32
		FlexDMD.Clear = True
		FlexDMD.GameName = cGameName
		FlexDMD.Run = True
		Set DMDScene = FlexDMD.NewGroup("Scene")
		DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.AGdmd")
		DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
		FlexDMD.LockRenderThread
		FlexDMD.Stage.AddActor DMDScene
		FlexDMD.UnlockRenderThread
	End If
End Sub

'***************************** Super Easy Method Attract by Scottacus
Dim seInit, seCount, seMinX, seMinY, seMaxX, seMaxY, seRangeX, seRangeY, semCount, seFlag, showNumber

Sub semAttract_timer
	If seInit = 0 Then seHelper
	Select Case showNumber
		Case 0:	If seFlag = 0 Then
					topDown 1, 1	'first number controlls on/off of Lights(1=on,0=off) second number controlls band of lights (0=off,1=On)
				Else
					bottomUp 1, 1
				End If
		Case 1: If seFlag = 0 Then
					topDown 1, 0
				Else
					bottomUp 0, 0
				End If
		Case 2:	If seFlag = 0 Then
					leftRight 1, 1
				Else
					rightLeft 1, 1
				End If
		Case 3: If seFlag = 0 Then
					leftRight 1, 0
				Else
					rightLeft 0, 0
				End If
		Case 4:	If seFlag = 0 Then
					bottomUp 1, 1
				Else
					topDown 0, 1
				End If
		Case 5: If seFlag = 0 Then
					topDown 1, 0
				Else
					bottomUp 0, 0
				End If
		Case 6: If seFlag = 0 Then
					rightLeft 1, 1
				Else
					leftRight 1, 1
				End If
		Case 7:	If seFlag = 0 Then
					topDown 1, 0
				Else
					topDown 0, 0
				End If
		Case 8: If seFlag = 0 Then
					topDown 1, 1
				Else
					bottomUp 1, 1
				End If
		Case 9: If seFlag = 0 Then
					leftRight 1, 0
				Else
					RightLeft 0, 0
				End If
		Case 10: If seFlag = 0 Then
					bottomUp 1, 0
				Else
					bottomUp 0, 0
				End If
		Case 11: If seFlag = 0 Then
					bottomUp 1, 0
				Else
					topDown 0, 0
				End If
		Case 12: If seFlag = 0 Then
					rightLeft 1, 0
				Else
					rightLeft 0, 0
				End If
		Case 13: If seFlag = 0 Then
					leftRight 1, 0
				Else
					rightLeft 0, 0
				End If
	End Select
	semCount = semCount + 1
	If semCount > 10 Then
		semCount = 0
		seFlag = seFlag + 1
		If seFlag > 1 Then 
			seFlag = 0
			showNumber = showNumber + 1
		End If
		If showNumber > 13 Then showNumber = 0
	End If
End Sub

Dim lChange, band
Sub topDown(lChange, band)
	For Each xx in attractLights
		If xx.y < seMinY + (seRangeY/10 * semCount) Then 
			If lChange = 1 Then 
				xx.state = 1
				If band = 1 Then If xx.y < seMinY + (seRangeY/10 * (semCount-1)) Then xx.state = 0
			Else	
				xx.state = 0
			End If
		End If
	Next
End Sub

Sub bottomUp(lChange, band)
	For Each xx in attractLights
		If xx.y > seMaxY - (seRangeY/10 * semCount) Then 
			If lChange = 1 Then 
				xx.state = 1
				If band = 1 Then If xx.y > seMaxY -(seRangeY/10 * (semCount-1)) Then xx.state = 0
			Else	
				xx.state = 0
			End If
		End If
	Next
End Sub

Sub leftRight(lChange, band)
	For Each xx in attractLights
		If xx.x < seMinX + (seRangeX/10 * semCount) Then 
			If lChange = 1 Then 
				xx.state = 1
				If band = 1 Then If xx.x < seMinX + (seRangeX/10 * (semCount-1)) Then xx.state = 0
			Else	 
				xx.state = 0
			End If
		End If
	Next
End Sub

Sub rightLeft(lChange, band)
	For Each xx in attractLights
		If xx.x > seMaxX - (seRangeX/10 * semCount) Then 
			If lChange = 1 Then 
				xx.state = 1
				If band = 1 Then If xx.x > seMaxX - (seRangeX/10 * (semCount-1)) Then xx.state = 0
			Else	
				xx.state = 0
			End If
		End If
	Next
End Sub

seMinX = 3000	'set these large enought so that there will be a value lower than them
seMiny = 3000
Sub seHelper
	seInit = 1
	For each xx in attractLights		'find the max and min x and y values
		If xx.x > seMaxX Then seMaxX = xx.x
		If xx.x < seMinX Then seMinX = xx.x
		If xx.y > seMaxY Then seMaxY = xx.y
		If xx.y < seMinY Then seMinY = xx.y
		seCount = seCount + 1
	Next
	seRangeX = seMaxX - seMinX		'calculate the range of x and y values
	seRangeY = seMaxY - seMinY
End Sub

Sub endSemAttract
	semAttract.enabled = 0
	semCount = 0
	For Each xx in attractLights: xx.state = 0: Next
End Sub
'******************************************************************************************************
