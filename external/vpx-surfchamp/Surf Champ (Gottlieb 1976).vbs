'*
'*
'*        Gottlieb's Surf Champ/Surfer (1976)
'*        Table build/scripted by Loserman76
'*        Image redraws/touchups/fixes by GNance
'*        
'*        
'*
'*

option explicit
Randomize
ExecuteGlobal GetTextFile("core.vbs")

On Error Resume Next
ExecuteGlobal GetTextFile("Controller.vbs")
If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the scripts folder."
On Error Goto 0

Const cGameName = "SurfChamp_1976"

Const ShadowFlippersOn = true
Const ShadowBallOn = true

Const ShadowConfigFile = false




Dim Controller	' B2S
Dim B2SScore	' B2S Score Displayed
Dim B2SOn		'True/False if want backglass

Const HSFileName="SurfChamp_76VPX.txt"
Const B2STableName="SurfChamp_1976"
Const LMEMTableConfig="LMEMTables.txt"
Const LMEMShadowConfig="LMEMShadows.txt"
Dim EnableBallShadow
Dim EnableFlipperShadow

'* this value adjusts score motor behavior - 0 allows you to continue scoring while the score motor is running - 1 sets score motor to behave more like a real EM
Const ScoreMotorAdjustment=1

'* this is a debug setting to use an older scoring routine vs a newer score routine - don't change this value
Const ScoreAdditionAdjustment=1

'* this controls whether you hear bells (0) or chimes (1) when scoring
Const ChimesOn=1

dim ScoreChecker
dim CheckAllScores
dim sortscores(4)
dim sortplayers(4)
Dim TextStr,TextStr2
Dim xx,LStep,RStep
Dim i
Dim obj
Dim bgpos
Dim dooralreadyopen
Dim TargetSpecialLit
Dim Points210counter
Dim Points500counter
Dim Points1000counter
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
Dim AlternatingRelay

Dim Ones
Dim Tens
Dim Hundreds
Dim Thousands

Dim Player
Dim Players

Dim rst
Dim bonuscountdown
Dim BonusCounter
Dim TempMultiCounter
dim TempPlayerup
dim RotatorTemp

Dim bump1
Dim bump2
Dim bump3

Dim LastChime10
Dim LastChime100
Dim LastChime1000

Dim Score10
Dim Score100

Dim SequenceCompleteStars, SequenceCompleteTargets, Sequence115, TopTargets, LowerTargets, SpecialLightTracking, ScoreMotorStepper

Dim MotorRunning
Dim Replay1Table(15)
Dim Replay2Table(15)
Dim Replay3Table(15)
Dim ReplayTableSet
Dim ReplayLevel
Dim ReplayTableMax

Dim tKickerTCount

Dim HorseshoeCounter

Dim DropTargetDownCounter


Sub Table1_Init()
	If Table1.ShowDT = false then
		For each obj in DesktopCrap
			obj.visible=False
		next
	End If
	OperatorMenuBackdrop.image = "PostitBL"
	For XOpt = 1 to MaxOption
		Eval("OperatorOption"&XOpt).image = "PostitBL"
	next
		
	For XOpt = 1 to 256
		Eval("Option"&XOpt).image = "PostItBL"
	next
	
	LoadEM
	LoadLMEMConfig2
	HideOptions
	SetupReplayTables
	PlasticsOff
	BumpersOff

	HighScore=0
	MotorRunning=0
	HighScoreReward=3
	BallsPerGame=5
	ReplayLevel=1
	Credits=0
	loadhs
	if HighScore=0 then HighScore=50000

	TableTilted=false
	

	Match=int(Rnd*10)*10
	MatchReel.SetValue((Match/10)+1)

	CanPlayReel.SetValue(0)
	BallInPlayReel.SetValue(0)

	For each obj in PlayerHUDScores
		obj.state=0
	next

	For each obj in PlayerHuds
		obj.SetValue(0)
	next
	For each obj in PlayerScores
		obj.ResetToZero
	next
	For each obj in PlayerScoresOn
		obj.ResetToZero
	next
	for each obj in bottgate
		obj.isdropped=true
    next
	for each obj in MultiplierLights
		obj.state=0
	next
    bgpos=6


	Replay1=Replay1Table(ReplayLevel)
	Replay2=Replay2Table(ReplayLevel)
	Replay3=Replay3Table(ReplayLevel)
 	dooralreadyopen=0
 
'	ResetBalls
'	PlaySound "Plunger"
	For each obj in Flashers
		obj.Visible=false
	next
	TargetSpecialLit = 0
	Points210counter=0
	Points500counter=0

	Players=0
	RotatorTemp=1
	InProgress=false
	AlternatingRelay=0
	InstructCard.image="IC_"+FormatNumber(BallsPerGame,0)

	RefreshReplayCard
	GameOverReel.SetValue(1)
	TiltReel.SetValue(1)
'	Bumper1L.image="HotShotBumperUnlit"
	RolloverScore.SetValue(1)
	Bumper1Light.state=0
	Bumper1Light.Visible=false
	If B2SOn Then

		'Controller.B2SSetScore 6, HighScore
		Controller.B2SSetTilt 1

		
		if Match=0 then
			Controller.B2SSetMatch 100
		else
			Controller.B2SSetMatch Match
		end if
		Controller.B2SSetGameOver 1
		Controller.B2SSetCanPlay 0
		Controller.B2SSetPlayerUp 0
		Controller.B2SSetBallInPlay 0
		Controller.B2SSetData 81,0
		Controller.B2SSetData 82,0
		Controller.B2SSetData 83,0
		Controller.B2SSetData 84,0
		Controller.B2SSetScoreRolloverPlayer1 1
		Controller.B2SSetScoreRolloverPlayer2 0
		Controller.B2SSetScoreRolloverPlayer3 0
		Controller.B2SSetScoreRolloverPlayer4 0
		Controller.B2SSetCredits Credits
	End If

	for i=1 to 4
    player=i
		If B2SOn Then 
			Controller.B2SSetScorePlayer player, 0
		End If
	next
	bump1=1
	
	InitPauser5.enabled=true

	If Credits > 0 Then DOF 125, 1

End Sub

Sub Table1_exit()
	savehs
	SaveLMEMConfig
	SaveLMEMConfig2
	If B2SOn Then Controller.Stop
end sub

Sub Table1_KeyDown(ByVal keycode)

	' GNMOD
	if EnteringInitials then
		CollectInitials(keycode)
		exit sub
	end if


	if EnteringOptions then
		CollectOptions(keycode)
		exit sub
	end if

	if keycode = 8 then
		LeftKicker_hit
	end if

	if keycode = 9 then
		RightKicker_hit
	end if


	If keycode = PlungerKey Then
		Plunger.PullBack
		PlungerPulled = 1
	End If

	if keycode = LeftFlipperKey and InProgress = false then
		OperatorMenuTimer.Enabled = true
	end if
	' END GNMOD


	If keycode = LeftFlipperKey and InProgress=true and TableTilted=false Then
		LeftFlipper.RotateToEnd
		

		PlaySound "FlipperUp"
		DOF 101, 1
		PlaySound "buzzL",-1
	End If
    
	If keycode = RightFlipperKey  and InProgress=true and TableTilted=false Then
		RightFlipper.RotateToEnd
		

		PlaySound "FlipperUp"
		DOF 102, 1
		PlaySound "buzz",-1
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

	If keycode = AddCreditKey or keycode = 4 then
		If B2SOn Then
			'Controller.B2SSetScorePlayer6 HighScore
			
		End If

		playsound "coinin"
		AddSpecial2
	end if

   if keycode = 5 then 
		playsound "coinin"
		AddSpecial2
		keycode= StartGameKey
	end if

   if keycode = StartGameKey and Credits>0 and InProgress=true and Players>0 and Players<4 and BallInPlay<2 then
		Credits=Credits-1
		If Credits < 1 Then DOF 125, 0
		CreditsReel.SetValue(Credits)
		Players=Players+1
		CanPlayReel.SetValue(Players)
		playsound "click"
		If B2SOn Then
			Controller.B2SSetCanPlay Players
			Controller.B2SSetCredits Credits
			If Players=2 Then
				Controller.B2SSetScoreRolloverPlayer2 0
			End If
			If Players=3 Then
				Controller.B2SSetScoreRolloverPlayer3 0
			End If
			If Players=4 Then
				Controller.B2SSetScoreRolloverPlayer4 0
			End If

		End If
    end if

	if keycode=StartGameKey and Credits>0 and InProgress=false and Players=0 and EnteringOptions = 0 then
'GNMOD
		OperatorMenuTimer.Enabled = false
'END GNMOD
		Credits=Credits-1
		If Credits = 0 Then DOF 125, 0
		Players=1
		CreditsReel.SetValue(Credits)
		Players=1
		CanPlayReel.SetValue(Players)
		MatchReel.SetValue(0)
		GameOverReel.SetValue(0)
		Player=1
		
		playsound "StartupSequence"
		TempPlayerUp=Player
		PlayerUpRotator.enabled=true
		rst=0
		BallInPlay=1
		InProgress=true
		resettimer.enabled=true
		BonusMultiplier=1
		If B2SOn Then
			Controller.B2SSetTilt 0
			Controller.B2SSetGameOver 0
			Controller.B2SSetMatch 0
			Controller.B2SSetCredits Credits
			'Controller.B2SSetScore 6, HighScore
			Controller.B2SSetCanPlay 1
			Controller.B2SSetData 81,0
			Controller.B2SSetData 82,0
			Controller.B2SSetData 83,0
			Controller.B2SSetData 84,0
			Controller.B2SSetPlayerUp 1
			Controller.B2SSetData 81,1
			Controller.B2SSetBallInPlay BallInPlay
			Controller.B2SSetScoreRolloverPlayer1 0
		End If
		For each obj in PlayerScores
			obj.ResetToZero
		next
		For each obj in PlayerScoresOn
			obj.ResetToZero
		next
		For each obj in PlayerHuds
			obj.SetValue(0)
		next
		For each obj in PlayerHUDScores
			obj.state=0
		next
		RolloverScore.SetValue(0)
		PlayerHuds(Player-1).SetValue(1)
		If Table1.ShowDT = True then
			PlayerScores(Player-1).Visible=0
			PlayerScoresOn(Player-1).Visible=1
		end If
	end if


    
End Sub

Sub Table1_KeyUp(ByVal keycode)

	' GNMOD
	if EnteringInitials then
		exit sub
	end if

	If keycode = PlungerKey Then
		
		if PlungerPulled = 0 then
			exit sub
		end if
		
		PlaySound"plungerrelease"
		Plunger.Fire
	End If

	if keycode = LeftFlipperKey then
		OperatorMenuTimer.Enabled = false
	end if

	' END GNMOD    

    
	If keycode = LeftFlipperKey  and InProgress=true and TableTilted=false Then
		
		LeftFlipper.RotateToStart
		
		PlaySound "FlipperDown"
		DOF 101, 0
		StopSound "buzzL"
	End If
    
	If keycode = RightFlipperKey  and InProgress=true and TableTilted=false Then
		RightFlipper.RotateToStart
        

		PlaySound "FlipperDown"
		DOF 102, 0
		StopSound "buzz"
	End If

End Sub



Sub Drain_Hit()
	Drain.DestroyBall
	PlaySound "fx_drain"
	DOF 106, 2
	Pause4Bonustimer.enabled=1
		
End Sub

Sub Trigger1_Unhit()	
	DOF 123, 2	
	DOF 124, 2
End Sub

Sub Pause4Bonustimer_timer
	Pause4Bonustimer.enabled=0
	AddBonus
	
End Sub

'***********************
'     Flipper Logos
'***********************

Sub UpdateFlipperLogos_Timer
	LFlip.RotY = LeftFlipper.CurrentAngle
	RFlip.RotY = RightFlipper.CurrentAngle
	PGate.Rotz = (Gate.CurrentAngle*.75) + 25
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
End Sub

'***********************
' slingshots
'

Sub RightSlingShot_Slingshot
    PlaySound "right_slingshot", 0, 1, 0.05, 0.05
	DOF 104,2
    RSling0.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	AddScore 10
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSling0.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySound "left_slingshot",0,1,-0.05,0.05
	DOF 103,2
    LSling0.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	AddScore 10
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing0.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


'***********************************

Sub RubberScoreWall1_Hit()
	
	If TableTilted=false then
		AddScore(10)
	End if
	
End Sub

Sub RubberScoreWall2_Hit()
	
	If TableTilted=false then
		AddScore(10)
	End if
End Sub

Sub RubberScoreWall3_Hit()
	
	If TableTilted=false then
		AddScore(10)
	End if
End Sub

Sub RubberScoreWall4_Hit()
	
	If TableTilted=false then
		AddScore(10)
	End if
End Sub

'***********************************
Sub Bumper1_Hit
	If TableTilted=false then

		PlaySound "bumper1"
		DOF 105,2
		bump1 = 1
		If BallsPerGame=3 then
			AddScore(1000)
		Else
			AddScore(100)
		End If
    end if
    
End Sub

Sub Bumper2_Hit
	If TableTilted=false then

		PlaySound "bumper1"
		DOF 105,2
		bump1 = 1
		If BallsPerGame=3 then
			AddScore(1000)
		Else
			AddScore(100)
		End If
    end if
    
End Sub


'***********************************
Sub TriggerTop1_Hit()
	if TableTilted=false then
		PlaySound "sensor"
		If Number1.state=1 then
			IncreaseBonus
		end if
		SetMotor(500)
		IncreaseBonus
	End If
End Sub

Sub TriggerTop2_Hit()
	if TableTilted=false then
		PlaySound "sensor"
		If Number2.state=1 then
			IncreaseBonus
		end if
		SetMotor(500)
		IncreaseBonus
	End If
End Sub

Sub TriggerTop3_Hit()
	if TableTilted=false then
		PlaySound "sensor"
		If Number3.state=1 then
			IncreaseBonus
		end if
		SetMotor(500)
		IncreaseBonus
	End If
End Sub

Sub TriggerTop4_Hit()
	if TableTilted=false then
		PlaySound "sensor"
		If Number4.state=1 then
			IncreaseBonus
		end if
		SetMotor(500)
		IncreaseBonus
	End If
End Sub

Sub TriggerTop5_Hit()
	if TableTilted=false then
		PlaySound "sensor"
		If Number5.state=1 then
			IncreaseBonus
		end if
		SetMotor(500)
		IncreaseBonus
	End If
End Sub

Sub TriggerTop6_Hit()
	if TableTilted=false then
		PlaySound "sensor"
		SetMotor(500)
		if Number6.state=1 then
			if (Bonus2xLight.state=0) AND (Bonus3xLight.state=0) then
				BonusMultiplier=2
				Bonus2xLight.state=1
			else
				BonusMultiplier=3
				Bonus2xLight.state=0
				Bonus3xLight.state=1
			end if
			Number6.state=0
			Number7.state=0
		end if
	End If
End Sub

Sub TriggerTop7_Hit()
	if TableTilted=false then
		PlaySound "sensor"
		SetMotor(500)
		if Number7.state=1 then
			if (Bonus2xLight.state=0) AND (Bonus3xLight.state=0) then
				BonusMultiplier=2
				Bonus2xLight.state=1
			else
				BonusMultiplier=3
				Bonus2xLight.state=0
				Bonus3xLight.state=1
			end if
			Number6.state=0
			Number7.state=0
		end if
	End If
End Sub


Sub TriggerLeftInlane_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 114,2
		SetMotor(500)
	End If
End Sub

Sub TriggerRightInlane_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 115,2
		SetMotor(500)
	End If
End Sub


Sub TriggerLeftOutlane_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 113,2
		SetMotor(500)
		IncreaseBonus
		If LeftOutlaneLight.state=1 then
			ShootAgainLight.state=1
		end if
	End If
End Sub

Sub TriggerRightOutlane_Hit()
	If TableTilted=false then
		PlaySound "sensor"
		DOF 113,2
		SetMotor(500)
		IncreaseBonus
		If RightOutlaneLight.state=1 then
			AddSpecial
		end if
	End If
End Sub

'***********************************

Sub StarTrigger1_hit
	If TableTilted=false then
		AddScore(100)
		KickerLight5.state=1
		StarTriggerLight1.state=0
		CheckAllStars
	end if
end sub

Sub StarTrigger2_hit
	If TableTilted=false then
		AddScore(100)
		KickerLight4.state=1
		StarTriggerLight2.state=0
		CheckAllStars
	end if
end sub

Sub StarTrigger3_hit
	If TableTilted=false then
		AddScore(100)
		KickerLight3.state=1
		StarTriggerLight3.state=0
		CheckAllStars
	end if
end sub

Sub StarTrigger4_hit
	If TableTilted=false then
		AddScore(100)
		KickerLight2.state=1
		StarTriggerLight4.state=0
		CheckAllStars
	end if
end sub

Sub StarTrigger5_hit
	If TableTilted=false then
		AddScore(100)
		KickerLight1.state=1
		StarTriggerLight5.state=0
		CheckAllStars
	end if
end sub

Sub CheckAllStars

	if SequenceCompleteStars=true then
		Exit sub
	else
		if (KickerLight1.state=1) AND (KickerLight2.state=1) AND (KickerLight3.state=1) AND (KickerLight4.state=1) AND (KickerLight5.state=1) then
			SequenceCompleteStars=true
			Number6.state=1
			Number7.state=1
			SpinnerLight.state=1
		end if
	end if

end sub

'***********************************
Sub DropTarget1_hit
	if TableTilted=false then
		DropTarget1.isDropped=true
		SetMotor(500)
		IncreaseBonus
		DropTargetDownCounter=DropTargetDownCounter+1
		'CheckAllDrops
	end if
end sub

Sub DropTarget2_hit
	if TableTilted=false then
		DropTarget2.isDropped=true
		SetMotor(500)
		IncreaseBonus
		DropTargetDownCounter=DropTargetDownCounter+1
		'CheckAllDrops
	end if
end sub

Sub DropTarget3_hit
	if TableTilted=false then
		DropTarget3.isDropped=true
		SetMotor(500)
		IncreaseBonus
		DropTargetDownCounter=DropTargetDownCounter+1
		'CheckAllDrops
	end if
end sub

Sub DropTarget4_hit
	if TableTilted=false then
		DropTarget4.isDropped=true
		SetMotor(500)
		IncreaseBonus
		DropTargetDownCounter=DropTargetDownCounter+1
		'CheckAllDrops
	end if
end sub

Sub DropTarget5_hit
	if TableTilted=false then
		DropTarget5.isDropped=true
		SetMotor(500)
		IncreaseBonus
		DropTargetDownCounter=DropTargetDownCounter+1
		'CheckAllDrops
	end if
end sub

Sub CheckAllDropsTimer_timer

	If MotorRunning<>0 then
		exit sub
	end if

	if DropTargetDownCounter>4 then
		CheckAllDropsTimer.enabled=false
		SequenceCompleteTargets=true

		if SequenceCompleteStars=true then
			SetMotor(5000)
			ResetDrops
			LeftOutlaneLight.state=1
			RightOutlaneLight.state=1
		else
			CheckAllDropsTimer.enabled=true
		end if
	end if
	
end sub

Sub ResetDrops
	ResetDropsTimer.enabled=true
end sub

Sub ResetDrops2
	ResetDropsTimer2.enabled=true
end sub

Sub ResetDropsTimer_timer
	ResetDropsTimer.enabled=false
	for each obj in TopDropTargets
		obj.IsDropped=0
	next
	PlaySound"dropsup"
	SequenceCompleteTargets=false
	DropTargetDownCounter=0
	CheckAllDropsTimer.enabled=true
end Sub

Sub ResetDropsTimer2_timer
	ResetDropsTimer2.enabled=false
	for each obj in TopDropTargets
		obj.IsDropped=0
	next
	PlaySound"dropsup"
	SequenceCompleteTargets=false
	DropTargetDownCounter=0
	CheckAllDropsTimer.enabled=true
end Sub
'***********************************

sub Spinner1_Spin
	If TableTilted=false then
		IncreaseHorseshoeLights
		if SpinnerLight.state=1 then
			AddScore(100)
		else
			AddScore(10)
		end if
	end if
end sub

'***********************************
Sub LeftKicker_Hit()
	If TableTilted=false then
		KickerHold1.ENABLED = TRUE
	else
		tKickerTCount=0
		LeftKicker.timerenabled=true
	end if

end sub

Sub KickerHold1_timer
	If MotorRunning<>0 then
		exit sub
	end if
	KickerHold1.enabled=false
	if LeftKickerLight.state=1 then
		AddSpecial
	end if
	ScoreLeftKicker.enabled = TRUE
	tKickerTCount=0		
	
End Sub

Sub ScoreLeftKicker_timer
	if tKickerTCount>5 then
		ScoreLeftKicker.enabled=false
		tKickerTCount=0
		LeftKicker.timerenabled=true
	else
		tKickerTCount=tKickerTCount+1
		Select Case tKickerTCount
			case 1:
				if KickerLight1.state=1 then
					AddScore(1000)
				else
					AddScore(100)
				end if
			case 2:
				if KickerLight2.state=1 then
					AddScore(1000)
				else
					AddScore(100)
				end if
			case 3:
				if KickerLight3.state=1 then
					AddScore(1000)
				else
					AddScore(100)
				end if
			case 4:
				if KickerLight4.state=1 then
					AddScore(1000)
				else
					AddScore(100)
				end if
			case 5:
				if KickerLight5.state=1 then
					AddScore(1000)
				else
					AddScore(100)
				end if
		end select
	end if
end sub
				

Sub LeftKicker_timer

	tKickerTCount=tKickerTCount+1
	select case tKickerTCount
	case 1:
		LeftKickArm.rotz=15
		LeftKicker.Kick 235,10
		PlaySound"saucer"
		
	case 2:
		LeftKicker.TIMERENABLED = FALSE
		LeftKickArm.rotz=0
	end Select

end sub



'***********************************

Sub CloseGateTrigger_Hit()
	if dooralreadyopen=1 then
		closeg.enabled=true
	end if
End Sub


Sub AddSpecial()
	PlaySound"knocker"
	DOF 126,2
	Credits=Credits+1
	DOF 125,1
	if Credits>9 then Credits=9
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	CreditsReel.SetValue(Credits)
End Sub

Sub AddSpecial2()
	PlaySound"click"
	Credits=Credits+1
	DOF 125,1
	if Credits>9 then Credits=9
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	CreditsReel.SetValue(Credits)
End Sub

Sub IncreaseHorseshoeLights
	for each obj in HorseshoeLights
		obj.state=0
	next
	if BallsPerGame=3 then
		HorseshoeLights(3).state=1
	end if
	HorseshoeCounter=HorseshoeCounter+1
	if HorseshoeCounter>4 then
		HorseshoeCounter=0
	end if
	HorseshoeLights(HorseshoeCounter).state=1

end sub

Sub IncreaseBonus
	if TableTilted=false then
		BonusCounter=BonusCounter+1
		if BonusCounter>15 then
			BonusCounter=15
		end if
		For each obj in Bonus
			obj.state=0
		next
		If BonusCounter>10 then
			Bonus(10).state=1
			Bonus(BonusCounter-10).state=1
		else
			Bonus(BonusCounter).state=1
		end if
	end if
end sub

Sub AddBonus()
	ScoreMotorStepper=0
	bonuscountdown=bonuscounter
	If BonusMultiplier=1 then
		ScoreBonus.interval=135
	Elseif BonusMultiplier=2 then
		ScoreBonus.interval=425
	else
		ScoreBonus.interval=800
	end if
	ScoreBonus.enabled=true
End Sub

sub ScoreBonus_timer
	ScoreMotorStepper=ScoreMotorStepper+1
	If (ScoreMotorStepper>5) AND (BonusMultiplier=1) then
		ScoreMotorStepper=0
		Exit Sub
	end if

	if bonuscountdown<=0 then
		ScoreBonus.enabled=false
		ScoreBonus.interval=50
		ResetHorseshoeLights.enabled=true
		exit sub
	end if
	if BonusCountdown>10 then
		SetMotor2(1000*BonusMultiplier)
		Bonus(bonuscountdown-10).state=0
	else
		SetMotor2(1000*BonusMultiplier)
		Bonus(bonuscountdown).state=0
	end if
	If BonusMultiplier=2 then
		Bonus(0).state=1
	end if
'	If BonusMultiplier=1 then
'		ScoreBonus.interval=50
'	Else
'		ScoreBonus.interval=150
'	end if
	bonuscountdown=bonuscountdown-1

	if bonuscountdown>10 then
		Bonus(bonuscountdown-10).state=1
		exit sub
	end if
	if bonuscountdown>0 then
		Bonus(bonuscountdown).state=1
	end if

end sub



Sub ToggleAlternatingRelay


end sub

Sub AltRelayLights


end sub

Sub LightsOut

	
	Bumper1Light.State=0
	Bumper2Light.state=0

	Light1.State=0
	Light2.State=0
	Light3.State=0
	Light4.State=0
	Light5.State=0
	Light6.State=0
	Light7.State=0
	Light8.State=0
	Light9.State=0
	Light10.State=0

end sub

sub ResetHorseshoeLights_timer
	if HorseshoeCounter=0 then
		ResetHorseshoeLights.enabled=0
		NextBallDelay.enabled=true
		exit sub
	end if
	HorseshoeLights(HorseshoeCounter).state=0
	HorseshoeCounter=HorseShoeCounter-1
	If HorseshoeCounter<0 then
		HorseshoeCounter=4
	end if
	HorseshoeLights(HorseshoeCounter).state=1
end sub

Sub ResetBallDrops()
	ResetDrops2
	StarTriggerLight1.state=1
	StarTriggerLight2.state=1
	StarTriggerLight3.state=1
	StarTriggerLight4.state=1
	StarTriggerLight5.state=1
	KickerLight1.state=0
	KickerLight2.state=0
	KickerLight3.state=0
	KickerLight4.state=0
	KickerLight5.state=0
	SpinnerLight.state=0
	Bonus2xLight.state=0
	Bonus3xLight.state=0
	LeftOutlaneLight.state=0
	RightOutlaneLight.state=0
	Number6.state=0
	Number7.state=0
	SequenceCompleteStars=false
	BonusCounter=0
	If BallsPerGame=3 then
		HorseshoeLights(3).state=1
	end if
	
	
End Sub

Sub ResetBalls()

	ResetBallDrops
	If BallInPlay=BallsPerGame then
		Bonus2xLight.state=1
		BonusMultiplier=2
	Else
		BonusMultiplier=1
	end if

	BumpersOn
	TableTilted=false
	TiltReel.SetValue(0)
	If B2Son then
		Controller.B2SSetTilt 0
	end if

	PlasticsOn
	BumpersOn
	Ballrelease.CreateSizedBall 25
    Ballrelease.Kick 40,7 
	DOF 107, 2
	if dooralreadyopen=1 then
		closeg.enabled=true
	end if
	BallInPlayReel.SetValue(BallInPlay)
  
End Sub

 sub openg_timer
    bottgate(bgpos).isdropped=true
    bgpos=bgpos-1
    bottgate(bgpos).isdropped=false  
	primgate.RotY=30+(bgpos*10)
     if bgpos=0 then 
		playsound "postup"
		openg.enabled=false
 		dooralreadyopen=1
	end if

 end sub 

sub closeg_timer
    closeg.interval=10
    bottgate(bgpos).isdropped=true
    bgpos=bgpos+1
    bottgate(bgpos).isdropped=false   
    primgate.RotY=30+(bgpos*10)
 	if bgpos=6 then
 		closeg.enabled=false
 		dooralreadyopen=0
 	end if
end sub   

sub resettimer_timer
    rst=rst+1
    for i=1 to 4
 	If B2SOn Then 
		Controller.B2SSetScorePlayer i, 0
	End If    
	next   
    if rst=20 then
    playsound "StartBall1"
    end if
    if rst=24 then
    newgame
    resettimer.enabled=false
    end if
end sub

sub ScoreTargetsBonus
	TopTargets=1
	LowerTargets=7
	NewBonusTimer.enabled=true


end sub


sub NewBonusTimer_timer

	BumpersOff
	If TopTargets<19 then
		Select case TopTargets
			case 1,2,3,4,5
		
				if BallCollection(TopTargets-1).state=1 then
					AddScore(1000)
				else
					PlaySound"MotorPause"
				end if
				
			case 7,8,9,10,11
		
				if BallCollection(TopTargets-2).state=1 then
					AddScore(1000)
				else
					PlaySound"MotorPause"
				end if

			case 13,14,15,16,17
		
				if BallCollection(TopTargets-3).state=1 then
					AddScore(1000)
				else
					PlaySound"MotorPause"
				end if
				
			case 6,12,18
				
				BumpersOn
		end Select
		TopTargets=TopTargets+1
		exit sub
	end if

	NewBonusTimer.enabled=0
	NextBallDelay.enabled=true

end sub

sub NextBallDelay_timer()
	NextBallDelay.enabled=false
	nextball

end sub

sub newgame
	InProgress=true
	queuedscore=0
	for i = 1 to 4
		Score(i)=0
		Score100K(1)=0
		HighScorePaid(i)=false
		Replay1Paid(i)=false
		Replay2Paid(i)=false
		Replay3Paid(i)=false
		HighScorePaid(i)=false
	next
	If B2SOn Then
		Controller.B2SSetTilt 0
		Controller.B2SSetGameOver 0
		Controller.B2SSetMatch 0
'		Controller.B2SSetScorePlayer1 0
'		Controller.B2SSetScorePlayer2 0
'		Controller.B2SSetScorePlayer3 0
'		Controller.B2SSetScorePlayer4 0
		Controller.B2SSetBallInPlay BallInPlay
	End if
	AlternatingRelay=0
	For each obj in BallCollection
		obj.state=0
	next
	For each obj in NumberLights
		obj.state=0
	next

	LeftKickerLight.state=0
	Bumper1Light.state=1
	Bumper2Light.state=1
	For each obj in HorseshoeLights
		obj.state=0
	next
	ShootAgainLight.state=0
	SpecialLightTracking=0
	HorseshoeCounter=0
	BonusCounter=0
	HorseshoeLights(0).state=1
	if BallsPerGame=3 then
		HorseshoeLights(3).state=1
	end if
	ResetBalls
End sub

sub nextball
	If B2SOn Then
		Controller.B2SSetTilt 0
	End If
	If ShootAgainLight.state=1 then
		ShootAgainLight.state=0
	else
		Player=Player+1
	end if
	If Player>Players Then
		BallInPlay=BallInPlay+1
		If BallInPlay>BallsPerGame then
			PlaySound("MotorLeer")
			InProgress=false
			
			If B2SOn Then
				Controller.B2SSetGameOver 1
				Controller.B2SSetPlayerUp 0
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetBallInPlay 0
				Controller.B2SSetCanPlay 0
			End If
			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			BallInPlayReel.SetValue(0)
			CanPlayReel.SetValue(0)
			GameOverReel.SetValue(1)
			LeftFlipper.RotateToStart
			
			StopSound "buzzL"

			RightFlipper.RotateToStart
			
			StopSound "buzz"
			For each obj in PlayerHUDScores
				obj.state=0
			next

			LightsOut
			checkmatch
			CheckHighScore
			Players=0
			HighScoreTimer.interval=100
			HighScoreTimer.enabled=True
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
			end If
		Else
			Player=1
			If B2SOn Then
				Controller.B2SSetPlayerUp Player
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetData 80+Player,1
				Controller.B2SSetBallInPlay BallInPlay

			End If
			'PlaySound("RotateThruPlayers")
			TempPlayerUp=Player
			PlayerUpRotator.enabled=true
			PlayStartBall.enabled=true
			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
				PlayerHuds(Player-1).SetValue(1)
				PlayerHUDScores(Player-1).state=1
				PlayerScores(Player-1).visible=0
				PlayerScoresOn(Player-1).visible=1
			end If
			ResetBalls
		End If
	Else 
		If B2SOn Then
			Controller.B2SSetPlayerUp Player
			Controller.B2SSetData 81,0
			Controller.B2SSetData 82,0
			Controller.B2SSetData 83,0
			Controller.B2SSetData 84,0
			Controller.B2SSetData 80+Player,1
			Controller.B2SSetBallInPlay BallInPlay
		End If
		PlaySound("RotateThruPlayers")
		TempPlayerUp=Player
		PlayerUpRotator.enabled=true
		PlayStartBall.enabled=true
		For each obj in PlayerHuds
			obj.SetValue(0)
		next
		If Table1.ShowDT = True then
			For each obj in PlayerScores
					obj.visible=1
			Next
			For each obj in PlayerScoresOn
					obj.visible=0
			Next
			PlayerHuds(Player-1).SetValue(1)
			PlayerHUDScores(Player-1).state=1
			PlayerScores(Player-1).visible=0
			PlayerScoresOn(Player-1).visible=1
		end If
		ResetBalls
	End If

End sub

sub CheckHighScore
	Dim playertops
		dim si
	dim sj
	dim stemp
	dim stempplayers
	for i=1 to 4
		sortscores(i)=0
		sortplayers(i)=0
	next
	playertops=0
	for i = 1 to Players
		sortscores(i)=Score(i)
		sortplayers(i)=i
	next

	for si = 1 to Players
		for sj = 1 to Players-1
			if sortscores(sj)>sortscores(sj+1) then
				stemp=sortscores(sj+1)
				stempplayers=sortplayers(sj+1)
				sortscores(sj+1)=sortscores(sj)
				sortplayers(sj+1)=sortplayers(sj)
				sortscores(sj)=stemp
				sortplayers(sj)=stempplayers
			end if
		next
	next
	ScoreChecker=4
	CheckAllScores=1
	NewHighScore sortscores(ScoreChecker),sortplayers(ScoreChecker)
	savehs
end sub

sub checkmatch
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
	End if
	for i = 1 to Players
		if Match=(Score(i) mod 100) then
			AddSpecial
		end if
	next
end sub

Sub TiltTimer_Timer()
	if TiltCount > 0 then TiltCount = TiltCount - 1
	if TiltCount = 0 then
		TiltTimer.Enabled = False
	end if
end sub

Sub TiltIt()
		TiltCount = TiltCount + 1
		if TiltCount = 3 then
			TableTilted=True
			TiltReel.SetValue(1)
			PlasticsOff
			BumpersOff

			LeftFlipper.RotateToStart
			
			StopSound "buzzL"

			RightFlipper.RotateToStart
			
			StopSound "buzz"
			
			If B2Son then
				Controller.B2SSetTilt 1
			end if
		else
			TiltTimer.Interval = 500
			TiltTimer.Enabled = True
		end if
	
end sub

Sub PlayStartBall_timer()

	PlayStartBall.enabled=false
	PlaySound("StartBall1")
end sub

Sub PlayerUpRotator_timer()
		If RotatorTemp<5 then
			TempPlayerUp=TempPlayerUp+1
			If TempPlayerUp>4 then
				TempPlayerUp=1
			end if
			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
				PlayerHuds(TempPlayerUp-1).SetValue(1)
				PlayerHUDScores(TempPlayerUp-1).state=1
				PlayerScores(TempPlayerUp-1).visible=0
				PlayerScoresOn(TempPlayerUp-1).visible=1
			end If
			If B2SOn Then
				Controller.B2SSetPlayerUp TempPlayerUp
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetData 80+TempPlayerUp,1
			End If

		else
			if B2SOn then
				Controller.B2SSetPlayerUp Player
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetData 80+Player,1
			end if
			PlayerUpRotator.enabled=false
			RotatorTemp=1
			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
				PlayerHuds(Player-1).SetValue(1)
				PlayerHUDScores(Player-1).state=1
				PlayerScores(Player-1).visible=0
				PlayerScoresOn(Player-1).visible=1
			end If
		end if
		RotatorTemp=RotatorTemp+1

	
end sub

sub savehs
	' Based on Black's Highscore routines
	Dim FileObj
	Dim ScoreFile
	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & HSFileName,True)
		ScoreFile.WriteLine 0
		ScoreFile.WriteLine Credits
		scorefile.writeline BallsPerGame
		scorefile.writeline ReplayLevel

		for xx=1 to 5
			scorefile.writeline HSScore(xx)
		next
		for xx=1 to 5
			scorefile.writeline HSName(xx)
		next

		ScoreFile.Close
	Set ScoreFile=Nothing
	Set FileObj=Nothing
end sub

sub loadhs
    ' Based on Black's Highscore routines
	Dim FileObj
	Dim ScoreFile
    dim temp1
    dim temp2
	dim temp3
	dim temp4
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

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & HSFileName) then
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & HSFileName)
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		temp1=TextStr.ReadLine
		temp2=textstr.readline
		temp3=textstr.readline
		temp4=textstr.readline
		HighScore=cdbl(temp1)
		if HighScore<1 then
			temp6=textstr.readline
			temp7=textstr.readline
			temp8=textstr.readline
			temp9=textstr.readline
			temp10=textstr.readline
			temp11=textstr.readline
			temp12=textstr.readline
			temp13=textstr.readline
			temp14=textstr.readline
			temp15=textstr.readline
		end if

		TextStr.Close
	    
	    Credits=cdbl(temp2)
		BallsPerGame=cdbl(temp3)
		ReplayLevel=cdbl(temp4)

		if HighScore<1 then
			HSScore(1) = int(temp6)
			HSScore(2) = int(temp7)
			HSScore(3) = int(temp8)
			HSScore(4) = int(temp9)
			HSScore(5) = int(temp10)
			
			HSName(1) = temp11
			HSName(2) = temp12
			HSName(3) = temp13
			HSName(4) = temp14
			HSName(5) = temp15
		end if

		Set ScoreFile=Nothing
	    Set FileObj=Nothing
end sub

sub SaveLMEMConfig
	Dim FileObj
	Dim LMConfig
	dim temp1
	dim tempb2s
	tempb2s=0
	if B2SOn=true then
		tempb2s=1
	else
		tempb2s=0
	end if
	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	Set LMConfig=FileObj.CreateTextFile(UserDirectory & LMEMTableConfig,True)
	LMConfig.WriteLine tempb2s
	LMConfig.Close
	Set LMConfig=Nothing
	Set FileObj=Nothing

end Sub

sub LoadLMEMConfig
	Dim FileObj
	Dim LMConfig
	dim tempC
	dim tempb2s

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & LMEMTableConfig) then
		Exit Sub
	End if
	Set LMConfig=FileObj.GetFile(UserDirectory & LMEMTableConfig)
	Set TextStr2=LMConfig.OpenAsTextStream(1,0)
	If (TextStr2.AtEndOfStream=True) then
		Exit Sub
	End if
	tempC=TextStr2.ReadLine
	TextStr2.Close
	tempb2s=cdbl(tempC)
	if tempb2s=0 then
		B2SOn=false
	else
		B2SOn=true
	end if
	Set LMConfig=Nothing
	Set FileObj=Nothing
end sub

sub SaveLMEMConfig2
	If ShadowConfigFile=false then exit sub
	Dim FileObj
	Dim LMConfig2
	dim temp1
	dim temp2
	dim tempBS
	dim tempFS

	if EnableBallShadow=true then
		tempBS=1
	else
		tempBS=0
	end if
	if EnableFlipperShadow=true then
		tempFS=1
	else
		tempFS=0
	end if

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	Set LMConfig2=FileObj.CreateTextFile(UserDirectory & LMEMShadowConfig,True)
	LMConfig2.WriteLine tempBS
	LMConfig2.WriteLine tempFS
	LMConfig2.Close
	Set LMConfig2=Nothing
	Set FileObj=Nothing

end Sub

sub LoadLMEMConfig2
	If ShadowConfigFile=false then
		EnableBallShadow = ShadowBallOn
		BallShadowUpdate.enabled = ShadowBallOn
		EnableFlipperShadow = ShadowFlippersOn
		FlipperLSh.visible = ShadowFlippersOn
		FlipperRSh.visible = ShadowFlippersOn
		exit sub
	end if
	Dim FileObj
	Dim LMConfig2
	dim tempC
	dim tempD
	dim tempFS
	dim tempBS

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & LMEMShadowConfig) then
		Exit Sub
	End if
	Set LMConfig2=FileObj.GetFile(UserDirectory & LMEMShadowConfig)
	Set TextStr2=LMConfig2.OpenAsTextStream(1,0)
	If (TextStr2.AtEndOfStream=True) then
		Exit Sub
	End if
	tempC=TextStr2.ReadLine
	tempD=TextStr2.Readline
	TextStr2.Close
	tempBS=cdbl(tempC)
	tempFS=cdbl(tempD)
	if tempBS=0 then
		EnableBallShadow=false
		BallShadowUpdate.enabled=false
	else
		EnableBallShadow=true
	end if
	if tempFS=0 then
		EnableFlipperShadow=false
		FlipperLSh.visible=false
		FLipperRSh.visible=false
	else
		EnableFlipperShadow=true
	end if
	Set LMConfig2=Nothing
	Set FileObj=Nothing

end sub



Sub DisplayHighScore
'	Dim Digit1
'	Dim Digit2
'	Dim Digit3
'	Dim Digit4
'	Dim Digit5
'	Dim Digit6
'	Digit1=Mid(CStr(HighScore),1,1)
'	HS1.image="PostIt"+Digit1
'	Digit2=Mid(CStr(HighScore),2,1)
'	HS2.image="PostIt"+Digit2
'	Digit3=Mid(CStr(HighScore),3,1)
'	HS3.image="PostIt"+Digit3
'	Digit4=Mid(CStr(HighScore),4,1)
'	HS4.image="PostIt"+Digit4
'	Digit5=Mid(CStr(HighScore),5,1)
'	HS5.image="PostIt"+Digit5
'	Digit6=Mid(CStr(HighScore),6,1)
'	HS6.image="PostIt"+Digit6

end sub

sub InitPauser5_timer
		If B2SOn Then
			'Controller.B2SSetScore 6,HighScore
		End If
		DisplayHighScore
		CreditsReel.SetValue(Credits)
		InitPauser5.enabled=false
end sub

sub BumpersOff
'	Bumper1L.image="HotShotBumperUnlit"
	
	
	Bumper1Light.Visible=false
	Bumper2Light.Visible=false


end sub

sub BumpersOn
	Bumper1Light.Visible=true
	Bumper2Light.Visible=true
	

end sub

Sub ToggleBumper

end sub

Sub PlasticsOn
	For each obj in Flashers
		obj.Visible=true
	next

end sub

Sub PlasticsOff
	For each obj in Flashers
		obj.Visible=false
	next

end sub

Sub SetupReplayTables

	Replay1Table(1)=61000
	Replay1Table(2)=62000
	Replay1Table(3)=65000
	Replay1Table(4)=68000
	Replay1Table(5)=71000
	Replay1Table(6)=74000
	Replay1Table(7)=77000
	Replay1Table(8)=80000
	Replay1Table(9)=83000
	Replay1Table(10)=86000
	Replay1Table(11)=60000
	Replay1Table(12)=63000
	Replay1Table(13)=66000
	Replay1Table(14)=69000
	Replay1Table(15)=999000

	Replay2Table(1)=77000
	Replay2Table(2)=78000
	Replay2Table(3)=81000
	Replay2Table(4)=84000
	Replay2Table(5)=87000
	Replay2Table(6)=90000
	Replay2Table(7)=93000
	Replay2Table(8)=96000
	Replay2Table(9)=99000
	Replay2Table(10)=99000
	Replay2Table(11)=74000
	Replay2Table(12)=77000
	Replay2Table(13)=80000
	Replay2Table(14)=83000
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

	ReplayTableMax=10

end sub

Sub RefreshReplayCard
	Dim tempst1
	Dim tempst2
	
	tempst1=FormatNumber(BallsPerGame,0)
	tempst2=FormatNumber(ReplayLevel,0)

	ReplayCard.image = "SC" + tempst2
	Replay1=Replay1Table(ReplayLevel)
	Replay2=Replay2Table(ReplayLevel)
	Replay3=Replay3Table(ReplayLevel)	
end sub


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
			If MotorRunning<>1 And InProgress=true then
				queuedscore=queuedscore+y
			end if
		end Select
end sub

Sub SetMotor2(x)
	If MotorRunning<>1 And InProgress=true then
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
	
	
	If MotorRunning<>1 And InProgress=true then
		if queuedscore>=5000 then
			tempscore=5000
			queuedscore=queuedscore-5000
			SetMotor2(5000)
			exit sub
		end if
		if queuedscore>=4000 then
			tempscore=4000
			queuedscore=queuedscore-4000
			SetMotor2(4000)
			exit sub
		end if
				
		if queuedscore>=3000 then
			tempscore=3000
			queuedscore=queuedscore-3000
			SetMotor2(3000)
			exit sub
		end if
			
		if queuedscore>=2000 then
			tempscore=2000
			queuedscore=queuedscore-2000
			SetMotor2(2000)
			exit sub
		end if
			
		if queuedscore>=1000 then
			tempscore=1000
			queuedscore=queuedscore-1000
			SetMotor2(1000)
			exit sub
		end if

		if queuedscore>=500 then
			tempscore=500
			queuedscore=queuedscore-500
			SetMotor2(500)
			exit sub
		end if
		if queuedscore>=400 then
			tempscore=400
			queuedscore=queuedscore-400
			SetMotor2(400)
			exit sub
		end if
		if queuedscore>=300 then
			tempscore=300
			queuedscore=queuedscore-300
			SetMotor2(300)
			exit sub
		end if
		if queuedscore>=200 then
			tempscore=200
			queuedscore=queuedscore-200
			SetMotor2(200)
			exit sub
		end if
		if queuedscore>=100 then
			tempscore=100
			queuedscore=queuedscore-100
			SetMotor2(100)
			exit sub
		end if

		if queuedscore>=50 then
			tempscore=50
			queuedscore=queuedscore-50
			SetMotor2(50)
			exit sub
		end if
		if queuedscore>=40 then
			tempscore=40
			queuedscore=queuedscore-40
			SetMotor2(40)
			exit sub
		end if
		if queuedscore>=30 then
			tempscore=30
			queuedscore=queuedscore-30
			SetMotor2(30)
			exit sub
		end if
		if queuedscore>=20 then
			tempscore=20
			queuedscore=queuedscore-20
			SetMotor2(20)
			exit sub
		end if
		if queuedscore>=10 then
			tempscore=10
			queuedscore=queuedscore-10
			SetMotor2(10)
			exit sub
		end if


	End If


end Sub

Sub ScoreMotorTimer_Timer
	If MotorPosition > 0 Then
		Select Case MotorPosition
			Case 5,4,3,2:
				If MotorMode=1000 Then 
					AddScore(1000)
				end if
				if MotorMode=100 then
					AddScore(100)
				End If
				if MotorMode=10 then
					AddScore(10)
				End if
				MotorPosition=MotorPosition-1
			Case 1:
				If MotorMode=1000 Then 
					AddScore(1000)
				end if
				If MotorMode=100 then 
					AddScore(100)
				End If
				if MotorMode=10 then
					AddScore(10)
				End if
				MotorPosition=0:MotorRunning=0:BumpersOn
		End Select
	End If

End Sub


Sub AddScore(x)
	If TableTilted=true then exit sub
	Select Case ScoreAdditionAdjustment
		Case 0:
			AddScore1(x)
		Case 1:
			AddScore2(x)
	end Select

end sub


Sub AddScore1(x)
'	debugtext.text=score
	Select Case x
		Case 1:
			PlayChime(10)
			Score(Player)=Score(Player)+1
			
		Case 10:
			PlayChime(10)
			Score(Player)=Score(Player)+10
'			debugscore=debugscore+10
			ToggleAlternatingRelay
		Case 100:
			PlayChime(100)
			Score(Player)=Score(Player)+100
'			debugscore=debugscore+100
			
		Case 1000:
			PlayChime(1000)
			Score(Player)=Score(Player)+1000
'			debugscore=debugscore+1000
	End Select
	PlayerScores(Player-1).AddValue(x)
	PlayerScoresOn(Player-1).AddValue(x)
	If ScoreDisplay(Player)<100000 then
		ScoreDisplay(Player)=Score(Player)
	Else
		Score100K(Player)=Int(Score(Player)/100000)
		ScoreDisplay(Player)=Score(Player)-100000
	End If
	if Score(Player)=>100000 then
		RolloverScore.SetValue(1)
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
	If Score(Player)>Replay1 and Replay1Paid(Player)=false then
		Replay1Paid(Player)=True
		AddSpecial
	End If
	If Score(Player)>Replay2 and Replay2Paid(Player)=false then
		Replay2Paid(Player)=True
		AddSpecial
	End If
	If Score(Player)>Replay3 and Replay3Paid(Player)=false then
		Replay3Paid(Player)=True
		AddSpecial
	End If
'	ScoreText.text=debugscore
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
	if Score(Player)=>100000 then
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

	OldTestScore = OldScore
	NewTestScore = NewScore
	Do
		if OldTestScore < Replay1 and NewTestScore >= Replay1 then
			AddSpecial()
			NewTestScore = 0
		Elseif OldTestScore < Replay2 and NewTestScore >= Replay2 then
			AddSpecial()
			NewTestScore = 0
		Elseif OldTestScore < Replay3 and NewTestScore >= Replay3 then
			AddSpecial()
			NewTestScore = 0
		End if
		NewTestScore = NewTestScore - 100000
		OldTestScore = OldTestScore - 100000
	Loop While NewTestScore > 0

    OldScore = int(OldScore / 10)	' divide by 10 for games with fixed 0 in 1s position, by 1 for games with real 1s digits
    NewScore = int(NewScore / 10)	' divide by 10 for games with fixed 0 in 1s position, by 1 for games with real 1s digits
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore&", OldScore Mod 10="&OldScore Mod 10 & ", NewScore % 10="&NewScore Mod 10)

    if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(10)
		ToggleAlternatingRelay
    end if

    OldScore = int(OldScore / 10)
    NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
    if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(100)
		
    end if

    OldScore = int(OldScore / 10)
    NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
    if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(1000)
		
    end if

    OldScore = int(OldScore / 10)
    NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
    if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(1000)
    end if

	If B2SOn Then
		Controller.B2SSetScorePlayer Player, Score(Player)
	End If
'	EMReel1.SetValue Score(Player)
	PlayerScores(Player-1).AddValue(x)
	PlayerScoresOn(Player-1).AddValue(x)
End Sub



Sub PlayChime(x)
	if ChimesOn=0 then
		Select Case x
			Case 10
				If LastChime10=1 Then
					PlaySound SoundFXDOF("SpinACard_1_10_Point_Bell",153,DOFPulse,DOFChimes)
					LastChime10=0
				Else
					PlaySound SoundFXDOF("SpinACard_1_10_Point_Bell",153,DOFPulse,DOFChimes)
					LastChime10=1
				End If
			Case 100
				If LastChime100=1 Then
					PlaySound SoundFXDOF("SpinACard_1_100_Point_Bell",154,DOFPulse,DOFChimes)
					LastChime100=0
				Else
					PlaySound SoundFXDOF("SpinACard_1_100_Point_Bell",154,DOFPulse,DOFChimes)
					LastChime100=1
				End If
		
		End Select
	else
		Select Case x
			Case 10
				If LastChime10=1 Then
					PlaySound SoundFXDOF("SJ_Chime_10a",153,DOFPulse,DOFChimes)
					LastChime10=0
				Else
					PlaySound SoundFXDOF("SJ_Chime_10b",153,DOFPulse,DOFChimes)
					LastChime10=1
				End If
			Case 100
				If LastChime100=1 Then
					PlaySound SoundFXDOF("SJ_Chime_100a",154,DOFPulse,DOFChimes)
					LastChime100=0
				Else
					PlaySound SoundFXDOF("SJ_Chime_100b",154,DOFPulse,DOFChimes)
					LastChime100=1
				End If
			Case 1000
				If LastChime1000=1 Then
					PlaySound SoundFXDOF("SJ_Chime_1000a",155,DOFPulse,DOFChimes)
					LastChime1000=0
				Else
					PlaySound SoundFXDOF("SJ_Chime_1000b",155,DOFPulse,DOFChimes)
					LastChime1000=1
				End If
		End Select
	end if
End Sub

Sub HideOptions()

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
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
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


' ============================================================================================
' GNMOD - Multiple High Score Display and Collection
' ============================================================================================
Dim EnteringInitials		' Normally zero, set to non-zero to enter initials
EnteringInitials = 0

Dim PlungerPulled
PlungerPulled = 0

Dim SelectedChar			' character under the "cursor" when entering initials

Dim HSTimerCount			' Pass counter for HS timer, scores are cycled by the timer
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

HSName(1) = "AAA"
HSName(2) = "ZZZ"
HSName(3) = "XXX"
HSName(4) = "ABC"
HSName(5) = "BBB"

Sub HighScoreTimer_Timer
	
	if EnteringInitials then
		if HSTimerCount = 1 then
			SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
			HSTimerCount = 2
		else
			SetHSLine 3, InitialString
			HSTimerCount = 1
		end if
	elseif InProgress then
		SetHSLine 1, "HIGH SCORE1"
		SetHSLine 2, HSScore(1)
		SetHSLine 3, HSName(1)
		HSTimerCount = 5	' set so the highest score will show after the game is over
		HighScoreTimer.enabled=false
	elseif CheckAllScores then
		NewHighScore sortscores(ScoreChecker),sortplayers(ScoreChecker)

	else
		' cycle through high scores
		HighScoreTimer.interval=2000
		HSTimerCount = HSTimerCount + 1
		if HsTimerCount > 5 then
			HSTimerCount = 1
		End If
		SetHSLine 1, "HIGH SCORE"+FormatNumber(HSTimerCount,0)
		SetHSLine 2, HSScore(HSTimerCount)
		SetHSLine 3, HSName(HSTimerCount)
	end if
End Sub

Function GetHSChar(String, Index)
	dim ThisChar
	dim FileName
	ThisChar = Mid(String, Index, 1)
	FileName = "PostIt"
	if ThisChar = " " or ThisChar = "" then
		FileName = FileName & "BL"
	elseif ThisChar = "<" then
		FileName = FileName & "LT"
	elseif ThisChar = "_" then
		FileName = FileName & "SP"
	else
		FileName = FileName & ThisChar
	End If
	GetHSChar = FileName
End Function

Sub SetHsLine(LineNo, String)
	dim Letter
	dim ThisDigit
	dim ThisChar
	dim StrLen
	dim LetterLine
	dim Index
	dim StartHSArray
	dim EndHSArray
	dim LetterName
	dim xfor
	StartHSArray=array(0,1,12,22)
	EndHSArray=array(0,11,21,31)
	StrLen = len(string)
	Index = 1

	for xfor = StartHSArray(LineNo) to EndHSArray(LineNo)
		Eval("HS"&xfor).image = GetHSChar(String, Index)
		Index = Index + 1
	next

End Sub

Sub NewHighScore(NewScore, PlayNum)
	if NewScore > HSScore(5) then
		HighScoreTimer.interval = 500
		HSTimerCount = 1
		AlphaStringPos = 1		' start with first character "A"
		EnteringInitials = 1	' intercept the control keys while entering initials
		InitialString = ""		' initials entered so far, initialize to empty
		SetHSLine 1, "PLAYER "+FormatNumber(PlayNum,0)
		SetHSLine 2, "ENTER NAME"
		SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
		HSNewHigh = NewScore
		For xx=1 to HighScoreReward
			AddSpecial
		next
	End if
	ScoreChecker=ScoreChecker-1
	if ScoreChecker=0 then
		CheckAllScores=0
	end if
End Sub

Sub CollectInitials(keycode)
	If keycode = LeftFlipperKey Then
		' back up to previous character
		AlphaStringPos = AlphaStringPos - 1
		if AlphaStringPos < 1 then
			AlphaStringPos = len(AlphaString)		' handle wrap from beginning to end
			if InitialString = "" then
				' Skip the backspace if there are no characters to backspace over
				AlphaStringPos = AlphaStringPos - 1
			End if
		end if
		SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
		PlaySound "DropTargetDropped"
	elseif keycode = RightFlipperKey Then
		' advance to next character
		AlphaStringPos = AlphaStringPos + 1
		if AlphaStringPos > len(AlphaString) or (AlphaStringPos = len(AlphaString) and InitialString = "") then
			' Skip the backspace if there are no characters to backspace over
			AlphaStringPos = 1
		end if
		SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
		PlaySound "DropTargetDropped"
	elseif keycode = StartGameKey or keycode = PlungerKey Then
		SelectedChar = MID(AlphaString, AlphaStringPos, 1)
		if SelectedChar = "_" then
			InitialString = InitialString & " "
			PlaySound("Ding10")
		elseif SelectedChar = "<" then
			InitialString = MID(InitialString, 1, len(InitialString) - 1)
			if len(InitialString) = 0 then
				' If there are no more characters to back over, don't leave the < displayed
				AlphaStringPos = 1
			end if
			PlaySound("Ding100")
		else
			InitialString = InitialString & SelectedChar
			PlaySound("Ding10")
		end if
		if len(InitialString) < 3 then
			SetHSLine 3, InitialString & SelectedChar
		End If
	End If
	if len(InitialString) = 3 then
		' save the score
		for i = 5 to 1 step -1
			if i = 1 or (HSNewHigh > HSScore(i) and HSNewHigh <= HSScore(i - 1)) then
				' Replace the score at this location
				if i < 5 then
' MsgBox("Moving " & i & " to " & (i + 1))
					HSScore(i + 1) = HSScore(i)
					HSName(i + 1) = HSName(i)
				end if
' MsgBox("Saving initials " & InitialString & " to position " & i)
				EnteringInitials = 0
				HSScore(i) = HSNewHigh
				HSName(i) = InitialString
				HSTimerCount = 5
				HighScoreTimer_Timer
				HighScoreTimer.interval = 2000
				PlaySound("Ding1000")
				exit sub
			elseif i < 5 then
				' move the score in this slot down by 1, it's been exceeded by the new score
' MsgBox("Moving " & i & " to " & (i + 1))
				HSScore(i + 1) = HSScore(i)
				HSName(i + 1) = HSName(i)
			end if
		next
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
Const OptionLinesToMark="111000011"
Const OptionLine1="" 'do not use this line
Const OptionLine2="" 'do not use this line
Const OptionLine3="" 'do not use this line
Const OptionLine4=""
Const OptionLine5=""
Const OptionLine6=""
Const OptionLine7=""
Const OptionLine8="" 'do not use this line
Const OptionLine9="" 'do not use this line

Sub OperatorMenuTimer_Timer
	EnteringOptions = 1
	OperatorMenuTimer.enabled=false
	ShowOperatorMenu
end sub

sub ShowOperatorMenu
	OperatorMenuBackdrop.image = "OperatorMenu"

	OptionCHS = 0
	CurrentOption = 1
	DisplayAllOptions
	OperatorOption1.image = "BluePlus"
	SetHighScoreOption

End Sub

Sub DisplayAllOptions
	dim linecounter
	dim tempstring
	For linecounter = 1 to MaxOption
		tempstring=Eval("OptionLine"&linecounter)
		Select Case linecounter
			Case 1:
				tempstring=tempstring + FormatNumber(BallsPerGame,0)
				SetOptLine 1,tempstring
			Case 2:
				if Replay3Table(ReplayLevel)=999000 then
					tempstring = tempstring + FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0)
				else
					tempstring = tempstring + FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0) + "/" + FormatNumber(Replay3Table(ReplayLevel),0)
				end if
				SetOptLine 2,tempstring
			Case 3:
				If OptionCHS=0 then
					tempstring = "NO"
				else
					tempstring = "YES"
				end if
				SetOptLine 3,tempstring
			Case 4:
				SetOptLine 4, tempstring
				SetOptLine 5, tempstring
			Case 5:
				SetOptLine 6, tempstring
				SetOptLine 7, tempstring

			Case 6:
				SetOptLine 8, tempstring
				SetOptLine 9, tempstring
				
			Case 7:
				SetOptLine 10, tempstring
				SetOptLine 11, tempstring
			
			Case 8:
		
			Case 9:
			
	
		End Select
		
	next
end sub

sub MoveArrow
	do 
		CurrentOption = CurrentOption + 1
		If CurrentOption>Len(OptionLinesToMark) then
			CurrentOption=1
		end if
	loop until Mid(OptionLinesToMark,CurrentOption,1)="1"
end sub

sub CollectOptions(ByVal keycode)
	if Keycode = LeftFlipperKey then
		PlaySound "DropTargetDropped"
		For XOpt = 1 to MaxOption
			Eval("OperatorOption"&XOpt).image = "PostitBL"
		next
		MoveArrow
		if CurrentOption<8 then
			Eval("OperatorOption"&CurrentOption).image = "BluePlus"
		elseif CurrentOption=8 then
			Eval("OperatorOption"&CurrentOption).image = "GreenCheck"
		else
			Eval("OperatorOption"&CurrentOption).image = "RedX"
		end if
			
	elseif Keycode = RightFlipperKey then
		PlaySound "DropTargetDropped"
		if CurrentOption = 1 then
			If BallsPerGame = 3 then
				BallsPerGame = 5
			else
				BallsPerGame = 3
			end if
			DisplayAllOptions
		elseif CurrentOption = 2 then
			ReplayLevel=ReplayLevel+1
			If ReplayLevel>ReplayTableMax then
				ReplayLevel=1
			end if
			DisplayAllOptions
		elseif CurrentOption = 3 then
			if OptionCHS = 0 then
				OptionCHS = 1
				
			else
				OptionCHS = 0
				
			end if
			DisplayAllOptions
		elseif CurrentOption = 8 or CurrentOption = 9 then
				if OptionCHS=1 then
					HSScore(1) = 75000	
					HSScore(2) = 70000
					HSScore(3) = 60000
					HSScore(4) = 55000
					HSScore(5) = 50000

					HSName(1) = "AAA"
					HSName(2) = "ZZZ"
					HSName(3) = "XXX"
					HSName(4) = "ABC"
					HSName(5) = "BBB"
				end if
	
				if CurrentOption = 8 then
					savehs
				else
					loadhs
				end if
				OperatorMenuBackdrop.image = "PostitBL"
				For XOpt = 1 to MaxOption
					Eval("OperatorOption"&XOpt).image = "PostitBL"
				next
			
				For XOpt = 1 to 256
					Eval("Option"&XOpt).image = "PostItBL"
				next
				RefreshReplayCard
				InstructCard.image="IC_"+FormatNumber(BallsPerGame,0)
				EnteringOptions = 0

		end if
	end if
End Sub

Sub SetHighScoreOption
	
End Sub

Function GetOptChar(String, Index)
	dim ThisChar
	dim FileName
	ThisChar = Mid(String, Index, 1)
	FileName = "PostIt"
	if ThisChar = " " or ThisChar = "" then
		FileName = FileName & "BL"
	elseif ThisChar = "<" then
		FileName = FileName & "LT"
	elseif ThisChar = "_" then
		FileName = FileName & "SP"
	elseif ThisChar = "/" then
		FileName = FileName & "SL"
	elseif ThisChar = "," then
		FileName = FileName & "CM"
	else
		FileName = FileName & ThisChar
	End If
	GetOptChar = FileName
End Function

dim LineLengths(22)	' maximum number of lines
Sub SetOptLine(LineNo, String)
	Dim DispLen
    Dim StrLen
	dim xfor
	dim Letter
	dim ThisDigit
	dim ThisChar
	dim LetterLine
	dim Index
	dim LetterName
	StrLen = len(string)
	Index = 1

	StrLen = len(String)
    DispLen = StrLen
    if (DispLen < LineLengths(LineNo)) Then
        DispLen = LineLengths(LineNo)
    end If

	for xfor = StartingArray(LineNo) to StartingArray(LineNo) + DispLen
		Eval("Option"&xfor).image = GetOptChar(string, Index)
		Index = Index + 1
	next
	LineLengths(LineNo) = StrLen

End Sub

