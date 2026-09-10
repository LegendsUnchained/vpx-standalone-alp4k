Option Explicit

'*****************************************************************************************************
' Skyscraper, Bally 1934
' bthlonewolf
'
' Attribution: 
' Elevator_Door_Ding by T.I.B -- https://freesound.org/s/788698/ -- License: Creative Commons 0
'
' See table description/rules for more information
'
' Version History
' 0.9 - test
' 1.0 - initial release
' 	  - chamged trough to midpoint of outhole and scored balls (like real game)
'	  - added invis ramp to tilter to help settle ball after tilt
' 1.1 - fixed missing walls on some pins. Tuned wall around skyscraper.
'
' todo: 
' 
' 
' bugs:
'
'*****************************************************************************************************

On Error Resume Next
ExecuteGlobal GetTextFile("Controller.vbs")
If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the Scripts folder of Visual Pinball."
On Error Goto 0

Const cGameName = "Skyscraper"
Const BallSize = 19
Dim UseDebugWindow : UseDebugWindow = false

Dim EnableRetractPlunger
EnableRetractPlunger = false 'Change to true to enable retracting the plunger at a linear speed; wait for 1 second at the maximum position; move back towards the resting position; nice for button/key plungers

Dim Controller	' B2S
Dim B2SScore	' B2S Score Displayed
Dim UseB2S : UseB2S = 1

Dim IsFirstLoad : IsFirstLoad = True
'Dim IsGameStarting : IsGameStarting = False
Dim IsGameOver : IsGameOver = False
Dim IsGameReady : IsGameReady = False
Dim IsGameActive : IsGameActive = False
Dim IsWheelDone : IsWheelDone = False

Dim TiltWarnings : TiltWarnings = 1
Dim CurrentWarnings : CurrentWarnings = 0
Dim IsTilted : IsTilted = False
Dim TiltWarningTime
Dim tiltball 

Const StandardBallsPerGame = 10
Dim BallsPerGame : BallsPerGame = StandardBallsPerGame
Dim Score, BIP, BallsPlayed, BallsCollected, HighScore, DefaultHighScore

Dim EnableBallControl : EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys
Dim LightLevel : LightLevel = 0.7
Dim StandardBallBrightness : StandardBallBrightness = 255 ' this is overriden by day/night setting in table

' for resetting high score: hold start key for 5+ seconds
Dim StartKeyBeginTime
Dim StartKeyEndTime

' tracking ball locks
Dim IsTopKickerLoaded : IsTopKickerLoaded = False
Dim IsBottomKickerLoaded : IsBottomKickerLoaded = False
Dim IsBallInBallReleaseKicker : IsBallInBallReleaseKicker = False

' Skyscraper specific

dim isRedSpecial : isRedSpecial = False
dim isGreenSpecial : isGreenSpecial = False


' DOF CODES
Const DOFElevator = 110
Const DOFBeacon = 111

Sub Table1_exit()
	if UseB2S and isObject(Controller) then
		Controller.Stop
	end if
End sub

Sub Table1_Init()

	BIP = 0
	BallsPlayed = 0
	Score = 0
	HighScore = 0
	DefaultHighScore = 500
	DisableCheaters

	LoadEM
	Debug "init"
	Randomize

	HighScore=LoadValue(cGameName,"HighScore")
 	If HighScore="" then
 		HighScore=DefaultHighScore
 	Else
		HighScore=Cdbl(HighScore)
 	End If

	CreateTiltBall
	CreateStartingBalls
	SetWheelToRandomSpot

	B2S_SetGameOver
	IsGameOver = True
	AttractModeTimer.enabled = True
End Sub


dim newAngle ' test angles

Sub Table1_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then
        If EnableRetractPlunger Then
            Plunger.PullBackandRetract
        Else
		    Plunger.PullBack
        End If
		PlaySound "plungerpull",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	if keycode = MechanicalTilt then 
		SetMechTilt
	end If

	if keycode = LeftFlipperKey and LFBWheelStop and WheelTimer.enabled Then
		'force the wheel to stop
		WheelTimerCount = WheelTimerTarget
	end If

	if keycode = LeftMagnaSave Then
		'KickSpinner
		'WheelTimer.enabled = True 
		'StartWheel

		'newAngle = wheel.Roty + 1
		'if newAngle > 360 Then newAngle = 360 - newAngle
		'wheel.Roty = newAngle
		'debug "Roty: " & newAngle  & " Points: " & getPointsForAngle(newAngle)
		'SetWheelToRandomSpot
	end if

	if keycode = RightMagnaSave Then
		'KickSpinner
		'WheelTimer.enabled = True 
		'wheel.Roty = wheel.Roty - 1
		'debug "Roty: " & wheel.Roty 

		'test points for each angle
		'newAngle = wheel.Roty + 10
		'if newAngle > 360 Then newAngle = 360 - newAngle
		'wheel.Roty = newAngle
		'debug "Roty: " & newAngle  & " Points: " & getPointsForAngle(newAngle)

	end if


	'if keycode = RightMagnaSave Then
	'
	'end if

	if keycode = StartGameKey Then
		StartKeyBeginTime = Now	
	end If

	If (keycode = StartGameKey and IsGameOver = True) Then

		TurnOffAllLights

		IsGameOver = False
		IsGameReady = False

		BallsPlayed = 0
		BIP = 0
		Score = 0
		IsTilted = False
		HasCheated = False
		
		isRedSpecial = False
		isGreenSpecial = False
		
		ResetTilt
		
		BallsPerGame = StandardBallsPerGame
		StartGameTimer.enabled = True
		StartWheel

		B2S_GameStart
		B2S_UpdateHighScoreReel

		'IsGameReady = True

	End If

	if keycode = LeftMagnaSave Then
		IsLeftMagnaPressed = True
	end if

	if keycode = RightMagnaSave Then
		IsRightMagnaPressed = True
	end if

	If keycode = RightFlipperKey and not IsGameOver and IsGameReady and _
		BallsPlayed < BallsPerGame and not IsShooterLaneFull Then '  and IsWheelDone

		PlaySound SoundFX("popper_ball",DOFContactors), 0,1,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)

		if not IsBallInBallReleaseKicker Then
			CreateNewBall BallRelease

			AddBIP(1)
			BallsPlayed = BallsPlayed + 1
		end If

		BallRelease.Kick -90, 7, 7
		IsBallInBallReleaseKicker = False

		B2S_UpdateBallReel
		debug "RightFlip: BIP:" & BIP & " Played:" & BallsPlayed
	End If

	If keycode = LeftTiltKey Then
		Nudge 90, 2
		CheckManualTilt 90
	End If

	If keycode = RightTiltKey Then
		Nudge 270, 2
		CheckManualTilt 270
	End If

	If keycode = CenterTiltKey Then
		Nudge 0, 2
		CheckManualTilt 0
	End If

    ' Manual Ball Control
	If keycode = 46 Then	 				' C Key
		If EnableBallControl = 1 Then
			EnableBallControl = 0
		Else
			EnableBallControl = 1
		End If
	End If
    If EnableBallControl = 1 Then
		If keycode = 48 Then 				' B Key
			If BCboost = 1 Then
				BCboost = BCboostmulti
			Else
				BCboost = 1
			End If
		End If
		If keycode = 203 Then BCleft = 1	' Left Arrow
		If keycode = 200 Then BCup = 1		' Up Arrow
		If keycode = 208 Then BCdown = 1	' Down Arrow
		If keycode = 205 Then BCright = 1	' Right Arrow
	End If

End Sub


Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySound "plunger",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	If keycode = LeftFlipperKey Then

	End If

	If keycode = RightFlipperKey Then

	End If

	if keycode = StartGameKey Then
		StartKeyEndTime = Now
	
		if DateDiff("s",StartKeyBeginTime,StartKeyEndTime) >= 5 Then
			ResetHighScore
		end If
	end If

    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If

	if keycode = RightMagnaSave Then
		IsRightMagnaPressed = False
	end if

	if keycode = LeftMagnaSave Then
		IsLeftMagnaPressed = False
	end if

	if IsCheatEnabled then 
		if not IsRightMagnaPressed or not IsLeftMagnaPressed Then
			DisableCheaters
		end If
	end If

End Sub


dim attractCount : attractCount = 0
dim attractMode : attractMode = 0
dim attractModeBaseInterval : attractModeBaseInterval = 500
sub AttractModeTimer_Timer
	
	if attractCount = 0 Then
		ResetAllLights
	end If
	
	if attractMode = 0 or attractMode = 1 then
		AttractModeTimer.Interval = attractModeBaseInterval
		if attractCount = 1 Then il_100.State = 1
		if attractCount = 2 Then il_200.State = 1
		if attractCount = 3 Then il_300.State = 1
		if attractCount = 4 Then il_400.State = 1
		if attractCount = 5 Then il_500.State = 1
		if attractCount = 6 Then il_600.State = 1
		if attractCount = 7 Then il_700.State = 1
		if attractCount = 8 Then il_800.State = 1
		if attractCount = 9 Then il_900.State = 1
		if attractCount = 10 Then il_1000.State = 1

		if attractCount = 12 Then
			ResetAllLights
		end If

		if attractCount = 15 Then
			attractCount = 0
			attractMode = attractMode + 1
		end If
	end if

	if attractMode = 2 or attractMode = 3 then
		AttractModeTimer.Interval = attractModeBaseInterval
		if attractCount = 1 Then il_100.State = 1 : il_1000.State = 1 : end if
		if attractCount = 2 Then il_200.State = 1 : il_900.State = 1 : end if
		if attractCount = 3 Then il_300.State = 1 : il_800.State = 1 : end if
		if attractCount = 4 Then il_400.State = 1 : il_700.State = 1 : end if
		if attractCount = 5 Then il_500.State = 1 : il_600.State = 1 : end if

		if attractCount = 7 Then
			ResetAllLights
		end If

		if attractCount = 9 Then
			attractCount = 0
			attractMode = attractMode + 1
		end If
	end if

	if attractMode = 4 then
		AttractModeTimer.Interval = AttractModeTimer.Interval-20

		if attractCount < 20 then
			if attractCount mod 2 = 0 Then 
				il_100.State = 0 : il_300.State = 0 : il_500.State = 0 : il_700.State = 0 : il_900.State = 0
				il_200.State = 1 : il_400.State = 1 : il_600.State = 1 : il_800.State = 1 : il_1000.State = 1
			else 
				il_200.State = 0 : il_400.State = 0 : il_600.State = 0 : il_800.State = 0 : il_1000.State = 0
				il_100.State = 1 : il_300.State = 1 : il_500.State = 1 : il_700.State = 1 : il_900.State = 1
			end if
		end if

		if attractCount = 20 Then
				il_100.State = 0 : il_300.State = 0 : il_500.State = 0 : il_700.State = 0 : il_900.State = 0
				il_200.State = 0 : il_400.State = 0 : il_600.State = 0 : il_800.State = 0 : il_1000.State = 0
		end If

		if attractCount = 21 Then
			attractCount = 0
			attractMode = attractMode + 1
		end If
	end if

	'reset attract mode if needed
	if attractMode > 4 then attractMode = 0
	attractCount = attractCount + 1

end sub

sub ResetAllLights

	il_100.State = 0
	il_200.State = 0
	il_300.State = 0
	il_400.State = 0
	il_500.State = 0
	il_600.State = 0
	il_700.State = 0
	il_800.State = 0
	il_900.State = 0
	il_1000.State = 0

	'il_beacon.State = 2

	B2S_SetFloorsOff

end sub

sub TurnOffAllLights

	AttractModeTimer.enabled = False

	il_100.State = 0
	il_200.State = 0
	il_300.State = 0
	il_400.State = 0
	il_500.State = 0
	il_600.State = 0
	il_700.State = 0
	il_800.State = 0
	il_900.State = 0
	il_1000.State = 0

	il_beacon.State = 0

	B2S_SetFloorsOff
end sub


' ***** SHOOTER LANE CONTROL *****

dim shooterx1 : shooterx1 = 770
dim shooterx2: shooterx2 = 830
dim shootery1 : shootery1 = 550
dim shootery2 : shootery2 = 1770
dim MaxBallsInShooterLane: MaxBallsInShooterLane = 2

function BallsInShooterLane

	dim BOT, b, ballsInShooter
	ballsInShooter = 0

	BOT = GetBalls
	For b = 0 to UBound(BOT)
		'define rough area of shooter lane 
		if BOT(b).X > shooterx1 and BOT(b).X < shooterx2 and BOT(b).Y > shootery1 and BOT(b).Y < shootery2 Then
			ballsInShooter = ballsInShooter + 1
		end If
    Next

	BallsInShooterLane = ballsInShooter
end Function

function IsShooterLaneFull

	IsShooterLaneFull = False

	if BallsInShooterLane >= MaxBallsInShooterLane Then
		IsShooterLaneFull = True
	end If

end Function

Sub BallRelease_Hit()
	'ball fell back into kicker??
	IsBallInBallReleaseKicker = True
	
End Sub

' ***** END SHOOTER LANE CONTROL *****

' ***** SKYSCRAPER SPECIFIC *****

dim isBeaconLit : isBeaconLit = False

sub AfterTriggerActions
	AddBIP(-1)
	if CalcScore Then
		'if score changed, play elevator ding
		PlaySound "elevator_door_ding"
		DOF DOFElevator, DOFPulse
	end If
	B2S_UpdateScoreReel
	EndCheck
end Sub

sub t100_Hit
	liteLampsForScore1 1
	AfterTriggerActions
end Sub

sub t200_Hit
	liteLampsForScore1 2
	AfterTriggerActions
end Sub

sub t300_Hit
	liteLampsForScore1 3
	AfterTriggerActions
end Sub

sub t400_Hit
	liteLampsForScore1 4
	AfterTriggerActions
end Sub

sub t500_Hit
	liteLampsForScore1 5
	AfterTriggerActions
end Sub

sub t600_Hit
	liteLampsForScore1 6
	AfterTriggerActions
end Sub

sub t700_Hit
	liteLampsForScore1 7
	AfterTriggerActions
end Sub

sub t800_Hit
	liteLampsForScore1 8
	AfterTriggerActions
end Sub

sub t900_Hit
	liteLampsForScore1 9
	AfterTriggerActions
end Sub

sub t1000_Hit
	liteLampsForScore1 10
	AfterTriggerActions
end Sub

sub t1500_Hit
	liteLampsForScore1 11
	
	if not isBeaconLit Then
		DOF DOFBeacon, DOFPulse
	end if

	AfterTriggerActions
	isBeaconLit = True
end Sub

sub t700_600_Hit
	liteLampsForScore2 6,7
	AfterTriggerActions
end Sub

sub t800_400_Hit
	liteLampsForScore2 4,8
	AfterTriggerActions
end Sub

sub t1000_300_200_Hit
	liteLampsForScore3 2,3,10
	AfterTriggerActions
end Sub

sub t900_500_100_Hit
	liteLampsForScore3 1,5,9
	AfterTriggerActions
end Sub

sub liteLampsForScore1(floor1)
	B2S_SetFloorOn(floor1)
	liteInsert(floor1)
end sub

sub liteLampsForScore2(floor1, floor2)
	liteLampsForScore1(floor1)
	liteLampsForScore1(floor2)
end sub

sub liteLampsForScore3(floor1, floor2, floor3)
	liteLampsForScore1(floor1)
	liteLampsForScore1(floor2)
	liteLampsForScore1(floor3)
end sub

sub liteInsert(floor)

	if floor = 1 Then il_100.State = 1 : exit sub
	if floor = 2 Then il_200.State = 1 : exit sub
	if floor = 3 Then il_300.State = 1 : exit sub
	if floor = 4 Then il_400.State = 1 : exit sub

	if floor = 5 Then il_500.State = 1 : exit sub
	if floor = 6 Then il_600.State = 1 : exit sub
	if floor = 7 Then il_700.State = 1 : exit sub
	if floor = 8 Then il_800.State = 1 : exit sub
	if floor = 9 Then il_900.State = 1 : exit sub
	if floor = 10 Then il_1000.State = 1 : exit sub
	if floor = 11 Then il_beacon.State = 1
	
	'no inserts for bonus 

end sub

function CalcScore
	'recalculates score and returns true if score has changed, otherwise false
	'reason for change flag: play sound/dof if a new floor is lit

	dim prevScore 
	prevScore = Score
	CalcScore = False
	score = 0

	if IsTilted Then
		debug "Tilted - no score"
		exit Function
	End If

	score = score + WheelScore

	if il_100.State = 1 Then score = score + 100
	if il_200.State = 1 Then score = score + 200
	if il_300.State = 1 Then score = score + 300
	if il_400.State = 1 Then score = score + 400
	if il_500.State = 1 Then score = score + 500
	if il_600.State = 1 Then score = score + 600
	if il_700.State = 1 Then score = score + 700
	if il_800.State = 1 Then score = score + 800
	if il_900.State = 1 Then score = score + 900
	if il_1000.State = 1 Then score = score + 1000
	if il_beacon.State = 1 Then score = score + 1500

	if il_beacon.State = 1 Then
		'beacon light doubles Score
		score = score * 2
	end if

	'if 2x or 3x wheel bonus, add to score
	if WheelBonusMode = 2 Then
		if isGreenSpecial Then
			score = score * 3
		elseif isRedSpecial Then
			score = score * 2
		end If
	end If

	debug "Score: " & Score

	if prevScore <> score Then
		CalcScore = True
	end If

end Function

Sub EndCheck

	debug "End check: bp: " & BallsPlayed & " BIP: " & BIP
	if BallsPlayed >= BallsPerGame and BIP <= 0 Then
		debug "End check: game over"
		IsGameOver = True
		IsGameReady = False
		IsGameActive = False
	end If

	if IsGameOver Then
		B2S_SetGameOver
		CheckHighScore
	end If

End Sub


' ***** END SKYSCRAPER SPECIFIC *****


'*****************
' Maths
'*****************

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

dim startGameTimerIndex : startGameTimerIndex = 0
sub StartGameTimer_Timer
	if startGameTimerIndex=0 Then
		Debug "Start game timer enabled"
		BallsCollected = 0
		OpenSliders
	end If

	startGameTimerIndex = startGameTimerIndex + 1

	if startGameTimerIndex > 100 then
		'max wait time for balls to drain
		Debug "Start game max run time reached"
		BallsCollected = 10
	end if

	if BallsCollected >= 10 Then
		Debug "Start game timer complete - " & startGameTimerIndex
		CloseSliders
		StartGameTimer.enabled = False
		IsGameReady = True
		IsGameActive = True
		startGameTimerIndex = 0
		B2S_GameStart
	end if

end Sub

Sub CheckHighScore
 	If Score>HighScore and not IsTilted and not HasCheated then
		HighScore=Score
  		SaveValue cGameName, "HighScore", HighScore
		B2S_UpdateHighScoreReel
 	End If
End Sub

Sub ResetHighScore

	HighScore = DefaultHighScore
	SaveValue cGameName, "HighScore", HighScore
	B2S_UpdateHighScoreReel

End Sub

Sub Plunger_Init()

End Sub

' ***** TILT LOGIC *****

dim resettiltcount : resettiltcount = 0
dim IsBallInTiltKicker : IsBallInTiltKicker = False
dim tiltmechX
dim tiltmechY
dim tiltmechZ

sub TiltWarningsTimer_Timer
	' not yet implemented
end Sub

sub ResetTilt
	tiltmechX = TiltKicker.X
	tiltmechY = TiltKicker.Y
	tiltmechZ = TiltPostWall.HeightTop + 25

	CurrentWarnings = 0
	IsTilted = False
	tiltkicker.kick 180,1 'ensure ball is released from kicker
	IsBallInTiltKicker = False
	ResetTiltTimer.enabled = True 
end Sub

sub MoveTiltBallToPost
	tiltball.X = tiltmechX
	tiltball.Y = tiltmechY
	tiltball.Z = tiltmechZ

	tiltball.VelX = 0
	tiltball.VelY = 0
	tiltball.VelZ = 0.1
end Sub

sub ResetTiltTimer_Timer

	if resettiltcount <= 1 Then
		TilterRamp.Collidable = False
		TiltKicker.Enabled=False
		TiltPostWall.IsDropped = True
		TiltPostMesh.TransY = -44
	end If

	if resettiltcount = 10 Then
		' allows time for the ball to slowly settle, but force move it if needed
		MoveTiltBallToPost
		TiltPostMesh.TransY = 0
		TiltPostWall.IsDropped = False

		TiltKicker.Enabled=True
	end If

	if resettiltcount > 10 Then
		if IsBallInTiltKicker = False Then
			'wait for tiltball to settle in kicker
			MoveTiltBallToPost
		else 
			'tiltball ready
			Debug "Reset tilt complete:" & resettiltcount

			tiltball.Z = 20		
			ResetTiltTimer.enabled = False
			resettiltcount = 0
			TiltKicker.Enabled=False
			TiltBallFence.Collidable = True
			TiltKicker.Kick 0,0
			Exit Sub
		End If
	end If

	if resettiltcount > 40 Then
		'ball lost or not settling, just abort.
		MoveTiltBallToPost
		Debug "Tilt ball not in kicker - aborting"
		ResetTiltTimer.enabled = False
		resettiltcount = 0
		TiltKicker.Enabled=False
		TiltBallFence.Collidable = True
		TiltKicker.Kick 0,0
		Exit Sub
	end If

	resettiltcount = resettiltcount + 1
end Sub

sub TiltKicker_Hit
	IsBallInTiltKicker = True
end Sub

sub CreateTiltBall
	Set tiltball = TiltKicker.CreateBall
	tiltball.radius = 15
	tiltball.UserValue = "tilt"
	tiltball.PlayfieldReflectionScale = 0
	tiltball.Image = "ball_HDR"
	tiltball.FrontDecal = "Scratches"
	tiltball.DecalMode = False
end Sub

Sub SetMechTilt 
	
	if not IsGameActive then exit Sub

	if IsTilted = True Then
		exit Sub
	end if

	if CurrentWarnings = 0 Then
		TiltWarningTime = Now
		CurrentWarnings = CurrentWarnings + 1
	Else
		'already have a warning, but don't allow multiple warnings in same few seconds
		if DateDiff("s",TiltWarningTime,Now) > 1 Then
			CurrentWarnings = CurrentWarnings + 1
		Else
			'in grace period
			exit Sub
		end if
	End If

	if CurrentWarnings > TiltWarnings Then
		IsTilted = True
		B2S_SetTiltedOn
		PlaySound "1000a"

		TilterRamp.Collidable = True
		TiltBallFence.Collidable = false
		TiltKicker.Enabled = False

		'TiltKicker.kick 190,7
		' kick randomly between 30-330 degrees
		'TiltKicker.kick Int((330-30+1)*Rnd+30),2
		
		dim velx, vely
		if Int((2*Rnd)+1) = 1 Then
			velx = 5
		Else
			velx = -5
		end If

		vely = Int((2-1+1)*Rnd+1)

		TiltBall.VelY = vely
		TiltBall.VelX = velx
	Else
		PlaySound "10a"
	end If

End Sub

Dim ManualTiltLevel : ManualTiltLevel = 0
Dim TiltSensitivity : TiltSensitivity = 7

Sub CheckManualTilt (direction) 'Called when table is nudged

	if not IsGameActive then exit Sub

	if IsTilted = True Then
		exit Sub
	end if

    ManualTiltLevel = ManualTiltLevel + TiltSensitivity       
    TiltDecreaseTimer.Enabled = True

	if ManualTiltLevel + TiltSensitivity > 15 and (ManualTiltLevel < 15) then 
		'sound a warning if 1 more nudge causes tilt
		PlaySound "10a"
		Debug "Manual Tilt Warning"
    End if

    If ManualTiltLevel > 15 Then 'more than 15 then TILT the table
        IsTilted = True
		B2S_SetTiltedOn
		PlaySound "1000a"

		dim velx : velx = 0
		dim vely : vely = 0
		dim minvelx, maxvelx, minvely, maxvely
			
		if direction = 0 Then
			 minvelx = -2 : maxvelx = 2
			 minvely = -3 : maxvely = -1
		elseif direction = 90 Then
			minvelx = 3 : maxvelx = 5
			 minvely = -1 : maxvely = 1
		elseif direction = 270 Then
			minvelx = -5 : maxvelx = -3
			minvely = -1 : maxvely = 1
		Else
			minvelx = 1 : maxvelx = 3
		    minvely = 1 : maxvely = 3
		end If
	
		velx = Int((maxvelx-minvelx+1)*Rnd+minvelx)
		vely = Int((maxvely-minvely+1)*Rnd+minvely)

		TilterRamp.Collidable = True
		TiltKicker.Enabled = False
		TiltBallFence.Collidable = False
	
		TiltBall.VelY = vely
		TiltBall.VelX = velx

		Debug "Manual Tilt " & velx & " " & VelY
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If ManualTiltLevel > 0 Then
        ManualTiltLevel = ManualTiltLevel - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
		Debug "Disabling Tilt Timer"
    End If
End Sub

' ***** END TILT LOGIC *****

' ***** CHEATER LOGIC *****

dim IsCheatEnabled : IsCheatEnabled = False ' for testing: use both magna
dim IsCheatWallVisible : IsCheatWallVisible = True ' sneaky, really meant for messing with people
dim IsRightMagnaPressed : IsRightMagnaPressed = False
dim IsLeftMagnaPressed : IsLeftMagnaPressed = False
dim HasCheated : HasCheated = False

Sub EnableCheaters
	dim wall, gate
	HasCheated = True
	for each wall in CheaterWalls
		wall.Collidable = True
		if IsCheatWallVisible Then
			wall.Visible = True
			wall.SideVisible = True
		Else
			wall.Visible = False
			wall.SideVisible = False
		end If
	Next
	for each gate in CheaterGates
		gate.Collidable = True
		if IsCheatWallVisible Then
			gate.Visible = True
		Else
			gate.Visible = False
		end If
	Next
End Sub

Sub DisableCheaters
	dim wall, gate
	for each wall in CheaterWalls
		wall.Collidable = False
		wall.Visible = False
		wall.SideVisible = False
	Next
	for each gate in CheaterGates
		gate.Collidable = false
		gate.Visible = False
	Next
End Sub

' ***** END CHEATER LOGIC *****

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
'   rothbauerw's Manual Ball Control
'*****************************************

Dim BCup, BCdown, BCleft, BCright
Dim ControlBallInPlay, ControlActiveBall
Dim BCvel, BCyveloffset, BCboostmulti, BCboost

BCboost = 1				'Do Not Change - default setting
BCvel = 4				'Controls the speed of the ball movement
BCyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
BCboostmulti = 3		'Boost multiplier to ball veloctiy (toggled with the B key) 

ControlBallInPlay = false

Sub StartBallControl_Hit()
	Set ControlActiveBall = ActiveBall
	ControlBallInPlay = true
End Sub

Sub StopBallControl_Hit()
	ControlBallInPlay = false
End Sub	

Sub BallControlTimer_Timer()
	If EnableBallControl and ControlBallInPlay then
		If BCright = 1 Then
			ControlActiveBall.velx =  BCvel*BCboost
		ElseIf BCleft = 1 Then
			ControlActiveBall.velx = -BCvel*BCboost
		Else
			ControlActiveBall.velx = 0
		End If

		If BCup = 1 Then
			ControlActiveBall.vely = -BCvel*BCboost
		ElseIf BCdown = 1 Then
			ControlActiveBall.vely =  BCvel*BCboost
		Else
			ControlActiveBall.vely = bcyveloffset
		End If
	End If
End Sub


'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds)
'********************************************************************

Const tnob = 14 ' total number of balls
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
	
	If UBound(BOT) > tnob Then Exit Sub

    For b = 0 to UBound(BOT)
        ' play the rolling sound for each ball
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' play ball drop sounds
		'ignore drop sounds from 18-23
		'If BOT(b).VelZ < -1 and (BOT(b).z < 14 or BOT(b).z > 28) Then 'height adjust for ball drop sounds
        'If BOT(b).VelZ < -1 and BOT(b).z < 20 and BOT(b).z > -30 Then 'height adjust for ball drop sounds
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 1 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/1200, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
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

Sub BumperWall_Hit
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub ContactWalls_Hit(idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub springbumper_Hit(idx)
	debug "spring hit"
	PlaySound "metalhit_thin",0,1,AudioPan(Pins(idx)),0,0,0,1,AudioFade(Pins(idx))
End Sub

sub cones_Hit
	PlaySound "metalhit_thin",0,1,AudioPan(ActiveBall),0,0,0,1,AudioFade(ActiveBall)
end sub

sub WireGuides_Hit(idx)
	PlaySound "metalhit_thin",0,1,AudioPan(ActiveBall),0,0,0,1,AudioFade(ActiveBall)
end sub

'Sub Pins_Hit(idx)
'	PlaySound "pintap3",0,1,AudioPan(Pins(idx)),0,0,0,1,AudioFade(Pins(idx))
'End Sub

Sub PinsWall_Hit(idx)
	PlaySound "pintap3",0,1,AudioPan(ActiveBall),0.1,0,0,1,AudioFade(ActiveBall)
End Sub

Sub BigPinsWall_Hit(idx)
	'PlaySound "pintap",0,1,AudioPan(ActiveBall),0,0,0,1,AudioFade(ActiveBall)
	'PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
	PlaySound "pintap3",0,1,AudioPan(ActiveBall), 0.1, 0.5, 0, 1, AudioFade(ActiveBall)
End Sub

Sub PFObjects_Hit(idx)
	PlaySound "metalhit_thin",0,1,AudioPan(ActiveBall),0,0,0,1,AudioFade(ActiveBall)
End Sub

Sub skyscraper_Hit
	debug "skyscraper hit"
	'PlaySound "pintap",0,1,AudioPan(ActiveBall),0,0,0,1,AudioFade(ActiveBall)
	'PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
	PlaySound "pintap3", 0, 1, AudioPan(ActiveBall), 0.05, 0.05, 0, 1, AudioFade(ActiveBall)
End Sub

Sub FreePlayBumpers_Hit(idx)
	debug "freeplay hit:"&idx
	'PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
	PlaySound "metalhit_thin", 0, 1, AudioPan(ActiveBall), 0.1, 0.7, 0, 1, AudioFade(ActiveBall)
End Sub

'Sub ApronTop_Hit
'	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub

'Sub ApronBottom_Hit
'	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub

Sub Rubbers_Hit(idx)
	debug "Rubbers hit:"&idx
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

Function PinTapVol(ball) ' Calculates the Volume of the sound based on the ball speed
    PinTapVol = Csng(BallVel(ball) ^2 / 60)
End Function

' ***** GAME LOGIC *****

dim index : index = 0
dim pfball
dim specialball
sub CreateNewBall(location)
	Set pfball = location.CreateBall
	'pfball.color = rgb(StandardBallBrightness,StandardBallBrightness,StandardBallBrightness)
	pfball.radius = BallSize
	pfball.UserValue = "normal"

	SetStandardBallColorAndBrightness(pfball)
end sub

sub CreateStartingBalls

	StartingBallTimer.enabled = True
	
	CreateNewBall StartingOutholeKicker
	StartingOutholeKicker.Kick 160, 2

	CreateNewBall StartingOutholeKicker2
	StartingOutholeKicker2.Kick 200, 2

end Sub

dim StartingBallCount : StartingBallCount = 0
sub StartingBallTimer_Timer

	if StartingBallCount mod 2 = 0 Then
		CreateNewBall StartingKicker1
		StartingKicker1.Kick 90, 2
	Else
		CreateNewBall StartingKicker2
		StartingKicker2.Kick 270, 2
	end If

	StartingBallCount = StartingBallCount + 1
	
	if StartingBallCount >= 8 Then
		StartingBallTimer.enabled = False
		StartingBallCount = 0
	end if

end Sub

Sub AddBIP(val)

	BIP = BIP + val
	if BIP < 0 then 
		debug "Adjust BIP to zero : " & BIP
		BIP = 0
	end If

end Sub


' ***** END GAME LOGIC *****

' ***** TEST *****

dim alternate : alternate = False

Sub TestTimer_Timer
	Dim ballAngle, ballPower
	alternate = not alternate

	if alternate then
		ballAngle = Int((180-1+1)*Rnd+1)
	Else
		ballAngle = Int((360-180+1)*Rnd+180)
	end If

	'ballAngle = 45

	ballPower = Int((9-2+1)*Rnd+2)

	CreateNewBall TestKicker
	TestKicker.kick ballAngle, ballPower
End Sub

Sub RunSim

	if TestTimer.Enabled = True Then
		TestTimer.enabled = False
		TestDrain.Enabled = False
		TestDrain2.Enabled = False
	Else
		TestTimer.enabled = True
		TestDrain.Enabled = True
		TestDrain2.Enabled = True
	end If

End Sub

Sub TestDrain_Hit
	TestDrain.DestroyBall
End Sub

Sub TestDrain2_Hit
	TestDrain2.DestroyBall
End Sub

' ***** END TEST *****

sub FreePlayDrains_Hit(idx)
	
	FreePlayDrains(idx).DestroyBall
	AddBIP(-1)
	BallsPlayed = BallsPlayed - 1
	B2S_UpdateBallReel
	debug "Freeplay drain " & idx & " hit, BIP: " & BIP
end Sub

sub BottomTriggers_hit(idx)
	debug "Bottom out trigger " & idx

	AddBIP(-1)
	EndCheck
end sub

Sub Drain_Hit()

	debug "Drain hit, BIP: " & BIP
	PlaySound "metalhit_medium",0,1,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain)
	Drain.DestroyBall

	if IsGameActive Then
		' drain hit when game not over -- for skyscraper, this shouldn't happen
		
		BallsPlayed = BallsPlayed - 1
		'check on this?
		'AddBIP(-1)
		B2S_UpdateBallReel
	Else
		'drain hit while game over
		BallsCollected = BallsCollected + 1
		debug "Balls Collected: " & BallsCollected
	end if

End Sub

Sub DrainHelper_Hit()
	' only active when resetting game -- to speed things up
	debug "Drain helper hit,"
	PlaySound "metalhit_medium",0,1,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain)
	DrainHelper.DestroyBall
	BallsCollected = BallsCollected + 1
End Sub

Sub VoidKicker_Hit()
	
	debug "Void hit - ball escaped"
	VoidKicker.DestroyBall
	BallsPlayed = BallsPlayed - 1
	BIP = BIP - 1

End Sub


dim kicker, trigger

sub EnableKickers
	for each kicker in AllKickers
		kicker.Enabled = True
	Next
end Sub

sub DisableKickers
	for each kicker in AllKickers
		kicker.Enabled = False
	Next
end Sub

sub DisableTriggers
	for each trigger in AllTriggers
		trigger.Enabled = False
	Next
end Sub

sub EnableTriggers
	for each trigger in AllTriggers
		trigger.Enabled = True
	Next
end Sub


' ***** END KICKERS / TRIGGERS *****

' ***** SLIDERS *****

dim isSliderOpen : isSliderOpen = False
sub SliderOpenTimer_Timer
	'opens scored ball wall
	'leaving this as a timer, even though there is no animation currently

	debug "ScoredBallRelease dropping..."
	ScoredBallReleaseWall.Collidable = False
	isSliderOpen = True
	SliderOpenTimer.Enabled = False

end sub
sub SliderCloseTimer_Timer
	debug "ScoredBallRelease closing..."
	SliderCloseTimer.Enabled = False
	ScoredBallReleaseWall.Collidable = True
	isSliderOpen = False

end sub

dim isSliderOutholeOpen : isSliderOutholeOpen = False
sub SliderOutholeOpenTimer_Timer
	if pf_sliderouthole.TransY <= 0 Then
		isSliderOutholeOpen = True
		pf_sliderouthole.TransY = 0
		pfsupportwallouthole.Collidable = False
		SliderOutholeOpenTimer.Enabled = False

		OutRamp.Collidable = False
	Else
		pf_sliderouthole.TransY = pf_sliderouthole.TransY - 2
	end If

end sub
sub SliderOutholeCloseTimer_Timer
	
	if pf_sliderouthole.TransY >= 45 Then
		isSliderOutholeOpen = False
		pf_sliderouthole.TransY = 45
		SliderOutholeCloseTimer.Enabled = False
		pfsupportwallouthole.Collidable = True

		OutRamp.Collidable = True
	Else
		pf_sliderouthole.TransY = pf_sliderouthole.TransY + 2
	end If

end sub

sub OpenOutholeSlider
	debug "Open Outhole Slider"

	' if slider is opening, stop
	if SliderOutholeCloseTimer.enabled = True then
		SliderOutholeCloseTimer.enabled = False
	end if

	SliderOutholeOpenTimer.Enabled = True
	PlaySound "metalhit2"
end Sub

sub CloseOutholeSlider
	debug "Close Outhole Slider"

	' if slider is opening, stop
	if SliderOutholeOpenTimer.enabled = True then
		SliderOutholeOpenTimer.enabled = False
	end if

	SliderOutholeCloseTimer.Enabled = True
	PlaySound "metalhit2"
end Sub

dim OutholeTimerCounter : OutholeTimerCounter = 0
sub OutholeTimer_Timer

	if OutholeTimerCounter = 0 Then
		OpenOutholeSlider
	end If

	OutholeTimerCounter = OutholeTimerCounter + 1

	if OutholeTimerCounter > 25 then
		CloseOutholeSlider
		OutholeTimerCounter = 0
		OutholeTimer.enabled = False
	end If

end Sub

sub RunOutholeSlider

	if OutholeTimer.enabled = True then
		'outhole is already open.
		OutholeTimerCounter = 1 ' extends the open time
	Else
		OutholeTimer.enabled = True
	end if

end Sub


sub OpenSliders
	debug "Open Sliders"

	SliderOpenTimer.Enabled = True
	SliderOutholeOpenTimer.Enabled = True

	'SeatOpenTimer.Enabled = True
	PlaySound "coinslidein"
end Sub

sub CloseSliders
	debug "Close Sliders"

	SliderCloseTimer.Enabled = True
	SliderOutholeCloseTimer.Enabled = True
	'SeatCloseTimer.Enabled = True
	PlaySound "coinslideout"
end Sub


' ***** END SLIDERS *****

' ***** B2S *****

sub B2SLoadedTimer_Timer

	B2S_UpdateHighScoreReel
	B2S_HighScoreLight
	IsGameOver = True

	B2SLoadedTimer.Enabled = False

end Sub

Sub B2S_HighScoreLight
	
	if UseB2S = 1 then
		Controller.B2SSetData 105, 1  ' High Score reel
		Controller.B2SSetData 106, 1  ' High Score light		
	end If

End Sub

Sub B2S_SetFloorOn(floor)
	' 11 = beacom/roof, 12 = green bonus, 13 = red bonus

	dim floorId
	if floor < 10 Then
		floorId = cInt("20" & floor)
	Else
		floorId = cInt("2" & floor)
	end if

	if UseB2S = 1 then
		Controller.B2SSetData floorId, 1 
	end If

End Sub

Sub B2S_SetFloorsOff
	' 11 = beacom/roof, 12 = green bonus, 13 = red bonus
	
	if UseB2S <> 1 then
		exit Sub
	end If

	dim floorId
	for floorId = 1 to 13
		Controller.B2SSetData 200+floorId, 0 
	next

End Sub

Sub B2S_SetTiltedOn
	'33 b2s id, default tilt
	if UseB2S = 1 then
		Controller.B2SSetTilt 1 
	end If
End Sub

Sub B2S_SetTiltedOff
	'33 b2s id, default tilt
	if UseB2S = 1 then
		Controller.B2SSetTilt 0
	end If
End Sub

Sub B2S_LightbulbAnimation
	if UseB2S = 1 then
		Controller.B2SStartAnimation "globes" 
	end If
End Sub



Sub B2S_SetGameOver
	if UseB2S = 1 then
		Controller.B2SSetGameOver 1 
	end If
End Sub

Sub B2S_GameStart
	
	if UseB2S = 1 then
		'debug "B2S_GameStart"
		Controller.B2SSetGameOver 0 

		Controller.B2SSetScorePlayer1 000
		Controller.B2SSetScore 2,0 ' Ball Reel
		Controller.B2SSetScore 3,BallsPerGame ' Ball Reel

		B2S_SetTiltedOff
	end If

End Sub

Sub B2S_UpdateBallReel
	dim currBall 

	if UseB2S = 1 then
		Controller.B2SSetScore 2,BallsPlayed ' Ball Reel
	end If

End Sub

Sub B2S_UpdateBallsPerGameReel
	
	if UseB2S = 1 then
		Controller.B2SSetScore 3,BallsPerGame ' Ball Reel
	end If

End Sub

Sub B2S_UpdateScoreReel
	
	if UseB2S = 1 then
		Controller.B2SSetScorePlayer1 Score
	end If

End Sub

Sub B2S_UpdateHighScoreReel
	
	if UseB2S = 1 then
		Controller.B2SSetScore 4,HighScore
	end If

End Sub

' ***** END B2S *****

' ***** DEBUG / SETTINGS *****

Dim objIEDebugWindow
Sub Debug( myDebugText )

	if UseDebugWindow = False Then
		Exit Sub
	end If

	If Not IsObject( objIEDebugWindow ) Then
	Set objIEDebugWindow = CreateObject( "InternetExplorer.Application" )
	objIEDebugWindow.Navigate "about:blank"
	objIEDebugWindow.Visible = True
	objIEDebugWindow.ToolBar = False
	objIEDebugWindow.Width = 1200
	objIEDebugWindow.Height = 600
	objIEDebugWindow.Left = 5200
	objIEDebugWindow.Top = 1
	Do While objIEDebugWindow.Busy
	WScript.Sleep 100
	Loop
	objIEDebugWindow.Document.Title = "My Debug Window"
	objIEDebugWindow.Document.Body.InnerHTML = "<b>Debugging Output Window Created -TimeStamp: " & Now & "</b></br>"
	End If

	'objIEDebugWindow.Document.Body.InnerHTML = objIEDebugWindow.Document.Body.InnerHTML & myDebugText & " -TimeStamp: " & Now & "<br>" & vbCrLf
	objIEDebugWindow.Document.Body.InnerHTML = myDebugText & " -TimeStamp: " & Now & "<br>" & vbCrLf & objIEDebugWindow.Document.Body.InnerHTML 

	'objIEDebugWindow.scrollTo(0, document.body.scrollHeight)
End Sub

Dim TableSlope : TableSlope = 4
Dim OddBallColor : OddBallColor = 3
Dim BallType : BallType = 0
Dim AlterBallMass : AlterBallMass = False
Dim OddR, OddG, OddB
Dim OddBallBrightnessModifier : OddBallBrightnessModifier = 1.0
Dim BallBrightnessModifier : BallBrightnessModifier = 0.60
Dim BellChoiceInt : BellChoiceInt = 0

' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings
Dim dspTriggered : dspTriggered = False

dim PlayElevatorSound : PlayElevatorSound = True
dim WheelDragModifier : WheelDragModifier = 0.5
dim LFBWheelStop : LFBWheelStop = False
dim WheelBonusMode : WheelBonusMode = 0

Sub Table1_OptionEvent(ByVal eventId)

	If eventId = 1 And Not dspTriggered Then 
		dspTriggered = True
		DisableStaticPreRendering = True
	End If

    TableSlope = Table1.Option("Slope", 1, 10, 0.5, 4, 0)
	Table1.SlopeMin = TableSlope

	TiltSensitivity = Table1.Option("Tilt Sensitivity", 1, 10, 1, 7, 0)
    MaxBallsInShooterLane = Table1.Option("Max Balls in Lane", 1, 5, 1, 2, 0)
	BallBrightnessModifier = Table1.Option("Ball Brightness", 0, 1, .01, 1, 1)
	PlayElevatorSound = Table1.Option("Elevator Sound", 0, 1, 1, 1, 0, Array("Off","On"))
	WheelDragModifier = Table1.Option("Wheel Drag", .01, 1, .01, .50, 1)
	LFBWheelStop = Table1.Option("Left Flipper Wheel Stop", 0, 1, 1, 0, 0, Array("Off","On"))
	WheelBonusMode = Table1.Option("Wheel Bonus", 0, 2, 1, 0, 0, Array("None","1 and 2 Extra Balls", "2x and 3x Points"))

	' Room brightness
	'	LightLevel = Table1.Option("Table Brightness (Ambient Light Level)", 0, 1, 0.01, .5, 1)
	LightLevel = NightDay/100
	SetRoomBrightness LightLevel 

	UpdateBalls

	If eventId = 3 And dspTriggered Then
		dspTriggered = False
		DisableStaticPreRendering = False
	End If

	'if the bell sound changed, play it
	'if eventId = 1 and newBellChoice <> BellChoiceInt Then
	'	BellChoiceInt = newBellChoice
	'	PlayActionSound
	'end If

End Sub

function GetStandardBallBrightness

	dim b_base

	b_base = 120 * LightLevel + 90
	b_base = b_base * BallBrightnessModifier

	' ensure min/max values
	If b_base < 10 Then b_base = 10
	If b_base > 255 Then b_base = 255 

	GetStandardBallBrightness = b_base
	
end Function

sub SetStandardBallColorAndBrightness(ball)

	dim b_base
	b_base = 100 * LightLevel + 155	
	b_base = b_base * BallBrightnessModifier

	' ensure min/max values
	If b_base < 10 Then b_base = 10
	If b_base > 255 Then b_base = 255 

	ball.color = rgb(b_base,b_base,b_base)
'	GetStandardBallBrightness = b_base
	
end sub

Sub UpdateBalls

	dim BOT, b, standardBrightness

	BOT = GetBalls
	For b = 0 to UBound(BOT)
		SetStandardBallColorAndBrightness(BOT(b))		
    Next

End Sub


' ***** END DEBUG / SETTINGS *****

' ***** ROOM BRIGHTNESS *****

' Update these arrays if you want to change more materials with room light level
Dim RoomBrightnessMtlArray: RoomBrightnessMtlArray = Array("VLM.Bake.Active","VLM.Bake.Solid") 

Sub SetRoomBrightness(lvl)
	If lvl > 1 Then lvl = 1
	If lvl < 0 Then lvl = 0

	' Lighting level
	Dim v: v=(lvl * 245 + 10)/255

	Dim i: For i = 0 to UBound(RoomBrightnessMtlArray)
		ModulateMaterialBaseColor RoomBrightnessMtlArray(i), i, v
	Next
End Sub

Dim SavedMtlColorArray
SaveMtlColors
Sub SaveMtlColors
	ReDim SavedMtlColorArray(UBound(RoomBrightnessMtlArray))
	Dim i: For i = 0 to UBound(RoomBrightnessMtlArray)
		SaveMaterialBaseColor RoomBrightnessMtlArray(i), i
	Next
End Sub

Sub SaveMaterialBaseColor(name, idx)
	Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	SavedMtlColorArray(idx) = round(base,0)
End Sub


Sub ModulateMaterialBaseColor(name, idx, val)
	Debug "ModulateMaterialBaseColor " & name & " " & idx & " " & val
	Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	Dim red, green, blue, saved_base, new_base
 
	'First get the existing material properties
	GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle

	'Get saved color
	saved_base = SavedMtlColorArray(idx)
    
	'Next extract the r,g,b values from the base color
	red = saved_base And &HFF
	green = (saved_base \ &H100) And &HFF
	blue = (saved_base \ &H10000) And &HFF
	'msgbox red & " " & green & " " & blue

	'Create new color scaled down by 'val', and update the material
	new_base = RGB(red*val, green*val, blue*val)
	UpdateMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, new_base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub


' *** SPINNER ***
dim spinnerclicks : spinnerclicks = 0

dim WheelTimerCount : WheelTimerCount = 0
dim WheelTimerTarget : WheelTimerTarget = 10000 'max tick count wheel can spin
dim AmountToRotate : AmountToRotate = 3
dim WheelCurrentAngle : WheelCurrentAngle = 0 
dim WheelNewAngle : WheelNewAngle = 0
dim LastClickAngle : LastClickAngle = -1
dim WheelScore : WheelScore = 0



sub WheelTimer_Timer
	WheelTimerCount = WheelTimerCount + 1
	'debug "Amount to rotate: " & AmountToRotate

	'no drag < in freespin
	if WheelTimerCount > WheelFreeSpin and WheelTimerCount < 300 and WheelTimerCount mod 4 = 0 Then
		' early in spin (30-300 Count) = more drag
		AmountToRotate = AmountToRotate * LargeDragCoefficient ' adjust drag
	elseif WheelTimerCount > 300 and WheelTimerCount mod 4 = 0 Then
		' mid/late in spin: less drag to simulate wheel slowing
		AmountToRotate = AmountToRotate * SmallDragCoefficient ' adjust drag
	end if

	WheelNewAngle = wheel.Roty + AmountToRotate
	if WheelNewAngle > 360 Then WheelNewAngle = 360 - WheelNewAngle
	'debug "WheelNewAngle: " & WheelNewAngle
	'play a click every 10 degrees, but filter if already played for this degree
	if (int(WheelNewAngle) mod 36 = 0) and LastClickAngle <> int(WheelNewAngle) then
		PlaySound "fx_spinner", 0, 1, AudioPan(wheel), 0.25, 0, 0, 1, AudioFade(wheel)
		LastClickAngle = int(WheelNewAngle)
	end If

	rotateWheelObjects(WheelNewAngle)
	WheelCurrentAngle = WheelNewAngle

	if AmountToRotate <= 0 Then
		debug "rotate =0"
		WheelTimer.enabled = False 
		WheelTimerCount = 0
		AmountToRotate = 3
	end If

	if AmountToRotate <= 0.005 Then
		'debug "slow wheel: " & AmountToRotate
		WheelTimer.enabled = False 
		WheelTimerCount = 0
		AmountToRotate = 3 
		WheelTimer2.enabled = true
	end If

	if WheelTimerCount >= WheelTimerTarget Then
		debug "wheel timeout " & AmountToRotate
		WheelTimer.enabled = False 
		WheelTimerCount = 0
		AmountToRotate = 3
		WheelTimer2.enabled = true
	end if
end sub

dim WheelTimer2_Count : WheelTimer2_Count = 0
sub WheelTimer2_Timer
	'handles the settling of the wheel

	if WheelTimer2_Count < 10 then 
		rotateWheelObjects(wheel.Roty - 0.25)
	Else
		rotateWheelObjects(wheel.Roty  + 0.1)
	end If

	if WheelTimer2_Count > 20 Then
		WheelTimer2.enabled = False
		WheelTimer2_Count = 0
		debug "ending wheeltimer2"
		
		WheelScore = getPointsForAngle(wheel.Roty)

		score = score + WheelScore
		IsWheelDone = True
		debug "Spinner Points: " & WheelScore		
		ProcessWheelBonus
		B2S_UpdateScoreReel
	end If

	WheelTimer2_Count = WheelTimer2_Count + 1
end sub

sub LiteBonusIfNeeded
	if isRedSpecial then B2S_SetFloorOn 13
	if isGreenSpecial then B2S_SetFloorOn 12
end Sub

sub ProcessWheelBonus

	LiteBonusIfNeeded
	'WheelBonusMode 0=off, 1=extra balls, 2=2x/3x mod

	if WheelBonusMode = 1 Then 'extra balls 

		if isRedSpecial then 
			BallsPerGame = StandardBallsPerGame + 1 
		ElseIf isGreenSpecial then
			BallsPerGame = StandardBallsPerGame + 2 
		end If

		B2S_UpdateBallsPerGameReel
		
	elseif WheelBonusMode = 2 Then
		' implemented in scoring
	end If

end sub

sub RotateWheel
	rotateWheelObjects(wheel.Roty + 5)
	debug "rotate wheel"
end Sub

sub doWheel
	rotateWheelObjects(wheel.Roty + 1)
	debug "Roty: " & wheel.Roty 
end sub

function getPointsForAngle(angle)

	dim points:points=0

	if angle <= 10 then	
		points = 100
	elseif angle <= 20 then points = 500 : isRedSpecial = True
	elseif angle <= 31 then points = 300
	elseif angle <= 41 then points = 200
	elseif angle <= 51 then points = 700
	elseif angle <= 61 then points = 100
	elseif angle <= 72 then points = 1000

	elseif angle <= 82 then points = 100
	elseif angle <= 93 then points = 500
	elseif angle <= 103 then points = 300
	elseif angle <= 113 then points = 200
	elseif angle <= 124 then points = 700
	elseif angle <= 134 then points = 100

	elseif angle <= 144 then points = 1500
	elseif angle <= 155 then points = 100
	elseif angle <= 165 then points = 500
	elseif angle <= 175 then points = 300 : isGreenSpecial = True
	elseif angle <= 185 then points = 200

	elseif angle <= 196 then points = 700
	elseif angle <= 206 then points = 100
	elseif angle <= 216 then points = 1000
	elseif angle <= 226 then points = 100
	elseif angle <= 236 then points = 500

	elseif angle <= 247 then points = 300
	elseif angle <= 257 then points = 200
	elseif angle <= 267 then points = 700
	elseif angle <= 278 then points = 100
	elseif angle <= 288 then points = 800

	elseif angle <= 298 then points = 100
	elseif angle <= 308 then points = 500 : isRedSpecial = True
	elseif angle <= 319 then points = 300
	elseif angle <= 329 then points = 200
	elseif angle <= 339 then points = 700

	elseif angle <= 350 then points = 100
	else points = 700
	end if

	'testing
	'isGreenSpecial = True
	
	getPointsForAngle = points

end function

dim RandomFactor : RandomFactor = 100
' drag values determine how much to rotation to maintain per time slice 
' higher values keep more momentum = less drag. 1 = no drag (wheel spins forever)
' low values makes wheel stop faster
Const cLargeDragCoefficientBase = 0.9955 ' beginning of wheel spin
Const cSmallDragCoefficientBase = 0.997  ' mid/end wheel spin
dim LargeDragCoefficient : LargeDragCoefficient = cLargeDragCoefficientBase
dim SmallDragCoefficient : SmallDragCoefficient = cSmallDragCoefficientBase
Const cInitialFreeSpin = 30 ' spin at full speed for this many interations
dim WheelFreeSpin : WheelFreeSpin = cInitialFreeSpin

sub StartWheel
	IsWheelDone = False
	'add some randomness
	RandomFactor = ((100-1+1)*Rnd+1) / 100000
	WheelFreeSpin = ((120-25+1)*Rnd+25) ' free spin between 25-120 interations for randomness

	'these numbers need to be =< 1 otherwise wheel speeds up 
	LargeDragCoefficient = (cLargeDragCoefficientBase - RandomFactor)-(WheelDragModifier/200)
	SmallDragCoefficient = (cSmallDragCoefficientBase - RandomFactor)-(WheelDragModifier/200)

	'in case of odd values, ensure modifiers are less than 1
	if LargeDragCoefficient >= 1.0 then LargeDragCoefficient = 0.998
	if SmallDragCoefficient >= 1.0 then SmallDragCoefficient = 0.998

	'Debug "Drag: " & RandomFactor & " | " & WheelDragModifier & " | " & LargeDragCoefficient & " | " & SmallDragCoefficient
	WheelTimer.enabled = True 
end Sub

sub SetWheelToRandomSpot
	dim randomAngle
	randomAngle = Int((360-1+1)*Rnd+1)
	rotateWheelObjects(randomAngle)
end Sub

sub rotateWheelObjects(angle)
	wheel.Roty = angle

	'WheelCenterPrim.Roty = angle
	'WheelCenterScrew.Roty = angle

	'BM_wheelimport.Rotz = angle
	BM_WheelCenterPrim.Roty = angle
	BM_WheelCenterScrew.Roty = angle
end Sub

' *** END SPINNER ***

' ***** GATE ANIMATIONS *****

sub Gate007_Animate
	exit sub
	Dim a : a = Gate007.CurrentAngle
	Dim BL
	For each BL in BP_Gate007_Wire
		BL.rotx = -a
	Next

	' orig from VPW Sega Godzilla
	'SpinnerShadow.size_y = abs(sin( (spinangle+180) * (2*PI/360)) * 5)

	' v2
	GateShadow.size_x = 6 + ((abs(sin( (a+180) * (2*PI/360)) * 5) + 1)*2.2)
	GateShadow.TransX = -(abs(a)/4)
	GateShadow.opacity = (100 - (abs(a)/1)) * 0.6

	GateShadow2.size_x = ((abs(sin( (a+180) * (2*PI/360)) * 5) + 1)*0.5)
	GateShadow2.TransX = (abs(a)/4)
	GateShadow2.opacity = (100 - (abs(a)/1)) * 0.6

end Sub

' ***** END GATE ANIMATIONS *****


' ***** VLM ANIMATIONS *****

sub gate_shooterlane_animate
	'exit sub
	Dim a : a = gate_shooterlane.CurrentAngle
	Dim BL
	For each BL in BP_gate_shooterlane_Wire
		BL.rotx = -a
	Next
end Sub


' ***** END VLM ANIMATIONS *****



' VLM  Arrays - Start
' Arrays per baked part
Dim BP_Bumper: BP_Bumper=Array(BM_Bumper)
Dim BP_CabWalls: BP_CabWalls=Array(BM_CabWalls, LM_1000_CabWalls, LM_200_CabWalls, LM_300_CabWalls, LM_400_CabWalls, LM_500_CabWalls, LM_700_CabWalls, LM_800_CabWalls)
Dim BP_Parts: BP_Parts=Array(BM_Parts)
Dim BP_Pins: BP_Pins=Array(BM_Pins)
Dim BP_Playfield: BP_Playfield=Array(BM_Playfield, LM_100_Playfield, LM_1000_Playfield, LM_200_Playfield, LM_300_Playfield, LM_400_Playfield, LM_500_Playfield, LM_600_Playfield, LM_700_Playfield, LM_800_Playfield, LM_900_Playfield, LM_Beacon_Playfield)
Dim BP_PlayfieldBottom: BP_PlayfieldBottom=Array(BM_PlayfieldBottom, LM_100_PlayfieldBottom, LM_1000_PlayfieldBottom, LM_200_PlayfieldBottom, LM_300_PlayfieldBottom, LM_400_PlayfieldBottom, LM_500_PlayfieldBottom, LM_600_PlayfieldBottom, LM_700_PlayfieldBottom, LM_800_PlayfieldBottom, LM_900_PlayfieldBottom, LM_Beacon_PlayfieldBottom)
Dim BP_TiltPostMesh: BP_TiltPostMesh=Array(BM_TiltPostMesh)
Dim BP_WheelCenterPrim: BP_WheelCenterPrim=Array(BM_WheelCenterPrim)
Dim BP_WheelCenterScrew: BP_WheelCenterScrew=Array(BM_WheelCenterScrew)
Dim BP_base: BP_base=Array(BM_base, LM_100_base, LM_200_base)
Dim BP_freeplaybumper000: BP_freeplaybumper000=Array(BM_freeplaybumper000)
Dim BP_freeplaybumper001: BP_freeplaybumper001=Array(BM_freeplaybumper001)
Dim BP_gate_shooterlane_Wire: BP_gate_shooterlane_Wire=Array(BM_gate_shooterlane_Wire)
Dim BP_screwleftfp: BP_screwleftfp=Array(BM_screwleftfp)
Dim BP_screwrightfp: BP_screwrightfp=Array(BM_screwrightfp)
Dim BP_skyscrapemodel_004: BP_skyscrapemodel_004=Array(BM_skyscrapemodel_004, LM_100_skyscrapemodel_004, LM_1000_skyscrapemodel_004, LM_200_skyscrapemodel_004, LM_300_skyscrapemodel_004, LM_400_skyscrapemodel_004, LM_500_skyscrapemodel_004, LM_600_skyscrapemodel_004, LM_700_skyscrapemodel_004, LM_800_skyscrapemodel_004, LM_900_skyscrapemodel_004, LM_Beacon_skyscrapemodel_004)
Dim BP_tilter: BP_tilter=Array(BM_tilter)
Dim BP_underwalldrop: BP_underwalldrop=Array(BM_underwalldrop)
Dim BP_wheel: BP_wheel=Array(BM_wheel)
' Arrays per lighting scenario
Dim BL_100: BL_100=Array(LM_100_Playfield, LM_100_PlayfieldBottom, LM_100_base, LM_100_skyscrapemodel_004)
Dim BL_1000: BL_1000=Array(LM_1000_CabWalls, LM_1000_Playfield, LM_1000_PlayfieldBottom, LM_1000_skyscrapemodel_004)
Dim BL_200: BL_200=Array(LM_200_CabWalls, LM_200_Playfield, LM_200_PlayfieldBottom, LM_200_base, LM_200_skyscrapemodel_004)
Dim BL_300: BL_300=Array(LM_300_CabWalls, LM_300_Playfield, LM_300_PlayfieldBottom, LM_300_skyscrapemodel_004)
Dim BL_400: BL_400=Array(LM_400_CabWalls, LM_400_Playfield, LM_400_PlayfieldBottom, LM_400_skyscrapemodel_004)
Dim BL_500: BL_500=Array(LM_500_CabWalls, LM_500_Playfield, LM_500_PlayfieldBottom, LM_500_skyscrapemodel_004)
Dim BL_600: BL_600=Array(LM_600_Playfield, LM_600_PlayfieldBottom, LM_600_skyscrapemodel_004)
Dim BL_700: BL_700=Array(LM_700_CabWalls, LM_700_Playfield, LM_700_PlayfieldBottom, LM_700_skyscrapemodel_004)
Dim BL_800: BL_800=Array(LM_800_CabWalls, LM_800_Playfield, LM_800_PlayfieldBottom, LM_800_skyscrapemodel_004)
Dim BL_900: BL_900=Array(LM_900_Playfield, LM_900_PlayfieldBottom, LM_900_skyscrapemodel_004)
Dim BL_Beacon: BL_Beacon=Array(LM_Beacon_Playfield, LM_Beacon_PlayfieldBottom, LM_Beacon_skyscrapemodel_004)
Dim BL_World: BL_World=Array(BM_Bumper, BM_CabWalls, BM_Parts, BM_Pins, BM_Playfield, BM_PlayfieldBottom, BM_TiltPostMesh, BM_WheelCenterPrim, BM_WheelCenterScrew, BM_base, BM_freeplaybumper000, BM_freeplaybumper001, BM_gate_shooterlane_Wire, BM_screwleftfp, BM_screwrightfp, BM_skyscrapemodel_004, BM_tilter, BM_underwalldrop, BM_wheel)
' Global arrays
Dim BG_Bakemap: BG_Bakemap=Array(BM_Bumper, BM_CabWalls, BM_Parts, BM_Pins, BM_Playfield, BM_PlayfieldBottom, BM_TiltPostMesh, BM_WheelCenterPrim, BM_WheelCenterScrew, BM_base, BM_freeplaybumper000, BM_freeplaybumper001, BM_gate_shooterlane_Wire, BM_screwleftfp, BM_screwrightfp, BM_skyscrapemodel_004, BM_tilter, BM_underwalldrop, BM_wheel)
Dim BG_Lightmap: BG_Lightmap=Array(LM_100_Playfield, LM_100_PlayfieldBottom, LM_100_base, LM_100_skyscrapemodel_004, LM_1000_CabWalls, LM_1000_Playfield, LM_1000_PlayfieldBottom, LM_1000_skyscrapemodel_004, LM_200_CabWalls, LM_200_Playfield, LM_200_PlayfieldBottom, LM_200_base, LM_200_skyscrapemodel_004, LM_300_CabWalls, LM_300_Playfield, LM_300_PlayfieldBottom, LM_300_skyscrapemodel_004, LM_400_CabWalls, LM_400_Playfield, LM_400_PlayfieldBottom, LM_400_skyscrapemodel_004, LM_500_CabWalls, LM_500_Playfield, LM_500_PlayfieldBottom, LM_500_skyscrapemodel_004, LM_600_Playfield, LM_600_PlayfieldBottom, LM_600_skyscrapemodel_004, LM_700_CabWalls, LM_700_Playfield, LM_700_PlayfieldBottom, LM_700_skyscrapemodel_004, LM_800_CabWalls, LM_800_Playfield, LM_800_PlayfieldBottom, LM_800_skyscrapemodel_004, LM_900_Playfield, LM_900_PlayfieldBottom, LM_900_skyscrapemodel_004, LM_Beacon_Playfield, LM_Beacon_PlayfieldBottom, LM_Beacon_skyscrapemodel_004)
Dim BG_All: BG_All=Array(BM_Bumper, BM_CabWalls, BM_Parts, BM_Pins, BM_Playfield, BM_PlayfieldBottom, BM_TiltPostMesh, BM_WheelCenterPrim, BM_WheelCenterScrew, BM_base, BM_freeplaybumper000, BM_freeplaybumper001, BM_gate_shooterlane_Wire, BM_screwleftfp, BM_screwrightfp, BM_skyscrapemodel_004, BM_tilter, BM_underwalldrop, BM_wheel, LM_100_Playfield, LM_100_PlayfieldBottom, LM_100_base, LM_100_skyscrapemodel_004, LM_1000_CabWalls, LM_1000_Playfield, LM_1000_PlayfieldBottom, LM_1000_skyscrapemodel_004, LM_200_CabWalls, LM_200_Playfield, LM_200_PlayfieldBottom, LM_200_base, LM_200_skyscrapemodel_004, LM_300_CabWalls, LM_300_Playfield, LM_300_PlayfieldBottom, LM_300_skyscrapemodel_004, LM_400_CabWalls, LM_400_Playfield, LM_400_PlayfieldBottom, LM_400_skyscrapemodel_004, LM_500_CabWalls, LM_500_Playfield, LM_500_PlayfieldBottom, LM_500_skyscrapemodel_004, LM_600_Playfield, LM_600_PlayfieldBottom, LM_600_skyscrapemodel_004, LM_700_CabWalls, LM_700_Playfield, LM_700_PlayfieldBottom, LM_700_skyscrapemodel_004, _
	LM_800_CabWalls, LM_800_Playfield, LM_800_PlayfieldBottom, LM_800_skyscrapemodel_004, LM_900_Playfield, LM_900_PlayfieldBottom, LM_900_skyscrapemodel_004, LM_Beacon_Playfield, LM_Beacon_PlayfieldBottom, LM_Beacon_skyscrapemodel_004)
' VLM  Arrays - End
