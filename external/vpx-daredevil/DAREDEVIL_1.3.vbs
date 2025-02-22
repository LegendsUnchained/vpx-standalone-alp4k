' ****************************************************************
'                       VISUAL PINBALL X .7
'                Daredevil and the Defenders v.1,0
' ******oin frenzy**********************************************************

Option Explicit
Randomize

Const BallSize = 50    ' 50 is the normal size
Const BallMass = 1     ' used in the Arcade Physics 3.0
Const SongVolume = 1 ' 1 is full volume. Value is from 0 to 1

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
'or False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = True

' Define any Constants
Const cGameName = "Daredevil_Pinball"
Const myVersion = "1.0"
Const MaxPlayers = 4
Const BallSaverTime = 15 ' value in seconds
Const MaxMultiplier = 99 ' almost no limit in this game
Const BallsPerGame = 3   ' 3 or 5
Const MaxMultiballs = 5  ' max number of balls during multiballs

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If

' Load the core.vbs for supporting Subs and functions
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
    On Error Goto 0
End Sub

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim Bonus
Dim BonusPoints(4)
Dim BonusMultiplier(4)
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue
Dim bAutoPlunger

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock
Dim BallsInHole

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJustStarted

Dim plungerIM 'used mostly as an autofire plunger
Dim ttable
Dim EggMagnet
Dim x

dim countr
countr = 0
FireTimer.enabled = 1


' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
    Randomize

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 46 ' Plunger Power
    Const IMTime = 0.25       ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    'Venusaur turntable
    Set ttable = New cvpmTurnTable
    With ttable
        .InitTurnTable Magnet1, 10
        .spinCW = False
        .MotorOn = True
        .CreateEvents "ttable"
    End With

    'Egg Magnet, it activates when the 4 eggs are lit
    Set EggMagnet = New cvpmMagnet
    With EggMagnet
        .X = 403
        .Y = 267
        .InitMagnet Magnet2, 90
        .GrabCenter = 1
        .CreateEvents "EggMagnet"
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    'load saved values, highscore, names, jackpot
    Loadhs

    'Init main variables
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    ' initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = False 'Set it to True if you don't want coins

    ' initialse any other flags
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInLock = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bJustStarted = True
    EndOfGame()

    ' Start the RealTime timer
    RealTime.Enabled = 1

    ' Load table color
    LoadLut
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If

    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        DOF 125, DOFOn
        If(Tilted = False)Then
            DMDFlush
            DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        PlungerIM.AutoFire:AnimateRattata
    End If
    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound SoundFX("fx_nudge", 0), 0, 1, 1, 0.25:CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    PlaySound "po_fanfare1"
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, ""
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        PlaySound "po_fanfare1"
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, ""

                        If Credits < 1 Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMDFlush
                            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, ""
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True)Then
                    If(BallsOnPlayfield = 0)Then
                        ResetForNewGame()
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            If Credits < 1 Then DOF 125, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, ""
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)

    If hsbModeActive Then EnterHighScoreKey(keycode)

' Table specific
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT
    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then SolLFlipper 0
        If keycode = RightFlipperKey Then SolRFlipper 0
    End If
End Sub

'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
    Savehs
    if B2SOn Then Controller.stop
End Sub


'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
        RotateLaneLightsLeft
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipperOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipperOn = 1
        RotateLaneLightsRight
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*********************************************************
' Real Time Flipper adjustments - by JLouLouLou & JPSalas
'        (to enable flipper tricks) 
'*********************************************************

Dim FlipperPower
Dim FlipperElasticity
Dim SOSTorque, SOSAngle
Dim FullStrokeEOS_Torque, LiveStrokeEOS_Torque
Dim LeftFlipperOn
Dim RightFlipperOn

Dim LLiveCatchTimer
Dim RLiveCatchTimer
Dim LiveCatchSensivity

FlipperPower = 5000
FlipperElasticity = 0.8
FullStrokeEOS_Torque = 0.3 	' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.2	' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

LeftFlipper.EOSTorqueAngle = 10
RightFlipper.EOSTorqueAngle = 10

SOSTorque = 0.1
SOSAngle = 6

LiveCatchSensivity = 10

LLiveCatchTimer = 0
RLiveCatchTimer = 0

LeftFlipper.TimerInterval = 1
LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's tricks timer
'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
	If LeftFlipperOn = 1 Then
		If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
			LeftFlipper.EOSTorque = FullStrokeEOS_Torque
			LLiveCatchTimer = LLiveCatchTimer + 1
			If LLiveCatchTimer < LiveCatchSensivity Then
				LeftFlipper.Elasticity = 0
			Else
				LeftFlipper.Elasticity = FlipperElasticity
				LLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		LeftFlipper.Elasticity = FlipperElasticity
		LeftFlipper.EOSTorque = LiveStrokeEOS_Torque
		LLiveCatchTimer = 0
	End If
	

'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
 	If RightFlipperOn = 1 Then
		If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
			RightFlipper.EOSTorque = FullStrokeEOS_Torque
			RLiveCatchTimer = RLiveCatchTimer + 1
			If RLiveCatchTimer < LiveCatchSensivity Then
				RightFlipper.Elasticity = 0
			Else
				RightFlipper.Elasticity = FlipperElasticity
				RLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		RightFlipper.Elasticity = FlipperElasticity
		RightFlipper.EOSTorque = LiveStrokeEOS_Torque
		RLiveCatchTimer = 0
	End If
End Sub

Sub RotateLaneLightsLeft
    Dim TempState
    TempState = LeftOutlane.State
    LeftOutlane.State = LeftInlane1.State
    LeftInlane1.State = LeftInlane2.State
    LeftInlane2.State = RightInlane.State
    RightInlane.State = RightOutlane.State
    RightOutlane.State = TempState
End Sub

Sub RotateLaneLightsRight
    Dim TempState
    TempState = RightOutlane.State
    RightOutlane.State = RightInlane.State
    RightInlane.State = LeftInlane2.State
    LeftInlane2.State = LeftInlane1.State
    LeftInlane1.State = LeftOutlane.State
    LeftOutlane.State = TempState
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt > TiltSensitivity)AND(Tilt < 15)Then 'show a warning
        DMD "_", CL(1, "CAREFUL"), "", eNone, eBlinkFast, eNone, 500, True, ""
    End if
    If Tilt > 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD "", CL(1, "TILT"), "", eNone, eNone, eNone, 200, False, ""
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        Me.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        Bumper3.Threshold = 100
        Bumper4.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        LeftSlingshot1.Disabled = 1
        RightSlingshot1.Disabled = 1
    Else
        'turn back on GI and the lights
        'GiOn
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        Bumper3.Threshold = 1
        Bumper4.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        LeftSlingshot1.Disabled = 0
        RightSlingshot1.Disabled = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        Me.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'********************
' Music as wav sounds
'********************

Dim Song
Song = ""

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            PlaySound Song, -1, SongVolume
        End If
    End If
End Sub

Sub PlayBattleSong
    Dim tmp
    tmp = INT(RND * 4)
    Select Case tmp
        Case 0:PlaySong "mu_battle1"
        Case 1:PlaySong "mu_battle2"
        Case 2:PlaySong "mu_battle3"
        Case 3:PlaySong "mu_battle4"
    End Select
End Sub

Sub ChangeSong
    If(BallsOnPlayfield = 0)Then
        PlaySong "mu_end"
    Else
        If bMultiballMode Then
            PlaySong "mu_multi"
        Else
            If bCatchemMode Then
                PlayBattleSong
            Else
                PlaySong "mu_main"
            End If
        End If
    End If
End Sub
'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = -1 Then
            GiOff
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

' GI light sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 25
            LightSeqGi.Play SeqBlinking, , 10, 15
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 15
            LightSeqGi.Play SeqBlinking, , 15, 10
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 25
            LightSeqInserts.Play SeqBlinking, , 10, 15
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 15
            LightSeqInserts.Play SeqBlinking, , 15, 10
    End Select
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v4.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.2, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v4.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls
Const lob = 0     'number of locked balls
Const maxvel = 45 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z - Ballsize / 2

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

'*****************************
' Ball 2 Ball Collision Sound
'*****************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_MetalWire":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_LongBands_Hit(idx):PlaySoundAtBall "fx_rubber_longband":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aRubber_Pegs_Hit(idx):PlaySoundAtBall "fx_rubber_peg":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub
Sub aTargets_Hit(idx):ActiveBall.VelZ = BallVel(Activeball) * (RND / 2):End Sub

' Pokémon sound efects and fanfares

Sub PlaySoundEffect()
    Dim n
    n = "po_effect" & INT(RND * 36) + 1
    PlaySound n, 0, 1, pan(ActiveBall)
End Sub

Sub PlayFanfare()
    Dim n
    n = "po_fanfare" & INT(RND * 6) + 1
    PlaySound n
End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

    'resets the score display, and turn off attrack mode
    StopAttractMode
    GiOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
        Coins(i) = 0
    Next

    ' initialise any other flags
    bMultiBallMode = False
    Tilt = 0

    ' initialise Game variables
    Game_Init()

    ' you may wish to start some music, play a sound, do whatever at this point

    ' set up the start delay to handle any Start of Game Attract Sequence
    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    AddScore 0

    ' set the current players bonus multiplier back down to 1X
    SetBonusMultiplier 1

    ' reset any drop targets, lights, game modes etc..
    'LightShootAgain.State = 0
    Bonus = 0
    bExtraBallWonThisBall = False
    ResetNewBallLights()

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    'bSkillShotReady = True 'no skillshot in this game

    'Change the music ?

    'Reset any table specific
    TargetBonus = 0
    BumperBonus = 0
    HoleBonus = 0
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedball BallSize / 2
    UpdateBallImage
    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4

' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield > 1 Then
        bMultiBallMode = True
        bAutoPlunger = True
        ChangeSong
    End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        If BallsOnPlayfield < MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
                Me.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            Me.Enabled = False
        End If
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded
'
Sub EndOfBall()
    Dim BonusDelayTime, AwardPoints, TotalBonus
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)
    If(Tilted = False)Then
        AwardPoints = 0:TotalBonus = 0

' add in any bonus points (multipled by the bonus multiplier)
'AwardPoints = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer)
'AddScore AwardPoints
'debug.print "Bonus Points = " & AwardPoints
'DMD "", CL(1, "BONUS: " & BonusPoints(CurrentPlayer) & " X" & BonusMultiplier(CurrentPlayer) ), "", eNone, eBlink, eNone, 1000, True, ""

'this table uses several bonus
        AwardPoints = TargetBonus * 1000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "TARGET BONUS: " & TargetBonus), "", eBlink, eNone, eNone, 800, False, "po_bonus1"

        AwardPoints = HoleBonus * 5000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "HOLE BONUS: " & HoleBonus), "", eBlink, eNone, eNone, 800, False, "po_bonus2"

        AwardPoints = PokemonBonusAward(CurrentPlayer) * 62500
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "VILLAINS CAUGHT: " & PokemonBonus(CurrentPlayer)), "", eBlink, eNone, eNone, 800, False, "po_bonus3"

        AwardPoints = EggBonus(CurrentPlayer) * 250000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "BASES FOUND: " & EggBonus(CurrentPlayer)), "", eBlink, eNone, eNone, 800, False, "po_bonus4"

        AwardPoints = BumperBonus * 100000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "BUMPER BONUS: " & BumperBonus), "", eBlink, eNone, eNone, 800, False, "po_bonus5"

        If Balls = BallsPerGame Then 'this is the last ball, add the coins left to the score
            AwardPoints = Coins(CurrentPlayer) * 1000
            TotalBonus = TotalBonus + AwardPoints
            DMD CL(0, FormatScore(AwardPoints)), CL(1, "COIN BONUS: " & Coins(CurrentPlayer)), "", eBlink, eNone, eNone, 800, False, "po_bonus6"
        End If

        DMD CL(0, FormatScore(TotalBonus)), CL(1, "TOTAL BONUS " & " X" & BonusMultiplier(CurrentPlayer)), "", eBlinkFast, eNone, eNone, 1500, True, "po_bonus7"
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        Addscore TotalBonus

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        BonusDelayTime = 7000
    Else
        'no bonus to count so move quickly to the next stage
        BonusDelayTime = 100
    End If
    ' start the end of ball timer which allows you to add a delay at this point
    vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the currentplayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0)Then
        'debug.print "Extra Ball"
        DMD "_", CL(1, "EXTRA BALL"), "_", eNone, eBlink, eNone, 1000, True, ""

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            'debug.print "No More Balls, High Score Entry"

            ' Submit the currentplayers score to the High Score system
            CheckHighScore()
        ' you may wish to play some music at this point

        Else

            ' not the last ball (for that player)
            ' if multiple players are playing then move onto the next one
            EndOfBallComplete()
        End If
    End If
End Sub

' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame > 1)Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer > PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode

        ' set the machine into game over mode
        EndOfGame()

    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then
        ChangeSong
    End If
    bJustStarted = False

    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all modes - eject locked balls

    ' set any lights for the attract mode
    GiOff
    StartAttractMode 1
' you may wish to light any Game Over Light you may have
End Sub

Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
    ' Destroy the ball
    Drain.DestroyBall
    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball

    BallsOnPlayfield = BallsOnPlayfield - 1
    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' is the ball saver active,
        If(bBallSaverActive = True)Then

            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            DMD "_", CL(1, "BALL SAVED"), "_", eNone, eBlinkfast, eNone, 800, True, ""
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ChangeGi white
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
                    ResetJackpotLights
                    ChangeSong
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
                ChangeSong
                ' handle the end of ball (change player, high score entry etc..)
                EndOfBall()
                ' End Modes and timers
                If bCatchemMode Then StopCatchem_Timer
                If bcoinfrenzy Then StopCoinFrenzyTimer_Timer
                If bPikachuTargetMode Then PikachuTargetTimer_Timer
                If bCharizardMode Then StopCharizardTimer_Timer
                If bRampBonus Then StopRampBonusTimer_Timer
                If bLoopBonus Then StopLoopBonusTimer_Timer
                ReduceBallType
                ChangeGi white
            End If
        End If
    End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
    PlaySoundAt "fx_sensor", swPlungerRest
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    LaunchLight.State = 2
    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        ' 'debug.print "autofire the ball"
        PlungerIM.AutoFire:AnimateRattata
        DOF 124, DOFPulse
        DOF 121, DOFPulse
        bAutoPlunger = False
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
        EnableBallSaver BallSaverTime
    End If
    'Start the skillshot if ready
    If bSkillShotReady Then
        ResetSkillShotTimer.Interval = 1000 * 5 ' 5 seconds
        ResetSkillShotTimer.Enabled = True
        LightSeqSkillshot.Play SeqAllOff
        LightSeqSkillshotHit.Play SeqBlinking, , 5, 150
    'PlaySound a sound
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
    DOF 141, DOFPulse
    DOF 121, DOFPulse
    ChangeSong
    DMDScoreNow
    ' turn off LaunchLight
    LaunchLight.State = 0
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimer.Interval = 1000 * seconds
    BallSaverTimer.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimer_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimer.Enabled = False
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
    LightShootAgain.State = 0
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
'
Sub AddScore(points)
    If(Tilted = False)Then
        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * BallType
    End if
' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board
'
Sub AddBonus(points)
    If(Tilted = False)Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    End if
' you may wish to check to see if the player has gotten a replay
End Sub

Sub AddCoin(n)
    If(Tilted = False)Then
        ' add the coins to the current players coin variable
        Coins(CurrentPlayer) = Coins(CurrentPlayer) + n
    ' update the score displays
    'DMDScore
    End if

    ' check if there is enough coins to enable the update ball
    If Coins(CurrentPlayer) > 249 Then
        BallUpdateLight.State = 2
    Else
        BallUpdateLight.State = 0
    End If
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If(Tilted = False)Then

        If(bMultiBallMode = True)Then
            Jackpot = Jackpot + points
        ' you may wish to limit the jackpot to a upper limit, ie..
        '	If (Jackpot >= 6000) Then
        '		Jackpot = 6000
        ' 	End if
        End if
    End if
End Sub

' Will increment the Bonus Multiplier to the next level
'
Sub IncrementBonusMultiplier()
    Dim NewBonusLevel

    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) < MaxMultiplier)then
        ' then set it the next next one AND set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + 1
        SetBonusMultiplier(NewBonusLevel)
    End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly
'
Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level

    ' If the multiplier is 1 then turn off all the bonus lights
    If(BonusMultiplier(CurrentPlayer) = 1)Then
        LightBonus2x.State = 0
        LightBonus3x.State = 0
        LightBigBonus.State = 0
    Else
        ' there is a bonus, turn on all the lights upto the current level
        If(BonusMultiplier(CurrentPlayer) >= 2)Then
            If(BonusMultiplier(CurrentPlayer) >= 2)Then
                LightBonus2x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 3)Then
                LightBonus3x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 4)Then
                LightBigBonus.state = 1
            End If
        End If
    ' etc..
    End If
End Sub

Sub IncrementBonus(Amount)
    Dim Value
    AddBonus Amount * 1000
    Bonus = Bonus + Amount
End Sub

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        DMD "_", CL(1, ("EXTRA BALL WON")), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        GiEffect 1
        LightEffect 2
    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL(1, ("EXTRA GAME WON")), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    Credits = Credits + 1
    DOF 125, DOFOn
    GiEffect 1
    LightEffect 1
End Sub

Sub AwardJackpot()
    DMD CL(0, FormatScore(Jackpot)), CL(1, ("JACKPOT")), "bkborder", eBlinkFast, eBlinkFast, eNone, 1000, True, ""
    DOF 126, DOFPulse
    PlayFanfare
    AddScore Jackpot
    AddJackpot 100000
    GiEffect 1
    LightEffect 2
End Sub

Sub AwardSuperJackpot()
    DMD CL(0, FormatScore(Jackpot * PokemonLevel)), CL(1, ("SUPERJACKPOT")), "bkborder", eBlinkFast, eBlinkFast, eNone, 1000, True, ""
    DOF 126, DOFPulse
    PlayFanfare
    AddScore Jackpot * PokemonLevel
    AddJackpot 100000
    GiEffect 1
    LightEffect 2
End Sub

Sub AwardSkillshot()
    DMD CL(0, FormatScore(SkillshotValue)), CL(1, ("SKILLSHOT")), "bkborder", eBlinkFast, eBlink, eNone, 1000, True, ""
    DOF 127, DOFPulse
    AddScore SkillshotValue
    ResetSkillShotTimer_Timer
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 100000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 100000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 100000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 100000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(cGameName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(cGameName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
    SaveValue cGameName, "HighScore1", HighScore(0)
    SaveValue cGameName, "HighScore1Name", HighScoreName(0)
    SaveValue cGameName, "HighScore2", HighScore(1)
    SaveValue cGameName, "HighScore2Name", HighScoreName(1)
    SaveValue cGameName, "HighScore3", HighScore(2)
    SaveValue cGameName, "HighScore3Name", HighScoreName(2)
    SaveValue cGameName, "HighScore4", HighScore(3)
    SaveValue cGameName, "HighScore4Name", HighScoreName(3)
    SaveValue cGameName, "Credits", Credits
    SaveValue cGameName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
    HighScoreName(0) = "AAA"
    HighScoreName(1) = "BBB"
    HighScoreName(2) = "CCC"
    HighScoreName(3) = "DDD"
    HighScore(0) = 1500000
    HighScore(1) = 1400000
    HighScore(2) = 1300000
    HighScore(3) = 1200000
    Savehs
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
    Dim tmp
    tmp = Score(CurrentPlayer)

    If tmp > HighScore(0)Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
        DOF 125, DOFOn
    End If

    If tmp > HighScore(3)Then
        PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    PlaySound "vo_enterinitials"
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow
    hsCurrentLetter = 1
    DMDFlush()
    HighScoreDisplayNameNow()

    HighScoreFlashTimer.Interval = 250
    HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit > 0)then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayNameNow()
        end if
    end if
End Sub

Sub HighScoreDisplayNameNow()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreDisplayName()
    Dim i
    Dim TempTopStr
    Dim TempBotStr

    TempTopStr = "YOUR NAME:"
    dLine(0) = ExpandLine(TempTopStr, 0)
    DMDUpdate 0

    TempBotStr = "    > "
    if(hsCurrentDigit > 0)then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit > 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit > 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3)then
        if(hsLetterFlash <> 0)then
            TempBotStr = TempBotStr & "_"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit < 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit < 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    dLine(1) = ExpandLine(TempBotStr, 1)
    DMDUpdate 1
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2)then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False

    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ")then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
    EndOfBallComplete()
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) < HighScore(j + 1)Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
End Sub

' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text. 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Const dqSize = 64

Dim dqHead
Dim dqTail
Dim deSpeed
Dim deBlinkSlowRate
Dim deBlinkFastRate

Dim dCharsPerLine(2)
Dim dLine(2)
Dim deCount(2)
Dim deCountEnd(2)
Dim deBlinkCycle(2)

Dim dqText(2, 64)
Dim dqEffect(2, 64)
Dim dqTimeOn(64)
Dim dqbFlush(64)
Dim dqSound(64)

Dim FlexDMD
Dim DMDScene

Sub DMD_Init() 'default/startup values
    If UseFlexDMD Then
        Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
        If Not FlexDMD is Nothing Then
            If FlexDMDHighQuality Then
                FlexDMD.TableFile = Table1.Filename & ".vpx"
                FlexDMD.RenderMode = 2
                FlexDMD.Width = 256
                FlexDMD.Height = 64
                FlexDMD.Clear = True
                FlexDMD.GameName = cGameName
                FlexDMD.Run = True
                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + i * 12, 6, 12, 22
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + (i - 20) * 12, 34, 12, 22
                Next
                FlexDMD.LockRenderThread
                FlexDMD.Stage.AddActor DMDScene
                FlexDMD.UnlockRenderThread
            Else
                FlexDMD.TableFile = Table1.Filename & ".vpx"
                FlexDMD.RenderMode = 2
                FlexDMD.Width = 128
                FlexDMD.Height = 32
                FlexDMD.Clear = True
                FlexDMD.GameName = cGameName
                FlexDMD.Run = True
                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3, 6, 11
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + (i - 20) * 6, 17, 6, 11
                Next
                FlexDMD.LockRenderThread
                FlexDMD.Stage.AddActor DMDScene
                FlexDMD.UnlockRenderThread
            End If
        End If
    End If

    Dim i, j
    DMDFlush()
    deSpeed = 20
    deBlinkSlowRate = 5
    deBlinkFastRate = 2
    dCharsPerLine(0) = 20 'characters lower line
    dCharsPerLine(1) = 20 'characters top line
    dCharsPerLine(2) = 1  'characters back line
    For i = 0 to 2
        dLine(i) = Space(dCharsPerLine(i))
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
    For i = 0 to 2
        For j = 0 to 64
            dqText(i, j) = ""
            dqEffect(i, j) = eNone
        Next
    Next
    DMD dLine(0), dLine(1), dLine(2), eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDFlush()
    Dim i
    DMDTimer.Enabled = False
    DMDEffectTimer.Enabled = False
    dqHead = 0
    dqTail = 0
    For i = 0 to 2
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
    Next
End Sub

Sub DMDScore()
    Dim tmp, tmp1, tmp2
    if(dqHead = dqTail)Then
        tmp = RL(0, FormatScore(Score(Currentplayer)))
        'tmp = CL(0, FormatScore(Score(Currentplayer) ) )
        tmp1 = CL(1, "PLAYER" & CurrentPlayer & " [" & Balls & " \" & PokemonBonus(CurrentPlayer) & " ]" & Coins(Currentplayer))
        'tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
        'tmp1 = FormatScore(Bonuspoints(Currentplayer) ) & " X" &BonusMultiplier(Currentplayer)
        tmp2 = ""
        If bCatchemMode Then
            tmp1 = RL(1, CatchMaxHits-CatchHits & " HITS LEFT")
            tmp2 = pokemon(CatchID)
        End If
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail < dqSize)Then
        if(Text0 = "_")Then
            dqEffect(0, dqTail) = eNone
            dqText(0, dqTail) = "_"
        Else
            dqEffect(0, dqTail) = Effect0
            dqText(0, dqTail) = ExpandLine(Text0, 0)
        End If

        if(Text1 = "_")Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1, 1)
        End If

        if(Text2 = "_")Then
            dqEffect(2, dqTail) = eNone
            dqText(2, dqTail) = "_"
        Else
            dqEffect(2, dqTail) = Effect2
            dqText(2, dqTail) = Text2 'it is always 1 letter in this table
        End If

        dqTimeOn(dqTail) = TimeOn
        dqbFlush(dqTail) = bFlush
        dqSound(dqTail) = Sound
        dqTail = dqTail + 1
        if(dqTail = 1)Then
            DMDHead()
        End If
    End If
End Sub

Sub DMDHead()
    Dim i
    deCount(0) = 0
    deCount(1) = 0
    deCount(2) = 0
    DMDEffectTimer.Interval = deSpeed

    For i = 0 to 2
        Select Case dqEffect(i, dqHead)
            Case eNone:deCountEnd(i) = 1
            Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead))
            Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead))
            Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
            Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
        End Select
    Next
    if(dqSound(dqHead) <> "")Then
        PlaySound(dqSound(dqHead))
    End If
    DMDEffectTimer.Enabled = True
End Sub

Sub DMDEffectTimer_Timer()
    DMDEffectTimer.Enabled = False
    DMDProcessEffectOn()
End Sub

Sub DMDTimer_Timer()
    Dim Head
    DMDTimer.Enabled = False
    Head = dqHead
    dqHead = dqHead + 1
    if(dqHead = dqTail)Then
        if(dqbFlush(Head) = True)Then
            DMDScoreNow()
        Else
            dqHead = 0
            DMDHead()
        End If
    Else
        DMDHead()
    End If
End Sub

Sub DMDProcessEffectOn()
    Dim i
    Dim BlinkEffect
    Dim Temp

    BlinkEffect = False

    For i = 0 to 2
        if(deCount(i) <> deCountEnd(i))Then
            deCount(i) = deCount(i) + 1

            select case(dqEffect(i, dqHead))
                case eNone:
                    Temp = dqText(i, dqHead)
                case eScrollLeft:
                    Temp = Right(dLine(i), dCharsPerLine(i)- 1)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), (dCharsPerLine(i) + 1)- deCount(i), 1)
                    Temp = Temp & Left(dLine(i), dCharsPerLine(i)- 1)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkSlowRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i))
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkFastRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i))
                    End If
            End Select

            if(dqText(i, dqHead) <> "_")Then
                dLine(i) = Temp
                DMDUpdate i
            End If
        End If
    Next

    if(deCount(0) = deCountEnd(0))and(deCount(1) = deCountEnd(1))and(deCount(2) = deCountEnd(2))Then

        if(dqTimeOn(dqHead) = 0)Then
            DMDFlush()
        Else
            if(BlinkEffect = True)Then
                DMDTimer.Interval = 10
            Else
                DMDTimer.Interval = dqTimeOn(dqHead)
            End If

            DMDTimer.Enabled = True
        End If
    Else
        DMDEffectTimer.Enabled = True
    End If
End Sub

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(dCharsPerLine(id))
    Else
        if(Len(TempStr) > Space(dCharsPerLine(id)))Then
            TempStr = Left(TempStr, Space(dCharsPerLine(id)))
        Else
            if(Len(TempStr) < dCharsPerLine(id))Then
                TempStr = TempStr & Space(dCharsPerLine(id)- Len(TempStr))
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

    NumString = CStr(abs(Num))

    For i = Len(NumString)-3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1))then
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1)) + 48) & right(NumString, Len(NumString)- i)
        end if
    Next
    FormatScore = NumString
End function

Function CL(id, NumString)
    Dim Temp, TempStr
    Temp = (dCharsPerLine(id)- Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(id, NumString)
    Dim Temp, TempStr
    Temp = dCharsPerLine(id)- Len(NumString)
    TempStr = Space(Temp) & NumString
    RL = TempStr
End Function

'**************
' Update DMD
'**************

Sub DMDUpdate(id)
    Dim digit, value
    If UseFlexDMD Then FlexDMD.LockRenderThread
    Select Case id
        Case 0 'top text line
            For digit = 0 to 19
                DMDDisplayChar mid(dLine(0), digit + 1, 1), digit
            Next
        Case 1 'bottom text line
            For digit = 20 to 39
                DMDDisplayChar mid(dLine(1), digit -19, 1), digit
            Next
        Case 2 ' back image - back animations
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "bkempty"
            Digits(40).ImageA = dLine(2)
            If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
    End Select
    If UseFlexDMD Then FlexDMD.UnlockRenderThread
End Sub

Sub DMDDisplayChar(achar, adigit)
    If achar = "" Then achar = " "
    achar = ASC(achar)
    Digits(adigit).ImageA = Chars(achar)
    If UseFlexDMD Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap
End Sub

'****************************
' JP's new DMD using flashers
'****************************

Dim Digits, Chars(255), Images(255)

DMDInit

Sub DMDInit
    Dim i
    Digits = Array(digit001, digit002, digit003, digit004, digit005, digit006, digit007, digit008, digit009, digit010, _
        digit011, digit012, digit013, digit014, digit015, digit016, digit017, digit018, digit019, digit020,            _
        digit021, digit022, digit023, digit024, digit025, digit026, digit027, digit028, digit029, digit030,            _
        digit031, digit032, digit033, digit034, digit035, digit036, digit037, digit038, digit039, digit040,            _
        digit041)
    For i = 0 to 255:Chars(i) = "dempty":Next

    Chars(32) = "dempty" 'space
    Chars(33) = "dexcl"  '!
    '    Chars(34) = '"
    '    Chars(36) = '$
    '    Chars(39) = ''
    '    Chars(42) = '*
    '    Chars(43) = '+
    '    Chars(45) = '-
    Chars(46) = "ddot"  '.
    '    Chars(47) = '/
    Chars(48) = "d0"     '0
    Chars(49) = "d1"     '1
    Chars(50) = "d2"     '2
    Chars(51) = "d3"     '3
    Chars(52) = "d4"     '4
    Chars(53) = "d5"     '5
    Chars(54) = "d6"     '6
    Chars(55) = "d7"     '7
    Chars(56) = "d8"     '8
    Chars(57) = "d9"     '9
    Chars(60) = "dless"  '<
    Chars(61) = "dequal" '=
    Chars(62) = "dmore"  '>
    '   Chars(64) = '@
    Chars(65) = "da"    'A
    Chars(66) = "db"    'B
    Chars(67) = "dc"    'C
    Chars(68) = "dd"    'D
    Chars(69) = "de"    'E
    Chars(70) = "df"    'F
    Chars(71) = "dg"    'G
    Chars(72) = "dh"    'H
    Chars(73) = "di"    'I
    Chars(74) = "dj"    'J
    Chars(75) = "dk"    'K
    Chars(76) = "dl"    'L
    Chars(77) = "dm"    'M
    Chars(78) = "dn"    'N
    Chars(79) = "do"    'O
    Chars(80) = "dp"    'P
    Chars(81) = "dq"    'Q
    Chars(82) = "dr"    'R
    Chars(83) = "ds"    'S
    Chars(84) = "dt"    'T
    Chars(85) = "du"    'U
    Chars(86) = "dv"    'V
    Chars(87) = "dw"    'W
    Chars(88) = "dx"    'X
    Chars(89) = "dy"    'Y
    Chars(90) = "dz"    'Z
    Chars(91) = "dball" '[
    Chars(92) = "dpika" '|
    Chars(93) = "dcoin" ']
    '    Chars(94) = '^
    '    Chars(95) = '_
    Chars(96) = "d0a"  '0.
    Chars(97) = "d1a"  '1.
    Chars(98) = "d2a"  '2.
    Chars(99) = "d3a"  '3.
    Chars(100) = "d4a" '4.
    Chars(101) = "d5a" '5.
    Chars(102) = "d6a" '6.
    Chars(103) = "d7a" '7.
    Chars(104) = "d8a" '8.
    Chars(105) = "d9a" '9
End Sub

'********************
' Real Time updatess
'********************
'used for all the real time updates

Sub RealTime_Timer
    RollingUpdate
    coin.RotX = Spinner.CurrentAngle
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
End Sub

'********************************************************************************************
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

    If TypeName(MyLight) = "Light" Then

        If FinalState = 2 Then
            FinalState = MyLight.State 'Keep the current light state
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then

        Dim steps
        ' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
        If FinalState = 2 Then                      'Keep the current flasher state
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state
        ' Start blink timer and create timer subroutine
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub

'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 12 colors: red, orange, amber, yellow...
'******************************************

'colors
Const red = 1
Const orange = 2
Const amber = 3
Const yellow = 4
Const darkgreen = 5
Const green = 6
Const blue = 7
Const darkblue = 8
Const purple = 9
Const white = 10
Const teal = 11
Const ledwhite = 12

Sub SetLightColor(n, col, stat) 'stat 0 = off, 1 = on, 2 = blink, -1= no change
    Select Case col
        Case red
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(255, 0, 0)
        Case orange
            n.color = RGB(18, 3, 0)
            n.colorfull = RGB(255, 64, 0)
        Case amber
            n.color = RGB(193, 49, 0)
            n.colorfull = RGB(255, 153, 0)
        Case yellow
            n.color = RGB(18, 18, 0)
            n.colorfull = RGB(255, 255, 0)
        Case darkgreen
            n.color = RGB(0, 8, 0)
            n.colorfull = RGB(0, 64, 0)
        Case green
            n.color = RGB(0, 16, 0)
            n.colorfull = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 18, 18)
            n.colorfull = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 8, 8)
            n.colorfull = RGB(0, 64, 64)
        Case purple
            n.color = RGB(64, 0, 96)
            n.colorfull = RGB(128, 0, 192)
        Case white 'bulb
            n.color = RGB(193, 91, 0)
            n.colorfull = RGB(255, 197, 143)
        Case teal
            n.color = RGB(1, 64, 62)
            n.colorfull = RGB(2, 128, 126)
        Case ledwhite
            n.color = RGB(255, 197, 143)
            n.colorfull = RGB(255, 252, 224)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

Sub SetFlashColor(n, col, stat) 'stat 0 = off, 1 = on, -1= no change - no blink for the flashers, use FlashForMs
    Select Case col
        Case red
            n.color = RGB(255, 0, 0)
        Case orange
            n.color = RGB(255, 64, 0)
        Case amber
            n.color = RGB(255, 153, 0)
        Case yellow
            n.color = RGB(255, 255, 0)
        Case darkgreen
            n.color = RGB(0, 64, 0)
        Case green
            n.color = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 64, 64)
        Case purple
            n.color = RGB(128, 0, 192)
        Case white 'bulb
            n.color = RGB(255, 197, 143)
        Case teal
            n.color = RGB(2, 128, 126)
        Case ledwhite
            n.color = RGB(255, 252, 224)
    End Select
    If stat <> -1 Then
        n.Visible = stat
    End If
End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim ii
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER1 " &FormatScore(Score(1))), "", eNone, eNone, eNone, 3000, False, ""
        If PokemonBonus(1)Then
            DMD "", "VILLAINS CAUGHT " & PokemonBonus(1), "", eNone, eNone, eNone, 1000, False, ""
            For ii = 1 to PokemonBonus(1)
                DMD "", RL(1, Pokename(PokemonCaught(1, ii))), Pokemon(PokemonCaught(1, ii)), eNone, eNone, eNone, 1000, False, ""
            Next
            DMD "", "BASES " & EggBonus(1), "", eNone, eNone, eNone, 1000, False, ""
        End If
    End If
    If Score(2)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 3000, False, ""
        If PokemonBonus(2)Then
            DMD "", "VILLAINS CAUGHT " & PokemonBonus(2), "", eNone, eNone, eNone, 1000, False, ""
            For ii = 1 to PokemonBonus(2)
                DMD "", RL(1, Pokename(PokemonCaught(2, ii))), Pokemon(PokemonCaught(2, ii)), eNone, eNone, eNone, 1000, False, ""
            Next
            DMD "", "BASES " & EggBonus(2), "", eNone, eNone, eNone, 1000, False, ""
        End If
    End If
    If Score(3)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 3000, False, ""
        If PokemonBonus(3)Then
            DMD "", "VILLAINS CAUGHT " & PokemonBonus(3), "", eNone, eNone, eNone, 1000, False, ""
            For ii = 1 to PokemonBonus(3)
                DMD "", RL(1, Pokename(PokemonCaught(3, ii))), Pokemon(PokemonCaught(3, ii)), eNone, eNone, eNone, 1000, False, ""
            Next
            DMD "", "BASES " & EggBonus(3), "", eNone, eNone, eNone, 1000, False, ""
        End If
    End If
    If Score(4)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 3000, False, ""
        If PokemonBonus(4)Then
            DMD "", "VILLAINS CAUGHT " & PokemonBonus(4), "", eNone, eNone, eNone, 1000, False, ""
            For ii = 1 to PokemonBonus(4)
                DMD "", RL(1, Pokename(PokemonCaught(4, ii))), Pokemon(PokemonCaught(4, ii)), eNone, eNone, eNone, 1000, False, ""
            Next
            DMD "", "BASES " & EggBonus(4), "", eNone, eNone, eNone, 1000, False, ""
        End If
    End If
    DMD "", CL(1, "GAME OVER"), "", eNone, eBlink, eNone, 2000, False, ""
    If bFreePlay Then
        DMD "", CL(1, "FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits > 0 Then
            DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "        ZANDYSARCADE", "          PRESENTS", "jppresents", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "pokemonpinball", eNone, eNone, eNone, 4000, False, ""
    DMD "", CL(1, "ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0)), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode(dummy)
    ChangeSong
    StartLightSeq
    DMDFlush
    ShowTableInfo
End Sub

Sub StopAttractMode
    Dim bulb
    DMDScoreNow
    LightSeqAttract.StopPlay
'StopSong
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 40, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 40, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqRightOn, 30, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqLeftOn, 30, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe1VertOn, 50, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

'************************************
'       LUT - Darkness control
' 10 normal level & 10 warmer levels 
'************************************

Dim bLutActive, LUTImage

Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "")Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 22:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0"
        Case 1:table1.ColorGradeImage = "LUT1"
        Case 2:table1.ColorGradeImage = "LUT2"
        Case 3:table1.ColorGradeImage = "LUT3"
        Case 4:table1.ColorGradeImage = "LUT4"
        Case 5:table1.ColorGradeImage = "LUT5"
        Case 6:table1.ColorGradeImage = "LUT6"
        Case 7:table1.ColorGradeImage = "LUT7"
        Case 8:table1.ColorGradeImage = "LUT8"
        Case 9:table1.ColorGradeImage = "LUT9"
        Case 10:table1.ColorGradeImage = "LUT10"
        Case 11:table1.ColorGradeImage = "LUT Warm 0"
        Case 12:table1.ColorGradeImage = "LUT Warm 1"
        Case 13:table1.ColorGradeImage = "LUT Warm 2"
        Case 14:table1.ColorGradeImage = "LUT Warm 3"
        Case 15:table1.ColorGradeImage = "LUT Warm 4"
        Case 16:table1.ColorGradeImage = "LUT Warm 5"
        Case 17:table1.ColorGradeImage = "LUT Warm 6"
        Case 18:table1.ColorGradeImage = "LUT Warm 7"
        Case 19:table1.ColorGradeImage = "LUT Warm 8"
        Case 20:table1.ColorGradeImage = "LUT Warm 9"
        Case 21:table1.ColorGradeImage = "LUT Warm 10"
    End Select
End Sub

Dim GiIntensity
GiIntensity = 1   'can be used by the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

' New LUT postit
Function GetHSChar(String, Index)
    Dim ThisChar
    Dim FileName
    ThisChar = Mid(String, Index, 1)
    FileName = "PostIt"
    If ThisChar = " " or ThisChar = "" then
        FileName = FileName & "BL"
    ElseIf ThisChar = "<" then
        FileName = FileName & "LT"
    ElseIf ThisChar = "_" then
        FileName = FileName & "SP"
    Else
        FileName = FileName & ThisChar
    End If
    GetHSChar = FileName
End Function

Sub SetLUTLine(String)
    Dim Index
    Dim xFor
    Index = 1
    LUBack.imagea="PostItNote"
    For xFor = 1 to 40
        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
SetLUTLine ""
LUBack.imagea="PostitBL"
End Sub


'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' tables walls and animations
Sub VPObjects_Init
End Sub

' tables variables and modes init

Dim Coins(4)
Dim BumperBonus, EggBonus(4), HoleBonus, PokemonBonus(4), PokemonBonusAward(4), PokemonCaught(4, 151), TargetBonus

Sub Game_Init()
    Dim ii, jj
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    'Play some Music
    ChangeSong
    'Init Variables
    Jackpot = 250000
    RattataPos = 0
    bumperHits = 100
    BumperBonus = 0
    PikachuHits = 0
    CatchHits = 0
    BallInHole = 0
    HoleBonus = 0
    For ii = 1 to 4
        PokemonBonus(ii) = 0
        PokemonBonusAward(ii) = 0
        EggBonus(ii) = 0
        For jj = 0 to 151
            PokemonCaught(ii, jj) = 0
        Next
    Next
    TargetBonus = 0
    PikachuTargetValue = 5000
    ResetPokemonLevel
    ResetHoleLights
    BallType = 1:UpdateBallType
    bCatchemMode = False
    bEggTargetsCompleted = False
    bLockEnabled = False
    LockedBalls = 0
    bcoinfrenzy = False
    coinstep = 0
    bPikachuTargetMode = False
    bCharizardMode = False
    bRampBonus = FALSE
    bLoopBonus = FALSE
'Init Delays
'Skillshot Init
'MainModes Init()
End Sub

Sub ResetSkillShotTimer_Timer
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
    HatchLight1b.Visible = 0
    HatchLight2b.Visible = 0
    HatchLight3b.Visible = 0
    HatchLight4b.Visible = 0
End Sub

Sub ResetNewBallLights()
    LightArrow1.State = 2
    LightArrow6.State = 2
    l53.State = 2
End Sub

Dim RattataPos

Sub AnimateRattata()
    RattataPos = 0
    RattataTimer.Enabled = 1
End Sub

Sub RattataTimer_Timer()
    Select Case RattataPos
        Case 0:Rattata.TransY = 25:RattataPos = 1
        Case 1:Rattata.TransY = 15:RattataPos = 2
        Case 2:Rattata.TransY = 7:RattataPos = 3
        Case 3:Rattata.TransY = 3:RattataPos = 4
        Case 4:Rattata.TransY = 0:RattataTimer.Enabled = 0
    End Select
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/modes this trigger is a member of
' - set the "LastSwicthHit" variable in case it is needed later
' *********************************************************************

' Slingshots has been hit

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Lemk
    DOF 105, DOFPulse
    LeftSling4.Visible = 1:LeftSling1.Visible = 0
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 110
    ' add some effect to the table?
    Bulbasaur.RotY = 26
    Gi2.State = 0
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14:Bulbasaur.RotY = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2:Bulbasaur.RotY = 2
        Case 3:LeftSLing2.Visible = 0:LeftSling1.Visible = 1:Lemk.RotX = -10:Bulbasaur.RotY = 0:Gi2.State = 1:LeftSlingShot.TimerEnabled = False
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 104, DOFPulse, DOFcontactors), Remk
    DOF 106, DOFPulse
    RightSling4.Visible = 1:RightSling1.Visible = 0
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 110
    ' add some effect to the table?
    Squirtle.RotY = -26
    Gi1.State = 0
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14:Squirtle.RotY = -14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2:Squirtle.RotY = -2
        Case 3:RightSLing2.Visible = 0:RightSLing1.Visible = 1:Remk.RotX = -10:Squirtle.RotY = 0:Gi1.State = 1:RightSlingShot.TimerEnabled = False
    End Select
    RStep = RStep + 1
End Sub

' Hatch Slingshots

Dim LStep1, RStep1

Sub LeftSlingShot1_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 143, DOFPulse, DOFContactors), Lemk1
    Sling3a.Visible = 1:Sling3.Visible = 0
    Lemk1.RotX = 26
    LStep1 = 0
    LeftSlingShot1.TimerEnabled = True
    ' add some points
    AddScore 510
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot1"
' add some effect to the table?
End Sub

Sub LeftSlingShot1_Timer
    Select Case LStep1
        Case 1:Sling3a.Visible = 0:Sling3b.Visible = 1:Lemk1.RotX = 14
        Case 2:Sling3b.Visible = 0:Sling3c.Visible = 1:Lemk.RotX = 0
        Case 3:Sling3c.Visible = 0:Sling3.Visible = 1:Lemk1.RotX = -20:LeftSlingShot1.TimerEnabled = False
    End Select
    LStep1 = LStep1 + 1
End Sub

Sub RightSlingShot1_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 144, DOFPulse, DOFContactors), Remk1
    Sling2a.Visible = 1:Sling2.Visible = 0
    Remk1.RotX = 26
    RStep1 = 0
    RightSlingShot1.TimerEnabled = True
    ' add some points
    AddScore 510
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot1"
' add some effect to the table?
End Sub

Sub RightSlingShot1_Timer
    Select Case RStep1
        Case 1:Sling2a.Visible = 0:Sling2b.Visible = 1:Remk.RotX = 14
        Case 2:Sling2b.Visible = 0:Sling2c.Visible = 1:Remk.RotX = 0
        Case 3:Sling2c.Visible = 0:Sling2.Visible = 1:Remk1.RotX = -20:RightSlingShot1.TimerEnabled = False
    End Select
    RStep1 = RStep1 + 1
End Sub

'**********
' Spinner
'**********

Sub Spinner_Spin()
    If Tilted Then Exit Sub
    PlaySoundAt "fx_spinner", Spinner
    DOF 136, DOFPulse
    AddScore 1000
    AddCoin 1
End Sub

'*********************
' Inlanes - Outlanes
'*********************

Sub LeftOutlaneTrigger_Hit()
    DOF 132, DOFPulse
    PlaySoundAt "fx_sensor", LeftOutlaneTrigger
    If NOT bMultiBallMode Then PlaySound"quoteL"&RndNbr(7)
    If Tilted Then Exit Sub
    PlaySoundEffect
    AddScore 10000
    Addcoin 50
    FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
    FlashForMs Light9, 1000, 50, 0
    FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
    FlashForMs Light10, 1000, 50, 0
    TargetBonus = TargetBonus + 1
    LeftOutlane.State = 1
    LastSwitchHit = "LeftOutlaneTrigger"
    CheckMultiplier
End Sub

Sub LeftInlaneTrigger1_Hit()
    DOF 133, DOFPulse
    
    PlaySoundAt "fx_sensor", LeftInlaneTrigger1
    If Tilted Then Exit Sub
    PlaySoundEffect
    AddScore 3000
    Addcoin 10
    TargetBonus = TargetBonus + 1
    LeftInlane1.State = 1
    LastSwitchHit = "LeftInlaneTrigger1"
    CheckMultiplier
End Sub

Sub LeftInlaneTrigger2_Hit()
    DOF 133, DOFPulse
    PlaySoundAt "fx_sensor", LeftInlaneTrigger2
    If Tilted Then Exit Sub
    PlaySoundEffect
    AddScore 3000
    Addcoin 10
    TargetBonus = TargetBonus + 1
    LeftInlane2.State = 1
    LastSwitchHit = "LeftInlaneTrigger2"
    CheckMultiplier
End Sub

Sub RightInlaneTrigger_Hit()
    DOF 134, DOFPulse
    PlaySoundAt "fx_sensor", RightInlaneTrigger
    If Tilted Then Exit Sub
    PlaySoundEffect
    AddScore 3000
    Addcoin 10
    TargetBonus = TargetBonus + 1
    RightInlane.State = 1
    CheckMultiplier
    LastSwitchHit = "RightInlaneTrigger"
End Sub

Sub RightOutlaneTrigger_Hit()
    DOF 135, DOFPulse
    PlaySoundAt "fx_sensor", RightOutlaneTrigger
    If NOT bMultiBallMode Then PlaySound"quoteI"&RndNbr(6)
    StartIron
    If Tilted Then Exit Sub
    PlaySoundEffect
    AddScore 10000
    Addcoin 50
    FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
    FlashForMs Light9, 1000, 50, 0
    FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
    FlashForMs Light10, 1000, 50, 0
    TargetBonus = TargetBonus + 1
    RightOutlane.State = 1
    CheckMultiplier
    LastSwitchHit = "RightOutlaneTrigger"
End Sub

Sub CheckMultiplier
    If(LeftOutlane.State = 1)And(LeftInlane1.State = 1)And(LeftInlane2.State = 1)And(RightInlane.State = 1)And(RightOutlane.State = 1)Then
        AddScore 50000
        PlayFanfare
        LightSeqLanes.Play SeqRandom, 5, , 3000
        IncrementBonusMultiplier()
        LeftOutlane.State = 0
        LeftInlane1.State = 0
        LeftInlane2.State = 0
        RightInlane.State = 0
        RightOutlane.State = 0
    End If
End Sub

Sub EggEntrance_Hit() 'diode switch used for catching pokemons & other modes
    If bcoinfrenzy AND CoinLight2.State = 2 Then
        Addcoin 50
        PlaySound "fx_coins"
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
        FlashForMs Light9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
        FlashForMs Light10, 1000, 50, 0
        CoinFrenzyTimer_Timer
    End If
    If bCatchemMode Then
        If CatchLight2.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
            CheckCatchHits
        End If
    End If
End Sub

'********
' Bumper
'********

Dim bumperHits

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper1
    DOF 138, DOFPulse
    FlashForMs BLight001, 1000, 50, 0
    AddScore 500 + 4500 * ABS(Flasher1.Visible) 'a bumper scores 500 points and 5000 points when lit
    bumperHits = bumperHits - 1
    DMDFlush
    DMD RL(0, FormatScore(Score(Currentplayer))), RL(1, bumperHits& " BUMPER HITS LEFT"), "bumper", eNone, eNone, eNone, 300, True, ""
    CheckBumpers
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 110, DOFPulse, DOFContactors), Bumper2
    DOF 140, DOFPulse
    FlashForMs BLight002, 1000, 50, 0
    AddScore 500 + 4500 * ABS(Flasher2.Visible) 'a bumper scores 500 points and 5000 points when lit
    bumperHits = bumperHits - 1
    DMDFlush
    DMD RL(0, FormatScore(Score(Currentplayer))), RL(1, bumperHits& " BUMPER HITS LEFT"), "bumper", eNone, eNone, eNone, 300, True, ""
    CheckBumpers
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper3
    DOF 137, DOFPulse
    FlashForMs BLight003, 1000, 50, 0
    AddScore 500 + 4500 * ABS(Flasher3.Visible) 'a bumper scores 500 points and 5000 points when lit
    bumperHits = bumperHits - 1
    DMDFlush
    DMD RL(0, FormatScore(Score(Currentplayer))), RL(1, bumperHits& " BUMPER HITS LEFT"), "bumper", eNone, eNone, eNone, 300, True, ""
    CheckBumpers
End Sub

Sub Bumper4_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), Bumper4
        DOF 139, DOFPulse
    FlashForMs BLight004, 1000, 50, 0
    AddScore 500 + 4500 * ABS(Flasher4.Visible) 'a bumper scores 500 points and 5000 points when lit
    bumperHits = bumperHits - 1
    DMDFlush
    DMD RL(0, FormatScore(Score(Currentplayer))), RL(1, bumperHits& " BUMPER HITS LEFT"), "bumper", eNone, eNone, eNone, 300, True, ""
    CheckBumpers
End Sub

' Bumper Bonus
' 100000 i bonus after each 100 hits

Sub CheckBumpers()
    If bumperHits <= 0 Then
        BumperBonus = BumperBonus + 1
        DMD "_", RL(1, "BUMPERS BONUS " & BumperBonus), "", eNone, eBlink, eNone, 500, True, ""
        bumperHits = 100
    ' do something more
    End If
End Sub

Sub ResetBumpers()
    bumperHits = 100
    Flasher1.Visible = 0
    Flasher2.Visible = 0
    Flasher3.Visible = 0
    Flasher4.Visible = 0
End Sub

'**************************
' Upper Lanes & loop switch
'**************************

Dim PokemonLevel, HolePos
Sub RightLoopTrigger_Hit() 'set the pokemon levels
    DOF 131, DOFPulse
    If NOT bMultiBallMode Then PlaySound"quoteC"&RndNbr(12)
    StartGun
    PlaySoundAt "fx_sensor", RightLoopTrigger
    If Tilted Then Exit Sub
    TargetBonus = TargetBonus + 1
    If LastSwitchHit = "LeftInlaneTrigger2" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("20000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 20000
    End If
    If bCatchemMode Then
        If CatchLight5.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
            CheckCatchHits
        End If
    Else
        If PokemonLevel < 4 Then
            PokemonLevel = PokemonLevel + 1
            UpdatePokemonLevel
        End If
        AddScore 2500 * PokemonLevel
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightLoopTrigger"
End Sub

Sub LeftLoopTrigger_Hit()
    DOF 130, DOFPulse
    PlaySoundAt "fx_sensor", LeftLoopTrigger
    If Tilted Then Exit Sub
    TargetBonus = TargetBonus + 1
    IncrementHoleLights
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("30000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 30000
    End If
    If bcoinfrenzy AND CoinLight1.State = 2 Then
        Addcoin 50
        PlaySound "fx_coins"
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
        FlashForMs Light9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
        FlashForMs Light10, 1000, 50, 0
        CoinFrenzyTimer_Timer
    End If
    If bCatchemMode Then
        If CatchLight1.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
            CheckCatchHits
        End If
    Else
        AddScore 2500 * PokemonLevel
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftLoopTrigger"
End Sub

Sub UpdatePokemonLevel
    Select Case PokemonLevel
        Case 0:LightLevel1.State = 0:LightLevel2.State = 0:LightLevel3.State = 0:LightLevel4.State = 0:Flasher1.Visible = 0:Flasher2.Visible = 0:Flasher3.Visible = 0:Flasher4.Visible = 0:LightArrow1.State = 2:LightArrow6.State = 2
        Case 1:LightLevel1.State = 1:LightLevel2.State = 0:LightLevel3.State = 0:LightLevel4.State = 0:Flasher1.Visible = 1:Flasher2.Visible = 0:Flasher3.Visible = 0:Flasher4.Visible = 0:LightArrow5.State = 2
        Case 2:LightLevel1.State = 1:LightLevel2.State = 1:LightLevel3.State = 0:LightLevel4.State = 0:Flasher1.Visible = 1:Flasher2.Visible = 1:Flasher3.Visible = 0:Flasher4.Visible = 0:HoleLight5.State = 2:l53.State = 0
        Case 3:LightLevel1.State = 1:LightLevel2.State = 1:LightLevel3.State = 1:LightLevel4.State = 0:Flasher1.Visible = 1:Flasher2.Visible = 1:Flasher3.Visible = 1:Flasher4.Visible = 0
        Case 4:LightLevel1.State = 1:LightLevel2.State = 1:LightLevel3.State = 1:LightLevel4.State = 1:Flasher1.Visible = 1:Flasher2.Visible = 1:Flasher3.Visible = 1:Flasher4.Visible = 1:LightArrow1.State = 0:LightArrow6.State = 0
    End Select
End Sub

Sub ResetPokemonLevel()
    PokemonLevel = 0
    UpdatePokemonLevel
End Sub

Sub IncrementHoleLights()
    If HolePos <> 5 Then
        HolePos = HolePos + 1
        If HolePos = 5 Then
            HolePos = 1
        End If
    End If
    UpdateHoleLights
End Sub

Sub UpdateHoleLights()
    Select Case HolePos
        Case 0:HoleLight1.State = 0:HoleLight2.State = 0:HoleLight3.State = 0:HoleLight4.State = 0
        Case 1:HoleLight1.State = 2:HoleLight2.State = 0:HoleLight3.State = 0:HoleLight4.State = 0
        Case 2:HoleLight1.State = 0:HoleLight2.State = 2:HoleLight3.State = 0:HoleLight4.State = 0
        Case 3:HoleLight1.State = 0:HoleLight2.State = 0:HoleLight3.State = 2:HoleLight4.State = 0
        Case 4:HoleLight1.State = 0:HoleLight2.State = 0:HoleLight3.State = 0:HoleLight4.State = 2
    End Select
End Sub

Sub ResetHoleLights()
    HolePos = 0
    UpdateHoleLights
End Sub

'*****************
' Pikachu Targets
'*****************

Dim bPikachuTargetMode, PikachuHits, PikachuTargetValue

Sub PikaTarget1_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 115, DOFPulse, DOFTargets), PikaTarget1
    If Tilted Then Exit Sub
    PlaySoundEffect
    PikachuShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher12, 1000, 50, 0:FlashForMs Flasher12a, 1000, 50, 0
        FlashForMs Light12, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight1.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget1"
End Sub

Sub PikaTarget2_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 115, DOFPulse, DOFTargets), PikaTarget2
     If NOT bMultiBallMode Then PlaySound"quoteM"&RndNbr(10)
    If Tilted Then Exit Sub
    PlaySoundEffect
    PikachuShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher11, 1000, 50, 0:FlashForMs Flasher11a, 1000, 50, 0
        FlashForMs Light11, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight2.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget2"
End Sub

Sub PikaTarget3_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 115, DOFPulse, DOFTargets), PikaTarget3
     If NOT bMultiBallMode Then PlaySound"quoteM"&RndNbr(10)
    If Tilted Then Exit Sub
    PlaySoundEffect
    PikachuShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher11, 1000, 50, 0:FlashForMs Flasher11a, 1000, 50, 0
        FlashForMs Light11, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight3.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget3"
End Sub

Sub PikaTarget4_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 115, DOFPulse, DOFTargets), PikaTarget4
    If Tilted Then Exit Sub
    PlaySoundEffect
    PikachuShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher11, 1000, 50, 0:FlashForMs Flasher11a, 1000, 50, 0
        FlashForMs Light11, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight4.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget4"
End Sub

Sub PikaTarget5_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 116, DOFPulse, DOFTargets), PikaTarget5
    If Tilted Then Exit Sub
    PlaySoundEffect
    If LastSwitchHit = "LeftInlaneTrigger2" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher12, 1000, 50, 0:FlashForMs Flasher12a, 1000, 50, 0
        FlashForMs Light12, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    CharmanderShake
    TargetBonus = TargetBonus + 1
    PikaLight5.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget5"
End Sub

Sub PikaTarget6_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 116, DOFPulse, DOFTargets), PikaTarget6
    If NOT bMultiBallMode Then PlaySound"quoteJ"&RndNbr(7)
    If Tilted Then Exit Sub
    PlaySoundEffect
    If LastSwitchHit = "LeftInlaneTrigger2" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    CharmanderShake
    If bPikachuTargetMode Then
       Addscore PikachuTargetValue * 3
       FlashForMs Flasher12, 1000, 50, 0:FlashForMs Flasher12a, 1000, 50, 0
       FlashForMs Light12, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight6.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget6"
End Sub

Sub PikaTarget7_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 116, DOFPulse, DOFTargets), PikaTarget7
    If Tilted Then Exit Sub
    PlaySoundEffect
    If LastSwitchHit = "LeftInlaneTrigger2" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    CharmanderShake
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher12, 1000, 50, 0:FlashForMs Flasher12a, 1000, 50, 0
        FlashForMs Light12, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight7.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget7"
End Sub

Sub CheckTargetLights()
    If(PikaLight1.State + PikaLight2.State + PikaLight3.State + PikaLight4.State + PikaLight5.State + PikaLight6.State + PikaLight7.State) = 7 Then 'turn on Pikachu Multiball: 2 ball multiball
        'start the target mode
        StartTargetMode
        ' enable the pikachumultiball light
        GiEffect 2
        PikaMultiballLight.State = 2
        PikaLight1.State = 0
        PikaLight2.State = 0
        PikaLight3.State = 0
        PikaLight4.State = 0
        PikaLight5.State = 0
        PikaLight6.State = 0
        PikaLight7.State = 0
        PikachuTargetValue = PikachuTargetValue + 5000
    End If
End Sub



Sub LightSeqPikachu_PlayDone()
    LightSeqPikachu.Play SeqRandom, 7, , 3000
End Sub

Sub StartTargetMode()
     DMD "         TARGET", "          MODE", "MURD", eBlink, eBlink, eNone, 1500, True, "QuoteM6"
        'do some show with the lights
    LightSeqPikachu.Play SeqRandom, 7, , 3000
    LightEffect 2
    bPikachuTargetMode = True
    'Starter the timer to turn off the target mode
    PikachuTargetTimer.Enabled = 1
End Sub

Sub PikachuTargetTimer_Timer()
    LightSeqPikachu.StopPlay
    bPikachuTargetMode = False
    PikachuTargetTimer.Enabled = 0
End Sub

'****************************
'Pokemon Center / Coin Frenzy
'****************************

Dim BallType

Sub CenterEnableTrigger_Hit()
    If Tilted Then Exit Sub
    If BallUpdateLight.State = 2 AND BallType < 4 Then 'coins are over 250 and ball can be updated
        PokemonCenter.Enabled = True
    Else
        PokemonCenter.Enabled = False
    End If
End Sub

Sub PokemonCenter_Hit()
    PokemonCenter.Enabled = False
    IncrementBallType
    vpmtimer.addtimer 1000, "PokemonCenter.kick 270,4 '"
End Sub

Sub IncrementBallType()
    If Coins(CurrentPlayer) > 249 AND BallType < 4 Then
        PlayFanfare
        AddCoin -250
        BallType = BallType + 1
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
    FlashForMs Light9, 1000, 50, 0
    StartSmoke
    
        Select case BallType
            Case 1:DMD "", "         JESSICA JONES", "pokeball", eNone, eBlink, eNone, 1500, True, ""
            Case 2:DMD "", "         LUKE CAGE", "greatball", eNone, eBlink, eNone, 1500, True, ""
            Case 3:DMD "", "        IRON FIST", "ultraball", eNone, eBlink, eNone, 1500, True, ""
            Case 4:DMD "", "        DAREDEVIL", "masterball", eNone, eBlink, eNone, 1500, True, ""
        End Select
        UpdateBallType
    End If
End Sub

Sub ReduceBallType()
    If BallType > 1 Then
        BallType = BallType - 1
        UpdateBallType
    End If
End Sub

Sub UpdateBallType()
    Select Case BallType
        Case 1:BallLight1.State = 1:BallLight2.State = 0:BallLight3.State = 0:BallLight4.State = 0
        Case 2:BallLight1.State = 0:BallLight2.State = 1:BallLight3.State = 0:BallLight4.State = 0
        Case 3:BallLight1.State = 0:BallLight2.State = 0:BallLight3.State = 1:BallLight4.State = 0
        Case 4:BallLight1.State = 0:BallLight2.State = 0:BallLight3.State = 0:BallLight4.State = 1
    End Select
    UpdateBallImage
End Sub

Sub UpdateBallImage()
    Dim BOT, b
    BOT = GetBalls
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    ' change the image for each ball
    For b = 0 to UBound(BOT)
        Select Case BallType
            Case 1:BOT(b).FrontDecal = "ball1"
            Case 2:BOT(b).FrontDecal = "ball2"
            Case 3:BOT(b).FrontDecal = "ball3"
            Case 4:BOT(b).FrontDecal = "ball4"
        End Select
    Next
End Sub

' Coin Frenzy
Dim bcoinfrenzy, coinstep

Sub CheckCoinFrenzy()
    If bcoinfrenzy = False Then
        If coinstep > 2 Then
            StartCoinFrenzy
        End If
    End If
End Sub

Sub StartCoinFrenzy()
    StartSmoke
    bcoinfrenzy = True
    CoinFrenzyTimer.Enabled = True
    StopCoinFrenzyTimer.Enabled = True
    CoinFrenzyTimer_Timer
End Sub

Sub CoinFrenzyTimer_Timer() 'blink one catch light for 5 seconds and then change to another light simulating the pokemon movement
    Dim i
    For each i in aCoinFrenzyLights
        i.State = 0
    Next
    i = INT(RND * 5)
    aCoinFrenzyLights(i).State = 2
End Sub

Sub StopCoinFrenzyTimer_Timer() 'reset lights & variables
    Dim i
    CoinFrenzyTimer.Enabled = False
    StopCoinFrenzyTimer.Enabled = False
    For each i in aCoinFrenzyLights
        i.State = 0
    Next
    bcoinfrenzy = False
    coinstep = 0
End Sub

'***********************************
'Venusaur Cave: Lock (software lock)
'***********************************

Dim bLockEnabled, LockedBalls
Sub VenusaurHole_Hit()
    If NOT bMultiBallMode Then PlaySound"quoteB"&RndNbr(15)
    PlaySoundAt "fx_kicker_enter", VenusaurHole
    If Not Tilted Then
        If bcoinfrenzy AND CoinLight4.State = 2 Then
            Addcoin 50
            PlaySound "fx_coins"
            FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
            FlashForMs Light9, 1000, 50, 0
            FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
            FlashForMs Light10, 1000, 50, 0
            CoinFrenzyTimer_Timer
        End If
        If bCatchemMode Then
            If CatchLight3.State = 2 Then
                Addscore 10000 * PokemonLevel
                CatchHits = CatchHits + 1
                CheckCatchHits
            End If
        Else
            Addscore 10000
        End If
        If bLockEnabled Then
            LockedBalls = LockedBalls + 1
        DMD "_", CL(1, "BALL " & LockedBalls & " IS LOCKED"), "_", eNone, eNone, eNone, 500, True, ""
        StartCharge
        If LockedBalls = 2 Then
                MultiLight1.State = 2
                MultiLight2.State = 2
                MultiLight3.State = 2
                MultiLight4.State = 2
                MultiLight5.State = 2
            End If
            If LockedBalls = 3 Then
                VenusaurMultiball
                StartLaser
            End IF
        End If
    End If
    vpmtimer.addtimer 1500, "VenusaurKickBall '"
    ' remember last trigger hit by the ball
    LastSwitchHit = "VenusaurHole"
End Sub

Sub VenusaurKickBall()
    PlaySoundAt SoundFXDOF("fx_kicker", 129, DOFPulse, DOFContactors), VenusaurHole
    DOF 121, DOFPulse
    AnimateVenusaur
    VenusaurHole.Kick 200, 8
End Sub

Sub VenusaurLock1_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 128, DOFPulse, DOFTargets), VenusaurLock1
    If Tilted Then Exit Sub
    PlaySoundEffect
    Addscore 20000
    VenusaurShake
    LockLight1.State = 1
    CheckLock
End Sub

Sub VenusaurLock2_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 128, DOFPulse, DOFTargets), VenusaurLock2
    If Tilted Then Exit Sub
    PlaySoundEffect
    Addscore 20000
    VenusaurShake
    LockLight2.State = 1
    CheckLock
End Sub

Sub CheckLock()
    If bLockEnabled Then Exit Sub
    If LockLight1.State + LockLight2.State = 2 Then
        bLockEnabled = True
        LockLight.State = 2
        DMD "", CL(1, "LOCK IS LIT"), "", eNone, eBlink, eNone, 1000, True, ""
    End If
End Sub

Sub VenusaurMultiball()
    DMD "", CL(1, "KINGPIN MULTIBALL"), "", eNone, eBlink, eNone, 1000, True, ""   
    PlayFanfare
    AddMultiball 3
    ' turn off the lock lights
    LockedBalls = 0
    bLockEnabled = False
    MultiLight1.State = 0
    MultiLight2.State = 0
    MultiLight3.State = 0
    MultiLight4.State = 0
    MultiLight5.State = 0
    LockLight1.State = 0
    LockLight2.State = 0
    LockLight.State = 0
    'Turn On the Super Jackpot lights
    CenterRampLight.State = 2
    RightRampLight.State = 2
    SetLightColor LightArrow4, red, 2
    SetLightColor LightArrow2, red, 2
End Sub

Sub ResetJackpotLights()
    CenterRampLight.State = 0
    RightRampLight.State = 0
    SetLightColor LightArrow4, white, 0
    SetLightColor LightArrow2, white, 0
    LightArrow4.State = 0
    LightArrow2.State = 0
End Sub

Dim VenusaurPos

Sub AnimateVenusaur()
    VenusaurPos = 0
    VenusaurTimer.Enabled = 1
End Sub

Sub VenusaurTimer_Timer()
    Select Case VenusaurPos
        Case 0:Venusaur.TransY = 40:VenusaurPos = 1
        Case 1:Venusaur.TransY = 20:VenusaurPos = 2
        Case 2:Venusaur.TransY = 10:VenusaurPos = 3
        Case 3:Venusaur.TransY = 5:VenusaurPos = 4
        Case 4:Venusaur.TransY = 0:VenusaurTimer.Enabled = 0
    End Select
End Sub

'***************
' Ramp Switches
'***************

Sub RightRampDone_Hit
    If Tilted Then Exit Sub
    If LastSwitchHit = "LeftInlaneTrigger1" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("20000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 20000
    End If
    If bcoinfrenzy AND CoinLight5.State = 2 Then
        Addcoin 50
        PlaySound "fx_coins"
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
        FlashForMs Light9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
        FlashForMs Light10, 1000, 50, 0
        CoinFrenzyTimer_Timer
    End If
    If bRampBonus Then
        AwardJackpot
    End If
    If bMultiBallMode Then
        If CenterRampLight.State = 2 Then
            AwardSuperJackpot
        Else
            AwardJackpot
        End If
    End If
   If PikaMultiballLight.State = 2 Then
        DMD "        MURDOCK", "         MULTIBALL", "MURD", eNone, eBlink, eNone, 500, True, "QuoteM1"
        StartSonar
        AddMultiball 1
        PikaMultiballLight.State = 0
    End If
    If bCatchemMode Then
        If CatchLight4.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
            CheckCatchHits
        End If
    Else
        Addscore 10000
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightRampDone"
End Sub

Sub CenterRampDone_Hit
    If NOT bMultiBallMode Then PlaySound"quoteA"&RndNbr(9)
    If Tilted Then Exit Sub
    If ExtraBallLight.State = 2 Then
        AwardExtraBall
        ExtraBallLight.State = 0
    End If
    If bcoinfrenzy AND CoinLight3.State = 2 Then
        Addcoin 50
        PlaySound "fx_coins"
        CoinFrenzyTimer_Timer
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
        FlashForMs Light9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
        FlashForMs Light10, 1000, 50, 0
    Else
        coinstep = coinstep + 1
        CheckCoinFrenzy
    End If
    If bRampBonus Then
        AwardJackpot
    End If
    If bMultiBallMode Then
        If CenterRampLight.State = 2 Then
            AwardSuperJackpot
        Else
            AwardJackpot
        End If
    End If
    If bCatchemMode Then
        If CatchLight6.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
            CheckCatchHits
        End If
    Else
        Addscore 10000
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "CenterRampDone"
End Sub

'*****************
'Ramp Bonus Mode
'*****************
' only availabe trough the slot machine

Dim bRampBonus

Sub StartRampBonus()
    bRampBonus = True
    SetLightColor LightArrow4, red, 2
    SetLightColor LightArrow2, red, 2
    StopRampBonusTimer.Enabled = True
End Sub

Sub StopRampBonusTimer_Timer()
    bRampBonus = FALSE
    SetLightColor LightArrow4, white, 0
    SetLightColor LightArrow2, white, 0
    StopRampBonusTimer.Enabled = False
End Sub

'*****************
' Loop Bonus Mode
'*****************
' only availabe trough the slot machine

Dim bLoopBonus

Sub StartLoopBonus()
    bLoopBonus = True
    SetLightColor LightArrow1, red, 2
    SetLightColor LightArrow6, red, 2
    StopLoopBonusTimer.Enabled = True
End Sub

Sub StopLoopBonusTimer_Timer()
    bLoopBonus = FALSE
    SetLightColor LightArrow1, white, 0
    SetLightColor LightArrow6, white, 0
    StopLoopBonusTimer.Enabled = False
End Sub

Sub LoopTrigger_Hit()
    If bLoopBonus Then
        AwardJackpot
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
        FlashForMs Light9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
        FlashForMs Light10, 1000, 50, 0
        GiEffect 1
    End If
    LastSwitchHit = "LoopTrigger"
End Sub

'*******************
'Pokemon definitions
'*******************

Dim Eggs, Eggname, Pokemon, Pokename

Eggs = Array("pok152", "pok153", "pok154", "pok155", "pok156", "pok157", "pok158", "pok159", "pok160", "pok161", "pok162", "pok163", "pok164", "pok165", _
    "pok166", "pok167", "pok168", "pok169", "pok170", "pok171", "pok172", "pok173", "pok174", "pok175", "pok176", "pok177", "pok178") '26 pokemon level 1

Eggname = Array("15TH PREDINCT", "CLINTON MISSION", "LAW OFFICE", "FOGWELLS GYM", "JAVITS CENTER", "JOSIES BAR", "PARADISO CLUB", "FISK TOWER", "CRIME CENTRAL", "BAXTER BLDG", _
 "HUDSON DOCKS", "ROYAL DRAGON", "S. SANCTORUM", "NY BULLETIN", "CENTRAL PARK", "ALL SAINTS", "RAIL YARD", "TRAVEL INN", "NELSON MEATS", "OSCORP", "CITY HALL", "PIER 81", _
 "RYKERS ISLAND", "SQUARE DINER", "VAN LUNT BLDG", "WHITESTONE", "CEMETARY")

Pokemon = Array("pok001", "pok002", "pok003", "pok004", "pok005", "pok006", "pok007", "pok008", "pok009", "pok010", _
    "pok011", "pok012", "pok013", "pok014", "pok015", "pok016", "pok017", "pok018", "pok019", "pok020", _
    "pok021", "pok022", "pok023", "pok024", "pok025", "pok026", "pok027", "pok028", "pok029", "pok030", _
    "pok031", "pok032", "pok033", "pok034", "pok035", "pok036", "pok037", "pok038", "pok039", "pok040", _
    "pok041", "pok042", "pok043", "pok044", "pok045", "pok046", "pok047", "pok048", "pok049", "pok050", _
    "pok051", "pok052", "pok053", "pok054", "pok055", "pok056", "pok057", "pok058", "pok055", "pok060", _
    "pok061", "pok062", "pok063", "pok064", "pok065", "pok066", "pok067", "pok068", "pok069", "pok070", _
    "pok071", "pok072", "pok073", "pok074", "pok075", "pok076", "pok077", "pok078", "pok079", "pok080", _
    "pok081", "pok082", "pok083", "pok084", "pok085", "pok086", "pok087", "pok088", "pok089", "pok090", _
    "pok091", "pok092", "pok093", "pok094", "pok095", "pok096", "pok097", "pok098", "pok099", "pok100", _
    "pok101", "pok002", "pok003", "pok004", "pok005", "pok006", "pok007", "pok008", "pok009", "pok010", _
    "pok111", "pok112", "pok113", "pok114", "pok115", "pok116", "pok117", "pok118", "pok119", "pok120", _
    "pok121", "pok122", "pok123", "pok124", "pok125", "pok126", "pok127", "pok128", "pok129", "pok130", _
    "pok131", "pok132", "pok133", "pok134", "pok135", "pok136", "pok137", "pok138", "pok139", "pok140", _
    "pok141", "pok142", "pok143", "pok144", "pok145", "pok146", "pok147", "pok148", "pok149", "pok151", "pok150", "pok150") ' all the 151 Pokémon, Mewtwo, can only be caught from the slotmachine                                                                                     '151

Pokename = Array("ECHO", "JESTER", "KINGPIN", "THE OWL", "TYPHOID MARY", "ELEKTRA", "BULLSEYE", "PURPLE MAN", "MR. FEAR", "GLADIATOR", "TOMBSTONE", _
    "DEATH STALKER", "STILT MAN", "MAN BULL", "ALEX BONT", "AMMO", "BENGAL", "BLINDSIDE", "NUKE", "EEL", "LADY BULLSEYE", "BLACK TARANTULA", _
    "BRUISER", "BUCK CASHMAN", "EVIL DAREDEVIL", "MYSTERIO", "MOLE MAN", "CALAVERA", "THE ROSE", "THE SPOT", "COYOTE", "IKARI", "JACKHAMMER", _
    "JUBULA PRIDE", "KRUEL", "MADCAP", "MASTER IZO", "MAULER", "MIND MASTER", "MACHINESMITH", "SNAKEROOT", "RAMROD", "R. CHERRYH", "RAZOR FIST", _
    "SCREWBALL", "SIN EATER", "SLUG", "TRIBUNE", "MR. NEGATIVE", "ENFORCERS", "KIRIGI", "ABE JENKINS", "ABSORBING MAN", "AGENT CROCK", "AKA", _
    "AKHENATEN", "RED AKUMA", "SILVER SAMURAI", "THE HAND", "CROSSBONES", "APE MAN", "BIRD MAN", "FROG MAN", "CAT MAN", "ARCADE", "WRECKER", _
    "THUNDERBALL", "PILEDRIVER", "BULLDOZER", "KLAW", "MEPHISTO", "ELECTRO", "SHOCKER", "BLACK KNIGHT", "ABOMINATION", "LOKI", _
    "DORMAMMU", "DARK BEAST", "CHEMISTRO", "CHESHIRE", "CHONDU", "COCKROACH", "COMANCHE", "COPPERHEAD", "CRANE MOTHER", "FEROCIA", "KHAN", _
    "SCIMITAR", "SHUO LAO", "STEEL SERPENT", "YU-TI", "DOC OCK", "GORGON", "HAYWIRE", "HOBGOBLIN", "JIGSAW", "KNULL", "LONE SHARK", "MR. BEYOND", _
    "GOLD GOBLIN", "RHINO", "SCARLETT WITCH", "SKADI", "STAR", "TOXIC DOXIE", "TRICKSHOT", "VERANKE", "VIKKI HAND", "SCORPION", _
    "SKRULLS", "GREEN GOBLIN", "J. BEEKMAN", "BLACK CAT", "DEADPOOL", "BLACK MAMBA", "EGGHEAD", "GOLIATH", "VULTURE", "CARNAGE", "VENOM", "WHIRLWIND", _
    "ZZZAX", "DR. DOOM", "SABERTOOTH", "YANDROTH", "ATTUMA", "BATROC", "MANDRILL", "NAMOR", "MELTER", "NEBULON", "AMORA", "ORKA", _
    "RED GHOST", "SAND MAN", "BLACKHEART", "KRAVEN", "MADAME GAO", "NOBU", "PALADIN", "TASKMASTER", "COLLECTOR", "WHITE TIGER", "THANOS", _
    "TIGER SHARK", "TOAD", "PORCUPINE", "BLACKWING", "SHE-HULK", "LIZARD", "PUNISHER", "PUNISHER")

'***************************
' Catch Hole /Catch'em Mode
'***************************

' Start always first the Cath'em mode if the light is blinking other wise gives a random award if one of the other lights are blinking.
' Catch'em mode ends after 2 minutes

Dim bCatchemMode, CatchID, CatchMaxHits, CatchHits, BallInHole

Dim aBall

Sub CatchHole_Timer
    Do While aBall.Z > 0
        aBall.Z = aBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
End Sub

Sub CatchHole_Hit
    PlaySoundAt "fx_hole_enter", CatchHole
    If NOT bMultiBallMode Then PlaySound"quoteE"&RndNbr(7)
    BallInHole = BallInHole + 1
    Set aBall = ActiveBall:Me.TimerEnabled = 1
    If HoleLight5.State = 2 Then 'Start catchem mode
        StartCatchem1
        vpmtimer.addtimer 2500, "CatchHoleExit '"
        Exit Sub
    End If
    If HolePos > 0 Then
        StartSlotmachine
    Else
        vpmtimer.addtimer 1000, "CatchHoleExit '"
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "CatchHole"
End Sub

Sub CatchHoleExit()
    If BallInHole > 0 Then
        FlashForMs FlasherExitHole, 1000, 30, 0
        BallInHole = BallInHole - 1
        CatchHole.CreateSizedball BallSize / 2
        UpdateBallImage
        PlaySoundAt SoundFXDOF("fx_popper", 120, DOFPulse, DOFContactors), CatchHole
        DOF 121, DOFPulse
        CatchHole.Kick 175, 14, 1
    End If
End Sub

Sub StartCatchem1()
    bCatchemMode = True
    CatchID = 0
    ChangeSong                              ' change the song
    Select Case PokemonLevel                'limit the numbers of pokemons based on the level
        Case 2:CatchID = INT(RND * 50)      ' CatchID the number of the pokemon to be caught, this are level 2 Pokémon
        Case 3:CatchID = INT(RND * 50) + 49 'level 3 Pokémon
        Case 4:CatchID = INT(RND * 50) + 99 'level 4 Pokémon
    End Select
    'debug.print catchid
    CatchMaxHits = PokemonLevel
    StartCatchem2
End Sub

Sub StartCatchem2()
    DMD RL(0, "VILLAIN TAKEDOWN"), RL(1, pokename(CatchID)), pokemon(CatchID), eNone, eNone, eNone, 2000, True, ""
    CatchHits = 0
    CatchemTimer.Enabled = True ' blink one light
    StopCatchem.Enabled = True  ' turn on the timer to stop the mode
    CatchemTimer_Timer
    ' turn off lights
    HoleLight5.State = 0:l53.State = 2
    EnableBallSaver 10
End Sub

Sub CatchemTimer_Timer() 'blink one catch light for 5 seconds and then change to another light simulating the pokemon movement
    Dim i
    For each i in aCatchLights
        i.State = 0
    Next
    i = INT(RND * 6)
    aCatchLights(i).State = 2
End Sub

Sub StopCatchem_Timer() 'reset lights & variables
    Dim i
    CatchemTimer.Enabled = False
    StopCatchem.Enabled = False
    For each i in aCatchLights
        i.State = 0
    Next
    bCatchemMode = False
    ChangeSong
    ResetPokemonLevel
End Sub

Sub CheckCatchHits()
    GiEffect 1
    PlaySoundEffect
    If CatchHits >= CatchMaxHits Then 'stop catch'em mode
        PlayFanfare
        DMD RL(0, "YOU CAUGHT "), RL(1, Pokename(CatchID)), Pokemon(CatchID), eNone, eBlinkFast, eNone, 1500, False, ""
        DMD RL(0, FormatScore(50000 * pokemonlevel)), RL(1, Pokename(CatchID)), Pokemon(CatchID), eBlinkFast, eNone, eNone, 1500, True, ""
        Addscore 50000 * pokemonlevel
        PokemonBonusAward(CurrentPlayer) = PokemonBonusAward(CurrentPlayer) + (pokemonlevel * pokemonlevel)
        PokemonBonus(CurrentPlayer) = PokemonBonus(CurrentPlayer) + 1
        PokemonCaught(CurrentPlayer, PokemonBonus(CurrentPlayer)) = CatchID
        LightEffect 1
        GiEffect 1
        If PokemonBonus(CurrentPlayer)MOD 3 = 0 Then 'turn on the extra ball light after catching 3 pokemons
            ExtraBallLight.State = 2
        End If
        StopCatchem_Timer
    Else 'change to another position
        CatchemTimer.Enabled = 0
        CatchemTimer.Enabled = 1
        CatchemTimer_Timer
    End If
End Sub

Sub CatchMewtwo()
    bCatchemMode = True
    CatchID = 150 'the number of Mewtwo in the reel
    PokemonLevel = 5
    CatchMaxHits = 6
    StartCatchem2
End Sub

'**************
' SlotMachine
'**************

Dim PokAward
PokAward = Array("","sa4", "sa1", "sa2", "sa3")

Sub StartSlotmachine() ' uses the HolePos variable
    HoleBonus = HoleBonus + 1
    DMDFlush
    DMD CL(0, "SLOT AWARDS"), CL(1, "SMALL POINTS"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "BIG POINTS"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "30S BALL SAVER"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "SMALL POINTS"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "BIG POINTS"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "UPGRADE HERO"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "SMALL POINTS"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "MYSTERIO MYSTERY"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "MULTIBALL"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "CATCH PUNISHER"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DOF 142, DOFPulse
    vpmtimer.AddTimer 2500, "GiveSlotAward '"
End Sub

Sub GiveSlotAward()
    Dim tmp
    DMDFlush
    tmp = RndNbr(9)
    Select Case tmp
        Case 0:AddScore INT(10000 * RND * 9)  'small points
            DMD CL(0, "YOUR AWARD"), CL(1, "SMALL POINTS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 1:AddScore INT(100000 * RND * 9) 'big points
            DMD CL(0, "YOUR AWARD"), CL(1, "BIG POINTS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 2:EnableBallSaver 30             'ball saver 30 seconds
            DMD CL(0, "YOUR AWARD"), CL(1, "30SEC BALL SAVER"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 3:AddScore INT(10000 * RND * 9)  'small points
            DMD CL(0, "YOUR AWARD"), CL(1, "SMALL POINTS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 4:AddScore INT(10000 * RND * 9)  'small points
            DMD CL(0, "YOUR AWARD"), CL(1, "SMALL POINTS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 5:AddCoin 250:IncrementBallType  'upgrade ball
            DMD CL(0, "YOUR AWARD"), CL(1, "UPGRADE HERO"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 6:AddScore INT(100000 * RND * 9) 'big points
            DMD CL(0, "YOUR AWARD"), CL(1, "BIG POINTS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 7:StartCoinFrenzy                'coin frenzy
            DMD CL(0, "YOUR AWARD"), CL(1, "MYSTERIO MYSTERY"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 8
            Select Case HolePos
                Case 1:AddScore INT(100000 * RND * 9) 'big points
                    DMD CL(0, "YOUR AWARD"), CL(1, "BIG POINTS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
                Case 2:PikaMultiballLight.State = 2   'enable picachu multiball
                    DMD CL(0, "YOUR AWARD"), CL(1, "MULTIBALL"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
                Case 3:StartRampBonus                 'Ramp Bonus Mode - Activate Jackpots on the ramps for 2 minutes
                    DMD CL(0, "YOUR AWARD"), CL(1, "RAMP BONUS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
                Case 4:StartLoopBonus                 'Loop Bonus Mode
                    DMD CL(0, "YOUR AWARD"), CL(1, "LOOP BONUS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
            End Select
        Case 9
            Select Case HolePos
                Case 1:StartTargetMode          'Target Bonus Mode
                    DMD CL(0, "YOUR AWARD"), CL(1, "TARGET BONUS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
                Case 2:CatchMewtwo              'Catch Mewtwo
                    DMD CL(0, "YOUR AWARD"), CL(1, "CATCH PUNISHER"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
                Case 3:ExtraBallLight.State = 2 'Lit Extra Ball
                    DMD CL(0, "YOUR AWARD"), CL(1, "EXTRA BALL IS LIT"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
                Case 4:StartCharizardMode       'Charizard Bonus Mode
                    DMD CL(0, "YOUR AWARD"), CL(1, "BULLSEYE MODE"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
            End Select
    End Select
    GiEffect 1
    vpmtimer.addtimer 1500, "CatchHoleExit '"
End Sub

'**********************
' Charizard Bonus Mode
'**********************

Dim bCharizardMode

Sub StartCharizardMode()
    bCharizardMode = True
    EggTarget5.IsDropped = FALSE
    DOF 119, DOFPulse
    SetLightColor LightArrow5, orange, 2
    StopCharizardTimer.Enabled = True
End Sub

Sub StopCharizardTimer_Timer()
    StopCharizardTimer.Enabled = False
    bCharizardMode = False
    SetLightColor LightArrow5, white, 0
End Sub

'******************
' Egg/Hatch Mode
'******************

Dim bEggTargetsCompleted, EggID

Sub EggTarget1_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 117, DOFPulse, DOFTargets), EggTarget1
    If Tilted Then Exit Sub
    PlaySoundEffect
    HatchLight1.State = 1:HatchLight1b.Visible = 1
    Addscore 20000
    CheckEggTargets
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget1"
End Sub

Sub EggTarget2_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 117, DOFPulse, DOFTargets), EggTarget2
    If Tilted Then Exit Sub
    PlaySoundEffect
    HatchLight2.State = 1:HatchLight2b.Visible = 1
    Addscore 20000
    CheckEggTargets
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget2"
End Sub

Sub EggTarget3_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 117, DOFPulse, DOFTargets), EggTarget3
    If Tilted Then Exit Sub
    PlaySoundEffect
    HatchLight3.State = 1:HatchLight3b.Visible = 1
    Addscore 20000
    CheckEggTargets
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget3"
End Sub

Sub EggTarget4_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 117, DOFPulse, DOFTargets), EggTarget4
    If Tilted Then Exit Sub
    PlaySoundEffect
    HatchLight4.State = 1:HatchLight4b.Visible = 1
    Addscore 20000
    CheckEggTargets
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget4"
End Sub

Sub EggTarget5_Hit()
    PlaySoundAt SoundFXDOF("fx_target", 117, DOFPulse, DOFTargets), EggTarget5
    If Tilted Then Exit Sub
    PlaySoundEffect
    CharizardShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("30000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 30000
        LightEffect 1
        GiEffect 1
    End If
    If bCharizardMode Then
        GiEffect 1
        AddScore 500000
    Else
        Addscore 20000
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget5"
End Sub

Sub EggTarget5_Dropped()
    If bCharizardMode Then
        EggTarget5.Isdropped = False
        DOF 119, DOFPulse
    End If
End Sub

Sub CheckEggTargets()
    If bEggTargetsCompleted = False Then
        If(HatchLight1.State + HatchLight2.State + HatchLight3.State + HatchLight4.State) = 4 Then
            bEggTargetsCompleted = True
            HatchReadyLight. State = 2
            EggMagnet.MagnetOn = True
        End If
    End If
End Sub

Sub EggHole_Hit()
    PlaySoundAt "fx_hole_enter", EggHole
    If NOT bMultiBallMode Then PlaySound"quoteD"&RndNbr(8)
    BallInHole = BallInHole + 1
    EggHole.DestroyBall
    If bEggTargetsCompleted Then
        EggID = INT(RND * 26)
        DMD RL(0, FormatScore(1000000)), RL(1, "FOUND " &Eggname(EggID)), Eggs(EggID), eBlinkFast, eBlinkFast, eNone, 1500, True, ""
        AddScore 1000000
        EggBonus(CurrentPlayer) = EggBonus(CurrentPlayer) + 1
        If EggBonus(CurrentPlayer)MOD 3 = 0 Then 'start the Charizard Bonus Mode after hatching 3 eggs
            StartCharizardMode
        End If
        HatchReadyLight. State = 0
        FlashForMs HatchLight1b, 1000, 50, 0
        FlashForMs HatchLight2b, 1000, 50, 0
        FlashForMs HatchLight3b, 1000, 50, 0
        FlashForMs HatchLight4b, 1000, 50, 0
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
        FlashForMs Light9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
        FlashForMs Light10, 1000, 50, 0
        Gieffect 1
        vpmtimer.Addtimer 3000, "EndEggMode '"
    Else
        vpmtimer.addtimer 1000, "CatchHoleExit '"
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggHole"
End Sub

Sub EndEggMode()
    HatchLight1.State = 0:HatchLight1b.Visible = 0
    HatchLight2.State = 0:HatchLight2b.Visible = 0
    HatchLight3.State = 0:HatchLight3b.Visible = 0
    HatchLight4.State = 0:HatchLight4b.Visible = 0
    bEggTargetsCompleted = False
    EggMagnet.MagnetOn = False
    EggTarget5.IsDropped = 0
    vpmtimer.addtimer 500, "CatchHoleExit '"
End Sub

'*****************
' objects shaking
'*****************

Dim PikaShake, CharmanShake, ChariShake, VenuShake

Sub PikachuShake()
    PikaShake = 6
    PikaShakeTimer.Enabled = True
End Sub

Sub PikaShakeTimer_Timer()
    Pikachu.Transz = PikaShake / 2
    If PikaShake = 0 Then Me.Enabled = False:Exit Sub
    If PikaShake < 0 Then
        PikaShake = ABS(PikaShake)- 0.1
    Else
        PikaShake = - PikaShake + 0.1
    End If
End Sub

Sub CharmanderShake()
    CharmanShake = 6
    CharmanShakeTimer.Enabled = True
End Sub

Sub CharmanShakeTimer_Timer()
    Charmander.Transz = CharmanShake / 2
    If CharmanShake = 0 Then Me.Enabled = False:Exit Sub
    If CharmanShake < 0 Then
        CharmanShake = ABS(CharmanShake)- 0.1
    Else
        CharmanShake = - CharmanShake + 0.1
    End If
End Sub

Sub CharizardShake()
    DOF 118, DOFPulse
    ChariShake = 6
    CharizardShakeTimer.Enabled = True
End Sub

Sub CharizardShakeTimer_Timer()
    Charizard.Transy = ChariShake / 2
    If ChariShake = 0 Then Me.Enabled = False:Exit Sub
    If ChariShake < 0 Then
        ChariShake = ABS(ChariShake)- 0.1
    Else
        ChariShake = - ChariShake + 0.1
    End If
End Sub

Sub VenusaurShake()
    DOF 118, DOFPulse
    VenuShake = 6
    VenusaurShakeTimer.Enabled = True
End Sub

Sub VenusaurShakeTimer_Timer()
    Venusaur.Transy = VenuShake / 2
    If VenuShake = 0 Then Me.Enabled = False:Exit Sub
    If VenuShake < 0 Then
        VenuShake = ABS(VenuShake)- 0.1
    Else
        VenuShake = - VenuShake + 0.1
    End If
End Sub


'**********************
'Effects only triggers
'**********************

Sub EffectTrigger1_Hit()
    FlashForMs Flasher11, 1000, 50, 0:FlashForMs Flasher11a, 1000, 50, 0
    FlashForMs Light11, 1000, 50, 0
End Sub

Sub EffectTrigger2_Hit()
    FlashForMs Flasher12, 1000, 50, 0:FlashForMs Flasher12a, 1000, 50, 0
    FlashForMs Light12, 1000, 50, 0
End Sub

'*******Mysterio Smoke and Animation************

Dim Fire1Pos, Fire2Pos, Fire3Pos, Flames
Flames = Array("fire01", "fire02", "fire03", "fire04", "fire05", "fire06", "fire07", "fire08", "fire09", _
    "fire10", "fire11", "fire12", "fire13", "fire14", "fire15", "fire16")

Sub StartFire
    Fire1Pos = 0
    Fire2Pos = 2
    Fire3Pos = 5
    
    FireTimer.Enabled = 1
End Sub

Sub FireTimer_Timer
    'debug.print fire1pos
    Fire1.ImageA = Flames(Fire1Pos)
    Fire2.ImageA = Flames(Fire2Pos)
    Fire3.ImageA = Flames(Fire3Pos)
   
    Fire1Pos = (Fire1Pos + 1) MOD 16
    Fire2Pos = (Fire2Pos + 1) MOD 16
    Fire3Pos = (Fire3Pos + 1) MOD 16
   
End Sub

Sub SphereTimer_Timer
   spherelord.rotY = spherelord.rotY + 2
End sub

Sub BoatTimer_Timer()
    BoatDir = SIN(BoatStep * MyPi)
    BoatStep = (BoatStep + 1)MOD 360
    Mysterio.Y = Mysterio.Y + BoatDir * 0.3
    Mysterio2.Y = Mysterio2.Y + BoatDir * 0.3
    spherelord.Y = spherelord.Y + BoatDir * 0.3
End Sub

'*******Moving Characters********

Dim MyPi, TreeStep, TreeDir, BoatStep, BoatDir
MyPi = Round(4 * Atn(1), 6) / 90
TreeStep = 0
BoatStep = 0

Sub Trees_Timer()
    TreeDir = SIN(TreeStep * MyPi)
    TreeStep = (TreeStep + 1)MOD 360
    
    Mysterio2.RotY = - TreeDir *10
    Mysterio.RotY = - TreeDir *10
    Punisher1.RotY = - TreeDir *2
    Punisher2.RotY = - TreeDir *2
    Punisher3.RotY = - TreeDir *2
    Punisher4.RotY = - TreeDir *2
    Elektra.RotZ = - TreeDir *1
    Pikachu.RotZ = - TreeDir *1
    Bulbasaur.RotX = - TreeDir *1
    Squirtle.RotY = - TreeDir *1
    
End Sub

'******Bumper Saw Blades**********

ringspin.enabled = 1

Sub ringspin_timer
	saw1.RotY = saw1.RotY + 1
    saw2.RotY = saw2.RotY + 1
    saw3.RotY = saw3.RotY - 1
    saw4.RotY = saw4.RotY - 1
	
End Sub

'******Gun********

Dim Gun1Pos, GunFrames
GunFrames = Array("MF-000", "MF-001", "MF-002", "MF-003", "MF-004", "MF-005", "MF-006", "MF-007", "MF-008", "MF-009", "MF-010", "MF-011", "MF-012", "MF-013", "MF-014", "MF-015", "MF-016", "MF-017", "MF-018", "MF-019", "MF-020", _
"MF-021", "MF-022", "MF-023", "MF-024", "MF-025", "MF-026", "MF-027", "MF-028", "MF-029")
 
 
Sub StartGun
GunFlash.visible = 1
Gun1Pos = 0
GunTimer.Enabled = 1
StopGunTimer.Enabled = 1
PlaySound "Gunsound"
End Sub
 
Sub GunTimer_timer
GunFlash.ImageA = GunFrames(Gun1Pos)
Gun1Pos = (Gun1Pos + 1) MOD 30
vpmtimer.addtimer 1500, "StopGun '"
End Sub
 

Sub StopGun
GunFlash.visible = 0
GunTimer.Enabled = 0
End Sub

Sub StopGunTimer_Timer
StopGun
Me.Enabled = 0
End Sub

'*****Laser*******

Dim	Laser1Pos, LaserFrames 
LaserFrames = Array("ML-000", "ML-001", "ML-002", "ML-003", "ML-004", "ML-005", "ML-006", "ML-007", "ML-008", "ML-009", "ML-010", "ML-011", "ML-012", "ML-013", "ML-014", "ML-015", "ML-016", "ML-017", "ML-018", "ML-019", "ML-020", _
"ML-021", "ML-022", "ML-023", "ML-024", "ML-025", "ML-026", "ML-027", "ML-028", "ML-029")


Sub StartLaser
LaserFlash.visible = 1
Laser1Pos = 0
LaserTimer.Enabled = 1
StopLaserTimer.Enabled = 1
PlaySound "Laser"
End Sub
 
Sub LaserTimer_timer
LaserFlash.ImageA = LaserFrames(Laser1Pos)
Laser1Pos = (Laser1Pos + 1) MOD 30
vpmtimer.addtimer 1200, "StopLaser '"
End Sub
 
Sub StopLaser
LaserFlash.visible = 0
LaserTimer.Enabled = 0
End Sub

Sub StopLaserTimer_Timer
StopLaser
Me.Enabled = 0
End Sub


'*******HellsKitchen*******
Dim mystate
mystate = 0
flasher0001.TimerEnabled = 1
 
Sub flasher0001_Timer
    flasher0001.visible = mystate
    mystate = ABS(mystate - 1)
End Sub

'*********Sonar*********

Dim	Sonar1Pos, SonarFrames 
SonarFrames = Array("MS-000", "MS-001", "MS-002", "MS-003", "MS-004", "MS-005", "MS-006", "MS-007", "MS-008", "MS-009", "MS-010", "MS-011", "MS-012", "MS-013", "MS-014", "MS-015", "MS-016", "MS-017", "MS-018", "MS-019", "MS-020")


Sub StartSonar
SonarFlash.visible = 1
Sonar1Pos = 0
SonarTimer.Enabled = 1
StopSonarTimer.Enabled = 1
PlaySound "Sonar"
End Sub
 
Sub SonarTimer_timer
SonarFlash.ImageA = SonarFrames(Sonar1Pos)
Sonar1Pos = (Sonar1Pos + 1) MOD 30
vpmtimer.addtimer 1500, "StopSonar '"
End Sub
 
Sub StopSonar
SonarFlash.visible = 0
SonarTimer.Enabled = 0
End Sub

Sub StopSonarTimer_Timer
StopSonar
Me.Enabled = 0
End Sub

'*********Smoke***********


Dim	Smoke1Pos, SmokeFrames 
SmokeFrames = Array("MG-000", "MG-001", "MG-002", "MG-003", "MG-004", "MG-005", "MG-006", "MG-007", "MG-008", "MG-009", "MG-010", "MG-011", "MG-012", "MG-013", "MG-014", "MG-015", "MG-016", "MG-017", "MG-018", "MG-019", "MG-020", _
"MG-021", "MG-022", "MG-023", "MG-024", "MG-025", "MG-026", "MG-027", "MG-028", "MG-029", "MG-030", "MG-031", "MG-032", "MG-033", "MG-034", "MG-035", "MG-036", "MG-037", "MG-038""MG-039", "MG-040", "MG-041", "MG-042", "MG-043")


Sub StartSmoke
SmokeFlash.visible = 1
Smoke1Pos = 0
SmokeTimer.Enabled = 1
StopSmokeTimer.Enabled = 1
PlaySound "Smoke"
End Sub
 
Sub SmokeTimer_timer
SmokeFlash.ImageA = SmokeFrames(Smoke1Pos)
Smoke1Pos = (Smoke1Pos + 1) MOD 43
vpmtimer.addtimer 1500, "StopSmoke '"
End Sub
 
Sub StopSmoke
SmokeFlash.visible = 0
SmokeTimer.Enabled = 0
End Sub

Sub StopSmokeTimer_Timer
StopSmoke
Me.Enabled = 0
End Sub

'********LASER CHARGE********

Dim	Charge1Pos, ChargeFrames 
ChargeFrames = Array("CH-000", "CH-001", "CH-002", "CH-003", "CH-004", "CH-005", "CH-006", "CH-007", "CH-008", "CH-009", "CH-010", "CH-011", "CH-012", "CH-013", "CH-014", "CH-015", "CH-016", "CH-017", "CH-018", "CH-019", "CH-020", _
"CH-021", "CH-022", "CH-023", "CH-024", "CH-025", "CH-026", "CH-027", "CH-028")


Sub StartCharge
ChargeFlash.visible = 1
Charge1Pos = 0
ChargeTimer.Enabled = 1
StopChargeTimer.Enabled = 1
PlaySound "Charge"
End Sub
 
Sub ChargeTimer_timer
ChargeFlash.ImageA = ChargeFrames(Charge1Pos)
Charge1Pos = (Charge1Pos + 1) MOD 28
vpmtimer.addtimer 1500, "StopCharge '"
End Sub
 
Sub StopCharge
ChargeFlash.visible = 0
ChargeTimer.Enabled = 0
End Sub

Sub StopChargeTimer_Timer
StopCharge
Me.Enabled = 0
End Sub

'******Iron Fists******


Dim	Iron1Pos, IronFrames 
IronFrames = Array("IF-000", "IF-001", "IF-002", "IF-003", "IF-004", "IF-005", "IF-006", "IF-007", "IF-008", "IF-009", "IF-010", "IF-011", "IF-012", "IF-013", "IF-014", "IF-015", "IF-016", "IF-017", "IF-018", "IF-019", "IF-020", _
"IF-021", "IF-022", "IF-023", "IF-024", "IF-025", "IF-026", "IF-027", "IF-028")


Sub StartIron
IronFlash.visible = 1
Iron1Pos = 0
IronTimer.Enabled = 1
StopIronTimer.Enabled = 1
PlaySound "IRON"
End Sub
 
Sub IronTimer_timer
IronFlash.ImageA = IronFrames(Iron1Pos)
Iron1Pos = (Iron1Pos + 1) MOD 28
vpmtimer.addtimer 1500, "StopIron '"
End Sub
 
Sub StopIron
IronFlash.visible = 0
IronTimer.Enabled = 0
End Sub

Sub StopIronTimer_Timer
StopIron
Me.Enabled = 0
End Sub


'*******PORTAL******

Dim RotAngle4
RotAngle4 = 0

Sub PORTALLogo_Timer
    RotAngle4 = (RotAngle4+ 5)MOD 360
    FlasherlogoVP.Rotz = RotAngle4
    FlasherlogoVP2.Rotz = RotAngle4
End Sub


