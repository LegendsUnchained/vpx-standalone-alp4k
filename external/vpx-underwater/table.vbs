' ***********************************************************************
'               VISUAL PINBALL X EM Script by JPSalas
'                  Basic EM script up to 4 players
'		          uses core.vbs for extra functions
'
' Underwater / IPD No. 5932 / 4 Players (Petaco)
' Underwater / IPD No. 2702 / June, 1976 / 4 Players (Recel)
'
' VPX8 - version by JPSalas & pedator 2024, version 5.5.1
' ***********************************************************************

Option Explicit
Randomize

' DOF config - Foxyt - leeoneil
'
' Option for more lights effects with DOF (Undercab effects on bumpers) 
' "True" to activate (False by default)
Const Epileptikdof = False


' core.vbs constants
Const BallSize = 50  ' 50 is the normal size
Const BallMass = 1   ' 1 is the normal ball mass.

' load extra vbs files
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    On Error Resume Next
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
End Sub

' Valores Constants
Const cGameName = "Underwater" ' B2S name & file name to save highscores and other variables
Const MaxPlayers = 4           ' 1 to 4 can play
Const MaxMultiplier = 2        ' limit bonus multiplier
Const BallsPerGame = 3         ' 3 or 5
Const FreePlay = True         ' Free play or coins
Const Special1 = 500000        ' extra ball or credit
Const Special2 = 750000
Const Special3 = 950000

' Global variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim Bonus
Dim BallsRemaining(4)
Dim BonusMultiplier
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
Dim Special3Awarded(4)
Dim Score(4)
Dim HighScore
Dim Match
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim Add10
Dim Add100
Dim Add1000
Dim Add10000
Dim x

' Control variables
Dim BallsOnPlayfield

' Boolean variables
Dim bAttractMode
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bBallInPlungerLane
Dim bBallSaverActive

' core.vbs variables
Dim cball1, cball2, cball3

' *********************************************************************
'                Common rutines to all the tables
' *********************************************************************

Sub Table1_Init()
    Dim x

    ' Init som objects, like walls, targets
    VPObjects_Init
    LoadEM

    ' init core.vbs objects
    Set cball1 = New cvpmCaptiveBall
    With cball1
        .InitCaptive CapTrigger1, CapWall1, CapKicker1, 0
        .ForceTrans = .9
        .MinForce = 3.5
        .CreateEvents "cball1"
        .Start
    End With

    Set cball2 = New cvpmCaptiveBall
    With cball2
        .InitCaptive CapTrigger2, capwall2, CapKicker2, 0
        .ForceTrans = .9
        .MinForce = 3.5
        .CreateEvents "cball2"
        .Start
    End With

    Set cball3 = New cvpmCaptiveBall
    With cball3
        .InitCaptive CapTrigger3, capwall3, capKicker3, 0
        .ForceTrans = .9
        .MinForce = 3.5
        .CreateEvents "cball3"
        .Start
    End With

    ' load highscore
    Credits = 0
    Loadhs
    ScoreReel1.SetValue HSScore(1)
    If B2SOn then
        Controller.B2SSetScorePlayer 1, HSScore(1)
    End If
    UpdateCredits

    ' init all the global variables
    bFreePlay = FreePlay
    bAttractMode = False
    bOnTheFirstBall = False
    bGameInPlay = False
    bBallInPlungerLane = False
    BallsOnPlayfield = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    Match = 0
    bJustStarted = True
    Add10 = 0
    Add100 = 0
    Add1000 = 0

    ' setup table in game over mode
    EndOfGame

    'turn on GI lights
    vpmtimer.addtimer 1000, "GiOn '"

    ' Remove desktop items in FS mode
    If Table1.ShowDT then
        For each x in aReels
            x.Visible = 1
        Next
    Else
        For each x in aReels
            x.Visible = 0
        Next
    End If

    ' Start the RealTime timer
    RealTime.Enabled = 1

    ' Load table color
    LoadLut
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    If EnteringInitials then
        CollectInitials(keycode)
        Exit Sub
    End If

    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT

    ' add coins
    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
        If(Tilted = False)Then
            AddCredits 1
            PlaySound "fx_coin"
        End If
    End If

    ' plunger
    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' tilt keys
    If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25

    ' keys during game

    If bGameInPlay AND NOT Tilted Then
        If keycode = LeftTiltKey Then CheckTilt
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt
        If keycode = MechanicalTilt Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                'PlayersReel.SetValue, PlayersPlayingGame
                'PlaySound "so_fanfare1"
                Else
                    If(Credits> 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        Credits = Credits - 1
                        UpdateCredits
                        UpdateBallInPlay
                    Else
                    ' Not Enough Credits to start a game.
                    ' PlaySound "so_nocredits"
                    End If
                End If
            End If
        End If
        Else

            If keycode = StartGameKey Then
                If(bFreePlay = True)Then
                    If(BallsOnPlayfield = 0)Then
                        ResetScores
                        ResetForNewGame()
                    End If
                Else
                    If(Credits> 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            UpdateCredits
                            ResetScores
                            ResetForNewGame()
                        End If
                    Else
                    ' Not Enough Credits to start a game.
                    ' PlaySound "so_nocredits"
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If EnteringInitials then
        Exit Sub
    End If

    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT

    If bGameInPlay AND NOT Tilted Then
        ' teclas de los flipers
        If keycode = LeftFlipperKey Then SolLFlipper 0
        If keycode = RightFlipperKey Then SolRFlipper 0
    End If

    If keycode = PlungerKey Then
        Plunger.Fire
        If bBallInPlungerLane Then
            PlaySoundAt "fx_plunger", plunger
        Else
            PlaySoundAt "fx_plunger_empty", plunger
        End If
    End If
End Sub

'******************
' Table stop/pause
'******************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
    Savehs
'Controller.Stop
End Sub

'*******************
' Flipper Subs Rev3
'*******************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
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
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

' flippers top animations

Sub LeftFlipper_Animate: LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle: End Sub
Sub RightFlipper_Animate: RightFlipperTop.RotZ = RightFlipper.CurrentAngle: End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
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

'***********
' GI lights
'***********

Sub GiOn 'enciende las luces GI
    Dim bulb
	PlaySound"fx_gion"
    For each bulb in aGiLights
        bulb.State = 1
        DOF 255, DOFOn
    Next
End Sub

Sub GiOff 'apaga las luces GI
    Dim bulb
	PlaySound"fx_gioff"
    For each bulb in aGiLights
        bulb.State = 0
        DOF 255, DOFOff
    Next
End Sub

'**************
'    TILT
'**************

Sub CheckTilt
    Tilt = Tilt + TiltSensitivity
    TiltDecreaseTimer.Enabled = True
    If Tilt> 15 Then
        Tilted = True
        TiltL.State = 1
        If B2SOn then
            Controller.B2SSetTilt 1
        end if
        DisableTable True
        ' BallsRemaining(CurrentPlayer) = 0 'player looses the game 'mostly on older 1 player games
        TiltRecoveryTimer.Enabled = True 'wait for all the balls to drain
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    If Tilt> 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        GiOff
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper001.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        RightSlingshot001.Disabled = 1
        DOF 101, DOFOff
        DOF 102, DOFOff
    Else
        GiOn
        Bumper001.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        RightSlingshot001.Disabled = 0
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' all the balls have drained
    If(BallsOnPlayfield = 0)Then
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' otherwise repeat
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v4.0
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
Const maxvel = 28 'max ball velocity
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
        aBallShadow(b).Height = BOT(b).Z -Ballsize/2

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 3
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
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed & spin control
            BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory <1 Then
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

'************************************
' Diverse Collection Hit Sounds v3.0
'************************************

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

'************************************************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'************************************************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState)

    If TypeName(MyLight) = "Light" Then
        If FinalState = 2 Then
            FinalState = MyLight.State
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then
        Dim steps
        steps = Int(TotalPeriod / BlinkPeriod + .5)
        If FinalState = 2 Then
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub

'****************************************
' Init table for a new game
'****************************************

Sub ResetForNewGame()
    'debug.print "ResetForNewGame"
    Dim i

    bGameInPLay = True
    bBallSaverActive = False

    StopAttractMode
    If B2SOn then
        Controller.B2SSetGameOver 0
    end if

    GiOn

    CurrentPlayer = 1
    PlayersPlayingGame = 1
  
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
        BallsRemaining(i) = BallsPerGame
    Next
    BonusMultiplier = 1
    Bonus = 0
    UpdateBallInPlay

    Clear_Match

    ' init other variables
    Tilt = 0

    ' init game variables
    Game_Init()
 
    ' start a music?
    ' first ball
    vpmtimer.addtimer 2000, "FirstBall '"
End Sub

Sub FirstBall
    'debug.print "FirstBall"
    ' reset table for a new ball, rise droptargets ++
    ResetForNewPlayerBall()
    CreateNewBall()
End Sub

' (Re-)init table for a new ball or player

Sub ResetForNewPlayerBall()
    'debug.print "ResetForNewPlayerBall"
    AddScore 0

    ' reset multiplier to 1x

    ' turn on lights, and variables
    bExtraBallWonThisBall = False
    ResetNewBallVariables
End Sub

' Crete new ball

Sub CreateNewBall()
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass
    BallsOnPlayfield = BallsOnPlayfield + 1
    UpdateBallInPlay
    PlaySoundAt SoundFXDOF ("fx_Ballrel", 104, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4
End Sub

' player lost the ball

Sub EndOfBall()
    'debug.print "EndOfBall"
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0
    ' Lost the first ball, now it cannot accept more players
    bOnTheFirstBall = False

    ' add bonus if no tilt
    ' tilt system will take care of the next ball

    If NOT Tilted Then
        BonusCountTimer.Interval = 150 + 50 * BonusMultiplier
        BonusCountTimer.Enabled = 1
    Else
        vpmtimer.addtimer 400, "EndOfBall2 '"
    End If
End Sub

Sub BonusCountTimer_Timer
    'debug.print "BonusCount_Timer"
    If Bonus> 0 Then
        If Bonus> 10 Then
            Bonus = Bonus -10
            AddScore 10000 * BonusMultiplier
            UpdateBonusLights
        Else
            Bonus = Bonus -1
            AddScore 1000 * BonusMultiplier
            UpdateBonusLights
        End If
    Else
        BonusCountTimer.Enabled = 0
        vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub

Sub UpdateBonusLights
    Select case Bonus MOD 10
        Case 0:t1.State = 1:t2.State = 0:t3.State = 0:t4.State = 0:t5.State = 0:t6.State = 0:t7.State = 0:t8.State = 0:t9.State = 0:t10.State = 0
        Case 1:t1.State = 0:t2.State = 1:t3.State = 0:t4.State = 0:t5.State = 0:t6.State = 0:t7.State = 0:t8.State = 0:t9.State = 0:t10.State = 0
        Case 2:t1.State = 0:t2.State = 0:t3.State = 1:t4.State = 0:t5.State = 0:t6.State = 0:t7.State = 0:t8.State = 0:t9.State = 0:t10.State = 0
        Case 3:t1.State = 0:t2.State = 0:t3.State = 0:t4.State = 1:t5.State = 0:t6.State = 0:t7.State = 0:t8.State = 0:t9.State = 0:t10.State = 0
        Case 4:t1.State = 0:t2.State = 0:t3.State = 0:t4.State = 0:t5.State = 1:t6.State = 0:t7.State = 0:t8.State = 0:t9.State = 0:t10.State = 0
        Case 5:t1.State = 0:t2.State = 0:t3.State = 0:t4.State = 0:t5.State = 0:t6.State = 1:t7.State = 0:t8.State = 0:t9.State = 0:t10.State = 0
        Case 6:t1.State = 0:t2.State = 0:t3.State = 0:t4.State = 0:t5.State = 0:t6.State = 0:t7.State = 1:t8.State = 0:t9.State = 0:t10.State = 0
        Case 7:t1.State = 0:t2.State = 0:t3.State = 0:t4.State = 0:t5.State = 0:t6.State = 0:t7.State = 0:t8.State = 1:t9.State = 0:t10.State = 0
        Case 8:t1.State = 0:t2.State = 0:t3.State = 0:t4.State = 0:t5.State = 0:t6.State = 0:t7.State = 0:t8.State = 0:t9.State = 1:t10.State = 0
        Case 9:t1.State = 0:t2.State = 0:t3.State = 0:t4.State = 0:t5.State = 0:t6.State = 0:t7.State = 0:t8.State = 0:t9.State = 0:t10.State = 1
    End Select
    Select Case Bonus \ 10
        Case 0:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 1:bl1.State = 1:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 2:bl1.State = 0:bl2.State = 1:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 3:bl1.State = 0:bl2.State = 0:bl3.State = 1:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 4:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 1:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 5:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 1:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 6:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 1:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 7:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 1:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 8:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 1:bl9.State = 0:bl10.State = 0
        Case 9:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 1:bl10.State = 0
        Case 10:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 1
        Case 11:bl1.State = 1:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 1
        Case 12:bl1.State = 0:bl2.State = 1:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 1
        Case 13:bl1.State = 0:bl2.State = 0:bl3.State = 1:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 1
        Case 14:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 1:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 1
        Case 15:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 1:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 1
    End Select
End Sub

' After bonus count go to the next step
'
Sub EndOfBall2()
    'debug.print "EndOfBall2"

    Tilted = False
    Tilt = 0
    TiltL.State = 0
    If B2SOn then
        Controller.B2SSetTilt 0
    end if
    DisableTable False

    ' win extra ball?
    If(ExtraBallsAwards(CurrentPlayer)> 0)Then
        'debug.print "Extra Ball"

        ' if so then give it
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' turn off light if no more extra balls
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
        'LightShootAgain.State = 0
        'If B2SOn then
        'Controller.B2SSetShootAgain 0
        'end if
        End If

        ' extra ball sound?

        ' reset as in a new ball
        ResetForNewPlayerBall()
        CreateNewBall()
    Else ' no extra ball

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' last ball?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            CheckHighScore()
        End If

        ' this is not the last ball, chack for new player
        EndOfBallComplete()
    End If
End Sub

Sub EndOfBallComplete()
    'debug.print "EndOfBallComplete"
    Dim NextPlayer

    ' other players?
    If(PlayersPlayingGame> 1)Then
        NextPlayer = CurrentPlayer + 1
        ' if it is the last player then go to the first one
        If(NextPlayer> PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' end of game?
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then

        ' match if playing with coins
        If bFreePlay = False Then
            Verification_Match
        End If

        ' end of game
        EndOfGame()
    Else
        ' next player
        CurrentPlayer = NextPlayer

        ' update score
        AddScore 0

        ' reset table for new player
        ResetForNewPlayerBall()
        CreateNewBall()
    End If
End Sub

' Called at the end of the game

Sub EndOfGame()
    'debug.print "EndOfGame"
    bGameInPLay = False
    bJustStarted = False
    If B2SOn then
        Controller.B2SSetGameOver 1
        Controller.B2SSetBallInPlay 0
        Controller.B2SSetPlayerUp 0
        Controller.B2SSetCanPlay 0
    end if
    ' turn off flippers
    SolLFlipper 0
    SolRFlipper 0

    ' start the attract mode
    StartAttractMode
End Sub

' Fuction to calculate the balls left
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' check the highscore
Sub CheckHighscore
    Dim playertops, si, sj, i, stemp, stempplayers
    For i = 1 to 4
        sortscores(i) = 0
        sortplayers(i) = 0
    Next
    playertops = 0
    For i = 1 to PlayersPlayingGame
        sortscores(i) = Score(i)
        sortplayers(i) = i
    Next
    For si = 1 to PlayersPlayingGame
        For sj = 1 to PlayersPlayingGame-1
            If sortscores(sj)> sortscores(sj + 1)then
                stemp = sortscores(sj + 1)
                stempplayers = sortplayers(sj + 1)
                sortscores(sj + 1) = sortscores(sj)
                sortplayers(sj + 1) = sortplayers(sj)
                sortscores(sj) = stemp
                sortplayers(sj) = stempplayers
            End If
        Next
    Next
    HighScoreTimer.interval = 100
    HighScoreTimer.enabled = True
    ScoreChecker = 4
    CheckAllScores = 1
    NewHighScore sortscores(ScoreChecker), sortplayers(ScoreChecker)
End Sub

'******************
'     Match
'******************

Sub Verification_Match()
    PlaySound "fx_match"
    Match = INT(RND(1)*10) ' random between 0 and 9
    'debug.print Match
    Display_Match
    If(Score(CurrentPlayer)MOD 1000) = Match * 100 Then
        PlaySound SoundFXDOF ("fx_knocker", 130, DOFPulse, DOFKnocker)
        DOF 230, DOFPulse
        DOF 125, DOFOn
        AddCredits 1
    End If
End Sub

Sub Clear_Match()
    Match = 0
    Update_Match
    m0.State = 0:m020.state = 0:m021.state = 0 'turn off the 0 Lights
    If B2SOn then
        Controller.B2SSetMatch Match
    end if
End Sub

Sub Display_Match()
    Update_Match
    If B2SOn then
        If Match = 0 then
            Controller.B2SSetMatch 100
        else
            Controller.B2SSetMatch Match*10
        end if
    end if
End Sub

Sub Update_Match
    Select Case Match
        Case 0
            m0.State = 1:m020.state = 1:m021.state = 1
            m1.State = 0:m001.state = 0:m011.state = 0
            m2.State = 0:m002.state = 0:m012.state = 0
            m3.State = 0:m003.state = 0:m013.state = 0
            m4.State = 0:m004.state = 0:m014.state = 0
            m5.State = 0:m005.state = 0:m015.state = 0
            m6.State = 0:m006.state = 0:m016.state = 0
            m7.State = 0:m007.state = 0:m017.state = 0
            m8.State = 0:m008.state = 0:m018.state = 0
            m9.State = 0:m009.state = 0:m010.state = 0
        Case 1
            m0.State = 0:m020.state = 0:m021.state = 0
            m1.State = 1:m001.state = 1:m011.state = 1
            m2.State = 0:m002.state = 0:m012.state = 0
            m3.State = 0:m003.state = 0:m013.state = 0
            m4.State = 0:m004.state = 0:m014.state = 0
            m5.State = 0:m005.state = 0:m015.state = 0
            m6.State = 0:m006.state = 0:m016.state = 0
            m7.State = 0:m007.state = 0:m017.state = 0
            m8.State = 0:m008.state = 0:m018.state = 0
            m9.State = 0:m009.state = 0:m010.state = 0
        Case 2
            m0.State = 0:m020.state = 0:m021.state = 0
            m1.State = 0:m001.state = 0:m011.state = 0
            m2.State = 1:m002.state = 1:m012.state = 1
            m3.State = 0:m003.state = 0:m013.state = 0
            m4.State = 0:m004.state = 0:m014.state = 0
            m5.State = 0:m005.state = 0:m015.state = 0
            m6.State = 0:m006.state = 0:m016.state = 0
            m7.State = 0:m007.state = 0:m017.state = 0
            m8.State = 0:m008.state = 0:m018.state = 0
            m9.State = 0:m009.state = 0:m010.state = 0
        Case 3
            m0.State = 0:m020.state = 0:m021.state = 0
            m1.State = 0:m001.state = 0:m011.state = 0
            m2.State = 0:m002.state = 0:m012.state = 0
            m3.State = 1:m003.state = 1:m013.state = 1
            m4.State = 0:m004.state = 0:m014.state = 0
            m5.State = 0:m005.state = 0:m015.state = 0
            m6.State = 0:m006.state = 0:m016.state = 0
            m7.State = 0:m007.state = 0:m017.state = 0
            m8.State = 0:m008.state = 0:m018.state = 0
            m9.State = 0:m009.state = 0:m010.state = 0
        Case 4
            m0.State = 0:m020.state = 0:m021.state = 0
            m1.State = 0:m001.state = 0:m011.state = 0
            m2.State = 0:m002.state = 0:m012.state = 0
            m3.State = 0:m003.state = 0:m013.state = 0
            m4.State = 1:m004.state = 1:m014.state = 1
            m5.State = 0:m005.state = 0:m015.state = 0
            m6.State = 0:m006.state = 0:m016.state = 0
            m7.State = 0:m007.state = 0:m017.state = 0
            m8.State = 0:m008.state = 0:m018.state = 0
            m9.State = 0:m009.state = 0:m010.state = 0
        Case 5
            m0.State = 0:m020.state = 0:m021.state = 0
            m1.State = 0:m001.state = 0:m011.state = 0
            m2.State = 0:m002.state = 0:m012.state = 0
            m3.State = 0:m003.state = 0:m013.state = 0
            m4.State = 0:m004.state = 0:m014.state = 0
            m5.State = 1:m005.state = 1:m015.state = 1
            m6.State = 0:m006.state = 0:m016.state = 0
            m7.State = 0:m007.state = 0:m017.state = 0
            m8.State = 0:m008.state = 0:m018.state = 0
            m9.State = 0:m009.state = 0:m010.state = 0
        Case 6
            m0.State = 0:m020.state = 0:m021.state = 0
            m1.State = 0:m001.state = 0:m011.state = 0
            m2.State = 0:m002.state = 0:m012.state = 0
            m3.State = 0:m003.state = 0:m013.state = 0
            m4.State = 0:m004.state = 0:m014.state = 0
            m5.State = 0:m005.state = 0:m015.state = 0
            m6.State = 1:m006.state = 1:m016.state = 1
            m7.State = 0:m007.state = 0:m017.state = 0
            m8.State = 0:m008.state = 0:m018.state = 0
            m9.State = 0:m009.state = 0:m010.state = 0
        Case 7
            m0.State = 0:m020.state = 0:m021.state = 0
            m1.State = 0:m001.state = 0:m011.state = 0
            m2.State = 0:m002.state = 0:m012.state = 0
            m3.State = 0:m003.state = 0:m013.state = 0
            m4.State = 0:m004.state = 0:m014.state = 0
            m5.State = 0:m005.state = 0:m015.state = 0
            m6.State = 0:m006.state = 0:m016.state = 0
            m7.State = 1:m007.state = 1:m017.state = 1
            m8.State = 0:m008.state = 0:m018.state = 0
            m9.State = 0:m009.state = 0:m010.state = 0
        Case 8
            m0.State = 0:m020.state = 0:m021.state = 0
            m1.State = 0:m001.state = 0:m011.state = 0
            m2.State = 0:m002.state = 0:m012.state = 0
            m3.State = 0:m003.state = 0:m013.state = 0
            m4.State = 0:m004.state = 0:m014.state = 0
            m5.State = 0:m005.state = 0:m015.state = 0
            m6.State = 0:m006.state = 0:m016.state = 0
            m7.State = 0:m007.state = 0:m017.state = 0
            m8.State = 1:m008.state = 1:m018.state = 1
            m9.State = 0:m009.state = 0:m010.state = 0
        Case 9
            m0.State = 0:m020.state = 0:m021.state = 0
            m1.State = 0:m001.state = 0:m011.state = 0
            m2.State = 0:m002.state = 0:m012.state = 0
            m3.State = 0:m003.state = 0:m013.state = 0
            m4.State = 0:m004.state = 0:m014.state = 0
            m5.State = 0:m005.state = 0:m015.state = 0
            m6.State = 0:m006.state = 0:m016.state = 0
            m7.State = 0:m007.state = 0:m017.state = 0
            m8.State = 0:m008.state = 0:m018.state = 0
            m9.State = 1:m009.state = 1:m010.state = 1
    End Select
End Sub

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

Sub Drain_Hit()
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1
    PlaySoundAt "fx_drain", Drain
    DOF 256, DOFPulse

    'tilted?
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if still playing and not tilted
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' ballsaver?
        If(bBallSaverActive = True)Then
            CreateNewBall()
        Else
            ' last ball?
            If(BallsOnPlayfield = 0)Then
                StopEndOfBallMode
                vpmtimer.addtimer 500, "EndOfBall '"
                Exit Sub
            End If
        End If
    End If
End Sub

Sub swPlungerRest_Hit()
    bBallInPlungerLane = True
End Sub

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
End Sub

' ****************************************
'             Score functions
' ****************************************

Sub AddScore(Points)
    If Tilted Then Exit Sub
    Select Case Points
        Case 100, 1000, 10000
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore points
            ' sounds
            If Points = 1000 AND(Score(CurrentPlayer)MOD 10000) \ 1000 = 0 Then  'new reel 10000
                PlaySound SoundFXDOF ("chime_10000",141,DOFPulse,DOFChimes)
            ElseIf Points = 100 AND(Score(CurrentPlayer)MOD 1000) \ 100 = 0 Then 'new reel 1000
                PlaySound SoundFXDOF ("chime_1000",142,DOFPulse,DOFChimes)
            Else
                PlaySound "chime_" &Points
                PlaySound SoundFXDOF ("chime_",143,DOFPulse,DOFChimes)
            End If
        Case 200, 300, 400, 500
            Add100 = Add100 + Points \ 100
            AddScore100Timer.Enabled = TRUE
        Case 2000, 3000, 4000, 5000
            Add1000 = Add1000 + Points \ 1000
            AddScore1000Timer.Enabled = TRUE
        Case 20000, 30000, 40000, 50000
            Add10000 = Add10000 + Points \ 10000
            AddScore10000Timer.Enabled = TRUE
    End Select

    ' check for higher score and specials
    If Score(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
        AwardSpecial
        Special1Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
        AwardSpecial
        Special2Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
        AwardSpecial
        Special3Awarded(CurrentPlayer) = True
    End If
End Sub

'************************************
'       Score sound Timers
'************************************

Sub AddScore10000Timer_Timer()
    if Add10000> 0 then
        AddScore 10000
        Add10000 = Add10000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore100Timer_Timer()
    if Add100> 0 then
        AddScore 100
        Add100 = Add100 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore1000Timer_Timer()
    if Add1000> 0 then
        AddScore 1000
        Add1000 = Add1000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

'*******************
'     BONUS
'*******************

Sub AddBonus(bonuspoints)
    If(Tilted = False)Then
        Bonus = Bonus + bonuspoints
        If Bonus> 150 Then
            Bonus = 150
        End If
        UpdateBonusLights
        ' this table only
        Check123
    End if
End Sub

Sub AddBonusMultiplier(multi)
    If(Tilted = False)Then
        BonusMultiplier = BonusMultiplier + multi
        If BonusMultiplier> MaxMultiplier Then
            BonusMultiplier = MaxMultiplier
        End If
        UpdateMultiplierLights
    End if
End Sub

Sub UpdateMultiplierLights
    Select Case BonusMultiplier
    'Case 1:li2.State = 1:li3.State = 0:li4.State = 0
    'Case 2:li2.State = 0:li3.State = 1:li4.State = 0
    'Case 3:li2.State = 0:li3.State = 0:li4.State = 1
    End Select
End Sub

'**********************************
'        Score EM reels
'**********************************

Sub UpdateScore(playerpoints)
    Select Case CurrentPlayer
        Case 1:ScoreReel1.Addvalue playerpoints
        Case 2:ScoreReel2.Addvalue playerpoints
        Case 3:ScoreReel3.Addvalue playerpoints
        Case 4:ScoreReel4.Addvalue playerpoints
    End Select
    If B2SOn then
        Controller.B2SSetScorePlayer CurrentPlayer, Score(CurrentPlayer)
    end if
End Sub

Sub ResetScores
    ScoreReel1.SetValue 1:ScoreReel1.ResetToZero
    ScoreReel2.ResetToZero
    ScoreReel3.ResetToZero
    ScoreReel4.ResetToZero
    If B2SOn then
        Controller.B2SSetScorePlayer1 0
        Controller.B2SSetScoreRolloverPlayer1 0
        Controller.B2SSetScorePlayer2 0
        Controller.B2SSetScoreRolloverPlayer2 0
        Controller.B2SSetScorePlayer3 0
        Controller.B2SSetScoreRolloverPlayer3 0
        Controller.B2SSetScorePlayer4 0
        Controller.B2SSetScoreRolloverPlayer4 0
    end if
End Sub

Sub AddCredits(value) 'limit to 15 credits
    If Credits <9 Then
        Credits = Credits + value
        CreditsReel.SetValue Credits
        UpdateCredits
    end if
End Sub

Sub UpdateCredits
     If Credits> 0 Then
    DOF 125 , DOFOn 
    Else
    DOF 125 , DOFOff
    End If
    PlaySound "fx_relay"
    CreditsReel.SetValue credits
    If B2SOn then
        Controller.B2SSetCredits Credits
    end if
End Sub

Sub UpdateBallInPlay 'actually updates all backdrop lights
    'Ball in play
    Select Case Balls
        Case 0:bip1.State = 0:bip2.State = 0:bip3.State = 0:bip4.State = 0:bip5.State = 0
        Case 1:bip1.State = 1:bip2.State = 0:bip3.State = 0:bip4.State = 0:bip5.State = 0
        Case 2:bip1.State = 0:bip2.State = 1:bip3.State = 0:bip4.State = 0:bip5.State = 0
        Case 3:bip1.State = 0:bip2.State = 0:bip3.State = 1:bip4.State = 0:bip5.State = 0
        Case 4:bip1.State = 0:bip2.State = 0:bip3.State = 0:bip4.State = 1:bip5.State = 0
        Case 5:bip1.State = 0:bip2.State = 0:bip3.State = 0:bip4.State = 0:bip5.State = 1
    End Select
    If B2SOn then
        Controller.B2SSetBallInPlay Balls
    End If
    ' Update player light
    Select Case CurrentPlayer
        Case 0:pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 0
        Case 1:pl1.State = 1:pl2.State = 0:pl3.State = 0:pl4.State = 0
        Case 2:pl1.State = 0:pl2.State = 1:pl3.State = 0:pl4.State = 0
        Case 3:pl1.State = 0:pl2.State = 0:pl3.State = 1:pl4.State = 0
        Case 4:pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 1
    End Select
    If B2SOn then
        Controller.B2SSetPlayerUp CurrentPlayer
    End If
    ' Player can play
    Select Case PlayersPlayingGame
        Case 0:cp1.State = 0:cp2.State = 0:cp3.State = 0:cp4.State = 0
        Case 1:cp1.State = 1:cp2.State = 0:cp3.State = 0:cp4.State = 0
        Case 2:cp1.State = 0:cp2.State = 1:cp3.State = 0:cp4.State = 0
        Case 3:cp1.State = 0:cp2.State = 0:cp3.State = 1:cp4.State = 0
        Case 4:cp1.State = 0:cp2.State = 0:cp3.State = 0:cp4.State = 1
    End Select
    If B2SOn then
        Controller.B2SSetCanPlay PlayersPlayingGame
    end if
End Sub

'*************************
'        Specials
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        PlaySound SoundFXDOF ("fx_knocker", 130, DOFPulse, DOFKnocker)
        DOF 230, DOFPulse
        DOF 125, DOFOn
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        light004.State = 1
    Else
    'Addscore 5000
    END If
End Sub

Sub AwardSpecial()
    PlaySound SoundFXDOF ("fx_knocker", 130, DOFPulse, DOFKnocker)
    DOF 230, DOFPulse
    DOF 125, DOFOn
    AddCredits 1
End Sub

Sub AwardAddaBall()
    If BallsRemaining(CurrentPlayer) <11 Then
        PlaySound SoundFXDOF ("fx_knocker", 130, DOFPulse, DOFKnocker)
        DOF 230, DOFPulse
        DOF 125, DOFOn
        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) + 1
        UpdateBallInPlay
    End If
End Sub

' ********************************
'        Attract Mode
' ********************************
' use the"Blink Pattern" of each light

Sub StartAttractMode()
    Dim x
    bAttractMode = True
    For each x in aLights
        x.State = 2
    Next
    GameOverL.State = 1
'update current player and balls
End Sub

Sub StopAttractMode()
    Dim x
    bAttractMode = False
    TurnOffPlayfieldLights
    ResetScores
    GameOverL.State = 0
End Sub

'*********************************
'    Load / Save / Highscore
'*********************************

Sub Loadhs
    ' Based on Black's Highscore routines
    Dim FileObj
    Dim ScoreFile, TextStr
    Dim temp1
    Dim temp2
    Dim temp3
    Dim temp4
    Dim temp5
    Dim temp6
    Dim temp8
    Dim temp9
    Dim temp10
    Dim temp11
    Dim temp12
    Dim temp13
    Dim temp14
    Dim temp15
    Dim temp16
    Dim temp17

    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory)then
        Credits = 0
        Exit Sub
    End If
    If Not FileObj.FileExists(UserDirectory & cGameName& ".txt")then
        Credits = 0
        Exit Sub
    End If
    Set ScoreFile = FileObj.GetFile(UserDirectory & cGameName& ".txt")
    Set TextStr = ScoreFile.OpenAsTextStream(1, 0)
    If(TextStr.AtEndOfStream = True)then
        Exit Sub
    End If
    temp1 = TextStr.ReadLine
    temp2 = textstr.readline

    HighScore = cdbl(temp1)
    If HighScore <1 then
        temp8 = textstr.readline
        temp9 = textstr.readline
        temp10 = textstr.readline
        temp11 = textstr.readline
        temp12 = textstr.readline
        temp13 = textstr.readline
        temp14 = textstr.readline
        temp15 = textstr.readline
        temp16 = textstr.readline
        temp17 = textstr.readline
    End If
    TextStr.Close
    Credits = cdbl(temp2)

    If HighScore <1 then
        HSScore(1) = int(temp8)
        HSScore(2) = int(temp9)
        HSScore(3) = int(temp10)
        HSScore(4) = int(temp11)
        HSScore(5) = int(temp12)

        HSName(1) = temp13
        HSName(2) = temp14
        HSName(3) = temp15
        HSName(4) = temp16
        HSName(5) = temp17
    End If
    Set ScoreFile = Nothing
    Set FileObj = Nothing
    SortHighscore 'added to fix a previous error
End Sub

Sub Savehs
    ' Based on Black's Highscore routines
    Dim FileObj
    Dim ScoreFile
    Dim xx
    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory)then
        Exit Sub
    End If
    Set ScoreFile = FileObj.CreateTextFile(UserDirectory & cGameName& ".txt", True)
    ScoreFile.WriteLine 0
    ScoreFile.WriteLine Credits
    For xx = 1 to 5
        scorefile.writeline HSScore(xx)
    Next
    For xx = 1 to 5
        scorefile.writeline HSName(xx)
    Next
    ScoreFile.Close
    Set ScoreFile = Nothing
    Set FileObj = Nothing
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 1 to 5
        For j = 1 to 4
            If HSScore(j) <HSScore(j + 1)Then
                tmp = HSScore(j + 1)
                tmp2 = HSName(j + 1)
                HSScore(j + 1) = HSScore(j)
                HSName(j + 1) = HSName(j)
                HSScore(j) = tmp
                HSName(j) = tmp2
            End If
        Next
    Next
End Sub

' ***************************************************
' GNMOD - Multiple High Score Display and Collection
' jpsalas: changed ramps by flashers to remove extra shadow
' ***************************************************

Dim EnteringInitials ' Normally zero, set to non-zero to enter initials
EnteringInitials = False
Dim ScoreChecker
ScoreChecker = 0
Dim CheckAllScores
CheckAllScores = 0
Dim sortscores(4)
Dim sortplayers(4)

Dim PlungerPulled
PlungerPulled = 0

Dim SelectedChar   ' character under the "cursor" when entering initials

Dim HSTimerCount   ' Pass counter For HS timer, scores are cycled by the timer
HSTimerCount = 5   ' Timer is initially enabled, it'll wrap from 5 to 1 when it's displayed

Dim InitialString  ' the string holding the player's initials as they're entered

Dim AlphaString    ' A-Z, 0-9, space (_) and backspace (<)
Dim AlphaStringPos ' pointer to AlphaString, move Forward and backward with flipper keys
AlphaString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_<"

Dim HSNewHigh      ' The new score to be recorded

Dim HSScore(5)     ' High Scores read in from config file
Dim HSName(5)      ' High Score Initials read in from config file

' default high scores
HSScore(1) = 450000
HSScore(2) = 440000
HSScore(3) = 430000
HSScore(4) = 420000
HSScore(5) = 410000

HSName(1) = "AAA"
HSName(2) = "ZZZ"
HSName(3) = "XXX"
HSName(4) = "ABC"
HSName(5) = "BBB"

Sub HighScoreTimer_Timer
    If EnteringInitials then
        If HSTimerCount = 1 then
            SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
            HSTimerCount = 2
        Else
            SetHSLine 3, InitialString
            HSTimerCount = 1
        End If
    ElseIf bGameInPlay then
        SetHSLine 1, "HIGH SCORE1"
        SetHSLine 2, HSScore(1)
        SetHSLine 3, HSName(1)
        HSTimerCount = 5 ' set so the highest score will show after the game is over
        HighScoreTimer.enabled = false
    ElseIf CheckAllScores then
        NewHighScore sortscores(ScoreChecker), sortplayers(ScoreChecker)
    Else
        ' cycle through high scores
        HighScoreTimer.interval = 2000
        HSTimerCount = HSTimerCount + 1
        If HsTimerCount> 5 then
            HSTimerCount = 1
        End If
        SetHSLine 1, "HIGH SCORE" + FormatNumber(HSTimerCount, 0)
        SetHSLine 2, HSScore(HSTimerCount)
        SetHSLine 3, HSName(HSTimerCount)
    End If
End Sub

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
    Dim xFor
    StartHSArray = array(0, 1, 12, 22)
    EndHSArray = array(0, 11, 21, 31)
    StrLen = len(string)
    Index = 1

    For xFor = StartHSArray(LineNo)to EndHSArray(LineNo)
        Eval("HS" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub NewHighScore(NewScore, PlayNum)
    If NewScore> HSScore(5)then
        HighScoreTimer.interval = 500
        HSTimerCount = 1
        AlphaStringPos = 1      ' start with first character "A"
        EnteringInitials = true ' intercept the control keys while entering initials
        InitialString = ""      ' initials entered so far, initialize to empty
        SetHSLine 1, "PLAYER " + FormatNumber(PlayNum, 0)
        SetHSLine 2, "ENTER NAME"
        SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
        HSNewHigh = NewScore
        AwardSpecial
    End If
    ScoreChecker = ScoreChecker-1
    If ScoreChecker = 0 then
        CheckAllScores = 0
    End If
End Sub

Sub CollectInitials(keycode)
    Dim i
    If keycode = LeftFlipperKey Then
        ' back up to previous character
        AlphaStringPos = AlphaStringPos - 1
        If AlphaStringPos <1 then
            AlphaStringPos = len(AlphaString) ' handle wrap from beginning to End
            If InitialString = "" then
                ' Skip the backspace If there are no characters to backspace over
                AlphaStringPos = AlphaStringPos - 1
            End If
        End If
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
        PlaySound "fx_Previous"
    ElseIf keycode = RightFlipperKey Then
        ' advance to Next character
        AlphaStringPos = AlphaStringPos + 1
        If AlphaStringPos> len(AlphaString)or(AlphaStringPos = len(AlphaString)and InitialString = "")then
            ' Skip the backspace If there are no characters to backspace over
            AlphaStringPos = 1
        End If
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
        PlaySound "fx_Next"
    ElseIf keycode = StartGameKey or keycode = PlungerKey Then
        SelectedChar = MID(AlphaString, AlphaStringPos, 1)
        If SelectedChar = "_" then
            InitialString = InitialString & " "
            PlaySound("fx_Esc")
        ElseIf SelectedChar = "<" then
            InitialString = MID(InitialString, 1, len(InitialString)- 1)
            If len(InitialString) = 0 then
                ' If there are no more characters to back over, don't leave the < displayed
                AlphaStringPos = 1
            End If
            PlaySound("fx_Esc")
        Else
            InitialString = InitialString & SelectedChar
            PlaySound("fx_Enter")
        End If
        If len(InitialString) <3 then
            SetHSLine 3, InitialString & SelectedChar
        End If
    End If
    If len(InitialString) = 3 then
        ' save the score
        For i = 5 to 1 step -1
            If i = 1 or(HSNewHigh> HSScore(i)and HSNewHigh <= HSScore(i - 1))then
                ' Replace the score at this location
                If i <5 then
                    HSScore(i + 1) = HSScore(i)
                    HSName(i + 1) = HSName(i)
                End If
                EnteringInitials = False
                HSScore(i) = HSNewHigh
                HSName(i) = InitialString
                HSTimerCount = 5
                HighScoreTimer_Timer
                HighScoreTimer.interval = 2000
                PlaySound("fx_Bong")
                Exit Sub
            ElseIf i <5 then
                ' move the score in this slot down by 1, it's been exceeded by the new score
                HSScore(i + 1) = HSScore(i)
                HSName(i + 1) = HSName(i)
            End If
        Next
    End If
End Sub
' End GNMOD

'*******************
' Realtime updates
'*******************

Sub Realtime_Timer
    RollingUpdate
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
'  *********     G A M E  C O D E  S T A R T S  H E R E      *********
' *********************************************************************
'***********************************************************************

Sub VPObjects_Init 'init objects
    TurnOffPlayfieldLights()
    ' change the apron
    If BallsPerGame = 5 Then
        Apron.Image = "plastics_5ball"
    End If
End Sub

' Dim all the variables
Dim TorpedoAdvance

Sub Game_Init() 'called at the start of a new game
    'Start music?
    'Init variables?
    'ResetDropTargets
    'Start or init timers
    'Init lights?
    TurnOffPlayfieldLights()
End Sub

Sub StopEndOfBallMode()     'called when the last ball is drained
End Sub

Sub ResetNewBallVariables() 'init variables & lights for a new ball or player
    TurnOffPlayfieldLights
    If Balls = BallsPerGame Then
        BonusMultiplier = 2
        Light001.State = 1
    Else
        BonusMultiplier = 1
    End If
    Bonus = 0
    UpdateBonusLights
    If BallsPerGame = 3 Then light002.State = 1
    TorpedoAdvance = 0
    light026.State = 1
    light027.State = 1
    light028.State = 1
    light014.State = 1
    light015.State = 1
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

' *********************************************************************
'                        Table Object Hit Events
' *********************************************************************
' Any target hit Sub will do this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/modes this trigger is a member of
' - set the "LastSwicthHit" variable in case it is needed later
' *********************************************************************

' Slingshots
Dim LStep, RStep, RStep2

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_slingshot",103,DOFPulse,DOFcontactors), Lemk
    DOF 203 , DOFPulse
    If Epileptikdof = True Then DOF 201, DOFPulse End If
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add points
    AddScore 100
    AddBonus 1
' some effect?
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing004.Visible = 0:LeftSLing003.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing003.Visible = 0:LeftSLing002.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing002.Visible = 0:Lemk.RotX = -20:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_slingshot",104,DOFPulse,DOFcontactors), Remk
    DOF 204 , DOFPulse
    If Epileptikdof = True Then DOF 201, DOFPulse End If
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add points
    AddScore 100
    AddBonus 1
' add effect?
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:Remk.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub RightSlingShot001_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_slingshot",107,DOFPulse,DOFcontactors), Remk001
    DOF 207 , DOFPulse
    If Epileptikdof = True Then DOF 201, DOFPulse End If
    Rubber023.Visible = 1
    Remk001.RotX = 26
    RStep2 = 0
    RightSlingShot001.TimerEnabled = True
    ' add points
    AddScore 100
    AddBonus 1
' add effect?
End Sub

Sub RightSlingShot001_Timer
    Select Case RStep2
        Case 1:Rubber023.Visible = 0:Rubber022.Visible = 1:Remk001.RotX = 14
        Case 2:Rubber022.Visible = 0:Rubber021.Visible = 1:Remk001.RotX = 2
        Case 3:Rubber021.Visible = 0:Remk001.RotX = -20:RightSlingshot001.TimerEnabled = 0
    End Select
    RStep2 = RStep2 + 1
End Sub

'***********************
'       Rubbers
'***********************

Sub rlband006_Hit
    If Tilted then Exit Sub
    AddScore 100
    AddBonus 1
End Sub

Sub rlband003_Hit
    If Tilted then Exit Sub
    AddScore 100
    AddBonus 1
End Sub

Sub rlband002_Hit
    If Tilted then Exit Sub
    AddScore 100
    AddBonus 1
End Sub

'*********
' Bumpers
'*********

Sub Bumper001_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",106,DOFPulse,DOFContactors), bumper001
    DOF 206, DOFPulse
    If Epileptikdof = True Then DOF 201, DOFPulse End If
    AddScore 100
    AddBonus 1
End Sub

'*****************
'     Lanes
'*****************

'top

Sub Trigger008_Hit '1
    PlaySoundAt "fx_sensor", Trigger008
    If Tilted Then Exit Sub
    DOF 218, DOFPulse
    AddScore 1000
    light026.State = 0
    light014.State = 0
    light029.State = 1
    Check123
End Sub

Sub Trigger009_Hit '2
    PlaySoundAt "fx_sensor", Trigger009
    If Tilted Then Exit Sub
    DOF 219, DOFPulse
    AddScore 1000
    light027.State = 0
    light015.State = 0
    light030.State = 1
    Check123
End Sub

Sub Trigger010_Hit '3
    PlaySoundAt "fx_sensor", Trigger010
    If Tilted Then Exit Sub
    DOF 220, DOFPulse
    AddScore 1000
    light028.State = 0
    light031.State = 1
    Check123
End Sub

'outlanes

Sub Trigger002_Hit
    PlaySoundAt "fx_sensor", Trigger002
    If Tilted Then Exit Sub
    DOF 212, DOFPulse
    AddScore 10000
End Sub

Sub Trigger003_Hit
    PlaySoundAt "fx_sensor", Trigger003
    If Tilted Then Exit Sub
    DOF 213, DOFPulse
    AddScore 10000
End Sub

'inlanes

Sub Trigger004_Hit '2
    PlaySoundAt "fx_sensor", Trigger004
    If Tilted Then Exit Sub
    DOF 214, DOFPulse
    AddScore 1000
    light027.State = 0
    light015.State = 0
    light030.State = 1
    Check123
End Sub

Sub Trigger001_Hit '1
    PlaySoundAt "fx_sensor", Trigger001
    If Tilted Then Exit Sub
    DOF 211, DOFPulse
    AddScore 1000
    light026.State = 0
    light014.State = 0
    light029.State = 1
    Check123
End Sub

' torpedo triggers

Sub Trigger005_Hit '1
    PlaySoundAt "fx_sensor", Trigger005
    If Tilted Then Exit Sub
    DOF 215, DOFPulse
    PlaySound "Torpedo1"
    DOF 132, DOFPulse
    Lbumper1a.Duration 0, 800, 1
    Lbumper1b.Duration 0, 800, 1
    If light029.State Then
        Addscore 5000
        AddBonus 10
    Else
        AddScore 500
    End If
    If light032.State Then
        AwardExtraBall
        light032.State = 0
    End If
End Sub

Sub Trigger006_Hit '2
    PlaySoundAt "fx_sensor", Trigger006
    If Tilted Then Exit Sub
    DOF 216, DOFPulse
    PlaySound "Torpedo1"
    DOF 132, DOFPulse
    Lbumper1a.Duration 0, 800, 1
    Lbumper1b.Duration 0, 800, 1
    If light030.State Then
        Addscore 5000
        AddBonus 10
    Else
        AddScore 500
    End If
    If light032.State Then
        AwardExtraBall
        light032.State = 0
    End If
End Sub

Sub Trigger007_Hit '3
    PlaySoundAt "fx_sensor", Trigger007
    If Tilted Then Exit Sub
    DOF 217, DOFPulse
    PlaySound "Torpedo1"
    DOF 132, DOFPulse
    Lbumper1a.Duration 0, 800, 1
    Lbumper1b.Duration 0, 800, 1
    If light031.State Then
        Addscore 5000
        AddBonus 10
    Else
        AddScore 500
    End If
    If light032.State Then
        AwardExtraBall
        light032.State = 0
    End If
End Sub

Sub Check123
    If light029.State + light030.State + light031.State = 3 Then
        If Bonus>= 110 AND Bonus <120 Then 'lit extra ball light
            light032.State = 1
        End If
    End If
    If Bonus> 140 Then
        light033.State = 1 'Special light
    End If
End Sub

'right Gate

Sub Trigger011_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    AddBonus 10
    DOF 221, DOFPulse
    PlaySound "Torpedo2"
    DOF 132, DOFPulse
    Lbumper1a.Duration 0, 800, 1
    Lbumper1b.Duration 0, 800, 1
End Sub

'************************
'       Targets
'************************

Sub Target001_hit 'lower
    PlaySoundAtBall SoundFXDOF ("fx_target", 110, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 231, DOFPulse
    If light003.State Then
        Addscore 50000
        light003.State = 0
        ResetTorpedoLights
        If BallsPerGame = 5 Then
            light002.State = 0
        End If
    Else
        Addscore 10000
    End If
End Sub

Sub Target002_hit 'top
    PlaySoundAtBall SoundFXDOF ("fx_target", 117, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 232, DOFPulse
    If light003.State Then
        light002.State = 1
    Else
        light003.State = 1
    End If
    Addscore 10000
End Sub

Sub ResetTorpedoLights
    Light029.State = 0
    light030.State = 0
    light031.State = 0
    light026.State = 1
    light027.State = 1
    light028.State = 1
    light014.State = 1
    light015.State = 1
End Sub

'****************
'    Kicker
'****************

Sub Kicker001_Hit
    If NOT Tilted Then
        DOF 330,DOFPulse
        Addscore 500
        Addscore 500
    End If
    vpmtimer.AddTimer 2000,"DOF 230,DOFPulse:DOF 105,DOFPulse:PlaySoundAt SoundFX(""fx_kicker"",DOFContactors), Kicker001: Kicker001.Kick 68, 10 '"
End Sub

'**************
'   Spinner
'**************

Sub Spinner001_Spin
    PlaySoundAt "fx_spinner", Spinner001
    If Tilted Then Exit Sub
    DOF 233, DOFPulse
    Addscore 100
    AddBonus 1
End Sub

'************
' Varitarget
'************

Dim variawarded, vtpos, varipos
variawarded = False
vtpos = Array(20, 18, 16, 14, 12, 10, 8, 6, 4, 2, 0, -2, -4, -6, -8, -10, -12, -14, -16, -18, -20, -22)

Sub vt_Hit(idx)
    Dim x
    If ActiveBall.VelY <0 Then
        varipos = idx
        ActiveBall.VelY = ActiveBall.VelY * 0.915
        PlaySound "fx_solenoidoff"
        DOF 131, DOFPulse 
        varitarget.roty = vtpos(idx)
    Else
        If VTTimer.Enabled = False Then
            VTTimer.Interval = 300
            VTTimer.Enabled = True
        End If
    End If
End Sub

Sub VTTimer_Timer()
    VTTimer.Interval = 30
    varipos = varipos -1
    If varipos <0 Then varipos = 0
    If varipos> 21 Then varipos = 21
    varitarget.roty = vtpos(varipos)
    If varipos = 19 AND variawarded = False Then
        If light002.State Then AddBonus 50
        If light033.State Then
            AddScore 50000::variawarded = True
        Else
            AddScore 5000:variawarded = True
        End If
    End If
    If varipos = 16 AND variawarded = False Then
        AddScore 4000:variawarded = True
        If light002.State Then AddBonus 40
    End If
    If varipos = 13 AND variawarded = False Then
        AddScore 3000:variawarded = True
        If light002.State Then AddBonus 30
    End If
    If varipos = 10 AND variawarded = False Then
        AddScore 2000:variawarded = True
        If light002.State Then AddBonus 20
    End If
    If varipos = 7 AND variawarded = False Then
        AddScore 1000:variawarded = True
        If light002.State Then AddBonus 10
    End If
    If varipos = 4 AND variawarded = False Then
        AddScore 1000:variawarded = True
        If light002.State Then AddBonus 10
    End If
    If varipos = 0 Then
        variawarded = False
        VTTimer.Enabled = False
    End If
End Sub

'******************************
'     DOF lights ball entrance
'******************************
Sub TriggerLaunch_Hit
If Tilted Then Exit Sub
DOF 260, DOFPulse
End sub