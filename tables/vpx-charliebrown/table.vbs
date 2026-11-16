' Charlie Brown Baseball Pinball (Original 2026)

Option Explicit ' Force explicit variable declaration
Randomize

' core.vbs constants
Const BallSize = 50 ' 50 is the normal size
Const BallMass = 1  ' 1 is the normal ball mass.

'DOF config - (DOF server needs to be updated)
'101 Left Flipper
'102 Right Flipper
'103
'104
'105 left target
'106 center target
'107 right target
'108
'109 bumper center
'110
'111 top left target
'112 top center target
'113 top right target
'114
'115 Captive ball target
'116
'117
'118
'119
'120
'121
'122 Knocker
'123
'124
'125
'126
'128
'129
'130
'131
'132
'133

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
Const TableName = "CBBaseball" ' file name to save highscores and other variables
Const cGameName = "CBBaseball" ' B2S name & DOF config
Const MaxPlayers = 1           ' 1 to 4 can play
Const MaxMultiplier = 3        ' limit bonus multiplier
Const MaxBonus = 20            ' highest bonus count
Const FreePlay = True         ' Free play or coins

' Global variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim Bonus
Dim BallsRemaining(4)
Dim BonusMultiplier
Dim PlayfieldMultiplier
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
Dim Special3Awarded(4)
Dim Special4Awarded(4)
Dim Special1
Dim Special2
Dim Special3
Dim Special4
Dim Score(4)
Dim HighScore
Dim Match
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim Add10
Dim Add100
Dim Add1000
Dim LastSwicthHit

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
Dim cbLeft

' *********************************************************************
'                Common rutines to all the tables
' *********************************************************************

Sub Table1_Init()
    Dim x

    ' Init som objects, like walls, targets
    VPObjects_Init
    LoadEM

    Set cbLeft = New cvpmCaptiveBall
    With cbLeft
        .InitCaptive CapTrigger1, CapWall1, Array(CapKicker1, CapKicker1a), 340
        .NailedBalls = 1
        .ForceTrans = .9
        .MinForce = 3.5
        '.CreateEvents "cbLeft"
        .Start
    End With
    CapKicker1.CreateSizedBallWithMass BallSize / 2, BallMass

    ' load highscore
    Credits = 0
    Loadhs
    vpmTimer.AddTimer 1000, "UpdateStartUp '"

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
    LastSwicthHit = ""

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
End Sub

Sub UpdateStartup
    ScoreReel1.SetValue HSScore(1)
    CreditsReel.SetValue credits
    If B2SOn then
        Controller.B2SSetScorePlayer 1, HSScore(1)
        Controller.B2SSetCredits Credits
    End If
End Sub

'******************
' Captive Ball Subs
'******************
Sub CapTrigger1_Hit:cbLeft.TrigHit ActiveBall:End Sub
Sub CapTrigger1_UnHit:cbLeft.TrigHit 0:End Sub
Sub CapWall1_Hit:cbLeft.BallHit ActiveBall:PlaySoundAtBall "fx_collide":End Sub
Sub CapKicker1a_Hit:cbLeft.BallReturn Me:End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    ' Reset High Score using the [7] key
    if keycode = "8" then 
        ResetHS
        ResHS.Visible = 1
        vpmTimer.AddTimer 2000, "Reshs.visible = 0 '"
    End If

    If EnteringInitials then
        CollectInitials(keycode)
        Exit Sub
    End If

    ' add coins
    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
        If(Tilted = False) Then
            AddCredits 1
            PlaySoundAt "fx_coin", coinslot
        End If
    End If

    ' plunger
    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' tilt keys
    If keycode = LeftTiltKey Then Nudge 90, 2:PlaySound "fx_nudge", 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 2:PlaySound "fx_nudge", 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 2:PlaySound "fx_nudge", 0, 1, 1, 0.25

    ' keys during game

    If bGameInPlay AND NOT Tilted Then
        If keycode = LeftTiltKey Then CheckTilt
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt
        If keycode = MechanicalTilt Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                'PlayersReel.SetValue, PlayersPlayingGame
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        Credits = Credits - 1
                        UpdateCredits
                        UpdateBallInPlay
                    Else
                    ' Not Enough Credits to start a game.
                    'PlaySound "so_nocredits"
                    End If
                End If
            End If
        End If
        Else

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        DisableTable 0
                        ResetScores
                        ResetForNewGame()
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            DisableTable 0
                            Credits = Credits - 1
                            UpdateCredits
                            ResetScores
                            ResetForNewGame()
                        End If
                    Else
                    ' Not Enough Credits to start a game.
                    'PlaySound "so_nocredits"
                    End If
                End If
            End If
    End If ' If (GameInPlay)
    if keycode = "3" then TurnON aBumper1Light
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If EnteringInitials then
        Exit Sub
    End If

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
    If B2SOn then
        Controller.Stop
    End If
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
        RotateLaneLights 0
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
        RotateLaneLights 1
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub


'**********************************
' Real Time Flipper EOS adjustment
' based on JLouLouLou flipper script
'     JP's physics Version 2026
'**********************************

Dim LeftFlipperOn
Dim RightFlipperOn

' EOS Torque when flipper hold up ( EOS Coil is fully charged. 
' Ampere increase due to flipper can't move or when it is pushed back when "On". EOS Coil have more power )
Const FullStrokeEOS_Torque = 0.9 

' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have 
' less Ampere due to flipper can freely move. EOS Coil have less power )
Const LiveStrokeEOS_Torque = 0.2 

LeftFlipper.TimerInterval = 1
LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's EOS changer
    'Left End Of Stroke Routine
    If LeftFlipperOn = 1 Then
        If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
            LeftFlipper.EOSTorque = FullStrokeEOS_Torque
        End If
    Else
        LeftFlipper.EOSTorque = LiveStrokeEOS_Torque
    End If
    'Right End Of Stroke Routine
    If RightFlipperOn = 1 Then
        If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
            RightFlipper.EOSTorque = FullStrokeEOS_Torque
        End If
    Else
        RightFlipper.EOSTorque = LiveStrokeEOS_Torque
    End If
End Sub

'***********
' GI lights
'***********

Sub GiOn 'enciende las luces GI
    Dim bulb
    PlaySound "fx_gion"
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff 'apaga las luces GI
    Dim bulb
    PlaySound "fx_gioff"
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = factor
    Next
End Sub

Sub TurnON(Col) 'turn on all the lights in a collection
    Dim i
    For each i in Col
        i.State = 1
    Next
End Sub

Sub TurnOFF(Col) 'turn off all the lights in a collection
    Dim i
    For each i in Col
        i.State = 0
    Next
End Sub

Sub LightEffect(n)
    LightSeqInserts.StopPlay
    Select Case n
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 40
            LightSeqInserts.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqInserts.UpdateInterval = 25
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 20
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 4 'center - used in the bonus count
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqCircleOutOn, 15, 1
        Case 5 'top down
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 15, 2
        Case 6 'down to top
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 15, 3
    End Select
End Sub

'**************
'    TILT
'**************

Sub CheckTilt
    Tilt = Tilt + TiltSensitivity
    TiltDecreaseTimer.Enabled = True
    If Tilt> 15 Then
        Tilted = True
        TurnON aTilt
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
    Else
        GiOn
        Bumper001.Threshold = 1
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' all the balls have drained
    If(BallsOnPlayfield = 0) Then
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' otherwise repeat
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions 2026
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
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.2, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a collision sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

Const Pi=3.1415926535

'***********************************************
'      JP's VPX8 Ball Rolling Sounds 2026
'***********************************************

' physics 2026

Dim maxvel
maxvel = 100 'initial value

Sub Maxvelswitch_Hit
    maxvel = 28 'max ball velocity during game play. EM: 20-35  SS: 35-50
End Sub

Sub maxvelswitch2_Hit
    maxvel = 100 'new ball
End Sub

' rolling routines

Const tnob = 19   'total number of balls, use a high number so you may test the table without errors.
Const lob = 2    'number of locked balls
ReDim rolling(tnob)

InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
    RollingTimer.Enabled = 1
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b, ballpitch, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)
        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <0 Then
                ballpitch = Pitch(BOT(b) ) - 5000 'decrease the pitch under the playfield
            ElseIf BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 30000 'increase the pitch on a ramp or platform
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' dropping sounds
        If BOT(b).VelZ <-1 Then
            'from ramp
            If BOT(b).z <55 and BOT(b).z> 27 Then PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
            'down a hole
            If BOT(b).z <10 and BOT(b).z> -10 Then PlaySound "fx_hole_enter", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        End If

        ' jps ball spin control
        BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95

        ' jps ball speed control 2
        If ABS(BallVel(BOT(b) ) )> maxvel Then
            If BOT(b).VelX <> 0 Then
                speedfactorx = ABS(maxvel / BOT(b).VelX)
                If speedfactorx <1 Then
                    BOT(b).VelX = BOT(b).VelX * speedfactorx
                    BOT(b).VelY = BOT(b).VelY * speedfactorx
                End If
            End If
            If BOT(b).VelY <> 0 Then
                speedfactory = ABS(maxvel / BOT(b).VelY)
                If speedfactory <1 Then
                    BOT(b).VelX = BOT(b).VelX * speedfactory
                    BOT(b).VelY = BOT(b).VelY * speedfactory
                End If
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
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
' Only for VPX 10.8 and higher.
' FlashForMs will blink light for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Blink, -1 Return to original state
'
' To blink a flasher you need to link it to a light, this will fade the flasher just like the light
'************************************************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState)
    If FinalState = -1 Then
        FinalState = MyLight.State
    End If
    MyLight.BlinkInterval = BlinkPeriod
    MyLight.Duration 2, TotalPeriod, FinalState
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
        Special4Awarded(i) = False
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
    ' PlaySound "gameStart"
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
    PlayRandomSong
End Sub

' Crete new ball

Sub CreateNewBall()
    'debug.print "CreateNewBall"
    ' BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass
    BallsOnPlayfield = BallsOnPlayfield + 1
    UpdateBallInPlay
End Sub

' player lost the ball

Sub EndOfBall()

    ' Lost the first ball, now it cannot accept more players
    bOnTheFirstBall = False

    vpmtimer.addtimer 200, "EndOfBall2 '"
End Sub

Sub EndOfBall2()
    'debug.print "EndOfBall2"

    Tilted = False
    Tilt = 0
    TurnOFF aTilt
    If B2SOn then
        Controller.B2SSetTilt 0
    end if
    DisableTable False

    ' win extra ball?
    If(ExtraBallsAwards(CurrentPlayer)> 0) Then
        'debug.print "Extra Ball"

        ' if so then give it
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' turn off light if no more extra balls
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            'LightShootAgain.State = 0
            If B2SOn then
                Controller.B2SSetShootAgain 0
            end if
        End If

        ' extra ball sound?

        ' reset as in a new ball
        ResetForNewPlayerBall()
        CreateNewBall()
    Else ' no extra ball

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' last ball?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
            CheckHighScore()
        End If

        ' this is not the last ball, check for new player
        EndOfBallComplete()
    End If
End Sub

Sub EndOfBallComplete()
    'debug.print "EndOfBallComplete"
    Dim NextPlayer

    ' other players?
    If(PlayersPlayingGame> 1) Then
        NextPlayer = CurrentPlayer + 1
        ' if it is the last player then go to the first one
        If(NextPlayer> PlayersPlayingGame) Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' end of game?
    If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then

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
    DisableTable True
    bGameInPLay = False
    bJustStarted = False
    TurnON aGameOver
    If B2SOn then
        Controller.B2SSetGameOver 1
    end if
    ' turn off flippers
    SolLFlipper 0
    SolRFlipper 0
    DOF 250, DOFPulse
    StopSound Song
    PlaySound "m_GameOver"
    ' start the attract mode
    vpmTimer.AddTimer 3000, "StartAttractMode '"
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

'******************
'     Match
'******************

Sub Verification_Match()
    PlaySound "fx_match"
    Match = INT(RND(1) * 10) * 10 ' random between 0 and 90
    Display_Match
    If(Score(CurrentPlayer) MOD 100) = Match Then
        PlaySound SoundFXDOF("fx_knocker", 122, DOFPulse, DOFknocker)
        AddCredits 1
    End If
End Sub

Sub Clear_Match()
    Match = 0
    Display_Match
    If B2SOn then
        Controller.B2SSetScorePlayer3 0
    end if
End Sub

Sub Display_Match()
    BallsReel.SetValue Match
    If B2SOn then
        Controller.B2SSetScorePlayer3 Match
    end if
End Sub

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

Sub Drain_Hit()
    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball
    BallsOnPlayfield = BallsOnPlayfield - 1
    PlaySoundAt "fx_sensor", Drain
    FlashForMs RedInsert002, 50, 50, 0
    DOF 124, 2
    'tilted?
    If Tilted Then
        StopEndOfBallMode
    End If
    ' if still playing and not tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' ballsaver?
        If(bBallSaverActive = True) Then
        ' CreateNewBall()
        Else
            ' last ball?
            If(BallsOnPlayfield = 0) Then
                StopEndOfBallMode
                vpmtimer.addtimer 100, "EndOfBall '"
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
    'PlayRandomSong
End Sub

' ****************************************
'             Score functions
' ****************************************

Sub AddScore(Points)
    If bGameInPLay = False Then Exit Sub 
    If Tilted Then Exit Sub
    Score(CurrentPlayer) = Score(CurrentPlayer) + Points
    PlaySound "tone" &Points
    UpdateScore

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

'**********************************
'        Score SS reels
'**********************************

Sub UpdateScore
    ScoreReel1.SetValue Score(CurrentPlayer)
    If B2SOn then
        Controller.B2SSetScorePlayer CurrentPlayer, Score(CurrentPlayer)
    end if
End Sub

Sub ResetScores
    ScoreReel1.SetValue 0
    If B2SOn then
        Controller.B2SSetScorePlayer1 0
    end if
End Sub

Sub AddCredits(value) 'limit to 9 credits
    If Credits <9 Then
        Credits = Credits + value
        UpdateCredits
        DOF 200, DOFOn
    end if
End Sub

Sub UpdateCredits
    CreditsReel.SetValue Credits
    If B2SOn then
        Controller.B2SSetCredits Credits
    end if
End Sub

Sub UpdateBallInPlay 'update backdrop lights
    'Ball in play
    BallsReel.SetValue BallsRemaining(CurrentPlayer)
    If B2SOn then
        Controller.B2SSetScorePlayer3 BallsRemaining(CurrentPlayer)
    end if
End Sub

'*************************
'        Specials
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        PlaySound SoundFXDOF("fx_knocker", 122, DOFPulse, DOFknocker)
        DOF 230, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        'LightShootAgain.State = 1
        If B2SOn then
            Controller.B2SSetShootAgain 1
        end if
    Else
        Addscore 5000
    END If
End Sub

Sub AwardSpecial()
    PlaySound SoundFXDOF("fx_knocker", 122, DOFPulse, DOFknocker)
    DOF 230, DOFPulse
    DOF 200, DOFOn
    AddCredits 1
End Sub

Sub AwardAddaBall()
    If BallsRemaining(CurrentPlayer) <11 Then
        PlaySound SoundFXDOF("fx_knocker", 122, DOFPulse, DOFknocker)
        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) + 1
        UpdateBallInPlay
    End If
End Sub

' ********************************
'        Attract Mode
' ********************************
' use the"Blink Pattern" of each light

Sub StartAttractMode()
    If bGameInPlay Then Exit Sub
    Dim x
    bAttractMode = True
    LightSeqAttract.UpdateInterval = 500
    LightSeqAttract.Play SeqRandom, 40, , 6000
    'turn on GameOver light
    TurnON aGameOver
'update current player and balls
End Sub

Sub LightSeqAttract_PlayDone()
    LightSeqAttract.Play SeqRandom, 40, , 6000
End Sub

Sub StopAttractMode()
    Dim x
    bAttractMode = False
    LightSeqAttract.StopPlay
    TurnOffPlayfieldLights
    ResetScores
    TurnOFF aGameOver
End Sub

'*************************************
'      Highscore Routines 2026
' Based og Black's and GNANCE's code
'*************************************

Dim EnteringInitials
EnteringInitials = False

Dim SelectedChar   ' character under the "cursor" when entering initials
Dim HSTimerCount:HSTimerCount = 0
Dim InitialString  ' the string holding the player's initials as they're entered
Dim AlphaString:AlphaString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_<"
Dim AlphaStringPos ' pointer to AlphaString, move Forward and backward with flipper keys
Dim HSNewHigh      ' The new score to be recorded
Dim HSScore(5)     ' High Scores read in from config file
Dim HSName(5)      ' High Score Initials read in from config file
Dim HS 'array with the flashers
HS = Array(HSpostit,HS1,HS2,HS3,HS4,HS5,HS6,HS7,HS8,HS9,HS10,HS11,HS12,HS13,HS14,HS15,HS16,HS17,HS18,HS19,HS20,HS21,HS22,HS23,HS24,HS25,HS26,HS27,HS28,HS29,HS30,HS31)

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "") Then HSScore(1) = CDbl(x) Else HSScore(1) = 50000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "") Then HSName(1) = x Else HSName(1) = "AAA" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "") then HSScore(2) = CDbl(x) Else HSScore(2) = 45000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "") then HSName(2) = x Else HSName(2) = "BBB" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "") then HSScore(3) = CDbl(x) Else HSScore(3) = 40000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "") then HSName(3) = x Else HSName(3) = "CCC" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "") then HSScore(4) = CDbl(x) Else HSScore(4) = 35000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "") then HSName(4) = x Else HSName(4) = "DDD" End If
    x = LoadValue(TableName, "HighScore5")
    If(x <> "") then HSScore(5) = CDbl(x) Else HSScore(5) = 30000 End If
    x = LoadValue(TableName, "HighScore5Name")
    If(x <> "") then HSName(5) = x Else HSName(5) = "EEE" End If
    x = LoadValue(TableName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0
End Sub

Sub Savehs
    SaveValue TableName, "HighScore1", HSScore(1)
    SaveValue TableName, "HighScore1Name", HSName(1)
    SaveValue TableName, "HighScore2", HSScore(2)
    SaveValue TableName, "HighScore2Name", HSName(2)
    SaveValue TableName, "HighScore3", HSScore(3)
    SaveValue TableName, "HighScore3Name", HSName(3)
    SaveValue TableName, "HighScore4", HSScore(4)
    SaveValue TableName, "HighScore4Name", HSName(4)
    SaveValue TableName, "HighScore5", HSScore(5)
    SaveValue TableName, "HighScore5Name", HSName(5)
    SaveValue TableName, "Credits", Credits
End Sub

Sub Reseths
    HSName(1) = "AAA"
    HSName(2) = "BBB"
    HSName(3) = "CCC"
    HSName(4) = "DDD"
    HSName(5) = "EEE"
    HSScore(1) = 50000
    HSScore(2) = 45000
    HSScore(3) = 40000
    HSScore(4) = 35000
    HSScore(5) = 30000
    Savehs
End Sub

' Sort Highscores

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 1 to 5
        For j = 1 to 4
            If HSScore(j) < HSScore(j + 1) Then
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

' check for new highscore

Sub CheckHighscore
    HighScoreTimer.Enabled = 1
    'If Score(CurrentPlayer)> HSSCore(1)Then
    '   AwardSpecial
    'End If
    If Score(CurrentPlayer) > HSSCore(5) Then
        NewHighScore Score(CurrentPlayer), CurrentPlayer
    End If
End Sub

' Highscore Timer

Sub HighScoreTimer_Timer
    If EnteringInitials then
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
    ElseIf bGameInPlay then
        SetHSLine 1, "HIGH SCORE"
        SetHSLine 2, HSScore(1)
        SetHSLine 3, HSName(1)
        Me.Enabled = 0
    Else
        ' cycle through high scores
        SetHSLine 1, "HIGH SCORE" & (HSTimerCount + 1)
        SetHSLine 2, HSScore(HSTimerCount + 1)
        SetHSLine 3, HSName(HSTimerCount + 1)
        HSTimerCount = (HSTimerCount + 1) MOD 5
    End If
End Sub

' Enter Highscore initials

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
    Dim StrLen
    Dim Index
    Dim StartHSArray
    Dim EndHSArray
    Dim xFor
    StartHSArray = array(0, 1, 12, 22)
    EndHSArray = array(0, 11, 21, 31)
    StrLen = len(string)
    Index = 1
    For xFor = StartHSArray(LineNo) to EndHSArray(LineNo)
        HS(xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub NewHighScore(NewScore, PlayNum)
    If NewScore > HSScore(5) then
        HighScoreTimer.interval = 500
        AlphaStringPos = 1      ' start with first character "A"
        EnteringInitials = true ' intercept the control keys while entering initials
        InitialString = ""      ' initials entered so far, initialize to empty
        SetHSLine 1, "PLAYER " + FormatNumber(PlayNum, 0)
        SetHSLine 2, "ENTER NAME"
        SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
        HSNewHigh = NewScore
    End If
End Sub

Sub CollectInitials(keycode)
    Dim i
    If keycode = LeftFlipperKey Then
        ' back up to previous character
        AlphaStringPos = AlphaStringPos - 1
        If AlphaStringPos < 1 then
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
        If AlphaStringPos > len(AlphaString) or(AlphaStringPos = len(AlphaString) and InitialString = "") then
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
            InitialString = MID(InitialString, 1, len(InitialString) - 1)
            If len(InitialString) = 0 then
                ' If there are no more characters to back over, don't leave the < displayed
                AlphaStringPos = 1
            End If
            PlaySound("fx_Esc")
        Else
            InitialString = InitialString & SelectedChar
            PlaySound("fx_Enter")
        End If
        If len(InitialString) < 3 then
            SetHSLine 3, InitialString & SelectedChar
        End If
    End If
    If len(InitialString) = 3 then
        EnteringInitials = False
        HSScore(5) = HSNewHigh
        HSName(5) = InitialString
        SortHighScore
        HighScoreTimer.interval = 2000
    End If
End Sub

'***********************************************************************
' *********************************************************************
'  *********     G A M E  C O D E  S T A R T S  H E R E      *********
' *********************************************************************
'***********************************************************************

Sub VPObjects_Init 'init objects
    TurnOffPlayfieldLights()
    vpmTimer.AddTimer 2000, "BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass: BallRelease.Kick 180, 0 '"
    DisableTable True
End Sub

' Dim all the variables
Dim Mode     '3 sequences in the Game
Dim ModesWon 'completed sequences
Dim base1
Dim base2
Dim base3
Dim triple1
Dim triple2
Dim triple3

Sub Game_Init 'called at the start of a new game
    'Start music?
    'Init variables?
    TurnOff aYouWin
    If B2SOn then
        Controller.B2SSetData 50,0
    end if
    Mode = 1
    ModesWon = 0
    inningsReel.SetValue ModesWon
    If B2SOn then
        Controller.B2SSetScorePlayer2 ModesWon
    end if
    'Start or init timers
    'Init lights?
    TurnOffPlayfieldLights
    ResetModes
    DisableTable False
End Sub

Sub StopEndOfBallMode     'called when the last ball is drained
    PlaySound "m_Ball_Drain"
End Sub

Sub ResetNewBallVariables 'init variables & lights new ball/player
    'TurnOffPlayfieldLights
    'PlaySound "ReelInitt"
    ' turn on new ball Lights
    UpdateModeLights
    UpdateSkillshotLights
End Sub

Sub TurnOffPlayfieldLights
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

'***********
' Bumpers
'***********

Sub Bumper001_Hit 'left bumper
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper001
    AddScore 100
End Sub

' 10 point rebounds

Sub rlband005_Hit:AddScore 10:End Sub
Sub rlband007_Hit:AddScore 10:End Sub
Sub rlband004_Hit:AddScore 10:End Sub
Sub rlband001_Hit:AddScore 10:End Sub

' Top Lanes - skillshot

Sub sw006_Hit '1st
    PlaySoundAt "fx_sensor", sw006
    If Tilted Then Exit Sub
    Select Case Mode
        Case 1
            If Insert003.State = 2 Then
                LightEffect 3
                base1 = 1
                AddScore 500
                CheckMode
            Else
                AddScore 100
            End If
        Case 2
            If Insert003.State = 2 Then
                LightEffect 3
                triple1 = 1
                AddScore 500
                CheckMode
            Else
                AddScore 100
            End If
        Case 3:AddScore 100
    End Select
    Insert001.State = 0
    Insert002.State = 0
    Insert003.State = 0
End Sub

Sub sw005_Hit '2nd
    PlaySoundAt "fx_sensor", sw005
    If Tilted Then Exit Sub
    Select Case Mode
        Case 1
            If Insert002.State = 2 Then
                LightEffect 3
                base2 = 1
                AddScore 500
                CheckMode
            Else
                AddScore 100
            End If
        Case 2
            If Insert002.State = 2 Then
                LightEffect 3
                triple2 = 1
                AddScore 500
                CheckMode
            Else
                AddScore 100
            End If
        Case 3
            If Insert002.State = 2 Then
                LightEffect 3
                AddScore 5000
                CheckMode
            Else
                AddScore 100
            End If
    End Select
    Insert001.State = 0
    Insert002.State = 0
    Insert003.State = 0
End Sub

Sub sw004_Hit '3rd
    PlaySoundAt "fx_sensor", sw004
    If Tilted Then Exit Sub
    Select Case Mode
        Case 1
            If Insert001.State = 2 Then
                LightEffect 3
                base3 = 1
                AddScore 500
                CheckMode
            Else
                AddScore 100
            End If
        Case 2
            If Insert001.State = 2 Then
                LightEffect 3
                triple3 = 1
                AddScore 500
                CheckMode
            Else
                AddScore 100
            End If
        Case 3:AddScore 100
    End Select
    Insert001.State = 0
    Insert002.State = 0
    Insert003.State = 0
End Sub

' Outlanes

Sub sw001_Hit 'left
    PlaySoundAt "fx_sensor", sw001
    If Tilted Then Exit Sub
    AddScore 500
    If Insert8.State = 0 Then
        Insert8.State = 1
        CheckOutlanes
    End If
End Sub

Sub sw003_Hit 'right
    PlaySoundAt "fx_sensor", sw003
    If Tilted Then Exit Sub
    AddScore 500
    If Insert10.State = 0 Then
        Insert10.State = 1
        CheckOutlanes
    End If
End Sub

' Inlanes

Sub sw002_Hit 'left
    PlaySoundAt "fx_sensor", sw002
    If Tilted Then Exit Sub
    AddScore 100
    If Insert9.State = 0 Then
        Insert9.State = 1
        CheckOutlanes
    End If
End Sub

Sub CheckOutlanes
    If Insert8.State + Insert9.State + Insert10.State = 3 Then 'spot one of the targets
        Insert8.State = 0
        Insert9.State = 0
        Insert10.State = 0
        If base1 = 2 Then
            LightEffect 3:base1 = 1:CheckMode:Addscore 500
        Elseif base2 = 2 Then
            LightEffect 3:base2 = 1:CheckMode:Addscore 500
        Elseif base3 = 2 Then
            LightEffect 3:base3 = 1:CheckMode:Addscore 500
        Elseif triple1 = 2 Then
            LightEffect 3:triple1 = 1:CheckMode:Addscore 500
        Elseif triple2 = 2 Then
            LightEffect 3:triple2 = 1:CheckMode:Addscore 500
        Elseif triple3 = 2 Then
            LightEffect 3:triple3 = 1:CheckMode:Addscore 500
        End if
    End If
End Sub

Sub RotateLaneLights(dir)
Dim tmp
Select case dir
case 0 'rotate to the left
tmp = Insert8.State
Insert8.State = Insert9.State
Insert9.State = Insert10.State
Insert10.State = tmp
Case 1 'rotate to the RightFlipper
tmp = Insert10.State
Insert10.State = Insert9.State
Insert9.State = Insert8.State
Insert8.State = tmp
End Select
End Sub
' Right Trigger

Sub Trigger011_Hit
    PlaySoundAt "fx_sensor", Trigger011
    If Tilted Then Exit Sub
    Addscore 100
End Sub

' orbit triggers

Sub Trigger006_Hit
    PlaySoundAt "fx_sensor", Trigger006
    If Tilted Then Exit Sub
    FlashForMs RedInsert006, 50, 50, 0
    Insert001.State = 0
    Insert002.State = 0
    Insert003.State = 0
    CheckMode
End Sub

Sub Trigger005_Hit
    PlaySoundAt "fx_sensor", Trigger005
    If Tilted Then Exit Sub
    FlashForMs RedInsert005, 50, 50, 0
End Sub

Sub Trigger002_Hit
    PlaySoundAt "fx_sensor", Trigger002
    If Tilted Then Exit Sub
    FlashForMs RedInsert004, 50, 50, 0
End Sub

Sub drain2_Hit
    PlaySoundAt "fx_sensor", drain2
    FlashForMs RedInsert001, 50, 50, 0
End Sub

' spinner

Sub Spinner001_Spin
    PlaySoundAt "fx_spinner", Spinner001
    If Tilted Then Exit Sub
    Addscore 50
End Sub

' Base Targets

Sub target001_Hit 'left - 3rd base
    PlaySoundAtBall SoundFXDOF("fx_target", 105, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    If mode = 1 AND base3 = 2 Then
        LightEffect 3
        base3 = 1
        CheckMode
        Addscore 500
    Else
        Addscore 100
    End If
End Sub

Sub target007_Hit 'center - 2nd base
    PlaySoundAtBall SoundFXDOF("fx_target", 106, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    If mode = 1 AND base2 = 2 Then
        LightEffect 3
        base2 = 1
        CheckMode
        Addscore 500
    Else
        Addscore 100
    End If
End Sub

Sub target002_Hit 'right - 1st base
    PlaySoundAtBall SoundFXDOF("fx_target", 107, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    If mode = 1 AND base1 = 2 Then
        LightEffect 3
        base1 = 1
        CheckMode
        Addscore 500
    Else
        Addscore 100
    End If
End Sub

' Grand Slam

Sub target003_Hit 'captive ball
    PlaySoundAtBall SoundFXDOF("fx_target", 115, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    If mode = 3 Then
        Addscore 5000
        CheckMode
    Else
        Addscore 100
    End If
End Sub

' triple Targets

Sub target004_Hit 'triple target top
    PlaySoundAtBall SoundFXDOF("fx_target", 111, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    If mode = 2 AND triple1 = 2 Then
        LightEffect 3
        triple1 = 1
        CheckMode
        Addscore 500
    Else
        Addscore 100
    End If
End Sub

Sub target005_Hit 'triple target center
    PlaySoundAtBall SoundFXDOF("fx_target", 112, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    If mode = 2 AND triple2 = 2 Then
        LightEffect 3
        triple2 = 1
        CheckMode
        Addscore 500
    Else
        Addscore 100
    End If
End Sub

Sub target006_Hit 'triple target lower
    PlaySoundAtBall SoundFXDOF("fx_target", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    If mode = 2 AND triple3 = 2 Then
        LightEffect 3
        triple3 = 1
        CheckMode
        Addscore 500
    Else
        Addscore 100
    End If
End Sub


'*******************
' MODES - Sequences
'*******************
'2 not started = light blinkng
'1 completed light on
'0 not active

Sub ResetModes
    base1 = 2 '2 not started = light blinkng
    base2 = 2 '1 completed light on
    base3 = 2 '0 not active
    triple1 = 2
    triple2 = 2
    triple3 = 2
'    UpdateModeLights
End Sub

Sub UpdateSkillshotLights
    Dim tmp1, tmp2, tmp3
    Select Case Mode
        Case 1 '3 bases
            If base1 = 1 Then tmp1 = 0 Else tmp1 = 2 End If
            If base2 = 1 Then tmp2 = 0 Else tmp2 = 2 End If
            If base3 = 1 Then tmp3 = 0 Else tmp3 = 2 End If
            Insert003.State = tmp1:Insert002.State = tmp2:Insert001.State = tmp3
        Case 2 'triple play
            If triple1 = 1 Then tmp1 = 0 Else tmp1 = 2 End If
            If triple2 = 1 Then tmp2 = 0 Else tmp2 = 2 End If
            If triple3 = 1 Then tmp3 = 0 Else tmp3 = 2 End If
            Insert003.State = tmp1:Insert002.State = tmp2:Insert001.State = tmp3
        Case 3:Insert002.State = 2
    End Select
End Sub

Sub UpdateModeLights
    TurnOffPlayfieldLights
    Select Case Mode
        Case 1:Insert1stBase.State = base1:Insert2ndBase.State = base2:Insert3rdBase.State = base3 '3 bases
        Case 2:Insert4.State = triple1:Insert5.State = triple2:Insert6.State = triple3             'triple play
        Case 3:Insert7.State = 2
    End Select
End Sub

Sub CheckMode
    Dim tmp
    Select Case Mode
        Case 1
            tmp = base1 + base2 + base3
            If tmp = 3 Then 'all bases are completed . move to the next sequence
                Mode = 2
            End If
        Case 2
            tmp = triple1 + triple2 + triple3
            If tmp = 3 Then ' triple targets are completed - move to the next sequence
                Mode = 3
            End If
        Case 3 'the grand Slam has been hit
            'count the completed modes
            ModesWon = ModesWon + 1
            AwardAddaBall
            inningsReel.SetValue ModesWon
            If B2SOn then
                Controller.B2SSetScorePlayer2 ModesWon
            end if
            'lightshow
            LightEffect 2
            If modeswon = 9 Then
                'win the Game
                vpmTimer.AddTimer 3000, "WinGame '"
            Else
                ResetModes
                mode = 1
            'reset Modes
            End If
    End Select
    UpdateModeLights
End Sub

Sub WinGame
    TurnOn aYouWin
    If B2SOn then
        Controller.B2SSetData 50,1
    end if
    inningsReel.SetValue ModesWon
    If B2SOn then
        Controller.B2SSetScorePlayer2 ModesWon
    end if
    AwardSpecial
    DisableTable 1
    BallsRemaining(CurrentPlayer) = 0
End Sub

'*****************************************
'       Music as internal sounds
'*****************************************

Dim Song, SongVolume, bMusicOn
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

Sub PlayRandomSong
    StopSound Song
    Song = ""
    PlaySong "m_Song"&RndNbr(10)
End Sub


'*********************************
' Table Options F12 User Options
'*********************************
' Table1.Option arguments are:
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional array of literal strings

Dim LUTImage, BallsPerGame
Dim dspTriggered : dspTriggered = False
LUTImage = Array("LUT0", "LUT1", "LUT2", "LUT3", "LUT4", "LUT5", "LUT6", "LUT7", "LUT8", "LUT9", "LUT10", _
        "LUT Warm 0", "LUT Warm 1", "LUT Warm 2", "LUT Warm 3", "LUT Warm 4", "LUT Warm 5", "LUT Warm 6", "LUT Warm 7", "LUT Warm 8", "LUT Warm 9", "LUT Warm 10", _
        "LUT Warmer 0", "LUT Warmer 1", "LUT Warmer 2", "LUT Warmer 3", "LUT Warmer 4", "LUT Warmer 5", "LUT Warmer 6", "LUT Warmer 7", "LUT Warmer 8", "LUT Warmer 9", "LUT Warmer 10")

Sub Table1_OptionEvent(ByVal eventId)
    If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If

    Dim x, y

    'LUT
    x = Table1.Option("Select LUT", 0, 32, 1, 0, 0, Array("Normal 0", "Normal 1", "Normal 2", "Normal 3", "Normal 4", "Normal 5", "Normal 6", "Normal 7", "Normal 8", "Normal 9", "Normal 10", _
        "Warm 0", "Warm 1", "Warm 2", "Warm 3", "Warm 4", "Warm 5", "Warm 6", "Warm 7", "Warm 8", "Warm 9", "Warm 10", _
        "Warmer 0", "Warmer 1", "Warmer 2", "Warmer 3", "Warmer 4", "Warmer 5", "Warmer 6", "Warmer 7", "Warmer 8", "Warmer 9", "Warmer 10") )
    table1.ColorGradeImage =  LUTImage(x)

    ' Cabinet rails
    x = Table1.Option("Cabinet Rails", 0, 1, 1, 1, 0, Array("Hide", "Show") )
    For each y in aRails:y.visible = x:next

    ' Balls per Game
    x = Table1.Option("Balls per Game", 0, 1, 1, 0, 0, Array("3 Balls", "5 Balls") )
    If x = 1 Then BallsPerGame = 5 Else BallsPerGame = 3
    If BallsPerGame = 5 Then
        Special1 = 330000
        Special2 = 470000
        Special3 = 630000
    Else
        Special1 = 240000
        Special2 = 380000
        Special3 = 540000
    End if

    ' Music  On/Off
    x = Table1.Option("Music", 0, 1, 1, 1, 0, Array("OFF", "ON") )
    If x Then bMusicOn = True Else bMusicOn = False

    ' Music Volume
    SongVolume = Table1.Option("Music Volume", 0, 1, 0.1, 0.3, 0)
    If bMusicOn AND bGameInPlay Then
        PlaySong Song
    Else
        StopSound Song
    End If

    ' Table's Slope
    x = Table1.Option("Table's Slope", 0, 10, 0.1, 5, 1)
    Table1.SlopeMin = x
    Table1.SlopeMax = x

    If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub
