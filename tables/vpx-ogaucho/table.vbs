' O Gaucho by LTD do Brasil from 1975
' vpx8 table by jpsalas & Halen v 6.0.1
' based on the FP table by Exegeta & RoAtaguile

'DOF Some Solenoid Config by Outhere
'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 Right Slingshot
'105 
'106 
'107 Bumper Left
'108 Bumper Right
'109 Bumper Center
'110 
'111 
'112 
'113 
'114 
'115 
'116 
'117 
'118 
'119 
'120 BallRelease
'121 
'122 Knocker
'123 
'124 
'125 
'126 
'128 
'129 
'130 
'131 Chime
'132 Chime


Option Explicit
Randomize

' core.vbs constants
Const BallSize = 50 ' 50 is the normal size
Const BallMass = 1  ' 1 is the normal ball mass.

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
Const TableName = "ogaucho" ' file name to save highscores and other variables
Const cGameName = "ogaucho" ' B2S name
Const MaxPlayers = 2        ' 1 to 4 can play
Const MaxMultiplier = 2     ' limit bonus multiplier
Const FreePlay = True      ' Free play or coins
Const Special1 = 79000      ' extra ball or credit
Const Special2 = 120000
' Const Special3 = 190000

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

' *********************************************************************
'                Common rutines to all the tables
' *********************************************************************

Sub Table1_Init()
    Dim x

    ' Init som objects, like walls, targets
    VPObjects_Init
    LoadEM

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
    If credits > 0 then:DOF 123, DOFOn
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    If EnteringInitials then
        CollectInitials(keycode)
        Exit Sub
    End If

    ' Reset High Score using the [7] key
    if keycode = "8" then 
        ResetHS
        ResHS.Visible = 1
        vpmTimer.AddTimer 2000, "Reshs.visible = 0 '"
    End If

    ' add coins
    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
        If(Tilted = False)Then
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
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                'PlayersReel.SetValue, PlayersPlayingGame
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        Credits = Credits - 1
                        UpdateCredits
                        UpdateBallInPlay
                    Else
                        ' Not Enough Credits to start a game.
                        DOF 123, DOFOff
                    'PlaySound "so_nocredits"
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
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
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
'Controller.Stop
End Sub

'********************
'     Flippers
'********************

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

'**************
'    TILT
'**************

Sub CheckTilt
    Tilt = Tilt + TiltSensitivity
    TiltDecreaseTimer.Enabled = True
    If Tilt > 15 Then
        Tilted = True
        TiltReel.SetValue 1
        If B2SOn then
            Controller.B2SSetTilt 1
        end if
        DisableTable True
        ' BallsRemaining(CurrentPlayer) = 0 'player looses the game 'mostly on older 1 player games
        TiltRecoveryTimer.Enabled = True 'wait for all the balls to drain
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    If Tilt > 0 Then
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
        Bumper002.Threshold = 100
        Bumper003.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        GiOn
        Bumper001.Threshold = 1
        Bumper002.Threshold = 1
        Bumper003.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
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
    maxvel = 32 'max ball velocity during game play. EM: 20-35  SS: 35-50
End Sub

Sub maxvelswitch2_Hit
    maxvel = 100 'new ball
End Sub

' rolling routines

Const tnob = 19   'total number of balls, use a high number so you may test the table without errors.
Const lob = 0    'number of locked balls
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
    Bonus = 1
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
    PlaySoundAt SoundFXDOF("fx_Ballrel", 120, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4
End Sub

' player lost the ball

Sub EndOfBall()
    'debug.print "EndOfBall"

    ' Lost the first ball, now it cannot accept more players
    bOnTheFirstBall = False

    'No bonus in this table

    ' add bonus if no tilt
    ' tilt system will take care of the next ball

    If NOT Tilted Then
        BonusCountTimer.Interval = 250 + 100 * BonusMultiplier
        BonusCountTimer.Enabled = 1
    Else
        vpmtimer.addtimer 400, "EndOfBall2 '"
    End If
End Sub

Sub BonusCountTimer_Timer
    'debug.print "BonusCount_Timer"
    If TotalBonus > 0 Then
        TotalBonus = TotalBonus -1
        AddScore 1000 * BonusMultiplier
        DigitsUpdate
    Else
        BonusCountTimer.Enabled = 0
        vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub

Sub UpdateBonusLights
    Select Case Bonus
        Case 0:bl1.State = 1:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 1:bl1.State = 0:bl2.State = 1:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 2:bl1.State = 0:bl2.State = 0:bl3.State = 1:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 3:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 1:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 4:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 1:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 5:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 1:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 6:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 1:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 7:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 1:bl9.State = 0:bl10.State = 0
        Case 8:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 1:bl10.State = 0
        Case 9:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 1
    End Select
End Sub

' After bonus count go to the next step
'
Sub EndOfBall2()
    'debug.print "EndOfBall2"

    Tilted = False
    Tilt = 0
    TiltReel.SetValue 0
    If B2SOn then
        Controller.B2SSetTilt 0
    end if
    DisableTable False

    ' win extra ball?
    If(ExtraBallsAwards(CurrentPlayer) > 0)Then
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
    If(PlayersPlayingGame > 1)Then
        NextPlayer = CurrentPlayer + 1
        ' if it is the last player then go to the first one
        If(NextPlayer > PlayersPlayingGame)Then
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
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

'******************
'     Match
'******************

Sub Verification_Match()
    Dim x
    PlaySound "fx_match"
    Match = INT(RND(1) * 10) * 10 ' random between 0 and 90
    Display_Match
    For x = 1 to PlayersPlayingGame
        If(Score(x)MOD 100) = Match Then
            PlaySound SoundFXDOF("fx_knocker", 122, DOFPulse, DOFknocker)
            AddCredits 1
        End If
    Next
End Sub

Sub Clear_Match()
    Match = 0
    MatchReel.SetValue Match
    If B2SOn then
        Controller.B2SSetMatch Match
    end if
End Sub

Sub Display_Match()
    'Update_Match
    MatchReel.SetValue Match
    If B2SOn then
        If Match = 0 then
            Controller.B2SSetMatch 100
        else
            Controller.B2SSetMatch Match
        end if
    end if
End Sub

Sub Update_Match
'    Select Case Match \ 10
'        Case 0:m0.State = 0:m000.state = 0:m1.State = 0:m001.state = 0:m2.State = 0:m002.state = 0:m3.State = 0:m003.state = 0:m4.State = 0:m004.state = 0:m5.State = 0:m005.state = 0:m6.State = 0:m006.state = 0:m7.State = 0:m007.state = 0:m8.State = 0:m008.state = 0:m9.State = 0:m009.state = 0
'        Case 1:m0.State = 0:m000.state = 0:m1.State = 1:m001.state = 1:m2.State = 0:m002.state = 0:m3.State = 0:m003.state = 0:m4.State = 0:m004.state = 0:m5.State = 0:m005.state = 0:m6.State = 0:m006.state = 0:m7.State = 0:m007.state = 0:m8.State = 0:m008.state = 0:m9.State = 0:m009.state = 0
'        Case 2:m0.State = 0:m000.state = 0:m1.State = 0:m001.state = 0:m2.State = 1:m002.state = 1:m3.State = 0:m003.state = 0:m4.State = 0:m004.state = 0:m5.State = 0:m005.state = 0:m6.State = 0:m006.state = 0:m7.State = 0:m007.state = 0:m8.State = 0:m008.state = 0:m9.State = 0:m009.state = 0
'        Case 3:m0.State = 0:m000.state = 0:m1.State = 0:m001.state = 0:m2.State = 0:m002.state = 0:m3.State = 1:m003.state = 1:m4.State = 0:m004.state = 0:m5.State = 0:m005.state = 0:m6.State = 0:m006.state = 0:m7.State = 0:m007.state = 0:m8.State = 0:m008.state = 0:m9.State = 0:m009.state = 0
'        Case 4:m0.State = 0:m000.state = 0:m1.State = 0:m001.state = 0:m2.State = 0:m002.state = 0:m3.State = 0:m003.state = 0:m4.State = 1:m004.state = 1:m5.State = 0:m005.state = 0:m6.State = 0:m006.state = 0:m7.State = 0:m007.state = 0:m8.State = 0:m008.state = 0:m9.State = 0:m009.state = 0
'        Case 5:m0.State = 0:m000.state = 0:m1.State = 0:m001.state = 0:m2.State = 0:m002.state = 0:m3.State = 0:m003.state = 0:m4.State = 0:m004.state = 0:m5.State = 1:m005.state = 1:m6.State = 0:m006.state = 0:m7.State = 0:m007.state = 0:m8.State = 0:m008.state = 0:m9.State = 0:m009.state = 0
'        Case 6:m0.State = 0:m000.state = 0:m1.State = 0:m001.state = 0:m2.State = 0:m002.state = 0:m3.State = 0:m003.state = 0:m4.State = 0:m004.state = 0:m5.State = 0:m005.state = 0:m6.State = 1:m006.state = 1:m7.State = 0:m007.state = 0:m8.State = 0:m008.state = 0:m9.State = 0:m009.state = 0
'        Case 7:m0.State = 0:m000.state = 0:m1.State = 0:m001.state = 0:m2.State = 0:m002.state = 0:m3.State = 0:m003.state = 0:m4.State = 0:m004.state = 0:m5.State = 0:m005.state = 0:m6.State = 0:m006.state = 0:m7.State = 1:m007.state = 1:m8.State = 0:m008.state = 0:m9.State = 0:m009.state = 0
'        Case 8:m0.State = 0:m000.state = 0:m1.State = 0:m001.state = 0:m2.State = 0:m002.state = 0:m3.State = 0:m003.state = 0:m4.State = 0:m004.state = 0:m5.State = 0:m005.state = 0:m6.State = 0:m006.state = 0:m7.State = 0:m007.state = 0:m8.State = 1:m008.state = 1:m9.State = 0:m009.state = 0
'        Case 9:m0.State = 0:m000.state = 0:m1.State = 0:m001.state = 0:m2.State = 0:m002.state = 0:m3.State = 0:m003.state = 0:m4.State = 0:m004.state = 0:m5.State = 0:m005.state = 0:m6.State = 0:m006.state = 0:m7.State = 0:m007.state = 0:m8.State = 0:m008.state = 0:m9.State = 1:m009.state = 1
'    End Select
End Sub

'*************************
' Update playfield Digits
'*************************

Dim Digits
Digits = Array("d_0", "d_1", "d_2", "d_3", "d_4", "d_5", "d_6", "d_7", "d_8", "d_9")

Sub DigitsUpdate
    Dim tmp
    'target multi
    digit1.ImageA = Digits(TargetMulti)
    'totalbonus
    tmp = TotalBonus \ 10
    digit2.ImageA = Digits(tmp)
    tmp = TotalBonus MOD 10
    digit3.ImageA = Digits(tmp)
End Sub

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

Sub Drain_Hit()
    Drain.DestroyBall
    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball
    BallsOnPlayfield = BallsOnPlayfield - 1
    PlaySoundAt "fx_drain", Drain
    DOF 124, 2
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
'SoundFXDOF("fx_resetdrop", 108, DOFPulse, DOFContactors)
Sub AddScore(Points)
    If Tilted Then Exit Sub
    Select Case Points
        Case 10
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore points
            PlaySound SoundFXDOF("beep10", 131, DOFPulse, DOFChimes)
        Case 100
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore points
            AddBonus 1 'all the 100 points add a bonus advance
            PlaySound SoundFXDOF("beep100", 132, DOFPulse, DOFChimes)
        Case 1000
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore points
            PlaySound SoundFXDOF("beep1000", 132, DOFPulse, DOFChimes)
        Case 20, 30, 40, 50
            Add10 = Add10 + Points \ 10
            AddScore10Timer.Enabled = TRUE
        Case 200, 300, 400, 500
            Add100 = Add100 + Points \ 100
            AddScore100Timer.Enabled = TRUE
        Case 2000, 3000, 4000, 5000
            Add1000 = Add1000 + Points \ 1000
            AddScore1000Timer.Enabled = TRUE
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
'    If Score(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
'        AwardSpecial
'        Special3Awarded(CurrentPlayer) = True
'    End If
End Sub

'************************************
'       Score sound Timers
'************************************

Sub AddScore10Timer_Timer()
    if Add10 > 0 then
        AddScore 10
        Add10 = Add10 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore100Timer_Timer()
    if Add100 > 0 then
        AddScore 100
        Add100 = Add100 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore1000Timer_Timer()
    if Add1000 > 0 then
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
        If Bonus > 9 Then
            Bonus = Bonus MOD 10
            AddTotalBonus 1
        End If
        UpdateBonusLights
    End if
End Sub

Sub AddTotalBonus(bonuspoints)
    If(Tilted = False)Then
        TotalBonus = TotalBonus + bonuspoints
        If TotalBonus > 15 Then
            TotalBonus = 15
        End If
        DigitsUpdate
    End if
End Sub

Sub AddBonusMultiplier(multi)
    If(Tilted = False)Then
        BonusMultiplier = BonusMultiplier + multi
        If BonusMultiplier > MaxMultiplier Then
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

Sub AddTargetMulti(n)
            TargetMulti = (TargetMulti + n)MOD 10
            If TargetMulti = 0 Then TargetMulti = 1
            DigitsUpdate
End Sub


'**********************************
'        Score EM reels
'**********************************

Sub UpdateScore(playerpoints)
    Select Case CurrentPlayer
        Case 1:ScoreReel1.Addvalue playerpoints
        Case 2:ScoreReel2.Addvalue playerpoints
    'Case 3:ScoreReel3.Addvalue playerpoints
    'Case 4:ScoreReel4.Addvalue playerpoints
    End Select
    If B2SOn then
        Controller.B2SSetScorePlayer CurrentPlayer, Score(CurrentPlayer)
    end if
End Sub

Sub ResetScores
    ScoreReel1.SetValue 1:ScoreReel1.ResetToZero
    ScoreReel2.ResetToZero
    'ScoreReel3.ResetToZero
    'ScoreReel4.ResetToZero
    If B2SOn then
        Controller.B2SSetScorePlayer1 0
        Controller.B2SSetScoreRolloverPlayer1 0
        Controller.B2SSetScorePlayer2 0
        Controller.B2SSetScoreRolloverPlayer2 0
    'Controller.B2SSetScorePlayer3 0
    'Controller.B2SSetScoreRolloverPlayer3 0
    'Controller.B2SSetScorePlayer4 0
    'Controller.B2SSetScoreRolloverPlayer4 0
    end if
End Sub

Sub AddCredits(value) 'limit to 15 credits
    If Credits < 15 Then
        Credits = Credits + value
        CreditsReel.SetValue Credits
        UpdateCredits
        DOF 123, DOFOn
    end if
End Sub

Sub UpdateCredits
    PlaySound "fx_relay"
    CreditsReel.SetValue credits
    If Credits > 0 Then
    'CreditsLight.State = 1
    Else
    'CreditsLight.State = 0
    End If
    If B2SOn then
        Controller.B2SSetCredits Credits
    end if
End Sub

Sub UpdateBallInPlay 'update backdrop lights
    'Ball in play
    BallnrReel.SetValue Balls
    If B2SOn then
        Controller.B2SSetBallInPlay Balls
    End If
    ' Update player light
    Select Case CurrentPlayer
        Case 0:pl1.State = 0:pl2.State = 0 ':pl3.State = 0:pl4.State = 0
        Case 1:pl1.State = 1:pl2.State = 0 ':pl3.State = 0:pl4.State = 0
        Case 2:pl1.State = 0:pl2.State = 1 ':pl3.State = 0:pl4.State = 0
    'Case 3:pl1.State = 0:pl2.State = 0:pl3.State = 1:pl4.State = 0
    'Case 4:pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 1
    End Select
    If B2SOn then
        Controller.B2SSetPlayerUp CurrentPlayer
    End If
' Player can play
'Select Case PlayersPlayingGame
'Case 0:cp1.State = 0:cp2.State = 0:cp3.State = 0:cp4.State = 0
'Case 1:cp1.State = 1:cp2.State = 0:cp3.State = 0:cp4.State = 0
'Case 2:cp1.State = 0:cp2.State = 1:cp3.State = 0:cp4.State = 0
'Case 3:cp1.State = 0:cp2.State = 0:cp3.State = 1:cp4.State = 0
'Case 4:cp1.State = 0:cp2.State = 0:cp3.State = 0:cp4.State = 1
'End Select
'If B2SOn then
'    Controller.B2SSetCanPlay PlayersPlayingGame
'end if
End Sub

'*************************
'        Specials
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        PlaySound SoundFXDOF("fx_knocker", 122, DOFPulse, DOFknocker)
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
    Else
    'Addscore 5000
    END If
End Sub

Sub AwardSpecial()
    PlaySound SoundFXDOF("fx_knocker", 122, DOFPulse, DOFknocker)
    AddCredits 1
End Sub

Sub AwardAddaBall()
    If BallsRemaining(CurrentPlayer) < 11 Then
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
    Dim x
    bAttractMode = True
    For each x in aLights
        x.State = 2
    Next
    GameOverReel.SetValue 1
'update current player and balls
End Sub

Sub StopAttractMode()
    Dim x
    bAttractMode = False
    TurnOffPlayfieldLights
    ResetScores
    GameOverReel.SetValue 0
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
    If(x <> "") Then HSScore(1) = CDbl(x) Else HSScore(1) = 20000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "") Then HSName(1) = x Else HSName(1) = "AAA" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "") then HSScore(2) = CDbl(x) Else HSScore(2) = 19000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "") then HSName(2) = x Else HSName(2) = "BBB" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "") then HSScore(3) = CDbl(x) Else HSScore(3) = 18000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "") then HSName(3) = x Else HSName(3) = "CCC" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "") then HSScore(4) = CDbl(x) Else HSScore(4) = 17000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "") then HSName(4) = x Else HSName(4) = "DDD" End If
    x = LoadValue(TableName, "HighScore5")
    If(x <> "") then HSScore(5) = CDbl(x) Else HSScore(5) = 16000 End If
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
    HSScore(1) = 20000
    HSScore(2) = 19000
    HSScore(3) = 18000
    HSScore(4) = 17000
    HSScore(5) = 16000
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
End Sub

' Dim all the variables
Dim t1 'the 3 targets
Dim t2
Dim t3
Dim l1 'the left and right lanes
Dim l2
Dim TotalBonus
Dim TargetMulti

Sub Game_Init() 'called at the start of a new game
    'Start music?
    'Init variables?
    t1 = 0
    t2 = 0
    t3 = 0
    l1 = 0
    l2 = 0
    TargetMulti = 1
    Bonus = 0:UpdateBonusLights
    'Start or init timers
    'Init lights?
    TurnOffPlayfieldLights()
End Sub

Sub StopEndOfBallMode()     'called when the last ball is drained
End Sub

Sub ResetNewBallVariables() 'init variables & lights new ball/player
    TurnOffPlayfieldLights
    BonusMultiplier = 1
    If Balls = 5 Then BonusMultiplier = 2:Light001.State = 1
    TotalBonus = 0:DigitsUpdate
    t1 = 0
    t2 = 0
    t3 = 0
    l1 = 0
    l2 = 0
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

'***********
' Bumpers
'***********

Sub Bumper001_Hit 'left bumper
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), bumper001
    AddScore 100
 AddTargetMulti 1
    Lbumper1a.Duration 1, 100, 0
    Lbumper1b.Duration 1, 100, 0
End Sub

Sub Bumper002_Hit 'center bumper
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), Bumper002
    AddScore 100
 AddTargetMulti 1
    Lbumper2a.Duration 1, 100, 0
    Lbumper2b.Duration 1, 100, 0
End Sub

Sub Bumper003_Hit 'right bumper
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper003
    AddScore 100
 AddTargetMulti 1
    Lbumper3a.Duration 1, 100, 0
    Lbumper3b.Duration 1, 100, 0
End Sub

'***************
'  Slingshots
'***************
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFContactors), Lemk
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    ' add points
    AddScore 10
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing004.Visible = 0:LeftSLing003.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing003.Visible = 0:LeftSLing002.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing002.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 104, DOFPulse, DOFContactors), Remk
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    ' add points
    AddScore 10
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'*********************
'  Targets & switches
'*********************

' Flipper Lanes

Sub Trigger008_Hit
    PlaySoundAt "fx_sensor", Trigger008
    If Tilted Then Exit Sub
    AddScore 500 + 2500 * Light013.State
End Sub

Sub Trigger007_Hit
    PlaySoundAt "fx_sensor", Trigger007
    If Tilted Then Exit Sub
    AddScore 500
    If(Light013.State + Light014.State) = 2 Then
        Light002.State = 1
        AwardExtraBall
    End If
End Sub

Sub Trigger006_Hit
    PlaySoundAt "fx_sensor", Trigger006
    If Tilted Then Exit Sub
    AddScore 500 + 2500 * Light014.State
End Sub

'side lanes
Sub Trigger004_Hit
    PlaySoundAt "fx_sensor", Trigger004
    If Tilted Then Exit Sub
    AddScore 500
    AddScore 500
    l2 = 1
    CheckLanes
End Sub

Sub Trigger005_Hit
    PlaySoundAt "fx_sensor", Trigger005
    If Tilted Then Exit Sub
    AddScore 500
    l1 = 1
    CheckLanes
End Sub

Sub CheckLanes
    Dim tmp
    tmp = l1 + l2
    If tmp = 2 Then
        Light013.State = 1
        l1 = 0
        l2 = 0
    End If
End Sub

'top lanes
Sub Trigger001_Hit
    PlaySoundAt "fx_sensor", Trigger001
    If Tilted Then Exit Sub
    AddScore 500
End Sub

Sub Trigger002_Hit
    PlaySoundAt "fx_sensor", Trigger002
    If Tilted Then Exit Sub
    AddScore 1000
    AddTotalBonus 1
End Sub

Sub Trigger003_Hit
    PlaySoundAt "fx_sensor", Trigger003
    If Tilted Then Exit Sub
    AddScore 500
End Sub

' 10 point rebounds

Sub rlband005_Hit:AddScore 10:End Sub
Sub rsband001_Hit:AddScore 10:End Sub
Sub rlband003_Hit:AddScore 10:End Sub
Sub rlband007_Hit:AddScore 10:End Sub
Sub rsband005_Hit:AddScore 10:End Sub

'spinner

Sub spinner_Spin:Addscore 100:End Sub
Sub GateTrigger_Hit:Addscore 3000:AddTotalBonus 1:End Sub

'Targets

Sub Target001_Hit
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    AddScore 500
    t1 = 1
    CheckTargets
End Sub

Sub Target002_Hit
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    AddScore 1000 * TargetMulti
    AddTotalBonus 1
End Sub

Sub Target003_Hit
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    AddScore 500
    t3 = 1
    CheckTargets
End Sub

Sub Target004_Hit
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    AddScore 500
    t2 = 1
    CheckTargets
End Sub

Sub CheckTargets
    Dim tmp
    tmp = t1 + t2 + t3
    If tmp = 3 Then
        Light014.State = 1
        t1 = 0
        t2 = 0
        t3 = 0
    End If
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
    x = Table1.Option("Balls per Game", 0, 1, 1, 1, 0, Array("3 Balls", "5 Balls") )
    If x = 1 Then BallsPerGame = 5 Else BallsPerGame = 3

    ' Table's Slope
    x = Table1.Option("Table's Slope", 0, 10, 0.1, 5, 1)
    Table1.SlopeMin = x
    Table1.SlopeMax = x

    If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub