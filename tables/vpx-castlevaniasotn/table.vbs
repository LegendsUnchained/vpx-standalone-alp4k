' ****************************************************************
'                      VISUAL PINBALL X
'                JPSalas original DMD Pinball Script
'       plain VPX script using core.vbs for supporting functions
'     and controller.vbs to load B2S server and other controllers
'              Including JP's Arcade Physics 3.0.1
'
' Mephisto / IPD No. 4077 / 1987 / 4 Players
' Universal de Desarrollos Electronicos, S.A., of Barcelona,Spain
' [Trade Name: Unidesa/Cirsa/Stargame]
' ****************************************************************

Option Explicit
Randomize

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1.7   ' standard ball mass in JP's VPX Physics 3.0
Const SongVolume = 1 ' 1 is full volume. Value is from 0 to 1

' Load the core.vbs for supporting Subs and functions
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
    On Error Goto 0
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

' Define any Constants
Const cGameName = "JPMephisto"
Const myVersion = "1.0.0"
Const MaxPlayers = 4         ' from 1 to 4
Const MaxBonusMultiplier = 5 'limit Bonus multiplier
Const BallsPerGame = 3       ' usually 3 or 5
Const MaxMultiballs = 3      ' max number of balls during multiballs

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim PlayfieldMultiplier(4)
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim JackpotValue
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue(2)
Dim bAutoPlunger
Dim bAttractMode
Dim x

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLLock
Dim BallsInRLock

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bMultiBallMode
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJustStarted

' core.vbs variables

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Loadhs

    ' Initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = False 'we want coins

    if bFreePlay Then DOF 125, DOFOn

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bMultiBallMode = False
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInLLock = 0
    BallsInRLock = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    JackpotValue = 50000
    bBonusHeld = False
	kicker004.enabled = False
    bJustStarted = True
    ' set any lights for the attract mode
    GiOff
    StartAttractMode

    ' Start the RealTime timer
    RealTime.Enabled = 1

    ' Load table color
    LoadLut
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    If keycode = LeftMagnaSave Then bLutActive = True
    If keycode = RightMagnaSave Then
        If bLutActive Then NextLUT:End If
    End If

    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        if bFreePlay = False Then DOF 125, DOFOn
        If(Tilted = False)Then
            DMDFlush
            DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
			Select Case RndNbr(2)
				Case 1:PlaySound "thankyou"
				Case 2:PlaySound "insertcoin"
			End Select
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
        PlaySoundAt "fx_reload", plunger
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Table specific

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "fx_Next"
                Else
                    If(Credits> 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "fx_Next"
                        If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
							DMDFlush
							DMD CL(0, "YOU MUST"), CL(1, "PAY THE TOLL"), "", eNone, eNone, eBlink, 1000, True, ""
                            'old DMD line 'DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "fx_Esc"
							Select Case RndNbr(2)
								Case 1:PlaySound "nocoins1"
								Case 2:PlaySound "nocoins2"
							End Select
							If NOT bGameInPlay Then ShowTableInfo
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
                    If(Credits> 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
						DMDFlush
						DMD CL(0, "YOU MUST"), CL(1, "PAY THE TOLL"), "", eNone, eNone, eBlink, 1000, True, ""
                        Select Case RndNbr(2)
							Case 1:PlaySound "nocoins1"
							Case 2:PlaySound "nocoins2"
						End Select
						ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)

'test keys
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = LeftMagnaSave Then bLutActive = False

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
        If bBallInPlungerLane Then PlaySoundAt "fx_fire", plunger
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
        End If
    End If
End Sub

'*************
' Pause Table
'*************

Sub table1_Paused
PlaySong "mu_death"
End Sub

Sub table1_unPaused
ChangeSong
End Sub

Sub Table1_Exit
    Savehs
    If UseFlexDMD Then FlexDMD.Run = False
    If B2SOn = true Then Controller.Stop
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.75:LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.2:LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.75:RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.2:RightFlipper.RotateToStart
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                  'Called when table is nudged
    Tilt = Tilt + TiltSensitivity              'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity)AND(Tilt <15)Then 'show a warning
        DMD "_", CL(1, "CAREFUL!"), "_", eNone, eBlinkFast, eNone, 500, True, ""
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD "", "", "TILT", eNone, eNone, eBlink, 200, False, ""
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt> 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Force = 0
        Bumper2.Force = 0
        Bumper3.Force = 0
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper1.Force = 7
        Bumper2.Force = 7
        Bumper3.Force = 7
        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'*****************************************
'         Music as wav sounds
' in VPX 10.7 you may use also mp3 or ogg
'*****************************************

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

Sub ChangeSong
    If(BallsOnPlayfield = 0)Then
        Select Case RndNbr(3)
            Case 1:PlaySong "mu_waiting1"
            Case 2:PlaySong "mu_waiting2"
			Case 3:PlaySong "mu_waiting3"
        End Select
    Else
        Select Case RndNbr(5)
            Case 1:PlaySong "mu_main1"
            Case 2:PlaySong "mu_main2"
            Case 3:PlaySong "mu_main3"
            Case 4:PlaySong "mu_main4"
            Case 5:PlaySong "mu_main5"
            Case 6:PlaySong "mu_main6"
            Case 7:PlaySong "mu_main7"
            Case 8:PlaySong "mu_main8"
        End Select
    End If
End Sub

'**********************
'     GI effects
'**********************

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub GiOn
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aGiLights2
        bulb.State = 2
    Next
    gieye001.State = 1
    gieye002.State = 1
End Sub

Sub GiOff
    DOF 118, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aGiLights2
        bulb.State = 0
    Next
    gieye001.State = 0
    gieye002.State = 0
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 100
            LightSeqGi.Play SeqBlinking, , 25, 20
        Case 2 'random
            LightSeqGi.UpdateInterval = 20
            LightSeqGi.Play SeqRandom, 100, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 10, 10
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 10, 10
    End Select
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v3.0
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
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.4, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls, 20 balls, from 0 to 19
Const lob = 0     'number of locked balls
Const maxvel = 40 'max ball velocity
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

    ' stop the sound of deleted balls and hide the shadow
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
        aBallShadow(b).Height = BOT(b).Z -24

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
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
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
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
Sub aTargets_Hit(idx):ActiveBall.VelZ = BallVel(Activeball) * (RND / 2):End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True
	PlaySound "welcome"
    'resets the score display, and turn off attract mode
    StopAttractMode
    GiOn
    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        PlayfieldMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next
    SkillshotValue(0) = 1000
    SkillshotValue(1) = 15000
    SkillshotValue(2) = 30000
	kicker004.enabled = False

    ' initialise any other flags
    Tilt = 0

    ' initialise Game variables
    Game_Init()

    ' you may wish to start some music, play a sound, do whatever at this point

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

    ' reduce the playfield multiplier
    ' reset any drop targets, lights, game Mode etc..

    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False

    'Reset any table specific
    ResetNewBallVariables
    ResetNewBallLights()

    'and the skillshot
    bSkillShotReady = True

'Change the music ?
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1
    ' kick it out..
    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4

' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
'   If BallsOnPlayfield> 1 Then
'       DOF 143, DOFPulse
'       bMultiBallMode = True
'       bAutoPlunger = True
'   End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
    'and eject the first ball
    CreateMultiballTimer_Timer
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        If BallsOnPlayfield <MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
                CreateMultiballTimer.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            CreateMultiballTimer.Enabled = False
        End If
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()

    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False
	kicker004.enabled = False
	gi028.state = 0
    PlaySound"death"
	'PlaySound "mu_death2"
    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

    If NOT Tilted Then
        StartBonusCount
    ' add a bit of a delay to allow for the bonus points to be shown & added up
    'vpmtimer.addtimer 6000, "EndOfBallComplete '" 'done in the bonuscount
    Else 'if tilted then only add a short delay
        vpmtimer.addtimer 100, "EndOfBall2 '"
    End If
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots
	kicker004.enabled = False
    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0)Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, ""

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            'debug.print "No More Balls, High Score Entry"

            ' Submit the CurrentPlayers score to the High Score system
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
	kicker004.enabled = False

    'debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame> 1)Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer> PlayersPlayingGame)Then
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

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            PlaySound "vo_player" &CurrentPlayer
            DMD "_", CL(1, "PLAYER " &CurrentPlayer), "_", eNone, eNone, eNone, 800, True, ""
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    'debug.print "End Of Game"
	DMD "", "", "gameover", eNone, eNone, eBlink, 2000, False, "mu_death2"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then
        ChangeSong
    End If

    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball
    EjectLeftkicker
    EjectRightkicker
    ' set any lights for the attract mode
    DisableTable True
    'GiOff
    StartAttractMode
' you may wish to light any Game Over Light you may have
End Sub

Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
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
    ' Exit Sub ' only for debugging - this way you can add balls from the debug window

    BallsOnPlayfield = BallsOnPlayfield - 1

    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain
    'if Tilted the end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If


    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' cancel any multiball if on last ball (ie. lost all other balls)
        If(BallsOnPlayfield - BallsInLlock - BallsInRLock = 1)Then
            ' AND in a multi-ball??
            If(bMultiBallMode = True)then
                ' not in multiball mode any more
                bMultiBallMode = False
                ' you may wish to change any music over at this point and
                ChangeSong
                ' turn off any multiball specific lights
                ChangeGi white
            'stop any multiball modes

            End If
        End If

        ' was that the last ball on the playfield
        If(BallsOnPlayfield - BallsInLlock - BallsInRLock = 0)Then
            ' End Mode and timers
            ChangeSong
            ChangeGi white
            ' Show the end of ball animation
            ' and continue with the end of ball
            ' DMD something?
            StopEndOfBallMode
            vpmtimer.addtimer 1500, "EndOfBall '" 'the delay is depending of the animation of the end of ball, if there is no animation then move to the end of ball
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
    'LaunchLight.State = 2

    'be sure to update the Scoreboard after the animations, if any

    ' kick the ball in play if the bAutoPlunger flag is on
    'If bAutoPlunger Then
    'debug.print "autofire the ball"
    '   PlungerIM.AutoFire
    '  DOF 121, DOFPulse
    '  PlaySoundAt "fx_fire", swPlungerRest
    '  bAutoPlunger = False
    'End If
    ' show the message to shoot the ball in case the player has fallen sleep :)
    swPlungerRest.TimerEnabled = 1

    'Start the Selection of the skillshot if ready
    If bSkillShotReady Then
        StartSkillshot
    End If
	kickerlight.state = 2
	if bLeftLockEnabled = True then li038.state = 2
	if bRightLockEnabled = true then li046.state = 2
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
	PlaySound "huh"
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    ChangeSong
' turn off LaunchLight
' LaunchLight.State = 0
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds

Sub swPlungerRest_Timer
    DMD "_", CL(1, "SHOOT THE BALL"), "_", eNone, eNone, eNone, 800, True, ""
    swPlungerRest.TimerEnabled = 0
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
' In this table we use SecondRound variable to double the score points in the second round after killing Malthael
Sub AddScore(points)
    If(Tilted = False)Then
        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
    End if
' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'in this table the jackpotValue also increases with the BonusPoints
    If(Tilted = False)Then
        ' increase the jackpot
        JackpotValue = JackpotValue + points * 1000 * BonusMultiplier(CurrentPlayer)
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
        If BonusPoints(CurrentPlayer)> 39 Then
            BonusPoints(CurrentPlayer) = 39
        End If
        UpdateBonusLights
    End if
End Sub

Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxBonusMultiplier)then
        ' then add and set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
        SetBonusMultiplier(NewBonusLevel)
        DMD "_", CL(1, "BONUS X " &NewBonusLevel), "_", eNone, eNone, eNone, 2000, True, "fx_bonus"
    Else
        AddScore 50000
        DMD "_", CL(1, "50000"), "_", eNone, eNone, eNone, 800, True, ""
    End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly

Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level
    ' Update the lights
    Select Case Level
        Case 1:li003.State = 0:li004.State = 0:li005.State = 0:li006.State = 0
        Case 2:li003.State = 1:li004.State = 0:li005.State = 0:li006.State = 0
        Case 3:li003.State = 0:li004.State = 1:li005.State = 0:li006.State = 0
        Case 4:li003.State = 0:li004.State = 0:li005.State = 1:li006.State = 0
        Case 5:li003.State = 0:li004.State = 0:li005.State = 0:li006.State = 1
    End Select
End Sub

Sub StartBonusCount
    If BonusPoints(CurrentPlayer)> 9 Then
		PlaySound "unrewarded"
	end If
    BonusCountTimer.Interval = 500
    BonusCountTimer.Enabled = 1
End Sub

Sub BonusCountTimer_Timer
    'debug.print "BonusCount_Timer"
    If BonusPoints(CurrentPlayer)> 0 Then
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer)-1
		PlaySound "Coins"
        AddScore 1000 * BonusMultiplier(Currentplayer)
        UpdateBonusLights
    Else
        BonusCountTimer.Enabled = 0
        vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub

Sub UpdateBonusLights
    Select Case BonusPoints(CurrentPlayer)MOD 10
        Case 0:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 1:bl1.State = 1:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 2:bl1.State = 1:bl2.State = 1:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 3:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 4:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 5:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 6:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 1:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 7:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 1:bl7.State = 1:bl8.State = 0:bl9.State = 0
        Case 8:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 1:bl7.State = 1:bl8.State = 1:bl9.State = 0
        Case 9:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 1:bl7.State = 1:bl8.State = 1:bl9.State = 1
    End Select
    Select Case BonusPoints(CurrentPlayer) \ 10
        Case 0:bl10.State = 0:bl20.State = 0:bl30.State = 0
        Case 1:bl10.State = 1:bl20.State = 0:bl30.State = 0
        Case 2:bl10.State = 1:bl20.State = 1:bl30.State = 0
        Case 3:bl10.State = 1:bl20.State = 1:bl30.State = 1
    End Select
End Sub

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        DMD "_", CL(1, ("EXTRA BALL WON")), "_", eNone, eBlink, eNone, 1500, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        LightShootAgain.State = 1 'light the shoot again lamp
        GiEffect 2
        LightEffect 2
    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL(1, ("EXTRA GAME WON")), "_", eNone, eBlink, eNone, 1500, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    Credits = Credits + 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
End Sub

Sub AwardJackpot() 'award a normal jackpot
    DMD CL(0, FormatScore(JackpotValue)), CL(1, "JACKPOT"), "bkborder", eBlinkFast, eBlinkFast, eNone, 1500, True, ""
    DOF 126, DOFPulse
    PlaySound "Jackpot"
    AddScore JackpotValue
    LightEffect 2
    'reset Jackpot after it has been collected
    JackpotValue = 50000
End Sub

Sub StopSkillShot
    SkillShotTimer.Enabled = 0
End Sub

Sub AwardSkillshot()
    li025.State = 0
    li026.State = 0
    li027.State = 0
    bSkillShotReady = False
    'show dmd animation
    DMD CL(0, FormatScore(SkillshotValue(SkStep))), CL(1, ("SKILLSHOT")), "bkborder", eBlinkFast, eBlink, eNone, 2000, True, ""
    DOF 127, DOFPulse
    'PlaySound "fx_fanfare2"
    Addscore SkillShotValue(SkStep)
    GiEffect 2
'    LightEffect 2
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
    HighScore(0) = 150000
    HighScore(1) = 140000
    HighScore(2) = 130000
    HighScore(3) = 120000
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
    tmp = Score(1)
    If Score(2)> tmp Then tmp = Score(2)
    If Score(3)> tmp Then tmp = Score(3)
    If Score(4)> tmp Then tmp = Score(4)

    If tmp> HighScore(1)Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
        DOF 125, DOFOn
    End If

    If tmp> HighScore(3)Then
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
    ChangeSong
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' ` is back arrow
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
        if(hsCurrentLetter> len(hsValidLetters))then
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
            if(hsCurrentDigit> 0)then
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
    if(hsCurrentDigit> 0)then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3)then
        if(hsLetterFlash <> 0)then
            TempBotStr = TempBotStr & "_"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

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
    ChangeSong
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
            If HighScore(j) <HighScore(j + 1)Then
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

'*********
'   LUT
'*********

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

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 10:UpdateLUT:SaveLUT:End Sub

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
    End Select
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
                DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3, 7, 11
            Next
            For i = 20 to 39 ' Bottom
                DMDScene.GetImage("Dig" & i).SetBounds 4 + (i - 20) * 6, 3 + 12 + 2, 7, 11
            Next
            FlexDMD.LockRenderThread
            FlexDMD.Stage.AddActor DMDScene
            FlexDMD.UnlockRenderThread
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
        tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
        'tmp1 = FormatScore(Bonuspoints(Currentplayer) ) & " X" &BonusMultiplier(Currentplayer)
        tmp2 = "bkborder"
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail <dqSize)Then
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
        if(Len(TempStr)> Space(dCharsPerLine(id)))Then
            TempStr = Left(TempStr, Space(dCharsPerLine(id)))
        Else
            if(Len(TempStr) <dCharsPerLine(id))Then
                TempStr = TempStr & Space(dCharsPerLine(id)- Len(TempStr))
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i, NumString
    NumString = CStr(abs(Num))
    For i = Len(NumString)-3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1))then
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1)) + 48) & right(NumString, Len(NumString)- i)
        end if
    Next
    FormatScore = NumString
End function

Function CL(id, NumString) 'center line
    Dim Temp, TempStr
    Temp = (dCharsPerLine(id)- Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(id, NumString) 'right line
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

    Chars(32) = "dempty"
    Chars(35) = "jp1"
    Chars(36) = "jp2"
    Chars(37) = "jp3"
    Chars(38) = "title1"
    Chars(39) = "title2"
    Chars(40) = "title3"
    Chars(46) = "dot"     '.
    Chars(48) = "d0"      '0
    Chars(49) = "d1"      '1
    Chars(50) = "d2"      '2
    Chars(51) = "d3"      '3
    Chars(52) = "d4"      '4
    Chars(53) = "d5"      '5
    Chars(54) = "d6"      '6
    Chars(55) = "d7"      '7
    Chars(56) = "d8"      '8
    Chars(57) = "d9"      '9
    Chars(60) = "dless"   '<
    Chars(61) = "dequal"  '=
    Chars(62) = "dmore"   '>
    Chars(64) = "bkempty" '@
    Chars(65) = "da"      'A
    Chars(66) = "db"      'B
    Chars(67) = "dc"      'C
    Chars(68) = "dd"      'D
    Chars(69) = "de"      'E
    Chars(70) = "df"      'F
    Chars(71) = "dg"      'G
    Chars(72) = "dh"      'H
    Chars(73) = "di"      'I
    Chars(74) = "dj"      'J
    Chars(75) = "dk"      'K
    Chars(76) = "dl"      'L
    Chars(77) = "dm"      'M
    Chars(78) = "dn"      'N
    Chars(79) = "do"      'O
    Chars(80) = "dp"      'P
    Chars(81) = "dq"      'Q
    Chars(82) = "dr"      'R
    Chars(83) = "ds"      'S
    Chars(84) = "dt"      'T
    Chars(85) = "du"      'U
    Chars(86) = "dv"      'V
    Chars(87) = "dw"      'W
    Chars(88) = "dx"      'X
    Chars(89) = "dy"      'Y
    Chars(90) = "dz"      'Z
    Chars(94) = "dup"     '^
    '    Chars(95) = '_
    Chars(96) = "d0a"  '0.
    Chars(97) = "d1a"  '1. 'a
    Chars(98) = "d2a"  '2. 'b
    Chars(99) = "d3a"  '3. 'c
    Chars(100) = "d4a" '4. 'd
    Chars(101) = "d5a" '5. 'e
    Chars(102) = "d6a" '6. 'f
    Chars(103) = "d7a" '7. 'g
    Chars(104) = "d8a" '8. 'h
    Chars(105) = "d9a" '9  'i
End Sub

'********************
' Real Time updates
'********************
' used for all the real time updates like gates, diverters, flippers

Sub Realtime_Timer
    RollingUpdate
    LeftFlipperTop.Rotz = LeftFlipper.CurrentAngle
    RightFlipperTop.Rotz = RightFlipper.CurrentAngle
    Diverter001.Rotz = DiverterF.CurrentAngle
End Sub

'********************************************************************************************
' Only for VPX 10.2 and higher.
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
' 10 colors: red, orange, amber, yellow...
'******************************************
' in this table this colors are use to keep track of the progress during the acts and battles

'colors
Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white

red = 10
orange = 9
amber = 8
yellow = 7
darkgreen = 6
green = 5
blue = 4
darkblue = 3
purple = 2
white = 1

Sub SetLightColor(n, col, stat)
    Select Case col
        Case 0
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(255, 0, 0)
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
        Case white
            n.color = RGB(255, 252, 224)
            n.colorfull = RGB(193, 91, 0)
        Case white
            n.color = RGB(255, 252, 224)
            n.colorfull = RGB(193, 91, 0)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

'*************************
' Rainbow Changing Lights
'*************************

Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

Sub StartRainbow(n)
    set RainbowLights = n
    RGBStep = 0
    RGBFactor = 5
    rRed = 255
    rGreen = 0
    rBlue = 0
    RainbowTimer.Enabled = 1
End Sub

Sub StopRainbow()
    Dim obj
    RainbowTimer.Enabled = 0
    RainbowTimer.Enabled = 0
End Sub

Sub RainbowTimer_Timer 'rainbow led light color changing
    Dim obj
    Select Case RGBStep
        Case 0 'Green
            rGreen = rGreen + RGBFactor
            If rGreen> 255 then
                rGreen = 255
                RGBStep = 1
            End If
        Case 1 'Red
            rRed = rRed - RGBFactor
            If rRed <0 then
                rRed = 0
                RGBStep = 2
            End If
        Case 2 'Blue
            rBlue = rBlue + RGBFactor
            If rBlue> 255 then
                rBlue = 255
                RGBStep = 3
            End If
        Case 3 'Green
            rGreen = rGreen - RGBFactor
            If rGreen <0 then
                rGreen = 0
                RGBStep = 4
            End If
        Case 4 'Red
            rRed = rRed + RGBFactor
            If rRed> 255 then
                rRed = 255
                RGBStep = 5
            End If
        Case 5 'Blue
            rBlue = rBlue - RGBFactor
            If rBlue <0 then
                rBlue = 0
                RGBStep = 0
            End If
    End Select
    For each obj in RainbowLights
        obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
        obj.colorfull = RGB(rRed, rGreen, rBlue)
    Next
End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim ii
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 1 " &FormatScore(Score(1))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    'DMD "", "", "gameover", eNone, eNone, eBlink, 2000, False, ""
    If bFreePlay Then
        DMD "", CL(1, "FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits> 0 Then
            DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "", "", "sgpresents", eNone, eNone, eNone, 3000, False, ""
	DMD "", "", "jpthanks", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "sotn", eNone, eNone, eNone, 4000, False, ""
    DMD "", "", "sotn2", eNone, eNone, eNone, 4000, False, ""
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0)), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    ChangeSong
    StartLightSeq
    DMDFlush
    ShowTableInfo
End Sub

Sub StopAttractMode
    DMDScoreNow
    LightSeqAttract.StopPlay
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

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
Dim SkStep
Dim SpecialL1
Dim SpecialL2
Dim SpecialL3
Dim SpecialL4
Dim SpecialL5
Dim BallInLkicker
Dim BallInRkicker
Dim bLeftLockEnabled
Dim bRightLockEnabled
Dim Drop1, Drop2, Drop3
Dim DropValue

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    DisableTable False
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    'Play some Music
    ChangeSong
    'Init Variables
    bLeftLockEnabled = False
    bRightLockEnabled = False
    BallsInRLock = 0
    BallsInLLock = 0
    rampdiverter.IsDropped = 1
    Target001.isDropped = 0:PlaySoundAt "fx_resetdrop", target001
'Init Delays/Timers
'MainMode Init()
'Init lights
End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained, reset skillshot, modes, timers
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
    SpecialL1 = 0
    SpecialL2 = 0
    SpecialL3 = 0
    SpecialL4 = 0
    SpecialL5 = 0
    ResetDropTargets
    DropValue = 1000
    CloseGate
End Sub

Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
    TurnOffPlayfieldLights
    li045.State = 1
    li047.State = 1
    li031.State = 2
    If(BallsInRLock + BallsInLLock)> 0 Then LitCenterLights
    If bRightLockEnabled Then li046.State = 2
    'li036.State = 1:li037.State = 1:li038.State = 1
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

'************
' SKILLSHOT
'************

Sub StartSkillShot() 'Setup and updates the skillshot lights
    SkStep = INT(3 * RND(1))
    SkillShotTimer.Interval = 1000
    SkillShotTimer.Enabled = 1
    DMD CL(0, "HIT LIT LIGHT"), CL(1, "FOR SKILLSHOT"), "", eNone, eNone, eNone, 1500, True, ""
End Sub

Sub SkillShotTimer_Timer
    SkStep = (SkStep + 1)MOD 3
    If SkillShotTimer.Interval> 200 Then
        SkillShotTimer.Interval = SkillShotTimer.Interval -25
    End If
    Select Case SkStep
        Case 0:li025.State = 0:li026.State = 0:li027.State = 1
        Case 1:li025.State = 0:li026.State = 1:li027.State = 0
        Case 2:li025.State = 1:li026.State = 0:li027.State = 0
    End Select
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************

'*******************
' 10 points rubbers
'*******************

Sub rsband002_Hit:Addscore 10:End Sub
Sub rlband004_Hit:Addscore 10:End Sub
Sub rlband006_Hit:Addscore 10:End Sub
Sub rlband003_Hit:Addscore 10:End Sub


'*******************
'    Slingshots
'*******************

' RStep and LStep are the variables that increment the animation
Dim RStep, LStep

Sub RightSlingShot_SlingShot
	Addscore 10
	RightSling003.Visible = 1
	RSling.TransY = -20			'Sling Metal Bracket
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	RightSlingShot.TimerInterval = 10
	PlaySound "pain1"
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 3:RightSling003.Visible = 0:RightSling002.Visible = 1:LSling.TransY = -10
		Case 4:RightSling002.Visible = 0:RSling.TransY = 0:RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_SlingShot
	Addscore 10
	LeftSling003.Visible = 1
	LSling.TransY = -20			'Sling Metal Bracket
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval = 10
	PlaySound "pain2"
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3:LeftSling003.Visible = 0:LeftSling002.Visible = 1:LSling.TransY = -10
		Case 4:LeftSling002.Visible = 0:LSling.TransY = 0:LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

'*********
' Bumpers
'*********

Sub Bumper1_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper1
        DOF 138, DOFPulse
        ' add some points
        Addscore 500 + 4500 * Bumper1L.State
        PlaySound"fx_bumper"
        PlaySound"whoosh"
    End If
End Sub

Sub Bumper2_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper2
        DOF 138, DOFPulse
        ' add some points
        Addscore 500 + 4500 * Bumper2L.State
        PlaySound"fx_bumper"
        PlaySound"whip"
    End If
End Sub

Sub Bumper3_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper3
        DOF 138, DOFPulse
        ' add some points
        Addscore 500 + 4500 * Bumper3L.State
        PlaySound"fx_bumper"
        PlaySound"slice"
    End If
End Sub

'***********
' Rollovers
'***********

Sub Trigger005_Hit 'Skillshot trigger
    DOF 128, DOFPulse
	PlaySoundAt "fx_sensor", Trigger005
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        StopSkillShot
    End If
	If ActiveBall.VelY < 0 Then 'ball moving down
		kicker004.enabled = True
	End If
End Sub

Sub Trigger001_Hit 'right inlane
    DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger001
    If Tilted Then Exit Sub
    Addscore 1000 + 29000 * SpecialL5
    AddBonus 1
    Flashforms li021, 500, 50, 1
    SpecialL5 = 1
    CheckSpecial
End Sub

Sub Trigger002_Hit 'right outlane
    DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger002
    If Tilted Then Exit Sub
    Addscore 40000
    If li002.State = 1 Then
        AwardSpecial
    Else
        PlaySound "falling"
    End IF
    vpmtimer.Addtimer 2000, "CloseGate '"
End Sub

Sub Trigger003_Hit 'left inlane
    DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger003
    If Tilted Then Exit Sub
    Addscore 1000 + 29000 * SpecialL4
    AddBonus 1
    Flashforms li020, 500, 50, 1
    SpecialL4 = 1
    CheckSpecial
End Sub

Sub Trigger004_Hit 'left outlane
    DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger004
    If Tilted Then Exit Sub
    Addscore 40000
    If li001.State = 1 Then
        AwardSpecial
    Else
        PlaySound "falling"
    End IF
End Sub


Sub Trigger006_Hit 'right kicker sensor
    DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger006
    If Tilted Then Exit Sub
    'PlaySound "business"
    AddScore 1000
    If BallsinRlock = 1 Then EjectRightkicker            'although this should never happen
    If bRightLockEnabled Then rampdiverter.IsDropped = 0 'next ball will go to the left kicker though the ramp.
End Sub

Sub Trigger007_Hit                                       'right loop
    DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger007
    If Tilted Then Exit Sub
    PlaySound"evillaugh"
    AddScore 1000 + 9000 * li047.State
    li047.State = 1
End Sub

Sub Trigger008_Hit 'top right lane
    DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger008
	PlaySound "rollover"
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        AwardSkillshot
    Else
        Addscore 1000 + 14000 * SpecialL1
        AddBonus 1
    End If
    Flashforms li035, 500, 50, 1
    SpecialL1 = 1
    CheckSpecial
End Sub

Sub Trigger009_Hit ' top center lane
    DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger009
	PlaySound "rollover"
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        AwardSkillshot
    Else
        Addscore 1000 + 14000 * SpecialL2
        AddBonus 1
    End If
    Flashforms li034, 500, 50, 1
    SpecialL2 = 1
    CheckSpecial
End Sub

Sub Trigger010_Hit 'top left lane
    DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger010
	PlaySound "rollover"
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        AwardSkillshot
    Else
        Addscore 1000 + 14000 * SpecialL3
        AddBonus 1
    End If
    Flashforms li033, 500, 50, 1
    SpecialL3 = 1
    CheckSpecial
End Sub

Sub Trigger011_Hit 'left loop
    DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger011
    If Tilted Then Exit Sub
    PlaySound"Bell"
    AddScore 1000 + 9000 * li045.State
    li045.State = 1
End Sub

Sub Trigger012_Hit 'left kicker sensor
    DOF 128, DOFPulse
    PlaySoundAt "fx_sensor", Trigger012
    If Tilted Then Exit Sub
    AddScore 1000
    If BallsinLlock = 1 Then EjectLeftkicker
End Sub

' Droptargets

Sub Target001_Hit 'left ramp droptarget
    PlaySoundAt "fx_droptarget", Target001
    If Tilted Then Exit Sub
    PlaySound"DropTargets"
	PlaySound "diemonster"
    flashforms flasher003, 2000, 50, 0
    bLeftLockEnabled = True
	li038.state = 2
    Addscore 30000
End Sub

Sub Target002_Hit 'top right droptarget
    PlaySoundAtBall "fx_droptarget"
    If Tilted Then Exit Sub
    PlaySound"DropTargets"
	PlaySound "rock1"
    bumper1L.State = 1
    Addscore DropValue
    Drop1 = 1
    CheckDroptgs
End Sub

Sub Target003_Hit 'center right droptarget
    PlaySoundAtBall "fx_droptarget"
    If Tilted Then Exit Sub
    PlaySound"DropTargets"
	PlaySound "rock2"
    bumper2L.State = 1
    Addscore DropValue
    Drop2 = 1
    CheckDroptgs
End Sub

Sub Target004_Hit 'lower right droptarget
    PlaySoundAtBall "fx_droptarget"
    If Tilted Then Exit Sub
    PlaySound"DropTargets"
	PlaySound "rock3"
    bumper3L.State = 1
    Addscore DropValue
    Drop3 = 1
    CheckDroptgs
End Sub

Sub ResetDroptargets
    Drop1 = 0
    Drop2 = 0
    Drop3 = 0
    PlaySoundat "fx_resetdrop", Target003
    Target002.IsDropped = 0
    Target003.IsDropped = 0
    Target004.IsDropped = 0
End Sub

Sub CheckDroptgs
    If Drop1 + Drop2 + Drop3 = 3 Then
        Select case Dropvalue
            Case 1000:DropValue = 5000:li024.State = 1:AddBonusMultiplier 1
            Case 5000:DropValue = 10000:li023.State = 1:AddBonusMultiplier 1
            Case 10000:DropValue = 5000:li022.State = 1:AddBonusMultiplier 1
            Case Else:AddBonusMultiplier 1
        End Select
        vpmtimer.addtimer 5000, "ResetDroptargets '" 'wait 5 seconds to reset droptargets so you may hit the target behind to open the right gate
    End If
End Sub

' Standup targets

Sub Target005_Hit 'behind droptargets
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    PlaySound "unlock"
    Addscore 1000
    DiverterF.RotateToEnd
    PlaySoundAt "fx_SolenoidON", DiverterF
    PlaySound"door_open"
End Sub

Sub CloseGate
    DiverterF.RotateToStart
    PlaySoundAt "fx_SolenoidOff", DiverterF
    PlaySound"door_close"
End Sub

Sub Target006_Hit 'first green target
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    PlaySound"LeftTargets"
    AddBonus 1
    If li031.State = 1 Then
        Addscore 30000
    ElseIf li031.State = 2 Then 'next green target
        li031.State = 1
        li030.State = 2
        Addscore 1000
    End If
    If li032.State = 2 Then
        li032.State = 0
        AwardExtraBall
    End If
End Sub

Sub Target007_Hit '2nd green target
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    PlaySound"LeftTargets"
    AddBonus 1
    If li030.State = 1 Then
        Addscore 30000
		PlaySound "chling"
		gi028.state = 1 'enable savepoint
		'flashforms flasher004, 2000, 50, 0
    ElseIf li030.State = 2 Then 'next green target
        li030.State = 1
        li029.State = 2
        Addscore 1000
    End If
End Sub

Sub Target008_Hit '3rd green target
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    PlaySound"LeftTargets"
    AddBonus 1
    If li029.State = 1 Then
        Addscore 30000
		PlaySound "chling"
		gi028.state = 1 'enable savepoint
		'flashforms flasher004, 2000, 50, 0
    ElseIf li029.State = 2 Then 'next green target
        li029.State = 1
        li028.State = 2
        Addscore 1000
    End If
End Sub

Sub Target009_Hit '4th green target
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    PlaySound"LeftTargets"
    AddBonus 1
    If li028.State = 1 Then
        Addscore 30000
		PlaySound "chling"
		gi028.state = 1 'enable savepoint
		'flashforms flasher004, 2000, 50, 0
    ElseIf li028.State = 2 Then 'next green target
        li028.State = 1
        li032.State = 2         'lit extraball
        DMD "_", CL(1, ("EXTRA BALL IS LIT")), "_", eNone, eBlink, eNone, 1500, True, ""
        Addscore 1000
    End If
End Sub

Sub Target010_Hit 'behind right Bumper1
	dim cansave
	If gi028.state = 0 Then Exit Sub
	If cansave = false then exit Sub
	PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
	Select Case RndNbr(3)
		Case 1: PlaySound "Save1"
		Case 2: PlaySound "Save2"
		Case 3: PlaySound "Save3"
	End Select
	AddScore 5000
	gi028.state = 2
	cansave = False
	Target010.timerenabled = 1
End Sub

Sub Target010_Timer()
	cansave = True
	gi028.state = 0
End Sub

' Special

Sub CheckSpecial
    If SpecialL1 + SpecialL2 + SpecialL3 + SpecialL4 + SpecialL5 = 5 Then
        DMD "_", CL(1, "SPECIAL IS LIT"), "", eNone, eBlink, eNone, 1500, True, ""
        li001.State = 1
        li002.State = 1
    End If
End Sub

'*******************
' Holes - kickers
'*******************

Sub kicker001_hit 'left kicker
    PlaySoundAt "fx_kicker_enter", kicker001
    If Tilted Then EjectLeftkicker:Exit Sub
    iF BallsInLLock = 1 Then Exit Sub
    If bLeftLockEnabled Then
        GiEffect 2
        AddMultiball 1
        LitCenterLights
        BallsInLLock = 1
		bLeftLockEnabled = 0
		li038.state = 0
		PlaySound "cease"
        DMD CL(0, "BALL LOCKED"), CL(1, "SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, ""
    Else
		PlaySound "notask"
        EjectLeftkicker
    End If
End Sub

Sub EjectLeftkicker
    PlaySoundAt "fx_kicker", kicker001
    kicker001.kick 90, 10
End Sub

Sub kicker002_hit 'right kicker
    PlaySoundAt "fx_kicker_enter", kicker002
    If Tilted Then EjectRightkicker:Exit Sub
    If BallsInRLock = 1 Then Exit Sub
    If bRightLockEnabled Then
        GiEffect 2
        AddMultiball 1
        LitCenterLights
        BallsInRLock = 1
		bRightLockEnabled = 0
		If BallsInLLock + BallsInRLock = 2 Then
			li046.state = 0
		End If
		PlaySound "business"
        DMD CL(0, "BALL LOCKED"), CL(1, "SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, ""
    Else
        bRightLockEnabled = True
		PlaySound "belmont"
        li046.State = 2
        EjectRightkicker
    End If
End Sub

Sub EjectRightkicker
    PlaySoundAt "fx_kicker", kicker001
    kicker002.kick -90, 10
End Sub

Sub kicker003_hit 'right kicker
    PlaySoundAt "fx_kicker_enter", kicker003
    If Tilted Then EjectCenterkicker:Exit Sub
    flashforms flasher001, 2000, 50, 0
    flashforms flasher002, 2000, 50, 0
    If BallsInRLock + BallsInLLock> 0 Then '1 or 2 balls are locked then start multiball
        bMultiBallMode = True
        DMD "", "", "MULTIBALL", eNone, eNone, eBlink, 2000, True, ""
        vpmtimer.addtimer 2000, "EjectCenterkicker '"
        vpmtimer.addtimer 2500, "EjectRightkicker '"
        vpmtimer.addtimer 3500, "EjectLeftkicker '"
        BallsInRLock = 0
        BallsInLLock = 0
		bLeftLockEnabled = False
		bRightLockEnabled = False
        GiEffect 3
    Else
        vpmtimer.addtimer 1000, "EjectCenterkicker '"
    End If
    If bMultiBallMode Then
        AwardJackpot
        PlaySound "comeforth"
		Select Case RndNbr(2)
			Case 1:PlaySong "mu_multiball1"
			Case 2:PlaySong "mu_multiball2"
		End Select
		UnLitCenterLights
		Target001.isdropped = False
    Else
        PlaySound "hya"
    End If
End Sub

Sub EjectCenterkicker
    PlaySoundAt "fx_kicker", kicker003
    kicker003.kick 190, 10
End Sub

Sub kicker004_hit 'lane kicker
    PlaySoundAt "fx_kicker_enter", kicker004
    vpmtimer.addtimer 1000, "EjectLanekicker '"
End Sub

Sub EjectLanekicker
    PlaySoundAt "fx_kicker", kicker004
	PlaySound "huh"
	AddScore 50000
    kicker004.kick 0, 30
	kicker004.enabled = False
End Sub


Sub LitCenterLights
    li039.State = 2
    li040.State = 2
    li041.State = 2
    li042.State = 2
    li043.State = 2
    li044.State = 2
End Sub

Sub UnLitCenterLights
    li039.State = 0
    li040.State = 0
    li041.State = 0
    li042.State = 0
    li043.State = 0
    li044.State = 0
End Sub

'*************
' Play Effects
'*************



Sub BallSaveTimer_Timer()
	
End Sub



Sub Gate4_Hit()
	kicker004.enabled = False
	kickerlight.state = 0
End Sub

