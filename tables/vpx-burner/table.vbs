' ********************************************************
'                     BURNER
'            for VISUAL PINBALL X 10.8
'        Uses FlexDMD for cabinet / FS mode
'            table by jpsalas - 2026
'            graphics by hassanchop
' all the user options are in the F12 user settings menu
' ********************************************************

Option Explicit
Randomize

Const BallSize = 50 ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1  ' standard ball mass

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

' Define any Constants
Const cGameName = "burner"     'used for DOF & saving highscores
Const MyTable = "burner"       ' used to save the highscores
Const myVersion = "1.01"
Const MaxPlayers = 4           ' from 1 to 4
Const MaxMultiplier = 100      ' no limit playfield multiplier
Const MaxBonusMultiplier = 100 'no limit Bonus multiplier
Const MaxMultiballs = 4        ' max number of balls during multiballs

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim PlayfieldMultiplier(4)
Dim PFxSeconds
Dim BallSaverTime      ' in seconds
Dim SkillshotTime      ' in seconds
Dim SuperSkillshotTime ' in seconds

Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot(4)
Dim SuperJackpot(4)
Dim Skillshot(4)
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode
Dim x, j

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim LockedBalls
Dim BallsInHole

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMultiBallStarted
Dim bMusicOn
Dim bSkillshotActive
Dim bSkillshotReady
Dim bExtraBallLit
Dim bExtraBallWonThisBall
Dim bJackpot
Dim bSongSelect

'debug variables
Dim logging
logging = False

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM

    Randomize

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 45 ' Plunger Power
    Const IMTime = 0.5        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Credits = 0
    Loadhs

    ' Initalise the DMD display
    DMD_Init

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    bMultiBallStarted = False
    PFxSeconds = 0
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    LockedBalls = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bBonusHeld = False
    bJackpot = False
    bInstantInfo = False
    BallsOnPlayfield = 0
    bExtraBallLit = False
    LastSwitchHit = ""

    ' set any lights for the attract mode
    vpmtimer.addtimer 2000, "GiOn '"
    StartAttractMode
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
        RotateLightsleft
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
        RightFlipper2.RotateToEnd
        RightFlipperOn = 1
        RotateLightsRight
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipper2.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper2_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper_Animate
    LeftFlipperTop.Rotz = LeftFlipper.CurrentAngle
End Sub

Sub RightFlipper_Animate
    RightFlipperTop.Rotz = RightFlipper.CurrentAngle
End Sub

Sub RightFlipper2_Animate
    RightFlipperTop2.Rotz = RightFlipper2.CurrentAngle
End Sub

'********************************
' Real Time Flipper adjustments
' by JLouLouLou & JPSalas
'      Version 5.0
'********************************

Dim FlipperElasticity
Dim FullStrokeEOS_Torque, LiveStrokeEOS_Torque
Dim LeftFlipperOn
Dim RightFlipperOn

Dim LLiveCatchTimer
Dim RLiveCatchTimer
Dim LiveCatchSensivity

FlipperElasticity = 0.9
FullStrokeEOS_Torque = 0.9 ' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.3 ' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

LiveCatchSensivity = 10

LLiveCatchTimer = 0
RLiveCatchTimer = 0

LeftFlipper.TimerInterval = 1
LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's tricks timer

    'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
    If LeftFlipperOn = 1 Then
        If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
            LeftFlipper.EOSTorque = FullStrokeEOS_Torque
            LLiveCatchTimer = LLiveCatchTimer + 1
            If LLiveCatchTimer <LiveCatchSensivity Then
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

    'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
    If RightFlipperOn = 1 Then
        If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
            RightFlipper.EOSTorque = FullStrokeEOS_Torque
            RLiveCatchTimer = RLiveCatchTimer + 1
            If RLiveCatchTimer <LiveCatchSensivity Then
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


'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    If bSelectModeActive Then
        SelectMode(keycode)
        Exit Sub
    End If

    If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound "fx_nudge", 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 6:PlaySound "fx_nudge", 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound "fx_nudge", 0, 1, 1, 0.25
    If keycode = MechanicalTilt Then CheckTilt

    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
        Credits = Credits + 1
        dof 125, dofon
        If(Tilted = False) Then
            DMDFlush
            DMD "", CL("CREDITS " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then
        ' Action Button
        If keycode = RightMagnaSave or keycode = LockBarKey Then
        'ActionButtonActivated
        End If
        If keycode = LeftTiltKey Then CheckTilt 'only check the tilt during game
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                        If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, ""
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetForNewGame()
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, ""
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If hsbModeActive Then
        Exit Sub
    End If

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
        DOF 147, DOFpulse
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
    End If
End Sub

Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = False
    If NOT hsbModeActive Then
        bInstantInfo = True
        DMDFlush
        InstantInfo
    End If
End Sub

'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub Table1_Exit
    Savehs
    If UseFlexDMD then
        If Not FlexDMD is Nothing Then
            FlexDMD.Show = False
            FlexDMD.Run = False
            FlexDMD = NULL
        End if
    End if
    If B2SOn = true Then Controller.Stop
End Sub


'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt 'Called when table is nudged
    Dim BOT
    BOT = GetBalls
    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub
    Tilt = Tilt + TiltSensitivity                  'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <= 15) Then 'show a warning
        DMD "_", CL("CAREFUL"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
        PlaySound "v_danger01", , VoiceVolume
    End if
    If(NOT Tilted) AND Tilt> 15 Then 'If more that 15 then TILT the table
        'display Tilt
        InstantInfoTimer.Enabled = False
        DMDFlush
        DMD CL("YOU"), CL("TILTED"), "", eNone, eNone, eNone, 3000, True, ""
        PlaySound "v_danger02", , VoiceVolume
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
        bMultiBallMode = False
        StopMBmodes
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
        Tilted = True
        'turn off GI and turn off all the lights
        GiOff
        TurnOFF aLights
        'LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        'Bumper001.Threshold = 100
        'Bumper002.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        StopSong
    Else
        Tilted = False
        'turn back on GI and the lights
        GiOn
        'LightSeqTilt.StopPlay
        'Bumper001.Threshold = 1
        'Bumper002.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        DMDFlush
        Song = ""
        PlayModeSong
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield - LockedBalls = 0) Then
        bMultiBallMode = False
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        vpmtimer.Addtimer 2000, "EndOfBall '"
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'*****************************************
'       Music as internal sounds
'*****************************************

Dim Song, Songnr
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

Sub StopSong:StopSound Song:End Sub

Sub PlayModeSong
    Select Case ModeActive
        Case 0:PlaySong "m_main"
        Case 1:PlaySong "m_purple"
        Case 2:PlaySong "m_yellow"
        Case 3:PlaySong "m_green"
        Case 4:PlaySong "m_orange"
        Case 5:PlaySong "m_racemb01"
        Case 6:PlaySong "m_turbomb"
        Case 7:PlaySong "m_rampmb01"
        Case 8:PlaySong "m_finalrace"
    End Select
End Sub

'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim GiIntensity
GiIntensity = 1   'can be used for the LUT changing to increase the GI lights when the table is darker

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub ChangeGIIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGILights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

Sub GiOn
    PlaySound "fx_GiOn"
    Dim bulb
    For each bulb in aGiLights
        FlashForMs bulb, 300, 40, 1
    Next
End Sub

Sub GiOff
    PlaySound "fx_GiOff"
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    LightSeqGi.StopPlay
    Select Case n
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 40
            LightSeqGi.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqGi.UpdateInterval = 25
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 40
            LightSeqGi.Play SeqBlinking, , 10, 20
    End Select
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
        Case 7 'rotate
            LightSeqInserts.UpdateInterval = 2
            LightSeqInserts.Play SeqScrewRightOn, 50, 8
    End Select
End Sub

Sub FlashEffect(n)
    Select Case n
        Case 1 'all blink
            LightSeqFlashers.UpdateInterval = 40
            LightSeqFlashers.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqFlashers.UpdateInterval = 100
            LightSeqFlashers.Play SeqRandom, 50, , 3000
        Case 3 'all blink fast
            LightSeqFlashers.UpdateInterval = 20
            LightSeqFlashers.Play SeqBlinking, , 10, 20
        Case 4 'center out
            LightSeqFlashers.UpdateInterval = 10
            LightSeqFlashers.Play SeqCircleOutOn, 15, 1
        Case 5 'top down
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqDownOn, 15, 2
        Case 6 'down to top
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqUpOn, 15, 2
        Case 7 'rotate
            LightSeqFlashers.UpdateInterval = 2
            LightSeqFlashers.Play SeqScrewRightOn, 50, 8
        Case 8 'random fast
            LightSeqFlashers.UpdateInterval = 100
            LightSeqFlashers.Play SeqRandom, 50, , 300
    End Select
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

Sub TurnBlink(Col) 'blink all the lights in a collection
    Dim i
    For each i in Col
        i.BlinkInterval = 125
        i.State = 2
    Next
End Sub

Sub TurnBlinkSlow(Col) 'blink all the lights in a collection
    Dim i
    For each i in Col
        i.BlinkInterval = 300
        i.State = 2
    Next
End Sub

Sub TurnBlinkFaster(Col) 'blink all the lights in a collection
    Dim i
    For each i in Col
        i.BlinkInterval = 50
        i.State = 2
    Next
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
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 200
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
    PlaySound soundname, 0, 1, Pan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls, 20 balls, from 0 to 19
Const lob = 1     'number of locked balls, 1st ball is the helmet animation
Const maxvel = 46 'max ball velocity
ReDim rolling(tnob)

Dim MyPi
MyPi = Round(4 * Atn(1), 6) / 90
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
    RollingTimer.Enabled = 1
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
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
                ballvol = Vol(BOT(b) )
            ElseIf BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 35000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 2
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
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

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'************************************
' Diverse Collection Hit Sounds v4.0
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
Sub aDroptargets_Hit(idx):PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, Pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall):End Sub
Sub aTargets_Hit(idx):PlaySound SoundFX("fx_target", DOFTargets), 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall):End Sub
Sub aTriggers_Hit(idx):PlaySoundAt "fx_sensor", aTriggers(idx):End Sub

' *********************************************************************
'User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

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

    ' initialise any other flags
    Tilt = 0

    ' initialise specific Game variables
    Game_Init()

    ' you may wish to start some music, play a sound, do whatever at this point
    ' PlaySound "vo_start" &RndNbr(3)

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
    DMDScoreNow

    ' set the current players bonus multiplier back down to 1X
    BonusMultiplier(CurrentPlayer) = 1

    ' reset any drop targets, lights, game Mode etc..

    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False

    'Reset any table specific
    ResetNewBallVariables

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

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
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4
    if logging then debug.print("Autoplunge create yes/no: " = CStr(BallsOnPlayfield-LockedBalls) )
' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield-LockedBalls> 1 then
        bMultiBallMode = True
        bAutoPlunger = True
    Else
        bMultiBallMode = False
        bAutoPlunger = False
    End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
    'and eject the first ball
    CreateMultiballTimer_Timer
    bAutoPlunger = True
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
    PlaySound "v_outlane0" & RndNbr(4), , VoiceVolume
    Dim AwardPoints, TotalBonus, ii, tmp
    AwardPoints = 0
    TotalBonus = 10 'yes 10 points :)
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False
    GiOff
    ' only process any of this if the table is not tilted.
    '(the tilt recovery mechanism will handle any extra balls or end of game)

    If NOT Tilted Then
        ' PlaySong "m_bonus"
        'Count bonus. This table uses several bonus
        DMD CL("BONUS"), "", "", eBlink, eNone, eNone, 1000, True, ""

        'Orbit hits
        AwardPoints = BonusOrbits * 100000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(FormatScore(AwardPoints) ), CL("ORBIT HITS " & BonusOrbits), "", eBlink, eNone, eNone, 1000, True, ""

        'Ramp hits
        AwardPoints = BonusRamps * 100000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(FormatScore(AwardPoints) ), CL("RAMP HITS " & BonusRamps), "", eBlink, eNone, eNone, 1000, True, ""

        'Helmet hits
        AwardPoints = BonusHelmet * 100000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(FormatScore(AwardPoints) ), CL("HELMET HITS " & BonusHelmet), "", eBlink, eNone, eNone, 1000, True, ""

        'Switch hits
        AwardPoints = BonusSwitches * 100000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(FormatScore(AwardPoints) ), CL("SWITCH HITS " & BonusSwitches), "", eBlink, eNone, eNone, 1000, True, ""

        'Modes Started
        AwardPoints = BonusModes * 250000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(FormatScore(AwardPoints) ), CL("MODES STARTED " & BonusModes), "", eBlink, eNone, eNone, 1000, True, ""

        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        DMD CL("TOTAL BONUS X " &BonusMultiplier(CurrentPlayer) ), CL(FormatScore(TotalBonus) ), "", eNone, eNone, eNone, 2000, True, ""
        Score(CurrentPlayer) = Score(CurrentPlayer) + TotalBonus
        LightEffect 4

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 8000, "EndOfBall2 '"
    Else 'if tilted then only add a short delay and move to the 2nd part of the end of the ball
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
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If ExtraBallsAwards(CurrentPlayer)> 0 Then
        if logging then debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's then turn off any Extra Ball light if there was any
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        If bTurboJustFinished Then
            if logging then debug.Print "Tubo mb finished"
            if logging then debug.Print "Old mode " & OldMode
            ResumeMode
            AddMultiball 1 'create a new ball and use auto plunger
            bTurboJustFinished = False
        Else
            DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, ""
            ResetForNewPlayerBall()
            ' Create a new ball in the shooters lane
            CreateNewBall()
        End If
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
            if logging then debug.print "No More Balls, High Score Entry"
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
' (or high score entry finished) AND it either ends the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer
    if logging then debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame> 1) Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer> PlayersPlayingGame) Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    if logging then debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode

        ' set the machine into game over mode
        EndOfGame()

    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        DMDScoreNow

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            Select Case CurrentPlayer
                Case 1:DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, "v_player1"
                Case 2:DMD "", CL("PLAYER 2"), "", eNone, eNone, eNone, 1000, True, "v_player2"
                Case 3:DMD "", CL("PLAYER 3"), "", eNone, eNone, eNone, 1000, True, "v_player3"
                Case 4:DMD "", CL("PLAYER 4"), "", eNone, eNone, eNone, 1000, True, "v_player4"
            End Select
        Else
            DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, ""
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()

    if logging then debug.print "End Of Game"

    bGameInPLay = False
    ' just ended your game then play the end of game tune

    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Modes - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball
    ReleaseLockedBalls

    ' set any lights for the attract mode
    GiOff
    StartAttractMode
' you may wish to light any Game Over Light you may have
End Sub

'this calculates the ball number in play
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
    if logging then debug.print "Ball Drain"
    ' Destroy the ball
    Drain.DestroyBall

    If BallsOnPlayfield> 0 Then
        BallsOnPlayfield = BallsOnPlayfield - 1
        if logging then debug.print "Balls on Playfield: " & Cstr(BallsOnPlayfield)
    End If

    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain

    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball

    'if Tilted the end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then
            if logging then debug.print "Ball Saved!"
            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            ' stop the ballsaver timer during the launch ball saver time, but not during multiballs
            If NOT bMultiBallMode Then
                DMD "_", CL("BALL SAVED"), "_", eNone, eBlinkfast, eNone, 2500, True, "" 'BallSaverTime = 0 'if you want to stop the ball saver
            End If
        Else
            if logging then debug.print "No Ballsaver"
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1) Then
                ' AND in a multi-ball??
                If bMultiBallMode then
                    if logging then debug.print "Multiball Over"
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
                    ' ChangeGIIntensity 1
                    ' ChangeGi white
                    'stop any multiball modes of this game
                    StopMBmodes
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield - LockedBalls <= 0) Then
                ' End Mode and timers
                if logging then debug.print "Ball Drained"
                ChangeGIIntensity 1
                ChangeGi white
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, if there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
    if logging then debug.print "ball in plunger lane"
    ' some sound according to the ball position
    PlaySoundAt "fx_sensor", swPlungerRest
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    'LaunchLight.State = 2
    ' be sure to update the Scoreboard after the animations, if any
    ' if the ball goes into the plunger lane during a multiball then activate the autoplunger
    If bMultiBallMode Then
        bAutoPlunger = True ' kick the ball in play if the bAutoPlunger flag is on
    End If
    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        if logging then debug.print "autofire the ball"
        bSkillShotReady = false
        vpmtimer.addtimer 1500, "PlungerIM.AutoFire:DOF 120, DOFPulse:PlaySoundAt ""fx_kicker"", swPlungerRest '"
    End If
    'Start the Selection of the song and skillshot if ready
    If bSkillShotReady Then
        SkillShotHits = 0
        UpdateSkillShot 4                                          'first skillshot
        PlaySoundAt "fx_diverter", Diverterf:Diverterf.RotateToEnd 'to enable several skillshots
    End If
' show the message to shoot the ball in case the player has fallen sleep
'swPlungerRest.TimerEnabled = 1
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    lighteffect 6

    'swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active

    ' if there is a need for a ball saver and skill shot, then start off the timers
    ' only start if it is ready+

    If(bBallSaverReady = True) Then
        EnableBallSaver 10
    End If
    If(bSkillshotReady = True) Then
        ResetSkillShotTimer.Enabled = 1
    End If
    ' turn off LaunchLight
    ' LaunchLight.State = 0
    If NOT bAutoPlunger Then PlaySound "s_01acc", , SFXVolume
End Sub

Sub AutoPLungerOFFSw_Hit:bAutoPlunger = False:bBallInPlungerLane = False:LastSwitchHit = "AutoPLungerOFFSw":End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball after a while
Sub swPlungerRest_Timer
'PlaySound "wakeup"
End Sub

'*********************
' Skillshot Timer routines
'*********************

' This is used to start the Skillshottimer and to add extra seconds

Sub UpdateSkillShot(seconds) 'Setup and updates the skillshot lights
    If logging Then debug.print "Skillshot Enabled"
    TurnOFF aLights
    DMD CL("HIT LIT LIGHT"), CL("FOR SKILLSHOT"), "", eNone, eNone, eNone, 3000, True, ""
    l030.State = 2
    bSkillshotActive = true
    SkillShotTime = seconds
'ResetSkillShotTimer.Enabled = 1
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    If SkillShotTime> 0 Then SkillShotTime = SkillShotTime -1
    If logging Then debug.print("STime: " & CStr(SkillShotTime) )
    If SkillShotTime = 0 Then ' stop the Lights
        ResetSkillShotTimer.Enabled = 0
        l030.State = 0
        UpdateModeLights
        bSkillShotReady = false
        bSkillshotActive = false
        PlaySoundAt "fx_diverter", Diverterf:Diverterf.RotateToStart
        ' Prepare Selection of main mode
        If bShootScoopPlayed = False Then PlaySound "v_01scoop", , VoiceVolume:bShootScoopPlayed = True
        F7.TimerInterval = 100
        F7.BlinkPattern = 10000000000
        ModeActive = 0
        ResumeMode
    Else
        DMDScoreNow
    End If
End Sub

'*********************
' Ball Saver routines
'*********************

' This is used to start the ballsaver and to add extra seconds

Sub EnableBallSaver(seconds)
    ' do not start the timer if extra ball has been awarded
    If ExtraBallsAwards(CurrentPlayer)> 0 Then
        BallSaverTimer.Enabled = False
        LightShootAgain.State = 1
        Exit Sub
    End If
    if logging then debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' restart the timer
    BallSaverTime = BallSaverTime + seconds
    BallSaverTimer.Enabled = False
    BallSaverTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
End Sub

' The ball saver timer counts seconds
'
Sub BallSaverTimer_Timer()
    BallSaverTime = BallSaverTime -1
    If BallSaverTime = 3 Then 'speed up the light
        LightShootAgain.BlinkInterval = 80
        LightShootAgain.State = 2
    End If
    If BallSaverTime = 0 Then ' stop the Light
        LightShootAgain.State = 0
    End If
    ' give 1 second grace period before stopping it

    If BallSaverTime <0 Then
        BallSaverTimer.Enabled = False
        bBallSaverActive = False
        BallSaverTime = 0
        if logging then debug.print "Ballsaver ended"
        ' if you have a ball saver light then turn it off at this point
        LightShootAgain.State = 0
        ' if the table uses the same lights for the extra ball or replay then turn them on if needed
        If ExtraBallsAwards(CurrentPlayer)> 0 Then
            LightShootAgain.State = 1
        End If
        If ModeActive = 8 Then CheckMode:End If 'the 60 seconds is finished so go to the next step
    End If
End Sub


' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board

Sub AddScore(points) 'normal score routine; points x playfieldmultiplier
    If Tilted Then Exit Sub
    if bSkillshotActive Then
        if logging then debug.print "Skillshot disabled by scoring"
        bSkillShotReady = false
        bSkillshotActive = false
        SkillshotTime = 0
    End if

    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If Tilted Then Exit Sub
    ' add the bonus to the current players bonus variable
    BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
End Sub

Sub AddBonusMultiplier(n)
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxBonusMultiplier) then
        ' then add and set the lights
        BonusMultiplier(CurrentPlayer) = BonusMultiplier(CurrentPlayer) + n
        DMD "_", CL("BONUS X " & BonusMultiplier(CurrentPlayer) ), "_", eNone, eBlink, eNone, 2000, True, ""
        GiEffect 1
    End if
End Sub

Sub ExtraBallIsLit
    DMD "_", CL("EXTRA BALL IS LIT"), "", eNone, eNone, eNone, 1500, True, ""
    PlaySound "v_extralit", , VoiceVolume
    bExtraBallLit = True
    l017.State = 2
End Sub

Sub AwardExtraBall()
    '    If NOT bExtraBallWonThisBall Then 'just one extra ball per ball
    DMD "_", CL("EXTRA BALL WON"), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
    PlaySound "v_extrawin", , VoiceVolume
    'bExtraBallWonThisBall = True
    LightShootAgain.State = 1 'light the shoot again lamp
    GiEffect 3
    LightEffect 2
'    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL("EXTRA GAME WON"), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    Credits = Credits + 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
    GiEffect 3
End Sub

Sub AwardJackpot()
    DMD CL("JACKPOT"), CL(FormatScore(Jackpot(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1500, True, ""
    DOF 126, DOFPulse
    vpmTimer.AddTimer 200, "PlaySound""v_jack0""&RndNbr(4),,VoiceVolume '"
    AddScore Jackpot(CurrentPlayer)
    LightEffect 2
    GiEffect 3
    FlashEffect 3
End Sub

Sub AwardSuperJackpot()
    Dim tmp
    DMD CL("SUPER JACKPOT"), CL(FormatScore(SuperJackpot(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 2000, True, ""
    DOF 126, DOFPulse
    AddScore SuperJackpot(CurrentPlayer)
    LightEffect 2
    GiEffect 3
End Sub

Sub AwardSkillshot()
    'show dmd animation
    DMD CL("SKILLSHOT"), CL(FormatScore(Skillshot(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 2000, True, ""
    AddScore Skillshot(CurrentPlayer)
    'do some light show
    DOF 127, DOFPulse
    GiEffect 3
    LightEffect 2
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(MyTable, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 1500000 End If
    x = LoadValue(MyTable, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(MyTable, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 1400000 End If
    x = LoadValue(MyTable, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(MyTable, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 1300000 End If
    x = LoadValue(MyTable, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(MyTable, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 1200000 End If
    x = LoadValue(MyTable, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(MyTable, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0
    x = LoadValue(MyTable, "TotalGamesPlayed")
    If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
    SaveValue MyTable, "HighScore1", HighScore(0)
    SaveValue MyTable, "HighScore1Name", HighScoreName(0)
    SaveValue MyTable, "HighScore2", HighScore(1)
    SaveValue MyTable, "HighScore2Name", HighScoreName(1)
    SaveValue MyTable, "HighScore3", HighScore(2)
    SaveValue MyTable, "HighScore3Name", HighScoreName(2)
    SaveValue MyTable, "HighScore4", HighScore(3)
    SaveValue MyTable, "HighScore4Name", HighScoreName(3)
    SaveValue MyTable, "Credits", Credits
    SaveValue MyTable, "TotalGamesPlayed", TotalGamesPlayed
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

    If tmp> HighScore(0) Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
    'DOF 125, DOFOn
    End If

    If tmp> HighScore(3) Then
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
    ' PlaySound "vo_greatscore"
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
        playsound "menu_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "menu_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then
            playsound "menu_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "menu_Enter"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit> 0) then
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
    dLine(0) = ExpandLine(TempTopStr)
    DMDUpdate 0

    TempBotStr = "    > "
    if(hsCurrentDigit> 0) then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempBotStr = TempBotStr & "_"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    dLine(1) = ExpandLine(TempBotStr)
    DMDUpdate 1
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2) then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False

    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") then
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
            If HighScore(j) <HighScore(j + 1) Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
    Savehs
End Sub

' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 1st and 2nd lines are 20 characters long
' 3rd line is just 1 character
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
Dim dmdLine2
dim DMDModeCounter

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
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_border")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
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
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_border")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
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
    Else
        digitgrid.Visible = True
        For i = 0 to 40
            Digits(i).Visible = True
        Next
    End If
    Dim i, j
    DMDFlush()
    deSpeed = 20
    deBlinkSlowRate = 10
    deBlinkFastRate = 5
    For i = 0 to 2
        dLine(i) = Space(20)
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
    dLine(2) = " "
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
    Dim tmp, tmp1, tmp1a, tmp1b, tmp2
    if(dqHead = dqTail) Then
        'tmp is the top line, mostly the score
        'tmp1 is the info on the second line: tmp1
        tmp = RL(FormatScore(Score(Currentplayer) ) )
        tmp1 = FL("PLAYER " &CurrentPlayer, "BALL " & Balls)
        Select Case ModeActive
            Case 0 'no Mode active
            Case 1:
                tmp = FL("DANICA", FormatScore(Score(Currentplayer) ) )
                Select Case M1Step(CurrentPlayer)
                    Case 0, 4
                        tmp1 = "SHOOT THE DROPTARGET"
                    Case 1, 2, 3
                        tmp1 = CL("SHOOT ORBITS " & M1Step(CurrentPlayer) -1&"/3")
                End Select
            Case 2
                tmp = FL("JUAN", FormatScore(Score(Currentplayer) ) )
                tmp1 = CL("LEFT " & M2LeftHits(CurrentPlayer) & "/3  RIGHT " & M2RightHits(CurrentPlayer) & "/3")
            Case 3
                tmp = FL("SCIOTTI", FormatScore(Score(Currentplayer) ) )
                tmp1 = CL("COMBOS DONE " & M3Combos(CurrentPlayer) & "/5")
            Case 4
                tmp = FL("FED", FormatScore(Score(Currentplayer) ) )
                tmp1 = CL("TARGET HITS " & M4TargetHits(CurrentPlayer) & "/10")
            Case 5
                tmp = FL("RACE MB", FormatScore(Score(Currentplayer) ) )
                tmp1 = CL("SHOOT THE JACKPOTS")
            Case 6
                tmp = FL("TURBO MB", FormatScore(Score(Currentplayer) ) )
                tmp1 = FL("SECONDS LEFT", ModeSecondsLeft)
            Case 7:tmp = FL("RAMP MB", FormatScore(Score(Currentplayer) ) )
                Select Case ModeStep
                    Case 1
                        tmp1 = "SHOOT THE LEFT RAMP"
                    Case 2
                        tmp1 = "SHOOT THE RIGHT RAMP"
                    Case 3
                        tmp1 = "SHOOT L AND R RAMPS"
                End Select
            Case 8
                tmp = FL("FINAL RACE", FormatScore(Score(Currentplayer) ) )
                Select Case ModeStep
                    Case 1
                        tmp1 = "SHOOT L RAMP  SEC " &BallSaverTime
                    Case 2
                        tmp1 = "HIT LIGHTS    SEC " &BallSaverTime
                    Case 3
                        tmp1 = "HIT HELMET    SEC " &BallSaverTime
                End Select
        End Select

        'background image
        Select Case ModeActive
            Case 0:tmp2 = "d_border" 'no Mode active
            Case 1:tmp2 = "d_danica2"
            Case 2:tmp2 = "d_juan2"
            Case 3:tmp2 = "d_sciotti2"
            Case 4:tmp2 = "d_fed2"
            Case 5:tmp2 = "d_border"
            Case 6:tmp2 = "d_border"
            Case 7:tmp2 = "d_border"
            Case 8:tmp2 = "d_border"
        End Select
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 10, True, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail <dqSize) Then
        if(Text0 = "_") Then
            dqEffect(0, dqTail) = eNone
            dqText(0, dqTail) = "_"
        Else
            dqEffect(0, dqTail) = Effect0
            dqText(0, dqTail) = ExpandLine(Text0)
        End If

        if(Text1 = "_") Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1)
        End If

        if(Text2 = "_") Then
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
        if(dqTail = 1) Then
            DMDHead()
        End If
    End If
End Sub

Sub DMDHead()
    Dim i
    deCount(0) = 0
    deCount(1) = 0
    deCount(2) = 0

    For i = 0 to 2
        Select Case dqEffect(i, dqHead)
            Case eNone:deCountEnd(i) = 1
            Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead) )
            Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead) )
            Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
            Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
        End Select
    Next
    if(dqSound(dqHead) <> "") Then
        PlaySound(dqSound(dqHead) )
    End If
    DMDEffectTimer.Interval = deSpeed
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
    if(dqHead = dqTail) Then
        if(dqbFlush(Head) = True) Then
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
        if(deCount(i) <> deCountEnd(i) ) Then
            deCount(i) = deCount(i) + 1

            select case(dqEffect(i, dqHead) )
                case eNone:
                    Temp = dqText(i, dqHead)
                case eScrollLeft:
                    Temp = Right(dLine(i), 19)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), 21 - deCount(i), 1)
                    Temp = Temp & Left(dLine(i), 19)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkSlowRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If

                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(20)
                        If i = 2 then
                            Temp = ""
                        End If
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkFastRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If

                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(20)
                        If i = 2 then
                            Temp = ""
                        End If
                    End If
                case eLongScrollLeft:
                    Temp = Right(dLine(i), 19)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
            End Select

            if(dqText(i, dqHead) <> "_") Then
                dLine(i) = Temp
                DMDUpdate i
            End If
        End If
    Next

    if(deCount(0) = deCountEnd(0) ) and(deCount(1) = deCountEnd(1) ) and(deCount(2) = deCountEnd(2) ) Then

        if(dqTimeOn(dqHead) = 0) Then
            DMDFlush()
        Else
            if(BlinkEffect = True) Then
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

Function ExpandLine(TempStr)
    If TempStr = "" Then
        TempStr = Space(20)
    Else
        if Len(TempStr)> Space(20) Then
            TempStr = Left(TempStr, Space(20) )
        Else
            if(Len(TempStr) <20) Then
                TempStr = TempStr & Space(20 - Len(TempStr) )
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

    NumString = CStr(abs(Num) )

    For i = Len(NumString) -3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1) ) then
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1) ) + 128) & right(NumString, Len(NumString) - i)
        end if
    Next
    FormatScore = NumString
End function

Function FL(NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    If Len(NumString1) + Len(NumString2) <20 Then
        Temp = 20 - Len(NumString1) - Len(NumString2)
        TempStr = NumString1 & Space(Temp) & NumString2
        FL = TempStr
    End If
End Function

Function CL(NumString) 'center line
    Dim Temp, TempStr
    If Len(NumString)> 20 Then NumString = Left(NumString, 20)
    Temp = (20 - Len(NumString) ) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(NumString) 'right line
    Dim Temp, TempStr
    If Len(NumString)> 20 Then NumString = Left(NumString, 20)
    Temp = 20 - Len(NumString)
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
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "d_border"
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

'************************************
'    JP's new DMD using flashers
' two text lines and 1 backdrop image
'************************************

Dim Digits, Chars(255), Images(255)

DMDInit

Sub DMDInit
    Dim i
    Digits = Array(digit001, digit002, digit003, digit004, digit005, digit006, digit007, digit008, digit009, digit010, _
        digit011, digit012, digit013, digit014, digit015, digit016, digit017, digit018, digit019, digit020,            _
        digit021, digit022, digit023, digit024, digit025, digit026, digit027, digit028, digit029, digit030,            _
        digit031, digit032, digit033, digit034, digit035, digit036, digit037, digit038, digit039, digit040,            _
        digit041)
    For i = 0 to 255:Chars(i) = "d_empty":Next

    Chars(32) = "d_empty" 'space
    'Chars(33) = ""        '!
    'Chars(34) = ""        '"
    'Chars(35) = ""        '#
    'Chars(36) = ""        '$
    'Chars(37) = ""        '%
    'Chars(38) = ""        '&
    'Chars(39) = ""        ''
    'Chars(40) = ""        '(
    'Chars(41) = ""        ')
    Chars(42) = "d_star" '*
    'Chars(43) = ""        '+
    Chars(44) = "d_comma" ',
    Chars(45) = "d_minus" '-
    Chars(46) = "d_dot"   '.
    Chars(47) = "d_div"   '/
    Chars(48) = "d_0"     '0
    Chars(49) = "d_1"     '1
    Chars(50) = "d_2"     '2
    Chars(51) = "d_3"     '3
    Chars(52) = "d_4"     '4
    Chars(53) = "d_5"     '5
    Chars(54) = "d_6"     '6
    Chars(55) = "d_7"     '7
    Chars(56) = "d_8"     '8
    Chars(57) = "d_9"     '9
    Chars(60) = "d_less"  '<
    'Chars(61) = ""        '=
    Chars(62) = "d_more" '>
    'Chars(64) = ""        '@
    Chars(65) = "d_a" 'A
    Chars(66) = "d_b" 'B
    Chars(67) = "d_c" 'C
    Chars(68) = "d_d" 'D
    Chars(69) = "d_e" 'E
    Chars(70) = "d_f" 'F
    Chars(71) = "d_g" 'G
    Chars(72) = "d_h" 'H
    Chars(73) = "d_i" 'I
    Chars(74) = "d_j" 'J
    Chars(75) = "d_k" 'K
    Chars(76) = "d_l" 'L
    Chars(77) = "d_m" 'M
    Chars(78) = "d_n" 'N
    Chars(79) = "d_o" 'O
    Chars(80) = "d_p" 'P
    Chars(81) = "d_q" 'Q
    Chars(82) = "d_r" 'R
    Chars(83) = "d_s" 'S
    Chars(84) = "d_t" 'T
    Chars(85) = "d_u" 'U
    Chars(86) = "d_v" 'V
    Chars(87) = "d_w" 'W
    Chars(88) = "d_x" 'X
    Chars(89) = "d_y" 'Y
    Chars(90) = "d_z" 'Z
    'Chars(94) = ""        '^
    'Chars(95) = "" '_
    'Chars(96) = ""
    'Chars(97) = ""  'a
    'Chars(98) = ""  'b
    'Chars(99) = ""  'c
    'Chars(100) = "" 'd
    'Chars(101) = "" 'e
    'Chars(102) = "" 'f
    'Chars(103) = "" 'g
    'Chars(104) = "" 'h
    'Chars(105) = "" 'i
    'Chars(106) = "" 'j
    'Chars(107) = "" 'k
    'Chars(108) = "" 'l
    'Chars(109) = "" 'm
    Chars(110) = "d_na" 'n red N
    'Chars(111) = "" 'o
    'Chars(112) = "" 'p
    'Chars(113) = "" 'q
    'Chars(114) = "" 'r
    'Chars(115) = "" 's
    'Chars(116) = "" 't
    'Chars(117) = "" 'u
    Chars(118) = "d_va" 'v red V
    'Chars(119) = "" 'w
    'Chars(120) = "" 'x
    'Chars(121) = "" 'y
    'Chars(122) = "" 'z
    'Chars(123) = "" '{
    'Chars(124) = "" '|
    'Chars(125) = "" '}
    'Chars(126) = "" '~
    'used in the FormatScore function
    Chars(176) = "d_0a" '0.
    Chars(177) = "d_1a" '1.
    Chars(178) = "d_2a" '2.
    Chars(179) = "d_3a" '3.
    Chars(180) = "d_4a" '4.
    Chars(181) = "d_5a" '5.
    Chars(182) = "d_6a" '6.
    Chars(183) = "d_7a" '7.
    Chars(184) = "d_8a" '8.
    Chars(185) = "d_9a" '9.
End Sub


'************************************************************************************************************************
' Only for VPX 10.8 and higher.
' FlashForMs will blink light for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Blink, -1 Return to original state
' To blink a flasher you need to link it to a light, this will fade the flasher just like the light
'************************************************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState)
    If FinalState = -1 Then
        FinalState = MyLight.State
    End If
    MyLight.BlinkInterval = BlinkPeriod
    MyLight.Duration 2, TotalPeriod, FinalState
End Sub


'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 11 colors: red, orange, amber, yellow...
'******************************************

'colors
Const yellow = 1
Const green = 2
Const darkgreen = 3
Const blue = 4
Const darkblue = 5
Const purple = 6
Const red = 7

Const orange = 8
Const amber = 9
Const teal = 10
Const white = 11

Sub SetLightColor(n, col, stat) 'stat 0 = off, 1 = on, 2 = blink, -1= no change
    Select Case col
        Case red
            n.color = RGB(255, 0, 0)
            n.colorfull = RGB(255, 0, 0)
        Case orange
            n.color = RGB(255, 64, 0)
            n.colorfull = RGB(255, 64, 0)
        Case amber
            n.color = RGB(255, 153, 0)
            n.colorfull = RGB(255, 153, 0)
        Case yellow
            n.color = RGB(255, 255, 0)
            n.colorfull = RGB(255, 255, 0)
        Case darkgreen
            n.color = RGB(0, 64, 0)
            n.colorfull = RGB(0, 64, 0)
        Case green
            n.color = RGB(0, 128, 0)
            n.colorfull = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 255, 255)
            n.colorfull = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 64, 64)
            n.colorfull = RGB(0, 64, 64)
        Case purple
            n.color = RGB(128, 0, 192)
            n.colorfull = RGB(128, 0, 192)
        Case teal
            n.color = RGB(2, 128, 126)
            n.colorfull = RGB(2, 128, 126)
        Case white
            n.color = RGB(255, 252, 224)
            n.colorfull = RGB(255, 252, 224)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

Sub SetFlashColor(n, col, stat) 'Flashers are linked to lights in VPX8
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
        Case white
            n.color = RGB(255, 197, 143)
        Case teal
            n.color = RGB(2, 128, 126)
    End Select
End Sub

'*************************
' Rainbow Changing Lights
'*************************

Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

Sub StartRainbow(n) 'n is a collection
    set RainbowLights = n
    RGBStep = 0
    RGBFactor = 5
    rRed = 255
    rGreen = 0
    rBlue = 0
    RainbowTimer.Enabled = 1
End Sub

Sub StopRainbow()
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
        obj.color = RGB(rRed, rGreen, rBlue)
        obj.colorfull = RGB(rRed, rGreen, rBlue)
    Next
End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim ii
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1) Then
        DMD CL("LAST SCORE"), CL("PLAYER 1 " &FormatScore(Score(1) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2) Then
        DMD CL("LAST SCORE"), CL("PLAYER 2 " &FormatScore(Score(2) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3) Then
        DMD CL("LAST SCORE"), CL("PLAYER 3 " &FormatScore(Score(3) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4) Then
        DMD CL("LAST SCORE"), CL("PLAYER 4 " &FormatScore(Score(4) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    DMD "", CL("GAME OVER"), "", eNone, eBlink, eNone, 2000, False, ""
    If bFreePlay Then
        DMD "", CL("FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits> 0 Then
            DMD CL("CREDITS " & Credits), CL("PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD CL("HASSANCHOP"), CL("AND JPSALAS"), "d_border", eNone, eNone, eNone, 3000, False, ""
    DMD "", CL("PRESENTS"), "d_border", eNone, eNone, eNone, 2000, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 4000, False, ""
    DMD "", CL("ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD "DEDICATED TO THE ME-", "MORY OF ENZO SCIOTTI", "d_border", eNone, eNone, eNone, 3000, False, ""
    DMD CL("HIGHSCORES"), Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL("HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL("HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    LightSeqInserts.StopPlay
    StartLightSeq
    ' StartRainbow aWhiteLights
    DMDFlush
    ShowTableInfo
End Sub

Sub StopAttractMode
    DMDScoreNow
    LightSeqAttract.StopPlay
    StopRainbow
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 15
    LightSeqAttract.Play SeqCircleInOn, 40, 1
    LightSeqAttract.UpdateInterval = 2
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqCircleOutOn, 25, 4
    LightSeqAttract.UpdateInterval = 4
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.UpdateInterval = 4
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.Play SeqUpOn, 25, 1, 500
    LightSeqAttract.UpdateInterval = 4
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.Play SeqUpOn, 25, 1, 500
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqCircleOutOn, 25, 4
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe2VertOn, 50, 4
    LightSeqAttract.UpdateInterval = 4
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.Play SeqUpOn, 25, 1, 500
    LightSeqAttract.UpdateInterval = 4
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.Play SeqUpOn, 25, 1, 500
    LightSeqAttract.UpdateInterval = 2
    LightSeqAttract.Play SeqScrewRightOn, 50, 8
    LightSeqAttract.UpdateInterval = 2
    LightSeqAttract.Play SeqBlinking, , 5, 150
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

' droptargets, animations, timers, etc
Sub VPObjects_Init
End Sub

' tables variables
Dim HelmetHits(4)
Dim bMainModeReady 'ready to choose a main mode
Dim bSelectModeActive
Dim Mode(4, 8)     ' 4 players, 8 modes. Modes 1 to 4 are the main modes, 5-8 are side modes
Dim ModeActive     'current mode number. can be a main mode or a side mode
Dim OldMode        'mode to resume after a side mode
Dim ModeSelect
Dim ModeStep       'different steps in a Mode
Dim ModeSecondsLeft
Dim SkillShotHits
Dim TurboHits(4)             'can only be run once per ball
Dim TurboScore(4)            'start value of the switches when TB is on.
Dim bTurboJustFinished
Dim RightRampHits(4)         'Right ramp hits for each player
Dim RightRampHitsRequired(4) 'Right ramp hits required for each player
Dim LeftRampHits             'nbr of hits needed to get superjackpot during rampmultiball
Dim RampHits                 'nbr of hits needed to get superjackpot the second time during rampmultiball
Dim RaceHits                 'nbr of hits during Gace MB
Dim CarLanes                 'nbr of car lanes completed
Dim M1Step(4)                'mode 1 step
Dim M2LeftHits(4)            'mode 2 left ramp hits
Dim M2RightHits(4)           'mode 2 right ramp hits
Dim M3Combos(4)              'count the combos
Dim M4TargetHits(4)          'count the hits in Mode 4
Dim M8RampHits               'count the ramp hits in Mode 8 phase 1, it will be used as the base for the super jackpot
Dim M8SuperJackpot
Dim M8LeftOrbitHits          'these are the variables for the phase 2
Dim M8ScoopHits
Dim M8LeftRampHits
Dim M8RightRampHits
Dim M8DropTargetHits
Dim M8TurboTargetHits
Dim M8RightOrbitHits
Dim M8HelmetHits
Dim bM8LeftOrbitAwarded
Dim bM8ScoopAwarded
Dim bM8LeftRampAwarded
Dim bM8RightRampAwarded
Dim bM8DropTargetAwarded
Dim bM8TurboTargetAwarded
Dim bM8RightOrbitAwarded
Dim BonusOrbits
Dim BonusRamps
Dim BonusHelmet
Dim BonusModes
Dim BonusSwitches
Dim bShootScoopPlayed

Sub Game_Init()     'called at the start of a new game
    BallsInHole = 1 ' add a ball to the hole
    ModeSelect = 0
    ModeStep = 0
    OldMode = 0
    ModeSecondsLeft = 0
    RampHits = 0
    RaceHits = 0
    M8RampHits = 0
    M8SuperJackpot = 0
    M8LeftOrbitHits = 0 'these are the variables for the phase 2
    M8ScoopHits = 0
    M8LeftRampHits = 0
    M8RightRampHits = 0
    M8DropTargetHits = 0
    M8TurboTargetHits = 0
    M8RightOrbitHits = 0
    M8HelmetHits = 0
    For x = 0 to 8 'reset the modes for each player
        Mode(1, x) = 0
        Mode(2, x) = 0
        Mode(3, x) = 0
        Mode(4, x) = 0
    Next
    For x = 1 to 4
        RightRampHits(x) = 0
        RightRampHitsRequired(x) = 5
        Jackpot(x) = 1000000
        SuperJackpot(x) = 5000000
        Skillshot(x) = 5000000
        TurboHits(x) = 0
        TurboScore(x) = 50000
        HelmetHits(x) = 0
        M1Step(x) = 0
        M2LeftHits(x) = 0
        M2RightHits(x) = 0
        M3Combos(x) = 0
        M4TargetHits(x) = 0
    Next
    bTurboJustFinished = False
    bShootScoopPlayed = False
End Sub

Sub InstantInfo
    Dim tmp
    DMD CL("INSTANT INFO"), "", "", eNone, eNone, eNone, 1000, False, ""
    'Show some info on the current Mode

    If Score(1) Then
        DMD CL("PLAYER 1 SCORE"), CL(FormatScore(Score(1) ) ), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(2) Then
        DMD CL("PLAYER 2 SCORE"), CL(FormatScore(Score(2) ) ), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(3) Then
        DMD CL("PLAYER 3 SCORE"), CL(FormatScore(Score(3) ) ), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(4) Then
        DMD CL("PLAYER 4 SCORE"), CL(FormatScore(Score(4) ) ), "", eNone, eNone, eNone, 2000, False, ""
    End If

    ' Bonus acuumulated
    DMD CL("BONUS"), "", "", eBlink, eNone, eNone, 1000, False, ""
    DMD CL("BONUS"), CL("ORBIT HITS " & BonusOrbits), "", eBlink, eNone, eNone, 1000, False, ""
    DMD CL("BONUS"), CL("RAMP HITS " & BonusRamps), "", eBlink, eNone, eNone, 1000, False, ""
    DMD CL("BONUS"), CL("HELMET HITS " & BonusHelmet), "", eBlink, eNone, eNone, 1000, False, ""
    DMD CL("BONUS"), CL("SWITCH HITS " & BonusSwitches), "", eBlink, eNone, eNone, 1000, False, ""
    DMD CL("BONUS"), CL("MODES STARTED " & BonusModes), "", eBlink, eNone, eNone, 1000, False, ""
    DMD CL("BONUS"), CL("MULIPLIER " & BonusMultiplier(CurrentPlayer) ), "", eBlink, eNone, eNone, 1000, False, ""
End Sub

Sub StopMBmodes 'stop multiball modes after loosing the last multiball
    StopRainbow
    If ModeActive> 4 Then StopMode
End Sub

Sub StopEndOfBallMode() 'this sub is called after the last ball in play is drained, reset skillshot, modes, timers
    ResetSkillShotTimer_Timer
    F7.State = 0        'stop the flasher if it was on
    If bTurboJustFinished = False Then StopMode
End Sub

Sub ResetNewBallVariables() 'reset variables and lights for a new ball or player
    bMainModeReady = False
    bSelectModeActive = False
    ModeActive = 0
    LowerHelmet
    PLayModeSong
    ResetDT
    LeftRampHits = 0
    RampHits = 0
    ChangeGi white
    ChangeGIIntensity 1
    CarLanes = 0
    'Bonus for each ball
    BonusOrbits = 0
    BonusRamps = 0
    BonusHelmet = 0
    BonusModes = 0
    BonusSwitches = 0
    TurnOff aLights  'turn off all the lights
    UpdateModeLights 'turn on the completed modes for the player
    If HelmetHits(CurrentPlayer) >= 3 Then
        RaiseHelmet
        UpdateLockLightsUp
    End If
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' *********************************************************************

'*********************************************************
' Slingshots has been hit

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Lemk
    DOF 106, DOFPulse 'DOF Solenoid/MX
    LeftSling001.Visible = 0
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    BonusSwitches = BonusSwitches + 1
' add some effect to the table?
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing004.Visible = 0:LeftSLing003.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing003.Visible = 0:LeftSLing002.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing002.Visible = 0:LeftSling001.Visible = 1:Lemk.RotX = -20:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 104, DOFPulse, DOFcontactors), Remk
    DOF 107, DOFPulse 'DOF Solenoid/MX
    RightSling001.Visible = 0
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    BonusSwitches = BonusSwitches + 1
' add some effect to the table?
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:RightSling001.Visible = 1:Remk.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'*********
' Lanes
'*********

Sub Trigger001_Hit 'left outlane
    PlaySoundAt "fx_sensor", Trigger001
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    Addscore 50000
    If l010.State = 0 Then
        PlaySound "s_lanesyes", , SFXVolume
        l010.State = 1
        CheckCarLanes
    Else
        PlaySound "s_lanesno", , SFXVolume
    End If
    ' Modes
    Select Case ModeActive
        Case 6
            AwardTurboScore
            LightEffect 3
            PlaySound "s_switchturbo01", , SFXVolume
    End Select
End Sub

Sub Trigger002_Hit 'left inlane
    PlaySoundAt "fx_sensor", Trigger002
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    Addscore 50000
    If l011.State = 0 Then
        PlaySound "s_lanesyes", , SFXVolume
        l011.State = 1
        CheckCarLanes
    Else
        PlaySound "s_lanesno", , SFXVolume
    End If
    ' Modes
    Select Case ModeActive
        Case 6
            LightEffect 3
            AwardTurboScore
            PlaySound "s_switchturbo01", , SFXVolume
    End Select
End Sub

Sub Trigger003_Hit 'right inlane
    PlaySoundAt "fx_sensor", Trigger003
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    Addscore 5000
    If l012.State = 0 Then
        PlaySound "s_lanesyes", , SFXVolume
        l012.State = 1
        CheckCarLanes
    Else
        PlaySound "s_lanesno", , SFXVolume
    End If
    ' Modes
    Select Case ModeActive
        Case 6
            LightEffect 3
            AwardTurboScore
            PlaySound "s_switchturbo01", , SFXVolume
    End Select
End Sub

Sub Trigger004_Hit 'right outlane
    PlaySoundAt "fx_sensor", Trigger004
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    Addscore 50000
    If l013.State = 0 Then
        PlaySound "s_lanesyes", , SFXVolume
        l013.State = 1
        CheckCarLanes
    Else
        PlaySound "s_lanesno", , SFXVolume
    End If
    ' Modes
    Select Case ModeActive
        Case 6
            LightEffect 3
            AwardTurboScore
            PlaySound "s_switchturbo01", , SFXVolume
    End Select
End Sub

Sub CheckCarLanes
    If l010.State + l011.State + l012.State + l013.State = 4 Then
        lighteffect 3
        l010.State = 0
        l011.State = 0
        l012.State = 0
        l013.State = 0
        CarLanes = CarLanes + 1
        AddBonusMultiplier 1
        If CarLanes = 5 OR CarLanes = 15 Then 'extra ball at the hole
            ExtraBallIsLit
        End If
    End if
End Sub

'***************
' Orbit triggers
'***************

Sub LeftOrbit_Hit 'left orbit trigger
    PlaySoundAt "fx_sensor", LeftOrbit
    If Tilted Then Exit Sub
    If bSkillshotActive Then Exit Sub
    If LastSwitchHit = "RightOrbit" Then Exit Sub
    Addscore 5000
    'Bonus
    BonusSwitches = BonusSwitches + 1
    BonusOrbits = BonusOrbits + 1
    'Modes
    Select Case ModeActive
        Case 1
            ' check loops
            Select Case M1Step(CurrentPlayer)
                Case 1, 2, 3
                    LightEffect 3
                    M1Step(CurrentPlayer) = M1Step(CurrentPlayer) + 1
                    PlaySound "s_outyes01", , SFXVolume
                    CheckMode
            End Select
        Case 3
            If M2ResetLights.Enabled Then 'score combo if the lights blink faster
                DMD "_", CL("COMBO " &FormatScore(150000) ), "", eNone, eBlinkFast, eNone, 1000, True, ""
                Addscore 150000
                LightEffect 2
                PlaySound "s_comboyes01", , SFXVolume
                M3Combos(CurrentPlayer) = M3Combos(CurrentPlayer) + 1
                CheckMode
                M2ResetLights.Enabled = 0:M2ResetLights.Enabled = 1 'reset the timer so the lights keep blinking faster for more combos
            Else
                TurnBlinkFaster aGreenArrows
                M2ResetLights.Enabled = 1 'to return to the slower blink
            End If
        Case 5
            If ModeStep = 1 Then
                AwardJackpot
                RaceHits = RaceHits + 1
                CheckMode
            End If
        Case 6
            LightEffect 3
            AwardTurboScore
            PlaySound "s_switchturbo01", , SFXVolume
        Case 8
            Select Case ModeStep
                Case 2
                    M8LeftOrbitHits = M8LeftOrbitHits + 1
                    CheckMode
            End Select
    End Select
    LastSwitchHit = "LeftOrbit"
End Sub

Sub LeftOrbit2_Hit:LastSwitchHit = "":End Sub

Sub RightOrbit_Hit 'right loop orbit
    PlaySoundAt "fx_sensor", RightOrbit
    If Tilted Then Exit Sub
    If bSkillshotActive Then Exit Sub
    If LastSwitchHit = "LeftOrbit" Then Exit Sub
    If LastSwitchHit = "AutoPLungerOFFSw" Then Exit Sub
    Addscore 5000
    'Bonus
    BonusSwitches = BonusSwitches + 1
    BonusOrbits = BonusOrbits + 1
    'Modes
    Select Case ModeActive
        Case 1
            ' check loops
            Select Case M1Step(CurrentPlayer)
                Case 1, 2, 3
                    LightEffect 3
                    M1Step(CurrentPlayer) = M1Step(CurrentPlayer) + 1
                    PlaySound "s_outyes02", , SFXVolume
                    CheckMode
            End Select
        Case 3
            If M2ResetLights.Enabled Then 'score combo if the lights blink faster
                DMD "_", CL("COMBO"), "", eNone, eBlinkFast, eNone, 1000, True, ""
                Addscore 150000
                PlaySound "s_comboyes01", , SFXVolume
                M3Combos(CurrentPlayer) = M3Combos(CurrentPlayer) + 1
                CheckMode
                M2ResetLights.Enabled = 0:M2ResetLights.Enabled = 1 'reset the timer
            Else
                TurnBlinkFaster aGreenArrows
                M2ResetLights.Enabled = 1
            End If
        Case 5
            If ModeStep = 1 Then
                AwardJackpot
                RaceHits = RaceHits + 1
                CheckMode
            End If
        Case 6
            LightEffect 3
            AwardTurboScore
            PlaySound "s_switchturbo01", , SFXVolume
        Case 8
            Select Case ModeStep
                Case 2
                    M8RightOrbitHits = M8RightOrbitHits + 1
                    CheckMode
            End Select
    End Select
    LastSwitchHit = "RightOrbit"
End Sub

Sub RightOrbit2_Hit:LastSwitchHit = "":End Sub

Sub Trigger007_Hit 'hole trigger
    PlaySoundAt "fx_sensor", Trigger007
    BallsInHole = BallsInHole + 1
    HoleOut
End Sub

Sub ScoopTrigger_Hit 'hole tunnel trigger
    BonusSwitches = BonusSwitches + 1
    BallsInHole = BallsInHole + 1
    F7.State = 0
    If ModesCompleted = 7 AND ModeActive <> 8 Then StartMode 8:HoleOut:Exit Sub
    If bMainModeReady AND(ModeActive = 0) Then 'just kick out the ball if a side mode is active
        StartSelectMainMode
    Else
        HoleOut
    End If
    'Extra Ball
    If bExtraBallLit AND ModeActive <> 8 Then
        bExtraBallLit = False
        AwardExtraBall
        l017.State = 0
    End If
    'Modes
    Select Case ModeActive
        Case 5
            If ModeStep = 1 Then
                AwardJackpot
                RaceHits = RaceHits + 1
                CheckMode
            End If
        Case 6
            AwardTurboScore
            LightEffect 3
            PlaySound "s_switchturbo01", , SFXVolume
        Case 8
            Select Case ModeStep
                Case 2
                    M8ScoopHits = M8ScoopHits + 1
                    CheckMode
            End Select
    End Select
End Sub

'***************
' Ramp triggers
'***************

Sub LeftRampDone_Hit 'left ramp done trigger
    If Tilted Then Exit Sub
    BallsInHole = BallsInHole + 1
    HoleOut
End Sub

Sub LeftRampStart_Hit
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    PlaySound "s_switch1ramps" &RndNbr(4), , SFXVolume
End Sub

Sub TopRampDone_Hit
    Dim tmp
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    PlaySound "s_ramp02", , SFXVolume
    If bSkillshotActive Then
        SkillShotHits = SkillShotHits + 1
        AwardSkillshot
        UpdateSkillShot 4 'reset the skillshot by a few seconds
        ResetSkillShotTimer.Enabled = 0
        ResetSkillShotTimer.Enabled = 1
        vpmTimer.AddTimer 250, "PlaySound""s_skillrampwin01"",,SFXVolume '"
        If SkillShotHits MOD 3 = 0 Then vpmTimer.AddTimer 500, "PlaySound ""v_loop0"" &RndNbr(4), , VoiceVolume '"
        Else
            Addscore 5000
    End If
    BonusRamps = BonusRamps + 1
    ' Modes
    Select Case ModeActive
        Case 2
            If M2LeftHits(CurrentPlayer) <3 Then
                M2LeftHits(CurrentPlayer) = M2LeftHits(CurrentPlayer) + 1
                DMD "_", CL(FormatScore(100000) ), "", eNone, eBlinkFast, eNone, 1000, True, ""
                Addscore 100000
                vpmTimer.AddTimer 250, "PlaySound""s_rampyes01"",,SFXVolume '"
                CheckMode
            End If
        Case 3
            If M2ResetLights.Enabled Then 'score combo if the lights blink faster
                DMD "_", CL("COMBO"), "", eNone, eBlinkFast, eNone, 1000, True, ""
                Addscore 150000
                PlaySound "s_comboyes01", , SFXVolume
                M3Combos(CurrentPlayer) = M3Combos(CurrentPlayer) + 1
                CheckMode
                M2ResetLights.Enabled = 0:M2ResetLights.Enabled = 1 'reset the timer
            Else
                TurnBlinkFaster aGreenArrows
                M2ResetLights.Enabled = 1
            End If
        Case 5
            If ModeStep = 1 Then
                AwardJackpot
                RaceHits = RaceHits + 1
                CheckMode
            End If
        Case 6
            LightEffect 3
            AwardTurboScore
            PlaySound "s_switchturbo01", , SFXVolume
        Case 7
            If ModeStep = 1 OR ModeStep = 3 Then
                AwardJackpot
                CheckMode
            End If
        Case 8
            Select case ModeStep
                Case 1
                    vpmTimer.AddTimer 250, "PlaySound""s_rampyes01"",,SFXVolume '"
                    M8RampHits = M8RampHits + 1
                    tmp = 1000000 * M8RampHits
                    DMD CL("SUPERJACKPOT VALUE"), CL(FormatScore(tmp)), "", eNone, eBlinkFast, eNone, 1500, True, ""
                    AddScore 25000
                Case 2
                    M8LeftRampHits = M8LeftRampHits + 1
                    CheckMode
                Case 3
            End Select
    End Select
End Sub

Sub RightRampEntry_Hit
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    PlaySound "s_ramp01", , SFXVolume
End Sub

Sub RightRampDone_Hit
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    PlaySound "s_ramp02", , SFXVolume
    Addscore 5000
    BonusRamps = BonusRamps + 1
    ' Modes
    If ModeActive <5 Then 'only during main modes
        RightRampHits(CurrentPlayer) = RightRampHits(CurrentPlayer) + 1
        DMD "_", CL("RIGHT RAMP HITS " &RightRampHits(CurrentPlayer) & "/" & RightRampHitsRequired(CurrentPlayer) ), "", eNone, eNone, eNone, 1000, True, ""
        If RightRampHits(CurrentPlayer) = RightRampHitsRequired(CurrentPlayer) Then StartMode 7
    End If
    Select Case ModeActive
        Case 2
            If M2RightHits(CurrentPlayer) <3 Then
                M2RightHits(CurrentPlayer) = M2RightHits(CurrentPlayer) + 1
                DMD "_", CL(FormatScore(100000) ), "", eNone, eBlinkFast, eNone, 1000, True, ""
                Addscore 100000
                vpmTimer.AddTimer 250, "PlaySound""s_rampyes01"",,SFXVolume '"
                CheckMode
            End If
        Case 3
            If M2ResetLights.Enabled Then 'score combo if the lights blink faster
                DMD "_", CL("COMBO"), "", eNone, eBlinkFast, eNone, 1000, True, ""
                Addscore 150000
                PlaySound "s_comboyes01", , SFXVolume
                M3Combos(CurrentPlayer) = M3Combos(CurrentPlayer) + 1
                CheckMode
                M2ResetLights.Enabled = 0:M2ResetLights.Enabled = 1 'reset the timer
            Else
                TurnBlinkFaster aGreenArrows
                M2ResetLights.Enabled = 1
            End If
        Case 5
            If ModeStep = 1 Then
                AwardJackpot
                RaceHits = RaceHits + 1
                CheckMode
            End If
        Case 6
            LightEffect 3
            AwardTurboScore
            PlaySound "s_switchturbo01", , SFXVolume
        Case 7
            Select Case ModeStep
                Case 2
                    AwardSuperJackpot
                    vpmTimer.AddTimer 200, "PlaySound""v_superjack01"",,VoiceVolume '"
                    CheckMode
                Case 3
                    AwardJackpot
                    CheckMode
            End Select
        Case 8
            Select Case ModeStep
                Case 2:M8RightRampHits = M8RightRampHits + 1:CheckMode
            End Select
    End Select
End Sub

'**************
' Drop Targets
'**************

Sub dt1_Hit:PlaySoundAt "fx_droptarget", dt1:End Sub

Sub dt1_Dropped
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    Addscore 5000
    vpmTimer.AddTimer 500, "ResetDT '"
    ' check modes
    Select case ModeActive
        Case 0
        Case 1
            Select Case M1Step(CurrentPlayer)
                Case 0, 4
                    LightEffect 3
                    PlaySound "s_dropyes01", , SFXVolume
                    M1Step(CurrentPlayer) = M1Step(CurrentPlayer) + 1
                    CheckMode
                Case 1, 2, 3
            End Select
        Case 4
            If l092.State Then
                PlaySound "s_targetyes", , SFXVolume
                TurnBlink aOrangeArrows
                l092.State = 0
                DMD "_", CL(FormatScore(100000) ), "", eNone, eBlinkFast, eNone, 1000, True, ""
                Addscore 100000
                LightEffect 3
                M4TargetHits(CurrentPlayer) = M4TargetHits(CurrentPlayer) + 1
                CheckMode
            End If
        Case 5
            If ModeStep = 1 Then
                AwardJackpot
                RaceHits = RaceHits + 1
                CheckMode
            End If
        Case 6
            AwardTurboScore
            LightEffect 3
            PlaySound "s_switchturbo01", , SFXVolume
        Case 8
            Select Case ModeStep
                Case 2
                    M8DropTargetHits = M8DropTargetHits + 1
                    CheckMode
            End Select
    End Select
End Sub

Sub ResetDT
    PlaySoundAt "fx_resetdrop", dt1
    dt1.IsDropped = 0
End Sub

'**************
' Targets
'**************

Sub Target001_Hit 'lower left
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    Addscore 5000
    ' check modes
    Select Case ModeActive
        Case 4
            If l001.State Then
                PlaySound "s_targetyes", , SFXVolume
                TurnBlink aOrangeArrows
                l001.State = 0
                DMD "_", CL(FormatScore(100000) ), "", eNone, eBlinkFast, eNone, 1000, True, ""
                Addscore 100000
                LightEffect 3
                M4TargetHits(CurrentPlayer) = M4TargetHits(CurrentPlayer) + 1
                CheckMode
            End If
        Case 6
            AwardTurboScore
            LightEffect 3
            PlaySound "s_switchturbo01", , SFXVolume
    End Select
End Sub

Sub Target002_Hit 'upper right
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    Addscore 5000
    ' check modes
    Select Case ModeActive
        Case 4
            If l084.State Then
                PlaySound "s_targetyes", , SFXVolume
                TurnBlink aOrangeArrows
                l084.State = 0
                DMD "_", CL(FormatScore(100000) ), "", eNone, eBlinkFast, eNone, 1000, True, ""
                Addscore 100000
                LightEffect 3
                M4TargetHits(CurrentPlayer) = M4TargetHits(CurrentPlayer) + 1
                CheckMode
            End If
        Case 6
            AwardTurboScore
            LightEffect 3
            PlaySound "s_switchturbo01", , SFXVolume
    End Select
End Sub

Sub Target003_Hit 'lower right
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    Addscore 5000
    ' check modes
    Select Case ModeActive
        Case 4
            If l037.State Then
                PlaySound "s_targetyes", , SFXVolume
                TurnBlink aOrangeArrows
                l037.State = 0
                DMD "_", CL(FormatScore(100000) ), "", eNone, eBlinkFast, eNone, 1000, True, ""
                Addscore 100000
                LightEffect 3
                M4TargetHits(CurrentPlayer) = M4TargetHits(CurrentPlayer) + 1
                CheckMode
            End If
        Case 6
            AwardTurboScore
            LightEffect 3
            PlaySound "s_switchturbo01", , SFXVolume
    End Select
End Sub

Sub TurboTarget_Hit 'Turbo target Upper flipper target
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    Addscore 5000
    ' check modes
    Select Case ModeActive
        Case 0, 1, 2, 3, 4
            TurboHits(CurrentPlayer) = TurboHits(CurrentPlayer) + 1
            UpdateTurboLights
            PlaySound "s_turbo0" & RndNbr(3), , SFXVolume
            If TurboHits(CurrentPlayer) = 7 Then StartMode 6
        Case 6
            FlashForms F5, 500, 50, 0
            LightEffect 3
            AwardTurboScore
            ModeSecondsLeft = ModeSecondsLeft + 5
            If ModeSecondsLeft> 30 Then ModeSecondsLeft = 30
            EnableBallSaver ModeSecondsLeft
            PlaySound "s_switchturbo01", , SFXVolume
        Case 8
            Select Case ModeStep
                Case 2
                    M8TurboTargetHits = M8TurboTargetHits + 1
                    CheckMode
            End Select
    End Select
End Sub

Sub Target005_Hit 'top target
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    Addscore 5000
    ' check modes
    Select Case ModeActive
        Case 4
            If l085.State Then
                PlaySound "s_targetyes", , SFXVolume
                TurnBlink aOrangeArrows
                l085.State = 0
                DMD "_", CL(FormatScore(100000) ), "", eNone, eBlinkFast, eNone, 1000, True, ""
                Addscore 100000
                LightEffect 3
                M4TargetHits(CurrentPlayer) = M4TargetHits(CurrentPlayer) + 1
                CheckMode
            End If
        Case 6
            AwardTurboScore
            LightEffect 3
            PlaySound "s_switchturbo01", , SFXVolume
    End Select
End Sub

Sub Target006_Hit 'top target 2
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    BonusSwitches = BonusSwitches + 1
    Addscore 5000
    ' check modes
    Select Case ModeActive
        Case 4
            If l038.State Then
                PlaySound "s_targetyes", , SFXVolume
                TurnBlink aOrangeArrows
                l038.State = 0
                DMD "_", CL(FormatScore(100000) ), "", eNone, eBlinkFast, eNone, 1000, True, ""
                Addscore 100000
                LightEffect 3
                M4TargetHits(CurrentPlayer) = M4TargetHits(CurrentPlayer) + 1
                CheckMode
            End If
        Case 6
            AwardTurboScore
            LightEffect 3
            PlaySound "s_switchturbo01", , SFXVolume
    End Select
End Sub

'*************
'  Left Hole
'*************

Sub HoleEntry_Hit
    HoleEntry.DestroyBall
    If Tilted Then vpmtimer.addtimer 500, "HoleOut '":Exit Sub
' score some points, do something on the DMD, check modes
' Select main mode
'If bMainModeReady AND(ModeActive = 0) Then 'just kick out the ball if a side mode is active
'    StartSelectMainMode
'End If
End Sub

Sub HoleOut
    If BallsInHole> 1 Then ' there must be always 1 ball in the hole
        BallsInHole = BallsInHole - 1
        PlaySoundAt SoundFXDOF("fx_popper", 111, DOFPulse, DOFcontactors), HoleExit
        HoleExit.CreateSizedBallWithMass BallSize / 2, BallMass
        HoleExit.kick 28, 34
        FlashForMs F6, 50, 50, 0           'one blink
        LightEffect 5
        vpmTimer.AddTimer 500, "HoleOut '" 'check if there are more balls in the hole to kick out
    End If
End Sub

'****************
'  4 BALLS LOCK
'****************

Sub lock1_Hit 'first lock
    If ModeActive = 8 AND ModeStep = 3 Then
        LightSeqInserts.StopPlay
        WinMode
        Exit Sub
    End If
    If ModeActive = 5 And ModeStep = 2 Then
        vpmTimer.AddTimer 200, "PlaySound""v_racesjp01"",,VoiceVolume '"
        AwardSuperJackpot
        vpmTimer.AddTimer 5000, "lock1.Kick 160, 4 '"
        LowerHelmet
        RaceHits = 0
        CheckMode 'to setup lights for the mode
    Else
        LockedBalls = 1
        DMD "_", CL("BALL 1 LOCKED"), "_", eNone, eBlinkfast, eNone, 2500, True, ""
        PlaySound "v_lock01", , VoiceVolume
        lock2.Enabled = 1
        Addmultiball 1
        UpdateLockLightsUp
    End If
End Sub

Sub lock2_Hit 'second lock
    LockedBalls = 2
    DMD "_", CL("BALL 2 LOCKED"), "_", eNone, eBlinkfast, eNone, 2500, True, ""
    PlaySound "v_lock01", , VoiceVolume
    lock3.Enabled = 1
    Addmultiball 1
    UpdateLockLightsUp
End Sub

Sub lock3_Hit 'third lock
    LockedBalls = 3
    DMD "_", CL("BALL 3 LOCKED"), "_", eNone, eBlinkfast, eNone, 2500, True, ""
    PlaySound "v_lock01", , VoiceVolume
    lock4.Enabled = 1
    Addmultiball 1
    UpdateLockLightsUp
End Sub

Sub lock4_Hit 'fourth lock
    LockedBalls = 4
    UpdateLockLightsDown
    DMD "_", CL("BALL 4 LOCKED"), "_", eNone, eBlinkfast, eNone, 2500, True, "s_mbstart01"
    PlaySound "v_mbstart01", , VoiceVolume
    'release the balls
    vpmTimer.AddTimer 4000, "StartMode 5:ReleaseLockedBalls '"
    'reset Helmet Hits & move down Helmet
    HelmetHits(CurrentPlayer) = 0
    LowerHelmet
End Sub

Sub ReleaseLockedBalls
    LockedBalls = 0
    bMultiBallMode = True
    lock1.Enabled = 0
    lock2.Enabled = 0
    lock3.Enabled = 0
    lock4.Enabled = 0
    lock1.Kick 0, 1
    lock2.Kick 0, 1
    lock3.Kick 0, 1
    lock4.Kick 0, 1
    vpmtimer.AddTimer 2000, "ResetLocks '"
End Sub

Sub ResetLocks
    lock1.Enabled = 1
    lock2.Enabled = 0
    lock3.Enabled = 0
    lock4.Enabled = 0
    UpdateLockLightsDown
End Sub

'****************************
'      Helmet Shake
'Inspired by koadic's code
'   from the CV table
' I know it doesn't look
' like Koadic's code but I
' got the idea from his code
'****************************

Dim cBall
Const cMod = .65 'percentage of hit power transfered to the helmet

HelmetInit

Sub HelmetShake
    cball.velx = activeball.velx * cMod
    cball.vely = activeball.vely * cMod
    aHelmetTimer.enabled = True
    bHelmetTimer.enabled = True
End Sub

Sub HelmetInit
    Set cBall = hball.createball
    hball.Kick 0, 0
    cball.Mass = 1.6
End Sub

Sub aHelmetTimer_Timer 'start animation
    helmet.rotx = (hball.y - cball.y)
    helmet.roty = (cball.x - hball.x)
End Sub

Sub bHelmetTimer_Timer 'stop animation
    helmet.rotx = 0
    helmet.roty = 0
    aHelmetTimer.enabled = False
    bHelmetTimer.enabled = False
End Sub

Sub HelmetSW_Hit()
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    PlaySound "s_helm0" &RndNbr(2), , SFXVolume
    FlashEffect 8
    HelmetShake
    AddScore 5000
    BonusSwitches = BonusSwitches + 1
    BonusHelmet = BonusHelmet + 1
    Select Case ModeActive
        Case 0, 1, 2, 3, 4
            HelmetHits(CurrentPlayer) = HelmetHits(CurrentPlayer) + 1
            If HelmetHits(CurrentPlayer) >= 3 Then RaiseHelmet:End If
        Case 5 'Race MB
            PlaySound "s_racej0" &RndNbr(3), , SFXVolume
            AwardJackpot
            RaceHits = RaceHits + 1
            CheckMode
        Case 6
            AwardTurboScore
            LightEffect 3
            PlaySound "s_switchturbo01", , SFXVolume
        Case 8
            Select Case ModeStep
                Case 3
                    PlaySound "s_wmyes0" &RndNbr(3), , SFXVolume
                    M8HelmetHits = M8HelmetHits + 1
                    CheckMode
            End Select
    End Select
End Sub

Sub RaiseHelmet
    PlaySoundAt "fx_diverter", helmet
    HelmetF.RotateToEnd
    HelmetSW.IsDropped = 1
    UpdateLockLightsUp
End Sub

Sub LowerHelmet
    PlaySoundAt "fx_diverter", helmet
    HelmetF.RotateToStart
    HelmetSW.IsDropped = 0
    UpdateLockLightsDown
End SUb

Sub HelmetF_Animate:helmet.Z = HelmetF.CurrentAngle:End Sub

Sub UpdateLockLightsUp
    Select Case LockedBalls
        Case 0:l039.State = 2:l003.State = 2:l033.State = 0:l034.State = 0:l035.State = 0
        Case 1:l039.State = 2:l003.State = 1:l033.State = 2:l034.State = 0:l035.State = 0
        Case 2:l039.State = 2:l003.State = 1:l033.State = 1:l034.State = 2:l035.State = 0
        Case 3:l039.State = 2:l003.State = 1:l033.State = 1:l034.State = 1:l035.State = 2
        Case 4:l039.State = 2:l003.State = 1:l033.State = 1:l034.State = 1:l035.State = 1
    End Select
End Sub

Sub UpdateLockLightsDown
    Select Case LockedBalls
        Case 0:l039.State = 0:l003.State = 0:l033.State = 0:l034.State = 0:l035.State = 0
        Case 1:l039.State = 0:l003.State = 1:l033.State = 0:l034.State = 0:l035.State = 0
        Case 2:l039.State = 0:l003.State = 1:l033.State = 1:l034.State = 0:l035.State = 0
        Case 3:l039.State = 0:l003.State = 1:l033.State = 1:l034.State = 1:l035.State = 0
        Case 4:l039.State = 0:l003.State = 1:l033.State = 1:l034.State = 1:l035.State = 1
    End Select
End Sub

'**********************
' Selecting Main Modes
'**********************

Sub StartSelectMainMode
    'Stop the Flasher
    F7.State = 0
    PlaySound "v_chooseracer01", , VoiceVolume
    ' set up the variables
    bMainModeReady = False
    bSelectModeActive = True
    ModeSelect = 0
    UpdateSelectMode
End Sub

Sub SelectMode(keycode)
    If keycode = LeftFlipperKey Then
        playsound "menu_Previous"
        ModeSelect = ModeSelect - 1
        if(ModeSelect <0) then
            ModeSelect = 3
        end if
        UpdateSelectMode
    End If

    If keycode = RightFlipperKey Then
        playsound "menu_Next"
        ModeSelect = (ModeSelect + 1) MOD 4
        UpdateSelectMode
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        playsound "menu_Enter"
        TurnOFF aModeArrows
        bSelectModeActive = False          'stop the selecting modes
        DMDScoreNow
        StartMode ModeSelect + 1           'main modes are from 1 to 4
        vpmTimer.AddTimer 500, "HoleOut '" 'and kick the ball
    End If
End Sub

Sub UpdateSelectMode 'after a mode selection
    TurnOFF aModeArrows
    DMDFlush
    Select Case ModeSelect
        Case 0
            DMD "DANICA THE QUEEN", "    ORBITS", "d_danica", eNone, eNone, eNone, 200, False, ""
            l006.BlinkInterval = 125:l006.State = 2
            l090.BlinkInterval = 125:l090.State = 2
            l018.BlinkInterval = 125:l018.State = 2
            l028.BlinkInterval = 125:l028.State = 2
            ChangeGi purple
            ChangeGIIntensity 1
        Case 1
            DMD "JUAN THE MASTER", "     RAMPS ", "d_juan", eNone, eNone, eNone, 200, False, ""
            l007.BlinkInterval = 125:l007.State = 2
            l031.BlinkInterval = 125:l031.State = 2
            l088.BlinkInterval = 125:l088.State = 2
            ChangeGi yellow
            ChangeGIIntensity 1
        Case 2
            DMD "    SCIOTTI THE GOD", "     ORBITS N RAMPS", "d_sciotti", eNone, eNone, eNone, 200, False, ""
            l008.BlinkInterval = 300:l008.State = 2
            l019.BlinkInterval = 300:l019.State = 2
            l029.BlinkInterval = 300:l029.State = 2
            l032.BlinkInterval = 300:l032.State = 2
            l089.BlinkInterval = 300:l089.State = 2
            ChangeGi green
            ChangeGIIntensity 1
        Case 3
            DMD "     FED THE FAST", "       TARGETS", "d_fed", eNone, eNone, eNone, 200, False, ""
            l009.BlinkInterval = 125:l009.State = 2
            l001.BlinkInterval = 125:l001.State = 2
            l084.BlinkInterval = 125:l084.State = 2
            l037.BlinkInterval = 125:l037.State = 2
            l085.BlinkInterval = 125:l085.State = 2
            l038.BlinkInterval = 125:l038.State = 2
            l092.BlinkInterval = 125:l092.State = 2
            ChangeGi orange
            ChangeGIIntensity 1
    End Select
End Sub

'****************
'    MODES
'****************

Sub StartMode(n) 'n is the new mode
    BonusModes = BonusModes + 1
    ModeActive = n
    If logging Then Debug.print "Starting Mode " &ModeActive
    TurnOff aModeArrows
    F7.State = 0 'in case it was blinking
    Select Case ModeActive
        Case 0   ' select main mode/character
            ResumeMode
        Case 1   ' Danica the Queen
            vpmTimer.AddTimer 2000, "PlaySound""v_purplestart"",,VoiceVolume '"
            OldMode = n
            ResumeMode
        Case 2 ' Juan the Master
            vpmTimer.AddTimer 2000, "PlaySound""v_yellowstart"",,VoiceVolume '"
            OldMode = n
            ResumeMode
        Case 3 ' Scioti the God
            vpmTimer.AddTimer 2000, "PlaySound""v_greenstart"",,VoiceVolume '"
            OldMode = n
            ResumeMode
        Case 4 ' Fed the Fast
            vpmTimer.AddTimer 2000, "PlaySound""v_orangestart"",,VoiceVolume '"
            OldMode = n
            ResumeMode
        Case 5 ' Race Multiball
            DMD CL("RACE MULTIBALL"), CL("STARTING"), "", eNone, eNone, eNone, 2000, True, ""
            PlaySong "m_racemb01"
            Mode(CurrentPlayer, ModeActive) = 2 'Mode active
            TurnOFF aModeArrows
            ChangeGi blue
            ChangeGIIntensity 1
            EnableBallSaver 30
            ModeStep = 1
            TurnBlink aJackpotLights
            PlaySoundAt "fx_diverter", Diverterf
            Diverterf.RotateToStart
        Case 6 ' Turbo Multiball
            DMD CL("TURBO MULTIBALL"), CL("STARTING"), "", eNone, eNone, eNone, 1500, True, ""
            vpmTimer.AddTimer 500, "PlaySound""v_turbotime01"",,VoiceVolume '"
            PlaySong "m_turbomb"
            Mode(CurrentPlayer, ModeActive) = 2
            TurnOFF aModeArrows
            TurnBlink aJackpotLights
            TurnBlink aOrangeArrows
            ModeSecondsLeft = 30
            ModeTimer.Enabled = 1
            EnableBallSaver 30
            AddMultiball 1
            ChangeGi blue
            ChangeGIIntensity 1
            bTurboJustFinished = False
            PlaySoundAt "fx_diverter", Diverterf
            Diverterf.RotateToStart
            LowerHelmet
        Case 7 ' Ramp Multiball
            DMD CL("RAMP MULTIBALL"), CL("STARTING"), "", eNone, eNone, eNone, 2000, True, ""
            PlaySong "m_rampmb01"
            TurnOFF aModeArrows
            Mode(CurrentPlayer, ModeActive) = 2
            ModeStep = 1
            l030.State = 2
            LeftRampHits = 0
            EnableBallSaver 30
            AddMultiball 1
            ChangeGi blue
            ChangeGIIntensity 1
            PlaySoundAt "fx_diverter", Diverterf
            Diverterf.RotateToEnd 'to enable lopp shots
            LowerHelmet
        Case 8                    ' Final Race
            DMD CL("FINAL RACE MULTIBALL"), CL("STARTING"), "", eNone, eNone, eNone, 2000, True, ""
            PlaySong "m_finalrace"
            For each x in aModeArrows:x.BlinkPattern = 10:Next
            TurnOFF aModeArrows
            Mode(CurrentPlayer, ModeActive) = 2
            ModeStep = 1
            ChangeGi red
            ChangeGIIntensity 1
            PlaySoundAt "fx_diverter", Diverterf
            Diverterf.RotateToEnd
            EnableBallSaver 60
            M8RampHits = 0
            M8SuperJackpot = 100000
            LowerHelmet
            l030.State = 2
    End Select
    UpdateModeLights
End Sub

Sub ResumeMode 'only Main modes can be resumed
    Modeactive = OldMode
    If logging Then Debug.print "Resuming Mode " &ModeActive
    Select Case ModeActive
        Case 0 ' no mode active
            PlaySong "m_main"
            ChangeGi white
            ChangeGIIntensity 1
            F7.State = 2
            bMainModeReady = True
            PlaySoundAt "fx_diverter", Diverterf
            Diverterf.RotateToStart
        Case 1                                  ' Danica the Queen
            PlaySong "m_purple"
            Mode(CurrentPlayer, ModeActive) = 2 'Mode active
            OldMode = 1                         'in case it needs to be restarted
            ResetDT
            UpdateModeLights
            ChangeGi purple
            ChangeGIIntensity 1
            PlaySoundAt "fx_diverter", Diverterf
            Diverterf.RotateToStart
        Case 2                                  ' Juan the Master
            PlaySong "m_yellow"
            Mode(CurrentPlayer, ModeActive) = 2 'Mode active
            OldMode = 2
            l031.State = 2
            l088.State = 2
            ChangeGi yellow
            ChangeGIIntensity 1
            PlaySoundAt "fx_diverter", Diverterf
            Diverterf.RotateToStart
            If M2LeftHits(CurrentPlayer) >= 3 AND M2RightHits(CurrentPlayer) >= 3 Then 'reset to replay the mode
                M2LeftHits(CurrentPlayer) = 0
                M2RightHits(CurrentPlayer) = 0
            End If
        Case 3                                  ' Scioti the God
            PlaySong "m_green"
            Mode(CurrentPlayer, ModeActive) = 2 'Mode active
            OldMode = 3
            TurnBlinkSlow aGreenArrows
            PlaySoundAt "fx_diverter", Diverterf
            Diverterf.RotateToStart
            ChangeGi green
            ChangeGIIntensity 1
            If M3Combos(CurrentPlayer) >= 5 Then
                M3Combos(CurrentPlayer) = 0
            End If
        Case 4                                  ' Fed the Fast
            PlaySong "m_orange"
            Mode(CurrentPlayer, ModeActive) = 2 'Mode active
            OldMode = 4
            TurnBlink aOrangeArrows
            PlaySoundAt "fx_diverter", Diverterf
            Diverterf.RotateToStart
            ChangeGi orange
            ChangeGIIntensity 1
            If M4TargetHits(CurrentPlayer) >= 10 Then
                M4TargetHits(CurrentPlayer) = 0
            End If
    End Select
    UpdateModeLights
End Sub

Sub CheckMode          'after hitting a mode target/switch check for next step or mode finish
    Select Case ModeActive
        Case 0         ' no mode active
        Case 1         ' Danica the Queen
            Select Case M1Step(CurrentPlayer)
                Case 1 'drop target hit
                    Addscore 10000
                    LightEffect 2
                    UpdateModeLights
                Case 2, 3 'orbit hit
                Case 4    'orbit hit
                    ResetDT
                    UpdateModeLights
                Case 5                        ' drop target hit and winmode
                    Addscore 25000
                    M1Step(CurrentPlayer) = 0 'ready to be restarted
                    WinMode
                    UpdateModeLights
                    vpmTimer.AddTimer 500, "ResetDT '"
            End Select
        Case 2 ' Juan the Master
            If M2LeftHits(CurrentPlayer) = 3 Then l031.State = 0:End If
            If M2RightHits(CurrentPlayer) = 3 Then l088.State = 0:End If
            If M2LeftHits(CurrentPlayer) = 3 AND M2RightHits(CurrentPlayer) = 3 Then WinMode:End If
        Case 3 ' Scioti the God
            If M3Combos(CurrentPlayer) = 5 Then
                WinMode
            End If
        Case 4 ' Fed the Fast
            If M4TargetHits(CurrentPlayer) = 10 Then WinMode:End If
        Case 5 ' Race Multiball
            Select Case ModeStep
                Case 1
                    If RaceHits = 10 then
                        ModeStep = 2
                        TurnOFF aJackpotLights
                        RaceHits = 0
                        l036.State = 2
                        l003.State = 2
                    End If
                Case 2
                    Select Case RaceHits
                        Case 0:l003.State = 2:l033.State = 0:l034.State = 0:l035.State = 0:l039.State = 0
                        Case 1:l003.State = 1:l033.State = 2:l034.State = 0:l035.State = 0:l039.State = 0
                        Case 2:l003.State = 1:l033.State = 1:l034.State = 2:l035.State = 0:l039.State = 0
                        Case 3:l003.State = 1:l033.State = 1:l034.State = 1:l035.State = 2:l039.State = 0
                        Case 4:l003.State = 1:l033.State = 1:l034.State = 1:l035.State = 1:l039.State = 2
                            'rise helmet
                            PlaySoundAt "fx_diverter", helmet
                            HelmetF.RotateToEnd
                            HelmetSW.IsDropped = 1
                    End Select
            End Select
        Case 6                               ' Turbo Multiball
        Case 7                               ' Ramp Multiball
            Select Case ModeStep
                Case 1                       'left ramp hits
                    LeftRampHits = LeftRampHits + 1
                    If LeftRampHits = 3 Then 'enable SuperJackpot at the right ramp
                        ModeStep = 2
                        l089.BlinkPattern = 100
                        l088.BlinkPattern = 010
                        l087.BlinkPattern = 001
                        l089.State = 2
                        l088.State = 2
                        l087.State = 2
                        l030.State = 0 'left ramp
                    End If
                Case 2                 'super jackpot has been at the right ramp, enable left & right ramp shots
                    Flashforms F5, 500, 50, 0
                    ModeStep = 3
                    l089.BlinkPattern = 10
                    l088.BlinkPattern = 10
                    l087.BlinkPattern = 10
                    l089.State = 0
                    l088.State = 0
                    l087.State = 2
                    l030.State = 2
                    RampHits = 0
                Case 3                   ' both ramps are lit for Jackpots
                    RampHits = RampHits + 1
                    If RampHits = 5 Then 'enable SuperJackpot at the right ramp
                        ModeStep = 2
                        l089.BlinkPattern = 100
                        l088.BlinkPattern = 010
                        l087.BlinkPattern = 001
                        l089.State = 2
                        l088.State = 2
                        l087.State = 2
                        l030.State = 0
                    End If
            End Select
        Case 8         ' Final Race
            Select Case ModeStep
                Case 1 'the 60 seconds are over move to phase 2
                    SuperJackpot(CurrentPlayer) = 1000000 * M8RampHits
                    ModeStep = 2
                    PlaySoundAt "fx_diverter", Diverterf
                    Diverterf.RotateToStart
                    ReleaseLockedBalls
                    vpmTimer.AddTimer 1000, "AddMultiball 3 '"
                    EnableBallSaver 60
                    M8LeftOrbitHits = 0 'these are the variables for the phase 2
                    M8ScoopHits = 0
                    M8LeftRampHits = 0
                    M8RightRampHits = 0
                    M8DropTargetHits = 0
                    M8TurboTargetHits = 0
                    M8RightOrbitHits = 0
                    bM8LeftOrbitAwarded = False
                    bM8ScoopAwarded = False
                    bM8LeftRampAwarded = False
                    bM8RightRampAwarded = False
                    bM8DropTargetAwarded = False
                    bM8TurboTargetAwarded = False
                    bM8RightOrbitAwarded = False
                    TurnBlink aM8HitLights
                Case 2 'phase 2
                    'update the lights
                    Select Case M8LeftOrbitHits
                        Case 0
                        Case 1:l019.State = 1:l018.State = 2:l026.State = 2
                        Case 2:l019.State = 1:l018.State = 1:l026.State = 2
                        Case 3:l019.State = 1:l018.State = 1:l026.State = 1
                            If NOT bM8LeftOrbitAwarded Then
                                bM8LeftOrbitAwarded = True
                                AddMultiball 3
                                EnableBallSaver 10
                            End If
                        Case Else
                            M8LeftOrbitHits = 3
                    End Select
                    Select Case M8ScoopHits
                        Case 0
                        Case 1:l017.State = 1:l002.State = 2
                        Case 2:l017.State = 1:l002.State = 1
                            If NOT bM8ScoopAwarded Then
                                bM8ScoopAwarded = True
                                AddMultiball 3
                                EnableBallSaver 10
                            End If
                        Case Else
                            bM8ScoopAwarded = 2
                    End Select
                    Select Case M8LeftRampHits
                        Case 0
                        Case 1:l032.State = 1:l031.State = 2:l030.State = 2
                        Case 2:l032.State = 1:l031.State = 1:l030.State = 2
                        Case 3:l032.State = 1:l031.State = 1:l030.State = 1
                            If NOT bM8LeftRampAwarded Then
                                bM8LeftRampAwarded = True
                                AddMultiball 3
                                EnableBallSaver 10
                            End If
                        Case Else
                            M8LeftRampHits = 3
                    End Select
                    Select Case M8RightRampHits
                        Case 0
                        Case 1:l089.State = 1:l088.State = 2:l087.State = 2
                        Case 2:l089.State = 1:l088.State = 1:l087.State = 2
                        Case 3:l089.State = 1:l088.State = 1:l087.State = 1
                            If NOT bM8RightRampAwarded Then
                                bM8RightRampAwarded = True
                                AddMultiball 3
                                EnableBallSaver 10
                            End If
                        Case Else
                            M8RightRampHits = 3
                    End Select
                    Select Case M8DropTargetHits
                        Case 0
                        Case 1:l090.State = 1:l091.State = 2:l092.State = 2
                        Case 2:l090.State = 1:l091.State = 1:l092.State = 2
                        Case 3:l090.State = 1:l091.State = 1:l092.State = 1
                            If NOT bM8DropTargetAwarded Then
                                bM8DropTargetAwarded = True
                                AddMultiball 3
                                EnableBallSaver 10
                            End If
                        Case Else
                            M8DropTargetHits = 3
                    End Select
                    Select Case M8TurboTargetHits
                        Case 0
                        Case 1:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 2:l024.State = 2:l025.State = 2:l004.State = 2
                        Case 2:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 1:l024.State = 1:l025.State = 1:l004.State = 2
                        Case 3:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 1:l024.State = 1:l025.State = 1:l004.State = 1
                            If NOT bM8TurboTargetAwarded Then
                                bM8TurboTargetAwarded = True
                                AddMultiball 3
                                EnableBallSaver 10
                            End If
                        Case Else
                            M8TurboTargetHits = 3
                    End Select
                    Select Case M8RightOrbitHits
                        Case 0
                        Case 1:l029.State = 1:l028.State = 2:l027.State = 2
                        Case 2:l029.State = 1:l028.State = 1:l027.State = 2
                        Case 3:l029.State = 1:l028.State = 1:l027.State = 1
                            If NOT bM8RightOrbitAwarded Then
                                bM8RightOrbitAwarded = True
                                AddMultiball 3
                                EnableBallSaver 10
                            End If
                        Case Else
                            M8RightOrbitHits = 3
                    End Select
                    If M8LeftOrbitHits + M8ScoopHits + M8LeftRampHits + M8RightRampHits + M8DropTargetHits + M8TurboTargetHits + M8RightOrbitHits = 20 Then
                        ' move to the next phase
                        ModeStep = 3
                        TurnOFF aM8HitLights
                        EnableBallSaver 60
                        AddMultiball 3
                        TurnBlink aM8HelmetLights
                        M8HelmetHits = 0
                    End If
                Case 3 'Phase 3
                    'Update Lights
                    Select Case M8HelmetHits
                        Case 0
                        Case 1:l003.State = 2:l033.State = 2:l034.State = 2:l035.State = 2:l036.State = 2:l039.State = 0
                        Case 2:l003.State = 1:l033.State = 1:l034.State = 2:l035.State = 2:l036.State = 2:l039.State = 0
                        Case 3:l003.State = 1:l033.State = 1:l034.State = 1:l035.State = 2:l036.State = 2:l039.State = 0
                        Case 4:l003.State = 1:l033.State = 1:l034.State = 1:l035.State = 1:l036.State = 2:l039.State = 2
                            RaiseHelmet
                            LightSeqInserts.UpdateInterval = 4
                            LightSeqInserts.Play SeqUpOn, 15, 1000
                    End Select
            End Select
    End Select
End Sub

Sub StopMode 'called after a drain or multiball om MB modes, stops the currernt mode (ModeActive variable)
    If logging Then Debug.print "StopMode " &ModeActive
    ChangeGi White
    ChangeGIIntensity 1
    Select Case ModeActive
        Case 1, 2, 3, 4
            M2ResetLights.Enabled = 0 'just to be sure is off
            ModeTimer.Enabled = 0     'just to be sure is off
            Mode(CurrentPlayer, ModeActive) = 0
            ModeActive = 0
            OldMode = 0
            bMainModeReady = True
            TurnOFF aModeArrows
            vpmTimer.AddTimer 500, "ResetDT '"
            StopSong
        Case 5
            Mode(CurrentPlayer, ModeActive) = 1 'finished the mode
            ModeActive = OldMode
            LowerHelmet
            TurnOFF aJackpotLights
            StopSong
            BallSaverTime = 0
            l033.State = 0
            l034.State = 0
            l035.State = 0
            l003.State = 0
            If BallsOnPlayfield> 0 Then 'ball is still in Play
                ResumeMode              'resume the old mode
                'check for Final Race
                CheckFinalRace
            End If
        Case 6                                  'stops only at the modetimer
        Case 7
            Mode(CurrentPlayer, ModeActive) = 1 'finished the mode
            PlaySoundAt "fx_diverter", Diverterf
            Diverterf.RotateToStart
            ModeActive = OldMode
            TurnOFF aJackpotLights
            TurnOFF aModeArrows
            BallSaverTime = 0
            StopSong
            l089.BlinkPattern = 10
            l088.BlinkPattern = 10
            l087.BlinkPattern = 10
            RightRampHits(CurrentPlayer) = 0
            RightRampHitsRequired(CurrentPlayer) = RightRampHitsRequired(CurrentPlayer) + 5 'increase the difficulty
            If BallsOnPlayfield> 0 Then                                                     'ball is still in Play
                ResumeMode                                                                  'resume the old mode
                'check for Final Race
                CheckFinalRace
            End If
        Case 8
            StopSong
            DMD CL("YOU ALMOST WON"), CL("THE RACE"), "", eNone, eNone, eNone, 5000, True, ""
            vpmTimer.AddTimer 200, "PlaySound""v_secondplace01"",,SFXVolume '"
            LightSeqInserts.StopPlay
            DisableTable True
            TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
            FlashEffect 3
            FlashEffect 2
            ReleaseLockedBalls 'there should not be any ball, but just in case
            BallSaverTime = 0                'in case the ballsaver was on
            'reset the Modes
            For x = 1 to 8:Mode(CurrentPlayer, x) = 0:Next
            ModeActive = 0
            OldMode = 0
            M1Step(CurrentPlayer) = 0
            M2LeftHits(CurrentPlayer) = 0
            M2RightHits(CurrentPlayer) = 0
            M3Combos(CurrentPlayer) = 0
            M4TargetHits(CurrentPlayer) = 0
    End Select
    UpdateModeLights
End Sub

Sub WinMode 'main modes
    If ModeActive = 8 Then
        StopSong
        DMD CL("YOU WON"), CL("THE RACE"), "", eBlinkFast, eBlinkFast, eNone, 5000, True, ""
        vpmTimer.AddTimer 200, "PlaySound""s_wmwin"",,SFXVolume '"
        LightSeqInserts.StopPlay
        AwardSuperJackpot
        DisableTable True
        FlashEffect 3
        FlashEffect 2
        FlashEffect 2
        FlashEffect 2
        vpmTimer.AddTimer 5000, "lock1.Kick 160, 4 '"
        LowerHelmet
        'Stop the game and give a credit
        ExtraBallsAwards(CurrentPlayer) = 0
        ReleaseLockedBalls
        AwardSpecial
        BallSaverTime = 0                'in case the ballsaver was on
        'reset the Modes
        For x = 1 to 8:Mode(CurrentPlayer, x) = 0:Next
        ModeActive = 0
        OldMode = 0
        M1Step(CurrentPlayer) = 0
        M2LeftHits(CurrentPlayer) = 0
        M2RightHits(CurrentPlayer) = 0
        M3Combos(CurrentPlayer) = 0
        M4TargetHits(CurrentPlayer) = 0
        'end the game
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    Else
        DMD CL("EXCELLENT"), CL("MODE COMPLETED"), "", eNone, eBlinkFast, eNone, 4000, True, ""
        LightEffect 2
        FlashEffect 2
        Mode(CurrentPlayer, ModeActive) = 1
        ModeActive = 0
        OldMode = 0
        ModeTimer.Enabled = 0
        M2ResetLights.Enabled = 0
        TurnBlink aModeArrows              'to reset the blink speed
        TurnOFF aModeArrows                'and turn them off
        vpmTimer.AddTimer 500, "ResetDT '" 'in case it was down
        vpmTimer.AddTimer 4000, "DMDScoreNow '"
        vpmTimer.AddTimer 500, "PlayModeSong '"
        UpdateModeLights
        ResumeMode 'restart the character selection
        'check for Final Race
        CheckFinalRace
    End If
End Sub

' the value of the Mode Array is
' 0 not started
' 1 completed
' 2 active
' and the lights will turn on according to this value

Sub UpdateModeLights 'called for ex. at the start of a new ball to update the finished modes
    If ModeActive = 0 Then Turnoff aModeArrows
    ' Main moes
    l006.State = Mode(CurrentPlayer, 1)
    l007.State = Mode(CurrentPlayer, 2)
    l008.State = Mode(CurrentPlayer, 3)
    l009.State = Mode(CurrentPlayer, 4)
    l014.State = Mode(CurrentPlayer, 5)
    l015.State = Mode(CurrentPlayer, 6)
    l016.State = Mode(CurrentPlayer, 7)
    l005.State = Mode(CurrentPlayer, 8)
    ' turbo hit Lights
    UpdateTurboLights
    'Mode 1
    If ModeActive = 1 Then
        Select Case M1Step(CurrentPlayer)
            Case 0:l090.State = 2:l028.State = 0:l018.State = 0
            Case 1:l090.State = 0:l028.State = 2:l018.State = 2
            Case 2:l090.State = 0:l028.State = 2:l018.State = 2
            Case 3:l090.State = 0:l028.State = 2:l018.State = 2
            Case 4:l090.State = 2:l028.State = 0:l018.State = 0
            Case 5:l090.State = 0:l028.State = 0:l018.State = 0
        End Select
    Else
        l090.State = 0:l028.State = 0:l018.State = 0
    End If
End Sub

Sub ModeTimer_Timer 'the mode ends when the ModeSecondsLeft reach 0
    ModeSecondsLeft = ModeSecondsLeft - 1
    Select Case ModeActive
        Case 6
            If ModeSecondsLeft = 10 Then PlaySound "v_turbocharge0" &RndNbr(2), , VoiceVolume
            Select Case ModeSecondsLeft
                Case 0
                    'the timer went out, you still have balls in play so stop the table and drain the balls
                    ModeTimer.Enabled = 0
                    BallSaverTime = 0                   'the ball saver timer will stop
                    TurboHits(CurrentPlayer) = 0
                    Mode(CurrentPlayer, ModeActive) = 1 'finished the mode
                    ModeActive = OldMode
                    TurnOFF aJackpotLights
                    TurnOFF aOrangeArrows
                    'ResumeMode
                    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
                    DisableTable True
                    TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
                    bTurboJustFinished = True
                Case 2, 1:l020.State = 0:l021.State = 0:l022.State = 0:l023.State = 0:l024.State = 0:l025.State = 0:l004.State = 0
                Case 6, 5, 4, 3:l020.State = 1:l021.State = 0:l022.State = 0:l023.State = 0:l024.State = 0:l025.State = 0:l004.State = 0
                Case 10, 9, 8, 7:l020.State = 1:l021.State = 1:l022.State = 0:l023.State = 0:l024.State = 0:l025.State = 0:l004.State = 0
                Case 14, 13, 12, 11:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 0:l024.State = 0:l025.State = 0:l004.State = 0
                Case 18, 17, 16, 15:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 1:l024.State = 0:l025.State = 0:l004.State = 0
                Case 22, 21, 20, 19:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 1:l024.State = 1:l025.State = 0:l004.State = 0
                Case 26, 25, 24, 23:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 1:l024.State = 1:l025.State = 1:l004.State = 0
                Case 30, 29, 28, 27:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 1:l024.State = 1:l025.State = 1:l004.State = 1
            End Select
    End Select
End Sub

'***********************
'   TURBO MultiBall
'***********************

Sub UpdateTurboLights
    Select Case TurboHits(CurrentPlayer)
        Case 0:l020.State = 0:l021.State = 0:l022.State = 0:l023.State = 0:l024.State = 0:l025.State = 0:l004.State = 0
        Case 1:l020.State = 1:l021.State = 0:l022.State = 0:l023.State = 0:l024.State = 0:l025.State = 0:l004.State = 0
        Case 2:l020.State = 1:l021.State = 1:l022.State = 0:l023.State = 0:l024.State = 0:l025.State = 0:l004.State = 0
        Case 3:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 0:l024.State = 0:l025.State = 0:l004.State = 0
        Case 4:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 1:l024.State = 0:l025.State = 0:l004.State = 0
        Case 5:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 1:l024.State = 1:l025.State = 0:l004.State = 0
        Case 6:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 1:l024.State = 1:l025.State = 1:l004.State = 0
        Case 7:l020.State = 1:l021.State = 1:l022.State = 1:l023.State = 1:l024.State = 1:l025.State = 1:l004.State = 1
    End Select
End Sub

Sub AwardTurboScore
    Addscore TurboScore(CurrentPlayer)
    DMD "_", CL(FormatScore(TurboScore(CurrentPlayer) ) ), "_", eNone, eBlinkFast, eNone, 1500, True, ""
    PlaySound "s_turbomb01", , SFXVolume
End Sub

'***********************
'   Rotate LANE Lights
'***********************

Sub RotateLightsLeft
    Dim tmp
    tmp = l010.state
    l010.State = l011.state
    l011.State = l012.state
    l012.State = l013.State
    l013.State = tmp
End Sub

Sub RotateLightsRight
    Dim tmp
    tmp = l013.state
    l013.State = l012.state
    l012.State = l011.state
    l011.State = l010.State
    l010.State = tmp
End Sub

Sub M2ResetLights_Timer
    If logging Then Debug.Print "M2ResetLights just run"
    M2ResetLights.Enabled = 0
    If ModeActive = 3 Then
        TurnBlinkSlow aGreenArrows
    Else
        TurnOff aGreenArrows
    End If
End Sub

'*****************************
'   Final RACE - Wizard Mode
'*****************************

Sub CheckFinalRace
    If ModesCompleted = 7 Then
        F7.State = 2
        l005.State = 2
    End If
End Sub

Function ModesCompleted
    Dim tmp
    tmp = 0
    For x = 1 to 7
        If Mode(CurrentPlayer, x) = 1 Then
            tmp = tmp + Mode(CurrentPlayer, x)
        End If
    Next
    ModesCompleted = tmp
End Function

'*********************************
' Table Options F12 User Options
'*********************************
' Table1.Option arguments are:
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional array of literal strings

Dim LUTImage, BallsPerGame, UseFlexDMD, OldUseFlex, FlexDMDHighQuality, SongVolume, VoiceVolume, SFXVolume, HelmetColor
UseFlexDMD = False 'initialize variable
OldUseFlex = False
SongVolume = 0.3
VoiceVolume = 1
SFXVolume = 1

Sub Table1_OptionEvent(ByVal eventId)
    Dim x, y

    'LUT
    LutImage = Table1.Option("Select LUT", 0, 21, 1, 0, 0, Array("Normal 0", "Normal 1", "Normal 2", "Normal 3", "Normal 4", "Normal 5", "Normal 6", "Normal 7", "Normal 8", "Normal 9", "Normal 10", _
        "Warm 0", "Warm 1", "Warm 2", "Warm 3", "Warm 4", "Warm 5", "Warm 6", "Warm 7", "Warm 8", "Warm 9", "Warm 10") )
    UpdateLUT

    ' Desktop DMD
    x = Table1.Option("DMD Type", 0, 1, 1, 0, 0, Array("Desktop DMD", "FlexDMD") )
    If UseFlexDMD AND x = 0 Then FlexDMD.Run = False
    If X then UseFlexDMD = True Else UseFlexDMD = False
    If Table1.ShowDT = False Then UseFlexDMD = True

    ' FlexDMD Quality
    x = Table1.Option("FlexDMD Quality", 0, 1, 1, 1, 0, Array("Low", "High") )
    If x Then FlexDMDHighQuality = True Else FlexDMDHighQuality = False
    If OldUseFlex <> UseFlexDMD Then
        DMD_Init
        If NOT bGameInPlay Then ShowTableInfo
        OldUseFlex = UseFlexDMD
    End If

    ' Cabinet rails
    x = Table1.Option("Cabinet Rails", 0, 1, 1, 1, 0, Array("Hide", "Show") )
    For each y in aRails:y.visible = x:Next

    ' Color Ramps
    RampColor = Table1.Option("Color Ramps", 0, 3, 1, 3, 0, Array("Blue", "White", "Green", "Red") )
    UpdateRampColor

    ' Helmet Color
    HelmetColor = Table1.Option("Helmet Color", 0, 9, 1, 0, 0, Array("Default", "Red", "Black", "Blue", "Cyan", "Green", "Orange", "Purple", "White", "yellow") )
    UpdateHelmetColor

    ' Balls per Game
    x = Table1.Option("Balls per Game", 0, 1, 1, 0, 0, Array("3 Balls", "5 Balls") )
    If x = 1 Then BallsPerGame = 5 Else BallsPerGame = 3

    ' FreePlay
    x = Table1.Option("Free Play", 0, 1, 1, 0, 0, Array("No", "Yes") )
    If x then bFreePlay = True Else bFreePlay = False

    ' Music  On/Off
    x = Table1.Option("Music", 0, 1, 1, 1, 0, Array("OFF", "ON") )
    If x Then bMusicOn = True Else bMusicOn = False

    ' Music Volume
    SongVolume = Table1.Option("Music Volume", 0, 1, 0.1, 0.3, 0)
    If bMusicOn AND bGameInPlay Then
        PlaySound Song, -1, SongVolume
    End If

    ' Voices Volume
    x = Table1.Option("Voices Volume", 0, 1, 0.1, 1, 0)
    If x <> VoiceVolume Then
        PlaySound "v_jack02", , VoiceVolume
        VoiceVolume = x
    End If

    ' SoundFX Volume
    x = Table1.Option("Sound FX Volume", 0, 1, 0.1, 1, 0)
    If x <> SFXVolume Then
        PlaySound "s_ramp01", , SFXVolume
        SFXVolume = x
    End If
End Sub

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

'*****************************
'    Change RAMP colors
'*****************************

Dim RampColor

Sub UpdateRampColor
    Dim x
    Select Case RampColor
        Case 0:x = RGB(0, 64, 255) 'blue
        Case 1:x = RGB(96, 96, 96) 'White
        Case 2:x = RGB(0, 128, 32) 'Green
        Case 3:x = RGB(128, 0, 0)  'Red
    End Select
    MaterialColor "Plastic Transp Ramps", x
End Sub

'*****************************
'    Change Helmet colors
'*****************************
'"Default","Black","Blue", "Cyan","Green", "Orange","Purple","Red","White","yellow"

Sub UpdateHelmetColor
    Dim x
    Select Case HelmetColor
        Case 1:helmet.Image = "helmet_Red"
        Case 2:helmet.Image = "helmet_Black"
        Case 3:helmet.Image = "helmet_Blue"
        Case 4:helmet.Image = "helmet_Cyan"
        Case 5:helmet.Image = "helmet_Green"
        Case 6:helmet.Image = "helmet_Orange"
        Case 7:helmet.Image = "helmet_Purple"
        Case 8:helmet.Image = "helmet_White"
        Case 9:helmet.Image = "helmet_yellow"
        Case 0:helmet.Image = "helmet_default"
    End Select
End Sub

'DMD CL(""), CL(""), "", eNone, eNone, eNone, 2000, True, ""
sub test_hit:test.destroyball:end Sub