' ****************************************************************
'      JP's Space Cadet for VISUAL PINBALL X 10.8, version 5.7.0
'       Based on the PC game by Cinematronics/Maxis from 1996
' ****************************************************************

Option Explicit
Randomize

Const BallSize = 50     ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1      ' standard ball mass in JP's VPX Physics 3.0
Const SongVolume = 0.16 ' 1 is full volume, but I set it quite low to listen better the other sounds since I use headphones, adjust to your setup :)

'FlexDMD in high or normal quality
'True if you have an LCD screen, 256x64
'or False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = True

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
Const cGameName = "jpspacecadet"
Const myVersion = "5.7.0"
Const MaxPlayers = 4         ' from 1 to 4
Const BallSaverTime = 10     ' in seconds of the first ball
Const MaxMultiplier = 10     ' limit playfield multiplier
Const MaxBonusMultiplier = 5 'limit Bonus multiplier
Const BallsPerGame = 3       ' usually 3 or 5
Const MaxMultiballs = 5      ' max number of balls during multiballs

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
Dim PFxSeconds
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot(4)
Dim SuperJackpot(4)
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue(4)
Dim SuperSkillshotValue(4)
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode
Dim i, j, k, x 'used in loops

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock(4)
Dim BallsInHole

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMael
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bJackpot

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs
Dim leftkickerIM
Dim rightkickerIM
Dim aMagnet
Dim bMagnet

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************
dim songplayed
songplayed = 1
Sub Table1_Init()
    LoadEM
    Dim i
    Randomize

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 40 ' Plunger Power
    '	Const IMPowerSetting2 = 80 ' Power
    Const IMTime = 0.30 ' Time in seconds for Full Plunge

    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.1
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

'Impulse Plunger as right kickback
'    Set rightkickerIM = New cvpmImpulseP
'    With rightkickerIM
'        .InitImpulseP RightKicker, IMPowerSetting2, IMTime
'        .Random 0
'        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
'        .CreateEvents "rightkickerIM"
'    End With

'    'Impulse Plunger as left kickback
'    Set leftkickerIM = New cvpmImpulseP
'    With leftkickerIM
'        .InitImpulseP RightKicker, IMPowerSetting2, IMTime
'        .Random 0
'        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
'        .CreateEvents "leftkickerIM"
'    End With

' Magnet
    Set aMagnet = New cvpmMagnet
    With aMagnet
        .InitMagnet GravityMagnet, 3
        .GrabCenter = True
        .CreateEvents "aMagnet"
    End With

    Set bMagnet = New cvpmMagnet
    With bMagnet
        .InitMagnet Trigger025, 1
        .GrabCenter = True
        .CreateEvents "bMagnet"
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Credits = 0
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
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    PFxSeconds = 0
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bBonusHeld = False
    bJustStarted = True
    bJackpot = False
    bInstantInfo = False
    wall062.HeightBottom = -1
    wall062.IsDropped = 1
    bmagnet.MagnetOn = 0

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

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25

    If keycode = LeftMagnaSave Then bLutActive = True:SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If
    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
        Credits = Credits + 1
        if bFreePlay = False Then DOF 125, DOFOn
        If(Tilted = False) Then
            If NOT bGameInPlay Then
                DMDFlush
                DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 3000, False, "fx_coin"
            Else
                DMDFlush
                DMD CL(0, "CREDITS " & Credits), CL(1, "_"), "", eNone, eBlink, eNone, 1500, False, ""
                PlaySound "fx_coin"
            '     DMD CL(1, "CREDITS " & Credits), "_", "", eNone, eNone, eNone, 500, True, "fx_coin"
            end If
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then CheckTilt 'only check the tilt during game
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt
        If keycode = MechanicalTilt Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 1
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 0

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eNone, eNone, 1000, True, ""
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eNone, eNone, 1000, True, ""
                        If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, "vo_nocredits"
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
                            If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, "vo_nocredits"
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

    If keycode = LeftMagnaSave Then bLutActive = False:HideLUT

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
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
    If hsbModeActive = False AND bInstantInfo = False Then
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
    If UseFlexDMD Then FlexDMD.Run = False
    If B2SOn = true Then Controller.Stop
End Sub

'********************
'     Flippers
'********************

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

Sub LeftFlipper_Animate:LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle:End Sub
Sub RightFlipper_Animate:RightFlipperTop.RotZ = RightFlipper.CurrentAngle:End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
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
FlipperElasticity = 0.75
FullStrokeEOS_Torque = 0.3 ' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.2 ' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

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
    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower:End If

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
    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower:End If

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

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                                                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt < 15) Then                                'show a warning
        DMDFlush
        DMD "_", CL(1, "CAREFUL"), "_", eNone, eBlinkFast, eNone, 1500, True, "" '"vo_Careful"
    End if
    If NOT Tilted AND Tilt> 15 Then                                              'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD CL(0, "YOU"), CL(1, "TILTED"), "", eNone, eNone, eNone, 2000, True, "" '"vo_You tilted"
        DisableTable True
        TiltRecoveryTimer.Enabled = True                                           'start the Tilt delay to check for all the balls to be drained
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
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        Bumper3.Threshold = 100
        Bumper4.Threshold = 100
        Bumper5.Threshold = 100
        Bumper6.Threshold = 100
        Bumper7.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        LeftSlingshot2.Disabled = 1
        Rightslingshot2.Disabled = 1
        li094.State = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        Bumper3.Threshold = 1
        Bumper4.Threshold = 1
        Bumper5.Threshold = 1
        Bumper6.Threshold = 1
        Bumper7.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        Leftslingshot2.Disabled = 0
        Rightslingshot2.Disabled = 0
        li094.State = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0) Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        vpmtimer.Addtimer 2000, "EndOfBall() '"
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'****************************************
'      Music as wav/ogg/mp3 sounds
'****************************************

Dim Song
Song = ""
Dim Lastsong

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            PlaySound Song, -1, SongVolume
        End If
    End If
End Sub

Sub ChangeSong 'not used in this table
    Dim a
    Select Case Mission(CurrentPlayer)
        Case 0 'no Mission active so play the standard songs or the multiball songs
    End Select
End Sub

'********************
' Play random soundfx
'********************

Sub PlaySfx
    'Dim tmp
    'tmp = RndNbr(16)
    PlaySound "SOUND136" '"sc_" &tmp
' LightEffect 3
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
        If UBound(tmp) = -1 Then '-1 means no balls, 0 is the first captive ball, 1 is the second captive ball...)
            GiOff                ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    PlaySoundAt "fx_GiOn", li036 'about the center of the table
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    PlaySoundAt "fx_GiOff", li036 'about the center of the table
    DOF 118, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 20
            LightSeqGi.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 20
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 20
            LightSeqGi.Play SeqBlinking, , 10, 10
        Case 4 'seq up
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqUpOn, 25, 3
        Case 5 'seq down
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqDownOn, 25, 3
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
        Case 4 'center
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqCircleOutOn, 15, 2
        Case 5 'top down
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 15, 1
        Case 6 'down to top
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 15, 1
    End Select
End Sub

Sub FlashEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqFlashers.Play SeqAlloff
        Case 1 'all blink
            LightSeqFlashers.UpdateInterval = 40
            LightSeqFlashers.Play SeqBlinking, , 15, 30
        Case 2 'random
            LightSeqFlashers.UpdateInterval = 30
            LightSeqFlashers.Play SeqRandom, 25, , 1500
        Case 3 'all blink fast
            LightSeqFlashers.UpdateInterval = 20
            LightSeqFlashers.Play SeqBlinking, , 10, 30
        Case 4 'center
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqCircleOutOn, 15, 2
        Case 5 'top down
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqDownOn, 15, 2
        Case 6 'down to top
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqUpOn, 15, 1
    End Select
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
    PlaySound soundname, 0, 1, Pan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
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
Const maxvel = 35 'max ball velocity
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

        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 5
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        End If

        ' jps ball speed & spin control
        BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95
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
Sub aTriggers_Hit(idx):PlaySfx:End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

    'resets the score display, and turn off attract mode
    StopRainbow
    LightSeqAttract.StopPlay
    DMDFlush
    DMD CL(0, "BOOT"), CL(1, "SEQUENCE"), "", eNone, enone, eNone, 2000, True, ""
    GiOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 10000
        BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        PlayfieldMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    ' initialise any other flags
    Tilt = 0

    ' initialise Game variables
    Game_Init()

    ' you may wish to start some music, play a sound, do whatever at this point
    PlaySong "silence"
    stopsound "mu_end"

    DMD CL(0, "STARTING"), CL(1, "ENGINE"), "", eblink, eBlink, eNone, 1500, True, ""
    DMD CL(0, "SHIP"), CL(1, "ONLINE"), "", eblinkfast, eBlinkfast, eNone, 800, True, ""
    PlaySound "sc_StartUp"

    LightEffect 0
    LightSeqInserts.UpdateInterval = 100
    LightSeqInserts.Play SeqRandom, 1, , 1000
    LightSeqInserts.Play SeqRandom, 2, , 750
    LightSeqInserts.Play SeqRandom, 3, , 500
    LightSeqInserts.Play SeqRandom, 4, , 400
    LightSeqInserts.Play SeqRandom, 5, , 200
    LightSeqInserts.Play SeqRandom, 100, , 200

    LightSeqInserts.UpdateInterval = 1
    LightSeqInserts.Play SeqBlinking, , 16, 10

    'LightEffect 0
    vpmtimer.addtimer 4500, "FirstBall : DMDscorenow '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
'    If BallsOnPlayfield = 1 Then
'End If
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    AddScore 0

    ' set the current players bonus multiplier back down to 1X
    ' SetBonusMultiplier 1

    ' reduce the playfield multiplier
    ' reset any drop targets, lights, game Mode etc..

    BonusPoints(CurrentPlayer) = 10000
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
    bBallInPlungerLane = True
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    If BallsOnPlayfield = 0 Then playsound "SOUND4"

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4

' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield> 1 Then
        DOF 143, DOFPulse
        bMultiBallMode = True
        bAutoPlunger = True
        playsound "SOUND4"
    End If
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
        If BallsOnPlayfield < MaxMultiballs Then
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
    Dim TotalBonus
    TotalBonus = 0
    PlaySound "sc_drain"

    playsong "mu_wait"

    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.
    '(the tilt recovery mechanism will handle any extra balls or end of game)

    If NOT Tilted Then

        'add in any bonus points (multipled by the bonus multiplier)

        'Crash Bonus
        DMD CL(0, FormatScore(BonusPoints(CurrentPlayer) ) ), CL(1, "CRASH BONUS"), "", eNone, eNone, eNone, 1500, True, "" '"vo_Crash Bonus"

' calculate the totalbonus
' DMD CL(0, "BONUS MULTIPLIER"), CL(1, "X " &BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 1000, True, ""
        TotalBonus = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer) + BonusHeldPoints(CurrentPlayer)

        ' handle the bonus held
        ' reset the bonus held value since it has been already added to the bonus
        BonusHeldPoints(CurrentPlayer) = 0

        ' the player has won the bonus held award so do something with it :)
        If bBonusHeld Then
            If Balls = BallsPerGame Then ' this is the last ball, so if bonus held has been awarded then double the bonus
                TotalBonus = TotalBonus * 2
            End If
        Else ' this is not the last ball so save the bonus for the next ball
            BonusHeldPoints(CurrentPlayer) = TotalBonus
        End If
        bBonusHeld = False

        ' Add the bonus to the score
        DMD CL(0, FormatScore(TotalBonus) ), CL(1, "TOTAL BONUS"), "", eNone, eNone, eNone, 1500, True, ""

        AddScore TotalBonus

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 3000, "EndOfBall2 '"
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
    ' set TiltWarnings back to zero) which is useful if we are changing player
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            li030.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, "shoot again"

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
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

    'debug.print "EndOfBall - Complete"

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

    'debug.print "Next Player = " & NextPlayer

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
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            Select Case CurrentPlayer
                Case 1:DMD "", CL(1, "PLAYER 1"), "", eNone, eNone, eNone, 1000, True, "vo_player1"
                Case 2:DMD "", CL(1, "PLAYER 2"), "", eNone, eNone, eNone, 1000, True, "vo_player2"
                Case 3:DMD "", CL(1, "PLAYER 3"), "", eNone, eNone, eNone, 1000, True, "vo_player3"
                Case 4:DMD "", CL(1, "PLAYER 4"), "", eNone, eNone, eNone, 1000, True, "vo_player4"
            End Select
        End If
    '    If BallsOnPlayfield = 1 Then
    'End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then

    End If
    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
    songplayed = 1
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
    BallsOnPlayfield = BallsOnPlayfield - 1
    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball
    ' Exit Sub ' only for debugging - this way you can add balls from the debug window

    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain
    'if Tilted the end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then
            'bBallInPlungerLane=True

            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1

            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            DMDFlush
            DMD "_", CL(1, "RE-DEPLOY"), "_", eNone, eBlink, eNone, 1000, True, ""
            If(bMultiBallMode = False) Then
                EnableBallSaver(1)
            End If
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)

            If(BallsOnPlayfield = 1) AND(bMultiBallMode = True) Then
                ' AND in a multi-ball??
                'If(bMultiBallMode = True)then
                ' not in multiball mode any more
                bMultiBallMode = False
                HyperSpaceHits = 0
                maeljack = 0
                ' you may wish to change any music over at this point and

                ' turn off any multiball specific lights
                ' ChangeGi white
                'stop any multiball modes
                StopMBmodes
            ' End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
                ' End Mode and timers

                ' ChangeGi white
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
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
    PlaySoundAt "fx_sensor", swPlungerRest
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    'LaunchLight.State = 2

    'be sure to update the Scoreboard after the animations, if any

    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        autofirepplungerTimer.Enabled = 1
    '   vpmtimer.addtimer 1000, "PlungerIM.AutoFire:bAutoPlunger = False:DOF 143, DOFPulse '" ' Outhere
    End If
    'Start the Selection of the skillshot if ready
    If bSkillShotReady Then
        PlaySong "mu_wait"
        UpdateSkillshot()
    End If
End Sub

sub autofirepplungerTimer_Timer
    PlungerIM.AutoFire:bAutoPlunger = False:DOF 143, DOFPulse
    autofirepplungerTimer.Enabled = 0
end Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    lighteffect 6
    ' bBallInPlungerLane = False
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active

    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
    End If
    ' turn off LaunchLights
    li113.State = 0
    li114.State = 0
    li115.State = 0
    li116.State = 0
    li117.State = 0
    li118.State = 0
    ' Start Fuel & timers
    Fuel = 6
    UpdateFuelLights
    StartFuelTimers
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimerExpired.Interval = 1000 * seconds
    BallSaverTimerExpired.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimerExpired_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimerExpired.Enabled = False
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

Sub AddScore(points)
    If Tilted Then Exit Sub
    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
    ' Add some points to the Bonus and Jackpot if they are enabled
    If bBonusActivated Then AddBonus points
    If bJackpotActivated Then AddJackpot points

' you may wish to check to see if the player has gotten a replay due to high score
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points)
    If Tilted Then Exit Sub
    ' add the bonus to the current players bonus variable
    BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    If BonusPoints(CurrentPlayer)> 5000000 Then
        BonusPoints(CurrentPlayer) = 5000000
    End If
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If Tilted Then Exit Sub

    ' If(bMultiBallMode = True) Then
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
' DMD "_", CL(1, "INCREASED JACKPOT"), "_", eNone, eNone, eNone, 1000, True, ""
' you may wish to limit the jackpot to a upper limit, ie..
'	If (Jackpot >= 6000) Then
'		Jackpot = 6000
' 	End if
'End if
End Sub

Sub AwardExtraBall() 'more like a replay in this table
    If NOT bExtraBallWonThisBall Then
        DMDFlush
        DMD "_", CL(1, ("REPLAY AWARDED") ), "_", eNone, eBlinkFast, eNone, 1000, True, SoundFXDOF("", 122, DOFPulse, DOFKnocker)
        PlaySound "replay"
        DOF 121, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = 1 ' only one ball per ball in this game
        bExtraBallWonThisBall = True
        li030.State = 1                     'light the shoot again lamp
        GiEffect 2
        LightEffect 2
    END If
End Sub

Sub AwardSpecial()
    DMDFlush
    DMD "_", CL(1, ("EXTRA GAME WON") ), "_", eNone, eBlinkFast, eNone, 1000, True, "extraball"
    DOF 121, DOFPulse
    Credits = Credits + 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardJackpot()                                                                                               'award a normal jackpot
    DMDFlush
    DMD CL(0, FormatScore(Jackpot(CurrentPlayer) ) ), CL(1, "JACKPOT"), "", eNone, eblink, eNone, 1000, True, "" '"vo_Jackpot Awarded"
    'DOF 126, DOFPulse
    AddScore Jackpot(CurrentPlayer)
    'reset the Jackpot Value
    Jackpot(CurrentPlayer) = 20000
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardSkillshot()
    'stop the skillshot lights
    StopSkillshot
    'show dmd animation
    DMDFlush
    DMD CL(0, FormatScore(SkillshotValue(CurrentPlayer) ) ), CL(1, "SKILLSHOT"), "", eNone, eblink, eNone, 1000, True, "sc_skillshot" '"vo_Skill Shot"
    DOF 127, DOFPulse
    Addscore SkillShotValue(CurrentPlayer)
    'do some light show
    GiEffect 2
    LightEffect 2
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 100000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 100000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 100000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 100000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(cGameName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(cGameName, "TotalGamesPlayed")
    If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
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
    Loadhs
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
        DOF 125, DOFOn
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
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "fx_Esc"
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
    dLine(0) = ExpandLine(TempTopStr, 0)
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

    if(hsCurrentDigit < 1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit < 2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    dLine(1) = ExpandLine(TempBotStr, 1)
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
            If HighScore(j) < HighScore(j + 1) Then
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

'************************************
'       LUT - Darkness control
' 10 normal level & 10 warmer levels
'************************************

Dim bLutActive, LUTImage

Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1) MOD 22:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

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
GiIntensity = 1               'can be used by the LUT changing to increase the GI lights when the table is darker

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
    LUBack.imagea = "PostItNote"
    For xFor = 1 to 40
        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
    SetLUTLine ""
    LUBack.imagea = "PostitBL"
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
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkborder")
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
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkborder")
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
        dLine(i) = Space(dCharsPerLine(i) )
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
    Dim tmp, tmp1, tmp1a, tmp1b, tmp2, tmp3, tmp4, tmp5
    if(dqHead = dqTail) Then
        'tmp = CL(0, FormatScore(Score(Currentplayer) ) )
        tmp = FL(0, "P" &CurrentPlayer& " B" &Balls, FormatScore(Score(Currentplayer) ) )
        tmp1 = ""
        tmp2 = "bkborder"
        tmp3 = True 'true or False
        tmp4 = eNone
        tmp5 = eNone

        'tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
        'tmp1 = FormatScore(Bonuspoints(Currentplayer) ) & " X" &BonusMultiplier(Currentplayer)
        Select Case Mission(CurrentPlayer)
            Case 0: 'no Mission active
                If Score(CurrentPlayer) = 0 OR bSkillshotReady OR bGameInPlay = False Then
                    tmp1 = CL(1, "AWAITING DEPLOYMENT")
                    tmp5 = eBlink
                Else
                    If MissionSelected Then
                        tmp1 = CL(1, "ACCEPT MISSION")
                    Else
                        if Rank(CurrentPlayer) = 9 Then
                            tmp1 = CL(1, "GO FOR ANY MISSION")
                            tmp5 = eBlink
                        Elseif Rank(CurrentPlayer) = 8 Then
                            tmp1 = CL(1, "CONTINUE MISSIONS")
                            tmp5 = eBlink
                        Else tmp1 = CL(1, "SELECT A MISSION")
                        end if
                    End If
                End If
            Case 1:tmp1 = " RAMP HITS LEFT " &MissionHits
            Case 2:tmp1 = " LANE HITS LEFT " &MissionHits
            Case 3:tmp1 = "BUMPER HITS LEFT " &MissionHits
            Case 4:tmp1 = "DROPTARGETS LEFT " &MissionHits
            Case 5:tmp1 = "TARGET HITS LEFT " &MissionHits
            Case 6:tmp1 = "   UPGRADE FLAGS"
            Case 61:tmp1 = "ENTER THE HYPERSPACE"
            Case 7:tmp1 = "UPGRADE ATT. BUMPERS"
            Case 71:tmp1 = "BUMPER HITS LEFT " &MissionHits
            Case 8:tmp1 = "HIT YELLOW WORMHOLE"
            Case 81:tmp1 = "HIT RED WORMHOLE"
            Case 82:tmp1 = "HIT GREEN WORMHOLE"
            Case 9:tmp1 = " HIT COMET TARGETS"
            Case 91:tmp1 = "ENTER THE HYPERSPACE"
            Case 10:tmp1 = "HIT RADIATION TRGETS"
            Case 101:tmp1 = "ENTER ANY WORMHOLE"
            Case 11:tmp1 = " UPGRADE ENG BUMPERS"
            Case 111:tmp1 = "ENTER THE BLACK HOLE"
            Case 12:tmp1 = "FLAG SPINS LEFT " &MissionHits
            Case 121:tmp1 = "  HIT SPACE WARP"
            Case 13:tmp1 = "SATELLITE HITS " &MissionHits
            Case 14:tmp1 = "LANE HITS LEFT " &MissionHits
            Case 15:tmp1 = "OUTLANE HITS LEFT " &MissionHits
            Case 16:tmp1 = "REBOUND HITS LEFT " &MissionHits
            Case 161:tmp1 = "RAMP OR HYPERSPACE"
            'Maelstrom
            Case 17:tmp1 = "DROPTARGETS LEFT " & MissionHits
            Case 171:tmp1 = "SPOT TARGETS LEFT " & MissionHits
            Case 172:tmp1 = "LANES LEFT " & MissionHits
            Case 173:tmp1 = " HIT THE FUEL CHUTE"
            Case 174:tmp1 = "HIT THE LAUNCH RAMP"
            Case 175:tmp1 = "   ROLL A FLAG"
            Case 176:tmp1 = "ENTER A WORMHOLE"
            Case 177:tmp1 = "ENTER HYPERSPACE"

        End Select
    End If
    If Fuel = 1 then
        tmp1 = CL(1, "WARNING - LOW FUEL")
        tmp5 = eBlink
    end If
    If Fuel = 0 then
        tmp1 = CL(1, "RE-FUEL SHIP")
        tmp5 = eBlinkFast
    end if
    DMD tmp, tmp1, tmp2, tmp4, tmp5, eNone, 200, tmp3, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail < dqSize) Then
        if(Text0 = "_") Then
            dqEffect(0, dqTail) = eNone
            dqText(0, dqTail) = "_"
        Else
            dqEffect(0, dqTail) = Effect0
            dqText(0, dqTail) = ExpandLine(Text0, 0)
        End If

        if(Text1 = "_") Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1, 1)
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
    DMDEffectTimer.Interval = deSpeed

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
                    Temp = Right(dLine(i), dCharsPerLine(i) - 1)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), (dCharsPerLine(i) + 1) - deCount(i), 1)
                    Temp = Temp & Left(dLine(i), dCharsPerLine(i) - 1)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkSlowRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If

                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i) )
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkFastRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If

                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i) )
                    End If
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

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(dCharsPerLine(id) )
    Else
        if(Len(TempStr)> Space(dCharsPerLine(id) ) ) Then
            TempStr = Left(TempStr, Space(dCharsPerLine(id) ) )
        Else
            if(Len(TempStr) < dCharsPerLine(id) ) Then
                TempStr = TempStr & Space(dCharsPerLine(id) - Len(TempStr) )
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

Function FL(id, NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    Temp = dCharsPerLine(id) - Len(NumString1) - Len(NumString2)
    TempStr = NumString1 & Space(Temp) & NumString2
    FL = TempStr
End Function

Function CL(id, NumString) 'center line
    Dim Temp, TempStr
    If Len(NumString) < dCharsPerLine(id) Then
        Temp = (dCharsPerLine(id) - Len(NumString) ) \ 2
        TempStr = Space(Temp) & NumString & Space(Temp)
        CL = TempStr
    Else
        CL = LEFT(NumString, dCharsPerLine(id) )
    End If
End Function

Function RL(id, NumString) 'right line
    Dim Temp, TempStr
    If Len(NumString) < dCharsPerLine(id) Then
        Temp = dCharsPerLine(id) - Len(NumString)
        TempStr = Space(Temp) & NumString
        RL = TempStr
    Else
        RL = LEFT(NumString, dCharsPerLine(id) )
    End If
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
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "bkborder"
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

'*******************************
'  JP's new DMD using flashers
' can now use small letters too
'*******************************

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

    Chars(32) = "d_empty"
    Chars(33) = ""        '!
    Chars(34) = ""        '"
    Chars(35) = ""        '#
    Chars(36) = ""        '$
    Chars(37) = ""        '%
    Chars(38) = ""        '&
    Chars(39) = ""        ''
    Chars(40) = ""        '(
    Chars(41) = ""        ')
    Chars(42) = ""        '*
    Chars(43) = "d_plus"  '+
    Chars(44) = ""        '
    Chars(45) = "d_minus" '-
    Chars(46) = "d_dot"   '.
    Chars(47) = ""        '/
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
    Chars(61) = ""        '=
    Chars(62) = "d_more"  '>
    Chars(64) = ""        '@
    Chars(65) = "d_a"     'A
    Chars(66) = "d_b"     'B
    Chars(67) = "d_c"     'C
    Chars(68) = "d_d"     'D
    Chars(69) = "d_e"     'E
    Chars(70) = "d_f"     'F
    Chars(71) = "d_g"     'G
    Chars(72) = "d_h"     'H
    Chars(73) = "d_i"     'I
    Chars(74) = "d_j"     'J
    Chars(75) = "d_k"     'K
    Chars(76) = "d_l"     'L
    Chars(77) = "d_m"     'M
    Chars(78) = "d_n"     'N
    Chars(79) = "d_o"     'O
    Chars(80) = "d_p"     'P
    Chars(81) = "d_q"     'Q
    Chars(82) = "d_r"     'R
    Chars(83) = "d_s"     'S
    Chars(84) = "d_t"     'T
    Chars(85) = "d_u"     'U
    Chars(86) = "d_v"     'V
    Chars(87) = "d_w"     'W
    Chars(88) = "d_x"     'X
    Chars(89) = "d_y"     'Y
    Chars(90) = "d_z"     'Z
    Chars(94) = "d_up"    '^
    '    Chars(95) = '_
    Chars(96) = ""  '`
    Chars(97) = ""  'a
    Chars(98) = ""  'b
    Chars(99) = ""  'c
    Chars(100) = "" 'd
    Chars(101) = "" 'e
    Chars(102) = "" 'f
    Chars(103) = "" 'g
    Chars(104) = "" 'h
    Chars(105) = "" 'i
    Chars(106) = "" 'j
    Chars(107) = "" 'k
    Chars(108) = "" 'l
    Chars(109) = "" 'm
    Chars(110) = "" 'n
    Chars(111) = "" 'o
    Chars(112) = "" 'p
    Chars(113) = "" 'q
    Chars(114) = "" 'r
    Chars(115) = "" 's
    Chars(116) = "" 't
    Chars(117) = "" 'u
    Chars(118) = "" 'v
    Chars(119) = "" 'w
    Chars(120) = "" 'x
    Chars(121) = "" 'y
    Chars(122) = "" 'z
    Chars(123) = "" '{
    Chars(124) = "" '|
    Chars(125) = "" '}
    Chars(126) = "" '~
    ' numbers with the dot
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

'********************
' Real Time updates
'********************
'used for all the real time updates

Sub Realtime_Timer
    RollingUpdate
End Sub

Sub LeftLaneGate_Animate:LeftDiverter.RotZ = LeftLaneGate.CurrentAngle:End Sub
Sub RightLaneGate_Animate:RightDiverter.RotZ = RightLaneGate.CurrentAngle:End Sub

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
' 11 colors: red, orange, amber, yellow...
'******************************************
' in this table this colors are use to keep track of the progress during the modes

'colors
Const red = 10
Const orange = 9
Const amber = 8
Const yellow = 7
Const darkgreen = 6
Const green = 5
Const blue = 4
Const darkblue = 3
Const purple = 2
Const white = 1
Const teal = 0

Sub SetLightColor(n, col, stat) 'stat 0 = off, 1 = on, 2 = blink, -1= no change
    Select Case col
        Case red
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(128, 0, 0)
        Case orange
            n.color = RGB(18, 3, 0)
            n.colorfull = RGB(255, 64, 0)
        Case amber
            n.color = RGB(193, 49, 0)
            n.colorfull = RGB(255, 153, 0)
        Case yellow
            n.color = RGB(18, 18, 0)
            n.colorfull = RGB(255, 240, 0)
        Case darkgreen
            n.color = RGB(0, 8, 0)
            n.colorfull = RGB(0, 64, 0)
        Case green
            n.color = RGB(0, 16, 0)
            n.colorfull = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 8, 18)
            n.colorfull = RGB(0, 64, 128)
        Case darkblue
            n.color = RGB(0, 8, 8)
            n.colorfull = RGB(0, 64, 64)
        Case purple
            n.color = RGB(64, 0, 96)
            n.colorfull = RGB(128, 0, 192)
        Case white
            n.color = RGB(255, 197, 143)
            n.colorfull = RGB(255, 252, 224)
        Case teal
            n.color = RGB(1, 64, 62)
            n.colorfull = RGB(2, 128, 126)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

Sub SetFlashColor(n, col, stat) 'stat 0 = off, 1 = on, -1= no change - no blink for the flashers
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
            n.color = RGB(255, 252, 224)
        Case teal
            n.color = RGB(2, 128, 126)
    End Select
    If stat <> -1 Then
        n.Visible = stat
    End If
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
    TurnOffArrows
End Sub

Sub TurnOffArrows() 'during Missions when changing modes
    For each x in aArrows
        x.State = 0
    Next
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
            If rRed < 0 then
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
            If rGreen < 0 then
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
            If rBlue < 0 then
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
    If Score(1) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 1 " &FormatScore(Score(1) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 2 " &FormatScore(Score(2) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 3 " &FormatScore(Score(3) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 4 " &FormatScore(Score(4) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    DMD "", CL(1, "GAME OVER"), "", eNone, eNone, eNone, 2000, False, ""
    If bFreePlay Then
        DMD "", CL(1, "FREE PLAY"), "", eNone, eNone, eNone, 2000, False, ""
    Else
        If Credits> 0 Then
            DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "        JPSALAS AND", "          MESTERIAL", "d_jppresents", eNone, eNone, eNone, 2500, False, ""
    DMD "", "          PRESENT", "d_jppresents", eNone, eNone, eNone, 1700, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 4000, False, ""
    DMD "", CL(1, "ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0) ), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    SetLightColor li080, yellow, 1
    SetLightColor li081, red, 1
    SetLightColor li082, green, 1
    StartRainbow BumperLights
    StartLightSeq
    DMDFlush
    ShowTableInfo
    StopSound "mu_main"
    StopSound "mu_mission"
    StopSound "mu_multiball"
    StopSound "mu_wait"
    if songplayed = 1 Then
        Playsound "mu_end"
    Else
        playsound "silence"
    end If
    songplayed = songplayed + 1
End Sub

Sub StopAttractMode
    StopRainbow
    DMDScoreNow
    LightSeqAttract.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 100
    LightSeqAttract.Play SeqRandom, 50, , 1000
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqAllOff
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, timers, etc
Sub VPObjects_Init
    LowerPost
    LeftLaneGate.RotateToEnd
    RightLaneGate.RotateToEnd
End Sub

' tables variables and Mode init
Dim BallsinWormHole
Dim WormHoleColor
Dim WormholeMBStep
Dim CurrentWormhole
Dim AttackBumperColor
Dim EngineBumperColor
Dim bFlagsUpgraded
Dim HyperSpaceHits
Dim bJackpotActivated
Dim bReflexRampShot
Dim bReflexHyperShot
Dim Fuel
Dim BoosterTarget1
Dim BoosterTarget2
Dim BoosterTarget3
Dim BoosterCount
Dim bBonusActivated
Dim MedalTarget1
Dim MedalTarget2
Dim MedalTarget3
Dim MedalCount
Dim xTarget1
Dim xTarget2
Dim xTarget3
Dim xTargetCount
Dim bMissionBonus
Dim Mission(4)
Dim Progress(4)
Dim Rank(4)
Dim MissionSelected
Dim MissionTarget1
Dim MissionTarget2
Dim MissionTarget3
Dim MissionHits 'hits to finish each Mission

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    'Play some Music

    'Init Variables
    WormholeMBStep = 0
    MissionSelected = 0
    MissionHits = 0
    Fuel = 6
    For i = 0 to 4
        BonusPoints(i) = 10000 'crash bonus start points
        BonusMultiplier(i) = 1
        PlayfieldMultiplier(i) = 1
        BallsInLock(i) = 0
        Progress(i) = -1 'because the lights array starts at 0 and counts as 1
        Rank(i) = 0
        Mission(i) = 0
    Next
'MainMode Init()
End Sub

Sub InstantInfo
    DMD CL(0, "INSTANT INFO"), "", "", eScrollLeft, eNone, eNone, 1000, False, ""
    Select Case Mission(CurrentPlayer)
        Case 0
        case 1 'Launch Training 'top target
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "LAUNCH TRAINING"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "PASS THE LAUNCH"), CL(1, "RAMP 3 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 2 'Re-entry Training 'middle target
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "RE-ENTRY TRAINING"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "PASS THE RE-ENTRY"), CL(1, "LANES 3 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 3 'Target Practice 'lower target
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "TARGET PRACTICE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT THE ATTACK"), CL(1, "BUMPERS 8 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 4 'Science Mission 'all three targets
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "SCIENCE MISSION"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT 9"), CL(1, "DROPTARGETS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 5 'Bug Hunt Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "BUG HUNT MISSION"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT 15"), CL(1, "TARGETS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 6, 61 'Rescue Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "RESCUE MISSION"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "UPGRADE FLAGS +"), CL(1, "ENTER HYPERSPACE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 7, 71 'Alien Menace Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "ALIEN MENACE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "UPGRADE THE"), CL(1, "ATTACK BUMPERS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT THE ATTACK"), CL(1, "BUMPERS 8 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 8, 81, 82 'Secret Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "SECRET MISSION"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "ENTER THE 3"), CL(1, "WORMHOLES IN ORDER"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 9, 91 'Stray Comet Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "STRAY COMET"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "LIGHT ALL 3"), CL(1, "COMET LIGHTS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "ENTER THE"), CL(1, "HYPERSPACE KICKER"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 10, 101 'Space Radiation Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "SPACE RADIATION"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "LIGHT ALL 3"), CL(1, "RADIATION LIGHTS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "ENTER ANY"), CL(1, "WORMHOLE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 11, 111 'Black Hole Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "BLACK HOLE THREAT"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "UPGRADE THE"), CL(1, "ENGINE BUMPERS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "ENTER THE"), CL(1, "BLACK HOLE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 12, 121 'Cosmic Plague Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "COSMIC PLAGUE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "SPIN FLAGS"), CL(1, "75 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT THE"), CL(1, "SPACE WARP ROLLOVER"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 13 'Satellite Retrieval Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "SATELLITE RETRIEVAL"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT 3 TIMES"), "THE SATELLITE BUMPER", "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 14 'Recon Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "RECON MISSION"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "PASS ANY OF THE"), CL(1, "LANES 15 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 15 'Doomsday Machine Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "DOOMSDAY MACHINE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "SEND BALL 3 TIMES"), CL(1, "THROUGH OUTLINES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 16, 161 'Time Warp Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "TIME WARP"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT THE REBOUNDS"), CL(1, "25 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "ENTER LAUNCH RAMP"), "OR HYPERSPACE KICKER", "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 17, 171, 172, 173, 174, 175, 176, 177 'Maelstrom
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "MAELSTROM"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "HIT 3 DROPTARGETS"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "HIT 3 SPOT TARGETS"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "HIT 5 LANES"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "HIT THE FUEL CHUTE"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "HIT THE LAUNCH RAMP"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "ROLL A FLAG"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "ENTER A WORMHOLE"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "ENTER HYPERSPACE"), "", eNone, eScrollLeft, eNone, 1500, True, ""
    End Select
    DMD CL(0, "BONUS POINTS"), CL(1, FormatScore(BonusPoints(CurrentPlayer) ) ), "", eScrollLeft, eScrollLeft, eNone, 1500, False, ""
    DMD CL(0, "JACKPOT POINTS"), CL(1, FormatScore(Jackpot(CurrentPlayer) ) ), "", eScrollLeft, eScrollLeft, eNone, 1500, False, ""

    If Score(1) Then
        DMD CL(0, "PLAYER 1 SCORE"), CL(1, FormatScore(Score(1) ) ), "", eScrollLeft, eScrollLeft, eNone, 3000, False, ""
    End If
    If Score(2) Then
        DMD CL(0, "PLAYER 2 SCORE"), CL(1, FormatScore(Score(2) ) ), "", eScrollLeft, eScrollLeft, eNone, 3000, False, ""
    End If
    If Score(3) Then
        DMD CL(0, "PLAYER 3 SCORE"), CL(1, FormatScore(Score(3) ) ), "", eScrollLeft, eScrollLeft, eNone, 3000, False, ""
    End If
    If Score(4) Then
        DMD CL(0, "PLAYER 4 SCORE"), CL(1, FormatScore(Score(4) ) ), "", eScrollLeft, eScrollLeft, eNone, 3000, False, ""
    End If
End Sub

Sub StopMBmodes 'stop multiball modes after loosing the last multibal
    PlaySong Lastsong
    AsteroidsTimer.Enabled = 0
    hyperspacelightpursuitTimer.Enabled = 0
    maellightpursuitTimer.Enabled = 0
    HyperSpaceHits = 0
    UpdateHyperLights
    maeljack = 0
    bMael = 0
    UpdateProgressLights
    li049.State = 0
    li050.State = 0
    li051.State = 0
    li099.state = 0
End Sub

Sub StopEndOfBallMode() 'this sub is called after the last ball in play is drained, reset skillshot, modes, timers
    DisableGravity
    StopMissions
    DowngradeEngineTimer.Enabled = 0:EngineBumperColor = 0
    DowngradeWeaponsTimer.Enabled = 0:AttackBumperColor = 0
    pfXTimerExpired.Enabled = 0:xTargetCount = 0
    BoosterTimerExpired.Enabled = 0:BoosterCount = 0
    FuelSpeedUpTimer.Enabled = 0
    FuelTimerExpired.Enabled = 0
    HyperExpiredTimer.Enabled = 0:HyperSpaceHits = 0
End Sub

Sub ResetNewBallVariables() 'reset objects, variables and lights for a new ball or player
    'lights
    TurnOffPlayfieldLights
    'objects
    OpenLeftGate
    vpmtimer.Addtimer 250, "OpenRightGate '"
    vpmtimer.Addtimer 500, "LowerPost '"
    'variables
    WormHoleColor = 0
    CurrentWormhole = 0
    BallsinWormHole = 0
    AttackBumperColor = 0
    EngineBumperColor = 0
    UpdateBumperColor
    bFlagsUpgraded = False
    BonusPoints(CurrentPlayer) = 10000
    bBonusActivated = False
    bBonusHeld = False
    HyperSpaceHits = 0
    UpdateHyperLights
    Jackpot(CurrentPLayer) = 20000
    bJackpotActivated = False
    StopReflexRampShot
    StopReflexHyperShot
    vpmtimer.Addtimer 750, "ResetBoosterTargets '"
    BoosterCount = 0
    UpdateBoosterLights
    bFlagsUpgraded = False
    bJackpotActivated = False
    vpmtimer.Addtimer 1000, "ResetMedalDT '"
    MedalCount = 0
    UpdateMedal
    vpmtimer.Addtimer 1250, "ResetXtargets '"
    PlayfieldMultiplier(CurrentPlayer) = 1
    xTargetCount = 0
    UpdatexTargets
    bMissionBonus = False
    PrepareForMission
    UpdateRankLights
    UpdateProgressLights
End Sub

Sub TurnOffPlayfieldLights()
    For each i in aLights
        i.State = 0
    Next
End Sub

Sub UpdateSkillShot()                'Setup and updates the skillshot lights
    DMD FL(0, "P" &CurrentPlayer& " B" &Balls, FormatScore(Score(Currentplayer) ) ), "HIT SKILLSHOT LIGHTS", "", eNone, eblink, eNone, 3000, True, ""
    LightSeqSkillshot.Play SeqAllOff 'turn off all the lights but the skillshot lights
    For each i in aSkillshotLights2:i.State = 2:Next
End Sub

Sub StopSkillshot() 'turn off the skillshot lights
    DMDScoreNow
    LightSeqSkillshot.StopPlay
    For each i in aSkillshotLights2:i.State = 0:Next
    bSkillshotReady = False
    bBallInPlungerLane = False
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

'*********************************************************
' Slingshots has been hit

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Lemk
    PlaySound "sc_slingshot"
    DOF 105, DOFPulse
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 500
    ' check modes
    ' add some effect to the table?
    FlashForMs gi002, 500, 50, 0
    FlashForMs gi004, 500, 50, 0
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 16
            MissionHits = MissionHits-1:CheckMission
    End Select
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
    PlaySoundAt SoundFXDOF("fx_slingshot", 104, DOFPulse, DOFcontactors), Remk
    PlaySound "sc_slingshot"
    DOF 106, DOFPulse
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 500
    ' check modes
    ' add some effect to the table?
    FlashForMs gi001, 500, 50, 0
    FlashForMs gi003, 500, 50, 0
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 16
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' Top Slingshots has been hit

Dim LStep2, RStep2

Sub LeftSlingShot2_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 131, DOFPulse, DOFcontactors), Lemk2 ' Outhere
    PlaySound "sc_slingshot"
    DOF 105, DOFPulse
    Rubber018.Visible = 1
    Lemk2.RotX = 26
    LStep2 = 0
    LeftSlingShot2.TimerEnabled = True
    ' add some points
    AddScore 500
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 16
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub LeftSlingShot2_Timer
    Select Case LStep2
        Case 1:Rubber018.Visible = 0:Rubber017.Visible = 1:Lemk2.RotX = 14
        Case 2:Rubber017.Visible = 0:Rubber016.Visible = 1:Lemk2.RotX = 2
        Case 3:Rubber016.Visible = 0:Lemk2.RotX = -10:LeftSlingShot2.TimerEnabled = 0
    End Select
    LStep2 = LStep2 + 1
End Sub

Sub RightSlingShot2_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 132, DOFPulse, DOFcontactors), Remk2 ' Outhere
    PlaySound "sc_slingshot"
    DOF 106, DOFPulse
    Rubber015.Visible = 1
    Remk2.RotX = 26
    RStep2 = 0
    RightSlingShot2.TimerEnabled = True
    ' add some points
    AddScore 500
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 16
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub RightSlingShot2_Timer
    Select Case RStep2
        Case 1:Rubber015.Visible = 0:Rubber014.Visible = 1:Remk2.RotX = 14
        Case 2:Rubber014.Visible = 0:Rubber013.Visible = 1:Remk2.RotX = 2
        Case 3:Rubber013.Visible = 0:Remk2.RotX = -10:Rightslingshot2.TimerEnabled = 0
    End Select
    RStep2 = RStep2 + 1
End Sub

'***********************
'    Attack Bumpers
'***********************

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper1
    PlaySound "sc_bumper"
    DOF 138, DOFPulse
    FlashForms li135, 1500, 75, 1
    ' add some points
    Select Case AttackBumperColor
        Case 0:AddScore 500
        Case 1:AddScore 1000
        Case 2:AddScore 1500
        Case 3:AddScore 2000
    End Select
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 3, 71:MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), Bumper2
    PlaySound "sc_bumper"
    DOF 138, DOFPulse
    FlashForms li136, 1500, 75, 1
    ' add some points
    Select Case AttackBumperColor
        Case 0:AddScore 500
        Case 1:AddScore 1000
        Case 2:AddScore 1500
        Case 3:AddScore 2000
    End Select
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 3, 71:MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper3
    PlaySound "sc_bumper"
    DOF 138, DOFPulse
    FlashForms li137, 1500, 75, 1
    ' add some points
    Select Case AttackBumperColor
        Case 0:AddScore 500
        Case 1:AddScore 1000
        Case 2:AddScore 1500
        Case 3:AddScore 2000
    End Select
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 3, 71:MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Bumper4_Hit                                                                'Satellite bumper
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 113, DOFPulse, DOFContactors), Bumper4 ' Outhere
    PlaySound "sc_bumper"
    DOF 138, DOFPulse
    FlashForms li138, 1500, 75, 1
    ' add some points
    Select Case AttackBumperColor
        Case 0:AddScore 1500
        Case 1:AddScore 2500
        Case 2:AddScore 3500
        Case 3:AddScore 4500
    End Select
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 13
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub UpgradeWeapons 'upgrade the color of the weapon bumpers
    Bumper1.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper1
    Bumper2.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper2
    Bumper3.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper3
    Bumper4.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper4
    AttackBumperColor = AttackBumperColor + 1
    DMDFlush
    DMD "_", CL(1, "WEAPONS UPGRADED"), "", eNone, eBlink, eNone, 1500, True, "" '"vo_Weapons Upgraded"
    If AttackBumperColor> 3 then AttackBumperColor = 3
    DowngradeWeaponsTimer.Enabled = 0
    DowngradeWeaponsTimer.Enabled = 1
    UpdateBumperColor
End Sub

Sub DowngradeWeapons 'upgrade the color of the weapon bumpers
    Bumper1.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper1
    Bumper2.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper2
    Bumper3.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper3
    Bumper4.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper4
    AttackBumperColor = AttackBumperColor - 1
    If AttackBumperColor <= 0 then
        AttackBumperColor = 0
        DowngradeWeaponsTimer.Enabled = 0
    End If
    UpdateBumperColor
End Sub

Sub DowngradeWeaponsTimer_Timer
    DowngradeWeapons
End Sub

Sub UpdateBumperColor
    Select case AttackBumperColor
        Case 0 'blue
            For each i in RightBumperLights:SetLightColor i, Blue, 1:next
        Case 1 'green
            For each i in RightBumperLights:SetLightColor i, Green, 1:next
        Case 2 'yellow
            For each i in RightBumperLights:SetLightColor i, Yellow, 1:next
        Case 3 'red
            For each i in RightBumperLights:SetLightColor i, Red, 1:next
    End Select
    Select case EngineBumperColor
        Case 0 'blue
            For each i in LeftBumperLights:SetLightColor i, Blue, 1:next
        Case 1 'green
            For each i in LeftBumperLights:SetLightColor i, Green, 1:next
        Case 2 'yellow
            For each i in LeftBumperLights:SetLightColor i, Yellow, 1:next
        Case 3 'red
            For each i in LeftBumperLights:SetLightColor i, Red, 1:next
    End Select
End Sub

'***********************
'    Engine Bumpers
'***********************

Sub Bumper5_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 110, DOFPulse, DOFContactors), Bumper5 ' Outhere
    DOF 138, DOFPulse
    PlaySound "sc_bumper"
    FlashForms li139, 1500, 75, 1
    ' add some points
    Select Case EngineBumperColor
        Case 0:AddScore 1500
        Case 1:AddScore 2500
        Case 2:AddScore 3500
        Case 3:AddScore 4500
    End Select
    ' check for modes
    Select Case Mission(CurrentPLayer)
        Case 0
    End Select
End Sub

Sub Bumper6_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 111, DOFPulse, DOFContactors), Bumper6 ' Outhere
    DOF 138, DOFPulse
    PlaySound "sc_bumper"
    FlashForms li140, 1500, 75, 1
    ' add some points
    Select Case EngineBumperColor
        Case 0:AddScore 1500
        Case 1:AddScore 2500
        Case 2:AddScore 3500
        Case 3:AddScore 4500
    End Select
    ' check for modes
    Select Case Mission(CurrentPLayer)
        Case 0
    End Select
End Sub

Sub Bumper7_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_bumper", 112, DOFPulse, DOFContactors), Bumper7 ' Outhere
    DOF 138, DOFPulse
    PlaySound "sc_bumper"
    FlashForms li141, 1500, 75, 1
    ' add some points
    Select Case EngineBumperColor
        Case 0:AddScore 1500
        Case 1:AddScore 2500
        Case 2:AddScore 3500
        Case 3:AddScore 4500
    End Select
    ' check for modes
    Select Case Mission(CurrentPLayer)
        Case 0
    End Select
End Sub

Sub UpgradeEngine 'upgrade the color of the engine bumper
    Bumper5.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper5
    Bumper6.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper6
    Bumper7.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper7
    EngineBumperColor = EngineBumperColor + 1
    If EngineBumperColor> 3 then EngineBumperColor = 3
    DMDFlush
    DMD "_", CL(1, "ENGINE UPGRADED"), "", eNone, eBlink, eNone, 1500, True, "" '"vo_Engines Upgraded"
    DowngradeEngineTimer.Enabled = 0
    DowngradeEngineTimer.Enabled = 1
    UpdateBumperColor
    If Mission(CurrentPlayer) = 11 Then CheckMission
End Sub

Sub DowngradeEngine 'upgrade the color of the engine bumper
    Bumper5.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper5
    Bumper6.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper6
    Bumper7.playhit
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper7
    EngineBumperColor = EngineBumperColor - 1
    If EngineBumperColor <= 0 then
        EngineBumperColor = 0
        DowngradeEngineTimer.Enabled = 0
    End If
    UpdateBumperColor
End Sub

Sub DowngradeEngineTimer_Timer
    DowngradeEngine
End Sub

' Rotate lane lights
Sub RotateLaneLights(n) 'n is the direction, 1 or 0, left or right
    Dim tmp
    If n = 1 Then
        tmp = li124.State
        li124.State = li125.State
        li125.State = li126.State
        li126.State = tmp
        tmp = li063.State
        li063.State = li064.State
        li064.State = li065.State
        li065.State = tmp
    Else
        tmp = li126.State
        li126.State = li125.State
        li125.State = li124.State
        li124.State = tmp
        tmp = li065.State
        li065.State = li064.State
        li064.State = li063.State
        li063.State = tmp
    End If
End Sub

sub Triggerendramp_unHit
    wall062.HeightBottom = 40
    wall062.IsDropped = 0
    bmagnet.MagnetOn = 1
    kicker001timer.enabled = 1
end Sub

sub kicker001timer_timer
    wall062.HeightBottom = -1
    wall062.IsDropped = 1
    bmagnet.MagnetOn = 0
    kicker001timer.enabled = 0
end Sub

'*************
' Lower lanes
'*************

Sub Trigger001_Hit 'left outlane
    PlaySoundAt "fx_sensor", Trigger001
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 20000
    If li032.State = 1 Then AwardExtraBall:li032.State = 0
    Select Case Mission(CurrentPlayer)
        case 14, 15, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger002_Hit 'left inlane, re-fuel, Bonus lane
    PlaySoundAt "fx_sensor", Trigger002
    If Tilted Then Exit Sub
    AddScore 10000
    'Mission Bonus AND/OR Booster Bonus activated
    If bMissionBonus Then
        playsound "SOUND50"
        DMDFlush
        DMD "_", CL(1, "BONUS AWARDED"), "", eNone, eNone, eNone, 1500, True, "" '"vo_Bonus Awarded"
        AddScore BonusHeldPoints(CurrentPlayer)
        bMissionBonus = False:li034.State = 0
    ElseIf bBonusActivated Then
        DMDFlush
        DMD "_", CL(1, "BONUS AWARDED"), "", eNone, eNone, eNone, 1500, True, "" '"vo_Bonus Awarded"
        AddScore BonusHeldPoints(CurrentPlayer)
        li034.State = 0
    Else
        playsound "sc_shiprefueled"
        DMDFlush
        DMD "", CL(1, "SHIP RE-FUELED"), "", eNone, eNone, eNone, 1500, True, ""
    End If
    're-fuel
    Fuel = 6:UpdateFuelLights:StartFuelTimers
    Select Case Mission(CurrentPlayer)
        case 14, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger003_Hit 'left inlane
    PlaySoundAt "fx_sensor", Trigger003
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 5000
    If li033.State = 1 Then AddScore 20000
    li033.State = 0
    Select Case Mission(CurrentPlayer)
        case 14, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger004_Hit 'right inlane
    PlaySoundAt "fx_sensor", Trigger004
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 5000
    If li035.State = 1 Then AddScore 20000
    li035.State = 0
    Select Case Mission(CurrentPlayer)
        case 14, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger005_Hit 'right outlane
    PlaySoundAt "fx_sensor", Trigger005
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 20000
    If li036.State = 1 Then AwardExtraBall:li036.State = 0
    Select Case Mission(CurrentPlayer)
        case 14, 15, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

'***********
' Top lanes
'***********

Sub Trigger006_Hit 'left
    PlaySoundAt "fx_sensor", Trigger006
    If Tilted Then Exit Sub
    If bSkillShotready Then StopSkillShot
    playSound "sc_lane"
    Addscore 2000
    li063.State = 1
    CheckWeaponLights
    Select Case Mission(CurrentPlayer)
        case 2, 14, 172:MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger007_Hit 'center
    PlaySoundAt "fx_sensor", Trigger007
    If Tilted Then Exit Sub
    If bSkillShotready Then StopSkillShot
    playSound "sc_lane"
    Addscore 2000
    li064.State = 1
    CheckWeaponLights
    Select Case Mission(CurrentPlayer)
        case 2, 14, 172:MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger008_Hit 'right
    PlaySoundAt "fx_sensor", Trigger008
    If Tilted Then Exit Sub
    If bSkillShotready Then StopSkillShot
    playSound "sc_lane"
    Addscore 2000
    li065.State = 1
    CheckWeaponLights
    Select Case Mission(CurrentPlayer)
        case 2, 14, 172:MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub CheckWeaponLights
    If li063.State + li064.State + li065.State = 3 then
        UpgradeWeapons
        li063.State = 0
        li064.State = 0
        li065.State = 0
        LightSeqReEntry.Play SeqBlinking, , 15, 10

        If Mission(CurrentPlayer) = 7 Then CheckMission
    End If
End Sub

'**************
' Engine lanes
'**************

Sub Trigger009_Hit 'left
    PlaySoundAt "fx_sensor", Trigger009
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 500
    li124.State = 1
    CheckEngineLights
    Select Case Mission(CurrentPlayer)
        case 14
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger010_Hit 'center
    PlaySoundAt "fx_sensor", Trigger010
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 500
    li125.State = 1
    CheckEngineLights
    Select Case Mission(CurrentPlayer)
        case 14, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger011_Hit 'right
    PlaySoundAt "fx_sensor", Trigger011
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 500
    li126.State = 1
    CheckEngineLights
    Select Case Mission(CurrentPlayer)
        case 14, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub CheckEngineLights
    If li124.State + li125.State + li126.State = 3 then
        UpgradeEngine
        li124.State = 0
        li125.State = 0
        li126.State = 0
        LightSeqEngine.Play SeqBlinking, , 15, 10
    End If
End Sub

Sub Trigger030_Hit 'ramp done
    dim c
    If Tilted Then Exit Sub
    Addscore 5000:c = 1
    If bReflexRampShot Then
        AwardReflexRampShot:c = 2
        StartReflexRampShot
    Else
        StartReflexRampShot
    End If

    select Case c
        Case 1:PLaySound "sc_end"
        Case 2:Playsound "SOUND21"
    end Select
    Select case Mission(CurrentPlayer)
        Case 0 'no mission active
            If MissionSelected Then AcceptMission MissionSelected
        Case 1:MissionHits = MissionHits-1:CheckMission
        Case 161:CheckMission
        Case 174:CheckMission
    End Select
End Sub

Sub Trigger024_Hit
    if bBallInPlungerLane Then
        bBallInPlungerLane = False
    End If
    If bSkillShotReady Then
        PlaySong "mu_main"
        StopSkillShot
    End If
'If bSkillShotready Then
End Sub
Sub Trigger029_unHit 'give skillshot
    if bBallInPlungerLane Then
        bBallInPlungerLane = False
    End If
End Sub

'**************
'  Fuel lanes
'**************

Sub Trigger012_Hit
    PlaySoundAt "fx_sensor", Trigger012
    If Tilted Then Exit Sub
    PlaySound "sc_fuellights"
    Addscore 500
    If Fuel < 1 Then
        Flashforms li128, 500, 50, 1
        Fuel = 1:StartFuelTimers
    Else
        Flashforms li128, 500, 50, 2
    End If
End Sub

Sub Trigger013_Hit
    PlaySoundAt "fx_sensor", Trigger013
    If Tilted Then Exit Sub
    PlaySound "sc_fuellights"
    Addscore 500
    If Fuel < 2 Then
        Flashforms li129, 500, 50, 1
        Fuel = 2:StartFuelTimers
    Else
        Flashforms li129, 500, 50, 2
    End If
End Sub

Sub Trigger014_Hit
    PlaySoundAt "fx_sensor", Trigger014
    If Tilted Then Exit Sub
    PlaySound "sc_fuellights"
    Addscore 500
    If Fuel < 3 Then
        Flashforms li130, 500, 50, 1
        Fuel = 3:StartFuelTimers
    Else
        Flashforms li130, 500, 50, 2
    End If
End Sub

Sub Trigger015_Hit
    PlaySoundAt "fx_sensor", Trigger015
    If Tilted Then Exit Sub
    PlaySound "sc_fuellights"
    Addscore 500
    If Fuel < 4 Then
        Flashforms li131, 500, 50, 1
        Fuel = 4:StartFuelTimers
    Else
        Flashforms li131, 500, 50, 2
    End If
End Sub

Sub Trigger016_Hit
    PlaySoundAt "fx_sensor", Trigger016
    If Tilted Then Exit Sub
    PlaySound "sc_fuellights"
    Addscore 500
    If Fuel < 5 Then
        Flashforms li131, 500, 50, 1
        Fuel = 5:StartFuelTimers
    Else
        Flashforms li131, 500, 50, 2
    End If
End Sub

Sub Trigger017_Hit
    PlaySoundAt "fx_sensor", Trigger017
    If Tilted Then Exit Sub
    'PlaySound "sc_fuellights"
    Addscore 500
    If li133.State = 0 Then Addscore 1000
    're-fuel
    DMDFlush
    DMD "", CL(1, "SHIP RE-FUELED"), "", eNone, eNone, eNone, 1000, True, "sc_shiprefueled"
    Fuel = 6:UpdateFuelLights:StartFuelTimers
    If Mission(CurrentPlayer) = 173 Then CheckMission
End Sub

'**************
' Fuel Timers
'**************

Sub StartFuelTimers
    FuelSpeedUpTimer.Enabled = 0
    FuelTimerExpired.Enabled = 0
    FuelSpeedUpTimer.Enabled = 1
    FuelTimerExpired.Enabled = 1
End Sub

Sub FuelTimerExpired_Timer 'reduces the fuel after 15 seconds
    Fuel = Fuel - 1
    UpdateFuelLights
    If Fuel = 0 then 'Stop Mission
        playsound "SOUND42"
        StopMissions
    End If
End Sub

Sub FuelSpeedUpTimer_Timer 'blinks the light 5 seconds before it expires
    Select Case Fuel
        Case 0:li128.State = 0:li129.State = 0:li130.State = 0:li131.State = 0:li132.State = 0:li133.State = 0
        Case 1:li128.State = 2:li129.State = 0:li130.State = 0:li131.State = 0:li132.State = 0:li133.State = 0
        Case 2:li128.State = 1:li129.State = 2:li130.State = 0:li131.State = 0:li132.State = 0:li133.State = 0
        Case 3:li128.State = 1:li129.State = 1:li130.State = 2:li131.State = 0:li132.State = 0:li133.State = 0
        Case 4:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 2:li132.State = 0:li133.State = 0
        Case 5:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 1:li132.State = 2:li133.State = 0
        Case 6:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 1:li132.State = 1:li133.State = 2
    End Select
End Sub

Sub UpdateFuelLights
    Select case Fuel
        Case 0:li128.State = 0:li129.State = 0:li130.State = 0:li131.State = 0:li132.State = 0:li133.State = 0
        Case 1:li128.State = 1:li129.State = 0:li130.State = 0:li131.State = 0:li132.State = 0:li133.State = 0
        Case 2:li128.State = 1:li129.State = 1:li130.State = 0:li131.State = 0:li132.State = 0:li133.State = 0
        Case 3:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 0:li132.State = 0:li133.State = 0
        Case 4:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 1:li132.State = 0:li133.State = 0
        Case 5:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 1:li132.State = 1:li133.State = 0
        Case 6:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 1:li132.State = 1:li133.State = 1
    End Select
End Sub

'*****************
' Skillshot lanes
'*****************

Sub Trigger018_Hit
    PlaySoundAt "fx_sensor", Trigger018
    If Tilted Then Exit Sub
    If bSkillShotready Then
        If ActiveBall.VelY < 0 Then 'only when the ball is going up
            PlaySound "sc_skillshotlight"
            li113.State = 1
            SkillShotValue(CurrentPLayer) = 15000
        End If
    End If
End Sub

Sub Trigger019_Hit
    PlaySoundAt "fx_sensor", Trigger019
    If Tilted Then Exit Sub
    If bSkillShotready Then
        If ActiveBall.VelY < 0 Then 'only when the ball is going up
            PlaySound "sc_skillshotlight"
            li114.State = 1
            SkillShotValue(CurrentPLayer) = 30000
        End If
    End If
End Sub

Sub Trigger020_Hit
    PlaySoundAt "fx_sensor", Trigger020
    If Tilted Then Exit Sub
    If bSkillShotready Then
        If ActiveBall.VelY < 0 Then 'only when the ball is going up
            PlaySound "sc_skillshotlight"
            li115.State = 1
            SkillShotValue(CurrentPLayer) = 75000
        End If
    End If
End Sub

Sub Trigger021_Hit
    PlaySoundAt "fx_sensor", Trigger021
    If Tilted Then Exit Sub
    If bSkillShotready Then
        If ActiveBall.VelY < 0 Then 'only when the ball is going up
            PlaySound "sc_skillshotlight"
            li116.State = 1
            SkillShotValue(CurrentPLayer) = 30000
        End If
    End If
End Sub

Sub Trigger022_Hit
    PlaySoundAt "fx_sensor", Trigger022
    If Tilted Then Exit Sub
    If bSkillShotready Then
        If ActiveBall.VelY < 0 Then 'only when the ball is going up
            PlaySound "sc_skillshotlight"
            li117.State = 1
            SkillShotValue(CurrentPLayer) = 15000
        End If
    End If
End Sub

Sub Trigger023_Hit
    PlaySoundAt "fx_sensor", Trigger023
    If Tilted Then Exit Sub
    If bSkillShotready Then
        If ActiveBall.VelY < 0 Then 'only when the ball is going up
            PlaySound "sc_skillshotlight"
            li118.State = 1
            SkillShotValue(CurrentPLayer) = 7500
        End If
    End If
End Sub

Sub Trigger029_Hit 'give skillshot
    PlaySoundAt "fx_sensor", Trigger029
    If Tilted Then Exit Sub
    If bSkillShotready Then
        AwardSkillshot
        PlaySong "mu_main"
    End If
End Sub

'*******************
' Space Warp trigger
'*******************

Sub Trigger028_Hit
    PlaySoundAt "fx_sensor", Trigger028
    If Tilted Then Exit Sub
    PlaySound "sc_wormholetarget"
    FlashForMs li092, 1000, 50, 0
    AddScore 10000
    li033.State = 1 'turn on the inlane lights for extra scoring
    li035.State = 1
    If Mission(CurrentPlayer) = 121 Then CheckMission
End Sub

'*****************
' Wormhole target
'*****************

Sub Target001_Hit
    PlaySoundAt "fx_target", Target001
    If Tilted Then Exit Sub
    Addscore 750
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
        case 8, 81, 82, 101, 176 'do nothing
        case else
            'li047.state=1
            'LightSeqWormtarget.play SeqBlinking, , 50, 10
            'vpmtimer.addtimer 1000, "LightSeqWormtarget.stopplay'"
            Flashforms li047, 1000, 50, 1
            PlaySound "sc_wormholetarget"
            ' activate Wormholes and change the color of the Wormhole lights
            li142.State = 1
            li143.State = 1
            li144.State = 1
            If WormholeColor = 0 Then WormholeColor = RndNbr(3) 'activate wormholes by giving them a color
            ChangeArrowColor
    End Select
End Sub

Sub ChangeArrowColor
    If WormholeColor = 0 Then Exit Sub 'if it is called from the spinners and the wormholes are not activated
    WormHoleColor = WormHoleColor + 1
    If WormHoleColor> 3 Then WormHoleColor = 1
    Select case WormHoleColor
        Case 1
            SetLightColor li080, yellow, 1
            SetLightColor li081, yellow, 1
            SetLightColor li082, yellow, 1
        Case 2
            SetLightColor li080, red, 1
            SetLightColor li081, red, 1
            SetLightColor li082, red, 1
        Case 3
            SetLightColor li080, green, 1
            SetLightColor li081, green, 1
            SetLightColor li082, green, 1
    End Select
End Sub

'*****************
' Mission targets
'*****************

Sub Target002_Hit 'lower
    PlaySoundAt "fx_target", Target002
    If Tilted Then Exit Sub
    PlaySound "SOUND58"
    Select Case Mission(CurrentPlayer)
        case 0
            li043.state = 1
            LightSeqMission1.play SeqBlinking, , 4, 40
            li096.state = 1
            LightSeqAcceptMission.play SeqBlinking, , 4, 40
            MissionTarget3 = 1
            CheckMissiontargets 3
        case 5, 171
            li043.state = 1
            LightSeqMission1.play SeqBlinking, , 4, 40
            MissionHits = MissionHits-1:CheckMission
        case else
            li043.state = 1
            LightSeqMission1.play SeqBlinking, , 4, 40
    End Select
End Sub

Sub Target003_Hit 'center
    PlaySoundAt "fx_target", Target003
    If Tilted Then Exit Sub
    PlaySound "SOUND58"
    Select Case Mission(CurrentPlayer)
        case 0
            li042.state = 1
            LightSeqMission2.play SeqBlinking, , 4, 40
            li096.state = 1
            LightSeqAcceptMission.play SeqBlinking, , 4, 40
            MissionTarget2 = 1
            CheckMissiontargets 2
        case 5, 171
            li042.state = 1
            LightSeqMission2.play SeqBlinking, , 4, 40
            MissionHits = MissionHits-1:CheckMission
        case Else
            li042.state = 1
            LightSeqMission2.play SeqBlinking, , 4, 40
    End Select
End Sub

Sub Target004_Hit 'upper
    PlaySoundAt "fx_target", Target004
    If Tilted Then Exit Sub
    PlaySound "SOUND58"
    Select Case Mission(CurrentPlayer)
        case 0
            li041.state = 1
            LightSeqMission3.play SeqBlinking, , 4, 40
            li096.state = 1
            LightSeqAcceptMission.play SeqBlinking, , 4, 40
            MissionTarget1 = 1
            CheckMissiontargets 1
        case 5, 171
            li041.state = 1
            LightSeqMission3.play SeqBlinking, , 4, 40
            MissionHits = MissionHits-1:CheckMission
        case Else
            li041.state = 1
            LightSeqMission3.play SeqBlinking, , 4, 40
    End Select
End Sub

'********************
' Top re-fuel targets
'********************

Sub Target005_Hit
    PlaySoundAt "fx_target", Target005
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Addscore 750
    li066.State = 1
    CheckTopTargets
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target006_Hit
    PlaySoundAt "fx_target", Target006
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Addscore 750
    li067.State = 1
    CheckTopTargets
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target007_Hit
    PlaySoundAt "fx_target", Target007
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Addscore 750
    li068.State = 1
    CheckTopTargets
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub CheckTopTargets
    If li066.State + li067.State + li068.State = 3 then
        're-fuel
        DMDFlush
        DMD "", CL(1, "SHIP RE-FUELED"), "", eNone, eNone, eNone, 1500, True, ""
        Fuel = 6:UpdateFuelLights:StartFuelTimers
        FlashForms li066, 1000, 50, 0
        FlashForms li067, 1000, 50, 0
        FlashForms li068, 1000, 50, 0
    End If
End Sub

'*****************************
' Comet targets -Right Hazard
'*****************************

Sub Target008_Hit 'lower
    PlaySoundAt "fx_target", Target008
    If Tilted Then Exit Sub
    PlaySound "SOUND58"
    Addscore 750
    li052.State = 1
    CheckCometLights
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target009_Hit 'center
    PlaySoundAt "fx_target", Target009
    If Tilted Then Exit Sub
    PlaySound "SOUND58"
    Addscore 750
    li053.State = 1
    CheckCometLights
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target010_Hit 'upper
    PlaySoundAt "fx_target", Target010
    If Tilted Then Exit Sub
    PlaySound "SOUND58"
    Addscore 750
    li054.State = 1
    CheckCometLights
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub CheckCometLights
    If li052.State + li053.State + li054.State = 3 Then
        li052.State = 0
        li053.State = 0
        li054.State = 0
        LightSeqComet.Play SeqBlinking, , 15, 10
        playsound "SOUND14"
        OpenRightGate
        If Mission(CurrentPlayer) = 9 Then CheckMission
    End If
End Sub

'********************************
' Radiation targets -Left Hazard
'********************************

Sub Target011_Hit 'lower
    PlaySoundAt "fx_target", Target011
    If Tilted Then Exit Sub
    PlaySound "SOUND58"
    Addscore 750
    li056.State = 1
    CheckRadiationLights
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target012_Hit 'center
    PlaySoundAt "fx_target", Target012
    If Tilted Then Exit Sub
    PlaySound "SOUND58"
    Addscore 750
    li057.State = 1
    CheckRadiationLights
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target013_Hit 'upper
    PlaySoundAt "fx_target", Target013
    If Tilted Then Exit Sub
    PlaySound "SOUND58"
    Addscore 750
    li058.State = 1
    CheckRadiationLights
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub CheckRadiationLights
    If li056.State + li057.State + li058.State = 3 Then
        li056.State = 0
        li057.State = 0
        li058.State = 0
        LightSeqRadiation.Play SeqBlinking, , 15, 10
        playsound "SOUND14"
        OpenLeftGate
        If Mission(CurrentPlayer) = 10 Then CheckMission
    End If
End Sub

'*******************
' Medal droptargets
'*******************

Sub Target014_Hit 'right
    PlaySoundAt "fx_droptarget", Target014
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    MedalTarget1 = 1
    CheckMedal
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target015_Hit 'center
    PlaySoundAt "fx_droptarget", Target015
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    MedalTarget2 = 1
    CheckMedal
    Select Case Mission(CurrentPlayer)
        case 4, 5, 15
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target016_Hit 'left
    PlaySoundAt "fx_droptarget", Target016
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    MedalTarget3 = 1
    CheckMedal
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub ResetMedalDT
    'PlaySoundAt SoundFXDOF("fx_resetdrop", 119, DOFPulse, DOFcontactors), Target015 ' Outhere
    PlaySoundAt "fx_resetdrop", Target015
    Target014.IsDropped = 0
    Target015.IsDropped = 0
    Target016.IsDropped = 0
    MedalTarget1 = 0
    MedalTarget2 = 0
    MedalTarget3 = 0
End Sub

Sub CheckMedal
    If MedalTarget1 + MedalTarget2 + MedalTarget3 = 3 Then
        Addscore 1500
        MedalCount = MedalCount + 1
        If MedalCount> 4 Then MedalCount = 4
        UpdateMedal
        Select Case MedalCount
            Case 1
                vpmtimer.addtimer 500, "playsound""SOUND50"" '"
                DMDFlush
                DMD "_", CL(1, "LEVEL 1 COMMENDATION"), "", eNone, eblink, eNone, 1500, True, "" '"vo_Level 1 Commendation"
                Addscore 10000
                EnableBallSaver 10
            Case 2
                vpmtimer.addtimer 500, "playsound""SOUND50"" '"
                DMDFlush
                DMD "_", CL(1, "LEVEL 2 COMMENDATION"), "", eNone, eBlink, eNone, 1500, True, "" '"vo_Level 2 Commendation"
                Addscore 50000
                EnableBallSaver 20
            Case 3
                vpmtimer.addtimer 500, "playsound""SOUND50"" '"
                DMDFlush
                DMD "_", CL(1, "LEVEL 3 COMMENDATION"), "", eNone, eBlinkFast, eNone, 1500, True, "" '"vo_Level 3 Commendation"
                AwardExtraBall
            Case 4
                EnableBallSaver 5
        End Select
        vpmtimer.addtimer 1400, "ResetMedalDT '"
    Else
        AddScore 500
    End if
End Sub

Sub UpdateMedal
    Select Case MedalCount
        case 0:li044.State = 0:li045.State = 0:li046.State = 0
        Case 1:li046.State = 1
        Case 2:li045.State = 1
        Case 3:li044.State = 1
    End Select
End Sub

'*********************
' Field X droptargets
'*********************

Sub Target017_Hit 'right
    PlaySoundAt "fx_droptarget", Target017
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    xTarget1 = 1
    CheckPFX
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target018_Hit 'center
    PlaySoundAt "fx_droptarget", Target018
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    xTarget2 = 1
    CheckPFX
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target019_Hit 'left
    PlaySoundAt "fx_droptarget", Target019
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    xTarget3 = 1
    CheckPFX
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub ResetxTargets
    'PlaySoundAt SoundFXDOF("fx_resetdrop", 117, DOFPulse, DOFcontactors), Target018 ' Outhere
    PlaySoundAt "fx_resetdrop", Target018
    Target019.IsDropped = 0
    Target018.IsDropped = 0
    Target017.IsDropped = 0
    xTarget1 = 0
    xTarget2 = 0
    xTarget3 = 0
End Sub

Sub CheckPFX
    If xTarget1 + xTarget2 + xTarget3 = 3 Then 'all targets has been hit
        Addscore 1500
        xTargetCount = xTargetCount + 1
        If xTargetCount> 4 then xTargetCount = 4
        UpdatexTargets
        SetPlayfieldX
        vpmtimer.addtimer 400, "playsound""sc_topdtreset"" '"
        pfXTimerExpired.Enabled = 0 're-start the timer to reset the countdown
        pfXTimerExpired.Enabled = 1
        vpmTimer.AddTimer 800, "ResetxTargets '"
    Else
        Addscore 500
    End If
End Sub

Sub UpdatexTargets
    Select Case xTargetCount
        Case 0:li059.State = 0:li060.State = 0:li061.State = 0:li062.State = 0
        Case 1:li059.State = 1:li060.State = 0:li061.State = 0:li062.State = 0
        Case 2:li059.State = 1:li060.State = 1:li061.State = 0:li062.State = 0
        Case 3:li059.State = 1:li060.State = 1:li061.State = 1:li062.State = 0
        Case 4:li059.State = 1:li060.State = 1:li061.State = 1:li062.State = 1
    End Select
End Sub

Sub SetPlayfieldX
    Select case xTargetCount
        Case 0: '1x
            DMDFlush
            DMD "_", CL(1, "PLAYFIELD 1 X"), "", eNone, eNone, eNone, 1500, True, ""
            PlayfieldMultiplier(CurrentPlayer) = 1
            UpdatexTargets
            vpmtimer.AddTimer 500, "ResetxTargets '"
        Case 1:                                                                       '2x
            DMDFlush
            DMD "_", CL(1, "PLAYFIELD 2 X"), "", eNone, eBlink, eNone, 1500, True, "" ' "vo_Field Multiplier 2x"
            PlayfieldMultiplier(CurrentPlayer) = 2
            UpdatexTargets
            vpmtimer.AddTimer 500, "ResetxTargets '"
        Case 2:                                                                       '3x
            DMDFlush
            DMD "_", CL(1, "PLAYFIELD 3 X"), "", eNone, eBlink, eNone, 1500, True, "" '"vo_Field Multiplier 3x"
            PlayfieldMultiplier(CurrentPlayer) = 3
            UpdatexTargets
            vpmtimer.AddTimer 500, "ResetxTargets '"
        Case 3:                                                                       '5x
            DMDFlush
            DMD "_", CL(1, "PLAYFIELD 5 X"), "", eNone, eBlink, eNone, 1500, True, "" '"vo_Field Multiplier 5x"
            PlayfieldMultiplier(CurrentPlayer) = 5
            UpdatexTargets
            vpmtimer.AddTimer 500, "ResetxTargets '"
        Case 4:                                                                            '10x
            DMDFlush
            DMD "_", CL(1, "PLAYFIELD 10 X"), "", eNone, eBlinkFast, eNone, 2000, True, "" '"vo_Field Multiplier 10x"
            PlayfieldMultiplier(CurrentPlayer) = 10
            UpdatexTargets
            vpmtimer.AddTimer 500, "ResetxTargets '"
    End Select
End Sub

Sub pfXTimerExpired_Timer
    xTargetCount = xTargetCount - 1
    If xTargetCount <= 0 Then
        xTargetCount = 0
        Me.Enabled = 0
    End If
    SetPlayfieldX
End Sub

'**********************
' Booster droptargets
'***********************

Sub Target020_Hit 'lower
    PlaySoundAt "fx_droptarget", Target020
    If Tilted Then Exit Sub
    PlaySound "sc_droptarget"
    BoosterTarget1 = 1
    CheckBooster
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target021_Hit 'center
    PlaySoundAt "fx_droptarget", Target021
    If Tilted Then Exit Sub
    PlaySound "sc_droptarget"
    BoosterTarget2 = 1
    CheckBooster
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target022_Hit 'upper
    PlaySoundAt "fx_droptarget", Target022
    If Tilted Then Exit Sub
    PlaySound "sc_droptarget"
    BoosterTarget3 = 1
    CheckBooster
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub ResetBoosterTargets
    'PlaySoundAt SoundFXDOF("fx_resetdrop", 120, DOFPulse, DOFcontactors), Target021 ' Outhere
    PlaySoundAt "fx_resetdrop", Target021
    Target020.isDropped = 0
    Target021.isDropped = 0
    Target022.isDropped = 0
    BoosterTarget1 = 0
    BoosterTarget2 = 0
    BoosterTarget3 = 0
End Sub

Sub CheckBooster
    If BoosterTarget1 + BoosterTarget2 + BoosterTarget3 = 3 Then 'all targets has been hit
        Addscore 5000
        BoosterCount = BoosterCount + 1
        If BoosterCount> 4 Then
            BoosterCount = 4
            Exit Sub
        End If
        UpdateBoosterLights
        BoosterTimerExpired.Enabled = 0 'stop and start the timer to reset the count
        BoosterTimerExpired.Enabled = 1
        Select case BoosterCount
            Case 1:                                                                        'Upgrade Flags
                DMDFlush
                DMD "_", CL(1, "FLAGS UPGRADED"), "", eNone, eBlink, eNone, 1500, True, "" '"vo_Flags Upgraded"
                bFlagsUpgraded = True
                li055.State = 1
                li048.State = 1
                If Mission(CurrentPlayer) = 6 Then CheckMission
            Case 2:                                                                           'Activate Jackpot
                DMDFlush
                DMD "_", CL(1, "JACKPOT ACTIVATED"), "", eNone, eBlink, eNone, 1500, True, "" '"vo_Jackpot Activated"
                bJackpotActivated = True
            Case 3:                                                                           'Activate Bonus at the Bonus lane
                DMDFlush
                DMD "_", CL(1, "BONUS ACTIVATED"), "", eNone, eBlink, eNone, 1500, True, ""   '"vo_Bonus Activated"
                bBonusActivated = True
                li034.State = 1
            Case 4:                                                                       'Bonus Hold
                DMDFlush
                DMD "_", "BONUS HOLD ACTIVATED", "", eNone, eBlink, eNone, 1500, True, "" '"vo_Bonus Hold"
                bBonusHeld = True
        End Select
        vpmtimer.addtimer 400, "playsound""sc_rightdtreset"" '"
        vpmTimer.AddTimer 800, "ResetBoosterTargets '"
    Else
        Addscore 500
    End If
End Sub

Sub UpdateBoosterLights
    Select Case BoosterCount
        Case 0:li040.State = 0:li039.State = 0:li038.State = 0:li037.State = 0
        Case 1:li040.State = 1:li039.State = 0:li038.State = 0:li037.State = 0
        Case 2:li040.State = 1:li039.State = 1:li038.State = 0:li037.State = 0
        Case 3:li040.State = 1:li039.State = 1:li038.State = 1:li037.State = 0
        Case 4:li040.State = 1:li039.State = 1:li038.State = 1:li037.State = 1
    End Select
End Sub

Sub BoosterTimerExpired_Timer
    ' Stop current mode
    Select Case BoosterCount
        Case 1: 'flags
            bFlagsUpgraded = False
            li055.State = 0
            li048.State = 0
        Case 2: 'Jackpot
            bJackpotActivated = False
        Case 3: 'Bonus
            bBonusActivated = false
            li034.State = 0
        Case 4: 'Bonus Hold
    'bBonusHeld = False 'do not reset it as it will be active when the ball drains
    End Select
    If BoosterCount> 0 Then
        BoosterCount = BoosterCount -1
        UpdateBoosterLights
    Else
        BoosterTimerExpired.Enabled = 0
    End If
End Sub

'**********
' Spinners
'**********

Sub Spinner001_Spin 'left
    DOF 133, 2      ' Outhere
    PlaySoundAt "fx_spinner", Spinner001
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    If bFlagsUpgraded Then
        Addscore 2500
    Else
        Addscore 500
    End If
    Select Case Mission(CurrentPlayer)
        Case 8, 81, 82, 101, 176
        Case Else
            ChangeArrowColor
    End Select
    Select Case Mission(CurrentPlayer)
        Case 12
            MissionHits = MissionHits-1:CheckMission
        Case 175
            CheckMission
    End Select
End Sub

Sub Spinner002_Spin 'right
    DOF 133, 2      ' Outhere
    PlaySoundAt "fx_spinner", Spinner002
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    If bFlagsUpgraded Then
        Addscore 2500
    Else
        Addscore 500
    End If
    Select Case Mission(CurrentPlayer)
        Case 8, 81, 82, 101, 176
        Case Else
            ChangeArrowColor
    End Select
    Select Case Mission(CurrentPlayer)
        Case 12
            MissionHits = MissionHits-1:CheckMission
        Case 175
            CheckMission
    End Select
End Sub

'************
' Black Hole
'************

Sub BlackHole_Hit
    PlaySoundAt "fx_kicker_enter", BlackHole
    PlaySound "sc_blackhole"
    If Not Tilted Then
        DMDFlush
        DMD CL(0, "BLACK HOLE"), CL(1, "20000"), "", eBlink, eNone, eNone, 1500, True, "" '"vo_Black Hole"
        Addscore 20000
    End If
    If Mission(CurrentPlayer) = 111 Then CheckMission
    ' Nothing left to do, so kick out the ball
    vpmtimer.addtimer 1700, "PlaySoundAt""fx_kicker"",BlackHole: BlackHole.kick 48+RND*6, 30:DOF 129,2 '" ' Outhere
End Sub

'******************
' Hyper Space Hole
'******************
dim maeljack:maeljack = 0
Sub HyperSpaceHole_Hit
    PlaySoundAt "fx_kicker_enter", HyperSpaceHole
    Dim a:a = 0
    If Not Tilted Then
        HyperSpaceHits = HyperSpaceHits + 1
        HyperExpiredTimer.Enabled = 0 'stop and start the timer to reduce the hyperscape hits
        HyperExpiredTimer.Enabled = 1
        If aMagnet.MagnetON Then
            aMagnet.MagnetON = False
            vpmtimer.Addtimer 4000, "aMagnet.MagnetON = True '"
            LightSeqGravity.play SeqBlinking, , 12, 80
        'FlashForms li002, 4000, 50, 1
        End If

        If bMultiBallMode Then
            HyperSpaceHits = 10
            if(maeljack> 0) Then
                a = 7
                DMDFlush
                DMD CL(0, "MAELSTROM"), CL(1, "JACKPOT"), "", eBlink, eBlinkFast, eNone, 1500, True, ""
                LightEffect 3
                Addscore 500000
            end If
            maeljack = maeljack + 1
        Else
            Select Case HyperSpaceHits
                Case 1:Addscore 10000:a = 1
                Case 2:Addscore 20000:a = 2
                Case 3:Addscore 20000:a = 3:RisePost
                Case 4:Addscore 50000:a = 4:li032.State = 1:li036.State = 1:DMDFlush:
                    DMD "", "EXTRA BALL AVAILABLE", "", eNone, eNone, eNone, 1500, True, "" '"vo_Extra Ball Available"
                Case 5:Addscore 150000:EnableGravity:a = 5:HyperSpaceHits = 0
            End Select

            If bJackpotActivated Then
                AwardJackpot
            End if
            If bReflexHyperShot Then
                AwardReflexHyperShot
                StartReflexHyperShot
                a = 6
            Else
                StartReflexHyperShot
            End If
        End If

        Select case a
            Case 1:Playsound "SOUND35"
            Case 2:Playsound "SOUND36"
            Case 3:Playsound "SOUND35"
            Case 4:Playsound "SOUND38"
            Case 5:Playsound "SOUND39"
            Case 6:Playsound "SOUND21"
            Case 7:playsound "maeljackpot"
        End select
        UpdateHyperLights
    End If
    ' Nothing left to do, so kick out the ball
    Select Case Mission(CurrentPlayer)
        Case 61, 91:CheckMission '177
        Case 161:CheckMission
    End Select
    if(Mission(CurrentPlayer) = 177) Then
        vpmtimer.addtimer 6000, "PlaySoundAt""fx_kicker"",HyperSpaceHole: HyperSpaceHole.kick 130, 30:DOF 130,2 '" ' Outhere
    Else
        vpmtimer.addtimer 2000, "PlaySoundAt""fx_kicker"",HyperSpaceHole: HyperSpaceHole.kick 130, 30:DOF 130,2 '" ' Outhere
    end If
    if Mission(CurrentPlayer) = 177 then CheckMission
End Sub

Sub UpdateHyperLights
    Select case HyperSpaceHits
        Case 0:li119.State = 0:li120.State = 0:li121.State = 0:li122.State = 0
        Case 1:Flashforms li119, 1000, 50, 1:li120.State = 0:li121.State = 0:li122.State = 0
        Case 2:li119.State = 1:Flashforms li120, 1000, 50, 1:li121.State = 0:li122.State = 0
        Case 3:li119.State = 1:li120.State = 1:Flashforms li121, 1000, 50, 1:li122.State = 0
        Case 4:li119.State = 1:li120.State = 1:li121.State = 1:Flashforms li122, 1000, 50, 1
        Case 10:hyperspacelightpursuit
    End Select
End Sub

dim Hyperlight:Hyperlight = 1
Sub hyperspacelightpursuit 'li119-122
    hyperspacelightpursuitTimer.Enabled = 1
End Sub

sub hyperspacelightpursuitTimer_Timer
    hyperlight = hyperlight + 1
    if hyperlight> 4 then hyperlight = 1
    select case Hyperlight
        case 1:li119.State = 1:li120.State = 0:li121.State = 0:li122.State = 0
        case 2:li119.State = 0:li120.State = 1:li121.State = 0:li122.State = 0
        case 3:li119.State = 0:li120.State = 0:li121.State = 1:li122.State = 0
        case 4:li119.State = 0:li120.State = 0:li121.State = 0:li122.State = 1
    End Select
    UpdateHyperLights
end Sub

Sub HyperExpiredTimer_Timer
    HyperSpaceHits = HyperSpaceHits - 1
    If HyperSpaceHits = 0 Then
        HyperExpiredTimer.Enabled = 0
    End If
    UpdateHyperLights
End Sub

'************
' Wormholes
'************

Sub Wormhole1_Hit
    PlaySoundAt "fx_hole_enter", Wormhole1
    Wormhole1.Destroyball
    BallsOnPlayfield = BallsOnPlayfield - 1
    BallsinWormHole = BallsinWormHole + 1
    Flashforms li142, 500, 50, 2
    PlaySound "sc_wormhole"
    Select Case Mission(CurrentPlayer)
        Case 8, 101, 176
            CheckMission
    End Select
    ' Score some points
    Select Case WormHoleColor
        Case 0             'wormhole no activated
            Addscore 2000
            kickWormhole 1 '"
        Case 1             'same color then lock the ball  or award extraball if in multiball mode
            Addscore 5000
            If bMultiBallMode Then
                AwardExtraball
                kickWormhole 1 '"
            Else
                vpmtimer.addtimer 300, "LockBall '"
            End If
        Case 2, 3                      'kick the ball from the other hole
            Addscore 7500
            kickWormhole WormHoleColor '"
    End Select
End Sub

Sub Wormhole2_Hit
    PlaySoundAt "fx_hole_enter", Wormhole2
    Wormhole2.Destroyball
    BallsOnPlayfield = BallsOnPlayfield - 1
    BallsinWormHole = BallsinWormHole + 1
    Flashforms li143, 500, 50, 2
    PlaySound "sc_wormhole"
    Select Case Mission(CurrentPlayer)
        Case 81, 101, 176
            CheckMission
    End Select
    ' Score some points
    Select Case WormHoleColor
        Case 0             'wormhole no activated
            Addscore 2000
            kickWormhole 2 '"
        Case 2             'same color then lock the ball  or award extraball if in multiball mode
            Addscore 5000
            If bMultiBallMode Then
                AwardExtraball
                kickWormhole 2 '"
            Else
                vpmtimer.addtimer 300, "LockBall '"
            End If
        Case 1, 3                      'kick the ball from the other holes
            Addscore 7500
            kickWormhole WormHoleColor '"
    End Select
End Sub

Sub Wormhole3_Hit
    PlaySoundAt "fx_hole_enter", Wormhole3
    Wormhole3.Destroyball
    BallsOnPlayfield = BallsOnPlayfield - 1
    BallsinWormHole = BallsinWormHole + 1
    Flashforms li144, 500, 50, 2
    PlaySound "sc_wormhole"
    Select Case Mission(CurrentPlayer)
        Case 82, 101, 176
            CheckMission
    End Select
    ' Score some points
    Select Case WormHoleColor
        Case 0             'wormhole no activated
            Addscore 2000
            kickWormhole 3 '"
        Case 3             'same color then lock the ball or award extraball if in multiball mode
            Addscore 5000
            If bMultiBallMode Then
                AwardExtraball
                kickWormhole 3 '"
            Else
                vpmtimer.addtimer 300, "LockBall '"
            End If
        Case 1, 2                      'kick the ball from the other holes
            Addscore 7500
            kickWormhole WormHoleColor '"
    End Select
End Sub

Sub LockBall
    ' remove the ball from the wormhole count as new ball will be kicked out from the wormholes or to the plunger
    BallsinWormHole = BallsinWormHole - 1
    ' add the ball to the lock
    BallsInLock(CurrentPlayer) = BallsInLock(CurrentPlayer) + 1
    playsound "vo_Balllocked" &BallsInLock(CurrentPlayer):DOF 134, 2
    DMDFlush
    DMD "", CL(1, "BALL " &BallsInLock(CurrentPlayer) & " LOCKED"), "", eNone, eBlinkFast, eNone, 2000, True, "" ' Outhere
    If BallsInLock(CurrentPlayer) = 3 Then
        vpmtimer.addtimer 1500, "StartWormholeMultiball '"
    Else
        ' we use the Addmultiball to create a ball at the plunger lane
        vpmtimer.addtimer 500, " BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass : BallsOnPlayfield = BallsOnPlayfield + 1 : PlaySoundAt SoundFXDOF(""fx_Ballrel"", 123, DOFPulse, DOFContactors), BallRelease : BallRelease.Kick 90, 4 '"

        ' we kick the ball with the autoplunger
        bAutoPlunger = True
        ' turn off the wormholes
        TurnOffWormholes
    End If
End Sub

Sub kickWormhole(hole) 'kicks the ball out in the color wormhole

    CurrentWormhole = hole
    If BallsinWormHole> 0 Then
        BallsinWormHole = BallsinWormHole - 1
        BallsOnPlayfield = BallsOnPlayfield + 1
        Select case hole
            Case 1
                'yellow
                vpmtimer.addtimer 1500, "WormHole1.CreateSizedBallWithMass BallSize / 2, BallMass : kickWormhole CurrentWormhole'"
                vpmtimer.addtimer 1501, "PlaySoundAt SoundFXDOF(""fx_popper"", 114, DOFPulse, DOFcontactors), WormHole1 : WormHole1.kick 220, 20 :PlaySound ""sc_wormhole""  '"
                Flashforms li142, 1500, 50, 2
                If(WormholeColor = 0) Then
                    vpmtimer.addtimer 1502, "li142.state=0 '"
                Else
                    vpmtimer.addtimer 1502, "li142.state=1 '"
                end If
            Case 2
                'red
                vpmtimer.addtimer 1500, "WormHole2.CreateSizedBallWithMass BallSize / 2, BallMass : kickWormhole CurrentWormhole'"
                vpmtimer.addtimer 1501, "PlaySoundAt SoundFXDOF(""fx_popper"", 115, DOFPulse, DOFcontactors), WormHole2 : WormHole2.kick 220, 20 :PlaySound ""sc_wormhole""  '"
                Flashforms li143, 1500, 50, 2
                If(WormholeColor = 0) Then
                    vpmtimer.addtimer 1502, "li143.state=0 '"
                Else
                    vpmtimer.addtimer 1502, "li143.state=1 '"
                end If
            Case 3
                'green
                vpmtimer.addtimer 1500, "WormHole3.CreateSizedBallWithMass BallSize / 2, BallMass : kickWormhole CurrentWormhole'"
                vpmtimer.addtimer 1501, "PlaySoundAt SoundFXDOF(""fx_popper"", 116, DOFPulse, DOFcontactors), WormHole3 : WormHole3.kick 320, 20 :PlaySound ""sc_wormhole""  '"
                Flashforms li144, 1500, 50, 2
        End Select
        If(WormholeColor = 0) Then
            vpmtimer.addtimer 1502, "li144.state=0 '"
        Else
            vpmtimer.addtimer 1502, "li144.state=1 '"
        end If
    ' Outhere
    End If
End Sub

Sub TurnOffWormholes
    WormHoleColor = 0
    li047.State = 0
    li142.State = 0
    li143.State = 0
    li144.State = 0
    li080.State = 0
    li081.State = 0
    li082.State = 0
End Sub

Sub StartWormholeMultiball
    StopSound "mu_main"
    StopSound "mu_mission"
    StopSound "mu_multiball"
    StopSound "mu_wait"
    DMDFlush
    DMD CL(0, "WORMHOLE"), CL(1, "MULTI BALL"), "", eBlinkFast, eBlinkFast, eNone, 3000, True, ""
    Playsound "multiball"
    LightSeqInserts.Play SeqAlloff
    LightSeqGi.Play SeqAlloff
    WormholeMBStep = 0
    BallsInLock(CurrentPlayer) = 0
    vpmtimer.addtimer 3300, "WormHoleMultiball.Enabled = 1 '"
    AsteroidsTimer.Enabled = 1
    vpmtimer.addtimer 3400, "TurnOffWormholes'"
    lastsong = Song
    vpmtimer.addtimer 3200, "PlaySong ""mu_multiball""  '"
    Flashforms li142, 3800, 50, 0
    Flashforms li143, 3800, 50, 0
    Flashforms li144, 3800, 50, 0
    SetLightColor li080, yellow, 1
    SetLightColor li081, red, 1
    SetLightColor li082, green, 1
    Flashforms li080, 3800, 50, 0
    Flashforms li081, 3800, 50, 0
    Flashforms li082, 3800, 50, 0
    li099.state = 2
    vpmtimer.addtimer 4000, "LightSeqInserts.stopPlay:LightSeqGi.stopplay : maeljack=1 : HyperSpaceHits=10:hyperspacelightpursuit '"
End Sub

Sub AsteroidsTimer_Timer
    Select Case BallsOnPlayfield
        Case 1:li049.State = 0:li050.State = 0:li051.State = 2
        Case 2:li049.State = 0:li050.State = 2:li051.State = 2
        Case 3:li049.State = 2:li050.State = 2:li051.State = 2
    End Select
End Sub

Sub WormHoleMultiball_Timer
    Select case WormholeMBStep
        Case 0
            PlaySoundAt SoundFXDOF("fx_popper", 114, DOFPulse, DOFcontactors), WormHole1 ' Outhere
            Flashforms li142, 500, 50, 2
            Wormhole1.CreateSizedBallWithMass BallSize / 2, BallMass
            BallsOnPlayfield = BallsOnPlayfield + 1
            Wormhole1.kick 220, 20
            WormholeMBStep = 1
            EnableBallSaver 20
        Case 1
            PlaySoundAt SoundFXDOF("fx_popper", 115, DOFPulse, DOFcontactors), WormHole2 ' Outhere
            Flashforms li143, 500, 50, 2
            Wormhole2.CreateSizedBallWithMass BallSize / 2, BallMass
            BallsOnPlayfield = BallsOnPlayfield + 1
            Wormhole2.kick 220, 20
            DOF 143, DOFPulse
            bMultiBallMode = True
            WormholeMBStep = 2
        Case 2
            PlaySoundAt SoundFXDOF("fx_popper", 116, DOFPulse, DOFcontactors), WormHole3 ' Outhere
            Flashforms li144, 1500, 50, 2
            Wormhole3.CreateSizedBallWithMass BallSize / 2, BallMass
            BallsOnPlayfield = BallsOnPlayfield + 1
            Wormhole3.kick 312, 20
            WormHoleMultiball.Enabled = 0
    End Select
End Sub

'***********************
' Left & Right KickBaks
'***********************

Sub LeftKickerRest_Hit
    PlaySoundAt "fx_kicker_enter", LeftKickerRest
    FlashForMs li102, 2000, 50, 0
    vpmtimer.addtimer 800, "PlaySoundAt SoundFXDOF (""fx_kicker"", 141, DOFPulse, DOFContactors), leftkicker : " & _
                                "PlaySoundAt SoundFXDOF(""fx_solenoid"", 141, DOFPulse, DOFContactors),leftkicker : " &                    _
                                "PlaySound ""sc_wormhole"":LeftKicker.kick 0, 50:DOF 124,2 '"                                              ' Outhere
    vpmtimer.addtimer 1000, "CloseLeftGate '"
End Sub

Sub RightKickerRest_Hit
    PlaySoundAt "fx_kicker_enter", RightKickerRest
    FlashForMs li103, 2000, 50, 0
    vpmtimer.addtimer 800, "PlaySoundAt SoundFXDOF (""fx_kicker"", 141, DOFPulse, DOFContactors), rightkicker :  " & _
                                "PlaySoundAt SoundFXDOF(""fx_solenoid"", 141, DOFPulse, DOFContactors),rightkicker :  " &                    _
                                "PlaySound ""sc_wormhole"":RightKicker.kick 1, 50:DOF 128,2 '"                                               ' Outhere
    vpmtimer.addtimer 1000, "CloseRightGate '"
End Sub

Sub OpenLeftGate
    PlaySoundAt "fx_diverter", LeftLaneGate
    LeftLaneGate.RotateToEnd
    li072.State = 1
End Sub

Sub CloseLeftGate
    PlaySoundAt "fx_diverter", LeftLaneGate
    LeftLaneGate.RotateToStart
    li072.State = 0
End Sub

Sub OpenRightGate
    PlaySoundAt "fx_diverter", RightLaneGate
    RightLaneGate.RotateToEnd
    li095.State = 1
End Sub

Sub CloseRightGate
    PlaySoundAt "fx_diverter", RightLaneGate
    RightLaneGate.RotateToStart
    li095.State = 0
End Sub

'*****************
' Gravity Magnet
'*****************

Sub GravityCenter_Hit 'the ball rests on this target, so kick the ball upwards
    If NOT Tilted Then
        If aMagnet.MagnetON Then
            Addscore 50000
            'deactivate magnet
            vpmtimer.Addtimer 2000, "GravityReleaseBall '"
            playsound "SOUND7"
        End If
    End If
End Sub

Sub EnableGravity 'aMagnet On. After 5 hits to the Hyper Space hole
    If NOT Tilted Then
        DMDFlush
        DMD "_", CL(1, "GRAVITY WELL"), "", eNone, eNone, eNone, 1500, True, "" '"vo_Gravity Well"
        vpmtimer.Addtimer 4000, "aMagnet.MagnetON = True '"
        li002.state = 1
        LightSeqGravity.play SeqBlinking, , 12, 80
    'FlashForms li002, 4000, 50, 1
    End If
End Sub

Sub DisableGravity
    LightSeqGravity.stopplay
    li002.State = 0
    aMagnet.MagnetOn = False
End Sub

Sub GravityReleaseBall 'aMagnet off and kick the ball if any
    Dim dir, speed, ball
    For Each ball In aMagnet.Balls
        With ball
            dir = Rnd * 6.28:speed = 25 + Rnd * 5
            .VelX = speed * Sin(dir)
            .VelY = - speed * ABS(Cos(dir) ) 'only upwards
        End With
    Next
    DMDFlush
    DMD "_", CL(1, "GRAVITY NORMALIZED"), "", eNone, eNone, eNone, 1500, True, "" '"vo_Gravity Normalized"
    DisableGravity
End Sub

'**************
' Flipper Post
'**************

Sub RisePost
    PlaySoundAt "Fx_SolenoidOn", rpeg001
    DMDFlush
    DMD "_", CL(1, "CENTER POST ACTIVE"), "", eNone, eNone, eNone, 1500, True, "" '"vo_Center post"
    li001.State = 1:li001.TimerEnabled = 1
    rpeg001.Z = 0
    rpin009.IsDropped = 0
    rpin009.TimerEnabled = 1
End Sub

Sub LowerPost
    PlaySoundAt "Fx_SolenoidOff", rpeg001
    li001.TimerEnabled = 0
    li001.State = 0
    rpeg001.Z = -52
    rpin009.IsDropped = 1
    rpin009.TimerEnabled = 0
End Sub

Sub li001_Timer
    li001.TimerEnabled = 0
    li001.State = 2
End Sub

Sub rpin009_Timer '60 seconds
    LowerPost
End Sub

'***********************************
' Reflex Shots: Ramp and Hyper Space
'***********************************

Sub AwardReflexRampShot
    DMDFlush
    DMD "_", CL(1, "REFLEX SHOT"), "", eNone, eNone, eNone, 1500, True, "" '"vo_Reflex Shot"
    Addscore 20000
    StopReflexRampShot
End Sub

Sub AwardReflexHyperShot
    DMDFlush
    DMD "_", CL(1, "REFLEX SHOT"), "", eNone, eNone, eNone, 1500, True, "" '"vo_Reflex Shot"
    Addscore 20000
    StopReflexHyperShot
End Sub

Sub StartReflexRampShot
    li098.State = 2
    li098.TimerEnabled = 1
    bReflexRampShot = True
End Sub

Sub li098_Timer
    StopReflexRampShot
End Sub

Sub StopReflexRampShot
    li098.State = 0
    li098.TimerEnabled = 0
    bReflexRampShot = False
End Sub

Sub StartReflexHyperShot
    li101.State = 2
    li101.TimerEnabled = 1
    bReflexHyperShot = True
End Sub

Sub li101_Timer
    StopReflexHyperShot
End Sub

Sub StopReflexHyperShot
    li101.State = 0
    li101.TimerEnabled = 0
    bReflexHyperShot = False
End Sub

'**********************
' Rotate lights Timers
'**********************

Sub RotateRankLights_Timer
    Dim tmp
    tmp = li003.State
    For i = 0 to 7
        RankLights(i).State = RankLights(i + 1).State
    Next
    li011.State = tmp
End Sub

dim Rotateprogplus
Sub RotateProgressLights_Timer
    If bMael = 1 Then Exit Sub
    Dim tmp
    tmp = li029.State
    For i = 17 to 1 step -1
        ProgressLights(i).State = ProgressLights(i-1).State
    Next
    li012.State = tmp
End Sub

'****************
' Progress Lights
'****************
' Collection ProgressLights, 18 Lights
' variables Progress(CurrentPlayer) value 0 to 17

Sub UpdateProgressLights
    For i = 0 to 17
        ProgressLights(i).State = 0
    Next
    If Progress(CurrentPlayer)> 0 Then
        For i = 0 to Progress(CurrentPlayer)
            ProgressLights(i).State = 1
        Next
    End If
End Sub

Sub AddProgress(n)
    progress(currentplayer) = progress(currentplayer) - Rotateprogplus
    Rotateprogplus = 0
    Progress(CurrentPlayer) = Progress(CurrentPlayer) + n
    If Progress(CurrentPlayer) >= 16 Then '+ winmission and update rank
        PromoteRank
        Progress(CurrentPlayer) = -1
    End If
    UpdateProgressLights
End Sub

'****************
'    Rank
'****************
' Collection RankLights, 9 Lights
' variables Rank(CurrentPlayer) value 0 to 8

Sub UpdateRankLights
    For i = 0 to 8
        RankLights(i).State = 0
    Next
    dim j
    if(Rank(CurrentPlayer)> 8) Then
        j = 8
    Else
        j = Rank(CurrentPlayer)
    End If

    For i = 0 to j
        RankLights(i).State = 1
    Next
End Sub

Sub PromoteRank
    If Rank(CurrentPlayer) < 9 Then
        PromoteRankfct
    End If
End Sub

sub promoterankfct
    Rank(CurrentPlayer) = Rank(CurrentPlayer) + 1
    'show DMD new rank
    Select Case Rank(CurrentPlayer)
        Case 0:DMDFlush:DMD CL(0, "PROMOTION TO"), CL(1, "CADET"), "", eNone, eNone, eNone, 1500, True, ""                                                                                                      'it should not happen :)
        Case 1:DMDFlush:DMD CL(0, "PROMOTION TO"), CL(1, "ENSIGN"), "", eNone, eNone, eNone, 1500, True, ""                                                                                                     '"vo_Promotion to Ensign"
        Case 2:DMDFlush:DMD CL(0, "PROMOTION TO"), CL(1, "LIEUTENANT"), "", eNone, eNone, eNone, 1500, True, ""                                                                                                 '"vo_Promotion to Lieutenant"
        Case 3:DMDFlush:DMD CL(0, "PROMOTION TO"), CL(1, "CAPTAIN"), "", eNone, eNone, eNone, 1500, True, ""                                                                                                    '"vo_Promotion to Captain"
        Case 4:DMDFlush:DMD CL(0, "PROMOTION TO"), CL(1, "LT COMMANDER"), "", eNone, eNone, eNone, 1500, True, ""                                                                                               '"vo_Promotion to Lieutenant Commander"
        Case 5:DMDFlush:DMD CL(0, "PROMOTION TO"), CL(1, "COMMANDER"), "", eNone, eNone, eNone, 1500, True, ""                                                                                                  '"vo_Promotion to Commander"
        Case 6:DMDFlush:DMD CL(0, "PROMOTION TO"), CL(1, "COMMODORE"), "", eNone, eNone, eNone, 1500, True, ""                                                                                                  '"vo_Promotion to Commodore"
        Case 7:DMDFlush:DMD CL(0, "PROMOTION TO"), CL(1, "ADMIRAL"), "", eNone, eNone, eNone, 1500, True, ""                                                                                                    '"vo_Promotion to Admiral"
        Case 8:DMDFlush:DMD CL(0, "PROMOTION TO"), CL(1, "FLEET ADMIRAL"), "", eNone, eNone, eNone, 1500, True, "":DMDFlush:DMD CL(0, "CONTINUE"), CL(1, "MISSIONS"), "", eBlink, eBlink, eNone, 1500, True, "" '"vo_Promotion to Fleet Admiral"
        Case 9:DMDFlush:DMD CL(0, "GO FOR ANY"), CL(1, "MISSION NOW"), "", eBlink, eBlink, eNone, 1500, True, ""                                                                                                '"vo_Promotion to Fleet Admiral"
    End Select
    vpmtimer.addtimer 1500, "LightEffect 4 : playsound ""SOUND54"" '"
    UpdateRankLights
end Sub

Sub DemoteRank
    If Rank(CurrentPlayer)> 0 Then
        LightEffect 6
        Rank(CurrentPlayer) = Rank(CurrentPlayer) -1
        If Rank(CurrentPlayer) < 0 then Rank(CurrentPlayer) = 0
        UpdateRankLights
        'show DMD new rank
        Select Case Rank(CurrentPlayer)
            Case 0:DMD CL(0, "DEMOTION TO"), CL(1, "CADET"), "", eNone, eNone, eNone, 1500, True, ""         '"vo_Demotion to Cadet"
            Case 1:DMD CL(0, "DEMOTION TO"), CL(1, "ENSIGN"), "", eNone, eNone, eNone, 1500, True, ""        '"vo_Demotion to Ensign"
            Case 2:DMD CL(0, "DEMOTION TO"), CL(1, "LIEUTENANT"), "", eNone, eNone, eNone, 1500, True, ""    '"vo_Demotion to Lieutenant"
            Case 3:DMD CL(0, "DEMOTION TO"), CL(1, "CAPTAIN"), "", eNone, eNone, eNone, 1500, True, ""       '"vo_Demotion to Captain"
            Case 4:DMD CL(0, "DEMOTION TO"), CL(1, "LT COMMANDER"), "", eNone, eNone, eNone, 1500, True, ""  '"vo_Demotion to Lieutenant Commander"
            Case 5:DMD CL(0, "DEMOTION TO"), CL(1, "COMMANDER"), "", eNone, eNone, eNone, 1500, True, ""     '"vo_Demotion to Commander"
            Case 6:DMD CL(0, "DEMOTION TO"), CL(1, "COMMODORE"), "", eNone, eNone, eNone, 1500, True, ""     '"vo_Demotion to Commodore"
            Case 7:DMD CL(0, "DEMOTION TO"), CL(1, "ADMIRAL"), "", eNone, eNone, eNone, 1500, True, ""       '"vo_Demotion to Admiral"
            Case 8:DMD CL(0, "DEMOTION TO"), CL(1, "FLEET ADMIRAL"), "", eNone, eNone, eNone, 1500, True, "" 'it should not happen :)
        End Select
    End If
End Sub

'***************
'   MISSIONS
'***************
' variables used:
' MissionSelected = 1 to 4 after a target has been hit
' Mission(CurrentPlayer) = x,  being x the mission number currently active

Sub PrepareForMission 'called after the skillshot on a new ball or after a mission is completed
    RotateProgressLights.Enabled = 0:progress(currentplayer) = progress(currentplayer) - Rotateprogplus:UpdateProgressLights
    RotateRankLights.Enabled = 0:UpdateRankLights
    Rotateprogplus = 0
    For each i in aArrows
        i.State = 0
    Next
    Mission(CurrentPlayer) = 0
    MissionSelected = 0
    LightSeqMission3.stopplay
    LightSeqMission2.stopplay
    LightSeqMission1.stopplay
    li041.State = 0
    li042.State = 0
    li043.State = 0
    li123.State = 0
    li076.State = 2 'arrow light
    MissionTarget1 = 0
    MissionTarget2 = 0
    MissionTarget3 = 0
    If bMultiballMode Then
        PlaySong "mu_Multiball"
        lastsong = "mu_main"
    Else
        PlaySong "mu_main"
    End If
End Sub

Sub CheckMissionTargets(n)
    ' a target has been hit, select mission
    ' n is from 1 to 4, 123 are target hits, mission 4 is when all 3 targets are hit
    If MissionTarget1 + MissionTarget2 + MissionTarget3 = 3 Then
        MissionSelected = 4
        ' turn off the targets lights
        LightSeqMission3.stopplay
        LightSeqMission2.stopplay
        LightSeqMission1.stopplay
        li041.State = 0
        li042.State = 0
        li043.State = 0
        LightSeqMission.Play SeqBlinking, , 15, 10
        MissionTarget1 = 0
        MissionTarget2 = 0
        MissionTarget3 = 0
    Else
        MissionSelected = n
        li076.State = 0
        li078.State = 2
        li123.State = 0
    End If
    ' Select Mission
    Select Case Rank(CurrentPlayer)
        Case 0         'Cadet
            Select case MissionSelected
                case 1 'Launch Training 'top target
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "LAUNCH TRAINING"), "", eNone, eNone, eNone, 1500, True, ""
                case 2 'Re-entry Training
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "RE-ENTRY TRAINING"), "", eNone, eNone, eNone, 1500, True, ""
                case 3 'Target Practice
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "TARGET PRACTICE"), "", eNone, eNone, eNone, 1500, True, ""
                case 4 'Science Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "SCIENCE MISSION"), "", eNone, eNone, eNone, 1500, True, ""
            End Select
        Case 1, 2      'Ensign & Lieutenant
            Select case MissionSelected
                case 1 'Bug Hunt Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "BUG HUNT MISSION"), "", eNone, eNone, eNone, 1500, True, ""
                case 2 'Rescue Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "RESCUE MISSION"), "", eNone, eNone, eNone, 1500, True, ""
                case 3 'Alien Menace Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "ALIEN MENACE"), "", eNone, eNone, eNone, 1500, True, ""
                case 4 'Secret Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "SECRET MISSION"), "", eNone, eNone, eNone, 1500, True, ""
            End Select
        Case 3, 4      'Captain & Lieut. Commander
            Select case MissionSelected
                case 1 'Stray Comet Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "STRAY COMET"), "", eNone, eNone, eNone, 1500, True, ""
                case 2 'Space Radiation Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "SPACE RADIATION"), "", eNone, eNone, eNone, 1500, True, ""
                case 3 'Black Hole Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "BLACK HOLE"), "", eNone, eNone, eNone, 1500, True, ""
                case 4 'Cosmic Plague Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "COSMIC PLAGUE"), "", eNone, eNone, eNone, 1500, True, ""
            End Select
        Case 5, 6      'Commander & Commodore
            Select case MissionSelected
                case 1 'Satellite Retrieval Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "SATELLITE RETRIEVAL"), "", eNone, eNone, eNone, 1500, True, ""
                case 2 'Recon Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "RECON MISSION"), "", eNone, eNone, eNone, 1500, True, ""
                case 3 'Doomsday Machine Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "DOOMSDAY MACHINE"), "", eNone, eNone, eNone, 1500, True, ""
                case 4 'Time Warp Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "TIME WARP"), "", eNone, eNone, eNone, 1500, True, ""
            End Select
        Case 7, 8      'Admiral & Fleet Admiral
            Select case MissionSelected
                case 1 'Cosmic Plague Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "COSMIC PLAGUE"), "", eNone, eNone, eNone, 1500, True, ""
                case 2 'Secret Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "SECRET MISSION"), "", eNone, eNone, eNone, 1500, True, ""
                case 3 'Time Warp Mission
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "TIME WARP"), "", eNone, eNone, eNone, 1500, True, ""
                case 4 'Maelstrom
                DMDFlush:DMD CL(0, "MISSION SELECTED"), CL(1, "MAELSTROM"), "", eNone, eNone, eNone, 1500, True, ""
            end select
        Case 9                  'end game
            Select case MissionSelected
                case 1, 2, 3, 4 'Maelstrom
                DMDFlush:DMD CL(0, "ACTIVATE"), CL(1, "MAELSTROM MISSION"), "", eBlink, eBlinkFast, eNone, 2000, True, ""
            end select
    End Select
End Sub

Sub AcceptMission(n)
    For each i in aArrows
        i.State = 0
    Next
    FlashForMs li123, 750, 50, 0
    li096.state = 0
    LightSeqAcceptMission.play SeqBlinking, , 5, 20
    li123.State = 1
    ' rotate progress lights during a mission
    Rotateprogplus = 0
    If Progress(CurrentPlayer) = -1 Then
        Progress(CurrentPlayer) = Progress(CurrentPlayer) + 2
        UpdateProgressLights
        Rotateprogplus = 2
    Elseif Progress(CurrentPlayer) = 0 Then
        Progress(CurrentPlayer) = Progress(CurrentPlayer) + 1
        UpdateProgressLights
        Rotateprogplus = 1
    End If
    RotateProgressLights.Enabled = 1
    RotateRankLights.Enabled = 1
    if(bMultiBallMode = False) Then
        PLaySong "mu_mission"
    else
        lastsong = "mu_mission"
    end If

    ' increase the base value of the crash bonus if it is still 10000
    If BonusPoints(CurrentPlayer) = 10000 Then BonusPoints(CurrentPlayer) = 25000
    ' a mission has been accepted, select the mission based on the rank,
    Select Case Rank(CurrentPlayer)
        Case 0                                                                                                                   'Cadet
            Select case MissionSelected
                case 1                                                                                                           'Launch Training 'top target
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "LAUNCH TRAINING"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_LAUNCH TRAINING"
                    DMD CL(0, "PASS THE LAUNCH"), CL(1, "RAMP 3 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 1
                    MissionHits = 3
                    li078.State = 2
                    Addscore 10000                                                                                                 '10000 points to accept mission
                case 2                                                                                                             'Re-entry Training 'middle target
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "RE-ENTRY TRAINING"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_RE-ENTRY TRAINING"
                    DMD CL(0, "PASS THE RE-ENTRY"), CL(1, "LANES 3 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 2
                    MissionHits = 3
                    li091.State = 2
                    Addscore 10000                                                                                               '10000 points to accept mission
                case 3                                                                                                           'Target Practice 'lower target
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "TARGET PRACTICE"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_TARGET PRACTICE"
                    DMD CL(0, "HIT THE ATTACK"), CL(1, "BUMPERS 8 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 3
                    MissionHits = 8
                    li085.State = 2
                    Addscore 10000                                                                                               '10000 points to accept mission
                case 4                                                                                                           'Science Mission 'all three targets
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "SCIENCE MISSION"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_SCIENCE MISSION"
                    DMD CL(0, "HIT 9"), CL(1, "DROPTARGETS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 4
                    MissionHits = 9
                    li088.State = 2
                    li079.State = 2
                    li084.State = 2
                    Addscore 10000 '10000 points to accept mission
                    'reset droptargets
                    ResetMedalDT
                    ResetxTargets
                    ResetBoosterTargets
            End Select
        Case 1, 2                                                                                                                 'Ensign & Lieutenant
            Select case MissionSelected
                case 1                                                                                                            'Bug Hunt Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "BUG HUNT MISSION"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_BUG HUNT"
                    DMD CL(0, "HIT 15"), CL(1, "TARGETS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 5
                    MissionHits = 15
                    Addscore 20000 '20000 points to accept mission
                    'hit 15 targets, all target lights goes out and droptargets reset
                    'reset droptargets
                    ResetMedalDT
                    ResetxTargets
                    ResetBoosterTargets
                    'turn off target lights
                    li037.State = 0
                    li038.State = 0
                    li039.State = 0
                    li040.State = 0
                    LightSeqMission3.stopplay
                    LightSeqMission2.stopplay
                    LightSeqMission1.stopplay
                    li041.State = 0
                    li042.State = 0
                    li043.State = 0
                    li052.State = 0
                    li053.State = 0
                    li054.State = 0
                    li056.State = 0
                    li057.State = 0
                    li058.State = 0
                    li059.State = 0
                    li060.State = 0
                    li061.State = 0
                    li062.State = 0
                    li047.State = 0
                    'turn on arrow lights
                    li084.State = 2
                    li076.State = 2
                    li079.State = 2
                    li093.State = 2
                    li088.State = 2
                    li086.State = 2
                    li090.State = 2
                case 2                                                                                                          'Rescue Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "RESCUE MISSION"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_RESCUE MISSION"
                    DMD CL(0, "UPGRADE FLAGS +"), CL(1, "ENTER HYPERSPACE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 6
                    Addscore 20000 '20000 points to accept mission
                    bFlagsUpgraded = False
                    BoosterCount = 0
                    ResetBoosterTargets
                    li079.State = 2
                case 3                                                                                                        'Alien Menace Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "ALIEN MENACE"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_ALIEN MENACE"
                    DMD CL(0, "UPGRADE THE"), CL(1, "ATTACK BUMPERS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    DMD CL(0, "HIT THE ATTACK"), CL(1, "BUMPERS 8 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 7
                    Addscore 20000 '20000 points to accept mission
                    If(AttackBumperColor <> 0) Then
                        AttackBumperColor = 0
                        UpdateBumperColor
                        DowngradeWeapons
                    End If
                    li091.State = 2
                case 4                                                                                                          'Secret Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "SECRET MISSION"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_SECRET MISSION"
                    DMD CL(0, "ENTER THE 3"), CL(1, "WORMHOLES IN ORDER"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 8
                    Addscore 30000 '30000 points to accept mission
                    'prepare the yellow Wormhole
                    TurnOffWormholes
                    SetLightColor li080, yellow, 0
                    SetLightColor li081, red, 0
                    SetLightColor li082, green, 0
                    li144.State = 0
                    li143.State = 0
                    li142.State = 2
                    li080.State = 2
            End Select
        Case 3, 4                                                                                                            'Captain & Lieut. Commander
            Select case MissionSelected
                case 1                                                                                                       'Stray Comet Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "STRAY COMET"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_STRAY COMET"
                    DMD CL(0, "LIGHT ALL 3"), CL(1, "COMET LIGHTS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 9
                    Addscore 20000 '20000 points to accept mission
                    li093.State = 2
                    li052.State = 0
                    li053.State = 0
                    li054.State = 0
                case 2                                                                                                           'Space Radiation Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "SPACE RADIATION"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_SPACE RADIATION"
                    DMD CL(0, "LIGHT ALL 3"), CL(1, "RADIATION LIGHTS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 10
                    Addscore 20000 '20000 points to accept mission
                    li056.State = 0
                    li057.State = 0
                    li058.State = 0
                    li086.State = 2
                case 3                                                                                                             'Black Hole Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "BLACK HOLE THREAT"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_BLACK HOLE THREAT"
                    DMD CL(0, "UPGRADE THE"), CL(1, "ENGINE BUMPERS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 11
                    Addscore 20000 '20000 points to accept mission
                    If(EngineBumperColor <> 0) Then
                        EngineBumperColor = 0
                        UpdateBumperColor
                        DowngradeEngine
                    End If
                    li078.State = 2
                case 4                                                                                                         'Cosmic Plague Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "COSMIC PLAGUE"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_COSMIC PLAGUE"
                    DMD CL(0, "SPIN FLAGS"), CL(1, "75 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 12
                    MissionHits = 75
                    Addscore 30000 '30000 points to accept mission
                    li077.State = 2
                    li134.State = 2
            End Select
        Case 5, 6                                                                                                                    'Commander & Commodore
            Select case MissionSelected
                case 1                                                                                                               'Satellite Retrieval Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "SATELLITE RETRIEVAL"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_SATELLITE RETRIEVAL"
                    DMD CL(0, "HIT 3 TIMES"), "THE SATELLITE BUMPER", "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 13
                    MissionHits = 3
                    Addscore 20000                                                                                             '20000 points to accept mission
                    li090.State = 2
                case 2                                                                                                         'Recon Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "RECON MISSION"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_RECON MISSION"
                    DMD CL(0, "PASS ANY OF THE"), CL(1, "LANES 15 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 14
                    MissionHits = 15
                    Addscore 20000 '20000 points to accept mission
                    li091.State = 2
                    li027.State = 2
                    li070.State = 2
                    li069.State = 2
                    li074.State = 2
                    li075.State = 2
                case 3                                                                                                            'Doomsday Machine Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "DOOMSDAY MACHINE"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_DOOMSDAY MACHINE"
                    DMD CL(0, "SEND BALL 3 TIMES"), CL(1, "THROUGH OUTLINES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 15
                    MissionHits = 3
                    Addscore 20000 '20000 points to accept mission
                    li069.State = 2
                    li075.State = 2
                case 4                                                                                                     'Time Warp Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "TIME WARP"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_TIME WARP"
                    DMD CL(0, "HIT THE REBOUNDS"), CL(1, "25 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    DMD CL(0, "ENTER LAUNCH RAMP"), "OR HYPERSPACE KICKER", "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""

                    MissionHits = 25
                    Addscore 30000 '30000 points to accept mission
                    li071.State = 2
                    li073.State = 2
                    Mission(CurrentPlayer) = 16
            End Select
        Case 7, 8                                                                                                              'Admiral & Fleet Admiral
            Select case MissionSelected
                case 1                                                                                                         'Cosmic Plague Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "COSMIC PLAGUE"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_COSMIC PLAGUE"
                    DMD CL(0, "SPIN FLAGS"), CL(1, "75 TIMES"), "", eNone, eNone, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 12
                    MissionHits = 75
                    Addscore 30000 '30000 points to accept mission
                    li077.State = 2
                    li134.State = 2
                case 2                                                                                                          'Secret Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "SECRET MISSION"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_SECRET MISSION"
                    DMD CL(0, "ENTER THE 3"), CL(1, "WORMHOLES IN ORDER"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 8
                    Addscore 30000 '30000 points to accept mission
                    'prepare the yellow Wormhole
                    TurnOffWormholes
                    SetLightColor li080, yellow, 0
                    SetLightColor li081, red, 0
                    SetLightColor li082, green, 0
                    li144.State = 0
                    li143.State = 0
                    li142.State = 2
                    li080.State = 2
                case 3                                                                                                     'Time Warp Mission
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "TIME WARP"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_TIME WARP"
                    DMD CL(0, "HIT THE REBOUNDS"), CL(1, "25 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    DMD CL(0, "ENTER LAUNCH RAMP"), "OR HYPERSPACE KICKER", "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 16
                    MissionHits = 25
                    Addscore 30000 '30000 points to accept mission
                    li071.State = 2
                    li073.State = 2
                case 4                                                                                                     'Maelstrom
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "MAELSTROM"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_MAELSTROM"
                    DMD CL(0, "STARTING MAELSTROM"), CL(1, "HIT 3 DROPTARGETS"), "", eNone, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 17
                    MissionHits = 3
                    Addscore 30000 '30000 points to accept mission
                    li079.State = 2
                    li084.State = 2
                    li088.State = 2
            End Select
        Case 9 'Admiral & Fleet Admiral
            Select case MissionSelected
                case 1, 2, 3, 4
                    DMDFlush
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "MAELSTROM MULTIBALL"), "", eBlinkFast, eNone, eNone, 1500, True, "SOUND24" ' "vo_MAELSTROM"
                    DMD CL(0, "GO INTO MAELSTROM"), CL(1, "HIT RAMP"), "", eBlink, eScrollLeft, eNone, 1500, True, ""
                    Mission(CurrentPlayer) = 177
                    MissionHits = 1
                    'Addscore 30000 '30000 points to accept mission
                    li099.state = 2
                    li134.state = 2
            End Select
    End Select
End Sub

Sub CheckMission                    'check if mission objectives are completed
    Select Case Mission(CurrentPlayer)
        Case 1                      'Launch Training
            If Missionhits = 0 then 'win the mission
                playsound "sound_3"
                DMDFlush
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(500000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
                Addscore 500000                                                                                                 '500k for finishing the mission
                AddProgress 6                                                                                                   'add 6 progress lights
                LightEffect 2
                bMissionBonus = True:li034.State = 1
                PrepareForMission 'ready for the next mission
            Else
                PlaySfx
            End If
        Case 2                      'Re-entry Training
            If Missionhits = 0 then 'win the mission
                playsound "sound_3"
                DMDFlush
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(500000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
                Addscore 500000                                                                                                 '500k for finishing the mission
                AddProgress 6                                                                                                   'add 6 progress lights
                LightEffect 2
                bMissionBonus = True:li034.State = 1
                PrepareForMission 'ready for the next mission
            Else
                PlaySfx
            End If
        Case 3                      'Target Practice
            If Missionhits = 0 then 'win the mission
                playsound "sound_3"
                DMDFlush
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(500000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
                Addscore 500000                                                                                                 '500k for finishing the mission
                AddProgress 6                                                                                                   'add 6 progress lights
                LightEffect 2
                bMissionBonus = True:li034.State = 1
                PrepareForMission 'ready for the next mission
            Else
                PlaySfx
            End If
        Case 4                      'Science Mission
            If Missionhits = 0 then 'win the mission
                playsound "sound_3"
                DMDFlush
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(750000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
                Addscore 750000                                                                                                 '750k for finishing the mission
                AddProgress 9                                                                                                   'add 9 progress lights
                LightEffect 2
                bMissionBonus = True:li034.State = 1
                PrepareForMission 'ready for the next mission
            Else
                PlaySfx
            End If
        Case 5                      'Bug Hunt Mission
            If Missionhits = 0 then 'win the mission
                playsound "sound_3"
                DMDFlush
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(750000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
                Addscore 750000                                                                                                 '750k for finishing the mission
                AddProgress 7                                                                                                   'add 7 progress lights
                LightEffect 2
                bMissionBonus = True:li034.State = 1
                PrepareForMission 'ready for the next mission
            Else
                PlaySfx
            End If
        Case 6 'Rescue Mission
            If bFlagsUpgraded Then
                DMDFlush
                DMD CL(0, "ENTER THE"), CL(1, "HYPERSPACE KICKER"), "", eNone, eNone, eNone, 1500, True, ""
                Mission(CurrentPlayer) = 61
                li079.State = 0
                li134.State = 2
                PlaySfx
                li123.State = 0
            End If
        Case 61 'Rescue Mission
            playsound "sound_3"
            DMDFlush
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(750000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
            Addscore 750000                                                                                                 '750k for finishing the mission
            AddProgress 7                                                                                                   'add 7 progress lights
            LightEffect 2
            bMissionBonus = True:li034.State = 1
            PrepareForMission 'ready for the next mission
        Case 7                'Alien Menace Mission
            DMDFlush
            DMD CL(0, "HIT THE ATTACK"), CL(1, "BUMPERS 8 TIMES"), "", eNone, eNone, eNone, 1500, True, ""
            Mission(CurrentPlayer) = 71
            PlaySfx
            Missionhits = 8
            li091.State = 0
            li085.State = 2
        case 71                     'Alien Menace Mission
            If Missionhits = 0 then 'win the mission
                playsound "sound_3"
                DMDFlush
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(750000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
                Addscore 750000                                                                                                 '750k for finishing the mission
                AddProgress 7                                                                                                   'add 7 progress lights
                LightEffect 2
                bMissionBonus = True:li034.State = 1
                PrepareForMission 'ready for the next mission
            Else
                PlaySfx
            End If
        Case 8 'Secret Mission
            DMDFlush
            DMD " 1ST TASK COMPLETED", " HIT RED WORMHOLE", "", eBlinkFast, eScrollLeft, eNone, 1500, True, ""
            PlaySfx
            'prepare the red Wormhole
            TurnOffWormholes
            SetLightColor li080, yellow, 0
            SetLightColor li081, red, 0
            SetLightColor li082, green, 0
            li144.State = 0
            li143.State = 2
            li142.State = 0
            li080.State = 0
            li081.State = 2
            li082.State = 0
            Mission(CurrentPlayer) = 81
        Case 81
            DMDFlush
            DMD " 2ND TASK COMPLETED", "HIT GREEN WORMHOLE", "", eBlinkFast, eScrollLeft, eNone, 1500, True, ""
            PlaySfx
            'prepare the green Wormhole
            TurnOffWormholes
            SetLightColor li080, yellow, 0
            SetLightColor li081, red, 0
            SetLightColor li082, green, 0
            li144.State = 2
            li143.State = 0
            li142.State = 0
            li080.State = 0
            li081.State = 0
            li082.State = 2
            Mission(CurrentPlayer) = 82
        Case 82
            playsound "sound_3"
            DMDFlush
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1500000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
            Addscore 1500000                                                                                                 '1,5 Mill for finishing the mission
            AddProgress 10                                                                                                   'add 10 progress lights
            LightEffect 2
            bMissionBonus = True:li034.State = 1
            li081.State = 0
            li082.State = 0
            li082.State = 0
            li144.State = 0
            li143.State = 0
            li142.State = 0
            PrepareForMission 'ready for the next mission
        Case 9                'Stray Comet Mission
            DMDFlush
            DMD "", CL(1, "1ST TASK COMPLETED"), "", eNone, eBlinkFast, eNone, 1500, True, ""
            DMD CL(0, "ENTER THE"), CL(1, "HYPERSPACE KICKER"), "", eNone, eNone, eNone, 1500, True, ""
            PlaySfx
            li093.State = 0
            li134.State = 2
            Mission(CurrentPlayer) = 91
        Case 91
            playsound "sound_3"
            DMDFlush
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1500000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
            Addscore 1000000                                                                                                 '1 Mill for finishing the mission
            AddProgress 8                                                                                                    'add 8 progress lights
            LightEffect 2
            bMissionBonus = True:li034.State = 1
            PrepareForMission 'ready for the next mission
        Case 10               'Space Radiation Mission
            DMDFlush
            DMD " 1ST TASK COMPLETED", " ENTER ANY WORMHOLE", "", eBlinkFast, eScrollLeft, eNone, 1500, True, ""
            PlaySfx
            TurnOffWormholes
            SetLightColor li080, yellow, 0
            SetLightColor li081, red, 0
            SetLightColor li082, green, 0
            li086.State = 0
            li080.State = 2
            li081.State = 2
            li082.State = 2
            li144.State = 2
            li143.State = 2
            li142.State = 2
            Mission(CurrentPlayer) = 101
        Case 101 'Space Radiation Mission
            playsound "sound_3"
            DMDFlush
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1500000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
            Addscore 1000000                                                                                                 '1 Mill for finishing the mission
            AddProgress 8                                                                                                    'add 8 progress lights
            li144.State = 0
            li143.State = 0
            li142.State = 0
            li080.State = 0
            li081.State = 0
            li082.State = 0
            LightEffect 2
            bMissionBonus = True:li034.State = 1

            ChangeArrowColor  'in case the wormholes were activated
            PrepareForMission 'ready for the next mission
        Case 11               'Black Hole Mission
            DMDFlush
            DMD " 1ST TASK COMPLETED", "ENTER THE BLACK HOLE", "", eBlinkFast, eScrollLeft, eNone, 1500, True, ""
            PlaySfx
            Mission(CurrentPlayer) = 111
            li078.State = 0
            li083.State = 2
        Case 111 'Black Hole Mission
            playsound "sound_3"
            DMDFlush
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1500000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
            Addscore 1000000                                                                                                 '1 Mill for finishing the mission
            AddProgress 8                                                                                                    'add 8 progress lights
            LightEffect 2
            bMissionBonus = True:li034.State = 1
            PrepareForMission       'ready for the next mission
        Case 12                     'Cosmic Plague Mission
            If Missionhits = 0 then 'win the mission
                DMDFlush
                DMD " 1ST TASK COMPLETED", "  HIT SPACE WARP", "", eBlinkFast, eScrollLeft, eNone, 1500, True, ""
                li077.State = 0
                li134.State = 0
                li087.State = 2
                Mission(CurrentPlayer) = 121
            End If
        Case 121 'Cosmic Plague Mission
            playsound "sound_3"
            DMDFlush
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1750000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
            Addscore 1750000                                                                                                 '1 Mill for finishing the mission
            AddProgress 11                                                                                                   'add 11 progress lights
            LightEffect 2
            bMissionBonus = True:li034.State = 1
            PrepareForMission       'ready for the next mission
        Case 13                     'Satellite Retrieval Mission
            If Missionhits = 0 then 'win the mission
                DMDFlush
                playsound "sound_3"
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1250000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
                Addscore 1250000                                                                                                 '1,25 Mill for finishing the mission
                AddProgress 9                                                                                                    'add 9 progress lights
                LightEffect 2
                bMissionBonus = True:li034.State = 1
                PrepareForMission 'ready for the next mission
            Else
                PlaySfx
            End If
        Case 14                     'Recon Mission
            If Missionhits = 0 then 'win the mission
                DMDFlush
                playsound "sound_3"
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1250000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
                Addscore 1250000                                                                                                 '1,25 Mill for finishing the mission
                AddProgress 9                                                                                                    'add 9 progress lights
                LightEffect 2
                bMissionBonus = True:li034.State = 1
                PrepareForMission 'ready for the next mission
            Else
                PlaySfx
            End If
        Case 15                     'Doomsday Machine Mission
            If Missionhits = 0 then 'win the mission
                DMDFlush
                playsound "sound_3"
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1250000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
                Addscore 1250000                                                                                                 '1,25 Mill for finishing the mission
                AddProgress 9                                                                                                    'add 9 progress lights
                LightEffect 2
                bMissionBonus = True:li034.State = 1
                PrepareForMission 'ready for the next mission
            Else
                PlaySfx
            End If
        Case 16                     'Time Warp Mission
            If Missionhits = 0 then 'win the mission
                DMDFlush
                DMD " 1ST TASK COMPLETED", "", "", eBlinkFast, eNone, eNone, 1500, True, ""
                DMD "HIT THE LAUNCH RAMP", " OR THE HYPERSPACE", "", eNone, eNone, eNone, 1500, True, ""
                li071.State = 0
                li073.State = 0
                li078.State = 2
                li134.State = 2
                li097.State = 2
                li100.State = 2
                Mission(CurrentPlayer) = 161
            Else
                PlaySfx
            End If
        Case 161 'Time Warp Mission
            playsound "sound_3"
            DMDFlush
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(2000000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
            Addscore 2000000                                                                                                 '2 Mill for finishing the mission
            AddProgress 12                                                                                                   'add 12 progress lights
            LightEffect 2
            bMissionBonus = True:li034.State = 1
            li097.State = 0
            li100.State = 0
            li034.State = 0
            li078.State = 0
            PrepareForMission       'ready for the next mission
        Case 17                     ' Maelstrom "DROPTARGETS LEFT " & MissionHits
            If Missionhits = 0 then '
                DMDFlush
                DMD CL(0, " 1ST TASK COMPLETED"), "HIT 3 SPOT TARGETS", "", eBlinkFast, eScrollLeft, eNone, 1500, True, ""
                li079.State = 0
                li084.State = 0
                li088.State = 0
                Missionhits = 3
                li076.State = 2
                li079.State = 2
                li086.State = 2
                li090.State = 2
                Mission(CurrentPlayer) = 171
            Else
                PlaySfx
            End If
        Case 171 '"SPOT TARGETS LEFT " & MissionHits
            If Missionhits = 0 then
                DMDFlush
                DMD CL(0, " 2ND TASK COMPLETED"), "HIT 5 LANES", "", eBlinkFast, eScrollLeft, eNone, 1500, True, ""
                li076.State = 0
                li079.State = 0
                li086.State = 0
                li090.State = 0
                Missionhits = 5
                li091.State = 2
                li027.State = 2
                li070.State = 2
                li069.State = 2
                li074.State = 2
                li075.State = 2
                Mission(CurrentPlayer) = 172
            Else
                PlaySfx
            End If
        Case 172 '"LANES LEFT " & MissionHits
            If Missionhits = 0 then
                DMDFlush
                DMD CL(0, " 3RD TASK COMPLETED"), " HIT THE FUEL CHUTE", "", eBlinkFast, eScrollLeft, eNone, 1500, True, ""
                li091.State = 0
                li027.State = 0
                li070.State = 0
                li069.State = 0
                li074.State = 0
                li075.State = 0
                li077.State = 2
                Mission(CurrentPlayer) = 173
            Else
                PlaySfx
            End If
        Case 173 '" HIT THE FUEL CHUTE"
            DMDFlush
            DMD CL(0, " 4TH TASK COMPLETED"), " HIT THE LAUNCH RAMP", "", eBlinkFast, eScrollLeft, eNone, 1500, True, ""
            li077.State = 0
            li078.State = 2
            Mission(CurrentPlayer) = 174
        Case 174 '"HIT THE LAUNCH RAMP"
            DMDFlush
            DMD CL(0, " 5TH TASK COMPLETED"), "   ROLL A FLAG", "", eBlinkFast, eScrollLeft, eNone, 1500, True, ""
            li078.State = 0
            li077.State = 2
            li134.State = 2
            Mission(CurrentPlayer) = 175
        Case 175 '"ROLL A FLAG"
            DMDFlush
            DMD CL(0, " 6TH TASK COMPLETED"), "  ENTER A WORMHOLE", "", eBlinkFast, eScrollLeft, eNone, 1500, True, ""

            TurnOffWormholes
            li077.State = 0
            li134.State = 0
            li080.State = 2
            li081.State = 2
            li082.State = 2
            li144.State = 2
            li143.State = 2
            li142.State = 2
            Mission(CurrentPlayer) = 176
        Case 176 '"ENTER A WORMHOLE"
            DMDFlush
            DMD CL(0, " 7TH TASK COMPLETED"), "  ENTER HYPERSPACE", "", eBlinkFast, eScrollLeft, eNone, 1500, True, ""

            li080.State = 0
            li081.State = 0
            li082.State = 0
            li144.State = 0
            li143.State = 0
            li142.State = 0
            li134.State = 2
            li099.State = 2
            Mission(CurrentPlayer) = 177
        Case 177                                                                                                             '"ENTER HYPERSPACE"
            DMDFlush
            DMD CL(0, "MAELSTROM ENGAGED"), CL(1, FormatScore(5000000) ), "", eBlinkFast, eScrollLeft, eNone, 1500, True, "" '"vo_Mission Completed"
            playsound "sc_7"
            Addscore 5000000                                                                                                 '5 Mill for finishing the mission
            if(Rank(CurrentPlayer) = 7) Or(Rank(CurrentPlayer) = 8) Then
                AddProgress 12                                                                                               'add 12 progress lights
            end If
            if(Rank(CurrentPlayer)> 8) Then
                Rank(CurrentPlayer) = 8
                Progress(CurrentPlayer) = 0
            end If

            'RotateProgressLights.Enabled = 0:UpdateProgressLights
            RotateProgressLights.Enabled = 0:progress(currentplayer) = progress(currentplayer) - Rotateprogplus:UpdateProgressLights
            Rotateprogplus = 0
            LightEffect 2

            StopSound "mu_main"
            StopSound "mu_mission"

            Playsound "maelmulti"

            LightSeqGi.UpdateInterval = 20
            LightSeqGi.Play SeqBlinking, , 1000, 10
            LightSeqInserts.UpdateInterval = 20
            LightSeqInserts.Play SeqBlinking, , 1000, 10
            DMD CL(0, "MAELSTROM"), CL(1, "MULTIBALL"), "", eBlinkFast, eBlinkFast, eNone, 6000, True, "" '"vo_Mission Completed"
            lastsong = Song
            bMael = 1
            vpmtimer.addtimer 5950, "HyperSpaceHits=10'"
            vpmtimer.addtimer 6000, "AddMultiball 3 : PlaySong ""mu_multiball"" : EnableBallSaver 30 : AsteroidsTimer.Enabled = 1 : " &_
            "hyperspacelightpursuit : maellightpursuit : LightSeqGi.stopplay : LightSeqInserts.stopplay '"
            vpmtimer.addtimer 6100, "PrepareForMission '" 'ready for the next mission
            vpmtimer.addtimer 6200, "maeljack=1'"
    End Select
End Sub

Sub StopMissions 'this will stop all missions. Called  at the end of the ball or when the fuel runs out
    If Fuel = 0 Then
        DMDFlush
        DMD "", CL(1, "MISSION ABORTED"), "", eNone, eNone, eNone, 1500, True, "" '"vo_Mission Aborted"
    End If
    RotateProgressLights.Enabled = 0:progress(currentplayer) = progress(currentplayer) - Rotateprogplus:UpdateProgressLights
    RotateRankLights.Enabled = 0
    for each i in aArrows:i.State = 0:Next
    Mission(CurrentPlayer) = 0
    MissionSelected = 0
    li123.State = 0
    Rotateprogplus = 0
End Sub

dim maellight:maellight = 0
sub maellightpursuit
    maellightpursuitTimer.Enabled = 1
End Sub

sub maellightpursuitTimer_Timer
    dim i
    maellight = maellight + 1

    if maellight> 1 then maellight = 0
    select case maellight
        case 0:For i = 0 to 17 step 2
            ProgressLights(i).State = 1
            Next
            For i = 0 to 17 step 2
                ProgressLights(i + 1).State = 0
            Next
        case 1:For i = 0 to 17 step 2
            ProgressLights(i + 1).State = 1
            Next
            For i = 0 to 17 step 2
                ProgressLights(i).State = 0
            Next
    End Select
end Sub

'DOF Update it by Outhere - Search the word > outhere < for DOF I Changed or Added
'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 Right Slingshot
'105
'106 Right Slingshot Shake
'107 Top Bumper Left
'108 Top Bumper Center
'109 Top Bumper Right
'110 Lower Bumper Left
'111 Lower Bumper CenterLeft
'112 Lower Bumper Right
'113 Very Top Bumper Left
'114 Wormhole 1
'115 Wormhole 2
'116 Wormhole 3
'117 Drop Targets Left
'118 GI on / off
'119 Drop Targets Center
'120 Drop Targets Right
'121
'122 Knocker
'123 Ball Release
'124 Left Kick Back
'125
'126sc_droptarget
'127
'128 Right Kick Back
'129 BlackHole Kick Out
'130 HyperSpaceHole Kick Out
'131 Upper Left Slingshot
'132 Upper Right Slingshot
'133 Shaker
'134 Beacon - BallLock
'135
'136
'137
'138
'139
'140
'141
'142
'143 AutoPlunger
'144
'146
'147
'148
'149
'150
'151
'152
'153
'154
'155

'DMD CL(0,""), CL(1,""), "", eNone, eNone, eNone, 1500, True, ""