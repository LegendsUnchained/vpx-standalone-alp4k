' ****************************************************************
'             IT for VISUAL PINBALL X 10.8
'    Table built by jpsalas with graphics by Joe Picasso
'         Uses FlexDMD for cabinet / FS mode
' ****************************************************************

Option Explicit
Randomize

'==================== PLayer Options ==================================

' Sound and music volumes
Const SongVolume = 0.1 ' 1 is full volume, but I set it quite low to listen better the other sounds since I use headphones, adjust to your setup :)

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
'or keep it False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = True

'==================== End of Player Options ===========================

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
Const cGameName = "it_pinball_madness" 'used for DOF
Const myVersion = "5.5.0"
Const MaxPlayers = 4         ' from 1 to 4
Const MaxMultiplier = 6      ' limit playfield multiplier
Const MaxBonusMultiplier = 6 'limit Bonus multiplier
Const BallsPerGame = 3       ' usually 3 or 5
Const MaxMultiballs = 7      ' max number of balls during multiballs

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
Dim BallSaverTime ' in seconds of the first ball
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
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode
Dim ComboCount
Dim ComboHits(4)
Dim ComboValue(4)
Dim x

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
Dim bMultiBallStarted
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJackpot

' core.vbs variables
Dim plungerIM   'used mostly as an autofire plunger during multiballs
Dim TopMagnet   'at the spinner
Dim LeftMagnet  'Georgie targets
Dim RightMagnet 'Fear targets

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
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

    ' TopMagnet
    Set TopMagnet = New cvpmMagnet
    With TopMagnet
        .InitMagnet Magnet1, 35
        .GrabCenter = True
        .CreateEvents "TopMagnet"
    End With

    ' Left Magnet
    Set LeftMagnet = New cvpmMagnet
    With LeftMagnet
        .InitMagnet Magnet3, 20
        .GrabCenter = False
        .CreateEvents "LeftMagnet"
    End With

    ' Right Magnet
    Set RightMagnet = New cvpmMagnet
    With RightMagnet
        .InitMagnet Magnet2, 15
        .GrabCenter = False
        .CreateEvents "RightMagnet"
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

    'if bFreePlay Then DOF 125, DOFOn

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
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bBonusHeld = False
    bJackpot = False
    bInstantInfo = False
    ' set any lights for the attract mode
    vpmtimer.addtimer 2000, "GiOn '"
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

    If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound "fx_nudge", 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 6:PlaySound "fx_nudge", 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound "fx_nudge", 0, 1, 1, 0.25

    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If

    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
        Credits = Credits + 1
        'if bFreePlay = False Then DOF 125, DOFOn
        If(Tilted = False)Then
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

        If keycode = LeftTiltKey Then CheckTilt 'only check the tilt during game
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt
        If keycode = MechanicalTilt Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 1
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 0

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                        If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, ""
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
                            If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
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

    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT

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

' test Keys
End Sub

Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = False
    If NOT hsbModeActive Then
        bInstantInfo = True
        DMDFlush
        InstantInfo
    End If
End Sub

Sub RotateLaneLights(dir)
    Dim tmp
    If dir = 1 Then 'rotate left
        tmp = TopLanes(CurrentPLayer, 1)
        TopLanes(CurrentPLayer, 1) = TopLanes(CurrentPLayer, 2)
        TopLanes(CurrentPLayer, 2) = TopLanes(CurrentPLayer, 3)
        TopLanes(CurrentPLayer, 3) = tmp
        tmp = LowerLanes(CurrentPLayer, 1)
        LowerLanes(CurrentPLayer, 1) = LowerLanes(CurrentPLayer, 2)
        LowerLanes(CurrentPLayer, 2) = LowerLanes(CurrentPLayer, 3)
        LowerLanes(CurrentPLayer, 3) = LowerLanes(CurrentPLayer, 4)
        LowerLanes(CurrentPLayer, 4) = tmp
    Else 'rotate RightFlipper
        tmp = TopLanes(CurrentPLayer, 3)
        TopLanes(CurrentPLayer, 3) = TopLanes(CurrentPLayer, 2)
        TopLanes(CurrentPLayer, 2) = TopLanes(CurrentPLayer, 1)
        TopLanes(CurrentPLayer, 1) = tmp
        tmp = LowerLanes(CurrentPLayer, 4)
        LowerLanes(CurrentPLayer, 4) = LowerLanes(CurrentPLayer, 3)
        LowerLanes(CurrentPLayer, 3) = LowerLanes(CurrentPLayer, 2)
        LowerLanes(CurrentPLayer, 2) = LowerLanes(CurrentPLayer, 1)
        LowerLanes(CurrentPLayer, 1) = tmp
    End If
    UpdateTopLaneLights
    UpdateLowerLaneLights
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
        RightFlipper001.RotateToEnd
        RightFlipperOn = 1
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipper001.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper001_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt 'Called when table is nudged
    Dim BOT
    BOT = GetBalls
    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub
    Tilt = Tilt + TiltSensitivity                 'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt > TiltSensitivity)AND(Tilt <= 15)Then 'show a warning
        DMD "_", CL("CAREFUL"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
    End if
    If(NOT Tilted)AND Tilt > 15 Then 'If more that 15 then TILT the table
        'display Tilt
        InstantInfoTimer.Enabled = False
        DMDFlush
        DMD CL("YOU"), CL("TILTED"), "", eNone, eNone, eNone, 3000, True, "vo_tilt" &RndNbr(3)
        DOF 146, DOFPulse                'MX Tilted
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
        StopMBmodes
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
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
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper001.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        Bumper3.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        TopMagnet.MagnetOn = False
        LeftMagnet.MagnetOn = False
        RightMagnet.MagnetOn = False
    Else
        Tilted = False
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        Bumper3.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        vpmtimer.Addtimer 2000, "EndOfBall() '"
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
    Select Case Mode(CurrentPlayer, 0)
        Case 0:
            If bMultiBallStarted Then
                Select Case MultiballType(CurrentPlayer)
                    Case 1:PlaySong "m_multiball1"
                    Case 2:PlaySong "m_multiball2"
                    Case 3:PlaySong "m_multiball1"
                    Case 4:PlaySong "m_multiball2"
                End Select
            Else
                PlaySong "m_main"
            End If
        Case 1, 2, 3:PlaySong "m_mode1"
        Case 5, 6, 7:PlaySong "m_mode2"
        Case 7, 8, 9, 10, 11:PlaySong "m_mode3"
        Case 12, 13, 14:PlaySong "m_main2"
    End Select
End Sub

Sub StopSong(name)
    StopSound name
End Sub

'********************
' Play random sounds
'********************

Dim Thunder:Thunder = ""

Sub PlayThunder
    StopSound Thunder
    Thunder = "sfx_thunder" &RndNbr(13)
    PlaySound Thunder, , 0.2
    FlashForMs ThunderFlasher, 2000 * RND, 50, 0
    LightSeqHouse.StopPlay
    LightSeqHouse.Play SeqRandom, 10, , 6000
End Sub

Sub PlayElectro
    PlaySound "sfx_electro" &RndNbr(9), , 0.3
End Sub

'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim GiIntensity
GiIntensity = 1   'used for the LUT changing to increase the GI lights when the table is darker

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
    PlaySoundAt "fx_GiOn", li000 'about the center of the table
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aGiBlink
        bulb.State = 2
    Next
    DarkNight.Visible = 0
    aBallGlowing = False
    For each bulb in aBallGlow
        bulb.Visible = 0
    Next
End Sub

Sub GiOff
    PlaySoundAt "fx_GiOff", li000 'about the center of the table
    DOF 118, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aGiBlink
        bulb.State = 0
    Next
End Sub

Sub NightMode                     'a kind of Gi Off but darker, stops by turning the GI back on
    PlaySoundAt "fx_GiOff", li000 'about the center of the table
    DOF 118, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aGiBlink
        bulb.State = 2
    Next
    DarkNight.Visible = 1
    aBallGlowing = True
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
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
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
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
            LightSeqInserts.Play SeqUpOn, 15, 1
    End Select
End Sub

Sub FlashEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqFlashers.Play SeqAlloff
        Case 1 'all blink
            LightSeqFlashers.UpdateInterval = 40
            LightSeqFlashers.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqFlashers.UpdateInterval = 25
            LightSeqFlashers.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqFlashers.UpdateInterval = 20
            LightSeqFlashers.Play SeqBlinking, , 10, 20
        Case 4 'center
            LightSeqFlashers.UpdateInterval = 10
            LightSeqFlashers.Play SeqCircleOutOn, 15, 1
        Case 5 'top down
            LightSeqFlashers.UpdateInterval = 10
            LightSeqFlashers.Play SeqDownOn, 15, 1
        Case 6 'down to top
            LightSeqFlashers.UpdateInterval = 10
            LightSeqFlashers.Play SeqUpOn, 15, 1
        Case 7 'top flashers left right
            LightSeqTopFlashers.UpdateInterval = 10
            LightSeqTopFlashers.Play SeqRightOn, 50, 10
        Case 8 'It
            LightSeqIT.UpdateInterval = 10
            LightSeqIT.Play SeqBlinking, , 15, 15
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
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 50
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
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls, 20 balls, from 0 to 19
Const lob = 1     'number of locked balls
Const maxvel = 45 'max ball velocity
ReDim rolling(tnob)

Dim aBallGlowing:aBallGlowing = False
Dim aBallWhiteGlowing:aBallWhiteGlowing = False

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
        aBallShadow(b).Y = 2072 'hide it under the apron
        aBallGlow(b).Visible = 0
        aBallGlow2(b).Visible = 0
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z -24

        If aBallGlowing Then
            aBallGlow(b).Visible = 1
            aBallGlow(b).X = BOT(b).X
            aBallGlow(b).Y = BOT(b).Y + 10
            aBallGlow(b).Height = BOT(b).Z + 32
        End If

        If aBallWhiteGlowing Then
            aBallGlow2(b).Visible = 1
            aBallGlow2(b).X = BOT(b).X
            aBallGlow2(b).Y = BOT(b).Y + 10
            aBallGlow2(b).Height = BOT(b).Z + 32
        End If

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
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
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
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
'                        User Defined Script Events
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
    UpdateBallInPlay

    ' you may wish to start some music, play a sound, do whatever at this point
    PlaySound "vo_start" &RndNbr(3)

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
    SetBonusMultiplier 1

    ' reduce the playfield multiplier by 1
    DecreasePlayfieldMultiplier

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
    UpdateBallInPlay

    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4

' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield > 1 Then
        DOF 143, DOFPulse
        bMultiBallMode = True
        bAutoPlunger = True
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

Sub UpdateBallInPlay
    If B2SOn Then
        if BallsOnPlayfield = 0 then
            Controller.B2sSetData 50, 0:Controller.B2sSetData 51, 0:Controller.B2sSetData 52, 0
        Else
            select case BallsRemaining(CurrentPlayer)
                Case 5:Controller.B2sSetData 50, 1:Controller.B2sSetData 51, 0:Controller.B2sSetData 52, 0
                Case 4:Controller.B2sSetData 50, 1:Controller.B2sSetData 51, 0:Controller.B2sSetData 52, 0
                Case 3:Controller.B2sSetData 50, 1:Controller.B2sSetData 51, 0:Controller.B2sSetData 52, 0
                Case 2:Controller.B2sSetData 50, 0:Controller.B2sSetData 51, 1:Controller.B2sSetData 52, 0
                Case 1:Controller.B2sSetData 50, 0:Controller.B2sSetData 51, 0:Controller.B2sSetData 52, 1
                Case 0:Controller.B2sSetData 50, 0:Controller.B2sSetData 51, 0:Controller.B2sSetData 52, 0
            end select
        end if
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 10 'yes 10 points :)
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False
    GiOff
    ' only process any of this if the table is not tilted.
    '(the tilt recovery mechanism will handle any extra balls or end of game)

    If NOT Tilted Then
        PlaySong "m_bonus"
        'Count the bonus. This table uses several bonus
        DMD CL("BONUS"), "", "", eNone, eNone, eNone, 1000, True, ""

        'Children rescued + bullies x 250.000
        AwardPoints = ModesWon(CurrentPlayer) * 250000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("CHILDREN - BULLIES " & ModesWon(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eNone, eNone, 800, True, ""
        LightEffect 4

        'Balloons popped X 100.000
        AwardPoints = Balloons(CurrentPlayer) * 100000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("BALLOONS POPPED " & Balloons(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eNone, eNone, 800, True, ""
        vpmTimer.AddTimer 800, "LightEffect 4 '"

        'Orbits X 75.000
        AwardPoints = OrbitHits(CurrentPlayer) * 75000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("ORBIT HITS " & OrbitHits(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eNone, eNone, 800, True, ""
        vpmTimer.AddTimer 1600, "LightEffect 4 '"

        'Combos X 75.000
        AwardPoints = ComboHits(CurrentPlayer) * 75000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("COMBO HITS " & ComboHits(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eNone, eNone, 800, True, ""
        vpmTimer.AddTimer 2400, "LightEffect 4 '"

        'Bumpers X 10.000
        AwardPoints = BumperHits(CurrentPlayer) * 75000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("BUMPER HITS " & BumperHits(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eNone, eNone, 800, True, ""
        vpmTimer.AddTimer 3200, "LightEffect 4 '"

        If TotalBonus > 750000 Then
            DMD CL("TOTAL BONUS X MULT"), CL(FormatScore(TotalBonus * BonusMultiplier(CurrentPlayer))), "", eNone, eNone, eNone, 2000, True, "vo_greatbonus" &RndNbr(3)
        Else
            DMD CL("TOTAL BONUS X MULT"), CL(FormatScore(TotalBonus * BonusMultiplier(CurrentPlayer))), "", eNone, eNone, eNone, 2000, True, "vo_bonus" &RndNbr(2)
        End If
        vpmTimer.AddTimer 4000, "LightEffect 4 '"
        Score(CurrentPlayer) = Score(CurrentPlayer) + TotalBonus * BonusMultiplier(CurrentPlayer)

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
    If ExtraBallsAwards(CurrentPlayer) > 0 Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if no more EB's then turn off any Extra Ball light if there was any
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, "vo_extraball" &RndNbr(2)

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            ' debug.print "No More Balls, High Score Entry"
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
        DMDScoreNow

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame > 1 Then
            Select Case CurrentPlayer
                Case 1:DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, "vo_player1"
                Case 2:DMD "", CL("PLAYER 2"), "", eNone, eNone, eNone, 1000, True, "vo_player2"
                Case 3:DMD "", CL("PLAYER 3"), "", eNone, eNone, eNone, 1000, True, "vo_player3"
                Case 4:DMD "", CL("PLAYER 4"), "", eNone, eNone, eNone, 1000, True, "vo_player4"
            End Select
        Else
            DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, "vo_youareup"
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    ChangeSong
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
    StartAttractMode
' you may wish to light any Game Over Light you may have
End Sub

'this calculates the ball number in play
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

        ' is the ball saver active,
        If(bBallSaverActive = True)Then

            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            ' stop the ballsaver timer during the launch ball saver time, but not during multiballs
            If NOT bMultiBallMode Then
                DMD "_", CL("BALL SAVED"), "_", eNone, eBlinkfast, eNone, 2500, True, "vo_ballsaved" &RndNbr(6)
                BallSaverTimerExpired_Timer
            End If
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    changesong
                    ' turn off any multiball specific lights
                    ChangeGIIntensity 1
                    ChangeGi white
                    'stop any multiball modes of this game
                    StopMBmodes
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
                ' End Mode and timers
                DOF 145, DOFPulse
                StopSong Song
                ChangeGIIntensity 1
                ChangeGi white
                UpdateBallInPlay
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
    ' be sure to update the Scoreboard after the animations, if any
    ' if the ball goes into the plunger lane during a multiball then activate the autoplunger
    If bMultiBallMode Then
        bAutoPlunger = True ' kick the ball in play if the bAutoPlunger flag is on
    Else
        DOF 147, DOFOn      'MX ball waiting
    End If
    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        vpmtimer.addtimer 1500, "PlungerIM.AutoFire:DOF 120, DOFPulse:PlaySoundAt ""fx_kicker"", swPlungerRest:bAutoPlunger = False '"
    End If
    'Start the skillshot lights & variables if any
    If bSkillShotReady Then
        ChangeSong
        UpdateSkillshot()
        ' show the message to shoot the ball in case the player has fallen sleep
        swPlungerRest.TimerEnabled = 1
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    lighteffect 6
    Dof 147, DOFoff                                                          'Turn off ball waiting
    bBallInPlungerLane = False
    If bMultiBallMode = False or bAutoPlunger = False Then Dof 148, DOFPulse 'MX plunger Streak
    swPlungerRest.TimerEnabled = 0                                           'stop the launch ball timer if active
    If bSkillShotReady Then
        ChangeSong
        ResetSkillShotTimer.Enabled = 1
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
        EnableBallSaver BallSaverTime
    End If
' turn off LaunchLight
' LaunchLight.State = 0
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball after a while
Sub swPlungerRest_Timer
    PlaySound "wakeup" &RndNbr(6) 'there are only 3 sounds in the table, so it will play a sound about 50% of times
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
    BallSaverSpeedUpTimer.Enabled = False 'ensure this timer is also stopped
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
    LightShootAgain.State = 0
    ' if the table uses the same lights for the extra ball or replay then turn them on if needed
    If ExtraBallsAwards(CurrentPlayer) > 0 Then
        LightShootAgain.State = 1
    End If
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

Sub AddScore(points) 'normal score routine; points x playfieldmultiplier
    If Tilted Then Exit Sub
    If bSkillshotReady Then ResetSkillShotTimer_Timer
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

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If Tilted Then Exit Sub

    If(bMultiBallMode = True)Then
        Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
        ' DMD "_", CL("INCREASED JACKPOT"), "_", eNone, eNone, eNone, 1000, True, ""
        ' you may wish to limit the jackpot to a upper limit, ie..
        If(Jackpot(CurrentPlayer) >= 1000000)Then
            Jackpot(CurrentPlayer) = 1000000
        End if
    End if
End Sub

Sub AddSuperJackpot(points)
    If Tilted Then Exit Sub
    If(bMultiBallMode = True)Then
        SuperJackpot(CurrentPlayer) = SuperJackpot(CurrentPlayer) + points
        ' DMD "_", "INCREASED SP.JACKPOT", "_", eNone, eNone, eNone, 1000, True, ""
        ' you may wish to limit the jackpot to a upper limit, ie..
        If(SuperJackpot(CurrentPlayer) >= 9000000)Then
            SuperJackpot(CurrentPlayer) = 9000000
        End if
    End if
End Sub

Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxBonusMultiplier)then
        ' then add and set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
        SetBonusMultiplier(NewBonusLevel)
        DMD "_", CL("BONUS X " &NewBonusLevel), "_", eNone, eBlink, eNone, 2000, True, ""
        GiEffect 1
    Else
        AddScore 500000
        DMD "_", CL("500000"), "_", eNone, eNone, eNone, 1000, True, ""
    End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly

Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level
    UpdateBonusXLights(Level)
End Sub

Sub UpdateBonusXLights(Level) '4 lights in this table, from 2x to 5x
    ' Update the lights
    Select Case Level
        Case 1:li070.State = 0:li071.State = 0:li072.State = 0:li073.State = 0:li074.State = 0
        Case 2:li070.State = 1:li071.State = 0:li072.State = 0:li073.State = 0:li074.State = 0
        Case 3:li070.State = 1:li071.State = 1:li072.State = 0:li073.State = 0:li074.State = 0
        Case 4:li070.State = 1:li071.State = 1:li072.State = 1:li073.State = 0:li074.State = 0
        Case 5:li070.State = 1:li071.State = 1:li072.State = 1:li073.State = 1:li074.State = 0
        Case 6:li070.State = 1:li071.State = 1:li072.State = 1:li073.State = 1:li074.State = 1
    End Select
End Sub

Sub AddPlayfieldMultiplier(n)
    Dim NewPFLevel
    ' if not at the maximum level x
    if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier)then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
        SetPlayfieldMultiplier(NewPFLevel)
        DMD "_", CL("PLAYFIELD X " &NewPFLevel), "_", eNone, eBlink, eNone, 2000, True, ""
        GiEffect 1
    Else 'if the max is already lit
        AddScore 500000
        DMD "_", CL("500000"), "_", eNone, eNone, eNone, 2000, True, ""
    End if
    ' restart the PlayfieldMultiplier timer to reduce the multiplier
    PFXTimer.Enabled = 0
    PFXTimer.Enabled = 1
End Sub

Sub PFXTimer_Timer
    DecreasePlayfieldMultiplier
End Sub

Sub DecreasePlayfieldMultiplier 'reduces by 1 the playfield multiplier
    Dim NewPFLevel
    ' if not at 1 already
    if(PlayfieldMultiplier(CurrentPlayer) > 1)then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer)- 1
        SetPlayfieldMultiplier(NewPFLevel)
        PFXTimer.Enabled = 1
    Else
        PFXTimer.Enabled = 0
    End if
End Sub

' Set the Playfield Multiplier to the specified level AND set any lights accordingly

Sub SetPlayfieldMultiplier(Level)
    ' Set the multiplier to the specified level
    PlayfieldMultiplier(CurrentPlayer) = Level
    UpdatePFXLights(Level)
End Sub

Sub UpdatePFXLights(Level) 'no lights in this table, the multiplier is shown in the DMD
    ' Update the playfield multiplier lights
    Select Case Level
        Case 1:li065.State = 0:li066.State = 0:li067.State = 0:li068.State = 0:li069.State = 0
        Case 2:li065.State = 1:li066.State = 0:li067.State = 0:li068.State = 0:li069.State = 0
        Case 3:li065.State = 1:li066.State = 1:li067.State = 0:li068.State = 0:li069.State = 0
        Case 4:li065.State = 1:li066.State = 1:li067.State = 1:li068.State = 0:li069.State = 0
        Case 5:li065.State = 1:li066.State = 1:li067.State = 1:li068.State = 1:li069.State = 0
        Case 6:li065.State = 1:li066.State = 1:li067.State = 1:li068.State = 1:li069.State = 1
    End Select
' perhaps show also the multiplier in the DMD?
End Sub

Sub ExtraBallIsLit
    DMD "_", CL("EXTRA BALL IS LIT"), "", eNone, eNone, eNone, 1500, True, "vo_extraballislit"
    li044.State = 1
End Sub

Sub AwardExtraBall()
    '   If NOT bExtraBallWonThisBall Then 'uncomment this If in case you want to give just one extra ball per ball
    DMD "_", CL("EXTRA BALL WON"), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
    '    bExtraBallWonThisBall = True
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
    DMD CL("JACKPOT"), CL(FormatScore(Jackpot(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 1500, True, "vo_Jackpot"
    DOF 126, DOFPulse
    AddScore Jackpot(CurrentPlayer)
    AddJackpot 50000
    LightEffect 2
    GiEffect 3
    FlashEffect 2
End Sub

Sub AwardSuperJackpot()
    DMD CL("SUPER JACKPOT"), CL(FormatScore(SuperJackpot(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_superjackpot"
    DOF 126, DOFPulse
    AddScore SuperJackpot(CurrentPlayer)
    AddSuperJackpot 100000
    LightEffect 2
    GiEffect 3
End Sub

Sub SuperJackpotTimer_Timer 'timer to stop superjackpots at Pennywise after 30 seconds
    SuperJackpotTimer.Enabled = 0
    SPJackpotLight(5) = 0
    li045.State = 0
End Sub

Sub AwardSkillshot(points)
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL("SKILLSHOT"), CL(FormatScore(points)), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_skillshot"
    DOF 127, DOFPulse
    AddScore points
    'do some light show
    GiEffect 3
    LightEffect 2
End Sub

Sub AwardSuperSkillshot(points)
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL("SUPER SKILLSHOT"), CL(FormatScore(points)), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_superskillshot"
    DOF 127, DOFPulse
    AddScore points
    'do some light show
    GiEffect 3
    LightEffect 2
End Sub

'**************
'   COMBOS
'**************

Sub AwardCombo
    DOF 128, DOFPulse                                       'Combo
    ComboCount = ComboCount + 1
    ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1 'bonus
    Select Case ComboCount
        Case 1:DMD CL("COMBO"), CL(FormatScore(ComboValue(CurrentPlayer))), "", eNone, eNone, eNone, 1500, True, "vo_combo1":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 2:DMD CL("2X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 2)), "", eNone, eNone, eNone, 1500, True, "vo_combo2":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 3:DMD CL("3X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 3)), "", eNone, eNone, eNone, 1500, True, "vo_combo3":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 4:DMD CL("4X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 4)), "", eNone, eNone, eNone, 1500, True, "vo_combo4":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 5:DMD CL("5X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 5)), "", eNone, eNone, eNone, 1500, True, "vo_combo5":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case Else:DMD CL("SUPER COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 10)), "", eNone, eNone, eNone, 1500, True, "vo_supercombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
    End Select
    AddScore ComboValue(CurrentPlayer) * ComboCount
    ComboValue(CurrentPlayer) = ComboValue(CurrentPlayer) + 100000
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Dim MyTable
MyTable = "JPsIT"

Sub Loadhs
    Dim x
    x = LoadValue(MyTable, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 100000 End If
    x = LoadValue(MyTable, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(MyTable, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 100000 End If
    x = LoadValue(MyTable, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(MyTable, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 100000 End If
    x = LoadValue(MyTable, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(MyTable, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 100000 End If
    x = LoadValue(MyTable, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(MyTable, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(MyTable, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
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

    If tmp > HighScore(0)Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
    'DOF 125, DOFOn
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
    PlaySound "vo_greatscore" &RndNbr(6)
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
    dLine(0) = ExpandLine(TempTopStr)
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
    dLine(1) = ExpandLine(TempBotStr)
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

' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text.
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
    if(dqHead = dqTail)Then
        ' default when no modes are active
        tmp = RL(FormatScore(Score(Currentplayer)))
        tmp1 = FL("PLAYER " &CurrentPlayer, "BALL " & Balls)
        tmp2 = "d_border"
        'info on the second line: tmp1
        Select Case Mode(CurrentPlayer, 0)
            Case 0 'no Mode active
            Case 1
                tmp = FL("PLAYER " &CurrentPlayer, FormatScore(Score(Currentplayer)))
                tmp1 = CL("SPINNERS LEFT " & SpinsToCount1-SpinCount1)
            Case 2
                tmp = FL("PLAYER " &CurrentPlayer, FormatScore(Score(Currentplayer)))
                tmp1 = CL("BUMPERS LEFT " & BumpersToHit2-BumperHits2)
            Case 3
                tmp = FL("PLAYER " &CurrentPlayer, FormatScore(Score(Currentplayer)))
                tmp1 = CL("RAMPHITS LEFT " & RampsToHit3-RampHits3)
            Case 4
                tmp = FL("PLAYER " &CurrentPlayer, FormatScore(Score(Currentplayer)))
                tmp1 = CL("ORBITS LEFT " & OrbitsToHit4-OrbitHits4)
            Case 5
                tmp = FL("PLAYER " &CurrentPlayer, FormatScore(Score(Currentplayer)))
                tmp1 = CL("HITS LEFT " & LightsToHit5-LightHits5)
            Case 6
                tmp = FL("PLAYER " &CurrentPlayer, FormatScore(Score(Currentplayer)))
                tmp1 = CL("HITS LEFT " & LightsToHit6-LightHits6)
            Case 7
                tmp = FL("PLAYER " &CurrentPlayer, FormatScore(Score(Currentplayer)))
                tmp1 = CL("TARGET HITS LEFT " & TargetsToHit7-TargetHits7)
            Case 8
                tmp = FL("PLAYER " &CurrentPlayer, FormatScore(Score(Currentplayer)))
                tmp1 = CL("COMPLETE YELLOW BANK")
            Case 9
                tmp = FL("PLAYER " &CurrentPlayer, FormatScore(Score(Currentplayer)))
                tmp1 = CL("COMPLETE GREEN BANK")
            Case 10
                tmp = FL("PLAYER " &CurrentPlayer, FormatScore(Score(Currentplayer)))
                tmp1 = CL("COMPLETE BLUE BANK")
            Case 11
                tmp = FL("PLAYER " &CurrentPlayer, FormatScore(Score(Currentplayer)))
                tmp1 = CL("HIT A RAMP COMBO")
        End Select
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 10, True, ""
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
            dqText(0, dqTail) = ExpandLine(Text0)
        End If

        if(Text1 = "_")Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1)
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
                    Temp = Right(dLine(i), 19)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), 21 - deCount(i), 1)
                    Temp = Temp & Left(dLine(i), 19)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkSlowRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(20)
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkFastRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(20)
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

Function ExpandLine(TempStr) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(20)
    Else
        if Len(TempStr) > Space(20)Then
            TempStr = Left(TempStr, Space(20))
        Else
            if(Len(TempStr) < 20)Then
                TempStr = TempStr & Space(20 - Len(TempStr))
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
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1)) + 128) & right(NumString, Len(NumString)- i)
        end if
    Next
    FormatScore = NumString
End function

Function FL(NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    If Len(NumString1) + Len(NumString2) < 20 Then
        Temp = 20 - Len(NumString1)- Len(NumString2)
        TempStr = NumString1 & Space(Temp) & NumString2
        FL = TempStr
    End If
End Function

Function CL(NumString) 'center line
    Dim Temp, TempStr
    If Len(NumString) > 20 Then NumString = Left(NumString, 20)
    Temp = (20 - Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(NumString) 'right line
    Dim Temp, TempStr
    If Len(NumString) > 20 Then NumString = Left(NumString, 20)
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
    Chars(43) = ""        '+
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
    Chars(96) = ""
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

'********************
' Real Time updates
'********************
'used for all the real time updates

Sub Realtime_Timer
    RollingUpdate
End Sub

' flipper animations

Sub LeftFlipper_Animate:LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle: End Sub
Sub RightFlipper_Animate: RightFlipperTop.RotZ = RightFlipper.CurrentAngle: End Sub
Sub RightFlipper001_Animate: RightFlipperTop001.RotZ = RightFlipper001.CurrentAngle: End Sub
Sub MetalRamp_Flipper_Animate
    MetalRamp.HeightBottom = MetalRamp_Flipper.CurrentAngle
    MetalRamp_Start.HeightBottom = MetalRamp_Flipper.CurrentAngle
    MetalRamp_Start.HeightTop = MetalRamp_Flipper.CurrentAngle + 6
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
' 11 colors: red, orange, amber, yellow...
'******************************************

'colors
Const red = 5
Const orange = 4
Const amber = 6
Const yellow = 3
Const darkgreen = 7
Const green = 2
Const blue = 1
Const darkblue = 8
Const purple = 9
Const white = 11
Const teal = 10

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
        Case white
            n.color = RGB(193, 91, 0)
            n.colorfull = RGB(255, 197, 143)
        Case teal
            n.color = RGB(1, 64, 62)
            n.colorfull = RGB(2, 128, 126)
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
        Case white
            n.color = RGB(255, 197, 143)
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
End Sub

Sub RainbowTimer_Timer 'rainbow led light color changing
    Dim obj
    Select Case RGBStep
        Case 0 'Green
            rGreen = rGreen + RGBFactor
            If rGreen > 255 then
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
            If rBlue > 255 then
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
            If rRed > 255 then
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
    If Score(1)Then
        DMD CL("LAST SCORE"), CL("PLAYER 1 " &FormatScore(Score(1))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2)Then
        DMD CL("LAST SCORE"), CL("PLAYER 2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3)Then
        DMD CL("LAST SCORE"), CL("PLAYER 3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4)Then
        DMD CL("LAST SCORE"), CL("PLAYER 4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    DMD "", CL("GAME OVER"), "", eNone, eBlink, eNone, 2000, False, ""
    If bFreePlay Then
        DMD "", CL("FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits > 0 Then
            DMD CL("CREDITS " & Credits), CL("PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "        JPSALAS", "           AND", "d_jppresents", eNone, eNone, eNone, 3000, False, ""
    DMD " JOE PICASSO", "  PRESENTS", "d_joe", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 4000, False, ""
    DMD "", "", "d_title2", eNone, eNone, eNone, 4000, False, ""
    DMD "", CL("ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("HIGHSCORES"), Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL("HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL("HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    StartLightSeq
    StartRainbow aArrows
    DMDFlush
    ShowTableInfo
    PlaySong "m_gameover"
    DOF 115, DOFon 'MX lights
End Sub

Sub StopAttractMode
    DOF 115, DOFoff 'MX lights
    DOF 159, DOFon  'Undercab Green
    StopRainbow
    DMDScoreNow
    LightSeqAttract.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 40
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

Sub LightSeqTopFlashers_PlayDone()
    FlashEffect 7
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, timers, etc
Sub VPObjects_Init
    CapKicker1.CreateBall
    kickerpin.IsDropped = 1
    For each x in aBallGlow
       x.Visible = 0
    Next
    For each x in aBallGlow2
       x.Visible = 0
    Next
End Sub

' tables variables and Mode init
Dim Mode(4, 14) '4 players, 11 games modes, and 3 wizard modes.
Dim ModesWon(4)
Dim Arrows(10)  'the mode lights
Dim JackpotLight(10)
Dim SPJackpotLight(10)
Dim UpdateArrowsCount
Dim Balloons(4)      'balloons popped
Dim BumperHits(4)    'used for the bonus and to activate super bumpers
Dim OrbitHits(4)
Dim Children(4)      'number of children rescued
Dim Bullies(4)       'number of bullies defeated
Dim TopLanes(4, 3)   'top lights - bonus x
Dim LowerLanes(4, 4) 'lower lane lights - playfield x
Dim GeorgieLights(4, 7)
Dim FearLights(4, 4)
Dim FloatLights(4, 5)
Dim HorrorsLights(4, 7)
Dim BumperValue
Dim bRampIsUp
Dim MultiballType(4)
Dim bLocksEnabled
Dim AddABallHits(4)
Dim bAddABallReady
Dim EndModeCountdown

' Modes variables
Dim SpinCount1
Dim BumperHits2
Dim RampHits3
Dim OrbitHits4
Dim LightHits5
Dim LightHits6
Dim TargetHits7
Dim JackpotHits

Dim SpinsToCount1
Dim BumpersToHit2
Dim RampsToHit3
Dim OrbitsToHit4
Dim LightsToHit5
Dim LightsToHit6
Dim TargetsToHit7

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    'Init Variables
    bExtraBallWonThisBall = False
    BallSaverTime = 20
    ComboCount = 0
    UpdateArrowsCount = 0
    BumperValue = 5000
    bRampIsUp = False
    bLocksEnabled = False
    bAddABallReady = False
    For i = 0 to 10
        Arrows(i) = 0
        JackpotLight(i) = 0
        SPJackpotLight(i) = 0
    Next
    For i = 0 to 4 'init players variables
    ComboHits(i) = 0
    ComboValue(i) = 100000
    BallsInLock(i) = 0
    Jackpot(i) = 100000
    SuperJackpot(i) = 500000
    ModesWon(i) = 0
    Balloons(i) = 0
    BumperHits(i) = 0
    OrbitHits(i) = 0
    Children(i) = 0
    Bullies(i) = 0
    MultiballType(i) = 0
    AddABallHits(i) = 0
    For j = 0 to 14:Mode(i, j) = 0:Next
    For j = 0 to 3:TopLanes(i, j) = 0:Next
    For j = 0 to 5:FloatLights(i, j) = 0:Next
        For j = 0 to 7
            GeorgieLights(i, j) = 0
            HorrorsLights(i, j) = 0
        Next
        For j = 0 to 4
            LowerLanes(i, j) = 0
            FearLights(i, j) = 0
        Next
    Next
    'Mode variables
    SpinsToCount1 = 50
    BumpersToHit2 = 20
    RampsToHit3 = 5
    OrbitsToHit4 = 5
    LightsToHit5 = 5
    LightsToHit6 = 5
    TargetsToHit7 = 8

    TurnOffPlayfieldLights()
    ' play a welcome Sound
    PLaySound "Start" &RndNbr(10)
End Sub

Sub InstantInfo
    Dim tmp
    DMD CL("INSTANT INFO"), "", "", eNone, eNone, eNone, 1000, False, ""
    'Show some info on the current Mode
    Select Case Mode(CurrentPlayer, 0)
        Case 1
            DMD CL("RESCUE EDDIE"), CL("SHOOT THE SPINNER"), "", eNone, eNone, eNone, 1500, True, ""
        Case 2
            DMD CL("RESCUE BEN"), CL("HIT THE POP BUMPERS"), "", eNone, eNone, eNone, 1500, True, ""
        Case 3
            DMD CL("RESCUE RICHIE"), CL("SHOOT THE RAMPS"), "", eNone, eNone, eNone, 1500, True, ""
        Case 4
            DMD CL("RESCUE BEVERLY"), CL("SHOOT THE ORBIT"), "", eNone, eNone, eNone, 1500, True, ""
        Case 5
            DMD CL("RESCUE STANLEY"), CL("SHOOT THE LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
        Case 6
            DMD CL("RESCUE MIKE"), CL("SHOOT THE LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
        Case 7
            DMD CL("RESCUE BILL"), CL("SHOOT THE TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
        Case 8
            DMD CL("RUN FROM PATRICK"), CL("COMPLETE YELLOW BANK"), "", eNone, eBlink, eNone, 1500, True, ""
        Case 9
            DMD CL("RUN FROM BOWERS"), CL("COMPLETE GREEN BANK"), "", eNone, eBlink, eNone, 1500, True, ""
        Case 10
            DMD CL("RUN FROM BELCH"), CL("COMPLETE BLUE BANK"), "", eNone, eBlink, eNone, 1500, True, ""
        Case 11
            DMD CL("RUN FROM VICTOR"), CL("HIT A COMBO"), "", eNone, eNone, eNone, 1500, True, ""
        Case 12
            DMD CL("FIND PENNYWISE"), CL("SHOOT JACKPOTS"), "", eNone, eNone, eNone, 2000, True, ""
        Case 13
            DMD CL("FIGHT PENNYWISE"), CL("SHOOT JACKPOTS"), "", eNone, eNone, eNone, 2000, True, ""
        Case 14
            DMD CL("KILL PENNYWISE"), CL("SHOOT PENNYWISE"), "", eNone, eNone, eNone, 2000, True, ""
    End Select
    DMD CL("YOUR SCORE"), CL(FormatScore(Score(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("PLAYFIELD MULTIPLIER"), CL("X " &PlayfieldMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("BONUS MULTIPLIER"), CL("X " &BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("JACKPOT VALUE"), CL(FormatScore(Jackpot(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SUPER JACKPOT VALUE"), CL(FormatScore(SuperJackpot(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    If Score(1)Then
        DMD CL("PLAYER 1 SCORE"), CL(FormatScore(Score(1))), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(2)Then
        DMD CL("PLAYER 2 SCORE"), CL(FormatScore(Score(2))), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(3)Then
        DMD CL("PLAYER 3 SCORE"), CL(FormatScore(Score(3))), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(4)Then
        DMD CL("PLAYER 4 SCORE"), CL(FormatScore(Score(4))), "", eNone, eNone, eNone, 2000, False, ""
    End If
End Sub

Sub StopMBmodes                                  'stop multiball modes after loosing the last multiball
    If Mode(CurrentPlayer, 0) > 11 Then StopMode 'wizard modes
    LightSeqTopFlashers.StopPlay
    RndJackpot.Enabled = 0
    RndSPJackpot.Enabled = 0
    ResetJackpots
    ResetSPJackpots
    ChangeGi white
    ChangeGIIntensity 1
    GiOn
    Stop_Fog
    BallsInLock(CurrentPlayer) = 0
    MultiballType(CurrentPlayer) = 0
    UpdateMBLights
    RiseTarget
    bMultiBallStarted = False
    aBallWhiteGlowing = False
    ChangeSong
    DOF 155, DOFoff     'Stop Georgie MX
    DOF 156, DOFoff     'Stop Fear MX
    DOF 157, DOFoff     'Stop Float MX
    DOF 158, DOFOff     'Stop Horror MX
    DOF 159, DOFon      'Start Undercab Green Up
End Sub

Sub StopEndOfBallMode() 'this sub is called after the last ball in play is drained, reset skillshot, modes, timers
    StopMode
    ResetJackpots
    ResetSPJackpots
End Sub

Sub ResetNewBallVariables() 'reset variables and lights for a new ball or player
    'turn on or off the needed lights before a new ball is released
    TurnOffPlayfieldLights
    'set up the lights according to the player achievments
    BumperValue = 5000
    BonusMultiplier(CurrentPlayer) = 1
    DecreasePlayfieldMultiplier
    RiseRamp
    RiseTarget
    bAddABallReady = False
    'update user lights
    UpdateModeLights
    UpdateLowerLaneLights
    UpdateTopLaneLights
    UpdateFearLights
    UpdateFloatLights
    UpdateGeorgieLights
    UpdateHorrorsLights
    UpdateMBLights
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
    LightSeqTilt.Play SeqAlloff
    LightSeqSkillshot.PLay SeqBlinking, , 50, 300
    DMD CL("HIT LIT LIGHT"), CL("FOR SKILLSHOT"), "", eNone, eNone, eNone, 3000, True, ""
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    LightSeqSkillshot.StopPlay
    LightSeqIT.StopPlay
    DMDScoreNow
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
' In this table the slingshots change the outlanes lights

Dim LStep, RStep, Lstep2

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Lemk
    DOF 106, DOFPulse 'DOF Solenoid/MX
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
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
    PlaySoundAt SoundFXDOF("fx_slingshot", 104, DOFPulse, DOFcontactors), Remk
    DOF 107, DOFPulse 'DOF Solenoid/MX
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:Remk.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot001_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Lemk001
    DOF 108, DOFPulse 'DOF Solenoid
    LeftSling008.Visible = 1
    Lemk001.RotX = 26
    LStep2 = 0
    LeftSlingShot001.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot001"
End Sub

Sub LeftSlingShot001_Timer
    Select Case LStep2
        Case 1:LeftSLing008.Visible = 0:LeftSLing007.Visible = 1:Lemk001.RotX = 14
        Case 2:LeftSLing007.Visible = 0:LeftSLing008.Visible = 1:Lemk001.RotX = 2
        Case 3:LeftSLing008.Visible = 0:Lemk001.RotX = -20:LeftSlingShot001.TimerEnabled = 0
    End Select
    LStep2 = LStep2 + 1
End Sub
'***********************
'        Bumpers
'***********************

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    PlaySoundAt SoundFXDOF("fx_bumper", 103, DOFPulse, DOFContactors), Bumper1
    AddScore BumperValue
    BumperHits(CurrentPlayer) = BumperHits(CurrentPlayer) + 1
    CheckBumperHits
    ' check for modes
    Select Case Mode(CurrentPlayer, 0)
        Case 2:BumperHits2 = BumperHits2 + 1:CheckWinMode
    End Select
	If B2SOn Then
        Controller.B2SSetData 50, 1
        vpmtimer.AddTimer 1000, " Controller.B2SSetData 50, 0 '"
      End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper1"
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    PlaySoundAt SoundFXDOF("fx_bumper", 104, DOFPulse, DOFContactors), Bumper2
    AddScore BumperValue
    BumperHits(CurrentPlayer) = BumperHits(CurrentPlayer) + 1
    CheckBumperHits
    ' check for modes
    Select Case Mode(CurrentPlayer, 0)
        Case 2:BumperHits2 = BumperHits2 + 1:CheckWinMode
    End Select
	If B2SOn Then
        Controller.B2SSetData 51, 1
        vpmtimer.AddTimer 1000, " Controller.B2SSetData 51, 0 '"
      End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper2"
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    PlaySoundAt SoundFXDOF("fx_bumper", 105, DOFPulse, DOFContactors), Bumper3
    AddScore BumperValue
    BumperHits(CurrentPlayer) = BumperHits(CurrentPlayer) + 1
    CheckBumperHits
    ' check for modes
    Select Case Mode(CurrentPlayer, 0)
        Case 2:BumperHits2 = BumperHits2 + 1:CheckWinMode
    End Select
    ' remember last trigger hit by the ball
	If B2SOn Then
        Controller.B2SSetData 52, 1
        vpmtimer.AddTimer 1000, " Controller.B2SSetData 52, 0 '"
      End If
    LastSwitchHit = "Bumper3"
End Sub

Sub CheckBumperHits                'to enable superbumpers
    If BumperHits(CurrentPlayer)MOD 10 = 0 Then
        If BumperValue = 5000 Then 'turn on the bumper lights
            DMD CL("SUPER BUMPERS"), CL("ARE LIT"), "", eNone, eNone, eNone, 1500, True, ""
            BumperValue = 50000
            Lbumper1.State = 1
            Lbumper2.State = 1
            Lbumper3.State = 1
        End If
    End If
End Sub

'*********
' Lanes
'*********
' in and outlanes
Sub Trigger008_Hit
    If Tilted Then Exit Sub
    Dof 141, Dofpulse 'MX Outlane
    Addscore 5000
    LowerLanes(CurrentPLayer, 1) = 1
    UpdateLowerLaneLights
    CheckPlayfieldX
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger008"
End Sub

Sub Trigger007_Hit
    If Tilted Then Exit Sub
    Dof 142, Dofpulse 'MX Inlane
    If bSkillShotReady Then
        AwardSkillShot 200000
    Else
        Addscore 5000
        LowerLanes(CurrentPLayer, 2) = 1
        UpdateLowerLaneLights
        CheckPlayfieldX
    End If
End Sub

Sub Trigger006_Hit
    If Tilted Then Exit Sub
    Dof 142, Dofpulse 'MX Inlane
    Addscore 5000
    LowerLanes(CurrentPLayer, 3) = 1
    UpdateLowerLaneLights
    CheckPlayfieldX
End Sub

Sub Trigger005_Hit
    If Tilted Then Exit Sub
    Dof 141, Dofpulse 'MX Outlane
    Addscore 5000
    LowerLanes(CurrentPLayer, 4) = 1
    UpdateLowerLaneLights
    CheckPlayfieldX
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger005"
End Sub

Sub CheckPlayfieldX
    Dim i, tmp
    tmp = LowerLanes(CurrentPLayer, 1) + LowerLanes(CurrentPLayer, 2) + LowerLanes(CurrentPLayer, 3) + LowerLanes(CurrentPLayer, 4)
    If tmp = 4 Then
        AddPlayfieldMultiplier 1
        For i = 0 to 4
            LowerLanes(CurrentPLayer, i) = 0
        Next
        UpdateLowerLaneLights
    End If
End Sub

Sub UpdateLowerLaneLights
    li002.State = LowerLanes(CurrentPLayer, 1)
    li037.State = LowerLanes(CurrentPLayer, 2)
    li038.State = LowerLanes(CurrentPLayer, 3)
    li039.State = LowerLanes(CurrentPLayer, 4)
End Sub

'top lanes

Sub Trigger001_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    TopLanes(CurrentPLayer, 1) = 1
    UpdateTopLaneLights
    CheckBonusX
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger001"
End Sub

Sub Trigger002_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    TopLanes(CurrentPLayer, 2) = 1
    UpdateTopLaneLights
    CheckBonusX
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger002"
End Sub

Sub Trigger003_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    TopLanes(CurrentPLayer, 3) = 1
    UpdateTopLaneLights
    CheckBonusX
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger003"
End Sub

Sub CheckBonusX
    Dim i, tmp
    tmp = TopLanes(CurrentPLayer, 1) + TopLanes(CurrentPLayer, 2) + TopLanes(CurrentPLayer, 3)
    If tmp = 3 Then
        AddBonusMultiplier 1
        For i = 0 to 3
            TopLanes(CurrentPLayer, i) = 0
        Next
        UpdateTopLaneLights
    End If
End Sub

Sub UpdateTopLaneLights
    li040.State = TopLanes(CurrentPLayer, 1)
    li041.State = TopLanes(CurrentPLayer, 2)
    li042.State = TopLanes(CurrentPLayer, 3)
End Sub

' diverse lanes

Sub Trigger013_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    ' activate magnet
    If activeBall.VelY > 0 Then ' ball going down
        TopMagnet.MagnetOn = True
        vpmTimer.AddTimer 2000, "TopMagnet.MagnetOn = False '"
        FlashForMs f3a, 1500, 50, 0
        FlashForMs f3b, 1500, 50, 0
        FlashForMs f3c, 1500, 50, 0
    End If
    'check for OrbitHits
    If LastSwitchHit = "Trigger011" Then
        OrbitHits(CurrentPlayer) = OrbitHits(CurrentPlayer) + 1
        FlashEffect 1
        If Mode(CurrentPlayer, 0) = 4 Then
            OrbitHits4 = OrbitHits4 + 1
            CheckWinMode
        End If
    End If
    'check for add-a-ball
    If bAddABallReady AND bMultiBallMode Then 'award add-a-ball but only if during multiball
        DMD "_", CL("ADD-A-BALL"), "", eNone, eBlink, eNone, 1500, True, ""
        bAddABallReady = False
        li059.State = 0
        LightEffect 1
        AddMultiball 1
    End If
    AddABallHits(CurrentPlayer) = (AddABallHits(CurrentPlayer) + 1)MOD 10
    If AddABallHits(CurrentPlayer) = 0 Then 'enable add-a-ball light
        DMD "_", CL("ADD-A-BALL IS LIT"), "", eNone, eBlink, eNone, 1500, True, ""
        bAddABallReady = True
        li059.State = 1
    End If
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 6
            If Arrows(9)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 12, 13, 14
            If JackpotLight(9)OR SPJackpotLight(9)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger013"
End Sub

Sub Trigger009_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    PlaySound "vo_greatshot" &RndNbr(5)
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger009"
End Sub

Sub Trigger011_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    'check for OrbitHits
    If LastSwitchHit = "Trigger013" Then
        OrbitHits(CurrentPlayer) = OrbitHits(CurrentPlayer) + 1
        FlashEffect 1
        If Mode(CurrentPlayer, 0) = 4 Then
            OrbitHits4 = OrbitHits4 + 1
            CheckWinMode
        End If
    End If
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5
            If Arrows(2)Then
                Arrows(2) = 0
                li050.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(2)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 12, 13, 14
            If JackpotLight(2)OR SPJackpotLight(2)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger011"
End Sub

' ramps

Sub Trigger004_Hit    'left Ramp
    If Tilted Then Exit Sub
    DOF 140, DOFPulse 'Strobe flash
    ActiveBall.VelX = 25
    Addscore 5000
    'check for Combo
    If LastSwitchHit = "Trigger004" Then
        AwardCombo
        If Mode(CurrentPlayer, 0) = 11 Then
            CheckWinMode
        End If
    Else
        ComboCount = 0
    End If
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 3:RampHits3 = RampHits3 + 1:CheckWinMode
        Case 5
            If Arrows(2)Then
                Arrows(2) = 0
                li050.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(2)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 12, 13, 14
            If JackpotLight(2)OR SPJackpotLight(2)Then
                CheckWinMode
            End If
    End Select
    ' ensure the lock is disabled during a MultiballType
    If bMultiBallMode Then Tlock.Enabled = 0
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger004"
End Sub

Sub Trigger012_Hit    'right ramp
    If Tilted Then Exit Sub
    DOF 140, DOFPulse 'Strobes
    Addscore 5000
    'check for Combo
    If LastSwitchHit = "Trigger012" Then
        AwardCombo
        If Mode(CurrentPlayer, 0) = 11 Then
            CheckWinMode
        End If
    Else
        ComboCount = 0
    End If
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 3:RampHits3 = RampHits3 + 1:CheckWinMode
        Case 5
            If Arrows(8)Then
                Arrows(8) = 0
                li048.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(8)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 12, 13, 14
            If JackpotLight(8)OR SPJackpotLight(8)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger012"
End Sub

Sub Trigger014_Hit 'IT ramp
    If Tilted Then Exit Sub
    Addscore 5000
    'check for Combo (it may only happen during a multiball in this )
    If LastSwitchHit = "Trigger014" Then
        AwardCombo
        If Mode(CurrentPlayer, 0) = 11 Then
            CheckWinMode
        End If
    Else
        ComboCount = 0
    End If
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 3:RampHits3 = RampHits3 + 1:CheckWinMode
        Case 6
            If Arrows(3)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 12, 13, 14
            If JackpotLight(3)OR SPJackpotLight(3)Then
                CheckWinMode
            End If
    End Select
    ' ensure the lock is disabled during a MultiballType
    If bMultiBallMode Then Clock.Enabled = 0
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger014"
End Sub

Sub Trigger015_Hit 'center ramp
    If Tilted Then Exit Sub
    Addscore 5000
    'check for Combo
    If LastSwitchHit = "Trigger015" Then
        AwardCombo
        If Mode(CurrentPlayer, 0) = 11 Then
            CheckWinMode
        End If
    Else
        ComboCount = 0
    End If
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 3:RampHits3 = RampHits3 + 1:CheckWinMode
        Case 6
            If Arrows(6)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 12, 13, 14
            If JackpotLight(6)OR SPJackpotLight(6)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger015"
End Sub

Sub Trigger016_Hit 'to release balls stuck under the ramp
    Me.TimerEnabled = 1
End Sub

Sub Trigger016_UnHit 'to release balls stuck under the ramp
    Me.TimerEnabled = 0
End Sub

Sub Trigger016_Timer 'to release balls stuck under the ramp
    Me.TimerEnabled = 0
    RiseRamp
End Sub

'***********
' Targets
'***********
' Georgie yellow left targets
Sub Target010_Hit
    If Tilted Then Exit Sub
    DOF 160, DOFpulse 'Georgie Flasher
    Addscore 5000
    PlayElectro
    GeorgieLights(CurrentPlayer, 1) = 1
    CheckGeorgieLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 6
            If Arrows(1)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(1)OR SPJackpotLight(1)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target010"
End Sub

Sub Target011_Hit
    If Tilted Then Exit Sub
    DOF 160, DOFpulse 'Georgie Flasher
    Addscore 5000
    PlayElectro
    GeorgieLights(CurrentPlayer, 2) = 1
    CheckGeorgieLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 6
            If Arrows(1)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(1)OR SPJackpotLight(1)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target011"
End Sub

Sub Target012_Hit
    If Tilted Then Exit Sub
    DOF 160, DOFpulse 'Georgie Flasher
    Addscore 5000
    PlayElectro
    GeorgieLights(CurrentPlayer, 3) = 1
    CheckGeorgieLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 6
            If Arrows(1)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(1)OR SPJackpotLight(1)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target012"
End Sub

Sub Target013_Hit
    If Tilted Then Exit Sub
    DOF 160, DOFpulse 'Georgie Flasher
    Addscore 5000
    PlayElectro
    GeorgieLights(CurrentPlayer, 4) = 1
    CheckGeorgieLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 6
            If Arrows(1)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(1)OR SPJackpotLight(1)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target013"
End Sub

Sub Target014_Hit
    If Tilted Then Exit Sub
    DOF 160, DOFpulse 'Georgie Flasher
    Addscore 5000
    PlayElectro
    GeorgieLights(CurrentPlayer, 5) = 1
    CheckGeorgieLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 6
            If Arrows(1)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(1)OR SPJackpotLight(1)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target014"
End Sub

Sub Target015_Hit
    If Tilted Then Exit Sub
    DOF 160, DOFpulse 'Georgie Flasher
    Addscore 5000
    PlayElectro
    GeorgieLights(CurrentPlayer, 6) = 1
    CheckGeorgieLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 6
            If Arrows(1)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(1)OR SPJackpotLight(1)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target015"
End Sub

Sub Target016_Hit
    If Tilted Then Exit Sub
    DOF 160, DOFpulse 'Georgie Flasher
    Addscore 5000
    PlayElectro
    GeorgieLights(CurrentPlayer, 7) = 1
    CheckGeorgieLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 6
            If Arrows(1)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(1)OR SPJackpotLight(1)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target016"
End Sub

Sub GeorgieTargets_Hit(idx)
    LeftMagnet.MagnetOn = True
    vpmTimer.AddTimer 1000, " LeftMagnet.MagnetOn = False '"
End Sub

Sub CheckGeorgieLights
    'select Georgie MB
    MultiballType(CurrentPlayer) = 1
    UpdateMBLights
    Dim i, tmp
    tmp = 0
    For i = 1 to 7
        tmp = tmp + GeorgieLights(CurrentPlayer, i)
    Next
    If tmp = 7 Then
        For i = 1 to 7
            GeorgieLights(CurrentPlayer, i) = 0
        Next
        GiEffect 3
        AwardJackpot
        ' check modes
        Select Case Mode(CurrentPlayer, 0)
            Case 8:CheckWinMode
        End Select
    End If
    UpdateGeorgieLights
End Sub

Sub UpdateMBLights
    Select Case MultiballType(CurrentPlayer)
        Case 0:f10.State = 0:f9.State = 0:f8.State = 0:f7.State = 0
        Case 1:f10.State = 1:f9.State = 0:f8.State = 0:f7.State = 0
        Case 2:f10.State = 0:f9.State = 1:f8.State = 0:f7.State = 0
        Case 3:f10.State = 0:f9.State = 0:f8.State = 1:f7.State = 0
        Case 4:f10.State = 0:f9.State = 0:f8.State = 0:f7.State = 1
    End Select
End Sub

Sub UpdateGeorgieLights
    li021.State = GeorgieLights(CurrentPlayer, 1)
    li022.State = GeorgieLights(CurrentPlayer, 2)
    li023.State = GeorgieLights(CurrentPlayer, 3)
    li024.State = GeorgieLights(CurrentPlayer, 4)
    li025.State = GeorgieLights(CurrentPlayer, 5)
    li026.State = GeorgieLights(CurrentPlayer, 6)
    li027.State = GeorgieLights(CurrentPlayer, 7)
End Sub

'fear blue right targets

Sub Target018_Hit
    If Tilted Then Exit Sub
    DOF 161, DOFpulse 'Fear Flasher
    Addscore 5000
    PlayElectro
    FearLights(CurrentPlayer, 1) = 1
    CheckFearLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5
            If Arrows(10)Then
                Arrows(10) = 0
                li062.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(10)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(10)OR SPJackpotLight(10)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target018"
End Sub

Sub Target019_Hit
    If Tilted Then Exit Sub
    DOF 161, DOFpulse 'Fear Flasher
    Addscore 5000
    PlayElectro
    FearLights(CurrentPlayer, 2) = 1
    CheckFearLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5
            If Arrows(10)Then
                Arrows(10) = 0
                li062.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(10)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(10)OR SPJackpotLight(10)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target019"
End Sub

Sub Target020_Hit
    If Tilted Then Exit Sub
    DOF 161, DOFpulse 'Fear Flasher
    Addscore 5000
    PlayElectro
    FearLights(CurrentPlayer, 3) = 1
    CheckFearLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5
            If Arrows(10)Then
                Arrows(10) = 0
                li062.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(10)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(10)OR SPJackpotLight(10)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target020"
End Sub

Sub Target021_Hit
    If Tilted Then Exit Sub
    DOF 161, DOFpulse 'Fear Flasher
    Addscore 5000
    PlayElectro
    FearLights(CurrentPlayer, 4) = 1
    CheckFearLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5
            If Arrows(10)Then
                Arrows(10) = 0
                li062.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(10)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(10)OR SPJackpotLight(10)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target021"
End Sub

Sub FearTargets_Hit(idx)
    RightMagnet.MagnetOn = True
    vpmTimer.AddTimer 1000, " RightMagnet.MagnetOn = False '"
End Sub

Sub CheckFearLights
    'select Fear MB
    MultiballType(CurrentPlayer) = 2
    UpdateMBLights
    Dim i, tmp
    tmp = 0
    For i = 1 to 4
        tmp = tmp + FearLights(CurrentPlayer, i)
    Next
    If tmp = 4 Then
        For i = 1 to 4
            FearLights(CurrentPlayer, i) = 0
        Next
        GiEffect 3
        AwardJackpot
        ' check modes
        Select Case Mode(CurrentPlayer, 0)
            Case 10:CheckWinMode
        End Select
    End If
    UpdateFearLights
End Sub

Sub UpdateFearLights
    li028.State = FearLights(CurrentPlayer, 1)
    li029.State = FearLights(CurrentPlayer, 2)
    li030.State = FearLights(CurrentPlayer, 3)
    li031.State = FearLights(CurrentPlayer, 4)
End Sub

' float green center targets

Sub Target002_Hit
    If Tilted Then Exit Sub
    DOF 162, DOFpulse 'Float Flasher
    Addscore 5000
    FloatLights(CurrentPlayer, 1) = 1
    CheckFloatLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5
            If Arrows(7)Then
                Arrows(7) = 0
                li061.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(7)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(7)OR SPJackpotLight(7)Then
                CheckWinMode
            End If
    End Select

    ' remember last trigger hit by the ball
    LastSwitchHit = "Target002"
End Sub

Sub Target003_Hit
    If Tilted Then Exit Sub
    DOF 162, DOFpulse 'Float Flasher
    Addscore 5000
    FloatLights(CurrentPlayer, 2) = 1
    CheckFloatLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5
            If Arrows(7)Then
                Arrows(7) = 0
                li061.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(7)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(7)OR SPJackpotLight(7)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target003"
End Sub

Sub Target004_Hit
    If Tilted Then Exit Sub
    DOF 162, DOFpulse 'Float Flasher
    Addscore 5000
    FloatLights(CurrentPlayer, 3) = 1
    CheckFloatLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5
            If Arrows(7)Then
                Arrows(7) = 0
                li061.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(7)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(7)OR SPJackpotLight(7)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target004"
End Sub

Sub Target005_Hit
    If Tilted Then Exit Sub
    DOF 162, DOFpulse 'Float Flasher
    Addscore 5000
    FloatLights(CurrentPlayer, 4) = 1
    CheckFloatLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5
            If Arrows(7)Then
                Arrows(7) = 0
                li061.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(7)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(7)OR SPJackpotLight(7)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target005"
End Sub

Sub Target006_Hit
    If Tilted Then Exit Sub
    DOF 162, DOFpulse 'Float Flasher
    Addscore 5000
    FloatLights(CurrentPlayer, 5) = 1
    CheckFloatLights
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5
            If Arrows(7)Then
                Arrows(7) = 0
                li061.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(7)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 7
            TargetHits7 = TargetHits7 + 1
            CheckWinMode
        Case 12, 13, 14
            If JackpotLight(7)OR SPJackpotLight(7)Then
                CheckWinMode
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target006"
End Sub

Sub CheckFloatLights
    'select Georgie MB
    MultiballType(CurrentPlayer) = 3
    UpdateMBLights
    Dim i, tmp
    tmp = 0
    For i = 1 to 5
        tmp = tmp + FloatLights(CurrentPlayer, i)
    Next
    If tmp = 5 Then
        For i = 1 to 5
            FloatLights(CurrentPlayer, i) = 0
        Next
        GiEffect 3
        AwardJackpot
        ' check modes
        Select Case Mode(CurrentPlayer, 0)
            Case 9:CheckWinMode
        End Select
    End If
    UpdateFloatLights
End Sub

Sub UpdateFloatLights
    li032.State = FloatLights(CurrentPlayer, 1)
    li033.State = FloatLights(CurrentPlayer, 2)
    li034.State = FloatLights(CurrentPlayer, 3)
    li035.State = FloatLights(CurrentPlayer, 4)
    li036.State = FloatLights(CurrentPlayer, 5)
End Sub

'Newton Ball

Sub CapWall1_Hit 'balloon pop
    If Tilted Then Exit Sub
    PlaySound "sfx_balloon", 0, 1
    balloon.Visible = 0
    Balloons(CurrentPlayer) = Balloons(CurrentPlayer) + 1
    vpmTimer.AddTimer 2000, "balloon.visible = 1 '"
    Addscore 25000
    FlashEffect 3
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 6
            If Arrows(4)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 12, 13, 14
            If JackpotLight(4)OR SPJackpotLight(4)Then
                CheckWinMode
            End If
    End Select
End Sub

' horrors red targets

Sub Target023_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    PlayThunder
    HorrorsLights(CurrentPlayer, 1) = 1
    CheckHorrorsLights
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target023"
End Sub

Sub Target022_Hit
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        AwardSkillShot 750000
    Else
        Addscore 5000
        PlayThunder
        HorrorsLights(CurrentPlayer, 2) = 1
        CheckHorrorsLights
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target022"
End Sub

Sub Target017_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    PlayThunder
    HorrorsLights(CurrentPlayer, 3) = 1
    CheckHorrorsLights
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target017"
End Sub

Sub Target008_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    PlayThunder
    HorrorsLights(CurrentPlayer, 4) = 1
    CheckHorrorsLights
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target008"
End Sub

Sub Target007_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    PlayThunder
    HorrorsLights(CurrentPlayer, 5) = 1
    CheckHorrorsLights
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target007"
End Sub

Sub Target024_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    PlayThunder
    HorrorsLights(CurrentPlayer, 6) = 1
    CheckHorrorsLights
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target024"
End Sub

Sub Target001_Hit
    If Tilted Then Exit Sub
    If bSkillshotReady Then
        AwardSkillShot 80000
    Else
        Addscore 5000
        PlayThunder
        HorrorsLights(CurrentPlayer, 7) = 1
        CheckHorrorsLights
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target001"
End Sub

Sub CheckHorrorsLights
    'select Horrors MB
    MultiballType(CurrentPlayer) = 4
    UpdateMBLights
    Dim i, tmp
    tmp = 0
    For i = 1 to 7
        tmp = tmp + HorrorsLights(CurrentPlayer, i)
    Next
    If tmp = 7 Then
        For i = 1 to 7
            HorrorsLights(CurrentPlayer, i) = 0
        Next
        GiEffect 3
        AwardJackpot
    End If
    UpdateHorrorsLights
End Sub

Sub UpdateHorrorsLights
    li014.State = HorrorsLights(CurrentPlayer, 1)
    li016.State = HorrorsLights(CurrentPlayer, 2)
    li015.State = HorrorsLights(CurrentPlayer, 3)
    li017.State = HorrorsLights(CurrentPlayer, 4)
    li018.State = HorrorsLights(CurrentPlayer, 5)
    li020.State = HorrorsLights(CurrentPlayer, 6)
    li019.State = HorrorsLights(CurrentPlayer, 7)
End Sub

' Droptarget

Sub Target009_Hit
    If Tilted Then Exit Sub
    Addscore 5000
    ' enable center ramp lock
    DMD "_", CL("LOCK IS LIT"), "", eNone, eNone, eNone, 1500, True, "vo_lockislit" &RndNbr(6)
    li060.State = 1
    li064.State = 1
    Tlock.Enabled = 1
    Clock.Enabled = 1
    bLocksEnabled = True
    LowerRamp
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target009"
End Sub

Sub RiseTarget
    PLaySoundAt SoundFX("fx_resetdrop", DOFContactors), Target009
    Target009.IsDropped = 0
    ' disable ramp locks
    li060.State = 0
    Clock.Enabled = 0
    li064.State = 0
    Tlock.Enabled = 0
    bLocksEnabled = False
End Sub

'***********
'  Spinner
'***********

Sub Spinner_Spin
    If Tilted Then Exit Sub
    'Addscore spinnervalue(CurrentPlayer)
    PlaySoundAt "fx_spinner", spinner
    DOF 136, DOFPulse
    Select Case Mode(CurrentPlayer, 0)
        Case 1
            Addscore 3000
            SpinCount1 = SpinCount1 + 1
            CheckWinMode
    End Select
End Sub

'*********
' scoop
'*********
' used to start Modes

Sub Scoopin_Hit
    PlaySoundAt "fx_hole_enter", Scoopin
    Scoopin.Destroyball
    BallsinHole = BallsInHole + 1
    If Tilted Then vpmtimer.addtimer 500, "kickBallOut '":Exit Sub
    If bSkillShotReady Then
        AwardSkillShot 200000
    Else
        Addscore 5000
        ' check modes
        Select Case Mode(CurrentPlayer, 0)
            Case 0 'no mode active so start a Mode
                SelectMode
            Case Else
                'rise or lower ramp
                If bRampIsUp Then
                    LowerRamp
                Else
                    RiseRamp
                End If
        End Select
    End If
    ' Nothing left to do, so kick out the ball
    vpmtimer.addtimer 1500, "kickBallOut '"
End Sub

Sub Tophole_Hit
    PlaySoundAt "fx_hole_enter", Tophole
    Tophole.Destroyball
    BallsinHole = BallsInHole + 1
    If Tilted Then vpmtimer.addtimer 500, "kickBallOut '":Exit Sub
    PlaySound "vo_taunt" &RndNbr(66)
    FlashEffect 8
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    Addscore 5000
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5
            If Arrows(5)Then
                Arrows(5) = 0
                li045.State = 0
                LightHits5 = LightHits5 + 1
                CheckWinMode
            End If
        Case 6
            If Arrows(5)Then
                LightHits6 = LightHits6 + 1
                CheckWinMode
            End If
        Case 12, 13, 14
            If JackpotLight(5)OR SPJackpotLight(5)Then
                CheckWinMode
            End If
        Case Else
            If JackpotLight(5)Then
                AwardJackpot
                If Mode(CurrentPlayer, 0) = 0 Then
                    SuperJackpotTimer_Timer
                End If
            End If
            If SPJackpotLight(5)Then
                AwardSuperJackpot
                If Mode(CurrentPlayer, 0) = 0 Then
                    SuperJackpotTimer_Timer
                End If
            End If
    End Select
    ' Nothing left to do, so kick out the ball
    vpmtimer.addtimer 1500, "kickBallOut '"
End Sub

Sub kickBallOut 'from all the holes
    If BallsinHole > 0 Then
        BallsinHole = BallsInHole - 1
        PlaySoundAt SoundFXDOF("fx_popper", 111, DOFPulse, DOFcontactors), ScoopExit
        DOF 130, DOFPulse
        ScoopExit.CreateSizedBallWithMass BallSize / 2, BallMass
        ScoopExit.kick 160, 22
        LightEffect 5
        FlashForMs f4a, 1500, 50, 0
        FlashForMs f4b, 1500, 50, 0
        vpmtimer.addtimer 1500, "kickBallOut '" 'kick out the rest of the balls, if any
    End If
End Sub

Sub VUK_Hit
    PlaySoundAt "fx_kicker_enter", vuk
    If Tilted Then vpmtimer.addtimer 500, "VukExit '":Exit Sub
    FlashEffect 2
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    Addscore 5000
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 12, 13, 14
            If JackpotLight(6)OR SPJackpotLight(6)Then
                CheckWinMode
            End If
    End Select
    ' award extra ball
    If li044.State Then
        AwardExtraBall
        li044.State = 0
    End If
    ' Nothing to do, so kick out the ball
    vpmtimer.addtimer 1500, "VukExit '"
End Sub

Sub VukExit
    PlaySoundAt SoundFXDOF("fx_kicker", 111, DOFPulse, DOFcontactors), vuk
    DOF 130, DOFPulse
    Vuk.kick 180, 55, 1.4
    kickerpin.IsDropped = 0:kickerpin.TimerEnabled = 1
    LightEffect 5
End Sub

Sub kickerpin_Timer:kickerpin.TimerEnabled = 0:kickerpin.IsDropped = 1:End Sub

'**************
' Metal Ramp
'**************

Sub RiseRamp
    MetalRamp_Flipper.RotateToEnd
    MetalRamp_invisible.Collidable = 0
    LeftGate.Open = True
    RightGate.Open = True
    bRampIsUp = True
    'disable top lock
    li064.State = 0
'Tlock.Enabled = 0
End Sub

Sub LowerRamp
    MetalRamp_Flipper.RotateToStart
    MetalRamp_invisible.Collidable = 1
    LeftGate.Open = False
    RightGate.Open = False
    bRampIsUp = False
    'enable top lock
    If bLocksEnabled Then
        li064.State = 1
        Tlock.Enabled = 1
    Else
        li064.State = 0
        Tlock.Enabled = 0
    End If
End Sub

'********************************
' Boat, Hand & Balloon animation
'********************************

Dim MyPi, BoatStep, BoatDir, BalloonStep, BalloonDir
MyPi = Round(4 * Atn(1), 6) / 90
BoatStep = 0
BalloonStep = 0

Sub BoatTimer_Timer()
    BoatDir = SIN(BoatStep * MyPi)
    BoatStep = (BoatStep + 1)MOD 360
    Boat.Y = Boat.Y + BoatDir * 4
End Sub

Sub BalloonTimer_Timer()
    BalloonDir = SIN(BalloonStep * MyPi)
    BalloonStep = (BalloonStep + 1)MOD 360
    Balloon.Z = Balloon.Z + BalloonDir * 0.9
    hand.roty = hand.roty + BalloonDir * 0.3
    hand.rotx = hand.rotx + BalloonDir * 0.3
End Sub

'********
'  FOG
'********

Dim fr, fh, fopac 'rotation and height

Sub Start_Fog
    fr = 0
    fh = 2
    fopac = 0.005
    fog_flash.visible = 1
    fog_flash.IntensityScale = 0
    fog_flash.Timerenabled = 1
End Sub

Sub fog_flash_Timer
    'rotation
    fr = fr + 0.125
    If fr > 360 Then fr = 0
    fog_flash.RotZ = fr
    'height
    If fh < 52 Then
        fh = fh + 0.125
        fog_flash.height = fh
    End If
    ' opacity
    If fopac > 0 then
        If fog_flash.IntensityScale < 1 then
            fog_flash.IntensityScale = fog_flash.IntensityScale + fopac
        End If
    End If
    If fopac < 0 Then
        fog_flash.IntensityScale = fog_flash.IntensityScale + fopac
        If fog_flash.IntensityScale < 0 Then
            fog_flash.Timerenabled = 0
        End If
    End If
End Sub

Sub Stop_Fog
    fopac = -0.01
End Sub

'********************************
' Modes - Rescue or evasion modes
'********************************
' 7 childs to rescue, 4 evasion modes and 1 wizard mode
' every 4 modes won you get to fight Pennywise, the wizard mode

Sub SelectMode                         'select a children to rescue or to run from, called from the scoop
    Dim i, tmp
    If Mode(CurrentPlayer, 0) = 0 Then 'no mode active so select one
        LightSeqBalloons.Play SeqRandom, 50, , 1000
        ' reset the modes that are not finished so they can be played again
        For i = 1 to 14
            If Mode(CurrentPlayer, i) = 2 Then Mode(CurrentPlayer, i) = 0
        Next
        ' choose a mode at random
        tmp = RndNbr(11)
        do while Mode(CurrentPlayer, tmp) <> 0
            tmp = RndNbr(11)
        loop
        Mode(CurrentPlayer, tmp) = 2
        Mode(CurrentPlayer, 0) = tmp
        UpdateModeLights
        StartMode
    End If
    vpmtimer.addtimer 1500, "kickBallOut '"
End Sub

Sub SelectModeManually(modenr) 'select manually a mod - mostly for debuggingm but also used to start the wizard modes
    Dim i
    LightSeqBalloons.Play SeqRandom, 50, , 1000
    For i = 1 to 14
        If Mode(CurrentPlayer, i) = 2 Then Mode(CurrentPlayer, i) = 0
    Next
    Mode(CurrentPlayer, modenr) = 2
    Mode(CurrentPlayer, 0) = modenr
    UpdateModeLights
    StartMode
End Sub

' Update the lights according to the Mode's state
' 0-off-stopped or not started, 1-on-finished, 2-blinking-started
Sub UpdateModeLights                      'children faces
    li003.State = Mode(CurrentPlayer, 1)  'Eddie
    li008.State = Mode(CurrentPlayer, 2)  'Ben
    li009.State = Mode(CurrentPlayer, 3)  'Richie
    li006.State = Mode(CurrentPlayer, 4)  'Beverly
    li007.State = Mode(CurrentPlayer, 5)  'Stanley
    li005.State = Mode(CurrentPlayer, 6)  'Mike
    li004.State = Mode(CurrentPlayer, 7)  'Bill
    li010.State = Mode(CurrentPlayer, 8)  'Patrick
    li011.State = Mode(CurrentPlayer, 9)  'Bowers
    li012.State = Mode(CurrentPlayer, 10) 'Belch
    li013.State = Mode(CurrentPlayer, 11) 'Victor
End Sub

' Starting a mode means to setup some lights and variables, maybe timers
' Mode lights will always blink during an active mode
Sub StartMode
    ChangeSong
    EnableBallsaver 15       'start a 15 seconds ball saver
    ResetArrows
    UpdateArrows.Enabled = 1 'make sure it is started
    Select Case Mode(CurrentPlayer, 0)
        Case 1               'Eddie
            DMD CL("RESCUE EDDIE"), CL("SHOOT THE SPINNER"), "", eNone, eNone, eNone, 3500, True, "vo_saveeddie"
            DMD CL("RESCUE EDDIE"), CL("SHOOT THE SPINNER"), "", eNone, eNone, eNone, 1500, True, "vo_shootspinner"
            Arrows(9) = 2
            SpinCount1 = 0
        Case 2 'Ben
            DMD CL("RESCUE BEN"), CL("HIT THE POP BUMPERS"), "", eNone, eNone, eNone, 3500, True, "vo_taunt34"
            DMD CL("RESCUE BEN"), CL("HIT THE POP BUMPERS"), "", eNone, eNone, eNone, 1500, True, "vo_shootbumpers" &RndNbr(2)
            RiseRamp
            li058.State = 2
            Lbumper1.State = 2
            Lbumper2.State = 2
            Lbumper3.State = 2
            BumperHits2 = 0
        Case 3 'Richie
            DMD CL("RESCUE RICHIE"), CL("SHOOT THE RAMPS"), "", eNone, eNone, eNone, 2500, True, "vo_taunt23"
            DMD CL("RESCUE RICHIE"), CL("SHOOT THE RAMPS"), "", eNone, eNone, eNone, 1500, True, "vo_shootramps"
            LowerRamp
            Arrows(2) = 2
            Arrows(3) = 2
            Arrows(6) = 2
            Arrows(8) = 2
            RampHits3 = 0
        Case 4 'Beverly
            DMD CL("RESCUE BEVERLY"), CL("SHOOT THE ORBIT"), "", eNone, eNone, eNone, 2500, True, "vo_taunt20"
            DMD CL("RESCUE BEVERLY"), CL("SHOOT THE ORBIT"), "", eNone, eNone, eNone, 1500, True, "vo_shootorbits"
            RiseRamp
            Arrows(2) = 2
            Arrows(9) = 2
            OrbitHits4 = 0
        Case 5 'Stanley
            Select Case RndNbr(2)
                Case 1:DMD CL("RESCUE STANLEY"), CL("SHOOT THE LIGHTS"), "", eNone, eNone, eNone, 2500, True, "vo_savestanley"
                Case 2:DMD CL("RESCUE STANLEY"), CL("SHOOT THE LIGHTS"), "", eNone, eNone, eNone, 8000, True, "vo_savestanley2"
            End Select
            DMD CL("RESCUE STANLEY"), CL("SHOOT THE LIGHTS"), "", eNone, eNone, eNone, 1500, True, "vo_shootlights"
            LowerRamp
            Arrows(2) = 2
            Arrows(5) = 2
            Arrows(7) = 2
            Arrows(8) = 2
            Arrows(10) = 2
            LightHits5 = 0
        Case 6 'Mike - shoot lights one at a time
            DMD CL("RESCUE MIKE"), CL("SHOOT THE LIGHTS"), "", eNone, eNone, eNone, 2000, True, "vo_savemike"
            DMD CL("RESCUE MIKE"), CL("SHOOT THE LIGHTS"), "", eNone, eNone, eNone, 1500, True, "vo_shootlights"
            Arrows(RndNbr(10)) = 2
            LightHits6 = 0
        Case 7 'Bill
            DMD CL("RESCUE BILL"), CL("SHOOT THE TARGETS"), "", eNone, eNone, eNone, 2000, True, "vo_savebill"
            DMD CL("RESCUE BILL"), CL("SHOOT THE TARGETS"), "", eNone, eNone, eNone, 1500, True, "vo_shootlights"
            Arrows(1) = 2
            Arrows(7) = 2
            Arrows(10) = 2
            TargetHits7 = 0
        ' Evasion modes
        Case 8 'Patrick
            DMD CL("RUN FROM PATRICK"), CL("COMPLETE YELLOW BANK"), "", eNone, eNone, eNone, 2500, True, "vo_runpatrick"
            DMD CL("RUN FROM PATRICK"), CL("COMPLETE YELLOW BANK"), "", eNone, eBlink, eNone, 1500, True, "vo_shootyellowtargets"
            Arrows(1) = 2
            EndModeCountdown = 90
            EndModeTimer.Enabled = 1
        Case 9 'Bowers
            DMD CL("RUN FROM BOWERS"), CL("COMPLETE GREEN BANK"), "", eNone, eNone, eNone, 3500, True, "vo_runhenry" &RndNbr(2)
            DMD CL("RUN FROM BOWERS"), CL("COMPLETE GREEN BANK"), "", eNone, eBlink, eNone, 1500, True, "vo_shootgreentargets"
            Arrows(7) = 2
            EndModeCountdown = 90
            EndModeTimer.Enabled = 1
        Case 10 'Belch
            DMD CL("RUN FROM BELCH"), CL("COMPLETE BLUE BANK"), "", eNone, eNone, eNone, 7500, True, "vo_runbelchor" &RndNbr(2)
            DMD CL("RUN FROM BELCH"), CL("COMPLETE BLUE BANK"), "", eNone, eBlink, eNone, 1500, True, "vo_shootbluetargets"
            Arrows(10) = 2
            EndModeCountdown = 90
            EndModeTimer.Enabled = 1
        Case 11 'Victor
            DMD CL("RUN FROM VICTOR"), CL("HIT A COMBO"), "", eNone, eNone, eNone, 3000, True, "vo_runvictor"
            DMD CL("RUN FROM VICTOR"), CL("HIT A COMBO"), "", eNone, eNone, eNone, 1500, True, "vo_shootramps"
            LowerRamp
            Arrows(2) = 2
            Arrows(6) = 2
            Arrows(8) = 2
            EndModeCountdown = 90
            EndModeTimer.Enabled = 1
        'wizard modes
        Case 12 'wizard 1
            DMD CL("FIND PENNYWISE"), CL("SHOOT JACKPOTS"), "", eNone, eNone, eNone, 2000, True, "vo_wizards1"
            AddMultiball 2
            ChangeGI red
            ChangeGIIntensity 2
            RndJackpot_Timer 'turn on a Jackpot
            JackpotHits = 0
            aBallWhiteGlowing = True
        Case 13              'wizard 2
            DMD CL("FIGHT PENNYWISE"), CL("SHOOT JACKPOTS"), "", eNone, eNone, eNone, 2000, True, "vo_wizards2"
            AddMultiball 3
            ChangeGi red
            ChangeGIIntensity 2
            RndJackpot_Timer 'turn on a Jackpot
            JackpotHits = 0
            aBallWhiteGlowing = True
        Case 14              'Wizard 3
            DMD CL("KILL PENNYWISE"), CL("SHOOT PENNYWISE"), "", eNone, eNone, eNone, 2000, True, "vo_wizards3"
            AddMultiball 4
            ChangeGi red
            ChangeGIIntensity 2
            JackpotLight(5) = 2
            JackpotHits = 0
            aBallWhiteGlowing = True
    End Select
End Sub

' check if the Mode is completed
Sub CheckWinMode
    'PlaySound "sfx_thunder" & RndNbr(13)
    DOF 126, DOFPulse
    LightSeqInserts.StopPlay 'stop the light effects before starting again so they don't play too long.
    LightEffect 3
    Select Case Mode(CurrentPlayer, 0)
        Case 1
            If SpinCount1 = SpinsToCount1 Then
                DMD CL("YOU RESCUED EDDIE"), "", "_", eNone, eNone, eNone, 1500, True, "vo_eddiesaved"
                WinMode
                SPJackpotLight(5) = 2
                SuperJackpotTimer.Enabled = 1
            End if
        Case 2
            If BumperHits2 = BumpersToHit2 Then
                DMD CL("YOU RESCUED BEN"), "", "_", eNone, eNone, eNone, 1500, True, "vo_bensaved"
                WinMode
                SPJackpotLight(5) = 2
                SuperJackpotTimer.Enabled = 1
            End if
        Case 3
            If RampHits3 = RampsToHit3 Then
                DMD CL("YOU RESCUED RICHIE"), "", "_", eNone, eNone, eNone, 1500, True, "vo_richiesaved"
                WinMode
                SPJackpotLight(5) = 2
                SuperJackpotTimer.Enabled = 1
            End if
        Case 4
            If OrbitHits4 = OrbitsToHit4 Then
                DMD CL("YOU RESCUED BEVERLY"), "", "_", eNone, eNone, eNone, 1500, True, "vo_beverlysaved"
                WinMode
                SPJackpotLight(5) = 2
                SuperJackpotTimer.Enabled = 1
            End if
        Case 5
            If LightHits5 = LightsToHit5 Then
                DMD CL("YOU RESCUED STANLEY"), "", "_", eNone, eNone, eNone, 1500, True, "vo_stanleysaved"
                WinMode
                SPJackpotLight(5) = 2
                SuperJackpotTimer.Enabled = 1
            End if
        Case 6
            If LightHits6 = LightsToHit6 Then
                DMD CL("YOU RESCUED MIKE"), "", "_", eNone, eNone, eNone, 1500, True, "vo_mikesaved"
                WinMode
                SPJackpotLight(5) = 2
                SuperJackpotTimer.Enabled = 1
            Else
                ResetArrows
                Arrows(RndNbr(10)) = 2
            End if
        Case 7
            If TargetHits7 = TargetsToHit7 Then
                DMD CL("YOU RESCUED BILL"), "", "_", eNone, eNone, eNone, 1500, True, "vo_billsaved"
                WinMode
                SPJackpotLight(5) = 2
                SuperJackpotTimer.Enabled = 1
            End if
        Case 8
            DMD CL("YOU RUN FROM PATRICK"), "", "_", eNone, eNone, eNone, 1500, True, "vo_patrickrun"
            WinMode
            ExtraBallIsLit
        Case 9
            DMD CL("YOU RUN FROM BOWERS"), "", "_", eNone, eNone, eNone, 1500, True, "vo_henryrun"
            WinMode
            ExtraBallIsLit
        Case 10
            DMD CL("YOU RUN FROM BELCH"), "", "_", eNone, eNone, eNone, 1500, True, "vo_belchrun"
            WinMode
            ExtraBallIsLit
        Case 11
            DMD CL("YOU RUN FROM VICTOR"), "", "_", eNone, eNone, eNone, 1500, True, "vo_victorrun"
            WinMode
            ExtraBallIsLit
        Case 12 'wizard modes they are multiballs with jackpots
            Select Case JackpotHits
                Case 1, 2, 3:AwardJackpot:ResetJackpots:RndJackpot_Timer:RndJackpot.Enabled = 1
                Case 4:AwardJackpot:ResetJackpots:RndJackpot.Enabled = 0:SPJackpotLight(5) = 2
                Case Else:AwardSuperJackpot
            End Select
            JackpotHits = JackpotHits + 1
        Case 13 '2nd wizard modes they are multiballs with jackpots
            Select Case JackpotHits
                Case 1, 2, 3, 4:AwardJackpot:ResetJackpots:RndJackpot_Timer:RndJackpot.Enabled = 1
                Case 5:AwardJackpot:ResetJackpots:RndJackpot.Enabled = 0:RndSPJackpot_Timer
                Case Else:AwardSuperJackpot:RndSPJackpot_Timer
            End Select
            JackpotHits = JackpotHits + 1
        Case 14 'last wizard mode
            Select Case JackpotHits
                Case 1, 2, 3, 4, 5:AwardJackpot
                Case 6:AwardJackpot:ResetJackpots:SPJackpotLight(5) = 2
                Case Else:AwardSuperJackpot
            End Select
            JackpotHits = JackpotHits + 1
    End Select
End Sub

Sub StopMode 'called at the end of a ball and also from the stopmode timer during times modes
    Dim i
    Select Case Mode(CurrentPlayer, 0)
        Case 0:PlaySound "vo_drain" &RndNbr(20)
        Case 1:DMD CL("EDDIE IS MINE"), "", "_", eNone, eNone, eNone, 1500, True, "vo_drain" &RndNbr(20)
        Case 2:DMD CL("BEN IS MINE"), "", "_", eNone, eNone, eNone, 1500, True, "vo_bendead"
        Case 3:DMD CL("RICHIE IS MINE"), "", "_", eNone, eNone, eNone, 1500, True, "vo_richiedead"
        Case 4:DMD CL("BEVERLY IS MINE"), "", "_", eNone, eNone, eNone, 1500, True, "vo_beverlydead"
        Case 5:DMD CL("STANLEY IS MINE"), "", "_", eNone, eNone, eNone, 1500, True, "vo_stanleydead"
        Case 6:DMD CL("MIKE IS MINE"), "", "_", eNone, eNone, eNone, 1500, True, "vo_mikedead"
        Case 7:DMD CL("BILL IS MINE"), "", "_", eNone, eNone, eNone, 1500, True, "vo_billdead"
        Case 8:DMD CL("PATRICK GOT YOU"), "", "_", eNone, eNone, eNone, 1500, True, "vo_patrickwon"
        Case 9:DMD CL("HENRY GOT YOU"), "", "_", eNone, eNone, eNone, 1500, True, "vo_henrywon"
        Case 10:DMD CL("BELCH GOT YOU"), "", "_", eNone, eNone, eNone, 1500, True, "vo_benchorwon"
        Case 11:DMD CL("VICTOR GOT YOU"), "", "_", eNone, eNone, eNone, 1500, True, "vo_victorwon"
        Case 12:DMD CL("YOU FOUND"), CL("PENNYWISE"), "_", eBlink, eBlink, eNone, 3000, True, "vo_wizards4"
        Case 13:DMD CL("YOU FOUGHT"), CL("PENNYWISE"), "_", eBlink, eBlink, eNone, 3000, True, "vo_wizards5"
        Case 14:DMD CL("YOU ALMOST KILLED"), CL("PENNYWISE"), "_", eBlink, eBlink, eNone, 2000, True, "vo_wizards6"
                 DMD CL("BUT HE ESCAPED"), CL("HA HA HA"), "_", eBlink, eBlink, eNone, 2000, True, ""
    End Select
    For i = 1 to 14
        If Mode(CurrentPlayer, i) = 2 Then Mode(CurrentPlayer, i) = 0
    Next
    UpdateModeLights
    StopMode2
End Sub

'called after completing a Mode
Sub WinMode
    ModesWon(CurrentPlayer) = ModesWon(CurrentPlayer) + 1
    Mode(CurrentPlayer, Mode(CurrentPlayer, 0)) = 1
    UpdateModeLights
    FlashEffect 2
    LightEffect 2
    GiEffect 3
    DMD "_", CL("MODE COMPLETED"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
    DOF 139, DOFPulse
    StopMode2
    ' check for extra awards
    Select Case ModesWon(CurrentPlayer)
        Case 4:SelectModeManually 12
        Case 8:SelectModeManually 13
        Case 11:SelectModeManually 14
    End Select
    ChangeSong
End Sub

Sub StopMode2 'called after a win or after loosing the ball (from StopMode)
    'Turn off the Arrow lights
    ResetArrows
    ' stop some timers or reset Mode variables or lights
    EndModeTimer.Enabled = 0
    Select Case Mode(CurrentPlayer, 0)
        Case 1:
        Case 2:li058.State = 0:Lbumper1.State = 0:Lbumper2.State = 0:Lbumper3.State = 0
        Case 3:
        Case 4:
        Case 5:
        Case 6:
        Case 7:
        Case 8:
        Case 9:
        Case 10:
        Case 11:
        Case 12, 13:ResetJackpots:ResetSPJackpots
        Case 14:ResetJackpots:ResetSPJackpots:ResetModes
    End Select
    Mode(CurrentPlayer, 0) = 0
End Sub

Sub ResetModes
    Dim i, j
    For j = 0 to 4
        ModesWon(j) = 0
        For i = 1 to 14
            Mode(CurrentPlayer, i) = 0
        Next
    Next
    Mode(CurrentPlayer, 0) = 0
End Sub

Sub EndModeTimer_Timer '1 second timer to count down to end the timed modes
    EndModeCountdown = EndModeCountdown - 1
    If EndModeCountdown < 61 Then
        Select Case EndModeCountdown
            Case 16:DMD "_", CL("TIME IS RUNNING OUT"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
            Case 10:DMD "_", CL("10"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_10"
            Case 9:DMD "_", CL("9"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_9"
            Case 8:DMD "_", CL("8"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_8"
            Case 7:DMD "_", CL("7"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_7"
            Case 6:DMD "_", CL("6"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_6"
            Case 5:DMD "_", CL("5"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_5"
            Case 4:DMD "_", CL("4"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_4"
            Case 3:DMD "_", CL("3"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_3"
            Case 2:DMD "_", CL("2"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_2"
            Case 1:DMD "_", CL("1"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_1"
            Case 0:DMD "_", CL("TIME IS UP"), "_", eNone, eBlinkFast, eNone, 1000, True, "":StopMode
            Case Else:DMD "_", CL(EndModeCountdown), "_", eNone, eNone, eNone, 500, True, ""
        End Select
    End If
End Sub

'********************
' Update Arrow color
'********************

Sub UpdateArrows_Timer 'timer change the color of the blinking lights according to the active Mode
    Select Case UpdateArrowsCount
        Case 0         'Mode Arrows -white
            If Arrows(1)Then SetlightColor li063, white, Arrows(1)
            If Arrows(2)Then SetlightColor li050, white, Arrows(2)
            If Arrows(3)Then SetlightColor li047, white, Arrows(3)
            If Arrows(4)Then SetlightColor li046, white, Arrows(4)
            If Arrows(5)Then SetlightColor li045, white, Arrows(5)
            If Arrows(6)Then SetlightColor li043, white, Arrows(6)
            If Arrows(7)Then SetlightColor li061, white, Arrows(7)
            If Arrows(8)Then SetlightColor li048, white, Arrows(8)
            If Arrows(9)Then SetlightColor li049, white, Arrows(9)
            If Arrows(10)Then SetlightColor li062, white, Arrows(10)
        Case 1 'Jackpots -red
            If JackPotLight(1)Then SetlightColor li063, red, JackPotLight(1)
            If JackPotLight(2)Then SetlightColor li050, red, JackPotLight(2)
            If JackPotLight(3)Then SetlightColor li047, red, JackPotLight(3)
            If JackPotLight(4)Then SetlightColor li046, red, JackPotLight(4)
            If JackPotLight(5)Then SetlightColor li045, red, JackPotLight(5)
            If JackPotLight(6)Then SetlightColor li043, red, JackPotLight(6)
            If JackPotLight(7)Then SetlightColor li061, red, JackPotLight(7)
            If JackPotLight(8)Then SetlightColor li048, red, JackPotLight(8)
            If JackPotLight(9)Then SetlightColor li049, red, JackPotLight(9)
            If JackPotLight(10)Then SetlightColor li062, red, JackPotLight(10)
        Case 2 'SuperJackpots -purple
            If SPJackPotLight(1)Then SetlightColor li063, purple, SPJackPotLight(1)
            If SPJackPotLight(2)Then SetlightColor li050, purple, SPJackPotLight(2)
            If SPJackPotLight(3)Then SetlightColor li047, purple, SPJackPotLight(3)
            If SPJackPotLight(4)Then SetlightColor li046, purple, SPJackPotLight(4)
            If SPJackPotLight(5)Then SetlightColor li045, purple, SPJackPotLight(5)
            If SPJackPotLight(6)Then SetlightColor li043, purple, SPJackPotLight(6)
            If SPJackPotLight(7)Then SetlightColor li061, purple, SPJackPotLight(7)
            If SPJackPotLight(8)Then SetlightColor li048, purple, SPJackPotLight(8)
            If SPJackPotLight(9)Then SetlightColor li049, purple, SPJackPotLight(9)
            If SPJackPotLight(10)Then SetlightColor li062, purple, SPJackPotLight(10)
    End Select
    UpdateArrowsCount = (UpdateArrowsCount + 1)MOD 3
End Sub

Sub ResetArrows
    Dim i
    For i = 1 to 10
        Arrows(i) = 0
    Next
    li063.State = 0
    li050.State = 0
    li047.State = 0
    li046.State = 0
    li045.State = 0
    li043.State = 0
    li061.State = 0
    li048.State = 0
    li049.State = 0
    li062.State = 0
End Sub

Sub RndArrow_Timer
    ResetArrows
    Arrows(RndNbr(10)) = 2
End Sub

'Jackpots

Sub ResetJackpots
    Dim i
    For i = 1 to 10
        JackpotLight(i) = 0
    Next
    li063.State = 0
    li050.State = 0
    li047.State = 0
    li046.State = 0
    li045.State = 0
    li043.State = 0
    li061.State = 0
    li048.State = 0
    li049.State = 0
    li062.State = 0
End Sub

Sub RndJackpot_Timer
    ResetJackpots
    JackpotLight(RndNbr(10)) = 2
End Sub

' SuperJackpots
Sub ResetSPJackpots
    SuperJackpotTimer.Enabled = 0
    Dim i
    For i = 1 to 10
        SPJackpotLight(i) = 0
    Next
    li063.State = 0
    li050.State = 0
    li047.State = 0
    li046.State = 0
    li045.State = 0
    li043.State = 0
    li061.State = 0
    li048.State = 0
    li049.State = 0
    li062.State = 0
End Sub

Sub RndSPJackpot_Timer
    ResetSPJackpots
    SPJackpotLight(RndNbr(10)) = 2
End Sub

'********************
' Multiballs & Locks
'********************

Sub Clock_Hit 'Center Lock
    PlaySoundAt "fx_kicker_enter", clock
    If Tilted Then vpmtimer.addtimer 500, "ClockExit '":Exit Sub
    FlashEffect 2
    Addscore 5000
    If NOT bMultiBallMode Then 'in multiball the kicker should be already disabled
        BallsInLock(CurrentPlayer) = BallsInLock(CurrentPlayer) + 1
        Select Case BallsInLock(CurrentPlayer)
            Case 1
                DMD "_", CL("BALL 1 LOCKED"), "_", eNone, eBlink, eNone, 1500, True, "vo_ball1locked"
                vpmtimer.addtimer 1500, "ClockExit '"
            Case 2
                DMD "_", CL("BALL 2 LOCKED"), "_", eNone, eBlink, eNone, 1500, True, "vo_ball2locked"
                vpmtimer.addtimer 1500, "ClockExit '"
            Case 3
                DMD "_", CL("BALL 3 LOCKED"), "_", eNone, eBlink, eNone, 1500, True, "vo_ball3locked"
                vpmtimer.addtimer 1500, "StartMultiball '"
                vpmtimer.addtimer 2500, "ClockExit '"
        End Select
    Else
        ' Nothing more to do, so kick out the ball
        vpmtimer.addtimer 200, "ClockExit '"
    End If
End Sub

Sub ClockExit
    PlaySoundAt SoundFXDOF("fx_kicker", 111, DOFPulse, DOFcontactors), Clock
    DOF 130, DOFPulse
    Clock.kick 190, 6
    LightEffect 5
End Sub

Sub Tlock_Hit 'Top ramp Lock
    PlaySoundAt "fx_kicker_enter", tlock
    If Tilted Then vpmtimer.addtimer 500, "TlockExit '":Exit Sub
    FlashEffect 2
    Addscore 5000
    If NOT bMultiBallMode Then 'in multiball the kicker should be already disabled
        BallsInLock(CurrentPlayer) = BallsInLock(CurrentPlayer) + 1
        Select Case BallsInLock(CurrentPlayer)
            Case 1
                DMD "_", CL("BALL 1 LOCKED"), "_", eNone, eBlink, eNone, 1500, True, "vo_ball1locked"
                vpmtimer.addtimer 1500, "TlockExit '"
            Case 2
                DMD "_", CL("BALL 2 LOCKED"), "_", eNone, eBlink, eNone, 1500, True, "vo_ball2locked"
                vpmtimer.addtimer 1500, "TlockExit '"
            Case 3
                DMD "_", CL("BALL 3 LOCKED"), "_", eNone, eBlink, eNone, 1500, True, "vo_ball3locked"
                vpmtimer.addtimer 1500, "StartMultiball '"
                vpmtimer.addtimer 2500, "TlockExit '"
        End Select
    Else
        ' Nothing more to do, so kick out the ball
        vpmtimer.addtimer 200, "TlockExit '"
    End If
End Sub

Sub TlockExit
    PlaySoundAt SoundFXDOF("fx_kicker", 111, DOFPulse, DOFcontactors), Tlock
    DOF 130, DOFPulse
    Tlock.kick 90, 12
    LightEffect 5
End Sub

Sub StartMultiball
    EnableBallSaver 20
    FlashEffect 7
    bMultiBallStarted = True
    ChangeSong
    ' disable locks
    bLocksEnabled = False
    li060.State = 0
    Clock.Enabled = 0
    li064.State = 0
    Tlock.Enabled = 0
    Select Case MultiballType(CurrentPlayer)
        Case 0 'normal 3 ball multiball, no jackpots, use it for completing modes
            DMD "_", CL("MULTIBALL"), "_", eNone, eBlink, eNone, 1500, True, "vo_multiball"
            AddMultiball 2
        Case 1 'Georgie - 7 ball - random jackpots
            DMD CL("GEORGIE"), CL("MULTIBALL"), "_", eBlink, eBlink, eNone, 1500, True, ""
            RndJackpot_Timer
            RndJackpot.Enabled = 1
            ChangeGi yellow
            ChangeGIIntensity 2
            AddMultiball 6
            DOF 159, DOFoff 'Turn Off Green Undercab
            DOF 155, DOFOn  'Multiball Lighting
        Case 2              'Fear - 4 ball - jackpots on left ramp, nice for combos, dark mode
            DMD CL("FEAR OF THE DARK"), CL("MULTIBALL"), "_", eBlink, eBlink, eNone, 1500, True, ""
            LowerRamp
            JackPotLight(2) = 2
            NightMode
            AddMultiball 3
            DOF 159, DOFoff 'Turn Off Green Undercab
            DOF 156, DOFOn  'Multiball Lighting
        Case 3              'Float - 5 ball - jackpots on right ramp, nice for combos, green fog
            DMD CL("YOU WILL ALL FLOAT"), CL("MULTIBALL"), "_", eBlink, eBlink, eNone, 1500, True, ""
            JackPotLight(8) = 2
            Start_Fog
            AddMultiball 4
            DOF 159, DOFoff 'Turn Off Green Undercab
            DOF 157, DOFOn  'Multiball Lighting
        Case 4              'Horrors - 7 ball - jackpots on orbits, all scores x balls in play
            DMD CL("HORRORS"), CL("MULTIBALL"), "_", eBlink, eBlink, eNone, 1500, True, ""
            JackPotLight(2) = 2
            JackPotLight(9) = 2
            ChangeGi red
            ChangeGIIntensity 2
            AddMultiball 6
            DOF 159, DOFoff 'Turn Off Green Undercab
            DOF 158, DOFOn  'Multiball Lighting
            RiseRamp
    End Select
End Sub