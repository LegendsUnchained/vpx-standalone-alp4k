' ****************************************************************
'             BOND for VISUAL PINBALL X 10.7
'                 Including VPX7 Physics 4.0.0
'                       Table Version 4.2

'
'    Table by marty02 (C) 2022 (thank you for the permission to mod this fantastic table)
'    Playfield and Plastic Mod by Timblo (C) 2022

' ****************************************************************

'On the DOF website put Exxx
'DOF commands:
'101 Left Flipper
'102 Right Flipper
'103 left slingshot
'104 right slingshot
'105
'106
'107
'108 RIGHT Bumper
'109
'110
'111 Scoop
'118
'119
'120 AutoFire
'122 knocker
'123 ballrelease
'204 Lamp posts (teenagers)
'205 Blue targets
'206 Ramps, lanes and spinners
'207 In and outlanes
'208 Plunger

Option Explicit
Randomize

'♥♡♥♡♥♡♥♡♥♡♥♡♥♡♥ PLayer choices ♥♡♥♡♥♡♥♡♥♡♥♡♥♡♥

Const SongVolume = 0.3     ' 1 is full volume, but I set it quite low to listen better the other sounds since I use headphones, adjust to your setup :)

'Const FlippersBlood = False

Const FlexDMDHighQuality = True 'FlexDMD in high quality (True = LCD at 256x64) or normal quality (False = Real DMD at 128x32)

'♥♡♥♡♥♡♥♡♥♡♥♡♥ End of pLayer choices ♥♡♥♡♥♡♥♡♥♡♥♡♥


' Load the core.vbs for supporting Subs and functions
Const BallSize = 50        ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1        ' standard ball mass

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
Const cGameName = "bond"
Const myVersion = "4.00"
Const MaxPlayers = 4          ' from 1 to 4
Const BallSaverTime = 25     ' in seconds of the first ball
Const MaxMultiplier = 5      ' limit playfield multiplier
Const MaxBonusMultiplier = 5 'limit Bonus multiplier
Const BallsPerGame = 5       ' usually 3 or 5
Const MaxMultiballs = 13     ' max number of balls during multiballs

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
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJackpot

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs
Dim mMagnet
Dim cbLeft    'captive ball at the magnet


sub K5_hit()
k5.destroyball
k6.createball
k6.kick 190,2
end Sub

Sub toiton_hit()
toitouvert.visible = 1
siegeaston.visible = 1
PlaySound "ejecttoit"
End Sub

Sub toitoff_hit()
toitouvert.visible = 0
siegeaston.visible = 0
End Sub

sub fusee1_hit()
    Flashforms fire, 2000, 50, 0
End Sub

Sub offmission_hit()
bondmission.visible = 0
End Sub


Sub jet_hit()
bondjetpack1.visible = 0
bondjetpack.visible = 1
PlaySound "jetpack"
torch3.visible = 1
torch4.visible = 1
End Sub

Sub jetoff_hit()
bondjetpack1.visible = 1
bondjetpack.visible = 0
torch3.visible = 0
torch4.visible = 0
End Sub


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

    ' Magnet
    Set mMagnet = New cvpmMagnet
    With mMagnet
        .InitMagnet Magnet, 35
        .GrabCenter = True
        .CreateEvents "mMagnet"
    End With

    Set cbLeft = New cvpmCaptiveBall
    With cbLeft
        .InitCaptive CapTrigger, MagnetPost, CapKicker, 0
        .ForceTrans = .7
        .MinForce = 3.5
        .CreateEvents "cbLeft"
        .Start
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Credits = 0
    Loadhs

    ' Initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = True 'we want coins

    if bFreePlay or Credits > 1 Then DOF 125, DOFOn

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
    bJackpot = False
    bInstantInfo = False
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

    If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound SoundFX("fx_nudge",0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 8:PlaySound SoundFX("fx_nudge",0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound SoundFX("fx_nudge",0), 0, 1, 1, 0.25

    If keycode = LeftMagnaSave Then bLutActive = True: Lutbox.text = "level of darkness " & LUTImage + 1
    If keycode = RightMagnaSave Then
        If bLutActive Then
            NextLUT
        End If
    End If

    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        if bFreePlay = False Then DOF 125, DOFOn
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

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then CheckTilt 'only check the tilt during game
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 1
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 0

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                Else
                    If(Credits > 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                        If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, "vo_givemeyourmoney"
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
                    If(Credits > 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, "vo_givemeyourmoney"
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = LeftMagnaSave Then bLutActive = False: LutBox.text = ""

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
    End If

    If hsbModeActive Then
        Exit Sub
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
    If UseFlexDMD Then FlexDMD.Run = False
    If B2SOn = true Then Controller.Stop
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled AND bFlippersEnabled Then
        PlaySoundAt SoundFXDOF("fx_flipperupG", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.65:LeftFlipper.RotateToEnd
        LeftFlipper001.EOSTorque = 0.65:LeftFlipper001.RotateToEnd
    Else
            PlaySoundAt SoundFXDOF("fx_flipperdownG", 101, DOFOff, DOFFlippers), LeftFlipper
            LeftFlipper.EOSTorque = 0.15:LeftFlipper.RotateToStart
            LeftFlipper001.EOSTorque = 0.15:LeftFlipper001.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled AND bFlippersEnabled Then
        PlaySoundAt SoundFXDOF("fx_flipperupD", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.65:RightFlipper.RotateToEnd
    Else
            PlaySoundAt SoundFXDOF("fx_flipperdownD", 102, DOFOff, DOFFlippers), RightFlipper
            RightFlipper.EOSTorque = 0.15:RightFlipper.RotateToStart
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper001_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Dim RSplat, LSplat

Sub RightSplat
    RSplat = 0
    Rightblood_Timer
End Sub

Sub Rightblood_Timer
    Select Case RSplat
        Case 0:Rightblood.ImageA = "blood1":Rightblood.Visible = 1:Rightblood.TimerEnabled = 1
        Case 1:Rightblood.ImageA = "blood2"
        Case 2:Rightblood.ImageA = "blood3"
        Case 3:Rightblood.ImageA = "blood4"
        Case 4:Rightblood.ImageA = "blood5"
        Case 5:Rightblood.ImageA = "blood6"
        Case 6:Rightblood.Visible = 0:Rightblood.TimerEnabled = 0
    End Select
    RSplat = RSplat + 1
End Sub

Sub LeftSplat
    LSplat = 0
    Leftblood_Timer
End Sub

Sub Leftblood_Timer
    Select Case LSplat
        Case 0:Leftblood.ImageA = "blood1a":Leftblood.Visible = 1:Leftblood.TimerEnabled = 1
        Case 1:Leftblood.ImageA = "blood2a"
        Case 2:Leftblood.ImageA = "blood3a"
        Case 3:Leftblood.ImageA = "blood4a"
        Case 4:Leftblood.ImageA = "blood5a"
        Case 5:Leftblood.ImageA = "blood6a"
        Case 6:Leftblood.Visible = 0:Leftblood.TimerEnabled = 0
    End Select
    LSplat = LSplat + 1
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                       'Called when table is nudged
    Dim BOT
    BOT = GetBalls
    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub
    Tilt = Tilt + TiltSensitivity                   'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt > TiltSensitivity) AND(Tilt <= 15) Then 'show a warning
        DMD "_", CL("CAREFUL"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
    End if
    If(NOT Tilted) AND Tilt > 15 Then 'If more that 15 then TILT the table
        'display Tilt
        InstantInfoTimer.Enabled = False
        DMDFlush
        DMD CL("YOU"), CL("TILTED"), "", eNone, eNone, eNone, 200, False, ""
        'PlaySound "vo_yousuck" &RndNbr(5)
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
        LeftFlipper001.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        Tilted = False
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
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
    If bJasonMBStarted Then
        PlaySong "mu_multiball1"
    ElseIf bFreddyMBStarted Then
        PlaySong "mu_multiball3"
    ElseIf bMichaelMBStarted Then
        PlaySong "mu_multiball2"
    ElseIf Mode(CurrentPLayer, 0) Then
        PlaySong "mu_pursuit"
    Else
        PlaySong "mu_main" &Balls
    End If
End Sub

Sub StopSong(name)
    StopSound name
End Sub

'********************
' Play random quotes
'********************

Sub PlayQuote 'Jason's mom
    PlaySound "vo_mother" &RndNbr(45)
End Sub

Sub PlayHighScoreQuote
    Select Case RndNbr(3)
        Case 1:PlaySound "vo_awesomescore"
        Case 2:PlaySound "vo_excellentscore"
        Case 3:PlaySound "vo_greatscore"
    End Select
End Sub

Sub PlayNotGoodScore
    Select Case RndNbr(4)
        Case 1:PlaySound "vo_heywhathappened"
        Case 2:PlaySound "vo_didyouscoreanypoints"
        Case 3:PlaySound "vo_youneedflipperskills"
        Case 4:PlaySound "vo_youmissedeverything"
        Case 5:PlaySound "vo_thatwasprettybad"
        Case 6:PlaySound "vo_thatwasprettybad2"
    End Select
End Sub

Sub PlayEndQuote
    Select Case RndNbr(9)
        Case 1:PlaySound "vo_hahaha1"
        Case 2:PlaySound "vo_hahaha2"
        Case 3:PlaySound "vo_hahaha3"
        Case 4:PlaySound "vo_hahaha4"
        Case 5:PlaySound "vo_hastalaviatababy"
        Case 6:PlaySound "vo_hastalaviatababy2"
        Case 7:PlaySound "vo_Illbeback"
        Case 8:PlaySound "vo_seeyoulater"
        Case 9:PlaySound "vo_youredonebyebye"
    End Select
End Sub

Sub PlayThunder
    PlaySound "sfx_thunder" &RndNbr(7)
End Sub

Sub PlaySword
    PlaySound "sfx_sword" &RndNbr(5)
End Sub

Sub PlayKill
    PlaySound "sfx_kill" &RndNbr(10)
End Sub

Sub PlayElectro
    PlaySound "sfx_electro" &RndNbr(9)
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

Dim GiIntensity
GiIntensity = 1   'used for the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 0 Then '-1 means no balls, 0 is the first captive ball, 1 is the second captive ball...)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
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
            LightSeqGi.UpdateInterval = 40
            LightSeqGi.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqGi.UpdateInterval = 25
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 20
            LightSeqGi.Play SeqBlinking, , 10, 20
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
            LightSeqInserts.UpdateInterval = 40
            LightSeqInserts.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqInserts.UpdateInterval = 25
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 20
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 4 'center - used in the bonus count
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqCircleOutOn, 15, 2
        Case 5 'top down
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 15, 2
        Case 6 'down to top
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 15, 1
        Case 7 'center from the magnet
            LightSeqMG.UpdateInterval = 4
            LightSeqMG.Play SeqCircleOutOn, 15, 1
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
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqCircleOutOn, 15, 2
        Case 5 'top down
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqDownOn, 15, 1
        Case 6 'down to top
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqUpOn, 15, 1
        Case 7 'top flashers left right
            LightSeqTopFlashers.UpdateInterval = 10
            LightSeqTopFlashers.Play SeqRightOn, 50, 10
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
Const lob = 1     'number of locked balls
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

'extra collections in this table
Sub aBlueRubbers_Hit(idx)
    Select Case RndNbr(15)
        Case 1:Playsound "vo_hahaha1"
        Case 2:Playsound "vo_hahaha2"
        Case 3:Playsound "vo_hahaha3"
        Case 4:Playsound "vo_toobusytoaim"
        Case 5:Playsound "vo_youmissedeverything"
        Case 6:Playsound "vo_yousuck1"
        Case 7:Playsound "vo_yousuck2"
    End Select
End Sub

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
    DMDScoreNow

    ' set the current players bonus multiplier back down to 1X
    SetBonusMultiplier 1

    ' reduce the playfield multiplier
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

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 10 'yes 10 points :)
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.
    '(the tilt recovery mechanism will handle any extra balls or end of game)

    If NOT Tilted Then
        StopSong Song
        PlaySound "sfx_suspense"
        'Count the bonus. This table uses several bonus
        DMD CL("BONUS"), "", "", eNone, eNone, eNone, 1000, True, ""

        'Weapons collected X 1.500.000
        AwardPoints = Weapons(CurrentPlayer) * 1500000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("WEAPONS COLLECTED " & Weapons(CurrentPlayer) ), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, "mu_bonus"

        'Counselors killed X 750.000
        AwardPoints = CounselorsKilled(CurrentPlayer) * 750000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("MOVIE YOU SAW"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, "mu_bonus"

        'Teenagers killed x 300.000
        AwardPoints = TeensKilled(CurrentPlayer) * 300000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("BAD BOYS KILLED"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, "mu_bonus"

        'Loops X 150.000
        AwardPoints = LoopHits(CurrentPlayer) * 150000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("LOOP COMBOS"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, "mu_bonus"

        'Combos X 150.000
        AwardPoints = ComboHits(CurrentPlayer) * 150000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("RAMP COMBOS"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, "mu_bonus"

        'Bumpers X 50.000
        AwardPoints = BumperHits(CurrentPlayer) * 50000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("BUMPER HITS"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, "mu_bonus"

        If TotalBonus > 5000000 Then
            DMD CL("TOTAL BONUS X MULT"), CL(FormatScore(TotalBonus * BonusMultiplier(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, True, "vo_heynicebonus"
        Else
            DMD CL("TOTAL BONUS X MULT"), CL(FormatScore(TotalBonus * BonusMultiplier(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, True, "vo_notbad"
        End If
        AddScore2 TotalBonus * BonusMultiplier(CurrentPlayer)

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 9000, "EndOfBall2 '"
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
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's then turn off any Extra Ball light if there was any
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            LightShootAgain.State = 0
            LightShootAgain2.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, "vo_replay"

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
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
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame > 1) Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer > PlayersPlayingGame) Then
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
    PlaySound "mu_death"
    vpmtimer.AddTimer 2500, "PlayEndQuote '"
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
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then

            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            ' stop the ballsaver timer during the launch ball saver time, but not during multiballs
            If NOT bMultiBallMode Then
                DMD "_", CL("BALL SAVED"), "_", eNone, eBlinkfast, eNone, 2500, True, "vo_giveballback"
                BallSaverTimerExpired_Timer
            End If
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1) Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True) then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    changesong
                    ' turn off any multiball specific lights
                    ChangeGi white
                    ChangeGIIntensity 1
                    'stop any multiball modes of this game
                    StopMBmodes
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
                ' End Mode and timers
                StopSong Song
                ChangeGi white
                ChangeGIIntensity 1
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
    DOF 208, DOFOn
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    'LaunchLight.State = 2
    ' be sure to update the Scoreboard after the animations, if any
    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        vpmtimer.addtimer 1500, "PlungerIM.AutoFire:DOF 120, DOFPulse:DOF 124, DOFPulse:PlaySoundAt SoundFX(""fx_kicker"",DOFContactors), swPlungerRest:bAutoPlunger = False '"
    End If
    'Start the skillshot lights & variables if any
    If bSkillShotReady Then
        PlaySong "mu_wait"
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
    bBallInPlungerLane = False
    DOF 208, DOFOff
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
        ChangeSong
        ResetSkillShotTimer.Enabled = 1
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
    End If
' turn off LaunchLight
' LaunchLight.State = 0
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds
Sub swPlungerRest_Timer
    IF bOnTheFirstBall Then
        Select Case RndNbr(5)
            Case 1:DMD CL("BACK FOR SOME"), CL("MORE TORTURE"), "_", eNone, eNone, eNone, 2000, True, "vo_backforsomemoretorture"
            Case 2:DMD CL("I KNEW YOU"), CL("YOU WOULD BE BACK"), "_", eNone, eNone, eNone, 2000, True, "vo_Iknewyoullbeback"
            Case 3:DMD CL("ARE YOU PLAYING"), CL("THIS GAME"), "_", eNone, eNone, eNone, 2000, True, "vo_areyouplayingthisgame"
            Case 4:DMD CL("HEY YOU"), CL("SHOOT THE BALL"), "_", eNone, eNone, eNone, 2000, True, "vo_shoothereandhere"
            Case 5:DMD CL("HEY YOU"), CL("WELCOME BACK"), "_", eNone, eNone, eNone, 2000, True, "vo_welcomeback"
        End Select
    Else
        Select Case RndNbr(4)
            Case 1:DMD CL("TIME TO"), CL("WAKE UP"), "_", eNone, eNone, eNone, 2000, True, "vo_timetowakeup"
            Case 2:DMD CL("WHAT ARE"), CL("YOU WAITING FOR"), "_", eNone, eNone, eNone, 2000, True, "vo_whatareyouwaitingfor"
            Case 3:DMD CL("HEY"), CL("PULL THE PLUNGER"), "_", eNone, eNone, eNone, 2000, True, "vo_heypulltheplunger"
            Case 4:DMD CL("ARE YOU PLAYING"), CL("THIS GAME"), "_", eNone, eNone, eNone, 2000, True, "vo_areyouplayingthisgame"
        End Select
    End If
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
    LightShootAgain2.BlinkInterval = 160
    LightShootAgain2.State = 2
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
    LightShootAgain2.State = 0
    ' if the table uses the same lights for the extra ball or replay then turn them on if needed
    If ExtraBallsAwards(CurrentPlayer) > 0 Then
        LightShootAgain.State = 1
        LightShootAgain2.State = 1
    End If
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
    LightShootAgain2.BlinkInterval = 80
    LightShootAgain2.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board

Sub AddScore(points) 'normal score routine
    If Tilted Then Exit Sub
    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
' you may wish to check to see if the player has gotten a replay
End Sub

Sub AddScore2(points) 'used in jackpots, skillshots, combos, and bonus as it does not use the PlayfieldMultiplier
    If Tilted Then Exit Sub
    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points
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

    ' If(bMultiBallMode = True) Then
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
    DMD "_", CL("INCREASED JACKPOT"), "_", eNone, eNone, eNone, 1000, True, ""
' you may wish to limit the jackpot to a upper limit, ie..
'	If (Jackpot >= 6000000) Then
'		Jackpot = 6000000
' 	End if
'End if
End Sub

Sub AddSuperJackpot(points) 'not used in this table
    If Tilted Then Exit Sub
End Sub

Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxBonusMultiplier) then
        ' then add and set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
        SetBonusMultiplier(NewBonusLevel)
        DMD "_", CL("BONUS X " &NewBonusLevel), "_", eNone, eBlink, eNone, 2000, True, ""
    Else
        AddScore2 500000
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
        Case 1:li021.State = 0:li022.State = 0:li023.State = 0:li024.State = 0
        Case 2:li021.State = 1:li022.State = 0:li023.State = 0:li024.State = 0
        Case 3:li021.State = 1:li022.State = 1:li023.State = 0:li024.State = 0
        Case 4:li021.State = 1:li022.State = 1:li023.State = 1:li024.State = 0
        Case 5:li021.State = 1:li022.State = 1:li023.State = 1:li024.State = 1
    End Select
End Sub

Sub AddPlayfieldMultiplier(n)
    Dim snd
    Dim NewPFLevel
    ' if not at the maximum level x
    if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier) then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
        SetPlayfieldMultiplier(NewPFLevel)
        PlayThunder
        DMD "_", CL("PLAYFIELD X " &NewPFLevel), "_", eNone, eBlink, eNone, 2000, True, snd
        LightEffect 4
        ' Play a voice sound
        Select Case NewPFLevel
            Case 2:PlaySound "vo_2xplayfield"
            Case 3:PlaySound "vo_3xplayfield"
            Case 4:PlaySound "vo_4xplayfield"
            Case 5:PlaySound "vo_5xplayfield"
        End Select
    Else 'if the max is already lit
        AddScore2 500000
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
    if(PlayfieldMultiplier(CurrentPlayer) > 1) then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) - 1
        SetPlayfieldMultiplier(NewPFLevel)
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

Sub UpdatePFXLights(Level) '4 lights in this table, from 2x to 5x
    ' Update the playfield multiplier lights
    Select Case Level
        Case 1:li025.State = 0:li026.State = 0:li027.State = 0:li027.State = 0
        Case 2:li025.State = 1:li026.State = 0:li027.State = 0:li027.State = 0
        Case 3:li025.State = 0:li026.State = 1:li027.State = 0:li027.State = 0
        Case 4:li025.State = 0:li026.State = 0:li027.State = 1:li027.State = 0
        Case 5:li025.State = 0:li026.State = 0:li027.State = 0:li027.State = 1
    End Select
' perhaps show also the multiplier in the DMD?
End Sub

Sub AwardExtraBall()
    '   If NOT bExtraBallWonThisBall Then 'in this table you can win several extra balls
    DMD "_", CL("EXTRA BALL WON"), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    DOF 124, DOFPulse
    PLaySound "vo_extraball"
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
    'bExtraBallWonThisBall = True
    LightShootAgain.State = 1  'light the shoot again lamp
    LightShootAgain2.State = 1 'light the shoot again lamp
    GiEffect 2
    LightEffect 2
'    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL("EXTRA GAME WON"), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    DOF 124, DOFPulse
    Credits = Credits + 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardJackpot() 'only used for the final mode
    DMD CL("JACKPOT"), CL(FormatScore(Jackpot(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1500, True, "vo_Jackpot" &RndNbr(6)
    DOF 126, DOFPulse
    AddScore2 Jackpot(CurrentPlayer)
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + 100000
    LightEffect 2
    GiEffect 2
    FlashEffect 2
End Sub

Sub AwardTargetJackpot() 'award a target jackpot after hitting all targets
    DMD CL("TARGET JACKPOT"), CL(FormatScore(TargetJackpot(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1500, True, "vo_Jackpot" &RndNbr(6)
    DOF 126, DOFPulse
    AddScore2 TargetJackpot(CurrentPlayer)
    TargetJackpot(CurrentPlayer) = TargetJackpot(CurrentPlayer) + 150000
    li069.State = 0
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardSuperJackpot() 'not used in this table as there are several superjackpots but I keep it as a reference
    DMD CL("SUPER JACKPOT"), CL(FormatScore(SuperJackpot(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_superjackpot"
    DOF 126, DOFPulse
    AddScore2 SuperJackpot(CurrentPlayer)
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardWeaponsSuperJackpot()
    DMD CL("WEAPONS SUPR JACKPOT"), CL(FormatScore(WeaponSJValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_superjackpot"
    DOF 126, DOFPulse
    AddScore2 WeaponSJValue(CurrentPlayer)
    WeaponSJValue(CurrentPlayer) = WeaponSJValue(CurrentPlayer) + ((Score(CurrentPlayer) * 0.2) \ 10) * 10 'increase the weapons score with 20%
    aWeaponSJactive = False
    li060.State = 0
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL("SKILLSHOT"), CL(FormatScore(SkillshotValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1000, False, "sfx_scare"
    DMD CL("SKILLSHOT"), CL(FormatScore(SkillshotValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1000, True, "vo_greatshot"
    DOF 127, DOFPulse
    Addscore2 SkillShotValue(CurrentPlayer)
    ' increment the skillshot value with 100.000
    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 100000
    'do some light show
    GiEffect 2
    LightEffect 2
End Sub

Sub AwardSuperSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL("SUPER SKILLSHOT"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1000, False, "sfx_scare"
    DMD CL("SUPER SKILLSHOT"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1000, True, "vo_excellentshot"
    DOF 127, DOFPulse
    Addscore2 SuperSkillshotValue(CurrentPlayer)
    ' increment the superskillshot value with 1.000.000
    SuperSkillshotValue(CurrentPlayer) = SuperSkillshotValue(CurrentPlayer) + 1000000
    'do some light show
    GiEffect 2
    LightEffect 2
End Sub

Sub aSkillshotTargets_Hit(idx) 'stop the skillshot if any other target is hit
    If bSkillshotReady then ResetSkillShotTimer_Timer
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

    If tmp > HighScore(0) Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
        DOF 125, DOFOn
    End If

    If tmp > HighScore(3) Then
        PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        HighScore(3) = tmp
        PlayHighScoreQuote
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
        PlayNotGoodScore
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
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters) ) then
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
            if(hsCurrentDigit > 0) then
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
    if(hsCurrentDigit > 0) then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit > 1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit > 2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

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

'***************************
'   LUT - Darkness control 
'***************************

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

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 15:UpdateLUT:SaveLUT:Lutbox.text = "level of darkness " & LUTImage + 1:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0":GiIntensity = 1:ChangeGIIntensity 1
        Case 1:table1.ColorGradeImage = "LUT1":GiIntensity = 1.05:ChangeGIIntensity 1
        Case 2:table1.ColorGradeImage = "LUT2":GiIntensity = 1.1:ChangeGIIntensity 1
        Case 3:table1.ColorGradeImage = "LUT3":GiIntensity = 1.15:ChangeGIIntensity 1
        Case 4:table1.ColorGradeImage = "LUT4":GiIntensity = 1.2:ChangeGIIntensity 1
        Case 5:table1.ColorGradeImage = "LUT5":GiIntensity = 1.25:ChangeGIIntensity 1
        Case 6:table1.ColorGradeImage = "LUT6":GiIntensity = 1.3:ChangeGIIntensity 1
        Case 7:table1.ColorGradeImage = "LUT7":GiIntensity = 1.35:ChangeGIIntensity 1
        Case 8:table1.ColorGradeImage = "LUT8":GiIntensity = 1.4:ChangeGIIntensity 1
        Case 9:table1.ColorGradeImage = "LUT9":GiIntensity = 1.45:ChangeGIIntensity 1
        Case 10:table1.ColorGradeImage = "LUT10":GiIntensity = 1.5:ChangeGIIntensity 1
        Case 11:table1.ColorGradeImage = "LUT11":GiIntensity = 1.55:ChangeGIIntensity 1
        Case 12:table1.ColorGradeImage = "LUT12":GiIntensity = 1.6:ChangeGIIntensity 1
        Case 13:table1.ColorGradeImage = "LUT13":GiIntensity = 1.65:ChangeGIIntensity 1
        Case 14:table1.ColorGradeImage = "LUT14":GiIntensity = 1.7:ChangeGIIntensity 1
    End Select
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
    if(dqHead = dqTail) Then
        ' default when no modes are active
        tmp = RL(FormatScore(Score(Currentplayer) ) )
        tmp1 = FL("PLAYER " &CurrentPlayer, "BALL " & Balls)
        'back image
        If bJasonMBStarted Then
            tmp2 = "d_jason"
        ElseIf bFreddyMBStarted Then
            tmp2 = "d_freddy"
        ElseIf bMichaelMBStarted Then
            tmp2 = "d_michael"
        Else
            tmp2 = "d_border"
        End If
        'info on the second line
        Select Case Mode(CurrentPlayer, 0)
            Case 0: 'no Mode active
                If bTommyStarted Then
                    tmp1 = "SHOOT THE RIGHT RAMP"
                ElseIf bPoliceStarted Then
                    tmp1 = CL("HIT THE MI 6")
                End If
            Case 1: 'spinners
                If Not ReadyToKill Then
                    tmp1 = FL("SPINNERS CENTER", SpinNeeded-SpinCount)
                Else
                    tmp1 = CL("SHOOT THE ASTON")
                End If
            Case 2:
                If Not ReadyToKill Then
                    tmp1 = FL("HITS LEFT", 4-TargetModeHits)
                Else
                    tmp1 = CL("SHOOT THE MAGNET")
                End If
            Case 3:
                If Not ReadyToKill Then
                    tmp1 = FL("HITS LEFT", 5-TargetModeHits)
                Else
                    tmp1 = CL("SHOOT THE ASTON")
                End If
            Case 4:tmp1 = FL("HITS LEFT", 5-TargetModeHits)
            Case 5:
                If Not ReadyToKill Then
                    tmp1 = FL("HITS LEFT", 4-TargetModeHits)
                Else
                    tmp1 = CL("SHOOT THE ROCKET")
                End If
            Case 6:tmp1 = FL("HITS LEFT", 5-TargetModeHits)
            Case 7:tmp1 = FL("HITS LEFT", 4-TargetModeHits)
            Case 8:tmp1 = FL("HITS LEFT", 5-TargetModeHits)
            Case 9:tmp1 = FL("HITS LEFT", 5-TargetModeHits)
            Case 10:tmp1 = FL("HITS LEFT", 6-TargetModeHits)
            Case 11:tmp1 = FL("HITS LEFT", 6-TargetModeHits)
            Case 12:tmp1 = FL("SPINNERS CENTER", SpinNeeded-SpinCount)
            Case 13:tmp1 = FL("HITS LEFT", 6-TargetModeHits)
            Case 14:tmp1 = FL("HITS LEFT", 6-TargetModeHits)
            Case 15:tmp1 = CL("SHOOT JACKPOTS")
        End Select
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
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

Function ExpandLine(TempStr) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(20)
    Else
        if Len(TempStr) > Space(20) Then
            TempStr = Left(TempStr, Space(20) )
        Else
            if(Len(TempStr) < 20) Then
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
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1) ) + 48) & right(NumString, Len(NumString) - i)
        end if
    Next
    FormatScore = NumString
End function

Function FL(NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    Temp = 20 - Len(NumString1) - Len(NumString2)
    TempStr = NumString1 & Space(Temp) & NumString2
    FL = TempStr
End Function

Function CL(NumString) 'center line
    Dim Temp, TempStr
    Temp = (20 - Len(NumString) ) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(NumString) 'right line
    Dim Temp, TempStr
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
    For i = 0 to 255:Chars(i) = "d_empty":Next

    Chars(32) = "d_empty"
    Chars(33) = ""       '!
    Chars(34) = ""       '"
    Chars(35) = ""       '#
    Chars(36) = ""       '$
    Chars(37) = ""       '%
    Chars(38) = ""       '&
    Chars(39) = ""       ''
    Chars(40) = ""       '(
    Chars(41) = ""       ')
    Chars(42) = ""       '*
    Chars(43) = ""       '+
    Chars(44) = ""       '
    Chars(45) = ""       '-
    Chars(46) = "d_dot"  '.
    Chars(47) = ""       '/
    Chars(48) = "d_0"    '0
    Chars(49) = "d_1"    '1
    Chars(50) = "d_2"    '2
    Chars(51) = "d_3"    '3
    Chars(52) = "d_4"    '4
    Chars(53) = "d_5"    '5
    Chars(54) = "d_6"    '6
    Chars(55) = "d_7"    '7
    Chars(56) = "d_8"    '8
    Chars(57) = "d_9"    '9
    Chars(60) = "d_less" '<
    Chars(61) = ""       '=
    Chars(62) = "d_more" '>
    Chars(64) = ""       '@
    Chars(65) = "d_a"    'A
    Chars(66) = "d_b"    'B
    Chars(67) = "d_c"    'C
    Chars(68) = "d_d"    'D
    Chars(69) = "d_e"    'E
    Chars(70) = "d_f"    'F
    Chars(71) = "d_g"    'G
    Chars(72) = "d_h"    'H
    Chars(73) = "d_i"    'I
    Chars(74) = "d_j"    'J
    Chars(75) = "d_k"    'K
    Chars(76) = "d_l"    'L
    Chars(77) = "d_m"    'M
    Chars(78) = "d_n"    'N
    Chars(79) = "d_o"    'O
    Chars(80) = "d_p"    'P
    Chars(81) = "d_q"    'Q
    Chars(82) = "d_r"    'R
    Chars(83) = "d_s"    'S
    Chars(84) = "d_t"    'T
    Chars(85) = "d_u"    'U
    Chars(86) = "d_v"    'V
    Chars(87) = "d_w"    'W
    Chars(88) = "d_x"    'X
    Chars(89) = "d_y"    'Y
    Chars(90) = "d_z"    'Z
    Chars(94) = "d_up"   '^
    '    Chars(95) = '_
    Chars(96) = "d_0a"  '0.
    Chars(97) = "d_1a"  '1. 'a
    Chars(98) = "d_2a"  '2. 'b
    Chars(99) = "d_3a"  '3. 'c
    Chars(100) = "d_4a" '4. 'd
    Chars(101) = "d_5a" '5. 'e
    Chars(102) = "d_6a" '6. 'f
    Chars(103) = "d_7a" '7. 'g
    Chars(104) = "d_8a" '8. 'h
    Chars(105) = "d_9a" '9. 'i
    Chars(106) = ""     'j
    Chars(107) = ""     'k
    Chars(108) = ""     'l
    Chars(109) = ""     'm
    Chars(110) = ""     'n
    Chars(111) = ""     'o
    Chars(112) = ""     'p
    Chars(113) = ""     'q
    Chars(114) = ""     'r
    Chars(115) = ""     's
    Chars(116) = ""     't
    Chars(117) = ""     'u
    Chars(118) = ""     'v
    Chars(119) = ""     'w
    Chars(120) = ""     'x
    Chars(121) = ""     'y
    Chars(122) = ""     'z
    Chars(123) = ""     '{
    Chars(124) = ""     '|
    Chars(125) = ""     '}
    Chars(126) = ""     '~
End Sub

'********************
' Real Time updates
'********************
'used for all the real time updates

Sub Realtime_Timer
    RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    LeftFlipperTop001.RotZ = LeftFlipper001.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
' add any other real time update subs, like gates or diverters, flippers
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
    TurnOffArrows
End Sub

Sub TurnOffArrows() 'during Modes when changing modes
    For each x in aArrows
        SetLightColor x, white, 0
    Next
End Sub

Sub TurnOnArrows(incolor) 'blink during Modes
    For each x in aArrows
        SetLightColor x, incolor, 2
    Next
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
        If Credits > 0 Then
            DMD CL("CREDITS " & Credits), CL("PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "       MARTY AND JPS", "          PRESENTS", "d_jppresents", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 4000, False, ""
    DMD "", CL("ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("HIGHSCORES"), Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL("HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL("HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    StartLightSeq
    StartRainbow aArrows
    DMDFlush
    ShowTableInfo
    PlaySong "mu_gameover"
End Sub

Sub StopAttractMode
    StopRainbow
    DMDScoreNow
    LightSeqAttract.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 10
    'LightSeqAttract.Play SeqAllOff
    LightSeqAttract.Play SeqDiagUpRightOn, 25, 2
    LightSeqAttract.Play SeqStripe1VertOn, 25
    LightSeqAttract.Play SeqClockRightOn, 180, 2
    LightSeqAttract.Play SeqFanLeftUpOn, 50, 2
    LightSeqAttract.Play SeqFanRightUpOn, 50, 2
    LightSeqAttract.Play SeqScrewRightOn, 50, 2

    LightSeqAttract.Play SeqDiagDownLeftOn, 25, 2
    LightSeqAttract.Play SeqStripe2VertOn, 25, 2
    LightSeqAttract.Play SeqFanLeftDownOn, 50, 2
    LightSeqAttract.Play SeqFanRightDownOn, 50, 2
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
End Sub

' tables variables and Mode init
Dim bRotateLights
Dim Mode(4, 15) 'the first 4 is the current player, contains status of the Mode, 0 not started, 1 won, 2 started
Dim Weapons(4)  ' collected weapons
Dim LoopCount
Dim LoopHits(4)
Dim LoopValue(4)
Dim SlingCount 'used for the db2s animation
Dim ComboCount
Dim ComboHits(4)
Dim ComboValue(4)
Dim Mystery(4, 4) 'inlane lights for each player
Dim BumperHits(4)
Dim BumperNeededHits(4)
Dim TargetHits(4, 7) '6 targets + the bumper -the blue lights
Dim WeaponHits(4, 6) '6 lights, lanes and the magnet post
Dim aWeaponSJactive
Dim WeaponSJValue(4)
Dim bFlippersEnabled
Dim bTommyStarted
Dim TommyCount 'counts the seconds left
Dim TommyValue
Dim bPoliceStarted
Dim PoliceRampHits
Dim PoliceTargetHits
Dim PoliceCount 'counts the seconds, used for the police jackpot
Dim TeensKilled(4)
Dim TeensKilledValue(4)
Dim CounselorsKilled(4)
Dim CenterSpinnerHits(4)
Dim LeftSpinnerHits(4)
Dim RightSpinnerHits(4)
Dim TargetJackpot(4)
Dim bJasonMBStarted
Dim bFreddyMBStarted
Dim bMichaelMBStarted
Dim ArrowMultiPlier(8) 'used for the Jackpot multiplier and the color for the Jason MB arrow lights
Dim FreddySJValue
Dim MichaelSJValue
'variables used only in the modes
Dim NewMode
Dim ReadyToKill ' final shot in a mode
Dim SpinCount
Dim SpinNeeded
Dim TargetModeHits 'mode 2,3 hits
Dim EndModeCountdown
Dim BlueTargetsCount
Dim ArrowsCount

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()

    'Init Variables
    bRotateLights = True
    aWeaponSJactive = False
    bFlippersEnabled = True 'only disabled if the police or Tommy catches you
    bTommyStarted = False
    TommyCount = 1          'we set it to 1 because it also acts as a multiplier in the hurry up
    TommyValue = 500000
    bPoliceStarted = False
    PoliceRampHits = 0
    PoliceTargetHits = 0
    bJasonMBStarted = False
    bFreddyMBStarted = False
    bMichaelMBStarted = False
    FreddySJValue = 1000000
    MichaelSJValue = 1000000
    NewMode = 0
    SpinCount = 0
    ReadyToKill = False
    SpinNeeded = 0
    EndModeCountdown = 0
    BlueTargetsCount = 0
    ArrowsCount = 0
    For i = 0 to 4
        SkillShotValue(i) = 500000
        SuperSkillShotValue(i) = 5000000
        LoopValue(i) = 500000
        ComboValue(i) = 500000
        BumperHits(i) = 0
        BumperNeededHits(i) = 10
        Weapons(i) = 0
        WeaponSJValue(i) = 3500000
        TeensKilled(i) = 0
        TeensKilledValue(i) = 250000
        CounselorsKilled(i) = 0
        LoopHits(i) = 0
        ComboHits(i) = 0
        CenterSpinnerHits(i) = 0
        LeftSpinnerHits(i) = 0
        RightSpinnerHits(i) = 0
        TargetJackpot(i) = 500000
        Jackpot(i) = 500000 'only used in the last mode
        BallsInLock(i) = 0
        ArrowMultiPlier(i) = 1
    Next
    For i = 0 to 4
        For j = 0 to 4
            Mystery(i, j) = 0
        Next
    Next
    For i = 0 to 4
        For j = 0 to 15
            Mode(i, j) = 0
        Next
    Next
    For i = 0 to 4
        For j = 0 to 7
            TargetHits(i, j) = 1
        Next
    Next
    For i = 0 to 4
        For j = 0 to 6
            WeaponHits(i, j) = 1
        Next
    Next
    LoopCount = 0
    ComboCount = 0
End Sub

Sub InstantInfo
    Dim tmp
    DMD CL("INSTANT INFO"), "", "", eNone, eNone, eNone, 1000, False, ""
    Select Case NewMode
        Case 1 'A.J Mason = Super Spinners
            DMD CL("CURRENT MODE"), CL("GRANT"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE SPINNERS"), CL("AND ASTON TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 2 'Adam = 5 Targets at semi random
            DMD CL("CURRENT MODE"), CL("DENT"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("HIT THE LIT TARGETS"), CL("AND MAGNET TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 3 'Brandon = 5 Flashing Shots 90 seconds to complete
            DMD CL("CURRENT MODE"), CL("DIKKO"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE LIGHTS"), CL("YOU HAVE 90 SECONDS"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE ASTON"), CL("BEFORE TIME IS UP"), "", eNone, eNone, eNone, 2000, False, ""
        Case 4 'Chad = 5 Orbits
            DMD CL("CURRENT MODE"), CL("BLOFELD"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 5"), CL("LIT ORBITS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 5 'Deborah = Shoot 4 lights 60 seconds
            DMD CL("CURRENT MODE"), CL("LARGO"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 4 LIGHTS"), CL("YOU HAVE 60 SECONDS"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("AND ROCKET TO FINISH"), CL("AND ROCKET TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 6 'Eric = Shoot the ramps
            DMD CL("CURRENT MODE"), CL("GOLDFINGER"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("GOLDFINGER"), CL("SHOOT 5 RAMPS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 7 'Jenny=  Target Frenzy
            DMD CL("CURRENT MODE"), CL("ROSA"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 4"), CL("LIT TARGETS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 8 'Mitch = 5 Targets in rotation
            DMD CL("CURRENT MODE"), CL("DR NO"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL(""), CL("SHOOT 5 LIT TARGETS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 9 'Fox = Magnet post
            DMD CL("CURRENT MODE"), CL("DIAMONDS"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE MAGNET"), CL("5 TIMES"), "", eNone, eNone, eNone, 2000, False, ""
        Case 10 'Victoria = Ramps and Orbits
            DMD CL("CURRENT MODE"), CL("LIVE"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 6 RAMPS"), CL("OR ORBITS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 11 'Kenny = 5 Blue Targets at random
            DMD CL("CURRENT MODE"), CL("THUNDER"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 6"), CL("BLUE TARGETS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 12 'Sheldon = Super Spinners at random
            DMD CL("CURRENT MODE"), CL("DR NO"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE SPINNERS"), CL("AT RANDOM"), "", eNone, eNone, eNone, 2000, False, ""
        Case 13 'Tiffany = Follow the Lights
            DMD CL("CURRENT MODE"), CL("RUSSIAN"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 6 LIT"), CL("LIGHTS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 14 'Vanessa = Follow the Lights random
            DMD CL("CURRENT MODE"), CL("GOLDFINGER"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 6 LIT"), CL("LIGHTS"), "", eNone, eNone, eNone, 2000, False, ""
    End Select
    DMD CL("YOUR SCORE"), CL(FormatScore(Score(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("EXTRA BALLS"), CL(ExtraBallsAwards(CurrentPlayer) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("PLAYFIELD MULTIPLIER"), CL("X " &PlayfieldMultiplier(CurrentPlayer) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("BONUS MULTIPLIER"), CL("X " &BonusMultiplier(CurrentPlayer) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SKILLSHOT VALUE"), CL(FormatScore(SkillshotValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SUPR SKILLSHOT VALUE"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("RAMP COMBO VALUE"), CL(FormatScore(ComboValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("LOOP COMBO VALUE"), CL(FormatScore(LoopValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("WEAPONS SUPR JACKPOT"), CL(FormatScore(WeaponSJValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("TARGET JACKPOT"), CL(FormatScore(TargetJackpot(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
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
End Sub

Sub StopMBmodes 'stop multiball modes after loosing the last multibal
    If bJasonMBStarted Then StopJasonMultiball
    If bFreddyMBStarted Then StopFreddyMultiball
    If bMichaelMBStarted Then StopMichaelMultiball
    If NewMode = 15 Then StopMode
End Sub

Sub StopEndOfBallMode()                         'this sub is called after the last ball in play is drained, reset skillshot, modes, timers
    If li048.State then SuperJackpotTimer_Timer 'to turn off the timer
    If bPoliceStarted Then StopPolice
    if bTommyStarted Then StopTommyJarvis
    StopMode
    TeenTimer.Enabled = 0
End Sub

Sub ResetNewBallVariables() 'reset variables and lights for a new ball or player
    'turn on or off the needed lights before a new ball is released
    TurnOffPlayfieldLights
    libumper.State = 0
    libumper1.State = 0
    Flasher002.Visible = 0
    Flasher002a.Visible = 0
    'set up the lights according to the player achievments
    BonusMultiplier(CurrentPlayer) = 1 'no need to update light as the 1x light do not exists
    UpdateTargetLights
    UpdateWeaponLights                 ' the W lights
    UpdateWeaponLights2                ' the collected weapons
    aWeaponSJactive = False
    UpdateLockLights                   ' turn on the lock lights for the current player
    UpdateModeLights                   ' show the killed counselors
    TeenTimer.Enabled = 1
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub UpdateTargetLights 'CurrentPlayer
    Li031.State = TargetHits(CurrentPlayer, 1)
    Li049.State = TargetHits(CurrentPlayer, 2)
    Li050.State = TargetHits(CurrentPlayer, 3)
    Li051.State = TargetHits(CurrentPlayer, 4)
    Li058.State = TargetHits(CurrentPlayer, 5)
    Li057.State = TargetHits(CurrentPlayer, 6)
    Li079.State = TargetHits(CurrentPlayer, 7)
End Sub

Sub TurnOffBlueTargets 'Turns off all blue targets at the start of a mode that uses the blue targets
    Li031.State = 0
    Li049.State = 0
    Li050.State = 0
    Li051.State = 0
    Li058.State = 0
    Li057.State = 0
    Li079.State = 0
End Sub

Sub ResetTargetLights 'CurrentPlayer
    Dim j
    For j = 0 to 7
        TargetHits(CurrentPlayer, j) = 1
    Next
    UpdateTargetLights
End Sub

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
    LightSeqSkillshot.Play SeqAllOff
    DMD CL("HIT LIT LIGHT"), CL("FOR SKILLSHOT"), "", eNone, eNone, eNone, 3000, True, ""
    li034.State = 2
    li034a.State = 2
    li063.State = 2
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    bRotateLights = True
    LightSeqSkillshot.StopPlay
    Li034.State = 0
    li034a.State = 0
    li063.State = 0
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

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Lemk
    DOF 105, DOFPulse
        'If FlippersBlood Then LeftSplat
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    ' add some effect to the table?
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
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
    DOF 106, DOFPulse
        'If FlippersBlood Then RightSplat
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    ' add some effect to the table?
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
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

Sub SlingTimer_Timer
    Select case SlingCount
        Case 0, 2, 4, 6, 8:Controller.B2SSetData 10, 1
        Case 1, 3, 5, 7, 9:Controller.B2SSetData 10, 0
        Case 10:SlingTimer.Enabled = 0
    End Select
    SlingCount = SlingCount + 1
End Sub

'***********************
'        Bumper
'***********************
' Bumper Jackpot is scored when the bumper light is on
' the value is always 200.000 + 20% of the score

Sub Bumper1_Hit ' W6
    If Tilted Then Exit Sub
    Dim tmp
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), Bumper1
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    DOF 138, DOFPulse
    ' add some points
    If libumper.State Then 'the light is on so give the bumper Jackpot
        FlashForms libumper, 1500, 75, 0
        FlashForms Flasher002, 1500, 75, 0
        DOF 127, DOFPulse
        tmp = 100000 + INT(Score(CurrentPlayer) * 0.01) * 10 'the bumper jackpot is 100.000 + 10% of the score
        DMD CL("BUMPER JACKPOT"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 1500, True, "vo_jackpot" &RndNbr(6)
        AddScore2 tmp
    Else 'score normal points
        AddScore 1000
    End If
    ' check for modes
    Select Case NewMode
        Case 2, 7, 8, 11
            If li079.State Then
                TargetModeHits = TargetModeHits + 1
                li079.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 7) = 0
            CheckTargets
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper1"
    ' increase the bumper hit count and increase the bumper value after each 30 hits
    BumperHits(CurrentPlayer) = BumperHits(CurrentPlayer) + 1
    ' Check the bumper hits to lit the bumper to collect the bumper jackpot
    If BumperHits(CurrentPlayer) = BumperNeededHits(CurrentPlayer) Then
        libumper.State = 1
        Flasher002.Visible = 1
        BumperNeededHits(CurrentPlayer) = BumperNeededHits(CurrentPlayer) + 10 + RndNbr(10)
    End If
End Sub

Sub Bumper2_Hit ' W6
    If Tilted Then Exit Sub
    Dim tmp
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), Bumper2
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    DOF 138, DOFPulse
    ' add some points
    If libumper1.State Then 'the light is on so give the bumper Jackpot
        FlashForms libumper1, 1500, 75, 0
        FlashForms Flasher002a, 1500, 75, 0
        DOF 127, DOFPulse
        tmp = 100000 + INT(Score(CurrentPlayer) * 0.01) * 10 'the bumper jackpot is 100.000 + 10% of the score
        DMD CL("BUMPER JACKPOT"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 1500, True, "vo_jackpot" &RndNbr(6)
        AddScore2 tmp
    Else 'score normal points
        AddScore 1000
    End If
    ' check for modes
    Select Case NewMode
        Case 2, 7, 8, 11
            If li079.State Then
                TargetModeHits = TargetModeHits + 1
                li079.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 7) = 0
            CheckTargets
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper2"
    ' increase the bumper hit count and increase the bumper value after each 30 hits
    BumperHits(CurrentPlayer) = BumperHits(CurrentPlayer) + 1
    ' Check the bumper hits to lit the bumper to collect the bumper jackpot
    If BumperHits(CurrentPlayer) = BumperNeededHits(CurrentPlayer) Then
        libumper1.State = 1
        Flasher002a.Visible = 1
        BumperNeededHits(CurrentPlayer) = BumperNeededHits(CurrentPlayer) + 10 + RndNbr(10)
    End If
End Sub

'*********
' Lanes
'*********
' in and outlanes - mystery ?
Sub Trigger001_Hit
    PLaySoundAt "fx_sensor", Trigger001
    DOF 207, DOFPulse
    If Tilted Then Exit Sub
    Addscore 5000
    Mystery(CurrentPlayer, 1) = 1
    CheckMystery
End Sub

Sub Trigger002_Hit
    PLaySoundAt "fx_sensor", Trigger002
    DOF 207, DOFPulse
    If Tilted Then Exit Sub
    Addscore 1000
    Mystery(CurrentPlayer, 2) = 1
    CheckMystery
End Sub

Sub Trigger002a_Hit
    PLaySoundAt "fx_sensor", Trigger002a
    DOF 207, DOFPulse
    If Tilted Then Exit Sub
    Addscore 1000
    Mystery(CurrentPlayer, 2) = 1
    CheckMystery
End Sub

Sub Trigger003_Hit
    PLaySoundAt "fx_sensor", Trigger003
    DOF 207, DOFPulse
    If Tilted Then Exit Sub
    Addscore 1000
    Mystery(CurrentPlayer, 3) = 1
    CheckMystery
End Sub

Sub Trigger003a_Hit
    PLaySoundAt "fx_sensor", Trigger003a
    DOF 207, DOFPulse
    If Tilted Then Exit Sub
    Addscore 1000
    Mystery(CurrentPlayer, 3) = 1
    CheckMystery
End Sub

Sub Trigger004_Hit
    PLaySoundAt "fx_sensor", Trigger004
    DOF 207, DOFPulse
    If Tilted Then Exit Sub
    Addscore 5000
    Mystery(CurrentPlayer, 4) = 1
    CheckMystery
End Sub

Sub UpdateMysteryLights
    'update lane lights
    li017.State = Mystery(CurrentPlayer, 1)
    li018.State = Mystery(CurrentPlayer, 2)
    li019.State = Mystery(CurrentPlayer, 3)
    li020.State = Mystery(CurrentPlayer, 4)
    li017a.State = Mystery(CurrentPlayer, 1)
    li018a.State = Mystery(CurrentPlayer, 2)
    li019a.State = Mystery(CurrentPlayer, 3)
    li020a.State = Mystery(CurrentPlayer, 4)
    If Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4) = 6 Then
        li078.State = 1
    End If
End Sub

Sub RotateLaneLights(n) 'n is the direction, 1 or 0, left or right. They are rotated by the flippers
    Dim tmp
    If bRotateLights Then
        If n = 1 Then
            tmp = Mystery(CurrentPlayer, 1)
            Mystery(CurrentPlayer, 1) = Mystery(CurrentPlayer, 2)
            Mystery(CurrentPlayer, 2) = Mystery(CurrentPlayer, 3)
            Mystery(CurrentPlayer, 3) = Mystery(CurrentPlayer, 4)
            Mystery(CurrentPlayer, 4) = tmp
        Else
            tmp = Mystery(CurrentPlayer, 4)
            Mystery(CurrentPlayer, 4) = Mystery(CurrentPlayer, 3)
            Mystery(CurrentPlayer, 3) = Mystery(CurrentPlayer, 2)
            Mystery(CurrentPlayer, 2) = Mystery(CurrentPlayer, 1)
            Mystery(CurrentPlayer, 1) = tmp
        End If
    End If
    UpdateMysteryLights
End Sub

'table lanes
Sub Trigger005_Hit
    PLaySoundAt "fx_sensor", Trigger005
    If Tilted Then Exit Sub
    Addscore 1000
    If bSkillShotReady Then li034.State = 0 : li034a.State = 0
    If bMichaelMBStarted AND li032.State Then 'award the michael super jackpot
        DOF 126, DOFPulse
        DMD CL("SUPER JACKPOT"), CL(FormatScore(MichaelSJValue) ), "_", eBlink, eNone, eNone, 1000, True, "vo_superjackpot"
        Addscore2 MichaelSJValue
        MichaelSJValue = 1000000
        li032.State = 0
        LightEffect 2
        GiEffect 2
    End If
End Sub

Sub Trigger006_Hit 'skillshot 1
    PLaySoundAt "pistolet", Trigger006
    If Tilted Then Exit Sub
    Addscore 1000
    If bSkillShotReady AND li034.State Then AwardSkillshot
    If bSkillShotReady AND li034a.State Then AwardSkillshot
End Sub

Sub Trigger008_Hit 'end top loop
    PLaySoundAt "pistolet1", Trigger008
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    Addscore 5000
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger008"
    Select Case NewMode
        Case 4:TargetModeHits = TargetModeHits + 1:CheckWinMode
    End Select
End Sub

Sub Trigger009_Hit 'right loop
    PLaySoundAt "pistoletsilencieux", Trigger009
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    PlayThunder
    Addscore 10000
    Flashforms f2A, 800, 50, 0
    Flashforms F2B, 800, 50, 0
    Flashforms Flasher004, 800, 50, 0
    Flashforms Flasher005, 800, 50, 0
    Flashforms Flashershadowgreen, 800, 50, 0
    If LastSwitchHit = "Trigger008" Then
        AwardLoop
    Else
        li061.State = 2 'super loops light
        LoopCount = 1
    End If
    'If F002.State Then TeenKilled:F002.State = 0
    ' Weapons Super Jackpot
    If aWeaponSJactive AND li060.State Then AwardWeaponsSuperJackpot
    'Jason multiball
    If bJasonMBStarted Then
        DOF 127, DOFPulse
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(7) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(7) * 1000000
        If ArrowMultiPlier(7) < 5 Then
            ArrowMultiPlier(7) = ArrowMultiPlier(7) + 1
            UpdateArrowLights
        End If
    End If
    'Freddy multiball
    If bFreddyMBStarted and li060.State Then
        DOF 126, DOFPulse
        DMD CL("SUPER JACKPOT"), CL(FormatScore(FreddySJValue) ), "_", eBlink, eNone, eNone, 1000, True, "vo_superjackpot"
        Addscore2 FreddySJValue
        FreddySJValue = 1000000
        li060.State = 0
        LightEffect 2
        GiEffect 2
    End If
    'Modes
    Select Case NewMode
        Case 5, 10, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger009"
End Sub

'****************************
' extra triggers - no sound
'****************************

Sub Gate001_Hit 'superskillshot
    If Tilted Then Exit Sub
    Addscore 1000
    If bSkillShotReady Then AwardSuperSkillshot
End Sub

Sub Trigger011_Hit 'cabin playfield , only active when the ball move upwards
    If Tilted OR ActiveBall.VelY > 0 Then Exit Sub
    Addscore 5000
    If aWeaponSJactive Then     'the lit Super Jackpot light is lit, so lit the Super Jackpot Light at the right loop
        SuperJackpotTimer_Timer 'call the timer to stop the 30s timer and blinking light at the cabin
        li060.State = 2
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DOF 127, DOFPulse
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(4) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(4) * 1000000
        If ArrowMultiPlier(4) < 5 Then
            ArrowMultiPlier(4) = ArrowMultiPlier(4) + 1
            UpdateArrowLights
        End If
    End If
    ' lock balls
    Select Case BallsInLock(CurrentPlayer)
        Case 0: 'enabled the first lock
            DMD "_", CL("LOCK IS LIT"), "_", eNone, eNone, eNone, 1000, True, "vo_lockislit"
            BallsInLock(CurrentPlayer) = 1
            UpdateLockLights
        Case 1: 'lock 1
            DMD "_", CL("BALL 1 LOCKED"), "_", eNone, eNone, eNone, 1000, True, "vo_ball1locked"
            BallsInLock(CurrentPlayer) = 2
            UpdateLockLights
        Case 2: 'lock 2
            DMD "_", CL("BALL 2 LOCKED"), "_", eNone, eNone, eNone, 1000, True, "vo_ball2locked"
            BallsInLock(CurrentPlayer) = 3
            UpdateLockLights
        Case 3 'lock 3 - start multiball if there is not a multiball already
            If NOT bMultiBallMode Then
                DMD "_", CL("BALL 3 LOCKED"), "_", eNone, eNone, eNone, 1000, True, "vo_ball3locked"
                BallsInLock(CurrentPlayer) = 4
                UpdateLockLights
                lighteffect 2
                Flasheffect 5
                StartJasonMultiball
            End If
    End Select
    'Modes
    Select Case NewMode
        Case 3
            If li066.State Then
                TargetModeHits = TargetModeHits + 1
                li066.State = 0
                CheckWinMode
            End If
        Case 5, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
            If ReadyToKill Then 'kill her :)
                WinMode
            End If
        Case 15
            AwardJackpot
    End Select
End Sub

Sub UpdateLockLights
    Select Case BallsInLock(CurrentPlayer)
        Case 0:li071.State = 0:li072.State = 0:li073.State = 0
        Case 1:li073.State = 2                                 'enabled the first lock
        Case 2:li073.State = 1:li072.State = 2                 'lock 1
        Case 3:li072.State = 1:li071.State = 2                 'lock 2
        Case 4:li071.State = 0:li072.State = 0:li073.State = 0 'lock 3
    End Select
End Sub

Sub Trigger012_Hit 'left spinner - W1
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs F5, 1000, 75, 0
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    'weapon hit
    If WeaponHits(CurrentPlayer, 1) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 1) = 0
        CheckWeapons
    End If
    'teen killed
    If F001.State Then TeenKilled:F001.State = 0
    'Jason multiball
    If bJasonMBStarted Then
        DOF 127, DOFPulse
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(1) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(1) * 1000000
        If ArrowMultiPlier(1) < 5 Then
            ArrowMultiPlier(1) = ArrowMultiPlier(1) + 1
            UpdateArrowLights
        End If
    End If
    'Michael multiball
    If bMichaelMBStarted Then
        DOF 127, DOFPulse
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
        Addscore2 1000000
        MichaelSJValue = MichaelSJValue + 2000000
        If MichaelSJValue >= 5000000 Then
            li032.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, "vo_getthestupidjackpot"
                Case Else:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, "vo_getthesuperjackpot"
            End Select
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li062.State Then
                TargetModeHits = TargetModeHits + 1
                li062.State = 0
                CheckWinMode
            End If
        Case 5, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
End Sub

Sub Trigger013_Hit 'behind right spinner
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs F4, 1000, 75, 0
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    'weapon Hit
    If WeaponHits(CurrentPlayer, 6) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 6) = 0
        CheckWeapons
    End If
    If li077.State Then 'give the special, which in this table is an add-a-ball
        PlaySound "vo_special"
        AddMultiball 1
        li077.State = 0
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(8) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(8) * 1000000
        If ArrowMultiPlier(8) < 5 Then
            ArrowMultiPlier(8) = ArrowMultiPlier(8) + 1
            UpdateArrowLights
        End If
    End If
    'Michael multiball
    If bMichaelMBStarted Then
        DOF 127, DOFPulse
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
        Addscore2 1000000
        MichaelSJValue = MichaelSJValue + 2000000
        If MichaelSJValue >= 5000000 Then
            li032.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, "vo_getthestupidjackpot"
                Case Else:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, "vo_getthesuperjackpot"
            End Select
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select case NewMode
        Case 5, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
End Sub

Sub Trigger014_Hit 'center spinner for loop awards - W4
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs F3, 1000, 75, 0
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    If LastSwitchHit = "Trigger008" Then
        AwardLoop
    Else
        li061.State = 2 'super loops light
        LoopCount = 1
    End If
    If F005.State Then TeenKilled:F005.State = 0
    'weapon hit
    If WeaponHits(CurrentPlayer, 4) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 4) = 0
        CheckWeapons
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(5) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(5) * 1000000
        If ArrowMultiPlier(5) < 5 Then
            ArrowMultiPlier(5) = ArrowMultiPlier(5) + 1
            UpdateArrowLights
        End If
    End If
    'Michael multiball
    If bMichaelMBStarted Then
        DOF 127, DOFPulse
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
        Addscore2 1000000
        MichaelSJValue = MichaelSJValue + 2000000
        If MichaelSJValue >= 5000000 Then
            li032.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, "vo_getthestupidjackpot"
                Case Else:DMD "_", "SPINNER JACKPOTS LIT", "_", eBlink, eNone, eNone, 1000, True, "vo_getthesuperjackpot"
            End Select
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li067.State Then
                TargetModeHits = TargetModeHits + 1
                li067.State = 0
                CheckWinMode
            End If
        Case 5, 10, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger014"
End Sub

Sub Trigger015_Hit 'right ramp done - W5
    If Tilted Then Exit Sub
    Addscore 5000
    If LastSwitchHit = "Trigger017b" Then
        AwardCombo
    Else
        ComboCount = 1
    End If
    If F003.State Then TeenKilled:F003.State = 0
    'weapon hit
    If WeaponHits(CurrentPlayer, 5) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 5) = 0
        CheckWeapons
    End If
    If bTommyStarted Then 'the Tommy Jarvis hurry up is started so award the jackpot
        AwardTommyJackpot
        StopTommyJarvis
    End If
    If bPoliceStarted Then 'the police hurry up is started
        PoliceRampHits = PoliceRampHits + 1
        If PoliceRampHits = 3 Then
            AwardPoliceJackpot
            StopPolice
        End If
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(6) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(6) * 1000000
        If ArrowMultiPlier(6) < 5 Then
            ArrowMultiPlier(6) = ArrowMultiPlier(6) + 1
            UpdateArrowLights
        End If
    End If
    'Freddy multiball
    If bFreddyMBStarted Then
        DOF 127, DOFPulse
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
        Addscore2 1000000
        FreddySJValue = FreddySJValue + 2000000
        If FreddySJValue >= 5000000 Then
            li060.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", CL("SUPERJACKPOT IS LIT"), "_", eBlink, eNone, eNone, 1000, True, "vo_getthestupidjackpot"
                Case Else:DMD "_", CL("SUPERJACKPOT IS LIT"), "_", eBlink, eNone, eNone, 1000, True, "vo_getthesuperjackpot"
            End Select
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li068.State Then
                TargetModeHits = TargetModeHits + 1
                li068.State = 0
                CheckWinMode
            End If
        Case 5, 6, 10, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
        Case 15
            AwardJackpot
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger015"
End Sub

Sub Trigger016_Hit 'left ramp done - W2
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    Addscore 5000
    If LastSwitchHit = "Trigger017a" Then
        AwardCombo
    Else
        ComboCount = 1
    End If
    If F004.State Then TeenKilled:F004.State = 0
    If li069.State Then AwardTargetJackpot
    'weapon hit
    If WeaponHits(CurrentPlayer, 2) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 2) = 0
        CheckWeapons
    End If
    If bPoliceStarted Then 'the police hurry up is started
        PoliceRampHits = PoliceRampHits + 1
        If PoliceRampHits = 3 Then
            AwardPoliceJackpot
            StopPolice
        End If
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(2) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
        LightEffect 2
        GiEffect 2
        Addscore2 ArrowMultiPlier(2) * 1000000
        If ArrowMultiPlier(2) < 5 Then
            ArrowMultiPlier(2) = ArrowMultiPlier(2) + 1
            UpdateArrowLights
        End If
    End If
    'Freddy multiball
    If bFreddyMBStarted Then
        DOF 127, DOFPulse
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
        Addscore2 1000000
        FreddySJValue = FreddySJValue + 2000000
        If FreddySJValue >= 5000000 Then
            li060.State = 2
            Select Case RndNbr(10)
                Case 1:DMD "_", CL("SUPERJACKPOT IS LIT"), "_", eBlink, eNone, eNone, 1000, True, "vo_getthestupidjackpot"
                Case Else:DMD "_", CL("SUPERJACKPOT IS LIT"), "_", eBlink, eNone, eNone, 1000, True, "vo_getthesuperjackpot"
            End Select
            LightEffect 2
            GiEffect 2
        End If
    End If
    'Modes
    Select Case NewMode
        Case 3
            If li064.State Then
                TargetModeHits = TargetModeHits + 1
                li064.State = 0
                CheckWinMode
            End If
        Case 5, 6, 10, 13, 14
            TargetModeHits = TargetModeHits + 1
            CheckWinMode
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger016"
End Sub

Sub Trigger017_Hit 'cabin top
    DOF 204, DOFPulse
    If Tilted Then Exit Sub
    ChangeGi red
    ChangeGIIntensity 2
    GiEffect 1
    FlashEffect 1
    PlaySound "mu_kikimama"
    vpmTimer.AddTimer 2500, "ChangeGi white:ChangeGIIntensity 1 '"
    ' Select Mode
    Select Case Mode(CurrentPlayer, 0)
        Case 0:SelectMode 'no mode is active then activate another mode
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger017"
End Sub

Sub Trigger017a_Hit 'cabin top
    DOF 204, DOFPulse
    If Tilted Then Exit Sub
    ChangeGi red
    ChangeGIIntensity 2
    GiEffect 1
    FlashEffect 1
    PlaySound "mu_kikimama"
    vpmTimer.AddTimer 2500, "ChangeGi white:ChangeGIIntensity 1 '"
    ' Select Mode
    Select Case Mode(CurrentPlayer, 0)
        Case 0:SelectMode 'no mode is active then activate another mode
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger017a"
End Sub

Sub Trigger017b_Hit 'cabin top
    DOF 204, DOFPulse
    If Tilted Then Exit Sub
    ChangeGi red
    ChangeGIIntensity 2
    GiEffect 1
    FlashEffect 1
    PlaySound "mu_kikimama"
    vpmTimer.AddTimer 2500, "ChangeGi white:ChangeGIIntensity 1 '"
    ' Select Mode
    Select Case Mode(CurrentPlayer, 0)
        Case 0:SelectMode 'no mode is active then activate another mode
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger017b"
End Sub

Sub Trigger018_Hit 'left loop - only light effect
    '	DOF 204, DOFPulse
    If Tilted Then Exit Sub
    Flashforms f1A, 800, 50, 0
    Flashforms F1B, 800, 50, 0
    Flashforms Flasher006, 800, 50, 0
    Flashforms Flasher007, 800, 50, 0
    Flashforms Flashershadow, 800, 50, 0
    Flashforms bond, 800, 50, 0
    Flashforms flashsideD, 800, 50, 0
    Flashforms flashsideG, 800, 50, 0
    Flashforms phareG, 800, 50, 0
    Flashforms phareD, 800, 50, 0
End Sub

'************
'  Targets
'************

Sub Target001_Hit 'police
    PLaySoundAtBall SoundFXDOF("fx_Target",205,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    Flashforms f5, 800, 50, 0
    Flashforms Flasher001, 800, 50, 0
    PlayElectro
    Select Case NewMode
        Case 2, 7, 8, 11
            If li031.State Then
                TargetModeHits = TargetModeHits + 1
                li031.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 1) = 0
            CheckTargets
    End Select
    If NOT bPoliceStarted Then
        PoliceTargetHits = PoliceTargetHits + 1
        If PoliceTargetHits = 5 Then
            StartPolice
        End If
    Else 'the police hurry up is started
        PoliceTargetHits = PoliceTargetHits + 1
        If PoliceTargetHits = 3 Then
            AwardPoliceJackpot
            StopPolice
        End If
    End If
End Sub

Sub Target002_Hit 'right target 1
    PLaySoundAtBall SoundFXDOF("fx_Target",205,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    PlayElectro
    Select Case NewMode
        Case 2, 7, 8, 11
            If li057.State Then
                TargetModeHits = TargetModeHits + 1
                li057.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 6) = 0
            CheckTargets
    End Select
End Sub

Sub Target003_Hit 'right target 2
    PLaySoundAtBall SoundFXDOF("fx_Target",205,DOFPulse,DOFTargets)
    Addscore 5000
    If Tilted Then Exit Sub
    PlayElectro
    Select Case NewMode
        Case 2, 7, 8, 11
            If li058.State Then
                TargetModeHits = TargetModeHits + 1
                li058.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 5) = 0
            CheckTargets
    End Select
End Sub

Sub Target004_Hit 'cabin target 1
    PLaySoundAtBall SoundFXDOF("fx_Target",109,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    PlayElectro
    Select Case NewMode
        Case 2, 7, 8, 11
            If li049.State Then
                TargetModeHits = TargetModeHits + 1
                li049.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 2) = 0
            CheckTargets
    End Select
End Sub

Sub Target005_Hit 'cabin target 2
    PLaySoundAtBall SoundFXDOF("fx_Target",109,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    PlayElectro
    Select Case NewMode
        Case 2, 7, 8, 11
            If li050.State Then
                TargetModeHits = TargetModeHits + 1
                li050.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 3) = 0
            CheckTargets
    End Select
End Sub

Sub Target006_Hit 'loop target
    PLaySoundAtBall SoundFXDOF("fx_Target",109,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    PlayElectro
    Select Case NewMode
        Case 2, 7, 8, 11
            If li051.State Then
                TargetModeHits = TargetModeHits + 1
                li051.State = 0
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 4) = 0
            CheckTargets
    End Select
End Sub

Sub Target007_Hit 'magnet target W3
    PLaySoundAtBall SoundFXDOF("fx_Target",107,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore 5000
    'weapon hit
    If WeaponHits(CurrentPlayer, 3) Then 'if the light is lit then turn it off
        WeaponHits(CurrentPlayer, 3) = 0
        CheckWeapons
    End If
    'extra ball
    If li070.State Then AwardExtraBall:li070.State = 0
    Select Case NewMode
        Case 2
            If li065.State Then
                WinMode
            End If
        Case 9
            If li065.State Then
                TargetModeHits = TargetModeHits + 1
                CheckWinMode
            End If
        Case Else
            TargetHits(CurrentPlayer, 1) = 0
            CheckTargets
    End Select
End Sub

Sub CheckTargets
    Dim tmp, i
    tmp = 0
    UpdateTargetLights
    For i = 1 to 7
        tmp = tmp + TargetHits(CurrentPlayer, i)
    Next
    If tmp = 0 then 'all targets are hit so turn on the target Jackpot
        DMD "_", CL("TARGET JACKPOT S LIT"), "", eNone, eNone, eNone, 1000, True, "vo_shoottheleftramp"
        li069.State = 1
        LightSeqBLueTargets.Play SeqBlinking, , 15, 25
        For i = 1 to 7 'and reset them
            TargetHits(CurrentPlayer, i) = 1
        Next
        UpdateTargetLights
    End If
End Sub

'*************
'  Spinners
'*************

Sub Spinner001_Spin 'right
    PlaySoundAt "fx_spinner", Spinner001
    DOF 200, DOFPulse
    If Tilted Then Exit Sub
    Addscore 1000
    RightSpinnerHits(CurrentPlayer) = RightSpinnerHits(CurrentPlayer) + 1
    ' check for add-a-aball during multiballs or during normal play
    If RightSpinnerHits(CurrentPlayer) >= 100 Then
        LitSpecial
        RightSpinnerHits(CurrentPlayer) = 0
    End If
    CheckSpinners
    'chek modes
    Select Case NewMode
        Case 1
            If SpinCount < SpinNeeded Then
                SpinCount = SpinCount + 1
                CheckWinMode
            End If
        Case 12
            If li059.State AND SpinCount < SpinNeeded Then
                SpinCount = SpinCount + 1
                CheckWinMode
            End If
    End Select
End Sub

Sub LitSpecial
    DMD "_", CL("SPECIAL IS LIT"), "", eNone, eNone, eNone, 1000, True, "vo_specialislit"
    li077.State = 1
End Sub

Sub Spinner002_Spin 'center
    PlaySoundAt "fx_spinner", Spinner002
    DOF 201, DOFPulse
    If Tilted Then Exit Sub
    Addscore 1000
    CenterSpinnerHits(CurrentPlayer) = CenterSpinnerHits(CurrentPlayer) + 1
    ' check for Bonus multiplier
    If CenterSpinnerHits(CurrentPlayer) >= 30 Then
        li074.State = 1
    End If
    If CenterSpinnerHits(CurrentPlayer) >= 40 Then
        AddBonusMultiplier 1
        CenterSpinnerHits(CurrentPlayer) = 0
        li074.State = 0
    End If
    CheckSpinners
    'chek modes
    Select Case NewMode
        Case 1
            If SpinCount < SpinNeeded Then
                SpinCount = SpinCount + 1
                CheckWinMode
            End If
        Case 12
            If li067.State AND SpinCount < SpinNeeded Then
                SpinCount = SpinCount + 1
                CheckWinMode
            End If
    End Select
End Sub

Sub Spinner003_Spin 'left
    PlaySoundAt "fx_spinner", Spinner003
    DOF 202, DOFPulse
    If Tilted Then Exit Sub
    Addscore 1000
    LeftSpinnerHits(CurrentPlayer) = LeftSpinnerHits(CurrentPlayer) + 1
    CheckSpinners
    'chek modes
    Select Case NewMode
        Case 1
            If SpinCount < SpinNeeded Then
                SpinCount = SpinCount + 1
                CheckWinMode
            End If
        Case 12
            If li062.State AND SpinCount < SpinNeeded Then
                SpinCount = SpinCount + 1
                CheckWinMode
            End If
    End Select
End Sub

Sub CheckSpinners
End Sub

'*********
' scoop
'*********

Dim aBall

Sub scoop_Hit
    PlaySoundAt "fx_hole_enter", scoop
    BallsinHole = BallsInHole + 1
    Set aBall = ActiveBall
    scoop.TimerEnabled = 1
    If Tilted Then Exit Sub
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    ' kick out the ball during hurry-ups
    If bTommyStarted OR bPoliceStarted Then vpmtimer.addtimer 500, "kickBallOut '"
    ' check for modes
    Addscore 5000
    Flashforms f4, 800, 50, 0
    Flashforms Flasher003, 800, 50, 0
    ' check modes
    If(Mode(CurrentPlayer, NewMode) = 2) AND(Mode(CurrentPlayer, 0) = 0) Then 'the mode is ready, so start it
        vpmtimer.addtimer 2000, "StartMode '"
        Exit Sub
    End If
    If li078.State Then
        AwardMystery 'after the award the ball will be kicked out
        li078.State = 0
    Else
        Select Case NewMode
            Case 1, 3
                If ReadyToKill Then
                    WinMode
                Else
                    vpmtimer.addtimer 1500, "kickBallOut '"
                End If  
            Case Else
                ' Nothing left to do, so kick out the ball
                vpmtimer.addtimer 1500, "kickBallOut '"
        End Select
    End If
End Sub

Sub scoop_Timer
    If aBall.Z > 0 Then
        aBall.Z = aBall.Z -5
    Else
        scoop.Destroyball
        Me.TimerEnabled = 0
        If Tilted Then kickBallOut
    End If
End Sub

Sub kickBallOut
    If BallsinHole > 0 Then
        BallsinHole = BallsInHole - 1
        PlaySoundAt SoundFXDOF("fx_popper", 111, DOFPulse, DOFcontactors), scoop
        DOF 124, DOFPulse
        scoop1.CreateSizedBallWithMass BallSize / 2, BallMass
        scoop1.kick 270, 3
        Flashforms F4, 500, 50, 0
        vpmtimer.addtimer 400, "kickBallOut '" 'kick out the rest of the balls, if any
    End If
End Sub

'*************
' Magnet
'*************

Sub Trigger007_Hit
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    If ActiveBall.VelY > 10 Then
        ActiveBall.VelY = 10
    End If
    mMagnet.MagnetOn = True
    DOF 112, DOFOn
    Me.TimerEnabled = 1 'to turn off the Magnet
    'Jason multiball
    If bJasonMBStarted Then
        If ActiveBall.VelY < 0 Then 'this means the ball going up
            DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(3) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(6)
            LightEffect 2
            GiEffect 2
            Addscore2 ArrowMultiPlier(3) * 1000000
            If ArrowMultiPlier(3) < 5 Then
                ArrowMultiPlier(3) = ArrowMultiPlier(3) + 1
                UpdateArrowLights
            End If
        End If
    End If
    'Modes
    If ActiveBall.VelY < 0 Then 'this means the ball going up
        Select Case NewMode
            Case 5, 13, 14
                TargetModeHits = TargetModeHits + 1
                CheckWinMode
            Case 15
                AwardJackpot
        End Select
    End If
End Sub

Sub Trigger007_Timer
    Me.TimerEnabled = 0
    ReleaseMagnetBalls
End Sub

Sub ReleaseMagnetBalls 'mMagnet off and release the ball if any
    Dim ball
    mMagnet.MagnetOn = False
    DOF 112, DOFOff
    For Each ball In mMagnet.Balls
        With ball
            .VelX = 0
            .VelY = 1
        End With
    Next
End Sub

'*******************
'    RAMP COMBOS
'*******************
' don't time out
' starts at 500K for a 2 way combo and it is doubled on each combo
' shots that count as ramp combos:
' Left Ramp and Right Ramp
' shots to the same ramp also count as loops

Sub AwardCombo
    ComboCount = ComboCount + 1
    Select Case ComboCount
        Case 1: 'just starting
        Case 2:DMD CL("COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_combo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 3:DMD CL("2X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 2) ), "", eNone, eNone, eNone, 1500, True, "vo_2xcombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 4:DMD CL("3X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 3) ), "", eNone, eNone, eNone, 1500, True, "vo_3xcombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 5:DMD CL("4X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 4) ), "", eNone, eNone, eNone, 1500, True, "vo_4xcombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 6:DMD CL("5X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 5) ), "", eNone, eNone, eNone, 1500, True, "vo_5xcombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
        Case 7:DMD CL("SUPER COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 7) ), "", eNone, eNone, eNone, 1500, True, "vo_supercombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1:DOF 126, DOFPulse
        Case Else:DMD CL("SUPERDUPER COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 10) ), "", eNone, eNone, eNone, 1500, True, "vo_superdupercombo":ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1:DOF 126, DOFPulse
    End Select
    AddScore2 ComboValue(CurrentPlayer) * ComboCount
    ComboValue(CurrentPlayer) = ComboValue(CurrentPlayer) + 100000
End Sub

Sub aComboTargets_Hit(idx) 'reset the combo count if the ball hits another target/trigger
    ComboCount = 0
End Sub

'*******************
'    LOOP COMBOS
'*******************
' starts at 500K for a 2 way combo and it is doubled on each combo
' shots that count as loop combos:
' Center loop and Right Loop
' shots to the same loop also count as loops

Sub AwardLoop
    LoopCount = LoopCount + 1
    Select Case LoopCount
        Case 1: 'just starting
        Case 2:DMD CL("COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_combo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
        Case 3:DMD CL("2X COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) * 2) ), "", eNone, eNone, eNone, 1500, True, "vo_2xcombo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
        Case 4:DMD CL("3X COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) * 3) ), "", eNone, eNone, eNone, 1500, True, "vo_3xcombo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
        Case 5:DMD CL("4X COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) * 4) ), "", eNone, eNone, eNone, 1500, True, "vo_4xcombo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
        Case 6:DMD CL("5X COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) * 5) ), "", eNone, eNone, eNone, 1500, True, "vo_5xcombo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
        Case 7:DMD CL("SUPER COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) * 7) ), "", eNone, eNone, eNone, 1500, True, "vo_supercombo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
        Case Else:DMD CL("SUPERDUPER COMBO"), CL(FormatScore(LoopValue(CurrentPlayer) * 10) ), "", eNone, eNone, eNone, 1500, True, "vo_superdupercombo":LoopHits(CurrentPlayer) = LoopHits(CurrentPlayer) + 1
    End Select
    AddScore2 LoopValue(CurrentPlayer) * LoopCount
    LoopValue(CurrentPlayer) = LoopValue(CurrentPlayer) + 100000
End Sub

Sub aLoopTargets_Hit(idx) 'reset the loop count if the ball hits another target/trigger
    li061.State = 0       'turn off also the super loops light
    LoopCount = 0
End Sub

'*******************
'  Teenager kill
'*******************

' the timer will change the current teenager by lighting the light on top of her

Sub TeenTimer_Timer
    Select Case RndNbr(15)
        Case 1:F001.State = 2:F002.State = 0:F003.State = 0:F004.State = 0:F005.State = 0:PlayQuote:DOF 204, DOFPulse
        Case 2:F001.State = 0:F002.State = 2:F003.State = 0:F004.State = 0:F005.State = 0:PlayQuote:DOF 204, DOFPulse
        Case 3:F001.State = 0:F002.State = 0:F003.State = 2:F004.State = 0:F005.State = 0:PlayQuote:DOF 204, DOFPulse
        Case 4:F001.State = 0:F002.State = 0:F003.State = 0:F004.State = 2:F005.State = 0:PlayQuote:DOF 204, DOFPulse
        Case 5:F001.State = 0:F002.State = 0:F003.State = 0:F004.State = 0:F005.State = 2:PlayQuote:DOF 204, DOFPulse
        Case Else:F001.State = 0:F002.State = 0:F003.State = 0:F004.State = 0:F005.State = 0
    End Select
End Sub

Sub TeenKilled 'a teen has been killed
    'DMD animation
    DMD "", "", "d_goa", eNone, eNone, eNone, 50, False, "sfx_kill" &RndNbr(10)
    DMD "", "", "d_gob", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_goc", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_god", eNone, eNone, eNone, 50, False, ""
    DMD "", "", "d_gof", eNone, eNone, eNone, 50, False, ""
    DMD CL("Y"), "", "d_go1", eNone, eNone, eNone, 100, False, ""
    DMD CL("YO"), "", "d_go2", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU"), "", "d_go3", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU "), "", "d_go4", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU K"), "", "d_go5", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KI"), "", "d_go6", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KIL"), "", "d_go7", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLE"), "", "d_go8", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), "", "d_go9", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("B"), "d_go10", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("B"), "d_go11", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("BA"), "d_go12", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("BAD"), "d_go13", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("BAD "), "d_go14", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("BAD B"), "d_go15", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("BAD BO"), "d_go16", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("BAD BOY"), "d_go17", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("BAD BOYS"), "d_go18", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("BAD BOYS"), "", eNone, eNone, eNone, 1000, False, ""
    Select Case RndNbr(3)
        Case 1:DMD CL("BAD BOYS KILLED"), CL(FormatScore(TeensKilledValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_excellent"
        Case 2:DMD CL("BAD BOYS KILLED"), CL(FormatScore(TeensKilledValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_greatshot"
        Case 3:DMD CL("BAD BOYS KILLED"), CL(FormatScore(TeensKilledValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_notbad"
    End Select
    TeensKilled(CurrentPlayer) = TeensKilled(CurrentPlayer) + 1
    AddScore2 TeensKilledValue(CurrentPlayer)
    TeensKilledValue(CurrentPlayer) = TeensKilledValue(CurrentPlayer) + 50000
    LightEffect 5
    GiEffect 5
    'check
    If TeensKilled(CurrentPlayer) MOD 6 = 0 Then StartTommyJarvis 'start Tommy Jarvis hurry up after each 6th killed teenager
    If TeensKilled(CurrentPlayer) MOD 4 = 0 Then StartPolice      'start police after each 4th killed teenager
    If TeensKilled(CurrentPlayer) MOD 10 = 0 Then LitExtraBall    'lit the extra ball
End Sub

Sub LitExtraBall
    DMD "_", CL("EXTRA BALL IS LIT"), "", eNone, eNone, eNone, 1500, True, "vo_extraballislit"
    li070.State = 1
End Sub

'********************************
' Weapons - Playfield multiplier
'********************************

Sub CheckWeapons
    Dim a, j
    a = 0
    For j = 1 to 6
        a = a + WeaponHits(CurrentPlayer, j)
    Next
    'debug.print a
    If a = 0 Then
        UpgradeWeapons
    Else
        LightSeqWeaponLights.UpdateInterval = 25
        LightSeqWeaponLights.Play SeqBlinking, , 15, 25
        UpdateWeaponLights
        PlaySword
    End If
End Sub

Sub UpdateWeaponLights 'CurrentPlayer
    Li035.State = WeaponHits(CurrentPlayer, 1)
    li037.State = WeaponHits(CurrentPlayer, 2)
    li038.State = WeaponHits(CurrentPlayer, 3)
    li039.State = WeaponHits(CurrentPlayer, 4)
    li040.State = WeaponHits(CurrentPlayer, 5)
    Li036.State = WeaponHits(CurrentPlayer, 6)
End Sub

Sub ResetWeaponLights 'CurrentPlayer
    Dim j
    For j = 1 to 6
        WeaponHits(CurrentPlayer, j) = 1
    Next
    UpdateWeaponLights
End Sub

Sub UpgradeWeapons 'increases the playfield multiplier
    Weapons(CurrentPlayer) = Weapons(CurrentPlayer) + 1
    UpdateWeaponLights2
    AddPlayfieldMultiplier 1
    ResetWeaponLights
    aWeaponSJactive = True
    StartSuperJackpot   'turn on the lits SJ at the cabin
End Sub

Sub UpdateWeaponLights2 'collected weapons
    Select Case Weapons(CurrentPlayer)
        Case 1:li041.State = 1:li041a.State = 1:li042.State = 0:li042a.State = 0:li043.State = 0:li043a.State = 0:li044.State = 0:li044a.State = 0:li045.State = 0:li045a.State = 0:li046.State = 0:li046a.State = 0:li047.State = 0:li047a.State = 0
        Case 2:li041.State = 1:li041a.State = 1:li042.State = 1:li042a.State = 1:li043.State = 0:li043a.State = 0:li044.State = 0:li044a.State = 0:li045.State = 0:li045a.State = 0:li046.State = 0:li046a.State = 0:li047.State = 0:li047a.State = 0
        Case 3:li041.State = 1:li041a.State = 1:li042.State = 1:li042a.State = 1:li043.State = 1:li043a.State = 1:li044.State = 0:li044a.State = 0:li045.State = 0:li045a.State = 0:li046.State = 0:li046a.State = 0:li047.State = 0:li047a.State = 0
        Case 4:li041.State = 1:li041a.State = 1:li042.State = 1:li042a.State = 1:li043.State = 1:li043a.State = 1:li044.State = 1:li044a.State = 1:li045.State = 0:li045a.State = 0:li046.State = 0:li046a.State = 0:li047.State = 0:li047a.State = 0       
        Case 5:li041.State = 1:li041a.State = 1:li042.State = 1:li042a.State = 1:li043.State = 1:li043a.State = 1:li044.State = 1:li044a.State = 1:li045.State = 1:li045a.State = 1:li046.State = 0:li046a.State = 0:li047.State = 0:li047a.State = 0 
        Case 6:li041.State = 1:li041a.State = 1:li042.State = 1:li042a.State = 1:li043.State = 1:li043a.State = 1:li044.State = 1:li044a.State = 1:li045.State = 1:li045a.State = 1:li046.State = 1:li046a.State = 1:li047.State = 0:li047a.State = 0 
        Case 7:li041.State = 1:li041a.State = 1:li042.State = 1:li042a.State = 1:li043.State = 1:li043a.State = 1:li044.State = 1:li044a.State = 1:li045.State = 1:li045a.State = 1:li046.State = 1:li046a.State = 1:li047.State = 1:li047a.State = 1 
    End Select
End Sub

'*******************************************
' Super Jackpot at the cabin and right Loop
'*******************************************

' 30 seconds timer to lit the Super Jackpot at the right loop
' once the Super Jackpot light is lit it will be lit until the end of the ball

Sub StartSuperJackpot 'lits the cabin's red SJ light
    li048.BlinkInterval = 160
    li048.State = 2
    SuperJackpotTimer.Enabled = 1
    SuperJackpotSpeedTimer.Enabled = 1
End Sub

Sub SuperJackpotTimer_Timer            '30 seconds hurry up to turn off the red light at the cabin
    SuperJackpotSpeedTimer.Enabled = 0 'to be sure it is stopped
    SuperJackpotTimer.Enabled = 0
    li048.State = 0
End Sub

Sub SuperJackpotSpeedTimer_Timer 'after 25 seconds speed opp the blinking of the lit SJ red cabin light
    DMD "_", CL("HURRY UP"), "_", eNone, eBlink, eNone, 1500, True, "vo_hurryup"
    SuperJackpotSpeedTimer.Enabled = 0
    li048.BlinkInterval = 80
    li048.State = 2
End Sub

'*************************
' Tommy Jarvis - Hurry up
'*************************
' it starts after each 6 teenagers killed
' a 30 seconds hurry up starts at the right ramp
' hit the right ramp to throw a deadly blow to your archie enemy
' fail and your flippers will die for 3 seconds

Sub StartTommyJarvis 'Tommy's hurry up
    DMD CL("JAMES BOND"), CL("HURRY UP"), "d_border", eNone, eNone, eNone, 2500, True, "vo_shoottherightramp" &RndNbr(2)
    bTommyStarted = True
    FlashEffect 7
    Li030.BlinkInterval = 160
    Li030.State = 2
    TommyCount = 1
    TommyValue = 500000
    TommyHurryUpTimer.Enabled = 1
End Sub

Sub StopTommyJarvis
    TommyHurryUpTimer.Enabled = 0
    TommyRecoverTimer.Enabled = 0
    bTommyStarted = False
    LightSeqTopFlashers.StopPlay
    Li030.State = 0
    TommyCount = 1
End Sub

Sub TommyHurryUpTimer_Timer '30 seconds, runs once a second
    TommyCount = TommyCount + 1
    If TommyCount = 20 Then 'speed up the Light
        DMD "_", CL("HURRY UP"), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_timerunningout"
        Li030.BlinkInterval = 80
        Li030.State = 2
    End If
    If TommyCount = 31 Then 'the 30 seconds are over
        TommyHurryUpTimer.Enabled = 0
        DMD CL("JAMES BOND"), CL("SHOT YOU"), "d_border", eNone, eNone, eNone, 2500, True, "vo_holyshityoushootme"
        DisableFlippers True          'disable the flippers...
        ChangeGi purple
        ChangeGIIntensity 2
        TommyRecoverTimer.Enabled = 1 '... start the 3 seconds timer to enable the table again
    End If
End Sub

Sub TommyRecoverTimer_Timer 'after disabling the table for 3 seconds then enable it
    TommyRecoverTimer.Enabled = 0
    StopTommyJarvis
    DisableFlippers False
    ChangeGi white 
    ChangeGIIntensity 1
End Sub

Sub AwardTommyJackpot() 'scores the seconds multiplied by 500.000, the longer you wait the higher the score
    Dim a
    a = TommyCount * 500000
    Select Case RndNbr(3)
        Case 1:DMD CL("JAMES BOND JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, "vo_waytogo"
        Case 2:DMD CL("JAMES BOND JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, "vo_welldone"
        Case 3:DMD CL("JAMES BOND JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, "vo_younailedit"
    End Select
    DOF 126, DOFPulse
    AddScore2 a
    LightEffect 2
    GiEffect 2
End Sub

'***********************
' The Police - Hurry up
'***********************
' similar to Tommy hurry up, but different score
' it starts after 5 police hits, 2 killed counselors or 4 teenagers
' 30 seconds to hit 2 ramps or 3 police targets
' scores 500.000 for each left second
' fail and your flippers will die for 3 seconds

Sub StartPolice 'police hurry up
    bPoliceStarted = True
    PoliceL1.BlinkInterval = 160
    PoliceL2.BlinkInterval = 160
    Li033.BlinkInterval = 160
    PoliceL1.State = 2
    PoliceL2.State = 2
    Li033.State = 2
    PlaySound "sfx_siren1", -1
    PoliceCount = 30
    PoliceHurryUpTimer.Enabled = 1
    PoliceRampHits = 0   'reset the count
    PoliceTargetHits = 0 'reset the count
End Sub

Sub StopPolice
    PoliceHurryUpTimer.Enabled = 0
    PoliceRecoverTimer.Enabled = 0
    bPoliceStarted = False
    StopSound "sfx_siren1"
    PoliceL1.State = 0
    PoliceL2.State = 0
    Li033.State = 0
    PoliceRampHits = 0       'reset the count
    PoliceTargetHits = 0     'reset the count
    PoliceCount = 0
    DisableFlippers False    'ensure they are not disabled
    ChangeGi white 
    ChangeGIIntensity 1          '
End Sub

Sub PoliceHurryUpTimer_Timer '30 seconds, runs once a second
    PoliceCount = PoliceCount - 1
    If PoliceCount = 8 Then  'speed up the Lights
        DMD "_", CL("HURRY UP"), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_timerunningout"
        PoliceL1.BlinkInterval = 80
        PoliceL2.BlinkInterval = 80
        Li033.BlinkInterval = 80
        PoliceL1.State = 2
        PoliceL2.State = 2
        Li033.State = 2
    End If
    If PoliceCount = 0 Then
        PoliceHurryUpTimer.Enabled = 0
        DMD CL("THE MI6"), CL("SHOT YOU"), "d_border", eNone, eNone, eNone, 2500, True, "vo_holyshityoushootme"
        DisableFlippers True           'disable the flippers...
        ChangeGi Blue
        ChangeGIIntensity 2
        PoliceRecoverTimer.Enabled = 1 '... start the 3 seconds timer to enable the table again
    End If
End Sub

Sub PoliceRecoverTimer_Timer 'after disabling the table for 3 seconds then enable it again
    PoliceRecoverTimer.Enabled = 0
    StopPolice
End Sub

Sub AwardPoliceJackpot() 'scores the seconds left multiplied by 500.000
    Dim a
    DOF 126, DOFPulse
    a = PoliceCount * 500000
    Select Case RndNbr(3)
        Case 1:DMD CL("MI6 JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, "vo_waytogo"
        Case 2:DMD CL("MI6 JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, "vo_welldone"
        Case 3:DMD CL("MI6 JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, "vo_younailedit"
    End Select
    AddScore2 a
    LightEffect 2
    GiEffect 2
End Sub

Sub DisableFlippers(enabled)
    If enabled Then
        SolLFlipper 0
        SolRFlipper 0
        bFlippersEnabled = 0
    Else
        bFlippersEnabled = 1
    End If
End Sub

'***********************************
' Jason Multiball - Main multiball
'***********************************
' shoot the cabin to start locking
' lock 3 balls under the cabin to start
' all main shots are Lit
' 15 seconds ball saver
' each succesive shot will increase the color and score
' Blue shots: 1 million points
' Green Shots: 2 million points
' Yellow shots: 3 million points
' Orange shots: 4 million points
' Red shots: 5 million points

Sub StartJasonMultiball
    Dim i
    If bMultiBallMode Then Exit Sub 'do not start if already in a multiball mode
    bJasonMBStarted = True
    EnableBallSaver 15
    DMD "_", CL("M MULTIBALL"), "_", eNone, eNone, eNone, 2500, True, "vo_multiball1"
    AddMultiball 2
    For i = 1 to 8
        ArrowMultiPlier(i) = 1
    Next
    UpdateArrowLights
    li016.State = 2
    ChangeSong
End Sub

Sub StopJasonMultiball
    bJasonMBStarted = False
    BallsInLock(CurrentPlayer) = 0
    TurnOffArrows
    li016.State = 0
End Sub

Sub UpdateArrowLights 'sets the color of the arrows according to the jackpot multiplier
    SetLightColor li062, ArrowMultiPlier(1), 2
    SetLightColor li064, ArrowMultiPlier(2), 2
    SetLightColor li053, ArrowMultiPlier(3), 2
    SetLightColor li066, ArrowMultiPlier(4), 2
    SetLightColor li067, ArrowMultiPlier(5), 2
    SetLightColor li068, ArrowMultiPlier(6), 2
    SetLightColor li052, ArrowMultiPlier(7), 2
    SetLightColor li059, ArrowMultiPlier(8), 2
End Sub

'***********************************
' Freddy Kruger Multiball - Ramps
'***********************************
' starts after 3 counselors are killed
' shoot the blue arrows at the ramps to collect jackpots
' this will build the super jackpot at the right loop, shoot the cabin for a better shot

Sub StartFreddyMultiball
    Dim i
    If bMultiBallMode Then Exit Sub 'do not start if already in a multiball mode
    bFreddyMBStarted = True
    ChangeSong
    EnableBallSaver 20
    DMD "_", CL("Q MULTIBALL"), "_", eNone, eNone, eNone, 2500, True, "vo_multiball1"
    AddMultiball 2
    SetLightColor li064, blue, 2
    SetLightColor li068, blue, 2
    li015.State = 2
End Sub

Sub StopFreddyMultiball
    bFreddyMBStarted = False
    li064.State = 0
    li068.State = 0
    li015.State = 0
End Sub

'***********************************
' Michael Myers Multiball - Spinners
'***********************************
' starts randomly after 3 counselors are killed
' shoot the blue arrows at the spinners to collect jackpots
' this will build the super jackpot at the lower right loop
' shoot it to collect it

Sub StartMichaelMultiball
    Dim i
    If bMultiBallMode Then Exit Sub 'do not start if already in a multiball mode
    bMichaelMBStarted = True
    ChangeSong
    EnableBallSaver 20
    DMD "_", CL("BOND MULTIBALL"), "_", eNone, eNone, eNone, 2500, True, "vo_multiball1"
    AddMultiball 2
    SetLightColor li062, blue, 2
    SetLightColor li067, blue, 2
    SetLightColor li059, blue, 2
    li075.State = 2
End Sub

Sub StopMichaelMultiball
    bMichaelMBStarted = False
    li062.State = 0
    li067.State = 0
    li059.State = 0
    li075.State = 0
End Sub

'****************************
' Mystery award at the scoop
'****************************
' this is a kind of award after completing the inlane and outlanes

Sub CheckMystery 'if all the inlanes and outlanes are lit then lit the mystery award
    dim i
    If Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4) = 4 Then
        DMD "_", CL("MYSTERY IS LIT"), "", eNone, eNone, eNone, 1000, True, "vo_waytogo"
        li078.State = 1
        ' and reset the lights
        For i = 1 to 4
            Mystery(CurrentPlayer, i) = 0
        Next
    End If
    UpdateMysteryLights
End Sub

Sub AwardMystery 'mostly points but sometimes it will lit the special or the extra ball
    Dim tmp
    FlashEffect 1
    Select Case RndNbr(20)
        Case 1:LitExtraBall                   'lit extra ball
        Case 2:LitSpecial                     'lit special
        Case Else:
            tmp = 250000 + RndNbr(25) * 10000 'add from 250.000 to 500.000
            Select Case RndNbr(4)
                Case 1:DMD CL("MYSTERY SCORE"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 2000, True, "vo_notbad"
                Case 2:DMD CL("MYSTERY SCORE"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 2000, True, "vo_excellentscore"
                Case 3:DMD CL("MYSTERY SCORE"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 2000, True, "vo_nowyouaregettinghot"
                Case 4:DMD CL("MYSTERY SCORE"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 2000, True, "vo_welldone"
            End Select
    End Select
    vpmtimer.addtimer 3500, "kickBallOut '"
End Sub

'**********************************
'   Modes - Hunting the counselors
'**********************************

' This table has 14 modes which will be selected at random
' After killing all counselors you'll a surprise :)

' current active Mode number is stored in Mode(CurrentPlayer,0)
' select a new random Mode if none is active
Sub SelectMode
    Dim i
    If Mode(CurrentPlayer, 0) = 0 Then
        ' reset the Modes that are not finished
        For i = 1 to 14
            If Mode(CurrentPlayer, i) = 2 Then Mode(CurrentPlayer, i) = 0
        Next
        NewMode = RndNbr(14)
        do while Mode(CurrentPlayer, NewMode) <> 0
            NewMode = RndNbr(14)
        loop
        Mode(CurrentPlayer, NewMode) = 2
        Li076.State = 2 'Start hunting light at the scoop
        bondmission.visible = 1
        UpdateModeLights
    'debug.print "newmode " & newmode
    End If
End Sub

' Update the lights according to the mode's state
Sub UpdateModeLights
    li014.State = Mode(CurrentPlayer, 1)
    li013.State = Mode(CurrentPlayer, 2)
    li012.State = Mode(CurrentPlayer, 3)
    li011.State = Mode(CurrentPlayer, 4)
    li010.State = Mode(CurrentPlayer, 5)
    li009.State = Mode(CurrentPlayer, 6)
    li008.State = Mode(CurrentPlayer, 7)
    li007.State = Mode(CurrentPlayer, 8)
    li006.State = Mode(CurrentPlayer, 9)
    li005.State = Mode(CurrentPlayer, 10)
    li004.State = Mode(CurrentPlayer, 11)
    li001.State = Mode(CurrentPlayer, 12)
    li002.State = Mode(CurrentPlayer, 13)
    li003.State = Mode(CurrentPlayer, 14)
End Sub

' Starting a mode means to setup some lights and variables, maybe timers
' Mode lights will always blink during an active mode
Sub StartMode
    Mode(CurrentPlayer, 0) = NewMode
    Li076.State = 0
    bondmission.visible = 0
    ChangeSong
    'PlaySound "vo_hahaha"&RndNbr(4)
    ReadyToKill = False
    Select Case NewMode
        Case 1 'A.J Mason = Super Spinners
            DMD CL("KILL GRANT"), CL("SHOOT THE SPINNERS"), "", eNone, eNone, eNone, 1500, True, "vo_shootthespinners"
            SpinCount = 0
            SetLightColor li062, amber, 2
            SetLightColor li067, amber, 2
            SetLightColor li059, amber, 2
            SpinNeeded = 50
        Case 2 'Adam = 5 Targets at semi random
            DMD CL("KILL DENT"), CL("HIT THE LIT TARGETS"), "", eNone, eNone, eNone, 1500, True, "vo_shootthetargets"
            TargetModeHits = 0
            'Turn off all blue targets
            Li031.State = 0
            Li049.State = 0
            Li050.State = 0
            Li051.State = 0
            Li058.State = 0
            Li057.State = 0
            Li079.State = 0
            Select Case RndNbr(4) 'select 4 blue targets
                Case 1:Li031.State = 2:Li050.State = 2:li058.State = 2:Li079.State = 2
                Case 2:Li049.State = 2:Li050.State = 2:li058.State = 2:Li057.State = 2
                Case 3:Li031.State = 2:Li049.State = 2:li051.State = 2:Li057.State = 2
                Case 4:Li031.State = 2:Li049.State = 2:li050.State = 2:Li051.State = 2
            End Select
        Case 3 'Brandon = 5 Flashing Shots 90 seconds to complete
            DMD CL("KILL DIKKO"), CL("SHOOT THE LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            DMD CL("KILL DIKKO"), CL("YOU HAVE 90 SECONDS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            SetLightColor li062, amber, 2
            SetLightColor li064, amber, 2
            SetLightColor li066, amber, 2
            SetLightColor li067, amber, 2
            SetLightColor li068, amber, 2
            EndModeCountdown = 90
            EndModeTimer.Enabled = 1
        Case 4 'Chad = 5 Orbits 'uses trigger008 to detect the completed orbits
            DMD CL("KILL BLOFELD"), CL("SHOOT 5 ORBITS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            SetLightColor li067, amber, 2
            SetLightColor li052, amber, 2
        Case 5 'Deborah = Shoot 4 lights 60 seconds, all shots are lit, after 4 shot lit the cabin to kill her & collect jackpot
            DMD CL("KILL LARGO"), CL("SHOOT 4 LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            DMD CL("KILL LARGO"), CL("YOU HAVE 60 SECONDS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            TurnonArrows amber
            EndModeCountdown = 60
            EndModeTimer.Enabled = 1
        Case 6 'Eric = Shoot the ramps
            DMD CL("KILL GOLDFINGER"), CL("SHOOT 5 RAMPS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            SetLightColor li064, amber, 2
            SetLightColor li068, amber, 2
        Case 7 'Jenny=  Target Frenzy
            DMD CL("KILL ROSA"), CL("SHOOT 4 LIT TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            'Turn off all blue targets
            TurnOffBlueTargets
            Select Case RndNbr(4) 'select 4 blue targets
                Case 1:Li031.State = 2:Li050.State = 2:li058.State = 2:Li079.State = 2
                Case 2:Li049.State = 2:Li050.State = 2:li058.State = 2:Li057.State = 2
                Case 3:Li031.State = 2:Li049.State = 2:li051.State = 2:Li057.State = 2
                Case 4:Li031.State = 2:Li049.State = 2:li050.State = 2:Li051.State = 2
            End Select
        Case 8 'Mitch = 5 Targets in rotation
            DMD CL("KILL DR NO"), CL("SHOOT 5 LIT TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            'Turn off all blue targets
            TurnOffBlueTargets
            'Start timer to rotate through the targets
            BlueTargetsCount = RndNbr(7)
            BlueTargetsTimer_Timer
            BlueTargetsTimer.Enabled = 1
        Case 9 'Fox = Magnet
            DMD CL("LOOK DIAMONDS"), CL("SHOOT THE MAGNET"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            li065.State = 2
        Case 10 'Victoria = Ramps and Orbits - all lights lit
            DMD CL("LOOK LIVE"), CL("SHOOT RAMPS ORBITS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            SetLightColor li064, amber, 2
            SetLightColor li067, amber, 2
            SetLightColor li068, amber, 2
            SetLightColor li052, amber, 2
        Case 11 'Kenny = 5 Blue Targets at random
            DMD CL("LOOK THUNDER"), CL("SHOOT BLUE TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            'Turn off all blue targets
            TurnOffBlueTargets
            'Start timer to rotate through the targets
            BlueTargetsCount = RndNbr(7)
            BlueTargetsTimer_Timer
            BlueTargetsTimer.Enabled = 1
        Case 12 'Sheldon = Super Spinners at random
            DMD CL("LOOK DR NO"), CL("SHOOT THE SPINNERS"), "", eNone, eNone, eNone, 1500, True, ""
            SpinCount = 0
            SpinNeeded = 50
            SpinnersTimer_Timer
            SpinnersTimer.Enabled = 1
        Case 13 'Tiffany = Follow the Lights All main Shots 1 at a time in rotation
            DMD CL("LOOK RUSSIAN"), CL("SHOOT LIT LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            ArrowsCount = 0
            SetLightColor li062, amber, 0
            SetLightColor li064, amber, 0
            SetLightColor li053, amber, 0
            SetLightColor li066, amber, 0
            SetLightColor li067, amber, 0
            SetLightColor li068, amber, 0
            SetLightColor li052, amber, 0
            SetLightColor li059, amber, 0
            FollowTheLights_Timer
            FollowTheLights.Enabled = 1
        Case 14 'Vanessa = Follow the Lights All main shots at random
            DMD CL("GOLDFINGER"), CL("SHOOT LIT LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            ArrowsCount = 0
            SetLightColor li062, amber, 0
            SetLightColor li064, amber, 0
            SetLightColor li053, amber, 0
            SetLightColor li066, amber, 0
            SetLightColor li067, amber, 0
            SetLightColor li068, amber, 0
            SetLightColor li052, amber, 0
            SetLightColor li059, amber, 0
            FollowTheLights_Timer
            FollowTheLights.Enabled = 1
        Case 15 'the big final mode: 5 ball multiball all jackpots are lit, no timer, score jackpots until the last multiball
            DMD CL("THE BIG FINALE"), CL("SHOOT THE JACKPOTS"), "", eNone, eNone, eNone, 1500, True, ""
            AddMultiball 5
            SetLightColor li062, red, 2
            SetLightColor li064, red, 2
            SetLightColor li053, red, 2
            SetLightColor li066, red, 2
            SetLightColor li067, red, 2
            SetLightColor li068, red, 2
            SetLightColor li052, red, 2
            SetLightColor li059, red, 2
            ChangeGi Red
            ChangeGIIntensity 2
    End Select
    ' kick out the ball
    If BallsinHole Then
        vpmtimer.addtimer 2500, "kickBallOut '"
    End If
End Sub

Sub CheckWinMode
    DOF 126, DOFPulse
    LightSeqInserts.StopPlay 'stop the light effects before starting again so they don't play too long.
    LightEffect 3
    Select Case NewMode
        Case 1
            Addscore 10000
            If SpinCount = SpinNeeded Then
                DMD CL("YOU CATCHED HER"), CL("SHOOT THE ASTON"), "", eNone, eNone, eNone, 1500, True, "vo_itstimetodie"
                li029.State = 2 'lit the Hunter Jackpot at the scoop
                li062.State = 0 'turn off the spinner lights
                li067.State = 0
                li059.State = 0
                ReadyToKill = True
            End If
        Case 2
            Addscore 150000
            If TargetModeHits = 4 Then
                DMD CL("YOU CATCHED HIM"), CL("SHOOT THE MAGNET"), "", eNone, eNone, eNone, 1500, True, "vo_itstimetodie"
                li065.State = 2
                ReadyToKill = True
            End If
        Case 3
            Addscore 150000
            If TargetModeHits = 5 Then
                DMD CL("YOU CATCHED HIM"), CL("SHOOT THE ASTON"), "", eNone, eNone, eNone, 1500, True, "vo_itstimetodie"
                li029.State = 2
                ReadyToKill = True
            End If
        Case 4
            Addscore 150000
            If TargetModeHits = 5 Then WinMode:End if
        Case 5
            Addscore 150000
            If TargetModeHits = 4 Then      'lit the cabin for the kill & jackpot
                DMD CL("YOU CATCHED HER"), CL("SHOOT THE ROCKET"), "", eNone, eNone, eNone, 1500, True, "vo_itstimetodie"
                SetLightColor li066, red, 2 'change the color to red
                ReadyToKill = True
            End If
        Case 6
            Addscore 150000
            If TargetModeHits = 5 Then WinMode:End if
        Case 7
            Addscore 150000
            If TargetModeHits = 4 Then WinMode:End if
        Case 8
            Addscore 150000
            If TargetModeHits = 5 Then
                WinMode
            Else
                BlueTargetsTimer_Timer 'to lit another target
            End if
        Case 9
            Addscore 150000
            If TargetModeHits = 5 Then WinMode:End if
        Case 10
            Addscore 150000
            If TargetModeHits = 6 Then WinMode:End if
        Case 11
            Addscore 150000
            If TargetModeHits = 6 Then
                WinMode
            Else
                BlueTargetsTimer_Timer 'to lit another target
            End if
        Case 12
            Addscore 10000
            If SpinCount = SpinNeeded Then
                WinMode
                SpinnersTimer.Enabled = 0
            End If
        Case 13
            Addscore 150000
            If TargetModeHits = 6 Then
                WinMode
            Else
                FollowtheLights_Timer 'to lit another target
            End if
        Case 14
            Addscore 150000
            If TargetModeHits = 6 Then
                WinMode
            Else
                FollowtheLights_Timer 'to lit another target
            End if
    End Select
End Sub

'called after completing a mode
Sub WinMode
    CounselorsKilled(CurrentPlayer) = CounselorsKilled(CurrentPlayer) + 1
    Select Case NewMode
        Case 1, 5, 7, 10, 13, 14
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "sfx_screamf" &RndNbr(5)
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU LOOKED AT HER"), CL(FormatScore(3500000) ), "_", eNone, eBlink, eNone, 1200, False, ""
            DMD CL("YOU LOOKED AT HER"), CL(FormatScore(3500000) ), "_", eNone, eBlink, eNone, 2000, True, "vo_fantastic"
            Addscore2 3500000
        Case 2, 3, 4, 6, 8, 9, 11, 12
            DMD "", "", "d_kill1", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill2", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill3", eNone, eNone, eNone, 120, False, "sfx_screamm" &RndNbr(5)
            DMD "", "", "d_kill4", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill5", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill6", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill7", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill8", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill9", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill10", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill11", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill12", eNone, eNone, eNone, 120, False, ""
            DMD "", "", "d_kill13", eNone, eNone, eNone, 120, False, ""
            DMD CL("YOU SAW THE MOVIE"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU SAW THE MOVIE"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, "vo_excellent"
            Addscore2 3500000
    End Select
    ' eject the ball if it is in the scoop
    If BallsinHole Then
        vpmtimer.addtimer 5000, "kickBallOut '"
    End If
    DOF 139, DOFPulse
    Mode(CurrentPlayer, 0) = 0
    Mode(CurrentPlayer, NewMode) = 1
    UpdateModeLights
    FlashEffect 2
    LightEffect 2
    GiEffect 2
    ChangeSong
    StopMode2 'to stop specific lights, timers and variables of the mode
    ' start the police hurry up after 2 kills
    IF CounselorsKilled(CurrentPlayer) MOD 2 = 0 Then
        StartPolice
    End If
    ' check for extra modes after each 3 completed kills
    IF CounselorsKilled(CurrentPlayer) MOD 6 = 0 Then
        StartMichaelMultiball
    ElseIF CounselorsKilled(CurrentPlayer) MOD 3 = 0 Then
        StartFreddyMultiball
    ' If all counselors er dead then start final mode
    ElseIF CounselorsKilled(CurrentPlayer) = 14 Then
        NewMode = 15
        StartMode
    End If
End Sub

Sub StopMode 'called at the end of a ball
    Dim i
    Mode(CurrentPlayer, 0) = 0
    For i = 0 to 14 'stop any counselor blinking light
        If Mode(CurrentPlayer, i) = 2 Then Mode(CurrentPlayer, i) = 0
    Next
    UpdateModeLights
    StopMode2
End Sub

Sub StopMode2 'stop any mode special lights, timers or variables, called after a win or end of ball
    ' stop some timers or reset Mode variables
    Select Case NewMode
        Case 1:TurnOffArrows:li029.State = 0
        Case 2:UpdateTargetLights:li065.State = 0
        Case 3:li029.State = 0:EndModeTimer.Enabled = 0
        Case 4:li067.State = 0:li052.State = 0
        Case 5:TurnOffArrows:EndModeTimer.Enabled = 0
        Case 6:TurnOffArrows
        Case 7:UpdateTargetLights
        Case 8:BlueTargetsTimer.Enabled = 0:UpdateTargetLights
        Case 9:li065.State = 0
        Case 10:TurnOffArrows
        Case 11:BlueTargetsTimer.Enabled = 0:UpdateTargetLights
        Case 12:TurnOffArrows:SpinnersTimer.Enabled = 0
        Case 13:TurnOffArrows:FollowTheLights.Enabled = 0
        Case 14:TurnOffArrows:FollowTheLights.Enabled = 0
        Case 15:ResetModes
    End Select
    ' restore multiball lights
    If bJasonMBStarted Then UpdateArrowLights:End If
    If bFreddyMBStarted Then SetLightColor li064, blue, 2:SetLightColor li068, blue, 2
    If bMichaelMBStarted Then SetLightColor li062, blue, 2:SetLightColor li067, blue, 2:SetLightColor li059, blue, 2
    NewMode = 0
    ReadyToKill = False
End Sub

Sub ResetModes
    Dim i, j
    For j = 0 to 4
        CounselorsKilled(j) = 0
        For i = 0 to 14
            Mode(CurrentPlayer, i) = 0
        Next
    Next
    NewMode = 0
    'reset Mode variables
    TurnOffArrows
    SpinCount = 0
    ReadyToKill = False
End Sub

Sub EndModeTimer_Timer '1 second timer to count down to end the timed modes
    EndModeCountdown = EndModeCountdown - 1
    Select Case EndModeCountdown
        Case 16:PlaySound "vo_timerunningout"
        Case 10:PlaySound "vo_ten"
        Case 9:PlaySound "vo_nine"
        Case 8:PlaySound "vo_eight"
        Case 7:PlaySound "vo_seven"
        Case 6:PlaySound "vo_six"
        Case 5:PlaySound "vo_five"
        Case 4:PlaySound "vo_four"
        Case 3:PlaySound "vo_three"
        Case 2:PlaySound "vo_two"
        Case 1:PlaySound "vo_one"
        Case 0:PlaySound "vo_timeisup":StopMode
    End Select
End Sub

Sub BlueTargetsTimer_Timer 'rotates though all the targets
    If NewMode = 8 Then
        BlueTargetsCount = (BlueTargetsCount + 1) MOD 7
    Else                                 'this will be mode 11
        BlueTargetsCount = RndNbr(7) - 1 'from 0 to 6
    End If
    Select Case BlueTargetsCount
        Case 0:Li031.State = 2:Li049.State = 0:Li050.State = 0:Li051.State = 0:Li058.State = 0:Li057.State = 0:Li079.State = 0
        Case 1:Li031.State = 0:Li049.State = 2:Li050.State = 0:Li051.State = 0:Li058.State = 0:Li057.State = 0:Li079.State = 0
        Case 2:Li031.State = 0:Li049.State = 0:Li050.State = 2:Li051.State = 0:Li058.State = 0:Li057.State = 0:Li079.State = 0
        Case 3:Li031.State = 0:Li049.State = 0:Li050.State = 0:Li051.State = 2:Li058.State = 0:Li057.State = 0:Li079.State = 0
        Case 4:Li031.State = 0:Li049.State = 0:Li050.State = 0:Li051.State = 0:Li058.State = 2:Li057.State = 0:Li079.State = 0
        Case 5:Li031.State = 0:Li049.State = 0:Li050.State = 0:Li051.State = 0:Li058.State = 0:Li057.State = 2:Li079.State = 0
        Case 6:Li031.State = 0:Li049.State = 0:Li050.State = 0:Li051.State = 0:Li058.State = 0:Li057.State = 0:Li079.State = 2
    End Select
End Sub

Sub SpinnersTimer_Timer
    Select Case RndNbr(3)
        Case 0:SetLightColor li062, amber, 2:SetLightColor li067, amber, 0:SetLightColor li059, amber, 0
        Case 1:SetLightColor li062, amber, 0:SetLightColor li067, amber, 2:SetLightColor li059, amber, 0
        Case 2:SetLightColor li062, amber, 0:SetLightColor li067, amber, 0:SetLightColor li059, amber, 2
    End Select
End Sub

Sub FollowTheLights_Timer 'rotates though all the targets
    If NewMode = 13 Then
        ArrowsCount = (ArrowsCount + 1) MOD 8
    Else                            'this will be mode 14
        ArrowsCount = RndNbr(8) - 1 'from 0 to 8
    End If
    Select Case ArrowsCount
        Case 0:li062.State = 2:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 0
        Case 1:li062.State = 0:Li064.State = 2:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 0
        Case 2:li062.State = 0:Li064.State = 0:Li053.State = 2:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 0
        Case 3:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 2:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 0
        Case 4:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 2:Li068.State = 0:Li052.State = 0:Li059.State = 0
        Case 5:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 2:Li052.State = 0:Li059.State = 0
        Case 6:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 2:Li059.State = 0
        Case 7:li062.State = 0:Li064.State = 0:Li053.State = 0:Li066.State = 0:Li067.State = 0:Li068.State = 0:Li052.State = 0:Li059.State = 2
    End Select
End Sub

'***********
'  torch
'***********
Dim i1, i2, i3, i4
i1 = 0
i2 = 4
i3 = 4
i4 = 4

Sub torchtimer_timer
    torch1.imageA = "torch" &i1
    torch2.imageA = "torch" &i2
    torch3.imageA = "torch" &i3
    torch4.imageA = "torch" &i4

    i1 = (i1 + 1) MOD 8
    i2 = (i2 + 1) MOD 8    
    i3 = (i3 + 1) MOD 8
    i4 = (i4 + 1) MOD 8

End Sub