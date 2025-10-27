' ****************************************************************
'                       VISUAL PINBALL X
'                 JPSalas Serious Sam Pinball Script
'		plain VPX script using core.vbs for supporting functions
'                         Version 1.4.0
' ****************************************************************

Option Explicit
Randomize




'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = true               ' enable Pinup Player functions for this table
cPuPPack = "DarkCrystal"    ' name of the PuP-Pack / PuPVideos folder for this table

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup Player

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub

Sub pupevent(EventNum)
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

' ******* How to use PUPEvent to trigger / control a PuP-Pack *******

' Usage: pupevent(EventNum)

' EventNum = PuP Exxx trigger from the PuP-Pack

' Example: pupevent 102

' This will trigger E102 from the table's PuP-Pack

' DO NOT use any Exxx triggers already used for DOF (if used) to avoid any possible confusion

'************ PuP-Pack Startup **************

PuPStart(cPuPPack) 'Check for PuP - 

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1
Const SongVolume = 0.5 ' 1 is full volume. Value is from 0 to 1
Const FlexDMDHighQuality = True		' If using RealDMD set to False

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
Const cGameName = "DarkCrystal"
Const TableName = "DarkCrystal"
Const myVersion = "1.4.0"
Const MaxPlayers = 1     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 5  ' limit to 5x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 3   ' usually 3 or 5
Const MaxMultiballs = 5  ' max number of balls during multiballs

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
Dim Jackpot(4)
Dim SuperJackpot
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue(4)
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode

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
Dim bJustStarted
Dim bJackpot

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs
Dim cbRight   'captive ball

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
    Randomize

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 36 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    Set cbRight = New cvpmCaptiveBall
    With cbRight
        .InitCaptive CapTrigger1, CapWall1, Array(CapKicker1, CapKicker1a), 0
        .NailedBalls = 1
        .ForceTrans = .9
        .MinForce = 3.5
        '.CreateEvents "cbRight"
        .Start
    End With
    CapKicker1.CreateSizedBallWithMass BallSize / 2, BallMass

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
    bAttractMode = True
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInLock(1) = 0
    BallsInLock(2) = 0
    BallsInLock(3) = 0
    BallsInLock(4) = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bBonusHeld = False
    bJustStarted = True
    bJackpot = False
    bInstantInfo = False
    ' set any lights for the attract mode
    GiOff
    StartAttractMode
End Sub

'******************
' Captive Ball Subs
'******************
Sub CapTrigger1_Hit:cbRight.TrigHit ActiveBall:End Sub
Sub CapTrigger1_UnHit:cbRight.TrigHit 0:End Sub
Sub CapWall1_Hit:cbRight.BallHit ActiveBall:PlaySoundAtBall "fx_collide":End Sub
Sub CapKicker1a_Hit:cbRight.BallReturn Me:End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
If keycode = StartGameKey Then pupevent 703:pupevent 704
    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        if bFreePlay = False Then DOF 125, DOFOn
        If(Tilted = False) Then
            DMDFlush
            DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
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

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
                        If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
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
                        DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)

'test keys
End Sub

Sub Table1_KeyUp(ByVal keycode)

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

Sub InstantInfo
    DMD CL(0, "INSTANT INFO"), "", "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "JACKPOT VALUE"), CL(1, Jackpot(CurrentPlayer) ), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "SPINNER VALUE"), CL(1, spinnervalue(CurrentPlayer) ), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "BUMPER VALUE"), CL(1, bumpervalue(CurrentPlayer) ), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "BONUS X"), CL(1, BonusMultiplier(CurrentPlayer) ), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "PLAYFIELD X"), CL(1, PlayfieldMultiplier(CurrentPlayer) ), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "LOCKED BALLS"), CL(1, BallsInLock(CurrentPlayer) ), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "LANE BONUS"), CL(1, LaneBonus), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "TARGET BONUS"), CL(1, TargetBonus), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "RAMP BONUS"), CL(1, RampBonus), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "GARTHIM KILLED"), CL(1, MonstersKilled(CurrentPlayer) ), "", eNone, eNone, eNone, 800, False, ""
    DMD CL(0, "HIGHEST SCORE"), CL(1, HighScoreName(0) & " " & HighScore(0) ), "", eNone, eNone, eNone, 800, False, ""
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
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
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
        'Bumper1.Force = 0

        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        'Bumper1.Force = 6
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
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
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
    tmp = INT(RND * 6)
    Select Case tmp
        Case 0:PlaySong "mu_battle1"
        Case 1:PlaySong "mu_battle2"
        Case 2:PlaySong "mu_battle3"
        Case 3:PlaySong "mu_battle4"
        Case 4:PlaySong "mu_battle5"
        Case 5:PlaySong "mu_battle6"
    End Select
End Sub

Sub PlayMultiballSong
    Dim tmp
    tmp = INT(RND * 4)
    Select Case tmp
        Case 0:PlaySong "mu_war1"
        Case 1:PlaySong "mu_war2"
        Case 2:PlaySong "mu_war3"
        Case 3:PlaySong "mu_war4"
    End Select
End Sub

Sub ChangeSong
    If(BallsOnPlayfield = 0) Then
        PlaySong "mu_end"
    Else
        Select Case Battle(CurrentPlayer, 0)
            Case 0
                PlaySong "mu_main"
            Case 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
                PlayBattleSong
            Case 13, 14, 15
                PlayMultiballSong
        End Select
    End If
End Sub

'********************
' Play random quotes
'********************

Sub PlayQuote
    Dim tmp
    tmp = INT(RND * 50) + 1
    PlaySound "quote_" &tmp
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
        If UBound(tmp) = 1 Then 'we have 2 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
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
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 50, , 1000
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

Sub FlashEffect(n)
    Dim ii
    Select case n
        Case 0 ' all off
            LightSeqFlasher.Play SeqAlloff
        Case 1 'all blink
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqBlinking, , 10, 10
        Case 2 'random
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqBlinking, , 5, 10
    End Select
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
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
    tmp = ball.y * 2 / Table1.height-1
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'********************************************
'   JP's VP10 Rolling Sounds + Ballshadow
' uses a collection of shadows, aBallShadow
'********************************************

Const tnob = 20 ' total number of balls
Const lob = 2   'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)

	 '	aBallShadow(b).X = BOT(b).X
	 '	aBallShadow(b).Y = BOT(b).Y

        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 10
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
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz)/17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

'***********************
' Idols follow the ball
'***********************
' they do not move during multiball

Sub aIdolPos_Hit(idx)
    If NOT bMultiBallMode Then
        Idol1.Rotz = -(idx * 2) -32
        Idol2.RotZ = idx * 2 + 32
    End If
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
pupevent 819
pupevent 710
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

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    bSkillShotReady = True

'Change the music ?
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
pupevent 820
pupevent 711
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

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
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

    If NOT Tilted Then

'add in any bonus points (multipled by the bonus multiplier)
'AwardPoints = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer)
'AddScore AwardPoints
'debug.print "Bonus Points = " & AwardPoints
'DMD "", CL(1, "BONUS: " & BonusPoints(CurrentPlayer) & " X" & BonusMultiplier(CurrentPlayer) ), "", eNone, eBlink, eNone, 1000, True, ""

'Count the bonus. This table uses several bonus
'Lane Bonus
        AwardPoints = LaneBonus * 1000
        TotalBonus = AwardPoints
        DMD CL(0, FormatScore(AwardPoints) ), CL(1, "LANE BONUS " & TargetBonus), "", eBlink, eNone, eNone, 800, False, ""

        'Number of Target hits
        AwardPoints = TargetBonus * 2000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints) ), CL(1, "TARGET BONUS " & TargetBonus), "", eBlink, eNone, eNone, 800, False, ""

        'Number of Ramps completed
        AwardPoints = RampBonus * 10000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints) ), CL(1, "RAMP BONUS " & TargetBonus), "", eBlink, eNone, eNone, 800, False, ""

        'Number of Monsters Killed
        AwardPoints = MonstersKilled(CurrentPlayer) * 25000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints) ), CL(1, "GARTHIM KILLED " & TargetBonus), "", eBlink, eNone, eNone, 800, False, ""

        ' calculate the totalbonus
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer) + BonusHeldPoints(CurrentPlayer)

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
        DMD CL(0, FormatScore(TotalBonus) ), CL(1, "TOTAL BONUS " & " X" & BonusMultiplier(CurrentPlayer) ), "", eBlinkFast, eNone, eNone, 1500, True, ""

        AddScore TotalBonus

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 6000, "EndOfBall2 '"
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

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
pupevent 852

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL(0,"EXTRA BALL"), CL(1,"SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, ""

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
pupevent 106

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

    ' set any lights for the attract mode
    GiOff
    StartAttractMode
pupevent 105
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
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then

            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            DMD "_", CL(1, "BALL SAVED"), "_", eNone, eBlinkfast, eNone, 800, True, ""
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1) Then
pupevent 110
                ' AND in a multi-ball??
                If(bMultiBallMode = True) then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
                    ResetJackpotLights
                    Select Case Battle(CurrentPlayer, 0)
                        Case 13, 14, 15:WinBattle
                    End Select
                    ChangeGi white
                    ChangeSong
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
                ' End Mode and timers
pupevent 700
pupevent 701
                ChangeSong
                ChangeGi white
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
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
pupevent 844
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
        DOF 121, DOFPulse
        PlaySoundAt "fx_fire", swPlungerRest
        bAutoPlunger = False
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
    Else
        ' show the message to shoot the ball in case the player has fallen sleep
        swPlungerRest.TimerEnabled = 1
    End If
    'Start the Selection of the skillshot if ready
    If bSkillShotReady Then
        UpdateSkillshot()
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
        ResetSkillShotTimer.Enabled = 1
    End If
    ChangeSong
' turn off LaunchLight
' LaunchLight.State = 0
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds

Sub swPlungerRest_Timer
    DMD "_", CL(1, "SHOOT THE BALL"), "_", eNone, eNone, eNone, 800, True, ""
    swPlungerRest.TimerEnabled = 0
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
' In this table we use SecondRound variable to double the score points in the second round after killing Malthael
Sub AddScore(points)
    If(Tilted = False) Then
        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
    End if
' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If(Tilted = False) Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    End if
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If(Tilted = False) Then

        ' If(bMultiBallMode = True) Then
        Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
        DMD "_", CL(1, "INCREASED JACKPOT"), "_", eNone, eNone, eNone, 800, True, ""
    ' you may wish to limit the jackpot to a upper limit, ie..
    '	If (Jackpot >= 6000) Then
    '		Jackpot = 6000
    ' 	End if
    'End if
    End if
End Sub

Sub AddSuperJackpot(points) 'not used in this table
    If(Tilted = False) Then
    End if
End Sub

Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxMultiplier) then
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
    UPdateBonusXLights(Level)
End Sub

Sub UpdateBonusXLights(Level)
    ' Update the lights
    Select Case Level
        Case 1:light54.State = 0:light55.State = 0:light56.State = 0:light57.State = 0
        Case 2:light54.State = 1:light55.State = 0:light56.State = 0:light57.State = 0
        Case 3:light54.State = 0:light55.State = 1:light56.State = 0:light57.State = 0
        Case 4:light54.State = 0:light55.State = 0:light56.State = 1:light57.State = 0
        Case 5:light54.State = 0:light55.State = 0:light56.State = 0:light57.State = 1
    End Select
End Sub

Sub AddPlayfieldMultiplier(n)
    Dim NewPFLevel
    ' if not at the maximum level x
    if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier) then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
        SetPlayfieldMultiplier(NewPFLevel)
        DMD "_", CL(1, "PLAYFIELD X " &NewPFLevel), "_", eNone, eNone, eNone, 2000, True, "fx_bonus"
    Else 'if the 5x is already lit
        AddScore 50000
        DMD "_", CL(1, "50000"), "_", eNone, eNone, eNone, 2000, True, ""
    End if
    'Start the timer to reduce the playfield x every 30 seconds
    pfxtimer.Enabled = 0
    pfxtimer.Enabled = 1
End Sub

' Set the Playfield Multiplier to the specified level AND set any lights accordingly

Sub SetPlayfieldMultiplier(Level)
    ' Set the multiplier to the specified level
    PlayfieldMultiplier(CurrentPlayer) = Level
    UpdatePFXLights(Level)
End Sub

Sub UpdatePFXLights(Level)
    ' Update the lights
    Select Case Level
        Case 1:light3.State = 0:light2.State = 0:light1.State = 0:light4.State = 0
        Case 2:light3.State = 1:light2.State = 0:light1.State = 0:light4.State = 0
        Case 3:light3.State = 0:light2.State = 1:light1.State = 0:light4.State = 0
        Case 4:light3.State = 0:light2.State = 0:light1.State = 1:light4.State = 0
        Case 5:light3.State = 0:light2.State = 0:light1.State = 0:light4.State = 1
    End Select
' show the multiplier in the DMD
' in this table the multiplier is always shown in the score display sub
End Sub

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        DMD "_", CL(1, ("EXTRA BALL WON") ), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        LightShootAgain.State = 1 'light the shoot again lamp
pupevent 828
        GiEffect 2
        LightEffect 2
    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL(1, ("EXTRA GAME WON") ), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
pupevent 850
    Credits = Credits + 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
    FlashEffect 2
End Sub

Sub AwardJackpot() 'award a normal jackpot, double or triple jackpot
    Dim tmp
    DMD CL(0, FormatScore(Jackpot(CurrentPlayer))), CL(1, "JACKPOT" ), "bkborder", eBlinkFast, eBlinkFast, eNone, 1000, True, ""
    DOF 126, DOFPulse
    tmp = INT(RND * 2)
    Select Case tmp
        Case 0:PlaySound "vo_Jackpot"
        Case 0:PlaySound "vo_Jackpot2"
        Case 0:PlaySound "vo_Jackpot3"
    End Select
pupevent 829
    AddScore Jackpot(CurrentPlayer)
    LightEffect 2
    FlashEffect 2
    'sjekk for superjackpot
    EnableSuperJackpot
End Sub

Sub AwardSuperJackpot() 'this is actually 4 times a jackpot
    SuperJackpot = Jackpot(CurrentPlayer) * 4
    DMD CL(0, FormatScore(SuperJackpot) ), CL(1, "SUPER JACKPOT" ), "bkborder", eBlinkFast, eBlinkFast, eNone, 1000, True, "vo_superjackpot"
    DOF 126, DOFPulse
    AddScore SuperJackpot
    LightEffect 2
    FlashEffect 2
    'enabled jackpots again
    StartJackpots
End Sub

Sub AwardSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL(0, FormatScore(SkillshotValue(CurrentPlayer))), CL(1, ("SKILLSHOT") ), "bkborder", eBlinkFast, eBlink, eNone, 1000, True, ""
    DOF 127, DOFPulse
    PlaySound "fx_fanfare2"
    Addscore SkillShotValue(CurrentPlayer)
pupevent 830
    ' increment the skillshot value with 250.000
    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 250000
    'do some light show
    GiEffect 2
    LightEffect 2
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 100000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 100000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 100000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 100000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(TableName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(TableName, "TotalGamesPlayed")
    If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
    SaveValue TableName, "HighScore1", HighScore(0)
    SaveValue TableName, "HighScore1Name", HighScoreName(0)
    SaveValue TableName, "HighScore2", HighScore(1)
    SaveValue TableName, "HighScore2Name", HighScoreName(1)
    SaveValue TableName, "HighScore3", HighScore(2)
    SaveValue TableName, "HighScore3Name", HighScoreName(2)
    SaveValue TableName, "HighScore4", HighScore(3)
    SaveValue TableName, "HighScore4Name", HighScoreName(3)
    SaveValue TableName, "Credits", Credits
    SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
    HighScoreName(0) = "AAA"
    HighScoreName(1) = "BBB"
    HighScoreName(2) = "CCC"
    HighScoreName(3) = "DDD"
    HighScore(0) = 100000
    HighScore(1) = 100000
    HighScore(2) = 100000
    HighScore(3) = 100000
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

    If tmp> HighScore(1) Then 'add 1 credit for beating the highscore
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
    ChangeSong
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ'<>*+-/=\^0123456789`" ' ` is back arrow
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
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "`") then
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

    if(hsCurrentDigit <1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

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
    ChangeSong
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
                For i = 0 to 35
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
                    Digits(i).Visible = False
                Next
                'digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + i * 12, 6, 12, 22
                Next
                For i = 20 to 35 ' Bottom
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
                For i = 0 to 35
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
                    Digits(i).Visible = False
                Next
                'digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3, 6, 11
                Next
                For i = 20 to 35 ' Bottom
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
    dCharsPerLine(0) = 16 'characters lower line
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
    Dim tmp, tmp1, tmp2
    if(dqHead = dqTail) Then
        tmp = RL(0, FormatScore(Score(Currentplayer) ) )
        'tmp = CL(0, FormatScore(Score(Currentplayer) ) )
        'tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
        'tmp1 = FormatScore(Bonuspoints(Currentplayer) ) & " X" &BonusMultiplier(Currentplayer)

        Select Case Battle(CurrentPlayer, 0)
            Case 0:tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
            Case 1:tmp1 = CL(1, "SPINNERS LEFT " & 200-SpinCount):pupevent 860
            Case 2:tmp1 = CL(1, "BUMPER HITS LEFT " & 100-SuperBumperHIts):pupevent 861
            Case 3:tmp1 = CL(1, "RAMP HITS LEFT " & 10-ramphits):pupevent 862
            Case 4:tmp1 = CL(1, "ORBIT HITS LEFT " & 10-orbithits):pupevent 863
            Case 5:tmp1 = CL(1, "HIT THE LIGHTS"):pupevent 864
            Case 6:tmp1 = CL(1, "HIT THE LIGHTS"):pupevent 865
            Case 7:tmp1 = CL(1, "HIT THE TARGETS " & 50-TargetHits):pupevent 866
            Case 8:tmp1 = CL(1, "HIT THE TARGETS " & 20-TargetHits):pupevent 867
            Case 9:tmp1 = CL(1, "HIT CAPTIVE BALL " & 10-CaptiveBallHits):pupevent 868
            Case 10:tmp1 = CL(1, "HIT RAMPS ORBITS " & 10-RampHits):pupevent 869
            Case 11:tmp1 = CL(1, "HIT BLUE TARGETS " & 20-TargetHits):pupevent 870
            Case 12:tmp1 = CL(1, "HIT THE LOOPS " & 5-loopCount):pupevent 871
            Case 13:tmp1 = CL(1, "HIT THE LIT LIGHT"):pupevent 872
            Case 14:tmp1 = CL(1, "HIT THE LIT LIGHT"):pupevent 873
            Case 15:tmp1 = CL(1, "CONJUNCTION " & "Ball " & Balls)
        End Select
        tmp2 = "bkborder"
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
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
            if(Len(TempStr) <dCharsPerLine(id) ) Then
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
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1) ) + 48) & right(NumString, Len(NumString) - i)
        end if
    Next
    FormatScore = NumString
End function

Function CL(id, NumString)
    Dim Temp, TempStr
    Temp = (dCharsPerLine(id) - Len(NumString) ) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(id, NumString)
    Dim Temp, TempStr
    Temp = dCharsPerLine(id) - Len(NumString)
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
            For digit = 20 to 35
                DMDDisplayChar mid(dLine(0), digit-19, 1), digit
            Next
        Case 1 'bottom text line
            For digit = 0 to 19
                DMDDisplayChar mid(dLine(1), digit + 1, 1), digit
            Next
        Case 2 ' back image - back animations
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "bkempty"
            DigitsBack(0).ImageA = dLine(2)
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

Dim Digits, DigitsBack, Chars(255), Images(255)

DMDInit

Sub DMDInit
    Dim i
        Digits = Array(digit0, digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9, digit10, digit11,                  _
            digit12, digit13, digit14, digit15, digit16, digit17, digit18, digit19, digit20, digit21, digit22, digit23, digit24, digit25, _
            digit26, digit27, digit28, digit29, digit30, digit31, digit32, digit33, digit34, digit35)
        DigitsBack = Array(digit36)
    For i = 0 to 255:Chars(i) = "dempty":Images(i) = "dempty":Next

    Chars(32) = "dempty"
    '    Chars(34) = '"
    '    Chars(36) = '$
    '    Chars(39) = ''
    '    Chars(42) = '*
    '    Chars(43) = '+
    '    Chars(45) = '-
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
    Chars(65) = "da" 'A
    Chars(66) = "db" 'B
    Chars(67) = "dc" 'C
    Chars(68) = "dd" 'D
    Chars(69) = "de" 'E
    Chars(70) = "df" 'F
    Chars(71) = "dg" 'G
    Chars(72) = "dh" 'H
    Chars(73) = "di" 'I
    Chars(74) = "dj" 'J
    Chars(75) = "dk" 'K
    Chars(76) = "dl" 'L
    Chars(77) = "dm" 'M
    Chars(78) = "dn" 'N
    Chars(79) = "do" 'O
    Chars(80) = "dp" 'P
    Chars(81) = "dq" 'Q
    Chars(82) = "dr" 'R
    Chars(83) = "ds" 'S
    Chars(84) = "dt" 'T
    Chars(85) = "du" 'U
    Chars(86) = "dv" 'V
    Chars(87) = "dw" 'W
    Chars(88) = "dx" 'X
    Chars(89) = "dy" 'Y
    Chars(90) = "dz" 'Z
    'Chars(91) = "dball" '[
    'Chars(92) = "dcoin" '|
    'Chars(93) = "dpika" ']
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

'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    RollingUpdate
' add any other real time update subs, like gates or diverters
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
    DMD "", "", "gameover", eNone, eNone, eBlink, 2000, False, ""
    If bFreePlay Then
        DMD "", CL(1, "FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits> 0 Then
            DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "", "", "jppresents", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "SeriousSam", eNone, eNone, eNone, 4000, False, ""
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0) ), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
pupevent 705
    ChangeSong
    StartLightSeq
    DMDFlush
    ShowTableInfo
End Sub

Sub StopAttractMode
pupevent 706
pupevent 707
    DMDScoreNow
    LightSeqAttract.StopPlay
	LightSeqFlasher.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqFlasher.UpdateInterval = 150
    LightSeqFlasher.Play SeqRandom, 10, , 50000
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

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqAllOff
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
Dim LaneBonus
Dim TargetBonus
Dim RampBonus
Dim BumperValue(4)
Dim BumperHits
Dim SuperBumperHits
Dim SpinnerValue(4)
Dim MonstersKilled(4)
Dim SpinCount
Dim RampHits
Dim OrbitHits
Dim TargetHits
Dim CaptiveBallHits
Dim loopCount
Dim BattlesWon(4)
Dim Battle(4, 15) '12 battles, 2 wizard modes, 1 final battle
Dim NewBattle
Dim MachineGunHits

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    'Play some Music
    ChangeSong
    'Init Variables
    LaneBonus = 0 'it gets deleted when a new ball is launched
    TargetBonus = 0
    RampBonus = 0
    BumperHits = 0
    For i = 1 to 4
        SkillshotValue(i) = 500000
        Jackpot(i) = 100000
        MonstersKilled(i) = 0
        BallsInLock(i) = 0
        SpinnerValue(i) = 1000
        BumperValue(i) = 210 'start at 210 and every 30 hits its value is increased by 500 points
    Next
    ResetBattles
    SpinCount = 0
    SuperBumperHits = 0
    RampHits = 0
    OrbitHits = 0
    TargetHits = 0
    CaptiveBallHits = 0
    loopCount = 0
    MachineGunHits = 0
'Init Delays/Timers
'MainMode Init()
'Init lights
End Sub

Sub StopEndOfBallMode() 'this sub is called after the last ball is drained
    ResetSkillShotTimer_Timer
    StopBattle
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
    Dim i
    LaneBonus = 0
    TargetBonus = 0
    RampBonus = 0
    BumperHits = 0
    ' select a battle
    SelectBattle
End Sub

Sub ResetNewBallLights()                                 'turn on or off the needed lights before a new ball is released
    UpdatePFXLights(PlayfieldMultiplier(CurrentPlayer) ) 'ensure the multiplier is displayed right
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
    LightSeqSkillshot.Play SeqAllOff
    Light21.State = 2
    Light29.State = 2
    Gate2.Open = 1
    Gate3.Open = 1
    DMD CL(0, "HIT LIT LIGHT"), CL(1, "FOR SKILLSHOT"), "", eNone, eNone, eNone, 1500, True, ""
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    LightSeqSkillshot.StopPlay
    If Light21.State = 2 Then Light21.State = 0
    Light29.State = 0
    Gate2.Open = 0
    Gate3.Open = 0
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

' Tree animation
Dim MyPi, TreeStep, TreeDir
MyPi = Round(4 * Atn(1), 6) / 90
TreeStep = 0

Sub Trees_Timer()
    TreeDir = SIN(TreeStep * MyPi)
    TreeStep = (TreeStep + 5) MOD 360
    Tree1.RotY = - TreeDir
    Tree2.RotY = TreeDir
    Tree3.RotY = - TreeDir
    Tree4.RotY = TreeDir
    Tree5.RotY = - TreeDir
    Tree6.RotY = TreeDir
    Tree7.RotY = - TreeDir
    Tree8.RotY = TreeDir
    Tree9.RotY = - TreeDir
    Tree10.RotY = TreeDir
    Tree11.RotY = - TreeDir
    Tree12.RotY = TreeDir
    Tree13.RotY = - TreeDir
    'Tree14.RotY = TreeDir
    'Tree15.RotY = - TreeDir
End Sub

'*********************************************************
' Slingshots has been hit
' In this table the slingshots change the outlanes lights

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Lemk
    DOF 105, DOFPulse
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
pupevent 831
    AddScore 210
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
    ChangeOutlanes
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 104, DOFPulse, DOFcontactors), Remk
    DOF 106, DOFPulse
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
pupevent 832
    AddScore 210
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
    ChangeOutlanes
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub ChangeOutlanes
    Dim tmp
    tmp = light5.State
    light5.State = light9.State
    light9.State = tmp
End Sub

'*********
' Bumpers
'*********
' after each 30 hits the bumpers increase their score value by 500 points up to 3210
' and they increase the playfield multiplier. The playfield multiplier is reduced after every 30 seconds

Sub Bumper1_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper1
        DOF 138, DOFPulse
        ' add some points
pupevent 800
        AddScore BumperValue(CurrentPlayer)
        If Battle(CurrentPlayer, 0) = 2 Then
            SuperBumperHits = SuperBumperHits + 1
            Addscore 5000
            CheckWinBattle
        End If
        ' remember last trigger hit by the ball
        LastSwitchHit = "Bumper1"
    End If
    CheckBumpers
End Sub

Sub Bumper2_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 110, DOFPulse, DOFContactors), Bumper2
        DOF 140, DOFPulse
        ' add some points
pupevent 801
        AddScore BumperValue(CurrentPlayer)
        If Battle(CurrentPlayer, 0) = 2 Then
            SuperBumperHits = SuperBumperHits + 1
            Addscore 5000
            CheckWinBattle
        End If
        ' remember last trigger hit by the ball
        LastSwitchHit = "Bumper2"
    End If
    CheckBumpers
End Sub

Sub Bumper3_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper3
        DOF 137, DOFPulse
        ' add some points
pupevent 802
        AddScore BumperValue(CurrentPlayer)
        If Battle(CurrentPlayer, 0) = 2 Then
            SuperBumperHits = SuperBumperHits + 1
            Addscore 5000
            CheckWinBattle
        End If
        ' remember last trigger hit by the ball
        LastSwitchHit = "Bumper3"
    End If
    CheckBumpers
End Sub

' Check the bumper hits

Sub CheckBumpers()
    ' increase the bumper hit count and increase the bumper value after each 30 hits
    BumperHits = BumperHits + 1
    If BumperHits MOD 30 = 0 Then
        If BumperValue(CurrentPlayer) <3210 Then
            BumperValue(CurrentPlayer) = BumperValue(CurrentPlayer) + 500
        End If
        ' lit the playfield multiplier light
        light53.State = 1
    End If
End Sub

'*************************
' Top & Inlanes: Bonus X
'*************************
' lit the 2 top lane lights and the 2 inlane lights to increase the bonus multiplier

Sub sw8_Hit
    DOF 128, DOFPulse
    PlaySoundAtBall "fx_sensor"
pupevent 803
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    Light20.State = 1
    FlashForMs f5, 1000, 50, 0
    If bSkillShotReady Then
        ResetSkillShotTimer_Timer
    Else
        CheckBonusX
    End If
End Sub

Sub sw9_Hit
    DOF 129, DOFPulse
    PlaySoundAtBall "fx_sensor"
pupevent 804
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    Light21.State = 1
    FlashForMs f5, 1000, 50, 0
    If bSkillShotReady Then
        Awardskillshot
    Else
        CheckBonusX
    End If
End Sub

Sub sw2_Hit
    DOF 133, DOFPulse
pupevent 805
    PlaySoundAtBall "fx_sensor"
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    Light6.State = 1
    FlashForMs f8, 1000, 50, 0
    AddScore 5000
    CheckBonusX
'do something
' Do some sound or light effect
End Sub

Sub sw3_Hit
    DOF 134, DOFPulse
    PlaySoundAtBall "fx_sensor"
pupevent 806
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    Light8.State = 1
    FlashForMs f9, 1000, 50, 0
    AddScore 5000
    CheckBonusX
'do something
' Do some sound or light effect
End Sub

Sub CheckBonusX
    If Light20.State + Light21.State + Light6.State + Light8.State = 4 Then
        AddBonusMultiplier 1
        GiEffect 1
        FlashForMs Light20, 1000, 50, 0
        FlashForMs Light21, 1000, 50, 0
        FlashForMs Light6, 1000, 50, 0
        FlashForMs Light8, 1000, 50, 0
    End IF
End Sub

'************************************
' Flipper OutLanes: Virtual kickback
'************************************
' if the light is lit then activate the ballsave

Sub sw1_Hit
    DOF 132, DOFPulse
    PlaySoundAtBall "fx_sensor"
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
pupevent 833
    AddScore 50000
    ' Do some sound or light effect
    ' do some check
    If light5.State = 1 Then
        EnableBallSaver 5
    End If
End Sub

Sub sw4_Hit
    DOF 135, DOFPulse
    PlaySoundAtBall "fx_sensor"
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
pupevent 834
    AddScore 50000
    ' Do some sound or light effect
    ' do some check
    If Light9.State = 1 Then
        EnableBallSaver 5
    End If
End Sub

'*******************************
' 3Bank Targets: Jungle Targets
'*******************************

Sub Target1_Hit
    PlaySoundAt SoundFXDOF("fx_target", 115, DOFPulse, DOFTargets), Target1
    If Tilted Then Exit Sub
    AddScore 5000
pupevent 807
    TargetBonus = TargetBonus + 1
    ' Do some sound or light effect
    Light12.State = 1
    FlashForMs f7, 1000, 50, 0
    ' do some check
    Check3BankTargets
    Select Case Battle(CurrentPlayer, 0)
        Case 5
            If Light44.State = 2 Then
                Light44.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light44.State = 2 Then
                Light50.State = 2
                Light44.State = 0
                Addscore 100000
            End If
        Case 7:TargetHits = TargetHits + 1:Addscore 10000:CheckWinBattle
        Case 8:TargetHits = TargetHits + 1:Addscore 25000:CheckWinBattle
        Case 13
            If Light44.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("100000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light44.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("120000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target1"
End Sub

Sub Target2_Hit
    PlaySoundAt SoundFXDOF("fx_target", 115, DOFPulse, DOFTargets), Target2
    If Tilted Then Exit Sub
pupevent 808
    AddScore 5000
    TargetBonus = TargetBonus + 1
    ' Do some sound or light effect
    Light13.State = 1
    FlashForMs f7, 1000, 50, 0
    ' do some check
    Check3BankTargets
    Select Case Battle(CurrentPlayer, 0)
        Case 5
            If Light44.State = 2 Then
                Light44.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light44.State = 2 Then
                Light44.State = 0
                Light50.State = 2
                Addscore 100000
            End If
        Case 7:TargetHits = TargetHits + 1:Addscore 10000:CheckWinBattle
        Case 8:TargetHits = TargetHits + 1:Addscore 25000:CheckWinBattle
        Case 13
            If Light44.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("100000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light44.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("120000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target2"
End Sub

Sub Target3_Hit
    PlaySoundAt SoundFXDOF("fx_target", 115, DOFPulse, DOFTargets), Target3
    If Tilted Then Exit Sub
pupevent 809
    AddScore 5000
    TargetBonus = TargetBonus + 1
    ' Do some sound or light effect
    Light14.State = 1
    FlashForMs f7, 1000, 50, 0
    ' do some check
    Check3BankTargets
    Select Case Battle(CurrentPlayer, 0)
        Case 5
            If Light44.State = 2 Then
                Light44.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light44.State = 2 Then
                Light50.State = 2
                Light44.State = 0
                Addscore 100000
            End If
        Case 7:TargetHits = TargetHits + 1:Addscore 10000:CheckWinBattle
        Case 8:TargetHits = TargetHits + 1:Addscore 25000:CheckWinBattle
        Case 13
            If Light44.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("100000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light44.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("120000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target3"
End Sub

Sub Check3BankTargets
    If light12.state + light13.state + light14.state = 3 Then
        light12.state = 0
        light13.state = 0
        light14.state = 0
        LightEffect 1
        FlashEffect 2
        Addscore 30000
        If Light5.State = 0 AND Light9.State = 0 Then 'light one of the outlanes lights
            Light5.State = 1
        Else
            Addscore 20000
        End If
        ' increment the spinner value
        spinnervalue(CurrentPlayer) = spinnervalue(CurrentPlayer) + 500
    End If
End Sub

'************
'  Spinners
'************

Sub spinner1_Spin
    If Tilted Then Exit Sub
pupevent 810
    Addscore spinnervalue(CurrentPlayer)
    PlaySoundAt "fx_spinner", spinner1
    DOF 136, DOFPulse
    Select Case Battle(CurrentPlayer, 0)
        Case 1
            Addscore 3000
            SpinCount = SpinCount + 1
            CheckWinBattle
    End Select
End Sub

Sub spinner2_Spin
    If Tilted Then Exit Sub
pupevent 811
    PlaySoundAt "fx_spinner", spinner2
    DOF 124, DOFPulse
    Addscore spinnervalue(CurrentPlayer)
    Select Case Battle(CurrentPlayer, 0)
        Case 1
            Addscore 3000
            SpinCount = SpinCount + 1
            CheckWinBattle
    End Select
End Sub

'*********************************
' 2Bank targets: The Lock Targets
'*********************************

Sub Target10_Hit
    PlaySoundAt SoundFXDOF("fx_target", 116, DOFPulse, DOFTargets), Target10
    If Tilted Then Exit Sub
    AddScore 5000
pupevent 812
    TargetBonus = TargetBonus + 1
    ' Do some sound or light effect
    Light11.State = 1
    FlashForMs f6, 1000, 50, 0
    ' do some check
    Check2BankTargets
    Select Case Battle(CurrentPlayer, 0)
        Case 5
            If Light50.State = 2 Then
                Light50.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light50.State = 2 Then
                Light46.State = 2
                Light50.State = 0
                Addscore 100000
            End If
        Case 7:TargetHits = TargetHits + 1:Addscore 10000:CheckWinBattle
        Case 8:TargetHits = TargetHits + 1:Addscore 25000:CheckWinBattle
        Case 13
            If Light50.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("100000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light50.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("120000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target10"
End Sub

Sub Target11_Hit
    PlaySoundAt SoundFXDOF("fx_target", 116, DOFPulse, DOFTargets), Target11
    If Tilted Then Exit Sub
    AddScore 5000
pupevent 813
    TargetBonus = TargetBonus + 1
    ' Do some sound or light effect
    Light10.State = 1
    FlashForMs f6, 1000, 50, 0
    ' do some check
    Check2BankTargets
    Select Case Battle(CurrentPlayer, 0)
        Case 5
            If Light50.State = 2 Then
                Light50.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light50.State = 2 Then
                Light46.State = 2
                Light50.State = 0
                Addscore 100000
            End If
        Case 7:TargetHits = TargetHits + 1:Addscore 10000:CheckWinBattle
        Case 8:TargetHits = TargetHits + 1:Addscore 25000:CheckWinBattle
        Case 13
            If Light50.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("100000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light50.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("120000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target11"
End Sub

Sub Check2BankTargets
    If light11.state + light10.state = 2 Then
        light11.state = 0
        light10.state = 0
        LightEffect 1
        FlashEffect 1
        Addscore 20000
        If(Light27.State = 0) AND(bMultiballMode = FALSE) Then 'lit the lock light if it is off, rise the lock post and activate the lock switch
            Light27.State = 1
            'PlaySound "vo_lockislit"
            DMD "_", CL(1, "LOCK IS LIT"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
pupevent 851
        ElseIf light52.State = 0 Then 'lit the increase jackpot light if the lock light is lit
            light52.State = 1
        'PlaySound "vo_IncreaseJakpot"
        Else
            Addscore 30000
        End If
    End If
End Sub

'**************************
' The Lock: Main Multiball
'**************************
' the lock is a virtual lock, where the locked balls are simply counted

Sub lock_Hit
    Dim delay
    delay = 500
    StopSound "fx_metalrolling"
    PlaySoundAt "fx_kicker_enter", lock
    If light27.State = 1 Then 'lock the ball
pupevent 845
        BallsInLock(CurrentPlayer) = BallsInLock(CurrentPlayer) + 1
        delay = 4000
        Select Case BallsInLock(CurrentPlayer)
            Case 1:DMD "_", CL(1, "BALL 1 LOCKED"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_ball1locked"
            Case 2:DMD "_", CL(1, "BALL 2 LOCKED"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_ball2locked"
            Case 3:DMD "_", CL(1, "BALL 3 LOCKED"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_ball3locked"
        End Select
        light27.State = 0
        If BallsInLock(CurrentPlayer) = 3 Then 'start multiball
pupevent 846
            vpmtimer.addtimer 2000, "StartMainMultiball '"
        End If
    End If
    If(Battle(CurrentPlayer, NewBattle) = 2) AND(Battle(CurrentPlayer, 0) = 0) Then 'the battle is ready, so start it
        vpmtimer.addtimer 2000, "StartBattle '"
        delay = 6000
    End If
    vpmtimer.addtimer delay, "ReleaseLockedBall '"
End Sub

Sub ReleaseLockedBall 'release locked ball
    FlashForMs f6, 1000, 50, 0
    lockpost.isdropped = 1:PlaySoundAt SoundFXDOF("fx_solenoid", 142, DOFPulse, DOFContactors), lock
    DOF 121, DOFPulse
    lock.kick 190, 4
    lockpost.TimerInterval = 400
    lockpost.TimerEnabled = 1
End Sub

Sub lockpost_Timer
    lockpost.TimerEnabled = 0
    lockpost.isdropped = 0:PlaySoundAt "fx_solenoidoff", lock
End Sub

Sub StartMainMultiball
    AddMultiball 3
    DMD "_", CL(1, "MULTIBALL"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_multiball"
    StartJackpots
    ChangeGi 5
    'reset BallsInLock variable
    BallsInLock(CurrentPlayer) = 0
End Sub

'**********
' Jackpots
'**********
' Jackpots are enabled during the Main multiball and the wizard battles

Sub StartJackpots
    bJackpot = true
    'turn on the jackpot lights
    Select Case Battle(CurrentPlayer, 0)
        Case 13 'first wizard mode
            light28.State = 2
            light30.State = 2
        Case 14 'second wizard mode
            light24.State = 2
            light28.State = 2
            light30.State = 2
        Case 15 'final battle
            light24.State = 2
            light25.State = 2
            light28.State = 2
            light30.State = 2
            light32.State = 2
        Case Else
            If bMultiballMode Then
                light28.State = 2
                light30.State = 2
            End If
    End Select
End Sub

Sub ResetJackpotLights 'when multiball is finished, resets jackpot and superjackpot lights
    bJackpot = False
    light24.State = 0
    light25.State = 0
    light28.State = 0
    light30.State = 0
    light32.State = 0
    light29.State = 0
    light51.State = 0
End Sub

Sub EnableSuperJackpot
    If bJackpot = True Then
        If light24.State + light25.State + light28.State + light30.State + light32.State = 0 Then
            'PlaySound "vo_superjackpotislit"
            light29.State = 2
            light51.State = 2
        End If
    End If
End Sub

'***********************************
' 5Bank Targets:  The Idols Targets
'***********************************

Sub Target4_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 120, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
pupevent 814
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target4"
    ' Do some sound or light effect
    Light15.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits = TargetHits + 1:Addscore 10000:CheckWinBattle
        Case 11:TargetHits = TargetHits + 1:Addscore 50000:CheckWinBattle
    End Select
    Check5BankTargets
End Sub

Sub Target5_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 120, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
pupevent 815
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target5"
    ' Do some sound or light effect
    Light16.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits = TargetHits + 1:Addscore 10000:CheckWinBattle
        Case 11:TargetHits = TargetHits + 1:Addscore 50000:CheckWinBattle
    End Select
    Check5BankTargets
End Sub

Sub Target7_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
pupevent 816
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target7"
    ' Do some sound or light effect
    Light17.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits = TargetHits + 1:Addscore 10000:CheckWinBattle
        Case 11:TargetHits = TargetHits + 1:Addscore 50000:CheckWinBattle
    End Select
    Check5BankTargets
End Sub

Sub Target8_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
pupevent 817
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target8"
    ' Do some sound or light effect
    Light18.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits = TargetHits + 1:Addscore 10000:CheckWinBattle
        Case 11:TargetHits = TargetHits + 1:Addscore 50000:CheckWinBattle
    End Select
    Check5BankTargets
End Sub

Sub Target9_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 114, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
pupevent 818
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target9"
    ' Do some sound or light effect
    Light19.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits = TargetHits + 1:Addscore 10000:CheckWinBattle
        Case 11:TargetHits = TargetHits + 1:Addscore 50000:CheckWinBattle
    End Select
    Check5BankTargets
End Sub

Sub Check5BankTargets
    Dim tmp
    FlashForMs f7, 1000, 50, 0
    FlashForMs f1, 1000, 50, 0
    FlashForMs f2, 1000, 50, 0
    FlashForMs f5, 1000, 50, 0
    FlashForMs f6, 1000, 50, 0
    tmp = INT(RND * 26) + 1
    PlaySoundAtBall "enemy_" &tmp
    ' if all 5 targets are hit then kill a monster & activate the mystery light
    If light15.state + light16.state + light17.state + light18.state + light19.state = 5 Then
        ' kill a monster
        MonstersKilled(CurrentPlayer) = MonstersKilled(CurrentPlayer) + 1
        DMD "", "", "monster_" &tmp, eNone, eNone, eBlink, 1500, True, ""
        LightEffect 1
        FlashEffect 1
        ' Lit the Mystery light if it is off
        If Light61.State = 1 Then
            AddScore 50000
        Else
            Light61.State = 1
            AddScore 25000
        End If
        ' reset the lights
        light15.state = 0
        light16.state = 0
        light17.state = 0
        light18.state = 0
        light19.state = 0
    End If
End Sub

' Playfiel Multiplier timer: reduces the multiplier after 30 seconds

Sub pfxtimer_Timer
    If PlayfieldMultiplier(CurrentPlayer)> 1 Then
        PlayfieldMultiplier(CurrentPlayer) = PlayfieldMultiplier(CurrentPlayer) -1
        SetPlayfieldMultiplier PlayfieldMultiplier(CurrentPlayer)
    Else
        pfxtimer.Enabled = 0
    End If
End Sub

'*****************
'  Captive Target
'*****************

Sub Target6_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 113, DOFPulse, DOFTargets)
pupevent 821
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        Awardskillshot
        Exit Sub
    End If
    AddScore 5000 'all targets score 5000
    ' Do some sound or light effect
    ' do some check
    If(bJackpot = True) AND(light51.State = 2) Then
        AwardSuperJackpot
        light51.State = 0
        light29.State = 0
        StartJackpots
    End If
    Select Case Battle(CurrentPlayer, 0)
        Case 0:SelectBattle 'no battle is active then change to another battle
        Case 9:CaptiveBallHits = CaptiveBallHits + 1:Addscore 25000:CheckWinBattle
    End Select

    ' increase the playfield multiplier for 30 seconds
    If light53.State = 1 Then
        AddPlayfieldMultiplier 1
        light53.State = 0
    End If

    ' increase Jackpot
    If light52.State = 1 Then
        AddJackpot 50000
        light52.State = 0
    End If
End Sub

'****************************
'  Pyramid Hole Hit & Awards
'****************************

Sub PyramidKicker_Hit
    Dim Delay
    Delay = 200
pupevent 822
    PlaySoundAt "fx_kicker_enter", PyramidKicker
    If NOT Tilted Then
        ' do something
        If(bJackpot = True) AND(light24.State = 2) Then
            light24.State = 0
            AwardJackpot
            Delay = 2000
        End If
        If light61.State = 1 Then ' mystery light is lit
            light61.State = 0
            GiveRandomAward
            Delay = 3500
        End If
        If light23.State = 2 Then ' extra ball is lit
            light23.State = 0
            AwardExtraBall
            Delay = 2000
        End If
        Select Case Battle(CurrentPlayer, 0)
            Case 5
                If Light46.State = 2 Then
                    Light46.State = 0
                    Addscore 100000
                    CheckWinBattle
                    Delay = 1000
                End If
            Case 6
                If Light46.State = 2 Then
                    Light45.State = 2
                    Light46.State = 0
                    Addscore 100000
                    Delay = 1000
                End If
            Case 13
                If Light46.State = 2 Then
                    AddScore 100000
                    FlashEffect 3
                    DMD "_", CL(1, FormatScore("100000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
                End If
            Case 14
                If Light46.State = 2 Then
                    AddScore 120000
                    FlashEffect 3
                    DMD "_", CL(1, FormatScore("120000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
                End If
        End Select
    End If
    vpmtimer.addtimer Delay, "PyramidExit '"
End Sub

Sub PyramidExit()
pupevent 823
    FlashForMs electric1, 1000, 40, 0
    FlashForMs electric2, 1000, 30, 0
    FlashForMs electric3, 1000, 20, 0
    FlashForMs electric4,  900,  2, 0
    FlashForMs electric5,  910,  2, 0
    FlashForMs electric6,  920,  2, 0
    FlashForMs electric7,  950, 30, 0
    FlashForMs f3, 1000, 50, 0
    FlashForMs f3a, 1000, 50, 0
    FlashForMs f3a1, 1000, 50, 0
    FlashForMs f3a2, 1000, 50, 0
    FlashForMs f3a3, 1000, 50, 0
    FlashForMs f3a4, 1000, 50, 0
    FlashForMs f3a5, 1000, 50, 0
    PlaySoundAt SoundFXDOF("fx_kicker", 119, DOFPulse, DOFContactors), PyramidKicker
    DOF 121, DOFPulse
    PyramidKicker.kick 180, 35
    PlaySoundAt "fx_cannon", PyramidKicker
End Sub

Sub GiveRandomAward() 'from the Pyramid
    Dim tmp, tmp2

    ' show some random values on the dmd
    DMD CL(0, "CRYSTAL AWARD"), "", "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "PLAYFIELD X"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "BUMPER VALUE"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "EXTRA BALL"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "BONUS X"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "SPINNER VALUE"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "BUMPER VALUE"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "PLAYFIELD X"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "BUMPER VALUE"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL(1, "EXTRA BALL"), "", eNone, eNone, eNone, 50, False, "fx_spinner"

    tmp = INT(RND(1) * 80)
    Select Case tmp
        Case 1, 2, 3, 4, 5, 6 'Lit Extra Ball
            DMD "", CL(1,"EXTRA BALL IS LIT"), "", eNone, eBlink, eNone, 1500, True, "fx_fanfare1"
            light23.State = 2
        Case 7, 8, 13, 14, 15 '100,000 points
            DMD CL(0,"BIG POINTS"), CL(1,"100000"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare1"
            AddScore 100000
        Case 9, 10, 11, 12 'Hold Bonus
            DMD CL(0,"BONUS HELD"), CL(1,"ACTIVATED"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare1"
            bBonusHeld = True
        Case 16, 17, 18 'Increase Bonus Multiplier
            DMD CL(0,"INCREASED"), CL(1,"BONUS X"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare1"
            AddBonusMultiplier 1
        Case 19, 20, 21 'Complete Battle
            If Battle(CurrentPlayer, 0)> 0 AND Battle(CurrentPlayer, 0) <13 Then
                DMD CL(0,"QUEST"), CL(1,"COMPLETED"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare1"
                WinBattle
            Else
                DMD CL(0,"BIG POINTS"), CL(1,"100000"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare1"
                AddScore 100000
            End If
        Case 22, 23, 36, 37, 38 'PlayField multiplier
            DMD CL(0,"INCREASED"), CL(1,"PLAYFIELD X"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare1"
            AddPlayfieldMultiplier 1
        Case 24, 25, 26, 27, 28 '100,000 points
            DMD CL(0,"BIG POINTS"), CL(1,"100000"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare1"
            AddScore 100000
        Case 29, 30, 31, 32, 33, 34, 35 'Increase Bumper value
            BumperValue(CurrentPlayer) = BumperValue(CurrentPlayer) + 500
            DMD CL(0,"BUMPER VALUE"), CL(1,BumperValue(CurrentPlayer)), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare1"
        Case 39, 40, 43, 44 'extra multiball
            DMD CL(0,"EXTRA"), CL(1,"MULTIBALL"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare1"
            AddMultiball 1
        Case 45, 46, 47, 48 ' Ball Save
            DMD CL(0,"BALL SAVE"), CL(1,"ACTIVATED"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare1"
            EnableBallSaver 20
        Case ELSE 'Add a Random score from 10.000 to 100,000 points
            tmp2 = INT((RND) * 9) * 10000 + 10000
            DMD CL(0,"EXTRA POINTS"), CL(1,tmp2), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare1"
            AddScore tmp2
    End Select
End Sub

'*******************
'   The Orbit lanes
'*******************

Sub sw5_Hit
    DOF 130, DOFPulse
pupevent 824
    PlaySoundAtBall "fx_sensor"
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    If(bJackpot = True) AND(light25.State = 2) Then
        light25.State = 0
        AwardJackpot
    End If
    Select Case Battle(CurrentPlayer, 0)
        Case 4:OrbitHits = OrbitHits + 1:Addscore 70000:CheckWinBattle
        Case 5
            If Light45.State = 2 Then
                Light45.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light45.State = 2 Then
                Light49.State = 2
                Light45.State = 0
                Addscore 100000
            End If
        Case 10
            If Light45.State = 2 Then
                RampHits = RampHits + 1
                Light48.State = 2
                Light47.State = 2
                Light45.State = 0
                Light49.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 12
            If LastSwitchHit = "sw6" Then
                LastSwitchHit = ""
                loopCount = loopCount + 1
                Addscore 140000
                CheckWinBattle
            End If
        Case 13
            If Light45.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("100000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light45.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("120000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "sw5"
End Sub

Sub sw6_Hit
    DOF 131, DOFPulse
pupevent 825
    PlaySoundAtBall "fx_sensor"
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    If(bJackpot = True) AND(light32.State = 2) Then
        light32.State = 0
        AwardJackpot
    End If
    Select Case Battle(CurrentPlayer, 0)
        Case 4:OrbitHits = OrbitHits + 1:Addscore 70000:CheckWinBattle
        Case 5
            If Light49.State = 2 Then
                Light49.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light49.State = 2 Then
                Light47.State = 2
                Light49.State = 0
                Addscore 100000
            End If
        Case 10
            If Light49.State = 2 Then
                RampHits = RampHits + 1
                Light48.State = 2
                Light47.State = 2
                Light45.State = 0
                Light49.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 12
            If LastSwitchHit = "sw5" Then
                LastSwitchHit = ""
                loopCount = loopCount + 1
                Addscore 140000
                CheckWinBattle
            End If
        Case 13
            If Light49.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("100000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light49.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("120000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "sw6"
End Sub

'****************
'     Ramps
'****************

Sub LeftRampDone_Hit
    Dim tmp
pupevent 826
    If Tilted Then Exit Sub
    'increase the ramp bonus
    RampBonus = RampBonus + 1
    If(bJackpot = True) AND(light28.State = 2) Then
        light28.State = 0
        AwardJackpot
    End If
    'Machine Gun - left ramp only counts the variable
    MachineGunHits = MachineGunHits + 1
    CheckMachineGun
    'Battles
    Select Case Battle(CurrentPlayer, 0)
        Case 3:RampHits = RampHits + 1:Addscore 100000:CheckWinBattle
        Case 5
            If Light47.State = 2 Then
                Light47.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light47.State = 2 Then
                Light48.State = 2
                Light47.State = 0
                Addscore 100000
            End If
        Case 10
            If Light47.State = 2 Then
                RampHits = RampHits + 1
                Light48.State = 0
                Light47.State = 0
                Light45.State = 2
                Light49.State = 2
                Addscore 100000
                CheckWinBattle
            End If
        Case 13
            If Light47.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("100000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light47.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("120000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case else
            ' play ss quote
            PlayQuote
    End Select
    'check for combos
    if LastSwitchHit = "LeftRampDone" Then
        Addscore jackpot(CurrentPlayer)
        DMD CL(0,"COMBO"), CL(1,jackpot(CurrentPlayer)), "_", eNone, eBlinkFast, eNone, 1000, True, ""
    End If
    LastSwitchHit = "LeftRampDone"
End Sub

Sub RightRampDone_Hit
    Dim tmp
pupevent 827
    If Tilted Then Exit Sub
    'increase the ramp bonus
    RampBonus = RampBonus + 1
    If(bJackpot = True) AND(light30.State = 2) Then
        light30.State = 0
        AwardJackpot
    End If
    'Machine Gun - rightt ramp counts the variable and give the jackpot if light31 is lit
    If light31.State = 2 Then
        DMD CL(0,"MACHINE GUN"), CL(1,jackpot(CurrentPlayer)), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_Jackpot"
        AddScore Jackpot(CurrentPlayer)
        LightEffect 2
        FlashEffect 2
    Else
        MachineGunHits = MachineGunHits + 1
        CheckMachineGun
    End If
    'Battles
    Select Case Battle(CurrentPlayer, 0)
        Case 3:RampHits = RampHits + 1:Addscore 100000:CheckWinBattle
        Case 5
            If Light48.State = 2 Then
                Light48.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light48.State = 2 Then
                Light48.State = 0
                Addscore 100000
                WinBattle
            End If
        Case 10
            If Light48.State = 2 Then
                RampHits = RampHits + 1
                Light48.State = 0
                Light47.State = 0
                Light45.State = 2
                Light49.State = 2
                Addscore 100000
                CheckWinBattle
            End If
        Case 13
            If Light48.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("100000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light48.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(1, FormatScore("120000") ), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case else
            ' play ss quote
            PlayQuote
    End Select

    'check for combos
    if LastSwitchHit = "RightRampDone" Then
        Addscore jackpot(CurrentPlayer)
        DMD CL(0,"COMBO"), CL(1,jackpot(CurrentPlayer)), "_", eNone, eBlinkFast, eNone, 1000, True, ""
    End If
    LastSwitchHit = "RightRampDone"
End Sub

'************************
'       Battles
'************************

' This table has 12 main battles, 2 wizard battles, and a final battle
' you may choose any the 12 main battles you want to play
' the first wizard mode is played after completing 4 battles
' the second wizard battle is played after completing 8 battles
' After completing all 12 battles you play the final battle

' current active battle number is stored in Battle(CurrentPlayer,0)

Sub SelectBattle 'select a new random battle if none is active
    Dim i
    If Battle(CurrentPlayer, 0) = 0 Then
        ' reset the battles that are not finished
        For i = 1 to 15
            If Battle(CurrentPlayer, i) = 2 Then Battle(CurrentPlayer, i) = 0
        Next
        Select Case BattlesWon(CurrentPlayer)
            Case 4:NewBattle = 13:Battle(CurrentPlayer, NewBattle) = 2:UpdateBattleLights:StartBattle  '4 battles = start wizard mode
            Case 9:NewBattle = 14:Battle(CurrentPlayer, NewBattle) = 2:UpdateBattleLights:StartBattle  '8 battles + wizard mode = start 2nd wizard mode
            Case 14:NewBattle = 15:Battle(CurrentPlayer, NewBattle) = 2:UpdateBattleLights:StartBattle '12 battles + 2 wizard modes = start final battle
            Case Else
                NewBattle = INT(RND * 12 + 1)
                do while Battle(CurrentPlayer, NewBattle) <> 0
                    NewBattle = INT(RND * 12 + 1)
                loop
                Battle(CurrentPlayer, NewBattle) = 2
                Light26.State = 2
                UpdateBattleLights
        End Select
    'debug.print "newbatle " & newbattle
    End If
End Sub

' Update the lights according to the battle's state
Sub UpdateBattleLights
    Light38.State = Battle(CurrentPlayer, 1)
    Light7.State = Battle(CurrentPlayer, 2)
    Light36.State = Battle(CurrentPlayer, 3)
    Light40.State = Battle(CurrentPlayer, 4)
    Light33.State = Battle(CurrentPlayer, 5)
    Light37.State = Battle(CurrentPlayer, 6)
    Light42.State = Battle(CurrentPlayer, 7)
    Light34.State = Battle(CurrentPlayer, 8)
    Light39.State = Battle(CurrentPlayer, 9)
    Light43.State = Battle(CurrentPlayer, 10)
    Light35.State = Battle(CurrentPlayer, 11)
    Light41.State = Battle(CurrentPlayer, 12)
    Light64.State = Battle(CurrentPlayer, 13)
    Light65.State = Battle(CurrentPlayer, 14)
    Light58.State = Battle(CurrentPlayer, 15)
End Sub

' Starting a battle means to setup some lights and variables, maybe timers
' Battle lights will always blink during an active battle
Sub StartBattle
    Battle(CurrentPlayer, 0) = NewBattle
    Light26.State = 0
    ChangeSong
    PlaySound "fx_alarm"
    Select Case NewBattle
        Case 1 'Serpent Yards = Super Spinners
            DMD CL(0, "MASTER URSU"), CL(1, "SHOOT THE SPINNERS"), "", eNone, eNone, eNone, 1500, True, ""
            Light45.State = 2
            Light49.State = 2
            SpinCount = 0
        Case 2 'Calakmul = Super Pop Bumpers
            DMD CL(0, "AUGHRA"), CL(1, "HIT THE POP BUMPERS"), "", eNone, eNone, eNone, 1500, True, ""
            Light22.State = 2
            LightSeqBumpers.Play SeqRandom, 10, , 1000
            SuperBumperHits = 0
        Case 3 'Sierra de Chiapas = Ramps
            DMD CL(0, "SHARD"), CL(1, "SHOOT THE RAMPS"), "", eNone, eNone, eNone, 1500, True, ""
            Light47.State = 2
            Light48.State = 2
            RampHits = 0
        Case 4 'Karnak = Orbits
            DMD CL(0, "GARTHIM"), CL(1, "SHOOT THE ORBITS"), "", eNone, eNone, eNone, 1500, True, ""
            OrbitHits = 0
            Light45.State = 2
            Light49.State = 2
        Case 5 'Copán = Shoot the lights 2
            DMD CL(0, "SWAMP"), CL(1, "SHOOT THE LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            Light44.State = 2
            Light45.State = 2
            Light46.State = 2
            Light47.State = 2
            Light48.State = 2
            Light49.State = 2
            Light50.State = 2
        Case 6 'The Pit = Shoot the lights 1
            DMD CL(0, "KIRA"), CL(1, "SHOOT THE LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            Light44.State = 2
        Case 7 'Palenque=  Target Frenzy
            DMD CL(0, "PODLINGS"), CL(1, "SHOOT THE TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetHits = 0
            LightSeqAllTargets.Play SeqRandom, 10, , 1000
        Case 8 'Chichén Itzá = Left & Right Targets
            DMD CL(0, "RUINS"), CL(1, "SHOOT THE TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetHits = 0
            Light44.State = 2
            Light50.State = 2
        Case 9 'Bonampak = Captive Ball
            DMD CL(0, "LAND STRIDERS"), CL(1, "SHOOT CAPTIVE BALL"), "", eNone, eNone, eNone, 1500, True, ""
            CaptiveBallHits = 0
            Light29.State = 2
        Case 10 'Monte Albán = Ramps and Orbits
            'uses the ramphits to count the hits
            DMD CL(0, "SKREESH"), CL(1, "SHOOT RAMPS ORBITS"), "", eNone, eNone, eNone, 1500, True, ""
            RampHits = 0
            Light47.State = 2
            Light48.State = 2
        Case 11 'Uxmal = Blue Targets
            DMD CL(0, "CHAMBER OF LIFE"), CL(1, "SHOOT BLUE TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetHits = 0
            LightSeqBlueTargets.Play SeqRandom, 10, , 1000
        Case 12 'Valley of the Jaguar = Super Loops
            DMD CL(0, "CRYSTAL CHAMBER"), CL(1, "SHOOT THE LOOPS"), "", eNone, eNone, eNone, 1500, True, ""
            loopCount = 0
            Light45.State = 2
            Light49.State = 2
            Gate2.Open = 1
            Gate3.Open = 1
        Case 13 'The Grand Cathedral = Follow the Lights 1
            DMD CL(0, "OBSERVATORY"), CL(1, "SHOOT LIT LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            FollowTheLights.Enabled = 1
            AddMultiball 2
            StartJackpots
            ChangeGi 5
        Case 14 'The City of the Gods = Follow the Lights 2
            DMD CL(0, "ATTACK PODLINGS"), CL(1, "SHOOT LIT LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            FollowTheLights.Enabled = 1
            AddMultiball 2
            StartJackpots
            ChangeGi 5
        Case 15 'Mordekai the Summoner - the final battle
            DMD CL(0, "CONJUNCTION"), CL(1, "SHOOT THE JACKPOTS"), "", eNone, eNone, eNone, 1500, True, ""
            AddMultiball 4
pupevent 842
            StartJackpots
            ChangeGi 5
    End Select
End Sub

' wizard modes can't be won, you simply play them.
' check if the battle is completed
Sub CheckWinBattle
    dim tmp
    tmp = INT(RND * 8) + 1
    PlaySound "fx_thunder" & tmp
    DOF 126, DOFPulse
    LightSeqInserts.StopPlay 'stop the light effects before starting again so they don't play too long.
    LightEffect 3
    Select Case NewBattle
        Case 1
            If SpinCount = 200 Then WinBattle:pupevent 833:End if
        Case 2
            If SuperBumperHits = 100 Then WinBattle:pupevent 834:End if
        Case 3
            If RampHits = 10 Then WinBattle:pupevent 835:End if
        Case 4
            If OrbitHits = 8 Then WinBattle:pupevent 836:End if
        Case 5
            If Light44.State + Light45.State + Light46.State + Light47.State + Light48.State + Light49.State + Light50.State = 0 Then WinBattle:pupevent 837:End if
        Case 6
        Case 7
            If TargetHits = 50 Then WinBattle:pupevent 838:End if
        Case 8
            If TargetHits = 20 Then WinBattle:pupevent 838:End if
        Case 9
            If CaptiveBallHits = 10 Then WinBattle:pupevent 839:End if
        Case 10:
            If RampHits = 10 Then pupevent 840:WinBattle
        Case 11
            If TargetHits = 20 Then WinBattle:pupevent 838:End if
        Case 12
            If loopCount = 5 Then WinBattle:pupevent 841:End if
    End Select
End Sub

Sub StopBattle 'called at the end of a ball
    Dim i
    Battle(CurrentPlayer, 0) = 0
    For i = 0 to 15
        If Battle(CurrentPlayer, i) = 2 Then Battle(CurrentPlayer, i) = 0
    Next
    UpdateBattleLights
    StopBattle2
    NewBattle = 0
End Sub

'called after completing a battle
Sub WinBattle
    Dim tmp
pupevent 843
    BattlesWon(CurrentPlayer) = BattlesWon(CurrentPlayer) + 1
    Battle(CurrentPlayer, 0) = 0
    Battle(CurrentPlayer, NewBattle) = 1
    UpdateBattleLights
    FlashEffect 2
    LightEffect 2
    GiEffect 2
    DMD "", CL(1, "BATTLE COMPLETED"), "_", eNone, eBlinkFast, eNone, 1000, True, "fx_Explosion01"
    DOF 139, DOFPulse
    tmp = INT(RND * 4)
    Select Case tmp
        Case 0:vpmtimer.addtimer 1500, "PlaySound ""vo_excelent"" '"
        Case 1:vpmtimer.addtimer 1500, "PlaySound ""vo_impressive"" '"
        Case 2:vpmtimer.addtimer 1500, "PlaySound ""vo_welldone"" '"
        Case 3:vpmtimer.addtimer 1500, "PlaySound ""vo_YouWon"" '"
    End Select
    StopBattle2
    NewBattle = 0
    SelectBattle 'automatically select a new battle
    ChangeSong
End Sub

Sub StopBattle2
    'Turn off the bomb lights
    Light44.State = 0
    Light45.State = 0
    Light46.State = 0
    Light47.State = 0
    Light48.State = 0
    Light49.State = 0
    Light50.State = 0
    ' stop some timers or reset battle variables
    Select Case NewBattle
        Case 1:SpinCount = 0
        Case 2:Light22.State = 0:LightSeqBumpers.StopPlay:SuperBumperHits = 0
        Case 3:RampHits = 0
        Case 4:OrbitHits = 0
        Case 5:LoopCount = 0
        Case 6:
        Case 7:LightSeqAllTargets.StopPlay:TargetHits = 0
        Case 8:Light44.State = 0:Light50.State = 0:TargetHits = 0
        Case 9:CaptiveBallHits = 0:Light29.State = 0
        Case 10:RampHits = 0
        Case 11:TargetHits = 0:LightSeqBlueTargets.StopPlay
        Case 12:LoopCount = 0:Gate2.Open = 0:Gate3.Open = 0
        Case 13:FollowTheLights.Enabled = 0:SelectBattle
        Case 14:FollowTheLights.Enabled = 0:SelectBattle
        Case 15:ResetBattles:SelectBattle
    End Select
End Sub

Sub ResetBattles
    Dim i, j
    For j = 0 to 4
        BattlesWon(j) = 0
        For i = 0 to 15
            Battle(CurrentPlayer, i) = 0
        Next
    Next
    NewBattle = 0
End Sub

'Extra subs for the battles

Sub LightSeqAllTargets_PlayDone()
    LightSeqAllTargets.Play SeqRandom, 10, , 1000
End Sub

Sub LightSeqBumpers_PlayDone()
    LightSeqBumpers.Play SeqRandom, 10, , 1000
End Sub

Sub LightSeqBlueTargets_PlayDone()
    LightSeqBlueTargets.Play SeqRandom, 10, , 1000
End Sub

' Wizards modes timer
Dim FTLstep:FTLstep = 0

Sub FollowTheLights_Timer
    Light44.State = 0
    Light45.State = 0
    Light46.State = 0
    Light47.State = 0
    Light48.State = 0
    Light49.State = 0
    Light50.State = 0
    Select Case Battle(CurrentPlayer, 0)
        Case 13 '1st wizard mode
            Select case FTLstep
                Case 0:FTLstep = 1:Light44.State = 2
                Case 1:FTLstep = 2:Light45.State = 2
                Case 2:FTLstep = 3:Light46.State = 2
                Case 3:FTLstep = 4:Light47.State = 2
                Case 4:FTLstep = 5:Light48.State = 2
                Case 5:FTLstep = 6:Light49.State = 2
                Case 6:FTLstep = 7:Light50.State = 2
                Case 7:FTLstep = 8:Light49.State = 2
                Case 8:FTLstep = 9:Light48.State = 2
                Case 9:FTLstep = 10:Light47.State = 2
                Case 10:FTLstep = 11:Light46.State = 2
                Case 11:FTLstep = 0:Light45.State = 2
            End Select
        Case 14 '2nd wizard mode
            FTLstep = INT(RND * 7)
            Select case FTLstep
                Case 0:Light44.State = 2
                Case 1:Light45.State = 2
                Case 2:Light46.State = 2
                Case 3:Light47.State = 2
                Case 4:Light48.State = 2
                Case 5:Light49.State = 2
                Case 6:Light50.State = 2
            End Select
    End Select
End Sub

'**********************
' Machine Gun Jackpot
'**********************
' 30 seconds hurry up with jackpots on the right ramp
' uses variable MachineGunHits and the light31

Sub CheckMachineGun
    If light31.State = 0 Then
        If MachineGunHits MOD 10 = 0 Then
            EnableMachineGun
        End If
    End If
End Sub

Sub EnableMachineGun
    ' start the timers
    MachineGunTimerExpired.Enabled = True
    MachineGunSpeedUpTimer.Enabled = True
    ' turn on the light
    Light31.BlinkInterval = 160
    Light31.State = 2
End Sub

Sub MachineGunTimerExpired_Timer()
    MachineGunTimerExpired.Enabled = False
    ' turn off the light
    Light31.State = 0
End Sub

Sub MachineGunSpeedUpTimer_Timer()
    MachineGunSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    Light31.BlinkInterval = 80
    Light31.State = 2
End Sub