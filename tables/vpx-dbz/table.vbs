


Option Explicit
Randomize

Dim b2sstep
b2sstep = 0
'b2sflash.enabled = 0
Dim b2satm

Sub startB2S(aB2S)
	b2sflash.enabled = 1
	b2satm = ab2s
End Sub

Sub b2sflash_timer
    If B2SOn Then
	b2sstep = b2sstep + 1
	Select Case b2sstep
		Case 0
		Controller.B2SSetData b2satm, 0
		Case 1
		Controller.B2SSetData b2satm, 1
		Case 2
		Controller.B2SSetData b2satm, 0
		Case 3
		Controller.B2SSetData b2satm, 1
		Case 4
		Controller.B2SSetData b2satm, 0
		Case 5
		Controller.B2SSetData b2satm, 1
		Case 6
		Controller.B2SSetData b2satm, 0
		Case 7
		Controller.B2SSetData b2satm, 1
		Case 8
		Controller.B2SSetData b2satm, 0
		b2sstep = 0
		b2sflash.enabled = 0
	End Select
    End If
End Sub

' /////////////////
' END b2s 
' well that was quick, tacos anyone?
' /////////////////

Const BallSize = 50
Const BallMass = 1.2

ExecuteGlobal GetTextFile("FPVPX.vbs")
If Err Then MsgBox "you need the fpvpx.vbs for the proper functioning of the table"

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

Const cGameName = "DBZ"


if Table1.ShowDT Then
   D1.visible = 1
 Else
   D1.visible = 0
End If

' Define any Constants
Const TableName = "Dragon Ball Z"
Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 15 ' in seconds
Const MaxMultiplier = 5  ' limit to 5x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 3   ' usually 3 or 5
Const MaxMultiballs = 5  ' max number of balls during multiballs

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
Dim HighScore(10)
Dim HighScoreName(10)
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

' tables variables and Mode init
Dim LaneBonus
Dim TargetBonus
Dim BonusBumper
Dim RampBonus
Dim BumperValue(4)
Dim BumperHits
Dim SuperBumperHits
Dim SpinnerValue(4)
Dim SpinCount
Dim RampHits
Dim OrbitHits
Dim TargetHits
Dim loopCount

Dim Ball

Sub Table1_Init()
    Dim i
    Randomize
    LoadEM


    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 80 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
        .CreateEvents "plungerIM"
    End With

	DisplayB2SText "           GAME OVER            "

'    Kicker5.CreateBall
'    Kicker5.kick 0, 2

	' Display "GAME OVER" message.
	D1.Text = "           GAME OVER            "

'	FixTargetTimer.Enabled = TRUE

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Loadhs





    ' freeplay or coins
    bFreePlay = False 'we dont want coins

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bBonusHeld = False
    bJustStarted = True
    bJackpot = False
    bInstantInfo = False
    ' set any lights for the attract mode
    GiOff
    Saiyan = 0
    LockDiverterOn
'	EndAttractMode
    'StartAttractMode
    vpmtimer.addtimer 4000, "StartAttractMode '"  

    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

	If Credits > 0 Then DOF 128, DOFOn

End Sub


'******
' Keys
'******


Sub Table1_KeyDown(ByVal Keycode)
    If Keycode = AddCreditKey Then
        Credits = Credits + 1
		DOF 128, DOFOn
        If(Tilted = False) Then
            PlaySound "fx_coin"
            DisplayB2SText " CREDITS " &credits
            D1.Text = " CREDITS " &credits
            PlaySound "go"
            If NOT bGameInPlay Then ShowTableInfo:
        End If
    End If

	If KeyCode = RightMagnaSave Then NextTrack
    If keycode = PlungerKey Then
        PlungerIM.AutoFire
       If bBallInPlungerLane Then DOF 109, DOFPulse
    End If

    If bGameInPlay AND NOT Tilted Then
        If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 6:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                Else
                    If(Credits > 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
						If Credits < 1 Then DOF 128, DOFOff
                    Else
                        ' Not Enough Credits to start a game.

                        'DMD CenterLine(0, "CREDITS " & Credits), CenterLine(1, "INSERT COIN"), 0, eNone, eBlink, eNone, 500, True, ""
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
							If Credits < 1 Then DOF 128, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        D1.Text = " CREDITS " &credits&"   INSERT COIN"
                        DisplayB2SText " CREDITS " &credits &"   INSERT COIN "
                    End If
                End If
            End If
    End If ' If (GameInPlay)

    If hsbModeActive Then EnterHighScoreKey(keycode)

' Table specific
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then SolLFlipper 0
        If keycode = RightFlipperKey Then SolRFlipper 0
    End If
End Sub


Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = False
    bInstantInfo = True
    DMDFlush
    UltraDMDTimer.Enabled = 1
End Sub

Sub InstantInfo
    Jackpot = 10000 + Round(Score(CurrentPlayer) / 10, 0)
    D1.Text = "INSTANT INFO"
    D1.Text = "JACKPOT   " & Jackpot
    D1.Text = "BONUS MULT   " & BonusMultiplier(CurrentPlayer)
    D1.Text = "RAMP BONUS   " & RampBonus
    DisplayB2SText "         INSTANT INFO           "
    DisplayB2SText "           JACKPOT              " & Jackpot
    DisplayB2SText "         BONUS MULT             " & BonusMultiplier(CurrentPlayer)
    DisplayB2SText "         RAMP BONUS             " & RampBonus
 '   DMD "black.jpg", "", "INSTANT INFO", 500
 '   DMD "black.jpg", "JACKPOT", Jackpot, 800
 '   DMD "black.jpg", "LEVEL", Level(CurrentPlayer), 800
 '   DMD "black.jpg", "BONUS MULT", BonusMultiplier(CurrentPlayer), 800
 '   DMD "black.jpg", "ORBIT BONUS", OrbitHits, 800
  '  DMD "black.jpg", "LANE BONUS", LaneBonus, 800
   ' DMD "black.jpg", "TARGET BONUS", TargetBonus, 800
  '  DMD "black.jpg", "RAMP BONUS", RampBonus, 800
 '   DMD "black.jpg", "MONSTERS KILLED", Monsters(CurrentPlayer), 800
End Sub


'*************
' Music Table
'*************




Dim musicNum
musicNum = int ( rnd * 34)
NextTrack
Sub NextTrack

If musicNum = 0 Then PlayMusic "DBZ/Dragon Ball 01.mp3" End If
If musicNum = 1 Then PlayMusic "DBZ/Dragon Ball 02.mp3" End If
If musicNum = 2 Then PlayMusic "DBZ/Dragon Ball 03.mp3" End If
If musicNum = 3 Then PlayMusic "DBZ/Dragon Ball 04.mp3" End If
If musicNum = 4 Then PlayMusic "DBZ/Dragon Ball 05.mp3" End If
If musicNum = 5 Then PlayMusic "DBZ/Dragon Ball 06.mp3" End If
If musicNum = 6 Then PlayMusic "DBZ/Dragon Ball 07.mp3" End If
If musicNum = 7 Then PlayMusic "DBZ/Dragon Ball 08.mp3" End If
If musicNum = 8 Then PlayMusic "DBZ/Dragon Ball 09.mp3" End If
If musicNum = 9 Then PlayMusic "DBZ/Dragon Ball 10.mp3" End If
If musicNum = 10 Then PlayMusic "DBZ/Dragon Ball 11.mp3" End If
If musicNum = 11 Then PlayMusic "DBZ/Dragon Ball 12.mp3" End If
If musicNum = 12 Then PlayMusic "DBZ/Dragon Ball 13.mp3" End If
If musicNum = 13 Then PlayMusic "DBZ/Dragon Ball 14.mp3" End If
If musicNum = 14 Then PlayMusic "DBZ/Dragon Ball 15.mp3" End If
If musicNum = 15 Then PlayMusic "DBZ/Dragon Ball 16.mp3" End If
If musicNum = 16 Then PlayMusic "DBZ/Dragon Ball 17.mp3" End If
If musicNum = 17 Then PlayMusic "DBZ/Dragon Ball 18.mp3" End If
If musicNum = 18 Then PlayMusic "DBZ/Dragon Ball 19.mp3" End If
If musicNum = 19 Then PlayMusic "DBZ/Dragon Ball 20.mp3" End If
If musicNum = 20 Then PlayMusic "DBZ/Dragon Ball 21.mp3" End If
If musicNum = 21 Then PlayMusic "DBZ/Dragon Ball 22.mp3" End If
If musicNum = 22 Then PlayMusic "DBZ/Dragon Ball 23.mp3" End If
If musicNum = 23 Then PlayMusic "DBZ/Dragon Ball 24.mp3" End If
If musicNum = 24 Then PlayMusic "DBZ/Dragon Ball 25.mp3" End If
If musicNum = 25 Then PlayMusic "DBZ/Dragon Ball 26.mp3" End If
If musicNum = 26 Then PlayMusic "DBZ/Dragon Ball 27.mp3" End If
If musicNum = 27 Then PlayMusic "DBZ/Dragon Ball 28.mp3" End If
If musicNum = 28 Then PlayMusic "DBZ/Dragon Ball 29.mp3" End If
If musicNum = 29 Then PlayMusic "DBZ/Dragon Ball 30.mp3" End If
If musicNum = 30 Then PlayMusic "DBZ/Dragon Ball 31.mp3" End If
If musicNum = 31 Then PlayMusic "DBZ/Dragon Ball 32.mp3" End If
If musicNum = 32 Then PlayMusic "DBZ/Dragon Ball 33.mp3" End If
If musicNum = 33 Then PlayMusic "DBZ/Dragon Ball 34.mp3" End If

musicNum = (musicNum + 1) mod 35
End Sub

Sub table1_MusicDone
        NextTrack
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
    If B2SOn Then Controller.Stop
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
	startB2S(4)
    If Enabled Then
        PlaySound SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), 0, 1, -0.05, 0.15
        LeftFlipper.RotateToEnd
        RotateLaneLightsLeft
        RotateLaneLightsLeftUp

    Else
        PlaySound SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), 0, 1, -0.05, 0.15
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
	startB2S(4)
    If Enabled Then
        PlaySound SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), 0, 1, 0.05, 0.15
        RightFlipper.RotateToEnd
        RotateLaneLightsRight
        RotateLaneLightsRightUp

    Else
        PlaySound SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), 0, 1, 0.05, 0.15
        RightFlipper.RotateToStart
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.05, 0.25
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.05, 0.25
End Sub





Sub RotateLaneLightsLeftUp
    Dim TempState
    TempState = L11.State
    l11.State = l12.State
    l12.State = L13.State
    L13.State = TempState
End Sub

Sub RotateLaneLightsRightUp
    Dim TempState
    TempState = L13.State
    L13.State = l12.State
    L12.State = L11.State
    L11.State = TempState
End Sub


Sub RotateLaneLightsLeft
    Dim TempState
    TempState = l14.State
    l14.State = l15.State
    l15.State = l16.State
    l16.State = l17.State
    l17.State = TempState
End Sub

Sub RotateLaneLightsRight
    Dim TempState
    TempState = l17.State
    l17.State = l16.State
    l16.State = l15.State
    l15.State = l14.State
    l14.State = TempState
End Sub




'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
        'DMDFlush
        'DMD "", " ", "CAREFUL!", 800
        DisplayB2SText "         CAREFUL!               "
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        'DMDFlush
        DisplayB2SText "     TILT!                      "
       ' DMD "", " ", "TILT!", 99999
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

        LeftSlingShot.Disabled = 1
        RightSlingShot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper1.Force = 7
        Bumper2.Force = 7
        Bumper3.Force = 7
        LeftSlingShot.Disabled = 0
        RightSlingShot.Disabled = 0
        'clean up the buffer display
       ' DMDFlush
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
            If Song = "DBZ_end" Then
                PlaySound Song, 0, 0.1  'this last number is the volume, from 0 to 1
            Else
                PlaySound Song, -1, 0.1 'this last number is the volume, from 0 to 1
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
        If UBound(tmp) = -1 Then 'we have 4 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    DOF 126, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    PlaySound "fx_relay_on"
    Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
End Sub

Sub GiOff
    DOF 126, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    PlaySound "fx_relay_off"
    Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSatDark"
End Sub



' GI & light sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 10, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'upon
            LightSeqGi.UpdateInterval = 4
            LightSeqGi.Play SeqUpOn, 5, 1
        Case 4 ' left-right-left
            LightSeqGi.UpdateInterval = 5
            LightSeqGi.Play SeqLeftOn, 10, 1
            LightSeqGi.UpdateInterval = 5
            LightSeqGi.Play SeqRightOn, 10, 1
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'upon
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 10, 1
        Case 4 ' left-right-left
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqLeftOn, 10, 1
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqRightOn, 10, 1
    End Select
End Sub

' Flasher Effects using lights

Dim FEStep, FEffect
FEStep = 0
FEffect = 0

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
        Case 3 'upon
            LightSeqFlasher.UpdateInterval = 4
            LightSeqFlasher.Play SeqUpOn, 10, 1
        Case 4 ' left-right-left
            LightSeqFlasher.UpdateInterval = 5
            LightSeqFlasher.Play SeqLeftOn, 10, 1
            LightSeqFlasher.UpdateInterval = 5
            LightSeqFlasher.Play SeqRightOn, 10, 1
    End Select
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 1500)
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
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 20 ' total number of balls
Const lob = 0   'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)
        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) * 50
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, ballpitch, 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub





'*********** BALL SHADOW *********************************
Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6, BallShadow7)

Sub BallShadowUpdate()
    Dim BOT, b
    BOT = GetBalls
	' render the shadow for each ball
    For b = 0 to Ubound(BOT)
		If BOT(b).X < Table1.Width/2 Then
			BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/10)) + 10
		Else
			BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/10)) - 10
		End If
	    ballShadow(b).Y = BOT(b).Y + 15
		If BOT(b).Z > 20 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
End Sub



'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySound "fx_MetalHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_postrubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_plastichit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub



' Ramp Soundss
Sub RHelp_Hit()
    StopSound "fx_metalrolling"
    PlaySound "fx_balldrop", 0, 1, pan(ActiveBall)
End Sub

Sub LHelp1_Hit()
    StopSound "fx_metalrolling"
    PlaySound "fx_balldrop", 0, 1, pan(ActiveBall)
End Sub

Sub RHelp1_Hit()
    StopSound "fx_metalrolling"
    PlaySound "DBZ_teleport", 0, 1, pan(ActiveBall)
End Sub

Sub Trigger2_Hit()
    PlaySound "fx_metalrolling", 0, 1, pan(ActiveBall)
End Sub





'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    RollingUpdate
    BallShadowUpdate
    LockDiverterP.objRotY = LockDiverter.CurrentAngle
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
            n.color = RGB(0, 18, 0)
            n.colorfull = RGB(0, 255, 0)
        Case blue
            n.color = RGB(0, 18, 18)
            n.colorfull = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 8, 8)
            n.colorfull = RGB(0, 64, 64)
        Case purple
            n.color = RGB(128, 0, 128)
            n.colorfull = RGB(255, 0, 255)
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

Sub ResetAllLightsColor ' Called at a new game
    'shoot again
    SetLightColor ShootAgainLight, amber, -1
End Sub

Sub UpdateBonusColors
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
    on Error Resume Next
    Dim i
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1) Then
        DMD "", "PLAYER 1", Score(1), 3000
    End If
    If Score(2) Then
        DMD "", "PLAYER 2", Score(2), 3000
    End If
    If Score(3) Then
        DMD "", "PLAYER 3", Score(3), 3000
    End If
    If Score(4) Then
        DMD "", "PLAYER 4", Score(4), 3000
    End If


    'coins or freeplay
    If bFreePlay Then
        DMD " ", "FREE PLAY", 2000
        DMD "IntroMS.wmv", "", "", 24500
    Else
        If Credits> 0 Then
            DMD "", "CREDITS " &credits, "PRESS START", 2000
        Else
            DMD "", "CREDITS " &credits, "INSERT COIN", 2000
        End If
        DMD "IntroMS.wmv", "", "", 24500
    End If
    ' some info about the table
    DMD "", "Oly,Javier And Pinwizkid", "PRESENTS", 3000
    DMD "", "", " Metal Slug ", 3000
    ' Highscores
    DMD "", "HIGHSCORE 1", HighScoreName(0) & " " & HighScore(0), 3000
    DMD "", "HIGHSCORE 2", HighScoreName(1) & " " & HighScore(1), 3000
    DMD "", "HIGHSCORE 3", HighScoreName(2) & " " & HighScore(2), 3000
    DMD "", "HIGHSCORE 4", HighScoreName(3) & " " & HighScore(3), 3000
End Sub



Sub StartAttractMode()
    bAttractMode = True
    StartLightSeq
    DisplayB2SText ""
    DMDUpdate.enabled = 0
    AttractMessagesTimer.Enabled = TRUE
End Sub

Sub StopAttractMode()
    bAttractMode = False
    LightSeqAttract.StopPlay
    LightSeqFlasher.StopPlay
    AttractMessagesTimer.Enabled = FALSE
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqFlasher.UpdateInterval = 150
    LightSeqFlasher.Play SeqRandom, 10, , 50000
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

End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqCircleOutOn
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init

End Sub




Sub Game_Init() 'called at the start of a new game
    Dim i
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    BumperHit = 200
    scoopLit.state = 1
    kamehouseLit.state = 1
    BallsRestart
    PinDiverterL.Isdropped = 1
    PinDiverterR.Isdropped = 1
    
    PlaySong "DBZ_GamePlay"
    LightSeqAllBalls.Play SeqLeftOn, 50, 1
End Sub

Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
End Sub


Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub






'-----------------------------
'-----  FS Display Code  -----
'-----------------------------

'If You want to hide a display, set the reel value of every reel to 44. This picture is transparent
'This is best done using collection:
'
'	If HideDisplay then 
'		For Each obj in ReelsCollection:obj.setvalue(44):next
'	end if
 

Dim Char(32),i,TempText                     'increase dimension if You need larger displays



'-----------------------------------------------
'-----  B2S section, not used in the demo  -----
'-----------------------------------------------

Sub DisplayB2SText(TextPar)							'Procedure to display Text on a 30 digit B2S LED reel. Assuming that it is display 1 with internal digit numbers 1-30

	TempText = TextPar		
	for i = 1 to 32
		if i <= len(TextPar) then
			Char(i) = left(TempText,1)
			TempText = right(Temptext,len(TempText)-1)		
		else
			Char(i) = " "
		end if
	next

	for i = 1 to 32
		controller.B2SSetLED i,B2SLEDValue(Char(i))
	next


End Sub

Function B2SLEDValue(CharPar)						'to be used with dB2S 15-segments-LED used in Herweh's Designer
	B2SLEDValue = 0									'default for unknown characters
	select case CharPar
		Case "","":	B2SLEDValue = 0
		Case "0":	B2SLEDValue = 63	
		Case "1":	B2SLEDValue = 8704
		Case "2":	B2SLEDValue = 2139
		Case "3":	B2SLEDValue = 2127	
		Case "4":	B2SLEDValue = 2150
		Case "5":	B2SLEDValue = 2157
		Case "6":	B2SLEDValue = 2172
		Case "7":	B2SLEDValue = 7
		Case "8":	B2SLEDValue = 2175
		Case "9":	B2SLEDValue = 2159
		Case "A":	B2SLEDValue = 2167
		Case "B":	B2SLEDValue = 10767
		Case "C":	B2SLEDValue = 57
		Case "D":	B2SLEDValue = 8719
		Case "E":	B2SLEDValue = 121
		Case "F":	B2SLEDValue = 2161
		Case "G":	B2SLEDValue = 2109
		Case "H":	B2SLEDValue = 2166
		Case "I":	B2SLEDValue = 8713
		Case "J":	B2SLEDValue = 31
		Case "K":	B2SLEDValue = 5232
		Case "L":	B2SLEDValue = 56
		Case "M":	B2SLEDValue = 1334
		Case "N":	B2SLEDValue = 4406
		Case "O":	B2SLEDValue = 63
		Case "P":	B2SLEDValue = 2163
		Case "Q":	B2SLEDValue = 4287
		Case "R":	B2SLEDValue = 6259
		Case "S":	B2SLEDValue = 2157
		Case "T":	B2SLEDValue = 8705
		Case "U":	B2SLEDValue = 62
		Case "V":	B2SLEDValue = 17456
		Case "W":	B2SLEDValue = 20534
		Case "X":	B2SLEDValue = 21760
		Case "Y":	B2SLEDValue = 9472
		Case "Z":	B2SLEDValue = 17417
		Case "<":	B2SLEDValue = 5120
		Case ">":	B2SLEDValue = 16640
		Case "^":	B2SLEDValue = 17414
		Case ".":	B2SLEDValue = 8
		Case "!":	B2SLEDValue = 0
		Case ".":	B2SLEDValue = 128
		Case "*":	B2SLEDValue = 32576
		Case "/":	B2SLEDValue = 17408
		Case "\":	B2SLEDValue = 4352
		Case "|":	B2SLEDValue = 8704
		Case "=":	B2SLEDValue = 2120
		Case "+":	B2SLEDValue = 10816
		Case "-":	B2SLEDValue = 2112
	end select			
	B2SLEDValue = cint(B2SLEDValue)
End Function







Sub DisplayScore
  If Score(1) < 1000000 Then
	 DisplayB2SText cstr(Score(1)) & "" & "                   BALL " & Ball
	Else
	 DisplayB2SText cstr(Score(1)) & (Score(1)) & String(32 - Len(Score(1)), " ") 
  End If
End Sub


Sub DisplayUpdate
	TempText = TextPar		
	for i = 1 to 32
		if i <= len(TextPar) then
			Char(i) = left(TempText,1)
			TempText = right(Temptext,len(TempText)-1)		
		else
			Char(i) = " "
		end if
	next

	if B2SOn Then
	for i = 1 to 32
		controller.B2SSetLED i,B2SLEDValue(Char(i))
	next   
	end if
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
    If(x <> "") then Credits = CInt(x) Else Credits = 0 End If

    'x = LoadValue(TableName, "Jackpot")
    'If(x <> "") then Jackpot = CDbl(x) Else Jackpot = 200000 End If
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
    'SaveValue TableName, "Jackpot", Jackpot
    SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
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
    DMDUpdate.enabled = 0
    D1.Text = " "
    DisplayB2SText " "
    Dim tmp
    tmp = Score(1)

    If Score(2)> tmp Then tmp = Score(2)
    If Score(3)> tmp Then tmp = Score(3)
    If Score(4)> tmp Then tmp = Score(4)

    If tmp> HighScore(1) Then 'add 1 credit for beating the highscore
        AwardSpecial()
    End If

    If tmp> HighScore(3) Then
        vpmtimer.addtimer 2000, "PlaySound ""vo_contratulationsgreatscore"" '"
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    PlaySound "vo_enteryourinitials"
    hsLetterFlash = 0

    hsEnteredDigits(0) = "A"
    hsEnteredDigits(1) = "A"
    hsEnteredDigits(2) = "A"
    hsCurrentDigit = 0

    hsValidLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ<+-0123456789" ' < is used to delete the last letter
    hsCurrentLetter = 1
   ' DMDFlush
    D1.text = "YOUR NAME:" & " "
    'DMDId "hsc", "", "YOUR NAME:", " ", 999999
    HighScoreDisplayName()
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        Playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayName()
    End If

    If keycode = RightFlipperKey Then
        Playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayName()
    End If

    If keycode = StartGameKey OR keycode = PlungerKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayName()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit> 0) then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayName()
        end if
    end if
End Sub

Sub HighScoreDisplayName()
    DMDUpdate.enabled = 0
    Dim i, TempStr
    D1.text = TempStr
    DisplayB2SText "" & TempStr
    TempStr = " >"
    if(hsCurrentDigit> 0) then TempStr = TempStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) then TempStr = TempStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempStr = TempStr & "_"
            DisplayB2SText TempStr & "_"
            D1.TEXT = TempStr & "_"
        else
            TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
            DisplayB2SText TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
            D1.TEXT = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempStr = TempStr & hsEnteredDigits(2)

    TempStr = TempStr & "< "
   ' DMDMod "hsc", "YOUR NAME:", Mid(TempStr, 2, 5), 999999
    DisplayB2SText "ENTER YOUR NAME: " & Mid(TempStr, 2, 5)
    D1.TEXT = "YOUR NAME:" & Mid(TempStr, 2, 5)
End Sub

Sub HighScoreCommitName()
    hsbModeActive = False
    'PlaySong "m_end"
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
   ' DMDFlush
    DMDUpdate.enabled = 1
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







' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True
    startB2S(27)

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
    Next

    letsGo.enabled = 1

    ' initialise any other flags
    bMultiBallMode = False
    Tilt = 0
    LockL = 0
    LockR = 0
    LockDiverterOff
    LockPinOff
    shenlong = 0
    Saiyan = 0
    SaiyanMultiball = False
    TargetsModeOn = False
    LockDiverterEnabled = False
    ExtraBallIsIit = False
    Drain1.enabled = 0
    Drain.enabled = 1
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


    ' reduce the playfield multiplier
    ' reset any drop targets, lights, game Mode etc..

    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False
    ResetNewBallLights()
    'Reset any table specific
    

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    bSkillShotReady = True

'Change the music ?
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedball BallSize / 2

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySound "BallRelease", 0, 1, 0.1, 0.1
    BallRelease.Kick 90, 4

' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield> 1 Then
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

    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

    If NOT Tilted Then
        DMDUpdate.enabled = 0
        EndOfBallTimerBonus.Enabled = 1
        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 6000, "EndOfBall2 '"
    Else 'if tilted then only add a short delay
        vpmtimer.addtimer 100, "EndOfBall2 '"
    End If
End Sub








Dim BonusD
Sub EndOfBallTimerBonus_Timer()
    Dim AwardPoints, TotalBonus, ii 
    AwardPoints = 0
    TotalBonus = 0

    DMDUpdate.enabled = 0
    DisplayB2SText " "
	BonusD = BonusD + 1
	Select Case BonusD
		Case 0:'Number of Target hits
               AwardPoints = TargetBonus * 50
               TotalBonus = TotalBonus + AwardPoints
               DisplayB2SText "  TARGET BONUS:            " & AwardPoints
                PlaySound "DBZ_teleport"

		Case 1:'Number of Bumper hits
               AwardPoints = BonusBumper * 10
               TotalBonus = TotalBonus + AwardPoints
               DisplayB2SText "  BUMPER BONUS:            " & AwardPoints
                PlaySound "DBZ_teleport"

		Case 2:'Lane Bonus
                AwardPoints = LaneBonus * 10
                TotalBonus = TotalBonus + AwardPoints
                DisplayB2SText "  LANE BONUS:              " & AwardPoints
                PlaySound "DBZ_teleport"

		Case 3:'Number of Ramps completed
               AwardPoints = RampBonus * 50
               TotalBonus = TotalBonus + AwardPoints
               DisplayB2SText "  RAMP BONUS                " & AwardPoints
                 PlaySound "DBZ_teleport"

		Case 4:'calculate the totalbonus
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer) + BonusHeldPoints(CurrentPlayer)
            If Balls = BallsPerGame Then ' this is the last ball, so if bonus held has been awarded then double the bonus
                TotalBonus = TotalBonus * 2
            End If

        ' Add the bonus to the score
               DisplayB2SText "  TOTAL BONUS        "&"     X "& BonusMultiplier(CurrentPlayer)
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        'AddScore TotalBonus
        vpmtimer.addtimer 1000, "AddScore TotalBonus '"
                 PlaySound "DBZ_teleport"
               BonusD = 0
               EndOfBallTimerBonus.Enabled = 0
   End Select



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

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            LightExtraBall.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
       ' DMD " ", "EXTRA BALL", 2000

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
	D1.Text = "               GAME OVER                "
    DisplayB2SText "           GAME OVER            "

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()
       ' PlaySong "DBZ_GamePlay"
        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            PlaySound "vo_player" &CurrentPlayer
            'DMD " ", "PLAYER " &CurrentPlayer, 800
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
        'ChangeSong
    End If

   PlaySong "DBZ_End"

    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0


    ' terminate all Mode - eject locked balls
    LockPinOff

    ' most of the Mode/timers terminate at the end of the ball
    Drain.enabled = 0
    Drain1.enabled = 1    
    DMDUpdate.enabled = 0

    ' show game over on the DMD
	D1.Text = "               GAME OVER                "
    DisplayB2SText "           GAME OVER            "
    AttractMessagesTimerUserData = 16

    ' set any lights for the attract mode
    GiOff
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





Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        'DMD " ", "EXTRA BALL", 2000
        D1.text = "EXTRA BALL"
        PlaySound "vo_extraball"
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        GiEffect 2
        LightEffect 2
    END If
End Sub

Sub AwardSpecial()
    'DMD " ", "EXTRA GAME", 2000
    D1.text = "EXTRA GAME"
    PlaySound "fx_fanfare2"
    Credits = Credits + 1
    LightEffect 2
    FlashEffect 2
End Sub

Sub AwardJackpot() 'award a normal jackpot, double or triple jackpot
   ' DMD "JACKPOT", Jackpot(CurrentPlayer), 2000
    D1.text = "JACKPOT" & Jackpot(CurrentPlayer)
    DisplayB2SText "       JACKPOT            " & Jackpot(CurrentPlayer)
    PlaySound "kamehameha"
    AddScore Jackpot(CurrentPlayer)
    LightEffect 2
    FlashEffect 2
    JackpotMSNTimer.enabled = 1
End Sub

Sub AwardSuperJackpot() 'this is actually 4 times a jackpot
    SuperJackpot = Jackpot(CurrentPlayer) * 4
   ' DMD "SUPER JACKPOT", SuperJackpot, 2000
    D1.text = "SUPER JACKPOT"
    DisplayB2SText "    SUPER JACKPOT         " 
    PlaySound "vo_Jackpot2"
    AddScore SuperJackpot
    LightEffect 2
    FlashEffect 2
    'enabled jackpots again
    StartJackpots
End Sub

Sub AwardSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMDFlush
    DMD "SKILLSHOT", SkillShotValue(CurrentPlayer), 2000
    PlaySound "fx_fanfare2"
    ' increment the skillshot value with 250.000
    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 25000
    'do some light show
    GiEffect 2
    LightEffect 2
End Sub


Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxMultiplier) then
        ' then add and set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
        SetBonusMultiplier(NewBonusLevel)
        DisplayB2SText "  BONUS X      " & NewBonusLevel
        D1.text = "BONUS X " &NewBonusLevel
        PlaySound "fx_bonux"
    Else
        DisplayB2SText "              5000              "
        D1.text = "        5000           " 
    End if
End Sub


' *********************************************************************
' **  SCORING FUNCTIONS                                              **
' *********************************************************************



' Add points to the score.
'
Sub AddScore(points)
	If Not(Tilted) Then
		' Add the points to the current players score variable.
        Score(CurrentPlayer) = Score(CurrentPlayer) + points 
		If Not(Tilted) Then
			' Display score and ball in play.
			If Score(1) < 10000000 Then
                DisplayScore
				D1.Text = FormatScore(Score(1)) & "                        BALL " & Ball
			Else
                DisplayScore
				D1.Text = FormatScore(Score(1)) & String(25 - Len(Score(1)), " ") 
			End If
			Exit Sub
		End If
	End If
End Sub


Function FormatScore(ByVal sc)
	Dim fsc
	Dim fdone
	fdone = ""
	While Len(sc) > 3
		fsc = Right(sc, 3)
		If fdone = "" Then
			fdone = "," & fsc
		Else
			fdone = "," & fsc & fdone
		End If
		sc = Left(sc, Len(sc)-3)
	Wend
	fdone = sc & fdone
	FormatScore = fdone
End Function









Sub Drain_Hit()
    ' pretend to knock the ball into the ball storage mech
    PlaySound "drain"
	startB2S(30)
if ballsonplayfield = 2 then
J1.state = 0
J2.state = 0
J3.state = 0
J4.state = 0
J5.state = 0
J6.state = 0
J7.state = 0

PlaySong "DBZ_End"
PlaySong "DBZ_GamePlay"
end if

   Drain.destroyball
   BallsOnPlayfield = BallsOnPlayfield - 1



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
           ' DMD " ", "BALL SAVE", 2000
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1) Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True) then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
                    ResetJackpotLights
                    If LockMultiball = True Then
                       ResetLockMultiball
                    End If
 
                    If ShenlongMultiball = True Then
                       ResetShenlongMultiball
                    End If
                    
                    If SaiyanMultiball = True Then
                       ResetSaiyanMultiball
                    End If

                   ' ChangeSong
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
                ' End Mode and timers
              '  ChangeSong
                ChangeGi white
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub


Sub Drain1_Hit()
    PlaySound "drain"
    Drain1.destroyball
    BallsOnPlayfield = 0
End Sub



Sub swPlungerRest_Hit()
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
    PlaySound "fx_sensor", 0, 1, 0.15, 0.25
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    'LaunchLight.State = 2
    SkillShotLightsTimer.enabled = 1

    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
        PlaySound "fx_fire", 0, 1, 0.05, 0.05
        bAutoPlunger = False
        GateOpenSkillShot.enabled = 1
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
      '  UpdateSkillshot()
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub


Dim startSeq
Sub SkillShotLightsTimer_Timer
    startSeq = startSeq + 1
  Select Case startSeq
         case 1: LightBall8.state = 1: DisplayB2SText "         SHOT THE BALL          ": D1.text = "         SHOT THE BALL          "
         Case 2: LightBall8.state = 0: LightBall9.state = 1
         Case 3: LightBall9.state = 0: LightBall10.state = 1
         Case 4: LightBall10.state = 0: LightBall11.state = 1
         Case 5: LightBall11.state = 0: LightBall12.state = 1
         Case 6: LightBall12.state = 0: LightBall13.state = 1
         Case 7: LightBall13.state = 0: LightBall14.state = 1
         Case 8: LightBall14.state = 0:
         Case 9: startSeq = 9: UpdateLightsSkillShot
  End Select
End Sub

Sub UpdateLightsSkillShot
  If startSeq = 9 Then startSeq = 0: SkillShotLightsTimer.enabled = 1
End Sub

Sub GateOpenSkillShot_Timer
     GateOpenSkillShot.enabled = 0
     Gate1.Open = 1
     Gate2.Open = 1
     vpmtimer.addtimer 2000, "ResetGate '" 
End Sub

Sub ResetGate
     Gate1.Open = 0
     Gate2.Open = 0
End Sub


' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
        ResetSkillShotTimer.Enabled = 1
        SkillShotLightsTimer.enabled = 0
        LightBall8.state = 0:LightBall9.state = 0:LightBall10.state = 0:LightBall11.state = 0:LightBall12.state = 0:LightBall13.state = 0:LightBall14.state = 0
    End If
    FlashEffect 2
    
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds

Sub swPlungerRest_Timer
    'DMD "", "SHOOT THE BALL", 2000
    D1.text "SHOOT THE BALL"
    swPlungerRest.TimerEnabled = 0
End Sub

Sub Trigger3_Hit:startB2S(28): SkillShotLightsTimer.enabled = 0 End Sub


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

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
    Gate1.Open = 0
    Gate2.Open = 0
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
   ' LightSeqSkillshot.StopPlay
    If L11.State = 2 Then L11.State = 0
    L11.State = 0
    Gate1.Open = 0
    Gate2.Open = 0
    'DMDScoreNow
End Sub



Sub CheckSuperJackpot
  If J1.state + J2.state + J3.state + J4.state + J5.state + J6.state = 1 Then
     AwardSuperJackpot
  End If
End Sub


'********
'Bumpers
'********

Dim BumperHit
Dim ExtraBallAwardBumpers

ExtraBallAwardBumpers = 200

Sub bumper1_hit
 If Tilted Then Exit Sub
   startB2S(10)
   PlaySound "fx_bumper1"
   AddScore (100)
   BonusBumper = BonusBumper + 1
   Playsound "DBZ_meleemiss1"
   BumperHit = BumperHit - 1
   checkBumpers()
   D1.text = "    " & BumperHit & " HITS   "	
   FlashForms BumperFlash, 100, 10, 0
   flashforms bumper1L, 500, 100, 1
   LastSwitchHit = "Bumper1"
End Sub


Sub bumper2_hit
 If Tilted Then Exit Sub
   startB2S(11)
   PlaySound "fx_bumper2"
   AddScore (100)
   BonusBumper = BonusBumper + 1
   Playsound "DBZ_meleemiss2" 
   BumperHit = BumperHit - 1
   checkBumpers()
   D1.text = "    " & BumperHit & " HITS   "	
   FlashForms BumperFlash, 100, 10, 0
   flashforms bumper2L, 500, 100, 1
   LastSwitchHit = "Bumper2"
End Sub


Sub bumper3_hit
 If Tilted Then Exit Sub
   startB2S(12)
   PlaySound "fx_bumper3"
   AddScore (100)
   BonusBumper = BonusBumper + 1
   Playsound "DBZ_meleemiss3"
   BumperHit = BumperHit - 1
   checkBumpers()
   D1.text = "    " & BumperHit & " HITS   "
   FlashForms BumperFlash, 100, 10, 0
   flashforms bumper3L, 500, 100, 1
   LastSwitchHit = "Bumper3"
End Sub


Sub checkBumpers()

    ExtraBallAwardBumpers = BumperHit 
 If BumperHit <= 0 Then
    AwardExtraBall
    BumperHit = 200
    D1.text = " EXTRABALL AWARD " 
    DisplayB2SText "         EXTRABALL AWARD        "
  Else
    D1.text = " EXTRABALL AT  " & ExtraBallAwardBumpers
    DisplayB2SText "   EXTRABALL AT   " & " " & ExtraBallAwardBumpers & "  HITS   "
 End If 
End Sub







' Slingshots has been hit

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
  '  startB2S(7)
    PlaySound SoundFXDOF("left_slingshot",103,DOFPulse,DOFContactors), 0, 1, -0.05, 0.05
    LeftSling4.Visible = 1:LeftSling1.Visible = 0
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore (500)
    ' add some effect to the table?
    Gi2.State = 0
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
    FlashForms gi2, 100, 10, 1
    FlashForms gi18, 100, 10, 1
    FlashForms gi20, 100, 10, 1
	DOF 103, DOFPulse
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:LeftSling1.Visible = 1:Lemk.RotX = -10:Gi2.State = 1:LeftSlingShot.TimerEnabled = False
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
   ' startB2S(8)
    If Tilted Then Exit Sub
    PlaySound SoundFXDOF("right_slingshot",104,DOFPulse,DOFContactors), 0, 1, 0.05, 0.05
    RightSling4.Visible = 1:RightSling1.Visible = 0
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore (500)
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
    FlashForms gi1, 100, 10, 0
    FlashForms gi19, 100, 10, 0
    FlashForms gi21, 100, 10, 0
	DOF 104, DOFPulse
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:RightSLing1.Visible = 1:Remk.RotX = -10:Gi1.State = 1:RightSlingShot.TimerEnabled = False
    End Select
    RStep = RStep + 1
End Sub





'********************
'Ramps Triggers
'********************


Dim LockL
Dim LockR
Dim LockDiverterEnabled
Dim LockMultiballReady

Sub TriggerVukRamp_Hit
 If Tilted Then Exit Sub
    RampBonus = RampBonus + 1
    Playsound "fx_metalrolling"
   FlashForms f2, 1000, 50, 0
   flashforms f2b, 1000, 50, 0
 If LockR = 1 Then
    vpmtimer.addtimer 1000, "LockR = 2 '"
    LockRightRamp.state = 1
    PinR.Isdropped = 0
    Playsound "DiverterOn"
    DisplayB2SText "       RIGHT LOCK DONE        "
    vpmtimer.addtimer 1000, "UnaBolaMas '"
    vpmtimer.addtimer 2000, "CheckLockDiverter '"
 End If

  If LockR = 0 Then
    LightLock4.state = 1
    vpmtimer.addtimer 4000, "CheckLocks '"
    AddScore 500

   Else

  If J4.state = 2 Then
     J4.state = 1
     AwardJackpot
   Else
     AddScore 500
     CheckSuperJackpot
   End If
  End If
 

End Sub



Sub TriggerRampL_Hit
 If Tilted Then Exit Sub
    RampBonus = RampBonus + 1
    PlaySound "fx_metalrolling", 0, 1, pan(ActiveBall)
    FlashForms f6, 1000, 50, 0
    DOF 210, DOFPulse
  If BallsOnPlayfield = 1 Then 
    LightLock2.state = 1
    AddScore 500
    CheckLocks
  If scoopLit.state = 0 Then
     scoopLit.state = 1
  End If

   Else

  If J2.state = 2 Then
     J2.state = 1
     AwardJackpot
   Else
     AddScore 500
     CheckSuperJackpot
   End If
  End If
 
    'check for combos
   vpmTimer.AddTimer 100, "ComboblinkLight"
   If ComboReady = True Then 
    if LastSwitchHit = "Leftorbittrigger" Or LastSwitchHit = "Rightorbittrigger" Or LastSwitchHit = "TriggerRampL"  Or LastSwitchHit = "TriggerRampR" Then
        Addscore jackpot(CurrentPlayer)
        DisplayB2SText "           COMBO           " & jackpot(CurrentPlayer)
        ComboMSNTimer.Interval = 500
        ComboMSNTimer.enabled = 1
        D1.TEXT = "COMBO" & jackpot(CurrentPlayer)
       ' DMD "COMBO", jackpot(CurrentPlayer), 1000
    End If
   End If
    LastSwitchHit = "TriggerRampL"

End Sub


Sub TriggerRampLockL_Hit
    Playsound "fx_metalrolling", 0, 1, pan(ActiveBall)
 If BallsOnPlayfield > 1 Then 
    CheckLockLeft
  Else

 If LockL = 1 Then
    LockL = 2
    LockLeftRamp.state = 1
    PinL.Isdropped = 0
    Playsound "DiverterOn"
    DisplayB2SText "       LEFT LOCK DONE        "
    vpmtimer.addtimer 1000, "UnaBolaMas '"
    vpmtimer.addtimer 2000, "CheckLockDiverter '"
   Else
    CheckLockLeft
 End If
 End If
 End Sub

Sub CheckLockLeft()
 If LockL = 2 Then 
    PinL.Isdropped = 1
    Playsound "DiverterOff"
    ResetPinL.interval = 400
    ResetPinL.enabled = 1
 End If
End Sub

Sub ResetPinL_Timer
  ResetPinL.enabled = 0
  PinL.Isdropped = 0
  Playsound "DiverterOn"
End Sub






Sub TriggerRampR_Hit
 If Tilted Then Exit Sub
    RampBonus = RampBonus + 1
    PlaySound "fx_metalrolling", 0, 1, pan(ActiveBall)
    FlashForms f5, 1000, 50, 0
    DOF 210, DOFPulse
  If BallsOnPlayfield = 1 Then 
    LightLock6.state = 1
    AddScore 500
    vpmtimer.addtimer 3500, "CheckLocks '"
  If kamehouseLit.state = 0 Then
     kamehouseLit.state = 1
  End If

   Else

  If J6.state = 2 Then
     J6.state = 1
     AwardJackpot
   Else
     AddScore 500
     CheckSuperJackpot
   End If
  End If

    'check for combos
   vpmTimer.AddTimer 100, "ComboblinkLight"
   If ComboReady = True Then 
    if LastSwitchHit = "Leftorbittrigger" Or LastSwitchHit = "Rightorbittrigger" Or LastSwitchHit = "TriggerRampL"  Or LastSwitchHit = "TriggerRampR" Then
        Addscore jackpot(CurrentPlayer)
        DisplayB2SText "           COMBO           " & jackpot(CurrentPlayer)
        ComboMSNTimer.Interval = 500
        ComboMSNTimer.enabled = 1
        D1.TEXT = "COMBO" & jackpot(CurrentPlayer)
       ' DMD "COMBO", jackpot(CurrentPlayer), 1000
    End If
   End If
    LastSwitchHit = "TriggerRampR"

End Sub


Sub TriggerRampLockR_Hit
    CheckLockRight
 End Sub

Sub TriggerRampLockR1_Hit
    CheckLockRight
 End Sub

Sub CheckLockRight()
 If LockR = 2 Then 
    PinR.Isdropped = 1
    Playsound "DiverterOff"
    ResetPinR.interval = 300
    ResetPinR.enabled = 1
 End If
End Sub


Sub ResetPinR_Timer
  ResetPinR.enabled = 0
  PinR.Isdropped = 0
  Playsound "DiverterOn"
End Sub


Sub CheckLockDiverter()
 If LockL = 2 And LockR = 2 Then
    vpmtimer.addtimer 3000, "LockMultiballReady = True '"
    DisplayB2SText "        MULTIBALL READY          "
    LockDiverterEnabled = True
    LockDiverterL.state = 2
    LockDiverterR.state = 2
    FlashForMs f1, 1000, 50, 2
    FlashForMs f1b, 1000, 50, 2
 End If
End Sub


Sub Leftorbittrigger_Hit
 If Tilted Then Exit Sub
    PlaySound "Gorilla DBZ", 0, 1, pan(ActiveBall)
  If BallsOnPlayfield = 1 Then 
    LightLock1.state = 1
    AddScore 500
    CheckLocks
 If LockL = 2 And LockR = 2 And LockMultiballReady = True Then
    LockDiverterOn
    Gate1.Open = 1
    PinDiverterR.Isdropped = 0
    Playsound "DiverterOn"
    LeftorbittriggerTimer.Interval = 2000
    LeftorbittriggerTimer.enabled = 1    
 End If

  Else

 If J1.state = 2 Then
     J1.state = 1
     AwardJackpot
   Else
     CheckSuperJackpot
   End If
 End If

    'check for combos
   If ComboReady = True Then 
      GateOpenSkillShot.enabled = 1
    if LastSwitchHit = "Leftorbittrigger" Or LastSwitchHit = "Rightorbittrigger" Or LastSwitchHit = "TriggerRampL"  Or LastSwitchHit = "TriggerRampR" Then
        Addscore jackpot(CurrentPlayer)
        DisplayB2SText "           COMBO           " & jackpot(CurrentPlayer)
        ComboMSNTimer.Interval = 500
        ComboMSNTimer.enabled = 1
        D1.TEXT = "COMBO" & jackpot(CurrentPlayer)
       ' DMD "COMBO", jackpot(CurrentPlayer), 1000
    End If
   End If

    LastSwitchHit = "Leftorbittrigger"

End Sub

Sub LeftorbittriggerTimer_Timer
    LeftorbittriggerTimer.enabled = 0
    LockDiverterOff
    Playsound "DiverterOff"
    Gate1.Open = 0
    PinDiverterR.Isdropped = 1
End Sub

Sub Rightorbittrigger_Hit
 If Tilted Then Exit Sub
    PlaySound "Cloud_sound", 0, 1, pan(ActiveBall)
   If BallsOnPlayfield = 1 Then
    LightLock7.state = 1
    AddScore 500
    CheckLocks
 If LockL = 2 And LockR = 2 And LockMultiballReady = True Then
    LockDiverterOn
    Playsound "DiverterOff"
    Gate2.Open = 0
    PinDiverterL.Isdropped = 0
    RightorbittriggerTimer.interval = 1000 
    RightorbittriggerTimer.enabled = 1
 End If

  Else

 If J7.state = 2 Then
     J7.state = 1
     AwardJackpot
   Else
     CheckSuperJackpot
   End If
 End If

    'check for combos
   If ComboReady = True Then 
      GateOpenSkillShot.enabled = 1
    if LastSwitchHit = "Leftorbittrigger" Or LastSwitchHit = "Rightorbittrigger" Or LastSwitchHit = "TriggerRampL"  Or LastSwitchHit = "TriggerRampR" Then
        Addscore jackpot(CurrentPlayer)
        DisplayB2SText "           COMBO           " & jackpot(CurrentPlayer)
        ComboMSNTimer.Interval = 500
        ComboMSNTimer.enabled = 1
        D1.TEXT = "COMBO" & jackpot(CurrentPlayer)
       ' DMD "COMBO", jackpot(CurrentPlayer), 1000
    End If
   End If
    LastSwitchHit = "Rightorbittrigger"

End Sub

Sub RightorbittriggerTimer_Timer
    RightorbittriggerTimer.enabled = 0
    Playsound "DiverterOff"
    Gate2.Open = 1
    PinDiverterL.Isdropped = 1
    LockDiverterOff
End Sub    
    



Sub CheckLocks() 
 If LockDiverterEnabled = True Then Exit Sub
 If LockL = 2 Or LockR = 2 Then Exit Sub  

 If LightLock1.state = 1 And LightLock2.state = 1 And LightLock3.state = 1 And LightLock4.state = 1 And LightLock5.state = 1 And LightLock6.state = 1 And LightLock7.state = 1 Then    
    LockLeftRamp.state = 2
    LockRightRamp.state = 2 
    LockL = 1
    LockR = 1
    DisplayB2SText "        LOCK THE BALLS        "
    FlashForms f3, 500, 50, 2
    flashforms f3b, 500, 50, 2
    FlashForms f4, 500, 50, 2
    flashforms f4b, 500, 50, 2
    DOF 105 ,DOFPulse
    DOF 106 ,DOFPulse
 End If
End Sub




'******
'Oyos
'******

Dim ShenlongMultiball
Dim Shenlong
Sub Scoop_Hit
    ScoopFake.enabled = 0
 If BallsOnPlayfield > 1 Then 

  If J3.state = 2 Then
     J3.state = 1
     AwardJackpot
   Else
     CheckSuperJackpot
   End If 
   vpmtimer.addtimer 2500, "ScoopSolenoidPulse '"
 Else

  If ExtraBallIsIit = True Then
     ExtraBallIsIit = False
     LightExtraBallLit.state = 0
     LightExtraBall.state = 1
     AwardExtraBall()
  End If

    ScoopTimer1.enabled = 1
  End If
End Sub



Sub ScoopTimer1_Timer
    ScoopTimer1.enabled = 0
    AddScore 1000
 If BallsOnPlayfield = 1 Then 
    LightLock3.state = 1
    CheckLocks
 End If
 If scoopLit.state = 1 Then
    scoopLit.state = 0
    shenlong = shenlong + 1
    CheckBalls
   Else
   vpmtimer.addtimer 2500, "ScoopSolenoidPulse '"
   DisplayB2SText "      FIND THE DRAGON BALLS     "    
   D1.text = "FIND THE DRAGON BALLS"
  End If
End Sub

Sub ScoopFake_Hit
    Playsound "fx_balldrop" 
End Sub

Sub CheckBalls
Dim i
 If shenlong = 7 Then
    startB2S(31)
    LightSeqAllBalls.Play SeqRandom, 10, , 50000
    AddMultiball 6
    ShenlongMultiball = True
    bMultiBallMode = True
    LightBall1.state = 0
    LightBall2.state = 0
    LightBall3.state = 0
    LightBall4.state = 0
    LightBall5.state = 0
    LightBall6.state = 0
    LightBall7.state = 0
    PlaySong "DBZ_End"
    PlaySong "DBZ_Multiball"
    StartJackpots
    OjoL.state = 2
    OjoR.state = 2
    vpmtimer.addtimer 2500, "ScoopSolenoidPulse '" 
    DisplayB2SText "       SHENLONG MULTIBALL       "    
    D1.text = "SHENLONG MULTIBALL"
    SHENLONGMSNTimer.interval = 1000
    SHENLONGMSNTimer.enabled = 1
    ResetLanes()    
  End If

  If shenlong = 6 Then LightBall6.state = 1: startB2S(6): LightSeqBalls5.StopPlay: BallsMessage() :vpmtimer.addtimer 2500, "ScoopSolenoidPulse '"      
  If shenlong = 5 Then LightBall5.state = 1: startB2S(5): LightSeqBalls4.StopPlay: BallsMessage(): LightSeqBalls5.Play SeqLeftOn, 25, 1: LightSeqBalls5.UpdateInterval = 25: 

LightSeqBalls5.Play SeqRightOn, 25, 1: vpmtimer.addtimer 2500, "ScoopSolenoidPulse '" 
  If shenlong = 4 Then LightBall4.state = 1: startB2S(4): LightSeqBalls3.StopPlay: BallsMessage(): LightSeqBalls4.Play SeqLeftOn, 25, 1: LightSeqBalls4.UpdateInterval = 25: 

LightSeqBalls4.Play SeqRightOn, 25, 1: vpmtimer.addtimer 2500, "ScoopSolenoidPulse '" 
  If shenlong = 3 Then LightBall3.state = 1: startB2S(3): LightSeqBalls2.StopPlay: BallsMessage(): LightSeqBalls3.Play SeqLeftOn, 25, 1: LightSeqBalls3.UpdateInterval = 25: 

LightSeqBalls3.Play SeqRightOn, 25, 1: vpmtimer.addtimer 2500, "ScoopSolenoidPulse '" 
  If shenlong = 2 Then LightBall2.state = 1: startB2S(2): LightSeqBalls1.StopPlay: BallsMessage(): LightSeqBalls2.Play SeqLeftOn, 25, 1: LightSeqBalls2.UpdateInterval = 25: 

LightSeqBalls2.Play SeqRightOn, 25, 1: vpmtimer.addtimer 2500, "ScoopSolenoidPulse '" 
  If shenlong = 1 Then LightBall1.state = 1: startB2S(1): LightSeqAllBalls.StopPlay: BallsMessage(): LightSeqBalls1.Play SeqLeftOn, 25, 1: LightSeqBalls1.UpdateInterval = 25: LightSeqBalls1.Play SeqRightOn, 25, 1: vpmtimer.addtimer 2500, "ScoopSolenoidPulse '" 
End Sub

Sub LightSeqAllBalls_PlayDone()
    BallsRestart()
End Sub


Sub BallsRestart
Dim i
    shenlong = 0
    LightSeqAllBalls.UpdateInterval = 25  
    LightSeqAllBalls.Play SeqLeftOn, 25, 1  
    LightSeqAllBalls.UpdateInterval = 25  
    LightSeqAllBalls.Play SeqRightOn, 25, 1  
    LightSeqAllBalls.UpdateInterval = 25
    LightSeqAllBalls.UpdateInterval = 25  
    LightSeqAllBalls.Play SeqLeftOn, 25, 1  
    LightSeqAllBalls.UpdateInterval = 25  
    LightSeqAllBalls.Play SeqRightOn, 25, 1  
    LightSeqAllBalls.UpdateInterval = 25
End Sub


Sub BallsRestart6
    LightBall7.state = 2
End Sub


Sub LightSeqBalls5_PlayDone()
    BallsRestart5()
End Sub


Sub BallsRestart5
Dim i
    LightSeqBalls5.UpdateInterval = 25  
    LightSeqBalls5.Play SeqLeftOn, 25, 1  
    LightSeqBalls5.UpdateInterval = 25  
    LightSeqBalls5.Play SeqRightOn, 25, 1  
    LightSeqBalls5.UpdateInterval = 25
    LightSeqBalls5.UpdateInterval = 25  
    LightSeqBalls5.Play SeqLeftOn, 25, 1  
    LightSeqBalls5.UpdateInterval = 25  
    LightSeqBalls5.Play SeqRightOn, 25, 1  
    LightSeqBalls5.UpdateInterval = 25
End Sub


Sub LightSeqBalls4_PlayDone()
    BallsRestart4()
End Sub


Sub BallsRestart4
Dim i
    LightSeqBalls4.UpdateInterval = 25  
    LightSeqBalls4.Play SeqLeftOn, 25, 1  
    LightSeqBalls4.UpdateInterval = 25  
    LightSeqBalls4.Play SeqRightOn, 25, 1  
    LightSeqBalls4.UpdateInterval = 25
    LightSeqBalls4.UpdateInterval = 25  
    LightSeqBalls4.Play SeqLeftOn, 25, 1  
    LightSeqBalls4.UpdateInterval = 25  
    LightSeqBalls4.Play SeqRightOn, 25, 1  
    LightSeqBalls4.UpdateInterval = 25
End Sub


Sub LightSeqBalls3_PlayDone()
    BallsRestart3()
End Sub


Sub BallsRestart3
Dim i
    LightSeqBalls3.UpdateInterval = 25  
    LightSeqBalls3.Play SeqLeftOn, 25, 1  
    LightSeqBalls3.UpdateInterval = 25  
    LightSeqBalls3.Play SeqRightOn, 25, 1  
    LightSeqBalls3.UpdateInterval = 25
    LightSeqBalls3.UpdateInterval = 25  
    LightSeqBalls3.Play SeqLeftOn, 25, 1  
    LightSeqBalls3.UpdateInterval = 25  
    LightSeqBalls3.Play SeqRightOn, 25, 1  
    LightSeqBalls3.UpdateInterval = 25
End Sub


Sub LightSeqBalls2_PlayDone()
    BallsRestart2()
End Sub


Sub BallsRestart2
Dim i
    LightSeqBalls2.UpdateInterval = 25  
    LightSeqBalls2.Play SeqLeftOn, 25, 1  
    LightSeqBalls2.UpdateInterval = 25  
    LightSeqBalls2.Play SeqRightOn, 25, 1  
    LightSeqBalls2.UpdateInterval = 25
    LightSeqBalls2.UpdateInterval = 25  
    LightSeqBalls2.Play SeqLeftOn, 25, 1  
    LightSeqBalls2.UpdateInterval = 25  
    LightSeqBalls2.Play SeqRightOn, 25, 1  
    LightSeqBalls2.UpdateInterval = 25
End Sub


Sub LightSeqBalls1_PlayDone()
    BallsRestart1
End Sub


Sub BallsRestart1
Dim i
    LightSeqBalls1.UpdateInterval = 25  
    LightSeqBalls1.Play SeqLeftOn, 25, 1  
    LightSeqBalls1.UpdateInterval = 25  
    LightSeqBalls1.Play SeqRightOn, 25, 1  
    LightSeqBalls1.UpdateInterval = 25
    LightSeqBalls1.UpdateInterval = 25  
    LightSeqBalls1.Play SeqLeftOn, 25, 1  
    LightSeqBalls1.UpdateInterval = 25  
    LightSeqBalls1.Play SeqRightOn, 25, 1  
    LightSeqBalls1.UpdateInterval = 25
End Sub

Sub ResetShenlongMultiball
    D1.Text =      "   END SHENLONG MULTIBALL       "
    DisplayB2SText "   END SHENLONG MULTIBALL       "
    ShenlongMultiball = False
    shenlong = 0
    LightBall1.state = 0
    LightBall2.state = 0
    LightBall3.state = 0
    LightBall4.state = 0
    LightBall5.state = 0
    LightBall6.state = 0
    LightBall7.state = 0
    OjoL.state = 0
    OjoR.state = 0
    BallsRestart
    ResetLanes
End Sub

Sub BallsMessage()
 select Case shenlong
        Case 0: If scoopLit.state = 0 Then
                DisplayB2SText "      FIND THE DRAGON BALLS     "    
                D1.text = "FIND THE DRAGON BALLS"
                End If
        Case 1: If shenlong = 1 Then
                DisplayB2SText "     DRAGON BALL 1 UNLOCKET     ":    
                D1.text = "DRAGON BALL 1 UNLOCKET"
                BallsRestart1
                End If
        Case 2: If shenlong = 2 Then
                DisplayB2SText "     DRAGON BALL 2 UNLOCKET     ":    
                D1.text = "DRAGON BALL 2 UNLOCKET"
                BallsRestart2
                End If

        Case 3: If shenlong = 3 Then 
                DisplayB2SText "     DRAGON BALL 3 UNLOCKET     ":    
                D1.text = "DRAGON BALL 3 UNLOCKET"
                BallsRestart3
                End If

        Case 4: If shenlong = 4 Then
                DisplayB2SText "      DRAGON BALL 4 UNLOCKET    ":    
                D1.text = "DRAGON BALL 4 UNLOCKET"
                BallsRestart4
                End If

        Case 5: If shenlong = 5 Then
                DisplayB2SText "      DRAGON BALL 5 UNLOCKET    ":    
                D1.text = "DRAGON BALL 5 UNLOCKET"
                BallsRestart5
                End If

        Case 6: If shenlong = 6 Then
                DisplayB2SText "      DRAGON BALL 6 UNLOCKET    ":    
                D1.text = "DRAGON BALL 6 UNLOCKET"
                BallsRestart6
                End If

        Case 7: If shenlong = 7 Then
                DisplayB2SText "      DRAGON BALL 7 UNLOCKET    ":    
                D1.text = "DRAGON BALL 7 UNLOCKET"
                End If
        End Select
End Sub



'***************
'KameHouse 
'***************

Sub kamehouse_Hit
    kamehouseFake.enabled = 0   
 If BallsOnPlayfield = 1 Then
    LightLock5.state = 1
    CheckLocks
    RandomAwardTimer.interval = 2000
    RandomAwardTimer.enabled = 1
  Else
   If J5.state = 2 Then
     J5.state = 1
     AwardJackpot
   Else
     CheckSuperJackpot
   End If 
     vpmtimer.addtimer 2500, "kamehouseSolenoidPulse '" 
  End If
End Sub

Sub RandomAwardTimer_Timer
    RandomAwardTimer.enabled = 0
 If kamehouseLit.state = 1 Then
    kamehouseLit.state = 0
    kamehouseAward
  Else
    kamehouseSolenoidPulse
    DisplayB2SText "         NOBODY'S HOME          "
    D1.TEXT = "NOBODY'S HOME"
    AddScore 1000
 End If
End Sub

Sub kamehouseFake_Hit
    Playsound "fx_balldrop"
End Sub


Dim ExtraBallIsIit
Sub kamehouseAward() 'from the Pyramid
    Dim tmp, tmp2

    tmp = INT(RND(1) * 80)
    Select Case tmp
        Case 1, 2, 3, 4, 5, 6 'Lit Extra Ball
            'DMD "EXTRA BALL IS LIT", " ", 2000
            D1.text = "EXTRA BALL IS LIT"
            DisplayB2SText "        EXTRA BALL IS LIT       "
            ExtraBallIsIit = True
            LightExtraBallLit.State = 2
        Case 7, 8, 13, 14, 15 '100,000 points
            'DMD "BIG POINTS", "10000", 2000
            D1.text = "BIG POINTS 10000"
            DisplayB2SText "        BIG POINTS 10000        "
            AddScore 10000
        Case 9, 10, 11, 12 'Hold Bonus
            'DMD "BONUS HELD", "ACTIVATED", 2000
            DisplayB2SText "      BONUS HELD ACTIVATED      "
            D1.text = "      BONUS HELD ACTIVATED      "
            bBonusHeld = True
        Case 16, 17, 18 'Increase Bonus Multiplier
            'DMD "INCREASED", "BONUS X", 2000
            D1.text = "INCREASED BONUS X"
            DisplayB2SText "       INCREASED BONUS X        "
            AddBonusMultiplier 1
        Case 19, 20, 21 'Add a Dragon Ball
             Shenlong = Shenlong + 1
             D1.text = "DRAGON BALL FOUND"
            DisplayB2SText "       DRAGON BALL FOUND        "
             CheckBalls
        Case 22, 23, 36, 37, 38 'PlayField multiplier
           ' DMD "INCREASED", "PLAYFIELD X", 2000
          '  AddPlayfieldMultiplier 1
        Case 24, 25, 26, 27, 28 '100,000 points
            'DMD "BIG POINTS", "10000", 2000
            D1.text = "BIG POINTS 50000"
            DisplayB2SText "        BIG POINTS 50000         "
            AddScore 50000
        Case 29, 30, 31, 32, 33, 34, 35 'Increase Bumper value
            BumperValue(CurrentPlayer) = BumperValue(CurrentPlayer) + 500
          '  DMD "BUMPER VALUE", BumperValue(CurrentPlayer), 2000
            D1.text = "BUMPER VALUE" & BumperValue(CurrentPlayer)
            DisplayB2SText "          BUMPER VALUE            " & BumperValue(CurrentPlayer)
        Case 39, 40, 43, 44 'extra multiball
            'DMD "EXTRA", "MULTIBALL", 2000
            D1.text = "QUICK MULTIBALL"
            DisplayB2SText "          QUICK MULTIBALL        "
            UnaBolaMas: bMultiBallMode = True :BallsOnPlayfield = 2 : PlaySong "DBZ_End": PlaySong "DBZ_Multiball": EnableBallSaver 10: StartJackpots: GiEffect 2: 

LightEffect 2
        Case 45, 46, 47, 48 ' Ball Save
           ' DMD "BALL SAVE", "ACTIVATED", 2000
            D1.text = "BALL SAVE ACTIVATED"
            DisplayB2SText "        BALL SAVE ACTIVATED        "
            EnableBallSaver 20
        Case 51, 52, 53, 54 ' Add Light Lock
            Select Case tmp
                   Case 1: If LightLock1.state = 0 Then
                              LightLock1.blinkinterval = 1000
                              LightLock1.state = 1
                              CheckLocks
                           End If
                   Case 2: If LightLock2.state = 0 Then
                              LightLock2.blinkinterval = 1000
                              LightLock2.state = 1
                              CheckLocks
                           End If
                   Case 3: If LightLock3.state = 0 Then
                              LightLock3.blinkinterval = 1000
                              LightLock3.state = 1
                              CheckLocks
                           End If
                   Case 4: If LightLock4.state = 0 Then
                              LightLock4.blinkinterval = 1000
                              LightLock4.state = 1
                              CheckLocks
                           End If
                   Case 5: If LightLock5.state = 0 Then
                              LightLock5.blinkinterval = 1000
                              LightLock5.state = 1
                              CheckLocks
                           End If
                   Case 6: If LightLock6.state = 0 Then
                              LightLock6.blinkinterval = 1000
                              LightLock6.state = 1
                              CheckLocks
                           End If
                   Case 7: If LightLock7.state = 0 Then
                              LightLock7.blinkinterval = 1000
                              LightLock7.state = 1
                              CheckLocks
                           End If
               End Select
            D1.text = "ADD LIGHT LOCK"
            DisplayB2SText "        ADD LIGHT LOCK          "
        Case 60, 61, 62, 63 ' Add Saiyan Level
            Saiyan = Saiyan + 1
            CheckNivel
            D1.text = "ADD SAIYAN LEVEL"
            DisplayB2SText "        ADD SAIYAN LEVEL        "
        Case ELSE 'Add a Random score from 10.000 to 100,000 points
            tmp2 = INT((RND) * 9) * 1000 + 10000
           ' DMD "EXTRA POINTS", tmp2, 2000
            D1.text = "EXTRA POINTS  " & tmp2
            DisplayB2SText "          EXTRA POINTS           " & tmp2
            AddScore tmp2
    End Select

            kamehouseSolenoidPulse

End Sub  



'**************
' LockHole
'**************

Sub LockHole_Hit
    LockDiverterOff
    ResetLanes()
    LockDiverterL.state = 1
    LockDiverterR.state = 1
    Gate4.Open = 1
    PlaySong "DBZ_End"
    PlaySong "DBZ_Multiball"
    LightEffect 2
    FlashEffect 2
    Playsound "subway2"
    LockHoleTimer.interval = 4000
    LockHoleTimer.enabled = 1
End Sub 

Sub LockHoleSolenoidPulse
    ScoopFake.enabled = 0
    LockHole.kick 225, 10
End Sub

Sub LockHoleTimer_Timer
    LockHoleTimer.enabled = 0
    LockMultiballReady = False
    LockMultiball = True
    bMultiBallMode = True
    LockL = 0
    LockR = 0
    LockPinOff
    BallsOnPlayfield = 3
    StartJackpots
    scoop1.enabled = 1
    scoop.enabled = 0
    DisplayB2SText "           MULTIBALL            "
    MultiballMSNTimer.interval = 2000
    MultiballMSNTimer.enabled = 1
    LockHoleSolenoidPulse
    FlashForMs f1, 1000, 50, 0
    FlashForMs f1b, 1000, 50, 0
End Sub


Sub scoop1_Hit
    scoop1.kick 0, 30 , 1.5
    Playsound "salidadebola"
    scoop1.enabled = 0
    Scoop1Timer.interval = 1000
    Scoop1Timer.enabled = 1
End Sub

Sub Scoop1Timer_Timer
    Scoop1Timer.enabled = 0
    scoop.enabled = 1    
End Sub

Sub kamehouseSolenoidPulse
    Playsound "salidadebola"
    kamehouseFake.enabled = 0
    kamehouse.kick 0, 33, 1.5
    kamehouseTimer.interval = 500
    kamehouseTimer.enabled = 1
End Sub

Sub kamehouseTimer_Timer
    kamehouseTimer.enabled = 0
    kamehouseFake.enabled = 1
End Sub

Sub ScoopSolenoidPulse
    Playsound "salidadebola"
    ScoopFake.enabled = 0
    scoop.kick 0, 30 , 1.5
    scoopTimer.interval = 500
    scoopTimer.enabled = 1
End Sub
Sub ScoopTimer_Timer
    ScoopTimer.enabled = 0
    ScoopFake.enabled = 1
End Sub


Sub Kicker1_Hit
    Kicker1.kick 180, 1
End Sub

Sub Kicker2_Hit
    Kicker2.kick 180, 1
End Sub


Sub LockPinOff()
    PinL.Isdropped = 1
    PinR.Isdropped = 1
End Sub



Sub LockPinOn()
    PinL.Isdropped = 0
    PinR.Isdropped = 0
End Sub



Sub LockDiverterOff()
    LockDiverter.RotateToStart
End Sub

Sub LockDiverterOn()
    LockDiverter.RotateToEnd
End Sub


Sub IncrementBonusMultiplier()
    Dim NewBonusLevel

    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) < MaxMultiplier) then
        ' then set it the next next one AND set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + 1
        SetBonusMultiplier(NewBonusLevel)
        D1.Text =      " BONUS X:       " & BonusMultiplier(CurrentPlayer)
        DisplayB2SText " BONUS X:       " & BonusMultiplier(CurrentPlayer)
    End if
End Sub

Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level
'    UPdateBonusXLights(Level)
End Sub

Sub CheckMultiplier
    If(l11.State = 1) And(L12.State = 1) And(L13.State = 1) Then
        AddScore 5000       
        LightSeqLanesUP.Play SeqRandom, 5, , 2000
        IncrementBonusMultiplier()
        L11.State = 0
        L12.State = 0
        L13.State = 0
    End If
End Sub




Sub ResetJackpotLights 'when multiball is finished, resets jackpot and superjackpot lights
    bJackpot = False
    J1.State = 0
    J2.State = 0
    J3.State = 0
    J4.State = 0
    J5.State = 0
    J6.State = 0
    J7.State = 0
End Sub

Sub StartJackpots
 If bMultiballMode Then
    bJackpot = True
    J1.State = 2
    J2.State = 2
    J3.State = 2
    J4.State = 2
    J5.State = 2
    J6.State = 2
    J7.State = 2
 End If
End Sub


Dim LockMultiball
Sub ResetLockMultiball
    LockL = 0
    LockR = 0
    bJackpot = False
    LockMultiball = False
    LockDiverterEnabled = False
    LightLock1.State = 0
    LightLock2.State = 0
    LightLock3.State = 0
    LightLock4.State = 0
    LightLock5.State = 0
    LightLock6.State = 0
    LightLock7.State = 0
    LockRightRamp.State = 0
    LockLeftRamp.State = 0
    LockDiverterL.State = 0
    LockDiverterR.State = 0
    ResetLanes()
End Sub


Sub ResetLanes()
    L14.state = 0 
    L15.state = 0 
    L16.state = 0
    L17.state = 0
End Sub 
'**********************
'LaneTriggersUp
'**********************
    

Sub LeftLaneTriggerUp_Hit()
  If Tilted Then Exit Sub
	PlaySound "DBZ_jump", 0, 1, pan(ActiveBall)
    LaneBonus = LaneBonus + 1
	L11.state = 1
    AddScore 500
    CheckMultiplier
End Sub


Sub MiddleLaneTriggerUp_Hit()
  If Tilted Then Exit Sub
	PlaySound "DBZ_jump", 0, 1, pan(ActiveBall)
    LaneBonus = LaneBonus + 1
	L12.state = 1
    AddScore 500
    CheckMultiplier
End Sub



Sub RightLaneTriggerUp_Hit()
  If Tilted Then Exit Sub
	PlaySound "DBZ_jump", 0, 1, pan(ActiveBall)
    LaneBonus = LaneBonus + 1
	L13.state = 1
    AddScore 500
    CheckMultiplier
End Sub


'**********************
'LaneTriggersDown
'**********************
Sub LeftOutLaneTrigger_Hit()
  If Tilted Then Exit Sub
    LaneSounds()
    LaneBonus = LaneBonus + 1
	L14.state = 1
    AddScore 500
    CheckLaneTriggersDown

   If(bBallSaverActive = True) Then
      bBallSaverActive = False
      EnableBallSaver 1
      BallsOnPlayfield = BallsOnPlayfield + 1
      UnaBolaMas
   End If
End Sub



Sub LeftInLaneTrigger_Hit()
  If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    LaneSounds()
	L15.state = 1
    AddScore 500
    CheckLaneTriggersDown
End Sub


Sub RightInLaneTrigger_Hit()
  If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    LaneSounds()
	L16.state = 1
    AddScore 500
    CheckLaneTriggersDown
End Sub



Sub RightOutLaneTrigger_Hit()
  If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    LaneSounds()
	L17.state = 1
    AddScore 500
    CheckLaneTriggersDown

   If(bBallSaverActive = True) Then
      bBallSaverActive = False
      EnableBallSaver 1
      BallsOnPlayfield = BallsOnPlayfield + 1
      UnaBolaMas
   End If

End Sub

Sub CheckLaneTriggersDown()
  If bMultiBallMode Then Exit Sub
 If(l14.State = 1) And(L15.State = 1) And(l16.State = 1) And(L17.State = 1) Then
    LightSeqLanesDown.Play SeqRandom, 5, , 2000
    AddScore 4000
    l14.State = 0
    l15.State = 0
    l16.State = 0
    L17.State = 0
    EnableBallSaver 10
 End If
End Sub




'******************
'Static Targets
'******************

Sub TargetL1_Hit
 If Tilted Then Exit Sub 
    AddScore 500
    TargetBonus = TargetBonus + 1
    TargetsSounds()
  If bMultiBallMode Then Exit Sub
    L81.State = 1
    DOF 119, DOFPulse 
    TargetsMode() 
    LastSwitchHit = "TargetL1"
End Sub


Sub TargetL2_Hit
 If Tilted Then Exit Sub 
    AddScore 500
    TargetBonus = TargetBonus + 1
    TargetsSounds()
  If bMultiBallMode Then Exit Sub
    L91.State = 1
    DOF 119, DOFPulse 
    TargetsMode() 
    LastSwitchHit = "TargetL2"
End Sub

Sub TargetL3_Hit
 If Tilted Then Exit Sub 
    AddScore 500
    TargetBonus = TargetBonus + 1
    TargetsSounds()
  If bMultiBallMode Then Exit Sub
    L101.State = 1
    DOF 119, DOFPulse 
    TargetsMode() 
    LastSwitchHit = "TargetL3"
End Sub

Sub TargetR1_Hit
 If Tilted Then Exit Sub 
    AddScore 500
    TargetBonus = TargetBonus + 1
    TargetsSounds()
  If bMultiBallMode Then Exit Sub
    L131.State = 1
    DOF 120, DOFPulse 
    TargetsMode() 
    LastSwitchHit = "TargetR1"
End Sub

Sub TargetR2_Hit
 If Tilted Then Exit Sub 
    AddScore 500
    TargetBonus = TargetBonus + 1
    TargetsSounds()
  If bMultiBallMode Then Exit Sub
    L121.State = 1
    DOF 120, DOFPulse
    TargetsMode() 
    LastSwitchHit = "TargetR2"
End Sub

Sub TargetR3_Hit
 If Tilted Then Exit Sub 
    AddScore 500
    TargetBonus = TargetBonus + 1
    TargetsSounds()
  If bMultiBallMode Then Exit Sub
    L111.State = 1
    DOF 120, DOFPulse
    TargetsMode() 
    LastSwitchHit = "TargetR3"
End Sub


Dim TargetsModeOn
Dim Saiyan: Saiyan = 0
Dim SaiyanMultiball

Sub TargetsMode()
 If TargetsModeOn Then Exit Sub
  If(L81.State = 1) And(L91.State = 1) And(L101.State = 1) And(L111.State = 1) And(L121.State = 1) And(L131.State = 1) Then
    LightSeqTargets.Play SeqRandom, 5, , 2000
    AddScore 6000
    D1.Text =      "           TRANSFORM            "
    DisplayB2SText "           TRANSFORM            "
  '  Saiyan = Saiyan + 1 
    TargetsModeOn = True
    vpmtimer.addtimer 400, "CheckNivel '"
    EnableBallSaver 10
  End If
End Sub



Sub CheckNivel
    Saiyan = Saiyan + 1
  If Saiyan >= 5 Then
     Saiyan = 5
     AddScore 5000
     D1.Text =      "        SAIYAN LEVEL 5          "
     DisplayB2SText "        SAIYAN LEVEL 5          "
     Playsound "burning_charge"
     startB2S(29)
     SetLightColor L81, yellow, 110
     SetLightColor L91, yellow, 110
     SetLightColor L101, yellow, 110
     SetLightColor L111, yellow, 110
     SetLightColor L121, yellow, 110
     SetLightColor L131, yellow, 110

     L81.State = 2
     L91.State = 2
     L101.State = 2
     L111.State = 2
     L121.State = 2
     L131.State = 2 
    FlashForms f3, 2000, 50, 2
    flashforms f3b, 2000, 50, 2
    FlashForms f4, 2000, 50, 2
    flashforms f4b, 2000, 50, 2
    FlashForms f7, 2000, 50, 2
    DOF 105 ,DOFPulse
    DOF 106 ,DOFPulse
    PlaySong "DBZ_End"
    PlaySong "DBZ_Multiball"
    StartJackpots
    AddMultiball 5
    SaiyanMultiball = True
    bMultiBallMode = True   
    D1.Text =      "         SAIYAN MULTIBALL       "
    DisplayB2SText "         SAIYAN MULTIBALL       "
    'vpmtimer.addtimer 2500, "TargetsLightOff '"
	TargetsLightOffTimer.Interval = 2500
	TargetsLightOffTimer.Enabled	= TRUE  
  End If


  If Saiyan = 4 Then
     AddScore 4000
     D1.Text =      "        SAIYAN LEVEL 4          "
     DisplayB2SText "        SAIYAN LEVEL 4          " 
     Playsound "burning_charge"
     SetLightColor L81, yellow, 80
     SetLightColor L91, yellow, 80
     SetLightColor L101, yellow, 80
     SetLightColor L111, yellow, 80
     SetLightColor L121, yellow, 80
     SetLightColor L131, yellow, 80

     L81.State = 2
     L91.State = 2
     L101.State = 2
     L111.State = 2
     L121.State = 2
     L131.State = 2 
    FlashForms f3, 2000, 50, 2
    flashforms f3b, 2000, 50, 2
    FlashForms f4, 2000, 50, 2
    flashforms f4b, 2000, 50, 2
    FlashForms f7, 2000, 50, 2
    DOF 105 ,DOFPulse
    DOF 106 ,DOFPulse
    'vpmtimer.addtimer 2500, "TargetsLightOff '"
	TargetsLightOffTimer.Interval = 2500
	TargetsLightOffTimer.Enabled	= TRUE      
  End If


  If Saiyan = 3 Then
     AddScore 3000 
     D1.Text =      "        SAIYAN LEVEL 3          "
     DisplayB2SText "        SAIYAN LEVEL 3          "  
     Playsound "burning_charge"
     startB2S(29)
     SetLightColor L81, yellow, 50
     SetLightColor L91, yellow, 50
     SetLightColor L101, yellow, 50
     SetLightColor L111, yellow, 50
     SetLightColor L121, yellow, 50
     SetLightColor L131, yellow, 50

     L81.State = 2
     L91.State = 2
     L101.State = 2
     L111.State = 2
     L121.State = 2
     L131.State = 2 
    FlashForms f3, 2000, 50, 2
    flashforms f3b, 2000, 50, 2
    FlashForms f4, 2000, 50, 2
    flashforms f4b, 2000, 50, 2
    FlashForms f7, 2000, 50, 2
    DOF 105 ,DOFPulse
    DOF 106 ,DOFPulse
    'vpmtimer.addtimer 2500, "TargetsLightOff '"
	TargetsLightOffTimer.Interval = 2500
	TargetsLightOffTimer.Enabled	= TRUE  
  End If


  If Saiyan = 2 Then
     AddScore 2000
     D1.Text =      "        SAIYAN LEVEL 2          "
     DisplayB2SText "        SAIYAN LEVEL 2          " 
     Playsound "burning_charge"
     SetLightColor L81, yellow, 30
     SetLightColor L91, yellow, 30
     SetLightColor L101, yellow, 30
     SetLightColor L111, yellow, 30
     SetLightColor L121, yellow, 30
     SetLightColor L131, yellow, 30

     L81.State = 2
     L91.State = 2
     L101.State = 2
     L111.State = 2
     L121.State = 2
     L131.State = 2  
    FlashForms f3, 2000, 50, 2
    flashforms f3b, 2000, 50, 2
    FlashForms f4, 2000, 50, 2
    flashforms f4b, 2000, 50, 2
    FlashForms f7, 2000, 50, 2
    DOF 105 ,DOFPulse
    DOF 106 ,DOFPulse
    'vpmtimer.addtimer 2500, "TargetsLightOff '"
	TargetsLightOffTimer.Interval = 2500
	TargetsLightOffTimer.Enabled	= TRUE   
  End If


  If Saiyan <= 1 Then
     AddScore 1000 
     D1.Text =      "        SAIYAN LEVEL 1          "
     DisplayB2SText "        SAIYAN LEVEL 1          " 
     Playsound "burning_charge"
     SetLightColor L81, yellow, 1
     SetLightColor L91, yellow, 1
     SetLightColor L101, yellow, 1
     SetLightColor L111, yellow, 1
     SetLightColor L121, yellow, 1
     SetLightColor L131, yellow, 1

     L81.State = 2
     L91.State = 2
     L101.State = 2
     L111.State = 2
     L121.State = 2
     L131.State = 2 
    FlashForms f3, 2000, 50, 2
    flashforms f3b, 2000, 50, 2
    FlashForms f4, 2000, 50, 2
    flashforms f4b, 2000, 50, 2
    FlashForms f7, 2000, 50, 2
    DOF 105 ,DOFPulse
    DOF 106 ,DOFPulse
    'vpmtimer.addtimer 2500, "TargetsLightOff '"
	TargetsLightOffTimer.Interval = 2500
	TargetsLightOffTimer.Enabled	= TRUE   
  End If
End Sub

Sub TargetsLightOffTimer_Timer()
	TargetsLightOffTimer.Enabled	= False
    TargetsLightOff
End Sub

Sub TargetsLightOff
    LightSeqTargets.StopPlay
    L81.State = 0
    L91.State = 0
    L101.State = 0
    L111.State = 0
    L121.State = 0
    L131.State = 0
    TargetsModeOn = False
    FlashForms f3, 50, 50, 2
    flashforms f3b, 50, 50, 2
    FlashForms f4, 50, 50, 2
    flashforms f4b, 50, 50, 2
    FlashForms f7, 50, 50, 2
    DOF 105 ,DOFPulse
    DOF 106 ,DOFPulse
    Playsound "DBZ_aura"
    vpmtimer.addtimer 1000, "Aura '"
End Sub

Sub Aura
    FlashForms f3, 4500, 50, 2
    flashforms f3b, 4500, 50, 2
    FlashForms f4, 4500, 50, 2
    flashforms f4b, 4500, 50, 2
    FlashForms f7, 4500, 50, 2
    DOF 105 ,DOFPulse
    DOF 106 ,DOFPulse
    AuraTimer.interval = 4600
    AuraTimer.enabled = 1
End Sub


Sub AuraTimer_Timer
     f3.State = 0
     f3.State = 0
     f4.State = 0
     f4.State = 0
     f7.State = 0
     AuraTimer.enabled = 0
End Sub


Sub ResetSaiyanMultiball
    D1.Text =      "     END SAIYAN MULTIBALL       "
    DisplayB2SText "     END SAIYAN MULTIBALL       "
    SaiyanMultiball = False
    Saiyan = 0
End Sub







Sub UnaBolaMas()
    BallsOnPlayfield = BallsOnPlayfield -1
    CreateNewBall()
    bAutoPlunger = True
    bBallInPlungerLane = True
End Sub




Dim ComboReady
sub ComboblinkLight(dummy)
    ComboReady = True
    Combo1.BlinkInterval = 180
    Combo2.BlinkInterval = 180
    Combo2b.BlinkInterval = 180
    Combo3.BlinkInterval = 180
    Combo3b.BlinkInterval = 180
    Combo4.BlinkInterval = 180
    Combo1.state=2
    Combo2.state=2
    Combo2b.state=2
    Combo3.state=2
    Combo3b.state=2
    Combo4.state=2
    ComboblinkLightTimer.Enabled = True
end sub

sub ComboblinkLightTimer_timer()
    Combo1.state=0
    Combo2.state=0
    Combo2b.state=0
    Combo3.state=0
    Combo3b.state=0
    Combo4.state=0
    ComboReady = False
    LastSwitchHit = "Sw28"
    me.enabled=0
end sub














Sub LaneSounds()
If PlayersPlayingGame Then
Dim tmp
     tmp = INT(RND * 3)
    Select Case tmp
        Case 0:PlaySound "Lane_1"
        Case 1:PlaySound "Lane_2"
        Case 2:PlaySound "Lane_3"
    End Select
End If
End Sub


Sub TargetsSounds()
Dim tmp
     tmp = INT(RND * 8)
    Select Case tmp
        Case 0:PlaySound "Gok_0"
        Case 1:PlaySound "Gok_1"
        Case 2:PlaySound "Gok_2"
        Case 3:PlaySound "Gok_3"
        Case 4:PlaySound "Gok_4"
        Case 5:PlaySound "Gok_5"
        Case 6:PlaySound "Gok_6"
        Case 7:PlaySound "Gok_7"
    End Select
End Sub













Sub FixTargetTimer_Timer()
	' Disable this timer.
	FixTargetTimer.Enabled = FALSE
	' Raise the three drop targets behind the captive ball.
	C1.Isdropped = 0
	C2.Isdropped = 0
	C3.Isdropped = 0
End Sub

Dim AttractMessagesTimerUserData
Sub AttractMessagesTimer_Timer()
'    FixTargetTimer.enabled = 1
	AttractMessagesTimerUserData = AttractMessagesTimerUserData + 1
	Select Case AttractMessagesTimerUserData
		Case 0:
			D1.Text = "    DRAGON BALL Z     "
            DisplayB2SText "          DRAGON BALL Z         "
		Case 1:
			D1.Text = "               GAME OVER                "
            DisplayB2SText "           GAME OVER            "
		Case 2:
			D1.Text = "       OLY PRESENTS     "
            DisplayB2SText "        OLY PRESENTS         "
		Case 3:
			D1.Text = "DRAGON BALL Z"
            DisplayB2SText "          DRAGON BALL Z         "
		Case 4:
			D1.Text = "       BY JAVIER        "
            DisplayB2SText "            BY JAVIER             "
		Case 5:
			D1.Text = "   ARTWORK UPDATED BY OLY  "
            DisplayB2SText "   ARTWORK UPDATED BY OLY      "
		Case 6:
			D1.Text = "          BANDAI           "
            DisplayB2SText "            BANDAI             "
		Case 7:
			D1.Text = "       SPECIAL THANKS TO        "
            DisplayB2SText "        SPECIAL THANKS TO       "
		Case 8:
			D1.Text = "    JAVIER / DEV TEAM / JP SALAS  "
            DisplayB2SText "   JAVIER / DEV TEAM / JP SALAS  "
		Case 9: 
			D1.Text = "   AND THE WHOLE COMMUNITY OF VP  "
            DisplayB2SText "  AND THE WHOLE COMMUNITY OF VP  "
		Case 10:
			D1.Text = "     * GAME RULES *     "
            DisplayB2SText "         * GAME RULES *          "
		Case 11:
			D1.Text = "     HIT BUMPERS      " 
            DisplayB2SText "          HIT BUMPERS           "
		Case 12:
			D1.Text = "        FOR EXTRABALL        " 
            DisplayB2SText "         FOR EXTRABALL          "
		Case 13:
			D1.Text = "     COLLECT LIGHT LOCKS      "
            DisplayB2SText "      COLLECT LIGHT LOCKS        "
		Case 14:
			D1.Text = "    FOR MULTIBALL    "
            DisplayB2SText "         FOR MULTIBALL          "
		Case 15:
			D1.Text = "    COMPLETE DRAGON BALLS   "
            DisplayB2SText "     COMPLETE DRAGON BALLS      "
		Case 16:
			D1.Text = "    FOR SHENLONG MULTIBALL    "
            DisplayB2SText "     FOR SHENLONG MULTIBALL     "
		Case 17:
			D1.Text = "      LEVEL UP SAIYAN      "
            DisplayB2SText "        LEVEL UP SAIYAN         "
		Case 18:
			D1.Text = "      FOR SAIYAN MULTIBALL      "
            DisplayB2SText "      FOR SAIYAN MULTIBALL      "
		Case 19:
			D1.Text = " ** GOOD LUCK ON YOUR GAME ** "
            DisplayB2SText "  ** GOOD LUCK ON YOUR GAME **  "
		Case 20:
             If Score(1) Then
                D1.Text = "PLAYER 1"& Score(1)
                DisplayB2SText "PLAYER 1"& Score(1)
             End If
             If Score(2) Then
               D1.Text = "PLAYER 2"& Score(2)
                DisplayB2SText "PLAYER 1"& Score(2)
             End If
             If Score(3) Then
               D1.Text = "PLAYER 3"& Score(3)
                DisplayB2SText "PLAYER 1"& Score(3)
             End If
             If Score(4) Then
               D1.Text = "PLAYER 4"& Score(4)
                DisplayB2SText "PLAYER 1"& Score(4)
             End If

          'coins or freeplay
             If bFreePlay Then
                D1.Text = "FREE PLAY"
                DisplayB2SText "           FREE PLAY            "
              Else
             If Credits> 0 Then
               DisplayB2SText "    CREDITS " &credits& "     PRESS START"
               D1.Text = "CREDITS " &credits& " PRESS START"
              Else
               DisplayB2SText "    CREDITS " &credits& "     INSERT COIN"
               D1.Text = "CREDITS " &credits& " INSERT COIN"
             End If
             End If
        Case 21:
			If (Score(1) = 0) And (Credits = 0) Then
				D1.Text = "      PLEASE INSERT COIN      "
                DisplayB2SText "      PLEASE INSERT COIN      "
			ElseIf (Score(1) = 0) And (Credits > 0) Then
				D1.Text = "    PUSH START BUTTON TO PLAY   "
                DisplayB2SText "    PUSH START BUTTON TO PLAY   "
			Else
                DisplayB2SText " LAST SCORE WAS -> " & " " & Score(1)', 0, , , -1
				D1.Text = " LAST SCORE WAS " & FormatScore(Score(1)) & String(8 - Int((Len(Score(1)) - 1) / 2), " ")

			End If

		Case 22:
             DisplayB2SText " HIGHSCORES  "& "1> " & HighScoreName(0) & " :" & HighScore(0)', 0, , , -1
             D1.Text =  " HIGHSCORES"& " 1> " & HighScoreName(0) & " " & FormatNumber(HighScore(0), 0, , , -1)
		Case 23:
             DisplayB2SText " HIGHSCORES  "& "2> " & HighScoreName(1) & " :" & HighScore(1)', 0, , , -1
             D1.Text =  " HIGHSCORES"& " 2> " & HighScoreName(1) & " " & FormatNumber(HighScore(1), 0, , , -1)
		Case 24:
             DisplayB2SText " HIGHSCORES  "& "3> " & HighScoreName(2) & " :" & HighScore(2)', 0, , , -1
             D1.Text =  " HIGHSCORES"& " 3> " & HighScoreName(2) & " " & FormatNumber(HighScore(2), 0, , , -1)
		Case 25:
             DisplayB2SText " HIGHSCORES  "& "4> " & HighScoreName(3) & " :" & HighScore(3)', 0, , , -1
             D1.Text =  " HIGHSCORES"& " 4> " & HighScoreName(3) & " " & FormatNumber(HighScore(3), 0, , , -1)
			 AttractMessagesTimerUserData = 0
	End Select
End Sub



'*****************
'Mensajes varios
'*****************



'multi 3 balls
Dim MultiballMSN
Sub MultiballMSNTimer_Timer
   MultiballMSN = MultiballMSN + 1 
 Select Case MultiballMSN
        Case 1:
              DisplayB2SText "           MULTIBALL            "
              D1.text = "           MULTIBALL            "
        Case 2:
              DisplayB2SText "     "
              D1.text = "      "      
         Case 3:
              DisplayB2SText "           MULTIBALL            "
              D1.text = "           MULTIBALL            "
        Case 4:
              DisplayB2SText "     "
              D1.text = "      "
        Case 5:
              DisplayB2SText "           MULTIBALL            "
              D1.text = "           MULTIBALL            "
        Case 6:
              DisplayB2SText "     "
              D1.text = "      "
        Case 7:
              DisplayB2SText "           MULTIBALL            "
              D1.text = "           MULTIBALL            "
        Case 8:
              DisplayB2SText "     "
              D1.text = "      "
        Case 9:
              DisplayB2SText "           MULTIBALL            "
              D1.text = "           MULTIBALL            "
        Case 10:
              DisplayB2SText "     "
              D1.text = "      "
        Case 11:
              DisplayB2SText "           MULTIBALL            "
              D1.text = "           MULTIBALL            "
        Case 12:
              DisplayB2SText "     "
              D1.text = "      "
        Case 13:
              DisplayB2SText "           MULTIBALL            "
              D1.text = "           MULTIBALL            "
        Case 14:
              DisplayB2SText "     "
              D1.text = "      "
        Case 15:
              DisplayB2SText "           MULTIBALL            "
              D1.text = "           MULTIBALL            "
        Case 16:
              DisplayB2SText "     "
              D1.text = "      "
        Case 17:
              DisplayB2SText "           MULTIBALL            "
              D1.text = "           MULTIBALL            "
        Case 18:
              DisplayB2SText "     "
              D1.text = "      "
              MultiballMSN = 0
              MultiballMSNTimer.enabled = 0
 End Select
              DisplayB2SText "     "
End Sub






Dim SHENLONGMSN
Sub SHENLONGMSNTimer_Timer
   SHENLONGMSN = SHENLONGMSN + 1 
 Select Case SHENLONGMSN
        Case 1:
              DisplayB2SText "      SHENLONG MULTIBALL        "
              D1.text = "      SHENLONG MULTIBALL        "
        Case 2:
              DisplayB2SText "     "
              D1.text = "      "      
         Case 3:
              DisplayB2SText "      SHENLONG MULTIBALL        "
              D1.text = "      SHENLONG MULTIBALL        "
        Case 4:
              DisplayB2SText "     "
              D1.text = "      "
        Case 5:
              DisplayB2SText "      SHENLONG MULTIBALL        "
              D1.text = "      SHENLONG MULTIBALL        "
        Case 6:
              DisplayB2SText "     "
              D1.text = "      "
        Case 7:
              DisplayB2SText "      SHENLONG MULTIBALL        "
              D1.text = "      SHENLONG MULTIBALL        "
        Case 8:
              DisplayB2SText "     "
              D1.text = "      "
        Case 9:
              DisplayB2SText "      SHENLONG MULTIBALL        "
              D1.text = "      SHENLONG MULTIBALL        "
        Case 10:
              DisplayB2SText "     "
              D1.text = "      "
        Case 11:
              DisplayB2SText "      SHENLONG MULTIBALL        "
              D1.text = "      SHENLONG MULTIBALL        "
        Case 12:
              DisplayB2SText "     "
              D1.text = "      "
        Case 13:
              DisplayB2SText "      SHENLONG MULTIBALL        "
              D1.text = "      SHENLONG MULTIBALL        "
        Case 14:
              DisplayB2SText "     "
              D1.text = "      "
        Case 15:
              DisplayB2SText "      SHENLONG MULTIBALL        "
              D1.text = "           MULTIBALL            "
        Case 16:
              DisplayB2SText "     "
              D1.text = "      "
        Case 17:
              DisplayB2SText "      SHENLONG MULTIBALL        "
              D1.text = "      SHENLONG MULTIBALL        "
        Case 18:
              DisplayB2SText "     "
              D1.text = "      "
              SHENLONGMSN = 0
              SHENLONGMSNTimer.enabled = 0
 End Select
              DisplayB2SText "     "
End Sub




'Combo 
Dim ComboMSN
Sub ComboMSNTimer_Timer
   ComboMSN = ComboMSN + 1 
 Select Case ComboMSN
        Case 1:
              DisplayB2SText "            COMBO               "
              D1.text = "            COMBO               "
        Case 2:
              DisplayB2SText "     "
              D1.text = "      "      
         Case 3:
              DisplayB2SText "            COMBO               "
              D1.text = "            COMBO               "
        Case 4:
              DisplayB2SText "     "
              D1.text = "      "
        Case 5:
              DisplayB2SText "            COMBO               "
              D1.text = "            COMBO               "
        Case 6:
              DisplayB2SText "     "
              D1.text = "      "
        Case 7:
              DisplayB2SText "            COMBO               "
              D1.text = "            COMBO               "
        Case 8:
              DisplayB2SText "     "
              D1.text = "      "

 End Select
              ComboMSN = 0
              ComboMSNTimer.enabled = 0
              DisplayB2SText "     "
End Sub


'Jackpot
Dim JackpotMSN
Sub JackpotMSNTimer_Timer
   JackpotMSN = JackpotMSN + 1 
 Select Case JackpotMSN
        Case 1:
              DisplayB2SText "       JACKPOT            " & Jackpot(CurrentPlayer)
              D1.text = "       JACKPOT            " & Jackpot(CurrentPlayer)
        Case 2:
              DisplayB2SText "     "
              D1.text = "      "      
         Case 3:
              DisplayB2SText "       JACKPOT            " & Jackpot(CurrentPlayer)
              D1.text = "       JACKPOT            " & Jackpot(CurrentPlayer)
        Case 4:
              DisplayB2SText "     "
              D1.text = "      "
        Case 5:
              DisplayB2SText "       JACKPOT            " & Jackpot(CurrentPlayer)
              D1.text = "       JACKPOT            " & Jackpot(CurrentPlayer)
        Case 6:
              DisplayB2SText "     "
              D1.text = "      "
        Case 7:
              DisplayB2SText "       JACKPOT            " & Jackpot(CurrentPlayer)
              D1.text = "       JACKPOT            " & Jackpot(CurrentPlayer)
        Case 8:
              DisplayB2SText "     "
              D1.text = "      "
        Case 9:
              DisplayB2SText "       JACKPOT            " & Jackpot(CurrentPlayer)
              D1.text = "       JACKPOT            " & Jackpot(CurrentPlayer)
        Case 10:
              DisplayB2SText "     "
              D1.text = "      "
        Case 11:
              DisplayB2SText "       JACKPOT            " & Jackpot(CurrentPlayer)
              D1.text = "       JACKPOT            " & Jackpot(CurrentPlayer)
        Case 12:
              DisplayB2SText "     "
              D1.text = "      "

 End Select
              JackpotMSN = 0
              JackpotMSNTimer.enabled = 0
              DisplayB2SText "     "
End Sub




Dim letsGoMSN
Sub letsGo_Timer
    letsGoMSN = letsGoMSN + 1
   Select Case letsGoMSN
          Case 0: DisplayB2SText "           LETS GO              "
                  D1.text = "           LETS GO              " 
          Case 1: DisplayB2SText "                                "
                  D1.text = "                                "
          Case 2: DisplayB2SText "           LETS GO              "
                  D1.text = "           LETS GO              "
          Case 3: DisplayB2SText "                                "
                  D1.text = "                                "
          Case 4: DisplayB2SText "           LETS GO              "
                  D1.text = "           LETS GO              "
          Case 5: DisplayB2SText "                                "
                  D1.text = "                                "
          Case 6: DisplayB2SText "           LETS GO              "
                  D1.text = "           LETS GO              "
          Case 7: DisplayB2SText "                                "
                  D1.text = "                                "
          Case 8: DisplayB2SText "           LETS GO              "
                  D1.text = "           LETS GO              "
          Case 9: DisplayB2SText "                                "
                  D1.text = "                                "
   End Select
         letsGo.enabled = 0
End Sub





