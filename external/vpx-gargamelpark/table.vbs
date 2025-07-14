' **********************************************
'               VISUAL PINBALL X8
'          JPSalas Gargamel Park v5.5.0
' **********************************************

Option Explicit
Randomize

' Sound and music volumes
Const SongVolume = 0.05 ' 1 is full volume, but I set it quite low to listen better the other sounds since I use headphones, adjust to your setup :)
Const VoiceVolume = 1   ' Volume of the voices

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
'or keep it False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = True
'Use FlexDMD also in desktop mode
Const AlwaysUseFlexDMD = True

Const BallSize = 50
Const BallMass = 1

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

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If
If AlwaysUseFlexDMD then UseFlexDMD = True

' Define any Constants
Const cGameName = "gargamel_park"
Const TableName = "Gargamel_Park" 'used for saving the highscores
Const myVersion = "5.01"
Const MaxPlayers = 4
Const BallSaverTime = 20 'in seconds
Const MaxMultiplier = 6  '6x is the max in this game
Const BallsPerGame = 5

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusMultiplier(4)
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue
Dim x

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock
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
Dim bAutoPlunger
Dim bSkillshotReady

'Define This Table objects and variables

Dim plungerIM, plungerIM2, cbLeft
Dim Bonus

Dim bExtraBallWonThisBall
Dim MusicChannelInUse
Dim CurrentMusicTunePlaying

Dim NCount
Dim R1Count
Dim R2Count
Dim K1Count
Dim K2Count
Dim cbCount
Dim K6Count
Dim P4Count
Dim P2Count

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    DOF 110, DOFOn
    Dim i
    Randomize

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 30 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd "fx_kicker", "fx_solenoid"
        .CreateEvents "plungerIM"
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    'load saved values, highscore, names, jackpot
    Credits = 1
    Loadhs

    'Init main variables
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    ' initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = FALSE 'we want coins

    ' initialse any other flags
    bOnTheFirstBall = FALSE
    bBallInPlungerLane = FALSE
    bBallSaverActive = FALSE
    bBallSaverReady = FALSE
    bMultiBallMode = FALSE
    bGameInPlay = FALSE
    bMusicOn = TRUE
    bAutoPlunger = FALSE
    BallsOnPlayfield = 0
    BallsInLock = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = FALSE
    If Credits > 0 Then DOF 131, DOFOn
    EndOfGame()
    Realtime.Enabled = 1
    LoadLUT
End Sub

Sub Realtime_Timer
    GIUpdate
    RollingUpdate
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If hsbModeActive Then EnterHighScoreKey(keycode):Exit Sub

    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT

    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
        Credits = Credits + 1
        DOF 131, DOFOn
        If(Tilted = FALSE)Then
            DMDFlush
            DMD "_", CL("CREDITS: " & Credits), "", eNone, eNone, eNone, 500, TRUE, "fx_coin"
            If Credits > 5 Then
                PlaySound "ga_iamrich"
            Else
                PlaySound "ga_diamond1"
            End If
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        PlaySoundAt "fx_plungerpull", Plunger
        Plunger.Pullback
    End If

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 1, 0.25:CheckTilt
        If keycode = MechanicalTilt Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = TRUE))Then

                If(bFreePlay = TRUE)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        If Credits < 1 Then DOF 131, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMDFlush
                            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 500, TRUE, "ga_diamonds"
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = TRUE)Then
                    If(BallsOnPlayfield = 0)Then
                        ResetForNewGame()
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            If Credits < 1 Then DOF 131, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 500, TRUE, "ga_diamonds"
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)

' Table specific

' test keys

End Sub

Sub Table1_KeyUp(ByVal keycode)

    If hsbModeActive Then Exit Sub

    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then SolLFlipper 0
        If keycode = RightFlipperKey Then SolRFlipper 0
    End If

    If keycode = PlungerKey Then
        PlaySoundAt "fx_plunger", Plunger
        Plunger.Fire
    End If
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
' Special JP Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFXDOF("fx_flipperup", 101, DOFOn, DOFContactors), 0, 1, -0.05, 0.15
        LeftFlipper.RotateToEnd
        LeftFlipper1.RotateToEnd
        LeftFlipper2.RotateToEnd
        LeftFlipper3.RotateToEnd
        LeftFlipper4.RotateToEnd
        LeftFlipperOn = 1
        RotateLaneLightsLeft
    Else
        PlaySound SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFContactors), 0, 1, -0.05, 0.15
        LeftFlipper.RotateToStart
        LeftFlipper1.RotateToStart
        LeftFlipper2.RotateToStart
        LeftFlipper3.RotateToStart
        LeftFlipper4.RotateToStart
        LeftFlipperOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFXDOF("fx_flipperup", 102, DOFOn, DOFContactors), 0, 1, 0.05, 0.15
        RightFlipper.RotateToEnd
        RightFlipper1.RotateToEnd
        RightFlipper2.RotateToEnd
        RightFlipper3.RotateToEnd
        RightFlipperOn = 1
        RotateLaneLightsRight
    Else
        PlaySound SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFContactors), 0, 1, 0.05, 0.15
        RightFlipper.RotateToStart
        RightFlipper1.RotateToStart
        RightFlipper2.RotateToStart
        RightFlipper3.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper2_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper3_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper4_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper2_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper3_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
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

Sub RotateLaneLightsLeft
    Dim TempState
    TempState = LightSS.State
    LightSS.State = LightM.State
    LightSpin.State = LightM.State
    LightM.State = LightU.State
    LightU.State = LightR.State
    LightR.State = LightF.State
    LightF.State = TempState
End Sub

Sub RotateLaneLightsRight
    Dim TempState
    TempState = LightF.State
    LightF.State = LightR.State
    LightR.State = LightU.State
    LightU.State = LightM.State
    LightM.State = LightSS.State
    LightSS.State = TempState
    LightSpin.State = TempState
End Sub

'*********
' TILT
'*********

'NOTE: The Game Timer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = TRUE
    If(Tilt > TiltSensitivity)AND(Tilt < 15)Then 'show a warning
        DMD "_", CL("CAREFUL"), "", eNone, eBlinkFast, eNone, 500, TRUE, ""
    End if
    If Tilt > 15 Then 'If more that 15 then TILT the table
        Tilted = TRUE
        'display Tilt
        DMDFlush
        DMD CL("TILT"), "", "", eBlinkFast, eNone, eNone, 200, FALSE, ""
        DMD "", CL("TILT"), "", eBlinkFast, eNone, eNone, 200, FALSE, ""
        DisableTable TRUE
        TiltRecoveryTimer.Enabled = TRUE 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        LeftFlipper1.RotateToStart
        RightFlipper1.RotateToStart
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        Bumper3.Threshold = 100
        Bumper4.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        Bumper3.Threshold = 1
        Bumper4.Threshold = 1
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
        EndOfBall()
        Me.Enabled = FALSE
    End If
' else retry (checks again in another second or so)
End Sub

'***************
' Random Music
'***************

Dim currentsong, song(21), SongLength(21)
currentsong = 0
For x = 1 to 21
    song(x) = "M_Song" &x
Next

SongLength(1) = 189000  '3:09
SongLength(2) = 230000  '3:50
SongLength(3) = 102000  '1:42
SongLength(4) = 222000  '3:42
SongLength(5) = 187000  '3:07
SongLength(6) = 234000  '3:54
SongLength(7) = 233000  '3:53
SongLength(8) = 230000  '3:50
SongLength(9) = 209000  '3:29
SongLength(10) = 166000 '2:46
SongLength(11) = 225000 '3:45
SongLength(12) = 191000 '3:12
SongLength(13) = 189000 '3:09
SongLength(14) = 209000 '3:29
SongLength(15) = 240000 '4:00
SongLength(16) = 218000 '3:38
SongLength(17) = 203000 '3:23
SongLength(18) = 188000 '3:08
SongLength(19) = 188000 '3:08
SongLength(20) = 210000 '3:30
SongLength(21) = 202000 '3:22

Sub PlaySong            'random
    Dim tmp
    If bMusicOn Then
        StopSound song(currentsong)
        tmp = RndNbr(21)
        PlaySound Song(tmp), 0, SongVolume
        ReplaySong.Interval = SongLength(tmp)
        ReplaySong.Enabled = 0
        ReplaySong.Enabled = 1
        currentsong = tmp
    End If
End Sub

Sub ReplaySong_Timer:PLaySong:End Sub

'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim OldGiState
OldGiState = -1  'start witht the Gi off

Sub ChangeGi(Gi) 'changes the gi color
    Dim gilight
    Select Case Gi
        Case "Normal"
            For each giLight in aGILights
                giLight.color = RGB(255, 197, 143)
                giLight.colorfull = RGB(255, 252, 224)
            Next
        Case "Green"
            For each giLight in aGILights
                giLight.color = RGB(0, 18, 0)
                giLight.colorfull = RGB(0, 180, 0)
            Next
        Case "Red"
            For each giLight in aGILights
                giLight.color = RGB(18, 0, 0)
                giLight.colorfull = RGB(180, 0, 0)
            Next
    End Select
End Sub

Sub GIUpdate
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = -1 Then
            GiOff
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    Dim bulb
    PlaySound "fx_gion"
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aFarolas
        bulb.State = 2
    Next
End Sub

Sub GiOff
    Dim bulb
    PlaySound "fx_gioff"
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aFarolas
        bulb.State = 0
    Next
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
Const maxvel = 46 'max ball velocity
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

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 5
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

'*****************************
' Ball 2 Ball Collision Sound
'*****************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_MetalWire":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_LongBands_Hit(idx):PlaySoundAtBall "fx_rubber_longband":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aRubber_Pegs_Hit(idx):PlaySoundAtBall "fx_rubber_peg":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub
Sub aGates_Hit(idx)
    PlaySoundAtBall "fx_Gate"
    If(idx = 1)AND(bMultiBallMode = FALSE)Then bAutoPlunger = FALSE
End Sub

' Ramp Soundss
Sub aRHelps_Hit(idx)
    ActiveBall.VelY = 0
    ActiveBall.VelX = 0
    PlaySoundAt"fx_balldrop", aRHelps(idx)
End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = TRUE

    'resets the score display, and turn off attrack mode
    StopAttractMode
    GiOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = TRUE
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    ' initialise any other flags
    bMultiBallMode = FALSE
    Tilt = 0

    ' initialise Game variables
    Game_Init()

    ' you may wish to start some music, play a sound, do whatever at this point

    ' set up the start delay to handle any Start of Game Attract Sequence
    FirstBallDelayTimer.Enabled = TRUE
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBallDelayTimer_Timer
    ' stop the timer
    Me.Enabled = FALSE
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

    ' reset any drop targets, lights, game modes etc..
    'LightShootAgain.State = 0
    Bonus = 0
    bExtraBallWonThisBall = FALSE
    ResetNewBallLights()

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = TRUE

    'and the skillshot
    bSkillShotReady = TRUE

'Change the music ?

End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.Createball

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 109, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4

    ' if there is 2 or more balls then set the multibal flag
    If BallsOnPlayfield > 1 Then
        bMultiBallMode = TRUE
        DOF 108, DOFOn
        DOF 110, DOFOff
    End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = TRUE
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    bAutoPlunger = TRUE

    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        CreateNewBall()
        mBalls2Eject = mBalls2Eject -1
        If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
            Me.Enabled = FALSE
        End If
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded
'
Sub EndOfBall()
    Dim BonusDelayTime
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = FALSE

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)
    If(Tilted = FALSE)Then
        Dim AwardPoints

        ' add in any bonus points (multipled by the bunus multiplier)
        AwardPoints = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer)
        AddScore AwardPoints
        debug.print "Bonus Points = " & AwardPoints

        DMD "_", CL("BONUS: " & BonusPoints(CurrentPlayer) & " X" & BonusMultiplier(CurrentPlayer)), "", eNone, eBlink, eNone, 1000, TRUE, ""

        ' add a bit of a delay to allow for the bonus points to be added up
        BonusDelayTime = 1000
    Else
        'no bonus to count so move quickly to the next stage
        BonusDelayTime = 20
    End If
    ' start the end of ball timer which allows you to add a delay at this point
    EndOfBall2Timer.Interval = BonusDelayTime
    EndOfBall2Timer.Enabled = TRUE
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the currentplayer)
'
Sub EndOfBall2Timer_Timer()
    ' disable the timer
    Me.Enabled = FALSE
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    Tilted = FALSE
    Tilt = 0
    DisableTable FALSE 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0)Then
        debug.print "Extra Ball"
        DMD "_", CL(("EXTRA BALL")), "d_border", eNone, eBlink, eNone, 1000, TRUE, ""

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            debug.print "No More Balls, High Score Entry"

            ' Submit the currentplayers score to the High Score system
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

    debug.print "EndOfBall - Complete"

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

    debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode

        ' set the machine into game over mode
        EndOfGame()

    ' you may wish to put a Game Over message on the

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    ' just ended your game then play the end of game tune
    'PlaySong

    debug.print "End Of Game"
    bGameInPLay = FALSE

    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all modes - eject locked balls

    ' set any lights for the attract mode
    GiOff
    StartAttractMode 1
' you may wish to light any Game Over Light you may have
End Sub

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
    DOF 141, DOFPulse
    BallsOnPlayfield = BallsOnPlayfield - 1
    ' pretend to knock the ball into the ball storage mech
    PlaySound "fx_drain"

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = TRUE)AND(Tilted = FALSE)Then

        ' is the ball saver active,
        If(bBallSaverActive = TRUE)Then

            ' yep, create a new ball in the shooters lane
            '   CreateNewBall
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
        ' you may wish to put something on a display or play a sound at this point

        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            '
            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = TRUE)then
                    ' not in multiball mode any more
                    bMultiBallMode = FALSE
                    DOF 108, DOFOff
                    DOF 110, DOFOn
                'ChangeGi "Normal"
                ' you may wish to change any music over at this point and
                ' turn off any multiball specific lights
                'PlaySong "m_main"
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
                ' handle the end of ball (change player, high score entry etc..)
                EndOfBall()
                ' End Modes and timers
                ChangeGi "Normal"
            End If
        End If
    End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
    debug.print "ball in plunger lane"
    ' some sound according to the ball position
    PlaySoundAt "fx_sensor", swPlungerRest
    bBallInPlungerLane = TRUE
    ' turn on Launch light is there is one
    'LaunchLight.State = 2
    ' if autoplunger is on then kick the ball
    If bAutoPlunger = TRUE Then
        DOF 133, DOFPulse
        DOF 104, DOFPulse
        plungerIM.AutoFire
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = TRUE)AND(BallSaverTime <> 0)And(bBallSaverActive = FALSE)Then
        EnableBallSaver BallSaverTime
    End If
    'Start the skillshot if ready
    If bSkillShotReady Then
        ResetSkillShotTimer.Interval = 1000 * 5 ' 5 seconds
        ResetSkillShotTimer.Enabled = TRUE
        LightSeqSkillshot.Play SeqAllOff
        LightSeqSkillshotHit.Play SeqBlinking, , 5, 150
    'PlaySound a sound
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = FALSE
' turn off LaunchLight
'LaunchLight.State = 0
End Sub

Sub EnableBallSaver(seconds)
    debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = TRUE
    bBallSaverReady = FALSE
    ' start the timer
    BallSaverTimer.Interval = 1000 * seconds
    BallSaverTimer.Enabled = TRUE
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 125
    LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimer_Timer()
    debug.print "Ballsaver ended"
    Me.Enabled = FALSE
    ' clear the flag
    bBallSaverActive = FALSE
    ' if you have a ball saver light then turn it off at this point
    LightShootAgain.State = 0
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
'
Sub AddScore(points)
    If(Tilted = FALSE)Then
        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + points
        ' update the score displays
        DMDScore
    End if

' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board
'
Sub AddBonus(points)
    If(Tilted = FALSE)Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
        ' update the score displays
        DMDScore
    End if

' you may wish to check to see if the player has gotten a replay
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If(Tilted = FALSE)Then

        If(bMultiBallMode = TRUE)Then
            Jackpot = Jackpot + points
        ' you may wish to limit the jackpot to a upper limit, ie..
        '	If (Jackpot >= 6000) Then
        '		Jackpot = 6000
        ' 	End if
        End if
    End if
End Sub

' Will increment the Bonus Multiplier to the next level
'
Sub IncrementBonusMultiplier()
    Dim NewBonusLevel

    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) < MaxMultiplier)then
        ' then set it the next next one AND set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + 1
        SetBonusMultiplier(NewBonusLevel)
    End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly
'
Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level

    ' If the multiplier is 1 then turn off all the bonus lights
    If(BonusMultiplier(CurrentPlayer) = 1)Then
        LightBonus2x.State = 0
        LightBonus3x.State = 0
        LightBonus4x.State = 0
        LightBonus5x.State = 0
        LightBonus6x.State = 0
    Else
        ' there is a bonus, turn on all the lights upto the current level
        If(BonusMultiplier(CurrentPlayer) >= 2)Then
            If(BonusMultiplier(CurrentPlayer) >= 2)Then
                LightBonus2x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 3)Then
                LightBonus3x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 4)Then
                LightBonus4x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 5)Then
                LightBonus5x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 6)Then
                LightBonus6x.state = 1
            End If
        End If
    ' etc..
    End If
End Sub

Sub IncrementBonus(Amount)
    Dim Value
    AddBonus Amount * 1000
    Bonus = Bonus + Amount
End Sub

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        DOF 119, DOFPulse
        DOF 104, DOFPulse
        DMD "_", CL(("EXTRA BALL WON")), "d_border", eNone, eBlink, eNone, 1000, TRUE, "fx_Knocker"
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = TRUE
    END If
End Sub

Sub AwardSpecial()
    DOF 119, DOFPulse
    DOF 104, DOFPulse
    DMD "_", CL(("EXTRA GAME WON")), "d_border", eNone, eBlink, eNone, 1000, TRUE, "fx_Knocker"
    DOF 131, DOFOn
    Credits = Credits + 1
End Sub

Sub AwardJackpot()
    DOF 107, DOFPulse
    DMD CL(FormatScore(Jackpot)), CL(("JACKPOT")), "d_border", eBlinkFast, eBlink, eNone, 1000, TRUE, "vo_jackpot"
    AddScore Jackpot
    Jackpot = Jackpot + 30000
End Sub

Sub AwardSkillshot()
    DOF 118, DOFPulse
    DMD CL(FormatScore(SkillshotValue)), CL(("SKILLSHOT")), "d_border", eBlinkFast, eBlink, eNone, 1000, TRUE, "vo_skillshot"
    AddScore SkillshotValue
    ResetSkillShotTimer_Timer
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    On Error Resume Next
    x = LoadValue(TableName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 100000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 100000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 100000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 100000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(TableName, "Credits")
    If x > 15 Then x = 15
    If(x <> "")then Credits = CInt(x)Else Credits = 0 End If
    'x = LoadValue(TableName, "Jackpot")
    'If(x <> "") then Jackpot = CDbl(x) Else Jackpot = 200000 End If
    x = LoadValue(TableName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
    On Error Goto 0
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

Sub Reseths
    HighScore(0) = 100000
    HighScoreName(0) = "AAA"
    HighScore(1) = 100000
    HighScoreName(1) = "BBB"
    HighScore(2) = 100000
    HighScoreName(2) = "CCC"
    HighScore(3) = 100000
    HighScoreName(3) = "DDD"
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
    If Score(2) > tmp Then tmp = Score(2)
    If Score(3) > tmp Then tmp = Score(3)
    If Score(4) > tmp Then tmp = Score(4)

    If tmp > HighScore(1)Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
        DOF 131, DOFOn
    End If

    If tmp > HighScore(3)Then
        PlaySound "fx_knocker"
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = TRUE
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
    HighScoreFlashTimer.Enabled = TRUE
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
    HighScoreFlashTimer.Enabled = FALSE
    hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = TRUE
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
    HighScoreFlashTimer.Enabled = FALSE
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2)then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = TRUE
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = FALSE
    hsbModeActive = FALSE
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ")then
        hsEnteredName = "JPS"
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
        tmp = RL(FormatScore(Score(Currentplayer)))
        tmp1 = FL("PLAYER " &CurrentPlayer, "BALL " & Balls)
        tmp2 = ""
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
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "d_empty"
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

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    'info goes in a loop only stopped by the credits and the startkey

    DMD "", CL("GAME OVER"), "", eNone, eBlink, eNone, 2000, False, ""
    If Credits > 0 Then
        DMD CL("CREDITS " & Credits), CL("PRESS START"), "", eNone, eBlink, eNone, 2000, FALSE, ""
    Else
        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 2000, FALSE, ""
    End If
    DMD "        JPSALAS", "          PRESENTS", "d_jppresents", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 2000, False, ""
    DMD "      GARGAMEL      ", "        PARK        ", "d_title", eScrollLeft, eScrollright, eNone, 3000, False, ""
    DMD "", CL("ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD "HIGHSCORES", Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, FALSE, ""
    DMD "HIGHSCORES", "", "", eBlinkFast, eNone, eNone, 1000, FALSE, ""
    DMD "HIGHSCORES", "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, FALSE, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, FALSE, ""
    DMD "HIGHSCORES", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, FALSE, ""
    DMD "HIGHSCORES", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, FALSE, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 500, FALSE, ""
End Sub

Sub StartAttractMode(dummy)
    'PlaySong "m_attract"
    StartLightSeq
    DMDFlush
    ShowTableInfo
End Sub

Sub StopAttractMode
    Dim bulb
    DMDFlush
    LightSeqAttract.StopPlay
'LightSeqFlasher.StopPlay
'StopSong
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

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqAllOff
End Sub

Sub LightSeqSkillshotHit_PlayDone()
    LightSeqSkillshotHit.Play SeqBlinking, , 5, 150
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqDouble_PlayDone()
    LightSeqDouble.Play SeqRandom, 10, , 4000
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' tables walls and animations
Sub VPObjects_Init
    SelectMaze 'reset the mirror house to a random maze
    LeftRideWall.IsDropped = 1
    RightRideWall.IsDropped = 1
End Sub

' tables variables and modes init

Dim Mode

Sub Game_Init()
    bExtraBallWonThisBall = FALSE
    TurnOffPlayfieldLights()
    'Play some Music
    PlaySong
    'Init Variables
    SkillshotValue = 100000
    Jackpot = 60000
    RollerCoasterValue = 0
    InsaneHits = 0
    LightR1.State = 0
    ResetAzrael
    ResetAzrael_Lights
    ResetBumpers
    bFreeFallStarted = FALSE
    ffRounds = 0
    ffDir = 2 ' a positiv number is upwards, negative downwards, this is also the speed of the animation
    freefallHits = 0
    LightR4.State = 0
    ferrysHits = 0
    LightR5.State = 0
    ResetHaunted
    'Init Delays & timers
    fwheelTimer.Enabled = 1
'Skillshot Init
'MainModes Init()
End Sub

Sub ResetSkillShotTimer_Timer
    ResetSkillShotTimer.Enabled = FALSE
    bSkillshotReady = FALSE
    'Reset skillshot lights if any
    LightSeqSkillshot.StopPlay
    LightSeqSkillshotHit.StopPlay
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aMainLights
        a.State = 0
    Next
End Sub

Sub ResetNewBallLights()
End Sub

'****************
' Flasher Timers
'****************

Sub Flasher1Timer_Timer
    DOF 129, DOFPulse
    Flasher1.Visible = NOT Flasher1.Visible
End Sub

Sub Flasher2Timer_Timer
    DOF 128, DOFPulse
    Flasher2.Visible = NOT Flasher2.Visible
End Sub

Sub Flasher3Timer_Timer
    DOF 130, DOFPulse
    Flasher3.Visible = NOT Flasher3.Visible
End Sub

Sub RideRampTrigger_Hit:FlashRamp.State = 2:Me.TimerEnabled = 1:End Sub
Sub RideRampTrigger_Timer:FlashRamp.State = 0:Me.TimerEnabled = 0:End Sub

'**********************
' The House of Mirrors
'         or
' The Maze /labyrinth
'**********************

Sub MazeTrigger_Hit:FlashMirror.State = 2:End Sub

Sub MazeTrigger_UnHit:SelectMaze:FlashMirror.State = 0:End Sub

Sub MazeTrigger2_Hit
    Voices2_Hit
    ActiveBall.X = 36
    ActiveBall.VelX = 0
    ActiveBall.VelY = 15
End Sub

Sub SelectMaze
    Dim a
    a = INT(3 * RND)

    Lab1.Isdropped = 1
    Lab2.Isdropped = 1
    Lab3.Isdropped = 1

    Select Case a
        Case 0:lab1.IsDropped = 0:lab2.IsDropped = 1:lab3.IsDropped = 1
        Case 1:lab1.IsDropped = 1:lab2.IsDropped = 0:lab3.IsDropped = 1
        Case 2:lab1.IsDropped = 1:lab2.IsDropped = 1:lab3.IsDropped = 0
    End Select
End Sub

'**************
' Spin Wheel
'**************

Dim swheelSpeed, swheelStep
swheelSpeed = 1 + 10 * RND(1)
swheelStep = 1.1 + RND(1) / 8

Sub swheelTimer_Timer
    swheelSpeed = swheelSpeed * swheelStep
    swheelTimer.Interval = swheelSpeed
    Spinwheel.rotz = (Spinwheel.rotz + 22.5)
    if Spinwheel.Rotz >= 360 Then
        Spinwheel.Rotz = 0
    End If
    If swheelSpeed > 200 Then
        swheelTimer.Enabled = FALSE
        DOF 137, DOFOff
        swheelTimer.Interval = 1 'ready for next round
        swheelSpeed = 1 + 10 * RND(1)
        swheelStep = 1.1 + RND(1) / 8
        GiveWheelPrize
    End If
End Sub

Sub GiveWheelPrize
    Dim a
    a = INT(Spinwheel.Rotz) \ 22.5
    debug.print a
    Select Case a
        Case 0:AddScore 100000:DMD CL("100000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Case 1:AddScore 20000:DMD CL("20000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Case 2: 'play a sound ' nada, nothing, sip, 0 points :)
        Case 3:AddScore 50000:DMD CL("50000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Case 4:AwardExtraBall
        Case 5:AddScore 30000:DMD CL("30000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Case 6:AddScore 60000:DMD CL("60000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Case 7:AddScore 10000:DMD CL("10000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Case 8:AddScore 200000:DMD CL("200000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Case 9:AddScore 30000:DMD CL("30000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Case 10:AddScore 5000:DMD CL("5000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Case 11:AddScore 75000:DMD CL("75000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Case 12:AwardSpecial
        Case 13:AddScore 20000:DMD CL("20000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Case 14:AddScore 40000:DMD CL("40000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Case 15:AddScore 10000:DMD CL("10000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
    End Select
    'eject the ball from the icecream hole
    Flasher3Timer.Enabled = TRUE
    IceCreamHole.TimerEnabled = TRUE 'eject the ball/s from the hole
End Sub

'***************
' Ferrys Wheel
'***************

Dim WheelBall, WheelBallReady, ferrysHits

Sub fwheelTimer_timer
    pwheel.rotz = (pwheel.rotz - 1)

    if pwheel.Rotz = -360 Then
        pwheel.Rotz = 0
    End If

    If pwheel.rotz = -322 AND WheelBallReady = TRUE Then
        WheelTrigger.destroyball
        WheelBallReady = FALSE
        WheelBall = TRUE
        DOF 140, DOFOn
        Flasher1Timer.Enabled = 1
    End If
    If pwheel.rotz = -212 AND WheelBall = TRUE Then
        kicker2.Createball
        kicker2.kick -90, 2
        WheelBall = FALSE
        DOF 140, DOFOff
        Flasher1Timer.Enabled = 0:Flasher1.Visible = 0
        ferrysHits = ferrysHits + 1
        If ferrysHits >= 4 Then
            AwardJackpot
            LightR5.State = 1
        Else
            LightR5.State = 2
        End If
    End If
End Sub

Sub Wheeltrigger_hit
    WheelBallReady = True
End Sub

Sub Wheeltrigger_unhit
    WheelBallReady = False
End Sub

Sub EnterBigRamp_Hit
    Dim a
    a = INT(RND(1) * 4)
    Select Case a
        Case 0:PlaySound "gp_siren1"
        Case 1:PlaySound "gp_siren2"
        Case 2:PlaySound "gp_siren3"
        Case 3:PlaySound "gp_siren4"
    End Select
    AddScore 100000:DMD CL("100000"), "_", "_", eBlinkFast, eNone, eNone, 500, TRUE, ""
End Sub

'****************
' Ghost animation
'****************

Dim g1, g2, g3
Dim g1dir, g2dir, g3dir

g1dir = 1
g2dir = 2
g3dir = 1

g1 = (1451 - Ghost1.X) \ 4
g2 = (1451 - Ghost2.X) \ 4
g3 = (1451 - Ghost3.X) \ 4

Sub StartGhosts_Hit
    GhostsTimer.Enabled = TRUE
    DOF 112, DOFOn
End Sub

Sub GhostsTimer_Timer
    ghost1w(69-g1).collidable = 0
    g1 = g1 + g1dir
    If g1 > 69 Then g1 = 69:g1dir = -1
    If g1 < 0 Then g1 = 0:g1dir = 1
    ghost1w(69-g1).collidable = 1
    ghost1.X = 1175 + g1 * 4

    ghost2w(69-g2).collidable = 0
    g2 = g2 + g2dir
    If g2 > 69 Then g2 = 69:g2dir = -2
    If g2 < 0 Then g2 = 0:g2dir = 2
    ghost2w(69-g2).collidable = 1
    ghost2.X = 1175 + g2 * 4

    ghost3w(69-g3).collidable = 0
    g3 = g3 + g3dir
    If g3 > 69 Then g3 = 69:g3dir = -1
    If g3 < 0 Then g3 = 0:g3dir = 1
    ghost3w(69-g3).collidable = 1
    ghost3.X = 1175 + g3 * 4
End Sub

Sub ghost1w_Hit(idx):PlaySoundAtBall "fx_plastichit":End Sub
Sub ghost2w_Hit(idx):PlaySoundAtBall "fx_plastichit":End Sub
Sub ghost3w_Hit(idx):PlaySoundAtBall "fx_plastichit":End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/modes this trigger is a member of
' - set the "LastSwicthHit" variable in case it is needed later
' *********************************************************************

' Slingshots has been hit

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If NOT Tilted Then
        PlaySound SoundFXDOF("fx_slingshot", 120, DOFPulse, DOFContactors), 0, 1, -0.05, 0.05
        DOF 122, DOFPulse
        LeftSling4.Visible = 1
        Lemk.RotX = 26
        LStep = 0
        LeftSlingShot.TimerEnabled = TRUE
        ' add some points
        AddScore 110
        ' remember last trigger hit by the ball
        LastSwitchHit = "LeftSlingShot"
    End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = FALSE
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Not Tilted Then
        PlaySound SoundFXDOF("fx_slingshot", 121, DOFPulse, DOFContactors), 0, 1, 0.05, 0.05
        DOF 123, DOFPulse
        RightSling4.Visible = 1
        Remk.RotX = 26
        RStep = 0
        RightSlingShot.TimerEnabled = TRUE
        ' add some points
        AddScore 110
        ' remember last trigger hit by the ball
        LastSwitchHit = "RightSlingShot"
    End If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = FALSE
    End Select
    RStep = RStep + 1
End Sub

'*********************
' Inlanes - Outlanes
'*********************

Sub Trigger1_Hit()
    PlaySound "fx_sensor", 0, 1, pan(ActiveBall)
    DOF 124, DOFPulse
    If Not Tilted Then
        AddScore 500
        IncrementBonus 1
        LightSS.State = 1
        LightSpin.State = 1
    End If
    LastSwitchHit = "Trigger1"
    CheckMultiplier
End Sub

Sub Trigger2_Hit()
    PlaySound "fx_sensor", 0, 1, pan(ActiveBall)
    DOF 125, DOFPulse
    If Not Tilted Then
        If bSkillShotReady Then
            'give skillshot
            AwardSkillshot
        Else
            AddScore 100
            IncrementBonus 1
            LightM.State = 1
        End If
    End If
    LastSwitchHit = "Trigger2"
    CheckMultiplier
End Sub

Sub Trigger3_Hit()
    PlaySound "fx_sensor", 0, 1, pan(ActiveBall)
    DOF 125, DOFPulse
    If Not Tilted Then
        AddScore 100
        IncrementBonus 1
        LightU.State = 1
    End If
    LastSwitchHit = "Trigger3"
    CheckMultiplier
End Sub

Sub Trigger4_Hit()
    PlaySound "fx_sensor", 0, 1, pan(ActiveBall)
    DOF 126, DOFPulse
    If Not Tilted Then
        AddScore 100
        IncrementBonus 1
        LightR.State = 1
    End If
    LastSwitchHit = "Trigger4"
    CheckMultiplier
End Sub

Sub Trigger5_Hit()
    PlaySound "fx_sensor", 0, 1, pan(ActiveBall)
    DOF 127, DOFPulse
    If Not Tilted Then
        AddScore 500
        IncrementBonus 1
        LightF.State = 1
    End If
    LastSwitchHit = "Trigger5"
    CheckMultiplier
End Sub

Sub CheckMultiplier
    If(LightSS.State = 1)And(LightM.State = 1)And(LightU.State = 1)And(LightR.State = 1)And(LightF.State = 1)Then
        AddScore 10000
        LightSeqLanes.Play SeqRandom, 4, , 3000
        'LightSeqFlasher.Play SeqRandom, 4, , 3000
        IncrementBonusMultiplier()
        LightSS.State = 0
        LightSpin.State = 0
        LightM.State = 0
        LightU.State = 0
        LightR.State = 0
        LightF.State = 0
    End If
    DMD "_", CL(FormatScore(Bonuspoints(Currentplayer)) & " X" &BonusMultiplier(Currentplayer)), "_", eNone, eNone, eNone, 500, TRUE, ""
End Sub

'**************
' Bumper cars
'**************

Dim bumperCarHits

Sub Bumpers_Hit(idx)
    PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, pan(ActiveBall)
    DOF 114 + idx, DOFPulse
    If NOT Tilted Then
        AddScore 1000
        bumperCarHits = bumperCarHits - 1
        DMDFlush
        DMD "      " &bumperCarHits, "         HITS LEFT", "d_bumpcar", eNone, eNone, eNone, 500, TRUE, ""

        If bumperCarHits <= 0 Then
            If LightBumper1.State = 0 Then
                DOF 106, DOFPulse
                AddScore 50000
                DMDFlush
                DMD CL("50000"), CL("BUMPERS APRENTICE"), "d_bumpcar", eBlinkFast, eNone, eNone, 500, TRUE, ""
                bumperCarHits = 100
                LightR3.State = 2
                LightBumper1.State = 1
            Else
                If LightBumper2.State = 0 Then
                    DOF 106, DOFPulse
                    AddScore 50000
                    DMDFlush
                    DMD CL("50000"), CL("BUMPERS ADEPT"), "d_bumpcar", eBlinkFast, eNone, eNone, 500, TRUE, ""
                    bumperCarHits = 100
                    LightBumper2.State = 1
                Else
                    If LightBumper3.State = 0 Then
                        DOF 106, DOFPulse
                        AddScore 50000
                        DMDFlush
                        DMD CL("50000"), CL("BUMPERS EXPERT"), "d_bumpcar", eBlinkFast, eNone, eNone, 500, TRUE, ""
                        bumperCarHits = 100
                        LightBumper3.State = 1
                    Else
                        If LightBumper4.State = 0 Then
                            DOF 106, DOFPulse
                            AddScore 50000
                            DMDFlush
                            DMD CL("50000"), CL("BUMPERS MASTER"), "d_bumpcar", eBlinkFast, eNone, eNone, 500, TRUE, ""
                            bumperCarHits = 100
                            LightBumper4.State = 1
                            LightR3.State = 1
                        Else ' give jackpots until the end of the game
                            bumperCarHits = 100
                            AwardJackpot
                        End If
                    End If
                End If
            End If
        End If
    End If
End Sub

Sub ResetBumpers
    bumperCarHits = 100
    LightR3.State = 0
    LightBumper1.State = 0
    LightBumper2.State = 0
    LightBumper3.State = 0
    LightBumper4.State = 0
End Sub

'**************
' Lower Targets
'**************
' the 2 lower targets are used to change the way the ball will go when shooting at the center ramp

Sub TargetL_Hit()
    PlaySound SoundFXDOF("fx_target", 113, DOFPulse, DOFContactors), 0, 1, pan(ActiveBall)
    If Not Tilted Then
        Addscore 1000
        IncrementBonus 1
        LightL.State = 1
        RightRideWall.IsDropped = 0
        LightO.State = 0
        LeftRideWall.IsDropped = 1
    End If
End Sub

Sub TargetO_Hit()
    PlaySound SoundFXDOF("fx_target", 113, DOFPulse, DOFContactors), 0, 1, pan(ActiveBall)
    If Not Tilted Then
        Addscore 1000
        IncrementBonus 1
        LightO.State = 1
        LeftRideWall.IsDropped = 0
        LightL.State = 0
        RightRideWall.IsDropped = 1
    End If
End Sub

'***************
' IceCreamHole
'***************

Sub IceCreamHole_Hit
    PlaySound "fx_hole-enter", 0, 1, pan(ActiveBall)
    IceCreamHole.Destroyball
    BallsInHole = BallsInHole + 1
    If Not Tilted Then
        DMDFlush
        DMD CL("ICE CREAM"), "", "d_icecream", eNone, eNone, eNone, 2000, TRUE, ""
        AddScore 10000
        'check spin light
        If LightSpin.State = 1 Then
            swheelTimer.Enabled = TRUE
            DOF 137, DOFOn
        Else
            Flasher3Timer.Enabled = TRUE
            IceCreamHole.TimerEnabled = TRUE 'eject the ball/s from the hole
        End If
    End If
End Sub

Sub IceCreamHole_Timer
    If BallsInHole Then
        BallsInHole = BallsInHole - 1
        IceCreamHole.CreateBall
        IceCreamHole.Kick 190, 12
        PlaySound SoundFXDOF("fx_Popper", 103, DOFPulse, DOFContactors)
        DOF 104, DOFPulse
    End If
    If BallsInHole = 0 Then
        IceCreamHole.TimerEnabled = FALSE
        Flasher3Timer.Enabled = FALSE:Flasher3.Visible = 0
    End If
End Sub

'*******************
' Other drain holes
'*******************

Sub Drain2_Hit
    PlaySound "fx_hole-enter", 0, 1, pan(ActiveBall)
    PlaySound "ga_laugh2", 0, 1, pan(ActiveBall)
    Me.Destroyball
    BallsInHole = BallsInHole + 1
    GhostsTimer.Enabled = FALSE
    DOF 112, DOFOff
    Flasher3Timer.Enabled = TRUE
    IceCreamHole.TimerEnabled = TRUE 'eject the ball/s from the hole
End Sub

Sub Drain3_Hit
    PlaySound "fx_hole-enter", 0, 1, pan(ActiveBall)
    PlaySound "ga_azrael_laugh", 0, 1, pan(ActiveBall)
    Me.Destroyball
    BallsInHole = BallsInHole + 1
    Flasher3Timer.Enabled = TRUE
    IceCreamHole.TimerEnabled = TRUE 'eject the ball/s from the hole
End Sub

'********************
' Spinner & Freefall
'********************
' down 330
' up 550

Dim bFreeFallStarted, ffRounds, ffDir, freefallHits

Sub Spinner1_Spin
    PlaySound "fx_spinner", 0, 1, -0.01
    DOF 138, DOFPulse
    If bFreeFallStarted = FALSE Then
        If FreeFall.Z < 550 Then
            FreeFall.Z = FreeFall.Z + 2
            DOF 143, DOFPulse
        Else
            FreeFall.Z = 550
            UpdateFreefallLight
            bFreeFallStarted = TRUE
            DOF 144, DOFOn
            ffRounds = 7
            ffDir = -6 'move down fast
            Flasher2Timer.Enabled = True
            vpmTimer.AddTimer 2500, "FreeFallTimer.Enabled = TRUE '"
            PlaySound "gp_pleasesit", 0, 1, -0.05, 0.05
        End If
    End If
End Sub

Sub Spinner2_Spin
    DOF 139, DOFPulse
    PlaySound "fx_spinner", 0, 1, 0.01
    If bFreeFallStarted = FALSE Then
        If FreeFall.Z < 550 Then
            FreeFall.Z = FreeFall.Z + 2
            DOF 143, DOFPulse
        Else
            FreeFall.Z = 550
            bFreeFallStarted = TRUE
            DOF 144, DOFOn
            ffRounds = 7
            ffDir = -6 'move down fast
            Flasher2Timer.Enabled = True
            vpmTimer.AddTimer 2500, "FreeFallTimer.Enabled = TRUE '"
            PlaySound "gp_pleasesit", 0, 1, -0.05, 0.05
        End If
    End If
End Sub

Sub FreeFallTimer_Timer
    If FreeFall.Z = 550 Then
        PlaySound "gp_freefall", 0, 1, -0.05, 0.05
    End If

    FreeFall.Z = FreeFall.Z + ffDir
    If FreeFall.Z < 330 Then
        FreeFall.Z = 330
        ffRounds = ffRounds - 1
        ffDir = 3
        If ffRounds = 0 Then
            Me.Enabled = 0
            bFreeFallStarted = FALSE
            DOF 144, DOFOff
            Flasher2Timer.Enabled = FALSE
            Flasher2.Visible = FALSE
        End If
    End If

    If FreeFall.Z > 550 Then
        FreeFall.Z = 550
        ffDir = -5
    End If
End Sub

Sub UpdateFreefallLight
    freefallHits = freefallHits + 1
    If freefallHits >= 4 Then
        LightR4.State = 1
        AwardJackpot
    Else
        LightR4.State = 2
    End If
End Sub

'*******************
' Haunted House
'*******************

Sub TargetHT1_Hit
    PlaySound SoundFXDOF("fx_target", 105, DOFPulse, DOFContactors), 0, 1, pan(ActiveBall)
    If Not Tilted Then
        Addscore 1000
        LightHT1.State = 1
        CheckHaunted
    End If
End Sub

Sub TargetHT2_Hit
    PlaySound SoundFXDOF("fx_target", 105, DOFPulse, DOFContactors), 0, 1, pan(ActiveBall)
    If Not Tilted Then
        Addscore 1000
        LightHT2.State = 1
        CheckHaunted
    End If
End Sub

Sub TargetHT3_Hit
    PlaySound SoundFXDOF("fx_target", 105, DOFPulse, DOFContactors), 0, 1, pan(ActiveBall)
    If Not Tilted Then
        Addscore 1000
        LightHT3.State = 1
        CheckHaunted
    End If
End Sub

Sub TargetHT4_Hit
    PlaySound SoundFXDOF("fx_target", 105, DOFPulse, DOFContactors), 0, 1, pan(ActiveBall)
    If Not Tilted Then
        Addscore 1000
        LightHT4.State = 1
        CheckHaunted
    End If
End Sub

Sub TriggerHT5_Hit
    PlaySound "fx_sensor", 0, 1, pan(ActiveBall)
    DOF 142, DOFPulse
    If Not Tilted Then
        flashhaunted.State = 2
        Me.TimerEnabled = 1
        Addscore 1000
        If LightHT5.State = 2 Then
            LightSeqGi.Play SeqRandom, 40, , 4000
            If LightH1.State = 0 Then
                DMD CL(FormatScore("50000")), CL("HA.HOUSE APRENTICE"), "d_haunted", eNone, eBlinkFast, eNone, 1000, TRUE, ""
                AddScore 50000
                LightH1.State = 1
                LightR6.State = 2
                ResetHHLights
                DOF 106, DOFPulse
            Else
                If LightH2.State = 0 Then
                    DMD CL(FormatScore("50000")), CL("HA.HOUSE ADEPT"), "d_haunted", eNone, eBlinkFast, eNone, 1000, TRUE, ""
                    AddScore 50000
                    LightH2.State = 1
                    ResetHHLights
                    DOF 106, DOFPulse
                Else
                    If LightH3.State = 0 Then
                        DMD CL(FormatScore("50000")), CL("HA.HOUSE EXPERT"), "d_haunted", eNone, eBlinkFast, eNone, 1000, TRUE, ""
                        AddScore 50000
                        LightH3.State = 1
                        ResetHHLights
                        DOF 106, DOFPulse
                    Else
                        If LightH4.State = 0 Then
                            DMD CL(FormatScore("50000")), CL("HA.HOUSE MASTER"), "d_haunted", eNone, eBlinkFast, eNone, 1000, TRUE, ""
                            AddScore 50000
                            LightH4.State = 1
                            LightR6.State = 1
                            ResetHHLights
                            DOF 106, DOFPulse
                        Else ' give jackpot for the rest of the game
                            AwardJackpot
                            ResetHHLights
                        End If
                    End If
                End If
            End If
        End If
    End If
End Sub

Sub TriggerHT5_Timer:Me.TimerEnabled = 0:flashhaunted.State = 0:End Sub

Sub ResetHHLights
    LightHT1.State = 0
    LightHT2.State = 0
    LightHT3.State = 0
    LightHT4.State = 0
    LightHT5.State = 0
    LightHT6.State = 0
End Sub

Sub CheckHaunted
    If(LightHT1.State + LightHT2.State + LightHT3.State + LightHT4.State = 4)Then
        LightHT5.State = 2
        LightHT6.State = 2
    End If
End Sub

Sub ResetHaunted
    ResetHHLights
    LightR6.State = 0
End Sub

'**********************
' Azrael Shoot Galleri
'**********************

Dim AzraelHits

Sub aDropTargets_HIT(idx)
    PlaySoundAtBall SoundFX("fx_droptarget", DOFContactors)
    DOF 132, DOFPulse
    aDropTargets(idx).IsDropped = 1
    AzraelHits = AzraelHits - 1
    If NOT Tilted Then
        AddScore 250 * (1 + LightA1.State + LightA2.State + LightA3.State + LightA4.State)
        DMDFlush
        DMD "      " &AzraelHits, "         HITS LEFT", "d_azrael", eNone, eNone, eNone, 500, TRUE, ""
        If AzraelHits <= 0 Then
            LightSeqGi.Play SeqRandom, 40, , 4000
            If LightA1.State = 0 Then
                DOF 106, DOFPulse
                DMD CL(FormatScore("50000")), CL("APRENTICE SHOOTER"), "d_azrael", eNone, eBlinkFast, eNone, 1000, TRUE, ""
                AddScore 50000
                LightA1.State = 1
                LightR2.State = 2
                vpmTimer.AddTimer 2000, "ResetAzrael '"
            Else
                If LightA2.State = 0 Then
                    DOF 106, DOFPulse
                    DMD CL(FormatScore("50000")), CL("ADEPT SHOOTER"), "d_azrael", eNone, eBlinkFast, eNone, 1000, TRUE, ""
                    AddScore 50000
                    LightA2.State = 1
                    vpmTimer.AddTimer 2000, "ResetAzrael '"
                Else
                    If LightA3.State = 0 Then
                        DOF 106, DOFPulse
                        DMD CL(FormatScore("50000")), CL("EXPERT SHOOTER"), "d_azrael", eNone, eBlinkFast, eNone, 1000, TRUE, ""
                        AddScore 50000
                        LightA3.State = 1
                        vpmTimer.AddTimer 2000, "ResetAzrael '"
                    Else
                        If LightA4.State = 0 Then
                            DOF 106, DOFPulse
                            DMD CL(FormatScore("50000")), CL("MASTER SHOOTER"), "d_azrael", eNone, eBlinkFast, eNone, 1000, TRUE, ""
                            AddScore 50000
                            LightA4.State = 1
                            LightR2.State = 1
                            vpmTimer.AddTimer 2000, "ResetAzrael '"
                        Else ' give jackpots until the end of the game
                            AwardJackpot
                            vpmTimer.AddTimer 2000, "ResetAzrael '"
                        End If
                    End If
                End If
            End If
        End If
    End If
End Sub

Sub ResetAzrael
    Dim obj
    PlaySound SoundFXDOF("fx_resetdrop", 134, DOFPulse, DOFContactors)
    For each obj in aDroptargets
        obj.Isdropped = 0
    Next
    AzraelHits = 24
End Sub

Sub ResetAzrael_Lights
    LightA1.State = 0
    LightA2.State = 0
    LightA3.State = 0
    LightA4.State = 0
    LightR2.State = 0
End Sub

'********************************
'    Rollercoaster - Insane Ride
'********************************
' the main scoring points of the rollercoaster depends on the droptargets
' each droptarget adds 50000 points to the rollercoaster value

Dim RollerCoasterValue, InsaneHits

Sub RTarget1_Hit()
    PlaySound SoundFXDOF("fx_droptarget", 136, DOFPulse, DOFContactors) ', 0, 1, pan(ActiveBall)
    If Not Tilted Then
        AddScore 1000
        RollerCoasterValue = RollerCoasterValue + 50000
    End If
End Sub

Sub RTarget2_Hit()
    PlaySound SoundFXDOF("fx_droptarget", 136, DOFPulse, DOFContactors) ', 0, 1, pan(ActiveBall)
    Me.IsDropped = 1
    If Not Tilted Then
        AddScore 1000
        RollerCoasterValue = RollerCoasterValue + 50000
    End If
End Sub

Sub RTarget3_Hit()
    PlaySound SoundFXDOF("fx_droptarget", 136, DOFPulse, DOFContactors) ', 0, 1, pan(ActiveBall)
    If Not Tilted Then
        AddScore 1000
        RollerCoasterValue = RollerCoasterValue + 50000
    End If
End Sub

' The 2 targets also increases the score of the Rollercoaster.
' Each hit increases the value by 10000

Sub TargetC_Hit()
    PlaySound SoundFXDOF("fx_target", 135, DOFPulse, DOFContactors), 0, 1, pan(ActiveBall)
    If Not Tilted Then
        Addscore 1000
        IncrementBonus 1
        RollerCoasterValue = RollerCoasterValue + 20000
        LightC.State = 1
    End If
End Sub

Sub TargetK_Hit()
    PlaySound SoundFXDOF("fx_target", 135, DOFPulse, DOFContactors), 0, 1, pan(ActiveBall)
    If Not Tilted Then
        Addscore 1000
        IncrementBonus 1
        RollerCoasterValue = RollerCoasterValue + 20000
        LightK.State = 1
    End If
End Sub

' Rollercoaster ride

Sub RollerCoasterKicker_Hit()
    PlaySound "fx_kicker_enter"
    If Not Tilted Then
        ' make some noise, show something on the DMD, wait a second and run
        PlaySound "gp_ring1"
        LightSeqGi.Play SeqRandom, 40, , 4000
        AddScore RollerCoasterValue
        DMD CL(FormatScore(RollerCoasterValue)), CL("RIDE ROLLERCOASTER"), "", eNone, eBlinkFast, eNone, 1000, TRUE, ""
        'update the insane light ride
        InsaneHits = InsaneHits + 1
        If InsaneHits >= 5 Then
            LightR1.State = 1
            AwardJackpot
        Else
            LightR1.State = 2
        End If
    End If
    'reset targets & lights and kick the ball
    vpmTimer.AddTimer 3000, "ResetRollercoaster"
End Sub

Sub ResetRollercoaster(dummy)
    PlaySound "fx_resetdrop"
    RollerCoasterValue = 0
    LightC.State = 0
    LightK.State = 0
    RTarget1.isDropped = 0
    RTarget2.isDropped = 0
    RTarget3.isDropped = 0
    RollerCoasterKicker.kick 270, 36
End Sub

'***************************
' Gargamel & Azrael voices
'***************************

Dim ga, az
az = Array("ga_azrael1", "ga_azrael2", "ga_azrael3", "ga_azrael4")
ga = Array("ga_afraid", "ga_beingnice", "ga_borrow", "ga_bringdiamonds", "ga_carriewood", "ga_diamond1", "ga_diamonds", "ga_favor", "ga_friends", "ga_get-hurt", _
    "ga_getyours", "ga_gooddeed", "ga_goodluck", "ga_hello", "ga_helpme", "ga_hunting", "ga_iamrich", "ga_laugh", "ga_littlepreties", "ga_luckyday",             _
    "ga_morediamonds", "ga_newman", "ga_niceday", "ga_no", "ga_nofriends", "ga_payment", "ga_poor", "ga_rich", "ga_seenthelight", "ga_sorry",                    _
    "ga_stupidsmurf", "ga_sweetness", "ga_toolate", "ga_trap", "ga_tudelu", "ga_whatsthat", "ga_wonderful")

Sub Voices1_Hit
    Dim a
    a = INT(Ubound(ga) * RND(1))
    'debug.print a
    PlaySound ga(a), 0, 1, 0.01
End Sub

Sub Voices2_Hit
    Dim a
    a = INT(Ubound(az) * RND(1))
    'debug.print a
    PlaySound az(a), 0, 1, 0.01
End Sub

Sub Voices3_Hit
    Voices1_Hit
End Sub