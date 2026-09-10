'############################################################################################
'############################################################################################
'#######                                                                             ########
'#######          Fast Draw                                                          ########
'#######          (Gottlieb 1975)                                                    ########
'#######                                                                             ########
'############################################################################################
'############################################################################################
' Version 1.4 mfuegemann 2016
'
' Thanks To:
' Zany for the Flipper model and texture
' JPSalas for the EM Reel images
' GTXJoe for the Highscore saving code
'
' Version 1.1:
' - GI light modulation set to 0.99 - thanks to hauntfreaks
' - Shadow layer and environment map by hauntfreaks
' - Kicker animation added
' - Coin sound added - thanks to STAT
' - Minor bugfixes
'
' Version 1.2:
' - EM Reel reset sequence fixed, no additional reset after first points
' - Primitive Apron
' - some sounds reviewed
'
' Version 1.3:
' - adjustable sound level for ball rolling sound
' - fixed standard sounds for rubbers, posts etc.
' - 2 rubber leaf switches animated
' - new inside case texture for DT mode
'
' Version 1.4:
' - B2S HighScore display fixed
' - Lane guide elasticity adjusted
' - 5K DropTarget reset animation changed
' - DOF support added by Arngrim
' - Chimes added by Arngrim
' - Option to mute Chimes added by mfuegemann ;)
' - Option to control Chime vlume added

option Explicit

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

Const cGameName = "Fast_Draw_1975"

'---------------------------
'-----  Configuration  -----
'---------------------------
Const BallsperGame = 3         'the original table allows for 3 or 5 Balls
Const ShowHighScore = True     'enable to show HighScore tape on Apron, a new HighScore will be saved anyway
Const ShowPlayfieldReel = True 'set to True to show Playfield EM-Reel and Post-It for Player Scores (instead of a B2S backglass)
Const FreePlay = True
const RollingSoundFactor = 2.5 'set sound level factor here for Ball Rolling Sound, 1=default level
Const ChimesEnabled = False    'You have been warned!
Const ChimeVolume = 1          'to limit ear bleeding set between 0 and 1

Const ResetHighscore = False   'enable to manually reset the Highscores

Const Special1Score = 58000    'set the 3 replay scores (3rd for display rollover)
Const Special2Score = 76000
Const Special3Score = 100000

Dim GameActive, NoofPlayers, i, HighScore, Credits
Sub FastDraw_Init
    LoadEM
    LoadHighScore
    HideScoreboard
    if ResetHighScore then
        SetDefaultHSTD
    end if
    if ShowHighScore then
        PTape.image = HSArray(12)
        UpdatePostIt
    else
        PTape.image = HSArray(10)
        PComma.image = HSArray(10)
        PComma2.image = HSArray(10)
        Pscore0.image = HSArray(10)
        PScore1.image = HSArray(10)
        PScore2.image = HSArray(10)
        PScore3.image = HSArray(10)
        PScore4.image = HSArray(10)
        PScore5.image = HSArray(10)
        PScore6.image = HSArray(10)
    end if

    Randomize

    if FreePlay then
        Credits = 5
        DOF 126, DOFOn
    end If

    if B2SOn then
        Controller.B2SSetMatch MatchValue
        Controller.B2SSetScoreRolloverPlayer1 0
        Controller.B2SSetScoreRolloverPlayer2 0
        Controller.B2SSetScoreRolloverPlayer3 0
        Controller.B2SSetScoreRolloverPlayer4 0
        Controller.B2SSetScore 6, HighScore
        Controller.B2SSetTilt 0
        if Credits = 0 then
            Controller.B2SSetData 29, 10
            DOF 126, DOFOff
        else
            DOF 126, DOFOn
            Controller.B2SSetData 29, Credits
        end if
        Controller.B2SSetGameOver 1
        for i = 1 to 4
            Controller.B2SSetScorePlayer i, 0
        next
    end if
    if not FastDraw.ShowDT then
        EMReel1.visible = False
        EMReel2.visible = False
        EMReel3.visible = False
        EMReel4.visible = False
        EMReel_BiP.visible = False
        EMReel_Credits.visible = False
        'CaseWall1.isdropped = True
        'CaseWall2.isdropped = True
        'CaseWall3.isdropped = True
        Ramp1.widthbottom = 0
        Ramp1.widthtop = 0
        Ramp15.widthbottom = 0
        Ramp15.widthtop = 0
        DOF 148, DOFPulse
    end If

    if not ShowPlayfieldReel or FastDraw.ShowDT Then
        ReelWall.isdropped = True
        P_Reel0.Transz = -5
        P_Reel1.Transz = -5
        P_Reel2.Transz = -5
        P_Reel3.Transz = -5
        P_Reel4.Transz = -5
        P_Reel5.Transz = -5
        P_ActivePlayer.Transz = -10
        P_Credits.Transz = -10
        P_CreditsText.Transz = -10
        P_BallinPlay.Transz = -10
        P_BallinPlayText.Transz = -10
    end If

    BallinPlay = 0
    GameActive = False
    NoofPlayers = 0
    GameStarted = False
    TargetSound = "FD_500Target"
    DTSound = "FD_DropTarget"
    RolloverSound1 = "FD_Top_Rollover"
    RolloverSound2 = "FD_Top_Rollover2"
    OutlaneSound = "FD_Outlane"
    Soundlevel = 1
    for i = 1 to 4
        Val10(i) = 0
        Val100(i) = 0
        Val1000(i) = 0
        Val10000(i) = 0
        Val100000(i) = 0
        Oldscore(i) = 0
    Next
    EMReel1.ResetToZero
    EMReel2.ResetToZero
    EMReel3.ResetToZero
    EMReel4.ResetToZero
    EMReel_Credits.setvalue Credits

    P_Credits.image = cstr(Credits)
    BumperWall1.isdropped = True
    BumperWall2.isdropped = True
    BumperWall3.isdropped = True
    SwitchA_2.isdropped = True
    SwitchB_2.isdropped = True
    LeftDTBank_Cover.isdropped = True
    RightDTBank_Cover.isdropped = True

    Tilt = 0
    PUPInit
End Sub

Dim GameStarted, NoPointsScored
Sub StartGame
    if Credits > 0 Then
        if not GameActive then
            for i = 1 to 4
                Playerscore(i) = 0
                Oldscore(i) = 0
                Val10(i) = 0
                Val100(i) = 0
                Val1000(i) = 0
                Val10000(i) = 0
                Val100000(i) = 0
                Special1(i) = False
                Special2(i) = False
                Special3(i) = False
                If B2SOn Then
                    Controller.B2SSetScorePlayer i, 0
                end if
            Next
            EMReel1.ResetToZero
            EMReel2.ResetToZero
            EMReel3.ResetToZero
            EMReel4.ResetToZero
            if not GameStarted Then
                GameStarted = True
                Playsound "FD_GameStartwithBallrelease"
                Playsound "ChildsPlayTheme", -1, 1, 0.0, 0.0, 1, 1, 0.0
                GameStartTimer.enabled = True
                ScoreMotor
                ScoreMotorStartTimer.enabled = True
            end If
            if NoofPlayers < 4 Then
                Credits = Credits - 1
                If Credits < 1 Then
                    DOF 126, DOFOff
                End If
                if FreePlay and(Credits = 0)then
                    Credits = 5
                    DOF 126, DOFOn
                end If
                if NoofPlayers > 0 Then
                    Playsound "AddPlayer"
                end If
                NoofPlayers = NoofPlayers + 1
                UpdateScoreboard
                EMReel_Credits.setvalue Credits
            end If

            If B2SOn Then
                Controller.B2SSetTilt 0
                Controller.B2SSetGameOver 0
                Controller.B2SSetMatch 0
                Controller.B2SSetBallInPlay BallInPlay
                if Credits = 0 then
                    Controller.B2SSetData 29, 10
                else
                    Controller.B2SSetData 29, Credits
                end if
                Controller.B2SSetPlayerUp 1
                Controller.B2SSetBallInPlay BallInPlay
                Controller.B2SSetCanPlay NoofPlayers
                Controller.B2SSetScore 6, HSAHighScore
                PupEvent 160
                DOF 160, DOFPulse
            End if
            EMReel_BiP.setvalue BallinPlay
        end If
    end If
    P_Credits.image = cstr(Credits)
pDMDStartGame
End Sub

Sub GameStartTimer_Timer
    GameStartTimer.enabled = False
    ScoreMotorStartTimer.enabled = False
    ActivePlayer = 0
    BallinPlay = 1
    NextBall
    PupEvent 159
    DOF 159, DOFPulse
End Sub

Sub ScoreMotorStartTimer_Timer
    ScoreMotor
End Sub

Sub NextBall
    if(ActivePlayer = NoofPlayers)and(BallinPlay = BallsperGame)Then
        EndGame
    Else
        UpdateScoreboard
        if ActivePlayer < NoofPlayers Then
            ActivePlayer = ActivePlayer + 1
        Else
            if ActivePlayer = NoofPlayers Then
                BallinPlay = BallinPlay + 1
                ActivePlayer = 1
            end If
        end If
        P_ActivePlayer.image = "Player" & CStr(ActivePlayer)
        P_BallinPlay.image = CStr(BallinPlay)
        EMReel_BiP.setvalue BallinPlay

        SetScoreReel

        If B2SOn Then
            Controller.B2SSetBallInPlay BallInPlay
            Controller.B2SSetPlayerUp ActivePlayer
            Controller.B2SSetBallInPlay BallInPlay
            Controller.B2SSetTilt 0
        End if

        ResetPlayfield
        if BallinPlay = BallsperGame Then
            DB = True
            ExtraBonus.state = Lightstateon
        end If
        NoPointsScored = True
        BallRelease.CreateBall
        BallRelease.Kick 90, 7
        PupEvent 122
        DOF 122, DOFPulse
        BallsOnPlayfield = 1
    End If
End Sub

Dim MatchValue
Sub EndGame
    UpdateScoreboard
    for i = 1 to NoofPlayers
        CheckNewHighScorePostIt(Playerscore(i))
    next
    GameActive = False
    GameStarted = False
    MatchValue = Int(Rnd * 10) * 10
    If B2SOn Then
        If MatchValue = 0 Then
            Controller.B2SSetMatch 100
        Else
            Controller.B2SSetMatch MatchValue
        End If
    End if
    for i = 1 to NoofPlayers
        if MatchValue = cint(right(Playerscore(i), 2))then
            AddSpecial
        end if
    next
    BallinPlay = 0
    NoofPlayers = 0
    if B2Son Then
        Controller.B2SSetGameOver 1
        Controller.B2SSetBallInPlay BallInPlay
    end If
    Stopsound "ChildsPlayTheme"
    Playsound "GameEnd"
    PupEvent 150
    DOF 150, DOFPulse
End Sub

Sub Drain_Hit()
    PlaySound "drain3", 0, 0.2, 0, 0.25
    PupEvent 121
    DOF 121, DOFPulse
    PupEvent 149
    DOF 149, DOFPulse
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1
    ' cancel any multiball if on last ball (ie. lost all other balls)
    If(BallsOnPlayfield = 1)Then
        bMultiBallMode = False
    End If
    If(BallsOnPlayfield = 0)Then 'this is the last ball
        if NoPointsScored and(Tilt = 0)Then
            Playsound "FD_DrainwithBallrelease"
            ReleaseSameBallTimer.enabled = True
        Else
            AwardBonus
        End If
    End If
End Sub

Sub ReleaseSameBallTimer_Timer
    ReleaseSameBallTimer.enabled = False
    BallRelease.CreateBall
    BallRelease.Kick 90, 7
    PupEvent 122
    DOF 122, DOFPulse
    BallsOnPlayfield = 1
End Sub

Sub MainTimer_Timer
    Target1_Cover.isdropped = Target1.isdropped
    Target6_Cover.isdropped = Target6.isdropped
    if not GameStarted Then
        LeftFlipper.RotateToStart
        LeftFlipper001.RotateToStart
        RightFlipper.RotateToStart
        RightFlipper001.RotateToStart
    end If
    if FastDraw.showdt Then
        P1Light.State = (Activeplayer = 1)
        P2Light.State = (Activeplayer = 2)
        P3Light.State = (Activeplayer = 3)
        P4Light.State = (Activeplayer = 4)
    end If
End Sub

Sub GameOverTimer_Timer
    if GameStarted Then
        P_GameOver.visible = False
    Else
        if FastDraw.showdt Then
            P_GameOver.visible = not P_GameOver.visible
        End If
    End If
End Sub

Sub TiltTimer_Timer
    if Tilt = 0 Then
        P_Tilt.visible = False
    Else
        if FastDraw.showdt Then
            P_Tilt.visible = not P_Tilt.visible
        End If
    End If
End Sub

' DT Score Reels
Dim Val10(4), Val100(4), Val1000(4), Val10000(4), Val100000(4), Score10, Score100, Score1000, Score10000, Score100000, TempScore, Oldscore(5)
Sub UpdateScoreReel_Timer
    TempScore = Playerscore(ActivePLayer)
    Score10 = 0
    Score100 = 0
    Score1000 = 0
    Score10000 = 0
    Score100000 = 0
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        Score10 = cstr(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        Score100 = cstr(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        Score1000 = cstr(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        Score10000 = cstr(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        Score100000 = cstr(right(Tempscore, 1))
    end If

    Val10(ActivePLayer) = ReelValue(Val10(ActivePLayer), Score10, 1)
    Val100(ActivePLayer) = ReelValue(Val100(ActivePLayer), Score100, 2)
    Val1000(ActivePLayer) = ReelValue(Val1000(ActivePLayer), Score1000, 3)
    Val10000(ActivePLayer) = ReelValue(Val10000(ActivePLayer), Score10000, 0)
    Val100000(ActivePLayer) = ReelValue(Val100000(ActivePLayer), Score100000, 0)
    Tempscore = Val10(ActivePLayer) * 10 + Val100(ActivePLayer) * 100 + Val1000(ActivePLayer) * 1000 + Val10000(ActivePLayer) * 10000 + Val100000(ActivePLayer) * 100000
    if Oldscore(ActivePLayer) <> TempScore Then
        Oldscore(ActivePLayer) = TempScore
        select Case ActivePlayer
            case 1:EMReel1.setvalue TempScore
            case 2:EMReel2.setvalue TempScore
            case 3:EMReel3.setvalue TempScore
            case 4:EMReel4.setvalue TempScore
        End Select
        If B2SOn Then
            Controller.B2SSetScorePlayer ActivePlayer, TempScore
        End If
        P_Reel1.image = cstr(Val10(ActivePLayer))
        P_Reel2.image = cstr(Val100(ActivePLayer))
        P_Reel3.image = cstr(Val1000(ActivePLayer))
        P_Reel4.image = cstr(Val10000(ActivePLayer))
        P_Reel5.image = cstr(Val100000(ActivePLayer))
    Else
        UpdateScoreReel.enabled = False
    end If
End Sub

Function ReelValue(ValPar, ScorPar, ChimePar)
    ReelValue = cint(ValPar)
    if ReelValue <> cint(ScorPar)Then
        if ChimesEnabled Then
            If ChimePar = 1 Then
                PlaySound SoundFXDOF("10a", 129, DOFPulse, DOFChimes), 0, ChimeVolume
            end If
            If ChimePar = 2 Then
                PlaySound SoundFXDOF("100a", 130, DOFPulse, DOFChimes), 0, ChimeVolume
            end If
            If ChimePar = 3 Then
                PlaySound SoundFXDOF("1000a", 131, DOFPulse, DOFChimes), 0, ChimeVolume
            End If
        end If
        ReelValue = ReelValue + 1
        if ReelValue > 9 Then
            ReelValue = 0
        end If
    end If
End Function

Sub SetScoreReel
    TempScore = Playerscore(ActivePLayer)
    Score10 = 0
    Score100 = 0
    Score1000 = 0
    Score10000 = 0
    Score100000 = 0
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        Score10 = cstr(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        Score100 = cstr(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        Score1000 = cstr(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        Score10000 = cstr(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        Score100000 = cstr(right(Tempscore, 1))
    end If
    P_Reel1.image = cstr(Score10)
    P_Reel2.image = cstr(Score100)
    P_Reel3.image = cstr(Score1000)
    P_Reel4.image = cstr(Score10000)
    P_Reel5.image = cstr(Score100000)
End Sub

' FS Post-It Playerscores
Sub UpdateScoreboard
    if not ShowPlayfieldReel or(NoofPlayers < 2)or FastDraw.ShowDT Then
        HideScoreBoard
    Else
        P_SB_Postit.image = "Postit"
        Select case NoofPlayers
            Case 2:
                P_SB_Postit.size_y = 50
                P_SB_Postit.Transy = -30
                PScore1_P.image = "Player1"
                PScore2_P.image = "Player2"
                SetScoreBoard(1)
                SetScoreBoard(2)
            Case 3:
                P_SB_Postit.size_y = 75
                P_SB_Postit.Transy = -15
                PScore1_P.image = "Player1"
                PScore2_P.image = "Player2"
                PScore3_P.image = "Player3"
                SetScoreBoard(1)
                SetScoreBoard(2)
                SetScoreBoard(3)
            Case 4:
                P_SB_Postit.size_y = 100
                P_SB_Postit.Transy = 0
                PScore1_P.image = "Player1"
                PScore2_P.image = "Player2"
                PScore3_P.image = "Player3"
                PScore4_P.image = "Player4"
                SetScoreBoard(1)
                SetScoreBoard(2)
                SetScoreBoard(3)
                SetScoreBoard(4)
        end Select
    end If
End Sub

Sub HideScoreboard
    for each obj in C_ScoreBoard
        obj.image = HSArray(10)
    Next
End Sub

Dim SBScore100k, SBScore10k, SBScoreK, SBScore100, SBScore10, SBScore1, SBTempScore, obj

Sub SetScoreBoard(PlayerPar)
    SBTempScore = PlayerScore(PlayerPar)
    SBScore1 = 0
    SBScore10 = 0
    SBScore100 = 0
    SBScoreK = 0
    SBScore10k = 0
    SBScore100k = 0
    if len(SBTempScore) > 0 Then
        SBScore1 = cint(right(SBTempscore, 1))
    end If
    if len(SBTempScore) > 1 Then
        SBTempScore = Left(SBTempScore, len(SBTempScore)-1)
        SBScore10 = cint(right(SBTempscore, 1))
    end If
    if len(SBTempScore) > 1 Then
        SBTempScore = Left(SBTempScore, len(SBTempScore)-1)
        SBScore100 = cint(right(SBTempscore, 1))
    end If
    if len(SBTempScore) > 1 Then
        SBTempScore = Left(SBTempScore, len(SBTempScore)-1)
        SBScoreK = cint(right(SBTempscore, 1))
    end If
    if len(SBTempScore) > 1 Then
        SBTempScore = Left(SBTempScore, len(SBTempScore)-1)
        SBScore10k = cint(right(SBTempscore, 1))
    end If
    if len(SBTempScore) > 1 Then
        SBTempScore = Left(SBTempScore, len(SBTempScore)-1)
        SBScore100k = cint(right(SBTempscore, 1))
    end If
    Select case PlayerPar
        Case 1:
            Pscore6_1.image = HSArray(SBScore100K):If PlayerScore(PlayerPar) < 100000 Then Pscore6_1.image = HSArray(10)
            Pscore5_1.image = HSArray(SBScore10K):If PlayerScore(PlayerPar) < 10000 Then Pscore5_1.image = HSArray(10)
            Pscore4_1.image = HSArray(SBScoreK):If PlayerScore(PlayerPar) < 1000 Then Pscore4_1.image = HSArray(10)
            Pscore3_1.image = HSArray(SBScore100):If PlayerScore(PlayerPar) < 100 Then Pscore3_1.image = HSArray(10)
            Pscore2_1.image = HSArray(SBScore10):If PlayerScore(PlayerPar) < 10 Then Pscore2_1.image = HSArray(10)
            Pscore1_1.image = HSArray(SBScore1):If PlayerScore(PlayerPar) < 1 Then Pscore1_1.image = HSArray(10)
            if PlayerScore(PlayerPar) < 1000 then
                PscoreComma_1.image = HSArray(10)
            else
                PscoreComma_1.image = HSArray(11)
            end if
        Case 2:
            Pscore6_2.image = HSArray(SBScore100K):If PlayerScore(PlayerPar) < 100000 Then Pscore6_2.image = HSArray(10)
            Pscore5_2.image = HSArray(SBScore10K):If PlayerScore(PlayerPar) < 10000 Then Pscore5_2.image = HSArray(10)
            Pscore4_2.image = HSArray(SBScoreK):If PlayerScore(PlayerPar) < 1000 Then PScore4_2.image = HSArray(10)
            Pscore3_2.image = HSArray(SBScore100):If PlayerScore(PlayerPar) < 100 Then Pscore3_2.image = HSArray(10)
            Pscore2_2.image = HSArray(SBScore10):If PlayerScore(PlayerPar) < 10 Then Pscore2_2.image = HSArray(10)
            Pscore1_2.image = HSArray(SBScore1):If PlayerScore(PlayerPar) < 1 Then Pscore1_2.image = HSArray(10)
            if PlayerScore(PlayerPar) < 1000 then
                PscoreComma_2.image = HSArray(10)
            else
                PscoreComma_2.image = HSArray(11)
            end if
        Case 3:
            Pscore6_3.image = HSArray(SBScore100K):If PlayerScore(PlayerPar) < 100000 Then Pscore6_3.image = HSArray(10)
            Pscore5_3.image = HSArray(SBScore10K):If PlayerScore(PlayerPar) < 10000 Then Pscore5_3.image = HSArray(10)
            Pscore4_3.image = HSArray(SBScoreK):If PlayerScore(PlayerPar) < 1000 Then Pscore4_3.image = HSArray(10)
            Pscore3_3.image = HSArray(SBScore100):If PlayerScore(PlayerPar) < 100 Then Pscore3_3.image = HSArray(10)
            Pscore2_3.image = HSArray(SBScore10):If PlayerScore(PlayerPar) < 10 Then Pscore2_3.image = HSArray(10)
            Pscore1_3.image = HSArray(SBScore1):If PlayerScore(PlayerPar) < 1 Then Pscore1_3.image = HSArray(10)
            if PlayerScore(PlayerPar) < 1000 then
                PscoreComma_3.image = HSArray(10)
            else
                PscoreComma_3.image = HSArray(11)
            end if
        Case 4:
            Pscore6_4.image = HSArray(SBScore100K):If PlayerScore(PlayerPar) < 100000 Then Pscore6_4.image = HSArray(10)
            Pscore5_4.image = HSArray(SBScore10K):If PlayerScore(PlayerPar) < 10000 Then Pscore5_4.image = HSArray(10)
            Pscore4_4.image = HSArray(SBScoreK):If PlayerScore(PlayerPar) < 1000 Then Pscore4_4.image = HSArray(10)
            Pscore3_4.image = HSArray(SBScore100):If PlayerScore(PlayerPar) < 100 Then Pscore3_4.image = HSArray(10)
            Pscore2_4.image = HSArray(SBScore10):If PlayerScore(PlayerPar) < 10 Then Pscore2_4.image = HSArray(10)
            Pscore1_4.image = HSArray(SBScore1):If PlayerScore(PlayerPar) < 1 Then Pscore1_4.image = HSArray(10)
            if PlayerScore(PlayerPar) < 1000 then
                PscoreComma_4.image = HSArray(10)
            else
                PscoreComma_4.image = HSArray(11)
            end if
    End Select
End Sub

' Keyboard Input

Sub FastDraw_KeyDown(ByVal keycode)
    If keycode = PlungerKey Then
        Plunger.PullBack
        PlaySound "plungerpull", 0, 1, 0.25, 0.25
    End If

    if GameStarted and(Tilt = 0)Then
        If keycode = LeftFlipperKey Then
            LeftFlipper.RotateToEnd
            LeftFlipper001.RotateToEnd
            PlaySound SoundFXDOF("flipperup_akiles", 101, DOFOn, DOFContactors), 0, 0.1, -0.05, 0.25
        End If

        If keycode = RightFlipperKey Then
            RightFlipper.RotateToEnd
            RightFlipper001.RotateToEnd
            PlaySound SoundFXDOF("flipperup_akiles", 102, DOFOn, DOFContactors), 0, 0.1, 0.05, 0.25
        End If
    end If

    If keycode = LeftTiltKey Then
        Nudge 90, 2
        checkNudge
    End If

    If keycode = RightTiltKey Then
        Nudge 270, 2
        checkNudge
    End If

    If keycode = CenterTiltKey Then
        Nudge 0, 2
        checkNudge
    End If

    if keycode = StartGameKey Then
        StartGame
    end If

    if keycode = AddCreditKey Then
        Playsound "Coin3"
        Credits = Credits + 1
        DOF 126, DOFOn
        if Credits > 9 then
            Credits = 9
        end If
        if B2Son Then
            Controller.B2SSetData 29, Credits
        end If
        P_Credits.image = cstr(Credits)
        EMReel_Credits.setvalue Credits
    end If

    if keycode = AddCreditKey2 Then
        Playsound "Coin3"
        Credits = Credits + 2
        DOF 126, DOFOn
        if Credits > 9 then
            Credits = 9
        end If
        if B2Son Then
            Controller.B2SSetData 29, Credits
        End If
        P_Credits.image = cstr(Credits)
        EMReel_Credits.setvalue Credits
    end If
End Sub

Sub FastDraw_KeyUp(ByVal keycode)
    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySound "plunger", 0, 1, 0.25, 0.25
        PupEvent 151
        DOF 151, DOFPulse
    End If

    If keycode = LeftFlipperKey Then
        LeftFlipper.RotateToStart
        LeftFlipper001.RotateToStart
        if GameStarted and(Tilt = 0)Then
            PlaySound SoundFXDOF("flipperdown_akiles", 101, DOFOff, DOFContactors), 0, 0.1, -0.05, 0.25
        end If
    End If

    If keycode = RightFlipperKey Then
        RightFlipper.RotateToStart
        RightFlipper001.RotateToStart
        if GameStarted and(Tilt = 0)Then
            PlaySound SoundFXDOF("flipperdown_akiles", 102, DOFOff, DOFContactors), 0, 0.1, 0.05, 0.25
        end If
    End If
End Sub

Sub AddSpecial
    playsound SoundFXDOF("knocker", 125, DOFPulse, DOFKnocker)
    Credits = Credits + 1
    DOF 126, DOFOn
    if Credits > 9 then
        Credits = 9
    end If
    if B2Son Then
        Controller.B2SSetData 29, Credits
    end If
    EMReel_Credits.setvalue Credits
End Sub

'--- Tilt recognition ---
Dim Tilt
Sub CheckNudge
    if GameActive then
        if NudgeTimer1.enabled then
            if NudgeTimer2.enabled then
                NudgeTimer1.enabled = False
                NudgeTimer2.enabled = False
                if Tilt = 0 then
                    GameTilted
                end if
            else
                NudgeTimer2.enabled = True
            end if
        else
            NudgeTimer1.enabled = True
        end if
    end if
End Sub

Sub NudgeTimer1_Timer
    NudgeTimer1.enabled = False
End Sub

Sub NudgeTimer2_Timer
    NudgeTimer2.enabled = False
End Sub

Sub GameTilted
    AdvanceBonusTimer.enabled = False
    Tilt = 1
    if B2SOn then
        Controller.B2SSetTilt 1
    end If

    Left500a.state = Lightstateoff
    Left500b.state = Lightstateoff
    Right500a.state = Lightstateoff
    Right500b.state = Lightstateoff
    LeftDTWL.state = Lightstateoff
    RightDTWL.state = Lightstateoff
    b1l.state = Lightstateoff
    b2l.state = Lightstateoff
    b3l.state = Lightstateoff
    TargetSound = "target"
    DTSound = "TargetDrop1"
    RolloverSound1 = "Soloff"
    RolloverSound2 = "Soloff"
    OutlaneSound = "Soloff"
    Soundlevel = 0.3

    BumperWall1.isdropped = False
    BumperWall2.isdropped = False
    BumperWall3.isdropped = False
End Sub

'#############################################################
'#####  Fast Draw Scoring                                #####
'#############################################################
Dim BallinPlay

Dim HoleValue
Sub LKicker_Hit
    PlaySound "kicker_enter_center", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    LKicker.timerenabled = False
    Select Case HoleValue
        Case 0:Playsound "FD_Hole_0", 0, 1, -0.07, 0.05
        Case 1:Playsound "FD_Hole_1", 0, 1, -0.07, 0.05
        Case 2:Playsound "FD_Hole_2", 0, 1, -0.07, 0.05
        Case 3:Playsound "FD_Hole_3", 0, 1, -0.07, 0.05
    end Select
    LKicker.timerenabled = True
    AddLeftHolePointsTimer.enabled = True
End Sub

Sub AddLeftHolePointsTimer_Timer
    AddLeftHolePointsTimer.enabled = False
    AddPoints(1000)
    HoleValue = 0
    if LightA Then AddPoints(1000):HoleValue = HoleValue + 1:End If
    if LightB Then AddPoints(1000):HoleValue = HoleValue + 1:End If
    if LightC Then AddPoints(1000):HoleValue = HoleValue + 1:End If
    ScoreMotor
    if LeftSpecial Then
        LeftSpecial = False
        LeftHoleSpecial.state = Lightstateoff
        AddSpecial
    end If
End Sub

Sub LKicker_Timer
    LKicker.timerenabled = False
    LKicker.kick 43, 23, 0.85
    P_RightKicker.rotx = -85
    P_LeftKicker.rotx = -85
    Kickertimer.enabled = False
    Kickertimer.enabled = True
    PupEvent 117
    DOF 117, DOFPulse
End Sub

Sub RKicker_Hit
    'playsound "kicker_enter_center",0,0.1,0.12,0.05
    PlaySound "kicker_enter_center", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    RKicker.timerenabled = False
    Select Case HoleValue
        Case 0:Playsound "FD_Hole_0", 0, 1, 0.07, 0.05
        Case 1:Playsound "FD_Hole_1", 0, 1, 0.07, 0.05
        Case 2:Playsound "FD_Hole_2", 0, 1, 0.07, 0.05
        Case 3:Playsound "FD_Hole_3", 0, 1, 0.07, 0.05
    end Select
    RKicker.timerenabled = True
    AddRightHolePointsTimer.enabled = True
End Sub

Sub AddRightHolePointsTimer_Timer
    AddRightHolePointsTimer.enabled = False
    AddPoints(1000)
    HoleValue = 0
    if LightA Then AddPoints(1000):HoleValue = HoleValue + 1:End If
    if LightB Then AddPoints(1000):HoleValue = HoleValue + 1:End If
    if LightC Then AddPoints(1000):HoleValue = HoleValue + 1:End If
    ScoreMotor
    if RightSpecial Then
        RightSpecial = False
        RightHoleSpecial.state = Lightstateoff
        AddSpecial
    end If
End Sub

Sub RKicker_Timer
    RKicker.timerenabled = False
    RKicker.kick -43, 23, 0.85
    P_RightKicker.rotx = -85
    P_LeftKicker.rotx = -85
    Kickertimer.enabled = False
    Kickertimer.enabled = True
    PupEvent 118
    DOF 118, DOFPulse
End Sub

Sub KickerTimer_Timer
    P_RightKicker.rotx = P_RightKicker.rotx + 5
    P_LeftKicker.rotx = P_LeftKicker.rotx + 5

    if P_RightKicker.rotx >= -10 then
        P_RightKicker.rotx = -10
        P_LeftKicker.rotx = -10
        Kickertimer.enabled = False
    end if
End Sub

Sub Bumper1_Hit
    if LightA Then
        Addpoints(100)
    Else
        Addpoints(10)
    end If
    PlaySound SoundFXDOF("FD_Bumper1", 103, DOFPulse, DOFContactors), 0, 1, -0.05, 0.05
    PupEvent 152
    DOF 152, DOFPulse
End Sub

Sub Bumper2_Hit
    if LightC Then
        Addpoints(100)
    Else
        Addpoints(10)
    end If
    PlaySound SoundFXDOF("FD_Bumper2", 105, DOFPulse, DOFContactors), 0, 1, 0.05, 0.05
    PupEvent 153
    DOF 153, DOFPulse
End Sub

Dim HeadPos

Sub Bumper001_Hit 'chucky bumper
    Addpoints(5000)
    PlaySound SoundFXDOF("FD_Bumper2", 105, DOFPulse, DOFContactors), 0, 1, 0.05, 0.05
    PupEvent 153
    DOF 153, DOFPulse
    HeadPos = 6
    Bumper001.TimerEnabled = True
End Sub

Sub Bumper001_timer
    chuckyhead1.TransZ = HeadPos
    chuckyhead2.TransZ = HeadPos
    If HeadPos =0  Then Bumper001.TimerEnabled = False: Exit Sub
    If HeadPos < 0 Then
        HeadPos = ABS(HeadPos) - 0.1
    Else
	   HeadPos = -HeadPos + 0.1
    End If
End Sub

Sub Bumper3_Hit
    if LightB Then
        Addpoints(1000)
    Else
        Addpoints(10)
    end If
    PlaySound SoundFXDOF("FD_Bumper3", 104, DOFPulse, DOFContactors), 0, 1, 0, 0.05
    PupEvent 154
    DOF 154, DOFPulse
    if LeftSpecial Then
        RightSpecial = True
        RightHoleSpecial.state = Lightstateon
        LeftSpecial = False
        LeftHoleSpecial.state = Lightstateoff
    Else
        if RightSpecial Then
            RightSpecial = False
            RightHoleSpecial.state = Lightstateoff
            LeftSpecial = True
            LeftHoleSpecial.state = Lightstateon
        end If
    end If
End Sub

Sub Trigger_A1_Hit:PlaySound RolloverSound1, 0, Soundlevel, -0.05, 0.05:Addpoints(50):Light_A:ScoreMotor:PupEvent 108: DOF 108, DOFPulse:End Sub
Sub Trigger_A2_Hit:Playsound RolloverSound2, 0, Soundlevel, -0.09, 0.05:Addpoints(50):Light_A:ScoreMotor:PupEvent 112:DOF 112, DOFPulse:End Sub
Sub Trigger_B1_Hit:Playsound RolloverSound1, 0, Soundlevel, 0, 0.05:Addpoints(50):Light_B:ScoreMotor:PupEvent 109:DOF 109, DOFPulse:End Sub
Sub Trigger_B2_Hit:P_LeftInlane.TransZ = -25:Playsound RolloverSound2, 0, Soundlevel, -0.08, 0.05:Addpoints(50):Light_B:ScoreMotor:PupEvent 113:DOF 113, DOFOn:DOF 144, DOFOn:End Sub
Sub Trigger_B2_Unhit:Trigger_B2.timerenabled = True:PupEvent 113:DOF 113, DOFOff:End Sub
Sub Trigger_B2_Timer:Trigger_B2.timerenabled = False:P_LeftInlane.TransZ = 0:End Sub
Sub Trigger_B3_Hit:P_RightInlane.TransZ = -25:Playsound RolloverSound2, 0, Soundlevel, 0.08, 0.05:Addpoints(50):Light_B:ScoreMotor:PupEvent 115:DOF 115, DOFOn:DOF 144, DOFOn:End Sub
Sub Trigger_B3_Unhit:Trigger_B3.timerenabled = True:PupEvent 115:DOF 115, DOFOff:End Sub
Sub Trigger_B3_Timer:Trigger_B3.timerenabled = False:P_RightInlane.TransZ = 0:End Sub

Sub Trigger_C1_Hit:Playsound RolloverSound1, 0, Soundlevel, 0.05, 0.05:Addpoints(50):Light_C:ScoreMotor:PupEvent 110:DOF 110, DOFPulse:End Sub
Sub Trigger_C2_Hit:Playsound RolloverSound2, 0, Soundlevel, 0.09, 0.05:Addpoints(50):Light_C:ScoreMotor:PupEvent 114:DOF 114, DOFPulse:End Sub

Sub Trigger_LeftOutlane_Hit:P_LeftOutlane.TransZ = -25:PlaySound OutlaneSound, 0, Soundlevel, -0.1, 0.05:AddBonus:Addpoints(500):ScoreMotor:PupEvent 111:DOF 111, DOFOn:PupEvent 147:DOF 147, DOFOn:End Sub
Sub Trigger_LeftOutlane_Unhit:Trigger_LeftOutlane.timerenabled = True:PupEvent 111:DOF 111, DOFOff:End Sub
Sub Trigger_LeftOutlane_Timer:Trigger_LeftOutlane.timerenabled = False:P_LeftOutlane.TransZ = 0:End Sub
Sub Trigger_RightOutlane_Hit:P_RightOutlane.TransZ = -25:PlaySound OutlaneSound, 0, Soundlevel, 0.1, 0.05:AddBonus:Addpoints(500):ScoreMotor:PupEvent 116:DOF 116, DOFOn:PupEvent 146:DOF 146, DOFOn:End Sub
Sub Trigger_RightOutlane_Unhit:Trigger_RightOutlane.timerenabled = True:PupEvent 116:DOF 116, DOFOff:End Sub
Sub Trigger_RightOutlane_Timer:Trigger_RightOutlane.timerenabled = False:P_RightOutlane.TransZ = 0:End Sub

Sub Target11_Hit:PlaySound SoundFXDOF(TargetSound, 106, DOFPulse, DOFContactors), 0, Soundlevel, -0.09, 0.05:AddBonus:Addpoints(500):ScoreMotor:End Sub
Sub Target12_Hit:PlaySound SoundFXDOF(TargetSound, 106, DOFPulse, DOFContactors), 0, Soundlevel, -0.09, 0.05:AddBonus:Addpoints(500):ScoreMotor:End Sub
Sub Target13_Hit:PlaySound SoundFXDOF(TargetSound, 107, DOFPulse, DOFContactors), 0, Soundlevel, 0.09, 0.05:AddBonus:Addpoints(500):ScoreMotor:End Sub
Sub Target14_Hit:PlaySound SoundFXDOF(TargetSound, 107, DOFPulse, DOFContactors), 0, Soundlevel, 0.09, 0.05:AddBonus:Addpoints(500):ScoreMotor:End Sub

'Drop Targets
Sub Target1_Hit:playsound SoundFXDOF(DTSound, 119, DOFPulse, DOFContactors), 0, Soundlevel, -0.08, 0.05:AddBonus:Addpoints(500):ScoreMotor:End Sub
Sub Target2_Hit:playsound SoundFXDOF(DTSound, 119, DOFPulse, DOFContactors), 0, Soundlevel, -0.08, 0.05:AddBonus:Addpoints(500):ScoreMotor:End Sub
Sub Target3_Hit
    playsound SoundFXDOF(DTSound, 119, DOFPulse, DOFContactors), 0, Soundlevel, -0.08, 0.05
    AddBonus
    if DTSpecial Then
        Addpoints(5000)
    Else
        Addpoints(500)
    end If
    ScoreMotor
End Sub
Sub Target4_Hit:playsound SoundFXDOF(DTSound, 119, DOFPulse, DOFContactors), 0, Soundlevel, -0.08, 0.05:AddBonus:Addpoints(500):ScoreMotor:PupEvent 140:DOF 140, DOFPulse:End Sub
Sub Target5_Hit:playsound SoundFXDOF(DTSound, 119, DOFPulse, DOFContactors), 0, Soundlevel, -0.08, 0.05:AddBonus:Addpoints(500):ScoreMotor:PupEvent 139:DOF 139, DOFPulse:End Sub
Sub Target6_Hit:playsound SoundFXDOF(DTSound, 120, DOFPulse, DOFContactors), 0, Soundlevel, 0.08, 0.05:AddBonus:Addpoints(500):ScoreMotor:PupEvent 138:DOF 138, DOFPulse:End Sub
Sub Target7_Hit:playsound SoundFXDOF(DTSound, 120, DOFPulse, DOFContactors), 0, Soundlevel, 0.08, 0.05:AddBonus:Addpoints(500):ScoreMotor:PupEvent 137:DOF 137, DOFPulse:End Sub
Sub Target8_Hit
    playsound SoundFXDOF(DTSound, 120, DOFPulse, DOFContactors), 0, Soundlevel, 0.08, 0.05
    AddBonus
    if DTSpecial Then
        Addpoints(5000)
    Else
        Addpoints(500)
    end If
    ScoreMotor
End Sub
Sub Target9_Hit:playsound SoundFXDOF(DTSound, 120, DOFPulse, DOFContactors), 0, Soundlevel, 0.08, 0.05:AddBonus:Addpoints(500):ScoreMotor:End Sub
Sub Target10_Hit:playsound SoundFXDOF(DTSound, 120, DOFPulse, DOFContactors), 0, Soundlevel, 0.08, 0.05:AddBonus:Addpoints(500):ScoreMotor:End Sub

Dim LightA, LightB, LightC
Sub Light_A
    if not LightA and(Tilt = 0)Then
        LightA = True
        TopA.state = Lightstateoff
        CenterA.state = Lightstateon
        LowerA.state = Lightstateoff
        b1l.state = Lightstateon
        if LightA and LightB and LightC Then
            ExtraBonus.state = Lightstateon
            DB = True
            if BallinPlay = BallsperGame Then
                TB = True
            end If
        end If
    end If
End Sub

Sub Light_B
    if not LightB and(Tilt = 0)Then
        LightB = True
        TopB.state = Lightstateoff
        CenterB.state = Lightstateon
        LowerB1.state = Lightstateoff
        LowerB2.state = Lightstateoff
        b3l.state = Lightstateblinking
        if LightA and LightB and LightC Then
            ExtraBonus.state = Lightstateon
            DB = True
            if BallinPlay = BallsperGame Then
                TB = True
            end If
        end If
    end If
End Sub

Sub Light_C
    if not LightC and(Tilt = 0)Then
        LightC = True
        TopC.state = Lightstateoff
        CenterC.state = Lightstateon
        LowerC.state = Lightstateoff
        b2l.state = Lightstateon
        if LightA and LightB and LightC Then
            ExtraBonus.state = Lightstateon
            DB = True
            if BallinPlay = BallsperGame Then
                TB = True
            end If
        end If
    end If
End Sub

Sub CheckDTStateTimer_Timer
    if Tilt = 0 Then
        if LightA and LightB And LightC And not SpecialActivated Then
            if Target1.isdropped And Target2.isdropped And Target3.isdropped And Target4.isdropped And Target5.isdropped Then
                SpecialActivated = True
                RightSpecial = True
                RightHoleSpecial.state = Lightstateon
            end If
            if Target6.isdropped And Target7.isdropped And Target8.isdropped And Target9.isdropped And Target10.isdropped Then
                SpecialActivated = True
                LeftSpecial = True
                LeftHoleSpecial.state = Lightstateon
            end If
        end If
        if Target1.isdropped And Target2.isdropped And Target3.isdropped And Target4.isdropped And Target5.isdropped And Target6.isdropped And Target7.isdropped And Target8.isdropped And Target9.isdropped And Target10.isdropped Then
            DTSpecial = True
            Playsound SoundFXDOF("FD_DT5KwithReset", 128, DOFPulse, DOFContactors)
            'ResetLeftDT
            LeftDTBank_Cover.isdropped = False
            Target1.isdropped = False
            Target2.isdropped = False
            Target3.isdropped = False
            Target4.isdropped = False
            Target5.isdropped = False
            'ResetRightDT
            RightDTBank_Cover.isdropped = False
            Target6.isdropped = False
            Target7.isdropped = False
            Target8.isdropped = False
            Target9.isdropped = False
            Target10.isdropped = False

            Reset5KTimer.enabled = True

            LeftDTWL.state = Lightstateoff
            Left5K.state = Lightstateon
            RightDTWL.state = Lightstateoff
            Right5K.state = Lightstateon
        end if
    end If
End Sub

Sub Reset5KTimer_Timer
    Reset5KTimer.enabled = False
    'DropLeftDT
    LeftDTBank_Cover.isdropped = True
    Target1.isdropped = True
    Target2.isdropped = True
    Target4.isdropped = True
    Target5.isdropped = True
    'DropRightDT
    RightDTBank_Cover.isdropped = True
    Target6.isdropped = True
    Target7.isdropped = True
    Target9.isdropped = True
    Target10.isdropped = True
End Sub

Dim DTSpecial, LeftSpecial, RightSpecial, SpecialActivated
'DTSpecial = 5K Drop Targets
'LeftSpecial = Left Hole Special
'RightSpecial = Right Hole Special

Sub ResetPlayfield
    LightA = False
    LightB = False
    LightC = False

    TopA.state = Lightstateon
    TopB.state = Lightstateon
    TopC.state = Lightstateon
    CenterA.state = Lightstateoff
    CenterB.state = Lightstateoff
    CenterC.state = Lightstateoff
    LowerA.state = Lightstateon
    LowerB1.state = Lightstateon
    LowerB2.state = Lightstateon
    LowerC.state = Lightstateon

    Left500a.state = Lightstateon
    Left500b.state = Lightstateon
    Right500a.state = Lightstateon
    Right500b.state = Lightstateon

    LeftHoleSpecial.state = Lightstateoff
    RightHoleSpecial.state = Lightstateoff

    LeftDTWL.state = Lightstateon
    Left5K.state = Lightstateoff
    RightDTWL.state = Lightstateon
    Right5K.state = Lightstateoff

    B1.state = Lightstateoff
    B2.state = Lightstateoff
    B3.state = Lightstateoff
    B4.state = Lightstateoff
    B5.state = Lightstateoff
    B6.state = Lightstateoff
    B7.state = Lightstateoff
    B8.state = Lightstateoff
    B9.state = Lightstateoff
    B10.state = Lightstateoff

    ExtraBonus.state = Lightstateoff

    b1l.state = Lightstateoff
    b2l.state = Lightstateoff
    b3l.state = Lightstateoff

    'ResetLeftDT
    Target1.isdropped = False
    Target2.isdropped = False
    Target3.isdropped = False
    Target4.isdropped = False
    Target5.isdropped = False
    'ResetRightDT
    Target6.isdropped = False
    Target7.isdropped = False
    Target8.isdropped = False
    Target9.isdropped = False
    Target10.isdropped = False
    DB = False
    TB = False
    TotalBonus = 0
    DTSpecial = False
    LeftSpecial = False
    RightSpecial = False
    SpecialActivated = False
    Tilt = 0
    BumperWall1.isdropped = True
    BumperWall2.isdropped = True
    BumperWall3.isdropped = True

    TargetSound = "FD_500Target"
    DTSound = "FD_DropTarget"
    RolloverSound1 = "FD_Top_Rollover"
    RolloverSound2 = "FD_Top_Rollover2"
    OutlaneSound = "FD_Outlane"
    Soundlevel = 1

    'Chucky's Targets
    ResetChuckyDTLeft
    ResetChuckyDTRight
End Sub

Dim TotalBonus, DB, TB
'DB = Double Bonus
'TB = Triple Bonus (DB on last Ball)

Sub AddBonus
    if Tilt = 0 Then
        if not AdvanceBonusTimer.enabled Then
            if TotalBonus < 55 then
                TotalBonus = TotalBonus + 1
            end If
            UpdateBonusLights
        end If
    end If
End Sub

Dim TargetSound, DTSound, RolloverSound1, RolloverSound2, OutlaneSound, Soundlevel
Sub ScoreMotor
    if Tilt = 0 Then
        AdvanceBonusTimer.enabled = True
        Left500a.state = Lightstateoff
        Left500b.state = Lightstateoff
        Right500a.state = Lightstateoff
        Right500b.state = Lightstateoff
        LeftDTWL.state = Lightstateoff
        RightDTWL.state = Lightstateoff
        b1l.state = Lightstateoff
        b2l.state = Lightstateoff
        b3l.state = Lightstateoff
        TargetSound = "target"
        DTSound = "TargetDrop1"
        RolloverSound1 = "Soloff"
        RolloverSound2 = "Soloff"
        OutlaneSound = "Soloff"
        Soundlevel = 0.3
    end If
End Sub

Sub AdvanceBonusTimer_Timer
    AdvanceBonusTimer.enabled = False
    Left500a.state = Lightstateon
    Left500b.state = Lightstateon
    Right500a.state = Lightstateon
    Right500b.state = Lightstateon
    if not DTSpecial Then
        LeftDTWL.state = Lightstateon
        RightDTWL.state = Lightstateon
    end If
    if LightA Then
        b1l.state = Lightstateon
    end If
    if LightC Then
        b2l.state = Lightstateon
    end If
    if LightB Then
        b3l.state = Lightstateblinking
    end If
    TargetSound = "FD_500Target"
    DTSound = "FD_DropTarget"
    RolloverSound1 = "FD_Top_Rollover"
    RolloverSound2 = "FD_Top_Rollover2"
    OutlaneSound = "FD_Outlane"
    Soundlevel = 1
End Sub

Dim BonusX
Sub AwardBonus
    if Tilt = 1 Then
        TotalBonus = 0
    end If
    BonusX = 1
    if DB then BonusX = 2
    if TB then BonusX = 3
    AwardBonusTimer.enabled = True
End Sub

Sub AwardBonusTimer_Timer
    if TotalBonus > 0 Then
        Playsound "FD_BonusCount"
        Addpoints(1000 * BonusX)
        TotalBonus = TotalBonus - 1
        UpdateBonusLights
    Else
        AwardBonusTimer.enabled = False
        Playsound "FD_BonusEnd"
        EndBounsTimer1.enabled = True
    end If
End Sub

Sub EndBounsTimer1_Timer
    EndBounsTimer1.enabled = False
    if(ActivePlayer < NoofPlayers)Or(BallinPlay < BallsperGame)Then
        Playsound "FD_DrainwithBallrelease"
    end If
    EndBounsTimer2.enabled = True
End Sub

Sub EndBounsTimer2_Timer
    EndBounsTimer2.enabled = False
    NextBall
End Sub

Sub UpdateBonusLights
    B1.state = ((TotalBonus mod 10) = 1)or TotalBonus > 54
    B2.state = ((TotalBonus mod 10) = 2)or TotalBonus > 53
    B3.state = ((TotalBonus mod 10) = 3)or TotalBonus > 51
    B4.state = ((TotalBonus mod 10) = 4)or TotalBonus > 48
    B5.state = ((TotalBonus mod 10) = 5)or TotalBonus > 44
    B6.state = ((TotalBonus mod 10) = 6)or TotalBonus > 39
    B7.state = ((TotalBonus mod 10) = 7)or TotalBonus > 33
    B8.state = ((TotalBonus mod 10) = 8)or TotalBonus > 26
    B9.state = ((TotalBonus mod 10) = 9)or TotalBonus > 18
    B10.state = TotalBonus > 9
End Sub

Dim PlayerScore(4), ActivePlayer, Special1(4), Special2(4), Special3(4)
Sub Addpoints(ScorePar)
    if Tilt = 0 Then
        Nopointsscored = False
        if not GameActive then
            GameActive = True
        end If
        if ScorePar < 50 Then
            Playerscore(ActivePLayer) = Playerscore(ActivePLayer) + ScorePar
        Else
            if not AdvanceBonusTimer.enabled Then
                Playerscore(ActivePLayer) = Playerscore(ActivePLayer) + ScorePar
            end If
        end If

        if(Playerscore(ActivePLayer) >= Special1Score)and not Special1(ActivePlayer)Then
            Special1(ActivePlayer) = True
            AddSpecial
        end If
        if(Playerscore(ActivePLayer) >= Special2Score)and not Special2(ActivePlayer)Then
            Special2(ActivePlayer) = True
            AddSpecial
        end If
        if(Playerscore(ActivePLayer) >= Special3Score)and not Special3(ActivePlayer)Then
            Special3(ActivePlayer) = True
            AddSpecial
        end If
        UpdateScoreReel.enabled = True
    end If
End Sub

Sub RubberWall3_Hit:Playsound "rubber_hit_3", 0, 0.1, -0.07, 0.05:End Sub

Sub RubberSW1_Hit:Addpoints(10):Playsound "FD_10Switch", 0, 0.5, -0.08, 0.05:End Sub
Sub RubberSW2_Hit:Addpoints(10):Playsound "FD_10Switch", 0, 0.5, 0.08, 0.05:End Sub
Sub RubberSW3_Hit:MoveSwitchA:Addpoints(10):Playsound "FD_10Switch", 0, 0.5, -0.08, 0.05:End Sub
Sub RubberSW4_Hit:MoveSwitchB:Addpoints(10):Playsound "FD_10Switch", 0, 0.5, 0.08, 0.05:End Sub
Sub Gate1_Hit:Playsound "Gate5", 0, 0.5, 0.07, 0.05:End Sub

Sub MoveSwitchA
    SwitchA_1.isdropped = True
    SwitchA_2.isdropped = False
    RubberSW3.timerenabled = False
    RubberSW3.timerenabled = True
End Sub

Sub RubberSW3_timer
    RubberSW3.timerenabled = False
    SwitchA_2.isdropped = True
    SwitchA_1.isdropped = False
End Sub

Sub MoveSwitchB
    SwitchB_1.isdropped = True
    SwitchB_2.isdropped = False
    RubberSW4.timerenabled = False
    RubberSW4.timerenabled = True
End Sub

Sub RubberSW4_timer
    RubberSW4.timerenabled = False
    SwitchB_2.isdropped = True
    SwitchB_1.isdropped = False
End Sub

Sub Flippertimer_Timer
    RFPrim.RotY = RightFlipper.currentangle
    LFPrim.RotY = LeftFlipper.currentangle
    RFPrim001.RotY = RightFlipper.currentangle
    LFPrim001.RotY = LeftFlipper.currentangle
'Hands and knife
    Primitive008.Rotz = (121 +RightFlipper.currentangle)/2  'right hand
    Primitive009.Rotx = -(121 +RightFlipper.currentangle)/2 'knife
    Primitive010.Rotz = 121 - LeftFlipper.currentangle   'left hand
End Sub

Sub ShooterLaneLaunch_Hit
    if ActiveBall.vely < -8 then playsound "Launch", 0, 0.3, 0.1, 0.25
    PupEvent 124
    DOF 124, DOFPulse
End Sub

'---------------------------
'----- High Score Code -----
'---------------------------
Const HighScoreFilename = "FastDraw.txt"

Dim HSArray, HSAHighScore, HSA1, HSA2, HSA3
Dim HSScoreM, HSScore100k, HSScore10k, HSScoreK, HSScore100, HSScore10, HSScore1, HSScorex 'Define 6 different score values for each reel to use
HSArray = Array("HS_0", "HS_1", "HS_2", "HS_3", "HS_4", "HS_5", "HS_6", "HS_7", "HS_8", "HS_9", "HS_Space", "HS_Comma", "Tape")
Const DefaultHighScore = 0

Sub LoadHighScore
    Dim FileObj
    Dim ScoreFile
    Dim TextStr
    Dim SavedDataTemp3 'HighScore
    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory)then
        Exit Sub
    End if
    If Not FileObj.FileExists(UserDirectory & HighScoreFilename)then
        SetDefaultHSTD:UpdatePostIt:SaveHighScore
        Exit Sub
    End if
    Set ScoreFile = FileObj.GetFile(UserDirectory & HighScoreFilename)
    Set TextStr = ScoreFile.OpenAsTextStream(1, 0)
    If(TextStr.AtEndOfStream = True)then
        SetDefaultHSTD:UpdatePostIt:SaveHighScore
        Exit Sub
    End if
    SavedDataTemp3 = Textstr.ReadLine ' HighScore
    TextStr.Close
    HSAHighScore = SavedDataTemp3
    UpdatePostIt
    Set ScoreFile = Nothing
    Set FileObj = Nothing
End Sub

Sub SetDefaultHSTD 'bad data or missing file - reset and resave
    HSAHighScore = DefaultHighScore
    SaveHighScore
End Sub

Sub UpdatePostIt
    HSScorex = HSAHighScore
    TempScore = HSScorex
    HSScore1 = 0
    HSScore10 = 0
    HSScore100 = 0
    HSScoreK = 0
    HSScore10k = 0
    HSScore100k = 0
    HSScoreM = 0
    if len(TempScore) > 0 Then
        HSScore1 = cint(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        HSScore10 = cint(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        HSScore100 = cint(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        HSScoreK = cint(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        HSScore10k = cint(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        HSScore100k = cint(right(Tempscore, 1))
    end If
    if len(TempScore) > 1 Then
        TempScore = Left(TempScore, len(TempScore)-1)
        HSScoreM = cint(right(Tempscore, 1))
    end If
    Pscore6.image = HSArray(HSScoreM):If HSScorex < 1000000 Then PScore6.image = HSArray(10)
    Pscore5.image = HSArray(HSScore100K):If HSScorex < 100000 Then PScore5.image = HSArray(10)
    PScore4.image = HSArray(HSScore10K):If HSScorex < 10000 Then PScore4.image = HSArray(10)
    PScore3.image = HSArray(HSScoreK):If HSScorex < 1000 Then PScore3.image = HSArray(10)
    PScore2.image = HSArray(HSScore100):If HSScorex < 100 Then PScore2.image = HSArray(10)
    PScore1.image = HSArray(HSScore10):If HSScorex < 10 Then PScore1.image = HSArray(10)
    PScore0.image = HSArray(HSScore1):If HSScorex < 1 Then PScore0.image = HSArray(10)
    if HSScorex < 1000 then
        PComma.image = HSArray(10)
    else
        PComma.image = HSArray(11)
    end if
    if HSScorex < 1000000 then
        PComma2.image = HSArray(10)
    else
        PComma2.image = HSArray(11)
    end if
    if B2SOn Then
        Controller.B2SSetScore 6, HSAHighScore
    End If
End Sub

Dim FileObj, ScoreFile
Sub SaveHighScore
    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory)then
        Exit Sub
    End if
    Set ScoreFile = FileObj.CreateTextFile(UserDirectory & HighScoreFilename, True)
    ScoreFile.WriteLine HSAHighScore
    ScoreFile.Close
    Set ScoreFile = Nothing
    Set FileObj = Nothing
End Sub

Sub CheckNewHighScorePostIt(newScore)
    If CLng(newScore) > CLng(HSAHighScore)Then
        AddSpecial
        HSAHighScore = newScore
        SaveHighScore
        UpdatePostIt
    End If
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************
Function Vol(ball)                 ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
    Vol = Vol * RollingSoundFactor 'mfuegemann: adjust sound level
End Function

Function Pan(ball)                 ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / FastDraw.width-1
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
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 20 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    ' Rotate Chucky's head
    chuckyhead1.RotY = (476 - BOT(0).X) / 10
    chuckyhead2.RotY = (476 - BOT(0).X) / 10

    ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc

'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, PAN and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the PAN function will change the stereo position according
' to the position of the ball on the table.

'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.

Sub Pins_Hit(idx)
    PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit(idx)
    PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit(idx)
    PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit(idx)
    PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit(idx)
    PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit(idx)
    PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Spinner_Spin
    PlaySound "fx_spinner", 0, .25, 0, 0.25
End Sub

Sub Rubbers_Hit(idx)
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 20 then
        PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End if
    If finalspeed >= 2 AND finalspeed <= 20 then
        RandomSoundRubber()
    End If
End Sub

Sub Posts_Hit(idx)
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 16 then
        PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End if
    If finalspeed >= 2 AND finalspeed <= 16 then
        RandomSoundRubber()
    End If
End Sub

Sub RandomSoundRubber()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 2:PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 3:PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End Select
End Sub

Sub LeftFlipper_Collide(parm)
    RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
    RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 2:PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 3:PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End Select
End Sub

Sub FastDraw_Exit()
    If B2SOn Then Controller.Stop
End Sub

' Chucky's left & right droptargets - multiball
' targets are in 2 collections: chuckyDTLeft and chuckyDTRight
' when all down starts a 3 ball multiball

Dim CLeftDTCount, CRightDTCount

Sub chuckyDTLeft_Hit(idx)
    playsound SoundFXDOF(DTSound, 119, DOFPulse, DOFContactors), 0, Soundlevel, -0.08, 0.05
    Addpoints(500)
    CLeftDTCount = CLeftDTCount + 1
    If CLeftDTCount = 5 Then
        Addmultiball 3
        RTimerChukyDTLeft.Enabled = 1
PupEvent 300
    End If
End Sub

Sub chuckyDTRight_Hit(idx)
    playsound SoundFXDOF(DTSound, 120, DOFPulse, DOFContactors), 0, Soundlevel, -0.08, 0.05
    Addpoints(500)
    CRightDTCount = CRightDTCount + 1
    If CRightDTCount = 5 Then
        Addmultiball 3
        RTimerChukyDTRight.Enabled = 1
PupEvent 300
    End If
End Sub

Sub RTimerChukyDTLeft_Timer
    Me.Enabled = 0
    ResetChuckyDTLeft
End Sub

Sub ResetChuckyDTLeft
    Dim i
    CLeftDTCount = 0
    Playsound SoundFXDOF("FD_DT5KwithReset", 128, DOFPulse, DOFContactors)
    For each i in chuckyDTLeft
        i.IsDropped = 0
    Next
End Sub

Sub RTimerChukyDTRight_Timer
    Me.Enabled = 0
    ResetChuckyDTRight
End Sub

Sub ResetchuckyDTRight
    Dim i
    CRightDTCount = 0
    Playsound SoundFXDOF("FD_DT5KwithReset", 128, DOFPulse, DOFContactors)
    For each i in chuckyDTRight
        i.IsDropped = 0
    Next
End Sub

' Multiballs

Dim bMultiBallMode
bMultiBallMode = False

Dim BallsOnPlayfield
BallsOnPlayfield = 0

Dim mBalls2Eject
mBalls2Eject = 0

Sub CreateMultiball
    BallRelease.CreateBall
    BallRelease.Kick 90, 7
    PupEvent 122
    DOF 122, DOFPulse
    BallsOnPlayfield = BallsOnPlayfield + 1
    bMultiBallMode = True
End Sub

Sub swPlungerRest_Hit
    If bMultiBallMode Then 'kick the ball up
        activeball.VelY = -50
    End If
End Sub

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
End Sub

Sub CreateMultiballTimer_Timer()
    CreateMultiball
    mBalls2Eject = mBalls2Eject -1
    If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
        CreateMultiballTimer.Enabled = False
    End If
End Sub

'  corner droptargets

Sub Target010_Hit 'left corner
    PlaySound SoundFXDOF(TargetSound, 106, DOFPulse, DOFContactors), 0, Soundlevel, -0.09, 0.05
    If bMultiBallMode Then
        Addpoints(10000)
    Else
        Addpoints(2500)
    End If
    Target010.TimerEnabled = 1 'to reset the target
End Sub

Sub Target010_Timer
    Me.TimerEnabled = 0
    Target010.IsDropped = 0
End Sub

Sub Target011_Hit 'right corner
    PlaySound SoundFXDOF(TargetSound, 106, DOFPulse, DOFContactors), 0, Soundlevel, -0.09, 0.05
    If bMultiBallMode Then
        Addpoints(10000)
    Else
        Addpoints(2500)
    End If
    Target011.TimerEnabled = 1 'to reset the target
End Sub

Sub Target011_Timer
    Me.TimerEnabled = 0
    Target011.IsDropped = 0
End Sub


'**************************
'   PinUp Player USER Config
'**************************

dim PuPDMDDriverType: PuPDMDDriverType=0   ' 0=LCD DMD, 1=RealDMD 2=FULLDMD (large/High LCD)
dim useRealDMDScale : useRealDMDScale=0    ' 0 or 1 for RealDMD scaling.  Choose which one you prefer.
dim useDMDVideos    : useDMDVideos=true   ' true or false to use DMD splash videos.
Dim pGameName       : pGameName="Fast_Draw_1975"  'pupvideos foldername, probably set to cGameName in realworld







'********************* START OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF BELOW   THIS LINE!!!! ***************
'******************************************************************************
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********
'******************************************************************************
'
'
'  Quick Steps:
'      1>  create a folder in PUPVideos with Starter_PuPPack.zip and call the folder "yourgame"
'      2>  above set global variable pGameName="yourgame"
'      3>  copy paste the settings section above to top of table script for user changes.
'      4>  on Table you need to create ONE timer only called pupDMDUpdate and set it to 250 ms enabled on startup.
'      5>  go to your table1_init or table first startup function and call PUPINIT function
'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
'      7>  attractmodenext at bottom is setup for you already,  just go to each case and add/remove as many as you want and setup the messages to show.  
'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
'
'
'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
'       and for future pinball you need to add the follow lines near top
'Need to use BAM and have com idll enabled.
'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
'				if icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
'				Function CreateObject(className)       
'   					Set CreateObject = icom.CreateObject(className)   
'				End Function


Const HasPuP = True   'dont set to false as it will break pup

Const pDMD=1


'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pTargerLetters=5

'dmdType
Const pDMDTypeLCD=0
Const pDMDTypeReal=1
Const pDMDTypeFULL=2






Dim PuPlayer
dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pInAttract : pInAttract=false   'pAttract mode




'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit

Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
PuPlayer.B2SInit "", pGameName

if (PuPDMDDriverType=pDMDTypeReal) and (useRealDMDScale=1) Then 
       PuPlayer.setScreenEx pDMD,0,0,128,32,0  'if hardware set the dmd to 128,32
End if

PuPlayer.LabelInit pDMD


if PuPDMDDriverType=pDMDTypeReal then
Set PUPDMDObject = CreateObject("PUPDMDControl.DMD") 
PUPDMDObject.DMDOpen
PUPDMDObject.DMDPuPMirror
PUPDMDObject.DMDPuPTextMirror
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":32, ""FQ"":3 }"   'set no antialias on font render if real
END IF


pSetPageLayouts

pDMDSetPage(pDMDBlank)   'set blank text overlay page.
pDMDStartUP				 ' firsttime running for like an startup video..


End Sub 'end PUPINIT



'PinUP Player DMD Helper Functions

Sub pDMDLabelHide(labName)
PuPlayer.LabelSet pDMD,labName,"",0,""   
end sub




Sub pDMDScrollBig(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub

Sub pDMDScrollBigV(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub


Sub pDMDSplashScore(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
end Sub

Sub pDMDSplashScoreScroll(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
end Sub

Sub pDMDZoomBig(msgText,timeSec,mColor)  'new Zoom
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':3,'hstart':5,'hend':80,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
end sub

Sub pDMDTargetLettersInfo(msgText,msgInfo, timeSec)  'msgInfo = '0211'  0= layer 1, 1=layer 2, 2=top layer3.
'this function is when you want to hilite spelled words.  Like B O N U S but have O S hilited as already hit markers... see example.
PuPlayer.LabelShowPage pDMD,5,timeSec,""  'show page 5
Dim backText
Dim middleText
Dim flashText
Dim curChar
Dim i
Dim offchars:offchars=0
Dim spaces:spaces=" "  'set this to 1 or more depends on font space width.  only works with certain fonts
                          'if using a fixed font width then set spaces to just one space.

For i=1 To Len(msgInfo)
    curChar="" & Mid(msgInfo,i,1)
    if curChar="0" Then
            backText=backText & Mid(msgText,i,1)
            middleText=middleText & spaces
            flashText=flashText & spaces          
            offchars=offchars+1
    End If
    if curChar="1" Then
            backText=backText & spaces
            middleText=middleText & Mid(msgText,i,1)
            flashText=flashText & spaces
    End If
    if curChar="2" Then
            backText=backText & spaces
            middleText=middleText & spaces
            flashText=flashText & Mid(msgText,i,1)
    End If   
Next 

if offchars=0 Then 'all litup!... flash entire string
   backText=""
   middleText=""
   FlashText=msgText
end if  

PuPlayer.LabelSet pDMD,"Back5"  ,backText  ,1,""
PuPlayer.LabelSet pDMD,"Middle5",middleText,1,""
PuPlayer.LabelSet pDMD,"Flash5" ,flashText ,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & "}"   
end Sub


Sub pDMDSetPage(pagenum)    
    PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub

Sub pHideOverlayText(pDisp)
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& pDisp &", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
end Sub



Sub pDMDShowLines3(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,3,timeSec,""
PuPlayer.LabelSet pDMD,"Splash3a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash3b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash3c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowLines2(msgText,msgText2,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,4,timeSec,""
PuPlayer.LabelSet pDMD,"Splash4a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash4b",msgText2,vis,pLine2Ani
end Sub

Sub pDMDShowCounter(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,6,timeSec,""
PuPlayer.LabelSet pDMD,"Splash6a",msgText,vis, pLine1Ani
PuPlayer.LabelSet pDMD,"Splash6b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash6c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowBig(msgText,timeSec, mColor)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,vis,pLine1Ani
end sub


Sub pDMDShowHS(msgText,msgText2,msgText3,timeSec) 'High Score
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,7,timeSec,""
PuPlayer.LabelSet pDMD,"Splash7a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash7b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash7c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDSetBackFrame(fname)
  PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
end Sub

Sub pDMDStartBackLoop(fPlayList,fname)
  PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
  PuPlayer.SetBackGround pDMD,1
end Sub

Sub pDMDStopBackLoop
  PuPlayer.SetBackGround pDMD,0
  PuPlayer.playstop pDMD
end Sub


Dim pNumLines

'Theme Colors for Text (not used currenlty,  use the |<colornum> in text labels for colouring.
Dim SpecialInfo
Dim pLine1Color : pLine1Color=8454143
Dim pLine2Color : pLine2Color=8454143    
Dim pLine3Color :  pLine3Color=8454143    
Dim curLine1Color: curLine1Color=pLine1Color  'can change later
Dim curLine2Color: curLine2Color=pLine2Color  'can change later
Dim curLine3Color: curLine3Color=pLine3Color  'can change later


Dim pDMDCurPriority: pDMDCurPriority =-1
Dim pDMDDefVolume: pDMDDefVolume = 0   'default no audio on pDMD

Dim pLine1
Dim pLine2
Dim pLine3
Dim pLine1Ani
Dim pLine2Ani
Dim pLine3Ani

Dim PriorityReset:PriorityReset=-1
DIM pAttractReset:pAttractReset=-1
DIM pAttractBetween: pAttractBetween=2000 '1 second between calls to next attract page
DIM pDMDVideoPlaying: pDMDVideoPlaying=false


'************************ where all the MAGIC goes,  pretty much call this everywhere  ****************************************
'*************************                see docs for examples                ************************************************
'****************************************   DONT TOUCH THIS CODE   ************************************************************

Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
' pEventID = reference if application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' also global variable useDMDVideos=true/false if user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation if any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345

DIM curPos
if pDMDCurPriority>pPriority then Exit Sub  'if something is being displayed that we don't want interrupted.  same level will interrupt.
pDMDCurPriority=pPriority
if timeSec=0 then timeSec=1 'don't allow page default page by accident


pLine1=""
pLine2=""
pLine3=""
pLine1Ani=""
pLine2Ani=""
pLine3Ani=""


if pAni=1 Then  'we flashy now aren't we
pLine1Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
pLine2Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
pLine3Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
end If

curPos=InStr(pText,"^")   'Lets break apart the string if needed
if curPos>0 Then 
   pLine1=Left(pText,curPos-1) 
   pText=Right(pText,Len(pText) - curPos)
   
   curPos=InStr(pText,"^")   'Lets break apart the string
   if curPOS>0 Then
      pLine2=Left(pText,curPos-1) 
      pText=Right(pText,Len(pText) - curPos)

      curPos=InStr("^",pText)   'Lets break apart the string   
      if curPos>0 Then
         pline3=Left(pText,curPos-1) 
      Else 
        if pText<>"" Then pline3=pText 
      End if 
   Else 
      if pText<>"" Then pLine2=pText
   End if    
Else 
  pLine1=pText  'just one line with no break 
End if


'lets see how many lines to Show
pNumLines=0
if pLine1<>"" then pNumLines=pNumlines+1
if pLine2<>"" then pNumLines=pNumlines+1
if pLine3<>"" then pNumLines=pNumlines+1

if pDMDVideoPlaying and (VideoName="") Then 
			PuPlayer.playstop pDMD
			pDMDVideoPlaying=False

End if


if (VideoName<>"") and (useDMDVideos) Then  'we are showing a splash video instead of the text.
    
    PuPlayer.playlistplayex pDMD,"DMDSplash",VideoName,pDMDDefVolume,pPriority  'should be an attract background (no text is displayed)
    pDMDVideoPlaying=true
end if 'if showing a splash video with no text




if StrComp(pEventID,"shownum",1)=0 Then              'check eventIDs
    pDMDShowCounter pLine1,pLine2,pLine3,timeSec
Elseif StrComp(pEventID,"target",1)=0 Then              'check eventIDs
    pDMDTargetLettersInfo pLine1,pLine2,timeSec
Elseif StrComp(pEventID,"highscore",1)=0 Then              'check eventIDs
    pDMDShowHS pLine1,pLine2,pline3,timeSec
Elseif (pNumLines=3) Then                'depends on # of lines which one to use.  pAni=1 will flash.
    pDMDShowLines3 pLine1,pLine2,pLine3,TimeSec
Elseif (pNumLines=2) Then
    pDMDShowLines2 pLine1,pLine2,TimeSec
Elseif (pNumLines=1) Then
    pDMDShowBig pLine1,timeSec, curLine1Color
Else
    pDMDShowBig pLine1,timeSec, curLine1Color
End if

PriorityReset=TimeSec*1000
End Sub 'pupDMDDisplay message

Sub pupDMDupdate_Timer()
	pUpdateScores    

    if PriorityReset>0 Then  'for splashes we need to reset current prioirty on timer
       PriorityReset=PriorityReset-pupDMDUpdate.interval
       if PriorityReset<=0 Then 
            pDMDCurPriority=-1            
            if pInAttract then pAttractReset=pAttractBetween ' pAttractNext  call attract next after 1 second
			pDMDVideoPlaying=false			
			End if
    End if

    if pAttractReset>0 Then  'for splashes we need to reset current prioirty on timer
       pAttractReset=pAttractReset-pupDMDUpdate.interval
       if pAttractReset<=0 Then 
            pAttractReset=-1            
            if pInAttract then pAttractNext
			End if
    end if 
End Sub

Sub PuPEvent(EventNum)
if hasPUP=false then Exit Sub
PuPlayer.B2SData "E"&EventNum,1  'send event to puppack driver  
End Sub


'********************* END OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF ABOVE THIS LINE!!!! ***************
'****************************************************************************

'*****************************************************************
'   **********  PUPDMD  MODIFY THIS SECTION!!!  ***************
'PUPDMD Layout for each Table1
'Setup Pages.  Note if you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS  "case sensitive exact naming fonts!"
'*****************************************************************

Sub pSetPageLayouts

DIM dmddef
DIM dmdalt
DIM dmdscr
DIM dmdfixed

'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
'***********************************************************************'
'<screen#>, in standard wed set this to pDMD ( or 1)
'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, degrees in tenths   (900=90 degrees)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but if you want to force a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT this will assign this label to this page or group.
'<visible> initial state of label. visible=1 show, 0 = off.



if PuPDMDDriverType=pDMDTypeReal Then 'using RealDMD Mirroring.  **********  128x32 Real Color DMD  
	dmdalt="PKMN Pinball"
    dmdfixed="Instruction"
    dmdscr="Impact"    'main scorefont
	dmddef="Zig"

	'Page 1 (default score display)
  		 PuPlayer.LabelNew pDMD,"Credits" ,dmddef,20,33023   ,0,2,2,95,0,1,0
		 PuPlayer.LabelNew pDMD,"Play1"   ,dmdalt,21,33023   ,1,0,0,15,0,1,0
		 PuPlayer.LabelNew pDMD,"Ball"    ,dmdalt,21,33023   ,1,2,0,85,0,1,0
		 PuPlayer.LabelNew pDMD,"MsgScore",dmddef,45,33023   ,0,1,0, 0,40,1,0
		 PuPlayer.LabelNew pDMD,"CurScore",dmdscr,60,8454143   ,0,1,1, 0,0,1,0


	'Page 2 (default Text Splash 1 Big Line)
		 PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40,33023,0,1,1,0,0,2,0

	'Page 3 (default Text Splash 2 and 3 Lines)
		 PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,8454143,0,1,0,0,2,3,0
		 PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,33023,0,1,0,0,30,3,0
	     PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,33023,0,1,0,0,55,3,0


	'Page 4 (2 Line Gameplay DMD)
		 PuPlayer.LabelNew pDMD,"Splash4a",dmddef,40,8454143,0,1,0,0,0,4,0
	     PuPlayer.LabelNew pDMD,"Splash4b",dmddef,30,33023,0,1,2,0,75,4,0

	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0

 	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,8454143,0,1,0,0,2,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,33023,0,1,0,0,20,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,33023,0,1,0,0,50,7,0


END IF  ' use PuPDMDDriver

Const fontScale = 0.7

if PuPDMDDriverType=pDMDTypeLCD THEN  'Using 4:1 Standard ratio LCD PuPDMD  ************ lcd **************

	'dmddef="Impact"
	dmdalt="OhTheHorror"    
    dmdfixed="Instruction"
	dmdscr="Impact"  'main score font
	dmddef="Impact"

	'Page 1 (default score display)
		PuPlayer.LabelNew pDMD,"Credits" ,dmddef,20 * fontScale,33023   ,0,2,2,95,0,1,0
		PuPlayer.LabelNew pDMD,"Play1"   ,dmdalt,20 * fontScale,33023   ,1,0,0,15,0,1,0
		PuPlayer.LabelNew pDMD,"Ball"    ,dmdalt,20 * fontScale,33023   ,1,2,0,85,0,1,0
		PuPlayer.LabelNew pDMD,"MsgScore",dmddef,45 * fontScale,33023   ,0,1,0, 0,40,1,0
		PuPlayer.LabelNew pDMD,"CurScore",dmdscr,60 * fontScale,8454143   ,0,1,1, 0,0,1,0


	'Page 2 (default Text Splash 1 Big Line)
		PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40 * fontScale,33023,0,1,1,0,0,2,0

	'Page 3 (default Text 3 Lines)
		PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30 * fontScale,8454143,0,1,0,0,2,3,0
		PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30 * fontScale,33023,0,1,0,0,30,3,0
		PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25 * fontScale,33023,0,1,0,0,57,3,0


	'Page 4 (default Text 2 Line)
		PuPlayer.LabelNew pDMD,"Splash4a",dmddef,40 * fontScale,8454143,0,1,0,0,0,4,0
		PuPlayer.LabelNew pDMD,"Splash4b",dmddef,30 * fontScale,33023,0,1,2,0,75,4,0

	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80 * fontScale,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80 * fontScale,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80 * fontScale,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90 * fontScale,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50 * fontScale,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40 * fontScale,33023,0,1,0,60,50,6,0

	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20 * fontScale,8454143,0,1,0,0,2,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40 * fontScale,33023,0,1,0,0,20,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40 * fontScale,33023,0,1,0,0,50,7,0


END IF  ' use PuPDMDDriver

if PuPDMDDriverType=pDMDTypeFULL THEN  'Using FULL BIG LCD PuPDMD  ************ lcd **************

	'dmddef="Impact"
	dmdalt="OhTheHorror"    
    dmdfixed="Instruction"
	dmdscr="Impact"  'main score font
	dmddef="Impact"

	'Page 1 (default score display)
		PuPlayer.LabelNew pDMD,"Credits" ,dmddef,20,33023   ,0,2,2,95,0,1,0
		PuPlayer.LabelNew pDMD,"Play1"   ,dmdscr,10, 16777215  ,1,0,0,10,0,1,0
		PuPlayer.LabelNew pDMD,"Ball"    ,dmdscr,10,  16777215 ,1,2,0,85,0,1,0
		PuPlayer.LabelNew pDMD,"MsgScore",dmddef,45, 242  ,0,1,0, 0,40,1,0
		PuPlayer.LabelNew pDMD,"CurScore",dmdalt,38, 242 ,0,1,1, 0,80,1,0		


	'Page 2 (default Text Splash 1 Big Line)
		PuPlayer.LabelNew pDMD,"Splash"  ,dmddef,20,242,0,1,1,0,0,2,0

	'Page 3 (default Text 3 Lines)
		PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,8454143,0,1,0,0,2,3,0
		PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,33023,0,1,0,0,30,3,0
		PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,33023,0,1,0,0,57,3,0


	'Page 4 (default Text 2 Line)
		PuPlayer.LabelNew pDMD,"Splash4a",dmddef,40,8454143,0,1,0,0,0,4,0
		PuPlayer.LabelNew pDMD,"Splash4b",dmddef,30,33023,0,1,2,0,75,4,0

	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0

	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,8454143,0,1,0,0,2,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,33023,0,1,0,0,20,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,33023,0,1,0,0,50,7,0


END IF  ' use PuPDMDDriver




end Sub 'page Layouts


'*****************************************************************
'        PUPDMD Custom SUBS/Events for each Table1
'     **********    MODIFY THIS SECTION!!!  ***************
'*****************************************************************
'
'
'  we need to somewhere in code if applicable
'
'   call pDMDStartGame,pDMDStartBall,pGameOver,pAttractStart
'
'
'
'
'


Sub pDMDStartGame
pInAttract=false
pDMDSetPage(pScores)   'set blank text overlay page.

end Sub


Sub pDMDStartBall
end Sub

Sub pDMDGameOver
pAttractStart
end Sub

Sub pAttractStart
pDMDSetPage(pDMDBlank)   'set blank text overlay page.
pCurAttractPos=0
pInAttract=True          'Startup in AttractMode
pAttractNext
end Sub

Sub pDMDStartUP
 pupDMDDisplay "attract","Welcome To","",2,0,10
 pInAttract=true
end Sub

DIM pCurAttractPos: pCurAttractPos=0


'********************** gets called auto each page next and timed already in DMD_Timer.  make sure you use pupDMDDisplay or it wont advance auto.
Sub pAttractNext
pCurAttractPos=pCurAttractPos+1

  Select Case pCurAttractPos

  Case 1 pupDMDDisplay "attract","Child's Play","",5,1,10
  Case 2 pupDMDDisplay "attract","Hi, I'm Tommy","",3,0,10
  Case 3 pupDMDDisplay "attract","And I'm Your Friend","",2,0,10
  Case 4 pupDMDDisplay "attract","Till The End","",3,1,10
  Case 5 pupDMDDisplay "attract","Hidey-Ho, HaHaHa","",1,0,10
  Case 6 pupDMDDisplay "attract","Heh, Wanna Play?","",3,1,10
  Case 7 pupDMDDisplay "attract","I Like To Be Hugged","",2,0,10
  Case 8 pupDMDDisplay "attract","Heh, Wanna Play?","",1,0,10
  Case 9 pupDMDDisplay "attract","You Just Can't Keep","",1,1,10
  Case 10 pupDMDDisplay "attract","A Good Guy Down","",3,1,10
  Case Else
    pCurAttractPos=0
    pAttractNext 'reset to beginning
  end Select

end Sub


'************************ called during gameplay to update Scores ***************************

Sub pUpdateScores  'call this ONLY on timer 300ms is good enough
if pDMDCurPage <> pScores then Exit Sub
'puPlayer.LabelSet pDMD,"Credits","CREDITS " & ""& Credits ,1,""
'puPlayer.LabelSet pDMD,"Play1","Player 1",1,""
'puPlayer.LabelSet pDMD,"Ball"," "&pDMDCurPriority ,1,""

puPlayer.LabelSet pDMD,"CurScore",""  & FormatNumber(PlayerScore(ActivePlayer),0),1,""
puPlayer.LabelSet pDMD,"Play1","Player: " & NoofPlayers,1,""
puPlayer.LabelSet pDMD,"Ball","Ball: " & BallinPlay,1,""
end Sub