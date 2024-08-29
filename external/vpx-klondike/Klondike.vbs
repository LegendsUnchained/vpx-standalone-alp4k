' ***********************************************************************
'               VISUAL PINBALL X EM Script by JPSalas
'                  Basic EM script up to 4 players
'		          uses core.vbs for extra functions
'
' Leon Spalding old version
' DOF config - leeoneil
' https://www.ipdb.org/machine.cgi?id=1388
' Klondike Williams 1971 VPX table by Kiwi, version 1.0.2
' ***********************************************************************

Option Explicit
Randomize

'******************************************** Options ***************************************

'****************************************** Volume Settings ********************************************

Const RolVol = 1	'Ball Rolling 
Const RubVol = 1	'Rubbers Collision
Const DroVol = 1	'Ball jump
Const NudVol = 1	'Nudge
Const PlpVol = 1	'Plunger pull
Const PlfVol = 1	'Plunger fire
Const SliVol = 1	'Slingshots
Const BumVol = 1	'Bumpers
Const SwiVol = 1	'Rollovers
Const TarVol = 1	'Targets
Const PouVol = 1	'Post up
Const PodVol = 1	'Post down
Const GatVol = 1	'Gate
Const KicVol = 1	'Kickers catch
Const KieVol = 1	'Kickers eject
Const KidVol = 1	'Kicker Drain
Const BarVol = 1	'Ball release
Const FluVol = 1	'Flippers up
Const FldVol = 1	'Flippers down
Const FlbVol = 0.2	'Flippers buzz
Const CoiVol = 1	'Coin
Const KnoVol = 1	'Knocker
Const TilVol = 1	'Tilt
Const RepVol = 1	'Reels points/credits
Const ResVol = 1	'Reels symbols
Const BdeVol = 0.15	'Bell 10
Const BceVol = 0.3	'Bell 100
Const BmiVol = 0.3	'Bell 1000
Const IniVol = 1	'Initialize/reset, start of game
Const MatVol = 1	'Match, end of game
Const BcuVol = 0.4	'Ball count unit
Const GirVol = 1	'General Illumination Relay

'************************ Balls per game: real pinball have 3 or 5 balls, you can put max 9

Const BallsPerGame = 5

'************************ Ball: 50 unit is standard ball size ** Mass=(50^3)/125000 ,(BallSize^3)/125000 ** Max ball velocity

Const BallSize = 50

Const BallMass = 1

Const MaxVel = 50

'************************ Ball Shadow: 0 hidden , 1 visible

Const BallSHW = 1

'************************ Show score on the instruction card: 0 off, 1 on

Const ScoreOnCard = 0

'************************ Chime 100 & 1000 points same sound (like an real): 0, separated souds: 1

Const Chimesep = 0

'************************ Rails Hidden/Visible in FS mode: 0 hidden , 1 visible

Const RailsVisible = 0

'************************ Color Grading LUT: 1 = Active, any other value = disabled

Const LutEnabled = 1

'******************************************** OPTIONS END ***********************************

' Load extra vbs files
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    On Error Resume Next
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
End Sub

' Constants
Const TableName = "Klondike1971" 		  ' file name to save highscores and other variables
Const cGameName = "Klondike1971" 		  ' B2S name
Const MaxPlayers = 1                      ' 1 to 4 can play
Const MaxMultiplier = 1                   ' limit bonus multiplier
Const FreePlay = False                    ' Free play or coins
Const SpecialMode = 4                     ' 1 = add-a-ball, 2 = replay, 3 = Novelty, 4 Legacy mode
'Const NoveltyScore = 100000               ' novelty score is set to 100.000 points
Const Electrician_Mode = 1                ' if 1 shows the 100.000 light when the score is over 100K

Dim Special1, Special2, Special3

If BallsPerGame > 4 Then
    Special1 = 90000 ' extra ball or credit when reaching this score
    Special2 = 150000
    Special3 = 180000
ElseIf BallsPerGame < 5 Then
    Special1 = 60000
    Special2 = 120000
    Special3 = 190000
End If

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
Dim Special4Awarded(4)
Dim Score(4)
Dim HighScore
Dim Match
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim Add10
Dim Add100
Dim Add1000
Dim Add10000
Dim x

Dim rv(3)
Dim Dec(4)
Dim Cen(4)
Dim Mil(4)
Dim DMi(4)
Dim Ngr
Dim Bpg
Dim NewGReset
Dim BallsPG

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
Dim bBallSaverActive'********************************** Klondike init
Sub Table1_Init
    ' Init some objects, like walls, targets
    VPObjects_Init
	LoadEM

    Set Dec(1)=Dec1
    Set Cen(1)=Cen1
    Set Mil(1)=Mil1
    Set DMi(1)=DMi1
    Set Dec(2)=Dec2
    Set Cen(2)=Cen2
    Set Mil(2)=Mil2
    Set DMi(2)=DMi2
    Set Dec(3)=Dec3
    Set Cen(3)=Cen3
    Set Mil(3)=Mil3
    Set DMi(3)=DMi3
    Set Dec(4)=Dec4
    Set Cen(4)=Cen4
    Set Mil(4)=Mil4
    Set DMi(4)=DMi4

    ' load score/highscore
	Score(1) = 0
	rv(1) = 0
	rv(2) = 0
	rv(3) = 0
    Credits = 1
    Loadhs
    CurrentPlayer = 1
    ScoreReel1.SetValue Score(1)
    UpdateBReels
    UpdateCredits
    Display_Match

    If B2SOn Then
        Controller.B2SSetScorePlayer 1, Score(1)
        Controller.B2SSetGameOver 1
    End If

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
    Add10000 = 0
    Ngr = 0

    ' setup table in game over mode
    EndOfGame

    'turn on GI lights
    vpmtimer.addtimer 1000, "GiOn '"

	CheckReels
	ReelLeft.RotX = -rv(1)*36
	ReelCenter.RotX = -rv(2)*36
	ReelRight.RotX = -rv(3)*36

 If Table1.ShowDT = True Then

End If

 If Table1.ShowDT = False Then
	CreditsReel.Visible = 0
	ScoreReel1.Visible = 0
	BallstoPlayR.Visible = 0
	GameOverReel.Visible = 0
	MatchR.Visible = 0
	CentoMR.Visible = 0
	TiltReel.Visible = 0
	CabRailSX.Visible = RailsVisible
	CabRailDX.Visible = RailsVisible
End If

 If Table1.ShowFSS = True Then
	CreditsReel.Visible = 0
	ScoreReel1.Visible = 0
	BallstoPlayR.Visible = 0
	GameOverReel.Visible = 0
	MatchR.Visible = 0
	CentoMR.Visible = 0
	TiltReel.Visible = 0
End If

	fgi1.IntensityScale = 0
	fgi2.IntensityScale = 0
	fgi3.IntensityScale = 0
	fbgi001.IntensityScale = 0
	fbgi002.IntensityScale = 0
	fbgi003.IntensityScale = 0
	fbgi004.IntensityScale = 0
	fbgi005.IntensityScale = 0
	fbgi006.IntensityScale = 0
	fbgi007.IntensityScale = 0
	fbgi008.IntensityScale = 0
	fbgi009.IntensityScale = 0
	fbgi010.IntensityScale = 0
	fbgi011.IntensityScale = 0
	fbgi012.IntensityScale = 0
	fbgi013.IntensityScale = 0
	fbgi014.IntensityScale = 0
	fbgi015.IntensityScale = 0
	fbgi016.IntensityScale = 0
	fbgi017.IntensityScale = 0
	fbgi018.IntensityScale = 0
	fbgi019.IntensityScale = 0

	fb1.Visible = 0
	fb2.Visible = 0
	fb3.Visible = 0
	fb4.Visible = 0
	fb5.Visible = 0
	fb6.Visible = 0
	fb7.Visible = 0
	fb8.Visible = 0
	fb9.Visible = 0
	fTilt.Visible = 0
	fCentoM.Visible = 0

' Backbox Flashers Y Position

Const Fpy = 68.2

fbgi001.y = Fpy:fbgi002.y = Fpy:fbgi003.y = Fpy:fbgi004.y = Fpy:fbgi005.y = Fpy:fbgi006.y = Fpy:fbgi007.y = Fpy:fbgi008.y = Fpy
fbgi009.y = Fpy:fbgi010.y = Fpy:fbgi011.y = Fpy:fbgi012.y = Fpy:fbgi013.y = Fpy:fbgi014.y = Fpy
fbgi015.y = 67:fbgi016.y = 67:fbgi017.y = 67:fbgi018.y = 67:fbgi019.y = 67

fb1.y = Fpy:fb2.y = Fpy:fb3.y = Fpy:fb4.y = Fpy:fb5.y = Fpy:fb6.y = Fpy:fb7.y = Fpy:fb8.y = Fpy:fb9.y = Fpy
fGameOver.y = Fpy:fTilt.y = Fpy:fCentoM.y = Fpy
fM00.y = Fpy:fM10.y = Fpy:fM20.y = Fpy:fM30.y = Fpy:fM40.y = Fpy:fM50.y = Fpy:fM60.y = Fpy:fM70.y = Fpy:fM80.y = Fpy:fM90.y = Fpy
End Sub'********************************** Key DownSub Table1_KeyDown(ByVal Keycode)

    If EnteringInitials Then
        CollectInitials(keycode)
        Exit Sub
    End If

    If Keycode = LeftMagnaSave And LutEnabled = 1 Then bLutActive = True: Lutbox.Text = "level of darkness " & LUTImage + 1
    If Keycode = RightMagnaSave Then
        If bLutActive Then NextLUT: End If
    End If

    ' add coins, limit to 9 credits
    If Keycode = AddCreditKey Then
        If(Tilted = False)Then
            PlaySoundAtVol "coin3", Bumper3, CoiVol
            If Credits < 9 Then
                AddCredits 1
            End If
        End If
    End If    
    ' plunger	If Keycode = PlungerKey Then:Plunger.Pullback:PlaySoundAtVol "fx_plungerpull", Plunger, PlpVol:End If
    ' tilt keys	If Keycode = LeftTiltKey Then:Nudge 90, 4:PlaySound SoundFX("fx_nudge",0), 0, NudVol, -0.1, 0.25:CheckTilt:End If
	If Keycode = RightTiltKey Then:Nudge 270, 4:PlaySound SoundFX("fx_nudge",0), 0, NudVol, 0.1, 0.25:CheckTilt:End If
	If Keycode = CenterTiltKey Then:Nudge 0, 5:PlaySound SoundFX("fx_nudge",0), 0, NudVol, 0, 0.25:CheckTilt:End If
    ' keys during game

    If bGameInPlay AND NOT Tilted Then
        If Keycode = LeftFlipperKey Then SolLFlipper 1
        If Keycode = RightFlipperKey Then SolRFlipper 1

        If Keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                'PlayersReel.SetValue, PlayersPlayingGame
                'PlaySound "so_fanfare1"
                Else
                    If(Credits > 0)Then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        AddCredits - 1
                    Else
                    ' Not Enough Credits to start a game.
                    'PlaySound "so_nocredits"
                    End If
                End If
            End If
        End If
        Else

            If Keycode = StartGameKey Then
                If(bFreePlay = True)Then
                    If(BallsOnPlayfield = 0)Then
                        ResetScores
                        ResetForNewGame()
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
                            AddCredits - 1
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
'********************************** Key Up
Sub Table1_KeyUp(ByVal Keycode)    If EnteringInitials Then
        Exit Sub
    End If

    If Keycode = LeftMagnaSave Then bLutActive = False: LutBox.text = ""

    If bGameInPlay AND NOT Tilted Then
        If Keycode = LeftFlipperKey Then SolLFlipper 0
        If Keycode = RightFlipperKey Then SolRFlipper 0
    End If
	If Keycode = PlungerKey Then:Plunger.Fire:PlaySoundAtVol "plunger", Plunger, PlfVol * (Plunger.Position/25):End If
        If bBallInPlungerLane Then
            PlaySoundAt "fx_plunger", Plunger
        Else
            PlaySoundAt "fx_plunger_empty", Plunger
        End If
End Sub
'******************
' Table stop/pause
'******************

Sub Table1_Paused
End Sub

Sub Table1_unPaused
End Sub

Sub Table1_Exit
    Savehs
'Controller.Stop
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
        Case 0:table1.ColorGradeImage = "LUT0":GiIntensity = 1:FlIntensity = 1:ChangeGIIntensity 1:UpdateFlashers
        Case 1:table1.ColorGradeImage = "LUT1":GiIntensity = 1.05:FlIntensity = 1.2:ChangeGIIntensity 1:UpdateFlashers
        Case 2:table1.ColorGradeImage = "LUT2":GiIntensity = 1.1:FlIntensity = 1.4:ChangeGIIntensity 1:UpdateFlashers
        Case 3:table1.ColorGradeImage = "LUT3":GiIntensity = 1.15:FlIntensity = 1.6:ChangeGIIntensity 1:UpdateFlashers
        Case 4:table1.ColorGradeImage = "LUT4":GiIntensity = 1.2:FlIntensity = 1.8:ChangeGIIntensity 1:UpdateFlashers
        Case 5:table1.ColorGradeImage = "LUT5":GiIntensity = 1.25:FlIntensity = 2:ChangeGIIntensity 1:UpdateFlashers
        Case 6:table1.ColorGradeImage = "LUT6":GiIntensity = 1.3:FlIntensity = 2.2:ChangeGIIntensity 1:UpdateFlashers
        Case 7:table1.ColorGradeImage = "LUT7":GiIntensity = 1.35:FlIntensity = 2.4:ChangeGIIntensity 1:UpdateFlashers
        Case 8:table1.ColorGradeImage = "LUT8":GiIntensity = 1.4:FlIntensity = 2.6:ChangeGIIntensity 1:UpdateFlashers
        Case 9:table1.ColorGradeImage = "LUT9":GiIntensity = 1.45:FlIntensity = 2.8:ChangeGIIntensity 1:UpdateFlashers
        Case 10:table1.ColorGradeImage = "LUT10":GiIntensity = 1.5:FlIntensity = 3:ChangeGIIntensity 1:UpdateFlashers
        Case 11:table1.ColorGradeImage = "LUT11":GiIntensity = 1.55:FlIntensity = 3.2:ChangeGIIntensity 1:UpdateFlashers
        Case 12:table1.ColorGradeImage = "LUT12":GiIntensity = 1.6:FlIntensity = 3.4:ChangeGIIntensity 1:UpdateFlashers
        Case 13:table1.ColorGradeImage = "LUT13":GiIntensity = 1.65:FlIntensity = 3.6:ChangeGIIntensity 1:UpdateFlashers
        Case 14:table1.ColorGradeImage = "LUT14":GiIntensity = 1.7:FlIntensity = 3.8:ChangeGIIntensity 1:UpdateFlashers
    End Select
End Sub

Dim GiIntensity, FlIntensity
GiIntensity = 1   'used by the LUT changing to increase the GI lights when the table is darker
FlIntensity = 1

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

'********************
'     Flippers
'********************   

Dim FlipLOn, FlipROn
Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAtVol SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper, FluVol
		PlaySoundLoopAtVol "buzz", LeftFlipper, FlbVol
        LeftFlipper.RotateToEnd:FlipLOn = 1
    Else
        PlaySoundAtVol SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper, FldVol
		StopSound "buzz"
        LeftFlipper.RotateToStart:FlipLOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAtVol SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper, FluVol
		PlaySoundLoopAtVol "buzz1", RightFlipper, FlbVol
        RightFlipper.RotateToEnd:FlipROn = 1
    Else
        PlaySoundAtVol SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper, FldVol
		StopSound "buzz1"
        RightFlipper.RotateToStart:FlipROn = 0
    End If
End Sub

'***********
' GI lights
'***********

Sub GiOn 'Turn on GI lights
    Dim bulb
	PlaySoundAtVol "fx_gion", Bumper3, GirVol
    For each bulb in aGiLights
        bulb.State = 1
    Next
    FlasherOn 1
    If B2SOn Then
        Controller.B2SSetData 50, 1
        Controller.B2SSetData 25, 1
    End If
End Sub

Sub GiOff 'Turn off GI lights
    Dim bulb
	PlaySoundAtVol "fx_gioff", Bumper3, GirVol
    For each bulb in aGiLights
        bulb.State = 0
    Next
    FlasherOn 0
    If B2SOn Then
        Controller.B2SSetData 50, 0
        Controller.B2SSetData 25, 0
    End If
End Sub

'********************************** Tilt

Sub CheckTilt
    If bGameInPlay Then
    Tilt = Tilt + TiltSensitivity
    TiltDecreaseTimer.Enabled = True
    If Tilt > 15 Then
        Tilted = True
		TiltReel.SetValue 1
		fTilt.Visible = 1
		PlaySoundAtVol "tilt", Bumper3, TilVol
        If B2SOn Then
            Controller.B2SSetTilt 1
        End If
        DisableTable True
        ' BallsRemaining(CurrentPlayer) = 0 'player looses the game
        TiltRecoveryTimer.Enabled = True 'wait for all the balls to drain
    End If
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
	Bumper1.HasHitEvent = 0
	Bumper2.HasHitEvent = 0
	Bumper3.HasHitEvent = 0
	Bumper4.HasHitEvent = 0
	Bumper5.HasHitEvent = 0
	LeftSlingShot.Disabled = 1
	RightSlingShot.Disabled = 1
    DOF 101, DOFOff
    DOF 102, DOFOff
	If FlipLOn = 1 Then:SolLFlipper 0:End If
	If FlipROn = 1 Then:SolRFlipper 0:End If
    PostDown
    TiltSeq.Play SeqAllOff
    FlasherOn 0
    If B2SOn Then
        Controller.B2SSetData 50, 0
        Controller.B2SSetData 25, 0
    End If
    Else
	Bumper1.HasHitEvent = 1
	Bumper2.HasHitEvent = 1
	Bumper3.HasHitEvent = 1
	Bumper4.HasHitEvent = 1
	Bumper5.HasHitEvent = 1
	LeftSlingShot.Disabled = 0
	RightSlingShot.Disabled = 0
    TiltSeq.StopPlay
    FlasherOn 1
    If B2SOn Then
        Controller.B2SSetData 50, 1
        Controller.B2SSetData 25, 1
    End If
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
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End If:End Sub"
    End If
End Sub
'****************************************
' Init table for a new game
'****************************************

Sub ResetForNewGame()
    'debug.print "ResetForNewGame"
    Dim i

    bGameInPlay = True
    bBallSaverActive = False
    NewGReset = 1

    StopAttractMode
    DOF 200, DOFOn

    If Electrician_Mode = 1 Then CentoMR.SetValue 0:fCentoM.Visible = 0

    If B2SOn Then
        Controller.B2SSetGameOver 0
        If Electrician_Mode = 1 Then
            Controller.B2SSetData 99, 0 'off
        End If
    End If

    GiOn

    Clear_Match

    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
        Special4Awarded(i) = False
        BallsRemaining(i) = BallsPerGame
    Next
    BonusMultiplier = 1
    Bonus = 0
    fM00.Visible=0:fM10.Visible=0:fM20.Visible=0:fM30.Visible=0:fM40.Visible=0:fM50.Visible=0:fM60.Visible=0:fM70.Visible=0:fM80.Visible=0:fM90.Visible=0

    ' init other variables
    Tilt = 0

    ' init game variables
    Game_Init()

    ' start a music?
    ' first ball
'    vpmtimer.addtimer 2000, "FirstBall '"
	PlaySoundAtVol "Klondike Init", Bumper3, IniVol
    NewGameReset.Enabled = 1
End Sub
'********************************** Reset ball count unit

Sub NewGameReset_Timer
	Select Case Ngr
	Case 1:ReelScoreReset.Enabled = 1:If BallsPerGame > 0 Then:Bpg = 1:UpdateBallInPlay:End If
	Case 2:If BallsPerGame > 1 Then:Bpg = 2:UpdateBallInPlay:End If
	Case 3:If BallsPerGame > 2 Then:Bpg = 3:UpdateBallInPlay:End If
	Case 4:If BallsPerGame > 3 Then:Bpg = 4:UpdateBallInPlay:End If
	Case 5:If BallsPerGame > 4 Then:Bpg = 5:UpdateBallInPlay:End If
	Case 6:If BallsPerGame > 5 Then:Bpg = 6:UpdateBallInPlay:End If
	Case 7:If BallsPerGame > 6 Then:Bpg = 7:UpdateBallInPlay:End If
	Case 8:If BallsPerGame > 7 Then:Bpg = 8:UpdateBallInPlay:End If
	Case 9:If BallsPerGame > 8 Then:Bpg = 9:UpdateBallInPlay:End If

	Case 14:FirstBall:NewGReset = 0:ReelScoreReset.Enabled = 0:NewGameReset.Enabled = 0
	End Select

	Ngr = Ngr + 1
End Sub

'********************************** Reset reels backbox

Sub ReelScoreReset_Timer
	If Dec1.RotX <> 360 And Dec1.RotX <> 0 Then:Dec1.RotX = Dec1.RotX + 6:End If
	If Dec1.RotX = 36 or Dec1.RotX = 72 or Dec1.RotX = 108 or Dec1.RotX = 144 or Dec1.RotX = 180 or Dec1.RotX = 216 or Dec1.RotX = 252 or Dec1.RotX = 288 or Dec1.RotX = 324 Then
	PlaySoundAtVol "cluper", Dec1, RepVol
End If
	If Cen1.RotX <> 360 And Cen1.RotX <> 0 Then:Cen1.RotX = Cen1.RotX + 6:End If
	If Cen1.RotX = 36 or Cen1.RotX = 72 or Cen1.RotX = 108 or Cen1.RotX = 144 or Cen1.RotX = 180 or Cen1.RotX = 216 or Cen1.RotX = 252 or Cen1.RotX = 288 or Cen1.RotX = 324 Then
	PlaySoundAtVol "cluper", Cen1, RepVol
End If
	If Mil1.RotX <> 360 And Mil1.RotX <> 0 Then:Mil1.RotX = Mil1.RotX + 6:End If
	If Mil1.RotX = 36 or Mil1.RotX = 72 or Mil1.RotX = 108 or Mil1.RotX = 144 or Mil1.RotX = 180 or Mil1.RotX = 216 or Mil1.RotX = 252 or Mil1.RotX = 288 or Mil1.RotX = 324 Then
	PlaySoundAtVol "cluper", Mil1, RepVol
End If
	If DMi1.RotX <> 360 And DMi1.RotX <> 0 Then:DMi1.RotX = DMi1.RotX + 6:End If
	If DMi1.RotX = 36 or DMi1.RotX = 72 or DMi1.RotX = 108 or DMi1.RotX = 144 or DMi1.RotX = 180 or DMi1.RotX = 216 or DMi1.RotX = 252 or DMi1.RotX = 288 or DMi1.RotX = 324 Then
	PlaySoundAtVol "cluper", DMi1, RepVol
End If
End Sub

Sub FirstBall
    'debug.print "FirstBall"
    ' reset table for a new ball, rise droptargets ++
    ResetForNewPlayerBall()
    CreateNewBall()
    DOF 250, DOFPulse
    Ngr = 0
    Bpg = 0
    UpdateS10 = 0:UpdateS100 = 0:UpdateS1000 = 0:UpdateS10000 = 0
End Sub

' (Re-)init table for a new ball or player

Sub ResetForNewPlayerBall()
    'debug.print "ResetForNewPlayerBall"
    AddScore 0

    ' reset multiplier to 1x

    ' turn on lights, and variables
    bExtraBallWonThisBall = False
    ResetNewBallLights
    ResetNewBallVariables
End Sub

' Crete new ball

Sub CreateNewBall()
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass
    BallsOnPlayfield = BallsOnPlayfield + 1
    PlaySoundAt SoundFXDOF ("fx_Ballrel", 150, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 60, 13
End Sub

' player lost the ball

Sub EndOfBall()
    'debug.print "EndOfBall"
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0
    ' Lost the first ball, now it cannot accept more players
    bOnTheFirstBall = False

    'No bonus in this table

    ' add bonus if no tilt
    ' tilt system will take care of the next ball

    'If NOT Tilted AND LightShootAgain.State Then
    'If NOT Tilted Then
    '    Select Case BonusMultiplier
    '        Case 1:BonusCountTimer.Interval = 250
    '        Case 2:BonusCountTimer.Interval = 400
    '        Case 3:BonusCountTimer.Interval = 550
    '    End Select
    '    BonusCountTimer.Enabled = 1
    'Else
    vpmtimer.addtimer 1000, "EndOfBall2 '"
'End If
End Sub
Sub BonusCountTimer_Timer
    'debug.print "BonusCount_Timer"
    If Bonus > 0 Then
        Bonus = Bonus -1
        AddScore 1000 * BonusMultiplier
        UpdateBonusLights
    Else
        BonusCountTimer.Enabled = 0
        vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub

Sub UpdateBonusLights
'    Select Case Bonus
'        Case 0:bl1.State = 1:bl2.State = 0:bl2a.State = 0:bl3.State = 0:bl3a.State = 0:bl4.State = 0:bl4a.State = 0:bl5.State = 0:bl5a.State = 0:bl5k.State = 0:bl10k.State = 0:bl15k.State = 0:bl20k.State = 0:SpecialL1.State = 0
'    End Select
End Sub

' After bonus count go to the next step
'
Sub EndOfBall2()
    'debug.print "EndOfBall2"

    Tilted = False
    Tilt = 0
    TiltReel.SetValue 0
    fTilt.Visible = 0
    If B2SOn Then
        Controller.B2SSetTilt 0
    End If
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
        'End If
        End If

        ' extra ball sound?

        ' reset as in a new ball
        ResetForNewPlayerBall()
        CreateNewBall()
    Else ' no extra ball

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1
        UpdateBallInPlay

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

        EndOfGame()
        Reel1Advance
        Reel3Advance
        BallstoPlayR.SetValue 0
        fb1.Visible = 0
        PlaySoundAtVol "BallCount", Screw027, BcuVol

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
    bGameInPlay = False
    bJustStarted = False
    If B2SOn Then
        Controller.B2SSetGameOver 1
        Controller.B2SSetBallInPlay 0
    End If
    ' turn off flippers
	If FlipLOn = 1 Then:SolLFlipper 0:End If
	If FlipROn = 1 Then:SolRFlipper 0:End If
    DOF 250, DOFPulse
    DOF 200, DOFOff

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

' check the highscore
Sub CheckHighscore
    Dim playertops, si, sj, i, stemp, stempplayers
    For i = 1 to 4
        sortscores(i) = 0
        sortplayers(i) = 0
    Next
    playertops = 0
    For i = 1 to PlayersPlayingGame
        sortscores(i) = Score(i)
        sortplayers(i) = i
    Next
    For si = 1 to PlayersPlayingGame
        For sj = 1 to PlayersPlayingGame-1
            If sortscores(sj) > sortscores(sj + 1)Then
                stemp = sortscores(sj + 1)
                stempplayers = sortplayers(sj + 1)
                sortscores(sj + 1) = sortscores(sj)
                sortplayers(sj + 1) = sortplayers(sj)
                sortscores(sj) = stemp
                sortplayers(sj) = stempplayers
            End If
        Next
    Next
    HighScoreTimer.Interval = 100
    HighScoreTimer.Enabled = True
    ScoreChecker = 4
    CheckAllScores = 1
    NewHighScore sortscores(ScoreChecker), sortplayers(ScoreChecker)
End Sub

'******************
'     Match
'******************

Sub Verification_Match()
    PlaySoundAtVol "fx_match", Bumper3, MatVol
    Match = INT(RND(1) * 10) *10 ' random between 0 and 90
    Display_Match
    If(Score(CurrentPlayer)MOD 100) = Match And (SpecialMode = 2 or SpecialMode = 4) Then
        PlaySoundAtVol SoundFXDOF  ("knocker", 127, DOFPulse, DOFKnocker), Screw022, KnoVol
        DOF 227, DOFPulse
        AddCredits 1
    End If
End Sub

Sub Clear_Match()
    MatchR.SetValue 10
    If B2SOn Then
        Controller.B2SSetMatch(0)
    End If
End Sub

Sub Display_Match()
    MatchR.SetValue Match\10

    Select Case (Match\10)
    Case 0:fM00.Visible=1:fM10.Visible=0:fM20.Visible=0:fM30.Visible=0:fM40.Visible=0:fM50.Visible=0:fM60.Visible=0:fM70.Visible=0:fM80.Visible=0:fM90.Visible=0
    Case 1:fM00.Visible=0:fM10.Visible=1:fM20.Visible=0:fM30.Visible=0:fM40.Visible=0:fM50.Visible=0:fM60.Visible=0:fM70.Visible=0:fM80.Visible=0:fM90.Visible=0
    Case 2:fM00.Visible=0:fM10.Visible=0:fM20.Visible=1:fM30.Visible=0:fM40.Visible=0:fM50.Visible=0:fM60.Visible=0:fM70.Visible=0:fM80.Visible=0:fM90.Visible=0
    Case 3:fM00.Visible=0:fM10.Visible=0:fM20.Visible=0:fM30.Visible=1:fM40.Visible=0:fM50.Visible=0:fM60.Visible=0:fM70.Visible=0:fM80.Visible=0:fM90.Visible=0
    Case 4:fM00.Visible=0:fM10.Visible=0:fM20.Visible=0:fM30.Visible=0:fM40.Visible=1:fM50.Visible=0:fM60.Visible=0:fM70.Visible=0:fM80.Visible=0:fM90.Visible=0
    Case 5:fM00.Visible=0:fM10.Visible=0:fM20.Visible=0:fM30.Visible=0:fM40.Visible=0:fM50.Visible=1:fM60.Visible=0:fM70.Visible=0:fM80.Visible=0:fM90.Visible=0
    Case 6:fM00.Visible=0:fM10.Visible=0:fM20.Visible=0:fM30.Visible=0:fM40.Visible=0:fM50.Visible=0:fM60.Visible=1:fM70.Visible=0:fM80.Visible=0:fM90.Visible=0
    Case 7:fM00.Visible=0:fM10.Visible=0:fM20.Visible=0:fM30.Visible=0:fM40.Visible=0:fM50.Visible=0:fM60.Visible=0:fM70.Visible=1:fM80.Visible=0:fM90.Visible=0
    Case 8:fM00.Visible=0:fM10.Visible=0:fM20.Visible=0:fM30.Visible=0:fM40.Visible=0:fM50.Visible=0:fM60.Visible=0:fM70.Visible=0:fM80.Visible=1:fM90.Visible=0
    Case 9:fM00.Visible=0:fM10.Visible=0:fM20.Visible=0:fM30.Visible=0:fM40.Visible=0:fM50.Visible=0:fM60.Visible=0:fM70.Visible=0:fM80.Visible=0:fM90.Visible=1
    Case 10:fM00.Visible=0:fM10.Visible=0:fM20.Visible=0:fM30.Visible=0:fM40.Visible=0:fM50.Visible=0:fM60.Visible=0:fM70.Visible=0:fM80.Visible=0:fM90.Visible=0
    End Select

    If B2SOn Then
        If Match = 0 Then
            Controller.B2SSetMatch 10
        Else
            Controller.B2SSetMatch Match\10
        End If
    End If
End Sub
' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

Sub Drain_Hit()
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1
    PlaySoundAt "fx_drain EM", Drain
    DOF 229, DOFPulse

    'tilted?
    If Tilted Then
        StopEndOfBallMode
        Reel1Advance
        Reel3Advance
    End If

    ' if still playing and not tilted
    If(bGameInPlay = True)AND(Tilted = False)Then

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

Sub AddScore(Points)
    If Tilted Then Exit Sub
    Select Case Points
        Case 10
            PlaySoundAtVol SoundFXDOF ("bell2M", 301, DOFPulse, DOFChimes), Screw025, BdeVol
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore points
            UpdateS10 = UpdateS10 + 1
            UpdateSDec
        Case 100
            PlaySoundAtVol SoundFXDOF ("bell1M", 302, DOFPulse, DOFChimes), Screw024, BceVol
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore points
            UpdateS100 = UpdateS100 + 1
            UpdateSCen
        Case 1000
         If Chimesep = 0 Then
            PlaySoundAtVol SoundFXDOF ("bell1M", 302, DOFPulse, DOFChimes), Screw024, BceVol
            Else
            PlaySoundAtVol SoundFXDOF ("bell4M", 303, DOFPulse, DOFChimes), Screw024, BmiVol
            End If
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore points
            UpdateS1000 = UpdateS1000 + 1
            UpdateSMig
        Case 10000
         If Chimesep = 0 Then
            PlaySoundAtVol SoundFXDOF ("bell1M", 302, DOFPulse, DOFChimes), Screw024, BceVol
            Else
            PlaySoundAtVol SoundFXDOF ("bell4M", 303, DOFPulse, DOFChimes), Screw024, BmiVol
            End If
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore points
            UpdateS10000 = UpdateS10000 + 1
            UpdateSDeM
        Case 500
            Add100 = Add100 + Points \ 100
            AddScore100Timer.Enabled = True
        Case 3000, 4000, 5000
            Add1000 = Add1000 + Points \ 1000
            AddScore1000Timer.Enabled = True
        Case 20000, 30000
            Add10000 = Add10000 + Points \ 10000
            AddScore10000Timer.Enabled = True
    End Select
    CheckHighScoreSpecials
End Sub

Dim UpdateS10, UpdateS100, UpdateS1000, UpdateS10000

'********************************** ReelScore10

Sub UpdateSDec
 If UpdateS10 > 9 Then
	UpdateS10 = 0
	UpdateS100 = UpdateS100 + 1
	UpdateSCen
End If
	ReelScore10Timer.Enabled = 1
	PlaySoundAtVol "cluper", Dec1, RepVol
End Sub

Sub ReelScore10Timer_Timer
	Dec(CurrentPlayer).RotX = (Dec(CurrentPlayer).RotX + 6) MOD 360
 If Dec(CurrentPlayer).RotX = UpdateS10 * 36 Then
	ReelScore10Timer.Enabled = 0
End If
End Sub

'********************************** ReelScore100

Sub UpdateSCen
 If UpdateS100 > 9 Then
	UpdateS100 = 0
	UpdateS1000 = UpdateS1000 + 1
	UpdateSMig
End If
	ReelScore100Timer.Enabled = 1
	PlaySoundAtVol "cluper", Cen1, RepVol
End Sub

Sub ReelScore100Timer_Timer
	Cen(CurrentPlayer).RotX = (Cen(CurrentPlayer).RotX + 6) MOD 360
 If Cen(CurrentPlayer).RotX = UpdateS100 * 36 Then
	ReelScore100Timer.Enabled = 0
End If
End Sub

'********************************** ReelScore1000

Sub UpdateSMig
 If UpdateS1000 > 9 Then
	UpdateS1000 = 0
	UpdateS10000 = UpdateS10000 + 1
	UpdateSDeM
End If
	ReelScore1000Timer.Enabled = 1
	PlaySoundAtVol "cluper", Mil1, RepVol
End Sub

Sub ReelScore1000Timer_Timer
	Mil(CurrentPlayer).RotX = (Mil(CurrentPlayer).RotX + 6) MOD 360
 If Mil(CurrentPlayer).RotX = UpdateS1000 * 36 Then
	ReelScore1000Timer.Enabled = 0
End If
End Sub

'********************************** ReelScore10000

Sub UpdateSDeM
 If UpdateS10000 > 9 Then
	UpdateS10000 = 0
End If
	ReelScore10000Timer.Enabled = 1
	PlaySoundAtVol "cluper", DMi1, RepVol
End Sub

Sub ReelScore10000Timer_Timer
	DMi(CurrentPlayer).RotX = (DMi(CurrentPlayer).RotX + 6) MOD 360
 If DMi(CurrentPlayer).RotX = UpdateS10000 * 36 Then
	ReelScore10000Timer.Enabled = 0
End If
End Sub

'********************************** Check for higher score and specials

Sub CheckHighScoreSpecials
    If Score(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
        Select Case SpecialMode
            Case 1:AwardAddaBall  'add-a-ball
            Case 2:AwardExtraBall 'replay
            Case 3:               ' nothing on Novelty mode
		   Case 4:AwardSpecial
        End Select
        Special1Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
        Select Case SpecialMode
            Case 1:AwardAddaBall  'add-a-ball
            Case 2:AwardExtraBall 'replay
            Case 3:               ' nothing on Novelty mode
		   Case 4:AwardSpecial
        End Select
        Special2Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
        Select Case SpecialMode
            Case 1:AwardAddaBall  'add-a-ball
            Case 2:AwardExtraBall 'replay
            Case 3:               ' nothing on Novelty mode
		   Case 4:AwardSpecial
        End Select
        Special3Awarded(CurrentPlayer) = True
    End If
End Sub'************************************
'       Score sound Timers
'************************************

Sub AddScore10Timer_Timer()
    If Add10 > 0 Then
        AddScore 10
        Add10 = Add10 - 1
    Else
        AddScore10Timer.Enabled = False
    End If
End Sub

Sub AddScore100Timer_Timer()
    If Add100 > 0 Then
        AddScore 100
        Add100 = Add100 - 1
    Else
        AddScore100Timer.Enabled = False
    End If
End Sub

Sub AddScore1000Timer_Timer()
    If Add1000 > 0 Then
        AddScore 1000
        Add1000 = Add1000 - 1
    Else
        AddScore1000Timer.Enabled = False
    End If
End Sub

Sub AddScore10000Timer_Timer()
    If Add10000 > 0 Then
        AddScore 10000
        Add10000 = Add10000 - 1
    Else
        AddScore10000Timer.Enabled = False
    End If
End Sub

'*******************
'     BONUS
'*******************

Sub AddBonus(bonuspoints)
    If(Tilted = False)Then
        Bonus = Bonus + bonuspoints
'        PlaySound SoundFXDOF ("Bell2" , 302, DOFPulse, DOFChimes)
        If Bonus > 20 Then
            Bonus = 20
        End If
        UpdateBonusLights
    End If
End Sub

Sub AddBonusMultiplier(multi)
    If(Tilted = False)Then
        BonusMultiplier = BonusMultiplier + multi
        If BonusMultiplier > MaxMultiplier Then
            BonusMultiplier = MaxMultiplier
        End If
        UpdateMultiplierLights
    End If
End Sub

Sub UpdateMultiplierLights
    Select Case BonusMultiplier
    'Case 1:li2.State = 1:li3.State = 0:li4.State = 0
    'Case 2:li2.State = 0:li3.State = 1:li4.State = 0
    'Case 3:li2.State = 0:li3.State = 0:li4.State = 1
    End Select
End Sub

'**********************************
'        Score EM reels
'**********************************

Sub UpdateScore(playerpoints)
    Select Case CurrentPlayer
        Case 1:ScoreReel1.Addvalue playerpoints
    ' Case 2:ScoreReel2.Addvalue playerpoints
    ' Case 3:ScoreReel3.Addvalue playerpoints
    ' Case 4:ScoreReel4.Addvalue playerpoints
    End Select
    If Electrician_Mode = 1 AND Score(CurrentPlayer) > 99999 Then CentoMR.SetValue 1:fCentoM.Visible = 1
    If B2SOn Then
        Controller.B2SSetScorePlayer CurrentPlayer, Score(CurrentPlayer)
        If Electrician_Mode = 1 AND Score(CurrentPlayer) > 99999 Then
            Controller.B2SSetData 99, 1
        End If
    End If
End Sub

Sub ResetScores
    ScoreReel1.ResetToZero
    If Electrician_Mode = 1 Then CentoMR.SetValue 0:fCentoM.Visible = 0
    If B2SOn Then
        Controller.B2SSetScorePlayer1 0
        Controller.B2SSetScoreRolloverPlayer1 0
        If Electrician_Mode = 1 Then
            Controller.B2SSetData 99, 0 'off
        End If
    End If
End Sub

'********************************** Credits

Sub AddCredits(value)
    Credits = Credits + value
    PlaySoundAtVol "cluper", CreditR, RepVol
    UpdateCredits
End Sub

Sub UpdateCredits
    If Credits > 0 Then
        CreditLight.State = 1
        DOF 126, DOFOn
    Else
        CreditLight.State = 0
        DOF 126, DOFOff
    End If
    CreditsReel.SetValue Credits
    CreditR.RotX = Credits * 7.2
    If B2SOn Then
        Controller.B2SSetCredits Credits
    End If
End Sub

'********************************** Ball In Play

Sub UpdateBallInPlay
    If NewGReset = 0 Then
    BallsPG = BallsPerGame - Balls + 1
    ElseIf NewGReset = 1 Then
    BallsPG = Bpg
End If
    BallstoPlayR.SetValue(BallsPG)
    If B2SOn Then
        Controller.B2SSetBallInPlay(BallsPG)
    End If

    Select Case BallsPG
    Case 0:fb1.Visible=0:fb2.Visible=0:fb3.Visible=0:fb4.Visible=0:fb5.Visible=0:fb6.Visible=0:fb7.Visible=0:fb8.Visible=0:fb9.Visible=0:PlaySoundAtVol "BallCount", Screw027, BcuVol
    Case 1:fb1.Visible=1:fb2.Visible=0:fb3.Visible=0:fb4.Visible=0:fb5.Visible=0:fb6.Visible=0:fb7.Visible=0:fb8.Visible=0:fb9.Visible=0:PlaySoundAtVol "BallCount", Screw027, BcuVol
    Case 2:fb1.Visible=0:fb2.Visible=1:fb3.Visible=0:fb4.Visible=0:fb5.Visible=0:fb6.Visible=0:fb7.Visible=0:fb8.Visible=0:fb9.Visible=0:PlaySoundAtVol "BallCount", Screw027, BcuVol
    Case 3:fb1.Visible=0:fb2.Visible=0:fb3.Visible=1:fb4.Visible=0:fb5.Visible=0:fb6.Visible=0:fb7.Visible=0:fb8.Visible=0:fb9.Visible=0:PlaySoundAtVol "BallCount", Screw027, BcuVol
    Case 4:fb1.Visible=0:fb2.Visible=0:fb3.Visible=0:fb4.Visible=1:fb5.Visible=0:fb6.Visible=0:fb7.Visible=0:fb8.Visible=0:fb9.Visible=0:PlaySoundAtVol "BallCount", Screw027, BcuVol
    Case 5:fb1.Visible=0:fb2.Visible=0:fb3.Visible=0:fb4.Visible=0:fb5.Visible=1:fb6.Visible=0:fb7.Visible=0:fb8.Visible=0:fb9.Visible=0:PlaySoundAtVol "BallCount", Screw027, BcuVol
    Case 6:fb1.Visible=0:fb2.Visible=0:fb3.Visible=0:fb4.Visible=0:fb5.Visible=0:fb6.Visible=1:fb7.Visible=0:fb8.Visible=0:fb9.Visible=0:PlaySoundAtVol "BallCount", Screw027, BcuVol
    Case 7:fb1.Visible=0:fb2.Visible=0:fb3.Visible=0:fb4.Visible=0:fb5.Visible=0:fb6.Visible=0:fb7.Visible=1:fb8.Visible=0:fb9.Visible=0:PlaySoundAtVol "BallCount", Screw027, BcuVol
    Case 8:fb1.Visible=0:fb2.Visible=0:fb3.Visible=0:fb4.Visible=0:fb5.Visible=0:fb6.Visible=0:fb7.Visible=0:fb8.Visible=1:fb9.Visible=0:PlaySoundAtVol "BallCount", Screw027, BcuVol
    Case 9:fb1.Visible=0:fb2.Visible=0:fb3.Visible=0:fb4.Visible=0:fb5.Visible=0:fb6.Visible=0:fb7.Visible=0:fb8.Visible=0:fb9.Visible=1:PlaySoundAtVol "BallCount", Screw027, BcuVol
    End Select

End Sub

'*************************
'        Specials
'*************************

Sub AwardAddaBall() 'Add-a-ball
    If BallsRemaining(CurrentPlayer) < 9 Then
        PlaySoundAtVol SoundFXDOF  ("knocker", 127, DOFPulse, DOFKnocker), Screw022, KnoVol
        DOF 227, DOFPulse
        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) + 1
        UpdateBallInPlay
    Else
'        Score(CurrentPlayer) = Score(CurrentPlayer) + 50000
'        UpdateScore 50000
    End If
End Sub

Sub AwardExtraBall() 'Replay
    PlaySoundAtVol SoundFXDOF  ("knocker", 127, DOFPulse, DOFKnocker), Screw022, KnoVol
    DOF 227, DOFPulse
    AddCredits 1
End Sub

Sub AwardSpecial() 'Legacy
    PlaySoundAtVol SoundFXDOF  ("knocker", 127, DOFPulse, DOFKnocker), Screw022, KnoVol
    DOF 227, DOFPulse
    AddCredits 1
End Sub

Sub AwardSpecialMode()
    Select Case SpecialMode
        Case 1:AwardAddaBall                 'Add-a-ball
        Case 2:AwardExtraBall                'Replay
        Case 3:'AddScore NoveltyScore         'Novelty
        Case 4:AwardSpecial                  'Legacy
    End Select
End Sub

'In "Add a ball" position, hi scores and reel values will advance ball count unit 1, 2 or 3 steps as indicated on score card.
'No. match adj should be off.

'In "Replay" position, hi scores and no. match will advance replay unit.
'A special will score 1, 2 o 3 replays.
'Other reel values, as indicated on score card, will advance ball count unit 1 or 2 steps.

'In "Novelty" play, reel values will advance 10.000 point drum unit 1, 2 or 3 times, as indicated on score card.
'No. match adj should be off.

' ********************************
'        Attract Mode
' ********************************
' use the"Blink Pattern" of each light

Sub StartAttractMode()
    Dim x
    bAttractMode = True
    For each x in aLights
        x.blinkInterval = 300
        x.State = 2
    Next
    GameOverReel.SetValue 1
    fGameOver.Visible = 1
    'BallstoPlayR.SetValue 0
End Sub

Sub StopAttractMode()
    Dim x
    bAttractMode = False
    TurnOffPlayfieldLights
    ResetScores
    GameOverReel.SetValue 0
    fGameOver.Visible = 0
End Sub

'*********************************
'    Load / Save / Highscore
'*********************************

Sub Loadhs
    ' Based on Black's Highscore routines
    Dim FileObj
    Dim ScoreFile, TextStr
    Dim temp1
    Dim temp2
    Dim temp3
    Dim temp4
    Dim temp5
    Dim temp6
    Dim temp7
    Dim temp8
    Dim temp9
    Dim temp10
    Dim temp11
    Dim temp12
    Dim temp13
    Dim temp14
    Dim temp15
    Dim temp16
    Dim temp17

    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory)Then
        Credits = 0
        Exit Sub
    End If
    If Not FileObj.FileExists(UserDirectory & TableName& ".txt")Then
        Credits = 0
        Exit Sub
    End If
    Set ScoreFile = FileObj.GetFile(UserDirectory & TableName& ".txt")
    Set TextStr = ScoreFile.OpenAsTextStream(1, 0)
    If(TextStr.AtEndOfStream = True)Then
        Exit Sub
    End If
    temp1 = TextStr.ReadLine
    temp2 = TextStr.ReadLine
    temp3 = TextStr.ReadLine
    temp4 = TextStr.ReadLine
    temp5 = TextStr.ReadLine
    temp6 = TextStr.ReadLine
    temp7 = TextStr.ReadLine

    HighScore = cdbl(temp1)
    If HighScore < 1 Then
        temp8 = textstr.readline
        temp9 = textstr.readline
        temp10 = textstr.readline
        temp11 = textstr.readline
        temp12 = textstr.readline
        temp13 = textstr.readline
        temp14 = textstr.readline
        temp15 = textstr.readline
        temp16 = textstr.readline
        temp17 = textstr.readline
    End If
    TextStr.Close
    Credits = cdbl(temp2)
    Score(1) = cdbl(temp3)
    Match = cdbl(temp4)
    rv(1) = cdbl(temp5)
    rv(2) = cdbl(temp6)
    rv(3) = cdbl(temp7)

    If HighScore < 1 Then
        HSScore(1) = int(temp8)
        HSScore(2) = int(temp9)
        HSScore(3) = int(temp10)
        HSScore(4) = int(temp11)
        HSScore(5) = int(temp12)
        HSName(1) = temp13
        HSName(2) = temp14
        HSName(3) = temp15
        HSName(4) = temp16
        HSName(5) = temp17
    End If
    Set ScoreFile = Nothing
    Set FileObj = Nothing
    SortHighscore 'added to fix a previous error
End Sub

Sub Savehs
    ' Based on Black's Highscore routines
    Dim FileObj
    Dim ScoreFile
    Dim xx
    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory)Then
        Exit Sub
    End If
    Set ScoreFile = FileObj.CreateTextFile(UserDirectory & TableName& ".txt", True)
    ScoreFile.WriteLine 0
    ScoreFile.WriteLine Credits
    ScoreFile.WriteLine Score(1)
    ScoreFile.WriteLine Match
    ScoreFile.WriteLine rv(1)
    ScoreFile.WriteLine rv(2)
    ScoreFile.WriteLine rv(3)
    For xx = 1 to 5
        scorefile.writeline HSScore(xx)
    Next
    For xx = 1 to 5
        scorefile.writeline HSName(xx)
    Next
    ScoreFile.Close
    Set ScoreFile = Nothing
    Set FileObj = Nothing
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 1 to 5
        For j = 1 to 4
            If HSScore(j) < HSScore(j + 1)Then
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

'********************************** Reels Backbox

Dim Score100K, Score10K, ScoreK, Score100, Score10		'Define 5 different score values for each reel to use
Sub UpdateBReels
	Score100K=Int(Score(CurrentPlayer)/100000)																'Calculate the value for the 100000's digit
	Score10K=Int((Score(CurrentPlayer)-(Score100K*100000))/10000)											'Calculate the value for the 10000's digit
	ScoreK=Int((Score(CurrentPlayer)-(Score100K*100000)-(Score10K*10000))/1000)								'Calculate the value for the 1000's digit
	Score100=Int((Score(CurrentPlayer)-(Score100K*100000)-(Score10K*10000)-(ScoreK*1000))/100)				'Calculate the value for the 100's digit
	Score10=Int((Score(CurrentPlayer)-(Score100K*100000)-(Score10K*10000)-(ScoreK*1000)-(Score100*100))/10)	'Calculate the value for the 10's digit
	DMi(CurrentPlayer).RotX = Score10K * 36
	Mil(CurrentPlayer).RotX = ScoreK * 36
	Cen(CurrentPlayer).RotX = Score100 * 36
	Dec(CurrentPlayer).RotX = Score10 * 36
End Sub
'********************************** Flashers

Dim FlashLevel, Direction

Sub FlasherOn(Enabled)
    If Enabled Then
    Direction = 1
Else
    Direction = 0
End If
    FlasherTimer.Enabled = 1
End Sub

Sub FlasherTimer_Timer
    If Direction = 1 Then
    FlashLevel = FlashLevel + 0.1
ElseIf Direction = 0 Then
    FlashLevel = FlashLevel - 0.1
End If
    If FlashLevel <= 0 Then FlashLevel = 0:FlasherTimer.Enabled = 0
    If FlashLevel >= 1 Then FlashLevel = 1:FlasherTimer.Enabled = 0
    UpdateFlashers
End Sub

Sub UpdateFlashers
	fgi1.IntensityScale = FlashLevel * FlIntensity
	fgi2.IntensityScale = FlashLevel * FlIntensity
	fgi3.IntensityScale = FlashLevel * FlIntensity
	fbgi001.IntensityScale = FlashLevel * FlIntensity
	fbgi002.IntensityScale = FlashLevel * FlIntensity
	fbgi003.IntensityScale = FlashLevel * FlIntensity
	fbgi004.IntensityScale = FlashLevel * FlIntensity
	fbgi005.IntensityScale = FlashLevel * FlIntensity
	fbgi006.IntensityScale = FlashLevel * FlIntensity
	fbgi007.IntensityScale = FlashLevel * FlIntensity
	fbgi008.IntensityScale = FlashLevel * FlIntensity
	fbgi009.IntensityScale = FlashLevel * FlIntensity
	fbgi010.IntensityScale = FlashLevel * FlIntensity
	fbgi011.IntensityScale = FlashLevel * FlIntensity
	fbgi012.IntensityScale = FlashLevel * FlIntensity
	fbgi013.IntensityScale = FlashLevel * FlIntensity
	fbgi014.IntensityScale = FlashLevel * FlIntensity
	fbgi015.IntensityScale = FlashLevel * FlIntensity
	fbgi016.IntensityScale = FlashLevel * FlIntensity
	fbgi017.IntensityScale = FlashLevel * FlIntensity
	fbgi018.IntensityScale = FlashLevel * FlIntensity
	fbgi019.IntensityScale = FlashLevel * FlIntensity
	fM00.IntensityScale = FlashLevel * FlIntensity
	fM10.IntensityScale = FlashLevel * FlIntensity
	fM20.IntensityScale = FlashLevel * FlIntensity
	fM30.IntensityScale = FlashLevel * FlIntensity
	fM40.IntensityScale = FlashLevel * FlIntensity
	fM50.IntensityScale = FlashLevel * FlIntensity
	fM60.IntensityScale = FlashLevel * FlIntensity
	fM70.IntensityScale = FlashLevel * FlIntensity
	fM80.IntensityScale = FlashLevel * FlIntensity
	fM90.IntensityScale = FlashLevel * FlIntensity
	fCentoM.IntensityScale = FlashLevel * FlIntensity
	fb1.IntensityScale = FlashLevel * FlIntensity
	fb2.IntensityScale = FlashLevel * FlIntensity
	fb3.IntensityScale = FlashLevel * FlIntensity
	fb4.IntensityScale = FlashLevel * FlIntensity
	fb5.IntensityScale = FlashLevel * FlIntensity
	fb6.IntensityScale = FlashLevel * FlIntensity
	fb7.IntensityScale = FlashLevel * FlIntensity
	fb8.IntensityScale = FlashLevel * FlIntensity
	fb9.IntensityScale = FlashLevel * FlIntensity
	fGameOver.IntensityScale = FlashLevel * FlIntensity
	fTilt.IntensityScale = FlIntensity
End Sub

'***********************************************************************
' *********************************************************************
'  *********     G A M E  C O D E  S T A R T S  H E R E      *********
' *********************************************************************
'***********************************************************************

Sub VPObjects_Init 'init objects
    Post.IsDropped = True
End Sub

Sub Game_Init() 'called at the start of a new game
    'Start music?
    'Init variables?
    'Start or init timers
    'Init lights?
End Sub

Sub StopEndOfBallMode() 'called when the last ball is drained
	PostDown
	BulbB1.State = 0
	BulbB2.State = 0
	BulbB3.State = 0
	BulbB4.State = 0
	BulbB5.State = 0
End Sub

Sub ResetNewBallVariables() 'init variables new ball/player
End Sub

Sub ResetNewBallLights()    'init lights for new ball/player
	BulbB1.State = 0
	BulbB2.State = 0
	BulbB3.State = 0
	BulbB4.State = 0
	BulbB5.State = 0
End Sub

Sub TurnOffPlayfieldLights()
'    Dim a
'    For each a in aLights
'        a.State = 0
'    Next
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

'********************************** Slingshots
Dim LStep, Rstep

Sub LeftSlingShot_Slingshot
	If Tilted = False Then
	PlaySoundAtVol SoundFXDOF ("fx_slingshot", 103, DOFPulse, DOFcontactors), SxEmKickerT1, SliVol
	LeftSling.Visible = 0
	LeftSling1.Visible = 1
	SxEmKickerT1.TransX = -20
	LStep = 0
	Me.TimerEnabled = 1
	Addscore 10
	End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 2:LeftSling1.Visible = 0:LeftSling2.Visible = 1:SxEmKickerT1.TransX = -10
        Case 3:LeftSling2.Visible = 0:LeftSling.Visible = 1:SxEmKickerT1.TransX = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	If Tilted = False Then
	PlaySoundAtVol SoundFXDOF ("fx_slingshot", 104, DOFPulse, DOFcontactors), DxEmKickerT1, SliVol
	RightSling.Visible = 0
	RightSling1.Visible = 1
	DxEmKickerT1.TransX = -20
	RStep = 0
	Me.TimerEnabled = 1
	Addscore 10
	End If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 2:RightSling1.Visible = 0:RightSling2.Visible = 1:DxEmKickerT1.TransX = -10
        Case 3:RightSling2.Visible = 0:RightSling.Visible = 1:DxEmKickerT1.TransX = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub
'********************************** Contacts

Sub LeftmidSw_Hit
	Addscore 10
End Sub

Sub RightmidSw_Hit
	Addscore 10
End Sub

'********************************** Bumpers
Sub Bumper1_Hit
	PlaySoundAtVol SoundFXDOF ("jet2", 205 , DOFPulse, DOFContactors), Bumper1, BumVol    AddScore 10 + 90 * BulbB1.StateEnd SubSub Bumper2_Hit
    PlaySoundAtVol SoundFXDOF ("jet2", 206 , DOFPulse, DOFContactors), Bumper2, BumVol
    AddScore 10 + 90 * BulbB2.State
End Sub      
Sub Bumper3_Hit
    PlaySoundAtVol SoundFXDOF ("jet2", 207 , DOFPulse, DOFContactors), Bumper3, BumVol
    AddScore 100 + 900 * BulbB3.State
End Sub
Sub Bumper4_Hit
    PlaySoundAtVol SoundFXDOF ("jet2", 208 , DOFPulse, DOFContactors), Bumper4, BumVol
    AddScore 10 + 90 * BulbB4.State
End SubSub Bumper5_Hit
    PlaySoundAtVol SoundFXDOF ("jet2", 209 , DOFPulse, DOFContactors), Bumper5, BumVol
    AddScore 10 + 90 * BulbB5.State
End Sub
'********************************** Top Targets
Sub TargetLT_Hit
	PlaySoundAtBallVol SoundFXDOF ("target", 116, DOFPulse, DOFTargets), TarVol
	DOF 230, DOFPulse
    If Tilted = False Then    Reel1Advance    Addscore 10
End IfEnd SubSub TargetCT_Hit
	PlaySoundAtBallVol SoundFXDOF ("target", 117, DOFPulse, DOFTargets), TarVol
	DOF 231, DOFPulse
    If Tilted = False Then    Reel2Advance    Addscore 10
End IfEnd Sub        Sub TargetRT_Hit
	PlaySoundAtBallVol SoundFXDOF ("target", 118, DOFPulse, DOFTargets), TarVol
	DOF 232, DOFPulse
    If Tilted = False Then    Reel3Advance    Addscore 10
End IfEnd Sub
'********************************** Mid Targets

Sub TargetLM_Hit
	PlaySoundAtBallVol SoundFXDOF ("target", 119, DOFPulse, DOFTargets), TarVol
	DOF 233, DOFPulse
    If Tilted = False Then
    Reel1Advance
    Addscore 10
End If
End Sub    

Sub TargetRM_Hit
	PlaySoundAtBallVol SoundFXDOF ("target", 120, DOFPulse, DOFTargets), TarVol
	DOF 234, DOFPulse
    If Tilted = False Then
    Reel3Advance
    Addscore 10
End If
End Sub 

'********************************** OutLanes Rollovers

Sub TriggerLO_Hit
	PlaySoundAtVol "sensor", TriggerLO, SwiVol
    ScoresReelValue
    DOF 210, DOFPulse 
End Sub

Sub TriggerRO_Hit
	PlaySoundAtVol "sensor", TriggerRO, SwiVol
    ScoresReelValue
    DOF 211, DOFPulse 
End Sub        

'********************************** Down Post Rollover

Sub TriggerDP_Hit
	PlaySoundAtVol "sensor", TriggerDP, SwiVol
    PostDown
End Sub

'********************************** Mid Rollovers

Sub TriggerLM_Hit
	PlaySoundAtVol "sensor", TriggerLM, SwiVol
    If Tilted = False Then
    Reel2Advance
    PostUp
    Addscore 1000
    DOF 212, DOFPulse
End If
End Sub

Sub TriggerRM_Hit
	PlaySoundAtVol "sensor", TriggerRM, SwiVol
    If Tilted = False Then
    Reel2Advance
    PostUp
    Addscore 1000
    DOF 213, DOFPulse
End If
End Sub  

'********************************** Top Rollovers
Sub TriggerLT_Hit
	PlaySoundAtVol "sensor", TriggerLT, SwiVol
    ScoresReelValue
    DOF 214, DOFPulseEnd SubSub TriggerRT_Hit
	PlaySoundAtVol "sensor", TriggerRT, SwiVol
    ScoresReelValue
    DOF 215, DOFPulseEnd SubSub TriggerLTB_Hit
	PlaySoundAtVol "sensor", TriggerLTB, SwiVol
    If Tilted = False Then    Reel1Advance    Addscore 10
    DOF 216, DOFPulse
End IfEnd SubSub TriggerCTB_Hit
	PlaySoundAtVol "sensor", TriggerCTB, SwiVol
    If Tilted = False Then    Reel2Advance    Addscore 10
    DOF 217, DOFPulse
End IfEnd SubSub TriggerRTB_Hit
	PlaySoundAtVol "sensor", TriggerRTB, SwiVol
    If Tilted = False Then    Reel3Advance    Addscore 10
    DOF 218, DOFPulse
End IfEnd Sub
'********************************** Kickers
Dim CkActivated

Sub TriggerCk_Hit:Me.TimerEnabled = 1:End Sub

Sub TriggerCk_Timer
 If CkActivated = 1 Then
	KickerCe.Enabled = 1
	Me.TimerEnabled = 0
End If
End Sub

Sub TriggerCk_UnHit:Me.TimerEnabled = 0:End Sub

Sub KickerCe_Hit
    ScoresReelValue
    PlaySoundAtVol "fx_kicker_enter1", KickerCe, KicVol
    vpmtimer.addtimer 800, "KickerCeEj '"
End Sub

Sub KickerCeEj()
	KickerCe.Kick (Int(Rnd(1)*3)+168),14,0.7:CkEjectArm.RotY = 40
	PlaySoundAtVol SoundFXDOF ("fx_kicker", 122, DOFPulse, DOFContactors), KickerCe, KieVol
	vpmtimer.addtimer 50, "KickerCe.TimerEnabled = 1 '"
    DOF 228, DOFPulse
End Sub

Sub KickerCe_Timer
	CkEjectArm.RotY = CkEjectArm.RotY - 2
 If CkEjectArm.RotY = 20 Then Me.TimerEnabled = 0
End Sub
Sub KickerSx_Hit     If Tilted = False Then    ScoresReelValue    BulbB1.State = 1    BulbB5.State = 1
    If BulbB1.State = 1 And BulbB2.State = 1 Then:BulbB3.State = 1:End If    End If
    PlaySoundAtVol "fx_kicker_enter1", KickerSx, KicVol    vpmtimer.addtimer 800, "KickerSxEj '"End Sub

Sub KickerSxEj()
	KickerSx.Kick (Int(Rnd(1)*3)+174),13,0.8:LkEjectArm.RotY = 40
	PlaySoundAtVol SoundFXDOF ("fx_kicker", 120, DOFPulse, DOFContactors), KickerSx, KieVol
	vpmtimer.addtimer 50, "KickerSx.TimerEnabled = 1 '"
    DOF 228, DOFPulse
End Sub
Sub KickerSx_Timer
	LkEjectArm.RotY = LkEjectArm.RotY - 2
 If LkEjectArm.RotY = 20 Then Me.TimerEnabled = 0
End Sub

Sub KickerDx_Hit
    If Tilted = False Then
    ScoresReelValue
    BulbB2.State = 1
    BulbB4.State = 1
    If BulbB1.State = 1 And BulbB2.State = 1 Then:BulbB3.State = 1:End If
    End If
    PlaySoundAtVol "fx_kicker_enter1", KickerDx, KicVol
    vpmtimer.addtimer 800, "KickerDxEj '"
End Sub

Sub KickerDxEj()
	KickerDx.Kick (Int(Rnd(1)*3)+185),13,0.8:RkEjectArm.RotY = 40
	PlaySoundAtVol SoundFXDOF ("fx_kicker", 121, DOFPulse, DOFContactors), KickerDx, KieVol
	vpmtimer.addtimer 50, "KickerDx.TimerEnabled = 1 '"
    DOF 228, DOFPulse
End Sub

Sub KickerDx_Timer
	RkEjectArm.RotY = RkEjectArm.RotY - 2
 If RkEjectArm.RotY = 20 Then Me.TimerEnabled = 0
End Sub

'********************************** Gate

Sub Gate_Hit():PlaySoundAtBallVol "Gate5", GatVol:End Sub

'********************************** Post up and down

Sub PostUp
	If Post.Isdropped = True Then
	PlaySoundAtVol "fx_solenoidon2", BSPost, PouVol
	Post.Isdropped = False
	BSPost.TransZ = 28
	LightPost.State = 1
	LightPost.BulbHaloHeight = 27
End If
End Sub

Sub PostDown
	If Post.Isdropped = False Then
	PlaySoundAtVol "fx_solenoidoff2", BSPost, PodVol
	Post.Isdropped = True
	LightPost.State = 0
	LightPost.BulbHaloHeight = 0
	BSPost.TransZ = 0
End If
End Sub

'********************************** Reels symbols

Sub Reel1Advance
	PlaySoundAtVol "cluper", ReelLeft, ResVol
	rv(1) = (rv(1)+1) MOD 10
	ReelSxTimer.Enabled = 1
	CheckReels
End Sub

Sub ReelSxTimer_Timer
	ReelLeft.RotX = ReelLeft.RotX - 6
 If ReelLeft.RotX = -360 Then
	ReelLeft.RotX = 0
End If
 If ReelLeft.RotX = -rv(1) * 36 Then
	ReelSxTimer.Enabled = 0
End If
End Sub

Sub Reel2Advance
	PlaySoundAtVol "cluper", ReelCenter, ResVol
	rv(2) = (rv(2)+1) MOD 10
	ReelCeTimer.Enabled = 1
	CheckReels
End Sub

Sub ReelCeTimer_Timer
	ReelCenter.RotX = ReelCenter.RotX - 6
 If ReelCenter.RotX = -360 Then
	ReelCenter.RotX = 0
End If
 If ReelCenter.RotX = -rv(2) * 36 Then
	ReelCeTimer.Enabled = 0
End If
End Sub

Sub Reel3Advance
	PlaySoundAtVol "cluper", ReelRight, ResVol
	rv(3) = (rv(3)+1) MOD 10
	ReelDxTimer.Enabled = 1
	CheckReels
End Sub

Sub ReelDxTimer_Timer
	ReelRight.RotX = ReelRight.RotX - 6
 If ReelRight.RotX = -360 Then
	ReelRight.RotX = 0
End If
 If ReelRight.RotX = -rv(3) * 36 Then
	ReelDxTimer.Enabled = 0
End If
End Sub

'********************************** Check Reels

'Lanterna 0  Padella  0
'Cappello 1  Lanterna 1
'Padella  2  Cappello 2
'Cappello 3  Padella  3
'Carro    4  Cappello 4
'Lanterna 5  Lanterna 5
'Cappello 6  Cappello 6
'Padella  7  Padella  7
'Cappello 8  Cappello 8
'Carro    9  Carro    9

Dim sv, sv1, eb, eb1
Sub CheckReels

'********************************** Carro 5000/30000 Special
	If (rv(1)=4 or rv(1)=9) And (rv(2)=4 or rv(2)=9) And rv(3)=9 Then	'49 49 9
    rvl1.State = 0:rvl2.State = 0:rvl3.State = 0:rvl4.State = 0:rvl5.State = 1
    sv=1

'********************************** Lanterna 5000/20000 + 2 Extra balls
    ElseIf (rv(1)=0 or rv(1)=5) And (rv(2)=0 or rv(2)=5) And (rv(3)=1 or rv(3)=5 or rv(3)=9) Then	'05 05 159
    rvl1.State = 0:rvl2.State = 0:rvl3.State = 0:rvl4.State = 1:rvl5.State = 0
    eb=2

'********************************** Padella 4000/14000 + 1 Extra ball
    ElseIf (rv(1)=2 or rv(1)=7) And (rv(2)=2 or rv(2)=7) And (rv(3)=0 or rv(3)=3 or rv(3)=7 or rv(3)=9) Then	'27 27 0379
    rvl1.State = 0:rvl2.State = 0:rvl3.State = 1:rvl4.State = 0:rvl5.State = 0
    eb=1

'********************************** Cappello 3000
    ElseIf (rv(1)=1 or rv(1)=3 or rv(1)=6 or rv(1)=8) And (rv(2)=1 or rv(2)=3 or rv(2)=6 or rv(2)=8) And (rv(3)=2 or rv(3)=4 or rv(3)=6 or rv(3)=8 or rv(3)=9) Then	'1368 1368 24689
    rvl1.State = 0:rvl2.State = 1:rvl3.State = 0:rvl4.State = 0:rvl5.State = 0
    Else
    rvl1.State = 1:rvl2.State = 0:rvl3.State = 0:rvl4.State = 0:rvl5.State = 0
    End If
End Sub

' 1 = add-a-ball, 2 = replay, 3 = Novelty, 4 Legacy mode
Sub ScoresReelValue
    If Tilted = False Then
    If rvl1.State = 1 Then Addscore 500
    If rvl2.State = 1 Then Addscore 3000

    If rvl3.State = 1 And SpecialMode = 3 Then
    Addscore 10000:Addscore 4000:Multiballadd.Enabled = True
    End If
    If rvl3.State = 1 And SpecialMode <> 3 Then
    Addscore 4000:Multiballadd.Enabled = True
    End If

    If rvl4.State = 1 And SpecialMode = 3 Then
    Addscore 20000:Multiballadd.Enabled = True
    End If
    If rvl4.State = 1 And SpecialMode <> 3 Then
    Addscore 5000:Multiballadd.Enabled = True
    End If

    If rvl5.State = 1 And SpecialMode = 3 Then
    Addscore 30000:Multireplay.Enabled = True:Reel2Advance
    End If
    If rvl5.State = 1 And SpecialMode <> 3 Then
    Addscore 5000:Multireplay.Enabled = True:Reel2Advance
    End If

End If
End Sub

'********************************** Add ExtraBalls

Sub Multiballadd_Timer
    eb1 = eb1 + 1
    If BallsRemaining(CurrentPlayer) < 9 Then
    BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) + 1
    UpdateBallInPlay
    End If
    If eb1 => eb Then
    eb1 = 0
    Multiballadd.Enabled = False
    End If
End Sub

'********************************** Add Replays
Sub MultiReplay_Timer    sv1 = sv1 + 1    AwardSpecialMode    If sv1 => sv Then    sv1 = 0    MultiReplay.Enabled = False    End IfEnd Sub
'******************
' RealTime Updates
'******************

Sub GameTimer_Timer()
	LogoL.RotZ = LeftFlipper.CurrentAngle
	LogoR.RotZ = RightFlipper.CurrentAngle
	GateA.RotX = Gate.CurrentAngle*0.6
End Sub

' *********************************************************************
' 					Wall, rubber and metal hit sounds
' *********************************************************************

Sub Rubbers_Hit(idx):PlaySoundAtBallVol "rubber1", RubVol:End Sub

Sub LeftFlipper_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

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

'*****************************************
'PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, Vol) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, Vol, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundLoopAtVol(soundname, tableobj, Vol) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, -1, Vol, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVol(soundname, VolMult) ' play a sound at the ball position, like rubbers, targets
    PlaySound soundname, 0, Vol(ActiveBall) * VolMult, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 19 ' total number of balls
Const lob = 0  'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
		aBallShadow(b).Visible = 0
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)

		aBallShadow(b).X = BOT(b).X
		aBallShadow(b).Y = BOT(b).Y
		aBallShadow(b).Height = BOT(b).Z - (BallSize / 2)+1

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 20000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 4
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol*RolVol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

		' Ball Shadow
		If BallSHW = 1 Then
			aBallShadow(b).Visible = 1
		Else
			aBallShadow(b).Visible = 0
		End If

       If INT(BallVel(BOT(b))*100) < 3 Then
			CkActivated = 1
            Else
			CkActivated = 0
		End If

		' play ball drop sounds
		If BOT(b).VelZ < -1 And BOT(b).z < 50 And BOT(b).z > 27 Then 'height adjust for ball drop sounds
			PlaySound ("fx_ballhit" & b), 0, (ABS(BOT(b).velz)/17)*DroVol, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(MaxVel / BOT(b).VelX)
            speedfactory = ABS(MaxVel / BOT(b).VelY)
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

Const ColVol = 1	'Ball Collision, no multiball in this table

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(ColVol*((velocity) ^2 / 200)), Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

' ******************************************************************
' GNMOD - Multiple High Score Display and Collection
' jpsalas mod: changed ramps by flashers (faster, no extra shadows)
' ******************************************************************

Dim EnteringInitials ' Normally zero, set to non-zero to enter initials
EnteringInitials = False
Dim ScoreChecker
ScoreChecker = 0
Dim CheckAllScores
CheckAllScores = 0
Dim sortscores(4)
Dim sortplayers(4)

Dim PlungerPulled
PlungerPulled = 0

Dim SelectedChar   ' character under the "cursor" when entering initials

Dim HSTimerCount   ' Pass counter For HS timer, scores are cycled by the timer
HSTimerCount = 5   ' Timer is initially enabled, it'll wrap from 5 to 1 when it's displayed

Dim InitialString  ' the string holding the player's initials as they're entered

Dim AlphaString    ' A-Z, 0-9, space (_) and backspace (<)
Dim AlphaStringPos ' pointer to AlphaString, move Forward and backward with flipper keys
AlphaString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_<"

Dim HSNewHigh      ' The new score to be recorded

Dim HSScore(5)     ' High Scores read in from config file
Dim HSName(5)      ' High Score Initials read in from config file

' default high scores, remove this when the scores are available from the config file
HSScore(1) = 25000
HSScore(2) = 24000
HSScore(3) = 23000
HSScore(4) = 22000
HSScore(5) = 21000

HSName(1) = "AAA"
HSName(2) = "ZZZ"
HSName(3) = "XXX"
HSName(4) = "ABC"
HSName(5) = "BBB"

Sub HighScoreTimer_Timer
    If EnteringInitials Then
        If HSTimerCount = 1 Then
            SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
            HSTimerCount = 2
        Else
            SetHSLine 3, InitialString
            HSTimerCount = 1
        End If
    ElseIf bGameInPlay And ScoreOnCard = 0 Then
        SetHSLine 1, "HIGH SCORE1"
        SetHSLine 2, HSScore(1)
        SetHSLine 3, HSName(1)
        HSTimerCount = 5 ' set so the highest score will show after the game is over
        HighScoreTimer.Enabled = False
    ElseIf bGameInPlay And ScoreOnCard = 1 Then
        SetHSLine 1, "CR" & Credits & " " & "SCORE"
        SetHSLine 2, Score(1)
        SetHSLine 3, "BALL  " & BallsRemaining(1)' & "  " & rv(1) & rv(2) & rv(3)
        HighScoreTimer.Interval = 10
    ElseIf CheckAllScores Then
        NewHighScore sortscores(ScoreChecker), sortplayers(ScoreChecker)
    Else
        ' cycle through high scores
        HighScoreTimer.interval = 2000
        HSTimerCount = HSTimerCount + 1
        If HsTimerCount > 5 Then
            HSTimerCount = 1
        End If
    If bGameInPlay = 0 And ScoreOnCard = 1 Then
        SetHSLine 1, Score(1) & " HS" + FormatNumber(HSTimerCount, 0)
        SetHSLine 2, HSScore(HSTimerCount)
        SetHSLine 3, HSName(HSTimerCount)
    Else
        SetHSLine 1, "HIGH SCORE" + FormatNumber(HSTimerCount, 0)
        SetHSLine 2, HSScore(HSTimerCount)
        SetHSLine 3, HSName(HSTimerCount)
    End If
    End If
End Sub

Function GetHSChar(String, Index)
    Dim ThisChar
    Dim FileName
    ThisChar = Mid(String, Index, 1)
    FileName = "PostIt"
    If ThisChar = " " or ThisChar = "" Then
        FileName = FileName & "BL"
    ElseIf ThisChar = "<" Then
        FileName = FileName & "LT"
    ElseIf ThisChar = "_" Then
        FileName = FileName & "SP"
    Else
        FileName = FileName & ThisChar
    End If
    GetHSChar = FileName
End Function

Sub SetHsLine(LineNo, String)
    Dim Letter
    Dim ThisDigit
    Dim ThisChar
    Dim StrLen
    Dim LetterLine
    Dim Index
    Dim StartHSArray
    Dim EndHSArray
    Dim LetterName
    Dim xFor
    StartHSArray = array(0, 1, 12, 22)
    EndHSArray = array(0, 11, 21, 31)
    StrLen = len(String)
    Index = 1

    For xFor = StartHSArray(LineNo)to EndHSArray(LineNo)
        Eval("HS" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub NewHighScore(NewScore, PlayNum)
    If NewScore > HSScore(5)Then
        HighScoreTimer.interval = 500
        HSTimerCount = 1
        AlphaStringPos = 1      ' start with first character "A"
        EnteringInitials = True ' intercept the control keys while entering initials
        InitialString = ""      ' initials entered so far, initialize to empty
        SetHSLine 1, "PLAYER " + FormatNumber(PlayNum, 0)
        SetHSLine 2, "ENTER NAME"
        SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
        HSNewHigh = NewScore
    ' AwardSpecial
    End If
    ScoreChecker = ScoreChecker-1
    If ScoreChecker = 0 Then
        CheckAllScores = 0
    End If
End Sub

Sub CollectInitials(keycode)
    Dim i
    If keycode = LeftFlipperKey Then
        ' back up to previous character
        AlphaStringPos = AlphaStringPos - 1
        If AlphaStringPos < 1 Then
            AlphaStringPos = len(AlphaString) ' handle wrap from beginning to End
            If InitialString = "" Then
                ' Skip the backspace If there are no characters to backspace over
                AlphaStringPos = AlphaStringPos - 1
            End If
        End If
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
        PlaySound "fx_Previous"
    ElseIf keycode = RightFlipperKey Then
        ' advance to Next character
        AlphaStringPos = AlphaStringPos + 1
        If AlphaStringPos > len(AlphaString)or(AlphaStringPos = len(AlphaString)and InitialString = "")Then
            ' Skip the backspace If there are no characters to backspace over
            AlphaStringPos = 1
        End If
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
        PlaySound "fx_Next"
    ElseIf keycode = StartGameKey or keycode = PlungerKey Then
        SelectedChar = MID(AlphaString, AlphaStringPos, 1)
        If SelectedChar = "_" Then
            InitialString = InitialString & " "
            PlaySound("fx_Esc")
        ElseIf SelectedChar = "<" Then
            InitialString = MID(InitialString, 1, len(InitialString)- 1)
            If len(InitialString) = 0 Then
                ' If there are no more characters to back over, don't leave the < displayed
                AlphaStringPos = 1
            End If
            PlaySound("fx_Esc")
        Else
            InitialString = InitialString & SelectedChar
            PlaySound("fx_Enter")
        End If
        If len(InitialString) < 3 Then
            SetHSLine 3, InitialString & SelectedChar
        End If
    End If
    If len(InitialString) = 3 Then
        ' save the score
        For i = 5 to 1 step -1
            If i = 1 or(HSNewHigh > HSScore(i)and HSNewHigh <= HSScore(i - 1))Then
                ' Replace the score at this location
                If i < 5 Then
                    HSScore(i + 1) = HSScore(i)
                    HSName(i + 1) = HSName(i)
                End If
                EnteringInitials = False
                HSScore(i) = HSNewHigh
                HSName(i) = InitialString
                HSTimerCount = 5
                HighScoreTimer_Timer
                HighScoreTimer.interval = 2000
                PlaySound("fx_Bong")
                Exit Sub
            ElseIf i < 5 Then
                ' move the score in this slot down by 1, it's been exceeded by the new score
                HSScore(i + 1) = HSScore(i)
                HSName(i + 1) = HSName(i)
            End If
        Next
    End If
End Sub
' End GNMOD

'******************************
'     DOF lights ball entrance
'******************************
Sub Trigger001_Hit
If Tilted Then Exit Sub
DOF 260, DOFPulse
End Sub

'16/10 a 16/9
'FSS X Y Scale da 1,77 a 2,05