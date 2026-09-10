' ***************************************************************************

'                           VISUAL PINBALL X
'                 .--.
'                ( XI )
'                 '--'
'             . ' ' |
'          .' _.-'¯¯¯¯¯¯'. _   _  ____      ___     _   _      _
'        .' .'    ____    | | | ||  _ '.  .'   '.  | \ | |  .'¯ ¯'.
'       .  /   .'¯    ¯'--| |_| || |_) ) / .'¯'. \ |  \| | / .'¯'. \
'      .  /   /           |  _  ||  _ ( ( (     ) )|     |( (     ) )
'  .--.. (_- (            | | | || | \ \ \ '._.' / | |\  | \ '._.' /
' ( IX )-<---+------o     |_| |_||_|  |_| '.___.'  |_| \_|  '._ _.'
'  '--'' (¯- (      |                                          ¯
'      '  \   \     |       _______ _____  _____ _____  _____ ______ _____
'       '  \   '.   |   __.|__   __|  __ \|_   _/ ____|/ ____|  ____|  __ \
'        '. '.   ¯¯\|/¯¯  |   | |  | |__) | | || |  __| |  __| |__  | |__) |
'          '. ¯'-.__V__.--'   | |  |  _  /  | || | |_ | | |_ |  __| |  _  /
'             ' . . |         | |  | | \ \ _| || |__| | |__| | |____| | \ \
'                 .--.        |_|  |_|  \_\_____\_____|\_____|______|_|  \_\
'                ( VI )
'                 '--'
'
' 					    2nd Table By: cHuGaLaefoo
'        An original table based on the layout of Stern's KISS table
'
'			  Fully re-themed from the table Pokemon by JPSalas
'                    JPSalas original DMD Pinball Script
'				       Modded gameplay rules by cHuG
'   Special thanks to JPSalas, Wylte and aphophis for answering all of
' 						 my scripting questions

' ****************************************************************************

Option Explicit
Randomize

Const BallSize = 50 	' 50 is the normal size
Const BallMass = 1		' 1 is normal ball
Const SongVolume = 0.1 ' 1 is full volume. Value is from 0 to 1

'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8
Const RollingVolume = 1.5

' Define any Constants

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
'or False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = True

Const cGameName = "ChronoTrigger"
Const myVersion = "1.18"
Const MaxPlayers = 4
Const BallSaverTime = 15' value in seconds
Const MaxMultiplier = 99 ' almost no limit in this game
Const BallsPerGame = 3   ' 3 or 5
Const MaxMultiballs = 5  ' max number of balls during multiballs

Const ReflipAngle = 20

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If

'//////////////////////////////////////////////////////////////////////
'// LUT
'//////////////////////////////////////////////////////////////////////

'//////////////---- LUT (Colour Look Up Table) ----//////////////
'0 = Fleep Natural Dark 1
'1 = Fleep Natural Dark 2
'2 = Fleep Warm Dark
'3 = Fleep Warm Bright
'4 = Fleep Warm Vivid Soft
'5 = Fleep Warm Vivid Hard
'6 = Skitso Natural and Balanced
'7 = Skitso Natural High Contrast
'8 = 3rdaxis Referenced THX Standard
'9 = CalleV Punchy Brightness and Contrast
'10 = HauntFreaks Desaturated
'11 = Tomate Washed Out 
'12 = VPW Original 1 to 1
'13 = Bassgeige
'14 = Blacklight
'15 = B&W Comic Book

Dim LUTset, DisableLUTSelector, LutToggleSound, bLutActive
LutToggleSound = True
LoadLUT
'LUTset = 12		' Override saved LUT for debug
SetLUT
DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1

Sub SetLUT  'AXS
	Table1.ColorGradeImage = "LUT" & LUTset
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
	Select Case LUTSet
		Case 0: LUTBox.text = "Fleep Natural Dark 1"
		Case 1: LUTBox.text = "Fleep Natural Dark 2"
		Case 2: LUTBox.text = "Fleep Warm Dark"
		Case 3: LUTBox.text = "Fleep Warm Bright"
		Case 4: LUTBox.text = "Fleep Warm Vivid Soft"
		Case 5: LUTBox.text = "Fleep Warm Vivid Hard"
		Case 6: LUTBox.text = "Skitso Natural and Balanced"
		Case 7: LUTBox.text = "Skitso Natural High Contrast"
		Case 8: LUTBox.text = "3rdaxis Referenced THX Standard"
		Case 9: LUTBox.text = "CalleV Punchy Brightness and Contrast"
		Case 10: LUTBox.text = "HauntFreaks Desaturated"
  		Case 11: LUTBox.text = "Tomate washed out"
        Case 12: LUTBox.text = "VPW original 1on1"
        Case 13: LUTBox.text = "bassgeige"
        Case 14: LUTBox.text = "blacklight"
        Case 15: LUTBox.text = "B&W Comic Book"
		Case 16: LUTBox.text = "Skitso New ColorLut"
	End Select
	LUTBox.TimerEnabled = 1
End Sub

Sub SaveLUT
	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if

	if LUTset = "" then LUTset = 12 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "ChronoTriggerLUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub
Sub LoadLUT
    bLutActive = False
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=12
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "ChronoTriggerLUT.txt") then
		LUTset=12
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "ChronoTriggerLUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=12
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub

' Load the core.vbs for supporting Subs and functions

LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
    On Error Goto 0
End Sub

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim Bonus
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
Dim bAutoPlunger

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock
Dim BallsInHole

' Define Game Flags

' Chug additions
Dim Spekkio 'Triple tech multiball
Dim CharMusic
Dim LavosMB 'Lavos multiball
Dim UltWeapons
Dim EggBonusCount
Dim EnemiesCount
Dim Lavos1stForm
Dim Lavos2ndForm
Dim Lavos3rdForm
Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Dim Gamewon
Dim HScheck 
Dim Magus
Dim Lavos

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
Dim TotalBonus

Dim plungerIM 'used mostly as an autofire plunger
Dim ttable
Dim EggMagnet

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
	LoadLUT
    LoadEM
    Dim i
    Randomize

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 60 ' Plunger Power
    Const IMTime = 0.25        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("", 141, DOFPulse, DOFContactors), SoundFXDOF("", 141, DOFPulse, DOFContactors)
		SoundPlungerReleaseBall() 
        .CreateEvents "plungerIM"
    End With

    'Venusaur turntable
    Set ttable = New cvpmTurnTable
    With ttable
        .InitTurnTable Magnet1, 90
        .spinCW = False
        .MotorOn = True
        .CreateEvents "ttable"
    End With

    'Egg Magnet, it activates when the 4 eggs are lit
    Set EggMagnet = New cvpmMagnet
    With EggMagnet
        .X = 403
        .Y = 267
        .InitMagnet Magnet2, 90
        .GrabCenter = 1
   .CreateEvents "EggMagnet"
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    'load saved values, highscore, names, jackpot
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
    bFreePlay = True 'Set it to True if you don't want coins

    ' initialze chug flags
	Spekkio = False
	LavosMB = False
'    CatchemTimer.Enabled = False
	EggBonusCount = 0
	EnemiesCount = 0
	Lavos1stForm = False
	Lavos2ndForm = False
	Lavos3rdForm = False
	Gamewon = False
	HScheck = False
	Magus = False
	Lavos = False
	

    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = True
    bMultiBallMode = False
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInLock = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bJustStarted = True
	TotalBonus = 0
    EndOfGame()
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

'LUT controls
If keycode = LeftMagnaSave Then bLutActive = True
    If keycode = RightMagnaSave Then
            If bLutActive Then
                    if DisableLUTSelector = 0 then
                LUTSet = LUTSet  - 1
                if LutSet < 0 then LUTSet = 16
                SetLUT
                ShowLUT
            End If
        End If
        End If


    If Keycode = AddCreditKey And Hscheck = False Then
        Credits = Credits + 1
        DOF 125, DOFOn

                Select Case Int(rnd*3)
                        Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
                        Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
                        Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

                End Select

        If(Tilted = False) Then
            DMDFlush
            DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, ""


            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        PlungerIM.AutoFire:AnimateRattata
		SoundPlungerReleaseBall()
		Playsound "CT_Portal"
    End If
    If bGameInPlay AND NOT Tilted Then


        If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft():CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight():CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter():CheckTilt

        If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress: SolLFlipper 1
        If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress: SolRFlipper 1 

        If keycode = StartGameKey Then 
			soundStartButton()
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    PlaySound "CT_Bell"
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 750, True, ""
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        PlaySound "CT_Bell"
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 750, True, ""

                        If Credits <1 Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMDFlush
                            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, ""
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
                            If Credits <1 Then DOF 125, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, ""
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)

    If hsbModeActive Then EnterHighScoreKey(keycode)

' Table specific
End Sub

Sub Table1_KeyUp(ByVal keycode)

If keycode = LeftMagnaSave Then bLutActive = False

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress: SolLFlipper 0
        If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress: SolRFlipper 0
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
	SaveLUT
    Savehs
    If UseFlexDMD Then FlexDMD.Run = False
    If B2SOn = True Then Controller.Stop
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("", 101, DOFOn, DOFFlippers), LeftFlipper
        lf.fire 'LeftFlipper.RotateToEnd

				If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
                        RandomSoundReflipUpLeft LeftFlipper
                Else 
                        SoundFlipperUpAttackLeft LeftFlipper
                        RandomSoundFlipperUpLeft LeftFlipper
                End If                
'		LFPress = 1
        RotateLaneLightsLeft
    Else
        PlaySoundAt SoundFXDOF("", 101, DOFOff, DOFFlippers), LeftFlipper
'		LFPress = 0
'		EOST = leftflipper.eostorque
'		EOSA = leftflipper.eostorqueangle
        LeftFlipper.RotateToStart
                If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
                        RandomSoundFlipperDownLeft LeftFlipper
                End If
                FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("", 102, DOFOn, DOFFlippers), RightFlipper
        rf.fire 'RightFlipper.RotateToEnd

				If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
                        RandomSoundReflipUpRight RightFlipper
                Else 
                        SoundFlipperUpAttackRight RightFlipper
                        RandomSoundFlipperUpRight RightFlipper
                End If
'		RFPress = 1
        RotateLaneLightsRight
    Else
        PlaySoundAt SoundFXDOF("", 102, DOFOff, DOFFlippers), RightFlipper
'		RFPress = 0
'		EOST = rightflipper.eostorque
'		EOSA = rightflipper.eostorqueangle
        RightFlipper.RotateToStart
                If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
                        RandomSoundFlipperDownRight RightFlipper
                End If        
                FlipperRightHitParm = FlipperUpSoundLevel
    End If
End Sub

' flippers hit Sound



Sub RotateLaneLightsLeft
    Dim TempState
    TempState = LeftOutlane.State
    LeftOutlane.State = LeftInlane1.State
    LeftInlane1.State = LeftInlane2.State
    LeftInlane2.State = RightInlane.State
    RightInlane.State = RightOutlane.State
    RightOutlane.State = TempState
End Sub

Sub RotateLaneLightsRight
    Dim TempState
    TempState = RightOutlane.State
    RightOutlane.State = RightInlane.State
    RightInlane.State = LeftInlane2.State
    LeftInlane2.State = LeftInlane1.State
    LeftInlane1.State = LeftOutlane.State
    LeftOutlane.State = TempState
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <15) AND HScheck = False Then 'show a warning
        DMD "_", CL(1, "CAREFUL!"), "", eNone, eBlinkFast, eNone, 500, True, ""
    End if
    If Tilt> 15 AND HSCheck = False Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD "", "", "tilt", eNone, eNone, eBlink, 200, False, ""
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
                If bCatchemMode Then StopCatchem_Timer
                If bcoinfrenzy Then StopCoinFrenzyTimer_Timer
                If bPikachuTargetMode Then PikachuTargetTimer_Timer
                If bCharizardMode Then StopCharizardTimer_Timer
                If bRampBonus Then StopRampBonusTimer_Timer
                If bLoopBonus Then StopLoopBonusTimer_Timer
				CharMusic = False
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt> 0 Then
        Tilt = Tilt - 0.1
    Else
        Me.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        'Bumper1.Force = 0

        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        LeftSlingshot1.Disabled = 1
        RightSlingshot1.Disabled = 1
    Else
        'turn back on GI and the lights
        'GiOn
        'Bumper1.Force = 6
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        LeftSlingshot1.Disabled = 0
        RightSlingshot1.Disabled = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0) Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        DMDFlush
        EndOfBall()
        Me.Enabled = False
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
    If CatchID <= 123 Then
				PlaySong "mu_battle1"
			Elseif CatchID <= 129 Then
				PlaySong "CTSpekkio"
			Elseif CatchID <= 152 Then
				PlaySong "mu_battle2"
			Elseif CatchID <= 169 Then
				PlaySong "mu_battle3"
	End If

End Sub

Sub ChangeSong
    If(BallsOnPlayfield = 0) Then
        PlaySong "mu_end"

		Else
			If Gamewon = True Then
			PlaySong "CTEnding"
		Else 
			If Magus = True Then
			PlaySong "CTMagus"
		Else 
			If Lavos1stForm = True And UltWeapons = 1 Then
			PlaySong "CTLavos1"
		Else
			If Lavos2ndForm = True Then
			PlaySong "CTLavos2"
		Else
			If Lavos3rdForm = True Then
			PlaySong "CTLavos3"
		Else
			If bMultiballMode And Spekkio = True Then
			PlaySong "CTChrono"
		Else
			If bMultiBallMode And LavosMB = True Then
				PlaySong "CTChrono"
        Else
            If bCatchemMode Then
                PlayBattleSong
		Else
			If BallLight7.State = 1  And CharMusic = True Then
				PlaySong "CTMagus"
		Else
			If BallLight6.State = 1  And CharMusic = True Then
				PlaySong "CTAyla"
		Else
			If BallLight5.State = 1  And CharMusic = True Then
				PlaySong "CTRobo"
		Else
			If BallLight4.State = 1  And CharMusic = True Then
				PlaySong "CTFrog"
		Else
			If BallLight3.State = 1  And CharMusic = True Then
				PlaySong "CTLucca"
		Else
			If BallLight2.State = 1  And CharMusic = True Then
				PlaySong "CTMarle"	
            Else
				Dim tmp
				tmp = INT(RND * 10)
				Select Case tmp
					Case 0:PlaySong "CT1000AD"
					Case 1:PlaySong "CTFair"
					Case 2:PlaySong "CT600AD"
					Case 3:PlaySong "CTLight"
					Case 4:PlaySong "CT2300AD"
					Case 5:PlaySong "CTHope"
					Case 6:PlaySong "CT6.5MBC"
					Case 7:PlaySong "CTTyran"
					Case 8:PlaySong "CT12000BC"
					Case 9:PlaySong "CTUnderSea"

				End Select
			End If
			End If
			End If
			End If
			End If
			End If
			End If
            End If
			End If
			End If
			End If
			End If
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
        If UBound(tmp) = -1 Then
            GiOff
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

' GI light sequence effects

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
        Case 1 'all blink fast
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 5, 10
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
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 5, 10
    End Select
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

'Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
'    Vol = Csng(BallVel(ball) ^2 / 2000)
'End Function
'
'Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
'    Dim tmp
'    tmp = ball.x * 2 / table1.width-1
'    If tmp > 0 Then
'        Pan = Csng(tmp ^10)
'    Else
'        Pan = Csng(-((- tmp) ^10))
'    End If
'End Function
'
'Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
'    Pitch = BallVel(ball) * 20
'End Function
'
'Function BallVel(ball) 'Calculates the ball speed
'    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
'End Function
'
'Function AudioFade(ball) 'only on VPX 10.4 and newer
'    Dim tmp
'    tmp = ball.y * 2 / Table1.height-1
'    If tmp > 0 Then
'        AudioFade = Csng(tmp ^10)
'    Else
'        AudioFade = Csng(-((- tmp) ^10))
'    End If
'End Function
'
'Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
'    PlaySound soundname, 0, 1, Pan(tableobj), 0.06, 0, 0, 0, AudioFade(tableobj)
'End Sub
'
'Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
'    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
'End Sub

'********************************************
'   JP's VP10 Rolling Sounds + Ballshadow
' uses a collection of shadows, aBallShadow
'********************************************

Const tnob = 10   'total number of balls, 20 balls, from 0 to 19
Const lob = 0     'number of locked balls
Const maxvel = 50 'max ball velocity
ReDim rolling(tnob)
InitRolling


Dim DropCount
ReDim DropCount(tnob)

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
        StopSound("BallRoll_" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z -24


		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
                        rolling(b) = True
                        PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * RollingVolume, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

                Else
                        If rolling(b) = True Then
                                StopSound("BallRoll_" & b)
                                rolling(b) = False
                        End If
                End If


                '***Ball Drop Sounds***
                If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
                        If DropCount(b) >= 5 Then
                                DropCount(b) = 0
                                If BOT(b).velz > -7 Then
                                        RandomSoundBallBouncePlayfieldSoft BOT(b)
                                Else
                                        RandomSoundBallBouncePlayfieldHard BOT(b)
                                End If                                
                        End If
                End If
                If DropCount(b) < 5 Then
                        DropCount(b) = DropCount(b) + 1
                End If

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

'Sub OnBallBallCollision(ball1, ball2, velocity)
'    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
'End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

'Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
'Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
'Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
'Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
'Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
'Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

' Pokémon sound efects and fanfares

'Sub PlaySoundEffect()
'    Dim n
'    n = "po_effect" & INT(RND * 36) + 1
'    PlaySound n, 0, 1, pan(ActiveBall)
'End Sub

'Sub PlayFanfare()
'    Dim n
'    n = "po_fanfare" & INT(RND * 6) + 1
'    PlaySound n
'End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

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
        Coins(i) = 0
		EggID(i) = -1
    Next

    ' initialise any other flags
    bMultiBallMode = False
    Tilt = 0

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

    ' set the current players bonus multiplier back down to 1X
    SetBonusMultiplier 1

    ' reset any drop targets, lights, game modes etc..
    'LightShootAgain.State = 0
    Bonus = 0
    bExtraBallWonThisBall = False
    ResetNewBallLights()

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    'bSkillShotReady = True 'no skillshot in this game

	HScheck = False

    'Change the music ?

    'Reset any table specific
    TargetBonus = 0
    BumperBonus = 0
    HoleBonus = 0
	TotalBonus = 0

	Spekkio = False
	LavosMB = False
'    CatchemTimer.Enabled = False
	EggMagnet.MagnetOn = False
	EggBonusCount = 0
	EnemiesCount = 0
'	Gamewon = False

End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedball BallSize / 2
'    UpdateBallImage
    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySoundAt SoundFXDOF("", 123, DOFPulse, DOFContactors), BallRelease
	RandomSoundBallRelease BallRelease
    BallRelease.Kick 90, 4

' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield> 1 Then
        bMultiBallMode = True
        bAutoPlunger = True
        ChangeSong
    End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
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
                Me.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            Me.Enabled = False
        End If
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded
'
Sub EndOfBall()
    Dim BonusDelayTime
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)
    If(Tilted = False) Then
        Dim AwardPoints
        AwardPoints = 0

' add in any bonus points (multipled by the bonus multiplier)
'AwardPoints = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer)
'AddScore AwardPoints
'debug.print "Bonus Points = " & AwardPoints
'DMD "", CL(1, "BONUS: " & BonusPoints(CurrentPlayer) & " X" & BonusMultiplier(CurrentPlayer) ), "", eNone, eBlink, eNone, 1000, True, ""

'this table uses several bonus
        AwardPoints = TargetBonus * 1000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints) ), CL(1, "TARGET BONUS: " & TargetBonus), "", eBlink, eNone, eNone, 800, False, "po_bonus1"

        AwardPoints = HoleBonus * 5000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints) ), CL(1, "HOLE BONUS: " & HoleBonus), "", eBlink, eNone, eNone, 800, False, "po_bonus2"

        AwardPoints = EnemiesCount * 62500
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints) ), CL(1, "ENEMIES DEFEATED: " & EnemiesCount ), "", eBlink, eNone, eNone, 800, False, "po_bonus3"

        AwardPoints = EggBonusCount * 250000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints) ), CL(1, "WEAPONS ACQUIRED: " & EggBonusCount ), "", eBlink, eNone, eNone, 800, False, "po_bonus4"

        AwardPoints = BumperBonus * 100000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints) ), CL(1, "GATO BUMPER BONUS: " & BumperBonus), "", eBlink, eNone, eNone, 800, False, "po_bonus5"

        If Balls = BallsPerGame Then 'this is the last ball, add the coins left to the score
            AwardPoints = Coins(CurrentPlayer) * 1000
            TotalBonus = TotalBonus + AwardPoints
            DMD CL(0, FormatScore(AwardPoints) ), CL(1, "COIN BONUS: " & Coins(CurrentPlayer) ), "", eBlink, eNone, eNone, 800, False, "po_bonus6"
        End If

        DMD CL(0, FormatScore(TotalBonus) ), CL(1, "TOTAL BONUS " & " X " & BonusMultiplier(CurrentPlayer) ), "", eBlinkFast, eNone, eNone, 1500, True, "po_bonus7"
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        BonusDelayTime = 7000
        vpmtimer.addtimer BonusDelayTime, "Addscore TotalBonus '"
    Else
        'no bonus to count so move quickly to the next stage
        BonusDelayTime = 100
    End If
    ' start the end of ball timer which allows you to add a delay at this point
    vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the currentplayer)
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
        DMD "", "", "Extraball2", eNone, eBlink, eBlink, 1500, True, ""
		bBallSaverready = True

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point

        ' Create a new ball in the shooters lane
        vpmtimer.addtimer 1500, "CreateNewBall() '"
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
            'debug.print "No More Balls, High Score Entry"

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
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    EggTarget5.IsDropped = 0
    If NOT bJustStarted Then
        ChangeSong
    End If
    bJustStarted = False

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
    ' pretend to knock the ball into the ball storage mech
	RandomSoundDrain Drain

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
            DMD "_", CL(1, "BALL SAVED"), "bkempty", eNone, eBlinkfast, eNone, 800, True, ""
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1) Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True) then
                    ' not in multiball mode any more
                    bMultiBallMode = False
					Spekkio = False
					LavosMB = False
                    ChangeGi "white"
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
                    ResetJackpotLights
                    ChangeSong
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
                ChangeSong
                ' handle the end of ball (change player, high score entry etc..)
                EndOfBall()
                ' End Modes and timers
                If bCatchemMode Then StopCatchem_Timer
                If bcoinfrenzy Then StopCoinFrenzyTimer_Timer
                If bPikachuTargetMode Then PikachuTargetTimer_Timer
                If bCharizardMode Then StopCharizardTimer_Timer
                If bRampBonus Then StopRampBonusTimer_Timer
                If bLoopBonus Then StopLoopBonusTimer_Timer
                ReduceBallType
				CharMusic = False
				Magus = False
				Gamewon = False
                ChangeGi "white"
            End If
        End If
    End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
	dmdscorenow
    PlaySoundAt "fx_sensor", swPlungerRest
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    LaunchLight.State = 2
    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        ' 'debug.print "autofire the ball"
        PlungerIM.AutoFire:AnimateRattata
		Playsound "CT_Portal"
        DOF 124, DOFPulse
        DOF 121, DOFPulse
        bAutoPlunger = False
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
 '   If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
 '       EnableBallSaver BallSaverTime
 '   End If
    'Start the skillshot if ready
    If bSkillShotReady Then
        ResetSkillShotTimer.Interval = 1000 * 5 ' 5 seconds
        ResetSkillShotTimer.Enabled = True
        LightSeqSkillshot.Play SeqAllOff
        LightSeqSkillshotHit.Play SeqBlinking, , 5, 150
    'PlaySound a sound
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
    DOF 141, DOFPulse
    DOF 121, DOFPulse
    ChangeSong

	If bBallSaverReady = True Then
		EnableBallSaver BallSaverTime
	End If
    ' turn off LaunchLight
    LaunchLight.State = 0
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimer.Interval = 1000 * seconds
    BallSaverTimer.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimer_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimer.Enabled = False
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
'
Sub AddScore(points)
    If(Tilted = False) Then
        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * BallType
    End if
' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board
'
Sub AddBonus(points)
    If(Tilted = False) Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    End if
' you may wish to check to see if the player has gotten a replay
End Sub

Sub AddCoin(n)
    If(Tilted = False) Then
        ' add the coins to the current players coin variable
        Coins(CurrentPlayer) = Coins(CurrentPlayer) + n
        ' update the score displays
        'DMDScore
    End if

    ' check if there is enough coins to enable the update ball
    If Coins(CurrentPlayer)> 249 Then
        BallUpdateLight.State = 2
    Else
        BallUpdateLight.State = 0
    End If
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If(Tilted = False) Then

        If(bMultiBallMode = True) Then
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
    if(BonusMultiplier(CurrentPlayer) <MaxMultiplier) then
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
    If(BonusMultiplier(CurrentPlayer) = 1) Then
        LightBonus2x.State = 0
        LightBonus3x.State = 0
        LightBigBonus.State = 0
    Else
        ' there is a bonus, turn on all the lights upto the current level
        If(BonusMultiplier(CurrentPlayer) >= 2) Then
            If(BonusMultiplier(CurrentPlayer) >= 2) Then
                LightBonus2x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 3) Then
                LightBonus3x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 4) Then
                LightBigBonus.state = 1
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
        DMD "", "", "Extraball", eNone, eBlinkFast, eBlinkFast, 1500, True, SoundFXDOF("CT_XtraBall", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        GiEffect 1
        LightEffect 2
    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL(1, ("EXTRA GAME WON") ), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("CT_SavePt", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    Credits = Credits + 1
    DOF 125, DOFOn
    GiEffect 1
    LightEffect 1
End Sub

Sub AwardJackpot()
    DMD CL(0, FormatScore(Jackpot) ), CL(1, ("JACKPOT") ), "bkborder", eBlinkFast, eBlinkFast, eNone, 1000, True, ""
    DOF 126, DOFPulse
'    PlayFanfare
    AddScore Jackpot
    AddJackpot 100000
    GiEffect 1
    LightEffect 2
End Sub

Sub AwardSuperJackpot()
    DMD CL(0, FormatScore(Jackpot * PokemonLevel) ), CL(1, ("SUPERJACKPOT") ), "bkborder", eBlinkFast, eBlinkFast, eNone, 1000, True, ""
    DOF 126, DOFPulse
'    PlayFanfare
    AddScore Jackpot * PokemonLevel
    AddJackpot 100000
    GiEffect 1
    LightEffect 2
End Sub

Sub AwardSkillshot()
    DMD CL(0, FormatScore(SkillshotValue) ), CL(1, ("SKILLSHOT") ), "bkborder", eBlinkFast, eBlink, eNone, 1000, True, ""
    DOF 127, DOFPulse
    AddScore SkillshotValue
    ResetSkillShotTimer_Timer
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
    If(x <> "") then Credits = CInt(x) Else Credits = 0:DOF 125, DOFOff:End If
    'x = LoadValue(TableName, "Jackpot")
    'If(x <> "") then Jackpot = CDbl(x) Else Jackpot = 200000 End If
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
    'SaveValue cGameName, "Jackpot", Jackpot
    SaveValue cGameName, "TotalGamesPlayed", TotalGamesPlayed
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
        DOF 125, DOFOn
    End If

    If tmp > HighScore(3)Then
        PlaySound SoundFXDOF("po_fanfare2", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
		HScheck = True
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    PlaySound "CT_Correct"
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
        playsound "CT_Select"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "CT_Select"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
            playsound "CT_HSEnter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "CT_HSWrong"
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
    dLine(0) = ExpandLine(TempTopStr, 0)
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
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
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
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
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
    deSpeed = 30
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

	if Gamewon = False Then
		if(dqHead = dqTail) Then
        tmp = RL(1, FormatScore(Score(Currentplayer) ) )
        'tmp = CL(0, FormatScore(Score(Currentplayer) ) )
        tmp1 = CL(1, "P" & CurrentPlayer & " [" & Balls & " \" & PokemonBonus(CurrentPlayer) & " ^" & EggID(CurrentPlayer) + 1 & " ]" & Coins(Currentplayer) )
        'tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
        'tmp1 = FormatScore(Bonuspoints(Currentplayer) ) & " X" &BonusMultiplier(Currentplayer)
        tmp2 = ""
        If bCatchemMode Then
			tmp = RL(1, CatchMaxHits-CatchHits & " HP LEFT")
            tmp1 = ""
            tmp2 = pokemon(CatchID)

        DMD RL(1, FormatScore(Score(Currentplayer) ) ) & RL(1, CatchMaxHits-CatchHits & " HP LEFT"), tmp1, Pokemon(CatchID), eNone, eNone, eNone, 750, True, ""
        DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 750, True, ""

        End If
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""

	Else
		if(dqHead = dqTail) Then
        tmp = RL(1, FormatScore(Score(Currentplayer) ) )
        'tmp = CL(0, FormatScore(Score(Currentplayer) ) )
        tmp1 = CL(1, "P" & CurrentPlayer & " [" & Balls & " \" & PokemonBonus(CurrentPlayer) & " ^" & EggID(CurrentPlayer) + 1 & " ]" & Coins(Currentplayer) )
        'tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
        'tmp1 = FormatScore(Bonuspoints(Currentplayer) ) & " X" &BonusMultiplier(Currentplayer)
        tmp2 = ""
        If Gamewon = True Then
			tmp = RL(1, "LAVOS DEFEATED")
            tmp1 = ""
            tmp2 = "Ending"

        DMD RL(1, FormatScore(Score(Currentplayer) ) ) & RL(1, ""), tmp1, "Ending", eNone, eNone, eNone, 750, True, ""
        DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 750, True, ""

        End If
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""

	End If

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

    Chars(43) = "dplus"   '+
    Chars(46) = "ddot"    '.
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
    Chars(91) = "dball" '[
    Chars(92) = "dcoin" '|
    Chars(93) = "dpika" ']
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

'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer	
'	Cor.Update 
    RollingUpdate
    coin.RotX = Spinner.CurrentAngle
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

        If MyLight.Name = "Flasher9" or MyLight.Name = "Flasher10" or MyLight.Name = "Flasher11" or MyLight.Name = "Flasher12" Then
            Dim flasherNumber
            flasherNumber = Split(MyLight.Name, "r")
            DOF CInt(flasherNumber(1) ) + 200, DOFPulse
        End If
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
' colors: red, orange, yellow, green, blue, white
'******************************************

Sub SetLightColor(n, col, stat)
    Select Case col
        Case "red"
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(255, 0, 0)
        Case "orange"
            n.color = RGB(18, 3, 0)
            n.colorfull = RGB(255, 64, 0)
        Case "yellow"
            n.color = RGB(18, 18, 0)
            n.colorfull = RGB(255, 255, 0)
        Case "green"
            n.color = RGB(0, 18, 0)
            n.colorfull = RGB(0, 255, 0)
        Case "blue"
            n.color = RGB(0, 18, 18)
            n.colorfull = RGB(0, 255, 255)
        Case "white"
            n.color = RGB(193, 91, 0)
            n.colorfull = RGB(255, 252, 224)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim ii
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER1 " &FormatScore(Score(1) ) ), "", eNone, eNone, eNone, 3000, False, ""
        If PokemonBonus(1) Then
            DMD "", "ENEMIES DEFEATED " & PokemonBonus(1), "", eNone, eNone, eNone, 1000, False, ""
            For ii = 1 to PokemonBonus(1)
                DMD "", "", Pokemon(PokemonCaught(1, ii) ), eNone, eNone, eNone, 1000, False, ""
            Next
		End If
		If EggBonus(1) Then
            DMD "", "WEAPONS ACQUIRED " & EggBonus(1), "", eNone, eNone, eNone, 1000, False, ""
			For ii = 1 to EggBonus(1)
                DMD "", "", Eggs(Weapons(1, ii) ), eNone, eNone, eNone, 1000, False, ""	
			Next
		End If
    End If
    If Score(2) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER2 " &FormatScore(Score(2) ) ), "", eNone, eNone, eNone, 3000, False, ""
        If PokemonBonus(2) Then
            DMD "", "ENEMIES DEFEATED " & PokemonBonus(2), "", eNone, eNone, eNone, 1000, False, ""
            For ii = 1 to PokemonBonus(2)
                DMD "", "", Pokemon(PokemonCaught(2, ii) ), eNone, eNone, eNone, 1000, False, ""
            Next
		End If
		If EggBonus(2) Then
            DMD "", "WEAPONS ACQUIRED " & EggBonus(2), "", eNone, eNone, eNone, 1000, False, ""
			For ii = 1 to EggBonus(2)
                DMD "", "", Eggs(Weapons(2, ii) ), eNone, eNone, eNone, 1000, False, ""	
			Next
        End If
    End If
    If Score(3) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER3 " &FormatScore(Score(3) ) ), "", eNone, eNone, eNone, 3000, False, ""
        If PokemonBonus(3) Then
            DMD "", "ENEMIES DEFEATED " & PokemonBonus(3), "", eNone, eNone, eNone, 1000, False, ""
            For ii = 1 to PokemonBonus(3)
                DMD "", "", Pokemon(PokemonCaught(3, ii) ), eNone, eNone, eNone, 1000, False, ""
            Next
		End If
		If EggBonus(3) Then
            DMD "", "WEAPONS ACQUIRED " & EggBonus(3), "", eNone, eNone, eNone, 1000, False, ""
			For ii = 1 to EggBonus(3)
                DMD "", "", Eggs(Weapons(3, ii) ), eNone, eNone, eNone, 1000, False, ""	
			Next
        End If
    End If
    If Score(4) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER4 " &FormatScore(Score(4) ) ), "", eNone, eNone, eNone, 3000, False, ""
        If PokemonBonus(4) Then
            DMD "", "ENEMIES DEFEATED " & PokemonBonus(4), "", eNone, eNone, eNone, 1000, False, ""
            For ii = 1 to PokemonBonus(4)
                DMD "", "", Pokemon(PokemonCaught(4, ii) ), eNone, eNone, eNone, 1000, False, ""
            Next
		End If
		If EggBonus(4) Then
            DMD "", "WEAPONS ACQUIRED " & EggBonus(4), "", eNone, eNone, eNone, 1000, False, ""
			For ii = 1 to EggBonus(4)
                DMD "", "", Eggs(Weapons(4, ii) ), eNone, eNone, eNone, 1000, False, ""	
			Next
        End If
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
    DMD "", "", "pokemonpinball", eNone, eNone, eNone, 4000, False, ""
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0) ), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode(dummy)
    ChangeSong
    StartLightSeq
    DMDFlush
    ShowTableInfo
End Sub

Sub StopAttractMode
    Dim bulb
    DMDScoreNow
    LightSeqAttract.StopPlay
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

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' tables walls and animations
Sub VPObjects_Init
End Sub

' tables variables and modes init

Dim Coins(4), EggID(4)
Dim BumperBonus, EggBonus(4), HoleBonus, PokemonBonus(4), PokemonBonusAward(4), PokemonCaught(4, 174), Weapons(4, 7), TargetBonus

Sub Game_Init()
    Dim ii, jj, kk
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    'Play some Music
    ChangeSong
    'Init Variables
    Jackpot = 250000
    RattataPos = 0
    bumperHits = 100
    BumperBonus = 0
    PikachuHits = 0
    CatchHits = 0
    BallInHole = 0
    HoleBonus = 0
    For ii = 1 to 4
        PokemonBonus(ii) = 0
        PokemonBonusAward(ii) = 0
        EggBonus(ii) = 0
        For jj = 0 to 174
            PokemonCaught(ii, jj) = 0
			Weapons (ii, kk) = 0
        Next
    Next
    TargetBonus = 0
    PikachuTargetValue = 5000
    ResetPokemonLevel
    ResetHoleLights
    BallType = 1:UpdateBallType
    bCatchemMode = False
    bEggTargetsCompleted = False
    bLockEnabled = False
    LockedBalls = 0
    bcoinfrenzy = False
    coinstep = 0
    bPikachuTargetMode = False
    bCharizardMode = False
    bRampBonus = FALSE
    bLoopBonus = FALSE

'   chug Init
	EggMagnet.MagnetOn = False
	UltWeapons = 0
	Lavos1stForm = False
	Lavos2ndForm = False
	Lavos3rdForm = False
	Gamewon = False

'Init Delays
'Skillshot Init
'MainModes Init()
End Sub

Sub ResetSkillShotTimer_Timer
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
    HatchLight1b.Visible = 0
    HatchLight2b.Visible = 0
    HatchLight3b.Visible = 0
    HatchLight4b.Visible = 0
End Sub

Sub ResetNewBallLights()
'    LightArrow1.State = 2
'    LightArrow6.State = 2
    l53.State = 2
End Sub

Dim RattataPos

Sub AnimateRattata()
    RattataPos = 0
    RattataTimer.Enabled = 1
End Sub

Sub RattataTimer_Timer()
    Select Case RattataPos
        Case 0:Rattata.TransY = 25:RattataPos = 1
        Case 1:Rattata.TransY = 15:RattataPos = 2
        Case 2:Rattata.TransY = 7:RattataPos = 3
        Case 3:Rattata.TransY = 3:RattataPos = 4
        Case 4:Rattata.TransY = 0:RattataTimer.Enabled = 0
    End Select
End Sub

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
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("", 103, DOFPulse, DOFcontactors), Lemk
	RandomSoundSlingshotLeft Lemk
    DOF 105, DOFPulse
    LeftSling4.Visible = 1:LeftSling1.Visible = 0
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 110
    ' add some effect to the table?
    Gi2.State = 0
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
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
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("", 104, DOFPulse, DOFcontactors), Remk
	RandomSoundSlingshotRight Remk
    DOF 106, DOFPulse
    RightSling4.Visible = 1:RightSling1.Visible = 0
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 110
    ' add some effect to the table?
    Gi1.State = 0
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:RightSLing1.Visible = 1:Remk.RotX = -10:Gi1.State = 1:RightSlingShot.TimerEnabled = False
    End Select
    RStep = RStep + 1
End Sub

' Hatch Slingshots

Dim LStep1, RStep1

Sub LeftSlingShot1_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("", 143, DOFPulse, DOFContactors), Lemk1
	RandomSoundSlingshotLeft Lemk1
    Sling3a.Visible = 1:Sling3.Visible = 0
    Lemk1.RotX = 26
    LStep1 = 0
    LeftSlingShot1.TimerEnabled = True
    CheckEggTargets
    ' add some points
    AddScore 510
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot1"
' add some effect to the table?
End Sub

Sub LeftSlingShot1_Timer
    Select Case LStep1
        Case 1:Sling3a.Visible = 0:Sling3b.Visible = 1:Lemk1.RotX = 14
        Case 2:Sling3b.Visible = 0:Sling3c.Visible = 1:Lemk.RotX = 0
        Case 3:Sling3c.Visible = 0:Sling3.Visible = 1:Lemk1.RotX = -20:LeftSlingShot1.TimerEnabled = False
    End Select
    LStep1 = LStep1 + 1
End Sub

Sub RightSlingShot1_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("", 144, DOFPulse, DOFContactors), Remk1
	RandomSoundSlingshotRight Remk1
    Sling2a.Visible = 1:Sling2.Visible = 0
    Remk1.RotX = 26
    RStep1 = 0
    RightSlingShot1.TimerEnabled = True
    CheckEggTargets
    ' add some points
    AddScore 510
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot1"
' add some effect to the table?
End Sub

Sub RightSlingShot1_Timer
    Select Case RStep1
        Case 1:Sling2a.Visible = 0:Sling2b.Visible = 1:Remk.RotX = 14
        Case 2:Sling2b.Visible = 0:Sling2c.Visible = 1:Remk.RotX = 0
        Case 3:Sling2c.Visible = 0:Sling2.Visible = 1:Remk1.RotX = -20:RightSlingShot1.TimerEnabled = False
    End Select
    RStep1 = RStep1 + 1
End Sub

'**********
' Spinner
'**********

Sub Spinner_Spin()
    If Tilted Then Exit Sub
    PlaySoundAt "fx_spinner", Spinner
    DOF 136, DOFPulse
    AddScore 1000
    AddCoin 1
End Sub

'*********************
' Inlanes - Outlanes
'*********************

Sub LeftOutlaneTrigger_Hit()
    DOF 132, DOFPulse
    PlaySoundAt "CT_Ozzie", LeftOutlaneTrigger
    If Tilted Then Exit Sub
'    PlaySoundEffect
    AddScore 10000
    Addcoin 50
    FlashForMs Flasher9, 1000, 50, 0
    FlashForMs Flasher10, 1000, 50, 0
    TargetBonus = TargetBonus + 1
    LeftOutlane.State = 1
    LastSwitchHit = "LeftOutlaneTrigger"
    CheckMultiplier
End Sub

Sub LeftInlaneTrigger1_Hit()
    DOF 133, DOFPulse
    PlaySoundAt "CT_Fire", LeftInlaneTrigger1
    If Tilted Then Exit Sub
'    PlaySoundEffect
    AddScore 3000
    Addcoin 10
    TargetBonus = TargetBonus + 1
    LeftInlane1.State = 1
    LastSwitchHit = "LeftInlaneTrigger1"
    CheckMultiplier
End Sub

Sub LeftInlaneTrigger2_Hit()
    DOF 133, DOFPulse
    PlaySoundAt "CT_Fire", LeftInlaneTrigger2
    If Tilted Then Exit Sub
'    PlaySoundEffect
    AddScore 3000
    Addcoin 10
    TargetBonus = TargetBonus + 1
    LeftInlane2.State = 1
    LastSwitchHit = "LeftInlaneTrigger2"
    CheckMultiplier
End Sub

Sub RightInlaneTrigger_Hit()
    DOF 134, DOFPulse
    PlaySoundAt "CT_Fire", RightInlaneTrigger
    If Tilted Then Exit Sub
'    PlaySoundEffect
    AddScore 3000
    Addcoin 10
    TargetBonus = TargetBonus + 1
    RightInlane.State = 1
    CheckMultiplier
    LastSwitchHit = "RightInlaneTrigger"
End Sub

Sub RightOutlaneTrigger_Hit()
    DOF 135, DOFPulse
    PlaySoundAt "CT_Ozzie", RightOutlaneTrigger
    If Tilted Then Exit Sub
'    PlaySoundEffect
    AddScore 10000
    Addcoin 50
    FlashForMs Flasher9, 1000, 50, 0
    FlashForMs Flasher10, 1000, 50, 0
    TargetBonus = TargetBonus + 1
    RightOutlane.State = 1
    CheckMultiplier
    LastSwitchHit = "RightOutlaneTrigger"
End Sub

Sub CheckMultiplier
    If(LeftOutlane.State = 1) And(LeftInlane1.State = 1) And(LeftInlane2.State = 1) And(RightInlane.State = 1) And(RightOutlane.State = 1) Then
        AddScore 50000
		PlaySound "CT_Chant"
'        PlayFanfare
        LightSeqLanes.Play SeqRandom, 5, , 3000
        IncrementBonusMultiplier()
        LeftOutlane.State = 0
        LeftInlane1.State = 0
        LeftInlane2.State = 0
        RightInlane.State = 0
        RightOutlane.State = 0
    End If
End Sub

Sub EggEntrance_Hit() 'diode switch used for catching pokemons & other modes

    If bcoinfrenzy AND CoinLight2.State = 2 Then
        Addcoin 25
        PlaySound "fx_coins"
        FlashForMs Flasher9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0
        CoinFrenzyTimer_Timer
    End If
    If bCatchemMode Then
        If CatchLight2.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
			PlaySound "CT_ATKA"
            CheckCatchHits
        End If
    End If
End Sub

'********
' Bumper
'********

Dim bumperHits

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("", 109, DOFPulse, DOFContactors), Bumper1
    DOF 138, DOFPulse
	RandomSoundBumperTop Bumper1
    AddScore 500 + 4500 * ABS(Flasher1.Visible) 'a bumper scores 500 points and 5000 points when lit
    bumperHits = bumperHits - 1
    DMDFlush
    DMD RL(1, FormatScore(Score(Currentplayer) ) ), RL(1, bumperHits& " HITS LEFT"), "bumper", eNone, eNone, eNone, 300, True, ""
    CheckBumpers
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("", 110, DOFPulse, DOFContactors), Bumper2
    DOF 140, DOFPulse
	RandomSoundBumperMiddle Bumper2
    AddScore 500 + 4500 * ABS(Flasher2.Visible) 'a bumper scores 500 points and 5000 points when lit
    bumperHits = bumperHits - 1
    DMDFlush
    DMD RL(1, FormatScore(Score(Currentplayer) ) ), RL(1, bumperHits& " HITS LEFT"), "bumper", eNone, eNone, eNone, 375, True, ""
    CheckBumpers
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("", 107, DOFPulse, DOFContactors), Bumper3
    DOF 137, DOFPulse
	RandomSoundBumperBottom Bumper3
    AddScore 500 + 4500 * ABS(Flasher3.Visible) 'a bumper scores 500 points and 5000 points when lit
    bumperHits = bumperHits - 1
    DMDFlush
    DMD RL(1, FormatScore(Score(Currentplayer) ) ), RL(1, bumperHits& " HITS LEFT"), "bumper", eNone, eNone, eNone, 375, True, ""
    CheckBumpers
End Sub

Sub Bumper4_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("", 108, DOFPulse, DOFContactors), Bumper4
    DOF 139, DOFPulse
	RandomSoundBumperBottom Bumper4
    AddScore 500 + 4500 * ABS(Flasher4.Visible) 'a bumper scores 500 points and 5000 points when lit
    bumperHits = bumperHits - 1
    DMDFlush
    DMD RL(1, FormatScore(Score(Currentplayer) ) ), RL(1, bumperHits& " HITS LEFT"), "bumper", eNone, eNone, eNone, 375, True, ""
    CheckBumpers
End Sub

' Bumper Bonus
' 100000 i bonus after each 100 hits

Sub CheckBumpers()
    If bumperHits <= 0 Then
        BumperBonus = BumperBonus + 1
        DMD "_", RL(1, "GATO BONUS " & BumperBonus), "", eNone, eBlink, eNone, 1000, True, ""
        bumperHits = 100
    ' do something more
    End If
End Sub

Sub ResetBumpers()
    bumperHits = 100
    Flasher1.Visible = 0
    Flasher2.Visible = 0
    Flasher3.Visible = 0
    Flasher4.Visible = 0
End Sub

'**************************
' Upper Lanes & loop switch
'**************************

Dim PokemonLevel, HolePos

Sub RightLoopTrigger_Hit() 'set the pokemon levels
    DOF 131, DOFPulse
    PlaySoundAt "fx_sensor", RightLoopTrigger
    If Tilted Then Exit Sub
    TargetBonus = TargetBonus + 1
    If LastSwitchHit = "LeftInlaneTrigger2" Then 'combo
        AddScore 20000
    End If
    If bCatchemMode Then
        If CatchLight5.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
			PlaySound "CT_ATKL"
            CheckCatchHits
        End If
    Else
        If PokemonLevel <4 Then
            PokemonLevel = PokemonLevel + 1
            UpdatePokemonLevel
        End If
        AddScore 2500 * PokemonLevel
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightLoopTrigger"
End Sub

Sub LeftLoopTrigger_Hit()
    DOF 130, DOFPulse
    PlaySoundAt "fx_sensor", LeftLoopTrigger
    If Tilted Then Exit Sub
    TargetBonus = TargetBonus + 1
    IncrementHoleLights
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        AddScore 30000
    End If
    If bcoinfrenzy AND CoinLight1.State = 2 Then
        Addcoin 25
        PlaySound "fx_coins"
        FlashForMs Flasher9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0
        CoinFrenzyTimer_Timer
    End If
    If bCatchemMode Then
        If CatchLight1.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
			PlaySound "CT_ATKM"
            CheckCatchHits
        End If
    Else
        AddScore 2500 * PokemonLevel
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftLoopTrigger"
End Sub

Sub UpdatePokemonLevel
    Select Case PokemonLevel
        Case 0:LightLevel1.State = 0:LightLevel2.State = 0:LightLevel3.State = 0:LightLevel4.State = 0:Flasher1.Visible = 0:Flasher2.Visible = 0:Flasher3.Visible = 0:Flasher4.Visible = 0:LightArrow1.State = 0:LightArrow6.State = 0
        Case 1:LightLevel1.State = 1:LightLevel2.State = 0:LightLevel3.State = 0:LightLevel4.State = 0:Flasher1.Visible = 1:Flasher2.Visible = 0:Flasher3.Visible = 0:Flasher4.Visible = 0:LightArrow5.State = 0
        Case 2:LightLevel1.State = 1:LightLevel2.State = 1:LightLevel3.State = 0:LightLevel4.State = 0:Flasher1.Visible = 1:Flasher2.Visible = 1:Flasher3.Visible = 0:Flasher4.Visible = 0:HoleLight6.State = 2:l53.State = 0
        Case 3:LightLevel1.State = 1:LightLevel2.State = 1:LightLevel3.State = 1:LightLevel4.State = 0:Flasher1.Visible = 1:Flasher2.Visible = 1:Flasher3.Visible = 1:Flasher4.Visible = 0
        Case 4:LightLevel1.State = 1:LightLevel2.State = 1:LightLevel3.State = 1:LightLevel4.State = 1:Flasher1.Visible = 1:Flasher2.Visible = 1:Flasher3.Visible = 1:Flasher4.Visible = 1:LightArrow1.State = 0:LightArrow6.State = 0
    End Select
End Sub

Sub ResetPokemonLevel()
    PokemonLevel = 0
    UpdatePokemonLevel
End Sub

Sub IncrementHoleLights()
    If HolePos <> 6 Then
        HolePos = HolePos + 1
        If HolePos = 6 Then
            HolePos = 1
        End If
    End If
    UpdateHoleLights
End Sub

Sub UpdateHoleLights()
    Select Case HolePos
        Case 0:HoleLight1.State = 0:HoleLight2.State = 0:HoleLight3.State = 0:HoleLight4.State = 0:HoleLight5.State = 0
        Case 1:HoleLight1.State = 2:HoleLight2.State = 0:HoleLight3.State = 0:HoleLight4.State = 0:HoleLight5.State = 0
        Case 2:HoleLight1.State = 0:HoleLight2.State = 2:HoleLight3.State = 0:HoleLight4.State = 0:HoleLight5.State = 0
        Case 3:HoleLight1.State = 0:HoleLight2.State = 0:HoleLight3.State = 2:HoleLight4.State = 0:HoleLight5.State = 0
        Case 4:HoleLight1.State = 0:HoleLight2.State = 0:HoleLight3.State = 0:HoleLight4.State = 2:HoleLight5.State = 0
        Case 5:HoleLight1.State = 0:HoleLight2.State = 0:HoleLight3.State = 0:HoleLight4.State = 0:HoleLight5.State = 2
    End Select
End Sub

Sub ResetHoleLights()
    HolePos = 0
    UpdateHoleLights
End Sub

'*****************
' Pikachu Targets
'*****************

Dim bPikachuTargetMode, PikachuHits, PikachuTargetValue

Sub PikaTarget1_Hit()
    PlaySoundAt SoundFXDOF("", 115, DOFPulse, DOFTargets), PikaTarget1
    If Tilted Then Exit Sub
'    PlaySoundEffect
'    PikachuShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher12, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight1.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget1"
End Sub

Sub PikaTarget2_Hit()
    PlaySoundAt SoundFXDOF("", 115, DOFPulse, DOFTargets), PikaTarget2
    If Tilted Then Exit Sub
'    PlaySoundEffect
'    PikachuShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher11, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight2.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget2"
End Sub

Sub PikaTarget3_Hit()
    PlaySoundAt SoundFXDOF("", 115, DOFPulse, DOFTargets), PikaTarget3
    If Tilted Then Exit Sub
'    PlaySoundEffect
'    PikachuShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher11, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight3.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    PlayRandonPikaSound
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget3"
End Sub

Sub PikaTarget4_Hit()
    PlaySoundAt SoundFXDOF("", 115, DOFPulse, DOFTargets), PikaTarget4
    If Tilted Then Exit Sub
'    PlaySoundEffect
'    PikachuShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher11, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight4.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget4"
End Sub

Sub PikaTarget5_Hit()
    PlaySoundAt SoundFXDOF("", 116, DOFPulse, DOFTargets), PikaTarget5
    If Tilted Then Exit Sub
'    PlaySoundEffect
    If LastSwitchHit = "LeftInlaneTrigger2" Then 'combo
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher12, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
'    CharmanderShake
    TargetBonus = TargetBonus + 1
    PikaLight5.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget5"
End Sub

Sub PikaTarget6_Hit()
    PlaySoundAt SoundFXDOF("", 116, DOFPulse, DOFTargets), PikaTarget6
    If Tilted Then Exit Sub
'    PlaySoundEffect
    If LastSwitchHit = "LeftInlaneTrigger2" Then 'combo
        AddScore 10000
    End If
'    CharmanderShake
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher12, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight6.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget6"
End Sub

Sub PikaTarget7_Hit()
    PlaySoundAt SoundFXDOF("", 116, DOFPulse, DOFTargets), PikaTarget7
    If Tilted Then Exit Sub
'    PlaySoundEffect
    If LastSwitchHit = "LeftInlaneTrigger2" Then 'combo
        AddScore 10000
    End If
 '   CharmanderShake
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher12, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight7.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget7"
End Sub

Sub CheckTargetLights()
    If(PikaLight1.State + PikaLight2.State + PikaLight3.State + PikaLight4.State + PikaLight5.State + PikaLight6.State + PikaLight7.State) = 7 Then 'turn on Pikachu Multiball: 2 ball multiball
        'start the target mode
        StartTargetMode
        ' enable the pikachumultiball light
        GiEffect 2
        PikaMultiballLight.State = 2
        PikaLight1.State = 0
        PikaLight2.State = 0
        PikaLight3.State = 0
        PikaLight4.State = 0
        PikaLight5.State = 0
        PikaLight6.State = 0
        PikaLight7.State = 0
        PikachuTargetValue = PikachuTargetValue + 5000
    End If
End Sub

Sub PlayRandonPikaSound()
    Dim n
    n = INT(RND * 7) + 1
    PlaySound "vo_pika" &n
End Sub

Sub LightSeqPikachu_PlayDone()
    LightSeqPikachu.Play SeqRandom, 7, , 3000
End Sub

Sub StartTargetMode()
    DMD "", "", "d-targetsbonus", eNone, eNone, eBlink, 800, True, "vo_pikachu1"
    'do some show with the lights
    LightSeqPikachu.Play SeqRandom, 7, , 3000
    LightEffect 2
    bPikachuTargetMode = True
    'Starter the timer to turn off the target mode
    PikachuTargetTimer.Enabled = 1
End Sub

Sub PikachuTargetTimer_Timer()
    LightSeqPikachu.StopPlay
    bPikachuTargetMode = False
    PikachuTargetTimer.Enabled = 0
End Sub

'****************************
'Pokemon Center / Coin Frenzy
'****************************

Dim BallType

Sub CenterEnableTrigger_Hit()
    If Tilted Then Exit Sub
    If BallUpdateLight.State = 2 AND BallType <7 Then 'coins are over 250 and ball can be updated
        PokemonCenter.Enabled = True
    Else
        PokemonCenter.Enabled = False
    End If
End Sub

Sub PokemonCenter_Hit()
    PokemonCenter.Enabled = False
    IncrementBallType
    vpmtimer.addtimer 1000, "PokemonCenter.kick 270,7 '"
End Sub

Sub IncrementBallType()
    If Coins(CurrentPlayer)> 249 AND BallType <7 Then
'       PlayFanfare
		CharMusic = True
		PlaySound "po_fanfare3"
		AddMultiball 1
		EnableBallSaver 10
        AddCoin -250
        BallType = BallType + 1
        FlashForMs Flasher9, 1000, 50, 0
        Select case BallType
            Case 1:DMD "", "", "", eNone, eNone, eBlinkFast, 1000, True, ""
            Case 2:DMD "", "", "PartyMarle", eNone, eNone, eBlinkFast, 1000, True, ""
            Case 3:DMD "", "", "PartyLucca", eNone, eNone, eBlinkFast, 1000, True, ""
            Case 4:DMD "", "", "PartyFrog", eNone, eNone, eBlinkFast, 1000, True, ""
            Case 5:DMD "", "", "PartyRobo", eNone, eNone, eBlinkFast, 1000, True, ""
            Case 6:DMD "", "", "PartyAyla", eNone, eNone, eBlinkFast, 1000, True, ""
            Case 7:DMD "", "", "PartyMagus", eNone, eNone, eBlinkFast, 1000, True, ""
        End Select
        UpdateBallType
    End If
	If BallType = 7 And UltWeapons = 1 And Lavos1stForm = True And Lavos = False Then
		PlaySound "CT_Lavos"
		StopCatchem_Timer
		HoleLight6.State = 2
        PokemonLevel = 4
        UpdatePokemonLevel		
	End If
End Sub

Sub ReduceBallType()
    If BallType> 1 Then
'        BallType = BallType - 1
        UpdateBallType
    End If
End Sub

Sub UpdateBallType()
    Select Case BallType
        Case 1:BallLight1.State = 1:BallLight2.State = 0:BallLight3.State = 0:BallLight4.State = 0:BallLight5.State = 0:BallLight6.State = 0:BallLight7.State = 0  
        Case 2:BallLight1.State = 1:BallLight2.State = 1:BallLight3.State = 0:BallLight4.State = 0:BallLight5.State = 0:BallLight6.State = 0:BallLight7.State = 0 
        Case 3:BallLight1.State = 1:BallLight2.State = 1:BallLight3.State = 1:BallLight4.State = 0:BallLight5.State = 0:BallLight6.State = 0:BallLight7.State = 0 
        Case 4:BallLight1.State = 1:BallLight2.State = 1:BallLight3.State = 1:BallLight4.State = 1:BallLight5.State = 0:BallLight6.State = 0:BallLight7.State = 0 
        Case 5:BallLight1.State = 1:BallLight2.State = 1:BallLight3.State = 1:BallLight4.State = 1:BallLight5.State = 1:BallLight6.State = 0:BallLight7.State = 0 
        Case 6:BallLight1.State = 1:BallLight2.State = 1:BallLight3.State = 1:BallLight4.State = 1:BallLight5.State = 1:BallLight6.State = 1:BallLight7.State = 0 
        Case 7:BallLight1.State = 1:BallLight2.State = 1:BallLight3.State = 1:BallLight4.State = 1:BallLight5.State = 1:BallLight6.State = 1:BallLight7.State = 1 
    End Select
'    UpdateBallImage

	If BallType = 7 And Gamewon = False And Lavos2ndForm = False And Lavos3rdForm = False Then 
		Lavos1stForm = True
	End If
End Sub

'Sub UpdateBallImage()
'    Dim BOT, b
'    BOT = GetBalls
    ' exit the Sub if no balls on the table
'    If UBound(BOT) = -1 Then Exit Sub

    ' change the image for each ball
'    For b = 0 to UBound(BOT)
'        Select Case BallType
'            Case 1:BOT(b).FrontDecal = "ball1"
'            Case 2:BOT(b).FrontDecal = "ball2"
'            Case 3:BOT(b).FrontDecal = "ball3"
'            Case 4:BOT(b).FrontDecal = "ball4"
'            Case 5:BOT(b).FrontDecal = "ball5"
'            Case 6:BOT(b).FrontDecal = "ball6"
'            Case 7:BOT(b).FrontDecal = "ball7"
'        End Select
'    Next
'End Sub

' Coin Frenzy
Dim bcoinfrenzy, coinstep

Sub CheckCoinFrenzy()
    If bcoinfrenzy = False Then
        If coinstep> 2 Then
            StartCoinFrenzy
        End If
    End If
End Sub

Sub StartCoinFrenzy()
    bcoinfrenzy = True
    CoinFrenzyTimer.Enabled = True
    StopCoinFrenzyTimer.Enabled = True
    CoinFrenzyTimer_Timer
End Sub

Sub CoinFrenzyTimer_Timer() 'blink one catch light for 5 seconds and then change to another light simulating the pokemon movement
    Dim i
    For each i in aCoinFrenzyLights
        i.State = 0
    Next
    i = INT(RND * 5)
    aCoinFrenzyLights(i).State = 2
End Sub

Sub StopCoinFrenzyTimer_Timer() 'reset lights & variables
    Dim i
    CoinFrenzyTimer.Enabled = False
    StopCoinFrenzyTimer.Enabled = False
    For each i in aCoinFrenzyLights
        i.State = 0
    Next
    bcoinfrenzy = False
    coinstep = 0
End Sub

'***********************************
'Venusaur Cave: Lock (software lock)
'***********************************

Dim bLockEnabled, LockedBalls

Sub VenusaurHole_Hit()
'    PlaySoundAt "fx_kicker_enter", VenusaurHole
	SoundSaucerLock
    If Not Tilted Then
        If bcoinfrenzy AND CoinLight4.State = 2 Then
            Addcoin 25
            PlaySound "fx_coins"
            FlashForMs Flasher9, 1000, 50, 0
            FlashForMs Flasher10, 1000, 50, 0
            CoinFrenzyTimer_Timer
        End If
        If bCatchemMode Then
            If CatchLight3.State = 2 Then
                Addscore 10000 * PokemonLevel
                CatchHits = CatchHits + 1
				PlaySound "CT_ATKC"
                CheckCatchHits
            End If
        Else
            Addscore 10000
        End If
        If bLockEnabled Then
            LockedBalls = LockedBalls + 1
			bLockEnabled = False
			LockLight.State = 0
			LockLight1.State = 0
			LockLight2.State = 0
            DMD "_", CL(1, "BALL " & LockedBalls & " IS LOCKED"), "bkempty", eNone, eNone, eNone, 1000, True, "CT_BallLock"
            If LockedBalls = 3 Then
                VenusaurMultiball
            End IF
        End If
    End If
    vpmtimer.addtimer 1500, "VenusaurKickBall '"
    ' remember last trigger hit by the ball
    LastSwitchHit = "VenusaurHole"
End Sub

Sub VenusaurKickBall()
    PlaySoundAt SoundFXDOF("", 129, DOFPulse, DOFContactors), VenusaurHole
RandomSoundBallRelease VenusaurHole
    DOF 121, DOFPulse
'    AnimateVenusaur
    VenusaurHole.Kick 175, 7
End Sub

Sub VenusaurLock1_Hit()
    PlaySoundAt SoundFXDOF("", 128, DOFPulse, DOFTargets), VenusaurLock1
    If Tilted Then Exit Sub
'    PlaySoundEffect
    Addscore 20000
'    VenusaurShake
    LockLight1.State = 1
    CheckLock
End Sub

Sub VenusaurLock2_Hit()
    PlaySoundAt SoundFXDOF("", 128, DOFPulse, DOFTargets), VenusaurLock2
    If Tilted Then Exit Sub
'    PlaySoundEffect
    Addscore 20000
'    VenusaurShake
    LockLight2.State = 1
    CheckLock
End Sub

Sub CheckLock()
    
	If LockedBalls = 2 And LockLight1.State + LockLight2.State = 2 Then
            MultiLight1.State = 2
            MultiLight2.State = 2
            MultiLight3.State = 2
            MultiLight4.State = 2
            MultiLight5.State = 2
    End If
    
	If bLockEnabled Then Exit Sub
    If LockLight1.State + LockLight2.State = 2 Then
        bLockEnabled = True
        LockLight.State = 2
'        DMD "", "", "lockislit", eNone, eNone, eBlinkFast, 1250, True, ""
    End If
End Sub

Sub VenusaurMultiball()
    DMD "", "", "multiball", eNone, eNone, eBlinkFast, 1250, True, ""
'    PlayFanfare
	LavosMB = True
    AddMultiball 2
	EnableBallSaver 10
    ' turn off the lock lights
    LockedBalls = 0
    bLockEnabled = False
    MultiLight1.State = 0
    MultiLight2.State = 0
    MultiLight3.State = 0
    MultiLight4.State = 0
    MultiLight5.State = 0
    LockLight1.State = 0
    LockLight2.State = 0
    LockLight.State = 0
    'Turn On the Super Jackpot lights
    CenterRampLight.State = 2
    RightRampLight.State = 2
    SetLightColor LightArrow4, "red", 2
    SetLightColor LightArrow2, "red", 2
End Sub

Sub ResetJackpotLights()
    CenterRampLight.State = 0
    RightRampLight.State = 0
    SetLightColor LightArrow4, "white", 0
    SetLightColor LightArrow2, "white", 0
    LightArrow4.State = 0
    LightArrow2.State = 0
End Sub

Dim VenusaurPos

'Sub AnimateVenusaur()
'    VenusaurPos = 0
'    VenusaurTimer.Enabled = 1
'End Sub

'Sub VenusaurTimer_Timer()
'    Select Case VenusaurPos
'        Case 0:Venusaur.TransY = 40:VenusaurPos = 1
'        Case 1:Venusaur.TransY = 20:VenusaurPos = 2
'        Case 2:Venusaur.TransY = 10:VenusaurPos = 3
'        Case 3:Venusaur.TransY = 5:VenusaurPos = 4
'        Case 4:Venusaur.TransY = 0:VenusaurTimer.Enabled = 0
'    End Select
'End Sub

'***************
' Ramp Switches
'***************

Sub RightRampDone_Hit
    If Tilted Then Exit Sub
    If LastSwitchHit = "LeftInlaneTrigger1" Then 'combo
        AddScore 20000
    End If
    If bcoinfrenzy AND CoinLight5.State = 2 Then
        Addcoin 25
        PlaySound "fx_coins"
        FlashForMs Flasher9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0
        CoinFrenzyTimer_Timer
    End If
    If bRampBonus Then
        AwardJackpot
    End If
    If bMultiBallMode Then
        If CenterRampLight.State = 2 Then
            AwardSuperJackpot
        Else
            AwardJackpot
        End If
    End If
    If PikaMultiballLight.State = 2 Then
		Spekkio = True
        DMD "", "", "a-multiball", eNone, eNone, eBlink, 1000, True, "vo_pika7"
        AddMultiball 2
		EnableBallSaver 10
        PikaMultiballLight.State = 0
    End If
    If bCatchemMode Then
        If CatchLight4.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
			PlaySound "CT_ATKF"
            CheckCatchHits
        End If
    Else
        Addscore 10000
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightRampDone"
End Sub

Sub CenterRampDone_Hit
    If Tilted Then Exit Sub
    If ExtraBallLight.State = 2 Then
        AwardExtraBall
        ExtraBallLight.State = 0
    End If
    If bcoinfrenzy AND CoinLight3.State = 2 Then
        Addcoin 25
        PlaySound "fx_coins"
        CoinFrenzyTimer_Timer
        FlashForMs Flasher9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0
    Else
        coinstep = coinstep + 1
        CheckCoinFrenzy
    End If
    If bRampBonus Then
        AwardJackpot
    End If
    If bMultiBallMode Then
        If CenterRampLight.State = 2 Then
            AwardSuperJackpot
        Else
            AwardJackpot
        End If
    End If
    If bCatchemMode Then
        If CatchLight6.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
			PlaySound "CT_ATKR"
            CheckCatchHits
        End If
    Else
        Addscore 10000
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "CenterRampDone"
End Sub

'*****************
'Ramp Bonus Mode
'*****************
' only availabe trough the slot machine

Dim bRampBonus

Sub StartRampBonus()
    bRampBonus = True
    SetLightColor LightArrow4, "green", 2
    SetLightColor LightArrow2, "green", 2
    StopRampBonusTimer.Enabled = True
End Sub

Sub StopRampBonusTimer_Timer()
    bRampBonus = FALSE
    SetLightColor LightArrow4, "white", 0
    SetLightColor LightArrow2, "white", 0
    StopRampBonusTimer.Enabled = False
End Sub

'*****************
' Loop Bonus Mode
'*****************
' only availabe trough the slot machine

Dim bLoopBonus

Sub StartLoopBonus()
    bLoopBonus = True
    SetLightColor LightArrow1, "blue", 2
    SetLightColor LightArrow6, "blue", 2
    StopLoopBonusTimer.Enabled = True
End Sub

Sub StopLoopBonusTimer_Timer()
    bLoopBonus = FALSE
    SetLightColor LightArrow1, "white", 0
    SetLightColor LightArrow6, "white", 0
    StopLoopBonusTimer.Enabled = False
End Sub

Sub LoopTrigger_Hit()
    If bLoopBonus Then
        AwardJackpot
        FlashForMs Flasher9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0
        GiEffect 1
    End If
    LastSwitchHit = "LoopTrigger"
End Sub

'*******************
'Pokemon definitions
'*******************

Dim Eggs, Pokemon
Eggs = Array("WeapBF", "WeapDS", "WeapMasa", "WeapRain", "WeapTA", "WeapValk", "WeapWond") 'Ultimate Weapons
Pokemon = Array("Enemy001", "Enemy002", "Enemy003", "Enemy004", "Enemy005", "Enemy006", "Enemy007", "Enemy008", "Enemy009", "Enemy010", "Enemy011", "Enemy012", "Enemy013", "Enemy014", _
	"Enemy015", "Enemy016", "Enemy017", "Enemy018", "Enemy019", "Enemy020", "Enemy021", "Enemy022", "Enemy023", "Enemy024", "Enemy025", "Enemy026", "Enemy027", "Enemy028", "Enemy029", _
	"Enemy030", "Enemy031", "Enemy032", "Enemy033", "Enemy034", "Enemy035", "Enemy036", "Enemy037", "Enemy038", "Enemy039", "Enemy040", "Enemy041", "Enemy042", "Enemy043", "Enemy044", _
    "Enemy045", "Enemy046", "Enemy047", "Enemy048", "Enemy049", "Enemy050", "Enemy051", "Enemy052", "Enemy053", "Enemy054", "Enemy055", "Enemy056", "Enemy057", "Enemy058", "Enemy059", _
	"Enemy060", "Enemy061", "Enemy062", "Enemy063", "Enemy064", "Enemy065", "Enemy066", "Enemy067", "Enemy068", "Enemy069", "Enemy070", "Enemy071", "Enemy072", "Enemy073", "Enemy074", _
	"Enemy075", "Enemy076", "Enemy077", "Enemy078", "Enemy079", "Enemy080", "Enemy081", "Enemy082", "Enemy083", "Enemy084", "Enemy085", "Enemy086", "Enemy087", "Enemy088", "Enemy089", _
	"Enemy090", "Enemy091", "Enemy092", "Enemy093", "Enemy094", "Enemy095", "Enemy096", "Enemy097", "Enemy098", "Enemy099", "Enemy100", "Enemy101", "Enemy102", "Enemy103", "Enemy104", _
	"Enemy105", "Enemy106", "Enemy107", "Enemy108", "Enemy109", "Enemy110", "Enemy111", "Enemy112", "Enemy113", "Enemy114", "Enemy115", "Enemy116", "Enemy117", "Enemy118", "Enemy119", _
	"Enemy120", "Enemy121", "Enemy122", "Enemy123", "Enemy124", "Enemy125", "Enemy126", "Enemy127", "Enemy128", "Enemy129", "Enemy130", _
	"Boss001", "Boss002", "Boss003", "Boss004", "Boss005", "Boss006", "Boss007", "Boss008", "Boss009", "Boss010", "Boss011", "Boss012", "Boss013", "Boss014", "Boss015", "Boss016", "Boss017", "Boss018", "Boss019", "Boss020", "Boss021", "Boss022", "Boss023", _
	"BBoss001", "BBoss002", "BBoss003", "BBoss004", "BBoss005", "BBoss006", "BBoss007", "BBoss008", "BBoss009", "BBoss010", "BBoss011", "BBoss012", "BBoss013", "BBoss014", "BBoss015", "BBoss016", "BBoss017", "BBoss018", "BBoss019", "BBoss020", "Secret")		' all the game's enemies, Magus, can only be caught from the slotmachine '151

'***************************
' Catch Hole /Catch'em Mode
'***************************

' Start always first the Cath'em mode if the light is blinking other wise gives a random award if one of the other lights are blinking.
' Catch'em mode ends after 2 minutes

Dim bCatchemMode, CatchID, CatchMaxHits, CatchHits, BallInHole

Dim aBall

Sub CatchHole_Timer
    Do While aBall.Z> 0
        aBall.Z = aBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
End Sub

Sub CatchHole_Hit
'    PlaySoundAt "fx_hole_enter", CatchHole
	SoundSaucerLock
    BallInHole = BallInHole + 1
    Set aBall = ActiveBall:Me.TimerEnabled = 1
    If HoleLight6.State = 2 Then 'Start catchem mode
        StartCatchem1
        vpmtimer.addtimer 2000, "FlashForMs FlasherExitHole, 1000, 30, 0 '"
        vpmtimer.addtimer 2500, "CatchHoleExit '"
        Exit Sub
    End If
    If HolePos> 0 Then
        StartSlotmachine
    Else
        vpmtimer.addtimer 500, "FlashForMs FlasherExitHole, 1000, 30, 0 '"
        vpmtimer.addtimer 1000, "CatchHoleExit '"
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "CatchHole"
End Sub

Sub CatchHoleExit()
    If BallInHole> 0 Then
        BallInHole = BallInHole - 1
        CatchHole.CreateSizedball BallSize / 2
'        UpdateBallImage
        PlaySoundAt SoundFXDOF("", 120, DOFPulse, DOFContactors), CatchHole
        DOF 121, DOFPulse
		SoundSaucerKick 1, CatchHole
        CatchHole.Kick 175, 14, 1
    End If
End Sub

Sub StartCatchem1()
    bCatchemMode = True
    CatchID = 0

If 	UltWeapons = 1 And Lavos1stForm = True Then
	LavosBattle1
Else If UltWeapons = 1 And Lavos2ndForm = True Then
	LavosBattle2
Else If UltWeapons = 1 And Lavos3rdForm = True Then
	LavosBattle3

Else
    Select Case PokemonLevel                  'limit the numbers of pokemons based on the level
        Case 2:CatchID = INT(RND * 127)       ' CatchID the number in the reel of the pokemon to be caught, this are level 2 Pokémon
        Case 3:CatchID = INT(RND * 26) + 127  'level 3 Pokémon
        Case 4:CatchID = INT(RND * 17) + 153  'level 4 Pokémon
    End Select
    ChangeSong                                ' change the song
'    debug.print catchid
    CatchMaxHits = PokemonLevel - UltWeapons
    StartCatchem2
End If
End If
End If
End Sub

Sub StartCatchem2()
	If CatchID <= 130 Then
		DMD RL(1, "BATTLE START"), "", pokemon(CatchID), eNone, eNone, eNone, 1000, True, "CT_Weapon"
	Else If CatchID <= 150 Then
			DMD RL(1, "BOSS BATTLE"), "", pokemon(CatchID), eNone, eNone, eNone, 1000, True, "CT_Weapon2"
		Else
				DMD RL(1, "ULT BATTLE"), "", pokemon(CatchID), eNone, eNone, eNone, 1000, True, "CT_Weapon"
		End If
	End If
			
    CatchHits = 0
    CatchemTimer.Enabled = True ' blink one light
    StopCatchem.Enabled = True  ' turn on the timer to stop the mode
    CatchemTimer_Timer
    ' turn off lights
    HoleLight6.State = 0:l53.State = 2
End Sub

Sub CatchemTimer_Timer() 'blink one catch light for 5 seconds and then change to another light simulating the pokemon movement
    Dim i
    For each i in aCatchLights
        i.State = 0
    Next
    i = INT(RND * 6)
    aCatchLights(i).State = 2
	Playsound "CT_Ready"

		If CatchLight1.State = 2 Then
			SetLightColor LightArrow6, "red", 2 
		Else If CatchLight2.State = 2 Then
			SetLightColor LightArrow5, "red", 2
		Else If CatchLight6.State = 2 Then
			SetLightColor LightArrow4, "red", 2
		Else If CatchLight3.State = 2 Then
			SetLightColor LightArrow3, "red", 2
		Else If CatchLight4.State = 2 Then
			SetLightColor LightArrow2, "red", 2
		Else If CatchLight5.State = 2 Then
			SetLightColor LightArrow1, "red", 2
		End If
		End If
		End If
		End If
		End If
		End If

End Sub

Sub StopCatchem_Timer() 'reset lights & variables
    Dim i
    CatchemTimer.Enabled = False
    StopCatchem.Enabled = False
    For each i in aCatchLights
        i.State = 0

			SetLightColor LightArrow6, "white", 0 
			SetLightColor LightArrow5, "white", 0
			SetLightColor LightArrow4, "white", 0
			SetLightColor LightArrow3, "white", 0
			SetLightColor LightArrow2, "white", 0
			SetLightColor LightArrow1, "white", 0


    Next
    bCatchemMode = False
	Magus = False
    ChangeSong
    ResetPokemonLevel
End Sub

Sub CheckCatchHits()
    GiEffect 1
'    PlaySoundEffect
	If CatchHits >= CatchMaxHits Then 'stop catch'em mode
'        PlayFanfare

		If CatchHits - UltWeapons <= 5 And Magus = True Then
			AwardExtraBall
			Magus = False
			PlaySound "po_fanfare2"
		End If

	If UltWeapons = 0 Then
        DMD RL(1, "DEFEATED"), "", Pokemon(CatchID), eBlinkFast, eBlinkFast, eNone, 1500, True, ""
			If CatchHits = 2 Or CatchHits - UltWeapons <= 1 Then
				DMD "     BATTLE WON", "     FOUND 25GP", "", eNone, eBlinkFast, eNone, 1500, True, ""
				PlaySound "po_fanfare3"
				AddCoin 25
			Else If CatchHits = 3 Or CatchHits - UltWeapons <= 2 Then
				DMD "     BATTLE WON", "     FOUND 50GP", "", eNone, eBlinkFast, eNone, 1500, True, ""
				PlaySound "po_fanfare1"
				AddCoin 50
			Else If CatchHits = 4 And EggID(CurrentPlayer) < 6 Or CatchHits - UltWeapons <= 8 And EggID(CurrentPlayer) < 6 Then
				EggID(CurrentPlayer) = EggID(CurrentPlayer) + 1
				EggBonusCount = EggBonusCount + 1
				DMD "", RL(1, "ACQUIRED "), Eggs(EggID(CurrentPlayer)), eNone, eBlinkFast, eNone, 1500, True, ""
				DMD "     BATTLE WON", "     FOUND 75GP", "", eNone, eBlinkFast, eNone, 1500, True, ""
				PlaySound "po_fanfare2"	
				AddCoin 75
				Weapons(CurrentPlayer, EggBonus(CurrentPlayer) ) = EggID(CurrentPlayer)
				EggBonus(CurrentPlayer) = EggBonus(CurrentPlayer) + 1
					If EggBonus(CurrentPlayer) MOD 7 = 0 Then 'start the Charizard Bonus Mode after hatching 3 eggs
						UltWeapons = 1
						DMD "  ALL WEAPONS FOUND", "  ENEMY HP REDUCED", "", eNone, eBlinkFast, eNone, 2000, True, ""
					End If
			End If
			End If
			End If
	Else If UltWeapons = 1 Then
        DMD RL(1, "DEFEATED"), "", Pokemon(CatchID), eBlinkFast, eBlinkFast, eNone, 1500, True, ""
			If CatchHits = 1  Then
				DMD "     BATTLE WON", "     FOUND 25GP", "", eNone, eBlinkFast, eNone, 1500, True, ""
				PlaySound "po_fanfare3"
				AddCoin 25
			Else If CatchHits = 2 Then
				DMD "     BATTLE WON", "     FOUND 50GP", "", eNone, eBlinkFast, eNone, 1500, True, ""
				PlaySound "po_fanfare1"
				AddCoin 50
			Else If CatchHits = 3 Then
				DMD "     BATTLE WON", "     FOUND 75GP", "", eNone, eBlinkFast, eNone, 1500, True, ""
				PlaySound "po_fanfare2"	
				AddCoin 75
			End If
			End If
			End If
	End If
	End If

        Addscore 50000 * pokemonlevel
		EnemiesCount = EnemiesCount + 1
        PokemonBonusAward(CurrentPlayer) = PokemonBonusAward(CurrentPlayer) + (pokemonlevel * pokemonlevel)
        PokemonBonus(CurrentPlayer) = PokemonBonus(CurrentPlayer) + 1
        PokemonCaught(CurrentPlayer, PokemonBonus(CurrentPlayer) ) = CatchID
        LightEffect 1
        GiEffect 1
        If PokemonBonus(CurrentPlayer) MOD 3 = 0 Then 'turn on the extra ball light after catching 3 pokemons
            ExtraBallLight.State = 2
        End If
		
			If CatchHits = 5 And Lavos1stForm = True Then
				Lavos1stForm = False
				Lavos2ndForm = True
				StopCatchem_Timer
				StartCatchem1
				Exit Sub

			Else If CatchHits = 6 Then
				Lavos2ndForm = False
				Lavos3rdForm = True
				StopCatchem_Timer
				StartCatchem1
				Exit Sub

			Else If CatchHits = 7 Then
				Lavos1stForm = False
				Lavos2ndForm = False
				Lavos3rdForm = False
				StopCatchem_Timer
				Gamewon = True
				EnableBallSaver 30
'				BallType = 1 ' Resets Lavos party counter
				AddScore 10000000
				IncrementBonusMultiplier

			End If
			End If
			End If
        StopCatchem_Timer
    Else 'change to another position
        CatchemTimer.Enabled = 0
        CatchemTimer.Enabled = 1
        CatchemTimer_Timer
	End If
End Sub

Sub CatchMewtwo()

	If (Lavos or Magus) = True Then
		Exit Sub
	Else
		StopCatchem_Timer
		bCatchemMode = True
		CatchID = 173 'the number of Magus in the reel
		PlaySong "CTMagus"
		PokemonLevel = 5
		CatchMaxHits = 5 - UltWeapons
		Magus = True
		StartCatchem2
	End If

End Sub

Sub LavosBattle1()
	Lavos = True
    bCatchemMode = True
    CatchID = 170 'Lavos 1st Form
	PlaySong "CTLavos1"
    PokemonLevel = 6
    CatchMaxHits = 6 - UltWeapons
    StartCatchem2
End Sub

Sub LavosBattle2()
    bCatchemMode = True
    CatchID = 171 'Lavos 2nd Form
	PlaySong "CTLavos2"
    PokemonLevel = 7
    CatchMaxHits = 7 - UltWeapons
    StartCatchem2
End Sub

Sub LavosBattle3()
    bCatchemMode = True
    CatchID = 172 'Lavos Final Form
	PlaySong "CTLavos3"
    PokemonLevel = 8
    CatchMaxHits = 8 - UltWeapons
    StartCatchem2
End Sub

'**************
' SlotMachine
'**************

Dim BulbasaurAward, PikachuAward, SquirtleAward, CharmanderAward, MarleAward

BulbasaurAward = Array("d-smallpoints", "d-bigpoints", "d-30sec", "d-chestlight", "d-smallpoints", "d-50gp", "d-chestlight", "d-coinfrenzy", "d-bigpoints", "d-targetsbonus") 'Robo
PikachuAward = Array("a-smallpoints", "a-bigpoints", "a-30sec", "a-chestlight", "a-smallpoints", "a-50gp", "a-chestlight", "a-coinfrenzy", "a-multiball", "a-magus") ' Frog
SquirtleAward = Array("e-smallpoints", "e-bigpoints", "e-30sec", "e-chestlight", "e-smallpoints", "e-50gp", "e-chestlight", "e-coinfrenzy", "e-bigpoints", "e-100gp") 'Lucca
CharmanderAward = Array("b-smallpoints", "b-bigpoints", "b-30sec", "b-chestlight", "b-smallpoints", "b-50gp", "b-chestlight", "b-coinfrenzy", "b-rampbonus", "b-special") ' Ayla
MarleAward = Array("c-smallpoints", "c-bigpoints", "c-30sec", "c-chestlight", "c-smallpoints", "c-50gp", "c-chestlight", "c-coinfrenzy", "c-loopbonus", "c-special") '  Marle


Sub StartSlotmachine() ' uses the HolePos variable

    Dim i
    HoleBonus = HoleBonus + 1
    DMDFlush

	If (Lavos1stForm or Lavos2ndForm or Lavos3rdForm or Magus) = True Then

    Select Case HolePos
        Case 1: 'Robo Award
            For i = 0 to 8
                DMD "", "", BulbasaurAward(i), eNone, eNone, eNone, 50, False, "CT_Select"
            Next
        Case 2: 'Frog Award
            For i = 0 to 7
                DMD "", "", PikachuAward(i), eNone, eNone, eNone, 50, False, "CT_Select"
            Next
        Case 3: 'SquirtleAward
            For i = 0 to 8
                DMD "", "", SquirtleAward(i), eNone, eNone, eNone, 50, False, "CT_Select"
            Next
        Case 4: 'Ayla Award
            For i = 0 to 8
                DMD "", "", CharmanderAward(i), eNone, eNone, eNone, 50, False, "CT_Select"
            Next
        Case 5: 'Marle Award
            For i = 0 to 8
                DMD "", "", MarleAward(i), eNone, eNone, eNone, 50, False, "CT_Select"
            Next
    End Select

	Else

    Select Case HolePos
        Case 1: 'Robo Award
            For i = 0 to 8
                DMD "", "", BulbasaurAward(i), eNone, eNone, eNone, 50, False, "CT_Select"
            Next
        Case 2: 'Frog Award
            For i = 0 to 8
                DMD "", "", PikachuAward(i), eNone, eNone, eNone, 50, False, "CT_Select"
            Next
        Case 3: 'SquirtleAward
            For i = 0 to 8
                DMD "", "", SquirtleAward(i), eNone, eNone, eNone, 50, False, "CT_Select"
            Next
        Case 4: 'Ayla Award
            For i = 0 to 8
                DMD "", "", CharmanderAward(i), eNone, eNone, eNone, 50, False, "CT_Select"
            Next
        Case 5: 'Marle Award
            For i = 0 to 8
                DMD "", "", MarleAward(i), eNone, eNone, eNone, 50, False, "CT_Select"
            Next
    End Select


	End If


    DOF 142, DOFPulse
    vpmtimer.AddTimer 2500, "GiveSlotAward '"
End Sub

Sub GiveSlotAward()
    Dim tmp
    DMDFlush
    tmp = INT(RND * 10)

    Select Case HolePos
        Case 1: 'BulbasaurAward
            DMD "", "", BulbasaurAward(tmp), eNone, eNone, eBlinkFast, 800, True, "po_fanfare3"
        Case 2: 'PikachuAward
            DMD "", "", PikachuAward(tmp), eNone, eNone, eBlinkFast, 800, True, "po_fanfare3"
        Case 3: 'SquirtleAward
            DMD "", "", SquirtleAward(tmp), eNone, eNone, eBlinkFast, 800, True, "po_fanfare3"
        Case 4: 'CharmanderAward
            DMD "", "", CharmanderAward(tmp), eNone, eNone, eBlinkFast, 800, True, "po_fanfare3"
        Case 5: 'MarleAward
            DMD "", "", MarleAward(tmp), eNone, eNone, eBlinkFast, 800, True, "po_fanfare3"
    End Select

    Select Case tmp
        Case 0:AddScore INT(10000 * RND * 9)  'small points
        Case 1:AddScore INT(100000 * RND * 9) 'big points
        Case 2:EnableBallSaver 15             'ball saver 30 seconds
        Case 3:WeaponLight  				  'weapons light
        Case 4:AddScore INT(10000 * RND * 9)  'small points
        Case 5:AddCoin 50					  'upgrade ball
        Case 6:WeaponLight  				  'weapons light
        Case 7:StartCoinFrenzy                'coin frenzy
        Case 8
            Select Case HolePos
                Case 1:AddScore INT(100000 * RND * 9) 'big points
                Case 2:PikaMultiballLight.State = 2   'enable picachu multiball
                Case 3:AddScore INT(100000 * RND * 9) 'big points
                Case 4:StartRampBonus              'Ramp Bonus Mode - Activate Jackpots on the ramps for 2 minutes
				Case 5:StartLoopBonus			   'Loops Bonus
            End Select
        Case 9
            Select Case HolePos
                Case 1:StartTargetMode          'Target Bonus Mode
                Case 2:CatchMewtwo              'Magus Fight
                Case 3:AddCoin 100
                Case 4:ExtraBallLight.State = 2 'Lit Extra Ball
				Case 5:HatchLight1.State = 1:HatchLight2.State = 1:HatchLight3.State = 1:HatchLight4.State = 1:CheckEggTargets
            End Select
    End Select
    GiEffect 1
    vpmtimer.addtimer 1500, "CatchHoleExit '"
End Sub

'**********************
' Charizard Bonus Mode
'**********************

Dim bCharizardMode

'Sub StartCharizardMode()
'    bCharizardMode = True
'    EggTarget5.IsDropped = FALSE
'    DOF 119, DOFPulse
'    SetLightColor LightArrow5, "orange", 2
'    StopCharizardTimer.Enabled = True
'End Sub

'Sub StopCharizardTimer_Timer()
'    StopCharizardTimer.Enabled = False
'    bCharizardMode = False
'    SetLightColor LightArrow5, "white", 0
'End Sub

'******************
' Egg/Hatch Mode
'******************

Dim bEggTargetsCompleted

Sub EggTarget1_Hit()
    PlaySoundAt SoundFXDOF("", 117, DOFPulse, DOFTargets), EggTarget1
    If Tilted Then Exit Sub
'    PlaySoundEffect
    HatchLight1.State = 1:HatchLight1b.Visible = 1
    Addscore 20000
    CheckEggTargets
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget1"
End Sub

Sub EggTarget2_Hit()
    PlaySoundAt SoundFXDOF("", 117, DOFPulse, DOFTargets), EggTarget2
    If Tilted Then Exit Sub
'    PlaySoundEffect
    HatchLight2.State = 1:HatchLight2b.Visible = 1
    Addscore 20000
    CheckEggTargets
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget2"
End Sub

Sub EggTarget3_Hit()
    PlaySoundAt SoundFXDOF("", 117, DOFPulse, DOFTargets), EggTarget3
    If Tilted Then Exit Sub
'    PlaySoundEffect
    HatchLight3.State = 1:HatchLight3b.Visible = 1
    Addscore 20000
    CheckEggTargets
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget3"
End Sub

Sub EggTarget4_Hit()
    PlaySoundAt SoundFXDOF("", 117, DOFPulse, DOFTargets), EggTarget4
    If Tilted Then Exit Sub
'    PlaySoundEffect
    HatchLight4.State = 1:HatchLight4b.Visible = 1
    Addscore 20000
    CheckEggTargets
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget4"
End Sub

Sub EggTarget5_Hit()
    PlaySoundAt SoundFXDOF("CT_Door", 117, DOFPulse, DOFTargets), EggTarget5
    If Tilted Then Exit Sub
'    PlaySoundEffect
'    CharizardShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        AddScore 30000
        LightEffect 1
        GiEffect 1
    End If
    If bCharizardMode Then
        GiEffect 1
        AddScore 500000
    Else
        Addscore 20000
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget5"
End Sub

Sub EggTarget5_Dropped()
    If bCharizardMode Then
        EggTarget5.Isdropped = False
        DOF 119, DOFPulse
    End If
End Sub

Sub CheckEggTargets()
'    If bEggTargetsCompleted = False Then
        If(HatchLight1.State + HatchLight2.State + HatchLight3.State + HatchLight4.State) => 4 Then
            EggMagnet.MagnetOn = True
			bEggTargetsCompleted = True
            HatchReadyLight.State = 2
        End If
 '   End If
End Sub

Sub EggHole_Hit()
'    PlaySoundAt "fx_hole_enter", EggHole
	SoundSaucerLock
    BallInHole = BallInHole + 1
    EggHole.DestroyBall

	If EggID(CurrentPlayer) < 6 Then
		If bEggTargetsCompleted Then
			EggTarget5.isDropped = 0
			EggID(CurrentPlayer) = EggID(CurrentPlayer) + 1
			EggBonusCount = EggBonusCount + 1
			DMD "", RL(1, "ACQUIRED "), Eggs(EggID(CurrentPlayer)), eNone, eBlinkFast, eNone, 1500, True, ""
			Weapons(CurrentPlayer, EggBonus(CurrentPlayer) ) = EggID(CurrentPlayer)
			PlaySound "po_fanfare3"
			AddScore 1000000

		If EggID(CurrentPlayer) = 6 Then
			DMD "  ALL WEAPONS FOUND", "  ENEMY HP REDUCED", "", eNone, eBlinkFast, eNone, 2000, True, ""
		End If

			EggBonus(CurrentPlayer) = EggBonus(CurrentPlayer) + 1
			If EggBonus(CurrentPlayer) MOD 7 = 0 Then 'start the Charizard Bonus Mode after hatching 3 eggs
				UltWeapons = 1
			End If
			HatchReadyLight. State = 0
			FlashForMs HatchLight1b, 1000, 50, 0
			FlashForMs HatchLight2b, 1000, 50, 0
			FlashForMs HatchLight3b, 1000, 50, 0
			FlashForMs HatchLight4b, 1000, 50, 0
			FlashForMs Flasher9, 1000, 50, 0
			FlashForMs Flasher10, 1000, 50, 0
			Gieffect 1
			vpmtimer.Addtimer 2500, "EndEggMode '"
		Else
			vpmtimer.addtimer 1250, "CatchHoleExit '"
			PlaySound "CT_Sealed"
		End If
	Else
		
			DMD "  ALL WEAPONS FOUND", "  ENEMY HP REDUCED", "", eNone, eBlinkFast, eNone, 1500, True, ""
			AddScore 1000000
			HatchReadyLight. State = 0
			FlashForMs HatchLight1b, 1000, 50, 0
			FlashForMs HatchLight2b, 1000, 50, 0
			FlashForMs HatchLight3b, 1000, 50, 0
			FlashForMs HatchLight4b, 1000, 50, 0
			FlashForMs Flasher9, 1000, 50, 0
			FlashForMs Flasher10, 1000, 50, 0
			Gieffect 1
			vpmtimer.Addtimer 2500, "EndEggMode '"
'			vpmtimer.addtimer 1500, "CatchHoleExit '"
	End If
	If BallType = 7 And UltWeapons = 1 And Lavos1stForm = True And Lavos = False Then
		PlaySound "CT_Lavos"
		StopCatchem_Timer
		HoleLight6.State = 2
		PokemonLevel = 4
        UpdatePokemonLevel
	End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggHole"
End Sub

Sub EndEggMode()
    HatchLight1.State = 0:HatchLight1b.Visible = 0
    HatchLight2.State = 0:HatchLight2b.Visible = 0
    HatchLight3.State = 0:HatchLight3b.Visible = 0
    HatchLight4.State = 0:HatchLight4b.Visible = 0
    bEggTargetsCompleted = False
	EggMagnet.MagnetOn = False
    EggTarget5.IsDropped = 0
    vpmtimer.addtimer 500, "CatchHoleExit '"
End Sub

Sub WeaponLight()

		HatchLight4.State = 1
		HatchLight3.State = 1
		HatchLight2.State = 1
		HatchLight1.State = 1
		CheckEggTargets

End Sub

'*****************
' objects shaking
'*****************

'Dim PikaShake, CharmanShake, ChariShake, VenuShake
'
'Sub PikachuShake()
'    PikaShake = 6
'    PikaShakeTimer.Enabled = True
'End Sub
'
'Sub PikaShakeTimer_Timer()
'    Pikachu.Transz = PikaShake / 2
'    If PikaShake = 0 Then Me.Enabled = False:Exit Sub
'    If PikaShake <0 Then
'        PikaShake = ABS(PikaShake) - 0.1
'    Else
'        PikaShake = - PikaShake + 0.1
'    End If
'End Sub
'
'Sub CharmanderShake()
'    CharmanShake = 6
'    CharmanShakeTimer.Enabled = True
'End Sub
'
'Sub CharmanShakeTimer_Timer()
'    Charmander.Transz = CharmanShake / 2
'    If CharmanShake = 0 Then Me.Enabled = False:Exit Sub
'    If CharmanShake <0 Then
'        CharmanShake = ABS(CharmanShake) - 0.1
'    Else
'        CharmanShake = - CharmanShake + 0.1
'    End If
'End Sub
'
'Sub CharizardShake()
'    DOF 118, DOFPulse
'    ChariShake = 6
'    CharizardShakeTimer.Enabled = True
'End Sub
'
'Sub CharizardShakeTimer_Timer()
'    Charizard.Transy = ChariShake / 2
'    If ChariShake = 0 Then Me.Enabled = False:Exit Sub
'    If ChariShake <0 Then
'        ChariShake = ABS(ChariShake) - 0.1
'    Else
'        ChariShake = - ChariShake + 0.1
'    End If
'End Sub
'
'Sub VenusaurShake()
'    DOF 118, DOFPulse
'    VenuShake = 6
'    VenusaurShakeTimer.Enabled = True
'End Sub
'
'Sub VenusaurShakeTimer_Timer()
'    Venusaur.Transy = VenuShake / 2
'    If VenuShake = 0 Then Me.Enabled = False:Exit Sub
'    If VenuShake <0 Then
'        VenuShake = ABS(VenuShake) - 0.1
'    Else
'        VenuShake = - VenuShake + 0.1
'    End If
'End Sub

'**********************
'Effects only triggers
'**********************

Sub EffectTrigger1_Hit()
    FlashForMs Flasher11, 1000, 50, 0
End Sub

Sub EffectTrigger2_Hit()
    FlashForMs Flasher12, 1000, 50, 0
End Sub



'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR



Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm) 'Rubberizer is handle here
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
			aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)

Dim AB, BC, CD, DA

AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)

BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)

CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)

DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)

If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then

InRect = True

Else

InRect = False

End If
End Function

Sub RDampen_Timer()
Cor.Update
End Sub



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************



'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 80
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -2.7        
'        AddPt "Polarity", 2, 0.33, -2.7
'        AddPt "Polarity", 3, 0.37, -2.7        
'        AddPt "Polarity", 4, 0.41, -2.7
'        AddPt "Polarity", 5, 0.45, -2.7
'        AddPt "Polarity", 6, 0.576,-2.7
'        AddPt "Polarity", 7, 0.66, -1.8
'        AddPt "Polarity", 8, 0.743, -0.5
'        AddPt "Polarity", 9, 0.81, -0.5
'        AddPt "Polarity", 10, 0.88, 0
'
'        addpt "Velocity", 0, 0,         1
'        addpt "Velocity", 1, 0.16, 1.06
'        addpt "Velocity", 2, 0.41,         1.05
'        addpt "Velocity", 3, 0.53,         1'0.982
'        addpt "Velocity", 4, 0.702, 0.968
'        addpt "Velocity", 5, 0.95,  0.968
'        addpt "Velocity", 6, 1.03,         0.945
'
'        LF.Object = LeftFlipper        
'        LF.EndPoint = EndPointLp
'        RF.Object = RightFlipper
'        RF.EndPoint = EndPointRp
'End Sub
'
'
'
''*******************************************
'' Mid 80's
'
'Sub InitPolarity()
'        dim x, a : a = Array(LF, RF)
'        for each x in a
'                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'                x.enabled = True
'                x.TimeDelay = 80
'        Next
'
'        AddPt "Polarity", 0, 0, 0
'        AddPt "Polarity", 1, 0.05, -3.7        
'        AddPt "Polarity", 2, 0.33, -3.7
'        AddPt "Polarity", 3, 0.37, -3.7
'        AddPt "Polarity", 4, 0.41, -3.7
'        AddPt "Polarity", 5, 0.45, -3.7 
'        AddPt "Polarity", 6, 0.576,-3.7
'        AddPt "Polarity", 7, 0.66, -2.3
'        AddPt "Polarity", 8, 0.743, -1.5
'        AddPt "Polarity", 9, 0.81, -1
'        AddPt "Polarity", 10, 0.88, 0
'
'        addpt "Velocity", 0, 0,         1
'        addpt "Velocity", 1, 0.16, 1.06
'        addpt "Velocity", 2, 0.41,         1.05
'        addpt "Velocity", 3, 0.53,         1'0.982
'        addpt "Velocity", 4, 0.702, 0.968
'        addpt "Velocity", 5, 0.95,  0.968
'        addpt "Velocity", 6, 1.03,         0.945
'
'        LF.Object = LeftFlipper        
'        LF.EndPoint = EndPointLp
'        RF.Object = RightFlipper
'        RF.EndPoint = EndPointRp
'End Sub
'
'


'*******************************************
'  Late 80's early 90's

'Sub InitPolarity()
'	dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
'		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'	Next
'
'	AddPt "Polarity", 0, 0, 0
'	AddPt "Polarity", 1, 0.05, -5
'	AddPt "Polarity", 2, 0.4, -5
'	AddPt "Polarity", 3, 0.6, -4.5
'	AddPt "Polarity", 4, 0.65, -4.0
'	AddPt "Polarity", 5, 0.7, -3.5
'	AddPt "Polarity", 6, 0.75, -3.0
'	AddPt "Polarity", 7, 0.8, -2.5
'	AddPt "Polarity", 8, 0.85, -2.0
'	AddPt "Polarity", 9, 0.9,-1.5
'	AddPt "Polarity", 10, 0.95, -1.0
'	AddPt "Polarity", 11, 1, -0.5
'	AddPt "Polarity", 12, 1.1, 0
'	AddPt "Polarity", 13, 1.3, 0
'
'	addpt "Velocity", 0, 0,         1
'	addpt "Velocity", 1, 0.16, 1.06
'	addpt "Velocity", 2, 0.41,         1.05
'	addpt "Velocity", 3, 0.53,         1'0.982
'	addpt "Velocity", 4, 0.702, 0.968
'	addpt "Velocity", 5, 0.95,  0.968
'	addpt "Velocity", 6, 1.03,         0.945
'
'	LF.Object = LeftFlipper        
'	LF.EndPoint = EndPointLp
'	RF.Object = RightFlipper
'	RF.EndPoint = EndPointRp
'End Sub



'
''*******************************************
'' Early 90's and after
'
Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 60
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -5.5
        AddPt "Polarity", 2, 0.4, -5.5
        AddPt "Polarity", 3, 0.6, -5.0
        AddPt "Polarity", 4, 0.65, -4.5
        AddPt "Polarity", 5, 0.7, -4.0
        AddPt "Polarity", 6, 0.75, -3.5
        AddPt "Polarity", 7, 0.8, -3.0
        AddPt "Polarity", 8, 0.85, -2.5
        AddPt "Polarity", 9, 0.9,-2.0
        AddPt "Polarity", 10, 0.95, -1.5
        AddPt "Polarity", 11, 1, -1.0
        AddPt "Polarity", 12, 1.05, -0.5
        AddPt "Polarity", 13, 1.1, 0
        AddPt "Polarity", 14, 1.3, 0

        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
End Sub


' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub




'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)

	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub

	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property

	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
				case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub

	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub

	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Property Get Pos 'returns % position a ball. For debug stuff.
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next                
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function




'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If        
End Function


'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST         
	Flipper.eostorqueangle = EOSA         
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST        
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If

	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************



'**************************************************
' Flipper Collision Subs
'NOTE: COpy and overwrite collision sound from original collision subs over
'RandomSoundFlipper()' below
'**************************************************'

Sub LeftFlipper_Collide(parm)
CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
'RandomSoundFlipper() 'Remove this line if Fleep is integrated
LeftFlipperCollide parm 'This is the Fleep code
End Sub

Sub RightFlipper_Collide(parm)
CheckLiveCatch Activeball, RightFlipper, RFCount, parm
'RandomSoundFlipper() 'Remove this line if Fleep is integrated
RightFlipperCollide parm 'This is the Fleep code
End Sub


' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
	LFLogo.RotZ = LeftFlipper.CurrentAngle
	RFlogo.RotZ = RightFlipper.CurrentAngle
End Sub


'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1                                                                                                                'volume level; range [0, 1]
NudgeLeftSoundLevel = 1                                                                                                        'volume level; range [0, 1]
NudgeRightSoundLevel = 1                                                                                                'volume level; range [0, 1]
NudgeCenterSoundLevel = 1                                                                                                'volume level; range [0, 1]
StartButtonSoundLevel = 0.1                                                                                                'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr                                                                                        'volume level; range [0, 1]
PlungerPullSoundLevel = 1                                                                                                'volume level; range [0, 1]
RollingSoundFactor = 1.1/5                

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010                                                           'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635                                                                'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                                                                        'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                                                                      'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
SlingshotSoundLevel = 0.95                                                                                                'volume level; range [0, 1]
BumperSoundFactor = 4.25                                                                                                'volume multiplier; must not be zero
KnockerSoundLevel = 1                                                                                                         'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2                                                                        'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5                                                                                        'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5                                                                                        'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5                                                                                'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025                                                                        'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025                                                                        'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8                                                                        'volume level; range [0, 1]
WallImpactSoundFactor = 0.075                                                                                        'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5                                                                                                        'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10                                                                                        'volume multiplier; must not be zero
DTSoundLevel = 0.25                                                                                                                'volume multiplier; must not be zero
RolloverSoundLevel = 0.5                                                                      'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                                                                      'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8                                                                                                                'volume level; range [0, 1]
BallReleaseSoundLevel = 1                                                                                                'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2                                                                        'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015                                                                                'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5                                                                                                        'volume multiplier; must not be zero


'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
        PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
        PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
        PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
        PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
        PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
        Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
        Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
        PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
    tmp = tableobj.y * 2 / tableheight-1

        if tmp > 7000 Then
                tmp = 7000
        elseif tmp < -7000 Then
                tmp = -7000
        end if

    If tmp > 0 Then
                AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1

        if tmp > 7000 Then
                tmp = 7000
        elseif tmp < -7000 Then
                tmp = -7000
        end if

    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
        Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
        Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
        Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
        BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
        VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
        PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
        RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
        RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
        PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
        PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
        PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
        PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub


Sub SoundPlungerPull()
        PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, swPlunger
End Sub

Sub SoundPlungerReleaseBall()
        PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, swPlunger      
End Sub

Sub SoundPlungerReleaseNoBall()
        PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, swPlunger
End Sub


'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
        PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
        PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
        PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
        PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
        PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
        PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
        PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
        PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////
Sub SoundSpinner(spinnerswitch)
        PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub


'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
        FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
        PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
        FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
        PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
        FlipperLeftHitParm = parm/10
        If FlipperLeftHitParm > 1 Then
                FlipperLeftHitParm = 1
        End If
        FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
        RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
        FlipperRightHitParm = parm/10
        If FlipperRightHitParm > 1 Then
                FlipperRightHitParm = 1
        End If
        FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
        RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
        PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
        PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
        RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
        dim finalspeed
        finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 5 then                
                RandomSoundRubberStrong 1
        End if
        If finalspeed <= 5 then
                RandomSoundRubberWeak()
        End If        
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
        Select Case Int(Rnd*10)+1
                Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
        End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
        PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
        RandomSoundWall()      
End Sub

Sub RandomSoundWall()
        dim finalspeed
        finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 16 then 
                Select Case Int(Rnd*5)+1
                        Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
                End Select
        End if
        If finalspeed >= 6 AND finalspeed <= 16 then
                Select Case Int(Rnd*4)+1
                        Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
                End Select
        End If
        If finalspeed < 6 Then
                Select Case Int(Rnd*3)+1
                        Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
                End Select
        End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
        PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
        RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
        RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
        dim finalspeed
        finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 16 then 
                PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
        End if
        If finalspeed >= 6 AND finalspeed <= 16 then
                Select Case Int(Rnd*2)+1
                        Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                        Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                End Select
        End If
        If finalspeed < 6 Then
                Select Case Int(Rnd*2)+1
                        Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                        Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                End Select
        End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
        PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
        If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
                RandomSoundBottomArchBallGuideHardHit()
        Else
                RandomSoundBottomArchBallGuide
        End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
        dim finalspeed
        finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 16 then 
                Select Case Int(Rnd*2)+1
                        Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
                        Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
                End Select
        End if
        If finalspeed >= 6 AND finalspeed <= 16 then
                PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
        End If
        If finalspeed < 6 Then
                PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
        End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
        PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()                
        PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
        dim finalspeed
        finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 10 then
                RandomSoundTargetHitStrong()
                RandomSoundBallBouncePlayfieldSoft Activeball
        Else 
                RandomSoundTargetHitWeak()
        End If        
End Sub

Sub Targets_Hit (idx)
        PlayTargetSound        
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
        Select Case Int(Rnd*9)+1
                Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
                Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
                Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
                Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
                Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
                Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
                Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
                Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
                Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
        End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
        PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
        Select Case Int(Rnd*5)+1
                Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
        End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()                        
        PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
        PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
        SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)        
        SoundPlayfieldGate        
End Sub        

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
        PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
        PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
        If Activeball.velx > 1 Then SoundPlayfieldGate
        StopSound "Arch_L1"
        StopSound "Arch_L2"
        StopSound "Arch_L3"
        StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
        If activeball.velx < -8 Then
                RandomSoundRightArch
        End If
End Sub

Sub Arch2_hit()
        If Activeball.velx < 1 Then SoundPlayfieldGate
        StopSound "Arch_R1"
        StopSound "Arch_R2"
        StopSound "Arch_R3"
        StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
        If activeball.velx > 10 Then
                RandomSoundLeftArch
        End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
        PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
        Select Case scenario
                Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
                Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
        End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
        Dim snd
        Select Case Int(Rnd*7)+1
                Case 1 : snd = "Ball_Collide_1"
                Case 2 : snd = "Ball_Collide_2"
                Case 3 : snd = "Ball_Collide_3"
                Case 4 : snd = "Ball_Collide_4"
                Case 5 : snd = "Ball_Collide_5"
                Case 6 : snd = "Ball_Collide_6"
                Case 7 : snd = "Ball_Collide_7"
        End Select

        PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
        PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
        PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd*6)+1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315                                                                        'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05                                                                        'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
        Select Case toggle
                Case 1
                        PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025*RelayGISoundLevel, obj
                Case 0
                        PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025*RelayGISoundLevel, obj
        End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
        Select Case toggle
                Case 1
                        PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025*RelayFlashSoundLevel, obj                        
                Case 0
                        PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025*RelayFlashSoundLevel, obj                
        End Select
End Sub

'/////////////////////////////////////////////////////////////////
'                                        End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

