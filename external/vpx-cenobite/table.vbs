' ****************************************************************
'        Cenobite (Uncle Willy 2023)
'         re-theme of JP's Deadpool
' ****************************************************************
'On the DOF website put Exxx
'DOF some updating by outhere
'101 Left Flipper
'102 Right Flipper
'103 left slingshot
'104 right slingshot
'105
'106
'107 Center Bumper
'108 RIGHT Bumper
'109 Left bumper
'110
'111 HellHouse
'118
'119 Reset drop Targets
'120 AutoFire
'122 knocker
'123 ballrelease

Option Explicit
Randomize

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1     ' standard ball mass in JP's VPX Physics 3.0
Const SongVolume = 0.2 ' 1 is full volume, but I set it quite low to listen better to the other sounds :)

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
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
Const cGameName = "cenobite"
Const myVersion = "4.0.0"
Const MaxPlayers = 4          ' from 1 to 4
Const BallSaverTime = 20      ' in seconds of the first ball
Const MaxMultiplier = 5       ' limit playfield multiplier
Const MaxBonusMultiplier = 50 'limit Bonus multiplier
Const BallsPerGame = 3        ' usually 3 or 5
Const MaxMultiballs = 6       ' max number of balls during multiballs

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
Dim bJustStarted
Dim bJackpot

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs

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

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Credits = 0
    Loadhs

    ' Initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = True 'we want coins

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
    ' set any lights for the attract mode
    ChangeGi white
    ChangeGiIntensity 1
    GiOff
    StartAttractMode
    ' Start the RealTime timer
    RealTime.Enabled = 1
	SetRenderingStuff
    ' Load table color
HideLUT
    LoadLut
End Sub

Sub SetRenderingStuff
dim vrobj
	If Table1.ShowFSS = true then
		For each vrobj in VRObjects: vrobj.visible = True:Next
		'VRWallRight.visible = True:VRWallLeft.visible = True
	Else
	End If
	If renderingmode = 2 then
		For each vrobj in VRObjects: vrobj.visible = True:Next
		'VRWallRight.visible = True:VRWallLeft.visible = True
	Else
	End If
End Sub

''**crickets
''************************************Ok Stupid cricket add on for my own amusement and seeing if i can still code
'''***********************************Looking like my coding skills a very rusty so i'm sure theres an easier way  but whatever
''****************************************
'4 tivpmtimermers to control the hopping
Dim cricket1, cricket2, cricket3, cricket4  'Balls for collision/scoring
Dim cricketsRunning : cricketsRunning = 0	'are the crickets hopping 0 = no  1 = yes
dim hop1d: hop1d = 1
dim hop1p: hop1p=1
Dim strt

Sub StartCrickets
	cricketsRunning = 1
	hop1.image = "GrassHopper_PCOn":hop2.image = "GrassHopper_PCOn":hop3.image = "GrassHopper_PCOn":hop4.image = "GrassHopper_PCOn"
	strt = RndNbr(4)	'rand between one and 4
	Select Case strt							'hop selected cricket to the table  
		Case 1		hop1Timer.enabled = True
		Case 2		hop2Timer.enabled = True
		Case 3		hop3Timer.enabled = True
		Case 4		hop4Timer.enabled = True
	End Select
	StrtRestCrickets	'Start the rest of the crickets at an interval
End Sub

Sub StopCrickets
	If 	cricketsRunning = 1 then 
		cricketsRunning = 0
hop1.image = "GrassHopper_PC":hop2.image = "GrassHopper_PC":hop3.image = "GrassHopper_PC":hop4.image = "GrassHopper_PC"
		hopper1.TimerEnabled = False:hopper2.TimerEnabled = False:hopper3.TimerEnabled = False:hopper4.TimerEnabled = False
		if hop1Timer.enabled = false and hop1p = 19 Then hop1Timer.Enabled = True
		if hop2Timer.enabled = false and hop2p = 19 Then hop2Timer.Enabled = True
		if hop3Timer.enabled = false and hop3p = 19 Then hop3Timer.Enabled = True
		if hop4Timer.enabled = false and hop4p = 19 Then hop4Timer.Enabled = True
		If hop1Timer.enabled = True and hop1d = 1 Then hop1d = -1
		If hop2Timer.enabled = True and hop2d = 1 Then hop2d = -1
		If hop3Timer.enabled = True and hop3d = 1 Then hop3d = -1
		If hop4Timer.enabled = True and hop4d = 1 Then hop4d = -1
		vpmTimer.addtimer 1000, "CleanUp '"
	End If
End Sub

Sub Cleanup
		if hop1Timer.enabled = false and hop1p = 19 Then hop1Timer.Enabled = True
		if hop2Timer.enabled = false and hop2p = 19 Then hop2Timer.Enabled = True
		if hop3Timer.enabled = false and hop3p = 19 Then hop3Timer.Enabled = True
		if hop4Timer.enabled = false and hop4p = 19 Then hop4Timer.Enabled = True
End Sub

Dim strtcrcnt

Sub StrtRestCrickets()		'start rest of crickets
	if hop1Timer.Enabled = False Then hop1Timer.Enabled = True
	if hop2Timer.Enabled = False Then hop2Timer.Enabled = True
	if hop3Timer.Enabled = False Then hop3Timer.Enabled = True
	if hop4Timer.Enabled = False Then hop4Timer.Enabled = True
End Sub



Sub hop1Timer_Timer
	Dim iv
	iv = Int(300 + rnd * 701)
	hop1p = hop1p + hop1d
	Select case hop1p
		case 1		hop1.TransZ = 0 : hop1.Z = 55 : hop1Timer.enabled = False : hop1d = -hop1d: hopper1.TimerInterval = iv: hopper1.TimerEnabled = true
		case 2		hop1.TransZ = -20 : hop1.Z = 75
		case 3		hop1.TransZ = -40 : hop1.Z = 95
		case 4		hop1.TransZ = -60 : hop1.Z = 115
		case 5		hop1.TransZ = -80 : hop1.Z = 135
		case 6		hop1.TransZ = -85 : hop1.Z = 130
		case 7	 	hop1.TransZ = -90 : hop1.Z = 120 
		case 8		hop1.TransZ = -95 : hop1.Z = 110
		case 9	 	hop1.TransZ = -100 : hop1.Z = 100
		case 10  	hop1.TransZ = -105 : hop1.Z = 90
		case 11 	hop1.TransZ = -110 : hop1.Z = 80
		case 12  	hop1.TransZ = -115 : hop1.Z = 70
		case 13 	hop1.TransZ = -120 : hop1.Z = 60 
		case 14 	hop1.TransZ = -125 : hop1.Z = 50
		case 15 	hop1.TransZ = -130 : hop1.Z = 40
		case 16 	hop1.TransZ = -135 : hop1.Z = 30
		case 17 	hop1.TransZ = -140 : hop1.Z = 20
		case 18 	hop1.TransZ = -145 : hop1.Z = 10
					If hop1d = 1 then 
						Set cricket1 = hopper1.CreateSizedBallWithMass (BallSize / 2, BallMass)
						cricket1.visible = False:cr1 = 1
					Else
						hopper1.Destroyball:cr1 = 0
					end If
		case 19 	hop1.TransZ = -150 : hop1.Z = 0 : hop1Timer.enabled = False : hop1d = -hop1d
					hopper1.TimerInterval = iv: hopper1.TimerEnabled = true
	end Select		
End Sub
Dim cr1, cr2,cr3,cr4
cr1 = 0:cr2 = 0:cr3 = 0:cr4 = 0
dim hop2d: hop2d = 1
dim hop2p: hop2p=1
Sub hop2Timer_Timer
	dim iv
	iv = Int(300 + rnd * 701)
	hop2p = hop2p + hop2d
	Select case hop2p
		case 1		hop2.TransZ = 0 : hop2.Z = 55 : hop2Timer.enabled = False : hop2d = -hop2d: hopper2.TimerInterval = iv: hopper2.TimerEnabled = true
		case 2		hop2.TransZ = -20 : hop2.Z = 75
		case 3		hop2.TransZ = -40 : hop2.Z = 95
		case 4		hop2.TransZ = -60 : hop2.Z = 115
		case 5		hop2.TransZ = -80 : hop2.Z = 135
		case 6		hop2.TransZ = -85 : hop2.Z = 130
		case 7	 	hop2.TransZ = -90 : hop2.Z = 120 
		case 8		hop2.TransZ = -95 : hop2.Z = 110
		case 9	 	hop2.TransZ = -100 : hop2.Z = 100
		case 10  	hop2.TransZ = -105 : hop2.Z = 90
		case 11 	hop2.TransZ = -110 : hop2.Z = 80
		case 12  	hop2.TransZ = -115 : hop2.Z = 70
		case 13 	hop2.TransZ = -120 : hop2.Z = 60 
		case 14 	hop2.TransZ = -125 : hop2.Z = 50
		case 15 	hop2.TransZ = -130 : hop2.Z = 40
		case 16 	hop2.TransZ = -135 : hop2.Z = 30
		case 17 	hop2.TransZ = -140 : hop2.Z = 20
		case 18 	hop2.TransZ = -145 : hop2.Z = 10
					If hop2d = 1 then 
						Set cricket2 = hopper2.CreateSizedBallWithMass (BallSize / 2, BallMass)
						cricket2.visible = False: cr2 = 1
					Else
						hopper2.Destroyball:cr2 = 0
					end If
		case 19 	hop2.TransZ = -150 : hop2.Z = 0 : hop2Timer.enabled = False : hop2d = -hop2d
					hopper2.TimerInterval = iv: hopper2.TimerEnabled = true
	end Select		
End Sub

dim hop3d: hop3d = 1
dim hop3p: hop3p=1
Sub hop3Timer_Timer
	dim iv
	iv = Int(300 + rnd * 701)
	hop3p = hop3p + hop3d
	Select case hop3p
		case 1		hop3.TransZ = 0 : hop3.Z = 55 : hop3Timer.enabled = False : hop3d = -hop3d : hopper3.TimerInterval = iv: hopper3.TimerEnabled = true
		case 2		hop3.TransZ = -20 : hop3.Z = 75
		case 3		hop3.TransZ = -40 : hop3.Z = 95
		case 4		hop3.TransZ = -60 : hop3.Z = 115
		case 5		hop3.TransZ = -80 : hop3.Z = 135
		case 6		hop3.TransZ = -85 : hop3.Z = 130
		case 7	 	hop3.TransZ = -90 : hop3.Z = 120 
		case 8		hop3.TransZ = -95 : hop3.Z = 110
		case 9	 	hop3.TransZ = -100 : hop3.Z = 100
		case 10  	hop3.TransZ = -105 : hop3.Z = 90
		case 11 	hop3.TransZ = -110 : hop3.Z = 80
		case 12  	hop3.TransZ = -115 : hop3.Z = 70
		case 13 	hop3.TransZ = -120 : hop3.Z = 60 
		case 14 	hop3.TransZ = -125 : hop3.Z = 50
		case 15 	hop3.TransZ = -130 : hop3.Z = 40
		case 16 	hop3.TransZ = -135 : hop3.Z = 30
		case 17 	hop3.TransZ = -140 : hop3.Z = 20
		case 18 	hop3.TransZ = -145 : hop3.Z = 10
					If hop3d = 1 then 
						Set cricket3 = hopper3.CreateSizedBallWithMass (BallSize / 2, BallMass)
						cricket3.visible = False:cr3 = 1
					Else
						hopper3.Destroyball:cr3 = 0
					end If
		case 19 	hop3.TransZ = -150 : hop3.Z = 0 : hop3Timer.enabled = False : hop3d = -hop3d
		hopper3.TimerInterval = iv : hopper3.TimerEnabled = true
	end Select		
End Sub


dim hop4d: hop4d = 1
dim hop4p: hop4p=1
Sub hop4Timer_Timer
	dim iv
	iv = Int(300 + rnd * 701)
	hop4p = hop4p + hop4d
	Select case hop4p
		case 1		hop4.TransZ = 0 : hop4.Z = 55 : hop4Timer.enabled = False : hop4d = -hop4d : hopper4.TimerInterval = iv: hopper4.TimerEnabled = true
		case 2		hop4.TransZ = -20 : hop4.Z = 75
		case 3		hop4.TransZ = -40 : hop4.Z = 95
		case 4		hop4.TransZ = -60 : hop4.Z = 115
		case 5		hop4.TransZ = -80 : hop4.Z = 135
		case 6		hop4.TransZ = -85 : hop4.Z = 130
		case 7	 	hop4.TransZ = -90 : hop4.Z = 120 
		case 8		hop4.TransZ = -95 : hop4.Z = 110
		case 9	 	hop4.TransZ = -100 : hop4.Z = 100
		case 10  	hop4.TransZ = -105 : hop4.Z = 90
		case 11 	hop4.TransZ = -110 : hop4.Z = 80
		case 12  	hop4.TransZ = -115 : hop4.Z = 70
		case 13 	hop4.TransZ = -120 : hop4.Z = 60 
		case 14 	hop4.TransZ = -125 : hop4.Z = 50
		case 15 	hop4.TransZ = -130 : hop4.Z = 40
		case 16 	hop4.TransZ = -135 : hop4.Z = 30
		case 17 	hop4.TransZ = -140 : hop4.Z = 20
		case 18 	hop4.TransZ = -145 : hop4.Z = 10
					If hop4d = 1 then 
						Set cricket4 = hopper4.CreateSizedBallWithMass (BallSize / 2, BallMass)
						cricket4.visible = False:cr4 = 1
					Else
						hopper4.Destroyball:cr4 = 0
					end If
		case 19 	hop4.TransZ = -150 : hop4.Z = 0 : hop4Timer.enabled = False : hop4d = -hop4d
					hopper4.TimerInterval = iv: hopper4.TimerEnabled = true
	end Select		
End Sub

Sub hopper1_Timer
	if cricketsRunning = 1 then hop1Timer.enabled = True
	hopper1.TimerEnabled = False
End Sub

Sub hopper2_Timer
	if cricketsRunning = 1 then hop2Timer.enabled = True
	hopper2.TimerEnabled = False
End Sub

Sub hopper3_Timer
	if cricketsRunning = 1 then hop3Timer.enabled = True
	hopper3.TimerEnabled = False
End Sub

Sub hopper4_Timer
	if cricketsRunning = 1 then hop4Timer.enabled = True
	hopper4.TimerEnabled = False
End Sub


'*****************************************************************************************************
' VR PLUNGER ANIMATION
'*****************************************************************************************************

Sub TimerPlunger_Timer
  If VR_Cab_Primary_plunger.Y < -200 then
  		VR_Cab_Primary_plunger.Y = VR_Cab_Primary_plunger.Y + 5
  End If
End Sub

Sub TimerPlunger2_Timer
 'debug.print plunger.position
  VR_Cab_Primary_plunger.Y = -300 + (5* Plunger.Position) -20
End Sub
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

        If keycode = StartGameKey Then VR_Cab_StartButton.Transy = -4			' Move VR Startbutton

    If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:lilshake 5
    If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:lilshake 5
    If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:lilshake 6

    If keycode = LeftMagnaSave Then bLutActive = True:SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave Then
        If bLutActive Then
            NextLUT	
		end If																					
    End If
	If keycode = LockBarKey Then
			boombut.isdropped = 1:vpmtimer.addtimer 200, "boombut.isdropped = False '"
	End If
 
    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        if bFreePlay = False Then DOF 125, DOFOn
        If(Tilted = False)Then
            DMDFlush
            DMD "_", CL(1, "CREDITS " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Normal flipper action
        If keycode = RightFlipperKey Then VR_Cab_FlipperButtonRight.TransX = -6
        If keycode = LeftFlipperKey Then VR_Cab_FlipperButtonLeft.TransX = 6
    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then CheckTilt 'only check the tilt during game
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 1:UpdateGates 1
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 0

        If bChooseBattle Then
            ChooseBattle(keycode)
            Exit Sub
        End If

        If keycode = RightMagnaSave or keycode = LockBarKey Then
            BoomHit
        End If

        If keycode = StartGameKey Then

            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
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
                    DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, "vo_nocredits"
                    ShowTableInfo
                End If
            End If
        End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)
        If keycode = StartGameKey Then VR_Cab_StartButton.Transy = 0			' Move VR Startbutton

    If keycode = LeftMagnaSave Then bLutActive = False:HideLUT

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
        PlaySoundAt "sfx4", plunger
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		VR_Cab_Primary_plunger.Y = -300
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific



if keycode = LeftFlipperKey Then
		 VR_Cab_FlipperButtonLeft.TransX = 0
    If bGameInPLay AND NOT Tilted Then

            SolLFlipper 0
            UpdateGates 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
	end if
End If
If keycode = RightFlipperKey Then
		 VR_Cab_FlipperButtonRight.TransX = 0
    If bGameInPLay AND NOT Tilted Then
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
    SaveLUT
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
        LeftFlipper.EOSTorque = 0.65:LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.15:LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.65:RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.15:RightFlipper.RotateToStart
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt > TiltSensitivity)AND(Tilt < 15)Then 'show a warning
        DMD "_", CL(1, "CAREFUL"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
    End if
    If Tilt > 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD "", "", "d_TILT", eNone, eNone, eBlink, 200, False, "" 
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
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
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        Bumper3.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
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
    Dim a
    Select Case Battle(CurrentPlayer, 0)
        Case 0 'no battle active so play the standard songs or the multiball songs
            If bBerserker Then
                PLaySong "mu_beserkerrage"
            ElseIf bDiscoLoopsEnabled Then
                PlaySong "mu_discoloops"
            ElseIf bDiscoMBEnabled Then
                PlaySong "mu_discomb"
            ElseIf bMechSuitMBStarted Then
                PlaySong "mu_mechsuitmb"
            ElseIf bNinjaMB Then
                PlaySong "mu_ninjamb"
            Else 'play default music
                PlaySong "mu_hellhouse" &Balls
            End If
        Case 1 'Juggernaut
            PlaySong "mu_juggernaut"
        Case 2 'Mystique
            PlaySong "mu_mystique"
        Case 3 'Sabretooth
            PlaySong "mu_sabretooth"
        Case 4 'T-Rex
            PlaySong "mu_trex"
        Case 5 'Megalodon
            PlaySong "mu_megalodon"
        Case 6 'Sauron
            PlaySong "mu_sauronmb"
        Case 7 'MrSinister
            PlaySong "mu_mrsinister"
    End Select
End Sub

'********************
' Play random quotes
'********************

Sub PlayQuote
    Dim tmp
    tmp = RndNbr(42)
    PlaySound "quote" &tmp
End Sub

Sub PlaySfx
    Dim tmp
    tmp = RndNbr(39)
    PlaySound "sfx" &tmp
    FlashForms flasher007, 1500, 50, 0
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
    Fi001.Visible = 1
    Fi002.Visible = 1
    Fi003.Visible = 1
    Fi004.Visible = 1
    Fi005.Visible = 1
    Fi006.Visible = 1

   FlashForms pinheadFlash, 1500, 500, 0
  ' BOXFLASH.UpdateInterval = 10
   'BOXFLASH.Play SeqUpOn, 25, 3
End Sub

Sub GiOff
    PlaySoundAt "fx_GiOff", li036 'about the center of the table
    DOF 118, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    Fi001.Visible = 0
    Fi002.Visible = 0
    Fi003.Visible = 0
    Fi004.Visible = 0
    Fi005.Visible = 0
    Fi006.Visible = 0

   FlashForms pinheadFlash, 1500, 500, 0
   'BOXFLASH.UpdateInterval = 10
   'BOXFLASH.Play SeqDownOn, 25, 3
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
        Case 4 'center fra lilDP
            LightSeqlilDP.UpdateInterval = 4
            LightSeqlilDP.Play SeqCircleOutOn, 15, 2
        Case 5 'top down
            LightSeqPlayfield.UpdateInterval = 4
            LightSeqPlayfield.Play SeqDownOn, 15, 2
        Case 6 'down to top
            LightSeqPlayfield.UpdateInterval = 4
            LightSeqPlayfield.Play SeqUpOn, 15, 1
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
Const lob = 0     'number of locked balls
Const maxvel = 45 'max ball velocity
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
       
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
       

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
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
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
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
iF cr1 = 0 Then
Else
	If Ball1.x = cricket1.x then CollectChimichanga : hop1Timer.enabled = true:end if
 	If ball2.x = cricket1.x then CollectChimichanga : hop1Timer.enabled = true:end if
END If
iF cr2 = 0 Then
Else
	If Ball1.x = cricket2.x then CollectChimichanga : hop2Timer.enabled = true:end if
	If ball2.x = cricket2.x then CollectChimichanga : hop2Timer.enabled = true:end if
End If
if cr3 = 0 Then
Else
	If Ball1.x = cricket3.x then CollectChimichanga : hop3Timer.enabled = true:end if
	If ball2.x = cricket3.x then CollectChimichanga : hop3Timer.enabled = true:end if
end If
if cr4 = 0 Then
Else 
	If Ball1.x = cricket4.x then CollectChimichanga : hop4Timer.enabled = true:end if
	If ball2.x = cricket4.x then CollectChimichanga : hop4Timer.enabled = true:end if
end If
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

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    bSkillShotReady = True

'*********** Set Pillar
	Select Case  BallsInLock(CurrentPlayer)
		Case 0: PilStop = 1 : pillarSpin.enabled = 1
		Case 1: PilStop = 2 : pillarSpin.enabled = 1
		Case 2: PilStop = 3 : pillarSpin.enabled = 1
		Case 3: PilStop = 4 : pillarSpin.enabled = 1
	End Select
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
        PLaySong "mu_bonuscount"
        'Count the bonus. This table uses several bonus
        DMD CL(0, "BONUS"), "", "", eBlink, eNone, eNone, 1000, True, ""

        'Ninja Stars
        AwardPoints = NinjaStars(CurrentPlayer) * 100000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "PUZZLE BOXES " & NinjaStars(CurrentPlayer)), "", eBlink, eNone, eNone, 1000, True, ""

        'Chimichangas
        AwardPoints = Chimichangas(CurrentPlayer) * 250000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "CRICKET " & Chimichangas(CurrentPlayer)), "", eBlink, eNone, eNone, 1000, True, ""

        'Chimichangas
        AwardPoints = Weapons(CurrentPlayer) * 100000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "CONFIGURATIONS " & Weapons(CurrentPlayer)), "", eBlink, eNone, eNone, 1000, True, ""

        ' calculate the totalbonus
        DMD CL(0, FormatScore(TotalBonus)), CL(1, "TOTAL BONUS " & " X" & BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 1500, True, ""
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        ' Add the bonus to the score
        AddScore TotalBonus

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
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0)Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, ""

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
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
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame > 1 Then
            Select Case CurrentPlayer
                Case 1:DMD "", "", "d_player1", eNone, eNone, eNone, 1000, True, "vo_player1"
                Case 2:DMD "", "", "d_player2", eNone, eNone, eNone, 1000, True, "vo_player2"
                Case 3:DMD "", "", "d_player3", eNone, eNone, eNone, 1000, True, "vo_player3"
                Case 4:DMD "", "", "d_player4", eNone, eNone, eNone, 1000, True, "vo_player4"
            End Select
        Else
            DMD "", "", "d_player1", eNone, eNone, eNone, 1000, True, ""
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
            DMD "_", CL(1, "BALL SAVED"), "_", eNone, eBlinkfast, eNone, 1000, True, ""
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and

                    ' turn off any multiball specific lights
                    'ChangeGi white
                    'ChangeGIIntensity 1
                    'stop any multiball modes
                    StopMBmodes
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
                ' End Mode and timers

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
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    'LaunchLight.State = 2

    'be sure to update the Scoreboard after the animations, if any

    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        vpmtimer.addtimer 1500, "PlungerIM.AutoFire:DOF 120, DOFPulse:PlaySoundAt ""fx_kicker"", swPlungerRest:bAutoPlunger = False '"
    End If
    'Start the Selection of the skillshot if ready
    If bSkillShotReady Then
        PlaySong "mu_shooterlane"
        UpdateSkillshot()
        ' show the message to shoot the ball in case the player has fallen sleep
        SwPlungerCount = 0
        swPlungerRest.TimerEnabled = 1
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    lighteffect 6
    bBallInPlungerLane = False
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
        ChangeSong
        ResetSkillShotTimer.Enabled = 1
    End If
    If bLilDPMB AND gate3.open = False Then 'if in LilDP MultiBall then open the left top gate if it was closed
        gate3.open = True
        vpmtimer.addtimer 2000, "gate3.open = False '"
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
        EnableBallSaver BallSaverTime
    End If
' turn off LaunchLight
' LaunchLight.State = 0
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds
Dim SwPlungerCount
Sub swPlungerRest_Timer
    Select Case SwPlungerCount
        Case 0
            If bInfoNeeded1(CurrentPLayer)Then
                DMD "USE FLIPPER BUTTONS", "TO SELECT SKILLSHOT", "_", eNone, eNone, eNone, 2000, True, "vo_useflipperselecttoplaneskillshot"
            End If
        Case 1
            If bInfoNeeded1(CurrentPLayer)Then
                DMD "HOLD LEFT FLIPPER", "FOR SUPERSKILLSHOT", "_", eNone, eNone, eNone, 2000, True, "vo_holdflipper-sk"
                bInfoNeeded1(CurrentPLayer) = False
            End If
        Case 4
            DMD CL(0, "HEY YOU"), CL(1, "SHOOT THE BALL"), "_", eNone, eNone, eNone, 2000, True, "vo_whatareyouwaitingfor"
            swPlungerRest.TimerEnabled = 0
    End Select
    SwPlungerCount = SwPlungerCount + 1
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
    If Tilted Then Exit Sub
    ' add the points to the current players score variable
    If Battle(CurrentPLayer, 0) > 0 Then
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer) * ColossusValue
    Else
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
    End if
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

    ' If(bMultiBallMode = True) Then
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
    DMD "_", CL(1, "INCREASED JACKPOT"), "_", eNone, eNone, eNone, 1000, True, ""
' you may wish to limit the jackpot to a upper limit, ie..
'	If (Jackpot >= 6000) Then
'		Jackpot = 6000
' 	End if
'End if
End Sub

Sub AddSuperJackpot(points) 'not used in this table
    If Tilted Then Exit Sub
End Sub

Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxBonusMultiplier)then
        ' then add and set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
        SetBonusMultiplier(NewBonusLevel)
    'DMD "_", CL(1, "BONUS X " &NewBonusLevel), "_", eNone, eBlink, eNone, 2000, True, ""
    Else
        AddScore 50000
        DMD "_", CL(1, "50000"), "_", eNone, eNone, eNone, 1000, True, ""
    End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly

Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level
    UPdateBonusXLights(Level)
End Sub

Sub UpdateBonusXLights(Level) 'no lights in this table
    ' Update the lights
    Select Case Level
    'Case 1:light54.State = 0:light55.State = 0:light56.State = 0:light57.State = 0
    'Case 2:light54.State = 1:light55.State = 0:light56.State = 0:light57.State = 0
    'Case 3:light54.State = 0:light55.State = 1:light56.State = 0:light57.State = 0
    'Case 4:light54.State = 0:light55.State = 0:light56.State = 1:light57.State = 0
    'Case 5:light54.State = 0:light55.State = 0:light56.State = 0:light57.State = 1
    End Select
End Sub

Sub AddPlayfieldMultiplier(n)
    Dim snd
    Dim NewPFLevel
    ' if not at the maximum level x
    if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier)then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
        SetPlayfieldMultiplier(NewPFLevel)
        snd = "sfx_thunder" &NewPFLevel
        DMD "_", CL(1, "PLAYFIELD X " &NewPFLevel), "_", eNone, eBlink, eNone, 2000, True, snd
    Else 'if the 5x is already lit
        AddScore 50000
        DMD "_", CL(1, "50000"), "_", eNone, eNone, eNone, 2000, True, ""
    End if
    'Start the timer to reduce the playfield x every 30 seconds
    pfxtimer.Enabled = 0
    pfxtimer.Enabled = 1
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

Sub UpdatePFXLights(Level)
    ' Update the playfield multiplier lights
    Select Case Level
        Case 1:li002.State = 0:li003.State = 0:li004.State = 0:li005.State = 0
        Case 2:li002.State = 1:li003.State = 0:li004.State = 0:li005.State = 0
        Case 3:li002.State = 1:li003.State = 1:li004.State = 0:li005.State = 0
        Case 4:li002.State = 1:li003.State = 1:li004.State = 1:li005.State = 0
        Case 5:li002.State = 1:li003.State = 1:li004.State = 1:li005.State = 1
    End Select
' show the multiplier in the DMD?
End Sub

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        DMD "_", CL(1, ("EXTRA BALL WON")), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        LightShootAgain.State = 1 'light the shoot again lamp
        GiEffect 2
        LightEffect 2
    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL(1, ("EXTRA GAME WON")), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    Credits = Credits + 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardJackpot() 'award a normal jackpot
    If bLilDPMB Then
        Select case RndNbr(4)
            Case 1:DMD CL(0, FormatScore(Jackpot(CurrentPlayer))), CL(1, "DERELICT JACKPOT"), "bkborder", eBlinkFast, eBlinkFast, eNone, 1000, True, "vo_lildp_jackpot1"
            Case 2:DMD CL(0, FormatScore(Jackpot(CurrentPlayer))), CL(1, "DERELICT JACKPOT"), "bkborder", eBlinkFast, eBlinkFast, eNone, 1000, True, "vo_lildp_jackpot2"
            Case 3:DMD CL(0, FormatScore(Jackpot(CurrentPlayer))), CL(1, "DERELICT JACKPOT"), "bkborder", eBlinkFast, eBlinkFast, eNone, 1000, True, "vo_lildp_jackpot3"
            Case 4:DMD CL(0, FormatScore(Jackpot(CurrentPlayer))), CL(1, "DERELICT JACKPOT"), "bkborder", eBlinkFast, eBlinkFast, eNone, 1000, True, "vo_lildp_jackpot4"
        End Select
        LightEffect 4
        GiEffect 4
    Else
        DMD CL(0, FormatScore(Jackpot(CurrentPlayer))), CL(1, "JACKPOT"), "bkborder", eBlinkFast, eBlinkFast, eNone, 1000, True, "vo_Jackpot"
        LightEffect 2
        GiEffect 2
    End If
    DOF 126, DOFPulse
    AddScore Jackpot(CurrentPlayer)
End Sub

Sub AwardMegaJackpot() 'award a Megalodon Jackpot
    DMD CL(0, FormatScore(Jackpot(CurrentPlayer))), CL(1, "CHANNARD JACKPOT"), "d_megalodon", eBlinkFast, eBlinkFast, eNone, 1000, True, "vo_Jackpot"
    DOF 126, DOFPulse
    AddScore MegaJackpot(CurrentPlayer)
    MegaJackpot(CurrentPlayer) = MegaJackpot(CurrentPlayer) + 10000
    MegaSuperJCount = MegaSuperJCount + 1
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardSuperMegaJackpot() 'award a SuperMegalodon Jackpot
    MegaSuperJValue = MegaJackpot(CurrentPlayer) * MegaSuperJCount
    DMD CL(0, FormatScore(MegaSuperJValue)), "CHANNARD SUPJACKPOT", "d_megalodon", eBlinkFast, eBlinkFast, eNone, 1000, True, "vo_superjackpot"
    AddScore MegaSuperJValue
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardSuperJackpot()
    DMD CL(0, FormatScore(SuperJackpot(CurrentPlayer))), CL(1, "SUPER JACKPOT"), "bkborder", eBlinkFast, eBlinkFast, eNone, 1000, True, "vo_superjackpot"
    DOF 126, DOFPulse
    AddScore SuperJackpot(CurrentPlayer)
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL(0, FormatScore(SkillshotValue(CurrentPlayer))), CL(1, ("SKILLSHOT")), "bkborder", eBlinkFast, eBlink, eNone, 1000, True, "fanfare"
    DOF 127, DOFPulse
    Addscore SkillShotValue(CurrentPlayer)
    ' increment the skillshot value with 250.000
    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 250000
    'do some light show
    GiEffect 2
    LightEffect 2
End Sub

Sub AwardSuperSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL(0, FormatScore(SuperSkillshotValue(CurrentPlayer))), CL(1, ("SUPER SKILLSHOT")), "bkborder", eBlinkFast, eBlink, eNone, 1000, True, "fanfare"
    DOF 127, DOFPulse
    Addscore SuperSkillshotValue(CurrentPlayer)
    ' increment the skillshot value with 500.000
    SuperSkillshotValue(CurrentPlayer) = SuperSkillshotValue(CurrentPlayer) + 500000
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
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 100000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 100000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 100000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 100000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(cGameName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(cGameName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
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
        Case 0:table1.ColorGradeImage = "LUT0":ChangeGiIntensity(.45)
        Case 1:table1.ColorGradeImage = "LUT1":ChangeGiIntensity(.5)
        Case 2:table1.ColorGradeImage = "LUT2":ChangeGiIntensity(.55)
        Case 3:table1.ColorGradeImage = "LUT3":ChangeGiIntensity(.6)
        Case 4:table1.ColorGradeImage = "LUT4":ChangeGiIntensity(.65)
        Case 5:table1.ColorGradeImage = "LUT5":ChangeGiIntensity(.7)
        Case 6:table1.ColorGradeImage = "LUT6":ChangeGiIntensity(.75)
        Case 7:table1.ColorGradeImage = "LUT7":ChangeGiIntensity(.8)
        Case 8:table1.ColorGradeImage = "LUT8":ChangeGiIntensity(.85)
        Case 9:table1.ColorGradeImage = "LUT9":ChangeGiIntensity(.9)
        Case 10:table1.ColorGradeImage = "LUT10":ChangeGiIntensity(.95)
        Case 11:table1.ColorGradeImage = "LUT Warm 0":ChangeGiIntensity(.45)
        Case 12:table1.ColorGradeImage = "LUT Warm 1":ChangeGiIntensity(.5)
        Case 13:table1.ColorGradeImage = "LUT Warm 2":ChangeGiIntensity(.55)
        Case 14:table1.ColorGradeImage = "LUT Warm 3":ChangeGiIntensity(.6)
        Case 15:table1.ColorGradeImage = "LUT Warm 4":ChangeGiIntensity(.65)
        Case 16:table1.ColorGradeImage = "LUT Warm 5":ChangeGiIntensity(.7)
        Case 17:table1.ColorGradeImage = "LUT Warm 6":ChangeGiIntensity(.75)
        Case 18:table1.ColorGradeImage = "LUT Warm 7":ChangeGiIntensity(.8)
        Case 19:table1.ColorGradeImage = "LUT Warm 8":ChangeGiIntensity(.85)
        Case 20:table1.ColorGradeImage = "LUT Warm 9":ChangeGiIntensity(.9)
        Case 21:table1.ColorGradeImage = "LUT Warm 10":ChangeGiIntensity(.95)
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
    LUBack.imagea = "PostItNote"
    String = CL1(String)
    For xFor = 1 to 40
        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
    SetLUTLine ""
    LUBack.imagea = "PostitBL"
End Sub

Function CL1(NumString) 'center line
    Dim Temp, TempStr
    If Len(NumString) > 40 Then NumString = Left(NumString, 40)
    Temp = (40 - Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL1 = TempStr
End Function


Dim GiIntensity
GiIntensity = 1               'used for the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
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
Dim InternalDMD
Dim InternalDMDScene

Sub DMD_Init() 'default/startup values
    If UseFlexDMD Then
        Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
		UseColoredDMD = true
        If Not FlexDMD is Nothing Then
            If FlexDMDHighQuality Then
                FlexDMD.TableFile = Table1.Filename& ".vpx"
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
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
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
    Dim tmp, tmp1, tmp1a, tmp1b, tmp2
    if(dqHead = dqTail)Then
        tmp = FL(0, "P" &CurrentPlayer& "B" &Balls, FormatScore(Score(Currentplayer)))
        'tmp = CL(0, FormatScore(Score(Currentplayer) ) )
        'tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
        'tmp1 = FormatScore(Bonuspoints(Currentplayer) ) & " X" &BonusMultiplier(Currentplayer)
        tmp2 = "bkborder"
        Select Case Battle(CurrentPlayer, 0)
            Case 0:              'no battle active then display the Team-up state
                If bLilDPMB Then 'LilDP multiball is active then show the jackpot value
                    tmp = RL(0, FormatScore(Score(Currentplayer)))
                    tmp1 = RL(1, FormatScore(LilDPJackpot(CurrentPlayer)) & " P" &CurrentPlayer& "B" &Balls)
                    tmp2 = "d_lildpmb"
                Else
                    tmp1a = Chr(33)&Chr(37 + DazzlePower(CurrentPlayer))&Chr(34)&Chr(37 + ColossusPower(CurrentPlayer))&Chr(35)&Chr(37 + WolverinePower(CurrentPlayer))&Chr(36)&Chr(37 + DominoPower(CurrentPlayer))
                    tmp1b = tmp1 & chr(42) & NinjaStars(CurrentPlayer) & chr(44) & Chimichangas(CurrentPlayer) & chr(47) & Weapons(CurrentPlayer)
                    tmp1 = FL(1, tmp1a, tmp1b)
                End If
            Case 1 'Juggernaut
                tmp = RL(0, FormatScore(Score(Currentplayer)))
                tmp1a = ShowLife(LifeLeft(CurrentPlayer, 1))
                tmp1b = "P" &CurrentPlayer& "B" &Balls
                if DazzlePower(CurrentPlayer) = 4 Then tmp1b = Chr(33)&tmp1b
                if ColossusPower(CurrentPlayer) = 4 Then tmp1b = Chr(34)&tmp1b
                if WolverinePower(CurrentPlayer) = 4 Then tmp1b = Chr(35)&tmp1b
                if DominoPower(CurrentPlayer) = 4 Then tmp1b = Chr(36)&tmp1b
                tmp1 = FL(1, tmp1a, tmp1b)
                tmp2 = "d_juggernaut"
            Case 2 'Mystique
                tmp = RL(0, FormatScore(Score(Currentplayer)))
                tmp1a = ShowLife(LifeLeft(CurrentPlayer, 2))
                tmp1b = "P" &CurrentPlayer& "B" &Balls
                if DazzlePower(CurrentPlayer) = 4 Then tmp1b = Chr(33)&tmp1b
                if ColossusPower(CurrentPlayer) = 4 Then tmp1b = Chr(34)&tmp1b
                if WolverinePower(CurrentPlayer) = 4 Then tmp1b = Chr(35)&tmp1b
                if DominoPower(CurrentPlayer) = 4 Then tmp1b = Chr(36)&tmp1b
                tmp1 = FL(1, tmp1a, tmp1b)
                tmp2 = "d_mystique"
            Case 3 'Sabretooth
                tmp = RL(0, FormatScore(Score(Currentplayer)))
                tmp1a = ShowLife(LifeLeft(CurrentPlayer, 3))
                tmp1b = "P" &CurrentPlayer& "B" &Balls
                if DazzlePower(CurrentPlayer) = 4 Then tmp1b = Chr(33)&tmp1b
                if ColossusPower(CurrentPlayer) = 4 Then tmp1b = Chr(34)&tmp1b
                if WolverinePower(CurrentPlayer) = 4 Then tmp1b = Chr(35)&tmp1b
                if DominoPower(CurrentPlayer) = 4 Then tmp1b = Chr(36)&tmp1b
                tmp1 = FL(1, tmp1a, tmp1b)
                tmp2 = "d_sabretooth"
            Case 4 'T-Rex
                tmp = RL(0, FormatScore(Score(Currentplayer)))
                tmp1a = ShowLife(LifeLeft(CurrentPlayer, 4))
                tmp1b = "P" &CurrentPlayer& "B" &Balls
                if DazzlePower(CurrentPlayer) = 4 Then tmp1b = Chr(33)&tmp1b
                if ColossusPower(CurrentPlayer) = 4 Then tmp1b = Chr(34)&tmp1b
                if WolverinePower(CurrentPlayer) = 4 Then tmp1b = Chr(35)&tmp1b
                if DominoPower(CurrentPlayer) = 4 Then tmp1b = Chr(36)&tmp1b
                tmp1 = FL(1, tmp1a, tmp1b)
                tmp2 = "d_trex"
            Case 5 'Megalodon
                tmp = RL(0, FormatScore(Score(Currentplayer)))
                tmp1a = ShowLife(LifeLeft(CurrentPlayer, 5))
                tmp1b = "P" &CurrentPlayer& "B" &Balls
                if DazzlePower(CurrentPlayer) = 4 Then tmp1b = Chr(33)&tmp1b
                if ColossusPower(CurrentPlayer) = 4 Then tmp1b = Chr(34)&tmp1b
                if WolverinePower(CurrentPlayer) = 4 Then tmp1b = Chr(35)&tmp1b
                if DominoPower(CurrentPlayer) = 4 Then tmp1b = Chr(36)&tmp1b
                tmp1 = FL(1, tmp1a, tmp1b)
                tmp2 = "d_megalodon"
            Case 6 'Sauron
                tmp = RL(0, FormatScore(Score(Currentplayer)))
                tmp1a = ShowLife(LifeLeft(CurrentPlayer, 6))
                tmp1b = "P" &CurrentPlayer& "B" &Balls
                if DazzlePower(CurrentPlayer) = 4 Then tmp1b = Chr(33)&tmp1b
                if ColossusPower(CurrentPlayer) = 4 Then tmp1b = Chr(34)&tmp1b
                if WolverinePower(CurrentPlayer) = 4 Then tmp1b = Chr(35)&tmp1b
                if DominoPower(CurrentPlayer) = 4 Then tmp1b = Chr(36)&tmp1b
                tmp1 = FL(1, tmp1a, tmp1b)
                tmp2 = "d_sauron"
            Case 7 'MrSinister
                tmp = RL(0, FormatScore(Score(Currentplayer)))
                tmp1a = ShowLife(LifeLeft(CurrentPlayer, 7))
                tmp1b = "P" &CurrentPlayer& "B" &Balls
                if DazzlePower(CurrentPlayer) = 4 Then tmp1b = Chr(33)&tmp1b
                if ColossusPower(CurrentPlayer) = 4 Then tmp1b = Chr(34)&tmp1b
                if WolverinePower(CurrentPlayer) = 4 Then tmp1b = Chr(35)&tmp1b
                if DominoPower(CurrentPlayer) = 4 Then tmp1b = Chr(36)&tmp1b
                tmp1 = FL(1, tmp1a, tmp1b)
                tmp2 = "d_mrsinister"
        End Select
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Function Showlife(n)
    Dim tmp, a
    tmp = INT(n)
    Select Case tmp
        Case 0:a = "mnnnnnnnno"
        Case 1:a = "jnnnnnnnno"
        Case 2:a = "jknnnnnnno"
        Case 3:a = "jkknnnnnno"
        Case 4:a = "jkkknnnnno"
        Case 5:a = "jkkkknnnno"
        Case 6:a = "jkkkkknnno"
        Case 7:a = "jkkkkkknno"
        Case 8:a = "jkkkkkkkno"
        Case 9:a = "jkkkkkkkko"
        Case 10:a = "jkkkkkkkkl"
    End Select
    Showlife = a
End Function

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
            dqText(0, dqTail) = ExpandLine(Text0, 0)
        End If

        if(Text1 = "_")Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1, 1)
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
                    Temp = Right(dLine(i), dCharsPerLine(i)- 1)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), (dCharsPerLine(i) + 1)- deCount(i), 1)
                    Temp = Temp & Left(dLine(i), dCharsPerLine(i)- 1)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkSlowRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i))
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkFastRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i))
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

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(dCharsPerLine(id))
    Else
        if(Len(TempStr) > Space(dCharsPerLine(id)))Then
            TempStr = Left(TempStr, Space(dCharsPerLine(id)))
        Else
            if(Len(TempStr) < dCharsPerLine(id))Then
                TempStr = TempStr & Space(dCharsPerLine(id)- Len(TempStr))
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
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1)) + 48) & right(NumString, Len(NumString)- i)
        end if
    Next
    FormatScore = NumString
End function

Function FL(id, NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    Temp = dCharsPerLine(id)- Len(NumString1)- Len(NumString2)
    TempStr = NumString1 & Space(Temp) & NumString2
    FL = TempStr
End Function

Function CL(id, NumString) 'center line
    Dim Temp, TempStr
    Temp = (dCharsPerLine(id)- Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(id, NumString) 'right line
    Dim Temp, TempStr
    Temp = dCharsPerLine(id)- Len(NumString)
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
    For i = 0 to 255:Chars(i) = "d_empty":Next

    Chars(32) = "d_empty"
    Chars(33) = "d_daz"    '! dazzle icon
    Chars(34) = "d_col"    '" colosus icon
    Chars(35) = "d_wol"    '# wonverine icon
    Chars(36) = "d_dom"    '$ domino icon
    Chars(37) = "d_Power0" '%
    Chars(38) = "d_Power1" '&
    Chars(39) = "d_Power2" ''
    Chars(40) = "d_Power3" '(
    Chars(41) = "d_Power4" ')
    Chars(42) = "d_star"   '* ninja star
    Chars(43) = "d_plus"   '+
    Chars(44) = "d_chimi"  ' chimichangas
    Chars(45) = "d_minus"  '-
    Chars(46) = "d_dot"    '.
    Chars(47) = "d_gun"    '/ weapons
    Chars(48) = "d_0"      '0
    Chars(49) = "d_1"      '1
    Chars(50) = "d_2"      '2
    Chars(51) = "d_3"      '3
    Chars(52) = "d_4"      '4
    Chars(53) = "d_5"      '5
    Chars(54) = "d_6"      '6
    Chars(55) = "d_7"      '7
    Chars(56) = "d_8"      '8
    Chars(57) = "d_9"      '9
    Chars(60) = "d_less"   '<
    Chars(61) = "d_equal"  '=
    Chars(62) = "d_more"   '>
    Chars(64) = "bkempty"  '@
    Chars(65) = "d_a"      'A
    Chars(66) = "d_b"      'B
    Chars(67) = "d_c"      'C
    Chars(68) = "d_d"      'D
    Chars(69) = "d_e"      'E
    Chars(70) = "d_f"      'F
    Chars(71) = "d_g"      'G
    Chars(72) = "d_h"      'H
    Chars(73) = "d_i"      'I
    Chars(74) = "d_j"      'J
    Chars(75) = "d_k"      'K
    Chars(76) = "d_l"      'L
    Chars(77) = "d_m"      'M
    Chars(78) = "d_n"      'N
    Chars(79) = "d_o"      'O
    Chars(80) = "d_p"      'P
    Chars(81) = "d_q"      'Q
    Chars(82) = "d_r"      'R
    Chars(83) = "d_s"      'S
    Chars(84) = "d_t"      'T
    Chars(85) = "d_u"      'U
    Chars(86) = "d_v"      'V
    Chars(87) = "d_w"      'W
    Chars(88) = "d_x"      'X
    Chars(89) = "d_y"      'Y
    Chars(90) = "d_z"      'Z
    Chars(94) = "d_up"     '^
    '    Chars(95) = '_
    Chars(96) = "d_0a"        '0.
    Chars(97) = "d_1a"        '1. 'a
    Chars(98) = "d_2a"        '2. 'b
    Chars(99) = "d_3a"        '3. 'c
    Chars(100) = "d_4a"       '4. 'd
    Chars(101) = "d_5a"       '5. 'e
    Chars(102) = "d_6a"       '6. 'f
    Chars(103) = "d_7a"       '7. 'g
    Chars(104) = "d_8a"       '8. 'h
    Chars(105) = "d_9a"       '9. 'i
    Chars(106) = "d_LifeLon"  'j
    Chars(107) = "d_LifeMon"  'k
    Chars(108) = "d_LifeRon"  'l
    Chars(109) = "d_LifeLoff" 'm
    Chars(110) = "d_LifeMoff" 'n
    Chars(111) = "d_LifeRoff" 'o
    Chars(112) = ""           'p
    Chars(113) = ""           'q
    Chars(114) = ""           'r
    Chars(115) = ""           's
    Chars(116) = ""           't
    Chars(117) = ""           'u
    Chars(118) = ""           'v
    Chars(119) = ""           'w
    Chars(120) = ""           'x
    Chars(121) = ""           'y
    Chars(122) = ""           'z
    Chars(123) = ""           '{
    Chars(124) = ""           '|
    Chars(125) = ""           '}
    Chars(126) = ""           '~
End Sub

'********************
' Real Time updates
'********************
'used for all the real time updates

Sub Realtime_Timer
    RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
' add any other real time update subs, like gates or diverters, fl
'Start and stop crickets
if cricketsRunning = 0 and bMultiBallMode = True Then 
	StartCrickets
Else
	if cricketsRunning = 1 and bMultiBallMode = False Then StopCrickets
End If
'bACKGLASSLIGHTS
bg8.IntensityScale = li008.GetInPlayIntensity
bg9.IntensityScale = li009.GetInPlayIntensity
bg10.IntensityScale = li010.GetInPlayIntensity
bg11.IntensityScale = li011.GetInPlayIntensity
bg12.IntensityScale = li012.GetInPlayIntensity
bg13.IntensityScale = li013.GetInPlayIntensity
bg13a.IntensityScale = li013.GetInPlayIntensity
bgGI1.IntensityScale = gi035.GetInPlayIntensity
bgGI2.IntensityScale = gi035.GetInPlayIntensity
bgGI3.IntensityScale = gi035.GetInPlayIntensity
bgGI4.IntensityScale = gi035.GetInPlayIntensity
bgGI5.IntensityScale = gi035.GetInPlayIntensity
bgGI6.IntensityScale = gi035.GetInPlayIntensity
bgGI7.IntensityScale = gi035.GetInPlayIntensity
bgGI9.IntensityScale = gi035.GetInPlayIntensity
bgGI10.IntensityScale = gi035.GetInPlayIntensity
bgGI11.IntensityScale = gi035.GetInPlayIntensity
bgGI12.IntensityScale = gi035.GetInPlayIntensity
bgGI13.IntensityScale = gi035.GetInPlayIntensity
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
' 12 colors: red, orange, amber, yellow...
'******************************************

'colors
Const red = 1
Const orange = 2
Const amber = 3
Const yellow = 4
Const darkgreen = 5
Const green = 6
Const blue = 7
Const darkblue = 8
Const purple = 9
Const white = 10
Const teal = 11
Const ledwhite = 12

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
        Case white 'bulb
            n.color = RGB(193, 91, 0)
            n.colorfull = RGB(255, 197, 143)
        Case teal
            n.color = RGB(1, 64, 62)
            n.colorfull = RGB(2, 128, 126)
        Case ledwhite
            n.color = RGB(255, 197, 143)
            n.colorfull = RGB(255, 252, 224)
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
        Case white 'bulb
            n.color = RGB(255, 197, 143)
        Case teal
            n.color = RGB(2, 128, 126)
        Case ledwhite
            n.color = RGB(255, 252, 224)
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

Sub TurnOffArrows() 'during battles when changing modes
    For each x in aArrows
        x.State = 0
    Next
    For x = 1 to 6
        BattleLights(x) = 0
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
    If Score(1)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 1 " &FormatScore(Score(1))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    DMD "", "", "d_gameover", eNone, eNone, eBlink, 2000, False, ""
    If bFreePlay Then
        DMD "", CL(1, "FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits > 0 Then
            DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "", "", "d_jppresents", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 4000, False, "vo_deadpool"
    DMD "", CL(1, "ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0)), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    Dim a
    StartRainbow aArrows
    StartLightSeq
    DMDFlush
    ShowTableInfo
    a = RndNbr(2)
    Select Case a
        Case 1:PlaySong "mu_gameover"
        case 2:PlaySong "mu_boom-dp-rap"
    End Select
End Sub

Sub StopAttractMode
    StopRainbow
    DMDScoreNow
    LightSeqAttract.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
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

' droptargets, animations, timers, etc
Sub VPObjects_Init
	PilStop = 1: pillarSpin.enabled = 1
End Sub

' tables variables and Mode init
Dim bRotateLights
Dim UpdateArrowsCount
Dim Battle(4, 7)        'the first 4 is the current player, contains status of the battle, 0 not started, 1 won, 2 started
Dim LifeLeft(4, 7)      'life left of the enemies on each battle
Dim Dead(4, 4)          'dead lights
Dim Pool(4, 4)          'pool lights
Dim Boom(4, 4)          'boom lights
Dim BoomCount
Dim WeaponLights(4, 6)  'weapon lights
Dim DiamondLights(4, 6) 'diamond lights
Dim BumperHits(4)
Dim lilPos
Dim bChooseBattle
Dim bBattleReady
Dim Battlenr
Dim WolverinePower(4)
Dim DazzlePower(4)
Dim ColossusPower(4)
Dim DominoPower(4)
Dim NinjaStars(4)   'bonus, collected Stars
Dim Chimichangas(4) 'bonus. collected chimichangas
Dim Weapons(4)      'bonus, collected weapons
Dim OldCombo
Dim ComboCount
Dim ComboValue
Dim AttackPower
Dim WolverineValue
Dim bEndBattleJackpot
Dim bFirstBattle(4)
Dim BattlesWon(4)
Dim BattlesToChoose
Dim MegaJackpot(4)
Dim MegaSuperJCount
Dim MegaSuperJValue
Dim bLockEnabled
Dim DominoValue
Dim ColossusValue
Dim DazzlerValue
Dim bLilDPMB
Dim LilDPHitsNeeded(4)
Dim LilDPHits
Dim LilDPJackpot(4)
Dim LeftSpinnerCount(4)
Dim bDiscoEnabled
Dim bDiscoLoopsEnabled
Dim DiscoLoopsValue(4)
Dim bDiscoMBEnabled
Dim DiscoSuperJackpot(4)
Dim NinjaMBJackpot(4)
Dim NinjaMBCount         'count the Ninja Multiball jackpots to calculate the super jackpot
Dim NinjaApocJackpot(4)
Dim BattleLights(6)      'Arrows
Dim StarLights(6)        'Arrows
Dim ChimiChangaLights(6) 'Arrows
Dim LilDPLights(6)       'Arrows
Dim NinjaMBLights(6)     'Arrows
Dim MechSuitLights(6)    'Arrows
Dim bNinjaMB
Dim bNinjaMBSJackpot
Dim bMechSuitMBLight(4)
Dim bMechSuitMBStarted
Dim MechMBJackpot(4)
Dim bMechSuitMBSJackpot
Dim ChimichangaHits(4)
Dim ChimichangaHitsNeeded(4)
Dim ChimichangaValue(4)
Dim bBerserker
Dim BerserkerCount(4)
Dim BerserkerValue(4)
Dim bKatanaramaTime
Dim KatanaramaValue(4)
Dim KatanaramaCount(4)
Dim bInfoNeeded1(4) 'skillshot
Dim bInfoNeeded2(4) 'dead
Dim bInfoNeeded3(4) 'pool
Dim bInfoNeeded4(4) 'deadpool mystery
Dim bInfoNeeded5(4) 'deadpool regenerate
Dim bInfoNeeded6(4) 'top naes bonus x
Dim bBallLilDPLocked

Sub Game_Init()     'called at the start of a new game
    Dim i, j
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    'Play some Music

    'Init Variables
    bRotateLights = True
    UpdateArrowsCount = 0
    lilPos = 0
    Battlenr = 1
    bChooseBattle = False
    bEndBattleJackpot = False
    BattlesToChoose = 3
    MegaSuperJCount = 0
    BoomCount = 0
    OldCombo = ""
    ComboCount = 0
    ComboValue = 500000
    AttackPower = 1
    WolverineValue = 1
    bBattleReady = True
    bLockEnabled = True:SwordEffect 1
    DominoValue = 0
    ColossusValue = 1
    DazzlerValue = 0
    bLilDPMB = False 'True during LilDP multiball
    bDiscoEnabled = False
    bDiscoLoopsEnabled = False
    bDiscoMBEnabled = False
    bNinjaMB = False
    bNinjaMBSJackpot = False
    bMechSuitMBStarted = False
    bMechSuitMBSJackpot = False
    bBerserker = False
    bKatanaramaTime = False
    bBallLilDPLocked = False
    For i = 0 to 4
        BonusMultiplier(i) = 1
        BumperHits(i) = 0
        WolverinePower(i) = 2
        DazzlePower(i) = 2
        ColossusPower(i) = 2
        DominoPower(i) = 2
        BallsInLock(i) = 0 'current player
        SkillshotValue(i) = 250000
        SuperSkillshotValue(i) = 5000000
        PlayfieldMultiplier(i) = 1
        NinjaStars(i) = 0
        Chimichangas(i) = 0
        Weapons(i) = 0
        bFirstBattle(i) = True
        BattlesWon(i) = 0
        MegaJackpot(i) = 50000
        LilDPHitsNeeded(i) = 1
        LilDPJackpot(i) = 500000
        LeftSpinnerCount(i) = 0
        DiscoLoopsValue(i) = 350000
        DiscoSuperJackpot(i) = 500000
        NinjaMBJackpot(i) = 500000
        NinjaApocJackpot(i) = 5000000
        bMechSuitMBLight(i) = False
        MechMBJackpot(i) = 5000000
        ChimichangaHits(i) = 0
        ChimichangaHitsNeeded(i) = 1
        ChimichangaValue(i) = 500000
        BerserkerCount(i) = 0
        BerserkerValue(i) = 50000
        KatanaramaValue(i) = 1000000
        KatanaramaCount(i) = 0
        bInfoNeeded1(i) = True
        bInfoNeeded2(i) = True
        bInfoNeeded3(i) = True
        bInfoNeeded4(i) = True
        bInfoNeeded5(i) = True
        bInfoNeeded6(i) = True
    Next
    For i = 0 to 4
        For j = 0 to 7
            Battle(i, j) = 0
            LifeLeft(i, j) = 10
        Next
    Next
    For i = 0 to 4
        For j = 0 to 4
            Dead(i, j) = 0
            Pool(i, j) = 0
            Boom(i, j) = 0
        Next
    Next
    For i = 0 to 4
        For j = 0 to 6
            DiamondLights(i, j) = 0
            WeaponLights(i, j) = 1
        Next
    Next
    For i = 0 to 6
        BattleLights(i) = 0
        StarLights(i) = 0
        ChimiChangaLights(i) = 0
        LilDPLights(i) = 0
        NinjaMBLights(i) = 0
        MechSuitLights(i) = 0
    Next
    'Init Delays/Timers
    DiamondLights(1, 1) = 2 'enable the first Diamond light
    DiamondLights(2, 1) = 2
    DiamondLights(3, 1) = 2
    DiamondLights(4, 1) = 2
    UpdateArrows.Enabled = 1
'MainMode Init()
End Sub

Sub InstantInfo
    Dim tmp
    DMD CL(0, "INSTANT INFO"), "", "", eNone, eNone, eNone, 1000, False, ""
    If Score(1)Then
        DMD CL(0, "PLAYER 1 SCORE"), CL(1, FormatScore(Score(1))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2)Then
        DMD CL(0, "PLAYER 2 SCORE"), CL(1, FormatScore(Score(2))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3)Then
        DMD CL(0, "PLAYER 3 SCORE"), CL(1, FormatScore(Score(3))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4)Then
        DMD CL(0, "PLAYER 4 SCORE"), CL(1, FormatScore(Score(4))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    DMD CL(0, "COLLECTED"), CL(1, NinjaStars(CurrentPlayer) & " BOXES"), "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "COLLECTED"), CL(1, Chimichangas(CurrentPlayer) & " CRICKETS"), "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "COLLECTED"), CL(1, Weapons(CurrentPlayer) & " CONFIGURATIONS"), "", eNone, eNone, eNone, 1000, False, ""
    tmp = 25 - NinjaStars(CurrentPlayer)MOD 25
    DMD CL(0, "COLLECT " &tmp& " BOXES"), CL(1, "TO INCREASE PFX"), "", eNone, eNone, eNone, 1000, False, ""
    tmp = 50 - NinjaStars(CurrentPlayer)MOD 50
    DMD CL(0, "COLLECT " &tmp& " BOXES"), CL(1, "TO LIT EXTRA BALL"), "", eNone, eNone, eNone, 1000, False, ""
    tmp = 10 - Chimichangas(CurrentPlayer)MOD 10
    DMD CL(0, "COLLECT " &tmp& " CRICKETS"), CL(1, "TO INCREASE PFX"), "", eNone, eNone, eNone, 1000, False, ""
    tmp = 30 - Weapons(CurrentPlayer)MOD 30
    DMD CL(0, "COLLECT " &tmp& " BOXES"), CL(1, "TO LIT EXTRA BALL"), "", eNone, eNone, eNone, 1000, False, ""
    tmp = 50 - Weapons(CurrentPlayer)MOD 50
    DMD CL(0, "COLLECT " &tmp& " BOXES"), CL(1, "TO INCREASE PFX"), "", eNone, eNone, eNone, 1000, False, ""
    tmp = 75 - Weapons(CurrentPlayer)MOD 75
    DMD CL(0, "COLLECT " &tmp& " BOXES"), CL(1, "TO ELYSIUM MB"), "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "BONUS X"), CL(1, BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "BALLS LOCKED"), CL(1, BallsInLock(CurrentPLayer)), "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "JACKPOT VALUES"), "", "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "PILLAR MB JACKPOT"), CL(1, FormatScore(NinjaMBJackpot(CurrentPlayer))), "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "CHANNARD JACKPOT"), CL(1, FormatScore(MegaJackpot(CurrentPlayer))), "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "CHANNARD SUPJACKPOT"), CL(1, FormatScore(MegaJackpot(CurrentPlayer) * MegaSuperJCount)), "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "DERELICT JACKPOT"), CL(1, FormatScore(LilDPJackpot(CurrentPlayer))), "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "LAMENT MB JACKPOT"), CL(1, FormatScore(DiscoSuperJackpot(CurrentPlayer))), "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "ELYSIUM MB JACKPOT"), CL(1, FormatScore(MechMBJackpot(CurrentPlayer))), "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHEST SCORE"), CL(1, HighScoreName(0) & " " & HighScore(0)), "", eNone, eNone, eNone, 1000, False, ""
End Sub

Sub StopMBmodes                                   'stop multiball modes after loosing the last multibal
    If bLilDPMB Then StopLilDPMB:ResetDropTargets 'this stops the LilDP multiball and reset the targets and lights
    If bDiscoMBEnabled Then StopDiscoMB           'discoMB is on so stop it
    If bNinjaMB Then StopNinjaMB
    If bMechSuitMBStarted Then StopMechSuitMB
    If bBallLilDPLocked Then 'a ball is locked in the lilDP and no MB modes are running anymore, then just release the ball
        DropDownTargets
		frameDir = -1
		MoveFrame
        vpmtimer.addtimer 1500, "ResetDropTargets '"
    End If
    Trigger017.TimerEnabled = 1 'last check to release a locked ball
    changesong
End Sub

Sub StopEndOfBallMode() 'this sub is called after the last ball in play is drained, reset skillshot, modes, timers
    pfxtimer.Enabled = 0
    StopCombo
    StopBattle
    If bDiscoLoopsEnabled Then DiscoLoopsTimer_Timer 'stops the discolopp mode
    If bBerserker Then StopBerserker
    If bKatanaramaTime Then StopKatanarama
	frameDir = -1
	MoveFrame
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
    dim i
    'turn on or off the needed lights before a new ball is released
    TurnOffPlayfieldLights
    'lilDp droptargets
    ResetDropTargets
    LilDPHits = 0
    CloseGates
    'reset playfield multipiplier
    SetPlayfieldMultiplier 1
    If Balls = 1 then 'only on the first ball
        bBattleReady = True
        bLockEnabled = True:SwordEffect 1
    End If
    'update dead, pool, boom & other lights
    TurnOffArrows
    For i = 0 to 6
        BattleLights(i) = 0
        StarLights(i) = 0
        ChimiChangaLights(i) = 0
        LilDPLights(i) = 0
    Next
    TurnOnTeamUpLights
End Sub

Sub TurnOnTeamUpLights
    li073.State = 2
    li040.State = 2
    li074.State = 2
    li072.State = 2
End Sub

Sub TurnOffTeamUpLights
    li073.State = 0
    li040.State = 0
    li074.State = 0
    li072.State = 0
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub UpdateLights
    Dim i
    li021.State = Dead(CurrentPlayer, 1)
    li022.State = Dead(CurrentPlayer, 2)
    li023.State = Dead(CurrentPlayer, 3)
    li024.State = Dead(CurrentPlayer, 4)
    li017.State = Pool(CurrentPlayer, 1)
    li018.State = Pool(CurrentPlayer, 2)
    li019.State = Pool(CurrentPlayer, 3)
    li020.State = Pool(CurrentPlayer, 4)
    li006.State = Boom(CurrentPlayer, 1)
    li014.State = Boom(CurrentPlayer, 2)
    li015.State = Boom(CurrentPlayer, 3)
    li016.State = Boom(CurrentPlayer, 4)
    li045.State = WeaponLights(CurrentPlayer, 1)
    li028.State = WeaponLights(CurrentPlayer, 2)
    li042.State = WeaponLights(CurrentPlayer, 3)
    li046.State = WeaponLights(CurrentPlayer, 4)
    li032.State = WeaponLights(CurrentPlayer, 5)
    li033.State = WeaponLights(CurrentPlayer, 6)
    li025.State = DiamondLights(CurrentPlayer, 1)
    li029.State = DiamondLights(CurrentPlayer, 2)
    li026.State = DiamondLights(CurrentPlayer, 3)
    li030.State = DiamondLights(CurrentPlayer, 4)
    li027.State = DiamondLights(CurrentPlayer, 5)
    li031.State = DiamondLights(CurrentPlayer, 6)
    li008.State = Battle(CurrentPLayer, 1):If B2SON then :controller.B2SSetData 94, Battle(CurrentPLayer, 1):End If
    li010.State = Battle(CurrentPLayer, 2):If B2SON then :controller.B2SSetData 93, Battle(CurrentPLayer, 2):End If
    li011.State = Battle(CurrentPLayer, 3):If B2SON then :controller.B2SSetData 92, Battle(CurrentPLayer, 3):End If
    li007.State = Battle(CurrentPLayer, 4):If B2SON then :controller.B2SSetData 90, Battle(CurrentPLayer, 4):End If
    li009.State = Battle(CurrentPLayer, 5):If B2SON then :controller.B2SSetData 95, Battle(CurrentPLayer, 5):End If
    li012.State = Battle(CurrentPLayer, 6):If B2SON then :controller.B2SSetData 91, Battle(CurrentPLayer, 6):End If
    li013.State = Battle(CurrentPLayer, 7)
    If bBattleReady Then
        li056.State = 2
        li070.State = 2
    Else
        li056.State = 0
        li070.State = 0
    End If
    If bLockEnabled Then
        li048.State = 2
    Else
        li048.State = 0
    End If
    If BoomCount > 0 then
        BoomLight.State = 2
    Else
        BoomLight.State = 0
    End If
    If bNinjaMBSJackpot Then
        SetLightColor li058, teal, 2
    ElseIf bSkillShotReady Then
        SetLightColor li058, red, 2
    End If
    If bMultiBallMode Then
        li060.state = 2:li061.State = 2
    Else
        li060.state = 0:li061.State = 0
    End If
    If bMechSuitMBLight(CurrentPlayer)Then
        li047.State = 1
    End If
End Sub

Sub UpdateArrows_Timer 'timer change the color of the blinking lights according to the Battle or Mode active
    'Update other lights
    UpdateLights
    Select Case UpdateArrowsCount
        Case 0 'Battle lights
            Select Case Battle(CurrentPlayer, 0)
                Case 1
                    If BattleLights(1) = 2 Then SetlightColor li054, red, 2
                    If BattleLights(2) = 2 Then SetlightColor li057, red, 2
                    If BattleLights(3) = 2 Then SetlightColor li055, red, 2
                    If BattleLights(4) = 2 Then SetlightColor li071, red, 2
                    If BattleLights(5) = 2 Then SetlightColor li058, red, 2
                    If BattleLights(6) = 2 Then SetlightColor li059, red, 2
                Case 2
                    If BattleLights(1) = 2 Then SetlightColor li054, white, 2
                    If BattleLights(2) = 2 Then SetlightColor li057, white, 2
                    If BattleLights(3) = 2 Then SetlightColor li055, white, 2
                    If BattleLights(4) = 2 Then SetlightColor li071, white, 2
                    If BattleLights(5) = 2 Then SetlightColor li058, white, 2
                    If BattleLights(6) = 2 Then SetlightColor li059, white, 2
                Case 3
                    If BattleLights(1) = 2 Then SetlightColor li054, yellow, 2
                    If BattleLights(2) = 2 Then SetlightColor li057, yellow, 2
                    If BattleLights(3) = 2 Then SetlightColor li055, yellow, 2
                    If BattleLights(4) = 2 Then SetlightColor li071, yellow, 2
                    If BattleLights(5) = 2 Then SetlightColor li058, yellow, 2
                    If BattleLights(6) = 2 Then SetlightColor li059, yellow, 2
                Case 4
                    If BattleLights(1) = 2 Then SetlightColor li054, darkgreen, 2
                    If BattleLights(2) = 2 Then SetlightColor li057, darkgreen, 2
                    If BattleLights(3) = 2 Then SetlightColor li055, darkgreen, 2
                    If BattleLights(4) = 2 Then SetlightColor li071, darkgreen, 2
                    If BattleLights(5) = 2 Then SetlightColor li058, darkgreen, 2
                    If BattleLights(6) = 2 Then SetlightColor li059, darkgreen, 2
                Case 5
                    If BattleLights(1) = 1 Then SetlightColor li054, blue, 1
                    If BattleLights(2) = 1 Then SetlightColor li057, blue, 1
                    If BattleLights(3) = 1 Then SetlightColor li055, blue, 1
                    If BattleLights(4) = 1 Then SetlightColor li071, blue, 1
                    If BattleLights(5) = 1 Then SetlightColor li058, blue, 1
                    If BattleLights(6) = 1 Then SetlightColor li059, blue, 1
                    If BattleLights(1) = 2 Then SetlightColor li054, red, 2
                    If BattleLights(2) = 2 Then SetlightColor li057, red, 2
                    If BattleLights(3) = 2 Then SetlightColor li055, red, 2
                    If BattleLights(4) = 2 Then SetlightColor li071, red, 2
                    If BattleLights(5) = 2 Then SetlightColor li058, red, 2
                    If BattleLights(6) = 2 Then SetlightColor li059, red, 2
                Case 6
                    If BattleLights(1) = 2 Then SetlightColor li054, green, 2
                    If BattleLights(2) = 2 Then SetlightColor li057, green, 2
                    If BattleLights(3) = 2 Then SetlightColor li055, green, 2
                    If BattleLights(4) = 2 Then SetlightColor li071, green, 2
                    If BattleLights(5) = 2 Then SetlightColor li058, green, 2
                    If BattleLights(6) = 2 Then SetlightColor li059, green, 2
                Case 7
                    If BattleLights(1) = 2 Then SetlightColor li054, darkblue, 2
                    If BattleLights(2) = 2 Then SetlightColor li057, darkblue, 2
                    If BattleLights(3) = 2 Then SetlightColor li055, darkblue, 2
                    If BattleLights(4) = 2 Then SetlightColor li071, darkblue, 2
                    If BattleLights(5) = 2 Then SetlightColor li058, darkblue, 2
                    If BattleLights(6) = 2 Then SetlightColor li059, darkblue, 2
            End Select
        Case 1 'Chimichangas
            If ChimiChangaLights(1) = 2 Then SetlightColor li054, orange, 2
            If ChimiChangaLights(2) = 2 Then SetlightColor li057, orange, 2
            If ChimiChangaLights(3) = 2 Then SetlightColor li055, orange, 2
            If ChimiChangaLights(4) = 2 Then SetlightColor li071, orange, 2
            If ChimiChangaLights(5) = 2 Then SetlightColor li058, orange, 2
            If ChimiChangaLights(6) = 2 Then SetlightColor li059, orange, 2
        Case 2 'Lil Deadpool
            If LilDPLights(1) = 2 Then SetlightColor li054, amber, 2
            If LilDPLights(2) = 2 Then SetlightColor li057, amber, 2
            If LilDPLights(3) = 2 Then SetlightColor li055, amber, 2
            If LilDPLights(4) = 2 Then SetlightColor li071, amber, 2
            If LilDPLights(5) = 2 Then SetlightColor li058, amber, 2
            If LilDPLights(6) = 2 Then SetlightColor li059, amber, 2
        Case 3 'Ninja Multiball jackpot lights
            If NinjaMBLights(1) = 2 Then SetlightColor li054, teal, 2
            If NinjaMBLights(2) = 2 Then SetlightColor li057, teal, 2
            If NinjaMBLights(3) = 2 Then SetlightColor li055, teal, 2
            If NinjaMBLights(4) = 2 Then SetlightColor li071, teal, 2
            If NinjaMBLights(5) = 2 Then SetlightColor li058, teal, 2
            If NinjaMBLights(6) = 2 Then SetlightColor li059, teal, 2
        Case 4 'Stars
            If StarLights(1) = 2 Then SetlightColor li054, purple, 2
            If StarLights(2) = 2 Then SetlightColor li057, purple, 2
            If StarLights(3) = 2 Then SetlightColor li055, purple, 2
            If StarLights(4) = 2 Then SetlightColor li071, purple, 2
            If StarLights(5) = 2 Then SetlightColor li058, purple, 2
            If StarLights(6) = 2 Then SetlightColor li059, purple, 2
        Case 5 'Mech Suit Multiball
            If MechSuitLights(1) = 2 Then SetlightColor li054, red, 2
            If MechSuitLights(2) = 2 Then SetlightColor li057, red, 2
            If MechSuitLights(3) = 2 Then SetlightColor li055, red, 2
            If MechSuitLights(4) = 2 Then SetlightColor li071, red, 2
            If MechSuitLights(5) = 2 Then SetlightColor li058, red, 2
            If MechSuitLights(6) = 2 Then SetlightColor li059, red, 2
    End Select
    UpdateArrowsCount = (UpdateArrowsCount + 1)MOD 6
End Sub

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
    LightSeqSkillshot.Play SeqAllOff
    DMD CL(0, "HIT LIT LIGHT"), CL(1, "FOR SKILLSHOT"), "", eNone, eNone, eNone, 1500, True, ""
    li050.State = 2
    SetLightColor li058, Red, 2
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    bRotateLights = True
    LightSeqSkillshot.StopPlay
    If Li049.State = 2 Then Li049.State = 0
    If Li050.State = 2 Then Li050.State = 0
    If Li051.State = 2 Then Li051.State = 0
    If Li052.State = 2 Then Li052.State = 0
    Li058.State = 0
    CloseGates
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
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    StopCombo
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
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
    DOF 106, DOFPulse
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
    StopCombo
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'*********
' Rubbers
'*********

Sub rlband004_Hit:Addscore 110:End Sub
Sub rlband003_Hit:Addscore 110:End Sub
Sub rlband007_Hit:Addscore 110:End Sub

'***********************
' Bumpers - Spawn Ninja
'***********************

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), Bumper1
    DOF 138, DOFPulse
    FlashForms Flasher008, 1500, 75, 0
    ' add some points
    AddScore 1000
    ' check for modes
    StopCombo
    Select Case Battle(CurrentPLayer, 0)
        Case 0:PlaySound "sfx_sword1"
        Case 3 'Sabretooth
            Addscore DominoValue
            PlaySound "sfx_hit1"
            CheckBattle
    End Select

    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper1"
    CheckBumpers
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), Bumper2
    DOF 138, DOFPulse
    FlashForms Flasher009, 1500, 75, 0
    ' add some points
    AddScore 1000
    ' check for modes
    StopCombo
    Select Case Battle(CurrentPLayer, 0)
        Case 0:PlaySound "sfx_sword2"
        Case 3 'Sabretooth
            Addscore DominoValue
            PlaySound "sfx_hit2"
            CheckBattle
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper2"
    CheckBumpers
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper3
    DOF 138, DOFPulse
    FlashForms Flasher010, 1500, 75, 0
    ' add some points
    AddScore 1000
    ' check for modes
    StopCombo
    Select Case Battle(CurrentPLayer, 0)
        Case 0:PlaySound "sfx_sword3"
        Case 3 'Sabretooth
            Addscore DominoValue
            PlaySound "sfx_hit3"
            CheckBattle
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper3"
    CheckBumpers
End Sub

' Check the bumper hits - spawn Ninjas

Sub CheckBumpers()
    ' increase the bumper hit count and increase the bumper value after each 30 hits
    BumperHits(CurrentPlayer) = BumperHits(CurrentPlayer) + 1
    If BumperHits(CurrentPlayer)MOD 10 = 0 Then ' spawn a Ninja Star - purple Arrow light
        SpawnNinja
    End If
End Sub

Sub SpawnNinja
    Dim tmp
    tmp = RndNbr(6)
    StarLights(tmp) = 2
End Sub

Sub CheckNinjaStars
    NinjaStars(CurrentPlayer) = NinjaStars(CurrentPlayer) + 1
    If NinjaStars(CurrentPlayer)MOD 50 = 0 Then 'light extra ball
        DMD "_", CL(1, "EXTRA BALL IS LIT"), "", eNone, eNone, eNone, 1000, True, "vo_lildp_xtraball"
        li038.State = 2
    End If
    If NinjaStars(CurrentPlayer)MOD 25 = 0 Then 'increase pfx
        AddPlayfieldMultiplier 1
    End If
End Sub

'*******************************
' Top lanes: Bonus X /Skillshot
'*******************************
' score 2.500.000, 250.000 or 1000

Sub Trigger006_Hit
    PlaySoundAt "fx_sensor", Trigger006
    If Tilted Then Exit Sub
    ' check for modes
    StopCombo
    If bSkillShotReady AND li049.State = 2 Then
        AwardSkillshot
    Else
        Addscore 1000
        li049.State = 1
        CheckBonusX
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger006"
End Sub

Sub Trigger007_Hit
    PlaySoundAt "fx_sensor", Trigger007
    If Tilted Then Exit Sub
    ' check for modes
    StopCombo
    If bSkillShotReady AND li050.State = 2 Then
        AwardSkillshot
    Else
        Addscore 1000
        li050.State = 1
        CheckBonusX
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger007"
End Sub

Sub Trigger008_Hit
    PlaySoundAt "fx_sensor", Trigger008
    If Tilted Then Exit Sub
    ' check for modes
    StopCombo
    If bSkillShotReady AND li051.State = 2 Then
        AwardSkillshot
    Else
        Addscore 1000
        li051.State = 1
        CheckBonusX
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger008"
End Sub

Sub Trigger009_Hit
    PlaySoundAt "fx_sensor", Trigger009
    If Tilted Then Exit Sub
    ' check for modes
    StopCombo
    Select Case Battle(CurrentPLayer, 0)
        Case 1
            If li054.State = 2 Then CheckBattle
    End Select
    If bSkillShotReady AND li052.State = 2 Then
        AwardSkillshot
    Else
        Addscore 1000
        li052.State = 1
        CheckBonusX
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger009"
End Sub

Sub CheckBonusX
    If Li049.State + Li050.State + Li051.State + Li052.State = 4 Then
        AddBonusMultiplier 1
        DMD CL(0, "BONUS MULTIPLIER"), CL(1, BonusMultiplier(CurrentPlayer) & "X"), "", eNone, eNone, eNone, 1500, True, "Fanfare"
        Addscore 15000
        GiEffect 1
        FlashForMs Li049, 1500, 100, 0
        FlashForMs Li050, 1500, 100, 0
        FlashForMs Li051, 1500, 100, 0
        FlashForMs Li052, 1500, 100, 0
        FlashForms Flasher008, 1500, 100, 0
        FlashForms Flasher009, 1500, 100, 0
        FlashForms Flasher010, 1500, 100, 0
    ElseIf bInfoNeeded6(CurrentPLayer)Then
        PlaySound "vo_completetoplanes"
        bInfoNeeded6(CurrentPLayer) = False
    End IF
End Sub

'**************************
' Out/inLanes : BOOM lanes
'**************************

Sub Trigger001_Hit
    PlaySoundAt "fx_sensor", Trigger001
    If Tilted Then Exit Sub
    ' check for modes
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    If li075.State Then                   'Regenerate
        If NOT bExtraBallWonThisBall Then 'same as extra ball
            DMD "_", CL(1, ("REGENERATE")), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
            DOF 121, DOFPulse
            ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
            bExtraBallWonThisBall = True
            GiEffect 2
            LightEffect 2
        END If
    End If
    Addscore 5000
    Boom(CurrentPlayer, 1) = 1
    CheckBOOM
End Sub

Sub Trigger002_Hit
    PlaySoundAt "fx_sensor", Trigger002
    If Tilted Then Exit Sub
    ' check for modes
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    Addscore 1000
    Boom(CurrentPlayer, 2) = 1
    CheckBOOM
End Sub

Sub Trigger003_Hit
    PlaySoundAt "fx_sensor", Trigger003
    If Tilted Then Exit Sub
    ' check for modes
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    Addscore 1000
    Boom(CurrentPlayer, 3) = 1
    CheckBOOM
End Sub

Sub Trigger004_Hit
    PlaySoundAt "fx_sensor", Trigger004
    If Tilted Then Exit Sub
    ' check for modes
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    Addscore 5000
    Boom(CurrentPlayer, 4) = 1
    CheckBOOM
End Sub

Sub CheckBOOM
    Dim tmp, i
    tmp = 0
    For i = 1 to 4:tmp = tmp + Boom(CurrentPlayer, i):Next
    If tmp = 4 Then
        PlaySound "vo_boombuttonisready"
        BoomCount = BoomCount + 1
        For i = 1 to 4:Boom(CurrentPlayer, i) = 0:Next
        If BoomCount > 3 Then
            SetLightColor BoomLight, red, 2
        Else
            SetLightColor BoomLight, white, 2
        End If
    End If
End Sub

Sub BoomHit
    If BoomCount > 0 Then
        'PlaySound "vo_boom"
        DMD " ", " ", "d_boom", eNone, eNone, eNone, 1500, True, "vo_boom"
        If Battle(CurrentPlayer, 0) > 0 Then
            LifeLeft(CurrentPlayer, Battle(CurrentPlayer, 0)) = LifeLeft(CurrentPlayer, Battle(CurrentPlayer, 0))- 4
            CheckBattle
        'DMDScoreNow
        ElseIf BoomCount > 3 Then 'not in a battle
            Addscore 5000000
        Else
            Addscore 500000
        End If
        BoomCount = BoomCount - 1
    End If
End Sub

'*************
' Battle lanes
'*************
' also used for combos and other modes

Sub Trigger011_Hit 'colossus loop
    PlaySoundAt "fx_sensor", Trigger011
    If Tilted Then Exit Sub
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    Addscore 10000
    If li039.State = 2 Then
        li039.State = 0
        AwardSpecial
    End If
    ' check for modes
    If ChimiChangaLights(3) = 2 Then 'collect chimichanga
        ChimiChangaLights(3) = 0:Li055.State = 0
        Collectchimichanga
    End If
    If MechSuitLights(3) = 2 Then 'collect jackpot
        MechSuitLights(3) = 0:Li055.State = 0
        CheckMechSuitMBHits
    End If
    If WeaponLights(CurrentPlayer, 3) = 1 Then 'collect weapon
        WeaponLights(CurrentPlayer, 3) = 0:Li042.State = 0
        CheckWeapons
    End If
    If StarLights(3) = 2 Then 'a ninja star is at that position
        StarLights(3) = 0:Li055.State = 0
        CheckNinjaStars
    End If
    If bNinjaMB AND NinjaMBLights(3) = 2 Then 'Ninja Multiball
        NinjaMBLights(3) = 0:li055.State = 0
        NinjaMBCheckHits
    End If
    If bLilDPMB AND LilDPHits > 8 AND LilDPLights(3) = 2 Then 'second part of the LilDP multiball
        LilDPLights(3) = 0:Li055.State = 0
        LilDPCheckHits
    End If
    Select Case Battle(CurrentPLayer, 0)
        Case 0
            ColossusPower(CurrentPlayer) = ColossusPower(CurrentPlayer) + 1
            CheckColossus
        Case 1, 4, 6, 7
            If BattleLights(3) = 2 Then PlaySound "sfx_hit1":CheckBattle
        Case 5
            If BattleLights(3) = 2 Then
                PlaySound "sfx_hit1":AwardSuperMegaJackpot:CheckBattle
            ElseIf BattleLights(3) = 1 Then
                BattleLights(3) = 0:li055.State = 0:AwardMegaJackpot
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger011"
End Sub

Sub Trigger010_Hit 'left loop
    PlaySoundAt "fx_sensor", Trigger010
    If Tilted Then Exit Sub
    Addscore 10000
    ' check for modes
    If bKatanaramaTime Then
        Addscore KatanaramaValue(CurrentPlayer)
        DMD " PILLAR OF SOULS JACKPOT", CL(1, FormatScore(KatanaramaValue(CurrentPlayer))), "", eNone, eBlink, eNone, 1500, True, "vo_jackpot"
        KatanaramaValue(CurrentPlayer) = KatanaramaValue(CurrentPlayer) + 250000
    End If
    If ChimiChangaLights(1) = 2 Then 'collect chimichanga
        ChimiChangaLights(1) = 0:Li054.State = 0
        Collectchimichanga
    End If
    If MechSuitLights(1) = 2 Then 'collect jackpot
        MechSuitLights(1) = 0:Li054.State = 0
        CheckMechSuitMBHits
    End If
    If WeaponLights(CurrentPlayer, 1) = 1 Then 'collect weapon
        WeaponLights(CurrentPlayer, 1) = 0:li045.State = 0
        CheckWeapons
    End If
    If StarLights(1) = 2 Then 'a ninja star is at that position
        StarLights(1) = 0:li054.State = 0
        CheckNinjaStars
    End If
    If bNinjaMB AND NinjaMBLights(1) = 2 Then 'Ninja Multiball
        NinjaMBLights(1) = 0:li054.State = 0
        NinjaMBCheckHits
    End If
    If bLilDPMB AND LilDPHits > 8 AND LilDPLights(1) = 2 Then 'second part of the LilDP multiball
        LilDPCheckHits
        LilDPLights(1) = 0:li054.State = 0
    End If
    If bDiscoLoopsEnabled AND LastSwitchHit = "Trigger005" Then
        Jackpot(CurrentPlayer) = DiscoLoopsValue(CurrentPlayer)
        DiscoLoopsValue(CurrentPlayer) = DiscoLoopsValue(CurrentPlayer) + 150000
        AwardJackpot
    End If
    If ActiveBall.VelY < 0 Then 'ball is going up
        Select Case Battle(CurrentPLayer, 0)
            Case 0
                DazzlePower(CurrentPlayer) = DazzlePower(CurrentPlayer) + 1
                CheckDazzler
            Case 1, 4, 6, 7
                If BattleLights(1) = 2 Then PlaySound "sfx_hit2":CheckBattle
            Case 5
                If BattleLights(1) = 2 Then
                    PlaySound "sfx_hit2":AwardSuperMegaJackpot:CheckBattle
                ElseIf BattleLights(1) = 1 Then
                    li054.State = 0:BattleLights(1) = 0:AwardMegaJackpot
                End If
        End Select
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger010"
End Sub

Sub Trigger005_Hit 'right loop
    PlaySoundAt "fx_sensor", Trigger005
    If Tilted Then Exit Sub
    If bSkillshotReady Then
        bRotateLights = False
        Exit Sub
    Else
        Addscore 10000
    End If
    ' check for modes
    If bKatanaramaTime Then
        Addscore KatanaramaValue(CurrentPlayer)
        DMD " PILLAR OF SOULS JACKPOT", CL(1, FormatScore(KatanaramaValue(CurrentPlayer))), "", eNone, eBlink, eNone, 1500, True, "vo_jackpot"
        KatanaramaValue(CurrentPlayer) = KatanaramaValue(CurrentPlayer) + 250000
    End If
    If ChimiChangaLights(6) = 2 Then 'if the chimichanga light is on then collect it
        ChimiChangaLights(6) = 0:Li059.State = 0
        CollectChimichanga
    Else 'count the chimichanga hits
        CheckChimichangaHits
    End If
    If MechSuitLights(6) = 2 Then 'collect jackpot
        MechSuitLights(6) = 0:Li059.State = 0
        CheckMechSuitMBHits
    End If
    If WeaponLights(CurrentPlayer, 6) = 1 Then 'collect weapon
        WeaponLights(CurrentPlayer, 6) = 0:li033.State = 0
        CheckWeapons
    End If
    If StarLights(6) = 2 Then 'a ninja star is at that position
        StarLights(6) = 0:li059.State = 0
        CheckNinjaStars
    End If
    If bNinjaMB AND NinjaMBLights(6) = 2 Then 'Ninja Multiball
        NinjaMBLights(6) = 0:li059.State = 0
        NinjaMBCheckHits
    End If
    If bLilDPMB AND LilDPHits > 8 AND LilDPLights(6) = 2 Then 'second part of the LilDP multiball
        LilDPCheckHits
        li059.State = 0:LilDPLights(6) = 0
    End If
    If bDiscoLoopsEnabled AND LastSwitchHit = "Trigger005" Then
        Jackpot(CurrentPlayer) = DiscoLoopsValue(CurrentPlayer)
        DiscoLoopsValue(CurrentPlayer) = DiscoLoopsValue(CurrentPlayer) + 150000
        AwardJackpot
    End If
    Select Case Battle(CurrentPLayer, 0)
        Case 0
            If bSkillshotReady = False Then
                DominoPower(CurrentPlayer) = DominoPower(CurrentPlayer) + 1
                CheckDomino
            End If
        Case 1, 4, 6, 7
            If BattleLights(6) = 2 Then PlaySound "sfx_hit3":CheckBattle
        Case 5
            If BattleLights(6) = 2 Then
                PlaySound "sfx_hit3":AwardSuperMegaJackpot:CheckBattle
            ElseIf BattleLights(6) = 1 Then
                li059.State = 0:BattleLights(6) = 0:AwardMegaJackpot
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger005"
End Sub

Sub Trigger012_Hit 'left ramp
    PlaySoundAt "fx_sensor", Trigger012
    If Tilted Then Exit Sub
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    Addscore 10000
    ' check for modes
    If bKatanaramaTime Then
        Addscore KatanaramaValue(CurrentPlayer)
        DMD " PILLAR OF SOULS JACKPOT", CL(1, FormatScore(KatanaramaValue(CurrentPlayer))), "", eNone, eNone, eNone, 1500, True, "vo_jackpot"
        KatanaramaValue(CurrentPlayer) = KatanaramaValue(CurrentPlayer) + 250000
    ElseIf DiamondLights(CurrentPLayer, 1) = 2 OR DiamondLights(CurrentPLayer, 3) = 2 OR DiamondLights(CurrentPLayer, 5) = 2 Then
        CheckKatanarama
    End If
    If ChimiChangaLights(2) = 2 Then 'collect chimichanga
        ChimiChangaLights(2) = 0:Li057.State = 0
        Collectchimichanga
    End If
    If MechSuitLights(2) = 2 Then 'collect jackpot
        MechSuitLights(2) = 0:Li057.State = 0
        CheckMechSuitMBHits
    End If
    If WeaponLights(CurrentPlayer, 2) = 1 Then 'collect weapon
        WeaponLights(CurrentPlayer, 2) = 0:li028.State = 0
        CheckWeapons
    End If
    If StarLights(2) = 2 Then 'a ninja star is at that position
        StarLights(2) = 0:li057.State = 0
        CheckNinjaStars
    End If
    If bNinjaMB AND NinjaMBLights(2) = 2 Then 'Ninja Multiball
        NinjaMBLights(2) = 0:li057.State = 0
        NinjaMBCheckHits
    End If
    If bLilDPMB AND LilDPHits > 8 AND LilDPLights(2) = 2 Then 'second part of the LilDP multiball
        LilDPCheckHits
        li057.State = 0:LilDPLights(2) = 0
    End If
    If bDiscoMBEnabled Then
        SuperJackpot(CurrentPlayer) = DiscoSuperJackpot(CurrentPlayer)
        AwardSuperjackpot
    End If
    CheckCombo "Trigger012"
    Select Case Battle(CurrentPLayer, 0)
        Case 1, 2, 4, 6
            If BattleLights(2) = 2 Then PlaySound "sfx_hit4":CheckBattle
        Case 5
            If BattleLights(2) = 2 Then
                PlaySound "sfx_hit4":AwardSuperMegaJackpot:CheckBattle
            ElseIf BattleLights(2) = 1 Then
                li057.State = 0:BattleLights(2) = 0:AwardMegaJackpot
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger012"
End Sub

Sub Trigger016_Hit 'just a quote trigger
    Playquote
End Sub

Sub Trigger013_Hit 'right ramp
    PlaySoundAt "fx_sensor", Trigger013
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        AwardSuperSkillShot
    Else
        Addscore 10000
    End If
    ' check for modes
    If LastSwitchHit = "Target014" Then 'Increase Playfield multiplier
        AddPlayfieldMultiplier 1
    End If
    If bDiscoMBEnabled Then
        SuperJackpot(CurrentPlayer) = DiscoSuperJackpot(CurrentPlayer)
        AwardSuperjackpot
    End If
    CheckCombo "Trigger013"
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger013"

'********pillar activation
	pillarSpin.enabled = 1
End Sub

Sub Trigger014_Hit 'wolverine loop
    PlaySoundAt "fx_sensor", Trigger014
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        AwardSuperSkillShot
    Else
        Addscore 10000
    End If
    ' check for modes
    If ChimiChangaLights(4) = 2 Then 'collect chimichanga
        ChimiChangaLights(4) = 0:Li071.State = 0
        Collectchimichanga
    End If
    If MechSuitLights(4) = 2 Then 'collect jackpot
        MechSuitLights(4) = 0:Li071.State = 0
        CheckMechSuitMBHits
    End If
    If WeaponLights(CurrentPlayer, 4) = 1 Then 'collect weapon
        WeaponLights(CurrentPlayer, 4) = 0:li046.State = 0
        CheckWeapons
    End If
    If StarLights(4) = 2 Then 'a ninja star is at that position
        StarLights(4) = 0:li071.State = 0
        CheckNinjaStars
    End If
    If bNinjaMB AND NinjaMBLights(4) = 2 Then 'Ninja Multiball
        NinjaMBLights(4) = 0:li071.State = 0
        NinjaMBCheckHits
    End If
    If bLilDPMB AND LilDPHits > 8 AND LilDPLights(4) = 2 Then 'second part of the LilDP multiball
        LilDPCheckHits
        li071.State = 0:LilDPLights(4) = 0
    End If
    CheckCombo "Trigger014"
    Select Case Battle(CurrentPLayer, 0)
        Case 0
            WolverinePower(CurrentPlayer) = WolverinePower(CurrentPlayer) + 1
            CheckWolverine
        Case 2, 4, 6, 7
            If BattleLights(4) = 2 Then PlaySound "sfx_hit1":CheckBattle
        Case 5
            If BattleLights(4) = 2 Then
                PlaySound "sfx_hit1":AwardSuperMegaJackpot:CheckBattle
            ElseIf BattleLights(4) = 1 Then
                li071.State = 0:BattleLights(4) = 0:AwardMegaJackpot
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger014"
End Sub

Sub Trigger015_Hit 'cross shot
    PlaySoundAt "fx_sensor", Trigger015
    If Tilted Then Exit Sub
    Addscore 10000
    ' check for modes
    If bKatanaramaTime Then
        Addscore KatanaramaValue(CurrentPlayer)
        DMD " PILLAR OF SOULS JACKPOT", CL(1, FormatScore(KatanaramaValue(CurrentPlayer))), "", eNone, eNone, eNone, 1500, True, "vo_jackpot"
        KatanaramaValue(CurrentPlayer) = KatanaramaValue(CurrentPlayer) + 250000
    ElseIf DiamondLights(CurrentPLayer, 2) = 2 OR DiamondLights(CurrentPLayer, 4) = 2 OR DiamondLights(CurrentPLayer, 6) = 2 Then
        CheckKatanarama
    End If
    If ChimiChangaLights(5) = 2 Then 'collect chimichanga
        ChimiChangaLights(5) = 0:Li058.State = 0
        Collectchimichanga
    End If
    If MechSuitLights(5) = 2 Then 'collect jackpot
        MechSuitLights(5) = 0:Li058.State = 0
        CheckMechSuitMBHits
    End If
    If WeaponLights(CurrentPlayer, 5) = 1 Then 'collect weapon
        WeaponLights(CurrentPlayer, 5) = 0:li032.State = 0
        CheckWeapons
    End If
    If StarLights(5) = 2 Then 'a ninja star is at that position
        StarLights(5) = 0:li058.State = 0
        CheckNinjaStars
    End If
    If bNinjaMB AND NinjaMBLights(5) = 2 Then 'Ninja Multiball
        NinjaMBLights(5) = 0:li058.State = 0
        NinjaMBCheckHits
    End If
    If bLilDPMB AND LilDPHits > 8 AND LilDPLights(5) = 2 Then 'second part of the LilDP multiball
        LilDPCheckHits
        li058.State = 0:LilDPLights(5) = 0
    End If
    Select Case Battle(CurrentPLayer, 0)
        Case 1, 2, 4, 6
            If BattleLights(5) = 2 Then PlaySound "sfx_hit2":CheckBattle
        Case 5
            If BattleLights(5) = 2 Then
                PlaySound "sfx_hit2":AwardSuperMegaJackpot:CheckBattle
            ElseIf BattleLights(5) = 1 Then
                li058.State = 0:BattleLights(5) = 0:AwardMegaJackpot
            End If
    End Select
End Sub

dim demonActive : demonActive = 0

Sub Trigger017_Hit 'LilDP Ball locked
    bBallLilDPLocked = True
	if demonActive = 1 then Exit Sub
		demonActive = 1
		frameDir = 1
		MoveFrame
End Sub

Sub Trigger017_UnHit 'LilDP Ball locked has been released
    bBallLilDPLocked = False
End Sub

Sub Trigger017_Timer 'LilDP Ball lock last check
    If bBallLilDPLocked Then
        DropDownTargets
        vpmtimer.addtimer 1500, "ResetDropTargets '"
    Else
        Trigger017.TimerEnabled = 0
    End If
End Sub

'**************
' DEAD targets
'**************

Sub Target001_Hit 'lower d
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    Addscore 1000
    ' check for modes
    StopCombo
    Select Case Battle(CurrentPlayer, 0)
        Case 0 'no battle active
            Dead(CurrentPlayer, 1) = 1
            CheckDead
        Case 1 'Juggernaut
            If LifeLeft(CurrentPlayer, 1) > 6 Then
                Addscore DominoValue
                PlaySound "sfx_hit1"
                CheckBattle
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target001"
End Sub

Sub Target002_Hit 'e
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    Addscore 1000
    ' check for modes
    StopCombo
    Select Case Battle(CurrentPlayer, 0)
        Case 0 'no battle active
            Dead(CurrentPlayer, 2) = 1
            CheckDead
        Case 1 'Juggernaut
            If LifeLeft(CurrentPlayer, 1) > 6 Then
                Addscore DominoValue
                PlaySound "sfx_hit2"
                CheckBattle
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target002"
End Sub

Sub Target003_Hit 'a
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    Addscore 1000
    ' check for modes
    StopCombo
    Select Case Battle(CurrentPlayer, 0)
        Case 0 'no battle active
            Dead(CurrentPlayer, 3) = 1
            CheckDead
        Case 1 'Juggernaut
            If LifeLeft(CurrentPlayer, 1) > 6 Then
                Addscore DominoValue
                PlaySound "sfx_hit3"
                CheckBattle
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target003"
End Sub

Sub Target004_Hit 'd
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    Addscore 1000
    ' check for modes
    StopCombo
    Select Case Battle(CurrentPlayer, 0)
        Case 0 'no battle active
            Dead(CurrentPlayer, 4) = 1
            CheckDead
        Case 1 'Juggernaut
            If LifeLeft(CurrentPlayer, 1) > 6 Then
                Addscore DominoValue
                PlaySound "sfx_hit4"
                CheckBattle
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target004"
End Sub

Sub CheckDead
    Dim tmp, i
    tmp = 0
    For i = 1 to 4:tmp = tmp + Dead(CurrentPLayer, i):Next
    If tmp = 2 AND bInfoNeeded2(CurrentPLayer)AND bBattleready = False Then PlaySound "vo_completedeadtargets":bInfoNeeded2(CurrentPLayer) = False
    If tmp = 4 then 'enable Battle at the scoop
        DMD "   BATTLE IS READY  ", "  AT THE LABYRINTH  ", "", eNone, eNone, eNone, 1000, True, "vo_battleislit"
        Flashforms flasher012, 800, 50, 1
        Addscore 250000
        li056.State = 2
        li070.BlinkInterval = 300:li070.State = 2
        bBattleready = True
        'reset targets
        For i = 1 to 4:Dead(CurrentPLayer, i) = 0:Next
        ' count how many times Pool targets has been all hit
        Pool(CurrentPLayer, 0) = Pool(CurrentPLayer, 0) + 1
        CheckDeadPool
    End If
End Sub

'**************
' POOL targets
'**************

Sub Target005_Hit 'l
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    Addscore 1000
    ' check for modes
    StopCombo
    Select Case Battle(CurrentPlayer, 0)
        Case 0 'no battle active
            Pool(CurrentPlayer, 1) = 1
            CheckPool
        Case 1 'Juggernaut
            If LifeLeft(CurrentPlayer, 1) > 6 Then
                Addscore DominoValue
                PlaySound "sfx_hit1"
                CheckBattle
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target005"
End Sub

Sub Target006_Hit 'lower o
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    Addscore 1000
    ' check for modes
    StopCombo
    Select Case Battle(CurrentPlayer, 0)
        Case 0 'no battle active
            Pool(CurrentPlayer, 2) = 1
            CheckPool
        Case 1 'Juggernaut
            If LifeLeft(CurrentPlayer, 1) > 6 Then
                Addscore DominoValue
                PlaySound "sfx_hit2"
                CheckBattle
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target006"
End Sub

Sub Target007_Hit 'o
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    Addscore 1000
    ' check for modes
    StopCombo
    Select Case Battle(CurrentPlayer, 0)
        Case 0 'no battle active
            Pool(CurrentPlayer, 3) = 1
            CheckPool
        Case 1 'Juggernaut
            If LifeLeft(CurrentPlayer, 1) > 6 Then
                Addscore DominoValue
                PlaySound "sfx_hit3"
                CheckBattle
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target007"
End Sub

Sub Target008_Hit 'p
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    Addscore 1000
    ' check for modes
    StopCombo
    Select Case Battle(CurrentPlayer, 0)
        Case 0 'no battle active
            Pool(CurrentPlayer, 4) = 1
            CheckPool
        Case 1 'Juggernaut
            If LifeLeft(CurrentPlayer, 1) > 6 Then
                Addscore DominoValue
                PlaySound "sfx_hit4"
                CheckBattle
            End If
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target008"
End Sub

Sub CheckPool
    Dim tmp, i
    tmp = 0
    For i = 1 to 4:tmp = tmp + Pool(CurrentPLayer, i):Next
    If tmp = 2 AND bInfoNeeded3(CurrentPLayer)AND bLockEnabled = False Then PlaySound "vo_completepool-lock":bInfoNeeded3(CurrentPLayer) = False
    If tmp = 4 then 'enable Lock again
        Addscore 250000
        If bLockEnabled = False Then
            DMD "", "", "d_lockislit", eNone, eNone, eBlink, 1500, True, "vo_lockislit"
            bLockEnabled = True
            SwordEffect 1
        End If
        'reset targets
        For i = 1 to 4:Pool(CurrentPLayer, i) = 0:Next
        ' count how many times Pool targets has been all hit
        Pool(CurrentPLayer, 0) = Pool(CurrentPLayer, 0) + 1
        CheckDeadPool
    End if
End Sub

Sub CheckDeadPool 'all targets has been hit
    IF Dead(CurrentPlayer, 0) = 1 OR Pool(CurrentPLayer, 0) = 1 AND bInfoNeeded4(CurrentPLayer)Then bInfoNeeded4(CurrentPLayer) = False:vpmtimer.addtimer 3000, "PlaySound ""vo_completedptargets-mystery"" '"
    IF Dead(CurrentPlayer, 0) = 2 OR Pool(CurrentPLayer, 0) = 2 AND bInfoNeeded5(CurrentPLayer)Then bInfoNeeded5(CurrentPLayer) = False:vpmtimer.addtimer 3000, "PlaySound ""vo_completedptargets-regenerate"" '"
    IF Dead(CurrentPlayer, 0) > 2 AND Pool(CurrentPLayer, 0) > 2 Then
        'regeneration light
        DMD "_", "  REGENERATE IS LIT", "", eNone, eNone, eNone, 1500, True, "vo_regenerateislit"
        li075.State = 1
        ' and reset the counter
        Dead(CurrentPlayer, 0) = 0
        Pool(CurrentPlayer, 0) = 0
    ElseIf Dead(CurrentPlayer, 0) > 1 AND Pool(CurrentPLayer, 0) > 1 Then
        'enable Mystery light
        DMD "_", "   MYSTERY IS LIT", "", eNone, eNone, eNone, 1500, True, "vo_mysteryislit"
        li037.State = 1
    End If
End Sub

'*********************
' Lil Deadpool targets
'*********************

Sub Target009_Dropped 'lil drop 1
    PlaySoundAt "fx_droptarget", Target009
    If Tilted Then Exit Sub
    Addscore 1000
    'drop down also the right droptarget to avoid ball getting stuck
    Target011.IsDropped = 1
    li036.State = 0
    li034.State = 0
    ' check for modes
    StopCombo
    setlightcolor li044, blue, 2
    setlightcolor li044a, blue, 2
    If bLilDPMB then
        DropDownTargets
        setlightcolor li044, red, 2
        setlightcolor li044a, red, 2
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target009"
End Sub

Sub Target010_Dropped 'lil drop 2
    PlaySoundAt "fx_droptarget", Target010
    If Tilted Then Exit Sub
    Addscore 1000
    'drop down also the right droptarget to avoid ball getting stuck
    Target011.IsDropped = 1
    li035.State = 0
    li034.State = 0
    ' check for modes
    StopCombo
    setlightcolor li044, blue, 2
    setlightcolor li044a, blue, 2
    If bLilDPMB then
        DropDownTargets
        setlightcolor li044, red, 2
        setlightcolor li044a, red, 2
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target010"
End Sub

Sub Target011_Dropped 'lil drop 3
    PlaySoundAt "fx_droptarget", Target011
    If Tilted Then Exit Sub
    Addscore 1000
    li034.State = 0
    ' check for modes
    StopCombo
    setlightcolor li044, blue, 2
    setlightcolor li044a, blue, 2
    If bLilDPMB then
        DropDownTargets
        setlightcolor li044, red, 2
        setlightcolor li044a, red, 2
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target011"
End Sub

Sub ResetDropTargets
    ' PlaySoundAt "fx_resetdrop", Target010
    If Target009.IsDropped OR Target010.IsDropped OR Target011.IsDropped Then
        PlaySoundAt SoundFXDOF("fx_resetdrop", 119, DOFPulse, DOFcontactors), Target010
        Target009.IsDropped = 0
        Target010.IsDropped = 0
        Target011.IsDropped = 0
        li034.State = 2:li035.State = 2:li036.State = 2 'lilDP
        li044a.State = 0:li044.State = 0
    End If
End Sub

Sub DropDownTargets 'after the hurry up timer or after the first hit to release the trapped ball
    Target009.IsDropped = 1
    Target010.IsDropped = 1
    Target011.IsDropped = 1
    li034.State = 2:li035.State = 2:li036.State = 2 'lilDP
    LilDPTimer.Enabled = 0
End Sub

Sub Target012_Hit 'lil DP Stand up target
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    If Spots.Enabled = 0 Then FlashForms spot1, 1000, 50, 0:FlashForms spot2, 1000, 50, 0
    Addscore 10000
    ' check for modes
    StopCombo
    If LilDPHits < 10 Then LilDPCheckHits
    lilshake activeball.velY
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target012"
End Sub


'**********************************
'Derelict Frame Animation
'**********************************
dim frameDir : frameDir = 0
Sub MoveFrame
	frametime.enabled = 1
End Sub

Sub frametime_timer
	frame.RotX = frame.RotX + frameDir
	If frame.RotX < 90 Then
		frametime.enabled = 0
		frame.RotX = 90
	End If
	If frame.RotX > 270 Then
		frametime.enabled = 0
		frame.RotX = 270
	End If
End Sub

'***********************
'Lil Deadpool Multiball
'***********************
' variabes used bLilDPMB, LilDPHits and LilDPHitsNeeded(4), LilDPJackpot(4)

Sub LilDPTimer_Timer 'hurry up timer
    LilDPJackpot(CurrentPlayer) = LilDPJackpot(CurrentPlayer)- 5000 * LilDPHitsNeeded(CurrentPlayer)
    If LilDPJackpot(CurrentPlayer) = 250000 Then PlaySound "vo_lildp_multibal"
    If LilDPJackpot(CurrentPlayer) <= 100000 OR bLilDPMB = False Then 'release the locked ball
        PlaySound "vo_lildp_timeup"
        LilDPJackpot(CurrentPlayer) = 100000
        Me.Enabled = 0
        DropdownTargets
        If bMultiBallMode = False Then 'not in a multibal anymore then reset the droptargets after a short pause to let the ball out
            vpmtimer.addtimer 1500, "ResetDropTargets '"
        End If
    End If
End Sub

Sub PlaylilDPHitSound
    Dim tmp
    tmp = RndNbr(44)
    PlaySound "vo_lildp" &tmp
End Sub

Sub LilDPCheckHits
    LilDPHits = LilDPHits + 1
    'debug.print "lilDPHits " & LilDPHits
    If bLilDPMB = False AND LilDPHits > 4 Then StopLilDPMB:Exit Sub 'this should never happen, but just in case :)
    If LilDPHits < LilDPHitsNeeded(CurrentPlayer)Then
        PlaylilDPHitSound
        lighteffect 4
        setlightcolor li044, blue, 2
        setlightcolor li044a, blue, 2
        Exit Sub
    ElseIf LilDPHits = LilDPHitsNeeded(CurrentPlayer)Then
        lighteffect 4
        setlightcolor li044, green, 2
        setlightcolor li044a, green, 2
        LilDPHits = 4 'to compensate for required hits
    End If

    Select Case LilDPHits
        Case 0'do nothing
        Case 4          'start multiball
            DMD "       DERELICT ", "         MULTIBALL  ", "d_lildpmb", eNone, eNone, eNone, 1500, True, "vo_lildp_multiball2"
            ResetDroptargets
            AddMultiball 1
            bLilDPMB = True
            LilDPJackpot(CurrentPlayer) = 500000 * LilDPHitsNeeded(CurrentPlayer)
            LilDPHitsNeeded(CurrentPlayer) = LilDPHitsNeeded(CurrentPlayer) + 1
            If LilDPHitsNeeded(CurrentPlayer) > 4 Then LilDPHitsNeeded(CurrentPlayer) = 4
            LilDPTimer.Interval = 350 + DazzlerValue * 10
            LilDPTimer.Enabled = 1
            setlightcolor li044, red, 2
            setlightcolor li044a, red, 2
        Case 5, 6, 7, 8 '5 jackpots at the LilDP target
            setlightcolor li044, red, 2
            setlightcolor li044a, red, 2
            lighteffect 4:PlaySfx
            Jackpot(CurrentPLayer) = LilDPJackpot(CurrentPlayer)
            AwardJackpot
        Case 9
            lighteffect 4:PlaySfx
            Jackpot(CurrentPLayer) = LilDPJackpot(CurrentPlayer)
            AwardJackpot
            'stop the lights at the lilDP target and start the arrow light jackpots
            li034.State = 0:li035.State = 0:li036.State = 0 'lilDP
            li044a.State = 0:li044.State = 0
            'arrow lights
            For x = 1 to 6:LilDPLights(x) = 2:Next
        Case 10, 11, 12, 13 '5 jackpots
            Jackpot(CurrentPLayer) = LilDPJackpot(CurrentPlayer)
            AwardJackpot
        Case 14 'last jackpot and enabled hurry up on the spinners
            Jackpot(CurrentPLayer) = LilDPJackpot(CurrentPlayer)
            AwardJackpot
            For x = 1 to 6:LilDPLights(x) = 0:Next
            li054.State = 0
            li057.State = 0
            li055.State = 0
            li071.State = 0
            li058.State = 0
            li059.State = 0
            LilDPLights(1) = 2
            LilDPLights(4) = 2
            LilDPSpinner.Interval = 20000 + dazzlerValue * 1000
            LilDPSpinner.Enabled = 1
        Case 15 'called after the hurry up spinner timer is out
            setlightcolor li044, red, 2
            setlightcolor li044a, red, 2
            LilDPHits = 4
            AddPlayFieldMultiplier 1
        Case Else 'should never happen
            StopLilDPMB
    End Select
End Sub

Sub StopLilDPMB 'end of LilDP multiball
    LilDPHits = 0
    bLilDPMB = False
	demonActive = 0
	frameDir = -1
	MoveFrame
    LilDPTimer.Enabled = 0
    LilDPSpinner.Enabled = 0
    For x = 1 to 6:LilDPLights(x) = 0:Next
    li054.State = 0
    li057.State = 0
    li055.State = 0
    li071.State = 0
    li058.State = 0
    li059.State = 0
End Sub

Sub LilDPSpinner_Timer
    Me.Enabled = 0
    For x = 1 to 6:LilDPLights(x) = 0:Next
    li054.State = 0
    li057.State = 0
    li055.State = 0
    li071.State = 0
    li058.State = 0
    li059.State = 0
    LilDPHits = 14 'ensure to go to the right step
    LilDPCheckHits 'go to the next step in LilDP multiball
End Sub

'***********************
'Lil Deadpool animation
'   (simple jiggle)
'***********************

Sub lilshake(strength)
    lilPos = strength 
    lilshakeTimer.Enabled = True
End Sub

Sub lilshakeTimer_Timer()
	If demonActive = 0 then Exit Sub
    lildp.Transy = lilPos + 175
    If lilPos = 0 Then Me.Enabled = False:Exit Sub
    If lilPos < 0 Then
        lilPos = ABS(lilPos)- 0.1
    Else
        lilPos = - lilPos + 0.1
    End If
End Sub

'****************************************
'guardian follows ball
'ObjRotY. 185r 165strt 130 lt  150lb
'****************************************
Dim endpt, rotdir, DProtypos
	rotdir = 0
Sub lookR_Hit
	If demonActive = 0 then Exit Sub
	endpt = 185
	DProtypos=lildp.objRoty
	If DProtypos < endpt Then
		lildpFBtimer.enabled =1: rotdir = 1
	end if
End Sub

Sub lookLBot_Hit
	If demonActive = 0 then Exit Sub
	endpt = 185
	DProtypos=lildp.objRoty
	If DProtypos > endpt Then
		lildpFBtimer.enabled =1:
		rotdir = -1
	end if	
End Sub

Sub lookLtop_Hit
	If demonActive = 0 then Exit Sub
	endpt = 130
	DProtypos=lildp.objRoty
	If DProtypos > endpt Then
		lildpFBtimer.enabled =1
		rotdir = -1
	end if	
End Sub

Sub lildpFBtimer_Timer()
	if DProtypos = endpt Then
		lildpFBtimer.enabled =0
	Else
		DProtypos = DProtypos + rotdir
		lildp.objRoty = DProtypos		
	end if
	if DProtypos > 185 or DProtypos < 130 Then
		rotdir = -rotdir
	end if
End Sub
'***************
' other targets
'***************

Sub Target013_Hit 'colossal jackpot
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    ' check for modes
    StopCombo
    If li069.State = 2 Then
        li069.State = 0
        SuperJackpot(CurrentPLayer) = Score(CurrentPlayer) * 0.5 ' 20% of the score
        AwardSuperJackpot
    Else
        Addscore 50000
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target013"
End Sub

Sub Target014_Hit 'snitch
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    PlaySound "vo_snitchshot"
    Addscore 1000
    ' check for modes
    StopCombo
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target014"
End Sub

'**********
' spinners
'**********

Sub Spinner001_Spin 'dazzler
    PlaySoundAt "fx_spinner", Spinner001
    If Tilted Then Exit Sub
    If bBerserker Then
        Addscore BerserkerValue(CurrentPlayer)
        BerserkerValue(CurrentPlayer) = BerserkerValue(CurrentPlayer) + 10000
    Else
        Addscore 1000
        CheckBerserker                  'check for the number of hits to start Berserker rage
    End If
    If bLilDPMB AND LilDPHits > 14 Then 'second part of the LilDP multiball
        Addscore 500000
    End If
    If bDiscoEnabled = False Then 'count the spins when not in Disco Mode
        LeftSpinnerCount(CurrentPlayer) = LeftSpinnerCount(CurrentPlayer) + 1
        CheckDisco
    End If
    If bDiscoMBEnabled Then 'increase the Disco Super Jackpot
        DiscoSuperJackpot(CurrentPlayer) = DiscoSuperJackpot(CurrentPlayer) + 100000
        DMD " LAMENT SUPERJACKPOT", CL(1, "IS " &FormatScore(DiscoSuperJackpot(CurrentPlayer))), "", eNone, eNone, eNone, 200, True, ""
    End if
End Sub

Sub Spinner002_Spin 'wolverine
    PlaySoundAt "fx_spinner", Spinner002
    If Tilted Then Exit Sub
    If bBerserker Then
        Addscore BerserkerValue(CurrentPlayer)
        BerserkerValue(CurrentPlayer) = BerserkerValue(CurrentPlayer) + 10000
    Else
        Addscore 1000
        CheckBerserker                  'check for the number of hits to start Berserker rage
    End If
    If bLilDPMB AND LilDPHits > 14 Then 'second part of the LilDP multiball
        Addscore 500000
    End If
    If bDiscoMBEnabled Then 'increase the Disco Super Jackpot
        DiscoSuperJackpot(CurrentPlayer) = DiscoSuperJackpot(CurrentPlayer) + 100000
        DMD " LAMENT SUPERJACKPOT", CL(1, "IS " &FormatScore(DiscoSuperJackpot(CurrentPlayer))), "", eNone, eNone, eNone, 200, True, ""
    End if
    Select Case Battle(CurrentPlayer, 0)
        Case 2 'Mystique
            Addscore DominoValue
    End Select
End Sub

'****************
' Top lane Gates
'****************

Sub OpenGates
    PlaySoundAt "fx_SolenoidOn", gate3
    gate3.open = True
    gate4.open = True
End Sub

Sub CloseGates
    PlaySoundAt "fx_SolenoidOff", gate3
    gate3.open = False
    gate4.open = False
End Sub

'*****************
'    Hell House
'*****************
dim LspinActive
LspinActive = 0
sub spinleviathan
	If LspinActive = 0 Then
		LspinActive = 1
		lspinTimer.enabled = 1
		vpmtimer.addtimer 1500, "StopLeviathan '"
	end If
End Sub

Sub StopLeviathan
	LspinActive = 0
	leviathan002.visible = 0
	lspinTimer.enabled = 0
End Sub


Sub lspinTimer_timer
	LEVIATHAN.Roty = LEVIATHAN.Roty + 1
	leviathan002.visible = 1
	leviathan002.rotz= leviathan002.rotz + 1
End Sub

Sub HellHouse_Hit
    PlaySoundAt "fx_hole_enter", HellHouse
	spinleviathan
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    ' check for modes
    StopCombo
    Addscore 25000
    HellHouse.destroyball
    BallsinHole = BallsInHole + 1
    li056.State = 0
    Flashforms flasher012, 800, 50, 0
    ' check modes
    If bChooseBattle Then Exit Sub 'do nothing if you already are selecing a battle, due to multiball
    If bMechSuitMBSJackpot Then
        SuperJackpot(CurrentPLayer) = MechMBJackpot(CurrentPlayer) * 6
        AwardSuperJackpot
        MechMBJackpot(CurrentPlayer) = MechMBJackpot(CurrentPlayer) + 1000000 'increase by 1 million
        ContinueMechSuitMB
        Exit Sub
    End If
    If bMechSuitMBLight(CurrentPlayer)Then 'Start Mech Suit Multiball
        StartMechSuitMB
        Exit Sub
    End If
    If bBattleReady AND bMechSuitMBStarted = False then
        StartChooseBattle 'choose battle will kick out the ball after choosing a battle
        bBattleReady = False
        PlaySong "mu_shooterlane"
        Exit Sub
    End If
    If bEndBattleJackpot Then
        bEndBattleJackpot = False
        Jackpot(CurrentPLayer) = 3000000:AwardJackpot
    End If
    If li041.State Then 'weapon jackpot
        li041.State = 0
        DMD CL(0, "CONFIGURATION JACKPOT"), CL(1, "1 MILLION"), "", eNone, eBlink, eNone, 2000, True, "fanfare"
        Addscore 1000000
        RelitWeapons
    End If
    If li038.State Then 'extra ball
        li038.State = 0
        AwardExtraBall
    End If
    If li037.State Then 'mystery
        li037.State = 0
        StartMystery
        Exit Sub 'mystery will kick the ball out after it is finished
    End If
    ' Nothing left to do, so kick out the ball
    Flashforms li070, 1500, 50, 0
    vpmtimer.addtimer 1500, "kickBallOut '"
End Sub

Sub kickBallOut
    If BallsinHole > 0 Then
        BallsinHole = BallsInHole - 1
        '    PlaySoundAt "fx_popper", HellHouse
        PlaySoundAt SoundFXDOF("fx_popper", 111, DOFPulse, DOFcontactors), HellHouse
        HellHouse.CreateSizedBallWithMass BallSize / 2, BallMass
        HellHouse.kick 164, 30, 1
        Flashforms li070, 500, 50, 0
        Flashforms flasher012, 500, 50, 0
        vpmtimer.addtimer 350, "kickBallOut '" 'kick out the rest of the balls, if any
		discolight2.visible = 0
		lspinTimer.enabled = 0: LspinActive = 0
    End If
End Sub

'************************
'   DISCO lights & Modes
'************************
Sub StartDisco
    boxopen.Enabled = 1
    'bDiscoEnabled = True

End Sub

Sub StopDisco
	closed.enabled = 1
    bDiscoEnabled = False
End Sub

Sub DiscoTimer_Timer
    'discolight.rotz = (discolight.rotz + 1)mod 360
End Sub

Sub CheckDisco
    Select Case LeftSpinnerCount(CurrentPlayer)
        Case 50 'Disco Loops
            DMD "_", CL(1, "LAMENT LOOPS"), "", eNone, eNone, eNone, 1500, True, "vo_discoloops"
            StartDisco
            StartSpots
            OpenGates
            DiscoLoopsTimer.Interval = 20000 + DazzlerValue * 1000
            DiscoLoopsTimer.Enabled = 1
            bDiscoLoopsEnabled = True
            ChangeSong
            li053.State = 2
        Case 100                                'Disco MB
            DMD "_", CL(1, "LAMENT MULTIBALL"), "", eNone, eNone, eNone, 1500, True, "vo_discoloops"
            LeftSpinnerCount(CurrentPlayer) = 0 'and start again counting towards Disco Loops
            StartDisco
            StartSpots
            bDiscoMBEnabled = True
            ChangeSong
            li053.State = 2
            SetlightColor li057, red, 2
            SetlightColor li058, red, 0
            AddMultiball 1
    End Select
End Sub

Sub DiscoLoopsTimer_Timer 'the discolopp is over
    DiscoLoopsTimer.Enabled = 0
    StopDisco
    StopSpots
    bDiscoLoopsEnabled = False
    ChangeSong
    If Battle(CurrentPlayer, 0) = 3 OR Battle(CurrentPlayer, 0) = 0 Then CloseGates 'Sabretooth because of the bumpers
    li053.State = 0
End Sub

Sub StopDiscoMB 'stop the Disco Multiball
    StopDisco
    StopSpots
    bDiscoMBEnabled = False
    ChangeSong
    li053.State = 0
    li057.State = 0
    li058.State = 0
End Sub

'******************************************************************
'Puzzlebox Disco Ball Mod
'******************************************************************
Dim boxstp, boxdir, boxst
boxst = 0  'colsed  openr=1  closr=2  
boxdir = 1
Sub BoxOpen_Timer()
	if Closed.enabled = 1 Then
		exit sub 
	end If
	discolight2.visible = 1
	Select Case BoxSt
		Case 0
			Box2.TransY= box2.transY + boxdir
			if box2.TransY = 110 Then
				boxst=1
			end If
		Case 1
			Box2.RotY= box2.RotY - boxdir
			discolight2.rotz = discolight2.rotz - 1
			if box2.RotY = -45 Then
				boxst= 2
			end If
		Case 2
			Box2.TransY= box2.transY - boxdir
			if box2.TransY = 0 Then
				BoxOpen.enabled = 0
				boxst = 0
				discolight2.visible = 0
			end If
	End Select
End sub

Sub Closed_Timer()
	if boxopen.enabled = 1 Then
		exit sub 
	end If
	discolight2.visible = 1		 
	Select Case BoxSt
		Case 0
			Box2.TransY= box2.transY + boxdir
			if box2.TransY = 110 Then
				boxst=1
			end If
		Case 1
			Box2.RotY= box2.RotY + boxdir
			discolight2.rotz = discolight2.rotz + 1
			if box2.RotY = 0 Then
				boxst= 2
			end If
		Case 2
			Box2.TransY= box2.transY - boxdir
			if box2.TransY = 0 Then
				Closed.enabled = 0
				boxst = 0
				discolight2.visible = 0
			end If
	End Select
End sub
	
'**************
'   SWORD
'***************

Sub SwordEffect(n)
    Select Case n
        Case 0 ' all off
            For each x in aSwordLights:x.State = 0:Next
        Case 1 'updown ball lock activated
            For each x in aSwordLights:x.BlinkInterval = 250:x.BlinkPattern = 10:x.State = 2:Next
        Case 2 'all blink when a ball is locked
            LightSeqSword.UpdateInterval = 20
            LightSeqSword.Play SeqRandom, 7, , 1000
        Case 3 'top down eject balls
            LightSeqSword.UpdateInterval = 8
            LightSeqSword.Play SeqDownOn, 7, 1
        Case 4
            li062.BlinkPattern = "000000100000"
            li063.BlinkPattern = "000001010000"
            li064.BlinkPattern = "000010001000"
            li065.BlinkPattern = "000100000100"
            li066.BlinkPattern = "001000000010"
            li067.BlinkPattern = "010000000001"
            li068.BlinkPattern = "100000000000"
            For each x in aSwordLights:x.BlinkInterval = 125:x.State = 2:Next
    End Select
End Sub

'*******************
' Rotate Lane Lights
'*******************
' BAM! lights and BOOM lights

Sub RotateLaneLights(n) 'n is the direction, 1 or 0, left or right
    Dim tmp
    If bRotateLights Then
        If n = 1 Then
            tmp = li049.State
            li049.State = li050.State
            li050.State = li051.State
            li051.State = li052.State
            li052.State = tmp
            tmp = li006.State
            li006.State = li014.State
            li014.State = li015.State
            li015.State = li016.State
            li016.State = tmp
            'rotate the Boom array too
            tmp = Boom(CurrentPlayer, 1)
            Boom(CurrentPlayer, 1) = Boom(CurrentPlayer, 2)
            Boom(CurrentPlayer, 2) = Boom(CurrentPlayer, 3)
            Boom(CurrentPlayer, 3) = Boom(CurrentPlayer, 4)
            Boom(CurrentPlayer, 4) = tmp
        Else
            tmp = li052.State
            li052.State = li051.State
            li051.State = li050.State
            li050.State = li049.State
            li049.State = tmp
            tmp = li016.State
            li016.State = li015.State
            li015.State = li014.State
            li014.State = li006.State
            li006.State = tmp
            'rotate the Boom array too
            tmp = Boom(CurrentPlayer, 4)
            Boom(CurrentPlayer, 4) = Boom(CurrentPlayer, 3)
            Boom(CurrentPlayer, 3) = Boom(CurrentPlayer, 2)
            Boom(CurrentPlayer, 2) = Boom(CurrentPlayer, 1)
            Boom(CurrentPlayer, 1) = tmp
        End If
    End If
End Sub

Sub UpdateGates(n) '1 is open 0 is closed
    If bskillshotready then
        Gate3.Open = n
        Gate4.Open = n
    End If
End Sub

'*********************
'      BATTLES
'*********************
' Battles((CurrentPlayer), x) = value
' x being from 1 to 7, this is 7 battles
' x = 0 current running battle number
' values 0: not started, 1 finished, 2 ready to start
' 7 Battles
' 5 Battles to choose
' complete 3 to battle Sauron
' complete 5 to battle Mr.Sinister

Sub StartChooseBattle
    If NOT bChooseBattle Then
        DMD " CHOOSE YOUR BATTLE", "", "", eNone, eNone, eNone, 2000, True, "vo_chooseyourbattle"
        bChooseBattle = True
        If BattlesWon(CurrentPlayer) > 2 Then BattlesToChoose = 6
        If BattlesWon(CurrentPlayer) > 5 Then BattlesToChoose = 7
        vpmtimer.addtimer 2000, "UpdateDMDBattle '"
    End If
End Sub

Sub ChooseBattle(keycode) '5 first battles
    If(keycode = PlungerKey OR keycode = StartGameKey OR keycode = Lockbarkey)AND LifeLeft(CurrentPlayer, Battlenr) > 0 Then
        bChooseBattle = False
        StartBattle Battlenr
    End If
    If keycode = LeftFlipperKey Then
        Battlenr = (Battlenr - 1)
        If Battlenr < 1 Then Battlenr = BattlesToChoose
        UpdateDMDBattle
    End If
    If keycode = RightFlipperKey Then
        Battlenr = (Battlenr + 1)
        If Battlenr > BattlesToChoose Then Battlenr = 1
        UpdateDMDBattle
    End If
End Sub

Sub UpdateDMDBattle
    Dim tmp
    tmp = ShowLife(LifeLeft(CurrentPlayer, Battlenr))
    Select Case Battlenr
        Case 1:DMDFlush:DMD "           GASH", tmp, "d_juggernaut", eNone, eNone, eNone, 10000, True, "vo_juggernaut"
        case 2:DMDFlush:DMD "     BUTTERBALL", tmp, "d_mystique", eNone, eNone, eNone, 10000, True, "vo_mystique"
        Case 3:DMDFlush:DMD "      CHATTERER", tmp, "d_sabretooth", eNone, eNone, eNone, 10000, True, "vo_sabretooth"
        Case 4:DMDFlush:DMD "        TORSO", tmp, "d_trex", eNone, eNone, eNone, 10000, True, "vo_trex"
        Case 5:DMDFlush:DMD "     DR CHANNARD", tmp, "d_megalodon", eNone, eNone, eNone, 10000, True, "vo_megalodon"
        Case 6:DMDFlush:DMD "      ANGELIQUE", tmp, "d_sauron", eNone, eNone, eNone, 10000, True, "vo_sauron"
        Case 7:DMDFlush:DMD "        PINHEAD", tmp, "d_mrsinister", eNone, eNone, eNone, 10000, True, "vo_mrsinister"
    End Select
End Sub

Sub StartBattle(n)
    DMDFlush
    TurnOffTeamUpLights
    Battle(CurrentPlayer, 0) = n
    Battle(CurrentPlayer, n) = 2 '0 battle not started, 1 battle finished, 2 battle started
    If bFirstBattle(CurrentPlayer)Then
        AttackPower = 1
    Else
        AttackPower = 0.5 'the battles are now double as hard to complete
    End If
    TurnOffArrows
    ChangeSong
    Select Case Battle(CurrentPlayer, 0)
        Case 1 'Juggernaut
            DMD "  SHOOT DP TARGETS", "TO ATTACK GASH", "d_juggernaut", eNone, eNone, eNone, 3000, True, "vo_shootflashingshots"
            If LifeLeft(CurrentPlayer, 1) > 6 Then
                LightSeqDPtargets.Play SeqRandom, 40, , 4000
            Else
                BattleLights(2) = 2:BattleLights(5) = 2
            End If
            Flasher003.visible = 1
            OpenGates
            ChangeGi Red
            ChangeGiIntensity 1.3
        Case 2 'Mystique
            DMD " SHOOT WHITE SHOTS", "  TO ATTACK BUTTERBALL", "d_mystique", eNone, eNone, eNone, 3000, True, "vo_shoot-mystique"
            BattleLights(4) = 2
            Flasher004.visible = 1
            OpenGates
            ChangeGi Purple
            ChangeGiIntensity 1.5
        Case 3 'Sabretooth
            DMD " SHOOT POP BUMPERS", "TO ATTACK CHATTERER", "d_sabretooth", eNone, eNone, eNone, 3000, True, "vo_shoot-sabretooth"
            BattleLights(1) = 2:BattleLights(6) = 2
            li001.State = 2
            Flasher005.visible = 1
            CloseGates
            ChangeGi Amber
            ChangeGiIntensity 1.3
        Case 4 'T-Rex
            DMD " SHOOT GREEN SHOTS", " TO ATTACK TORSO", "d_trex", eNone, eNone, eNone, 3000, True, "vo_shootgreenshots-trex"
            Select case RndNbr(6)
                Case 1:BattleLights(1) = 2:BattleLights(2) = 2
                Case 2:BattleLights(2) = 2:BattleLights(4) = 2
                Case 3:BattleLights(4) = 2:BattleLights(5) = 2
                Case 4:BattleLights(5) = 2:BattleLights(6) = 2
                Case 5:BattleLights(6) = 2:BattleLights(3) = 2
                Case 6:BattleLights(3) = 2:BattleLights(1) = 2
            End Select
            Flasher002.visible = 1

            OpenGates
            ChangeGi Green
            ChangeGiIntensity 2
        Case 5 'Megalodon
            DMD "   SHOOT RED SHOTS", " TO ATTACK DR CHANNARD", "d_megalodon", eNone, eNone, eNone, 3000, True, "vo_shootflashingshots"
            For each x in aArrows
                SetLightColor x, blue, 1
            Next
            BattleLights(1) = 1
            BattleLights(2) = 1
            BattleLights(3) = 1
            BattleLights(4) = 1
            BattleLights(5) = 1
            BattleLights(6) = 1
            Select case RndNbr(6)
                Case 1:BattleLights(1) = 2
                Case 2:BattleLights(2) = 2
                Case 3:BattleLights(3) = 2
                Case 4:BattleLights(4) = 2
                Case 5:BattleLights(5) = 2
                Case 6:BattleLights(6) = 2
            End Select
            OpenGates
            ChangeGi Blue
            ChangeGiIntensity 2
        Case 6 'Sauron
            DMD " SHOOT GREEN SHOTS", " TO ATTACK ANGELIQUE", "d_sauron", eNone, eNone, eNone, 3000, True, "vo_shootgreenshots-sauron"
            AddMultiball 2
            BattleLights(2) = 2
            BattleLights(5) = 2
            OpenGates
            ChangeGi Teal
            ChangeGiIntensity 2
        Case 7 'MrSinister
            DMD "SHOOT FLASHING SHOTS", "TO ATTACK PINHEAD", "d_mrsinister", eNone, eNone, eNone, 3000, True, "vo_shootflashingshots-ifyoudare"
            Select case RndNbr(4)
                Case 1:BattleLights(1) = 2
                Case 2:BattleLights(4) = 2
                Case 3:BattleLights(6) = 2
                Case 4:BattleLights(3) = 2
            End Select
			FlashForms pinheadFlash, 1500, 500, 0
            AddMultiball 1
            OpenGates
            MrSinisterTimer.Interval = 120 + dazzlerValue
            MrSinisterTimer.Enabled = 1
            ChangeGi Blue
            ChangeGiIntensity 2
    End Select
    Flashforms li070, 3000, 50, 0
    vpmtimer.addtimer 3000, "KickBallOut '"
End Sub

Sub LightSeqDPtargets_PlayDone()
    LightSeqDPtargets.Play SeqRandom, 40, , 4000
End Sub

Sub CheckBattle 'called after each target or lane hit to change lights and check for the end of the battle
    DMD "", "", "d_bam", eNone, eNone, eBlink, 1000, True, "sfx19"
    Select Case Battle(CurrentPlayer, 0)
        Case 1                                      'Juggernaut
            LifeLeft(CurrentPlayer, 1) = LifeLeft(CurrentPlayer, 1)- AttackPower * WolverineValue
            If LifeLeft(CurrentPlayer, 1) <= 0 Then 'life is empty, then enabled the kicker to finish the battle
                TurnOffArrows
                SetLightColor li056, red, 2
                WinBattle
            ElseIF LifeLeft(CurrentPlayer, 1) < 7 Then 'end the target hits and start the lane hits
                LightSeqDPtargets.StopPlay
                If BattleLights(3) = 2 Then
                    TurnOffArrows
                    BattleLights(2) = 2:BattleLights(5) = 2
                Else
                    TurnOffArrows
                    BattleLights(1) = 2:BattleLights(4) = 2:BattleLights(6) = 2
                End If
            End If
        Case 2                                      'Mystique
            LifeLeft(CurrentPlayer, 2) = LifeLeft(CurrentPlayer, 2)- AttackPower * WolverineValue
            If LifeLeft(CurrentPlayer, 2) <= 0 Then 'life is empty, then enabled the kicker to finish the battle
                TurnOffArrows
                SetLightColor li056, red, 2
                WinBattle
            ElseIF LifeLeft(CurrentPlayer, 2) < 8 Then
                BattleLights(2) = 2:BattleLights(4) = 2:BattleLights(5) = 2
            ElseIF LifeLeft(CurrentPlayer, 2) < 10 Then
                BattleLights(2) = 2:li071.State = 0:BattleLights(5) = 2
            End If
        Case 3                                      'Sabretooth
            LifeLeft(CurrentPlayer, 3) = LifeLeft(CurrentPlayer, 3)-(AttackPower / 4) * WolverineValue
            If LifeLeft(CurrentPlayer, 3) <= 0 Then 'life is empty, then enabled the kicker to finish the battle
                TurnOffArrows
                SetLightColor li056, red, 2
                WinBattle
            End If
        Case 4                                      'T-Rex
            LifeLeft(CurrentPlayer, 4) = LifeLeft(CurrentPlayer, 4)- AttackPower * WolverineValue
            If LifeLeft(CurrentPlayer, 4) <= 0 Then 'life is empty, then enabled the kicker to finish the battle
                TurnOffArrows
                SetLightColor li056, red, 2
                WinBattle
            ElseIF LifeLeft(CurrentPlayer, 4) < 10 Then 'change the green arrow
                TurnOffArrows
                Select case RndNbr(6)
                    Case 1:BattleLights(1) = 2:BattleLights(2) = 2
                    Case 2:BattleLights(2) = 2:BattleLights(4) = 2
                    Case 3:BattleLights(4) = 2:BattleLights(5) = 2
                    Case 4:BattleLights(5) = 2:BattleLights(6) = 2
                    Case 5:BattleLights(6) = 2:BattleLights(3) = 2
                    Case 6:BattleLights(3) = 2:BattleLights(1) = 2
                End Select
            End If
        Case 5                                      'Megalodon
            LifeLeft(CurrentPlayer, 5) = LifeLeft(CurrentPlayer, 5)- AttackPower * WolverineValue
            If LifeLeft(CurrentPlayer, 5) <= 0 Then 'life is empty, then enabled the kicker to finish the battle
                turnOffarrows
                SetLightColor li056, red, 2
                WinBattle
            ElseIF LifeLeft(CurrentPlayer, 5) < 10 Then 'change the red blinking arrow
                For each x in aArrows
                    SetLightColor x, blue, 1
                Next
                BattleLights(1) = 1
                BattleLights(2) = 1
                BattleLights(3) = 1
                BattleLights(4) = 1
                BattleLights(5) = 1
                BattleLights(6) = 1
                Select case RndNbr(6)
                    Case 1:BattleLights(1) = 2
                    Case 2:BattleLights(2) = 2
                    Case 3:BattleLights(3) = 2
                    Case 4:BattleLights(4) = 2
                    Case 5:BattleLights(5) = 2
                    Case 6:BattleLights(6) = 2
                End Select
            End If
        Case 6                                      'Sauron
            LifeLeft(CurrentPlayer, 6) = LifeLeft(CurrentPlayer, 6)- AttackPower * WolverineValue
            If LifeLeft(CurrentPlayer, 6) <= 0 Then 'life is empty, then enabled the kicker to finish the battle
                turnOffarrows
                SetLightColor li056, red, 2
                WinBattle
            ElseIF LifeLeft(CurrentPlayer, 5) < 3 Then 'enable superjackpot at the colossus target
                li069.State = 2
            ElseIF LifeLeft(CurrentPlayer, 5) < 7 Then 'enable superjackpot at the colossus target
                li069.State = 2
            End If
        Case 7                                      'MrSinister
            LifeLeft(CurrentPlayer, 7) = LifeLeft(CurrentPlayer, 7)- AttackPower * WolverineValue
            If LifeLeft(CurrentPlayer, 7) <= 0 Then 'life is empty, then enabled the kicker to finish the battle
                turnOffarrows
                SetLightColor li056, red, 2
                WinBattle
            ElseIF LifeLeft(CurrentPlayer, 7) < 10 Then 'change the darkblue blinking arrow
                TurnOffArrows
                Select case RndNbr(4)
                    Case 1:BattleLights(1) = 2
                    Case 2:BattleLights(4) = 2
                    Case 3:BattleLights(6) = 2
                    Case 4:BattleLights(3) = 2
                End Select
            End If
    End Select
End Sub

Sub StopBattle 'stops the battle, mostly when you loose the ball, it can be continued
    bEndBattleJackpot = False
    TurnOffArrows
    li056.State = 0
    li070.State = 0
    DMDScoreNow
    Select Case Battle(CurrentPlayer, 0)
        Case 1 'Juggernaut
            LightSeqDPtargets.StopPlay
            Battle(CurrentPlayer, 1) = 0
            Flasher003.visible = 0
        Case 2 'Mystique
            Battle(CurrentPlayer, 2) = 0
            Flasher004.visible = 0
        Case 3 'Sabretooth
            Battle(CurrentPlayer, 3) = 0
            Flasher005.visible = 0
        Case 4 'T-Rex
            Battle(CurrentPlayer, 4) = 0
            Flasher002.visible = 0

        Case 5 'Megalodon
            Battle(CurrentPlayer, 5) = 0
            Flasher006.visible = 0
        Case 6 'Sauron
            Battle(CurrentPlayer, 6) = 0
        Case 7 'MrSinister
            Battle(CurrentPlayer, 7) = 0
            ResetForNewRound
            MrSinisterTimer.Enabled = 0
    End Select
    ResetTeamUps
    Battle(CurrentPlayer, 0) = 0
    CloseGates
    ChangeGi white
    ChangeGiIntensity 1
End Sub

Sub WinBattle
    BattlesWon(CurrentPlayer) = BattlesWon(CurrentPlayer) + 1
    Jackpot(CurrentPlayer) = 500000 * BattlesWon(CurrentPlayer)
    Select Case Battle(CurrentPlayer, 0)
        Case 1 'Juggernaut
            Battle(CurrentPlayer, 1) = 1
            Flasher003.visible = 0
            DMD "              GASH", "          DEFEATED", "d_juggernaut", eBlink, eNone, eNone, 2000, False, "vo_juggernautdefeated"
        Case 2 'Mystique
            Battle(CurrentPlayer, 2) = 1
            Flasher004.visible = 0
            DMD "        BUTTERBALL", "          DEFEATED", "d_mystique", eBlink, eNone, eNone, 2000, False, "vo_mystiquedefeated"
        Case 3 'Sabretooth
            Battle(CurrentPlayer, 3) = 1
            li001.State = 0
            Flasher005.visible = 0
            DMD "         CHATTERER", "          DEFEATED", "d_sabretooth", eBlink, eNone, eNone, 2000, False, "vo_sabretoothdefeated"
        Case 4 'T-Rex
            Battle(CurrentPlayer, 4) = 1
            DMD "           TORSO", "          DEFEATED", "d_trex", eBlink, eNone, eNone, 2000, False, "vo_trexdefeated"
            Flasher002.visible = 0

        Case 5 'Megalodon
            Battle(CurrentPlayer, 5) = 1
            Flasher006.visible = 0
            DMD "        DR CHANNARD", "          DEFEATED", "d_megalodon", eBlink, eNone, eNone, 2000, False, "vo_megalodondefeated"
        Case 6 'Sauron
            Battle(CurrentPlayer, 6) = 1
            DMD "          ANGELIQUE", "          DEFEATED", "d_sauron", eBlink, eNone, eNone, 2000, False, "vo_saurondefeated"
        Case 7 'MrSinister
            Jackpot(CurrentPlayer) = 1000000 * BattlesWon(CurrentPlayer)
            Battle(CurrentPlayer, 7) = 1
            DMD "           PINHEAD", "          DEFEATED", "d_mrsinister", eBlink, eNone, eNone, 2000, False, "vo_mrsinisterdefeated"
            MrSinisterTimer.Enabled = 0
			FlashForms pinheadFlash, 1500, 500, 0
            ResetForNewRound
            'turn on the Special light
            li039.State = 2
    End Select
    ResetTeamUps
    Battle(CurrentPlayer, 0) = 0
    bEndBattleJackpot = True
    DMD "  SHOOT THE SCOOP", " FOR EXTRA JACKPOT", "", eBlink, eNone, eNone, 2500, True, "vo_shootthescoop"
    li070.BlinkInterval = 300:li070.State = 2
    CloseGates
    ChangeGi white
    ChangeGiIntensity 1
    ChangeSong
End Sub

Sub ResetForNewRound 'reset battles after finishing them
    Dim j
    bFirstBattle(CurrentPlayer) = False
    For j = 0 to 7
        Battle(CurrentPlayer, j) = 0
        LifeLeft(CurrentPlayer, j) = 10
    Next
End Sub

Sub MrSinisterTimer_Timer 'timed battle
    StopBattle
    MrSinisterTimer.Enabled = 0
End Sub

'***********************
' spotflasher animation
'***********************

Dim MyPi, SpotStep, SpotDir
Dim sRGBStep, sRGBFactor, sRed, sGreen, sBlue

Sub StartSpots
    Spot1.visible = 1
    Spot2.visible = 1
    MyPi = Round(4 * Atn(1), 6) / 90
    SpotStep = 0
    sRGBStep = 0
    sRGBFactor = 5
    sRed = 255
    sGreen = 0
    sBlue = 0
    Spots.Enabled = 1
End Sub

Sub StopSpots
    Spot1.visible = 0
    Spot2.visible = 0
    Spots.Enabled = 0
    Spot1.RotZ = 210
    Camera1.RotZ = 210
    Spot2.RotZ = 170
    Camera2.RotZ = 170
    Spot1.color = RGB(255, 252, 224)
    Spot2.color = RGB(255, 252, 224)
End Sub

Sub Spots_Timer()
    Spot1.visible = 1
    Spot2.visible = 1
    'rotate spots
    SpotDir = SIN(SpotStep * MyPi) * 50
    SpotStep = (SpotStep + 1)MOD 360
    Spot1.RotZ = 210 - SpotDir
    Camera1.RotZ = 210 - SpotDir
    Spot2.RotZ = 170 + SpotDir
    Camera2.RotZ = 170 + SpotDir
    ' color the spotlights
    Select Case sRGBStep
        Case 0 'Green
            sGreen = sGreen + sRGBFactor
            If sGreen > 255 then
                sGreen = 255
                sRGBStep = 1
            End If
        Case 1 'Red
            sRed = sRed - sRGBFactor
            If sRed < 0 then
                sRed = 0
                sRGBStep = 2
            End If
        Case 2 'Blue
            sBlue = sBlue + sRGBFactor
            If sBlue > 255 then
                sBlue = 255
                sRGBStep = 3
            End If
        Case 3 'Green
            sGreen = sGreen - sRGBFactor
            If sGreen < 0 then
                sGreen = 0
                sRGBStep = 4
            End If
        Case 4 'Red
            sRed = sRed + sRGBFactor
            If sRed > 255 then
                sRed = 255
                sRGBStep = 5
            End If
        Case 5 'Blue
            sBlue = sBlue - sRGBFactor
            If sBlue < 0 then
                sBlue = 0
                sRGBStep = 0
            End If
    End Select
    Spot1.color = RGB(sRed, sGreen, sBlue)
    Spot2.color = RGB(sRed, sGreen, sBlue)
End Sub

'*******************
'      COMBOS
'*******************
' don't time out
' starts at 500K for a 2 way combo and it is doubled on each combo
' shots that count as combos:
'	Left Ramp	1
'	Wolverine loop	2
'	Right Ramp	3
'   5 combos, supercombo and superdupercombo

Sub CheckCombo(n)
    If Battle(CurrentPlayer, 0) <> 0 then Exit Sub             'no combos during battles
    If n = OldCombo Then StopCombo:Exit Sub                   'repeated shot so stop the combos

    If n = "Trigger013" AND LastSwitchHit = "Trigger010" Then 'NinjaApocalypse
        DMD CL(0, "PUZZLE BOX"), CL(1, "APOCAYPSE"), "d_ninjamb", eNone, eNone, eNone, 1500, True, "vo_supercombo"
        For x = 1 to 10
            CheckNinjaStars
        Next
        Addscore NinjaApocJackpot(CurrentPLayer)
        NinjaApocJackpot(CurrentPLayer) = NinjaApocJackpot(CurrentPLayer) + 500000
        StopCombo
    Else
        OldCombo = n
        ComboCount = ComboCount + 1
        Select Case ComboCount
            Case 1: 'just starting
            Case 2:DMD "", "", "d_combo", eNone, eNone, eBlink, 1500, True, "vo_combo"
            Case 3:DMD "", "", "d_combo2", eNone, eNone, eBlink, 1500, True, "vo_2xcombo"
            Case 4:DMD "", "", "d_combo3", eNone, eNone, eBlink, 1500, True, "vo_3xcombo"
            Case 5:DMD "", "", "d_combo4", eNone, eNone, eBlink, 1500, True, "vo_4xcombo"
            Case 6:DMD "", "", "d_combo5", eNone, eNone, eBlink, 1500, True, "vo_5xcombo"
            Case 7:DMD CL(0, "SUPER"), CL(1, "COMBO"), "", eNone, eBlink, eNone, 1500, True, "vo_supercombo"
            Case Else:DMD CL(0, "SUPERDUPER"), CL(1, "COMBO"), "", eNone, eBlink, eNone, 1500, True, "vo_superdupercombo"
        End Select
        AddScore ComboValue * ComboCount
    End If
End Sub

Sub StopCombo
    ComboCount = 0
    ComboValue = 500000
    OldCombo = ""
End Sub

'************
'  Team ups
'************

'reset team-ups
Sub ResetTeamUps
    If DazzlePower(CurrentPlayer) = 4 Then DazzlePower(CurrentPlayer) = 0:DazzlerValue = 0
    If ColossusPower(CurrentPlayer) = 4 Then ColossusPower(CurrentPlayer) = 0:ColossusValue = 1
    If WolverinePower(CurrentPlayer) = 4 Then WolverinePower(CurrentPlayer) = 0:WolverineValue = 1
    If DominoPower(CurrentPlayer) = 4 Then DominoPower(CurrentPlayer) = 0:DominoValue = 0
    Flasher013.Visible = 0
    Flasher014.Visible = 0
    Flasher016.Visible = 0
    Flasher017.Visible = 0
End Sub

'dazzler
Sub CheckDazzler
    If DazzlePower(CurrentPlayer) = 4 Then
        DMD "  TIFFANY", "  TEAM-UP", "d_dazzler", eNone, eNone, eNone, 1500, True, "vo_dazzlerteamup"
        DazzlerValue = 20 'add 20 extra seconds to any timer or hurry up
        FlashForms Flasher014, 1500, 50, 1
    End If
    If DazzlePower(CurrentPlayer) > 4 Then DazzlePower(CurrentPlayer) = 4
End Sub

'colossus
Sub CheckColossus
    If ColossusPower(CurrentPlayer) = 4 Then
        DMD " JOEY", "  TEAM-UP", "d_colossus", eNone, eNone, eNone, 1500, True, "vo_colossusteamup"
        ColossusValue = 2 'doubles points during battles
        FlashForms Flasher017, 1500, 50, 1
    End If
    If ColossusPower(CurrentPlayer) > 4 Then ColossusPower(CurrentPlayer) = 4
End Sub

'wolverine
Sub CheckWolverine
    If WolverinePower(CurrentPlayer) = 4 Then
        DMD " KIRSTY", "  TEAM-UP", "d_wolverine", eNone, eNone, eNone, 1500, True, "vo_wolverineteamup"
        WolverineValue = 2 'doubles damage during battles
        FlashForms Flasher013, 1500, 50, 1
    End If
    If WolverinePower(CurrentPlayer) > 4 Then WolverinePower(CurrentPlayer) = 4
End Sub

'domino
Sub CheckDomino
    If DominoPower(CurrentPlayer) = 4 Then
        DMD " ELIOT SPENCER", "  TEAM-UP", "d_domino", eNone, eNone, eNone, 1500, True, "vo_dominoteamup"
        DominoValue = 10000 '10000 extra points during some battles
        FlashForms Flasher016, 1500, 50, 1
    End If
    If DominoPower(CurrentPlayer) > 4 Then DominoPower(CurrentPlayer) = 4
End Sub

'*************
'  Weapons x
'*************
'30 weapons = light extra ball
'75 weapons = light MechSuit multiball
'50 weapons = playfield X + 1

Sub Checkweapons                                            'increase and check the number of weapons collected
    Weapons(CurrentPlayer) = Weapons(CurrentPlayer) + 1
    If Weapons(CurrentPlayer)MOD 6 = 0 Then li041.State = 2 'jackpot at the Hell house scoop
    If Weapons(CurrentPlayer)MOD 30 = 0 Then                'lit extra ball
        li038.State = 2
        DMD "_", CL(1, "EXTRA BALL IS LIT"), "", eNone, eNone, eNone, 2000, True, "vo_shootscoop-extraball"
    End If
    If Weapons(CurrentPlayer)MOD 75 = 0 Then bMechSuitMBLight(CurrentPlayer) = True 'lit Mech Suit MB, starts at the Hellhouse
    If Weapons(CurrentPlayer)MOD 50 = 0 Then AddPlayfieldMultiplier 1
    FlashForms Flasher007, 1500, 50, 0
End Sub

Sub RelitWeapons
    Dim i
    For i = 1 to 6:WeaponLights(CurrentPlayer, i) = 1:Next
End Sub
'********************
' Mech Suit multiball
'********************

Sub StartMechSuitMB
    Dim i
    DMD CL(0, "ELYSIUM"), CL(1, "MULTIBALL"), "", eNone, eNone, eNone, 1500, True, "vo_multiball"
    vpmtimer.addtimer 4000, "AddMultiball 3 '"
    bMechSuitMBLight(CurrentPlayer) = False:li047.State = 2
    bMechSuitMBStarted = true
    ChangeSong
    For i = 1 to 6:MechSuitLights(i) = 2:Next
    vpmtimer.addtimer 1500, "kickBallOut '"
End Sub

Sub ContinueMechSuitMB 'after super jackpot
    Dim i
    bMechSuitMBSJackpot = False
    bMechSuitMBLight(CurrentPlayer) = False
    li047.BlinkInterval = 250:li047.State = 0:li047.State = 2
    bMechSuitMBStarted = true
    For i = 1 to 6:MechSuitLights(i) = 2:Next
    vpmtimer.addtimer 1500, "kickBallOut '"
End Sub

Sub CheckMechSuitMBHits
    Dim i, tmp
    i = 0:tmp = 0
    Jackpot(Currentplayer) = MechMBJackpot(CurrentPlayer)
    AwardJackpot
    MechMBJackpot(CurrentPlayer) = MechMBJackpot(CurrentPlayer) + 100000 'increment the mech suit mb jackpot
    For i = 1 to 6:tmp = tmp + MechSuitLights(i):next
    If tmp = 0 Then                                                      'turn on the Super Jackpot at HellHouse scoop
        bMechSuitMBSJackpot = true
        li047.BlinkInterval = 125:li047.State = 0:li047.State = 2
    End If
End Sub

Sub StopMechSuitMB
    Dim i
    For i = 1 to 6:MechSuitLights(i) = 0:Next
    bMechSuitMBStarted = False
    ChangeSong
    li047.State = 0
    li054.State = 0
    li057.State = 0
    li055.State = 0
    li071.State = 0
    li058.State = 0
    li059.State = 0
End Sub

'************************
' Ninja Lock & Multiball
'************************'***************
'   Pillar of souls stuff
'*****************************************
Dim PilStop
Pilstop = 1

Sub pillarSpin_timer()
	pillar.RotY = pillar.RotY +1
	pillarbg.RotY = pillarbg.RotY + 1 
	pillarbg2.RotY = pillarbg2.RotY + 1 
	if pillar.RotY = 360 then pillar.RotY = 0
	if pillarbg.RotY = 360 then pillarbg.RotY = 0
	if pillarbg2.RotY = 360 then pillarbg2.RotY = 0
	Select Case PilStop
		Case 1: if pillar.RotY = 270 then pillarSpin.Enabled = 0 
		Case 2: if pillar.RotY = 0 then pillarSpin.Enabled = 0
		Case 3: if pillar.RotY = 90 then pillarSpin.Enabled = 0
		Case 4: if pillar.RotY = 180 then pillarSpin.Enabled = 0
	End Select
End Sub


' Uses a virtual lock for easier handling when several players are playing

Sub Lock_Hit
    Dim i, tmp               'time to kick the ball
    tmp = 1500
    IF bNinjaMBSJackpot Then 'Award Ninja MB Super Jackpot
        SuperJackpot(CurrentPlayer) = NinjaMBJackpot(CurrentPlayer) * 6
        NinjaMBJackpot(CurrentPlayer) = NinjaMBJackpot(CurrentPlayer) + 1000000
        AwardSuperJackpot
        'reset normal jackpots
        bNinjaMBSJackpot = False
        li058.State = 0
        SwordEffect 0
        For i = 1 to 6:NinjaMBLights(i) = 2:Next
    ElseIf bLockEnabled Then
        BallsInLock(CurrentPLayer) = BallsInLock(CurrentPLayer) + 1
        SwordEffect 2
        Select Case BallsInLock(CurrentPLayer)
            Case 1:DMD "", "", "d_lock1", eNone, eNone, eNone, 1500, True, "vo_ball1locked" : Pilstop = 2
            Case 2:DMD "", "", "d_lock2", eNone, eNone, eNone, 1500, True, "vo_ball2locked" : Pilstop = 3	
            Case 3:DMD "", "", "d_lock3", eNone, eNone, eNone, 1500, True, "vo_ball3locked" : Pilstop = 4
                'Start Ninja Multiball
                DMD CL(0, "PILLAR OF SOULS "), CL(1, "MULTIBALL"), "", eNone, eNone, eNone, 1500, True, "vo_multiball"
                vpmtimer.addtimer 4000, "AddMultiball 2 '"
                BallsInLock(CurrentPLayer) = 0
                bLockEnabled = False
                SwordEffect 0
                bNinjaMB = True
				
                ChangeSong
                NinjaMBJackpot(CurrentPlayer) = 500000 + 50000 * NinjaStars(CurrentPLayer)
                'Turn On the Ninja Jacpot Arrows in a teal color
                For i = 1 to 6:NinjaMBLights(i) = 2:Next
                tmp = 3000
        End Select
    End If
    vpmtimer.addtimer tmp, "ExitLock '"
End Sub

Sub NinjaMBCheckHits
    Dim i, tmp:i = 0:tmp = 0
    Jackpot(Currentplayer) = NinjaMBJackpot(CurrentPlayer)
    AwardJackpot
    For i = 1 to 6:tmp = tmp + NinjaMBLights(i):next
    If tmp = 0 Then 'turn on the Super Jackpot at the right ramp
        bNinjaMBSJackpot = true
        SwordEffect 4
    End If
End Sub

Sub StopNinjaMB 'when loose last multiball
    bNinjaMB = False
    bNinjaMBSJackpot = False
    ChangeSong
    For x = 1 to 6:NinjaMBLights(x) = 0:Next
    li054.State = 0
    li057.State = 0
    li055.State = 0
    li071.State = 0
    li058.State = 0
    li059.State = 0
    SwordEffect 0
	PilStop = 1
	pillarSpin.enabled = 1
End Sub

Sub ExitLock
    SwordEffect 3
    PLaySoundAt "fx_kicker", lock
    Lock.kick 180, 3
End Sub

'*****************
'  Chimichangas
'*****************
' chimichangas will also increase the playfield multiplier

Sub CheckChimichangaHits 'called from the right orbit
    Dim tmp
    ChimichangaHits(CurrentPlayer) = ChimiChangaHits(CurrentPlayer) + 1
    If ChimichangaHits(CurrentPlayer) = ChimichangaHitsNeeded(CurrentPlayer)Then 'spot a chimichanga as an  orange light
        Flasher011.Visible = 1
        tmp = RndNbr(6)
        ChimiChangaLights(tmp) = 2
        ChimichangaHits(CurrentPlayer) = 0                                              'reset count and
        ChimichangaHitsNeeded(CurrentPlayer) = ChimichangaHitsNeeded(CurrentPlayer) + 2 'increase the needed hits to spot a new chimichanga
    Else
        FlashForms flasher011, 500, 50, 0
    End If
End Sub

Sub CollectChimichanga
    DMD CL(0, "COLLECTED ONE"), CL(1, "CRICKET"), "d_chimi2", eNone, eNone, eNone, 1000, True, "sfx17"
    DMD "", CL(1, FormatScore(ChimichangaValue(CurrentPlayer))), "d_chimi2", eNone, eBlink, eNone, 1000, True, ""
    Chimichangas(CurrentPlayer) = Chimichangas(CurrentPlayer) + 1
    ChimichangaValue(CurrentPlayer) = ChimichangaValue(CurrentPlayer) + 100000
    FlashForms flasher011, 1500, 50, 0
    If Chimichangas(CurrentPlayer)MOD 10 = 0 Then AddPlayfieldMultiplier 1
End Sub

'*****************
'  Berserker Rage
'*****************
' spinners timed mode

Sub StartBerserker
    BerserkerTime.Interval = 20000 + dazzlerValue * 1000
    BerserkerTime.Enabled = 1
    bBerserker = True
    ChangeSong
End Sub

Sub StopBerserker
    BerserkerTime.Enabled = 0
    bBerserker = False
    ChangeSong
    BerserkerCount(CurrentPLayer) = 0
End Sub

Sub CheckBerserker
    BerserkerCount(CurrentPlayer) = BerserkerCount(CurrentPlayer) + 1
    If BerserkerCount(CurrentPLayer) = 25 Then 'Start Berserker Rage
        DMD CL(0, "STARTING"), CL(1, "CENOBITE RAGE"), "", eNone, eNone, eNone, 1500, True, "" 
        DMD CL(0, "CENOBITE RAGE"), CL(1, "SHOOT THE SPINNERS"), "", eNone, eNone, eNone, 1500, True, ""
        StartBerserker
    End If
End Sub

Sub BerserkerTime_Timer 'stop the Berserker Rage mode
    StopBerserker
End Sub

'*******************
'  Katanarama Time
'*******************
' orbits and ramps timed mode

Sub StartKatanarama
    KatanaramaTime.Interval = 20000 + dazzlerValue * 1000
    KatanaramaTime.Enabled = 1
    bKatanaramaTime = True
    DiamondLights(CurrentPlayer, 1) = 2
    DiamondLights(CurrentPlayer, 2) = 2
    DiamondLights(CurrentPlayer, 3) = 2
    DiamondLights(CurrentPlayer, 4) = 2
    DiamondLights(CurrentPlayer, 5) = 2
    DiamondLights(CurrentPlayer, 6) = 2
End Sub

Sub StopKatanarama
    KatanaramaTime.Enabled = 0
    bKatanaramaTime = False
    KatanaramaCount(CurrentPlayer) = 0
    'reset diamond lights
    KatanaramaCount(CurrentPlayer) = 0
    DiamondLights(CurrentPlayer, 1) = 2
    DiamondLights(CurrentPlayer, 2) = 0
    DiamondLights(CurrentPlayer, 3) = 0
    DiamondLights(CurrentPlayer, 4) = 0
    DiamondLights(CurrentPlayer, 5) = 0
    DiamondLights(CurrentPlayer, 6) = 0
End Sub

Sub CheckKatanarama 'increases the counter and check the lights to start KatanaramaTime
    KatanaramaCount(CurrentPlayer) = KatanaramaCount(CurrentPlayer) + 1
    Select Case KatanaramaCount(CurrentPlayer)
        Case 1
            DiamondLights(CurrentPlayer, 1) = 1
            DiamondLights(CurrentPlayer, 2) = 2
            DMD "1 CONFIGURATION COLLECTED", "", "d_diamond1", eNone, eNone, eNone, 1000, True, "fanfare"
        Case 2
            DiamondLights(CurrentPlayer, 2) = 1
            DiamondLights(CurrentPlayer, 3) = 2
            DMD "2 CONFIGURATION COLLECTED", "", "d_diamond2", eNone, eNone, eNone, 1000, True, "fanfare"
        Case 3
            DiamondLights(CurrentPlayer, 3) = 1
            DiamondLights(CurrentPlayer, 4) = 2
            DMD "3 CONFIGURATION COLLECTED", "", "d_diamond3", eNone, eNone, eNone, 1000, True, "fanfare"
        Case 4
            DiamondLights(CurrentPlayer, 4) = 1
            DiamondLights(CurrentPlayer, 5) = 2
            DMD "4 CONFIGURATION COLLECTED", "", "d_diamond4", eNone, eNone, eNone, 1000, True, "fanfare"
        Case 5
            DiamondLights(CurrentPlayer, 5) = 1
            DiamondLights(CurrentPlayer, 6) = 2
            DMD "5 CONFIGURATION COLLECTED", "", "d_diamond5", eNone, eNone, eNone, 1000, True, "fanfare"
        Case 6 'all diamonds are on, so start katanarama time
            DiamondLights(CurrentPlayer, 6) = 1
            DMD "6 CONFIGURATION COLLECTED", "", "d_diamond6", eNone, eNone, eNone, 1000, True, "vo_katanaramaisready"
            DMD CL(0, "STARTING"), CL(1, "REVERSE CONFIGURATION"), "", eNone, eNone, eNone, 1500, True, "" 
            DMD CL(0, "REVERSE CONFIGURE"), CL(1, "SHOOT ORBITS-RAMPS"), "", eNone, eNone, eNone, 1500, True, "" 
            StartKatanarama
    End Select
End Sub

Sub KatanaramaTime_Timer
    StopKatanarama
End Sub

'****************
' Mystery award
'****************

Sub StartMystery
    DMD "   MYSTERY AWARD    ", CL(1, "LIGHT LOCK"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "DEAMONS TO SOME"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "SMALL POINTS"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "INFINITE BALLS"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "COLLECT X BOXES"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "NOTHING"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "LIGHT EXTRA BALL"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "SUFFERING"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "COLLECT X CONFIGURATIONS"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "YOU OPENED THE BOX"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "BIG POINTS"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "5 POINTS"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "LIGHT ADD-A-BALL"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "50 MILLIONS"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "ADVANCE CRICKET"), "", eNone, eNone, eNone, 100, False, ""
    DMD "   MYSTERY AWARD    ", CL(1, "DEFEAT EVERYONE"), "", eNone, eNone, eNone, 100, False, ""
    vpmtimer.addtimer 3000, "ChooseMysteryAward '"
End Sub

Sub ChooseMysteryAward
    Dim tmp, tmp2
    tmp = RndNbr(20)
    DMDFlush
    Select case tmp
        Case 1
            DMD "   MYSTERY AWARD    ", CL(1, "LIGHT LOCK"), "", eNone, eBlink, eNone, 1000, True, "fanfare"
            DMD "", "", "d_lockislit", eNone, eNone, eBlink, 1500, True, "vo_lockislit"
            bLockEnabled = True
            SwordEffect 1
        Case 3
            DMD "   MYSTERY AWARD    ", CL(1, "COLLECT X BOXES"), "", eNone, eBlink, eNone, 1000, True, "fanfare"
            tmp2 = RndNbr(5)
            DMD "   MYSTERY AWARD    ", CL(1, "COLLECTED " &tmp2& " BOXES"), "", eNone, eBlink, eNone, 1500, True, ""
            For x = 1 to tmp2
                CheckNinjaStars
            Next
        Case 4
            DMD "   MYSTERY AWARD    ", CL(1, "LIGHT EXTRA BALL"), "", eNone, eBlink, eNone, 2500, True, "vo_extraballislit"
            li038.State = 1
        Case 5
            DMD "   MYSTERY AWARD    ", CL(1, "COLLECT X CONFIGURATIONS"), "", eNone, eBlink, eNone, 1000, True, "fanfare"
            tmp2 = RndNbr(10)
            DMD "   MYSTERY AWARD    ", CL(1, "COLLECTED " &tmp2& " CONFIGURATIONS"), "", eNone, eBlink, eNone, 1500, True, ""
            For x = 1 to tmp2
                CheckWeapons
            Next
        Case 6, 12
            DMD "   MYSTERY AWARD    ", CL(1, "BIG POINTS"), "", eNone, eBlink, eNone, 1000, True, "fanfare"
            tmp2 = 500000 * RndNbr(5)
            DMD "   MYSTERY AWARD    ", CL(1, FormatScore(tmp2)), "", eNone, eBlink, eNone, 1500, True, ""
            Addscore tmp2
        Case 8
            DMD "   MYSTERY AWARD    ", CL(1, "COLLECT 1 CRICKET"), "", eNone, eBlink, eNone, 1000, True, ""
            Collectchimichanga
        Case Else
            DMD "   MYSTERY AWARD    ", CL(1, "SMALL POINTS"), "", eNone, eBlink, eNone, 1000, True, "fanfare"
            tmp2 = 50000 * RndNbr(5)
            DMD "   MYSTERY AWARD    ", CL(1, FormatScore(tmp2)), "", eNone, eBlink, eNone, 1500, True, ""
            Addscore tmp2
    End Select
    'kickout the ball
    Flashforms li070, 2500, 50, 0
    vpmtimer.addtimer 2600, "kickBallOut '"
End Sub

' Update the embedded DMD inside VPX. This requires the table to have a timer object, named 'FlexDMDTimer', enabled, with a timer interval of -1 (VPX > 10.2)
Sub FlexDMDTimer_Timer
	Dim DMDp
	If UseDMD Then
		DMDp = FlexDMD.DmdPixels
		If Not IsEmpty(DMDp) Then
			DMDWidth = FlexDMD.Width
			DMDHeight = FlexDMD.Height
			DMDPixels = DMDp
		End If
	ElseIf UseColoredDMD Then
		DMDp = FlexDMD.DmdColoredPixels
		If Not IsEmpty(DMDp) Then
			DMDWidth = FlexDMD.Width
			DMDHeight = FlexDMD.Height
			DMDColoredPixels = DMDp
		End If
	End If
End Sub
