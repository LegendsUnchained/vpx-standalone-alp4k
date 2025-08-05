' ****************************************************************
'             NIGHTMARE BEFORE CHRISTMAS 2024
'                Visual Pinball 10.8
' ****************************************************************
'  Coding - Oqqsan
'  Art Design and 3D - Joe Picasso
'  Sound Design - iDigStuff
'  Physics nfozzy - Apophis
'  VR Implementation - DarDog
'  DOF by Darrin VPCLE
'  Backglass - Hauntfreaks
'  Based on Friday the 13 by JPSalas
'  Layout based on Stern Godzilla's layout Pro version.
' ****************************************************************
'
'  BackGlass + Full DMD : https://vpuniverse.com/files/file/22642-nightmare-before-christmas-original-2024-b2s-full-dmd/
'  Video Tutorial : https://vpuniverse.com/files/file/22629-nightmare-before-christmas-original-2024-vpx-video-instruction/

Option Explicit
Randomize

'♥♡♥♡♥♡♥♡♥♡♥♡♥♡♥ PLayer choices ♥♡♥♡♥♡♥♡♥♡♥♡♥♡♥

Const maxExtraballs = 3		'3=3 2=2 1=1 and 0 = no eb , scoring points if not avail
Const SongVolume = 1		' 1 is full volume, but I set it quite low to listen better the other sounds since I use headphones, adjust to your setup :)
Const BackboxVolume = 1	' 1 is full
Const FlexDMDHighQuality = True  'FlexDMD in high quality (True = LCD at 256x64) or normal quality (False = Real DMD at 128x32)
Const StagedFlippers = 0 '0 = Non-Staged Flippers; 1 = Staged Flippers

'----- Target Bouncer Levels -----
Const TargetBouncerEnabled = 1	  ' 0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.1	  ' Level of bounces. Recommmended value of 0.7

'----- General Sound Options -----
Const VolumeDial = 1			  ' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 1		  ' Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 1		  ' Level of ramp rolling volume. Value between 0 and 1

'****************************************************************
'	VR
'****************************************************************
'///////////////////////---- VR Room ----////////////////////////
Dim VRRoomChoice : VRRoomChoice = 0					'0 - VR Room Off, 1 - Minimal Room, 2 - MEGA
Dim VRTest : VRTest = False

'//////////////F12 Menu//////////////
Dim dspTriggered : dspTriggered = False
Sub Table1_OptionEvent(ByVal eventId)
	If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If

    ' VRRoom
	VRRoomChoice = Table1.Option("VR Room", 0, 2, 1, 2, 0, Array("Off", "Minimal Room", "MEGA"))
	LoadVRRoom

	If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub

'♥♡♥♡♥♡♥♡♥♡♥♡♥ End of pLayer choices ♥♡♥♡♥♡♥♡♥♡♥♡♥

Const	score_skillshot				=  300000
Const	score_addskillshot			=  200000
Const	score_SuperSS				= 2500000
Const	score_addSuperSS			= 1000000

Const	score_spinnerspin			=      60 ' not increasing
Const	score_multispin				=      20 ' reward for multispin upto 20*20 and bonus at 19 spins in a row?
Const	score_SpinnerJP				= 1000000 ' 50 spins during megaspinners, collect at scoop + megaspinlevel
Const	score_megaspinlevel			=  250000 ' 1 level can be earned for each megaspinnround  max level 6
Const	score_Gravestones			=  100000 ' all 6 targets to start megaspinner
Const	score_megaspinner_targets	=   15010 ' added to targethits during megaspinn

Const	score_scoophit				=    2000
Const	score_targethits			=     500
Const	score_megaspinns			=	25000
Const	score_slingshots			=     110
Const	score_triggerhits			=     150
Const	score_Gate1_hit				=     100
Const	score_Outlanes_hit			=  100000
Const	score_Inlanes_hit			=    1000
Const	score_ramphit				=   10000


Const	score_JasonSuperBonus		=  100000 ' values added to JP and SJP comes from this
Const	score_JasonJackpot	 		=  250000
Const	score_JasonSJP				= 1000000 ' adding value for each JP to JP and SJP 


Const	score_nightmare_triggers	=    5000
Const	score_nightmare_alllit		=  750000
Const	score_nightmare_supers		=  750000 ' base value jp on nightmare the score_nightmare_nrlitsupers = added for each lit JP 
Const	score_nightmare_nrlitsupers =  500000 ' adds to supers JP's -> 1 lit =750k+500k-> 1.250k   2 lit = 1.750k   3 løit = 2.250k etc upto 9 or 10
Const	NightmareLitJP				=      25  ' x + 5 for lit jp


Const	score_bumperSpecial			=   50000
Const	score_bumperhit				=    5000

Const	score_Combo_1				=   50000
Const	score_Combo_2				=   60000
Const	score_Combo_3				=   70000
Const	score_Combo_4				=   80000
Const	score_Combo_5				=  100000
Const	score_Combo_6				=  125000	' sound upgrade igor

Const	score_megaloop				=  200000	' 2 4 6 8 10 12 13 16 18 2mill for 10 in a row bonus scoring ( gotta be fast timer is on 2777 ms )=
Const	score_TopLoop				=   25000	' 4 times to start superloops with both right up down loop
Const	score_SuperLoopJP			=  750000	
Const	score_SuperLoopJPadd		=  250000						' so 500k for firstone  only reset at new ball and limit at 2mill  
Const	score_DogChase				= 1000000
Const	score_DogChaseAdd			=  250000
Const	score_XmasHurryup			=  650000
Const	score_XmasHurryupAdd		=  100000

' modes	
Const	score_DeliverPresent		= 1000000	' scoop = Delivered
Const	score_ModeCollected			=  250000	' all collected not delivered
Const	score_ModeProgress			=   75000	' progress

Const	score_Full_Revive			=  250000	'awarded if both revive is already on
Const	score_new_Revive			=   21000
Const	score_Jack_Secondpass		=  100000	' 50000 first then 100 150 200 250 250 250 250  colecting jack when blinking ( until jackMB is Reset)
Const	score_NO_EB					= 2000000	' if eb's turned off
Const	score_Extraball				=	50000

Const	score_EOB_Missions			=   50000
Const	score_EOB_Orbits			=   25000 	' ramps
Const	score_EOB_Combo				=   10000
Const	score_EOB_bumper			=    5000
Const	score_EOB_spins				=  	  100	' ATM NOT USED
Const	score_spinsneededforeob		=      25	' lites upperspinner bonusx light  25+5*bonuslevel = 30 for 2x  35 for 3x etc

Const	score_jackBonus1			=  500000
Const	score_jackBonus2			= 1000000

Const score_wizAllHits=	   3010
Const score_wiz1_hits =   33300 '100 ? 3330000
Const score_wiz2_hits =  250000 '17h   4250000
Const score_wiz3_hits =  500000 '10	   5000000
Const score_wiz4_hits = 3000000 '2     6000000
Const score_wiz5_hits =   44000 '150   6500000
Const score_wiz6_hits =  400000 '17    6800000
Const score_wiz7_hits = 5000000 '2	  10000000
Const score_wiz8_hits =25000000 '1	  25000000
Const score_wiz1_done = 2000000
Const score_wiz2_done = 4000000
Const score_wiz3_done = 5000000
Const score_wiz4_done = 6000000
Const score_wiz5_done = 7000000
Const score_wiz6_done = 8000000
Const score_wiz7_done = 9000000


' Load the core.vbs for supporting Subs and functions
Const BallSize = 50        ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1        ' standard ball mass

Const tnob = 10
Const lob = 0
Dim tablewidth:tablewidth = Table1.width
Dim tableheight:tableheight = Table1.height

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
Const cGameName = "nightmarebc"
Const myVersion = "4.00"
Const MaxPlayers = 4          ' from 1 to 4
Const BallSaverTime = 12     ' in seconds of the first ball
Const MaxMultiplier = 5      ' limit playfield multiplier
Const MaxBonusMultiplier = 31 'limit Bonus multiplier
Const BallsPerGame = 3       ' usually 3 or 5
Const MaxMultiballs = 13     ' max number of balls during multiballs

' Use FlexDMD if in FS mode
Dim UseFlexDMD


   UseFlexDMD = True


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

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
    Randomize

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 30 ' Plunger Power
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


    ' load saved values, highscore, names, jackpot
    Credits = 0
    Loadhs

    ' Initalise the DMD display
    DMDInit

    ' freeplay or coins
    bFreePlay = False 'we want coins

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
	setupXlights

    ' Load table color
    LoadLut
	PlaySong "mu_gameover" 'idig


	inserts_init
	ChangeGi white

	Frametimer.Enabled = True
	GameTimer.enabled = True
	DMD_Timer.enabled = True

End Sub

'******
' Keys
'******


dIM swapfeature : swapfeature = 0
Dim Rdown
Dim Ldown

Sub Table1_KeyDown(ByVal Keycode)
	If keycode = LeftFlipperKey Then
		PinCab_Left_Flipper_Button.X = PinCab_Left_Flipper_Button.X + 8
        DOF 101, DOFPulse
	End If

	If keycode = RightFlipperKey Then
		PinCab_Right_Flipper_Button.X = PinCab_Right_Flipper_Button.X - 8
        DOF 102, DOFPulse
	End If
    If keycode = LeftTiltKey Then Nudge 90, 1.8:SoundNudgeLeft()
    If keycode = RightTiltKey Then Nudge 270, 1.8:SoundNudgeRight()
    If keycode = CenterTiltKey Then Nudge 0, 2.4:soundNudgeCenter()

    If keycode = LeftMagnaSave Then bLutActive = True: Lutbox.text = "level of darkness " & LUTImage + 1
    If keycode = RightMagnaSave Then
		Select Case swapfeature
			case 0 : swapfeature = 1 : DMD_Jackpot = 1
			case 1 : swapfeature = 2 : DMD_DoggieLeft = 1
			case 2 : swapfeature = 0 : DMD_DoggieRight = 1
		End Select

        If bLutActive Then
            NextLUT
        End If
    End If
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,60 , "insertcoin" , 19 ,0,0
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,60 , "credits" , 19 ,0,0
    If Keycode = AddCreditKey Then
		If DMDmode = 2 And SplashEffect <> "skillshot" Then DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,50 , "credits" , 19 ,0,0
        Credits = Credits + 1
		Playsoundat "fx_Coin",Drain
		CoinInQuote
        if bFreePlay = False Then DOF 125, DOFOn
    End If
    If Keycode = AddCreditKey2 Then
		If DMDmode = 2 And SplashEffect <> "skillshot" Then DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,50 , "credits" , 19 ,0,0
        Credits = Credits + 1
		PlaySoundAt "fx_Coin",Drain
		CoinInQuote
        if bFreePlay = False Then DOF 125, DOFOn
    End If
    If keycode = PlungerKey Then
        Plunger.Pullback
        SoundPlungerPull()
		If VRRoom > 0 Then
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
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
		If keycode = 20 Then CheckTilt
        If keycode = LeftFlipperKey  Then 
			Ldown = 1 : FlipperActivate LeftFlipper,  LFPress:SolLFlipper 1:RotateLaneLights 1: IF Not bInstantInfo Then InstantInfoTimer.Enabled = False : InstantInfoTimer.interval = 3000 : InstantInfoTimer.Enabled = True
			If StagedFlippers = 0 Then FlipperActivate LeftFlipper001,  ULFPress:SolULFlipper 1
		End If
		If StagedFlippers = 1 Then
			If keycode = 30  Then FlipperActivate LeftFlipper001,  ULFPress:SolULFlipper 1
		End If

		If keycode = RightFlipperKey Then Rdown = 1 : FlipperActivate RightFlipper, RFPress:SolRFlipper 1:RotateLaneLights 0: IF Not bInstantInfo Then InstantInfoTimer.Enabled = False : InstantInfoTimer.interval = 3000 : InstantInfoTimer.Enabled = True
		If Ldown = 1 And Rdown = 1 And EOB_FastSkip Then EOB_Faster = True

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
					PlayVoice "vo_player" & PlayersPlayingGame
					DMD_newmode = 2
                    TotalGamesPlayed = TotalGamesPlayed + 1
					If DMDmode = 2 And SplashEffect <> "skillshot" Then DMD_StartSplash "WELCOME","PLAYER " & PlayersPlayingGame,FontHugeOrange, FontHugeOrange,88 , "oneimage" ,55,0,0
                Else
                    If(Credits > 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
						PlayVoice "vo_player" & PlayersPlayingGame
						DMD_newmode = 2
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
						If DMDmode = 2 And SplashEffect <> "skillshot" Then DMD_StartSplash "WELCOME","PLAYER " & PlayersPlayingGame,FontHugeOrange, FontHugeOrange,88 , "oneimage" ,55,0,0


                        If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
					Else
							If DMDmode = 2 And SplashEffect <> "skillshot" Then DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,60 , "insertcoin" , 19 ,0,0
							PlayVoice "vo_givemeyourmoney"
                    End If
                End If
            End If
        End If
    Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
							FlushSplashQ
							DMD_removeall
                            ResetForNewGame()
							DMD_newmode = 2
							blinklights 53,54,1,10,10,0

                    End If
                Else
                    If(Credits > 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
							FlushSplashQ
							DMD_removeall
                            ResetForNewGame()
							DMD_newmode = 2
							blinklights 53,54,1,10,10,0
                        End If
                    Else
						If DMDmode = 2 And SplashEffect <> "skillshot" Then DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,60 , "insertcoin" , 19 ,0,0
						StopSound "vo_givemeyourmoney"
						StopSound "vo_sorry"
						StopSound "_jinglebells"
						StopSound "mu_gameover"
						Select Case RndNbr(3)'idig
							Case 1:PlayVoice "vo_givemeyourmoney"
							Case 2:PlayVoice "vo_sorry"
							Case 3:PlaySound "_jinglebells",1,BackBoxVolume * 0.8
						End Select
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub



Sub Table1_KeyUp(ByVal keycode)

	If keycode = LeftFlipperKey Then
		PinCab_Left_Flipper_Button.X = PinCab_Left_Flipper_Button.X - 8
	End If

	If keycode = RightFlipperKey Then
		PinCab_Right_Flipper_Button.X = PinCab_Right_Flipper_Button.X + 8
	End If
    If keycode = LeftMagnaSave Then bLutActive = False: LutBox.text = ""

    If keycode = PlungerKey Then
        Plunger.Fire
		PlaySound"sfx_sleighbell1"
        SoundPlungerReleaseBall()
		If VRRoom > 0 Then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
			PinCab_Shooter.Y = 55
			
		End If
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
			Ldown = 0 
			FlipperDeActivate LeftFlipper, LFPress
            SolLFlipper 0
				If StagedFlippers = 0 Then
						SolULFlipper 0
				End If
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                bInstantInfo = False
				FlushSplashQ
				If bSkillshotReady Then DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,25522 , "skillshot" , 500,0,0
            End If
        End If

		

		If StagedFlippers = 1 Then
			If keycode = 30  Then
				FlipperDeActivate LeftFlipper001, ULFPress
				SolULFlipper 0
				End If
			End If

        If keycode = RightFlipperKey Then
			Rdown = 0
			FlipperDeActivate RightFlipper, RFPress
            SolRFlipper 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                bInstantInfo = False
				FlushSplashQ
				If bSkillshotReady Then DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,25522 , "skillshot" , 500,0,0
            End If
        End If
    End If
End Sub

Sub InstantInfoTimer_Timer
	InstantInfoTimer.interval = 3000
    InstantInfoTimer.Enabled = False
	If DMDmode <> 2 Then Exit Sub
    If NOT hsbModeActive Then
        bInstantInfo = True
        FlushSplashQ
		DMD_StartSplash "INFO","INSTANT",FontHugeOrange, FontHugeOrange,80 , "oneimage" , 67 ,0,0
		If Score(1) Then DMD_StartSplash "PLAYER 1",FormatScore(Score(1) ),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
		If Score(2) Then DMD_StartSplash "PLAYER 2",FormatScore(Score(2) ),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
		If Score(3) Then DMD_StartSplash "PLAYER 3",FormatScore(Score(3) ),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
		If Score(4) Then DMD_StartSplash "PLAYER 4",FormatScore(Score(4) ),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
		DMD_StartSplash "BALLS LEFT",	BallsRemaining(CurrentPlayer),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0	
		DMD_StartSplash "EXTRABALLS",	ExtraBallsAwards(CurrentPlayer),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
		DMD_StartSplash "PLAYFIELD",	"X " & PlayfieldMultiplier(CurrentPlayer),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
		DMD_StartSplash "BONUS",		"X " & BonusMultiplier(CurrentPlayer),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0 :' Bonuscalls
		DMD_StartSplash "COMBOS", ComboCount(CurrentPlayer) ,FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
		DMD_StartSplash "RAMP HITS", Loopcount(CurrentPlayer) ,FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
'		DMD_StartSplash "SPINNERSPINS", SpinnerSpins ,FontHugeOrange, FontHugeOrange,100 ,"oneimage" , 67 ,0,0
		DMD_StartSplash "BUMPERHITS", BumperHits(CurrentPlayer) ,FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
		DMD_StartSplash "SKILLSHOT",	FormatScore(SkillshotValue(CurrentPlayer) ),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0


		DMD_StartSplash "COMPLETE","MISSIONS",FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
		DMD_StartSplash "HIT","BLUE LIGHTS",FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
		DMD_StartSplash "TO ASSEMBLE","PRESENTS",FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
		DMD_StartSplash "DELIVER","AT SCOOP",FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
		DMD_StartSplash "WINNERS DON'T","DO DRUGS"	,FontHugeOrange, FontHugeOrange,100 , "oneimageINFO" , 67 ,0,0

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

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
    If Enabled AND bFlippersEnabled Then
		LF.Fire
			If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If  
    Else
		LeftFlipper.RotateToStart
		LeftFlipper001.EOSTorque = 0.15
		LeftFlipper001.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolULFlipper(Enabled)
    If Enabled AND bFlippersEnabled Then
		LeftFlipper001.EOSTorque = 0.65
        LeftFlipper001.RotateToEnd
		If leftflipper001.currentangle < leftflipper001.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper001
		Else 
			SoundFlipperUpAttackLeft LeftFlipper001
			RandomSoundFlipperUpLeft LeftFlipper001
		End If  
    Else
		LeftFlipper001.RotateToStart
		LeftFlipper001.EOSTorque = 0.15
		LeftFlipper001.RotateToStart
		If LeftFlipper001.currentangle < LeftFlipper001.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled AND bFlippersEnabled Then
		RF.Fire
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
    Else
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If        
		FlipperRightHitParm = FlipperUpSoundLevel
    End If
End Sub

' Flipper Hit Sound & Live Catch
Sub LeftFlipper_Collide(parm)
    CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
    LeftFlipperCollide parm   'This is the Fleep code
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
    RightFlipperCollide parm   'This is the Fleep code
End Sub

Sub LeftFlipper001_Collide(parm)
    PlaySoundAt "fx_rubber_flipper", LeftFlipper001
End Sub





GameTimer.interval = 20
Sub GameTimer_Timer
	Dim tmp
	RollingUpdate
	updateXlights
	Insertupdate
	For x = 0 to ubound(Bonearray)
		If bonearray(x).transy < 0 Then bonearray(x).transy = bonearray(x).transy + 0.2 
		bonearray(x).blenddisablelighting = (( tmp / 2 ) + (bonearray(x).transy)) / 133
	Next
	For x = 0 to 5
		If BonesDown(x) > -1 Then Bonearray(BonesDown(x)).transy = Bonearray(BonesDown(x)).transy - 0.6 
	Next
	If Follower Then MoveFollower
	If gi033.state = 1 Then
		x = 360
	Else
		x = 180
	End If
	If jackturn <> x Then jackturn = jackturn + 3
	If jackturn > 360 Then jackturn = jackturn - 360
	Primitive178.objrotz  = jackturn
	Primitive018.objrotz  = jackturn



	For x = 1 to 6 
		If rl(x+6) < rl(x) Then rl(x+6) = rl(x+6) + 6 : If rl(x+6) > rl(x) Then rl(x+6) = rl(x) 
		If rl(x+6) > rl(x) Then rl(x+6) = rl(x+6) - 6 : If rl(x+6) < rl(x) Then rl(x+6) = rl(x) 
	Next
    Dim bulb
	For each bulb in aGILights
        bulb.color		= rgb( rl(7) , rl(8) , rl(9) )
        bulb.colorfull	= rgb( rl(10) , rl(11) , rl(12) ) 
    Next

	x = 0
	If Blink(43,1) = 2 And Primitive154.z > 124 Then x = li043.getinplayintensity / 11
	If jumpertime > 0 Then jumpertime = jumpertime - 1 : x = ( x + light004.getinplayintensity ) / 50
	Primitive154.transy = x
	
End Sub



Sub updateXlights
	Dim Xon : Xon = 144
	Dim x, tmp
	tmp = 1
	for each x in Xrighttop  ' 1-16
		If xlight(tmp,4) > 0 Then
			xlight(tmp,0) = 0
			xlight(tmp,4) = xlight(tmp,4) - 1
		ElseIf xlight(tmp,3) > 0 Then
			 xlight(tmp,3) =  xlight(tmp,3) - 1
		Else
			Select Case xlight(tmp,1)
				case 0
					xlight(tmp,0) = 0
				case 2,4,6,8,10,12,14,16
					xlight(tmp,0) = 0			
					xlight(tmp,1) = xlight(tmp,1) - 1
					xlight(tmp,3) = xlight(tmp,2)	
				case 1,3,5,7,9,11,13,15,17
					xlight(tmp,0) = 1
					xlight(tmp,1) = xlight(tmp,1) - 1
					xlight(tmp,3) = xlight(tmp,2)
			End Select
		End If
		If xlight(tmp,0) = 1 Then
			xlight(tmp,0) = 2 
			x.blenddisablelighting = Xon
			If xlight(tmp,5) > 0 Then
				x.image = "xmas"& xlight(tmp,5)
			Else
				x.image = "xmas"& Int(rnd(1)*4)+1
			End If
		Elseif xlight(tmp,0) = 0 then
			x.image ="xmas" & 1 + tmp mod 4
			x.blenddisablelighting = 0.3
		End If
		tmp = tmp + 1
	Next

	for each x in Xlefttop  ' 17->61    17+45=61
		If xlight(tmp,4) > 0 Then
			xlight(tmp,0) = 0
			xlight(tmp,4) = xlight(tmp,4) - 1
		ElseIf xlight(tmp,3) > 0 Then
			 xlight(tmp,3) =  xlight(tmp,3) - 1
		Else
			Select Case xlight(tmp,1)
				case 0
					xlight(tmp,0) = 0
				case 2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36
					xlight(tmp,0) = 0			
					xlight(tmp,1) = xlight(tmp,1) - 1
					xlight(tmp,3) = xlight(tmp,2)	
				case 1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37
					xlight(tmp,0) = 1
					xlight(tmp,1) = xlight(tmp,1) - 1
					xlight(tmp,3) = xlight(tmp,2)
			End Select
		End If
		If xlight(tmp,0) = 1 Then
			xlight(tmp,0) = 2 
			x.blenddisablelighting = Xon
			If xlight(tmp,5) > 0 Then
				x.image = "xmas"& xlight(tmp,5)
			Else
				x.image = "xmas"& Int(rnd(1)*4)+1
			End If
		Elseif xlight(tmp,0) = 0 then
			x.image ="xmas" & 1 + tmp mod 4
			x.blenddisablelighting = 0.3
		End If
		tmp = tmp + 1
	Next

	for each x in Xrightrail  ' 62->130    62+68=139
		If xlight(tmp,4) > 0 Then
			xlight(tmp,0) = 0
			xlight(tmp,4) = xlight(tmp,4) - 1
		ElseIf xlight(tmp,3) > 0 Then
			 xlight(tmp,3) =  xlight(tmp,3) - 1
		Else
			Select Case xlight(tmp,1)
				case 0
					xlight(tmp,0) = 0
				case 2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36
					xlight(tmp,0) = 0			
					xlight(tmp,1) = xlight(tmp,1) - 1
					xlight(tmp,3) = xlight(tmp,2)	
				case 1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37
					xlight(tmp,0) = 1
					xlight(tmp,1) = xlight(tmp,1) - 1
					xlight(tmp,3) = xlight(tmp,2)
			End Select
		End If
		If xlight(tmp,0) = 1 Then
			xlight(tmp,0) = 2 
			x.blenddisablelighting = Xon
			If xlight(tmp,5) > 0 Then
				x.image = "xmas"& xlight(tmp,5)
			Else
				x.image = "xmas"& Int(rnd(1)*4)+1
			End If
		Elseif xlight(tmp,0) = 0 then
			x.image ="xmas" & 1 + tmp mod 4
			x.blenddisablelighting = 0.3
		End If
		tmp = tmp + 1
	Next

	for each x in Xkeftrail  ' 131->200    131+69=200
		If xlight(tmp,4) > 0 Then
			xlight(tmp,0) = 0
			xlight(tmp,4) = xlight(tmp,4) - 1
		ElseIf xlight(tmp,3) > 0 Then
			 xlight(tmp,3) =  xlight(tmp,3) - 1
		Else
			Select Case xlight(tmp,1)
				case 0
					xlight(tmp,0) = 0
				case 2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36
					xlight(tmp,0) = 0			
					xlight(tmp,1) = xlight(tmp,1) - 1
					xlight(tmp,3) = xlight(tmp,2)	
				case 1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37
					xlight(tmp,0) = 1
					xlight(tmp,1) = xlight(tmp,1) - 1
					xlight(tmp,3) = xlight(tmp,2)
			End Select
		End If
		If xlight(tmp,0) = 1 Then
			xlight(tmp,0) = 2 
			x.blenddisablelighting = Xon
			If xlight(tmp,5) > 0 Then
				x.image = "xmas"& xlight(tmp,5)
			Else
				x.image = "xmas"& Int(rnd(1)*4)+1
			End If
		Elseif xlight(tmp,0) = 0 then
			x.image ="xmas" & 1 + tmp mod 4
			x.blenddisablelighting = 0.3
		End If
		tmp = tmp + 1
	Next

	for each x in Xbottomtop  ' 201->230    201+29=230
		If xlight(tmp,4) > 0 Then
			xlight(tmp,0) = 0
			xlight(tmp,4) = xlight(tmp,4) - 1
		ElseIf xlight(tmp,3) > 0 Then
			 xlight(tmp,3) =  xlight(tmp,3) - 1
		Else
			Select Case xlight(tmp,1)
				case 0
					xlight(tmp,0) = 0
				case 2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36
					xlight(tmp,0) = 0			
					xlight(tmp,1) = xlight(tmp,1) - 1
					xlight(tmp,3) = xlight(tmp,2)	
				case 1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37
					xlight(tmp,0) = 1
					xlight(tmp,1) = xlight(tmp,1) - 1
					xlight(tmp,3) = xlight(tmp,2)
			End Select
		End If
		If xlight(tmp,0) = 1 Then
			xlight(tmp,0) = 2 
			x.blenddisablelighting = Xon
			If xlight(tmp,5) > 0 Then
				x.image = "xmas"& xlight(tmp,5)
			Else
				x.image = "xmas"& Int(rnd(1)*4)+1
			End If
		Elseif xlight(tmp,0) = 0 then
			x.image ="xmas" & 1 + tmp mod 4
			x.blenddisablelighting = 0.3
		End If
		tmp = tmp + 1
	Next

	RandoXlights
End Sub

Dim Stoprando : Stoprando = 0
Sub RandoXlights
	Dim x
	If Stoprando > 0 Then
		Stoprando = Stoprando - 1
	Else
		x = int(rnd(1)*230)+1
		SetXlight x,x,1,40,1,0
		Stoprando = 1
	End If
End Sub



Sub SetXlight( light1,light2, blinks,timer,delay,col )
	dim x, tmp
	tmp = delay
	If light1 > light2 Then
		For x = light1 to light2 step -1
			if tmp > 0 then xlight(x,0) = 0 else xlight(x,0) = 0
			xlight(x,1) = blinks
			xlight(x,2) = timer
			xlight(x,3) = timer
			xlight(x,4) = tmp
			tmp = tmp + delay
			xlight(x,5) = col
		Next
	Else
		For x = light1 to light2 
			xlight(x,0) = 1
			xlight(x,1) = blinks
			xlight(x,2) = timer
			xlight(x,3) = timer
			xlight(x,4) = tmp
			tmp = tmp + delay
			xlight(x,5) = col
		Next
	End If
End Sub

' light1,light2,blinks,timer,delay,color
' SetXlight 1,230,10,5,2,3   ' all one color
' SetXlight 1,230,10,5,2,0   ' all rgb

' SetXlight   1, 16,10,5,2,2   ' all these 5 at once
' SetXlight  17, 61,10,5,2,3   ' 
' SetXlight 130, 62,10,5,2,0   ' 
' SetXlight 131,200,10,5,2,0   ' 
' SetXlight 201,230,20,5,2,4   ' 

' righttop	 1-16
' lefttop	17-61
' rightrail	62->130
' leftrail  131->200 
' bottomtop	201->230


Dim xlight(250,5)
Sub setupXlights
	Dim x
	For x = 0 to 250 
		xlight(x,0) = 0	' on off
		xlight(x,1) = 0	' blinks
		xlight(x,2) = 0	' timer
		xlight(x,3) = 0	' countdown
		xlight(x,4) = -1' delay
		xlight(x,5) = 0	' Forced color = 1-4  off = 0 = rnd
	Next
End Sub




Dim DMD_dog_Dir
Sub BoneRampStartSound_Hit
    DOF 251, DOFPulse
	shakethebones = 6
	activeball.vely = 4
	pupevent 901
	WireRampOn False
End Sub
Sub boneramp003_hit
	activeball.velx = 8 : activeball.vely = 5
	PlaySound"_BoneFake",1,BackboxVolume
End Sub
Sub boneramp041_hit
	If RndInt(1,2) = 1 Then BoneCalls 
	activeball.velx = 1 : activeball.vely = 13
End Sub

Sub RightRampStart_Hit
    DOF 252, DOFPulse
	WireRampOn False
	BlinkLights 57,57,3,5,1,0
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
	FlushSplashQ
    DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,60 , "warning" , 19 ,0,0 
	Playsoundat "fx_shaker",drain
	PlayVoice "vo_yousuck" & int(rnd(1)*2)+1 
	Playsound"_Tilt",1,BackboxVolume

    End if
    If(NOT Tilted) AND Tilt > 15 Then 'If more that 15 then TILT the table

        'display Tilt
        InstantInfoTimer.Enabled = False
		FlushSplashQ
		DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,150 , "tilt" , 19 ,0,0  
		Playsoundat "fx_shaker",drain

	'	PlayVoice "vo_yousuck" & int(rnd(1)*2)+1 
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
        StopMBmodes
		PlaySound"_jinglebells"
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
		PlaySong ""
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        LeftFlipper001.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        Tilted = False
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper1.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0) Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        vpmtimer.Addtimer 2000, "EndOfBall() '"
        TiltRecoveryTimer.Enabled = False
		FlushSplashQ
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
            If song <> "" Then PlaySound Song, -1, SongVolume 
        End If
    End If
End Sub

Sub ChangeSong
	If WizardMode(CurrentPlayer) = 2 Then PlaySong "mu_wizard" : Exit Sub
	If WizardMode(CurrentPlayer) = 1 Then PlaySong "mu_nightmareMB" : Exit Sub 'Nightmare MB
    If bJackMBStarted Then PlaySong "mu_boogieMB" : Exit Sub 'Boogie MB
	If blink(43,1) = 2 Then PlaySong "mu_main" &  BallsRemaining(CurrentPlayer) : Exit Sub		' scoop is lit so no mode song
	If TranslateLetters( ModeRunning(CurrentPlayer)) =  5 Then PlaySong "mu_sally" : Exit Sub 					 'RaiseTheDead
	If TranslateLetters( ModeRunning(CurrentPlayer)) =  2 Then PlaySong "mu_townmeeting" : Exit Sub				 'TownMeeting
	If TranslateLetters( ModeRunning(CurrentPlayer)) =  4 Then PlaySong "mu_bequick" : Exit Sub					 'BeQuick
	If TranslateLetters( ModeRunning(CurrentPlayer)) =  3 Then PlaySong "mu_whatsthis" : Exit Sub				 'WhatsThis
	If TranslateLetters( ModeRunning(CurrentPlayer)) = 10 Then PlaySong "mu_sandy" : Exit Sub					 'SandyClaws
	If TranslateLetters( ModeRunning(CurrentPlayer)) = 17 Then PlaySong "mu_makingxmas" : Exit Sub				 '80Days
	If TranslateLetters( ModeRunning(CurrentPlayer)) = 20 Then PlaySong "mu_nimble" : Exit Sub					 'Nimble
	If TranslateLetters( ModeRunning(CurrentPlayer)) = 21 Then PlaySong "mu_savexmas" :  Exit Sub				 'SaveChristmas
    If BallsRemaining(CurrentPlayer) > 0 Then PlaySong "mu_main" &  BallsRemaining(CurrentPlayer) : Exit Sub     'Gameplay
End Sub

'idigsounds

Sub StopSong(name)
    StopSound name
End Sub

Sub shootthetargets()
    If RndNbr(5) = 1 Then PlayVoice "vo_shootthetargets"
End Sub


sub bonecalls
Select Case RndNbr(2)
		Case 1:PlayVoice "sfx_fetch"
		Case 2:PlayVoice "vo_tickletickle"
	End Select
End Sub

sub bonuscalls
Select Case RndNbr(2)
		Case 1:PlayVoice "vo_bonus1"
		Case 2:PlayVoice "vo_bonus2"
	End Select
End Sub

Sub nimblecalls
Select Case RndNbr(2)
		Case 1:Playsound "vo_rooster",1,BackboxVolume
		Case 2:Playsound "vo_somethingup",1,BackboxVolume
	End Select
End Sub


Sub PlayHighScoreQuote
    Select Case RndNbr(7)
        Case 1:PlaySound "vo_thatssplendid",1,BackboxVolume
        Case 2:PlaySound "vo_excellentscore",1,BackboxVolume
        Case 3:PlaySound "vo_greatscore",1,BackboxVolume
        Case 4:PlaySound "_EndGmGood-001",1,BackboxVolume
        Case 5:PlaySound "_EndGmGood-002",1,BackboxVolume
		Case 6:PlaySound "vo_whatisyourname",1,BackboxVolume
		Case 7:PlaySound "_witchesdream",1,BackboxVolume
    End Select
End Sub

Sub ReviveSound
    Select Case RndNbr(2)
        Case 1:PlaySound "_ballsave-002",1,BackboxVolume
        Case 2:PlaySound "_ballsave-004",1,BackboxVolume
    End Select
End Sub

Sub LockIsLit
	Select Case RndNbr(3)
        Case 1:PlayVoice "vo_lockislitA"
        Case 2:PlayVoice "vo_lockislitB"
        Case 3:PlayVoice "vo_lockislitC"
    End Select
End Sub

Sub DontForgetTheJackpot()
    If RndNbr(4) = 1 Then PlayVoice "vo_getthestupidjackpot"
End Sub


Sub PlayNotGoodScore
    Select Case RndNbr(10)
        Case 1:PlaySound "_GmOverNoInit-001",1,BackboxVolume
        Case 2:PlaySound "_GmOverNoInit-002",1,BackboxVolume
        Case 3:PlaySound "_GmOverNoInit-003",1,BackboxVolume
        Case 4:PlaySound "_GmOverNoInit-004",1,BackboxVolume
        Case 5:PlaySound "vo_howfrightening",1,BackboxVolume
        Case 6:PlaySound "vo_perhapsimprove",1,BackboxVolume
		Case 7 PlaySound "_MostHorrible",1,BackboxVolume
		Case 8:PlaySound "vo_confidence",1,BackBoxVolume
		Case 9:PlaySound "vo_greatshape",1,BackBoxVolume
		Case 10 : PlaySound"_negative-002",1,BackboxVolume
    End Select
End Sub
      

Sub BoneWind()
    If RndNbr(4) = 1 Then PlaySound "_bonewind",1,BackBoxVolume
End Sub


Sub Mayorsounds
    Select Case RndNbr(3)
        Case 1:PlayVoice "_MayorHum1"
        Case 2:PlayVoice "_MayorHum2"
        Case 3:PlayVoice "_MayorHum3"
    End Select
End Sub

Sub Jacklight
    Select Case RndNbr(3)
        Case 1:PlayVoice "_jacklight-001"',1,BackboxVolume * 0.5
        Case 2:PlayVoice "_jacklight-002"',1,BackboxVolume * 0.5
        Case 3:PlayVoice "_jacklight-003"',1,BackboxVolume * 0.5
    End Select
End Sub

Sub PlayEndQuote
   Select Case RndNbr(1)
       Case 1:PlaySound "vo_endquote",1,BackboxVolume
    End Select
End Sub

Sub PlayThunder
	Select Case RndInt(1,5)
		Case 1 : PlaySfx "sfx_thunder1"',1,BackboxVolume * 0.4
		Case 2 : PlaySfx "sfx_thunder2"',1,BackboxVolume * 0.4
		Case 3 : PlaySfx "sfx_thunder3"',1,BackboxVolume * 0.4
		Case 4 : PlaySfx "sfx_thunder4"',1,BackboxVolume * 0.4
		Case 5 : PlaySfx "sfx_thunder5"',1,BackboxVolume * 0.4
	End Select
	ShakeTheBones = 3
	Objlevel(1) = 1 : FlasherFlash1_Timer : blinklights 40,40,2,8,3,0
End Sub

Sub PlayElectro
    PlaySound "sfx_tambo",1,BackBoxVolume 
End Sub

Sub PlayTomb
Select Case RndInt(1,3)
	Case 1 : Playsfx2 "_Tomb-001"
	Case 2 : Playsfx2 "_Tomb-002"
    Case 3 : Playsfx2 "_Tomb-003"
	End Select
End Sub


Sub CoinInQuote
	Select Case RndInt(1,8)
		Case 1: PlayVoice "_MayorGents"
        Case 2: PlayVoice "_Greetings-004"
        Case 3: PlayVoice "_Greetings-005"
        Case 4: PlayVoice "_Greetings-001"
        Case 5: PlayVoice "_Greetings-002"
        Case 6: PlayVoice "_Greetings-003"
		Case 7: PlayVoice "vo_whatapleasure"
		Case 8: PlayVoice "vo_thankyou"

     End Select
	 StopSound"mu_gameover"
	 StopSound"mu_ending"
	 StopSound"vo_endquote"
End Sub

Sub DrainSounds
	Select Case RndInt(1,8)
	Case 1 : PlaySound "vo_byebyedollface",1,BackboxVolume
	Case 2 : PlaySound "vo_ashestodust",1,BackboxVolume
    Case 3 : PlaySound "vo_curiositykilledthecat",1,BackboxVolume
	Case 4 : PlaySound"_negative-001",1,BackboxVolume
	Case 5 : PlaySound"vo_hopetherestilltime",1,BackboxVolume * 0.6
	Case 6 : PlaySound"vo_somuchexcitement",1,BackBoxVolume
	Case 7 : PlaySound"_negative-003",1,BackboxVolume
	Case 8 : PlaySound"vo_ohwell",1,BackBoxVolume

	End Select
End Sub

'idig
Sub Greetings
    Select Case RndNbr(1)
		Case 1:PlayVoice "vo_timeubegun"
    End Select
	Stopsound"_jinglebells"
End Sub

Sub BallSaveSound
	Select Case RndInt(1,3)
	Case 1 : PlaySound "vo_giveballback",1,BackboxVolume/4
	Case 2 : PlaySound "_ballsave-001",1,BackboxVolume/3
    Case 3 : PlaySound "_ballsave-003",1,BackboxVolume/3
	End Select
End Sub


sub PositiveCallout
	Select Case RndInt(1,10)
	Case 1 : PlayVoice "_positive-001"
	Case 2 : PlayVoice "_positive-002"
    Case 3 : PlayVoice "_positive-003"
    Case 4 : PlayVoice "_positive-004"
    Case 5 : PlayVoice "_positive-005"
    Case 6 : PlayVoice "_positive-006"
    Case 7 : PlayVoice "_positive-007"
	Case 8 : PlayVoice "vo_bonedaddy" 
	Case 9 : PlayVoice "vo_welldone"  
	Case 10 : PlayVoice "_wedidit" 
	End Select
End Sub

sub PresentCallout
	Select Case RndInt(1,3)
	Case 1 : PlayVoice "_present-001"',1,BackboxVolume
	Case 2 : PlayVoice "_present-002"',1,BackboxVolume
    Case 3 : PlayVoice "vo_openitupquickly"',1,BackboxVolume
	End select
End Sub


Dim currentVoiceIndex ' Declare a global variable to store the current index  'idig
currentVoiceIndex = 1 ' Initialize the index to start from the first voice

Sub RightSlingSounds
    Select Case currentVoiceIndex
        Case 1: PlaySfx "sfx_electro2"
        Case 2: PlaySfx "sfx_electro7"
        Case 3: PlaySfx "sfx_electro1"
        Case 4: PlaySound "vo_myjewel",1,BackboxVolume
        Case 5: PlaySfx "sfx_electro3"
        Case 6: PlaySfx "sfx_electro4"
        Case 7: PlaySfx "sfx_electro5"
        Case 8: PlaySfx "sfx_electro8"
        Case 9: PlaySound "vo_igor",1,BackboxVolume
        Case 10:PlaySfx "sfx_electro9"
        Case 11:PlaySfx "sfx_electro6"
		Case 12:PlaySfx "sfx_electro2"
		Case 13:PlaySfx "sfx_electro1"
		Case 14:PlaySound "_naughty",1,BackboxVolume/2
		Case 15:PlaySfx "sfx_electro3"
		Case 16:PlaySfx "sfx_electro4"
		Case 17:PlaySfx "sfx_electro5"
     End Select
    
    ' Increment the index to play the next sound
    currentVoiceIndex = currentVoiceIndex + 1
    
    ' If the index exceeds the number of voices, reset it back to 1
    If currentVoiceIndex > 17 Then
        currentVoiceIndex = 1
    End If
End Sub


Dim currentVoiceIndexB ' Declare a global variable to store the current index  'idig
currentVoiceIndexB = 1 ' Initialize the index to start from the first voice

Sub savexmascallout
    Select Case CurrentVoiceIndexB
        Case 1:PlaySound "vo_headoftheteam",1,BackboxVolume
        Case 2:PlaySound "sfx_sleighbell3",1,BackboxVolume
		Case 3:PlaySound "vo_whosnextonmylist",1,BackboxVolume
		Case 4:PlaySound "sfx_sleighbell3",1,BackboxVolume
		Case 5:PlaySound "vo_wonttheybesurprised",1,BackboxVolume
		Case 6:PlaySound "sfx_sleighbell3",1,BackboxVolume
		Case 7:PlaySound "_lookzero",1,BackboxVolume
		Case 8:PlaySound "sfx_sleighbell3",1,BackboxVolume
End Select
' Increment the index to play the next sound
    currentVoiceIndexB = currentVoiceIndexB + 1
    
    ' If the index exceeds the number of voices, reset it back to 1
    If currentVoiceIndexB > 8 Then
        currentVoiceIndexB = 1
    End If
End Sub
Dim currentVoiceIndexC ' Declare a global variable to store the current index  'idig
currentVoiceIndexC = 1 ' Initialize the index to start from the first voice
Sub sandycalls
Select Case CurrentVoiceIndexC
		Case 1:PlaySound "vo_sandyclaws1",1,BackboxVolume
		Case 2:PlaySound "vo_sandyclaws1",1,BackboxVolume
		Case 3:PlaySound "vo_sandyclaws2",1,BackboxVolume
		Case 4:PlaySound "vo_sandyclaws3",1,BackboxVolume
		Case 5:PlaySound "vo_sandyclaws4",1,BackboxVolume
		Case 6:PlaySound "vo_youdontneedtoworry",1,BackboxVolume
	
	End Select
' Increment the index to play the next sound
    currentVoiceIndexC = currentVoiceIndexC + 1
    
    ' If the index exceeds the number of voices, reset it back to 1
    If currentVoiceIndexC > 6 Then
        currentVoiceIndexC = 1
    End If

End Sub

Dim currentVoiceIndexD ' Declare a global variable to store the current index  'idig
currentVoiceIndexD = 1 ' Initialize the index to start from the first voice
Sub JackHereSound
Select Case CurrentVoiceIndexD
		Case 1:PlaySound "_jack_you_home",1,BackboxVolume
		Case 2:PlaySound "vo_hesnothome",1,BackboxVolume
		Case 3:PlaySound "vo_nothomeallnight",1,BackboxVolume
		Case 4:PlaySound "vo_whereishe",1,BackboxVolume
		Case 5:PlaySound "_jack_please_im_only_an_elected",1,BackboxVolume
		Case 6:PlaySound "vo_leftuntillnexthalloween",1,BackboxVolume
	
	End Select
' Increment the index to play the next sound
    currentVoiceIndexD = currentVoiceIndexD + 1
    
    ' If the index exceeds the number of voices, reset it back to 1
    If currentVoiceIndexD > 6 Then
        currentVoiceIndexD = 1
    End If

End Sub

Dim currentVoiceIndexE ' Declare a global variable to store the current index  'idig
currentVoiceIndexE = 1 ' Initialize the index to start from the first voice
Sub towncalls
Select Case CurrentVoiceIndexE
		Case 1:PlaySound "vo_town3",1,BackboxVolume
		Case 2:PlaySound "vo_town4",1,BackboxVolume/2
		Case 3:PlaySound "vo_town6",1,BackboxVolume
		Case 4:PlaySound "vo_town7",1,BackboxVolume
		Case 5:PlaySound "vo_town8",1,BackboxVolume
		Case 6:PlaySound "vo_town9",1,BackboxVolume
		Case 7:PlaySound "vo_town10",1,BackboxVolume
		Case 8:PlaySound "vo_town11",1,BackboxVolume
		Case 9:PlaySound "vo_town12",1,BackboxVolume
	End Select
' Increment the index to play the next sound
    currentVoiceIndexE = currentVoiceIndexE + 1
    
    ' If the index exceeds the number of voices, reset it back to 1
    If currentVoiceIndexE > 9 Then
        currentVoiceIndexE = 1
    End If
End Sub


Dim currentVoiceIndexF ' Declare a global variable to store the current index  'idig
currentVoiceIndexF = 1 ' Initialize the index to start from the first voice
Sub makingxmascalls
Select Case CurrentVoiceIndexF
		Case 1:PlaySound "vo_makingxmas",1,BackboxVolume
		Case 2:PlaySound "vo_makingxmas2",1,BackboxVolume
		Case 3:PlaySound "vo_makingxmas3",1,BackboxVolume
		Case 4:PlaySound "vo_makingxmas4",1,BackboxVolume
		Case 5:PlaySound "vo_makingxmas5",1,BackboxVolume
		Case 6:PlaySound "vo_makingxmas6",1,BackboxVolume
		Case 7:PlaySound "vo_makingxmas7",1,BackboxVolume

	End Select
' Increment the index to play the next sound
    currentVoiceIndexF = currentVoiceIndexF + 1
    
    ' If the index exceeds the number of voices, reset it back to 1
    If currentVoiceIndexF > 7 Then
        currentVoiceIndexF = 1
    End If
End Sub


Dim currentVoiceIndexG ' Declare a global variable to store the current index  'idig
currentVoiceIndexG = 1 ' Initialize the index to start from the first voice
Sub quickcalls
Select Case CurrentVoiceIndexG
		Case 1:PlayVoice "vo_bequick-001"
		Case 2:PlayVoice "vo_bequick-002"
		Case 3:PlayVoice "vo_bequick-003"
		Case 4:PlayVoice "vo_bequick-005"
		Case 5:PlayVoice "vo_bequick-006"
		Case 6:PlayVoice "vo_bequick-007"

	End Select
' Increment the index to play the next sound
    currentVoiceIndexG = currentVoiceIndexG + 1
    
    ' If the index exceeds the number of voices, reset it back to 1
    If currentVoiceIndexG > 6 Then
        currentVoiceIndexG = 1
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
    PlaySoundAt "fx_GiOn", li018 'about the center of the table
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next


End Sub

Sub GiOff
    PlaySoundAt "fx_GiOff", li018 'about the center of the table
    DOF 118, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
	LightSeqGi.stopplay
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
            LightSeqGi.Play SeqBlinking, , 5, 20
        Case 4 'seq up
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqUpOn, 25, 3
        Case 5 'seq down
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqDownOn, 25, 3
		Case 6
			LightSeqGi.UpdateInterval = 33
			LightSeqGi.Play SeqBlinking, , 3, 33
    End Select
End Sub

Sub presentseffect(n)
	LightSeqpresents.stopplay
	Select Case n
		Case 1 : LightSeqpresents.UpdateInterval = 25		: LightSeqpresents.Play SeqBlinking, , 10, 10
		Case 2 : LightSeqpresents.UpdateInterval = 25		: LightSeqpresents.Play SeqRandom, 50, , 800
		Case 3 : LightSeqpresents.UpdateInterval = 25		: LightSeqpresents.Play SeqCircleOutOn, 15, 2	: LightSeqpresents.Play SeqCircleinoff, 15, 2


	End Select
End Sub

Sub LightEffect(n)
	LightSeqInserts.stopplay
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
			LightSeqInserts.centerx = 450
			LightSeqInserts.centery = 1280
            LightSeqInserts.UpdateInterval = 40
            LightSeqInserts.Play SeqBlinking, , 15, 25
        Case 2 'random
			LightSeqInserts.centerx = 450
			LightSeqInserts.centery = 1280
            LightSeqInserts.UpdateInterval = 25
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
			LightSeqInserts.centerx = 450
			LightSeqInserts.centery = 1280
            LightSeqInserts.UpdateInterval = 20
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 4 'center - used in the bonus count
			LightSeqInserts.centerx = 450
			LightSeqInserts.centery = 1280
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqCircleOutOn, 15, 2
        Case 5 'top down
			LightSeqInserts.centerx = 450
			LightSeqInserts.centery = 1280
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 15, 2
        Case 6 'down to top
			LightSeqInserts.centerx = 450
			LightSeqInserts.centery = 1280
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 15, 1
        Case 7 'center from the magnet
            LightSeqMG.UpdateInterval = 4
            LightSeqMG.Play SeqCircleOutOn, 15, 1
		Case 8
			LightSeqInserts.stopplay
			LightSeqInserts.UpdateInterval = 8
			LightSeqInserts.centerx = 800
			LightSeqInserts.centery = 1140
			LightSeqInserts.Play SeqCircleinOn, 80, 1
		Case 9
			LightSeqInserts.stopplay
			LightSeqInserts.UpdateInterval = 8
			LightSeqInserts.centerx = 800
			LightSeqInserts.centery = 1140
			LightSeqInserts.Play SeqCircleOutOn, 44, 1

    End Select
End Sub

Sub FlashEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqFlashers.Play SeqAlloff
        Case 1 'all blink
            LightSeqFlashers.UpdateInterval = 40
            LightSeqFlashers.Play SeqBlinking, , 5, 25
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


'************************************
' Diverse Collection Hit Sounds v3.0
'************************************
'extra collections in this table
Sub aBlueRubbers_Hit(idx)
    Select Case RndNbr(15)

'        Case 5:PlayVoice ""
'        Case 6:PlayVoice ""
'        Case 7:PlayVoice ""
    End Select
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function


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
	For i = nightmarestart to nightmarestart+23 : Blink(i,1)=0 : Next
	StopSong "mu_ending"
	StopSound"vo_endquote"
	Greetings 'idig
'	Playsound"mu_pre"
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
    bSkillShotReady = True
	DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,25522 , "skillshot" , 500,0,0
    ' create a new ball in the shooters lane
    CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))


Dim BumperSpecial : BumperSpecial = 0

Sub ResetForNewPlayerBall()
	FindJack = 0
	combocombo = 0
	ChangeGi PURPLE
	DMD_pointsCounter = 0
	DMD_pointsgain = 0
	updatecountdown 0

	EOB_FastSkip = False
	EOB_Faster = False
	BG_imageDMD = 55 + RndInt(1,5) * 2
	SuperLoopCounter = 0
	SuperLoopTimer = 0
	SuperLoopJPs = 0
	DogChaseCounter = 0
	DogChaseTimer = 0
	XmasHurryupCounter = 0
	XmasHurryupTimer = 0

	li053.state = 0

	BumperSpecial = 0
	arrowlights(17,1) = 0 '	Blink(38,1) = 0


	
	ResetTargetLights

    ' set the current players bonus multiplier back down to 1X
    BonusMultiplier(CurrentPlayer) = 1

    ' reduce the playfield multiplier
    DecreasePlayfieldMultiplier


    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False

    'Reset any table specific
    ResetNewBallVariables
	ResetWeaponLights
	UpdateWeaponLights
	UpdateExtraballLight
    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
	FlushSplashQ

	UpdateLockLights
	UpdateReviveLight
	
	Blink(48,1) = 1 ' left revive = on at ball Start
'Change the music ?
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1
	StopSound"mu_suspense2"
    ' kick it out..
 



' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield > 1 Then
		plunger.timerinterval = 1333
		plunger.timerenabled = True
        DOF 143, DOFPulse
        bMultiBallMode = True
        bAutoPlunger = True
	Else
		plunger.timerinterval = 777
		plunger.timerenabled = True
    End If
End Sub


Sub plunger_Timer
	plunger.timerenabled = False	
	BallRelease.Kick 90, 4
	PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
    'and eject the first ball
'    CreateMultiballTimer_Timer
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Or Plunger.timerenabled Then
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
		DMD_newmode = 3
        Playsound "mu_suspense2" ,1,BackboxVolume / 8 'idig
 

        ' add a bit of a delay to allow for the bonus points to be shown & added up
    Else 'if tilted then only add a short delay and move to the 2nd part of the end of the ball
        vpmtimer.addtimer 200, "EndOfBall2 '"
    End If
End Sub


' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()

	DMD_newmode = 2	' be sure this is going for extraballs too
	StopSplash
	FlushSplashQ

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
			Blink(39,1) = 0		' LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
'        DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, "vo_replay"

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()
		bSkillShotReady = True
		DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,25522 , "skillshot" , 500,0,0
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


        ' reset the playfield for the new player (or new ball)
		ResetForNewPlayerBall()
		bSkillShotReady = True
		DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,25522 , "skillshot" , 500,0,0
        ' AND create a new ball
        CreateNewBall()
UpdateMysteryLights
        ' play a sound if more than 1 player
        If PlayersPlayingGame > 1 Then
            Select Case CurrentPlayer
				Case 1 : PlayVoice "vo_player1"
                Case 2 : PlayVoice "vo_player2"
				Case 3 : PlayVoice "vo_player3"
				Case 4 : PlayVoice "vo_player4"
            End Select
        Else
            PlayVoice "vo_youareup"
        End If
    End If


End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..


Sub EndOfGame()
	DMD_newmode = 1
	DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,222 , "gameover" , 500,0,0

    'debug.print "End Of Game"
    ' just ended your game then play the end of game tune
	PlaySound "mu_ending",1,BackboxVolume / 1.7
	StopSound "mu_suspense2"
    vpmtimer.AddTimer 4000, "PlayEndQuote '"
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
    
   vpmtimer.AddTimer 7000, "StartAttractMode '"

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

dim DoDeliverNextball : DoDeliverNextball = False
Sub Drain_Hit()
	DoDeliverNextball = False
	Stoprando = 1200
	SetXlight   1, 16,13,5,0,4   ' all these 5 at once
	SetXlight  17, 61,13,5,0,4   ' 
	SetXlight 130, 62,13,5,0,4   ' 
	SetXlight 131,200,13,5,0,4   ' 
	SetXlight 201,230,13,5,0,4   
	LightEffect 5
    ' Destroy the ball
    Drain.DestroyBall
    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball
    ' Exit Sub ' only for debugging - this way you can add balls from the debug window

    BallsOnPlayfield = BallsOnPlayfield - 1

    ' pretend to knock the ball into the ball storage mech
    RandomSoundDrain Drain
    'if Tilted the end Ball Mode
 

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) or BallsaveExtended < 3 Then
            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
			AddMultiball 1  
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            ' stop the ballsaver timer during the launch ball saver time, but not during multiballs
            If NOT bMultiBallMode Then
				DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,170 , "ballsave" , 500,0,0
                DOF 253, DOFPulse
				BallSaveSound
				If mBalls2Eject = 0 Then BallSaverTimerExpired_Timer
            End If
        Elseif ReviveBall1 Then
			ReviveBall1 = False
			AddMultiball 1  
            bAutoPlunger = True
			DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,160 , "revive" ,49,0,0
            DOF 156, DOFPulse
        Elseif ReviveBall2 Then
			ReviveBall2 = False
			AddMultiball 1  
            bAutoPlunger = True
			DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,160 , "revive" ,49,0,0
            DOF 157, DOFPulse

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
				Primitive178.visible = True
				Primitive018.visible = False
				greenoogie = False

				LightEffect 5
				If Delivered_mission(CurrentPlayer) = 8 Then  
					TurnOffAllBlueLights
					Blink(43,1) = 2 : BlinkLights 43,43,12,6,6,0
					DoDeliverNextball = True
'					DMD_StartSplash "DELIVER","PRESENT" ,FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
				Elseif Delivered_mission(CurrentPlayer) = 23 Then
					TurnOffAllBlueLights
					Blink(43,1) = 2 : BlinkLights 43,43,12,6,6,0
					DoDeliverNextball = True
'					DMD_StartSplash "DELIVER","PRESENT" ,FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
				Else
					If WizardMode(CurrentPlayer) > 0 Then
						StopWizard 
						ModeRunning(CurrentPlayer) = 0
						Delivered_mission(CurrentPlayer) = 0

					Else
						Presents (ModeRunning(CurrentPlayer),CurrentPlayer) = 1 ' letter solid
						ModeRunning(CurrentPlayer) = RndInt(1,24)
						For x = 1 to 24
							If Presents (ModeRunning(CurrentPlayer),CurrentPlayer) =  0 Then Exit For
							If x = 24 Then msgbox "something wrong nextmode fail"
							ModeRunning(CurrentPlayer) = ModeRunning(CurrentPlayer) + 1
							If ModeRunning(CurrentPlayer) > 24 Then ModeRunning(CurrentPlayer) = 1
						Next
	'					ModeRunning(CurrentPlayer) = NextMode(currentplayer)
	'					NewPresentsNextMode
						Delivered_mission(CurrentPlayer) = Delivered_mission(CurrentPlayer)  + 1
						If Delivered_mission(CurrentPlayer) = 15 Then Extraball_one(CurrentPlayer) = 1 
						If JackResetAvail(CurrentPlayer) = True Then
							JackResetCounter(CurrentPlayer) = JackResetCounter(CurrentPlayer) + 1
							If JackResetCounter(CurrentPlayer) = JackResetNeeded(CurrentPlayer) Then
								JackResetCounter(CurrentPlayer) = 10
								JackResetNeeded(CurrentPlayer) = JackResetNeeded(CurrentPlayer) + 2
								If JackResetNeeded(CurrentPlayer) > 6 Then JackResetNeeded(CurrentPlayer) = 6
								JackResetAvail(CurrentPlayer) = False
								' reset Jack
								Mystery(CurrentPlayer, 1) = 0
								Mystery(CurrentPlayer, 2) = 0
								Mystery(CurrentPlayer, 3) = 0
								Mystery(CurrentPlayer, 4) = 0
							End If
						End If
					End If



				End If
				BlinkAllPresents 3,2
				DrainSounds
				Playsound "sfx_churchbell",1,BackboxVolume
				TurnOffThingsAtDrain
                ' End Mode and timers
                StopSong Song
                ChangeGi white
                ChangeGIIntensity 1
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
				EOB_FastSkip = True

                vpmtimer.addtimer 2323, "EndOfBall '" 'the delay is depending of the animation of the end of ball, if there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub


Sub TurnOffThingsAtDrain

				For x = 1 to 6
					WeaponHits(CurrentPlayer, x) = 0
				Next
				UpdateWeaponLights	
				arrowlights(17,1) = 0
				arrowlights(16,1) = 0
				BumperSpecial = 0
				blinklights 53,54,2,1,10,5
				DMD_pointsCounter = 0
				DMD_pointsgain = 0

				ModeTimer.Enabled = False ' turn off everything mby running at Drain
				MegaSpinnersTime = 0 : MegaSpinnersActive = 0
				XmasHurryupTimer = 0 
				DogChaseTimer = 0
				SuperLoopTimer = 0
				DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,150 , "balllost" , 500,0,0
			    DOF 254, DOFPulse	

				For x = 1 to 17 
					ArrowLights(x,1) = 0
					ArrowLights(x,2) = 0
					ArrowLights(x,3) = 0
				Next

				ResetAllBlink

End Sub
' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
	Blink(0,1) = 2
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
    If Not bAutoPlunger Then
    DOF 208, DOFOn
    End If
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
	Blink(0,1) = 0
	BlinkLights 0,0,10,5,5,0
	ShakePlunger 8


    lighteffect 6
    bBallInPlungerLane = False
    DOF 208, DOFOff
    DOF 155, DOFPulse
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
        Select Case RndNbr(3)
            Case 1:Playsound "vo_whatareyouwaitingfor"  ,1,BackboxVolume  /2  'idig
            Case 2:Playsound "vo_areyouplayingthisgame",1,BackboxVolume/2
            Case 3:Playsound "vo_thinkyoucan",1,BackboxVolume/2
        End Select
    Else
        Select Case RndNbr(3)
            Case 1:Playsound "vo_whatareyouwaitingfor",1,BackboxVolume/2
            Case 2:Playsound "vo_thinkyoucan",1,BackboxVolume/2
            Case 3:Playsound "vo_areyouplayingthisgame",1,BackboxVolume/2
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

	Blink(39,1) = 2		' LightShootAgain.State = 2

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
			Blink(39,1) = 0		' LightShootAgain.State = 0
    ' if the table uses the same lights for the extra ball or replay then turn them on if needed
    If ExtraBallsAwards(CurrentPlayer) > 0 Then
			Blink(39,1) = 1		' LightShootAgain.State = 1
    End If
	BallsaveExtended = 0
	blinklights 39,39,10,6,6,2
End Sub

Dim BallsaveExtended
BallsaveExtended = 10
Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
	blinklights 39,39,4,6,6,2
	Blink(39,1) = 2		' LightShootAgain.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board

Sub AddScore(points) 'normal score routine
    If Tilted Then Exit Sub

	Points = points * PlayfieldMultiplier(CurrentPlayer)
	If Points > DMD_pointsgain Then
		DMD_pointsgain = DMD_pointsgain + points
		If points > 90000 then DMD_pointsCounter = 100 Else DMD_pointsCounter = 70
	Else
		DMD_pointsgain = DMD_pointsgain + points
		If DMD_pointsCounter < 40 Then DMD_pointsCounter = 40
	End If

    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points 
' you may wish to check to see if the player has gotten a replay
End Sub

Sub AddScore2(points) 'used in jackpots, skillshots, combos, and bonus as it does not use the PlayfieldMultiplier
    If Tilted Then Exit Sub

	If Points > DMD_pointsgain Then
		DMD_pointsgain = DMD_pointsgain + points
		If points > 90000 then DMD_pointsCounter = 100 Else DMD_pointsCounter = 70
	Else
		DMD_pointsgain = DMD_pointsgain + points
		If DMD_pointsCounter < 40 Then DMD_pointsCounter = 40
	End If

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
'    DMD "_", CL("INCREASED JACKPOT"), "_", eNone, eNone, eNone, 1000, True, ""
' you may wish to limit the jackpot to a upper limit, ie..
'	If (Jackpot >= 6000000) Then
'		Jackpot = 6000000
' 	End if
'End if
End Sub

Sub AddSuperJackpot(points) 'not used in this table
    If Tilted Then Exit Sub
End Sub







Sub AwardExtraBall()
    '   If NOT bExtraBallWonThisBall Then 'in this table you can win several extra balls

	If Extraball_one(CurrentPlayer) = 1 Then
		Extraball_one(CurrentPlayer) = 2
	elseif Extraball_two(CurrentPlayer) = 1 Then
		Extraball_two(CurrentPlayer) = 2
	elseif Extraball_3(CurrentPlayer) = 1 Then
		Extraball_3(CurrentPlayer) = 2
	End If
	ExtraballsGotten (CurrentPlayer) = ExtraballsGotten (CurrentPlayer) + 1
	If ExtraballsGotten (CurrentPlayer) > maxExtraballs Then
		AddScore score_NO_EB
	Else
		AddScore score_Extraball
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
		If blink(39,1) = 0 Then Blink(39,1) = 1	
	End If

	UpdateExtraballLight
	PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)

    DOF 121, DOFPulse
    DOF 124, DOFPulse
	BlinkLights 52,52,10,5,5,0
	BlinkLights nightmarestart,nightmarestart+23,10,5,5,0
	FlushSplashQ
	DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,170 , "extraball" , 500,0,0 
	Playsound"sfx_crowdcheer",1,BackboxVolume / 3.45
	PlayVoice "_pumpkinking" ',1,BackboxVolume
    GiEffect 2
    LightEffect 2
'    END If
End Sub

Sub AwardSpecial()
	FlushSplashQ
	DMD_StartSplash "GAME WON","EXTRA",FontHugeOrange, FontHugeRedhalf,5 , "blinktop1" ,55,0,0
	playsound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    DOF 124, DOFPulse
    Credits = Credits + 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
    GiEffect 2
End Sub








'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 250000000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "JCK" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 150000000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "SAL" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 100000000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "OOG" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 50000000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "XMS" End If
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
    HighScoreName(0) = "JCK"
    HighScoreName(1) = "SAL"
    HighScoreName(2) = "OOG"
    HighScoreName(3) = "XMS"
    HighScore(0) = 250000000
    HighScore(1) = 150000000
    HighScore(2) = 100000000
    HighScore(3) = 50000000
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
	DMD_newmode = 5
    hsbModeActive = True
	
    'PlayVoice "vo_whatisyourname"

    hsLetterFlash = 0
	TempTopStr = ""
	TempBotStr = ""
    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow
    hsCurrentLetter = 1
    HighScoreDisplayNameNow()

    HighScoreFlashTimer.Interval = 250
    HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        PlaySound "sfx_tambo",1,BackBoxVolume
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        PlaySound "sfx_tambo",1,BackBoxVolume
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then
			PlaySound "sfx_sleighbell1",1,BackBoxVolume
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            PlayVoice "fx_Esc"
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

Dim TempTopStr
Dim TempBotStr
Sub HighScoreDisplayName()
    Dim i


    TempTopStr = "ENTER INITIALS"
'    dLine(0) = ExpandLine(TempTopStr)
'    DMDUpdate 0

    TempBotStr = " > "
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

    TempBotStr = TempBotStr & " < "
'    dLine(1) = ExpandLine(TempBotStr)
'    DMDUpdate 1
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
	blinkentry = 1
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

'********************
' Real Time updates
'********************
'used for all the real time updates


Dim testlights(12,1)
testlights(0,0) = 0
testlights(1,0) = 1
testlights(2,0) = 2
testlights(3,0) = 3
testlights(4,0) = 4
testlights(5,0) = 5
testlights(6,0) = 6
testlights(7,0) = 7
testlights(8,0) = 8
testlights(9,0) = 9
testlights(10,0) = 10
Sub cLight(light,time,state)
	light.timerinterval = time
	light.timerenabled = True
	light.state = 2
End Sub
Sub cLightGarland_Timer
	cLightGarland.timerenabled = False
	cLightGarland.state = 0
End Sub
Sub cLightEndRRamp_Timer
	cLightEndRRamp.timerenabled = False
	cLightEndRRamp.state = 0
End Sub

Dim PlungerPulled : PlungerPulled = False
Dim PlungerPos : PlungerPos = 4
Dim testnextlight


Flasher022.height = 8
Flasher022.rotx = -47
Flasher022.x = 285
Flasher022.y = 680
Flasher023.height = 8
Flasher023.rotx = -47
Flasher023.x = 285+146
Flasher023.y = 675
dim jackturn : jackturn = 180
dim greenoogie
dim flasher2state : flasher2state = 0
dim flasher3state : flasher3state = 0
dim flasher4state : flasher4state = 0
dim flasher5state : flasher5state = 0

FrameTimer.interval = -1
Sub FrameTimer_Timer
	dim tmp, BL, testlight,x, tmp2,z

	Primitive156.roty = Spinner003.currentangle - 45

	zero2.blenddisablelighting = zerolight.GetInPlayIntensity * 5

	LFLogo.RotZ = LeftFlipper.CurrentAngle
	LFLogo1.RotZ = LeftFlipper.CurrentAngle
	RFlogo.RotZ = RightFlipper.CurrentAngle
	RFLogo1.RotZ = RightFlipper.CurrentAngle
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
	LFLogo001.RotZ = LeftFlipper001.CurrentAngle
	LFLogo002.RotZ = LeftFlipper001.CurrentAngle

' add any other real time update subs, like gates or diverters, flippers

	police2.blenddisablelighting = PoliceL22.GetInPlayIntensity
' inserts
	p003.blenddisablelighting = LightShootAgain.GetInPlayIntensity * 10  + mb

	dim mb
	mb = (( 50 - gi007.getinplayintensity ) / 120 )
	p17.blenddisablelighting = li001.GetInPlayIntensity * (2.2 + mb)	' bottom lanes
	p18.blenddisablelighting = li002.GetInPlayIntensity * (2.2 + mb)
	p19.blenddisablelighting = li003.GetInPlayIntensity * (2.2 + mb)
	p20.blenddisablelighting = li004.GetInPlayIntensity * (2.2 + mb)

	p28.blenddisablelighting = li012.GetInPlayIntensity * (1.5 + mb)	' pf multi/bonus multi
	p27.blenddisablelighting = li011.GetInPlayIntensity * (1.5 + mb)
	p26.blenddisablelighting = li010.GetInPlayIntensity * (1.5 + mb)
	p25.blenddisablelighting = li009.GetInPlayIntensity * (1.5 + mb)
	p24.blenddisablelighting = li005.GetInPlayIntensity * (1.5 + mb)
	p23.blenddisablelighting = li006.GetInPlayIntensity * (1.5 + mb)
	p22.blenddisablelighting = li007.GetInPlayIntensity * (1.5 + mb)
	p21.blenddisablelighting = li008.GetInPlayIntensity * (1.5 + mb)


	p35.blenddisablelighting = li013.GetInPlayIntensity * (2.2 + mb)	' BoOGie
	p36.blenddisablelighting = li018.GetInPlayIntensity * (2.2 + mb)
	p37.blenddisablelighting = li014.GetInPlayIntensity * (2.2 + mb)
	p38.blenddisablelighting = li015.GetInPlayIntensity * (2.2 + mb)
'	p39.blenddisablelighting = li016.GetInPlayIntensity * (2.2 + mb)
	p40.blenddisablelighting = li017.GetInPlayIntensity * (2.2 + mb)

	p62.blenddisablelighting = li023.GetInPlayIntensity * (2.2 + mb)	' RGB ARROWS
	p64.blenddisablelighting = li024.GetInPlayIntensity * (2.2 + mb)
	p65.blenddisablelighting = li025.GetInPlayIntensity * (2.2 + mb)
	p53.blenddisablelighting = li026.GetInPlayIntensity * (2.2 + mb)
	p66.blenddisablelighting = li027.GetInPlayIntensity * (2.2 + mb)
	p67.blenddisablelighting = li028.GetInPlayIntensity * (2.2 + mb)
	p68.blenddisablelighting = li029.GetInPlayIntensity * (2.2 + mb)
	p52.blenddisablelighting = li030.GetInPlayIntensity * (2.2 + mb)
	p59.blenddisablelighting = li031.GetInPlayIntensity * (2.2 + mb)
	p29.blenddisablelighting = li051.GetInPlayIntensity * (2   + mb)	' SCOOP Arrow   

	p31.blenddisablelighting = li032.GetInPlayIntensity * (2.2 + mb)		' gravestones
	p49.blenddisablelighting = li033.GetInPlayIntensity * (2.2 + mb)
	p50.blenddisablelighting = li034.GetInPlayIntensity * (2.2 + mb)
 	p51.blenddisablelighting = li035.GetInPlayIntensity * (2.2 + mb)
	p58.blenddisablelighting = li036.GetInPlayIntensity * (2.2 + mb)
	p57.blenddisablelighting = li037.GetInPlayIntensity * (2.2 + mb)

	p79.blenddisablelighting = li038.GetInPlayIntensity * (2.2 + mb)		' bumper red smal Insert GREEN NOW
								' 39 = ballsave1
								' 40 = ballsave2
	p48.blenddisablelighting = li041.GetInPlayIntensity * (2.2 + mb)		' light super Jackpot
	p60.blenddisablelighting = li042.GetInPlayIntensity * (2.2 + mb)		' super Jackpot
	p76.blenddisablelighting = li043.GetInPlayIntensity * (3.2 + mb)		' scoop

	p61.blenddisablelighting = li044.GetInPlayIntensity * (2.2 + mb)		' Superloop1
	p32.blenddisablelighting = li045.GetInPlayIntensity * (2.2 + mb)		' Superloop2
	p69.blenddisablelighting = li046.GetInPlayIntensity * (2.2 + mb)		' leftramp hurry
	p30.blenddisablelighting = li047.GetInPlayIntensity * (2.2 + mb)		' rigfhtramp hurry

	p001.blenddisablelighting= li048.GetInPlayIntensity * (18 + mb)  	'revive Left
	p002.blenddisablelighting= li049.GetInPlayIntensity * (18 + mb)  	'revive Right
	p33.blenddisablelighting = li050.GetInPlayIntensity * (2.2 + mb)		'start revive



	p70.blenddisablelighting = li052.GetInPlayIntensity * (2.2 + mb)	' eb light
	p74.blenddisablelighting = li053.GetInPlayIntensity * (2.2 + mb)	' bonus up


	p71.blenddisablelighting = li019.GetInPlayIntensity * (2.2 + mb)	' lock 1 2 3
	p72.blenddisablelighting = li020.GetInPlayIntensity * (2.2 + mb)
	p73.blenddisablelighting = li021.GetInPlayIntensity * (2.2 + mb)

	p63.blenddisablelighting = li063.GetInPlayIntensity	* (2.2 + mb)	' skillshots
	
	p34.blenddisablelighting = li062.GetInPlayIntensity * (2.2 + mb)
	Flasher024.opacity = jackslight.GetInPlayIntensity	' jackshouselithg



	tmp = F1B.GetInPlayIntensity ' building  from red top flasher
	Primitive139.blenddisablelighting = tmp / 400 + gi033.GetInPlayIntensity / 333
'	blinklights 40,40,2020,8,4,0
	z = houseclocklight.GetInPlayIntensity *2.4
	Primitive139.COLOR = RGB(255,255- z,255-z )
	Flasher019.opacity = z * 3

	Primitive140.blenddisablelighting = z / 123
	Primitive140.COLOR = RGB(255,255- z,255-z )

	Primitive141.blenddisablelighting = tmp / 123 + gi033.GetInPlayIntensity / 55' + ObjLevel(1)
	Primitive141.COLOR = RGB(255,255- tmp*2,255- tmp*2 )
	Primitive138.blenddisablelighting = tmp / 123 + gi033.GetInPlayIntensity / 55' + ObjLevel(1)
	Primitive138.COLOR = RGB(255,255- tmp*2,255- tmp*2 )
'	Primitive140.blenddisablelighting = tmp / 123 + gi033.GetInPlayIntensity / 111' + ObjLevel(1)
'	Primitive140.COLOR = RGB(255,255- tmp*2,255- tmp*2 )
	Primitive142.blenddisablelighting = tmp / 123 + gi033.GetInPlayIntensity / 55'+ ObjLevel(1)
	Primitive142.COLOR = RGB(255,255- tmp*2,255- tmp*2 )
	Primitive143.blenddisablelighting = tmp / 123 + gi033.GetInPlayIntensity / 55' + ObjLevel(1)
	Primitive143.COLOR = RGB(255,255- tmp*2,255- tmp*2 )
	Primitive144.blenddisablelighting = tmp / 123 + gi033.GetInPlayIntensity / 55 + ObjLevel(1)
	Primitive144.COLOR = RGB(255,255- tmp*2,255- tmp*2 )
'	Primitive145.blenddisablelighting = tmp / 123 + gi033.GetInPlayIntensity / 111' + ObjLevel(1)
'	Primitive146.COLOR = RGB(255,255- tmp*2,255- tmp*2 )
'	Primitive125.blenddisablelighting = tmp/222 + gi033.GetInPlayIntensity / 55 
'	Primitive125.COLOR = RGB(255,255- tmp*2,255- tmp*2 )
	Primitive123.blenddisablelighting = tmp/222 + gi033.GetInPlayIntensity / 55
	Primitive123.COLOR = RGB(255,255- tmp*2,255- tmp*2 )


	If f4.GetInPlayStateBool = true And flasher5state = 0 Then flasher5state = 1 : objlevel(5) = 1 : Flasherflash5_Timer
	If f4.getinplaystatebool = false Then flasher5state = 0
	If f5.GetInPlayStateBool = true And flasher4state = 0 Then flasher4state = 1 : objlevel(4) = 1 : Flasherflash4_Timer
	If f5.getinplaystatebool = false Then flasher4state = 0
	If f2b.GetInPlayStateBool = true And flasher2state = 0 Then flasher2state = 1 : objlevel(2) = 1 : Flasherflash2_Timer
	If f2b.getinplaystatebool = false Then flasher2state = 0
	If f1b.GetInPlayStateBool = true And flasher3state = 0 Then flasher3state = 1 : objlevel(3) = 1 : Flasherflash3_Timer
	If f1b.getinplaystatebool = false Then flasher3state = 0
	tmp = F2B.GetInPlayIntensity ' Rightflasher red
	tmp2 = gi033.GetInPlayIntensity / 123


	Primitive170.blenddisablelighting = tmp / 200 + tmp2
	Primitive170.COLOR = RGB(255,255- tmp*2.5,255- tmp*2.5)
	Jackshouse.blenddisablelighting = tmp / 40 + tmp2 * 5
	Jackshouse.COLOR = RGB(255,255- tmp,255- tmp )
	If tmp > 4 Then
		Primitive008.color = RGB(255, rl(11)/2, rl(12)/2)
	Else
		Primitive008.color = rgb( rl(10), rl(11) , rl(12) ) 
	End If
'primitive115.blenddisablelighting = 88
'primitive116.blenddisablelighting = 88
'Primitive067.blenddisablelighting = .5
'Primitive068.blenddisablelighting = .5

	x = light002.getinplayintensity
	Primitive068.blenddisablelighting = x/400
	primitive116.blenddisablelighting = x/2.2+ mb
	light003.intensity = x
	Flasher023.opacity = x / 4
'400max

	x = light004.getinplayintensity
	primitive115.blenddisablelighting = x/2.2 + mb
	Primitive067.blenddisablelighting = x/400
	light005.intensity = x
	Primitive170.blenddisablelighting = tmp / 300 + tmp2
	Flasher022.opacity = x / 4

	Flasher020.opacity = Light001.getinplayintensity
	For Each x in Collection001 
		x.blenddisablelighting = tmp2 / 2 + Flasher020.opacity/2777
		x.color = rgb (245 , 255 - flasher020.opacity / 3 , 255 - Flasher020.opacity / 1.5 )
	Next
	primitive017.blenddisablelighting = tmp2 / 2 + Flasher020.opacity/2777
	primitive017.color = rgb (245 , 255 - flasher020.opacity / 3 , 255 - Flasher020.opacity / 1.5 )

	Primitive293.color = rgb (245 , 255 - flasher020.opacity / 3 , 255 - Flasher020.opacity / 1.5 )
	Primitive293.blenddisablelighting =  tmp2 / 2 + Flasher020.opacity/1333
	Primitive186.blenddisablelighting =  Flasher020.opacity/5
' gion : boneramp027_hit
'240 190 90  flasher=200max  light56



	x = backwalllight.getinplayintensity * 8
	dim y
	For y = 0 to 8
		If TeenLight = y Then Teenkiller(y).opacity = x + mb Else Teenkiller(y).opacity = 0
	Next


	Primitive020.blenddisablelighting = (F3.GetInPlayIntensity / 5) ^2

	Primitive278.color = rgb (255,255- F5.GetInPlayIntensity *10,255- F5.GetInPlayIntensity * 10) 


	gibulbs001.blenddisablelighting = gi033.GetInPlayIntensity 
	gibulbs002.blenddisablelighting = gi033.GetInPlayIntensity 
	gibulbs003.blenddisablelighting = gi033.GetInPlayIntensity 
	gibulbs004.blenddisablelighting = gi033.GetInPlayIntensity 
	gibulbs006.blenddisablelighting = gi033.GetInPlayIntensity / 5
	gibulbs005.blenddisablelighting = gi033.GetInPlayIntensity / 7

	gibulbs001.color = gi033.colorfull
	gibulbs002.color = gi033.colorfull
	gibulbs003.color = gi033.colorfull
	gibulbs004.color = gi033.colorfull
	gibulbs005.color = gi033.colorfull
	gibulbs006.color = gi033.colorfull

	tmp = gi033.GetInPlayIntensity / 123 ' boneramps and walls
	x=150 + gi033.GetInPlayIntensity * 3
	wheel002.color = rgb(x,x,x)
	wheel001.color = rgb(x,x,x)
	wheel000.color = rgb(x,x,x)

	Primitive008.blenddisablelighting = tmp * 2  + ObjLevel(1) / 2

	Primitive171.blenddisablelighting = libumper.GetInPlayIntensity / 60 + tmp 
	Primitive280.blenddisablelighting = libumper.GetInPlayIntensity * 10

	flasher008.opacity = libumper.GetInPlayIntensity /1.5
	Primitive171.z = 65 + libumper.GetInPlayIntensity / 7
	Primitive280.z = 65 + libumper.GetInPlayIntensity / 7

	Wall21.blenddisablelighting = tmp 
	bumperbase1.blenddisablelighting = tmp*5 + 0.2
	bumperdisk1.blenddisablelighting = tmp*5 
		For Each BL in pegplastic : BL.blenddisablelighting = tmp / 3 : Next
	
	If tmp > 0.02 Then  ' wireramps + slings
		Spinner003.material = "plastic"
		RightSling001.material = "Rubber White"
		RightSling002.material = "Rubber White"
		RightSling003.material = "Rubber White"
		RightSling004.material = "Rubber White"
		LeftSling001.material = "Rubber White"
		LeftSling002.material = "Rubber White"
		LeftSling003.material = "Rubber White"
		LeftSling004.material = "Rubber White"
		For Each BL in Wireramps  : BL.material = "Metal Dark3" : Next
	Else
		Spinner003.material = "plastic2"
		RightSling001.material = "Rubber White1"
		RightSling002.material = "Rubber White1"
		RightSling003.material = "Rubber White1"
		RightSling004.material = "Rubber White1"
		LeftSling001.material = "Rubber White1"
		LeftSling002.material = "Rubber White1"
		LeftSling003.material = "Rubber White1"
		LeftSling004.material = "Rubber White1"
		For Each BL in Wireramps  : BL.material = "Metal Dark2" : Next
	End If


'	Light001.intensity =  tmp * 3						' overlight
'	Primitive179.blenddisablelighting = tmp	+ 0.1			' Wirescmaslights

'	If testlights(11,0) > 12 And int(rnd(1) * 100) > 97 Then testlights(11,1) = 194 : testlights(11,0) = 0 
'	If testlights(12,0) > 12 And int(rnd(1) * 100) > 94 Then testlights(12,1) = 195 : testlights(12,0) = 0 

	If RightRampLight.state = 2 Then
		tmp2 = RightRampLight.GetInPlayIntensity
		For Each BL In Collection004 : BL.blenddisablelighting = tmp2 : Next
	End If
	garlandlights.blenddisablelighting = cLightGarland.GetInPlayIntensity 

'	Primitive395.blenddisablelighting = cLightEndRRamp.GetInPlayIntensity 


'	For Each BL in Collection002 : BL.blenddisablelighting = tmp * 55 + 0.1 : Next		' some walls
	Primitive119.blenddisablelighting = tmp * 6 + 0.1	'Walls
	Primitive185.blenddisablelighting = tmp * 11 + 0.1	'Walls
	Primitive184.blenddisablelighting = tmp * 3 + 0.1	'Walls
	Primitive120.blenddisablelighting = tmp * 20 + 0.1	'apron Wall
	Primitive179.blenddisablelighting = tmp * 33 + 0.5 'blades
	Primitive181.blenddisablelighting = tmp * 33 + 0.5'blades
	Primitive182.blenddisablelighting = tmp * 3 	 ' plastics
	Primitive125.blenddisablelighting = tmp * 3 	 ' plastic green part

	primitive183.blenddisablelighting = tmp * 3 	' glassunderplastic
	Primitive121.blenddisablelighting = tmp * 7 + 0.3	' apron top

		Primitive119.color = rgb( (252 + rl(10))/2 , (226 + rl(11))/2 , (166 + rl(12))/2 )
		Primitive120.color = rgb( (252 + rl(10))/2 , (226 + rl(11))/2 , (166 + rl(12))/2 )
		Primitive189.color = rgb( (252 + rl(10))/2 , (252 + rl(11))/2 , (252 + rl(12))/2 ) 
		Primitive147.color = rgb( (186 + rl(10))/2 , (165 + rl(11))/2 , (135 + rl(12))/2 ) 
		Primitive052.color = rgb( (186 + rl(10))/2 , (165 + rl(11))/2 , (135 + rl(12))/2 )


	tmp2 = tmp/2

	Primitive189.blenddisablelighting = tmp2

	Primitive150.transz = -77 + Plunger.Position * 4 + ShakeStuff1
	Primitive148.transz = -77 + Plunger.Position * 4 + ShakeStuff1
	Primitive187.transz = -77 + Plunger.Position * 4 + ShakeStuff1
	Primitive187.transy = ShakeStuff1 + ShakeStuff3 / 2
	If shakestuff1 + Shakestuff3 > 0 and Plunger.Position < 5 then
		light006.state = 1
		flasher021.visible = 1
	Else
		light006.state = 0
		flasher021.visible = 0
	End If
	Primitive187.transz = -77 + Plunger.Position * 4 + ShakeStuff1

	Primitive149.transz = -77 + Plunger.Position * 4 + ShakeStuff1
	Primitive146.transz = -77 + Plunger.Position * 4 + ShakeStuff1
	Primitive012.transz = -77 + Plunger.Position * 4 + ShakeStuff1
	Primitive155.transy = PoliceL22.getinplayintensity /10		' spinnerman
	Primitive156.transy = PoliceL22.getinplayintensity /10
	Primitive157.transy = PoliceL22.getinplayintensity /10
	Primitive150.blenddisablelighting = tmp
	Primitive150.COLOR = RGB(255,255- libumper.GetInPlayIntensity * 3,255- libumper.GetInPlayIntensity*10 )
'	Light001.intensity = libumper.GetInPlayIntensity*2
	Primitive148.blenddisablelighting = tmp
	Primitive149.blenddisablelighting = tmp
	Primitive146.blenddisablelighting = tmp
	Primitive012.blenddisablelighting = tmp


	Primitive132.blenddisablelighting = tmp
	Primitive131.blenddisablelighting = tmp
	Primitive130.blenddisablelighting = tmp
	Primitive129.blenddisablelighting = tmp
	Primitive128.blenddisablelighting = tmp

	Primitive133.blenddisablelighting = tmp * 3 + ObjLevel(1) / 4 + f3.GetInPlayIntensity / 170
	Primitive133.color = RGB(255 , 255 - f3.GetInPlayIntensity * 4, 255 - f3.GetInPlayIntensity * 4 )

	Primitive122.blenddisablelighting = tmp2 * 3 + f3.GetInPlayIntensity / 66
	Primitive122.color = RGB(255 , 255 - f3.GetInPlayIntensity * 8, 255 - f3.GetInPlayIntensity * 8 )
	LFLogo001.blenddisablelighting = tmp
	LFLogo002.blenddisablelighting = tmp + 0.1
	LFLogo.blenddisablelighting = tmp * 2
	RFLogo.blenddisablelighting = tmp * 2
	RFLogo1.blenddisablelighting = tmp * 2 + 0.1
	LFLogo1.blenddisablelighting = tmp * 2 + 0.1
	Primitive160.blenddisablelighting = tmp2
	Primitive169.blenddisablelighting = f4.getinplayintensity / 80 + tmp2/3
	Primitive160.transy = f4.getinplayintensity / 9
	Primitive169.roty = 33 + f4.getinplayintensity / 9


	Primitive151.blenddisablelighting = tmp2
	Primitive164.blenddisablelighting = tmp2
	Primitive162.blenddisablelighting = tmp2
	Primitive158.blenddisablelighting = tmp2
	Primitive178.blenddisablelighting = tmp2

	Primitive018.blenddisablelighting = tmp2


	Primitive089.blenddisablelighting = tmp2
	Primitive155.blenddisablelighting = tmp2
	Primitive157.blenddisablelighting = tmp2
	Primitive156.blenddisablelighting = tmp2

	Primitive172.blenddisablelighting = tmp2
'	Primitive176.blenddisablelighting = tmp2
'	Primitive174.blenddisablelighting = tmp2
'	Primitive175.blenddisablelighting = tmp2
	Primitive081.blenddisablelighting = tmp2
	Primitive052.blenddisablelighting = tmp2

	If greenoogie = True Then
		Primitive154.material = "Oogiegreen"
		Primitive154.blenddisablelighting = tmp2*15 + ObjLevel(1)	' oogie
		If primitive154.z < 125 Then
			primitive154.z = primitive154.z + 0.05
		End If
	Else

		Primitive154.material = "Plastic"
		Primitive154.blenddisablelighting = tmp2*2 + ObjLevel(1)/2	' oogie
		If primitive154.z > 105 Then
			primitive154.z = primitive154.z - 0.05
		End If
	End If


	Primitive126.blenddisablelighting = tmp2
	Primitive128.blenddisablelighting = tmp2
	Primitive165.blenddisablelighting = tmp2'
'	Primitive166.blenddisablelighting = tmp2
'	Primitive167.blenddisablelighting = tmp2

	For Each BL in pegs : BL.blenddisablelighting = tmp /2 : Next '/2

End Sub



poff63.blenddisablelighting = 1.5
poff64.blenddisablelighting = 2
poff65.blenddisablelighting = 2
poff53.blenddisablelighting = 2
poff66.blenddisablelighting = 2
poff68.blenddisablelighting = 1.5
poff60.blenddisablelighting = 1.5
poff61.blenddisablelighting = 1.5
poff52.blenddisablelighting = 2
poff32.blenddisablelighting = 1.5
poff59.blenddisablelighting = 2
poff34.blenddisablelighting = 1.5
poff39.blenddisablelighting = 1.5
poff62.blenddisablelighting = 2
poff67.blenddisablelighting = 2
poff68.blenddisablelighting = 2

poff24.blenddisablelighting = 1
poff23.blenddisablelighting = 1
poff22.blenddisablelighting = 1
poff21.blenddisablelighting = 1
poff25.blenddisablelighting = 1
poff26.blenddisablelighting = 1
poff27.blenddisablelighting = 1
poff28.blenddisablelighting = 1
poff49.blenddisablelighting = 1.5
poff48.blenddisablelighting = 1.5
poff39.blenddisablelighting = 1.5

poff73.blenddisablelighting = 1
poff72.blenddisablelighting = 1
poff71.blenddisablelighting = 1
poff49.blenddisablelighting = 2
poff50.blenddisablelighting = 2
poff51.blenddisablelighting = 2
poff57.blenddisablelighting = 2
poff58.blenddisablelighting = 2
poff31.blenddisablelighting = 2
poff79.blenddisablelighting = 2
poff74.blenddisablelighting = 2
poff30.blenddisablelighting = 2
poff69.blenddisablelighting = .25
poff70.blenddisablelighting = .25
poff76.blenddisablelighting = 1.5
poff29.blenddisablelighting = 1.5
poff001.blenddisablelighting = 1
poff002.blenddisablelighting = 1

Dim Follower : Follower = False
Sub MoveFollower
	Primitive168.x = Followball.x - Followball.velx * 8
	Primitive168.y = Followball.y - 100
	Primitive168.roty = - followball.velx * 3
	light007.x = Followball.x - Followball.velx * 8
	light007.y = Followball.y - 79 

	zero2.x = Followball.x - Followball.velx * 8
	zero2.y = Followball.y - 100
	zero2.roty = - followball.velx * 3 

	If Primitive168.y > 1570 Then
		Set Followball = nothing : Primitive168.visible = 0 : zero2.visible = False : light007.state = 0
		Follower = False
	End If
End Sub

Dim Followball
Dim Bonearray
Bonearray = array(primitive111,primitive112,primitive113,primitive114,primitive275,primitive279,primitive019,primitive023,primitive024,primitive027,primitive026,primitive025,primitive054,primitive053 ,primitive051 ,primitive063 ,primitive062 ,primitive061,primitive060 ,primitive059 ,primitive058 ,primitive057 ,primitive056 ,primitive055 ,primitive281 ,primitive282,primitive283 ,primitive284 ,primitive285 ,primitive286 ,primitive287,primitive288,primitive289,primitive290,primitive291,primitive292)
Dim BonesDown(6)
BonesDown(0) = -1
BonesDown(1) = -1
BonesDown(2) = -1
BonesDown(3) = -1
BonesDown(4) = -1
BonesDown(5) = -1
BonesDown(6) = -1

Sub Boneramp005_hit
	activeball.vely = 1.5
	If Not Follower Then SET followball = activeball : Follower = True : Primitive168.x = 558 : Primitive168.y=626 : Primitive168.z = 0 : Primitive168.visible = true : zero2.x = 558 : zero2.y = 626 : zero2.z = 0 : zero2.visible = True : light007.state = 2
	Playsound "sfx_zero",1,BackboxVolume
End Sub
Sub Boneramp006_Hit : activeball.vely = 1.5 : BonesDown(1)= 0 : BonesDown(2)=1 : BonesDown(3)=2 : BonesDown(4)=3 : BonesDown(5)=4 : End Sub
Sub Boneramp007_Hit : BonesDown(0)= 5 : End Sub
Sub Boneramp008_Hit : BonesDown(1)= 6 : End Sub
Sub Boneramp009_Hit : BonesDown(2)= 7 : End Sub
Sub Boneramp010_Hit : BonesDown(3)= 8 : End Sub
Sub Boneramp011_Hit : BonesDown(4)= 9 : End Sub
Sub Boneramp012_Hit : BonesDown(5)=10 : End Sub
Sub Boneramp013_Hit : BonesDown(0)=11 : End Sub
Sub Boneramp014_Hit : BonesDown(1)=12 : End Sub
Sub Boneramp015_Hit : BonesDown(2)=13 : End Sub
Sub Boneramp016_Hit : BonesDown(3)=14 : End Sub
Sub Boneramp017_Hit : BonesDown(4)=15 : End Sub
Sub Boneramp018_Hit : BonesDown(5)=16 : End Sub
Sub Boneramp019_Hit : BonesDown(0)=17 : End Sub
Sub Boneramp020_Hit : BonesDown(1)=18 : End Sub
Sub Boneramp021_Hit : BonesDown(2)=19 : End Sub
Sub Boneramp022_Hit : BonesDown(3)=20 : End Sub
Sub Boneramp023_Hit : BonesDown(4)=21 : End Sub
Sub Boneramp024_Hit : BonesDown(5)=22 : End Sub
Sub Boneramp025_Hit : BonesDown(0)=23 : End Sub
Sub Boneramp026_Hit : BonesDown(1)=24 : End Sub
Sub Boneramp027_Hit : BonesDown(2)=25 : blinklights 56,56,5,4,1,0 : End Sub
Sub Boneramp028_Hit : BonesDown(3)=26 : End Sub
Sub Boneramp029_Hit : BonesDown(4)=27 : End Sub
Sub Boneramp030_Hit : BonesDown(5)=28 : End Sub
Sub Boneramp031_Hit : BonesDown(0)=29 : End Sub
Sub Boneramp032_Hit : BonesDown(1)=30 : End Sub
Sub Boneramp033_Hit : BonesDown(2)=31 : End Sub
Sub Boneramp034_Hit : BonesDown(3)=32 : End Sub
Sub Boneramp035_Hit : BonesDown(4)=33 : End Sub
Sub Boneramp036_Hit : BonesDown(5)=34 : TipLastOne = 1 : End Sub
Sub Boneramp037_Hit : BonesDown(0)=35 : End Sub
Sub Boneramp038_Hit : BonesDown(1)=-1 : End Sub
Sub Boneramp039_Hit : BonesDown(2)=-1 : End Sub
Sub Boneramp040_Hit : BonesDown(0)=-1 : BonesDown(3)=-1 : BonesDown(4)=-1 : BonesDown(5)=-1 : StopSound"_BoneFake" : bonewind : End Sub

Sub RightRampAccelerate_Hit
	ActiveBall.VelY = ActiveBall.VelY * 1.15
	If activeball.vely > 0 then PlaySound"",1,BackBoxVolume * 0.1 Else Playsound"sfx_gate",1,BackBoxVolume  
End Sub

Sub BoneRampStart_Hit
    DOF 251, DOFPulse
	PlaySoundAt("sfx_electro7"), BoneRampStart
	Activeball.VelX = Activeball.VelX * 1.2
	Activeball.VelY = Activeball.VelY * 1.2
'	Activeball.VelZ = 0
'	Activeball.X = 176
'	ActiveBall.Y = 178
'	ActiveBall.Z = 122
End Sub

Sub HouseStart_Hit
    DOF 255, DOFPulse
	If DMD_dog_Dir = 1 Then
		DMD_DoggieLeft = 1	: DMD_dog_Dir = 0
	Else
		DMD_DoggieRight = 1	: DMD_dog_Dir = 1
	End If

'	Activeball.VelX = 0
'	Activeball.VelY = 0
'	Activeball.VelZ = 0
'	Activeball.X = 458
'	ActiveBall.Y = 536
'	ActiveBall.Z = 116
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

Dim rl(12)


Sub ChangeGi(col)
	If col = white Then
		If bJackMBStarted					Then col = orange
		If WizardMode(CurrentPlayer) = 1	Then col = amber
		If WizardMode(CurrentPlayer) = 2	Then col = red
	End If
    Select Case col
        Case red		: rl(1) = 18	: rl(2) = 0		: rl(3) = 0		: rl(4) = 255	: rl(5) = 0		: rl(6) = 0
        Case orange 	: rl(1) = 18	: rl(2) = 3		: rl(3) = 0		: rl(4) = 255	: rl(5) = 64	: rl(6) = 0
        Case amber		: rl(1) = 193	: rl(2) = 49	: rl(3) = 0		: rl(4) = 255	: rl(5) = 153	: rl(6) = 0
        Case yellow		: rl(1) = 18	: rl(2) = 18	: rl(3) = 0		: rl(4) = 255	: rl(5) = 255	: rl(6) = 0
        Case darkgreen	: rl(1) = 0		: rl(2) = 8		: rl(3) = 0		: rl(4) = 0		: rl(5) = 64	: rl(6) = 0
        Case green		: rl(1) = 0		: rl(2) = 16	: rl(3) = 0		: rl(4) = 0		: rl(5) = 128	: rl(6) = 0
        Case blue		: rl(1) = 0		: rl(2) = 18	: rl(3) = 18	: rl(4) = 0		: rl(5) = 77	: rl(6) = 255
        Case darkblue	: rl(1) = 0		: rl(2) = 8		: rl(3) = 8		: rl(4) = 0		: rl(5) = 64	: rl(6) = 64
        Case purple		: rl(1) = 64	: rl(2) = 0		: rl(3) = 96	: rl(4) = 128	: rl(5) = 0		: rl(6) = 192
        Case white		: rl(1) = 193	: rl(2) = 91	: rl(3) = 0		: rl(4) = 255	: rl(5) = 197	: rl(6) = 143
        Case teal		: rl(1) = 1		: rl(2) = 64	: rl(3) = 62	: rl(4) = 2		: rl(5) = 128	: rl(6) = 126

    End Select
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
            n.color = RGB(155, 22, 255)
        Case white
            n.color = RGB(255, 197, 143)
        Case teal
            n.color = RGB(2, 128, 126)
    End Select
    If stat <> -1 Then
        n.Visible = stat
    End If
End Sub


' ********************************
'   Table info & Attract Mode
' ********************************



Sub StartAttractMode
    DOF 250, DOFOn
	ChangeGi white
    bGameInPLay = False
	For x = 1 to 17 : ArrowLights(x,1) = 2 : Next
	bAttractMode = True
    StartLightSeq
    StopSong "mu_gameover"
    PlaySong "mu_gameover"
End Sub

Sub StopAttractMode
    DOF 250, DOFOff
	bAttractMode = False
	For x = 1 to 17 : ArrowLights(x,1) = 0 : Next
    LightSeqAttract.StopPlay
	StopSong "mu_gameover"
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
	UpdatePresentsLights
End Sub

Sub LightSeqTopFlashers_PlayDone()
    FlashEffect 7
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************



' tables variables and Mode init
Dim bRotateLights
Dim Mode(4, 15) 'the first 4 is the current player, contains status of the Mode, 0 not started, 1 won, 2 started
Dim Weapons(4)  ' collected weapons
Dim LoopCount(4)
Dim SpinnerSpins
Dim LoopHits(4)
Dim LoopValue(4)
Dim SlingCount 'used for the db2s animation
Dim ComboCount(4)
Dim ComboHits(4)
Dim ComboValue(4)
Dim Mystery(4, 4) 'inlane lights for each player
Dim BumperHits(4)
Dim BumperNeededHits(4)
Dim TargetHits(4, 7) '6 targets + the bumper -the blue lights

Dim aWeaponSJactive
Dim WeaponSJValue(4)
Dim bFlippersEnabled
Dim bTommyStarted
Dim TommyCount 'counts the seconds left
Dim TommyValue
'
Dim CenterSpinnerHits(4)
Dim LeftSpinnerHits(4)
Dim TargetJackpot(4)
Dim bJackMBStarted
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
Dim ModeRunning(4)
Dim NextMode(4)
Dim WeaponHits(4, 6) '6 lights, lanes and the magnet post  boogie
Dim HurryupNeededDog(4)
Dim HurryupNeededDXmas(4)
Dim JackResetNeeded(4)
Dim JackResetCounter(4) 
Dim JackResetAvail(4) 
Dim Delivered_mission(4) 
Dim Extraball_one(4)
Dim Extraball_two(4)
Dim Extraball_3(4)
Dim MysteryCollected(4)
Dim MegaSpinner_started(4)
Dim ExtraballsGotten(4)
Dim WizardMode(4)
Dim WizardsDone(4)
Dim MegaspinnerLevel(4)
Dim Addrevive(4)
Dim Addaball1(4)
Dim Addaball2(4)
Dim Addaball3(4)
Dim Addaball4(4)
Dim Presents(24,4)


Sub Game_Init() 'called at the start of a new game
    Dim i
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
	currentplayer = 1
    bFlippersEnabled = True 'only disabled if the police or Tommy catches you

	For i = 0 to 4 
		resetplayer i
		Extraball_one(i) = 0			' things you only reset at first start not end of wizard
		Extraball_two(i) = 0
		Extraball_3(i) = 0
		ExtraballsGotten(i) = 0

	Next
End Sub

Sub resetplayer(i)
	dim j
    'Init Variables
    bRotateLights = True
    aWeaponSJactive = False
    bTommyStarted = False
    TommyCount = 1          'we set it to 1 because it also acts as a multiplier in the hurry up
    TommyValue = 500000
    bJackMBStarted = False
    NewMode = 0
    SpinCount = 0
    ReadyToKill = False
    SpinNeeded = 0
    EndModeCountdown = 0
    BlueTargetsCount = 0
    ArrowsCount = 0
	ResetPresentsLights
	UpdatePresentsLights

	For j = 1 to 17
		ArrowLights(j,1) = 0
		ArrowLights(j,2) = 0
		ArrowLights(j,3) = 0
		ArrowLights(j,4) = 0
	Next
	For x = 11 to 16
		InsertColors(x,3)  = 180	' red targetlights
		InsertColors(x,4)  = 22
		InsertColors(x,5)  = 11
	Next
	InsertColors(17,3)  = 11 ' green bumperlight
	InsertColors(17,4)  = 233
	InsertColors(17,5)  = 33
 
		
'    For i = 0 to 4
		ModeRunning(i) = 0


		HurryupNeededDog(i) = 3 ' +1 FOR EACH START - MAX IS 8
		HurryupNeededDXmas(i) = 3
		JackResetNeeded(i) = 2 ' max 6
		JackResetCounter(i) = 10 ' turn off = more than 6
		JackResetAvail(i) = False ' 1 when mb is done
		Delivered_mission(i) = 0  
		ModeRunning(i) = 0
		SkillShotValue(i) = Score_skillshot
        SuperSkillShotValue(i) = Score_SuperSS

		MysteryCollected(i) = 0
		MegaSpinner_started(i) = 0

		WizardMode(i) = 0
		WizardsDone(i) = 0
		MegaspinnerLevel(i) = 0
		LoopCount(i) = 0
		ComboCount(i) = 0
		Addrevive(i) = 0
		Addaball1(i) = 0
		Addaball2(i) = 0
		Addaball3(i) = 0
		Addaball4(i) = 0
		for x = 0 to 24
			Presents(x,i) = 0
		Next


        LoopValue(i) = 500000
        ComboValue(i) = 500000
        BumperHits(i) = 0
        BumperNeededHits(i) = 10
        Weapons(i) = 0
        WeaponSJValue(i) = 3500000
        LoopHits(i) = 0
        ComboHits(i) = 0
        CenterSpinnerHits(i) = 0
        LeftSpinnerHits(i) = 0
        TargetJackpot(i) = 500000
        Jackpot(i) = 500000 'only used in the last mode
        BallsInLock(i) = 0
        ArrowMultiPlier(i) = 1
'    Next
 '   For i = 0 to 4
        For j = 0 to 4
            Mystery(i, j) = 0
        Next
'    Next
'    For i = 0 to 4
        For j = 0 to 15
            Mode(i, j) = 0
        Next
'    Next
'    For i = 0 to 4
        For j = 0 to 7
            TargetHits(i, j) = 1
        Next
 '   Next
 '   For i = 0 to 4
        For j = 0 to 6
            WeaponHits(i, j) = 1
        Next
 '   Next
	SpinnerSpins = 0 ' for eob
End Sub



Sub StopMBmodes 'stop multiball modes after loosing the last multibal
    If bJackMBStarted Then StopJackMultiball : PlaySound"vo_hahahahaaa",1,BackboxVolume : PlaySound "sfx_churchbell" ,1,BackboxVolume: ChangeSong
	If WizardMode(CurrentPlayer) = 1 Then StopWizard : PlaySound "sfx_churchbell",1,BackboxVolume : ChangeSong 
End Sub


Sub ResetNewBallVariables() 'reset variables and lights for a new ball or player
    'turn on or off the needed lights before a new ball is released
    TurnOffPlayfieldLights
    Flasher002.Visible = 0
    'set up the lights according to the player achievments
    BonusMultiplier(CurrentPlayer) = 1 'no need to update light as the 1x light do not exists
    UpdateTargetLights
    UpdateWeaponLights                 ' the W lights
    aWeaponSJactive = False
    UpdateLockLights                   ' turn on the lock lights for the current player
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For a = 0 to 154
		Blink(a,1) = 0
    Next
End Sub



Sub TurnOffBlueTargets 'Turns off all blue targets at the start of a mode that uses the blue targets
	ArrowLights(11,1) = 0
	ArrowLights(12,1) = 0
	ArrowLights(13,1) = 0
	ArrowLights(14,1) = 0
	ArrowLights(15,1) = 0
	ArrowLights(16,1) = 0

'		Blink(32 , 1) = 0
'		Blink(33 , 1) = 0
'		Blink(34 , 1) = 0
'		Blink(35 , 1) = 0
'		Blink(36 , 1) = 0
'		Blink(37 , 1) = 0
End Sub

Sub ResetTargetLights 'CurrentPlayer
    Dim j
    For j = 0 to 6
        TargetHits(CurrentPlayer, j) = 2
    Next
    UpdateTargetLights
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
	NewPresentsNextMode
	LS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotLeft Lemk
    DOF 105, DOFPulse
	LeftSling004.Visible = 1
	Lemk.RotX = 26
	LStep = 0
	LeftSlingShot.TimerEnabled = True

	AddScore score_slingshots	' add some points


	'check modes
	' add some effect to the table?
	If B2sOn then
		SlingCount = 0
		SlingTimer.Enabled = 1
	End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
	LeftSlingSounds
End Sub

Sub LeftSlingSounds
    PlaySound "sfx_electro" &RndInt(1,9),1,BackboxVolume/2 'idig
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
	NewPresentsNextMode
	RS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotRight Remk
    DOF 106, DOFPulse
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore score_slingshots
    ' check modes
    ' add some effect to the table?
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
	RightSlingSounds
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






'table lanes
Dim block012
Sub Trigger005_Hit
    DOF 256, DOFPulse
    PLaySoundAt "fx_sensor", Trigger005
	If activeball.vely > 0 then PlaySound"_SqueakDwn",1,BackBoxVolume * 0.2 Else Playsound"_SqueakUp",1,BackBoxVolume * 0.2
    If Tilted Then Exit Sub
    Addscore score_triggerhits
	block012 = DMD_Frame + 50
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "trigger005"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "trigger005" : Exit Sub

End Sub




Sub Trigger008_Hit 'end top loop
    DOF 257, DOFPulse
    PLaySoundAt "fx_sensor", Trigger008

    If Tilted Then Exit Sub
    DOF 206, DOFPulse

	Addscore score_triggerhits
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "trigger008"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "trigger008" : Exit Sub
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger008"
End Sub

Sub Trigger009_Timer
	Trigger009.timerenabled = False
End Sub

Dim combocombo : combocombo = 0

Sub Trigger009_Hit 'right loop
    DOF 258, DOFPulse
    PLaySoundAt "fx_sensor", Trigger009
    If Tilted Then Exit Sub
	If activeball.velx < 0 Then
		DOF 206, DOFPulse
		If bJackMBStarted Then JasonHit8
		PlayThunder
		Addscore score_triggerhits
'		Flashforms f2A, 800, 100, 0
		Flashforms F2B, 800, 100, 0
		Flashforms Flasher004, 800, 50, 0
		Flashforms Flasher005, 800, 50, 0

		updatemodecombolights "outerloop"
		If Combotimer > DMD_Frame Then
			ComboLasthit = ""
			AwardComboShot "outerloop"
		Else
			RestartComboTimer 
		End If   ' loop can repeat combos ( only this one )
		If WizardMode(CurrentPlayer) = 1 Then Wizardhit "outerloop"
		If WizardMode(CurrentPlayer) = 2 Then Wizardhit "outerloop" : Exit Sub
		If ArrowLights(8,3) = 2 Then ModeHit "outerloop"
		Superloop1



		If Trigger009.timerenabled = True Then 
			combocombo = combocombo + 1 
			If combocombo > 10 Then combocombo = 10
			DMD_StartSplash "MEGA LOOP",FormatScore(score_megaloop * combocombo),FontHugeOrange, FontHugeOrange,70 , "oneimage" ,55,0,0			' 2 rounds megaloop
			AddScore score_megaloop * combocombo
			PlaySound "vo_moonatnight",1,BackBoxVolume * 0.8

		Else
			combocombo = 0
		End If
		Trigger009.timerenabled = False
		If BallsOnPlayfield < 2 Then Trigger009.timerenabled = True



	End If

    LastSwitchHit = "Trigger009"

End Sub

Sub Trigger012_hit ' super down
    PLaySoundAt "fx_sensor", Trigger012
	If Tilted Or activeball.vely <= 0 Then Exit Sub
	If block012 > DMD_Frame Or Tilted Then Exit Sub

	If Combotimer > DMD_Frame Then AwardComboShot "loopdown" Else  : RestartComboTimer
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "loopdown"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "loopdown" : Exit Sub

	Superloop2
	updatemodecombolights "loopdown"

    LastSwitchHit = "Trigger012"
	If activeball.vely>0 then PlaySfx"sfx_bats"
End Sub


'****************************
' extra triggers - no sound
'****************************




Sub Trigger013_Hit 'behind right spinner
    DOF 259, DOFPulse
    PLaySoundAt "fx_sensor", Trigger013
    If Tilted or activeball.vely > 0 Then Exit Sub
    DOF 206, DOFPulse
	Addscore score_triggerhits
    FlashForMs F4, 1000, 75, 0
    FlashForMs Flasher003, 1000, 75, 0

	If Combotimer > DMD_Frame Then AwardComboShot "rightspinner" Else RestartComboTimer

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "rightspinner" 
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "rightspinner" : Exit Sub

	Boogiehit6	'weapon hit
	Playsound"vo_alllaughing",1,BackboxVolume
	If bJackMBStarted Then JasonHit9
	If ArrowLights(9,3) = 2 Then ModeHit "rightspinner"
	updatemodecombolights "rightspinner"


    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If



 
	LastSwitchHit = "Trigger013"
End Sub

Sub Trigger014_Hit 'center spinner for loop awards - W4
    DOF 260, DOFPulse
    PLaySoundAt "fx_sensor", Trigger014
    If Tilted or activeball.vely > 0 Then Exit Sub
     DOF 206, DOFPulse
   FlashForMs F3, 1000, 75, 0
    FlashForMs F3A, 1000, 75, 0
	Addscore score_triggerhits
	If bMultiBallMode = false Then gieffect 3
    If blink(55,1) = 2 Then
        AddBonusMultiplier
        CenterSpinnerHits(CurrentPlayer) = 0
		blink(55,1) = 0
    End If
	If Combotimer > DMD_Frame Then AwardComboShot "middlespinner" Else RestartComboTimer
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "middlespinner"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "middlespinner" : Exit Sub

	Boogiehit4	    'weapon hit
	updatemodecombolights "middlespinner" :

	If ArrowLights(6,3) = 2 Then ModeHit "middlespinner"
    If bJackMBStarted Then JasonHit6



    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If


 
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger014"
End Sub

Sub Trigger010_Hit
    PLaySoundAt "fx_sensor", Trigger010
	RightRampLight.state = 0
	clight cLightEndRRamp,1500,2
	Addscore score_triggerhits
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "trigger010"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "trigger010" : Exit Sub
End Sub


Sub Trigger015_Hit 'right ramp done - W5
	pupevent 902

    DOF 261, DOFPulse
    PLaySoundAt "fx_sensor", Trigger015
    If Tilted Then Exit Sub
'	Flashforms f2A, 800, 100, 0
	Flashforms F2B, 800, 100, 0
	Flashforms Flasher004, 800, 50, 0
	Flashforms Flasher005, 800, 50, 0
	blinklights 54,54,1,12,2,0
	updatemodecombolights "rightramp" 
	If Combotimer > DMD_Frame Then AwardComboShot "rightramp" Else RestartComboTimer : Combotimer = DMD_Frame + 400 ' extra for ramps
	gieffect 4
	LightEffect 6
	BlinkAllPresents 3,0
	Addscore score_triggerhits
	AddScore score_ramphit
 	RightRampLight.state = 2
	clight cLightEndRRamp,330,2
	
	DMD_xmas = 1
	NightmareShowCounter = 1

	LoopCount(CurrentPlayer) = LoopCount(CurrentPlayer) + 1 ' for EOB
	Addscore score_triggerhits

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "rightramp"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "rightramp" : Exit Sub

	Boogiehit5	'weapon hit
    If bJackMBStarted Then JasonHit7
	
	If ArrowLights(7,3) = 2 Then ModeHit "rightramp"
	HurryupXmas
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger015"
End Sub

Sub Trigger016_Hit 'left ramp done - W2
	pupevent 903
    DOF 262, DOFPulse
    PLaySoundAt "fx_sensor", Trigger016
	activeball.velx = 2 : activeball.vely = 6
    If Tilted Then Exit Sub
    DOF 206, DOFPulse
	gieffect 4
	LightEffect 6
	BlinkAllPresents 3,0
	blinklights 53,53,1,12,2,0
'    Flashforms f1A, 800, 50, 0
    Flashforms F1B, 800, 50, 0
    Flashforms Flasher006, 800, 50, 0
    Flashforms Flasher007, 800, 50, 0
	activeball.velx = 6 : activeball.vely = 3
	Addscore score_triggerhits
	AddScore score_ramphit
	LoopCount(CurrentPlayer) = LoopCount(CurrentPlayer) + 1 ' for EOB
	If Combotimer > DMD_Frame Then AwardComboShot "leftramp" Else  RestartComboTimer : Combotimer = DMD_Frame + 400 ' extra for ramps

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "leftramp"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "leftramp" : Exit Sub

	Boogiehit2    'weapon hit
	If bJackMBStarted Then JasonHit2	
	
	If ArrowLights(2,3) = 2 Then ModeHit "leftramp"


	updatemodecombolights "leftramp"
	HurryupDogChase


    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger016"
End Sub

Sub Trigger017_Hit 'cabin top
    DOF 263, DOFPulse
    PLaySoundAt "fx_sensor", Trigger017
    If Tilted Or activeball.vely > 0 Then Exit Sub
	Shakehousefront 10
    DOF 204, DOFPulse
	Addscore score_triggerhits
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "trigger017"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "trigger017" : Exit Sub
	If BallsInLock(CurrentPlayer) < 1 or BallsInLock(CurrentPlayer) = 4 Then
'		Flashforms f2A, 400, 100, 0
		Flashforms F2B, 400, 100, 0
		Flashforms Flasher004, 400, 50, 0
		Flashforms Flasher005, 400, 50, 0
'		Flashforms f1A, 400, 50, 0
		Flashforms F1B, 400, 50, 0
		Flashforms Flasher006, 400, 50, 0
		Flashforms Flasher007, 400, 50, 0
		FlashForMs F3, 1100, 75, 0
		FlashForMs F3A, 1100, 75, 0
		
		ChangeGi red
		ChangeGIIntensity 2
		GiEffect 1
	End If
 '   FlashEffect 1
    PlaySound "_achieve-002",1, BackboxVolume /2

	Trigger017.Timerenabled = 0
	Trigger017.Timerenabled = 1
'    vpmTimer.AddTimer 2500, "ChangeGi white:ChangeGIIntensity 1 '"


    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger017"
End Sub

Sub Trigger017_Timer
	Trigger017.Timerenabled = 0
	ChangeGi white
	ChangeGIIntensity 1
End Sub



'************
'  Targets
'************


Sub Target007_Hit 'magnet target W3
    PLaySoundAtBall SoundFXDOF("fx_Target",107,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore score_targethits
	blinklights 54,3,1,7,7,2
	ShakeMagnet 10

    If blink(52,1) = 2 Then AwardExtraBall : UpdateExtraballLight

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "captiveball"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "captiveball" : Exit Sub

	If ArrowLights(3,3) = 2 Then ModeHit "captiveball"

	Boogiehit3    'weapon hit
	If bJackMBStarted Then JasonHit3

End Sub



'*************
'  Spinners
'*************

Sub Spinner001_Spin 'right
    DOF 264, DOFPulse
    PlaySoundAt "fx_spinner", Spinner001
    If Tilted Then Exit Sub
	Playsound "sfx_whoosh",1,BackboxVolume
    DOF 200, DOFPulse

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "spinner001"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "spinner001" : Exit Sub

	SpinnerSpins = SpinnerSpins + 1 ' eob
	If SpinnerTime > DMD_frame Then
		SpinnerCount = SpinnerCount + 1
		If SpinnerCount > 20 Then SpinnerCount = 20
'		If SpinnerCount > 16 Then Debug.Print "spinnercount " & SpinnerCount
		SpinnerTime = DMD_Frame + 50 ' 50 = 1sec
	Else
		SpinnerTime = DMD_Frame + 50 ' 50 = 1sec
		SpinnerCount = 1
	End If
	If MegaSpinnersActive Then
		megaspinnerjackpot
		Addscore score_megaspinns
		If SpinnerCount = 18 Then
			AddScore score_megaspinns * 2
		End If
	Else
		Addscore score_spinnerspin + ( score_multispin * SpinnerCount )
	End If



	If TranslateLetters(ModeRunning(CurrentPlayer)) = 16 Then
		ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
		ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2	
		If ModeProgress = 10 Then DMD_StartSplash "+10 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		If ModeProgress = 20 Then ModeCollectionDone
	End If
	If TranslateLetters(ModeRunning(CurrentPlayer)) = 13 Then
		ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
		ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2	
		If ModeProgress = 10 Then DMD_StartSplash "+40 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		If ModeProgress = 20 Then DMD_StartSplash "+30 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		If ModeProgress = 30 Then DMD_StartSplash "+20 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
'		If ModeProgress = 40 Then DMD_StartSplash "+10 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		If ModeProgress = 50 Then ModeCollectionDone
	End If
	If TranslateLetters(ModeRunning(CurrentPlayer)) = 17 Then
		ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
		ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2	
		dim tmp
		tmp = 80-ModeProgress
		If tmp < 0 Then tmp = 0
		updatecountdown tmp
		If ModeProgress < 80 Then DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,55 , "80daysupdate" ,999,0,0 
		If ModeProgress = 20 Then makingxmascalls
		If ModeProgress = 30 Then makingxmascalls
		If ModeProgress = 40 Then makingxmascalls
		If ModeProgress = 50 Then makingxmascalls
		If ModeProgress = 60 Then makingxmascalls
		If ModeProgress = 70 Then makingxmascalls
		If ModeProgress = 80 Then ModeCollectionDone : FlushSplashQ : DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,120 , "80dayscomplete" ,111,0,0 
	End If
    CenterSpinnerHits(CurrentPlayer) = CenterSpinnerHits(CurrentPlayer) + 1
    If Blink(52,1) = 0 And CenterSpinnerHits(CurrentPlayer) >= score_spinsneededforeob + BonusMultiplier(CurrentPlayer) * 5 Then
		blink(55,1) = 2
    End If

End Sub



'*********
' scoop
'*********

Dim aBall
Dim kickdelay

Sub scoop_Hit
    DOF 267, DOFPulse

    SoundSaucerLock
    BallsinHole = BallsInHole + 1
    Set aBall = ActiveBall
    scoop.TimerEnabled = 1
    If Tilted Then Exit Sub
	ChangeGi blue
	kickdelay = 800
	ScoopTopLight.state = 2

	Blink(57,1) = 1
	BlinkLights 57,57,2,4,2,0

	Shakesnalones 12

    Flashforms f4, 800, 50, 0
    Flashforms Flasher003, 800, 50, 0
	ObjLevel(1) = 1 : FlasherFlash1_Timer : blinklights 40,40,2,8,3,0
	LightEffect 8
	presentseffect 3
    Addscore score_scoophit
	PlaySound "sfx_thunder" &RndInt(1,3),1,BackboxVolume * 0.4
	If bMultiBallMode = False Then
		kickdelay = 1222
	End If


	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "scoophit" : kickdelay = 2222
	If WizardMode(CurrentPlayer) = 2 Then
		Wizardhit "scoophit"
		If WizardStage < 8 Then kickdelay = 1313
	Else
		If Blink(43,1) = 2 Then kickdelay = 3434 : DeliverPresent : greenoogie = False
		If ArrowLights(10,1) = 2 Then MegaspinnerScoop : kickdelay = 3000
		If ArrowLights(10,3) = 2 Then ModeHit "scoophit" : kickdelay = 2777

		If bSkillShotReady Then ResetSkillShotTimer_Timer
	End If
    ' kick out the ball during hurry-ups
    ' check for modes
	BallRelease.timerinterval = kickdelay
	BallRelease.timerenabled = True
'	vpmtimer.addtimer kickdelay, "kickBallOut '"
End Sub

Sub Boogie
	Playsound "vo_boog" &RndNbr(2),1,BackboxVolume/2 'idig
End Sub


Sub scoop_Timer
    If aBall.Z > -15 Then
        aBall.Z = aBall.Z -5
    Else
        scoop.Destroyball
        Me.TimerEnabled = 0
        If Tilted Then BallRelease_Timer
    End If
End Sub

Sub BallRelease_Timer ' for scoop KickOut
	BallRelease.timerenabled = False
	Blink(57,1) = 0
	ChangeGi white
    If BallsinHole > 0 Then
		Objlevel(1) = 1 : FlasherFlash1_Timer : blinklights 40,40,2,8,3,0
		PlaySound "sfx_thunder" &RndNbr(5),1,BackboxVolume * 0.4
        BallsinHole = BallsInHole - 1
        SoundSaucerKick 1, scoop
        DOF 124, DOFPulse
        scoop.CreateSizedBallWithMass BallSize / 2, BallMass
        scoop.kick 235, 22, 1
		ScoopTopLight.state = 0
		LightEffect 9
        Flashforms F4, 500, 50, 0
		Flashforms Flasher003, 500, 50, 0

		BallRelease.timerinterval = 555
		BallRelease.timerenabled = True
       ' vpmtimer.addtimer 555, "kickBallOut '" 'kick out the rest of the balls, if any
    End If
End Sub

'*************
' Magnet
'*************
Dim jumpertime
Sub Trigger007_Hit
    If Tilted Then Exit Sub
	jumpertime = 50

	Stoprando = 88
	SetXlight   1, 16,7,7,0,4 
	SetXlight  17, 61,7,7,0,4  
	SetXlight 130, 62,7,7,0,4  
	SetXlight 131,200,7,7,0,4  
	SetXlight 201,230,7,7,0,4 

    DOF 206, DOFPulse
 '   If ActiveBall.VelY > 4 Then
 '       ActiveBall.VelY = 4
 '   End If
	If LastSwitchHit = "Trigger011" Then
		mMagnet.MagnetOn = True
		DOF 112, DOFOn
		Me.TimerEnabled = 1 'to turn off the Magnet
	End If

	addscore score_triggerhits

	If ActiveBall.vely < 0 Then   ' only going upwards

			If Combotimer > DMD_Frame Then AwardComboShot "leftinnerloop" Else RestartComboTimer
		If WizardMode(CurrentPlayer) = 1 Then Wizardhit "leftinnerloop"
		If WizardMode(CurrentPlayer) = 2 Then Wizardhit "leftinnerloop" : Exit Sub

		If Blink(26,1) = 2 Then ModeHit "leftinnerloop"
		If bJackMBStarted Then JasonHit4	    'Jason multiball
		updatemodecombolights "leftinnerloop" 

	End If



    LastSwitchHit = "Trigger007"
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
'  Teenager kill
'*******************

' the timer will change the current teenager by lighting the light on top of her
Dim Teenkiller : Teenkiller = Array(flasher010,flasher011,flasher012,flasher013,flasher014,flasher015,flasher016,flasher017,flasher018)
Dim TeenLight
	TeenTimer.interval = 500
Sub TeenTimer_Timer
	TeenTimer.interval = 500
	TeenLight = RndInt(0,9)
End Sub




'*************************
' Tommy Jarvis - Hurry up
'*************************
' it starts after each 6 teenagers killed
' a 30 seconds hurry up starts at the right ramp
' hit the right ramp to throw a deadly blow to your archie enemy
' fail and your flippers will die for 3 seconds



Sub DisableFlippers(enabled)
    If enabled Then
        SolLFlipper 0
        SolRFlipper 0
        bFlippersEnabled = 0
    Else
        bFlippersEnabled = 1
    End If
End Sub






'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF
Set LF = New FlipperPolarity
Dim RF
Set RF = New FlipperPolarity

InitPolarity

'*******************************************
' Early 90's and after

Sub InitPolarity()
	Dim x, a
	a = Array(LF, RF)
	For Each x In a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger
		
		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, -5.5
		x.AddPt "Polarity", 2, 0.4, -5.5
		x.AddPt "Polarity", 3, 0.6, -5.0
		x.AddPt "Polarity", 4, 0.65, -4.5
		x.AddPt "Polarity", 5, 0.7, -4.0
		x.AddPt "Polarity", 6, 0.75, -3.5
		x.AddPt "Polarity", 7, 0.8, -3.0
		x.AddPt "Polarity", 8, 0.85, -2.5
		x.AddPt "Polarity", 9, 0.9,-2.0
		x.AddPt "Polarity", 10, 0.95, -1.5
		x.AddPt "Polarity", 11, 1, -1.0
		x.AddPt "Polarity", 12, 1.05, -0.5
		x.AddPt "Polarity", 13, 1.1, 0
		x.AddPt "Polarity", 14, 1.3, 0
		
		x.AddPt "Velocity", 0, 0,	   1
		x.AddPt "Velocity", 1, 0.160, 1.06
		x.AddPt "Velocity", 2, 0.410, 1.05
		x.AddPt "Velocity", 3, 0.530, 1'0.982
		x.AddPt "Velocity", 4, 0.702, 0.968
		x.AddPt "Velocity", 5, 0.95,  0.968
		x.AddPt "Velocity", 6, 1.03,  0.945
	Next
	
	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
End Sub


'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before\
' modified 2024 by rothbauerw
' Added Reprocessballs for flipper collisions (LF.Reprocessballs Activeball and RF.Reprocessballs Activeball must be added to the flipper collide subs
' Improved handling to remove correction for backhand shots when the flipper is raised

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef, FlipStartAngle
	Private Balls(20), balldata(20)
	Private Name
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = new SpoofBall
		Next
	End Sub
	
	Public Sub SetObjects(aName, aFlipper, aTrigger)
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper
		FlipperStart = aFlipper.x
		FlipperEnd = Flipper.Length * Sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * Cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
		
		Dim str
		str = "Sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "Sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)
		
	End Sub
	
	' Legacy: just no op
	Public Property Let EndPoint(aInput)
		
	End Property
	
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
		Select Case aChooseArray
			Case "Polarity"
				ShuffleArrays PolarityIn, PolarityOut, 1
				PolarityIn(aIDX) = aX
				PolarityOut(aIDX) = aY
				ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
				ShuffleArrays VelocityIn, VelocityOut, 1
				VelocityIn(aIDX) = aX
				VelocityOut(aIDX) = aY
				ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
				ShuffleArrays YcoefIn, YcoefOut, 1
				YcoefIn(aIDX) = aX
				YcoefOut(aIDX) = aY
				ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
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
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				balldata(x).Data = balls(x)
			End If
		Next
		FlipStartAngle = Flipper.currentangle
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

	Public Sub ReProcessBalls(aBall) 'save data of balls in flipper range
		If FlipperOn() Then
			Dim x
			For x = 0 To UBound(balls)
				If Not IsEmpty(balls(x)) Then
					if balls(x).ID = aBall.ID Then
						If isempty(balldata(x).ID) Then
							balldata(x).Data = balls(x)
						End If
					End If
				End If
			Next
		End If
	End Sub

	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef, BalltoFlip, BalltoBase, NoCorrection, checkHit
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY > -8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					BalltoFlip = DistanceFromFlipperAngle(BallData(x).x, BallData(x).y, Flipper, FlipStartAngle)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
				NoCorrection = 1
			Else
				checkHit = 50 + (20 * BallPos) 

				If BalltoFlip > checkHit or (PartialFlipCoef < 0.5 and BallPos > 0.22) Then
					NoCorrection = 1
				Else
					NoCorrection = 0
				End If
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				'If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled and NoCorrection = 0 Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef*VelCoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray)		'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount-1+offset)		'Resize original array
	For x = 0 To aCount-1				'set objects back into original array
		If IsObject(a(x)) Then
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
	BallSpeed = Sqr(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	'find active line
	Dim ii
	For ii = 1 To UBound(xKeyFrame)
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )		 'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )		'Clamp upper
	
	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS
'******************************************************
' To add the flipper tricks you must
'	 - Include a call to FlipperCradleCollision from within OnBallBallCollision subroutine
'	 - Include a call the CheckLiveCatch from the LeftFlipper_Collide and RightFlipper_Collide subroutines
'	 - Include FlipperActivate and FlipperDeactivate in the Flipper solenoid subs

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	Dim gBOT
	gBOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper2) Then
					gBOT(b).velx = gBOT(b).velx / 1.3
					gBOT(b).vely = gBOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub


Dim FCCDamping: FCCDamping = 0.4

Sub FlipperCradleCollision(ball1, ball2, velocity)
	if velocity < 0.7 then exit sub		'filter out gentle collisions
    Dim DoDamping, coef
    DoDamping = false
    'Check left flipper
    If LeftFlipper.currentangle = LFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, LeftFlipper) OR FlipperTrigger(ball2.x, ball2.y, LeftFlipper) Then DoDamping = true
    End If
    'Check right flipper
    If RightFlipper.currentangle = RFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, RightFlipper) OR FlipperTrigger(ball2.x, ball2.y, RightFlipper) Then DoDamping = true
    End If
    If DoDamping Then
		coef = FCCDamping
        ball1.velx = ball1.velx * coef: ball1.vely = ball1.vely * coef: ball1.velz = ball1.velz * coef
        ball2.velx = ball2.velx * coef: ball2.vely = ball2.vely * coef: ball2.velz = ball2.velz * coef
    End If
End Sub

'*****************
' Maths
'*****************

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function DistanceFromFlipperAngle(ballx, bally, Flipper, Angle)
	DistanceFromFlipperAngle = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Angle + 90)) + Flipper.x, Sin(Radians(angle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, ULFPress, RFPress, LFCount, RFCount
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
		SOSRampup = 2.5
	Case 1
		SOSRampup = 6
	Case 2
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
' Const EOSReturn = 0.055  'EM's
' Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
' Const EOSReturn = 0.025  'mid 90's and later

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
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, gBOT
		gBOT = GetBalls
		
		For b = 0 To UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If gBOT(b).vely >= - 0.4 Then gBOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 5  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
Const BaseDampen = 0.55

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
    Dim Dir, LiveDist
    Dir = Flipper.startangle / Abs(Flipper.startangle)    '-1 for Right Flipper
    Dim LiveCatchBounce   'If live catch is not perfect, it won't freeze ball totally
    Dim CatchTime
    CatchTime = GameTime - FCount
    LiveDist = Abs(Flipper.x - ball.x)

    If CatchTime <= LiveCatch And parm > 3 And LiveDist > LiveDistanceMin And LiveDist < LiveDistanceMax Then
        If CatchTime <= LiveCatch * 0.5 Then   'Perfect catch only when catch time happens in the beginning of the window
            LiveCatchBounce = 0
        Else
            LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)  'Partial catch when catch happens a bit late
        End If
        
        If LiveCatchBounce = 0 And ball.velx * Dir > 0 And LiveDist > 30 Then ball.velx = 0

        If ball.velx * Dir > 0 And LiveDist < 30 Then
            ball.velx = BaseDampen * ball.velx
            ball.vely = BaseDampen * ball.vely
            ball.angmomx = BaseDampen * ball.angmomx
            ball.angmomy = BaseDampen * ball.angmomy
            ball.angmomz = BaseDampen * ball.angmomz
        Elseif LiveDist > 30 Then
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx = 0
            ball.angmomy = 0
            ball.angmomz = 0
        End If
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
    End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************





'******************************************************
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	TargetBouncer ActiveBall, 0.7
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports In debugger (In vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports In debugger (In vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		aBall.velz = aBall.velz * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
'			aBall.velz = aBall.velz * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = GetBalls
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
Sub RDampen_Timer
	Cor.Update
End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************



'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel, vratio
	If TargetBouncerEnabled = 1 And aball.z < 30 Then
		'   debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
		If aBall.velx = 0 Then vratio = 1 Else vratio = aBall.vely / aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1
				zMultiplier = 0.2 * defvalue
			Case 2
				zMultiplier = 0.25 * defvalue
			Case 3
				zMultiplier = 0.3 * defvalue
			Case 4
				zMultiplier = 0.4 * defvalue
			Case 5
				zMultiplier = 0.45 * defvalue
			Case 6
				zMultiplier = 0.5 * defvalue
		End Select
		aBall.velz = Abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = Sgn(aBall.velx) * Sqr(Abs((vel ^ 2 - aBall.velz ^ 2) / (1 + vratio ^ 2)))
		aBall.vely = aBall.velx * vratio
		'   debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'   debug.print "conservation check: " & BallSpeed(aBall)/vel
	End If
End Sub

'Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer ActiveBall, 1
End Sub

'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS
	
	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script In-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function

Function RotPoint(x,y,angle)
	dim rx, ry
	rx = x*dCos(angle) - y*dSin(angle)
	ry = x*dSin(angle) + y*dCos(angle)
	RotPoint = Array(rx,ry)
End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			'   debug.print " "
		End If
	End Sub
End Class

'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]
RollingSoundFactor = 1.1 / 5

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.95					  'volume level; range [0, 1]
BumperSoundFactor = 0.025						'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2		  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055 / 5			 'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075 / 5			   'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075 / 5			'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025		   'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025		   'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8	  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075				   'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075 / 3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.6			'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10	 'volume multiplier; must not be zero
DTSoundLevel = 0.25				 'volume multiplier; must not be zero
RolloverSoundLevel = 0.25		   'volume level; range [0, 1]
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8				   'volume level; range [0, 1]
BallReleaseSoundLevel = 1			   'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2	'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015	 'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025 / 5			 'volume multiplier; must not be zero

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
	PlaySound playsoundparams, - 1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
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
	PlaySound playsoundparams, - 1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
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
	PlaySound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioFade = CSng(tmp ^ 10)
	Else
		AudioFade = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioPan = CSng(tmp ^ 10)
	Else
		AudioPan = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = CSng(BallVel(ball) ^ 2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(BallVel(ball) ^ 3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^ 2 * 15
End Function

Function RndInt(min, max) ' Sets a random number integer between min and max
	RndInt = Int(Rnd() * (max - min + 1) + min)
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeLeftSoundLevel * VolumeDial, - 0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub

Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////

Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////

Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd * 11) + 1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd * 7) + 1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////

Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////

Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////

Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd * 7) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd * 8) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm / 10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm / 10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd * 7) + 1), parm * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd * 4) + 1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub Rubbers_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd * 10) + 1
		Case 1
			PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 2
			PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 3
			PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 4
			PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 5
			PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 6
			PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 7
			PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 8
			PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 9
			PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 10
			PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd * 9) + 1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()
End Sub

Sub RandomSoundWall()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 5) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 4) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 3) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd * 13) + 1), Vol(ActiveBall) * MetalImpactSoundFactor
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
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Bounce_" & Int(Rnd * 2) + 1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////

Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd * 3) + 1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(ActiveBall.id) < 4) And cor.ballvely(ActiveBall.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd * 3) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd * 7) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft ActiveBall
	Else
		RandomSoundTargetHitWeak()
	End If
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd * 9) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd * 7) + 1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd * 5) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, ActiveBall
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, ActiveBall
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If ActiveBall.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If ActiveBall.velx <  - 8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If ActiveBall.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If ActiveBall.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, ActiveBall
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0
			PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1
			PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd * 7) + 1
		Case 1
			snd = "Ball_Collide_1"
		Case 2
			snd = "Ball_Collide_2"
		Case 3
			snd = "Ball_Collide_3"
		Case 4
			snd = "Ball_Collide_4"
		Case 5
			snd = "Ball_Collide_5"
		Case 6
			snd = "Ball_Collide_6"
		Case 7
			snd = "Ball_Collide_7"
	End Select
	
	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)

	FlipperCradleCollision ball1, ball2, velocity

End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315  'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05	  'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025 * RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025 * RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025 * RelayFlashSoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025 * RelayFlashSoundLevel, obj
	End Select
End Sub


'******************************************************
'****  END FLEEP MECHANICAL SOUNDS
'******************************************************

'******************************************************
'	ZBRL:  BALL ROLLING AND DROP SOUNDS
'******************************************************

' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 To tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b
	Dim gBOT
	gBOT = GetBalls
	
	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 To tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		'If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(gBOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(gBOT)
		If BallVel(gBOT(b)) > 1 And gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If
		
		' Ball Drop Sounds
		If gBOT(b).VelZ <  - 1 And gBOT(b).z < 55 And gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz >  - 7 Then
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard gBOT(b)
				End If
			End If
		End If
		
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
		
		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dynamic Ball Shadows
		'If AmbientBallShadowOn = 0 Then
		'	If gBOT(b).Z > 30 Then
		'		BallShadowA(b).height = gBOT(b).z - BallSize / 4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
		'	Else
		'		BallShadowA(b).height = 0.1
		'	End If
		'	BallShadowA(b).Y = gBOT(b).Y + offsetY
		'	BallShadowA(b).X = gBOT(b).X + offsetX
		'	BallShadowA(b).visible = 1
		'End If
	Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************


'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'		  * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'		  * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'		  * Create a Timer called RampRoll, that is enabled, with a interval of 100
'		  * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'		  * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'		  * To stop tracking ball
'				 * call WireRampOff
'				 * Otherwise, the ball will auto remove if it's below 30 vp units
'

Dim RampMinLoops
RampMinLoops = 4

' RampBalls
' Setup:  Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RampBalls(6,2)
Dim RampBalls(6,2)
'x,0 = ball x,1 = ID, 2 = Protection against ending early (minimum amount of updates)

'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
' Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
' Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
Dim RampType(6)

Sub WireRampOn(input)
	Waddball ActiveBall, input
	RampRollUpdate
End Sub

Sub WireRampOff()
	WRemoveBall ActiveBall.ID
End Sub

' WaddBall (Active Ball, Boolean)
Sub Waddball(input, RampInput) 'This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	Dim x
	For x = 1 To UBound(RampBalls)	'Check, don't add balls twice
		If RampBalls(x, 1) = input.id Then
			If Not IsEmpty(RampBalls(x,1) ) Then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next
	
	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 To UBound(RampBalls)
		If IsEmpty(RampBalls(x, 1)) Then
			Set RampBalls(x, 0) = input
			RampBalls(x, 1) = input.ID
			RampType(x) = RampInput
			RampBalls(x, 2) = 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			Exit Sub
		End If
		If x = UBound(RampBalls) Then	 'debug
			Debug.print "WireRampOn error, ball queue Is full: " & vbNewLine & _
			RampBalls(0, 0) & vbNewLine & _
			TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbNewLine & _
			TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbNewLine & _
			TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbNewLine & _
			TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbNewLine & _
			TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbNewLine & _
			" "
		End If
	Next
End Sub

' WRemoveBall (BallId)
Sub WRemoveBall(ID) 'This subroutine is called from the RampRollUpdate subroutine and is used to remove and stop the ball rolling sounds
	'   Debug.Print "In WRemoveBall() + Remove ball from loop array"
	Dim ballcount
	ballcount = 0
	Dim x
	For x = 1 To UBound(RampBalls)
		If ID = RampBalls(x, 1) Then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		If Not IsEmpty(Rampballs(x,1)) Then ballcount = ballcount + 1
	Next
	If BallCount = 0 Then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer()
	RampRollUpdate
End Sub

Sub RampRollUpdate()	'Timer update
	Dim x
	For x = 1 To UBound(RampBalls)
		If Not IsEmpty(RampBalls(x,1) ) Then
			If BallVel(RampBalls(x,0) ) > 1 Then ' if ball is moving, play rolling sound
				If RampType(x) Then
					PlaySound("RampLoop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2) = RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			End If
			If RampBalls(x,0).Z < 30 And RampBalls(x, 2) > RampMinLoops Then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
	Next
	If Not RampBalls(0,0) Then RampRoll.enabled = 0
End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	Me.text = "on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbNewLine & _
	"1 " & TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbNewLine & _
	"2 " & TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbNewLine & _
	"3 " & TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbNewLine & _
	"4 " & TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbNewLine & _
	"5 " & TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbNewLine & _
	"6 " & TypeName(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbNewLine & _
	" "
End Sub

Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
	BallPitch = pSlope(BallVel(ball), 1, - 1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, - 4000, 60, 7000)
End Function

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************





'******************************************************
'*****   FLUPPER DOMES 
'******************************************************
' Based on FlupperDoms2.2

' What you need in your table to use these flashers:
' Open this table and your table both in VPX
' Export all the materials domebasemat, Flashermaterial0 - 20 and import them in your table
' Export all textures (images) starting with the name "dome" and "ronddome" and import them into your table with the same names
' Export all textures (images) starting with the name "flasherbloom" and import them into your table with the same names
' Copy a set of 4 objects flasherbase, flasherlit, flasherlight and flasherflash from layer 7 to your table
' If you duplicate the four objects for a new flasher dome, be sure that they all end with the same number (in the 0-20 range)
' Copy the flasherbloom flashers from layer 10 to your table. you will need to make one per flasher dome that you plan to make
' Select the correct flasherbloom texture for each flasherbloom flasher, per flasher dome
' Copy the script below 
' Only use InitFlasher and RotateFlasher for the flasher(s) in your table
' Align the FlasherFlash object by setting TestFlashers = 1 (below in the script)
' For flashing the flasher use in the script: "ObjLevel(1) = 1 : FlasherFlash1_Timer"
' This should also work for flashers with variable flash levels from the rom, just use ObjLevel(1) = xx from the rom (in the range 0-1)
'
' Notes (please read!!):
' - The init script moves the flasherlit primitive on the exact same spot and rotation as the flasherbase
' - The flasherbase position determines the position of the flasher you will see when playing the table
' - The rotation of the primitives with "handles" is done with a script command, not on the primitive itself (see RotateFlasher below)
'   (I have used animated primitives to rotate the "handles" of the flashers)
' - Color of the objects are set in the script, not on the primitive itself
' - Screws are optional to copy and position manually
' - If your table is not named "Table1" then change the name below in the script
' - Every flasher uses its own material (Flashermaterialxx), do not use it for anything else
' - Lighting > Bloom Strength affects how the flashers look, do not set it too high
' - Change RotY and RotX of flasherbase only when having a flasher something other then parallel to the playfield
' - Leave RotX of the flasherflash object to -45; this makes sure that the flash effect is visible in FS and DT
' - If the flasherbase is parallel to the playfield, RotZ is automatically set for the flasherflash object
' - If you want to resize a flasher, be sure to resize flasherbase, flasherlit and flasherflash with the same percentage
' - If you think that the flasher effects are too bright, change flasherlightintensity and/or flasherflareintensity below
' - You need to manually position the VPX lights at the correct position and height (for instance just above a plastic)

' Some more notes for users of the v1 flashers and/or JP's fading lights routines:
' - Delete all textures/primitives/script/materials in your table from the v1 flashers and scripts before you start; they don't mix well with v2
' - Remove flupperflash(m) routines if you have them; they do not work with this new script
' - Do not try to mix this v2 script with the JP fading light routine (that is making it too complicated), just use the example script below

' example script for rom based tables (non modulated):

' SolCallback(25)="FlashRed"
'
' Sub FlashRed(flstate)
'	If Flstate Then
'		Objlevel(1) = 1 : FlasherFlash1_Timer
'	End If
' End Sub

' example script for rom based tables (modulated):

' SolModCallback(25)="FlashRed"
'
' Sub FlashRed(level)
'	Objlevel(1) = level/255 : FlasherFlash1_Timer
' End Sub




Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.15		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.2		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.9		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.4		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"

InitFlasher 1, "white"
InitFlasher 2, "red"
InitFlasher 3, "red"
InitFlasher 4, "red"
InitFlasher 5, "red"

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 1,17 : RotateFlasher 2,0 : RotateFlasher 3,90 : RotateFlasher 4,90 

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objbloom(nr).color = RGB(4,120,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4) : objbloom(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4) : objbloom(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) : objbloom(nr).color = RGB(230,49,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50) : objbloom(nr).color = RGB(200,173,25)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59) : objbloom(nr).color = RGB(255,240,150)
		Case "orange" :  objlight(nr).color = RGB(255,70,0) : objflasher(nr).color = RGB(255,70,0) : objbloom(nr).color = RGB(255,70,0)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub FlashFlasher(nr)
    DOF 276, DOFPulse
	If not objflasher(nr).TimerEnabled Then
		objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 
		objbloom(nr).visible = 1
		objlit(nr).visible = 1
	End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objbloom(nr).opacity = ( 555 + gi007.getinplayintensity) *  FlasherBloomIntensity * ObjLevel(nr)^2.5 
	If nr = 1 Then lightning.opacity = ObjLevel(1)^4 * 333

	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then
		objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objbloom(nr).visible = 0 : objlit(nr).visible = 0
	End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 



'******************************************************
'******  END FLUPPER DOMES
'******************************************************

Dim ShakeStuff1
Dim Shakestuff2
Dim Shakestuff3
Dim Shakestuff4

Sub ShakePlunger(nr)
	If nr > 12 Then nr = 12
	If nr < 3 Then Exit Sub
	ShakeStuff1 = nr
	PlungerShakeTimer.Enabled = True
	TrashcanSound
End Sub

Sub TrashcanSound
	Playsound "vo_trashcan" & RndInt(1,6),1,BackboxVolume/12
End Sub

Sub ShakeMagnet(nr)
	If nr > 12 Then nr = 12
	If nr < 3 Then Exit Sub
	ShakeStuff2 = nr
	MagnetShaker.Enabled = True
End Sub

Sub MagnetShaker_Timer
	Playsoundat "fx_motor",drain
	Primitive154.transx = ShakeStuff2 / 3
	Primitive154.rotz = ShakeStuff2 / 3

	Primitive154.transz = ShakeStuff2 / 3
	Primitive154.transy = ShakeStuff2 / 3
    If Shakestuff2 = 0 Then MagnetShaker.Enabled = False : Exit Sub
    If Shakestuff2 < 0 Then
        Shakestuff2 = ABS(Shakestuff2) - 1
    Else
        Shakestuff2 = - Shakestuff2 + 1
    End If
End Sub


Sub PlungerShakeTimer_Timer
	Playsoundat "fx_motor",drain
	Primitive150.transx = ShakeStuff1 / 3	'plungerstuff
	Primitive148.transx = ShakeStuff1 / 3
	Primitive149.transx = ShakeStuff1 / 3
	Primitive146.transx = ShakeStuff1 / 3
	Primitive012.transx = ShakeStuff1 / 3
	Primitive293.transx = ShakeStuff1 / 4	' rampend
	Primitive165.transx = ShakeStuff1 / 4	' wheelchair
	Primitive165.transz = ShakeStuff1 / 4	' wheelchair
'	Primitive166.transx = ShakeStuff1 / 4
'	Primitive166.transz = ShakeStuff1 / 4
'	Primitive167.transx = ShakeStuff1 / 5	' wheelchair
'	Primitive167.transz = ShakeStuff1 / 5	' wheelchair
	Primitive155.transx = ShakeStuff1 / 5	' spinnerman
	Primitive155.transz = ShakeStuff1 / 5
	Primitive156.transx = ShakeStuff1 / 5
	Primitive156.transz = ShakeStuff1 / 5
	Primitive157.transx = ShakeStuff1 / 5
	Primitive157.transz = ShakeStuff1 / 5
    If ShakeStuff1 = 0 Then PlungerShakeTimer.Enabled = False : Exit Sub
    If ShakeStuff1 < 0 Then
        ShakeStuff1 = ABS(ShakeStuff1) - 1
    Else
        ShakeStuff1 = - ShakeStuff1 + 1
    End If
End Sub

Sub Shakesnalones(nr)
	If nr > 12 Then nr = 12
	If nr < 3 Then Exit Sub
	ShakeStuff3 = nr
	Shakesmalones.Enabled = True
	TrashcanSound
End Sub

Sub Shakesmalones_Timer
	Playsoundat "fx_motor",drain
	Primitive138.transz = ShakeStuff3 * 0.2

	Primitive158.transy = ShakeStuff3 * 0.7
	Primitive162.transy = 8.4 - ShakeStuff3 * 0.7
	Primitive187.objrotx = ShakeStuff3 / 7
	Primitive187.objroty = -ShakeStuff3 / 7
    If ShakeStuff3 = 0 Then Shakesmalones.Enabled = False : Exit Sub
    If ShakeStuff3 < 0 Then
        ShakeStuff3 = ABS(ShakeStuff3) - 1
    Else
        ShakeStuff3 = - ShakeStuff3 + 1
    End If
End Sub
Sub Shakehousefront(nr)
	If nr > 12 Then nr = 12
	If nr < 3 Then Exit Sub
	ShakeStuff4 = nr
	Boneramp002.timerenabled = True
	TrashcanSound
End Sub
Boneramp002.timerinterval = 100
Sub Boneramp002_timer
	Playsoundat "fx_motor",drain
	Primitive138.transz = ShakeStuff4 * 0.2
    If ShakeStuff4 = 0 Then Boneramp002.timerenabled = False : Exit Sub
    If ShakeStuff4 < 0 Then
        ShakeStuff4 = ABS(ShakeStuff4) - 1
    Else
        ShakeStuff4 = - ShakeStuff4 + 1
    End If
End Sub

'******************************************************
'**** NEW FLEXDMD
'******************************************************

'bm_army-12.fnt   smal big dmd
'bm_army-12.png
'teeny_tiny_pixls-5.fnt  smal smal dmd
'teeny_tiny_pixls-5.png
'udmd-f12by24.fnt
'udmd-f12by24.png    '<<<   big huge dmd
'udmd-f14by26.fnt
'udmd-f14by26.png
'udmd-f4by5.fnt
'udmd-f4by5.png
'udmd-f5by7.fnt
'udmd-f5by7.png
'udmd-f6by12.fnt
'udmd-f6by12.png   <<<--- big smal dmd
'udmd-f7by13.fnt
'udmd-f7by13.png
'udmd-f7by5.fnt
'udmd-f7by5.png
'zx_spectrum-7.fnt
'zx_spectrum-7.png
Dim FontSmalBlue
Dim FontSmalGrey
Dim FontSmalRed
Dim FontSmalGreen
Dim FontBigOrange
Dim FontBigOrange2
Dim FontHugeRedhalf
Dim FontHugeOrange
Dim FontHugeOrange2
Dim FontHugeRed
Dim FontBigGrey2
Dim FontBigRed2

Dim FlexDMD
Dim DMDmode : DMDmode = 0
Dim DMD_newmode : DMD_newmode = 0
' FlexDMD constants
Const 	FlexDMD_RenderMode_DMD_GRAY = 0, _
		FlexDMD_RenderMode_DMD_GRAY_4 = 1, _
		FlexDMD_RenderMode_DMD_RGB = 2, _
		FlexDMD_RenderMode_SEG_2x16Alpha = 3, _
		FlexDMD_RenderMode_SEG_2x20Alpha = 4, _
		FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num = 5, _
		FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num_4x1Num = 6, _
		FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num = 7, _
		FlexDMD_RenderMode_SEG_2x7Num_2x7Num_10x1Num = 8, _
		FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num_gen7 = 9, _
		FlexDMD_RenderMode_SEG_2x7Num10_2x7Num10_4x1Num = 10, _
		FlexDMD_RenderMode_SEG_2x6Num_2x6Num_4x1Num = 11, _
		FlexDMD_RenderMode_SEG_2x6Num10_2x6Num10_4x1Num = 12, _
		FlexDMD_RenderMode_SEG_4x7Num10 = 13, _
		FlexDMD_RenderMode_SEG_6x4Num_4x1Num = 14, _
		FlexDMD_RenderMode_SEG_2x7Num_4x1Num_1x16Alpha = 15, _
		FlexDMD_RenderMode_SEG_1x16Alpha_1x16Num_1x7Num = 16

Const 	FlexDMD_Align_TopLeft = 0, _
		FlexDMD_Align_Top = 1, _
		FlexDMD_Align_TopRight = 2, _
		FlexDMD_Align_Left = 3, _
		FlexDMD_Align_Center = 4, _
		FlexDMD_Align_Right = 5, _
		FlexDMD_Align_BottomLeft = 6, _
		FlexDMD_Align_Bottom = 7, _
		FlexDMD_Align_BottomRight = 8



Dim FlexSizeX, FlexSizeY, FlexBorderOffsetX, FlexBorderOffsetY
Dim DMDScene
Sub DMDInit 'default/startup values

	If FlexDMDHighQuality = True Then
		FlexSizeX = 256 ' 128x32 or 256x64
		FlexSizeY = 64
		FlexBorderOffsetX = 5
		FlexBorderOffsetY = 3
 
	Else
		FlexSizeX = 128 ' 128x32 or 256x64
		FlexSizeY = 32
		FlexBorderOffsetX = 2
		FlexBorderOffsetY = 1
	End If

'	Dim fso,curdir

	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
	If FlexDMD is Nothing Then
		MsgBox "No FlexDMD found. This table will NOT run without it."
		Exit Sub
	End If
'	SetLocale(1033)
	FlexDMD.TableFile = Table1.Filename & ".vpx"
	FlexDMD.RenderMode = 2
	FlexDMD.Width = FlexSizeX
	FlexDMD.Height = FlexSizeY
'	If RenderingMode = 2 or VRTest Then FlexDMD.show = False
	FlexDMD.Clear = True
	FlexDMD.GameName = cGameName
	FlexDMD.Run = True




	Set DMDScene = FlexDMD.NewGroup("Scene")
	DMDScene.AddActor FlexDMD.NewImage("Nightmare4", "VPX.z4")   : DMDScene.GetImage("Nightmare4").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare4").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare9", "VPX.z9")   : DMDScene.GetImage("Nightmare9").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare9").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare11", "VPX.z11") : DMDScene.GetImage("Nightmare11").SetBounds 0,0,FlexSizeX,FlexSizeY : DMDScene.GetImage("Nightmare11").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare12", "VPX.z12") : DMDScene.GetImage("Nightmare12").SetBounds 0,0,FlexSizeX,FlexSizeY : DMDScene.GetImage("Nightmare12").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare13", "VPX.z13") : DMDScene.GetImage("Nightmare13").SetBounds 0,0,FlexSizeX,FlexSizeY : DMDScene.GetImage("Nightmare13").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare14", "VPX.z14") : DMDScene.GetImage("Nightmare14").SetBounds 0,0,FlexSizeX,FlexSizeY : DMDScene.GetImage("Nightmare14").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare15", "VPX.z15") : DMDScene.GetImage("Nightmare15").SetBounds 0,0,FlexSizeX,FlexSizeY : DMDScene.GetImage("Nightmare15").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare16", "VPX.z16") : DMDScene.GetImage("Nightmare16").SetBounds 0,0,FlexSizeX,FlexSizeY : DMDScene.GetImage("Nightmare16").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare17", "VPX.z17") : DMDScene.GetImage("Nightmare17").SetBounds 0,0,FlexSizeX,FlexSizeY : DMDScene.GetImage("Nightmare17").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare18", "VPX.z18") : DMDScene.GetImage("Nightmare18").SetBounds 0,0,FlexSizeX,FlexSizeY : DMDScene.GetImage("Nightmare18").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare56", "VPX.z19")   : DMDScene.GetImage("Nightmare56").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare56").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare57", "VPX.z57")   : DMDScene.GetImage("Nightmare57").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare57").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare58", "VPX.z58")   : DMDScene.GetImage("Nightmare58").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare58").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare59", "VPX.z59")   : DMDScene.GetImage("Nightmare59").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare59").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare60", "VPX.z60")   : DMDScene.GetImage("Nightmare60").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare60").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare61", "VPX.z61")   : DMDScene.GetImage("Nightmare61").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare61").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare62", "VPX.z62")   : DMDScene.GetImage("Nightmare62").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare62").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare63", "VPX.z63")   : DMDScene.GetImage("Nightmare63").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare63").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare64", "VPX.z64")   : DMDScene.GetImage("Nightmare64").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare64").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare65", "VPX.z65")   : DMDScene.GetImage("Nightmare65").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare65").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare66", "VPX.z66")   : DMDScene.GetImage("Nightmare66").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare66").Visible = False

	If FlexDMDHighQuality = True Then
		Set FontBigRed2		= FlexDMD.NewFont("FlexDMD.Resources.bm_army-12.fnt",  RGB(222, 55, 22), vbWhite, 0)
		Set FontBigGrey2	= FlexDMD.NewFont("FlexDMD.Resources.bm_army-12.fnt",  RGB(222, 200, 190), vbWhite, 0)
		Set FontBigOrange2	= FlexDMD.NewFont("FlexDMD.Resources.bm_army-12.fnt",  RGB(120, 77, 24), vbWhite, 0)
		Set FontHugeOrange	= FlexDMD.NewFont("FlexDMD.Resources.udmd-f12by24.fnt", RGB(245, 137, 33), vbWhite, 0)
		Set FontHugeOrange2	= FlexDMD.NewFont("FlexDMD.Resources.udmd-f12by24.fnt", RGB(245, 137, 33), RGB(90, 50, 11), 1)
		Set FontHugeRedhalf	= FlexDMD.NewFont("FlexDMD.Resources.udmd-f12by24.fnt", RGB(255, 33, 11), RGB(90, 50, 11), 0)
		Set FontHugeRED		= FlexDMD.NewFont("FlexDMD.Resources.udmd-f12by24.fnt", RGB(44, 22, 12), RGB(255, 33, 11), 2)
		Set FontSmalGrey	= FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt",  RGB(183,183,183), RGB(0, 0, 0), 0)
		Set FontSmalRed		= FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt",  RGB(255,33,11), RGB(0, 0, 0), 0)
		Set FontSmalGreen	= FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt",  RGB(33,244,11), RGB(0, 0, 0), 0)
		Set FontSmalBlue	= FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt",  RGB(88,133,254), RGB(0, 0, 0), 0)
	Else
		Set FontBigGrey2	= FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt",  RGB(222, 200, 190), vbWhite, 0)
		Set FontBigRed2		= FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt",  RGB(222, 55, 22), vbWhite, 0)
		Set FontBigOrange2	= FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt",  RGB(120, 77, 24), vbWhite, 0)
		Set FontHugeOrange	= FlexDMD.NewFont("FlexDMD.Resources.bm_army-12.fnt", RGB(245, 137, 33), vbWhite, 0)
		Set FontHugeOrange2	= FlexDMD.NewFont("FlexDMD.Resources.bm_army-12.fnt", RGB(245, 137, 33), RGB(90, 50, 11), 2)
		Set FontHugeRedhalf	= FlexDMD.NewFont("FlexDMD.Resources.bm_army-12.fnt", RGB(255, 33, 11), RGB(90, 50, 11), 0)
		Set FontHugeRED		= FlexDMD.NewFont("FlexDMD.Resources.udmd-f12by24.fnt", RGB(88, 33, 44), RGB(255, 33, 11), 2)
		Set FontSmalGrey	= FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", RGB(183, 183, 183), RGB(0, 0, 0), 0)
		Set FontSmalRed		= FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt",  RGB(255,33,11), RGB(0, 0, 0), 0)
		Set FontSmalGreen	= FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt",  RGB(33,244,11), RGB(0, 0, 0), 0)
		Set FontSmalBlue	= FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt",  RGB(88,133,254), RGB(0, 0, 0), 0)
	End If
' after this is over BG and under text

	DMDScene.AddActor FlexDMD.NewImage("middle", "VPX.zPumpkin1")
	DMDScene.GetImage("middle").SetSize FlexDMD.Width, FlexDMD.Height
	DMDScene.GetImage("middle").visible = False

' these ^ under all text


	DMDScene.AddActor FlexDMD.NewLabel("Text1", FontHugeOrange, " ")
	DMDScene.AddActor FlexDMD.NewLabel("Text2", FontSmalGrey, " ")
	DMDScene.AddActor FlexDMD.NewLabel("Text3", FontSmalGrey, " ")
	DMDScene.AddActor FlexDMD.NewLabel("Text4", FontSmalGrey, " ")
	DMDScene.AddActor FlexDMD.NewLabel("Text5", FontSmalGrey, " ")
	DMDScene.AddActor FlexDMD.NewLabel("TextRed", FontSmalRed, " ")

	If FlexDMDHighQuality = True Then
		DMDScene.AddActor FlexDMD.NewImage("Letter1", "VPX.z_j") : DMDScene.GetImage("Letter1").Visible = False : DMDScene.GetImage("Letter1").SETposition  10,5
		DMDScene.AddActor FlexDMD.NewImage("Letter2", "VPX.z_a") : DMDScene.GetImage("Letter2").Visible = False : DMDScene.GetImage("Letter2").SETposition  43,5  
		DMDScene.AddActor FlexDMD.NewImage("Letter3", "VPX.z_c") : DMDScene.GetImage("Letter3").Visible = False : DMDScene.GetImage("Letter3").SETposition  74,5 
		DMDScene.AddActor FlexDMD.NewImage("Letter4", "VPX.z_k") : DMDScene.GetImage("Letter4").Visible = False : DMDScene.GetImage("Letter4").SETposition  106,5 
		DMDScene.AddActor FlexDMD.NewImage("Letter5", "VPX.z_p") : DMDScene.GetImage("Letter5").Visible = False : DMDScene.GetImage("Letter5").SETposition  138,5 
		DMDScene.AddActor FlexDMD.NewImage("Letter6", "VPX.z_o") : DMDScene.GetImage("Letter6").Visible = False : DMDScene.GetImage("Letter6").SETposition  170,5
		DMDScene.AddActor FlexDMD.NewImage("Letter7", "VPX.z_t") : DMDScene.GetImage("Letter7").Visible = False : DMDScene.GetImage("Letter7").SETposition  202,5
	Else
		DMDScene.AddActor FlexDMD.NewLabel("Letter1", FontHugeRED, "J")
		DMDScene.AddActor FlexDMD.NewLabel("Letter2", FontHugeRED, "A")
		DMDScene.AddActor FlexDMD.NewLabel("Letter3", FontHugeRED, "C")
		DMDScene.AddActor FlexDMD.NewLabel("Letter4", FontHugeRED, "K")
		DMDScene.AddActor FlexDMD.NewLabel("Letter5", FontHugeRED, "P")
		DMDScene.AddActor FlexDMD.NewLabel("Letter6", FontHugeRED, "O")
		DMDScene.AddActor FlexDMD.NewLabel("Letter7", FontHugeRED, "T")
		DMDScene.GetLabel("Letter1").Visible = False
		DMDScene.GetLabel("Letter2").Visible = False
		DMDScene.GetLabel("Letter3").Visible = False
		DMDScene.GetLabel("Letter4").Visible = False
		DMDScene.GetLabel("Letter5").Visible = False
		DMDScene.GetLabel("Letter6").Visible = False
		DMDScene.GetLabel("Letter7").Visible = False

	End If
' under here TOP OF Text
	DMDScene.AddActor FlexDMD.NewImage("Nightmare0", "VPX.z0")     : DMDScene.GetImage("Nightmare0").SetBounds 0,0,FlexSizeX,FlexSizeY   : DMDScene.GetImage("Nightmare0").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare1", "VPX.z1")     : DMDScene.GetImage("Nightmare1").SetBounds 0,0,FlexSizeX,FlexSizeY   : DMDScene.GetImage("Nightmare1").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare2", "VPX.z2")     : DMDScene.GetImage("Nightmare2").SetBounds 0,0,FlexSizeX,FlexSizeY   : DMDScene.GetImage("Nightmare2").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare3", "VPX.z3")     : DMDScene.GetImage("Nightmare3").SetBounds 0,0,FlexSizeX,FlexSizeY   : DMDScene.GetImage("Nightmare3").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare5", "VPX.z5")     : DMDScene.GetImage("Nightmare5").SetBounds 0,0,FlexSizeX,FlexSizeY   : DMDScene.GetImage("Nightmare5").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare6", "VPX.z6")     : DMDScene.GetImage("Nightmare6").SetBounds 0,0,FlexSizeX,FlexSizeY   : DMDScene.GetImage("Nightmare6").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare7", "VPX.z7")     : DMDScene.GetImage("Nightmare7").SetBounds 0,0,FlexSizeX,FlexSizeY   : DMDScene.GetImage("Nightmare7").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare8", "VPX.z8")     : DMDScene.GetImage("Nightmare8").SetBounds 0,0,FlexSizeX,FlexSizeY   : DMDScene.GetImage("Nightmare8").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare10", "VPX.z10")   : DMDScene.GetImage("Nightmare10").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare10").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare19", "VPX.z19")   : DMDScene.GetImage("Nightmare19").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare19").Visible = False


	DMDScene.AddActor FlexDMD.NewImage("Nightmare20", "VPX.zMultiJ")		: DMDScene.GetImage("Nightmare20").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare20").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare21", "VPX.zMultiJA")		: DMDScene.GetImage("Nightmare21").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare21").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare22", "VPX.zMultiJAC")		: DMDScene.GetImage("Nightmare22").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare22").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare23", "VPX.zMultiJACK")		: DMDScene.GetImage("Nightmare23").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare23").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare24", "VPX.zMULTIJackoff")		: DMDScene.GetImage("Nightmare24").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare24").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare25", "VPX.zMulti_JACKpot")		: DMDScene.GetImage("Nightmare25").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare25").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare26", "VPX.zMulti_jackPOT2")		: DMDScene.GetImage("Nightmare26").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare26").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare27", "VPX.zMULTIACTIVE")		: DMDScene.GetImage("Nightmare27").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare27").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare28", "VPX.zMultiJACKPOT")		: DMDScene.GetImage("Nightmare28").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare28").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare29", "VPX.zMultiJACKPOToff")		: DMDScene.GetImage("Nightmare29").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare29").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare30", "VPX.zMultiLockislit")		: DMDScene.GetImage("Nightmare30").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare30").Visible = False

' PlayfieldMultiplier
	DMDScene.AddActor FlexDMD.NewImage("Nightmare31", "VPX.zBonus (4)")	: DMDScene.GetImage("Nightmare31").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare31").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare32", "VPX.zBonus (3)")	: DMDScene.GetImage("Nightmare32").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare32").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare33", "VPX.zBonus (2)")	: DMDScene.GetImage("Nightmare33").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare33").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare34", "VPX.zBonus (1)")	: DMDScene.GetImage("Nightmare34").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare34").Visible = False
' bonusMultiplier

	DMDScene.AddActor FlexDMD.NewImage("Nightmare40", "VPX.zLock1")		: DMDScene.GetImage("Nightmare40").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare40").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare41", "VPX.zLock1b")	: DMDScene.GetImage("Nightmare41").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare41").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare42", "VPX.zLock2")		: DMDScene.GetImage("Nightmare42").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare42").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare43", "VPX.zLock2b")	: DMDScene.GetImage("Nightmare43").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare43").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare44", "VPX.zLock3")		: DMDScene.GetImage("Nightmare44").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare44").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare45", "VPX.zLock3b")	: DMDScene.GetImage("Nightmare45").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare45").Visible = False

	DMDScene.AddActor FlexDMD.NewImage("Nightmare46", "VPX.zDelivered1")	: DMDScene.GetImage("Nightmare46").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare46").Visible = False 'deliverpresent
	DMDScene.AddActor FlexDMD.NewImage("Nightmare47", "VPX.zDelivered2")	: DMDScene.GetImage("Nightmare47").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare47").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare48", "VPX.zDelivered3")	: DMDScene.GetImage("Nightmare48").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare48").Visible = False


	DMDScene.AddActor FlexDMD.NewImage("Nightmare49", "VPX.zFindJack0")		: DMDScene.GetImage("Nightmare49").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare49").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare50", "VPX.zFindJack1")		: DMDScene.GetImage("Nightmare50").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare50").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare51", "VPX.zFindJack2")		: DMDScene.GetImage("Nightmare51").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare51").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare52", "VPX.zFindJack3")		: DMDScene.GetImage("Nightmare52").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare52").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare53", "VPX.zFindJack4")		: DMDScene.GetImage("Nightmare53").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare53").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare54", "VPX.zFindJack5")		: DMDScene.GetImage("Nightmare54").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare54").Visible = False
	DMDScene.AddActor FlexDMD.NewImage("Nightmare55", "VPX.zModeProgress")	: DMDScene.GetImage("Nightmare55").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare55").Visible = False
' 												 56 57 58 59 60 61 62 63 64 65 66  taken
	DMDScene.AddActor FlexDMD.NewImage("Nightmare67", "VPX.zInstantinfo")   : DMDScene.GetImage("Nightmare67").SetBounds 0,0,FlexSizeX,FlexSizeY  : DMDScene.GetImage("Nightmare67").Visible = False

	DMDScene.AddActor FlexDMD.NewLabel("Text6", FontSmalGrey, " ")
	DMDScene.AddActor FlexDMD.NewLabel("Text7", FontSmalGrey, " ")

	DMDScene.AddActor FlexDMD.NewImage("gameover1", "VPX.zGameover1")	: DMDScene.GetImage("gameover1").SetBounds 0,0,FlexSizeX,FlexSizeY * 6.25  : DMDScene.GetImage("gameover1").Visible = False

	DMDScene.AddActor FlexDMD.NewImage("Last", "VPX.z9")
	DMDScene.GetImage("Last").SetSize FlexDMD.Width, FlexDMD.Height
	DMDScene.GetImage("Last").visible = False
	DMDScene.AddActor FlexDMD.NewImage("Last2", "VPX.z9")
	DMDScene.GetImage("Last2").SetSize FlexDMD.Width, FlexDMD.Height
	DMDScene.GetImage("Last2").visible = False
	DMDScene.AddActor FlexDMD.NewLabel("Lasttext1", FontSmalGrey, " ")

	FlexDMD.LockRenderThread
	FlexDMD.Stage.RemoveAll
	FlexDMD.Stage.AddActor DMDScene
	FlexDMD.UnlockRenderThread



	PlayersPlayingGame = 1
	DMD_newmode = 1

End Sub

Dim BG_imageDMD :BG_imageDMD = -1


Dim DMD_Counter
Sub DMD_startmode(number)
	DMDmode = number
	DMD_Counter = 0
	DMD_removeall
	DMD_newmode = 0

	FlexDMD.LockRenderThread :

	Select Case DMDmode ' for initialization
		Case 5 :	'enter highscore display
					blinkentry = 0

					FlexDMD.Stage.GetLabel("Text1").Font = FontBigGrey2 ' bigdmd  
					FlexDMD.Stage.GetLabel("Text1").Text = " "
					FlexDMD.Stage.GetLabel("Text1").SetAlignedPosition FlexSizeX/2,FlexSizey/4-FlexBorderOffsetY, FlexDMD_Align_top
					FlexDMD.Stage.GetLabel("Text1").visible = true
					FlexDMD.Stage.GetLabel("Text2").Font = FontBigRed2 
					FlexDMD.Stage.GetLabel("Text2").Text = " "
					FlexDMD.Stage.GetLabel("Text2").SetAlignedPosition FlexSizeX/2,FlexSizey*3/4, FlexDMD_Align_Bottom
					FlexDMD.Stage.GetLabel("Text2").visible = true


		Case 3 : 	DMD_Showimage 60
					FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange ' bigdmd  
					FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1)) 
					FlexDMD.Stage.GetLabel("Text1").SetAlignedPosition FlexSizeX/2,FlexSizey*3/4, FlexDMD_Align_Bottom
					FlexDMD.Stage.GetLabel("Text1").visible = true
					FlexDMD.Stage.GetLabel("Text2").Font = FontSmalGrey 
					FlexDMD.Stage.GetLabel("Text2").Text = "303"
					FlexDMD.Stage.GetLabel("Text2").SetAlignedPosition FlexSizeX/2,FlexSizey/4-FlexBorderOffsetY, FlexDMD_Align_top
					FlexDMD.Stage.GetLabel("Text2").visible = true
					FlexDMD.Stage.GetLabel("Text3").Font = FontSmalRed 
					FlexDMD.Stage.GetLabel("Text3").Text = "303"
					FlexDMD.Stage.GetLabel("Text3").SetAlignedPosition FlexSizeX/2,FlexSizey*3/4-FlexBorderOffsetY, FlexDMD_Align_bottom
					FlexDMD.Stage.GetLabel("Text3").visible = true
					EOBcounter = 0



		Case 1 :	DMD_Showimage 4 
					lightshowrepeat = 2
					ResetAllBlink

		case 2 :	If PlayersPlayingGame = 1 Then
						DMD_Showimage BG_imageDMD 
						FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange ' bigdmd  
						FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1)) 
						FlexDMD.Stage.GetLabel("Text1").SetAlignedPosition FlexSizeX/2,FlexSizey/2+FlexBorderOffsetY+1, FlexDMD_Align_Center
						FlexDMD.Stage.GetLabel("Text1").visible = true

							FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGrey ' bigdmd  
							FlexDMD.Stage.GetLabel("Text5").Text = "BALL " & 4-BallsRemaining(CurrentPlayer) & "  CREDITS "  & Credits 
							FlexDMD.Stage.GetLabel("Text5").SetAlignedPosition FlexSizeX/2,FlexSizey-FlexBorderOffsetY, FlexDMD_Align_bottom
							FlexDMD.Stage.GetLabel("Text5").visible = true

						FlexDMD.Stage.GetLabel("Text2").Font = FontSmalRed 
						FlexDMD.Stage.GetLabel("Text2").Text = " "
						FlexDMD.Stage.GetLabel("Text2").SetAlignedPosition FlexSizeX/2,FlexSizey/2-FlexBorderOffsetY, FlexDMD_Align_bottom
						FlexDMD.Stage.GetLabel("Text2").visible = true
					ElseIf PlayersPlayingGame > 0 Then

						DMD_Showimage BG_imageDMD + 1 
						If CurrentPlayer = 1 Then FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange Else FlexDMD.Stage.GetLabel("Text1").Font = FontBigOrange2
						If CurrentPlayer = 2 Then FlexDMD.Stage.GetLabel("Text2").Font = FontHugeOrange Else FlexDMD.Stage.GetLabel("Text2").Font = FontBigOrange2
						If CurrentPlayer = 3 Then FlexDMD.Stage.GetLabel("Text3").Font = FontHugeOrange Else FlexDMD.Stage.GetLabel("Text3").Font = FontBigOrange2
						If CurrentPlayer = 4 Then FlexDMD.Stage.GetLabel("Text4").Font = FontHugeOrange Else FlexDMD.Stage.GetLabel("Text4").Font = FontBigOrange2
'						FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange ' bigdmd  
						FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))  
						FlexDMD.Stage.GetLabel("Text1").SetAlignedPosition FlexBorderOffsetX, FlexBorderOffsetY+1, FlexDMD_Align_TopLeft
						FlexDMD.Stage.GetLabel("Text1").visible = true
'						FlexDMD.Stage.GetLabel("Text2").Font = FontHugeOrange ' bigdmd  
						FlexDMD.Stage.GetLabel("Text2").Text = formatscore (score(2)) 
						FlexDMD.Stage.GetLabel("Text2").SetAlignedPosition FlexSizeX-FlexBorderOffsetX+1, FlexBorderOffsetY+1, FlexDMD_Align_TopRight
						FlexDMD.Stage.GetLabel("Text2").visible = true
						If PlayersPlayingGame > 2 Then
'							FlexDMD.Stage.GetLabel("Text3").Font = FontHugeOrange ' bigdmd  
							FlexDMD.Stage.GetLabel("Text3").text = formatscore (score(3)) 
							FlexDMD.Stage.GetLabel("Text3").SetAlignedPosition FlexBorderOffsetX, FlexSizey-FlexBorderOffsetY, FlexDMD_Align_bottomLeft
							FlexDMD.Stage.GetLabel("Text3").visible = true
						End If
						If PlayersPlayingGame > 3 Then
'							FlexDMD.Stage.GetLabel("Text4").Font = FontHugeOrange ' bigdmd  
							FlexDMD.Stage.GetLabel("Text4").Text = formatscore (score(4)) 
							FlexDMD.Stage.GetLabel("Text4").SetAlignedPosition FlexSizeX-FlexBorderOffsetX+1,FlexSizey-FlexBorderOffsetY, FlexDMD_Align_bottomright
							FlexDMD.Stage.GetLabel("Text4").visible = true
						End If

							FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGrey ' bigdmd  
							FlexDMD.Stage.GetLabel("Text5").Text = "BALL " & 4-BallsRemaining(CurrentPlayer) & "  CREDITS "  & Credits
							FlexDMD.Stage.GetLabel("Text5").SetAlignedPosition FlexSizeX/2,FlexSizey/2+FlexBorderOffsetY/2, FlexDMD_Align_center
							FlexDMD.Stage.GetLabel("Text5").visible = true

					End If
		



	End Select
	FlexDMD.UnlockRenderThread :

End Sub

Dim DMD_Frame
Dim DMD_Jackpot : DMD_Jackpot = 0
Dim DMD_Skillshot


dmd_timer.interval = 19
Sub DMD_Timer_Timer
	If TipLastOne > 0 Then AnimateLastone
	TextOverlay001.opacity = 30 - ( gi007.getinplayintensity * 6)
	DMD_Frame = DMD_Frame + 1
	If DMD_Frame mod 3 = 1 And ShakeTheBones2 <> 0 Then Boneshaker2
	If ShakeTheBones <> 0 Or LongShaker > 0 Then Boneshaker 
	DMD_Counter = DMD_Counter + 1
	If DMD_newmode > 0 Then DMD_startmode(DMD_newmode) : Exit Sub


	FlexDMD.LockRenderThread


	If SplashBlink > 0 Then DMD_showsplash

	Select Case DMDmode
		Case    5   :	DMD_updateHigh
		Case	1	:	DMD_UpdateMode1		 					' intromode/attract part ?
		Case	3	:	If DMD_Frame mod 2 = 1 Then DMD_EOB
						If NightmareShowCounter > 0 Then NightmareLigtsShow1
		Case	2	:	DMD_UpdateMode2						' normal score screen  
						If DMD_Jackpot > 0 Then DMD_UpdateJackpot

						If DMD_Frame mod 4 = 1 Then

							If DMD_Pumpkins > 0 Then
								DMD_Pumpkin = 0
								DMD_DoggieLeft = 0
								DMD_DoggieRight = 0
								DMD_xmas = 0	
								DMD_UpdatePumkins
							End If
						End If
						If DMD_Frame mod 2 = 1 Then

						If NightmareShowCounter > 0 Then NightmareLigtsShow1

							If DMD_pumpkin > 0 Then
								DMD_UpdatePumkin
								DMD_DoggieLeft = 0
								DMD_DoggieRight = 0
								DMD_xmas = 0
							ElseIf DMD_DoggieLeft > 0 Then
								DMD_UpdateDoggieLeft
								DMD_DoggieRight = 0
								DMD_xmas = 0
							ElseIf DMD_DoggieRight > 0 Then
								DMD_UpdateDoggieRight
								DMD_xmas = 0
							ElseIf DMD_xmas > 0 Then
								DMD_UpdateXmas
							End If
						End If
	End Select

	FlexDMD.UnlockRenderThread

End Sub


Dim blinkentry
Sub DMD_updateHigh
	Dim Letters(3)
	Dim LetterOffset(3)
	DMD_Showimage 4

	FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange2 ' bigdmd  
	FlexDMD.Stage.GetLabel("Text1").Text = "ENTER NAME"
	FlexDMD.Stage.GetLabel("Text1").SetAlignedPosition FlexSizeX/2,FlexSizey/3-FlexBorderOffsetY, FlexDMD_Align_center
	FlexDMD.Stage.GetLabel("Text1").visible = true
	FlexDMD.Stage.GetLabel("Text2").Font = FontHugeRedhalf 
	FlexDMD.Stage.GetLabel("Text2").Text = ">       <"
	If DMD_Frame mod 22 < 2 Then
		FlexDMD.Stage.GetLabel("Text2").Font = FontHugeRedhalf
	Else
		FlexDMD.Stage.GetLabel("Text2").Font = FontHugeorange 
	End If
	FlexDMD.Stage.GetLabel("Text2").SetAlignedPosition FlexSizeX/2,FlexSizey*2/3+FlexBorderOffsetY, FlexDMD_Align_center
	FlexDMD.Stage.GetLabel("Text2").visible = true

	Letters(0) = hsEnteredDigits(0)
	Letters(1) = hsEnteredDigits(1)
	Letters(2) = hsEnteredDigits(2)
	LetterOffset(0) = 0
	LetterOffset(1) = 0
	LetterOffset(2) = 0

	For x = 0 to 2
		If (hsCurrentDigit = x) Then 
			If(hsLetterFlash <> 0) Then
				If x = 0 Then FlexDMD.Stage.GetLabel("Text3").Font = FontHugeRedhalf
				If x = 1 Then FlexDMD.Stage.GetLabel("Text4").Font = FontHugeRedhalf
				If x = 2 Then FlexDMD.Stage.GetLabel("Text5").Font = FontHugeRedhalf
				Letters(x) = "-"
				LetterOffset(x) = FlexSizeY / 6
			Else
				Letters(x) = mid(hsValidLetters, hsCurrentLetter, 1)
				If x = 0 Then FlexDMD.Stage.GetLabel("Text3").Font = FontHugeOrange2 
				If x = 1 Then FlexDMD.Stage.GetLabel("Text4").Font = FontHugeOrange2 
				If x = 2 Then FlexDMD.Stage.GetLabel("Text5").Font = FontHugeOrange2 
			End If
		Else
			If x = 0 Then FlexDMD.Stage.GetLabel("Text3").Font = FontHugeOrange2 
			If x = 1 Then FlexDMD.Stage.GetLabel("Text4").Font = FontHugeOrange2 
			If x = 2 Then FlexDMD.Stage.GetLabel("Text5").Font = FontHugeOrange2 
		End If

	Next
	FlexDMD.Stage.GetLabel("Text3").Text = Letters(0)
	FlexDMD.Stage.GetLabel("Text3").SetAlignedPosition ((FlexSizeX/2) - (FlexSizeX/15)) ,FlexSizey*2/3+FlexBorderOffsetY+LetterOffset(0), FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Text3").visible = true
	FlexDMD.Stage.GetLabel("Text4").Text = Letters(1)
	FlexDMD.Stage.GetLabel("Text4").SetAlignedPosition FlexSizeX/2  ,FlexSizey*2/3+FlexBorderOffsetY+LetterOffset(1), FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Text4").visible = true
	FlexDMD.Stage.GetLabel("Text5").Text = Letters(2)
	FlexDMD.Stage.GetLabel("Text5").SetAlignedPosition ((FlexSizeX/2) + (FlexSizeX/15)) ,FlexSizey*2/3+FlexBorderOffsetY+LetterOffset(2), FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Text5").visible = true

	If blinkentry > 0 Then
		blinkentry = blinkentry + 1
		FlexDMD.Stage.GetLabel("Text3").Font = FontHugeRedhalf
		FlexDMD.Stage.GetLabel("Text4").Font = FontHugeRedhalf
		FlexDMD.Stage.GetLabel("Text5").Font = FontHugeRedhalf
		If blinkentry Mod 17 < 6 Then
			FlexDMD.Stage.GetLabel("Text3").Text = ""
			FlexDMD.Stage.GetLabel("Text4").Text = ""
			FlexDMD.Stage.GetLabel("Text5").Text = ""
		Else
			FlexDMD.Stage.GetLabel("Text3").Text = hsEnteredDigits(0)
			FlexDMD.Stage.GetLabel("Text4").Text = hsEnteredDigits(1)
			FlexDMD.Stage.GetLabel("Text5").Text = hsEnteredDigits(2)
		End If
	End If
	If blinkentry = 180 Then
		DMD_newmode = 2
		EndOfBallComplete()
	End If

End Sub




Dim Splashtext1
Dim	Splashtext2
Dim Splashfont1
Dim Splashfont2
Dim SplashTimer
Dim SplashEffect
Dim SplashImage
Dim SplashImage2
Dim SplashImage3
Dim SplashBlink : SplashBlink = 0

Dim SplashQ(20,9)


Sub FlushSplashQ
	StopSplash
	FlexDMD.Stage.GetLabel("Text1").Text = ""
	FlexDMD.Stage.GetLabel("Text2").Text = ""
	FlexDMD.Stage.GetLabel("Text3").Text = ""
	FlexDMD.Stage.GetLabel("Text4").Text = ""
	FlexDMD.Stage.GetLabel("Text5").Text = ""
	FlexDMD.Stage.GetLabel("Text6").Text = ""
	FlexDMD.Stage.GetLabel("Text7").Text = ""

	dim y
	SplashBlink = 0 ' turn off
	For x = 0 to 20
		For y = 0 to 9
			SplashQ(x,y) = ""
		Next
	Next
	Splashtext1 = ""	' startagain
	Splashtext2 = ""
	SplashTimer = 0
	SplashEffect = ""
	SplashImage = 500
	SplashImage2 = 500
	SplashImage3 = 500
End Sub

Sub NextSplashQ 
	dim y
	SplashBlink = 0 ' turn off	
	If SplashQ(1,1) = "" Then Exit Sub   ' add mb check here
	
	Splashtext1 = SplashQ(1,1)	' startagain
	Splashtext2 = SplashQ(1,2)
	Splashfont1 = SplashQ(1,3)
	Splashfont2 = SplashQ(1,4)
	SplashTimer = SplashQ(1,5)
	SplashEffect = SplashQ(1,6)
	SplashImage = SplashQ(1,7)
	SplashImage2 = SplashQ(1,8)
	SplashImage3 = SplashQ(1,9)
	SplashBlink = 1
	
	For x = 1 to 19
		For y = 1 to 9
			SplashQ(x,y) = SplashQ(x+1,y)
		Next
	Next
End Sub


Sub DMD_StartSplash ( text1 ,text2 , font1 , font2 , time , effect , image,image2,image3 )
'DMD_StartSplash "SKILLSHOT",Formatscore ( SkillShotValue(CurrentPlayer) - Score_addskillshot ),FontHugeOrange, FontHugeRedhalf,70 , "blinktop1" , 19,0,0
	If Tilted Then FlushSplashQ : exit sub
'insertcoin
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,60 , "insertcoin" , 19 ,0,0
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,60 , "credits" , 19 ,0,0
'DMD_StartSplash "5 MORE TO","COLLECT" ,FontHugeOrange, FontHugeRedhalf,70 , "blinktop1" ,55,0,0
'DMD_StartSplash "*SUPER - SHOT* ",Formatscore ( SuperSkillShotValue(CurrentPlayer) - Score_addskillshot ) ,FontHugeOrange, FontHugeRedhalf,60 , "blinktop1" ,19,0,0
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,120 , "blink2images" ,50,51,0
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,88 , "oneimage" ,31,0,0
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,120 , "blink2fast1" ,40,41,0
'DMD_StartSplash "x7"," ",FontHugeRED, FontHugeRedhalf,120 , "blinkbonus" ,35,0,0
'DMD_StartSplash "SUPER",Formatscore ( 550000 ) ,FontHugeOrange, FontHugeRedhalf,70 , "blinkbot2" ,29,0,0
'DMD_StartSplash "",Formatscore ( 550000 ) ,FontHugeOrange, FontHugeRedhalf,70 , "blink2text1" ,29,0,0
'DMD_StartSplash "",Formatscore ( Scoring ) ,FontHugeOrange, FontHugeRedhalf,70 , "blink2text1" ,28,29,0
'DMD_StartSplash "COLLECT","REVIVE ",FontHugeOrange, FontHugeOrange,88 , "oneimage" ,55,0,0

	If SplashEffect = "wiz1start" Then Exit Sub  
	If SplashEffect = "wiz2start" Then Exit Sub  
	If SplashEffect = "wiz3start" Then Exit Sub  
	If SplashEffect = "wiz4start" Then Exit Sub  
	If SplashEffect = "wiz5start" Then Exit Sub  
	If SplashEffect = "wiz6start" Then Exit Sub  
	If SplashEffect = "wiz7start" Then Exit Sub  
	If SplashEffect = "wiz8start" Then Exit Sub  
	If effect = "wiz1progress" And stopwizcounter > 0 Then Exit Sub
	If effect = "wiz5progress" And stopwizcounter > 0 Then Exit Sub

	If effect = "revive" or effect = "balllost" or effect = "ballsave" or effect = "gameover" Then
		FlushSplashQ 
	Elseif effect = "credits" or effect = "insertcoin" or effect = "80daysupdate" or effect = "wiz1progress" Then 
		If SplashEffect = "credits" Or SplashEffect = "insertcoin" Or SplashEffect = "80daysupdate" Or SplashEffect = "wiz1progress" Then SplashBlink = 6: Exit Sub
	Else
		If SplashBlink > 0 Then  ' AddSplashQ 
			For x = 1 to 19 ' 19 is max q  '' danger danger mostly for attract mode   info mode
				If SplashQ(x,1) = "" Then
					SplashQ(x,1) = text1
					SplashQ(x,2) = text2
					SplashQ(x,3) = font1
					SplashQ(x,4) = font2
					SplashQ(x,5) = time
					SplashQ(x,6) = effect
					SplashQ(x,7) = image
					SplashQ(x,8) = image2
					SplashQ(x,9) = image3
					Exit For
				End If
			Next
			Exit Sub ' cancel new ones
		End If
	End If
	Splashtext1 = text1
	Splashtext2 = text2
	Splashfont1 = font1
	Splashfont2 = font2
	SplashTimer = time
	SplashEffect = effect
	SplashImage = image
	SplashImage2 = image2
	SplashImage3 = image3
	SplashBlink = 1
End Sub


Dim stopwizcounter : stopwizcounter = 0
Sub StopSplash
		If SplashEffect = "wiz1progress" Then stopwizcounter = 2
		If SplashEffect = "wiz5progress" Then stopwizcounter = 2

		SplashEffect = ""
		FlexDMD.Stage.GetLabel("Text6").Text = " "
		FlexDMD.Stage.GetLabel("Text7").Text = " "	
		FlexDMD.Stage.GetLabel("Text6").visible = False
		FlexDMD.Stage.GetLabel("Text7").visible = False
		If Not bInstantInfo Then If SplashImage  > 0 and SplashImage  < 88 Then FlexDMD.Stage.GetImage("Nightmare" & SplashImage ).visible = False	
		If SplashImage2 > 0 and SplashImage2 < 88 Then FlexDMD.Stage.GetImage("Nightmare" & SplashImage2 ).visible = False	
		If SplashImage3 > 0 and SplashImage3 < 88 Then FlexDMD.Stage.GetImage("Nightmare" & SplashImage3 ).visible = False	
		FlexDMD.Stage.GetImage("Nightmare20").visible = False	
		FlexDMD.Stage.GetImage("Nightmare21").visible = False	
		FlexDMD.Stage.GetImage("Nightmare22").visible = False	
		FlexDMD.Stage.GetImage("Nightmare23").visible = False	
		FlexDMD.Stage.GetImage("Nightmare24").visible = False	
		FlexDMD.Stage.GetImage("Nightmare25").visible = False	
		FlexDMD.Stage.GetImage("Nightmare26").visible = False	
		FlexDMD.Stage.GetImage("Nightmare27").visible = False	
		FlexDMD.Stage.GetImage("Nightmare28").visible = False	
		FlexDMD.Stage.GetImage("Nightmare29").visible = False	
		FlexDMD.Stage.GetImage("Nightmare30").visible = False	
		FlexDMD.Stage.GetImage("Nightmare46").visible = False	
		FlexDMD.Stage.GetImage("Nightmare47").visible = False	
		FlexDMD.Stage.GetImage("Nightmare48").visible = False	
		FlexDMD.Stage.GetImage("Nightmare49").visible = False	
		FlexDMD.Stage.GetImage("Nightmare50").visible = False	
		FlexDMD.Stage.GetImage("Nightmare51").visible = False	
		FlexDMD.Stage.GetImage("Nightmare52").visible = False	
		FlexDMD.Stage.GetImage("Nightmare53").visible = False	
		FlexDMD.Stage.GetImage("Nightmare54").visible = False	
		FlexDMD.Stage.GetImage("Nightmare55").visible = False	
		FlexDMD.Stage.GetImage("Nightmare19").visible = False	
		FlexDMD.Stage.GetImage("Nightmare24").visible = False	
		FlexDMD.Stage.GetImage("Nightmare27").visible = False	
		FlexDMD.Stage.GetImage("Nightmare60").visible = False
		FlexDMD.Stage.GetImage("Nightmare61").visible = False
		FlexDMD.Stage.GetImage("Nightmare62").visible = False
		FlexDMD.Stage.GetImage("Nightmare63").visible = False
		FlexDMD.Stage.GetImage("Nightmare64").visible = False
		FlexDMD.Stage.GetImage("Nightmare65").visible = False
		FlexDMD.Stage.GetImage("Nightmare66").visible = False
		ballsavecounter = 2
		DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.z9").Bitmap
		DMDScene.GetImage("Last").visible = False	
		DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.z9").Bitmap
		DMDScene.GetImage("Last2").visible = False	
		DMDScene.GetLabel("Lasttext1").visible = False
		FlexDMD.Stage.GetImage("gameover1").visible = False
		revivecounter = 0
		NextSplashQ
End Sub
Dim BallsaveCounter : BallsaveCounter = 0
Dim revivecounter : revivecounter = 0
Dim GameoverCounter : GameoverCounter = 0
Dim wizlastframe : wizlastframe = array (0,23,21,30,19,22,22)
Dim wizframes : wizframes = array("","zWizA","zwizardC","zwizardF","zWizD","zWizE","zwizardG")
Sub DMD_showsplash
	dim tmp
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,170 , "ballsave" , 500,0,0
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,150 , "revive" , 500,0,0
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,150 , "balllost" , 500,0,0
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,230 , "gameover" , 500,0,0
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,25522 , "skillshot" , 500,0,0
	If bInstantInfo And SplashBlink < 3 And Splashimage < 88 Then SplashBlink = 3 : FlexDMD.Stage.GetImage("Nightmare" & SplashImage).visible = True 
	If SplashBlink < 3 Then BallsaveCounter = 10 : revivecounter = 0 : GameoverCounter = 1 
	If SplashBlink < 3 And Splashimage < 88 Then SplashBlink = 3 : FlexDMD.Stage.GetImage("Nightmare" & SplashImage).visible = True 
	SplashBlink = SplashBlink + 1
	
	If SplashBlink > SplashTimer Then StopSplash : Exit Sub

	Select Case SplashEffect


		Case "addaball"
			If SplashBlink = 10 Then
					DMDScene.GetImage("Last").visible = True
					DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zRedjack1").Bitmap	
					DMDScene.GetLabel("Lasttext1").visible = True
					DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
					DMDScene.GetLabel("Lasttext1").text = "AWARD ADDABALL"
					DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
			End If

		Case "megaspinner"
			If SplashBlink = 10 Then PlaySound "_spring"	,1,BackboxVolume /3																																'idig  delayed by flashing
			DMDScene.GetImage("Last").visible = True
			If SplashBlink < 70 Then
				If SplashBlink mod  6 = 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zMegaspinner1").Bitmap
				If SplashBlink mod  6 = 4 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zMegaspinner2").Bitmap
			Else
				If SplashBlink = 73 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zMegaspinner3").Bitmap
			End If

		Case "jackaward1"
		    If SplashBlink = 10 Then PlaySound"_a_reward" ,1,BackboxVolume																																'idig  jackaward1
			DMDScene.GetImage("Last").visible = True 
			If SplashBlink < 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward1").Bitmap
			If SplashBlink mod 15 = 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward1").Bitmap
			If SplashBlink mod 15 = 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward4").Bitmap
		Case "jackaward2" 																															
			If SplashBlink = 10 Then PlaySound"_jacklaugh",1,BackboxVolume																																'idig jackaward2
			DMDScene.GetImage("Last").visible = True 
			If SplashBlink < 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward1").Bitmap
			If SplashBlink mod 15 = 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward1").Bitmap
			If SplashBlink mod 15 = 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward5").Bitmap
		Case "jackawardeb"
			DMDScene.GetImage("Last").visible = True : 
			If SplashBlink < 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward1").Bitmap
			If SplashBlink mod 15 = 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward1").Bitmap
			If SplashBlink mod 15 = 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward2").Bitmap
		Case "jackawardmission"																														
			If SplashBlink = 10 Then PlaySound"_JackThankEveryone",1,BackboxVolume																														'idig  jackawardmission
			DMDScene.GetImage("Last").visible = True 
			If SplashBlink < 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward1").Bitmap
			If SplashBlink < 84 then
				If SplashBlink mod 15 = 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward1").Bitmap
				If SplashBlink mod 15 = 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward3").Bitmap
			Else
				If SplashBlink = 88 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackaward6").Bitmap
			End If
		Case "extraball"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zWizB1").Bitmap	
			If SplashBlink mod 7 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 14 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zWizB" & revivecounter).Bitmap
 			End If
			Select Case SplashBlink
				case 15 : DMDScene.GetImage("Last2").visible = True
					DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zExtraball1").Bitmap
				case 23 : DMDScene.GetImage("Last2").visible = True
					DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zExtraball2").Bitmap
				case 31 : DMDScene.GetImage("Last2").visible = True
					DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zExtraball3").Bitmap
				case 39 : DMDScene.GetImage("Last2").visible = True
					DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zExtraball4").Bitmap
				case 47 : DMDScene.GetImage("Last2").visible = True
					DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zExtraball5").Bitmap
				case 55 : DMDScene.GetImage("Last2").visible = True
					DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zExtraball6").Bitmap
				case 63 : DMDScene.GetImage("Last2").visible = True
					DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zExtraball7").Bitmap
				case 71 : DMDScene.GetImage("Last2").visible = True
					DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zExtraball8").Bitmap
			End Select

			If SplashBlink > 80 Then
				If SplashBlink mod 15 = 2 Then DMDScene.GetImage("Last2").visible = False
				If SplashBlink mod 15 = 5 Then DMDScene.GetImage("Last2").visible = True
			End If


		Case "wakethedead"
			If SplashBlink = 10 Then PlaySound "sfx_scream",1,BackboxVolume  'idig delayed by flashing
			DMDScene.GetImage("Last").visible = True 
			If SplashBlink < 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zWakethedead1").Bitmap 
			If SplashBlink mod 3 = 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zWakethedead" & RndInt(1,4)).Bitmap

		Case "targets2"		' ebislit
			DMDScene.GetImage("Last").visible = True
			If SplashBlink < 6 Then
				SplashBlink = 6
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zCompleteTargets7").Bitmap
				PlayVoice "vo_extraballislit" 
				Playsound"_achieve-001",1,BackboxVolume
			End If
			If splashblink mod 15 = 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zCompleteTargets7").Bitmap
			If splashblink mod 15 = 9 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zCompleteTargets8").Bitmap

'DMD_StartSplash " ","  ",FontHugeOrange, FontHugeRedhalf,120 , "targets1" ,555,0,0
'If MegaSpinner_started(CurrentPlayer) < 5 Then DMD_StartSplash " ","  ",FontHugeOrange, FontHugeRedhalf,120 , "targets2" ,555,0,0

		Case "targets1" 	'less than 5
			DMDScene.GetImage("Last").visible = True : If SplashBlink < 6 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zCompleteTargets1").Bitmap
			Select Case MegaSpinner_started(CurrentPlayer)
				Case 1
					If splashblink mod 15 = 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zCompleteTargets2").Bitmap
					If splashblink mod 15 = 9 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zCompleteTargets1").Bitmap
				Case 2
					If splashblink mod 15 = 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zCompleteTargets3").Bitmap
					If splashblink mod 15 = 9 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zCompleteTargets1").Bitmap
				Case 3
					If splashblink mod 15 = 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zCompleteTargets4").Bitmap
					If splashblink mod 15 = 9 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zCompleteTargets1").Bitmap
				Case 4
					If splashblink mod 15 = 5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zCompleteTargets5").Bitmap
					If splashblink mod 15 = 9 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zCompleteTargets6").Bitmap
			End Select



		Case "wiz8start"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(6) & "1").Bitmap
			If SplashBlink mod 3 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > wizlastframe(6) Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(6) & revivecounter).Bitmap
 			End If

			If SplashBlink = 25 Then
				DMDScene.GetImage("Last2").visible = True
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zWizscoopmega").Bitmap
			End If
			If SplashBlink = 120 Then
				DMDScene.GetImage("Last2").visible = False
			End If
		Case "wiz8progress"
			DMDScene.GetImage("Last").visible = True
			If SplashBlink mod 2 = 1 Then
				DMDScene.GetImage("Last").visible = True
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(6) & "1").Bitmap	
			End If
			If SplashBlink = 20 Then PlayVoice "vo_openitupquickly"
			If SplashBlink mod 25 =  5 Then	DMDScene.GetImage("Last2").visible = True : DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zWizmegatwentyfive2").Bitmap
			If SplashBlink mod 25 = 15 Then	DMDScene.GetImage("Last2").visible = True : DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zWizmegatwentyfive1").Bitmap

		Case "wiz7start"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & "1").Bitmap
			If SplashBlink mod 3 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > wizlastframe(WizardBackground) Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & revivecounter).Bitmap
 			End If

			If SplashBlink = 25 Then
				DMDScene.GetImage("Last2").visible = True
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zWiztwosuperjp").Bitmap
			End If
			If SplashBlink = 120 Then
				DMDScene.GetImage("Last2").visible = False
			End If

		Case "wiz7progress"
			If SplashBlink mod 25 = 7 Then
				DMDScene.GetImage("Last2").visible = False
			Elseif SplashBlink mod 25 = 8 Then 
				DMDScene.GetImage("Last2").visible = True
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zWizonesuperjp").Bitmap	
			End If
			If SplashBlink mod 2 = 1 Then
				DMDScene.GetImage("Last").visible = True
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & "1").Bitmap	
			End If
			If SplashBlink = 20 Then PlayVoice "vo_hailtothepumkinking"
		Case "wiz6start"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & "1").Bitmap	
			If SplashBlink mod 3 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > wizlastframe(WizardBackground) Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & revivecounter).Bitmap
 			End If

			If SplashBlink = 25 Then
				PlaySound "vo_bigbellrings",1,BackboxVolume
				DMDScene.GetImage("Last2").visible = True
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zWizcollectlights").Bitmap
			End If
			If SplashBlink = 120 Then
				DMDScene.GetImage("Last2").visible = False
			End If


		Case "wiz6progress"
			If SplashBlink mod 40 = 37 Then
				DMDScene.GetLabel("Lasttext1").visible = False
			Else
				DMDScene.GetImage("Last").visible = True
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & "1").Bitmap	
				DMDScene.GetLabel("Lasttext1").visible = True
				DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
				DMDScene.GetLabel("Lasttext1").text = "COLLECT " & 17-WizardProgress  & " LIT LIGHTS"
				DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
			End If


		Case "wiz5start"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & "1").Bitmap
			If SplashBlink mod 3 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > wizlastframe(WizardBackground) Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & revivecounter).Bitmap
 			End If

			If SplashBlink = 25 Then
				DMDScene.GetImage("Last2").visible = True
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zWizswitchstext").Bitmap
			End If
			If SplashBlink = 120 Then
				DMDScene.GetImage("Last2").visible = False
			End If


		Case "wiz5progress"
			If SplashBlink mod 40 = 37 Then
				DMDScene.GetLabel("Lasttext1").visible = False
			Else
				DMDScene.GetImage("Last").visible = True
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zRedjack" & (WizardProgress mod 20) + 1).Bitmap	
				DMDScene.GetLabel("Lasttext1").visible = True
				DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
				DMDScene.GetLabel("Lasttext1").text = "HIT " & 150-WizardProgress  & " SWITCHES"
				DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
			End If

		Case "wiz4progress"
			If SplashBlink mod 25 = 7 Then
				DMDScene.GetImage("Last2").visible = False
			Elseif SplashBlink mod 25 = 8 Then 
				DMDScene.GetImage("Last2").visible = True
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zWizonesuperjp").Bitmap	
			End If
			If SplashBlink mod 2 = 1 Then
				DMDScene.GetImage("Last").visible = True
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & "1").Bitmap	
			End If
			If SplashBlink = 20 Then PlayVoice "vo_hailtothepumkinking"

		Case "wiz4start"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & "1").Bitmap
			If SplashBlink mod 3 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > wizlastframe(WizardBackground) Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & revivecounter).Bitmap
 			End If

			If SplashBlink = 25 Then
				DMDScene.GetImage("Last2").visible = True
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zWiztwosuperjp").Bitmap
			End If
			If SplashBlink = 120 Then
				DMDScene.GetImage("Last2").visible = False
			End If	


		Case "wiz3start"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & "1").Bitmap
			If SplashBlink mod 3 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > wizlastframe(WizardBackground) Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & revivecounter).Bitmap
 			End If

			If SplashBlink = 25 Then
				PlayVoice "vo_sayitoncesayittwise"
				DMDScene.GetImage("Last2").visible = True
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zWizfinishtenramps").Bitmap
			End If
			If SplashBlink = 120 Then
				DMDScene.GetImage("Last2").visible = False
			End If

		Case "wiz3progress"
			If SplashBlink mod 40 = 37 Then
				DMDScene.GetLabel("Lasttext1").visible = False
			Else
				DMDScene.GetImage("Last").visible = True
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & "1").Bitmap	
				DMDScene.GetLabel("Lasttext1").visible = True
				DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
				DMDScene.GetLabel("Lasttext1").text = "FINISH " & 10-WizardProgress  & " RAMPS"
				DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
			End If



		Case "wiz2start"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & "1").Bitmap	
			If SplashBlink mod 3 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > wizlastframe(WizardBackground) Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & revivecounter).Bitmap
 			End If

			If SplashBlink = 25 Then
				PlaySound "vo_bigbellrings",1,BackboxVolume
				DMDScene.GetImage("Last2").visible = True
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zWizcollectlights").Bitmap
			End If
			If SplashBlink = 120 Then
				DMDScene.GetImage("Last2").visible = False
			End If


		Case "wiz2progress"
			If SplashBlink mod 40 = 37 Then
				DMDScene.GetLabel("Lasttext1").visible = False
			Else
				DMDScene.GetImage("Last").visible = True
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX." & wizframes(WizardBackground) & "1").Bitmap	
				DMDScene.GetLabel("Lasttext1").visible = True
				DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
				DMDScene.GetLabel("Lasttext1").text = "COLLECT " & 17-WizardProgress  & " LIT LIGHTS"
				DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
			End If


		Case "wiz1start"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zRedjack1").Bitmap	
			If SplashBlink mod 3 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 20 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zRedjack" & revivecounter).Bitmap
			End If

			If SplashBlink = 50 Then
				DMDScene.GetImage("Last2").visible = True
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zWizStarttext").Bitmap
			End If
			If SplashBlink = 140 Then
				DMDScene.GetImage("Last2").visible = False
			End If


		Case "wiz1progress"
			If SplashBlink mod 40 = 37 Then
				DMDScene.GetLabel("Lasttext1").visible = False
			Else
				DMDScene.GetImage("Last").visible = True
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zRedjack" & (WizardProgress mod 20) + 1).Bitmap	
				DMDScene.GetLabel("Lasttext1").visible = True
				DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
				DMDScene.GetLabel("Lasttext1").text = "HIT " & 100-WizardProgress  & " SWITCHES"
				DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
			End If

' 3	whats this	  2 red gifs					 3-	all 7 top lanes 
' progress+finish = 1-20 zWhatsthisA16
' zWhatsthisb1-25 + letme out = start "vo_letmeout
' whatathis sound for whats this progress ...  letmeout for start ? vo_whosnextonmylist for finish
'DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,120 , "whatsthisstart" ,999,0,0
		Case "whatsthisstart"	
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zWhatsthisb1").Bitmap : PlayVoice "vo_letmeout"	     'idig
			If SplashBlink mod 4 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 25 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zWhatsthisb" & revivecounter).Bitmap	
				DMDScene.GetLabel("Lasttext1").visible = False
			Else
			DMDScene.GetLabel("Lasttext1").visible = True
			DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
			DMDScene.GetLabel("Lasttext1").text = "WHATS THIS"
			DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
			End If

		Case "whatsthisprogress"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zWhatsthisA1").Bitmap	
			If SplashBlink mod 4 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 20 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zWhatsthisA" & revivecounter).Bitmap	
				DMDScene.GetLabel("Lasttext1").visible = False
			Else
				DMDScene.GetLabel("Lasttext1").visible = True
				DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
				DMDScene.GetLabel("Lasttext1").text = "GET " & 7-ModeProgress & " BLUE LIGHTS"
				DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
			End If


		Case "whatsthisfinish"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zWhatsthisA1").Bitmap   
			If SplashBlink mod 4 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 20 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zWhatsthisA" & revivecounter).Bitmap	
				DMDScene.GetLabel("Lasttext1").visible = False
			Else
				DMDScene.GetLabel("Lasttext1").visible = True
				DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
				DMDScene.GetLabel("Lasttext1").text = "WHATS THIS COMPLETE"
				DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
			End If




		Case "townmeetingstart"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zTownMeetingA1").Bitmap : PlaySound "vo_townmeetingtonight",1,BackboxVolume	    'idig
			If SplashBlink mod 4 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 26 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zTownMeetingA" & revivecounter).Bitmap	
				DMDScene.GetLabel("Lasttext1").visible = False
			Else
			DMDScene.GetLabel("Lasttext1").visible = True
			DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
			DMDScene.GetLabel("Lasttext1").text = "TOWNMEETING"
			DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
			End If

		Case "townmeetingprogress"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zTownMeetingB1").Bitmap	
			If SplashBlink mod 4 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 22 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zTownMeetingB" & revivecounter).Bitmap	
				DMDScene.GetLabel("Lasttext1").visible = False
			Else
				DMDScene.GetLabel("Lasttext1").visible = True
				DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
				DMDScene.GetLabel("Lasttext1").text = "GET " & 7-ModeProgress & " BLUE LIGHTS"
				DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
			End If


		Case "townmeetingfinish"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zTownMeetingB1").Bitmap	
			If SplashBlink mod 4 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 22 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zTownMeetingB" & revivecounter).Bitmap	
				DMDScene.GetLabel("Lasttext1").visible = False
			Else

				DMDScene.GetLabel("Lasttext1").visible = True
				DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
				DMDScene.GetLabel("Lasttext1").text = "TOWNMEETING OVER"
				DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
				
			End If


		Case "bequickstart"
			DMDScene.GetImage("Last").visible = True
			DMDScene.GetImage("Last2").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick1").Bitmap : PlayVoice "vo_bequick-008"    'idig

			If SplashBlink mod 4 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 26 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick" & revivecounter).Bitmap	
			End If

			If SplashBlink mod 15 < 3 Then
				DMDScene.GetImage("Last2").visible = False
			Else
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick27").Bitmap	
			End If


		Case "bequickprogress"
			
			DMDScene.GetImage("Last").visible = True
			DMDScene.GetImage("Last2").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick1").Bitmap	

			If SplashBlink mod 4 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 26 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick" & revivecounter).Bitmap	
			End If

			If SplashBlink mod 15 < 3 Then
				DMDScene.GetImage("Last2").visible = False
			Else
				Select Case ModeProgress
					Case 1 : DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick28").Bitmap	
					Case 2 : DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick29").Bitmap	
					Case 3 : DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick30").Bitmap	
					Case 4 : DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick31").Bitmap	
					Case 5 : DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick32").Bitmap	
					Case 6 : DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick33").Bitmap	
				End Select
			End If



		Case "bequickfinish"
			DMDScene.GetImage("Last").visible = True
			DMDScene.GetImage("Last2").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick1").Bitmap	
			If SplashBlink mod 4 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 26 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick" & revivecounter).Bitmap	
			End If

			If SplashBlink mod 15 < 3 Then
				DMDScene.GetImage("Last2").visible = False
			Else
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zJackbequick34").Bitmap	
			End If



		Case "nimblestart"
			
			DMDScene.GetImage("Last").visible = True
			DMDScene.GetImage("Last2").visible = True
			DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zNimble7").Bitmap 
	'		If SplashBlink = 10 Then Playsfx ""
			If SplashBlink mod 40 < 5 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zNimble3").Bitmap	
			Elseif SplashBlink mod 40 < 20 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zNimble4").Bitmap			
			Elseif SplashBlink mod 40 < 25 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zNimble3").Bitmap			
			Else 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zNimble5").Bitmap			
			End If

		Case "nimbleprogress"
			DMDScene.GetImage("Last").visible = True
			If SplashBlink mod 8 < 2 Then
				DMDScene.GetImage("Last2").visible = False
			Else
				DMDScene.GetImage("Last2").visible = True
				DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zNimble6").Bitmap
			End If

			If SplashBlink mod 40 < 5 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zNimble3").Bitmap	
			Elseif SplashBlink mod 40 < 20 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zNimble4").Bitmap			
			Elseif SplashBlink mod 40 < 25 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zNimble3").Bitmap			
			Else 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zNimble5").Bitmap			
			End If

		Case "nimblefinish"
			DMDScene.GetImage("Last").visible = True
			If SplashBlink mod 26 < 13 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zNimble1").Bitmap	
			Else 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zNimble2").Bitmap	
				
			End If

' 1+2 complete + sound
'20	x?? workingonit 	Jack be nimble	8 combos    images made



		Case "80daysstart" '12345
			DMDScene.GetImage("Last").visible = True
			
			If SplashBlink mod 80 < 16 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zDaysleftbg1").Bitmap			
			Elseif SplashBlink mod 80 < 32 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zDaysleftbg2").Bitmap			
			Elseif SplashBlink mod 80 < 48 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zDaysleftbg3").Bitmap			
			Elseif SplashBlink mod 80 < 64 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zDaysleftbg4").Bitmap			
			Else 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zDaysleftbg5").Bitmap			
			End If

		Case "80dayscomplete" '678
			If SplashBlink = 10 Then
				PlaySound "_merryxmasjack" ,1,BackboxVolume'idig
				PlaySound "sfx_crowdcheer",1,BackboxVolume/3.45 'idig
				ChangeSong				   'idig
			End If
			DMDScene.GetImage("Last").visible = True
			
			
			
			If SplashBlink mod 60 < 30 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zDaysleftbg6").Bitmap			
			Elseif SplashBlink mod 60 < 45 Then 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zDaysleftbg7").Bitmap			
			Else
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zDaysleftbg8").Bitmap			
			End If

		Case "80daysupdate"
			If SplashBlink mod 40 = 37 Then
				DMDScene.GetImage("Last").visible = False
				DMDScene.GetLabel("Lasttext1").visible = False
			Else
				DMDScene.GetImage("Last").visible = True
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zDaysleftbg").Bitmap	
				DMDScene.GetLabel("Lasttext1").visible = True
				DMDScene.GetLabel("Lasttext1").font = FontHugeOrange
				DMDScene.GetLabel("Lasttext1").text = "" & 80-ModeProgress & " DAYS LEFT"
				DMDScene.GetLabel("Lasttext1").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
			End If
	

		Case "skillshot"
			DMDScene.GetImage("Last").visible = True
			If SplashBlink mod 5 = 1 Then 
				revivecounter = revivecounter + 1
				If bSkillshotReady = False And revivecounter < 40 Then revivecounter = 50
				Select Case revivecounter
					case 1 :  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot1").Bitmap
					case 2 :  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot2").Bitmap
					case 3 :  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot3").Bitmap
					case 4 :  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot4").Bitmap
					case 5 :  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot5").Bitmap
					Case 8 :  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot6").Bitmap
					Case 12:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot7").Bitmap
					Case 13:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot8").Bitmap
					Case 14:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot9").Bitmap
					Case 15:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot10").Bitmap
					Case 16:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot11").Bitmap
					Case 17:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot6").Bitmap
					Case 21:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot7").Bitmap
					Case 22:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot8").Bitmap
					Case 23:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot9").Bitmap
					Case 24:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot10").Bitmap
					Case 25:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot11").Bitmap
					Case 26:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot6").Bitmap
					Case 30:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot7").Bitmap
					Case 31:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot8").Bitmap
					Case 32:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot9").Bitmap
					Case 33:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot10").Bitmap
					Case 34:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot11").Bitmap

					case 35:  If PlayersPlayingGame = 1 Then revivecounter = 7
							  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot5").Bitmap

					case 38:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot" & (CurrentPlayer + 11)).Bitmap
					Case 41:  DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot5").Bitmap : revivecounter = 5

					Case 50 : DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot5").Bitmap
					Case 51 : DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot4").Bitmap
					Case 52 : DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot3").Bitmap
					Case 53 : DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot2").Bitmap
					Case 54 : DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSkillshot1").Bitmap : revivecounter = 0 : StopSplash : StartPresentsMode
				End Select
			End If
		Case "nightmare"
			If SplashBlink Mod 5 = 1 Then revivecounter = revivecounter + 1 : If revivecounter > 14 Then revivecounter = 1
			DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zNightmareMB" & revivecounter + 2 ).Bitmap
			DMDScene.GetImage("Last").visible = True
			Select Case SplashBlink mod 30
				Case 1 : DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zNightmareMB1" ).Bitmap : DMDScene.GetImage("Last2").visible = True
				Case 12: DMDScene.GetImage("Last2").visible = False
				Case 16: DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zNightmareMB2" ).Bitmap : DMDScene.GetImage("Last2").visible = True
				Case 27: DMDScene.GetImage("Last2").visible = False
			End Select


		Case "gameover" ' 1 +2til13
			DMDScene.GetImage("Last").visible = True
			If SplashBlink < 4 Then 
				SplashBlink = 4
				FlexDMD.Stage.GetImage("gameover1").visible = True
				FlexDMD.Stage.GetImage("gameover1").setposition 0,-50
			End If

			tmp = 1
			If FlexDMDHighQuality Then tmp = 2
			If SplashBlink mod 2 = 1 Then
				revivecounter = revivecounter + 1
				FlexDMD.Stage.GetImage("gameover1").setposition 0,-revivecounter * tmp - 50
			End If
			If SplashBlink mod 2 = 1 Then
				GameoverCounter = GameoverCounter + 1
				If GameoverCounter < 14 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGameover" & GameoverCounter ).Bitmap
				If GameoverCounter = 14 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGameover2").Bitmap
				If GameoverCounter > 20 Then GameoverCounter = 1
			End If

		Case "balllost"' 1 til 10 + 10+11
			DMDScene.GetImage("Last").visible = True
			If revivecounter < 9 Then
				If SplashBlink mod 5 = 2 Then
					revivecounter = revivecounter + 1
					DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zBalllost" &  revivecounter).Bitmap
				End If
			Else
				If SplashBlink mod 12 = 2 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zBalllost10").Bitmap
				If SplashBlink mod 12 = 8 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zBalllost11").Bitmap
			End If

		Case "ballsave"' 10 til 2 og 1+2
			DMDScene.GetImage("Last").visible = True
			If SplashBlink mod 5 = 2 And BallsaveCounter > 2 Then
				BallsaveCounter = BallsaveCounter - 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zBallsave" &  BallsaveCounter ).Bitmap
			Elseif SplashBlink mod 12 = 2 And BallsaveCounter < 3 Then
					DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zBallsave1").Bitmap
			Elseif SplashBlink mod 12 = 8  And BallsaveCounter < 3 Then
					DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zBallsave2").Bitmap
			End If

		Case "revive"
			DMDScene.GetImage("Last").visible = True
			If SplashBlink mod 5 = 2 Then
				revivecounter = revivecounter + 1
				If revivecounter = 7 Then revivecounter = 1 
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zRevive" &  revivecounter ).Bitmap
			End If
		Case "workers"
			DMDScene.GetImage("Last").visible = True
			If SplashBlink mod 95 = 15 Then
				revivecounter = revivecounter + 1
'				If revivecounter = 4 Then revivecounter = 1
				Select Case revivecounter
				Case 1 : DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.z_joepicasso").Bitmap
				BlinkAllPresents 1,1
				Case 2 : DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.z_idigstuff").Bitmap
				BlinkAllPresents 1,1
				Case 3 : DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.z_tastywasps").Bitmap
				BlinkAllPresents 1,1
				Case 4 : DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.z_oqqsan").Bitmap
				BlinkAllPresents 1,1
				Case 5 : StopSplash : DMD_counter = 999
				End Select
			End If

		Case "deliverpresent"
			Select Case SplashBlink mod 30   ' 45=no 46=rigfht 47=left pos
	
				Case 5,6,7,8,9,10
					FlexDMD.Stage.GetImage("Nightmare47").visible = True 	
					FlexDMD.Stage.GetImage("Nightmare46").visible = False 	
				Case 20,21,22,23,24,25
					FlexDMD.Stage.GetImage("Nightmare48").visible = True 	
					FlexDMD.Stage.GetImage("Nightmare46").visible = False 	
				Case else :
					FlexDMD.Stage.GetImage("Nightmare46").visible = True 
					FlexDMD.Stage.GetImage("Nightmare47").visible = False 	
					FlexDMD.Stage.GetImage("Nightmare48").visible = False	
			End Select


		Case "sandyclaws"
			DMDScene.GetImage("Last").visible = True 
			Select Case ModeProgress
				Case 0  ' intro
					If SplashBlink mod 120 < 60 Then
						DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws10").Bitmap
					Else
						DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws11").Bitmap
					End If
				case 1
					If SplashBlink mod 25= 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws3").Bitmap
					If SplashBlink mod 25= 12 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws4").Bitmap
				case 2
					If SplashBlink mod 25= 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws3").Bitmap
					If SplashBlink mod 25= 12 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws5").Bitmap
				case 3
					If SplashBlink mod 25= 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws3").Bitmap
					If SplashBlink mod 25= 12 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws6").Bitmap
				case 4
					If SplashBlink mod 25= 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws3").Bitmap
					If SplashBlink mod 25= 12 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws7").Bitmap
				case 5
					If SplashBlink mod 25= 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws3").Bitmap
					If SplashBlink mod 25= 12 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws8").Bitmap
				case 6
					If SplashBlink mod 25= 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws3").Bitmap
					If SplashBlink mod 25= 12 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws9").Bitmap
			End Select

		case "sandyclaws2"
			DMDScene.GetImage("Last").visible = True 
			If SplashBlink mod 25= 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws1").Bitmap
			If SplashBlink mod 25= 12 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSandyclaws2").Bitmap




		Case "savechristmas2"
			DMDScene.GetImage("Last").visible = True
			DMDScene.GetImage("Last2").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas1").Bitmap : revivecounter = 1 : SplashBlink = 3 : DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas1").Bitmap : savexmascallout:    'idig 
			If SplashBlink mod 4 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 19 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas" & revivecounter ).Bitmap
			End If
			If SplashBlink mod 20 = 8  Then DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas22").Bitmap
			If SplashBlink mod 20 = 18 Then DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas23").Bitmap

		Case "savechristmas"
		
			DMDScene.GetImage("Last").visible = True
			DMDScene.GetImage("Last2").visible = True 
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas1").Bitmap : revivecounter = 1 : SplashBlink = 3 : DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas1").Bitmap : savexmascallout   'idig
			If SplashBlink mod 4 = 1 Then
				revivecounter = revivecounter + 1
				If revivecounter > 19 Then revivecounter = 1
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas" & revivecounter ).Bitmap
			End If
			Select Case ModeProgress
				Case 0
						If SplashBlink mod 20 = 8  Then DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas20").Bitmap
						If SplashBlink mod 20 = 18 Then DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas21").Bitmap
				Case 1,3,5,7
						If SplashBlink mod 20 = 8  Then DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas26").Bitmap 
						If SplashBlink mod 20 = 18 Then DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas27").Bitmap
				case 2,4,6
						If SplashBlink mod 20 = 8  Then DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas24").Bitmap 
						If SplashBlink mod 20 = 18 Then DMDScene.GetImage("Last2").Bitmap = FlexDMD.NewImage("", "VPX.zSavechristmas25").Bitmap
				End Select


		case "raisethedead2"
			DMDScene.GetImage("Last").visible = True
			If revivecounter = 0 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zRaisethedead1").Bitmap : revivecounter = 1 : SplashBlink = 3
			If SplashBlink mod 10 = 5 Then
				revivecounter = revivecounter + 1
				If revivecounter > 23 Then StopSplash : Exit Sub
				If revivecounter < 21 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zRaisethedead" & revivecounter ).Bitmap
			End If

		Case "raisethedead1"
			DMDScene.GetImage("Last").visible = True 
			if SplashBlink mod 26 < 13 Then
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard5").Bitmap
			Else
				DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard6").Bitmap 
			End If

		Case "raisethedead"
			DMDScene.GetImage("Last").visible = True
			 If SplashBlink = 10 then shootthetargets
			Select Case ModeProgress
				Case 0  ' intro
					If SplashBlink mod 40=  5 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard7").Bitmap 
					If SplashBlink mod 40= 10 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard8").Bitmap
					If SplashBlink mod 40= 25 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard9").Bitmap
					If SplashBlink mod 40= 30 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard8").Bitmap
				case 1
					If SplashBlink mod 25= 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard0").Bitmap
					If SplashBlink mod 25= 12 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard1").Bitmap
				case 2
					If SplashBlink mod 25= 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard1").Bitmap
					If SplashBlink mod 25= 12 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard2").Bitmap
				case 3
					If SplashBlink mod 25= 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard2").Bitmap
					If SplashBlink mod 25= 12 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard3").Bitmap
				case 4
					If SplashBlink mod 25= 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard3").Bitmap
					If SplashBlink mod 25= 12 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard4").Bitmap
				case 5
					If SplashBlink mod 25= 1 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard4").Bitmap
					If SplashBlink mod 25= 12 Then DMDScene.GetImage("Last").Bitmap = FlexDMD.NewImage("", "VPX.zGraveyard5").Bitmap
			End Select


		Case "blink2images"
			If SplashBlink mod 25= 1 Then  FlexDMD.Stage.GetImage("Nightmare" & SplashImage).visible = True		: FlexDMD.Stage.GetImage("Nightmare" & SplashImage2).visible = False	
			If SplashBlink mod 25= 12 Then  FlexDMD.Stage.GetImage("Nightmare" & SplashImage2).visible = True	: FlexDMD.Stage.GetImage("Nightmare" & SplashImage).visible = False	
		Case "blink2fast1"
			If SplashBlink mod 25= 1 Then  FlexDMD.Stage.GetImage("Nightmare" & SplashImage).visible = True		: FlexDMD.Stage.GetImage("Nightmare" & SplashImage2).visible = False	
			If SplashBlink mod 25= 7 Then  FlexDMD.Stage.GetImage("Nightmare" & SplashImage2).visible = True	: FlexDMD.Stage.GetImage("Nightmare" & SplashImage).visible = False	

		Case "blink2text1"
			If SplashBlink mod 25= 1 Then  FlexDMD.Stage.GetImage("Nightmare" & SplashImage).visible = True		: FlexDMD.Stage.GetImage("Nightmare" & SplashImage2).visible = False	
			If SplashBlink mod 25= 12 Then  FlexDMD.Stage.GetImage("Nightmare" & SplashImage2).visible = True	: FlexDMD.Stage.GetImage("Nightmare" & SplashImage).visible = False	
			FlexDMD.Stage.GetLabel("Text7").Font = Splashfont2
			FlexDMD.Stage.GetLabel("Text7").Text = Splashtext2
			FlexDMD.Stage.GetLabel("Text7").SetAlignedPosition FlexSizeX/2,FlexSizey*3/4, FlexDMD_Align_center
			FlexDMD.Stage.GetLabel("Text7").visible = True	

		Case "jasonmb"
			If SplashBlink > 300 Then
				FlexDMD.Stage.GetImage("Nightmare24").visible = False	
				FlexDMD.Stage.GetImage("Nightmare27").visible = False	
				FlexDMD.Stage.GetImage("Nightmare" & SplashImage2)
				FlexDMD.Stage.GetImage("Nightmare" & SplashImage)	
				FlushSplashQ
			Elseif SplashBlink < 150 Then
				If SplashBlink mod 15= 1 Then  FlexDMD.Stage.GetImage("Nightmare" & SplashImage).visible = True		: FlexDMD.Stage.GetImage("Nightmare" & SplashImage2).visible = False	
				If SplashBlink mod 15= 6 Then  FlexDMD.Stage.GetImage("Nightmare" & SplashImage2).visible = True	: FlexDMD.Stage.GetImage("Nightmare" & SplashImage).visible = False	
			Else
				FlexDMD.Stage.GetImage("Nightmare" & SplashImage2).visible = False
				If SplashBlink mod 15= 1 Then  FlexDMD.Stage.GetImage("Nightmare24").visible = True	: FlexDMD.Stage.GetImage("Nightmare27").visible = False	
				If SplashBlink mod 15= 8 Then  FlexDMD.Stage.GetImage("Nightmare27").visible = True	: FlexDMD.Stage.GetImage("Nightmare24").visible = False				
			End If
		Case "oneimage"
				FlexDMD.Stage.GetLabel("Text7").Font = Splashfont2
				FlexDMD.Stage.GetLabel("Text7").Text = Splashtext2
				FlexDMD.Stage.GetLabel("Text7").SetAlignedPosition FlexSizeX/2,FlexSizey/3-FlexBorderOffsetY, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text7").visible = True
				FlexDMD.Stage.GetLabel("Text6").Font = Splashfont1
				FlexDMD.Stage.GetLabel("Text6").Text = Splashtext1
				FlexDMD.Stage.GetLabel("Text6").SetAlignedPosition FlexSizeX/2,FlexSizey*2/3+FlexBorderOffsetY, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text6").visible = True
		Case "oneimageINFO"
				InstantInfoTimer.interval = 1000
				InstantInfoTimer.enabled = False
				InstantInfoTimer.enabled = True
				SplashEffect = "oneimage"

		Case "blinkbot1"
			If SplashBlink mod  3 = 1 Then
				FlexDMD.Stage.GetLabel("Text6").Font = Splashfont2
				FlexDMD.Stage.GetLabel("Text6").Text = Splashtext2
				FlexDMD.Stage.GetLabel("Text6").SetAlignedPosition FlexSizeX/2,FlexSizey*2/3+FlexBorderOffsetY, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text6").visible = True
			Else
				FlexDMD.Stage.GetLabel("Text6").visible = False
			End If
			FlexDMD.Stage.GetLabel("Text7").Font = Splashfont1
			FlexDMD.Stage.GetLabel("Text7").Text = Splashtext1
			FlexDMD.Stage.GetLabel("Text7").SetAlignedPosition FlexSizeX/2,FlexSizey/3, FlexDMD_Align_center
			FlexDMD.Stage.GetLabel("Text7").visible = True

		Case "blinkbot2"
			If SplashBlink mod  3 = 1 Then
				FlexDMD.Stage.GetLabel("Text6").visible = False
			Else
				FlexDMD.Stage.GetLabel("Text6").Font = Splashfont2
				FlexDMD.Stage.GetLabel("Text6").Text = Splashtext2
				FlexDMD.Stage.GetLabel("Text6").SetAlignedPosition FlexSizeX/2,FlexSizey*2/3+FlexBorderOffsetY, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text6").visible = True
			End If
			FlexDMD.Stage.GetLabel("Text7").Font = Splashfont1
			FlexDMD.Stage.GetLabel("Text7").Text = Splashtext1
			FlexDMD.Stage.GetLabel("Text7").SetAlignedPosition FlexSizeX/2,FlexSizey/3, FlexDMD_Align_center
			FlexDMD.Stage.GetLabel("Text7").visible = True

		Case "blinktop1"
			If SplashBlink mod  3 = 1 Then
				FlexDMD.Stage.GetLabel("Text6").Font = Splashfont1
				FlexDMD.Stage.GetLabel("Text6").Text = Splashtext1
				FlexDMD.Stage.GetLabel("Text6").SetAlignedPosition FlexSizeX/2,FlexSizey/3, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text6").visible = True
			Else
				FlexDMD.Stage.GetLabel("Text6").visible = False
			End If
			FlexDMD.Stage.GetLabel("Text7").Font = Splashfont2
			FlexDMD.Stage.GetLabel("Text7").Text = Splashtext2
			FlexDMD.Stage.GetLabel("Text7").SetAlignedPosition FlexSizeX/2,FlexSizey*2/3+FlexBorderOffsetY, FlexDMD_Align_center
			FlexDMD.Stage.GetLabel("Text7").visible = True
		Case "insertcoin"
			If SplashBlink mod  6 > 1 Then
				FlexDMD.Stage.GetLabel("Text6").Font = Splashfont1
				FlexDMD.Stage.GetLabel("Text6").Text = "INSERT COIN"
				FlexDMD.Stage.GetLabel("Text6").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text6").visible = True
			Else
				FlexDMD.Stage.GetLabel("Text6").visible = False
			End If
		Case "credits"
			If SplashBlink mod  6 > 1 Then
				FlexDMD.Stage.GetLabel("Text6").Font = Splashfont1
				FlexDMD.Stage.GetLabel("Text6").Text = "CREDITS " & Credits
				FlexDMD.Stage.GetLabel("Text6").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text6").visible = True
			Else
				FlexDMD.Stage.GetLabel("Text6").Text = "CREDITS "
			End If
		Case "warning"
			If SplashBlink mod  6 > 1 Then
				FlexDMD.Stage.GetLabel("Text6").Font = Splashfont1
				FlexDMD.Stage.GetLabel("Text6").Text = "WARNING"
				FlexDMD.Stage.GetLabel("Text6").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text6").visible = True
			Else
				FlexDMD.Stage.GetLabel("Text6").Text = "  "
			End If
		Case "tilt"
			If SplashBlink mod  6 > 1 Then
				FlexDMD.Stage.GetLabel("Text6").Font = Splashfont1
				FlexDMD.Stage.GetLabel("Text6").Text = "YOU TILTED"
				FlexDMD.Stage.GetLabel("Text6").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text6").visible = True
			Else
				FlexDMD.Stage.GetLabel("Text6").Text = " "
			End If
		Case "blinktext"
			If SplashBlink mod  5 = 1 Then
				FlexDMD.Stage.GetLabel("Text6").Font = Splashfont1
				FlexDMD.Stage.GetLabel("Text6").Text = Splashtext1
				FlexDMD.Stage.GetLabel("Text6").SetAlignedPosition FlexSizeX/2,FlexSizey/3-FlexBorderOffsetY, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text6").visible = True
				FlexDMD.Stage.GetLabel("Text7").visible = False
			Else
				FlexDMD.Stage.GetLabel("Text6").visible = False
				FlexDMD.Stage.GetLabel("Text7").Font = Splashfont2
				FlexDMD.Stage.GetLabel("Text7").Text = Splashtext2
				FlexDMD.Stage.GetLabel("Text7").SetAlignedPosition FlexSizeX/2,FlexSizey*2/3+FlexBorderOffsetY, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text7").visible = True
			End If

		Case "blinktextSlow"
			If SplashBlink mod  21 < 19 Then
				FlexDMD.Stage.GetLabel("Text6").Font = Splashfont1
				FlexDMD.Stage.GetLabel("Text6").Text = Splashtext1
				FlexDMD.Stage.GetLabel("Text6").SetAlignedPosition FlexSizeX/2,FlexSizey/3-FlexBorderOffsetY, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text6").visible = True
				FlexDMD.Stage.GetLabel("Text7").Font = Splashfont2
				FlexDMD.Stage.GetLabel("Text7").Text = Splashtext2
				FlexDMD.Stage.GetLabel("Text7").SetAlignedPosition FlexSizeX/2,FlexSizey*2/3+FlexBorderOffsetY, FlexDMD_Align_center
				FlexDMD.Stage.GetLabel("Text7").visible = True
			Else
				FlexDMD.Stage.GetLabel("Text7").visible = False
				FlexDMD.Stage.GetLabel("Text6").visible = False

			End If


		Case "blinkbonus"
'			If FlexDMDHighQuality = True Then
				If SplashBlink mod  4 <> 1 Then
					FlexDMD.Stage.GetLabel("Text6").Font = Splashfont1
					FlexDMD.Stage.GetLabel("Text6").Text = Splashtext1
					FlexDMD.Stage.GetLabel("Text6").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_center
					FlexDMD.Stage.GetLabel("Text6").visible = True
				Else
					FlexDMD.Stage.GetLabel("Text6").visible = False
				End If
'			Else
'				If SplashBlink mod  4 = 1 Then
'				
'				Else
'
'				End If
'			End If

		Case Else
			FlexDMD.Stage.GetLabel("Text6").Font = Splashfont1
			FlexDMD.Stage.GetLabel("Text6").Text = Splashtext1
			FlexDMD.Stage.GetLabel("Text6").SetAlignedPosition FlexSizeX/2,FlexSizey*2/3+FlexBorderOffsetY, FlexDMD_Align_center
			FlexDMD.Stage.GetLabel("Text6").visible = True
	End Select
End Sub




Dim DMD_oldimage : DMD_oldimage = -1
Sub DMD_Showimage ( image )
	If DMD_oldimage <> -1 Then FlexDMD.Stage.GetImage("Nightmare" & DMD_oldimage ).visible = False 
	If image = -1 Then
		DMD_oldimage = -1
	Else
		FlexDMD.Stage.GetImage("Nightmare" & image).visible = True
		DMD_oldimage = image
	End If
End Sub


Sub DMD_removeall
	FlexDMD.Stage.GetLabel("Text1").text = " "
	FlexDMD.Stage.GetLabel("Text2").text = " "
	FlexDMD.Stage.GetLabel("Text3").text = " "
	FlexDMD.Stage.GetLabel("Text4").text = " "
	FlexDMD.Stage.GetLabel("Text5").text = " "
	FlexDMD.Stage.GetLabel("Text6").text = " "
	FlexDMD.Stage.GetLabel("Text7").text = " "
	FlexDMD.Stage.GetLabel("Text1").visible = False
	FlexDMD.Stage.GetLabel("Text2").visible = False
	FlexDMD.Stage.GetLabel("Text3").visible = False
	FlexDMD.Stage.GetLabel("Text4").visible = False
	FlexDMD.Stage.GetLabel("Text5").visible = False
	FlexDMD.Stage.GetLabel("Text6").visible = False
	FlexDMD.Stage.GetLabel("Text7").visible = False

End Sub

'zLights1
Dim DMD_xmas : DMD_xmas = 0
Dim DMD_xmasarray : DMD_xmasarray = array ( 1,8,2,8,3,8,4,8,5,8,6,8,7,8,1,8,2,8,3,8,4,8,5,8,6,8,7,8,9,8,9,8,9,8) '33
Sub DMD_UpdateXmas
	DMD_xmas = DMD_xmas + 1
	If DMD_xmas < 20 Then Exit Sub
	If DMD_xmas < 53 then
		DMDScene.GetImage("middle").Bitmap = FlexDMD.NewImage("", "VPX.zLights" &  DMD_xmasarray ( DMD_xmas - 20 ) & "&dmd=2").Bitmap
		DMDScene.GetImage("middle").visible = True
	Else
		DMD_xmas = 0
		DMDScene.GetImage("middle").visible = False
	End If
End Sub


Dim DMD_Pumpkins : DMD_Pumpkins = 0
Sub DMD_UpdatePumkins
	DMD_Pumpkins = DMD_Pumpkins + 1
	If DMD_Pumpkins < 27 then
		DMDScene.GetImage("middle").Bitmap = FlexDMD.NewImage("", "VPX.zPumkins" & ( DMD_Pumpkins - 1 ) & "&dmd=2").Bitmap
		DMDScene.GetImage("middle").visible = True
	Else
		DMD_Pumpkins = 0
		DMDScene.GetImage("middle").visible = False
	End If
End Sub



Dim DMD_Pumpkin : DMD_Pumpkin = 0
Sub DMD_UpdatePumkin
	DMD_Pumpkin = DMD_Pumpkin + 1
	If DMD_Pumpkin < 11 then
		DMDScene.GetImage("middle").Bitmap = FlexDMD.NewImage("", "VPX.zPumpkin" & ( DMD_Pumpkin - 1 ) & "&dmd=2").Bitmap
		DMDScene.GetImage("middle").visible = True
	Else
		DMD_Pumpkin = 0
		DMDScene.GetImage("middle").visible = False
	End If
End Sub


Dim DMD_DoggieLeft : DMD_DoggieLeft = 0
Sub DMD_UpdateDoggieLeft
	DMD_DoggieLeft = DMD_DoggieLeft + 1
	If DMD_DoggieLeft < 60 then
		DMDScene.GetImage("middle").Bitmap = FlexDMD.NewImage("", "VPX.zdog (" & DMD_DoggieLeft - 1 & ")&dmd=2").Bitmap

		DMDScene.GetImage("middle").visible = True
	Else
		DMD_DoggieLeft = 0
		DMDScene.GetImage("middle").visible = False
	End If
End Sub

Dim DMD_DoggieRight : DMD_DoggieRight = 0
Sub DMD_UpdateDoggieRight
	DMD_DoggieRight = DMD_DoggieRight + 1
	If DMD_DoggieRight < 60 then
		DMDScene.GetImage("middle").Bitmap = FlexDMD.NewImage("", "VPX.zdog (" & DMD_DoggieRight + 64 & ")&dmd=2").Bitmap
		DMDScene.GetImage("middle").visible = True
	Else
		DMD_DoggieRight = 0
		DMDScene.GetImage("middle").visible = False
	End If
End Sub


Dim ChangeJP : ChangeJP = 0
Sub DMD_UpdateJackpot

	DMD_Jackpot = DMD_Jackpot + 1

	If FlexDMDHighQuality = True Then

		Select Case ChangeJP
		Case 0:
			If DMD_Jackpot = 25 Then ChangeJP = 1 
			FlexDMD.Stage.GetImage("Letter1").visible = True
			FlexDMD.Stage.GetImage("Letter2").visible = True
			FlexDMD.Stage.GetImage("Letter3").visible = True
			FlexDMD.Stage.GetImage("Letter4").visible = True
			FlexDMD.Stage.GetImage("Letter5").visible = True
			FlexDMD.Stage.GetImage("Letter6").visible = True
			FlexDMD.Stage.GetImage("Letter7").visible = True

		Case 1:

			If DMD_Jackpot = 66 Then ChangeJP = 2 
			Select Case int(rnd(1)*7)
				Case 0 :	FlexDMD.Stage.GetImage("Letter1").visible = True
				Case 1 :	FlexDMD.Stage.GetImage("Letter2").visible = TRUE
				Case 2 :	FlexDMD.Stage.GetImage("Letter3").visible = TRUE
				Case 3 :	FlexDMD.Stage.GetImage("Letter4").visible = TRUE
				Case 4 :	FlexDMD.Stage.GetImage("Letter5").visible = TRUE
				Case 5 :	FlexDMD.Stage.GetImage("Letter6").visible = TRUE
				Case 6 :	FlexDMD.Stage.GetImage("Letter7").visible = TRUE

			End Select
			Select Case int(rnd(1)*7)+7
				Case 7 :	FlexDMD.Stage.GetImage("Letter1").visible = False
				Case 8 :	FlexDMD.Stage.GetImage("Letter2").visible = False
				Case 9 :	FlexDMD.Stage.GetImage("Letter3").visible = False
				Case 10 :	FlexDMD.Stage.GetImage("Letter4").visible = False
				Case 11 :	FlexDMD.Stage.GetImage("Letter5").visible = False
				Case 12 :	FlexDMD.Stage.GetImage("Letter6").visible = False
				Case 13 :	FlexDMD.Stage.GetImage("Letter7").visible = False
			End Select
		Case 2:
			If DMD_Jackpot > 95 Then  
				DMD_Jackpot = 0
				ChangeJP = 0
				FlexDMD.Stage.GetImage("Letter1").visible = False
				FlexDMD.Stage.GetImage("Letter2").visible = False
				FlexDMD.Stage.GetImage("Letter3").visible = False
				FlexDMD.Stage.GetImage("Letter4").visible = False
				FlexDMD.Stage.GetImage("Letter5").visible = False
				FlexDMD.Stage.GetImage("Letter6").visible = False
				FlexDMD.Stage.GetImage("Letter7").visible = False
				DMD_StartSplash "SUPER",Formatscore ( Scoring ) ,FontHugeOrange, FontHugeRedhalf,70 , "blinkbot2" ,29,0,0
				Exit Sub
			End If

			If DMD_Frame mod 3 = 1 Then
				FlexDMD.Stage.GetImage("Letter1").visible = True
				FlexDMD.Stage.GetImage("Letter2").visible = True
				FlexDMD.Stage.GetImage("Letter3").visible = True
				FlexDMD.Stage.GetImage("Letter4").visible = True
				FlexDMD.Stage.GetImage("Letter5").visible = True
				FlexDMD.Stage.GetImage("Letter6").visible = True
				FlexDMD.Stage.GetImage("Letter7").visible = True
			Else
				FlexDMD.Stage.GetImage("Letter1").visible = False
				FlexDMD.Stage.GetImage("Letter2").visible = False
				FlexDMD.Stage.GetImage("Letter3").visible = False
				FlexDMD.Stage.GetImage("Letter4").visible = False
				FlexDMD.Stage.GetImage("Letter5").visible = False
				FlexDMD.Stage.GetImage("Letter6").visible = False
				FlexDMD.Stage.GetImage("Letter7").visible = False
			End If
		End Select
	Else   ' low res JACKPOT
		If DMD_Jackpot = 2 Then DMD_Jackpot_init
		If DMD_Jackpot > 95 Then
			DMD_Jackpot = 0
			FlexDMD.Stage.GetLabel("Letter1").visible = False
			FlexDMD.Stage.GetLabel("Letter2").visible = False
			FlexDMD.Stage.GetLabel("Letter3").visible = False
			FlexDMD.Stage.GetLabel("Letter4").visible = False
			FlexDMD.Stage.GetLabel("Letter5").visible = False
			FlexDMD.Stage.GetLabel("Letter6").visible = False
			FlexDMD.Stage.GetLabel("Letter7").visible = False
			DMD_StartSplash "SUPER",Formatscore ( Scoring ) ,FontHugeOrange, FontHugeRedhalf,70 , "blinkbot2" ,29,0,0
			Exit Sub
		End If
		Select Case int(rnd(1)*7)
			Case 0 :	FlexDMD.Stage.GetLabel("Letter1").visible = TRUE
			Case 1 :	FlexDMD.Stage.GetLabel("Letter2").visible = TRUE
			Case 2 :	FlexDMD.Stage.GetLabel("Letter3").visible = TRUE
			Case 3 :	FlexDMD.Stage.GetLabel("Letter4").visible = TRUE
			Case 4 :	FlexDMD.Stage.GetLabel("Letter5").visible = TRUE
			Case 5 :	FlexDMD.Stage.GetLabel("Letter6").visible = TRUE
			Case 6 :	FlexDMD.Stage.GetLabel("Letter7").visible = TRUE
		End Select
		Select Case int(rnd(1)*7)+7
			Case 7 :	FlexDMD.Stage.GetLabel("Letter1").visible = False
			Case 8 :	FlexDMD.Stage.GetLabel("Letter2").visible = False
			Case 9 :	FlexDMD.Stage.GetLabel("Letter3").visible = False
			Case 10 :	FlexDMD.Stage.GetLabel("Letter4").visible = False
			Case 11 :	FlexDMD.Stage.GetLabel("Letter5").visible = False
			Case 12 :	FlexDMD.Stage.GetLabel("Letter6").visible = False
			Case 13 :	FlexDMD.Stage.GetLabel("Letter7").visible = False
		End Select
	End If
End Sub


Sub DMD_Jackpot_init
	FlexDMD.Stage.GetLabel("Letter1").text = "J"
	FlexDMD.Stage.GetLabel("Letter2").text = "A"
	FlexDMD.Stage.GetLabel("Letter3").text = "C"
	FlexDMD.Stage.GetLabel("Letter4").text = "K"
	FlexDMD.Stage.GetLabel("Letter5").text = "P"
	FlexDMD.Stage.GetLabel("Letter6").text = "0"
	FlexDMD.Stage.GetLabel("Letter7").text = "T"
	FlexDMD.Stage.GetLabel("Letter1").Font = FontHugeRED
	FlexDMD.Stage.GetLabel("Letter2").Font = FontHugeRED
	FlexDMD.Stage.GetLabel("Letter3").Font = FontHugeRED
	FlexDMD.Stage.GetLabel("Letter4").Font = FontHugeRED
	FlexDMD.Stage.GetLabel("Letter5").Font = FontHugeRED
	FlexDMD.Stage.GetLabel("Letter6").Font = FontHugeRED
	FlexDMD.Stage.GetLabel("Letter7").Font = FontHugeRED
	Dim x
	x = FlexSizeX / 8
	FlexDMD.Stage.GetLabel("Letter1").SetAlignedPosition x * 1 ,FlexSizey/2, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Letter2").SetAlignedPosition x * 2 ,FlexSizey/2, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Letter3").SetAlignedPosition x * 3 ,FlexSizey/2, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Letter4").SetAlignedPosition x * 4 ,FlexSizey/2, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Letter5").SetAlignedPosition x * 5 ,FlexSizey/2, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Letter6").SetAlignedPosition x * 6 ,FlexSizey/2, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Letter7").SetAlignedPosition x * 7 ,FlexSizey/2, FlexDMD_Align_Center

End Sub




Dim EOBcounter : EOBcounter = 0
Dim EOB_TotalBonus
Dim EOB_tenpercent
Dim EOB_scoring
Dim EOB_FastSkip
Dim EOB_Faster
Sub DMD_EOB
	Dim tmp
	tmp = 35
	DMD_Showimage 60
	EOBcounter = EOBcounter + 1
	Select Case EOBcounter

		Case 1			: FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange		: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))	
		Case 2			: FlexDMD.Stage.GetLabel("Text2").Font = FontBigOrange2		: FlexDMD.Stage.GetLabel("Text2").Text = "END OF BALL"
						  EOB_tenpercent = 10
						  EOB_TotalBonus = 0
						  PlaySoundat "fx_relay",drain
		Case 4			: FlexDMD.Stage.GetLabel("Text3").Font = FontBigOrange2		: FlexDMD.Stage.GetLabel("Text3").Text = "BONUS"
							
		'-----------
		Case 20			: FlexDMD.Stage.GetLabel("Text2").Font = FontBigGrey2		: FlexDMD.Stage.GetLabel("Text2").Text = " "
		Case 22			: FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2		: FlexDMD.Stage.GetLabel("Text3").Text = " "
						  PlaySoundat "fx_relay",drain
							If EOB_Faster = True Then
								EOB_TotalBonus = EOB_TotalBonus + score_EOB_Missions * BonusMultiplier(CurrentPlayer) * Delivered_mission(CurrentPlayer)
								EOB_TotalBonus = EOB_TotalBonus + score_EOB_Orbits * BonusMultiplier(CurrentPlayer) * Loopcount(CurrentPlayer)
								EOB_TotalBonus = EOB_TotalBonus + score_EOB_Combo * BonusMultiplier(CurrentPlayer) * ComboCount(CurrentPlayer)
								EOB_TotalBonus = EOB_TotalBonus + score_EOB_bumper * BonusMultiplier(CurrentPlayer) * BumperHits(CurrentPlayer)
								AddScore2 EOB_TotalBonus
								EOBcounter = 499 : Exit Sub
							End If
		'-----------
		Case 40			: PlaySoundat "fx_relay",drain
						  BlinkLights 9,12,9,7,7,0
						  BlinkLights 55,55,9,7,7,0
						  FlexDMD.Stage.GetLabel("Text2").Font = FontBigGrey2		: FlexDMD.Stage.GetLabel("Text2").Text = "BONUS"
		Case 42			: FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2		: FlexDMD.Stage.GetLabel("Text3").Text = "MULTIPLIER x" & BonusMultiplier(CurrentPlayer) 


		'-----------
		Case 70			: PlaySoundat "fx_relay",drain
						  FlexDMD.Stage.GetLabel("Text2").Font = FontBigGrey2		: FlexDMD.Stage.GetLabel("Text2").Text = "PRESENTS " & Delivered_mission(CurrentPlayer)
						  scoring =  score_EOB_Missions * BonusMultiplier(CurrentPlayer) * Delivered_mission(CurrentPlayer)
						  FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2		: FlexDMD.Stage.GetLabel("Text3").Text = formatscore (Scoring)
						  EOB_scoring = Scoring
						  EOB_TotalBonus = EOB_TotalBonus + EOB_scoring
						  If EOB_Faster Then AddScore2 Scoring : EOBcounter = 199 : Exit Sub
						  Shakesnalones 6

						  NightmareShowCounter = 1
						  BlinkLights 43,43,9,7,7,0
		Case 90		  If EOB_scoring = 0 Then EOBcounter = 190 : Exit Sub
						  EOB_tenpercent = EOB_tenpercent - 1
						  AddScore2 Scoring / 10 : PlaySoundat "fx_spinner",spinner001
						  EOB_scoring = EOB_scoring - Scoring / 10
						  If EOB_tenpercent = 0 Then
							FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange		: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))	
						  Else
							FlexDMD.Stage.GetLabel("Text1").Font = FontHugeRedhalf		: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))	
						  End If
						  FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2		: FlexDMD.Stage.GetLabel("Text3").Text = formatscore (EOB_scoring)
		Case 91	: If EOB_scoring < 10 Then EOBcounter = 195 Else EOBcounter = 89
'-----------
		Case 200  : PlaySoundat "fx_relay",drain
					FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange	: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))
		Case 202  : FlexDMD.Stage.GetLabel("Text2").Font = FontBigGrey2		: FlexDMD.Stage.GetLabel("Text2").Text = " "
					FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2		: FlexDMD.Stage.GetLabel("Text3").Text = " "
		Case 210  : PlaySoundat "fx_relay",drain
					FlexDMD.Stage.GetLabel("Text2").Font = FontBigGrey2		: FlexDMD.Stage.GetLabel("Text2").Text = "RAMPHITS " & Loopcount(CurrentPlayer)	
					scoring =  score_EOB_Orbits * BonusMultiplier(CurrentPlayer) * Loopcount(CurrentPlayer)
					FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2		: FlexDMD.Stage.GetLabel("Text3").Text = formatscore (Scoring)
					EOB_scoring = Scoring : EOB_tenpercent = 10 :
					EOB_TotalBonus = EOB_TotalBonus + EOB_scoring
					If EOB_Faster Then AddScore2 Scoring : EOBcounter = 299 : Exit Sub 
					blinklights 57,57,7,5,3,0
					Shakesnalones 6
					Shakehousefront 10
'					Flashforms f2A, 800, 100, 0
					Flashforms F2B, 800, 100, 0
					Flashforms Flasher004, 800, 50, 0
					Flashforms Flasher005, 800, 50, 0
'					Flashforms f1A, 800, 50, 0
					Flashforms F1B, 800, 50, 0
					Flashforms Flasher006, 800, 50, 0
					Flashforms Flasher007, 800, 50, 0
					ArrowLights(2,2) = 2
					ArrowLights(7,2) = 2
		Case 230  : If EOB_scoring = 0 Then EOBcounter = 290 : Exit Sub
					EOB_tenpercent = EOB_tenpercent - 1
					AddScore2 Scoring / 10 : PlaySoundat "fx_spinner",spinner001
					EOB_scoring = EOB_scoring - ( Scoring / 10 )
					If EOB_tenpercent = 0 Then
						FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange	: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))	
					Else
						FlexDMD.Stage.GetLabel("Text1").Font = FontHugeRedhalf	: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))	
					End If
					FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2			: FlexDMD.Stage.GetLabel("Text3").Text = formatscore (EOB_scoring)
'-----------
		Case 231  : If EOB_scoring = 0 Then EOBcounter = 295 Else EOBcounter = 229

		Case 300  : PlaySoundat "fx_relay",drain
					ArrowLights(2,2) = 0
					ArrowLights(7,2) = 0
					FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange	: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))
		Case 302  : FlexDMD.Stage.GetLabel("Text2").Font = FontBigGrey2		: FlexDMD.Stage.GetLabel("Text2").Text = " "
					FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2		: FlexDMD.Stage.GetLabel("Text3").Text = " "
		Case 310  : PlaySoundat "fx_relay",drain
					FlexDMD.Stage.GetLabel("Text2").Font = FontBigGrey2		: FlexDMD.Stage.GetLabel("Text2").Text = "COMBOS " & ComboCount(CurrentPlayer)
					scoring =  score_EOB_Combo * BonusMultiplier(CurrentPlayer) * ComboCount(CurrentPlayer)
					FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2		: FlexDMD.Stage.GetLabel("Text3").Text = formatscore (Scoring)
					EOB_scoring = Scoring : EOB_tenpercent = 10 :
					EOB_TotalBonus = EOB_TotalBonus + EOB_scoring
					If EOB_Faster Then AddScore2 Scoring : EOBcounter = 399 : Exit Sub
					Shakesnalones 6
					For x = 1 to 9 : ArrowLights(x,2) = 2 : Next
		Case 330  : If EOB_scoring = 0 Then EOBcounter = 390 : Exit Sub
					EOB_tenpercent = EOB_tenpercent - 1
					AddScore2 Scoring / 10 : PlaySoundat "fx_spinner",spinner002
					EOB_scoring = EOB_scoring - ( Scoring / 10 )
					If EOB_tenpercent = 0 Then
						FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange	: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))	
					Else
						FlexDMD.Stage.GetLabel("Text1").Font = FontHugeRedhalf	: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))	
					End If
					FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2			: FlexDMD.Stage.GetLabel("Text3").Text = formatscore (EOB_scoring)

		Case 331  : If EOB_scoring = 0 Then EOBcounter = 395 Else EOBcounter = 329
'-----------
		Case 400  : PlaySoundat "fx_relay",drain
					For x = 1 to 9 : ArrowLights(x,2) = 0 : Next
					FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange	: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))
		Case 402  : FlexDMD.Stage.GetLabel("Text2").Font = FontBigGrey2		: FlexDMD.Stage.GetLabel("Text2").Text = " "
					FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2		: FlexDMD.Stage.GetLabel("Text3").Text = " "
		Case 410  : PlaySoundat "fx_relay",drain
					FlexDMD.Stage.GetLabel("Text2").Font = FontBigGrey2		: FlexDMD.Stage.GetLabel("Text2").Text = "BUMPERHITS " & BumperHits(CurrentPlayer)
					scoring =  score_EOB_bumper * BonusMultiplier(CurrentPlayer) * BumperHits(CurrentPlayer)
					FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2		: FlexDMD.Stage.GetLabel("Text3").Text = formatscore (Scoring)
					EOB_scoring = Scoring : EOB_tenpercent = 10 
					EOB_TotalBonus = EOB_TotalBonus + EOB_scoring
					If EOB_Faster Then AddScore2 Scoring : EOBcounter = 499 : Exit Sub
					Shakesnalones 6
					blinklights 38,38,9,7,7,0
					Flashforms libumper, 850, 75, 0
		Case 430  : If EOB_scoring = 0 Then EOBcounter = 490 : Exit Sub
					EOB_tenpercent = EOB_tenpercent - 1
					AddScore2 Scoring / 10 : PlaySoundat "fx_spinner",spinner002
					EOB_scoring = EOB_scoring - ( Scoring / 10 )
					If EOB_tenpercent = 0 Then
						FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange	: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))	
					Else
						FlexDMD.Stage.GetLabel("Text1").Font = FontHugeRedhalf	: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))	
					End If
					FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2			: FlexDMD.Stage.GetLabel("Text3").Text = formatscore (EOB_scoring)

		Case 431  : If EOB_scoring = 0 Then EOBcounter = 495 Else EOBcounter = 429
'-----------
		Case 500  : PlaySoundat "fx_relay",drain : Shakesnalones 12
					GiEffect 3
					LightEffect 3
					BlinkAllPresents 4,0
					FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange	: FlexDMD.Stage.GetLabel("Text1").Text = formatscore (score(1))
		Case 502  : FlexDMD.Stage.GetLabel("Text2").Font = FontBigGrey2		: FlexDMD.Stage.GetLabel("Text2").Text = " "
					FlexDMD.Stage.GetLabel("Text3").Font = FontBigRed2		: FlexDMD.Stage.GetLabel("Text3").Text = " "
		Case 510  : PlaySoundat "fx_relay",drain
					Flashforms WizardCenterLight,500, 50, 0
					FlexDMD.Stage.GetLabel("Text2").Font = FontBigGrey2		: FlexDMD.Stage.GetLabel("Text2").Text = "TOTAL BONUS"
					FlexDMD.Stage.GetLabel("Text3").Font = FontBigGrey2		: FlexDMD.Stage.GetLabel("Text3").Text = formatscore (EOB_TotalBonus)
		Case 514,518,522,526,530,534,538,540,544 : FlexDMD.Stage.GetLabel("Text1").Font = FontHugeRedhalf 
		Case 516,520,524,528,532,536,540,542,546 : FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange 
		Case 536: LightEffect 3
		Case 555  : endOfBall2 : EOB_FastSkip = False : EOB_Faster = False : 
	End Select
	FlexDMD.Stage.GetLabel("Text1").SetAlignedPosition FlexSizeX/2,FlexBorderOffsetY, FlexDMD_Align_Top : FlexDMD.Stage.GetLabel("Text1").visible = true 
	FlexDMD.Stage.GetLabel("Text2").SetAlignedPosition FlexSizeX/2,FlexSizey/2, FlexDMD_Align_top : FlexDMD.Stage.GetLabel("Text2").visible = true
	FlexDMD.Stage.GetLabel("Text3").SetAlignedPosition FlexSizeX/2,FlexSizey -FlexBorderOffsetY, FlexDMD_Align_bottom: FlexDMD.Stage.GetLabel("Text3").visible = true
End Sub

'Delivered_mission(CurrentPlayer) = 4
'Loopcount(CurrentPlayer) = 20
'ComboCount(CurrentPlayer) = 25
'BumperHits(CurrentPlayer) = 15
'spinnerspins = 104
'DMD_newmode = 3

Dim Jack_minidisplay
Dim DMD_pointsgain : DMD_pointsgain = 0
Dim DMD_pointsCounter : DMD_pointsCounter = 0
Dim DMD_mini : DMD_mini = 1
Sub DMD_UpdateMode2
	dim mini_displaytime
	mini_displaytime = 100
	Dim mini(30) , tmp , tmp2
	For x = 0 to 30 : mini(x) = 0 : Next
	mini(1) = 1 : x = 1																	' ball/Credit
	If WizardStage > 0 then 
		mini(1) = 28 : x = 1
		mini(2) = 29 : x = 2
			
		If WizardStage = 1 Then x = x + 1 : mini(x) = 20 : x = x + 1 : mini(x) = 20
		If WizardStage = 2 Then x = x + 1 : mini(x) = 21 : x = x + 1 : mini(x) = 21
		If WizardStage = 3 Then x = x + 1 : mini(x) = 22 : x = x + 1 : mini(x) = 22
		If WizardStage = 4 Then x = x + 1 : mini(x) = 23 : x = x + 1 : mini(x) = 23 
		If WizardStage = 5 Then x = x + 1 : mini(x) = 24 : x = x + 1 : mini(x) = 24 
		If WizardStage = 6 Then x = x + 1 : mini(x) = 25 : x = x + 1 : mini(x) = 25 
		If WizardStage = 7 Then x = x + 1 : mini(x) = 26 : x = x + 1 : mini(x) = 26 
		If WizardStage = 8 Then x = x + 1 : mini(x) = 27 : x = x + 1 : mini(x) = 27 
	Else
		If Blink(50,1) = 2 Then x = x + 1 : mini(x) = 30
		If Blink(52,1)		= 2 			Then x = x + 1 : mini(x) = 13			' eb is lit
		If ModeRunning(CurrentPlayer) > 0	Then x = x + 1 : mini(x) = 2			' modeinfo
		If bJackMBStarted	= True			Then x = x + 1 : mini(x) = 8			' jackmb
		If BallsInLock(CurrentPlayer) > 0 And BallsInLock(CurrentPlayer) < 4 Then x = x + 1 : mini(x) = 9			' jackmb lockis lit
		If Delivered_mission(CurrentPlayer) = 8 Then x = x + 1 : mini(x) = 10			' next is NightmareMB
		If JackResetAvail(CurrentPlayer) Then x = x + 1 : mini(x) = 11			' jackreset is close
		If JackResetAvail(CurrentPlayer) = False And BallsInLock(CurrentPlayer) = 0 Then x = x + 1 : mini(x) = 12			' jackmb is starting
		If MegaSpinnersTime	> 0				Then x = x + 1 : mini(x) = 3			' 
		If ArrowLights(10,1) = 2			Then x = x + 1 : mini(x) = 14	
		If XmasHurryupTimer	> 0 			Then x = x + 1 : mini(x) = 4			' 
		If DogChaseTimer	> 0				Then x = x + 1 : mini(x) = 5			' 
		If SuperLoopTimer	> 0 			Then x = x + 1 : mini(x) = 6			' 
		If WizardMode(CurrentPlayer) = 1	Then x = x + 1 : mini(x) = 15


	End If

	If DMD_Frame mod mini_displaytime = 1 Then DMD_mini = DMD_mini + 1   
	If DMD_mini > x Then DMD_mini = 1
	FlexDMD.Stage.GetLabel("Text5").visible = true
	Select Case mini(DMD_mini)
		Case 1
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGrey
			If Not FlexDMDHighQuality Then
				FlexDMD.Stage.GetLabel("Text5").Text = "B-" & 4-BallsRemaining(CurrentPlayer) & "  C-"  & Credits 
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = "BALL " & 4-BallsRemaining(CurrentPlayer) & "  CREDITS "  & Credits 
			End If
		Case 2
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalBlue
			If Blink(43,1) = 2 Then
				FlexDMD.Stage.GetLabel("Text5").Text = "DELIVER PRESENT"
			Else
				Select Case TranslateLetters( ModeRunning(CurrentPlayer) )
					Case  1 : FlexDMD.Stage.GetLabel("Text5").Text = "HIT RAMPS " & 3-ModeProgress & " LEFT"
					Case  2 : FlexDMD.Stage.GetLabel("Text5").Text = "BLUE LIGHTS " & 7-ModeProgress & " LEFT"
					Case  3 : FlexDMD.Stage.GetLabel("Text5").Text = "BLUE LIGHTS " & 7-ModeProgress & " LEFT"
					Case  4 : FlexDMD.Stage.GetLabel("Text5").Text = "BLUE LIGHTS " & 3-ModeProgress & " LEFT"
					Case  5 : FlexDMD.Stage.GetLabel("Text5").Text = "ALL TARGETS " & 6-ModeProgress & " LEFT"
					Case  6 : FlexDMD.Stage.GetLabel("Text5").Text = "INTO SCOOP " & 2-ModeProgress & " LEFT"
					Case  7 : FlexDMD.Stage.GetLabel("Text5").Text = "WHERE IS JACK"
					Case  8 : FlexDMD.Stage.GetLabel("Text5").Text = "BLUE LIGHTS " & 3-ModeProgress & " LEFT"
					Case  9 : FlexDMD.Stage.GetLabel("Text5").Text = "HIT RAMPS " & 5-ModeProgress & " LEFT"
					Case 10 : FlexDMD.Stage.GetLabel("Text5").Text = "HIT RAMPS " & 7-ModeProgress & " LEFT"
					Case 11 : FlexDMD.Stage.GetLabel("Text5").Text = "CAPTIVE HITS " & 3-ModeProgress & " LEFT"
					Case 12 : FlexDMD.Stage.GetLabel("Text5").Text = "HIT BUMPER " & 5-ModeProgress & " LEFT"
					Case 13 : FlexDMD.Stage.GetLabel("Text5").Text = "SPINNERS " & 50-ModeProgress & " LEFT"
					Case 14 : FlexDMD.Stage.GetLabel("Text5").Text = "CENTERSPINNER " & 3-ModeProgress & " LEFT"
					Case 15 : FlexDMD.Stage.GetLabel("Text5").Text = "HIT BUMPER ONCE"
					Case 16 : FlexDMD.Stage.GetLabel("Text5").Text = "SPINNERS " & 20-ModeProgress & " LEFT"
					Case 17 : FlexDMD.Stage.GetLabel("Text5").Text = "SPINNERS " & 80-ModeProgress & " LEFT"
					Case 18 : FlexDMD.Stage.GetLabel("Text5").Text = "GET COMBOS " & 2-ModeProgress & " LEFT"
					Case 19 : FlexDMD.Stage.GetLabel("Text5").Text = "GET COMBOS " & 4-ModeProgress & " LEFT"
					Case 20 : FlexDMD.Stage.GetLabel("Text5").Text = "GET COMBOS " & 8-ModeProgress & " LEFT"
					Case 21 : FlexDMD.Stage.GetLabel("Text5").Text = "BLUE LIGHTS " & 8-ModeProgress & " LEFT"
					Case 22 : FlexDMD.Stage.GetLabel("Text5").Text = "BLUE LIGHTS " & 4-ModeProgress & " LEFT"
					Case 23 : FlexDMD.Stage.GetLabel("Text5").Text = "HIT RAMPS " & 5-ModeProgress & " LEFT"
					Case 24 : FlexDMD.Stage.GetLabel("Text5").Text = "CAPTIVE HITS " & 2-ModeProgress & " LEFT"
				End Select
			End If

		Case 3
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGreen   
			FlexDMD.Stage.GetLabel("Text5").Text = "MEGASPINS " & MegaSpinnersTime & " SEC"
		Case 4
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGreen   
			FlexDMD.Stage.GetLabel("Text5").Text = "XMASRAMP " & XmasHurryupTimer & " SEC"
		Case 5
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGreen   
			FlexDMD.Stage.GetLabel("Text5").Text = "DOGCHASE " & DogChaseTimer & " SEC"
		Case 6
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred   
			FlexDMD.Stage.GetLabel("Text5").Text = "SUPERLOOP " & SuperLoopTimer & " SEC"
		Case 7
			DMD_mini = DMD_mini + 1
		Case 8
			If Blink(41,1) = 2 Then		' lite sjp is active
				FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred   
				FlexDMD.Stage.GetLabel("Text5").Text = "HOUSE TO LIT SUPER"		
			ElseIf Blink(42,1) = 2 Then			'42,1 = 2 -- superactive
				FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred   
				FlexDMD.Stage.GetLabel("Text5").Text = "GET SUPERJACKPOT"
			Else
				If Jack_minidisplay = 1 Then
					FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred   
					FlexDMD.Stage.GetLabel("Text5").Text = "JACKPOT = " & formatscore (score_JasonJackpot + score_JasonSuperBonus * JasonSuper)
				Else
					FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred   
					FlexDMD.Stage.GetLabel("Text5").Text = "SUPERJP = " & formatscore (score_JasonSJP + score_JasonSuperBonus * JasonSuper * 2)
				End If
			End If
		Case 9
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGreen 
			If BallsInLock(CurrentPlayer) = 1 Then FlexDMD.Stage.GetLabel("Text5").Text = "JACK LOCK 1 IS LIT"  
			If BallsInLock(CurrentPlayer) = 2 Then FlexDMD.Stage.GetLabel("Text5").Text = "JACK LOCK 2 IS LIT" 
			If BallsInLock(CurrentPlayer) = 3 Then FlexDMD.Stage.GetLabel("Text5").Text = "JACK LOCK 3 IS LIT"  

		Case 10
			If DMD_Frame mod mini_displaytime < mini_displaytime/2 Then
				FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGrey   
				FlexDMD.Stage.GetLabel("Text5").Text = "NEXT DELIVERY"
			Else
				FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGrey   
				FlexDMD.Stage.GetLabel("Text5").Text = "START NIGHTMARE"
			End If
		Case 11
			If DMD_Frame mod mini_displaytime < mini_displaytime/2 Then
				FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGrey   
				FlexDMD.Stage.GetLabel("Text5").Text = JackResetNeeded(CurrentPlayer) - JackResetCounter(CurrentPlayer) & " MORE PRESENTS"
			Else
				FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGrey   
				FlexDMD.Stage.GetLabel("Text5").Text = "RESET JACK MULTIBALL"
			End If
		Case 12
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGreen 
			If DMD_Frame mod mini_displaytime < mini_displaytime/2 Then
  				FlexDMD.Stage.GetLabel("Text5").Text = "SPELL J A C K"
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = "LITES LOCK 1"
			End If
		Case 13
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGreen 
			If DMD_Frame mod 8 > 1 Then
				FlexDMD.Stage.GetLabel("Text5").Text = "GET EXTRA BALL"
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = " "
			End If
		Case 14
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred   
			FlexDMD.Stage.GetLabel("Text5").Text = "SCOOP = SPINN JACKPOT"
		Case 15 'nightmare show jackpots
			tmp  = 0
			tmp2 = 0
			for x = 1 to 10
				If ArrowLights(x,4) = 2 Then
					tmp = tmp + score_nightmare_nrlitsupers
					tmp2 = tmp2 + 5
				End If

			Next
			If tmp = 0 Then
				FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred   
				FlexDMD.Stage.GetLabel("Text5").Text = "JP LIT =" & NightmareLitJP + tmp2 - AddWizHit1 & " SWITCHES"
			Else
				FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGrey   
				FlexDMD.Stage.GetLabel("Text5").Text = "JP VALUE " & FormatScore(score_nightmare_supers + tmp )
			End If


'WIZARDSTUFF
		Case 20
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred
			If WizardProgress < WizardTarget Then
				FlexDMD.Stage.GetLabel("Text5").Text = "HIT " & WizardTarget - WizardProgress & "SWITCHES"
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = "COMPLETE"
			End If
		Case 21
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred
			If WizardProgress < WizardTarget Then
				FlexDMD.Stage.GetLabel("Text5").Text = WizardTarget - WizardProgress & " LIGHTS LEFT"
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = "COMPLETE"
			End If
		Case 22 ' stage3
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred
			If WizardProgress < WizardTarget Then
				FlexDMD.Stage.GetLabel("Text5").Text = WizardTarget - WizardProgress & " MORE RAMPS"
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = "COMPLETE"
			End If
		Case 23 ' stage 4
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred
			If WizardProgress < WizardTarget Then
				FlexDMD.Stage.GetLabel("Text5").Text = "GET SUPERJACKPOT"
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = "COMPLETE"
			End If
		Case 24 ' stage 5
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred
			If WizardProgress < WizardTarget Then
				FlexDMD.Stage.GetLabel("Text5").Text = "HIT " & WizardTarget - WizardProgress & "SWITCHES"
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = "COMPLETE"
			End If
		Case 24 ' stage 6
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred
			If WizardProgress < WizardTarget Then
				FlexDMD.Stage.GetLabel("Text5").Text = WizardTarget - WizardProgress & " LIGHTS LEFT"
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = "COMPLETE"
			End If
		Case 25 ' stage 7
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred
			If WizardProgress < WizardTarget Then
				FlexDMD.Stage.GetLabel("Text5").Text = "GET SUPERJACKPOT"
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = "COMPLETE"
			End If
		Case 26 ' stage 8
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalred
			If WizardProgress < WizardTarget Then
				FlexDMD.Stage.GetLabel("Text5").Text = "ENTER SCOOP"
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = "COMPLETE"
			End If



'stage1 : 100 triggers			 50k
'stage2 : all 17 arrowlights once	100k
'stage3 : 10 ramps			200k	
'stage4 : superjackpot 2 times		  2kk	
'stage5 : 150 triggers			100k
'stage6 : all arrows 2 times each	200k
'stage7 : superjackpot 2 times		  3kk
'stage8 : scoop for finish mega jackpot ? 30kk

'		If WizardStage = 6 Then x = x + 1 : mini(x) = 25			' wiz1
'		If WizardStage = 7 Then x = x + 1 : mini(x) = 26			' wiz1
'		If WizardStage = 8 Then x = x + 1 : mini(x) = 27			' wiz1

		Case 28 ' WIZINFO
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGrey 
			If DMD_Frame mod mini_displaytime < mini_displaytime/2 Then
  				FlexDMD.Stage.GetLabel("Text5").Text = "WIZARD MULTIBALL"
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = "HIT LIT LIGHTS"
			End If
		Case 28 ' WIZINFO
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGrey 
			If DMD_Frame mod mini_displaytime < mini_displaytime * 0.66 Then
				FlexDMD.Stage.GetLabel("Text5").Text = "STAGE " & WizardStage
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = " "
			End If

		Case 30 ' add revive is lit
			FlexDMD.Stage.GetLabel("Text5").Font = FontSmalGreen 
			If DMD_Frame mod mini_displaytime < mini_displaytime/2 Then
  				FlexDMD.Stage.GetLabel("Text5").Text = "START REVIVE"
			Else
				FlexDMD.Stage.GetLabel("Text5").Text = "IS LIT"
			End If


	End Select





	If PlayersPlayingGame = 1 Or wizardmode(CurrentPlayer) > 0 Then
		FlexDMD.Stage.GetLabel("Text5").SetAlignedPosition FlexSizeX/2,FlexSizey-FlexBorderOffsetY, FlexDMD_Align_bottom
	Else 
			FlexDMD.Stage.GetLabel("Text5").SetAlignedPosition FlexSizeX/2,FlexSizey/2 , FlexDMD_Align_center
			FlexDMD.Stage.GetLabel("Text5").SetAlignedPosition FlexSizeX/2,FlexSizey/2 , FlexDMD_Align_center
	End If

	If PlayersPlayingGame = 1 Or wizardmode(CurrentPlayer) > 0 Then
		DMD_Showimage BG_imageDMD 
		FlexDMD.Stage.GetLabel("Text1").Text = formatscore (Score(currentplayer)) 
		FlexDMD.Stage.GetLabel("Text1").SetAlignedPosition FlexSizeX/2,FlexSizey/2+FlexBorderOffsetY+1, FlexDMD_Align_Center
		If DMD_pointsgain > 0 And DMD_pointsCounter > 1 Then
			FlexDMD.Stage.GetLabel("TextRed").Text = FormatScore(DMD_pointsgain)
			FlexDMD.Stage.GetLabel("TextRed").SetAlignedPosition FlexSizeX/1.7,FlexSizey/2.4-FlexBorderOffsetY, FlexDMD_Align_bottom
			FlexDMD.Stage.GetLabel("TextRed").visible = true
			DMD_pointsCounter = DMD_pointsCounter - 1
		Else
			FlexDMD.Stage.GetLabel("TextRed").Text = " "
			FlexDMD.Stage.GetLabel("TextRed").visible = false
			DMD_pointsgain = 0
		End If

	ElseIf PlayersPlayingGame > 0 Then
		DMD_Showimage BG_imageDMD + 1
		If CurrentPlayer = 1 Then FlexDMD.Stage.GetLabel("Text1").Font = FontHugeOrange Else FlexDMD.Stage.GetLabel("Text1").Font = FontBigOrange2
		If CurrentPlayer = 2 Then FlexDMD.Stage.GetLabel("Text2").Font = FontHugeOrange Else FlexDMD.Stage.GetLabel("Text2").Font = FontBigOrange2
		If CurrentPlayer = 3 Then FlexDMD.Stage.GetLabel("Text3").Font = FontHugeOrange Else FlexDMD.Stage.GetLabel("Text3").Font = FontBigOrange2
		If CurrentPlayer = 4 Then FlexDMD.Stage.GetLabel("Text4").Font = FontHugeOrange Else FlexDMD.Stage.GetLabel("Text4").Font = FontBigOrange2

		FlexDMD.Stage.GetLabel("Text1").Text = formatscore (Score(1))   
		FlexDMD.Stage.GetLabel("Text1").SetAlignedPosition FlexBorderOffsetX, FlexBorderOffsetY+1, FlexDMD_Align_TopLeft
		FlexDMD.Stage.GetLabel("Text2").Text = formatscore (Score(2))  
		FlexDMD.Stage.GetLabel("Text2").SetAlignedPosition FlexSizeX-FlexBorderOffsetX+1, FlexBorderOffsetY+1, FlexDMD_Align_TopRight
		If PlayersPlayingGame > 2 Then
			FlexDMD.Stage.GetLabel("Text3").Text = formatscore (Score(3)) 
			FlexDMD.Stage.GetLabel("Text3").SetAlignedPosition FlexBorderOffsetX, FlexSizey-FlexBorderOffsetY, FlexDMD_Align_bottomLeft
		End If
		If PlayersPlayingGame > 3 Then
			FlexDMD.Stage.GetLabel("Text4").Text = formatscore (Score(4)) 
			FlexDMD.Stage.GetLabel("Text4").SetAlignedPosition FlexSizeX-FlexBorderOffsetX+1,FlexSizey-FlexBorderOffsetY, FlexDMD_Align_bottomright
		End If

		If DMD_pointsgain > 0 And DMD_pointsCounter > 1 Then
			FlexDMD.Stage.GetLabel("TextRed").Text = FormatScore(DMD_pointsgain)
			If CurrentPlayer = 1 Or CurrentPlayer  = 3 Then
				FlexDMD.Stage.GetLabel("TextRed").SetAlignedPosition FlexSizeX/5,FlexSizey/2, FlexDMD_Align_center
			Else
				FlexDMD.Stage.GetLabel("TextRed").SetAlignedPosition FlexSizeX*4/5,FlexSizey/2, FlexDMD_Align_center
			End If
			FlexDMD.Stage.GetLabel("TextRed").visible = true
			DMD_pointsCounter = DMD_pointsCounter - 1
		Else
			FlexDMD.Stage.GetLabel("TextRed").Text = " "
			FlexDMD.Stage.GetLabel("TextRed").visible = false
			DMD_pointsgain = 0
		End If
	End If
End Sub

Dim lightshowrepeat : lightshowrepeat = 0
Dim nightmarestart :nightmarestart = 0   
For Each x in InsertsAll : nightmarestart = nightmarestart + 1 : Next
nightmarestart = nightmarestart -24
Dim attractcolor : attractcolor = 0
Sub DMD_UpdateMode1
	If DMD_Counter mod 100 = 5 And RndInt(1,3) = 1 Then FlashForms libumper, 300, 75, 0 : Flashforms Flasher002, 300, 50, 0 : PlaySoundat "fx_GiOn",drain

	
	If DMD_Counter > 150 And DMD_Counter mod 50 = 2 Then ChangeArrows int(rnd(1)*8)
	Select Case DMD_Counter
		case   1 			: ChangeArrows int(rnd(1)*8)
								FlexDMD.Stage.GetLabel("Text1").text = ""
								FlexDMD.Stage.GetLabel("Text2").text = ""
							  DMD_Showimage 4 : For x = nightmarestart to nightmarestart + 23 : blink(x,1) = 0 : Next
		case  20, 24, 28	: DMD_Showimage 1 : For x = nightmarestart to nightmarestart + 8 : blink(x,1) = 1 : Next
		case  31			: ChangeArrows int(rnd(1)*8)
		If RndInt(1,3) = 1 Then
'			Flashforms f1A, 222, 100, 0
			Flashforms F1B, 222, 100, 0
			Flashforms Flasher006, 222, 100, 0
			Flashforms Flasher007, 222, 100, 0
		End If
		case  22, 26, 30	: DMD_Showimage 4 : For x = nightmarestart to nightmarestart + 8 : blink(x,1) = 0 : Next
		case  35, 39, 43	: DMD_Showimage 2 : For x = nightmarestart+9 to nightmarestart + 14 : blink(x,1) = 1 : Next
		case  37, 41, 45	: DMD_Showimage 4 : For x = nightmarestart+9 to nightmarestart + 14 : blink(x,1) = 0 : Next
		case  50, 54, 58	: DMD_Showimage 3 : For x = nightmarestart+15 to nightmarestart + 23 : blink(x,1) = 1 : Next
		case  52, 56, 60	: DMD_Showimage 4 : For x = nightmarestart+15 to nightmarestart + 23 : blink(x,1) = 0 : Next
		case  65			: DMD_Showimage 0 : For x = nightmarestart to nightmarestart + 23 : blink(x,1) = 1 : Next : If RndInt(1,2) = 1 Then gion
		case  70, 80, 90	: DMD_Showimage 5 : If RndInt(1,2) = 1 Then Objlevel(1) = 1 : FlasherFlash1_Timer : blinklights 40,40,2,8,3,0
		case  73, 83, 93	: DMD_Showimage 0 : blinklights 53,54,4,5,1,0

		case 100 :  ChangeArrows int(rnd(1)*8) : Gioff
			If RndInt(1,3) = 1 Then
'			Flashforms f2A, 222, 100, 0
			Flashforms F2B, 222, 100, 0
			Flashforms Flasher004, 222, 100, 0
			Flashforms Flasher005, 222, 100, 0
		End If	
		case 110 : DMD_Showimage 6			  : For x = nightmarestart to nightmarestart + 8 : blink(x,1) = 0 : Next
		case 114 : DMD_Showimage 7			  : For x = nightmarestart to nightmarestart + 8 : blink(x,1) = 1 : Next : For x = nightmarestart+9 to nightmarestart + 14 : blink(x,1) = 0 : Next
		case 118 : DMD_Showimage 8			  : For x = nightmarestart+9 to nightmarestart + 14 : blink(x,1) = 1 : Next : For x = nightmarestart+15 to nightmarestart + 23 : blink(x,1) = 0 : Next
		case 122 : DMD_Showimage 6			  : For x = nightmarestart+15 to nightmarestart + 23 : blink(x,1) = 1 : Next : For x = nightmarestart to nightmarestart + 8 : blink(x,1) = 0 : Next
		case 126 : DMD_Showimage 7			  : For x = nightmarestart to nightmarestart + 8 : blink(x,1) = 1 : Next : For x = nightmarestart+9 to nightmarestart + 14 : blink(x,1) = 0 : Next
		case 130 : DMD_Showimage 8			  : For x = nightmarestart+9 to nightmarestart + 14 : blink(x,1) = 1 : Next : For x = nightmarestart+15 to nightmarestart + 23 : blink(x,1) = 0 : Next
		case 134 : DMD_Showimage 0			  : For x = nightmarestart+15 to nightmarestart + 23 : blink(x,1) = 1 : Next : blinklights 53,54,2,5,12,0
		Case 150 :	If lightshowrepeat < 2 Then DMD_Counter = 0 : lightshowrepeat = lightshowrepeat + 1
		Case 151 : lightshowrepeat = 0 : DMD_Showimage 4 

		case 160 : 
			FlexDMD.Stage.GetLabel("Text1").text = ""
			FlexDMD.Stage.GetLabel("Text2").text = ""
			FlexDMD.Stage.GetLabel("Text1").SetAlignedPosition FlexSizeX/2,FlexBorderOffsetY, FlexDMD_Align_bottom : FlexDMD.Stage.GetLabel("Text1").visible = true 
			FlexDMD.Stage.GetLabel("Text2").SetAlignedPosition FlexSizeX/2,FlexSizey-FlexBorderOffsetY, FlexDMD_Align_top : FlexDMD.Stage.GetLabel("Text2").visible = true
			
		For x = nightmarestart to nightmarestart + 23 : blink(x,1) = 0 : Next
		BlinkAllPresents 3,3 : blinklights 56,56,3,5,2,0
		If Score(1) Then DMD_StartSplash "SCORES","LAST"	,FontHugeRedhalf, FontHugeOrange, 111 , "oneimage" , 4 ,0,0
		If Score(1) Then
			DMD_StartSplash "PLAYER 1",FormatScore(Score(1) )	,FontHugeRedhalf, FontHugeOrange, 133 , "oneimage" , 4 ,0,0
		End If
		If Score(2) Then DMD_StartSplash "PLAYER 2",FormatScore(Score(2) )	,FontHugeRedhalf, FontHugeOrange, 133 , "oneimage" , 4 ,0,0
		If Score(3) Then DMD_StartSplash "PLAYER 3",FormatScore(Score(3) )	,FontHugeRedhalf, FontHugeOrange, 133 , "oneimage" , 4 ,0,0
		If Score(4) Then DMD_StartSplash "PLAYER 4",FormatScore(Score(4) )	,FontHugeRedhalf, FontHugeOrange, 133 , "oneimage" , 4 ,0,0
		If bFreePlay Then
						 DMD_StartSplash "PRESS START","FREE PLAY"			,FontHugeOrange	, FontHugeOrange, 133 , "oneimage" , 4 ,0,0
		ElseIf Credits > 0 Then
						 DMD_StartSplash "PRESS START","CREDITS " & Credits ,FontHugeOrange , FontHugeOrange, 133 , "oneimage" , 4 ,0,0
		Else
						 DMD_StartSplash "NO CREDITS","INSERT COIN"			,FontHugeOrange	, FontHugeOrange, 133 , "oneimage" , 4 ,0,0
		End If
						 DMD_StartSplash HighScoreName(0) & " " & FormatScore(HighScore(0)),"CHAMPION", FontHugeRedhalf, FontHugeOrange, 133 , "oneimage" , 4 ,0,0
						 DMD_StartSplash HighScoreName(1) & " " & FormatScore(HighScore(1)),"HIGHSCORE 1"	,FontHugeRedhalf, FontHugeOrange, 133 , "oneimage" , 4 ,0,0
						 DMD_StartSplash HighScoreName(2) & " " & FormatScore(HighScore(2)),"HIGHSCORE 2"	,FontHugeRedhalf, FontHugeOrange, 133 , "oneimage" , 4 ,0,0
						 DMD_StartSplash HighScoreName(3) & " " & FormatScore(HighScore(3)),"HIGHSCORE 3"	,FontHugeRedhalf, FontHugeOrange, 133 , "oneimage" , 4 ,0,0
						 DMD_StartSplash "PINBALL","PLAY MORE"				,FontHugeOrange, FontHugeOrange, 133 , "oneimage" , 4 ,0,0
						 DMD_StartSplash "TO DRUGS","SAY NO"				,FontHugeOrange, FontHugeOrange, 133 , "oneimage" , 4 ,0,0
						 DMD_StartSplash "PRESENTED BY","NIGHTMARE V1"				,FontHugeOrange, FontHugeOrange, 100 , "oneimage" , 19 ,0,0
' fixiing add hightscores
		DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,999 , "workers" ,4,0,0
		case 400 : DMD_Counter = 222
				Select Case RndInt(1,5)
					Case 1 : BlinkAllPresents 1,2
					case 2 : BlinkAllPresents 2,0
					case 3 : BlinkAllPresents 1,3
					case 4 : BlinkAllPresents 1,4
				End Select

		case 1000,1001,1002 : dmd_counter = 0
		case 1100 : dmd_counter = 0
	End Select


'	If DMD_Counter > 222 Then DMD_Counter = 0
'
'        FlushSplashQ
'		DMD_StartSplash "BALLS LEFT",	BallsRemaining(CurrentPlayer),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0	
'		DMD_StartSplash "EXTRABALLS",	ExtraBallsAwards(CurrentPlayer),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
'		DMD_StartSplash "PLAYFIELD",	"X " & PlayfieldMultiplier(CurrentPlayer),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
'		DMD_StartSplash "BONUS",		"X " & BonusMultiplier(CurrentPlayer),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
'		If SuperLoopCounter < 5 Then DMD_StartSplash "SUPERLOOPS", 5 - SuperLoopCounter & " RIGHTLOOP",FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0	
'		DMD_StartSplash "COMBOS", ComboCount(CurrentPlayer) ,FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
'		DMD_StartSplash "RAMPSHITS", Loopcount(CurrentPlayer) ,FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
'		DMD_StartSplash "SPINNERHITS", SpinnerSpins ,FontHugeOrange, FontHugeOrange,100 ,"oneimage" , 67 ,0,0
'		DMD_StartSplash "BUMPERHITS", BumperHits(CurrentPlayer) ,FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
'		DMD_StartSplash "SKILLSHOT",	FormatScore(SkillshotValue(CurrentPlayer) ),FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
'
'
'		DMD_StartSplash "COMPLETE","MISSIONS",FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
'		DMD_StartSplash "COLLECT","BLUELIGHTS",FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
'		DMD_StartSplash "ASSEMBLE","PRESENTS",FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
'		DMD_StartSplash "DELIVER","AT SCOOP",FontHugeOrange, FontHugeOrange,100 , "oneimage" , 67 ,0,0
'		DMD_StartSplash "DONT DO","DRUGS"	,FontHugeOrange, FontHugeOrange,100 , "oneimageINFO" , 67 ,0,0



'	debug.print DMD_Counter
End Sub

'    If bFreePlay Then
' '       DMD "", CL("FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
'    Else
'        If Credits > 0 Then
' '           DMD CL("CREDITS " & Credits), CL("PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
'        Else
' '           DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
'        End If
'    End If
''    DMD "        JPSALAS", "          PRESENTS", "d_jppresents", eNone, eNone, eNone, 3000, False, ""
''    DMD "", "", "d_title", eNone, eNone, eNone, 4000, False, ""
''    DMD "", CL("ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
''    DMD CL("HIGHSCORES"), Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
''    DMD CL("HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
''    DMD CL("HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
''    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
''    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
''    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
''    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
'End Sub

Dim NightmareShowCounter
Sub NightmareLigtsShow1
	NightmareShowCounter = NightmareShowCounter + 1 
	Select Case NightmareShowCounter
		case  10 : BlinkLights nightmarestart,nightmarestart+8,6,7,7,0
		case  30 : BlinkLights nightmarestart+9,nightmarestart+14,6,7,7,0
		case  50 : BlinkLights nightmarestart+15,nightmarestart+23,6,7,7,0
		case  70 : NightmareShowCounter = 0
	End Select
End Sub


Sub ChangeArrows ( nr )
	dim col
	If nr = 1 Then col = "red"
	If nr = 2 Then col = "blue"
	If nr = 3 Then col = "green"
	If nr = 4 Then col = "yellow"
	If nr = 5 Then col = "orange"
	If nr = 6 Then col = "purple"
	If nr = 7 Then col = "white"
	If nr = 0 Then col = "white"

	For x = 1 to 10 : ChangeArrowColor x,-1,col,60,"111011101110111111" : Next

End Sub

'InsertColors(x,3) = 255	: InsertColors(x,4) = 255 	: InsertColors(x,5) = 255 
'		case 1 : For x = 1 to 10 : InsertColors(x,3) = 255	: InsertColors(x,4) = 0		: InsertColors(x,5) = 0 : Next
'		case 2 : For x = 1 to 10 : InsertColors(x,3) = 0		: InsertColors(x,4) = 255	: InsertColors(x,5) = 0 : Next
'		case 3 : For x = 1 to 10 : InsertColors(x,3) = 0		: InsertColors(x,4) = 0		: InsertColors(x,5) = 255 : Next
'		case 4 : For x = 1 to 10 : InsertColors(x,3) = 225	: InsertColors(x,4) = 255	: InsertColors(x,5) = 0 : Next
'		case 5 : For x = 1 to 10 : InsertColors(x,3) = 0		: InsertColors(x,4) = 255	: InsertColors(x,5) = 200 : Next
'		case 6 : For x = 1 to 10 : InsertColors(x,3) = 255 	: InsertColors(x,4) = 150	: InsertColors(x,5) =0  : Next
'		case 7 : For x = 1 to 10 : InsertColors(x,3) = 150 	: InsertColors(x,4) = 0	: InsertColors(x,5) =255  : Next
'Sub ChangeArrowColor( x,state,col1,interval,pattern ) 
'	ChangeArrowColor 1,2,"orange",60,"111011101110111111"


Dim TipLastOne : TipLastOne = 0
Dim TipLastPos
Sub AnimateLastone
	TipLastOne = TipLastOne + 1
	if TipLastPos < 0 And TipLastOne > 18 Then TipLastPos = 0 : TipLastOne = 0 : Exit Sub
	If TipLastOne < 5 Then
		TipLastPos = TipLastPos - 1
	Elseif TipLastOne < 18 Then
		TipLastPos = TipLastPos + 1
	Else
		TipLastPos = TipLastPos - 1
	End If
	primitive293.rotx = 90 - TipLastPos

End Sub

Function FormatScore(ByVal Num)
    dim NumString
    NumString = CStr(abs(Num) )
	If len(NumString)>9 then NumString = left(NumString, Len(NumString)-9) & "," & right(NumString,9) 
	If len(NumString)>6 then NumString = left(NumString, Len(NumString)-6) & "," & right(NumString,6) 
	If len(NumString)>3 then NumString = left(NumString, Len(NumString)-3) & "," & right(NumString,3)
	FormatScore = NumString
End function

'******************************************************
'**** Lampz lights addon with no lampz ?
'******************************************************
Dim y
Dim Blink(160,11)
ResetAllBlink
Sub ResetAllBlink
	for x = 0 to 160
		for y = 0 to 11
			Blink(x,y) = 0
		Next
	Next
End Sub

Sub Insertupdate
	dim idx : idx = 0 
	dim ins 
	for each ins in InsertsAll 
		If Blink(idx,2)>0 Then	' Lightsequencer ? + multipleblinks
			If Blink(idx,8)>0 Then ' is there a delay ! ?=
				ins.state = 0
				Blink(idx,8) = Blink(idx,8) - 1
			Else
				Select Case Blink(idx,3)
					Case 0 : ins.state = 1 :  Blink(idx,5)=Blink(idx,4) : Blink(idx,3) = 1 
					Case 1 : Blink(idx,5)=Blink(idx,5)-1 : If Blink(idx,5) < 1 Then Blink(idx,3) = 2
					Case 2 : ins.state = 0   :  Blink(idx,7)=Blink(idx,6) : Blink(idx,3) = 3
					Case 3 : Blink(idx,7)=Blink(idx,7)-1 
							 If Blink(idx,7) < 1 Then
								Blink(idx,2)=Blink(idx,2)-1
								If Blink(idx,2)<1 Then Blink(idx,2)=0
								Blink(idx,3) = 0
							 End If
				End Select
			End If
		Else
			ins.state = Blink(idx,1)
		End If
	idx=idx+1
	Next
	Update_RGB_inserts
End Sub



Sub BlinkLights (For_nr,Next_nr,blinks,timeon,timeoff,delay)
	Dim i,Bdir,AddedDelay
	AddedDelay = delay
	If For_nr > Next_nr Then Bdir = -1 Else Bdir = 1
	For i = For_nr to Next_nr step Bdir
		Blink(i,2)=blinks
		Blink(i,3)=0 ' flag
		Blink(i,4)=timeon
		Blink(i,6)=timeoff
		Blink(i,8)=AddedDelay
'		If delay>0 Then all_c_lights(i).state = 0 ' turn off at start if there is delay
		AddedDelay = AddedDelay + delay
	Next
End Sub


' test boneramp transx
Dim ShakeTheBones : ShakeTheBones = 0
Dim ShakeTheBones2 : ShakeTheBones2 = 0
Dim LongShaker : LongShaker = 0
Sub Boneshaker
	Dim x,tmp
	tmp = 0
	Playsound "sfx_bikebell",1,BackboxVolume / 2 ' Addsound here
    If ShakeTheBones < 0 Then
        ShakeTheBones = ABS(ShakeTheBones) - 1
    Else
        ShakeTheBones = - ShakeTheBones + 1
    End If
	If LongShaker > 0 Then
		LongShaker = LongShaker + 1
		If LongShaker > 80 Then LongShaker = 0
	Else
		If ShakeTheBones = 0 Then LongShaker = 1
	End If

	Collection001(0).transx = shakethebones
	primitive017.transx = Collection001(0).transx / 2

	For x = 79 to 1 step -1 
		Collection001(x).transx =Collection001(x-1).transx 
	Next
	Collection001(80).transx =Collection001(79).transx * 2
End Sub
' 64-114  275 279 19 23 24 27 26 25 54 53 51 63-55 281-293

Sub Boneshaker2
	Dim x
	Playsound "sfx_bikebell",1,BackboxVolume / 2  ' Addsound here
    If ShakeTheBones2 < 0 Then
        ShakeTheBones2 = ABS(ShakeTheBones2) - 1
    Else
        ShakeTheBones2 = - ShakeTheBones2 + 1
    End If
	For each x in Collection001 : x.transx = ShakeTheBones2 : Next
	Primitive293.transx = ShakeTheBones2 * 2
End Sub

'******************************************************
'**** ALL PLAYFIELD TRIGGERS  Light effects only
'******************************************************
Sub LightTrigger001_hit ' down xmas_ramp 

	SetXlight   1, 16,10,5,2,2   ' all these 5 at once
	SetXlight  17, 61,10,5,2,3   ' 
	SetXlight 130, 62,10,5,2,0   ' 
	SetXlight 131,200,10,5,2,0   ' 
	Dim x
	For x = 201 to 230 
		SetXlight x,x,15,3,145,4
	Next
	Playsound"sfx_dwnlong",1,BackBoxVolume * 0.5
End sub


Sub Trigger020_hit
	Shakehousefront 10
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "trigger020"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "trigger020" : Exit Sub
	BlinkLights 40,40,5,8,3,0
End Sub


Sub Trigger018_unHit
	BlinkLights 57,57,3,5,1,0
	If activeball.velx > 0 Then activeball.velx = activeball.velx + 2
	If activeball.vely < 0 Then activeball.vely = activeball.vely - 2

End Sub


'******************************************************
'**** SPINNERS
'******************************************************

Dim SpinnerCount
Dim SpinnerTime : SpinnerTime = 99000999

Sub Trigger019_hit
	If Tilted or activeball.vely > 0 then Exit Sub
	If Combotimer > DMD_Frame Then AwardComboShot "leftspinner" Else  RestartComboTimer : Combotimer = DMD_Frame
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "leftspinner"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "leftspinner" : Exit Sub

	updatemodecombolights "leftspinner"
	BoogieHit1
	MayorSounds
	If ArrowLights(1,3) = 2 Then ModeHit "leftspinner"
End Sub

Sub Spinner003_Spin 'left
    DOF 265, DOFPulse
    PlaySoundAt "fx_spinner", Spinner003
    If Tilted Then Exit Sub
	Playsoundat "sfx_whoosh",drain
    DOF 202, DOFPulse

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "Spinner003"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "Spinner003" : Exit Sub

	If TranslateLetters(ModeRunning(CurrentPlayer)) = 16 Then
		ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2	
		ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
		If ModeProgress = 10 Then DMD_StartSplash "+10 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		If ModeProgress = 20 Then ModeCollectionDone
	End If
	If TranslateLetters(ModeRunning(CurrentPlayer)) = 13 Then
		ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2	
		ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
		If ModeProgress = 10 Then DMD_StartSplash "+40 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		If ModeProgress = 20 Then DMD_StartSplash "+30 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		If ModeProgress = 30 Then DMD_StartSplash "+20 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
'		If ModeProgress = 40 Then DMD_StartSplash "+10 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		If ModeProgress = 50 Then ModeCollectionDone
	End If
	If TranslateLetters(ModeRunning(CurrentPlayer)) = 17 Then
		ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2	
		ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
		dim tmp
		tmp = 80-ModeProgress
		If tmp < 0 Then tmp = 0
		updatecountdown tmp
		If ModeProgress < 80 Then DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,55 , "80daysupdate" ,999,0,0
		If ModeProgress = 20 Then makingxmascalls
		If ModeProgress = 30 Then makingxmascalls
		If ModeProgress = 40 Then makingxmascalls
		If ModeProgress = 50 Then makingxmascalls
		If ModeProgress = 60 Then makingxmascalls
		If ModeProgress = 70 Then makingxmascalls

		If ModeProgress = 80 Then ModeCollectionDone : DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,120 , "80dayscomplete" ,111,0,0
	End If

	SpinnerSpins = SpinnerSpins + 1 ' eob

	If SpinnerTime > DMD_frame Then
		SpinnerCount = SpinnerCount + 1
		If SpinnerCount > 20 Then SpinnerCount = 20
'		If SpinnerCount > 16 Then Debug.Print "spinnercount " & SpinnerCount
		SpinnerTime = DMD_Frame + 50 ' 50 = 1sec
	Else
		SpinnerTime = DMD_Frame + 50 ' 50 = 1sec
		SpinnerCount = 1
	End If


	If MegaSpinnersActive Then
		megaspinnerjackpot
		Addscore score_megaspinns
		If SpinnerCount = 18 Then
			AddScore score_megaspinns * 2
		End If
	Else
		Addscore score_spinnerspin + ( score_multispin * SpinnerCount )
	End If
	BlinkLights 22,22,1,4,3,0

    LeftSpinnerHits(CurrentPlayer) = LeftSpinnerHits(CurrentPlayer) + 1

    CenterSpinnerHits(CurrentPlayer) = CenterSpinnerHits(CurrentPlayer) + 1
    If Blink(52,1) = 0 And CenterSpinnerHits(CurrentPlayer) >= score_spinsneededforeob + BonusMultiplier(CurrentPlayer) * 5 Then
		blink(55,1) = 2
    End If
	If bJackMBStarted Then JasonHit1
End Sub



'******************************************************
'**** BUMPER
'******************************************************

dim Bumersquich
Sub Bumper1_Timer
	Bumersquich = Bumersquich + 1
	Select Case Bumersquich
		case 1 : primitive280.z = 65 - 1	: primitive171.z = 65 - 1
primitive280.roty = 35 : primitive171.roty = 35
PlaySoundat "fx_GiOn",drain
		case 2 : primitive280.z = 65 - 2	: primitive171.z = 65 - 2
ShakePlunger 4
		case 3 : primitive280.z = 65 - 3	: primitive171.z = 65 - 3
primitive280.roty = 33 : primitive171.roty = 33
		case 4 : primitive280.z = 65 - 2.5	: primitive171.z = 65 - 2.5
				 primitive280.blenddisablelighting = 55
		case 5 : primitive280.z = 65 - 2	: primitive171.z = 65 - 2
primitive280.roty = 31 : primitive171.roty = 31
		case 6 : primitive280.z = 65 - 1.5	: primitive171.z = 65 - 1.5
		case 7 : primitive280.z = 65 - 1	: primitive171.z = 65 - 1
primitive280.roty = 33 : primitive171.roty = 33
		case 8 : primitive280.z = 65 - .5	: primitive171.z = 65 - .5

		case 9 : primitive280.z = 65  		: primitive171.z = 65 
				Bumersquich = 0 : Bumper1.timerenabled = False : Exit Sub
	End Select
End Sub

Sub Bumper1_Hit ' W6
    DOF 269, DOFPulse
	Dim x
	Stoprando = 88
    If Tilted Then Exit Sub
	For x = 201 to 230 
		SetXlight x,x,22,4,0,4
	Next
	Bumper1.timerenabled = True

	DMD_Pumpkin = 1

    Dim tmp
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    RandomSoundBumperBottom Bumper1
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    DOF 138, DOFPulse
    ' add some points
    FlashForms libumper, 700, 75, 0
	Flashforms Flasher002, 700, 50, 0

	BumperSword 'idig
	AddScore score_bumperhit : BlinkLights 38,38,3,4,4,0
    BumperHits(CurrentPlayer) = BumperHits(CurrentPlayer) + 1

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "bumper1"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "bumper1" : Exit Sub


	If ArrowLights(17,3) = 2 Then ModeHit "bumper1"
	NewPresentsNextMode
	BumperSpecial = BumperSpecial + 1
	If Blink(38,1) = 2 Then
		BumperSpecial = 0
		BlinkLights 38,38,7,6,6,0
		arrowlights(17,1) = 0  ' Blink(38,1) = 0
		AddScore score_bumperSpecial
		DMD_Pumpkins = 1

' DMD CL("BUMPER JACKPOT"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 1500, True, "vo_jackpot" &RndNbr(6)

	End If
	If BumperSpecial = 9 Then
		arrowlights(17,1) = 2		'Blink(38,1)=2
		BlinkLights 38,38,10,7,7,0
	End If


    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper1"
End Sub

Sub BumperSword
    PlaySound "sfx_sword" &RndNbr(5) ,1,BackboxVolume/1.3 'idig 'idig
End Sub







'******************************************************
'**** SKILLSHOT
'******************************************************


Sub Gate001_Hit 'superskillshot
    If Tilted Then Exit Sub
    Addscore score_triggerhits
    If bSkillShotReady Then AwardSuperSkillshot
End Sub


Sub Trigger006_Hit 'skillshot 1
    PLaySoundAt "fx_sensor", Trigger006
    If Tilted Then Exit Sub
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "trigger006"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "trigger006" : Exit Sub

    Addscore score_triggerhits
    If bSkillShotReady AND li062.State Then AwardSkillshot
End Sub

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
    LightSeqSkillshot.Play SeqAllOff
'   DMD CL("HIT LIT LIGHT"), CL("FOR SKILLSHOT"), "", eNone, eNone, eNone, 3000, True, ""
    li062.State = 2
    li063.State = 2
	li063.blinkpattern = "10100100010"
	li063.blinkinterval = 37
	li062.blinkpattern = "10100100010"
	li062.blinkinterval = 37
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
	ChangeGi white
	PFmultiCounter = 240 ' start new pf multi timer 240 seconds
    bRotateLights = True
    LightSeqSkillshot.StopPlay
    Li062.State = 0
    li063.State = 0
	li063.blinkpattern = "10"
	li063.blinkinterval = 125
	li062.blinkpattern = "10"
	li062.blinkinterval = 125
'Delivered_mission(CurrentPlayer) =  8
'DoDeliverNextball = True

	If Delivered_mission(CurrentPlayer) =  8 And DoDeliverNextball = True Then
		DMD_StartSplash "SCOOP START","NIGHTMARE" ,FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		blink(43,1) = 2
	End If

	If Delivered_mission(CurrentPlayer) = 23 And DoDeliverNextball = True Then
		DMD_StartSplash "SCOOP START","HOLIDAY BATTLE" ,FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		blink(43,1) = 2
	End If
	UpdatePresentsLights
End Sub

Sub AwardSkillshot()
	DMD_StartSplash "SKILLSHOT",Formatscore ( SkillShotValue(CurrentPlayer)),FontHugeOrange, FontHugeRedhalf,70 , "blinktop1" ,19,0,0

    ResetSkillShotTimer_Timer

    'show dmd animation
'    DMD CL("SKILLSHOT"), CL(FormatScore(SkillshotValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1000, False,
	playsound "sfx_crowdcheer",1,BackboxVolume  /3.45
'    DMD CL("SKILLSHOT"), CL(FormatScore(SkillshotValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1000, True,
	PlaySound "_skillshot",1,BackboxVolume
    DOF 127, DOFPulse
    Addscore2 SkillShotValue(CurrentPlayer)
    ' increment the skillshot value with 100.000
    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + Score_addskillshot
    'do some light show
    GiEffect 2
    LightEffect 2
End Sub


Sub AwardSuperSkillshot()
	DMD_StartSplash "*SUPER - SHOT* ",Formatscore ( SuperSkillShotValue(CurrentPlayer)) ,FontHugeOrange, FontHugeRedhalf,70 , "blinktop1" ,19,0,0
    ResetSkillShotTimer_Timer

    'show dmd animation
'    DMD CL("SUPER SKILLSHOT"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1000, False, 
	playsound "sfx_crowdcheer",1,BackboxVolume  /3.45
'    DMD CL("SUPER SKILLSHOT"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1000, True, 
	PlaySound "_goodshot",1,BackboxVolume
    DOF 127, DOFPulse
    Addscore2 SuperSkillshotValue(CurrentPlayer)
    ' increment the superskillshot value with 1.000.000
    SuperSkillshotValue(CurrentPlayer) = SuperSkillshotValue(CurrentPlayer) + Score_addSuperSS
    'do some light show
    GiEffect 2
    LightEffect 2
End Sub

Sub aSkillshotTargets_Hit(idx) 'stop the skillshot if any other target is hit
    If bSkillshotReady then ResetSkillShotTimer_Timer
End Sub



'*******************
'*** Lanes bottom 4
'*******************

Sub Trigger001_Hit
    DOF 151, DOFPulse  'J light
    Playsoundat "fx_sensor", Trigger001
    DOF 207, DOFPulse
    If Tilted Then Exit Sub

	Revive_Left
    Addscore score_Outlanes_hit
	blinklights 1,1,2,5,5,0

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "leftoutlane"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "leftoutlane" : Exit Sub

	
	If Mystery(CurrentPlayer, 1) = 0 Then
		Mystery(CurrentPlayer, 1) = 1
		JackLight
		CheckMystery
		BlinkLights 2,4,2,5,5,0
		blinklights 1,1,6,8,8,0
	Else
		If Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4) > 3 Then Mystery(CurrentPlayer, 1) = 2  : mysterylightsfull
		blinklights 1,1,2,5,5,0
	End If

End Sub

Sub Trigger002_Hit
    DOF 152, DOFPulse  'A light
    PLaySoundAt "fx_sensor", Trigger002
    DOF 207, DOFPulse
    If Tilted Then Exit Sub

    Addscore score_Inlanes_hit
	If activeball.vely>0 then PlaySound"_SqueakDwn",1,BackBoxVolume * 0.2 Else PlaySound"_SqueakUp",1,BackBoxVolume * 0.2
	blinklights 2,2,2,5,5,0
	leftInlaneSpeedLimit
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "leftinlane"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "leftinlane" : Exit Sub

	If Mystery(CurrentPlayer, 2) = 0 Then
		Mystery(CurrentPlayer, 2) = 1
		Jacklight
		CheckMystery : 
		BlinkLights 1,4,2,5,5,0
		blinklights 2,2,6,8,8,0
	Else
		If Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4) > 3 Then Mystery(CurrentPlayer, 2) = 2  : mysterylightsfull

	End If
End Sub

Sub Trigger003_Hit
    DOF 153, DOFPulse  'C light
    PLaySoundAt "fx_sensor", Trigger003
    DOF 207, DOFPulse
    If Tilted Then Exit Sub
	blinklights 3,3,2,5,5,0
	rightInlaneSpeedLimit
    Addscore score_Inlanes_hit
	If activeball.vely>0 then Playsound"_SqueakDwn",1,BackBoxVolume * 0.2	Else Playsound"_SqueakUp",1,BackBoxVolume * 0.2
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "rightinlane" 
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "rightinlane" : Exit Sub

 	If Mystery(CurrentPlayer, 3) = 0 Then
		Mystery(CurrentPlayer, 3) = 1
		Jacklight
		CheckMystery
		BlinkLights 1,4,2,5,5,0
		blinklights 3,3,6,8,8,0
	Else
		If Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4) > 3 Then Mystery(CurrentPlayer, 3) = 2  : mysterylightsfull
	End If
End Sub

Sub Trigger004_Hit
    DOF 154, DOFPulse  'K light
    PLaySoundAt "fx_sensor", Trigger004
    DOF 207, DOFPulse
    If Tilted Then Exit Sub
	Revive_Right
		blinklights 4,4,2,5,5,0
    Addscore score_Outlanes_hit

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "rightoutlane"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "rightoutlane" : Exit Sub



	If Mystery(CurrentPlayer, 4) = 0 Then
		Mystery(CurrentPlayer, 4) = 1
		jacklight
		CheckMystery
		BlinkLights 1,3,2,5,5,0
		blinklights 4,4,6,8,8,0
	Else
		If Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4) > 3 Then Mystery(CurrentPlayer, 4) = 2  : mysterylightsfull

	End If
End Sub



Sub leftInlaneSpeedLimit
	'Wylte's implementation
'    debug.print "Spin in: "& activeball.AngMomZ
'    debug.print "Speed in: "& activeball.vely
	If activeball.vely < 0 Then Exit Sub 							'don't affect upwards movement
    activeball.AngMomZ = -abs(activeball.AngMomZ) * RndNum(3,6)
    If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
    If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)

    If abs(activeball.vely) > 5 Then activeball.vely = 0.8 * activeball.vely
    If abs(activeball.vely) > 10 Then activeball.vely = 0.8 * activeball.vely
    If abs(activeball.vely) > 15 Then activeball.vely = 0.8 * activeball.vely
    If activeball.vely > 16 Then activeball.vely = RndNum(14,16)
    If activeball.vely < -16 Then activeball.vely = RndNum(-14,-16)
'    debug.print "Spin out: "& activeball.AngMomZ
'    debug.print "Speed out: "& activeball.vely
End Sub


Sub rightInlaneSpeedLimit
	'Wylte's implementation
'    debug.print "Spin in: "& activeball.AngMomZ
'    debug.print "Speed in: "& activeball.vely
	If activeball.vely < 0 Then Exit Sub 							'don't affect upwards movement

    activeball.AngMomZ = abs(activeball.AngMomZ) * RndNum(2,4)
    If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
    If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)

	If abs(activeball.vely) > 5 Then activeball.vely = 0.8 * activeball.vely
    If abs(activeball.vely) > 10 Then activeball.vely = 0.8 * activeball.vely
    If abs(activeball.vely) > 15 Then activeball.vely = 0.8 * activeball.vely
    If activeball.vely > 16 Then activeball.vely = RndNum(14,16)
    If activeball.vely < -16 Then activeball.vely = RndNum(-14,-16)
'    debug.print "Spin out: "& activeball.AngMomZ
'    debug.print "Speed out: "& activeball.vely
End Sub



Sub mysterylightsfull 
	If Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4) = 8 Then
		MysteryCollected(CurrentPlayer) = MysteryCollected(CurrentPlayer) + 1
		Mystery(CurrentPlayer, 1) = 1
		Mystery(CurrentPlayer, 2) = 1
		Mystery(CurrentPlayer, 3) = 1
		Mystery(CurrentPlayer, 4) = 1

		Select Case MysteryCollected(CurrentPlayer)
			Case 1,2,4 
'Const	score_jackBonus1			=  500000   = zJackaward1+4
'Const	score_jackBonus2			= 1000000	= zJackaward1+5   1= off
'	"jackawardeb" zJackaward1+2   1= off
				DMD_StartSplash " ","  ",FontHugeOrange, FontHugeRedhalf,130 , "jackaward1" ,555,0,0
				AddScore2 500000
			Case 3,9 : 
				DMD_StartSplash " ","  ",FontHugeOrange, FontHugeRedhalf,180 , "jackawardmission" ,555,0,0

				AddScore2 score_ModeCollected
				BlinkAllPresents 5,1 
				TurnOffAllBlueLights
				Blink(43,1) = 2 : BlinkLights 43,43,12,6,6,0
				greenoogie = True
				
				Select Case TranslateLetters(ModeRunning(CurrentPlayer))
				' for special endings
				End Select
				UpdatePresentsLights

			Case 5 : 
				DMD_StartSplash " ","  ",FontHugeOrange, FontHugeRedhalf,130 , "jackaward2" ,555,0,0
				AddScore2 1000000
			Case 6 : 
				Extraball_3(CurrentPlayer) = 1 
				FlushSplashQ	
				DMD_StartSplash " ","  ",FontHugeOrange, FontHugeRedhalf,144 , "jackawardeb" ,555,0,0
				PlayVoice "vo_extraballislit"
				UpdateExtraballLight
				NightmareShowCounter = 1
			Case Else 
				DMD_StartSplash " ","  ",FontHugeOrange, FontHugeRedhalf,130 , "jackaward2" ,555,0,0
				AddScore2 1000000
		End Select


	End If
	UpdateMysteryLights
End Sub


Sub UpdateMysteryLights 				'update lane lights
	If BallsOnPlayfield = 0 then exit Sub
	If Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4) > 3 Then  'blinking
		If Mystery(CurrentPlayer, 1) = 1 Then Blink(1,1) = 2 Else Blink(1,1) = 0
		If Mystery(CurrentPlayer, 2) = 1 Then Blink(2,1) = 2 Else Blink(2,1) = 0
		If Mystery(CurrentPlayer, 3) = 1 Then Blink(3,1) = 2 Else Blink(3,1) = 0
		If Mystery(CurrentPlayer, 4) = 1 Then Blink(4,1) = 2 Else Blink(4,1) = 0
	Else
		Blink(1,1) = Mystery(CurrentPlayer, 1)
		Blink(2,1) = Mystery(CurrentPlayer, 2)
		Blink(3,1) = Mystery(CurrentPlayer, 3)
		Blink(4,1) = Mystery(CurrentPlayer, 4)	
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


' this is a kind of award after completing the inlane and outlanes

Sub CheckMystery 'if all the inlanes and outlanes are lit then lit the mystery award
    dim i,x
	x = Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4)
	
	Select Case x
		Case 1 : 	DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,70 , "blink2images" ,20,24,0
		Case 2 : 	DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,70 , "blink2images" ,21,20,0
		Case 3 : 	DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,70 , "blink2images" ,22,21,0
		Case 4 : 	DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,70 , "blink2images" ,30,30,0
					BallsInLock(CurrentPlayer) = 1
					UpdateLockLights
					LockIsLit
	End Select
    UpdateMysteryLights
End Sub


Sub Trigger011_Hit 'cabin playfield , only active when the ball move upwards
	PLaySoundAt "fx_sensor", Trigger011
	ActiveBall.vely = ActiveBall.vely * 0.5
	ActiveBall.velx = ActiveBall.velx * 0.7

    If Tilted OR ActiveBall.VelY > 0 Then Exit Sub
	GiEffect 3
	blinklights 53,54,4,5,1,0
	clight cLightGarland,1500,2
    Addscore score_triggerhits
	If Combotimer > DMD_Frame Then AwardComboShot "houseup" Else RestartComboTimer : Combotimer = DMD_Frame + 400 ' extra for ramps
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "houseup"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "houseup" : Exit Sub
    If bJackMBStarted Then JasonHit5
	If ArrowLights(5,3) = 2 Then ModeHit "houseup"
	updatemodecombolights "houseup" 

	LockBallsForJasonMB
    LastSwitchHit = "Trigger011"
End Sub


Sub UpdateLockLights
		Select Case BallsInLock(CurrentPlayer)
			Case 0: Blink(19,1)=0 : Blink(20,1)=0 : Blink(21,1)=0
					blinklights 19,21,3,5,4,0
			Case 1:	Blink(21,1)=2                                 'enabled the first lock
					blinklights 19,21,9,7,7,0
			Case 2:	Blink(21,1)=1 : Blink(20,1)=2                 'lock 1
					blinklights 19,21,9,7,7,0
			Case 3:	Blink(20,1)=1 : Blink(19,1)=2                 'lock 2
					blinklights 19,21,9,7,7,0
			Case 4:	Blink(19,1)=0 : Blink(20,1)=0 : Blink(21,1)=0 'lock 3
					blinklights 21,19,33,10,7,3
		End Select
End Sub

Dim JasonCollected, JasonSJP_Counter
Dim JasonSuper

Sub StartJackMultiball

    Dim i
    If bMultiBallMode Then Exit Sub 'do not start if already in a multiball mode
    bJackMBStarted = True
    EnableBallSaver 20
	PlayVoice "vo_itsoogiesturn"

	DelayStart 2500,2  ' 2mb's

	ChangeArrowColor 1,2,"orange",60,"111011101110111111"
	ChangeArrowColor 2,2,"orange",60,"111011101110111111"
	ChangeArrowColor 3,2,"orange",60,"111011101110111111"
	ChangeArrowColor 4,2,"orange",60,"111011101110111111"
'	ChangeArrowColor 5,2,"orange",60,"111011101110111111" -  house off until super ?
	ChangeArrowColor 6,2,"orange",60,"111011101110111111"
	ChangeArrowColor 7,2,"orange",60,"111011101110111111"
	ChangeArrowColor 8,2,"orange",60,"111011101110111111"
	ChangeArrowColor 9,2,"orange",60,"111011101110111111"
'	lightnr,state,col1,col2,interval,pattern 
	JasonCollected = 0
	JasonSJP_Counter = 0
	JasonSuper = 0  ' add more score when collecting super ( not collected = smaler scoring on the rest
    ChangeSong
End Sub

Dim DelayBalls
Sub DelayStart(time,addballs)
	DelayMultiball.enabled = False
	DelayMultiball.interval = time
	DelayMultiball.enabled = True
	DelayBalls = addballs
End Sub

Sub DelayMultiball_Timer
	DelayMultiball.enabled = False
    AddMultiball DelayBalls
End Sub


Sub JasonHit1 : If ArrowLights(1,1) = 2 Then  ArrowLights(1,1) = 0 : BlinkLights 23,31,2,7,7,0 : Blinklights 23,23,7,7,7,0 : Jason_Shot : End If : End Sub			'Spinner003
Sub JasonHit2 : If ArrowLights(2,1) = 2 Then  ArrowLights(2,1) = 0 : BlinkLights 23,31,2,7,7,0 : Blinklights 24,24,7,7,7,0 : Jason_Shot : End If : End Sub			'Trigger016(Ramp)		
Sub JasonHit3 : If ArrowLights(3,1) = 2 Then  ArrowLights(3,1) = 0 : BlinkLights 23,31,2,7,7,0 : Blinklights 25,25,7,7,7,0 : Jason_Shot : End If : End Sub			'Target007_Hit(captive)
Sub JasonHit4 : If ArrowLights(4,1) = 2 Then  ArrowLights(4,1) = 0 : BlinkLights 23,31,2,7,7,0 : Blinklights 26,26,7,7,7,0 : Jason_Shot : End If : End Sub			'Trigger007




Dim Scoring		' delayd show scoring jackpots
Sub JasonHit5																																				' big house
	Dim tmp
	DOF 127, DOFPulse
	LightEffect 2
	GiEffect 2


	If Blink(41,1) = 2 Then ' lightsuperjp
		Blink(41,1 ) = 0 
		BlinkLights 41,42,7,7,7,0
		Blink(42,1) = 2
		PlayVoice "vo_superjackpotlit"
		DMD_StartSplash "SUPER JACKPOT","IS LIT" ,FontHugeOrange, FontHugeRedhalf,90 , "blink2text1" ,28,29,0
		Scoring = score_JasonJackpot + score_JasonSuperBonus * JasonSuper
		AddScore2 Scoring
	End If

	If ArrowLights(5,1) = 2 Then ' collect JP
		ArrowLights(5,1) = 0
		BlinkLights 23,31,2,7,7,0
		Blinklights 27,27,7,7,7,0
		Jason_Shot
	End If

End Sub




Sub LockBallsForJasonMB

	   Select Case BallsInLock(CurrentPlayer)    ' lock balls
			Case 1: 'lock 1							
				PlayVoice "vo_ball1locked"
                DOF 158, DOFPulse  'BALL 1 LOCK
				DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,123 , "blink2fast1" ,40,41,0
				BallsInLock(CurrentPlayer) = 2
				UpdateLockLights
				changeGi green : GiEffect 6 : gi016.timerenabled = True : gi015.timerenabled = False
			Case 2: 'lock 2
				BallsInLock(CurrentPlayer) = 3
				PlayVoice "vo_ball2locked"
                DOF 159, DOFPulse  'BALL 2 LOCK
				DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,123 , "blink2fast1" ,42,43,0
				UpdateLockLights
				changeGi green : GiEffect 6 : gi016.timerenabled = True : gi015.timerenabled = False
			Case 3 'lock 3 - start multiball if there is not a multiball already
'				If NOT bMultiBallMode Then
					PlayVoice "vo_ball3locked"
                    DOF 160, DOFPulse  'BALL 3 LOCK
                    DOF 161, DOFPulse  'MULTIBALL
					DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,500 , "jasonmb" ,44,45,0
					BallsInLock(CurrentPlayer) = 4
					UpdateLockLights
					lighteffect 2
					Flasheffect 5
					StartJackMultiball
					changeGi green : GiEffect 6 : gi016.timerenabled = True : gi015.timerenabled = False
'				End If
		End Select

End Sub	

Sub JasonHit6 : If ArrowLights(6,1) = 2 Then  ArrowLights(6,1) = 0 : BlinkLights 23,31,2,7,7,0 : Blinklights 28,28,7,7,7,0 : Jason_Shot : End If : End Sub			'trigger014 centerspin
Sub JasonHit7 : If ArrowLights(7,1) = 2 Then  ArrowLights(7,1) = 0 : BlinkLights 23,31,2,7,7,0 : Blinklights 29,29,7,7,7,0 : Jason_Shot : End If : End Sub			'trigger015
Sub JasonHit8		'trigger009
	Dim tmp
	If ArrowLights(8,1) = 2 Then
		ArrowLights(8,1) = 0
		BlinkLights 23,31,2,7,7,0
		Blinklights 30,30,7,7,7,0 
		Jason_Shot
	End If


	If Blink(42,1) = 2 then ' reward SuperJackpot
		Blink(42,1) = 0
		changeGi RED : GiEffect 6 : gi015.timerenabled = True
		JasonSJP_Counter = 0

		Scoring = score_JasonSJP + score_JasonSuperBonus * JasonSuper * 2
		JasonSuper = JasonSuper + 1
		For tmp = 1 to 9		' change colors for all blinking lights
			If tmp <> 5 Then
				Select Case JasonSuper mod 6 ' 0-5
					Case 0 : ChangeArrowColor tmp,-1,"orange",60,"111011101110111111"
					Case 1 : ChangeArrowColor tmp,-1,"red",60,"111011101110111111"
					Case 2 : ChangeArrowColor tmp,-1,"green",60,"111011101110111111"
					Case 3 : ChangeArrowColor tmp,-1,"purple",60,"111011101110111111"
					Case 4 : ChangeArrowColor tmp,-1,"teal",60,"111011101110111111"
					Case 5 : ChangeArrowColor tmp,-1,"yellow",60,"111011101110111111"
					Case 6 : ChangeArrowColor tmp,-1,"blue",60,"111011101110111111"' saved for missions
					Case 7 : ChangeArrowColor tmp,-1,"white",60,"111011101110111111" ' hurryups
				End Select
			End If
		Next
		AddScore2 Scoring
		DMD_Jackpot = 1
		If int(rnd(1)*2) = 1 Then
			PlayVoice "vo_perfect"
		Else
			PlayVoice "vo_superjackpot"
		End If
	End If

End Sub	

Sub JasonHit9 : If ArrowLights(9,1) = 2 Then  ArrowLights(9,1) = 0 : BlinkLights 23,31,2,7,7,0 : Blinklights 31,31,7,7,7,0 : Jason_Shot : End If : End Sub			'trigger013


Sub Jason_Shot
	LightEffect 2 : changeGi orange : GiEffect 6 : gi015.timerenabled = True ' from old jasonmb

	JasonCollected = JasonCollected + 1
	PlaySound "vo_jackpotoogi",1,BackboxVolume * 0.7 
	If Blink(41,1) = 0 And Blink(42,1) = 0 Then JasonSJP_Counter = JasonSJP_Counter + 1 : debug.print "JasonSJP_Counter " & JasonSJP_Counter

	If JasonCollected > 6 Then 		' add more lights at Random
		Dim tmp, Scoring
		tmp = int(rnd(1)*9) + 1
		If ArrowLights(tmp,1) = 2 Then tmp = tmp + 1 : If tmp > 9 Then tmp = 1
		If ArrowLights(tmp,1) = 2 Then tmp = tmp : If tmp > 9 Then tmp = 1
		Select Case JasonSuper mod 6 ' change colors for each Super collected
			Case 0 : ChangeArrowColor tmp,2,"orange",60,"111011101110111111"
			Case 1 : ChangeArrowColor tmp,2,"red",60,"111011101110111111"
			Case 2 : ChangeArrowColor tmp,2,"green",60,"111011101110111111"
			Case 3 : ChangeArrowColor tmp,2,"purple",60,"111011101110111111"
			Case 4 : ChangeArrowColor tmp,2,"teal",60,"111011101110111111"
			Case 5 : ChangeArrowColor tmp,2,"blue",60,"111011101110111111"
			Case 6 : ChangeArrowColor tmp,2,"yellow",60,"111011101110111111"
			Case 7 : ChangeArrowColor tmp,2,"white",60,"111011101110111111"
		End Select
		debug.print "newlight started: " & tmp 
	End If

	Scoring = score_JasonJackpot + score_JasonSuperBonus * JasonSuper
	AddScore2 Scoring
	DMD_StartSplash "JACKPOT",Formatscore ( Scoring ) ,FontHugeOrange, FontHugeRedhalf,90 , "blink2text1" ,28,29,0

'	If Blink(41,1) = 0 And Blink(42,1) = 0  Then ' jackpots or not 
'	Else
'			Scoring = score_JasonSuperBonus * ( JasonSuper + 1 ) ' just smal scoring when super is lit 
'			AddScore2 Scoring
'	End If

	If JasonSJP_Counter = 6 Then
		Blink(41,1) = 2
		DMD_StartSplash "HOUSE TO ","LIGHT SUPER JP" ,FontHugeOrange, FontHugeRedhalf,90 , "blink2text1" ,28,29,0

		PlayVoice "vo_getthesuperjackpot"


	End If
End Sub


Sub StopJackMultiball
    bJackMBStarted = False
	JackResetAvail(CurrentPlayer) = True 
	JackResetCounter(CurrentPlayer) = 0

	For x = 1 to 17 ' off with arrows 
		ArrowLights(x,1) = 0
	Next
	Blink(41,1) = 0
	Blink(42,1) = 0
    BallsInLock(CurrentPlayer) = 0
    JasonCollected = 0	
	JasonSJP_Counter = 0
	JasonSuper = 0
End Sub





'***********************************
'**** Weapons - Playfield multiplier
'***********************************
' Spinner003  Trigger016(Ramp)  Target003(captive) trigger014(centerspin) trigger015  trigger013 
Sub Boogiehit1 : If WeaponHits(CurrentPlayer, 1) = 1 Then BlinkLights 13,18,2,4,4,0 : blinklights 13,13,7,8,8,0 : WeaponHits(CurrentPlayer, 1) = 0 : CheckWeapons : End If : End Sub
Sub Boogiehit2 : If WeaponHits(CurrentPlayer, 2) = 1 Then BlinkLights 13,18,2,4,4,0 : blinklights 14,14,7,8,8,0 : WeaponHits(CurrentPlayer, 2) = 0 : CheckWeapons : Boogie : End If : End Sub
Sub Boogiehit3 : If WeaponHits(CurrentPlayer, 3) = 1 Then BlinkLights 13,18,2,4,4,0 : blinklights 15,15,7,8,8,0 : WeaponHits(CurrentPlayer, 3) = 0 : CheckWeapons : Boogie : End If : End Sub
Sub Boogiehit4 : If WeaponHits(CurrentPlayer, 4) = 1 Then BlinkLights 13,18,2,4,4,0 : blinklights 16,16,7,8,8,0 : WeaponHits(CurrentPlayer, 4) = 0 : CheckWeapons : Boogie : End If : End Sub
Sub Boogiehit5 : If WeaponHits(CurrentPlayer, 5) = 1 Then BlinkLights 13,18,2,4,4,0 : blinklights 17,17,7,8,8,0 : WeaponHits(CurrentPlayer, 5) = 0 : CheckWeapons : Boogie : End If : End Sub
Sub Boogiehit6 : If WeaponHits(CurrentPlayer, 6) = 1 Then BlinkLights 13,18,2,4,4,0 : blinklights 18,18,7,8,8,0 : WeaponHits(CurrentPlayer, 6) = 0 : CheckWeapons : End If : End Sub

Sub CheckWeapons
'	Boogie ' sound
    Dim a, j
    a = 0
    For j = 1 to 6
        a = a + WeaponHits(CurrentPlayer, j)
    Next
    'debug.print a
    If a = 0 Then
        UpgradeWeapons
    Else
'        LightSeqWeaponLights.UpdateInterval = 25
 '       LightSeqWeaponLights.Play SeqBlinking, , 15, 25
        UpdateWeaponLights
		PlaySound "sfx_bikebell",1,BackboxVolume/2  'idig
    End If
End Sub

Sub UpdateWeaponLights 'CurrentPlayer
	Blink(13,1) = WeaponHits(CurrentPlayer, 1)
	Blink(14,1) = WeaponHits(CurrentPlayer, 2)
	Blink(15,1) = WeaponHits(CurrentPlayer, 3)
	Blink(16,1) = WeaponHits(CurrentPlayer, 4)
	Blink(17,1) = WeaponHits(CurrentPlayer, 5)
	Blink(18,1) = WeaponHits(CurrentPlayer, 6)
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
'    UpdateWeaponLights2   ' collected letters =?=
    AddPlayfieldMultiplier 1
    ResetWeaponLights

End Sub

Sub AddPlayfieldMultiplier(n)
    Dim snd
    Dim NewPFLevel
    ' if not at the maximum level x
    if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier) then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n

	' show new pf level on DMD
		Select Case NewPFLevel
			Case 2 : DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,50 , "oneimage" ,31,0,0
			Case 3 : DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,50 , "oneimage" ,32,0,0
			Case 4 : DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,50 , "oneimage" ,33,0,0
			Case 5 : DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,50 , "oneimage" ,34,0,0
		End Select

        SetPlayfieldMultiplier(NewPFLevel)
        PlayThunder
        LightEffect 4
        Select Case NewPFLevel
            Case 2:PlayVoice "vo_2xplayfield"
            Case 3:PlayVoice "vo_3xplayfield"
            Case 4:PlayVoice "vo_4xplayfield"
            Case 5:PlayVoice "vo_5xplayfield"
        End Select
    Else 'if the max is already lit
        AddScore2 25000
    End if
	PFmultiCounter = 240 ' reset timer
End Sub

Dim PFmultiCounter
Dim BumperSpecialVO
Sub SecondTimer_Timer
	BallsaveExtended = BallsaveExtended + 1
	If stopwizcounter > 0 Then stopwizcounter = stopwizcounter - 1

	lightning.imageA = "Lightning" & RndInt(1,4)


	If WizardMode(CurrentPlayer) = 2 Then BlinkLights nightmarestart ,nightmarestart +23,1,2,2,0

	If XmasHurryupTimer > 0 Then
		XmasHurryupTimer = XmasHurryupTimer - 1
		If XmasHurryupTimer < 1 Then
			XmasHurryupCounter = 0
			XmasHurryupTimer = 0
			Blink(47,1) = 0 : Blink(47,1) = 0
		Else
			blinklights 47,47,1,1,7,5
		End If
	End If

	If DogChaseTimer > 0 Then
		DogChaseTimer = DogChaseTimer - 1
		If DogChaseTimer < 1 Then
			DogChaseCounter = 0
			DogChaseTimer = 0
			Blink(46,1) = 0 : Blink(46,1) = 0
		Else
			blinklights 46,46,1,1,7,5
		End If
	End If


	If SuperLoopTimer > 0 Then
		SuperLoopTimer = SuperLoopTimer - 1
		If SuperLoopTimer < 1 Then
			SuperLoopCounter = 0
			SuperLoopTimer = 0
			Blink(44,1) = 0 : Blink(45,1) = 0
		Else
			blinklights 44,45,1,1,7,5
		End If
	End If

	If BumperSpecial > 8 Then
		FlashForms libumper, 300, 75, 0
		Flashforms Flasher002, 300, 50, 0
		PlaySoundat "fx_GiOn",drain
		BumperSpecialVO = BumperSpecialVO + 1
		If BumperSpecialVO = 12 Then
			BumperSpecialVO = 0
		End If
	End If

	If MegaSpinnersTime > 0 Then
		MegaSpinnersTime = MegaSpinnersTime - 1 
		If MegaSpinnersTime < 1 Then
			MegaSpinnersTime = 0
			MegaSpinnersActive = False
			ResetTargetLights
			ArrowLights(1,2) = 0
			ArrowLights(6,2) = 0
			ArrowLights(9,2) = 0
		End If
	End If

    Dim NewPFLevel
	If PFmultiCounter > 0 Then
		PFmultiCounter = PFmultiCounter - 1
		If PFmultiCounter = 0 Then
			DecreasePlayfieldMultiplier
			PFmultiCounter = 240
			Exit Sub
		End If

		NewPFLevel = PlayfieldMultiplier(CurrentPlayer) - 1

		If PFmultiCounter < 10 Then
			Select Case NewPFLevel
				case 5 : blink(5,1) = 2
				case 4 : blink(6,1) = 2
				case 3 : blink(7,1) = 2
				case 2 : blink(8,1) = 2
			End Select		
		Elseif PFmultiCounter = 20 or PFmultiCounter = 35 Then
			Select Case NewPFLevel
				Case 5 : BlinkLights 5,5,5,10,10,0
				Case 4 : BlinkLights 6,6,5,10,10,0
				Case 3 : BlinkLights 7,7,5,10,10,0
				Case 2 : BlinkLights 8,8,5,10,10,0
			End Select
		End If
	End If
    
End Sub

Sub DecreasePlayfieldMultiplier 'reduces by 1 the playfield multiplier
    Dim NewPFLevel
    ' if not at 1 already
    if(PlayfieldMultiplier(CurrentPlayer) > 1) then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) - 1
        SetPlayfieldMultiplier(NewPFLevel)

		Select Case NewPFLevel
			Case 2 : DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,50 , "oneimage" ,31,0,0
			Case 3 : DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,50 , "oneimage" ,32,0,0
			Case 4 : DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,50 , "oneimage" ,33,0,0
		End Select


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
	BlinkLights 9,12,2,5,5,0
    Select Case Level
		Case 1 : Blink(5,1) = 0 : Blink(6,1) = 0 : Blink(7,1) = 0 : Blink(8,1) = 0 
		Case 5 : Blink(5,1) = 1 : Blink(6,1) = 0 : Blink(7,1) = 0 : Blink(8,1) = 0 : BlinkLights 5,5,7,5,5,0
		Case 4 : Blink(5,1) = 0 : Blink(6,1) = 1 : Blink(7,1) = 0 : Blink(8,1) = 0 : BlinkLights 6,6,7,5,5,0
		Case 3 : Blink(5,1) = 0 : Blink(6,1) = 0 : Blink(7,1) = 1 : Blink(8,1) = 0 : BlinkLights 7,7,7,5,5,0
		Case 2 : Blink(5,1) = 0 : Blink(6,1) = 0 : Blink(7,1) = 0 : Blink(8,1) = 1 : BlinkLights 8,8,7,5,5,0
    End Select
End Sub


'***********************************
'**** Bonus multipliers
'***********************************

Sub Spinner002_Spin 'center
    DOF 266, DOFPulse
    PlaySoundAt "fx_spinner", Spinner002
    If Tilted Then Exit Sub

    DOF 201, DOFPulse
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "Spinner002"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "Spinner002" : Exit Sub

	If TranslateLetters(ModeRunning(CurrentPlayer)) = 16 Then
		ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
		ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2	
		If ModeProgress = 10 Then DMD_StartSplash "+10 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		If ModeProgress = 20 Then ModeCollectionDone
	End If
	If TranslateLetters(ModeRunning(CurrentPlayer)) = 13 Then
		ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
		ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2	
		If ModeProgress = 10 Then DMD_StartSplash "+40 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		If ModeProgress = 20 Then DMD_StartSplash "+30 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		If ModeProgress = 30 Then DMD_StartSplash "+20 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
'		If ModeProgress = 40 Then DMD_StartSplash "+10 SPINS","COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		If ModeProgress = 50 Then ModeCollectionDone
	End If
	If TranslateLetters(ModeRunning(CurrentPlayer)) = 17 Then
		ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
		ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2	
		dim tmp
		tmp = 80-ModeProgress
		If tmp < 0 Then tmp = 0
		updatecountdown tmp
		If ModeProgress < 80 Then DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,55 , "80daysupdate" ,999,0,0
		If ModeProgress = 20 Then makingxmascalls
		If ModeProgress = 30 Then makingxmascalls
		If ModeProgress = 40 Then makingxmascalls
		If ModeProgress = 50 Then makingxmascalls
		If ModeProgress = 60 Then makingxmascalls
		If ModeProgress = 70 Then makingxmascalls
		If ModeProgress = 80 Then ModeCollectionDone : DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,120 , "80dayscomplete" ,111,0,0
	End If

	SpinnerSpins = SpinnerSpins + 1 ' eob

	If SpinnerTime > DMD_frame Then
		SpinnerCount = SpinnerCount + 1
		If SpinnerCount > 20 Then SpinnerCount = 20
'		If SpinnerCount > 16 Then Debug.Print "spinnercount " & SpinnerCount
		SpinnerTime = DMD_Frame + 50 ' 50 = 1sec
	Else
		SpinnerTime = DMD_Frame + 50 ' 50 = 1sec
		SpinnerCount = 1
	End If

	If MegaSpinnersActive Then
		Addscore score_megaspinns
		megaspinnerjackpot
		If SpinnerCount = 18 Then
			AddScore score_megaspinns * 2
		End If
	Else
		Addscore score_spinnerspin + ( score_multispin * SpinnerCount )
	End If

    CenterSpinnerHits(CurrentPlayer) = CenterSpinnerHits(CurrentPlayer) + 1
    If Blink(52,1) = 0 And CenterSpinnerHits(CurrentPlayer) >= score_spinsneededforeob + BonusMultiplier(CurrentPlayer) * 5 Then
		blink(55,1) = 2
    End If

    

End Sub

Sub AddBonusMultiplier
    ' if not at the maximum bonus level
	If BonusMultiplier(CurrentPlayer) = 1 Then 
		BonusMultiplier(CurrentPlayer) = 2
	Else 
		BonusMultiplier(CurrentPlayer) = BonusMultiplier(CurrentPlayer) + 2
		If BonusMultiplier(CurrentPlayer) > MaxBonusMultiplier Then
			BonusMultiplier(CurrentPlayer) = MaxBonusMultiplier
			AddScore2 score_maxbonus
		End if
	End If
	BlinkLights 9,12,10,6,10,0
	DMD_StartSplash "BONUS  x" & BonusMultiplier(CurrentPlayer)," ",FontHugeRedhalf, FontHugeRedhalf,120 , "blinkbonus" ,19,0,0
    UpdateBonusXLights(BonusMultiplier(CurrentPlayer))
	bonuscalls
End Sub



Sub UpdateBonusXLights(Level) '4 lights in this table, from 2x to 5x
    ' Update the lights
    Select Case Level
		Case  2 : Blink(9,1) = 1 :  Blink(10,1) = 0 :  Blink(11,1) = 0 :  Blink(12,1) = 0
		Case  4 : Blink(9,1) = 0 :  Blink(10,1) = 1 :  Blink(11,1) = 0 :  Blink(12,1) = 0
		Case  6 : Blink(9,1) = 1 :  Blink(10,1) = 1 :  Blink(11,1) = 0 :  Blink(12,1) = 0
		Case  8 : Blink(9,1) = 0 :  Blink(10,1) = 0 :  Blink(11,1) = 1 :  Blink(12,1) = 0
		Case 10 : Blink(9,1) = 1 :  Blink(10,1) = 0 :  Blink(11,1) = 1 :  Blink(12,1) = 0
		Case 12 : Blink(9,1) = 0 :  Blink(10,1) = 1 :  Blink(11,1) = 1 :  Blink(12,1) = 0
		Case 14 : Blink(9,1) = 1 :  Blink(10,1) = 1 :  Blink(11,1) = 1 :  Blink(12,1) = 0
		Case 16 : Blink(9,1) = 0 :  Blink(10,1) = 0 :  Blink(11,1) = 0 :  Blink(12,1) = 1
		Case 18 : Blink(9,1) = 1 :  Blink(10,1) = 0 :  Blink(11,1) = 0 :  Blink(12,1) = 1
		Case 20 : Blink(9,1) = 0 :  Blink(10,1) = 1 :  Blink(11,1) = 0 :  Blink(12,1) = 1
		Case 22 : Blink(9,1) = 1 :  Blink(10,1) = 1 :  Blink(11,1) = 0 :  Blink(12,1) = 1
		Case 24 : Blink(9,1) = 0 :  Blink(10,1) = 0 :  Blink(11,1) = 1 :  Blink(12,1) = 1
		Case 26 : Blink(9,1) = 1 :  Blink(10,1) = 0 :  Blink(11,1) = 1 :  Blink(12,1) = 1
		Case 28 : Blink(9,1) = 0 :  Blink(10,1) = 1 :  Blink(11,1) = 1 :  Blink(12,1) = 1
		Case 30 : Blink(9,1) = 1 :  Blink(10,1) = 1 :  Blink(11,1) = 1 :  Blink(12,1) = 1
	End Select
End Sub



'***********************************
'**** RGB arrow inserts
'***********************************
'UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity, OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive, float elasticity, float elasticityFalloff, float friction, float scatterAngle)
'	UpdateMaterial "plasticRamps",0.1,0.8,0.7,1,1,0,0.999999,RampGiColor,rgb(250,200,180),rgb(250,200,180),False,True,0,0,0,0
'	UpdateMaterial "InsertPurpleOn" & nr,0,0,1,1,1,0,0.999,rgb(col1,col2,col3),rgb(0,0,0),rgb(0,0,0),False,True,0,0,0,0
Sub inserts_init
'	For Each x In InsertsRGB : x.colorfull = RGB(255,240,225) : x.color = RGB(255,200,150) : Next
	For x = 1 to 20 
		InsertColors(x,0)  = 232
		InsertColors(x,1)  = 222
		InsertColors(x,2)  = 211
		'from top to bottom color 
		InsertColors(x,3)  = 55
		InsertColors(x,4)  = 222' this one change colors
		InsertColors(x,5)  = 55
		' 2nd color
		InsertColors(x,6)  = 232
		InsertColors(x,7)  = 222' white
		InsertColors(x,8)  = 211
		'3rd color
		InsertColors(x,9)  = 11
		InsertColors(x,10) = 23	' blue
		InsertColors(x,11) = 255

		InsertColors(x,12) = 180 ' for nightmare = reds
		InsertColors(x,13) = 44
		InsertColors(x,14) = 11

	Next
	For x = 11 to 16
		InsertColors(x,3)  = 180	' red targetlights
		InsertColors(x,4)  = 22
		InsertColors(x,5)  = 11
	Next
		InsertColors(17,3)  = 11 ' green bumperlight
		InsertColors(17,4)  = 233
		InsertColors(17,5)  = 33
End Sub


Dim InsertColors(20,14)
Sub Update_RGB_inserts
	Dim tmp,y,col(5) , tmp2	
	For x = 1 to 17
		tmp = 1																				' how many is on 
		If ArrowLights(x,1) = 2 Then col(tmp) = 3 : tmp = tmp + 1 
		If ArrowLights(x,2) = 2 Then col(tmp) = 6 : tmp = tmp + 1
		If ArrowLights(x,3) = 2 Then col(tmp) = 9  : tmp = tmp + 1
		If ArrowLights(x,4) = 2 Then col(tmp) = 12 : tmp = tmp + 1
		tmp2 = 22
		If x = 10 Then tmp2 = 41
		If x > 10 Then tmp2 = 21
		If tmp > 1 Then 
			If Blink(x + tmp2,1) = 0 Then blink((x+tmp2),1) = 2
		Else
			blink((x+tmp2),1) = 0
		End If
		Select Case tmp 
			case 2																			'one color
				col(5) = col(1)	
			case 3																			'two colors
				If DMD_Frame mod 120 < 60 Then col(5) = col(1) Else col(5) = col(2)
			case 4 :																		'three colors
				col(5) = col(1)
				If DMD_Frame mod 120 < 80 Then col(5) = col(2)
				If DMD_Frame mod 120 < 40 Then col(5) = col(3)
			Case 5																			'4 colors
				col(5) = col(1)
				If DMD_Frame mod 120 < 90 Then col(5) = col(2)
				If DMD_Frame mod 120 < 60 Then col(5) = col(3)
				If DMD_Frame mod 120 < 30 Then col(5) = col(4)
		End Select
		For y = 0 to 2
			tmp = InsertColors( x , y + col(5))
			If InsertColors(x,y) < tmp Then InsertColors(x,y) = InsertColors(x,y) + 33 : If InsertColors(x,y) > tmp Then InsertColors(x,y) = tmp
			If InsertColors(x,y) > tmp Then InsertColors(x,y) = InsertColors(x,y) - 33 : If InsertColors(x,y) < tmp Then InsertColors(x,y) = tmp
		Next
		UpdateMaterial "InsertPurpleOn" & x,0,0,1,1,1,0,0.999,rgb(InsertColors(x,0),InsertColors(x,1),InsertColors(x,2)),rgb(0,0,0),rgb(0,0,0),False,True,0,0,0,0
		InsertsRGB(x-1).colorfull = RGB(InsertColors(x,0),InsertColors(x,1),InsertColors(x,2))
		InsertsRGB(x-1).color = RGB(InsertColors(x,0),InsertColors(x,1),InsertColors(x,2))
	Next
End Sub




Dim ArrowLights(20,4)
' ChangeArrowColor 1,2,"red",60,"1010101111"
'			lightnr,state,col1,col2,interval,pattern 
Sub ChangeArrowColor( x,state,col1,interval,pattern ) ' arrows 1->9   for jackmb
	Select Case col1
		case "red"		: InsertColors(x,3) = 180 : InsertColors(x,4) = 044 : InsertColors(x,5) = 022
		case "blue"		: InsertColors(x,3) = 013 : InsertColors(x,4) = 011 : InsertColors(x,5) = 255		
		case "green"	: InsertColors(x,3) = 011 : InsertColors(x,4) = 255 : InsertColors(x,5) = 011
		case "yellow"	: InsertColors(x,3) = 200 : InsertColors(x,4) = 255 : InsertColors(x,5) = 011
		case "orange"	: InsertColors(x,3) = 233 : InsertColors(x,4) =  66 : InsertColors(x,5) =  11
		case "teal"		: InsertColors(x,3) = 011 : InsertColors(x,4) = 200 : InsertColors(x,5) = 255
		case "purple"	: InsertColors(x,3) = 245 : InsertColors(x,4) = 011 : InsertColors(x,5) = 245
		case "white"	: InsertColors(x,3) = 232 : InsertColors(x,4) = 222 : InsertColors(x,5) = 211
		Case Else		: debug.print "wrong color in ChangeArrowColor col1"
	End Select

	InsertsRGB(x-1).blinkinterval = interval
	InsertsRGB(x-1).blinkpattern = pattern

'color = auto on x,2 and x,.3 ( white and blue
	If state > -1 Then ArrowLights(x,1) = state
End Sub
Sub ChangeArrowColor2( x,state,col1,interval,pattern ) ' arrows 1->9 for nightmare !!
	Select Case col1
		case "red"		: InsertColors(x,3) = 180 : InsertColors(x,4) = 044 : InsertColors(x,5) = 022
		case "blue"		: InsertColors(x,3) = 013 : InsertColors(x,4) = 011 : InsertColors(x,5) = 255		
		case "green"	: InsertColors(x,3) = 011 : InsertColors(x,4) = 255 : InsertColors(x,5) = 011
		case "yellow"	: InsertColors(x,3) = 200 : InsertColors(x,4) = 255 : InsertColors(x,5) = 011
		case "orange"	: InsertColors(x,3) = 233 : InsertColors(x,4) =  66 : InsertColors(x,5) =  11
		case "teal"		: InsertColors(x,3) = 011 : InsertColors(x,4) = 200 : InsertColors(x,5) = 255
		case "purple"	: InsertColors(x,3) = 245 : InsertColors(x,4) = 011 : InsertColors(x,5) = 245
		case "white"	: InsertColors(x,3) = 232 : InsertColors(x,4) = 222 : InsertColors(x,5) = 211
		Case Else		: debug.print "wrong color in ChangeArrowColor col1"
	End Select

	InsertsRGB(x-1).blinkinterval = interval
	InsertsRGB(x-1).blinkpattern = pattern

'color = auto on x,2 and x,.3 ( white and blue
	If state > -1 Then ArrowLights(x,4) = state
End Sub






'***********************************
'**** gravestones and t's
'***********************************
Dim MegaSpinnersActive : MegaSpinnersActive = False
Dim MegaSpinnersTime : MegaSpinnersTime = 0
Dim MegaSpinnerCount : MegaSpinnerCount = 0



Sub CheckTargets
	If MegaSpinnersActive = True Then
		AddScore score_megaspinner_targets
		BlinkLights 32,37,7,3,3,1
		Exit Sub
	End If
    Dim tmp, i
    tmp = 0
    For i = 1 to 6
        tmp = tmp + TargetHits(CurrentPlayer, i)
    Next
    If tmp = 0 then 'all targets are hit so turn on the target Jackpot
		blinklights 53,54,4,7,12,12
		FlushSplashQ
		DMD_StartSplash " ","  ",FontHugeOrange, FontHugeRedhalf,77 , "wakethedead" ,555,0,0
		MegaSpinner_started(CurrentPlayer) = MegaSpinner_started(CurrentPlayer) + 1
		Select Case MegaSpinner_started(CurrentPlayer)
			Case 5
				DMD_StartSplash " ","  ",FontHugeOrange, FontHugeRedhalf,130 , "targets2" ,555,0,0
				Extraball_two(CurrentPlayer) = 1 
				UpdateExtraballLight
				NightmareShowCounter = 1
				MegaSpinnersTime = 0
				MegaSpinnersActive = False
				ResetTargetLights

			Case Else

				LightSeqBLueTargets.stopplay : LightSeqBLueTargets.Play SeqBlinking, , 15, 25
				MegaSpinnersActive = True
				MegaSpinnerCount = 0
				ArrowLights(1,2) = 2
				ArrowLights(6,2) = 2
				ArrowLights(9,2) = 2
				MegaSpinnersTime = 35
				AddScore score_Gravestones ' initaly20k
				DMD_StartSplash "MEGA","SPINNERS",FontHugeOrange, FontHugeRedhalf,170 , "megaspinner" ,555,0,0
				If MegaSpinner_started(CurrentPlayer) < 5 Then DMD_StartSplash " ","  ",FontHugeOrange, FontHugeRedhalf,188 , "targets1" ,555,0,0

		End Select
	End If

	UpdateTargetLights
End Sub


Sub MegaspinnerScoop
		kickdelay = 3500	
		DMD_StartSplash "MEGASPINNER","JACKPOT ",FontHugeOrange, FontHugeRedhalf,70 , "blinktext" ,19,0,0
		DMD_StartSplash FormatScore( score_SpinnerJP + ( score_megaspinlevel * MegaspinnerLevel(CurrentPlayer) )),"POINTS",FontHugeOrange, FontHugeRedhalf,88 , "blinktext" ,19,0,0
		PlayVoice "_SpinnerJackpot"  
		AddScore2 score_SpinnerJP + ( score_megaspinlevel * MegaspinnerLevel(CurrentPlayer) )
		MegaspinnerLevel(CurrentPlayer) = MegaspinnerLevel(CurrentPlayer) + 1
		If MegaspinnerLevel(CurrentPlayer) > 6 Then MegaspinnerLevel(CurrentPlayer) = 6
		ChangeArrowColor 10,0,"red",60,"10101011111111111" 
		BlinkLights 51,51,12,4,4,0
		MegaSpinnerCount = 10
End Sub


Sub megaspinnerjackpot
	MegaSpinnerCount = MegaSpinnerCount + 1
	If MegaSpinnerCount = 50 Then
		DMD_StartSplash "SCOOP FOR","SPINNER JACKPOT",FontHugeOrange, FontHugeRedhalf,130 , "blinktext" ,19,0,0
		ChangeArrowColor 10,2,"red",60,"10101011111111111"
	End If
End Sub

Sub UpdateTargetLights 'CurrentPlayer

ArrowLights(11,1) = TargetHits(CurrentPlayer, 1)
ArrowLights(12,1) = TargetHits(CurrentPlayer, 2)
ArrowLights(13,1) = TargetHits(CurrentPlayer, 3)
ArrowLights(14,1) = TargetHits(CurrentPlayer, 4)
ArrowLights(15,1) = TargetHits(CurrentPlayer, 5)
ArrowLights(16,1) = TargetHits(CurrentPlayer, 6)

'		Blink(32 , 1) = TargetHits(CurrentPlayer, 1)  'Target001
'		Blink(33 , 1) = TargetHits(CurrentPlayer, 2)  'Target004
'		Blink(34 , 1) = TargetHits(CurrentPlayer, 3)  'Target005
'		Blink(35 , 1) = TargetHits(CurrentPlayer, 4)  'Target006
'		Blink(36 , 1) = TargetHits(CurrentPlayer, 5)  'Target003
'		Blink(37 , 1) = TargetHits(CurrentPlayer, 6)  'Target002
'	End If
End Sub


Sub Target001_Hit 
    DOF 270, DOFPulse
	target001_timer
    PLaySoundAtBall SoundFXDOF("fx_Target",205,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub


	Add_Revive
	Addscore score_targethits
    Flashforms f5, 800, 50, 0
    Flashforms Flasher001, 800, 50, 0
    PlayTomb

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "target001"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "target001" : Exit Sub

	If ArrowLights(11,3) = 2 Then ModeHit "target001"


	TargetHits(CurrentPlayer, 1) = 0
	CheckTargets
End Sub



Sub Target002_Hit 'right target 1
    DOF 271, DOFPulse
	target002_timer
    PLaySoundAtBall SoundFXDOF("fx_Target",205,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore score_targethits
    PlayTomb
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "target002"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "target002" : Exit Sub


	If ArrowLights(16,3) = 2 Then ModeHit "target002"

	TargetHits(CurrentPlayer, 6) = 0
	CheckTargets
End Sub


Sub Target003_Hit 'right target 2
    DOF 272, DOFPulse
	target003_timer
	If Tilted Then Exit Sub

    PLaySoundAtBall SoundFXDOF("fx_Target",205,DOFPulse,DOFTargets)
    Addscore score_targethits
    PlayTomb
	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "target003"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "target003" : Exit Sub

	If ArrowLights(15,3) = 2 Then ModeHit "target003"

	TargetHits(CurrentPlayer, 5) = 0
	CheckTargets
End Sub


Sub Target004_Hit 'cabin target 1
    DOF 273, DOFPulse
	target004_timer
    If Tilted Then Exit Sub
	clight cLightGarland,600,2
	blinklights 54,54,1,7,7,0
    PLaySoundAtBall SoundFXDOF("fx_Target",109,DOFPulse,DOFTargets)

    Addscore score_targethits
    PlayElectro

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "target004"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "target004" : Exit Sub

	If ArrowLights(12,3) = 2 Then ModeHit "target004"

	TargetHits(CurrentPlayer, 2) = 0
	CheckTargets
End Sub


Sub Target005_Hit 'cabin target 2
    DOF 274, DOFPulse
	target005_timer
    PLaySoundAtBall SoundFXDOF("fx_Target",109,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
	blinklights 53,53,1,7,7,0
	clight cLightGarland,600,2
    Addscore score_targethits
    PlayElectro

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "target005"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "target005" : Exit Sub

	If ArrowLights(13,3) = 2 Then ModeHit "target005"

    TargetHits(CurrentPlayer, 3) = 0
    CheckTargets
End Sub

Sub Target006_Hit 'loop target
    DOF 275, DOFPulse
	target006_timer
    PLaySoundAtBall SoundFXDOF("fx_Target",109,DOFPulse,DOFTargets)
    If Tilted Then Exit Sub
    Addscore score_targethits
    PlayTomb

	If WizardMode(CurrentPlayer) = 1 Then Wizardhit "target006"
	If WizardMode(CurrentPlayer) = 2 Then Wizardhit "target006" : Exit Sub

	If ArrowLights(14,3) = 2 Then ModeHit "target006"
	TargetHits(CurrentPlayer, 4) = 0
	CheckTargets
End Sub

Dim Target1blink
Sub target001_timer
	If Target001.timerenabled = False Then
		Target001.timerenabled = True
		Target1blink = 0
	Else
		Target1blink = Target1blink + 1
		Select Case Target1blink
			Case 1 : Primitive278.blenddisablelighting = 1
			Case 2 : Primitive278.blenddisablelighting = 2
			Case 3 : Primitive278.blenddisablelighting = 1.5
			Case 4 : Primitive278.blenddisablelighting = 1
			Case 5 : Primitive278.blenddisablelighting = 0.7
			Case 6 : Primitive278.blenddisablelighting = 0.4
			Case 7 : Primitive278.blenddisablelighting = 0.2
			Case 8 : Primitive278.blenddisablelighting = 0
					 Target001.timerenabled = False 
		End Select
		Primitive278.objroty = - Primitive278.blenddisablelighting
	End If
End Sub
Dim Target2blink
Sub target002_timer
	If Target002.timerenabled = False Then
		Target002.timerenabled = True
		Target2blink = 0
	Else
		Target2blink = Target2blink + 1
		Select Case Target2blink
			Case 1 : Primitive136.blenddisablelighting = 1
			Case 2 : Primitive136.blenddisablelighting = 2
			Case 3 : Primitive136.blenddisablelighting = 1.5
			Case 4 : Primitive136.blenddisablelighting = 1 
			Case 5 : Primitive136.blenddisablelighting = 0.7 
			Case 6 : Primitive136.blenddisablelighting = 0.4 
			Case 7 : Primitive136.blenddisablelighting = 0.2 
			Case 8 : Primitive136.blenddisablelighting = 0 
				Target002.timerenabled = False 
		End Select
		Primitive136.objroty = Primitive136.blenddisablelighting
		Primitive136.objrotx = Primitive136.blenddisablelighting
	End If
End Sub
Dim Target3blink
Sub target003_timer
	If Target003.timerenabled = False Then
		Target003.timerenabled = True
		Target3blink = 0
	Else
		Target3blink = Target3blink + 1
		Select Case Target3blink
			Case 1 : Primitive137.blenddisablelighting = 1
			Case 2 : Primitive137.blenddisablelighting = 2
			Case 3 : Primitive137.blenddisablelighting = 1.5 
			Case 4 : Primitive137.blenddisablelighting = 1 
			Case 5 : Primitive137.blenddisablelighting = 0.7 
			Case 6 : Primitive137.blenddisablelighting = 0.4
			Case 7 : Primitive137.blenddisablelighting = 0.2 
			Case 8 : Primitive137.blenddisablelighting = 0
				Target003.timerenabled = False 
		End Select
		Primitive137.objroty = Primitive137.blenddisablelighting
	End If
End Sub
Dim Target4blink
Sub target004_timer
	If Target004.timerenabled = False Then
		Target004.timerenabled = True
		Target4blink = 0
	Else
		Target4blink = Target4blink + 1
		Select Case Target4blink
			Case 1 : Primitive134.blenddisablelighting = 1 
			Case 2 : Primitive134.blenddisablelighting = 2 
			Case 3 : Primitive134.blenddisablelighting = 1 
			Case 4 : Primitive134.blenddisablelighting = 2 
			Case 5 : Primitive134.blenddisablelighting = 1.5 
			Case 6 : Primitive134.blenddisablelighting = 1 
			Case 7 : Primitive134.blenddisablelighting = 0.7
			Case 8 : Primitive134.blenddisablelighting = 0.4
			Case 9 : Primitive134.blenddisablelighting = 0.2 
			Case 10: Primitive134.blenddisablelighting = 0
					Target004.timerenabled = False 
		End Select
		Primitive134.objrotx = Primitive134.blenddisablelighting
	End If
End Sub
Dim Target5blink
Sub target005_timer
	If Target005.timerenabled = False Then
		Target005.timerenabled = True
		Target5blink = 0
	Else
		Target5blink = Target5blink + 1
		Select Case Target5blink
			Case 1 : Primitive135.blenddisablelighting = 1 
			Case 2 : Primitive135.blenddisablelighting = 2 
			Case 3 : Primitive135.blenddisablelighting = 1 
			Case 4 : Primitive135.blenddisablelighting = 2 
			Case 5 : Primitive135.blenddisablelighting = 1.5 
			Case 6 : Primitive135.blenddisablelighting = 1 
			Case 7 : Primitive135.blenddisablelighting = 0.7 
			Case 8 : Primitive135.blenddisablelighting = 0.4 
			Case 9 : Primitive135.blenddisablelighting = 0.2 
			Case 10: Primitive135.blenddisablelighting = 0 
					Target005.timerenabled = False 
		End Select
		Primitive135.objrotx = Primitive135.blenddisablelighting
	End If
End Sub
Dim Target6blink
Sub target006_timer
	If Target006.timerenabled = False Then
		Target006.timerenabled = True
		Target6blink = 0
	Else
		Target6blink = Target6blink + 1
		Select Case Target6blink
			Case 1 : Primitive005.blenddisablelighting = 1 
			Case 2 : Primitive005.blenddisablelighting = 2 
			Case 3 : Primitive005.blenddisablelighting = 1 
			Case 4 : Primitive005.blenddisablelighting = 2 
			Case 5 : Primitive005.blenddisablelighting = 1.5 
			Case 6 : Primitive005.blenddisablelighting = 1 
			Case 7 : Primitive005.blenddisablelighting = 0.7 
			Case 8 : Primitive005.blenddisablelighting = 0.4 
			Case 9 : Primitive005.blenddisablelighting = 0.2 
			Case 10: Primitive005.blenddisablelighting = 0 
					Target006.timerenabled = False 
		End Select
		Primitive005.objrotx = Primitive005.blenddisablelighting
	End If
End Sub

'***********************************
'**** combostuff
'***********************************
Dim Combotimer
Dim ComboCounter			
Dim ComboLasthit
Sub RestartComboTimer	 
	If Not Combotimer > DMD_Frame Then ComboCounter = 0 : ComboLasthit = ""
	Combotimer = DMD_Frame + 300
End Sub  


Sub updatemodecombolights(name)
	If Blink(43,1) = 2 Then Exit Sub
	If TranslateLetters(ModeRunning(CurrentPlayer)) = 18 or TranslateLetters(ModeRunning(CurrentPlayer)) = 19 or TranslateLetters(ModeRunning(CurrentPlayer)) = 20 Then
		For x = 1 to 9 : if x <> 3 then ArrowLights(x,3) = 2 : End If : Next
		If name = "leftspinner"		Then ArrowLights(1,3) =  0
		If name = "leftramp"		Then ArrowLights(2,3) =  0
		If name = "captiveball"		Then ArrowLights(3,3) =  0 
		If name = "leftinnerloop"	Then ArrowLights(4,3) =  0 
		If name = "houseup"			Then ArrowLights(5,3) =  0 
		If name = "middlespinner"	Then ArrowLights(6,3) =  0 
		If name = "rightramp"		Then ArrowLights(7,3) =  0 
		If name = "outerloop"		Then ArrowLights(8,3) =  0 
		If name = "rightspinner"	Then ArrowLights(9,3) =  0 
	End If
End Sub


Sub AwardComboShot(name)  ' will award extended combo time

	If ComboLasthit = name Then Exit Sub

	If TranslateLetters(ModeRunning(CurrentPlayer)) = 18 Then
		ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
		AddScore2 score_ModeProgress
		If ModeProgress < 2 Then DMD_StartSplash ((2-ModeProgress) & " COMBOS"),"COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0 
		If ModeProgress = 2 Then ModeCollectionDone
	End If
	If TranslateLetters(ModeRunning(CurrentPlayer)) = 19 Then
		AddScore2 score_ModeProgress
		ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
		If ModeProgress < 4 Then DMD_StartSplash ((4-ModeProgress) & " COMBOS"),"COLLECT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0 	
		If ModeProgress = 4 Then ModeCollectionDone
	End If
	If TranslateLetters(ModeRunning(CurrentPlayer)) = 20 Then
		AddScore2 score_ModeProgress
		ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
		If ModeProgress < 8 Then DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,88 , "nimbleprogress" ,999,0,0 : nimblecalls
		If ModeProgress = 8 Then ModeCollectionDone : DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,88 , "nimblefinish" ,999,0,0 : 
	End If

	ComboCount(CurrentPlayer) = ComboCount(CurrentPlayer) + 1 ' for EOB
	ComboCounter = ComboCounter + 1



	Combotimer = DMD_Frame + 250 '5s
	If name = "Trigger15" or name = "Trigger016" Then Combotimer = DMD_Frame + 450 '8s
	ComboLasthit = name



	If Not bJackMBStarted And WizardMode(CurrentPlayer) = 0 Then
		Select Case ComboCounter
			Case 1 : PlayVoice "vo_combo"
		addscore2 score_Combo_1
			Case 2 : PlayVoice "vo_2xcombo" : stopsound "vo_combo"
		addscore2 score_Combo_2
			Case 3 : PlayVoice "vo_3xcombo": stopsound "vo_2xcombo"
		addscore2 score_Combo_3
			Case 4 : PlayVoice "vo_4xcombo": stopsound "vo_3xcombo"
		addscore2 score_Combo_4
			Case 5 : PlayVoice "vo_5xcombo": stopsound "vo_4xcombo"
		addscore2 score_Combo_5
			Case Else : stopsound "": PlayVoice "": stopsound "vo_5xcombo"
		addscore2 score_Combo_6
		End Select
	End If

	'Trigger009   outerloop superloop
	'Trigger012   loopdown  superloop
	'trigger007   innerloop going up
	'Trigger011   house going Uå
	'Trigger016   lerftramp
	'Trigger015   RightRampAccelerate
	'Trigger014   middlespinner
End Sub

Dim SuperLoopCounter
Dim SuperLoopTimer
Dim SuperLoopJPs

Sub Superloop1
	SuperLoopCounter = SuperLoopCounter + 1
	AddScore score_TopLoop
	If Blink(44,1) = 2 Then AwardSuperLoopJP
	If SuperLoopCounter = 5 Then Start_Superloops
End Sub

Sub Superloop2
	SuperLoopCounter = SuperLoopCounter + 1
	If Blink(44,1) = 2 Then AwardSuperLoopJP
	If SuperLoopCounter = 5 Then Start_Superloops
End Sub

Sub AwardSuperLoopJP
	PlaySound "vo_moonatnight",1,BackBoxVolume * 0.8
	SuperLoopJPs = SuperLoopJPs + 1
	If SuperLoopJPs > 16 Then SuperLoopJPs = 16 ' limit 2mill
	addscore2 score_SuperLoopJP + score_SuperLoopJPadd * SuperLoopJPs
	DMD_StartSplash formatscore (  score_SuperLoopJP + score_SuperLoopJPadd * SuperLoopJPs ),"LOOP JACKPOT",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
End Sub

Sub Start_Superloops
		Blink(44,1) = 2 : Blink(45,1) = 2
		BlinkLights 44,45,10,5,5,3
		SuperLoopTimer = 60
		DMD_StartSplash "JACKPOTS 60s","GET SUPERLOOP",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
End Sub



'***********************************
'**** hurryup dog chase ...leftramps only
'***********************************

Dim DogChaseCounter
Dim DogChaseTimer
Dim ChaseCollected

Sub HurryupDogChase
	DogChaseCounter = DogChaseCounter + 1

	If Blink(46,1) = 2 Then 
		dim tmp
		tmp = score_DogChase + score_DogChaseAdd * ChaseCollected
		addscore2 tmp
		PlayVoice "sfx_zero"
		DMD_StartSplash FormatScore(tmp	), "FETCH HURRYUP", FontHugeOrange, FontHugeRedhalf,150 , "blinktextSlow" ,19,0,0
		ChaseCollected = ChaseCollected + 1

	End If
'HurryupNeededDog(i) = 3 ' +1 FOR EACH START - MAX IS 8
'HurryupNeededDXmas(i) = 3
'JackResetNeeded(i) = 3

	If DogChaseCounter = HurryupNeededDog(CurrentPlayer) Then
		DogChaseCounter = DogChaseCounter + 10
		HurryupNeededDog(CurrentPlayer) = HurryupNeededDog(CurrentPlayer) + 1
		If HurryupNeededDog(CurrentPlayer) > 8 Then HurryupNeededDog(CurrentPlayer) = 8
		DogChaseTimer = 60
		ChaseCollected = 0
		Blink(46,1) = 2
		DMD_StartSplash "LEFT RAMP ", "FETCH 60S", FontHugeOrange, FontHugeRedhalf,150 , "blinktextSlow" ,19,0,0
		PlayVoice "vo_cuttingitclose"
	End If
End Sub

Dim XmasHurryupCounter
Dim XmasHurryupTimer
Dim XmasCollected
Sub HurryupXmas
	XmasHurryupCounter = XmasHurryupCounter + 1

	If Blink(47,1) = 2 Then
		dim tmp
		tmp = score_XmasHurryup + score_XmasHurryupAdd * XmasCollected
		addscore2 tmp
		DMD_StartSplash FormatScore(tmp	), "XMAS HURRYUP", FontHugeOrange, FontHugeRedhalf,150 , "blinktextSlow" ,19,0,0
		XmasCollected = XmasCollected + 1
		Playsound "sfx_sleighbell3",1,BackboxVolume
		Playsfx "sfx_hoho"
	End If

	If XmasHurryupCounter = HurryupNeededDXmas(CurrentPlayer) Then
		XmasHurryupCounter = XmasHurryupCounter + 10
		HurryupNeededDXmas(CurrentPlayer) = HurryupNeededDXmas(CurrentPlayer) + 1
		If HurryupNeededDXmas(CurrentPlayer) > 8 Then HurryupNeededDXmas(CurrentPlayer) = 8
		XmasHurryupTimer = 60
		XmasCollected = 0
		Blink(47,1) = 2
		DMD_StartSplash "RIGHT RAMP ", "XMAS 60S", FontHugeOrange, FontHugeRedhalf,150 , "blinktextSlow" ,19,0,0
		PlayVoice "vo_hurryup"
	End If
End Sub


'***********************************
'**** collectpresents   addrevive
'***********************************

Dim ModeProgress
'nightmarestart
'ModeRunning(CurrentPlayer)
'NextMode
Dim Switcharooo : Switcharooo = 0
Dim FindJack
Sub StartPresentsMode
	If ModeRunning(CurrentPlayer) = 0 Then ModeRunning(CurrentPlayer) = 18 ' fixing RndInt(1,24) 
	ModeProgress = 0 
	If bSkillshotReady = False Then 
		Select Case TranslateLetters( ModeRunning(CurrentPlayer))
			Case 24 :	DMD_StartSplash "CAPTIVEBALL","HIT 2 TIMES" ,FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 23  :	DMD_StartSplash "COMPLETE 5" ,"RAMPS",FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 22  :	DMD_StartSplash "COLLECT 4" ,"BLUE LIGHTS" ,FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 19 :	DMD_StartSplash "COMBOS","GET 4" ,FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 18 :	DMD_StartSplash "COMBOS","GET 2" ,FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 15 : 	DMD_StartSplash "HIT","BUMPER" ,FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 14 :	DMD_StartSplash "GET 3 RIPS","TOP SPINNER" ,FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 16 :	DMD_StartSplash "COLLECT" ,"20 SPINNS",FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 13 :	DMD_StartSplash "COLLECT" ,"50 SPINNS",FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 12 :	DMD_StartSplash "HIT BUMPER" ,"5 TIMES",FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 11 :	DMD_StartSplash "CAPTIVEBALL","HIT 3 TIMES" ,FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 9  :	DMD_StartSplash "COMPLETE" ,"5 RAMPS",FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 6  :	DMD_StartSplash "HIT SCOOP","2 TIMES" ,FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 1  :	DMD_StartSplash "COMPLETE" ,"3 RAMPS",FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0
			Case 8  :	DMD_StartSplash "COLLECT 3" ,"BLUE LIGHTS",FontHugeOrange, FontHugeRedhalf,155 , "blinktop1" ,55,0,0

		' nightmare ones
			Case 5  :	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "raisethedead" ,999,0,0
			Case 2	:	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,211 , "townmeetingstart" ,999,0,0 
			Case 7  :	DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,200 , "blink2images" ,50,51,0 ' sound skip on previous if next is nightmare
			Case 4	:	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,222 , "bequickstart" ,999,0,0 
			Case 3	:	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,222 , "whatsthisstart" ,999,0,0
			Case 10 :	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,222 , "sandyclaws" ,999,0,0  
						Primitive178.visible = False
						Primitive018.visible = True
			Case 17 :	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,222 , "80daysstart" ,999,0,0
			Case 20 :	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,222 , "nimblestart" ,999,0,0
			Case 21 :	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,222 , "savechristmas" ,999,0,0
		End Select
	End If
	DEBUG.Print "modestart(translatednr) = " & TranslateLetters( ModeRunning(CurrentPlayer))

	Select Case TranslateLetters(ModeRunning(CurrentPlayer))
		Case   24 : ArrowLights(3,3) = 2																																			' 2 cpative		easy
		Case   23 : ArrowLights(2,3) = 2 : ArrowLights(7,3) = 2																														' 5 ramps  			 normal
		Case   22 : For x = 1 to 6 : ArrowLights(x+10,3) = 2 : Next : Switcharooo = 1 																								' hit any of 6 targets next house ... repeat 2 times 	Easy
		Case   21 :	For x = 1 to 6 : ArrowLights(x+10,3) = 2 : Next : Switcharooo = 1 	 																							' hit any of 6 targets next house ... repeat 4 times				Hard
		Case   20 : For x = 1 to 9 : If x <> 3 Then ArrowLights(x,3) = 2 : End If : Next 																							' 8 combos					hard
		Case   19 : For x = 1 to 9 : If x <> 3 Then ArrowLights(x,3) = 2 : End If : Next																							' 4 combos			 normal 
		Case   18 : For x = 1 to 9 : If x <> 3 Then ArrowLights(x,3) = 2 : End If : Next																							' 2 combos		easy 	
		Case   17 : ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2																								' 80 spins			 		hard
					
		Case   16 : ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2																								' 20 spins		easy
		Case   15 : ArrowLights(17,3) = 2																																			' 1 BumperHit	easy
		Case   14 : ArrowLights(6,3) = 2																																			' centerspinx3		 NORMAL	hard
		Case   13 : ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2																								' 50 spins			 normal	
		Case   12 : ArrowLights(17,3) = 2																																			' 5 BumperHits		 normal
		Case   11 : ArrowLights(3,3) = 2																																			' 3 cpative		easy
		Case   10 : ArrowLights(2,3) = 2 : ArrowLights(7,3) = 2																														' 7 ramps  					hard
		Case    9 :	ArrowLights(2,3) = 2 : ArrowLights(7,3) = 2																														' 5 ramps  			 normal
		Case	8 : ArrowLights(5,3) = 2 : ArrowLights(12,3) = 2 : ArrowLights(13,3) = 2																							' house + 2t	easy
		Case    7 : For x = 1 to 9 : ArrowLights(x,3) = 2 : Next : Playsound "vo_findjack",1,BackboxVolume : FindJack = RndInt(1,9) 												' FindJack					hard
		Case    6 : ArrowLights(10,3) = 2																																			' scoop x2		easy
		Case    5 : For x = 11 to 16 : ArrowLights(x,3) = 2 : Next																													' 6 target			 normal HARD
		Case    4 : ArrowLights(2,3) = 2 : ArrowLights(9,3) = 2 : ModeTimer.Interval = 7777 : ModeTimer.Enabled = True		:														' random					hard
		Case    3 :	For x = 2 to 8 : ArrowLights(x,3) = 2 : Next																													' top 7 			 		hard
		Case    2 :	ArrowLights(2,3) = 2																																			' top 7 1by1				hard
		Case    1 :	ArrowLights(2,3) = 2 : ArrowLights(7,3) = 2																														' 3 ramps		easy
	End Select																																										'  CHRISTMA+s BEFORE NIGHTMARE
	changesong

	UpdatePresentsLights
End Sub


Sub gi015_Timer : changegi white : gi015.timerenabled = False : End Sub
Sub gi016_Timer : changegi white : gi016.timerenabled = False : End Sub

'Nightmare	

' 5 xxx raise the dead	(N)		6 gravestonelit Update		raise jack							5	all 6 targets   'oogie green ?=
' 2	town meeting		(i)		fx2  townmeeting   with progres text    +  town meeting complete						 2-	all 7 top lanes 1 by 1
' 7 xxx find jack		(G)		jackX	 					JACK								7	find jack ( all done )
' 3						(H)
' 4	xxx jack be quick   (T)		one gif made	with image overlays								4	random 2 light on timer then they change ... 7 hits 
'10 xxx sandy claws		(M)		bag							dead present						10	7 ramps  swap jack with santajack part of a time
'17	xxx 80 days to hallo(A)		xx days Left-halloween Flash	  --- 80 spins
'20	xxx workingonit 	(R)		Jack be nimble													8 combos    
'21 xxx save christmas	(E)		riding sleigh gif into text overlay 1: hit target 2: enter house     finish = Finished collecting text			21	1 target then house   repeat 4 times




' 3	whats this	  2 red gifs					 3-	all 7 top lanes 
' progress+finish = 1-20 zWhatsthisA16
' zWhatsthisb1-25 + letme out = start "vo_letmeout
' whatathis sound for whats this progress ...  letmeout for start ? vo_whosnextonmylist for finish
'DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,120 , "whatsthisstart" ,999,0,0



Dim TranslateLetters : TranslateLetters = Array (0,5,2,7,3,4,10,17,20,21,    23,19,13,12,9,14    ,1,6,8,11,15,16,18,22,24,0,0 )

Sub ModeHit(n)
	Dim tmp
	blinklights 53,54,4,5,1,0
	If TranslateLetters(ModeRunning(CurrentPlayer)) = 13 Or TranslateLetters(ModeRunning(CurrentPlayer)) = 16 Or TranslateLetters(ModeRunning(CurrentPlayer)) = 17 Or TranslateLetters(ModeRunning(CurrentPlayer)) = 18 Or TranslateLetters(ModeRunning(CurrentPlayer)) = 19 Or TranslateLetters(ModeRunning(CurrentPlayer)) = 20 Then Exit Sub

	ModeProgress = ModeProgress + 1 : changegi blue : gi015.timerenabled = True : GiEffect 6
	TurnOffBlueArrows n

	Select Case TranslateLetters( ModeRunning(CurrentPlayer))
		Case   24 : If ModeProgress < 2 Then DMD_StartSplash "HIT " &  (2-ModeProgress) ,"CAPTIVEBALL",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0 : PlaySound"sfx_anvil",1,BackBoxVolume
		Case   23 :	If ModeProgress < 5 Then DMD_StartSplash ((5-ModeProgress) & " BLUE LIGHTS"),"COLLECT"	,FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0 : Playsfx"vo_jacklaughquick"
		Case   22 : If ModeProgress < 4 Then ' hit any of 6 targets next house ... repeat 2 times for hard 
						If Switcharooo = 1 Then
							For x = 1 to 6 : ArrowLights(x+10,3) = 0 : Next : Switcharooo = 2 : ArrowLights(5,3) = 2
						Else
							For x = 1 to 6 : ArrowLights(x+10,3) = 2 : Next : Switcharooo = 1
						End If
					End If
		Case   21 :	If ModeProgress < 8 Then ' hit any of 6 targets next house ... repeat 4 times for easy
						If Switcharooo = 1 Then
							For x = 1 to 6 : ArrowLights(x+10,3) = 0 : Next : Switcharooo = 2 : ArrowLights(5,3) = 2
						Else
							For x = 1 to 6 : ArrowLights(x+10,3) = 2 : Next : Switcharooo = 1
						End If
					End If
					DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,130 , "savechristmas" ,999,0,0


		Case   20 : 		' 8 combos progress on combos
		Case   19 : 		' 4 combos progress on combos	
		Case   18 : 		' 2 combos progress on combos
		Case   17 : 
		Case   16 : 
		Case   15 : 'na
		Case   14 : If ModeProgress < 3 Then DMD_StartSplash ((3-ModeProgress) & " RIPS LEFT"),"TOPSPINNER",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		Case   13 : 'na
		Case   12 : If ModeProgress < 5 Then DMD_StartSplash ((5-ModeProgress) & " BLUE LIGHTS"),"COLLECT"	,FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0 : Playsfx"vo_jacklaughquick"
		Case   11 : If ModeProgress < 3 Then DMD_StartSplash "HIT " & (3-ModeProgress) ,"CAPTIVEBALL",FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0 : : PlaySound"sfx_anvil",1,BackBoxVolume
		Case   10 : If ModeProgress < 7 Then DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,120 , "sandyclaws" ,999,0,0 : updatecountdown 8 - ModeProgress : SandyCalls
		case    9 : If ModeProgress < 5 Then DMD_StartSplash ((5-ModeProgress) & " BLUE LIGHTS"),"COLLECT"	,FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0 : Playsfx"vo_jacklaughquick"
		case    8 : If ModeProgress < 3 Then DMD_StartSplash ((3-ModeProgress) & " BLUE LIGHTS"),"COLLECT"	,FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0 : Playsfx"vo_jacklaughquick"
		case    7 : ' no
		case    6 : If ModeProgress = 1 Then DMD_StartSplash "SCOOP AGAIN","HIT"						,FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0
		case    5 : If ModeProgress < 6 Then DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,120 , "raisethedead" ,999,0,0 : PlaySound "sfx_ribbet",1,BackBoxVolume
		case    4 : If ModeProgress < 7 Then FlushSplashQ : DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,90 , "bequickprogress" ,999,0,0 : quickcalls
		case    3 : If ModeProgress < 7 Then DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,120 , "whatsthisprogress" ,999,0,0 : PlayVoice "vo_whatsthis"  
		case    2 : If ModeProgress < 7 Then ArrowLights(2+ModeProgress,3) = 2  : DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,130 , "townmeetingprogress" ,999,0,0	: TownCalls
		case    1 : If ModeProgress < 3 Then DMD_StartSplash ((3-ModeProgress) & " BLUE LIGHTS"),"COLLECT"	,FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0 
		case Else : If ModeProgress < 5 Then DMD_StartSplash ((5-ModeProgress) & " BLUE LIGHTS"),"COLLECT" 	,FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0 : Playsfx"vo_jacklaughquick"
	End Select

	AddScore2 score_ModeProgress 

	Select Case TranslateLetters(ModeRunning(CurrentPlayer))
		Case   24 : ArrowLights(3,3) = 2 : If ModeProgress = 2 Then ModeCollectionDone 
		Case   23 : ArrowLights(2,3) = 2 : ArrowLights(7,3) = 2	: If ModeProgress = 5 Then ModeCollectionDone
		Case   22 : If ModeProgress = 4 Then ModeCollectionDone		 ' hit any of 6 targets next house ... repeat 4 times for hard 
		Case   21 :	If ModeProgress = 8 Then FlushSplashQ : DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,130 , "savechristmas2" ,999,0,0 : ModeCollectionDone 		' hit any of 6 targets next house ... repeat 2 times for easy
		Case   20 : 		' 8 combos		easy ( light all shots that start combo ? then light all that give it. repeat	
		Case   19 : 		' 4 combos		easy ( light all shots that start combo ? then light all that give it. repeat	
		Case   18 : 		' 2 combos		easy ( light all shots that start combo ? then light all that give it. repeat	
		Case   17 : ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2 : ' mode progress and stuff controlled on spinners
		Case   16 : ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2 : ' mode progress and stuff controlled on spinners
		Case   15 : ArrowLights(17,3) = 2 : ModeCollectionDone
		Case   14 : ArrowLights(6,3) = 2 : If ModeProgress = 3 Then ModeCollectionDone
		Case   13 : ArrowLights(1,3) = 2 : ArrowLights(6,3) = 2 : ArrowLights(9,3) = 2 : ' mode progress and stuff controlled on spinners
		Case   12 : ArrowLights(17,3) = 2 : If ModeProgress = 5 Then ModeCollectionDone 
		Case   11 : ArrowLights(3,3) = 2 : If ModeProgress = 3 Then ModeCollectionDone 
		Case   10 : ArrowLights(2,3) = 2 : ArrowLights(7,3) = 2	: If ModeProgress = 7 Then updatecountdown 0 : FlushSplashQ : DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,120 , "sandyclaws2" ,999,0,0 : ModeCollectionDone
		Case    9 : ArrowLights(2,3) = 2 : ArrowLights(7,3) = 2	: If ModeProgress = 5 Then ModeCollectionDone
		Case    8 : If ModeProgress > 2 Then ModeCollectionDone 
		Case    7 : If FindJack = 100 Then :  FindJack = 0 : ModeCollectionDone : FlushSplashQ : DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,120 , "blink2images" ,49,54,0  : Else : DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,67 , "blink2images" ,52,53,0 : End If
		Case    6 : If ModeProgress = 2 Then : ModeCollectionDone : Else : ArrowLights(10,3) = 2 : End If
		Case    5 : If ModeProgress = 6 Then FlushSplashQ  : DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,66 , "raisethedead1" ,999,0,0 : DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,400 , "raisethedead2" ,999,0,0  : ModeCollectionDone
		Case    4 : ModeTimer_Timer : If ModeProgress = 7 Then ModeCollectionDone : ModeTimer.Enabled = False : FlushSplashQ : DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,120 , "bequickfinish" ,999,0,0
		Case    3 : If ModeProgress = 7 Then ModeCollectionDone : FlushSplashQ : DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,130 , "whatsthisfinish" ,999,0,0 : 
		Case    2 : If ModeProgress = 7 Then ModeCollectionDone : FlushSplashQ : DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,130 , "townmeetingfinish" ,999,0,0: PlaySound "sfx_churchbell",1,BackboxVolume
		Case    1 : ArrowLights(2,3) = 2 : ArrowLights(7,3) = 2	: If ModeProgress = 3 Then ModeCollectionDone
		Case Else : ArrowLights(2,3) = 2 : ArrowLights(7,3) = 2	: If ModeProgress = 5 Then ModeCollectionDone
	End Select
	UpdatePresentsLights
End Sub
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,77 , "raisethedead" ,999,0,0
'DMD_StartSplash " "," ",FontHugeOrange, FontHugeRedhalf,400 , "raisethedead2" ,999,0,0


Sub ModeCollectionDone

		AddScore2 score_ModeCollected
		BlinkAllPresents 5,1 
		TurnOffAllBlueLights
		Blink(43,1) = 2 : BlinkLights 43,43,12,6,6,0
		ChangeSong
		greenoogie = True
		FlushSplashQ
		DMD_StartSplash "DELIVER","PRESENT" ,FontHugeOrange, FontHugeRedhalf,55 , "blinktop1" ,55,0,0

	Select Case TranslateLetters(ModeRunning(CurrentPlayer))
		Case    5 :  PositiveCallout : PlaySound"sfx_crowdcheer" ,1,BackboxVolume /3.45: changesong   					 			'RaiseTheDead    'modeendings				
		Case    2 :  PlaySound"vo_town13",1,BackboxVolume/2: PlaySound"sfx_crowdcheer" ,1,BackboxVolume /3.45: changesong			'TownMeeting							
		Case    7 :  PlaySound "vo_whereubeen",1,BackboxVolume : PlaySound"sfx_crowdcheer",1,BackboxVolume /3.45 	 				'FindJack
		Case    4 :  PlaySound"sfx_crowdcheer",1,BackboxVolume /3.45 :PlaySound"vo_bequick-004",1,BackboxVolume: changesong			'BeQuick
		Case    3 :  ChangeSong : PlaySound "vo_hohoho",1,BackBoxVolume : PlaySound"sfx_crowdcheer" ,1,BackboxVolume /3.45 			'WhatsThis
		Case    10 : PlaySound"vo_sandyclaws5",1,BackBoxVolume: PositiveCallout: changesong	  							    		'SandyClaws
		Case    17 : 																									    		'80 Days	
		Case    20 : PlaySound"vo_eureka",1,BackBoxVolume: changesong 														 		'Nimble
		Case    21 : Playsound"sfx_crowdcheer" ,1,BackboxVolume/5 :PositiveCallout : ChangeSong   						 			'SaveChristmas

'		so any other nr you can add here to be an exeption to case else that will play on all others

		Case Else	: PlayVoice"_present-003" : Playsound"sfx_award",1,BackboxVolume 
	
' for special endings
	End Select
	UpdatePresentsLights
End Sub


Sub ModeTimer_Timer
	ModeTimer.Enabled = False
	Select Case TranslateLetters(ModeRunning(CurrentPlayer))
		Case    4 : For x = 1 to 17 : ArrowLights(x,3) = 0 : Next : LightRandomModeLights 2,17
	End Select
	ModeTimer.Enabled = True
End Sub


Sub LightRandomModeLights(n,nr)
	dim tmp,y
	For x = 1 to n
		tmp = RndInt(1,nr)
		For y = 1 to nr
			If ArrowLights(tmp,3) = 0 Then ArrowLights(tmp,3) = 2 : Exit For
			tmp = tmp + 1 : If tmp > nr Then tmp = 1
		Next
	Next

End Sub



Sub TurnOffBlueArrows(n)
	If n = "leftspinner"	Then
		ArrowLights(1,3) =  0
		If FindJack = 1 Then
			FindJack = 100
		Elseif FindJack > 0 Then
			JackHereSound
		End If
	End If  

	If n = "leftramp"	Then
		ArrowLights(2,3) =  0
		If FindJack = 2 Then
			FindJack = 100
		Elseif FindJack > 0 Then
			JackHereSound
		End If
	End If 

	If n = "captiveball"	Then
		ArrowLights(3,3) =  0
		If FindJack = 3 Then
			FindJack = 100
		Elseif FindJack > 0 Then
			JackHereSound
		End If
	End If 

	If n = "leftinnerloop"	Then
		ArrowLights(4,3) =  0
		If FindJack = 4 Then
			FindJack = 100
		Elseif FindJack > 0 Then
			JackHereSound
		End If
	End If 

	If n = "houseup"	Then
		ArrowLights(5,3) =  0
		If FindJack = 5 Then
			FindJack = 100
		Elseif FindJack > 0 Then
			JackHereSound
		End If
	End If 

	If n = "middlespinner"	Then
		ArrowLights(6,3) =  0
		If FindJack = 6 Then
			FindJack = 100
		Elseif FindJack > 0 Then
			JackHereSound
		End If
	End If 

	If n = "rightramp"	Then
		ArrowLights(7,3) =  0
		If FindJack = 7 Then
			FindJack = 100
		Elseif FindJack > 0 Then
			JackHereSound
		End If
	End If 

	If n = "outerloop"	Then
		ArrowLights(8,3) =  0
		If FindJack = 8 Then
			FindJack = 100
		Elseif FindJack > 0 Then
			JackHereSound
		End If
	End If 

	If n = "rightspinner"	Then
		ArrowLights(9,3) =  0
		If FindJack = 9 Then
			FindJack = 100
		Elseif FindJack > 0 Then
			JackHereSound
		End If
	End If 


'	If n = "leftspinner"	Then ArrowLights(1,3) =  0 : If FindJack = 1 Then FindJack = 100 Else JackHereSound
'	If n = "leftramp"		Then ArrowLights(2,3) =  0 : If FindJack = 2 Then FindJack = 100 Else JackHereSound
'	If n = "captiveball"	Then ArrowLights(3,3) =  0 : If FindJack = 3 Then FindJack = 100 Else JackHereSound
'	If n = "leftinnerloop"	Then ArrowLights(4,3) =  0 : If FindJack = 4 Then FindJack = 100 Else JackHereSound
'	If n = "houseup"		Then ArrowLights(5,3) =  0 : If FindJack = 5 Then FindJack = 100 Else JackHereSound
'	If n = "middlespinner"	Then ArrowLights(6,3) =  0 : If FindJack = 6 Then FindJack = 100 Else JackHereSound
'	If n = "rightramp"		Then ArrowLights(7,3) =  0 : If FindJack = 7 Then FindJack = 100 Else JackHereSound
'	If n = "outerloop"		Then ArrowLights(8,3) =  0 : If FindJack = 8 Then FindJack = 100 Else JackHereSound
'	If n = "rightspinner"	Then ArrowLights(9,3) =  0 : If FindJack = 9 Then FindJack = 100 Else JackHereSound
	If n = "scoophit"		Then ArrowLights(10,3) = 0 
	If n = "target001"		Then ArrowLights(11,3) = 0 
	If n = "target004"		Then ArrowLights(12,3) = 0 
	If n = "target005"		Then ArrowLights(13,3) = 0 
	If n = "target006"		Then ArrowLights(14,3) = 0 
	If n = "target003"		Then ArrowLights(15,3) = 0 
	If n = "target002"		Then ArrowLights(16,3) = 0 
	If n = "bumper1"		Then ArrowLights(17,3) = 0 


End Sub



Sub UpdateExtraballLight
	If Extraball_one(CurrentPlayer) = 1 or Extraball_two(CurrentPlayer) = 1 or Extraball_3(CurrentPlayer) = 1 Then Blink(52,1)= 2 Else Blink(52,1) = 0 
End Sub



Sub DeliverPresent ' scoop hit with deliverlight on
    DOF 268, DOFPulse
	blinklights 53,54,3,5,22,0
'	JackReset
	PresentCallout
	PlaySound"sfx_crowdcheer",1,BackboxVolume * 0.2
	
	If JackResetAvail(CurrentPlayer) = True Then
		JackResetCounter(CurrentPlayer) = JackResetCounter(CurrentPlayer) + 1
		If JackResetCounter(CurrentPlayer) = JackResetNeeded(CurrentPlayer) Then
			JackResetCounter(CurrentPlayer) = 10
			JackResetNeeded(CurrentPlayer) = JackResetNeeded(CurrentPlayer) + 2
			If JackResetNeeded(CurrentPlayer) > 6 Then JackResetNeeded(CurrentPlayer) = 6
			JackResetAvail(CurrentPlayer) = False
			' reset Jack
			Mystery(CurrentPlayer, 1) = 0
			Mystery(CurrentPlayer, 2) = 0
			Mystery(CurrentPlayer, 3) = 0
			Mystery(CurrentPlayer, 4) = 0
			BlinkLights 1,4,10,4,4,0
			UpdateMysteryLights
		End If
	End If

	Delivered_mission(CurrentPlayer) = Delivered_mission(CurrentPlayer)  + 1


	If addaball1(CurrentPlayer) = 0 And Delivered_mission(CurrentPlayer) >= 3 Then
		AddMultiball 1 : addaball1(CurrentPlayer) = 1
		DMD_StartSplash "ADDABALL","REWARD",FontHugeOrange, FontHugeOrange,155 , "oneimage" ,55,0,0
		PlaySound "_Greetings-006",1,BackBoxVolume /2
		EnableBallSaver 7

	End If
	If addaball2(CurrentPlayer) = 0 And Delivered_mission(CurrentPlayer) >= 10 Then
		AddMultiball 1 : addaball2(CurrentPlayer) = 1
		DMD_StartSplash "ADDABALL","REWARD",FontHugeOrange, FontHugeOrange,155 , "oneimage" ,55,0,0
		PlaySound "_Greetings-006",1,BackBoxVolume /2
		EnableBallSaver 7
	End If
	If addaball3(CurrentPlayer) = 0 And Delivered_mission(CurrentPlayer) >= 16 Then
		AddMultiball 1 : addaball3(CurrentPlayer) = 1
		DMD_StartSplash "ADDABALL","REWARD",FontHugeOrange, FontHugeOrange,155 , "oneimage" ,55,0,0
		PlaySound "_Greetings-006",1,BackBoxVolume /2
		EnableBallSaver 7
	End If
	If addaball4(CurrentPlayer) = 0 And Delivered_mission(CurrentPlayer) >= 17 Then
		AddMultiball 1 : addaball4(CurrentPlayer) = 1
		DMD_StartSplash "ADDABALL","REWARD",FontHugeOrange, FontHugeOrange,155 , "oneimage" ,55,0,0
		PlaySound "_Greetings-006",1,BackBoxVolume /2
		EnableBallSaver 7
	End If




	If Delivered_mission(CurrentPlayer) >= 6 And Extraball_one(CurrentPlayer) = 0 Then
		Extraball_one(CurrentPlayer) = 1
		DMD_StartSplash " ","  ",FontHugeOrange, FontHugeRedhalf,130 , "targets2" ,555,0,0
		UpdateExtraballLight
		NightmareShowCounter = 1
		kickdelay = 5000
	End If

	If Delivered_mission(CurrentPlayer) = 9 Then Startwizard ' for nightmare multi
	If Delivered_mission(CurrentPlayer) = 24 Then
		Startwizard ' for last Wizard
		ModeRunning(CurrentPlayer) = 25
		kickdelay = 7000
	Else
		AddScore2 score_DeliverPresent
		Blink(43,1) = 0
		DMD_StartSplash " ", " ", FontHugeOrange, FontHugeRedhalf,180 , "deliverpresent" ,46,47,48

		Presents (ModeRunning(CurrentPlayer),CurrentPlayer) = 1 ' letter solid

		Addrevive(currentplayer) = 2 : UpdateReviveLight : BlinkLights 50,50,10,4,3,0
'		DMD_StartSplash "READY","ADD REVIVE",FontHugeOrange, FontHugeOrange,70 , "oneimage" ,55,0,0

		ModeRunning(CurrentPlayer) = RndInt(1,24)
		For x = 1 to 24
			If Presents (ModeRunning(CurrentPlayer),CurrentPlayer) =  0 Then Exit For
			If x = 24 Then debug.Print "something wrong nextmode fail"
			ModeRunning(CurrentPlayer) = ModeRunning(CurrentPlayer) + 1
			If ModeRunning(CurrentPlayer) > 24 Then ModeRunning(CurrentPlayer) = 1
		Next
'		ModeRunning(CurrentPlayer) = NextMode(currentplayer)
'		NewPresentsNextMode
		BlinkAllPresents 7,2
		StartPresentsMode
	End If

End Sub

Sub UpdateReviveLight
	Blink(50,1) = Addrevive(currentplayer)
End Sub


Sub TurnOffAllBlueLights
	For x = 1 to 17
		ArrowLights(x,3) = 0
	Next
End Sub

Sub NewPresentsNextMode
	If ModeRunning(CurrentPlayer) = 25 or NextMode(currentplayer) = 25 Then NextMode(currentplayer) = 25 : Exit Sub

'	If NextMode(currentplayer) = ModeRunning(CurrentPlayer) Then ModeRunning(CurrentPlayer) = ModeRunning(CurrentPlayer) + 1
	For x = 1 to 24
		NextMode(currentplayer) = NextMode(currentplayer) + 1
		If NextMode(currentplayer) > 24 Then NextMode(currentplayer) = 1
		If NextMode(currentplayer) <> ModeRunning(CurrentPlayer)  And  Presents (NextMode(currentplayer),CurrentPlayer) = 0 Then Exit For
	Next
End Sub

Sub BlinkAllPresents(nr,delay)
	BlinkLights nightmarestart , nightmarestart + 23, nr ,7,4,delay
End Sub

Sub UpdatePresentsLights
	For x = 1 to 24
		If WizardMode(CurrentPlayer) = 1 Then
			If x < 10 Then Blink( nightmarestart + x - 1 ,1 ) = 2 Else Blink( nightmarestart + x - 1 ,1 ) = 0
		Else
			If x < Delivered_mission(CurrentPlayer) + 1 Then Blink( nightmarestart + x - 1 ,1 ) = 1
			If x = Delivered_mission(CurrentPlayer) + 1 Then Blink( nightmarestart + x - 1 ,1 ) = 2
'			Blink( nightmarestart + x - 1 ,1 ) = Presents(x, CurrentPlayer)
		End If
	Next
'	If ModeRunning(CurrentPlayer) > 0 And ModeRunning(CurrentPlayer) < 25  Then Blink((nightmarestart - 1 + ModeRunning(CurrentPlayer)),1) = 2
'	debug.print nightmarestart + ModeRunning(CurrentPlayer) - 1
End Sub

Sub ResetPresentsLights
	For x = 1 to 24 
		Presents(x,1) = 0
		Presents(x,2) = 0
		Presents(x,3) = 0
		Presents(x,4) = 0
	Next
End Sub

Sub Add_Revive
	If Blink(50,1) = 0 Then Exit Sub
	Addrevive(currentplayer) = 0 : UpdateReviveLight
	BlinkLights 50,50,3,5,5,0

	If Blink(49,1) = 0 Then
		AddScore score_new_Revive
		Blink(49,1) = 1
		BlinkLights 49,49,10,6,5,0
		DMD_StartSplash "ACTIVATED","RIGHT REVIVE",FontHugeOrange, FontHugeOrange,70 , "oneimage" ,55,0,0
	Elseif Blink(48,1) = 0 Then
		AddScore score_new_Revive
		Blink(48,1) = 1
		BlinkLights 48,48,10,6,5,0
		DMD_StartSplash "ACTIVATED","LEFT REVIVE",FontHugeOrange, FontHugeOrange,70 , "oneimage" ,55,0,0
	Else
		Addscore2 score_Full_Revive
		BlinkLights 48,50,10,6,5,0
		DMD_StartSplash "REVIVE BONUS",FormatScore(score_Full_Revive) ,FontHugeOrange, FontHugeOrange,70 , "oneimage" ,55,0,0
	End If
End Sub

Dim ReviveBall1 : ReviveBall1 = False
Dim ReviveBall2 : ReviveBall2 = False
Sub Revive_Right ' trigger004
	If(bBallSaverActive = True) or BallsaveExtended < 3 Then BallsaveExtended = 0 : Exit Sub

	If Blink(49,1) = 0 Then Playsound"sfx_giggle",1,BackboxVolume : Exit Sub

	Blink(49,1) = 0
	BlinkLights 49,49,10,6,5,0
	ReviveBall1 = True
	revivesound
End Sub

Sub Revive_Left	'trigger001

	If(bBallSaverActive = True) or BallsaveExtended < 3 Then BallsaveExtended = 0 : Exit Sub

	If Blink(48,1) = 0 Then Playsound "sfx_giggle" ,.5,BackboxVolume : Exit Sub

	Blink(48,1) = 0
	BlinkLights 48,48,10,6,5,0
	ReviveBall2 = True
	revivesound
End Sub



'***********************************
'**** cwizardstuff
'***********************************
Dim AddWizHit1
Dim WizardStage
Dim WizardBackground : WizardBackground = 5
Sub Startwizard
	WizardsDone(CurrentPlayer) = WizardsDone(CurrentPlayer) + 1 ' first = wiz 1

	Select Case WizardsDone(CurrentPlayer)
		Case 1		' NIGHTMARE MULTIBALL start

				WizardMode(CurrentPlayer) = 1
				AddWizHit1 = 0
				For x = 1 to 17 : ArrowLights(x,4) = 0 : next ''' to be sure they be off
				EnableBallSaver 28
				AddMultiball 2
				FlushSplashQ
				DMD_StartSplash "NIGHTMARE","MULTIBALL" ,FontHugeOrange, FontHugeOrange,240 , "nightmare" ,1000,0,0 
				DMD_StartSplash NightmareLitJP & "+ SWITCHES","LIGHTS JACKPOTS" ,FontHugeOrange, FontHugeOrange,77 , "oneimage" ,55,0,0 
				kickdelay = 4000
				PlaySound "vo_hailtothepumkinking",1,BackBoxVolume
				PlayVoice"vo_multiball1"
				presentseffect 3

		Case 2				' WIZARD  STAGE 1 start  100 switches
			ModeTimer.Enabled = False ' to be sure its not running when testing
			WizardProgress = 0
			WizardStage = 1
			WizardMode(CurrentPlayer) = 2
			AddMultiball 2
			ResetAllBlink
			EnableBallSaver 66


			UpdateExtraballLight

			For x = 1 to 17
				ArrowLights(x,1) = 2
				ArrowLights(x,2) = 2
				ArrowLights(x,3) = 0
				ArrowLights(x,4) = 0
				ChangeArrowColor x,2,"red",45,"1100110011111"
			Next
			Blink( 1,1) = 2
			Blink( 2,1) = 2
			Blink( 3,1) = 2
			Blink( 4,1) = 2
			WizardTarget = 100
			DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "wiz1start" ,999,0,0
			PlayVoice "vo_lasttimeboogiesong"

			greenoogie = False
			LightEffect 5
			presentseffect 3
			BlinkAllPresents 3,4
			WizardBackground = WizardBackground + 1
			If WizardBackground > 5 Then WizardBackground = 1


	End Select
	ChangeSong 
	ChangeGi white   ' sets color for wizards amber and red
End Sub

Sub StopWizard
	FlushSplashQ
	For x = 1 to 17 : ArrowLights(x,4) = 0 : next

	AddWizHit1 = 0
	UpdateLockLights

	If WizardMode(CurrentPlayer) = 2 Then
		For x = 1 to 17
			ArrowLights(x,1) = 0
			ArrowLights(x,2) = 0
			ArrowLights(x,3) = 0
			ArrowLights(x,4) = 0
		Next
		WizardStage = 0
		resetplayer CurrentPlayer
		ResetForNewPlayerBall
		StartPresentsMode

		WizardsDone(CurrentPlayer) = 0
	
	End If
	'end of nightmare is here .. will work for wiz2 aswell

	WizardMode(CurrentPlayer) = 0 ' turn off
	ChangeSong
End Sub




Dim WizardProgress
Dim WizardTarget

Sub Wizardhit(n)
	dim tmp
	Select Case WizardMode(CurrentPlayer)
		case 1 ' nightmare multiball ''' 50+ hits = lit super

			Select Case n
				' all trigers go here + spinner spins spinner hits ignored
				Case "leftspinner" 
				Case "middlespinner"
				Case "rightspinner"
				Case Else AddWizHitNightMare
			End Select
			tmp = 0
			for x = 1 to 10
				If ArrowLights(x,4) = 2 Then tmp = tmp + score_nightmare_nrlitsupers
			Next
			
			Select Case n
				Case "leftspinner"
						If ArrowLights(1,4) = 2 Then
							ArrowLights(1,4) = 0
							AddScore2 score_nightmare_supers + tmp
							DMD_StartSplash "JACKPOT",FormatScore(score_nightmare_supers + tmp) ,FontHugeOrange, FontHugeOrange,150 , "oneimage" ,55,0,0
							Playsound "vo_jackpotoogi",1,BackboxVolume
						End If

				Case "leftramp"			: If ArrowLights(2,4) = 2 Then ArrowLights(2,4) = 0 : AddScore2 score_nightmare_supers + tmp : DMD_StartSplash "JACKPOT",FormatScore(score_nightmare_supers + tmp) ,FontHugeOrange, FontHugeOrange,90 , "oneimage" ,55,0,0 : Playsound "vo_jackpotoogi",1,BackboxVolume
				Case "captiveball"		: If ArrowLights(3,4) = 2 Then ArrowLights(3,4) = 0 : AddScore2 score_nightmare_supers + tmp : DMD_StartSplash "JACKPOT",FormatScore(score_nightmare_supers + tmp) ,FontHugeOrange, FontHugeOrange,90 , "oneimage" ,55,0,0 : Playsound "vo_jackpotoogi",1,BackboxVolume
				Case "leftinnerloop"	: If ArrowLights(4,4) = 2 Then ArrowLights(4,4) = 0 : AddScore2 score_nightmare_supers + tmp : DMD_StartSplash "JACKPOT",FormatScore(score_nightmare_supers + tmp) ,FontHugeOrange, FontHugeOrange,90 , "oneimage" ,55,0,0 : Playsound "vo_jackpotoogi",1,BackboxVolume
				Case "houseup"			: If ArrowLights(5,4) = 2 Then ArrowLights(5,4) = 0 : AddScore2 score_nightmare_supers + tmp : DMD_StartSplash "JACKPOT",FormatScore(score_nightmare_supers + tmp) ,FontHugeOrange, FontHugeOrange,90 , "oneimage" ,55,0,0 : Playsound "vo_jackpotoogi",1,BackboxVolume
				Case "middlespinner"	: If ArrowLights(6,4) = 2 Then ArrowLights(6,4) = 0 : AddScore2 score_nightmare_supers + tmp : DMD_StartSplash "JACKPOT",FormatScore(score_nightmare_supers + tmp) ,FontHugeOrange, FontHugeOrange,90 , "oneimage" ,55,0,0 : Playsound "vo_jackpotoogi",1,BackboxVolume
				Case "rightramp"		: If ArrowLights(7,4) = 2 Then ArrowLights(7,4) = 0 : AddScore2 score_nightmare_supers + tmp : DMD_StartSplash "JACKPOT",FormatScore(score_nightmare_supers + tmp) ,FontHugeOrange, FontHugeOrange,90 , "oneimage" ,55,0,0 : Playsound "vo_jackpotoogi",1,BackboxVolume
				Case "outerloop"		: If ArrowLights(8,4) = 2 Then ArrowLights(8,4) = 0 : AddScore2 score_nightmare_supers + tmp : DMD_StartSplash "JACKPOT",FormatScore(score_nightmare_supers + tmp) ,FontHugeOrange, FontHugeOrange,90 , "oneimage" ,55,0,0 : Playsound "vo_jackpotoogi",1,BackboxVolume
				Case "rightspinner"		: If ArrowLights(9,4) = 2 Then ArrowLights(9,4) = 0 : AddScore2 score_nightmare_supers + tmp : DMD_StartSplash "JACKPOT",FormatScore(score_nightmare_supers + tmp) ,FontHugeOrange, FontHugeOrange,90 , "oneimage" ,55,0,0 : Playsound "vo_jackpotoogi",1,BackboxVolume
				Case "scoophit"			: If ArrowLights(10,4)= 2 Then ArrowLights(10,4)= 0 : AddScore2 score_nightmare_supers + tmp : DMD_StartSplash "JACKPOT",FormatScore(score_nightmare_supers + tmp) ,FontHugeOrange, FontHugeOrange,90 , "oneimage" ,55,0,0 : Playsound "vo_jackpotoogi",1,BackboxVolume
			End Select


'Big Wizard at 24 presents - 3 ball 1 minute respawn very hard wizard 
'stage1 : 100 triggers			 50k
'stage2 : all 17 arrowlights once	100k
'stage3 : 10 ramps			200k	
'stage4 : superjackpot 2 times		  2kk	
'stage5 : 150 triggers			100k
'stage6 : all arrows 2 times each	200k
'stage7 : superjackpot 2 times		  3kk
'stage8 : scoop for finish mega jackpot ? 30kk
'repeat ?




		Case 2
			addscore score_wizAllHits ' all triggers get this always in wizmode

			Select Case WizardStage
				Case 1 ' wiz1 stage1

					Select Case n
							Case "leftspinner" 'na
							Case "middlespinner"'na
							Case "rightspinner"'na
							Case Else
								WizardProgress = WizardProgress + 1
								If WizardProgress < 100 Then
									DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,30 , "wiz1progress" ,999,0,0
									addscore2 score_wiz1_hits
									If WizardProgress = 20 Then playsound "vo_hahahahaaa" ,1,BackboxVolume * 0.7 : presentseffect 1
									If WizardProgress = 30 Then playsound "vo_oohhoohooha",1,BackboxVolume * 0.9 : presentseffect 1
									If WizardProgress = 50 Then playsound "vo_oohhoohooha",1,BackboxVolume * 0.7 : presentseffect 1
									If WizardProgress = 60 Then playsound "vo_oohhoohooha",1,BackboxVolume * 0.9 : presentseffect 1
									If WizardProgress = 70 Then playsound "vo_hahahahaaa" ,1,BackboxVolume * 0.7 : presentseffect 1
								Else
		'							DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,140 , "wiz2start" ,999,0,0
									addscore2 score_wiz1_done
									Blink(50,1)= 1 : Add_Revive
									start_wizardstage2
								End If
					End Select



				Case 2 ' wiz1 stage2
					Select Case n
							Case "leftspinner"		: If ArrowLights( 1,1) = 2 Then ArrowLights( 1,1) = 0 : ArrowLights( 1,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 1"
							Case "leftramp"			: If ArrowLights( 2,1) = 2 Then ArrowLights( 2,1) = 0 : ArrowLights( 2,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 2" 
							Case "captiveball"		: If ArrowLights( 3,1) = 2 Then ArrowLights( 3,1) = 0 : ArrowLights( 3,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 3" 
							Case "leftinnerloop"	: If ArrowLights( 4,1) = 2 Then ArrowLights( 4,1) = 0 : ArrowLights( 4,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 4"
							Case "houseup"			: If ArrowLights( 5,1) = 2 Then ArrowLights( 5,1) = 0 : ArrowLights( 5,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 5"
							Case "middlespinner"	: If ArrowLights( 6,1) = 2 Then ArrowLights( 6,1) = 0 : ArrowLights( 6,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 6"
							Case "rightramp"		: If ArrowLights( 7,1) = 2 Then ArrowLights( 7,1) = 0 : ArrowLights( 7,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 7"
							Case "outerloop"		: If ArrowLights( 8,1) = 2 Then ArrowLights( 8,1) = 0 : ArrowLights( 8,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 8"
							Case "rightspinner"		: If ArrowLights( 9,1) = 2 Then ArrowLights( 9,1) = 0 : ArrowLights( 9,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 9"
							Case "scoophit"			: If ArrowLights(10,1) = 2 Then ArrowLights(10,1) = 0 : ArrowLights(10,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 10"
							Case "target001"		: If ArrowLights(11,1) = 2 Then ArrowLights(11,1) = 0 : ArrowLights(11,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 11"
							Case "target004"		: If ArrowLights(12,1) = 2 Then ArrowLights(12,1) = 0 : ArrowLights(12,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 12"
							Case "target005"		: If ArrowLights(13,1) = 2 Then ArrowLights(13,1) = 0 : ArrowLights(13,3) = 0 : wiz1stage2Hit' : debug.print "wiz1stage2hit 13"
							Case "target006"		: If ArrowLights(14,1) = 2 Then ArrowLights(14,1) = 0 : ArrowLights(14,3) = 0 : wiz1stage2Hit 
							Case "target003"		: If ArrowLights(15,1) = 2 Then ArrowLights(15,1) = 0 : ArrowLights(15,3) = 0 : wiz1stage2Hit
							Case "target002"		: If ArrowLights(16,1) = 2 Then ArrowLights(16,1) = 0 : ArrowLights(16,3) = 0 : wiz1stage2Hit
							Case "bumper1"			: If ArrowLights(17,1) = 2 Then ArrowLights(17,1) = 0 : ArrowLights(17,3) = 0 : wiz1stage2Hit 
					End Select

				Case 3 ' wiz1 stage3
					Select Case n
							Case "leftramp"			: wiz1stage3Hit : debug.print "wiz1stage3hit"
							Case "rightramp"		: wiz1stage3Hit : debug.print "wiz1stage3hit"
					End Select



				Case 4 ' wiz1 stage4  superjackpot 2 times		  2kk	
						if n = "outerloop" Then wiz1stage4Hit

				Case 5 ' wiz1 stage5 150 switches
					Select Case n
							Case "leftspinner" 'na
							Case "middlespinner"'na
							Case "rightspinner"'na
							Case Else
								WizardProgress = WizardProgress + 1
								If WizardProgress < 150 Then
									DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,30 , "wiz5progress" ,999,0,0
									addscore2 score_wiz1_hits
									If WizardProgress = 20 Then playsound "vo_hahahahaaa" ,1,BackboxVolume * 0.7 : presentseffect 1
									If WizardProgress = 30 Then playsound "vo_oohhoohooha",1,BackboxVolume * 0.9 : presentseffect 1
									If WizardProgress = 50 Then playsound "vo_oohhoohooha",1,BackboxVolume * 0.7 : presentseffect 1
									If WizardProgress = 60 Then playsound "vo_oohhoohooha",1,BackboxVolume * 0.9 : presentseffect 1
									If WizardProgress = 70 Then playsound "vo_hahahahaaa" ,1,BackboxVolume * 0.7 : presentseffect 1
								Else
		'							DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,140 , "wiz2start" ,999,0,0
									addscore2 score_wiz1_done
									Blink(50,1)= 1 : Add_Revive
									start_wizardstage6
								End If
					End Select




				Case 6 ' wiz1 stage6
					Select Case n
							Case "leftspinner"		: If ArrowLights( 1,1) = 2 Then ArrowLights( 1,1) = 0 : ArrowLights( 1,3) = 0 : wiz1stage6Hit
							Case "leftramp"			: If ArrowLights( 2,1) = 2 Then ArrowLights( 2,1) = 0 : ArrowLights( 2,3) = 0 : wiz1stage6Hit
							Case "captiveball"		: If ArrowLights( 3,1) = 2 Then ArrowLights( 3,1) = 0 : ArrowLights( 3,3) = 0 : wiz1stage6Hit
							Case "leftinnerloop"	: If ArrowLights( 4,1) = 2 Then ArrowLights( 4,1) = 0 : ArrowLights( 4,3) = 0 : wiz1stage6Hit
							Case "houseup"			: If ArrowLights( 5,1) = 2 Then ArrowLights( 5,1) = 0 : ArrowLights( 5,3) = 0 : wiz1stage6Hit
							Case "middlespinner"	: If ArrowLights( 6,1) = 2 Then ArrowLights( 6,1) = 0 : ArrowLights( 6,3) = 0 : wiz1stage6Hit
							Case "rightramp"		: If ArrowLights( 7,1) = 2 Then ArrowLights( 7,1) = 0 : ArrowLights( 7,3) = 0 : wiz1stage6Hit
							Case "outerloop"		: If ArrowLights( 8,1) = 2 Then ArrowLights( 8,1) = 0 : ArrowLights( 8,3) = 0 : wiz1stage6Hit
							Case "rightspinner"		: If ArrowLights( 9,1) = 2 Then ArrowLights( 9,1) = 0 : ArrowLights( 9,3) = 0 : wiz1stage6Hit
							Case "scoophit"			: If ArrowLights(10,1) = 2 Then ArrowLights(10,1) = 0 : ArrowLights(10,3) = 0 : wiz1stage6Hit
							Case "target001"		: If ArrowLights(11,1) = 2 Then ArrowLights(11,1) = 0 : ArrowLights(11,3) = 0 : wiz1stage6Hit
							Case "target004"		: If ArrowLights(12,1) = 2 Then ArrowLights(12,1) = 0 : ArrowLights(12,3) = 0 : wiz1stage6Hit
							Case "target005"		: If ArrowLights(13,1) = 2 Then ArrowLights(13,1) = 0 : ArrowLights(13,3) = 0 : wiz1stage6Hit
							Case "target006"		: If ArrowLights(14,1) = 2 Then ArrowLights(14,1) = 0 : ArrowLights(14,3) = 0 : wiz1stage6Hit 
							Case "target003"		: If ArrowLights(15,1) = 2 Then ArrowLights(15,1) = 0 : ArrowLights(15,3) = 0 : wiz1stage6Hit
							Case "target002"		: If ArrowLights(16,1) = 2 Then ArrowLights(16,1) = 0 : ArrowLights(16,3) = 0 : wiz1stage6Hit
							Case "bumper1"			: If ArrowLights(17,1) = 2 Then ArrowLights(17,1) = 0 : ArrowLights(17,3) = 0 : wiz1stage6Hit 
					End Select


				Case 7 ' wiz1 stage7
						if n = "outerloop" Then wiz1stage7Hit


				Case 8 ' wiz1 stage8
					If n = "scoophit" Then
						StopWizard
						AddScore2 score_wiz8_hits
						DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "wiz8progress" ,999,0,0
						DMD_StartSplash "VICTORIOUS""HOLIDAY BATTLE",,FontHugeOrange, FontHugeOrange,123 , "oneimage" ,55,0,0
						DMD_StartSplash "DO IT AGAIN","CAN YOU" ,"DO IT AGAIN",FontHugeOrange, FontHugeOrange,123 , "oneimage" ,55,0,0
						DMD_StartSplash "GAME" ,"RESTARTING",FontHugeOrange, FontHugeOrange,123 , "oneimage" ,55,0,0
						'hold ball extra !
						kickdelay = 9090


					End If
			End Select
	End Select
End Sub
'							Case "leftspinner"
'							Case "leftramp"
'							Case "captiveball"
'							Case "leftinnerloop"
'							Case "houseup"
'							Case "middlespinner"
'							Case "rightramp"
'							Case "outerloop"
'							Case "rightspinner"
'							Case "scoophit"
'							Case "target001"
'							Case "target004"
'							Case "target005"
'							Case "target006"
'							Case "target003"
'							Case "target002"
'							Case "bumper1"
'							Case "loopdown"
'							Case "rightoutlane"
'							Case "rightinlane"
'							Case "leftoutlane"
'							Case "leftinlane"

Dim nextwizsound
Sub newwizardstagesounds
	nextwizsound = nextwizsound + 1
	If nextwizsound > 3 Then nextwizsound = 1
	Select Case nextwizsound
		Case 1 : PlayVoice "vo_yourejoking1"
		Case 2 : PlayVoice "vo_yourejoking2" 
		Case 3 : PlayVoice "vo_whathavewehere"
	End Select
End Sub



Sub wiz1stage6Hit
	WizardProgress = WizardProgress + 1

	If WizardProgress = 5 Then playsound "vo_hahahahaaa" ,1,BackboxVolume * 0.7 : presentseffect 1
	If WizardProgress = 8 Then playsound "vo_oohhoohooha",1,BackboxVolume * 0.9 : presentseffect 1
	If WizardProgress = 10 Then playsound "vo_alllaughing",1,BackboxVolume * 0.7 : presentseffect 1
	If WizardProgress = 12 Then playsound "vo_oohhoohooha",1,BackboxVolume * 0.9 : presentseffect 1
	If WizardProgress = 14 Then playsound "vo_hahahahaaa" ,1,BackboxVolume * 0.7 : presentseffect 1

	If WizardProgress < 17 Then
		AddScore2 score_wiz6_hits
		presentseffect 3
		DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,88 , "wiz2progress" ,999,0,0
	Else
		AddScore2 score_wiz6_done
		presentseffect 2
		FlushSplashQ
		Blink(50,1)= 1 : Add_Revive
		WizardStage = 3
		WizardProgress = 0
		start_wizardstage7
	End If
End Sub



Sub start_wizardstage6 ' all 17 arrowlights once	100k
	FlushSplashQ
	WizardBackground = WizardBackground + 1
	If WizardBackground > 5 Then WizardBackground = 1

	ResetAllBlink
	UpdateExtraballLight
	For x = 1 to 17
		ArrowLights(x,1) = 0
		ArrowLights(x,2) = 0
		ArrowLights(x,3) = 2
		ArrowLights(x,4) = 0
		ChangeArrowColor x,2,"purple",45,"11011100111111111"
	Next
	WizardTarget = 17
	WizardStage = 6
	WizardProgress = 0

	newwizardstagesounds
	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "wiz6start" ,999,0,0

End Sub


Sub start_wizardstage7
	WizardBackground = WizardBackground + 1
	If WizardBackground > 5 Then WizardBackground = 1


	ResetAllBlink
	UpdateExtraballLight
	For x = 1 to 17
		ArrowLights(x,1) = 0
		ArrowLights(x,2) = 0
		ArrowLights(x,3) = 0
		ArrowLights(x,4) = 0
	Next
	'42 44 arrowlights(8
	Blink(42,1) = 2
	Blink(44,1) = 2
	ArrowLights(8,2) = 2
	ChangeArrowColor 8,2,"red",55,"110"
	WizardTarget = 2
	WizardStage = 7
	WizardProgress = 0

	newwizardstagesounds
	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "wiz7start" ,999,0,0

End Sub


Sub wiz1stage7Hit
	WizardProgress = WizardProgress + 1
	If WizardProgress < 2 Then
		AddScore2 score_wiz7_hits
		PlayVoice "vo_excellentshot"
		DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "wiz7progress" ,999,0,0
	Else
		AddScore2 score_wiz7_done
		Blink(50,1)= 1 : Add_Revive
		WizardStage = 5
		WizardProgress = 0
		start_wizardstage5
	End If
End Sub


Sub start_wizardstage8

	WizardBackground = WizardBackground + 1
	If WizardBackground > 5 Then WizardBackground = 1


	ResetAllBlink
	UpdateExtraballLight
	For x = 1 to 17
		ArrowLights(x,1) = 0
		ArrowLights(x,2) = 0
		ArrowLights(x,3) = 0
		ArrowLights(x,4) = 0
	Next
	'42 44 arrowlights(8
	ChangeArrowColor 10,2,"red",55,"10"
	WizardTarget = 1
	WizardStage = 8
	WizardProgress = 0

	newwizardstagesounds
	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "wiz8start" ,999,0,0

End Sub


'stage5 : 150 triggers			100k
'stage6 : all arrows 2 times each	200k
'stage7 : superjackpot 2 times		  3kk
'stage8 : scoop for finish mega jackpot ? 30kk

Sub start_wizardstage5
	WizardBackground = WizardBackground + 1
	If WizardBackground > 5 Then WizardBackground = 1

	ResetAllBlink
	UpdateExtraballLight
	For x = 1 to 17
		ArrowLights(x,1) = 0
		ArrowLights(x,2) = 0
		ArrowLights(x,3) = 0
		ArrowLights(x,4) = 0
		ChangeArrowColor x,2,"orange",45,"101010110110110"
	Next

	WizardTarget = 150
	WizardStage = 5
	WizardProgress = 0

	newwizardstagesounds
	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "wiz5start" ,999,0,0
End Sub

Sub wiz1stage4Hit
	WizardProgress = WizardProgress + 1
	If WizardProgress < 2 Then
		AddScore2 score_wiz4_hits
		PlayVoice "vo_excellentshot"
		DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "wiz4progress" ,999,0,0
	Else
		AddScore2 score_wiz4_done
		Blink(50,1)= 1 : Add_Revive
		WizardStage = 5
		WizardProgress = 0
		start_wizardstage5
	End If
End Sub


Sub start_wizardstage4	
	WizardBackground = WizardBackground + 1
	If WizardBackground > 5 Then WizardBackground = 1


	ResetAllBlink
	UpdateExtraballLight
	For x = 1 to 17
		ArrowLights(x,1) = 0
		ArrowLights(x,2) = 0
		ArrowLights(x,3) = 0
		ArrowLights(x,4) = 0
	Next
	'42 44 arrowlights(8
	Blink(42,1) = 2
	Blink(44,1) = 2
	ArrowLights(8,2) = 2
	ChangeArrowColor 8,2,"red",55,"110"
	WizardTarget = 2
	WizardStage = 4
	WizardProgress = 0


	newwizardstagesounds
	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "wiz4start" ,999,0,0
End Sub



Sub wiz1stage3Hit
	WizardProgress = WizardProgress + 1

	If WizardProgress = 2 Then playsound "vo_hahahahaaa" ,1,BackboxVolume * 0.7 : presentseffect 1
	If WizardProgress = 5 Then playsound "vo_oohhoohooha",1,BackboxVolume * 0.9 : presentseffect 1
	If WizardProgress = 4 Then playsound "vo_alllaughing",1,BackboxVolume * 0.7 : presentseffect 1
	If WizardProgress = 7 Then playsound "vo_oohhoohooha",1,BackboxVolume * 0.9 : presentseffect 1
	If WizardProgress = 8 Then playsound "vo_hahahahaaa" ,1,BackboxVolume * 0.7 : presentseffect 1

	If WizardProgress < 10 Then
		AddScore2 score_wiz3_hits
		presentseffect 3
		DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,88 , "wiz3progress" ,999,0,0
		PlayVoice "vo_fantastic"	

	Else
		AddScore2 score_wiz3_done
		presentseffect 2
		Blink(50,1)= 1 : Add_Revive
		WizardStage = 4
		WizardProgress = 0
		start_wizardstage4
	End If

End Sub


Sub start_wizardstage3		'stage3 : 10 ramps			200k	
	AddMultiball 1
	DMD_StartSplash " ","  ",FontHugeOrange, FontHugeRedhalf,77 , "addaball" ,555,0,0 
	WizardBackground = WizardBackground + 1
	If WizardBackground > 5 Then WizardBackground = 1

	ResetAllBlink
	If bBallSaverActive = True Then Blink(39,1) = 2

	UpdateExtraballLight
	For x = 1 to 17
		ArrowLights(x,1) = 0
		ArrowLights(x,2) = 0
		ArrowLights(x,3) = 0
		ArrowLights(x,4) = 0
	Next
	ArrowLights(2,2) = 2
	ChangeArrowColor 2,2,"orange",45,"11011011111111111"
	ArrowLights(7,2) = 2
	ChangeArrowColor 7,2,"orange",45,"11011011111111111"
	WizardTarget = 10
	WizardStage = 3
	WizardProgress = 0
	
	newwizardstagesounds
	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "wiz3start" ,999,0,0
End Sub



Sub wiz1stage2Hit
	WizardProgress = WizardProgress + 1

	If WizardProgress = 5 Then playsound "vo_hahahahaaa" ,1,BackboxVolume * 0.7 : presentseffect 1
	If WizardProgress = 8 Then playsound "vo_oohhoohooha",1,BackboxVolume * 0.9 : presentseffect 1
	If WizardProgress = 10 Then playsound "vo_alllaughing",1,BackboxVolume * 0.7 : presentseffect 1
	If WizardProgress = 12 Then playsound "vo_oohhoohooha",1,BackboxVolume * 0.9 : presentseffect 1
	If WizardProgress = 14 Then playsound "vo_hahahahaaa" ,1,BackboxVolume * 0.7 : presentseffect 1

	If WizardProgress < 17 Then
		AddScore2 score_wiz2_hits
		presentseffect 3
		DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,88 , "wiz2progress" ,999,0,0
	Else
		AddScore2 score_wiz2_done
		presentseffect 2
		FlushSplashQ
		Blink(50,1)= 1 : Add_Revive
		WizardStage = 3
		WizardProgress = 0
		DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "wiz3start" ,999,0,0
		start_wizardstage3
	End If
End Sub



Sub start_wizardstage2 ' all 17 arrowlights once	100k
	FlushSplashQ
	WizardBackground = WizardBackground + 1
	If WizardBackground > 5 Then WizardBackground = 1

	ResetAllBlink
	If bBallSaverActive = True Then Blink(39,1) = 2

	UpdateExtraballLight

	For x = 1 to 17
		ArrowLights(x,1) = 0
		ArrowLights(x,2) = 0
		ArrowLights(x,3) = 2
		ArrowLights(x,4) = 0
		ChangeArrowColor x,2,"teal",45,"11011100111111111"
	Next
	WizardTarget = 17
	WizardStage = 2
	WizardProgress = 0

	newwizardstagesounds
	DMD_StartSplash " "," " ,FontHugeOrange, FontHugeRedhalf,200 , "wiz2start" ,999,0,0

End Sub



Sub AddWizHitNightMare
	dim tmp
	AddWizHit1 = AddWizHit1 + 1
	AddScore score_nightmare_triggers 
	tmp = 0
	For x = 1 To 10
		If ArrowLights(x,1) = 2 Then tmp = tmp + 5
	Next
	If AddWizHit1 => NightmareLitJP + tmp Then		' + lit jackpots ?
		AddWizHit1 = 0
		' add random 9 lanes for jackpot 
		tmp = RndInt(1,10)
		For x = 1 To 10
			If tmp = 11 Then tmp = 1
			If ArrowLights(tmp,1) = 0 Then arrowlights(tmp,4) = 2 : Exit For		
			tmp = tmp + 1
			If x = 10 Then
				AddScore2 score_nightmare_alllit	
				Exit Sub
			End If
		Next
		DMD_StartSplash "NIGHTMARE","JACKPOT IS LIT",FontHugeOrange, FontHugeOrange,90 , "oneimage" ,55,0,0
		DontForgetTheJackpot
	End If

End Sub
Wheel000.x = 347
Wheel001.x = 355
Wheel002.x = 363
Dim wheelnumber : wheelnumber = 0
Dim wheeltimer : wheeltimer = 0

Sub updatecountdown (nr)
	wheel002.timerenabled = True
	wheeltimer = 0
	wheelnumber = nr
End Sub


Sub wheel002_Timer
	wheeltimer = wheeltimer + 1
	If wheeltimer > 3 Then
		Dim tmp,tmp2
		tmp2 = wheelnumber
		tmp = int( tmp2 / 10 )
		wheel000.imagea = "countdown0"
		tmp2 = tmp2 - tmp * 10
		wheel001.imagea = "countdown" & tmp
		wheel002.imagea = "countdown" & tmp2
		wheel002.timerenabled = false
	Else
		wheel000.imagea = "countdown" & RndInt(0,3)
		wheel001.imagea = "countdown" & RndInt(0,6)
		wheel002.imagea = "countdown" & RndInt(0,5)
	End If
End Sub

Dim LastSfx
Sub PlaySfx(sound)
	If LastSfx <> "" Then stopsound LastSfx
	LastSfx = sound
	PlaySound sound,1, BackboxVolume / 3
End Sub

Dim LastSfx2
Sub PlaySfx2(sound)
	If LastSfx2 <> "" Then stopsound LastSfx2
	LastSfx2 = sound
	PlaySound sound,1, BackboxVolume * 0.2
End Sub

Dim LastPlayVO
ComboPriority.interval = 3333 ' 3.333 seconds   change this if you need bigger stop time
Sub PlayVoice ( sound ) ' volume = 0 to 1
    If sound = "vo_Combo" or sound = "vo_2xCombo" or sound = "vo_3xCombo" or sound = "vo_4xCombo" or sound = "vo_5xCombo" Then
        If ComboPriority.enabled = False Then PlayVoice2 sound
        Exit Sub
    End If
    Playvoice2 sound
    ComboPriority.enabled = False
    ComboPriority.enabled = True
End Sub
Sub ComboPriority_timer
    ComboPriority.enabled = False
End Sub
Sub Playvoice2(sound)
    If lastplayVO <> "" Then stopsound lastplayVO
    lastplayVO = sound
    PlaySound sound,1, BackboxVolume /1.2
End Sub


Sub	StopVO 
	If lastplayVO <> "" Then stopsound lastplayVO
	lastplayVO = ""
End Sub

'****************************************************************
'		VR Mode
'****************************************************************
'Detect if VPX is rendering in VR and then make sure the VR Room Choice is used
Dim VRRoom
DIM VRThings

Sub LoadVRRoom
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
	for each VRThings in VR_Mega:VRThings.visible = 0:Next
	lrail.visible = True
	rrail.visible = True

	If Table1.ShowDT = False then
		rrail.visible = 0	
		lrail.visible = 0
	End If

	wall21.sidevisible = True
	TextOverlay001.rotx = -4.8
	Flasherbloom1.rotx = -4.6
    TextOverlay001.height = 280
'    Primitive181.visible = True
'	Primitive179.visible = True
    DMD.visible = False
	If RenderingMode = 2 or VRTest Then
		VRRoom = VRRoomChoice
	Else
		VRRoom = 0
	End If
    If VRRoom > 0 Then
		Wall8.visible = 0
		Wall8.sidevisible = 0
		Wall9.sidevisible = 0
		Wall9.visible = 0

		TextOverlay001.rotx = -7.2
        TextOverlay001.height = 278
		Flasherbloom1.rotx = -7.2
		lrail.visible = 0
		rrail.visible = 0
		wall21.sidevisible = 0
		Primitive181.visible = False
		Primitive179.visible = False
		DMD.visible = True
    End If
	If VRRoom = 0 Then
		for each VRThings in VR_Cab:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next
	End If
	If VRRoom = 1 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 1:Next
	End If
End Sub

'**********************************************************************************************************
' VR Plunger
'**********************************************************************************************************

Sub TimerVRPlunger_Timer
	If PinCab_Shooter.Y < 180 then
		PinCab_Shooter.Y = PinCab_Shooter.Y + 5
	End If
End Sub

Sub TimerVRPlunger2_Timer
	PinCab_Shooter.Y = 55 + (5* Plunger.Position)
End Sub



'*************************************************************************

'On the DOF website put Exxx
'DOF commands:
'101 Left Flipper
'102 Right Flipper
'103 
'104 
'105 LeftSlingShot
'106 RightSlingShot
'107 MAGNET TARGET
'108 RIGHT Bumper
'109 cabin target 1
'110
'111 Scoop
'118 GION
'119
'120 AutoFire
'122 knocker
'123 ballrelease
'151 LEFTOUTLANE  J
'152 LEFTINLANE   A
'153 RIGHTINLANE  C
'154 RIGHTOUTLANE K
'155 ball launch
'156 Revive 1
'157 Revive 2
'158 BALL 1 LOCK
'159 BALL 2 LOCK
'160 BALL 3 LOCK
'161 MULTIBALL
'204 Lamp posts (teenagers)
'205 Blue targets
'206 Ramps, lanes and spinners
'207 In and outlanes
'208 BALL READY
'250 ATTRACT
'251 BONE RAMP START
'252 RIGHT RAMP
'253 BALL SAVE
'254 DRAIN
'255 HOUSE RAMP
'256 RIGHT SCOOP RAMP
'257 TOP LOOP
'258 RIGHT LOOP
'259 BEHIND RIGHT SPINNER
'260 BEHIND CENTER SPINNER
'261 RIGHT RAMP DONE
'262 LEFT RAMP DONE
'263 SHAKER 1SEC
'264 RIGHT SPINNER
'265 LEFT SPINNER
'266 CENTER SPINNER
'267 SCOOP HIT -LIGHTNING -SHAKER 1SEC
'268 DELIVER PRESENT
'269 BUMPER JACKoLANTERN
'270 TARGET001
'271 TARGET002
'272 TARGET003
'273 TARGET004 LEFT CROSS
'274 TARGET005 RIGHT CROSS
'275 TARGET006 LOOP TARGET
'276 LIGHTNING FLASHER

'*************************************************************************


'///////// PuP Pack Section /////////
Dim usePUP: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!
Dim cPuPPack : cPuPPack= cGameName		' name of the PuP-Pack / PuPVideos folder for this table
usePUP = true					' enable Pinup Player functions for this table   
'******** Initialize Pinup Player *********
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

'******** PuP Event trigger processing  *********
' -- How to use PUPEvent to trigger / control a PuP-Pack :
' Usage: pupevent(EventNum)
' EventNum = PuP Exxx trigger from the PuP-Pack
' Example: pupevent 102
' This will trigger E102 from the table's PuP-Pack
' DO NOT use any Exxx triggers already used for DOF (if used) to avoid issues
'
Sub pupevent(EventNum)
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

'************ PuP-Pack Startup **************
PuPStart(cPuPPack) 'Check for PuP - If found, then start Pinup Player / PuP-Pack
