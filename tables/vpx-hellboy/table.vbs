' ****************************************************************
'                     HELLBOY
'                  By: ZandysArcade
'            for VISUAL PINBALL X 10.8
'         Uses FlexDMD for cabinet / FS mode
'            script by jpsalas/ZandysArcade
' ****************************************************************

Option Explicit
Randomize

'==================== PLayer Options ==================================

' Music volume:
Const SongVolume = 0.2 ' volume is from 0 to 1

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
'or keep it False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = True

'==================== End of Player Options ===========================



'*******************************************
'  ZOPT: User Options
'*******************************************

Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		' Level of ramp rolling volume. Value between 0 and 1

Const BallSize = 50				  'Ball diameter in VPX units; must be 50
Const BallMass = 1				  'Ball mass must be 1
Const tnob = 19					  'Total number of balls the table can hold
Const lob = 2					  'Locked balls
Const cGameName = "HELLBOY"		  'used for DOF
Const myVersion = "1.0"
Const MaxPlayers = 4           ' from 1 to 4
Const MaxMultiplier = 2        ' limit playfield multiplier
Const MaxBonusMultiplier = 10  'limit Bonus multiplier
Dim BallsPerGame         ' usually 3 or 5
Const MaxMultiballs = 7        ' max number of balls during multiballs

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

Dim tablewidth
tablewidth = Table1.width
Dim tableheight
tableheight = Table1.height
Dim BIP							 'Balls in play
BIP = 0
Dim BIPL							'Ball in plunger lane
BIPL = False

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
    table1.BloomStrength = 0.5 'default is 0.5
Else
    UseFlexDMD = True
    table1.BloomStrength = 0.5 'default is 0.5
End If

 '  UseFlexDMD = True

Dim pQRlocation
pQRlocation 					= 2     ' 2 for backglass, 5 for FullDMD
Const     ScorbitUploadLog		= 1 	' Store local log and upload after the game is over 
'Dim     ScorbitUploadLog' 		= 0 	' Store local log and upload after the game is over 
'/////////////////////////////////////////////////////////////////////
Dim bOnTheFirstBallScorbit
Dim GameModeStrTmp

'dmdType
Const pDMDTypeLCD=0
Const pDMDTypeReal=1
Const pDMDTypeFULL=2

'Dim PuPlayer
dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pBGCurPage: pBGCurPage=0
Dim pInAttract : pInAttract=false   'pAttract mode


'****** PuP Variables ******
Const HasPuP = True   'dont set to false as it will break pup

Dim usePUP: Dim cPuPPack:  Dim PUPStatus: PUPStatus=false ' dont edit this line!!!
Dim PuPlayer

'*************************** PuP Settings for this table ********************************

usePUP   = false               ' enable Pinup Player functions for this table
cPuPPack = "HELLBOY"    ' name of the PuP-Pack / PuPVideos folder for this table

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup 
'************ PuP-Pack Startup **************

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
		
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
			'Dbg "Starting Pup Pack"
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub
'----- VR Room Auto-Detect -----
Dim VRRoom, VR_Obj, VRMode
Const VRTest = 0				' 1 = Testing VR in Live View, 0 = Do not force VR mode.


If RenderingMode = 2 or Table1.ShowFSS = True or VRTest = 1 Then
	VRMode = True
    UseFlexDMD = True
	lrail.Visible = False
	rrail.Visible = False
	Ramp1.Visible = False
	Ramp2.Visible = False
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 1 : Next
	'For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 1 : Next
	'If VRRoomChoice = 1 Then
	'	For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 1 : Next
	'Else
	'	For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 1 : Next
	'	For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 0 : Next
	'End If
Else
	VRMode = False
	lrail.Visible = True
	rrail.Visible = True
	Ramp1.Visible = True
	Ramp2.Visible = True
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 0 : Next
	'For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VRMegaRoom : VR_Obj.Visible = 0 : Next
End If

' Define Global Variables
Dim RemoveTrustPost
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
Dim bSkillShotSelect
Dim bExtraBallWonThisBall
Dim bJackpot

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs
Dim cbRight

'*******************************************
'Directb2s Effects
'*******************************************

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


'*******************************************
'	ZTIM: Timers
'*******************************************

'The FrameTimer interval should be -1, so executes at the display frame rate
'The frame timer should be used to update anything visual, like some animations, shadows, etc.
'However, a lot of animations will be handled in their respective _animate subroutines.

Dim FrameTime, InitFrameTime
InitFrameTime = 0

FrameTimer.Interval = -1
Sub FrameTimer_Timer() 'The frame timer interval should be -1, so executes at the display frame rate
	FrameTime = GameTime - InitFrameTime
	InitFrameTime = GameTime	'Count frametime
	'Add animation stuff here
	RollingUpdate   		'update rolling sounds
	DoDTAnim				'handle drop target animations
	'DoSTAnim				'handle stand up target animations
	'BSUpdate
End Sub

'The CorTimer interval should be 10. It's sole purpose is to update the Cor calculations
CorTimer.Interval = 10
Sub CorTimer_Timer(): Cor.Update: End Sub

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    Playsound "vo_Welcome"
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
        .InitExitSnd SoundFXDOF("fx_kicker", 181, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 181, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    Set cbRight = New cvpmCaptiveBall
    With cbRight
        .InitCaptive CapTrigger1, CapWall1, Array(CapKicker1, CapKicker1a), 0
        .NailedBalls = 1
        .ForceTrans = .9
        .MinForce = 3.5
        .CreateEvents "cbRight"
        .Start
    End With
    CapKicker1.CreateSizedBallWithMass BallSize / 2, BallMass

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Credits = 0
    Loadhs

	' Turn off the bumper lights
	FlBumperFadeTarget(1) = 0
	FlBumperFadeTarget(2) = 0
	FlBumperFadeTarget(3) = 0

    ' Initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = True 'we do not want coins

	bOnTheFirstBallScorbit = False
	
	if usePUP = True Then
		PuPStart(cPuPPack)  ' Start Pup Pack
		PuPlayer.playlistplayex pQRlocation,"PuPOverlays","DefaultBG.png",0,1
	End If
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
    StartFire

    ' Load table color
    LoadLut
    ChangeGIIntensity 1 'default is 1
	if usePUP Then PUPInit

	' Load Scorbit
	initScorbit
End Sub

'==================================================================================================================================
' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts

' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings

Dim LeftOutlaneDifficulty,RightOutlaneDifficulty,nBallsPerGame
Sub Table1_OptionEvent(ByVal eventId)

	'Balls Per Game
	nBallsPerGame = Table1.Option("Balls Per Game", 0, 2, 1, 0, 0, Array("3 (Default)", "4", "5"))
	if bGameInPlay = False Then SetBallsPerGame nBallsPerGame

	'Difficulty
	RemoveTrustPost = Table1.Option("Remove Trust Post", 0, 1, 1, 1, 0, Array("False", "True (Default)"))
	CheckTrustPost 

	'Outlane Difficulty
	LeftOutlaneDifficulty = Table1.Option("Left Outlane Difficulty", 0, 2, 1, 1, 0, Array("Easy", "Medium (Default)", "Hard"))
	UpdateLeftOutlanePosts LeftOutlaneDifficulty

	RightOutlaneDifficulty = Table1.Option("Right Outlane Difficulty", 0, 2, 1, 1, 0, Array("Easy", "Medium (Default)", "Hard"))
	UpdateRightOutlanePosts RightOutlaneDifficulty

	If eventId = 3 Then initScorbit
End Sub

'**********************************
' 	ZMAT: General Math Functions
'**********************************
' These get used throughout the script. 

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

Function ArcCos(x)
	If x = 1 Then
		ArcCos = 0/180*PI
	ElseIf x = -1 Then
		ArcCos = 180/180*PI
	Else
		ArcCos = Atn(-x/Sqr(-x * x + 1)) + 2 * Atn(1)
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

' Used for drop targets
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy) 'Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
	Dim AB, BC, CD, DA
	AB = (bx * py) - (by * px) - (ax * py) + (ay * px) + (ax * by) - (ay * bx)
	BC = (cx * py) - (cy * px) - (bx * py) + (by * px) + (bx * cy) - (by * cx)
	CD = (dx * py) - (dy * px) - (cx * py) + (cy * px) + (cx * dy) - (cy * dx)
	DA = (ax * py) - (ay * px) - (dx * py) + (dy * px) + (dx * ay) - (dy * ax)
	
	If (AB <= 0 And BC <= 0 And CD <= 0 And DA <= 0) Or (AB >= 0 And BC >= 0 And CD >= 0 And DA >= 0) Then
		InRect = True
	Else
		InRect = False
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
	Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
	Dim rotxy
	rotxy = RotPoint(ax,ay,angle)
	rax = rotxy(0) + px
	ray = rotxy(1) + py
	rotxy = RotPoint(bx,by,angle)
	rbx = rotxy(0) + px
	rby = rotxy(1) + py
	rotxy = RotPoint(cx,cy,angle)
	rcx = rotxy(0) + px
	rcy = rotxy(1) + py
	rotxy = RotPoint(dx,dy,angle)
	rdx = rotxy(0) + px
	rdy = rotxy(1) + py
	
	InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
	Dim rx, ry
	rx = x * dCos(angle) - y * dSin(angle)
	ry = x * dSin(angle) + y * dCos(angle)
	RotPoint = Array(rx,ry)
End Function

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

	If keycode = LeftFlipperKey Then
		PinCab_Flipper_Button_Left.X = PinCab_Flipper_Button_Left.X + 8
	End If

	If keycode = RightFlipperKey Then
		PinCab_Flipper_Button_Right.X = PinCab_Flipper_Button_Right.X - 8
	End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    If bSkillShotSelect Then
        If keycode = LeftFlipperKey Then SkillshotType = 2:UpdateSkillShot
        If keycode = RightFlipperKey Then SkillshotType = 3:UpdateSkillShot
    End If

    If keycode = LeftTiltKey Then
		Nudge 90, 1
		SoundNudgeLeft
	End If
	If keycode = RightTiltKey Then
		Nudge 270, 1
		SoundNudgeRight
	End If
	If keycode = CenterTiltKey Then
		Nudge 0, 1
		SoundNudgeCenter
	End If
	If keycode = MechanicalTilt Then
		SoundNudgeCenter() 'Send the Tilting command to the ROM (usually by pulsing a Switch), or run the tilting code for an orginal table
	End If

    If keycode = LeftMagnaSave Then bLutActive = True:SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If
	If keycode = AddCreditKey Or keycode = AddCreditKey2 Then
		Select Case Int(Rnd * 3)
			Case 0
				PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
                DOF 103, DOFPulse
			Case 1
				PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
                DOF 103, DOFPulse
			Case 2
				PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
                DOF 103, DOFPulse
		End Select
	End If
    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        'if bFreePlay = False Then DOF 125, DOFOn
        If(Tilted = False) Then
            DMDFlush
            DMD "", CL("CREDITS " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
            DOF 103, DOFPulse
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

	If Keycode = AddCreditKey And BallsOnPlayfield < 2 And bBallInPlungerLane Then
		if RuleCardScreen.Visible Then 
			RuleCardScreen.Visible = 0
		Else
			RuleCardScreen.Visible = 1
		End If
	End if

    If keycode = PlungerKey Then
        Plunger.Pullback
        SoundPlungerPull
		TimerVRPlunger.Enabled = True
		TimerVRPlunger2.Enabled = False
    End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then CheckTilt 'only check the tilt during game
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper True:InstantInfoTimer.Enabled = True:RotateNunLeft:RotateKnivesLeft
        DOF 101, DOFPulse
        If keycode = RightFlipperKey Then SolRFlipper True:InstantInfoTimer.Enabled = True:RotateNunRight:RotateKnivesRight
        DOF 102, DOFPulse 
        If keycode = StartGameKey Then
			SoundStartButton
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                        If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, ""
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
				SoundStartButton
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
                        DMDFlush
                        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, ""
                        DOF 104, DOFPulse 
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)
'test Keys
End Sub

Sub Table1_KeyUp(ByVal keycode)

	If keycode = LeftFlipperKey Then
		PinCab_Flipper_Button_Left.X = PinCab_Flipper_Button_Left.X - 8
	End If

	If keycode = RightFlipperKey Then
		PinCab_Flipper_Button_Right.X = PinCab_Flipper_Button_Right.X + 8
	End If

    If hsbModeActive Then
        Exit Sub
    End If

    If bSkillShotSelect Then
        If keycode = LeftFlipperKey Then SkillshotType = 1:UpdateSkillShot
        If keycode = RightFlipperKey Then SkillshotType = 1:UpdateSkillShot
    End If

    If keycode = LeftMagnaSave Then bLutActive = False:HideLUT

	If KeyCode = PlungerKey Then
		Plunger.Fire
		TimerVRPlunger.Enabled = False
		TimerVRPlunger2.Enabled = True
		PinCab_Shooter.Y = 0
		If BIPL = 1 Then
			SoundPlungerReleaseBall()   'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall() 'Plunger release sound when there is no ball in shooter lane
		End If
	End If
    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
        DOF 147, DOFpulse
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper False
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper False
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

	If Scorbit.SessionActive = True Then Scorbit.StopSession2 Score(1), Score(2), Score(3), Score(4), PlayersPlayingGame, true 'Cancel an ongoing session
End Sub

'*******************************************
'	ZFLP: Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled) 'Left flipper solenoid callback
	If Enabled Then
    DOF 101, DOFPulse
		FlipperActivate LeftFlipper, LFPress
		LF.Fire  'leftflipper.rotatetoend
		
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper
		Else
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
	Else
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled) 'Right flipper solenoid callback
	If Enabled Then
    DOF 102, DOFPulse
		FlipperActivate RightFlipper, RFPress
		RF.Fire 'rightflipper.rotatetoend
		
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		FlipperDeActivate RightFlipper, RFPress
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	RightFlipperCollide parm
End Sub

'******************************************************
'  ZANI: Misc Animations
'******************************************************

Sub LeftFlipper_Animate
	dim a: a = LeftFlipper.CurrentAngle
	'FlipperLSh.RotZ = a
	LeftFlipperTop.RotZ = a
	'Add any left flipper related animations here
End Sub

Sub RightFlipper_Animate
	dim a: a = RightFlipper.CurrentAngle
	'FlipperRSh.RotZ = a
	RightFlipperTop.RotZ = a
	'Add any right flipper related animations here
End Sub

Sub Spinner001_Animate()
baba001.RotY = Spinner002.CurrentAngle

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
    Tilt = Tilt + TiltSensitivity                  'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <= 15) Then 'show a warning
        DMD "_", CL("CAREFUL"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_careful"
    End if
    If(NOT Tilted) AND Tilt> 15 Then 'If more that 15 then TILT the table
        'display Tilt
        InstantInfoTimer.Enabled = False
        DMDFlush
        DMD CL("YOU"), CL("TILTED"), "", eNone, eNone, eNone, 3000, True, "vo_youtilted"
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
        bMultiBallMode = False
        StopMBmodes
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
        Tilted = True
		Start_Splash "zTILTED1","zTILTED1","","blink2",120,0  
	
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper001.Threshold = 100
        Bumper002.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        Tilted = False
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper001.Threshold = 1
        Bumper002.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0) Then
        bMultiBallMode = False
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        If bRestorePower Then
            vpmtimer.Addtimer 4000, "EndOfBall() '"
            LightSeqFlashers.StopPlay
            bRestorePower = False
        Else
            vpmtimer.Addtimer 2000, "EndOfBall() '"
        End If
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'*****************************************
'         Internal Music
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
    If bGameInPlay Then
        PlaySong "m_main" &Balls
    Else
        PlaySong "m_gameover"
    End If
End Sub

Sub StopSong
    StopSound Song
End Sub

'********************
' Play random sounds
'********************

Sub PlaySfx
    PlaySound "sfx" &RndNbr(12)
End Sub

'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim GiIntensity
GiIntensity = 1   'can be used for the LUT changing to increase the GI lights when the table is darker

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
    DOF 174, DOFOn
    PlaySoundAt "fx_GiOn", GiRelay 'about the center of the table
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    DOF 174, DOFOff
    PlaySoundAt "fx_GiOff", GiRelay 'about the center of the table
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

Sub GiRedOn
    DOF 175, DOFOn
    PlaySoundAt "fx_GiOn", GiRelay 'about the center of the table
    Dim bulb
    For each bulb in aGiLightsRED
        bulb.State = 1
    Next
End Sub

Sub GiRedOff
    DOF 175, DOFOff
    PlaySoundAt "fx_GiOff", GiRelay 'about the center of the table
    Dim bulb
    For each bulb in aGiLightsRED
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
        Case 7
            LightSeqFlashers.UpdateInterval = 25
            LightSeqFlashers.Play SeqBlinking, , 15, 25
            PlaySound "sfx_thunder" &RndNbr(11)
    End Select
End Sub

' Fire lamps
Dim Fire1Pos, Fire3Pos, Fire4Pos, Fire5Pos, Flames
Flames = Array("fire01", "fire02", "fire03", "fire04", "fire05", "fire06", "fire07", "fire08", "fire09", _
    "fire10", "fire11", "fire12", "fire13", "fire14", "fire15", "fire16")

Sub StartFire
    Fire1Pos = 0
    Fire3Pos = 5
    Fire4Pos = 8
    Fire5Pos = 10
   
    FireTimer.Enabled = 1
End Sub

Sub FireTimer_Timer
    'debug.print fire1pos
    Fire1.ImageA = Flames(Fire1Pos)
    Fire3.ImageA = Flames(Fire3Pos)
    Fire4.ImageA = Flames(Fire4Pos)
    Fire5.ImageA = Flames(Fire5Pos)
    Fire1Pos = (Fire1Pos + 1) MOD 16
    Fire3Pos = (Fire3Pos + 1) MOD 16
    Fire4Pos = (Fire4Pos + 1) MOD 16
    Fire5Pos = (Fire5Pos + 1) MOD 16
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
    UpdateBallInPlay

	'Start a Scorbit session
	if Table1.Option("Scorbit", 0, 1, 1, 0, 0, Array("Disabled", "Enabled")) = 1 And (Scorbit.NeedsPairing) = False Then 
		Scorbit.StartSession()
		'Dbg "Starting Scorbit Session"
		if Scorbit.SessionActive then
			GameModeStrTmp="NA{blue}:Game On"
			Scorbit.SetGameMode(GameModeStrTmp)
		End If
	End If

    ' you may wish to start some music, play a sound, do whatever at this point
    ' PlaySound "vo_start" &RndNbr(3)

    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
	bOnTheFirstBallScorbit = True
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
    bSkillShotSelect = True

'Change the music ?
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    DOF 105, DOFPulse
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1
    UpdateBallInPlay
	BIP = BIP + 1
    ' kick it out..
    RandomSoundBallRelease BallRelease
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
    If NOT bMultiBallMode Then PlaySound"BL_"&RndNbr(15)
    Dim AwardPoints, TotalBonus
    AwardPoints = 0
    TotalBonus = 0
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False
    GiOff
    ' only process any of this if the table is not tilted.
    '(the tilt recovery mechanism will handle any extra balls or end of game)

	if Scorbit.SessionActive then
		GameModeStrTmp="BL{red}:Ball "&Balls& " Lost"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
    If NOT Tilted Then
        PlaySong "m_bonus"
        'Count the bonus. This table uses several bonus
        DMD CL("BONUS"), "", "", eNone, eNone, eNone, 1000, True, ""
        AwardPoints = Switches * 300:TotalBonus = TotalBonus + AwardPoints
        DMD CL("SWITCH BONUS"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 1000, True, ""
        AwardPoints = Jumps(CurrentPlayer) * 10000:TotalBonus = TotalBonus + AwardPoints
        DMD CL("JUMP BONUS"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 1000, True, ""
        AwardPoints = Weapons(CurrentPlayer) * 50000:TotalBonus = TotalBonus + AwardPoints
        DMD CL("PANCAKE BONUS"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 1000, True, ""
        AwardPoints = HostagesRescued(CurrentPlayer) * 25000:TotalBonus = TotalBonus + AwardPoints
        DMD CL("RELICS FOUND"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 1000, True, ""
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        DMD CL("TOTAL BONUS"), CL(FormatScore(TotalBonus) ), "", eNone, eBlinkFast, eNone, 2000, True, ""
        Score(CurrentPlayer) = Score(CurrentPlayer)

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 7500, "EndOfBall2 '"
    Else 'if tilted then only add a short delay and move to the 2nd part of the end of the ball
		if Scorbit.SessionActive then
			GameModeStrTmp="NA{red}:TILT"
			Scorbit.SetGameMode(GameModeStrTmp)
		End If
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
    If ExtraBallsAwards(CurrentPlayer)> 0 Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's then turn off any Extra Ball light if there was any
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, "vo_Shootagain"

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
' (or high score entry finished) AND it either ends the game or
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
        DMDScoreNow

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            Select Case CurrentPlayer
                Case 1:DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, "vo_player1"
                Case 2:DMD "", CL("PLAYER 2"), "", eNone, eNone, eNone, 1000, True, "vo_player2"
                Case 3:DMD "", CL("PLAYER 3"), "", eNone, eNone, eNone, 1000, True, "vo_player3"
                Case 4:DMD "", CL("PLAYER 4"), "", eNone, eNone, eNone, 1000, True, "vo_player4"
            End Select
        Else
            DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, ""
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    'debug.print "End Of Game"
	if Scorbit.SessionActive then
		GameModeStrTmp="NA{Red}:Game Over"
		Scorbit.SetGameMode(GameModeStrTmp)
		StopScorbit
	End If

    ' just ended your game then play the end of game tune
    PlaySound "vo_gameover"
    ChangeSong
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
	Start_Splash "zgameover1","","","gameover",200,0 

' splash gamneover thing    fixing
' move StartAttractMode to end gameover Animate
'   StartAttractMode

' you may wish to light any Game Over Light you may have
End Sub

'this calculates the ball number in play
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
	BIP = BIP - 1
    If BallsOnPlayfield> 0 Then
        BallsOnPlayfield = BallsOnPlayfield - 1
    End If

    ' pretend to knock the ball into the ball storage mech
    RandomSoundDrain Drain
	
    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball

    'if Tilted the end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then

			if Scorbit.SessionActive then
				GameModeStrTmp="NA{yellow}:Ball Saved"
				Scorbit.SetGameMode(GameModeStrTmp)
			End If

            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            ' stop the ballsaver timer during the launch ball saver time, but not during multiballs
            If NOT bMultiBallMode Then

				Start_Splash "zballsaved1","","","ballsaved",100,0  
                DMD "_", CL("BALL SAVED"), "_", eNone, eBlinkfast, eNone, 1234, True, "vo_ballsaved"
            DOF 173, DOFPulse
            'BallSaverTimerExpired_Timer 'uncomment the line if you want to stop the ballsaver
            End If
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1) Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True) then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    ' changesong
                    ' turn off any multiball specific lights
                    'ChangeGIIntensity 1
                    'ChangeGi white
                    'stop any multiball modes of this game
                    StopMBmodes
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
            DOF 172, DOFPulse

If BallsRemaining(CurrentPlayer) > 1 Then Start_Splash "zBALLLOST1","","","balllost",95,0

                ' End Mode and timers
                'ChangeGIIntensity 1
                'ChangeGi white
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

Sub swPlunger_Hit
	BIPL = True
End Sub

Sub swPlunger_UnHit
	BIPL = False
End Sub

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
    End If

	Scorbit_updateQR

    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        vpmtimer.addtimer 1500, "PlungerIM.AutoFire:DOF 120, DOFPulse:PlaySoundAt ""fx_kicker"", swPlungerRest:bAutoPlunger = False '"
		hideScorbit 'backup call to make sure all scorbit QR codes are gone
    End If
    'Start the skillshot lights & variables if any
    If bSkillShotReady Then
        DOF 171, DOFOn
        ChangeSong
        SkillshotType = 1:UpdateSkillshot()
    ' show the message to shoot the ball in case the player has fallen sleep
    ' swPlungerRest.TimerEnabled = 1
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    DOF 171, DOFOff
    DOF 176, DOFPulse
    lighteffect 6
    bBallInPlungerLane = False
	Scorbit_updateQR
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    bSkillShotSelect = False
    If bSkillShotReady Then
        ChangeSong
        ResetSkillShotTimer.Enabled = 0
        ResetSkillShotTimer.Enabled = 1
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
    End If
	bOnTheFirstBallScorbit = False
' turn off LaunchLight
' LaunchLight.State = 0
End Sub

' swPlungerRest timer to play a sound if the player has not shot the ball after a while
Sub swPlungerRest_Timer
    Dim i
    i = RndNbr(8) 'there are only 4 sounds in the table, so it will play a sound about 50% of times
    Select case i
        Case 1:PlaySound "vo_areyougoingtoplay"
        Case 2:PlaySound "vo_areyouplayingthisgame"
        Case 3:PlaySound "vo_pressthestartbutton"
        Case 4:PlaySound "vo_whatareyouwaitingfor"
    End Select
End Sub

Sub EnableBallSaver(seconds)
    ' do not start the timer if extra ball has been awarded
    If ExtraBallsAwards(CurrentPlayer)> 0 Then
        BallSaverTimerExpired.Enabled = False
        BallSaverSpeedUpTimer.Enabled = False
        LightShootAgain.State = 1
        Exit Sub
    End If
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' stop the timers
    BallSaverTimerExpired.Enabled = False
    BallSaverSpeedUpTimer.Enabled = False
    ' restart the timers
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
    If ExtraBallsAwards(CurrentPlayer)> 0 Then
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
    If Mode <> 0 Then ModeScore = ModeScore + points * PlayfieldMultiplier(CurrentPlayer)
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

    If(bMultiBallMode = True) Then
        Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
        ' DMD "_", CL("INCREASED JACKPOT"), "_", eNone, eNone, eNone, 1000, True, ""
        ' you may wish to limit the jackpot to a upper limit, ie..
        If(Jackpot(CurrentPlayer) >= 1000000) Then
            Jackpot(CurrentPlayer) = 1000000
        End if
    End if
End Sub

Sub AddSuperJackpot(points)
    If Tilted Then Exit Sub
    If(bMultiBallMode = True) Then
        SuperJackpot(CurrentPlayer) = SuperJackpot(CurrentPlayer) + points
        ' DMD "_", "INCREASED SP.JACKPOT", "_", eNone, eNone, eNone, 1000, True, ""
        ' you may wish to limit the jackpot to a upper limit, ie..
        If(SuperJackpot(CurrentPlayer) >= 9000000) Then
            SuperJackpot(CurrentPlayer) = 9000000
        End if
    End if
End Sub

Sub AddBonusMultiplier(n) 'adapted to this table
    Knives(0) = Knives(0) + 1
    Select Case Knives(0)
        Case 1:SetBonusMultiplier 2
        Case 2:SetBonusMultiplier 3
        Case 3:SetBonusMultiplier 5
        Case 4:SetBonusMultiplier 7
        Case 5:SetBonusMultiplier 8
        Case 6:SetBonusMultiplier 10
        Case Else
            AddScore 50000
            DMD "_", CL("50.000 POINTS"), "_", eNone, eBlink, eNone, 1000, True, ""
    End Select
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly

Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level
    UpdateBonusXLights(Level)
    If level> 1 Then
        DMD "_", CL("BONUS X " &Level), "_", eNone, eBlink, eNone, 2000, True, ""
        GiEffect 1

		if Scorbit.SessionActive then
			GameModeStrTmp="NA{blue}:Bonus " &Level & "X"
			Scorbit.SetGameMode(GameModeStrTmp)
		End If
    End If

End Sub

Sub UpdateBonusXLights(Level) '4 lights in this table, from 2x to 5x
    ' Update the lights
    Select Case Level
        Case 1:Light060.State = 0:Light059.State = 0:Light061.State = 0
        Case 2:Light060.State = 1:Light059.State = 0:Light061.State = 0
        Case 3:Light060.State = 0:Light059.State = 1:Light061.State = 0
        Case 5:Light060.State = 0:Light059.State = 0:Light061.State = 1
        Case 7:Light060.State = 1:Light059.State = 0:Light061.State = 1
        Case 8:Light060.State = 0:Light059.State = 1:Light061.State = 1
        Case 10:Light060.State = 1:Light059.State = 1:Light061.State = 1
    End Select
End Sub

Sub AddPlayfieldMultiplier(n)
    Dim NewPFLevel
    ' if not at the maximum level x
    if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier) then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
        SetPlayfieldMultiplier(NewPFLevel)
		Start_Splash "z2XPLAYFIELD1","z2XPLAYFIELD2","","blink2",150,0 
'        DMD "_", CL("PLAYFIELD X " &NewPFLevel), "_", eNone, eBlink, eNone, 2000, True, ""
        GiEffect 1

		if Scorbit.SessionActive then
			GameModeStrTmp="NA{blue}:Playfield " &NewPFLevel & "X"
			Scorbit.SetGameMode(GameModeStrTmp)
		End If
    Else 'if the max is already lit
        AddScore 50000
        DMD "_", CL("50.000 POINTS"), "_", eNone, eBlink, eNone, 2000, True, ""
    End if
    ' restart the PlayfieldMultiplier timer in case it was already started
    PFXTimer.Enabled = 0
    PFXTimer.Enabled = 1
    PFXTimerSpeedUp.Enabled = 0
    PFXTimerSpeedUp.Enabled = 1

End Sub

Sub PFXTimer_Timer
    DecreasePlayfieldMultiplier
End Sub

Sub PFXTimerSpeedUp_Timer 'speed up the blink light for the last 10 seconds
    Light058.BlinkInterval = 200:Light058.State = 2
    PFXTimerSpeedUp.Enabled = 0
End Sub

Sub DecreasePlayfieldMultiplier 'reduces by 1 the playfield multiplier, this will stop the timer as this table only has a 2x multiplier
    Dim NewPFLevel

	Nun(1) = 0:Light050.State = 0
	Nun(2) = 0:Light051.State = 0
	Nun(3) = 0:Light052.State = 0

    ' if not at 1 already
    if(PlayfieldMultiplier(CurrentPlayer)> 1) then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) - 1
        SetPlayfieldMultiplier(NewPFLevel)
        PFXTimer.Enabled = 0
  '      PFXTimer.Enabled = 1
        PFXTimerSpeedUp.Enabled = 0
   '     PFXTimerSpeedUp.Enabled = 1
		if Scorbit.SessionActive then
			GameModeStrTmp="NA{greeen}:Playfield " &NewPFLevel & "X"
			Scorbit.SetGameMode(GameModeStrTmp)
		End If
    Else
        PFXTimer.Enabled = 0
        PFXTimerSpeedUp.Enabled = 0
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
        Case 1:Light058.State = 0
        Case 2:Light058.BlinkInterval = 400:Light058.State = 2
    End Select
' perhaps show also the multiplier in the DMD?
End Sub

Sub ExtraBallIsLit
    If Light048.State = 0 Then
		Start_Splash "zExtraballislit1","zExtraballislit2","","blink2eblit",160,0  
        DMD "_", CL("EXTRA BALL IS LIT"), "", eNone, eNone, eNone, 1500, True, "vo_extraballislit"
        Light048.State = 1
        XtraBalisLit(CurrentPlayer) = 1
    End If
End Sub

Sub AwardExtraBall()
      If NOT bExtraBallWonThisBall Then 'uncomment this If in case you want to give just one extra ball per ball
    DMD "_", CL(" "), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("vo_extraball", 122, DOFPulse, DOFKnocker)
	Start_Splash "zEXTRABALL1","zEXTRABALL2","","blink2",120,0  
 

    DOF 121, DOFPulse
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
    ApronDMDUpdate
    bExtraBallWonThisBall = True
    LightShootAgain.State = 1 'light the shoot again lamp
    GiEffect 3
    LightEffect 2

	if Scorbit.SessionActive then
		GameModeStrTmp="EB{yellow}:Extraball Awarded"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

   END If
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
    DMDFlush
'	If UseFlexDMD Then
'		Start_Splash "zJackpot1","","","blinktextJackpot",160,0
'		delayscoring = FormatScore(Jackpot(CurrentPlayer) )
'
'	Else
''		Start_Splash "zJackpot1","","","blinktextJackpot",120,0
		DMD CL(" "), CL(FormatScore(Jackpot(CurrentPlayer) ) ), "zJackpot1", eNone, eBlinkFast, eNone, 1500, True, "vo_Jackpot"
		DMD CL(" "), CL(" "), "d_border", eNone, eBlinkFast, eNone, 200, True, ""

'	End If

    DOF 126, DOFPulse

    AddScore Jackpot(CurrentPlayer)
    LightEffect 2
    GiEffect 3
    ' modes handling
    Select Case Mode
        Case 4 'Dracula MB 'after 5 jackpots turn on the super jackpot light
            JackpotCount = JackpotCount + 1
            If JackpotCount >= 5 Then
                Light077.State = 2
            End If

    End Select

	if Scorbit.SessionActive then
		GameModeStrTmp="NA{yellow}:Jackpot Awarded"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

End Sub

Sub AwardSuperJackpot()
    DMDflush
    SuperJackpot(CurrentPlayer) = Jackpot(CurrentPlayer) * JackpotCount '250.000 or more

    DMD CL(" "), CL(FormatScore(SuperJackpot(CurrentPlayer) ) ), "zSUPERJACKPOT", eNone, eBlinkFast, eNone, 2000, True, "vo_superjackpot"
	DMD CL(" "), " ", "d_border", eNone, eBlinkFast, eNone, 300, True, ""
    DOF 126, DOFPulse
    AddScore SuperJackpot(CurrentPlayer)
    LightEffect 2
    GiEffect 3

	if Scorbit.SessionActive then
		GameModeStrTmp="NA{purple}:Super Jackpot Awarded"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

End Sub

Sub AwardSkillshot(points)
    ResetSkillShotTimer_Timer
    'show dmd animation
	Start_Splash "zSKILLSHOT","zSKILLSHOT2","","blink2",90,0 
    DMD CL(" "), CL(FormatScore(points) ), "d_border", eNone, eBlinkFast, eNone, 3300, True, "vo_skillshot"
'    DMD CL("SKILLSHOT"), CL(FormatScore(points) ), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_skillshot"
    DOF 127, DOFPulse
    AddScore points
    'do some light show
    GiEffect 3
    LightEffect 2

	if Scorbit.SessionActive then
		GameModeStrTmp="NA:Skillshot Awarded"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

End Sub

Sub AwardSuperSkillshot(points)
    ResetSkillShotTimer_Timer
    'show dmd animation
	Start_Splash "zSUPERSKILLSHOT","zSUPERSKILLSHOT2","","blink2",90,0 
    DMD CL(" "), CL(FormatScore(points) ), "d_border", eNone, eBlinkFast, eNone, 3300, True, "vo_superskillshot"
'    DMD CL("SUPER SKILLSHOT"), CL(FormatScore(points) ), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_superskillshot"
    DOF 127, DOFPulse
    AddScore points
    'do some light show
    GiEffect 3
    LightEffect 2

	if Scorbit.SessionActive then
		GameModeStrTmp="NA:Super Skillshot Awarded"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

End Sub

'**************
'   COMBOS
'**************

Sub AwardCombo
    DOF 128, DOFPulse 'Combo
    ComboCount = ComboCount + 1
    Select Case ComboCount
        Case 1:DMD CL("COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_combo"
        Start_Splash "zCOMBO1","zCOMBO2","","blink2",90,0
        Case 2:DMD CL("2X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 2) ), "", eNone, eNone, eNone, 1500, True, "vo_doublecombo"
        Start_Splash "z2XCOMBO1","z2XCOMBO2","","blink2",90,0
        Case 3:DMD CL("3X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 3) ), "", eNone, eNone, eNone, 1500, True, "vo_triplecombo"
        Start_Splash "z3XCOMBO1","z3XCOMBO2","","blink2",90,0
        Case 4:DMD CL("4X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 4) ), "", eNone, eNone, eNone, 1500, True, "vo_supercombo"
        Start_Splash "z4XCOMBO1","z4XCOMBO2","","blink2",90,0
        Case 5:DMD CL("5X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 5) ), "", eNone, eNone, eNone, 1500, True, "vo_supercombo"
        Start_Splash "z5XCOMBO1","z5XCOMBO2","","blink2",90,0
        Case Else:DMD CL("SUPER COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount) ), "", eNone, eNone, eNone, 1500, True, "vo_supercombo"
        Start_Splash "zSUPERCOMBO1","zSUPERCOMBO2","","blink2",90,0
    End Select
    AddScore ComboValue(CurrentPlayer) * ComboCount
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Dim MyTable
MyTable = "hellboy"

Sub Loadhs
    Dim x
    x = LoadValue(MyTable, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 100000 End If
    x = LoadValue(MyTable, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(MyTable, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 100000 End If
    x = LoadValue(MyTable, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(MyTable, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 100000 End If
    x = LoadValue(MyTable, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(MyTable, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 100000 End If
    x = LoadValue(MyTable, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(MyTable, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(MyTable, "TotalGamesPlayed")
    If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
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

    If tmp> HighScore(0) Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
    'DOF 125, DOFOn
    End If

    If tmp> HighScore(3) Then
        PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        vpmTimer.AddTimer 2000, "PlaySound ""vo_taunt"" &RndNbr(9) '"
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    Dim tmp
    tmp = RndNbr(3)
    Select Case tmp
        Case 1:PlaySound "vo_nicescore"
        Case 2:PlaySound "vo_enterinitials"
        Case 3:playSound "vo_excellentscore"
    End Select
    hsbModeActive = True
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
        playsound "sfx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "sfx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then
            playsound "sfx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "sfx_Esc"
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
    dLine(0) = ExpandLine(TempTopStr)
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
    Savehs
End Sub

'************************************
'       LUT - Darkness control
' 10 normal level & 10 warmer levels
'************************************

Dim bLutActive, LUTImage

Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1) MOD 38:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

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
        Case 22:table1.ColorGradeImage = "Fleep Natural Dark 1"
        Case 23:table1.ColorGradeImage = "Fleep Natural Dark 2"
        Case 24:table1.ColorGradeImage = "Fleep Warm Dark"
        Case 25:table1.ColorGradeImage = "Fleep Warm Bright"
        Case 26:table1.ColorGradeImage = "Fleep Warm Vivid Soft"
        Case 27:table1.ColorGradeImage = "Fleep Warm Vivid Hard"
        Case 28:table1.ColorGradeImage = "Skitso Natural and Balanced"
        Case 29:table1.ColorGradeImage = "Skitso Natural High Contrast"
        Case 30:table1.ColorGradeImage = "3rdaxis Referenced THX Standard"
        Case 31:table1.ColorGradeImage = "CalleV Punchy Brightness and Contrast"
        Case 32:table1.ColorGradeImage = "HauntFreaks Desaturated"
        Case 33:table1.ColorGradeImage = "Tomate Washed Out"
        Case 34:table1.ColorGradeImage = "VPW Original 1 to 1"
        Case 35:table1.ColorGradeImage = "Bassgeige"
        Case 36:table1.ColorGradeImage = "Blacklight"
        Case 37:table1.ColorGradeImage = "B&W Comic Book"
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
    String = CL2(String)
    For xFor = 1 to 40
        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
    SetLUTLine ""
    LUBack.imagea = "PostitBL"
End Sub

Function CL2(NumString) 'center line
    Dim Temp, TempStr
    If Len(NumString)> 40 Then NumString = Left(NumString, 40)
    Temp = (40 - Len(NumString) ) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL2 = TempStr
End Function

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

Dim FontHugeRED
Dim FlexDim
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
FlexDim = 256

				Set FontHugeRED		= FlexDMD.NewFont("FlexDMD.Resources.udmd-f12by24.fnt", RGB(255, 33, 11), RGB(255, 33, 11), 0)

                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_border")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
				digit047.visible = False

                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + i * 12, 6, 12, 22
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + (i - 20) * 12, 34, 12, 22
                Next
' new
                DMDScene.AddActor FlexDMD.NewImage("Front", "VPX.d_empty")
                DMDScene.GetImage("Front").SetSize FlexDMD.Width, FlexDMD.Height
                DMDScene.AddActor FlexDMD.NewImage("Front2", "VPX.d_empty")
                DMDScene.GetImage("Front2").SetSize 50, FlexDMD.Height
				DMDScene.GetImage("Front2").visible = False
                DMDScene.AddActor FlexDMD.NewImage("Front3", "VPX.d_empty")
				DMDScene.GetImage("Front3").setposition 206, 0
                DMDScene.GetImage("Front3").SetSize 50, FlexDMD.Height
				DMDScene.GetImage("Front3").visible = False

				DMDScene.AddActor FlexDMD.NewLabel("Text1", FontHugeRED, " ")
				DMDScene.GetLabel("Text1").Visible = False

	'			DMDScene.GetImage("Front").visible = False
' until here

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
FlexDim = 128
				Set FontHugeRED		= FlexDMD.NewFont("FlexDMD.Resources.udmd-f6by12.fnt", RGB(255, 33, 11), RGB(255, 33, 11), 0)

                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_border")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
				digit047.visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3, 6, 11
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + (i - 20) * 6, 17, 6, 11
                Next
' new
                DMDScene.AddActor FlexDMD.NewImage("Front", "VPX.d_border")
                DMDScene.GetImage("Front").SetSize FlexDMD.Width, FlexDMD.Height
                DMDScene.AddActor FlexDMD.NewImage("Front2", "VPX.d_empty")
                DMDScene.GetImage("Front2").SetSize 25, FlexDMD.Height
				DMDScene.GetImage("Front2").visible = False
                DMDScene.AddActor FlexDMD.NewImage("Front3", "VPX.d_empty")
				DMDScene.GetImage("Front3").setposition 103, 0
                DMDScene.GetImage("Front3").SetSize 25, FlexDMD.Height
				DMDScene.GetImage("Front3").visible = False

				DMDScene.AddActor FlexDMD.NewLabel("Text1", FontHugeRED, " ")
				DMDScene.GetLabel("Text1").Visible = False

	'			DMDScene.GetImage("Front").visible = False

' until here
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
        tmp2 = "d_border"
        'info on the second line: tmp1
        If bRestorePowerReady OR bEscapeHWReady Then tmp1 = "  SHOOT THE SCOOP"
        Select Case Mode
            Case 0 'no Mode active
            Case 1:tmp1 = "   RESTORE DAYLIGHT"
            Case 2:tmp1 = " STOP THE APOCALYPSE"
            Case 3:tmp1 = "ABE SAPIEN MULTIBALL"
            Case 4:tmp1 = " HELLBOY MULTIBALL"
        End Select
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 10, True, ""
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
    DMDEffectTimer.Interval = deSpeed
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
                        If i = 2 then
                            Temp = ""
                        End If
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
                        If i = 2 then
                            Temp = ""
                        End If
                    End If
                case eLongScrollLeft:
                    Temp = Right(dLine(i), 19)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
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

Function ExpandLine(TempStr)
    If TempStr = "" Then
        TempStr = Space(20)
    Else
        if Len(TempStr)> Space(20) Then
            TempStr = Left(TempStr, Space(20) )
        Else
            if(Len(TempStr) <20) Then
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
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1) ) + 128) & right(NumString, Len(NumString) - i)
        end if
    Next
    FormatScore = NumString
End function

Function FL(NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    If Len(NumString1) + Len(NumString2) <20 Then
        Temp = 20 - Len(NumString1) - Len(NumString2)
        TempStr = NumString1 & Space(Temp) & NumString2
        FL = TempStr
    End If
End Function

Function CL(NumString) 'center line
    Dim Temp, TempStr
    If Len(NumString)> 20 Then NumString = Left(NumString, 20)
    Temp = (20 - Len(NumString) ) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(NumString) 'right line
    Dim Temp, TempStr
    If Len(NumString)> 20 Then NumString = Left(NumString, 20)
    Temp = 20 - Len(NumString)
    TempStr = Space(Temp) & NumString
    RL = TempStr
End Function

'**************
' Update DMD
'**************
Dim updateskulls : updateskulls = True
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
			If dLine(2) <> "d_border" Then
				updateskulls = False
				Digits(40).ImageA = dLine(2)
				If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
			Else
				updateskulls = True
			End If

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
        digit041, digit042, digit043, digit044, digit045, digit046)
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
    Chars(42) = "d_star"  '*
    Chars(43) = ""        '+
    Chars(44) = "d_comma" ',
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
    Chars(94) = ""        '^
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

'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates
Dim DMD_Frame
Dim DMD_Splash
Dim DMD_Splash_image1
Dim DMD_Splash_image2
Dim DMD_Splash_image3
Dim DMD_Splash_mode
Dim DMD_Splash_timer
Dim DMD_Splash_delay
' Start_Splash "zHBMultiball0","","","multiball1",2222,1200  ' 1200 delaytime
' Start_Splash "zHBMultiball17","zHBMultiball18","","blink2",120,0
' Start_Splash "ztezCAUGHTBABAst1","","","image",120,0   ' ( only 1 Images )
' Start_Splash "zCAUGHTBABA","zCAUGHTBABA2","","blink2",120,0   ' 
' Start_Splash "zASMultiball0","","","multiball2",2222,0
Sub Start_Splash ( img1, img2 , img3 , mode , time , delay)
	DMD_Splash_image1 = img1
	DMD_Splash_image2 = img2
	DMD_Splash_image3 = img3
	DMD_Splash_mode = mode
	DMD_Splash_timer = time
	DMD_Splash_delay = delay
	DMD_Splash = 1
End Sub

Dim delayscoring
Dim Showhead1 : Showhead1 = 2
Sub ShowSplash
	dim tmp,tmp2
	If DMD_Splash_delay > 0 Then
		DMD_Splash_delay = DMD_Splash_delay - Realtime.interval
		Exit Sub
	End If

	If UseFlexDMD Then
		DMDScene.GetImage("Front2").visible = False
	Else
		digit047.visible = True
	End If


	DMD_Splash = DMD_Splash + 1

	If DMD_Splash = 2 Then
		For x = 0 to 40 : Digits(x).visible = False : Next
		If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX." & DMD_Splash_image1 & "&dmd=2").Bitmap
		If UseFlexDMD Then DMDScene.GetImage("Front").visible = True
        digit047.ImageA = DMD_Splash_image1
	End If

	If DMD_Splash > DMD_Splash_timer Then
		If UseFlexDMD Then
			DMDScene.GetImage("Front").visible = False
			DMDScene.GetImage("Front2").visible = False
			DMDScene.GetImage("Front3").visible = False
			DMDScene.GetLabel("Text1").visible = False
		End If
		digit047.ImageA = "d_bkempty"
		digit047.visible = False
		digit048.visible = False
		digit049.visible = False

		If not UseFlexDMD Then For x = 0 to 40 : Digits(x).visible = True : Next
		If DMD_Splash_mode = "startapoc" Then DMD CL("SHOOT THE JACKPOTS"), CL("AND THE SPINNERS"), "_", eNone, eNone, eNone, 2500, True, ""
		If DMD_Splash_mode = "gameover" Then StartAttractMode : bGameInPLay = False
		If DMD_Splash_mode = "blink2restore" Then 
			DMD CL("COMPLETE"), CL("THE 11 LIGHTS"), "_", eNone, eNone, eNone, 2500, True, ""   ' movedlast2
			DMD CL("YOU HAVE"), CL("2 MINUTES"), "_", eNone, eNone, eNone, 2500, True, ""
		End If
		DMD_Splash_mode = ""
		DMD_Splash = 0
		Showhead1 = 2

		Exit Sub

	End If

		Select Case DMD_Splash_mode

'	Start_Splash "zJackpot1","","","blinktextJackpot",160,0 : delayscoring = 2000000
			Case "blinktextJackpot"   ' just for flex
			If DMD_Splash mod 40 = 18 Then
				If UseFlexDMD Then
					DMDScene.GetLabel("Text1").Text = delayscoring
					DMDScene.GetLabel("Text1").SetAlignedPosition FlexDim/2,FlexDim/4*2/3, 4
					DMDScene.GetLabel("Text1").visible = True	
				End If
'				digit047.ImageA = DMD_Splash_image2
			End If
			If DMD_Splash mod 40 = 38 Then
				If UseFlexDMD Then DMDScene.GetLabel("Text1").visible = False
'				digit047.ImageA = DMD_Splash_image1
			End If
'                FlexDMD.Width = 256
'                FlexDMD.Height = 64
'SetAlignedPosition FlexSizeX/2,FlexSizey/2+FlexBorderOffsetY+1, FlexDMD_Align_Center


			Case "blink2restore"
			If DMD_Splash mod 40 = 18 Then
				If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX." & DMD_Splash_image2 & "&dmd=2").Bitmap
				digit047.ImageA = DMD_Splash_image2
			End If
			If DMD_Splash mod 40 = 38 Then
				If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX." & DMD_Splash_image1 & "&dmd=2").Bitmap
				digit047.ImageA = DMD_Splash_image1
			End If

			Case "gameover"
			tmp = 5
			tmp2 = int(rnd(1)*8) +1
			If DMD_Splash Mod 10 < 8 Then
				If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zGameover" & tmp2 & "&dmd=2").Bitmap	else digit047.ImageA = "zGameover" & tmp2
			Else
				If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zGameover9&dmd=2").Bitmap	else digit047.ImageA = "zGameover9"			
			End If

			If DMD_Splash Mod tmp = 4 Then Showhead1 = Showhead1 + 1 : If Showhead1 > 12 Then Showhead1 = 1

			If UseFlexDMD Then
				DMDScene.GetImage("Front3").Bitmap = FlexDMD.NewImage("", "VPX.zGameoverh" & Showhead1 & "&dmd=2").Bitmap
				DMDScene.GetImage("Front3").visible = True
			Else
				digit049.ImageA = "zGameoverh" & Showhead1 
				digit049.visible = True
			End If
'Start_Splash "zGameover1","","","gameover",200,0 
           
             Case "balllost"
			tmp = 6
			tmp2 = int(rnd(1)*8) +1
			If DMD_Splash Mod 10 < 7 Then
				If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zBALLLOST" & tmp2 & "&dmd=2").Bitmap	else digit047.ImageA = "zBALLLOST" & tmp2
			Else
				If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zBALLLOST9&dmd=2").Bitmap	else digit047.ImageA = "zBALLLOST9"			
			End If

			If DMD_Splash Mod tmp = 4 Then Showhead1 = Showhead1 + 1 : If Showhead1 > 12 Then Showhead1 = 1

			If UseFlexDMD Then
				DMDScene.GetImage("Front3").Bitmap = FlexDMD.NewImage("", "VPX.zBALLLOSTh" & Showhead1 & "&dmd=2").Bitmap
				DMDScene.GetImage("Front3").visible = True
			Else
				digit049.ImageA = "zBALLLOSTh" & Showhead1 
				digit049.visible = True
			End If
'Start_Splash "zBALLLOST1","","","balllost",200,0 


			Case "ballsaved"
			tmp = 5
			tmp2 = int(rnd(1)*9) +1
			If DMD_Splash Mod 10 < 7 Then
				If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zballsaved" & tmp2 & "&dmd=2").Bitmap	else digit047.ImageA = "zballsaved" & tmp2
			Else
				If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zballsaved10&dmd=2").Bitmap	else digit047.ImageA = "zballsaved10"			
			End If

			If DMD_Splash Mod tmp = 4 Then Showhead1 = Showhead1 + 1 : If Showhead1 > 11 Then Showhead1 = 2

			If UseFlexDMD Then
				DMDScene.GetImage("Front2").Bitmap = FlexDMD.NewImage("", "VPX.zballsavedh" & Showhead1 & "&dmd=2").Bitmap
				DMDScene.GetImage("Front2").visible = True
			Else
				digit048.ImageA = "zballsavedh" & Showhead1 
				digit048.visible = True
			End If

'digit048
'zballsavedh1
'Start_Splash "zballsaved1","","","ballsaved",160,0  

		Case "blink2eblit"

		If DMD_Splash > 90 Then
			If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zextraballislit3&dmd=2").Bitmap
			digit047.ImageA = "zextraballislit3"
		Else
			If DMD_Splash mod 20 = 2 Then
				If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX." & DMD_Splash_image2 & "&dmd=2").Bitmap
				digit047.ImageA = DMD_Splash_image2
			End If
			If DMD_Splash mod 20 = 12 Then
				If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX." & DMD_Splash_image1 & "&dmd=2").Bitmap
				digit047.ImageA = DMD_Splash_image1
			End If
		End If

		Case "blink2","startapoc"
			If DMD_Splash mod 50 = 23 Then
				If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX." & DMD_Splash_image2 & "&dmd=2").Bitmap
				digit047.ImageA = DMD_Splash_image2
			End If
			If DMD_Splash mod 50 = 48 Then
				If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX." & DMD_Splash_image1 & "&dmd=2").Bitmap
				digit047.ImageA = DMD_Splash_image1
			End If


		Case "multiball2"
			tmp = 3
			Select Case DMD_Splash
				Case 2+tmp		: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball0" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball0"
				Case 2+tmp*2	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball1" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball1"
				Case 2+tmp*3	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball2" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball2"
				Case 2+tmp*4	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball3" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball3"
				Case 2+tmp*5	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball4" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball4"
				Case 2+tmp*6	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball5" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball5"
				Case 2+tmp*7	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball6" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball6"
				Case 2+tmp*8	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball7" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball7"
				Case 2+tmp*9	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball8" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball8"
				Case 2+tmp*10	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball9" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball9"
				Case 2+tmp*11	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball10" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball10"
				Case 2+tmp*12	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball11" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball11"
				Case 2+tmp*13	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball12" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball12"
				Case 2+tmp*14	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball13" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball13"
				Case 2+tmp*15	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball14" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball14"
				Case 2+tmp*16	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball15" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball15"
				Case 2+tmp*17	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball16" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball16"
				Case 2+tmp*18	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball17" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball17"
				Case 2+tmp*19	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball18" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball18"
				Case 2+tmp*20	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball19" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball19"

				Case  6+tmp*18,6+tmp*24,6+tmp*30,6+tmp*36,6+tmp*42
								  If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball20" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball20"
				Case  6+tmp*21,6+tmp*27,6+tmp*33,6+tmp*39,6+tmp*45
								  If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zASMultiball21" & "&dmd=2").Bitmap	else digit047.ImageA = "zASMultiball21"
				Case 2+tmp*48	: If UseFlexDMD Then DMDScene.GetImage("Front").visible = False
								  DMD_Splash = 0 : digit047.ImageA = "d_bkempty"
								  If not UseFlexDMD Then For x = 0 to 40 : Digits(x).visible = True : Next
								  digit047.visible = False
								  Exit Sub
			End Select
		Case "multiball1"

			tmp = 3
			Select Case DMD_Splash
				Case 2+tmp		: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball1" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball1"
				Case 2+tmp*2	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball2" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball2"
				Case 2+tmp*3	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball3" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball3"
				Case 2+tmp*4	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball4" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball4"
				Case 2+tmp*5	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball5" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball5"
				Case 2+tmp*6	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball6" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball6"
				Case 2+tmp*7	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball7" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball7"
				Case 2+tmp*8	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball8" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball8"
				Case 2+tmp*9	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball9" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball9"
				Case 2+tmp*10	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball10" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball10"
				Case 2+tmp*11	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball11" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball11"
				Case 2+tmp*12	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball12" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball12"
				Case 2+tmp*13	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball13" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball13"
				Case 2+tmp*14	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball14" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball14"
				Case 2+tmp*15	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball15" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball15"
				Case 2+tmp*16	: If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball16" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball16"

				Case  2+tmp*18,2+tmp*24,2+tmp*30,2+tmp*36,2+tmp*42
								  If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball17" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball17"
				Case  2+tmp*21,2+tmp*27,2+tmp*33,2+tmp*39,2+tmp*45
								  If UseFlexDMD Then DMDScene.GetImage("Front").Bitmap = FlexDMD.NewImage("", "VPX.zHBMultiball18" & "&dmd=2").Bitmap	else digit047.ImageA = "zHBMultiball18"
				Case 2+tmp*48	: If UseFlexDMD Then DMDScene.GetImage("Front").visible = False
								  DMD_Splash = 0 : digit047.ImageA = "d_bkempty"
								  If not UseFlexDMD Then For x = 0 to 40 : Digits(x).visible = True : Next
								  digit047.visible = False
								  DMD CL("SHOOT 5 JACKPOTS"), "AND THE SUPERJACKPOT", "_", eNone, eNone, eNone, 2500, True, ""
								  Exit Sub
		End Select
	End Select



End Sub

Dim spinnerblack
Dim bumperlight
Sub Realtime_Timer
	dim tmp,tmp2
	If useFlexDMD then FlexDMD.LockRenderThread

	If spinnerblack > 0 Then
		spinnerblack = spinnerblack - 1
		If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX.d_border0&dmd=2").Bitmap
		Digits(40).ImageA = "d_border0"
	End If
	If bumperlight > 0 Then
		bumperlight = bumperlight - 1
		If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX.d_border11&dmd=2").Bitmap
		Digits(40).ImageA = "d_border11"
	End If

	If updateskulls And DMD_Frame Mod 10 = 5 Then
		Digits(40).ImageA = "d_border"
		If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX.d_border&dmd=2").Bitmap
	End If
	If updateskulls And DMD_Frame Mod 10 = 2 Then
		tmp = int(rnd(1)*15)
		tmp2 = "d_border"
		Select Case tmp
			Case 0 : tmp2 = "d_border"

			Case 1,2,3,4,5,6,7,8,9,10 : tmp2 = "d_border" & tmp
			Case else : tmp2 = "d_border"
		End Select
		Digits(40).ImageA = tmp2
		If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & tmp2 & "&dmd=2").Bitmap
	End If


	DMD_Frame = DMD_Frame + 1 
	If DMD_Splash > 0 Then ShowSplash

If useFlexDMD then FlexDMD.UnlockRenderThread

p050on.blenddisablelighting = Light050.getinplayintensity * 40
p050off.blenddisablelighting = Light050.getinplayintensity + 1
p051on.blenddisablelighting = Light051.getinplayintensity * 40
p051off.blenddisablelighting = Light051.getinplayintensity + 1
p052on.blenddisablelighting = Light052.getinplayintensity * 40
p052off.blenddisablelighting = Light052.getinplayintensity + 1
p049on.blenddisablelighting = Light049.getinplayintensity * 40
p049off.blenddisablelighting = Light049.getinplayintensity + 1
p080on.blenddisablelighting = Light080.getinplayintensity * 40
p080off.blenddisablelighting = Light080.getinplayintensity + 1
p068on.blenddisablelighting = Light068.getinplayintensity * 40
p068off.blenddisablelighting = Light068.getinplayintensity + 1
p073on.blenddisablelighting = Light073.getinplayintensity * 40
p073off.blenddisablelighting = Light073.getinplayintensity + 1
p076on.blenddisablelighting = Light076.getinplayintensity * 40
p076off.blenddisablelighting = Light076.getinplayintensity + 1
p083on.blenddisablelighting = Light083.getinplayintensity * 40
p083off.blenddisablelighting = Light083.getinplayintensity + 1
p077on.blenddisablelighting = Light077.getinplayintensity * 40
p077off.blenddisablelighting = Light077.getinplayintensity + 1
p082on.blenddisablelighting = Light082.getinplayintensity * 40
p082off.blenddisablelighting = Light082.getinplayintensity + 1
p070on.blenddisablelighting = Light070.getinplayintensity * 40
p070off.blenddisablelighting = Light070.getinplayintensity + 1
p071on.blenddisablelighting = Light071.getinplayintensity * 40
p071off.blenddisablelighting = Light071.getinplayintensity + 1
p081on.blenddisablelighting = Light081.getinplayintensity * 40
p081off.blenddisablelighting = Light081.getinplayintensity + 1
p069on.blenddisablelighting = Light069.getinplayintensity * 40
p069off.blenddisablelighting = Light069.getinplayintensity + 1
p072on.blenddisablelighting = Light072.getinplayintensity * 40
p072off.blenddisablelighting = Light072.getinplayintensity + 1
p001on.blenddisablelighting = Light001.getinplayintensity * 40
p001off.blenddisablelighting = Light001.getinplayintensity + 1
p079on.blenddisablelighting = Light079.getinplayintensity * 40
p079off.blenddisablelighting = Light079.getinplayintensity + 1
p002on.blenddisablelighting = Light002.getinplayintensity * 40
p002off.blenddisablelighting = Light002.getinplayintensity + 1
p067on.blenddisablelighting = Light067.getinplayintensity * 40
p067off.blenddisablelighting = Light067.getinplayintensity + 1
p074on.blenddisablelighting = Light074.getinplayintensity * 40
p074off.blenddisablelighting = Light074.getinplayintensity + 1
p078on.blenddisablelighting = Light078.getinplayintensity * 40
p078off.blenddisablelighting = Light078.getinplayintensity + 1
p066on.blenddisablelighting = Light066.getinplayintensity * 40
p066off.blenddisablelighting = Light066.getinplayintensity + 1
p075on.blenddisablelighting = Light075.getinplayintensity * 40
p075off.blenddisablelighting = Light075.getinplayintensity + 1
p048on.blenddisablelighting = Light048.getinplayintensity * 40
p048off.blenddisablelighting = Light048.getinplayintensity + 1
p047on.blenddisablelighting = Light047.getinplayintensity * 40
p047off.blenddisablelighting = Light047.getinplayintensity + 1
p046on.blenddisablelighting = Light046.getinplayintensity * 40
p046off.blenddisablelighting = Light046.getinplayintensity + 1
p085on.blenddisablelighting = Light085.getinplayintensity * 40
p085off.blenddisablelighting = Light085.getinplayintensity + 1
p062on.blenddisablelighting = Light062.getinplayintensity * 40
p062off.blenddisablelighting = Light062.getinplayintensity + 1
p044on.blenddisablelighting = Light044.getinplayintensity * 40
p044off.blenddisablelighting = Light044.getinplayintensity + 1
p045on.blenddisablelighting = Light045.getinplayintensity * 40
p045off.blenddisablelighting = Light045.getinplayintensity + 1
p060on.blenddisablelighting = Light060.getinplayintensity * 40
p060off.blenddisablelighting = Light060.getinplayintensity + 1
p059on.blenddisablelighting = Light059.getinplayintensity * 40
p059off.blenddisablelighting = Light059.getinplayintensity + 1
p061on.blenddisablelighting = Light061.getinplayintensity * 40
p061off.blenddisablelighting = Light061.getinplayintensity + 1
p058on.blenddisablelighting = Light058.getinplayintensity * 40
p058off.blenddisablelighting = Light058.getinplayintensity + 1
p065on.blenddisablelighting = Light065.getinplayintensity * 40
p065off.blenddisablelighting = Light065.getinplayintensity + 1
p064on.blenddisablelighting = Light064.getinplayintensity * 40
p064off.blenddisablelighting = Light064.getinplayintensity + 1
p063on.blenddisablelighting = Light063.getinplayintensity * 40
p063off.blenddisablelighting = Light063.getinplayintensity + 1

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
        If Credits> 0 Then
            DMD CL("CREDITS " & Credits), CL("PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "", "         ", "d_jpsalas", eNone, eNone, eNone, 3000, False, ""
' add images copy ^^ line and change the image name to new ones
    DMD CL("HELLBOY"), CL("ROM VERSION " &myversion), "", eNone, eNone, eNone, 4000, False, ""
    DMD CL("HIGHSCORES"), Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL("HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL("HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 1000, False, ""
    DMD "     HOW TO PLAY    ", "                    ", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD " HIT MAIN SHOTS TO ", " CAPTURE 5 VILLAINS", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD " CAPTURE 2 VILLAINS  ", "LIGHT EXTRA BALL ", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD " CAPTURE 5 VILLAINS ", "START RESTORE LIGHT ", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD " HIT RASPUTIN TO ", "  START HURRY UP  ", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD "HIT TOMBS FOR", " HELLBOY MULTIBALL ", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD " FIND EGGS FOR  ", "ABE SAPIEN MULTIBALL", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD " HIT BUMPERS TO ", " BUILD ROGER VALUE ", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD " HIT ROGER TARGET  ", "     TO COLLECT     ", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD " HIT KROENEN  ", "FIND HIDDEN RELICS", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD "COLLECT SKULLS", "FOR BONUS MULTIPLIER", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD "  COMPLETE LIZ  ", " FOR DOUBLE SCORING ", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD "  COLLECT RELICS  ", "TO STOP THE APOCALYPSE", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD "  SPELL DEMON TO  ", "  COLLECT PANCAKES   ", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD " PANCAKES INCREASE  ", "SCORE IN MULTIBALLS ", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD "", "", "", eScrollLeft, eScrollLeft, eNone, 2000, False, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 1000, False, ""
End Sub

Sub StartAttractMode
    DOF 170, DOFOn
    DOF 174, DOFOff
    StartLightSeq
    DMDFlush
    ShowTableInfo
    PlaySong "m_gameover"
End Sub

Sub StopAttractMode
    DOF 170, DOFOff
    DOF 174, DOFOn
    DMDScoreNow
    LightSeqAttract.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 40
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 6000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 15
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 15
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 15
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 15
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 15
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 15
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 15
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 15
    LightSeqAttract.Play SeqLeftOn, 50, 1
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
End Sub

' tables variables and Mode init
Dim BalloonsLeft(4)
Dim HostagesLeft(4)
Dim HostagesRescued(4)
Dim HostagesLights(4, 5) '5 Lights
Dim ChaosLights(4, 5)    '5 lights
Dim KillerHits(4, 5)     '5 killers
Dim KillersCompleted(4)
Dim SkillshotType
Dim Nun(3)             'top lanes
Dim Knives(4)          '4 inlanes & outlanes skulls(0) contains the current bonus multiplier
Dim Weapons(4)         'pancakes collected for each player, demon awards pancakes
Dim ChuckyValue(4)     'players roger value, it is increased by the bumpers
Dim ChainSawHits(4)    'players johan hits
Dim JigSawHits(4)      'players kroenan hits
Dim DT(4, 3)           'droptargets state
Dim Switches           'nr of switches hit by each ball
Dim Jumps(4)           'total number of ramp jumps for each player
Dim AnnabelleHits(4)   'captive ball hits for each player
Dim LeatherfaceHits(4) 'johan hits
Dim RPlights(11)       'number of hits on each Power
Dim ModeScore          'points earned during the Restore daylight
Dim Mode               'Current Wizard and multiball modes
Dim JackpotLights(5)   'the state of the jackpot Lights
Dim JackpotCount       'count the jackpots to enable the super jackpot
Dim SpinnerHits(4)
Dim XtraBalisLit(4)

Dim bRestorePower
Dim bRestorePowerReady
Dim bEscapeHW
Dim bEscapeHWReady
Dim bPennywise
Dim bDracula
' Modes variables

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    ' play a welcome Sound
    ' PLaySound "Start" &RndNbr(10)
    For i = 0 to 4
        Jackpot(i) = 50000
        SuperJackpot(i) = 250000
        BalloonsLeft(i) = 25
        HostagesLeft(i) = 25
        HostagesRescued(i) = 0
        Knives(i) = 0
        Weapons(i) = 0
        ChuckyValue(i) = 1000
        ChainSawHits(i) = 0
        JigSawHits(i) = 0
        Jumps(i) = 0
        AnnabelleHits(i) = 0
        LeatherfaceHits(i) = 0
        SpinnerHits(i) = 0
        ComboHits(i) = 0
        ComboValue(i) = 100000
        KillersCompleted(i) = 0
        XtraBalisLit(i) = 0
        For j = 0 to 5
            HostagesLights(i, j) = 0
            ChaosLights(i, j) = 0
            KillerHits(i, j) = 0
            JackpotLights(j) = 0
        Next
        For j = 0 to 3
            DT(i, j) = 0
        Next
    Next
    ' set the first chaos Light for each player
    For i = 0 to 4
        ChaosLights(i, 1) = 2
    Next
    ReleaseHostage 'release 1 hostage
    SkillshotType = 1
    BallSaverTime = 20
    bRestorePower = False
    bRestorePowerReady = False
    bEscapeHW = False
    bEscapeHWReady = False
    bPennywise = False
    bDracula = False
    Mode = 0

End Sub

Sub InstantInfo
    Dim tmp
    DMD CL("INSTANT INFO"), "", "", eNone, eNone, eNone, 1000, False, ""
    'Show some info on the current Mode

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
    DMD CL("ROGER VALUE"), CL(FormatScore(ChuckyValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("JACKPOT VALUE"), CL(FormatScore(Jackpot(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SPINS 2 UP PANCAKE"), CL(250-SpinnerHits(CurrentPlayer) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("PANCAKES COLLECTED"), CL(Weapons(CurrentPlayer) ), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("JUMPS COMPLETED"), CL(Jumps(CurrentPlayer) ), "", eNone, eNone, eNone, 2000, False, ""
End Sub

Sub StopMBmodes 'stop multiball modes after loosing the last multiball
    If bEscapeHW Then StopEscapeHW
    If bPennywise Then StopPennywiseMB
    If bDracula Then StopDraculaMB

	if Scorbit.SessionActive then
		GameModeStrTmp="MB{red}:Multiball Ended"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

End Sub

Sub StopEndOfBallMode() 'this sub is called after the last ball in play is drained, reset skillshot, modes, timers
    ResetSkillShotTimer_Timer
    DecreasePlayfieldMultiplier
    If bRestorePowerReady Then
        LightSeqFlashers.StopPlay
        bRestorePowerReady = False
    End If
End Sub

Sub ResetNewBallVariables() 'reset variables and lights for a new ball or player
    Dim i
    'turn on or off the needed lights before a new ball is released
    TurnOffPlayfieldLights
    'set up the lights according to the player achievments
    UpdateLights 'chaos and hostages lights
    UpdateKillerLights
    ApronDMDUpdate
    UpdateJigSaw
    'reset NUN variables
    For i = 0 to 3
        Nun(i) = 0
    Next
    'reset knives and bonus multiplier
    For i = 0 to 4
        Knives(i) = 0
    Next
    SetBonusMultiplier 1
    If AnnabelleHits(CurrentPlayer) = 9 Then AnnabelleHits(CurrentPlayer) = 8 'you need an extra hit to activate the jackpot
    Switches = 0
    Mode = 0
    ResetDT 'reset Dracula & drop targets
    ComboCount = 0
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
    LightSeqTilt.Play SeqAlloff
    LightSeqSkillshot.StopPlay
    LightSeqSkillshot2.StopPlay
    LightSeqSkillshot3.StopPlay
    Select Case SkillshotType
        Case 1:
            LightSeqSkillshot.PLay SeqBlinking, , 50, 300
            DMD CL("HIT LIT LIGHT"), CL("FOR SKILLSHOT"), "", eNone, eNone, eNone, 3000, True, ""
        Case 2:
            LeftGate.Open = True
            LightSeqSkillshot2.PLay SeqBlinking, , 50, 300
            DMD CL("HIT THE RAMP"), CL("FOR SUPERSKILLSHOT"), "", eNone, eNone, eNone, 3000, True, ""
        Case 3:
            LeftGate.Open = True
            LightSeqSkillshot3.PLay SeqBlinking, , 50, 300
            DMD CL("HIT ABE SAPIEN"), CL("FOR SUPERSKILLSHOT"), "", eNone, eNone, eNone, 3000, True, ""
    End Select
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    bSkillShotSelect = False
    LeftGate.Open = False
    LightSeqTilt.StopPLay
    LightSeqSkillshot.StopPlay
    LightSeqSkillshot2.StopPlay
    LightSeqSkillshot3.StopPlay
    DMDScoreNow
End Sub

Sub UpdateLights 'chaos and hostages lights
    Light048.State = XtraBalisLit(CurrentPlayer)
    UpdateHLights
    UpdateCLights
End Sub

Sub UpdateHLights 'hostages lights for the current player
    'Hostages
    Light071.State = HostagesLights(CurrentPlayer, 1)
    Light072.State = HostagesLights(CurrentPlayer, 2)
    Light073.State = HostagesLights(CurrentPlayer, 3)
    Light074.State = HostagesLights(CurrentPlayer, 4)
    Light075.State = HostagesLights(CurrentPlayer, 5)
End Sub

Sub UpdateCLights 'chaos lights for the current player
    'Chaos
    Light070.State = ChaosLights(CurrentPlayer, 1)
    Light069.State = ChaosLights(CurrentPlayer, 2)
    Light068.State = ChaosLights(CurrentPlayer, 3)
    Light067.State = ChaosLights(CurrentPlayer, 4)
    Light066.State = ChaosLights(CurrentPlayer, 5)
End Sub

Sub UpdateKillerLights
    Select Case KillerHits(CurrentPlayer, 1) 'SILVERLANCE TWINS
        Case 0:Light053.State = 0:Light003.State = 0:Light004.State = 0:Light005.State = 0:Light006.State = 0:Light007.State = 0:Light008.State = 0:Light082.State = 0
        Case 1:Light053.State = 0:Light003.State = 1:Light004.State = 1:Light005.State = 0:Light006.State = 0:Light007.State = 0:Light008.State = 0:Light082.State = 0
        Case 2:Light053.State = 0:Light003.State = 1:Light004.State = 1:Light005.State = 1:Light006.State = 1:Light007.State = 0:Light008.State = 0:Light082.State = 0
        Case 3:Light053.State = 2:Light003.State = 0:Light004.State = 0:Light005.State = 0:Light006.State = 0:Light007.State = 0:Light008.State = 0:Light082.State = 0
        Case 4:Light053.State = 2:Light003.State = 1:Light004.State = 0:Light005.State = 0:Light006.State = 0:Light007.State = 0:Light008.State = 0:Light082.State = 2
        Case 5:Light053.State = 2:Light003.State = 1:Light004.State = 1:Light005.State = 0:Light006.State = 0:Light007.State = 0:Light008.State = 0:Light082.State = 2
        Case 6:Light053.State = 2:Light003.State = 1:Light004.State = 1:Light005.State = 1:Light006.State = 0:Light007.State = 0:Light008.State = 0:Light082.State = 2
        Case 7:Light053.State = 2:Light003.State = 1:Light004.State = 1:Light005.State = 1:Light006.State = 1:Light007.State = 0:Light008.State = 0:Light082.State = 2
        Case 8:Light053.State = 2:Light003.State = 1:Light004.State = 1:Light005.State = 1:Light006.State = 1:Light007.State = 1:Light008.State = 0:Light082.State = 2
        Case 9:Light053.State = 1:Light003.State = 1:Light004.State = 1:Light005.State = 1:Light006.State = 1:Light007.State = 1:Light008.State = 1:Light082.State = 0
        Case 10:KillerHits(CurrentPlayer, 1) = 9
    End Select
    Select Case KillerHits(CurrentPlayer, 2) 'KROENAN
        Case 0:Light056.State = 0:Light022.State = 0:Light023.State = 0:Light024.State = 0:Light025.State = 0:Light026.State = 0:Light021.State = 0:Light081.State = 0
        Case 1:Light056.State = 0:Light022.State = 1:Light023.State = 1:Light024.State = 0:Light025.State = 0:Light026.State = 0:Light021.State = 0:Light081.State = 0
        Case 2:Light056.State = 0:Light022.State = 1:Light023.State = 1:Light024.State = 1:Light025.State = 1:Light026.State = 0:Light021.State = 0:Light081.State = 0
        Case 3:Light056.State = 2:Light022.State = 0:Light023.State = 0:Light024.State = 0:Light025.State = 0:Light026.State = 0:Light021.State = 0:Light081.State = 0
        Case 4:Light056.State = 2:Light022.State = 1:Light023.State = 0:Light024.State = 0:Light025.State = 0:Light026.State = 0:Light021.State = 0:Light081.State = 2
        Case 5:Light056.State = 2:Light022.State = 1:Light023.State = 1:Light024.State = 0:Light025.State = 0:Light026.State = 0:Light021.State = 0:Light081.State = 2
        Case 6:Light056.State = 2:Light022.State = 1:Light023.State = 1:Light024.State = 1:Light025.State = 0:Light026.State = 0:Light021.State = 0:Light081.State = 2
        Case 7:Light056.State = 2:Light022.State = 1:Light023.State = 1:Light024.State = 1:Light025.State = 1:Light026.State = 0:Light021.State = 0:Light081.State = 2
        Case 8:Light056.State = 2:Light022.State = 1:Light023.State = 1:Light024.State = 1:Light025.State = 1:Light026.State = 1:Light021.State = 0:Light081.State = 2
        Case 9:Light056.State = 1:Light022.State = 1:Light023.State = 1:Light024.State = 1:Light025.State = 1:Light026.State = 1:Light021.State = 1:Light081.State = 0
        Case 10:KillerHits(CurrentPlayer, 2) = 9
    End Select
    Select Case KillerHits(CurrentPlayer, 3) 'RASPUTIN
        Case 0:Light057.State = 0:Light028.State = 0:Light029.State = 0:Light030.State = 0:Light031.State = 0:Light032.State = 0:Light027.State = 0:Light080.State = 0
        Case 1:Light057.State = 0:Light028.State = 1:Light029.State = 1:Light030.State = 0:Light031.State = 0:Light032.State = 0:Light027.State = 0:Light080.State = 0
        Case 2:Light057.State = 0:Light028.State = 1:Light029.State = 1:Light030.State = 1:Light031.State = 1:Light032.State = 0:Light027.State = 0:Light080.State = 0
        Case 3:Light057.State = 2:Light028.State = 0:Light029.State = 0:Light030.State = 0:Light031.State = 0:Light032.State = 0:Light027.State = 0:Light080.State = 0
        Case 4:Light057.State = 2:Light028.State = 1:Light029.State = 0:Light030.State = 0:Light031.State = 0:Light032.State = 0:Light027.State = 0:Light080.State = 2
        Case 5:Light057.State = 2:Light028.State = 1:Light029.State = 1:Light030.State = 0:Light031.State = 0:Light032.State = 0:Light027.State = 0:Light080.State = 2
        Case 6:Light057.State = 2:Light028.State = 1:Light029.State = 1:Light030.State = 1:Light031.State = 0:Light032.State = 0:Light027.State = 0:Light080.State = 2
        Case 7:Light057.State = 2:Light028.State = 1:Light029.State = 1:Light030.State = 1:Light031.State = 1:Light032.State = 0:Light027.State = 0:Light080.State = 2
        Case 8:Light057.State = 2:Light028.State = 1:Light029.State = 1:Light030.State = 1:Light031.State = 1:Light032.State = 1:Light027.State = 0:Light080.State = 2
        Case 9:Light057.State = 1:Light028.State = 1:Light029.State = 1:Light030.State = 1:Light031.State = 1:Light032.State = 1:Light027.State = 1:Light080.State = 0
        Case 10:KillerHits(CurrentPlayer, 3) = 9
    End Select
    Select Case KillerHits(CurrentPlayer, 4) 'BABA YAGA
        Case 0:Light054.State = 0:Light010.State = 0:Light011.State = 0:Light012.State = 0:Light013.State = 0:Light014.State = 0:Light009.State = 0:Light002.State = 0
        Case 1:Light054.State = 0:Light010.State = 1:Light011.State = 1:Light012.State = 0:Light013.State = 0:Light014.State = 0:Light009.State = 0:Light002.State = 0
        Case 2:Light054.State = 0:Light010.State = 1:Light011.State = 1:Light012.State = 1:Light013.State = 1:Light014.State = 0:Light009.State = 0:Light002.State = 0
        Case 3:Light054.State = 2:Light010.State = 0:Light011.State = 0:Light012.State = 0:Light013.State = 0:Light014.State = 0:Light009.State = 0:Light002.State = 0
        Case 4:Light054.State = 2:Light010.State = 1:Light011.State = 0:Light012.State = 0:Light013.State = 0:Light014.State = 0:Light009.State = 0:Light002.State = 2
        Case 5:Light054.State = 2:Light010.State = 1:Light011.State = 1:Light012.State = 0:Light013.State = 0:Light014.State = 0:Light009.State = 0:Light002.State = 2
        Case 6:Light054.State = 2:Light010.State = 1:Light011.State = 1:Light012.State = 1:Light013.State = 0:Light014.State = 0:Light009.State = 0:Light002.State = 2
        Case 7:Light054.State = 2:Light010.State = 1:Light011.State = 1:Light012.State = 1:Light013.State = 1:Light014.State = 0:Light009.State = 0:Light002.State = 2
        Case 8:Light054.State = 2:Light010.State = 1:Light011.State = 1:Light012.State = 1:Light013.State = 1:Light014.State = 1:Light009.State = 0:Light002.State = 2
        Case 9:Light054.State = 1:Light010.State = 1:Light011.State = 1:Light012.State = 1:Light013.State = 1:Light014.State = 1:Light009.State = 1:Light002.State = 0
        Case 10:KillerHits(CurrentPlayer, 4) = 9
    End Select
    Select Case KillerHits(CurrentPlayer, 5) 'OGDRU JAHAD
        Case 0:Light055.State = 0:Light016.State = 0:Light017.State = 0:Light018.State = 0:Light019.State = 0:Light020.State = 0:Light015.State = 0:Light078.State = 0
        Case 1:Light055.State = 0:Light016.State = 1:Light017.State = 1:Light018.State = 0:Light019.State = 0:Light020.State = 0:Light015.State = 0:Light078.State = 0
        Case 2:Light055.State = 0:Light016.State = 1:Light017.State = 1:Light018.State = 1:Light019.State = 1:Light020.State = 0:Light015.State = 0:Light078.State = 0
        Case 3:Light055.State = 2:Light016.State = 0:Light017.State = 0:Light018.State = 0:Light019.State = 0:Light020.State = 0:Light015.State = 0:Light078.State = 0
        Case 4:Light055.State = 2:Light016.State = 1:Light017.State = 0:Light018.State = 0:Light019.State = 0:Light020.State = 0:Light015.State = 0:Light078.State = 2
        Case 5:Light055.State = 2:Light016.State = 1:Light017.State = 1:Light018.State = 0:Light019.State = 0:Light020.State = 0:Light015.State = 0:Light078.State = 2
        Case 6:Light055.State = 2:Light016.State = 1:Light017.State = 1:Light018.State = 1:Light019.State = 0:Light020.State = 0:Light015.State = 0:Light078.State = 2
        Case 7:Light055.State = 2:Light016.State = 1:Light017.State = 1:Light018.State = 1:Light019.State = 1:Light020.State = 0:Light015.State = 0:Light078.State = 2
        Case 8:Light055.State = 2:Light016.State = 1:Light017.State = 1:Light018.State = 1:Light019.State = 1:Light020.State = 1:Light015.State = 0:Light078.State = 2
        Case 9:Light055.State = 1:Light016.State = 1:Light017.State = 1:Light018.State = 1:Light019.State = 1:Light020.State = 1:Light015.State = 1:Light078.State = 0
        Case 10:KillerHits(CurrentPlayer, 5) = 9
    End Select
End Sub

' Other animations
Sub coffinf_Animate
	coffin.Z = coffinf.CurrentAngle:coffindoor.Z = coffinf.CurrentAngle + 10
	coffin2.Z = coffinf.CurrentAngle
	coffindoor2.Z = coffinf.CurrentAngle + 10
End Sub

'Sub coffinf_Animate:FlasherlogoVP.Z = coffinf.CurrentAngle:coffindoor.Z = coffinf.CurrentAngle + 10:End Sub
Sub coffindoorf_Animate
	coffindoor.RotZ = coffindoorf.CurrentAngle
	coffindoor2.RotZ = coffindoorf.CurrentAngle
End Sub
'Sub coffinf_Animate:coffin2.Z = coffinf.CurrentAngle:coffindoor2.Z = coffinf.CurrentAngle + 10:End Sub
'Sub coffindoorf_Animate:coffindoor2.RotZ = coffindoorf.CurrentAngle:End Sub

Sub dgatef_Animate:dgate1.RotZ = - dgatef.CurrentAngle:dgate2.RotZ = dgatef.CurrentAngle:End Sub
'Sub dtf1_Animate:dt1.Z = dtf1.CurrentAngle:End Sub
'Sub dtf2_Animate:dt2.Z = dtf2.CurrentAngle:End Sub
'Sub dtf3_Animate:dt3.Z = dtf3.CurrentAngle:End Sub

' Apron digits display

Sub ApronDMDUpdate
    'apron Digits
    dim digit, tmp
    'hostages left
    tmp = HostagesLeft(CurrentPlayer)
    CStr(abs(tmp) )
    If len(tmp) = 1 then tmp = "0" &tmp
    For digit = 41 to 42
        ApronDMDDisplayChar mid(tmp, digit -40, 1), digit
    Next
    'balloons left
    tmp = Balloonsleft(CurrentPlayer)
    CStr(abs(tmp) )
    If len(tmp) = 1 then tmp = "0" &tmp
    For digit = 43 to 44
        ApronDMDDisplayChar mid(tmp, digit -42, 1), digit
    Next
    'extra balls
    tmp = ExtraBallsAwards(CurrentPlayer)
    CStr(abs(tmp) )
    ApronDMDDisplayChar mid(tmp, 1, 1), 45
End Sub

Sub ApronDMDDisplayChar(achar, adigit)
    achar = ASC(achar)
    Digits(adigit).ImageA = Chars(achar)
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

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
	LS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotLeft Lemk
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Lemk
    ShakeLeftCat
    DOF 106, DOFPulse 'DOF Solenoid/MX
    startB2S(12)
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
	RS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotRight Remk
    ShakeRightCat
    DOF 107, DOFPulse 'DOF Solenoid/MX
    startB2S(13)
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

'***********************
'        Bumpers
'***********************

Sub Bumper001_Hit
    DOF 108, DOFPulse
    If Tilted Then Exit Sub
	bumperlight = 20
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    RandomSoundBumperTop Bumper001
    FlBumperFadeTarget(1) = 1		'Flupper bumper demo
	Bumper001.timerenabled = True
    ' check for modes
    AddScore 1000
    Switches = Switches + 1
    ChuckyValue(CurrentPlayer) = INT(ChuckyValue(CurrentPlayer) + 500)
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper001"
End Sub
Sub Bumper001_timer
	FlBumperFadeTarget(1) = 0
End Sub

Sub Bumper002_Hit
    DOF 109, DOFPulse
    If Tilted Then Exit Sub
	bumperlight = 20
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    RandomSoundBumperBottom Bumper002
    FlBumperFadeTarget(2) = 1		'Flupper bumper demo
	Bumper002.timerenabled = True
    ' check for modes
    AddScore 1000
    Switches = Switches + 1
    ChuckyValue(CurrentPlayer) = INT(ChuckyValue(CurrentPlayer) + 500)
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper002"
End Sub
Sub Bumper002_timer
	FlBumperFadeTarget(2) = 0
End Sub

' prepare some global vars to dim/brighten objects when using day-night slider
Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0 : DNA45 = (NightDay-10)/20 : DNA90 = 0 : DayNightAdjust = 0.4
Else
	DNA30 = (NightDay-10)/30 : DNA45 = (NightDay-10)/45 : DNA90 = (NightDay-10)/90 : DayNightAdjust = NightDay/25
End If

Dim FlBumperFadeActual(6), FlBumperFadeTarget(6), FlBumperColor(6), FlBumperTop(6), FlBumperSmallLight(6), Flbumperbiglight(6)
Dim FlBumperDisk(6), FlBumperBase(6), FlBumperBulb(6), FlBumperscrews(6), FlBumperActive(6), FlBumperHighlight(6)
Dim cnt : For cnt = 1 to 6 : FlBumperActive(cnt) = False : Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight

FlInitBumper 1, "red"
FlInitBumper 2, "orange"


' ### uncomment the statement below to change the color for all bumpers ###
' Dim ind : For ind = 1 to 5 : FlInitBumper ind, "green" : next

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	' store all objects in an array for use in FlFadeBumper subroutine
	FlBumperFadeActual(nr) = 1 : FlBumperFadeTarget(nr) = 1.1: FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr) : FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr) : Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr) : Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr) : FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr): FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	' set the color for the two VPX lights
	Select Case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0)
			FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0)
			FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
			
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255)
			FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255)
			FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8)
			FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32)
			FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255)
			MaterialColor "bumpertopmat" & nr, RGB(16,255,16)
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			
		Case "orange"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0)
			FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8)
			FlBumperBigLight(nr).colorfull = RGB(255,190,8)
			
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190)
			FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
			
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255)
			FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4)
			FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50)
			FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			FlBumperSmallLight(nr).TransmissionScale = 0
			
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255)
			FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255)
			FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	End Select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
	'   UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
	'			   OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
	'			   float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 ) * DayNightAdjust
	
	Select Case FlBumperColor(nr)
		Case "blue"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(38 - 24 * Z,130 - 98 * Z,255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z + 0.97 * Z ^ 3)
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z ^ 3) / (0.5 + DNA90)
			
		Case "green"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(16 + 16 * Sin(Z * 3.14),255,16 + 16 * Sin(Z * 3.14)), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z ^ 3) / (1 + DNA90)
			
		Case "red"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255, 16 - 11 * Z + 16 * Sin(Z * 3.14),0), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30 ^ 2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z ^ 3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z * 4,8 - Z * 8)
			
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255, 100 - 22 * z + 16 * Sin(Z * 3.14),Z * 32), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30 ^ 2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z ^ 3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,100 + Z * 50, 0)
			
		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 8 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z ^ 3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20 * Z,255 - 65 * Z)
			FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20 * Z,255 - 65 * Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z * 36,220 - Z * 90)
			
		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 1, RGB(30 - 27 * Z ^ 0.03,30 - 28 * Z ^ 0.01, 255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z ^ 3
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z ^ 3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255 - 240 * (Z ^ 0.1),255 - 240 * (Z ^ 0.1),255)
			FlBumperSmallLight(nr).colorfull = RGB(255 - 200 * z,255 - 200 * Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255 - 190 * Z,235 - z * 180,220 + 35 * Z)
			
		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255, 180 + 40 * z, 48 * Z), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30 ^ 2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z ^ 3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)
			
		Case "purple"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(128 - 118 * Z - 32 * Sin(Z * 3.14), 32 - 26 * Z ,255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15 + 200 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z + 0.97 * Z ^ 3)
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z ^ 3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128 - 60 * Z,32,255)
	End Select
End Sub

Sub BumperTimer_Timer
	Dim nr
	For nr = 1 To 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) And FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) And FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	Next
End Sub

'*********
' Lanes
'*********

Sub leftInlaneSpeedLimit
	'Wylte's implementation
'    debug.print "Spin in: "& activeball.AngMomZ
'    debug.print "Speed in: "& activeball.vely
	if activeball.vely < 0 then exit sub 							'don't affect upwards movement
    activeball.AngMomZ = -abs(activeball.AngMomZ) * RndNum(3,6)
    If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
    If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)

    if abs(activeball.vely) > 5 then activeball.vely = 0.8 * activeball.vely
    if abs(activeball.vely) > 10 then activeball.vely = 0.8 * activeball.vely
    if abs(activeball.vely) > 15 then activeball.vely = 0.8 * activeball.vely
    if activeball.vely > 16 then activeball.vely = RndNum(14,16)
    if activeball.vely < -16 then activeball.vely = RndNum(-14,-16)
'    debug.print "Spin out: "& activeball.AngMomZ
'    debug.print "Speed out: "& activeball.vely
End Sub

Sub rightInlaneSpeedLimit
	'Wylte's implementation
'    debug.print "Spin in: "& activeball.AngMomZ
'    debug.print "Speed in: "& activeball.vely
	if activeball.vely < 0 then exit sub 							'don't affect upwards movement
    activeball.AngMomZ = abs(activeball.AngMomZ) * RndNum(2,4)
    If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
    If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)

	if abs(activeball.vely) > 5 then activeball.vely = 0.8 * activeball.vely
    if abs(activeball.vely) > 10 then activeball.vely = 0.8 * activeball.vely
    if abs(activeball.vely) > 15 then activeball.vely = 0.8 * activeball.vely
    if activeball.vely > 16 then activeball.vely = RndNum(14,16)
    if activeball.vely < -16 then activeball.vely = RndNum(-14,-16)
'    debug.print "Spin out: "& activeball.AngMomZ
'    debug.print "Speed out: "& activeball.vely
End Sub

' in and outlanes
Sub Trigger001_Hit
    DOF 110, DOFPulse
    FlashEggs 800, 100
    FlashForMs f10, 1000, 50, 0
    FlashForMs f11, 1000, 50, 0
    If Tilted Then Exit Sub
    Switches = Switches + 1
    Select Case Mode
        Case 0 'normal scoring
            Knives(1) = 1:CheckKnives
            Light044.State = Knives(1)
            Addscore 5000
        Case 1 'Restore Power
            RPlights(1) = RPlights(1) + 1
            CheckRP
            If RPlights(1) <4 Then Addscore 1000 * RPlights(1)
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger001"
End Sub

Sub Trigger002_Hit
    DOF 111, DOFPulse
    FlashEggs 800, 100
    FlashForMs f10, 1000, 50, 0
    FlashForMs f11, 1000, 50, 0
	leftInlaneSpeedLimit
    If Tilted Then Exit Sub
    Switches = Switches + 1
    Select Case Mode
        Case 0 'normal scoring
            Knives(2) = 1:CheckKnives
            Light045.State = Knives(2)
            Addscore 1000
        Case 1 'Restore Power
            RPlights(1) = RPlights(1) + 1
            CheckRP
            If RPlights(1) <4 Then Addscore 1000 * RPlights(1)
    End Select
    ' remember last trigger hit by the ball
    If LastSwitchHit <> "Trigger011" Then
        LastSwitchHit = "Trigger002"
    End If
End Sub

Sub Trigger003_Hit
    DOF 112, DOFPulse
    FlashEggs 800, 100
    FlashForMs f10, 1000, 50, 0
    FlashForMs f11, 1000, 50, 0
	rightInlaneSpeedLimit
    If Tilted Then Exit Sub
    Switches = Switches + 1
    Select Case Mode
        Case 0 'normal scoring
            Knives(3) = 1:CheckKnives
            Light046.State = Knives(3)
            Addscore 1000
        Case 1 'Restore Power
            RPlights(1) = RPlights(1) + 1
            CheckRP
            If RPlights(1) <4 Then Addscore 1000 * RPlights(1)
    End Select
    ' remember last trigger hit by the ball
    If LastSwitchHit <> "Trigger010" Then
        LastSwitchHit = "Trigger003"
    End If
End Sub

Sub Trigger004_Hit
    DOF 113, DOFPulse
    FlashEggs 800, 100
    FlashForMs f10, 1000, 50, 0
    FlashForMs f11, 1000, 50, 0
    If Tilted Then Exit Sub
    Switches = Switches + 1
    Select Case Mode
        Case 0 'normal scoring
            Knives(4) = 1:CheckKnives
            Light047.State = Knives(4)
            Addscore 5000
        Case 1 'Restore Power
            RPlights(1) = RPlights(1) + 1
            CheckRP
            If RPlights(1) <4 Then Addscore 1000 * RPlights(1)
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger004"
End Sub

Sub CheckKnives
    Dim tmp
    tmp = Knives(1) + Knives(2) + Knives(3) + Knives(4)
    If tmp = 4 Then
        AddBonusMultiplier 1
        Knives(1) = 0:Light044.State = 0
        Knives(2) = 0:Light045.State = 0
        Knives(3) = 0:Light046.State = 0
        Knives(4) = 0:Light047.State = 0
        LightEffect 2
    End If
End Sub

Sub RotateKnivesLeft
    DOF 114, DOFPulse
    Dim tmp
    tmp = Knives(1)
    Knives(1) = Knives(2)
    Knives(2) = Knives(3)
    Knives(3) = Knives(4)
    Knives(4) = tmp
    Light044.State = Knives(1)
    Light045.State = Knives(2)
    Light046.State = Knives(3)
    Light047.State = Knives(4)
End Sub

Sub RotateKnivesRight
    DOF 115, DOFPulse
    Dim tmp
    tmp = Knives(4)
    Knives(4) = Knives(3)
    Knives(3) = Knives(2)
    Knives(2) = Knives(1)
    Knives(1) = tmp
    Light044.State = Knives(1)
    Light045.State = Knives(2)
    Light046.State = Knives(3)
    Light047.State = Knives(4)
End Sub

'top lanes
Sub Trigger006_Hit 'top left
    PLaySoundAt "fx_sensor", Trigger006
    FlashEggs 800, 100
    FlashForMs f10, 1000, 50, 0
    FlashForMs f11, 1000, 50, 0
    If Tilted Then Exit Sub
    Switches = Switches + 1
    Select Case Mode
        Case 0 'normal scoring
            Nun(1) = 1:CheckNun
            DOF 116, DOFPulse 
            Light050.State = Nun(1)
            Addscore 1000
        Case 1 'Restore Power
            RPlights(10) = RPlights(10) + 1
            CheckRP
            If RPlights(10) <4 Then Addscore 1000 * RPlights(10)
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger006"
End Sub

Sub Trigger007_Hit 'top center
    PLaySoundAt "fx_sensor", Trigger007
    FlashEggs 800, 100
    FlashForMs f10, 1000, 50, 0
    FlashForMs f11, 1000, 50, 0
    If Tilted Then Exit Sub
    If bSkillshotReady AND SkillshotType = 1 Then 'award Skillshot
        AwardSkillshot 250000
        Exit Sub
    End If
    Switches = Switches + 1
    Select Case Mode
        Case 0 'normal scoring
            Nun(2) = 1:CheckNun
            DOF 117, DOFPulse 
            Light051.State = Nun(2)
            Addscore 1000
        Case 1 'Restore Power
            RPlights(10) = RPlights(10) + 1
            CheckRP
            If RPlights(10) <4 Then Addscore 1000 * RPlights(10)
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger007"
End Sub

Sub Trigger008_Hit 'top right
    PLaySoundAt "fx_sensor", Trigger008
    FlashEggs 800, 100
    FlashForMs f10, 1000, 50, 0
    FlashForMs f11, 1000, 50, 0
    If Tilted Then Exit Sub
    Switches = Switches + 1
    Select Case Mode
        Case 0 'normal scoring
            Nun(3) = 1:CheckNun
            DOF 118, DOFPulse 
            Light052.State = Nun(3)
            Addscore 1000
        Case 1 'Restore Power
            RPlights(10) = RPlights(10) + 1
            CheckRP
            If RPlights(10) <4 Then Addscore 1000 * RPlights(10)
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger008"
End Sub

Sub CheckNuN
    Dim tmp
    tmp = Nun(1) + Nun(2) + Nun(3)
    If tmp = 3 Then
        DOF 180, DOFPulse
		If PlayfieldMultiplier(CurrentPlayer) = 2 Then
			StartLIZF
			Nun(1) = 0:Light050.State = 0
			Nun(2) = 0:Light051.State = 0
			Nun(3) = 0:Light052.State = 0
			LightEffect 2
			AddScore 50000
		Else
			AddPlayfieldMultiplier 1
			StartLIZF
			If NOT bMultiBallMode Then PlaySound"Liz_"&RndNbr(23)
			Nun(1) = 0:Light050.State = 0
			Nun(2) = 0:Light051.State = 0
			Nun(3) = 0:Light052.State = 0
			LightEffect 2
		End If
    End If
End Sub

Sub RotateNunLeft
    Dim tmp
    tmp = Nun(1)
    Nun(1) = Nun(2)
    Nun(2) = Nun(3)
    Nun(3) = tmp
    Light050.State = Nun(1)
    Light051.State = Nun(2)
    Light052.State = Nun(3)
End Sub

Sub RotateNunRight
    Dim tmp
    tmp = Nun(3)
    Nun(3) = Nun(2)
    Nun(2) = Nun(1)
    Nun(1) = tmp
    Light050.State = Nun(1)
    Light051.State = Nun(2)
    Light052.State = Nun(3)
End Sub

' 5 killer switches
Sub Trigger005_Hit 'Baba - left orbit
    PLaySoundAt "fx_sensor", Trigger005
    FlashEggs 800, 100
    FlashForMs f10, 1000, 50, 0
    FlashForMs f11, 1000, 50, 0
    If Tilted OR bSkillShotReady Then Exit Sub
    Switches = Switches + 1
    'Hostages - can be rescued on all modes
    If HostagesLights(CurrentPlayer, 1) = 2 Then 'the light is blinking, so rescue the HostagesLeft
        HostagesLights(CurrentPlayer, 1) = 0
		Start_Splash "zRELIC1","zRELIC2","","blink2",120,0

        HostagesRescued(CurrentPlayer) = HostagesRescued(CurrentPlayer) + 1
        HostagesLeft(CurrentPlayer) = HostagesLeft(CurrentPlayer) - 1
        AddScore 10000
        
        CheckHostages
    End If
    Select Case Mode
        Case 0 'normal scoring
			KillerHits(CurrentPlayer, 1) = KillerHits(CurrentPlayer, 1) + 1
			CheckKillers 1
			UpdateKillerLights

            'Chaos letter can only be collected during Mode 0: standard mode
            If ChaosLights(CurrentPlayer, 1) = 2 Then 'collect the letter and light the next letter
                PLaySound "sfx_bomb1"
                If NOT bMultiBallMode Then PlaySound"Baba_"&RndNbr(24)
                ChaosLights(CurrentPlayer, 1) = 1
                ChaosLights(CurrentPlayer, 2) = 2
                UpdateLights
                DOF 119, DOFPulse
            End If
        Case 1 'Restore Power
            RPlights(3) = RPlights(3) + 1
            CheckRP
            If RPlights(3) <4 Then Addscore 1000 * RPlights(3)
        Case 2 'Escape HW
            If JackpotLights(1) Then
                JackpotLights(1) = 0
                AwardJackpot
                SetupJackpots
            End If
        Case 4 'Dracula MB
            If JackpotLights(1) Then
                JackpotLights(1) = 0
                AwardJackpot
                SetupJackpots
            End If
             if bDracula then MBTENT2Up.enabled = True
             
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger005"
End Sub

Sub Trigger009_Hit 'Ghostface - left ramp done
    PLaySoundAt "fx_sensor", Trigger009
    StartPort
    If Tilted Then Exit Sub
    Switches = Switches + 1 'always counting
    'combo
    If LastSwitchHit = "Trigger011" OR LastSwitchHit = "Trigger010" Then
        AwardCombo
    Else
        ComboCount = 0
    End If
    'Hostages
    If HostagesLights(CurrentPlayer, 2) = 2 Then 'the light is blinking, so rescue the HostagesLeft
        HostagesLights(CurrentPlayer, 2) = 0
		Start_Splash "zRELIC1","zRELIC2","","blink2",120,0

        HostagesRescued(CurrentPlayer) = HostagesRescued(CurrentPlayer) + 1
        HostagesLeft(CurrentPlayer) = HostagesLeft(CurrentPlayer) - 1
        AddScore 10000
        CheckHostages
    End If
    Select Case Mode
        Case 0 'normal scoring
            KillerHits(CurrentPlayer, 2) = KillerHits(CurrentPlayer, 2) + 1
            CheckKillers 2
            UpdateKillerLights
            'Chaos letter can only be collected during Mode 0: standard mode
            If ChaosLights(CurrentPlayer, 2) = 2 Then 'collect the letter and light the next letter
                PLaySound "sfx_bomb1"
                If NOT bMultiBallMode Then PlaySound"Baba_"&RndNbr(24)
                ChaosLights(CurrentPlayer, 2) = 1
                ChaosLights(CurrentPlayer, 3) = 2
                UpdateLights
                DOF 122, DOFPulse
            End If
        Case 1 'Restore Power
            RPlights(4) = RPlights(4) + 1
            CheckRP
            If RPlights(4) <4 Then Addscore 1000 * RPlights(4)
        Case 2 'Escape HW
            If JackpotLights(2) Then
                JackpotLights(2) = 0
                AwardJackpot
                SetupJackpots
            End If
        Case 3 'Pennywise MB
            If JackpotLights(2) Then
                AwardJackpot
            End If
			if bPennywise then GoldenArmy1Up.enabled = True
        Case 4 'Dracula MB
            If JackpotLights(2) Then
                JackpotLights(2) = 0
                AwardJackpot
                SetupJackpots
             End If
			if bDracula then MBTENT3Up.enabled = True
            
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger009"
End Sub

Sub Trigger013_Hit 'inner loop

    PLaySoundAt "fx_sensor", Trigger013
    If NOT bMultiBallMode Then PlaySound"Roger_"&RndNbr(3)
    If Tilted Then Exit Sub
    FlashEggs 800, 100
    FlashForMs f10, 1000, 50, 0
    FlashForMs f11, 1000, 50, 0
    Switches = Switches + 1
    If LastSwitchHit = "Trigger013" Then
        AwardCombo
    Else
        ComboCount = 0
    End If
    'Hostages
    If HostagesLights(CurrentPlayer, 3) = 2 Then 'the light is blinking, so rescue the HostagesLeft
        HostagesLights(CurrentPlayer, 3) = 0

		Start_Splash "zRELIC1","zRELIC2","","blink2",120,0

        HostagesRescued(CurrentPlayer) = HostagesRescued(CurrentPlayer) + 1
        HostagesLeft(CurrentPlayer) = HostagesLeft(CurrentPlayer) - 1
        AddScore 10000
        CheckHostages
    End If
    Select Case Mode
        Case 0 'normal scoring
            KillerHits(CurrentPlayer, 3) = KillerHits(CurrentPlayer, 3) + 1
            CheckKillers 3
            UpdateKillerLights

            'Chaos letter can only be collected during Mode 0: standard mode
            If ChaosLights(CurrentPlayer, 3) = 2 Then 'collect the letter and light the next letter
                PLaySound "sfx_bomb1"
                If NOT bMultiBallMode Then PlaySound"Baba_"&RndNbr(24)
                ChaosLights(CurrentPlayer, 3) = 1
                ChaosLights(CurrentPlayer, 4) = 2
                UpdateLights
                DOF 123, DOFPulse
            End If
        Case 1 'Restore Power
            RPlights(5) = RPlights(5) + 1
            CheckRP
            If RPlights(5) <4 Then Addscore 1000 * RPlights(5)
        Case 2 'Escape HW
            If JackpotLights(3) Then
                JackpotLights(3) = 0
                AwardJackpot
                SetupJackpots
            End If
        Case 4 'Dracula MB
            If JackpotLights(3) Then
                JackpotLights(3) = 0
                AwardJackpot
                SetupJackpots
            End If
            if bDracula then MBTENT5Up.enabled = True
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger013"
End Sub

Sub Trigger011_Hit 'Pinhead - center ramp
    PLaySoundAt "fx_sensor", Trigger011
    StartGuns
    If Tilted Then Exit Sub
    If bSkillshotReady AND SkillshotType = 2 Then 'award SuperSkillshot
        AwardSuperSkillshot 500000
        Exit Sub
    End If
    Switches = Switches + 1
    If LastSwitchHit = "Trigger011" OR LastSwitchHit = "Trigger010" Then
        AwardCombo
    Else
        ComboCount = 0
    End If
    'Hostages
    If HostagesLights(CurrentPlayer, 4) = 2 Then 'the light is blinking, so rescue the HostagesLeft
        HostagesLights(CurrentPlayer, 4) = 0
		Start_Splash "zRELIC1","zRELIC2","","blink2",120,0
        HostagesRescued(CurrentPlayer) = HostagesRescued(CurrentPlayer) + 1
        HostagesLeft(CurrentPlayer) = HostagesLeft(CurrentPlayer) - 1
        AddScore 10000
        CheckHostages
    End If
    Select Case Mode
        Case 0 'normal scoring
            KillerHits(CurrentPlayer, 4) = KillerHits(CurrentPlayer, 4) + 1
            CheckKillers 4
            UpdateKillerLights
            'Chaos letter can only be collected during Mode 0: standard mode
            If ChaosLights(CurrentPlayer, 4) = 2 Then 'collect the letter and light the next letter

                PLaySound "sfx_bomb1"
                If NOT bMultiBallMode Then PlaySound"Baba_"&RndNbr(24)
                ChaosLights(CurrentPlayer, 4) = 1
                ChaosLights(CurrentPlayer, 5) = 2
                UpdateLights
                DOF 124, DOFPulse
            End If
        Case 1 'Restore Power
            RPlights(6) = RPlights(6) + 1
            CheckRP
            If RPlights(6) <4 Then Addscore 1000 * RPlights(6)
        Case 2 'Escape HW
            If JackpotLights(4) Then
                JackpotLights(4) = 0
                AwardJackpot
                SetupJackpots
            End If
        Case 3 'Pennywise MB
            If JackpotLights(4) Then
                AwardJackpot
            End If
			if bPennywise then GoldenArmy2Up.enabled = True
        Case 4 'Dracula MB
            If JackpotLights(4) Then
                JackpotLights(4) = 0
                AwardJackpot
                SetupJackpots
            End If
			if bDracula then MBTENT4Up.enabled = True
           
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger011"
End Sub

Sub Trigger012_Hit 'Michael - right orbit
    PLaySoundAt "fx_sensor", Trigger012
    If Tilted OR bSkillShotReady Then Exit Sub
    Switches = Switches + 1
    'Hostages
    If HostagesLights(CurrentPlayer, 5) = 2 Then 'the light is blinking, so rescue the HostagesLeft
        HostagesLights(CurrentPlayer, 5) = 0
		Start_Splash "zRELIC1","zRELIC2","","blink2",120,0
        HostagesRescued(CurrentPlayer) = HostagesRescued(CurrentPlayer) + 1
        HostagesLeft(CurrentPlayer) = HostagesLeft(CurrentPlayer) - 1
        AddScore 10000
        CheckHostages
    End If
    Select Case Mode
        Case 0 'normal scoring
            KillerHits(CurrentPlayer, 5) = KillerHits(CurrentPlayer, 5) + 1
            CheckKillers 5
            UpdateKillerLights
            'Chaos letter can only be collected during Mode 0: standard mode
            If ChaosLights(CurrentPlayer, 5) = 2 Then 'collect the letter and light the next letter
				Start_Splash "zPANCAKES1","zPANCAKES2","","blink2",100,0
                PLaySound "vo_weaponsupgraded"
                If NOT bMultiBallMode Then PlaySound"Baba_"&RndNbr(24)
                Weapons(CurrentPlayer) = Weapons(CurrentPlayer) + 1
                LightSeqChaos.Play SeqRandom, 50, , 1000
                ChaosLights(CurrentPlayer, 1) = 2
                ChaosLights(CurrentPlayer, 2) = 0
                ChaosLights(CurrentPlayer, 3) = 0
                ChaosLights(CurrentPlayer, 4) = 0
                ChaosLights(CurrentPlayer, 5) = 0
                UpdateLights
                DOF 125, DOFPulse
            End If
        Case 1 'Restore Power
            RPlights(7) = RPlights(7) + 1
            CheckRP
            If RPlights(7) <4 Then Addscore 1000 * RPlights(7)
        Case 2 'Escape HW
            If JackpotLights(5) Then
                JackpotLights(5) = 0
                AwardJackpot
				Start_Splash "zASJACKPOT1","zASJACKPOT2","","blink2",120,0

                SetupJackpots
            End If
        Case 4 'Dracula MB
            If JackpotLights(5) Then
                JackpotLights(5) = 0
                AwardJackpot
                SetupJackpots
            End If
            if bDracula then MBTENT1Up.enabled = True
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger012"
End Sub

'jump switch

Sub Trigger010_Hit 'jump ramp
    DOF 126, DOFPulse
    PLaySoundAt "fx_sensor", Trigger010
    If Tilted Then Exit Sub
    Addscore 4000
    Jumps(CurrentPlayer) = Jumps(CurrentPlayer) + 1 'only used in the bonus
    LightEffect 2
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger010"
End Sub

'***********
' Targets
'***********
Sub Target001_Hit 'left - jigsaw - 3 hits releases hostages
    DOF 127, DOFPulse
    STHit 1
    If Tilted Then Exit Sub
    Switches = Switches + 1
    Select Case Mode
        Case 0 'normal scoring
            Addscore 1000
            JigSawHits(CurrentPlayer) = JigSawHits(CurrentPlayer) + 1
            UpdateJigSaw
        Case 1 'Restore Power
            RPlights(2) = RPlights(2) + 1
            CheckRP
            If RPlights(2) <4 Then Addscore 1000 * RPlights(2)
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target001"
End Sub


Sub UpdateJigSaw '3 target Lights
    Select Case JigSawHits(CurrentPlayer)
        Case 0:Light065.State = 2:Light064.State = 2:Light063.State = 2
        Case 1:Light065.State = 1:Light064.State = 2:Light063.State = 2
        Case 2:Light065.State = 1:Light064.State = 1:Light063.State = 2
        Case 3:Light065.State = 2:Light064.State = 2:Light063.State = 2:LightEffect 2:JigSawHits(CurrentPlayer) = 0:ReleaseHostage
    End Select
End Sub

Sub ReleaseHostage 'lights one random hostage light
    Dim i, tmp
    tmp = 0
    For i = 1 to 5
        tmp = tmp + HostagesLights(CurrentPlayer, i) 'the lights are blinkning or off, so the value for each light is 2 or 0
    Next
    If tmp <10 Then                                  'there are some light/s off (state is 2 and there are 5 Lights)
        i = RndNbr(5)
        do while HostagesLights(CurrentPlayer, i) <> 0
            i = RndNbr(5)
        Loop
        HostagesLights(CurrentPlayer, i) = 2
        UpdateLights
    Else
        Addscore 10000 '10000 points if all the relic lights are lit
    End If
End Sub

Sub Target002_Hit 'top right - roger
    DOF 129, DOFPulse
    If Tilted Then Exit Sub
    If LastSwitchHit = "Target007" Then
        LightEffect 7
        AwardCombo
    Else
        ComboCount = 0
    End If
    DMD " ROGER BONUS SCORE", CL(FormatScore(ChuckyValue(CurrentPlayer) ) ), "_", eNone, eBlink, eNone, 1500, True, ""
    Addscore ChuckyValue(CurrentPlayer)
    Switches = Switches + 1
    ChuckyValue(CurrentPlayer) = 1000 'reset the Roger value
    ' check modes
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target002"
End Sub

Sub Target003_Hit ' dracula left
    DOF 130, DOFPulse
    STHit 3
    If Tilted Then Exit Sub
    Addscore 1000
    Switches = Switches + 1
    RotateHostagesLeft
    ' check modes
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target003"
End Sub

Sub Target003o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub Target004_Hit 'dracula right
    DOF 128, DOFPulse
    STHit 4
    If Tilted Then Exit Sub
    Addscore 1000
    Switches = Switches + 1
    RotateHostagesRight
    ' check modes
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target004"
End Sub

Sub Target004o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub RotateHostagesLeft 'rotate hostages lights to the left
    Dim tmp
    tmp = Light071.State
    Light071.State = Light072.State
    Light072.State = Light073.State
    Light073.State = Light074.State
    Light074.State = Light075.State
    Light075.State = tmp
End Sub

Sub RotateHostagesRight 'rotate hostages lights to the right
    Dim tmp
    tmp = Light075.State
    Light075.State = Light074.State
    Light074.State = Light073.State
    Light073.State = Light072.State
    Light072.State = Light071.State
    Light071.State = tmp
End Sub

'collect eggs at abe swamp
Sub Target005_Hit 'abe
    DOF 131, DOFPulse
    FlashEggs 800, 100
    If NOT bMultiBallMode Then PlaySound"ABE_"&RndNbr(33)
    If Tilted Then Exit Sub
    If bSkillshotReady AND SkillshotType = 3 Then 'award SuperSkillshot 2
        AwardSuperSkillshot 750000
        Exit Sub
    End If

    Switches = Switches + 1
    ' check modes
    Select Case Mode
        Case 0 'normal scoring
            FlashForMs Light084, 500, 80, 0
            Addscore 1000
            GetaBalloon
            If XtraBalisLit(CurrentPlayer) Then
                AwardExtraBall
                XtraBalisLit(CurrentPlayer) = 0
                Light048.State = 0
            End If
        Case 1 'Restore Power
            RPlights(8) = RPlights(8) + 1
            CheckRP
            If RPlights(8) <4 Then Addscore 1000 * RPlights(8)
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target005"
End Sub

Sub Rubberband007_Hit 'abe rubber
    DOF 132, DOFPulse
    If Tilted Then Exit Sub
    Switches = Switches + 1
    ' check modes
    Select Case Mode
        Case 0 'normal scoring
            FlashForMs Light084, 500, 80, 0
            Addscore 1000
            GetaBalloon
    End Select
End Sub

Sub Rubberband005_Hit 'pennywise rubber
    DOF 133, DOFPulse
    If Tilted Then Exit Sub
    If bRestorePower Then Exit Sub
    Switches = Switches + 1
    ' check modes
    Select Case Mode
        Case 0 'normal scoring
            FlashForMs Light084, 500, 80, 0
            Addscore 1000
            GetaBalloon
    End Select
End Sub

Sub GetaBalloon
    BalloonsLeft(CurrentPlayer) = BalloonsLeft(CurrentPlayer) - 1
    If BalloonsLeft(CurrentPlayer) <0 Then BalloonsLeft(CurrentPlayer) = 0
    ApronDMDUpdate
    If BalloonsLeft(CurrentPlayer) = 0 Then 'Start Pennywise multiball
        StartPennywiseMB
    End If
End Sub

Sub Target006_Timer : Target006.timerenabled = False : End Sub

Sub Target006_Hit 'Rasputin - captive ball target
    If Target006.timerenabled = True Then Exit Sub
    Target006. timerenabled = True
    DOF 134, DOFPulse
    STHit 6
    FlashGloveWeapon 500, 100
    StartIron
    StartCharge
    If Tilted Then Exit Sub
    Switches = Switches + 1
    ' check modes
    Select Case Mode
        Case 0 'normal scoring
            AnnabelleHits(CurrentPlayer) = AnnabelleHits(CurrentPlayer) + 1
            Addscore 1000 * AnnabelleHits(CurrentPlayer)
            Select Case AnnabelleHits(CurrentPlayer)
                Case 1
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("  ASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("  ASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("  ASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                Case 2
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("   SPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("   SPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("   SPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                Case 3
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("    PUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("    PUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("    PUTIN"), "", eNone, eNone, eNone, 200, True, ""
                Case 4
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("     UTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("     UTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("     UTIN"), "", eNone, eNone, eNone, 200, True, ""
                Case 5
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("      TIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("      TIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("      TIN"), "", eNone, eNone, eNone, 200, True, ""
                Case 6
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("       IN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("       IN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("       IN"), "", eNone, eNone, eNone, 200, True, ""
                Case 7
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("        N"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("        N"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("        N"), "", eNone, eNone, eNone, 200, True, ""
                Case 8
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("         "), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("         "), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("RASPUTIN"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("         "), "", eNone, eNone, eNone, 200, True, ""
                
                    DMD " RASPUTIN JACKPOT", CL("IS READY"), "", eNone, eNone, eNone, 2000, True, "vo_annabellebonusisready"
                    Light079.BlinkInterval = 400
                    Light079.State = 2
                    Capkicker1.TimerEnabled = 1  '15 seconds
                    Capkicker1a.TimerEnabled = 1 '10 seconds
                Case 9
					Start_Splash "zRASJACKPOT","zRASJACKPOT2","","blink2",100,0
                    DMD CL(" "), CL("100.000"), "", eNone, eBlink, eNone, 4000, True, "vo_jackpot"
'                    DMD CL("RASPUTIN JACKPOT"), CL("100.000"), "", eNone, eBlink, eNone, 2000, True, "vo_jackpot"
                    If NOT bMultiBallMode Then PlaySound"Ras_"&RndNbr(18)
                    Addscore 150000
                    Light079.State = 0
                    LightEffect 3
                    AnnabelleHits(CurrentPlayer) = 0
                    Capkicker1.TimerEnabled = 0
                    Capkicker1a.TimerEnabled = 0
                    AnnabelleHits(CurrentPlayer) = 0
            End Select
        Case 1 'Restore Power
            RPlights(11) = RPlights(11) + 1
            CheckRP
            If RPlights(11) <4 Then Addscore 1000 * RPlights(11)
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target006"

End Sub

Sub Capkicker1_Timer 'turn off the light and stop the hurry-up
    Capkicker1.TimerEnabled = 0
    Light079.State = 0
    AnnabelleHits(CurrentPlayer) = 0
End Sub

Sub Capkicker1a_Timer 'speed up the light
    Capkicker1a.TimerEnabled = 0
    Light079.State = 0
    Light079.BlinkInterval = 150
    Light079.State = 2
End Sub

Sub Target007_Hit 'Leatherface - chainsaw
    DOF 135, DOFPulse
    If Tilted Then Exit Sub
    Switches = Switches + 1
    ' check modes
    Select Case Mode
        Case 0 'normal scoring
            LeatherfaceHits(CurrentPlayer) = LeatherfaceHits(CurrentPlayer) + 1
            Addscore 1000 * LeatherfaceHits(CurrentPlayer)
            Select Case LeatherfaceHits(CurrentPlayer)
                Case 1
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL(" OHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL(" OHANKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL(" OHANKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                Case 2
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("  HANKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("  HANKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("  HANKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                Case 3
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("   ANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("   ANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("   ANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                Case 4
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("    NNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("    NNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("    NNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                Case 5
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("     NKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("     NKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("     NKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                Case 6
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("      KRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("      KRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("      KRAUS"), "", eNone, eNone, eNone, 200, True, ""
                Case 7
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("       RAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("       RAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("       RAUS"), "", eNone, eNone, eNone, 200, True, ""
                Case 8
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("        AUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("        AUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("        AUS"), "", eNone, eNone, eNone, 200, True, ""
                Case 9
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("         US"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("         US"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("         US"), "", eNone, eNone, eNone, 200, True, ""
                Case 10
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("          S"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("          S"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("          S"), "", eNone, eNone, eNone, 200, True, ""
                Case 11
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("         "), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("         "), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("JOHANNKRAUS"), "", eNone, eNone, eNone, 200, True, ""
                    DMD "_", CL("         "), "", eNone, eNone, eNone, 200, True, ""
                    DMD "JOHANNKRAUS JACKPOT", CL("IS READY"), "", eNone, eNone, eNone, 2000, True, "vo_leatherfacejackpotisready"
                    LightEffect 7
                    Light076.BlinkInterval = 300
                    Light076.State = 2
                    Target007.TimerEnabled = 1 '20 seconds
                Case 12
                    DMDFlush

					Start_Splash "zJOHANNJACKPOT","zJOHANNJACKPOT2","","blink2",100,0
                    DMD " ", CL("75.000"), "", eNone, eBlink, eNone, 4000, True, "vo_jackpot"
  '                  DMD "JOHANNKRAUS JACKPOT", CL("75.000"), "", eNone, eBlink, eNone, 4000, True, "vo_jackpot"
                    If NOT bMultiBallMode Then PlaySound"Joh_"&RndNbr(7)
                    Addscore 75000
                    LightEffect 7
                    LightEffect 3
                    LeatherfaceHits(CurrentPlayer) = 0
                    Target007.TimerEnabled = 0
                    Light076.State = 0
                    LeatherfaceHits(CurrentPlayer) = 0
            End Select
    End Select
    ' remember last trigger hit by the ball
    LastSwitchHit = "Target007"
End Sub

Sub Target007_Timer 'turn off the light and stop the hurry-up
    Target007.TimerEnabled = 0
    Light076.State = 0
    LeatherfaceHits(CurrentPlayer) = 0
End Sub

'rubbers

Sub rlband004_Hit:AddScore 110:Switches = Switches + 1:End Sub
Sub rlband005_Hit:AddScore 110:Switches = Switches + 1:End Sub

'***********
'  Spinner
'***********

Sub Spinner001_Spin 'left

    DOF 136, DOFPulse
    If Tilted Then Exit Sub
	spinnerblack = 10

    SoundSpinner Spinner001
    Addscore 1000
    Light049.Duration 1, 100, 0
    'check modes
    Select case Mode
        Case 0 'standad Mode
            SpinnerHits(CurrentPlayer) = SpinnerHits(CurrentPlayer) + 1
            CheckSpinnerHits
        Case 2 'Escape HW
            Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + 1000
    End Select
End Sub





Sub Spinner002_Spin 'right

    DOF 137, DOFPulse
    If Tilted Then Exit Sub
	spinnerblack = 10

    SoundSpinner Spinner002
    Addscore 1000
    Light001.Duration 1, 100, 0
    'check modes
    Select case Mode
        Case 0 'standad Mode
            SpinnerHits(CurrentPlayer) = SpinnerHits(CurrentPlayer) + 1
            CheckSpinnerHits
        Case 2 'Escape HW
            Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + 1000
    End Select
End Sub

Sub CheckSpinnerHits
    If SpinnerHits(CurrentPlayer) = 250 Then
        SpinnerHits(CurrentPlayer) = 0
        Weapons(CurrentPlayer) = Weapons(CurrentPlayer) + 1
        PlaySound "vo_weaponsupgraded"
		Start_Splash "zPANCAKES1","zPANCAKES2","","blink2",100,0
    End If
End Sub

'*******************
' Hellboy hole
'*******************

Sub Kicker001_Hit
    Dim delay
    delay = 1500
    SoundSaucerLock
    'Kicker001.Destroyball 'do not delete the ball, just close the door
    FlashEggs 800, 100
    FlashForMs f10, 1000, 50, 0
    FlashForMs f11, 1000, 50, 0
    coffindoorf.RotateToStart
    BallsinHole = BallsInHole + 1
    If NOT Tilted Then
        If bRestorePowerReady Then StartRestorePower2:delay = 8000
        If bEscapeHWReady Then StartEscapeHW2:delay = 8000
        Select Case Mode
            Case 0 'normal scoring
                BallsInLock(CurrentPlayer) = BallsInLock(CurrentPlayer) + 1
				if Scorbit.SessionActive then
					GameModeStrTmp="BL{blue}:Ball "&BallsInLock(CurrentPlayer)& " Locked"
					Scorbit.SetGameMode(GameModeStrTmp)
				End If


                Select Case BallsInLock(CurrentPlayer)
                    Case 1
                        DMD "", CL("BALL 1 LOCKED"), "_", eNone, eBlink, eNone, 1500, True, "vo_ball1locked":Addscore 25000:Delay = 2500
		Start_Splash "zBALL1LOCK","zBALL1LOCK2","","blink2",120,0  
                       DOF 138, DOFPulse
                    Case 2
		Start_Splash "zBALL2LOCK","zBALL2LOCK2","","blink2",120,0   
                       DMD "", CL("BALL 2 LOCKED"), "_", eNone, eBlink, eNone, 1500, True, "vo_ball2locked":Addscore 25000:Delay = 2500
                        DOF 139, DOFPulse
                    Case 3
		Start_Splash "zBALL3LOCK","zBALL3LOCK2","","blink2",120,0  
                        DMD "", CL("BALL 3 LOCKED"), "_", eNone, eBlink, eNone, 1500, True, "vo_ball3locked":Addscore 25000
                        DOF 140, DOFPulse
                        StartDraculaMBtimer.interval = 3000
                        StartDraculaMBtimer.enabled = True
                        delay = 5500

                End Select
            Case 1 'Restore Daylight
                RPlights(9) = RPlights(9) + 1
                CheckRP
                If RPlights(9) <4 Then Addscore 1000 * RPlights(9)
            Case 4 'Hellboy MB 'check for super jackpot
                If Light077.State = 2 Then
                    AwardSuperJackpot
                    Light077.State = 0
                    JackpotCount = 0
                Else
                    Addscore 1000
                End If
        End Select
        LightEffect 7
        vpmtimer.addtimer delay, "kickBallOut '"
    Else 'if Tilted kick the ball fast
        vpmtimer.addtimer 500, "kickBallOut '"
    End If
End Sub

sub StartDraculaMBtimer_timer
    StartDraculaMBtimer.enabled = False
    StartDraculaMB
End Sub 

Sub kickBallOut
    DOF 159, DOFPulse
    If BallsinHole> 0 Then
        If NOT bMultiBallMode Then PlaySound"H_"&RndNbr(38)
        BallsinHole = BallsInHole - 1
        SoundSaucerKick 1, Kicker001
        FlashHornsWeapon 2000, 100
        coffindoorf.RotateToEnd
        'Kicker001.CreateSizedBallWithMass BallSize / 2, BallMass
        vpmTimer.AddTimer 200, "Kicker001.kick 170, 22 '"
        LightEffect 5
        StartGun
    End If
End Sub

' Drop targets

Sub Tomb1_Hit 'left dt
	Dbg "Tomb 1 Hit"
    DOF 141, DOFPulse
    If Tilted Then Exit Sub
    Switches = Switches + 1
    ' check modes
    LightEffect 7
    Addscore 1000
    Light083.Duration 1, 200, 0
    FlashForMs Light084, 500, 80, 0
    DT(CurrentPlayer, 1) = 1
    UpdateDT
    ' remember last trigger hit by the ball
    LastSwitchHit = "Tomb1"
End Sub

Sub Tomb2_Hit 'center dt
	Dbg "Tomb 2 Hit"
    DOF 142, DOFPulse
    If Tilted Then Exit Sub
    Switches = Switches + 1
    ' check modes
    LightEffect 7
    Addscore 1000
    Light083.Duration 1, 200, 0
    FlashForMs Light084, 500, 80, 0
    DT(CurrentPlayer, 2) = 1
    UpdateDT
    ' remember last trigger hit by the ball
    LastSwitchHit = "Tomb2"
End Sub

Sub Tomb3_Hit 'right dt
	Dbg "Tomb 3 Hit"
    DOF 143, DOFPulse
    If Tilted Then Exit Sub
    LightEffect 7
    Addscore 1000
    Light083.Duration 1, 200, 0
    FlashForMs Light084, 500, 80, 0
    Switches = Switches + 1
    DT(CurrentPlayer, 3) = 1
    UpdateDT
    ' check modes
    ' remember last trigger hit by the ball
    LastSwitchHit = "Tomb3"
End Sub

Sub ResetDT ' all
    DT(CurrentPlayer, 1) = 0
    DT(CurrentPlayer, 2) = 0
    DT(CurrentPlayer, 3) = 0
    UpdateDT
End Sub
'    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors)
Sub UpdateDT
	Dbg "In updatedDT"
    If DT(CurrentPlayer, 1) = 1 Then
		Dbg "Tomb 1 Should be down already"
        startB2S(10)
        DTHit 1
        dtf1.RotateToEnd
    Else
		Dbg "Should be Raising Tomb 1"
        RandomSoundDropTargetReset tomb1p
		DTRaise 1
        dtf1.RotateToStart
    End If

    If DT(CurrentPlayer, 2) = 1 Then
		Dbg "Tomb 2 Should be down already"
        startB2S(11)
        DTHit 2
        dtf2.RotateToEnd
'        dgatef.RotateToEnd
'        PlaySound "sfx_tomb"
'        vpmTimer.AddTimer 200, "coffinf.RotateToEnd '"
'        vpmTimer.AddTimer 2000, "coffindoorf.RotateToEnd '"
'        draculagate.IsDropped = 1
'        Kicker001.Enabled = 1
    Else
		Dbg "Should be Raising Tomb 2"
        RandomSoundDropTargetReset  tomb2p
		DTRaise 2
        dtf2.RotateToStart
'        dgatef.RotateToStart
'        PlaySound "sfx_tomb"
'        vpmTimer.AddTimer 500, "coffinf.RotateToStart '"
'        vpmTimer.AddTimer 200, "coffindoorf.RotateToStart '"
'        draculagate.IsDropped = 0
'        Kicker001. Enabled = 0
    End If

    If DT(CurrentPlayer, 3) = 1 Then
		Dbg "Tomb 3 Should be down already"
        startB2S(10)
        DTHit 3
        dtf3.RotateToEnd
    Else
		Dbg "Should be Raising Tomb 3"
        RandomSoundDropTargetReset  tomb2p
		DTRaise 3
        dtf3.RotateToStart
    End If

	CheckDTs

	If DT(CurrentPlayer, 1) = 1 And DT(CurrentPlayer, 2) = 1 And DT(CurrentPlayer, 3) = 1 Then

Dbg "PASSED"
		dgatef.RotateToEnd
        PlaySound "sfx_tomb"

'        vpmTimer.AddTimer 200, "coffinf.RotateToEnd '"
'        vpmTimer.AddTimer 2000, "coffindoorf.RotateToEnd '"
		Coffintimercount = 1
		Coffintimer.enabled = False
		Coffintimer.interval = 200
		Coffintimer.enabled = True
 

       draculagate.IsDropped = 1
        Kicker001.Enabled = 1
	Else
        dgatef.RotateToStart
        PlaySound "sfx_tomb"

'        vpmTimer.AddTimer 500, "coffinf.RotateToStart '"
'        vpmTimer.AddTimer 200, "coffindoorf.RotateToStart '"
		Coffintimercount = 3
		Coffintimer.enabled = False
		Coffintimer.interval = 200
		Coffintimer.enabled = True

        draculagate.IsDropped = 0
        Kicker001. Enabled = 0
	End If
End Sub

Sub CheckDTs
	if DT(CurrentPlayer, 1) = 1 Then DTDrop 1
	if DT(CurrentPlayer, 2) = 1 Then DTDrop 2
	if DT(CurrentPlayer, 3) = 1 Then DTDrop 3
End Sub


dim Coffintimercount
Sub Coffintimer_Timer
	Coffintimer.enabled = False
	Select Case Coffintimercount 
		case 1 : 
			Coffintimercount = 2
			coffinf.RotateToEnd
			Coffintimer.interval = 1800 ' 1800+200 = 2000
			Coffintimer.enabled = True
		case 2 : 
			Coffintimercount = 0
			coffindoorf.RotateToEnd
		case 3 : 
			Coffintimercount = 4
			coffindoorf.RotateToStart
			Coffintimer.interval = 300
			Coffintimer.enabled = True
		case 4 :
			Coffintimercount = 0
			coffinf.RotateToStart	
	End Select
End Sub


'********************************
'   Wizard Modes & Multiballs
'********************************

Dim Counter

Sub StartCountDown(n)
    Counter = n
    CountDown.Enabled = 1
End Sub

Sub CountDown_Timer
    Counter = Counter -1
    If Counter = 0 Then Me.Enabled = 0
End Sub

'********************************
'        Restore Daylight:
'         Wizard mode
'  after 5 enemies are captured
'********************************
' this is not a multiball,
' but a 2 minutes scoring feast
' Mode 1

Sub CheckKillers(n)   
Dim STmp
                       'n is the number of the current killer hit, and it is used to score
    Select Case KillerHits(CurrentPlayer, n) 'number of hits 1 to 9
        Case 1, 2, 3:Addscore 10000
        Case 4, 5, 6:Addscore 25000:PlaySfx
        Case 7, 8:Addscore 50000:PlaySfx
        Case 9:Addscore 200000:PlaySfx ' last Hit. extra hits do not score anymore
			Select Case n
				Case 1 : Start_Splash "zCAUGHTTWINS1","zCAUGHTTWINS2","","blink2",120,0 : STmp = "Twins"
				Case 2 : Start_Splash "zCAUGHTKROENEN1","zCAUGHTKROENEN2","","blink2",120,0 : STmp = "Kroenen"
				Case 3 : Start_Splash "zCAUGHTRAS1","zCAUGHTRAS2","","blink2",120,0 : STmp = "Rasputten"
				Case 4 : Start_Splash "zCAUGHTBABA1","zCAUGHTBABA2","","blink2",120,0 : STmp = "Baba Yaga"
				Case 5 : Start_Splash "zCAUGHTOGDRU1","zCAUGHTOGDRU2","","blink2",120,0  : STmp = "OgDru"

				if Scorbit.SessionActive then
					GameModeStrTmp="NA{purple}:Caught "&STmp
					Scorbit.SetGameMode(GameModeStrTmp)
				End If

			End Select
			KillersCompleted(CurrentPlayer) = KillersCompleted(CurrentPlayer) + 1
			If KillersCompleted(CurrentPlayer) MOD 2 = 0 Then
                ExtraBallIsLit
            End If
    End Select
    dim i, tmp
    For i = 1 to 5
        tmp = tmp + KillerHits(CurrentPlayer, i)
    Next
    If tmp >= 45 AND Mode = 0 Then '9 hits for each killer = 45, then all the killers are captured so start the wizard Restore Power
        StartRestorePower
    End If
End Sub

Sub StartRestorePower
    Dim i
    For Each i in aTiltLights:i.State = 0:Next 'turn all lights off
    GiOff
    Light084.BlinkInterval = 150
    Light084.State = 2
    Light083.BlinkInterval = 150
    Light083.State = 2
    Light077.BlinkInterval = 150
    Light077.State = 2
    Light062.State = 2
    bRestorePowerReady = True
    Mode = -1 'stop all modes even the normal scoring
    'Drop the droptargets
    DT(CurrentPlayer, 1) = 1
    DT(CurrentPlayer, 2) = 1
    DT(CurrentPlayer, 3) = 1
    UpdateDT
    LightSeqFlashers.UpdateInterval = 100
    LightSeqFlashers.Play SeqBlinking, , 150, 250
    DMDFlush
    DMD CL("RESTORE DAYLIGHT"), CL("IS READY"), "_", eNone, eNone, eNone, 2500, True, "vo_restorepower"
    DMD CL("SHOOT THE SCOOP"), CL("TO START"), "_", eNone, eNone, eNone, 2500, True, ""
End Sub

Sub StartRestorePower2
    dim i
    PlaySong "m_RestorePower"
'zRestore1
'zRestore2
	Start_Splash "zRestore1","zRestore2","","blink2restore",160,0 

'    DMD CL("STARTING"), CL("RESTORE DAYLIGHT"), "_", eNone, eNone, eNone, 2500, True, ""
'    DMD CL("COMPLETE"), CL("THE 11 LIGHTS"), "_", eNone, eNone, eNone, 2500, True, ""   ' movedlast2
 '   DMD CL("YOU HAVE"), CL("2 MINUTES"), "_", eNone, eNone, eNone, 2500, True, ""
    LightSeqFlashers.StopPlay
    bRestorePowerReady = False
    bRestorePower = True
    Mode = 1
    'setup lights - all lights off
    For Each i in aTiltLights:i.State = 0:Next
    'init the hit array
    For i = 0 to 11:RPlights(i) = 0:Next
    UpdateRPLights
    GiOff
    GiRedOn
    ModeScore = 0
    'Start the timers
    EnableBallSaver 120
    StartCountDown 120
    RestorePowerTimer.Enabled = 1  '120 seconds
    RestorePowerTimer2.Enabled = 1 '15 seconds to reduce the power on all powerlines that are not completed.
	if Scorbit.SessionActive then
		GameModeStrTmp="MB{blue}:Restore Daylight Started"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

End Sub

Sub CheckRP
    Dim i, tmp
    tmp = 0
    UpdateRPLights
    For i = 1 to 11
        tmp = tmp + RPlights(i)
    Next
    If tmp >= 55 Then '11 power lines, value 5 is completed, all the completed so...
        WinRestorePower
    End If
End Sub

Sub RestorePowerTimer_Timer
    Me.Enabled = 0
    StopRestorePower
End Sub

Sub RestorePowerTimer2_Timer
    Dim i
    For i = 1 to 11 'check the power lines, and if they are not online (value 5) then reduce them
        If RPlights(i)> 0 AND RPlights(i) <5 Then
            RPlights(i) = RPlights(i) - 1
        End If
    Next
    UpdateRPLights
End Sub

Sub StopRestorePower
    Dim i
    For i = 1 to 5 'reset the killers hits count
        KillerHits(CurrentPlayer, i) = 0
    Next
    'stop the timers
    BallSaverTimerExpired_Timer 'stop the ball saver
    RestorePowerTimer.Enabled = 0
    RestorePowerTimer2.Enabled = 0
    CountDown.Enabled = 0
    For each i in aTiltLights:i.BlinkInterval = 400:Next
    For each i in aHostagesLights:i.BlinkInterval = 350:Next
    DisableTable True
    TiltRecoveryTimer.Enabled = True 'this will check for all the balls being drained and it will continue the game.
    GiOn
    GiRedOff
    Mode = 0
    DMDFlush
    DMD "RESTORE DAYLIGHT SCORE", CL(FormatScore(ModeScore) ), "_", eNone, eNone, eNone, 3000, True, ""
    DMD CL("PLEASE WAIT"), CL("COLLECTING BALLS"), "_", eNone, eNone, eNone, 2500, True, ""
    ResetDT
    bRestorePower = False

	if Scorbit.SessionActive then
		GameModeStrTmp="MB{red}:Restore Daylight Ended"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

End Sub

Sub WinRestorePower
    AwardExtraBall
    LightEffect 2
    GiEffect 2
    'DMD
    DMDFlush
    DMD CL("CONGRATULATIONS"), "", "_", eBlink, eNone, eNone, 2500, True, "vo_welldone" &RndNbr(4)
    DMD CL("YOU RESTORED"), " DAYLIGHT", "_", eNone, eNone, eNone, 2500, True, ""
    StopRestorePower

	if Scorbit.SessionActive then
		GameModeStrTmp="MB{blue}:Restore Daylight Completed"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub UpdateRPLights 'update the lights blinking according to the number of hits
    Dim i
    Select Case RPlights(1)
        Case 0:For each i in aRPL1:i.BlinkInterval = 1500:i.State = 0:i.State = 2:Next:Addscore 1000
        Case 1:For each i in aRPL1:i.BlinkInterval = 1000:i.State = 0:i.State = 2:Next:Addscore 2000
        Case 2:For each i in aRPL1:i.BlinkInterval = 500:i.State = 0:i.State = 2:Next:Addscore 3000
        Case 3:For each i in aRPL1:i.BlinkInterval = 250:i.State = 0:i.State = 2:Next:Addscore 4000
        Case 4:For each i in aRPL1:i.State = 1:Next:RPlights(1) = 5:i = 5000 * Counter:DMD " DAYLIGHT 1 IS ON", CL(FormatScore(i) ), "_", eNone, eNone, eNone, 2000, True, "sfx_Electricity":Addscore i
        Case Else:RPlights(1) = 5 ' power line is on
    End Select
    Select Case RPlights(2)
        Case 0:For each i in aRPL2:i.BlinkInterval = 1500:i.State = 0:i.State = 2:Next
        Case 1:For each i in aRPL2:i.BlinkInterval = 1000:i.State = 0:i.State = 2:Next
        Case 2:For each i in aRPL2:i.BlinkInterval = 500:i.State = 0:i.State = 2:Next
        Case 3:For each i in aRPL2:i.BlinkInterval = 250:i.State = 0:i.State = 2:Next
        Case 4:For each i in aRPL2:i.State = 1:Next:RPlights(2) = 5:i = 5000 * Counter:DMD " DAYLIGHT 2 IS ON", CL(FormatScore(i) ), "_", eNone, eNone, eNone, 2000, True, "sfx_Electricity":Addscore i
        Case Else:RPlights(2) = 5 ' power line is on
    End Select
    Select Case RPlights(3)
        Case 0:For each i in aRPL3:i.BlinkInterval = 1500:i.State = 0:i.State = 2:Next
        Case 1:For each i in aRPL3:i.BlinkInterval = 1000:i.State = 0:i.State = 2:Next
        Case 2:For each i in aRPL3:i.BlinkInterval = 500:i.State = 0:i.State = 2:Next
        Case 3:For each i in aRPL3:i.BlinkInterval = 250:i.State = 0:i.State = 2:Next
        Case 4:For each i in aRPL3:i.State = 1:Next:RPlights(3) = 5:i = 5000 * Counter:DMD " DAYLIGHT 4 IS ON", CL(FormatScore(i) ), "_", eNone, eNone, eNone, 2000, True, "sfx_Electricity":Addscore i
        Case Else:RPlights(3) = 5 ' power line is on
    End Select
    Select Case RPlights(4)
        Case 0:For each i in aRPL4:i.BlinkInterval = 1500:i.State = 0:i.State = 2:Next
        Case 1:For each i in aRPL4:i.BlinkInterval = 1000:i.State = 0:i.State = 2:Next
        Case 2:For each i in aRPL4:i.BlinkInterval = 500:i.State = 0:i.State = 2:Next
        Case 3:For each i in aRPL4:i.BlinkInterval = 250:i.State = 0:i.State = 2:Next
        Case 4:For each i in aRPL4:i.State = 1:Next:RPlights(4) = 5:i = 5000 * Counter:DMD " DAYLIGHT 4 IS ON", CL(FormatScore(i) ), "_", eNone, eNone, eNone, 2000, True, "sfx_Electricity":Addscore i
        Case Else:RPlights(4) = 5 ' power line is on
    End Select
    Select Case RPlights(5)
        Case 0:For each i in aRPL5:i.BlinkInterval = 1500:i.State = 0:i.State = 2:Next
        Case 1:For each i in aRPL5:i.BlinkInterval = 1000:i.State = 0:i.State = 2:Next
        Case 2:For each i in aRPL5:i.BlinkInterval = 500:i.State = 0:i.State = 2:Next
        Case 3:For each i in aRPL5:i.BlinkInterval = 250:i.State = 0:i.State = 2:Next
        Case 4:For each i in aRPL5:i.State = 1:Next:RPlights(5) = 5:i = 5000 * Counter:DMD " DAYLIGHT 5 IS ON", CL(FormatScore(i) ), "_", eNone, eNone, eNone, 2000, True, "sfx_Electricity":Addscore i
        Case Else:RPlights(5) = 5 ' power line is on
    End Select
    Select Case RPlights(6)
        Case 0:For each i in aRPL6:i.BlinkInterval = 1500:i.State = 0:i.State = 2:Next
        Case 1:For each i in aRPL6:i.BlinkInterval = 1000:i.State = 0:i.State = 2:Next
        Case 2:For each i in aRPL6:i.BlinkInterval = 500:i.State = 0:i.State = 2:Next
        Case 3:For each i in aRPL6:i.BlinkInterval = 250:i.State = 0:i.State = 2:Next
        Case 4:For each i in aRPL6:i.State = 1:Next:RPlights(6) = 5:i = 5000 * Counter:DMD " DAYLIGHT 6 IS ON", CL(FormatScore(i) ), "_", eNone, eNone, eNone, 2000, True, "sfx_Electricity":Addscore i
        Case Else:RPlights(6) = 5 ' power line is on
    End Select
    Select Case RPlights(7)
        Case 0:For each i in aRPL7:i.BlinkInterval = 1500:i.State = 0:i.State = 2:Next
        Case 1:For each i in aRPL7:i.BlinkInterval = 1000:i.State = 0:i.State = 2:Next
        Case 2:For each i in aRPL7:i.BlinkInterval = 500:i.State = 0:i.State = 2:Next
        Case 3:For each i in aRPL7:i.BlinkInterval = 250:i.State = 0:i.State = 2:Next
        Case 4:For each i in aRPL7:i.State = 1:Next:RPlights(7) = 5:i = 5000 * Counter:DMD " DAYLIGHT 7 IS ON", CL(FormatScore(i) ), "_", eNone, eNone, eNone, 2000, True, "sfx_Electricity":Addscore i
        Case Else:RPlights(7) = 5 ' power line is on
    End Select
    Select Case RPlights(8)
        Case 0:For each i in aRPL8:i.BlinkInterval = 1500:i.State = 0:i.State = 2:Next
        Case 1:For each i in aRPL8:i.BlinkInterval = 1000:i.State = 0:i.State = 2:Next
        Case 2:For each i in aRPL8:i.BlinkInterval = 500:i.State = 0:i.State = 2:Next
        Case 3:For each i in aRPL8:i.BlinkInterval = 250:i.State = 0:i.State = 2:Next
        Case 4:For each i in aRPL8:i.State = 1:Next:RPlights(8) = 5:i = 5000 * Counter:DMD " DAYLIGHT 8 IS ON", CL(FormatScore(i) ), "_", eNone, eNone, eNone, 2000, True, "sfx_Electricity":Addscore i
        Case Else:RPlights(8) = 5 ' power line is on
    End Select
    Select Case RPlights(9)
        Case 0:For each i in aRPL9:i.BlinkInterval = 1500:i.State = 0:i.State = 2:Next
        Case 1:For each i in aRPL9:i.BlinkInterval = 1000:i.State = 0:i.State = 2:Next
        Case 2:For each i in aRPL9:i.BlinkInterval = 500:i.State = 0:i.State = 2:Next
        Case 3:For each i in aRPL9:i.BlinkInterval = 250:i.State = 0:i.State = 2:Next
        Case 4:For each i in aRPL9:i.State = 1:Next:RPlights(9) = 5:i = 5000 * Counter:DMD " DAYLIGHT 9 IS ON", CL(FormatScore(i) ), "_", eNone, eNone, eNone, 2000, True, "sfx_Electricity":Addscore i
        Case Else:RPlights(9) = 5 ' power line is on
    End Select
    Select Case RPlights(10)
        Case 0:For each i in aRPL10:i.BlinkInterval = 1500:i.State = 0:i.State = 2:Next
        Case 1:For each i in aRPL10:i.BlinkInterval = 1000:i.State = 0:i.State = 2:Next
        Case 2:For each i in aRPL10:i.BlinkInterval = 500:i.State = 0:i.State = 2:Next
        Case 3:For each i in aRPL10:i.BlinkInterval = 250:i.State = 0:i.State = 2:Next
        Case 4:For each i in aRPL10:i.State = 1:Next:RPlights(10) = 5:i = 5000 * Counter:DMD " DAYLIGHT 10 IS ON", CL(FormatScore(i) ), "_", eNone, eNone, eNone, 2000, True, "sfx_Electricity":Addscore i
        Case Else:RPlights(10) = 5 ' power line is on
    End Select
    Select Case RPlights(11)
        Case 0:For each i in aRPL11:i.BlinkInterval = 1500:i.State = 0:i.State = 2:Next
        Case 1:For each i in aRPL11:i.BlinkInterval = 1000:i.State = 0:i.State = 2:Next
        Case 1:For each i in aRPL11:i.BlinkInterval = 1000:i.State = 0:i.State = 2:Next
        Case 2:For each i in aRPL11:i.BlinkInterval = 500:i.State = 0:i.State = 2:Next
        Case 3:For each i in aRPL11:i.BlinkInterval = 250:i.State = 0:i.State = 2:Next
        Case 4:For each i in aRPL11:i.State = 1:Next:RPlights(11) = 5:i = 5000 * Counter:DMD " DAYLIGHT 11 IS ON", CL(FormatScore(i) ), "_", eNone, eNone, eNone, 2000, True, "sfx_Electricity":Addscore i
        Case Else:RPlights(11) = 5 ' power line is on
    End Select
End Sub

'********************************
'       Stop the Apcolpyse
' after 25 relics are found
'********************************
' Mode 2
' 3 ball multiball, ballsaver 20 seconds

Sub CheckHostages
    PlaySound "vo_rescuedhostage"
    UpdateHLights
    ApronDMDUpdate
    If HostagesLeft(CurrentPlayer) <= 0 AND Mode = 0 Then
        vpmTimer.AddTimer 2000, "StartEscapeHW '"
    End If

	if Scorbit.SessionActive then
		GameModeStrTmp="NA{pink}:Relics Collected (" &HostagesRescued(CurrentPlayer)& ")" 
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub StartEscapeHW
    DOF 144, DOFOn
    Dim i
    For Each i in aTiltLights:i.State = 0:Next 'turn all lights off
    GiOff
    Light084.BlinkInterval = 150
    Light084.State = 2
    Light083.BlinkInterval = 150
    Light083.State = 2
    Light077.BlinkInterval = 150
    Light077.State = 2
    Light085.State = 2
    bEscapeHWReady = True
    Mode = -1 'stop all modes even the normal scoring
    'Drop the droptargets
    DT(CurrentPlayer, 1) = 1
    DT(CurrentPlayer, 2) = 1
    DT(CurrentPlayer, 3) = 1
    UpdateDT
    LightSeqFlashers.UpdateInterval = 100
    LightSeqFlashers.Play SeqBlinking, , 150, 250
    DMDFlush
    DMD CL("STOP THE APOCALYPSE"), CL("IS READY"), "_", eNone, eNone, eNone, 2500, True, "vo_escapehorrorburg"
    DMD CL("SHOOT THE SCOOP"), CL("TO START"), "_", eNone, eNone, eNone, 2500, True, ""
End Sub

Sub StartEscapeHW2
    DOF 145, DOFOn
    dim i
    PlaySong "m_mb3"
	Start_Splash "zAPOC1","zAPOC2","","startapoc",200,0
'	DMD CL("STARTING"), CL("STOP APOCALYPSE"), "_", eNone, eNone, eNone, 2500, True, ""
'	DMD CL("SHOOT THE JACKPOTS"), CL("AND THE SPINNERS"), "_", eNone, eNone, eNone, 2500, True, ""
    LightSeqFlashers.StopPlay
    bEscapeHWReady = False
    bEscapeHW = True
    Mode = 2
    'setup lights - all lights off - all Gi blinks
    For Each i in aTiltLights:i.State = 0:Next
    For Each i In aGiLights:i.State = 2:Next
    For Each i In aGiLightsRED:i.State = 2:Next
    Jackpot(CurrentPlayer) = 50000 'reset to 50000 - spinners increase value
    'setup jackpot lights according to the nr of weapons collected
    SetupJackpots
    vpmTimer.AddTimer 600, "AddMultiball 2 '"
	if Scorbit.SessionActive then
		GameModeStrTmp="MB{blue}:Apocalypse Multiball Started"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
    EnableBallSaver 20
    ModeScore = 0
End Sub

Sub StopEscapeHW 'when the multiball is over
    DOF 144, DOFOff
    DOF 145, DOFOff
    bEscapeHW = False
    Mode = 0
    GiOn
    GiRedOff
    UpdateLights 'chaos and hostages lights
    UpdateKillerLights
    ChangeSong
    DT(CurrentPlayer, 1) = 0
    DT(CurrentPlayer, 2) = 0
    DT(CurrentPlayer, 3) = 0
    UpdateDT
    HostagesLeft(CurrentPlayer) = 25 'reset the hostages to start rescuing again
    ApronDMDUpdate
    Light077.State = 0               'be sure the super jackpot is off
    DMD "APOCALYPSE SCORE", CL(FormatScore(ModeScore) ), "_", eNone, eNone, eNone, 3000, True, ""

	if Scorbit.SessionActive then
		GameModeStrTmp="MB{red}:Apocalypse Multiball Ended"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

End Sub

Sub SetupJackpots
    Dim i, j, tmp
    tmp = Weapons(CurrentPlayer)
    If tmp> 5 then tmp = 5
    If tmp = 0 Then tmp = 1
    'reset the Jackpots
    for i = 1 to 5
        JackpotLights(i) = 0
    Next
    'setup random jackpots according to the nr of weapons
    j = RndNbr(5)
    For i = 1 to tmp
        do while JackpotLights(j) <> 0
            j = RndNbr(5)
        Loop
        JackpotLights(j) = 2
    Next
    UpdateJackpotLights
End Sub

Sub UpdateJackpotLights
    Light082.State = JackpotLights(1)
    Light081.State = JackpotLights(2)
    Light080.State = JackpotLights(3)
    Light002.State = JackpotLights(4)
    Light078.State = JackpotLights(5)
End Sub

'********************************
'       Abe Sapien
' after all required eggs
'       are collected
'********************************
' Mode 3
' jackpot on the ramps only
' aim for ramp combos


Sub StartPennywiseMB
    DOF 146, DOFOn
    bPennywise = True
    Mode = 3
    Dim i
    For Each i in aTiltLights:i.State = 0:Next 'turn all lights off
    GiOff
    EnableBallSaver 20
    GiRedOn
    UpdateHLights
    Jackpot(CurrentPlayer) = 50000
    'setup jackpot lights on the ramps
    JackpotLights(2) = 2
    JackpotLights(4) = 2
    UpdateJackpotLights
    ModeScore = 0
    PlaySong "m_mb2"
'fixing
	Start_Splash "zASMultiball0","","","multiball2",2222,0
    DMD CL("ABE SAPIEN"), CL("MULTIBALL"), "_", eNone, eNone, eNone, 1000, True, "vo_pennywisemb"
    vpmTimer.AddTimer 3500, "PlaySound""vo_shootramps"" '"
    AddMultiball 1
    FlashCrownWeapon 10000, 100
	GoldenArmy1.transz = -100
	GoldenArmy1Up.enabled = True
	GoldenArmy2.transz = -100
	GoldenArmy2Up.enabled = True

	if Scorbit.SessionActive then
		GameModeStrTmp="MB{blue}:ABE Sapien Multiball Started"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub StopPennywiseMB
    DOF 146, DOFOff
    bPennywise = False
    Mode = 0
    GiOn
    GiRedOff
    UpdateLights 'chaos and hostages lights
    UpdateKillerLights
    ChangeSong
    BalloonsLeft(CurrentPlayer) = 25 'set the balloons needed for next mb
    ApronDMDUpdate
    DMD "  ABE SAPIEN SCORE", CL(FormatScore(ModeScore) ), "_", eNone, eNone, eNone, 3000, True, ""
	GoldenArmy1Down.Enabled = True
	GoldenArmy2Down.Enabled = True

	if Scorbit.SessionActive then
		GameModeStrTmp="MB{red}:ABE Sapien Multiball Ended"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

End Sub

Sub GoldenArmy1_hit
    DOF 147, DOFPulse
	if GoldenArmy1.transz <> 0 Then exit Sub
    PlaySound "GARMY"
	GoldenArmy1Down.Enabled = True
end Sub

Sub GoldenArmy2_hit
    DOF 148, DOFPulse
	if GoldenArmy2.transz <> 0 Then exit Sub
    PlaySound "GARMY"
	GoldenArmy2Down.Enabled = True
end Sub

Sub GoldenArmy1Up_timer
	if GoldenArmy1.transz = 0 then
		GoldenArmy1Up.Enabled = False
		exit Sub
	end If
	GoldenArmy1.visible = True
	GoldenArmy1.collidable = True
	GoldenArmy1.transz = GoldenArmy1.transz + 10
End Sub

Sub GoldenArmy1Down_timer
	if GoldenArmy1.transz = -100 then
		GoldenArmy1Down.Enabled = False
		GoldenArmy1.visible = False
		GoldenArmy1.collidable = False
        AddScore 5000
	end If
	GoldenArmy1.transz = GoldenArmy1.transz - 10
End Sub

Sub GoldenArmy2Up_timer
	if GoldenArmy2.transz = 0 then
		GoldenArmy2Up.Enabled = False
		exit Sub
	end If
	GoldenArmy2.visible = True
	GoldenArmy2.collidable = True
	GoldenArmy2.transz = GoldenArmy2.transz + 10
End Sub

Sub GoldenArmy2Down_timer
	if GoldenArmy2.transz = -100 then
		GoldenArmy2Down.Enabled = False
		GoldenArmy2.visible = False
		GoldenArmy2.collidable = False
        AddScore 5000
	end If
	GoldenArmy2.transz = GoldenArmy2.transz - 10
End Sub

'********************************
'         HELLBOY SHOOTOUT
'     after locking 3 balls
'********************************
' Mode 4

'Sub Debug001_Hit()
	'if bdracula = false then StartDraculaMB
'End Sub
 
Sub StartDraculaMB
    DOF 149, DOFOn
    dim i
    PlaySong "m_mb1"
    DMD CL("STARTING"), CL("HELLBOY MULTIBALL"), "_", eNone, eNone, eNone, 1111, True, "vo_draculamb"
'moved    DMD CL("SHOOT 5 JACKPOTS"), "AND THE SUPERJACKPOT", "_", eNone, eNone, eNone, 2500, True, ""
    Start_Splash "zHBMultiball0","","","multiball1",2222,0
    bDracula = True
    Mode = 4
    'setup lights - all lights off - all Gi RED blinks
    For Each i in aTiltLights:i.State = 0:Next
    GiOff
    For Each i In aGiLightsRED:i.State = 2:Next
    Jackpot(CurrentPlayer) = 50000       'reset to 50000
    SuperJackpot(CurrentPlayer) = 250000 'reset to 250000
    'setup jackpot lights according to the nr of weapons collected
    SetupJackpots
    vpmTimer.AddTimer 3500, "AddMultiball 3 '"
	if Scorbit.SessionActive then
		GameModeStrTmp="MB{purple}:Hellboy Multiball Started"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
    EnableBallSaver 20
    JackpotCount = 0
    ModeScore = 0
    Light077.State = 0


    MBTENT1.transz = -100
    MBTENT1Up.enabled = True
	MBTENT2.transz = -100
    MBTENT2Up.enabled = True
	MBTENT3.transz = -100
    MBTENT3Up.enabled = True
    MBTENT4.transz = -100
    MBTENT4Up.enabled = True
    MBTENT5.transz = -100
    MBTENT5Up.enabled = True


End Sub

Sub StopDraculaMB
    DOF 149, DOFOff
    bDracula = False
    Mode = 0
    GiOn
    GiRedOff
    UpdateLights 'chaos and hostages lights
    UpdateKillerLights
    ChangeSong
    ApronDMDUpdate
    DMD "  HELLBOY MB SCORE", CL(FormatScore(ModeScore) ), "_", eNone, eNone, eNone, 3000, True, ""
    'reset locked balls
    DT(CurrentPlayer, 1) = 0
    DT(CurrentPlayer, 2) = 0
    DT(CurrentPlayer, 3) = 0
    UpdateDT
    BallsInLock(CurrentPlayer) = 0

MBTENT1Down.Enabled = True
MBTENT2Down.Enabled = True
MBTENT3Down.Enabled = True
MBTENT4Down.Enabled = True
MBTENT5Down.Enabled = True

End Sub

Sub MBTENT1_hit
    DOF 150, DOFPulse
	if MBTENT1.transz <> 0 Then exit Sub
    PlaySound "SLIME_1"
	MBTENT1Down.Enabled = True
end Sub

Sub MBTENT2_hit
    DOF 161, DOFPulse
	if MBTENT2.transz <> 0 Then exit Sub
    PlaySound "SLIME_2"
    PlaySound"tent_"&RndNbr(12)
    MBTENT2Down.Enabled = True
end sub

Sub MBTENT3_hit
    DOF 162, DOFPulse
	if MBTENT3.transz <> 0 Then exit Sub
    PlaySound "SLIME_3"
	MBTENT3Down.Enabled = True
end Sub

Sub MBTENT4_hit
    DOF 163, DOFPulse
	if MBTENT4.transz <> 0 Then exit Sub
    PlaySound "SLIME_4"
    PlaySound"tent_"&RndNbr(12)
	MBTENT4Down.Enabled = True
end Sub

Sub MBTENT5_hit
    DOF 164, DOFPulse
	if MBTENT5.transz <> 0 Then exit Sub
    PlaySound "Mon"
	MBTENT5Down.Enabled = True
end Sub


Sub MBTENT1Up_timer
	if MBTENT1.transz = 0 then
	   MBTENT1Up.Enabled = False
		exit Sub
	end If
	MBTENT1.visible = True
	MBTENT1.collidable = True
	MBTENT1.transz = MBTENT1.transz + 10
End Sub

Sub MBTENT1Down_timer
	if MBTENT1.transz = -100 then
		MBTENT1Down.Enabled = False
		MBTENT1.visible = False
		MBTENT1.collidable = False
        AddScore 50000
	end If
	MBTENT1.transz = MBTENT1.transz - 10
End Sub

Sub MBTENT2Up_timer
	if MBTENT2.transz = 0 then
		MBTENT2Up.Enabled = False
		exit Sub
	end If
	MBTENT2.visible = True
	MBTENT2.collidable = True
	MBTENT2.transz = MBTENT2.transz + 10
End Sub

Sub MBTENT2Down_timer
	if MBTENT2.transz = -100 then
		MBTENT2Down.Enabled = False
		MBTENT2.visible = False
		MBTENT2.collidable = False
        AddScore 50000
	end If
	MBTENT2.transz = MBTENT2.transz - 10
End Sub

Sub MBTENT3Up_timer
	if MBTENT3.transz = 0 then
		MBTENT3Up.Enabled = False
		exit Sub
	end If
	MBTENT3.visible = True
	MBTENT3.collidable = True
	MBTENT3.transz = MBTENT3.transz + 10
End Sub

Sub MBTENT3Down_timer
	if MBTENT3.transz = -100 then
		MBTENT3Down.Enabled = False
		MBTENT3.visible = False
		MBTENT3.collidable = False
        AddScore 50000
	end If
	MBTENT3.transz = MBTENT3.transz - 10
End Sub

Sub MBTENT4Up_timer
	if MBTENT4.transz = 0 then
		MBTENT4Up.Enabled = False
		exit Sub
	end If
	MBTENT4.visible = True
	MBTENT4.collidable = True
	MBTENT4.transz = MBTENT4.transz + 10
End Sub

Sub MBTENT4Down_timer
	if MBTENT4.transz = -100 then
		MBTENT4Down.Enabled = False
		MBTENT4.visible = False
		MBTENT4.collidable = False
        AddScore 50000
	end If
	MBTENT4.transz = MBTENT4.transz - 10
End Sub

Sub MBTENT5Up_timer
	if MBTENT5.transz = 0 then
	   MBTENT5Up.Enabled = False
		exit Sub
	end If
	MBTENT5.visible = True
	MBTENT5.collidable = True
	MBTENT5.transz = MBTENT5.transz + 10
End Sub

Sub MBTENT5Down_timer
	if MBTENT5.transz = -100 then
		MBTENT5Down.Enabled = False
		MBTENT5.visible = False
		MBTENT5.collidable = False
        AddScore 150000
	end If
	MBTENT5.transz = MBTENT5.transz - 10
End Sub


'******************************************
' check for balls trapped behind the gates
'******************************************

Sub Trigger014_Hit
    DOF 121, DOFPulse
    Me.TimerEnabled = 0
    Me.TimerEnabled = 1
End Sub

Sub Trigger014_UnHit
    Me.TimerEnabled = 0
End Sub

Sub Trigger014_Timer
    Me.TimerEnabled = 0
    dgatef.RotateToEnd
    draculagate.IsDropped = 1
    draculagate.TimerEnabled = 1
End Sub

Sub draculagate_Timer
    Me.TimerEnabled = 0
    dgatef.RotateToStart
    draculagate.IsDropped = 0
End Sub

'*******PORTAL******

Dim RotAngle4
RotAngle4 = 0

Sub PORTALLogo_Timer
    RotAngle4 = (RotAngle4+ 1)MOD 360
    FlasherlogoVP.Roty = RotAngle4
    
End Sub

'**************************
' LAVA lamps
'**************************

Dim LAVA1Pos, LAVA2POS, LAVAFLOW
LAVAFLOW = Array("f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", _
    "f10", "f11", "f12", "f13", "f14", "f15", "f16")

Sub StartLAVA
    LAVA1Pos = 0
    LAVA2POS = 2
   
    LAVATimer.Enabled = 1
End Sub

Sub LAVATimer_Timer
    'debug.print fire1pos
    LAVA1.ImageA = LAVAFLOW(LAVA1Pos)
    LAVA2.ImageA = LAVAFLOW(LAVA2Pos)
    
    LAVA1Pos = (LAVA1Pos + 1) MOD 16
    LAVA2Pos = (LAVA2Pos + 1) MOD 16
    
End Sub

'**************************
' SAVA lamps
'**************************

Dim SAVA1Pos, SAVAFLOW
SAVAFLOW = Array("S_1", "S_2", "S_3", "S_4", "S_5", "S_6", "S_7", "S_8", "S_9", "S_10", "S_11", "S_12", "S_13", "S_14", "S_15", "S_16", "S_17", "S_18", "S_19", "S_20", "S_21", "S_22", "S_23", "S_24", "S_25", "S_26", "S_27", "S_28", "S_29", _
    "S_30", "S_31", "S_32", "S_33", "S_34", "S_35", "S_36", "S_37", "S_38", "S_39", "S_40", "S_41", "S_42", "S_43", "S_44", "S_45", "S_46", "S_47", "S_48", "S_49", "S_50", "S_51", "S_52", "S_53", "S_54", "S_55", "S_56", "S_57", "S_58", "S_59")

Sub StartSAVA
    SAVA1Pos = 0
   
    SAVATimer.Enabled = 1
End Sub

Sub SAVATimer_Timer
    'debug.print fire1pos
    SAVA1.ImageA = SAVAFLOW(SAVA1Pos)
    
    SAVA1Pos = (SAVA1Pos + 1) MOD 59   
    
End Sub


'**************************
' PORTAL RASPUTIN
'**************************

Dim PORT1Pos, PORTFLOW
PORTFLOW = Array("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", _
    "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24")

Sub StartPORT
    PORT1Pos = 0
   
    PORTTimer.Enabled = 1
End Sub

Sub PORTTimer_Timer
    'debug.print fire1pos
    PORT1.ImageA = PORTFLOW(PORT1Pos)
    
    PORT1Pos = (PORT1Pos + 1) MOD 24    
    
End Sub

'******RASPUTINGLOVE******

Dim    Iron1Pos, IronFrames 
IronFrames = Array("ML-00", "ML-01", "ML-02", "ML-03", "ML-04", "ML-05", "ML-06", "ML-07", "ML-08", "ML-09", "ML-010", "ML-011", "ML-012", "ML-013", "ML-014", "ML-015", "ML-016", "ML-017", "ML-018", "ML-019", "ML-020", _
"ML-021", "ML-022", "ML-023", "ML-024")

Sub StartIron
    IronFlash.visible = True
    Iron1Pos = 0
    IronTimer.Enabled = True
    StopIronTimer.interval = 1500
    StopIronTimer.Enabled = True
    PlaySound "TF_electricA"
    DOF 160, DOFPulse
End Sub
 
Sub IronTimer_timer
    IronFlash.ImageA = IronFrames(Iron1Pos)
    Iron1Pos = (Iron1Pos + 1) MOD 25
End Sub
 
Sub StopIron
    IronFlash.visible = False
    IronTimer.Enabled = False
End Sub

Sub StopIronTimer_Timer
    StopIron
    Me.Enabled = False
    IronTimer.enabled = False
End Sub

'***************************

' Tree animation
Dim MyPi, TreeStep, TreeDir, BoatStep, BoatDir
MyPi = Round(4 * Atn(1), 6) / 90
TreeStep = 0
BoatStep =0

Sub Trees_Timer()
    TreeDir = SIN(TreeStep * MyPi)
    TreeStep = (TreeStep + 1)MOD 360
    Tree1.RotY = - TreeDir *5
    Tree2.RotY =  TreeDir *8
    Tree3.RotY =  TreeDir *3
    Tree4.RotY = - TreeDir *5
    BABA.RotY = - TreeDir *5
    thead.RotZ = - TreeDir *10
    bht.RotZ = - TreeDir *6
    thead2.Rotz = - TreeDir *10
    thorns.Rotz = - TreeDir *10
    BABA001.RotY = - TreeDir *5
    idol3.Rotz = - TreeDir *15
    CatL.Roty = - TreeDir *2
    CatR.Roty =  TreeDir *2
    Tree001.RotZ = - TreeDir *5
    Tree002.RotZ =  TreeDir *8
    Tree007.RotZ = - TreeDir *5
    Tree008.RotZ =  TreeDir *8
    Tree009.RotY = - TreeDir *5
    Tree010.RotZ =  TreeDir *5
    Tree011.RotZ =  TreeDir *8
    Tree013.RotZ = - TreeDir *5
    Tree018.RotZ = - TreeDir *3
    MBTENT1.RotY = - TreeDir *8
    MBTENT2.RotY =  TreeDir *8
    MBTENT3.RotY = - TreeDir *8
    MBTENT4.RotY =  TreeDir *8
    MBTENT5.RotZ =  TreeDir *5
    
    
End Sub

' Shake Cats
Dim CatLPos, CatRPos

Sub ShakeLeftCat
    CatLPos = 8
    CatLTimer.Enabled = 1
End Sub

Sub CatLTimer_Timer
    CatL.TransY = CatLPos
    If CatLPos = 0 Then Me.Enabled = 0:Exit Sub
    If CatLPos < 0 Then
        CatLPos = ABS(CatLPos)- 1
    Else
        CatLPos = - CatLPos + 1
    End If
End Sub

Sub ShakeRightCat
    CatRPos = 8
    CatRTimer.Enabled = 1
End Sub

Sub CatRTimer_Timer
    CatR.TransY = CatRPos
    If CatRPos = 0 Then Me.Enabled = 0:Exit Sub
    If CatRPos < 0 Then
        CatRPos = ABS(CatRPos)- 1
    Else
        CatRPos = - CatRPos + 1
    End If
End Sub

'******Model Flashing*********

Dim fDuration
Dim fPeriod

Sub FlashGloveWeapon (fDuration, fPeriod)
	FlashGloveTimer.interval = fPeriod
	FlashGloveTimer.enabled = True
	StopGloveTimer.interval = fDuration
	StopGloveTimer.Enabled = True
End Sub

Sub FlashGloveTimer_Timer
	if Glove.visible = false Then
	   Glove.visible = True
	Else
		Glove.visible = False
	End If
End Sub

Sub StopGloveTimer_Timer
	StopGloveTimer.enabled = False
	FlashGloveTimer.enabled = False
	Glove.visible = False
End Sub

Sub FlashHornsWeapon (fDuration, fPeriod)
	FlashHornsTimer.interval = fPeriod
	FlashHornsTimer.enabled = True
	StopHornsTimer.interval = fDuration
	StopHornsTimer.Enabled = True
End Sub

Sub FlashHornsTimer_Timer
	if Horns.visible = false Then
	   Horns.visible = True
	Else
		Horns.visible = False
	End If
End Sub

Sub StopHornsTimer_Timer
	StopHornsTimer.enabled = False
	FlashHornsTimer.enabled = False
	Horns.visible = False
End Sub



Sub FlashEggs (fDuration, fPeriod)
	FlashEggsTimer.interval = fPeriod
	FlashEggsTimer.enabled = True
	StopEggsTimer.interval = fDuration
	StopEggsTimer.Enabled = True
End Sub

Sub FlashEggsTimer_Timer
	if egg1.visible = false Then
		egg1.visible = True
    
	Else
    egg1.visible = False
    
	End If
End Sub

Sub StopEggsTimer_Timer
	StopEggsTimer.enabled = False
	FlashEggsTimer.enabled = False
	egg1.visible = False
    
End Sub

Sub FlashLitRamp1 (fDuration, fPeriod)
	FlashRamp1Timer.interval = fPeriod
	FlashRamp1Timer.enabled = True
	StopRamp1Timer.interval = fDuration
	StopRamp1Timer.Enabled = True
End Sub

Sub FlashRamp1Timer_Timer
	if LITRAMP1.visible = false Then
	   LITRAMP1.visible = True
	Else
		LITRAMP1.visible = False
	End If
End Sub

Sub StopRamp1timer_Timer
	StopRamp1Timer.enabled = False
	FlashRamp1Timer.enabled = False
	LITRAMP1.visible = False
End Sub

Sub FlashCrownWeapon (fDuration, fPeriod)
	FlashCrownTimer.interval = fPeriod
	FlashCrownTimer.enabled = True
	StopCrownTimer.interval = fDuration
	StopCrownTimer.Enabled = True
End Sub

Sub FlashCrownTimer_Timer
	if Crown.visible = false Then
	   Crown.visible = True
	Else
		Crown.visible = False
	End If
End Sub

Sub StopCrownTimer_Timer
	StopCrownTimer.enabled = False
	FlashCrownTimer.enabled = False
	Crown.visible = False
End Sub


'*****Arm Mr Wink********

WatL.visible = True
WatL2.visible = False

' Initialize the timer
TimerJAG.Interval = 2
TimerJAG.Enabled = False
Dim Timer1Count: Timer1Count = 0

Sub TimerJAG_Timer
	Timer1Count = Timer1Count + 1
	Select Case Timer1Count
		Case 1:  
			WatL.visible = False
			WatL2.visible = True
		Case 100:  ' number of timer intervals to wait before swapping back
			WatL.visible = True
			WatL2.visible = False
			Timer1Count = 0
			TimerJAG.Enabled = False
	End Select
End Sub


Sub Leftslingshot_Hit()
	
End Sub

'********Portal Charge********

Dim	Charge1Pos, ChargeFrames 
ChargeFrames = Array("CH-000", "CH-001", "CH-002", "CH-003", "CH-004", "CH-005", "CH-006", "CH-007", "CH-008", "CH-009", "CH-010", "CH-011", "CH-012", "CH-013", "CH-014", "CH-015", "CH-016", "CH-017", "CH-018", "CH-019", "CH-020", _
"CH-021", "CH-022", "CH-023", "CH-024", "CH-025", "CH-026", "CH-027", "CH-028")


Sub StartCharge
    ChargeFlash.visible = True
    Charge1Pos = 0
    ChargeTimer.Enabled = True
    StopChargeTimer.interval = 1500
    StopChargeTimer.Enabled = True
    PlaySound ""
End Sub
 
Sub ChargeTimer_timer
    ChargeFlash.ImageA = ChargeFrames(Charge1Pos)
    Charge1Pos = (Charge1Pos + 1) MOD 28
End Sub
 
Sub StopCharge
    ChargeFlash.visible = False
    ChargeTimer.Enabled = False
End Sub

Sub StopChargeTimer_Timer
    StopCharge
    Me.Enabled = False
    ChargeTimer.enabled = False
End Sub

'**********HELLBOYGUN**************

Dim	Gun1Pos, Frames
Frames = Array("MF-000", "MF-001", "MF-002", "MF-003", "MF-004", "MF-005", "MF-006", "MF-007", "MF-008", "MF-009", "MF-010", "MF-011", "MF-012", "MF-013", "MF-014", "MF-015", "MF-016", "MF-017", "MF-018", "MF-019", "MF-020", _
"MF-021", "MF-022", "MF-023", "MF-024", "MF-025", "MF-026", "MF-027", "MF-028", "MF-029")


Sub StartGun
    GunFlash.visible = True
    Gun1Pos = 0
    GunTimer.Enabled = True
    StopGunTimer.interval = 1500
    StopGunTimer.Enabled = True
    PlaySound "BigGun"
End Sub
 
Sub GunTimer_timer
    GunFlash.ImageA = Frames(Gun1Pos)
    Gun1Pos = (Gun1Pos + 1) MOD 30
End Sub
 
Sub StopGun
    GunFlash.visible = False
    GunTimer.Enabled = False
End Sub

Sub StopGunTimer_Timer
    StopGun
    Me.Enabled = False
    GunTimer.enabled = False
End Sub

'**********LIZFIRE**************

Dim	LIZF1Pos, LIZFFrames
LIZFFrames = Array("LZ-1", "LZ-2", "LZ-3", "LZ-4", "LZ-5", "LZ-6", "LZ-7", "LZ-8", "LZ-9", "LZ-10", "LZ-11", "LZ-12", "LZ-13", "LZ-14", "LZ-15", "LZ-16", "LZ-17", "LZ-18", "LZ-19", "LZ-20", "LZ-21", "LZ-22", "LZ-23", "LZ-24", "LZ-25", "LZ-26", "LZ-27", "LZ-28", "LZ-29", "LZ-30", _
"LZ-31", "LZ-32", "LZ-33", "LZ-34", "LZ-35", "LZ-36", "LZ-37", "LZ-38", "LZ-39", "LZ-40", "LZ-41", "LZ-42", "LZ-43", "LZ-44", "LZ-45", "LZ-46", "LZ-47", "LZ-48")


Sub StartLIZF
    LIZFFlash.visible = True
    LIZF1Pos = 0
    LIZFTimer.Enabled = True
    StopLIZFTimer.interval = 2500
    StopLIZFTimer.Enabled = True
    PlaySound "FLAMEUP"
End Sub
 
Sub LIZFTimer_timer
    LIZFFlash.ImageA = LIZFFrames(LIZF1Pos)
    LIZF1Pos = (LIZF1Pos + 1) MOD 48
End Sub
 
Sub StopLIZF
    LIZFFlash.visible = False
    LIZFTimer.Enabled = False
End Sub

Sub StopLIZFTimer_Timer
    StopLIZF
    Me.Enabled = False
    LIZFTimer.enabled = False

End Sub
'*******LobsterJohnson Gun*********

Dim	Guns1Pos, GunsFrames
GunsFrames = Array("MF-000", "MF-001", "MF-002", "MF-003", "MF-004", "MF-005", "MF-006", "MF-007", "MF-008", "MF-009", "MF-010", "MF-011", "MF-012", "MF-013", "MF-014", "MF-015", "MF-016", "MF-017", "MF-018", "MF-019", "MF-020", _
"MF-021", "MF-022", "MF-023", "MF-024", "MF-025", "MF-026", "MF-027", "MF-028", "MF-029")

Sub StartGuns
    Gun2Flash.visible = True
    Guns1Pos = 0
    Gun2Timer.Enabled = True
    StopGun2Timer.interval = 1500
    StopGun2Timer.Enabled = True
    PlaySound "fx_fire"
End Sub
 
Sub Gun2Timer_timer
    Gun2Flash.ImageA = GunsFrames(Guns1Pos)
    Guns1Pos = (Guns1Pos + 1) MOD 30
End Sub
 
Sub StopGuns
    Gun2Flash.visible = False
    Gun2Timer.Enabled = False
End Sub

Sub StopGun2Timer_Timer
    StopGuns
    Me.Enabled = False
    Gun2Timer.enabled = False
End Sub

'****BABAYAGA******

Sub BoatTimer_Timer()
    BoatDir = SIN(BoatStep * MyPi)
    BoatStep = (BoatStep + 1)MOD 360
    BABA.Y = BABA.Y + BoatDir * 0.5
    BABA001.Y = BABA001.Y + BoatDir * 0.5
    MBTENT5.X = MBTENT5.X + BoatDir * 0.5
    
End Sub
Sub ramptrigger001_hit()
    TimerJAG.Enabled = True
If NOT bMultiBallMode Then PlaySound"WINK_"&RndNbr(5)
	WireRampOn True	 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger01_hit()
    DOF 151, DOFPulse
	WireRampOn True	 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger01_unhit()
	If ActiveBall.VelY > 0 Then WireRampOff: End If
End Sub

Sub ramptrigger02_hit()
    DOF 152, DOFPulse
    If NOT bMultiBallMode Then PlaySound"LJ_"&RndNbr(15)
	WireRampOff	 'Turn off the Plastic Ramp Sound
End Sub

Sub ramptrigger02_unhit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
End Sub

Sub ramptrigger03_hit()
    DOF 153, DOFPulse
	WireRampOff	 'Exiting Wire Ramp Stop Playing Sound
End Sub

Sub ramptrigger03_unhit()
	RandomSoundRampStop ramptrigger03
End Sub

Sub ramptrigger04_hit()
    DOF 154, DOFPulse
	WireRampOn True	 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger04_unhit()
	If ActiveBall.VelY > 0 Then WireRampOff: End If
End Sub

Sub ramptrigger05_hit()
    DOF 155, DOFPulse
	WireRampOff	 'Turn off the Plastic Ramp Sound
End Sub

Sub ramptrigger05_unhit()
	RandomSoundRampStop ramptrigger05
End Sub

Sub ramptrigger06_hit()
    DOF 156, DOFPulse
	WireRampOn True	 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger06_unhit()
	If ActiveBall.VelY > 0 Then WireRampOff: End If
End Sub

Sub ramptrigger07_hit()
    DOF 157, DOFPulse
	WireRampOff	 'Turn off the Plastic Ramp Sound
    FlashLitRamp1 1000, 100
End Sub

Sub ramptrigger07_unhit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
End Sub

Sub ramptrigger08_hit()
    DOF 158, DOFPulse
	WireRampOff	 'Exiting Wire Ramp Stop Playing Sound
End Sub

Sub ramptrigger08_unhit()
	RandomSoundRampStop ramptrigger08
End Sub

'******************************************************
'	ZPHY:  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Adding nFozzy roth physics : pt1 rubber dampeners 				https://youtu.be/AXX3aen06FM?si=Xqd-rcaqTlgEd_wx
' Adding nFozzy roth physics : pt2 flipper physics 					https://youtu.be/VSBFuK2RCPE?si=i8ne8Ao2co8rt7fy
' Adding nFozzy roth physics : pt3 other elements 					https://youtu.be/JN8HEJapCvs?si=hvgMOk-ej1BEYjJv
'
' Note: BallMass must be set to 1. BallSize should be set to 50 (in other words the ball radius is 25)
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 12-15    |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
'
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 3-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |





'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. and, special scripting
'
' TriggerLF and RF should now be 27 vp units from the flippers. In addition, 3 degrees should be added to the end angle
' when creating these triggers.
'
' RF.ReProcessBalls Activeball and LF.ReProcessBalls Activeball must be added the flipper_collide subs.
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.4            | 0.4                   | 0.375                  | 0.375              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'
'        x.AddPt "Polarity", 0, 0, 0
'        x.AddPt "Polarity", 1, 0.05, - 2.7
'        x.AddPt "Polarity", 2, 0.16, - 2.7
'        x.AddPt "Polarity", 3, 0.22, - 0
'        x.AddPt "Polarity", 4, 0.25, - 0
'        x.AddPt "Polarity", 5, 0.3, - 1
'        x.AddPt "Polarity", 6, 0.4, - 2
'        x.AddPt "Polarity", 7, 0.5, - 2.7
'        x.AddPt "Polarity", 8, 0.65, - 1.8
'        x.AddPt "Polarity", 9, 0.75, - 0.5
'        x.AddPt "Polarity", 10, 0.81, - 0.5
'        x.AddPt "Polarity", 11, 0.88, 0
'        x.AddPt "Polarity", 12, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
'
'
''*******************************************
'' Mid 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 3.7
'		x.AddPt "Polarity", 2, 0.16, - 3.7
'		x.AddPt "Polarity", 3, 0.22, - 0
'		x.AddPt "Polarity", 4, 0.25, - 0
'		x.AddPt "Polarity", 5, 0.3, - 2
'		x.AddPt "Polarity", 6, 0.4, - 3
'		x.AddPt "Polarity", 7, 0.5, - 3.7
'		x.AddPt "Polarity", 8, 0.65, - 2.3
'		x.AddPt "Polarity", 9, 0.75, - 1.5
'		x.AddPt "Polarity", 10, 0.81, - 1
'		x.AddPt "Polarity", 11, 0.88, 0
'		x.AddPt "Polarity", 12, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
''*******************************************
''  Late 80's early 90's
'
'Sub InitPolarity()
'	dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 5
'		x.AddPt "Polarity", 2, 0.16, - 5
'		x.AddPt "Polarity", 3, 0.22, - 0
'		x.AddPt "Polarity", 4, 0.25, - 0
'		x.AddPt "Polarity", 5, 0.3, - 2
'		x.AddPt "Polarity", 6, 0.4, - 3
'		x.AddPt "Polarity", 7, 0.5, - 4.0
'		x.AddPt "Polarity", 8, 0.7, - 3.5
'		x.AddPt "Polarity", 9, 0.75, - 3.0
'		x.AddPt "Polarity", 10, 0.8, - 2.5
'		x.AddPt "Polarity", 11, 0.85, - 2.0
'		x.AddPt "Polarity", 12, 0.9, - 1.5
'		x.AddPt "Polarity", 13, 0.95, - 1.0
'		x.AddPt "Polarity", 14, 1, - 0.5
'		x.AddPt "Polarity", 15, 1.1, 0
'		x.AddPt "Polarity", 16, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945

'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'	LF.SetObjects "LF", LeftFlipper, TriggerLF
'	RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub

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
		x.AddPt "Polarity", 1, 0.05, - 5.5
		x.AddPt "Polarity", 2, 0.16, - 5.5
		x.AddPt "Polarity", 3, 0.20, - 0.75
		x.AddPt "Polarity", 4, 0.25, - 1.25
		x.AddPt "Polarity", 5, 0.3, - 1.75
		x.AddPt "Polarity", 6, 0.4, - 3.5
		x.AddPt "Polarity", 7, 0.5, - 5.25
		x.AddPt "Polarity", 8, 0.7, - 4.0
		x.AddPt "Polarity", 9, 0.75, - 3.5
		x.AddPt "Polarity", 10, 0.8, - 3.0
		x.AddPt "Polarity", 11, 0.85, - 2.5
		x.AddPt "Polarity", 12, 0.9, - 2.0
		x.AddPt "Polarity", 13, 0.95, - 1.5
		x.AddPt "Polarity", 14, 1, - 1.0
		x.AddPt "Polarity", 15, 1.05, -0.5
		x.AddPt "Polarity", 16, 1.1, 0
		x.AddPt "Polarity", 17, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.23, 0.85
		x.AddPt "Velocity", 2, 0.27, 1
		x.AddPt "Velocity", 3, 0.3, 1
		x.AddPt "Velocity", 4, 0.35, 1
		x.AddPt "Velocity", 5, 0.6, 1 '0.982
		x.AddPt "Velocity", 6, 0.62, 1.0
		x.AddPt "Velocity", 7, 0.702, 0.968
		x.AddPt "Velocity", 8, 0.95,  0.968
		x.AddPt "Velocity", 9, 1.03,  0.945
		x.AddPt "Velocity", 10, 1.5,  0.945

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
	Dim BOT
	BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
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

Dim LFPress, RFPress, LFCount, RFCount
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
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
'   Const EOSReturn = 0.035  'mid 80's to early 90's
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
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls
		
		For b = 0 To UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= - 0.4 Then BOT(b).vely =  - 0.4
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
			aBall.velz = aBall.velz * coef
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
'Sub RDampen_Timer
'	Cor.Update
'End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9	 'Level of bounces. Recommmended value of 0.7-1

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

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
'Function RotPoint(x,y,angle)
'	dim rx, ry
'	rx = x*dCos(angle) - y*dSin(angle)
'	ry = x*dSin(angle) + y*dCos(angle)
'	RotPoint = Array(rx,ry)
'End Function

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
' 	ZRDT:  DROP TARGETS by Rothbauerw
'******************************************************
' The Stand Up and Drop Target solutions improve the physics for targets to create more realistic behavior. It allows the ball
' to move through the target enabling the ability to score more than one target with a well placed shot.
' It also handles full target animation, switch handling and deflection on hit. For drop targets there is also a slight lift when
' the drop targets raise, bricking, and popping the ball up if it's over the drop target when it raises.
'
' Add a Timers named DTAnim and STAnim to editor to handle drop & standup target animations, or run them off an always-on 10ms timer (GameTimer)
' DTAnim.interval = 10
' DTAnim.enabled = True

' Sub DTAnim_Timer
' 	DoDTAnim
'	DoSTAnim
' End Sub

' For each drop target, we'll use two wall objects for physics calculations and one primitive for visuals and
' animation. We will not use target objects.  Place your drop target primitive the same as you would a VP drop target.
' The primitive should have it's pivot point centered on the x and y axis and at or just below the playfield
' level on the z axis. Orientation needs to be set using Rotz and bending deflection using Rotx. You'll find a hooded
' target mesh in this table's example. It uses the same texture map as the VP drop targets.
'
' For each stand up target we'll use a vp target, a laid back collidable primitive, and one primitive for visuals and animation.
' The visual primitive should should have it's pivot point centered on the x and y axis and the z should be at or just below the playfield.
' The target should animate backwards using transy.
'
' To create visual target primitives that work with the stand up and drop target code, follow the below instructions:
' (Other methods will work as well, but this is easy for even non-blender users to do)
' 1) Open a new blank table. Delete everything off the table in editor.
' 2) Copy and paste the VP target from your table into this blank table.
' 3) Place the target at x = 0, y = 0  (upper left hand corner) with an orientation of 0 (target facing the front of the table)
' 4) Under the file menu, select Export "OBJ Mesh"
' 5) Go to "https://threejs.org/editor/". Here you can modify the exported obj file. When you export, it exports your target and also 
'    the playfield mesh. You need to delete the playfield mesh here. Under the file menu, chose import, and select the obj you exported
'    from VPX. In the right hand panel, find the Playfield object and click on it and delete. Then use the file menu to Export OBJ.
' 6) In VPX, you can add a primitive and use "Import Mesh" to import the exported obj from the previous step. X,Y,Z scale should be 1.
'    The primitive will use the same target texture as the VP target object. 
'
' * Note, each target must have a unique switch number. If they share a same number, add 100 to additional target with that number.
' For example, three targets with switch 32 would use 32, 132, 232 for their switch numbers.
' The 100 and 200 will be removed when setting the switch value for the target.

'******************************************************
'  DROP TARGETS INITIALIZATION
'******************************************************

Class DropTarget
  Private m_primary, m_secondary, m_prim, m_sw, m_animate, m_isDropped

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Secondary(): Set Secondary = m_secondary: End Property
  Public Property Let Secondary(input): Set m_secondary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
  Public Property Let IsDropped(input): m_isDropped = input: End Property

  Public default Function init(primary, secondary, prim, sw, animate, isDropped)
    Set m_primary = primary
    Set m_secondary = secondary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate
    m_isDropped = isDropped

    Set Init = Me
  End Function
End Class

'Define a variable for each drop target
Dim DT1, DT2, DT3

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
'   primary:	primary target wall to determine drop
'   secondary:  wall used to simulate the ball striking a bent or offset target after the initial Hit
'   prim:	   primitive target used for visuals and animation
'				   IMPORTANT!!!
'				   rotz must be used for orientation
'				   rotx to bend the target back
'				   transz to move it up and down
'				   the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'   switch:	 ROM switch number
'   animate:	Array slot for handling the animation instrucitons, set to 0
'				   Values for animate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target
'   isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.
'					Use the function DTDropped(switchid) to check a target's drop status.

Set DT1 = (new DropTarget)(tomb1, tomb1b, tomb1p, 1, 0, False)
Set DT2 = (new DropTarget)(tomb2, tomb2b, tomb2p, 2, 0, False)
Set DT3 = (new DropTarget)(tomb3, tomb3b, tomb3p, 3, 0, False)

Dim DTArray
DTArray = Array(DT1, DT2, DT3)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 90 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 100 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick
Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dbg "*****  DTHIT " &switch
	Dim i
	i = DTArrayID(switch)
	
	PlayTargetSound
	DTArray(i).animate = DTCheckBrick(ActiveBall,DTArray(i).prim)
	If DTArray(i).animate = 1 Or DTArray(i).animate = 3 Or DTArray(i).animate = 4 Then
		DTBallPhysics ActiveBall, DTArray(i).prim.rotz, DTMass
		Dbg " DT Hit -- NOT 2"
	Else
		Dbg "DT Hit ANIMATE is 2 "
	End If
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch)
	
	DTArray(i).animate =  - 1
	DoDTAnim
End Sub

Sub DTDrop(switch)
	Dbg "DT DROP " &switch
	Dim i
	i = DTArrayID(switch)
	
	DTArray(i).animate = 1
	DoDTAnim
End Sub

Function DTArrayID(switch)
	Dim i
	For i = 0 To UBound(DTArray)
		If DTArray(i).sw = switch Then
			DTArrayID = i
			Exit Function
		End If
	Next
End Function

Sub DTBallPhysics(aBall, angle, mass)
	Dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	
	calc1 = cor.BallVel(aball.id) * Cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Cos(rangle + 4 * Atn(1) / 2)
	calc3 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Sin(rangle + 4 * Atn(1) / 2)
	
	aBall.velx = calc1 * Cos(rangle) + calc2
	aBall.vely = calc1 * Sin(rangle) + calc3
End Sub

'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim)
	Dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	Xintersect = (aBall.y - dtprim.y - Tan(bangle) * aball.x + Tan(rangle2) * dtprim.x) / (Tan(rangle2) - Tan(bangle))
	Yintersect = Tan(rangle2) * Xintersect + (dtprim.y - Tan(rangle2) * dtprim.x)
	
	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		If DTEnableBrick = 1 And  perpvel > DTBrickVel And DTBrickVel <> 0 And cdist < 8 Then
			DTCheckBrick = 3
		Else
			DTCheckBrick = 1
		End If
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		DTCheckBrick = 4
	Else
		DTCheckBrick = 0
	End If
End Function

Sub DoDTAnim()
	Dim i
	For i = 0 To UBound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch, animate)
	Dim transz, switchid
	Dim animtime, rangle
	
	switchid = switch
	
	Dim ind
	ind = DTArrayID(switchid)
	
	rangle = prim.rotz * PI / 180
	
	DTAnimate = animate
	
	If animate = 0 Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If (animate = 1 Or animate = 4) And animtime < DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		DTAnimate = animate
		Exit Function
	ElseIf (animate = 1 Or animate = 4) And animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 1 'If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0 'updated by rothbauerw to account for edge case
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		animate = 2
		SoundDropTargetDrop prim
	End If
	
	If animate = 2 Then
		transz = (animtime - DTDropDelay) / DTDropSpeed * DTDropUnits *  - 1
		If prim.transz >  - DTDropUnits  Then
			prim.transz = transz
		End If
		
		prim.rotx = DTMaxBend * Cos(rangle) / 2
		prim.roty = DTMaxBend * Sin(rangle) / 2
		
		If prim.transz <= - DTDropUnits Then
			prim.transz =  - DTDropUnits
			secondary.collidable = 0
			DTArray(ind).isDropped = True 'Mark target as dropped
			'If UsingROM Then
			'	controller.Switch(Switchid mod 100) = 1
			'Else
			'	DTAction switchid
			'End If
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		End If
	End If
	
	If animate = 3 And animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
	ElseIf animate = 3 And animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If
	
	If animate =  - 1 Then
		transz = (1 - (animtime) / DTDropUpSpeed) * DTDropUnits *  - 1
		
		If prim.transz =  - DTDropUnits Then
			Dim b
			Dim BOT
			BOT = GetBalls
			
			For b = 0 To UBound(BOT)
				If InRotRect(BOT(b).x,BOT(b).y,prim.x, prim.y, prim.rotz, - 25, - 10,25, - 10,25,25, - 25,25) And BOT(b).z < prim.z + DTDropUnits + 25 Then
					BOT(b).velz = 20
				End If
			Next
		End If
		
		If prim.transz < 0 Then
			prim.transz = transz
		ElseIf transz > 0 Then
			prim.transz = transz
		End If
		
		If prim.transz > DTDropUpUnits Then
			DTAnimate =  - 2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = GameTime
		End If
		primary.collidable = 0
		secondary.collidable = 1
		DTArray(ind).isDropped = False 'Mark target as not dropped
'		If UsingROM Then controller.Switch(Switchid mod 100) = 0
	End If
	
	If animate =  - 2 And animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay) / DTDropSpeed * DTDropUnits *  - 1 + DTDropUpUnits
		If prim.transz < 0 Then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0
			
			primary.collidable = 1
			secondary.collidable = 0
		End If
	End If
End Function

Function DTDropped(switchid)
	Dim ind
	ind = DTArrayID(switchid)
	
	DTDropped = DTArray(ind).isDropped
End Function

'Sub DTAction(switchid)
'	Select Case switchid
'		Case 1
'			Addscore 1000
'			ShadowDT(0).visible = False
'			
'		Case 2
'			Addscore 1000
'			ShadowDT(1).visible = False
'			
'		Case 3
'			Addscore 1000
'			ShadowDT(2).visible = False
'	End Select
'End Sub


'******************************************************
'****  END DROP TARGETS
'******************************************************

'******************************************************
'	ZRST: STAND-UP TARGETS by Rothbauerw
'******************************************************

Class StandupTarget
  Private m_primary, m_prim, m_sw, m_animate

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public default Function init(primary, prim, sw, animate)
    Set m_primary = primary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate

    Set Init = Me
  End Function
End Class

'Define a variable for each stand-up target
Dim ST1, ST2, ST3, ST4, ST5, ST6, ST7

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
'   primary:	vp target to determine target hit
'   prim:	   primitive target used for visuals and animation
'				   IMPORTANT!!!
'				   transy must be used to offset the target animation
'   switch:	 ROM switch number
'   animate:	Arrary slot for handling the animation instrucitons, set to 0
'
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts


Set ST3 = (new StandupTarget)(Target003, Target003p,3, 0)
Set ST4 = (new StandupTarget)(Target004, Target004p,4, 0)


'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
'   STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST3, ST4)

'Configure the behavior of Stand-up Targets
Const STAnimStep = 1.5  'vpunits per animation step (control return to Start)
Const STMaxOffset = 9   'max vp units target moves when hit

Const STMass = 0.2	  'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)
	
	PlayTargetSound
	STArray(i).animate = STCheckHit(ActiveBall,STArray(i).primary)
	
	If STArray(i).animate <> 0 Then
		DTBallPhysics ActiveBall, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 To UBound(STArray)
		If STArray(i).sw = switch Then
			STArrayID = i
			Exit Function
		End If
	Next
End Function

Function STCheckHit(aBall, target) 'Check if target is hit on it's face
	Dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		STCheckHit = 1
	Else
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i = 0 To UBound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime
	
	STAnimate = animate
	
	If animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If animate = 1 Then
		primary.collidable = 0
		prim.transy =  - STMaxOffset
		'If UsingROM Then
		'	vpmTimer.PulseSw switch mod 100
		'Else
		'	STAction switch
		'End If
		STAnimate = 2
		Exit Function
	ElseIf animate = 2 Then
		prim.transy = prim.transy + STAnimStep
		If prim.transy >= 0 Then
			prim.transy = 0
			primary.collidable = 1
			STAnimate = 0
			Exit Function
		Else
			STAnimate = 2
		End If
	End If
End Function


'Sub STAction(Switch)
'	Select Case Switch
'		Case 11
'			Addscore 1000
'			Flash1 True 'Demo of the flasher
'			vpmTimer.AddTimer 150,"Flash1 False'"   'Disable the flash after short time, just like a ROM would do
'			
'		Case 12
'			Addscore 1000
'			Flash2 True 'Demo of the flasher
'			vpmTimer.AddTimer 150,"Flash2 False'"   'Disable the flash after short time, just like a ROM would do
'			
'		Case 13
'			Addscore 1000
'			Flash3 True 'Demo of the flasher
'			vpmTimer.AddTimer 150,"Flash3 False'"   'Disable the flash after short time, just like a ROM would do
'	End Select
'End Sub

'******************************************************
'****   END STAND-UP TARGETS
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
	Dim BOT
	BOT = GetBalls
	
	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 To tnob - 1
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(BOT) =lob- 1 Then Exit Sub

'Rotate the idols
   
    Idol2.Rotz = 120 - (BOT(2).Y)\15
	Idol1.Rotz = -120 + (BOT(2).Y)\15
    
	' play the rolling sound for each ball
	For b = 0 To UBound(BOT)
		If BallVel(BOT(b)) > 1 And BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If
		
		' Ball Drop Sounds
		If BOT(b).VelZ <  - 1 And BOT(b).z < 55 And BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz >  - 7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If
			End If
		End If
		
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
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

Sub RandomSoundRampStop(obj)
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtVol "wireramp_stop1", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtVol "wireramp_stop2", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtVol "wireramp_stop3", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************





'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.
' Create the following new collections:
'	 Metals (all metal objects, metal walls, metal posts, metal wire guides)
'	 Apron (the apron walls and plunger wall)
'	 Walls (all wood or plastic walls)
'	 Rollovers (wire rollover triggers, star triggers, or button triggers)
'	 Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
'	 Gates (plate gates)
'	 GatesWire (wire gates)
'	 Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate
' how to make these updates. But in summary the following needs to be updated:
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
'
' Tutorial videos by Apophis
' Audio : Adding Fleep Part 1					https://youtu.be/rG35JVHxtx4?si=zdN9W4cZWEyXbOz_
' Audio : Adding Fleep Part 2					https://youtu.be/dk110pWMxGo?si=2iGMImXXZ0SFKVCh
' Audio : Adding Fleep Part 3					https://youtu.be/ESXWGJZY_EI?si=6D20E2nUM-xAw7xy


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
BumperSoundFactor = 4.25						'volume multiplier; must not be zero
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

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
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
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
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

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
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

	FlipperCradleCollision ball1, ball2, velocity

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

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************
'
' MerlinRTP PupDMD Framework 
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



Const pTopper=0
Const pDMD=1
Const pBackglass=2
Const pPlayfield=3
Const pMusic=4
Const pMusic2=5
Const pCallouts=6
Const pBackglass2=7
Const pTopper2=8
Const pPopUP=9
Const pPopUP2=10


'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pTargerLetters=5


'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit

'Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
'PuPlayer.B2SInit "", pGameName

	If usePUP = false then exit sub

	PuPlayer.LabelInit pQRlocation

	pSetPageLayouts

	pSetPage(1)   'set blank text overlay page.
	if Table1.Option("Scorbit", 0, 1, 1, 0, 0, Array("Disabled", "Enabled")) = 1 Then DelayPairing
End Sub 'end PUPINIT

sub pSetPage(pagenum)  
	If usePUP = false then exit sub
  
    PuPlayer.LabelShowPage pQRlocation,pagenum,0,""   'set page to blank 0 page if want off
'    PDMDCurPage=pagenum
end Sub

sub delayPairing
	vpmtimer.addtimer 2500,"CheckPairing '"
end sub

Sub PuPEvent(EventNum)
	If usePUP = false then exit sub
	PuPlayer.B2SData "E"&EventNum,1  'send event to puppack driver  
End Sub


sub CheckPairing 'Check if a pairing code should display on PinUp
	If usePUP = false then exit sub
	If Table1.Option("Scorbit QR on PinUp", 0, 1, 1, 1, 0, Array("No", "Yes")) = 0 Then Exit Sub

	if (Scorbit.NeedsPairing) then 
		PuPlayer.playlistplayex pQRlocation,"PuPOverlays","Scorbit_Pair.png",0,1
		PuPlayer.LabelSet pQRlocation, "ScorbitQR1", "PuPOverlays\\QRcode.png",1,"{'mt':2,'width':19.61, 'height':36,'xalign':0,'yalign':0,'ypos':32,'xpos':74.6}"
		Dbg "Should be displaying QR Pairing Code"
		DelayQRClaim.Interval=6000
		DelayQRClaim.Enabled=True
	end if
End sub

Sub hideScorbit 'Hides scorbit from PinUp
	if usePUP And Table1.Option("Scorbit QR on PinUp", 0, 1, 1, 1, 0, Array("No", "Yes")) = 1 Then
		Dbg "Should be hiding QR Codes"
		PuPlayer.playlistplayex pQRlocation,"PuPOverlays","DefaultBG.png",0,1
		plabelhide "ScorbitQR1"
		plabelhide "ScorbitQR2"
	end if
End Sub

Sub pLabelHide(labName)
	If usePUP = false then exit sub
	PuPlayer.LabelSet pQRlocation,labName,"",0,""  
end sub

Sub pLabelShow(labName)
	If usePUP = false then exit sub
	PuPlayer.LabelSet pQRlocation,labName,"",1,""   
end sub

sub pLabelSetPos(labName, xpos, ypos)
	If usePUP = false then exit sub
	PuPlayer.LabelSet pQRlocation,labName,"",1,"{'mt':2,'xpos':"&xpos& ",'ypos':"&ypos&"}"    
end sub

sub pLabelSetSizeImage(labName, lWidth, lHeight)
	If usePUP = false then exit sub
	PuPlayer.LabelSet pQRlocation,labName,"",1,"{'mt':2,'width':"& lWidth & ",'height':"&lHeight&"}" 
end sub

Sub pupCreateLabelImage(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
	If usePUP = false then exit sub
	PuPlayer.LabelNew pQRlocation,lName ,"",50,RGB(100,100,100),0,1,1,1,1,pagenum,lvis
	PuPlayer.LabelSet pQRlocation,lName,lFilename,lvis,"{'mt':2,'width':"&IWidth&",'height':"&Iheight&",'xpos':"&xpos&",'ypos':"&ypos&"}"
end Sub

'*****************************************************************************************************************************************
'  ZERR: ERROR LOGS by baldgeek, modified by Arelyel (adds a buffer to avoid excessive jitter)
'*****************************************************************************************************************************************

' Log File Usage:
'   WriteToLog "Label 1", "Message 1 "
'   WriteToLog "Label 2", "Message 2 "

Dim LogFileObj
'Set LogFileObj = New DebugLogFile

Class DebugLogFile
	
	Private Filename
	Private TxtFileStream
	Private LogBuffer(25)
	Private LogBufferIndex

	Private Sub Class_Initialize
		Dim i
		For i = LBound(LogBuffer) To UBound(LogBuffer)
			LogBuffer(i) = NULL
		Next
		LogBufferIndex = LBound(LogBuffer)
	End Sub
	
	Private Function LZ(ByVal Number, ByVal Places)
		Dim Zeros
		Zeros = String(CInt(Places), "0")
		LZ = Right(Zeros & CStr(Number), Places)
	End Function
	
	Private Function GetTimeStamp
		Dim CurrTime, Elapsed, MilliSecs
		CurrTime = Now()
		Elapsed = Timer()
		MilliSecs = Int((Elapsed - Int(Elapsed)) * 1000)
		GetTimeStamp = _
		LZ(Year(CurrTime),   4) & "-" _
		& LZ(Month(CurrTime),  2) & "-" _
		& LZ(Day(CurrTime),    2) & " " _
		& LZ(Hour(CurrTime),   2) & ":" _
		& LZ(Minute(CurrTime), 2) & ":" _
		& LZ(Second(CurrTime), 2) & ":" _
		& LZ(MilliSecs, 4)
	End Function
	
	' *** Debug.Print the time with milliseconds, and a message of your choice
	Public Sub WriteToBuffer(label, message, append)
		Dim FormattedMsg, Timestamp
		Timestamp = GetTimeStamp
		FormattedMsg = GetTimeStamp + " : " + label + " : " + message
		Debug.print FormattedMsg

		If append = False Then
			DumpBuffer
			
			'Filename = UserDirectory + "\" + cGameName + "_debug_log.txt"
			Filename = cGameName + "_debug_log.txt"
			Set TxtFileStream = CreateObject("Scripting.FileSystemObject").OpenTextFile(Filename, 2, True)
			TxtFileStream.WriteLine FormattedMsg
			TxtFileStream.Close
			Set TxtFileStream = Nothing
		Else
			LogBuffer(LogBufferIndex) = FormattedMsg
			LogBufferIndex = LogBufferIndex + 1
			If LogBufferIndex > UBound(LogBuffer) Then DumpBuffer
		End If
	End Sub

	Public Sub DumpBuffer()
		Dim i

		LogBufferIndex = LBound(LogBuffer)

		'Filename = UserDirectory + "\" + cGameName + "_debug_log.txt"
		Filename = cGameName + "_debug_log.txt"
		Set TxtFileStream = CreateObject("Scripting.FileSystemObject").OpenTextFile(Filename, 8, True)

		For i = LBound(LogBuffer) To UBound(LogBuffer)
			If Not IsNull(LogBuffer(i)) Then TxtFileStream.WriteLine LogBuffer(i)
			LogBuffer(i) = NULL
		Next

		TxtFileStream.Close
		Set TxtFileStream = Nothing
	End Sub
End Class

Sub Dbg(message) 'Modified to be backwards compatible for Hellboy
	'LogFileObj.WriteToBuffer cGameName, message, True
End Sub

Sub NewLog()
	'LogFileObj.WriteToBuffer "NEW Log", " ", False
End Sub


'*****************************************************************

Sub pSetPageLayouts

DIM dmddef
DIM dmdalt
DIM dmdscr
DIM dmdfixed

'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
'***********************************************************************'
'<screen#>, in standard we’d set this to pDMD ( or 1)
'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, degrees in tenths   (900=90 degrees)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but if you want to ‘force’ a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT… this will assign this label to this ‘page’ or group.
'<visible> initial state of label. visible=1 show, 0 = off.

	pupCreateLabelImage "ScorbitQR1","PuPOverlays\\QRcode.png",50,30,34,60,1,0
	pupCreateLabelImage "ScorbitQR2","PuPOverlays\\QRclaim.png",50,30,34,60,1,0


End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
' ZSCO: SCORBIT Interface v2.0.beta1
' To Use:
' 0) Contact the Scorbit team (such as in the Scorbit Discord server) to get (or create) the Machine ID for your table.
' 1) Create a timer named tmrScorbit; it should be disabled by default. Interval does not matter (it is set to 2 seconds in the code).
' 2) If you want to use a VPX flasher for Scorbit QR code and statuses:
'		Set ScorbitUseFlasher to true. Create a square flasher named ScorbitFlasher. Set visibility off, opacity 100%, amount 100%. 
'		Place this where you want Scorbit QR codes and statuses to appear, such as on the apron. 
'		Set height to 2 (or make it 2 units higher than the highest object under it).
' 3) If you want to use a large VPX flasher to display large Scorbit QR codes (easier to scan for lower-res displays):
'		Set ScorbitUseLargeFlasher to true. Create a square flasher named ScorbitFlasherLarge. Set visibility off, opacity 100%, amount 100%. 
'		Make this flasher rather large. Place on the middle of the playfield. 
'		Set height to 2 (or make it 2 units higher than the highest object under it). 
'		This will display only QR codes when the option "Scorbit QR Large" is enabled.
' 4) If ScorbitUseFlasher (step 2), in the VPX images manager, import these images (from the SDK):
'		ScorbitNotReady, ScorbitError, ScorbitClaimInApp, ScorbitClaimed, ScorbitDisabled, and ScorbitReady.
' 5) In the VPX sound manager, import these sounds:
'		scorbit_detected_2, scorbit_detected_2b, scorbit_login.
' 6) If your table has a PinUp pack, copy (and optionally customize) these images to your PUP pack (pupoverlays typically):
'		QRcodeB, QRcodeS.
' 7) Modify the DoInit call in the initScorbit Sub as follows:
'    	Replace 0 with your Machine ID from Scorbit
'	  	Replace TablesDirectory & "\ScorbitSDK_2_0" with the full path to the Scorbit SDK and binaries
'     	Replace puplayer.getroot & "\PupOverlays" with the full path to where the generated QR Code images should be saved
'			(Should use PupOverlays if the table uses PinUp, otherwise can be anything like TablesDirectory & "\" & cGameName)
'     	Replace "1.0.0" with the version number of your table
'			(Ideally you should define your table version in a constant at the top of your table script)
'     	Replace OPDB-ID with your table on OPDB - eg: https://opdb.org/machines/2103 
'			(original vpins will have a different OPDB ID provided by Scorbit)
' 8) Customize these functions as necessary, and then have them get called as indicated 
'		initScorbit
'			in your Table1_Init Sub (After PUP is initialized, if applicable)
'			in your Table1_OptionEvent Sub (when eventId is 3)
'		StartSession
'			When a game starts (e.g. ResetForNewGame)
'		StopSession
'			When the game is over (e.g. EndOfGame)
'		StopSession2
'			When the game is cancelled (e.g. Table1_Exit, or a Slam Tilt)
'		SendUpdate
'			When Score Changes (e.g. AddScore) 
'			(this is optional but not recommended for vpins; see Scorbit_SendSessionUpdate and ideally use that instead)
'		SetGameMode
'			When different game events happen like starting a mode, MB etc. 
'			(ScorbitBuildGameModes helper function shows you how)
' 9) Customize Callbacks 
'		Scorbit_Paired
'			Called when machine is successfully paired.
'		Scorbit_PlayerClaimed
'			Called when player is claimed.
'		Scorbit_SendSessionUpdate
'			Called when it is time to send a score update to Scorbit
'		Scorbit_ClaimQRPinUp
'			Call when we want to show/hide the claim QR code on PinUp
'			(generally should be called by Scorbit_UpdateQR)
'		Scorbit_updateQR
'			Call/called when we should re-calculate what to display for Scorbit statuses and QR codes
'			(any changes to game in progress status, change in which player is up, or when a ball hits / unhits the plunger lane trigger)
'		Scorbit_Debug
'			All debug information gets called by this Sub. You can customize this to use debug.print, your choice of a logger, or 
'			nothing at all.
' 10) MOVE YOUR CAR!!!
'
'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
' TABLE CUSTOMIZATION START HERE 

Const ScorbitUseFlasher = False
Const ScorbitUseLargeFlasher = True

Dim ScorbitQRPath

Sub initScorbit()
	'Register Tweak UI Scorbit Options
	Dim WorkAroundOptionBug 'Table.Option currently throws an error when calling as a sub due to a bug
    WorkAroundOptionBug = Table1.Option("Scorbit", 0, 1, 1, 0, 0, Array("Disabled", "Enabled"))
	If ScorbitUseFlasher = False And ScorbitUseLargeFlasher = True Then WorkAroundOptionBug = Table1.Option("Scorbit Large Table QR", 0, 1, 1, 0, 0, Array("Off", "On"))
    WorkAroundOptionBug = Table1.Option("Scorbit Claim QR", 0, 1, 1, 1, 0, Array("Hide (Claim in App)", "Show"))
	WorkAroundOptionBug = Table1.Option("Scorbit UUID", 0, 1, 1, 0, 0, Array("Use System", "Force Alternate")) 'If Scorbit is having trouble getting UUID on Windows, you can try "Force Alternate"
	'ScorbitUploadLog = Table1.Option("Scorbit Upload Log", 0, 1, 1, 0, 0, Array("No Timeline", "TimeLine")) 'Slower machines might suffer with timeline feature
	If UsePUP = True Then WorkAroundOptionBug = Table1.Option("Scorbit QR on PinUp", 0, 1, 1, 1, 0, Array("No", "Yes")) 'Comment this out if table does not support PinUp
    'If UsePUP = True Then WorkAroundOptionBug = Table1.Option("Scorbit Large PinUp QR", 0, 1, 1, 0, 0, Array("Off", "On"))

	'Initialize, or deactivate, Scorbit, as appropriate
	If (Table1.Option("Scorbit", 0, 1, 1, 0, 0, Array("Disabled", "Enabled")) = 1) And Scorbit.Enabled = False Then 'Enabling Scorbit when disabled
'		if Scorbit.DoInit(4372, "PupOverlays", myVersion, "hellboy-vpin") then 	' Staging

		If UsePUP = True And Table1.Option("Scorbit QR on PinUp", 0, 1, 1, 1, 0, Array("No", "Yes")) = 1 Then ScorbitQRPath = puplayer.getroot & "\PupOverlays"
		If UsePUP = False Or Table1.Option("Scorbit QR on PinUp", 0, 1, 1, 1, 0, Array("No", "Yes")) = 0 Then ScorbitQRPath = TablesDirectory & "\ScorbitSDK_2_0"

		if Scorbit.DoInit(4352, TablesDirectory & "\ScorbitSDK_2_0", ScorbitQRPath, myVersion, "hellboy-vpin") then
			tmrScorbit.Interval=2000
			tmrScorbit.UserValue = 0
			tmrScorbit.Enabled=True 
			'Scorbit.UploadLog = ScorbitUploadLog
			Scorbit.UploadLog = 0
		End if
	ElseIf (Table1.Option("Scorbit", 0, 1, 1, 0, 0, Array("Disabled", "Enabled")) = 0) And Scorbit.Enabled = True Then 'Disabling Scorbit when enabled
		Scorbit.RunAsync = False 'So we force the script to wait after calling StopSession2 before destroying the class
		tmrScorbit.Enabled = False
		Scorbit.StopSession2 Score(1), Score(2), Score(3), Score(4), PlayersPlayingGame, true 'This is a cancelled session
		Set Scorbit = Nothing 'Reset Scorbit class
		Set Scorbit = New ScorbitIF
	End If

	Scorbit_updateQR
End Sub

Sub Scorbit_Paired()								' Scorbit callback when new machine is paired 
	Scorbit_Debug "Machine paired"
	PlaySound "scorbit_login"

	'If we paired in the middle of a game, we should immediately initialize a session (note that any game logs prior to this call will not be included)
	If bGameInPlay = True And Scorbit.SessionActive = False Then
		Scorbit.StartSession
	End If

	Scorbit_UpdateQR
	HideScorbit
End Sub 

Sub Scorbit_PlayerClaimed(PlayerNum, PlayerNameOrInitials)	' Scorbit callback when QR Is Claimed 
	Scorbit_Debug "Player claimed"
	PlaySound "scorbit_login"
	Scorbit_UpdateQR

	If usePUP = false then exit sub
	puPlayer.LabelSet pDMDText,"Player", PlayerNameOrInitials,1,""
End Sub

Sub Scorbit_SendSessionUpdate() 'Scorbit callback when we should send a game update
	'Note: If you are calling SendUpdate on every score change (not recommended for VPX/vpins), you can remove it from this callback
	Scorbit.SendUpdate Score(1), Score(2), Score(3), Score(4), Balls, CurrentPlayer, PlayersPlayingGame
End Sub

Sub Scorbit_updateQR() 'Call when we should update the display of Scorbit statuses / QR Codes (should also be called on hit/unhit of the plunger lane trigger)
	If Table1.Option("Scorbit", 0, 1, 1, 0, 0, Array("Disabled", "Enabled")) = 0 Then 'Scorbit manually disabled
		ScorbitSetFlasherImages "ScorbitDisabled", False
		Exit Sub
	End If

	If Scorbit is Nothing Then 'Scorbit not initialized
		ScorbitSetFlasherImages "ScorbitNotReady", False
		Exit Sub
	End If

	If Scorbit.Enabled = False Then 'Scorbit error
		ScorbitSetFlasherImages "ScorbitError", False
		Exit Sub
	End If

	CheckPairing

	If Scorbit.NeedsPairing Then 'Machine needs to be paired
		'Don't show on the big flasher if a ball is in play
		If BIP <= 0 And bBallInPlungerLane = False Then 'No balls are in play on the playfield
			ScorbitSetFlasherImages "ScorbitQRCode", True
		Else
			ScorbitSetFlasherImages "ScorbitQRCode", False
		End If
		Exit Sub
	End If

	If Scorbit.SessionActive = False And bGameInPlay = False Then 'No game in progress, but Scorbit was paired
		ScorbitSetFlasherImages "ScorbitReady", False
		Exit Sub
	End If

	'Here, you might add any special cases where Scorbit would be unavailable, such as running a co-op game

	If Scorbit.SessionActive = False Then 'Game in progress but Scorbit not initialized for this game
		ScorbitSetFlasherImages "ScorbitDisabled", False
		Exit Sub
	End If

	If Scorbit.SessionActive = True Then
		If Scorbit.GetName(CurrentPlayer) = "" Then 'Current player did not claim their slot
			If Table1.Option("Scorbit Claim QR", 0, 1, 1, 1, 0, Array("Hide (Claim in App)", "Show")) = 1 Then
				If BIP <= 1 And bBallInPlungerLane = True And bAutoPlunger = False Then 'No balls are in play on the playfield, but a ball is sitting in the plunger
					ScorbitSetFlasherImages "ScorbitQRClaim", True
					Scorbit_ClaimQRPinUP True
				Else
					ScorbitSetFlasherImages "ScorbitQRClaim", False
					Scorbit_ClaimQRPinUP False
				End If
			Else
				ScorbitSetFlasherImages "ScorbitClaimInApp", False
				Scorbit_ClaimQRPinUP False
			End If
		Else 'Current player claimed
			ScorbitSetFlasherImages "ScorbitClaimed", False
			Scorbit_ClaimQRPinUP False
		End If
	End If
End Sub

Sub Scorbit_ClaimQRPinUP(bShow)
	'Exit Sub 'Un-comment this line if your table does not support PinUp

	if usePUP = False Then Exit Sub
	if Scorbit.SessionActive=False then Exit Sub 
	if Scorbit.NeedsPairing then exit sub

	if Table1.Option("Scorbit Claim QR", 0, 1, 1, 1, 0, Array("Hide (Claim in App)", "Show")) = 0 then Exit Sub
	If Table1.Option("Scorbit QR on PinUp", 0, 1, 1, 1, 0, Array("No", "Yes")) = 0 Then Exit Sub

	Dbg "bShow:" & bShow
	Dbg "Balls:" & Balls
	Dbg "CurrentPlayer:" & Scorbit.GetNameOrInitials(CurrentPlayer)

	if bShow and balls=1 and Scorbit.GetNameOrInitials(CurrentPlayer)="" And UsePUP = True then
		Dbg "Should be displaying Claim QR Code"
		PuPlayer.playlistplayex pQRlocation,"PuPOverlays","Scorbit_Claim.png",0,1
		PuPlayer.LabelSet pQRlocation, "ScorbitQR2", "PuPOverlays\\QRclaim.png",1,"{'mt':2,'width':19.61, 'height':36,'xalign':0,'yalign':0,'ypos':32,'xpos':74.6}"
	Else
		Dbg "Should be displaying main overlay"
		HideScorbit
	End if 
End Sub

Sub StopScorbit
	Dbg "Should be stopping scorbit"
	Scorbit.StopSession Score(1), Score(2), Score(3), Score(4), PlayersPlayingGame   ' Stop updateing scores
End Sub

Sub ScorbitBuildGameModes(sMode)		' Custom function to build the game modes for better stats 
	dim GameModeStr
	if Scorbit.SessionActive=False then Exit Sub 
	GameModeStr = sMode

	Scorbit.SetGameMode(GameModeStr)

End Sub 

' END ----------

Sub Scorbit_LOGUpload(state)	' Callback during the log creation process.  0=Creating Log, 1=Uploading Log, 2=Done 
	Select Case state 
		case 0:
			dbg "CREATING LOG"
		case 1:
			dbg "Uploading LOG"
		case 2:
			dbg "LOG Complete"
	End Select 
End Sub 

Sub Scorbit_Debug(debugInfo)	'Callback containing debugging information from Scorbit
	Dbg "SCORBIT: " & debugInfo 'You can comment out this line if you do not want debugging, or change to your own debug handling.
	Debug.print "SCORBIT: " & debugInfo
End Sub
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
' TABLE CUSTOMIZATION END HERE - NO NEED TO EDIT BELOW THIS LINE


dim Scorbit : Set Scorbit = New ScorbitIF
' Workaround - Call get a reference to Member Function
Sub tmrScorbit_Timer()	'Timer to send heartbeat 
	tmrScorbit.Interval = 2000 'DO NOT MODIFY
	Scorbit.DoTimer(tmrScorbit.UserValue)
	tmrScorbit.UserValue=tmrScorbit.UserValue+1
	if tmrScorbit.UserValue>5 then tmrScorbit.UserValue=0
End Sub

Function ScorbitIF_Callback()
	Scorbit.Callback()
End Function 

Sub ScorbitSetFlasherImages(imageName, showOnLargeFlasher) 'When using flashers, this sets the image which the flashers display. Kept outside of the class because we may be setting images when Scorbit class has not been constructed.
	If ScorbitUseFlasher = True Then
		If imageName<>"" Then
			ScorbitFlasher.ImageA = imageName
			ScorbitFlasher.Visible = True
		Else
			ScorbitFlasher.Visible = False
		End If
	End If

	If ScorbitUseLargeFlasher = True And (Table1.Option("Scorbit Large Table QR", 0, 1, 1, 0, 0, Array("Off", "On")) = 1 Or ScorbitUseFlasher = False) Then
		If showOnLargeFlasher = True And imageName<>"" Then
			ScorbitFlasherLarge.ImageA = imageName
			ScorbitFlasherLarge.Visible = True
		Else
			ScorbitFlasherLarge.Visible = False
		End If
	End If
End Sub

Class ScorbitIF
	Public RunAsync 'Whether API calls should be async (do not stop the script while waiting for a response)

	Private bSessionActive
	Private bNeedsPairing
	Private bUploadLog
	Private bActive
	Private LogFile
	Private LogIdx

	Private bProduction

	Private TypeLib
	Private MyMac
	Private Serial
	Private MyUUID
	Private TableVersion

	Private SessionUUID
	Private SessionSeq
	Private SessionTimeStart
	Private bWaitResp
	Private GameMode
	Private GameModeOrig		' Non escaped version for log
	Private VenueMachineID
	Private CachedPlayer
	Private SaveCurrentPlayer

	private bEnabled
	Private sToken
	Private machineID
	Private dirQRCode
	Private dirScorbitBin
	Private opdbID
	Private wsh

	Private objXmlHttpMain
	Private objXmlHttpMainAsync
	Private fso
	Private Domain

	Public Sub Class_Initialize()
		bActive="false"
		bSessionActive=False
		bEnabled=False
		RunAsync=False

		Set LogFile = CreateObject("Scripting.Dictionary")
		Set CachedPlayer = CreateObject("Scripting.Dictionary")
		LogIdx = 0
	End Sub 

	Public Property Let UploadLog(bValue)
		bUploadLog = bValue
	End Property

	Public Sub DoTimer(bInterval)	'Heartbeat timer (2 second interval)
		if bInterval=0 then 
			SendHeartbeat()
		elseif bSessionActive then
			Scorbit_SendSessionUpdate()
		End if 
	End Sub 

	Public Function GetName(PlayerNum)	'Return Parsed Player's Scorbit name  
		if PlayerNum<1 or PlayerNum>4 then 
			GetName=""
		else 
			GetName=CachedPlayer.Item(PlayerNum & "_name")
		End if 
	End Function

	Public Function GetNameOrInitials(PlayerNum)	'Return Parsed Player's Scorbit name, or initials if they prefer to use their initials
		if PlayerNum<1 or PlayerNum>4 then 
			GetNameOrInitials=""
		else
			If CachedPlayer.Item(PlayerNum & "_prefer_initials") = True Then
				GetNameOrInitials=CachedPlayer.Item(PlayerNum & "_initials")
			Else
				GetNameOrInitials=CachedPlayer.Item(PlayerNum & "_name")
			End If
		End if 
	End Function

	Public Function GetInitials(PlayerNum)	'Return Parsed Player's initials
		if PlayerNum<1 or PlayerNum>4 then 
			GetInitials=""
		else
			GetInitials=CachedPlayer.Item(PlayerNum & "_initials")
		End if 
	End Function

	Public Function DoInit(MyMachineID, ScorbitSDKDir, QRCodeDir, MyTableVersion, opdb) 'Initialize a Scorbit connection
		DoInit = False
		If bEnabled Then Exit Function 	'DO NOT REMOVE; prevent initializing if we already initialized

		dim Nad
		Dim EndPoint
		Dim resultStr 
		Dim UUIDParts 
		Dim UUIDFile

		bProduction=1
'		bProduction=0 'Uncomment if using a staging MyMachineID
		SaveCurrentPlayer=0
		VenueMachineID=""
		bWaitResp=False 
		RunAsync=False 
		opdbID=opdb
		dirScorbitBin=ScorbitSDKDir
		dirQrCode=QRCodeDir
		MachineID=MyMachineID
		TableVersion=MyTableVersion
		bNeedsPairing=False 
		if bProduction then 
			domain = "api.scorbit.io"
		else 
			domain = "staging.scorbit.io"
			domain = "scorbit-api-staging.herokuapp.com"
		End if 
		Set fso = CreateObject("Scripting.FileSystemObject")
		dim objLocator:Set objLocator = CreateObject("WbemScripting.SWbemLocator")
		Dim objService:Set objService = objLocator.ConnectServer(".", "root\cimv2")
		Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
		Set objXmlHttpMainAsync = CreateObject("Microsoft.XMLHTTP")
		objXmlHttpMain.onreadystatechange = GetRef("ScorbitIF_Callback")
		Set wsh = CreateObject("WScript.Shell")

		' Get Mac for Serial Number 
		dim Nads: set Nads = objService.ExecQuery("Select * from Win32_NetworkAdapter where physicaladapter=true")
		for each Nad in Nads
			if not isnull(Nad.MACAddress) then
				if left(Nad.MACAddress, 6)<>"00090F" then ' Skip over forticlient MAC
					Scorbit_Debug "Using MAC Addresses:" & Nad.MACAddress & " From Adapter:" & Nad.description   
					MyMac=replace(Nad.MACAddress, ":", "")
					Exit For 
				End if 
			End if 
		Next
		Serial=eval("&H" & mid(MyMac, 5))
		if Serial<0 then Serial=eval("&H" & mid(MyMac, 6))		' Mac Address Overflow Special Case 
		if MyMachineID<>2108 then 			' GOTG did it wrong but MachineID should be added to serial number also
			Serial=Serial+MyMachineID
		End if 
'		Serial=123456
		Scorbit_Debug "Detected serial: " & Serial

		' Get System UUID
		set Nads = objService.ExecQuery("SELECT * FROM Win32_ComputerSystemProduct")
		for each Nad in Nads
			Scorbit_Debug "Using UUID:" & Nad.UUID   
			MyUUID=Nad.UUID
			Exit For 
		Next

		if MyUUID="" or IsNull(MyUUID) then 
			MsgBox "SCORBIT - Cannot get UUID, Disabling. Try going into the Tweak UI and setting Scorbit UUID to Force Alternate."
			Exit Function
		elseif CountLetters("0", MyUUID) >= 16 or CountLetters("F", MyUUID) >= 16 or Table1.Option("Scorbit UUID", 0, 1, 1, 0, 0, Array("Use System", "Force Alternate")) = 1 then 'UUIDs with 16 (half) or more 0s or Fs are likely invalid / defaults, so we need to fall back to the alternate
			If fso.FolderExists(UserDirectory) then 
				If fso.FileExists(UserDirectory & "ScorbitUUID.dat") then
					Set UUIDFile = fso.OpenTextFile(UserDirectory & "ScorbitUUID.dat",1)
					MyUUID = UUIDFile.ReadLine()
					UUIDFile.Close
					Set UUIDFile = Nothing
				Else 
					MyUUID=GUID()
					Set UUIDFile=fso.CreateTextFile(UserDirectory & "ScorbitUUID.dat",True)
					UUIDFile.WriteLine MyUUID
					UUIDFile.Close
					Set UUIDFile=Nothing
				End if
			End if 
		End if

		' Clean UUID
		UUIDParts=split(MyUUID, "-")
		MyUUID=LCASE(Hex(eval("&h" & UUIDParts(0))+MyMachineID) & UUIDParts(1) &  UUIDParts(2) &  UUIDParts(3) & UUIDParts(4))		 ' Add MachineID to UUID
		MyUUID=LPad(MyUUID, 32, "0")
'		MyUUID=Replace(MyUUID, "-",  "")
		Scorbit_Debug "MyUUID: " & MyUUID 


' Debug
'		myUUID="adc12b19a3504453a7414e722f58737f"
'		Serial="123456778"

		' Authenticate, get our token, and post install status
		if getStoken() then
			If SendInstalled() Then 'Must be in a separate if statement because we don't want to call this if getSToken failed
				bEnabled=True 
				DoInit=True
				RunAsync=True
				Scorbit_updateQR
			End If
		End If
	End Function 

	Public Sub Callback()
		Dim ResponseStr
		Dim i 
		Dim Parts
		Dim Parts2
		Dim Parts3
		if bEnabled=False then Exit Sub 'DO NOT REMOVE

		if bWaitResp and objXmlHttpMain.readystate=4 then 
			Scorbit_Debug "Callback data received: " & objXmlHttpMain.Status & " " & objXmlHttpMain.readystate
			if objXmlHttpMain.Status=200 and objXmlHttpMain.readystate = 4 then 
				ResponseStr=objXmlHttpMain.responseText
				Scorbit_Debug "---response: " & ResponseStr

				'Check player names / claiming
				HandlePlayerClaimResp ResponseStr

				'Check heartbeat
				HandleHeartbeatResp ResponseStr
			End if 
			bWaitResp=False
		End if 
	End Sub

	Public Sub StartSession()
		if bEnabled=False then Exit Sub 'DO NOT REMOVE
		if bNeedsPairing=True Then Exit Sub 'DO NOT REMOVE
		if bSessionActive=True Then Exit Sub 'DO NOT REMOVE

		Scorbit_Debug "Starting new session..."

		Dim i
		For i = 1 to 4
			CachedPlayer.Item(i & "_name") = ""
			CachedPlayer.Item(i & "_initials") = ""
			CachedPlayer.Item(i & "_prefer_initials") = False
		Next
		bActive="true"
		bSessionActive=True
		SessionSeq=0
		SessionUUID=GUID()
		SessionTimeStart=GameTime
		LogFile.RemoveAll
		LogIdx=0
		SendUpdate 0, 0, 0, 0, 1, 1, 1

		Scorbit_Debug "Started session " & SessionUUID
	End Sub 

	Public Sub StopSession(P1Score, P2Score, P3Score, P4Score, NumberPlayers)
		StopSession2 P1Score, P2Score, P3Score, P4Score, NumberPlayers, False
	End Sub 

	Public Sub StopSession2(P1Score, P2Score, P3Score, P4Score, NumberPlayers, bCancel)
		Dim i
		dim objFile

		if bEnabled=False then Exit Sub 'DO NOT REMOVE
		if bNeedsPairing=True Then Exit Sub 'DO NOT REMOVE
		if bSessionActive=False then Exit Sub 'DO NOT REMOVE

		Scorbit_Debug "Stopping session..."

		bActive="false" 
		SendUpdate P1Score, P2Score, P3Score, P4Score, -1, -1, NumberPlayers
		'SendUpdate P1Score, P2Score, P3Score, P4Score, , , NumberPlayers
		bSessionActive=False
'		SendHeartbeat

		if bUploadLog and LogIdx<>0 and bCancel=False then 
			Scorbit_Debug "Creating Scorbit Log: Size " & LogIdx
			Scorbit_LOGUpload(0)
			Set objFile = fso.CreateTextFile(dirScorbitBin & "\sGameLog_" & MachineID & ".csv")
			For i = 0 to LogIdx-1 
				objFile.Writeline LogFile.Item(i)
			Next 
			objFile.Close
			LogIdx=0
			LogFile.RemoveAll
			Scorbit_LOGUpload(1)
			pvPostFile "https://" & domain & "/api/session_log/", dirScorbitBin & "\sGameLog_" & MachineID & ".csv", False
			Scorbit_LOGUpload(2)
			on error resume next
			'fso.DeleteFile(dirScorbitBin & "\sGameLog_" & MachineID & ".csv")
			on error goto 0
		End if
		Scorbit_Debug "Stopped session"
	End Sub 

	Public Sub SetGameMode(GameModeStr)
		GameModeOrig=GameModeStr
		GameMode=GameModeStr
		GameMode=Replace(GameMode, ":", "%3a")
		GameMode=Replace(GameMode, ";", "%3b")
		GameMode=Replace(GameMode, " ", "%20")
		GameMode=Replace(GameMode, "{", "%7B")
		GameMode=Replace(GameMode, "}", "%7D")
	End sub 

	Public Sub SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers)
		SendUpdateAsynch P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers, RunAsync
	End Sub 

	Public Sub SendUpdateAsynch(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers, bAsynch)
		dim i
		Dim PostData
		Dim resultStr
		dim LogScores(4)

		if bUploadLog then
			Dbg "LOG:" &NumberPlayers
			if NumberPlayers>=1 then LogScores(0)=P1Score
			if NumberPlayers>=2 then LogScores(1)=P2Score
			if NumberPlayers>=3 then LogScores(2)=P3Score
			if NumberPlayers>=4 then LogScores(3)=P4Score
			LogFile.Add LogIdx, DateDiff("S", "1/1/1970", Now()) & "," & LogScores(0) & "," & LogScores(1) & "," & LogScores(2) & "," & LogScores(3) & ",,," &  CurrentPlayer & "," & CurrentBall & ",""" & GameModeOrig & """"
			LogIdx=LogIdx+1
		End if 

		if bSessionActive=False then Exit Sub 'DO NOT REMOVE
		if bEnabled=False then Exit Sub 'DO NOT REMOVE
		if bWaitResp then exit sub ' Drop message until we get our next response 

		SaveCurrentPlayer=CurrentPlayer
'		PostData = "session_uuid=" & SessionUUID & "&session_time=" & DateDiff("S", "1/1/1970", Now()) & _
'					"&session_sequence=" & SessionSeq & "&active=" & bActive
		PostData = "session_uuid=" & SessionUUID & "&session_time=" & GameTime-SessionTimeStart+1 & _
					"&session_sequence=" & SessionSeq & "&active=" & bActive

		SessionSeq=SessionSeq+1
		if NumberPlayers > 0 then 
			for i = 0 to NumberPlayers-1
				PostData = PostData & "&current_p" & i+1 & "_score="
				if i <= NumberPlayers-1 then 
					if i = 0 then PostData = PostData & P1Score
					if i = 1 then PostData = PostData & P2Score
					if i = 2 then PostData = PostData & P3Score
					if i = 3 then PostData = PostData & P4Score
				else 
					PostData = PostData & "-1"
				End if 
			Next 

			PostData = PostData & "&current_ball=" & CurrentBall & "&current_player=" & CurrentPlayer
			if GameMode<>"" then PostData=PostData & "&game_modes=" & GameMode
		End if 
		resultStr = PostMsg("https://" & domain, "/api/entry/", PostData, bAsynch)
		if resultStr<>"" then Scorbit_Debug "SendUpdate Resp: " & resultStr
	End Sub

	' Getter-only properties
	Public Property Get Enabled()
		Enabled = bEnabled
	End Property
	Public Property Get SessionActive()
		SessionActive = bSessionActive
	End Property
	Public Property Get NeedsPairing()
		NeedsPairing = bNeedsPairing
	End Property

' PRIVATE BELOW 
	Private Function LPad(StringToPad, Length, CharacterToPad)
	  Dim x : x = 0
	  If Length > Len(StringToPad) Then x = Length - len(StringToPad)
	  LPad = String(x, CharacterToPad) & StringToPad
	End Function

	Private Function GUID()		
		Dim TypeLib
		Set TypeLib = CreateObject("Scriptlet.TypeLib")
		GUID = Mid(TypeLib.Guid, 2, 36)

'		Set wsh = CreateObject("WScript.Shell")
'		Set fso = CreateObject("Scripting.FileSystemObject")
'
'		dim rc
'		dim result
'		dim objFileToRead
'		Dim sessionID:sessionID=dirScorbitBin & "\sessionID_" & MachineID & ".txt"
'
'		on error resume next
'		fso.DeleteFile(sessionID)
'		On error goto 0 
'
'		rc = wsh.Run("powershell -Command ""(New-Guid).Guid"" | out-file -encoding ascii " & sessionID, 0, True)
'		if FileExists(sessionID) and rc=0 then
'			Set objFileToRead = fso.OpenTextFile(sessionID,1)
'			result = objFileToRead.ReadLine()
'			objFileToRead.Close
'			GUID=result
'		else 
'			MsgBox "Cant Create SessionUUID through powershell. Disabling Scorbit"
'			bEnabled=False 
'		End if

	End Function

	Private Function GetJSONValue(JSONStr, key)
		dim i 
		Dim tmpStrs,tmpStrs2
		GetJSONValue=""
		if Instr(1, JSONStr, key)<>0 then 
			tmpStrs=split(JSONStr,",")
			for i = 0 to ubound(tmpStrs)
				if instr(1, tmpStrs(i), key)<>0 then 
					tmpStrs2=split(tmpStrs(i),":")
					GetJSONValue=tmpStrs2(1)
					exit for
				End if 
			Next
		End If
	End Function

	Private Sub HandlePlayerClaimResp(ResponseStr)
		If bEnabled = False Then Exit Sub 'DO NOT REMOVE
        Dim Parts, Parts2

		'Parse Names if present
		If bSessionActive = True Then
			if CachedPlayer.Item(SaveCurrentPlayer & "_name")="" then  ' Player doesnt have a name
				if instr(1, ResponseStr, "cached_display_name") <> 0 Then	' There are names in the result
					Parts=Split(ResponseStr,",{")							' split it 
					if ubound(Parts)>=SaveCurrentPlayer-1 then 				' Make sure they are enough avail
						if instr(1, Parts(SaveCurrentPlayer-1), "cached_display_name")<>0 then 	' See if mine has a name 

							'Grab full Scorbit name
							CachedPlayer.Item(SaveCurrentPlayer & "_name")=GetJSONValue(Parts(SaveCurrentPlayer-1), "cached_display_name")	'Get my name
							CachedPlayer.Item(SaveCurrentPlayer & "_name")=Replace(CachedPlayer.Item(SaveCurrentPlayer & "_name"), """", "")
							Scorbit_Debug "Found name for player " & SaveCurrentPlayer & ": " & CachedPlayer.Item(SaveCurrentPlayer & "_name")

							'Do we also have initials?
							if instr(1, Parts(SaveCurrentPlayer-1), "initials")<>0 Then
								CachedPlayer.Item(SaveCurrentPlayer & "_initials")=GetJSONValue(Parts(SaveCurrentPlayer-1), "initials")	'Get my initials
								CachedPlayer.Item(SaveCurrentPlayer & "_initials")=Replace(CachedPlayer.Item(SaveCurrentPlayer & "_initials"), """", "")
								Scorbit_Debug "Found initials for player " & SaveCurrentPlayer & ": " & CachedPlayer.Item(SaveCurrentPlayer & "_initials")
							End If

							'Does the player prefer initials over name?
							if instr(1, Parts(SaveCurrentPlayer-1), """prefer_initials"":true")<>0 Then
								CachedPlayer.Item(SaveCurrentPlayer & "_prefer_initials")=True
								Scorbit_Debug "Player " & SaveCurrentPlayer & " prefers initials."
							Else
								CachedPlayer.Item(SaveCurrentPlayer & "_prefer_initials")=False
							End If

							Scorbit_PlayerClaimed SaveCurrentPlayer, GetNameOrInitials(SaveCurrentPlayer)
							Scorbit_Debug "Player Claim: " & SaveCurrentPlayer & " " & GetNameOrInitials(SaveCurrentPlayer)
						End if 
					End if
				End if 
			else												    ' Check for unclaim 
				if instr(1, ResponseStr, """player"":null")<>0 Then	' Someone doesnt have a name
					Parts=Split(ResponseStr,"[")						' split it 
					Scorbit_Debug "Parts: " & Parts(1)

					Parts2=Split(Parts(1),"}")							' split it 
					for i = 0 to Ubound(Parts2)
					    Scorbit_Debug "Parts2: " & Parts2(i)

						if instr(1, Parts2(i), """player"":null")<>0 Then
							Scorbit_Debug "Player " & (i+1) & " is not claimed or does not have a name"
							CachedPlayer.Item((i+1) & "_name") = ""
							CachedPlayer.Item((i+1) & "_initials") = ""
							CachedPlayer.Item((i+1) & "_prefer_initials") = False
						End if 
					Next 
				End if 
			End if
		End If
	End Sub

	Private Sub SendHeartbeat()
		Dim resultStr
		if bEnabled=False then Exit Sub 'DO NOT REMOVE

		resultStr = GetMsgHdr("https://" & domain, "/api/heartbeat/", "Authorization", "SToken " & sToken)
		
		If RunAsync = False Then 'Should never happen; heartbeat should normally always be async
			Scorbit_Debug "Heartbeat Resp: " & resultStr
			HandleHeartbeatResp ResultStr
		End If
	End Sub 

	Private Sub HandleHeartbeatResp(resultStr)
		dim TmpStr
		Dim Command
		Dim rc
		Dim QRFile:QRFile=dirQrCode

		If VenueMachineID="" then
			'Parse pairing status
			if resultStr<>"" and Instr(resultStr, """unpaired"":true")=0 then 'We Paired
				Scorbit_Debug "Heartbeat: We are paired"
				bNeedsPairing=False
				Scorbit_Paired()
			else 
				Scorbit_Debug "Heartbeat: We are not paired"
				bNeedsPairing=True
				Scorbit_updateQR
			End if 

			'Parse venue machine ID and generate a claim QR code
			TmpStr=GetJSONValue(resultStr, "venuemachine_id")
			if TmpStr<>"" then 
				VenueMachineID=TmpStr		
				Command = """" & dirScorbitBin & "\sQRCode.exe"" " & VenueMachineID & " " & opdbID & " """ & QRFile & """"
				rc = wsh.Run(Command, 0, False)
				LoadTexture "ScorbitQRClaim", QRFile & "\QRclaim.bmp"
				Scorbit_updateQR
				Scorbit_Debug "Heartbeat: Received venuemachine_id and generated QR claim code"
			End if
		End if
	End Sub

	Private Function SendInstalled()
		SendInstalled = False

		Dim installType
		'installType = "vpin" 'Does not work yet
		installType = "score_detector"
		Dim installVersion
		'installVersion = "2.0" 'version of the SDK
		installVersion = TableVersion

		Dim resultStr
		Dim postData

		PostData = "type=" & installType & "&version=" & installVersion & "&installed=true"
		resultStr = PostMsg("https://" & domain, "/api/installed/", PostData, False)
		Scorbit_Debug "POSTed installed Resp: " & resultStr

		Dim msgKey
		msgKey = GetJSONValue(resultStr, "msg")
		if msgKey = "" Then
			bEnabled=False
			msgBox "Scorbit - non-successful response in SendInstalled(). Scorbit disabled."
			Scorbit_updateQR
		Else
			SendInstalled = True
		End If
	End Function

	Private Function getStoken()
		Dim result
		Dim results
'		dim wsh
		Dim tmpUUID:tmpUUID="adc12b19a3504453a7414e722f58736b"
		Dim tmpVendor:tmpVendor="vscorbitron"
		Dim tmpSerial:tmpSerial="999990104"
		Dim QRFile:QRFile=dirQrCode
		Dim sTokenFile:sTokenFile=dirScorbitBin & "\sToken_" & MachineID & ".dat"

		' Set everything up
		tmpUUID=MyUUID
		tmpVendor="vpin"
		tmpSerial=Serial
		
		on error resume next
		fso.DeleteFile(sTokenFile)
		On error goto 0 

		' get sToken and generate QRCode
'		Set wsh = CreateObject("WScript.Shell")
		Dim waitOnReturn: waitOnReturn = True
		Dim windowStyle: windowStyle = 0
		Dim Command 
		Dim rc
		Dim objFileToRead

		Command = """" & dirScorbitBin & "\sToken.exe"" " & tmpUUID & " " & tmpVendor & " " &  tmpSerial & " " & MachineID & " """ & QRFile & """ """ & sTokenFile & """ " & domain
		Scorbit_Debug "RUNNING Command:" & Command
		rc = wsh.Run(Command, windowStyle, waitOnReturn)
		Scorbit_Debug "Return:" & rc
		if FileExists(sTokenFile) and rc=0 then
			Set objFileToRead = fso.OpenTextFile(sTokenFile,1)
			result = objFileToRead.ReadLine()
			objFileToRead.Close
			Set objFileToRead = Nothing
			Scorbit_Debug "Token auth: " & result

			if Instr(1, result, "Invalid timestamp")<> 0 then 
				MsgBox "Scorbit Timestamp Error: Please make sure the time on your system is exact"
				getStoken=False
			elseif Instr(1, result, ":")<>0 then 
				results=split(result, ":")
				sToken=results(1)
				sToken=mid(sToken, 3, len(sToken)-4)
				Scorbit_Debug "Got TOKEN: " & sToken
				LoadTexture "ScorbitQRCode", QRFile & "\QRcode.bmp"
				Scorbit_updateQR
				getStoken=True
			Else 
				Scorbit_Debug "Token ERROR: " & result
				getStoken=False
			End if 
		else 
			Scorbit_Debug "Token ERROR No File: " & rc
		End if 

	End Function 

	private Function FileExists(FilePath)
		If fso.FileExists(FilePath) Then
			FileExists=CBool(1)
		Else
			FileExists=CBool(0)
		End If
	End Function

	Private Function GetMsg(URLBase, endpoint)
		GetMsg = GetMsgHdr(URLBase, endpoint, "", "")
	End Function

	Private Function GetMsgHdr(URLBase, endpoint, Hdr1, Hdr1Val)
		Dim Url
		Url = URLBase + endpoint & "?session_active=" & bActive
		Scorbit_Debug "Running GetMsgHdr: Url: " & Url  & "  Async=" & RunAsync
		objXmlHttpMain.open "GET", Url, RunAsync
'		objXmlHttpMain.setRequestHeader "Content-Type", "text/xml"
		objXmlHttpMain.setRequestHeader "Cache-Control", "no-cache"
		if Hdr1<> "" then objXmlHttpMain.setRequestHeader Hdr1, Hdr1Val

'		on error resume next
			err.clear
			objXmlHttpMain.send ""
			if err.number=-2147012867 then 
				MsgBox "Scorbit - Multiplayer Server is down; Scorbit disabled. To restart Scorbit, open and close the Tweak UI (without changing anything)."
				bEnabled=False
				Scorbit_updateQR
			elseif err.number <> 0 then 
				Scorbit_Debug "Server error: (" & err.number & ") " & Err.Description
			End if 
			if RunAsync=False then 
				Scorbit_Debug "Status: " & objXmlHttpMain.status
				If objXmlHttpMain.status = 200 Then
					GetMsgHdr = objXmlHttpMain.responseText
				Else 
					GetMsgHdr=""
				End if 
			Else 
				bWaitResp=True
				GetMsgHdr=""
			End if 
'		On error goto 0

	End Function

	Private Function PostMsg(URLBase, endpoint, PostData, bAsynch)
		Dim Url

		Url = URLBase + endpoint
		Scorbit_Debug "Running PostMSg: " & Url & " " & PostData

		objXmlHttpMain.open "POST",Url, bAsynch
		objXmlHttpMain.setRequestHeader "Content-Type", "application/x-www-form-urlencoded"
		objXmlHttpMain.setRequestHeader "Content-Length", Len(PostData)
		objXmlHttpMain.setRequestHeader "Cache-Control", "no-cache"
		objXmlHttpMain.setRequestHeader "Authorization", "SToken " & sToken
		if bAsynch then bWaitResp=True 

		on error resume next
			objXmlHttpMain.send PostData
			if err.number=-2147012867 then 
				MsgBox "Scorbit - Multiplayer Server is down; Scorbit disabled. To restart Scorbit, open and close the Tweak UI (without changing anything)."
				bEnabled=False
				Scorbit_updateQR
			elseif err.number <> 0 then 
				Scorbit_Debug "Multiplayer Server error (" & err.number & ") " & Err.Description
			End if 
			If objXmlHttpMain.status = 200 Then
				PostMsg = objXmlHttpMain.responseText
			else 
				PostMsg="ERROR: " & objXmlHttpMain.status & " >" & objXmlHttpMain.responseText & "<"
			End if 
		On error goto 0
	End Function

	Private Function pvPostFile(sUrl, sFileName, bAsync)
		Scorbit_Debug "Posting File " & sUrl & " " & sFileName & " " & bAsync & " File: " & Mid(sFileName, InStrRev(sFileName, "\") + 1)
		Dim STR_BOUNDARY:STR_BOUNDARY  = GUID()
		Dim nFile  
		Dim baBuffer()
		Dim sPostData
		Dim Response

		'--- read file
		Set nFile = fso.GetFile(sFileName)
		With nFile.OpenAsTextStream()
			sPostData = .Read(nFile.Size)
			.Close
		End With
'		fso.Open sFileName For Binary Access Read As nFile
'		If LOF(nFile) > 0 Then
'			ReDim baBuffer(0 To LOF(nFile) - 1) As Byte
'			Get nFile, , baBuffer
'			sPostData = StrConv(baBuffer, vbUnicode)
'		End If
'		Close nFile

		'--- prepare body
		sPostData = "--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""uuid""" & vbCrLf & vbCrLf & _
			SessionUUID & vbcrlf & _
			"--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""log_file""; filename=""" & SessionUUID & ".csv""" & vbCrLf & _
			"Content-Type: application/octet-stream" & vbCrLf & vbCrLf & _
			sPostData & vbCrLf & _
			"--" & STR_BOUNDARY & "--"

		Scorbit_Debug "POSTDATA: " & sPostData & vbcrlf

		'--- post
		With objXmlHttpMain
			.Open "POST", sUrl, bAsync
			.SetRequestHeader "Content-Type", "multipart/form-data; boundary=" & STR_BOUNDARY
			.SetRequestHeader "Authorization", "SToken " & sToken
			.Send sPostData ' pvToByteArray(sPostData)
			If Not bAsync Then
				Response= .ResponseText
				pvPostFile = Response
				Scorbit_Debug "Upload Response: " & Response
			End If
		End With

	End Function

	Private Function pvToByteArray(sText)
		pvToByteArray = StrConv(sText, 128)		' vbFromUnicode
	End Function
	
	Private Function CountLetters(strLetter, strInput)
	    Dim i, letterCount, strPosition
	    i = 0
	    letterCount = 0
	    strPosition = 0
	    
	    Do While i < Len(strInput)
	        i = i + 1
	        strPosition = InStr(i, strInput, strLetter)
	        If strPosition > 0 Then
	        	letterCount = letterCount + 1
	        	i = strPosition
	        End If
	    Loop
	    
	    CountLetters = letterCount
	End Function

End Class 
'  END SCORBIT 
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Sub DelayQRClaim_Timer()
	if bOnTheFirstBall AND bBallInPlungerLane then Scorbit_updateQR
	DelayQRClaim.Enabled=False
End Sub

Sub DisableModels(Opt)
BABA.visible = 0
BABA001.visible = 0

CatL.Visible = 0
CatR.Visible = 0

Primitive055.Visible = 0
Primitive066.Visible = 0
Primitive071.Visible = 0
Primitive081.Visible = 0

Primitive082.Visible = 0
Primitive084.Visible = 0
Primitive087.Visible = 0

WATL.Visible = 0
WATL2.visible = 0
WATL001.visible = 0

Primitive104.visible = 0

End Sub

Sub CheckTrustPost
	if RemoveTrustPost Then
		TrustPost.Visible = 0
		TrustPost.Collidable = 0
		TrustPostRubber.visible = 0
	Else
		TrustPost.Visible = 1
		TrustPost.Collidable = 1
		TrustPostRubber.visible = 1
	End If
End Sub

Sub SetTrustPost(Opt)
	Select Case Opt
		Case 0:
			RemoveTrustPost = 0
			CheckTrustPost
		Case 1:
			RemoveTrustPost = 1
			CheckTrustPost
		End Select
End Sub

Sub UpdateLeftOutlanePosts(Opt)
	Select Case Opt
		Case 0
			zCol_Rubber_Post007.y = 1414.95
			Primitive035.y = 1414.95
			RubberOutlaneLeftEasy.visible = True
			RubberOutlaneLeftMed.visible = False
			RubberOutlaneLeftHard.visible = False
		Case 1
			zCol_Rubber_Post007.y = 1416.95
			Primitive035.y = 1416.95
			RubberOutlaneLeftEasy.visible = False
			RubberOutlaneLeftMed.visible = True
			RubberOutlaneLeftHard.visible = False
		Case 2
			zCol_Rubber_Post007.y = 1418.95
			Primitive035.y = 1418.95
			RubberOutlaneLeftEasy.visible = False
			RubberOutlaneLeftMed.visible = False
			RubberOutlaneLeftHard.visible = True
	End Select
End Sub

Sub UpdateRightOutlanePosts(Opt)
	Select Case Opt
		Case 0
			zCol_Rubber_Post035.y = 1415.14
			Primitive036.y = 1415.14
			RubberOutlaneRightEasy.visible = True
			RubberOutlaneRightMed.visible = False
			RubberOutlaneRightHard.visible = False
		Case 1
			zCol_Rubber_Post035.y = 1417.14
			Primitive036.y = 1417.14
			RubberOutlaneRightEasy.visible = False
			RubberOutlaneRightMed.visible = True
			RubberOutlaneRightHard.visible = False
		Case 2
			zCol_Rubber_Post035.y = 1419.14
			Primitive036.y = 1419.14
			RubberOutlaneRightEasy.visible = False
			RubberOutlaneRightMed.visible = False
			RubberOutlaneRightHard.visible = True
	End Select
End Sub

Sub SetBallsPerGame(Opt)
	Select Case Opt
		Case 0: BallsPerGame = 3
		Case 1:	BallsPerGame = 4
		Case 2:	BallsPerGame = 5
	End Select
End Sub

'**********************************************************************************************************
' VR Plunger
'**********************************************************************************************************

Sub TimerVRPlunger_Timer
	If PinCab_Shooter.Y < 100 then
		PinCab_Shooter.Y = PinCab_Shooter.Y + 5
	End If
End Sub

Sub TimerVRPlunger2_Timer
	PinCab_Shooter.Y = 0 + (5* Plunger.Position)
End Sub
'
'
'
'*************************************************************************************************************************************************
'*************************************************************************************************************************************************
'
' DOF Config by VPCLE
'
' E103 ADD CREDIT
' E104 NOT ENOUGH CREDITS
' E105 BALL RELEASE
' E106 L SLING
' E107 R SLING
' E108 1 BUMPER
' E109 2 BUMPER
' E110 TRIGGER001
' E111 TRIGGER002
' E112 TRIGGER003
' E113 TRIGGER004
' E114 flipper LEFT
' E115 flipper RIGHT
' E116  L  TRIGGER006
' E117  I  TRIGGER007
' E118  Z  TRIGGER008
' E121 HELLBOY GATE
' E119 D LEFT ORBIT
' E122 E TRIGGER009
' E123 M TRIGGER013
' E124 O TRIGGER011
' E125 N RIGHT ORBIT
' E126 TRIGGER010
' E127 TARGET001
' E128 TARGET004
' E129 TARGET002
' E130 TARGET003
' E131 TARGET005
' E132 RUBBERBAND007
' E133 RUBBERBAND005
' E134 TARGET006
' E135 TARGET007
' E136 SPINNER LEFT
' E137 SPINNER RIGHT
' E138 BALL 1 LOCK
' E139 BALL 2 LOCK
' E140 BALL 3 LOCK
' E141 TOMB1
' E142 TOMB2
' E143 TOMB3
' E144 ESCAPEHW1
' E145 ESCAPEHW2
' E146 PENNYWISE MB
' E147 GARMY1
' E148 GARMY2
' E149 Hellboy MB
' E151 RAMP CENTER ENTER
' E152 RAMP CENTER EXIT
' E153 NOT USED
' E154 RAMP LEFT ENTER
' E155 RAMP LEFT EXIT
' E156 RAMP RIGHT EXIT
' E157 RAMP RIGHT ENTER
' E158 NOT USED
' E159 GATE KICKER DOWNWARD
' E160 ARCING STROBE
' E150 MBTENT 1
' E161 MBTENT 2
' E162 MBTENT 3
' E163 MBTENT 4
' E164 MBTENT 5
' E170 Attract
' E171 ball ready
' E172 Drain
' E173 Ball saved
' E174 GI On
' E175 GI RED On
' E176 ball launch
' E180 ALL LIZ
' E181 START KNOCKER
'
'
'*************************************************************************************************************************************************
'*************************************************************************************************************************************************
'