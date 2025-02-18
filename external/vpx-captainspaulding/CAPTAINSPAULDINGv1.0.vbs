' ****************************************************************
' ****************************************************************
'            CAPTAIN SPAULDING VISUAL PINBALL X 10.7
' ****************************************************************

Option Explicit
Randomize

'*******************************************
'  ZOPT: User Options
'*******************************************

Const VRRoomChoice = 0			  
Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.6   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.8 		' Level of ramp rolling volume. Value between 0 and 1
Dim StagedFlippers : StagedFlippers = 0         ' Staged Flippers. 0 = Disabled, 1 = Enabled

'♥♡♥♡♥♡♥♡♥♡♥♡♥♡♥ PLayer choices ♥♡♥♡♥♡♥♡♥♡♥♡♥♡♥

Const SongVolume = 0.1     ' 1 is full volume, but I set it quite low to listen better the other sounds since I use headphones, adjust to your setup :)
Const FlippersBlood = True 'set it to false if you don't like that. In this version 4 I moved the blood to the slingshots
Const FlexDMDHighQuality = False 'FlexDMD in high quality (True = LCD at 256x64) or normal quality (False = Real DMD at 128x32)

'************************
'Glowball
'*************************
Dim GlowAura,GlowIntensity
GlowAura=120 'GlowBlob Auroa radius
GlowIntensity=10 'Glowblob intensity

Const ChooseBall 			= 8	' *** Ball Settings **********
									' *** 0 = Normal Ball	
									' *** 1 = Purple GlowBall
									' *** 2 = Green GlowBall																		
									' *** 3 = Blue Glowball
									' *** 4 = Orange Glowball 
									' *** 5 = Red Glowball
									' *** 6 = White Glowball
									' *** 7 = Yellow Glowball
									' *** 8 = Gold Glowball

'♥♡♥♡♥♡♥♡♥♡♥♡♥ End of pLayer choices ♥♡♥♡♥♡♥♡♥♡♥♡♥

'*******************************************
'  ZCON: Constants and Global Variables
'*******************************************

Const UsingROM = False			    'The UsingROM flag is to indicate code that requires ROM usage. Mostly for instructional purposes only.
Const BallSize = 50				    'Ball diameter in VPX units; must be 50
Const BallMass = 1				    'Ball mass must be 1
Const tnob = 19					    'Total number of balls the table can hold
Const lob = 1					    'Locked balls
Const cGameName = "CAPTAINSPAULDING1"'The unique alphanumeric name for this table

'Detect if VPX is rendering in VR and then make sure the VR Room Chioce is used
Dim VRRoom
If RenderingMode = 2 Then
	VRRoom = VRRoomChoice
Else
	VRRoom = 0
End If

Dim tablewidth
tablewidth = Table1.width
Dim tableheight
tableheight = Table1.height
Dim BIP							 'Balls in play
BIP = 0
Dim BIPL							'Ball in plunger lane
BIPL = False

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
Const myVersion = "1.0"
Const MaxPlayers = 4          ' from 1 to 4
Const BallSaverTime = 20    ' in seconds of the first ball
Const MaxMultiplier = 5      ' limit playfield multiplier
Const MaxBonusMultiplier = 5 'limit Bonus multiplier
Const BallsPerGame = 3       ' usually 3 or 5
Const MaxMultiballs = 13     ' max number of balls during multiballs

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If

'*************************************************************
' VR Room Auto-Detect
'*************************************************************
Const LiveViewVRSim = 0' 0 = Default, 1 = View table in VR mode in "Live View Editor" 

' VR uses FlexDMD to display DMD.
Dim VR_Obj, VRMode

If RenderingMode = 2 or LiveViewVRSim = 1 Then
	VRMode = True
	lrail.Visible = 0
	rrail.Visible = 0
	ramp17.Visible = 0
    primitive060.visible = 0
	Pincab_Backglass.BlendDisableLighting = 1
	For Each VR_Obj in VRCabinet:VR_Obj.Visible = 1:Next
	For Each VR_Obj in VR_Room:VR_Obj.Visible = 1:Next
Else
	For Each VR_Obj in VRCabinet:VR_Obj.Visible = 0:Next
	For Each VR_Obj in VR_Room:VR_Obj.Visible = 0:Next
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

dim countr
countr = 0
FireTimer.enabled = 1

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
	DoSTAnim				'handle stand up target animations
End Sub

'The CorTimer interval should be 10. It's sole purpose is to update the Cor calculations
CorTimer.Interval = 10
Sub CorTimer_Timer(): Cor.Update: End Sub

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
    bFreePlay = True'we want coins

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
    StopSPAL
    ' set any lights for the attract mode
    GiOff
    StartAttractMode

    ' Start the RealTime timer
    RealTime.Enabled = 1

    ' Load table color
    LoadLut

    Glowball_Init 'Start Glowballs
End Sub

'==================================================================================================================================
' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reset
' - 3: player closed the tweak UI, good time to update staticly prerendered parts

' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings

Dim LeftOutlaneDifficulty,RightOutlaneDifficulty
Sub Table1_OptionEvent(ByVal eventId)

	'Difficulty
	RemoveTrustPost = Table1.Option("Remove Trust Post", 0, 1, 1, 1, 0, Array("False", "True (Default)"))
	CheckTrustPost 

	'Outlane Difficulty
	LeftOutlaneDifficulty = Table1.Option("Left Outlane Difficulty", 0, 2, 1, 1, 0, Array("Easy", "Medium (Default)", "Hard"))
	UpdateLeftOutlanePosts LeftOutlaneDifficulty

	RightOutlaneDifficulty = Table1.Option("Right Outlane Difficulty", 0, 2, 1, 1, 0, Array("Easy", "Medium (Default)", "Hard"))
	UpdateRightOutlanePosts RightOutlaneDifficulty

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

    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If

	If keycode = AddCreditKey Or keycode = AddCreditKey2 Then
		Select Case Int(Rnd * 3)
			Case 0
				PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1
				PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2
				PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
        Credits = Credits + 1
        if bFreePlay = False Then DOF 125, DOFOn
		' xCFx
		DOF 301, DOFPulse
		' ----
        If(Tilted = False) Then
            DMDFlush
            DMD "", CL("CREDITS " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
            If NOT bGameInPlay Then ShowTableInfo
        
        End If
    End If
	If keycode = StartGameKey Then
		SoundStartButton
		' xCFx
		DOF 300, DOFPulse
		' ----
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

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then CheckTilt 'only check the tilt during game
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 1:PinCab_Left_Flipper_Button.X = PinCab_Left_Flipper_Button.X + 10
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 0:PinCab_Right_Flipper_Button.X = PinCab_Right_Flipper_Button.X - 10

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

    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT

    If keycode = PlungerKey Then
        Plunger.Fire
        If BIPL = 1 Then
			SoundPlungerReleaseBall()   'Plunger release sound when there is a ball in shooter lane
			' xCFx
			'DOF 306, DOFPulse
			'Debug.print "DOF 306 BIPL=1"
			' ---
		Else
			SoundPlungerReleaseNoBall() 'Plunger release sound when there is no ball in shooter lane
			' xCFx
			DOF 306, DOFPulse
			'Debug.print "DOF 306 BIPL<>1"
			' ---
		End If

		TimerVRPlunger.Enabled = False
		TimerVRPlunger2.Enabled = True
		Pincab_Shooter.Y = 0
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
            InstantInfoTimer.Enabled = False
			PinCab_Left_Flipper_Button.X = PinCab_Left_Flipper_Button.X - 10
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
            InstantInfoTimer.Enabled = False
			PinCab_Right_Flipper_Button.X = PinCab_Right_Flipper_Button.X + 10
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

'*******************************************
'	ZFLP: Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled) 'Left flipper solenoid callback
	If Enabled Then
		FlipperActivate LeftFlipper, LFPress
		LF.Fire  'leftflipper.rotatetoend
		LeftFlipper001.RotateToEnd
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper
		Else
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
	Else
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart
		LeftFlipper001.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled) 'Right flipper solenoid callback
	If Enabled Then
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

Sub LeftFlipper001_Collide(parm)
    LeftFlipperCollide parm
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

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

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
        PlaySong "mu_multiball2"
    ElseIf bMichaelMBStarted Then
        PlaySong "mu_multiball3"
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

Sub PlayQuote 'Captain Spaulding
    PlaySound "vo_Spaulding" &RndNbr(70)
End Sub

Sub PlayHighScoreQuote
    Select Case RndNbr(3)
        Case 1:PlaySound ""
        Case 2:PlaySound ""
        Case 3:PlaySound ""
    End Select
End Sub

Sub PlayNotGoodScore
    Select Case RndNbr(2)
        Case 1:PlaySound "vo_thatwasprettybad"
        Case 2:PlaySound "vo_thatwasprettybad2"
    End Select
End Sub

Sub PlayEndQuote
    Select Case RndNbr(9)
        Case 1:PlaySound "vo_hahaha1"
        Case 2:PlaySound "vo_hahaha2"
        Case 3:PlaySound "vo_hahaha3"
        Case 4:PlaySound "vo_hahaha4"
        
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

'extra collections in this table
Sub aBlueRubbers_Hit(idx)
    Select Case RndNbr(15)
        Case 1:Playsound "vo_hahaha1"
        Case 2:Playsound "vo_hahaha2"
        Case 3:Playsound "vo_hahaha3"
        Case 4:Playsound "vo_hahaha1"
        Case 5:Playsound "vo_hahaha2"
        Case 6:Playsound "vo_hahaha3"
        Case 7:Playsound "vo_hahaha1"
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
    RandomSoundBallRelease BallRelease
	BIP = BIP + 1
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
        DMD CL("BODYPARTS COLLECTED" & Weapons(CurrentPlayer) ), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, "mu_bonus"

        'Counselors killed X 750.000
        AwardPoints = CounselorsKilled(CurrentPlayer) * 750000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("PEOPLE KILLED"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, "mu_bonus"

        'Teenagers killed x 300.000
        AwardPoints = TeensKilled(CurrentPlayer) * 300000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("TEENAGERS KILLED"), CL(FormatScore(AwardPoints) ), "", eNone, eNone, eNone, 800, True, "mu_bonus"

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
            DMD CL("TOTAL BONUS X MULT"), CL(FormatScore(TotalBonus * BonusMultiplier(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, True, "vo_worst"&RndNbr(3)
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
    RandomSoundDrain Drain
	BIP = BIP - 1
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
			' xCFx
			DOF 305, DOFPulse
			' ----
            End If
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1) Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True) then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point
                    ' turn off any multiball specific lights
                    ChangeGi white
                    ChangeGIIntensity 1
                    'stop any multiball modes of this game
                    StopMBmodes
                    changesong
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
			' xCFx
			DOF 304, DOFPulse
			' ----
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
	' xCFx
	If (bMultiBallMode=False) then 
		DOF 307, DOFOn
	End If
	' ----
    bBallInPlungerLane = True
	BIPL = True
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
	BIPL = False
    DOF 208, DOFOff
	' xCFx
	If (bMultiBallMode=False) then 
		DOF 307, DOFOff
	End If
	' ----

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
            Case 1:DMD CL("BACK FOR SOME"), CL("MORE TORTURE"), "_", eNone, eNone, eNone, 2000, True, "vo_backforsomemoretorture"
            Case 2:DMD CL("ARE YOU PLAYING"), CL("THIS GAME"), "_", eNone, eNone, eNone, 2000, True, "vo_areyouplayingthisgame"
            Case 3:DMD CL("WELCOME"), CL("TO HELL"), "_", eNone, eNone, eNone, 2000, True, "vo_welcometohell"
        End Select
    Else
        Select Case RndNbr(1)
            Case 1:DMD CL("ARE YOU PLAYING"), CL("THIS GAME"), "_", eNone, eNone, eNone, 2000, True, "vo_areyouplayingthisgame"
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
    LightShootAgain2.State = 0
    ' if the table uses the same lights for the extra ball or replay then turn them on if needed
    If ExtraBallsAwards(CurrentPlayer) > 0 Then
        LightShootAgain2.State = 1
    End If
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
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
    DMD CL("JACKPOT"), CL(FormatScore(Jackpot(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1500, True, "vo_Jackpot" &RndNbr(3)
    DOF 126, DOFPulse
    AddScore2 Jackpot(CurrentPlayer)
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + 100000
    LightEffect 2
    GiEffect 2
    FlashEffect 2
End Sub

Sub AwardTargetJackpot() 'award a target jackpot after hitting all targets
    DMD CL("TARGET JACKPOT"), CL(FormatScore(TargetJackpot(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 1500, True, "vo_Jackpot" &RndNbr(3)
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
    DMD CL("BODIES SUPERJACKPOT"), CL(FormatScore(WeaponSJValue(CurrentPlayer) ) ), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_superjackpot"
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
        if(hsCurrentLetter > len(hsValidLetters) ) then
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

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 23:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

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
        Case 22:table1.ColorGradeImage = "LUT BLACKLIGHT"
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
                    tmp1 = CL("HIT THE POLICE")
                End If
            Case 1: 'spinners
                If Not ReadyToKill Then
                    tmp1 = FL("SPINNERS LEFT", SpinNeeded-SpinCount)
                Else
                    tmp1 = CL("SHOOT THE SCOOP")
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
                    tmp1 = CL("SHOOT THE SCOOP")
                End If
            Case 4:tmp1 = FL("HITS LEFT", 5-TargetModeHits)
            Case 5:
                If Not ReadyToKill Then
                    tmp1 = FL("HITS LEFT", 4-TargetModeHits)
                Else
                    tmp1 = CL("SHOOT SPAULDING")
                End If
            Case 6:tmp1 = FL("HITS LEFT", 5-TargetModeHits)
            Case 7:tmp1 = FL("HITS LEFT", 4-TargetModeHits)
            Case 8:tmp1 = FL("HITS LEFT", 5-TargetModeHits)
            Case 9:tmp1 = FL("HITS LEFT", 5-TargetModeHits)
            Case 10:tmp1 = FL("HITS LEFT", 6-TargetModeHits)
            Case 11:tmp1 = FL("HITS LEFT", 6-TargetModeHits)
            Case 12:tmp1 = FL("SPINNERS LEFT", SpinNeeded-SpinCount)
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
    'RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    LeftFlipperTop001.RotZ = LeftFlipper001.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle   
    chicken2.Roty = Spinner002.CurrentAngle
    FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperLSh2.RotZ = LeftFlipper001.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle

'**INSERTS***

pon071.blenddisablelighting = Li071.getinplayintensity * 40
poff071.blenddisablelighting = Li071.getinplayintensity + 1
pon072.blenddisablelighting = Li072.getinplayintensity * 40
poff072.blenddisablelighting = Li072.getinplayintensity + 1
pon073.blenddisablelighting = Li073.getinplayintensity * 40
poff073.blenddisablelighting = Li073.getinplayintensity + 1
pon048.blenddisablelighting = Li048.getinplayintensity * 40
poff048.blenddisablelighting = Li048.getinplayintensity + 1
pon066.blenddisablelighting = Li066.getinplayintensity * 40
poff066.blenddisablelighting = Li066.getinplayintensity + 1
pon067.blenddisablelighting = Li067.getinplayintensity * 40
poff067.blenddisablelighting = Li067.getinplayintensity + 1
pon068.blenddisablelighting = Li068.getinplayintensity * 40
poff068.blenddisablelighting = Li068.getinplayintensity + 1
pon052.blenddisablelighting = Li052.getinplayintensity * 40
poff052.blenddisablelighting = Li052.getinplayintensity + 1
pon061.blenddisablelighting = Li061.getinplayintensity * 40
poff061.blenddisablelighting = Li061.getinplayintensity + 1
pon060.blenddisablelighting = Li060.getinplayintensity * 40
poff060.blenddisablelighting = Li060.getinplayintensity + 1
pon032.blenddisablelighting = Li032.getinplayintensity * 40
poff032.blenddisablelighting = Li032.getinplayintensity + 1
pon053.blenddisablelighting = Li053.getinplayintensity * 40
poff053.blenddisablelighting = Li053.getinplayintensity + 1
pon065.blenddisablelighting = Li065.getinplayintensity * 40
poff065.blenddisablelighting = Li065.getinplayintensity + 1
pon064.blenddisablelighting = Li064.getinplayintensity * 40
poff064.blenddisablelighting = Li064.getinplayintensity + 1
pon063.blenddisablelighting = Li063.getinplayintensity * 40
poff063.blenddisablelighting = Li063.getinplayintensity + 1
pon062.blenddisablelighting = Li062.getinplayintensity * 40
poff062.blenddisablelighting = Li062.getinplayintensity + 1
pon034.blenddisablelighting = Li034.getinplayintensity * 40
poff034.blenddisablelighting = Li034.getinplayintensity + 1
pon059.blenddisablelighting = Li059.getinplayintensity * 40
poff059.blenddisablelighting = Li059.getinplayintensity + 1
pon029.blenddisablelighting = Li029.getinplayintensity * 40
poff029.blenddisablelighting = Li029.getinplayintensity + 1
pon037.blenddisablelighting = Li037.getinplayintensity * 40
poff037.blenddisablelighting = Li037.getinplayintensity + 1
pon038.blenddisablelighting = Li038.getinplayintensity * 40
poff038.blenddisablelighting = Li038.getinplayintensity + 1
pon069.blenddisablelighting = Li069.getinplayintensity * 40
poff069.blenddisablelighting = Li069.getinplayintensity + 1
pon070.blenddisablelighting = Li070.getinplayintensity * 40
poff070.blenddisablelighting = Li070.getinplayintensity + 1
pon049.blenddisablelighting = Li049.getinplayintensity * 40
poff049.blenddisablelighting = Li049.getinplayintensity + 1
pon050.blenddisablelighting = Li050.getinplayintensity * 40
poff050.blenddisablelighting = Li050.getinplayintensity + 1
pon039.blenddisablelighting = Li039.getinplayintensity * 40
poff039.blenddisablelighting = Li039.getinplayintensity + 1
pon074.blenddisablelighting = Li074.getinplayintensity * 40
poff074.blenddisablelighting = Li074.getinplayintensity + 1
pon040.blenddisablelighting = Li040.getinplayintensity * 40
poff040.blenddisablelighting = Li040.getinplayintensity + 1
pon030.blenddisablelighting = Li030.getinplayintensity * 40
poff030.blenddisablelighting = Li030.getinplayintensity + 1
pon051.blenddisablelighting = Li051.getinplayintensity * 40
poff051.blenddisablelighting = Li051.getinplayintensity + 1
pon015.blenddisablelighting = Li015.getinplayintensity * 40
poff015.blenddisablelighting = Li015.getinplayintensity + 1
pon057.blenddisablelighting = Li057.getinplayintensity * 40
poff057.blenddisablelighting = Li057.getinplayintensity + 1
pon058.blenddisablelighting = Li058.getinplayintensity * 40
poff058.blenddisablelighting = Li058.getinplayintensity + 1
pon075.blenddisablelighting = Li075.getinplayintensity * 40
poff075.blenddisablelighting = Li075.getinplayintensity + 1
pon036.blenddisablelighting = Li036.getinplayintensity * 40
poff036.blenddisablelighting = Li036.getinplayintensity + 1
pon077.blenddisablelighting = Li077.getinplayintensity * 40
poff077.blenddisablelighting = Li077.getinplayintensity + 1
pon078.blenddisablelighting = Li078.getinplayintensity * 40
poff078.blenddisablelighting = Li078.getinplayintensity + 1
pon079.blenddisablelighting = Li079.getinplayintensity * 40
poff079.blenddisablelighting = Li079.getinplayintensity + 1
pon016.blenddisablelighting = Li016.getinplayintensity * 40
poff016.blenddisablelighting = Li016.getinplayintensity + 1
pon019.blenddisablelighting = Li019.getinplayintensity * 40
poff019.blenddisablelighting = Li019.getinplayintensity + 1
pon020.blenddisablelighting = Li020.getinplayintensity * 40
poff020.blenddisablelighting = Li020.getinplayintensity + 1
pon025.blenddisablelighting = Li025.getinplayintensity * 40
poff025.blenddisablelighting = Li025.getinplayintensity + 1
pon026.blenddisablelighting = Li026.getinplayintensity * 40
poff026.blenddisablelighting = Li026.getinplayintensity + 1
pon027.blenddisablelighting = Li027.getinplayintensity * 40
poff027.blenddisablelighting = Li027.getinplayintensity + 1
pon028.blenddisablelighting = Li028.getinplayintensity * 40
poff028.blenddisablelighting = Li028.getinplayintensity + 1
ponshoot.blenddisablelighting = LightShootAgain2.getinplayintensity * 40
poffshoot.blenddisablelighting = LightShootAgain2.getinplayintensity + 1
pon021.blenddisablelighting = Li021.getinplayintensity * 40
poff021.blenddisablelighting = Li021.getinplayintensity + 1
pon022.blenddisablelighting = Li022.getinplayintensity * 40
poff022.blenddisablelighting = Li022.getinplayintensity + 1
pon023.blenddisablelighting = Li023.getinplayintensity * 40
poff023.blenddisablelighting = Li023.getinplayintensity + 1
pon024.blenddisablelighting = Li024.getinplayintensity * 40
poff024.blenddisablelighting = Li024.getinplayintensity + 1
pon017.blenddisablelighting = Li017.getinplayintensity * 40
poff017.blenddisablelighting = Li017.getinplayintensity + 1
pon018.blenddisablelighting = Li018.getinplayintensity * 40
poff018.blenddisablelighting = Li018.getinplayintensity + 1
pon031.blenddisablelighting = Li031.getinplayintensity * 40
poff031.blenddisablelighting = Li031.getinplayintensity + 1
pon033.blenddisablelighting = Li033.getinplayintensity * 40
poff033.blenddisablelighting = Li033.getinplayintensity + 1
pon035.blenddisablelighting = Li035.getinplayintensity * 40
poff035.blenddisablelighting = Li035.getinplayintensity + 1
pon041.blenddisablelighting = Li041.getinplayintensity * 40
poff041.blenddisablelighting = Li041.getinplayintensity + 1
pon042.blenddisablelighting = Li042.getinplayintensity * 40
poff042.blenddisablelighting = Li042.getinplayintensity + 1
pon043.blenddisablelighting = Li043.getinplayintensity * 40
poff043.blenddisablelighting = Li043.getinplayintensity + 1
pon044.blenddisablelighting = Li044.getinplayintensity * 40
poff044.blenddisablelighting = Li044.getinplayintensity + 1
pon045.blenddisablelighting = Li045.getinplayintensity * 40
poff045.blenddisablelighting = Li045.getinplayintensity + 1
pon046.blenddisablelighting = Li046.getinplayintensity * 40
poff046.blenddisablelighting = Li046.getinplayintensity + 1
pon047.blenddisablelighting = Li047.getinplayintensity * 40
poff047.blenddisablelighting = Li047.getinplayintensity + 1
pon076.blenddisablelighting = Li076.getinplayintensity * 40
poff076.blenddisablelighting = Li076.getinplayintensity + 1
pon001.blenddisablelighting = Li001.getinplayintensity * 40
poff001.blenddisablelighting = Li001.getinplayintensity + 1
pon002.blenddisablelighting = Li002.getinplayintensity * 40
poff002.blenddisablelighting = Li002.getinplayintensity + 1
pon003.blenddisablelighting = Li003.getinplayintensity * 40
poff003.blenddisablelighting = Li003.getinplayintensity + 1
pon004.blenddisablelighting = Li004.getinplayintensity * 40
poff004.blenddisablelighting = Li004.getinplayintensity + 1
pon005.blenddisablelighting = Li005.getinplayintensity * 40
poff005.blenddisablelighting = Li005.getinplayintensity + 1
pon006.blenddisablelighting = Li006.getinplayintensity * 40
poff006.blenddisablelighting = Li006.getinplayintensity + 1
pon007.blenddisablelighting = Li007.getinplayintensity * 40
poff007.blenddisablelighting = Li007.getinplayintensity + 1
pon008.blenddisablelighting = Li008.getinplayintensity * 40
poff008.blenddisablelighting = Li008.getinplayintensity + 1
pon009.blenddisablelighting = Li009.getinplayintensity * 40
poff009.blenddisablelighting = Li009.getinplayintensity + 1
pon010.blenddisablelighting = Li010.getinplayintensity * 40
poff010.blenddisablelighting = Li010.getinplayintensity + 1
pon011.blenddisablelighting = Li011.getinplayintensity * 40
poff011.blenddisablelighting = Li011.getinplayintensity + 1
pon012.blenddisablelighting = Li012.getinplayintensity * 40
poff012.blenddisablelighting = Li012.getinplayintensity + 1
pon013.blenddisablelighting = Li013.getinplayintensity * 40
poff013.blenddisablelighting = Li013.getinplayintensity + 1
pon014.blenddisablelighting = Li014.getinplayintensity * 40
poff014.blenddisablelighting = Li014.getinplayintensity + 1

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
    DMD " ", "", "d_jppresents", eNone, eNone, eNone, 3000, False, ""
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
        Case 1 'ABBIE = Super Spinners
            DMD CL("CURRENT MODE"), CL("ABBIE"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE SPINNERS"), CL("AND SCOOP TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 2 'KAREN MURPHY = 5 Targets at semi random
            DMD CL("CURRENT MODE"), CL("KAREN"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("HIT THE LIT TARGETS"), CL("AND MAGNET TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 3 'DON WILLIS = 5 Flashing Shots 90 seconds to complete
            DMD CL("CURRENT MODE"), CL("DON"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE LIGHTS"), CL("YOU HAVE 90 SECONDS"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE SCOOP"), CL("BEFORE TIME IS UP"), "", eNone, eNone, eNone, 2000, False, ""
        Case 4 'GLORIA SULLIVAN = 5 Orbits
            DMD CL("CURRENT MODE"), CL("GLORIA"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 5"), CL("LIT ORBITS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 5 'RICHARD WICK = Shoot 4 lights 60 seconds
            DMD CL("CURRENT MODE"), CL("RICHARD"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 4 LIGHTS"), CL("YOU HAVE 60 SECONDS"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("AND SPAULDING TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 6 'BILL HUDLEY = Shoot the ramps
            DMD CL("CURRENT MODE"), CL("BILL"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("ERIC"), CL("SHOOT 5 RAMPS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 7 'ADAM BANJO=  Target Frenzy
            DMD CL("CURRENT MODE"), CL("ADAM"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 4"), CL("LIT TARGETS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 8 'DENISE WILLIS = 5 Targets in rotation
            DMD CL("CURRENT MODE"), CL("DENISE"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL(""), CL("SHOOT 5 LIT TARGETS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 9 'STEVE NAISH = Magnet post
            DMD CL("CURRENT MODE"), CL("STEVE"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE MAGNET"), CL("5 TIMES"), "", eNone, eNone, eNone, 2000, False, ""
        Case 10 'JERRY GOLDSMITH = Ramps and Orbits
            DMD CL("CURRENT MODE"), CL("JERRY"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 6 RAMPS"), CL("OR ORBITS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 11 'ROY SULLIVAN = 5 Blue Targets at random
            DMD CL("CURRENT MODE"), CL("ROY"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 6"), CL("BLUE TARGETS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 12 'MARY KNOWLES = Super Spinners at random
            DMD CL("CURRENT MODE"), CL("MARY"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE SPINNERS"), CL("AT RANDOM"), "", eNone, eNone, eNone, 2000, False, ""
        Case 13 'KILLER KARL = Follow the Lights
            DMD CL("CURRENT MODE"), CL("KARL"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT 6 LIT"), CL("LIGHTS"), "", eNone, eNone, eNone, 2000, False, ""
        Case 14 'GEORGE WYDELL = Follow the Lights random
            DMD CL("CURRENT MODE"), CL("GEORGE"), "", eNone, eNone, eNone, 2000, False, ""
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
    DMD CL("BODIES SUPER JACKPOT"), CL(FormatScore(WeaponSJValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 2000, False, ""
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
    if bTommyStarted Then StopSHERIFFWYDELL
    StopMode
    TeenTimer.Enabled = 0
End Sub

Sub ResetNewBallVariables() 'reset variables and lights for a new ball or player
    'turn on or off the needed lights before a new ball is released
    TurnOffPlayfieldLights
    libumper.State = 0
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
    li063.State = 2
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    bRotateLights = True
    LightSeqSkillshot.StopPlay
    Li034.State = 0
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
    FlashForMs Flasherspaulding1, 1000, 50, 0
    If Tilted Then Exit Sub
    LS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotLeft Lemk
    DOF 105, DOFPulse
        If FlippersBlood Then LeftSplat
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    'flash the gun
    flash1.Duration 1, 150, 0
    flash3.Duration 1, 150, 0
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
    FlashForMs Flasherspaulding2, 1000, 50, 0
	RS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotRight Remk
    DOF 106, DOFPulse
        If FlippersBlood Then RightSplat
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    'flash the gun
    flash2.Duration 1, 150, 0
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
    RandomSoundBumperTop Bumper1
    If B2sOn then
        SlingCount = 0
        SlingTimer.Enabled = 1
    End If
    DOF 138, DOFPulse
    ' add some points
    If libumper.State Then 'the light is on so give the bumper Jackpot
        FlashForms libumper, 1500, 75, 0
        FlashForms Flasher002, 1500, 75, 0
        FlashForms Flasher002a, 1500, 75, 0
        DOF 127, DOFPulse
        tmp = 100000 + INT(Score(CurrentPlayer) * 0.01) * 10 'the bumper jackpot is 100.000 + 10% of the score
        DMD CL("BUMPER JACKPOT"), CL(FormatScore(tmp) ), "", eNone, eNone, eNone, 1500, True, "vo_jackpot" &RndNbr(3)
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
        Flasher002a.Visible = 1
        BumperNeededHits(CurrentPlayer) = BumperNeededHits(CurrentPlayer) + 10 + RndNbr(10)
    End If
End Sub

'*********
' Lanes
'*********
' in and outlanes - mystery ?
Sub Trigger001_Hit
    DOF 207, DOFPulse
	' xCFx
	DOF 302, DOFPulse
	' ----
    If Tilted Then Exit Sub
    Addscore 5000
    Mystery(CurrentPlayer, 1) = 1
    CheckMystery
End Sub

Sub Trigger002_Hit
    DOF 207, DOFPulse
	' xCFx
	DOF 303, DOFPulse
	' ----
    If Tilted Then Exit Sub
    Addscore 1000
    Mystery(CurrentPlayer, 2) = 1
    CheckMystery
End Sub

Sub Trigger003_Hit
    DOF 207, DOFPulse
	' xCFx
	DOF 303, DOFPulse
	' ----
    If Tilted Then Exit Sub
    Addscore 1000
    Mystery(CurrentPlayer, 3) = 1
    CheckMystery
End Sub

Sub Trigger004_Hit
    DOF 207, DOFPulse
	' xCFx
	DOF 302, DOFPulse
	' ----
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
    If Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4) = 4 Then
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
    If Tilted Then Exit Sub
	' xCFx
	DOF 328, DOFPulse
	' ----
    Addscore 1000
    If bSkillShotReady Then li034.State = 0
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
    If Tilted Then Exit Sub
    Addscore 1000
    If bSkillShotReady AND li034.State Then AwardSkillshot
End Sub

Sub Trigger008_Hit 'end top loop
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
    Addscore 5000
    ' remember last trigger hit by the ball
    LastSwitchHit = "Trigger008"
    Select Case NewMode
        Case 4:TargetModeHits = TargetModeHits + 1:CheckWinMode
    End Select
End Sub

Sub Trigger009_Hit 'right loop enter
    DOF 206, DOFPulse
    If Tilted Then Exit Sub
	' xCFx
	DOF 326, DOFPulse
	' ----

    PlayThunder
    Addscore 10000
    Flashforms f2A, 800, 50, 0
    Flashforms F2B, 800, 50, 0
    Flashforms Flasher004, 800, 50, 0
    Flashforms Flasher004a, 800, 50, 0
    Flashforms Flasher005, 800, 50, 0
    FlashForMs Flasherspaulding1, 1000, 50, 0
    FlashForMs Flasherspaulding2, 1000, 50, 0
    FlashForMs LightSpaulding, 1000, 50, 0
    If LastSwitchHit = "Trigger008" Then
        AwardLoop
    Else
        li061.State = 2 'super loops light
        LoopCount = 1
    End If
    If F002.State Then TeenKilled:F002.State = 0
    ' Weapons Super Jackpot
    If aWeaponSJactive AND li060.State Then AwardWeaponsSuperJackpot
    'Jason multiball
    If bJasonMBStarted Then
        DOF 127, DOFPulse
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(7) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(3)
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

'*******************************************
'  Ramp Triggers
'*******************************************
Sub ramptrigger01_hit()
	WireRampOn True	 'Play Plastic Ramp Sound
	' xCFx
	DOF 324, DOFPulse
	' ----
End Sub

Sub ramptrigger02_hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
End Sub

Sub ramptrigger02_unhit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
End Sub

Sub ramptrigger03_hit()
	WireRampOff	 'Exiting Wire Ramp Stop Playing Sound
End Sub

Sub ramptrigger03_unhit()
	RandomSoundRampStop ramptrigger03
End Sub

Sub ramptrigger04_hit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
	' xCFx
	DOF 323, DOFPulse
	Debug.print "DOF 323"
	' ----
End Sub

Sub ramptrigger05_hit()
	WireRampOff	 'Exiting Wire Ramp Stop Playing Sound
	' xCFx
	DOF 331, DOFPulse
	' ----
End Sub

Sub ramptrigger05_unhit()
	RandomSoundRampStop ramptrigger05
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
	' xCFx
	DOF 320, DOFPulse
	' ----
    
    FlashForMs Flasherspaulding1, 1000, 50, 0
    FlashForMs Flasherspaulding2, 1000, 50, 0
    FlashForMs LightSpaulding, 1000, 50, 0
    
    Flashlogo 3000, 100
    Addscore 5000
    If aWeaponSJactive Then     'the lit Super Jackpot light is lit, so lit the Super Jackpot Light at the right loop
        SuperJackpotTimer_Timer 'call the timer to stop the 30s timer and blinking light at the cabin
        li060.State = 2
    End If
    'Jason multiball
    If bJasonMBStarted Then
        DOF 127, DOFPulse
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(4) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(3)
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
            Playsound "YUYUYU"
            DMD "_", CL("BALL 1 LOCKED"), "_", eNone, eNone, eNone, 1000, True, "vo_ball1locked"
            BallsInLock(CurrentPlayer) = 2
            UpdateLockLights
        Case 2: 'lock 2
            Playsound "YUYUYU"
            DMD "_", CL("BALL 2 LOCKED"), "_", eNone, eNone, eNone, 1000, True, "vo_ball2locked"
            BallsInLock(CurrentPlayer) = 3
            UpdateLockLights
        Case 3 'lock 3 - start multiball if there is not a multiball already
             Playsound "YUYUYU"
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
	' xCFx
	DOF 316, DOFPulse
	' ----
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
	' xCFx
	DOF 329, DOFPulse
	' ----
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
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(8) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(3)
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
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(3)
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
	' xCFx
	DOF 321, DOFPulse
	' ----
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
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(5) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(3)
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
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(3)
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
	' xCFx
	DOF 325, DOFPulse
	' ----
    Addscore 5000
    If LastSwitchHit = "Trigger017" Then
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
        StopSHERIFFWYDELL
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
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(6) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(3)
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
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(3)
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
	' xCFx
	DOF 322, DOFPulse
	Debug.print "DOF 322"
	' ---

    Flashramplight1 500, 100 
    Flashramplight2 700, 200 
    If Tilted Then Exit Sub
    Addscore 5000
    If LastSwitchHit = "Trigger017" Then
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
        DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(2) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(3)
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
        DMD CL("JACKPOT"), CL(FormatScore(1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(3)
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

Sub Trigger018_Hit 'left loop - only light effect
    '	DOF 204, DOFPulse
    If Tilted Then Exit Sub
    Flashforms f1A, 800, 50, 0
    Flashforms F1B, 800, 50, 0
    Flashforms Flasher006, 800, 50, 0
    Flashforms Flasher007, 800, 50, 0
    Flashforms Flasher007a, 800, 50, 0
End Sub

'************
'  Targets
'************

Sub Target001_Hit 'police
    STHit 1
	' xCFx
	DOF 310, DOFPulse
	' ----
    If Tilted Then Exit Sub
    Addscore 5000
    Flashforms f5, 800, 50, 0
    Flashforms Flasher001a, 800, 50, 0
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

Sub Target001o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub Target002_Hit 'right target 1
    STHit 2
	' xCFx
	DOF 312, DOFPulse
	' ----
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

Sub Target002o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub Target003_Hit 'right target 2
    STHit 3
	' xCFx
	DOF 312, DOFPulse
	' ----
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

Sub Target003o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub Target004_Hit 'cabin target 1
    STHit 4
	' xCFx
	DOF 311, DOFPulse
	' ----
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

Sub Target004o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub Target005_Hit 'cabin target 2
    STHit 5
	' xCFx
	DOF 311, DOFPulse
	' ----
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

Sub Target005o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub Target006_Hit 'loop target
    STHit 6
	'debug.print "Target6" 
	' xCFx
	DOF 313, DOFPulse
	' ----
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

Sub Target006o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub Target007_Hit 'magnet target W3
    STHit 7
	' xCFx
	DOF 314, DOFPulse
	' ----
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

Sub Target007o_Hit
	TargetBouncer ActiveBall, 1
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
    SoundSpinner Spinner001
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
    SoundSpinner Spinner002
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
    SoundSaucerLock
	' xCFx
	DOF 330, DOFOn
	'Debug.print "ball in scoop"
	' ----
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
		' xCFx
		DOF 330, DOFOff
		'Debug.print "ball ejected from scoop"
		' ----
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
    StopSPAL
    If BallsinHole > 0 Then
        BallsinHole = BallsInHole - 1
        SoundSaucerKick 1, scoop
        DOF 124, DOFPulse
        scoop.CreateSizedBallWithMass BallSize / 2, BallMass
        scoop.kick 235, 32+(rnd*5), 1 'scoop.kick 235, 22, 1
        Flashforms F4, 500, 50, 0
        vpmtimer.addtimer 400, "kickBallOut '" 'kick out the rest of the balls, if any
    End If
	' xCFx
	DOF 330, DOFOff
	Debug.print "ball ejected from scoop"
	' ----
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
	' xCFx
	If ActiveBall.VelY>0 Then
		DOF 315, DofOn
	Else	
		DOF 332, DOFPulse
	End If
	' ----

    'Jason multiball
    If bJasonMBStarted Then
        If ActiveBall.VelY < 0 Then 'this means the ball going up
            DMD CL("JACKPOT"), CL(FormatScore(ArrowMultiPlier(3) * 1000000) ), "_", eBlink, eNone, eNone, 1000, True, "vo_jackpot" &RndNbr(3)
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
	' xCFx
	DOF 315, DOFoff
	' ----
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
    DMD CL("YOU KILLED"), CL("A"), "d_go10", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("A "), "d_go11", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("A TE"), "d_go12", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("A TEE"), "d_go13", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("A TEEN"), "d_go14", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("A TEENA"), "d_go15", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("A TEENAG"), "d_go16", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("A TEENAGE"), "d_go17", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("A TEENAGER"), "d_go18", eNone, eNone, eNone, 100, False, ""
    DMD CL("YOU KILLED"), CL("A TEENAGER"), "d_go19", eNone, eNone, eNone, 1000, False, ""
    Select Case RndNbr(3)
        Case 1:DMD CL("TEENAGER KILLED"), CL(FormatScore(TeensKilledValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_excellent"
        Case 2:DMD CL("TEENAGER KILLED"), CL(FormatScore(TeensKilledValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_greatshot"
        Case 3:DMD CL("TEENAGER KILLED"), CL(FormatScore(TeensKilledValue(CurrentPlayer) ) ), "", eNone, eNone, eNone, 1500, True, "vo_notbad"
    End Select
    TeensKilled(CurrentPlayer) = TeensKilled(CurrentPlayer) + 1
    AddScore2 TeensKilledValue(CurrentPlayer)
    TeensKilledValue(CurrentPlayer) = TeensKilledValue(CurrentPlayer) + 50000
    LightEffect 5
    GiEffect 5
    'check
    If TeensKilled(CurrentPlayer) MOD 6 = 0 Then StartSHERIFFWYDELL 'start Tommy Jarvis hurry up after each 6th killed teenager
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
     lightbodies.State = 1
    Weapons(CurrentPlayer) = Weapons(CurrentPlayer) + 1
    UpdateWeaponLights2
    AddPlayfieldMultiplier 1
    ResetWeaponLights
    aWeaponSJactive = True
    StartSuperJackpot   'turn on the lits SJ at the cabin
End Sub

Sub UpdateWeaponLights2 'collected weapons
   
    Select Case Weapons(CurrentPlayer)
        Case 1:li041.State = 1:li042.State = 0:li043.State = 0:li044.State = 0:li045.State = 0:li046.State = 0:li047.State = 0
        Case 2:li041.State = 1:li042.State = 1:li043.State = 0:li044.State = 0:li045.State = 0:li046.State = 0:li047.State = 0
        Case 3:li041.State = 1:li042.State = 1:li043.State = 1:li044.State = 0:li045.State = 0:li046.State = 0:li047.State = 0
        Case 4:li041.State = 1:li042.State = 1:li043.State = 1:li044.State = 1:li045.State = 0:li046.State = 0:li047.State = 0
        Case 5:li041.State = 1:li042.State = 1:li043.State = 1:li044.State = 1:li045.State = 1:li046.State = 0:li047.State = 0
        Case 6:li041.State = 1:li042.State = 1:li043.State = 1:li044.State = 1:li045.State = 1:li046.State = 1:li047.State = 0
        Case 7:li041.State = 1:li042.State = 1:li043.State = 1:li044.State = 1:li045.State = 1:li046.State = 1:li047.State = 1
        lightbodies.State = 0
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
'SHERIFF WYDELL - Hurry up
'*************************
' it starts after each 6 teenagers killed
' a 30 seconds hurry up starts at the right ramp
' hit the right ramp to throw a deadly blow to your archie enemy
' fail and your flippers will die for 3 seconds

Sub StartSHERIFFWYDELL 'Tommy's hurry up
    DMD CL("SHERIFF WYDELL"), CL("HURRY UP"), "d_border", eNone, eNone, eNone, 2500, True, "vo_shoottherightramp" &RndNbr(2)
    bTommyStarted = True
    FlashEffect 7
    Li030.BlinkInterval = 160
    Li030.State = 2
    TommyCount = 1
    TommyValue = 500000
    TommyHurryUpTimer.Enabled = 1
End Sub

Sub StopSHERIFFWYDELL
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
        DMD CL("SHERIFF WYDELL"), CL("SHOT YOU"), "d_border", eNone, eNone, eNone, 2500, True, "vo_holyshityoushootme"
        DisableFlippers True          'disable the flippers...
        ChangeGi purple
        ChangeGIIntensity 2
        TommyRecoverTimer.Enabled = 1 '... start the 3 seconds timer to enable the table again
    End If
End Sub

Sub TommyRecoverTimer_Timer 'after disabling the table for 3 seconds then enable it
    TommyRecoverTimer.Enabled = 0
    StopSHERIFFWYDELL
    DisableFlippers False
    ChangeGi white 
    ChangeGIIntensity 1
End Sub

Sub AwardTommyJackpot() 'scores the seconds multiplied by 500.000, the longer you wait the higher the score
    Dim a
    a = TommyCount * 500000
    Select Case RndNbr(3)
        Case 1:DMD CL("SHERIFFWYDELLJACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, "vo_welldone"
        Case 2:DMD CL("SHERIFFWYDELLJACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, "vo_welldone"
        Case 3:DMD CL("SHERIFFWYDELLJACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, "vo_younailedit"
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
    PoliceL3.BlinkInterval = 160
    PoliceL4.BlinkInterval = 160
    Li033.BlinkInterval = 160
    PoliceL1.State = 2
    PoliceL2.State = 2
    PoliceL3.State = 2
    PoliceL4.State = 2
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
    PoliceL3.State = 0
    PoliceL4.State = 0
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
        PoliceL3.BlinkInterval = 80
        PoliceL4.BlinkInterval = 80
        Li033.BlinkInterval = 80
        PoliceL1.State = 2
        PoliceL2.State = 2
        PoliceL3.State = 2
        PoliceL4.State = 2
        Li033.State = 2
    End If
    If PoliceCount = 0 Then
        PoliceHurryUpTimer.Enabled = 0
        DMD CL("THE POLICE"), CL("SHOT YOU"), "d_border", eNone, eNone, eNone, 2500, True, "vo_holyshityoushootme"
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
        Case 1:DMD CL("POLICE JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, "vo_welldone"
        Case 2:DMD CL("POLICE JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, "vo_welldone"
        Case 3:DMD CL("POLICE JACKPOT"), CL(FormatScore(a) ), "d_border", eNone, eBlinkFast, eNone, 2500, True, "vo_younailedit"
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
    EnableBallSaver 25
    DMD "_", CL("SPAULDING MULTIBALL"), "_", eNone, eNone, eNone, 2500, True, "vo_multiball1" 
    AddMultiball 2
    For i = 1 to 8
        ArrowMultiPlier(i) = 1
    Next
    UpdateArrowLights
    li016.State = 2
    LightCUTTER.State = 2
    ChangeSong
End Sub

Sub StopJasonMultiball
    bJasonMBStarted = False
    BallsInLock(CurrentPlayer) = 0
    TurnOffArrows
    li016.State = 0
    LightCUTTER.State = 0
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
' BABY FIREFLY Multiball - Ramps
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
    DMD "_", CL("BABY MULTIBALL"), "_", eNone, eNone, eNone, 2500, True, "vo_multiballbaby"
    AddMultiball 2
    SetLightColor li064, blue, 2
    SetLightColor li068, blue, 2
    li015.State = 2
    LightBABY.State = 2
End Sub

Sub StopFreddyMultiball
    bFreddyMBStarted = False
    li064.State = 0
    li068.State = 0
    li015.State = 0
    LightBABY.State = 0
End Sub

'***********************************
' OTIS DRIFTWOOD Multiball - Spinners
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
    DMD "_", CL("OTIS MULTIBALL"), "_", eNone, eNone, eNone, 2500, True, "vo_multiballotis"
    AddMultiball 2
    SetLightColor li062, blue, 2
    SetLightColor li067, blue, 2
    SetLightColor li059, blue, 2
    li075.State = 2
    LightOTIS.State = 2
End Sub

Sub StopMichaelMultiball
    bMichaelMBStarted = False
    li062.State = 0
    li067.State = 0
    li059.State = 0
    li075.State = 0
    LightOTIS.State = 0
End Sub

'****************************
' Mystery award at the scoop
'****************************
' this is a kind of award after completing the inlane and outlanes

Sub CheckMystery 'if all the inlanes and outlanes are lit then lit the mystery award
    dim i
    If Mystery(CurrentPlayer, 1) + Mystery(CurrentPlayer, 2) + Mystery(CurrentPlayer, 3) + Mystery(CurrentPlayer, 4) = 4 Then
        DMD "_", CL("MYSTERY IS LIT"), "", eNone, eNone, eNone, 1000, True, "vo_welldone"
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
'   Modes - Hunting the people
'**********************************

' This table has 14 modes which will be selected at random
' After killing all people you'll get a surprise :)

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
        UpdateModeLights
    'debug.print "newmode " & newmode
    End If
End Sub

' Update the lights according to the mode's state
Sub UpdateModeLights
    li014.State = Mode(CurrentPlayer, 1)
    Lightabbie.State = Mode(CurrentPlayer, 1)
    li013.State = Mode(CurrentPlayer, 2)
    Lightkaren.State = Mode(CurrentPlayer, 2)
    li012.State = Mode(CurrentPlayer, 3)
    LightDON.State = Mode(CurrentPlayer, 3)
    li011.State = Mode(CurrentPlayer, 4)
    Lightgloria.State = Mode(CurrentPlayer, 4)
    li010.State = Mode(CurrentPlayer, 5)
    Lightrichard.State = Mode(CurrentPlayer, 5)
    li009.State = Mode(CurrentPlayer, 6)
    LightBILL.State = Mode(CurrentPlayer, 6)
    li008.State = Mode(CurrentPlayer, 7)
    Lightadam.State = Mode(CurrentPlayer, 7)
    li007.State = Mode(CurrentPlayer, 8)
    Lightdenise.State = Mode(CurrentPlayer, 8)
    li006.State = Mode(CurrentPlayer, 9)
    Lightsteve.State = Mode(CurrentPlayer, 9)
    li005.State = Mode(CurrentPlayer, 10)
    Lightjerry.State = Mode(CurrentPlayer, 10)
    li004.State = Mode(CurrentPlayer, 11)
    lightroy.State = Mode(CurrentPlayer, 11)
    li001.State = Mode(CurrentPlayer, 12)
    Lightmary.State = Mode(CurrentPlayer, 12)
    li002.State = Mode(CurrentPlayer, 13)
    Lightkarl.State = Mode(CurrentPlayer, 13)
    li003.State = Mode(CurrentPlayer, 14)
    Lightgeorge.State = Mode(CurrentPlayer, 14)


End Sub

' Starting a mode means to setup some lights and variables, maybe timers
' Mode lights will always blink during an active mode
Sub StartMode
    StartSPAL
    Playsound "vo_welcometohell"
    Mode(CurrentPlayer, 0) = NewMode
    Li076.State = 0
    ChangeSong
    EnableBallSaver 20
    'PlaySound "vo_hahaha"&RndNbr(4)
    ReadyToKill = False
    Select Case NewMode
        Case 1 'ABBIE = Super Spinners
            DMD CL("KILL ABBIE"), CL("SHOOT THE SPINNERS"), "", eNone, eNone, eNone, 1500, True, "vo_shootthespinners"
            SpinCount = 0
            SetLightColor li062, amber, 2
            SetLightColor li067, amber, 2
            SetLightColor li059, amber, 2
            SpinNeeded = 50
        Case 2 'KAREN = 5 Targets at semi random
            DMD CL("KILL KAREN"), CL("HIT THE LIT TARGETS"), "", eNone, eNone, eNone, 1500, True, "vo_shootthetargets"
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
        Case 3 'DON = 5 Flashing Shots 90 seconds to complete
            DMD CL("KILL DON"), CL("SHOOT THE LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            DMD CL("KILL DON"), CL("YOU HAVE 90 SECONDS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            SetLightColor li062, amber, 2
            SetLightColor li064, amber, 2
            SetLightColor li066, amber, 2
            SetLightColor li067, amber, 2
            SetLightColor li068, amber, 2
            EndModeCountdown = 90
            EndModeTimer.Enabled = 1
        Case 4 'GLORIA = 5 Orbits 'uses trigger008 to detect the completed orbits
            DMD CL("KILL GLORIA"), CL("SHOOT 5 ORBITS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            SetLightColor li067, amber, 2
            SetLightColor li052, amber, 2
        Case 5 'RICHARD = Shoot 4 lights 60 seconds, all shots are lit, after 4 shot lit the cabin to kill her & collect jackpot
            DMD CL("KILL RICHARD"), CL("SHOOT 4 LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
            DMD CL("KILL RICHARD"), CL("YOU HAVE 60 SECONDS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            TurnonArrows amber
            EndModeCountdown = 60
            EndModeTimer.Enabled = 1
        Case 6 'BILL = Shoot the ramps
            DMD CL("KILL BILL"), CL("SHOOT 5 RAMPS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            SetLightColor li064, amber, 2
            SetLightColor li068, amber, 2
        Case 7 'ADAM=  Target Frenzy
            DMD CL("KILL ADAM"), CL("SHOOT 4 LIT TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            'Turn off all blue targets
            TurnOffBlueTargets
            Select Case RndNbr(4) 'select 4 blue targets
                Case 1:Li031.State = 2:Li050.State = 2:li058.State = 2:Li079.State = 2
                Case 2:Li049.State = 2:Li050.State = 2:li058.State = 2:Li057.State = 2
                Case 3:Li031.State = 2:Li049.State = 2:li051.State = 2:Li057.State = 2
                Case 4:Li031.State = 2:Li049.State = 2:li050.State = 2:Li051.State = 2
            End Select
        Case 8 'KILLER DENISE WILLIS = 5 Targets in rotation
            DMD CL("KILL DENISE"), CL("SHOOT 5 LIT TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            'Turn off all blue targets
            TurnOffBlueTargets
            'Start timer to rotate through the targets
            BlueTargetsCount = RndNbr(7)
            BlueTargetsTimer_Timer
            BlueTargetsTimer.Enabled = 1
        Case 9 'STEVE = Magnet
            DMD CL("KILL STEVE"), CL("SHOOT THE MAGNET"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            li065.State = 2
        Case 10 'JIMMY = Ramps and Orbits - all lights lit
            DMD CL("KILL JERRY"), CL("SHOOT RAMPS ORBITS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            SetLightColor li064, amber, 2
            SetLightColor li067, amber, 2
            SetLightColor li068, amber, 2
            SetLightColor li052, amber, 2
        Case 11 'STEVE = 5 Blue Targets at random
            DMD CL("KILL ROY"), CL("SHOOT BLUE TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            TargetModeHits = 0
            'Turn off all blue targets
            TurnOffBlueTargets
            'Start timer to rotate through the targets
            BlueTargetsCount = RndNbr(7)
            BlueTargetsTimer_Timer
            BlueTargetsTimer.Enabled = 1
        Case 12 'MARY = Super Spinners at random
            DMD CL("KILL MARY"), CL("SHOOT THE SPINNERS"), "", eNone, eNone, eNone, 1500, True, ""
            SpinCount = 0
            SpinNeeded = 50
            SpinnersTimer_Timer
            SpinnersTimer.Enabled = 1
        Case 13 'KARL = Follow the Lights All main Shots 1 at a time in rotation
            DMD CL("KILL KARL"), CL("SHOOT LIT LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
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
        Case 14 'GEORGE = Follow the Lights All main shots at random
            DMD CL("GEORGE"), CL("SHOOT LIT LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""
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
                DMD CL("YOU CAUGHT HER"), CL("SHOOT THE SCOOP"), "", eNone, eNone, eNone, 1500, True, "vo_itstimetodie"
                li029.State = 2 'lit the Hunter Jackpot at the scoop
                li062.State = 0 'turn off the spinner lights
                li067.State = 0
                li059.State = 0
                ReadyToKill = True
            End If
        Case 2
            Addscore 150000
            If TargetModeHits = 4 Then
                DMD CL("YOU CAUGHT HIM"), CL("SHOOT THE MAGNET"), "", eNone, eNone, eNone, 1500, True, "vo_itstimetodie"
                li065.State = 2
                ReadyToKill = True
            End If
        Case 3
            Addscore 150000
            If TargetModeHits = 5 Then
                DMD CL("YOU CAUGHT HIM"), CL("SHOOT THE SCOOP"), "", eNone, eNone, eNone, 1500, True, "vo_itstimetodie"
                li029.State = 2
                ReadyToKill = True
            End If
        Case 4
            Addscore 150000
            If TargetModeHits = 5 Then WinMode:End if
        Case 5
            Addscore 150000
            If TargetModeHits = 4 Then      'lit the CLOWN for the kill & jackpot
                DMD CL("YOU CAUGHT HER"), CL("SHOOT SPAULDING"), "", eNone, eNone, eNone, 1500, True, "vo_itstimetodie"
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
        Case 1, 2, 4, 8, 12
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
            DMD CL("YOU KILLED HER"), CL(FormatScore(3500000) ), "_", eNone, eBlink, eNone, 1200, False, ""
            DMD CL("YOU KILLED HER"), CL(FormatScore(3500000) ), "_", eNone, eBlink, eNone, 2000, True, "vo_fantastic"
            Addscore2 3500000
        Case 3, 5, 6, 7, 9, 10, 11, 13, 14
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
            DMD CL("YOU KILLED THEM"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 1200, False, ""
            DMD CL("YOU KILLED THEM"), CL(FormatScore(3500000) ), "_", eNone, eBlinkFast, eNone, 2000, True, "vo_excellent"
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

' Fire lamps
Dim Fire1Pos, Fire2Pos, Fire3Pos, Fire4Pos, Fire5Pos, Fire6Pos, Flames
Flames = Array("fire01", "fire02", "fire03", "fire04", "fire05", "fire06", "fire07", "fire08", "fire09", _
    "fire10", "fire11", "fire12", "fire13", "fire14", "fire15", "fire16")

Sub StartFire
    Fire1Pos = 0
    Fire2Pos = 2
    Fire3Pos = 5
    Fire4Pos = 8
    Fire5Pos = 11
    Fire6Pos = 14
    FireTimer.Enabled = 1
End Sub

Sub FireTimer_Timer
    'debug.print fire1pos
    Fire1.ImageA = Flames(Fire1Pos)
    Fire2.ImageA = Flames(Fire2Pos)
    Fire3.ImageA = Flames(Fire3Pos)
    Fire4.ImageA = Flames(Fire4Pos)
    Fire5.ImageA = Flames(Fire5Pos)
    Fire1Pos = (Fire1Pos + 1) MOD 16
    Fire2Pos = (Fire2Pos + 1) MOD 16
    Fire3Pos = (Fire3Pos + 1) MOD 16
    Fire4Pos = (Fire4Pos + 1) MOD 16
    Fire5Pos = (Fire5Pos + 1) MOD 16
    Fire6Pos = (Fire6Pos + 1) MOD 16
End Sub

Dim mystate
mystate = 0
flasher0001.TimerEnabled = 1
 
Sub flasher0001_Timer
    flasher0001.visible = mystate
    mystate = ABS(mystate - 1)
End Sub

Sub SphereTimer_Timer
   BUMPTOP.rotz = BUMPTOP.rotz + 1
    Primitive022.rotz = Primitive022.rotz + 1
end sub


'**********************************************************************************************************
'*********** Glowball Section *****************************************************************************
Dim GlowBall, CustomBulbIntensity(10)
Dim  GBred(10)
Dim GBgreen(10)
Dim GBblue(10)
Dim CustomBallImage(10), CustomBallLogoMode(10), CustomBallDecal(10), CustomBallGlow(10)


' default Ball
CustomBallGlow(0) = 		False
CustomBallImage(0) = 		"ball"
CustomBallLogoMode(0) = 	False
CustomBallDecal(0) = 		""
CustomBulbIntensity(0) = 	0.01
GBred(0) = 0 : GBgreen(0)	= 0 : GBblue(0) = 0

' Purple GlowBall
CustomBallGlow(1) = 		True
CustomBallImage(1) = 		"glowball purple"
CustomBallLogoMode(1) = 	True
CustomBallDecal(1) = 		""
CustomBulbIntensity(1) = 	0
GBred(1) = 255 : GBgreen(1)	= 0 : GBblue(1) = 255


' green GlowBall
CustomBallGlow(2) = 		True
CustomBallImage(6) = 		"glowball green"
CustomBallLogoMode(6) = 	True
CustomBallDecal(6) = 		""
CustomBulbIntensity(6) = 	0
GBred(2) = 100 : GBgreen(2)	= 255 : GBblue(2) = 100

' blue GlowBall
CustomBallGlow(3) = 		True
CustomBallImage(3) = 		"glowball blue"
CustomBallLogoMode(3) = 	True
CustomBallDecal(3) = 		""
CustomBulbIntensity(3) = 	0
GBred(3) = 50 : GBgreen(3)	= 50 : GBblue(3) = 255


' Orange GlowBall
CustomBallGlow(4) = 		True
CustomBallImage(4) = 		"glowball orange"
CustomBallLogoMode(4) = 	True
CustomBallDecal(4) = 		""
CustomBulbIntensity(4) = 	0
GBred(4) = 255 : GBgreen(4)	= 165 : GBblue(4) = 000


' red GlowBall
CustomBallGlow(5) = 		True
CustomBallImage(5) = 		"glowball red"
CustomBallLogoMode(5) = 	True
CustomBallDecal(5) = 		""
CustomBulbIntensity(5) = 	0
GBred(5) = 255 : GBgreen(5)	= 99 : GBblue(5) = 71

' white GlowBall
CustomBallGlow(6) = 		True
CustomBallImage(6) = 		"glowball white"
CustomBallLogoMode(6) = 	True
CustomBallDecal(6) = 		""
CustomBulbIntensity(6) = 	0
GBred(6) = 255 : GBgreen(6)	= 255 : GBblue(6) = 255

' yellow GlowBall
CustomBallGlow(7) = 		True
CustomBallImage(7) = 		"glowball yellow"
CustomBallLogoMode(7) = 	True
CustomBallDecal(7) = 		""
CustomBulbIntensity(7) = 	0
GBred(7) = 255 : GBgreen(7)	= 255 : GBblue(7) = 000

' gold GlowBall
CustomBallGlow(8) = 		True
CustomBallImage(8) = 		"glowball gold"
CustomBallLogoMode(8) = 	True
CustomBallDecal(8) = 		""
CustomBulbIntensity(8) = 	0
GBred(8) = 255 : GBgreen(8)	= 215 : GBblue(8) = 000



' *** prepare the variable with references to three lights for glow ball ***
Dim Glowing(10)
Set Glowing(0) = Glowball1 : Set Glowing(1) = Glowball2 : Set Glowing(2) = Glowball3 : Set Glowing(3) = Glowball4


'*** change ball appearance ***

Sub ChangeBall(ballnr)
	Dim BOT, ii, col
	table1.BallDecalMode = CustomBallLogoMode(ballnr)
	table1.BallFrontDecal = CustomBallDecal(ballnr)
	table1.DefaultBulbIntensityScale = CustomBulbIntensity(ballnr)
	table1.BallImage = CustomBallImage(ballnr)
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 3
		col = RGB(GBred(ballnr), GBgreen(ballnr), GBblue(ballnr))
		Glowing(ii).color = col : Glowing(ii).colorfull = col 
	Next
End Sub

' *** Ball Shadow code / Glow Ball code / Primitive Flipper Update ***

Dim BallShadowArray
BallShadowArray = Array (BallShadow1, BallShadow2, BallShadow3,BallShadow004,BallShadow005)
Const anglecompensate = 15

Sub GraphicsTimer_Timer()
	Dim BOT, b
    BOT = GetBalls

	' switch off glowlight for removed Balls
	IF GlowBall Then
		For b = UBound(BOT) + 1 to 3
			If GlowBall and Glowing(b).state = 1 Then Glowing(b).state = 0 End If
		Next
	End If

    For b = 0 to UBound(BOT)
		' *** move ball shadow for max 3 balls ***
'		If BallShadow and b < 3 Then
'			If BOT(b).X < table1.Width/2 Then
'				BallShadowArray(b).X = ((BOT(b).X) - (50/6) + ((BOT(b).X - (table1.Width/2))/7)) + 10
'			Else
'				BallShadowArray(b).X = ((BOT(b).X) + (50/6) + ((BOT(b).X - (table1.Width/2))/7)) - 10
'			End If
'			BallShadowArray(b).Y = BOT(b).Y + 20 : BallShadowArray(b).Z = 1
'			If BOT(b).Z > 20 Then BallShadowArray(b).visible = 1 Else BallShadowArray(b).visible = 0 End If
'		End If
		' *** move glowball light for max 3 balls ***
		If GlowBall and b < 4 Then
			If Glowing(b).state = 0 Then Glowing(b).state = 1 end if
			Glowing(b).BulbHaloHeight = BOT(b).z + 25
			Glowing(b).x = BOT(b).x : Glowing(b).y = BOT(b).y + anglecompensate
			Glowing(b).falloff=GlowAura 'GlowBlob Auroa radius
			Glowing(b).intensity=GlowIntensity 'Glowblob intensity
		End If
	Next
End Sub

Sub Glowball_Init
	ChangeBall(ChooseBall)
	If GlowBall Then GraphicsTimer.enabled = True End If
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


Set ST1 = (new StandupTarget)(Target001, PTarget001,1, 0)
Set ST2 = (new StandupTarget)(Target002, PTarget002,2, 0)
Set ST3 = (new StandupTarget)(Target003, PTarget003,3, 0)
Set ST4 = (new StandupTarget)(Target004, PTarget004,4, 0)
Set ST5 = (new StandupTarget)(Target005, PTarget005,5, 0)
Set ST6 = (new StandupTarget)(Target006, PTarget006,6, 0)
Set ST7 = (new StandupTarget)(Target007, PTarget007,7, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
'   STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST1, ST2, ST3, ST4, ST5, ST6, ST7)

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
	If UBound(BOT) =Lob- 1 Then Exit Sub

'Rotate the idols
    Idol1.Rotz = -120 + (BOT(1).Y)\15
    Idol2.Rotz = 120 - (BOT(1).Y)\15
	Tiny2.Rotz = 90 - (BOT(1).Y)\15
    

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
 
'***************************************************************************
' VR Plunger Code
'***************************************************************************
Sub TimerVRPlunger_Timer
	If PinCab_Shooter.Y < 100 then
		PinCab_Shooter.Y = PinCab_Shooter.Y + 5
	End If
End Sub

Sub TimerVRPlunger1_Timer
	PinCab_Shooter.Y = 0 + (5* Plunger.Position) - 20
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
			zCol_Rubber_Corner_016.y = 1557.32
			Primitive12.y = 1557.32
			RubberOutlaneLeftEasy.visible = True
			RubberOutlaneLeftMed.visible = False
			RubberOutlaneLeftHard.visible = False
		Case 1
			zCol_Rubber_Corner_016.y = 1559.32
			Primitive12.y = 1559.32
			RubberOutlaneLeftEasy.visible = False
			RubberOutlaneLeftMed.visible = True
			RubberOutlaneLeftHard.visible = False
		Case 2
			zCol_Rubber_Corner_016.y = 1561.32
			Primitive12.y = 1561.32
			RubberOutlaneLeftEasy.visible = False
			RubberOutlaneLeftMed.visible = False
			RubberOutlaneLeftHard.visible = True
	End Select
End Sub

Sub UpdateRightOutlanePosts(Opt)
	Select Case Opt
		Case 0
			zCol_Rubber_Corner_005.y = 1556.48
			Primitive1.y = 1556.48
			RubberOutlaneRightEasy.visible = True
			RubberOutlaneRightMed.visible = False
			RubberOutlaneRightHard.visible = False
		Case 1
			zCol_Rubber_Corner_005.y = 1558.48
			Primitive1.y = 1558.48
			RubberOutlaneRightEasy.visible = False
			RubberOutlaneRightMed.visible = True
			RubberOutlaneRightHard.visible = False
		Case 2
			zCol_Rubber_Corner_005.y = 1560.48
			Primitive1.y = 1560.48
			RubberOutlaneRightEasy.visible = False
			RubberOutlaneRightMed.visible = False
			RubberOutlaneRightHard.visible = True
	End Select
End Sub

'***************************************************************************

'****Flash*****
Sub Flashlogo (fDuration, fPeriod)
	FlashlogoTimer.interval = fPeriod
	FlashlogoTimer.enabled = True
	StoplogoTimer.interval = fDuration
	StoplogoTimer.Enabled = True
End Sub

Sub FlashlogoTimer_Timer
	if eyeballs.visible = false Then
		eyeballs.visible = True
	Else
		eyeballs.visible = False
	End If
End Sub

Sub StoplogoTimer_Timer
	StoplogoTimer.enabled = False
	FlashlogoTimer.enabled = False
	eyeballs.visible = False
End Sub


Sub baxingtimer_Timer
countr = countr + 1 : If Countr > 2 then Countr = 1 : end If
select case countr
case 1 : bax001.z=55:bax002.z=-200
case 2 : bax001.z=-200:bax002.z=55

end Select
End Sub 
Sub StartSPAL
 baxingtimer.enabled = 1

End Sub

Sub StopSPAL
 baxingtimer.enabled = 0

End Sub


'*****Ramplight*****

Sub Flashramplight1 (eDuration, ePeriod)
	Flashramplight1Timer.interval = ePeriod
	Flashramplight1Timer.enabled = True
	Stopramplight1Timer.interval = eDuration
	Stopramplight1Timer.Enabled = True
End Sub

Sub Flashramplight1Timer_Timer
	if bluelit.visible = false Then
		bluelit.visible = True
	Else
		bluelit.visible = False
	End If
End Sub

Sub Stopramplight1Timer_Timer
	Stopramplight1Timer.enabled = False
	Flashramplight1Timer.enabled = False
	bluelit.visible = False
End Sub

Sub Flashramplight2 (gDuration, gPeriod)
	Flashramplight2Timer.interval = gPeriod
	Flashramplight2Timer.enabled = True
	Stopramplight2Timer.interval = gDuration
	Stopramplight2Timer.Enabled = True
End Sub

Sub Flashramplight2Timer_Timer
	if redlit.visible = false Then
		redlit.visible = True
	Else
		redlit.visible = False
	End If
End Sub

Sub Stopramplight2Timer_Timer
	Stopramplight2Timer.enabled = False
	Flashramplight2Timer.enabled = False
	redlit.visible = False
End Sub

'****** Sign light flashing*********

Dim SignLightColour
SignLightColour = 1

Sub SignLightTimer_Timer()
	If SignLightColour = 1 Then
		bluesign.visible = True
		yellowsign.visible = False
		bluesignoff.visible = False
		yellowsignoff.visible = True
		SignLightColour = 2
		exit Sub
	end If
	If SignLightColour = 2 Then
		bluesign.visible = False
		yellowsign.visible = True
		bluesignoff.visible = True
		yellowsignoff.visible = False
		SignLightColour = 1
		exit Sub
	end If
End Sub


Dim MyPi, BoatStep, BoatDir
MyPi = Round(4 * Atn(1), 6) / 90
BoatStep = 0


Sub BoatTimer_Timer()
    BoatDir = SIN(BoatStep * MyPi)
    BoatStep = (BoatStep + 1)MOD 360
    Fetus.Z = Fetus.Z + BoatDir * 0.1
    
End Sub
