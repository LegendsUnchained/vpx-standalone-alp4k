' ****************************************************************
'                       VISUAL PINBALL X .7
'                Daredevil and the Defenders v.1,0
' ****************************************************************

Option Explicit
Randomize

'***DOF Configs at bottom of script******

'*******************************************
'  ZOPT: User Options
'*******************************************

'----- VR Room -----

Const VRRoomChoice = 0			  ' 1 - Minimal Room
Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		' Level of ramp rolling volume. Value between 0 and 1

Const SongVolume = 1 ' 1 is full volume. Value is from 0 to 1

'*******************************************
'  ZCON: Constants and Global Variables
'*******************************************

Const BallSize = 50				 'Ball diameter in VPX units; must be 50
Const BallMass = 1				  'Ball mass must be 1
Const tnob = 19					  'Total number of balls the table can hold
Const lob = 0					   'Locked balls
Const cGameName = "Daredevil"'The unique alphanumeric name for this table

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

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
'or False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = True

' Define any Constants
Const myVersion = "1.0"
Const MaxPlayers = 4
Const BallSaverTime = 15 ' value in seconds
Const MaxMultiplier = 99 ' almost no limit in this game
Const BallsPerGame = 3   ' 3 or 5
Const MaxMultiballs = 5  ' max number of balls during multiballs

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

'VR uses FlexDMD to display DMD.
Dim VR_Obj, VRMode

If RenderingMode = 2 or LiveViewVRSim = 1 Then
	VRMode = True
    UseFlexDMD = True
	lrail.Visible = 0
	rrail.Visible = 0
	rghtshadow.Visible = 0
    primitive028.visible = 0
    leftshadow.Visible = 0
	Pincab_Backglass.BlendDisableLighting = 1
	For Each VR_Obj in VR_Cabinet:VR_Obj.Visible = 1:Next
	For Each VR_Obj in VR_Room:VR_Obj.Visible = 1:Next
Else
    'UseFlexDMD = False     'This is commented out for Cabinet Users. Comment Back for VR
	For Each VR_Obj in VR_Cabinet:VR_Obj.Visible = 0:Next
	For Each VR_Obj in VR_Room:VR_Obj.Visible = 0:Next
End If


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
	DoSTAnim				'handle stand up target animations
End Sub

'The CorTimer interval should be 10. It's sole purpose is to update the Cor calculations
CorTimer.Interval = 10
Sub CorTimer_Timer(): Cor.Update: End Sub

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
Dim RemoveTrustPost
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
Dim bWorldPFActive

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock
Dim BallsInHole
Dim bReverseFlippers

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

Dim plungerIM 'used mostly as an autofire plunger
Dim ttable
Dim EggMagnet
Dim x

dim countr
countr = 0
FireTimer.enabled = 1

dim countrr
countrr = 0
TorTimer.enabled = 1

dim countrrr
countrrr = 0
RainTimer.enabled = 1

dim countr2
countr2 = 0
BEYEtimer.enabled = 1
BEYEtimer.enabled = False
BEYETimerstop.enabled = False

dim countr3
countr3 = 1
PUNISHERtimer.enabled = 1
PUNISHERtimer.enabled = False
PUNISHERTimerstop.enabled = False

dim countr4
countr4 = 1
ELEKTRAtimer.enabled = 1
ELEKTRAtimer.enabled = False
ELEKTRATimerstop.enabled = False

dim countr5
countr5 = 1
JESSICAtimer.enabled = 1
JESSICAtimer.enabled = False
JESSICATimerstop.enabled = False

dim countr6
countr6 = 1
DAREtimer.enabled = 1
DAREtimer.enabled = False
DARETimerstop.enabled = False

dim countr7
countr7 = 1
NINJAtimer.enabled = 1
NINJAtimer.enabled = False
NINJATimerstop.enabled = False

dim countr8
countr8 = 1
NINJA2timer.enabled = 1
NINJA2timer.enabled = False
NINJA2Timerstop.enabled = False

dim countr9
countr9 = 1
SPIDtimer.enabled = 1
SPIDtimer.enabled = False
SPIDTimerstop.enabled = False

dim countrrrr
countrrr = 1
PURPLEtimer.enabled = 1
PURPLEtimer.enabled = False
PURPLETimerstop.enabled = False

dim countrM
countrM = 1
MOLEtimer.enabled = 1
MOLEtimer.enabled = False
MOLETimerstop.enabled = False

dim countrSL
countrSL = 1
STANtimer.enabled = 1
STANtimer.enabled = False
STANTimerstop.enabled = False

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
    Randomize

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 46 ' Plunger Power
    Const IMTime = 0.25       ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    'Venusaur turntable
    Set ttable = New cvpmTurnTable
    With ttable
        .InitTurnTable Magnet1, 10
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

    ' initialse any other flags
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
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
    EndOfGame()
    MystUnsee
    KINGUnsee
    RAINUnsee
    StanHide
    FAKEMAN1Down.Enabled = True
	FAKEMAN2Down.Enabled = True
    FAKEMAN3Down.Enabled = True
    FAKEMAN4Down.Enabled = True
    Sawtimer.enabled = False
    Ringspin.enabled = False

' Start the RealTime timer
    RealTime.Enabled = 1
    
    ' Load table color
    LoadLut
End Sub

Dim LeftOutlaneDifficulty,RightOutlaneDifficulty,nBallsPerGame

Dim SidewallChoice: SidewallChoice = 0
Dim RailChoice: RailChoice = True
'//////////////F12 Menu//////////////
' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings
Dim dspTriggered : dspTriggered = False
Sub Table1_OptionEvent(ByVal eventId)

	'Difficulty
	RemoveTrustPost = Table1.Option("Remove Trust Post", 0, 1, 1, 1, 0, Array("False", "True (Default)"))
	CheckTrustPost 

	'Outlane Difficulty
	LeftOutlaneDifficulty = Table1.Option("Left Outlane Difficulty", 0, 2, 1, 1, 0, Array("Easy", "Medium (Default)", "Hard"))
	UpdateLeftOutlanePosts LeftOutlaneDifficulty

	RightOutlaneDifficulty = Table1.Option("Right Outlane Difficulty", 0, 2, 1, 1, 0, Array("Easy", "Medium (Default)", "Hard"))
	UpdateRightOutlanePosts RightOutlaneDifficulty
	If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If

    	RailChoice = Table1.Option("Rails Visible", 0, 1, 1, 1, 0, Array("False", "True (Default)"))
	SetRails RailChoice

	SidewallChoice = Table1.Option("Sidewall Art", 0, 2, 1, 0, 0, Array("Desktop", "PinCab", "Wood"))
	SetSidewall SidewallChoice
        If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
	End Sub

Sub SetRails(Opt)
	Select Case Opt
		Case 0:
			lrail.Visible = 0
			rrail.Visible = 0
		Case 1:
			lrail.Visible = 1
			rrail.Visible = 1
	End Select
End Sub

Sub SetSidewall(Opt)
	Select Case Opt
		Case 0:
			PinCab_Blades2.visible = 0
			Primitive028.visible = 1
		Case 1:
			Primitive028.visible = 0
			PinCab_Blades2.Image = "Sidewalls D"
			PinCab_Blades2.visible = 1
        Case 2:
			Primitive028.visible = 0
			PinCab_Blades2.Image = "Sidewalls Black"
			PinCab_Blades2.visible = 1
    End Select
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

'******************************************************
'  ZANI: Misc Animations
'******************************************************

Sub LeftFlipper_Animate
	dim a: a = LeftFlipper.CurrentAngle
	LeftFlipperTop.RotZ = a
	'Add any left flipper related animations here
End Sub

Sub RightFlipper_Animate
	dim a: a = RightFlipper.CurrentAngle
	RightFlipperTop.RotZ = a
	'Add any right flipper related animations here
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

        If keycode = LeftFlipperKey Then
			if bReverseFlippers Then
				PinCab_Right_Flipper_Button.X = PinCab_Right_Flipper_Button.X - 10			
			Else
				PinCab_Left_Flipper_Button.X = PinCab_Left_Flipper_Button.X + 10
			End If
		End If
	
		If keycode = RightFlipperKey Then
			if bReverseFlippers Then
				PinCab_Left_Flipper_Button.X = PinCab_Left_Flipper_Button.X + 10			
			Else			
				PinCab_Right_Flipper_Button.X = PinCab_Right_Flipper_Button.X - 10
			End If
		End If

    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If

    If Keycode = AddCreditKey Then
		Select Case Int(Rnd * 3)
			Case 0
				PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1
				PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2
				PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
        Credits = Credits + 1
        DOF 125, DOFOn
        ' Fix: don't overwrite the high score name entry screen (the coin message and the
        ' name entry were redrawing over each other, making the DMD go haywire)
        If(Tilted = False) And Not hsbModeActive Then
            DMDFlush
            DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
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
        PlungerIM.AutoFire:AnimateRattata
    End If
    If bGameInPlay AND NOT Tilted Then
		If keycode = LeftTiltKey Then
			Nudge 90, 1
			SoundNudgeLeft
			CheckTilt
		End If
		If keycode = RightTiltKey Then
			Nudge 270, 1
			SoundNudgeRight
			CheckTilt
		End If
		If keycode = CenterTiltKey Then
			Nudge 0, 1
			SoundNudgeCenter
			CheckTilt
		End If

        If keycode = LeftFlipperKey Then
			if bReverseFlippers Then
				SolRFlipper True	'This would be called by the solenoid callbacks if using a ROM			
			Else
				SolLFlipper True	'This would be called by the solenoid callbacks if using a ROM
			End If
		End If
	
		If keycode = RightFlipperKey Then
			if bReverseFlippers Then
				SolLFlipper True	'This would be called by the solenoid callbacks if using a ROM			
			Else
				SolRFlipper True	'This would be called by the solenoid callbacks if using a ROM
			End If
		End If

        If keycode = StartGameKey Then
            SoundStartButton
			If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    PlaySound "po_fanfare1"
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, ""
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        PlaySound "po_fanfare1"
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, ""

                        If Credits < 1 Then DOF 125, DOFOff
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
				SoundStartButton
                If(bFreePlay = True)Then
                    If(BallsOnPlayfield = 0)Then
                        ResetForNewGame()
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            If Credits < 1 Then DOF 125, DOFOff
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
        If keycode = LeftFlipperKey Then
			if bReverseFlippers Then
				PinCab_Right_Flipper_Button.X = PinCab_Right_Flipper_Button.X + 10			
			Else
				PinCab_Left_Flipper_Button.X = PinCab_Left_Flipper_Button.X - 10
			End If
		End If
	
		If keycode = RightFlipperKey Then
			if bReverseFlippers Then
				PinCab_Left_Flipper_Button.X = PinCab_Left_Flipper_Button.X - 10			
			Else
				PinCab_Right_Flipper_Button.X = PinCab_Right_Flipper_Button.X + 10
			End If
		End If
    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT
    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then 
			if bReverseFlippers Then
				SolRFlipper False			
			Else
				SolLFlipper False
			End If
		End If	
        If keycode = RightFlipperKey Then
			if bReverseFlippers Then
				SolLFlipper False			
			Else		
				SolRFlipper False
			End If
		End If
    End If
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
    If UseFlexDMD Then FlexDMD.Run = False
    if B2SOn = True Then Controller.stop
End Sub

'*******************************************
'	ZFLP: Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled)
    If Enabled Then
        FlipperActivate LeftFlipper, LFPress
        LF.Fire
        DOF 101, DOFOn  ' Left flipper up
        If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
            RandomSoundReflipUpLeft LeftFlipper
        Else
            SoundFlipperUpAttackLeft LeftFlipper
            RandomSoundFlipperUpLeft LeftFlipper
            RotateLaneLightsLeft
        End If
    Else
        FlipperDeActivate LeftFlipper, LFPress
        LeftFlipper.RotateToStart
        DOF 101, DOFOff  ' Left flipper down
        If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
            RandomSoundFlipperDownLeft LeftFlipper
        End If
        FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        FlipperActivate RightFlipper, RFPress
        RF.Fire
        DOF 102, DOFOn  ' Right flipper up
        If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
            RandomSoundReflipUpRight RightFlipper
        Else
            SoundFlipperUpAttackRight RightFlipper
            RandomSoundFlipperUpRight RightFlipper
            RotateLaneLightsRight
        End If
    Else
        FlipperDeActivate RightFlipper, RFPress
        RightFlipper.RotateToStart
        DOF 102, DOFOff  ' Right flipper down
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
    If(Tilt > TiltSensitivity)AND(Tilt < 15)Then 'show a warning
        DMD "_", CL(1, "CAREFUL"), "", eNone, eBlinkFast, eNone, 500, True, ""
    End if
    If Tilt > 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD "", CL(1, "TILT"), "", eNone, eNone, eNone, 200, False, ""
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
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
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        Bumper3.Threshold = 100
        Bumper4.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        LeftSlingshot1.Disabled = 1
        RightSlingshot1.Disabled = 1
    Else
        'turn back on GI and the lights
        'GiOn
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        Bumper3.Threshold = 1
        Bumper4.Threshold = 1
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
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
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
    Dim tmp
    tmp = INT(RND * 4)
    Select Case tmp
        Case 0:PlaySong "mu_battle1"
        Case 1:PlaySong "mu_battle2"
        Case 2:PlaySong "mu_battle3"
        Case 3:PlaySong "mu_battle4"
    End Select
End Sub
Dim sMultiModePhase   ' assign as a variable before calling change score

Sub ChangeSong
    If(BallsOnPlayfield = 0)Then
        PlaySong "mu_end"
    Else
        If bMultiballMode Then
        Select Case sMultiModePhase
            Case "ModeA"
                        PlaySong "mu_multiA"
            Case "ModeB"
                        PlaySong "mu_multiB"
            Case "ModeC"
                        PlaySong "mu_multiC"
            Case Else
                ' Can play a default song here if you want 
        End Select
        Else
            If bCatchemMode Then
                PlayBattleSong
            Else
                PlaySong "mu_main"
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
            LightSeqGi.UpdateInterval = 25
            LightSeqGi.Play SeqBlinking, , 10, 15
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 15
            LightSeqGi.Play SeqBlinking, , 15, 10
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 25
            LightSeqInserts.Play SeqBlinking, , 10, 15
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 15
            LightSeqInserts.Play SeqBlinking, , 15, 10
    End Select
End Sub

' Pokémon sound efects and fanfares

Sub PlaySoundEffect()
    Dim n
    n = "po_effect" & INT(RND * 36) + 1
    PlaySound n, 0, 1, pan(ActiveBall)
End Sub

Sub PlayFanfare()
    Dim n
    n = "po_fanfare" & INT(RND * 6) + 1
    PlaySound n
End Sub

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
    Next

    ' initialise any other flags
    bMultiBallMode = False
    MurdockLightsreset
    MAYHEMLightsreset
    MYSTUnsee
    KINGUnsee
    RAINUnsee
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

    'Change the music ?

    'Reset any table specific
    TargetBonus = 0
    BumperBonus = 0
    HoleBonus = 0
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    BallRelease.CreateSizedball BallSize / 2
    UpdateBallImage
    BallsOnPlayfield = BallsOnPlayfield + 1
    RandomSoundBallRelease BallRelease
    BallRelease.Kick 90, 4
    DOF 123, DOFPulse  ' Ball release solenoid
    If BallsOnPlayfield > 1 Then
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
        If BallsOnPlayfield < MaxMultiballs Then
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
    Dim BonusDelayTime, AwardPoints, TotalBonus
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)
    If(Tilted = False)Then
        AwardPoints = 0:TotalBonus = 0

' add in any bonus points (multipled by the bonus multiplier)
'AwardPoints = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer)
'AddScore AwardPoints
'debug.print "Bonus Points = " & AwardPoints
'DMD "", CL(1, "BONUS: " & BonusPoints(CurrentPlayer) & " X" & BonusMultiplier(CurrentPlayer) ), "", eNone, eBlink, eNone, 1000, True, ""

'this table uses several bonus
        AwardPoints = TargetBonus * 1000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "TARGET BONUS: " & TargetBonus), "", eBlink, eNone, eNone, 800, False, "po_bonus1"

        AwardPoints = HoleBonus * 5000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "HOLE BONUS: " & HoleBonus), "", eBlink, eNone, eNone, 800, False, "po_bonus2"

        AwardPoints = PokemonBonusAward(CurrentPlayer) * 62500
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "VILLAINS CAUGHT: " & PokemonBonus(CurrentPlayer)), "", eBlink, eNone, eNone, 800, False, "po_bonus3"

        AwardPoints = EggBonus(CurrentPlayer) * 250000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "BASES FOUND: " & EggBonus(CurrentPlayer)), "", eBlink, eNone, eNone, 800, False, "po_bonus4"

        AwardPoints = BumperBonus * 100000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "BUMPER BONUS: " & BumperBonus), "", eBlink, eNone, eNone, 800, False, "po_bonus5"

        If Balls = BallsPerGame Then 'this is the last ball, add the coins left to the score
            AwardPoints = Coins(CurrentPlayer) * 1000
            TotalBonus = TotalBonus + AwardPoints
            DMD CL(0, FormatScore(AwardPoints)), CL(1, "COIN BONUS: " & Coins(CurrentPlayer)), "", eBlink, eNone, eNone, 800, False, "po_bonus6"
        End If

        DMD CL(0, FormatScore(TotalBonus)), CL(1, "TOTAL BONUS " & " X" & BonusMultiplier(CurrentPlayer)), "", eBlinkFast, eNone, eNone, 1500, True, "po_bonus7"
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        Addscore TotalBonus
        FAKEMAN1Down.Enabled = True
	    FAKEMAN2Down.Enabled = True
        FAKEMAN3Down.Enabled = True
        FAKEMAN4Down.Enabled = True
        Select Case Balls
 
    Case 1: PlaySound""
    Case 2: PlaySound""
    Case 3: PlaySound"gameover"

End Select
        ' add a bit of a delay to allow for the bonus points to be shown & added up
        BonusDelayTime = 7000
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
    If(ExtraBallsAwards(CurrentPlayer) <> 0)Then
        'debug.print "Extra Ball"
        DMD "_", CL(1, "EXTRA BALL"), "_", eNone, eBlink, eNone, 1000, True, ""

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
    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball

    BallsOnPlayfield = BallsOnPlayfield - 1
    ' pretend to knock the ball into the ball storage mech
    RandomSoundDrain Drain

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
            DMD "_", CL(1, "BALL SAVED"), "SAVED", eNone, eBlinkfast, eNone, 800, True, ""
            
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    
                    ' not in multiball mode any more
                    bMultiBallMode = False
		            sMultiModePhase = ""
                    If bcoinfrenzy Then StopCoinFrenzyTimer_Timer
			' Call anything else to end multiball modes 
                    MurdockLightsreset
                    MAYHEMLightsreset
                    MYSTUnsee
                    KINGUnsee
                    RAINUnsee
                    ChangeGi white
                    FAKEMAN1Down.Enabled = True
	                FAKEMAN2Down.Enabled = True
                    FAKEMAN3Down.Enabled = True
                    FAKEMAN4Down.Enabled = True
               
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
                    ResetJackpotLights
                    ChangeSong
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
                ChangeSong
                StanHide
                ' handle the end of ball (change player, high score entry etc..)
                EndOfBall()
                
                ' End Modes and timers
                If bCatchemMode Then StopCatchem_Timer
                'If bcoinfrenzy Then StopCoinFrenzyTimer_Timer ' redundant call moved up to BOP = 1
                If bPikachuTargetMode Then PikachuTargetTimer_Timer
                If bCharizardMode Then StopCharizardTimer_Timer
                If bRampBonus Then StopRampBonusTimer_Timer
                If bLoopBonus Then StopLoopBonusTimer_Timer
                ReduceBallType
                ChangeGi white
            End If
        End If
    End If
if MOLEtimer.enabled = false Then
       MOLEtimer.enabled = True
       MOLEtimerstop.enabled = True
	End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    LaunchLight.State = 2
    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        ' 'debug.print "autofire the ball"
        PlungerIM.AutoFire:AnimateRattata
        DOF 121, DOFPulse
        bAutoPlunger = False
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
        EnableBallSaver BallSaverTime
    End If
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
    DMDScoreNow
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
    If(Tilted = False)Then
        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * BallType
    End if
' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board
'
Sub AddBonus(points)
    If(Tilted = False)Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    End if
' you may wish to check to see if the player has gotten a replay
End Sub

Sub AddCoin(n)
    If(Tilted = False)Then
        ' add the coins to the current players coin variable
        Coins(CurrentPlayer) = Coins(CurrentPlayer) + n
    ' update the score displays
    'DMDScore
    End if

    ' check if there is enough coins to enable the update ball
    If Coins(CurrentPlayer) > 249 Then
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
    If(Tilted = False)Then

        If(bMultiBallMode = True)Then
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
        LightBigBonus.State = 0
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
        DMD "_", CL(1, ("EXTRA BALL WON")), "EBALL", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        GiEffect 1
        LightEffect 2
    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL(1, ("EXTRA GAME WON")), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    Credits = Credits + 1
    DOF 125, DOFOn
    GiEffect 1
    LightEffect 1
End Sub

Sub AwardJackpot()
    DMD CL(0, FormatScore(Jackpot)), CL(1, ("")), "JACKPOT", eBlinkFast, eBlinkFast, eNone, 1000, True, ""
    If bMultiBallMode Then PlaySound"quoteDD"&RndNbr(13)
    DOF 126, DOFPulse
    PlayFanfare
    AddScore Jackpot
    AddJackpot 100000
    GiEffect 1
    LightEffect 2
End Sub

Sub AwardSuperJackpot()
    DMD CL(0, FormatScore(Jackpot * PokemonLevel)), CL(1, ("")), "JACKPOT2", eBlinkFast, eBlinkFast, eNone, 1000, True, ""
    DOF 126, DOFPulse
    PlayFanfare
    AddScore Jackpot * PokemonLevel
    AddJackpot 100000
    GiEffect 1
    LightEffect 2
End Sub

Sub AwardSkillshot()
    DMD CL(0, FormatScore(SkillshotValue)), CL(1, ("SKILLSHOT")), "bkborder", eBlinkFast, eBlink, eNone, 1000, True, ""
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
    PlaySound ""
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
    ' Fix: clear the name entry screen. Nothing redrew the DMD until the next score
    ' change, so in multiplayer the next player's ball was ready behind "YOUR NAME:"
    DMDScoreNow
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
    Dim tmp, tmp1, tmp2
    if(dqHead = dqTail)Then
        tmp = RL(0, FormatScore(Score(Currentplayer)))
        'tmp = CL(0, FormatScore(Score(Currentplayer) ) )
        tmp1 = CL(1, "PLAYER" & CurrentPlayer & " [" & Balls & " \" & PokemonBonus(CurrentPlayer) & " ]" & Coins(Currentplayer))
        'tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
        'tmp1 = FormatScore(Bonuspoints(Currentplayer) ) & " X" &BonusMultiplier(Currentplayer)
        tmp2 = ""
        If bCatchemMode Then
            tmp1 = RL(1, CatchMaxHits-CatchHits & " HITS LEFT")
            tmp2 = pokemon(CatchID)
        End If
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
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

Function CL(id, NumString)
    Dim Temp, TempStr
    Temp = (dCharsPerLine(id)- Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(id, NumString)
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
    For i = 0 to 255:Chars(i) = "dempty":Next

    Chars(32) = "dempty" 'space
    Chars(33) = "dexcl"  '!
    '    Chars(34) = '"
    '    Chars(36) = '$
    '    Chars(39) = ''
    '    Chars(42) = '*
    '    Chars(43) = '+
    '    Chars(45) = '-
    Chars(46) = "ddot"  '.
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
    Chars(65) = "da"    'A
    Chars(66) = "db"    'B
    Chars(67) = "dc"    'C
    Chars(68) = "dd"    'D
    Chars(69) = "de"    'E
    Chars(70) = "df"    'F
    Chars(71) = "dg"    'G
    Chars(72) = "dh"    'H
    Chars(73) = "di"    'I
    Chars(74) = "dj"    'J
    Chars(75) = "dk"    'K
    Chars(76) = "dl"    'L
    Chars(77) = "dm"    'M
    Chars(78) = "dn"    'N
    Chars(79) = "do"    'O
    Chars(80) = "dp"    'P
    Chars(81) = "dq"    'Q
    Chars(82) = "dr"    'R
    Chars(83) = "ds"    'S
    Chars(84) = "dt"    'T
    Chars(85) = "du"    'U
    Chars(86) = "dv"    'V
    Chars(87) = "dw"    'W
    Chars(88) = "dx"    'X
    Chars(89) = "dy"    'Y
    Chars(90) = "dz"    'Z
    Chars(91) = "dball" '[
    Chars(92) = "dpika" '|
    Chars(93) = "dcoin" ']
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

'********************
' Real Time updatess
'********************
'used for all the real time updates

Sub RealTime_Timer
    coin.RotX = Spinner.CurrentAngle


hl1on.blenddisablelighting = HatchLight1.getinplayintensity * 40
hl1off.blenddisablelighting = hatchlight1.getinplayintensity + 1
hl2on.blenddisablelighting = HatchLight2.getinplayintensity * 40
hl2off.blenddisablelighting = hatchlight2.getinplayintensity + 1
hl3on.blenddisablelighting = HatchLight3.getinplayintensity * 40
hl3off.blenddisablelighting = hatchlight3.getinplayintensity + 1
hl4on.blenddisablelighting = HatchLight4.getinplayintensity * 40
hl4off.blenddisablelighting = hatchlight4.getinplayintensity + 1
LL1on.blenddisablelighting = LockLight1.getinplayintensity * 40
LL1off.blenddisablelighting = LockLight1.getinplayintensity + 1
LL2on.blenddisablelighting = LockLight2.getinplayintensity * 40
LL2off.blenddisablelighting = LockLight2.getinplayintensity + 1
MM1on.blenddisablelighting = MultiLight1.getinplayintensity * 40
MM1off.blenddisablelighting = MultiLight1.getinplayintensity + 1
MM2on.blenddisablelighting = MultiLight2.getinplayintensity * 40
MM2off.blenddisablelighting = MultiLight2.getinplayintensity + 1
MM3on.blenddisablelighting = MultiLight3.getinplayintensity * 40
MM3off.blenddisablelighting = MultiLight3.getinplayintensity + 1
MM4on.blenddisablelighting = MultiLight4.getinplayintensity * 40
MM4off.blenddisablelighting = MultiLight4.getinplayintensity + 1
MM5on.blenddisablelighting = MultiLight5.getinplayintensity * 40
MM5off.blenddisablelighting = MultiLight5.getinplayintensity + 1
LV1on.blenddisablelighting = LightLevel1.getinplayintensity * 40
LV1off.blenddisablelighting = LightLevel1.getinplayintensity + 1
LV2on.blenddisablelighting = LightLevel2.getinplayintensity * 40
LV2off.blenddisablelighting = LightLevel2.getinplayintensity + 1
LV3on.blenddisablelighting = LightLevel3.getinplayintensity * 40
LV3off.blenddisablelighting = LightLevel3.getinplayintensity + 1
LV4on.blenddisablelighting = LightLevel4.getinplayintensity * 40
LV4off.blenddisablelighting = LightLevel4.getinplayintensity + 1
LO1on.blenddisablelighting = LeftOutlane.getinplayintensity * 40
LO1off.blenddisablelighting = LeftOutlane.getinplayintensity + 1
LI1on.blenddisablelighting = LeftInlane1.getinplayintensity * 40
LI1off.blenddisablelighting = LeftInlane1.getinplayintensity + 1
LI2on.blenddisablelighting = LeftInlane2.getinplayintensity * 40
LI2off.blenddisablelighting = LeftInlane2.getinplayintensity + 1
RI1on.blenddisablelighting = RightInlane.getinplayintensity * 40
RI1off.blenddisablelighting = RightInlane.getinplayintensity + 1
ROon.blenddisablelighting = RightOutlane.getinplayintensity * 40
ROoff.blenddisablelighting = RightOutlane.getinplayintensity + 1
BB1on.blenddisablelighting = LightBigBonus.getinplayintensity * 40
BB1off.blenddisablelighting = RightOutlane.getinplayintensity + 1
LB1on.blenddisablelighting = LightBonus2x.getinplayintensity * 40
LB1off.blenddisablelighting = LightBonus2x.getinplayintensity + 1
LB2on.blenddisablelighting = LightBonus3x.getinplayintensity * 40
LB2off.blenddisablelighting = LightBonus3x.getinplayintensity + 1
SA1on.blenddisablelighting = LightShootAgain.getinplayintensity * 40
SA1off.blenddisablelighting = LightShootAgain.getinplayintensity + 1
L53on.blenddisablelighting = l53.getinplayintensity * 40
L53off.blenddisablelighting = l53.getinplayintensity + 1
BULon.blenddisablelighting = BallUpdateLight.getinplayintensity * 40
BULoff.blenddisablelighting = BallUpdateLight.getinplayintensity + 1
HOLE1on.blenddisablelighting = HoleLight5.getinplayintensity * 40
HOLE1off.blenddisablelighting = HoleLight5.getinplayintensity + 1
LA6on.blenddisablelighting = LightArrow6.getinplayintensity * 40
LA6off.blenddisablelighting = LightArrow6.getinplayintensity + 1
LA5on.blenddisablelighting = LightArrow5.getinplayintensity * 40
LA5off.blenddisablelighting = LightArrow5.getinplayintensity + 1
LA4on.blenddisablelighting = LightArrow4.getinplayintensity * 40
LA4off.blenddisablelighting = LightArrow4.getinplayintensity + 1
LA3on.blenddisablelighting = LightArrow3.getinplayintensity * 40
LA3off.blenddisablelighting = LightArrow3.getinplayintensity + 1
LA2on.blenddisablelighting = LightArrow2.getinplayintensity * 40
LA2off.blenddisablelighting = LightArrow2.getinplayintensity + 1
LA1on.blenddisablelighting = LightArrow1.getinplayintensity * 40
LA1off.blenddisablelighting = LightArrow1.getinplayintensity + 1
EBon.blenddisablelighting = ExtraBallLight.getinplayintensity * 40
EBoff.blenddisablelighting = ExtraBallLight.getinplayintensity + 1
Lockon.blenddisablelighting = LockLight.getinplayintensity * 40
Lockoff.blenddisablelighting = Locklight.getinplayintensity + 1
HO1on.blenddisablelighting = HoleLight1.getinplayintensity * 40
HO1off.blenddisablelighting = HoleLight1.getinplayintensity + 1
HO2on.blenddisablelighting = HoleLight2.getinplayintensity * 40
HO2off.blenddisablelighting = HoleLight2.getinplayintensity + 1
HO3on.blenddisablelighting = HoleLight3.getinplayintensity * 40
HO3off.blenddisablelighting = HoleLight3.getinplayintensity + 1
HO4on.blenddisablelighting = HoleLight4.getinplayintensity * 40
HO4off.blenddisablelighting = HoleLight4.getinplayintensity + 1
PP1on.blenddisablelighting = PikaLight1.getinplayintensity * 40
PP1off.blenddisablelighting = PikaLight1.getinplayintensity + 1
PP2on.blenddisablelighting = PikaLight2.getinplayintensity * 40
PP2off.blenddisablelighting = PikaLight2.getinplayintensity + 1
PP3on.blenddisablelighting = PikaLight3.getinplayintensity * 40
PP3off.blenddisablelighting = PikaLight3.getinplayintensity + 1
PP4on.blenddisablelighting = PikaLight4.getinplayintensity * 40
PP4off.blenddisablelighting = PikaLight4.getinplayintensity + 1
PP5on.blenddisablelighting = PikaLight5.getinplayintensity * 40
PP5off.blenddisablelighting = PikaLight5.getinplayintensity + 1
PP6on.blenddisablelighting = PikaLight6.getinplayintensity * 40
PP6off.blenddisablelighting = PikaLight6.getinplayintensity + 1
PP7on.blenddisablelighting = PikaLight7.getinplayintensity * 40
PP7off.blenddisablelighting = PikaLight7.getinplayintensity + 1

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

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim ii
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER1 " &FormatScore(Score(1))), "", eNone, eNone, eNone, 3000, False, ""
        If PokemonBonus(1)Then
            DMD "", "VILLAINS CAUGHT " & PokemonBonus(1), "", eNone, eNone, eNone, 1000, False, ""
            For ii = 1 to PokemonBonus(1)
                DMD "", RL(1, Pokename(PokemonCaught(1, ii))), Pokemon(PokemonCaught(1, ii)), eNone, eNone, eNone, 1000, False, ""
            Next
            DMD "", "BASES " & EggBonus(1), "", eNone, eNone, eNone, 1000, False, ""
        End If
    End If
    If Score(2)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 3000, False, ""
        If PokemonBonus(2)Then
            DMD "", "VILLAINS CAUGHT " & PokemonBonus(2), "", eNone, eNone, eNone, 1000, False, ""
            For ii = 1 to PokemonBonus(2)
                DMD "", RL(1, Pokename(PokemonCaught(2, ii))), Pokemon(PokemonCaught(2, ii)), eNone, eNone, eNone, 1000, False, ""
            Next
            DMD "", "BASES " & EggBonus(2), "", eNone, eNone, eNone, 1000, False, ""
        End If
    End If
    If Score(3)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 3000, False, ""
        If PokemonBonus(3)Then
            DMD "", "VILLAINS CAUGHT " & PokemonBonus(3), "", eNone, eNone, eNone, 1000, False, ""
            For ii = 1 to PokemonBonus(3)
                DMD "", RL(1, Pokename(PokemonCaught(3, ii))), Pokemon(PokemonCaught(3, ii)), eNone, eNone, eNone, 1000, False, ""
            Next
            DMD "", "BASES " & EggBonus(3), "", eNone, eNone, eNone, 1000, False, ""
        End If
    End If
    If Score(4)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 3000, False, ""
        If PokemonBonus(4)Then
            DMD "", "VILLAINS CAUGHT " & PokemonBonus(4), "", eNone, eNone, eNone, 1000, False, ""
            For ii = 1 to PokemonBonus(4)
                DMD "", RL(1, Pokename(PokemonCaught(4, ii))), Pokemon(PokemonCaught(4, ii)), eNone, eNone, eNone, 1000, False, ""
            Next
            DMD "", "BASES " & EggBonus(4), "", eNone, eNone, eNone, 1000, False, ""
        End If
    End If
    DMD "", CL(1, "GAME OVER"), "", eNone, eBlink, eNone, 2000, False, ""
    If bFreePlay Then
        DMD "", CL(1, "FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits > 0 Then
            DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "", "", "jppresents", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "pokemonpinball", eNone, eNone, eNone, 4000, False, ""
    DMD "", CL(1, "ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0)), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
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

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 24:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

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
        Case 22:table1.ColorGradeImage = "LUT Murdock"
        Case 23:table1.ColorGradeImage = "LUT MAYHEM"
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


'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' tables walls and animations
Sub VPObjects_Init
End Sub

' tables variables and modes init

Dim Coins(4)
Dim BumperBonus, EggBonus(4), HoleBonus, PokemonBonus(4), PokemonBonusAward(4), PokemonCaught(4, 151), TargetBonus

Sub Game_Init()
    Dim ii, jj
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
        For jj = 0 to 151
            PokemonCaught(ii, jj) = 0
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
	bReverseFlippers = False
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
    LightArrow1.State = 2
    LightArrow6.State = 2
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
    Playsound "Punch"
    DOF 103, DOFPulse
    TimerJAL.Enabled = True
    If Tilted Then Exit Sub
	LS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotLeft Lemk
    DOF 105, DOFPulse
    LeftSling4.Visible = 1:LeftSling1.Visible = 0
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 110
    ' add some effect to the table?
    LUKE.RotY = 26
    LUKE.RotY = 26
    Gi2.State = 0
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14:LUKE.RotY = 14:LUKE2.RotY = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2:LUKE.RotY = 2:LUKE2.RotY = 2
        Case 3:LeftSLing2.Visible = 0:LeftSling1.Visible = 1:Lemk.RotX = -10:LUKE.RotY = 0:LUKE2.RotY = 0:Gi2.State = 1:LeftSlingShot.TimerEnabled = False
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    Playsound "Punch"
    DOF 104, DOFPulse
    FlashFISTS 800, 100
    TimerJAB.Enabled = True
    If Tilted Then Exit Sub
	RS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotRight Remk
    DOF 106, DOFPulse
    RightSling4.Visible = 1:RightSling1.Visible = 0
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 110
    ' add some effect to the table?
    IRONFIST.RotY = -26
    IRONFIST2.RotY = -26
    FISTS.RotY = -26
    Gi1.State = 0
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14:IRONFIST.RotY = -14:FISTS.RotY = -14:IRONFIST2.RotY = -14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2:IRONFIST.RotY = -2:FISTS.RotY = -2:IRONFIST2.RotY = -2
        Case 3:RightSLing2.Visible = 0:RightSLing1.Visible = 1:Remk.RotX = -10:IRONFIST.RotY = 0:FISTS.RotY = 0:IRONFIST2.RotY = 0:Gi1.State = 1:RightSlingShot.TimerEnabled = False
    End Select
    RStep = RStep + 1
End Sub

' Hatch Slingshots

Dim LStep1, RStep1

Sub LeftSlingShot1_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 143, DOFPulse, DOFContactors), Lemk1
    Sling3a.Visible = 1:Sling3.Visible = 0
    Lemk1.RotX = 26
    LStep1 = 0
    LeftSlingShot1.TimerEnabled = True
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
    PlaySoundAt SoundFXDOF("fx_slingshot", 144, DOFPulse, DOFContactors), Remk1
    Sling2a.Visible = 1:Sling2.Visible = 0
    Remk1.RotX = 26
    RStep1 = 0
    RightSlingShot1.TimerEnabled = True
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
    SoundSpinner Spinner
    DOF 136, DOFPulse
    AddScore 1000
    AddCoin 1
End Sub

'*********************
' Inlanes - Outlanes
'*********************

Sub LeftOutlaneTrigger_Hit()
    DOF 132, DOFPulse
    TimerJAL.Enabled = True
    If NOT bMultiBallMode Then PlaySound"quoteMM"&RndNbr(6)
    If Tilted Then Exit Sub
    PlaySoundEffect
    AddScore 10000
    Addcoin 50
    FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
    FlashForMs Light9, 1000, 50, 0
    FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
    FlashForMs Light10, 1000, 50, 0
    FlashForMs ap1, 1000, 50, 0
    FlashForMs ap2, 1000, 50, 0
    TargetBonus = TargetBonus + 1
    LeftOutlane.State = 1
    LastSwitchHit = "LeftOutlaneTrigger"
    CheckMultiplier
End Sub

Sub LeftInlaneTrigger1_Hit()
    DOF 133, DOFPulse
    
    If Tilted Then Exit Sub
    PlaySoundEffect
    AddScore 3000
    Addcoin 10
    TargetBonus = TargetBonus + 1
    LeftInlane1.State = 1
    LastSwitchHit = "LeftInlaneTrigger1"
    CheckMultiplier
End Sub

Sub LeftInlaneTrigger2_Hit()
    If NOT bMultiBallMode Then PlaySound"quoteL"&RndNbr(7)
    DOF 133, DOFPulse
    If Tilted Then Exit Sub
    PlaySoundEffect
    AddScore 3000
    Addcoin 10
    TargetBonus = TargetBonus + 1
    LeftInlane2.State = 1
    LastSwitchHit = "LeftInlaneTrigger2"
    CheckMultiplier
End Sub

Sub RightInlaneTrigger_Hit()
    DOF 134, DOFPulse
    If Tilted Then Exit Sub
    PlaySoundEffect
    AddScore 3000
    Addcoin 10
    TargetBonus = TargetBonus + 1
    RightInlane.State = 1
    CheckMultiplier
    LastSwitchHit = "RightInlaneTrigger"
End Sub

Sub RightOutlaneTrigger_Hit()
    FlashFISTS 800, 100
    TimerJAB.Enabled = True
    DOF 135, DOFPulse
    If NOT bMultiBallMode Then PlaySound"quoteI"&RndNbr(6)
    If Tilted Then Exit Sub
    PlaySoundEffect
    AddScore 10000
    Addcoin 50
    FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
    FlashForMs Light9, 1000, 50, 0
    FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
    FlashForMs Light10, 1000, 50, 0
    FlashForMs ap1, 1000, 50, 0
    FlashForMs ap2, 1000, 50, 0
    TargetBonus = TargetBonus + 1
    RightOutlane.State = 1
    CheckMultiplier
    LastSwitchHit = "RightOutlaneTrigger"
End Sub

Sub CheckMultiplier
    If(LeftOutlane.State = 1)And(LeftInlane1.State = 1)And(LeftInlane2.State = 1)And(RightInlane.State = 1)And(RightOutlane.State = 1)Then
        AddScore 50000
        PlayFanfare
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
        Addcoin 50
        PlaySound "fx_coins"
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
        FlashForMs Light9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
        FlashForMs Light10, 1000, 50, 0
        FlashForMs ap1, 1000, 50, 0
        FlashForMs ap2, 1000, 50, 0
        CoinFrenzyTimer_Timer
    End If
    If bCatchemMode Then
        If CatchLight2.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
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
    RandomSoundBumperTop Bumper1
    FlashForMs bumpersmalllight1, 1000, 50, 0
    DOF 110, DOFPulse
    DOF 138, DOFPulse
    FlashForMs BLight001, 1000, 50, 0
    AddScore 500 + 4500 * ABS(Flasher1.Visible) 'a bumper scores 500 points and 5000 points when lit
    bumperHits = bumperHits - 1
    DMDFlush
    DMD RL(0, FormatScore(Score(Currentplayer))), RL(1, bumperHits& " BUMPER HITS LEFT"), "bumper", eNone, eNone, eNone, 300, True, ""
    CheckBumpers
    Sawtimer.enabled = True
    Ringspin.enabled = True
    
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    RandomSoundBumperMiddle Bumper2
    FlashForMs bumpersmalllight2, 1000, 50, 0
    DOF 108, DOFPulse
    DOF 140, DOFPulse
    FlashForMs BLight002, 1000, 50, 0
    AddScore 500 + 4500 * ABS(Flasher2.Visible) 'a bumper scores 500 points and 5000 points when lit
    bumperHits = bumperHits - 1
    DMDFlush
    DMD RL(0, FormatScore(Score(Currentplayer))), RL(1, bumperHits& " BUMPER HITS LEFT"), "bumper", eNone, eNone, eNone, 300, True, ""
    CheckBumpers
    Sawtimer.enabled = True
    Ringspin.enabled = True
    
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub
    RandomSoundBumperBottom Bumper3
    FlashForMs bumpersmalllight3, 1000, 50, 0
    DOF 109, DOFPulse
    DOF 137, DOFPulse
    FlashForMs BLight003, 1000, 50, 0
    AddScore 500 + 4500 * ABS(Flasher3.Visible) 'a bumper scores 500 points and 5000 points when lit
    bumperHits = bumperHits - 1
    DMDFlush
    DMD RL(0, FormatScore(Score(Currentplayer))), RL(1, bumperHits& " BUMPER HITS LEFT"), "bumper", eNone, eNone, eNone, 300, True, ""
    CheckBumpers
    Sawtimer.enabled = True
    Ringspin.enabled = True
    if NINJA2timer.enabled = false Then
       NINJA2timer.enabled = True
       NINJA2timerstop.enabled = True
	End If
End Sub

Sub Bumper4_Hit
    If Tilted Then Exit Sub
    RandomSoundBumperTop Bumper4
    FlashForMs bumpersmalllight4, 1000, 50, 0
    DOF 107, DOFPulse
    DOF 139, DOFPulse
    FlashForMs BLight004, 1000, 50, 0
    AddScore 500 + 4500 * ABS(Flasher4.Visible) 'a bumper scores 500 points and 5000 points when lit
    bumperHits = bumperHits - 1
    DMDFlush
    DMD RL(0, FormatScore(Score(Currentplayer))), RL(1, bumperHits& " BUMPER HITS LEFT"), "bumper", eNone, eNone, eNone, 300, True, ""
    CheckBumpers
    Sawtimer.enabled = True
    Ringspin.enabled = True
    if NINJAtimer.enabled = false Then
       NINJAtimer.enabled = True
       NINJAtimerstop.enabled = True
	End If
End Sub

' Bumper Bonus
' 100000 i bonus after each 100 hits

Sub CheckBumpers()
    If bumperHits <= 0 Then
        BumperBonus = BumperBonus + 1
        DMD "_", RL(1, "" & BumperBonus), "BUMP", eNone, eBlink, eNone, 500, True, ""
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

FlInitBumper 1, "yellow"
FlInitBumper 2, "red"
FlInitBumper 3, "red"
FlInitBumper 4, "yellow"

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
	select case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0) : FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0) : FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255) : FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255) : FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8) : FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32) : FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255) : MaterialColor "bumpertopmat" & nr, RGB(16,255,16) 
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "orange"
			FlBumperHighlight(nr).color = RGB(205,170,109)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(205,170,109) : FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(194,178,128) : FlBumperBigLight(nr).colorfull = RGB(205,170,109)
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190) : FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255) : FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4) : FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50) : FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255) : FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255) : FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : 
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	end select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
'	UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
'               OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
'               float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 )* DayNightAdjust	

	select case FlBumperColor(nr)

		Case "blue" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(38-24*Z,130 - 98*Z,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20  + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 45 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z^3) / (0.5 + DNA90)

		Case "green"	
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(16 + 16 * sin(Z*3.14),255,16 + 16 * sin(Z*3.14)), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z^3) / (1 + DNA90)
		
		Case "red" 
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 16 - 11*Z + 16 * sin(Z*3.14),0), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z*4,8-Z*8)     
		
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(205, 170 - 22*z  + 16 * sin(Z*3.14),Z*32), RGB(205,205,205), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,136, 50)

		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 14 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20*Z,255-65*Z) : FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20*Z,255-65*Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z*36,220 - Z*90)

		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 1, RGB(30-27*Z^0.03,30-28*Z^0.01, 255), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z^3
			Flbumperbiglight(nr).intensity = 40 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255-240*(Z^0.1),255 - 240*(Z^0.1),255) : FlBumperSmallLight(nr).colorfull = RGB(255-200*z,255 - 200*Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255-190*Z,235 - z*180,220 + 35*Z)

		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 180 + 40*z, 48* Z), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)

		Case "purple" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(128-118*Z - 32 * sin(Z*3.14), 32-26*Z ,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15  + 200 * Z / (0.5 + DNA30) 
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 50 * Z / (1 + DNA45) 
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128-60*Z,32,255)


	end select
End Sub

Sub BumperTimer_Timer
	dim nr
	For nr = 1 to 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	next
End Sub



'**************************
' Upper Lanes & loop switch
'**************************

Dim PokemonLevel, HolePos
Sub RightLoopTrigger_Hit() 'set the pokemon levels
    DOF 131, DOFPulse
    If Tilted Then Exit Sub
    TargetBonus = TargetBonus + 1
    If LastSwitchHit = "LeftInlaneTrigger2" Then 'combo
        DMD "_", CL(1, " " & FormatScore("20000")), "COMBO2", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 20000
    End If
    If bCatchemMode Then
        If CatchLight5.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
            CheckCatchHits
        End If
    Else
        If PokemonLevel < 4 Then
            PokemonLevel = PokemonLevel + 1
            UpdatePokemonLevel
        End If
        AddScore 2500 * PokemonLevel
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightLoopTrigger"
    if bLoopBonus Then
    StartGun
    If NOT bMultiBallMode Then PlaySound"quoteC"&RndNbr(12)
    if PUNISHERtimer.enabled = false Then
   PUNISHERtimer.enabled = True
   PUNISHERtimerstop.enabled = True
	End If
    End If
End Sub

Sub LeftLoopTrigger_Hit()
    DOF 130, DOFPulse
    If Tilted Then Exit Sub
    TargetBonus = TargetBonus + 1
    IncrementHoleLights
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        DMD "_", CL(1, " " & FormatScore("30000")), "COMBO1", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 30000
    End If
    If bcoinfrenzy AND CoinLight1.State = 2 Then
        Addcoin 50
        PlaySound "fx_coins"
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
        FlashForMs Light9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
        FlashForMs Light10, 1000, 50, 0
        FlashForMs ap1, 1000, 50, 0
        FlashForMs ap2, 1000, 50, 0
        CoinFrenzyTimer_Timer
    End If
    If bCatchemMode Then
        If CatchLight1.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
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
        Case 0:LightLevel1.State = 0:LightLevel2.State = 0:LightLevel3.State = 0:LightLevel4.State = 0:Flasher1.Visible = 0:Flasher2.Visible = 0:Flasher3.Visible = 0:Flasher4.Visible = 0:LightArrow1.State = 2:LightArrow6.State = 2
        Case 1:LightLevel1.State = 1:LightLevel2.State = 0:LightLevel3.State = 0:LightLevel4.State = 0:Flasher1.Visible = 1:Flasher2.Visible = 0:Flasher3.Visible = 0:Flasher4.Visible = 0:LightArrow5.State = 2
        Case 2:LightLevel1.State = 1:LightLevel2.State = 1:LightLevel3.State = 0:LightLevel4.State = 0:Flasher1.Visible = 1:Flasher2.Visible = 1:Flasher3.Visible = 0:Flasher4.Visible = 0:HoleLight5.State = 2:l53.State = 0
        Case 3:LightLevel1.State = 1:LightLevel2.State = 1:LightLevel3.State = 1:LightLevel4.State = 0:Flasher1.Visible = 1:Flasher2.Visible = 1:Flasher3.Visible = 1:Flasher4.Visible = 0
        Case 4:LightLevel1.State = 1:LightLevel2.State = 1:LightLevel3.State = 1:LightLevel4.State = 1:Flasher1.Visible = 1:Flasher2.Visible = 1:Flasher3.Visible = 1:Flasher4.Visible = 1:LightArrow1.State = 0:LightArrow6.State = 0
    End Select
End Sub

Sub ResetPokemonLevel()
    PokemonLevel = 0
    UpdatePokemonLevel
End Sub

Sub IncrementHoleLights()
    If HolePos <> 5 Then
        HolePos = HolePos + 1
        If HolePos = 5 Then
            HolePos = 1
        End If
    End If
    UpdateHoleLights
End Sub

Sub UpdateHoleLights()
    Select Case HolePos
        Case 0:HoleLight1.State = 0:HoleLight2.State = 0:HoleLight3.State = 0:HoleLight4.State = 0
        Case 1:HoleLight1.State = 2:HoleLight2.State = 0:HoleLight3.State = 0:HoleLight4.State = 0
        Case 2:HoleLight1.State = 0:HoleLight2.State = 2:HoleLight3.State = 0:HoleLight4.State = 0
        Case 3:HoleLight1.State = 0:HoleLight2.State = 0:HoleLight3.State = 2:HoleLight4.State = 0
        Case 4:HoleLight1.State = 0:HoleLight2.State = 0:HoleLight3.State = 0:HoleLight4.State = 2
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
    STHit 5
	'PlaySoundAt SoundFXDOF("fx_target", 115, DOFPulse, DOFTargets), PikaTarget1
    Playsound "dkick"
    If Tilted Then Exit Sub
    PlaySoundEffect
    PikachuShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher12, 1000, 50, 0:FlashForMs Flasher12a, 1000, 50, 0
        FlashForMs Light12, 1000, 50, 0
        FlashForMs ap1, 1000, 50, 0
        FlashForMs ap2, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight1.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget1"
     if DAREtimer.enabled = false Then
   DAREtimer.enabled = True
   DAREtimerstop.enabled = True
	End If
End Sub

Sub PikaTarget1o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub PikaTarget2_Hit()
	STHit 6
    'PlaySoundAt SoundFXDOF("fx_target", 115, DOFPulse, DOFTargets), PikaTarget2
    Playsound "dkick"
     If NOT bMultiBallMode Then PlaySound"quoteM"&RndNbr(10)
    If Tilted Then Exit Sub
    PlaySoundEffect
    PikachuShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher11, 1000, 50, 0:FlashForMs Flasher11a, 1000, 50, 0
        FlashForMs Light11, 1000, 50, 0
        FlashForMs ap1, 1000, 50, 0
        FlashForMs ap2, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight2.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget2"
     if DAREtimer.enabled = false Then
   DAREtimer.enabled = True
   DAREtimerstop.enabled = True
	End If
End Sub

Sub PikaTarget2o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub PikaTarget3_Hit()
	STHit 7
    'PlaySoundAt SoundFXDOF("fx_target", 115, DOFPulse, DOFTargets), PikaTarget3
    Playsound "dkick"
     If NOT bMultiBallMode Then PlaySound"quoteM"&RndNbr(10)
    If Tilted Then Exit Sub
    PlaySoundEffect
    PikachuShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher11, 1000, 50, 0:FlashForMs Flasher11a, 1000, 50, 0
        FlashForMs Light11, 1000, 50, 0
        FlashForMs ap1, 1000, 50, 0
        FlashForMs ap2, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight3.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget3"
     if DAREtimer.enabled = false Then
   DAREtimer.enabled = True
   DAREtimerstop.enabled = True
	End If
End Sub

Sub PikaTarget3o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub PikaTarget4_Hit()
	STHit 8
    'PlaySoundAt SoundFXDOF("fx_target", 115, DOFPulse, DOFTargets), PikaTarget4
    Playsound "dkick"
    If Tilted Then Exit Sub
    PlaySoundEffect
    PikachuShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher11, 1000, 50, 0:FlashForMs Flasher11a, 1000, 50, 0
        FlashForMs Light11, 1000, 50, 0
        FlashForMs ap1, 1000, 50, 0
        FlashForMs ap2, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight4.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget4"
     if DAREtimer.enabled = false Then
   DAREtimer.enabled = True
   DAREtimerstop.enabled = True
	End If
End Sub

Sub PikaTarget4o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub PikaTarget5_Hit()
	STHit 9
    'PlaySoundAt SoundFXDOF("fx_target", 116, DOFPulse, DOFTargets), PikaTarget5
    Playsound "JHIT"
    If Tilted Then Exit Sub
    PlaySoundEffect
    If LastSwitchHit = "LeftInlaneTrigger2" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher12, 1000, 50, 0:FlashForMs Flasher12a, 1000, 50, 0
        FlashForMs Light12, 1000, 50, 0
        FlashForMs ap1, 1000, 50, 0
        FlashForMs ap2, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    CharmanderShake
    TargetBonus = TargetBonus + 1
    PikaLight5.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget5"
     if jessicatimer.enabled = false Then
        jessicatimer.enabled = True
        jessicatimerstop.enabled = True
	End If
End Sub

Sub PikaTarget5o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub PikaTarget6_Hit()
	STHit 10
    'PlaySoundAt SoundFXDOF("fx_target", 116, DOFPulse, DOFTargets), PikaTarget6
    Playsound "JHIT"
    If NOT bMultiBallMode Then PlaySound"quoteJ"&RndNbr(7)
    If Tilted Then Exit Sub
    PlaySoundEffect
    If LastSwitchHit = "LeftInlaneTrigger2" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    CharmanderShake
    If bPikachuTargetMode Then
       Addscore PikachuTargetValue * 3
       FlashForMs Flasher12, 1000, 50, 0:FlashForMs Flasher12a, 1000, 50, 0
       FlashForMs Light12, 1000, 50, 0
       FlashForMs ap1, 1000, 50, 0
       FlashForMs ap2, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight6.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget6"
     if jessicatimer.enabled = false Then
        jessicatimer.enabled = True
        jessicatimerstop.enabled = True
	End If
End Sub

Sub PikaTarget6o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub PikaTarget7_Hit()
	STHit 11
    'PlaySoundAt SoundFXDOF("fx_target", 116, DOFPulse, DOFTargets), PikaTarget7
    Playsound "JHIT"
    If Tilted Then Exit Sub
    PlaySoundEffect
    If LastSwitchHit = "LeftInlaneTrigger2" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("10000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 10000
    End If
    CharmanderShake
    If bPikachuTargetMode Then
        Addscore PikachuTargetValue * 3
        FlashForMs Flasher12, 1000, 50, 0:FlashForMs Flasher12a, 1000, 50, 0
        FlashForMs Light12, 1000, 50, 0
        FlashForMs ap1, 1000, 50, 0
        FlashForMs ap2, 1000, 50, 0
    Else
        Addscore PikachuTargetValue
    End If
    TargetBonus = TargetBonus + 1
    PikaLight7.State = 1
    CheckTargetLights
    PikachuHits = PikachuHits + 1
    ' remember last trigger hit by the ball
    LastSwitchHit = "PikaTarget7"
     if jessicatimer.enabled = false Then
        jessicatimer.enabled = True
        jessicatimerstop.enabled = True
	End If
End Sub

Sub PikaTarget7o_Hit
	TargetBouncer ActiveBall, 1
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

Sub LightSeqPikachu_PlayDone()
    LightSeqPikachu.Play SeqRandom, 7, , 3000
End Sub

Sub StartTargetMode()
     DMD "  ", "    TARGET MODE ", "MURD", eBlink, eBlink, eNone, 1500, True, "QuoteM6"
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
    If BallUpdateLight.State = 2 AND BallType < 4 Then 'coins are over 250 and ball can be updated
        PokemonCenter.Enabled = True
    Else
        PokemonCenter.Enabled = False
    End If
End Sub

Sub PokemonCenter_Hit()
    PokemonCenter.Enabled = False
    IncrementBallType
    vpmtimer.addtimer 1000, "PokemonCenter.kick 270,4 '"
End Sub

Sub IncrementBallType()
    If Coins(CurrentPlayer) > 249 AND BallType < 4 Then
        PlayFanfare
        AddCoin -250
        BallType = BallType + 1
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
    FlashForMs Light9, 1000, 50, 0
    FlashForMs ap1, 1000, 50, 0
        FlashForMs ap2, 1000, 50, 0
    StartSmoke
    TimerJAZ.Enabled = True
    
        Select case BallType
            Case 1:DMD "", "         JESSICA JONES", "pokeball", eNone, eBlink, eNone, 1500, True, ""
            Case 2:DMD "", "         LUKE CAGE", "greatball", eNone, eBlink, eNone, 1500, True, ""
            Case 3:DMD "", "        IRON FIST", "ultraball", eNone, eBlink, eNone, 1500, True, ""
            Case 4:DMD "", "        DAREDEVIL", "masterball", eNone, eBlink, eNone, 1500, True, ""
        End Select
        UpdateBallType
    End If
End Sub

Sub ReduceBallType()
    If BallType > 1 Then
        BallType = BallType - 1
        UpdateBallType
    End If
End Sub

Sub UpdateBallType()
    Select Case BallType
        Case 1:BallLight1.State = 1:BallLight2.State = 0:BallLight3.State = 0:BallLight4.State = 0
        Case 2:BallLight1.State = 0:BallLight2.State = 1:BallLight3.State = 0:BallLight4.State = 0
        Case 3:BallLight1.State = 0:BallLight2.State = 0:BallLight3.State = 1:BallLight4.State = 0
        Case 4:BallLight1.State = 0:BallLight2.State = 0:BallLight3.State = 0:BallLight4.State = 1
    End Select
    UpdateBallImage
End Sub

Sub UpdateBallImage()
    Dim BOT, b
    BOT = GetBalls
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    ' change the image for each ball
    For b = 0 to UBound(BOT)
        Select Case BallType
            Case 1:BOT(b).FrontDecal = "ball1"
            Case 2:BOT(b).FrontDecal = "ball2"
            Case 3:BOT(b).FrontDecal = "ball3"
            Case 4:BOT(b).FrontDecal = "ball4"
        End Select
    Next
End Sub

' Coin Frenzy
Dim bcoinfrenzy, coinstep

Sub CheckCoinFrenzy()
    If bcoinfrenzy And sMultiModePhase = "" Then
        If coinstep > 9 Then
            StartCoinFrenzy
            
        End If
    End If
End Sub

Sub StartCoinFrenzy()
    DMD "", CL(1, "MYSTERIO MYSTERY"), "MYSTERIO", eNone, eBlink, eNone, 1000, True, ""  
    AddMultiball 1
    sMultiModePhase = "ModeB"
    ChangeSong
    Changegi blue
    Playsound "Mysteriofight"
    TimerJAM.Enabled = True
    StartIron
    MYSTSee
    vpmtimer.Addtimer 2000, "MAYHEMLights '"
    EnableBallSaver 20
    bcoinfrenzy = True
    CoinFrenzyTimer.Enabled = True
    'StopCoinFrenzyTimer.Enabled = True
    CoinFrenzyTimer_Timer
    FAKEMAN1.transz = -100
	FAKEMAN1Up.enabled = True
	FAKEMAN2.transz = -100
	FAKEMAN2Up.enabled = True
    FAKEMAN3.transz = -100
	FAKEMAN3Up.enabled = True
    FAKEMAN4.transz = -100
	FAKEMAN4Up.enabled = True
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
    MAYHEMLightsreset
    MYSTUnsee
    Playsound "Mysterioend"
    Dim i
    CoinFrenzyTimer.Enabled = False
    StopCoinFrenzyTimer.Enabled = False
    For each i in aCoinFrenzyLights
        i.State = 0
    Next
    bcoinfrenzy = False
    coinstep = 0
    FAKEMAN1Down.Enabled = True
	FAKEMAN2Down.Enabled = True
    FAKEMAN3Down.Enabled = True
    FAKEMAN4Down.Enabled = True
End Sub

'***********************************
'Venusaur Cave: Lock (software lock)
'***********************************

Dim bLockEnabled, LockedBalls

Sub VenusaurHole_Hit()
    FlashForMs ap1, 1000, 50, 0
    FlashForMs ap2, 1000, 50, 0
    FlashForMs flash3, 1000, 50, 0
    If NOT bMultiBallMode Then PlaySound"quoteB"&RndNbr(26)
    SoundSaucerLock
    If Not Tilted Then
		If bcoinfrenzy AND CoinLight4.State = 2 Then
            Addcoin 50
            PlaySound "fx_coins"
            FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
            FlashForMs Light9, 1000, 50, 0
            FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
            FlashForMs Light10, 1000, 50, 0
            CoinFrenzyTimer_Timer
		End If
		If bCatchemMode Then
            If CatchLight3.State = 2 Then
                Addscore 10000 * PokemonLevel
                CatchHits = CatchHits + 1
                CheckCatchHits
            End If
        Else
            Addscore 10000
        End If

        If bLockEnabled And bcoinfrenzy = False And sMultiModePhase = "" Then
            LockedBalls = LockedBalls + 1
        DMD "_", CL(1, "BALL " & LockedBalls & " IS LOCKED"), "LOCK", eNone, eNone, eNone, 2000, True, ""
        StartCharge
        FlashCANE 800, 100
        TimerJAG.Enabled = True
			If LockedBalls = 2 Then
                MultiLight1.State = 2
                MultiLight2.State = 2
                MultiLight3.State = 2
                MultiLight4.State = 2
                MultiLight5.State = 2
			End If

			If LockedBalls = 3 Then
                VenusaurMultiball
                StartLaser
                
            End IF
        End If
    End If
    vpmtimer.addtimer 2000, "VenusaurKickBall '"
    ' remember last trigger hit by the ball
    LastSwitchHit = "VenusaurHole"
End Sub

Sub VenusaurKickBall()
    SoundSaucerKick 1, VenusaurHole
    DOF 121, DOFPulse
    DOF 114, DOFPulse
    AnimateVenusaur
    VenusaurHole.Kick 200, 8
End Sub

Sub VenusaurLock1_Hit()
	STHit 12
	if bcoinfrenzy Then Exit Sub
    'PlaySoundAt SoundFXDOF("fx_target", 128, DOFPulse, DOFTargets), VenusaurLock1
    If Tilted Then Exit Sub
    PlaySoundEffect
    Addscore 20000
    VenusaurShake
    LockLight1.State = 1
    CheckLock
End Sub

Sub VenusaurLock1o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub VenusaurLock2_Hit()
	STHit 13
	if bcoinfrenzy Then Exit Sub
    'PlaySoundAt SoundFXDOF("fx_target", 128, DOFPulse, DOFTargets), VenusaurLock2
    If Tilted Then Exit Sub
    PlaySoundEffect
    Addscore 20000
    VenusaurShake
    LockLight2.State = 1
    CheckLock
End Sub

Sub VenusaurLock2o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub CheckLock()
    If bLockEnabled Then Exit Sub
    If LockLight1.State + LockLight2.State = 2 Then
        bLockEnabled = True
        LockLight.State = 2
        DMD "", CL(1, "LOCK IS LIT"), "LOCK2", eNone, eBlink, eNone, 2000, True, ""
    End If
End Sub

Sub VenusaurMultiball()
if bcoinfrenzy = False And sMultiModePhase = "" Then
    DMD "", CL(1, "KINGPIN MULTIBALL"), "KPM", eNone, eBlink, eNone, 3000, True, ""   
    PlayFanfare
    KINGSee 
    TimerJAG.Enabled = True
    AddMultiball 2
    sMultiModePhase = "ModeA"
    ChangeSong
    EnableBallSaver 20
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
    SetLightColor LightArrow4, red, 2
    SetLightColor LightArrow2, red, 2
End If
End Sub

Sub ResetJackpotLights()
    CenterRampLight.State = 0
    RightRampLight.State = 0
    SetLightColor LightArrow4, white, 0
    SetLightColor LightArrow2, white, 0
    LightArrow4.State = 0
    LightArrow2.State = 0
End Sub

Dim VenusaurPos

Sub AnimateVenusaur()
    VenusaurPos = 0
    VenusaurTimer.Enabled = 1
End Sub

Sub VenusaurTimer_Timer()
    Select Case VenusaurPos
        Case 0:KINGPIN2.TransY = 40:VenusaurPos = 1:KINGPIN.TransY = 40:VenusaurPos = 1:KINGPIN3.TransY = 40:VenusaurPos = 1
        Case 1:KINGPIN2.TransY = 20:VenusaurPos = 2:KINGPIN.TransY = 20:VenusaurPos = 2:KINGPIN3.TransY = 20:VenusaurPos = 2
        Case 2:KINGPIN2.TransY = 10:VenusaurPos = 3:KINGPIN.TransY = 10:VenusaurPos = 3:KINGPIN3.TransY = 10:VenusaurPos = 3
        Case 3:KINGPIN2.TransY = 5:VenusaurPos = 4:KINGPIN.TransY = 5:VenusaurPos = 4:KINGPIN3.TransY = 5:VenusaurPos = 4
        Case 4:KINGPIN2.TransY = 0:VenusaurTimer.Enabled = 0:KINGPIN.TransY = 0:VenusaurTimer.Enabled = 0:KINGPIN3.TransY = 0:VenusaurTimer.Enabled = 0
    End Select
End Sub

'***************
' Ramp Switches
'***************

Sub RightRampDone_Hit
    FlashForMs ap1, 1000, 50, 0
    FlashForMs ap2, 1000, 50, 0
    If Tilted Then Exit Sub
    If LastSwitchHit = "LeftInlaneTrigger1" Then 'combo
        DMD "_", CL(1, " " & FormatScore("20000")), "COMBO3", eNone, eBlinkFast, eNone, 500, True, ""
        AddScore 20000
    End If
    If bcoinfrenzy AND CoinLight5.State = 2 Then
        Addcoin 50
        PlaySound "fx_coins"
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
        FlashForMs Light9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
        FlashForMs Light10, 1000, 50, 0
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
        if bcoinfrenzy then FAKEMAN1Up.enabled = True:FAKEMAN2Up.enabled = True:FAKEMAN3Up.enabled = True:FAKEMAN4Up.enabled = True
        End If
    End If
   If PikaMultiballLight.State = 2 Then
if bcoinfrenzy = False And sMultiModePhase = "" Then
        DMD "MURDOCK", "MULTIBALL", "DEVIL", eNone, eBlink, eNone, 2000, True, "QuoteM1"
        StartSonar
        vpmtimer.Addtimer 500, "MurdockLights '"
        AddMultiball 2
        RAINSee
        sMultiModePhase = "ModeC"
        ChangeSong
        EnableBallSaver 20
        PikaMultiballLight.State = 0
    End If
End If
    If bCatchemMode Then
        If CatchLight4.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
            CheckCatchHits
        End If
    Else
        Addscore 10000
    End If

If bRampBonus Then
    If NOT bMultiBallMode Then PlaySound"quotePM"&RndNbr(9)
       Playsound "purpleman"
    if PURPLEtimer.enabled = false Then
       PURPLEtimer.enabled = True
       PURPLEtimerstop.enabled = True
	End If
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightRampDone"
   
End Sub

Sub CenterRampDone_Hit
    If NOT bMultiBallMode Then PlaySound"quoteA"&RndNbr(9)
    If Tilted Then Exit Sub
    If ExtraBallLight.State = 2 Then
    if SPIDtimer.enabled = false Then
      SPIDtimer.enabled = True
     SPIDtimerstop.enabled = True
	End If
        AwardExtraBall
        Playsound "ExtraBall"
        Playsound "EBAWARD"
        ExtraBallLight.State = 0
    End If
    If bcoinfrenzy AND CoinLight3.State = 2 Then
        Addcoin 50
        PlaySound "fx_coins"
        CoinFrenzyTimer_Timer
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
        FlashForMs Light9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
        FlashForMs Light10, 1000, 50, 0
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
            StartSonar
        Else
            AwardJackpot
            if bcoinfrenzy then FAKEMAN1Up.enabled = True:FAKEMAN2Up.enabled = True:FAKEMAN3Up.enabled = True:FAKEMAN4Up.enabled = True
            StartSonar
        End If
    End If
    If bCatchemMode Then
        If CatchLight6.State = 2 Then
            Addscore 10000 * PokemonLevel
            CatchHits = CatchHits + 1
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
    
    EnableBallSaver 15
    DMD CL(0, "PURPLE MAN 30SEC"), CL(1, "RAMP BONUS MODE"), "RAMPB", eNone, eBlink, eNone, 2000, True, "QuotePM4"
    bRampBonus = True
	bReverseFlippers = True
    SetLightColor LightArrow4, red, 2
    SetLightColor LightArrow2, red, 2
    StopRampBonusTimer.Enabled = True
End Sub

Sub StopRampBonusTimer_Timer()
    bRampBonus = FALSE
	bReverseFlippers = False 
    SetLightColor LightArrow4, white, 0
    SetLightColor LightArrow2, white, 0
    StopRampBonusTimer.Enabled = False
End Sub

'*****************
' Loop Bonus Mode
'*****************
' only availabe trough the slot machine

Dim bLoopBonus

Sub StartLoopBonus()
    DMD CL(0, "   PUNISHER 30SEC"), CL(1, "   LOOP BONUS MODE"), "LOOP", eNone, eBlink, eNone, 2000, True, ""
    bLoopBonus = True
    SetLightColor LightArrow1, red, 2
    SetLightColor LightArrow6, red, 2
    StopLoopBonusTimer.Enabled = True
End Sub

Sub StopLoopBonusTimer_Timer()
    bLoopBonus = FALSE
    SetLightColor LightArrow1, white, 0
    SetLightColor LightArrow6, white, 0
    StopLoopBonusTimer.Enabled = False
End Sub

Sub LoopTrigger_Hit()
    If bLoopBonus Then
        AwardJackpot
        if bcoinfrenzy then FAKEMAN1Up.enabled = True:FAKEMAN2Up.enabled = True:FAKEMAN3Up.enabled = True:FAKEMAN4Up.enabled = True
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
        FlashForMs Light9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
        FlashForMs Light10, 1000, 50, 0
        GiEffect 1
    End If
    LastSwitchHit = "LoopTrigger"
End Sub

'*******************
'Pokemon definitions
'*******************

Dim Eggs, Eggname, Pokemon, Pokename

Eggs = Array("pok152", "pok153", "pok154", "pok155", "pok156", "pok157", "pok158", "pok159", "pok160", "pok161", "pok162", "pok163", "pok164", "pok165", _
    "pok166", "pok167", "pok168", "pok169", "pok170", "pok171", "pok172", "pok173", "pok174", "pok175", "pok176", "pok177", "pok178") '26 BASES level 1

Eggname = Array("15TH PRECINCT", "CLINTON MISSION", "LAW OFFICE", "FOGWELL GYM", "JAVITS CENTER", "JOSIES BAR", "PARADISO CLUB", "FISK TOWER", "CRIME CENTRAL", "BAXTER BLDG", _
 "HUDSON DOCKS", "ROYAL DRAGON", "NY SANCTUM", "NY BULLETIN", "CENTRAL PARK", "ALL SAINTS", "RAIL YARD", "TRAVEL INN", "NELSON MEATS", "OSCORP", "CITY HALL", "PIER 81", _
 "RYKERS ISLAND", "SQUARE DINER", "VANLUNT BLDG", "WHITESTONE", "CEMETARY")

Pokemon = Array("pok001", "pok002", "pok003", "pok004", "pok005", "pok006", "pok007", "pok008", "pok009", "pok010", _
    "pok011", "pok012", "pok013", "pok014", "pok015", "pok016", "pok017", "pok018", "pok019", "pok020", _
    "pok021", "pok022", "pok023", "pok024", "pok025", "pok026", "pok027", "pok028", "pok029", "pok030", _
    "pok031", "pok032", "pok033", "pok034", "pok035", "pok036", "pok037", "pok038", "pok039", "pok040", _
    "pok041", "pok042", "pok043", "pok044", "pok045", "pok046", "pok047", "pok048", "pok049", "pok050", _
    "pok051", "pok052", "pok053", "pok054", "pok055", "pok056", "pok057", "pok058", "pok055", "pok060", _
    "pok061", "pok062", "pok063", "pok064", "pok065", "pok066", "pok067", "pok068", "pok069", "pok070", _
    "pok071", "pok072", "pok073", "pok074", "pok075", "pok076", "pok077", "pok078", "pok079", "pok080", _
    "pok081", "pok082", "pok083", "pok084", "pok085", "pok086", "pok087", "pok088", "pok089", "pok090", _
    "pok091", "pok092", "pok093", "pok094", "pok095", "pok096", "pok097", "pok098", "pok099", "pok100", _
    "pok101", "pok102", "pok103", "pok104", "pok105", "pok106", "pok107", "pok108", "pok109", "pok110", _
    "pok111", "pok112", "pok113", "pok114", "pok115", "pok116", "pok117", "pok118", "pok119", "pok120", _
    "pok121", "pok122", "pok123", "pok124", "pok125", "pok126", "pok127", "pok128", "pok129", "pok130", _
    "pok131", "pok132", "pok133", "pok134", "pok135", "pok136", "pok137", "pok138", "pok139", "pok140", _
    "pok141", "pok142", "pok143", "pok144", "pok145", "pok146", "pok147", "pok148", "pok149", "pok150", "pok151", "pok151") ' all the 151 VILLAINS, STAN LEE, can only be caught from the slotmachine                                                                                     '151

Pokename = Array("ECHO", "JESTER", "KINGPIN", "THE OWL", "TYPHOID MARY", "ELEKTRA", "BULLSEYE", "PURPLE MAN", "MR. FEAR", "GLADIATOR", "TOMBSTONE", _
    "DEATH STALKER", "STILT MAN", "MAN BULL", "ALEX BONT", "AMMO", "BENGAL", "BLINDSIDE", "NUKE", "EEL", "LADY BULLSEYE", "BLACK TARANTULA", _
    "BRUISER", "BUCK CASHMAN", "EVIL DAREDEVIL", "MYSTERIO", "MOLE MAN", "CALAVERA", "THE ROSE", "THE SPOT", "COYOTE", "IKARI", "JACKHAMMER", _
    "JUBULA PRIDE", "KRUEL", "MADCAP", "MASTER IZO", "MAULER", "MIND MASTER", "MACHINESMITH", "SNAKEROOT", "RAMROD", "R. CHERRYH", "RAZOR FIST", _
    "SCREWBALL", "SIN EATER", "SLUG", "TRIBUNE", "MR. NEGATIVE", "ENFORCERS", "KIRIGI", "ABE JENKINS", "ABSORBING MAN", "AGENT CROCK", "AKA", _
    "AKHENATEN", "RED AKUMA", "SILVER SAMURAI", "THE HAND", "CROSSBONES", "APE MAN", "BIRD MAN", "FROG MAN", "CAT MAN", "ARCADE", "WRECKER", _
    "THUNDERBALL", "PILEDRIVER", "BULLDOZER", "KLAW", "MEPHISTO", "ELECTRO", "SHOCKER", "BLACK KNIGHT", "ABOMINATION", "LOKI", _
    "DORMAMMU", "DARK BEAST", "CHEMISTRO", "CHESHIRE", "CHONDU", "COCKROACH", "COMANCHE", "COPPERHEAD", "CRANE MOTHER", "FEROCIA", "KHAN", _
    "SCIMITAR", "SHUO LAO", "STEEL SERPENT", "YU-TI", "DOC OCK", "GORGON", "HAYWIRE", "HOBGOBLIN", "JIGSAW", "KNULL", "LONE SHARK", "MR. BEYOND", _
    "GOLD GOBLIN", "RHINO", "SCARLETT WITCH", "SKADI", "STAR", "TOXIC DOXIE", "TRICKSHOT", "VERANKE", "VIKKI HAND", "SCORPION", _
    "SKRULLS", "GREEN GOBLIN", "J. BEEKMAN", "BLACK CAT", "DEADPOOL", "BLACK MAMBA", "EGGHEAD", "GOLIATH", "VULTURE", "CARNAGE", "VENOM", "WHIRLWIND", _
    "ZZZAX", "DR. DOOM", "SABERTOOTH", "YANDROTH", "ATTUMA", "BATROC", "MANDRILL", "NAMOR", "MELTER", "NEBULON", "AMORA", "ORKA", _
    "RED GHOST", "SAND MAN", "BLACKHEART", "KRAVEN", "MADAME GAO", "NOBU", "PALADIN", "TASKMASTER", "COLLECTOR", "WHITE TIGER", "THANOS", _
    "TIGER SHARK", "TOAD", "PORCUPINE", "BLACKWING", "SHE-HULK", "LIZARD", "STAN LEE", "STAN LEE")

'***************************
' Catch Hole /Catch'em Mode
'***************************

' Start always first the Cath'em mode if the light is blinking other wise gives a random award if one of the other lights are blinking.
' Catch'em mode ends after 2 minutes

Dim bCatchemMode, CatchID, CatchMaxHits, CatchHits, BallInHole

Dim aBall

Sub CatchHole_Timer
    Do While aBall.Z > 0
        aBall.Z = aBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
End Sub

Sub CatchHole_Hit
    PlaySoundAt "fx_hole_enter", CatchHole
    BallInHole = BallInHole + 1
    Set aBall = ActiveBall:Me.TimerEnabled = 1
    If HoleLight5.State = 2 Then 'Start catchem mode
        StartCatchem1
        vpmtimer.addtimer 2500, "CatchHoleExit '"
        Exit Sub
    End If
    If HolePos > 0 Then
        StartSlotmachine
    Else
        vpmtimer.addtimer 1000, "CatchHoleExit '"
    End If
    ' remember last trigger hit by the ball
    LastSwitchHit = "CatchHole"
    
End Sub

Sub CatchHoleExit()
    If BallInHole > 0 Then
        FlashForMs FlasherExitHole, 1000, 30, 0
        BallInHole = BallInHole - 1
        CatchHole.CreateSizedball BallSize / 2
        UpdateBallImage
        PlaySoundAt SoundFXDOF("fx_popper", 120, DOFPulse, DOFContactors), CatchHole
        DOF 121, DOFPulse
        CatchHole.Kick 175, 14, 1
        if ELEKTRAtimer.enabled = false Then
       ELEKTRAtimer.enabled = True
       ELEKTRAtimerstop.enabled = True
	End If
    End If
End Sub

Sub StartCatchem1()
    bCatchemMode = True
    CatchID = 0
    ChangeSong                              ' change the song
    Select Case PokemonLevel                'limit the numbers of pokemons based on the level
        Case 2:CatchID = INT(RND * 50)      ' CatchID the number of the pokemon to be caught, this are level 2 Pokémon
        Case 3:CatchID = INT(RND * 50) + 49 'level 3 Pokémon
        Case 4:CatchID = INT(RND * 50) + 99 'level 4 Pokémon
    End Select
    'debug.print catchid
    CatchMaxHits = PokemonLevel
    StartCatchem2
End Sub

Sub StartCatchem2()
    Playsound "gbeam"
    FlashForMs gbeam2, 1000, 50, 0
    FlashForMs flash4, 1000, 50, 0
    FlashForMs ray, 1000, 50, 0
    DMD RL(0, "VILLAIN TAKEDOWN"), RL(1, pokename(CatchID)), pokemon(CatchID), eNone, eNone, eNone, 2000, True, ""
    CatchHits = 0
    CatchemTimer.Enabled = True ' blink one light
    StopCatchem.Enabled = True  ' turn on the timer to stop the mode
    CatchemTimer_Timer
    ' turn off lights
    HoleLight5.State = 0:l53.State = 2
    EnableBallSaver 10
End Sub

Sub CatchemTimer_Timer() 'blink one catch light for 5 seconds and then change to another light simulating the pokemon movement
    Dim i
    For each i in aCatchLights
        i.State = 0
    Next
    i = INT(RND * 6)
    aCatchLights(i).State = 2
End Sub

Sub StopCatchem_Timer() 'reset lights & variables
    Dim i
    CatchemTimer.Enabled = False
    StopCatchem.Enabled = False
    For each i in aCatchLights
        i.State = 0
    Next
    bCatchemMode = False
    ChangeSong
    ResetPokemonLevel
End Sub

Sub CheckCatchHits()
    GiEffect 1
    PlaySoundEffect
    If CatchHits >= CatchMaxHits Then 'stop catch'em mode
        PlayFanfare
        If CatchID = 150 then Playsound "Excelsior"
        StanHide
        DMD RL(0, "YOU CAUGHT "), RL(1, Pokename(CatchID)), Pokemon(CatchID), eNone, eBlinkFast, eNone, 1500, False, ""
        DMD RL(0, FormatScore(50000 * pokemonlevel)), RL(1, Pokename(CatchID)), Pokemon(CatchID), eBlinkFast, eNone, eNone, 1500, True, ""
        Addscore 50000 * pokemonlevel
        PokemonBonusAward(CurrentPlayer) = PokemonBonusAward(CurrentPlayer) + (pokemonlevel * pokemonlevel)
        PokemonBonus(CurrentPlayer) = PokemonBonus(CurrentPlayer) + 1
        PokemonCaught(CurrentPlayer, PokemonBonus(CurrentPlayer)) = CatchID
        LightEffect 1
        GiEffect 1
        If PokemonBonus(CurrentPlayer)MOD 3 = 0 Then 'turn on the extra ball light after catching 3 pokemons
            ExtraBallLight.State = 2
        End If
        StopCatchem_Timer
    Else 'change to another position
        CatchemTimer.Enabled = 0
        CatchemTimer.Enabled = 1
        CatchemTimer_Timer
    End If
if CatchID = 150 then 
    If   STANtimer.enabled = false Then
       STANtimer.enabled = True
       STANtimerstop.enabled = True
	End If
    End If
End Sub

Sub CatchMewtwo()
    Playsound "STANLEE"
    StanSee
    bCatchemMode = True
    CatchID = 150 'the number of Mewtwo in the reel
    PokemonLevel = 5
    CatchMaxHits = 6
    StartCatchem2
End Sub

Sub StanSee

Stan001.visible= True
Stan002.visible= True
Stan003.visible= True
Stan004.visible= True

End Sub

Sub StanHide

Stan001.visible= False
Stan002.visible= False
Stan003.visible= False
Stan004.visible= False


End Sub


'**************
' SlotMachine
'**************

Dim PokAward
PokAward = Array("","sa4", "sa1", "sa2", "sa3")

Sub StartSlotmachine() ' uses the HolePos variable
	if bMultiballMode Then 
    		vpmtimer.addtimer 1000, "CatchHoleExit '"
		Exit Sub
	End If

    HoleBonus = HoleBonus + 1
    DMDFlush
    DMD CL(0, "SLOT AWARDS"), CL(1, "SMALL POINTS"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "BIG POINTS"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "30S BALL SAVER"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "SMALL POINTS"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "BIG POINTS"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "UPGRADE HERO"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "SMALL POINTS"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "MYSTERIO MYSTERY"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "MULTIBALL"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD CL(0, "SLOT AWARDS"), CL(1, "CATCH STAN LEE"), PokAward(HolePos), eNone, eNone, eNone, 50, False, "fx_spinner"
    DOF 142, DOFPulse
    vpmtimer.AddTimer 2500, "GiveSlotAward '"
End Sub

Sub GiveSlotAward()
    If NOT bMultiBallMode Then PlaySound"quoteE"&RndNbr(7)
    Dim tmp
    DMDFlush
    tmp = RndNbr(9)
    Select Case tmp
        Case 0:AddScore INT(10000 * RND * 9)  'small points
            DMD CL(0, "YOUR AWARD"), CL(1, "SMALL POINTS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 1:AddScore INT(100000 * RND * 9) 'big points
            DMD CL(0, "YOUR AWARD"), CL(1, "BIG POINTS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 2:EnableBallSaver 30             'ball saver 30 seconds
            DMD CL(0, "YOUR AWARD"), CL(1, "30SEC BALL SAVER"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 3:AddScore INT(10000 * RND * 9)  'small points
            DMD CL(0, "YOUR AWARD"), CL(1, "SMALL POINTS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 4:AddScore INT(10000 * RND * 9)  'small points
            DMD CL(0, "YOUR AWARD"), CL(1, "SMALL POINTS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 5:AddCoin 250:IncrementBallType  'upgrade ball
            DMD CL(0, "YOUR AWARD"), CL(1, "UPGRADE HERO"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 6:AddScore INT(100000 * RND * 9) 'big points
            DMD CL(0, "YOUR AWARD"), CL(1, "BIG POINTS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 7:StartCoinFrenzy                'coin frenzy
            DMD CL(0, "YOUR AWARD"), CL(1, "MYSTERIO MYSTERY"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
        Case 8
            Select Case HolePos
                Case 1:AddScore INT(100000 * RND * 9) 'big points
                    DMD CL(0, "YOUR AWARD"), CL(1, "BIG POINTS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
                Case 2:Addmultiball 1   'enable picachu multiball
                    DMD CL(0, "YOUR AWARD"), CL(1, "MULTIBALL"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
                Case 3:StartRampBonus                 'Purple Man Ramp Bonus Mode - Activate Jackpots on the ramps for 30 sec
                    DMD CL(0, "YOUR AWARD"), CL(1, "RAMP BONUS"),PokAward(HolePos), eNone, eBlink, eNone, 2000, True, "po_fanfare6"
                Case 4:StartLoopBonus                 'Loop Bonus Mode
                    DMD CL(0, "YOUR AWARD"), CL(1, "LOOP BONUS"), PokAward(HolePos), eNone, eBlink, eNone, 2000, True, "po_fanfare6"
            End Select
        Case 9
            Select Case HolePos
                Case 1:StartTargetMode          'Target Bonus Mode
                    DMD CL(0, "YOUR AWARD"), CL(1, "TARGET BONUS"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
                Case 2:CatchMewtwo              'Catch Mewtwo
                    DMD CL(0, "YOUR AWARD"), CL(1, "CATCH STAN LEE"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
                Case 3:ExtraBallLight.State = 2 'Lit Extra Ball
                    DMD CL(0, "YOUR AWARD"), CL(1, "EXTRA BALL IS LIT"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
                Case 4:StartCharizardMode       'Charizard Bonus Mode
                    DMD CL(0, "YOUR AWARD"), CL(1, "BULLSEYE MODE"), PokAward(HolePos), eNone, eBlink, eNone, 1000, True, "po_fanfare6"
            End Select
    End Select
    GiEffect 1
    vpmtimer.addtimer 1500, "CatchHoleExit '"
End Sub
'**********************
' Charizard Bonus Mode
'**********************

Dim bCharizardMode

Sub StartCharizardMode()
    DMD "", CL(1, "BULLSEYE MODE"), "BULL", eNone, eBlink, eNone, 1000, True, ""  
    bCharizardMode = True
    'EggTarget5.IsDropped = FALSE
	RandomSoundDropTargetReset PEggTarget5
	DTRaise 1
    DOF 119, DOFPulse
    SetLightColor LightArrow5, orange, 2
    StopCharizardTimer.Enabled = True
End Sub

Sub StopCharizardTimer_Timer()
    StopCharizardTimer.Enabled = False
    bCharizardMode = False
    SetLightColor LightArrow5, white, 0
End Sub

'******************
' Egg/Hatch Mode
'******************

Dim bEggTargetsCompleted, EggID

Sub EggTarget1_Hit()
    CharizardShake
	STHit 1
    'PlaySoundAt SoundFXDOF("fx_target", 117, DOFPulse, DOFTargets), EggTarget1
    If Tilted Then Exit Sub
    PlaySoundEffect
    HatchLight1.State = 1:HatchLight1b.Visible = 1
    Addscore 20000
    CheckEggTargets
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget1"
    if BEYEtimer.enabled = false Then
   BEYEtimer.enabled = True
   BEYEtimerstop.enabled = True
	End If
End Sub

Sub EggTarget1o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub EggTarget2_Hit()
    CharizardShake
	STHit 2
    'PlaySoundAt SoundFXDOF("fx_target", 117, DOFPulse, DOFTargets), EggTarget2
    If Tilted Then Exit Sub
    PlaySoundEffect
    HatchLight2.State = 1:HatchLight2b.Visible = 1
    Addscore 20000
    CheckEggTargets
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget2"
    if BEYEtimer.enabled = false Then
   BEYEtimer.enabled = True
   BEYEtimerstop.enabled = True
	End If
End Sub

Sub EggTarget2o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub EggTarget3_Hit()
    CharizardShake
	STHit 3
    'PlaySoundAt SoundFXDOF("fx_target", 117, DOFPulse, DOFTargets), EggTarget3
    If Tilted Then Exit Sub
    PlaySoundEffect
    HatchLight3.State = 1:HatchLight3b.Visible = 1
    Addscore 20000
    CheckEggTargets
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget3"
    if BEYEtimer.enabled = false Then
   BEYEtimer.enabled = True
   BEYEtimerstop.enabled = True
	End If
End Sub

Sub EggTarget3o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub EggTarget4_Hit()
    CharizardShake
	STHit 4
    PlaySoundAt SoundFXDOF("fx_target", 117, DOFPulse, DOFTargets), EggTarget4
    If Tilted Then Exit Sub
    PlaySoundEffect
    HatchLight4.State = 1:HatchLight4b.Visible = 1
    Addscore 20000
    CheckEggTargets
    ' remember last trigger hit by the ball
    LastSwitchHit = "EggTarget4"
    if BEYEtimer.enabled = false Then
   BEYEtimer.enabled = True
   BEYEtimerstop.enabled = True
	End If
End Sub

Sub EggTarget4o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub EggTarget5_Hit()
    DTHit 1
'	PlaySoundAt SoundFXDOF("fx_target", 117, DOFPulse, DOFTargets), EggTarget5
    If NOT bMultiBallMode Then PlaySound"quoteD"&RndNbr(8)
    If Tilted Then Exit Sub
    PlaySoundEffect
    CharizardShake
    If LastSwitchHit = "RightInlaneTrigger" Then 'combo
        DMD "_", CL(1, "COMBO! " & FormatScore("30000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
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
      if BEYEtimer.enabled = false Then
         BEYEtimer.enabled = True
         BEYEtimerstop.enabled = True
	End If
End Sub

Sub EggTarget5_Dropped()
    If bCharizardMode Then
		RandomSoundDropTargetReset PEggTarget5
		DTRaise 1
        'EggTarget5.Isdropped = False
        DOF 119, DOFPulse
    End If
End Sub

Sub CheckEggTargets()
    If bEggTargetsCompleted = False Then
        If(HatchLight1.State + HatchLight2.State + HatchLight3.State + HatchLight4.State) = 4 Then
            bEggTargetsCompleted = True
            HatchReadyLight. State = 2
            EggMagnet.MagnetOn = True
        End If
    End If
End Sub

Sub EggHole_Hit()
    PlaySoundAt "fx_hole_enter", EggHole
    If NOT bMultiBallMode Then PlaySound"quoteD"&RndNbr(8)
    BallInHole = BallInHole + 1
    EggHole.DestroyBall
    If bEggTargetsCompleted Then
        EggID = INT(RND * 26)
        DMD RL(0, FormatScore(1000000)), RL(1, "BASE " &Eggname(EggID)), Eggs(EggID), eBlinkFast, eBlinkFast, eNone, 1500, True, ""
        AddScore 1000000
        EggBonus(CurrentPlayer) = EggBonus(CurrentPlayer) + 1
        If EggBonus(CurrentPlayer)MOD 3 = 0 Then 'start the Charizard Bonus Mode after hatching 3 eggs
            StartCharizardMode
        End If
        HatchReadyLight. State = 0
        FlashForMs HatchLight1b, 1000, 50, 0
        FlashForMs HatchLight2b, 1000, 50, 0
        FlashForMs HatchLight3b, 1000, 50, 0
        FlashForMs HatchLight4b, 1000, 50, 0
        FlashForMs Flasher9, 1000, 50, 0:FlashForMs Flasher9a, 1000, 50, 0
        FlashForMs Light9, 1000, 50, 0
        FlashForMs Flasher10, 1000, 50, 0:FlashForMs Flasher10a, 1000, 50, 0
        FlashForMs Light10, 1000, 50, 0
        FlashForMs ap1, 1000, 50, 0
        FlashForMs ap2, 1000, 50, 0
        Gieffect 1
        vpmtimer.Addtimer 3000, "EndEggMode '"
    Else
        vpmtimer.addtimer 1000, "CatchHoleExit '"
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
    'EggTarget5.IsDropped = 0
	RandomSoundDropTargetReset PEggTarget5
        DOF 119, DOFPulse
	DTRaise 1
    vpmtimer.addtimer 500, "CatchHoleExit '"
End Sub

'*****************
' objects shaking
'*****************

Dim PikaShake, CharmanShake, ChariShake, VenuShake

Sub PikachuShake()
    PikaShake = 6
    PikaShakeTimer.Enabled = True
End Sub

Sub PikaShakeTimer_Timer()
    M001.Transz = PikaShake / 2
    M002.Transz = PikaShake / 2
    M003.Transz = PikaShake / 2
    M004.Transz = PikaShake / 2
   
    If PikaShake = 0 Then Me.Enabled = False:Exit Sub
    If PikaShake < 0 Then
        PikaShake = ABS(PikaShake)- 0.1
    Else
        PikaShake = - PikaShake + 0.1
    End If
End Sub

Sub CharmanderShake()
    CharmanShake = 6
    CharmanShakeTimer.Enabled = True
End Sub

Sub CharmanShakeTimer_Timer()
    JESS001.Transz = CharmanShake / 2
    JESS002.Transz = CharmanShake / 2
    JESS003.Transz = CharmanShake / 2
    JESS004.Transz = CharmanShake / 2
    If CharmanShake = 0 Then Me.Enabled = False:Exit Sub
    If CharmanShake < 0 Then
        CharmanShake = ABS(CharmanShake)- 0.1
    Else
        CharmanShake = - CharmanShake + 0.1
    End If
End Sub

Sub CharizardShake()
    DOF 118, DOFPulse
    ChariShake = 6
    CharizardShakeTimer.Enabled = True
End Sub

Sub CharizardShakeTimer_Timer()
    BEYE001.Transy = ChariShake / 2
    BEYE002.Transy = ChariShake / 2
    BEYE003.Transy = ChariShake / 2
    BEYE004.Transy = ChariShake / 2
    If ChariShake = 0 Then Me.Enabled = False:Exit Sub
    If ChariShake < 0 Then
        ChariShake = ABS(ChariShake)- 0.1
    Else
        ChariShake = - ChariShake + 0.1
    End If
End Sub

Sub VenusaurShake()
    DOF 118, DOFPulse
    VenuShake = 6
    VenusaurShakeTimer.Enabled = True
End Sub

Sub VenusaurShakeTimer_Timer()
    KINGPIN3.Transy = VenuShake / 2
    KINGPIN2.Transy = VenuShake / 2
    KINGPIN.Transy = VenuShake / 2
    If VenuShake = 0 Then Me.Enabled = False:Exit Sub
    If VenuShake < 0 Then
        VenuShake = ABS(VenuShake)- 0.1
    Else
        VenuShake = - VenuShake + 0.1
    End If
End Sub

'**********************
'Effects only triggers
'**********************

Sub EffectTrigger1_Hit()
    FlashForMs Flasher11, 1000, 50, 0:FlashForMs Flasher11a, 1000, 50, 0
    FlashForMs Light11, 1000, 50, 0
    FlashForMs ap1, 1000, 50, 0
        FlashForMs ap2, 1000, 50, 0
End Sub

Sub EffectTrigger2_Hit()
    FlashForMs Flasher12, 1000, 50, 0:FlashForMs Flasher12a, 1000, 50, 0
    FlashForMs Light12, 1000, 50, 0
    FlashForMs ap1, 1000, 50, 0
    FlashForMs ap2, 1000, 50, 0
End Sub

'*******Mysterio Smoke and Animation************

Dim Fire1Pos, Fire2Pos, Fire3Pos, Fire4Pos, Fire5Pos, Fire6Pos, Fire7Pos,Fire8Pos, Flames
Flames = Array("fire01", "fire02", "fire03", "fire04", "fire05", "fire06", "fire07", "fire08", "fire09", _
    "fire10", "fire11", "fire12", "fire13", "fire14", "fire15", "fire16")

Sub StartFire
    Fire1Pos = 0
    Fire2Pos = 2
    Fire3Pos = 5
    Fire4Pos = 7
    Fire5Pos = 9
    Fire6Pos = 10
    Fire7Pos = 12
    Fire8Pos = 14
    FireTimer.Enabled = 1
End Sub

Sub FireTimer_Timer
    'debug.print fire1pos
    Fire1.ImageA = Flames(Fire1Pos)
    Fire2.ImageA = Flames(Fire2Pos)
    Fire3.ImageA = Flames(Fire3Pos)
    Fire4.ImageA = Flames(Fire4Pos)
    Fire5.ImageA = Flames(Fire5Pos)
    Fire6.ImageA = Flames(Fire6Pos)
    Fire7.ImageA = Flames(Fire7Pos)
    Fire8.ImageA = Flames(Fire8Pos)
    Fire1Pos = (Fire1Pos + 1) MOD 16
    Fire2Pos = (Fire2Pos + 1) MOD 16
    Fire3Pos = (Fire3Pos + 1) MOD 16
    Fire4Pos = (Fire4Pos + 1) MOD 16
    Fire5Pos = (Fire5Pos + 1) MOD 16
    Fire6Pos = (Fire6Pos + 1) MOD 16
    Fire7Pos = (Fire7Pos + 1) MOD 16
    Fire8Pos = (Fire8Pos + 1) MOD 16
End Sub

' TORCH lamps
Dim TOR1Pos, TOR2Pos, Torches
Torches = Array("TOR01", "TOR02", "TOR03", "TOR04", "TOR05", "TOR06", "TOR07", "TOR08", "TOR09", _
    "TOR10", "TOR11", "TOR12", "TOR13", "TOR14", "TOR15", "TOR16")

Sub StartTOR
    TOR1Pos = 0
    TOR2Pos = 2
    
    TorTimer.Enabled = 1
End Sub

Sub TorTimer_Timer
    'debug.print fire1pos
    TOR001.ImageA = Torches(Tor1Pos)
    TOR002.ImageA = Torches(Tor2Pos)
   
    Tor1Pos = (Tor1Pos + 1) MOD 16
    Tor2Pos = (Tor2Pos + 1) MOD 16
    
End Sub

' RAIN ****
Dim RAIN1Pos,RAIN2Pos,RAIN3Pos, RAINING
RAINING = Array("RAIN001", "RAIN002", "RAIN003", "RAIN004", "RAIN005", "RAIN006", "RAIN007", "RAIN008", "RAIN009", _
    "RAIN010", "RAIN011", "RAIN012", "RAIN013", "RAIN014", "RAIN015")

Sub StartRAIN
    RAIN1Pos = 0
    RAIN2Pos = 2
    RAIN3Pos = 2

    RainTimer.Enabled = 1
End Sub

Sub RainTimer_Timer
    'debug.print fire1pos
    RAIN.ImageA = RAINING(RAIN1Pos)
    RAIN2.ImageA = RAINING(RAIN2Pos)
    RAIN3.ImageA = RAINING(RAIN3Pos)

    RAIN1Pos = (RAIN1Pos + 1) MOD 15
    RAIN2Pos = (RAIN2Pos + 1) MOD 15
    RAIN3Pos = (RAIN3Pos + 1) MOD 15

End Sub

Sub RAINSee
RAIN.visible =True 
RAIN2.visible =True 
RAIN3.visible =True 

End Sub

Sub RAINUnsee
RAIN.visible=False
RAIN2.visible=False
RAIN3.visible=False

End Sub

'********

Sub SphereTimer_Timer
   spherelord.rotY = spherelord.rotY + 2
   spherelord2.rotY = spherelord2.rotY + 2
   spherelord3.rotY = spherelord3.rotY + 3
   spherelord4.rotY = spherelord4.rotY + 3
   spherelord5.rotY = spherelord5.rotY + 3
   spherelord7.rotY = spherelord7.rotY + 3
End sub

Sub BoatTimer_Timer()
    BoatDir = SIN(BoatStep * MyPi)
    BoatStep = (BoatStep + 1)MOD 360
    Mysterio1.Y = Mysterio1.Y + BoatDir * 0.3
    Mysterio2.Y = Mysterio2.Y + BoatDir * 0.3
    Mysterio4.Y = Mysterio4.Y + BoatDir * 0.5
    Mysterio5.Y = Mysterio5.Y + BoatDir * 0.5
    Mysterio6.Y = Mysterio6.Y + BoatDir * 0.5
    Mysterio7.Y = Mysterio7.Y + BoatDir * 0.5
    spherelord.Y = spherelord.Y + BoatDir * 0.3
    spherelord3.Y = spherelord3.Y + BoatDir * 0.5
    spherelord4.Y = spherelord4.Y + BoatDir * 0.5
    spherelord5.Y = spherelord5.Y + BoatDir * 0.5
    spherelord7.Y = spherelord7.Y + BoatDir * 0.5
End Sub

'*******Moving Characters********

Dim MyPi, TreeStep, TreeDir, BoatStep, BoatDir
MyPi = Round(4 * Atn(1), 6) / 90
TreeStep = 0
BoatStep = 0

Sub Trees_Timer()
    TreeDir = SIN(TreeStep * MyPi)
    TreeStep = (TreeStep + 1)MOD 360
    Mysterio3.RotY = - TreeDir *5
    Mysterio2.RotY = - TreeDir *10
    Mysterio4.RotY = - TreeDir *10
    Mysterio5.RotY = - TreeDir *10
    Mysterio6.RotY = - TreeDir *10
    Mysterio1.RotY = - TreeDir *10
    Elektra001.RotZ = - TreeDir *1
    Elektra002.RotZ = - TreeDir *1
    LUKE.RotX = - TreeDir *1
    LUKE2.RotX = - TreeDir *1
    IRONFIST.RotY = - TreeDir *1
    IRONFIST2.RotY = - TreeDir *1
    FISTS.RotY = - TreeDir *1
End Sub

'******Bumper Saw Blades**********

ringspin.enabled = 1

Sub ringspin_timer
	saw1.RotZ = saw1.RotZ + 1
    saw2.RotZ = saw2.RotZ + 1
    saw3.RotZ = saw3.RotZ - 1
    saw4.RotZ = saw4.RotZ - 1
	
End Sub

Sub Bumper_hit
Sawtimer.enabled = true
Ringspin.enabled = true
saw1.RotZ = saw1.RotZ + 1
saw2.RotZ = saw2.RotZ + 1
saw3.RotZ = saw3.RotZ - 1
saw4.RotZ = saw4.RotZ - 1
End sub

Sub sawtimer_timer
Ringspin.enabled = false
Sawtimer.enabled = false
End sub 

'******Gun********

Dim Gun1Pos, GunFrames
GunFrames = Array("MF-000", "MF-001", "MF-002", "MF-003", "MF-004", "MF-005", "MF-006", "MF-007", "MF-008", "MF-009", "MF-010", "MF-011", "MF-012", "MF-013", "MF-014", "MF-015", "MF-016", "MF-017", "MF-018", "MF-019", "MF-020", _
"MF-021", "MF-022", "MF-023", "MF-024", "MF-025", "MF-026", "MF-027", "MF-028", "MF-029")
 
Sub StartGun
GunFlash.visible = True
Gun1Pos = 0
GunTimer.Enabled = True
StopGunTimer.interval = 1500
StopGunTimer.Enabled = True
PlaySound "Gunsound"
End Sub
 
Sub GunTimer_timer
GunFlash.ImageA = GunFrames(Gun1Pos)
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

'*****Laser*******

Dim	Laser1Pos, LaserFrames 
LaserFrames = Array("ML-000", "ML-001", "ML-002", "ML-003", "ML-004", "ML-005", "ML-006", "ML-007", "ML-008", "ML-009", "ML-010", "ML-011", "ML-012", "ML-013", "ML-014", "ML-015", "ML-016", "ML-017", "ML-018", "ML-019", "ML-020", _
"ML-021", "ML-022", "ML-023", "ML-024", "ML-025", "ML-026", "ML-027", "ML-028", "ML-029")

Sub StartLaser
LaserFlash.visible = True
Laser1Pos = 0
LaserTimer.Enabled = True
StopLaserTimer.interval = 1200
StopLaserTimer.Enabled = True
PlaySound "Laser"
End Sub
 
Sub LaserTimer_timer
LaserFlash.ImageA = LaserFrames(Laser1Pos)
Laser1Pos = (Laser1Pos + 1) MOD 30
End Sub
 
Sub StopLaser
LaserFlash.visible = False
LaserTimer.Enabled = False
End Sub

Sub StopLaserTimer_Timer
StopLaser
Me.Enabled = False
LaserTimer.Enabled = False
End Sub

'*******HellsKitchen*******
Dim mystate
mystate = 0
flasher0001.TimerEnabled = 1
 
Sub flasher0001_Timer
    flasher0001.visible = mystate
    mystate = ABS(mystate - 1)
End Sub

'*********Sonar*********

Dim	Sonar1Pos, SonarFrames 
SonarFrames = Array("MS-000", "MS-001", "MS-002", "MS-003", "MS-004", "MS-005", "MS-006", "MS-007", "MS-008", "MS-009", "MS-010", "MS-011", "MS-012", "MS-013", "MS-014", "MS-015", "MS-016", "MS-017", "MS-018", "MS-019", "MS-020")

Sub StartSonar
SonarFlash.visible = True
Sonar1Pos = 0
SonarTimer.Enabled = True
StopSonarTimer.interval = 1400
StopSonarTimer.Enabled = True
PlaySound "Sonar"
End Sub
 
Sub SonarTimer_timer
SonarFlash.ImageA = SonarFrames(Sonar1Pos)
Sonar1Pos = (Sonar1Pos + 1) MOD 21 ' Fix: 21 frames (was 20, last frame never shown)
End Sub
 
Sub StopSonar
SonarFlash.visible = False
SonarTimer.Enabled = False
End Sub

Sub StopSonarTimer_Timer
StopSonar
Me.Enabled = False
SonarTimer.Enabled = False
End Sub

'*********Smoke***********

Dim	Smoke1Pos, SmokeFrames 
SmokeFrames = Array("MG-000", "MG-001", "MG-002", "MG-003", "MG-004", "MG-005", "MG-006", "MG-007", "MG-008", "MG-009", "MG-010", "MG-011", "MG-012", "MG-013", "MG-014", "MG-015", "MG-016", "MG-017", "MG-018", "MG-019", "MG-020", _
"MG-021", "MG-022", "MG-023", "MG-024", "MG-025", "MG-026", "MG-027", "MG-028", "MG-029", "MG-030", "MG-031", "MG-032", "MG-033", "MG-034", "MG-035", "MG-036", "MG-037", "MG-038", "MG-039", "MG-040", "MG-041", "MG-042", "MG-043")

Sub StartSmoke
SmokeFlash.visible = True
Smoke1Pos = 0
SmokeTimer.Enabled = True
StopSmokeTimer.interval = 1500
StopSmokeTimer.Enabled = True
PlaySound "Smoke"
End Sub
 
Sub SmokeTimer_timer
SmokeFlash.ImageA = SmokeFrames(Smoke1Pos)
Smoke1Pos = (Smoke1Pos + 1) MOD 44 ' Fix: 44 frames (was 43, last frame never shown)
End Sub
 
Sub StopSmoke
SmokeFlash.visible = False
SmokeTimer.Enabled = False
End Sub

Sub StopSmokeTimer_Timer
StopSmoke
Me.Enabled = False
SmokeTimer.Enabled = False
End Sub

'********LASER CHARGE********

Dim	Charge1Pos, ChargeFrames 
ChargeFrames = Array("CH-000", "CH-001", "CH-002", "CH-003", "CH-004", "CH-005", "CH-006", "CH-007", "CH-008", "CH-009", "CH-010", "CH-011", "CH-012", "CH-013", "CH-014", "CH-015", "CH-016", "CH-017", "CH-018", "CH-019", "CH-020", _
"CH-021", "CH-022", "CH-023", "CH-024", "CH-025", "CH-026", "CH-027", "CH-028")

Sub StartCharge
ChargeFlash.visible = True
Charge1Pos = 0
ChargeTimer.Enabled = True
StopChargeTimer.interval = 1500
StopChargeTimer.Enabled = True
PlaySound "Charge"
End Sub
 
Sub ChargeTimer_timer
ChargeFlash.ImageA = ChargeFrames(Charge1Pos)
Charge1Pos = (Charge1Pos + 1) MOD 29
End Sub
 
Sub StopCharge
ChargeFlash.visible = False
ChargeTimer.Enabled = False
End Sub

Sub StopChargeTimer_Timer
StopCharge
Me.Enabled = False
ChargeTimer.Enabled = False
End Sub

'******MYSTERIO HANDS******

Dim	Iron1Pos, IronFrames 
IronFrames = Array("FL-01", "FL-02", "FL-03", "FL-04", "FL-05", "FL-06", "FL-07", "FL-08", "FL-09", "FL-010", "FL-011", "FL-012", "FL-013", "FL-014", "FL-015", "FL-016", "FL-017", "FL-018", "FL-019", "FL-020", _
"FL-021", "FL-022", "FL-023", "FL-024", "FL-025", "FL-026", "FL-027", "FL-028", "FL-029", "FL-030", "FL-031", "FL-032", "FL-033", "FL-034", "FL-035", "FL-036", "FL-037", "FL-038", "FL-039", "FL-040", "FL-041", "FL-042", "FL-043", "FL-044", "FL-045", "FL-046", "FL-047", "FL-048")

Sub StartIron
IronFlash.visible = True
Iron1Pos = 0
IronTimer.Enabled = True
StopIronTimer.interval = 2000
StopIronTimer.Enabled = True
PlaySound "FRENZY"
End Sub
 
Sub IronTimer_timer
IronFlash.ImageA = IronFrames(Iron1Pos)
Iron1Pos = (Iron1Pos + 1) MOD 48 ' Fix: 48 frames (duplicate FL-03 removed; MOD 45 skipped the last frames)
End Sub
 
Sub StopIron
IronFlash.visible = False
IronTimer.Enabled = False
End Sub

Sub StopIronTimer_Timer
StopIron
Me.Enabled = False
IronTimer.Enabled = False
End Sub

'*******PORTAL******

Dim RotAngle4
RotAngle4 = 0

Sub PORTALLogo_Timer
    RotAngle4 = (RotAngle4+ 5)MOD 360
    FlasherlogoVP.Rotz = RotAngle4
    FlasherlogoVP2.Rotz = RotAngle4
End Sub

'******LUT MODES****

sub MurdockLights
		table1.colorgradeimage= "LUT Murdock"        
end Sub

sub MurdockLightsreset
		table1.colorgradeimage= "LUT 0"        
end Sub

sub MAYHEMLights
		table1.colorgradeimage= "LUT MAYHEM"
                
end Sub

sub MAYHEMLightsreset
		table1.colorgradeimage= "LUT 0"        
end Sub
'************Primitive Swap*******************

KINGPIN.visible = True
KINGPIN2.visible = False

' Initialize the timer
TimerJAG.Interval = 5
TimerJAG.Enabled = False
Dim Timer1Count: Timer1Count = 0

Sub TimerJAG_Timer
	Timer1Count = Timer1Count + 1
	Select Case Timer1Count
		Case 1:  
			KINGPIN.visible = False
			KINGPIN2.visible = True
		Case 100:  ' number of timer intervals to wait before swapping back
			KINGPIN.visible = True
			KINGPIN2.visible = False
			Timer1Count = 0
			TimerJAG.Enabled = False
	End Select
End Sub

'************MYSTERIO Swap*******************

Mysterio1.visible = True
Mysterio2.visible = False

' Initialize the timer
TimerJAZ.Interval = 5
TimerJAZ.Enabled = False
Dim Timer2Count: Timer2Count = 0

Sub TimerJAZ_Timer
	Timer2Count = Timer2Count + 1
	Select Case Timer2Count
		Case 1:  
			Mysterio1.visible = False
			Mysterio2.visible = True
		Case 300: ' number of timer intervals to wait before swapping back
			Mysterio1.visible = True
			Mysterio2.visible = False
			Timer2Count = 0
			TimerJAZ.Enabled = False
	End Select
End Sub

'MysterioZap**********
            Mysterio3.visible=False
            Fire4.visible=False
            light002.visible=False
            spherelord2.visible=False
            Mysterio1.visible=True
            spherelord.visible=True
            Fire1.visible=True
            Fire2.visible=True
            Fire3.visible=True
            light001.visible=True
            
' Initialize the timer
TimerJAM.Interval = 5
TimerJAM.Enabled = False
Dim Timer3Count: Timer3Count = 0

Sub TimerJAM_Timer
	Timer3Count = Timer3Count + 1
	Select Case Timer3Count
		Case 1:  
			Mysterio3.visible=True
            Fire4.visible=True
            spherelord4.visible=True
            light002.visible=True
            spherelord2.visible=True
            Mysterio1.visible=False
            Mysterio2.visible=False
            spherelord.visible=False
            Fire1.visible=False
            Fire2.visible=False
            Fire3.visible=False
            light001.visible=False
		Case 400: ' number of timer intervals to wait before swapping back
			Mysterio3.visible=False
            Fire4.visible=False
            light002.visible=False
            spherelord2.visible=False
            Mysterio1.visible=True
            spherelord.visible=True
            Fire1.visible=True
            Fire2.visible=True
            Fire3.visible=True
            light001.visible=True
			Timer3Count = 0
			TimerJAM.Enabled = False
	End Select
End Sub

'******iRON fIST HANDS GLOW***

Sub FlashFISTS (fDuration, fPeriod)
	FlashFISTSTimer.interval = fPeriod
	FlashFISTSTimer.enabled = True
	StopFISTSTimer.interval = fDuration
	StopFISTSTimer.Enabled = True
End Sub

Sub FlashFISTSTimer_Timer
	if FISTS.visible = false Then
		FISTS.visible = True
	Else
		FISTS.visible = False
	End If
End Sub

Sub StopFISTSTimer_Timer
	StopFISTSTimer.enabled = False
	FlashFISTSTimer.enabled = False
	FISTS.visible = False
End Sub

'******CANE GLOW***

Sub FlashCANE (gDuration, gPeriod)
	FlashCANETimer.interval = gPeriod
	FlashCANETimer.enabled = True
	StopCANETimer.interval = gDuration
	StopCANETimer.Enabled = True
End Sub

Sub FlashCANETimer_Timer
	if KINGPIN3.visible = false Then
		KINGPIN3.visible = True
	Else
		FISTS.visible = False
	End If
End Sub

Sub StopCANETimer_Timer
	StopCANETimer.enabled = False
	FlashCANETimer.enabled = False
	KINGPIN3.visible = False
End Sub
'***Mysterio Fakes******

Sub FAKEMAN1_hit
    FlashForMs MYSTLIGHT, 1000, 50, 0
	if FAKEMAN1.transz <> 0 Then exit Sub
    PlaySound"MYS"&RndNbr(7)
    Playsound "MYSTHIT"
	FAKEMAN1Down.Enabled = True
end Sub

Sub FAKEMAN2_hit
    FlashForMs MYSTLIGHT, 1000, 50, 0
	if FAKEMAN2.transz <> 0 Then exit Sub
    PlaySound"MYS"&RndNbr(7)
    Playsound "MYSTHIT"
	FAKEMAN2Down.Enabled = True
end Sub

Sub FAKEMAN3_hit
    FlashForMs MYSTLIGHT, 1000, 50, 0
	if FAKEMAN3.transz <> 0 Then exit Sub
    PlaySound"MYS"&RndNbr(7)
    Playsound "MYSTHIT"
	FAKEMAN3Down.Enabled = True
end Sub

Sub FAKEMAN4_hit
    FlashForMs MYSTLIGHT, 1000, 50, 0
	if FAKEMAN4.transz <> 0 Then exit Sub
    PlaySound"MYS"&RndNbr(7)
    Playsound "MYSTHIT"
	FAKEMAN4Down.Enabled = True
end Sub

Sub FAKEMAN1Up_timer
	if FAKEMAN1.transz = 0 then
		FAKEMAN1Up.Enabled = False
		exit Sub
	end If
	FAKEMAN1.visible = True
    MYSTERIO5.visible = True
    FIRE6.visible = True
    spherelord4.visible = True
	FAKEMAN1.collidable = True
	FAKEMAN1.transz = FAKEMAN1.transz + 10
End Sub

Sub FAKEMAN1Down_timer
	if FAKEMAN1.transz = -100 then
	FAKEMAN1Down.Enabled = False
	FAKEMAN1.visible = False
    MYSTERIO5.visible = False
    FIRE6.visible = False
    spherelord4.visible = False
		FAKEMAN1.collidable = False
        AddScore 5000
	end If
	FAKEMAN1.transz = FAKEMAN1.transz - 10
End Sub

Sub FAKEMAN2Up_timer
	if FAKEMAN2.transz = 0 then
		FAKEMAN2Up.Enabled = False
		exit Sub
	end If
	FAKEMAN2.visible = True
    MYSTERIO4.visible = True
    FIRE5.visible = True
    spherelord3.visible = True
	FAKEMAN2.collidable = True
	FAKEMAN2.transz = FAKEMAN2.transz + 10
End Sub

Sub FAKEMAN2Down_timer
	if FAKEMAN2.transz = -100 then
		FAKEMAN2Down.Enabled = False
		FAKEMAN2.visible = False
        MYSTERIO4.visible = False
        FIRE5.visible = False
        spherelord3.visible = False
		FAKEMAN2.collidable = False
        AddScore 5000
	end If
	FAKEMAN2.transz = FAKEMAN2.transz - 10
End Sub

Sub FAKEMAN3Up_timer
	if FAKEMAN3.transz = 0 then
		FAKEMAN3Up.Enabled = False
		exit Sub
	end If
	FAKEMAN3.visible = True
    MYSTERIO6.visible = True
    FIRE7.visible = True
    spherelord5.visible = True
	FAKEMAN3.collidable = True
	FAKEMAN3.transz = FAKEMAN3.transz + 10
End Sub

Sub FAKEMAN3Down_timer
	if FAKEMAN3.transz = -100 then
		FAKEMAN3Down.Enabled = False
		FAKEMAN3.visible = False
        MYSTERIO6.visible = False
        FIRE7.visible = False
        spherelord5.visible = False
		FAKEMAN3.collidable = False
        AddScore 5000
	end If
	FAKEMAN3.transz = FAKEMAN3.transz - 10
End Sub

Sub FAKEMAN4Up_timer
	if FAKEMAN4.transz = 0 then
		FAKEMAN4Up.Enabled = False
		exit Sub
	end If
	FAKEMAN4.visible = True
    MYSTERIO7.visible = True
    FIRE8.visible = True
    spherelord7.visible = True
	FAKEMAN4.collidable = True
	FAKEMAN4.transz = FAKEMAN4.transz + 10
End Sub

Sub FAKEMAN4Down_timer
	if FAKEMAN4.transz = -100 then
		FAKEMAN4Down.Enabled = False
		FAKEMAN4.visible = False
        MYSTERIO7.visible = False
        FIRE8.visible = False
        spherelord7.visible = False
		FAKEMAN4.collidable = False
        AddScore 5000
	end If
	FAKEMAN4.transz = FAKEMAN4.transz - 10
End Sub

'************FISTS Primitive Swap*******************

IRONFIST.visible = True
IRONFIST2.visible = False

' Initialize the timer
TimerJAB.Interval = 5
TimerJAB.Enabled = False
Dim Timer4Count: Timer4Count = 0

Sub TimerJAB_Timer
	Timer4Count = Timer4Count + 1
	Select Case Timer4Count
		Case 1:  
			IRONFIST.visible = False
			IRONFIST2.visible = True
		Case 60:  ' number of timer intervals to wait before swapping back
			IRONFIST.visible = True
			IRONFIST2.visible = False
			Timer4Count = 0
			TimerJAB.Enabled = False
	End Select
End Sub

'************LUKE Primitive Swap*******************

LUKE.visible = True
LUKE2.visible = False

' Initialize the timer
TimerJAL.Interval = 5
TimerJAL.Enabled = False
Dim Timer5Count: Timer5Count = 0

Sub TimerJAL_Timer
	Timer5Count = Timer5Count + 1
	Select Case Timer5Count
		Case 1:  
			LUKE.visible = False
			LUKE2.visible = True
		Case 50:  ' number of timer intervals to wait before swapping back
			LUKE.visible = True
			LUKE2.visible = False
			Timer5Count = 0
			TimerJAL.Enabled = False
	End Select
End Sub

'**BULLSEYE THROW***

Sub BEYEtimer_Timer
countr2 = countr2 + 1 : If Countr2 > 4 then Countr2 = 1 : end If
select case countr2
case 1 : BEYE001.z=55:BEYE002.z=-250:BEYE003.z=-250:BEYE004.z=-250
case 2 : BEYE001.z=-250:BEYE002.z=55:BEYE003.z=-250:BEYE004.z=-250
case 3 : BEYE001.z=-250:BEYE002.z=-250:BEYE003.z=55:BEYE004.z=-250
case 4 : BEYE001.z=-250:BEYE002.z=-250:BEYE003.z=-250:BEYE004.z=55

end Select
End Sub 

Sub BEYEtimerstop_Timer()
    if countr2 = 1 then
   BEYEtimer.enabled = False
   BEYETimerstop.enabled = False
    end if
End Sub

'***PUNISHER SHOOT****

Sub PUNISHERtimer_Timer
countr3 = countr3 + 1 : If Countr3 > 4 then Countr3 = 1 : end If
select case countr3
case 1 : PUNISHER001.z=55:PUNISHER002.z=-250:PUNISHER003.z=-250:PUNISHER004.z=-250
case 2 : PUNISHER001.z=-250:PUNISHER002.z=55:PUNISHER003.z=-250:PUNISHER004.z=-250
case 3 : PUNISHER001.z=-250:PUNISHER002.z=-250:PUNISHER003.z=55:PUNISHER004.z=-250
case 4 : PUNISHER001.z=-250:PUNISHER002.z=-250:PUNISHER003.z=-250:PUNISHER004.z=55

end Select
End Sub 

Sub PUNISHERtimerstop_Timer()
    if countr3 = 1 then
   PUNISHERtimer.enabled = False
   PUNISHERTimerstop.enabled = False
    end if
End Sub

'***ELEKTRA CLANK****

Sub ELEKTRAtimer_Timer
Playsound "EBLADE"
countr4 = countr4 + 1 : If Countr4 > 2 then Countr4 = 1 : end If
select case countr4
case 1 : ELEKTRA001.z=55:ELEKTRA002.z=-250
case 2 : ELEKTRA001.z=-250:ELEKTRA002.z=55

end Select
End Sub 

Sub ELEKTRAtimerstop_Timer()
    if countr4 = 1 then
   ELEKTRAtimer.enabled = False
   ELEKTRATimerstop.enabled = False
    end if
End Sub

'***NINJAS****

Sub NINJAtimer_Timer
IF NOT bcoinfrenzy Then Playsound "EBLADE2"
countr7 = countr7 + 1 : If Countr7 > 2 then Countr7 = 1 : end If
select case countr7
case 1 : NINJA001.z=95:NINJA002.z=-250
case 2 : NINJA001.z=-250:NINJA002.z=95


end Select
End Sub 

Sub NINJAtimerstop_Timer()
    if countr7 = 1 then
   NINJAtimer.enabled = False
   NINJATimerstop.enabled = False
    end if
End Sub

'***NINJAS****

Sub NINJA2timer_Timer
IF NOT bcoinfrenzy Then Playsound "EBLADE2"
countr8 = countr8 + 1 : If Countr8 > 2 then Countr8 = 1 : end If
select case countr8
case 1 : NINJA003.z=95:NINJA004.z=-250
case 2 : NINJA003.z=-250:NINJA004.z=95

end Select
End Sub 

Sub NINJA2timerstop_Timer()
    if countr8 = 1 then
   NINJA2timer.enabled = False
   NINJA2Timerstop.enabled = False
    end if
End Sub

'***JESSICA WRENCH****

Sub JESSICAtimer_Timer
Playsound "swing"
countr5 = countr5 + 1 : If Countr5 > 4 then Countr5 = 1 : end If
select case countr5
case 1 : JESS001.z=55:JESS002.z=-250:JESS003.z=-250:JESS004.z=-250
case 2 : JESS001.z=-250:JESS002.z=55:JESS003.z=-250:JESS004.z=-250
case 3 : JESS001.z=-250:JESS002.z=-250:JESS003.z=55:JESS004.z=-250
case 4 : JESS001.z=-250:JESS002.z=-250:JESS003.z=-250:JESS004.z=55

end Select
End Sub 

Sub JESSICAtimerstop_Timer()
    if countr5 = 1 then
   JESSICAtimer.enabled = False
   JESSICATimerstop.enabled = False
    end if
End Sub

'**DAREDEVIL***

Sub DAREtimer_Timer
countr6 = countr6 + 1 : If Countr6 > 4 then Countr6 = 1 : end If
select case countr6
case 1 : M001.z=55:M002.z=-250:M003.z=-250:M004.z=-250
case 2 : M001.z=-250:M002.z=55:M003.z=-250:M004.z=-250
case 3 : M001.z=-250:M002.z=-250:M003.z=55:M004.z=-250
case 4 : M001.z=-250:M002.z=-250:M003.z=-250:M004.z=55
end Select
End Sub 

Sub DAREtimerstop_Timer()
    if countr6 = 1 then
   DAREtimer.enabled = False
   DARETimerstop.enabled = False
    end if
End Sub

'**SPIDERMAN***

Sub SPIDtimer_Timer
countr9 = countr9 + 1 : If Countr9 > 24 then Countr9 = 1 : end If
select case countr9
case 1 : SP001.z=151:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 2 : SP001.z=-350:SP002.z=151:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 3 : SP001.z=-350:SP002.z=-350:SP003.z=151:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 4 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=151:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 5 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=151:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 6 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=151:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 7 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=151:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 8 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=151:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 9 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=151:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 10 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=151:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 11 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=151:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 12 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=151:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 13 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=151:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 14 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=151:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 15 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=151:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 16: SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=151:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 17 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=151:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 18 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=151:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 19 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=151:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 20 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=151:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 21 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=151:SP022.z=-350:SP023.z=-350:SP024.z=-350
case 22 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=151:SP023.z=-350:SP024.z=-350
case 23 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=151:SP024.z=-350
case 24 : SP001.z=-350:SP002.z=-350:SP003.z=-350:SP004.z=-350:SP005.z=-350:SP006.z=-350:SP007.z=-350:SP008.z=-350:SP009.z=-350:SP010.z=-350:SP011.z=-350:SP012.z=-350:SP013.z=-350:SP014.z=-350:SP015.z=-350:SP016.z=-350:SP017.z=-350:SP018.z=-350:SP019.z=-350:SP020.z=-350:SP021.z=-350:SP022.z=-350:SP023.z=-350:SP024.z=151

end Select
End Sub 

Sub SPIDtimerstop_Timer()
    if countr9 = 1 then
   SPIDtimer.enabled = False
   SPIDTimerstop.enabled = False
    end if
End Sub

'***PURPLE MAN****

Sub PURPLEtimer_Timer
countrrrr = countrrrr + 1 : If Countrrrr > 7 then Countrrrr = 1 : end If
select case countrrrr
case 1 : purp001.z=230:purp002.z=-250:purp003.z=-250:purp004.z=-250:purp005.z=-250:purp006.z=-250:purp007.z=-250
case 2 : purp001.z=-250:purp002.z=230:purp003.z=-250:purp004.z=-250:purp005.z=-250:purp006.z=-250:purp007.z=-250
case 3 : purp001.z=-250:purp002.z=-250:purp003.z=230:purp004.z=-250:purp005.z=-250:purp006.z=-250:purp007.z=-250
case 4 : purp001.z=-250:purp002.z=-250:purp003.z=-250:purp004.z=230:purp005.z=-250:purp006.z=-250:purp007.z=-250
case 5 : purp001.z=-250:purp002.z=-250:purp003.z=-250:purp004.z=-250:purp005.z=230:purp006.z=-250:purp007.z=-250
case 6 : purp001.z=-250:purp002.z=-250:purp003.z=-250:purp004.z=-250:purp005.z=-250:purp006.z=230:purp007.z=-250
case 7 : purp001.z=-250:purp002.z=-250:purp003.z=-250:purp004.z=-250:purp005.z=-250:purp006.z=-250:purp007.z=230
end Select
End Sub 

Sub PURPLEtimerstop_Timer()
    if countrrrr = 1 then
   PURPLEtimer.enabled = False
   PURPLETimerstop.enabled = False
    end if
End Sub

'***MOLE MAN****

Sub MOLEtimer_Timer
countrM = countrM + 1 : If CountrM > 5 then CountrM = 1 : end If
select case countrM
case 1 : MOLE001.z=12:MOLE002.z=-250:MOLE003.z=-250:MOLE004.z=-250:MOLE005.z=-250
case 2 : MOLE001.z=-250:MOLE002.z=12:MOLE003.z=-250:MOLE004.z=-250:MOLE005.z=-250
case 3 : MOLE001.z=-250:MOLE002.z=-250:MOLE003.z=12:MOLE004.z=-250:MOLE005.z=-250
case 4 : MOLE001.z=-250:MOLE002.z=-250:MOLE003.z=-250:MOLE004.z=12:MOLE005.z=-250
case 5 : MOLE001.z=-250:MOLE002.z=-250:MOLE003.z=-250:MOLE004.z=-250:MOLE005.z=12
end Select
End Sub 

Sub MOLEtimerstop_Timer()
    if countrM = 1 then
   MOLEtimer.enabled = False
   MOLETimerstop.enabled = False
    end if
End Sub


'***STAN****

Sub STANtimer_Timer
countrSL = countrSL + 1 : If CountrSL > 4 then CountrSL = 1 : end If
select case countrSL
case 1 : STAN001.z=55:STAN002.z=-250:STAN003.z=-250:STAN004.z=-250
case 2 : STAN001.z=-250:STAN002.z=55:STAN003.z=-250:STAN004.z=-250
case 3 : STAN001.z=-250:STAN002.z=-250:STAN003.z=55:STAN004.z=-250
case 4 : STAN001.z=-250:STAN002.z=-250:STAN003.z=-250:STAN004.z=55
end Select
End Sub 

Sub STANtimerstop_Timer()
    if countrSL = 1 then
   STANtimer.enabled = False
   STANTimerstop.enabled = False
    end if
End Sub

'**************************
' LAVA lamps
'**************************

Dim LAVA1Pos,LAVAFLOW
LAVAFLOW = Array("f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", _
    "f10", "f11", "f12", "f13", "f14", "f15", "f16")

Sub StartLAVA
    LAVA1Pos = 0
       
    LAVATimer.Enabled = 1
End Sub

Sub LAVATimer_Timer
    'debug.print fire1pos
    LAVA1.ImageA = LAVAFLOW(LAVA1Pos) 
    LAVA1Pos = (LAVA1Pos + 1) MOD 16
    
End Sub

Sub MystSee
bWorldPFActive = False
lava1.visible =True
PF2World.visible = True
End Sub
Sub MystUnsee
bWorldPFActive = True
lava1.visible=False
pf2world.visible = False
End Sub

Sub KINGSee
bWorldPFActive = False
PF3World.visible = True
End Sub
Sub KINGUnsee
bWorldPFActive = True
pf3world.visible = False
End Sub

'*******************************************
'  Ramp Triggers
'*******************************************
Sub ramptrigger01_hit()
	WireRampOn True	 'Play Plastic Ramp Sound
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
	WireRampOn True	 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger05_hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
End Sub

Sub ramptrigger05_unhit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
End Sub

Sub ramptrigger06_hit()
	WireRampOff	 'Exiting Wire Ramp Stop Playing Sound
End Sub

Sub ramptrigger06_unhit()
	RandomSoundRampStop ramptrigger06
End Sub

'******************************************************
'	ZPHY:  GENERAL ADVICE ON PHYSICS
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
Dim DT1

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

Set DT1 = (new DropTarget)(EggTarget5, EggTarget5a, PEggTarget5, 1, 0, False)

Dim DTArray
DTArray = Array(DT1)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 90 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
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
	Dim i
	i = DTArrayID(switch)
	
	PlayTargetSound
	DTArray(i).animate = DTCheckBrick(ActiveBall,DTArray(i).prim)
	If DTArray(i).animate = 1 Or DTArray(i).animate = 3 Or DTArray(i).animate = 4 Then
		DTBallPhysics ActiveBall, DTArray(i).prim.rotz, DTMass
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
				DTAction switchid
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
			Dim gBOT
			gBOT = GetBalls
			
			For b = 0 To UBound(gBOT)
				If InRotRect(gBOT(b).x,gBOT(b).y,prim.x, prim.y, prim.rotz, - 25, - 10,25, - 10,25,25, - 25,25) And gBOT(b).z < prim.z + DTDropUnits + 25 Then
					gBOT(b).velz = 20
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
		'If UsingROM Then controller.Switch(Switchid mod 100) = 0
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

Sub DTAction(switchid)
	Select Case switchid
		Case 1
			'Addscore 1000
			'ShadowDT(0).visible = False	
		Case 2
			'Addscore 1000
			'ShadowDT(1).visible = False
		Case 3
			'Addscore 1000
			'ShadowDT(2).visible = False
	End Select
End Sub

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
Dim ST1, ST2, ST3, ST4, ST5, ST6, ST7, ST8, ST9, ST10, ST11, ST12, ST13

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


Set ST1 = (new StandupTarget)(EggTarget1, PEggTarget1,1, 0)
Set ST2 = (new StandupTarget)(EggTarget2, PEggTarget2,2, 0)
Set ST3 = (new StandupTarget)(EggTarget3, PEggTarget3,3, 0)
Set ST4 = (new StandupTarget)(EggTarget4, PEggTarget4,4, 0)
Set ST5 = (new StandupTarget)(PikaTarget1, PPikaTarget1,5, 0)
Set ST6 = (new StandupTarget)(PikaTarget2, PPikaTarget2,6, 0)
Set ST7 = (new StandupTarget)(PikaTarget3, PPikaTarget3,7, 0)
Set ST8 = (new StandupTarget)(PikaTarget4, PPikaTarget4,8, 0)
Set ST9 = (new StandupTarget)(PikaTarget5, PPikaTarget5,9, 0)
Set ST10 = (new StandupTarget)(PikaTarget6, PPikaTarget6,10, 0)
Set ST11 = (new StandupTarget)(PikaTarget7, PPikaTarget7,11, 0)
Set ST12 = (new StandupTarget)(VenusaurLock1, PVenusaurLock1,12, 0)
Set ST13 = (new StandupTarget)(VenusaurLock2, PVenusaurLock2,13, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
'   STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST1, ST2, ST3, ST4, ST5, ST6, ST7, ST8, ST9, ST10, ST11, ST12, ST13)

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
			STAction switch
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


Sub STAction(Switch)
	Select Case Switch
		Case 1
			'Addscore 1000
			'Flash1 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash1 False'"   'Disable the flash after short time, just like a ROM would do
		Case 2
			'Addscore 1000
			'Flash2 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash2 False'"   'Disable the flash after short time, just like a ROM would do
		Case 3
			'Addscore 1000
			'Flash3 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash3 False'"   'Disable the flash after short time, just like a ROM would do
		Case 4
			'Addscore 1000
			'Flash3 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash3 False'"   'Disable the flash after short time, just like a ROM would do
		Case 5
			'Addscore 1000
			'Flash3 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash3 False'"   'Disable the flash after short time, just like a ROM would do
		Case 6
			'Addscore 1000
			'Flash3 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash3 False'"   'Disable the flash after short time, just like a ROM would do
		Case 7
			'Addscore 1000
			'Flash3 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash3 False'"   'Disable the flash after short time, just like a ROM would do
		Case 8
			'Addscore 1000
			'Flash3 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash3 False'"   'Disable the flash after short time, just like a ROM would do
		Case 9
			'Addscore 1000
			'Flash3 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash3 False'"   'Disable the flash after short time, just like a ROM would do
		Case 10
			'Addscore 1000
			'Flash3 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash3 False'"   'Disable the flash after short time, just like a ROM would do
		Case 11
			'Addscore 1000
			'Flash3 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash3 False'"   'Disable the flash after short time, just like a ROM would do
		Case 12
			'Addscore 1000
			'Flash3 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash3 False'"   'Disable the flash after short time, just like a ROM would do
		Case 13
			'Addscore 1000
			'Flash3 True 'Demo of the flasher
			'vpmTimer.AddTimer 150,"Flash3 False'"   'Disable the flash after short time, just like a ROM would do
	End Select
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
		aBallShadow(b).Y = 2148.333
	Next
	
	' exit the sub if no balls on the table
	If UBound(BOT) =  - 1 Then Exit Sub

Idol2.Roty = 120 - (BOT(0).Y)\15
SAT.Roty = - 120 + (BOT(0).Y)\15	
	' play the rolling sound for each ball
	For b = 0 To UBound(BOT)
		aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z - Ballsize / 2
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

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
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
    DOF 128, DOFPulse  ' Example DOF for targets (adjust ID as needed)
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
			zCol_Rubber_Post025.y = 1523.66
			Primitive035.y = 1523.66
			RubberOutlaneLeftEasy.visible = True
			RubberOutlaneLeftMed.visible = False
			RubberOutlaneLeftHard.visible = False
		Case 1
			zCol_Rubber_Post025.y = 1525.66
			Primitive035.y = 1525.66
			RubberOutlaneLeftEasy.visible = False
			RubberOutlaneLeftMed.visible = True
			RubberOutlaneLeftHard.visible = False
		Case 2
			zCol_Rubber_Post025.y = 1527.66
			Primitive035.y = 1527.66
			RubberOutlaneLeftEasy.visible = False
			RubberOutlaneLeftMed.visible = False
			RubberOutlaneLeftHard.visible = True
	End Select
End Sub

Sub UpdateRightOutlanePosts(Opt)
	Select Case Opt
		Case 0
			zCol_Rubber_Post026.y = 1521.996
			Primitive032.y = 1521.996
			RubberOutlaneRightEasy.visible = True
			RubberOutlaneRightMed.visible = False
			RubberOutlaneRightHard.visible = False
		Case 1
			zCol_Rubber_Post026.y = 1523.996
			Primitive032.y = 1523.996
			RubberOutlaneRightEasy.visible = False
			RubberOutlaneRightMed.visible = True
			RubberOutlaneRightHard.visible = False
		Case 2
			zCol_Rubber_Post026.y = 1525.996
			Primitive032.y = 1525.996
			RubberOutlaneRightEasy.visible = False
			RubberOutlaneRightMed.visible = False
			RubberOutlaneRightHard.visible = True
	End Select
End Sub

'DOF Config for solenoids by Outhere

'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 Right Slingshot
'105 
'106 
'107 Bumper Top
'108 Bumper Centere Left
'109 Bumper Centere Right
'110 Bumper
'111 
'112 
'113 
'114 VenusaurKickBall
'115 
'116 
'117 
'118 
'119 DropTargetReset
'120 CatchHoleExit
'121 
'122 Knocker
'123 Ball Release
'124 
'125 
'136 spinner
'141 AutoPlunger + Manual Plunger
'144 
'145 
'146 
'147 