' ****************************************************************
'          NOSFERATU 1922 (WPX Original 2023)
'             for VISUAL PINBALL X 10.8
'         Uses FlexDMD for cabinet / FS mode
'                  by dea TEE
' ****************************************************************
Option Explicit
Randomize

'==================== PLayer Options ==================================

' Sound and music volumes
Const SongVolume = 1 ' 1 is full volume, but I set it quite low to listen better the other sounds since I use headphones, adjust to your setup :)

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
'or keep it False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = True

'==================== End of Player Options ===========================

Const BallSize = 50 ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1  ' standard ball mass

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
Const cGameName = "Nosferatu"     'used for DOF and saving some values
Const myVersion = "1.00"
Const MaxPlayers = 1         ' from 1 to 4
Const MaxMultiplier = 7      ' limit playfield multiplier
Const MaxBonusMultiplier = 1 'limit Bonus multiplier
Const BallsPerGame = 3       ' usually 3 or 5
Const MaxMultiballs = 5      ' max number of balls during multiballs

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True Then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(1)
Dim PlayfieldMultiplier(7)
Dim BallSaverTime ' in seconds of the first ball
Dim bBonusHeld
Dim BallsRemaining(1)
Dim ExtraBallsAwards(1)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot(1)
Dim SuperJackpot(1)
Dim LockedBalls
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode
Dim LoopCount
Dim ComboCount
Dim ComboHits(1)
Dim ComboValue(1)
Dim x
Dim GiOn

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock(1)
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
'Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJackpot

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs
Dim LeftMagnet

'*****************************************************************************************************
' CREDITS
' Initial table created by fuzzel, jimmyfingers, jpsalas, toxie & unclewilly (in alphabetical order)
' Flipper primitives by zany
' Ball rolling sound script by jpsalas
' Ball shadow by ninuzzu
' Ball control & ball dropping sound by rothbauerw
' DOF by arngrim
' Positional sound helper functions by djrobx
' Plus a lot of input from the whole community (sorry if we forgot you :/)
'*****************************************************************************************************
'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound "fx_nudge", 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 6:PlaySound "fx_nudge", 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound "fx_nudge", 0, 1, 1, 0.25

    If keycode = LeftMagnaSave Then bLutActive = True:SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If
    If Keycode = AddCreditKey AND NOT bFreePlay Then
        Credits = Credits + 1
        'if bFreePlay = False Then DOF 125, DOFOn
        If(Tilted = False) Then
            DMDFlush
            DMD "", CL("CREDITS " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "plungerpull", plunger
    End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then PlaySound "flip_hit_1" 'only check the tilt during game
        If keycode = RightTiltKey Then PlaySound "flip_hit_1"
        If keycode = CenterTiltKey Then PlaySound "flip_hit_1"

	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToEnd
        PlaySound "buzzL",-1
		PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
	'InstantInfoTimer.Enabled = True
End If

	If keycode = RightFlipperKey Then
		RightFlipper.RotateToEnd
        PlaySound "buzz",-1
		PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
	'InstantInfoTimer.Enabled = True
End If

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                Else
                    If(Credits> 0) Then
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
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If hsbModeActive Then
        Exit Sub
    End If

    If keycode = LeftMagnaSave Then bLutActive = False:HideLUT

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "plunger", plunger
        DOF 159, DOFpulse
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            LeftFlipper.RotateToStart
		PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
	    StopSound "buzzL"
            'InstantInfoTimer.Enabled = False
            'If bInstantInfo Then
                'DMDScoreNow
                'bInstantInfo = False
            End If
        End If
        If keycode = RightFlipperKey Then
            RightFlipper.RotateToStart
		PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
	    StopSound "buzz"
            'InstantInfoTimer.Enabled = False
            'If bInstantInfo Then
                'DMDScoreNow
                'bInstantInfo = False
            End If


End Sub

'Sub InstantInfoTimer_Timer
    'InstantInfoTimer.Enabled = False
    'If NOT hsbModeActive Then
        'bInstantInfo = True
        'DMDFlush
        'InstantInfo
    'End If
'End Sub


'*****GI Lights On
dim xx

For each xx in GI:xx.State = 1: Next

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
PlaySound "SJ_Chime_100a"
CheckMultiplier
Addscore 100
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1

End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
PlaySound "SJ_Chime_100a" 
CheckMultiplier
Addscore 100
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0
    End Select
    LStep = LStep + 1
End Sub


'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function


'*****************************************
'   rothbauerw's Manual Ball Control
'*****************************************

Dim BCup, BCdown, BCleft, BCright
Dim ControlBallInPlay, ControlActiveBall
Dim BCvel, BCyveloffset, BCboostmulti, BCboost

BCboost = 1				'Do Not Change - default setting
BCvel = 4				'Controls the speed of the ball movement
BCyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
BCboostmulti = 3		'Boost multiplier to ball veloctiy (toggled with the B key) 

ControlBallInPlay = false

Sub StartBallControl_Hit()
	Set ControlActiveBall = ActiveBall
	ControlBallInPlay = true
End Sub

Sub StopBallControl_Hit()
	ControlBallInPlay = false
End Sub	

Sub BallControlTimer_Timer()
	If EnableBallControl and ControlBallInPlay then
		If BCright = 1 Then
			ControlActiveBall.velx =  BCvel*BCboost
		ElseIf BCleft = 1 Then
			ControlActiveBall.velx = -BCvel*BCboost
		Else
			ControlActiveBall.velx = 0
		End If

		If BCup = 1 Then
			ControlActiveBall.vely = -BCvel*BCboost
		ElseIf BCdown = 1 Then
			ControlActiveBall.vely =  BCvel*BCboost
		Else
			ControlActiveBall.vely = bcyveloffset
		End If
	End If
End Sub


'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds)
'********************************************************************

Const tnob = 5 ' total number of balls
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

    For b = 0 to UBound(BOT)
        ' play the rolling sound for each ball
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS v3 (VPX 10.8)
'*****************************************

Sub LeftFlipper_Animate()
    FlipperLSh.RotZ = LeftFlipper.CurrentAngle
End Sub

Sub RightFlipper_Animate()
    FlipperRSh.RotZ = RightFlipper.CurrentAngle
End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
        'If BOT(b).X < Table1.Width/2 Then
        '    BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        'Else
        '    BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        'End If
		BallShadow(b).X = BOT(b).X + (BOT(b).X - (Table1.Width/2)) * 1.25 / BallSize
        BallShadow(b).Y = BOT(b).Y + 12
		BallShadow(b).Size_X = 5
		BallShadow(b).Size_Y = 5
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

'physucs?

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

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
        .InitImpulseP swplunger002, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Left Magnet
    'Set LeftMagnet = New cvpmMagnet
    'With LeftMagnet
        '.InitMagnet Magnet1, 20
        '.GrabCenter = True
        '.CreateEvents "LeftMagnet"
    'End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    'VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Credits = 0
    Loadhs

    ' Initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = True 'we do not want coins

    'if bFreePlay Then DOF 125, DOFOn

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    BallSaverTime = 1 'at the start and multiball
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    bMultiBallStarted = False
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
    LockedBalls = 0
    ' set any lights for the attract mode
    'vpmtimer.addtimer 2000, "GiOn '"
    StartAttractMode

    ' Load table color
    LoadLut
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
    If bNOSFER = True Then
        PlaySong "music_nosferatu"
    Else
        PlaySong "music_gp"
    End If
End Sub

Sub StopSong
    StopSound Song
    Song = ""
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
    'GiOn
PlaySound "cbell"
    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        PlayfieldMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
        StopMBmodes = False
    Next

    ' initialise any other flags
    Tilt = 0

    ' initialise specific Game variables
    'Game_Init()
    UpdateBallInPlay

    ' you may wish to start some music, play a sound, do whatever at this point
    PlaySound ""

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
    'DMDScoreNow

    ' set the current players bonus multiplier back down to 1X
    ' SetBonusMultiplier 1

    ' Set the playfield multiplier to 1
    SetPlayfieldMultiplier 1

    ' reset any drop targets, lights, game Mode etc..

    'BonusPoints(CurrentPlayer) = 0
    bExtraBallWonThisBall = False

    'Reset any table specific
    'ResetNewBallVariables

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    'bSkillShotReady = True

'Change the music ?
PlaySong "music_gp"
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1
    UpdateBallInPlay

    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4

' if there is 2 or more balls Then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BloodisLife = True Then
        bMultiBallMode = True
        bAutoPlunger = True
    End If
    If KickerJacks = True Then
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
            If mBalls2Eject = 0 Then 'if there are no more balls to eject Then stop the timer
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
        if BallsOnPlayfield = 0 Then
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

Sub NextBallReset ()
lb1.state = 0:lb2.state = 0:lb3.state = 0:lb4.state = 0:lb5.state = 0:lb6.state = 0:lb7.state = 0:lb8.state = 0
deadsl001.state = 0:plag2.state = 0:Light003.state = 0:PlagueCenterLight.state = 0
PLx1.state = 0:PLx2.state = 0:PLx3.state = 0
Target1N.IsDropped = False:Target2O.IsDropped = False:Target3S.IsDropped = False:Target4F.IsDropped = False:Target5R.IsDropped = False:Target6A.IsDropped = False:Target7T.IsDropped = False:Target8U.IsDropped = False
PlaySoundAt "fx_droptarget", Target2O:PlaySoundAt "fx_droptarget", Target7T
saucl1.state = 0:saucl2.state = 0:saucl3.state = 0
Enos.state = 0:Nnos.state = 0:Onos.state = 0:Snos.state = 0:Fnos.state = 0:Rnos.state = 0:Anos.state = 0:Tnos.state = 0:Unos.state = 0
BILl001.state = 0:BILl002.state = 0:kickL1.state = 0:kickL2.state = 0:kickL3.state = 0
fl1b.state = 0:fl2l.state = 0:fl3o.state = 0:fl4o.state = 0:fl5d.state = 0:fl6i.state = 0:fl7s.state = 0:fl8l.state = 0:fl9i.state = 0:fl10f.state = 0:fl11e.state = 0
PegLight.state = 0:PegLight001.state = 0:PegLight002.state = 0:PegLight003.state = 0:PegLight004.state = 0:PegLight005.state = 0:PegLight006.state = 0
PegLight007.state = 0:PegLight008.state = 0:PegLight009.state = 0:PegLight010.state = 0
EYELightR.state = 0
EYELightL.state = 0
LCRL.state = 2
LskL.state = 0
WWTarget001.IsDropped = True
wul001.state = 0
DoorLeft.IsDropped = False
DoorLeft001.IsDropped = True
PlaySoundAt "fx_droptargetreset", DoorLeft
sealite1.state = 0
sealite2.state = 0
sealite3.state = 0
DoorRight.IsDropped = False
DoorRight001.IsDropped = True
PlaySoundAt "fx_droptargetreset", DoorRight
RCRL.state = 2
RskL.state = 0
CMTarget001.IsDropped = True
cul001.state = 0
bb1.IsDropped = True:bb001.IsDropped = True:bb002.IsDropped = True:bb003.IsDropped = True:bb004.IsDropped = True
sul1.state = 0:sul2.state = 0:sul3.state = 0:sul001.state = 0:sul5.state = 0
PLagueStatic.IsDropped = True:prise001.IsDropped = True:prise002.IsDropped = True:prise003.IsDropped = True:prise004.IsDropped = True:prise005.IsDropped = True:prise006.IsDropped = True:prise007.IsDropped = True:prise008.IsDropped = True
prise009.IsDropped = True:prise010.IsDropped = True:PLagueT1.IsDropped = True
plag1.state = 0
plag2.state = 0
plul1.state = 0
FinalPLTarget001.IsDropped = True
FPTul001.state = 0
NosTarget001.IsDropped = True
Nosul001.state = 0
progR.state = 0:progL.state = 0
Light005R.state = 0:Light005L.state = 0:Light005.state = 0
gdr.state = 0:gdl.state = 0
'-----States------
BloodisLife = False:NosReady = False:DropTargetsUp = True:FlatPTargetHitEnabled = True:KickerJacks = False:PlagueReady = False:bNOSFER = False:bbilMB = False:bMultiBallMode = False:bAutoPlunger = False
PlagueModeActive = false:FlatETargetHitEnabled = True:PlaHitCount = 0:BallSaveDoors = False
End Sub

Sub EndOfBall()
    Dim TotalBonus
    ToTalBonus = 0
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False
    'GiOff
    ' only process any of this if the table is not tilted.
    '(the tilt recovery mechanism will handle any extra balls or end of game)
    StopSong
    PlaySound "s_end of ball"
    DMDFlush
    DMD CL("GREAT DEATH"), CL("...AND GOODBYE"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
    'If NOT Tilted Then
        'Add the bonus
        'DMD CL("AFTERLIFE BONUS"), CL(BonusPoints(CurrentPlayer) ), "", eNone, eNone, eNone, 1500, True, "s_AM_collectBonus"
        'ToTalBonus = TotalBonus + BonusPoints(CurrentPlayer)

        'DMD CL("LOOP BONUS"), "     2000 X " &LoopCount, "", eNone, eNone, eNone, 1500, True, "s_AM_collectBonus"
        'ToTalBonus = TotalBonus + 2000 * LoopCount

        'DMD CL("COMBO BONUS"), "     2000 X " & ComboCount, "", eNone, eNone, eNone, 1500, True, "s_AM_collectBonus"
        'ToTalBonus = TotalBonus + 2000 * ComboCount

        'DMD CL("TOTAL BONUS"), CL(TotalBonus), "", eNone, eBlink, eNone, 2000, True, "s_AM_collectBonus"
        'Score(CurrentPlayer) = Score(CurrentPlayer) + TotalBonus

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 4600, "EndOfBall2 '"
PlaySound "endofballsfx"
DropNosferatu
    'Else 'if tilted Then only add a short delay and move to the 2nd part of the end of the ball
        'vpmtimer.addtimer 100, "EndOfBall2 '"
NextBallReset   
'End If
If bNOSFER = True Then
StopNOS
StopSong
vpmtimer.addtimer 4600, ""
End If
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, Then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    Tilt = 0
    'DisableTable False 'enable again bumpers and slingshots

    ' Check for the last ball and the Afterlife extra Ball
    'If BallsRemaining(CurrentPlayer) = 1 And bAfterLifeBallAwarded = False AND BonusPoints(CurrentPlayer) >= 6000 Then
        'bAfterLifeBallAwarded = True
        'ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
    'End If
    ' has the player won an extra-ball ? (might be multiple outstanding)
    If ExtraBallsAwards(CurrentPlayer)> 0 Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's Then turn off any Extra Ball light if there was any
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
        ' LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        If bAfterLifeBallAwarded Then
            bAfterLifeBallAwarded = false
            DMD CL("4TH BALL-AFTERLIFE"), CL("SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, "s_4th ball afterlife"
        Else
            DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, ""
        End If

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
            ' if multiple players are playing Then move onto the next one
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
        ' Then move to the next player
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
                Case 1:DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, ""
                Case 2:DMD "", CL("PLAYER 2"), "", eNone, eNone, eNone, 1000, True, ""
                Case 3:DMD "", CL("PLAYER 3"), "", eNone, eNone, eNone, 1000, True, ""
                Case 4:DMD "", CL("PLAYER 4"), "", eNone, eNone, eNone, 1000, True, ""
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
    bGameInPLay = False
    ' just ended your game Then play the end of game tune
    PlaySound "s_GameOver"
    ' ChangeSong
    ' ensure that the flippers are down
    LeftFlipper.RotateToStart
    RightFlipper.RotateToStart

    ' terminate all Mode - eject locked balls
    'If LockedBalls Then
        'EjectRightHole
        'EjectTopHole
    'End If
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    'GiOff
    StartAttractMode
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
' if only one Then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) Then kill of the ball but don't create
' a new one
'
Dim StopMBmodes
Sub Drain_Hit()
    ' Destroy the ball
    Drain.DestroyBall
    ' Exit Sub ' only for debugging - this way you can add balls from the debug window
PlayfieldMultiplier(CurrentPlayer) = 1
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
If bbilMB = True Then
PlaySound "mbended"
bbilMB = False
    DMDFlush
    DMD CL("BLOOD IS LIFE"), CL("MULTIBALL ENDED"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
bAutoPlunger = False
End If
If bNOSFER = True Then
PlaySound "nosmballloss"
    DMDFlush
    DMD CL("MULTIBALL LOST"), CL("BUT KEEP SHOOTING"), "_", eNone, eBlinkFast, eNone, 1250, True, ""

End If
    If BallsOnPlayfield> 0 Then
        BallsOnPlayfield = BallsOnPlayfield - 1
    End If

    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "drain", Drain

    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball

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
            'If NOT bMultiBallMode Then
                'DMD "_", CL("ETERNAL LIFE"), "_", eNone, eBlinkfast, eNone, 2500, True, ""
                'BallSaverTimerExpired_Timer
            'End If
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield - lockedballs = 1) Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True) Then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    changesong
                    ' turn off any multiball specific lights
                    'ChangeGIIntensity 1
                    'stop any multiball modes of this game
                    StopMBmodes = True
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield - LockedBalls = 0) Then
                ' End Mode and timers
                StopSong
                'ChangeGIIntensity 1
                UpdateBallInPlay
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
                'StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, if there is no animation Then move to the end of ball
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
    ' be sure to update the Scoreboard after the animations, if any
    ' if the ball goes into the plunger lane during a multiball Then activate the autoplunger
    If bMultiBallMode Then
        bAutoPlunger = True ' kick the ball in play if the bAutoPlunger flag is on
    End If
    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        vpmtimer.addtimer 1750, "PlungerIM.AutoFire:DOF 120, DOFPulse:PlaySoundAt ""fx_kicker"", swPlungerRest:bAutoPlunger = False '"
    End If
    'Start the skillshot lights & variables if any
    ChangeSong
    'If bSkillShotReady Then
        'UpdateSkillshot()
        ' show the message to shoot the ball in case the player has fallen sleep
        'swPlungerRest.TimerEnabled = 1
    'End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    'LightEffect 6
    bBallInPlungerLane = False
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    'If bSkillShotReady Then
        'ChangeSong
        'ResetSkillShotTimer.Enabled = 1
    'End If
    ' if there is a need for a ball saver, Then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
    End If
' turn off LaunchLight
' LaunchLight.State = 0
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball after a while
Sub swPlungerRest_Timer
'PlaySound "wakeup" &RndNbr(6) 'there are only 3 sounds in the table, so it will play a sound about 50% of times
End Sub

Sub EnableBallSaver(seconds)
    ' do not start the timer if extra ball has been awarded
    'If ExtraBallsAwards'(CurrentPlayer) > 0 Then
    '    BallSaverTimerExpired.Enabled = False
    '    BallSaverSpeedUpTimer.Enabled = False
    '    LightShootAgain.State = 1
    '    Exit Sub
    'End If
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimerExpired.Interval = 1000 * seconds
    BallSaverTimerExpired.Enabled = False
    BallSaverTimerExpired.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = False
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    'LightShootAgain.BlinkInterval = 160
    'LightShootAgain.State = 0
    'LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimerExpired_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimerExpired.Enabled = False
    BallSaverSpeedUpTimer.Enabled = False 'ensure this timer is also stopped
    ' clear the flag after 1.5 seconds of grace period
    vpmTimer.AddTimer 1500, "bBallSaverActive = False '"
    ' if you have a ball saver light Then turn it off at this point
    'LightShootAgain.State = 0
' if the table uses the same lights for the extra ball or replay Then turn them on if needed
' If ExtraBallsAwards(CurrentPlayer) > 0 Then
' LightShootAgain.State = 1
' End If
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    'LightShootAgain.BlinkInterval = 80
    'LightShootAgain.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

Sub CheckMultiplier()
If butl1.state = 1 Then PlayfieldMultiplier(CurrentPlayer) = 2 End If
	If butl2.state = 1 Then PlayfieldMultiplier(CurrentPlayer) = 3 End If
	If butl3.state = 1 Then PlayfieldMultiplier(CurrentPlayer) = 4 End If
	If butl4.state = 1 Then PlayfieldMultiplier(CurrentPlayer) = 5 End If
	If butl5.state = 1 Then PlayfieldMultiplier(CurrentPlayer) = 6 End If
	If butl6.state = 1 Then PlayfieldMultiplier(CurrentPlayer) = 7 End If
If butl1.state = 1 and butl2.state = 0 and butl3.state = 0 and butl4.state = 0 and butl5.state = 0 and butl6.state = 0 Then PlayfieldMultiplier(CurrentPlayer) = 1 End If
'If LastSwitchHit = "Spinner001" Then PlayfieldMultiplier(CurrentPlayer) = 1 End If
'If LastSwitchHit = "SpinTrigger001" Then PlayfieldMultiplier(CurrentPlayer) = 1 End If
'If LastSwitchHit = "Drain" Then PlayfieldMultiplier(CurrentPlayer) = 1 End If
'If LastSwitchHit = "RightInlane" Then PlayfieldMultiplier(CurrentPlayer) = 1 End If
'If LastSwitchHit = "LeftInlane" Then PlayfieldMultiplier(CurrentPlayer) = 1 End If
End Sub


' Add points to the score AND update the score board

Sub AddScore(points) 'normal score routine; points x playfieldmultiplier
    If Tilted Then Exit Sub
    'If bSkillshotReady AND(Score(CurrentPlayer)> 0) Then ResetSkillShotTimer_Timer
    ' add the points to the current players score variable

    Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
' you may wish to check to see if the player has gotten a replay
End Sub

Sub AddBonus(points)
    If Tilted Then Exit Sub
    ' add the bonus to the current players bonus variable
    BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
End Sub

' Set the Playfield Multiplier to the specified level AND set any lights accordingly

Sub SetPlayfieldMultiplier(Level)
    ' Set the multiplier to the specified level
    'PlayfieldMultiplier(CurrentPlayer) = Level
    UpdatePFXLights(Level)
End Sub

Sub UpdatePFXLights(Level)
    ' Update the playfield multiplier lights
    Select Case Level
        Case 2:butl1.state = 1:butl2.state = 0:butl3.state = 0:butl4.state = 0:butl5.state = 0:butl6.state = 0
        Case 3:butl1.state = 0:butl2.state = 1:butl3.state = 0:butl4.state = 0:butl5.state = 0:butl6.state = 0
        Case 4:butl1.state = 0:butl2.state = 0:butl3.state = 1:butl4.state = 0:butl5.state = 0:butl6.state = 0
        Case 5:butl1.state = 0:butl2.state = 0:butl3.state = 0:butl4.state = 1:butl5.state = 0:butl6.state = 0
        Case 6:butl1.state = 0:butl2.state = 0:butl3.state = 0:butl4.state = 0:butl5.state = 1:butl6.state = 0
        Case 7:butl1.state = 0:butl2.state = 0:butl3.state = 0:butl4.state = 0:butl5.state = 0:butl6.state = 1
    End Select
' perhaps show also the multiplier in the DMD?
End Sub

'Sub AwardExtraBall()
    '   If NOT bExtraBallWonThisBall Then 'uncomment this If in case you want to give just one extra ball per ball
    'DMD "_", " EXTRA BALL AWARDED", "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    'DOF 121, DOFPulse
    'ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
    '    bExtraBallWonThisBall = True
    'LightShootAgain.State = 1 'light the shoot again lamp
    'GiEffect 3
    'LightEffect 2
'    END If
'End Sub

'Sub AwardSkillshot(points)
    'ResetSkillShotTimer_Timer
    'show dmd animation
    'DMD CL("SKILLSHOT"), CL(FormatScore(points) ), "d_border", eNone, eBlinkFast, eNone, 2000, True, "s_AM_SkillShot"
    'DOF 127, DOFPulse
    'AddScore points
    'do some light show
    'GiEffect 3
    'LightEffect 2
'End Sub

'Sub AwardSuperSkillshot(points)
    'ResetSkillShotTimer_Timer
    'show dmd animation
    'DMD CL("SUPER SKILLSHOT"), CL(FormatScore(points) ), "d_border", eNone, eBlinkFast, eNone, 2000, True, "s_SuperSKillShot"
    'DOF 127, DOFPulse
    'AddScore points
    'do some light show
    'GiEffect 3
    'LightEffect 2
'End Sub

'Sub AwardSuperJackpot
    'show dmd animation
    'DMD CL("SUPER JACKPOT"), CL("500.000"), "d_border", eNone, eBlinkFast, eNone, 2000, True, "s_SuperJackpot"
    'DOF 127, DOFPulse
    'AddScore 500000
    'do some light show
    'GiEffect 3
    'LightEffect 2
'End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Dim MyTable
MyTable = "Nosferatu"

Sub Loadhs
    Dim x
    x = LoadValue(MyTable, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 5000000 End If
    x = LoadValue(MyTable, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(MyTable, "HighScore2")
    If(x <> "") Then HighScore(1) = CDbl(x) Else HighScore(1) = 2500000 End If
    x = LoadValue(MyTable, "HighScore2Name")
    If(x <> "") Then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(MyTable, "HighScore3")
    If(x <> "") Then HighScore(2) = CDbl(x) Else HighScore(2) = 1000000 End If
    x = LoadValue(MyTable, "HighScore3Name")
    If(x <> "") Then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(MyTable, "HighScore4")
    If(x <> "") Then HighScore(3) = CDbl(x) Else HighScore(3) = 500000 End If
    x = LoadValue(MyTable, "HighScore4Name")
    If(x <> "") Then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(MyTable, "Credits")
    If(x <> "") Then Credits = CInt(x) Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(MyTable, "TotalGamesPlayed")
    If(x <> "") Then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
    SortHighscore 'to be sure they are in the right order.
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
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    'PlaySound ""
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
        if(hsCurrentLetter = 0) Then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters) ) Then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") Then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) Then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit> 0) Then
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
    if(hsCurrentDigit> 0) Then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) Then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) Then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) Then
        if(hsLetterFlash <> 0) Then
            TempBotStr = TempBotStr & "_"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) Then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) Then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    dLine(1) = ExpandLine(TempBotStr)
    DMDUpdate 1
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2) Then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False

    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") Then
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
    savehs 'save the highscore in case the table is forced quit or it crashes.
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

Sub NextLUT:LUTImage = (LUTImage + 1) MOD 22:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

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
    'LUBack.imagea = "PostItNote"
    String = CLP(String)
    For xFor = 1 to 40
        'Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
    SetLUTLine ""
    'LUBack.imagea = "PostitBL"
End Sub

Function CLP(NumString) 'center line postit
    Dim Temp, TempStr
    If Len(NumString)> 40 Then NumString = Left(NumString, 40)
    Temp = (40 - Len(NumString) ) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CLP = TempStr
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
        tmp2 = "d_border"
    'info on the second line: tmp1
    'Select Case Mode(CurrentPlayer, 0)
    'Case 0 'no Mode active
    'Case 1
    '    tmp = FL("PLAYER " &CurrentPlayer, FormatScore(Score(Currentplayer)))
    '    tmp1 = CL("SPINNERS LEFT " & SpinsToCount1-SpinCount1)
    'End Select
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
                        If i = 2 Then
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
                        If i = 2 Then
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
        if IsNumeric(mid(NumString, i, 1) ) Then
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
    'Chars(33) = ""        '!
    'Chars(34) = ""        '"
    'Chars(35) = ""        '#
    'Chars(36) = ""        '$
    'Chars(37) = ""        '%
    'Chars(38) = ""        '&
    'Chars(39) = ""        ''
    'Chars(40) = ""        '(
    'Chars(41) = ""        ')
    Chars(42) = "d_star" '*
    Chars(43) = "d_plus" '+
    'Chars(44) = ""        '
    Chars(45) = "d_minus" '-
    Chars(46) = "d_dot"   '.
    'Chars(47) = ""        '/
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
    'Chars(61) = ""        '=
    Chars(62) = "d_more" '>
    'Chars(64) = ""        '@
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
    Chars(95) = "d_under" '_
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

'********************************************************************************************
' FlashForMs VPX8 will blink light for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Blink, -1 keep the original state
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version
    If FinalState = -1 Then
        FinalState = MyLight.State                            'Keep the current light state
    End If
    MyLight.BlinkInterval = BlinkPeriod
    MyLight.Duration 2, TotalPeriod, FinalState
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
            If rGreen> 255 Then
                rGreen = 255
                RGBStep = 1
            End If
        Case 1 'Red
            rRed = rRed - RGBFactor
            If rRed <0 Then
                rRed = 0
                RGBStep = 2
            End If
        Case 2 'Blue
            rBlue = rBlue + RGBFactor
            If rBlue> 255 Then
                rBlue = 255
                RGBStep = 3
            End If
        Case 3 'Green
            rGreen = rGreen - RGBFactor
            If rGreen <0 Then
                rGreen = 0
                RGBStep = 4
            End If
        Case 4 'Red
            rRed = rRed + RGBFactor
            If rRed> 255 Then
                rRed = 255
                RGBStep = 5
            End If
        Case 5 'Blue
            rBlue = rBlue - RGBFactor
            If rBlue <0 Then
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
        DMD "", CL("FREE PLAY ONLY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits> 0 Then
            DMD CL("CREDITS " & Credits), CL("PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD CL("BASED ON THE NOVEL"), CL("DRACULA"), "", eNone, eNone, eNone, 3000, False, ""
    DMD CL("BY"), CL("BRAM STOKER"), "", eScrollLeft, eScrollLeft, eNone, 2300, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 5000, False, ""
    DMD "", CL("ROM VERSION " &myversion), "", eNone, eNone, eNone, 1520, False, ""
    DMD CL("HIGHSCORES"), Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL("HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1380, False, ""
    DMD CL("HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    StartLightSeq
    DMDFlush
    ShowTableInfo
    PlaySong "music_attract"
End Sub

Sub StopAttractMode
    DMDScoreNow
    StartLightSeqOff
End Sub

Sub StartLightSeq()
    'lights sequences
lb1.state = 2:lb2.state = 2:lb3.state = 2:lb4.state = 2:lb5.state = 2:lb6.state = 2:lb7.state = 2:lb8.state = 2
rats1.State = 2:rats2.State = 2:rats3.State = 2:rats4.State = 2
deadsl001.state = 2:plag2.state = 2:Light003.state = 2:PlagueCenterLight.state = 2
EHKO1.state = 2:EHKO2.state = 2:EHKO3.state = 2:EHKO4.state = 2
PLx1.state = 2:PLx2.state = 2:PLx3.state = 2
Light002.state = 0
gdr.duration 2, 1500, 0:gdl.duration 2, 1500, 0
DropNosferatu
End Sub

Sub StartLightSeqOff()
lb1.state = 0:lb2.state = 0:lb3.state = 0:lb4.state = 0:lb5.state = 0:lb6.state = 0:lb7.state = 0:lb8.state = 0
rats1.State = 0:rats2.State = 0:rats3.State = 0:rats4.State = 0
deadsl001.state = 0:plag2.state = 0:Light003.state = 0:PlagueCenterLight.state = 0
sqL1.state = 0:sqL2.state = 0:sqL3.state = 0:sqL4.state = 0:sqL5.state = 0:sqL6.state = 0:sqL7.state = 0
EHKO1.state = 0:EHKO2.state = 0:EHKO3.state = 0:EHKO4.state = 0
PLx1.state = 0:PLx2.state = 0:PLx3.state = 0
Light002.state = 2
'blood is life test if needed'
'fl1b.state = 1:fl2l.state = 1:fl3o.state = 1:fl4o.state = 1:fl5d.state = 1:fl6i.state = 1:fl7s.state = 1:fl8l.state = 1:fl9i.state = 1:fl10f.state = 1:fl11e.state = 1
'nos test if needed'
'Enos.state = 1:Target1N.IsDropped = True:Target2O.IsDropped = True:Target3S.IsDropped = True:Target4F.IsDropped = True:Target5R.IsDropped = True:Target6A.IsDropped = True:Target7T.IsDropped = True:Target8U.IsDropped = True
Target1N.IsDropped = False:Target2O.IsDropped = False:Target3S.IsDropped = False:Target4F.IsDropped = False:Target5R.IsDropped = False:Target6A.IsDropped = False:Target7T.IsDropped = False:Target8U.IsDropped = False
saucl1.state = 0:saucl2.state = 0:saucl3.state = 0
Enos.state = 0:Nnos.state = 0:Onos.state = 0:Snos.state = 0:Fnos.state = 0:Rnos.state = 0:Anos.state = 0:Tnos.state = 0:Unos.state = 0
BILl001.state = 0:BILl002.state = 0:kickL1.state = 0:kickL2.state = 0:kickL3.state = 0
fl1b.state = 0:fl2l.state = 0:fl3o.state = 0:fl4o.state = 0:fl5d.state = 0:fl6i.state = 0:fl7s.state = 0:fl8l.state = 0:fl9i.state = 0:fl10f.state = 0:fl11e.state = 0
PegLight.state = 0:PegLight001.state = 0:PegLight002.state = 0:PegLight003.state = 0:PegLight004.state = 0:PegLight005.state = 0:PegLight006.state = 0
PegLight007.state = 0:PegLight008.state = 0:PegLight009.state = 0:PegLight010.state = 0
ThrallO1.state = 0: orl1.state = 0: orl2.state = 0: orl3.state = 0: orl4.state = 0: orl5.state = 0: orl6.state = 0
ThrallE1.state = 0: ell1.state = 0: ell2.state = 0: ell3.state = 0: ell4.state = 0: ell5.state = 0
ThrallH1001.state = 0: hut1.state = 0: hut2.state = 0: hut3.state = 0: hut4.state = 0: hut5.state = 0: hut6.state = 0
ThrallK1.state = 0: kno1.state = 0: kno2.state = 0: kno3.state = 0: kno4.state = 0: kno5.state = 0
EYELightR.state = 0
EYELightL.state = 0
LCRL.state = 2
LskL.state = 0
WWTarget001.IsDropped = True
wul001.state = 0
DoorLeft.IsDropped = False
DoorLeft001.IsDropped = True
PlaySoundAt "fx_droptargetreset", DoorLeft
sealite1.state = 0
sealite2.state = 0
sealite3.state = 0
DoorRight.IsDropped = False
DoorRight001.IsDropped = True
PlaySoundAt "fx_droptargetreset", DoorRight
RCRL.state = 2
RskL.state = 0
CMTarget001.IsDropped = True
cul001.state = 0
rul1.state = 0:rul2.state = 0:rul3.state = 0:rul4.state = 0:rul5.state = 0
ratt1.IsDropped = True:ratt2.IsDropped = True:ratt3.IsDropped = True:ratt4.IsDropped = True:ratt5.IsDropped = True
bb1.IsDropped = True:bb001.IsDropped = True:bb002.IsDropped = True:bb003.IsDropped = True:bb004.IsDropped = True
sul1.state = 0:sul2.state = 0:sul3.state = 0:sul001.state = 0:sul5.state = 0
ILBLight1.state = 0:ILBLight2.state = 0
FlatPL.State = 0:FlatLL001.State = 0:FlatAL.State = 0:FlatGL.State = 0:FlatUL001.State = 0:FlatEL.State = 0 
PLagueStatic.IsDropped = True:prise001.IsDropped = True:prise002.IsDropped = True:prise003.IsDropped = True:prise004.IsDropped = True:prise005.IsDropped = True:prise006.IsDropped = True:prise007.IsDropped = True:prise008.IsDropped = True
prise009.IsDropped = True:prise010.IsDropped = True:PLagueT1.IsDropped = True
plag1.state = 0
plag2.state = 0
plul1.state = 0
FinalPLTarget001.IsDropped = True
FPTul001.state = 0
NosTarget001.IsDropped = True
Nosul001.state = 0
progR.state = 0:progL.state = 0
Light005R.state = 0:Light005L.state = 0:Light005.state = 0
gdr.state = 0:gdl.state = 0
DropNosferatu
'-----States------
BloodisLife = False:NosReady = False:RHitCount = 0:DropTargetsUp = True:FlatPTargetHitEnabled = True:KickerJacks = False:PlagueReady = False:bNOSFER = False:bbilMB = False:bMultiBallMode = False:bAutoPlunger = False
PlagueModeActive = false:FlatETargetHitEnabled = True:PlaHitCount = 0:BallSaveDoors = False
CurrentLightEll = 1
TotalLightsEll = 5
CurrentLightHut = 1
TotalLightsHut = 6
CurrentLightKno = 1
TotalLightsKno = 5
CurrentLightOrl = 1
TotalLightsOrl = 6
PLightCounter = 1
NosHitCount = 0
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


dim EHKO

'----------------------game codes--------------------

' PLAY MOVIES

Dim MStep:    MStep = 0 'frame number currently playing
Dim MFrames:  MFrames = 0 'number of movie frames
Dim MName:   MName = "" 'name of the movie

'Movies
Dim movcar: movcar = Array("movcar1","movcar2","movcar3","movcar4","movcar5","movcar6","movcar7","movcar8","movcar9","movcar10") 
Dim movell: movell = Array("movell1","movell2","movell3","movell4","movell5","movell6","movell7","movell8","movell9","movell10")
Dim movhut: movhut = Array("movhut1","movhut2","movhut3","movhut4","movhut5","movhut6","movhut7","movhut8","movhut9","movhut10") 
Dim movkno: movkno = Array("movkno1","movkno2","movkno3","movkno4","movkno5","movkno6","movkno7","movkno8","movkno9","movkno10")
Dim movwer: movwer = Array("movwer1","movwer2","movwer3","movwer4","movwer5","movwer6","movwer7","movwer8","movwer9","movwer10") 
Dim movorl: movorl = Array("movorl1","movorl2","movorl3","movorl4","movorl5","movorl6","movorl7","movorl8","movorl9","movorl10")
Dim movsai: movsai = Array("movsai1","movsai2","movsai3","movsai4","movsai5","movsai6","movsai7","movsai8","movsai9","movsai10") 
Dim movemp: movemp = Array("movemp1","movemp2","movemp3","movemp4","movemp5","movemp6","movemp7","movemp8","movemp9","movemp10")
Dim movnos: movnos = Array("movnos1","movnos2","movnos3","movnos4","movnos5","movnos6","movnos7","movnos8","movnos9","movnos10") 
Dim movsun: movsun = Array("movsun1","movsun2","movsun3","movsun4","movsun5","movsun6","movsun7","movsun8","movsun9","movsun10")
Dim movpla: movpla = Array("movpla1","movpla2","movpla3","movpla4","movpla5","movpla6","movpla7","movpla8","movpla9","movpla10") 
Dim movrat: movrat = Array("movrat1","movrat2","movrat3","movrat4","movrat5","movrat6","movrat7","movrat8","movrat9","movrat10")
Dim movbil: movbil = Array("movbil1","movbil2","movbil3","movbil4","movbil5","movbil6","movbil7","movbil8","movbil9","movbil10") 
Dim movsuk: movsuk = Array("movsuk1","movsuk2","movsuk3","movsuk4","movsuk5","movsuk6","movsuk7","movsuk8","movsuk9","movsuk10")


Sub PlayMovieTimer_Timer
Dim Temp
Temp = MName(MStep)
MovieF.ImageA = Temp
MStep = MStep + 1
If MStep > MFrames Then StopMovie
End Sub

Sub PlayMovie(movname)
MStep = 1
Mname = movname
MFrames = UBound(movname)
PlayMovieTimer.Enabled = 1
MovieF.ImageA = MName(0)
MovieF.Visible = 1
End Sub

Sub StopMovie
MovieF.Visible = 0
PlayMovieTimer.Enabled = 0
End Sub

Dim bbilMB
Dim BloodisLife
Dim NosReady

'Initialize BloodisLife state?
BloodisLife = False

'Initialize NosReady state?
NosReady = False

Sub kicker001_Hit()
CheckBil
If BloodisLife = True Then
bbilMB = True:AddMultiball 1:
    bAutoPlunger = True
Light004.duration 2,2000, 0
    CheckMultiplier  
    AddScore 100000
    DMDFlush
    DMD CL("BLOOD IS LIFE"), CL("MULTIBALL"), "_", eNone, eBlinkFast, eNone, 3000, True, ""
sqL5.state = 2
    kicker001.TimerEnabled = 1
    PlaySoundAt "UpperKickerEnter", kicker001
PlaySound "mbstarted"
    Enos.state = 1
    EYELightR.state = 1
    EYELightL.state = 1
    kickL1.duration 2, 1000, 0
BloodisLife = False
StopMBmodes = True
BILl001.state = 0:BILl002.state = 0
fl1b.state = 0:fl2l.state = 0:fl3o.state = 0:fl4o.state = 0:fl5d.state = 0:fl6i.state = 0:fl7s.state = 0:fl8l.state = 0:fl9i.state = 0:fl10f.state = 0:fl11e.state = 0
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movbil
End If
If KickerJacks = True Then
    CheckCHARB
    'CheckBil
    kicker001.TimerEnabled = 1
    PlaySoundAt "UpperKickerEnter", kicker001
    Enos.state = 1
    EYELightR.state = 1
    EYELightL.state = 1
    kickL1.duration 2, 1000, 0
Else
    CheckCHARB
    'CheckBil
    kicker001.TimerEnabled = 1
    PlaySoundAt "UpperKickerEnter", kicker001
    Enos.state = 1
    EYELightR.state = 1
    EYELightL.state = 1
CheckNosReady
    kickL1.duration 2, 1000, 0
End If
End Sub

Sub CheckNosReady ()
If Enos.state = 1 and Target1N.IsDropped = 1 and Target2O.IsDropped = 1 and Target3S.IsDropped = 1 and Target4F.IsDropped = 1 and Target5R.IsDropped = 1 and Target6A.IsDropped = 1 and Target7T.IsDropped = 1 and Target8U.IsDropped = 1 Then
NosReady = True
saucl2.state = 2
Enos.state = 2:Nnos.state = 2:Onos.state = 2:Snos.state = 2:Fnos.state = 2:Rnos.state = 2:Anos.state = 2:Tnos.state = 2:Unos.state = 2
End If
End Sub

Sub CheckCHARB()
    ' Check Character Bonus
    If ThrallK1.state = 1 Then
        ThrallK1.state = 0: kno1.state = 0: kno2.state = 0: kno3.state = 0: kno4.state = 0: kno5.state = 0: saucl1.state = 0
        CurrentLightKno = 1
        PlaySound "charb"
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movkno
     CheckMultiplier   
AddScore 10000
    DMDFlush
    DMD CL("CHARACTER BONUS"), CL("KNOCK  MULT X 10.000"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
    ElseIf ThrallH1001.state = 1 Then
        ThrallH1001.state = 0: hut1.state = 0: hut2.state = 0: hut3.state = 0: hut4.state = 0: hut5.state = 0: hut6.state = 0: saucl1.state = 0
        CurrentLightHut = 1
        PlaySound "charb"
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movhut
      CheckMultiplier  
AddScore 10000
    DMDFlush
    DMD CL("CHARACTER BONUS"), CL("HUTTER MULT X 10.000"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
    ElseIf ThrallE1.state = 1 Then
        ThrallE1.state = 0: ell1.state = 0: ell2.state = 0: ell3.state = 0: ell4.state = 0: ell5.state = 0: saucl1.state = 0
        CurrentLightEll = 1
        PlaySound "charb"
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movell
        CheckMultiplier
AddScore 10000
    DMDFlush
    DMD CL("CHARACTER BONUS"), CL("ELLEN  MULT X 10.000"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
    ElseIf ThrallO1.state = 1 Then
        ThrallO1.state = 0: orl1.state = 0: orl2.state = 0: orl3.state = 0: orl4.state = 0: orl5.state = 0: orl6.state = 0: saucl1.state = 0
        CurrentLightOrl = 1
        PlaySound "charb"
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movorl
        CheckMultiplier
AddScore 10000
    DMDFlush
    DMD CL("CHARACTER BONUS"), CL("ORLOCK MULT X 10.000"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
End If
    ' BLOOD IS LIFE COLLECT
    'ElseIf BloodisLife = True Then
        'fl1b.state = 0:fl2l.state = 0:fl3o.state = 0:fl4o.state = 0:fl5d.state = 0:fl6i.state = 0:fl7s.state = 0:fl8l.state = 0: fl9i.state = 0:fl10f.state = 0:fl11e.state = 0
        'sqL5.state = 1
        'PegLight.state = 0
        'PegLight001.state = 0
        'PegLight002.state = 0
        'PegLight003.state = 0
        'PegLight004.state = 0
        'PegLight005.state = 0
        'PegLight006.state = 0
        'PegLight007.state = 0
        'PegLight008.state = 0
        'PegLight009.state = 0
        'PegLight010.state = 0
        'BloodisLife = False
    'End If
End Sub

Sub CheckBil()
If fl1b.state = 1 and fl2l.state = 1 and fl3o.state = 1 and fl4o.state = 1 and fl5d.state = 1 and fl6i.state = 1 and fl7s.state = 1 and fl8l.state = 1 and fl9i.state = 1 and fl10f.state = 1 and fl11e.state = 1 Then
   ' If fl1b.state = 1 and fl2l.state = 1 and fl3o.state = 1 and fl4o.state = 1 and fl5d.state = 1 and fl6i.state = 1 and fl7s.state = 1 and fl8l.state = 1 and fl9i.state = 1 and fl10f.state = 1 and fl11e.state = 1 Then
BILl001.state = 2:BILl002.state = 2:kickL1.state = 2
       BloodisLife = True
Else
BloodisLife = False
   End If
End Sub

Dim KickerJacks ' Kickers 2 and 3 become jackpots now'
'initial state
KickerJacks = False

Sub kicker001_Timer
kicker001.Kick 135, 70
PlaySoundAt "fx_kicker" , kicker001
kicker001.TimerEnabled = 0
End Sub

Dim bNOSFER
'initial state
bNOSFER = False

Sub kicker002_Hit()
If NosReady = True Then 
bNOSFER = True
ChangeSong
NOSFERATU
AddMultiball 1
    bAutoPlunger = True
kickL2.duration 2,2250,0
kicker002.TimerEnabled = 1
PlaySoundAt "fx_kicker" , kicker002
saucl2.state = 0
NosReady = False
End If
If KickerJacks = True Then
CheckMultiplier
AddScore 30000
    DMDFlush
    DMD CL("NOSFERATU"), CL("KICKER JACKPOTS"), "_", eNone, eBlink, eNone, 1250, True, ""
kickL2.duration 2,1000,0
kicker002.TimerEnabled = 1
PlaySoundAt "fx_kicker" , kicker002
Else
    DMDFlush
    DMD CL("BLOOD CELL BONUS"), CL("COLLECTED"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
CheckMultiplier
If lb1.state = 2 Then AddScore 2500 End If
If lb2.state = 2 Then AddScore 2500 End If
If lb3.state = 2 Then AddScore 2500 End If
If lb4.state = 2 Then AddScore 2500 End If
kicker002.TimerEnabled = 1
PlaySoundAt "BloodKickerEnter" , kicker002
PlaySoundAt "Score500" , kicker002
PlaySoundAt "MotorLeer" , kicker002
DoorLeft.IsDropped = False
PlaySoundAt "fx_droptargetreset", DoorLeft
LCRL.state = 2
LskL.state = 0
Target1N.IsDropped = False
Target2O.IsDropped = False
Target3S.IsDropped = False
Target4F.IsDropped = False
WWTarget001.IsDropped = True
wul001.state = 0
PegLight.state = 0
PegLight001.state = 0
PegLight002.state = 0
PegLight003.state = 0
PegLight004.state = 0
PegLight005.state = 0
PegLight006.state = 0
PegLight007.state = 0
PegLight008.state = 0
PegLight009.state = 0
PegLight010.state = 0
fl1b.state = 0:fl2l.state = 0:fl3o.state = 0:fl4o.state = 0:fl5d.state = 0:fl6i.state = 0:fl7s.state = 0:fl8l.state = 0:fl9i.state = 0:fl10f.state = 0:fl11e.state = 0
Nnos.state = 0
Onos.state = 0
Snos.state = 0
Fnos.state = 0
Enos.state = 0
EYELightR.state = 0
EYELightL.state = 0
PlaySoundAt "fx_droptarget", Target3S
lb1.state = 0
lb2.state = 0
lb3.state = 0
lb4.state = 0
sealite1.state = 0
sealite2.state = 0
sealite3.state = 0
deadsl001.state = 0
kickL2.duration 2,1000,0
BloodisLife = False
BILl001.state = 0
BILl002.state = 0
kickL1.state = 0
saucl2.state = 0
NosReady = False
End If
End Sub

Sub kicker002_Timer
kicker002.Kick 184, 10
PlaySoundAt "popper_ball" , kicker002
kicker002.TimerEnabled = 0
End Sub

'create dim for plague ready
dim PlagueReady
' set initial state to false and plague lights to off
PlagueReady = false
FlatPL.State = 0
FlatLL001.State = 0
FlatAL.State = 0
FlatGL.State = 0
FlatUL001.State = 0
FlatEL.State = 0


'Sub PLAG10:FlatPL.State = 2 End Sub
'Sub PLAG20:FlatLL001.State = 2 End Sub
'Sub PLAG3:FlatAL.State = 2 End Sub
'Sub PLAG4:FlatGL.State = 2 End Sub
'Sub PLAG5:FlatUL001.State = 2 End Sub
'Sub PLAG6:FlatEL.State = 2 End Sub

'create plague ready check'
Sub CheckPlagueReady
     If FlatPL.State = 2 and FlatLL001.State = 2 and FlatAL.State = 2 and FlatGL.State = 2 and FlatUL001.State = 2 and FlatEL.State = 2 Then
     PlagueReady = true
     saucl3.state = 2
     plag1.state = 1
     End If
End Sub

Sub kicker003_Hit()
If PlagueReady = true Then
PlagueMode
kickL3.duration 2,850,0
kicker003.TimerEnabled = 1
End If
If KickerJacks = True Then
CheckMultiplier
AddScore 30000
    DMDFlush
    DMD CL("NOSFERATU"), CL("KICKER JACKPOTS"), "_", eNone, eBlink, eNone, 1250, True, ""
kickL3.duration 2,1000,0
kicker003.TimerEnabled = 1
PlaySoundAt "fx_kicker" , kicker003
PlaySound "jackpot"
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movsuk
Else
    DMDFlush
    DMD CL("BLOOD CELL BONUS"), CL("COLLECTED"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
CheckMultiplier
If lb5.state = 2 Then AddScore 2500 End If
If lb6.state = 2 Then AddScore 2500 End If
If lb7.state = 2 Then AddScore 2500 End If
If lb8.state = 2 Then AddScore 2500 End If
kicker003.TimerEnabled = 1
PlaySoundAt "BloodKickerEnter" , kicker003
PlaySoundAt "Score500" , kicker003
PlaySoundAt "MotorLeer" , kicker003
DoorRight.IsDropped = False
PlaySoundAt "fx_droptargetreset", DoorRight
RCRL.state = 2
RskL.state = 0
Target5R.IsDropped = False
Target6A.IsDropped = False
Target7T.IsDropped = False
Target8U.IsDropped = False
CMTarget001.IsDropped = True
cul001.state = 0
WWTarget001.IsDropped = True
wul001.state = 0
PegLight.state = 0
PegLight001.state = 0
PegLight002.state = 0
PegLight003.state = 0
PegLight004.state = 0
PegLight005.state = 0
PegLight006.state = 0
PegLight007.state = 0
PegLight008.state = 0
PegLight009.state = 0
PegLight010.state = 0
fl1b.state = 0:fl2l.state = 0:fl3o.state = 0:fl4o.state = 0:fl5d.state = 0:fl6i.state = 0:fl7s.state = 0:fl8l.state = 0:fl9i.state = 0:fl10f.state = 0:fl11e.state = 0
Rnos.state = 0
Anos.state = 0
Tnos.state = 0
Unos.state = 0
Enos.state = 0
EYELightR.state = 0
EYELightL.state = 0
PlaySoundAt "fx_droptarget", Target6A
lb5.state = 0
lb6.state = 0
lb7.state = 0
lb8.state = 0
kickL3.duration 2,1000,0
BloodisLife = False
BILl001.state = 0
BILl002.state = 0
kickL1.state = 0
saucl2.state = 0
NosReady = False
End If
End Sub

Sub kicker003_Timer
kicker003.Kick 49, 50
PlaySoundAt "fx_kicker" , kicker003
kicker003.TimerEnabled = 0
End Sub

'flat targets PLAGUE lights'
' Define variables to keep track of the number of hits and drop targets
Dim RHitCount
Dim DropTargetsUp
Dim FlatPTargetHitEnabled ' Flag to enable/disable FlatPTarget_Hit

' Initialize variables
RHitCount = 0
DropTargetsUp = True
FlatPTargetHitEnabled = True ' Enable FlatPTarget_Hit initially

Sub FlatPTarget_Hit()

CheckPlagueReady
    If FlatPTargetHitEnabled And DropTargetsUp Then
        PlaySoundAt "target", FlatPTarget
        PlaySoundAt "Target_Hit_1", FlatPTarget
        FlatPL.State = 2
        RHitCount = RHitCount + 1 ' Increment the hit count
        
        ' Reset all lights and drop targets to their initial states
        rats1.State = 0
        rats2.State = 0
        rats3.State = 0
        rats4.State = 0
        
        ' Control lights based on the hit count
        If RHitCount = 1 Then
            rats1.State = 1
        CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("P    PLAGUE TARGET"), CL("R..."), "_", eNone, eBlinkFast, eNone, 1250, True, ""
        ElseIf RHitCount = 2 Then
            rats1.State = 1
            rats2.State = 1
            CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("P    PLAGUE TARGET"), CL("RA.."), "_", eNone, eBlinkFast, eNone, 1250, True, ""
        ElseIf RHitCount = 3 Then
            rats1.State = 1
            rats2.State = 1
            rats3.State = 1
            CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("P    PLAGUE TARGET"), CL("RAT."), "_", eNone, eBlinkFast, eNone, 1250, True, ""
        ElseIf RHitCount = 4 Then
            rats1.State = 1
            rats2.State = 1
            rats3.State = 1
            rats4.State = 1
        CheckMultiplier    
AddScore 20000
    DMDFlush
    DMD CL("P    PLAGUE TARGET"), CL("RATS"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
If KickerJacks = True Then
PlaySoundAt "flip_hit_3", FlatPTarget
        DoorLeft.IsDropped = False
DoorLeft001.IsDropped = True
        PlaySoundAt "fx_droptargetreset", DoorLeft
        LCRL.state = 2
        LskL.state = 0
DoorRight001.IsDropped = True   
        DoorRight.IsDropped = False
        PlaySoundAt "fx_droptargetreset", DoorRight
        RCRL.state = 2
        RskL.state = 0
        RATSUP
Else
        'outlane doors up
        DoorLeft.IsDropped = False
        PlaySoundAt "fx_droptargetreset", DoorLeft
        LCRL.state = 2
        LskL.state = 0
        DoorRight.IsDropped = False
        PlaySoundAt "fx_droptargetreset", DoorRight
        RCRL.state = 2
        RskL.state = 0
        RATSUP
        End If
    End If
End If
End Sub

Sub RATSUP()
    ' Bring up the drop targets
    ratt1.IsDropped = False: rul1.state = 2
    ratt2.IsDropped = False: rul2.state = 2
    ratt3.IsDropped = False: rul3.state = 2
    ratt4.IsDropped = False: rul4.state = 2
    ratt5.IsDropped = False: rul5.state = 2
    DropTargetsUp = False
    PlaySound "ratsup"
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movrat
    ' Disable FlatPTarget_Hit after RATSUP
    FlatPTargetHitEnabled = False
End Sub

Sub CheckResetRats()
    ' Check if all 5 drop targets are dropped
    If ratt1.IsDropped = 1 and ratt2.IsDropped = 1 and ratt3.IsDropped = 1 and ratt4.IsDropped = 1 and ratt5.IsDropped = 1 Then
        ' Reset the hit count
        RHitCount = 0
        rats1.State = 0
        rats2.State = 0
        rats3.State = 0
        rats4.State = 0
        DropTargetsUp = True ' Set DropTargetsUp to True after resetting
        
        ' Re-enable FlatPTarget_Hit and the mode
        FlatPTargetHitEnabled = True ' Enable FlatPTarget_Hit initially
'light scorecard for rats and make winning noise
        sqL4.state = 1
        PlaySound "MotorLeer"
        PlaySound "ratwin"
     CheckMultiplier   
AddScore 50000
    DMDFlush
    DMD CL("RATS BONUS"), CL("50.000"), "_", eNone, eBlink, eNone, 1500, True, ""
End If
End Sub

'Sub FlatPTarget_Hit()
'PlaySoundAt "target" , FlatPTarget
'PlaySoundAt "Target_Hit_1" , FlatPTarget
'rats1.State = 1:FlatPL.state = 2:rats2.State = 0:rats3.State = 0:rats4.State = 0
      'ratt1.IsDropped = False: ratt2.IsDropped = False: ratt3.IsDropped = False: ratt4.IsDropped = False: ratt5.IsDropped = False
   'rul1.state = 2
   'rul2.state = 2
   'rul3.state = 2
   'rul4.state = 2
  ' rul5.state = 2
  ' DoorLeft.IsDropped = False
  ' PlaySoundAt "fx_droptargetreset", DoorLeft
  ' LCRL.state = 2
 '  LskL.state = 0
  ' DoorRight.IsDropped = False
  ' PlaySoundAt "fx_droptargetreset", DoorRight
  ' RCRL.state = 2
  ' RskL.state = 0
  ' PlaySound "ratsup"
'End Sub

Sub ratt1_Hit()
ratt1.IsDropped = True: PlaySoundAt "fx_droptarget" , ratt1: PlaySoundAt "fx_droptarget" , ratt1: PlaySoundAt "rsquee" , ratt1
rul1.state = 0
  CheckMultiplier 
 AddScore 5000
    DMDFlush
    DMD CL("RAT DESTROYED"), CL("MULT X 5.000"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
CheckResetRats
End Sub

Sub ratt2_Hit()
ratt2.IsDropped = True: PlaySoundAt "fx_droptarget" , ratt2: PlaySoundAt "fx_droptarget" , ratt2: PlaySoundAt "rsquee" , ratt1
rul2.state = 0
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("RAT DESTROYED"), CL("MULT X 5.000"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
CheckResetRats
End Sub

Sub ratt3_Hit()
ratt3.IsDropped = True: PlaySoundAt "fx_droptarget" , ratt3: PlaySoundAt "fx_droptarget" , ratt3: PlaySoundAt "rsquee" , ratt1
rul3.state = 0
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("RAT DESTROYED"), CL("MULT X 5.000"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
CheckResetRats
End Sub

Sub ratt4_Hit()
ratt4.IsDropped = True: PlaySoundAt "fx_droptarget" , ratt4: PlaySoundAt "fx_droptarget" , ratt4: PlaySoundAt "rsquee" , ratt1
rul4.state = 0
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("RAT DESTROYED"), CL("MULT X 5.000"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
CheckResetRats
End Sub

Sub ratt5_Hit()
ratt5.IsDropped = True: PlaySoundAt "fx_droptarget" , ratt5: PlaySoundAt "fx_droptarget" , ratt5: PlaySoundAt "rsquee" , ratt1
rul5.state = 0
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("RAT DESTROYED"), CL("MULT X 5.000"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
CheckResetRats
End Sub

'Sub FlatPTarget_Hit(r)
'Check_RATS1
'End Sub

'Sub Check_RATS1()
'Select Case r
'Case 0:rats1.State = 0:rats2.State = 0:rats3.State = 0:rats4.State = 0:FlatPL.state = 0
        'Case 1:rats1.State = 1:rats2.State = 0:rats3.State = 0:rats4.State = 0:FlatPL.state = 2
       ' Case 2:rats1.State = 1:rats2.State = 1:rats3.State = 0:rats4.State = 0:FlatPL.state = 2
       ' Case 3:rats1.State = 1:rats2.State = 1:rats3.State = 1:rats4.State = 0:FlatPL.state = 2
       ' Case 4:rats1.State = 1:rats2.State = 1:rats3.State = 1:rats4.State = 1:FlatPL.state = 2
'End Select


'End Sub

Dim CMTarget001DroppedFlag ' Global flag variable

Sub FlatLTarget_Hit()
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("L    PLAGUE TARGET"), CL("YOUR CARRIAGE AWAITS"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
CheckPlagueReady
    FlatLL001.state = 2
    PlaySoundAt "target", FlatLTarget
    PlaySoundAt "Target_Hit_1", FlatLTarget
    If CMTarget001.IsDropped Then
        CMTarget001DroppedFlag = True ' Set the flag to True
        CMTarget001_Dropped
    End If
If KickerJacks = True Then
PlaySoundAt "flip_hit_3", FlatLTarget
Else
    DoorLeft.IsDropped = False
    PlaySoundAt "fx_droptargetreset", DoorLeft
    LCRL.state = 2
    LskL.state = 0
    DoorRight.IsDropped = False
    PlaySoundAt "fx_droptargetreset", DoorRight
    RCRL.state = 2
    RskL.state = 0
End If
End Sub

Sub CMTarget001_Dropped()
    If CMTarget001DroppedFlag Then ' Check the flag
        CMTarget001.IsDropped = 0
        PlaySoundAt "DropTarget_Up", CMTarget001
        PlaySound "horsup"
        cul001.state = 2
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movcar
        CMTarget001.TimerEnabled = True ' Enable the timer
        CMTarget001.TimerInterval = 15000 ' Set the timer interval to 15,000 milliseconds (15 seconds)

        ' Reset the flag after processing
        CMTarget001DroppedFlag = False
    End If
End Sub

Sub CMTarget001_Timer
    CMTarget001.IsDropped = 1
    cul001.state = 0
'had to remove sound of target going back down - can't explain why but would sound when table started???
    ' You don't need to reset the flag here because it should only be set when FlatLTarget_Hit() is called.

    CMTarget001.TimerEnabled = False ' Disable the timer after it runs once
End Sub


'PlaySoundAt "DropTarget_Up" , CMTarget001
'PlaySound "horsup"
'CMTarget001.IsDropped = False
'cul001.state = 2
'End Sub


Sub FlatATarget_Hit()
   CheckMultiplier 
AddScore 5000
    DMDFlush
    DMD CL("A    PLAGUE TARGET"), CL("DEAD FLOWERS 4 ELLEN"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
CheckPlagueReady
PlaySoundAt "target" , FlatATarget
PlaySoundAt "Target_Hit_1" , FlatATarget
FlatAL.state = 2
ILBLight1.state = 1
End Sub

Sub FlatGTarget_Hit()
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("G    PLAGUE TARGET"), CL("LAND OF SPECTRES"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
CheckPlagueReady
PlaySoundAt "target" , FlatGTarget
PlaySoundAt "Target_Hit_1" , FlatGTarget
FlatGL.state = 2
ILBLight2.state = 1
End Sub

Dim WWTarget001DroppedFlag ' Global flag variable

Sub FlatUTarget_Hit()
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("U    PLAGUE TARGET"), CL("WEREWOLF BEWARE"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
CheckPlagueReady
    FlatUL001.state = 2
    WolfLight004.duration 2,2300,0
    PlaySound "wgrowl"
    PlaySoundAt "target", FlatUTarget
    PlaySoundAt "Target_Hit_1", FlatUTarget
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movwer
    If WWTarget001.IsDropped Then
        WWTarget001DroppedFlag = True ' Set the flag to True
        WWTarget001_Dropped
    End If

End Sub

Sub WWTarget001_Dropped()
    If WWTarget001DroppedFlag Then ' Check the flag
        WWTarget001.IsDropped = 0
        PlaySoundAt "DropTarget_Up", WWTarget001
        wul001.state = 2
        WWTarget001.TimerEnabled = True ' Enable the timer
        WWTarget001.TimerInterval = 6000 ' Set the timer interval to 6,000 milliseconds (6 seconds)

        ' Reset the flag after processing
        WWTarget001DroppedFlag = False
    End If
End Sub

Sub WWTarget001_Timer
    WWTarget001.IsDropped = 1
    wul001.state = 0
'had to remove sound of target going back down - can't explain why but would sound when table started???
    ' You don't need to reset the flag here because it should only be set when FlatUTarget_Hit() is called.

    WWTarget001.TimerEnabled = False ' Disable the timer after it runs once
End Sub

dim FlatETargetHitEnabled

Sub FlatETarget_Hit()
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("E    PLAGUE TARGET"), CL("GALAZ TO WISBORG"), "_", eNone, eBlinkFast, eNone, 1250, True, ""
CheckPlagueReady
PlaySoundAt "target" , FlatETarget
PlaySoundAt "Target_Hit_1" , FlatETarget
FlatEL.state = 2
Light003.duration 2,750,0
If FlatETargetHitEnabled Then
sealite1.duration 2,1350,0
sealite2.duration 2,1350,0
sealite3.duration 2,1350,0
deadsl001.duration 1,13000,0 '13 seconds to get 5 dead sailors raised
PlaySound "sailorsready"
End If
End Sub
'plaguecircle'

dim PlagueModeActive 'create dim to tell plague trigger and possbly lights to only active in plague mode
'initial state
PlagueModeActive = false

Sub PlagueMode ()
PlagueReady = false
PlagueModeActive = true
PlaySound "plaguevoice"
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movpla
CheckMultiplier
AddScore 35000
    DMDFlush
    DMD CL("PLAGUE PLAGUE PLAGUE"), CL("SHOOT THE ORBIT"), "_", eNone, eBlink, eNone, 2000, True, ""
saucl3.state = 0
FlatPL.State = 0:FlatLL001.State = 0:FlatAL.State = 0:FlatGL.State = 0:FlatUL001.State = 0:FlatEL.State = 0 
PLagueStatic.IsDropped = False
    DoorLeft.IsDropped = False
DoorLeft001.IsDropped = True
    PlaySoundAt "fx_droptargetreset", DoorLeft
    LCRL.state = 2
    LskL.state = 0
    DoorRight.IsDropped = False
DoorRight001.IsDropped = True
    PlaySoundAt "fx_droptargetreset", DoorRight
    RCRL.state = 2
    RskL.state = 0
Target1N.IsDropped = False
Target2O.IsDropped = False
Target3S.IsDropped = False
Target4F.IsDropped = False
WWTarget001.IsDropped = True
wul001.state = 0
Nnos.state = 0
Onos.state = 0
Snos.state = 0
Fnos.state = 0
Enos.state = 0
EYELightR.state = 0
EYELightL.state = 0
PlaySoundAt "fx_droptarget", Target3S
lb1.state = 0
lb2.state = 0
lb3.state = 0
lb4.state = 0
sealite1.state = 0
sealite2.state = 0
sealite3.state = 0
deadsl001.state = 0
Target5R.IsDropped = False
Target6A.IsDropped = False
Target7T.IsDropped = False
Target8U.IsDropped = False
CMTarget001.IsDropped = True
cul001.state = 0
Rnos.state = 0
Anos.state = 0
Tnos.state = 0
Unos.state = 0
PlaySoundAt "fx_droptarget", Target6A
lb5.state = 0
lb6.state = 0
lb7.state = 0
lb8.state = 0

prise001.IsDropped = False
PlaySoundAt "whooshup", prise001


prise002.IsDropped = False



prise003.IsDropped = False



prise004.IsDropped = False
PlaySoundAt "fx_droptarget", prise004


prise005.IsDropped = False



prise006.IsDropped = False
PlaySoundAt "whooshup", prise006


prise007.IsDropped = False



prise008.IsDropped = False
PlaySoundAt "fx_droptarget", prise008


prise009.IsDropped = False



prise010.IsDropped = False
PlaySoundAt "whooshup", prise010
PlagueCenterLight.state = 1
End Sub
'plague trigger test'

Sub PlagueTrigger001_Hit ()
If PlagueModeActive = true Then
CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("PLAGUE PLAGUE PLAGUE"), CL("SHOOT THE TARGET"), "_", eBlink, eBlinkFast, eNone, 1500, True, ""
PLagueT1.IsDropped = False
PlaySoundAt "DropTarget_Up", PLagueT1
PlaySoundAt "plagtrigshov", PLagueT1
plag1.state = 1
plag2.state = 0
plul1.state = 2
End If
End Sub

Dim PlaHitCount ' hit count for PLagueT1_Hit
'initial state
PlaHitCount = 0

Sub PLagueT1_Hit ()
If PlagueModeActive = true Then
PlaHitCount = PlaHitCount + 1 ' Increment the hit count
If PlaHitCount = 1 Then
CheckMultiplier
AddScore 15000
    DMDFlush
    DMD CL("GREAT SHOT"), CL("SHOOT THE ORBIT"), "_", eBlink, eBlinkFast, eNone, 1500, True, ""
PLagueT1.IsDropped = True
PlaySoundAt "fx_droptarget", PLagueT1
PlaySoundAt "plaguecophit", PLagueT1
plag2.state = 2
plag1.state = 0
plul1.state = 0
PLx1.state =1

ElseIf PlaHitCount = 2 Then
CheckMultiplier
AddScore 15000
    DMDFlush
    DMD CL("GREAT SHOT"), CL("SHOOT THE ORBIT"), "_", eBlink, eBlinkFast, eNone, 1500, True, ""
PLagueT1.IsDropped = True
PlaySoundAt "fx_droptarget", PLagueT1
PlaySoundAt "plaguecophit", PLagueT1
plag2.state = 2
plag1.state = 0
plul1.state = 0
PLx1.state =1
PLx2.state =1

ElseIf PlaHitCount = 3 Then
CheckMultiplier
AddScore 15000
    DMDFlush
    DMD CL("GREAT SHOT"), CL("KILL THE VAMPYRE"), "_", eBlink, eNone, eNone, 2000, True, ""
PLagueT1.IsDropped = True
PlaySoundAt "fx_droptarget", PLagueT1
PlaySoundAt "plaguecophit", PLagueT1
plag2.state = 2
plag1.state = 0
plul1.state = 0
PLx1.state =1
PLx2.state =1
PLx3.state =1
PLagueStatic.IsDropped = True
PlaySound "plaguevoice"
prise001.IsDropped = True
PlaySoundAt "fx_droptarget", prise001
prise002.IsDropped = True
prise003.IsDropped = True
prise004.IsDropped = True
PlaySoundAt "Drop_Target_Down_2" , prise004
prise005.IsDropped = True
prise006.IsDropped = True
prise007.IsDropped = True
prise008.IsDropped = True
PlaySoundAt "Drop_Target_Down_2" , prise008
prise009.IsDropped = True
prise010.IsDropped = True
PlagueCenterLight.state = 0
PlagueModeActive = false
FinalPLTarget001.IsDropped = False
FPTul001.state = 2
End If
End If
End Sub

Sub FinalPLTarget001_Hit () ' plague mode last target
CheckMultiplier
AddScore 75000
    DMDFlush
    DMD CL("PLAGUE BONUS"), CL("MODE COMPLETED"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
FinalPLTarget001.IsDropped = False
FPTul001.state = 0
plag2.state = 0
PLx1.state =0
PLx2.state =0
PLx3.state =0
PlaHitCount = 0
sqL6.state = 1
PlaySoundAt "Drop_Target_Down_2" , FinalPLTarget001
PlaySound "MotorLeer"
PlaySound "plagwin"
End Sub

'ball color'
Sub ChangeBallImage()
    Dim BOT, b
    BOT = GetBalls
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
 
' change the ball color
bRedBall = NOT bRedBall
    ' change the image for each ball
    For b = 0 to UBound(BOT)
        If bRedBall Then 
            BOT(b).FrontDecal = "NosBall2"
        Else
            BOT(b).FrontDecal = "NosBall2"
        End If
    Next
End Sub

'BLOOD active peg test'

'B'
Sub PegB001_Hit()
PlaySoundAt "rubber_hit_1" , PegB001
PegLight.state = 2
fl1b.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub PegB002_Hit()
PlaySoundAt "rubber_hit_2" , PegB002
PegLight.state = 2
fl1b.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub PegB003_Hit()
PlaySoundAt "rubber_hit_2" , PegB003
PegLight.state = 2
fl1b.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub PegB004_Hit()
PlaySoundAt "rubber_hit_3" , PegB004
PegLight.state = 2
fl1b.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

'L'
Sub PegL001_Hit()
PlaySoundAt "rubber_hit_2" , PegL001
PegLight001.state = 2
fl2l.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub PegL002_Hit()
PlaySoundAt "rubber_hit_3" , PegL002
PegLight001.state = 2
fl2l.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub PegL003_Hit()
PlaySoundAt "rubber_hit_1" , PegL003
PegLight001.state = 2
fl2l.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub PegL004_Hit()
PlaySoundAt "rubber_hit_3" , PegL004
PegLight001.state = 2
fl2l.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

'O1'
Sub Pego1002_Hit()
PlaySoundAt "rubber_hit_2" , Pego1002
PegLight002.state = 2
fl3o.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub Pego1003_Hit()
PlaySoundAt "rubber_hit_2" , Pego1003
PegLight002.state = 2
fl3o.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub Pego1004_Hit()
PlaySoundAt "rubber_hit_3" , Pego1004
PegLight002.state = 2
fl3o.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub Pego1005_Hit()
PlaySoundAt "rubber_hit_1" , Pego1005
PegLight002.state = 2
fl3o.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

'O2'
Sub Pego2001_Hit()
PlaySoundAt "rubber_hit_1" , Pego2001
PegLight003.state = 2
fl4o.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub Pego2002_Hit()
PlaySoundAt "rubber_hit_3" , Pego2002
PegLight003.state = 2
fl4o.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub Pego2003_Hit()
PlaySoundAt "rubber_hit_3" , Pego2003
PegLight003.state = 2
fl4o.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub Pego2004_Hit()
PlaySoundAt "rubber_hit_2" , Pego2004
PegLight003.state = 2
fl4o.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub Pego2005001_Hit()
PlaySoundAt "rubber_hit_2" , Pego2005001
PegLight003.state = 2
fl4o.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub Pego2006_Hit()
PlaySoundAt "rubber_hit_2" , Pego2006
PegLight003.state = 2
fl4o.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub Pego2007_Hit()
PlaySoundAt "rubber_hit_2" , Pego2007
PegLight003.state = 2
fl4o.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub Pego2008_Hit()
PlaySoundAt "rubber_hit_1" , Pego2008
PegLight003.state = 2
fl4o.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

'E'
Sub PegD001_Hit()
PlaySoundAt "rubber_hit_3" , PegD001
PegLight004.state = 2: fl11e.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub PegD002_Hit()
PlaySoundAt "rubber_hit_2" , PegD002
PegLight004.state = 2
fl11e.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub PegD003_Hit()
PlaySoundAt "rubber_hit_2" , PegD003
PegLight004.state = 2
fl11e.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub PegD004_Hit()
PlaySoundAt "rubber_hit_3" , PegD004
PegLight004.state = 2
fl11e.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

'D'
Sub Wall003_Hit()
PegLight005.state = 2
fl5d.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

'I'
Sub Wall004_Hit()
PegLight006.state = 2
fl6i.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

'SLIFE'
Sub UnderTarg1_Hit()
CheckMultiplier
AddScore 2000
PlaySound "target2"
PegLight007.state = 2
fl7s.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub UnderTarg001_Hit()
CheckMultiplier
AddScore 2000
PegLight008.state = 2
PlaySound "target2"
fl8l.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub UnderTarg002_Hit()
CheckMultiplier
AddScore 2000
PegLight009.state = 2
PlaySound "target2"
fl9i.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

Sub UnderTarg003_Hit()
CheckMultiplier
AddScore 2000
PegLight010.state = 2
PlaySound "target2"
fl10f.state = 1
CheckBil
If BloodisLife = False Then BILl001.state = 0 and BILl002.state = 0 End If
End Sub

'Bumpers'

' Define variables to keep track of the number of bumper hits for each character
'Dim EHitCount
'Dim HHitCount
'Dim KHitCount
'Dim OHitCount

' Initialize variables
'EHitCount = 0
'HHitCount = 0
'KHitCount = 0
'OHitCount = 0


    'EHitCount = EHitCount + 1 ' Increment Ellen hit count
    'HHitCount = HHitCount + 1 ' Increment Hutter hit count
    'KHitCount = KHitCount + 1 ' Increment Knock hit count
    'OHitCount = OHitCount + 1 ' Increment Orlock hit count
    ' Reset all lights and drop targets to their initial states
'ell1.state = 0:ell2.state = 0:ell3.state = 0:ell4.state = 0:ell5.state = 0
'hut1.state = 0:hut2.state = 0:hut3.state = 0:hut4.state = 0:hut5.state = 0:hut6.state = 0
'kno1.state = 0:kno2.state = 0:kno3.state = 0:kno4.state = 0:kno5.state = 0
'orl1.state = 0:orl2.state = 0:orl3.state = 0:orl4.state = 0:orl5.state = 0:orl6.state = 0

' Define variables to keep track of the currently lit light and the total number of lights in each set
Dim CurrentLightEll
Dim TotalLightsEll
CurrentLightEll = 1
TotalLightsEll = 5

Dim CurrentLightHut
Dim TotalLightsHut
CurrentLightHut = 1
TotalLightsHut = 6

Dim CurrentLightKno
Dim TotalLightsKno
CurrentLightKno = 1
TotalLightsKno = 5

Dim CurrentLightOrl
Dim TotalLightsOrl
CurrentLightOrl = 1
TotalLightsOrl = 6

Sub Bumper002_Hit ()
PlaySoundAt "bumper_4" , Bumper002
BumpLight001.duration 1,200,0
BumpLight008.duration 1,200,0
BotBumLight001.duration 1,200,0
CheckMultiplier
Addscore 1000    
    If EHKO1.State = 1 Then
        LightUpEllLights
    ElseIf EHKO2.State = 1 Then
        LightUpHutLights
    ElseIf EHKO3.State = 1 Then
        LightUpKnoLights
    ElseIf EHKO4.State = 1 Then
        LightUpOrlLights
    End If
End Sub

Sub LightUpEllLights()
    ' Turn off all ell lights
    ell1.State = 0
    ell2.State = 0
    ell3.State = 0
    ell4.State = 0
    ell5.State = 0

    ' Turn on the current ell light
    Select Case CurrentLightEll
        Case 1
            ell1.State = 1
        Case 2
            ell1.State = 1:ell2.State = 1
        Case 3
            ell1.State = 1:ell2.State = 1:ell3.State = 1
        Case 4
            ell1.State = 1:ell2.State = 1:ell3.State = 1:ell4.State = 1
        Case 5
            ell1.State = 1:ell2.State = 1:ell3.State = 1:ell4.State = 1:ell5.State = 1
    End Select

    CurrentLightEll = CurrentLightEll + 1
' Signal kicker to collect character bonus
    If CurrentLightEll > TotalLightsEll Then
        ell1.State = 2:ell2.State = 2:ell3.State = 2:ell4.State = 2:ell5.State = 2
    ThrallE1.State = 1
    ThrallE1.TimerEnabled = 1
    ThrallE1.TimerInterval = 15000 ' Set the timer interval to 15,000 milliseconds (15 seconds)
    saucl1.duration 2,15000, 0 'collect kicker flashing red
End If

End Sub

Sub ThrallE1_Timer
    ThrallE1.State = 0 'turn off Thrall light
    ell1.State = 0:ell2.State = 0:ell3.State = 0:ell4.State = 0:ell5.State = 0 'turn off character lights
PlaySound "chareset"
    ThrallE1.TimerEnabled = False ' Disable the timer after it runs once
' Reset CurrentLightEll to zero
    CurrentLightEll = 1
End Sub


Sub LightUpHutLights()
    ' Similar logic as LightUpEllLights for hut lights
    ' Turn off all hut lights
    hut1.State = 0
    hut2.State = 0
    hut3.State = 0
    hut4.State = 0
    hut5.State = 0
    hut6.State = 0
    ' Turn on the current hut light
    Select Case CurrentLightHut
        Case 1
            hut1.State = 1
        Case 2
            hut1.State = 1:hut2.State = 1
        Case 3
            hut1.State = 1:hut2.State = 1:hut3.State = 1
        Case 4
            hut1.State = 1:hut2.State = 1:hut3.State = 1:hut4.State = 1
        Case 5
            hut1.State = 1:hut2.State = 1:hut3.State = 1:hut4.State = 1:hut5.State = 1
        Case 6
            hut1.State = 1:hut2.State = 1:hut3.State = 1:hut4.State = 1:hut5.State = 1:hut6.State = 1
    End Select

    ' Update the current ell light for the next hit
    CurrentLightHut = CurrentLightHut + 1
    ' Signal kicker to collect character bonus
    If CurrentLightHut > TotalLightsHut Then
        hut1.State = 2:hut2.State = 2:hut3.State = 2:hut4.State = 2:hut5.State = 2:hut6.State = 2
    ThrallH1001.State = 1
    ThrallH1001.TimerEnabled = 1
    ThrallH1001.TimerInterval = 15000 ' Set the timer interval to 15,000 milliseconds (15 seconds)
    saucl1.duration 2,15000, 0 'collect kicker flashing red
End If
' Reset CurrentLightHut to zero
    
End Sub

Sub ThrallH1001_Timer
    ThrallH1001.State = 0 'turn off Thrall light
    hut1.State = 0:hut2.State = 0:hut3.State = 0:hut4.State = 0:hut5.State = 0:hut6.State = 0 'turn off character lights
PlaySound "chareset"
    ThrallH1001.TimerEnabled = False ' Disable the timer after it runs once
CurrentLightHut = 1
End Sub

Sub LightUpKnoLights()
    ' Similar logic as LightUpEllLights for kno lights
    ' Turn off all kno lights
    kno1.State = 0
    kno2.State = 0
    kno3.State = 0
    kno4.State = 0
    kno5.State = 0

    ' Turn on the current ell light
    Select Case CurrentLightKno
        Case 1
            kno1.State = 1
        Case 2
            kno1.State = 1:kno2.State = 1
        Case 3
            kno1.State = 1:kno2.State = 1:kno3.State = 1
        Case 4
            kno1.State = 1:kno2.State = 1:kno3.State = 1:kno4.State = 1
        Case 5
            kno1.State = 1:kno2.State = 1:kno3.State = 1:kno4.State = 1:kno5.State = 1
    End Select

    ' Update the current ell light for the next hit
    CurrentLightKno = CurrentLightKno + 1
' Signal kicker to collect character bonus
    If CurrentLightKno > TotalLightsKno Then
        kno1.State = 2:kno2.State = 2:kno3.State = 2:kno4.State = 2:kno5.State = 2
    ThrallK1.State = 1
    ThrallK1.TimerEnabled = 1
    ThrallK1.TimerInterval = 15000 ' Set the timer interval to 15,000 milliseconds (15 seconds)
    saucl1.duration 2,15000, 0 'collect kicker flashing red
End If
End Sub

Sub ThrallK1_Timer
    ThrallK1.State = 0 'turn off Thrall light
    kno1.State = 0:kno2.State = 0:kno3.State = 0:kno4.State = 0:kno5.State = 0 'turn off character lights
PlaySound "chareset"
    ThrallK1.TimerEnabled = False ' Disable the timer after it runs once
' Reset CurrentLightKno to zero
    CurrentLightKno = 1
End Sub

Sub LightUpOrlLights()
    ' Similar logic as LightUpEllLights for orl lights
    ' Turn off all orl lights
    orl1.State = 0
    orl2.State = 0
    orl3.State = 0
    orl4.State = 0
    orl5.State = 0
    orl6.State = 0
    ' Turn on the current orl light
    Select Case CurrentLightOrl
        Case 1
            orl1.State = 1
        Case 2
            orl1.State = 1:orl2.State = 1
        Case 3
            orl1.State = 1:orl2.State = 1:orl3.State = 1
        Case 4
            orl1.State = 1:orl2.State = 1:orl3.State = 1:orl4.State = 1
        Case 5
            orl1.State = 1:orl2.State = 1:orl3.State = 1:orl4.State = 1:orl5.State = 1
        Case 6
            orl1.State = 1:orl2.State = 1:orl3.State = 1:orl4.State = 1:orl5.State = 1:orl6.State = 1
    End Select

    ' Update the current ell light for the next hit
    CurrentLightOrl = CurrentLightOrl + 1
' Signal kicker to collect character bonus
    If CurrentLightOrl > TotalLightsOrl Then
        orl1.State = 2:orl2.State = 2:orl3.State = 2:orl4.State = 2:orl5.State = 2:orl6.State = 2
    ThrallO1.State = 1
    ThrallO1.TimerEnabled = 1
    ThrallO1.TimerInterval = 15000 ' Set the timer interval to 15,000 milliseconds (15 seconds)
    saucl1.duration 2,15000, 0 'collect kicker flashing red
End If
End Sub

Sub ThrallO1_Timer
    ThrallO1.State = 0 'turn off Thrall light
    orl1.State = 0:orl2.State = 0:orl3.State = 0:orl4.State = 0:orl5.State = 0:orl6.State = 0 'turn off character lights
PlaySound "chareset"
    ThrallO1.TimerEnabled = False ' Disable the timer after it runs once
' Reset CurrentLightOrl to zero
    CurrentLightOrl = 1
End Sub

Sub Bumper001_Hit ()
PlaySoundAt "bumper_3" , Bumper001
BumpLight002.duration 1,200,0
BumpLight007.duration 1,200,0
BotBumLight002.duration 1,200,0
CheckMultiplier
Addscore 1000
If EHKO1.State = 1 Then
        LightUpEllLights
    ElseIf EHKO2.State = 1 Then
        LightUpHutLights
    ElseIf EHKO3.State = 1 Then
        LightUpKnoLights
    ElseIf EHKO4.State = 1 Then
        LightUpOrlLights
    End If
End Sub


Sub Bumper003_Hit ()
PlaySoundAt "bumper_1" , Bumper003
BumpLight003.duration 1,200,0
BumpLight006.duration 1,200,0
BotBumLight003.duration 1,200,0
CheckMultiplier
Addscore 1000
If EHKO1.State = 1 Then
        LightUpEllLights
    ElseIf EHKO2.State = 1 Then
        LightUpHutLights
    ElseIf EHKO3.State = 1 Then
        LightUpKnoLights
    ElseIf EHKO4.State = 1 Then
        LightUpOrlLights
    End If
End Sub


Sub Bumper004_Hit ()
PlaySoundAt "bumper_2" , Bumper004
BumpLight004.duration 1,200,0
BumpLight005.duration 1,200,0
BotBumLight004.duration 1,200,0
CheckMultiplier
Addscore 1000
If EHKO1.State = 1 Then
        LightUpEllLights
    ElseIf EHKO2.State = 1 Then
        LightUpHutLights
    ElseIf EHKO3.State = 1 Then
        LightUpKnoLights
    ElseIf EHKO4.State = 1 Then
        LightUpOrlLights
    End If
End Sub

'Buttons'
Sub SunTrigger004_Hit ()
PlaySoundAt "target" , SunTrigger004
butl1.state = 1:spbl1.state = 2
butl2.state = 0:spbl2.state = 0
butl3.state = 0:spbl3.state = 0
butl4.state = 0:spbl4.state = 0
butl5.state = 0:spbl5.state = 0
butl6.state = 0:spbl6.state = 0
End Sub

Sub SunTrigger001_Hit ()
PlaySoundAt "target" , SunTrigger001
butl2.state = 1:spbl2.state = 2
butl1.state = 0:spbl1.state = 0
butl3.state = 0:spbl3.state = 0
butl4.state = 0:spbl4.state = 0
butl5.state = 0:spbl5.state = 0
butl6.state = 0:spbl6.state = 0
End Sub

Sub SunTrigger002_Hit ()
PlaySoundAt "target" , SunTrigger002
butl3.state = 1:spbl3.state = 2
butl1.state = 0:spbl1.state = 0
butl2.state = 0:spbl2.state = 0
butl4.state = 0:spbl4.state = 0
butl5.state = 0:spbl5.state = 0
butl6.state = 0:spbl6.state = 0
End Sub

Sub SunTrigger003_Hit ()
PlaySoundAt "target" , SunTrigger003
butl4.state = 1:spbl4.state = 2
butl1.state = 0:spbl1.state = 0
butl2.state = 0:spbl2.state = 0
butl3.state = 0:spbl3.state = 0
butl5.state = 0:spbl5.state = 0
butl6.state = 0:spbl6.state = 0
End Sub

Sub SunTrigger005_Hit ()
PlaySoundAt "target" , SunTrigger005
butl5.state = 1:spbl5.state = 2
butl1.state = 0:spbl1.state = 0
butl2.state = 0:spbl2.state = 0
butl3.state = 0:spbl3.state = 0
butl4.state = 0:spbl4.state = 0
butl6.state = 0:spbl6.state = 0
End Sub

Sub SunTrigger006_Hit ()
PlaySoundAt "target" , SunTrigger006
butl6.state = 1:spbl6.state = 2
butl1.state = 0:spbl1.state = 0
butl2.state = 0:spbl2.state = 0
butl3.state = 0:spbl3.state = 0
butl4.state = 0:spbl4.state = 0
butl5.state = 0:spbl5.state = 0
End Sub

'Spinner'
Sub Spinner001_Spin ()
PlaySoundAt "fx_spinner", spinner001
spblCOL001.duration 2,150,0

End Sub

Sub SpinTrigger001_Hit ()
PlaySound "sunphase"
PlayfieldMultiplier(CurrentPlayer) = 1
If spbl1.state = 2 Then
AddScore 15000
    DMDFlush
    DMD CL("SUN PHASE BONUS"), CL("15.000"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
ElseIf spbl2.state = 2 Then
AddScore 25000
    DMDFlush
    DMD CL("SUN PHASE BONUS"), CL("25.000"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
ElseIf spbl3.state = 2 Then
AddScore 40000
    DMDFlush
    DMD CL("SUN PHASE BONUS"), CL("40.000"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
ElseIf spbl4.state = 2 Then
AddScore 60000
    DMDFlush
    DMD CL("SUN PHASE BONUS"), CL("60.000"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
ElseIf spbl5.state = 2 Then
    AddScore 88000
    DMDFlush
    DMD CL("SUN PHASE BONUS"), CL("88.000"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
ElseIf spbl6.state = 2 Then
    AddScore 125000
    DMDFlush
    DMD CL("SUN PHASE BONUS"), CL("125.000"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
End If
End Sub

'Door and Outlane Lights'

'doors with 001 suffix only come up in NOSFERATU'
Sub DoorLeft001_Hit ()
If bNOSFER = True Then
DoorLeft.IsDropped = False
    DMDFlush
    DMD CL("NOSFERATU MODE"), CL("YOU ARE PROTECTED"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
PlaySoundAt "Target_Hit_1", DoorLeft
End If
End Sub

Sub DoorRight001_Hit ()
If bNOSFER = True Then
DoorRight001.IsDropped = False
    DMDFlush
    DMD CL("NOSFERATU MODE"), CL("YOU ARE PROTECTED"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
PlaySoundAt "Target_Hit_1", DoorLeft
End If
End Sub

Sub DoorLeft_Hit ()
    CheckMultiplier
AddScore 100
    DMDFlush
    DMD CL("CAREFUL YOU ARE"), CL("NO LONGER PROTECTED"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
DoorLeft.IsDropped = True
PlaySoundAt "Drop_Target_Down_2" , DoorLeft
PlaySoundAt "creek" , DoorLeft
LCRL.state = 0
LskL.state = 2

End Sub

Sub DoorRight_Hit ()
   CheckMultiplier
 AddScore 100
    DMDFlush
    DMD CL("CAREFUL YOU ARE"), CL("NO LONGER PROTECTED"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
DoorRight.IsDropped = True
PlaySoundAt "Drop_Target_Down_2" , DoorRight
PlaySoundAt "creek" , DoorRight
RCRL.state = 0
RskL.state = 2
End Sub

'NOSF-RATU Drop Targets'
Sub Target1N_Hit ()
CheckMultiplier
AddScore 1000
Target1N.IsDropped = True
PlaySoundAt "fx_droptarget" , Target1N
Nnos.state = 1
lb1.state = 2
CheckNosReady
End Sub

Sub Target2O_Hit ()
CheckMultiplier
AddScore 1000
Target2O.IsDropped = True
PlaySoundAt "fx_droptarget" , Target2O
Onos.state = 1
lb2.state = 2
CheckNosReady
End Sub

Sub Target3S_Hit ()
CheckMultiplier
AddScore 1000
Target3S.IsDropped = True
PlaySoundAt "fx_droptarget" , Target3S
Snos.state = 1
lb3.state = 2
CheckNosReady
End Sub

Sub Target4F_Hit ()
CheckMultiplier
AddScore 1000
Target4F.IsDropped = True
PlaySoundAt "fx_droptarget" , Target4F
Fnos.state = 1
lb4.state = 2
CheckNosReady
End Sub

Sub Target5R_Hit ()
CheckMultiplier
AddScore 1000
Target5R.IsDropped = True
PlaySoundAt "fx_droptarget" , Target5R
Rnos.state = 1
lb5.state = 2
CheckNosReady
End Sub

Sub Target6A_Hit ()
CheckMultiplier
AddScore 1000
Target6A.IsDropped = True
PlaySoundAt "fx_droptarget" , Target6A
Anos.state = 1
lb6.state = 2
CheckNosReady
End Sub

Sub Target7T_Hit ()
CheckMultiplier
AddScore 1000
Target7T.IsDropped = True
PlaySoundAt "fx_droptarget" , Target7T
Tnos.state = 1
lb7.state = 2
CheckNosReady
End Sub

Sub Target8U_Hit ()
CheckMultiplier
AddScore 1000
Target8U.IsDropped = True
PlaySoundAt "fx_droptarget" , Target8U
Unos.state = 1
lb8.state = 2
CheckNosReady
End Sub

'fake EHKO test'
' Define a counter variable to keep track of the currently lit light
Dim PLightCounter
PLightCounter = 1 ' Start with the first light (EHKO1)

Sub Wall001_SlingShot()
PlaySound "Chime_Right"
PlaySound "metalhit_medium"
CheckMultiplier
Addscore 100
    ' Turn off all lights
    EHKO1.State = 0
    EHKO2.State = 0
    EHKO3.State = 0
    EHKO4.State = 0

    ' Determine the next light to turn on based on the counter
    If PLightCounter = 1 Then
        EHKO1.State = 1
        PLightCounter = 2
    ElseIf PLightCounter = 2 Then
        EHKO2.State = 1
        PLightCounter = 3
    ElseIf PLightCounter = 3 Then
        EHKO3.State = 1
        PLightCounter = 4
    ElseIf PLightCounter = 4 Then
        EHKO4.State = 1
        PLightCounter = 1
    End If
End Sub

Sub Wall002_SlingShot()
PlaySound "Chime_Right"
PlaySound "metalhit_medium"
CheckMultiplier
Addscore 100
    ' Turn off all lights
    EHKO1.State = 0
    EHKO2.State = 0
    EHKO3.State = 0
    EHKO4.State = 0

    ' Determine the next light to turn on based on the counter
    If PLightCounter = 1 Then
        EHKO1.State = 1
        PLightCounter = 2
    ElseIf PLightCounter = 2 Then
        EHKO2.State = 1
        PLightCounter = 3
    ElseIf PLightCounter = 3 Then
        EHKO3.State = 1
        PLightCounter = 4
    ElseIf PLightCounter = 4 Then
        EHKO4.State = 1
        PLightCounter = 1
    End If
End Sub

'Mini Wall Slingshots'
Sub Wall006_SlingShot()
PlaySound "minislingR"
CheckMultiplier
Addscore 100
End Sub

Sub Wall004_SlingShot()
PlaySound "minislingR"
CheckMultiplier
Addscore 100
End Sub

Sub Wall003_SlingShot()
PlaySound "minislingL"
CheckMultiplier
Addscore 100
End Sub

'Inlanes / Outlanes'
Sub LeftInlane_Hit
If ILBLight1.state = 1 Then
PlayfieldMultiplier(CurrentPlayer) = 1
AddScore 25000
    DMDFlush
    DMD CL("DEAD FLOWERS 4 ELLEN"), CL("BONUS COLLECTED"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
PlaySound "sensor" 
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
ILBLight1.duration 2, 2000, 0
PlaySoundAt "Bell10", LeftInlane
Else
PlayfieldMultiplier(CurrentPlayer) = 1
AddScore 500
PlaySound "sensor" 
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
End If
End Sub

Sub RightInlane_Hit
If ILBLight2.state = 1 Then
PlayfieldMultiplier(CurrentPlayer) = 1
AddScore 25000
    DMDFlush
    DMD CL("LAND OF SPECTRES"), CL("BONUS COLLECTED"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
PlaySound "sensor" 
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
ILBLight2.duration 2, 2000, 0
PlaySoundAt "Bell10", RightInlane
Else
PlaySound "sensor" 
PlayfieldMultiplier(CurrentPlayer) = 1
AddScore 500
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
End If
End Sub

Sub LeftOutlane_Hit
PlaySound "sensor" 
PlaySound "ding1000"
PlaySoundAt "great", LeftOutlane
PlayfieldMultiplier(CurrentPlayer) = 1
AddScore 500
    DMDFlush
    DMD CL("GREAT DEATH"), CL("...AND GOODBYE"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
gdr.duration 2, 1500, 0:gdl.duration 2, 1500, 0
End Sub

Sub RightOutlane_Hit
PlaySound "sensor"
PlaySound "ding1000"
PlaySoundAt "great", RightOutlane
PlayfieldMultiplier(CurrentPlayer) = 1       
AddScore 500
    DMDFlush
    DMD CL("GREAT DEATH"), CL("...AND GOODBYE"), "_", eNone, eBlinkFast, eNone, 1500, True, ""
butl1.state = 0
butl2.state = 0
butl3.state = 0
butl4.state = 0
butl5.state = 0
butl6.state = 0
spbl1.state = 0
spbl2.state = 0
spbl3.state = 0
spbl4.state = 0
spbl5.state = 0
spbl6.state = 0
gdr.duration 2, 1500, 0:gdl.duration 2, 1500, 0
End Sub

'spring drain post test'
Sub RubberTemp002_Hit
PlaySound "rubber_hit_1"
End Sub

' 5 dead sailors'

LastSwitchHit = ""
FlatETargetHitEnabled = True

Sub bb1_Hit()
    bb1.IsDropped = True: PlaySoundAt "fx_droptarget" , bb1:PlaySoundAt "sailorhit" , bb1
    sul1.state = 0
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("DEAD SAILOR DOWN"), CL("MULT X 5.000"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
CheckResetSailors
End Sub

Sub bb001_Hit()
    bb001.IsDropped = True: PlaySoundAt "fx_droptarget" , bb001:PlaySoundAt "sailorhit" , bb001
    sul2.state = 0
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("DEAD SAILOR DOWN"), CL("MULT X 5.000"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
CheckResetSailors
End Sub

Sub bb002_Hit()
    bb002.IsDropped = True: PlaySoundAt "fx_droptarget" , bb002:PlaySoundAt "sailorhit" , bb002
    sul3.state = 0
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("DEAD SAILOR DOWN"), CL("MULT X 5.000"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
CheckResetSailors
End Sub

Sub bb003_Hit()
    bb003.IsDropped = True: PlaySoundAt "fx_droptarget" , bb003:PlaySoundAt "sailorhit" , bb003
    sul001.state = 0
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("DEAD SAILOR DOWN"), CL("MULT X 5.000"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
CheckResetSailors
End Sub

Sub bb004_Hit()
    bb004.IsDropped = True: PlaySoundAt "fx_droptarget" , bb004:PlaySoundAt "sailorhit" , bb004
    sul5.state = 0
    CheckMultiplier
AddScore 5000
    DMDFlush
    DMD CL("DEAD SAILOR DOWN"), CL("MULT X 5.000"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
CheckResetSailors
End Sub

Sub SailTrigger001_Hit()
    PlaySoundAt "sensor", SailTrigger001
    LastSwitchHit = "SailTrigger001"
End Sub

Sub SailTrigger003_Hit()
       ' Check for trigger and light
PlaySoundAt "sensor", SailTrigger003
    LastSwitchHit = "SailTrigger003" 
If KickerJacks = True Then
    CheckMultiplier
AddScore 12500
    DMDFlush
    DMD CL("EMPUSA JACKPOT"), CL("NOSFERATU"), "_", eNone, eBlinkFast, eNone, 2000, True, ""    
PlaySound "jackpot"
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movemp
End If
End Sub

Sub SailTrigger002_Hit()
If LastSwitchHit = "SailTrigger003" And deadsl001.state = 1 Then
        SAILORSUP
        deadsl001.state = 0
DoorLeft001.IsDropped = True
        DoorLeft.IsDropped = False
        PlaySoundAt "fx_droptargetreset", DoorLeft
        LCRL.state = 2
        LskL.state = 0
DoorRight001.IsDropped = True        
DoorRight.IsDropped = False
        PlaySoundAt "fx_droptargetreset", DoorRight
        RCRL.state = 2
        RskL.state = 0
    ElseIf deadsl001.state = 0 Then
        ' Handle the case when deadsl001.state is 0
    Else
        ' Handle the case when deadsl001.state is 1 but the last trigger hit was not SailTrigger001
 End If   
End Sub

Sub SUP1:bb1.IsDropped = False:sul1.state = 2:PlaySoundAt "DropTarget_Up", bb1:End Sub
Sub SUP2:bb001.IsDropped = False:sul2.state = 2:PlaySoundAt "DropTarget_Up", bb001:End Sub
Sub SUP3:bb002.IsDropped = False:sul3.state = 2:PlaySoundAt "DropTarget_Up", bb002:End Sub
Sub SUP4:bb003.IsDropped = False:sul001.state = 2:PlaySoundAt "DropTarget_Up", bb003:End Sub
Sub SUP5:bb004.IsDropped = False:sul5.state = 2:PlaySoundAt "DropTarget_Up", bb004:End Sub

Sub SAILORSUP()
    CheckMultiplier
AddScore 20000
    DMDFlush
    DMD CL("E    PLAGUE TARGET"), CL("5 DEAD SAILORS"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
vpmTimer.AddTimer 100, "SUP1 '"
vpmTimer.AddTimer 200, "SUP2 '"
vpmTimer.AddTimer 300, "SUP3 '"
vpmTimer.AddTimer 400, "SUP4 '"
vpmTimer.AddTimer 500, "SUP5 '"
PlaySound "sailorsupsplash"
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movsai
FlatETargetHitEnabled = False
End Sub


Sub CheckResetSailors
    If bb1.IsDropped = 1 and bb001.IsDropped = 1 and bb002.IsDropped = 1 and bb003.IsDropped = 1 and bb004.IsDropped = 1 Then
FlatETargetHitEnabled = True
    CheckMultiplier
AddScore 50000
    DMDFlush
    DMD CL("DEAD SAILOR BONUS"), CL("MULT X 50.000"), "_", eNone, eBlink, eNone, 1500, True, ""
sqL3.state = 1
PlaySound "MotorLeer"
PlaySound "sailwin"
    End If
End Sub

'The Carriage Man'
Sub CMTarget001_Hit()
    CheckMultiplier
AddScore 25000
    DMDFlush
    DMD CL("CARRIAGEMAN BONUS"), CL("MULT X 25.000"), "_", eNone, eBlink, eNone, 2000, True, ""
PlaySoundAt "fx_droptarget" , CMTarget001
PlaySound "MotorLeer"
PlaySound "carrichime"
CMTarget001.IsDropped = True
sqL2.state = 2
cul001.state = 0
End Sub

'Werewolf'
Sub WWTarget001_Hit ()
    CheckMultiplier
AddScore 10000
    DMDFlush
    DMD CL("WEREWOLF BONUS"), CL("MULT X 10.000"), "_", eNone, eBlink, eNone, 2000, True, ""
PlaySoundAt "fx_droptarget" , WWTarget001:PlaySoundAt "target2" , WWTarget001
PlaySound "MotorLeer"
PlaySound "wolfbon"
WWTarget001.IsDropped = True
sqL1.State = 2
wul001.state = 0
End Sub

Sub NosTargetF_Animate
    NosTargetP1.Z = NosTargetF.CurrentAngle
End Sub

Sub DropNosferatu
    NosTargetF.RotateToStart
    NosTarget001.IsDropped = 1
    NosTargetW.IsDropped = 1
End Sub

Sub RiseNosferatu
    NosTargetF.RotateToEnd
    NosTarget001.IsDropped = 0
    NosTargetW.IsDropped = 0
End Sub

Dim NosHitCount ' hit count for NosTarget001_Hit
'initial state
NosHitCount = 0

Dim BallSaveDoors
'initial state
BallSaveDoors = False

Sub BallSaveCountDown ()
    If BallSaveDoors Then ' Check the flag
DoorLeft.TimerEnabled = True ' Enable the timer
DoorLeft.TimerInterval = 55000 ' Set the timer interval to 55,000 milliseconds (55 seconds)
DoorRight.TimerEnabled = True ' Enable the timer
DoorRight.TimerInterval = 55000 ' Set the timer interval to 55,000 milliseconds (55 seconds)
BallSaveDoors = False ' reset flag
End If
End Sub

Sub DoorLeft_Timer
If bNOSFER = True and DoorLeft001.IsDropped = 0 Then
DoorLeft.IsDropped = False
DoorLeft001.IsDropped = True
LCRL.state = 2
LskL.state = 0
DoorLeft.TimerEnabled = False
End If
If bNOSFER = True and DoorLeft.IsDropped = 0 Then
DoorLeft.IsDropped = False
DoorLeft001.IsDropped = True
LCRL.state = 2
LskL.state = 0
DoorLeft.TimerEnabled = False
End If
If bNOSFER = True and DoorLeft.IsDropped = 1 Then
DoorLeft.IsDropped = True
DoorLeft001.IsDropped = True
LCRL.state = 0
LskL.state = 2
DoorLeft.TimerEnabled = False
End If
If bNOSFER = False Then
DoorLeft.TimerEnabled = False
End If
End Sub

Sub DoorRight_Timer
If bNOSFER = True and DoorRight001.IsDropped = 0 Then
DoorRight.IsDropped = False
DoorRight001.IsDropped = True
RCRL.state = 2
RskL.state = 0
DoorRight.TimerEnabled = False
End If
If bNOSFER = True and DoorRight.IsDropped = 0 Then
DoorRight.IsDropped = False
DoorRight001.IsDropped = True
RCRL.state = 2
RskL.state = 0
DoorRight.TimerEnabled = False
End If
If bNOSFER = True and DoorRight.IsDropped = 1 Then
DoorRight.IsDropped = True
DoorRight001.IsDropped = True
RCRL.state = 0
RskL.state = 2
DoorRight.TimerEnabled = False
End If
If bNOSFER = False Then
DoorRight.TimerEnabled = False
End If
End Sub

'Nosferatu'
Sub NOSFERATU()
AddScore 35000
    DMDFlush
    DMD CL("NOSFERATU"), CL("KICKER JACKPOTS"), "_", eNone, eBlink, eNone, 2000, True, ""
Light004.duration 2,2000, 0
RiseNosferatu
Nosul001.state = 2
PlaySoundAt "fx_droptarget" , NosTargetP1
Enos.state = 1:Nnos.state = 0:Onos.state = 0:Snos.state = 0:Fnos.state = 0:Rnos.state = 0:Anos.state = 0:Tnos.state = 0:Unos.state = 0
BallSaveDoors = True
BallSaveCountDown
DoorLeft.IsDropped = True
DoorLeft001.IsDropped = False
PlaySoundAt "fx_droptargetreset", DoorLeft001
LCRL.state = 1
LskL.state = 0
DoorRight.IsDropped = True
DoorRight001.IsDropped = False
PlaySoundAt "fx_droptargetreset", DoorRight001
RCRL.state = 1
RskL.state = 0
KickerJacks = True
NosHitCount = 0
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movnos
End Sub

Sub CheckNOST ()
If KickerJacks = True Then
RiseNosferatu
Nosul001.state = 2
PlaySoundAt "nosunhit", NosTargetP1
End If
End Sub

Sub NosTarget001_Hit()
If KickerJacks = True Then
NosHitCount = NosHitCount + 1 ' Increment the hit count
If NosHitCount = 1 Then
CheckMultiplier
AddScore 15000
    DMDFlush
    DMD CL("GREAT SHOT"), CL("6 MORE TO GO..."), "_", eBlink, eBlinkFast, eNone, 1500, True, ""
PlaySoundAt "noshit", NosTargetP1
Light005.duration 2 ,90, 0
Nnos.duration 2, 875, 0
Onos.duration 2, 875, 0
Snos.duration 2, 875, 0
Fnos.duration 2, 875, 0
Rnos.duration 2, 875, 0
Anos.duration 2, 875, 0
Tnos.duration 2, 875, 0
Unos.duration 2, 875, 0
DropNosferatu
Nosul001.state = 0
vpmTimer.AddTimer 500, "CheckNOST '"

ElseIf NosHitCount = 2 Then
CheckMultiplier
AddScore 15000
    DMDFlush
    DMD CL("GREAT SHOT"), CL("5 MORE TO GO..."), "_", eBlink, eBlinkFast, eNone, 1500, True, ""
PlaySoundAt "noshit", NosTargetP1
Light005.duration 2 ,90, 0
Nnos.duration 2, 875, 0
Onos.duration 2, 875, 0
Snos.duration 2, 875, 0
Fnos.duration 2, 875, 0
Rnos.duration 2, 875, 0
Anos.duration 2, 875, 0
Tnos.duration 2, 875, 0
Unos.duration 2, 875, 0
DropNosferatu
Nosul001.state = 0
vpmTimer.AddTimer 500, "CheckNOST '"

ElseIf NosHitCount = 3 Then
CheckMultiplier
AddScore 15000
    DMDFlush
    DMD CL("GREAT SHOT"), CL("4 MORE TO GO..."), "_", eBlink, eBlinkFast, eNone, 1500, True, ""
PlaySoundAt "noshit", NosTargetP1
Light005.duration 2 ,90, 0
Nnos.duration 2, 875, 0
Onos.duration 2, 875, 0
Snos.duration 2, 875, 0
Fnos.duration 2, 875, 0
Rnos.duration 2, 875, 0
Anos.duration 2, 875, 0
Tnos.duration 2, 875, 0
Unos.duration 2, 875, 0
DropNosferatu
Nosul001.state = 0
vpmTimer.AddTimer 500, "CheckNOST '"

ElseIf NosHitCount = 4 Then
CheckMultiplier
AddScore 15000
    DMDFlush
    DMD CL("GREAT SHOT"), CL("3 MORE TO GO..."), "_", eBlink, eBlinkFast, eNone, 1500, True, ""
PlaySoundAt "noshit", NosTargetP1
Light005.duration 2 ,90, 0
Nnos.duration 2, 875, 0
Onos.duration 2, 875, 0
Snos.duration 2, 875, 0
Fnos.duration 2, 875, 0
Rnos.duration 2, 875, 0
Anos.duration 2, 875, 0
Tnos.duration 2, 875, 0
Unos.duration 2, 875, 0
DropNosferatu
Nosul001.state = 0
vpmTimer.AddTimer 500, "CheckNOST '"

ElseIf NosHitCount = 5 Then
CheckMultiplier
AddScore 15000
    DMDFlush
    DMD CL("GREAT SHOT"), CL("2 MORE TO GO..."), "_", eBlink, eBlinkFast, eNone, 1500, True, ""
PlaySoundAt "noshit", NosTargetP1
Light005.duration 2 ,90, 0
Nnos.duration 2, 875, 0
Onos.duration 2, 875, 0
Snos.duration 2, 875, 0
Fnos.duration 2, 875, 0
Rnos.duration 2, 875, 0
Anos.duration 2, 875, 0
Tnos.duration 2, 875, 0
Unos.duration 2, 875, 0
DropNosferatu
Nosul001.state = 0
vpmTimer.AddTimer 500, "CheckNOST '"

ElseIf NosHitCount = 6 Then
CheckMultiplier
AddScore 15000
    DMDFlush
    DMD CL("GREAT SHOT"), CL("1 MORE TO GO..."), "_", eBlink, eBlinkFast, eNone, 1500, True, ""
PlaySoundAt "noshit", NosTargetP1
Light005.duration 2 ,90, 0
Nnos.duration 2, 875, 0
Onos.duration 2, 875, 0
Snos.duration 2, 875, 0
Fnos.duration 2, 875, 0
Rnos.duration 2, 875, 0
Anos.duration 2, 875, 0
Tnos.duration 2, 875, 0
Unos.duration 2, 875, 0
DropNosferatu
Nosul001.state = 0
vpmTimer.AddTimer 500, "CheckNOST '"

ElseIf NosHitCount = 7 Then
CheckMultiplier
AddScore 200000
    DMDFlush
    DMD CL("GREAT SHOT"), CL("NOSEFRATU DESTROYED"), "_", eBlink, eBlinkFast, eNone, 1500, True, ""
DropNosferatu
Nosul001.state = 0
PlaySoundAt "noshit", NosTargetP1
Light005.duration 2 ,90, 0
PlaySoundAt "projector", Peg5
Light005R.duration 2, 1772, 0:Light005L.duration 2, 1772, 0
progR.duration 2, 1772, 0:progL.duration 2, 1772, 0
PlaySoundAt "projector", Peg
PlayMovie movsun
sqL7.state = 2
PlaySound "cbell"
PlaySound "MotorLeer"
StopNOS
End If
End If
End Sub

Sub StopNOS ()
DropNosferatu
Nosul001.state = 0
KickerJacks = False
bNOSFER = False
        bMultiBallMode = False
        bAutoPlunger = False
StopSong
PlaySong "music_gp"
LCRL.state = 2
LskL.state = 0
RCRL.state = 2
RskL.state = 0
DoorRight.IsDropped = False
DoorRight001.IsDropped = True
PlaySoundAt "fx_droptargetreset", DoorRight001
DoorLeft.IsDropped = False
DoorLeft001.IsDropped = True
PlaySoundAt "fx_droptargetreset", DoorLeft001
NosHitCount = 0
Target1N.IsDropped = False:Target2O.IsDropped = False:Target3S.IsDropped = False:Target4F.IsDropped = False:Target5R.IsDropped = False:Target6A.IsDropped = False:Target7T.IsDropped = False:Target8U.IsDropped = False
Enos.state = 0:Nnos.state = 0:Onos.state = 0:Snos.state = 0:Fnos.state = 0:Rnos.state = 0:Anos.state = 0:Tnos.state = 0:Unos.state = 0
    EYELightR.state = 0
    EYELightL.state = 0
Nosul001.state = 0
BallSaveDoors = False
End Sub