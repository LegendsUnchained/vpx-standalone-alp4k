' ****************************************************************
'             VISUAL PINBALL X script for 10.7 and lateriser
'                Willow Script MARTY & JPSalas
'                          Version 3.0
' ****************************************************************


Option Explicit
Randomize


TR12.Enabled = 0 'To deactivate magna save replace 1 with 0
TR11.Enabled = 0 'To deactivate magna save replace 1 with 0

'************************
'Glowball
'*************************
Dim GlowAura,GlowIntensity

Const ChooseBall 			= 0		' *** Ball Settings **********
									' *** 0 = Normal Ball	
									' *** 1 = Purple GlowBall
									' *** 2 = Green GlowBall																		
									' *** 3 = Blue Glowball
									' *** 4 = Orange Glowball 
									' *** 5 = Red Glowball
									' *** 6 = White Glowball
									' *** 7 = Yellow Glowball
									' *** 8 = Gold Glowball
									

'******************
'Additional Ball Settings
'******************

GlowAura=200 'GlowBlob Auroa radius
GlowIntensity=12 'Glowblob intensity

'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = false               ' enable Pinup Player functions for this table
cPuPPack = "willow"    ' name of the PuP-Pack / PuPVideos folder for this table

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup Player

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

Sub pupevent(EventNum)
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

' ******* How to use PUPEvent to trigger / control a PuP-Pack *******

' Usage': pupevent(EventNum)

' EventNum = PuP Exxx trigger from the PuP-Pack

' Example: pupevent 102

' This will trigger E102 from the table's PuP-Pack

' DO NOT use any Exxx triggers already used for DOF (if used) to avoid any possible confusion

'************ PuP-Pack Startup **************

PuPStart(cPuPPack) 'Check for PuP - 

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1    ' 1 is the normal mass
Const SongVolume = 0.5 ' 1 is full volume. Value is from 0 to 1
Dim mMagnaSave1,mMagnaSave2, mMagnaSave3
dim spinner

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
'or keep it False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = True

' Define any Constants
Const cGameName = "willow"
Const myVersion = ""
Const MaxPlayers = 1     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 5  ' limit to 5x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 3   ' usually 3 or 5
Const MaxMultiballs = 5  ' max number of balls during multiballs

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If

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

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim PlayfieldMultiplier(4)
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot(4)
Dim SuperJackpot
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue(4)
Dim SkillshotValue1(4)
Dim bAutoPlunger
Dim bInstantInfo
Dim bInstantInfo1
Dim bAttractMode
Dim x
Dim bFlippersEnabled
Dim bRotateLights

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
'Dim bSongSelect
Dim bRampIsUp
Dim bRampIsUp1

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs
Dim cbRight   'captive ball

sub timer3_Timer()
'spinnerrond.rotZ=Spinner2.currentangle
'gouve.rotZ=Spinner2.currentangle
'spinnerrond1.rotZ=Spinner4.currentangle
end Sub

'***********************
' spot
'***********************

Dim MyPi1, SpotStep, SpotDir
Dim sRGBStep, sRGBFactor, sRed, sGreen, sBlue

Sub StartSpots
    Spot1.visible = 1
    Spot2.visible = 1
    MyPi1 = Round(4 * Atn(1), 6) / 90
    SpotStep = 0
    sRGBStep = 0
    sRGBFactor = 5
    sRed = 255
    sGreen = 252
    sBlue = 224
    Spots.Enabled = 1
End Sub

Sub StopSpots
    Spot1.visible = 0
    Spot2.visible = 0
    Spots.Enabled = 0
    Spot1.RotZ = 180
    Camera1.RotZ = 180
    Spot2.RotZ = 150
    Camera2.RotZ = 150
    Spot1.color = RGB(255, 252, 224)
    Spot2.color = RGB(255, 252, 224)
End Sub

Sub Spots_Timer()
    Spot1.visible = 1
    Spot2.visible = 1
    'rotate spots
    SpotDir = SIN(SpotStep * MyPi) * 30
    SpotStep = (SpotStep + 1)MOD 360
    Spot1.RotZ = 260 - SpotDir
    Camera1.RotZ = 260 - SpotDir
    Spot2.RotZ = 150 + SpotDir
    Camera2.RotZ = 150 + SpotDir
    ' color the spotlights
    Select Case sRGBStep
        Case 0 'Green
            sGreen = sGreen + sRGBFactor
            If sGreen> 255 then
                sGreen = 255
                sRGBStep = 1
            End If
        Case 1 'Red
            sRed = sRed - sRGBFactor
            If sRed <0 then
                sRed = 0
                sRGBStep = 2
            End If
        Case 2 'Blue
            sBlue = sBlue + sRGBFactor
            If sBlue> 255 then
                sBlue = 255
                sRGBStep = 3
            End If
        Case 3 'Green
            sGreen = sGreen - sRGBFactor
            If sGreen <0 then
                sGreen = 0
                sRGBStep = 4
            End If
        Case 4 'Red
            sRed = sRed + sRGBFactor
            If sRed> 255 then
                sRed = 255
                sRGBStep = 5
            End If
        Case 5 'Blue
            sBlue = sBlue - sRGBFactor
            If sBlue <0 then
                sBlue = 0
                sRGBStep = 0
            End If
    End Select
    Spot1.color = RGB(sRed, sGreen, sBlue)
    Spot2.color = RGB(sRed, sGreen, sBlue)
End Sub

'magnet'

Set mMagnaSave1 = New cvpmMagnet : With mMagnaSave1
	.InitMagnet Magna1, 5
End With

Sub Magna1_Hit():mMagnaSave1.AddBall ActiveBall: End Sub
Sub Magna1_UnHit(): mMagnaSave1.RemoveBall ActiveBall: End Sub

sub tr11_hit()
    PlaySound "Fx_magnet"
	mMagnaSave1.MagnetOn = 1
	magnettimer001.enabled=1

end sub

sub magnettimer001_timer()
	magnettimer001.enabled=0
	mMagnaSave1.MagnetOn = 0
end Sub

Set mMagnaSave2 = New cvpmMagnet : With mMagnaSave2
	.InitMagnet Magna2, 3
End With

Sub Magna2_Hit():mMagnaSave2.AddBall ActiveBall: End Sub
Sub Magna2_UnHit(): mMagnaSave2.RemoveBall ActiveBall: End Sub

sub tr12_hit()
    PlaySound "Fx_magnet"
	mMagnaSave2.MagnetOn = 1 
	magnettimer002.enabled=1 
end sub

sub magnettimer002_timer()
	magnettimer002.enabled=0
	mMagnaSave2.MagnetOn = 0
end Sub
  '************
  '**LockPost**

    lockpostG.isdropped = 1
    lockpostD.isdropped = 1

Sub checkpostoff()
    TpostG.Enabled = 0
    TpostD.Enabled = 0
End Sub

Sub checkpost()
    TpostG.Enabled = 1
    TpostD.Enabled = 1
End Sub


Sub TpostG_hit
    lockpostG.isdropped = 0:PlaySoundAt SoundFXDOF("fx_solenoid", 142, DOFPulse, DOFContactors), TpostG
    lockpostG.TimerInterval = 1500
    lockpostG.TimerEnabled = 1
End Sub

Sub lockpostG_Timer
    lockpostG.TimerEnabled = 0
    lockpostG.isdropped = 1:PlaySoundAt "fx_solenoidoff", TpostG
End Sub

Sub TpostD_hit
    lockpostD.isdropped = 0:PlaySoundAt SoundFXDOF("fx_solenoid", 142, DOFPulse, DOFContactors), TpostD
    lockpostD.TimerInterval = 1500
    lockpostD.TimerEnabled = 1
End Sub

Sub lockpostD_Timer
    lockpostD.TimerEnabled = 0
    lockpostD.isdropped = 1:PlaySoundAt "fx_solenoidoff", TpostD
End Sub

 '************
 '**lifering**

sub checklife()
    if Light38.state = 1 Then
       Lightsave1.state = 2
       Targetsave1.collidable = 1: PlaySoundAt "Fx_rebond", Targetsave1
end If
End Sub

sub lifeclose()
    if Light38.state = 0 Then
       Lightsave1.state = 0
       Targetsave1.collidable = 0
end If
End Sub

Sub Targetsave1_hit
    Targetsave1.collidable = 0
    Lightsave.state = 1
    Lightsave1.state = 0
    Rsaveball.Collidable = 0
    Tclosesave.enabled = 0
    AddScore 10000
End Sub

'***********

    recordspin.enabled = 0

'Sub recordspin_timer
    'helice.RotY = helice.RotY + 2
'End Sub

	 Set spinner = New cvpmTurntable
		With spinner
			.InitTurntable TurnTable, 10
			.SpinDown = 10
			.CreateEvents "spinner"
		End With

Sub SphereTimer_Timer
   'portal.objrotz = portal.objrotz + 3
end sub
 

'*******

ringspin.enabled = 1


' barell

Dim couteauGPos, couteauDPos

Sub ShakecouteauG
    couteauGPos = 8
    couteauGTimer.Enabled = 1
End Sub

Sub couteauGTimer_Timer
    'cage.TransZ = couteauGPos
    'seau.TransZ = couteauGPos
    If couteauGPos = 0 Then Me.Enabled = 0:Exit Sub
    If couteauGPos < 0 Then
        couteauGPos = ABS(couteauGPos)- 1
    Else
        couteauGPos = - couteauGPos + 1
    End If
End Sub

Sub ShakecouteauD
    couteauDPos = 8
    couteauDTimer.Enabled = 1
End Sub

Sub couteauDTimer_Timer
    'cage.TransZ = couteauDPos'
    'seau.TransZ = couteauDPos
    If couteauDPos = 0 Then Me.Enabled = 0:Exit Sub
    If couteauDPos < 0 Then
        couteauDPos = ABS(couteauDPos)- 1
    Else
        couteauDPos = - couteauDPos + 1
    End If
End Sub

sub KsaveR_hit()
	Timersaveball.enabled=1
end sub

sub Timersaveball_timer
	Timersaveball.enabled=0
	KsaveR.kick 0, 50
    Lightsave.state = 0
    Lightsave1.state = 0
    Tclosesave.enabled = 1
    PlaySound "saveball"
end sub

Sub Tclosesave_hit
    Rsaveball.Collidable = 1
End Sub


'**************
' Metal Ramp
'*************
'**ramp Down**

Sub RiseRamp
    MetalRamp_Flipper.RotateToEnd 
    Ramp1.Collidable = 1
    RcrocoUP.Collidable = 0
    RcrocoUP1.Collidable = 0
    bRampIsUp = True
End Sub

'***********
'**ramp UP**

Sub LowerRamp
    MetalRamp_Flipper.RotateToStart
    Ramp1.Collidable = 0
    RcrocoUP.Collidable = 0
    RcrocoUP1.Collidable = 0
    bRampIsUp = False
End Sub

Sub RiseRamp1
    MetalRamp_Flipper1.RotateToEnd 
    'Ramphouse.Collidable = 1
    'Ramphouse1.Collidable = 0
    bRampIsUp1 = True
End Sub

Sub LowerRamp1
    MetalRamp_Flipper1.RotateToStart
    'Ramphouse.Collidable = 0
    'Ramphouse.Collidable = 0
    bRampIsUp1 = False
End Sub

Sub Trampup_hit
    Ramphouse.Collidable = 0
    Ramphouse1.Collidable = 0
    LowerRamp1
End Sub


'**********
'**bumper**

sub Kbumper_hit
    Kbumper.destroyball:PlaySound "fx_kicker_enter"
    Kbumper1.createball
	Timerbump.enabled=1
    AddScore 20000
end sub

sub Timerbump_timer
	Kbumper1.kick 90, 5
    PlaySound ""
    Timerbump.Enabled = 0
end sub


'***********
'**general**

    Tgeneral.isdropped = 1
    lockpostkael1.isdropped = 1
    lockpostkael2.isdropped = 1

Sub Tkael_hit()
    Lightkael1.state = 1
    checkkael
End Sub

 Sub checkkael()
 If Light13.state + Light15.state + Light16.state + Light16a.state + Light17.state + Lightkael1.state = 6 Then
    Tgeneral.isdropped = 0
    Tgeneral1.isdropped = 0
    Lightkael.state = 1
    AddScore 10000
    PlaySound ""
    End If
End Sub

Sub Tgeneral1_hit()
    Tgeneral.isdropped = 1
    Tgeneral1.isdropped = 1
    Lightkael1.state = 0
    AddScore 20000
    PlaySound ""
    AddMultiball 1
pupevent 710
end sub

sub Kenterramp_hit
    Kenterramp.destroyball:PlaySound "fx_kicker_enter"
    Kexitramp.createball
	Timerkael.enabled=1
    AddScore 5000
    LowerRamp
end sub

sub Timerkael_timer
	Kexitramp.kick 160, 10
    PlaySound ""
    Timerkael.Enabled = 0
    Texitramp.Enabled = 1
end sub

Sub Texitramp_hit()
    Texitramp.Enabled = 0
    RiseRamp
    PlayQuote
End Sub

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    ChangeBall(ChooseBall)
	spinner.MotorOn = True
    LoadEM
    Dim i
    Randomize

	'CaptiveKick.createball
	'CaptiveKick.kick 0,0

	'CaptiveKick1.createball
	'CaptiveKick1.kick 0,0

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 50 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    Set cbRight = New cvpmCaptiveBall
    With cbRight
        .InitCaptive CapTrigger1, CapWall1, Array(CapKicker1, CapKicker1a), 0
        .NailedBalls = 1
        .ForceTrans = .4
        .MinForce = 1.5
        '.CreateEvents "cbRight"
        .Start
    End With
    CapKicker1.CreateSizedBallWithMass BallSize / 2, BallMass


    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Credits = 1
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
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInLock(1) = 0
    BallsInLock(2) = 0
    BallsInLock(3) = 0
    BallsInLock(4) = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 2
    Tilted = False
    bBonusHeld = False
    bJustStarted = True
    bJackpot = False
    bInstantInfo = False
    bInstantInfo1 = False
    'bSongSelect = False
    ' set any lights for the attract mode
    GiOff
    StartAttractMode

    ' Start the RealTime timer
    RealTime.Enabled = 1

    ' Load table color
    LoadLut
	Glowball_Init 'Start Glowballs
End Sub

'******************
' Captive Ball Subs
'******************
Sub CapTrigger1_Hit:cbRight.TrigHit ActiveBall:End Sub
Sub CapTrigger1_UnHit:cbRight.TrigHit 0:End Sub
Sub CapWall1_Hit:cbRight.BallHit ActiveBall:PlaySoundAtBall "fx_collide":End Sub
Sub CapKicker1a_Hit:cbRight.BallReturn Me:End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    If keycode = LeftMagnaSave Then bLutActive = True
    If keycode = RightMagnaSave Then
        If bLutActive Then NextLUT:End If
    End If

    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        if bFreePlay = False Then DOF 125, DOFOn
        If(Tilted = False)Then
            DMDFlush
            DMD "_", CL("CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
        PlaySoundAt "fx_reload", plunger
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Table specific

    'If bsongSelect Then
        'SelectSong(keycode)
    'End If

    ' Normal flipper action


    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 1
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer1.Enabled = True:RotateLaneLights 0

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
                        If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
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
                        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)

'test keys
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If keycode = LeftMagnaSave Then bLutActive = False: LutBox.text = ""
    if keycode=35 then kicker001.createball:kicker001.kick 90,22,80 'H

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
        If bBallInPlungerLane Then PlaySoundAt "fx_plungerbig", plunger
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
            InstantInfoTimer1.Enabled = False
            If bInstantInfo1 Then
                DMDScoreNow
                bInstantInfo1 = False
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

Sub InstantInfo
    TR11.Enabled = 0
    TR12.Enabled = 0
    DMD CL("MAGNA OUT"), "", "", eNone, eNone, eNone, 800, False, ""
    DMD CL("INSTANT INFO"), "", "", eNone, eNone, eNone, 800, False, ""
    DMD CL("JACKPOT VALUE"), CL(Jackpot(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("SPINNER VALUE"), CL(spinnervalue(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("BUMPER VALUE"), CL(bumpervalue(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("BONUS X"), CL(BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("PLAYFIELD X"), CL(PlayfieldMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("LOCKED BALLS"), CL(BallsInLock(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("LANE BONUS"), CL(LaneBonus), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("TARGET BONUS"), CL(TargetBonus), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("RAMP BONUS"), CL(RampBonus), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("FOUND ITEMS"), CL(MonstersKilled(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("HIGHEST SCORE"), CL(HighScoreName(0) & " " & HighScore(0)), "", eNone, eNone, eNone, 800, False, ""
End Sub

Sub InstantInfoTimer1_Timer
    InstantInfoTimer1.Enabled = False
    If NOT hsbModeActive Then
        bInstantInfo1 = True
        DMDFlush
        InstantInfo1
    End If
End Sub

Sub InstantInfo1
 DMD CL("MAGNA IN"), "", "", eNone, eNone, eNone, 800, False, ""
 TR11.Enabled = 1
 TR12.Enabled = 1
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
        FlipperminiD.EOSTorque = 0.65:FlipperminiD.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.15:RightFlipper.RotateToStart
        FlipperminiD.EOSTorque = 0.15:FlipperminiD.RotateToStart
    End If
End Sub


' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper_1Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub FlipperminiD_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RotateLaneLights(n) 'n is the direction, 1 = left or 0 = right
    Dim tmp
    If bRotateLights Then
        If n = 1 Then
            tmp = Light20.State
            Light20.State = Light6.State
            Light6.State = Light8.State
            Light8.State = Light21.State
            Light21.State = tmp
        Else
            tmp = Light21.State
            Light21.State = Light8.State
            Light8.State = Light6.State
            Light6.State = Light20.State
            Light20.State = tmp
        End If
    End If
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt > TiltSensitivity)AND(Tilt < 15)Then 'show a warning
        DMD "_", CL("DANGER"), "_", eNone, eBlinkFast, eNone, 500, True, ""
    End if
    If Tilt > 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD "", CL("TILT"), "", eNone, eNone, eBlink, 200, False, ""
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
        'Bumper1.Force = 0

        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        'Bumper1.Force = 6
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
        TiltRecoveryTimer.Enabled = False
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
    tmp = INT(RND * 6)
    Select Case tmp
        Case 0:PlaySong "mu_battle1"
        Case 1:PlaySong "mu_battle2"
        Case 2:PlaySong "mu_battle3"
        Case 3:PlaySong "mu_battle4"
        Case 4:PlaySong "mu_battle5"
        Case 5:PlaySong "mu_battle6"
    End Select
End Sub

Sub PlayMultiballSong
    Dim tmp
    tmp = INT(RND * 4)
    Select Case tmp
        Case 0:PlaySong "mu_war1":pupevent 878:pupevent 879
        Case 1:PlaySong "mu_war2":pupevent 878:pupevent 879
        Case 2:PlaySong "mu_war3":pupevent 878:pupevent 879
        Case 3:PlaySong "mu_war4":pupevent 878:pupevent 879
    End Select
End Sub

Sub ChangeSong
    If(BallsOnPlayfield = 0)Then
        PlaySong "mu_end"
    Else
        Select Case Battle(CurrentPlayer, 0)
            Case 0
                PlaySong "mu_main"
            Case 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
                PlayBattleSong
            Case 13, 14, 15
                PlayMultiballSong
        End Select
    End If
End Sub

'********************
' Play random quotes
'********************

Sub PlayQuote
    Dim tmp
    tmp = INT(RND * 100) + 1
    PlaySound "quote_" &tmp
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
        If UBound(tmp) = 1 Then 'we have 2 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
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
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 10, 10
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
    End Select
End Sub

Sub FlashEffect(n)
    Dim ii
    Select case n
        Case 0 ' all off
            LightSeqFlasher.Play SeqAlloff
        Case 1 'all blink
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqBlinking, , 10, 10
        Case 2 'random
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqBlinking, , 5, 10
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
Const lob = 2     'number of locked balls
Const maxvel = 40 'max ball velocity
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
        aBallShadow(b).Height = BOT(b).Z -Ballsize/2

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
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
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
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

'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
   dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 80
		x.DebugOn=False ' prints some info in debugger


        x.AddPt "Polarity", 0, 0, 0
        x.AddPt "Polarity", 1, 0.05, - 2.7
        x.AddPt "Polarity", 2, 0.16, - 2.7
        x.AddPt "Polarity", 3, 0.22, - 0
        x.AddPt "Polarity", 4, 0.25, - 0
        x.AddPt "Polarity", 5, 0.3, - 1
        x.AddPt "Polarity", 6, 0.4, - 2
        x.AddPt "Polarity", 7, 0.5, - 2.7
        x.AddPt "Polarity", 8, 0.65, - 1.8
        x.AddPt "Polarity", 9, 0.75, - 0.5
        x.AddPt "Polarity", 10, 0.81, - 0.5
        x.AddPt "Polarity", 11, 0.88, 0
        x.AddPt "Polarity", 12, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.15, 0.85
		x.AddPt "Velocity", 2, 0.2, 0.9
		x.AddPt "Velocity", 3, 0.23, 0.95
		x.AddPt "Velocity", 4, 0.41, 0.95
		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
		x.AddPt "Velocity", 6, 0.62, 1.0
		x.AddPt "Velocity", 7, 0.702, 0.968
		x.AddPt "Velocity", 8, 0.95,  0.968
		x.AddPt "Velocity", 9, 1.03,  0.945
		x.AddPt "Velocity", 10, 1.5,  0.945

	Next

	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
    LF.SetObjects "LF", LeftFlipper, TriggerLF
    RF.SetObjects "RF", RightFlipper, TriggerRF
    RF.SetObjects "RF", FlipperminiD, TriggerRFminiD
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

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
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
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

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
        LowerRamp1
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
    recordspin.enabled = 0
    Rsaveball.Collidable = 1
    Lightsave.state = 0

    ' set the current players bonus multiplier back down to 1X
    SetBonusMultiplier 1

    ' reduce the playfield multiplier
    ' reset any drop targets, lights, game Mode etc..

    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False

    'Reset any table specific
    ResetNewBallVariables
    ResetNewBallLights()

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
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4 : pupevent 811:pupevent 812
    lifeclose
    'StopBattle
    'SelectBattle

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
    TotalBonus = 0
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False
pupevent 800
pupevent 801

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

    If NOT Tilted Then

'add in any bonus points (multipled by the bonus multiplier)
'AwardPoints = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer)
'AddScore AwardPoints
'debug.print "Bonus Points = " & AwardPoints
'DMD "", CL("BONUS: " & BonusPoints(CurrentPlayer) & " X" & BonusMultiplier(CurrentPlayer) ), "", eNone, eBlink, eNone, 1000, True, ""

'Count the bonus. This table uses several bonus
'Lane Bonus
        AwardPoints = LaneBonus * 1000
        TotalBonus = AwardPoints
        DMD CL(FormatScore(AwardPoints)), CL("LANE BONUS " & LaneBonus), "", eBlink, eNone, eNone, 800, False, ""

        'Number of Target hits
        AwardPoints = TargetBonus * 2000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(FormatScore(AwardPoints)), CL("TARGET BONUS " & TargetBonus), "", eBlink, eNone, eNone, 800, False, ""

        'Number of Ramps completed
        AwardPoints = RampBonus * 10000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(FormatScore(AwardPoints)), CL("RAMP BONUS " & RampBonus), "", eBlink, eNone, eNone, 800, False, ""

        'Number of Monsters Killed
        AwardPoints = MonstersKilled(CurrentPlayer) * 25000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(FormatScore(AwardPoints)), CL("" & MonstersKilled(CurrentPlayer)), "", eBlink, eNone, eNone, 800, False, ""

        ' calculate the totalbonus
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer) + BonusHeldPoints(CurrentPlayer)

        ' handle the bonus held
        ' reset the bonus held value since it has been already added to the bonus
        BonusHeldPoints(CurrentPlayer) = 0

        ' the player has won the bonus held award so do something with it :)
        If bBonusHeld Then
            If Balls = BallsPerGame Then ' this is the last ball, so if bonus held has been awarded then double the bonus
                TotalBonus = TotalBonus * 2
            End If
        Else ' this is not the last ball so save the bonus for the next ball
            BonusHeldPoints(CurrentPlayer) = TotalBonus
        End If
        bBonusHeld = False

        ' Add the bonus to the score
        DMD CL(FormatScore(TotalBonus)), CL("TOTAL BONUS " & " X" & BonusMultiplier(CurrentPlayer)), "", eBlinkFast, eNone, eNone, 1500, True, ""

        AddScore TotalBonus

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 3300, "EndOfBall2 '"
    Else 'if tilted then only add a short delay
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
            LightShootAgaina.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, ""

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
        EndOfGame() :Playsound "gameover"
pupevent 883
      
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
            PlaySound "vo_player" &CurrentPlayer
            DMD "_", CL("PLAYER " &CurrentPlayer), "_", eNone, eNone, eNone, 800, True, ""
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
        ChangeSong
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
pupevent 813
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
    Drain.DestroyBall
    'ChangeBall(0)
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
            DMD "_", CL("BALL SAVED"), "_", eNone, eBlinkfast, eNone, 800, True, "":pupevent 884
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
pupevent 720
pupevent 721
pupevent 722
pupevent 723
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
                    ResetJackpotLights
                    Select Case Battle(CurrentPlayer, 0)
                        Case 13, 14, 15:WinBattle
                    End Select
                    ChangeGi white
                    ChangeSong
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
                ' End Mode and timers
                ChangeSong
                ChangeGi white
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
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
        PlungerIM.AutoFire
        DOF 117, DOFPulse
        PlaySoundAt "", swPlungerRest
        bAutoPlunger = False
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
        EnableBallSaver BallSaverTime
    Else
        ' show the message to shoot the ball in case the player has fallen sleep
        swPlungerRest.TimerEnabled = 1
    End If
    'Start the Selection of the skillshot if ready
    If bSkillShotReady Then
        UpdateSkillshot()
        UpdateSkillshot1()
        'bSongSelect = True
        'vpmtimer.addtimer 2000, "UpdateDMDSong '"
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
    'bsongSelect = False
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
        ResetSkillShotTimer.Enabled = 1
        ResetSkillShotTimer1.Enabled = 1
    End If
    ChangeSong
' turn off LaunchLight
' LaunchLight.State = 0
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds

Sub swPlungerRest_Timer
    DMD "_", CL("SHOOT THE BALL"), "_", eNone, eNone, eNone, 800, True, ""
    swPlungerRest.TimerEnabled = 0
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
    LightShootAgaina.BlinkInterval = 160
    LightShootAgain.State = 2
    LightShootAgaina.State = 2
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
    LightShootAgaina.State = 0
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
    LightShootAgaina.BlinkInterval = 80
    LightShootAgaina.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
' In this table we use SecondRound variable to double the score points in the second round after killing Malthael
Sub AddScore(points)
    If(Tilted = False)Then
        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
    End if
' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If(Tilted = False)Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    End if
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If(Tilted = False)Then

        ' If(bMultiBallMode = True) Then
        Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
        DMD "_", CL("INCREASED JACKPOT"), "_", eNone, eNone, eNone, 800, True, ""
    ' you may wish to limit the jackpot to a upper limit, ie..
    '	If (Jackpot >= 6000) Then
    '		Jackpot = 6000
    ' 	End if
    'End if
    End if
End Sub

Sub AddSuperJackpot(points) 'not used in this table
    If(Tilted = False)Then
    End if
End Sub

Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxMultiplier)then
        ' then add and set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
        SetBonusMultiplier(NewBonusLevel)
        DMD "_", CL("BONUS X " &NewBonusLevel), "_", eNone, eNone, eNone, 2000, True, "fx_bonus"
    Else
        AddScore 50000
        DMD "_", CL("50000"), "_", eNone, eNone, eNone, 800, True, ""
    End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly

Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level
    UPdateBonusXLights(Level)
End Sub

Sub UpdateBonusXLights(Level)
    ' Update the lights
    Select Case Level
        Case 1:light54.State = 0:light55.State = 0:light56.State = 0:light57.State = 0
        Case 2:light54.State = 1:light55.State = 0:light56.State = 0:light57.State = 0
        Case 3:light54.State = 0:light55.State = 1:light56.State = 0:light57.State = 0
        Case 4:light54.State = 0:light55.State = 0:light56.State = 1:light57.State = 0
        Case 5:light54.State = 0:light55.State = 0:light56.State = 0:light57.State = 1
    End Select
End Sub

Sub AddPlayfieldMultiplier(n)
    Dim NewPFLevel
    ' if not at the maximum level x
    if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier)then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
        SetPlayfieldMultiplier(NewPFLevel)
        DMD "_", CL("PLAYFIELD X " &NewPFLevel), "_", eNone, eNone, eNone, 2000, True, "fx_bonus"
    Else 'if the 5x is already lit
        AddScore 50000
        DMD "_", CL("50000"), "_", eNone, eNone, eNone, 2000, True, ""
    End if
    'Start the timer to reduce the playfield x every 30 seconds
    pfxtimer.Enabled = 0
    pfxtimer.Enabled = 1
End Sub

' Set the Playfield Multiplier to the specified level AND set any lights accordingly

Sub SetPlayfieldMultiplier(Level)
    ' Set the multiplier to the specified level
    PlayfieldMultiplier(CurrentPlayer) = Level
    UpdatePFXLights(Level)
End Sub

Sub UpdatePFXLights(Level)
    ' Update the lights
    Select Case Level
        Case 1:light3.State = 0:light2.State = 0:light1.State = 0:light4.State = 0
        Case 2:light3.State = 1:light2.State = 0:light1.State = 0:light4.State = 0
        Case 3:light3.State = 0:light2.State = 1:light1.State = 0:light4.State = 0
        Case 4:light3.State = 0:light2.State = 0:light1.State = 1:light4.State = 0
        Case 5:light3.State = 0:light2.State = 0:light1.State = 0:light4.State = 1
    End Select
' show the multiplier in the DMD
' in this table the multiplier is always shown in the score display sub
End Sub

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        DMD "_", CL(("EXTRA BALL WON")), "_", eNone, eBlink, eNone, 1500, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker):pupevent 885
        DOF 121, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        LightShootAgain.State = 1 'light the shoot again lamp
        LightShootAgaina.State = 1 'light the shoot again lamp
        GiEffect 2
        LightEffect 2
    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL(("EXTRA GAME WON")), "_", eNone, eBlink, eNone, 1500, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker):pupevent 886
    DOF 121, DOFPulse
    Credits = Credits + 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
    FlashEffect 2
End Sub

Sub AwardJackpot() 'award a normal jackpot, double or triple jackpot
    Dim tmp
    DMD CL(FormatScore(Jackpot(CurrentPlayer))), CL("JACKPOT"), "d_border", eBlinkFast, eBlinkFast, eNone, 1000, True, "":pupevent 887
    DOF 126, DOFPulse
    tmp = INT(RND * 2)
    Select Case tmp
        Case 0:PlaySound "vo_Jackpot"
        Case 0:PlaySound "vo_Jackpot2"
        Case 0:PlaySound "vo_Jackpot3"
    End Select
    AddScore Jackpot(CurrentPlayer)
    LightEffect 2
    FlashEffect 2
    'sjekk for superjackpot
    EnableSuperJackpot
End Sub

Sub AwardSuperJackpot() 'this is actually 4 times a jackpot
    SuperJackpot = Jackpot(CurrentPlayer) * 4
    DMD CL(FormatScore(SuperJackpot)), CL("SUPER JACKPOT"), "d_border", eBlinkFast, eBlinkFast, eNone, 1500, True, "vo_superjackpot":pupevent 888
    DOF 126, DOFPulse
    AddScore SuperJackpot
    LightEffect 2
    FlashEffect 2
    'enabled jackpots again
    StartJackpots
End Sub

Sub AwardSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL(FormatScore(SkillshotValue(CurrentPlayer))), CL(("SKILLSHOT")), "d_border", eBlinkFast, eBlink, eNone, 1000, True, "":pupevent 889
    DOF 127, DOFPulse
    PlaySound "fx_fanfare2"
    Addscore SkillShotValue(CurrentPlayer)
    ' increment the skillshot value with 250.000
    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 250000
    'do some light show
    GiEffect 2
    LightEffect 2
End Sub

Sub AwardSkillshot1()
    ResetSkillShotTimer1_Timer
    'show dmd animation
    DMD CL(FormatScore(SkillshotValue1(CurrentPlayer))), CL(("SUPER SKILLSHOT")), "d_border", eBlinkFast, eBlink, eNone, 1000, True, ""
    DOF 127, DOFPulse
    PlaySound "fx_fanfare2"
    Addscore SkillShotValue1(CurrentPlayer)
    ' increment the skillshot value with 250.000
    SkillShotValue1(CurrentPlayer) = SkillShotValue1(CurrentPlayer) + 500000
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
    HighScore(0) = 100000
    HighScore(1) = 100000
    HighScore(2) = 100000
    HighScore(3) = 100000
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
    'PlaySound "vo_greatscore" &RndNbr(6)
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
    Dim tmp, tmp1, tmp2
    if(dqHead = dqTail)Then
        tmp = RL(FormatScore(Score(Currentplayer)))
        'tmp = CL(FormatScore(Score(Currentplayer) ) )
        'tmp1 = CL("PLAYER " & CurrentPlayer & " BALL " & Balls)
        'tmp1 = FormatScore(Bonuspoints(Currentplayer) ) & " X" &BonusMultiplier(Currentplayer)

        Select Case Battle(CurrentPlayer, 0)
            Case 0:tmp1 = CL("PLAYER " & CurrentPlayer & " BALL " & Balls)
            Case 1:tmp1 = CL("HIT SPINNERS " & 50-SpinCount)
            Case 2:tmp1 = CL("HIT THE BUMPERS " & 10-SuperBumperHIts)
            Case 3:tmp1 = CL("HIT THE RAMPS " & 6-ramphits3)
            Case 4:tmp1 = CL("HIT ORBIT LANE " & 6-orbithits)
            Case 5:tmp1 = CL("CHASE THE LIGHTS")
            Case 6:tmp1 = CL("FOLLOWS THE LIGHTS ")
            Case 7:tmp1 = CL("HIT BLUE LIGHT " & 20-TargetHits7)
            Case 8:tmp1 = CL("HIT LANES " & 10-TargetHits8)
            Case 9:tmp1 = CL("TARGET YELLOW " & 5-CaptiveBallHits)
            Case 10:tmp1 = CL("HIT RAMPS-LANE " & 6-RampHits10)
            Case 11:tmp1 = CL("HIT BLUE TARGETS " & 10-TargetHits11)
            Case 12:tmp1 = CL("HIT THE LOOPS " & 5-loopCount)
            Case 13:tmp1 = CL("HIT THE LIT LIGHT")
            Case 14:tmp1 = CL("HIT THE LIT LIGHT")
            Case 15:tmp1 = CL("WILLOW" & "FINAL" & Balls)
        End Select
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
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "d_bkempty"
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

'********************
' Real Time updatess
'********************
'used for all the real time updates

Sub RealTime_Timer
    RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
    RcrocoUP.HeightBottom = MetalRamp_Flipper.CurrentAngle
    RcrocoUP1.HeightBottom = MetalRamp_Flipper.CurrentAngle
    'Ramphouse.HeightBottom = MetalRamp_Flipper1.CurrentAngle
    'Ramphouse1.HeightBottom = MetalRamp_Flipper1.CurrentAngle
    'boat.RotX = - boatR.CurrentAngle - 40
    'RcrocoUPguideD.HeightBottom = MetalRamp_Flipper.CurrentAngle
    'RcrocoUPguideG.HeightBottom = MetalRamp_Flipper.CurrentAngle
End Sub

Sub Opencar
    boatR.RotateToEnd
End Sub

Sub Closecar
    boatR.RotateToStart
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

Sub StartRainbow(n)
    set RainbowLights = n
    RGBStep = 0
    RGBFactor = 5
    rRed = 255
    rGreen = 0
    rBlue = 0
    RainbowTimer.Enabled = 1
End Sub

Sub StopRainbow()
    Dim obj
    RainbowTimer.Enabled = 0
    RainbowTimer.Enabled = 0
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
        DMD CL("LAST SCORE"), CL("PLAYER 1 " &FormatScore(Score(1))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2)Then
        DMD CL("LAST SCORE"), CL("PLAYER 2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3)Then
        DMD CL("LAST SCORE"), CL("PLAYER 3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4)Then
        DMD CL("LAST SCORE"), CL("PLAYER 4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 3000, False, ""
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
    DMD "          MARTY", "         PRESENTS", "d_jppresents", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 4000, False, ""
    DMD "", CL("GOOD QUEST" &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("HIGHSCORES"), Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL("HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL("HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub
Sub StartAttractMode
    ChangeSong
    StartLightSeq
    DMDFlush
    ShowTableInfo
End Sub

Sub StopAttractMode
    DMDScoreNow
    LightSeqAttract.StopPlay
    LightSeqFlasher.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqFlasher.UpdateInterval = 150
    LightSeqFlasher.Play SeqRandom, 10, , 50000
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

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqAllOff
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

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 15:UpdateLUT:SaveLUT:Lutbox.text = "level of darkness " & LUTImage:End Sub

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

Dim GiIntensity
GiIntensity = 1               'used for the LUT changing to increase the GI lights when the table is darker

Sub ChangeGIIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGILights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
Dim LaneBonus
Dim TargetBonus
Dim RampBonus
Dim BumperValue(4)
Dim BumperHits
Dim SuperBumperHits
Dim SpinnerValue(4)
Dim MonstersKilled(4)
Dim SpinCount
Dim RampHits3
Dim RampHits10
Dim OrbitHits
Dim TargetHits7
Dim TargetHits8
Dim TargetHits11
Dim CaptiveBallHits
Dim loopCount
Dim BattlesWon(4)
Dim Battle(4, 15) '12 battles, 2 wizard modes, 1 final battle
Dim NewBattle
Dim MachineGunHits
Dim Beach

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    bRotateLights = True
    'Play some Music
    'ChangeSong
    'Init Variables
    LaneBonus = 0 'it gets deleted when a new ball is launched
    TargetBonus = 0
    RampBonus = 0
    BumperHits = 0
    For i = 1 to 4
        SkillshotValue(i) = 250000
        SkillshotValue1(i) = 500000
        Jackpot(i) = 100000
        MonstersKilled(i) = 0
        BallsInLock(i) = 0
        SpinnerValue(i) = 1000
        BumperValue(i) = 210 'start at 210 and every 30 hits its value is increased by 500 points
    Next
    ResetBattles
'Init Delays/Timers
'MainMode Init()
'Init lights
End Sub

Sub StopEndOfBallMode() 'this sub is called after the last ball is drained
    ResetSkillShotTimer_Timer
    ResetSkillShotTimer1_Timer
    StopBattle
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
    Dim i
    LaneBonus = 0
    TargetBonus = 0
    RampBonus = 0
    BumperHits = 0
    RiseRamp
    RiseRamp1
    ' select a battle
    SelectBattle
End Sub

Sub ResetNewBallLights()                                'turn on or off the needed lights before a new ball is released
    UpdatePFXLights(PlayfieldMultiplier(CurrentPlayer)) 'ensure the multiplier is displayed right
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
    LightSeqSkillshot.Play SeqAllOff
    Light21.State = 2
    Light29.State = 2
    Gate2.Open = 1
    Gate3.Open = 1
    DMD CL("HIT LIT LIGHT"), CL("FOR SKILLSHOT"), "", eNone, eNone, eNone, 1500, True, ""
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    LightSeqSkillshot.StopPlay
    If Light21.State = 2 Then Light21.State = 0
    Light29.State = 0
    Gate2.Open = 0
    Gate3.Open = 0
    DMDScoreNow
End Sub

Sub UpdateSkillShot1() 'Setup and updates the skillshot lights
    LightSeqSkillshot.Play SeqAllOff
    Light21.State = 2
    'Light29.State = 2
    Gate2.Open = 1
    Gate3.Open = 1
    DMD CL("HIT LIT LIGHT"), CL("FOR SKILLSHOT"), "", eNone, eNone, eNone, 1500, True, ""
End Sub

Sub ResetSkillShotTimer1_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer1.Enabled = 0
    bSkillShotReady = False
    LightSeqSkillshot.StopPlay
    If Light21.State = 2 Then Light21.State = 0
    'Light29.State = 0
    Gate2.Open = 0
    Gate3.Open = 0
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

' Car animation

Dim MyPi, RcarStep, RcarDir, BoatStep, BoatDir
MyPi = Round(4 * Atn(1), 6) / 90
RcarStep = 0
BoatStep = 0

 Rcar.Enabled = 0

Sub Rcar_Timer()
    RcarDir = SIN(RcarStep * MyPi)
    RcarStep = (RcarStep + 1)MOD 100
     'mur.RotZ = RcarDir *90
     'Car.RotY = - RcarDir *-90
End Sub

BoatTimer.Enabled = 0

Sub BoatTimer_Timer()
    BoatDir = SIN(BoatStep * MyPi)
    BoatStep = (BoatStep + 1)MOD 360
    'heli.Y = heli.Y + BoatDir * 6
    'helice.Y = helice.Y + BoatDir * 6
End Sub


'*********************************************************
' Slingshots has been hit
' In this table the slingshots change the outlanes lights

Dim LStep, RStep, LStepht

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFcontactors), Lemk
    DOF 105, DOFPulse
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 210
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
    ChangeOutlanes
    ShakecouteauG
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 104, DOFPulse, DOFcontactors), Remk
    DOF 106, DOFPulse
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 210
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
    ChangeOutlanes
    ShakecouteauD
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub ChangeOutlanes
    Dim tmp
    tmp = light5.State
    light5.State = light9.State
    light9.State = tmp
End Sub

'*********
' Bumpers
'*********
' after each 30 hits the bumpers increase their score value by 500 points up to 3210
' and they increase the playfield multiplier. The playfield multiplier is reduced after every 30 seconds

Sub Bumper1_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper1
        DOF 138, DOFPulse
        FlashForMs FbumpD, 500, 50, 0
        FlashForMs FbumpG, 500, 50, 0
        ' add some points
        AddScore BumperValue(CurrentPlayer)
        If Battle(CurrentPlayer, 0) = 2 Then
            SuperBumperHits = SuperBumperHits + 1
            Addscore 5000
            CheckWinBattle
        End If
        ' remember last trigger hit by the ball
        LastSwitchHit = "Bumper1"
    End If
    CheckBumpers
    Movecup1
    FlashForMs Fspot, 1000, 50, 0
    FlashForMs Fspot1, 1000, 50, 0
End Sub

Sub Bumper2_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper1
        DOF 138, DOFPulse
        FlashForMs FbumpD, 500, 50, 0
        FlashForMs FbumpG, 500, 50, 0
        ' add some points
        AddScore BumperValue(CurrentPlayer)
        If Battle(CurrentPlayer, 0) = 2 Then
            SuperBumperHits = SuperBumperHits + 1
            Addscore 5000
            CheckWinBattle
        End If
        ' remember last trigger hit by the ball
        LastSwitchHit = "Bumper2"
    End If
    CheckBumpers
    Movecup2
    FlashForMs Fspot, 1000, 50, 0
    FlashForMs Fspot1, 1000, 50, 0
End Sub

Sub Bumper3_Hit
    If NOT Tilted Then
        PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), Bumper1
        DOF 138, DOFPulse
        FlashForMs FbumpD, 500, 50, 0
        FlashForMs FbumpG, 500, 50, 0
        ' add some points
        AddScore BumperValue(CurrentPlayer)
        If Battle(CurrentPlayer, 0) = 2 Then
            SuperBumperHits = SuperBumperHits + 1
            Addscore 5000
            CheckWinBattle
        End If
        ' remember last trigger hit by the ball
        LastSwitchHit = "Bumper3"
    End If
    CheckBumpers
    Movecup3
    FlashForMs Fspot, 1000, 50, 0
    FlashForMs Fspot1, 1000, 50, 0
End Sub


' Check the bumper hits

Sub CheckBumpers()
    ' increase the bumper hit count and increase the bumper value after each 30 hits
    BumperHits = BumperHits + 1
    If BumperHits MOD 30 = 0 Then
        If BumperValue(CurrentPlayer) < 3210 Then
            BumperValue(CurrentPlayer) = BumperValue(CurrentPlayer) + 500
        End If
        ' lit the playfield multiplier light
        light53.State = 1
    End If
End Sub

' Move Cup
Dim Cup1Pos, Cup2Pos, Cup3Pos, Cup4Pos, Cup5Pos

Sub Movecup1
    Cup1Pos = 6
    Cup1Timer.Enabled = 1
End Sub

Sub Cup1Timer_Timer
    bumperlight1.state = 1
    cup1.TransY = Cup1Pos
    cage.TransY = Cup1Pos
    mad.TransY = Cup1Pos
    If Cup1Pos = 0 Then Me.Enabled = 0:Exit Sub
    If Cup1Pos < 0 Then
        Cup1Pos = ABS(Cup1Pos)- 1
    Else
        Cup1Pos = - Cup1Pos + 1
    End If
End Sub

Sub Movecup2
    Cup2Pos = 6
    Cup2Timer.Enabled = 1
End Sub

Sub Cup2Timer_Timer
    bumperlight2.state = 1
    Cup2.TransY = Cup2Pos
    cage.TransY = Cup2Pos
    mad.TransY = Cup1Pos
    If Cup2Pos = 0 Then Me.Enabled = 0:Exit Sub
    If Cup2Pos < 0 Then
        Cup2Pos = ABS(Cup2Pos)- 1
    Else
        Cup2Pos = - Cup2Pos + 1
    End If
End Sub

Sub Movecup3
    Cup3Pos = 6
    Cup3Timer.Enabled = 1
End Sub

Sub Cup3Timer_Timer
    bumperlight3.state = 1
    Cup3.TransY = Cup3Pos
    cage.TransY = Cup3Pos
    mad.TransY = Cup1Pos
    If Cup3Pos = 0 Then Me.Enabled = 0:Exit Sub
    If Cup3Pos < 0 Then
        Cup3Pos = ABS(Cup3Pos)- 1
    Else
        Cup3Pos = - Cup3Pos + 1
    End If
End Sub

Sub Movecup4
    Cup4Pos = 6
    Cup4Timer.Enabled = 1
End Sub

Sub Cup4Timer_Timer
    shark.TransY = Cup4Pos
    If Cup4Pos = 0 Then Me.Enabled = 0:Exit Sub
    If Cup4Pos < 0 Then
        Cup4Pos = ABS(Cup4Pos)- 1
    Else
        Cup4Pos = - Cup4Pos + 1
    End If
End Sub

Sub Movecup5
    Cup5Pos = 6
    Cup5Timer.Enabled = 1
End Sub

Sub Cup5Timer_Timer
    shark.TransY = Cup5Pos
    'cage.TransY = Cup5Pos
    plongeur.TransY = Cup5Pos
    If Cup5Pos = 0 Then Me.Enabled = 0:Exit Sub
    If Cup5Pos < 0 Then
        Cup5Pos = ABS(Cup5Pos)- 1
    Else
        Cup5Pos = - Cup5Pos + 1
    End If
End Sub



'*************************
' Top & Inlanes: Bonus X
'*************************
' lit the 2 top lane lights and the 2 inlane lights to increase the bonus multiplier

Sub sw8_Hit
    DOF 128, DOFPulse
    PlaySoundAtBall "fx_sensor"
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    Light20.State = 1
    FlashForMs f5, 1000, 50, 0
    FlashForMs Fspot1, 1000, 50, 0
    If bSkillShotReady Then
        ResetSkillShotTimer_Timer
        ResetSkillShotTimer1_Timer
    Else
        CheckBonusX
    End If
End Sub

Sub sw9_Hit
    DOF 129, DOFPulse
    PlaySoundAtBall "fx_sensor"
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    Light21.State = 1
    FlashForMs f5, 1000, 50, 0
    FlashForMs Fspot1, 1000, 50, 0
    If bSkillShotReady Then
        AwardSkillshot1
    Else
        CheckBonusX
    End If
    PlayQuote
End Sub

Sub sw2_Hit
    DOF 133, DOFPulse
    PlaySoundAtBall "fx_sensor"
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    Light6.State = 1
    FlashForMs f8, 1000, 50, 0
    FlashForMs Fspot1, 1000, 50, 0
    AddScore 5000
    If bSkillShotReady Then
        ResetSkillShotTimer_Timer
        ResetSkillShotTimer1_Timer
    Else
        CheckBonusX
    End If
'do something
' Do some sound or light effect
End Sub

Sub sw3_Hit
    DOF 134, DOFPulse
    PlaySoundAtBall "fx_sensor"
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    Light8.State = 1
    FlashForMs f9, 1000, 50, 0
    FlashForMs Fspot1, 1000, 50, 0
    AddScore 5000
    If bSkillShotReady Then
        ResetSkillShotTimer_Timer
        ResetSkillShotTimer1_Timer
    Else
        CheckBonusX
    End If
'do something
' Do some sound or light effect
End Sub


Sub CheckBonusX
    If Light20.State + Light21.State + Light6.State + Light8.State  = 4 Then
        AddBonusMultiplier 1
        GiEffect 1
        FlashForMs Light20, 1000, 50, 0
        FlashForMs Light21, 1000, 50, 0
        FlashForMs Light6, 1000, 50, 0
        FlashForMs Light8, 1000, 50, 0
    End IF
End Sub

'************************************
' Flipper OutLanes: Virtual kickback
'************************************
' if the light is lit then activate the ballsave

Sub sw1_Hit
    DOF 132, DOFPulse
    PlaySoundAtBall "fx_sensor"
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    AddScore 50000
    ' Do some sound or light effect
    ' do some check
    If light5.State = 1 Then
        EnableBallSaver 5
    End If
End Sub

Sub sw4_Hit
    DOF 135, DOFPulse
    PlaySoundAtBall "fx_sensor"
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    AddScore 50000
    ' Do some sound or light effect
    ' do some check
    If Light9.State = 1 Then
        EnableBallSaver 5
    End If
End Sub

'*******************************
' 3Bank Targets: Jungle Targets
'*******************************

Sub Target1_Hit
    PlaySoundAt SoundFXDOF("fx_sensor", 115, DOFPulse, DOFTargets), Target1
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    PlayQuote
    ' Do some sound or light effect
    Light12.State = 1
    FlashForMs f7, 1000, 50, 0
    ' do some check
    Check3BankTargets
    Select Case Battle(CurrentPlayer, 0)
        Case 5
            If Light44.State = 2 Then
                Light44.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light44.State = 2 Then
                Light50.State = 2
                Light44.State = 0
                Addscore 100000
            End If
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
        Case 8:TargetHits8 = TargetHits8 + 1:Addscore 25000:CheckWinBattle
        Case 13
            If Light44.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light44.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target1"
End Sub

Sub Target2_Hit
    PlaySoundAt SoundFXDOF("fx_sensor", 115, DOFPulse, DOFTargets), Target2
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    ' Do some sound or light effect
    Light13.State = 1
    FlashForMs f7, 1000, 50, 0
    ' do some check
    Check3BankTargets
checkkael
    Select Case Battle(CurrentPlayer, 0)
        Case 5
            If Light44.State = 2 Then
                Light44.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light44.State = 2 Then
                Light44.State = 0
                Light50.State = 2
                Addscore 100000
            End If
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
        Case 8:TargetHits8 = TargetHits8 + 1:Addscore 25000:CheckWinBattle
        Case 13
            If Light44.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light44.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target2"
End Sub

Sub Target3_Hit
    PlaySoundAt SoundFXDOF("fx_sensor", 115, DOFPulse, DOFTargets), Target3
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    ' Do some sound or light effect
    Light14.State = 1
    FlashForMs f7, 1000, 50, 0
    ' do some check
    Check3BankTargets
    Select Case Battle(CurrentPlayer, 0)
        Case 5
            If Light44.State = 2 Then
                Light44.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light44.State = 2 Then
                Light50.State = 2
                Light44.State = 0
                Addscore 100000
            End If
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
        Case 8:TargetHits8 = TargetHits8 + 1:Addscore 25000:CheckWinBattle
        Case 13
            If Light44.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light44.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target3"
End Sub

Sub Check3BankTargets
     If light12.state + light13.state + light14.state = 3 Then
        light12.state = 0
        light13.state = 0
        light14.state = 0
        Targetsave.collidable = 1: PlaySoundAt "Fx_rebond", Targetsave
        Lightsave1.state = 2
        Addscore 50000 
        LightEffect 1
        FlashEffect 2
        Addscore 30000
        If Light5.State = 0 AND Light9.State = 0 Then 'light one of the outlanes lights
            Light5.State = 1
        Else
            Addscore 20000
        End If
        ' increment the spinner value
        spinnervalue(CurrentPlayer) = spinnervalue(CurrentPlayer) + 500
    End If
End Sub

Sub Targetsave_hit
    PlaySound "Fx_bell"
    Targetsave.collidable = 0
    Lightsave.state = 1
    Lightsave1.state = 0
    Rsaveball.Collidable = 0
    Tclosesave.enabled = 0
    Addscore 20000
End Sub


'************
'  Spinners
'************

Sub spinner1_Spin
    If Tilted Then Exit Sub
    Addscore spinnervalue(CurrentPlayer)
    PlaySoundAt "fx_spinner", spinner1
    'DOF 136, DOFPulse
    Select Case Battle(CurrentPlayer, 0)
        Case 1
            Addscore 3000
            SpinCount = SpinCount + 1
            CheckWinBattle
    End Select
End Sub

Sub spinner2_Spin
    If Tilted Then Exit Sub
    PlaySoundAt "fx_spinner", spinner2
    'DOF 124, DOFPulse
    Addscore spinnervalue(CurrentPlayer)
    Select Case Battle(CurrentPlayer, 0)
        Case 1
            Addscore 3000
            SpinCount = SpinCount + 1
            CheckWinBattle
    End Select
End Sub

Sub spinner3_Spin
    If Tilted Then Exit Sub
    PlaySoundAt "fx_spinner", spinner2
    'DOF 124, DOFPulse
    Addscore spinnervalue(CurrentPlayer)
    Select Case Battle(CurrentPlayer, 0)
        Case 1
            Addscore 3000
            SpinCount = SpinCount + 1
            CheckWinBattle
    End Select
End Sub

'*********************************
' 2Bank targets: The Lock Targets
'*********************************

Sub Target10_Hit
    PlaySoundAt SoundFXDOF("Target_Hit_9", 116, DOFPulse, DOFTargets), Target10
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    ' Do some sound or light effect
    Light11.State = 1
    FlashForMs f6, 1000, 50, 0
    FlashForMs FvertD, 1000, 50, 0
    FlashForMs FvertG, 1000, 50, 0
    ' do some check
    Check2BankTargets
    Select Case Battle(CurrentPlayer, 0)
        Case 5
            If Light50.State = 2 Then
                Light50.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light50.State = 2 Then
                Light46.State = 2
                Light50.State = 0
                Addscore 100000
            End If
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
        Case 8:TargetHits8 = TargetHits8 + 1:Addscore 25000:CheckWinBattle
        Case 13
            If Light50.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light50.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target10"
End Sub

Sub Target11_Hit
    PlaySoundAt SoundFXDOF("Target_Hit_8", 116, DOFPulse, DOFTargets), Target11
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    ' Do some sound or light effect
    Light10.State = 1
    FlashForMs f6, 1000, 50, 0
    FlashForMs FvertD, 1000, 50, 0
    FlashForMs FvertG, 1000, 50, 0
    ' do some check
    Check2BankTargets
    Select Case Battle(CurrentPlayer, 0)
        Case 5
            If Light50.State = 2 Then
                Light50.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light50.State = 2 Then
                Light46.State = 2
                Light50.State = 0
                Addscore 100000
            End If
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
        Case 8:TargetHits8 = TargetHits8 + 1:Addscore 25000:CheckWinBattle
        Case 13
            If Light50.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light50.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target11"
End Sub

Sub Check2BankTargets
    If light11.state + light10.state = 2 Then
        light11.state = 0
        light10.state = 0
        LightEffect 1
        FlashEffect 1
        Addscore 20000
        If(Light27.State = 0)AND(bMultiballMode = FALSE)Then 'lit the lock light if it is off, rise the lock post and activate the lock switch
            Light27.State = 1
            'PlaySound "vo_lockislit"
            DMD "_", CL("LOCK IS LIT"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_lockislit"
        ElseIf light52.State = 0 Then 'lit the increase jackpot light if the lock light is lit
            light52.State = 1
        'PlaySound "vo_IncreaseJakpot"
        Else
            Addscore 30000
        End If
    End If
End Sub

'**************************
' The Lock: Main Multiball
'**************************
' the lock is a virtual lock, where the locked balls are simply counted

Sub lock_Hit
    Dim delay
    delay = 500
    PlaySoundAt "fx_kicker_enter", lock
    If light27.State = 1 Then 'lock the ball
        BallsInLock(CurrentPlayer) = BallsInLock(CurrentPlayer) + 1
        delay = 3000
        Select Case BallsInLock(CurrentPlayer)
            Case 1:DMD "_", CL("BALL 1 LOCKED"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_oneballlock" : Lock1.State = 1: pupevent 906
            Case 2:DMD "_", CL("BALL 2 LOCKED"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_twoballlock" : Lock2.State = 1: pupevent 907
            Case 3:DMD "_", CL("BALL 3 LOCKED"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_threeballlock" : Lock3.State = 1: pupevent 908
        End Select
        light27.State = 0
        If BallsInLock(CurrentPlayer) = 3 Then 'start multiball
            vpmtimer.addtimer 2000, "StartMainMultiball '":pupevent 700
        End If
    End If
    If(Battle(CurrentPlayer, NewBattle) = 2)AND(Battle(CurrentPlayer, 0) = 0)Then 'the battle is ready, so start it
        vpmtimer.addtimer 2000, "StartBattle '"
        delay = 6500
    End If
    vpmtimer.addtimer delay, "ReleaseLockedBall '"
End Sub

Sub ReleaseLockedBall 'release locked ball
    FlashForMs f6, 1000, 50, 0
    'lockpost.isdropped = 1:PlaySoundAt SoundFXDOF("fx_solenoid", 142, DOFPulse, DOFContactors), lock
    DOF 121, DOFPulse
    lock.kick 200, 12
    PlaySoundAt "fx_clock", lock
    FlashForMs f10, 1000, 50, 0
    FlashForMs f10a, 1000, 50, 0
    FlashForMs f10a1, 1000, 50, 0
    FlashForMs f10a2, 1000, 50, 0
    FlashForMs f10a3, 1000, 50, 0
    FlashForMs f10a4, 1000, 50, 0
    FlashForMs f10a5, 1000, 50, 0
    'lockpost.TimerInterval = 400
    'lockpost.TimerEnabled = 1
End Sub

Sub lockpost_Timer
    lockpost.TimerEnabled = 0
    lockpost.isdropped = 1
End Sub

Sub StartMainMultiball
    AddMultiball 3
    DMD "_", CL("MULTIBALL"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_multiball" : Lock1.State = 0 : Lock2.State = 0 : Lock3.State = 0
    pupevent 701
    StartJackpots
    ChangeGi blue
    'reset BallsInLock variable
    BallsInLock(CurrentPlayer) = 0
End Sub

'**********
' Jackpots
'**********
' Jackpots are enabled during the Main multiball and the wizard battles

Sub StartJackpots
    bJackpot = true
    'turn on the jackpot lights
    Select Case Battle(CurrentPlayer, 0)
        Case 13 'first wizard mode
pupevent 712
            light28.State = 2
            light30.State = 2
        Case 14 'second wizard mode
pupevent 713
            light24.State = 2
            light28.State = 2
            light30.State = 2
        Case 15 'final battle
pupevent 714
            light24.State = 2
            light25.State = 2
            light28.State = 2
            light30.State = 2
            light32.State = 2
        Case Else
            If bMultiballMode Then
pupevent 715
                light28.State = 2
                light30.State = 2
            End If
    End Select
End Sub

Sub ResetJackpotLights 'when multiball is finished, resets jackpot and superjackpot lights
    bJackpot = False
pupevent 726
pupevent 727
pupevent 728
pupevent 729
    light24.State = 0
    light25.State = 0
    light28.State = 0
    light30.State = 0
    light32.State = 0
    light29.State = 0
    light51.State = 0
End Sub

Sub EnableSuperJackpot
    If bJackpot = True Then
        If light24.State + light25.State + light28.State + light30.State + light32.State = 0 Then
            'PlaySound "vo_superjackpotislit"
            light29.State = 2
            light51.State = 2
        End If
    End If
End Sub

'***********************************
' 5Bank Targets
'***********************************

Sub Target4_Hit
    PlaySoundAtBall SoundFXDOF("Target_Hit_2", 120, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target4"
    ' Do some sound or light effect
    Light15.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
        Case 11:TargetHits11 = TargetHits11 + 1:Addscore 50000:CheckWinBattle
    End Select
    Check5BankTargets
    checkkael
End Sub

Sub Target5_Hit
    PlaySoundAtBall SoundFXDOF("Target_Hit_4", 120, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target_Hit_1"
    ' Do some sound or light effect
    Light16.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
        Case 11:TargetHits11 = TargetHits11 + 1:Addscore 50000:CheckWinBattle
    End Select
    Check5BankTargets
    checkkael
End Sub

Sub Target7_Hit
    PlaySoundAtBall SoundFXDOF("Target_Hit_1", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target7"
    ' Do some sound or light effect
    Light17.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
        Case 11:TargetHits11 = TargetHits11 + 1:Addscore 50000:CheckWinBattle
    End Select
    Check5BankTargets
    checkkael
End Sub

Sub Target8_Hit
    PlaySoundAtBall SoundFXDOF("Target_Hit_2", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target8"
    ' Do some sound or light effect
    Light18.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
        Case 11:TargetHits11 = TargetHits11 + 1:Addscore 50000:CheckWinBattle
    End Select
    Check5BankTargets
    PlayQuote
End Sub

Sub Target9_Hit
    PlaySoundAtBall SoundFXDOF("Target_Hit_3", 114, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target9"
    ' Do some sound or light effect
    Light19.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
        Case 11:TargetHits11 = TargetHits11 + 1:Addscore 50000:CheckWinBattle
    End Select
    Check5BankTargets
End Sub

Sub Target12_Hit
    PlaySoundAtBall SoundFXDOF("Target_Hit_3", 114, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target12"
    ' Do some sound or light effect
    Light16a.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
        Case 11:TargetHits11 = TargetHits11 + 1:Addscore 50000:CheckWinBattle
    End Select
    Check5BankTargets
    checkkael
End Sub

Sub Check5BankTargets
    Dim tmp
    FlashForMs f7, 1000, 50, 0
    FlashForMs f1, 1000, 50, 0
    FlashForMs f2, 1000, 50, 0
    FlashForMs f5, 1000, 50, 0
    FlashForMs f6, 1000, 50, 0
    FlashForMs FredG, 1000, 50, 0
    FlashForMs FredD, 1000, 50, 0
    FlashForMs Fspot, 1000, 50, 0
    FlashForMs Fspot1, 1000, 50, 0
    tmp = INT(RND * 24) + 1
    PlaySoundAtBall "enemy_" &tmp
    ' if all 5 targets are hit then kill a monster & activate the mystery light
    If light15.state + light16.state + Light16a.state + light17.state + light18.state + light19.state = 6 Then
        ' kill a monster
        MonstersKilled(CurrentPlayer) = MonstersKilled(CurrentPlayer) + 1
        DMD "", "", "monster_" &tmp, eNone, eNone, eNone, 3500, True, "splendid1"
        LightEffect 1
        FlashEffect 1
        ' Lit the Mystery light if it is off
        If Light61.State = 1 Then
            AddScore 50000
        Else
            Light61.State = 1
            AddScore 25000
        End If
        ' reset the lights
        light15.state = 0
        light16.state = 0
        Light16a.state = 0
        light17.state = 0
        light18.state = 0
        light19.state = 0
    End If
End Sub

' Playfiel Multiplier timer: reduces the multiplier after 30 seconds

Sub pfxtimer_Timer
    If PlayfieldMultiplier(CurrentPlayer) > 1 Then
        PlayfieldMultiplier(CurrentPlayer) = PlayfieldMultiplier(CurrentPlayer)-1
        SetPlayfieldMultiplier PlayfieldMultiplier(CurrentPlayer)
    Else
        pfxtimer.Enabled = 0
    End If
End Sub

'*****************
'  Captive Target
'*****************

Sub Target6_Hit
    PlaySoundAtBall SoundFXDOF("fx_dingding", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        Awardskillshot
        Exit Sub
    End If
    AddScore 5000 'all targets score 5000
    ' Do some sound or light effect
    ' do some check
    If(bJackpot = True)AND(light51.State = 2)Then
        AwardSuperJackpot
        light51.State = 0
        light29.State = 0
        StartJackpots
    End If
    Select Case Battle(CurrentPlayer, 0)
        Case 0:SelectBattle 'no battle is active then change to another battle
        Case 9:CaptiveBallHits = CaptiveBallHits + 1:Addscore 25000:CheckWinBattle
    End Select

    ' increase the playfield multiplier for 30 seconds
    If light53.State = 1 Then
        AddPlayfieldMultiplier 1
        light53.State = 0
    End If

    ' increase Jackpot
    If light52.State = 1 Then
        AddJackpot 50000
        light52.State = 0
    End If
End Sub

'****************************
'  House Quint's Hole Hit & Awards
'****************************

Sub PyramidKicker_Hit
    Dim Delay
    Delay = 200
    PlaySoundAt "fx_kicker_enter", PyramidKicker
    If NOT Tilted Then
        ' do something
        If(bJackpot = True)AND(light24.State = 2)Then
            light24.State = 0
            AwardJackpot
            Delay = 2000
        End If
        If light61.State = 1 Then ' mystery light is lit
            light61.State = 0
            GiveRandomAward
            Delay = 3500
        End If
        If light23.State = 2 Then ' extra ball is lit
            light23.State = 1
            AwardExtraBall
            Delay = 2000
        End If
        Select Case Battle(CurrentPlayer, 0)
            Case 5
                If Light46.State = 2 Then
                    Light46.State = 0
                    Addscore 100000
                    CheckWinBattle
                    Delay = 1000
                End If
            Case 6
                If Light46.State = 2 Then
                    Light45.State = 2
                    Light46.State = 0
                    Addscore 100000
                    Delay = 1000
                End If
            Case 13
                If Light46.State = 2 Then
                    AddScore 100000
                    FlashEffect 3
                    DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
                End If
            Case 14
                If Light46.State = 2 Then
                    AddScore 120000
                    FlashEffect 3
                    DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
                End If
        End Select
    End If
    vpmtimer.addtimer Delay, "PyramidExit '"
End Sub

Sub PyramidExit()
    FlashForMs f3, 1000, 50, 0
    FlashForMs f3a, 1000, 50, 0
    FlashForMs f3a1, 1000, 50, 0
    FlashForMs f3a2, 1000, 50, 0
    FlashForMs f3a3, 1000, 50, 0
    FlashForMs f3a4, 1000, 50, 0
    FlashForMs f3a5, 1000, 50, 0
    FlashForMs FvertG, 1000, 50, 0
    FlashForMs FvertD, 1000, 50, 0
    PlaySoundAt SoundFXDOF("fx_kicker", 119, DOFPulse, DOFContactors), PyramidKicker
    DOF 121, DOFPulse
    PyramidKicker.kick 150, 10
    PlaySoundAt "fx_up", PyramidKicker
End Sub

Sub GiveRandomAward() 'from the Pyramid
    Dim tmp, tmp2

    ' show some random values on the dmd
    DMD CL("DUDE AWARD"), "", "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("PLAYFIELD X"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("BUMPER VALUE"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA BALL"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("BONUS X"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("SPINNER VALUE"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("BUMPER VALUE"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("PLAYFIELD X"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("BUMPER VALUE"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA BALL"), "", eNone, eNone, eNone, 50, False, "fx_spinner"

    tmp = INT(RND(1) * 80)
    Select Case tmp
        Case 1, 2, 3, 4, 5, 6 'Lit Extra Ball
            DMD "", CL("EXTRA BALL IS LIT"), "", eNone, eBlink, eNone, 1500, True, "fx_fanfare8"'pupevent
            light23.State = 2
        Case 7, 8, 13, 14, 15 '100,000 points
            DMD CL("BIG POINTS"), CL("100000"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare8"'pupevent
            AddScore 100000
        Case 9, 10, 11, 12 'Hold Bonus
            DMD CL("BONUS HELD"), CL("ACTIVATED"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare8"'pupevent
            bBonusHeld = True
        Case 16, 17, 18 'Increase Bonus Multiplier
            DMD CL("INCREASED"), CL("BONUS X"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare8"'pupevent
            AddBonusMultiplier 1
        Case 19, 20, 21 'Complete Battle
            If Battle(CurrentPlayer, 0) > 0 AND Battle(CurrentPlayer, 0) < 13 Then
                DMD CL("MISSION"), CL("COMPLETED"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare8"'pupevent
                WinBattle
            Else
                DMD CL("BIG POINTS"), CL("100000"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare8"'pupevent
                AddScore 100000
            End If
        Case 22, 23, 36, 37, 38 'PlayField multiplier
            DMD CL("INCREASED"), CL("PLAYFIELD X"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare8"'pupevent
            AddPlayfieldMultiplier 1
        Case 24, 25, 26, 27, 28 '100,000 points
            DMD CL("BIG POINTS"), CL("100000"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare8"'pupevent
            AddScore 100000
        Case 29, 30, 31, 32, 33, 34, 35 'Increase Bumper value
            BumperValue(CurrentPlayer) = BumperValue(CurrentPlayer) + 500
            DMD CL("BUMPER VALUE"), CL(BumperValue(CurrentPlayer)), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare8"'pupevent
        Case 39, 40, 43, 44 'extra multiball
            DMD CL("EXTRA"), CL("MULTIBALL"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare8":pupevent 711
            AddMultiball 1
        Case 45, 46, 47, 48 ' Ball Save
            DMD CL("BALL SAVE"), CL("ACTIVATED"), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare8":pupevent 899
            EnableBallSaver 20
        Case ELSE 'Add a Random score from 10.000 to 100,000 points
            tmp2 = INT((RND) * 9) * 10000 + 10000
            DMD CL("EXTRA POINTS"), CL(tmp2), "", eBlink, eBlink, eNone, 1500, True, "fx_fanfare8"'pupevent
            AddScore tmp2
    End Select
End Sub

'*******************
'   The Orbit lanes
'*******************

Sub sw5_Hit
    DOF 130, DOFPulse
    PlaySoundAtBall "fx_sensor"
    PlayQuote
    FlashForMs f4, 1000, 50, 0
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    If(bJackpot = True)AND(light25.State = 2)Then
        light25.State = 0
        AwardJackpot
    End If
    Select Case Battle(CurrentPlayer, 0)
        Case 4:OrbitHits = OrbitHits + 1:Addscore 70000:CheckWinBattle
        Case 5
            If Light45.State = 2 Then
                Light45.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light45.State = 2 Then
                Light49.State = 2
                Light45.State = 0
                Addscore 100000
            End If
        Case 10
            If Light45.State = 2 Then
                RampHits10 = RampHits10 + 1
                Light48.State = 2
                Light47.State = 2
                Light45.State = 0
                Light49.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 12
            If LastSwitchHit = "sw6" Then
                LastSwitchHit = ""
                loopCount = loopCount + 1
                Addscore 140000
                CheckWinBattle
            End If
        Case 13
            If Light45.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light45.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "sw5"
End Sub

Sub sw6_Hit
    DOF 131, DOFPulse
    PlaySoundAtBall "fx_sensor"
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    If(bJackpot = True)AND(light32.State = 2)Then
        light32.State = 0
        AwardJackpot
    End If
    Select Case Battle(CurrentPlayer, 0)
        Case 4:OrbitHits = OrbitHits + 1:Addscore 70000:CheckWinBattle
        Case 5
            If Light49.State = 2 Then
                Light49.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light49.State = 2 Then
                Light47.State = 2
                Light49.State = 0
                Addscore 100000
            End If
        Case 10
            If Light49.State = 2 Then
                RampHits10 = RampHits10 + 1
                Light48.State = 2
                Light47.State = 2
                Light45.State = 0
                Light49.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 12
            If LastSwitchHit = "sw5" Then
                LastSwitchHit = ""
                loopCount = loopCount + 1
                Addscore 140000
                CheckWinBattle
            End If
        Case 13
            If Light49.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light49.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "sw6"
End Sub

'****************
'     Ramps
'****************

Sub LeftRampDone_Hit
    Dim tmp
    If Tilted Then Exit Sub
    'increase the ramp bonus
    RampBonus = RampBonus + 1
    If(bJackpot = True)AND(light28.State = 2)Then
        light28.State = 0
        AwardJackpot
    End If
    'Machine Gun - left ramp only counts the variable
    MachineGunHits = MachineGunHits + 1
    CheckMachineGun
    'Battles
    Select Case Battle(CurrentPlayer, 0)
        Case 3:RampHits3 = RampHits3 + 1:Addscore 100000:CheckWinBattle
        Case 5
            If Light47.State = 2 Then
                Light47.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light47.State = 2 Then
                Light48.State = 2
                Light47.State = 0
                Addscore 100000
            End If
        Case 10
            If Light47.State = 2 Then
                RampHits10 = RampHits10 + 1
                Light48.State = 0
                Light47.State = 0
                Light45.State = 2
                Light49.State = 2
                Addscore 100000
                CheckWinBattle
            End If
        Case 13
            If Light47.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light47.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case else
            ' play ss quote
            PlayQuote
    End Select
    'check for combos
    if LastSwitchHit = "LeftRampDone" Then
        Addscore jackpot(CurrentPlayer)
        DMD CL("COMBO"), CL(jackpot(CurrentPlayer)), "_", eNone, eBlinkFast, eNone, 1000, True, "combosound"
    End If
    LastSwitchHit = "LeftRampDone"
End Sub

Sub RightRampDone_Hit
    Dim tmp
    If Tilted Then Exit Sub
    'increase the ramp bonus
    RampBonus = RampBonus + 1
    If(bJackpot = True)AND(light30.State = 2)Then
        light30.State = 0
        AwardJackpot
    End If
    'Machine Gun - rightt ramp counts the variable and give the jackpot if light31 is lit
    If light31.State = 2 Then
        DMD CL("QUINT HARPOON"), CL(jackpot(CurrentPlayer)), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_Jackpot"
        AddScore Jackpot(CurrentPlayer)
        LightEffect 2
        FlashEffect 2
    Else
        MachineGunHits = MachineGunHits + 1
        CheckMachineGun
    End If
    'Battles
    Select Case Battle(CurrentPlayer, 0)
        Case 3:RampHits3 = RampHits3 + 1:Addscore 100000:CheckWinBattle
        Case 5
            If Light48.State = 2 Then
                Light48.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6
            If Light48.State = 2 Then
                Light48.State = 0
                Addscore 100000
                WinBattle
            End If
        Case 10
            If Light48.State = 2 Then
                RampHits10 = RampHits10 + 1
                Light48.State = 0
                Light47.State = 0
                Light45.State = 2
                Light49.State = 2
                Addscore 100000
                CheckWinBattle
            End If
        Case 13
            If Light48.State = 2 Then
                AddScore 100000
                FlashEffect 3
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 14
            If Light48.State = 2 Then
                AddScore 120000
                FlashEffect 3
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case else
            ' play ss quote
            PlayQuote
    End Select

    'check for combos
    if LastSwitchHit = "RightRampDone" Then
        Addscore jackpot(CurrentPlayer)
        DMD CL("COMBO"), CL(jackpot(CurrentPlayer)), "_", eNone, eBlinkFast, eNone, 1000, True, "combosound"
    End If
    LastSwitchHit = "RightRampDone"
End Sub

'************************
'       Battles
'************************

' This table has 12 main battles, 2 wizard battles, and a final battle
' you may choose any the 12 main battles you want to play
' the first wizard mode is played after completing 4 battles
' the second wizard battle is played after completing 8 battles
' After completing all 12 battles you play the final battle

' current active battle number is stored in Battle(CurrentPlayer,0)

Sub SelectBattle 'select a new random battle if none is active
    Dim i
    If Battle(CurrentPlayer, 0) = 0 Then
        ' reset the battles that are not finished
        For i = 1 to 15
            If Battle(CurrentPlayer, i) = 2 Then Battle(CurrentPlayer, i) = 0
        Next
        Select Case BattlesWon(CurrentPlayer)
            Case 4:NewBattle = 13:Battle(CurrentPlayer, NewBattle) = 2:UpdateBattleLights:StartBattle  '4 battles = start wizard mode
            Case 9:NewBattle = 14:Battle(CurrentPlayer, NewBattle) = 2:UpdateBattleLights:StartBattle  '8 battles + wizard mode = start 2nd wizard mode
            Case 14:NewBattle = 15:Battle(CurrentPlayer, NewBattle) = 2:UpdateBattleLights:StartBattle '12 battles + 2 wizard modes = start final battle
            Case Else
                NewBattle = INT(RND * 12 + 1)
                do while Battle(CurrentPlayer, NewBattle) <> 0
                    NewBattle = INT(RND * 12 + 1)
                loop
                Battle(CurrentPlayer, NewBattle) = 2
                Light26.State = 2
                Light26a.State = 2
                UpdateBattleLights
        End Select
    'debug.print "newbatle " & newbattle
    End If
End Sub

' Update the lights according to the battle's state
Sub UpdateBattleLights
    Light38.State = Battle(CurrentPlayer, 1)
    Light7.State = Battle(CurrentPlayer, 2)
    Light36.State = Battle(CurrentPlayer, 3)
    Light40.State = Battle(CurrentPlayer, 4)
    Light33.State = Battle(CurrentPlayer, 5)
    Light37.State = Battle(CurrentPlayer, 6)
    Light42.State = Battle(CurrentPlayer, 7)
    Light34.State = Battle(CurrentPlayer, 8)
    Light34a.State = Battle(CurrentPlayer, 8)
    Light39.State = Battle(CurrentPlayer, 9)
    Light43.State = Battle(CurrentPlayer, 10)
    Light35.State = Battle(CurrentPlayer, 11)
    Light41.State = Battle(CurrentPlayer, 12)
    Light64.State = Battle(CurrentPlayer, 13)
    Light65.State = Battle(CurrentPlayer, 14)
    Light58.State = Battle(CurrentPlayer, 15)
End Sub

' Starting a battle means to setup some lights and variables, maybe timers
' Battle lights will always blink during an active battle
Sub StartBattle
    Battle(CurrentPlayer, 0) = NewBattle
    Light26.State = 0
    Light26a.State = 0
    ChangeSong
    PlaySound "Fx_countdown"
    EnableBallsaver 15 'start a 15 seconds ball saver
    Select Case NewBattle
        Case 1 'spinner = Super Spinners
            DMD CL("BERGLEKUTT"), CL("SHOOT SPINNERS"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            Light45a.State = 2
            Light49.State = 2
            Light49a.State = 2
            SpinCount = 0
        Case 2 'Hammerhead = Super Pop Bumpers
            DMD CL("QUEEN"), CL("BAVMORDA"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            Light22.State = 2
            Light22a.State = 2
            LightSeqBumpers.Play SeqRandom, 10, , 1000
            SuperBumperHits = 0
        Case 3 'jumelle = Ramps
            DMD CL("KIAYA"), CL("UFGOOD"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            Light47.State = 2
            Light48.State = 2
            RampHits3 = 0
        Case 4 'REAL = Orbits
            DMD CL("ELORA"), CL("DANAN"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            Light45.State = 2
            Light49.State = 2
            OrbitHits = 0
        Case 5 'Tiger = Shoot the lights 2
            DMD CL("FRANJEAN"), CL("ROOL"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            Light44.State = 2
            Light45.State = 2
            Light46.State = 2
            Light47.State = 2
            Light48.State = 2
            Light49.State = 2
            Light50.State = 2
        Case 6 'Search = Shoot the lights 1
            DMD CL("FINN"), CL("RAZIEL"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            Light44.State = 2
        Case 7 'JULY =  Target Frenzy
            DMD CL("CHERLINDREA"), CL("TARGETS-RAMPS"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            LightSeqAllTargets.Play SeqRandom, 10, , 1000
            TargetHits7 = 0
        Case 8 'BARREL = Left & Right Targets
            DMD CL("ROYAL"), CL("OWL"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            Light44.State = 2
            Light12.State = 2
            Light13.State = 2
            Light14.State = 2
            Light50.State = 2
            TargetHits8 = 0
        Case 9 'THRESHER = Captive Ball
            DMD CL("GRAND"), CL("ALDWIN"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            Light29.State = 2
            CaptiveBallHits = 0
        Case 10 'JESUS= Ramps and Orbits
            'uses the ramphits10 to count the hits
            DMD CL("MEEGHOSH"), CL("SHOOT RAMPS AND LANE"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            Light47.State = 2
            Light48.State = 2
            RampHits10 = 0
        Case 11 'OXYGEN = Blue Targets
            DMD CL("AIRK"), CL("THAUGHBAER"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            LightSeqBlueTargets.Play SeqRandom, 10, , 1000
            TargetHits11 = 0
        Case 12 'MAKO = Super Loops
            DMD CL("GENERAL"), CL("KAEL"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            Light45.State = 2
            Light49.State = 2
            Gate3.Open = 1
            insertnuit.visible = 1
            checkpost
            GiOff
            StartSpots
            loopCount = 0
        Case 13 'QUINT = Follow the Lights 1
            DMD CL("MADMARTIGAN"), CL("SHOOT LIT LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            FollowTheLights.Enabled = 1
pupevent 702
            AddMultiball 2
            StartJackpots
            ChangeGi green
        Case 14 'LIGHT = Follow the Lights 2
            DMD CL("SORSHA"), CL("SHOOT LIT LIGHTS"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
            FollowTheLights.Enabled = 1
pupevent 703            
            AddMultiball 2
            StartJackpots
            ChangeGi yellow
        Case 15 'JAWS - the final battle
            DMD CL("WILLOW"), CL("SHOOT THE JACKPOTS"), "", eNone, eNone, eNone, 1500, True, ""'pupevent
pupevent 704
            AddMultiball 3
            StartJackpots
            ChangeGi red
    End Select
End Sub

' wizard modes can't be won, you simply play them.
' check if the battle is completed
Sub CheckWinBattle
    dim tmp
    tmp = INT(RND * 8) + 1
    PlaySound "fx_sea" & tmp
    DOF 126, DOFPulse
    LightSeqInserts.StopPlay 'stop the light effects before starting again so they don't play too long.
    LightEffect 3
    Select Case NewBattle
        Case 1
            If SpinCount = 50 Then WinBattle:End if
        Case 2
            If SuperBumperHits = 10 Then WinBattle:End if
        Case 3
            If RampHits3 = 6 Then WinBattle:End if
        Case 4
            If OrbitHits = 6 Then WinBattle:End if
        Case 5
            If Light44.State + Light45.State + Light46.State + Light47.State + Light48.State + Light49.State + Light50.State = 0 Then WinBattle:End if
        Case 6
        Case 7
            If TargetHits7 = 20 Then WinBattle:End if 
        Case 8
            If TargetHits8 = 10 Then WinBattle:End if
        Case 9
            If CaptiveBallHits = 5 Then WinBattle:End if:SelectBattle
        Case 10:
            If RampHits10 = 6 Then WinBattle:End if
        Case 11
            If TargetHits11 = 10 Then WinBattle:End if
        Case 12
            If loopCount = 5 Then WinBattle:End if
    End Select
End Sub

Sub StopBattle 'called at the end of a ball
    Dim i
    Battle(CurrentPlayer, 0) = 0
    For i = 0 to 15
        If Battle(CurrentPlayer, i) = 2 Then Battle(CurrentPlayer, i) = 0
    Next
    UpdateBattleLights
    StopBattle2
    NewBattle = 0
End Sub

'called after completing a battle
Sub WinBattle
    Dim tmp
    BattlesWon(CurrentPlayer) = BattlesWon(CurrentPlayer) + 1
    Battle(CurrentPlayer, 0) = 0
    Battle(CurrentPlayer, NewBattle) = 1
    UpdateBattleLights
    FlashEffect 2
    LightEffect 2
    GiEffect 2
    DMD "", CL("MISSION COMPLETED"), "_", eNone, eBlinkFast, eNone, 1000, True, "missionsok":pupevent 903
    DOF 139, DOFPulse
    tmp = INT(RND * 4)
    Select Case tmp
        Case 0:vpmtimer.addtimer 1500, "PlaySound ""vo_excelent"" '"
        Case 1:vpmtimer.addtimer 1500, "PlaySound ""vo_impressive"" '"
        Case 2:vpmtimer.addtimer 1500, "PlaySound ""vo_welldone"" '"
        Case 3:vpmtimer.addtimer 1500, "PlaySound ""vo_YouWon"" '"
    End Select
    StopBattle2
    NewBattle = 0
    SelectBattle 'automatically select a new battle
    ChangeSong
    ChangeBall(0)
End Sub

Sub StopBattle2
    'Turn off the bomb lights
    Light44.State = 0
    Light45.State = 0
    Light45a.State = 0
    Light46.State = 0
    Light47.State = 0
    Light48.State = 0
    Light49.State = 0
    Light49a.State = 0
    Light50.State = 0
    ' stop some timers or reset battle variables
    Select Case NewBattle
        Case 1:
        Case 2:Light22.State = 0:LightSeqBumpers.StopPlay
        Case 3:
        Case 4:
        Case 5:
        Case 6:
        Case 7:LightSeqAllTargets.StopPlay
        Case 8:Light44.State = 0:Light50.State = 0
        Case 9:Light29.State = 0
        Case 10:
        Case 11:LightSeqBlueTargets.StopPlay
        Case 12:insertnuit.visible = 0:GiOn:StopSpots:checkpostoff
        Case 13:FollowTheLights.Enabled = 0:SelectBattle
        Case 14:FollowTheLights.Enabled = 0
        Case 15:ResetBattles:SelectBattle
    End Select
        ChangeBall(0)
End Sub

Sub ResetBattles
    Dim i, j
    For j = 0 to 4
        BattlesWon(j) = 0
        For i = 0 to 15
            Battle(CurrentPlayer, i) = 0
        Next
    Next
    NewBattle = 0
    'reset battle variables
    SpinCount = 0
    SuperBumperHits = 0
    RampHits3 = 0
    RampHits10 = 0
    OrbitHits = 0
    TargetHits7 = 0
    TargetHits8 = 0
    TargetHits11 = 0
    CaptiveBallHits = 0
    loopCount = 0
    MachineGunHits = 0
    ChangeBall(0)
End Sub

'Extra subs for the battles

Sub LightSeqAllTargets_PlayDone()
    LightSeqAllTargets.Play SeqRandom, 10, , 1000
End Sub

Sub LightSeqBumpers_PlayDone()
    LightSeqBumpers.Play SeqRandom, 10, , 1000
End Sub

Sub LightSeqBlueTargets_PlayDone()
    LightSeqBlueTargets.Play SeqRandom, 10, , 1000
End Sub

' Wizards modes timer
Dim FTLstep:FTLstep = 0

Sub FollowTheLights_Timer
    Light44.State = 0
    Light45.State = 0
    Light46.State = 0
    Light47.State = 0
    Light48.State = 0
    Light49.State = 0
    Light50.State = 0
    Select Case Battle(CurrentPlayer, 0)
        Case 13 '1st wizard mode
            Select case FTLstep
                Case 0:FTLstep = 1:Light44.State = 2
                Case 1:FTLstep = 2:Light45.State = 2
                Case 2:FTLstep = 3:Light46.State = 2
                Case 3:FTLstep = 4:Light47.State = 2
                Case 4:FTLstep = 5:Light48.State = 2
                Case 5:FTLstep = 6:Light49.State = 2
                Case 6:FTLstep = 7:Light50.State = 2
                Case 7:FTLstep = 8:Light49.State = 2
                Case 8:FTLstep = 9:Light48.State = 2
                Case 9:FTLstep = 10:Light47.State = 2
                Case 10:FTLstep = 11:Light46.State = 2
                Case 11:FTLstep = 0:Light45.State = 2
            End Select
        Case 14 '2nd wizard mode
            FTLstep = INT(RND * 7)
            Select case FTLstep
                Case 0:Light44.State = 2
                Case 1:Light45.State = 2
                Case 2:Light46.State = 2
                Case 3:Light47.State = 2
                Case 4:Light48.State = 2
                Case 5:Light49.State = 2
                Case 6:Light50.State = 2
            End Select
    End Select
End Sub

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
CustomBallImage(1) = 		"ball"
CustomBallLogoMode(1) = 	False
CustomBallDecal(1) = 		""
CustomBulbIntensity(1) = 	0
GBred(1) = 255 : GBgreen(1)	= 0 : GBblue(1) = 255


' green GlowBall
CustomBallGlow(2) = 		True
CustomBallImage(6) = 		"ball"
CustomBallLogoMode(6) = 	False
CustomBallDecal(6) = 		""
CustomBulbIntensity(6) = 	0
GBred(2) = 100 : GBgreen(2)	= 255 : GBblue(2) = 100

' blue GlowBall
CustomBallGlow(3) = 		True
CustomBallImage(3) = 		"ball"
CustomBallLogoMode(3) = 	False
CustomBallDecal(3) = 		""
CustomBulbIntensity(3) = 	0
GBred(3) = 50 : GBgreen(3)	= 50 : GBblue(3) = 255


' Orange GlowBall
CustomBallGlow(4) = 		True
CustomBallImage(4) = 		"ball"
CustomBallLogoMode(4) = 	False
CustomBallDecal(4) = 		""
CustomBulbIntensity(4) = 	0
GBred(4) = 255 : GBgreen(4)	= 165 : GBblue(4) = 000


' red GlowBall
CustomBallGlow(5) = 		True
CustomBallImage(5) = 		"ball"
CustomBallLogoMode(5) = 	False
CustomBallDecal(5) = 		""
CustomBulbIntensity(5) = 	0
GBred(5) = 255 : GBgreen(5)	= 99 : GBblue(5) = 71

' white GlowBall
CustomBallGlow(6) = 		True
CustomBallImage(6) = 		"ball"
CustomBallLogoMode(6) = 	False
CustomBallDecal(6) = 		""
CustomBulbIntensity(6) = 	0
GBred(6) = 255 : GBgreen(6)	= 255 : GBblue(6) = 255

' yellow GlowBall
CustomBallGlow(7) = 		True
CustomBallImage(7) = 		"ball"
CustomBallLogoMode(7) = 	False
CustomBallDecal(7) = 		""
CustomBulbIntensity(7) = 	0
GBred(7) = 255 : GBgreen(7)	= 255 : GBblue(7) = 000

' gold GlowBall
CustomBallGlow(8) = 		True
CustomBallImage(8) = 		"ball"
CustomBallLogoMode(8) = 	False
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

'**********************
' Machine Gun Jackpot
'**********************
' 30 seconds hurry up with jackpots on the right ramp
' uses variable MachineGunHits and the light31

Sub CheckMachineGun
    If light31.State = 0 Then
        If MachineGunHits MOD 10 = 0 Then
            EnableMachineGun
        End If
    End If
End Sub

Sub EnableMachineGun
    ' start the timers
    MachineGunTimerExpired.Enabled = True
    MachineGunSpeedUpTimer.Enabled = True
    ' turn on the light
    Light31.BlinkInterval = 160
    Light31.State = 2
End Sub

Sub MachineGunTimerExpired_Timer()
    MachineGunTimerExpired.Enabled = False
    ' turn off the light
    Light31.State = 0
End Sub

Sub MachineGunSpeedUpTimer_Timer()
    MachineGunSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    Light31.BlinkInterval = 80
    Light31.State = 2
End Sub

'***********
'  night
'***********

Dim i1, i2
i1 = 0
i2 = 3


Sub holotimer_timer
    night.imageA = "night" &i2
    i2 = (i2 + 1) MOD 10   
End Sub
