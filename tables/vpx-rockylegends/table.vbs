'#######     #######     #######  ##    ## ##    ##          ##        ########   #######  ######## ##    ## #######  #######
'##    ##   ##     ##   ##     ## ##   ##   ##  ##           ##        ##        ##     ## ##       ###   ## ##    ## ##     
'##    ##   ##     ##   ##        ##  ##     ####            ##        ##        ##        ##       ####  ## ##    ## ##     
'#######    ##     ##   ##        #####       ##             ##        ######    ##  ####  ######   ## ## ## ##    ## #######
'##  ##     ##     ##   ##        ##  ##      ##             ##        ##        ##     ## ##       ##  #### ##    ##      ##
'##   ##    ##     ##   ##     ## ##   ##     ##             ##        ##        ##     ## ##       ##   ### ##    ## ##   ##
'##    ##    #######     #######  ##    ##    ##             ########  ########   #######  ######## ##    ## #######  #######

' ****************************************************************
'          ROCKY LEGENDS for VISUAL PINBALL X 10.8
'                      version 2.0
' Table based on the VP9 prototype made by JPS and T-800
' This is my second table, and I hope you like it 
'            as much as I enjoyed creating it.
' I made this in 2026 with permission off JPS,     Joe Dilon
' ****************************************************************

Option Explicit
Randomize

Const BallSize = 50    
Const BallMass = 1.2    

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
Const cGameName = "ROCKYLEGENDS2026"
Const myVersion = "1.00"
Const MaxPlayers = 4          ' from 1 to 4
Const MaxMultiplier = 10      ' limit playfield multiplier
Const MaxBonusMultiplier = 10 'limit Bonus multiplier
Const MaxMultiballs = 6       ' max number of balls during multiballs

' Define Global Variables
Dim BallSaverTime ' in seconds of the first ball and during the game
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
Dim x 'used in loops

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
Dim Balboa1
Dim Balboa2
Dim Balboa3
Dim PhiladelphiaCount
Dim PunchHits(4)
Dim CityFadeStep
Dim CityRotationSpeed
Dim CityRotEnabled
Dim LastDMDUpdate
Dim bModeReady(4)
Dim PhiladelphiaLightsSaved(4)
Dim LovedOnesSaved(4)
Dim ComboLightsSaved(4)
Dim ModeLightsSaved(4)
Dim ComboCountSaved(4)
Dim LotteryLitThisBall(4)
Dim bBalboaMBStarted(4)
Dim bChampionnatMBStarted(4)
Dim bPhiladelphiaMBPlayed(4)
Dim bGRADESStarted(4)
Dim bPhiladelphiaMBStarted(4)
Dim bChampionnatMBPlayed(4)
Dim FlipperGlowStep

' Achievements
Dim bCombosAchieved(4)
Dim bLovedAchieved(4)
Dim bPhiladelphiaAchieved(4)
Dim bChampionnatAchieved(4)
Dim bAllCombatsAchieved(4)

Dim CombatCount(4)          

Dim SavedLoved024(4)
Dim SavedLoved025(4)
Dim SavedLoved026(4)
Dim SavedLoved027(4)
Dim SavedLoved028(4)

Dim SavedChamp061(4)
Dim SavedChamp062(4)
Dim SavedChamp063(4)
Dim SavedChamp064(4)
Dim SavedChamp050(4)

Dim SavedPhil015(4)
Dim SavedPhil016(4)
Dim SavedPhil074(4)
Dim SavedPhil019(4)
Dim SavedPhil020(4)
Dim SavedPhil021(4)
Dim SavedPhil022(4)
Dim SavedPhil023(4)
Dim SavedPhil073(4)
Dim SavedPhil018(4)
Dim SavedPhil072(4)
Dim SavedPhil017(4)

Dim SavedAchieve035(4)
Dim SavedAchieve075(4)
Dim SavedAchieve076(4)
Dim SavedAchieve077(4)
Dim SavedAchieve078(4)

' core.vbs variables
Dim plungerIM 
Dim mMagnet
Dim cbLeft    


'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////
Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!
usePUP   = true              ' enable Pinup Player functions for this table
cPuPPack = "ROCKYLEGENDS2026"   	 ' name of the PuP-Pack / PuPVideos folder for this table

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

PuPStart(cPuPPack)  


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

    Set cbLeft = New cvpmCaptiveBall
    With cbLeft
        .InitCaptive CapTrigger, CapWall, CapKicker, 0
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

	target001.IsDropped = True
	target002.IsDropped = True
	Balboa1 = 0
	Balboa2 = 0
	Balboa3 = 0

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
    RealTime.Interval = 20
	RealTime.Enabled = 1

    ' ====================== ROTATION objet's 3D ======================
    RockybTimer.Enabled = True
	GantsTimer.Enabled = True
	CombatTimer.Enabled = False      ' on désactive au démarrage
    Combat.Z = -100                  ' position cachée au début
    CombatPos = -100

End Sub

' ====================== ROTATION ROCKYB ======================
Dim RockybPos
Dim GantsPos

' ====================== ANIMATION COMBAT (montée/descente) ======================
Dim CombatPos
Dim CombatTargetHeight


RockybPos = 0
GantsPos = 0

Sub RockybTimer_Timer()
    RockybPos = RockybPos + 0.02          
    
    ' Rotation oscillante (gauche ↔ droite)
    rockyb.ObjRotZ = 30 * Sin(RockybPos)    
    
    
End Sub

' ====================== DÉCLENCHE L'ANIMATION POUR LES GANTS ======================
Dim GantsAnimStep

Sub TriggerGantsAnimation()
    GantsAnimStep = 0
    GantsAnimTimer.Interval = 35          
    GantsAnimTimer.Enabled = True
End Sub

' ====================== ANIMATION PENDANT 2 SECONDES ======================
Sub GantsAnimTimer_Timer()
    GantsAnimStep = GantsAnimStep + 1
    
    ' Balancier fluide et visible pendant ~2 secondes (66 frames × 30ms - 2s)
    gants.ObjRotX = 18 * Sin(GantsAnimStep * 0.45)   ' Amplitude ±18° - bien visible
    
    ' Arrêt après environ 2 secondes
    If GantsAnimStep >= 67 Then
        GantsAnimTimer.Enabled = False
        gants.ObjRotX = 0                     ' Retour à la position neutre
    End If
End Sub

' ====================== ANIMATION COMBAT - EXTINCTION GI FORCÉE ======================


Dim GIWasEnabled
Dim CombatBlinkStep

Sub StartCombatAnimation()
    If Combat Is Nothing Then Exit Sub
    
    GIWasEnabled = GIUpdateTimer.Enabled
    GIUpdateTimer.Enabled = False
    
    ' EXTINCTION DU PLAYFIELD
    GiOff
    ChangeGiIntensity 0.0
    
    Dim b
    For each b in aBumperLights
        b.State = 0
    Next
    
    ' Préparation de Combat pour la montée
    Combat.BlendDisableLighting = 2.0
    CombatBlinkStep = 0
    
    CombatTargetHeight = 150
    CombatPos = Combat.Z
    CombatTimer.Interval = 35
    CombatTimer.Enabled = True
End Sub

Sub StopCombatAnimation()
    If Combat Is Nothing Then Exit Sub
    
    CombatTargetHeight = -100
    CombatPos = Combat.Z
    CombatTimer.Interval = 25
    CombatTimer.Enabled = True
End Sub

Sub CombatTimer_Timer()
    If Combat Is Nothing Then 
        CombatTimer.Enabled = False
        Exit Sub 
    End If
    
    ' Mouvement
    CombatPos = CombatPos + (CombatTargetHeight - CombatPos) * 0.02
    Combat.Z = CombatPos
    
    ' Clignotement lent UNIQUEMENT pendant la montée
    If CombatTargetHeight = 150 Then
        CombatBlinkStep = CombatBlinkStep + 1
        If CombatBlinkStep MOD 12 = 0 Then
            Combat.BlendDisableLighting = 1.5     
        ElseIf CombatBlinkStep MOD 12 = 6 Then
            Combat.BlendDisableLighting = 0.9     
        End If
    End If
    
	' === EXTINCTION PROGRESSIVE PENDANT LA DESCENTE ===
    If CombatTargetHeight = -100 Then
        ' On réduit progressivement l'intensité lumineuse pendant la descente
        Combat.BlendDisableLighting = Combat.BlendDisableLighting * 0.92   ' diminution douce
        If Combat.BlendDisableLighting < 0.1 Then Combat.BlendDisableLighting = 0.0
    End If

		' Arrivée à destination
		If Abs(Combat.Z - CombatTargetHeight) < 2 Then
        Combat.Z = CombatTargetHeight
        CombatTimer.Enabled = False
        
        If CombatTargetHeight = 150 Then
            ' === ARRIVÉ EN HAUT ===
            GiOn
            ChangeGiIntensity 1.0
        
			GIUpdateTimer.Enabled = GIWasEnabled
            'LightSeqInserts.StopPlay
            UpdateModeLights
            Combat.BlendDisableLighting = 1.0     
        Else
            ' === DESCENTE TERMINÉE ===
            Combat.BlendDisableLighting = 0.0     
        End If
    End If
End Sub

' ==================== CITY - PHILADELPHIA MULTIBALL ====================


Sub CityFadeIn_Timer()
    CityFadeStep = CityFadeStep + 0.1
    city.BlendDisableLighting = CityFadeStep
    
    If CityFadeStep >= 1.0 Then
        CityFadeIn.Enabled = False
        city.BlendDisableLighting = 1.0 : pupevent 836
		'PlaySound "philadelphia"
    End If
End Sub

Sub CityFadeOut_Timer()
    CityFadeStep = CityFadeStep - 0.1
    city.BlendDisableLighting = CityFadeStep
    
    If CityFadeStep <= 0 Then
        CityFadeOut.Enabled = False
        city.Visible = False
        city.BlendDisableLighting = 0
        city.Z = -100
    End If
End Sub

Sub ShowCity()
    city.Z = 60
    city.Visible = True
    CityFadeStep = 0
    city.BlendDisableLighting = 0
    CityFadeIn.Enabled = True
    
    ' === Rotation légère ===
    CityRotationSpeed = 0.02          
    CityRotEnabled = True
    CityRotationTimer.Enabled = True

	' On augmente l'éclairage pendant la rotation
    city.BlendDisableLighting = 1.1        

End Sub

Sub CityRotationTimer_Timer()
    If CityRotEnabled = True Then
        city.ObjRotZ = city.ObjRotZ + CityRotationSpeed
        
        ' Option : léger pulse de luminosité pendant la rotation
        city.BlendDisableLighting = 1.1 + 0.5 * Sin(CityRotationTimer.Interval * 0.01)
    End If
End Sub

Sub HideCity()
    CityFadeStep = 1.0
    CityFadeOut.Enabled = True
    CityRotEnabled = False
    CityRotationTimer.Enabled = False
	city.BlendDisableLighting = 0.3
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25

    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
        If Credits < 99 Then Credits = Credits + 1
        if bFreePlay = False Then DOF 121, DOFOn
        If(Tilted = False)Then
            DMDFlush
            DMD "", CL("CREDITS " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
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
        If keycode = MechanicalTilt Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 1
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 0
        'If keycode = LeftMagnaSave Then mMagnet.MagnetOn = True:DOF 132, DOFOn
        If keycode = RightMagnaSave Then kickBallOut 'sometimes all the balls don't come out od the scoop (!?)

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    PlaySound "gong"
					DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                        If Credits < 1 And bFreePlay = False Then DOF 121, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, ""
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
                            If Credits < 1 And bFreePlay = False Then DOF 121, DOFOff
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
    ' If keycode = LeftMagnaSave OR keycode = RightMagnaSave Then ReleaseMagnetBalls

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
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
        LeftFlipper.RotateToEnd
        LeftFlipper2.RotateToEnd
        LeftFlipper001.RotateToEnd
        LeftFlipperOn = 1
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipper2.RotateToStart
        LeftFlipper001.RotateToStart
        LeftFlipperOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipper2.RotateToEnd
        RightFlipper001.RotateToEnd
        RightFlipperOn = 1
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipper2.RotateToStart
        RightFlipper001.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper2_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper2_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper001_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper001_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'*********************************************************
' Real Time Flipper adjustments - by JLouLouLou & JPSalas
'        (to enable flipper tricks) 
'*********************************************************

Dim FlipperPower
Dim FlipperElasticity
Dim SOSTorque, SOSAngle
Dim FullStrokeEOS_Torque, LiveStrokeEOS_Torque
Dim LeftFlipperOn
Dim RightFlipperOn

Dim LLiveCatchTimer
Dim RLiveCatchTimer
Dim LiveCatchSensivity

FlipperPower = 5000
FlipperElasticity = 0.6
FullStrokeEOS_Torque = 0.6 ' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.3 ' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

LeftFlipper.EOSTorqueAngle = 10
RightFlipper.EOSTorqueAngle = 10

SOSTorque = 0.2
SOSAngle = 6

LiveCatchSensivity = 10

LLiveCatchTimer = 0
RLiveCatchTimer = 0

LeftFlipper.TimerInterval = 1
LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's tricks timer
'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
	If LeftFlipperOn = 1 Then
		If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
			LeftFlipper.EOSTorque = FullStrokeEOS_Torque
			LLiveCatchTimer = LLiveCatchTimer + 1
			If LLiveCatchTimer < LiveCatchSensivity Then
				LeftFlipper.Elasticity = 0
			Else
				LeftFlipper.Elasticity = FlipperElasticity
				LLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		LeftFlipper.Elasticity = FlipperElasticity
		LeftFlipper.EOSTorque = LiveStrokeEOS_Torque
		LLiveCatchTimer = 0
	End If
	

'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
 	If RightFlipperOn = 1 Then
		If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
			RightFlipper.EOSTorque = FullStrokeEOS_Torque
			RLiveCatchTimer = RLiveCatchTimer + 1
			If RLiveCatchTimer < LiveCatchSensivity Then
				RightFlipper.Elasticity = 0
			Else
				RightFlipper.Elasticity = FlipperElasticity
				RLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		RightFlipper.Elasticity = FlipperElasticity
		RightFlipper.EOSTorque = LiveStrokeEOS_Torque
		RLiveCatchTimer = 0
	End If
End Sub

Sub RotateLaneLights(n) 'n is the direction, 1 = left or 0 = right
    Dim tmp
    If bRotateLights Then
        If n = 1 Then
            tmp = li002.State
            li002.State = li003.State
            li003.State = li004.State
            li004.State = li005.State
            li005.State = tmp
        Else
            tmp = li005.State
            li005.State = li004.State
            li004.State = li003.State
            li003.State = li002.State
            li002.State = tmp
        End If
    End If
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
    Tilt = Tilt + TiltSensitivity                 'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt > TiltSensitivity)AND(Tilt <= 15)Then 'show a warning
        DMD "_", CL("CAREFUL"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
    End if
    If(NOT Tilted)AND Tilt > 15 Then 'If more that 15 then TILT the table
        'display Tilt
        InstantInfoTimer.Enabled = False
        DMDFlush
        DMD CL("YOU"), CL("TILTED"), "", eNone, eNone, eNone, 200, False, ""
        
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
        bMultiBallMode = False           'normally disabled in the drain sub
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
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        vpmtimer.Addtimer 2000, "EndOfBall() '"
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'*****************************************
'         Music as wav sounds
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

Sub PlayGameOverMusic()
    If bMusicOn Then
        PlaySong "SurvivorNextGameOver"   ' ← change le nom selon ton fichier musique
    End If
End Sub

Sub ChangeSong
    Select Case Mode(CurrentPlayer, 0)
        Case 0:
            iF bMultiBallMode OR bBalboaMBStarted(CurrentPlayer) Then
                PlaySong "balboamultiball"
            Else
                PlaySong "mu_theme" : pupevent 861 : pupevent 800
            End If
        Case 1:PlaySong "Apollo" 						' Apollo
        Case 2:PlaySong "" 		    					' Clubber
        Case 3:PlaySong "Hogan"  						' Hogan
        Case 4:PlaySong "Drago"     					' Drago
        Case 5:PlaySong "Apollo"     					' Tommy
        Case 6:PlaySong "Cluber"  						' Mason
        Case 7:PlaySong "Hogan"     					' Conlan
        Case 8:PlaySong "Drago"							' Viktor
        Case 9:PlaySong "balboamultiball" 				' CHAMPIONNAT
    End Select
End Sub

Sub StopSong(name)
    StopSound name
End Sub

'******************************
' Play random quotes & sounds
'******************************

Sub PlayThunder
    PlaySound "sfx_thunder" &RndNbr(9)
End Sub

Sub PlayLightning
    PlaySound "sfx_lightning" &RndNbr(6)
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
    SetFlashColor GiFlash, col, 1
End Sub

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = factor
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
    PlaySoundAt "fx_GiOn", li008 'about the center of the table
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    PlaySoundAt "fx_GiOff", li008 'about the center of the table
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
            LightSeqInserts.UpdateInterval = 40
            LightSeqInserts.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqInserts.UpdateInterval = 25
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 20
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 4 'center
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqCircleOutOn, 15, 2
        Case 5 'top down
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 15, 1
        Case 6 'down to top
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 15, 1
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
Const lob = 1     'number of locked balls
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

    If UBound(BOT) = lob - 1 Then Exit Sub   ' Sortie rapide si pas de billes

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        If rolling(b) = True Then
            StopSound("fx_ballrolling" & b)
            rolling(b) = False
        End If
        aBallShadow(b).Y = 3000
    Next

    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z - BallSize/2

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000
                ballvol = Vol(BOT(b)) * 3
            End If
            rolling(b) = True
            PlaySound "fx_ballrolling" & b, -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound "fx_ballrolling" & b
                rolling(b) = False
            End If
        End If

        ' Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' Speed control
        BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95
        If BOT(b).VelX <> 0 AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx < 1 Then BOT(b).VelX = BOT(b).VelX * speedfactorx
            If speedfactory < 1 Then BOT(b).VelY = BOT(b).VelY * speedfactory
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
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
Sub aCBallsHit_Hit(idx):PlaySoundAt "fx_collide", CapKicker:End Sub 

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

    ' === Initialisation par joueur ===
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        PlayfieldMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
		CombatCount(i) = 0
		
		        ' === RESET Philadelphia (nouvelle partie) ===
        SavedPhil015(i) = 0
        SavedPhil016(i) = 0
        SavedPhil074(i) = 0
        SavedPhil019(i) = 0
        SavedPhil020(i) = 0
        SavedPhil021(i) = 0
        SavedPhil022(i) = 0
        SavedPhil023(i) = 0
        SavedPhil073(i) = 0
        SavedPhil018(i) = 0
        SavedPhil072(i) = 0
        SavedPhil017(i) = 0
        PhiladelphiaLightsSaved(i) = False
        bPhiladelphiaMBPlayed(i) = False

        ' === RESET Championnat boxeurs (nouvelle partie) ===
        SavedChamp061(i) = 0
        SavedChamp062(i) = 0
        SavedChamp063(i) = 0
        SavedChamp064(i) = 0
        SavedChamp050(i) = 0
        ChampionnatCount(i) = 0

		        SavedLoved024(i) = 0
        SavedLoved025(i) = 0
        SavedLoved026(i) = 0
        SavedLoved027(i) = 0
        SavedLoved028(i) = 0

        ' === Flags par joueur ===
        bModeReady(i) = False
        PhiladelphiaLightsSaved(i) = False
        LovedOnesSaved(i) = False
        ComboLightsSaved(i) = False
        ModeLightsSaved(i) = False
        ComboCountSaved(i) = False
        LotteryLitThisBall(i) = False
		bPhiladelphiaMBStarted(i) = False
        bChampionnatMBPlayed(i) = False
		bBalboaMBStarted(i) = False
        bChampionnatMBStarted(i) = False
        bPhiladelphiaMBPlayed(i) = False
        bGRADESStarted(i) = False

        ' === Achèvements ===
        bCombosAchieved(i) = False
        bLovedAchieved(i) = False
        bPhiladelphiaAchieved(i) = False
        bChampionnatAchieved(i) = False
        bAllCombatsAchieved(i) = False
    Next

    ' resets the score display, and turn off attract mode
    StopAttractMode
    StopSound Song          ' ← Arrête la musique Game Over
    Song = ""
	GiOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True

    ' initialise any other flags
    Tilt = 0

    ' initialise specific Game variables
    Game_Init()

    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall() : pupevent 800
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    DMDScoreNow

    Mode(CurrentPlayer, 0) = 0
    ModeStep = 0
    bModeReady(CurrentPlayer) = False
    li038.State = 0

    ' === EXTINCTION FORCÉE ===
    li024.State = 0
    li025.State = 0
    li026.State = 0
    li027.State = 0
    li028.State = 0

	li035.State = 0
	li075.State = 0
	li076.State = 0
	li077.State = 0
	li078.State = 0

	li045.State = 0
	li046.State = 0
	li047.State = 0
	li048.State = 0

	li009.State = 0
	li010.State = 0

    ' === Lumières des Combats ===
li056.State = 0 : li057.State = 0 : li058.State = 0 : li059.State = 0
li065.State = 0
li066.State = 0 : li067.State = 0 : li068.State = 0 : li069.State = 0
li070.State = 0 : li071.State = 0

UpdateCombatLights()     ' ← Reconstruit les lumières du joueur actuel

    ' (le reste des extinctions forcées reste)
    li011.State = 0 : li012.State = 0 : li013.State = 0 : li014.State = 0
    li007.State = 0 : li008.State = 0 : li009.State = 0 : li010.State = 0
    li049.State = 0
    li051.State = 0 : li052.State = 0 : li053.State = 0 : li054.State = 0 : li055.State = 0
    li015.State = 0 : li016.State = 0 : li074.State = 0
    li019.State = 0 : li020.State = 0 : li021.State = 0
    li022.State = 0 : li023.State = 0
    li073.State = 0 : li018.State = 0 : li072.State = 0 : li017.State = 0

    SetBonusMultiplier 1
    SetPlayfieldMultiplier 1
    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False

    ResetNewBallVariables
    UpdatePlayerLights()

	    ' === RESTAURATION Loved Ones : uniquement les VALIDÉS (State = 1) ===
    ' State 2 = clignotement en attente → on ne le restaure PAS
    If SavedLoved024(CurrentPlayer) = 1 Then li024.State = 1 Else li024.State = 0
    If SavedLoved025(CurrentPlayer) = 1 Then li025.State = 1 Else li025.State = 0
    If SavedLoved026(CurrentPlayer) = 1 Then li026.State = 1 Else li026.State = 0
    If SavedLoved027(CurrentPlayer) = 1 Then li027.State = 1 Else li027.State = 0
    If SavedLoved028(CurrentPlayer) = 1 Then li028.State = 1 Else li028.State = 0

	' === EXTINCTION FORCÉE des 5 boxeurs Championnat ===
    li061.State = 0
    li062.State = 0
    li063.State = 0
    li064.State = 0
    li050.State = 0

	    ' === RESTAURATION Championnat (5 boxeurs) ===
    If SavedChamp061(CurrentPlayer) > 0 Then li061.State = 1
    If SavedChamp062(CurrentPlayer) > 0 Then li062.State = 1
    If SavedChamp063(CurrentPlayer) > 0 Then li063.State = 1
    If SavedChamp064(CurrentPlayer) > 0 Then li064.State = 1
    If SavedChamp050(CurrentPlayer) > 0 Then li050.State = 1

    ' === RESTAURATION Philadelphia (uniquement si le MB n'a pas encore été fait) ===
    If bPhiladelphiaMBPlayed(CurrentPlayer) = False Then
        If SavedPhil015(CurrentPlayer) > 0 Then li015.State = 1
        If SavedPhil016(CurrentPlayer) > 0 Then li016.State = 1
        If SavedPhil074(CurrentPlayer) > 0 Then li074.State = 1
        If SavedPhil019(CurrentPlayer) > 0 Then li019.State = 1
        If SavedPhil020(CurrentPlayer) > 0 Then li020.State = 1
        If SavedPhil021(CurrentPlayer) > 0 Then li021.State = 1
        If SavedPhil022(CurrentPlayer) > 0 Then li022.State = 1
        If SavedPhil023(CurrentPlayer) > 0 Then li023.State = 1
        If SavedPhil073(CurrentPlayer) > 0 Then li073.State = 1
        If SavedPhil018(CurrentPlayer) > 0 Then li018.State = 1
        If SavedPhil072(CurrentPlayer) > 0 Then li072.State = 1
        If SavedPhil017(CurrentPlayer) > 0 Then li017.State = 1
    End If

	' === RESTAURATION des lumières d'Achèvements ===
    If SavedAchieve035(CurrentPlayer) > 0 Then li035.State = 1
    If SavedAchieve075(CurrentPlayer) > 0 Then li075.State = 1
    If SavedAchieve076(CurrentPlayer) > 0 Then li076.State = 1
    If SavedAchieve077(CurrentPlayer) > 0 Then li077.State = 1
    If SavedAchieve078(CurrentPlayer) > 0 Then li078.State = 1

    bBallSaverReady = True
    bSkillShotReady = True
End Sub
' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

	' === AFFICHE JOUEUR + BILLE ===
    If PlayersPlayingGame > 1 Then
        DMD CL("PLAYER " & CurrentPlayer), CL("BALL " & Balls), "", eNone, eNone, eNone, 1500, True, ""
    Else
        DMD CL("BALL " & Balls), "", "", eNone, eNone, eNone, 1500, True, ""
    End If

    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 107, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4

' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield > 1 Then
        DOF 131, DOFPulse
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
    ' Attendre qu'il n'y ait plus de bille dans le couloir du plunger
    If bBallInPlungerLane Then
        Exit Sub
    End If

    If BallsOnPlayfield < MaxMultiballs And mBalls2Eject > 0 Then
        CreateNewBall()
        mBalls2Eject = mBalls2Eject - 1
        
        If mBalls2Eject = 0 Then
            CreateMultiballTimer.Enabled = False
        End If
    Else
        mBalls2Eject = 0
        CreateMultiballTimer.Enabled = False
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0 : pupevent 801 : DOF_UnderCab "Cyan"
    TotalBonus = 10 'yes 10 points :)
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False
	StopCombatAnimation
	pupevent 825 : pupevent 823 : pupevent 828 : pupevent 831 : pupevent 869 : pupevent 870 : pupevent 871 : pupevent 872 : pupevent 868 : pupevent 852 : pupevent 853
    ' only process any of this if the table is not tilted.
    '(the tilt recovery mechanism will handle any extra balls or end of game)

    If NOT Tilted Then
        PlaySong "mu_plunger"
        'Count the bonus. This table uses several bonus
        DMD CL("BONUS"), "", "", eNone, eNone, eNone, 750, True, ""

        'Targets Hit x 3,500
        AwardPoints = BonusTargets(CurrentPlayer) * 3500
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("TARGETS HIT " & BonusTargets(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""

        'Ramps Hit x 9,000
        AwardPoints = BonusRamps(CurrentPlayer) * 9000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("RAMPS HIT " & BonusRamps(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""
        
        'Punching collected x 12,500
        AwardPoints = TreeHits(CurrentPlayer) * 12500
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("PUNCHING COLLECTED " & TreeHits(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""

        'Combo Hits x 25,000
        AwardPoints = ComboHits(CurrentPlayer) * 25000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("COMBO HITS " & ComboHits(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""

        'MISSIONS COMPLETED x 300,000
        AwardPoints = TotalMISSIONS(CurrentPlayer) * 300000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("COMBATS COMPLETED " & TotalMISSIONS(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""

		 ' === ICI : NOMBRE DE MODES RESTANTS ===
        Dim ModesRestants : ModesRestants = 0
        Dim j
        For j = 1 To 9
            If Mode(CurrentPlayer, j) <> 1 Then ModesRestants = ModesRestants + 1
        Next
        DMD CL("COMBATS LEFT "), CL(ModesRestants & " OF 9"), "", eNone, eBlink, eNone, 1500, True, ""

        'LOVEDS COMPLETED x 300,000
        AwardPoints = TotalTEAMS(CurrentPlayer) * 300000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL("LOVEDS COMPLETED " & TotalTEAMS(CurrentPlayer)), CL(FormatScore(AwardPoints)), "", eNone, eBlinkFast, eNone, 750, True, ""
    
     		
		DMD CL("BONUS X MULTIPLIER"), CL(FormatScore(TotalBonus) & " X " & BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 1500, True, ""
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        DMD CL("TOTAL BONUS"), CL(FormatScore(TotalBonus)), "", eNone, eNone, eNone, 2000, True, ""
        AddScore2 TotalBonus

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 11000, "EndOfBall2 '"
    Else 'if tilted then only add a short delay and move to the 2nd part of the end of the ball
        vpmtimer.addtimer 200, "EndOfBall2 '"
    End If
	HideCity()
	StopFlipperTopGlow
' Arrêt forcé des sons de roulement
    Dim b
    For b = 0 to tnob
        StopSound "fx_ballrolling" & b
    Next
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    Tilt = 0
    DisableTable False

        ' === SAUVEGARDE Loved Ones : uniquement les validés ===
    If li024.State = 1 Then SavedLoved024(CurrentPlayer) = 1 Else SavedLoved024(CurrentPlayer) = 0
    If li025.State = 1 Then SavedLoved025(CurrentPlayer) = 1 Else SavedLoved025(CurrentPlayer) = 0
    If li026.State = 1 Then SavedLoved026(CurrentPlayer) = 1 Else SavedLoved026(CurrentPlayer) = 0
    If li027.State = 1 Then SavedLoved027(CurrentPlayer) = 1 Else SavedLoved027(CurrentPlayer) = 0
    If li028.State = 1 Then SavedLoved028(CurrentPlayer) = 1 Else SavedLoved028(CurrentPlayer) = 0

	' === SAUVEGARDE Championnat (5 boxeurs) ===
    SavedChamp061(CurrentPlayer) = li061.State
    SavedChamp062(CurrentPlayer) = li062.State
    SavedChamp063(CurrentPlayer) = li063.State
    SavedChamp064(CurrentPlayer) = li064.State
    SavedChamp050(CurrentPlayer) = li050.State

    ' === SAUVEGARDE Philadelphia (12 cibles) ===
    SavedPhil015(CurrentPlayer) = li015.State
    SavedPhil016(CurrentPlayer) = li016.State
    SavedPhil074(CurrentPlayer) = li074.State
    SavedPhil019(CurrentPlayer) = li019.State
    SavedPhil020(CurrentPlayer) = li020.State
    SavedPhil021(CurrentPlayer) = li021.State
    SavedPhil022(CurrentPlayer) = li022.State
    SavedPhil023(CurrentPlayer) = li023.State
    SavedPhil073(CurrentPlayer) = li073.State
    SavedPhil018(CurrentPlayer) = li018.State
    SavedPhil072(CurrentPlayer) = li072.State
    SavedPhil017(CurrentPlayer) = li017.State

	' === SAUVEGARDE lumières d'Achèvements ===
    SavedAchieve035(CurrentPlayer) = li035.State
    SavedAchieve075(CurrentPlayer) = li075.State
    SavedAchieve076(CurrentPlayer) = li076.State
    SavedAchieve077(CurrentPlayer) = li077.State
    SavedAchieve078(CurrentPlayer) = li078.State

	If ExtraBallsAwards(CurrentPlayer) > 0 Then
        ' === IL Y A UN SHOOT AGAIN → on ne touche pas à ces lumières ===
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        If ExtraBallsAwards(CurrentPlayer) = 0 Then
            LightShootAgain.State = 0
        End If
DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, "" : pupevent 818
        ResetForNewPlayerBall()
        CreateNewBall()

    Else
        ' === PAS DE SHOOT AGAIN → on éteint ces lumières ===
        
        ' 1er paquet
        li030.State = 0
        li031.State = 0
        li032.State = 0
        li033.State = 0
        li034.State = 0
       
        
        ' Suite normale (fin de balle ou changement de joueur)
        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        If BallsRemaining(CurrentPlayer) <= 0 Then
            CheckHighScore()
        Else
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
        EndOfGame() : pupevent 803 : DOF_UnderCab "Cyan"
		vpmtimer.addtimer 2500, "PlayGameOverMusic '"

    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' === RESET DES LUMIÈRES LOVED ONES AU CHANGEMENT DE JOUEUR ===
    If PlayersPlayingGame > 1 And NextPlayer <> CurrentPlayer Then
        li024.State = 0
        li025.State = 0
        li026.State = 0
        li027.State = 0
        li028.State = 0
    End If
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
    ' just ended your game then play the end of game tune
    ' PlaySound "mu_death"
    ' vpmtimer.AddTimer 2500, "PlayEndQuote '"
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
    StartAttractMode
' you may wish to light any Game Over Light you may have

' Arrêt forcé des sons de roulement
    Dim b
    For b = 0 to tnob
        StopSound "fx_ballrolling" & b
    Next
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

	      ' === PROTECTION DES MODES (ne pas arrêter si billes restantes) ===
    If Mode(CurrentPlayer, 0) > 0 And BallsOnPlayfield > 0 Then
        ' Mode continue (multiball ou extra ball)
    ElseIf Mode(CurrentPlayer, 0) > 0 And BallsOnPlayfield = 0 Then
        StopMode
    End If

' === PHILADELPHIA MULTIBALL END ===
    If bPhiladelphiaMBStarted(CurrentPlayer) Then
    If BallsOnPlayfield = 1 Then           ' ← Quand il ne reste plus qu'une bille
        bPhiladelphiaMBStarted(CurrentPlayer) = False
        
        HideCity()                          ' ← Disparition de l'objet city
        
        ' On éteint les lumières Philadelphia
        li015.State = 0 : li016.State = 0 : li074.State = 0
        li019.State = 0 : li020.State = 0 : li021.State = 0
        li022.State = 0 : li023.State = 0
        li073.State = 0 : li018.State = 0 : li072.State = 0 : li017.State = 0
        
        DMDScoreNow
    End If
End If

    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain
    DOF 109, DOFPulse
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
            ' stop the ballsaver timer during the launch ball saver time, but not during multiballs
            If NOT bMultiBallMode Then
                DMD "_", CL("BALL SAVED"), "_", eNone, eBlinkfast, eNone, 2500, True, "" : pupevent 812
            'BallSaverTimerExpired_Timer 'enable this line to stop the ballsaver timer
            End If
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' turn off any multiball specific lights
                    If Mode(CurrentPlayer, 0) = 0 Then
                        ChangeGi white
                        ChangeGIIntensity 1
                    End If
                    'stop any multiball modes of this game
                    StopMBmodes
                    ' you may wish to change any music over at this point
                    changesong
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
                
                              
                If bPhiladelphiaMBStarted(CurrentPlayer) Then
                    bPhiladelphiaMBStarted(CurrentPlayer) = False
                    ' On éteint TOUTES les cibles Philadelphia
                    li015.State = 0 : li016.State = 0 : li074.State = 0
                    li019.State = 0 : li020.State = 0 : li021.State = 0
                    li022.State = 0 : li023.State = 0
                    li073.State = 0 : li018.State = 0 : li072.State = 0 : li017.State = 0
                End If
                
                If bChampionnatMBStarted(CurrentPlayer) Then
                    li051.State = 0 : li052.State = 0 : li053.State = 0
                    li054.State = 0 : li055.State = 0
                    bChampionnatMBStarted(CurrentPlayer) = False
                End If
                
                ' End Mode and timers
                ChangeGi white
                ChangeGIIntensity 1
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 
            End If
        End If
    End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
    If bPlayIntro Then PlaySound "":bPlayIntro = False
    PlaySoundAt "fx_sensor", swPlungerRest
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    'LaunchLight.State = 2
    ' be sure to update the Scoreboard after the animations, if any
    'Start the skillshot lights & variables if any
    If bSkillShotReady Then
        PlaySong "mu_plunger"
        UpdateSkillshot()
        ' show the message to shoot the ball in case the player has fallen sleep
        swPlungerRest.TimerEnabled = 1
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

Sub swPLunger2_Hit 'extra trigger to detect a ball resting down on the plunger
    ' Pendant un multiball on force toujours le lancement automatique
    If bAutoPlunger OR bMultiBallMode OR bPhiladelphiaMBStarted(CurrentPlayer) OR bBalboaMBStarted(CurrentPlayer) OR bChampionnatMBStarted(CurrentPlayer) Then
        bAutoPlunger = True
        vpmtimer.addtimer 1200, "PlungerIM.AutoFire:DOF 113, DOFPulse:DOF 130, DOFPulse:PlaySoundAt ""fx_kicker"", swPlungerRest '"
    End If
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    lighteffect 6
    bBallInPlungerLane = False
    bAutoPlunger = False           'disable the autoplunger as the ball has left the plunger lane
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
        ChangeSong
        ResetSkillShotTimer.Enabled = 1
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

 Sub swPlungerRest_Timer
    IF bOnTheFirstBall Then
        Select Case RndNbr(5)
            Case 1:DMD "", "", "d_rules", eNone, eNone, eNone, 50000, False, "" : pupevent 808
            Case 2:DMD "", "", "d_rules", eNone, eNone, eNone, 50000, False, "" : pupevent 808
            Case 3:DMD "", "", "d_rules", eNone, eNone, eNone, 50000, False, "" : pupevent 808
            Case 4:DMD "", "", "d_rules", eNone, eNone, eNone, 50000, False, "" : pupevent 808
            Case 5:DMD "", "", "d_rules", eNone, eNone, eNone, 50000, False, "" : pupevent 808
        End Select
    Else
        Select Case RndNbr(4)
            Case 1:DMD "", "", "d_rules", eNone, eNone, eNone, 50000, False, "" : pupevent 808
            Case 2:DMD "", "", "d_rules", eNone, eNone, eNone, 50000, False, "" : pupevent 808
            Case 3:DMD "", "", "d_rules", eNone, eNone, eNone, 50000, False, "" : pupevent 808
            Case 4:DMD "", "", "d_rules", eNone, eNone, eNone, 50000, False, "" : pupevent 808
        End Select
    End If
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimerExpired.Enabled = False
    BallSaverSpeedUpTimer.Enabled = False
    BallSaverTimerExpired.Interval = 1000 * seconds
    BallSaverTimerExpired.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
    li006.BlinkInterval = 160
    li006.State = 2 'Mission mode light
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
    li006.State = 0 'Mission mode light
    ' if the table uses the same lights for the extra ball or replay then turn them on if needed
    If ExtraBallsAwards(CurrentPlayer) > 0 Then
        LightShootAgain.State = 1
    End If
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
    li006.BlinkInterval = 80
    li006.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board

Sub AddScore(points) 'normal score routine
    If Tilted Then Exit Sub
    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer) * GRADESMultiplier
' you may wish to check to see if the player has gotten a replay
End Sub

Sub AddScore2(points) 'used in jackpots, skillshots, combos, and bonus as they doe not use the PlayfieldMultiplier
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
    if(BonusMultiplier(CurrentPlayer) + n <= MaxBonusMultiplier)then
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

Sub UpdateBonusXLights(Level) 'no lights in this table
    ' Update the lights
    Select Case Level
    '        Case 1:li021.State = 0:li022.State = 0:li023.State = 0:li024.State = 0
    '        Case 2:li021.State = 1:li022.State = 0:li023.State = 0:li024.State = 0
    '        Case 3:li021.State = 1:li022.State = 1:li023.State = 0:li024.State = 0
    '        Case 4:li021.State = 1:li022.State = 1:li023.State = 1:li024.State = 0
    '        Case 5:li021.State = 1:li022.State = 1:li023.State = 1:li024.State = 1
    End Select
End Sub

Sub AddPlayfieldMultiplier(n)
    Dim NewPFLevel
    ' if not at the maximum level x
    if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier)then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
        SetPlayfieldMultiplier(NewPFLevel)
        DMD "_", CL("PLAYFIELD X " &NewPFLevel), "_", eNone, eBlink, eNone, 2000, True, "sfx_thunder" &RndNbr(7)
        LightEffect 4
    ' Play a voice sound
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
    if(PlayfieldMultiplier(CurrentPlayer) > 1)then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer)- 1
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

Sub UpdatePFXLights(Level) 'no lights in this table
    ' Update the playfield multiplier lights
    Select Case Level
    '        Case 1:li025.State = 0:li026.State = 0:li027.State = 0:li027.State = 0
    '        Case 2:li025.State = 1:li026.State = 0:li027.State = 0:li027.State = 0
    '        Case 3:li025.State = 0:li026.State = 1:li027.State = 0:li027.State = 0
    '        Case 4:li025.State = 0:li026.State = 0:li027.State = 1:li027.State = 0
    '        Case 5:li025.State = 0:li026.State = 0:li027.State = 0:li027.State = 1
    End Select
' perhaps show also the multiplier in the DMD?
End Sub

Sub AwardExtraBall()
    '   If NOT bExtraBallWonThisBall Then 'in this table you can win several extra balls
    DMD "_", CL("EXTRA BALL WON"), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 111, DOFPulse, DOFKnocker)
    DOF 130, DOFPulse
    PLaySound "" : pupevent 802
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
    'bExtraBallWonThisBall = True
    light009.State = 0        'turn off extra ball light
    LightShootAgain.State = 1 'light the shoot again lamp
    GiEffect 1
    LightEffect 2
'    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL("SPECIAL WON"), "_", eNone, eBlink, eNone, 2000, True, SoundFXDOF("fx_Knocker", 111, DOFPulse, DOFKnocker)
    DOF 130, DOFPulse : pupevent 816
    Credits = Credits + 1
    AddScore2 3000000 '3 mill only for this table
    If bFreePlay = False Then DOF 121, DOFOn
    LightEffect 2
    GiEffect 1
    Light010.State = 0 'turn off special light
End Sub

Sub AwardJackpot()     'only used for the final mode
    DMD CL("JACKPOT"), CL(FormatScore(Jackpot(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_Jackpot"
    DOF 137, DOFPulse
    AddScore2 Jackpot(CurrentPlayer)
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + 100000
    LightEffect 2
    GiEffect 1
    FlashEffect 1
End Sub

Sub AwardSuperJackpot() 'not used in this table as there are several superjackpots but I keep it as a reference
    DMD CL("SUPER JACKPOT"), CL(FormatScore(SuperJackpot(CurrentPlayer))), "d_border", eNone, eBlink, eNone, 2000, True, "vo_super_jackpot"
    DOF 137, DOFPulse
    AddScore2 SuperJackpot(CurrentPlayer)
    LightEffect 2
    GiEffect 1
End Sub

Sub AwardSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL("SKILLSHOT"), CL(FormatScore(SkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_skillshot"
    DOF 127, DOFPulse
    Addscore2 SkillShotValue(CurrentPlayer)
    ' increment the skillshot value with 50.000
    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 50000
    'do some light show
    GiEffect 1
    LightEffect 2
End Sub

Sub AwardSuperSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL("SUPER SKILLSHOT"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_superskillshot"
    DOF 138, DOFPulse
    Addscore2 SuperSkillshotValue(CurrentPlayer)
    ' increment the skillshot value with 500.000
    SuperSkillshotValue(CurrentPlayer) = SuperSkillshotValue(CurrentPlayer) + 500000
    'do some light show
    GiEffect 1
    LightEffect 2
End Sub

Sub AwardFreakySkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL("FREAKY SKILLSHOT"), CL(FormatScore(FreakySkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, "vo_freakyskillshot"
    DOF 138, DOFPulse
    Addscore2 FreakySkillshotValue(CurrentPlayer)
    ' increment the skillshot value with 500.000
    FreakySkillshotValue(CurrentPlayer) = FreakySkillshotValue(CurrentPlayer) + 500000
    'do some light show
    GiEffect 1
    LightEffect 2
End Sub

Sub aSkillshotTargets_Hit(idx) 'stop the skillshot if any other target/switch is hit
    If bSkillshotReady then ResetSkillShotTimer_Timer
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
    If(x <> "")then Credits = CInt(x)Else Credits = 0:If bFreePlay = False Then DOF 121, DOFOff:End If
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
        DOF 121, DOFOn
    End If

    If tmp > HighScore(3)Then
        PlaySound SoundFXDOF("fx_Knocker", 111, DOFPulse, DOFKnocker)
        DOF 130, DOFPulse
        HighScore(3) = tmp
        'Play HighScore sound
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    pupevent 807
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
    Dim i, j
    If UseFlexDMD Then
        Set FlexDMD = CreateObject("FlexDMD.FlexDMD") : : pupevent 849 : pupevent 806
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
    Else
        digitgrid.Visible = True
        For i = 0 to 40
            Digits(i).Visible = True
        Next
    End If

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
    

	If GameTime < LastDMDUpdate + 30 Then Exit Sub   ' limite à ~33 mises à jour par seconde
    LastDMDUpdate = GameTime
	Dim tmp, tmp1, tmp2
    
    If (dqHead = dqTail) Then
        ' default when no modes are active
        tmp = RL(FormatScore(Score(CurrentPlayer)))
        tmp1 = FL("PLAYER " & CurrentPlayer, "BALL " & Balls)
        tmp2 = "d_border"
        
        ' ==================== INFO MODE ====================
        Select Case Mode(CurrentPlayer, 0)
            
            Case 0 ' no mode active
                ' rien de spécial
            
            Case 1 ' Apollo
                tmp2 = "d_border"
                Select Case ModeStep
                    Case 1: tmp1 = CL("SHOOT LIT LIGHTS")
                    Case 2: tmp1 = CL("SHOOT CENTER TARGETS")
                End Select
                
            Case 2 ' Clubber
                tmp2 = "d_border"
                Select Case ModeStep
                    Case 1: tmp1 = CL("SHOOT LIT LIGHT")
                    Case 2: tmp1 = CL("SHOOT CENTER TARGETS")
                End Select
                
            Case 3 ' Hogan
                tmp2 = "d_border"
                Select Case ModeStep
                    Case 1: tmp1 = CL("SHOOT LIT LIGHTS")
                    Case 2: tmp1 = CL("SHOOT CENTER TARGETS")
                End Select
                
            Case 4 ' Drago
                tmp2 = "d_border"
                Select Case ModeStep
                    Case 1: tmp1 = CL("SHOOT LIT LIGHT")
                    Case 2: tmp1 = CL("SHOOT CENTER TARGETS")
                End Select
                
            Case 5 ' Tommy
                tmp2 = "d_border"
                Select Case ModeStep
                    Case 1: tmp1 = CL("SHOOT LIT LIGHT")
                    Case 2: tmp1 = CL("SHOOT CENTER TARGETS")
                End Select
                
            Case 6 ' Mason
                tmp2 = "d_border"
                Select Case ModeStep
                    Case 1: tmp1 = CL("SHOOT LIT TARGETS")
                    Case 2: tmp1 = CL("SHOOT CENTER TARGETS")
                End Select
                
            Case 7 ' Conlan
                tmp2 = "d_border"
                Select Case ModeStep
                    Case 1: tmp1 = CL("SHOOT LIT TARGETS")
                    Case 2: tmp1 = CL("SHOOT CENTER TARGETS")
                End Select
                
            Case 8 ' Viktor
                tmp2 = "d_border"
                Select Case ModeStep
                    Case 1: tmp1 = CL("SHOOT LIT TARGETS")
                    Case 2: tmp1 = CL("SHOOT CENTER TARGETS")
                End Select
                
            Case 9 ' Damian
                tmp2 = "d_border"
                Select Case ModeStep
                    Case 1: tmp1 = CL("SHOOT LIT TARGETS")
                    Case 2: tmp1 = CL("SHOOT CENTER TARGETS")
                End Select
                
        End Select
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
                    Temp = Right(dLine(i), 19)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), 21 - deCount(i), 1)
                    Temp = Temp & Left(dLine(i), 19)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkSlowRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(20)
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkFastRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(20)
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


Function ExpandLine(TempStr)
    If TempStr = "" Then
        ExpandLine = Space(20)
    Else
        If Len(TempStr) >= 20 Then
            ExpandLine = Left(TempStr, 20)
        Else
            ExpandLine = TempStr & Space(20 - Len(TempStr))
        End If
    End If
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

Function FL(NumString1, NumString2) ' Fill Line - version sécurisée
    Dim Temp, TempStr
    Temp = 20 - Len(NumString1) - Len(NumString2)
    If Temp < 0 Then Temp = 0
    TempStr = NumString1 & Space(Temp) & NumString2
    FL = TempStr
End Function

Function CL(NumString) ' Center Line - version sécurisée
    Dim Temp, TempStr
    If Len(NumString) >= 20 Then
        CL = Left(NumString, 20)
    Else
        Temp = (20 - Len(NumString)) \ 2
        If Temp < 0 Then Temp = 0
        TempStr = Space(Temp) & NumString & Space(Temp)
        CL = TempStr
    End If
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
    'Chars(33) = ""        '!
    'Chars(34) = ""        '"
    'Chars(35) = ""        '#
    'Chars(36) = ""        '$
    'Chars(37) = ""        '%
    'Chars(38) = ""        '&
    'Chars(39) = ""        ''
    'Chars(40) = ""        '(
    'Chars(41) = ""        ')
    'Chars(42) = ""        '*
    Chars(43) = "d_plus"  '+
    'Chars(44) = ""        '
    Chars(45) = "d_minus" '-
    Chars(46) = "d_dot"   '.
    'Chars(47) = ""        '/
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
    'Chars(61) = ""        '=
    Chars(62) = "d_more"  '>
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
    Chars(94) = "d_up"    '^
    'Chars(95) = "" '_
    'Chars(96) = ""
    'Chars(97) = ""  'a
    'Chars(98) = ""  'b
    'Chars(99) = ""  'c
    'Chars(100) = "" 'd
    'Chars(101) = "" 'e
    'Chars(102) = "" 'f
    'Chars(103) = "" 'g
    'Chars(104) = "" 'h
    'Chars(105) = "" 'i
    'Chars(106) = "" 'j
    'Chars(107) = "" 'k
    'Chars(108) = "" 'l
    'Chars(109) = "" 'm
    'Chars(110) = "" 'n
    'Chars(111) = "" 'o
    'Chars(112) = "" 'p
    'Chars(113) = "" 'q
    'Chars(114) = "" 'r
    'Chars(115) = "" 's
    'Chars(116) = "" 't
    'Chars(117) = "" 'u
    'Chars(118) = "" 'v
    'Chars(119) = "" 'w
    'Chars(120) = "" 'x
    'Chars(121) = "" 'y
    'Chars(122) = "" 'z
    'Chars(123) = "" '{
    'Chars(124) = "" '|
    'Chars(125) = "" '}
    'Chars(126) = "" '~
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
' Real Time updates
'********************
'used for all the real time updates

Sub Realtime_Timer
    RollingUpdate

	' Sécurité Kickback - bille coincée (peu importe l'état de la lumière)
If Kickback.BallCntOver > 0 Then
    vpmtimer.addtimer 400, "KickbackEjectBall '"
End If

End Sub

' flippers top animations

Sub LeftFlipper_Animate: LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle: End Sub
Sub RightFlipper_Animate: RightFlipperTop.RotZ = RightFlipper.CurrentAngle: End Sub
Sub LeftFlipper001_Animate: LeftFlipperTop001.RotZ = LeftFlipper001.CurrentAngle: End Sub
Sub RightFlipper001_Animate: RightFlipperTop001.RotZ = RightFlipper001.CurrentAngle: End Sub
Sub LeftFlipper2_Animate: LeftFlipperTop002.RotZ = LeftFlipper2.CurrentAngle: End Sub
Sub RightFlipper2_Animate: RightFlipperTop002.RotZ = RightFlipper2.CurrentAngle: End Sub

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

        ' Store all blink inChampionnat
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
            n.color = RGB(0, 0, 18)
            n.colorfull = RGB(0, 0, 255)
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
            stat = 0
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
    DMD "", "          PRESENTS", "d_jppresents", eNone, eNone, eNone, 4000, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 4000, False, ""
    DMD "", "", "d_title2", eNone, eNone, eNone, 3000, False, ""
	DMD "     THANKS TO   ", "     JPS   T-800", "d_t800", eNone, eNone, eNone, 4000, False, ""
    DMD "", "", "d_instructions", eNone, eNone, eNone, 6000, False, ""
	DMD CL("HIGHSCORES"), Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL("HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL("HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    StartLightSeq
    DMDFlush
    ShowTableInfo
    PlaySong ""
End Sub

Sub StopAttractMode
    StopRainbow
    DMDScoreNow
    LightSeqAttract.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 10
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

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, timers, etc
Sub VPObjects_Init
    'TrapdoorDown
End Sub

' tables variables and Mode init
Dim bRotateLights
Dim bPlayIntro
Dim Mode(4, 9) '4 players, 8 modes
Dim BumperHits 'used for the skillshot and
Dim FreakySkillshotValue(4)
Dim ComboValue(4)
Dim ComboHits(4)
Dim ComboCount
Dim BumperAward
Dim OrbitHits
Dim RampHits
Dim CurrentMode(4)       'the current selected mode, used to increase the modes or CHALLENGES
Dim ModeStep             'use for the different steps during the modes/CHALLENGES
Dim EndModeCountdown
Dim ChampionnatTargetsCompleted 'used in Championnat mode to count the time the Championnat targets has been completed
Dim ChampionnatHits
Dim ChampionnatHitsNeeded
Dim ChampionnatCount(4)  'hits needed to start Championnat multiball
Dim bDRAGOMBStarted
Dim MedussaX      'multiplier during DRAGO MB
Dim ExtraBallHits 'used in DRAGO multiball
Dim SpinnerHits   'used in TOMMY mode
Dim TrapDoorHits  'used in VIKTOR mode
Dim bJackpotsEnabled
Dim bLockEnabled
Dim ApolloJackpot(4)              'jackpot value
Dim AdriansHits(4)                 'targets hits
Dim AdriansNeeded(4)               'number of hits needed to start the random award
Dim TreeHits(4)
Dim thunderbModeHits(4)   'the number of times the thunderb mode targets has been hit
Dim thunderbModeNeeded(4) 'number of hits required to start
Dim GRADESHits(4)     'number of hits to start the GRADES mode: PlayfieldMultiplier
Dim GRADESMultiplier
Dim CBHits(4) 'captive ball hits
Dim BonusTargets(4)
Dim BonusRamps(4)
Dim BonusLOOPS(4)
Dim BonusXHits(4)
Dim HiddenShots(4)
Dim TotalMISSIONS(10)
Dim TotalTEAMS(5)

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    'Init Variables
    bPlayIntro = True
    BallSaverTime = 20
    bExtraBallWonThisBall = False
    BumperHits = 0
    bRotateLights = True
    BumperAward = 5000
    ComboCount = 0
    OrbitHits = 0
    RampHits = 0
    EndModeCountdown = 0
    ChampionnatHits = 0
    ChampionnatHitsNeeded = 12
    ModeStep = 0
    bDRAGOMBStarted = False
    MedussaX = 1
    ExtraBallHits = 0
    SpinnerHits = 0
    TrapDoorHits = 0
    ChampionnatTargetsCompleted = 0
    bJackpotsEnabled = False
    bLockEnabled = False
    Balboa1 = 0
    Balboa2 = 0
    Balboa3 = 0
    GRADESMultiplier = 1
    
	
	

For i = 0 to 4
    PunchHits(i) = 0
Next	
For i = 0 to 4
        SkillshotValue(i) = 100000
        SuperSkillshotValue(i) = 1000000
        FreakySkillshotValue(i) = 1500000
        CurrentMode(i) = 0
        ComboValue(i) = 250000
        ChampionnatCount(i) = 0
        ApolloJackpot(i) = 250000
        BallsInLock(i) = 0
        SuperJackpot(i) = 5000000
        Jackpot(i) = 1000000
        AdriansHits(i) = 0
        AdriansNeeded(i) = 2
        TreeHits(i) = 0
        thunderbModeHits(i) = 0
        thunderbModeNeeded(i) = 1
        GRADESHits(i) = 0
        CBHits(i) = 0
        BonusTargets(i) = 0
        BonusRamps(i) = 0
        BonusLOOPS(i) = 0
        BonusXHits(i) = 0
        ComboHits(i) = 0
        HiddenShots(i) = 0
        TotalMISSIONS(i) = 0
        TotalTEAMS(i) = 0
    Next
    For i = 0 to 4
        For j = 0 to 9
            Mode(i, j) = 0
        Next
    Next
    TurnOffPlayfieldLights()
End Sub

Sub UpdateEndTargetsLight
    If target001.IsDropped = False OR target002.IsDropped = False Then
        ' Les targets sont levées → Light003 clignote
        Light003.BlinkInterval = 350
        Light003.State = 2 : StartFlipperTopGlow
    Else
        ' Les targets sont baissées → Light003 éteinte
        Light003.State = 0 : StopFlipperTopGlow
	End If
End Sub

' ====================== TARGETS CENTRALES ======================

Sub target001_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 140, DOFPulse, DOFTargets)   ' Son principal
                                            
    target001.IsDropped = True
    UpdateEndTargetsLight
    
    LastSwitchHit = "target001"
    
    ' Si on est en phase 2 d'un mode
    If ModeStep = 2 Then
        CheckWinMode
    End If
End Sub

Sub target002_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 141, DOFPulse, DOFTargets)
    
    target002.IsDropped = True
    UpdateEndTargetsLight
   
    LastSwitchHit = "target002"

    ' === APPEL WINMODE UNIQUEMENT EN PHASE 2 D'UN MODE ===
    If Mode(CurrentPlayer, 0) > 0 And ModeStep = 2 Then
        WinMode
    End If
End Sub

Sub InstantInfo
    Dim tmp
    DMD CL("INSTANT INFO"), "", "", eNone, eNone, eNone, 1000, True, ""
    Select Case Mode(CurrentPlayer, 0)
        Case 0 ' no CHALLENGE active
        Case 1 ' Apollo
            DMD CL("CURRENT MODE"), CL("APOLLO"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE LIGHTS"), CL("AND TARGETS TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 2 ' Clubber
            DMD CL("CURRENT MODE"), CL("CLUBER"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE LIGHTS"), CL("AND TARGETS TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 3 ' Drago
            DMD CL("CURRENT MODE"), CL("DRAGO"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE LIGHTS"), CL("AND TARGETS TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 4 ' DRAGO
            DMD CL("CURRENT MODE"), CL("DRAGO"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE LIGHTS"), CL("AND TARGETS TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 5 ' TOMMY
            DMD CL("CURRENT MODE"), CL("TOMMY"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT THE SPINNERS"), CL("AND TARGETS TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 6 ' MASON
            DMD CL("CURRENT MODE"), CL("MASON"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT LIT TARGETS"), CL("AND TARGETS TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 7 ' CONLAN
            DMD CL("CURRENT MODE"), CL("CONLAN"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT LIT TARGETS"), "", "", eNone, eNone, eNone, 2000, False, ""
        Case 8 ' VIKTORs
            DMD CL("CURRENT MODE"), CL("VIKTOR"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT LIT TARGETS"), CL("AND TARGETS TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
        Case 9 ' DAMIAN
            DMD CL("CURRENT MODE"), CL("DAMIAN"), "", eNone, eNone, eNone, 2000, False, ""
            DMD CL("SHOOT LIT TARGETS"), CL("AND TARGETS TO FINISH"), "", eNone, eNone, eNone, 2000, False, ""
    End Select

    DMD CL("YOUR SCORE"), CL(FormatScore(Score(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("EXTRA BALLS"), CL(ExtraBallsAwards(CurrentPlayer)), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("BONUS MULTIPLIER"), CL(FormatScore(BonusMultiplier(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("PLAYFIELD MULTIPLIER"), CL(FormatScore(PlayfieldMultiplier(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SKILLSHOT VALUE"), CL(FormatScore(SkillshotValue(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SR SKILLSHOT VALUE"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("TARGETS HIT"), CL(FormatScore(BonusTargets(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("RAMPS HIT"), CL(FormatScore(BonusRamps(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("LOOPS HIT"), CL(FormatScore(BonusLOOPS(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("COMBO VALUE"), CL(FormatScore(ComboValue(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("X HITS"), CL(FormatScore(BonusXHits(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("COMBATS COMPLETED"), CL(TotalMISSIONS(CurrentPlayer)), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("LOVED COMPLETED"), CL(TotalTEAMS(CurrentPlayer)), "", eNone, eNone, eNone, 2000, False, ""
    If Score(1)Then
        DMD CL("PLAYER 1 SCORE"), CL(FormatScore(Score(1))), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(2)Then
        DMD CL("PLAYER 2 SCORE"), CL(FormatScore(Score(2))), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(3)Then
        DMD CL("PLAYER 3 SCORE"), CL(FormatScore(Score(3))), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(4)Then
        DMD CL("PLAYER 4 SCORE"), CL(FormatScore(Score(4))), "", eNone, eNone, eNone, 2000, False, ""
    End If
End Sub

Sub StopMBmodes 'stop multiball modes after loosing the last multibal
    bDRAGOMBStarted = False
    MedussaX = 1
   ' If Mode(CurrentPlayer, 0) = 9 Then StopMode 
    If bBalboaMBStarted(CurrentPlayer) Then
        bBalboaMBStarted(CurrentPlayer) = False
        'If Mode(CurrentPlayer, 0) <> 7 Then 'Not in the VIKTOR mode
           
        'End If
    End If
    bChampionnatMBStarted(CurrentPlayer) = False
    zeusMBFlashTimer.Enabled = 0
End Sub

Sub StopEndOfBallMode()      'this sub is called after the last ball in play is drained, modes, timers
    StopMode                 'stop current mode
    'TrapdoorDown
    LightSeqBumpers.StopPlay 'in case it was on.
End Sub

Sub ResetNewBallVariables()
    Mode(CurrentPlayer, 0) = 0
    ModeStep = 0
    bModeReady(CurrentPlayer) = False
    li038.State = 0 

	' === RESET BALBOA MULTIBALL ===
    bBalboaMBStarted(CurrentPlayer) = False
    Balboa1 = 0
    Balboa2 = 0
    Balboa3 = 0
    BallsInLock(CurrentPlayer) = 0      ' ← Compteur de lock remis à zéro
    bLockEnabled = False
    li039.State = 0 : li080.State = 0 : li081.State = 0 : li082.State = 0 : light013.State = 0

	' === RESET KICKBACK ===
	PunchHits(CurrentPlayer) = 0
	Light002.State = 0
	gatekf.RotateToStart
	leftoutlane.Enabled = 1

    LotteryLitThisBall(CurrentPlayer) = False
    target001.IsDropped = True
    target002.IsDropped = True
    UpdateEndTargetsLight

    bBalboaMBStarted(CurrentPlayer) = False
    Balboa1 = 0
    Balboa2 = 0
    Balboa3 = 0
    PunchHits(CurrentPlayer) = 0
End Sub

'==============================================================
' NOUVELLE FONCTION - Reconstruit les lumières selon le joueur
'==============================================================
Sub UpdatePlayerLights()

    ' === Modes ===
    Select Case Mode(CurrentPlayer, 1)
        Case 1: li013.State = 1
        Case 2: li013.State = 2
        Case Else: li013.State = 0
    End Select

    Select Case Mode(CurrentPlayer, 2)
        Case 1: li011.State = 1
        Case 2: li011.State = 2
        Case Else: li011.State = 0
    End Select

    Select Case Mode(CurrentPlayer, 3)
        Case 1: li012.State = 1
        Case 2: li012.State = 2
        Case Else: li012.State = 0
    End Select

    Select Case Mode(CurrentPlayer, 4)
        Case 1: li014.State = 1
        Case 2: li014.State = 2
        Case Else: li014.State = 0
    End Select

    Select Case Mode(CurrentPlayer, 5)
        Case 1: li009.State = 1
        Case 2: li009.State = 2
        Case Else: li009.State = 0
    End Select

    Select Case Mode(CurrentPlayer, 6)
        Case 1: li007.State = 1
        Case 2: li007.State = 2
        Case Else: li007.State = 0
    End Select

    Select Case Mode(CurrentPlayer, 7)
        Case 1: li010.State = 1
        Case 2: li010.State = 2
        Case Else: li010.State = 0
    End Select

    Select Case Mode(CurrentPlayer, 8)
        Case 1: li008.State = 1
        Case 2: li008.State = 2
        Case Else: li008.State = 0
    End Select

    Select Case Mode(CurrentPlayer, 9)
        Case 1: li049.State = 1
        Case 2: li049.State = 2
        Case Else: li049.State = 0
    End Select

    ' === Championnat ===
    If ChampionnatCount(CurrentPlayer) >= 1 Then li051.State = 1
    If ChampionnatCount(CurrentPlayer) >= 2 Then li052.State = 1
    If ChampionnatCount(CurrentPlayer) >= 3 Then li053.State = 1
    If ChampionnatCount(CurrentPlayer) >= 4 Then li054.State = 1
    If ChampionnatCount(CurrentPlayer) >= 5 Then li055.State = 1

        ' === Philadelphia ===
    If bPhiladelphiaMBPlayed(CurrentPlayer) = False Then
        If SavedPhil015(CurrentPlayer) > 0 Then li015.State = 1 Else li015.State = 0
        If SavedPhil016(CurrentPlayer) > 0 Then li016.State = 1 Else li016.State = 0
        If SavedPhil074(CurrentPlayer) > 0 Then li074.State = 1 Else li074.State = 0
        If SavedPhil019(CurrentPlayer) > 0 Then li019.State = 1 Else li019.State = 0
        If SavedPhil020(CurrentPlayer) > 0 Then li020.State = 1 Else li020.State = 0
        If SavedPhil021(CurrentPlayer) > 0 Then li021.State = 1 Else li021.State = 0
        If SavedPhil022(CurrentPlayer) > 0 Then li022.State = 1 Else li022.State = 0
        If SavedPhil023(CurrentPlayer) > 0 Then li023.State = 1 Else li023.State = 0
        If SavedPhil073(CurrentPlayer) > 0 Then li073.State = 1 Else li073.State = 0
        If SavedPhil018(CurrentPlayer) > 0 Then li018.State = 1 Else li018.State = 0
        If SavedPhil072(CurrentPlayer) > 0 Then li072.State = 1 Else li072.State = 0
        If SavedPhil017(CurrentPlayer) > 0 Then li017.State = 1 Else li017.State = 0
    Else
        li015.State = 0 : li016.State = 0 : li074.State = 0
        li019.State = 0 : li020.State = 0 : li021.State = 0
        li022.State = 0 : li023.State = 0
        li073.State = 0 : li018.State = 0 : li072.State = 0 : li017.State = 0
    End If

    ' === Achievements ===
    If bCombosAchieved(CurrentPlayer) Then li035.State = 1
    If bLovedAchieved(CurrentPlayer) Then li075.State = 1
    If bPhiladelphiaAchieved(CurrentPlayer) Then li076.State = 1
    If bChampionnatAchieved(CurrentPlayer) Then li077.State = 1
    If bAllCombatsAchieved(CurrentPlayer) Then li078.State = 1

	    UpdateCombatLights()

End Sub

    BumperLights 0
    'set up the lights according to the player achievments
    UpdateModeLights
    BonusMultiplier(CurrentPlayer) = 1
    PlayfieldMultiplier(CurrentPlayer) = 1
    Light007.State = 1
    bJackpotsEnabled = False
    If BallsInLock(CurrentPlayer)Then
        'li039.State = 2 
        bLockEnabled = True
    End If
    BumperHits = 0       
    BumperAward = 5000
    gatekf.RotateToStart 
    leftoutlane.Enabled = 1
    If bGRADESStarted(CurrentPlayer) Then
        bGRADESStarted(CurrentPlayer) = False
        GRADESHits(CurrentPlayer) = 0
        GRADESMultiplier = 1
    End If


Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub BumperLights(stat)
    Dim x
    For each x in aBumperLights
        x.State = stat
    Next
End Sub

Sub TurnOffXlights 'turn off the other lights after selecting a X light
    If li024.State = 2 Then li024.State = 0
    If li025.State = 2 Then li025.State = 0
    If li026.State = 2 Then li026.State = 0
    If li027.State = 2 Then li027.State = 0
    If li028.State = 2 Then li028.State = 0
End Sub

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
    LightSeqSkillshot.Play SeqAllOff
    DMD CL("HIT LIT LIGHT"), CL("FOR SKILLSHOT"), "", eNone, eNone, eNone, 3000, True, ""
    li044.State = 2
    li040.State = 2
    BumperLights 2            'blinking
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    bRotateLights = True
    LightSeqSkillshot.StopPlay
    li044.State = 0
    li040.State = 0
    BumperLights 1 'on
    DMDScoreNow
End Sub

Sub CheckSkillshot
    If bSkillShotReady Then
        If BumperHits >= 3 Then
            AwardSkillshot
        End If
    End If
End Sub

Sub CheckSuperSkillshot
    If bSkillShotReady Then
        If LastSwitchHit = "hurrican" Then
            AwardSuperSkillshot
        End If
    End If
End Sub

Sub CheckFreakySkillshot
    If bSkillShotReady Then
        If LastSwitchHit = "swPlungerRest" Then
            AwardFreakySkillshot
        End If
    End If
End Sub

'********************
' Flasher light seq.
'********************

Sub RBF 'right bottom Flasher
    LightSeqRBF.Play SeqBlinking, , 8, 40
    DOF 301, DOFPulse
End Sub

Sub RMF
    LightSeqRMF.Play SeqBlinking, , 8, 40
    DOF 304, DOFPulse
End Sub

Sub RTF
    LightSeqRTF.Play SeqBlinking, , 8, 40
    DOF 307, DOFPulse
End Sub

Sub LBF 'left bottom Flasher
    LightSeqLBF.Play SeqBlinking, , 8, 40
    DOF 310, DOFPulse
End Sub

Sub LMF
    LightSeqLMF.Play SeqBlinking, , 8, 40
    DOF 313, DOFPulse
End Sub

Sub LTF
    LightSeqLTF.Play SeqBlinking, , 8, 40
    DOF 316, DOFPulse
End Sub

Sub BALBOAF

    DOF 319, DOFPulse
End Sub

Sub ChampionnatF
LightSeqZeusF.Play SeqRandom, 1, , 1500
    DOF 322, DOFPulse
End Sub

Sub FlashEffect(n)
    Select Case n
        Case 1 'all blink
            LightSeqRBF.Play SeqBlinking, , 8, 40:DOF 301, DOFPulse
            LightSeqRMF.Play SeqBlinking, , 8, 40:DOF 304, DOFPulse
            LightSeqRTF.Play SeqBlinking, , 8, 40:DOF 307, DOFPulse
            LightSeqLBF.Play SeqBlinking, , 8, 40:DOF 310, DOFPulse
            LightSeqLMF.Play SeqBlinking, , 8, 40:DOF 313, DOFPulse
            LightSeqLTF.Play SeqBlinking, , 8, 40:DOF 316, DOFPulse
          
        Case 2 'random
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqRBF.Play SeqBlinking, , 5, 40: DOF 302, DOFPulse '"
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqRMF.Play SeqBlinking, , 5, 40: DOF 305, DOFPulse '"
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqRTF.Play SeqBlinking, , 5, 40: DOF 308, DOFPulse '"
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqLBF.Play SeqBlinking, , 5, 40: DOF 311, DOFPulse '"
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqLMF.Play SeqBlinking, , 5, 40: DOF 314, DOFPulse '"
            vpmtimer.addtimer RndNbr(6) * 200, "LightSeqLTF.Play SeqBlinking, , 5, 40: DOF 317, DOFPulse '"
           
        Case 3 'all blink fast
            LightSeqRBF.Play SeqBlinking, , 4, 30:DOF 302, DOFPulse
            LightSeqRMF.Play SeqBlinking, , 4, 30:DOF 305, DOFPulse
            LightSeqRTF.Play SeqBlinking, , 4, 30:DOF 308, DOFPulse
            LightSeqLBF.Play SeqBlinking, , 4, 30:DOF 311, DOFPulse
            LightSeqLMF.Play SeqBlinking, , 4, 30:DOF 314, DOFPulse
            LightSeqLTF.Play SeqBlinking, , 4, 30:DOF 317, DOFPulse
           
        Case 4 'center
            vpmtimer.addtimer 800, "LightSeqRBF.Play SeqBlinking, , 6, 30: DOF 302, DOFPulse '"
            vpmtimer.addtimer 400, "LightSeqRMF.Play SeqBlinking, , 6, 30: DOF 305, DOFPulse '"
            vpmtimer.addtimer 800, "LightSeqRTF.Play SeqBlinking, , 6, 30: DOF 308, DOFPulse '"
            vpmtimer.addtimer 800, "LightSeqLBF.Play SeqBlinking, , 6, 30: DOF 311, DOFPulse '"
            vpmtimer.addtimer 400, "LightSeqLMF.Play SeqBlinking, , 6, 30: DOF 314, DOFPulse '"
            vpmtimer.addtimer 800, "LightSeqLTF.Play SeqBlinking, , 6, 30: DOF 317, DOFPulse '"

        Case 5 'top down
            vpmtimer.addtimer 200, "LightSeqRBF.Play SeqBlinking, , 2, 40: DOF 303, DOFPulse '"
            vpmtimer.addtimer 100, "LightSeqRMF.Play SeqBlinking, , 2, 40: DOF 306, DOFPulse '"
            LightSeqRTF.Play SeqBlinking, , 2, 40:DOF 309, DOFPulse
            vpmtimer.addtimer 200, "LightSeqLBF.Play SeqBlinking, , 2, 40: DOF 312, DOFPulse '"
            vpmtimer.addtimer 100, "LightSeqLMF.Play SeqBlinking, , 2, 40: DOF 315, DOFPulse '"
            LightSeqLTF.Play SeqBlinking, , 2, 40:DOF 318, DOFPulse
 
        Case 6 'down to top
            LightSeqRBF.Play SeqBlinking, , 2, 40:DOF 303, DOFPulse
            vpmtimer.addtimer 100, "LightSeqRMF.Play SeqBlinking, , 2, 40: DOF 306, DOFPulse '"
            vpmtimer.addtimer 200, "LightSeqRTF.Play SeqBlinking, , 2, 40: DOF 309, DOFPulse '"
            LightSeqLBF.Play SeqBlinking, , 2, 40:DOF 312, DOFPulse
            vpmtimer.addtimer 100, "LightSeqLMF.Play SeqBlinking, ,2, 40: DOF 315, DOFPulse '"
            vpmtimer.addtimer 200, "LightSeqLTF.Play SeqBlinking, , 2, 40: DOF 318, DOFPulse '"

        Case 7 'circle 2 rounds
            vpmtimer.addtimer 250, "LightSeqRBF.Play SeqBlinking, , 1, 40: DOF 303, DOFPulse '"
            vpmtimer.addtimer 200, "LightSeqRMF.Play SeqBlinking, , 1, 40: DOF 306, DOFPulse '"
            vpmtimer.addtimer 150, "LightSeqRTF.Play SeqBlinking, , 1, 40: DOF 309, DOFPulse '"
            LightSeqLBF.Play SeqBlinking, , 1, 40:DOF 312, DOFPulse
            vpmtimer.addtimer 50, "LightSeqLMF.Play SeqBlinking, , 1, 40: DOF 315, DOFPulse '"
            vpmtimer.addtimer 100, "LightSeqLTF.Play SeqBlinking, , 1, 40: DOF 318, DOFPulse '"
            vpmtimer.addtimer 550, "LightSeqRBF.Play SeqBlinking, , 1, 40: DOF 303, DOFPulse '"
            vpmtimer.addtimer 500, "LightSeqRMF.Play SeqBlinking, , 1, 40: DOF 306, DOFPulse '"
            vpmtimer.addtimer 450, "LightSeqRTF.Play SeqBlinking, , 1, 40: DOF 309, DOFPulse '"
            vpmtimer.addtimer 300, "LightSeqLBF.Play SeqBlinking, , 1, 40: DOF 312, DOFPulse '"
            vpmtimer.addtimer 350, "LightSeqLMF.Play SeqBlinking, , 1, 40: DOF 315, DOFPulse '"
            vpmtimer.addtimer 400, "LightSeqLTF.Play SeqBlinking, , 1, 40: DOF 318, DOFPulse '"
    End Select


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
    DOF 144, DOFPulse
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 530
    ' check modes
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
    PlaySoundAt SoundFXDOF("fx_slingshot", 104, DOFPulse, DOFcontactors), Remk
    DOF 145, DOFPulse
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

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    Dim tmp
    PlaySoundAt SoundFXDOF("fx_bumper", 105, DOFPulse, DOFContactors), Bumper1
    DOF 147, DOFPulse : pupevent 819
    BumperHits = BumperHits + 1
    AddScore BumperAward
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper1"
    'checkmodes this switch is part of
    CheckSkillshot
    CheckBumperHits
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    Dim tmp
    PlaySoundAt SoundFXDOF("fx_bumper", 106, DOFPulse, DOFContactors), Bumper2
    DOF 147, DOFPulse : pupevent 819
    BumperHits = BumperHits + 1
    AddScore BumperAward
    ' remember last trigger hit by the ball
    LastSwitchHit = "Bumper2"
    'checkmodes this switch is part of
    CheckSkillshot
    CheckBumperHits
End Sub

Sub CheckBumperHits
    If BumperHits MOD 20 = 0 Then 'add a ball if in multiball Mode
        If bMultiBallMode Then
            DMD "_", CL("ADD A BALL"), "_", eNone, eNone, eNone, 1500, True, ""
            AddMultiball 1
        End If
    End If
    If BumperHits MOD 25 = 0 Then 'activate chain lightning, bumpers score 4x
        BumperAward = BumperAward * 4
        DMD CL("THE PEAK OF FIGHT"), CL("BUMPER VALUE " &FormatScore(BumperAward)), "_", eNone, eNone, eNone, 3000, True, "" : pupevent 819
        FlashEffect RndNbr(7)
        LightSeqBumpers.Play SeqRandom, 10, , 1000
    End If
End Sub

Sub LightSeqBumpers_PlayDone()
    LightSeqBumpers.Play SeqRandom, 10, , 1000
End Sub
'*********
' Lanes
'*********
' in and outlanes
Sub leftoutlane_Hit
    PLaySoundAt "fx_sensor", leftoutlane
    If Tilted Then Exit Sub
    'score & bonus
    AddScore 250000
    If activeball.VelY > 0 Then 'only when going down
        If li002.State Then     
            PlaySound ""
            BallSaverTime = BallSaverTime -10
        End If
    End If
    LastSwitchHit = "leftoutlane"
End Sub

Sub leftinlane_Hit
    PLaySoundAt "fx_sensor", leftinlane
    If Tilted Then Exit Sub
    'score & bonus
    AddScore 100000
    If li003.State = 0 Then 'count the trees that are being lit up
        li003.State = 1
        TreeHits(CurrentPlayer) = TreeHits(CurrentPlayer) + 1
        CheckTrees
    End If
'LastSwitchHit = "leftinlane"
End Sub

Sub rightinlane_Hit
    PLaySoundAt "fx_sensor", rightinlane
    If Tilted Then Exit Sub
    'score & bonus
    AddScore 100000
    If li004.State = 0 Then 'count the trees that are being lit up
        li004.State = 1
        TreeHits(CurrentPlayer) = TreeHits(CurrentPlayer) + 1
        CheckTrees
    End If
'LastSwitchHit = "rightinlane"
End Sub

Sub rightoutlane_Hit
    PLaySoundAt "fx_sensor", rightoutlane
    If Tilted Then Exit Sub
    'score & bonus
    AddScore 250000
    If li005.State Then 
        PlaySound ""
        BallSaverTime = BallSaverTime -10
        DMD CL("BALLSAVE DECREASED"), CL("IT IS NOW " &BallSaverTime& " SEC"), "", eNone, eNone, eNone, 2000, True, ""
    End If
    LastSwitchHit = "rightoutlane"
End Sub

Sub CheckTrees                               'check for 6 treehits and if all the lights are lit
    If TreeHits(CurrentPlayer)MOD 6 = 0 Then 'every 6 trees adds 3 seconds to the ball saver value
        If BallSaverTime < 50 then
            BallSaverTime = BallSaverTime + 3
            DMD CL("BALLSAVE INCREASED"), CL("IT IS NOW " &BallSaverTime& " SEC"), "", eNone, eNone, eNone, 2000, True, ""
        End If
    End If
    If li002.State + li003.State + li004.State + li005.State = 4 Then 'all lights are lit then turn them off
        li002.State = 0
        li003.State = 0
        li004.State = 0
        li005.State = 0
        LightSeqLanes.Play SeqRandom, 4, , 1000
    End If
End Sub

' loops

Sub hurrican_Hit
    PLaySoundAt "fx_sensor", hurrican
    If Tilted Then Exit Sub
    'score & bonus
    AddScore 100000 * MedussaX
    If li024.State = 1 Then
        Addscore 100000 * MedussaX 'double the score
    End If
    If li024.State = 2 Then
        li024.State = 1 : pupevent 873
        TurnOffXlights
    End If
    If li030.State = 0 Then
        li030.State = 1
        CheckBonusX
    End If
    'Modes
    Select Case Mode(CurrentPlayer, 0)
        Case 1:li060.State = 0:CheckWinMode
        Case 2:li060.State = 0:CheckWinMode
		Case 3:li060.State = 0:CheckWinMode
		Case 4:li060.State = 0:CheckWinMode
		Case 5:li060.State = 0:CheckWinMode
		Case 6:li060.State = 0:CheckWinMode
		Case 7:li060.State = 0:CheckWinMode
		Case 8:li060.State = 0:CheckWinMode
		Case 9:li060.State = 0:CheckWinMode
		

	Case 4
            If ModeStep = 1 Then
                CheckWinMode
            End If
    End Select
    ' ==================== COMBO SYSTEM ====================
    If activeBall.VelY < 0 Then   ' balle qui monte = bon sens pour combo
        If LastSwitchHit = "tramp" OR LastSwitchHit = "lramp" OR _
           LastSwitchHit = "rramp" OR LastSwitchHit = "hurrican" OR _
           LastSwitchHit = "upperloop1" OR LastSwitchHit = "upperloop4" Then
           
            AwardCombo
        
        End If
    
    End If
    LastSwitchHit = "hurrican"
End Sub

Sub upperloop1_Hit
    PLaySoundAt "fx_sensor", upperloop1
    If Tilted Then Exit Sub
    'score & bonus
    AddScore 100000 * MedussaX
    'Modes
    Select Case Mode(CurrentPlayer, 0)
        Case 1:li037.State = 0:CheckWinMode
        Case 2:li037.State = 0:CheckWinMode
		Case 3:li037.State = 0:CheckWinMode
		Case 4:li037.State = 0:CheckWinMode
		Case 5:li037.State = 0:CheckWinMode
		Case 6:li037.State = 0:CheckWinMode
		Case 7:li037.State = 0:CheckWinMode
		Case 8:li037.State = 0:CheckWinMode
		

		
	Case 4
            If ModeStep = 3 Then
                CheckWinMode
            End If
	End Select
    ' ==================== COMBO SYSTEM ====================
    If activeBall.VelY < 0 Then   ' balle qui monte = bon sens pour combo
        If LastSwitchHit = "tramp" OR LastSwitchHit = "lramp" OR _
           LastSwitchHit = "rramp" OR LastSwitchHit = "hurrican" OR _
           LastSwitchHit = "upperloop1" OR LastSwitchHit = "upperloop4" Then
           
            AwardCombo
        Else
            
		End If
    End If
    LastSwitchHit = "upperloop1"
End Sub

Sub upperloop2_Hit
    PLaySoundAt "fx_sensor", upperloop2
    If Tilted Then Exit Sub
    'score & bonus
    AddScore 100000 * MedussaX
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 0:OrbitHits = OrbitHits + 1:CheckStartModes
    End Select
    'DRAGO MB
    If bDRAGOMBStarted Then
        ExtraBallHits = ExtraBallHits + 1
        CheckExtraBallHits
    End If
    LastSwitchHit = "upperloop2"
End Sub

Sub upperloop3_Hit
    PLaySoundAt "fx_sensor", upperloop3
    If Tilted Then Exit Sub
    
    ' === SCORE NORMAL ===
    AddScore 100000 * MedussaX
    
    ' === LOVED ONES JACKPOT UNIQUEMENT SI LES 5 LUMIÈRES SONT ALLUMÉES FIXES (State = 1) ===
    If li024.State = 1 AND li025.State = 1 AND li026.State = 1 AND _
       li027.State = 1 AND li028.State = 1 Then
        
        AddScore2 5000000
        DMD CL("LOVED ONES JACKPOT"), CL(FormatScore(5000000)), "_", eNone, eBlinkFast, eNone, 2500, True, "vo_jackpot"
        'li035.State = 2
        LightEffect 2
        FlashEffect 1
        GiEffect 1
        
        
    End If
    
    LastSwitchHit = "upperloop3"
    CheckFreakySkillshot
    
    'Modes
    Select Case Mode(CurrentPlayer, 0)
        Case 1: li044.State = 0: CheckWinMode
        Case 4
            If ModeStep = 4 Then
                CheckWinMode
            End If
    End Select
End Sub

Sub upperloop4_Hit
    PLaySoundAt "fx_sensor", upperloop4
    If Tilted Then Exit Sub
    'score & bonus
    AddScore 100000 * MedussaX
    'Modes
    If li028.State = 1 Then
        Addscore 100000 'double the score
    End If
    If li028.State = 2 Then
        li028.State = 1 : pupevent 877
        TurnOffXlights
    End If
    If li034.State = 0 Then
        li034.State = 1
        CheckBonusX
    End If
    Select Case Mode(CurrentPlayer, 0)
        Case 1:li041.State = 0:CheckWinMode
        Case 2:li041.State = 0:CheckWinMode
		Case 3:li041.State = 0:CheckWinMode
		Case 4:li041.State = 0:CheckWinMode
		Case 5:li041.State = 0:CheckWinMode
		Case 6:li041.State = 0:CheckWinMode
		Case 7:li041.State = 0:CheckWinMode
		Case 8:li041.State = 0:CheckWinMode
		Case 9:li041.State = 0:CheckWinMode
           


	Case 4
            If ModeStep = 2 Then
                CheckWinMode
            End If
    End Select
    ' ==================== COMBO SYSTEM ====================
    If activeBall.VelY < 0 Then   ' balle qui monte = bon sens pour combo
        If LastSwitchHit = "tramp" OR LastSwitchHit = "lramp" OR _
           LastSwitchHit = "rramp" OR LastSwitchHit = "hurrican" OR _
           LastSwitchHit = "upperloop1" OR LastSwitchHit = "upperloop4" Then
           
            AwardCombo
        
        End If
    
    End If
    LastSwitchHit = "upperloop4"
End Sub

'ramps completed
Sub lramp_Hit
    PLaySoundAt "fx_sensor", lramp
    If Tilted Then Exit Sub
    'score & bonus
    If Light009.State Then
        AwardExtraBall
    End If
    
    If bJackpotsEnabled Then AwardJackpot
    'GRADES
    If bGRADESStarted(CurrentPlayer) Then 'change the scoring multiplier
        light006.State = 0
        Light007.State = 0
        Light008.State = 0
        Select Case RndNbr(3)
            Case 1:GRADESMultiplier = 0.5:light006.State = 1
            Case 2:GRADESMultiplier = 1:light007.State = 1
            Case 3:GRADESMultiplier = 2:light008.State = 1
        End Select
    Else
        GRADESHits(CurrentPlayer) = GRADESHits(CurrentPlayer) + 1
        CheckGRADES
    End If
    'fire & X lights
    If li025.State = 1 Then
        AddScore 100000
    End If
    If li025.State = 2 Then
        li025.State = 1 : pupevent 874
        TurnOffXlights
    End If
    If li031.State = 0 Then
        li031.State = 1
        CheckBonusX
    End If
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 0
            RampHits = RampHits + 1:CheckStartModes
Case 1 ' Apollo
    If li036.State = 2 Then
        li036.State = 0        
CheckWinMode
End If
Case 4 ' Drago
    If li036.State = 2 Then
        li036.State = 0
CheckWinMode
End If
Case 5 ' Tommy
    If li036.State = 2 Then
        li036.State = 0        
CheckWinMode
End If
Case 8 ' Viktor
    If li036.State = 2 Then
        li036.State = 0
CheckWinMode
End If 
Case 9 ' Damian
     If li036.State = 2 Then 
         li036.State = 0
 CheckWinMode
    End If
        Case 3:li036.State = 0:CheckWinMode
        Case 4
            If ModeStep = 2 Then
                CheckWinMode
            End If
        Case 9
            AwardJackpot
    End Select
    ' ==================== COMBO SYSTEM ====================
    If activeBall.VelY < 0 Then   ' balle qui monte = bon sens pour combo
        If LastSwitchHit = "tramp" OR LastSwitchHit = "lramp" OR _
           LastSwitchHit = "rramp" OR LastSwitchHit = "hurrican" OR _
           LastSwitchHit = "upperloop1" OR LastSwitchHit = "upperloop4" Then
           
            AwardCombo
        
        End If
    
    End If
    LastSwitchHit = "lramp"
End Sub

Sub rramp_Hit
    PLaySoundAt "fx_sensor", rramp
    If Tilted Then Exit Sub
    'score & bonus
    CheckSuperSkillshot
    If Light010.State Then
        AwardSpecial
    End If
    If bJackpotsEnabled Then AwardJackpot
    If li027.State = 1 Then
        AddScore 100000 'double the score
    End If
    If li027.State = 2 Then
        li027.State = 1 : pupevent 876
        TurnOffXlights
    End If
    If li033.State = 0 Then
        li033.State = 1
        CheckBonusX
    End If
   ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 0
            RampHits = RampHits + 1:CheckStartModes
Case 2 ' Cluber
    If li040.State = 2 Then
        li040.State = 0
CheckWinMode	
End If	
Case 4 ' Drago
    If li040.State = 2 Then
        li040.State = 0
CheckWinMode	
End If
Case 6 ' Mason
    If li040.State = 2 Then
        li040.State = 0
CheckWinMode	
End If
Case 8 ' Viktor
    If li040.State = 2 Then
        li040.State = 0        
CheckWinMode
    End If
Case 9 ' Damian
            If li040.State = 2 Then
        li040.State = 0        
CheckWinMode
End If       
		Case 3:li040.State = 0:CheckWinMode
        Case 9
            AwardJackpot
    End Select
   ' ==================== COMBO SYSTEM ====================
    If activeBall.VelY < 0 Then   ' balle qui monte = bon sens pour combo
        If LastSwitchHit = "tramp" OR LastSwitchHit = "lramp" OR _
           LastSwitchHit = "rramp" OR LastSwitchHit = "hurrican" OR _
           LastSwitchHit = "upperloop1" OR LastSwitchHit = "upperloop4" Then
           
            AwardCombo
        
        End If
    
    End If
    LastSwitchHit = "rramp"
End Sub


Sub tramp_Hit
    PLaySoundAt "fx_sensor", tramp
    If Tilted Then Exit Sub
LTF:RTF
'score & bonus
If bDRAGOMBStarted Then
    AddScore 250000
Else
    AddScore 500000
End If
    If bJackpotsEnabled Then AwardJackpot
    If bChampionnatMBStarted(CurrentPlayer) Then AwardSuperJackpot
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 0
            RampHits = RampHits + 1:CheckStartModes
        Case 3:li043.State = 0:CheckWinMode
        Case 9
            AwardJackpot
    End Select
    LastSwitchHit = "tramp"
End Sub

'Combos
If LastSwitchHit = "tramp" OR LastSwitchHit = "lramp" OR _
   LastSwitchHit = "rramp" OR LastSwitchHit = "hurrican" OR _
   LastSwitchHit = "upperloop1" OR LastSwitchHit = "upperloop4" Then
   
    AwardCombo

End If

'Effect triggers

Sub Trigger001_Hit:LMF:End Sub
Sub Trigger002_Hit:LBF:End Sub
Sub Trigger003_Hit:RMF:End Sub
Sub Trigger004_Hit:RBF:End Sub



'***********
' Targets
'***********

' Philadelphia targets
Sub leftkb_Hit 
    PLaySoundAtBall SoundFXDOF("fx_Target", 124, DOFPulse, DOFTargets)
	If Tilted Then Exit Sub
    Addscore 10000
'   
          li015.State = 1
         AddScore 15000
			
    LastSwitchHit = "leftkb"
	CheckPhiladelphia
End Sub

Sub rightkb_Hit 'left upper kickback target
   PLaySoundAtBall SoundFXDOF("fx_Target", 125, DOFPulse, DOFTargets)
  If Tilted Then Exit Sub
    Addscore 10000
'    
                li074.State = 1
                AddScore 15000
                
    LastSwitchHit = "rightkb"
	CheckPhiladelphia
End Sub

'Philadelphia targets

Sub lmyst_Hit 'right upper Adrian target
    PLaySoundAtBall SoundFXDOF("fx_Target", 126, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
 '   
                li073.State = 1
                AddScore 15000
               
    LastSwitchHit = "lmyst"
	CheckPhiladelphia
End Sub

Sub rmyst_Hit 'right lower Adrian target
    PLaySoundAtBall SoundFXDOF("fx_Target", 127, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000

'   
                li017.State = 1
                AddScore 15000
              
    LastSwitchHit = "rmyst"
	CheckPhiladelphia
End Sub

Sub rightkb001_Hit 'left upper kickback target
    PLaySoundAtBall SoundFXDOF("fx_Target", 125, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
 '   
                li016.State = 1
                AddScore 15000
 
    LastSwitchHit = "rightkb001"
	CheckPhiladelphia
End Sub

Sub liup1_Hit 'left upper kickback target
    PLaySoundAtBall SoundFXDOF("fx_Target", 125, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
 
                li019.State = 1
                AddScore 15000
                
    LastSwitchHit = "liup1"
	CheckPhiladelphia
	CheckAdrian
End Sub

Sub liup2_Hit 'left upper kickback target
    PLaySoundAtBall SoundFXDOF("fx_Target", 125, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
 
                li020.State = 1
                AddScore 15000
 
    LastSwitchHit = "liup2"
	CheckPhiladelphia
End Sub

Sub liup3_Hit 'left upper kickback target
    PLaySoundAtBall SoundFXDOF("fx_Target", 125, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
 
                li021.State = 1
                AddScore 15000

    LastSwitchHit = "liup3"
	CheckPhiladelphia
End Sub

Sub liup4_Hit 'left upper kickback target
    PLaySoundAtBall SoundFXDOF("fx_Target", 125, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
 
                li022.State = 1
                AddScore 15000

    LastSwitchHit = "liup4"
	CheckPhiladelphia
End Sub

Sub liup5_Hit 'left upper kickback target
    PLaySoundAtBall SoundFXDOF("fx_Target", 125, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000

                li023.State = 1
                AddScore 15000
  
    LastSwitchHit = "liup5"
	CheckPhiladelphia
	CheckAdrian
End Sub

Sub lmyst001_Hit 'left upper kickback target
    PLaySoundAtBall SoundFXDOF("fx_Target", 125, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000

                li018.State = 1
                AddScore 15000
 
    LastSwitchHit = "lmyst001"
	CheckPhiladelphia
End Sub

Sub lmyst002_Hit 'left upper kickback target
    PLaySoundAtBall SoundFXDOF("fx_Target", 125, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 10000
 
                li072.State = 1
                AddScore 15000
 
    LastSwitchHit = "lmyst002"
	CheckPhiladelphia
End Sub

' Championnat targets
Sub ztgt_Hit
    PLaySoundAtBall SoundFXDOF("sfx_lightning1", 133, DOFPulse, DOFTargets)
	If Tilted Then Exit Sub
    Addscore 50000
    li061.State = 1
    TriggerGantsAnimation : pupevent 856
	ChampionnatF
    CheckStartModes
    LastSwitchHit = "ztgt"
End Sub

Sub etgt_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 134, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 50000
    TriggerGantsAnimation: pupevent 857
	li062.State = 1
    ChampionnatF
    CheckStartModes
    LastSwitchHit = "etgt"
End Sub

Sub utgt_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 135, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 50000
    li063.State = 1
    TriggerGantsAnimation : pupevent 858
	ChampionnatF
    CheckStartModes
    LastSwitchHit = "utgt"
End Sub

Sub stgt_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 136, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 50000
    li064.State = 1
    TriggerGantsAnimation : pupevent 859
	ChampionnatF
    CheckStartModes
    LastSwitchHit = "stgt"
End Sub

Sub xtgt_Hit
    PLaySoundAtBall SoundFXDOF("fx_Target", 136, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    Addscore 50000
    li050.State = 1
    TriggerGantsAnimation : pupevent 860
	ChampionnatF
    CheckStartModes
    LastSwitchHit = "xtgt"
End Sub

'*************
'  Spinners
'*************

Sub rspin_Spin 'right
    PlaySoundAt "fx_spinner", rspin
    DOF 129, DOFPulse
    If Tilted Then Exit Sub
    Addscore 10000
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5:SpinnerHits = SpinnerHits + 1:CheckWinMode
    End Select
End Sub

Sub lspin_Spin 'left
    PlaySoundAt "fx_spinner", lspin
    DOF 128, DOFPulse
    If Tilted Then Exit Sub
    Addscore 10000
    ' check modes
    Select Case Mode(CurrentPlayer, 0)
        Case 5:SpinnerHits = SpinnerHits + 1:CheckWinMode
    End Select
End Sub

'*********
' scoops
'*********

Sub scoop1_Hit 'BALBOA scoop
    PlaySoundAt "fx_hole_enter", scoop1
    scoop1.Destroyball
    BallsInHole = BallsInHole + 1
    
    If Tilted Then 
        vpmtimer.addtimer 500, "kickBallOut '"
        Exit Sub
    End If

    FlashEffect 7
    If bSkillShotReady Then ResetSkillShotTimer_Timer
    AddScore 5000

    ' === BALBOA MULTIBALL LOGIC ===
    If bBalboaMBStarted(CurrentPlayer) AND Balboa1 = 0 Then
        ' Pendant le multiball → Jackpot
        AwardJackpot
        Balboa1 = 1
        CheckBalboaMBHits
    Else
        ' On ne peut lock que s'il reste 1 bille maximum
        If BallsOnPlayfield <= 1 Then
            If NOT bLockEnabled Then 
                DMD "_", CL("LOCK IS LIT"), "_", eNone, eNone, eNone, 1500, True, "" : pupevent 810
                bLockEnabled = True
                li080.state = 0 : li081.state = 0 : li082.state = 0 : light013.State = 0
				li039.State = 2 
            Else 
                BallsInLock(CurrentPlayer) = BallsInLock(CurrentPlayer) + 1
                CheckBalboaMB
            End If
        Else
            ' Trop de billes en jeu → on ne fait rien (juste score)
        End If
    End If

    ' === PHASE FINALE DES MODES ===
    Select Case Mode(CurrentPlayer, 0)
        Case 1, 2, 3, 4, 5, 6, 8
            If ModeStep >= 2 Then
                ' On ne relève plus les cibles ici
                vpmtimer.addtimer 800, "kickBallOut '"
            Else
                vpmtimer.addtimer 1500, "kickBallOut '"
            End If
        Case Else
            vpmtimer.addtimer 1500, "kickBallOut '"
    End Select
End Sub

Sub RaiseEndTargets()
    target001.IsDropped = False
    target002.IsDropped = False
    UpdateEndTargetsLight
End Sub

'Sub target002_Hit
'    PlaySoundAtBall SoundFXDOF("fx_target", 141, DOFPulse, DOFTargets)
'    target002.IsDropped = True
'    
'    WinMode
'    vpmtimer.addtimer 800, "kickBallOut '"   ' sécurité
'End Sub

Sub kickBallOut 'from all the holes
    If BallsinHole > 0 Then
        BallsinHole = BallsInHole - 1
        PlaySoundAt SoundFXDOF("fx_popper", 106, DOFPulse, DOFcontactors), scoopexit
        DOF 130, DOFPulse
        scoopexit.CreateSizedBallWithMass BallSize / 2, BallMass
        scoopexit.kick 196, 28
        BALBOAF
        LightEffect 5
        vpmtimer.addtimer 1500, "kickBallOut '" 'kick out the rest of the balls, if any
    End If
End Sub

' hole2 - Start COMBATS

Sub scoop2_Hit 'Start COMBATS
    PlaySoundAt "fx_hole_enter", scoop2
    scoop2.Destroyball
    BallsinHole = BallsInHole + 1
    If Tilted Then vpmtimer.addtimer 500, "kickBallOut '":Exit Sub
    ' Modes
    Addscore 25000
    If li026.State = 1 Then
        AddScore 25000 'double the score
    End If
    If li026.State = 2 Then
        li026.State = 1 : pupevent 875
        TurnOffXlights
    End If
    If li032.State = 0 Then
        li032.State = 1
        CheckBonusX
    End If
    If bBalboaMBStarted(CurrentPlayer) AND Balboa2 = 0 Then
        AwardBalboaJackpot
        Balboa2 = 1
        CheckBalboaMBHits
    End If
    If bModeReady(CurrentPlayer) Then
    bModeReady(CurrentPlayer) = False
    li038.State = 0
    
    ' === COMBAT COMPTABILISÉ (avec limite à 9) ===
    If CombatCount(CurrentPlayer) < 9 Then
        CombatCount(CurrentPlayer) = CombatCount(CurrentPlayer) + 1
    End If
    
    UpdateCombatLights
    
    StartNextMode
Else
    vpmtimer.addtimer 500, "kickBallOut '"
End If

End Sub


'***********
' KICKBACK - Activé par 3 coups sur cball
'***********

Sub CheckKickbackProgress
    Select Case PunchHits(CurrentPlayer)
        Case 1
            DMD "_", CL("MORE 2 PUNCH"), "_", eNone, eBlink, eNone, 1500, True, ""
        Case 2
            DMD "_", CL("MORE 1 PUNCH"), "_", eNone, eBlink, eNone, 1500, True, ""
        Case 3
            DMD "_", CL("KICKBACK IS LIT"), "_", eNone, eBlinkFast, eNone, 2000, True, "" : pupevent 839
            Light002.State = 1
            PlaySoundAt "fx_diverter", gatekf
            gatekf.RotateToEnd
            leftoutlane.Enabled = 0
            LMF : LBF
    End Select
End Sub

Sub cball_Hit
    PlaySoundAtBall SoundFXDOF("fx_Target", 119, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    
    Addscore 75000
    LightEffect 3
    
    PunchHits(CurrentPlayer) = PunchHits(CurrentPlayer) + 1
    CheckKickbackProgress
    
    CBHits(CurrentPlayer) = CBHits(CurrentPlayer) + 1
    If CBHits(CurrentPlayer) MOD 25 = 0 Then
        DMD CL("FULL FIGHT"), CL(FormatScore(750000)), "_", eNone, eNone, eNone, 3000, True, ""
        Addscore2 750000
        li045.State = 1
    End If
    
    LastSwitchHit = "cball"
End Sub

Sub Kickback_Hit
    PlaySoundAt "fx_kicker_enter", Kickback
    vpmtimer.addtimer 1800, "KickbackEjectBall '"
End Sub

Sub KickbackEjectBall
    If Kickback.BallCntOver > 0 Then
        DOF 110, DOFPulse
        PlaySoundAt SoundFX("fx_kicker", DOFContactors), Kickback
        Kickback.Kick 0, 45
    End If
    
    ' === FERME LA GATE IMMÉDIATEMENT APRÈS L'ÉJECTION ===
    ResetKickback
End Sub

Sub ResetKickback
    Light002.State = 0
    gatekf.RotateToStart
    leftoutlane.Enabled = 1
    PunchHits(CurrentPlayer) = 0
    PlaySoundAt "fx_diverter", gatekf
End Sub


Sub StartFlipperTopGlow()
    FlipperGlowStep = 0
    RightFlipperTop.BlendDisableLighting = 0.8
    LeftFlipperTop.BlendDisableLighting = 0.8
	FlipperGlowTimer.Enabled = True
End Sub

Sub StopFlipperTopGlow()
    FlipperGlowTimer.Enabled = False
    RightFlipperTop.BlendDisableLighting = 0
	LeftFlipperTop.BlendDisableLighting = 0
End Sub

Sub FlipperGlowTimer_Timer()
    FlipperGlowStep = FlipperGlowStep + 0.06   ' Vitesse du clignotement (ajuste si besoin)
    
    ' Clignotement doux (va et vient)
    RightFlipperTop.BlendDisableLighting = 0.1 + 2.0 * Abs(Sin(FlipperGlowStep))
	LeftFlipperTop.BlendDisableLighting = 0.1 + 2.0 * Abs(Sin(FlipperGlowStep))
End Sub

'******************************
' ROCKY lotery - Extra awards
'******************************
'hole4 - lotery

Sub Adriank_Hit
    PlaySoundAt "fx_hole_enter", Adriank
    Adriank.Destroyball
    BallsinHole = BallsInHole + 1
    If Tilted Then vpmtimer.addtimer 500, "kickBallOut '":Exit Sub
    RMF:RBF
If li029.State Then
    StartAdrian
    AdriansHits(CurrentPlayer) = 0
    If AdriansNeeded(CurrentPlayer) < 6 Then
        AdriansNeeded(CurrentPlayer) = AdriansNeeded(CurrentPlayer) + 2
    End If
    li029.State = 0
Else
    vpmtimer.addtimer 1500, "kickBallOut '"
End If
End Sub

Sub CheckAdrian
    ' Vérifie si les deux targets sont allumées
    If li019.State = 1 AND li023.State = 1 Then
        
        ' On affiche le message UNE SEULE FOIS par bille
        If li029.State = 0 AND LotteryLitThisBall(CurrentPlayer) = False Then
            DMD "_", CL("LOTERY LIT"), "", eNone, eNone, eNone, 1500, True, ""
            pupevent 840
            li029.State = 1                    ' Allume la lumière de loterie
            LotteryLitThisBall(CurrentPlayer) = True          ' On bloque le message pour cette bille
        End If
        
    End If
    If AdriansHits(CurrentPlayer) >= AdriansNeeded(CurrentPlayer)Then
       FlashForms li056001, 3000, 100, 3
    Else
       FlashForms li056001, 3000, 100, 0
       
    End If
End Sub

Sub StartAdrian
    'do some animation
    DMDFlush
    DMD CL("LOTERY MYSTERY"), CL("LIGHT EXTRABALL"), "", eNone, eNone, eNone, 200, False, "" 
    DMD "_", CL("1 MILLION"), "", eNone, eNone, eNone, 200, False, ""
    DMD "_", CL("LIT SPECIAL"), "", eNone, eNone, eNone, 200, False, ""
    DMD "_", CL("5 MILLION"), "", eNone, eNone, eNone, 200, False, ""
    DMD "_", CL("START MODE"), "", eNone, eNone, eNone, 200, False, ""
    'give award
    Dim tmp
    Select case RndNbr(20)
        Case 1, 8 'light extraball
            If Light009.State = 0 Then
                DMD CL("LOTERY MYSTERY"), CL("EXTRA BALL IS LIT"), "_", eNone, eBlink, eNone, 2500, True, "" : pupevent 842
                Light009.State = 2
            Else
                DMD CL("LOTERY MYSTERY"), CL(FormatScore(250000)), "_", eNone, eBlink, eNone, 2000, True, ""
                Addscore 250000
            End If
        Case 2, 9 'light special
            If Light010.State = 0 Then
                DMD CL("LOTERY MYSTERY"), CL("SPECIAL IS LIT"), "_", eNone, eBlink, eNone, 2500, True, "" : pupevent 843
                Light010.State = 2
            Else
                DMD CL("LOTERY MYSTERY"), CL(FormatScore(250000)), "_", eNone, eBlink, eNone, 2000, True, ""
                Addscore2 250000
            End If
        Case 3, 10 'start 3 balls multiball, double playfield scores
            DMD CL("LOTERY MYSTERY"), CL("MULTIBALL"), "_", eNone, eBlinkFast, eNone, 1000, True, "" : pupevent 846
            DMD "_", CL("PLAYFIELD X 2"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            AddPlayfieldMultiplier 1
            AddMultiball 2
        Case 4, 11 'award from 250k to 5 million
            tmp = 250000 * RndNbr(20)
            DMD CL("LOTERY MYSTERY"), CL(FormatScore(tmp)), "_", eNone, eBlink, eNone, 2000, True, "" : pupevent 844
            Addscore2 tmp
        Case 5, 12 'increment bonus multiplier
            AddBonusMultiplier 1
            DMD CL("LOTERY MYSTERY"), CL("BONUS X " & BonusMultiplier(CurrentPlayer)), "_", eNone, eBlinkFast, eNone, 2000, True, "" : pupevent 844
        Case 6, 13 'add 10 pop bumper values
            BumperAward = BumperAward * 10
            DMD CL("LOTERY MYSTERY"), CL("BUMPERS 2X VALUE"), "_", eNone, eNone, eNone, 1000, True, "" : pupevent 845
            DMD CL("BUMPERS VALUE"), CL(FormatScore(BumperAward)), "_", eNone, eNone, eNone, 1500, True, ""
        Case 7, 14 '20 or more seconds ball saver
            StartthunderbMode
        Case Else  'hahaha just from 1000 to 25000 points
            tmp = 1000 * RndNbr(25)
            DMD CL("LOTERY MYSTERY"), CL(FormatScore(tmp)), "_", eNone, eBlink, eNone, 2000, True, "" : pupevent 841
            Addscore2 tmp
    End Select
    vpmtimer.addtimer 4500, "kickBallOut '"
End Sub

Sub StartthunderbMode
    
    DMD CL("SAVE MODE"), CL("FOR " &BallSaverTime& " SECONDS"), "_", eNone, eNone, eNone, 2000, True, ""
    EnableBallSaver BallSaverTime
    PlayThunder
    FlashEffect RndNbr(7)
End Sub

Sub CheckthunderbMode
    If li019.State + li020.State + li021.State + li022.State + li023.State = 5 Then
        thunderbModeHits(CurrentPlayer) = thunderbModeHits(CurrentPlayer) + 1
        If thunderbModeHits(CurrentPlayer) = thunderbModeNeeded(CurrentPlayer)Then
            StartthunderbMode
            thunderbModeHits(CurrentPlayer) = 0
            thunderbModeNeeded(CurrentPlayer) = thunderbModeNeeded(CurrentPlayer) + 1 'increase the number of times needed to start Perfect Mode
        End If
        li019.State = 0
        li020.State = 0
        li021.State = 0
        li022.State = 0
        li023.State = 0
        LightEffect 2
    End If
End Sub

Sub CheckLovedOnesJackpot
    
    ' === Chaque lumière allumée = +1 Team + sauvegarde ===
    If li024.State = 1 Then
        
		TotalTEAMS(CurrentPlayer) = TotalTEAMS(CurrentPlayer) + 1
        LovedOnesSaved(CurrentPlayer) = True
    End If
    If li025.State = 1 Then
        
		TotalTEAMS(CurrentPlayer) = TotalTEAMS(CurrentPlayer) + 1
        LovedOnesSaved(CurrentPlayer) = True
    End If
    If li026.State = 1 Then
        
		TotalTEAMS(CurrentPlayer) = TotalTEAMS(CurrentPlayer) + 1
        LovedOnesSaved(CurrentPlayer) = True
    End If
    If li027.State = 1 Then
        
		TotalTEAMS(CurrentPlayer) = TotalTEAMS(CurrentPlayer) + 1
        LovedOnesSaved(CurrentPlayer) = True
    End If
    If li028.State = 1 Then
        
		TotalTEAMS(CurrentPlayer) = TotalTEAMS(CurrentPlayer) + 1
        LovedOnesSaved(CurrentPlayer) = True
    End If

    ' === JACKPOT + MULTIBALL QUAND LES 5 LUMIÈRES SONT ALLUMÉES ===
    If li024.State = 1 AND li025.State = 1 AND li026.State = 1 AND li027.State = 1 AND li028.State = 1 Then
        
        DMD CL("LOVED JACKPOT"), CL(FormatScore(5000000)), "_", eNone, eBlinkFast, eNone, 3000, True, "vo_jackpot"
        
		If Not bLovedAchieved(CurrentPlayer) Then
        bLovedAchieved(CurrentPlayer) = True
        li075.State = 1
        CheckAllAchievements
    End If

        AddScore2 50000000
        AddMultiball 2
        DMD "_", CL("LOVED MULTIBALL"), "_", eNone, eBlinkFast, eNone, 2000, True, "" : pupevent 862  
     
        LovedOnesSaved(CurrentPlayer) = False   ' Reset après jackpot complet

        LightEffect 2
        FlashEffect 1
        GiEffect 1
        
        ' On éteint les 5 lumières
        li024.State = 0
        li025.State = 0
        li026.State = 0
        li027.State = 0
        li028.State = 0
    End If
End Sub


'***********************************
' Modes - MISSIONS and TEAMS CHALLENGES
'***********************************
' only one mode can be played at one time
' This table has 8 modes or CHALLENGES, with a nr. 9 being the end Wizard mode

Sub CheckStartModes
    Dim tmp
    tmp = li061.State + li062.State + li063.State + li064.State + li050.State
    
    ' === LOGIQUE CHAMPIONNAT - TOUJOURS ACTIVE (même pendant un autre mode) ===
    If tmp = 5 Then
        AddPlayfieldMultiplier 1
        
        ' Reset des 5 cibles Championnat
        li061.State = 0
        li062.State = 0
        li063.State = 0
        li064.State = 0
        li050.State = 0
        
        ChampionnatCount(CurrentPlayer) = ChampionnatCount(CurrentPlayer) + 1
        
        ' Allumage progressif des lumières li051 à li055
        Select Case ChampionnatCount(CurrentPlayer)
            Case 1: li051.State = 1
            Case 2: li052.State = 1
            Case 3: li053.State = 1
            Case 4: li054.State = 1
            Case 5: li055.State = 1
        End Select
        
        
        If ChampionnatCount(CurrentPlayer) MOD 5 = 0 And Not bChampionnatMBPlayed(CurrentPlayer) Then
			bChampionnatMBPlayed(CurrentPlayer) = True
			StartChampionnatMB
		End If
    End If
    
'
    If Mode(CurrentPlayer, 0) = 0 Then
        If (tmp = 5 OR (OrbitHits + RampHits = 5)) And bModeReady(CurrentPlayer) = False Then
            DMD "_", CL("MISSIONS IS LIT"), "_", eNone, eBlink, eNone, 2500, True, ""
            bModeReady(CurrentPlayer) = True 
            pupevent 838
            li038.State = 2
        End If
    End If
End Sub

Sub StartNextMode
    Dim i
	bModeReady(CurrentPlayer) = False
    li038.State = 0

    ' === Si Damian est terminé OU en cours → comportement "nettoyage" ===
    If Mode(CurrentPlayer, 9) = 1 Or Mode(CurrentPlayer, 9) = 2 Then
        
        ' Cherche le premier mode non terminé
        For i = 1 To 9
            If Mode(CurrentPlayer, i) <> 1 Then
                CurrentMode(CurrentPlayer) = i
                Mode(CurrentPlayer, 0) = i
                Exit For
            End If
        Next
        
    Else
        ' === Comportement normal (avant Damian) : progression séquentielle ===
        CurrentMode(CurrentPlayer) = CurrentMode(CurrentPlayer) + 1
        Mode(CurrentPlayer, 0) = CurrentMode(CurrentPlayer)
    End If
    
    ChangeSong
    
    Select Case Mode(CurrentPlayer, 0)
        
        Case 1 ' Apollo
            DMD CL("APOLLO STARTED"), CL("HIT LIT SHOTS"), "d_apollo", eNone, eBlink, eNone, 2500, True, "" 
            pupevent 820 : DOF_UnderCab "Blue"
            li013.BlinkInterval = 100
            li056.State = 1
			Mode(CurrentPlayer, 1) = 2
            UpdateModeLights
            EndModeCountdown = 120 : pupevent 850 : pupevent 851
            EndModeTimer.Enabled = 1
            ModeStep = 1
            li060.State = 2 : li036.State = 2 
            ChangeGi darkblue
            ChangeGIIntensity 2

        Case 2 ' Cluber 
            DMD CL("CLUBER STARTED"), CL("HIT LIT SHOTS"), "d_cluber", eNone, eBlink, eNone, 2500, True, "" 
            pupevent 821 : DOF_UnderCab "Purple"
            li011.BlinkInterval = 100
            li057.State = 1
			Mode(CurrentPlayer, 2) = 2
            UpdateModeLights
            EndModeCountdown = 120 : pupevent 850 : pupevent 851
            EndModeTimer.Enabled = 1
            ModeStep = 1
            li040.State = 2 : li041.State = 2 
            ChangeGi purple
            ChangeGIIntensity 2

        Case 3 ' HOGAN 
            DMD CL("HOGAN STARTED"), CL("HIT LIT SHOTS"), "d_hogan", eNone, eBlink, eNone, 2500, True, "" 
            pupevent 826 : DOF_UnderCab "White"
            li012.BlinkInterval = 100
            li058.State = 1
			Mode(CurrentPlayer, 3) = 2
            UpdateModeLights
            EndModeCountdown = 120 : pupevent 850 : pupevent 851
            EndModeTimer.Enabled = 1
            ModeStep = 1
            li060.State = 2 : li041.State = 2 
            ChangeGi white
            ChangeGIIntensity 2

        Case 4 ' DRAGO
            DMD CL("DRAGO STARTED"), CL("HIT LIT SHOTS"), "d_drago", eNone, eBlink, eNone, 2500, True, "" 
            pupevent 829 : DOF_UnderCab "Red"
            li014.BlinkInterval = 100
            li059.State = 1
			Mode(CurrentPlayer, 4) = 2
            UpdateModeLights
            EndModeCountdown = 120 : pupevent 850 : pupevent 851
            EndModeTimer.Enabled = 1
            ModeStep = 1
            li036.State = 2 : li040.State = 2 : li037.State = 2
            ChangeGi red
            ChangeGIIntensity 2

        Case 5 ' TOMMY
            DMD CL("TOMMY STARTED"), CL("HIT LIT SHOTS"), "d_tommy", eNone, eBlink, eNone, 2500, True, "" 
            pupevent 804 : DOF_UnderCab "Orange_red"
            li009.BlinkInterval = 100
            li065.State = 1
			Mode(CurrentPlayer, 5) = 2
            UpdateModeLights
            EndModeCountdown = 120 : pupevent 850 : pupevent 851
            EndModeTimer.Enabled = 1
            ModeStep = 1
            li060.State = 2 : li036.State = 2 : li037.State = 2
            ChangeGi orange
            ChangeGIIntensity 2

        Case 6 ' MASON 
            DMD CL("MASON STARTED"), CL("HIT LIT SHOTS"), "d_mason", eNone, eBlink, eNone, 2500, True, "" 
            pupevent 854 : DOF_UnderCab "Turquoise"
            li007.BlinkInterval = 100
            li067.State = 1
			Mode(CurrentPlayer, 6) = 2
            UpdateModeLights
            EndModeCountdown = 120 : pupevent 850 : pupevent 851
            EndModeTimer.Enabled = 1
            ModeStep = 1
            li040.State = 2 : li041.State = 2 : li037.State = 2
            ChangeGi teal
            ChangeGIIntensity 2

        Case 7 ' CONLAN
            DMD CL("CONLAN STARTED"), CL("HIT LIT SHOTS"), "d_conlan", eNone, eBlink, eNone, 2500, True, "" 
            pupevent 847 : DOF_UnderCab "Red"
            li010.BlinkInterval = 100
            li066.State = 1
			Mode(CurrentPlayer, 7) = 2
            UpdateModeLights
            EndModeCountdown = 120 : pupevent 850 : pupevent 851
            EndModeTimer.Enabled = 1
            ModeStep = 1
            li060.State = 2 : li041.State = 2 : li037.State = 2
            ChangeGi red
            ChangeGIIntensity 2

        Case 8 ' VIKTOR
            DMD CL("VIKTOR STARTED"), CL("HIT LIT SHOTS"), "d_viktor", eNone, eBlink, eNone, 2500, True, "" 
            pupevent 864 : DOF_UnderCab "Purple"
            li008.BlinkInterval = 100
			li068.State = 1
            Mode(CurrentPlayer, 8) = 2
            UpdateModeLights
            EndModeCountdown = 120 : pupevent 850 : pupevent 851
            EndModeTimer.Enabled = 1
            ModeStep = 1
            li060.State = 2 : li041.State = 2 : li037.State = 2 : li036.State = 2 : li040.State = 2
            ChangeGi purple
            ChangeGIIntensity 2

        Case 9 ' DAMIAN 
            DMD CL("DAMIAN STARTED"), CL("HIT LIT SHOTS"), "d_damian", eNone, eBlink, eNone, 2500, True, "" 
            pupevent 866 : DOF_UnderCab "Blue"
			li049.BlinkInterval = 100
            li069.State = 1 : li070.State = 1
			Mode(CurrentPlayer, 9) = 2
            UpdateModeLights
            EndModeCountdown = 120 : pupevent 850 : pupevent 851
            EndModeTimer.Enabled = 1
            ModeStep = 1
            li060.State = 2 : li041.State = 2 : li036.State = 2 : li040.State = 2
            ChangeGi darkblue
            ChangeGIIntensity 2

    End Select
    vpmtimer.addtimer 1500, "kickBallOut '"
End Sub

' Update the lights according to the mode's state, 0 not started, 1 finished, 2 started
Sub UpdateModeLights
    ModeLightsSaved(CurrentPlayer) = True
    
    ' Mode 1 = Apollo
    If Mode(CurrentPlayer, 1) = 1 Then 
        li013.State = 1        ' Fixe (terminé)
    ElseIf Mode(CurrentPlayer, 1) = 2 Then 
        li013.State = 2        ' Clignotant (en cours)
    End If

    ' Mode 2 = Clubber
    If Mode(CurrentPlayer, 2) = 1 Then 
        li011.State = 1
    ElseIf Mode(CurrentPlayer, 2) = 2 Then 
        li011.State = 2
    End If

    ' Mode 3 = Hogan
    If Mode(CurrentPlayer, 3) = 1 Then 
        li012.State = 1
    ElseIf Mode(CurrentPlayer, 3) = 2 Then 
        li012.State = 2
    End If

    ' Mode 4 = Drago
    If Mode(CurrentPlayer, 4) = 1 Then 
        li014.State = 1
    ElseIf Mode(CurrentPlayer, 4) = 2 Then 
        li014.State = 2
    End If

    ' Mode 5 = Tommy
    If Mode(CurrentPlayer, 5) = 1 Then 
        li009.State = 1
    ElseIf Mode(CurrentPlayer, 5) = 2 Then 
        li009.State = 2
    End If

    ' Mode 6 = Mason
    If Mode(CurrentPlayer, 6) = 1 Then 
        li007.State = 1
    ElseIf Mode(CurrentPlayer, 6) = 2 Then 
        li007.State = 2
    End If

    ' Mode 7 = Conlan
    If Mode(CurrentPlayer, 7) = 1 Then 
        li010.State = 1
    ElseIf Mode(CurrentPlayer, 7) = 2 Then 
        li010.State = 2
    End If

    ' Mode 8 = Viktor
    If Mode(CurrentPlayer, 8) = 1 Then 
        li008.State = 1        
    ElseIf Mode(CurrentPlayer, 8) = 2 Then 
        li008.State = 2
    End If

	' Mode 9 = Damian
    If Mode(CurrentPlayer, 9) = 1 Then 
        li049.State = 1        
    ElseIf Mode(CurrentPlayer, 9) = 2 Then 
        li049.State = 2
    End If

End Sub
Sub CheckWinMode
    Dim tmp                                                                       'when you complete one the tasks
    Select Case Mode(CurrentPlayer, 0)
        Case 1 ' APOLLO
            If ModeStep = 1 Then
                If li060.State + li036.State = 0 Then 'all 2 lights are out then move to step 2
                    ModeStep = 2
                    RaiseEndTargets  
					pupevent 878
                End If
            End If
        Case 2 ' CLUBER
		    If ModeStep = 1 Then
                If li040.State + li041.State = 0 Then 'all 2 lights are out then move to step 2
                    ModeStep = 2
                    RaiseEndTargets  
					pupevent 878
                End If
            End If
        Case 3 ' HOGAN
            If ModeStep = 1 Then
                If li060.State + li041.State = 0 Then 'all 2 lights are out then move to step 2
                    ModeStep = 2
                    RaiseEndTargets  
					pupevent 878
                End If
            End If

        Case 4 ' DRAGO
            If ModeStep = 1 Then
                If li036.State + li040.State + li037.State = 0 Then 'all 3 lights are out then move to step 2
                    ModeStep = 2
                    RaiseEndTargets  
					pupevent 879
                End If
            End If

        Case 5 ' TOMMY
            If ModeStep = 1 Then
                If li060.State + li036.State + li037.State = 0 Then 'all 3 lights are out then move to step 2
                    ModeStep = 2
                    RaiseEndTargets  
					pupevent 880
                End If
            End If

        Case 6 ' MASON
            If ModeStep = 1 Then
                If li040.State + li041.State + li037.State = 0 Then 'all 3 lights are out then move to step 2
                    ModeStep = 2
                    RaiseEndTargets  
					pupevent 881
                End If
            End If
		
		Case 7 ' CONLAN
            If ModeStep = 1 Then
                If li060.State + li041.State + li037.State = 0 Then 'all 3 lights are out then move to step 2
                    ModeStep = 2
                    RaiseEndTargets  
					pupevent 882
                End If
            End If

        Case 8 ' VIKTOR
            If ModeStep = 1 Then
                If li060.State + li041.State + li037.State + li040.State + li036.State = 0 Then 'all 5 lights are out then move to step 2
                    ModeStep = 2
                    RaiseEndTargets  
					pupevent 882
                End If
            End If

        Case 9 ' DAMIAN
            If ModeStep = 1 Then
                If li060.State + li041.State + li040.State + li036.State = 0 Then 'light are out then move to step 2
                    ModeStep = 2
                    RaiseEndTargets
                    pupevent 882
                End If
            End If

            
    End Select
End Sub

' =============================================
'               MODE PHILADELPHIA 
' =============================================

Dim PhiladelphiaLightsLit   ' Compteur pour savoir combien sont allumées

Sub CheckPhiladelphia()
    Dim tmp
    tmp = li015.State + li016.State + li074.State + li019.State + li020.State + _
          li021.State + li022.State + li023.State + li073.State + li018.State + _
          li072.State + li017.State
    
    If tmp = 12 Then
        If bPhiladelphiaMBPlayed(CurrentPlayer) = False Then
            bPhiladelphiaMBPlayed(CurrentPlayer) = True
            StartPhiladelphiaMB()
        Else
           
            AddScore2 500000
        End If
    Else
        ' === ON N'AFFICHE LE COMPTEUR QUE SI LE MULTIBALL N'EST PAS ENCORE FAIT ===
        If bPhiladelphiaMBPlayed(CurrentPlayer) = False Then
            DMD "_", CL(12 - tmp & " TARGETS LEFT"), "_", eNone, eNone, eNone, 800, True, ""
        End If
        
        PhiladelphiaLightsSaved(CurrentPlayer) = True
    End If
End Sub

Sub ResetPhiladelphiaLights()
    li015.State = 0 : li016.State = 0 : li074.State = 0
    li019.State = 0 : li020.State = 0 : li021.State = 0
    li022.State = 0 : li023.State = 0
    li073.State = 0 : li018.State = 0 : li072.State = 0 : li017.State = 0
End Sub

Sub ProtectPhiladelphiaLights()
    ' On ne réactive QUE les lumières qui étaient déjà allumées avant
    ' (on ne touche pas aux autres)
    If li015.State = 1 Then li015.State = 1
    If li016.State = 1 Then li016.State = 1
    If li074.State = 1 Then li074.State = 1
    If li019.State = 1 Then li019.State = 1
    If li020.State = 1 Then li020.State = 1
    If li021.State = 1 Then li021.State = 1
    If li022.State = 1 Then li022.State = 1
    If li023.State = 1 Then li023.State = 1
    If li073.State = 1 Then li073.State = 1
    If li018.State = 1 Then li018.State = 1
    If li072.State = 1 Then li072.State = 1
    If li017.State = 1 Then li017.State = 1
End Sub

Sub SavePhiladelphiaLights()
    PhiladelphiaLightsSaved(CurrentPlayer) = True
End Sub

Sub StartPhiladelphiaMB()
    bPhiladelphiaMBStarted(CurrentPlayer) = True
    
	    If Not bPhiladelphiaAchieved(CurrentPlayer) Then
        bPhiladelphiaAchieved(CurrentPlayer) = True
        li076.State = 1
        CheckAllAchievements
    End If
	
	DMD CL("PHILADELPHIA"), CL("MULTIBALL"), "d_philadelphia", eNone, eBlink, eNone, 2500, True, "vo_multiball"
    AddMultiball 4          ' + la bille en jeu = 5 billes
    EnableBallSaver 10
   

' === APPARITION DE LA VILLE ===
    ShowCity()
    'vpmtimer.addtimer 30000, "HideCity '"
    
    PhiladelphiaLightsSaved(CurrentPlayer) = False   ' ← On reset la sauvegarde après avoir fait le multiball
End Sub

Sub WinMode 'when you complete all the tasks
    GiEffect 1
    LightEffect 2
    FlashEffect RndNbr(7)
    ChampionnatF
    Select Case Mode(CurrentPlayer, 0)
        Case 1                         ' Apollo
            Mode(CurrentPlayer, 1) = 1 'set the mode as finished, and it will make the light solid lit (UpdateModeLights)
            TotalMISSIONS(CurrentPlayer) = TotalMISSIONS(CurrentPlayer) + 1
            DMDFlush 
            DMD "_", CL("APOLLO COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("APOLLO COMPLETED"), "_", eNone, eBlinkFast, eNone, 1500, True, "" : DOF_UnderCab "Cyan" 
            DMD "_", "_", "_", eNone, eNone, eNone, 1, True, ""
            ModeStep = 0
            AddScore2 1000000 : pupevent 824 : pupevent 852 : pupevent 853
            
		UpdateCombatLights
			vpmtimer.addtimer 2500, "kickBallOut '"
        Case 2 ' Clubber
            Mode(CurrentPlayer, 2) = 1
            TotalMISSIONS(CurrentPlayer) = TotalMISSIONS(CurrentPlayer) + 1
            DMDFlush 
            DMD "_", CL("CLUBER COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("CLUBER COMPLETED"), "_", eNone, eBlinkFast, eNone, 1500, True, "" : DOF_UnderCab "Cyan"
            DMD "_", "_", "_", eNone, eNone, eNone, 1, True, ""
            ModeStep = 0
            AddScore2 1000000 : pupevent 822 : pupevent 852 : pupevent 853
            
		UpdateCombatLights
            vpmtimer.addtimer 2500, "kickBallOut '"
        Case 3 ' HOGAN
     
       Mode(CurrentPlayer, 3) = 1
            TotalMISSIONS(CurrentPlayer) = TotalMISSIONS(CurrentPlayer) + 1
            DMDFlush 
            DMD "_", CL("HOGAN COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("HOGAN COMPLETED"), "_", eNone, eBlinkFast, eNone, 1500, True, "" : DOF_UnderCab "Cyan"
            DMD "_", "_", "_", eNone, eNone, eNone, 1, True, ""
            ModeStep = 0
            AddScore2 1000000 : pupevent 827 : pupevent 852 : pupevent 853
            
		UpdateCombatLights
            vpmtimer.addtimer 2500, "kickBallOut '"
        Case 4 ' DRAGO
            Mode(CurrentPlayer, 4) = 1
            TotalMISSIONS(CurrentPlayer) = TotalMISSIONS(CurrentPlayer) + 1
            DMDFlush 
            DMD "_", CL("DRAGO COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("DRAGO COMPLETED"), "_", eNone, eBlinkFast, eNone, 1500, True, "" : DOF_UnderCab "Cyan"
            DMD "_", "_", "_", eNone, eNone, eNone, 1, True, ""
            ModeStep = 0
            AddScore2 1000000 : pupevent 830 : pupevent 852 : pupevent 853
           
		UpdateCombatLights
            vpmtimer.addtimer 4000, "kickBallOut '"
        Case 5 ' TOMMY
            Mode(CurrentPlayer, 5) = 1
            TotalMISSIONS(CurrentPlayer) = TotalMISSIONS(CurrentPlayer) + 1
            DMDFlush 
            DMD "_", CL("TOMMY COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("TOMMY COMPLETED"), "_", eNone, eBlinkFast, eNone, 2000, True, "" : DOF_UnderCab "Cyan"
            DMD "_", "_", "_", eNone, eNone, eNone, 1, True, ""
            ModeStep = 0
            AddScore2 1000000 : pupevent 805 : pupevent 852 : pupevent 853
           
		UpdateCombatLights
			vpmtimer.addtimer 3000, "kickBallOut '"
        Case 6 ' MASON
            Mode(CurrentPlayer, 6) = 1
            TotalMISSIONS(CurrentPlayer) = TotalMISSIONS(CurrentPlayer) + 1
            DMDFlush 
            DMD "_", CL("MASON COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("MASON COMPLETED"), "_", eNone, eBlinkFast, eNone, 2000, True, "" : DOF_UnderCab "Cyan"
            DMD "_", "_", "_", eNone, eNone, eNone, 1, True, ""
            ModeStep = 0
            AddScore2 1000000 : pupevent 855 : pupevent 852 : pupevent 853
            
		UpdateCombatLights
			vpmtimer.addtimer 3000, "kickBallOut '"
        Case 7 ' CONLAN
            Mode(CurrentPlayer, 7) = 1
            TotalMISSIONS(CurrentPlayer) = TotalMISSIONS(CurrentPlayer) + 1
            DMDFlush 
            DMD "_", CL("CONLAN COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("CONLAN COMPLETED"), "_", eNone, eBlinkFast, eNone, 2000, True, "" : DOF_UnderCab "Cyan"
            DMD "_", "_", "_", eNone, eNone, eNone, 1, True, ""
            ModeStep = 0
            AddScore2 5000000 : pupevent 848 : pupevent 852 : pupevent 853
            
		UpdateCombatLights
			vpmtimer.addtimer 3000, "kickBallOut '"
        Case 8 ' VIKTOR
            Mode(CurrentPlayer, 8) = 1
            TotalMISSIONS(CurrentPlayer) = TotalMISSIONS(CurrentPlayer) + 1
            DMDFlush 
            DMD "_", CL("VIKTOR COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("VIKTOR COMPLETED"), "_", eNone, eBlinkFast, eNone, 2000, True, "" : DOF_UnderCab "Cyan"
            DMD "_", "_", "_", eNone, eNone, eNone, 1, True, ""
            ModeStep = 0
            AddScore2 5000000 : pupevent 865 : pupevent 852 : pupevent 853
            
		UpdateCombatLights
			vpmtimer.addtimer 3000, "kickBallOut '"
        Case 9 ' DAMIAN 
            Mode(CurrentPlayer, 9) = 1
			li049.State = 1
            TotalMISSIONS(CurrentPlayer) = TotalMISSIONS(CurrentPlayer) + 1
            DMDFlush 
            DMD "_", CL("DAMIAN COMPLETED"), "_", eNone, eScrollLeft, eNone, 20, True, "sfx_thunder" & RndNbr(9)
            DMD "_", CL("DAMIAN COMPLETED"), "_", eNone, eBlinkFast, eNone, 2000, True, "" : DOF_UnderCab "Cyan"
            DMD "_", "_", "_", eNone, eNone, eNone, 1, True, ""
            ModeStep = 0
            AddScore2 5000000 : pupevent 867 : pupevent 852 : pupevent 853

    UpdateCombatLights

            ' === Vérification finale (protégée avec timer) ===
            CheckFinalReward
    End Select

    UpdateEndTargetsLight
    StopMode
	
	' === Vérification du grand final (uniquement en phase retry) ===
    If Mode(CurrentPlayer, 9) = 1 Or Mode(CurrentPlayer, 9) = 2 Then
        CheckFinalReward
    End If

End Sub

Sub UpdateCombatLights()
    ' Éteint tout d'abord
    li056.State = 0 : li057.State = 0 : li058.State = 0 : li059.State = 0
    li065.State = 0
    li066.State = 0 : li067.State = 0 : li068.State = 0 : li069.State = 0
    li070.State = 0 : li071.State = 0

    ' Allume selon le nombre de combats gagnés
    If CombatCount(CurrentPlayer) >= 1 Then li056.State = 1
    If CombatCount(CurrentPlayer) >= 2 Then li057.State = 1
    If CombatCount(CurrentPlayer) >= 3 Then li058.State = 1
    If CombatCount(CurrentPlayer) >= 4 Then li059.State = 1
    If CombatCount(CurrentPlayer) >= 5 Then li065.State = 1
    If CombatCount(CurrentPlayer) >= 6 Then li066.State = 1
    If CombatCount(CurrentPlayer) >= 7 Then li067.State = 1
    If CombatCount(CurrentPlayer) >= 8 Then li068.State = 1
    If CombatCount(CurrentPlayer) >= 9 Then li069.State = 1

    ' Lumière 10ème (tous les combats terminés)
    If CombatCount(CurrentPlayer) = 9 Then
        li070.State = 1
        li071.State = 1   ' ou seulement li071 selon ce que tu veux
    End If
End Sub

Sub CheckFinalReward
    Dim AllModesCompleted : AllModesCompleted = True
    Dim i
    For i = 1 To 9
        If Mode(CurrentPlayer, i) <> 1 Then AllModesCompleted = False
    Next

    If AllModesCompleted Then
        
        DMD CL("CONGRATULATIONS"), CL("ALL MODES COMPLETED"), "_", eNone, eBlink, eNone, 2500, True, ""
		pupevent 863

            If Not bAllCombatsAchieved(CurrentPlayer) Then
        bAllCombatsAchieved(CurrentPlayer) = True
        li078.State = 1
        CheckAllAchievements
    
	End If
      DoFinalMultiball
    Else
        ResetUnfinishedModes
    End If
End Sub

Sub CheckAllAchievements()
   
	' === li079 clignote pendant 10 secondes ===
    li079.BlinkInterval = 150          ' Vitesse du clignotement (250 = vitesse)
    li079.State = 2                    ' 2 = clignotant
    vpmtimer.addtimer 10000, "StopLi079Blink '"   ' Après 10 secondes, on arrête le clignotement
         
	
	If li035.State = 1 And li075.State = 1 And li076.State = 1 And li077.State = 1 And li078.State = 1 Then
        
        DMD CL("ALL ACHIEVEMENTS"), CL("COMPLETED"), "_", eNone, eBlinkFast, eNone, 2500, True, "vo_rocky_win"
        DMD CL("BIG JACKPOT"), CL(FormatScore(1000000000)), "_", eNone, eNone, eNone, 2500, True, ""
        
		AddScore2 1000000000
        FlashEffect 7
        GiEffect 1
        LightEffect 2
		
' === li079 clignote pendant 10 secondes ===
    li079.BlinkInterval = 150          ' Vitesse du clignotement (250 = vitesse)
    li079.State = 2                    ' 2 = clignotant
    vpmtimer.addtimer 10000, "StopLi079Blink '"   ' Après 10 secondes, on arrête le clignotement
         
			bAllCombatsAchieved(CurrentPlayer) = True
			li078.State = 1   ' on la laisse allumée quand même
    End If
End Sub

Sub StopLi079Blink()
    li079.State = 1   ' On la laisse allumée en fixe après les 10 secondes
End Sub

'======================================
' Sous-routine pour le multiball final 
'======================================
Sub DoFinalMultiball
    AddScore2 50000000
    pupevent 852 : pupevent 853
    li071.State = 2
    AddMultiball 5
    EnableBallSaver 20
End Sub

Sub StopMode                 'called after a win or at the end of a ball to stop the current mode variables and timers
    EndModeTimer.Enabled = 0 'ensure it is stopped
    TurnOffArrows
    Select Case Mode(CurrentPlayer, 0)
        Case 1                         ' Apollo
            li013.BlinkInterval = 1000 'slow blink in case the mode is not finished
        Case 2                         ' Cluber
            li011.BlinkInterval = 1000
        Case 3                         ' Hogan
            li012.BlinkInterval = 1000
        Case 4                         ' Drago
            li014.BlinkInterval = 1000
        Case 5                         ' Tommy
            li009.BlinkInterval = 1000
        Case 6                         ' Mason
            li007.BlinkInterval = 1000
        Case 7 						   ' Conlan
            li010.BlinkInterval = 1000
        Case 8 						   ' Viktor
            li008.BlinkInterval = 1000
        Case 9 						   ' Damian
			li049.BlinkInterval = 1000													
            ' ResetModes
    End Select
    ' reset variables
    ModeStep = 0
    UpdateModeLights
    OrbitHits = 0 'start counting again for the next mode
    RampHits = 0
    Mode(CurrentPlayer, 0) = 0
    bModeReady(CurrentPlayer) = False
    ChangeGi white
    ChangeGIIntensity 1
    ChangeSong
	HideCity()
End Sub

Sub ResetUnfinishedModes
    Dim i, UnfinishedCount
    UnfinishedCount = 0

    For i = 1 To 9
        If Mode(CurrentPlayer, i) <> 1 Then
            Mode(CurrentPlayer, i) = 2          ' On remet en "à refaire"
            UnfinishedCount = UnfinishedCount + 1
        End If
    Next

    If UnfinishedCount > 0 Then
        DMD CL(UnfinishedCount & " MODES LEFT"), CL("RETRY THEM"), "_", eNone, eBlinkFast, eNone, 3000, True, ""
        
        bModeReady(CurrentPlayer) = True
        li038.State = 2                         ' On rallume la lumière du scoop
    Else
        DMD CL("ALL MODES"), CL("COMPLETED"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
    End If
End Sub

Sub ResetModes 'called after the last wizard mode to start all over again
    Dim i, j
    For i = 0 to 9
        Mode(CurrentPlayer, i) = 0
    Next
    StopRainbow
    UpdateModeLights
    'reset Mode variables
    CurrentMode(CurrentPlayer) = 0
    bModeReady(CurrentPlayer) = False
End Sub

Sub EndModeTimer_Timer '1 second timer to count down to end the timed modes
    EndModeCountdown = EndModeCountdown - 1
    Select Case EndModeCountdown
        Case 16:DMD "_", CL("TIME IS RUNNING OUT"), "_", eNone, eNone, eNone, 1000, True, ""
        Case 10:DMD "_", CL("10"), "_", eNone, eNone, eNone, 500, True, ""
        Case 9:DMD "_", CL("9"), "_", eNone, eNone, eNone, 500, True, ""
        Case 8:DMD "_", CL("8"), "_", eNone, eNone, eNone, 500, True, ""
        Case 7:DMD "_", CL("7"), "_", eNone, eNone, eNone, 500, True, ""
        Case 6:DMD "_", CL("6"), "_", eNone, eNone, eNone, 500, True, ""
        Case 5:DMD "_", CL("5"), "_", eNone, eNone, eNone, 500, True, ""
        Case 4:DMD "_", CL("4"), "_", eNone, eNone, eNone, 500, True, ""
        Case 3:DMD "_", CL("3"), "_", eNone, eNone, eNone, 500, True, ""
        Case 2:DMD "_", CL("2"), "_", eNone, eNone, eNone, 500, True, ""
        Case 1:DMD "_", CL("1"), "_", eNone, eNone, eNone, 500, True, ""
        Case 0
            DMD CL("TIME IS UP"), CL("CHALLENGE TERMINATED"), "_", eNone, eBlinkFast, eNone, 1500, True, "" : pupevent 813 : pupevent 852 : pupevent 853 : DOF_UnderCab "Cyan"
            
	' Baisse automatiquement les deux targets pour le prochain mode
    target001.IsDropped = True
    target002.IsDropped = True
    UpdateEndTargetsLight
			
			If Mode(CurrentPlayer, 0) = 9 Then
                DMD CL("GET READY"), CL("LEGENDS ARE BACK"), "_", eNone, eBlinkFast, eNone, 2000, True, ""
            Else
                DMD CL("TIME IS UP"), CL("YOU LOOSE"), "_", eNone, eBlinkFast, eNone, 1000, True, "" : pupevent 813 : DOF_UnderCab "Cyan"
            End If
            StopMode
    End Select
End Sub

Sub target 

Select Case Mode(CurrentPlayer, 0)
        Case 1, 2, 3, 4, 5, 6, 8, 9
            If ModeStep >= 2 Then
                RaiseEndTargets
            Else
                vpmtimer.addtimer 1500, "kickBallOut '"
            End If
        Case Else
            vpmtimer.addtimer 1500, "kickBallOut '"
    End Select
End Sub


Sub TurnOffArrows 'at the end of the ball or timed mode
    li060.State = 0
    li036.State = 0
    li038.State = 0
    li041.State = 0
    li037.State = 0
    li044.State = 0
    li040.State = 0
    li043.State = 0
    li042.State = 0
    li039.State = 0 
End Sub


'**************
'   COMBOS
'**************

'**************
'   COMBOS - OPTION C (persistant entre les billes)
'**************

Sub AwardCombo
    ComboCount = ComboCount + 1
    ComboLightsSaved(CurrentPlayer) = True
    ComboCountSaved(CurrentPlayer) = True
    
    DOF 130, DOFPulse
    
    Select Case ComboCount
        
        Case 2
            DMD CL("2X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 2)), "", eNone, eNone, eNone, 1500, True, "vo_combo"
            li045.State = 1
            
        Case 3
            DMD CL("3X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 6)), "", eNone, eNone, eNone, 1500, True, "vo_combo"
            li046.State = 1
            
        Case 4
            DMD CL("4X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 8)), "", eNone, eNone, eNone, 1500, True, "vo_combo"
            li047.State = 1
            
        Case 5
            DMD CL("5X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 10)), "", eNone, eNone, eNone, 1500, True, "vo_combo"
            li048.State = 1
            
        Case 6
            DMD CL("SUPER COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * 12)), "", eNone, eNone, eNone, 1500, True, "vo_combo_king"
            ComboValue(CurrentPlayer) = ComboValue(CurrentPlayer) + 100000
            li045.State = 1 : li046.State = 1 : li047.State = 1 : li048.State = 1
            'Option : reset après Super Combo
            ComboCount = 1
            
        Case Else
            DMD CL(ComboCount & "X COMBO"), CL(FormatScore(ComboValue(CurrentPlayer) * ComboCount)), "", eNone, eNone, eNone, 1200, True, "vo_combo"
    End Select
    
    AddScore2 ComboValue(CurrentPlayer) * ComboCount
    ComboValue(CurrentPlayer) = ComboValue(CurrentPlayer) + 50000
    ComboHits(CurrentPlayer) = ComboHits(CurrentPlayer) + 1
	
	    ' === TOUS LES COMBOS ACHEVÉS ===
    If li045.State = 1 And li046.State = 1 And li047.State = 1 And li048.State = 1 And Not bCombosAchieved(CurrentPlayer) Then
        bCombosAchieved(CurrentPlayer) = True
        li035.State = 1
        CheckAllAchievements
    End If

End Sub

'    MULTIBALLS

'****************************************************
' Balboa MB - holes & lock system at the TOMMY hole
'****************************************************
' lock 3 balls, and MB starts
' holes score Jackpots
' value doubles each time all three holes has been Hit
' each hole gives just 1 jackpot until all three has been hit again.

Sub CheckBalboaMB
    If BallsInLock(CurrentPlayer) = 1 Then
        DMD "_", CL("BALL 1 LOCKED"), "_", eNone, eNone, eNone, 1500, True, "" : pupevent 811
		li080.State = 1
		li039.State = 0
	End If
    If BallsInLock(CurrentPlayer) = 2 Then
        DMD "_", CL("BALL 2 LOCKED"), "_", eNone, eNone, eNone, 1500, True, "" : pupevent 809
		li080.State = 0
		li081.State = 1
		li039.State = 0
	End If
    If BallsInLock(CurrentPlayer) = 3 Then
        DMD CL("BALBOA MULTIBALL"), CL("SHOOT THE HOLES"), "_", eNone, eNone, eNone, 1500, True, "" : pupevent 815
        bBalboaMBStarted(CurrentPlayer) = True
        bLockEnabled = False
        li081.State = 0
		li082.State = 1 : light013.State = 2
		li039.State = 0 
        Balboa1 = 0
        Balboa2 = 0
        Balboa3 = 0
        AddMultiball 2
        BallsInLock(CurrentPlayer) = 0
        ChangeSong
    End If
End Sub

Sub CheckBalboaMBHits
    If Balboa1 + Balboa2 + Balboa3 = 3 Then 'all 3 holes has been hit so double the jackpot
        ApolloJackpot(CurrentPlayer) = ApolloJackpot(CurrentPlayer) * 2
        DMD CL("BALBOA JACKPOT IS"), CL(FormatScore(ApolloJackpot(CurrentPlayer))), "_", eNone, eNone, eNone, 1500, True, ""
        Balboa1 = 0
        Balboa2 = 0
        Balboa3 = 0
    End If
End Sub

Sub AwardBalboaJackpot()
    DOF 130, DOFPulse
    DMD CL("BALBOA JACKPOT"), CL(FormatScore(ApolloJackpot(CurrentPlayer))), "_", eNone, eBlinkFast, eNone, 2000, True, "vo_Jackpot"
    DOF 126, DOFPulse
    AddScore2 Jackpot(CurrentPlayer)
    ApolloJackpot(CurrentPlayer) = ApolloJackpot(CurrentPlayer) + 100000
    LightEffect 2
    GiEffect 1
    FlashEffect 1
End Sub

'**********************
' Championnat MB - Upper Ramp
'**********************

Sub StartChampionnatMB
    ' === SAUVEGARDE DES LUMIÈRES AVANT D'ÉTEINDRE ===
    ' Championnat lights
    Dim Saved051, Saved052, Saved053, Saved054, Saved055
    Saved051 = li051.State : Saved052 = li052.State : Saved053 = li053.State
    Saved054 = li054.State : Saved055 = li055.State

    ' Mode lights (li056 à li070)
    Dim Saved056, Saved057, Saved058, Saved059, Saved065, Saved067, Saved066, Saved068, Saved069, Saved070
    Saved056 = li056.State : Saved057 = li057.State : Saved058 = li058.State
    Saved059 = li059.State : Saved065 = li065.State : Saved067 = li067.State
    Saved066 = li066.State : Saved068 = li068.State : Saved069 = li069.State
    Saved070 = li070.State

	' Arrêt propre des séquences avant extinction
    LightSeqInserts.StopPlay
    LightSeqGi.StopPlay

    ' === LANCEMENT DU MULTIBALL ===
    DMD CL("CHAMPIONNAT MULTIBALL"), CL(""), "_", eNone, eNone, eNone, 1500, True, "" : pupevent 837
    bChampionnatMBStarted(CurrentPlayer) = True
    
	    If Not bChampionnatAchieved(CurrentPlayer) Then
        bChampionnatAchieved(CurrentPlayer) = True
        li077.State = 1
        CheckAllAchievements
    End If

	StartCombatAnimation
	AddMultiball 3
    EnableBallSaver 15
    zeusMBFlashTimer.Enabled = 1

    ' === RESTAURATION IMMÉDIATE DES LUMIÈRES ===
    ' Championnat lights
    If Saved051 > 0 Then li051.State = Saved051
    If Saved052 > 0 Then li052.State = Saved052
    If Saved053 > 0 Then li053.State = Saved053
    If Saved054 > 0 Then li054.State = Saved054
    If Saved055 > 0 Then li055.State = Saved055

    ' Mode lights
    If Saved056 > 0 Then li056.State = Saved056
    If Saved057 > 0 Then li057.State = Saved057
    If Saved058 > 0 Then li058.State = Saved058
    If Saved059 > 0 Then li059.State = Saved059
    If Saved065 > 0 Then li065.State = Saved065
    If Saved067 > 0 Then li067.State = Saved067
    If Saved066 > 0 Then li066.State = Saved066
    If Saved068 > 0 Then li068.State = Saved068
    If Saved069 > 0 Then li069.State = Saved069
    If Saved070 > 0 Then li070.State = Saved070
End Sub

Sub ChampionnatMBFlashTimer_Timer
    LTF
    vpmtimer.addtimer 250, "RTF '" 'delay a little the right flasher
End Sub

'*******************
' BONUS MULTIPLIER
'*******************
' fire shots

Sub CheckBonusX
    If li030.State + li031.State + li032.State + li033.State + li034.State = 5 Then 'all the fire lights are on
        AddBonusMultiplier 1
        FlashEffect 5
        LightEffect 5
        
		li030.State = 0
        li031.State = 0
        li032.State = 0
        li033.State = 0
        li034.State = 0
        'blink the X lights fast but only the ones that were off
        If li024.State = 0 Then li024.State = 2
        If li025.State = 0 Then li025.State = 2
        If li026.State = 0 Then li026.State = 2
        If li027.State = 0 Then li027.State = 2
        If li028.State = 0 Then li028.State = 2
    End If
CheckLovedOnesJackpot
End Sub

'************
'   GRADES
'************

Sub CheckGRADES 'checks for hits and start the GRADES mode
    If GRADESHits(CurrentPlayer)MOD 10 = 0 Then
        bGRADESStarted(CurrentPlayer) = True
    End If
End Sub

'*****************
' BONUS HIT SUBS
'*****************

Sub aBonusTargets_Hit(idx):BonusTargets(CurrentPlayer) = BonusTargets(CurrentPlayer) + 1:End Sub
Sub aBonusRamps_Hit(idx):BonusRamps(CurrentPlayer) = BonusRamps(CurrentPlayer) + 1:End Sub
Sub aBonusLOOPS_Hit(idx):BonusLOOPS(CurrentPlayer) = BonusLOOPS(CurrentPlayer) + 1:End Sub

'*********************************
' Table Options F12 User Options
'*********************************
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional array of literal strings

Dim LUTImage, BallsPerGame, UseFlexDMD, OldUseFlex, FlexDMDHighQuality, SongVolume
UseFlexDMD = False 'initialize variable
OldUseFlex = False

Sub Table1_OptionEvent(ByVal eventId)
    Dim x, y

    'LUT
    LutImage = Table1.Option("Select LUT", 0, 21, 1, 0, 0, Array("Normal 0", "Normal 1", "Normal 2", "Normal 3", "Normal 4", "Normal 5", "Normal 6", "Normal 7", "Normal 8", "Normal 9", "Normal 10", _
        "Warm 0", "Warm 1", "Warm 2", "Warm 3", "Warm 4", "Warm 5", "Warm 6", "Warm 7", "Warm 8", "Warm 9", "Warm 10") )
    UpdateLUT

    ' Desktop DMD
    x = Table1.Option("DMD Type", 0, 1, 1, 1, 0, Array("Desktop DMD", "FlexDMD") )
    If UseFlexDMD AND x = 0 Then FlexDMD.Run = False
    If X then UseFlexDMD = True Else UseFlexDMD = False

    ' FlexDMD Quality
    x = Table1.Option("FlexDMD Quality", 0, 1, 1, 1, 0, Array("Low", "High") )
    If x Then FlexDMDHighQuality = True Else FlexDMDHighQuality = False
    If OldUseFlex <> UseFlexDMD Then
        DMD_Init
        If NOT bGameInPlay Then ShowTableInfo
        OldUseFlex = UseFlexDMD 
    End If   

    ' Cabinet rails
    x = Table1.Option("Cabinet Rails", 0, 1, 1, 1, 0, Array("Hide", "Show") )
    For each y in aRails:y.visible = x:next

    ' Side Blades
    x = Table1.Option("Side Blades", 0, 1, 1, 1, 0, Array("Hide", "Show") )
    For each y in aSideBlades:y.SideVisible = x:next

    ' Balls per Game
    x = Table1.Option("Balls per Game", 0, 1, 1, 0, 0, Array("3 Balls", "5 Balls") )
    If x = 1 Then BallsPerGame = 5 Else BallsPerGame = 3

    ' FreePlay
    x = Table1.Option("Free Play", 0, 1, 1, 0, 0, Array("No", "Yes") )
    If x then bFreePlay = True Else bFreePlay = False

    ' Music  On/Off
    x = Table1.Option("Music", 0, 1, 1, 1, 0, Array("OFF", "ON") )
    If x Then bMusicOn = True Else bMusicOn = False

    ' Music Volume
    SongVolume = Table1.Option("Music Volume", 0, 1, 0.1, 0.2, 0)
    If bMusicOn Then
        PlaySound Song, -1, SongVolume, , , , 1, 0
    Else
        StopSound Song
    End If
End Sub

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



'***********************************
'			DOF - TerryRed
'***********************************

Sub DOF_UnderCab(Colour)
	DOF_UnderCab_Off
	if Colour = "Bonus_Cyan" then DOF 501, DOFOn	
	if Colour = "Bonus_Red" then DOF 502, DOFOn		
	if Colour = "Bonus_Orange" then DOF 503, DOFOn	
	if Colour = "Bonus_Yellow" then DOF 504, DOFOn	
	if Colour = "Bonus_Pink" then DOF 505, DOFOn	
	if Colour = "Bonus_White" then DOF 506, DOFOn	
	if Colour = "Bonus_Green" then DOF 507, DOFOn	
	if Colour = "Bonus_Purple" then DOF 508, DOFOn	
	if Colour = "Bonus_Blue" then DOF 509, DOFOn	
	if Colour = "Black" then DOF 510, DOFOn		'In game
	if Colour = "Cyan" then DOF 511, DOFOn		
	if Colour = "Red" then DOF 512, DOFOn		
	if Colour = "Orange" then DOF 513, DOFOn	
	if Colour = "Yellow" then DOF 514, DOFOn	
	if Colour = "Pink" then DOF 515, DOFOn		
	if Colour = "White" then DOF 516, DOFOn		
	if Colour = "Green" then DOF 517, DOFOn		
	if Colour = "Purple" then DOF 518, DOFOn	
	if Colour = "Blue" then DOF 519, DOFOn		
End Sub

Sub DOF_UnderCab_Off
	'End of Ball Bonus Colours
	DOF 501, DOFOff	
	DOF 502, DOFOff	
	DOF 503, DOFOff	
	DOF 504, DOFOff	
	DOF 505, DOFOff	
	DOF 506, DOFOff	
	DOF 507, DOFOff	
	DOF 508, DOFOff	
	DOF 509, DOFOff	
	'In Game Colours
	DOF 510, DOFOff	
	DOF 511, DOFOff	
	DOF 512, DOFOff	
	DOF 513, DOFOff	
	DOF 514, DOFOff	
	DOF 515, DOFOff	
	DOF 516, DOFOff	
	DOF 517, DOFOff	
	DOF 518, DOFOff	
	DOF 519, DOFOff	
End Sub

Sub RightFlipperTop001_Hit()
	
End Sub