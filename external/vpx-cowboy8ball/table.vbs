' Cowboy Eight Ball / IPD No. 5132 / 1981 / 3 Players
' by LTD do Brasil Diversões Eletrônicas Ltda, of Campinas,
' VPX8 - version by JPSalas 2024, version 6.0.0
' vpm script by mfuegemann

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01550000", "ltd3.vbs", 3.26

Dim bsTrough, obj, RightDropTargetBank, LeftDropTargetBank, SingleDT, Flipperactive, bsTopHole, x

Const cGameName = "cowboy3p"

Const UseSolenoids = 1
Const UseLamps = 1
Const UseGI = 0
Const UseSync = 0   'set it to 1 if the table runs too fast
Const HandleMech = 0
Const vpmhidden = 1 'hide the vpinmame window
Const FreePlay = False

Plunger.MechStrength = 150

If Table1.ShowDT = true then
    For each x in aReels
        x.Visible = 1
    Next
else
    For each x in aReels
        x.Visible = 0
    Next
end if

' Standard Sounds
Const SSolenoidOn = "fx_Solenoidon"
Const SSolenoidOff = "fx_Solenoidoff"
Const SCoin = "fx_Coin"

'******************
' Realtime Updates
'******************

Set Motorcallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates
    RollingUpdate
End Sub

Set LampCallBack = GetRef("UpdateLamps")

Sub UpdateLamps
    UpdateLeds
    'Hole Solenoid is triggered by Lamp 14
    if controller.Lamp(14)Then
        If bsTopHole.Balls Then
            bsTopHole.ExitSol_on
        End If
    end If

    'Player 1+3 play 1-7, Player 2 plays 9-15
    for each obj in BallInserts
        obj.state = LightstateOff
    Next
    if controller.Lamp(8)Then
        light018.state = controller.Lamp(41)
        light046.state = controller.Lamp(41)
        light019.state = controller.Lamp(42)
        light044.state = controller.Lamp(42)
        light020.state = controller.Lamp(43)
        light042.state = controller.Lamp(43)
        light016.state = controller.Lamp(44)
        light040.state = controller.Lamp(44)
        light014.state = controller.Lamp(45)
        light038.state = controller.Lamp(45)
        light015.state = controller.Lamp(46)
        light036.state = controller.Lamp(46)
        light002.state = controller.Lamp(47)
        light034.state = controller.Lamp(47)
    Else
        light023.state = controller.Lamp(41)
        light047.state = controller.Lamp(41)
        light024.state = controller.Lamp(42)
        light045.state = controller.Lamp(42)
        light025.state = controller.Lamp(43)
        light043.state = controller.Lamp(43)
        light026.state = controller.Lamp(44)
        light041.state = controller.Lamp(44)
        light021.state = controller.Lamp(45)
        light039.state = controller.Lamp(45)
        light027.state = controller.Lamp(46)
        light037.state = controller.Lamp(46)
        light017.state = controller.Lamp(47)
        light035.state = controller.Lamp(47)
    end If
End Sub

'************
' Table init.
'************

Sub table1_Init
    vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Cowboy Eight Ball - LTD 1982" & vbNewLine & "VPX table by JPSalas v6.0.0"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = vpmhidden
        .Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
        .Games(cGameName).Settings.Value("sound") = 1 '1 enabled rom sound
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 1
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

    'Trough
    Set bsTrough = New cvpmBallStack
    bsTrough.InitSw 0, 57, 0, 0, 0, 0, 0, 0 '0.1 = Switch 0
    bsTrough.InitKick BallRelease, 90, 5
    bsTrough.InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsTrough.Balls = 1

    'Top Hole
    set bsTopHole = new cvpmBallStack
    bsTopHole.InitSaucer TopHole, 25, 300, 15
    bsTopHole.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoidon", DOFContactors)

    'droptargets
    set RightDropTargetBank = new cvpmDropTarget
    RightDropTargetBank.InitDrop Array(DT1, DT2, DT3, DT4, DT5, DT6, DT7), Array(33, 34, 35, 36, 37, 38, 41)
    RightDropTargetBank.InitSnd SoundFX("fx_droptarget", DOFContactors), SoundFX("fx_resetdrop", DOFContactors)
    RightDropTargetBank.CreateEvents "RightDropTargetBank"

    set LeftDropTargetBank = new cvpmDropTarget
    LeftDropTargetBank.InitDrop Array(DT8, DT9, DT10, DT11), Array(43, 44, 45, 46)
    LeftDropTargetBank.InitSnd SoundFX("fx_droptarget", DOFContactors), SoundFX("fx_resetdrop", DOFContactors)
    LeftDropTargetBank.CreateEvents "LeftDropTargetBank"

    set SingleDT = new cvpmDropTarget
    SingleDT.InitDrop Array(DT12), Array(42)
    SingleDT.InitSnd SoundFX("fx_droptarget", DOFContactors), SoundFX("fx_resetdrop", DOFContactors)
    SingleDT.CreateEvents "SingleDT"

    vpmMapLights AllLights

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = True

    LampTimer.Enabled = True

    ' Turn on Gi
    vpmtimer.addtimer 2000, "GiOn '"
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.stop:End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = LeftFlipperKey AND flipperactive Then SolLFlipper 1
    If keycode = RightFlipperKey AND flipperactive Then SolRFlipper 1
    If FreePlay and keycode = Startgamekey Then vpmtimer.pulsesw -7
    If vpmKeyDown(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_PlungerPull", Plunger:Plunger.Pullback
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = LeftFlipperKey AND flipperactive Then SolLFlipper 0
    If keycode = RightFlipperKey AND flipperactive Then SolRFlipper 0
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PLaySoundAt "fx_plunger", Plunger:Plunger.Fire
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 6
    LeftSlingShot.TimerEnabled = 1
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
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 5
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' Scoring rubbers
Sub s9a_Hit:vpmTimer.PulseSw 9:End Sub
Sub s9b_Hit:vpmTimer.PulseSw 9:End Sub
Sub s9c_Hit:vpmTimer.PulseSw 9:End Sub
Sub s9d_Hit:vpmTimer.PulseSw 9:End Sub
Sub s9e_Hit:vpmTimer.PulseSw 9:End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 2:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper1:lb001.Duration 0, 100, 1:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 3:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper2:lb002.Duration 0, 100, 1:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 4:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper3:lb003.Duration 0, 100, 1:End Sub

' Drain & Saucers
Sub TopHole_Hit:PlaysoundAt "fx_kicker_enter", TopHole:bsTopHole.AddBall Me:End Sub
Sub Drain_Hit:PlaysoundAt "fx_kicker_enter", Drain:bsTrough.AddBall Me:End Sub

' Rollovers
Sub LeftLane_Hit:Controller.Switch(27) = 1:PlaySoundAt "fx_sensor", LeftLane:rol1.Duration 1, 600, 0:End Sub
Sub LeftLane_UnHit:Controller.Switch(27) = 0:End Sub

Sub LeftOutlane_Hit:Controller.Switch(10) = 1:PlaySoundAt "fx_sensor", LeftOutlane:End Sub
Sub LeftOutlane_Unhit:Controller.Switch(10) = 0:End Sub

Sub RightOutlane_Hit:Controller.Switch(10) = 1:PlaySoundAt "fx_sensor", RightOutlane:End Sub
Sub RightOutlane_Unhit:Controller.Switch(10) = 0:End Sub

Sub TopRollover1_Hit:Controller.Switch(13) = 1:PlaySoundAt "fx_sensor", TopRollover1:End Sub
Sub TopRollover1_Unhit:Controller.Switch(13) = 0:End Sub

Sub TopRollover2_Hit:Controller.Switch(14) = 1:PlaySoundAt "fx_sensor", TopRollover2:End Sub
Sub TopRollover2_Unhit:Controller.Switch(14) = 0:End Sub

Sub Leftinlane_Hit:Controller.Switch(11) = 1:PlaySoundAt "fx_sensor", Leftinlane:End Sub
Sub Leftinlane_Unhit:Controller.Switch(11) = 0:End Sub

Sub Rightinlane_Hit:Controller.Switch(12) = 1:PlaySoundAt "fx_sensor", Rightinlane:End Sub
Sub Rightinlane_Unhit:Controller.Switch(12) = 0:End Sub

' targets
Sub TopTarget_Hit:vpmtimer.pulsesw 26:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub '26 or 42
Sub RightTarget1_Hit:vpmtimer.pulsesw 17:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub RightTarget2_Hit:vpmtimer.pulsesw 18:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub RightTarget3_Hit:vpmtimer.pulsesw 19:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub RightTarget4_Hit:vpmtimer.pulsesw 20:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub RightTarget5_Hit:vpmtimer.pulsesw 21:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub RightTarget6_Hit:vpmtimer.pulsesw 22:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub

'*********
'Solenoids
'*********

SolCallback(1) = "bsTrough.SolOut"
SolCallback(7) = "LeftDropTargetBank.SolDropUp"
SolCallback(16) = "RightDropTargetBank.SolDropUp"
SolCallback(8) = "SingleDT.SolDropUp"
SolCallback(17) = "Sol_GameOn"
SolCallback(9) = "RightDropTargetBank.SolHit 1,"
SolCallback(10) = "RightDropTargetBank.SolHit 2,"
SolCallback(11) = "RightDropTargetBank.SolHit 3,"
SolCallback(12) = "RightDropTargetBank.SolHit 4,"
SolCallback(13) = "RightDropTargetBank.SolHit 5,"
SolCallback(14) = "RightDropTargetBank.SolHit 6,"
SolCallback(15) = "RightDropTargetBank.SolHit 7,"

Flipperactive = False

Sub Sol_GameOn(enabled)
    Dim obj
    VpmNudge.SolGameOn enabled
    Flipperactive = enabled
    if not Flipperactive Then
        LeftFlipper.RotateToStart
        LeftFlipper001.RotateToStart
        RightFlipper.RotateToStart
    end If
    for each obj in aGiLights
        obj.state = abs(Enabled)
    Next
end sub

'*******************
' Flipper Subs Rev3
'*******************

' SolCallback(sLRFlipper) = "SolRFlipper"
' SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipper001.RotateToEnd
        LeftFlipperOn = 1
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipper001.RotateToStart
        LeftFlipperOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipperOn = 1
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

' flippers top animations

Sub LeftFlipper_Animate:LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle: End Sub
Sub RightFlipper_Animate: RightFlipperTop.RotZ = RightFlipper.CurrentAngle: End Sub
Sub LeftFlipper001_Animate:LeftFlipperTop001.RotZ = LeftFlipper001.CurrentAngle: End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper001_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
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

FlipperPower = 3600
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

'*****************
'   Gi Effects
'*****************

Dim OldGiState
OldGiState = -1 'start witht he Gi off

Sub GiON
    For each x in aGiLights
        x.State = 1
    Next
End Sub

Sub GiOFF
    For each x in aGiLights
        x.State = 0
    Next
End Sub

Sub GiEffect(enabled)
    If enabled Then
        For each x in aGiLights
            x.Duration 2, 1000, 1
        Next
    End If
End Sub

Sub GIUpdate
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = -1 Then
            GiOff
        Else
            GiOn
        End If
    End If
End Sub

'************************************
'          LEDs Display
'     Based on Scapino's LEDs
'************************************

Dim Digits(32)
Dim Patterns(11)

Patterns(0) = 0    'empty
Patterns(1) = 63   '0
Patterns(2) = 6    '1
Patterns(3) = 91   '2
Patterns(4) = 79   '3
Patterns(5) = 102  '4
Patterns(6) = 109  '5
Patterns(7) = 124  '6
Patterns(8) = 7    '7
Patterns(9) = 127  '8
Patterns(10) = 103 '9

'Assign 6-digit output to reels
Set Digits(0) = a0
Set Digits(1) = a1
Set Digits(2) = a2
Set Digits(3) = a3
Set Digits(4) = a4
Set Digits(5) = a5

Set Digits(6) = b0
Set Digits(7) = b1
Set Digits(8) = b2
Set Digits(9) = b3
Set Digits(10) = b4
Set Digits(11) = b5

Set Digits(12) = c0
Set Digits(13) = c1
Set Digits(14) = c2
Set Digits(15) = c3
Set Digits(16) = c4
Set Digits(17) = c5

Set Digits(18) = e0
Set Digits(19) = e1

Sub UpdateLeds
    On Error Resume Next
    Dim ChgLED, ii, jj, chg, stat
    ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(ChgLED)
            chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For jj = 0 to 10
                If stat = Patterns(jj)then Digits(chgLED(ii, 0)).SetValue jj
            Next
        Next
    End IF
End Sub

'*********************************
' Diverse Collection Hit Sounds
'*********************************

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
Const maxvel = 36 'max ball velocity
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
                ballvol = Vol(BOT(b)) * 5
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

        ' jps ball speed & spin control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95
            BOT(b).AngMomX = BOT(b).AngMomX * 0.95
            BOT(b).AngMomY = BOT(b).AngMomY * 0.95
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

'*****************************
' Ball 2 Ball Collision Sound
'*****************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'*********************************
' Table Options F12 User Options
'*********************************
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional array of literal strings

Dim LUTImage

Sub Table1_OptionEvent(ByVal eventId)
    Dim x, y

    'LUT
    LutImage = Table1.Option("Select LUT", 0, 21, 1, 0, 0, Array("Normal 0", "Normal 1", "Normal 2", "Normal 3", "Normal 4", "Normal 5", "Normal 6", "Normal 7", "Normal 8", "Normal 9", "Normal 10", _
        "Warm 0", "Warm 1", "Warm 2", "Warm 3", "Warm 4", "Warm 5", "Warm 6", "Warm 7", "Warm 8", "Warm 9", "Warm 10") )
    UpdateLUT

    ' Cabinet rails
    x = Table1.Option("Cabinet Rails", 0, 1, 1, 1, 0, Array("Hide", "Show") )
    For each y in aRails:y.visible = x:next
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