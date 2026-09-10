'Barracora - Williams 1981 by Dids666
'
'Thanks to JP and gtxJoe for some of the code used and the lamp numbers(VP9 version and Titan VPX)  (I would have no idea otherwise)
'
'Thanks to DJRobX for adding surround support and solving some errors
'
'Thanks to the testers:...
'
'Ver 1.1
'
'
'
'
Option Explicit
Randomize
 
' ===============================================================================================
' Load game controller
' ===============================================================================================
 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0
 
LoadVPM "00990300", "S7.VBS", 3.36
 
' ===============================================================================================
' General constants and variables
' ===============================================================================================
Const UseSolenoids = 1
Const UseLamps = 1
Const UseGI = 1
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 0
 
 
' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = ""
Const SFlipperOn    = "fx_Flipperup"
Const SFlipperOff   = "fx_Flipperdown"
Const SCoin = "fx_Coin"
 
Dim bsTrough, dtbank1, dtbank2, bsLeftSaucer, bsRightSaucer, x
Const cGameName = "barra_l1"
Dim bsLowerEject,bsUpperEject
Dim gameRun
 
 
' Lights
 
  vpmMapLights AllLights
 
 
 
' ===============================================================================================
' Init routines
' ===============================================================================================
 
Sub table1_Init
    vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Tango & Cash (TBA 2019)" & vbNewLine & "VPX table by IVANTBA"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 0
        .ShowFrame = 0
        .HandleMechanics = 0
        .hidden = 1
        .Games(cGameName).Settings.Value("sound") = 0 '1= rotated display, 0= normal
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 1
        'vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With
 
 PlaySound "Music",-1
 PlaySound "Intro",-0  
   
 
    ' Nudging
    vpmNudge.TiltSwitch = 1
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(sw20, sw21, sw22, LeftSlingshot, RightSlingshot)
 
    ' Trough
    set bsTrough= new cvpmballstack
    bsTrough.initsw 0,15,14,13,0,0,0,0
    bsTrough.initkick ballrelease,90,1  
    bsTrough.balls=3  
 
    ' Saucers
    Set bsUpperEject = New cvpmBallStack
    bsUpperEject.InitSaucer sw10, 10,65,8
bsUpperEject.kickanglevar=5
    bsUpperEject.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsUpperEject.KickForceVar = 6
 
    Set bsLowerEject = New cvpmBallStack
    bsLowerEject.InitSaucer sw11, 11,169,5
bsLowerEject.kickanglevar=5
    bsLowerEject.InitExitSnd SoundFX("fx_kicker2", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsLowerEject.KickForceVar = 6
 
 
 
    ' Drop targets
    set dtbank1 = new cvpmdroptarget
    dtbank1.InitDrop Array(sw41, sw42, sw43), Array(41, 42, 43)
    dtbank1.initsnd SoundFX("", DOFDropTargets), SoundFX("fx2_droptargetreset", DOFContactors)
 
    set dtbank2 = new cvpmdroptarget
    dtbank2.InitDrop Array(sw44, sw45, sw46, sw47, sw48), Array(44, 45, 46, 47, 48)
    dtbank2.initsnd SoundFX("", DOFDropTargets), SoundFX("fx2_droptargetreset", DOFContactors)
 
 
 
vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'"
 
 
End Sub
 
Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.stop:End Sub
 
 
' ===============================================================================================
' KeyDown routines
' ===============================================================================================
 
Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then PlaySoundAt "fx_PlungerPull", Plunger:Plunger.Pullback
    if keycode = rightflipperkey then controller.switch(35) = 1 'Mudas as lampadas de cima
    if keycode = leftflipperkey then controller.switch(34) = 1
    If vpmKeyDown(keycode)Then Exit Sub
End Sub
 
 
' ===============================================================================================
' KeyUp routines
' ===============================================================================================
 
Sub table1_KeyUp(ByVal Keycode)
    if keycode = rightflipperkey then controller.switch(35) = 0
    if keycode = leftflipperkey then controller.switch(34) = 0
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_plunger", Plunger:Plunger.Fire
End Sub
 
 

 
' ===============================================================================================
' Solenoids constants and routines
' ===============================================================================================
 
 
 
Const sBallRampThrow=12
 
Const sBell=15
'Const sCoin=16
Const sUpperGate=17
Const sJetLeft=18
Const sJetRight=19
Const sJetCenter=20
Const sSlingLeft=21
Const sSlingRight=22
Const sEnable=25
Const sDTLeftB_S1=1
Const sDTLeftA_S2=2
Const sDTLeftRR_S3=3
Const sDTRightA_S4=4
Const sDTRightC_S5=5
Const sDTRightO_S6=6
Const sDTRightR_S7=7
Const sDTRightA2_S8=8
Const sDTLeftRel_S9=9
Const sDTRightRel_S10=10
Const sBallRel=11
Const sEjectUpper=13
Const sEjectLower=14
 
 
 
 
'SolCallbacks
 
SolCallback(sDTLeftB_S1)     = "dtbank1.SolUnhit 1,"
SolCallback(sDTLeftA_S2)     = "dtbank1.SolUnhit 2,"
SolCallback(sDTLeftRR_S3)    = "dtbank1.SolUnhit 3,"
SolCallback(sDTRightA_S4)    = "dtbank2.SolUnhit 1,"
SolCallback(sDTRightC_S5)    = "dtbank2.SolUnhit 2,"
SolCallback(sDTRightO_S6)    = "dtbank2.SolUnhit 3,"
SolCallback(sDTRightR_S7)    = "dtbank2.SolUnhit 4,"
SolCallback(sDTRightA2_S8)   = "dtbank2.SolUnhit 5,"
SolCallback(sDTLeftRel_S9)   = "dtbank1.SolDropDown"
SolCallback(sDTRightRel_S10)  = "dtbank2.SolDropDown"
SolCallback(sBallRel)="sBallRelease"
'SolCallback(1) = "bsTrough.SolOut"
SolCallback(sBallRampThrow)="sBallRampThrower"
SolCallback(sEjectUpper)="bsUpperEject.solout"
SolCallback(sEjectLower)="bsLowerEject.solout"
SolCallback(sBell)="vpmsolsound""fx2_bell"","
''SolCallback(sCoin)= - Not Used
SolCallback(sUpperGate)="UpperKickerGate"
SolCallback(sJetLeft) = "vpmsolsound""fx2_Bumper_2"","
SolCallback(sJetRight) = "vpmsolsound ""fx2_Bumper_2"","
SolCallback(sJetCenter) = "vpmsolsound ""fx2_Bumper_2"","
SolCallback(sSlingLeft)   = "vpmsolsound ""fx_slingshot1"","
SolCallback(sSlingRight)  = "vpmsolsound ""fx_slingshot2"","
 
SolCallback(sllflipper)="SolLFlipper"
SolCallback(slrflipper)="SolRFlipper"
 
'----- Solenoid Subroutines
Sub sBallRelease(enabled) '11
    controller.switch (12)=False
End Sub
 
Dim SyncLampsFlag
Sub sBallRampThrower(enabled)
    if enabled then
    controller.switch (12)=False
    bsTrough.exitsol_on
    playsoundat "fx_BallRel", Plunger
    'SyncLamps 'ensure lights are synchronized at start of new ball
    SyncLampsFlag = 4
    end if
 
End Sub
 
Sub UpperKickerGate(enabled): gate1.open=True: End Sub
 
' ===============================================================================================
' Trigger/Switch routines
' ===============================================================================================
 
'----- Kickers -----
Sub sw10_Hit()  'Switch10
    bsUpperEject.addball 0
    PlaySoundAt "kicker_enter_left", sw10
End Sub  
 
Sub sw11_Hit()  'Switch11
    bsLowerEject.addball 0
    PlaySoundAt "kicker_enter_left2", sw11
End Sub
 
Sub Drain_Hit()         'Switch12
    Drain.DestroyBall
    PlaySound "fx_drain"
    bsTrough.addball me
    controller.switch (12)=True
End Sub
 
'----- Flippers -----
Sub LaneChangeRight:vpmtimer.pulsesw 35:End Sub
Sub LangeChangeLeft:vpmtimer.pulsesw 34:End Sub
 
'********************
'    JP Flippers
'********************
 
Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFContactors), 0, 1, -0.1, 0.25, 0, 0, 1, 1
        LeftFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown",DOFContactors), 0, 1, -0.1, 0.25, 0, 0, 1, 1
        LeftFlipper.RotateToStart
    End If
End Sub
 
Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFContactors), 0, 1, 0.1, 0.25, 0, 0, 1, 1
        RightFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown",DOFContactors), 0, 1, 0.1, 0.25, 0, 0, 1, 1
        RightFlipper.RotateToStart
    End If
End Sub
 
Sub LeftFlipper_Collide(parm)
    PlaySoundAtBall "fx_rubber_flipper"
End Sub
 
Sub RightFlipper_Collide(parm)
    PlaySoundAtBall "fx_rubber_flipper"
End Sub
 
 
'----Rollovers---
 
Sub sw23_Hit:Controller.Switch(23) = 1:PlaySoundAtBall "fx_sensor":End Sub
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
 
Sub sw31_Hit:Controller.Switch(31) = 1:PlaySoundAtBall "fx_sensor11":End Sub
Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub
 
Sub sw32_Hit:Controller.Switch(32) = 1:PlaySoundAtBall "fx_sensor3":End Sub
Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub
 
Sub sw24_Hit:Controller.Switch(24) = 1:PlaySoundAtBall "fx_sensor4":End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub
 
Sub sw26_Hit:Controller.Switch(26) = 1:PlaySoundAtBall "fx_sensor5":End Sub
Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub
 
Sub sw19_Hit:Controller.Switch(19) = 1:PlaySoundAtBall "fx_sensor6":End Sub
Sub sw19_UnHit:Controller.Switch(19) = 0:End Sub
 
Sub sw18_Hit:Controller.Switch(18) = 1:PlaySoundAtBall "fx_sensor7":End Sub
Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub
 
Sub sw16_Hit:Controller.Switch(16) = 1:PlaySoundAtBall "fx_sensor8":End Sub
Sub sw16_UnHit:Controller.Switch(16) = 0:End Sub
 
Sub sw17_Hit:Controller.Switch(17) = 1:PlaySoundAtBall "fx_sensor9":End Sub
Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub
 
Sub sw9_Hit:Controller.Switch(9) = 1:PlaySoundAtBall "fx_sensor10":End Sub
Sub sw9_UnHit:Controller.Switch(9) = 0:End Sub
 
Sub sw18_Hit:Controller.Switch(18) = 1:PlaySoundAtBall "fx_sensor2":End Sub
Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub
 
Sub sw18_Hit:Controller.Switch(18) = 1:PlaySoundAtBall "fx_sensor12":End Sub
Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub
 
Sub sw12_Hit:Controller.Switch(12) = 1:PlaySoundAtBall "fx_sensor13":End Sub
Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub
 
Sub Launchball_Hit
    If Activeball.VelY < 0 Then
        PlaySoundAt "fx_launchball", Launchball
    End If
End Sub
 
'----- Spinner -----
Sub sw25_Spin():vpmtimer.pulsesw 25:PlaySoundAt "fx_spinner", sw25:End Sub
 
 
'---- Gate ----
Sub sw16_Hit()
    gate1.open=False
    Controller.Switch(16)=True
End Sub
 
'----- Bumpers/Jets -----
Dim bumpl, bumpr, bumpc
Sub sw20_Hit
    PlaySound "Bumper_1"
    bumpl = 1
    Me.TimerEnabled = 1
    vpmTimer.PulseSwitch 20, 0, 0
    Execute SolCallback(sJetLeft) & 1
End Sub
 
 
 
Sub sw21_Hit
    PlaySound "Bumper_2"
    bumpr = 1
    Me.TimerEnabled = 1
    vpmTimer.PulseSwitch 21, 0, 0
    Execute SolCallback(sJetRight) & 1
End Sub
 
 
Sub sw22_Hit
    PlaySound "Bumper_3"
    bumpc = 1
    Me.TimerEnabled = 1
    vpmTimer.PulseSwitch 22, 0, 0
    Execute SolCallback(sJetCenter) & 1
End Sub
 
 
 
'----Targets-----
 
 
Sub sw33_Hit:vpmTimer.PulseSw 33:PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw27_Hit:vpmTimer.PulseSw 27:PlaySoundAtBall SoundFX("fx_target2", DOFTargets):End Sub
Sub sw28_Hit:vpmTimer.PulseSw 28:PlaySoundAtBall SoundFX("fx_target3", DOFTargets):End Sub
 
'---- Drop Targets ----
 
Sub sw41_Dropped:dtbank1.Hit 1:End Sub
Sub sw42_Dropped:dtbank1.Hit 2:End Sub
Sub sw43_Dropped:dtbank1.Hit 3:End Sub
Sub sw44_Dropped:dtbank2.Hit 1:End Sub
Sub sw45_Dropped:dtbank2.Hit 2:End Sub
Sub sw46_Dropped:dtbank2.Hit 3:End Sub
Sub sw47_Dropped:dtbank2.Hit 4:End Sub
Sub sw48_Dropped:dtbank2.Hit 5:End Sub
 
 
'----- Standup Targets -----
Sub sw37_Hit():vpmtimer.pulsesw 37:End Sub
Sub sw38_Hit():vpmtimer.pulsesw 38:End Sub
Sub sw39_Hit():vpmtimer.pulsesw 39:End Sub
Sub sw40_Hit():vpmtimer.pulsesw 40:End Sub
 
 
'---- Slings ----
 
 
Dim LStep, RStep
 
Sub LeftSlingShot_Slingshot
    vpmTimer.PulseSwitch 29, 0, 0
 
    PlaySound SoundFX("fx_slingshot", DOFContactors), 0, 1, -0.05, 0.05, 0, 0, 1, .8
    DOF 101, DOFPulse
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 29
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
    vpmTimer.PulseSwitch 30, 0, 0
    PlaySound SoundFX("fx_slingshot2", DOFContactors), 0, 1, 0.05, 0.05, 0, 0, 1, .8
    DOF 102, DOFPulse
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 30
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
 
 
 
' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************
 
Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function
 
Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function
 
Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function
 
Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function
 
Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function
 
' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub
 
Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub
 
'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************
 
Const tnob = 20 ' total number of balls
Const lob = 0   'number of locked balls
ReDim rolling(tnob)
InitRolling
 
Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub
 
Sub RollingTimer_Timer()
    Dim BOT, b, ballpitch
    BOT = GetBalls
 
    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next
 
    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table
 
    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)
        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 15000 'increase the pitch on a ramp or elevated surface
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub
 
'**********************
' Ball Collision Sound
'**********************
 
Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub
 
 
'===============================================================================================
' Sound routines for VP collections
' ===============================================================================================
 
 
'******************************
' Diverse Collection Hit Sounds
'******************************
 
Sub aMetals_Hit(idx):PlaySound "fx3_EM_metalhit_medium", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx2_plastichit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_rubber_pin", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aGates_Hit(idx):PlaySound "fx3_EM_gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub

Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub