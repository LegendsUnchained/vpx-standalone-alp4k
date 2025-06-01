Option Explicit
Randomize

Const BallSize =48

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="csi_240",UseSolenoids=1,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

LoadVPM "01560000", "Sam.VBS", 3.38
Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
Primitive13.visible=1
Else
Ramp16.visible=1
Ramp15.visible=1
Primitive13.visible=0
End if

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************

SolCallBack(1)    = "SolTrough"
SolCallBack(2)    = "SolAutoPlungerIM"
SolCallback(3)    = "Mag.MagnetOn=  " 
SolCallback(4)	  = "bsLEject.SolOut" 'Left Eject
SolCallback(5)    = "SolLeftPostDown"	'Left up down Post
SolCallback(6)	  = "SolCentrifuge" 'Centrifuge Motor
SolCallback(7)    = "dtLDrop.SolDropUp" 'Drop Targets
SolCallback(12)   = "SolCLock" 'CenterLock
SolCallback(13)	  = "bsRVuk.SolOut" 'Rig77ht Eject
SolCallback(15)   = "SolLFlipper"
SolCallback(16)   = "SolRFlipper"
'SolCallback(27)  = "SolIman" 'Centrifuge Motor Relay
SolCallback(29)   = "SolSkullMotor" 'Skull Motor
'SolCallback(30)  = "SolRightPostDown"	'Right up down Post
SolCallback(32)   = "SolSkullMotorRelay" 'Skull Motor Relay

'Flash Lamps
SolCallBack(19)  = "setlamp 119," 'Bumpers x2
SolCallBack(20)  = "setlamp 120," 'Centrifuge
SolCallBack(21)  = "Setlamp 121," 'Skull x3
SolCallBack(22)  = "setlamp 122," 'Bumpers x3
SolCallBack(23)  = "setlamp 123," 'Background x2
SolCallBack(26)  = "setlamp 126," 'Left Spinner
SolCallBack(28)  = "setlamp 128," 'Microscopio
SolCallBack(31)  = "setlamp 131," 'Right Spinner


Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):RightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart
     End If
End Sub

'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

'Trough Ball
Sub SolTrough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		vpmTimer.PulseSw 22
	End If
 End Sub

Sub SolAutoPlungerIM(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
 End Sub


'Locks Post Down
   'Left
 Sub SolLeftPostDown(Enabled)
   If Enabled Then
 		LockPosDown.IsDropped = 0
 		LockPosDownP.z = 35
        PlaySound SoundFX("Diverter",DOFContactors)
 	else
 		LockPosDown.IsDropped = 1
 		LockPosDownP.z = -65
        PlaySound SoundFX("Diverter",DOFContactors)
 	End If
 End Sub

   'Right
 Sub SolRightPostDown(Enabled)
   If Enabled Then
 		LockPosDownR.IsDropped = 0
 		LockPosDownRP.z = 35
        PlaySound SoundFX("Diverter",DOFContactors)
 	else
 		LockPosDownR.IsDropped = 1
 		LockPosDownRP.z = -65
        PlaySound SoundFX("Diverter",DOFContactors)
 	End If
 End Sub

'Centrifuge Motor
Sub SolCentrifuge(Enabled)
    If Enabled Then
        Sw34a.Enabled = 1
     Else
        Sw34a.Enabled = 0
    End If
End Sub

'Ball Locks
 Sub SolCLock(Enabled)
   If Enabled Then
 		Div.IsDropped = 0
 	else
 		Div.IsDropped = 1
 	End If
 End Sub

'Stern-Sega GI 
set GICallback = GetRef("UpdateGI")

Sub UpdateGI(no, Enabled)
	If Enabled Then
		dim xx, xxx
		For each xx in GI:xx.State = 1: Next
		For each xxx in GI2:xxx.visible = 1: Next
        PlaySound "fx_relay"
	Else
		For each xx in GI:xx.State = 0: Next
		For each xxx in GI2:xxx.visible = 0: Next
        PlaySound "fx_relay"
	End If
End Sub







'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, Mag, bsLEject, bsRVuk, dtLDrop

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "CSI (Stern 2008)"&chr(13)&"You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 0
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0
     InitVpmFFlipsSAM

    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    vpmNudge.TiltSwitch = -7
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(LeftSlingshot, RightSlingshot)

    ' Trough & Ball Release
  Set bsTrough = New cvpmBallStack
      bsTrough.InitSw 0, 21, 20, 19, 18, 0, 0, 0
      bsTrough.InitKick BallRelease, 90, 10
      bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
      bsTrough.IsTrough = True
      bsTrough.Balls = 4

  Set Mag= New cvpmMagnet  
      Mag.InitMagnet Magnet, 105
	  Mag.Solenoid = 3
	  Mag.GrabCenter = true
	  Mag.CreateEvents "Mag"

     'Left Eject


	Set bsLEject = New cvpmSaucer
	With bsLEject
	    .InitKicker Sw35, 35, 170, 30, 1.56
		.InitSounds "fx_sensor", SoundFX(SSolenoidOn,DOFContactors), SoundFX("Popper",DOFContactors)
		.CreateEvents "bsLEject", Sw35
    End With 


 ' Set bsLEject = new cvpmBallStack
 '     bsLEject.InitSw 0, 35, 0, 0, 0, 0, 0, 0
 '     bsLEject.InitKick sw35, 170, 25, 1.5
 '     bsLEject.KickZ = 1.5
 '     bsLEject.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
 '     bsLEject.KickAngleVar = 2
 '     bsLEject.KickBalls = 1

    ' Right vuk
  Set bsRVuk = New cvpmBallStack
      bsRVuk.InitSaucer Sw36, 36, 237, 29
      bsRVuk.KickZ = 1.38
      bsRVuk.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

 Set dtLDrop = new cvpmDropTarget
	 dtLDrop.Initdrop Array(Sw2,Sw3,Sw4), Array(2,3,4)
	 dtLDrop.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)


 	Div.IsDropped = 1

    Controller.switch(10) = 1 'Opto switch: inverted logic
    Controller.switch(11) = 1 'Opto switch: inverted logic
    Controller.switch(12) = 1 'Opto switch: inverted logic 

    BolaPL.Visible = 0
    BolaPR.Visible = 0

End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
	if KeyCode = LeftTiltKey Then Nudge 90, 4
	if KeyCode = RightTiltKey Then Nudge 270, 4
	if KeyCode = CenterTiltKey Then Nudge 0, 4

End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"

End Sub

     ' Impulse Plunger
	Dim PlungerIM
    Const IMPowerSetting = 60
    Const IMTime = 0.6
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .CreateEvents "plungerIM"
    End With

'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : playsound"drain" : End Sub
Sub Sw36_Hit: bsRVuk.AddBall 0: playsound "popper_ball": End Sub


Sub Sw35b_hit:PlaySound "fx_kicker-enter": Sw35b.enabled = 0 : End Sub
Sub Sw35_UnHit: Sw35bTimer.interval = 400: Sw35bTimer.enabled = 1 :End Sub'vpmtimer.addtimer 200, "Sw35b.enabled = 1 '" :End Sub

Sub Sw35bTimer_Timer
    Sw35bTimer.enabled = 0
    Sw35b.enabled = 1
End Sub
'Dim bBall
'Sub sw35_Hit
'    playsound "popper_ball"
'    Set bBall = ActiveBall:Me.TimerEnabled = 1

'    bsLEject.AddBall 0
'End Sub

'Sub sw35_Timer
'    Do While bBall.Z > 0
'        bBall.Z = bBall.Z -5
'        Exit Sub
'    Loop
'    Me.DestroyBall
'    Me.TimerEnabled = 0
'End Sub

'Drop Targets
Sub Sw2_Dropped:dtLDrop.Hit 1:End Sub
Sub Sw3_Dropped:dtLDrop.Hit 2:End Sub
Sub Sw4_Dropped:dtLDrop.Hit 3:End Sub

'Mircroscope magnet
Sub Magnet_Hit:Mag.AddBall ActiveBall:End Sub	
Sub Magnet_UnHit:Mag.RemoveBall ActiveBall:End Sub

'Stand Up Targets
Sub Sw1_Hit:vpmTimer.PulseSw 1:End Sub
Sub Sw41_Hit:vpmTimer.PulseSw 41:End Sub
Sub Sw44_Hit:vpmTimer.PulseSw 44:End Sub
Sub Sw45_Hit:vpmTimer.PulseSw 45:End Sub
Sub Sw49_Hit:vpmTimer.PulseSw 49:End Sub

'Rollovers
Sub Sw5_Hit:Controller.Switch(5)=1 : playsound"rollover" : End Sub
Sub Sw5_UnHit:Controller.Switch(5)=0: End Sub
Sub Sw6_Hit:Controller.Switch(6)=1 : playsound"rollover" : End Sub
Sub Sw6_UnHit:Controller.Switch(6)=0: End Sub
Sub Sw14_Hit:Controller.Switch(14)=1 : playsound"rollover" : End Sub
Sub Sw14_UnHit:Controller.Switch(14)=0: End Sub
Sub sw23_Hit:Controller.Switch(23)=1 : playsound"rollover" : End Sub
Sub sw23_UnHit:Controller.Switch(23)=0: End Sub
Sub Sw24_Hit:Controller.Switch(24)=1 : playsound"rollover" : End Sub
Sub Sw24_UnHit:Controller.Switch(24)=0: End Sub
Sub Sw25_Hit:Controller.Switch(25)=1 : playsound"rollover" : End Sub
Sub Sw25_UnHit:Controller.Switch(25)=0: End Sub
Sub Sw28_Hit:Controller.Switch(28)=1 : playsound"rollover" : End Sub
Sub Sw28_UnHit:Controller.Switch(28)=0: End Sub
Sub Sw29_Hit:Controller.Switch(29)=1 : playsound"rollover" : End Sub
Sub Sw29_UnHit:Controller.Switch(29)=0: End Sub
Sub Sw39_Hit:Controller.Switch(39)=1 : playsound"rollover" : End Sub
Sub Sw39_UnHit:Controller.Switch(39)=0: End Sub
Sub Sw40_Hit:Controller.switch(40)=1 : playsound"rollover" : End Sub
Sub Sw40_UnHit:Controller.switch(40) = 0 End Sub
Sub Sw51_Hit:Controller.Switch(51)=1 : playsound"rollover" : End Sub
Sub Sw51_UnHit:Controller.Switch(51)=0: End Sub

'Bumpers
Sub Bumper1_Hit : vpmTimer.PulseSw(30) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(31) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper3_Hit : vpmTimer.PulseSw(32) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub

'Centrifuge
Sub Sw33_Hit:Controller.switch(33) = 1 End Sub
Sub Sw33_UnHit:Controller.switch(33) = 0 End Sub
Sub Sw34_Hit:Controller.switch(34) = 1 End Sub
Sub Sw34_UnHit:Controller.switch(34) = 0 End Sub
Sub Sw34a_Hit():Sw34a.Kick 280,30:End Sub

'Ramp Trigger
Sub Sw37_Hit:vpmTimer.PulseSw 37 : End Sub

'Spinners
Sub Sw7_Spin:vpmTimer.PulseSw 7 : playsound"fx_spinner" : End Sub
Sub Sw38_Spin:vpmTimer.PulseSw 38 : playsound"fx_spinner" : End Sub

'Generic Sounds
Sub Trigger1_Hit : playsound"fx_ballrampdrop" : End Sub
Sub Trigger2_Hit : playsound"fx_ballrampdrop" : End Sub
Sub Trigger3_Hit : playsound"Wire Ramp" : End Sub
Sub Trigger4_Hit : playsound"Wire Ramp" : End Sub
Sub Trigger5_Hit : playsound"fx_ballrampdrop" : End Sub

'**********************************************************************************************************
'Skull
'**********************************************************************************************************
Dim mSkull

Sub SolSkullMotor(Enabled)
	If Enabled Then
		UpdateSkull.Enabled=1 
		'debug.print Timer & " SOL:SkullMotor ON" & sDir 
	Else
		UpdateSkull.Enabled=0
		'debug.print Timer & " SOL:SkullMotor OFF" & sDir 
	End If
 
End Sub

Dim BallsInPlay 
Sub SolSkullMotorRelay (enabled)
	if Enabled Then
		sDir = - SkullStepSize 'down
		'debug.print timer & " SOL:SkullMotorDir DOWN" 
	else
		sDir = SkullStepSize 'Up
		'debug.print timer & " SOL:SkullMotorDir UP"
	end if

   If sPos >= 135 Then
       SwLock.Enabled = 1:GI_021.State = 2:GI_022.State = 2
	else
       SwLock.Enabled = 0:GI_021.State = 0:GI_022.State = 0
   End If

   If sPos >= 149 and LockBall > 1 then
       Sw8.Createball: Sw8.Kick 150, 0.5: controller.switch(8) = 0: BolaPL.Visible = 0: PlaySound "fx_kout"
       Sw9.Createball: Sw9.Kick 150, 0.5: controller.switch(9) = 0: BolaPR.Visible = 0: PlaySound "fx_kout"
       LockBall = 0
       BallsInPlay = BallsInPlay + 2
   End If
   If sPos >= 149 and LockBall = 1 then
       Sw8.Createball: Sw8.Kick 150, 0.5: controller.switch(8) = 0: BolaPL.Visible = 0: PlaySound "fx_kout"
       LockBall = 0
       BallsInPlay = BallsInPlay + 1
   End If

End Sub


Sub SwLock_Hit
   If LockBall = 0 Then
       controller.switch(8) = 1
       Me.DestroyBall
       BolaPL.Visible = 1
       LockBall = LockBall + 1
      ' BallsInPlay = BallsInPlay - 1
   ElseIf LockBall >= 1 Then
       controller.switch(9) = 1
       Me.DestroyBall
       BolaPR.Visible = 1
       LockBall = LockBall + 1
       'BallsInPlay = BallsInPlay - 1
    End If      
   playsound "popper_ball"     
End Sub



Const SkullStepSize = .2
Dim LockBall: LockBall = 0
Dim sPos, sDir
sPos = SkullP.Z
sDir = SkullStepSize

Sub UpdateSkull_Timer()
    PlaySound "motor"  
    sPos = sPos + sDir

	If sPos <= 80 then  'DOWN position
		'debug.print Timer & " down12 sw 0:" & sPos
		controller.switch(12) = 0
	Else
		'debug.print Timer & " down12 sw 1:" & sPos
		controller.switch(12) = 1
	End if

	If sPos >= 139 and sPos <= 142 then  'MIDDLE position
		'debug.print timer & " middle11 sw 0:" & sPos
		controller.switch(11) = 0      
	Else
		'debug.print timer & " middle11 sw 1:" & sPos
		controller.switch(11) = 1
	End if

	If sPos >= 150 then 'UP position
		'debug.print timer & "up10 sw 0:" & sPos
		controller.switch(10) = 0
	Else
		'debug.print timer & " up10 sw 1:" & sPos
		controller.switch(10) = 1
	End if

	'When motor reaches end of travel, change direction
	If sPos <= 75 then 'change direction of motor
		sdir = SkullStepSize
		debug.print "skull changing direction (up)"
	Elseif sPos >= 155 Then
		sdir = -SkullStepSize
		debug.print "skull changing direction (down)"
	End If

    SkullP.Z = sPos
    BolaPL.TransY = sPos
    BolaPR.TransY = sPos
 End Sub




'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 5 'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
    UpdateLamps
End Sub

Sub UpdateLamps
    NFadeL 3, l3
    NFadeL 4, l4
    NFadeL 5, l5
    NFadeL 6, l6
'    NFadeL 7, l7
'    NFadeL 8, l8
    NFadeL 9, l9
    NFadeL 10, l10
    NFadeL 11, l11
    NFadeL 12, l12
    NFadeL 13, l13
    NFadeL 14, l14
    NFadeL 15, l15
    NFadeL 16, l16
    NFadeL 17, l17
    NFadeL 18, l18
    NFadeL 19, l19
    NFadeObjm  20, l20, "bulbcover1_greenON", "bulbcover1_green" 'Left Spinner sign Green LED
    Flash 20, f20
    NFadeObjm  21, l21, "bulbcover1_greenON", "bulbcover1_green" 'Right Spinner sign Green LED
    Flash 21, f21
    NFadeL 22, l22
    NFadeL 23, l23
    NFadeL 24, l24
    NFadeL 25, l25
    NFadeL 26, l26
    NFadeL 27, l27
    NFadeL 28, l28
    NFadeL 29, l29
    NFadeL 30, l30
    NFadeL 31, l31
    NFadeL 32, l32
    NFadeL 33, l33 'Right Spot Light
    NFadeL 34, l34
    NFadeL 35, l35
    NFadeL 36, l36
    NFadeL 37, l37
    NFadeL 38, l38
 '   NFadeL 39, l39
    NFadeL 40, l40
    NFadeL 41, l41
    NFadeL 42, l42
    NFadeL 43, l43
    NFadeL 44, l44
    NFadeL 45, l45
    NFadeObjm  46, l46, "bulbOn", "[plastic-white3]" 'Bullet
    Flash 46, f46
    NFadeL 47, l47
    NFadeL 48, l48
    NFadeL 49, l49
    NFadeL 50, l50
    NFadeL 51, l51
    NFadeL 52, l52
    NFadeL 53, l53
    NFadeL 54, l54
    NFadeL 55, l55
    NFadeL 56, l56
    NFadeL 57, l57
    NFadeL 58, l58
    NFadeL 59, l59
	NFadeObjm 60, P60, "lampbulbON", "lampbulb"
	NFadeL 60, l60 'top left bumper
	NFadeObjm 61, P61, "lampbulbON", "lampbulb"
	NFadeL 61, l61 'right bumper
	NFadeObjm 62, P62, "lampbulbON", "lampbulb"
	NFadeL 62, l62 'lower left bumper
    NFadeL 63, l63
    NFadeL 64, l64
    NFadeL 65, l65
    NFadeL 66, l66
    NFadeL 67, l67
    NFadeL 68, l68
    NFadeL 69, l69
    NFadeL 70, l70
    NFadeL 71, l71
    NFadeL 72, l72
    NFadeL 73, l73
    NFadeL 74, l74
    NFadeL 75, l75
    NFadeL 76, l76
    NFadeL 77, l77
    NFadeL 78, l78
    NFadeL 79, l79
    NFadeL 80, l80
    Flash 86, F86
    Flash 87, F87
    Flash 88, F88

'Solenoid Controlled Flashers
    NFadeObjm  119, f119, "dome2_0_yellowON", "dome2_0_yellow"
    NFadeObjm  119, f119a, "dome2_0_yellowON", "dome2_0_yellow"
    NFadeLm 119, f119b
    NFadeL 119, f119c

    NFadeLm 120, f120
    NFadeL 120, f120a

    NFadeObjm  121, SkullP, "skull03", "skull02" 'Skull

    NFadeL 122, f122

    NFadeObjm  123, f123, "dome2_0_greenON", "dome2_0_green"
    NFadeObjm  123, f123a, "dome2_0_greenON", "dome2_0_green"
    Flashm 123, f123b
    Flash 123, f123c

    Flash 126, f126

    NFadeLm 128, f128

    Flash 131, f131

End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.4   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.2 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself

Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

' Flasher objects

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(nr)
End Sub

 'Reels
Sub FadeReel(nr, reel)
    Select Case FadingLevel(nr)
        Case 2:FadingLevel(nr) = 0
        Case 3:FadingLevel(nr) = 2
        Case 4:reel.Visible = 0:FadingLevel(nr) = 3
        Case 5:reel.Visible = 1:FadingLevel(nr) = 1
    End Select
End Sub

 'Inverted Reels
Sub FadeIReel(nr, reel)
    Select Case FadingLevel(nr)
        Case 2:FadingLevel(nr) = 0
        Case 3:FadingLevel(nr) = 2
        Case 4:reel.Visible = 1:FadingLevel(nr) = 3
        Case 5:reel.Visible = 0:FadingLevel(nr) = 1
    End Select
End Sub



'**********************************************************************************************************
'**********************************************************************************************************
'	Start of VPX functions
'**********************************************************************************************************
'**********************************************************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 27
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 26
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
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
'      JP's VP10 Rolling Sounds
'*****************************************

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

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
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
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperLSh1.RotZ = LeftFlipper1.currentangle

    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
    LFLogo1.RotY = LeftFlipper1.CurrentAngle

End Sub



'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc


'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and 
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


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
